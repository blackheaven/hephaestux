module Hephaestux.Install
  ( InstallStep (..),
    apply,
    mergeHooks,
    mentionsHephaestux,
    runAgent,
  )
where

import Data.Aeson (Value (..), eitherDecode, encode)
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TE
import qualified Data.Text.IO as TextIO
import qualified Data.Vector as V
import System.Directory (doesDirectoryExist, doesFileExist, getHomeDirectory, getPermissions, renameFile, setOwnerExecutable, setPermissions)
import System.FilePath (takeDirectory, (</>))
import System.IO (hPutStrLn, stderr)
import System.Posix.Process (getProcessID)

-- | Installer steps as data so --dry-run can print them without side effects.
--
-- For the JSON steps, the Text argument is the hephaestux payload: a JSON
-- array of hook entries (MergeJsonHooks) or the full object (ReplaceJsonKey).
data InstallStep
  = WriteFile FilePath Text
  | MakeExecutable FilePath
  | MergeJsonHooks FilePath Text
  | ReplaceJsonKey FilePath Text
  | AppendTomlHooks FilePath Text
  deriving stock (Show, Eq)

-- | Apply steps, printing intent in dry-run mode or outcome in real mode.
apply :: Bool -> [InstallStep] -> IO ()
apply dry steps = do
  expanded <- mapM expandPath steps
  mapM_ step expanded
  where
    step = \case
      WriteFile path content -> writeFileStep dry path content
      MakeExecutable path -> chmodStep dry path
      MergeJsonHooks path payload -> mergeStep dry path payload
      ReplaceJsonKey path payload -> replaceStep dry path payload
      AppendTomlHooks path block -> tomlStep dry path block

-- | Expand a leading "~/" to the real home directory.
expandPath :: InstallStep -> IO InstallStep
expandPath = \case
  WriteFile p c -> WriteFile <$> expandHome p <*> pure c
  MakeExecutable p -> MakeExecutable <$> expandHome p
  MergeJsonHooks p t -> (`MergeJsonHooks` t) <$> expandHome p
  ReplaceJsonKey p t -> (`ReplaceJsonKey` t) <$> expandHome p
  AppendTomlHooks p t -> (`AppendTomlHooks` t) <$> expandHome p

expandHome :: FilePath -> IO FilePath
expandHome ('~' : '/' : rest) = (</> rest) <$> getHomeDirectory
expandHome p = pure p

writeFileStep :: Bool -> FilePath -> Text -> IO ()
writeFileStep dry path content = do
  proceed <- checkDir dry path
  if not proceed
    then pure ()
    else
      if dry
        then note ("would write " <> Text.pack path)
        else do
          exists <- doesFileExist path
          if exists
            then do
              current <- TextIO.readFile path
              if current == content
                then note ("unchanged " <> Text.pack path)
                else
                  if "hephaestux" `isInfixOfT` current
                    then do
                      atomicWriteText path content
                      note ("wrote " <> Text.pack path)
                    else note ("skipped " <> Text.pack path <> " (existing file is not hephaestux-generated)")
            else do
              atomicWriteText path content
              note ("wrote " <> Text.pack path)

chmodStep :: Bool -> FilePath -> IO ()
chmodStep dry path = do
  proceed <- checkDir dry path
  if not proceed
    then pure ()
    else
      if dry
        then note ("would make " <> Text.pack path <> " executable")
        else do
          p <- getPermissions path
          setPermissions path (setOwnerExecutable True p)
          note ("made " <> Text.pack path <> " executable")

-- | Read-modify-write of a JSON config: under the top-level "hooks" key,
-- drop entries whose command mentions hephaestux and append the new ones;
-- all other keys preserved. Creates the file fresh when missing. Skips with
-- a loud note when the existing file is not valid JSON (JSONC comments).
mergeStep :: Bool -> FilePath -> Text -> IO ()
mergeStep dry path payload = do
  proceed <- checkDir dry path
  if not proceed
    then pure ()
    else
      if dry
        then note ("would merge hephaestux hooks under \"hooks\" in " <> Text.pack path)
        else do
          exists <- doesFileExist path
          mroot <- if exists then readJson path else pure (Right (Object KM.empty))
          case mroot of
            Left msg -> note ("skipped " <> Text.pack path <> ": " <> msg)
            Right (Object obj) -> case eitherDecode (BL.fromStrict (TE.encodeUtf8 payload)) of
              Left err -> note ("skipped " <> Text.pack path <> ": invalid payload (" <> Text.pack err <> ")")
              Right newValue -> do
                let obj' = Object (KM.insert "hooks" (mergeHooks (KM.lookup "hooks" obj) newValue) obj)
                    encoded = encode obj' <> "\n"
                current <- if exists then BS.readFile path else pure ""
                if current == BL.toStrict encoded
                  then note ("unchanged " <> Text.pack path)
                  else do
                    atomicWriteBL path encoded
                    note ("merged hephaestux hooks into " <> Text.pack path)
            Right _ -> note ("skipped " <> Text.pack path <> ": top level is not a JSON object")

-- | Replace one top-level key wholesale; all other keys preserved.
replaceStep :: Bool -> FilePath -> Text -> IO ()
replaceStep dry path payload = do
  proceed <- checkDir dry path
  if not proceed
    then pure ()
    else
      if dry
        then note ("would replace key \"hephaestux\" in " <> Text.pack path)
        else do
          exists <- doesFileExist path
          mroot <- if exists then readJson path else pure (Right (Object KM.empty))
          case mroot of
            Left msg -> note ("skipped " <> Text.pack path <> ": " <> msg)
            Right (Object obj) -> case eitherDecode (BL.fromStrict (TE.encodeUtf8 payload)) of
              Left err -> note ("skipped " <> Text.pack path <> ": invalid payload (" <> Text.pack err <> ")")
              Right val -> do
                let obj' = Object (KM.insert "hephaestux" val obj)
                atomicWriteBL path (encode obj' <> "\n")
                note ("replaced key \"hephaestux\" in " <> Text.pack path)
            Right _ -> note ("skipped " <> Text.pack path <> ": top level is not a JSON object")

-- | Append a TOML block at EOF; skip with a notice when the file already
-- mentions hephaestux (no TOML parser dependency).
tomlStep :: Bool -> FilePath -> Text -> IO ()
tomlStep dry path block = do
  proceed <- checkDir dry path
  if not proceed
    then pure ()
    else
      if dry
        then note ("would append hooks to " <> Text.pack path)
        else do
          exists <- doesFileExist path
          current <- if exists then TextIO.readFile path else pure ""
          if "hephaestux" `isInfixOfT` current
            then note ("skipped " <> Text.pack path <> " (hephaestux hooks already present)")
            else do
              atomicWriteText path (current <> block)
              note ("appended hooks to " <> Text.pack path)

-- | Whether the step may touch its target: the parent directory must
-- already exist. hephaestux never creates agent directories; setup skips
-- agents that are not installed.
checkDir :: Bool -> FilePath -> IO Bool
checkDir dry path = do
  let dir = takeDirectory path
  exists <- doesDirectoryExist dir
  if exists
    then pure True
    else do
      if dry
        then note ("skip " <> Text.pack path <> " (missing directory " <> Text.pack dir <> ")")
        else note ("skipped " <> Text.pack path <> ": " <> Text.pack dir <> " does not exist")
      pure False

isInfixOfT :: Text -> Text -> Bool
isInfixOfT = Text.isInfixOf

-- | Atomic text write: temp file in the same directory + rename.
atomicWriteText :: FilePath -> Text -> IO ()
atomicWriteText path content = atomicWriteBL path (encodeUtf8ToLazy content)

atomicWriteBL :: FilePath -> BL.ByteString -> IO ()
atomicWriteBL path content = do
  tmpPid <- getProcessID
  let tmp = path <> ".tmp-" <> show tmpPid
  BL.writeFile tmp content
  renameFile tmp path

encodeUtf8ToLazy :: Text -> BL.ByteString
encodeUtf8ToLazy = BL.fromStrict . TE.encodeUtf8

readJson :: FilePath -> IO (Either Text Value)
readJson path = do
  raw <- BS.readFile path
  pure $ case eitherDecode (BL.fromStrict raw) of
    Left err -> Left (Text.pack (takeWhile (/= '\n') err))
    Right v -> Right v

-- array of event objects) into an existing "hooks" value, which may be an
-- object keyed by event, an array of event objects, or absent. Existing
-- entries that are ours (command mentions hephaestux) are replaced; others
-- are kept.
mergeHooks :: Maybe Value -> Value -> Value
mergeHooks Nothing new = new
mergeHooks (Just (Array old)) (Array new) =
  let kept = filter (not . mentionsHephaestux) (V.toList old)
   in Array (V.fromList (kept <> V.toList new))
mergeHooks (Just (Array old)) (Object new) =
  Array (V.fromList (V.toList old <> [Object new]))
mergeHooks (Just (Object old)) new =
  Object (foldl addEvent old (eventEntries new))
  where
    eventEntries v = case v of
      Object km -> KM.toList km
      _ -> []
    addEvent acc (k, Array es) =
      let existing = case KM.lookup k acc of
            Just (Array old') -> filter (not . mentionsHephaestux) (V.toList old')
            _ -> []
       in KM.insert k (Array (V.fromList (existing <> V.toList es))) acc
    addEvent acc _ = acc
mergeHooks (Just old) _ = old

mentionsHephaestux :: Value -> Bool
mentionsHephaestux (Object obj)
  | Just (String s) <- KM.lookup "command" obj = Text.isInfixOf "hephaestux" s
  | Just (Array hs) <- KM.lookup "hooks" obj = any mentionsHephaestux hs
  | otherwise = False
mentionsHephaestux _ = False

note :: Text -> IO ()
note t = hPutStrLn stderr ("  " <> Text.unpack t)

-- | Print one agent's grouped output block — "<name>:" header, then
-- indented notes, then the step lines — in dry-run and real mode alike.
runAgent :: Text -> [Text] -> Bool -> [InstallStep] -> IO ()
runAgent name notes dry plan = do
  hPutStrLn stderr (Text.unpack name <> ":")
  mapM_ note notes
  apply dry plan
