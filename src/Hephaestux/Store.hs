module Hephaestux.Store
  ( storeDir,
    sessionFile,
    ensureStore,
    readSessions,
    writeSession,
    deleteSession,
  )
where

import Control.Exception (IOException, try)
import Control.Monad (unless, when)
import Data.Aeson (eitherDecode', encode)
import Data.Bits ((.&.))
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import Data.Text (Text)
import qualified Data.Text as Text
import Hephaestux.Types (SessionInfo)
import System.Directory (createDirectoryIfMissing, doesDirectoryExist, doesFileExist, listDirectory, removeFile, renameFile)
import System.Exit (exitFailure)
import System.FilePath (isExtensionOf, (</>))
import System.IO (hClose, hPutStrLn, stderr)
import System.Posix.Files (fileMode, fileOwner, getFileStatus, setFileMode)
import System.Posix.IO (OpenFileFlags (..), OpenMode (WriteOnly), defaultFileFlags, exclusive, fdToHandle, openFd)
import System.Posix.Process (getProcessID)
import System.Posix.Types (CPid)
import System.Posix.User (getEffectiveUserID)

storeDir :: FilePath
storeDir = "/tmp/hephaestux"

sessionFile :: CPid -> FilePath
sessionFile pid = storeDir </> (show pid <> ".json")

-- | Create the store with owner-only permissions if it does not exist yet;
-- refuse to use a store we do not own exclusively (pre-planted directory).
ensureStore :: IO ()
ensureStore = do
  existed <- doesDirectoryExist storeDir
  unless existed $ do
    createDirectoryIfMissing True storeDir
    setFileMode storeDir 0o700
  verifyStore

verifyStore :: IO ()
verifyStore = do
  st <- getFileStatus storeDir
  uid <- getEffectiveUserID
  let badOwner = fileOwner st /= uid
      badMode = fileMode st .&. 0o077 /= 0
  when (badOwner || badMode) $ do
    hPutStrLn stderr "hephaestux: refusing unsafe /tmp/hephaestux store (owner or permissions)"
    exitFailure

readSessions :: IO [(FilePath, Either Text SessionInfo)]
readSessions = do
  ensureStore
  files <- listDirectory storeDir
  mapM readOne (filter (isExtensionOf ".json") files)
  where
    readOne f = do
      r <- try (BS.readFile (storeDir </> f))
      case r of
        Left (_ :: IOException) -> pure (storeDir </> f, Left "unreadable (vanished during read)")
        Right contents ->
          pure (storeDir </> f, either (Left . Text.pack) Right (eitherDecode' (BL.fromStrict contents)))

-- | Atomically write the session file: exclusive temp file + rename, so
-- concurrent hooks are safe and a pre-planted symlink cannot be followed.
writeSession :: CPid -> SessionInfo -> IO ()
writeSession pid info = do
  ensureStore
  tmpPid <- getProcessID
  let payload = encode info <> "\n"
      tryWrite n = do
        let tmp = sessionFile pid <> ".tmp-" <> show tmpPid <> "-" <> show (n :: Int)
        r <- try (openFd tmp WriteOnly defaultFileFlags {creat = Just 0o600, exclusive = True, trunc = True})
        case r of
          Left (_ :: IOException) -> tryWrite (n + 1)
          Right fd -> do
            h <- fdToHandle fd
            BL.hPut h payload
            hClose h
            renameFile tmp (sessionFile pid)
  tryWrite 0

-- | Delete the session file; True if it existed.
deleteSession :: CPid -> IO Bool
deleteSession pid = do
  existed <- doesFileExist (sessionFile pid)
  when existed $ removeFile (sessionFile pid)
  pure existed
