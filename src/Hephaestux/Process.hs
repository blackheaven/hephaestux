module Hephaestux.Process
  ( ancestorChain,
    ancestorsOf,
    cmdlineOf,
    matchAncestor,
    resolveAgentPid,
    resolveEndTarget,
    pidAlive,
    shellWrapperNames,
  )
where

import Control.Exception (IOException, try)
import Data.Text (Text)
import qualified Data.Text as Text
import Hephaestux.Store (sessionFile)
import System.Directory (doesFileExist)
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)
import System.IO.Error (isPermissionError)
import System.Posix.Process (getParentProcessID)
import System.Posix.Signals (nullSignal, signalProcess)
import System.Posix.Types (CPid)
import Text.Read (readMaybe)

-- | Ancestor pids starting at the given pid (inclusive), walking upward,
-- stopping at init or an unreadable /proc entry.
ancestorsOf :: CPid -> IO [CPid]
ancestorsOf = go
  where
    go 1 = pure []
    go pid = do
      mparent <- ppidOf pid
      case mparent of
        Nothing -> pure []
        Just 1 -> pure [pid]
        Just parent -> (pid :) <$> go parent

ancestorChain :: IO [CPid]
ancestorChain = ancestorsOf =<< getParentProcessID

-- | ppid is field 4 in /proc/<pid>/stat; comm may contain spaces/parens, so
-- take everything after the LAST ')' and read the field after the state.
ppidOf :: CPid -> IO (Maybe CPid)
ppidOf pid = do
  r <- try (readFile ("/proc/" <> show pid <> "/stat"))
  pure $ case r of
    Left (_ :: IOException) -> Nothing
    Right stat -> case words (afterLastParen stat) of
      (_state : ppid : _) -> readMaybe ppid
      _ -> Nothing
  where
    afterLastParen s = reverse (takeWhile (/= ')') (reverse s))

-- | argv of a process, split on NUL; Nothing when unreadable (zombie/exit race).
cmdlineOf :: CPid -> IO (Maybe [Text])
cmdlineOf pid = do
  r <- try (readFile ("/proc/" <> show pid <> "/cmdline"))
  pure $ case r of
    Left (_ :: IOException) -> Nothing
    Right s -> Just (splitNul (Text.pack s))
  where
    splitNul t
      | Text.null t = []
      | otherwise = case Text.breakOn "\NUL" t of
          (a, rest) -> a : splitNul (Text.drop 1 rest)

-- | First ancestor whose cmdline has an argument whose file name
-- (case-insensitive) equals one of the proc names. Exact match, never substring.
matchAncestor :: [Text] -> [CPid] -> IO (Maybe CPid)
matchAncestor procNames = go
  where
    go [] = pure Nothing
    go (p : rest) = do
      mcmd <- cmdlineOf p
      case mcmd of
        Nothing -> go rest
        Just args
          | any matches args -> pure (Just p)
          | otherwise -> go rest
    matches arg = Text.toLower (basename arg) `elem` map Text.toLower procNames

basename :: Text -> Text
basename = Text.pack . reverse . takeWhile (/= '/') . reverse . Text.unpack

shellWrapperNames :: [Text]
shellWrapperNames = ["sh", "bash", "dash", "zsh"]

-- | Resolve the agent pid: registry proc names first, then the first ancestor
-- that is not a shell wrapper (skips `sh -c` hook indirection).
resolveAgentPid :: [Text] -> [CPid] -> IO CPid
resolveAgentPid procNames pids = do
  byName <- matchAncestor procNames pids
  case byName of
    Just p -> pure p
    Nothing -> do
      byNonShell <- go pids
      case byNonShell of
        Just p -> pure p
        Nothing -> do
          hPutStrLn stderr "hephaestux: cannot locate agent process in ancestor chain"
          exitFailure
  where
    go [] = pure Nothing
    go (p : rest) = do
      mcmd <- cmdlineOf p
      case mcmd of
        Nothing -> go rest
        Just (arg0 : _)
          | Text.toLower (basename arg0) `notElem` map Text.toLower shellWrapperNames -> pure (Just p)
        Just _ -> go rest

-- | First ancestor with an existing session file (for `end`).
resolveEndTarget :: IO (Maybe CPid)
resolveEndTarget = do
  pids <- ancestorChain
  go pids
  where
    go [] = pure Nothing
    go (p : rest) = do
      exists <- doesFileExist (sessionFile p)
      if exists then pure (Just p) else go rest

-- | Liveness via signal 0; a permission error means the process exists.
pidAlive :: CPid -> IO Bool
pidAlive pid = do
  r <- try (signalProcess nullSignal pid)
  pure $ case r of
    Right () -> True
    Left e -> isPermissionError e
