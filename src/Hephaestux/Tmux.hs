module Hephaestux.Tmux
  ( locateSessions,
    parseIndex,
    parsePaneEntries,
  )
where

import Control.Exception (IOException, try)
import Data.Char (isDigit)
import Data.Text (Text)
import qualified Data.Text as Text
import Hephaestux.Process (ancestorsOf)
import Hephaestux.Types (Location (..))
import System.Posix.Types (CPid)
import System.Process (readProcess)
import Text.Read (readMaybe)

-- | Map each pid to its tmux pane location by walking ancestors until one is a
-- pane's direct child (`pane_pid`). Each matched pane needs three per-field
-- display-message calls (session, window, pane).
locateSessions :: [CPid] -> IO [(CPid, Either Text Location)]
locateSessions [] = pure []
locateSessions pids = do
  r <- try (readProcess "tmux" ["list-panes", "-a", "-F", "#{pane_pid} #{pane_id}"] "")
  case r of
    Left (_ :: IOException) ->
      pure [(pid, Left "tmux list-panes failed") | pid <- pids]
    Right out -> case parsePaneEntries (Text.pack out) of
      Left err -> pure [(pid, Left err) | pid <- pids]
      Right panes -> traverse (locate panes) pids
  where
    locate panes pid = do
      loc <- derive pid panes
      pure (pid, loc)
    derive pid panes = do
      ancestors <- ancestorsOf pid
      case firstPane panes ancestors of
        Nothing -> pure (Left ("no tmux pane found for pid " <> Text.pack (show pid)))
        Just target -> deriveAt target
    deriveAt target = do
      msession <- queryAt target "#{session_id}"
      case msession of
        Nothing -> pure (Left "cannot read session_id from tmux")
        Just session -> do
          mwindow <- queryAt target "#{window_index}"
          mpane <- queryAt target "#{pane_index}"
          pure $ do
            window <- parseEither "window_index" mwindow
            pane <- parseEither "pane_index" mpane
            pure Location {session = session, window = window, pane = pane}
    firstPane panes = go
      where
        go [] = Nothing
        go (a : as) = case lookup a panes of
          Just target -> Just target
          Nothing -> go as

parseEither :: Text -> Maybe Text -> Either Text Int
parseEither what = maybe (Left ("cannot read " <> what <> " from tmux")) parseIndex

-- | Parse `list-panes` lines strictly as `"<pid> <%n>"`.
parsePaneEntries :: Text -> Either Text [(CPid, Text)]
parsePaneEntries = traverse parseLine . filter (not . Text.null) . Text.lines
  where
    parseLine line = case Text.words line of
      [pidStr, target]
        | Text.all isDigit pidStr,
          Just pid <- readMaybe (Text.unpack pidStr),
          Just tid <- Text.stripPrefix "%" target,
          not (Text.null tid),
          Text.all isDigit tid ->
            Right (pid, target)
      _ -> Left ("unparsable list-panes output: " <> line)

-- | Run `tmux display-message -p -t target -F fmt`, trimming the trailing newline.
queryAt :: Text -> Text -> IO (Maybe Text)
queryAt target fmt = do
  r <- try (readProcess "tmux" ["display-message", "-p", "-t", Text.unpack target, "-F", Text.unpack fmt] "")
  pure $ case r of
    Left (_ :: IOException) -> Nothing
    Right s ->
      let t = Text.strip (Text.pack s)
       in if Text.null t then Nothing else Just t

-- | Strict non-empty digit string -> Int.
parseIndex :: Text -> Either Text Int
parseIndex t
  | Text.null t = Left "empty tmux index"
  | Text.all isDigit t = maybe (Left ("unparsable tmux index: " <> t)) Right (readMaybe (Text.unpack t))
  | otherwise = Left ("unparsable tmux index: " <> t)
