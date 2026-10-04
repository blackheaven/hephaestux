module Hephaestux.Commands
  ( run,
  )
where

import Control.Monad (forM, forM_, unless, void)
import Data.Maybe (catMaybes, fromMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as TextIO
import Hephaestux.Agents (AgentSpec (..), agentNames, labelFor, lookupAgent)
import Hephaestux.Cli
import Hephaestux.Install (runAgent)
import Hephaestux.Process (ancestorChain, pidAlive, resolveAgentPid, resolveEndTarget)
import Hephaestux.Store (deleteSession, readSessions, writeSession)
import Hephaestux.Style (Margin (..), renderSegments)
import Hephaestux.Tmux (locateSessions)
import Hephaestux.Types
import System.Exit (exitFailure, exitSuccess)
import System.FilePath (takeFileName)
import System.IO (hPutStrLn, stderr)
import System.Posix.Types (CPid)
import Text.Read (readMaybe)

run :: Options -> IO ()
run opts = case optCommand opts of
  CmdUpdate u -> update u
  CmdEnd -> end
  CmdList l -> list l
  CmdStatus s -> status s
  CmdSetup s -> setup s

die :: Text -> IO a
die msg = do
  hPutStrLn stderr ("hephaestux: " <> Text.unpack msg)
  exitFailure

-- | update: resolve pid, then atomically record the session.
update :: UpdateOptions -> IO ()
update u = do
  spec <- agentSpecOf u.uoAgent
  pids <- ancestorChain
  let info =
        SessionInfo
          { agent = Agent {name = u.uoAgent, symbol = u.uoSymbol},
            session = SessionState {state = u.uoState, title = u.uoTitle}
          }
  pid <- resolveAgentPid spec.procNames pids
  writeSession pid info

agentSpecOf :: Text -> IO AgentSpec
agentSpecOf name = maybe (die ("unknown agent: " <> name)) pure (lookupAgent name)

-- | end: delete this session's record; idempotent, silent success.
end :: IO ()
end = do
  target <- resolveEndTarget
  case target of
    Nothing -> do
      hPutStrLn stderr "hephaestux: no session record found"
      exitSuccess
    Just pid -> void (deleteSession pid)

-- | list: read records, filter dead pids and state; plain or tmux rendering.
list :: ListOptions -> IO ()
list l = do
  sessions <- collectSessions l.loState
  located <- locateSessions (map fst sessions)
  let infoOf pid = fromMaybe (error "locateSessions pid mismatch") (lookup pid sessions)
  if l.loTmux
    then do
      forM_ located $ \(pid, mloc) -> case mloc of
        Left err -> skipWarning pid err
        Right _ -> pure ()
      TextIO.putStrLn
        ( renderSegments
            l.loColors
            True
            MarginNone
            [infoOf pid | (pid, Right _) <- located]
        )
    else forM_ located $ \(pid, mloc) -> case mloc of
      Left err -> skipWarning pid err
      Right loc ->
        let s = infoOf pid
         in TextIO.putStrLn
              ( labelOf s.agent
                  <> "  "
                  <> stateName s.session.state
                  <> "  "
                  <> locText loc
                  <> "  "
                  <> fromMaybe "--" s.session.title
              )
  where
    labelOf = labelFor

locText :: Location -> Text
locText loc =
  loc.session <> "/" <> Text.pack (show loc.window) <> ":" <> Text.pack (show loc.pane)

skipWarning :: CPid -> Text -> IO ()
skipWarning pid err =
  hPutStrLn stderr ("hephaestux: skipping " <> show pid <> ": " <> Text.unpack err)

-- | status: render one window's sessions as tmux status segments.
status :: StatusOptions -> IO ()
status s = do
  sessions <- collectSessions s.soState
  located <- locateSessions (map fst sessions)
  let inWindow =
        [ fromMaybe (error "locateSessions pid mismatch") (lookup pid sessions)
        | (pid, Right loc) <- located,
          loc.window == s.soWindow
        ]
  TextIO.putStrLn (renderSegments s.soColors s.soPadding s.soMargin inWindow)

-- | Shared: read records, warn+skip dead pids, filter by state.
collectSessions :: Maybe State -> IO [(CPid, SessionInfo)]
collectSessions mstate = do
  raw <- readSessions
  valid <- forM raw $ \(path, er) -> case er of
    Left err -> do
      hPutStrLn stderr ("hephaestux: skipping " <> path <> ": " <> Text.unpack err)
      pure Nothing
    Right info -> case filePid path of
      Nothing -> do
        hPutStrLn stderr ("hephaestux: skipping " <> path <> ": unparsable pid in filename")
        pure Nothing
      Just pid -> do
        alive <- pidAlive pid
        unless alive $
          hPutStrLn stderr ("hephaestux: skipping " <> path <> ": agent process is dead")
        pure (if alive then Just (pid, info) else Nothing)
  let aliveSessions = catMaybes valid
  pure (maybe id (\st -> filter (\(_, x) -> x.session.state == st)) mstate aliveSessions)

filePid :: FilePath -> Maybe CPid
filePid path = readMaybe (takeWhile (/= '.') (takeFileName path))

-- | setup: install hook files for the selected agents.
setup :: SetupOptions -> IO ()
setup s = do
  specs <- mapM resolveAgent (if null s.seAgents then agentNames else s.seAgents)
  forM_ specs $ \spec -> runAgent spec.specName spec.notes s.seDryRun spec.plan
  where
    resolveAgent name = case lookupAgent name of
      Just spec -> pure spec
      Nothing ->
        die
          ( "unknown agent: "
              <> name
              <> " (valid: "
              <> Text.intercalate ", " agentNames
              <> ")"
          )
