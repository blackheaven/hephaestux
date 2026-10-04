module Hephaestux.Cli
  ( Options (..),
    Command (..),
    UpdateOptions (..),
    ListOptions (..),
    StatusOptions (..),
    SetupOptions (..),
    parseOptions,
  )
where

import Data.Text (Text)
import qualified Data.Text as Text
import Hephaestux.Style (Margin (..), normalizeStyle)
import Hephaestux.Types (State (..), parseState)
import Options.Applicative

data Command
  = CmdUpdate UpdateOptions
  | CmdEnd
  | CmdList ListOptions
  | CmdStatus StatusOptions
  | CmdSetup SetupOptions

data UpdateOptions = UpdateOptions
  { uoAgent :: Text,
    uoSymbol :: Maybe Text,
    uoState :: State,
    uoTitle :: Maybe Text
  }
  deriving stock (Show)

data ListOptions = ListOptions
  { loTmux :: Bool,
    loState :: Maybe State,
    loColors :: State -> Maybe Text
  }

data StatusOptions = StatusOptions
  { soWindow :: Int,
    soState :: Maybe State,
    soPadding :: Bool,
    soMargin :: Margin,
    soColors :: State -> Maybe Text
  }

data SetupOptions = SetupOptions
  { seDryRun :: Bool,
    seAgents :: [Text]
  }
  deriving stock (Show)

newtype Options = Options
  { optCommand :: Command
  }

parseOptions :: IO Options
parseOptions = execParser parser
  where
    parser =
      info
        (opts <**> helper)
        (fullDesc <> progDesc "Track coding-agent sessions for tmux")

opts :: Parser Options
opts = Options <$> subparsers

subparsers :: Parser Command
subparsers =
  hsubparser
    ( command "update" (info updateParser (progDesc "Record current agent session state"))
        <> command "end" (info (pure CmdEnd) (progDesc "Remove this agent's session record"))
        <> command "list" (info listParser (progDesc "List recorded sessions"))
        <> command "status" (info statusParser (progDesc "Render tmux segments for one window"))
        <> command "setup" (info setupParser (progDesc "Install agent hooks"))
    )

stateReader :: ReadM State
stateReader = do
  s <- str
  maybe (readerError ("unknown state: " <> Text.unpack s <> " (expected stale|working|waiting-input|done)")) pure (parseState s)

updateParser :: Parser Command
updateParser =
  CmdUpdate
    <$> ( UpdateOptions
            <$> strOption (long "agent" <> metavar "AGENT" <> help "Agent name")
            <*> (fmap . fmap) safeText (optional (strOption (long "symbol" <> metavar "SYMBOL" <> help "Symbol override")))
            <*> option stateReader (long "state" <> metavar "STATE" <> help "stale|working|waiting-input|done")
            <*> (fmap . fmap) safeText (optional (strOption (long "title" <> metavar "TITLE" <> help "Session title")))
        )

-- | Reject control characters that could inject terminal escapes.
safeText :: Text -> Text
safeText = Text.filter (\c -> c >= ' ' && c /= '\DEL')

listParser :: Parser Command
listParser =
  CmdList
    <$> ( ListOptions
            <$> switch (long "tmux" <> help "Render tmux status segments")
            <*> optional (option stateReader (long "state" <> metavar "STATE" <> help "Filter by state"))
            <*> colorsParser
        )

statusParser :: Parser Command
statusParser =
  CmdStatus
    <$> ( StatusOptions
            <$> option auto (long "window" <> metavar "WINDOW" <> help "Window index (required)")
            <*> optional (option stateReader (long "state" <> metavar "STATE" <> help "Filter by state"))
            <*> (flag' False (long "no-padding" <> help "No space between segments") <|> flag' True (long "padding") <|> pure True)
            <*> ( flag' MarginLeft (long "left-margin" <> help "Leading space when agents are present (default)")
                    <|> flag' MarginRight (long "right-margin" <> help "Trailing space when agents are present")
                    <|> flag' MarginNone (long "no-margin" <> help "No margin space")
                    <|> pure MarginLeft
                )
            <*> colorsParser
        )

-- | The four per-state color overrides.
colorsParser :: Parser (State -> Maybe Text)
colorsParser =
  mkOverride
    <$> optional (styleOption "color-stale")
    <*> optional (styleOption "color-working")
    <*> optional (styleOption "color-waiting-input")
    <*> optional (styleOption "color-done")

styleOption :: String -> Parser Text
styleOption name = fmap normalizeStyle (strOption (long name <> metavar "STYLE" <> help "tmux style fragment, e.g. fg=#ffaaff or [bg=#0011ff]"))

mkOverride :: Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> State -> Maybe Text
mkOverride stale working waiting done s = case s of
  Stale -> stale
  Working -> working
  WaitingInput -> waiting
  Done -> done

setupParser :: Parser Command
setupParser =
  CmdSetup
    <$> ( SetupOptions
            <$> switch (long "dry-run" <> help "Print planned steps without writing")
            <*> many (strOption (long "agent" <> metavar "AGENT" <> help "Agent to set up (repeatable; default: all)"))
        )
