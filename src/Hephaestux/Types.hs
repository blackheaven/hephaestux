module Hephaestux.Types
  ( State (..),
    stateName,
    parseState,
    Agent (..),
    Location (..),
    SessionState (..),
    SessionInfo (..),
  )
where

import Data.Aeson (FromJSON (..), Options, ToJSON (..), Value (String), defaultOptions, genericParseJSON, genericToEncoding, genericToJSON, omitNothingFields, withText)
import Data.Text (Text)
import GHC.Generics (Generic)

data State = Stale | Working | WaitingInput | Done
  deriving stock (Show, Eq, Generic)

stateName :: State -> Text
stateName = \case
  Stale -> "stale"
  Working -> "working"
  WaitingInput -> "waiting-input"
  Done -> "done"

parseState :: Text -> Maybe State
parseState = \case
  "stale" -> Just Stale
  "working" -> Just Working
  "waiting-input" -> Just WaitingInput
  "done" -> Just Done
  _ -> Nothing

data Agent = Agent
  { name :: Text,
    symbol :: Maybe Text
  }
  deriving stock (Show, Eq, Generic)

data Location = Location
  { session :: Text,
    window :: Int,
    pane :: Int
  }
  deriving stock (Show, Eq, Generic)

data SessionState = SessionState
  { state :: State,
    title :: Maybe Text
  }
  deriving stock (Show, Eq, Generic)

data SessionInfo = SessionInfo
  { agent :: Agent,
    session :: SessionState
  }
  deriving stock (Show, Eq, Generic)

jsonOptions :: Options
jsonOptions = defaultOptions {omitNothingFields = True}

instance ToJSON State where
  toJSON = String . stateName

instance FromJSON State where
  parseJSON = withText "State" (maybe (fail "unknown state") pure . parseState)

instance ToJSON Agent where
  toJSON = genericToJSON jsonOptions
  toEncoding = genericToEncoding jsonOptions

instance FromJSON Agent where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON SessionState where
  toJSON = genericToJSON jsonOptions
  toEncoding = genericToEncoding jsonOptions

instance FromJSON SessionState where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON SessionInfo where
  toJSON = genericToJSON jsonOptions
  toEncoding = genericToEncoding jsonOptions

instance FromJSON SessionInfo where
  parseJSON = genericParseJSON jsonOptions
