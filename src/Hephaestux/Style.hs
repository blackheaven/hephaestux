module Hephaestux.Style
  ( Margin (..),
    defaultStyle,
    normalizeStyle,
    renderSegments,
  )
where

import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import Hephaestux.Agents (labelFor)
import Hephaestux.Types (Agent (..), SessionInfo (..), SessionState (..), State (..))

defaultStyle :: State -> Text
defaultStyle = \case
  Stale -> "fg=black"
  Working -> "fg=#ff8c00"
  WaitingInput -> "fg=#8b0000"
  Done -> "fg=#006400"

-- | Strip one surrounding bracket pair: `[fg=#ffaaff]` -> `fg=#ffaaff`.
normalizeStyle :: Text -> Text
normalizeStyle t
  | Text.isPrefixOf "[" t && Text.isSuffixOf "]" t && Text.length t >= 2 = Text.drop 1 (Text.dropEnd 1 t)
  | otherwise = t

data Margin = MarginLeft | MarginRight | MarginNone
  deriving stock (Show, Eq)

renderSegments :: (State -> Maybe Text) -> Bool -> Margin -> [SessionInfo] -> Text
renderSegments override padding margin infos = case margin of
  MarginLeft | not (null infos) -> " " <> body
  MarginRight | not (null infos) -> body <> " "
  _ -> body
  where
    body =
      Text.intercalate (if padding then " " else "") . map segment $ infos
    segment info =
      let style = fromMaybe (defaultStyle info.session.state) (override info.session.state)
          label = labelFor info.agent
       in "#[" <> style <> "]" <> label <> "#[default]"
