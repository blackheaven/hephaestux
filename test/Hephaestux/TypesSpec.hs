module Hephaestux.TypesSpec (spec) where

import Data.Aeson (eitherDecode, encode)
import qualified Data.Text as Text
import qualified Data.Text.Lazy as LazyText
import qualified Data.Text.Lazy.Encoding as LazyTextEncoding
import Hephaestux.Types
import Test.Hspec

spec :: Spec
spec = do
  describe "parseState/stateName" $
    it "roundtrips every state" $
      mapM_ (\s -> parseState (stateName s) `shouldBe` Just s) [Stale, Working, WaitingInput, Done]

  describe "parseState" $
    it "rejects unknown" $
      parseState "bogus" `shouldBe` Nothing

  describe "JSON roundtrip" $ do
    it "roundtrips with all fields" $ do
      let info =
            SessionInfo
              { agent = Agent {name = "codex", symbol = Just "Cx"},
                session = SessionState {state = WaitingInput, title = Just "Refactor API"}
              }
      (eitherDecode . encode) info `shouldBe` Right info
    it "roundtrips with absent optional fields" $ do
      let info =
            SessionInfo
              { agent = Agent {name = "codex", symbol = Nothing},
                session = SessionState {state = Working, title = Nothing}
              }
      (eitherDecode . encode) info `shouldBe` Right info
    it "omits Nothing fields in encoding" $ do
      let encoded = LazyText.toStrict . LazyTextEncoding.decodeUtf8 . encode $ (SessionInfo {agent = Agent {name = "pi", symbol = Nothing}, session = SessionState {state = Stale, title = Nothing}} :: SessionInfo)
      encoded `shouldSatisfy` \t -> not (Text.isInfixOf "symbol" t) && not (Text.isInfixOf "title" t)
    it "matches spec sample keys" $ do
      let info =
            SessionInfo
              { agent = Agent {name = "codex", symbol = Just "Cx"},
                session = SessionState {state = WaitingInput, title = Just "Refactor API"}
              }
      LazyText.toStrict (LazyTextEncoding.decodeUtf8 (encode info))
        `shouldBe` "{\"agent\":{\"name\":\"codex\",\"symbol\":\"Cx\"},\"session\":{\"state\":\"waiting-input\",\"title\":\"Refactor API\"}}"
    it "accepts legacy records with a location key" $
      (eitherDecode "{\"agent\":{\"name\":\"codex\"},\"location\":{\"session\":\"$0\",\"window\":5,\"pane\":2},\"session\":{\"state\":\"working\"}}" :: Either String SessionInfo)
        `shouldBe` Right (SessionInfo {agent = Agent {name = "codex", symbol = Nothing}, session = SessionState {state = Working, title = Nothing}})
    it "rejects malformed JSON" $
      (eitherDecode "{\"state\":\"nope\"}" :: Either String SessionInfo) `shouldSatisfy` isLeftish
  where
    isLeftish = either (const True) (const False)
