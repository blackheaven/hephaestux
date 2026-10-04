module Hephaestux.TmuxSpec (spec) where

import Data.Text (Text)
import Hephaestux.Tmux (parseIndex, parsePaneEntries)
import System.Posix.Types (CPid)
import Test.Hspec

spec :: Spec
spec = do
  describe "parseIndex" $ do
    it "parses digits" $ parseIndex "5" `shouldBe` Right 5
    it "rejects empty" $ (parseIndex "" :: Either Text Int) `shouldSatisfy` isLeft
    it "rejects letters" $ (parseIndex "abc" :: Either Text Int) `shouldSatisfy` isLeft
    it "rejects session ids" $ (parseIndex "$0" :: Either Text Int) `shouldSatisfy` isLeft
  describe "parsePaneEntries" $ do
    it "parses a single entry" $
      parsePaneEntries "12345 %0" `shouldBe` Right [(12345, "%0")]
    it "parses two well-formed lines" $
      parsePaneEntries "12345 %0\n99 %3" `shouldBe` Right [(12345, "%0"), (99, "%3")]
    it "rejects a bad pid" $ (parsePaneEntries "abc %0" :: Either Text [(CPid, Text)]) `shouldSatisfy` isLeft
    it "rejects a target missing %" $ (parsePaneEntries "12345 0" :: Either Text [(CPid, Text)]) `shouldSatisfy` isLeft
    it "rejects a single token" $ (parsePaneEntries "12345" :: Either Text [(CPid, Text)]) `shouldSatisfy` isLeft
    it "parses empty input to []" $ parsePaneEntries "" `shouldBe` Right []
  where
    isLeft = either (const True) (const False)
