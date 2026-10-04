module Hephaestux.ProcessSpec (spec) where

import Data.Text (Text)
import qualified Data.Text as Text
import Hephaestux.Process (matchAncestor, shellWrapperNames)
import System.Posix.Types (CPid)
import Test.Hspec

spec :: Spec
spec = do
  describe "matchAncestor" $ do
    it "matches this process by its exact basename" $ do
      pid <- selfPid
      name <- ownArg0
      r <- matchAncestor [name] [pid]
      r `shouldBe` Just pid
    it "matches case-insensitively" $ do
      pid <- selfPid
      name <- ownArg0
      r <- matchAncestor [Text.toUpper name] [pid]
      r `shouldBe` Just pid
    it "returns Nothing for an empty chain" $ do
      r <- matchAncestor ["codex"] ([] :: [CPid])
      r `shouldBe` Nothing
  describe "shellWrapperNames" $
    it "contains the common shells" $
      map Text.toLower shellWrapperNames `shouldContain` ["sh", "bash"]

-- This test binary's own pid; its argv0 basename is the name matchAncestor
-- must find.
selfPid :: IO CPid
selfPid = fromIntegral . (read :: String -> Int) . takeWhile (/= ' ') <$> readFile "/proc/self/stat"

ownArg0 :: IO Text
ownArg0 = do
  contents <- readFile "/proc/self/cmdline"
  let arg0 = Text.takeWhileEnd (/= '/') (Text.pack (takeWhile (/= '\NUL') contents))
  pure (Text.toLower arg0)
