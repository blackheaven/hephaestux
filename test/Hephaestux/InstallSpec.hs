module Hephaestux.InstallSpec (spec) where

import Control.Monad (when)
import Data.Aeson (Value (..), eitherDecode)
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString.Lazy as BL
import Data.Text (Text)
import qualified Data.Text.Encoding as TE
import Hephaestux.Install (InstallStep (..), apply, mentionsHephaestux, mergeHooks)
import System.Directory (createDirectoryIfMissing, doesDirectoryExist, doesFileExist, getTemporaryDirectory, removePathForcibly)
import System.FilePath ((</>))
import Test.Hspec

dec :: Text -> Value
dec t = either (error "bad fixture json") id (eitherDecode (BL.fromStrict (TE.encodeUtf8 t)))

spec :: Spec
spec = do
  describe "mergeHooks" $ do
    let ours =
          dec
            "{\
            \ \"SessionStart\": [{\"hooks\": [{\"type\": \"command\", \"command\": \"hephaestux update --state stale || true\"}]}],\
            \ \"Stop\": [{\"hooks\": [{\"type\": \"command\", \"command\": \"hephaestux update --state waiting-input || true\"}]}]\
            \}"
    it "installs into a missing hooks key" $
      mergeHooks Nothing ours `shouldBe` ours
    it "merges into an object and keeps foreign entries" $ do
      let existing = dec "{\"SessionStart\": [{\"user\": 1}]}"
          merged = mergeHooks (Just existing) ours
      case merged of
        Object obj -> case KM.lookup "SessionStart" obj of
          Just (Array es) -> length es `shouldBe` 2
          _ -> expectationFailure "SessionStart missing"
        _ -> expectationFailure "not an object"
    it "replaces our own entries (idempotent)" $ do
      let once = mergeHooks (Just (dec "{\"Stop\": [{\"user\": 2}]}")) ours
          twice = mergeHooks (Just once) ours
      case twice of
        Object obj -> case KM.lookup "Stop" obj of
          Just (Array es) -> length es `shouldBe` 2 -- user + ours, not ours twice
          _ -> expectationFailure "Stop missing"
        _ -> expectationFailure "not an object"
    it "merges into a legacy array by append" $ do
      let existing = dec "[{\"legacy\": true}]"
          merged = mergeHooks (Just existing) ours
      case merged of
        Array es -> length es `shouldBe` 2
        _ -> expectationFailure "not an array"
  describe "mentionsHephaestux" $ do
    it "detects a direct command" $
      mentionsHephaestux (dec "{\"command\": \"hephaestux update --state stale\"}") `shouldBe` True
    it "detects a nested hooks wrapper" $
      mentionsHephaestux (dec "{\"hooks\": [{\"command\": \"hephaestux end\"}]}") `shouldBe` True
    it "ignores foreign entries" $
      mentionsHephaestux (dec "{\"command\": \"echo hi\"}") `shouldBe` False
  describe "apply" $ do
    it "creates nothing when the agent directory is missing" $ do
      root <- (</> "hphx-guard-test") <$> getTemporaryDirectory
      stale <- doesDirectoryExist root
      when stale (removePathForcibly root)
      let target = root </> ".claude" </> "settings.json"
      apply False [MergeJsonHooks target "{}"] -- payload decodes to Object
      exists <- doesFileExist target
      dirExists <- doesDirectoryExist (root </> ".claude")
      exists `shouldBe` False
      dirExists `shouldBe` False
    it "installs when the agent directory exists" $ do
      root <- (</> "hphx-guard-test-pos") <$> getTemporaryDirectory
      stale <- doesDirectoryExist root
      when stale (removePathForcibly root)
      let dir = root </> ".claude"
          target = dir </> "settings.json"
      createDirectoryIfMissing True dir
      apply False [MergeJsonHooks target "{}"]
      exists <- doesFileExist target
      exists `shouldBe` True
      removePathForcibly root
