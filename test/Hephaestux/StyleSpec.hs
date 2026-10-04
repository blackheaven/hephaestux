module Hephaestux.StyleSpec (spec) where

import Data.Text (Text)
import Hephaestux.Style
import Hephaestux.Types
import Test.Hspec

spec :: Spec
spec = do
  describe "defaultStyle" $
    it "maps the four states" $ do
      defaultStyle Stale `shouldBe` "fg=black"
      defaultStyle Working `shouldBe` "fg=#ff8c00"
      defaultStyle WaitingInput `shouldBe` "fg=#8b0000"
      defaultStyle Done `shouldBe` "fg=#006400"

  describe "normalizeStyle" $
    it "strips one bracket pair" $ do
      normalizeStyle "[fg=#ffaaff]" `shouldBe` "fg=#ffaaff"
      normalizeStyle "fg=#ffaaff" `shouldBe` "fg=#ffaaff"

  describe "renderSegments" $ do
    let mk name sym st =
          SessionInfo
            { agent = Agent {name = name, symbol = sym},
              session = SessionState {state = st, title = Nothing}
            }
        noOverride = const Nothing :: State -> Maybe Text
    it "uses defaults and symbol fallback" $
      renderSegments noOverride True MarginNone [mk "codex" (Just "Cx") Working]
        `shouldBe` "#[fg=#ff8c00]Cx#[default]"
    it "falls back to the registry symbol" $
      renderSegments noOverride True MarginNone [mk "codex" Nothing Working]
        `shouldBe` "#[fg=#ff8c00]Cx#[default]"
    it "prefers an explicit symbol over the registry" $
      renderSegments noOverride True MarginNone [mk "codex" (Just "Ω") Working]
        `shouldBe` "#[fg=#ff8c00]Ω#[default]"
    it "falls back to name for unknown agents" $
      renderSegments noOverride True MarginNone [mk "mystery" Nothing Working]
        `shouldBe` "#[fg=#ff8c00]mystery#[default]"
    it "applies bracketed overrides" $
      renderSegments (const (Just "fg=#ffaaff")) True MarginNone [mk "codex" (Just "Cx") Working]
        `shouldBe` "#[fg=#ffaaff]Cx#[default]"
    it "joins with a space when padding" $
      renderSegments noOverride True MarginNone [mk "a" Nothing Stale, mk "b" Nothing Working]
        `shouldBe` "#[fg=black]a#[default] #[fg=#ff8c00]b#[default]"
    it "joins without space when no padding" $
      renderSegments noOverride False MarginNone [mk "a" Nothing Stale, mk "b" Nothing Working]
        `shouldBe` "#[fg=black]a#[default]#[fg=#ff8c00]b#[default]"
    it "renders empty for no sessions" $
      renderSegments noOverride True MarginNone ([] :: [SessionInfo]) `shouldBe` ""
    it "adds a leading space with left margin" $
      renderSegments noOverride True MarginLeft [mk "codex" (Just "Cx") Working]
        `shouldBe` " #[fg=#ff8c00]Cx#[default]"
    it "adds a trailing space with right margin" $
      renderSegments noOverride True MarginRight [mk "codex" (Just "Cx") Working]
        `shouldBe` "#[fg=#ff8c00]Cx#[default] "
    it "adds no margin space for no sessions (left)" $
      renderSegments noOverride True MarginLeft ([] :: [SessionInfo]) `shouldBe` ""
    it "adds no margin space for no sessions (right)" $
      renderSegments noOverride True MarginRight ([] :: [SessionInfo]) `shouldBe` ""
