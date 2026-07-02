{-# LANGUAGE UnicodeSyntax, OverloadedStrings #-}
-- | Video-config YAML parsing tests (issue #433). The contract under
--   test: the legacy-@fullscreen@ fallback (taken whenever @window_mode@
--   is absent) must treat @fullscreen@ itself as *optional* — a @video:@
--   section with neither key parses to windowed defaults instead of
--   failing the whole parse and silently resetting every video setting
--   back to 'defaultVideoConfig'.
module Test.Headless.Graphics.VideoConfig (spec) where

import UPrelude
import Test.Hspec
import Data.ByteString (ByteString)
import qualified Data.Yaml as Yaml
import Engine.Graphics.Config

-- | Parse a video-config YAML document or fail the test loudly.
parseConfig ∷ ByteString → VideoConfigFile
parseConfig bs = case Yaml.decodeEither' bs of
    Left err  → error ("decode failed: " ⧺ show err)
    Right cfg → cfg

minimalVideo ∷ ByteString
minimalVideo = "video:\n  resolution:\n    width: 1280\n    height: 720\n"

spec ∷ Spec
spec = do
    describe "legacy fullscreen fallback" $ do
        it "parses when neither window_mode nor fullscreen is present" $ do
            let cfg = parseConfig minimalVideo
            vfWindowMode cfg `shouldBe` Windowed
            -- The point of #433: the rest of the section survives too.
            vfResolution cfg `shouldBe` Resolution 1280 720

        it "maps legacy fullscreen: true to Fullscreen" $ do
            let cfg = parseConfig (minimalVideo <> "  fullscreen: true\n")
            vfWindowMode cfg `shouldBe` Fullscreen

        it "maps legacy fullscreen: false to Windowed" $ do
            let cfg = parseConfig (minimalVideo <> "  fullscreen: false\n")
            vfWindowMode cfg `shouldBe` Windowed

        it "prefers window_mode over the legacy key when both appear" $ do
            let cfg = parseConfig
                    (minimalVideo <> "  window_mode: borderless\n  fullscreen: true\n")
            vfWindowMode cfg `shouldBe` BorderlessWindowed
