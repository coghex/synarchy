module Test.Headless.App.PreviewConfig (spec) where

import UPrelude
import Test.Hspec
import App.Preview.Config (previewWindowConfig)
import Engine.Graphics.Config
  (VideoConfig(..), WindowMode(..), defaultVideoConfig)
import Engine.Graphics.Window.Types (WindowConfig(..))

spec ∷ Spec
spec = describe "preview window presentation" $ do
    it "keeps an ordinary preview visible and focused" $ do
        let config = previewWindowConfig False defaultVideoConfig
        wcVisible config `shouldBe` True
        wcFocused config `shouldBe` True

    it "makes an automated preview hidden and non-activating" $ do
        let fullscreenVideo = defaultVideoConfig
              { vcWindowMode = Fullscreen }
            ordinary = previewWindowConfig False fullscreenVideo
            hidden = previewWindowConfig True fullscreenVideo
        wcFullscreen ordinary `shouldBe` True
        wcFullscreen hidden `shouldBe` False
        wcVisible hidden `shouldBe` False
        wcFocused hidden `shouldBe` False
        (wcWidth hidden, wcHeight hidden, wcTitle hidden,
         wcResizable hidden) `shouldBe`
          (wcWidth ordinary, wcHeight ordinary, wcTitle ordinary,
           wcResizable ordinary)

    -- #1731 gave the preview's ordinary path a persisted borderless
    -- request. The hidden automated presentation must suppress it for
    -- the same reason it suppresses fullscreen: a probe window that
    -- went undecorated and monitor-sized would not be the ordinary
    -- window the surface/swapchain/resize path is being exercised on.
    it "passes a persisted borderless request through only when visible" $ do
        let borderlessVideo = defaultVideoConfig
              { vcWindowMode = BorderlessWindowed }
        wcBorderless (previewWindowConfig False borderlessVideo)
            `shouldBe` True
        wcBorderless (previewWindowConfig True borderlessVideo)
            `shouldBe` False
