module Test.Headless.Capability.Audio (spec) where

import UPrelude
import Control.Concurrent.STM (atomically)
import Control.Exception (finally)
import Engine.Audio.Config.Player
import Engine.Audio.Transport
import Engine.Core.Capability.Audio
import Engine.Core.State (EngineEnv(..))
import Test.Hspec

spec ∷ SpecWith EngineEnv
spec = describe "Audio.Capability" $ do
  it "aliases the status reference" $ \env →
    (acStatusRef (toAudioCapability env) ≡ audioStatusRef env) `shouldBe` True
  it "aliases the opaque transport in both directions" $ \env → do
    let projected = acTransport $ toAudioCapability env
        live = audioTransport env
    old ← atomically $ readAudioVolumes live
    flip finally (atomically $ publishAudioVolumes live old) $ do
      atomically $ publishAudioVolumes projected (Volumes 12 34 56)
      atomically (readAudioVolumes live) `shouldReturn` Volumes 12 34 56
      atomically $ publishAudioVolumes live (Volumes 65 43 21)
      atomically (readAudioVolumes projected) `shouldReturn` Volumes 65 43 21
