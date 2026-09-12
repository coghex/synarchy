module Test.Headless.Audio.Integration (spec) where

import UPrelude
import Control.Concurrent.STM (atomically)
import Data.IORef (readIORef)
import qualified HsLua as Lua
import Engine.Audio.Config.Player
import Engine.Audio.Transport
import Engine.Audio.Types
import Engine.Core.Capability.Audio
import Engine.Core.Capability.WorldSim
import Engine.Core.State (EngineEnv, loggerRef, loadStatusRef)
import Engine.Load.Status (beginLoad, failLoad)
import Engine.Scripting.Lua.API.Core (setPausedFn)
import Test.Headless.Harness (withHeadlessEngineNoWorld)
import World.Load.Publish (resetTransientState)
import World.Pause (imposePause, releasePause)
import World.Thread.Command.Basic (handleWorldDestroyAllCommand)
import Test.Hspec

drain ∷ AudioTransport → IO [AudioRequest]
drain transport = map stampedRequest ∘ batchRequests <$> atomically (readAudioBatch transport 1024)

setPaused ∷ EngineEnv → Bool → IO Bool
setPaused env paused = Lua.run @Lua.Exception $ do
  Lua.pushboolean paused
  void $ setPausedFn env
  Lua.toboolean (-1)

spec ∷ Spec
spec = describe "Audio.Integration" $ do
  it "publishes only accepted player pause calls, including the load gate" $ withHeadlessEngineNoWorld $ \env → do
    let transport = acTransport (toAudioCapability env)
    void $ drain transport
    setPaused env True `shouldReturn` True
    drain transport `shouldReturn` [AudioSetPlayerPaused True]
    Right request ← beginLoad (loadStatusRef env) "audio_pause_gate"
    setPaused env False `shouldReturn` False
    drain transport `shouldReturn` []
    failLoad (loadStatusRef env) request "fixture completed"
    setPaused env False `shouldReturn` True
    drain transport `shouldReturn` [AudioSetPlayerPaused False]

  it "does not turn internal engine pause into an audio freeze" $ withHeadlessEngineNoWorld $ \env → do
    let transport = acTransport (toAudioCapability env)
    void $ drain transport
    imposePause (toWorldSimCapability env)
    releasePause (toWorldSimCapability env)
    drain transport `shouldReturn` []

  it "invalidates old requests at load reset and world destruction, retaining player volumes" $ withHeadlessEngineNoWorld $ \env → do
    let transport = acTransport (toAudioCapability env)
    atomically $ do
      setAudioAvailable transport True
      void $ enqueuePlay transport "menu_selected" defaultTriggerOptions
      publishAudioVolumes transport (Volumes 23 45 67)
    resetTransientState env
    first ← atomically $ readAudioBatch transport 1024
    batchEpoch first `shouldBe` 1
    map stampedRequest (batchRequests first) `shouldBe` [AudioVolumes (Volumes 23 45 67), AudioResetSession]
    transportStaleDrops (batchStats first) `shouldBe` 1
    atomically (readAudioVolumes transport) `shouldReturn` Volumes 23 45 67
    logger ← readIORef (loggerRef env)
    handleWorldDestroyAllCommand env logger
    atomically (readAudioEpoch transport) `shouldReturn` 2
    drain transport `shouldReturn` [AudioResetSession]
