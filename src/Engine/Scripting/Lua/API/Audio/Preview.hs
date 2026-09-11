-- | Authoring controls are unavailable outside BootPreview. They only name
-- entries the worker published, never arbitrary file paths supplied by Lua.
module Engine.Scripting.Lua.API.Audio.Preview
  ( previewPlayFn, previewStopFn, previewReloadFn ) where

import UPrelude
import Control.Concurrent.STM (STM, atomically)
import Data.IORef (readIORef)
import qualified HsLua as Lua
import Engine.Audio.Preview.Types
import Engine.Audio.Status
import Engine.Audio.Transport
import Engine.Core.Capability.Audio
import Engine.Core.Capability.Core
import Engine.Core.Types (EngineConfig(..), BootProfile(..))
import Engine.Scripting.Lua.API.Audio.Args (readText)

isPreview ∷ CoreCapability → Bool
isPreview = (≡ BootPreview) ∘ ecBootProfile ∘ ccEngineConfig

previewPlayFn ∷ CoreCapability → AudioCapability → Lua.LuaE Lua.Exception Lua.NumResults
previewPlayFn core capability = do
  count ← Lua.gettop
  name ← readText 1
  accepted ← if not (isPreview core) ∨ count ≢ 1 then pure False else case name of
    Left _ → pure False
    Right key → Lua.liftIO $ do
      status ← readIORef (acStatusRef capability)
      if any (\entry → paeId entry ≡ key ∧ paePlayable entry) (audioPreviewEntries status)
        then atomically $ enqueuePreviewPlay (acTransport capability) key
        else pure False
  Lua.pushboolean accepted
  pure 1

previewStopFn, previewReloadFn ∷ CoreCapability → AudioCapability → Lua.LuaE Lua.Exception Lua.NumResults
previewStopFn = controlFn resetAudioSession
previewReloadFn = controlFn reloadAudioPreview

controlFn ∷ (AudioTransport → STM ()) → CoreCapability → AudioCapability
  → Lua.LuaE Lua.Exception Lua.NumResults
controlFn action core capability = do
  count ← Lua.gettop
  let accepted = isPreview core ∧ count ≡ 0
  when accepted $ Lua.liftIO $ atomically $ action (acTransport capability)
  Lua.pushboolean accepted
  pure 1
