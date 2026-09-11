module Engine.Scripting.Lua.API.Register.Audio (registerAudioAPI) where

import Engine.Core.Capability.Audio (AudioCapability)
import Engine.Core.Capability.Core (CoreCapability)
import Engine.Scripting.Lua.API.Audio
import Engine.Scripting.Lua.API.Audio.Preview
import Engine.Scripting.Lua.API.Internal (registerLuaFunction)
import Engine.Scripting.Lua.CallStats (LuaCallStats)
import qualified HsLua as Lua

registerAudioAPI ∷ LuaCallStats → CoreCapability → AudioCapability → Lua.LuaE Lua.Exception ()
registerAudioAPI callStats core audio = do
  Lua.newtable
  registerLuaFunction callStats "audio" "play" (playFn core audio)
  registerLuaFunction callStats "audio" "startLoop" (startLoopFn core audio)
  registerLuaFunction callStats "audio" "updateLoop" (updateLoopFn core audio)
  registerLuaFunction callStats "audio" "stopLoop" (stopLoopFn core audio)
  registerLuaFunction callStats "audio" "getStatus" (statusFn audio)
  registerLuaFunction callStats "audio" "getSavedVolumes" savedVolumesFn
  registerLuaFunction callStats "audio" "getDefaultVolumes" defaultVolumesFn
  registerLuaFunction callStats "audio" "setVolumes" (setVolumesFn core audio)
  registerLuaFunction callStats "audio" "saveVolumes" (saveVolumesFn core audio)
  registerLuaFunction callStats "audio" "previewPlay" (previewPlayFn core audio)
  registerLuaFunction callStats "audio" "previewStop" (previewStopFn core audio)
  registerLuaFunction callStats "audio" "previewReload" (previewReloadFn core audio)
  Lua.setglobal (Lua.Name "audio")
