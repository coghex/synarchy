module Engine.Scripting.Lua.API.Register.Audio (registerAudioAPI) where

import Engine.Core.Capability.Audio (AudioCapability)
import Engine.Core.Capability.Core (CoreCapability)
import Engine.Scripting.Lua.API.Audio
import Engine.Scripting.Lua.API.Audio.Preview
import Engine.Scripting.Lua.API.Internal (registerLuaFunction)
import Engine.Scripting.Lua.CallStats (LuaCallStats)
import qualified HsLua as Lua

registerAudioAPI ∷ LuaCallStats → CoreCapability → AudioCapability → Lua.LuaE Lua.Exception ()
registerAudioAPI stats core audio = do
  Lua.newtable
  registerLuaFunction stats "audio" "play" (playFn core audio)
  registerLuaFunction stats "audio" "startLoop" (startLoopFn core audio)
  registerLuaFunction stats "audio" "updateLoop" (updateLoopFn core audio)
  registerLuaFunction stats "audio" "stopLoop" (stopLoopFn core audio)
  registerLuaFunction stats "audio" "getStatus" (statusFn audio)
  registerLuaFunction stats "audio" "getSavedVolumes" savedVolumesFn
  registerLuaFunction stats "audio" "getDefaultVolumes" defaultVolumesFn
  registerLuaFunction stats "audio" "setVolumes" (setVolumesFn core audio)
  registerLuaFunction stats "audio" "saveVolumes" (saveVolumesFn core audio)
  registerLuaFunction stats "audio" "previewPlay" (previewPlayFn core audio)
  registerLuaFunction stats "audio" "previewStop" (previewStopFn core audio)
  registerLuaFunction stats "audio" "previewReload" (previewReloadFn core audio)
  Lua.setglobal (Lua.Name "audio")
