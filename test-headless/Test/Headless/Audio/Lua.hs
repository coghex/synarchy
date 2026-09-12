module Test.Headless.Audio.Lua (spec) where

import UPrelude
import Control.Concurrent.STM (atomically)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified HsLua as Lua
import Engine.Audio.Transport
import Engine.Core.Capability.Audio
import Engine.Core.Capability.Core
import Engine.Scripting.Lua.API.Register.Audio (registerAudioAPI)
import Engine.Scripting.Lua.CallStats (newLuaCallStats)
import Test.Headless.Harness (withHeadlessEngineNoWorld)
import Test.Headless.Harness.Isolation (withIsolatedResourceRoot)
import Test.Hspec

runs ∷ Text → IO ()
runs source = withIsolatedResourceRoot $ withHeadlessEngineNoWorld $ \env → do
  let capability = toAudioCapability env
  atomically $ setAudioAvailable (acTransport capability) True
  errorMessage ← Lua.run @Lua.Exception $ do
    Lua.openlibs
    stats ← Lua.liftIO newLuaCallStats
    registerAudioAPI stats (toCoreCapability env) capability
    result ← Lua.dostring (Text.encodeUtf8 source)
    if result ≡ Lua.OK then pure Nothing
    else fmap Text.decodeUtf8Lenient <$> Lua.tostring (-1)
  maybe (pure ()) (expectationFailure ∘ Text.unpack) errorMessage

spec ∷ Spec
spec = describe "Audio.Lua" $ do
  it "refuses preview-only replacement and reload outside preview mode" $ runs $ Text.unlines
    [ "local epoch=audio.getStatus().epoch"
    , "assert(audio.previewPlay('menu_selected')==false)"
    , "assert(audio.previewStop()==false and audio.previewReload()==false)"
    , "assert(audio.getStatus().epoch==epoch)"
    ]
  it "accepts named requests and keeps options closed without running metamethods" $ runs $ Text.unlines
    [ "assert(audio.play('menu_selected'))"
    , "assert(audio.play('world_test', {pageId='world',position={x=1,y=2,z=3},gainDb=-4,pitchSemitones=2}))"
    , "assert(audio.startLoop('forge', 'world_test', {pageId='world',position={x=1,y=2,z=3}}))"
    , "assert(audio.updateLoop('forge', {gainDb=-2}))"
    , "assert(audio.stopLoop('forge'))"
    , "local options = setmetatable({}, {__index=function() error('must not evaluate') end})"
    , "assert(audio.play('menu_selected', options))"
    ]
  it "returns false for malformed arguments and survives cyclic tables and numeric overflow" $ runs $ Text.unlines
    [ "local bad = {false, 3, 'options', {gainDb='1'}, {gainDb=13}, {pitchSemitones=-25},"
    , "  {bus='ui'}, {source='path'}, {position={x=1,y=2,z=3}}, {pageId='world'},"
    , "  {pageId='world',position={x=1e300,y=2,z=3}}, {gainDb=0/0}, {gainDb=math.huge}}"
    , "for _,v in ipairs(bad) do local ok,value=pcall(audio.play,'menu_selected',v); assert(ok and value==false) end"
    , "assert(audio.play(123)==false and audio.play('Bad-ID')==false)"
    , "assert(audio.play('menu_selected',{},7)==false)"
    , "local cycle={}; cycle.position=cycle; assert(audio.play('menu_selected',cycle)==false)"
    , "assert(audio.updateLoop('x',{pitchSemitones=1})==false)"
    , "assert(audio.stopLoop('')==false and audio.startLoop('x','y',true)==false)"
    , "assert(audio.play('menu_selected'))"
    ]
  it "previews volumes separately from sparse durable saves, rejecting malformed writes" $ runs $ Text.unlines
    [ "local saved=audio.getSavedVolumes()"
    , "assert(audio.setVolumes({master=13,world=37,ui=71}))"
    , "assert(audio.getStatus().volumes.master==13)"
    , "assert(audio.getSavedVolumes().master==saved.master)"
    , "for _,v in ipairs({{master=2}, {master=-1,world=50,ui=50}, {master=1.5,world=50,ui=50},"
    , "  {master=1,world=2,ui=3,extra=4}}) do assert(audio.saveVolumes(v)==false) end"
    , "assert(audio.getStatus().volumes.master==13)"
    , "assert(audio.saveVolumes({master=14,world=38,ui=72}))"
    , "assert(audio.getSavedVolumes().master==14 and audio.getStatus().volumes.master==14)"
    , "assert(audio.getStatus().lifecycle=='disabled')"
    ]
