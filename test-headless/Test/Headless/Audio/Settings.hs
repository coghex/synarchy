module Test.Headless.Audio.Settings (spec) where

import UPrelude
import Test.Headless.UI.ResponsiveMenus.Fixture
  (withMenusEngine, newBareLuaBackend, evalBool, luaLines)
import Test.Hspec

spec ∷ Spec
spec = around withMenusEngine $ describe "Audio.Settings" $ do
  it "preserves previewed values and labels through a resize without making them saved" $ \env → do
    lua ← newBareLuaBackend env
    result ← evalBool lua $ luaLines
      [ "audio.saveVolumes({master=70,world=80,ui=90});"
      , "local m=require('scripts.settings_menu'); m.init(1,2,3,1280,720);"
      , "local d=require('scripts.settings.data'); d.previewAudio('master',23);"
      , "m.onTabChanged('audio'); m.onFramebufferResize(1600,900);"
      , "local row=m.tabScroll.audio.rowHandles[1]; local info=UI.getElementInfo(row.labelHandle);"
      , "return d.pendingAudio.master==23 and d.currentAudio.master==70 and d.savedAudio.master==70"
      , "  and audio.getStatus().volumes.master==23 and audio.getSavedVolumes().master==70"
      , "  and m.activeTab=='audio' and info.text=='Master 23%'"
      ]
    result `shouldBe` True

  it "keeps Apply and Defaults temporary and lets Back restore the last successful Save" $ \env → do
    lua ← newBareLuaBackend env
    result ← evalBool lua $ luaLines
      [ "audio.saveVolumes({master=70,world=80,ui=90});"
      , "local d=require('scripts.settings.data'); d.reloadAudio();"
      , "d.previewAudio('world',20); d.applyAudio();"
      , "assert(d.currentAudio.world==20 and audio.getSavedVolumes().world==80);"
      , "d.revertAudio(); assert(audio.getStatus().volumes.world==80);"
      , "d.previewAudio('world',30); d.applyAudio(); assert(d.saveAudio());"
      , "d.loadDefaultAudio(); assert(audio.getStatus().volumes.world==audio.getDefaultVolumes().world);"
      , "d.revertAudio(); return audio.getStatus().volumes.world==30 and d.savedAudio.world==30"
      ]
    result `shouldBe` True

  it "retains the saved baseline when an audio write fails" $ \env → do
    lua ← newBareLuaBackend env
    result ← evalBool lua $ luaLines
      [ "audio.saveVolumes({master=70,world=80,ui=90});"
      , "local d=require('scripts.settings.data'); d.reloadAudio();"
      , "d.previewAudio('ui',10); d.applyAudio();"
      , "audio.saveVolumes=function() return false end; assert(d.saveAudio()==false);"
      , "assert(d.savedAudio.ui==90); d.revertAudio(); return audio.getStatus().volumes.ui==90"
      ]
    result `shouldBe` True

  it "keeps all audio tracks positive and inside their content area at narrow high scale" $ \env → do
    lua ← newBareLuaBackend env
    result ← evalBool lua $ luaLines
      [ "engine.setUIScale(4); engine.getTextWidth=function(_,t,size) return #t*size*0.55 end;"
      , "local m=require('scripts.settings_menu'); m.init(1,2,3,800,2160);"
      , "m.onTabChanged('audio'); local tab=m.tabScroll.audio;"
      , "for _,row in ipairs(tab.rowHandles) do"
      , "  for _,handle in ipairs(row.widgetHandles) do local e=UI.getElementInfo(handle);"
      , "    assert(e.width>0 and e.height>0 and e.x>=tab.contentX and e.x+e.width<=tab.contentX+tab.contentW,"
      , "      string.format('%s: x=%g width=%g height=%g content=%g..%g',e.name,e.x,e.width,e.height,tab.contentX,tab.contentX+tab.contentW));"
      , "  end"
      , "end; return #tab.rowHandles==3"
      ]
    result `shouldBe` True
