module Test.Headless.Audio.PreviewUI (spec) where

import UPrelude
import Test.Headless.Preview.LuaHarness (runsOk, lns, uiStub, engineStub)
import Test.Hspec

fixture ∷ Text
fixture = uiStub <> "\n" <> engineStub <> "\n" <> lns
  [ "local shown, hidden = {}, {}"
  , "local newText=UI.newText; UI.newText=function(...) assert(type(select(9,...))=='number','text needs page'); return newText(...) end"
  , "local newElement=UI.newElement; UI.newElement=function(...) assert(type(select(4,...))=='number','element needs page'); return newElement(...) end"
  , "local nextPage=10; UI.newPage=function() nextPage=nextPage+1; return nextPage end"
  , "UI.showPage=function(id) shown[id]=true; hidden[id]=false end"
  , "UI.hidePage=function(id) hidden[id]=true; shown[id]=false end"
  , "local current={lifecycle='running_null',previewRevision=1,snapshotSequence=1,"
  , "  volumes={master=80,world=90,ui=100},previewEntries={"
  , "  {id='a',label='menu_back',category='synth',playable=true},"
  , "  {id='b',label='menu_selected',category='synth',playable=true},"
  , "  {id='c',label='bear.wav',category='files',path='/tmp/bear.wav',playable=true}}}"
  , "local plays, stops, reloads, quit = {},0,0,false"
  , "audio={getStatus=function() return current end,"
  , "  previewPlay=function(id) plays[#plays+1]=id; return true end,"
  , "  previewStop=function() stops=stops+1; return true end,"
  , "  previewReload=function() reloads=reloads+1; return true end,"
  , "  setVolumes=function(value) current.volumes=value; return true end}"
  , "engine.quit=function() quit=true end"
  , "local pane=require('scripts.ui.preview_audio')"
  ]

spec ∷ Spec
spec = describe "Audio.PreviewUI" $ do
  it "keeps a real bottom-left hit target in visual preview, switches pages and stops on return" $
    runsOk $ fixture <> "\n" <> lns
      [ "pane.init(1,1,{mode='list'})"
      , "local d=pane.dump(); assert(not d.open)"
      , "assert(d.footer.bounds.width>0 and d.footer.bounds.height>0 and d.footer.bounds.x==12)"
      , "local _,h=engine.getFramebufferSize(); assert(d.footer.bounds.y==h-36)"
      , "assert(pane.click(d.footer.handle)); assert(hidden[1] and pane.isOpen())"
      , "d=pane.dump(); assert(#d.rows==2 and #plays==0)"
      , "pane.click(d.rows[2].handle); assert(plays[1]=='b')"
      , "pane.resize(1280,720); assert(pane.dump().selected=='b' and #plays==1)"
      , "pane.key('Escape'); assert(not pane.isOpen() and shown[1] and stops==1)"
      , "pane.shutdown()"
      ]
  it "autoplays a CLI file once, preserves it through resize and supports playback controls" $
    runsOk $ fixture <> "\n" <> lns
      [ "pane.init(1,1,{mode='audio',category='files',file='/tmp/bear.wav'})"
      , "assert(pane.dump().selected=='c' and #plays==0)"
      , "pane.update(); pane.update(); assert(#plays==1 and plays[1]=='c')"
      , "pane.resize(800,600); pane.update(); assert(#plays==1)"
      , "pane.click(pane.dump().buttons.preview_audio_play.handle); assert(#plays==2)"
      , "pane.click(pane.dump().buttons.preview_audio_stop.handle); assert(stops==1)"
      , "pane.click(pane.dump().buttons.preview_audio_quieter.handle); assert(current.volumes.master==70)"
      , "pane.key('Escape'); assert(quit)"
      , "pane.shutdown()"
      ]
  it "waits for a new catalog revision, restores selection by source and never replays on Reload" $
    runsOk $ fixture <> "\n" <> lns
      [ "pane.init(1,1,{mode='audio',category='files'})"
      , "pane.play(); assert(plays[1]=='c')"
      , "assert(pane.reload()); assert(not pane.reload()); assert(reloads==1)"
      , "current.snapshotSequence=20; pane.update(); assert(pane.dump().reloading)"
      , "assert(not pane.play()); assert(#plays==1)"
      , "current.previewEntries[3].id='new_id'; current.previewRevision=2"
      , "pane.update(); assert(not pane.dump().reloading and pane.dump().selected=='new_id')"
      , "assert(#plays==1); pane.play(); assert(plays[2]=='new_id')"
      , "pane.shutdown()"
      ]
  it "shows an unavailable file without sending playback and keeps synth category usable" $
    runsOk $ fixture <> "\n" <> lns
      [ "current.previewEntries[3].playable=false; current.lastError='invalid WAV'"
      , "pane.init(1,1,{mode='audio',category='files',file='/tmp/bear.wav'}); pane.update()"
      , "assert(#plays==0 and pane.dump().state:find('invalid WAV',1,true))"
      , "pane.chooseCategory('synth'); pane.key('Down'); assert(plays[1]=='b')"
      , "pane.shutdown()"
      ]
  it "does not mistake an old disabled snapshot for completion of a retry" $
    runsOk $ fixture <> "\n" <> lns
      [ "current.lifecycle='disabled'; current.lastError='device unavailable'"
      , "pane.init(1,1,{mode='audio',category='synth'})"
      , "pane.reload(); pane.update(); assert(pane.dump().reloading)"
      , "current.snapshotSequence=20; pane.update(); assert(pane.dump().reloading)"
      , "current.previewRevision=2; pane.update(); assert(not pane.dump().reloading)"
      , "assert(pane.dump().state:find('device unavailable',1,true))"
      , "pane.shutdown()"
      ]
