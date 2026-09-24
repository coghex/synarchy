-- | Real debug overlay, both press subscribers and load/view teardown.
-- Engine queries are controlled; gesture and outcome owners are production Lua.
module Test.Headless.Lua.DebugGrab (spec) where

import UPrelude
import Test.Hspec
import Control.Exception (bracket)
import qualified HsLua as Lua
import qualified Data.Text as T
import Engine.Scripting.Lua.Thread.Console (executeDebugLua)

runCase ∷ Text → Expectation
runCase body = bracket Lua.newstate Lua.close $ \state → do
    Lua.runWith state Lua.openlibs
    result ← executeDebugLua state (fixture <> "\n" <> body <> "\nreturn true")
    result `shouldBe` "true"

fixture ∷ Text
fixture = T.unlines
    [ "local function noop() end"
    , "PAGE, PICKPAGE, GEN, WIDTH = 'arena', 'arena', 1, 0"
    , "PICK = {10.75, 20.75}"
    , "ROW = {id=7, instanceId=77, x=10.25, y=20.5}"
    , "MOUSE, WIN, FB = {500,400}, {1280,720}, {2560,1440}"
    , "MOVES, RECORDS, HITS, LEAKS = {}, {}, 0, 0"
    , "VALID, HIT, MODAL, BLOCKED, GAME = true, true, false, false, true"
    , "engine = {getWindowSize=function() return table.unpack(WIN) end,"
    , " getFramebufferSize=function() return table.unpack(FB) end,"
    , " getMousePosition=function() return table.unpack(MOUSE) end,"
    , " getUIScale=function() return 1 end, getFPS=function() return 60 end,"
    , " getTextWidth=function(_,s) return #s*16 end,"
    , " loadFont=function() return 1 end, logDebug=noop, logInfo=noop,"
    , " logError=noop, isKeyDown=function() return false end}"
    , "UI = setmetatable({isInputBlocked=function() return MODAL end,"
    , " isPointerBlockedAt=function(x,y)"
    , "   assert(x==MOUSE[1]*FB[1]/WIN[1] and y==MOUSE[2]*FB[2]/WIN[2])"
    , "   return MODAL or BLOCKED"
    , " end}, {__index=function() return function() return 1 end end})"
    , "debug.recordOutcome=function(r) RECORDS[#RECORDS+1]=r end"
    , "world = {getActiveWorldId=function() return PAGE end,"
    , " pickPos=function() if PICK then return PICK[1],PICK[2],PICKPAGE,GEN end end,"
    , " getWrapWidth=function() return WIDTH end,"
    , " getHoverPos=function() error('cached hover must never be read') end}"
    , "item = {hitTestAt=function() HITS=HITS+1; if HIT then return 7 end end,"
    , " listGround=function() return ROW and {ROW} or {} end,"
    , " debugMoveGround=function(gid,iid,x,y,page)"
    , "   MOVES[#MOVES+1]={x=x,y=y,page=page,gid=gid,iid=iid}"
    , "   assert(page==PAGE and gid==ROW.id and iid==ROW.instanceId)"
    , "   if VALID then ROW.x,ROW.y=x,y; return true end"
    , "   return false"
    , " end}"
    , "unit, building = {}, {}"
    , "local hud={currentView='zoomed_in',onMouseDown=function() LEAKS=LEAKS+1 end}"
    , "package.loaded['scripts.hud']=hud"
    , "UIM={fbW=FB[1],fbH=FB[2],isGameplayView=function() return GAME end,"
    , " isGameplayInputActive=function() return GAME and not MODAL end}"
    , "package.loaded['scripts.ui_manager']=UIM"
    , "for _,name in ipairs({'textbox','checkbox','button','dropdown','scrollbar',"
    , " 'tabbar','slider','randbox','toggle','list','item_list','context_menu','focus_indicator'}) do"
    , " package.loaded['scripts.ui.'..name]=setmetatable({}, {__index=function() return noop end})"
    , "end"
    , "for _,name in ipairs({'build_tool','mine_tool','chop_tool','till_tool','plant_tool'}) do"
    , " package.loaded['scripts.'..name]={handleMouseDown=function() LEAKS=LEAKS+1; return true end}"
    , "end"
    , "package.loaded['scripts.debug_anim_panel']={tryClaimClick=function() return false end}"
    , "D=require('scripts.debug'); D.init(); D.show()"
    , "DS=require('scripts.unit_drag_select'); DS.edgeIds={}"
    , "ROUTER=require('scripts.init_mouse')"
    , "require('scripts.ui_manager_widgets')"
    , "function press(order,button)"
    , " button=button or 1"
    , " if order=='ui-first' then UIM.onMouseDown(button,table.unpack(MOUSE)) end"
    , " ROUTER.onMouseDown(button,table.unpack(MOUSE))"
    , " if order~='ui-first' then UIM.onMouseDown(button,table.unpack(MOUSE)) end"
    , "end"
    , "function release(route,button)"
    , " button=button or 1"
    , " D.onMouseUp(button,MOUSE[1],MOUSE[2],route or 'game')"
    , " DS.onMouseUp(button,MOUSE[1],MOUSE[2],route or 'game')"
    , "end"
    , "function arm() D.setArmedGrab(true) end"
    , "function move(x,y) PICK={x,y}; D.update(.03) end"
    , "function near(a,b) assert(math.abs(a-b)<.000001,tostring(a)..' ~= '..tostring(b)) end"
    , "function only(n) assert(#RECORDS==(n or 1), 'outcomes='..#RECORDS); assert(LEAKS==0,'leaked input'); assert(not DS.boxSelectArmed) end"
    ]

spec ∷ Spec
spec = describe "Debug Grab gesture (#2489)" $ do
    it "claims exactly once with ui-first" $
        runCase "arm(); press('ui-first'); assert(HITS==1); move(12.75,23.75); near(ROW.x,12.25); near(ROW.y,23.5); release(); only(); assert(D.armedGrab); press('ui-first'); assert(HITS==2); release(); only(2)"
    it "claims exactly once with game-first" $
        runCase "arm(); press('game-first'); assert(HITS==1); move(12.75,23.75); near(ROW.x,12.25); near(ROW.y,23.5); release(); only(); assert(D.armedGrab); press('game-first'); assert(HITS==2); release(); only(2)"
    it "toggles the real button and uses the armed label treatment" $
        runCase "local rect=D.clickableRects[#D.clickableRects]; MOUSE={(rect.x+4)/2,(rect.y+4)/2}; press(); assert(D.armedGrab); local mode=D.modeOrder[#D.modeOrder]; assert(require('scripts.ui.label').getText(mode.buttonId)=='> Grab'); release(); press(); assert(not D.armedGrab); release(); only(2)"
    it "excludes every placement category in both directions" $
        runCase "for _,mode in ipairs(D.modeOrder) do if mode.key~='grab' then D[mode.armedField]=true end end; arm(); for _,mode in ipairs(D.modeOrder) do if mode.key~='grab' then assert(not D[mode.armedField]) end end; for _,name in ipairs({'setArmed','setArmedFluid','setArmedItem','setArmedTerrain','setArmedLocation','setArmedStructure'}) do arm(); D[name]('value'); assert(not D.armedGrab,name) end"
    it "owns empty ground and sprite hits without usable ground" $
        runCase "for _,kind in ipairs({'empty','no-pick'}) do HIT=kind~='empty'; PICK=kind~='no-pick' and {10,20} or nil; arm(); press(); move(15,25); release(); assert(#MOVES==0) end; only(2)"
    it "resumes after nil picks and destination refusals" $
        runCase "arm(); press(); PICK=nil; D.update(.03); assert(#MOVES==0); VALID=false; move(15.75,25.75); near(ROW.x,10.25); VALID=true; move(17.75,27.75); near(ROW.x,17.25); release(); only()"
    it "applies the final pick even without an update" $
        runCase "arm(); press(); PICK={16.75,30.75}; MOUSE={650,500}; release(); near(ROW.x,16.25); near(ROW.y,30.5); only()"
    it "ends an invalid release without another move" $
        runCase "arm(); press(); move(12.75,23.75); PICK=nil; release(); move(20,30); assert(#MOVES==1); only()"
    it "ignores unmatched and non-left releases" $
        runCase "arm(); release(); assert(#MOVES==0); press(); release('game',3); move(12.75,23.75); assert(#MOVES==1); release(); only()"
    it "cancels a matching swallowed release" $
        runCase "arm(); press(); move(12.75,23.75); release('swallowed'); move(40,50); assert(#MOVES==1); only()"
    it "cancels a matching ui release" $
        runCase "arm(); press(); move(12.75,23.75); release('ui'); move(40,50); assert(#MOVES==1); only()"
    it "leaves later updates inert after D.onKeyDown('Escape')" $
        runCase "arm(); press(); move(12.75,23.75); D.onKeyDown('Escape'); move(40,50); release(); assert(#MOVES==1); only()"
    it "leaves later updates inert after D.hide()" $
        runCase "arm(); press(); move(12.75,23.75); D.hide(); move(40,50); release(); assert(#MOVES==1); only()"
    it "leaves later updates inert after D.setArmedItem('lantern')" $
        runCase "arm(); press(); move(12.75,23.75); D.setArmedItem('lantern'); move(40,50); release(); assert(#MOVES==1); only()"
    it "leaves later updates inert after D.onFramebufferResize(1280,720)" $
        runCase "arm(); press(); move(12.75,23.75); D.onFramebufferResize(1280,720); move(40,50); release(); assert(#MOVES==1); only()"
    it "leaves later updates inert after D.onFramebufferResize(0,0)" $
        runCase "arm(); press(); move(12.75,23.75); D.onFramebufferResize(0,0); move(40,50); release(); assert(#MOVES==1); only()"
    it "leaves later updates inert after require('scripts.lib.session_teardown').runAll()" $
        runCase "arm(); press(); move(12.75,23.75); require('scripts.lib.session_teardown').runAll(); move(40,50); release(); assert(#MOVES==1); only()"
    it "leaves later updates inert after D.createUI()" $
        runCase "arm(); press(); move(12.75,23.75); D.createUI(); move(40,50); release(); assert(#MOVES==1); only()"
    it "right-click cancels before designation handlers in either subscriber order" $
        runCase "for _,order in ipairs({'ui-first','game-first'}) do arm(); press(order); move(12.75,23.75); local n=#MOVES; press(order,2); move(40,50); release(); release('game',2); assert(#MOVES==n); assert(not D.armedGrab) end; only(4)"
    it "clears through real load apply and reconciliation even with reused ids" $
        runCase "arm(); press(); move(12.75,23.75); local save=require('scripts.lib.save_modules'); local snap=save.snapshotAll(); assert(snap.ok); assert(save.prepareLoad(snap.components,{}).ok); save.applyAll(); ROW={id=7,instanceId=77,x=1,y=2}; require('scripts.ui.view_teardown').run('saveLoaded',{}); move(40,50); release(); assert(#MOVES==1); near(ROW.x,1); only()"
    it "clears through the actual zoomBand debug-overlay hook" $
        runCase "arm(); press(); move(12.75,23.75); local vt=require('scripts.ui.view_teardown'); for _,failure in ipairs(vt.run('zoomBand',{newView='zoomed_out'})) do assert(failure.name~='debug_overlay',failure.err) end; move(40,50); release(); assert(#MOVES==1); only()"
    it "clears through the actual hudHide debug-overlay hook" $
        runCase "arm(); press(); move(12.75,23.75); local vt=require('scripts.ui.view_teardown'); for _,failure in ipairs(vt.run('hudHide',{newView='zoomed_out'})) do assert(failure.name~='debug_overlay',failure.err) end; move(40,50); release(); assert(#MOVES==1); only()"
    it "clears through the actual menu debug-overlay hook" $
        runCase "arm(); press(); move(12.75,23.75); local vt=require('scripts.ui.view_teardown'); for _,failure in ipairs(vt.run('menu',{newView='zoomed_out'})) do assert(failure.name~='debug_overlay',failure.err) end; move(40,50); release(); assert(#MOVES==1); only()"
    it "blocks capture and held/final movement under pointer-blocking UI" $
        runCase "arm(); BLOCKED=true; assert(not D.tryClaimGrab(1,500,400)); BLOCKED=false; press(); move(12.75,23.75); BLOCKED=true; move(20,30); release(); assert(#MOVES==1); only()"
    it "freezes under a modal but keeps debug controls usable above it" $
        runCase "arm(); MODAL=true; assert(not D.tryClaimGrab(1,500,400)); local rect=D.clickableRects[#D.clickableRects]; assert(D.tryClaimClick(1,(rect.x+4)/2,(rect.y+4)/2)); assert(not D.armedGrab); D.onMouseUp(1,0,0,'game'); MODAL=false; arm(); press(); move(12.75,23.75); MODAL=true; move(20,30); release(); assert(#MOVES==1); only()"
    it "ends capture when the item disappears or its identity changes" $
        runCase "for _,kind in ipairs({'missing','replaced'}) do ROW={id=7,instanceId=77,x=10.25,y=20.5}; arm(); press(); if kind=='missing' then ROW=nil else ROW.instanceId=88 end; D.update(.03); ROW={id=7,instanceId=77,x=1,y=2}; move(40,50); release() end; assert(#MOVES==0); only(2)"
    it "never mixes active item and visible pick pages" $
        runCase "arm(); PICKPAGE='other'; press(); move(20,30); release(); assert(#MOVES==0); PICKPAGE=PAGE; press(); PAGE='other'; D.update(.03); PAGE='arena'; move(20,30); release(); assert(#MOVES==0); only(2)"
    it "rejects a different pick page and selection generation during a hold" $
        runCase "for _,kind in ipairs({'page','generation'}) do PICKPAGE=PAGE; arm(); press(); if kind=='page' then PICKPAGE='other' else GEN=GEN+2 end; move(20,30); PICKPAGE=PAGE; move(40,50); release() end; assert(#MOVES==0); only(2)"
    it "preserves fractional offsets through seam direction -1" $
        runCase "WIDTH=1024; ROW.x,ROW.y=255.25,-255.5; PICK={255.75,-255.25}; arm(); press(); move(255.75+(-1)*512+1,-255.25-(-1)*512+2); near(ROW.x,256.25); near(ROW.y,-253.5); release(); only()"
    it "preserves fractional offsets through seam direction 1" $
        runCase "WIDTH=1024; ROW.x,ROW.y=255.25,-255.5; PICK={255.75,-255.25}; arm(); press(); move(255.75+(1)*512+1,-255.25-(1)*512+2); near(ROW.x,256.25); near(ROW.y,-253.5); release(); only()"
    it "preserves offset when the stored item already uses a seam alias" $
        runCase "WIDTH=1024; ROW.x,ROW.y=767.25,-767.5; PICK={255.75,-255.25}; arm(); press(); move(-255.25,257.75); near(ROW.x,256.25); near(ROW.y,-254.5); release(); only()"
    it "does not wrap arena positions" $
        runCase "arm(); press(); move(600.75,-500.25); near(ROW.x,600.25); near(ROW.y,-500.5); release(); only()"
