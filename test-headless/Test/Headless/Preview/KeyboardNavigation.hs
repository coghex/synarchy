-- | CPU-only keyboard-navigation gate for @--preview@ (#2026).
--
-- The scroll/dump cases deliberately run the REAL
-- @scripts/ui/list.lua@ and @scripts/ui/asset_browser.lua@. The shared
-- list's ordinary selection primitive must stay non-scrolling for its
-- save/settings/menu consumers; only the preview browser adds the
-- minimum scroll-into-view step before routing through that same
-- click-equivalent selection call.
module Test.Headless.Preview.KeyboardNavigation (spec) where

import UPrelude
import Test.Hspec
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified HsLua as Lua

runsOk ∷ Text → Expectation
runsOk chunkText = do
    result ← Lua.run $ do
        Lua.openlibs
        status ← Lua.dostring (TE.encodeUtf8 chunkText)
        case status of
            Lua.OK → pure Nothing
            _ → do
                err ← Lua.tostring (-1)
                pure (Just (maybe "<no message>" TE.decodeUtf8Lenient err))
    case result of
        Nothing → pure ()
        Just msg → expectationFailure (T.unpack msg)

lns ∷ [Text] → Text
lns = T.intercalate "\n"

-- A small stateful UI oracle sufficient for the real list, scrollbar,
-- browser, and unit-direction view. dump() reads the same element records
-- these setters mutate, so visible-row/highlight assertions are read-backs
-- of rendered state rather than parallel test bookkeeping.
uiStub ∷ Text
uiStub = lns
    [ "elements = {}"
    , "local nextElem = 1"
    , "local function elem(name, w, h, text)"
    , "  local id = nextElem; nextElem = nextElem + 1"
    , "  elements[id] = {name=name, x=0, y=0, width=w or 1, height=h or 1,"
    , "      text=text or '', visible=true, clickable=false, page=1}"
    , "  return id"
    , "end"
    , "UI = {"
    , "  newPage=function() return 1 end, showPage=function() end,"
    , "  deletePage=function() end,"
    , "  newElement=function(name,w,h) return elem(name,w,h) end,"
    , "  newSprite=function(name,w,h) return elem(name,w,h) end,"
    , "  newBox=function(name,w,h) return elem(name,w,h) end,"
    , "  newText=function(name,text) return elem(name,1,1,text) end,"
    , "  addToPage=function(page,id,x,y)"
    , "      local e=elements[id]; if e then e.page=page; e.x=x; e.y=y end end,"
    , "  addChild=function(parent,id,x,y)"
    , "      local p,e=elements[parent],elements[id]"
    , "      if e then e.x=(p and p.x or 0)+x; e.y=(p and p.y or 0)+y end end,"
    , "  deleteElement=function(id) elements[id]=nil end,"
    , "  setSize=function(id,w,h) local e=elements[id]; if e then e.width=w; e.height=h end end,"
    , "  setPosition=function(id,x,y) local e=elements[id]; if e then e.x=x; e.y=y end end,"
    , "  setVisible=function(id,v) local e=elements[id]; if e then e.visible=v end end,"
    , "  setClickable=function(id,v) local e=elements[id]; if e then e.clickable=v end end,"
    , "  setText=function(id,t) local e=elements[id]; if e then e.text=t end end,"
    , "  setSpriteTexture=function() end, setSpriteFrame=function() end,"
    , "  setColor=function() end, setOnClick=function() end,"
    , "  setScrollCapture=function() end, setDragActivation=function() end,"
    , "  setPointerBlocking=function() end, setClipChildren=function() end,"
    , "  setZIndex=function() end,"
    , "  getElementInfo=function(id)"
    , "      local e=elements[id]; if not e then return nil end"
    , "      return {name=e.name,x=e.x,y=e.y,width=e.width,height=e.height,"
    , "          text=e.text,visible=e.visible,clickable=e.clickable,"
    , "          hovered=false,focused=false,pageVisible=true,page=e.page,handle=id}"
    , "  end,"
    , "}"
    ]

engineStub ∷ Text
engineStub = lns
    [ "local nextTexture = 100"
    , "engine = {"
    , "  loadTexture=function() nextTexture=nextTexture+1; return nextTexture end,"
    , "  loadFont=function() return 10 end,"
    , "  getTextureSize=function() return {width=64,height=64} end,"
    , "  getTextWidth=function(_,text,size) return #text * (size or 1) * 0.5 end,"
    , "  getUIScale=function() return 1 end,"
    , "  getFramebufferSize=function() return 1000,800 end,"
    , "  getWindowSize=function() return 1000,800 end,"
    , "  getMousePosition=function() return 0,0 end,"
    , "  logDebug=function() end, logInfo=function() end,"
    , "  logWarn=function() end, logError=function() end,"
    , "  quit=function() QUIT_CALLS=(QUIT_CALLS or 0)+1 end,"
    , "  setTextureFilter=function() end, realTime=function() return NOW or 10 end,"
    , "  getPreviewBrowse=function() return BROWSE end,"
    , "  getPreviewTarget=function() return TARGET end,"
    , "}"
    ]

realBrowserHarness ∷ Text
realBrowserHarness = lns
    [ uiStub
    , engineStub
    , "package.loaded['scripts.ui.box_textures'] = {load=function() return 90 end}"
    , "package.loaded['scripts.ui.scale'] = {get=function() return 1 end}"
    , "package.loaded['scripts.ui.list'] = dofile('scripts/ui/list.lua')"
    , "package.loaded['scripts.ui.asset_browser'] = dofile('scripts/ui/asset_browser.lua')"
    , "browser = package.loaded['scripts.ui.asset_browser']"
    , "browser.init()"
    ]

unitHarness ∷ Text
unitHarness = lns
    [ uiStub
    , engineStub
    , "package.loaded['scripts.ui.scale'] = {get=function() return 1 end}"
    , "package.loaded['scripts.ui.preview_zoom'] = dofile('scripts/ui/preview_zoom.lua')"
    , "unitView = dofile('scripts/ui/unit_animation_view.lua')"
    , "local function frame() return {path='atlas.png',u0=0,v0=0,u1=1,v1=1,width=32,height=48} end"
    , "local function dir(direction,source,mirrored)"
    , "  return {direction=direction,source=source,mirrored=mirrored,"
    , "          frames={frame(),frame(),frame(),frame()}}"
    , "end"
    ]

-- preview_manager is real here; its collaborators are narrow spies so
-- this group separately proves routing plus the preview-local held-key
-- clock (the shared input thread suppresses OS-specific repeat events).
managerHarness ∷ Text
managerHarness = lns
    [ uiStub
    , engineStub
    , "BROWSER_STEPS, DIRECTION_STEPS = {}, {}"
    , "local browsers = {}"
    , "local nextBrowser = 1"
    , "assetBrowserStub = {"
    , "  init=function() end,"
    , "  new=function(p) local id=nextBrowser; nextBrowser=nextBrowser+1;"
    , "      browsers[id]={p=p,index=nil,scroll=0}; return id end,"
    , "  getPanelBounds=function() return {x=340,y=40,width=620,height=720} end,"
    , "  selectEntry=function(id,path)"
    , "      local b=browsers[id]; if not b then return end"
    , "      local idx=1; for i,e in ipairs(b.p.entries or {}) do if e.path==path then idx=i end end"
    , "      b.index=idx; local e=(b.p.entries or {})[idx]"
    , "      if e and b.p.onSelect then b.p.onSelect(e.path,e.label,idx) end"
    , "  end,"
    , "  selectEntrySilently=function(id,path)"
    , "      local b=browsers[id]; for i,e in ipairs(b.p.entries or {}) do if e.path==path then b.index=i end end end,"
    , "  selectAdjacent=function(id,step)"
    , "      local b=browsers[id]; if not b or not b.index then return false end"
    , "      local target=b.index+step; if target<1 or target>#b.p.entries then return false end"
    , "      b.index=target; table.insert(BROWSER_STEPS,step); local e=b.p.entries[target]"
    , "      if b.p.onSelect then b.p.onSelect(e.path,e.label,target) end; return true"
    , "  end,"
    , "  getSelectedPath=function(id) local b=browsers[id]; local e=b and b.p.entries[b.index]; return e and e.path end,"
    , "  getSelectedLabel=function(id) local b=browsers[id]; local e=b and b.p.entries[b.index]; return e and e.label end,"
    , "  getScrollOffset=function(id) local b=browsers[id]; return b and b.scroll or 0 end,"
    , "  setScrollOffset=function(id,o) browsers[id].scroll=o end,"
    , "  dump=function() return {} end, destroy=function(id) browsers[id]=nil end,"
    , "  handleCallback=function() return false end, onScroll=function() return false end,"
    , "}"
    , "unitViewStub = {"
    , "  new=function(p) UNIT_PANEL=p.panel; UNIT_ZOOM=p.zoom; return 1 end,"
    , "  setAnimation=function(_,a) UNIT_ANIM=a.name; UNIT_ANIM_SETS=(UNIT_ANIM_SETS or 0)+1 end,"
    , "  selectAdjacentDirection=function(_,step) table.insert(DIRECTION_STEPS,step); return true end,"
    , "  getZoomRegion=function() return UNIT_PANEL end, setZoom=function(_,z) UNIT_ZOOM=z end,"
    , "  setPanel=function(_,p) UNIT_PANEL=p end, setDirection=function() return true end,"
    , "  update=function() end, destroy=function() end, handleCellClick=function() return nil end,"
    , "  dump=function() return {ready=true,direction='south',directions={},"
    , "      zoom={multiplier=UNIT_ZOOM,region=UNIT_PANEL}} end,"
    , "}"
    , "buildingViewStub = {"
    , "  new=function(p) BUILD_PANEL=p.panel; BUILD_ZOOM=p.zoom; return 1 end,"
    , "  setEntry=function(_,e) BUILD_ENTRY=e.label; BUILD_ENTRY_SETS=(BUILD_ENTRY_SETS or 0)+1 end,"
    , "  getZoomRegion=function() return BUILD_PANEL end, setZoom=function(_,z) BUILD_ZOOM=z end,"
    , "  setPanel=function(_,p) BUILD_PANEL=p end, update=function() end, destroy=function() end,"
    , "  dump=function() return {ready=true,animated=false,zoom={multiplier=BUILD_ZOOM,region=BUILD_PANEL}} end,"
    , "}"
    , "package.loaded['scripts.ui.asset_browser']=assetBrowserStub"
    , "package.loaded['scripts.ui.list']={getChromeTexture=function() return 90 end}"
    , "package.loaded['scripts.ui.unit_animation_view']=unitViewStub"
    , "package.loaded['scripts.ui.building_asset_view']=buildingViewStub"
    , "package.loaded['scripts.ui.preview_zoom']=dofile('scripts/ui/preview_zoom.lua')"
    , "function bootManager(browse,target)"
    , "  BROWSE,TARGET=browse,target"
    , "  package.loaded['scripts.preview_manager']=nil"
    , "  dofile('scripts/preview_manager.lua')"
    , "  local pm=package.loaded['scripts.preview_manager']"
    , "  pm.init(1); pm.onAssetLoaded('font',10,'assets/fonts/arcade.ttf')"
    , "  return pm"
    , "end"
    ]

spec ∷ Spec
spec = do
    describe "real preview browser adjacency" $ do
        it "selects through the click-equivalent callback, clamps without re-firing, and minimally scrolls the real visible rows/dump" $ runsOk $ lns
            [ realBrowserHarness
            , "local entries={} for i=1,6 do entries[i]={label='item'..i,path='p'..i} end"
            , "local picked={}"
            , "local id=browser.new({page=1,x=10,y=20,width=500,height=60,"
            , "    itemHeight=20,maxVisible=3,entries=entries,"
            , "    onSelect=function(path,_,index) table.insert(picked,path..':'..index) end})"
            , "browser.selectEntry(id,'p1')"
            , "assert(browser.selectAdjacent(id,1) and browser.getSelectedPath(id)=='p2')"
            , "assert(browser.getScrollOffset(id)==0,'row 2 is already visible')"
            , "assert(browser.selectAdjacent(id,1) and browser.getSelectedPath(id)=='p3')"
            , "assert(browser.getScrollOffset(id)==0,'row 3 is already visible')"
            , "assert(browser.selectAdjacent(id,1) and browser.getSelectedPath(id)=='p4')"
            , "assert(browser.getScrollOffset(id)==1,'row 4 needs exactly offset 1')"
            , "local rows=browser.dump(id)"
            , "assert(#rows==3 and rows[1].label=='item2' and rows[3].label=='item4')"
            , "assert(rows[1].value==false and rows[2].value==false and rows[3].value==true,"
            , "    'dump highlight must agree with selected item')"
            , "assert(browser.selectAdjacent(id,1) and browser.getScrollOffset(id)==2)"
            , "assert(browser.selectAdjacent(id,1) and browser.getScrollOffset(id)==3)"
            , "local before=#picked; assert(not browser.selectAdjacent(id,1))"
            , "assert(#picked==before and browser.getSelectedPath(id)=='p6',"
            , "    'Down at the last row is a true no-op')"
            , "for _=1,5 do assert(browser.selectAdjacent(id,-1)) end"
            , "before=#picked; assert(not browser.selectAdjacent(id,-1))"
            , "assert(#picked==before and browser.getSelectedPath(id)=='p1',"
            , "    'Up at the first row is a true no-op')"
            , "assert(picked[1]=='p1:1' and picked[2]=='p2:2',"
            , "    'keyboard used the ordinary onSelect path')"
            ]

        it "ignores empty and not-yet-selected lists" $ runsOk $ lns
            [ realBrowserHarness
            , "local empty=browser.new({page=1,x=0,y=0,width=300,height=60,entries={}})"
            , "assert(not browser.selectAdjacent(empty,1))"
            , "local idle=browser.new({page=1,x=0,y=80,width=300,height=60,"
            , "    entries={{label='only',path='only'}}})"
            , "assert(not browser.selectAdjacent(idle,1) and browser.getSelectedPath(idle)==nil)"
            ]

    describe "unit direction adjacency" $ do
        it "wraps in displayed order through mirrored cells while preserving playback phase and zoom" $ runsOk $ lns
            [ unitHarness
            , "local id=unitView.new({page=1,font=1,panel={x=0,y=0,width=600,height=500},"
            , "    requestTexture=function() return 101 end,chromeTexture=90,zoom=0.5})"
            , "local anim={name='idle',fps=10,loop=true,flip=true,atlas='atlas.png',directions={"
            , "    dir('south','south',false), dir('south-west','south-east',true),"
            , "    dir('west','east',true), dir('north-west','north-east',true),"
            , "    dir('north','north',false)}}"
            , "unitView.setAnimation(id,anim,10,nil); unitView.update(id,10.25)"
            , "local before=unitView.dump(id)"
            , "assert(before.direction=='south' and before.frameIndex==2)"
            , "assert(unitView.selectAdjacentDirection(id,-1))"
            , "local wrapped=unitView.dump(id)"
            , "assert(wrapped.direction=='north','Left from first must wrap to last')"
            , "assert(wrapped.frameIndex==2 and wrapped.zoom.multiplier==0.5)"
            , "assert(unitView.selectAdjacentDirection(id,1))"
            , "assert(unitView.dump(id).direction=='south','Right from last must wrap to first')"
            , "assert(unitView.selectAdjacentDirection(id,1))"
            , "local mirrored=unitView.dump(id)"
            , "assert(mirrored.direction=='south-west' and mirrored.mirrored==true"
            , "    and mirrored.sourceDirection=='south-east',"
            , "    'mirrored displayed cells are first-class navigation targets')"
            , "unitView.update(id,10.25)"
            , "assert(unitView.dump(id).frameIndex==2 and unitView.dump(id).zoom.multiplier==0.5,"
            , "    'direction navigation must not restart the clock or reset zoom')"
            ]

        it "ignores an animation with no direction cells" $ runsOk $ lns
            [ unitHarness
            , "local id=unitView.new({page=1,font=1,panel={x=0,y=0,width=600,height=500},"
            , "    requestTexture=function() return 101 end,chromeTexture=90,zoom=1})"
            , "unitView.setAnimation(id,{name='empty',fps=8,directions={}},10,nil)"
            , "assert(not unitView.selectAdjacentDirection(id,1))"
            ]

    describe "preview-manager key routing" $ do
        it "routes every shared list mode through browser adjacency and keeps bare/grouped zoom identity semantics" $ do
            runsOk $ lns
                [ managerHarness
                , "local entries={{label='a',path='a.png'},{label='b',path='b.png'}}"
                , "local pm=bootManager({mode='list',entries=entries},{category='icons'})"
                , "pm.onUIScroll(pm.dump().zoom.surface,0,2)"
                , "assert(pm.dump().zoom.multiplier < 1)"
                , "assert(pm.onKeyDown('Down') and BROWSER_STEPS[1]==1)"
                , "assert(pm.dump().selected.path=='b.png' and pm.dump().zoom.multiplier==1,"
                , "    'a keyboard-selected bare texture is a new preview object')"
                , "assert(not pm.onKeyDown('Left') and not pm.onKeyDown('Right'))"
                ]
            runsOk $ lns
                [ managerHarness
                , "local entries={{label='stage0',path='stage0.png'},{label='stage1',path='stage1.png'}}"
                , "local pm=bootManager({mode='list',entries=entries},{category='flora',item='oak'})"
                , "pm.onUIScroll(pm.dump().zoom.surface,0,2)"
                , "local held=pm.dump().zoom.multiplier"
                , "assert(pm.onKeyDown('Down') and pm.dump().selected.path=='stage1.png')"
                , "assert(pm.dump().zoom.multiplier==held,"
                , "    'another grouped-object stage preserves zoom')"
                ]

        it "routes unit Up/Down to animation selection and Left/Right only to displayed directions" $ runsOk $ lns
            [ managerHarness
            , "local function anim(n) return {name=n,fps=8,loop=true,flip=true,directions={}} end"
            , "local pm=bootManager({mode='unit',unit={name='u',defaultAnim='idle',"
            , "    animations={anim('idle'),anim('walk')}}},{category='units',item='u'})"
            , "assert(UNIT_ANIM_SETS==1)"
            , "assert(pm.onKeyDown('Down') and BROWSER_STEPS[1]==1 and UNIT_ANIM_SETS==2)"
            , "assert(pm.onKeyDown('Left') and pm.onKeyDown('Right'))"
            , "assert(DIRECTION_STEPS[1]==-1 and DIRECTION_STEPS[2]==1 and UNIT_ANIM_SETS==2,"
            , "    'direction changes do not reselect/restart the animation')"
            ]

        it "routes building Up/Down through the same selection path and ignores Left/Right" $ runsOk $ lns
            [ managerHarness
            , "local entries={{label='idle',frames={'a.png'}},{label='default.png',frames={'b.png'}}}"
            , "local pm=bootManager({mode='building',building={name='b',defaultEntry='idle',"
            , "    entries=entries}},{category='buildings',item='b'})"
            , "assert(BUILD_ENTRY_SETS==1 and pm.onKeyDown('Down'))"
            , "assert(BROWSER_STEPS[1]==1 and BUILD_ENTRY_SETS==2)"
            , "assert(not pm.onKeyDown('Left') and not pm.onKeyDown('Right'))"
            ]

        it "moves immediately, repeats after a short delay at a fast fixed cadence, and stops exactly on key-up" $ runsOk $ lns
            [ managerHarness
            , "NOW=100"
            , "local entries={} for i=1,5 do entries[i]={label='e'..i,path='p'..i} end"
            , "local pm=bootManager({mode='list',entries=entries},{category='icons'})"
            , "assert(pm.onKeyDown('Down') and pm.dump().selected.path=='p2')"
            , "NOW=100.19; pm.update(0.016)"
            , "assert(#BROWSER_STEPS==1 and pm.dump().selected.path=='p2',"
            , "    'the initial delay must not repeat early')"
            , "NOW=100.21; pm.update(0.016)"
            , "assert(#BROWSER_STEPS==2 and pm.dump().selected.path=='p3')"
            , "NOW=100.249; pm.update(0.016)"
            , "assert(#BROWSER_STEPS==2,'the fixed interval must not repeat early')"
            , "NOW=100.251; pm.update(0.016)"
            , "assert(#BROWSER_STEPS==3 and pm.dump().selected.path=='p4')"
            , "assert(pm.onKeyUp('Down'))"
            , "NOW=101; pm.update(0.016)"
            , "assert(#BROWSER_STEPS==3 and pm.dump().selected.path=='p4',"
            , "    'release must stop the held-key clock')"
            , "assert(not pm.onKeyUp('Up'),'an unrelated release is a no-op')"
            , "NOW=102; assert(pm.onKeyDown('Down') and pm.dump().selected.path=='p5')"
            , "NOW=102.21; pm.update(0.016)"
            , "assert(#BROWSER_STEPS==4 and pm.dump().selected.path=='p5',"
            , "    'a repeat that reaches the list boundary fires no callback')"
            ]

        it "applies the same held-key clock to wrapped unit directions" $ runsOk $ lns
            [ managerHarness
            , "NOW=200"
            , "local function anim(n) return {name=n,fps=8,loop=true,flip=true,directions={}} end"
            , "local pm=bootManager({mode='unit',unit={name='u',defaultAnim='idle',"
            , "    animations={anim('idle')}}},{category='units',item='u'})"
            , "assert(pm.onKeyDown('Left') and #DIRECTION_STEPS==1 and DIRECTION_STEPS[1]==-1)"
            , "NOW=200.19; pm.update(0.016); assert(#DIRECTION_STEPS==1)"
            , "NOW=200.21; pm.update(0.016)"
            , "assert(#DIRECTION_STEPS==2 and DIRECTION_STEPS[2]==-1)"
            , "assert(pm.onKeyUp('Left'))"
            , "NOW=201; pm.update(0.016); assert(#DIRECTION_STEPS==2)"
            ]

        it "closes preview on Escape and cancels any held-arrow repeat" $ runsOk $ lns
            [ managerHarness
            , "NOW=300"
            , "local entries={{label='a',path='a'},{label='b',path='b'},{label='c',path='c'}}"
            , "local pm=bootManager({mode='list',entries=entries},{category='icons'})"
            , "assert(pm.onKeyDown('Down') and #BROWSER_STEPS==1)"
            , "assert(pm.onKeyDown('Escape') and QUIT_CALLS==1)"
            , "NOW=301; pm.update(0.016)"
            , "assert(#BROWSER_STEPS==1 and pm.dump().selected.path=='b',"
            , "    'Escape must cancel the held-arrow clock before quitting')"
            , "assert(not pm.onKeyUp('Down'))"
            ]

        it "ignores all four arrows in focused-item mode" $ runsOk $ lns
            [ managerHarness
            , "local pm=bootManager({mode='item',entry={label='a',path='a.png'}},"
            , "    {category='icons',item='a.png'})"
            , "for _,k in ipairs({'Up','Down','Left','Right'}) do assert(not pm.onKeyDown(k)) end"
            , "assert(#BROWSER_STEPS==0 and #DIRECTION_STEPS==0)"
            ]
