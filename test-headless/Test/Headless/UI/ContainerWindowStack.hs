-- | The nested container-window stack (#1238, epic #1013 phase 1):
--   @scripts/cargo_inventory_panel.lua@ owns an ordered stack of levels,
--   only the deepest of which is interactive.
--
--   Same technique as 'Test.Headless.UI.ResponsiveGameplay': one shared
--   headless engine and one shared bare Lua VM, with the REAL production
--   modules driven on a real page tree and synthetic texture/font
--   handles. The engine reads a level's data comes from
--   (@building.getContainerKnowledge@, @building.getRememberedItemContents@,
--   @unit.getItemContents@, @unit.transferEndpointInfo@) are stubbed per
--   case, because what is under test is the WINDOW behaviour — the verbs
--   themselves are gated by 'Test.Headless.Item.NestedContents' against
--   real live refs.
--
--   Modality is asserted through the engine's own page-ownership
--   answer (@UI.isInputBlocked@ / @UI.isPageInScope@,
--   "UI.InputOwnership"), never through bookkeeping the window keeps
--   about itself: the whole design claim is that a deeper level is
--   input-exclusive because #742 already makes a 'LayerModal' page one.
--
--   Run just this gate: @cabal test synarchy-test-headless
--   --test-options='--match "container window stack"'@.
module Test.Headless.UI.ContainerWindowStack (spec) where

import UPrelude
import Test.Hspec
import Data.Aeson (FromJSON(..), decode, withObject, (.:), (.:?), (.!=))
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.ByteString.Lazy as BL
import Data.IORef (newIORef, writeIORef, atomicModifyIORef')
import Engine.Core.State (EngineEnv(..))
import Engine.Core.Thread (ThreadControl(..))
import Engine.Graphics.Config (vcUIScale)
import Engine.Scripting.Lua.API (registerLuaAPI)
import Engine.Scripting.Lua.Thread (createLuaBackendState)
import Engine.Scripting.Lua.Thread.Console (executeDebugLua)
import Engine.Scripting.Lua.Types (LuaBackendState(..))
import Test.Headless.Harness (withHeadlessEngine)
import UI.Types (emptyUIPageManager)

luaLines ∷ [Text] → Text
luaLines = T.intercalate " "

withSharedFixture ∷ ((EngineEnv, LuaBackendState) → IO ()) → IO ()
withSharedFixture action = withHeadlessEngine $ \env → do
    ls ← newBareLuaBackend env
    action (env, ls)

resetFixture ∷ EngineEnv → LuaBackendState → IO ()
resetFixture env ls = do
    writeIORef (uiManagerRef env) emptyUIPageManager
    atomicModifyIORef' (videoConfigRef env) $ \c → (c { vcUIScale = 1.0 }, ())
    cleared ← evalOk ls
        "for k, _ in pairs(package.loaded) do package.loaded[k] = nil end; return true"
    cleared `shouldBe` "true"

newBareLuaBackend ∷ EngineEnv → IO LuaBackendState
newBareLuaBackend env = do
    ls ← createLuaBackendState (luaToEngineQueue env) (luaQueue env)
                                (assetPoolRef env) (nextObjectIdRef env)
                                (inputStateRef env) (loggerRef env)
    stateRef ← newIORef ThreadRunning
    registerLuaAPI (lbsLuaState ls) env ls stateRef
    pure ls

evalOk ∷ LuaBackendState → Text → IO Text
evalOk ls src = do
    r ← executeDebugLua (lbsLuaState ls) src
    r `shouldNotSatisfy` isLuaError
    pure r

evalJSON ∷ LuaBackendState → Text → IO Text
evalJSON = evalOk

isLuaError ∷ Text → Bool
isLuaError t = "error:" `T.isPrefixOf` t ∨ "syntax error:" `T.isPrefixOf` t

-- | The scene every case starts from: a visible overlay page standing in
--   for @hud.world_page@, the window manager pointed at it, and stubs
--   for every engine read a level performs.
--
--   The building endpoint remembers three rows, ONE of which is an
--   item-container (@toolbox@, instance 77). Its remembered nested
--   contents hold a second container (@case@, instance 88) so a THIRD
--   level has somewhere to go. Every remembered read answers the same
--   @revealedAt@, which is the point: a nested level inherits its
--   parent's observation time.
--
--   @__reveals@ / @__liveStorage@ count the two things a remembered
--   level must never do.
sceneLua ∷ Text
sceneLua = luaLines
    [ "_G.__reveals = 0; _G.__liveStorage = 0;"
    , "building.refreshContainerKnowledge = function()"
    , "    _G.__reveals = _G.__reveals + 1; return true end;"
    , "building.getStorage = function()"
    , "    _G.__liveStorage = _G.__liveStorage + 1; return {} end;"
    , "building.getInfo = function() return { displayName='Cargo Hold' } end;"
    , "building.getContainerKnowledge = function() return"
    , "  { state='known', storedWeight=12.0, capacity=400.0, revealedAt=100.0,"
    , "    items = {"
    , "      { defName='ration', displayName='Ration', category='Food',"
    , "        kind='food', instanceId=11, weight=0.5 },"
    , "      { defName='toolbox', displayName='Toolbox', category='Tools',"
    , "        kind='container', instanceId=77, weight=3.0 },"
    , "      { defName='crate', displayName='Crate', category='Tools',"
    , "        kind='container', instanceId=78, weight=4.0 } } } end;"
    , "building.getRememberedItemContents = function(_, path)"
    , "    if path == nil or #path == 0 then return nil end;"
    , "    if #path == 1 and path[1] == 77 then return { revealedAt = 100.0,"
    , "        items = { { defName='case', displayName='Case', kind='container',"
    , "                    instanceId=88, count=1, weight=1.0 } } } end;"
    , "    if #path == 1 and path[1] == 78 then return { revealedAt = 100.0,"
    , "        items = { { defName='nail', displayName='Nail', kind='misc',"
    , "                    instanceId=90, count=9, weight=0.01 } } } end;"
    , "    if #path == 2 and path[1] == 77 and path[2] == 88 then"
    , "      return { revealedAt = 100.0, items = {"
    , "        { defName='pin', displayName='Pin', kind='misc',"
    , "          instanceId=91, count=2, weight=0.01 } } } end;"
    , "    return nil end;"
    , "unit.getInfo = function() return { name='Ada' } end;"
    , "unit.transferEndpointInfo = function() return { eligible=true,"
    , "    displayName='Acolyte', capacity=50.0, storedWeight=7.0,"
    , "    contents = {"
    , "      { defName='kit', displayName='Kit', category='Medical',"
    , "        kind='container', instanceId=55, weight=1.0 },"
    , "      { defName='rope', displayName='Rope', category='Tools',"
    , "        kind='misc', instanceId=56, weight=2.0 } } } end;"
    , "unit.getItemContents = function(_, _, iid, path)"
    , "    if iid ~= 55 then return nil end;"
    , "    if path and #path == 1 and path[1] == 66 then return {"
    , "        { defName='swab', displayName='Swab', kind='misc',"
    , "          instanceId=67, count=4, weight=0.01 } } end;"
    , "    if path and #path > 0 then return nil end;"
    , "    return { { defName='splint', displayName='Splint', kind='container',"
    , "               instanceId=66, count=1, weight=0.3 },"
    , "             { defName='gauze', displayName='Gauze', kind='misc',"
    , "               instanceId=68, count=2, weight=0.02 } } end;"
    , "engine.gameTime = function() return 160.0 end;"
    , "_G.__page = UI.newPage('cw_stack_base', 'overlay'); UI.showPage(_G.__page);"
    , "local cip = require('scripts.cargo_inventory_panel');"
    , "cip.setup({page = _G.__page, fbW = 1920, fbH = 1080,"
    , "           boxTexSet = 1, menuFont = 1});"
    -- The one gesture the whole stack is driven by: right-click a row of
    -- level `lvl`, then fire whichever menu entry the level offered by
    -- LABEL. Routing goes through the real widget callback and the real
    -- context menu, so nothing here reaches past a player's own reach.
    , "_G.__menu = function(lvl, rowIdx)"
    , "  local il = require('scripts.ui.item_list');"
    , "  local cm = require('scripts.ui.context_menu');"
    , "  local level = cip.getLevel(lvl); if not level then return nil end;"
    , "  local rows = il.getRows(level.listId);"
    , "  local row = rows[rowIdx]; if not row then return nil end;"
    , "  local captured, orig = nil, cm.show;"
    , "  cm.show = function(items) captured = items end;"
    , "  il.handleCallback('onItemListRightClick', row.hitId);"
    , "  cm.show = orig;"
    , "  return captured end;"
    , "_G.__fire = function(lvl, rowIdx, label)"
    , "  local items = _G.__menu(lvl, rowIdx); if not items then return false end;"
    , "  for _, e in ipairs(items) do"
    , "    if e.label == label and e.callback then e.callback(); return true end"
    , "  end; return false end;"
    , "_G.__labels = function(lvl, rowIdx)"
    , "  local items = _G.__menu(lvl, rowIdx) or {};"
    , "  local out = {}; for i, e in ipairs(items) do out[i] = e.label end;"
    , "  return table.concat(out, '|') end;"
    , "return true"
    ]

setupScene ∷ LuaBackendState → IO ()
setupScene ls = do
    r ← evalOk ls sceneLua
    r `shouldBe` "true"

-- * Decoders

data StackProbe = StackProbe
    { spDepth ∷ Int, spKinds ∷ Text, spTitles ∷ Text
    , spPaths ∷ Text } deriving Show
instance FromJSON StackProbe where
    parseJSON = withObject "StackProbe" $ \o →
        StackProbe <$> o .: "depth" <*> o .: "kinds" <*> o .: "titles"
                   <*> o .:? "paths" .!= ""

data ModalProbe = ModalProbe
    { mpBlockedAlone ∷ Bool, mpBlockedNested ∷ Bool
    , mpBaseScopeAlone ∷ Bool, mpBaseScopeNested ∷ Bool
    , mpDeepScopeNested ∷ Bool, mpBlockedAfterPop ∷ Bool
    , mpBaseScopeAfterPop ∷ Bool, mpModalFlags ∷ Text } deriving Show
instance FromJSON ModalProbe where
    parseJSON = withObject "ModalProbe" $ \o →
        ModalProbe <$> o .: "blockedAlone" <*> o .: "blockedNested"
                   <*> o .: "baseScopeAlone" <*> o .: "baseScopeNested"
                   <*> o .: "deepScopeNested" <*> o .: "blockedAfterPop"
                   <*> o .: "baseScopeAfterPop" <*> o .: "modalFlags"

data AgeProbe = AgeProbe
    { apParentAge ∷ Text, apChildAge ∷ Text, apGrandAge ∷ Text
    , apRows ∷ Text, apReveals ∷ Int, apLiveStorage ∷ Int } deriving Show
instance FromJSON AgeProbe where
    parseJSON = withObject "AgeProbe" $ \o →
        AgeProbe <$> o .: "parentAge" <*> o .: "childAge" <*> o .: "grandAge"
                 <*> o .: "rows" <*> o .: "reveals" <*> o .: "liveStorage"

data EscapeProbe = EscapeProbe
    { epDepths ∷ Text, epConsumed ∷ Text, epScopes ∷ Text } deriving Show
instance FromJSON EscapeProbe where
    parseJSON = withObject "EscapeProbe" $ \o →
        EscapeProbe <$> o .: "depths" <*> o .: "consumed" <*> o .: "scopes"

data ResizeProbe = ResizeProbe
    { rpDepthBefore ∷ Int, rpDepthAfter ∷ Int
    , rpPathsBefore ∷ Text, rpPathsAfter ∷ Text
    , rpScrollBefore ∷ Text, rpScrollAfter ∷ Text
    , rpTabBefore ∷ Text, rpTabAfter ∷ Text } deriving Show
instance FromJSON ResizeProbe where
    parseJSON = withObject "ResizeProbe" $ \o →
        ResizeProbe <$> o .: "depthBefore" <*> o .: "depthAfter"
                    <*> o .: "pathsBefore" <*> o .: "pathsAfter"
                    <*> o .: "scrollBefore" <*> o .: "scrollAfter"
                    <*> o .: "tabBefore" <*> o .: "tabAfter"

data MenuProbe = MenuProbe
    { mnBuildingRow ∷ Text, mnBuildingPlain ∷ Text
    , mnUnitRow ∷ Text, mnItemRow ∷ Text
    , mnItemPlain ∷ Text } deriving Show
instance FromJSON MenuProbe where
    parseJSON = withObject "MenuProbe" $ \o →
        MenuProbe <$> o .: "buildingRow" <*> o .: "buildingPlain"
                  <*> o .: "unitRow" <*> o .: "itemRow" <*> o .: "itemPlain"

data ResetProbe = ResetProbe
    { rsDepthBefore ∷ Int, rsDepthAfter ∷ Int
    , rsListsBefore ∷ Int, rsListsAfter ∷ Int
    , rsPagesGone ∷ Bool, rsBlockedAfter ∷ Bool } deriving Show
instance FromJSON ResetProbe where
    parseJSON = withObject "ResetProbe" $ \o →
        ResetProbe <$> o .: "depthBefore" <*> o .: "depthAfter"
                   <*> o .: "listsBefore" <*> o .: "listsAfter"
                   <*> o .: "pagesGone" <*> o .: "blockedAfter"

-- | The stack's shape, as three parallel joined strings.
dumpStackLua ∷ Text
dumpStackLua = luaLines
    [ "local d = cip.dump();"
    , "local kinds, titles, paths = {}, {}, {};"
    , "for i, l in ipairs(d.levels) do"
    , "  kinds[i] = l.kind; titles[i] = tostring(l.title);"
    , "  paths[i] = table.concat(l.path or {}, '.');"
    , "end;"
    ]

-- * Spec

spec ∷ Spec
spec = aroundAll withSharedFixture $

  describe "the nested container window stack (#1238)" $ do

    describe "level targeting and replacement" $ do

        it "an external request targets the BASE level, a container row \
           \targets its owning level plus one, and each level's path \
           \descends by exact instance identity" $ \(env, ls) → do
            resetFixture env ls
            setupScene ls
            r ← evalJSON ls $ luaLines
                [ "local cip = require('scripts.cargo_inventory_panel');"
                , "cip.openFor('building', 42, 300, 300);"
                , "_G.__fire(1, 2, 'Contents');"   -- the toolbox row
                , "_G.__fire(2, 1, 'Contents');"   -- the case inside it
                , dumpStackLua
                , "return {depth = d.depth, kinds = table.concat(kinds, '|'),"
                , "        titles = table.concat(titles, '|'),"
                , "        paths = table.concat(paths, '|')}"
                ]
            case decode (BL.fromStrict (TE.encodeUtf8 r)) ∷ Maybe StackProbe of
                Nothing → expectationFailure ("failed to decode: " ⧺ T.unpack r)
                Just p → do
                    spDepth p `shouldBe` 3
                    spKinds p `shouldBe` "endpoint|buildingItem|buildingItem"
                    spTitles p `shouldBe` "Cargo Hold|Toolbox|Case"
                    spPaths p `shouldBe` "|77|77.88"

        -- Driven through the row menu of a level that is NOT the
        -- deepest, which a real pointer cannot reach (the modal
        -- boundary is exactly what makes a shallower level inert, and
        -- tools/item_list_widget_probe.py proves that with real
        -- clicks). The CONTRACT still has to hold for the code path:
        -- `openLevel(src, mx, my, parentIndex)` is public, and Mode A's
        -- own session level will call it.
        it "opening a second container at level N replaces level N and \
           \DISCARDS every deeper level, while every level below N \
           \survives untouched" $ \(env, ls) → do
            resetFixture env ls
            setupScene ls
            r ← evalJSON ls $ luaLines
                [ "local cip = require('scripts.cargo_inventory_panel');"
                , "cip.openFor('building', 42, 300, 300);"
                , "_G.__fire(1, 2, 'Contents');"
                , "_G.__fire(2, 1, 'Contents');"
                -- Now open the OTHER container from level 1: level 2 is
                -- replaced and level 3 must be gone with it.
                , "_G.__fire(1, 3, 'Contents');"
                , dumpStackLua
                , "return {depth = d.depth, kinds = table.concat(kinds, '|'),"
                , "        titles = table.concat(titles, '|'),"
                , "        paths = table.concat(paths, '|')}"
                ]
            case decode (BL.fromStrict (TE.encodeUtf8 r)) ∷ Maybe StackProbe of
                Nothing → expectationFailure ("failed to decode: " ⧺ T.unpack r)
                Just p → do
                    spDepth p `shouldBe` 2
                    spTitles p `shouldBe` "Cargo Hold|Crate"
                    spPaths p `shouldBe` "|78"

        it "an external request while a stack is open starts over at the \
           \base — the deeper levels were addressed through an endpoint \
           \that is no longer open" $ \(env, ls) → do
            resetFixture env ls
            setupScene ls
            r ← evalJSON ls $ luaLines
                [ "local cip = require('scripts.cargo_inventory_panel');"
                , "cip.openFor('building', 42, 300, 300);"
                , "_G.__fire(1, 2, 'Contents');"
                , "cip.openFor('unit', 5, 300, 300);"
                , dumpStackLua
                , "return {depth = d.depth, kinds = table.concat(kinds, '|'),"
                , "        titles = table.concat(titles, '|'),"
                , "        paths = table.concat(paths, '|')}"
                ]
            case decode (BL.fromStrict (TE.encodeUtf8 r)) ∷ Maybe StackProbe of
                Nothing → expectationFailure ("failed to decode: " ⧺ T.unpack r)
                Just p → do
                    spDepth p `shouldBe` 1
                    spKinds p `shouldBe` "endpoint"
                    spTitles p `shouldBe` "Ada"

        it "a unit-carried container opens from BOTH gestures — the \
           \unit-info Contents entry at the base, and a container row \
           \inside a unit endpoint's own window — and renders LIVE rows" $ \(env, ls) → do
            resetFixture env ls
            setupScene ls
            r ← evalJSON ls $ luaLines
                [ "local cip = require('scripts.cargo_inventory_panel');"
                , "local il = require('scripts.ui.item_list');"
                -- Gesture 1: the unit-info inventory row's Contents.
                , "local icp = require('scripts.item_contents_panel');"
                , "icp.openFor(5, 'kit', 300, 300, 55, 'Kit');"
                , "local d1 = cip.dump();"
                -- Gesture 2: through the unit ENDPOINT window's own row.
                , "cip.openFor('unit', 5, 300, 300);"
                , "_G.__fire(1, 1, 'Contents');"
                -- and one level deeper, into the splint inside the kit.
                , "_G.__fire(2, 1, 'Contents');"
                , dumpStackLua
                , "local names = {};"
                , "for _, row in ipairs(il.getRows(cip.getLevel(3).listId)) do"
                , "  names[#names+1] = row.item.defName end;"
                , "return {depth = d.depth, kinds = table.concat(kinds, '|'),"
                , "        titles = table.concat(titles, '|') .. '/'"
                , "                 .. tostring(d1.depth) .. ':'"
                , "                 .. tostring(d1.levels[1].kind),"
                , "        paths = table.concat(names, ',')}"
                ]
            case decode (BL.fromStrict (TE.encodeUtf8 r)) ∷ Maybe StackProbe of
                Nothing → expectationFailure ("failed to decode: " ⧺ T.unpack r)
                Just p → do
                    -- Gesture 1 opened a unit-item level AT THE BASE.
                    spTitles p `shouldBe` "Ada|Kit|Splint/1:unitItem"
                    spDepth p `shouldBe` 3
                    spKinds p `shouldBe` "endpoint|unitItem|unitItem"
                    -- The deepest level renders the LIVE nested contents.
                    spPaths p `shouldBe` "swab"

    describe "modality" $ do

        it "the base level is non-modal and interactive alone; a deeper \
           \level is a LayerModal page that takes the boundary, putting \
           \the base out of scope; closing it gives the base back" $ \(env, ls) → do
            resetFixture env ls
            setupScene ls
            r ← evalJSON ls $ luaLines
                [ "local cip = require('scripts.cargo_inventory_panel');"
                , "cip.openFor('building', 42, 300, 300);"
                , "local blockedAlone = UI.isInputBlocked();"
                , "local baseScopeAlone = cip.dump().levels[1].pageInScope;"
                , "_G.__fire(1, 2, 'Contents');"
                , "local d = cip.dump();"
                , "local flags = {};"
                , "for i, l in ipairs(d.levels) do"
                , "  flags[i] = l.modal and '1' or '0' end;"
                , "local blockedNested = UI.isInputBlocked();"
                , "local baseScopeNested = d.levels[1].pageInScope;"
                , "local deepScopeNested = d.levels[2].pageInScope;"
                , "cip.popLevel();"
                , "return {blockedAlone = blockedAlone,"
                , "        blockedNested = blockedNested,"
                , "        baseScopeAlone = baseScopeAlone,"
                , "        baseScopeNested = baseScopeNested,"
                , "        deepScopeNested = deepScopeNested,"
                , "        blockedAfterPop = UI.isInputBlocked(),"
                , "        baseScopeAfterPop = cip.dump().levels[1].pageInScope,"
                , "        modalFlags = table.concat(flags, '')}"
                ]
            case decode (BL.fromStrict (TE.encodeUtf8 r)) ∷ Maybe ModalProbe of
                Nothing → expectationFailure ("failed to decode: " ⧺ T.unpack r)
                Just p → do
                    mpBlockedAlone p `shouldBe` False
                    mpBaseScopeAlone p `shouldBe` True
                    mpModalFlags p `shouldBe` "01"
                    mpBlockedNested p `shouldBe` True
                    mpBaseScopeNested p `shouldBe` False
                    mpDeepScopeNested p `shouldBe` True
                    mpBlockedAfterPop p `shouldBe` False
                    mpBaseScopeAfterPop p `shouldBe` True

    describe "remembered building-side levels" $ do

        it "a building-stored container's level renders the REMEMBERED \
           \snapshot and carries the PARENT's age, without one live \
           \storage read or one reveal" $ \(env, ls) → do
            resetFixture env ls
            setupScene ls
            r ← evalJSON ls $ luaLines
                [ "local cip = require('scripts.cargo_inventory_panel');"
                , "local il = require('scripts.ui.item_list');"
                , "cip.openFor('building', 42, 300, 300);"
                , "_G.__fire(1, 2, 'Contents');"
                , "_G.__fire(2, 1, 'Contents');"
                , "local d = cip.dump();"
                , "cip.update(0.1); cip.update(0.1);"
                , "local names = {};"
                , "for _, row in ipairs(il.getRows(cip.getLevel(3).listId)) do"
                , "  names[#names+1] = row.item.defName .. 'x' .. tostring(row.item.count) end;"
                , "return {parentAge = tostring(d.levels[1].ageText),"
                , "        childAge = tostring(d.levels[2].ageText),"
                , "        grandAge = tostring(d.levels[3].ageText),"
                , "        rows = table.concat(names, ','),"
                , "        reveals = _G.__reveals, liveStorage = _G.__liveStorage}"
                ]
            case decode (BL.fromStrict (TE.encodeUtf8 r)) ∷ Maybe AgeProbe of
                Nothing → expectationFailure ("failed to decode: " ⧺ T.unpack r)
                Just p → do
                    -- gameTime 160 - revealedAt 100 = 60 s.
                    apParentAge p `shouldBe` "as of 1m ago"
                    apChildAge p `shouldBe` apParentAge p
                    apGrandAge p `shouldBe` apParentAge p
                    apRows p `shouldBe` "pinx2"
                    apReveals p `shouldBe` 0
                    apLiveStorage p `shouldBe` 0

        it "a level whose remembered path stops resolving closes, and \
           \takes every level below it with it — never retargeting a \
           \same-def sibling" $ \(env, ls) → do
            resetFixture env ls
            setupScene ls
            r ← evalJSON ls $ luaLines
                [ "local cip = require('scripts.cargo_inventory_panel');"
                , "cip.openFor('building', 42, 300, 300);"
                , "_G.__fire(1, 2, 'Contents');"
                , "_G.__fire(2, 1, 'Contents');"
                , "local before = cip.depth();"
                -- The toolbox is no longer in the record: the level that
                -- rendered it, and the one below it, must both go.
                , "local orig = building.getRememberedItemContents;"
                , "building.getRememberedItemContents = function(bid, path)"
                , "  if path and path[1] == 77 then return nil end;"
                , "  return orig(bid, path) end;"
                , "cip.update(0.1);"
                , dumpStackLua
                , "building.getRememberedItemContents = orig;"
                , "return {depth = d.depth, kinds = table.concat(kinds, '|'),"
                , "        titles = table.concat(titles, '|') .. '/' .. before,"
                , "        paths = table.concat(paths, '|')}"
                ]
            case decode (BL.fromStrict (TE.encodeUtf8 r)) ∷ Maybe StackProbe of
                Nothing → expectationFailure ("failed to decode: " ⧺ T.unpack r)
                Just p → do
                    spDepth p `shouldBe` 1
                    spTitles p `shouldBe` "Cargo Hold/3"

    describe "row actions" $ do

        -- #1249 retired the Withdraw entry this case was written around
        -- and put "Retrieve" in its place, which is OMITTED when no
        -- eligible source resolves (this scene selects nothing) rather
        -- than shown disabled. So an endpoint row here offers exactly
        -- what the item levels do: "Contents" when it is a container,
        -- and no menu at all when it is not. What the case still pins is
        -- unchanged and is the point — an item-container level never
        -- gains a transfer operation (D-5), and neither endpoint kind
        -- gains one it could not run.
        it "a container row on any level offers Contents; no level ever \
           \gains a transfer operation it did not have, and a level with \
           \nothing to offer shows no menu at all" $ \(env, ls) → do
            resetFixture env ls
            setupScene ls
            r ← evalJSON ls $ luaLines
                [ "local cip = require('scripts.cargo_inventory_panel');"
                , "unit.getSelected = function() return {} end;"
                , "cip.openFor('building', 42, 300, 300);"
                , "local buildingRow = _G.__labels(1, 2);"
                , "local buildingPlain = _G.__labels(1, 1);"
                , "_G.__fire(1, 2, 'Contents');"
                , "local itemRow = _G.__labels(2, 1);"
                , "cip.openFor('unit', 5, 300, 300);"
                , "local unitRow = _G.__labels(1, 1);"
                , "_G.__fire(1, 1, 'Contents');"
                , "local itemPlain = _G.__labels(2, 2);"
                , "return {buildingRow = buildingRow,"
                , "        buildingPlain = buildingPlain,"
                , "        unitRow = unitRow, itemRow = itemRow,"
                , "        itemPlain = itemPlain}"
                ]
            case decode (BL.fromStrict (TE.encodeUtf8 r)) ∷ Maybe MenuProbe of
                Nothing → expectationFailure ("failed to decode: " ⧺ T.unpack r)
                Just p → do
                    mnBuildingRow p `shouldBe` "Contents"
                    -- Where the retired disabled "Withdraw (select an
                    -- adjacent unit first)" row used to be: an
                    -- unrunnable gesture is now absent, so a plain row
                    -- with no eligible retriever shows no menu.
                    mnBuildingPlain p `shouldBe` ""
                    mnUnitRow p `shouldBe` "Contents"
                    mnItemRow p `shouldBe` "Contents"
                    -- A plain row on a render-only level offers nothing
                    -- at all, so no menu is shown.
                    mnItemPlain p `shouldBe` ""

    describe "dismissal" $ do

        it "Escape closes exactly one level per press, deepest first, \
           \restoring the newly deepest level's interactivity each time \
           \— and stops consuming the key once the stack is empty" $ \(env, ls) → do
            resetFixture env ls
            setupScene ls
            r ← evalJSON ls $ luaLines
                [ "local cip = require('scripts.cargo_inventory_panel');"
                , "cip.openFor('building', 42, 300, 300);"
                , "_G.__fire(1, 2, 'Contents');"
                , "_G.__fire(2, 1, 'Contents');"
                , "local depths, consumed, scopes = {cip.depth()}, {}, {};"
                , "for i = 1, 4 do"
                , "  consumed[i] = cip.handleKeyDown('Escape') and '1' or '0';"
                , "  depths[i+1] = cip.depth();"
                , "  local d = cip.dump();"
                , "  local deepest = d.levels[d.depth];"
                , "  scopes[i] = deepest and (deepest.pageInScope and '1' or '0') or '-';"
                , "end;"
                , "return {depths = table.concat(depths, ','),"
                , "        consumed = table.concat(consumed, ''),"
                , "        scopes = table.concat(scopes, '')}"
                ]
            case decode (BL.fromStrict (TE.encodeUtf8 r)) ∷ Maybe EscapeProbe of
                Nothing → expectationFailure ("failed to decode: " ⧺ T.unpack r)
                Just p → do
                    epDepths p `shouldBe` "3,2,1,0,0"
                    epConsumed p `shouldBe` "1110"
                    -- After each press the newly deepest level is in
                    -- scope again; the fourth press has nothing left.
                    epScopes p `shouldBe` "11--"

    describe "lifecycle" $ do

        it "a resize preserves the whole stack — every level's nesting \
           \path, its selected tab and its own scroll offset" $ \(env, ls) → do
            resetFixture env ls
            setupScene ls
            r ← evalJSON ls $ luaLines
                [ "local cip = require('scripts.cargo_inventory_panel');"
                , "local il = require('scripts.ui.item_list');"
                , "local tb = require('scripts.ui.tabbar');"
                -- A LONG endpoint list (25 rows, cap 10) and a long
                -- remembered nested list (20 rows, cap 12) so both levels
                -- have somewhere to scroll to.
                , "building.getContainerKnowledge = function()"
                , "  local items = { { defName='toolbox', displayName='Toolbox',"
                , "      category='Tools', kind='container', instanceId=77,"
                , "      weight=3.0 } };"
                , "  for i = 1, 24 do items[#items+1] = { defName='r'..i,"
                , "      displayName='R'..i, category='Food', kind='food',"
                , "      instanceId=1000+i, weight=0.5 } end;"
                , "  return { state='known', storedWeight=12.0, capacity=400.0,"
                , "           revealedAt=100.0, items=items } end;"
                , "building.getRememberedItemContents = function(_, path)"
                , "  if not path or path[1] ~= 77 then return nil end;"
                , "  local items = {};"
                , "  for i = 1, 20 do items[i] = { defName='n'..i,"
                , "      displayName='N'..i, kind='misc', instanceId=2000+i,"
                , "      count=1, weight=0.1 } end;"
                , "  return { revealedAt=100.0, items=items } end;"
                , "local hud = require('scripts.hud');"
                , "hud.init(1,2,1920,1080); hud.createUI();"
                , "cip.openFor('building', 42, 200, 200);"
                , "for _, t in ipairs(il.getTabs(cip.getLevel(1).listId)) do"
                , "  if t.key == 'Food' then tb.handleCallback('onTabClick', t.boxId) end"
                , "end;"
                -- Level 1 is on the Food tab now, so the toolbox row is
                -- filtered out; go back to All to descend.
                , "for _, t in ipairs(il.getTabs(cip.getLevel(1).listId)) do"
                , "  if t.key == 'All' then tb.handleCallback('onTabClick', t.boxId) end"
                , "end;"
                , "_G.__fire(1, 1, 'Contents');"
                , "cip.getLevel(1).scroll ="
                , "  il.setScrollOffset(cip.getLevel(1).listId, 6);"
                , "cip.getLevel(2).scroll ="
                , "  il.setScrollOffset(cip.getLevel(2).listId, 4);"
                , "local function snap()"
                , "  local d = cip.dump(); local paths, scrolls, tabs = {}, {}, {};"
                , "  for i, l in ipairs(d.levels) do"
                , "    paths[i] = table.concat(l.path or {}, '.');"
                , "    scrolls[i] = tostring(l.scroll);"
                , "    tabs[i] = tostring(l.activeTab) end;"
                , "  return d.depth, table.concat(paths, '|'),"
                , "         table.concat(scrolls, ','), table.concat(tabs, '|') end;"
                , "local db, pb, sb, tbf = snap();"
                , "hud.onFramebufferResize(1600, 900);"
                , "local da, pa, sa, ta = snap();"
                , "return {depthBefore = db, depthAfter = da,"
                , "        pathsBefore = pb, pathsAfter = pa,"
                , "        scrollBefore = sb, scrollAfter = sa,"
                , "        tabBefore = tbf, tabAfter = ta}"
                ]
            case decode (BL.fromStrict (TE.encodeUtf8 r)) ∷ Maybe ResizeProbe of
                Nothing → expectationFailure ("failed to decode: " ⧺ T.unpack r)
                Just p → do
                    rpDepthBefore p `shouldBe` 2
                    rpDepthAfter p `shouldBe` 2
                    rpPathsBefore p `shouldBe` "|77"
                    rpPathsAfter p `shouldBe` rpPathsBefore p
                    rpScrollBefore p `shouldBe` "6,4"
                    rpScrollAfter p `shouldBe` rpScrollBefore p
                    rpTabAfter p `shouldBe` rpTabBefore p

        it "the save-load reset drops the whole stack — every level, its \
           \modal pages, its list instances and the modal boundary" $ \(env, ls) → do
            resetFixture env ls
            setupScene ls
            r ← evalJSON ls $ luaLines
                [ "local cip = require('scripts.cargo_inventory_panel');"
                , "local il = require('scripts.ui.item_list');"
                , "cip.openFor('building', 42, 300, 300);"
                , "_G.__fire(1, 2, 'Contents');"
                , "_G.__fire(2, 1, 'Contents');"
                , "local depthBefore, listsBefore = cip.depth(), il.count();"
                , "local pages = {};"
                , "for i = 1, cip.depth() do"
                , "  pages[#pages+1] = cip.getLevel(i).pageId end;"
                -- The REAL broadcast every load trigger reaches, not a
                -- direct closeIfOpen(): uiManager.onSaveLoaded is where
                -- the reset is wired, so a future edit that drops it
                -- fails here.
                , "world.getActiveWorldId = function() return nil end;"
                , "local uiManager = require('scripts.ui_manager');"
                , "uiManager.onSaveLoaded({}, {});"
                , "local pagesGone = true;"
                , "for _, pid in ipairs(pages) do"
                , "  if UI.isPageVisible(pid) then pagesGone = false end end;"
                , "return {depthBefore = depthBefore, depthAfter = cip.depth(),"
                , "        listsBefore = listsBefore, listsAfter = il.count(),"
                , "        pagesGone = pagesGone,"
                , "        blockedAfter = UI.isInputBlocked()}"
                ]
            case decode (BL.fromStrict (TE.encodeUtf8 r)) ∷ Maybe ResetProbe of
                Nothing → expectationFailure ("failed to decode: " ⧺ T.unpack r)
                Just p → do
                    rsDepthBefore p `shouldBe` 3
                    rsDepthAfter p `shouldBe` 0
                    rsListsBefore p `shouldBe` 3
                    rsListsAfter p `shouldBe` 0
                    rsPagesGone p `shouldBe` True
                    rsBlockedAfter p `shouldBe` False
