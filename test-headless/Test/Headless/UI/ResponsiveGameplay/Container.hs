-- | Container owner of the #750 responsive-gameplay gate (#2126):
--   cargo / item-contents responsive layout, the endpoint-kind
--   agnostic container window (#1234) and last-known container
--   knowledge (#1237). Future portable-container UI coverage from
--   epic #1231 belongs here too.
--
--   A 'SpecWith' fragment over the facade's single shared fixture —
--   see 'Test.Headless.UI.ResponsiveGameplay.Fixture'. This module
--   never boots an engine or a Lua VM of its own.
module Test.Headless.UI.ResponsiveGameplay.Container (spec) where

import UPrelude
import Test.Hspec
import Data.Aeson (FromJSON(..), decode, withObject, (.:), (.:?), (.!=))
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.ByteString.Lazy as BL
import Test.Headless.UI.ResponsiveGameplay.Fixture

spec ∷ SpecWith SharedFixture
spec = do
    describe "cargo_inventory_panel.lua / item_contents_panel.lua stay in-frame at a narrow, high-scale, still-C2-supported combination (round-7 review)" $ do
        it "cargo_inventory_panel: the panel width is capped instead of only repositioning an oversized panel" $ \(env, ls) → do
            resetFixture env ls
            r ← evalJSON ls $ luaLines
                -- Since #1237 a building endpoint's whole data source is
                -- building.getContainerKnowledge, so that is what these
                -- pre-existing #750 geometry cases stub.
                [ "engine.setUIScale(4.0);"
                , "local origK = building.getContainerKnowledge;"
                , "building.getContainerKnowledge = function() return"
                , "  { state='empty', items={}, storedWeight=0.0,"
                , "    capacity=100.0, revealedAt=0.0 } end;"
                , "local pg = UI.newPage('cargo_test_page', 'overlay');"
                , "local cip = require('scripts.cargo_inventory_panel');"
                -- #1238: the container window owns a STACK of levels;
                -- `L(i)` is the nil-safe projection of one (default the
                -- base), so a read after a close answers absent rather
                -- than raising.
                , "local L = function(i) return cip.getLevel(i or 1) or {src={}} end;"
                , "cip.setup({page = pg, fbW = 800, fbH = 2160, boxTexSet = 1});"
                , "cip.openFor('building', 1, 400, 400);"
                , "local p = require('scripts.ui.panel');"
                , "local x, y = p.getPosition(L().panelId);"
                , "local pw, ph = p.getSize(L().panelId);"
                , "building.getContainerKnowledge = origK;"
                , "return {w=pw,"
                , "        inFrame=(x>=0 and y>=0 and (x+pw)<=800 and (y+ph)<=2160)}"
                ]
            case decode (BL.fromStrict (TE.encodeUtf8 r)) ∷ Maybe WidthCapProbe of
                Nothing → expectationFailure ("failed to decode: " ⧺ T.unpack r)
                Just p → wcpInFrame p `shouldBe` True

        it "cargo_inventory_panel: a multi-category inventory's tab strip shrinks to fit instead of running off-frame (round-8 review)" $ \(env, ls) → do
            resetFixture env ls
            r ← evalJSON ls $ luaLines
                [ "engine.setUIScale(4.0);"
                , "local origK = building.getContainerKnowledge;"
                , "building.getContainerKnowledge = function() return"
                , "  { state='known', storedWeight=0.0, capacity=100.0,"
                , "    revealedAt=0.0, items = {"
                , "    { defName='i1', category='Cat1' }, { defName='i2', category='Cat2' },"
                , "    { defName='i3', category='Cat3' }, { defName='i4', category='Cat4' },"
                , "    { defName='i5', category='Cat5' }, { defName='i6', category='Cat6' },"
                , "    { defName='i7', category='Cat7' }, { defName='i8', category='Cat8' },"
                , "} } end;"
                , "local pg = UI.newPage('cargo_tab_test_page', 'overlay');"
                , "local cip = require('scripts.cargo_inventory_panel');"
                -- #1238: the container window owns a STACK of levels;
                -- `L(i)` is the nil-safe projection of one (default the
                -- base), so a read after a close answers absent rather
                -- than raising.
                , "local L = function(i) return cip.getLevel(i or 1) or {src={}} end;"
                , "cip.setup({page = pg, fbW = 800, fbH = 2160, boxTexSet = 1});"
                , "cip.openFor('building', 1, 400, 400);"
                , "local out = {};"
                , "local il = require('scripts.ui.item_list');"
                , "for _, t in ipairs(il.getTabs(L().listId)) do"
                , "    local info = UI.getElementInfo(t.boxId);"
                , "    table.insert(out, {x=info.x, y=info.y, w=info.width, h=info.height})"
                , "end;"
                , "building.getContainerKnowledge = origK;"
                , "return out"
                ]
            case decode (BL.fromStrict (TE.encodeUtf8 r)) ∷ Maybe [RectRow] of
                Nothing → expectationFailure ("failed to decode: " ⧺ T.unpack r)
                Just rects → do
                    length rects `shouldSatisfy` (≥ 8)
                    forM_ rects $ \rc → do
                        rrX rc `shouldSatisfy` (≥ 0)
                        rrW rc `shouldSatisfy` (≥ 20)
                        (rrX rc + rrW rc) `shouldSatisfy` (≤ 800)

        it "cargo_inventory_panel: a heavily-shrunk tab's label renders smaller than an unshrunk one, not at a fixed full-uiscale size (round-12 review)" $ \(env, ls) → do
            resetFixture env ls
            -- #750 round-12 review: shrinking the tab BOX alone left the
            -- label rendering at the full uiscale, unclipped and
            -- page-rooted — wider than its own compressed box, bleeding
            -- into neighbours. engine.getTextWidth (and so label WIDTH)
            -- always measures 0 in this suite's synthetic boot (see the
            -- module docstring), so this can't assert on rendered pixel
            -- overlap directly — but label.lua's own HEIGHT is derived
            -- straight from `fontSize * uiscale` (scripts/ui/label.lua's
            -- label.new), independent of any real text metrics. A fixed,
            -- unshrunk uiscale would report the SAME height regardless of
            -- category count; the fix (labelUiscale = uiscale * shrink)
            -- makes a heavily-shrunk tab's label measurably smaller than
            -- one that needed no shrink at all.
            r ← evalJSON ls $ luaLines
                [ "engine.setUIScale(4.0);"
                , "local origK = building.getContainerKnowledge;"
                , "local label = require('scripts.ui.label');"
                , "local cip = require('scripts.cargo_inventory_panel');"
                -- #1238: the container window owns a STACK of levels;
                -- `L(i)` is the nil-safe projection of one (default the
                -- base), so a read after a close answers absent rather
                -- than raising.
                , "local L = function(i) return cip.getLevel(i or 1) or {src={}} end;"
                , "local function known(items) return function() return"
                , "  { state='known', items=items, storedWeight=0.0,"
                , "    capacity=100.0, revealedAt=0.0 } end end;"
                , "building.getContainerKnowledge = known({"
                , "    { defName='i1', category='Cat1' } });"
                , "local pg1 = UI.newPage('cargo_lbl_test_1', 'overlay');"
                , "cip.setup({page = pg1, fbW = 800, fbH = 2160, boxTexSet = 1});"
                , "cip.openFor('building', 1, 400, 400);"
                , "local il = require('scripts.ui.item_list');"
                , "local _, unshunkH = label.getSize(il.getTabs(L().listId)[1].labelId);"
                , "building.getContainerKnowledge = known({"
                , "    { defName='i1', category='Cat1' }, { defName='i2', category='Cat2' },"
                , "    { defName='i3', category='Cat3' }, { defName='i4', category='Cat4' },"
                , "    { defName='i5', category='Cat5' }, { defName='i6', category='Cat6' },"
                , "    { defName='i7', category='Cat7' }, { defName='i8', category='Cat8' },"
                , "});"
                , "local pg2 = UI.newPage('cargo_lbl_test_2', 'overlay');"
                , "cip.setup({page = pg2, fbW = 800, fbH = 2160, boxTexSet = 1});"
                , "cip.openFor('building', 1, 400, 400);"
                , "local _, shrunkH = label.getSize(il.getTabs(L().listId)[2].labelId);"
                , "building.getContainerKnowledge = origK;"
                , "return {unshrunkH = unshunkH, shrunkH = shrunkH}"
                ]
            case decode (BL.fromStrict (TE.encodeUtf8 r)) ∷ Maybe ShrinkHeightProbe of
                Nothing → expectationFailure ("failed to decode: " ⧺ T.unpack r)
                Just p → do
                    shpShrunkH p `shouldSatisfy` (> 0)
                    shpShrunkH p `shouldSatisfy` (< shpUnshrunkH p)

        it "cargo_inventory_panel: a resize preserves the open panel's target building AND its selected tab instead of silently closing it (round-13 review)" $ \(env, ls) → do
            resetFixture env ls
            -- #750 round-13 review: hud.lua's "resize" teardown used to
            -- be a one-way close() — a resize while a cargo panel was
            -- open discarded it (and which tab was selected) entirely,
            -- rather than surviving the layout-only change like #750
            -- requires. Drives the REAL hud.world_page (not a standalone
            -- test page) so hud.createUI()'s snapshot/reopen machinery
            -- actually engages.
            r ← evalJSON ls $ luaLines
                [ "local origK = building.getContainerKnowledge;"
                , "building.getContainerKnowledge = function() return"
                , "  { state='known', storedWeight=0.0, capacity=100.0,"
                , "    revealedAt=0.0, items = {"
                , "    { defName='i1', category='Cat1' }, { defName='i2', category='Cat2' },"
                , "} } end;"
                , "local hud = require('scripts.hud');"
                , "hud.init(1,2,1920,1080);"
                , "hud.createUI();"
                , "local cip = require('scripts.cargo_inventory_panel');"
                -- #1238: the container window owns a STACK of levels;
                -- `L(i)` is the nil-safe projection of one (default the
                -- base), so a read after a close answers absent rather
                -- than raising.
                , "local L = function(i) return cip.getLevel(i or 1) or {src={}} end;"
                , "cip.openFor('building', 42, 400, 400);"
                , "local il = require('scripts.ui.item_list');"
                , "local targetBox = nil;"
                , "for _, t in ipairs(il.getTabs(L().listId)) do"
                , "    if t.key == 'Cat2' then targetBox = t.boxId end"
                , "end;"
                , "require('scripts.ui.tabbar').handleCallback('onTabClick', targetBox);"
                , "local wasOpenBefore = cip.isOpen();"
                , "local tabBefore = L().activeTab;"
                , "hud.onFramebufferResize(1600, 900);"
                , "building.getContainerKnowledge = origK;"
                , "return {wasOpenBefore = wasOpenBefore, tabBefore = tabBefore,"
                , "        isOpenAfter = cip.isOpen(), kindAfter = L().src.endpointKind,"
                , "        idAfter = L().src.id,"
                , "        tabAfter = L().activeTab}"
                ]
            case decode (BL.fromStrict (TE.encodeUtf8 r)) ∷ Maybe CargoResizeProbe of
                Nothing → expectationFailure ("failed to decode: " ⧺ T.unpack r)
                Just p → do
                    crpWasOpenBefore p `shouldBe` True
                    crpTabBefore p `shouldBe` "Cat2"
                    crpIsOpenAfter p `shouldBe` True
                    -- #1234: the resize snapshot carries the whole
                    -- endpoint IDENTITY through hud's rebuild, not a
                    -- bare building id.
                    crpKindAfter p `shouldBe` "building"
                    crpIdAfter p `shouldBe` 42
                    crpTabAfter p `shouldBe` "Cat2"

        it "cargo_inventory_panel: the migrated tab strip adds no content frame, and its rows keep a working right-click (#1088)" $ \(env, ls) → do
            resetFixture env ls
            -- #1088 migration: cargo's bespoke tab strip is now a
            -- frame-free scripts/ui/tabbar layout. The default framed
            -- layout would add a content frame BEHIND the item rows,
            -- which is exactly the regression requirement 6 forbids.
            -- The row right-click must keep resolving to the exact
            -- rendered row's representative instance, so it is driven
            -- through the real shared dispatcher, not a host function.
            --
            -- #1249: proving a menu APPEARS now needs the host to have
            -- something to offer, so this selects a real retriever and
            -- gives the remembered rows the instance ids a batch gesture
            -- names. Without both, the row would route correctly and
            -- still show nothing, and the routing half of this case
            -- would pass vacuously.
            r ← evalJSON ls $ luaLines
                [ "local origK = building.getContainerKnowledge;"
                , "local origSel = unit.getSelected;"
                , "local origUInfo = unit.getInfo;"
                , "local origFac = unit.getFaction;"
                , "local origCmd = faction.isPlayerCommandable;"
                , "building.getContainerKnowledge = function() return"
                , "  { state='known', storedWeight=3.0, capacity=100.0,"
                , "    revealedAt=0.0, items = {"
                , "    { defName='i1', displayName='Ore', category='Cat1',"
                , "      weight=2.0, instanceId=901 },"
                , "    { defName='i2', displayName='Rope', category='Cat2',"
                , "      weight=1.0, instanceId=902 },"
                , "} } end;"
                , "unit.getSelected = function() return {5} end;"
                , "unit.getInfo = function() return"
                , "    { name='Vela', gridX=10, gridY=10 } end;"
                , "unit.getFaction = function() return 'player' end;"
                , "faction.isPlayerCommandable = function(f)"
                , "    return f == 'player' end;"
                , "local cm = require('scripts.ui.context_menu');"
                , "local origShow = cm.show; _G.__menuShown = false;"
                , "cm.show = function() _G.__menuShown = true end;"
                , "local pg = UI.newPage('cargo_frame_page', 'overlay');"
                , "local cip = require('scripts.cargo_inventory_panel');"
                -- #1238: the container window owns a STACK of levels;
                -- `L(i)` is the nil-safe projection of one (default the
                -- base), so a read after a close answers absent rather
                -- than raising.
                , "local L = function(i) return cip.getLevel(i or 1) or {src={}} end;"
                , "cip.setup({page = pg, fbW = 1920, fbH = 1080, boxTexSet = 1});"
                , "cip.openFor('building', 7, 100, 100);"
                , "local il = require('scripts.ui.item_list');"
                , "local tb = require('scripts.ui.tabbar');"
                , "local rows = il.getRows(L().listId);"
                , "local rowInfo = UI.getElementInfo(rows[1].hitId);"
                , "local routed = il.handleCallback('onItemListRightClick', rows[1].hitId);"
                , "cm.show = origShow;"
                , "building.getContainerKnowledge = origK;"
                , "unit.getSelected = origSel; unit.getInfo = origUInfo;"
                , "unit.getFaction = origFac;"
                , "faction.isPlayerCommandable = origCmd;"
                , "return {hasFrame = tb.hasFrame(il.getTabBarId(L().listId)),"
                , "        tabCount = #il.getTabs(L().listId),"
                , "        rowCount = #rows, interactive = rowInfo.interactive,"
                , "        routed = routed, menuShown = _G.__menuShown}"
                ]
            case decode (BL.fromStrict (TE.encodeUtf8 r)) ∷ Maybe CargoMigrationProbe of
                Nothing → expectationFailure ("failed to decode: " ⧺ T.unpack r)
                Just p → do
                    cmpHasFrame p `shouldBe` False
                    cmpTabCount p `shouldBe` 3      -- All + Cat1 + Cat2
                    cmpRowCount p `shouldBe` 2
                    cmpInteractive p `shouldBe` True
                    cmpRouted p `shouldBe` True
                    cmpMenuShown p `shouldBe` True

    describe "cargo_inventory_panel.lua is an endpoint-kind agnostic container window (#1234)" $ do
        it "a building endpoint opens through the generalized signature with the same rows, tabs, header and row action" $ \(env, ls) → do
            resetFixture env ls
            r ← evalJSON ls $ luaLines
                -- #1237: the LIVE reads are left deliberately answering
                -- something else entirely, so a header or row that came
                -- from them rather than from the remembered snapshot
                -- would be unmistakable.
                [ "local origCap = building.getStorageCapacity;"
                , "local origStorage = building.getStorage;"
                , "local origWeight = building.getStorageWeight;"
                , "local origInfo = building.getInfo;"
                , "local origK = building.getContainerKnowledge;"
                , "building.getStorageCapacity = function() return 999 end;"
                , "building.getStorageWeight = function() return 777.5 end;"
                , "building.getStorage = function() return {"
                , "    { defName='live_only', displayName='Live Only',"
                , "      category='Wrong', weight=9.0 } } end;"
                , "building.getInfo = function() return"
                , "    { displayName='Cargo Hold', gridX=0, gridY=0,"
                , "      tileW=1, tileH=1 } end;"
                , "building.getContainerKnowledge = function() return"
                , "  { state='known', storedWeight=12.5, capacity=400.0,"
                , "    revealedAt=0.0, items = {"
                , "    { defName='steel_bar', displayName='Steel Bar',"
                , "      category='Materials', weight=2.0 },"
                , "    { defName='steel_bar', displayName='Steel Bar',"
                , "      category='Materials', weight=2.0 },"
                , "    { defName='bandage', displayName='Bandage',"
                , "      category='Medical', weight=0.1 },"
                , "} } end;"
                , "local origSel = unit.getSelected;"
                , "unit.getSelected = function() return {} end;"
                , "local cm = require('scripts.ui.context_menu');"
                , "local origShow = cm.show; cm.show = function() end;"
                , "local pg = UI.newPage('cargo_ep_building', 'overlay');"
                , "local cip = require('scripts.cargo_inventory_panel');"
                -- #1238: the container window owns a STACK of levels;
                -- `L(i)` is the nil-safe projection of one (default the
                -- base), so a read after a close answers absent rather
                -- than raising.
                , "local L = function(i) return cip.getLevel(i or 1) or {src={}} end;"
                , "cip.setup({page = pg, fbW = 1920, fbH = 1080, boxTexSet = 1});"
                , "local accepted = cip.openFor('building', 11, 300, 300);"
                , "local il = require('scripts.ui.item_list');"
                , "local lbl = require('scripts.ui.label');"
                , "local rows = il.getRows(L().listId);"
                , "local names = {};"
                , "for i, rw in ipairs(rows) do names[i] = rw.item.defName end;"
                , "local out = {accepted = accepted, open = cip.isOpen(),"
                , "  kind = L().src.endpointKind, id = L().src.id,"
                , "  title = lbl.getText(L().titleId),"
                , "  subtitle = lbl.getText(L().subtitleId),"
                , "  rowCount = #rows, tabCount = #il.getTabs(L().listId),"
                , "  rowNames = names,"
                , "  rightClick = (rows[1] ~= nil and rows[1].hitId ~= nil"
                , "      and il.handleCallback('onItemListRightClick', rows[1].hitId)"
                , "      or false)};"
                , "cip.closeIfOpen();"
                , "cm.show = origShow; unit.getSelected = origSel;"
                , "building.getStorageCapacity = origCap;"
                , "building.getStorage = origStorage;"
                , "building.getStorageWeight = origWeight;"
                , "building.getInfo = origInfo;"
                , "building.getContainerKnowledge = origK;"
                , "return out"
                ]
            case decode (BL.fromStrict (TE.encodeUtf8 r)) ∷ Maybe EndpointOpenProbe of
                Nothing → expectationFailure ("failed to decode: " ⧺ T.unpack r)
                Just p → do
                    cwoAccepted p `shouldBe` True
                    cwoOpen p `shouldBe` True
                    cwoKind p `shouldBe` Just "building"
                    cwoId p `shouldBe` Just 11
                    cwoTitle p `shouldBe` "Cargo Hold"
                    -- Byte-identical to the pre-#1234 header, and the
                    -- REMEMBERED numbers — not the live 777.5 / 999
                    -- those same stubs answer with (#1237).
                    cwoSubtitle p `shouldBe` "Storage: 12.50 / 400.00 kg"
                    cwoRowCount p `shouldBe` 2      -- steel_bar x2 stacks
                    cwoTabCount p `shouldBe` 3      -- All + Materials + Medical
                    cwoRowNames p `shouldContain` ["steel_bar"]
                    -- #1249: this scene selects NOTHING, and the row
                    -- action that replaced Withdraw ("Retrieve") is
                    -- omitted rather than disabled when no eligible
                    -- retriever resolves — where the retired path always
                    -- produced a menu, showing a greyed "select an
                    -- adjacent unit first" row. A plain (non-container)
                    -- row with no gesture therefore opens no menu at all.
                    cwoRightClick p `shouldBe` False

        it "a unit endpoint opens through the SAME manager, reading capacity and stored weight from transferEndpointInfo and its rows from that call's loose inventory" $ \(env, ls) → do
            resetFixture env ls
            -- The unit endpoint's whole data source is ONE engine read
            -- (#1234 / the issue's review correction): `contents` is
            -- loose inventory, while `storedWeight` deliberately
            -- includes equipment and accessories too — so the header
            -- here must NOT equal the rows' summed weight.
            r ← evalJSON ls $ luaLines
                [ "local origEp = unit.transferEndpointInfo;"
                , "local origInfo = unit.getInfo;"
                , "unit.getInfo = function() return { name = 'Sister Vela' } end;"
                , "unit.transferEndpointInfo = function(ep)"
                , "  if ep.kind ~= 'unit' or ep.id ~= 5 then return nil end;"
                , "  return { eligible = true, displayName = 'Acolyte',"
                , "           capacity = 40.0, storedWeight = 31.25,"
                , "           contents = {"
                , "    { defName='wood_log', displayName='Wood Log',"
                , "      category='Materials', weight=5.0 },"
                , "    { defName='wood_log', displayName='Wood Log',"
                , "      category='Materials', weight=5.0 },"
                , "    { defName='ration', displayName='Ration',"
                , "      category='Food', weight=0.5 } } } end;"
                , "local pg = UI.newPage('cargo_ep_unit', 'overlay');"
                , "local cip = require('scripts.cargo_inventory_panel');"
                -- #1238: the container window owns a STACK of levels;
                -- `L(i)` is the nil-safe projection of one (default the
                -- base), so a read after a close answers absent rather
                -- than raising.
                , "local L = function(i) return cip.getLevel(i or 1) or {src={}} end;"
                , "cip.setup({page = pg, fbW = 1920, fbH = 1080, boxTexSet = 1});"
                , "local accepted = cip.openFor('unit', 5, 300, 300);"
                , "local il = require('scripts.ui.item_list');"
                , "local lbl = require('scripts.ui.label');"
                , "local rows = il.getRows(L().listId);"
                , "local names = {};"
                , "for i, rw in ipairs(rows) do names[i] = rw.item.defName end;"
                , "local out = {accepted = accepted, open = cip.isOpen(),"
                , "  kind = L().src.endpointKind, id = L().src.id,"
                , "  title = lbl.getText(L().titleId),"
                , "  subtitle = lbl.getText(L().subtitleId),"
                , "  rowCount = #rows, tabCount = #il.getTabs(L().listId),"
                , "  rowNames = names,"
                , "  rightClick = (rows[1] ~= nil and rows[1].hitId ~= nil"
                , "      and il.handleCallback('onItemListRightClick', rows[1].hitId)"
                , "      or false)};"
                , "cip.closeIfOpen();"
                , "unit.transferEndpointInfo = origEp; unit.getInfo = origInfo;"
                , "return out"
                ]
            case decode (BL.fromStrict (TE.encodeUtf8 r)) ∷ Maybe EndpointOpenProbe of
                Nothing → expectationFailure ("failed to decode: " ⧺ T.unpack r)
                Just p → do
                    cwoAccepted p `shouldBe` True
                    cwoOpen p `shouldBe` True
                    cwoKind p `shouldBe` Just "unit"
                    cwoId p `shouldBe` Just 5
                    cwoTitle p `shouldBe` "Sister Vela"
                    -- 31.25 is the recursive load, NOT the 10.5 kg of
                    -- loose inventory the rows below add up to.
                    cwoSubtitle p `shouldBe` "Carrying: 31.25 / 40.00 kg"
                    cwoRowCount p `shouldBe` 2      -- wood_log x2 stacks
                    cwoTabCount p `shouldBe` 3      -- All + Materials + Food
                    cwoRowNames p `shouldContain` ["ration"]
                    -- A plain row with nothing selected offers nothing:
                    -- not a container (so no "Contents"), and #1249's
                    -- "Retrieve" resolves no eligible retriever here.
                    cwoRightClick p `shouldBe` False

        it "an unknown endpoint kind is refused, creating no panel state and leaving an already-open window alone" $ \(env, ls) → do
            resetFixture env ls
            r ← evalJSON ls $ luaLines
                [ "local origK = building.getContainerKnowledge;"
                , "building.getContainerKnowledge = function() return"
                , "  { state='known', storedWeight=1.0, capacity=100.0,"
                , "    revealedAt=0.0, items = {"
                , "    { defName='i1', category='Cat1' } } } end;"
                , "local pg = UI.newPage('cargo_ep_unknown', 'overlay');"
                , "local cip = require('scripts.cargo_inventory_panel');"
                -- #1238: the container window owns a STACK of levels;
                -- `L(i)` is the nil-safe projection of one (default the
                -- base), so a read after a close answers absent rather
                -- than raising.
                , "local L = function(i) return cip.getLevel(i or 1) or {src={}} end;"
                , "cip.setup({page = pg, fbW = 1920, fbH = 1080, boxTexSet = 1});"
                , "cip.openFor('building', 3, 200, 200);"
                , "local accepted = cip.openFor('item_container', 9, 300, 300);"
                , "local out = {accepted = accepted, open = cip.isOpen(),"
                , "  survivorKind = L().src.endpointKind, survivorId = L().src.id,"
                , "  panelId = L().panelId, listId = L().listId};"
                , "cip.closeIfOpen();"
                , "building.getContainerKnowledge = origK;"
                , "return out"
                ]
            case decode (BL.fromStrict (TE.encodeUtf8 r)) ∷ Maybe EndpointRejectProbe of
                Nothing → expectationFailure ("failed to decode: " ⧺ T.unpack r)
                Just p → do
                    erpAccepted p `shouldBe` False
                    -- The valid window it could have replaced survives
                    -- intact — validation happens BEFORE the teardown.
                    erpOpen p `shouldBe` True
                    erpSurvivorKind p `shouldBe` Just "building"
                    erpSurvivorId p `shouldBe` Just 3

        it "a unit that is not player-commandable is not an eligible endpoint, and creates no panel or list state" $ \(env, ls) → do
            resetFixture env ls
            r ← evalJSON ls $ luaLines
                [ "local origEp = unit.transferEndpointInfo;"
                -- Exactly what the engine reports for a live wildlife
                -- unit: a real view, eligible = false.
                , "unit.transferEndpointInfo = function() return"
                , "  { eligible = false, displayName = 'Red Squirrel',"
                , "    capacity = 0.5, storedWeight = 0.0, contents = {} } end;"
                , "local pg = UI.newPage('cargo_ep_wildlife', 'overlay');"
                , "local cip = require('scripts.cargo_inventory_panel');"
                -- #1238: the container window owns a STACK of levels;
                -- `L(i)` is the nil-safe projection of one (default the
                -- base), so a read after a close answers absent rather
                -- than raising.
                , "local L = function(i) return cip.getLevel(i or 1) or {src={}} end;"
                , "cip.setup({page = pg, fbW = 1920, fbH = 1080, boxTexSet = 1});"
                , "local accepted = cip.openFor('unit', 77, 300, 300);"
                , "local out = {accepted = accepted, open = cip.isOpen(),"
                , "  survivorKind = L().src.endpointKind, survivorId = L().src.id,"
                , "  panelId = L().panelId, listId = L().listId};"
                , "unit.transferEndpointInfo = origEp;"
                , "return out"
                ]
            case decode (BL.fromStrict (TE.encodeUtf8 r)) ∷ Maybe EndpointRejectProbe of
                Nothing → expectationFailure ("failed to decode: " ⧺ T.unpack r)
                Just p → do
                    erpAccepted p `shouldBe` False
                    erpOpen p `shouldBe` False
                    erpPanelId p `shouldBe` Nothing
                    erpListId p `shouldBe` Nothing
                    erpSurvivorKind p `shouldBe` Nothing
                    erpSurvivorId p `shouldBe` Nothing

        it "a unit endpoint distinguishes a genuine fresh open (resets the tab) from a resize (preserves kind, id and tab)" $ \(env, ls) → do
            resetFixture env ls
            r ← evalJSON ls $ luaLines
                [ "local origEp = unit.transferEndpointInfo;"
                , "unit.transferEndpointInfo = function() return"
                , "  { eligible = true, displayName = 'Acolyte',"
                , "    capacity = 40.0, storedWeight = 4.0, contents = {"
                , "    { defName='i1', category='Cat1', weight=1.0 },"
                , "    { defName='i2', category='Cat2', weight=1.0 } } } end;"
                , "local pg = UI.newPage('cargo_ep_tabs', 'overlay');"
                , "local cip = require('scripts.cargo_inventory_panel');"
                -- #1238: the container window owns a STACK of levels;
                -- `L(i)` is the nil-safe projection of one (default the
                -- base), so a read after a close answers absent rather
                -- than raising.
                , "local L = function(i) return cip.getLevel(i or 1) or {src={}} end;"
                , "cip.setup({page = pg, fbW = 1920, fbH = 1080, boxTexSet = 1});"
                , "cip.openFor('unit', 5, 300, 300);"
                , "local il = require('scripts.ui.item_list');"
                , "local targetBox = nil;"
                , "for _, t in ipairs(il.getTabs(L().listId)) do"
                , "    if t.key == 'Cat2' then targetBox = t.boxId end"
                , "end;"
                , "require('scripts.ui.tabbar').handleCallback('onTabClick', targetBox);"
                , "local afterClick = L().activeTab;"
                -- The resize path: same endpoint, same tab.
                , "cip.reopenWithTab('unit', 5, 300, 300, afterClick);"
                , "local afterReopen = L().activeTab;"
                , "local kindAfter, idAfter = L().src.endpointKind, L().src.id;"
                -- A genuine fresh open starts back at All.
                , "cip.openFor('unit', 5, 300, 300);"
                , "local afterFresh = L().activeTab;"
                , "cip.closeIfOpen();"
                , "unit.transferEndpointInfo = origEp;"
                , "return {tabAfterClick = afterClick, tabAfterReopen = afterReopen,"
                , "        tabAfterFresh = afterFresh, kindAfterReopen = kindAfter,"
                , "        idAfterReopen = idAfter}"
                ]
            case decode (BL.fromStrict (TE.encodeUtf8 r)) ∷ Maybe EndpointTabProbe of
                Nothing → expectationFailure ("failed to decode: " ⧺ T.unpack r)
                Just p → do
                    etpTabAfterClick p `shouldBe` "Cat2"
                    etpTabAfterReopen p `shouldBe` "Cat2"
                    etpKindAfterReopen p `shouldBe` "unit"
                    etpIdAfterReopen p `shouldBe` 5
                    etpTabAfterFresh p `shouldBe` "All"

        it "a REFUSED reopen leaves the surviving window's endpoint AND tab untouched, rather than re-tabbing an endpoint the caller never named" $ \(env, ls) → do
            resetFixture env ls
            -- Round-1 review: openFor deliberately leaves an
            -- already-open valid window alone when it refuses, so
            -- `state` afterwards describes THAT window. reopenWithTab
            -- ignoring the result therefore applied ITS requested tab to
            -- an unrelated endpoint — a rebuild of a window the caller
            -- never asked about.
            r ← evalJSON ls $ luaLines
                [ "local origK = building.getContainerKnowledge;"
                , "building.getContainerKnowledge = function() return"
                , "  { state='known', storedWeight=2.0, capacity=100.0,"
                , "    revealedAt=0.0, items = {"
                , "    { defName='i1', category='Cat1', weight=1.0 },"
                , "    { defName='i2', category='Cat2', weight=1.0 },"
                , "} } end;"
                , "local pg = UI.newPage('cargo_reject_reopen', 'overlay');"
                , "local cip = require('scripts.cargo_inventory_panel');"
                -- #1238: the container window owns a STACK of levels;
                -- `L(i)` is the nil-safe projection of one (default the
                -- base), so a read after a close answers absent rather
                -- than raising.
                , "local L = function(i) return cip.getLevel(i or 1) or {src={}} end;"
                , "cip.setup({page = pg, fbW = 1920, fbH = 1080, boxTexSet = 1});"
                , "cip.openFor('building', 3, 200, 200);"
                , "local il = require('scripts.ui.item_list');"
                , "local targetBox = nil;"
                , "for _, t in ipairs(il.getTabs(L().listId)) do"
                , "    if t.key == 'Cat2' then targetBox = t.boxId end"
                , "end;"
                , "require('scripts.ui.tabbar').handleCallback('onTabClick', targetBox);"
                , "local tabBefore = L().activeTab;"
                , "local rowsBefore = #il.getRows(L().listId);"
                -- Ask for a DIFFERENT, genuinely valid tab so the only
                -- thing stopping it is the refusal itself.
                , "local accepted ="
                , "    cip.reopenWithTab('item_container', 9, 400, 400, 'Cat1');"
                , "local out = {accepted = accepted, kind = L().src.endpointKind,"
                , "  id = L().src.id, tabBefore = tabBefore,"
                , "  tabAfter = L().activeTab, rowsBefore = rowsBefore,"
                , "  rowsAfter = #il.getRows(L().listId)};"
                , "cip.closeIfOpen();"
                , "building.getContainerKnowledge = origK;"
                , "return out"
                ]
            case decode (BL.fromStrict (TE.encodeUtf8 r)) ∷ Maybe RejectedReopenProbe of
                Nothing → expectationFailure ("failed to decode: " ⧺ T.unpack r)
                Just p → do
                    crrAccepted p `shouldBe` False
                    crrKind p `shouldBe` Just "building"
                    crrId p `shouldBe` Just 3
                    crrTabBefore p `shouldBe` "Cat2"
                    -- The survivor keeps ITS tab, not the refused
                    -- call's, and its rendered rows are still Cat2's.
                    crrTabAfter p `shouldBe` "Cat2"
                    crrRowsAfter p `shouldBe` crrRowsBefore p

        it "the context menu's 'Contents' row still routes into the window, naming the building endpoint it hit-tested" $ \(env, ls) → do
            resetFixture env ls
            -- #1234 changed `openFor`'s signature, and the context menu
            -- is the ONE player-facing route into this window. Merely
            -- building the menu would not prove the route survived, so
            -- this ACTIVATES the real row's real callback and reads back
            -- which endpoint the manager opened for.
            --
            -- tools/transfer_context_menu_probe.py checks the same thing
            -- against a real Vulkan-rendered menu, but it is manual-only
            -- (needs-gpu) AND currently aborts before its building
            -- scenario on master (e573a8c64e9b) — so this is the gate
            -- that actually runs.
            r ← evalJSON ls $ luaLines
                [ "local origHit = building.hitTestAt;"
                , "local origAct = building.getActivity;"
                , "local origCap = building.getStorageCapacity;"
                , "local origOps = building.getOperations;"
                , "local origStorage = building.getStorage;"
                , "local origWeight = building.getStorageWeight;"
                , "local origInfo = building.getInfo;"
                , "local origSession = package.loaded['scripts.transfer_session'];"
                , "local origK = building.getContainerKnowledge;"
                , "building.hitTestAt = function() return 77 end;"
                , "building.getActivity = function() return 'built' end;"
                , "building.getStorageCapacity = function() return 200 end;"
                , "building.getStorageWeight = function() return 3.0 end;"
                , "building.getOperations = function() return {} end;"
                , "building.getInfo = function() return"
                , "    { displayName='Cargo Hold', gridX=0, gridY=0,"
                , "      tileW=1, tileH=1 } end;"
                , "building.getStorage = function() return {"
                , "    { defName='i1', category='Cat1', weight=1.0 } } end;"
                , "building.getContainerKnowledge = function() return"
                , "  { state='known', storedWeight=1.0, capacity=200.0,"
                , "    revealedAt=0.0, items = {"
                , "    { defName='i1', category='Cat1', weight=1.0 } } } end;"
                -- Keep this test on the Contents row alone: an absent
                -- source is exactly how the real module omits Transfer.
                , "package.loaded['scripts.transfer_session'] ="
                , "    { resolveSource = function() return nil end };"
                , "local cm = require('scripts.ui.context_menu');"
                , "local origShow = cm.show; local captured = nil;"
                , "cm.show = function(items) captured = items end;"
                , "local pg = UI.newPage('cargo_ctx_route', 'overlay');"
                , "local cip = require('scripts.cargo_inventory_panel');"
                -- #1238: the container window owns a STACK of levels;
                -- `L(i)` is the nil-safe projection of one (default the
                -- base), so a read after a close answers absent rather
                -- than raising.
                , "local L = function(i) return cip.getLevel(i or 1) or {src={}} end;"
                , "cip.setup({page = pg, fbW = 1920, fbH = 1080, boxTexSet = 1});"
                , "local claimed ="
                , "    require('scripts.init_context_menu').tryBuildingMenu(10, 10);"
                , "local row = nil;"
                , "for _, itm in ipairs(captured or {}) do"
                , "    if itm.label == 'Contents' then row = itm end"
                , "end;"
                , "local fired = false;"
                , "if row and row.callback then row.callback(); fired = true end;"
                , "local out = {accepted = (claimed == true and fired),"
                , "  open = cip.isOpen(), survivorKind = L().src.endpointKind,"
                , "  survivorId = L().src.id, panelId = L().panelId,"
                , "  listId = L().listId};"
                , "cip.closeIfOpen(); cm.show = origShow;"
                , "package.loaded['scripts.transfer_session'] = origSession;"
                , "building.hitTestAt = origHit; building.getActivity = origAct;"
                , "building.getStorageCapacity = origCap;"
                , "building.getOperations = origOps;"
                , "building.getStorage = origStorage;"
                , "building.getStorageWeight = origWeight;"
                , "building.getInfo = origInfo;"
                , "building.getContainerKnowledge = origK;"
                , "return out"
                ]
            case decode (BL.fromStrict (TE.encodeUtf8 r)) ∷ Maybe EndpointRejectProbe of
                Nothing → expectationFailure ("failed to decode: " ⧺ T.unpack r)
                Just p → do
                    -- The menu claimed the click and offered a real,
                    -- callable Contents row.
                    erpAccepted p `shouldBe` True
                    erpOpen p `shouldBe` True
                    erpSurvivorKind p `shouldBe` Just "building"
                    erpSurvivorId p `shouldBe` Just 77

        it "an open unit endpoint that stops being eligible closes on the next refresh" $ \(env, ls) → do
            resetFixture env ls
            r ← evalJSON ls $ luaLines
                [ "local origEp = unit.transferEndpointInfo;"
                , "_G.__epEligible = true;"
                , "unit.transferEndpointInfo = function() return"
                , "  { eligible = _G.__epEligible, displayName = 'Acolyte',"
                , "    capacity = 40.0, storedWeight = 1.0, contents = {"
                , "    { defName='i1', category='Cat1', weight=1.0 } } } end;"
                , "local pg = UI.newPage('cargo_ep_lifecycle', 'overlay');"
                , "local cip = require('scripts.cargo_inventory_panel');"
                -- #1238: the container window owns a STACK of levels;
                -- `L(i)` is the nil-safe projection of one (default the
                -- base), so a read after a close answers absent rather
                -- than raising.
                , "local L = function(i) return cip.getLevel(i or 1) or {src={}} end;"
                , "cip.setup({page = pg, fbW = 1920, fbH = 1080, boxTexSet = 1});"
                , "cip.openFor('unit', 5, 300, 300);"
                , "local openBefore = cip.isOpen();"
                , "cip.update(0.1);"
                , "local openAfterTick = cip.isOpen();"
                , "_G.__epEligible = false;"
                , "cip.update(0.1);"
                , "local out = {accepted = openBefore, open = cip.isOpen(),"
                , "  panelId = L().panelId, listId = L().listId,"
                , "  survivorKind = L().src.endpointKind, survivorId = L().src.id};"
                , "unit.transferEndpointInfo = origEp;"
                , "if not openAfterTick then out.accepted = false end;"
                , "return out"
                ]
            case decode (BL.fromStrict (TE.encodeUtf8 r)) ∷ Maybe EndpointRejectProbe of
                Nothing → expectationFailure ("failed to decode: " ⧺ T.unpack r)
                Just p → do
                    -- Open, and still open after an ordinary tick.
                    erpAccepted p `shouldBe` True
                    erpOpen p `shouldBe` False
                    erpPanelId p `shouldBe` Nothing
                    erpListId p `shouldBe` Nothing
                    erpSurvivorKind p `shouldBe` Nothing

        it "item_contents_panel: an item level offers NO transfer action -- a plain row routes to no menu at all, while a CONTAINER row opens the next level -- and an empty container still renders its empty state (#1088/#1238)" $ \(env, ls) → do
            resetFixture env ls
            -- #1088 requirement 8 said a missing right-click callback
            -- must create no right-click action at all, and before
            -- #1238 this level supplied none. It does now: an
            -- item-container row has to be able to open the next level,
            -- which is INSPECTION rather than transfer. So the contract
            -- this pins is the one that replaced it -- the level still
            -- offers no transfer operation, and a row with nothing to
            -- offer routes to no menu (`routed` false) rather than
            -- opening an empty one. The widget's own "no callback, no
            -- surface" rule is unchanged and is covered by
            -- "Item list widget".
            r ← evalJSON ls $ luaLines
                [ "local orig = unit.getItemContents;"
                , "local origInv = unit.getInventory;"
                , "unit.getInventory = function() return {} end;"
                , "unit.getItemContents = function() return {"
                , "    { defName='bandage', displayName='Bandage', count=3,"
                , "      weight=0.1, condition=100 },"
                , "} end;"
                , "local pg = UI.newPage('item_contents_page_a', 'overlay'); UI.showPage(pg);"
                , "local icp = require('scripts.item_contents_panel');"
                , "local cip = require('scripts.cargo_inventory_panel');"
                , "local L = function(i) return cip.getLevel(i or 1) or {src={}} end;"
                , "cip.setup({page = pg, fbW = 1920, fbH = 1080, boxTexSet = 1, menuFont = 1});"
                , "icp.openFor(3, 'first_aid_kit', 100, 100);"
                , "local il = require('scripts.ui.item_list');"
                , "local rows = il.getRows(L().listId);"
                , "local info = UI.getElementInfo(rows[1].hitId);"
                , "local routed = il.handleCallback('onItemListRightClick', rows[1].hitId);"
                , "local depthPlain = cip.depth();"
                -- A CONTAINER row instead: right-clicking it offers the
                -- inspection entry, and firing that entry pushes a
                -- level.
                , "unit.getItemContents = function(_, _, _, path)"
                , "    if path and #path > 0 then return {"
                , "        { defName='wrench', displayName='Wrench', count=1,"
                , "          weight=0.4, condition=100 } } end;"
                , "    return { { defName='toolbox', displayName='Toolbox',"
                , "               kind='container', instanceId=77, count=1,"
                , "               weight=2.0, condition=100 } } end;"
                , "icp.openFor(3, 'first_aid_kit', 100, 100);"
                , "local crows = il.getRows(L().listId);"
                , "_G.__menu = nil;"
                , "local cm = require('scripts.ui.context_menu');"
                , "local origShow = cm.show;"
                , "cm.show = function(items) _G.__menu = items end;"
                , "local routedContainer ="
                , "    il.handleCallback('onItemListRightClick', crows[1].hitId);"
                , "local entry = _G.__menu and _G.__menu[#_G.__menu] or nil;"
                , "if entry and entry.callback then entry.callback() end;"
                , "cm.show = origShow;"
                , "local depthNested = cip.depth();"
                , "local nestedRow = (cip.getLevel(2) and"
                , "    #il.getRows(cip.getLevel(2).listId)) or 0;"
                , "icp.closeIfOpen();"
                , "unit.getItemContents = function() return {} end;"
                , "local pg2 = UI.newPage('item_contents_page_b', 'overlay'); UI.showPage(pg2);"
                , "cip.setup({page = pg2, fbW = 1920, fbH = 1080, boxTexSet = 1, menuFont = 1});"
                , "icp.openFor(3, 'first_aid_kit', 100, 100);"
                , "local emptyOpen = icp.isOpen();"
                , "local emptyRows = #il.getRows(L().listId);"
                , "local hasEmptyLabel = false;"
                , "for _, e in ipairs(UI.getVisibleElements()) do"
                , "    if e.name == 'item_contents_empty_text'"
                , "       or e.text == '(empty)' then hasEmptyLabel = true end"
                , "end;"
                , "icp.closeIfOpen();"
                , "unit.getItemContents = orig; unit.getInventory = origInv;"
                , "return {rowCount = #rows, interactive = info.interactive,"
                , "        routed = routed, emptyOpen = emptyOpen,"
                , "        emptyRows = emptyRows, hasEmptyLabel = hasEmptyLabel,"
                , "        depthPlain = depthPlain, routedContainer = routedContainer,"
                , "        menuLabel = entry and entry.label or '',"
                , "        depthNested = depthNested, nestedRow = nestedRow}"
                ]
            case decode (BL.fromStrict (TE.encodeUtf8 r)) ∷ Maybe ItemContentsMigrationProbe of
                Nothing → expectationFailure ("failed to decode: " ⧺ T.unpack r)
                Just p → do
                    icmpRowCount p `shouldBe` 1
                    -- The level routes right-clicks, so the row IS an
                    -- interactive surface (#1238) ...
                    icmpInteractive p `shouldBe` True
                    -- ... but a bandage is not a container and this
                    -- level has no transfer action, so nothing opens.
                    icmpRouted p `shouldBe` False
                    icmpDepthPlain p `shouldBe` 1
                    -- A container row offers exactly the inspection
                    -- entry, and firing it pushes a level rendering the
                    -- nested contents.
                    icmpRoutedContainer p `shouldBe` True
                    icmpMenuLabel p `shouldBe` "Contents"
                    icmpDepthNested p `shouldBe` 2
                    icmpNestedRow p `shouldBe` 1
                    icmpEmptyOpen p `shouldBe` True
                    icmpEmptyRows p `shouldBe` 0
                    icmpHasEmptyLabel p `shouldBe` True

        it "crafting_panel: a resize preserves the open station AND in-progress recipe count/until-target edits (round-14 review)" $ \(env, ls) → do
            resetFixture env ls
            -- #750 round-14 review: round-13's snapshot only preserved
            -- WHICH station was open — plain show(bid) always resets
            -- recipeInputs (per-recipe count-text/until-target edits),
            -- so a resize still silently discarded them. recipeInputs
            -- is a plain Lua table, unvalidated against real recipe
            -- data (unlike recipePage, which self-clamps to the current
            -- recipe count and so can't be tested without real station
            -- recipes) — set directly and checked for an exact round trip.
            r ← evalJSON ls $ luaLines
                [ "local origGetInfo = building.getInfo;"
                , "building.getInfo = function() return {name='test_station'} end;"
                , "local hud = require('scripts.hud');"
                , "hud.init(1,2,1920,1080);"
                , "hud.createUI();"
                , "local cp = require('scripts.crafting_panel');"
                , "cp.show(99);"
                , "cp.state.recipeInputs = {my_recipe = {count = '5', until_ = true}};"
                , "local wasOpenBefore = cp.isOpen();"
                , "hud.onFramebufferResize(1600, 900);"
                , "building.getInfo = origGetInfo;"
                , "local ri = cp.state.recipeInputs.my_recipe;"
                , "return {wasOpenBefore = wasOpenBefore, isOpenAfter = cp.isOpen(),"
                , "        bidAfter = cp.state.bid, countAfter = ri and ri.count or '',"
                , "        untilAfter = ri and ri.until_ or false}"
                ]
            case decode (BL.fromStrict (TE.encodeUtf8 r)) ∷ Maybe CraftingResizeProbe of
                Nothing → expectationFailure ("failed to decode: " ⧺ T.unpack r)
                Just p → do
                    crfWasOpenBefore p `shouldBe` True
                    crfIsOpenAfter p `shouldBe` True
                    crfBidAfter p `shouldBe` 99
                    crfCountAfter p `shouldBe` "5"
                    crfUntilAfter p `shouldBe` True

        it "plant_panel: a resize preserves the open tile AND the player's sort mode/crop selection (round-14 review)" $ \(env, ls) → do
            resetFixture env ls
            -- #750 round-14 review: round-13's snapshot only preserved
            -- WHICH tile was open — plain show() always resets
            -- sortMode/selectedCrop, so a resize still silently
            -- discarded the player's sort choice and crop selection.
            r ← evalJSON ls $ luaLines
                [ "local origPlantable = world.isPlantable;"
                , "local origSuitability = world.getPlantSuitability;"
                , "world.isPlantable = function() return true end;"
                , "world.getPlantSuitability = function() return {"
                , "    {name = 'wheat', score = 0.8}, {name = 'corn', score = 0.5}"
                , "} end;"
                , "local hud = require('scripts.hud');"
                , "hud.init(1,2,1920,1080);"
                , "hud.createUI();"
                , "local pp = require('scripts.plant_panel');"
                , "pp.show('main_world', 10, 20);"
                , "pp.state.sortMode = 'name';"
                , "pp.state.selectedCrop = 'corn';"
                , "local wasOpenBefore = pp.isOpen();"
                , "hud.onFramebufferResize(1600, 900);"
                , "world.isPlantable = origPlantable;"
                , "world.getPlantSuitability = origSuitability;"
                , "return {wasOpenBefore = wasOpenBefore, isOpenAfter = pp.isOpen(),"
                , "        gxAfter = pp.state.gx, gyAfter = pp.state.gy,"
                , "        sortAfter = pp.state.sortMode, cropAfter = pp.state.selectedCrop}"
                ]
            case decode (BL.fromStrict (TE.encodeUtf8 r)) ∷ Maybe PlantResizeProbe of
                Nothing → expectationFailure ("failed to decode: " ⧺ T.unpack r)
                Just p → do
                    prpWasOpenBefore p `shouldBe` True
                    prpIsOpenAfter p `shouldBe` True
                    prpGxAfter p `shouldBe` 10
                    prpGyAfter p `shouldBe` 20
                    prpSortAfter p `shouldBe` "name"
                    prpCropAfter p `shouldBe` "corn"

        it "item_contents_panel: the panel width is capped instead of only repositioning an oversized panel" $ \(env, ls) → do
            resetFixture env ls
            r ← evalJSON ls $ luaLines
                [ "engine.setUIScale(4.0);"
                , "local orig = unit.getItemContents;"
                , "unit.getItemContents = function() return {} end;"
                , "local pg = UI.newPage('item_contents_test_page', 'overlay');"
                , "local icp = require('scripts.item_contents_panel');"
                , "local cip = require('scripts.cargo_inventory_panel');"
                , "local L = function(i) return cip.getLevel(i or 1) or {src={}} end;"
                , "cip.setup({page = pg, fbW = 800, fbH = 2160, boxTexSet = 1});"
                , "icp.openFor(1, 'some_container', 400, 400, nil);"
                , "local p = require('scripts.ui.panel');"
                , "local x, y = p.getPosition(L().panelId);"
                , "local pw, ph = p.getSize(L().panelId);"
                , "unit.getItemContents = orig;"
                , "return {w=pw,"
                , "        inFrame=(x>=0 and y>=0 and (x+pw)<=800 and (y+ph)<=2160)}"
                ]
            case decode (BL.fromStrict (TE.encodeUtf8 r)) ∷ Maybe WidthCapProbe of
                Nothing → expectationFailure ("failed to decode: " ⧺ T.unpack r)
                Just p → wcpInFrame p `shouldBe` True

    -- #1237 (epic #1013, UIT-1B). The container window renders a BUILDING
    -- endpoint's REMEMBERED contents, not live truth. Every case below
    -- deliberately leaves building.getStorage / getStorageWeight /
    -- getStorageCapacity answering something the knowledge record does
    -- not, so a value that came from the live path is unmistakable rather
    -- than merely equal-by-luck. The issue's own gate for the rendered
    -- integration is tools/item_list_widget_probe.py (manual-only,
    -- needs-gpu); these are the CI-blocking half.
    describe "cargo_inventory_panel.lua renders LAST-KNOWN container contents with an age indicator (#1237)" $ do
        it "a never-inspected container renders as UNKNOWN — never as an empty one — with a live capacity, an unknown stored weight and no age line" $ \(env, ls) → do
            resetFixture env ls
            r ← evalJSON ls $ luaLines
                [ "local origK = building.getContainerKnowledge;"
                , "local origInfo = building.getInfo;"
                , "local origCap = building.getStorageCapacity;"
                , "local origStorage = building.getStorage;"
                , "local origWeight = building.getStorageWeight;"
                -- Live truth says the hold is full of ore; none of it may
                -- reach the window, because nobody has looked inside.
                , "building.getStorageCapacity = function() return 400 end;"
                , "building.getStorageWeight = function() return 88.0 end;"
                , "building.getStorage = function() return {"
                , "    { defName='ore', displayName='Ore',"
                , "      category='Materials', weight=44.0 } } end;"
                , "building.getInfo = function() return"
                , "    { displayName='Cargo Hold', gridX=0, gridY=0,"
                , "      tileW=1, tileH=1 } end;"
                -- Exactly what the engine reports with no record at all:
                -- a numeric zero weight and NO revealedAt key.
                , "building.getContainerKnowledge = function() return"
                , "  { state='unknown', items={}, storedWeight=0.0,"
                , "    capacity=400.0 } end;"
                , "local pg = UI.newPage('cargo_k_unknown', 'overlay');"
                , "UI.showPage(pg);"
                , "local cip = require('scripts.cargo_inventory_panel');"
                -- #1238: the container window owns a STACK of levels;
                -- `L(i)` is the nil-safe projection of one (default the
                -- base), so a read after a close answers absent rather
                -- than raising.
                , "local L = function(i) return cip.getLevel(i or 1) or {src={}} end;"
                , "cip.setup({page = pg, fbW = 1920, fbH = 1080,"
                , "           boxTexSet = 1, menuFont = 1});"
                , "local accepted = cip.openFor('building', 11, 300, 300);"
                , "local il = require('scripts.ui.item_list');"
                , "local lbl = require('scripts.ui.label');"
                , "local rows = il.getRows(L().listId);"
                , "local names = {};"
                , "for i, rw in ipairs(rows) do names[i] = rw.item.defName end;"
                , "local et = nil;"
                , "for _, e in ipairs(UI.getVisibleElements()) do"
                , "    if e.name == 'cargo_inv_empty_text' then et = e.text end"
                , "end;"
                , "local out = {accepted = accepted,"
                , "  subtitle = lbl.getText(L().subtitleId),"
                , "  age = L().ageId and lbl.getText(L().ageId) or nil,"
                , "  emptyText = et, rowCount = #rows,"
                , "  rowNames = table.concat(names, ',')};"
                , "cip.closeIfOpen();"
                , "building.getContainerKnowledge = origK;"
                , "building.getInfo = origInfo;"
                , "building.getStorageCapacity = origCap;"
                , "building.getStorage = origStorage;"
                , "building.getStorageWeight = origWeight;"
                , "return out"
                ]
            case decode (BL.fromStrict (TE.encodeUtf8 r)) ∷ Maybe ContainerKnowledgeProbe of
                Nothing → expectationFailure ("failed to decode: " ⧺ T.unpack r)
                Just p → do
                    ckpAccepted p `shouldBe` True
                    -- Capacity is LIVE and always shown; the stored
                    -- weight reads as unknown rather than as the
                    -- engine's numeric 0 (and never as the live 88).
                    ckpSubtitle p `shouldBe` "Storage: unknown / 400.00 kg"
                    ckpAge p `shouldBe` Nothing
                    ckpRowCount p `shouldBe` 0
                    ckpRowNames p `shouldBe` ""
                    -- The distinguishing fact: an explicit
                    -- never-inspected line, NOT an empty item list.
                    ckpEmpty p `shouldBe`
                        Just "Contents unknown (never inspected)"

        it "a known-EMPTY container renders as empty, with a stored weight of zero and an age derived from revealedAt" $ \(env, ls) → do
            resetFixture env ls
            r ← evalJSON ls $ luaLines
                [ "local origK = building.getContainerKnowledge;"
                , "local origInfo = building.getInfo;"
                , "local origGT = engine.gameTime;"
                , "local origStorage = building.getStorage;"
                -- Live truth again disagrees, in the other direction.
                , "building.getStorage = function() return {"
                , "    { defName='ore', category='Materials', weight=44.0 } } end;"
                , "building.getInfo = function() return"
                , "    { displayName='Cargo Hold' } end;"
                , "building.getContainerKnowledge = function() return"
                , "  { state='empty', items={}, storedWeight=0.0,"
                , "    capacity=400.0, revealedAt=100.0 } end;"
                , "engine.gameTime = function() return 250.0 end;"
                , "local pg = UI.newPage('cargo_k_empty', 'overlay');"
                , "UI.showPage(pg);"
                , "local cip = require('scripts.cargo_inventory_panel');"
                -- #1238: the container window owns a STACK of levels;
                -- `L(i)` is the nil-safe projection of one (default the
                -- base), so a read after a close answers absent rather
                -- than raising.
                , "local L = function(i) return cip.getLevel(i or 1) or {src={}} end;"
                , "cip.setup({page = pg, fbW = 1920, fbH = 1080,"
                , "           boxTexSet = 1, menuFont = 1});"
                , "local accepted = cip.openFor('building', 12, 300, 300);"
                , "local il = require('scripts.ui.item_list');"
                , "local lbl = require('scripts.ui.label');"
                , "local rows = il.getRows(L().listId);"
                , "local et = nil;"
                , "for _, e in ipairs(UI.getVisibleElements()) do"
                , "    if e.name == 'cargo_inv_empty_text' then et = e.text end"
                , "end;"
                , "local out = {accepted = accepted,"
                , "  subtitle = lbl.getText(L().subtitleId),"
                , "  age = L().ageId and lbl.getText(L().ageId) or nil,"
                , "  emptyText = et, rowCount = #rows, rowNames = ''};"
                , "cip.closeIfOpen();"
                , "building.getContainerKnowledge = origK;"
                , "building.getInfo = origInfo; engine.gameTime = origGT;"
                , "building.getStorage = origStorage;"
                , "return out"
                ]
            case decode (BL.fromStrict (TE.encodeUtf8 r)) ∷ Maybe ContainerKnowledgeProbe of
                Nothing → expectationFailure ("failed to decode: " ⧺ T.unpack r)
                Just p → do
                    ckpAccepted p `shouldBe` True
                    ckpSubtitle p `shouldBe` "Storage: 0.00 / 400.00 kg"
                    ckpRowCount p `shouldBe` 0
                    ckpEmpty p `shouldBe` Just "(empty)"
                    -- 250 - 100 = 150 game seconds. Derived from
                    -- revealedAt against engine.gameTime(), never a
                    -- wall clock.
                    ckpAge p `shouldBe` Just "as of 2m 30s ago"

        it "a KNOWN container renders the remembered rows and remembered weight through the shared widget, not the live storage" $ \(env, ls) → do
            resetFixture env ls
            r ← evalJSON ls $ luaLines
                [ "local origK = building.getContainerKnowledge;"
                , "local origInfo = building.getInfo;"
                , "local origGT = engine.gameTime;"
                , "local origCap = building.getStorageCapacity;"
                , "local origStorage = building.getStorage;"
                , "local origWeight = building.getStorageWeight;"
                , "local origSel = unit.getSelected;"
                , "unit.getSelected = function() return {} end;"
                , "building.getStorageCapacity = function() return 999 end;"
                , "building.getStorageWeight = function() return 777.5 end;"
                , "building.getStorage = function() return {"
                , "    { defName='live_only', category='Wrong', weight=9.0 } } end;"
                , "building.getInfo = function() return"
                , "    { displayName='Cargo Hold' } end;"
                , "building.getContainerKnowledge = function() return"
                , "  { state='known', storedWeight=4.10, capacity=400.0,"
                , "    revealedAt=0.0, items = {"
                , "    { defName='steel_bar', displayName='Steel Bar',"
                , "      category='Materials', weight=2.0 },"
                , "    { defName='steel_bar', displayName='Steel Bar',"
                , "      category='Materials', weight=2.0 },"
                , "    { defName='bandage', displayName='Bandage',"
                , "      category='Medical', weight=0.1 } } } end;"
                , "engine.gameTime = function() return 45.0 end;"
                , "local pg = UI.newPage('cargo_k_known', 'overlay');"
                , "UI.showPage(pg);"
                , "local cip = require('scripts.cargo_inventory_panel');"
                -- #1238: the container window owns a STACK of levels;
                -- `L(i)` is the nil-safe projection of one (default the
                -- base), so a read after a close answers absent rather
                -- than raising.
                , "local L = function(i) return cip.getLevel(i or 1) or {src={}} end;"
                , "cip.setup({page = pg, fbW = 1920, fbH = 1080,"
                , "           boxTexSet = 1, menuFont = 1});"
                , "local accepted = cip.openFor('building', 13, 300, 300);"
                , "local il = require('scripts.ui.item_list');"
                , "local lbl = require('scripts.ui.label');"
                , "local rows = il.getRows(L().listId);"
                , "local names = {};"
                , "for i, rw in ipairs(rows) do names[i] = rw.item.defName end;"
                , "table.sort(names);"
                , "local et = nil;"
                , "for _, e in ipairs(UI.getVisibleElements()) do"
                , "    if e.name == 'cargo_inv_empty_text' then et = e.text end"
                , "end;"
                , "local out = {accepted = accepted,"
                , "  subtitle = lbl.getText(L().subtitleId),"
                , "  age = L().ageId and lbl.getText(L().ageId) or nil,"
                , "  emptyText = et, rowCount = #rows,"
                , "  rowNames = table.concat(names, ',')};"
                , "cip.closeIfOpen();"
                , "building.getContainerKnowledge = origK;"
                , "building.getInfo = origInfo; engine.gameTime = origGT;"
                , "building.getStorageCapacity = origCap;"
                , "building.getStorage = origStorage;"
                , "building.getStorageWeight = origWeight;"
                , "unit.getSelected = origSel;"
                , "return out"
                ]
            case decode (BL.fromStrict (TE.encodeUtf8 r)) ∷ Maybe ContainerKnowledgeProbe of
                Nothing → expectationFailure ("failed to decode: " ⧺ T.unpack r)
                Just p → do
                    ckpAccepted p `shouldBe` True
                    ckpSubtitle p `shouldBe` "Storage: 4.10 / 400.00 kg"
                    -- One row per remembered STACK, through the same
                    -- shared widget the live path uses — and never the
                    -- live_only row those stubs would have produced.
                    ckpRowCount p `shouldBe` 2
                    ckpRowNames p `shouldBe` "bandage,steel_bar"
                    -- "known" has rows, so no empty-state line at all.
                    ckpEmpty p `shouldBe` Nothing
                    ckpAge p `shouldBe` Just "as of 45s ago"

        it "the age ADVANCES as game time passes, retexting the existing label instead of rebuilding the window" $ \(env, ls) → do
            resetFixture env ls
            -- Requirement 3's "advance as game time passes" read
            -- literally: the same fixed revealedAt observed at two
            -- increasing engine.gameTime() values. Routing the age
            -- through the widget's staleness key would tear the popup
            -- down once a game second, so the list instance is required
            -- to survive.
            r ← evalJSON ls $ luaLines
                [ "local origK = building.getContainerKnowledge;"
                , "local origInfo = building.getInfo;"
                , "local origGT = engine.gameTime;"
                , "_G.__now = 10.0;"
                , "engine.gameTime = function() return _G.__now end;"
                , "building.getInfo = function() return"
                , "    { displayName='Cargo Hold' } end;"
                , "building.getContainerKnowledge = function() return"
                , "  { state='known', storedWeight=2.0, capacity=400.0,"
                , "    revealedAt=0.0, items = {"
                , "    { defName='steel_bar', category='Materials',"
                , "      weight=2.0 } } } end;"
                , "local pg = UI.newPage('cargo_k_age', 'overlay');"
                , "UI.showPage(pg);"
                , "local cip = require('scripts.cargo_inventory_panel');"
                -- #1238: the container window owns a STACK of levels;
                -- `L(i)` is the nil-safe projection of one (default the
                -- base), so a read after a close answers absent rather
                -- than raising.
                , "local L = function(i) return cip.getLevel(i or 1) or {src={}} end;"
                , "cip.setup({page = pg, fbW = 1920, fbH = 1080,"
                , "           boxTexSet = 1, menuFont = 1});"
                , "cip.openFor('building', 14, 300, 300);"
                , "local lbl = require('scripts.ui.label');"
                , "local listBefore = L().listId;"
                , "local before = lbl.getText(L().ageId);"
                , "_G.__now = 3700.0;"
                , "cip.update(0.1);"
                , "local after = lbl.getText(L().ageId);"
                , "local out = {before = before, after = after,"
                , "  sameList = (L().listId == listBefore)};"
                , "cip.closeIfOpen();"
                , "building.getContainerKnowledge = origK;"
                , "building.getInfo = origInfo; engine.gameTime = origGT;"
                , "return out"
                ]
            case decode (BL.fromStrict (TE.encodeUtf8 r)) ∷ Maybe ContainerAgeAdvanceProbe of
                Nothing → expectationFailure ("failed to decode: " ⧺ T.unpack r)
                Just p → do
                    caaBefore p `shouldBe` "as of 10s ago"
                    caaAfter p `shouldBe` "as of 1h 1m ago"
                    caaSameList p `shouldBe` True

        it "opening, ticking, re-tabbing and resize-reopening a never-inspected container never calls building.refreshContainerKnowledge" $ \(env, ls) → do
            resetFixture env ls
            -- Requirement 4: opening the window reveals nothing, and
            -- proximity alone never changes what renders. The knowledge
            -- layer has exactly one write verb reachable from Lua, so
            -- counting its calls across every entry point this window
            -- has is the whole gate.
            r ← evalJSON ls $ luaLines
                [ "local origK = building.getContainerKnowledge;"
                , "local origR = building.refreshContainerKnowledge;"
                , "local origInfo = building.getInfo;"
                , "_G.__reveals = 0;"
                , "building.refreshContainerKnowledge = function()"
                , "    _G.__reveals = _G.__reveals + 1; return true end;"
                , "building.getInfo = function() return"
                , "    { displayName='Cargo Hold' } end;"
                , "building.getContainerKnowledge = function() return"
                , "  { state='unknown', items={}, storedWeight=0.0,"
                , "    capacity=400.0 } end;"
                , "local pg = UI.newPage('cargo_k_noreveal', 'overlay');"
                , "UI.showPage(pg);"
                , "local cip = require('scripts.cargo_inventory_panel');"
                -- #1238: the container window owns a STACK of levels;
                -- `L(i)` is the nil-safe projection of one (default the
                -- base), so a read after a close answers absent rather
                -- than raising.
                , "local L = function(i) return cip.getLevel(i or 1) or {src={}} end;"
                , "cip.setup({page = pg, fbW = 1920, fbH = 1080,"
                , "           boxTexSet = 1, menuFont = 1});"
                , "local il0 = require('scripts.ui.item_list');"
                , "local accepted = cip.openFor('building', 15, 300, 300);"
                , "cip.update(0.1); cip.update(0.1); cip.update(0.1);"
                , "local tb = require('scripts.ui.tabbar');"
                , "for _, t in ipairs(il0.getTabs(L().listId)) do"
                , "    if t.key == 'All' then tb.handleCallback('onTabClick', t.boxId) end"
                , "end;"
                , "cip.reopenWithTab('building', 15, 300, 300, 'All');"
                , "local lbl = require('scripts.ui.label');"
                , "local il = require('scripts.ui.item_list');"
                , "local out = {accepted = accepted,"
                , "  subtitle = lbl.getText(L().subtitleId),"
                , "  age = L().ageId and lbl.getText(L().ageId) or nil,"
                , "  rowCount = #il.getRows(L().listId), rowNames = '',"
                , "  reveals = _G.__reveals};"
                , "cip.closeIfOpen();"
                , "building.getContainerKnowledge = origK;"
                , "building.refreshContainerKnowledge = origR;"
                , "building.getInfo = origInfo;"
                , "return out"
                ]
            case decode (BL.fromStrict (TE.encodeUtf8 r)) ∷ Maybe ContainerKnowledgeProbe of
                Nothing → expectationFailure ("failed to decode: " ⧺ T.unpack r)
                Just p → do
                    ckpAccepted p `shouldBe` True
                    ckpReveals p `shouldBe` 0
                    -- Still unknown after all of it.
                    ckpSubtitle p `shouldBe` "Storage: unknown / 400.00 kg"
                    ckpAge p `shouldBe` Nothing
                    ckpRowCount p `shouldBe` 0

        it "a completed movement's refreshed record reaches an ALREADY-OPEN window through the existing per-tick refresh" $ \(env, ls) → do
            resetFixture env ls
            -- Requirement 5. The engine replaces the record at the
            -- instant a deposit commits, so the window needs no new
            -- plumbing — but the unknown → known transition renders zero
            -- rows on BOTH sides of it, which is exactly the case a
            -- row-only staleness comparison would miss.
            r ← evalJSON ls $ luaLines
                [ "local origK = building.getContainerKnowledge;"
                , "local origR = building.refreshContainerKnowledge;"
                , "local origInfo = building.getInfo;"
                , "local origGT = engine.gameTime;"
                , "_G.__reveals = 0;"
                , "building.refreshContainerKnowledge = function()"
                , "    _G.__reveals = _G.__reveals + 1; return true end;"
                , "engine.gameTime = function() return 600.0 end;"
                , "building.getInfo = function() return"
                , "    { displayName='Cargo Hold' } end;"
                , "_G.__record = { state='unknown', items={},"
                , "                storedWeight=0.0, capacity=400.0 };"
                , "building.getContainerKnowledge = function()"
                , "    return _G.__record end;"
                , "local pg = UI.newPage('cargo_k_refresh', 'overlay');"
                , "UI.showPage(pg);"
                , "local cip = require('scripts.cargo_inventory_panel');"
                -- #1238: the container window owns a STACK of levels;
                -- `L(i)` is the nil-safe projection of one (default the
                -- base), so a read after a close answers absent rather
                -- than raising.
                , "local L = function(i) return cip.getLevel(i or 1) or {src={}} end;"
                , "cip.setup({page = pg, fbW = 1920, fbH = 1080,"
                , "           boxTexSet = 1, menuFont = 1});"
                , "cip.openFor('building', 16, 300, 300);"
                , "local il = require('scripts.ui.item_list');"
                , "local lbl = require('scripts.ui.label');"
                , "local beforeSubtitle = lbl.getText(L().subtitleId);"
                , "local beforeRows = #il.getRows(L().listId);"
                , "local beforeAge = L().ageId"
                , "                    and lbl.getText(L().ageId) or nil;"
                -- The engine's own post-commit replacement.
                , "_G.__record = { state='known', storedWeight=2.0,"
                , "  capacity=400.0, revealedAt=597.0, items = {"
                , "  { defName='steel_bar', category='Materials',"
                , "    weight=2.0 } } };"
                , "cip.update(0.1);"
                , "local rows = il.getRows(L().listId);"
                , "local names = {};"
                , "for i, rw in ipairs(rows) do names[i] = rw.item.defName end;"
                , "local out = {beforeSubtitle = beforeSubtitle,"
                , "  beforeRows = beforeRows, beforeAge = beforeAge,"
                , "  afterSubtitle = lbl.getText(L().subtitleId),"
                , "  afterRows = #rows,"
                , "  afterAge = L().ageId"
                , "               and lbl.getText(L().ageId) or nil,"
                , "  afterNames = table.concat(names, ','),"
                , "  reveals = _G.__reveals};"
                , "cip.closeIfOpen();"
                , "building.getContainerKnowledge = origK;"
                , "building.refreshContainerKnowledge = origR;"
                , "building.getInfo = origInfo; engine.gameTime = origGT;"
                , "return out"
                ]
            case decode (BL.fromStrict (TE.encodeUtf8 r)) ∷ Maybe ContainerRefreshProbe of
                Nothing → expectationFailure ("failed to decode: " ⧺ T.unpack r)
                Just p → do
                    crfpBeforeSubtitle p `shouldBe`
                        "Storage: unknown / 400.00 kg"
                    crfpBeforeRows p `shouldBe` 0
                    crfpBeforeAge p `shouldBe` Nothing
                    crfpAfterSubtitle p `shouldBe` "Storage: 2.00 / 400.00 kg"
                    crfpAfterRows p `shouldBe` 1
                    crfpAfterNames p `shouldBe` "steel_bar"
                    -- 600 - 597 = 3 game seconds.
                    crfpAfterAge p `shouldBe` Just "as of just now"
                    -- And the window still wrote nothing itself.
                    crfpReveals p `shouldBe` 0

        it "a UNIT endpoint keeps reporting live contents, with no staleness, no age line and no knowledge read at all" $ \(env, ls) → do
            resetFixture env ls
            -- Requirement 6, and the reason the knowledge branch lives in
            -- the endpoint's own `view` rather than in the renderer: an
            -- entity knows its own contents.
            r ← evalJSON ls $ luaLines
                [ "local origEp = unit.transferEndpointInfo;"
                , "local origUInfo = unit.getInfo;"
                , "local origK = building.getContainerKnowledge;"
                , "_G.__kreads = 0;"
                , "building.getContainerKnowledge = function()"
                , "    _G.__kreads = _G.__kreads + 1; return nil end;"
                , "unit.getInfo = function() return { name = 'Sister Vela' } end;"
                , "unit.transferEndpointInfo = function() return"
                , "  { eligible = true, displayName = 'Acolyte',"
                , "    capacity = 40.0, storedWeight = 31.25, contents = {"
                , "    { defName='wood_log', category='Materials',"
                , "      weight=5.0 } } } end;"
                , "local pg = UI.newPage('cargo_k_unit', 'overlay');"
                , "UI.showPage(pg);"
                , "local cip = require('scripts.cargo_inventory_panel');"
                -- #1238: the container window owns a STACK of levels;
                -- `L(i)` is the nil-safe projection of one (default the
                -- base), so a read after a close answers absent rather
                -- than raising.
                , "local L = function(i) return cip.getLevel(i or 1) or {src={}} end;"
                , "cip.setup({page = pg, fbW = 1920, fbH = 1080,"
                , "           boxTexSet = 1, menuFont = 1});"
                , "local accepted = cip.openFor('unit', 5, 300, 300);"
                , "cip.update(0.1);"
                , "local il = require('scripts.ui.item_list');"
                , "local lbl = require('scripts.ui.label');"
                , "local rows = il.getRows(L().listId);"
                , "local names = {};"
                , "for i, rw in ipairs(rows) do names[i] = rw.item.defName end;"
                , "local et = nil;"
                , "for _, e in ipairs(UI.getVisibleElements()) do"
                , "    if e.name == 'cargo_inv_empty_text' then et = e.text end"
                , "end;"
                , "local out = {accepted = accepted,"
                , "  subtitle = lbl.getText(L().subtitleId),"
                , "  age = L().ageId and lbl.getText(L().ageId) or nil,"
                , "  emptyText = et, rowCount = #rows,"
                , "  rowNames = table.concat(names, ','),"
                , "  reveals = _G.__kreads};"
                , "cip.closeIfOpen();"
                , "unit.transferEndpointInfo = origEp; unit.getInfo = origUInfo;"
                , "building.getContainerKnowledge = origK;"
                , "return out"
                ]
            case decode (BL.fromStrict (TE.encodeUtf8 r)) ∷ Maybe ContainerKnowledgeProbe of
                Nothing → expectationFailure ("failed to decode: " ⧺ T.unpack r)
                Just p → do
                    ckpAccepted p `shouldBe` True
                    ckpSubtitle p `shouldBe` "Carrying: 31.25 / 40.00 kg"
                    ckpAge p `shouldBe` Nothing
                    ckpRowCount p `shouldBe` 1
                    ckpRowNames p `shouldBe` "wood_log"
                    -- Not "(empty)" and not an unknown line either: a
                    -- live endpoint keeps its pre-#1237 blank.
                    ckpEmpty p `shouldBe` Nothing
                    -- The knowledge layer is building-scoped; a unit
                    -- endpoint must never consult it.
                    ckpReveals p `shouldBe` 0

        it "the three header lines never overlap at any supported UI scale, and the age line stays clear of the tab strip (round-1 review)" $ \(env, ls) → do
            resetFixture env ls
            -- Round-1 review: label.new rasterises at fontSize *
            -- uiscale, so a baseline advanced by RAW constants falls
            -- behind its own glyphs as the scale rises — at 2x the age
            -- line's glyph mass reached back up into the subtitle's
            -- while buildLayout's reserved (scaled) space below it sat
            -- empty. Geometry-only, which is exactly what this suite can
            -- measure: engine.getTextWidth is 0 headless, but a text
            -- element's position IS its baseline and label height is
            -- derived straight from fontSize * uiscale.
            r ← evalJSON ls $ luaLines
                [ "local origK = building.getContainerKnowledge;"
                , "local origInfo = building.getInfo;"
                , "local origGT = engine.gameTime;"
                , "engine.gameTime = function() return 300.0 end;"
                , "building.getInfo = function() return"
                , "    { displayName='Cargo Hold' } end;"
                , "building.getContainerKnowledge = function() return"
                , "  { state='known', storedWeight=2.0, capacity=400.0,"
                , "    revealedAt=0.0, items = {"
                , "    { defName='steel_bar', category='Materials',"
                , "      weight=2.0 } } } end;"
                , "local cip = require('scripts.cargo_inventory_panel');"
                -- #1238: the container window owns a STACK of levels;
                -- `L(i)` is the nil-safe projection of one (default the
                -- base), so a read after a close answers absent rather
                -- than raising.
                , "local L = function(i) return cip.getLevel(i or 1) or {src={}} end;"
                , "local il = require('scripts.ui.item_list');"
                , "local lbl = require('scripts.ui.label');"
                , "local out = {};"
                -- The whole #748 envelope: 0.5x floor through the 4x cap.
                , "for i, u in ipairs({0.5, 1.0, 2.0, 3.0, 4.0}) do"
                , "    engine.setUIScale(u);"
                , "    local pg = UI.newPage('cargo_hdr_' .. i, 'overlay');"
                , "    UI.showPage(pg);"
                , "    cip.setup({page = pg, fbW = 1920, fbH = 2160,"
                , "               boxTexSet = 1, menuFont = 1});"
                , "    cip.openFor('building', 20 + i, 300, 300);"
                , "    local hasAge = L().ageId ~= nil;"
                , "    local tb = UI.getElementInfo("
                , "        lbl.getElementHandle(L().titleId)).y;"
                , "    local sb = UI.getElementInfo("
                , "        lbl.getElementHandle(L().subtitleId)).y;"
                , "    local _, sh = lbl.getSize(L().subtitleId);"
                , "    local ab, ah = 0, 0;"
                , "    if hasAge then"
                , "        ab = UI.getElementInfo("
                , "            lbl.getElementHandle(L().ageId)).y;"
                , "        local _, hh = lbl.getSize(L().ageId);"
                , "        ah = hh;"
                , "    end;"
                , "    local tabTop = 1000000;"
                , "    for _, t in ipairs(il.getTabs(L().listId)) do"
                , "        local bi = UI.getElementInfo(t.boxId);"
                , "        if bi and bi.y < tabTop then tabTop = bi.y end"
                , "    end;"
                -- Glyph mass sits ABOVE the baseline, so line N is clear
                -- of line N-1 exactly when (baselineN - heightN) is at
                -- or below baselineN-1.
                , "    out[i] = {scale = u, hasAge = hasAge,"
                , "              titleGap = (sb - sh) - tb,"
                , "              ageGap = (ab - ah) - sb,"
                , "              listGap = tabTop - ab};"
                , "    cip.closeIfOpen();"
                , "end;"
                , "building.getContainerKnowledge = origK;"
                , "building.getInfo = origInfo; engine.gameTime = origGT;"
                , "return out"
                ]
            case decode (BL.fromStrict (TE.encodeUtf8 r)) ∷ Maybe [HeaderGeometryRow] of
                Nothing → expectationFailure ("failed to decode: " ⧺ T.unpack r)
                Just rows → do
                    length rows `shouldBe` 5
                    forM_ rows $ \g → do
                        hgrHasAge g `shouldBe` True
                        -- The subtitle clears the title, the age clears
                        -- the subtitle, and the tab strip clears the age
                        -- — at 0.5x through 4x alike.
                        hgrTitleGap g `shouldSatisfy` (≥ 0)
                        hgrAgeGap g `shouldSatisfy` (≥ 0)
                        hgrListGap g `shouldSatisfy` (> 0)

        it "the row context menu operates on the REMEMBERED row, handing \
           \the queued order the remembered instance id" $ \(env, ls) → do
            resetFixture env ls
            -- #1237 requirement 7, restated for #1249's gesture: the row
            -- action reads the REMEMBERED snapshot, so the id it acts on
            -- is the remembered one — the live storage here is
            -- deliberately empty, and an action sourced from it would
            -- carry nothing. What consumes that id changed (a queued
            -- transfer order, not an immediate `unit.withdrawFromCargo`)
            -- while which id reaches it did not, which is the property
            -- this case exists for.
            r ← evalJSON ls $ luaLines
                [ "local origK = building.getContainerKnowledge;"
                , "local origInfo = building.getInfo;"
                , "local origSel = unit.getSelected;"
                , "local origUInfo = unit.getInfo;"
                , "local origCreate = unit.createTransferOrder;"
                , "local origFac = unit.getFaction;"
                , "local origCmd = faction.isPlayerCommandable;"
                , "local origGT = engine.gameTime;"
                , "engine.gameTime = function() return 0.0 end;"
                , "building.getInfo = function() return"
                , "    { displayName='Cargo Hold', gridX=10, gridY=10,"
                , "      tileW=1, tileH=1 } end;"
                -- instanceId 4242 exists only in the MEMORY; the live
                -- storage is deliberately empty.
                , "building.getContainerKnowledge = function() return"
                , "  { state='known', storedWeight=2.0, capacity=400.0,"
                , "    revealedAt=0.0, items = {"
                , "    { defName='steel_bar', displayName='Steel Bar',"
                , "      category='Materials', weight=2.0,"
                , "      instanceId=4242 } } } end;"
                , "unit.getSelected = function() return {7} end;"
                -- `defName` is not decoration: the retriever is the
                -- order's EXECUTOR, and both the gesture's resolution
                -- and unit_ai's command boundary ask the action registry
                -- whether that unit's species can run `transfer_order`
                -- (#2030). The registry is empty when the MENU is built
                -- and populated by the time the callback commits --
                -- `transfer_gestures.queueOrder` requires
                -- `scripts.unit_ai` lazily -- so a species-less stub
                -- gets offered the gesture and then refused by it. A
                -- real `unit.getInfo` always reports a species; this one
                -- now does too.
                , "unit.getInfo = function() return"
                , "    { name='Sister Vela', defName='acolyte',"
                , "      gridX=10, gridY=11 } end;"
                , "unit.getFaction = function() return 'player' end;"
                , "faction.isPlayerCommandable = function(f)"
                , "    return f == 'player' end;"
                -- Intercepted at the ENGINE boundary, so everything above
                -- it -- the shared gesture builder and unit_ai's own
                -- command -- is the real production path.
                , "unit.createTransferOrder = function(uid, req)"
                , "    _G.__wd = {uid=uid, bid=req.source.id,"
                , "               defName=req.items[1].defName,"
                , "               instanceId=req.items[1].instanceId};"
                , "    return { accepted = true, orderId = 1,"
                , "             outcomes = {} } end;"
                , "local cm = require('scripts.ui.context_menu');"
                , "local origShow = cm.show; local captured = nil;"
                , "cm.show = function(items) captured = items end;"
                , "local pg = UI.newPage('cargo_k_menu', 'overlay');"
                , "UI.showPage(pg);"
                , "local cip = require('scripts.cargo_inventory_panel');"
                -- #1238: the container window owns a STACK of levels;
                -- `L(i)` is the nil-safe projection of one (default the
                -- base), so a read after a close answers absent rather
                -- than raising.
                , "local L = function(i) return cip.getLevel(i or 1) or {src={}} end;"
                , "cip.setup({page = pg, fbW = 1920, fbH = 1080,"
                , "           boxTexSet = 1, menuFont = 1});"
                , "cip.openFor('building', 17, 300, 300);"
                , "local il = require('scripts.ui.item_list');"
                , "local rows = il.getRows(L().listId);"
                , "il.handleCallback('onItemListRightClick', rows[1].hitId);"
                , "local row = captured and captured[1] or nil;"
                , "if row and row.callback then row.callback() end;"
                , "local wd = _G.__wd or {};"
                , "local out = {label = row and row.label or '',"
                , "  defName = wd.defName or '', instanceId = wd.instanceId or -1,"
                , "  bid = wd.bid or -1, uid = wd.uid or -1};"
                , "cip.closeIfOpen(); cm.show = origShow;"
                , "building.getContainerKnowledge = origK;"
                , "building.getInfo = origInfo; unit.getSelected = origSel;"
                , "unit.getInfo = origUInfo;"
                , "unit.createTransferOrder = origCreate;"
                , "unit.getFaction = origFac;"
                , "faction.isPlayerCommandable = origCmd;"
                , "engine.gameTime = origGT;"
                , "return out"
                ]
            case decode (BL.fromStrict (TE.encodeUtf8 r)) ∷ Maybe RememberedMenuProbe of
                Nothing → expectationFailure ("failed to decode: " ⧺ T.unpack r)
                Just p → do
                    rmpLabel p `shouldBe` "Retrieve 1"
                    rmpDefName p `shouldBe` "steel_bar"
                    rmpInstance p `shouldBe` 4242
                    rmpBid p `shouldBe` 17
                    rmpUid p `shouldBe` 7

-- * FromJSON row types owned by the container groups

data CraftingResizeProbe = CraftingResizeProbe
    { crfWasOpenBefore ∷ Bool, crfIsOpenAfter ∷ Bool, crfBidAfter ∷ Int
    , crfCountAfter ∷ Text, crfUntilAfter ∷ Bool } deriving Show
instance FromJSON CraftingResizeProbe where
    parseJSON = withObject "CraftingResizeProbe" $ \o →
        CraftingResizeProbe <$> o .: "wasOpenBefore" <*> o .: "isOpenAfter"
                             <*> o .: "bidAfter" <*> o .: "countAfter" <*> o .: "untilAfter"

data PlantResizeProbe = PlantResizeProbe
    { prpWasOpenBefore ∷ Bool, prpIsOpenAfter ∷ Bool, prpGxAfter ∷ Int, prpGyAfter ∷ Int
    , prpSortAfter ∷ Text, prpCropAfter ∷ Text } deriving Show
instance FromJSON PlantResizeProbe where
    parseJSON = withObject "PlantResizeProbe" $ \o →
        PlantResizeProbe <$> o .: "wasOpenBefore" <*> o .: "isOpenAfter"
                          <*> o .: "gxAfter" <*> o .: "gyAfter"
                          <*> o .: "sortAfter" <*> o .: "cropAfter"

data CargoResizeProbe = CargoResizeProbe
    { crpWasOpenBefore ∷ Bool, crpTabBefore ∷ Text
    , crpIsOpenAfter ∷ Bool, crpKindAfter ∷ Text, crpIdAfter ∷ Int
    , crpTabAfter ∷ Text } deriving Show
instance FromJSON CargoResizeProbe where
    parseJSON = withObject "CargoResizeProbe" $ \o →
        CargoResizeProbe <$> o .: "wasOpenBefore" <*> o .: "tabBefore"
                          <*> o .: "isOpenAfter" <*> o .: "kindAfter"
                          <*> o .: "idAfter" <*> o .: "tabAfter"

-- #1234: one endpoint-kind-agnostic open, read back through the
-- generalized state.
data EndpointOpenProbe = EndpointOpenProbe
    { cwoAccepted ∷ Bool, cwoOpen ∷ Bool, cwoKind ∷ Maybe Text
    , cwoId ∷ Maybe Int, cwoTitle ∷ Text, cwoSubtitle ∷ Text
    , cwoRowCount ∷ Int, cwoTabCount ∷ Int, cwoRowNames ∷ [Text]
    , cwoRightClick ∷ Bool } deriving Show
instance FromJSON EndpointOpenProbe where
    parseJSON = withObject "EndpointOpenProbe" $ \o →
        EndpointOpenProbe <$> o .: "accepted" <*> o .: "open"
                          <*> o .:? "kind" <*> o .:? "id"
                          <*> o .: "title" <*> o .: "subtitle"
                          <*> o .: "rowCount" <*> o .: "tabCount"
                          <*> o .: "rowNames" <*> o .: "rightClick"

-- #1234: a refused open must leave NO panel state behind, and must not
-- disturb a window already open on a valid endpoint.
data EndpointRejectProbe = EndpointRejectProbe
    { erpAccepted ∷ Bool, erpOpen ∷ Bool, erpPanelId ∷ Maybe Int
    , erpListId ∷ Maybe Int, erpSurvivorKind ∷ Maybe Text
    , erpSurvivorId ∷ Maybe Int } deriving Show
instance FromJSON EndpointRejectProbe where
    parseJSON = withObject "EndpointRejectProbe" $ \o →
        EndpointRejectProbe <$> o .: "accepted" <*> o .: "open"
                            <*> o .:? "panelId" <*> o .:? "listId"
                            <*> o .:? "survivorKind" <*> o .:? "survivorId"

-- #1234: a REFUSED reopen must not touch the window that survived it.
data RejectedReopenProbe = RejectedReopenProbe
    { crrAccepted ∷ Bool, crrKind ∷ Maybe Text, crrId ∷ Maybe Int
    , crrTabBefore ∷ Text, crrTabAfter ∷ Text
    , crrRowsBefore ∷ Int, crrRowsAfter ∷ Int } deriving Show
instance FromJSON RejectedReopenProbe where
    parseJSON = withObject "RejectedReopenProbe" $ \o →
        RejectedReopenProbe <$> o .: "accepted" <*> o .:? "kind"
                            <*> o .:? "id" <*> o .: "tabBefore"
                            <*> o .: "tabAfter" <*> o .: "rowsBefore"
                            <*> o .: "rowsAfter"

-- #1234: fresh open resets the tab; the resize path preserves it.
data EndpointTabProbe = EndpointTabProbe
    { etpTabAfterClick ∷ Text, etpTabAfterReopen ∷ Text
    , etpTabAfterFresh ∷ Text, etpKindAfterReopen ∷ Text
    , etpIdAfterReopen ∷ Int } deriving Show
instance FromJSON EndpointTabProbe where
    parseJSON = withObject "EndpointTabProbe" $ \o →
        EndpointTabProbe <$> o .: "tabAfterClick" <*> o .: "tabAfterReopen"
                         <*> o .: "tabAfterFresh" <*> o .: "kindAfterReopen"
                         <*> o .: "idAfterReopen"

-- #1237: how one container-window open RENDERED. `age` and `emptyText`
-- are Maybe because their ABSENCE is the assertion in half these cases
-- (a live or never-inspected endpoint draws no age line; a "known" one
-- draws no empty-state line) — and a Lua table simply has no such key
-- when the panel produced nil. `rowNames` is a comma-joined string
-- rather than a list because an empty Lua table serializes as `{}`,
-- which no [Text] decoder accepts.
data ContainerKnowledgeProbe = ContainerKnowledgeProbe
    { ckpAccepted ∷ Bool, ckpSubtitle ∷ Text, ckpAge ∷ Maybe Text
    , ckpEmpty ∷ Maybe Text, ckpRowCount ∷ Int, ckpRowNames ∷ Text
    , ckpReveals ∷ Int } deriving Show
instance FromJSON ContainerKnowledgeProbe where
    parseJSON = withObject "ContainerKnowledgeProbe" $ \o →
        ContainerKnowledgeProbe <$> o .: "accepted" <*> o .: "subtitle"
                                <*> o .:? "age" <*> o .:? "emptyText"
                                <*> o .: "rowCount"
                                <*> o .:? "rowNames" .!= ""
                                <*> o .:? "reveals" .!= 0

data ContainerAgeAdvanceProbe = ContainerAgeAdvanceProbe
    { caaBefore ∷ Text, caaAfter ∷ Text, caaSameList ∷ Bool } deriving Show
instance FromJSON ContainerAgeAdvanceProbe where
    parseJSON = withObject "ContainerAgeAdvanceProbe" $ \o →
        ContainerAgeAdvanceProbe <$> o .: "before" <*> o .: "after"
                                 <*> o .: "sameList"

data ContainerRefreshProbe = ContainerRefreshProbe
    { crfpBeforeSubtitle ∷ Text, crfpBeforeRows ∷ Int
    , crfpBeforeAge ∷ Maybe Text, crfpAfterSubtitle ∷ Text
    , crfpAfterRows ∷ Int, crfpAfterAge ∷ Maybe Text
    , crfpAfterNames ∷ Text, crfpReveals ∷ Int } deriving Show
instance FromJSON ContainerRefreshProbe where
    parseJSON = withObject "ContainerRefreshProbe" $ \o →
        ContainerRefreshProbe <$> o .: "beforeSubtitle" <*> o .: "beforeRows"
                              <*> o .:? "beforeAge" <*> o .: "afterSubtitle"
                              <*> o .: "afterRows" <*> o .:? "afterAge"
                              <*> o .:? "afterNames" .!= ""
                              <*> o .: "reveals"

-- #1237 round-1 review: the three header lines' vertical clearances at
-- one UI scale. Each gap is (next line's glyph top - previous line's
-- baseline), so a negative value is a real overlap.
data HeaderGeometryRow = HeaderGeometryRow
    { hgrScale ∷ Double, hgrHasAge ∷ Bool, hgrTitleGap ∷ Double
    , hgrAgeGap ∷ Double, hgrListGap ∷ Double } deriving Show
instance FromJSON HeaderGeometryRow where
    parseJSON = withObject "HeaderGeometryRow" $ \o →
        HeaderGeometryRow <$> o .: "scale" <*> o .: "hasAge"
                          <*> o .: "titleGap" <*> o .: "ageGap"
                          <*> o .: "listGap"

data RememberedMenuProbe = RememberedMenuProbe
    { rmpLabel ∷ Text, rmpDefName ∷ Text, rmpInstance ∷ Int
    , rmpBid ∷ Int, rmpUid ∷ Int } deriving Show
instance FromJSON RememberedMenuProbe where
    parseJSON = withObject "RememberedMenuProbe" $ \o →
        RememberedMenuProbe <$> o .: "label" <*> o .: "defName"
                            <*> o .: "instanceId" <*> o .: "bid"
                            <*> o .: "uid"

data ShrinkHeightProbe = ShrinkHeightProbe
    { shpUnshrunkH ∷ Double, shpShrunkH ∷ Double } deriving Show
instance FromJSON ShrinkHeightProbe where
    parseJSON = withObject "ShrinkHeightProbe" $ \o →
        ShrinkHeightProbe <$> o .: "unshrunkH" <*> o .: "shrunkH"

data CargoMigrationProbe = CargoMigrationProbe
    { cmpHasFrame ∷ Bool, cmpTabCount ∷ Int, cmpRowCount ∷ Int
    , cmpInteractive ∷ Bool, cmpRouted ∷ Bool, cmpMenuShown ∷ Bool } deriving Show
instance FromJSON CargoMigrationProbe where
    parseJSON = withObject "CargoMigrationProbe" $ \o →
        CargoMigrationProbe <$> o .: "hasFrame" <*> o .: "tabCount"
                            <*> o .: "rowCount" <*> o .: "interactive"
                            <*> o .: "routed" <*> o .: "menuShown"

data ItemContentsMigrationProbe = ItemContentsMigrationProbe
    { icmpRowCount ∷ Int, icmpInteractive ∷ Bool, icmpRouted ∷ Bool
    , icmpEmptyOpen ∷ Bool, icmpEmptyRows ∷ Int
    , icmpHasEmptyLabel ∷ Bool
    , icmpDepthPlain ∷ Int, icmpRoutedContainer ∷ Bool
    , icmpMenuLabel ∷ Text, icmpDepthNested ∷ Int
    , icmpNestedRow ∷ Int } deriving Show
instance FromJSON ItemContentsMigrationProbe where
    parseJSON = withObject "ItemContentsMigrationProbe" $ \o →
        ItemContentsMigrationProbe <$> o .: "rowCount" <*> o .: "interactive"
                                   <*> o .: "routed" <*> o .: "emptyOpen"
                                   <*> o .: "emptyRows" <*> o .: "hasEmptyLabel"
                                   <*> o .: "depthPlain" <*> o .: "routedContainer"
                                   <*> o .: "menuLabel" <*> o .: "depthNested"
                                   <*> o .: "nestedRow"
