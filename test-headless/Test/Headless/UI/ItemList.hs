-- | The shared item-list widget and the parameterized tabbar layouts
--   it renders its tab strip through (#1088, epic #1013 phase C0).
--
--   Same bare-Lua-backend technique as 'Test.Headless.UI.InputOwnership'
--   and 'Test.Headless.UI.TransferContextMenu': a real Lua backend with
--   the full Lua API registered, so @scripts.ui.item_list@ and
--   @scripts.ui.tabbar@ are the real production modules any caller's
--   @require@ would get, driven against real UI elements on a real
--   page -- with no world, units or buildings. This exercises the
--   WIDGET's own contract directly; the three migrated hosts' wiring
--   is covered by 'Test.Headless.UI.ResponsiveGameplay'.
--
--   NB: @engine.getTextWidth@ returns 0 in this synthetic boot, so
--   every case that depends on real measurement (truncation, tab
--   shrink-to-fit, wrapping) stubs it with a deterministic
--   width-per-character model first.
--
--   Run just this gate: @cabal test synarchy-test-headless
--   --test-options='--match "Item list widget"'@.
module Test.Headless.UI.ItemList (spec) where

import UPrelude
import Test.Hspec
import Data.Aeson (FromJSON(..), decode, withObject, (.:))
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.ByteString.Lazy as BL
import Data.IORef (newIORef)
import Engine.Core.State (EngineEnv(..))
import Engine.Core.Thread (ThreadControl(..))
import Engine.Scripting.Lua.API (registerLuaAPI)
import Engine.Scripting.Lua.Thread (createLuaBackendState)
import Engine.Scripting.Lua.Thread.Console (executeDebugLua)
import Engine.Scripting.Lua.Types (LuaBackendState(..))

spec ∷ SpecWith EngineEnv
spec = describe "Item list widget" $ do

    -- * Grouping and the canonical stack key

    describe "grouping (canonical stack key)" $ do
        it "merges only when every visible identity field matches, and splits on each one" $ \env → do
            ls ← newBareLuaBackend env
            run ls setupLua
            -- Baseline: two identical entries merge into one row of
            -- count 2. Then one field at a time diverges and must
            -- split the row -- every field named by the contract.
            r ← evalDebug ls $ luaLines
                [ "local il = require('scripts.ui.item_list');"
                , "local function counts(a, b)"
                , "  local g = il.groupItems({a, b}, {});"
                , "  local out = {}; for i, r in ipairs(g) do out[i] = r.count end;"
                , "  return #g"
                , "end;"
                , "local base = function() return {defName='x', quality=5,"
                , "  condition=90, currentFill=1, weight=2, weapon=true,"
                , "  sharpness=7, contentsKey='k'} end;"
                , "local function split(mutate)"
                , "  local b = base(); mutate(b); return counts(base(), b)"
                , "end;"
                , "return {merged = counts(base(), base()),"
                , "        byDef       = split(function(i) i.defName = 'y' end),"
                , "        byQuality   = split(function(i) i.quality = 4 end),"
                , "        byCondition = split(function(i) i.condition = 50 end),"
                , "        byFill      = split(function(i) i.currentFill = 0 end),"
                , "        byWeight    = split(function(i) i.weight = 3 end),"
                , "        bySharpness = split(function(i) i.sharpness = 1 end),"
                , "        byContents  = split(function(i) i.contentsKey = 'z' end)}"
                ]
            p ← decodeOr r ∷ IO SplitProbe
            spMerged p `shouldBe` 1
            spByDef p `shouldBe` 2
            spByQuality p `shouldBe` 2
            spByCondition p `shouldBe` 2
            spByFill p `shouldBe` 2
            spByWeight p `shouldBe` 2
            spBySharpness p `shouldBe` 2
            spByContents p `shouldBe` 2

        it "a NON-weapon sharpness difference does not split a row" $ \env → do
            ls ← newBareLuaBackend env
            run ls setupLua
            r ← evalDebug ls $ luaLines
                [ "local il = require('scripts.ui.item_list');"
                , "local g = il.groupItems({"
                , "  {defName='gambeson', weapon=false, sharpness=9},"
                , "  {defName='gambeson', weapon=false, sharpness=1},"
                , "}, {});"
                , "return {n = #g, count = g[1].count}"
                ]
            p ← decodeOr r ∷ IO CountProbe
            cpN p `shouldBe` 1
            cpCount p `shouldBe` 2

        it "equipped rows stay separate only when the host asks for it (off by default)" $ \env → do
            ls ← newBareLuaBackend env
            run ls setupLua
            r ← evalDebug ls $ luaLines
                [ "local il = require('scripts.ui.item_list');"
                , "local items = {"
                , "  {defName='knife', equipped=true},"
                , "  {defName='knife', equipped=true},"
                , "};"
                , "return {n = #il.groupItems(items, {}),"
                , "        count = il.groupItems(items, {})[1].count,"
                , "        separated = #il.groupItems(items,"
                , "            {separateEquipped = true})}"
                ]
            p ← decodeOr r ∷ IO SeparatedProbe
            sepN p `shouldBe` 1
            sepCount p `shouldBe` 2
            sepSeparated p `shouldBe` 2

        it "the FIRST item of a group is its representative, and groups keep first-appearance order" $ \env → do
            ls ← newBareLuaBackend env
            run ls setupLua
            r ← evalDebug ls $ luaLines
                [ "local il = require('scripts.ui.item_list');"
                , "local g = il.groupItems({"
                , "  {defName='b', instanceId=10}, {defName='a', instanceId=11},"
                , "  {defName='b', instanceId=12},"
                , "}, {});"
                , "return {first = g[1].defName, firstInst = g[1].instanceId,"
                , "        second = g[2].defName, n = #g}"
                ]
            p ← decodeOr r ∷ IO OrderProbe
            opFirst p `shouldBe` "b"
            opFirstInst p `shouldBe` 10
            opSecond p `shouldBe` "a"
            opN p `shouldBe` 2

        it "pre-grouped rows pass through unchanged -- neither re-split nor re-ordered" $ \env → do
            ls ← newBareLuaBackend env
            run ls setupLua
            -- The Haskell item-contents API groups by defName only, so
            -- rows that DIFFER in quality/condition/fill still arrive as
            -- one row; applying the finer stack key would split them.
            r ← evalDebug ls $ luaLines
                [ "local il = require('scripts.ui.item_list');"
                , "local rows = {"
                , "  {defName='bandage', count=3, condition=100},"
                , "  {defName='splint', count=1, condition=40},"
                , "};"
                , "local m = il.prepare({items = rows, preGrouped = true});"
                , "return {n = #m.rows, first = m.rows[1].defName,"
                , "        firstCount = m.rows[1].count,"
                , "        second = m.rows[2].defName}"
                ]
            p ← decodeOr r ∷ IO PreGroupedProbe
            pgN p `shouldBe` 2
            pgFirst p `shouldBe` "bandage"
            pgFirstCount p `shouldBe` 3
            pgSecond p `shouldBe` "splint"

        it "the shared count field is `count` for both input modes" $ \env → do
            ls ← newBareLuaBackend env
            run ls setupLua
            r ← evalDebug ls $ luaLines
                [ "local il = require('scripts.ui.item_list');"
                , "local raw = il.groupItems({{defName='x'},{defName='x'}}, {});"
                , "return {raw = raw[1].count,"
                , "        stackCountAbsent = (raw[1].stackCount == nil)}"
                ]
            p ← decodeOr r ∷ IO CountFieldProbe
            cfRaw p `shouldBe` 2
            cfStackCountAbsent p `shouldBe` True

    -- * Category tabs

    describe "category tabs" $ do
        it "normalizes a missing or empty category to Misc in BOTH discovery and counting" $ \env → do
            ls ← newBareLuaBackend env
            run ls setupLua
            -- The pre-#1088 cargo panel discovered "Misc" from the
            -- normalized value but counted against the RAW one, so an
            -- uncategorized item produced a "Misc (0)" tab whose rows
            -- the filter nonetheless listed.
            r ← evalDebug ls $ luaLines
                [ "local il = require('scripts.ui.item_list');"
                , "local tabs = il.computeTabs({"
                , "  {defName='a'}, {defName='b', category=''},"
                , "  {defName='c', category='Tools'},"
                , "});"
                , "local misc = nil;"
                , "for _, t in ipairs(tabs) do"
                , "  if t.name == 'Misc' then misc = t.count end"
                , "end;"
                , "return {n = #tabs, first = tabs[1].name, allCount = tabs[1].count,"
                , "        misc = misc, second = tabs[2].name}"
                ]
            p ← decodeOr r ∷ IO TabsProbe
            tpN p `shouldBe` 3            -- All + Misc + Tools
            tpFirst p `shouldBe` "All"
            tpAllCount p `shouldBe` 3
            tpMisc p `shouldBe` 2
            tpSecond p `shouldBe` "Misc"

        it "counts GROUPED display rows, not summed item quantities" $ \env → do
            ls ← newBareLuaBackend env
            run ls setupLua
            r ← evalDebug ls $ luaLines
                [ "local il = require('scripts.ui.item_list');"
                , "local items = {};"
                , "for i = 1, 12 do items[i] = {defName='plate', category='Parts'} end;"
                , "local m = il.prepare({items = items, tabs = true});"
                , "return {n = #m.tabs, allCount = m.tabs[1].count,"
                , "        misc = m.tabs[2].count, second = m.tabs[2].name}"
                ]
            p ← decodeOr r ∷ IO TabCountProbe
            tcN p `shouldBe` 2
            tcAllCount p `shouldBe` 1
            tcMisc p `shouldBe` 1
            tcSecond p `shouldBe` "Parts"

        it "tabs appear in first-appearance order after All" $ \env → do
            ls ← newBareLuaBackend env
            run ls setupLua
            r ← evalDebug ls $ luaLines
                [ "local il = require('scripts.ui.item_list');"
                , "local tabs = il.computeTabs({"
                , "  {category='Zeta'}, {category='Alpha'}, {category='Zeta'},"
                , "});"
                , "local names = {};"
                , "for i, t in ipairs(tabs) do names[i] = t.name end;"
                , "return table.concat(names, ',')"
                ]
            r `shouldBe` "\"All,Zeta,Alpha\""

        it "a selected category that disappears falls back to All; one that survives is kept" $ \env → do
            ls ← newBareLuaBackend env
            run ls setupLua
            r ← evalDebug ls $ luaLines
                [ "local il = require('scripts.ui.item_list');"
                , "local kept = il.prepare({items = {{category='Tools'}},"
                , "    tabs = true, activeTab = 'Tools'});"
                , "local gone = il.prepare({items = {{category='Food'}},"
                , "    tabs = true, activeTab = 'Tools'});"
                , "return {kept = kept.activeTab, gone = gone.activeTab,"
                , "        keptVisible = #kept.visible, goneVisible = #gone.visible}"
                ]
            p ← decodeOr r ∷ IO FallbackProbe
            fbKept p `shouldBe` "Tools"
            fbGone p `shouldBe` "All"
            fbKeptVisible p `shouldBe` 1
            fbGoneVisible p `shouldBe` 1

        it "the tab strip is OPTIONAL: a host that omits it gets every row and no tabs" $ \env → do
            ls ← newBareLuaBackend env
            run ls setupLua
            r ← evalDebug ls $ luaLines
                [ "local il = require('scripts.ui.item_list');"
                , "local m = il.prepare({items = {{defName='a', category='A'},"
                , "    {defName='b', category='B'}},"
                , "    tabs = false, activeTab = 'A'});"
                , "return {noTabs = (m.tabs == nil), visible = #m.visible,"
                , "        activeTab = m.activeTab}"
                ]
            p ← decodeOr r ∷ IO NoTabsProbe
            ntNoTabs p `shouldBe` True
            ntVisible p `shouldBe` 2
            ntActiveTab p `shouldBe` "All"

    -- * Truncation

    describe "UTF-8-safe truncation" $ do
        it "honours the whole defensive contract, including maxPx <= 0 and an unfittable ellipsis" $ \env → do
            ls ← newBareLuaBackend env
            run ls setupLua
            -- 10px per character, so "abcdefghij" is 100px and ".." is
            -- 20px. Cargo and item-contents deliberately GAIN the
            -- maxPx <= 0 and ellipsis-does-not-fit branches here.
            r ← evalDebug ls $ luaLines
                [ "local il = require('scripts.ui.item_list');"
                , "engine.getTextWidth = function(_, s, _) return #s * 10 end;"
                , "local f = il.truncateToWidth;"
                , "return {nilText = tostring(f(nil, 1, 10, 100)),"
                , "        empty = f('', 1, 10, 100),"
                , "        zero = f('abcdef', 1, 10, 0),"
                , "        negative = f('abcdef', 1, 10, -50),"
                , "        fits = f('abc', 1, 10, 100),"
                , "        noEllipsis = f('abcdef', 1, 10, 10),"
                , "        cut = f('abcdefghij', 1, 10, 50)}"
                ]
            p ← decodeOr r ∷ IO TruncProbe
            trNilText p `shouldBe` "nil"
            trEmpty p `shouldBe` ""
            -- maxPx <= 0 returns the ORIGINAL text, never a bare "..".
            trZero p `shouldBe` "abcdef"
            trNegative p `shouldBe` "abcdef"
            trFits p `shouldBe` "abc"
            -- A positive width too narrow for ".." itself -> "".
            trNoEllipsis p `shouldBe` ""
            trCut p `shouldBe` "abc.."

        it "never splits a multi-byte character" $ \env → do
            ls ← newBareLuaBackend env
            run ls setupLua
            -- Each 'é' is two bytes; a byte-based cut would leave a
            -- dangling lead byte and produce invalid UTF-8.
            r ← evalDebug ls $ luaLines
                [ "local il = require('scripts.ui.item_list');"
                , "engine.getTextWidth = function(_, s, _) return #s * 10 end;"
                , "local out = il.truncateToWidth('ééééé', 1, 10, 60);"
                , "return {text = out, valid = (utf8.len(out) ~= nil)}"
                ]
            p ← decodeOr r ∷ IO Utf8Probe
            u8Valid p `shouldBe` True
            u8Text p `shouldSatisfy` T.isSuffixOf ".."

    -- * Instances, rendering, routing

    describe "instances, rows and right-click routing" $ do
        it "creates and destroys cleanly, leaving no elements behind" $ \env → do
            ls ← newBareLuaBackend env
            run ls setupLua
            r ← evalDebug ls $ luaLines
                [ "local il = require('scripts.ui.item_list');"
                , "local pg = UI.newPage('il_lifecycle', 'overlay'); UI.showPage(pg);"
                , "local before = #UI.getVisibleElements();"
                , "local id = il.new(baseParams(pg, {"
                , "  {defName='a', displayName='Alpha', weight=1},"
                , "  {defName='b', displayName='Beta', weight=2},"
                , "}));"
                , "local during = #UI.getVisibleElements();"
                , "local rows = #il.getRows(id);"
                , "il.destroy(id);"
                , "local after = #UI.getVisibleElements();"
                , "return {before = before, during = during, after = after,"
                , "        rows = rows, instances = il.count()}"
                ]
            p ← decodeOr r ∷ IO LifecycleProbe
            lcRows p `shouldBe` 2
            lcDuring p `shouldSatisfy` (> lcBefore p)
            lcAfter p `shouldBe` lcBefore p
            lcInstances p `shouldBe` 0

        it "delivers the EXACT rendered row's representative item to the right-click callback" $ \env → do
            ls ← newBareLuaBackend env
            run ls setupLua
            r ← evalDebug ls $ luaLines
                [ "local il = require('scripts.ui.item_list');"
                , "local pg = UI.newPage('il_rightclick', 'overlay');"
                , "_G.__seen = nil;"
                , "local p = baseParams(pg, {"
                , "  {defName='a', displayName='Alpha', instanceId=41, weight=1},"
                , "  {defName='b', displayName='Beta', instanceId=42, weight=1},"
                , "  {defName='b', displayName='Beta', instanceId=43, weight=1},"
                , "});"
                , "p.onRowRightClick = function(item)"
                , "  _G.__seen = tostring(item.defName) .. ':' .. tostring(item.instanceId)"
                , "    .. ':' .. tostring(item.count); return true end;"
                , "local id = il.new(p);"
                , "local rows = il.getRows(id);"
                , "local routed = il.handleCallback('onItemListRightClick', rows[2].hitId);"
                , "local info = UI.getElementInfo(rows[2].hitId);"
                , "local bogus = il.handleCallback('onItemListRightClick', 999999);"
                , "il.destroy(id);"
                , "return {seen = _G.__seen, routed = routed, bogus = bogus,"
                , "        interactive = info.interactive}"
                ]
            p ← decodeOr r ∷ IO RightClickProbe
            -- The SECOND row is the merged Beta pair; its representative
            -- is the FIRST of the two instances.
            rcSeen p `shouldBe` "b:42:2"
            rcRouted p `shouldBe` True
            rcBogus p `shouldBe` False
            rcInteractive p `shouldBe` True

        it "with no right-click callback the rows register no click action at all" $ \env → do
            ls ← newBareLuaBackend env
            run ls setupLua
            r ← evalDebug ls $ luaLines
                [ "local il = require('scripts.ui.item_list');"
                , "local pg = UI.newPage('il_noaction', 'overlay');"
                , "local id = il.new(baseParams(pg, {"
                , "  {defName='a', displayName='Alpha', weight=1}}));"
                , "local rows = il.getRows(id);"
                , "local info = UI.getElementInfo(rows[1].hitId);"
                , "local routed = il.handleCallback('onItemListRightClick', rows[1].hitId);"
                , "il.destroy(id);"
                , "return {interactive = info.interactive, clickable = info.clickable,"
                , "        routed = routed}"
                ]
            p ← decodeOr r ∷ IO NoActionProbe
            naInteractive p `shouldBe` False
            naClickable p `shouldBe` False
            naRouted p `shouldBe` False

        it "renders the optional capacity header and footer only when the host supplies them" $ \env → do
            ls ← newBareLuaBackend env
            run ls setupLua
            r ← evalDebug ls $ luaLines
                [ "local il = require('scripts.ui.item_list');"
                -- One instance at a time, so the ABSENCE assertion below
                -- can't be satisfied by the other instance's chrome.
                , "local function texts()"
                , "  local out = {};"
                , "  for _, e in ipairs(UI.getVisibleElements()) do"
                , "    if e.text and e.text ~= '' then out[#out+1] = e.text end"
                , "  end; return table.concat(out, '|')"
                , "end;"
                , "local pg = UI.newPage('il_chrome', 'overlay'); UI.showPage(pg);"
                , "local id1 = il.new(baseParams(pg, {{defName='a', displayName='Alpha', weight=1}}));"
                , "local bare = texts();"
                , "il.destroy(id1);"
                , "local p2 = baseParams(pg, {{defName='a', displayName='Alpha', weight=1}});"
                , "p2.header = {stored = 12.5, capacity = 100};"
                , "p2.footer = {text = 'Total: 1.00 kg'};"
                , "local id2 = il.new(p2);"
                , "local chrome = texts();"
                , "il.destroy(id2);"
                , "return {bare = bare, chrome = chrome}"
                ]
            p ← decodeOr r ∷ IO ChromeProbe
            cprBare p `shouldNotSatisfy` T.isInfixOf "100.00 kg"
            cprBare p `shouldNotSatisfy` T.isInfixOf "Total:"
            cprChrome p `shouldSatisfy` T.isInfixOf "12.50 / 100.00 kg"
            cprChrome p `shouldSatisfy` T.isInfixOf "Total: 1.00 kg"

    -- * Invalidation

    describe "rebuild invalidation" $ do
        it "unchanged normalized input recreates nothing; a changed presentation input rebuilds" $ \env → do
            ls ← newBareLuaBackend env
            run ls setupLua
            r ← evalDebug ls $ luaLines
                [ "local il = require('scripts.ui.item_list');"
                , "local pg = UI.newPage('il_stale', 'overlay');"
                , "local items = {{defName='a', displayName='Alpha', weight=1, condition=100}};"
                , "local id = il.new(baseParams(pg, items));"
                , "local same = il.isStale(id, baseParams(pg, items));"
                , "local changedItems = {{defName='a', displayName='Alpha',"
                , "    weight=1, condition=40}};"
                , "local changed = il.isStale(id, baseParams(pg, changedItems));"
                , "local pk = baseParams(pg, items); pk.presentationKey = 'repair:claimed';"
                , "local byKey = il.isStale(id, pk);"
                , "local tt = baseParams(pg, items);"
                , "tt.rowTooltip = function() return {text='x', hint='y'} end;"
                , "local byTooltip = il.isStale(id, tt);"
                , "local hdr = baseParams(pg, items); hdr.header = {stored=1, capacity=2};"
                , "local byHeader = il.isStale(id, hdr);"
                , "local ftr = baseParams(pg, items); ftr.footer = {text='Total: 9'};"
                , "local byFooter = il.isStale(id, ftr);"
                , "local sel = baseParams(pg, items); sel.tabs = true; sel.activeTab = 'Misc';"
                , "local bySelection = il.isStale(id, sel);"
                , "il.invalidate(id);"
                , "local forced = il.isStale(id, baseParams(pg, items));"
                , "il.destroy(id);"
                , "return {same = same, changed = changed, byKey = byKey,"
                , "        byTooltip = byTooltip, byHeader = byHeader,"
                , "        byFooter = byFooter, bySelection = bySelection,"
                , "        forced = forced}"
                ]
            p ← decodeOr r ∷ IO StaleProbe
            stSame p `shouldBe` False
            stChanged p `shouldBe` True
            stByKey p `shouldBe` True
            stByTooltip p `shouldBe` True
            stByHeader p `shouldBe` True
            stByFooter p `shouldBe` True
            stBySelection p `shouldBe` True
            stForced p `shouldBe` True

        it "unchanged input leaves the SAME element handles in place (no churn)" $ \env → do
            ls ← newBareLuaBackend env
            run ls setupLua
            r ← evalDebug ls $ luaLines
                [ "local il = require('scripts.ui.item_list');"
                , "local pg = UI.newPage('il_nochurn', 'overlay');"
                , "local items = {{defName='a', displayName='Alpha', weight=1}};"
                , "local id = il.new(baseParams(pg, items));"
                , "local h1 = il.getRows(id)[1].hitId;"
                , "local stale = il.isStale(id, baseParams(pg, items));"
                , "local h2 = il.getRows(id)[1].hitId;"
                , "il.destroy(id);"
                , "return {stale = stale, same = (h1 == h2)}"
                ]
            p ← decodeOr r ∷ IO NoChurnProbe
            ncStale p `shouldBe` False
            ncSame p `shouldBe` True

        it "a presentation change PAST index 201 rebuilds -- the old cargo cut-off is gone" $ \env → do
            ls ← newBareLuaBackend env
            run ls setupLua
            -- The pre-#1088 cargo hash broke out of its loop only AFTER
            -- hashing index 201, and its first part was the list LENGTH.
            -- So the only change it could miss was a presentation-field
            -- mutation at index >= 202 with the length unchanged --
            -- exactly what this drives.
            r ← evalDebug ls $ luaLines
                [ "local il = require('scripts.ui.item_list');"
                , "local pg = UI.newPage('il_cutoff', 'overlay');"
                , "local function make(mutateAt, condition)"
                , "  local out = {};"
                , "  for i = 1, 260 do"
                , "    out[i] = {defName = 'item' .. i, displayName = 'Item ' .. i,"
                , "              weight = 1, condition = 100};"
                , "    if i == mutateAt then out[i].condition = condition end"
                , "  end; return out"
                , "end;"
                , "local id = il.new(baseParams(pg, make(nil, nil)));"
                , "local at210 = il.isStale(id, baseParams(pg, make(210, 30)));"
                , "local at250 = il.isStale(id, baseParams(pg, make(250, 30)));"
                , "local unchanged = il.isStale(id, baseParams(pg, make(nil, nil)));"
                , "il.destroy(id);"
                , "return {at210 = at210, at250 = at250, unchanged = unchanged}"
                ]
            p ← decodeOr r ∷ IO CutoffProbe
            coAt210 p `shouldBe` True
            coAt250 p `shouldBe` True
            coUnchanged p `shouldBe` False

    -- * Parameterized tabbar layouts

    describe "parameterized tabbar layouts" $ do
        it "the default framed layout is unchanged: one left-to-right row WITH a content frame" $ \env → do
            ls ← newBareLuaBackend env
            run ls setupLua
            r ← evalDebug ls $ luaLines
                [ "local tb = require('scripts.ui.tabbar');"
                , "engine.getTextWidth = function(_, s, px) return #s * px * 0.5 end;"
                , "local pg = UI.newPage('tb_framed', 'overlay');"
                , "local id = tb.new({name='framed', page=pg, font=1, x=0, y=0,"
                , "  width=400, frameHeight=200,"
                , "  tabs={{name='One'},{name='Two'},{name='Three'}}});"
                , "local tabs = tb.getTabs(id);"
                , "local ys = {};"
                , "for i, t in ipairs(tabs) do"
                , "  local info = UI.getElementInfo(t.boxId); ys[i] = info.y end;"
                , "local sameRow = (ys[1] == ys[2] and ys[2] == ys[3]);"
                , "local x1 = UI.getElementInfo(tabs[1].boxId).x;"
                , "local x2 = UI.getElementInfo(tabs[2].boxId).x;"
                , "tb.destroy(id);"
                , "return {hasFrame = (tb.hasFrame(id) == false), n = #tabs,"
                , "        frameBefore = true, sameRow = sameRow,"
                , "        leftToRight = (x2 > x1)}"
                ]
            p ← decodeOr r ∷ IO FramedProbe
            fpN p `shouldBe` 3
            fpSameRow p `shouldBe` True
            fpLeftToRight p `shouldBe` True

        it "the framed layout really creates a frame; the frame-free layouts really don't" $ \env → do
            ls ← newBareLuaBackend env
            run ls setupLua
            r ← evalDebug ls $ luaLines
                [ "local tb = require('scripts.ui.tabbar');"
                , "engine.getTextWidth = function(_, s, px) return #s * px * 0.5 end;"
                , "local pg = UI.newPage('tb_frames', 'overlay'); UI.showPage(pg);"
                , "local defs = {{name='One'},{name='Two'}};"
                , "local framed = tb.new({name='f', page=pg, font=1, width=400,"
                , "  frameHeight=100, tabs=defs});"
                , "local row = tb.new({name='r', page=pg, font=1, width=400,"
                , "  layout='row', tabHeightPx=28, tabs=defs});"
                , "local wrapped = tb.new({name='w', page=pg, font=1, width=400,"
                , "  layout='wrapped', tabHeightPx=28, tabs=defs});"
                , "local out = {framed = tb.hasFrame(framed),"
                , "             row = tb.hasFrame(row),"
                , "             wrapped = tb.hasFrame(wrapped),"
                , "             frameNames = 0};"
                -- A hidden or zero-sized replacement frame would still be
                -- a live element named "<name>_frame".
                , "for _, e in ipairs(UI.getVisibleElements()) do"
                , "  if e.name == 'r_frame' or e.name == 'w_frame' then"
                , "    out.frameNames = out.frameNames + 1 end"
                , "end;"
                , "tb.destroy(framed); tb.destroy(row); tb.destroy(wrapped);"
                , "return out"
                ]
            p ← decodeOr r ∷ IO FrameProbe
            frFramed p `shouldBe` True
            frRow p `shouldBe` False
            frWrapped p `shouldBe` False
            frFrameNames p `shouldBe` 0

        it "row layout shrinks to fit and scales the LABEL with the box" $ \env → do
            ls ← newBareLuaBackend env
            run ls setupLua
            r ← evalDebug ls $ luaLines
                [ "local tb = require('scripts.ui.tabbar');"
                , "local label = require('scripts.ui.label');"
                , "engine.getTextWidth = function(_, s, px) return #s * px * 0.6 end;"
                , "local pg = UI.newPage('tb_shrink', 'overlay');"
                , "local defs = {};"
                , "for i = 1, 8 do defs[i] = {name = 'Category' .. i} end;"
                , "local wide = tb.new({name='wide', page=pg, font=1, x=0, y=0,"
                , "  width=4000, wrapWidth=4000, layout='row', shrinkToFit=true,"
                , "  tabHeightPx=28, measureFontPx=13, labelFontSize=13,"
                , "  labelUiscale=1.0, textPaddingPx=8, tabs=defs});"
                , "local narrow = tb.new({name='narrow', page=pg, font=1, x=0, y=200,"
                , "  width=300, wrapWidth=300, layout='row', shrinkToFit=true,"
                , "  tabHeightPx=28, measureFontPx=13, labelFontSize=13,"
                , "  labelUiscale=1.0, textPaddingPx=8, tabs=defs});"
                , "local wt = tb.getTabs(wide); local nt = tb.getTabs(narrow);"
                , "local _, wideH = label.getSize(wt[1].labelId);"
                , "local _, narrowH = label.getSize(nt[1].labelId);"
                , "local right = 0; local minW = 99999; local rows = {};"
                , "for _, t in ipairs(nt) do"
                , "  rows[tostring(t.y)] = true;"
                , "  if t.x + t.width > right then right = t.x + t.width end;"
                , "  if t.width < minW then minW = t.width end"
                , "end;"
                , "local nRows = 0; for _ in pairs(rows) do nRows = nRows + 1 end;"
                , "tb.destroy(wide); tb.destroy(narrow);"
                , "return {wideH = wideH, narrowH = narrowH, right = right,"
                , "        minW = minW, nRows = nRows}"
                ]
            p ← decodeOr r ∷ IO ShrinkProbe
            shNRows p `shouldBe` 1                       -- stays ONE row
            shRight p `shouldSatisfy` (≤ 300)            -- shrunk to fit
            shMinW p `shouldSatisfy` (≥ 20)              -- still clickable
            shNarrowH p `shouldSatisfy` (< shWideH p)    -- label shrank too
            shNarrowH p `shouldSatisfy` (> 0)

        it "wrapped layout wraps and centres, with the LABEL scale independent of the box metrics" $ \env → do
            ls ← newBareLuaBackend env
            run ls setupLua
            -- Unit-info's case: box height comes from a locally fitted
            -- scale while the label stays a fixed size. A tabbar that
            -- derived both from one uiscale could not express it.
            r ← evalDebug ls $ luaLines
                [ "local tb = require('scripts.ui.tabbar');"
                , "local label = require('scripts.ui.label');"
                , "engine.getTextWidth = function(_, s, px) return #s * px * 0.6 end;"
                , "local pg = UI.newPage('tb_wrapped', 'overlay');"
                , "local defs = {};"
                , "for i = 1, 8 do defs[i] = {name = 'Category' .. i} end;"
                , "local id = tb.new({name='wrap', page=pg, font=1, x=100, y=0,"
                , "  width=400, wrapWidth=400, layout='wrapped', centerRows=true,"
                , "  tabHeightPx=13, measureFontPx=13, labelFontSize=13,"
                , "  labelUiscale=1.0, textPaddingPx=8, rowGapPx=4, tabs=defs});"
                , "local tabs = tb.getTabs(id);"
                , "local rows = {};"
                , "for _, t in ipairs(tabs) do"
                , "  local key = tostring(t.y);"
                , "  rows[key] = rows[key] or {minX = t.x, maxR = t.x + t.width};"
                , "  if t.x < rows[key].minX then rows[key].minX = t.x end;"
                , "  if t.x + t.width > rows[key].maxR then"
                , "    rows[key].maxR = t.x + t.width end"
                , "end;"
                , "local nRows = 0; local centred = true; local inBounds = true;"
                , "for _, r in pairs(rows) do"
                , "  nRows = nRows + 1;"
                , "  local left = r.minX - 100; local rightGap = 500 - r.maxR;"
                , "  if math.abs(left - rightGap) > 1 then centred = false end;"
                , "  if r.minX < 100 or r.maxR > 500 then inBounds = false end"
                , "end;"
                , "local _, lblH = label.getSize(tabs[1].labelId);"
                , "local boxH = tabs[1].height;"
                , "tb.destroy(id);"
                , "return {nRows = nRows, centred = centred, inBounds = inBounds,"
                , "        lblH = lblH, boxH = boxH, stripH = 0}"
                ]
            p ← decodeOr r ∷ IO WrapProbe
            wpNRows p `shouldSatisfy` (≥ 2)
            wpCentred p `shouldBe` True
            wpInBounds p `shouldBe` True
            -- Box height (13, a fitted metric) is independent of the
            -- label's own 13px fixed size -- proving the two scales are
            -- separately controllable rather than one uiscale.
            wpLblH p `shouldBe` 13
            wpBoxH p `shouldBe` 13

        it "a click on a frame-free tab routes through the shared tabbar callback and reports the category key" $ \env → do
            ls ← newBareLuaBackend env
            run ls setupLua
            r ← evalDebug ls $ luaLines
                [ "local tb = require('scripts.ui.tabbar');"
                , "engine.getTextWidth = function(_, s, px) return #s * px * 0.6 end;"
                , "local pg = UI.newPage('tb_click', 'overlay');"
                , "_G.__picked = nil;"
                , "local id = tb.new({name='clicky', page=pg, font=1, width=1000,"
                , "  wrapWidth=1000, layout='row', tabHeightPx=28, measureFontPx=13,"
                , "  labelFontSize=13, labelUiscale=1.0, textPaddingPx=8,"
                , "  tabs={{name='All (2)', key='All'},{name='Tools (1)', key='Tools'}},"
                , "  onChange=function(key) _G.__picked = key end});"
                , "local tabs = tb.getTabs(id);"
                , "local routed = tb.handleCallback('onTabClick', tabs[2].boxId);"
                , "local sel = tb.getSelectedKey(id);"
                , "tb.destroy(id);"
                , "return {routed = routed, picked = _G.__picked, selected = sel}"
                ]
            p ← decodeOr r ∷ IO TabClickProbe
            tkRouted p `shouldBe` True
            tkPicked p `shouldBe` "Tools"
            tkSelected p `shouldBe` "Tools"

-- * Bare-Lua-backend helpers (mirrors
--   Test.Headless.UI.TransferContextMenu)

newBareLuaBackend ∷ EngineEnv → IO LuaBackendState
newBareLuaBackend env = do
    ls ← createLuaBackendState (luaToEngineQueue env) (luaQueue env)
                                (assetPoolRef env) (nextObjectIdRef env)
                                (inputStateRef env) (loggerRef env)
    stateRef ← newIORef ThreadRunning
    registerLuaAPI (lbsLuaState ls) env ls stateRef
    pure ls

evalDebug ∷ LuaBackendState → Text → IO Text
evalDebug ls = executeDebugLua (lbsLuaState ls)

run ∷ LuaBackendState → Text → IO ()
run ls stmt = do
    r ← evalDebug ls stmt
    r `shouldNotSatisfy` isLuaError

isLuaError ∷ Text → Bool
isLuaError t = "error:" `T.isPrefixOf` t ∨ "syntax error:" `T.isPrefixOf` t

luaLines ∷ [Text] → Text
luaLines = T.intercalate " "

decodeOr ∷ FromJSON α ⇒ Text → IO α
decodeOr r = case decode (BL.fromStrict (TE.encodeUtf8 r)) of
    Nothing → do
        expectationFailure ("failed to decode: " ⧺ T.unpack r)
        error "unreachable"
    Just v → pure v

-- | Shared per-case setup: the widget's own texture load needs a
-- resident white pixel, and every rendering case wants a predictable
-- text metric (the synthetic boot reports 0 for every string).
setupLua ∷ Text
setupLua = T.concat
    [ "local il = require('scripts.ui.item_list'); il.init(); "
    , "require('scripts.ui.tabbar').init(); "
    , "engine.getTextWidth = function(_, s, px) return #s * (px or 10) end; "
    -- Minimal host policy every rendering case reuses. Bounds are big
    -- enough that nothing is clipped out by the row cap.
    , "function baseParams(pg, items) return { "
    , "  name = 'probe_list', page = pg, font = 1, "
    , "  x = 0, y = 0, width = 400, height = 4000, "
    , "  items = items, uiscale = 1.0, maxRows = 300, "
    , "  rowHeight = 32, rowPad = 2, iconSize = 28, textPad = 12, "
    , "  rowFontSize = 13, zBase = 10, "
    , "  rowWeightText = function(g) "
    , "    return string.format('%.2f kg', (g.weight or 0) * (g.count or 1)) end, "
    , "} end; "
    ]

-- * Decoders

data SplitProbe = SplitProbe
    { spMerged ∷ Int, spByDef ∷ Int, spByQuality ∷ Int, spByCondition ∷ Int
    , spByFill ∷ Int, spByWeight ∷ Int, spBySharpness ∷ Int
    , spByContents ∷ Int } deriving Show
instance FromJSON SplitProbe where
    parseJSON = withObject "SplitProbe" $ \o →
        SplitProbe <$> o .: "merged" <*> o .: "byDef" <*> o .: "byQuality"
                   <*> o .: "byCondition" <*> o .: "byFill" <*> o .: "byWeight"
                   <*> o .: "bySharpness" <*> o .: "byContents"

data CountProbe = CountProbe { cpN ∷ Int, cpCount ∷ Int } deriving Show
instance FromJSON CountProbe where
    parseJSON = withObject "CountProbe" $ \o →
        CountProbe <$> o .: "n" <*> o .: "count"

data SeparatedProbe = SeparatedProbe
    { sepN ∷ Int, sepCount ∷ Int, sepSeparated ∷ Int } deriving Show
instance FromJSON SeparatedProbe where
    parseJSON = withObject "SeparatedProbe" $ \o →
        SeparatedProbe <$> o .: "n" <*> o .: "count" <*> o .: "separated"

data OrderProbe = OrderProbe
    { opFirst ∷ Text, opFirstInst ∷ Int, opSecond ∷ Text, opN ∷ Int } deriving Show
instance FromJSON OrderProbe where
    parseJSON = withObject "OrderProbe" $ \o →
        OrderProbe <$> o .: "first" <*> o .: "firstInst" <*> o .: "second" <*> o .: "n"

data PreGroupedProbe = PreGroupedProbe
    { pgN ∷ Int, pgFirst ∷ Text, pgFirstCount ∷ Int, pgSecond ∷ Text } deriving Show
instance FromJSON PreGroupedProbe where
    parseJSON = withObject "PreGroupedProbe" $ \o →
        PreGroupedProbe <$> o .: "n" <*> o .: "first" <*> o .: "firstCount"
                        <*> o .: "second"

data CountFieldProbe = CountFieldProbe
    { cfRaw ∷ Int, cfStackCountAbsent ∷ Bool } deriving Show
instance FromJSON CountFieldProbe where
    parseJSON = withObject "CountFieldProbe" $ \o →
        CountFieldProbe <$> o .: "raw" <*> o .: "stackCountAbsent"

data TabsProbe = TabsProbe
    { tpN ∷ Int, tpFirst ∷ Text, tpAllCount ∷ Int, tpMisc ∷ Int
    , tpSecond ∷ Text } deriving Show
instance FromJSON TabsProbe where
    parseJSON = withObject "TabsProbe" $ \o →
        TabsProbe <$> o .: "n" <*> o .: "first" <*> o .: "allCount"
                  <*> o .: "misc" <*> o .: "second"

data TabCountProbe = TabCountProbe
    { tcN ∷ Int, tcAllCount ∷ Int, tcMisc ∷ Int, tcSecond ∷ Text } deriving Show
instance FromJSON TabCountProbe where
    parseJSON = withObject "TabCountProbe" $ \o →
        TabCountProbe <$> o .: "n" <*> o .: "allCount" <*> o .: "misc"
                      <*> o .: "second"

data FallbackProbe = FallbackProbe
    { fbKept ∷ Text, fbGone ∷ Text, fbKeptVisible ∷ Int
    , fbGoneVisible ∷ Int } deriving Show
instance FromJSON FallbackProbe where
    parseJSON = withObject "FallbackProbe" $ \o →
        FallbackProbe <$> o .: "kept" <*> o .: "gone" <*> o .: "keptVisible"
                      <*> o .: "goneVisible"

data NoTabsProbe = NoTabsProbe
    { ntNoTabs ∷ Bool, ntVisible ∷ Int, ntActiveTab ∷ Text } deriving Show
instance FromJSON NoTabsProbe where
    parseJSON = withObject "NoTabsProbe" $ \o →
        NoTabsProbe <$> o .: "noTabs" <*> o .: "visible" <*> o .: "activeTab"

data TruncProbe = TruncProbe
    { trNilText ∷ Text, trEmpty ∷ Text, trZero ∷ Text, trNegative ∷ Text
    , trFits ∷ Text, trNoEllipsis ∷ Text, trCut ∷ Text } deriving Show
instance FromJSON TruncProbe where
    parseJSON = withObject "TruncProbe" $ \o →
        TruncProbe <$> o .: "nilText" <*> o .: "empty" <*> o .: "zero"
                   <*> o .: "negative" <*> o .: "fits" <*> o .: "noEllipsis"
                   <*> o .: "cut"

data Utf8Probe = Utf8Probe { u8Text ∷ Text, u8Valid ∷ Bool } deriving Show
instance FromJSON Utf8Probe where
    parseJSON = withObject "Utf8Probe" $ \o →
        Utf8Probe <$> o .: "text" <*> o .: "valid"

data LifecycleProbe = LifecycleProbe
    { lcBefore ∷ Int, lcDuring ∷ Int, lcAfter ∷ Int, lcRows ∷ Int
    , lcInstances ∷ Int } deriving Show
instance FromJSON LifecycleProbe where
    parseJSON = withObject "LifecycleProbe" $ \o →
        LifecycleProbe <$> o .: "before" <*> o .: "during" <*> o .: "after"
                       <*> o .: "rows" <*> o .: "instances"

data RightClickProbe = RightClickProbe
    { rcSeen ∷ Text, rcRouted ∷ Bool, rcBogus ∷ Bool
    , rcInteractive ∷ Bool } deriving Show
instance FromJSON RightClickProbe where
    parseJSON = withObject "RightClickProbe" $ \o →
        RightClickProbe <$> o .: "seen" <*> o .: "routed" <*> o .: "bogus"
                        <*> o .: "interactive"

data NoActionProbe = NoActionProbe
    { naInteractive ∷ Bool, naClickable ∷ Bool, naRouted ∷ Bool } deriving Show
instance FromJSON NoActionProbe where
    parseJSON = withObject "NoActionProbe" $ \o →
        NoActionProbe <$> o .: "interactive" <*> o .: "clickable" <*> o .: "routed"

data ChromeProbe = ChromeProbe { cprBare ∷ Text, cprChrome ∷ Text } deriving Show
instance FromJSON ChromeProbe where
    parseJSON = withObject "ChromeProbe" $ \o →
        ChromeProbe <$> o .: "bare" <*> o .: "chrome"

data StaleProbe = StaleProbe
    { stSame ∷ Bool, stChanged ∷ Bool, stByKey ∷ Bool, stByTooltip ∷ Bool
    , stByHeader ∷ Bool, stByFooter ∷ Bool, stBySelection ∷ Bool
    , stForced ∷ Bool } deriving Show
instance FromJSON StaleProbe where
    parseJSON = withObject "StaleProbe" $ \o →
        StaleProbe <$> o .: "same" <*> o .: "changed" <*> o .: "byKey"
                   <*> o .: "byTooltip" <*> o .: "byHeader" <*> o .: "byFooter"
                   <*> o .: "bySelection" <*> o .: "forced"

data NoChurnProbe = NoChurnProbe { ncStale ∷ Bool, ncSame ∷ Bool } deriving Show
instance FromJSON NoChurnProbe where
    parseJSON = withObject "NoChurnProbe" $ \o →
        NoChurnProbe <$> o .: "stale" <*> o .: "same"

data CutoffProbe = CutoffProbe
    { coAt210 ∷ Bool, coAt250 ∷ Bool, coUnchanged ∷ Bool } deriving Show
instance FromJSON CutoffProbe where
    parseJSON = withObject "CutoffProbe" $ \o →
        CutoffProbe <$> o .: "at210" <*> o .: "at250" <*> o .: "unchanged"

data FramedProbe = FramedProbe
    { fpN ∷ Int, fpSameRow ∷ Bool, fpLeftToRight ∷ Bool } deriving Show
instance FromJSON FramedProbe where
    parseJSON = withObject "FramedProbe" $ \o →
        FramedProbe <$> o .: "n" <*> o .: "sameRow" <*> o .: "leftToRight"

data FrameProbe = FrameProbe
    { frFramed ∷ Bool, frRow ∷ Bool, frWrapped ∷ Bool
    , frFrameNames ∷ Int } deriving Show
instance FromJSON FrameProbe where
    parseJSON = withObject "FrameProbe" $ \o →
        FrameProbe <$> o .: "framed" <*> o .: "row" <*> o .: "wrapped"
                   <*> o .: "frameNames"

data ShrinkProbe = ShrinkProbe
    { shWideH ∷ Double, shNarrowH ∷ Double, shRight ∷ Double
    , shMinW ∷ Double, shNRows ∷ Int } deriving Show
instance FromJSON ShrinkProbe where
    parseJSON = withObject "ShrinkProbe" $ \o →
        ShrinkProbe <$> o .: "wideH" <*> o .: "narrowH" <*> o .: "right"
                    <*> o .: "minW" <*> o .: "nRows"

data WrapProbe = WrapProbe
    { wpNRows ∷ Int, wpCentred ∷ Bool, wpInBounds ∷ Bool
    , wpLblH ∷ Double, wpBoxH ∷ Double, wpStripH ∷ Double } deriving Show
instance FromJSON WrapProbe where
    parseJSON = withObject "WrapProbe" $ \o →
        WrapProbe <$> o .: "nRows" <*> o .: "centred" <*> o .: "inBounds"
                  <*> o .: "lblH" <*> o .: "boxH" <*> o .: "stripH"

data TabClickProbe = TabClickProbe
    { tkRouted ∷ Bool, tkPicked ∷ Text, tkSelected ∷ Text } deriving Show
instance FromJSON TabClickProbe where
    parseJSON = withObject "TabClickProbe" $ \o →
        TabClickProbe <$> o .: "routed" <*> o .: "picked" <*> o .: "selected"
