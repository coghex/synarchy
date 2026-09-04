-- | The Item list widget's MODEL contracts (#1088, #1268; split out
--   under #2147): the canonical stack key that decides which items
--   merge, the tracked-temperature summary the group carries, and the
--   category tabs derived from the grouped list.
--
--   Composed by 'Test.Headless.UI.ItemList', which owns the
--   @Item list widget@ describe and the per-case queue drain. This
--   module adds no describe of its own, so every example path is the
--   one the facade already published.
--
--   NB: @engine.getTextWidth@ returns 0 in this synthetic boot;
--   'Test.Headless.UI.ItemList.Support.setupLua' installs the
--   deterministic width-per-character stub every case starts from.
module Test.Headless.UI.ItemList.Model (spec) where

import UPrelude
import Test.Hspec
import Data.Aeson (FromJSON(..), withObject, (.:))
import Engine.Core.State (EngineEnv)
import Test.Headless.UI.ItemList.Support

spec ∷ SpecWith EngineEnv
spec = do

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

    -- * Tracked temperature presentation (#1268)

    describe "tracked temperature summary" $ do
        it "different exact temperatures merge into ONE group whose representative is still the first instance" $ \env → do
            ls ← newBareLuaBackend env
            run ls setupLua
            -- Temperature is deliberately NOT a stack-key field: it
            -- cools continuously, so keying on it (raw OR bucketed)
            -- would split and re-merge a row forever. The visible
            -- identity fields must still split exactly as before.
            r ← evalDebug ls $ luaLines
                [ "local il = require('scripts.ui.item_list');"
                , "local g = il.groupItems({"
                , "  {defName='stew', instanceId=71, temp=35.0},"
                , "  {defName='stew', instanceId=72, temp=61.4},"
                , "  {defName='stew', instanceId=73},"
                , "}, {});"
                -- ... while a REAL identity difference still splits,
                -- temperature difference or not.
                , "local split = il.groupItems({"
                , "  {defName='stew', condition=100, temp=35.0},"
                , "  {defName='stew', condition=40,  temp=35.0},"
                , "}, {});"
                , "local splitSameTemp = il.groupItems({"
                , "  {defName='stew', quality=90, temp=50.0},"
                , "  {defName='stew', quality=10, temp=50.0},"
                , "}, {});"
                , "return {n = #g, count = g[1].count, rep = g[1].instanceId,"
                , "        summary = g[1].tempSummary,"
                , "        split = #split, splitSameTemp = #splitSameTemp}"
                ]
            p ← decodeOr r ∷ IO TempGroupProbe
            tgN p `shouldBe` 1
            tgCount p `shouldBe` 3
            tgRep p `shouldBe` 71
            tgSummary p `shouldBe` "ambient + 35–61°C"
            tgSplit p `shouldBe` 2
            tgSplitSameTemp p `shouldBe` 2

        it "summarizes EVERY member: ambient, one value, a range, and a mixture" $ \env → do
            ls ← newBareLuaBackend env
            run ls setupLua
            -- Rounding happens BEFORE the single-vs-range decision, so
            -- 41.6 and 42.3 are one presented value, never "42–42°C".
            -- The representative is deliberately the COLDEST/last-named
            -- member in several cases, so a summary that reported only
            -- the first instance's value could not pass.
            r ← evalDebug ls $ luaLines
                [ "local il = require('scripts.ui.item_list');"
                , "local function sum(...)"
                , "  local items = {};"
                , "  for i, t in ipairs({...}) do"
                , "    items[i] = {defName='x', temp = (t ~= false) and t or nil}"
                , "  end;"
                , "  return il.groupItems(items, {})[1].tempSummary"
                , "end;"
                , "return {ambient = sum(false, false),"
                , "        single = sum(42.0),"
                , "        singleRounded = sum(41.6, 42.3),"
                , "        range = sum(42.4, 34.4),"
                , "        mixedSingle = sum(false, 42.0),"
                , "        mixedRange = sum(60.2, false, 35.1),"
                , "        negative = sum(-4.2),"
                , "        negativeRange = sum(-4.2, 3.7),"
                , "        halfUp = sum(41.5),"
                , "        halfDown = sum(-41.5),"
                , "        zero = sum(-0.3)}"
                ]
            p ← decodeOr r ∷ IO TempSummaryProbe
            tsAmbient p `shouldBe` "ambient"
            tsSingle p `shouldBe` "42°C"
            tsSingleRounded p `shouldBe` "42°C"
            tsRange p `shouldBe` "34–42°C"
            tsMixedSingle p `shouldBe` "ambient + 42°C"
            tsMixedRange p `shouldBe` "ambient + 35–60°C"
            tsNegative p `shouldBe` "-4°C"
            tsNegativeRange p `shouldBe` "-4–4°C"
            -- Half away from zero, so a value and its negation present
            -- symmetrically.
            tsHalfUp p `shouldBe` "42°C"
            tsHalfDown p `shouldBe` "-42°C"
            -- ... and a rounded-away negative never renders as "-0°C".
            tsZero p `shouldBe` "0°C"

        it "a non-number, NaN or infinite temp is treated as ambient rather than poisoning the range" $ \env → do
            ls ← newBareLuaBackend env
            run ls setupLua
            -- Every comparison against a NaN is false, so an unguarded
            -- min/max walk would silently keep whichever endpoint it
            -- saw first and report a range the group does not have.
            r ← evalDebug ls $ luaLines
                [ "local il = require('scripts.ui.item_list');"
                , "local nan = 0/0;"
                , "local function sum(t)"
                , "  return il.groupItems({{defName='x', temp=t},"
                , "                        {defName='x', temp=40.0}}, {})[1].tempSummary"
                , "end;"
                , "return {nan = sum(nan), inf = sum(math.huge),"
                , "        negInf = sum(-math.huge), str = sum('hot'),"
                , "        onlyNan = il.groupItems({{defName='x', temp=nan}},"
                , "                                {})[1].tempSummary,"
                -- FINITE but too large to hold an integer: math.floor
                -- answers a float there and string.format's "%d" RAISES
                -- on one, which would abort a whole panel rebuild.
                -- iiTemp is a 32-bit float, so unit.setItemTemp(_, _,
                -- 1e38) really can produce this.
                , "        huge = (pcall(function()"
                , "          return il.groupItems({{defName='x', temp=1e38}},"
                , "                               {})[1].tempSummary end))}"
                ]
            p ← decodeOr r ∷ IO TempGuardProbe
            tgdNan p `shouldBe` "ambient + 40°C"
            tgdInf p `shouldBe` "ambient + 40°C"
            tgdNegInf p `shouldBe` "ambient + 40°C"
            tgdStr p `shouldBe` "ambient + 40°C"
            tgdOnlyNan p `shouldBe` "ambient"
            tgdHuge p `shouldBe` True

        it "the widget synthesizes NO summary for rows it did not group" $ \env → do
            ls ← newBareLuaBackend env
            run ls setupLua
            -- Pre-grouped input (the item-contents popup's coarse
            -- by-defName grouping) hides its members from the widget,
            -- so inventing a summary there would present the
            -- representative's temperature as the whole group's --
            -- and the two join helpers must say nothing at all.
            r ← evalDebug ls $ luaLines
                [ "local il = require('scripts.ui.item_list');"
                , "local m = il.prepare({preGrouped = true, items = {"
                , "  {defName='bandage', count=3, temp=55.0}}});"
                , "local row = m.rows[1];"
                , "return {absent = (row.tempSummary == nil),"
                , "        summary = tostring(il.tempSummary(row)),"
                , "        name = il.withTempSuffix('Bandage ×3', row),"
                , "        hint = tostring(il.tempHintLine(row))}"
                ]
            p ← decodeOr r ∷ IO PreGroupedTempProbe
            pgtAbsent p `shouldBe` True
            pgtSummary p `shouldBe` "nil"
            pgtName p `shouldBe` "Bandage ×3"
            pgtHint p `shouldBe` "nil"

        it "row text and tooltip line come from ONE helper, so the two surfaces cannot disagree" $ \env → do
            ls ← newBareLuaBackend env
            run ls setupLua
            r ← evalDebug ls $ luaLines
                [ "local il = require('scripts.ui.item_list');"
                , "local row = il.groupItems({"
                , "  {defName='stew', temp=35.0}, {defName='stew', temp=42.4},"
                , "}, {})[1];"
                , "local ambient = il.groupItems({{defName='rock'}}, {})[1];"
                , "return {name = il.withTempSuffix('Stew ×2', row),"
                , "        hint = il.tempHintLine(row),"
                , "        ambientName = il.withTempSuffix('Rock', ambient),"
                , "        ambientHint = il.tempHintLine(ambient),"
                , "        summary = il.tempSummary(row)}"
                ]
            p ← decodeOr r ∷ IO TempJoinProbe
            tjSummary p `shouldBe` "35–42°C"
            tjName p `shouldBe` "Stew ×2 · 35–42°C"
            tjHint p `shouldBe` "temperature: 35–42°C"
            -- An ambient row is labeled too, never left blank.
            tjAmbientName p `shouldBe` "Rock · ambient"
            tjAmbientHint p `shouldBe` "temperature: ambient"

        it "a rendered row reports its text, summary and tooltip through the widget's dump" $ \env → do
            ls ← newBareLuaBackend env
            run ls setupLua
            -- No Lua API reads a rendered label or an element's tooltip
            -- content back, so this dump extension is the ONLY read
            -- path a probe has for either surface.
            r ← evalDebug ls $ luaLines
                [ "local il = require('scripts.ui.item_list');"
                , "local pg = UI.newPage('il_tempdump', 'overlay'); UI.showPage(pg);"
                , "local p = baseParams(pg, {"
                , "  {defName='stew', displayName='Stew', weight=1, temp=41.6},"
                , "  {defName='stew', displayName='Stew', weight=1, temp=42.3},"
                , "  {defName='rock', displayName='Rock', weight=1},"
                , "});"
                , "p.rowName = function(g) return il.withTempSuffix("
                , "  (g.displayName or g.defName), g) end;"
                , "p.rowTooltip = function(g) return {text = g.displayName,"
                , "  hint = il.tempHintLine(g)} end;"
                , "local id = il.new(p);"
                , "local mine = {};"
                , "for _, d in ipairs(il.dump()) do"
                , "  if d.type == 'item_list' then mine[d.label] = d end"
                , "end;"
                , "il.destroy(id);"
                , "return {hotText = mine['Stew'].text,"
                , "        hotSummary = mine['Stew'].tempSummary,"
                , "        hotTipText = mine['Stew'].tooltipText,"
                , "        hotTipHint = mine['Stew'].tooltipHint,"
                , "        coldText = mine['Rock'].text,"
                , "        coldSummary = mine['Rock'].tempSummary}"
                ]
            p ← decodeOr r ∷ IO TempDumpProbe
            -- Both members round to 42, so the merged row presents ONE
            -- value rather than the degenerate range "42–42°C".
            tdHotText p `shouldBe` "Stew · 42°C"
            tdHotSummary p `shouldBe` "42°C"
            tdHotTipText p `shouldBe` "Stew"
            tdHotTipHint p `shouldBe` "temperature: 42°C"
            tdColdText p `shouldBe` "Rock · ambient"
            tdColdSummary p `shouldBe` "ambient"

        it "cooling within one displayed degree rebuilds nothing; every change to the DISPLAYED summary does" $ \env → do
            ls ← newBareLuaBackend env
            run ls setupLua
            -- Requirement 4 in full. The presentation key is held
            -- CONSTANT throughout, so nothing here can pass through the
            -- host's escape hatch, and each mutation is applied to a
            -- freshly-built item set.
            r ← evalDebug ls $ luaLines
                [ "local il = require('scripts.ui.item_list');"
                , "local pg = UI.newPage('il_tempstale', 'overlay');"
                , "local function items(a, b) return {"
                , "  {defName='stew', displayName='Stew', weight=1, temp=a},"
                , "  {defName='stew', displayName='Stew', weight=1, temp=b}} end;"
                , "local function params(a, b)"
                , "  local p = baseParams(pg, items(a, b));"
                , "  p.presentationKey = 'constant';"
                , "  p.rowName = function(g) return il.withTempSuffix('Stew', g) end;"
                , "  return p"
                , "end;"
                , "local id = il.new(params(41.6, 42.3));"
                , "local h1 = il.getRows(id)[1].hitId;"
                -- Same displayed degree for both members: no rebuild.
                , "local withinDegree = il.isStale(id, params(41.9, 42.4));"
                , "local h2 = il.getRows(id)[1].hitId;"
                -- One member crosses a degree boundary -> the group's
                -- displayed MAXIMUM moves, so the row must rebuild.
                , "local crossed = il.isStale(id, params(41.6, 42.6));"
                -- ... and so must the minimum moving.
                , "local minMoved = il.isStale(id, params(40.2, 42.3));"
                -- Tracked -> ambient, with the surviving member's own
                -- displayed value unchanged.
                , "local toAmbient = il.isStale(id, params(nil, 42.3));"
                , "il.destroy(id);"
                -- Ambient -> tracked, from an all-ambient baseline.
                , "local id2 = il.new(params(nil, nil));"
                , "local toTracked = il.isStale(id2, params(nil, 42.3));"
                , "local stillAmbient = il.isStale(id2, params(nil, nil));"
                , "il.destroy(id2);"
                , "return {withinDegree = withinDegree, handleKept = (h1 == h2),"
                , "        crossed = crossed, minMoved = minMoved,"
                , "        toAmbient = toAmbient, toTracked = toTracked,"
                , "        stillAmbient = stillAmbient}"
                ]
            p ← decodeOr r ∷ IO TempStaleProbe
            tstWithinDegree p `shouldBe` False
            tstHandleKept p `shouldBe` True
            tstCrossed p `shouldBe` True
            tstMinMoved p `shouldBe` True
            tstToAmbient p `shouldBe` True
            tstToTracked p `shouldBe` True
            tstStillAmbient p `shouldBe` False

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

data TempGroupProbe = TempGroupProbe
    { tgN ∷ Int, tgCount ∷ Int, tgRep ∷ Int, tgSummary ∷ Text
    , tgSplit ∷ Int, tgSplitSameTemp ∷ Int } deriving Show
instance FromJSON TempGroupProbe where
    parseJSON = withObject "TempGroupProbe" $ \o →
        TempGroupProbe <$> o .: "n" <*> o .: "count" <*> o .: "rep"
                       <*> o .: "summary" <*> o .: "split"
                       <*> o .: "splitSameTemp"

data TempSummaryProbe = TempSummaryProbe
    { tsAmbient ∷ Text, tsSingle ∷ Text, tsSingleRounded ∷ Text
    , tsRange ∷ Text, tsMixedSingle ∷ Text, tsMixedRange ∷ Text
    , tsNegative ∷ Text, tsNegativeRange ∷ Text, tsHalfUp ∷ Text
    , tsHalfDown ∷ Text, tsZero ∷ Text } deriving Show
instance FromJSON TempSummaryProbe where
    parseJSON = withObject "TempSummaryProbe" $ \o →
        TempSummaryProbe <$> o .: "ambient" <*> o .: "single"
                         <*> o .: "singleRounded" <*> o .: "range"
                         <*> o .: "mixedSingle" <*> o .: "mixedRange"
                         <*> o .: "negative" <*> o .: "negativeRange"
                         <*> o .: "halfUp" <*> o .: "halfDown" <*> o .: "zero"

data TempGuardProbe = TempGuardProbe
    { tgdNan ∷ Text, tgdInf ∷ Text, tgdNegInf ∷ Text, tgdStr ∷ Text
    , tgdOnlyNan ∷ Text, tgdHuge ∷ Bool } deriving Show
instance FromJSON TempGuardProbe where
    parseJSON = withObject "TempGuardProbe" $ \o →
        TempGuardProbe <$> o .: "nan" <*> o .: "inf" <*> o .: "negInf"
                       <*> o .: "str" <*> o .: "onlyNan" <*> o .: "huge"

data PreGroupedTempProbe = PreGroupedTempProbe
    { pgtAbsent ∷ Bool, pgtSummary ∷ Text, pgtName ∷ Text
    , pgtHint ∷ Text } deriving Show
instance FromJSON PreGroupedTempProbe where
    parseJSON = withObject "PreGroupedTempProbe" $ \o →
        PreGroupedTempProbe <$> o .: "absent" <*> o .: "summary"
                            <*> o .: "name" <*> o .: "hint"

data TempJoinProbe = TempJoinProbe
    { tjName ∷ Text, tjHint ∷ Text, tjAmbientName ∷ Text
    , tjAmbientHint ∷ Text, tjSummary ∷ Text } deriving Show
instance FromJSON TempJoinProbe where
    parseJSON = withObject "TempJoinProbe" $ \o →
        TempJoinProbe <$> o .: "name" <*> o .: "hint" <*> o .: "ambientName"
                      <*> o .: "ambientHint" <*> o .: "summary"

data TempDumpProbe = TempDumpProbe
    { tdHotText ∷ Text, tdHotSummary ∷ Text, tdHotTipText ∷ Text
    , tdHotTipHint ∷ Text, tdColdText ∷ Text
    , tdColdSummary ∷ Text } deriving Show
instance FromJSON TempDumpProbe where
    parseJSON = withObject "TempDumpProbe" $ \o →
        TempDumpProbe <$> o .: "hotText" <*> o .: "hotSummary"
                      <*> o .: "hotTipText" <*> o .: "hotTipHint"
                      <*> o .: "coldText" <*> o .: "coldSummary"

data TempStaleProbe = TempStaleProbe
    { tstWithinDegree ∷ Bool, tstHandleKept ∷ Bool, tstCrossed ∷ Bool
    , tstMinMoved ∷ Bool, tstToAmbient ∷ Bool, tstToTracked ∷ Bool
    , tstStillAmbient ∷ Bool } deriving Show
instance FromJSON TempStaleProbe where
    parseJSON = withObject "TempStaleProbe" $ \o →
        TempStaleProbe <$> o .: "withinDegree" <*> o .: "handleKept"
                       <*> o .: "crossed" <*> o .: "minMoved"
                       <*> o .: "toAmbient" <*> o .: "toTracked"
                       <*> o .: "stillAmbient"

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
