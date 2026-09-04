-- | The Item list widget's ROW contracts (#1088, #1238; split out
--   under #2147): UTF-8-safe truncation, the instance/row lifecycle
--   and its right-click routing, and the scroll offset with its clamp.
--
--   Composed by 'Test.Headless.UI.ItemList', which owns the
--   @Item list widget@ describe and the per-case queue drain. This
--   module adds no describe of its own, so every example path is the
--   one the facade already published.
--
--   NB: truncation depends on real measurement, so those cases replace
--   'Test.Headless.UI.ItemList.Support.setupLua' 's default
--   @engine.getTextWidth@ stub with their own per-case model.
module Test.Headless.UI.ItemList.Rows (spec) where

import UPrelude
import Test.Hspec
import qualified Data.Text as T
import Data.Aeson (FromJSON(..), withObject, (.:))
import Engine.Core.State (EngineEnv)
import Test.Headless.UI.ItemList.Support

spec ∷ SpecWith EngineEnv
spec = do

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
            -- #1157 replaced #1107's return-the-original carve-out with
            -- the uniform rule: a budget that cannot fit the ellipsis
            -- drops the field, and a non-positive budget cannot.
            trZero p `shouldBe` ""
            trNegative p `shouldBe` ""
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

        it "a rowIcon callback is authoritative even when it answers nil (missing-icon handle)" $ \env → do
            ls ← newBareLuaBackend env
            run ls setupLua
            -- The item-contents host answers nil for a NEGATIVE icon
            -- handle (that API reports a missing icon that way). A
            -- `p.rowIcon(row) or row.iconTex` fallback would put the
            -- negative handle straight back and render an
            -- undefined-texture sprite, silently undoing the guard.
            r ← evalDebug ls $ luaLines
                [ "local il = require('scripts.ui.item_list');"
                , "local pg = UI.newPage('il_icon', 'overlay'); UI.showPage(pg);"
                , "local p = baseParams(pg, {"
                , "  {defName='missing', displayName='Missing', weight=1, iconTex=-1},"
                , "  {defName='present', displayName='Present', weight=1, iconTex=1},"
                , "});"
                , "p.rowIcon = function(g)"
                , "  if g.iconTex and g.iconTex >= 0 then return g.iconTex end;"
                , "  return nil end;"
                , "local id = il.new(p);"
                , "local names = {};"
                , "for _, e in ipairs(UI.getVisibleElements()) do"
                , "  names[e.name] = true end;"
                , "local rows = #il.getRows(id);"
                , "il.destroy(id);"
                , "return {rows = rows,"
                , "        missingIcon = names['probe_list_icon_1'] == true,"
                , "        presentIcon = names['probe_list_icon_2'] == true}"
                ]
            p ← decodeOr r ∷ IO IconProbe
            ipRows p `shouldBe` 2
            -- No sprite at all for the missing-icon row ...
            ipMissingIcon p `shouldBe` False
            -- ... while the row that DOES have one still gets it, so the
            -- check can't pass by the widget dropping every icon.
            ipPresentIcon p `shouldBe` True

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

    -- * Scroll offset

    -- #1238: the widget gained a scroll offset because the container
    -- window's nesting levels have to be reachable past their row cap
    -- AND have to restore where each level was after a resize. The
    -- CLAMP lives here rather than in a host: only the widget knows the
    -- visible capacity.
    describe "scroll offset" $ do
        it "renders the offset window of the filtered list, and reports \
           \which DATA rows the visible slots are showing" $ \env → do
            ls ← newBareLuaBackend env
            run ls setupLua
            r ← evalDebug ls $ luaLines
                [ "local il = require('scripts.ui.item_list');"
                , "local pg = UI.newPage('il_scroll_window', 'overlay');"
                , "UI.showPage(pg);"
                , "local items = {};"
                , "for i = 1, 12 do items[i] = {defName='d'..i,"
                , "  displayName='D'..i, weight=1} end;"
                , "local p = baseParams(pg, items); p.maxRows = 4;"
                , "local id = il.new(p);"
                , "local function shown()"
                , "  local names, data = {}, {};"
                , "  for i, row in ipairs(il.getRows(id)) do"
                , "    names[i] = row.item.defName; data[i] = row.dataIndex end;"
                , "  return table.concat(names, ',') .. '/' .. table.concat(data, ',') end;"
                , "local first = shown();"
                , "local applied = il.setScrollOffset(id, 5);"
                , "local scrolled = shown();"
                , "local dumped = 0;"
                , "for _, d in ipairs(il.dump()) do"
                , "  if d.label == 'D6' then dumped = d.dataIndex end end;"
                , "il.destroy(id);"
                , "return {first = first, scrolled = scrolled,"
                , "        applied = applied, dumped = dumped,"
                , "        capacity = 4, maxOffset = 8}"
                ]
            p ← decodeOr r ∷ IO ScrollWindowProbe
            swFirst p `shouldBe` "d1,d2,d3,d4/1,2,3,4"
            swApplied p `shouldBe` 5
            swScrolled p `shouldBe` "d6,d7,d8,d9/6,7,8,9"
            -- The introspection surface a probe locates a scrolled row
            -- through carries the same data index.
            swDumped p `shouldBe` 6

        it "clamps every offset to the rows that exist: negative and \
           \past-the-end both land in range, a list that fits scrolls \
           \nowhere, and shrinking the contents re-clamps a restored \
           \offset instead of leaving a blank window" $ \env → do
            ls ← newBareLuaBackend env
            run ls setupLua
            r ← evalDebug ls $ luaLines
                [ "local il = require('scripts.ui.item_list');"
                , "local pg = UI.newPage('il_scroll_clamp', 'overlay');"
                , "UI.showPage(pg);"
                , "local function mk(n, cap, offset)"
                , "  local items = {};"
                , "  for i = 1, n do items[i] = {defName='d'..i,"
                , "    displayName='D'..i, weight=1} end;"
                , "  local p = baseParams(pg, items); p.maxRows = cap;"
                , "  p.scrollOffset = offset; return il.new(p) end;"
                , "local id = mk(12, 4, 0);"
                , "local low = il.setScrollOffset(id, -7);"
                , "local high = il.setScrollOffset(id, 999);"
                , "local maxOff = il.maxScrollOffset(id);"
                , "local cap = il.rowCapacity(id);"
                , "local byStep = il.scrollBy(id, -3);"
                , "il.destroy(id);"
                , "local fits = mk(3, 10, 0);"
                , "local fitsOffset = il.setScrollOffset(fits, 5);"
                , "local fitsMax = il.maxScrollOffset(fits);"
                , "il.destroy(fits);"
                -- A level restored against SHRUNKEN contents asks for an
                -- offset that no longer exists.
                , "local shrunk = mk(6, 4, 9);"
                , "local shrunkOffset = il.getScrollOffset(shrunk);"
                , "local shrunkRows = #il.getRows(shrunk);"
                , "il.destroy(shrunk);"
                , "return {low = low, high = high, maxOff = maxOff, cap = cap,"
                , "        byStep = byStep, fitsOffset = fitsOffset,"
                , "        fitsMax = fitsMax, shrunkOffset = shrunkOffset,"
                , "        shrunkRows = shrunkRows}"
                ]
            p ← decodeOr r ∷ IO ScrollClampProbe
            scLow p `shouldBe` 0
            scHigh p `shouldBe` 8
            scMaxOff p `shouldBe` 8
            scCap p `shouldBe` 4
            scByStep p `shouldBe` 5
            -- Nothing to scroll: the request is clamped to zero rather
            -- than blanking the list.
            scFitsOffset p `shouldBe` 0
            scFitsMax p `shouldBe` 0
            -- Six rows, four visible: the furthest the list can go is 2,
            -- and it still renders a FULL window.
            scShrunkOffset p `shouldBe` 2
            scShrunkRows p `shouldBe` 4

        it "row SELECTION follows the offset: a right-click on a visible \
           \slot delivers the data row under it, not the slot's index \
           \into the unscrolled list" $ \env → do
            ls ← newBareLuaBackend env
            run ls setupLua
            r ← evalDebug ls $ luaLines
                [ "local il = require('scripts.ui.item_list');"
                , "local pg = UI.newPage('il_scroll_select', 'overlay');"
                , "UI.showPage(pg);"
                , "_G.__seen = nil;"
                , "local items = {};"
                , "for i = 1, 12 do items[i] = {defName='d'..i,"
                , "  displayName='D'..i, instanceId = 100+i, weight=1} end;"
                , "local p = baseParams(pg, items); p.maxRows = 4;"
                , "p.onRowRightClick = function(item)"
                , "  _G.__seen = item.defName .. ':' .. tostring(item.instanceId);"
                , "  return true end;"
                , "local id = il.new(p);"
                , "il.handleCallback('onItemListRightClick', il.getRows(id)[2].hitId);"
                , "local before = _G.__seen;"
                , "il.setScrollOffset(id, 5);"
                , "il.handleCallback('onItemListRightClick', il.getRows(id)[2].hitId);"
                , "local after = _G.__seen;"
                , "il.destroy(id);"
                , "return {before = before, after = after}"
                ]
            p ← decodeOr r ∷ IO ScrollSelectProbe
            ssBefore p `shouldBe` "d2:102"
            ssAfter p `shouldBe` "d7:107"

        it "a tab change is a different list, so the offset does not \
           \survive it: the widget re-clamps against the new filtered \
           \rows rather than opening part-way down them" $ \env → do
            ls ← newBareLuaBackend env
            run ls setupLua
            r ← evalDebug ls $ luaLines
                [ "local il = require('scripts.ui.item_list');"
                , "local pg = UI.newPage('il_scroll_tab', 'overlay');"
                , "UI.showPage(pg);"
                , "local items = {};"
                , "for i = 1, 10 do items[i] = {defName='a'..i,"
                , "  displayName='A'..i, category='Alpha', weight=1} end;"
                , "items[11] = {defName='b1', displayName='B1',"
                , "             category='Beta', weight=1};"
                , "local function build(tab, offset)"
                , "  local p = baseParams(pg, items); p.maxRows = 4;"
                , "  p.tabs = {mode='row'}; p.activeTab = tab;"
                , "  p.scrollOffset = offset; return il.new(p) end;"
                , "local wide = build('All', 6);"
                , "local wideOffset = il.getScrollOffset(wide);"
                , "il.destroy(wide);"
                , "local narrow = build('Beta', 6);"
                , "local narrowOffset = il.getScrollOffset(narrow);"
                , "local narrowRows = #il.getRows(narrow);"
                , "il.destroy(narrow);"
                , "return {wideOffset = wideOffset, narrowOffset = narrowOffset,"
                , "        narrowRows = narrowRows}"
                ]
            p ← decodeOr r ∷ IO ScrollTabProbe
            stWideOffset p `shouldBe` 6
            -- One Beta row: nothing to scroll past, so the requested
            -- offset collapses rather than hiding the only row.
            stNarrowOffset p `shouldBe` 0
            stNarrowRows p `shouldBe` 1

-- * Decoders

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

data IconProbe = IconProbe
    { ipRows ∷ Int, ipMissingIcon ∷ Bool, ipPresentIcon ∷ Bool } deriving Show
instance FromJSON IconProbe where
    parseJSON = withObject "IconProbe" $ \o →
        IconProbe <$> o .: "rows" <*> o .: "missingIcon" <*> o .: "presentIcon"

data ChromeProbe = ChromeProbe { cprBare ∷ Text, cprChrome ∷ Text } deriving Show
instance FromJSON ChromeProbe where
    parseJSON = withObject "ChromeProbe" $ \o →
        ChromeProbe <$> o .: "bare" <*> o .: "chrome"

data ScrollWindowProbe = ScrollWindowProbe
    { swFirst ∷ Text, swScrolled ∷ Text, swApplied ∷ Int
    , swDumped ∷ Int } deriving Show
instance FromJSON ScrollWindowProbe where
    parseJSON = withObject "ScrollWindowProbe" $ \o →
        ScrollWindowProbe <$> o .: "first" <*> o .: "scrolled"
                          <*> o .: "applied" <*> o .: "dumped"

data ScrollClampProbe = ScrollClampProbe
    { scLow ∷ Int, scHigh ∷ Int, scMaxOff ∷ Int, scCap ∷ Int
    , scByStep ∷ Int, scFitsOffset ∷ Int, scFitsMax ∷ Int
    , scShrunkOffset ∷ Int, scShrunkRows ∷ Int } deriving Show
instance FromJSON ScrollClampProbe where
    parseJSON = withObject "ScrollClampProbe" $ \o →
        ScrollClampProbe <$> o .: "low" <*> o .: "high" <*> o .: "maxOff"
                         <*> o .: "cap" <*> o .: "byStep" <*> o .: "fitsOffset"
                         <*> o .: "fitsMax" <*> o .: "shrunkOffset"
                         <*> o .: "shrunkRows"

data ScrollSelectProbe = ScrollSelectProbe
    { ssBefore ∷ Text, ssAfter ∷ Text } deriving Show
instance FromJSON ScrollSelectProbe where
    parseJSON = withObject "ScrollSelectProbe" $ \o →
        ScrollSelectProbe <$> o .: "before" <*> o .: "after"

data ScrollTabProbe = ScrollTabProbe
    { stWideOffset ∷ Int, stNarrowOffset ∷ Int
    , stNarrowRows ∷ Int } deriving Show
instance FromJSON ScrollTabProbe where
    parseJSON = withObject "ScrollTabProbe" $ \o →
        ScrollTabProbe <$> o .: "wideOffset" <*> o .: "narrowOffset"
                       <*> o .: "narrowRows"
