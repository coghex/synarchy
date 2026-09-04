-- | The Item list widget's REBUILD-INVALIDATION contract (#1269;
--   split out under #2147): which normalized inputs recreate nothing,
--   which presentation inputs force a rebuild, and the row-field
--   coverage that decides between them.
--
--   Composed by 'Test.Headless.UI.ItemList', which owns the
--   @Item list widget@ describe and the per-case queue drain. This
--   module adds no describe of its own, so every example path is the
--   one the facade already published.
module Test.Headless.UI.ItemList.Invalidation (spec) where

import UPrelude
import Test.Hspec
import qualified Data.Text as T
import Data.Aeson (FromJSON(..), withObject, (.:))
import Engine.Core.State (EngineEnv)
import Test.Headless.UI.ItemList.Support

spec ∷ SpecWith EngineEnv
spec = do

    -- * Rebuild invalidation

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

        -- #1269: the signature must cover every value SUPPLIED IN THE
        -- ROW that the widget or one of its host callbacks can put on
        -- screen -- not just the ones the widget renders itself. The
        -- tooltip is baked into the hit element at rebuild time, so a
        -- not-stale verdict keeps yesterday's tooltip text on screen.
        -- Every mutation below is driven with the presentation key
        -- held CONSTANT and a rowTooltip callback installed, which is
        -- exactly the configuration the pre-#1269 signature accepted.
        it "a change to any callback-consumed row field goes stale under a constant presentation key" $ \env → do
            ls ← newBareLuaBackend env
            run ls setupLua
            r ← evalDebug ls $ luaLines
                [ "local il = require('scripts.ui.item_list');"
                , "local pg = UI.newPage('il_rowfields', 'overlay');"
                -- Rebuilt from scratch every call, exactly as the
                -- engine reconstructs `weapon` and `buffs` on every
                -- inventory read -- so nothing here can pass by
                -- comparing table identities.
                , "local function baseItems() return {{"
                , "  defName='blade', displayName='Blade', weight=1,"
                , "  instanceId=7, condition=100, currentFill=1,"
                , "  make='forged', material='iron', capacity=2.5,"
                , "  weapon = {bladeLength=30, baseSharpness=4,"
                , "            stabEffectiveness=0.9, slashEffectiveness=0.8,"
                , "            bluntEffectiveness=0.2},"
                , "  buffs = {{stat='perception', amount=1, percent=0.1,"
                , "            scalesWithCondition=false},"
                , "           {stat='strength', amount=2, percent=0.0,"
                , "            scalesWithCondition=true}},"
                , "}} end;"
                , "local function params(items)"
                , "  local p = baseParams(pg, items);"
                , "  p.presentationKey = 'constant';"
                , "  p.rowTooltip = function(row) return {text='t', hint='h'} end;"
                , "  return p"
                , "end;"
                , "local id = il.new(params(baseItems()));"
                , "local muts = {"
                , "  {'make', function(r) r.make = 'cast' end},"
                , "  {'material', function(r) r.material = 'steel' end},"
                , "  {'capacity', function(r) r.capacity = 3.0 end},"
                , "  {'weapon.bladeLength', function(r) r.weapon.bladeLength = 31 end},"
                , "  {'weapon.baseSharpness', function(r) r.weapon.baseSharpness = 5 end},"
                , "  {'weapon.stabEffectiveness', function(r) r.weapon.stabEffectiveness = 0.95 end},"
                , "  {'weapon.slashEffectiveness', function(r) r.weapon.slashEffectiveness = 0.85 end},"
                , "  {'weapon.bluntEffectiveness', function(r) r.weapon.bluntEffectiveness = 0.25 end},"
                , "  {'weapon.absent', function(r) r.weapon = nil end},"
                , "  {'buffs.length', function(r) r.buffs[2] = nil end},"
                , "  {'buffs.order', function(r)"
                , "     r.buffs[1], r.buffs[2] = r.buffs[2], r.buffs[1] end},"
                , "  {'buffs.stat', function(r) r.buffs[1].stat = 'agility' end},"
                , "  {'buffs.amount', function(r) r.buffs[1].amount = 5 end},"
                , "  {'buffs.percent', function(r) r.buffs[1].percent = 0.25 end},"
                , "  {'buffs.scalesWithCondition', function(r)"
                , "     r.buffs[1].scalesWithCondition = true end},"
                , "  {'buffs.absent', function(r) r.buffs = nil end},"
                , "};"
                , "local covered, missed = {}, {};"
                , "for _, m in ipairs(muts) do"
                , "  local items = baseItems(); m[2](items[1]);"
                , "  if il.isStale(id, params(items)) then covered[#covered+1] = m[1]"
                , "  else missed[#missed+1] = m[1] end"
                , "end;"
                -- The other half of the contract: value-equivalent but
                -- FRESHLY ALLOCATED nested tables must still compare
                -- not-stale, or production rebuilds on every poll.
                , "local h1 = il.getRows(id)[1].hitId;"
                , "local fresh = il.isStale(id, params(baseItems()));"
                , "local h2 = il.getRows(id)[1].hitId;"
                , "il.destroy(id);"
                , "return {covered = table.concat(covered, ','),"
                , "        missed = table.concat(missed, ','),"
                , "        fresh = fresh, handleKept = (h1 == h2)}"
                ]
            p ← decodeOr r ∷ IO RowFieldProbe
            rfMissed p `shouldBe` ""
            rfCovered p `shouldBe` T.intercalate ","
                [ "make", "material", "capacity"
                , "weapon.bladeLength", "weapon.baseSharpness"
                , "weapon.stabEffectiveness", "weapon.slashEffectiveness"
                , "weapon.bluntEffectiveness", "weapon.absent"
                , "buffs.length", "buffs.order", "buffs.stat", "buffs.amount"
                , "buffs.percent", "buffs.scalesWithCondition", "buffs.absent"
                ]
            rfFresh p `shouldBe` False
            rfHandleKept p `shouldBe` True

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

-- * Decoders

data StaleProbe = StaleProbe
    { stSame ∷ Bool, stChanged ∷ Bool, stByKey ∷ Bool, stByTooltip ∷ Bool
    , stByHeader ∷ Bool, stByFooter ∷ Bool, stBySelection ∷ Bool
    , stForced ∷ Bool } deriving Show
instance FromJSON StaleProbe where
    parseJSON = withObject "StaleProbe" $ \o →
        StaleProbe <$> o .: "same" <*> o .: "changed" <*> o .: "byKey"
                   <*> o .: "byTooltip" <*> o .: "byHeader" <*> o .: "byFooter"
                   <*> o .: "bySelection" <*> o .: "forced"

data RowFieldProbe = RowFieldProbe
    { rfCovered ∷ Text, rfMissed ∷ Text, rfFresh ∷ Bool
    , rfHandleKept ∷ Bool } deriving Show
instance FromJSON RowFieldProbe where
    parseJSON = withObject "RowFieldProbe" $ \o →
        RowFieldProbe <$> o .: "covered" <*> o .: "missed" <*> o .: "fresh"
                      <*> o .: "handleKept"

data NoChurnProbe = NoChurnProbe { ncStale ∷ Bool, ncSame ∷ Bool } deriving Show
instance FromJSON NoChurnProbe where
    parseJSON = withObject "NoChurnProbe" $ \o →
        NoChurnProbe <$> o .: "stale" <*> o .: "same"

data CutoffProbe = CutoffProbe
    { coAt210 ∷ Bool, coAt250 ∷ Bool, coUnchanged ∷ Bool } deriving Show
instance FromJSON CutoffProbe where
    parseJSON = withObject "CutoffProbe" $ \o →
        CutoffProbe <$> o .: "at210" <*> o .: "at250" <*> o .: "unchanged"
