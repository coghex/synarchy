{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}
-- | The "craft bill cycle replenishment" gate (#2524): a craft bill that
--   chains into another cycle re-plans its ingredient sourcing for THAT
--   cycle, instead of re-entering the fetch phase with the previous
--   cycle's already-consumed fetch plan.
--
--   Before this, @scripts/unit_ai_craft.lua@ planned
--   @job.fromGround@ \/ @job.fromMule@ \/ @job.fromCargo@ exactly once,
--   at claim time. The production fetch helpers in
--   @scripts/unit_ai_fetch.lua@ empty those tables IN PLACE as they
--   source (@fetchWantsFromGround@ clears an entry once satisfied or
--   exhausted; @fetchWantsFromMule@ and @fetchWantsFromCargo@ clear
--   every entry after one visit), so a continuing cycle re-entered the
--   fetch phase with all three empty while @job.need@ still held a
--   whole cycle's demand. All three @fetchWants*@ calls returned
--   immediately, the post-fetch inventory reconciliation found the
--   demand unmet, and the worker released a bill it was perfectly able
--   to continue. A later utility scan re-claimed it, so the symptom was
--   an unnecessary release\/reclaim churn rather than a permanent
--   stoppage — which is why "an output eventually appears" is NOT a
--   sufficient assertion here, and every case below counts
--   @craft.releaseBill@ calls and asserts claimant continuity directly.
--
--   Same standalone-Lua-VM pattern as
--   "Test.Headless.Lua.WorkClaimCapacity": each 'it' runs one
--   self-contained chunk via 'Lua.dostring' in a fresh interpreter,
--   asserting inside Lua via @assert()@, with a non-OK 'Lua.Status'
--   surfaced as an hspec failure carrying the Lua message. The real
--   @scripts.unit_ai_craft@ and @scripts.unit_ai_fetch@ modules run;
--   @scripts.movement_speed@ is stubbed at @package.loaded@ (only
--   @comfort@ is reached, and the real one pulls in the whole
--   physiology chain) while @scripts.unit_roles@ and
--   @scripts.unit_ai_page@ are the real modules.
--
--   The engine stubs here go well past WorkClaimCapacity's, because
--   this gate has to observe a cycle BOUNDARY rather than a claim
--   decision: @craft.executeAt@ really consumes the recipe's demands
--   out of the crafter's inventory and hands back fresh output instance
--   ids, @craft.completeBillCycle@ mirrors 'Craft.Bills.completeBillCycle'
--   (progress reset, working cleared, remaining decremented, the bill
--   deleted at 0, the claim kept for a continuing UNPAUSED bill and
--   dropped for a paused one), @craft.getBill@ \/ @craft.getBills@ \/
--   @building.getInfo@ \/ @unit.getInfo@ hand back per-call SNAPSHOTS
--   the way the real @pushBill@ \/ @pushBuildingInfo@ projections do,
--   and every claim, release, working-flag flip, pickup, mule transfer,
--   cargo withdrawal and walk is counted so "no intervening
--   release\/reclaim" and "no inter-cycle sourcing" are directly
--   observable. @unit.moveTo@ arrives immediately, which is what keeps
--   a multi-cycle run deterministic.
--
--   The scenario NUMBERS are not written here: 'shippedFixture' reads
--   every item YAML under @data\/items@ (recursively, #1232) and
--   @data\/recipes\/smelting.yaml@ and renders them into the Lua
--   fixture verbatim, so the recipes and item weights under test are
--   the ones the game ships. The single exception is
--   @smelt_steel_selffuel@, a SYNTHETIC recipe injected into the
--   fixture's recipe table by 'selfFuelledRecipe' because no shipped
--   recipe has a @fuel:@ item equal to one of its own @inputs:@ items
--   (verified across every file under @data\/recipes@); the case that
--   uses it says so at its assertion.
--
--   Run just this gate: @cabal test synarchy-test-headless
--   --test-options='--match "craft bill cycle replenishment"'@.
module Test.Headless.Lua.CraftCycleReplenishment (spec) where

import UPrelude
import Test.Hspec
import qualified HsLua as Lua
import qualified Data.Aeson.Key as K
import qualified Data.Aeson.KeyMap as KM
import Data.Aeson (Value (..))
import qualified Data.Scientific as Sci
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Vector as V
import qualified Data.Yaml as Y
import Data.List (sort)
import System.FilePath ((</>))
import Engine.Asset.Discovery (walkFilesWithExtension)

runsOk ∷ Text → Expectation
runsOk chunkText = do
    result ← Lua.run @Lua.Exception $ do
        Lua.openlibs
        status ← Lua.dostring (TE.encodeUtf8 chunkText)
        case status of
            Lua.OK → return Nothing
            _ → do
                err ← Lua.tostring (-1)
                return (Just (maybe "<no message>" TE.decodeUtf8Lenient err))
    case result of
        Nothing  → pure ()
        Just msg → expectationFailure (T.unpack msg)

lns ∷ [Text] → Text
lns = T.intercalate "\n"

-- * Shipped data → Lua literals

-- | Render an arbitrary decoded YAML document as a Lua value, so
--   @craft.get@ hands the AI the real recipe table rather than a
--   hand-copied subset that could drift from what ships.
luaValue ∷ Value → Text
luaValue v = case v of
    Object o → "{" <> T.intercalate ", "
        [ "[" <> luaString (K.toText k) <> "] = " <> luaValue x
        | (k, x) ← KM.toList o, x /= Null ] <> "}"
    Array a  → "{" <> T.intercalate ", " (map luaValue (V.toList a)) <> "}"
    String s → luaString s
    Number n → luaNumber n
    Bool b   → if b then "true" else "false"
    Null     → "nil"

luaString ∷ Text → Text
luaString s = "\"" <> T.concatMap esc s <> "\""
  where
    esc '"'  = "\\\""
    esc '\\' = "\\\\"
    esc '\n' = "\\n"
    esc '\r' = "\\r"
    esc c    = T.singleton c

luaNumber ∷ Sci.Scientific → Text
luaNumber = T.pack ∘ show ∘ Sci.toRealFloat @Double

num ∷ Double → Text
num = T.pack ∘ show

-- | The shipped data these cases are built from.
data Shipped = Shipped
    { shItemDefs ∷ Value          -- ^ every definition under @data/items@,
                                  --   at any depth (#1232)
    , shRecipes  ∷ Value          -- ^ smelting recipes keyed by id, plus
                                  --   the one synthetic entry
    , shWeight   ∷ Text → Double  -- ^ item def weight, 0 when unknown
    , shDemand   ∷ Text → Double  -- ^ one cycle's inputs + fuel, in kg
    }

objectLookup ∷ Text → Value → Maybe Value
objectLookup k (Object o) = KM.lookup (K.fromText k) o
objectLookup _ _          = Nothing

asArray ∷ Maybe Value → [Value]
asArray (Just (Array a)) = V.toList a
asArray _                = []

asText ∷ Maybe Value → Maybe Text
asText (Just (String s)) = Just s
asText _                 = Nothing

asDouble ∷ Maybe Value → Maybe Double
asDouble (Just (Number n)) = Just (Sci.toRealFloat n)
asDouble _                 = Nothing

-- | The one synthetic recipe this gate needs: a fuel line naming the
--   SAME item def as an input line, which @craftDemands@ (and the
--   engine's 'Craft.Types.recipeDemands') sum into a single key of 2.
--   No shipped recipe does that — @data\/recipes\/smelting.yaml@ pairs
--   ore inputs with distinct coal fuels and @smelt_steel_electric@ has
--   no fuel line at all — so the "fuel is included, including when it
--   shares an item type with an input" case is unreachable from shipped
--   data and is built here instead. Everything else about it is
--   ordinary: shipped item defs, the same station and work as its
--   smelting neighbours.
selfFuelledRecipe ∷ Value
selfFuelledRecipe = Object (KM.fromList
    [ ("id",      String "smelt_steel_selffuel")
    , ("name",    String "Smelt Steel (self-fuelled, SYNTHETIC)")
    , ("station", String "smelt")
    , ("inputs",  Array (V.fromList
        [ Object (KM.fromList [ ("item", String "iron_ore_chunk")
                              , ("count", Number 1) ]) ]))
    , ("fuel",    Object (KM.fromList [ ("item", String "iron_ore_chunk")
                                      , ("count", Number 1) ]))
    , ("work",    Number 30)
    , ("outputs", Array (V.fromList
        [ Object (KM.fromList [ ("item", String "steel_bar")
                              , ("count", Number 1) ]) ]))
    ])

-- | Read the shipped YAML this gate reasons about. Decode failures are
--   raised rather than defaulted: a fixture silently built from an
--   empty item catalogue would weigh every load at 0 kg and make every
--   capacity case vacuous.
shippedFixture ∷ IO Shipped
shippedFixture = do
    itemFiles ← sort ⊚ walkFilesWithExtension "data/items" ".yaml"
    itemDocs ← mapM (Y.decodeFileThrow @IO @Value ∘ ("data/items" </>)) itemFiles
    let defs = concatMap (asArray ∘ objectLookup "items") itemDocs
        weightOf name = case
            [ w | d ← defs
                , asText (objectLookup "name" d) ≡ Just name
                , Just w ← [asDouble (objectLookup "weight" d)] ] of
                (w : _) → w
                []      → 0
    recipeDoc ← Y.decodeFileThrow @IO @Value "data/recipes/smelting.yaml"
    let recipeList = asArray (objectLookup "recipes" recipeDoc)
                   <> [selfFuelledRecipe]
        byId = Object (KM.fromList
            [ (K.fromText rid, r)
            | r ← recipeList, Just rid ← [asText (objectLookup "id" r)] ])
        -- Mirrors craftDemands: inputs plus the single fuel line,
        -- summed by def name so a self-fuelled recipe counts twice.
        demandOf rid = case
            [ r | r ← recipeList
                , asText (objectLookup "id" r) ≡ Just rid ] of
            (r : _) →
                let ins = [ (i, c)
                          | x ← asArray (objectLookup "inputs" r)
                          , Just i ← [asText (objectLookup "item" x)]
                          , let c = maybe 1 id (asDouble (objectLookup "count" x)) ]
                    fuel = case objectLookup "fuel" r of
                        Just f | Just i ← asText (objectLookup "item" f) →
                            [(i, maybe 1 id (asDouble (objectLookup "count" f)))]
                        _ → []
                in sum [ weightOf i * c | (i, c) ← ins <> fuel ]
            [] → 0
    return Shipped
        { shItemDefs = Array (V.fromList defs)
        , shRecipes  = byId
        , shWeight   = weightOf
        , shDemand   = demandOf
        }

-- * The Lua fixture

-- | Every engine stub the two real AI modules reach, plus the scenario
--   helpers and the tick driver.
--
--   Geometry: the worker (unit 1) starts at the origin, the station is
--   building 7 at (20, 0), the technomule is unit 2 at (22, 0) and the
--   cargo store is building 9 at (24, 0) — all well inside
--   @craft_scan_range@ (30) of both the origin AND the station, which
--   matters because a CONTINUING cycle plans from where the previous
--   one finished (beside the station) rather than from the scan
--   position. Ground stock is laid next to the station for the same
--   reason: stock only reachable from the origin would make the
--   two-cycle ground case a false negative instead of a regression
--   signal.
craftPrelude ∷ Shipped → Text
craftPrelude sh = lns
    [ "package.loaded['scripts.unit_ai'] = {}"
    , "package.loaded['scripts.movement_speed'] ="
    , "  { comfort = function() return 1.0 end,"
    , "    ordered = function() return 1.0 end,"
    , "    sprint  = function() return 1.0 end }"
    , "NOW = 0"
    , "WARNS, EVENTS = 0, 0"
    -- Every observable this gate asserts on. MARK is the snapshot
    -- craft.completeBillCycle takes of them, so "between the cycle's
    -- completion and whatever happened next" is a comparison rather
    -- than an inference from tick counts.
    , "CLAIM_CALLS, NEW_CLAIMS, RELEASES = 0, 0, 0"
    , "WORKING_CALLS, WORKING_ON = 0, 0"
    , "PICKUPS, WITHDRAWALS, TRANSFERS, MOVES, DROPS = 0, 0, 0, 0, 0"
    , "EXECUTES, CYCLES, PROGRESS_POURED = 0, 0, 0.0"
    , "XP_GRANTS = 0"
    , "SPAWNED, PRODUCED = 0, 0"
    , "CONSUMED, MARK = {}, nil"
    , "NEXT_IID, NEXT_GID = 0, 0"
    -- #1673: the AI pairs every candidate with the ACTING unit's own
    -- page, so the stub world needs one. A single page throughout --
    -- cross-page rejection is Test.Headless.Lua.UnitAiPageTargets'
    -- subject.
    , "PAGE = 'stub_page'"
    , "UNITS = {"
    , "  [1] = { gridX = 0.0,  gridY = 0.0, page = PAGE, defName = 'acolyte' },"
    , "  [2] = { gridX = 22.0, gridY = 0.0, page = PAGE,"
    , "          defName = 'technomule' } }"
    , "INVS = { [1] = {}, [2] = {} }"
    , "CARRIED = { [1] = 0.0, [2] = 0.0 }"
    , "CAPACITY = math.huge"
    , "GROUND = {}"
    , "STATIONS = {"
    , "  [7] = { gridX = 20, gridY = 0, tileW = 1, tileH = 1, page = PAGE },"
    , "  [9] = { gridX = 24, gridY = 0, tileW = 1, tileH = 1, page = PAGE } }"
    , "CARGO = { [7] = {}, [9] = {} }"
    , "BILLS = {}"
    , "ITEM_DEFS = " <> luaValue (shItemDefs sh)
    , "RECIPES = " <> luaValue (shRecipes sh)
    -- ---- small shared helpers -------------------------------------
    , "function wOf(defName)"
    , "  for _, d in ipairs(ITEM_DEFS) do"
    , "    if d.name == defName then return d.weight or 0 end end"
    , "  return 0"
    , "end"
    , "-- Mirrors unit_ai_craft's craftDemands / Craft.Types.recipeDemands:"
    , "-- inputs plus the single fuel line, summed by def name."
    , "function demandsOf(r)"
    , "  local d = {}"
    , "  for _, i in ipairs(r.inputs or {}) do"
    , "    d[i.item] = (d[i.item] or 0) + (i.count or 1) end"
    , "  if r.fuel then"
    , "    d[r.fuel.item] = (d[r.fuel.item] or 0) + (r.fuel.count or 1) end"
    , "  return d"
    , "end"
    , "function copy(t)"
    , "  if t == nil then return nil end"
    , "  local n = {}"
    , "  for k, v in pairs(t) do n[k] = v end"
    , "  return n"
    , "end"
    , "function invCount(u, defName)"
    , "  local n = 0"
    , "  for _, it in ipairs(INVS[u] or {}) do"
    , "    if it.defName == defName then n = n + 1 end end"
    , "  return n"
    , "end"
    , "local function invPut(u, it)"
    , "  local inv = INVS[u]"
    , "  inv[#inv + 1] = it"
    , "  CARRIED[u] = (CARRIED[u] or 0) + wOf(it.defName)"
    , "end"
    , "-- Remove ONE instance of defName from u, returning it (instance"
    , "-- identity preserved, so ownership can be traced end to end)."
    , "local function invTake(u, defName)"
    , "  for i, it in ipairs(INVS[u] or {}) do"
    , "    if it.defName == defName then"
    , "      table.remove(INVS[u], i)"
    , "      CARRIED[u] = CARRIED[u] - wOf(defName)"
    , "      return it"
    , "    end"
    , "  end"
    , "end"
    , "local function newInstance(defName)"
    , "  NEXT_IID = NEXT_IID + 1"
    , "  return { defName = defName, iid = NEXT_IID }"
    , "end"
    -- ---- engine stubs ---------------------------------------------
    , "local function bumpWarn() WARNS = WARNS + 1 end"
    , "engine = { gameTime = function() return NOW end,"
    , "           logWarn = bumpWarn, logError = bumpWarn,"
    , "           logInfo = function() end, logDebug = function() end,"
    , "           emitEvent = function() EVENTS = EVENTS + 1 end,"
    , "           emitEventAt = function() EVENTS = EVENTS + 1 end,"
    , "           emitEventForUnit = function() EVENTS = EVENTS + 1 end }"
    , "world = { getActiveWorldId = function() return 1 end }"
    , "debug = debug or {}"
    , "debug.recordOutcome = function() end"
    , "unit = {"
    , "  exists = function(u) return UNITS[u] ~= nil end,"
    , "  getAllIds = function() return { 1, 2 } end,"
    -- The real projection builds a fresh table per call, so the AI's
    -- `info` local is the position as of the tick it was read in --
    -- which is exactly what pins a continuation's ground reach to the
    -- station the worker is standing beside.
    , "  getInfo = function(u) return copy(UNITS[u]) end,"
    , "  getInventory = function(u) return INVS[u] end,"
    , "  getCarryingWeight = function(u) return CARRIED[u] or 0 end,"
    , "  getStat = function(_, k)"
    , "    if k == 'carrying_capacity' then return CAPACITY end"
    , "    return 1.0 end,"
    , "  getSkill = function() return 25.0 end,"
    , "  setSkill = function() end,"
    , "  addXP = function() XP_GRANTS = XP_GRANTS + 1 end,"
    , "  getKnowledge = function() return true end,"
    , "  getMentalEffectiveness = function() return 1.0 end,"
    , "  stop = function() end,"
    , "  moveTo = function(u, x, y)"
    , "    MOVES = MOVES + 1"
    , "    UNITS[u].gridX, UNITS[u].gridY = x, y"
    , "  end,"
    , "  transferItemToUnit = function(fromU, toU, defName)"
    , "    local it = invTake(fromU, defName)"
    , "    if not it then return false end"
    , "    invPut(toU, it)"
    , "    TRANSFERS = TRANSFERS + 1"
    , "    return true"
    , "  end,"
    , "  withdrawFromCargo = function(u, bid, defName)"
    , "    for i, it in ipairs(CARGO[bid] or {}) do"
    , "      if it.defName == defName then"
    , "        table.remove(CARGO[bid], i)"
    , "        invPut(u, it)"
    , "        WITHDRAWALS = WITHDRAWALS + 1"
    , "        return true"
    , "      end"
    , "    end"
    , "    return false"
    , "  end,"
    , "  dropItemById = function(u, iid)"
    , "    for i, it in ipairs(INVS[u] or {}) do"
    , "      if it.iid == iid then"
    , "        table.remove(INVS[u], i)"
    , "        CARRIED[u] = CARRIED[u] - wOf(it.defName)"
    , "        NEXT_GID = NEXT_GID + 1"
    , "        GROUND[#GROUND + 1] = { id = NEXT_GID, iid = iid,"
    , "          defName = it.defName, x = UNITS[u].gridX,"
    , "          y = UNITS[u].gridY, weight = wOf(it.defName) }"
    , "        DROPS = DROPS + 1"
    , "        return true"
    , "      end"
    , "    end"
    , "    return false"
    , "  end,"
    , "  removeItem = function() return true end }"
    , "item = {"
    , "  listDefs = function() return ITEM_DEFS end,"
    -- #1666: the pickup order reads the CARRIER'S OWN page. This
    -- fixture has exactly one page, so the owning-page lookup answers
    -- from the same GROUND table listGround does.
    , "  getGroundForUnit = function(_, gid)"
    , "    for _, g in ipairs(GROUND) do"
    , "      if g.id == gid then return g, true end end"
    , "    return nil, true end,"
    , "  listGround = function() return GROUND end,"
    , "  spawnGround = function() end,"
    , "  pickupGround = function(u, gid)"
    , "    for i, g in ipairs(GROUND) do"
    , "      if g.id == gid then"
    , "        table.remove(GROUND, i)"
    , "        invPut(u, { defName = g.defName, iid = g.iid })"
    , "        PICKUPS = PICKUPS + 1"
    , "        return true"
    , "      end"
    , "    end"
    , "    return false"
    , "  end }"
    , "building = {"
    , "  getActiveIds = function() return { 7, 9 } end,"
    , "  getActivity = function() return 'built' end,"
    , "  getStorage = function(bid) return CARGO[bid] or {} end,"
    , "  getInfo = function(bid) return copy(STATIONS[bid]) end }"
    , "power = { isStationPoweredForRecipe = function() return true end }"
    , "local function findBill(id)"
    , "  for i, b in ipairs(BILLS) do"
    , "    if b.id == id then return b, i end end"
    , "end"
    -- #2325: the bill lifecycle verbs take the ACTING UNIT first, so
    -- these stubs must too -- a 1-arg getBill stub would answer nil for
    -- every production call and this fixture would never reach a cycle
    -- boundary at all.
    , "craft = {"
    , "  getBills = function()"
    , "    local out = {}"
    , "    for i, b in ipairs(BILLS) do out[i] = copy(b) end"
    , "    return out"
    , "  end,"
    , "  getBill = function(_u, id) return copy(findBill(id)) end,"
    , "  get = function(rid) return RECIPES[rid] end,"
    -- Mirrors Craft.Bills.claimAvailable: a paused bill refuses every
    -- claimant but its current holder, a fresh foreign claim wins, and
    -- a claim by the holder is a refresh.
    , "  claimBill = function(id, u, timeout)"
    , "    local b = findBill(id)"
    , "    if not b then return false end"
    , "    if b.paused and b.claimant ~= u then return false end"
    , "    if b.claimant and b.claimant ~= u"
    , "       and (NOW - (b.claimedAt or 0)) <= timeout then return false end"
    , "    if b.claimant ~= u then NEW_CLAIMS = NEW_CLAIMS + 1 end"
    , "    b.claimant, b.claimedAt = u, NOW"
    , "    CLAIM_CALLS = CLAIM_CALLS + 1"
    , "    return true"
    , "  end,"
    , "  releaseBill = function(_u, id)"
    , "    RELEASES = RELEASES + 1"
    , "    local b = findBill(id)"
    , "    if b then b.claimant, b.claimedAt, b.working = nil, nil, false end"
    , "  end,"
    , "  setBillWorking = function(_u, id, flag)"
    , "    WORKING_CALLS = WORKING_CALLS + 1"
    , "    if flag then WORKING_ON = WORKING_ON + 1 end"
    , "    local b = findBill(id)"
    , "    if b then b.working = flag and true or false end"
    , "  end,"
    , "  addBillProgress = function(_u, id, delta)"
    , "    local b = findBill(id)"
    , "    if not b then return nil end"
    , "    b.progress = math.max(0, math.min(1, (b.progress or 0) + delta))"
    , "    PROGRESS_POURED = PROGRESS_POURED + delta"
    , "    return b.progress"
    , "  end,"
    -- The real verb is the single authority for consumption: it checks
    -- the recipe's whole demand, removes those instances from the
    -- crafter, and returns the FRESH output instance ids (which land in
    -- inventory for the AI to drop at the station).
    , "  executeAt = function(u, rid, _bid, _billId)"
    , "    local r = RECIPES[rid]"
    , "    if not r then return false, 'unknown recipe' end"
    , "    local need = demandsOf(r)"
    , "    for defName, count in pairs(need) do"
    , "      if invCount(u, defName) < count then"
    , "        return false, 'missing ' .. defName"
    , "      end"
    , "    end"
    , "    for defName, count in pairs(need) do"
    , "      for _ = 1, count do"
    , "        CONSUMED[#CONSUMED + 1] = invTake(u, defName)"
    , "      end"
    , "    end"
    , "    local out = {}"
    , "    for _, o in ipairs(r.outputs or {}) do"
    , "      for _ = 1, (o.count or 1) do"
    , "        local inst = newInstance(o.item)"
    , "        invPut(u, inst)"
    , "        PRODUCED = PRODUCED + 1"
    , "        out[#out + 1] = inst.iid"
    , "      end"
    , "    end"
    , "    EXECUTES = EXECUTES + 1"
    , "    return true, out"
    , "  end,"
    -- Mirrors Craft.Bills.completeBillCycle exactly: progress reset and
    -- cbWorking cleared either way, a finite bill at 1 deleted and 0
    -- returned, a repeat bill left at -1, and the claim retained only
    -- for a continuing UNPAUSED bill (a paused one loses cbClaimant
    -- here, which is the enforced half of #796's pause boundary).
    , "  completeBillCycle = function(_u, id)"
    , "    local b, idx = findBill(id)"
    , "    if not b then return nil end"
    , "    CYCLES = CYCLES + 1"
    , "    MARK = { pickups = PICKUPS, withdrawals = WITHDRAWALS,"
    , "             transfers = TRANSFERS, moves = MOVES,"
    , "             workingOn = WORKING_ON, releases = RELEASES,"
    , "             poured = PROGRESS_POURED, claims = NEW_CLAIMS }"
    , "    b.progress, b.working = 0, false"
    , "    if b.paused then b.claimant, b.claimedAt = nil, nil end"
    , "    if b.remaining < 0 then return -1 end"
    , "    if b.remaining <= 1 then table.remove(BILLS, idx); return 0 end"
    , "    b.remaining = b.remaining - 1"
    , "    return b.remaining"
    , "  end }"
    -- ---- scenario helpers -----------------------------------------
    , "-- A standing bill on station 7. mode defaults to the fixed-count"
    , "-- spelling Craft.Bills projects ('fixed' | 'repeat' | 'until')."
    , "function bill(id, recipeId, remaining, opts)"
    , "  opts = opts or {}"
    , "  local b = { id = id, seq = id, recipe = recipeId, station = 7,"
    , "              mode = opts.mode or 'fixed', progress = 0,"
    , "              remaining = remaining, paused = opts.paused or false,"
    , "              working = false }"
    , "  if b.mode == 'until' then"
    , "    b.target, b.outputItem = opts.target, opts.outputItem"
    , "  end"
    , "  BILLS[#BILLS + 1] = b"
    , "  return b"
    , "end"
    , "function liveBill(id) return findBill(id) end"
    , "-- Lay `count` instances of `defName` on the ground at (x, y)."
    , "function ground(defName, count, x, y)"
    , "  for _ = 1, count do"
    , "    local inst = newInstance(defName)"
    , "    NEXT_GID = NEXT_GID + 1"
    , "    GROUND[#GROUND + 1] = { id = NEXT_GID, iid = inst.iid,"
    , "      defName = defName, x = x, y = y, weight = wOf(defName) }"
    , "    SPAWNED = SPAWNED + 1"
    , "  end"
    , "end"
    , "-- Put `count` instances straight into a unit's inventory."
    , "function hold(u, defName, count)"
    , "  for _ = 1, count do"
    , "    invPut(u, newInstance(defName))"
    , "    SPAWNED = SPAWNED + 1"
    , "  end"
    , "end"
    , "-- Stock `count` instances in a cargo store."
    , "function stock(bid, defName, count)"
    , "  for _ = 1, count do"
    , "    local store = CARGO[bid]"
    , "    store[#store + 1] = newInstance(defName)"
    , "    SPAWNED = SPAWNED + 1"
    , "  end"
    , "end"
    , "function groundCount(defName)"
    , "  local n = 0"
    , "  for _, g in ipairs(GROUND) do"
    , "    if g.defName == defName then n = n + 1 end end"
    , "  return n"
    , "end"
    , "function cargoCount(bid, defName)"
    , "  local n = 0"
    , "  for _, it in ipairs(CARGO[bid] or {}) do"
    , "    if it.defName == defName then n = n + 1 end end"
    , "  return n"
    , "end"
    , "-- Requirement 4's bookkeeping: every instance ever spawned or"
    , "-- produced is still somewhere (ground, an inventory, a store) or"
    , "-- was consumed by a craft, each exactly once. A planner that"
    , "-- fabricated, duplicated or prematurely consumed anything breaks"
    , "-- one of these two."
    , "function conserved()"
    , "  local live = #GROUND"
    , "  for _, inv in pairs(INVS) do live = live + #inv end"
    , "  for _, st in pairs(CARGO) do live = live + #st end"
    , "  return live + #CONSUMED == SPAWNED + PRODUCED"
    , "end"
    , "function distinctInstances()"
    , "  local seen = {}"
    , "  local function visit(it)"
    , "    if it.iid == nil then return false end"
    , "    if seen[it.iid] then return false end"
    , "    seen[it.iid] = true"
    , "    return true"
    , "  end"
    , "  for _, g in ipairs(GROUND) do if not visit(g) then return false end end"
    , "  for _, inv in pairs(INVS) do"
    , "    for _, it in ipairs(inv) do if not visit(it) then return false end end"
    , "  end"
    , "  for _, st in pairs(CARGO) do"
    , "    for _, it in ipairs(st) do if not visit(it) then return false end end"
    , "  end"
    , "  for _, it in ipairs(CONSUMED) do"
    , "    if not visit(it) then return false end end"
    , "  return true"
    , "end"
    -- ---- the driver -----------------------------------------------
    , "local craftAi = require('scripts.unit_ai_craft')"
    , "-- The shipped craft tunables (scripts/unit_ai_tunables.lua)."
    , "PARAMS = { craft_scan_range = 30.0, craft_base_utility = 3.2,"
    , "           craft_lock_utility = 6.0, craft_rate = 1.0,"
    , "           craft_claim_timeout = 30.0, craft_xp_per_craft = 1.5,"
    , "           pickup_arrival_tiles = 1.2, mule_fetch_arrival = 1.5 }"
    , "S = {}"
    , "-- One decision tick: score, then execute only if the action"
    , "-- actually won, exactly as the arbitration loop does. The clock"
    , "-- advances one game second, well inside the shared"
    , "-- MAX_CHARGED_INTERVAL bound, so each working tick charges 1 s."
    , "function tick()"
    , "  local u = craftAi.craftUtility(1, S, PARAMS)"
    , "  if u > -math.huge then craftAi.craftExecute(1, S, PARAMS) end"
    , "  NOW = NOW + 1"
    , "end"
    , "function ticks(n) for _ = 1, n do tick() end end"
    , "-- Tick until `pred` holds, failing with `msg` if it never does."
    , "-- A recipe's work (30) over craft_rate 1.0 at skill 25 pours"
    , "-- 0.025 per second, so one cycle needs ~40 working ticks plus"
    , "-- the fetch and walk around it; 400 is generous headroom for"
    , "-- three cycles and is never the assertion itself."
    , "function tickUntil(pred, msg)"
    , "  for _ = 1, 400 do"
    , "    if pred() then return end"
    , "    tick()"
    , "  end"
    , "  error(msg .. ' (never happened in 400 ticks)', 2)"
    , "end"
    , "function cyclesDone(n) return function() return CYCLES >= n end end"
    ]

spec ∷ Spec
spec = beforeAll shippedFixture $
    describe "craft bill cycle replenishment" $ do

    describe "a continuing cycle re-plans its sourcing" $ do
        it "runs both cycles of a two-cycle ground-fed bill under ONE \
           \claim, with no intervening release and a freshly planned \
           \ground fetch for the second cycle" $ \sh → do
            runsOk $ lns
                [ craftPrelude sh
                -- Beside the station, so the reach is the same whether
                -- it is measured from the scan position or from where
                -- cycle 1 finished.
                , "ground('iron_ore_chunk', 2, 21.0, 0.0)"
                , "ground('anthracite_chunk', 2, 21.0, 0.0)"
                , "bill(1, 'smelt_steel_anthracite', 2)"
                , "tickUntil(cyclesDone(1), 'the first cycle must complete')"
                -- The direct gate on the fix: the very tick that
                -- finished cycle 1 must have left a NON-EMPTY plan
                -- behind. Before #2524 all three tables were empty here
                -- and the next tick released the bill.
                , "assert(S.craftJob, 'the job must survive the cycle boundary')"
                , "assert(S.craftJob.phase == 'fetch',"
                , "  'and re-enter the fetch phase: '"
                , "  .. tostring(S.craftJob.phase))"
                , "assert(next(S.craftJob.fromGround) ~= nil,"
                , "  'with the next cycle sourced from the ground again')"
                , "assert(RELEASES == 0,"
                , "  'no release may happen at the boundary: ' .. RELEASES)"
                , "tickUntil(cyclesDone(2), 'the second cycle must complete')"
                , "assert(RELEASES == 0,"
                , "  'and none across the whole run: ' .. RELEASES)"
                , "assert(NEW_CLAIMS == 1,"
                , "  'exactly one claim, never a reclaim: ' .. NEW_CLAIMS)"
                , "assert(EXECUTES == 2, 'both crafts ran: ' .. EXECUTES)"
                , "assert(groundCount('iron_ore_chunk') == 0"
                , "       and groundCount('anthracite_chunk') == 0,"
                , "  'every input was actually consumed, none left behind')"
                , "assert(groundCount('steel_bar') == 8,"
                , "  'both cycles laid their outputs at the station: '"
                , "  .. groundCount('steel_bar'))"
                , "assert(liveBill(1) == nil, 'and the finite bill is gone')"
                , "assert(conserved() and distinctInstances(),"
                , "  'with no fabricated or duplicated instances')"
                ]

        it "continues a two-cycle bill fed only from a technomule, \
           \whose fetch table the mule helper clears after one visit" $ \sh → do
            runsOk $ lns
                [ craftPrelude sh
                , "hold(2, 'iron_ore_chunk', 2)"
                , "hold(2, 'anthracite_chunk', 2)"
                , "bill(1, 'smelt_steel_anthracite', 2)"
                , "tickUntil(cyclesDone(1), 'the first cycle must complete')"
                , "assert(S.craftJob and S.craftJob.phase == 'fetch',"
                , "  'the job must survive into a second fetch phase')"
                , "assert(next(S.craftJob.fromMule) ~= nil,"
                , "  'planning the mule again for cycle two')"
                , "tickUntil(cyclesDone(2), 'the second cycle must complete')"
                , "assert(RELEASES == 0, 'under one claim: ' .. RELEASES)"
                , "assert(NEW_CLAIMS == 1, 'never reclaimed: ' .. NEW_CLAIMS)"
                , "assert(EXECUTES == 2, 'both crafts ran: ' .. EXECUTES)"
                , "assert(invCount(2, 'iron_ore_chunk') == 0,"
                , "  'the mule was drained across both cycles')"
                , "assert(TRANSFERS == 4,"
                , "  'two items per cycle came off the mule: ' .. TRANSFERS)"
                , "assert(conserved() and distinctInstances(),"
                , "  'with no fabricated or duplicated instances')"
                ]

        it "continues a two-cycle bill fed only from cargo storage, \
           \whose fetch table the cargo helper also clears per visit" $ \sh → do
            runsOk $ lns
                [ craftPrelude sh
                , "stock(9, 'iron_ore_chunk', 2)"
                , "stock(9, 'anthracite_chunk', 2)"
                , "bill(1, 'smelt_steel_anthracite', 2)"
                , "tickUntil(cyclesDone(1), 'the first cycle must complete')"
                , "assert(S.craftJob and S.craftJob.phase == 'fetch',"
                , "  'the job must survive into a second fetch phase')"
                , "assert(next(S.craftJob.fromCargo) ~= nil,"
                , "  'planning the store again for cycle two')"
                , "tickUntil(cyclesDone(2), 'the second cycle must complete')"
                , "assert(RELEASES == 0, 'under one claim: ' .. RELEASES)"
                , "assert(NEW_CLAIMS == 1, 'never reclaimed: ' .. NEW_CLAIMS)"
                , "assert(EXECUTES == 2, 'both crafts ran: ' .. EXECUTES)"
                , "assert(cargoCount(9, 'iron_ore_chunk') == 0"
                , "       and cargoCount(9, 'anthracite_chunk') == 0,"
                , "  'the store was drained across both cycles')"
                , "assert(WITHDRAWALS == 4,"
                , "  'two withdrawals per cycle: ' .. WITHDRAWALS)"
                , "assert(conserved() and distinctInstances(),"
                , "  'with no fabricated or duplicated instances')"
                ]

        it "honours a SOURCE CHANGE between cycles: ground stock good \
           \for one cycle only, then cargo covers the second" $ \sh → do
            runsOk $ lns
                [ craftPrelude sh
                , "ground('iron_ore_chunk', 1, 21.0, 0.0)"
                , "ground('anthracite_chunk', 1, 21.0, 0.0)"
                , "stock(9, 'iron_ore_chunk', 1)"
                , "stock(9, 'anthracite_chunk', 1)"
                , "bill(1, 'smelt_steel_anthracite', 2)"
                , "tickUntil(cyclesDone(1), 'the first cycle must complete')"
                -- The ground rung is exhausted now, so the re-plan has
                -- to fall through to the store. A planner that reused
                -- the claim-time ground plan, or one that only re-tried
                -- the previous cycle's rung, stalls here.
                , "assert(groundCount('iron_ore_chunk') == 0,"
                , "  'cycle one ate the ground stock')"
                , "assert(S.craftJob and next(S.craftJob.fromCargo) ~= nil,"
                , "  'so the second cycle must be planned against cargo')"
                , "assert(next(S.craftJob.fromGround) == nil,"
                , "  'and not against ground stock that no longer exists')"
                , "tickUntil(cyclesDone(2), 'the second cycle must complete')"
                , "assert(RELEASES == 0, 'under one claim: ' .. RELEASES)"
                , "assert(EXECUTES == 2, 'both crafts ran: ' .. EXECUTES)"
                , "assert(WITHDRAWALS == 2,"
                , "  'the store supplied exactly cycle two: ' .. WITHDRAWALS)"
                , "assert(conserved() and distinctInstances(),"
                , "  'with no fabricated or duplicated instances')"
                ]

        it "counts what is already CARRIED: inputs left over from the \
           \first fetch are not fetched again for the second cycle" $ \sh → do
            runsOk $ lns
                [ craftPrelude sh
                -- Both cycles' ore is carried from the start and NO
                -- ore exists anywhere else, so the second cycle's ore
                -- demand is covered by inventory alone. A re-plan that
                -- ignored inventory would put the whole ore demand back
                -- on a rung (the cargo rung takes any remainder), find
                -- nothing there, and release the bill.
                , "hold(1, 'iron_ore_chunk', 2)"
                , "ground('anthracite_chunk', 2, 21.0, 0.0)"
                , "bill(1, 'smelt_steel_anthracite', 2)"
                , "tickUntil(cyclesDone(1), 'the first cycle must complete')"
                , "assert(invCount(1, 'iron_ore_chunk') == 1,"
                , "  'one ore is still carried after cycle one: '"
                , "  .. invCount(1, 'iron_ore_chunk'))"
                , "assert(S.craftJob, 'the job must survive the boundary')"
                , "assert(S.craftJob.fromGround['iron_ore_chunk'] == nil"
                , "       and S.craftJob.fromMule['iron_ore_chunk'] == nil"
                , "       and S.craftJob.fromCargo['iron_ore_chunk'] == nil,"
                , "  'the carried ore must not be fetched a second time')"
                , "assert(S.craftJob.fromGround['anthracite_chunk'] == 1,"
                , "  'only the missing fuel is sourced: '"
                , "  .. tostring(S.craftJob.fromGround['anthracite_chunk']))"
                , "tickUntil(cyclesDone(2), 'the second cycle must complete')"
                , "assert(RELEASES == 0, 'under one claim: ' .. RELEASES)"
                , "assert(EXECUTES == 2, 'both crafts ran: ' .. EXECUTES)"
                , "assert(groundCount('iron_ore_chunk') == 0"
                , "       and cargoCount(9, 'iron_ore_chunk') == 0"
                , "       and invCount(2, 'iron_ore_chunk') == 0,"
                , "  'the ore was only ever in inventory, so crediting '"
                , "  .. 'it is the only way this can succeed')"
                , "assert(PICKUPS == 2,"
                , "  'two pickups -- one fuel per cycle, no ore: ' .. PICKUPS)"
                , "assert(conserved() and distinctInstances(),"
                , "  'with no fabricated or duplicated instances')"
                ]

        it "includes FUEL in the re-plan even when the fuel line names \
           \the same item def as an input (a synthetic recipe: no \
           \shipped recipe does this)" $ \sh → do
            -- craftDemands sums inputs and fuel into ONE key, so this
            -- recipe demands 2 iron_ore_chunk per cycle. A continuation
            -- planner that re-derived demands from `recipe.inputs`
            -- alone would plan 1 and stall on the reconciliation; one
            -- that re-derived from the recipe at all rather than from
            -- the job's own `need` would be visible here first.
            runsOk $ lns
                [ craftPrelude sh
                , "assert(RECIPES['smelt_steel_selffuel'], 'synthetic recipe present')"
                , "ground('iron_ore_chunk', 4, 21.0, 0.0)"
                , "bill(1, 'smelt_steel_selffuel', 2)"
                , "tickUntil(cyclesDone(1), 'the first cycle must complete')"
                , "assert(S.craftJob, 'the job must survive the boundary')"
                , "assert(S.craftJob.need['iron_ore_chunk'] == 2,"
                , "  'input + fuel sum to one demand of 2: '"
                , "  .. tostring(S.craftJob.need['iron_ore_chunk']))"
                , "assert(S.craftJob.fromGround['iron_ore_chunk'] == 2,"
                , "  'and the whole 2 is re-planned, fuel included: '"
                , "  .. tostring(S.craftJob.fromGround['iron_ore_chunk']))"
                , "tickUntil(cyclesDone(2), 'the second cycle must complete')"
                , "assert(RELEASES == 0, 'under one claim: ' .. RELEASES)"
                , "assert(EXECUTES == 2, 'both crafts ran: ' .. EXECUTES)"
                , "assert(groundCount('iron_ore_chunk') == 0,"
                , "  'all four chunks were consumed: '"
                , "  .. groundCount('iron_ore_chunk'))"
                , "assert(conserved() and distinctInstances(),"
                , "  'with no fabricated or duplicated instances')"
                ]

    describe "the bill-mode boundaries are unchanged" $ do
        it "a repeat-forever bill keeps chaining cycles on one claim" $ \sh → do
            runsOk $ lns
                [ craftPrelude sh
                , "ground('iron_ore_chunk', 3, 21.0, 0.0)"
                , "ground('anthracite_chunk', 3, 21.0, 0.0)"
                , "bill(1, 'smelt_steel_anthracite', -1, { mode = 'repeat' })"
                , "tickUntil(cyclesDone(3), 'three cycles must complete')"
                , "assert(RELEASES == 0, 'under one claim: ' .. RELEASES)"
                , "assert(NEW_CLAIMS == 1, 'never reclaimed: ' .. NEW_CLAIMS)"
                , "assert(liveBill(1).remaining == -1,"
                , "  'a repeat bill stays at -1: '"
                , "  .. tostring(liveBill(1).remaining))"
                , "assert(EXECUTES == 3, 'three crafts ran: ' .. EXECUTES)"
                , "assert(conserved() and distinctInstances(),"
                , "  'with no fabricated or duplicated instances')"
                ]

        it "an until-stock bill still stops at its stock boundary \
           \instead of chaining another cycle" $ \sh → do
            runsOk $ lns
                [ craftPrelude sh
                -- Target 4 and one cycle yields 4 bars, so the FIRST
                -- completed cycle satisfies it (#795): the continuation
                -- branch must not fire even though remaining is 2.
                , "ground('iron_ore_chunk', 2, 21.0, 0.0)"
                , "ground('anthracite_chunk', 2, 21.0, 0.0)"
                , "bill(1, 'smelt_steel_anthracite', 2,"
                , "     { mode = 'until', target = 4,"
                , "       outputItem = 'steel_bar' })"
                , "tickUntil(cyclesDone(1), 'the first cycle must complete')"
                , "assert(S.craftJob == nil,"
                , "  'the satisfied until-stock claim must be dropped')"
                , "assert(RELEASES == 1,"
                , "  'handed back to pending exactly once: ' .. RELEASES)"
                , "ticks(60)"
                , "assert(EXECUTES == 1,"
                , "  'and no second cycle runs: ' .. EXECUTES)"
                , "assert(groundCount('iron_ore_chunk') == 1,"
                , "  'one cycle of inputs is left untouched on the ground')"
                , "assert(conserved() and distinctInstances(),"
                , "  'with no fabricated or duplicated instances')"
                ]

        it "a bill paused DURING work finishes only the permitted cycle \
           \and releases BEFORE any inter-cycle sourcing" $ \sh → do
            runsOk $ lns
                [ craftPrelude sh
                , "ground('iron_ore_chunk', 2, 21.0, 0.0)"
                , "ground('anthracite_chunk', 2, 21.0, 0.0)"
                , "bill(1, 'smelt_steel_anthracite', 2)"
                -- Reach the working phase, then pause mid-cycle. The
                -- #796 abort guard (`paused and not working`) is past,
                -- so this cycle is the permitted one.
                , "tickUntil(function()"
                , "    return S.craftJob and S.craftJob.phase == 'working'"
                , "  end, 'the worker must reach the working phase')"
                , "liveBill(1).paused = true"
                , "tickUntil(cyclesDone(1), 'the in-flight cycle must finish')"
                , "assert(EXECUTES == 1, 'the permitted cycle ran: ' .. EXECUTES)"
                -- The amendment's assertion: the fix makes the
                -- continuation path reachable where the reconciliation
                -- used to self-terminate, so the pause contract now
                -- rests on the branch that skips it. NOTHING may be
                -- sourced, walked or marked working after the boundary.
                , "assert(S.craftJob == nil,"
                , "  'the job must be dropped at the boundary, not continued')"
                , "assert(MARK, 'the cycle boundary was observed')"
                , "assert(PICKUPS == MARK.pickups"
                , "       and WITHDRAWALS == MARK.withdrawals"
                , "       and TRANSFERS == MARK.transfers,"
                , "  'nothing may be sourced after the permitted cycle')"
                , "assert(MOVES == MARK.moves,"
                , "  'nor may the worker walk anywhere for a next cycle')"
                , "assert(WORKING_ON == MARK.workingOn,"
                , "  'nor may the bill be marked actively worked again')"
                , "assert(liveBill(1).claimant == nil,"
                , "  'the paused bill goes idle, unclaimed')"
                , "assert(liveBill(1).remaining == 1,"
                , "  'with exactly one cycle spent: '"
                , "  .. tostring(liveBill(1).remaining))"
                , "ticks(60)"
                , "assert(EXECUTES == 1,"
                , "  'and a paused bill is never re-claimed: ' .. EXECUTES)"
                , "assert(groundCount('iron_ore_chunk') == 1,"
                , "  'the second cycle\\'s inputs stay on the ground')"
                , "assert(conserved() and distinctInstances(),"
                , "  'with no fabricated or duplicated instances')"
                ]

    describe "a continuation that cannot be supplied still hands the \
             \bill back" $ do
        it "releases the bill and clears the job when no source can \
           \cover the next cycle, without producing a second output" $ \sh → do
            runsOk $ lns
                [ craftPrelude sh
                -- Exactly one cycle's inputs exist anywhere.
                , "ground('iron_ore_chunk', 1, 21.0, 0.0)"
                , "ground('anthracite_chunk', 1, 21.0, 0.0)"
                , "bill(1, 'smelt_steel_anthracite', 2)"
                , "tickUntil(cyclesDone(1), 'the first cycle must complete')"
                , "assert(S.craftJob, 'the continuation is attempted')"
                -- The outcome this pins: re-planning must not turn an
                -- unnecessary release/reclaim into a worker sitting on
                -- an unfulfillable claim forever. The post-fetch
                -- reconciliation is still the thing that gives up.
                , "tickUntil(function() return S.craftJob == nil end,"
                , "  'the unsuppliable continuation must give up')"
                , "assert(RELEASES >= 1,"
                , "  'handing the bill back to pending: ' .. RELEASES)"
                , "assert(S.craftJob == nil, 'and clearing the local job')"
                , "assert(liveBill(1).claimant == nil,"
                , "  'so the bill is claimable by someone else')"
                , "assert(liveBill(1).remaining == 1,"
                , "  'with the unspent cycle intact: '"
                , "  .. tostring(liveBill(1).remaining))"
                , "assert(EXECUTES == 1,"
                , "  'and no second output was produced: ' .. EXECUTES)"
                , "assert(groundCount('steel_bar') == 4,"
                , "  'exactly one cycle of bars exists: '"
                , "  .. groundCount('steel_bar'))"
                , "assert(conserved() and distinctInstances(),"
                , "  'with no fabricated or duplicated instances')"
                ]

        it "obeys the capacity policy on the continuation: a worker \
           \loaded past a full cycle's headroom between cycles fetches \
           \nothing it cannot carry, produces no second output, and is \
           \released" $ \sh → do
            let cycleKg = shDemand sh "smelt_steel_anthracite"
                oreKg   = shWeight sh "iron_ore_chunk"
                fuelKg  = shWeight sh "anthracite_chunk"
                barKg   = shWeight sh "steel_bar"
                -- Room for a whole cycle at claim time, so cycle one is
                -- feasible and really runs.
                cap = cycleKg + oreKg / 2
                -- Then dead weight arrives mid-job (a transfer order,
                -- a loot pickup) leaving headroom that fits the fuel
                -- but NOT the ore, which is what makes the per-pickup
                -- capacity gates refuse the continuation's plan.
                -- Derived, and guarded below, so a weight change in
                -- data/items fails here rather than going vacuous.
                headroom = (oreKg + fuelKg) / 2
                deadBars = floor ((cap - headroom) / barKg) ∷ Int
                left = cap - fromIntegral deadBars * barKg
            (cycleKg > 0 && barKg > 0) `shouldBe` True
            (cycleKg ≤ cap) `shouldBe` True
            (fuelKg ≤ left && left < oreKg) `shouldBe` True
            runsOk $ lns
                [ craftPrelude sh
                , "CAPACITY = " <> num cap
                , "ground('iron_ore_chunk', 2, 21.0, 0.0)"
                , "ground('anthracite_chunk', 2, 21.0, 0.0)"
                , "bill(1, 'smelt_steel_anthracite', 2)"
                , "tickUntil(cyclesDone(1), 'the first cycle must complete')"
                , "assert(S.craftJob, 'the continuation is attempted')"
                , "hold(1, 'steel_bar', " <> T.pack (show deadBars) <> ")"
                , "assert(CAPACITY - CARRIED[1] < " <> num oreKg <> ","
                , "  'the ore can no longer fit: '"
                , "  .. tostring(CAPACITY - CARRIED[1]))"
                , "tickUntil(function() return S.craftJob == nil end,"
                , "  'the over-capacity continuation must give up')"
                , "assert(EXECUTES == 1,"
                , "  'only the feasible cycle produced: ' .. EXECUTES)"
                , "assert(RELEASES >= 1, 'the bill was handed back: ' .. RELEASES)"
                , "assert(liveBill(1).claimant == nil,"
                , "  'so a stronger worker can take it')"
                , "assert(invCount(1, 'iron_ore_chunk') == 0,"
                , "  'and no ore was carried past the gate: '"
                , "  .. invCount(1, 'iron_ore_chunk'))"
                , "assert(CARRIED[1] <= CAPACITY + 1e-6,"
                , "  'the capacity policy was never exceeded: '"
                , "  .. tostring(CARRIED[1]))"
                , "assert(conserved() and distinctInstances(),"
                , "  'with no fabricated or duplicated instances')"
                ]

    describe "inter-cycle fetching is not crafting work" $ do
        it "pours no progress and leaves the active-recipe working flag \
           \off for the whole gap between two cycles" $ \sh → do
            runsOk $ lns
                [ craftPrelude sh
                -- Stock the second cycle far enough away that the gap
                -- spans several ticks of walking and fetching, so a
                -- leaked progress pour or a stale working flag has room
                -- to show up.
                , "ground('iron_ore_chunk', 1, 21.0, 0.0)"
                , "ground('anthracite_chunk', 1, 21.0, 0.0)"
                , "stock(9, 'iron_ore_chunk', 1)"
                , "stock(9, 'anthracite_chunk', 1)"
                , "bill(1, 'smelt_steel_anthracite', 2)"
                , "tickUntil(cyclesDone(1), 'the first cycle must complete')"
                , "local pouredAt = MARK.poured"
                , "assert(liveBill(1).working == false,"
                , "  'completing a cycle clears the working flag')"
                -- Walk the whole gap one tick at a time, asserting the
                -- two requirement-6 observables on EVERY tick of it
                -- rather than only at the far end.
                , "local gapTicks = 0"
                , "while S.craftJob and S.craftJob.phase ~= 'working' do"
                , "  tick()"
                , "  gapTicks = gapTicks + 1"
                , "  if S.craftJob and S.craftJob.phase ~= 'working' then"
                , "    assert(PROGRESS_POURED == pouredAt,"
                , "      'no progress may be poured while fetching: '"
                , "      .. PROGRESS_POURED .. ' vs ' .. pouredAt)"
                , "    assert(liveBill(1).working == false,"
                , "      'nor may the recipe draw power while fetching')"
                , "  end"
                , "  assert(gapTicks < 100, 'the gap must end')"
                , "end"
                , "assert(gapTicks > 1,"
                , "  'the gap must really span fetching and walking: '"
                , "  .. gapTicks)"
                , "assert(WORKING_ON == MARK.workingOn + 1,"
                , "  'the flag goes back on exactly once, at the station: '"
                , "  .. WORKING_ON)"
                , "assert(RELEASES == 0, 'all under one claim: ' .. RELEASES)"
                , "tickUntil(cyclesDone(2), 'the second cycle must complete')"
                , "assert(EXECUTES == 2, 'both crafts ran: ' .. EXECUTES)"
                , "assert(conserved() and distinctInstances(),"
                , "  'with no fabricated or duplicated instances')"
                ]
