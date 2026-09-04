{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}
-- | The "work claim capacity feasibility" gate (#1326): a craft bill or
--   a structure-construction designation whose still-to-fetch input load
--   cannot fit in the scanning worker is not a claim candidate for THAT
--   worker.
--
--   Before this, the pre-claim filters
--   (@scripts/unit_ai_craft.lua@'s @craftMaterialsAvailable@ and
--   @scripts/unit_ai_construct.lua@'s @constructMaterialsAvailable@)
--   summed available COUNTS only. Every capacity gate lives further down
--   — inside @fetchWantsFromGround@ \/ @fetchWantsFromMule@ \/
--   @fetchWantsFromCargo@, each refusing one pickup at a time — so a
--   worker claimed a job it could never carry, fetched until a gate
--   stopped it, failed the post-fetch inventory reconciliation, released
--   the job still holding the partial load, and re-claimed the same job
--   on the very next decision tick. There is no cooldown or backoff
--   anywhere in that loop.
--
--   Same standalone-Lua-VM pattern as "Test.Headless.Lua.UnitAiStall":
--   each 'it' runs one self-contained chunk via 'Lua.dostring' in a
--   fresh interpreter, asserting inside Lua via @assert()@, with a
--   non-OK 'Lua.Status' surfaced as an hspec failure carrying the Lua
--   message. The engine API the two real AI modules call is stubbed;
--   @scripts.movement_speed@ is stubbed at @package.loaded@ (only
--   @comfort@ is reached, and the real one pulls in the whole
--   physiology chain), while @scripts.unit_roles@ is the real module
--   (a role-less state weighs 1.0).
--
--   The scenario NUMBERS are not written here: 'shippedFixture' reads
--   every item YAML under @data\/items@ (recursively, #1232),
--   @data\/recipes\/smelting.yaml@ and
--   @data\/structure_packs\/dungeon_1.yaml@ and renders them into the
--   Lua fixture verbatim, so the recipes, pack costs and item weights
--   under test are the ones the game ships. The capacities are DERIVED
--   from those weights (and guarded, so a data change that made a case
--   vacuous fails here rather than passing silently) instead of being
--   hard-coded against today's values.
--
--   Run just this gate: @cabal test synarchy-test-headless
--   --test-options='--match "work claim capacity feasibility"'@.
module Test.Headless.Lua.WorkClaimCapacity (spec) where

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

-- | Render an arbitrary decoded YAML document as a Lua value. Keeps the
--   fixture honest: @craft.get@ hands the AI the real recipe table and
--   @engine.loadYaml@ the real structure pack, rather than a hand-copied
--   subset that could drift from what ships.
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

-- | The pieces of shipped data these cases are built from.
data Shipped = Shipped
    { shItemDefs ∷ Value            -- ^ every definition under @data/items@,
                                    --   at any depth (#1232)
    , shRecipes  ∷ Value            -- ^ smelting recipes keyed by id
    , shPack     ∷ Value            -- ^ the whole @dungeon_1@ pack
    , shWeight   ∷ Text → Double    -- ^ item def weight, 0 when unknown
    , shDemand   ∷ Text → Double    -- ^ one cycle's inputs + fuel, in kg
    , shBuildCost ∷ Text → Double   -- ^ a pack piece kind's materials, kg
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

-- | Read the shipped YAML this gate reasons about. Decode failures are
--   raised rather than defaulted: a fixture silently built from an empty
--   item catalogue would weigh every job at 0 kg and pass every case.
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
    packDoc ← Y.decodeFileThrow @IO @Value "data/structure_packs/dungeon_1.yaml"
    let recipeList = asArray (objectLookup "recipes" recipeDoc)
        byId = Object (KM.fromList
            [ (K.fromText rid, r)
            | r ← recipeList, Just rid ← [asText (objectLookup "id" r)] ])
        -- Mirrors craftDemands: inputs plus the single fuel line.
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
        buildCostOf kind = case
            objectLookup "build" packDoc ≫= objectLookup kind
                          ≫= objectLookup "materials" of
            Just (Object mats) → sum
                [ weightOf (K.toText k) * maybe 1 id (asDouble (Just c))
                | (k, c) ← KM.toList mats ]
            _ → 0
    return Shipped
        { shItemDefs = Array (V.fromList defs)
        , shRecipes  = byId
        , shPack     = packDoc
        , shWeight   = weightOf
        , shDemand   = demandOf
        , shBuildCost = buildCostOf
        }

num ∷ Double → Text
num = T.pack ∘ show

-- * Lua fixtures

-- | Engine-API stubs both AI modules share. Inventory, ground stock,
--   carried weight and capacity are plain globals each case fills in;
--   @item.pickupGround@ really moves an instance from the ground into
--   inventory and charges its weight, so the fetch phase behaves.
--
--   @WARNS@\/@EVENTS@ back requirement 7: the filter is silent, so a
--   rejected candidate must not reach @engine.logWarn@ or any event
--   verb on any tick.
commonPrelude ∷ Shipped → Text
commonPrelude sh = lns
    [ "package.loaded['scripts.unit_ai'] = {}"
    , "package.loaded['scripts.movement_speed'] ="
    , "  { comfort = function() return 1.0 end,"
    , "    ordered = function() return 1.0 end,"
    , "    sprint  = function() return 1.0 end }"
    , "NOW, WARNS, EVENTS = 0, 0, 0"
    , "INV, GROUND = {}, {}"
    , "CARRIED, CAPACITY = 0, math.huge"
    -- #1673: the AI pairs every candidate with the ACTING unit's own
    -- page, so the stub world needs one. A single page throughout —
    -- cross-page rejection is Test.Headless.Lua.UnitAiPageTargets'
    -- subject, and this gate is still only about the capacity gate.
    , "PAGE = 'stub_page'"
    , "POS = { gridX = 0, gridY = 0, page = PAGE }"
    , "ITEM_DEFS = " <> luaValue (shItemDefs sh)
    , "RECIPES = " <> luaValue (shRecipes sh)
    , "PACK = " <> luaValue (shPack sh)
    , "local function bump() WARNS = WARNS + 1 end"
    , "engine = { gameTime = function() return NOW end,"
    , "           logWarn = bump, logError = bump,"
    , "           logInfo = function() end, logDebug = function() end,"
    , "           loadYaml = function() return PACK end,"
    , "           emitEvent = function() EVENTS = EVENTS + 1 end,"
    , "           emitEventAt = function() EVENTS = EVENTS + 1 end,"
    , "           emitEventForUnit = function() EVENTS = EVENTS + 1 end }"
    , "world = { getActiveWorldId = function() return 1 end }"
    , "debug = debug or {}"
    , "debug.recordOutcome = function() end"
    , "unit = {"
    , "  exists = function() return true end,"
    , "  getAllIds = function() return {} end,"
    , "  getInfo = function() return POS end,"
    , "  getInventory = function() return INV end,"
    , "  getCarryingWeight = function() return CARRIED end,"
    , "  getStat = function(_, k)"
    , "    if k == 'carrying_capacity' then return CAPACITY end"
    , "    return 1.0 end,"
    , "  getSkill = function() return 25.0 end,"
    , "  getKnowledge = function() return true end,"
    , "  getMentalEffectiveness = function() return 1.0 end,"
    , "  moveTo = function() end, stop = function() end,"
    , "  dropItemById = function() end,"
    , "  removeItem = function() return true end }"
    , "item = {"
    , "  listDefs = function() return ITEM_DEFS end,"
    , "-- #1666: the pickup order reads the CARRIER'S OWN page. This"
    , "-- fixture has exactly one page, so the owning-page lookup is"
    , "-- the same GROUND table listGround answers from — resolved"
    , "-- (second return) is always true, which is what lets a missing"
    , "-- id still mean 'gone' here."
    , "  getGroundForUnit = function(_, gid)"
    , "    for _, g in ipairs(GROUND) do"
    , "      if g.id == gid then return g, true end end"
    , "    return nil, true end,"
    , "  listGround = function() return GROUND end,"
    , "  spawnGround = function() end,"
    , "  pickupGround = function(_, id)"
    , "    for i, g in ipairs(GROUND) do"
    , "      if g.id == id then"
    , "        table.remove(GROUND, i)"
    , "        INV[#INV + 1] = { defName = g.defName }"
    , "        CARRIED = CARRIED + (g.weight or 0)"
    , "        return true"
    , "      end"
    , "    end"
    , "    return false"
    , "  end }"
    , "-- Lay `count` instances of `defName` on the unit's own tile."
    , "function ground(defName, count)"
    , "  local w = 0"
    , "  for _, d in ipairs(ITEM_DEFS) do"
    , "    if d.name == defName then w = d.weight or 0 end"
    , "  end"
    , "  for _ = 1, count do"
    , "    GROUND[#GROUND + 1] = { id = #GROUND + 1, defName = defName,"
    , "                            x = POS.gridX, y = POS.gridY, weight = w }"
    , "  end"
    , "end"
    , "-- Put `count` instances straight into inventory, charging weight."
    , "function hold(defName, count)"
    , "  local w = 0"
    , "  for _, d in ipairs(ITEM_DEFS) do"
    , "    if d.name == defName then w = d.weight or 0 end"
    , "  end"
    , "  for _ = 1, count do"
    , "    INV[#INV + 1] = { defName = defName }"
    , "    CARRIED = CARRIED + w"
    , "  end"
    , "end"
    , "function snapshotInv()"
    , "  local n = {}"
    , "  for _, it in ipairs(INV) do n[it.defName] = (n[it.defName] or 0) + 1 end"
    , "  return n"
    , "end"
    , "function sameInv(a, b)"
    , "  for k, v in pairs(a) do if b[k] ~= v then return false end end"
    , "  for k, v in pairs(b) do if a[k] ~= v then return false end end"
    , "  return true"
    , "end"
    ]

-- | The craft half: @craft.*@ stubs plus a one-station bill list, and
--   @tick()@ = one decision tick (score, then execute only if the action
--   actually won, exactly as the arbitration loop does).
craftPrelude ∷ Shipped → Text
craftPrelude sh = lns
    [ commonPrelude sh
    , "BILLS = {}"
    , "STATIONS = { [7] = { gridX = 20, gridY = 0, tileW = 1, tileH = 1,"
    , "                      page = PAGE } }"
    , "CLAIMS = {}"
    , "building = {"
    , "  getActiveIds = function() return {} end,"
    , "  getActivity = function() return 'built' end,"
    , "  getStorage = function() return {} end,"
    , "  getInfo = function(bid) return STATIONS[bid] end }"
    , "power = { isStationPoweredForRecipe = function() return true end }"
    -- #2325: the bill lifecycle verbs take the ACTING UNIT first, so
    -- these stubs must too — a 1-arg getBill stub would answer nil for
    -- every production call and this fixture would stop reaching the
    -- claim path it exists to count.
    , "craft = {"
    , "  getBills = function() return BILLS end,"
    , "  getBill = function(_uid, id)"
    , "    for _, b in ipairs(BILLS) do if b.id == id then return b end end"
    , "  end,"
    , "  get = function(rid) return RECIPES[rid] end,"
    , "  claimBill = function(id) CLAIMS[#CLAIMS + 1] = id; return true end,"
    , "  releaseBill = function() end,"
    , "  setBillWorking = function() end,"
    , "  addBillProgress = function() return 0 end,"
    , "  completeBillCycle = function() return 0 end,"
    , "  executeAt = function() return true, {} end }"
    , "-- A standing fixed-count bill on station 7 (mode ~= 'until', so"
    , "-- untilStockSatisfied never gates it)."
    , "function bill(id, recipeId)"
    , "  BILLS[#BILLS + 1] = { id = id, recipe = recipeId, station = 7,"
    , "                        mode = 'count', progress = 0 }"
    , "end"
    , "local craftAi = require('scripts.unit_ai_craft')"
    , "PARAMS = { craft_scan_range = 30.0, craft_base_utility = 3.2,"
    , "           craft_lock_utility = 6.0, craft_rate = 1.0,"
    , "           craft_claim_timeout = 30.0, craft_xp_per_craft = 1.5,"
    , "           pickup_arrival_tiles = 1.2, mule_fetch_arrival = 1.5 }"
    , "S = {}"
    , "function tick()"
    , "  local u = craftAi.craftUtility(1, S, PARAMS)"
    , "  if u > -math.huge then craftAi.craftExecute(1, S, PARAMS) end"
    , "  NOW = NOW + 1"
    , "end"
    ]

-- | The construction half: @construction.*@ \/ @structure.*@ stubs over
--   a pending-designation list, with the engine-side status the AI
--   flips recorded in @STATUS@ so \"the job stays pending\" is observed
--   the way the durable layer would show it.
constructPrelude ∷ Shipped → Text
constructPrelude sh = lns
    [ commonPrelude sh
    , "JOBS, STATUS, DESIGN = {}, {}, {}"
    , "local function key(x, y) return x .. ',' .. y end"
    , "construction = {"
    , "  getPendingJobs = function() return JOBS end,"
    , "  setJobStatus = function(_, x, y, st) STATUS[key(x, y)] = st end,"
    , "  getDesignationAt = function(_, x, y) return DESIGN[key(x, y)] end,"
    , "  addJobProgress = function() end,"
    , "  setMaterialsPaid = function() end,"
    , "  cancelDesignation = function() end }"
    , "structure = { floorZAt = function() return 0 end,"
    , "              hasAt = function() return false end }"
    , "building = { spawn = function() return 1 end,"
    , "             getInfo = function() return nil end }"
    , "-- A pending structure designation from the dungeon_1 pack."
    , "function designate(x, y, kind, paid)"
    , "  JOBS[#JOBS + 1] = { x = x, y = y, lx = x, ly = y, status = 'pending',"
    , "                      category = 'structure', pack = 'dungeon_1',"
    , "                      kind = kind, edge = 'ne', paid = paid or false }"
    , "  STATUS[key(x, y)] = 'pending'"
    , "  DESIGN[key(x, y)] = { x = x, y = y }"
    , "end"
    , "function statusAt(x, y) return STATUS[key(x, y)] end"
    , "local constructAi = require('scripts.unit_ai_construct')"
    , "PARAMS = { construct_scan_range = 30.0, construct_scan_chunks = 2,"
    , "           construct_arrival_tiles = 1.5, construct_base_utility = 3.5,"
    , "           construct_lock_utility = 6.0, construct_rate = 1.0,"
    , "           construct_claim_timeout = 30.0, construct_xp_per_piece = 1.0,"
    , "           pickup_arrival_tiles = 1.2, mule_fetch_arrival = 1.5 }"
    , "S = {}"
    , "function tick()"
    , "  local u = constructAi.constructUtility(1, S, PARAMS)"
    , "  if u > -math.huge then constructAi.constructExecute(1, S, PARAMS) end"
    , "  NOW = NOW + 1"
    , "end"
    ]

spec ∷ Spec
spec = beforeAll shippedFixture $
    describe "work claim capacity feasibility" $ do

    describe "a craft bill heavier than the worker is never claimed" $ do
        it "leaves smelt_bronze_bituminous pending across repeated \
           \decision ticks even though every input is on the ground, \
           \and the worker's inventory never changes" $ \sh → do
            let total = shDemand sh "smelt_bronze_bituminous"
                ores  = shWeight sh "copper_ore_chunk"
                      + shWeight sh "tin_ore_chunk"
                -- Room for both ores, not for the fuel on top: the
                -- rejection has to come from the AGGREGATE, so a gate
                -- that only weighed the first input would pass here.
                cap = ores + (total - ores) / 2
            (ores < cap && cap < total) `shouldBe` True
            runsOk $ lns
                [ craftPrelude sh
                , "CAPACITY = " <> num cap
                , "ground('copper_ore_chunk', 1)"
                , "ground('tin_ore_chunk', 1)"
                , "ground('bituminous_coal_chunk', 2)"
                , "bill(1, 'smelt_bronze_bituminous')"
                , "local before = snapshotInv()"
                , "for _ = 1, 12 do tick() end"
                , "assert(#CLAIMS == 0,"
                , "  'the bill must never be claimed: ' .. #CLAIMS .. ' claims')"
                , "assert(S.craftJob == nil, 'and no job may be held')"
                , "assert(S.craftCandidate == nil,"
                , "  'nor may it be left scored as a candidate')"
                , "assert(sameInv(before, snapshotInv()),"
                , "  'the worker must not have fetched anything')"
                , "assert(CARRIED == 0, 'nor be carrying a partial load')"
                , "assert(#GROUND == 4, 'and the materials stay on the ground')"
                -- Requirement 7: a scan-time filter, run every tick.
                , "assert(WARNS == 0 and EVENTS == 0,"
                , "  'rejection must be silent: ' .. WARNS .. '/' .. EVENTS)"
                ]

        it "is per WORKER — a strong enough worker claims the same bill \
           \and gets past the fetch phase" $ \sh → do
            let total = shDemand sh "smelt_bronze_bituminous"
            runsOk $ lns
                [ craftPrelude sh
                , "CAPACITY = " <> num (total + 1)
                , "ground('copper_ore_chunk', 1)"
                , "ground('tin_ore_chunk', 1)"
                , "ground('bituminous_coal_chunk', 2)"
                , "bill(1, 'smelt_bronze_bituminous')"
                , "for _ = 1, 12 do tick() end"
                , "assert(#CLAIMS > 0, 'the bill must be claimed')"
                , "assert(S.craftJob, 'and the job held')"
                , "assert(S.craftJob.phase == 'walking',"
                , "  'reaching the walking phase, not stuck fetching: '"
                , "  .. tostring(S.craftJob.phase))"
                , "assert(#GROUND == 0, 'with every input actually fetched')"
                , "assert(CARRIED == " <> num total <> ","
                , "  'carrying the whole load: ' .. tostring(CARRIED))"
                ]

        it "counts only the SHORTFALL: a worker already holding one \
           \bronze input, and landing exactly on capacity once the rest \
           \is fetched, still claims it" $ \sh → do
            let total = shDemand sh "smelt_bronze_bituminous"
                held  = shWeight sh "copper_ore_chunk"
            (held > 0 && held < total) `shouldBe` True
            runsOk $ lns
                [ craftPrelude sh
                -- Exactly enough for held + shortfall and not a gram
                -- more. Double-charging the held input, or comparing
                -- with a strict `>`, would reject this — and the gates
                -- that really refuse a pickup use `>`, so a load landing
                -- ON capacity does fit.
                , "CAPACITY = " <> num total
                , "hold('copper_ore_chunk', 1)"
                , "assert(CARRIED == " <> num held <> ", 'staged carrying the input')"
                , "ground('tin_ore_chunk', 1)"
                , "ground('bituminous_coal_chunk', 2)"
                , "bill(1, 'smelt_bronze_bituminous')"
                , "for _ = 1, 12 do tick() end"
                , "assert(#CLAIMS > 0,"
                , "  'a load landing exactly on capacity must be claimable')"
                , "assert(S.craftJob and S.craftJob.phase == 'walking',"
                , "  'and must get past the fetch phase')"
                , "assert(CARRIED == " <> num total <> ","
                , "  'ending exactly at capacity: ' .. tostring(CARRIED))"
                ]

        it "is per CANDIDATE, not per action: the same scan skips the \
           \heavy bill and claims a lighter one at the same station" $ \sh → do
            let heavy = shDemand sh "smelt_bronze_bituminous"
                light = shDemand sh "smelt_steel_electric"
                cap   = (heavy + light) / 2
            (light < cap && cap < heavy) `shouldBe` True
            runsOk $ lns
                [ craftPrelude sh
                , "CAPACITY = " <> num cap
                , "ground('copper_ore_chunk', 1)"
                , "ground('tin_ore_chunk', 1)"
                , "ground('bituminous_coal_chunk', 2)"
                , "ground('iron_ore_chunk', 1)"
                -- The heavy bill is listed LAST and sits at the same
                -- station, so on the `d <= bestD` tie it is what an
                -- ungated scan would pick.
                , "bill(1, 'smelt_steel_electric')"
                , "bill(2, 'smelt_bronze_bituminous')"
                , "for _ = 1, 12 do tick() end"
                , "assert(S.craftJob, 'a bill must still be claimed')"
                , "assert(S.craftJob.recipeId == 'smelt_steel_electric',"
                , "  'the LIGHT bill must win the scan: '"
                , "  .. tostring(S.craftJob.recipeId))"
                , "assert(CLAIMS[1] == 1, 'and the heavy one is never claimed')"
                ]

    describe "a structure designation heavier than the worker is never \
             \claimed" $ do
        it "leaves an 8 kg wood_log post pending for an equipped worker \
           \with less headroom than that, with the log still on the \
           \ground" $ \sh → do
            let post = shBuildCost sh "post"
                wall = shBuildCost sh "wall"
                -- An already-loaded worker: room for the wall's bars,
                -- not for the post's log.
                carried = 19.0
                cap     = carried + wall
            (wall < post) `shouldBe` True
            runsOk $ lns
                [ constructPrelude sh
                , "hold('steel_plate', 0)"
                , "CARRIED = " <> num carried
                , "CAPACITY = " <> num cap
                , "ground('wood_log', 1)"
                , "designate(3, 0, 'post')"
                , "for _ = 1, 12 do tick() end"
                , "assert(S.constructJob == nil, 'no job may be held')"
                , "assert(S.constructCandidate == nil,"
                , "  'nor may it be left scored as a candidate')"
                , "assert(statusAt(3, 0) == 'pending',"
                , "  'the designation must stay pending: '"
                , "  .. tostring(statusAt(3, 0)))"
                , "assert(#GROUND == 1, 'and the log stays on the ground')"
                , "assert(CARRIED == " <> num carried <> ","
                , "  'the worker fetched nothing')"
                , "assert(WARNS == 0 and EVENTS == 0, 'rejection is silent')"
                ]

        it "is per candidate here too: the same worker skips the post \
           \and claims a lighter two-bar wall in the same scan" $ \sh → do
            let post    = shBuildCost sh "post"
                wall    = shBuildCost sh "wall"
                carried = 19.0
                cap     = carried + wall
            (wall < post) `shouldBe` True
            runsOk $ lns
                [ constructPrelude sh
                , "CARRIED = " <> num carried
                , "CAPACITY = " <> num cap
                , "ground('wood_log', 1)"
                , "ground('steel_bar', 2)"
                -- Same tie-break as the craft case: the post is listed
                -- last and equidistant, so an ungated scan picks it.
                , "designate(3, 0, 'wall')"
                , "designate(0, 3, 'post')"
                , "for _ = 1, 12 do tick() end"
                , "assert(S.constructJob, 'a designation must still be claimed')"
                , "assert(S.constructJob.kind == 'wall',"
                , "  'the LIGHT designation must win: '"
                , "  .. tostring(S.constructJob.kind))"
                , "assert(statusAt(0, 3) == 'pending',"
                , "  'and the post stays pending')"
                ]

        it "never rejects a durably PAID job: its materials already left \
           \inventory, so there is nothing left to carry" $ \sh → do
            let post    = shBuildCost sh "post"
                carried = 19.0
                cap     = carried + post / 2
            (cap < carried + post) `shouldBe` True
            runsOk $ lns
                [ constructPrelude sh
                , "CARRIED = " <> num carried
                , "CAPACITY = " <> num cap
                -- Nothing on the ground at all: a paid job needs no
                -- source, and must not be gated on one either.
                , "designate(3, 0, 'post', true)"
                , "tick()"
                , "assert(S.constructJob, 'a paid job must still be claimable')"
                , "assert(S.constructJob.kind == 'post',"
                , "  'and it is the post: ' .. tostring(S.constructJob.kind))"
                , "assert(statusAt(3, 0) == 'claimed',"
                , "  'flipped to claimed engine-side')"
                , "assert(next(S.constructJob.need) == nil,"
                , "  'with nothing planned to fetch')"
                ]

    describe "the existing late gates are untouched" $ do
        it "a job that becomes infeasible AFTER the claim still bails \
           \the old way — the post-fetch reconciliation releases it and \
           \the tile goes back to pending" $ \sh → do
            let post = shBuildCost sh "post"
            runsOk $ lns
                [ constructPrelude sh
                , "CAPACITY = " <> num (post + 10)
                , "ground('wood_log', 1)"
                , "designate(3, 0, 'post')"
                , "tick()"
                , "assert(S.constructJob, 'the feasible job is claimed')"
                , "assert(statusAt(3, 0) == 'claimed', 'and marked claimed')"
                -- The race this issue deliberately does NOT change:
                -- someone else takes the log between claim and fetch.
                , "GROUND = {}"
                , "for _ = 1, 4 do tick() end"
                , "assert(S.constructJob == nil,"
                , "  'the claim must be released after the failed fetch')"
                , "assert(statusAt(3, 0) == 'pending',"
                , "  'and the tile handed back: ' .. tostring(statusAt(3, 0)))"
                ]
