{-# LANGUAGE TypeApplications #-}
-- | The "cached logistics target clears" gate (#1484):
--   @scripts/unit_ai_logistics.lua@'s two cached building ids —
--   @s.buildTarget@ and @s.storeTarget@ — stop naming a building the
--   moment target resolution finds nothing, on ALL FOUR of the exported
--   entry points that resolve one (@buildNearbyUtility@,
--   @buildNearbyExecute@, @storeMaterialsUtility@,
--   @storeMaterialsExecute@).
--
--   Both fields are PERSISTED building references
--   (@scripts/unit_ai_save_refs.lua@'s @AI_BUILDING_REF_FIELDS@), so a
--   value left behind after its building died crossed the save boundary
--   as a @dangling-reference@ diagnostic and, after a load that reused
--   the id, read as a live building the unit never targeted.
--   @unitAi.onSaveLoaded@'s scrub cleaned it at LOAD time; nothing
--   cleaned it at runtime, and @buildTarget@ had no clear of any kind
--   (@storeTarget@ had only the completion clear after a successful
--   deposit). Its stale value is also read live —
--   @countBuildersAt@ and @scripts/unit_ai_core.lua@ both compare
--   @s.buildTarget@ against a bid — so a collision inflated a real
--   site's worker count.
--
--   The four negative cases pre-populate the corresponding field with a
--   building id, then call the function in a world where resolution
--   fails, and require the field to come back @nil@. The four positive
--   cases are the other half of the contract (requirement 4): with a
--   valid target present each function must still record that target's
--   id, so the fix cannot degenerate into "never cache". The execute
--   positives deliberately stand the unit FAR from the target so the
--   walk branch is what is observed — @storeMaterialsExecute@'s
--   arrival branch ends with its own completion clear, which would mask
--   the caching write.
--
--   Same standalone-Lua-VM pattern as "Test.Headless.Lua.UnitAiStall":
--   each 'it' runs one self-contained chunk via 'Lua.dostring' in a
--   fresh interpreter, asserting inside Lua via @assert()@, with a
--   non-OK 'Lua.Status' surfaced as an hspec failure carrying the Lua
--   message. @scripts.movement_speed@ is stubbed at @package.loaded@
--   (only @comfort@ is reached, and the real one pulls in the whole
--   physiology chain); @scripts.unit_roles@ is the real module (a
--   role-less state weighs 1.0), and the utility parameters are the
--   REAL shipped @scripts/unit_ai_tunables.lua@ acolyte block rather
--   than hand-copied numbers, so a tunable change cannot leave this
--   gate scoring against values the game no longer uses.
--
--   Run just this gate: @cabal test synarchy-test-headless
--   --test-options='--match "cached logistics target clears"'@.
module Test.Headless.Lua.UnitAiLogisticsTargets (spec) where

import UPrelude
import Test.Hspec
import qualified HsLua as Lua
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE

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

-- | The engine surface the four functions reach at call time, driven by
--   a single @BUILDINGS@ table the cases rewrite.
--
--   A row is @{ bid, x, y, activity, required, capacity, used }@:
--   @findStorageTarget@ wants @activity == 'built'@ with
--   @used < capacity@, @findNearestUnbuilt@ wants
--   @activity == 'constructing'@ with @required > 0@. Emptying
--   @BUILDINGS@, or leaving only rows of the other shape, is how a case
--   makes resolution fail — the same way a destroyed site fails it in
--   game.
--
--   @INV@ carries one Materials item so @storeMaterialsUtility@ gets
--   past its inventory guard, and @CARRIED@\/@CAPACITY@ put the unit at
--   half load so its fill factor is a real number rather than 0 or a
--   division by zero.
prelude ∷ Text
prelude = lns
    [ "package.loaded['scripts.unit_ai'] = {}"
    , "package.loaded['scripts.movement_speed'] ="
    , "  { comfort = function() return 1.0 end,"
    , "    ordered = function() return 1.0 end,"
    , "    sprint  = function() return 1.0 end }"
    , "NOW = 0"
    -- #1673: every candidate is now page-qualified against the actor,
    -- so the stub world has to HAVE a page. One page throughout —
    -- cross-page rejection is Test.Headless.Lua.UnitAiPageTargets'
    -- subject, and this gate is still only about the cached ids.
    , "PAGE = 'stub_page'"
    , "POS = { gridX = 0, gridY = 0, page = PAGE }"
    , "BUILDINGS = {}"
    , "INV = { { defName = 'plate_steel', category = 'Materials' } }"
    , "CARRIED, CAPACITY = 50, 100"
    , "MOVES, STOPS, DEPOSITS = 0, 0, 0"
    , "engine = { gameTime = function() return NOW end,"
    , "           logWarn = function() end, logError = function() end,"
    , "           logInfo = function() end, logDebug = function() end,"
    , "           emitEvent = function() end, emitEventAt = function() end,"
    , "           emitEventForUnit = function() end }"
    , "world = { getActiveWorldId = function() return 1 end }"
    , "debug = debug or {}"
    , "debug.recordOutcome = function() end"
    , "local function row(bid)"
    , "  for _, b in ipairs(BUILDINGS) do if b.bid == bid then return b end end"
    , "end"
    , "building = {"
    , "  getActiveIds = function()"
    , "    local ids = {}"
    , "    for _, b in ipairs(BUILDINGS) do ids[#ids + 1] = b.bid end"
    , "    return ids end,"
    , "  getActivity = function(bid) local b = row(bid); return b and b.activity end,"
    , "  getBuildRequired = function(bid) local b = row(bid); return b and b.required end,"
    , "  getStorageCapacity = function(bid) local b = row(bid); return b and b.capacity end,"
    , "  getStorageWeight = function(bid) local b = row(bid); return b and b.used end,"
    , "  getInfo = function(bid)"
    , "    local b = row(bid)"
    , "    if not b then return nil end"
    , "    return { gridX = b.x, gridY = b.y, tileW = 1, tileH = 1,"
    , "             page = PAGE } end }"
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
    , "  moveTo = function() MOVES = MOVES + 1 end,"
    , "  stop = function() STOPS = STOPS + 1 end,"
    , "  depositToCargo = function() DEPOSITS = DEPOSITS + 1; return false end }"
    , "local logistics = require('scripts.unit_ai_logistics')"
    , "-- The SHIPPED acolyte tunables, not a hand-copied subset."
    , "local PARAMS = require('scripts.unit_ai_tunables').acolyte"
    , "assert(PARAMS and PARAMS.build_scan_range and PARAMS.store_scan_range,"
    , "  'fixture: the shipped acolyte tunables must carry the build/store keys')"
    , "-- A building of the WANTED shape, at `d` tiles along +x."
    , "local function place(kind, bid, d)"
    , "  if kind == 'store' then"
    , "    BUILDINGS = { { bid = bid, x = d, y = 0, activity = 'built',"
    , "                    required = 0, capacity = 100, used = 0 } }"
    , "  else"
    , "    BUILDINGS = { { bid = bid, x = d, y = 0, activity = 'constructing',"
    , "                    required = 240.0, capacity = 0, used = 0 } }"
    , "  end"
    , "end"
    , "local STALE = 4242   -- the id a dead building left behind"
    ]

-- | Every case asserts on a state table it owns outright, so nothing
--   leaks between them.
newState ∷ Text
newState = "local s = { currentAction = nil }"

spec ∷ Spec
spec = describe "cached logistics target clears" $ do

    describe "buildTarget" $ do
        it "is cleared when buildNearbyUtility resolves no site" $
            runsOk $ lns
                [ prelude
                , newState
                , "s.buildTarget = STALE"
                -- A built (not constructing) building is still no build
                -- site: resolution runs and finds nothing, exactly as
                -- it does once the real site is destroyed.
                , "place('store', 9, 3)"
                , "local u = logistics.buildNearbyUtility(1, s, PARAMS)"
                , "assert(u == -math.huge, 'no site must score -inf, got ' .. tostring(u))"
                , "assert(s.buildTarget == nil,"
                , "  'buildNearbyUtility must clear the cached site, left '"
                , "  .. tostring(s.buildTarget))"
                ]

        it "is cleared when buildNearbyExecute resolves no site" $
            runsOk $ lns
                [ prelude
                , newState
                , "s.buildTarget = STALE"
                , "BUILDINGS = {}"
                , "logistics.buildNearbyExecute(1, s, PARAMS)"
                , "assert(s.buildTarget == nil,"
                , "  'buildNearbyExecute must clear the cached site, left '"
                , "  .. tostring(s.buildTarget))"
                , "assert(MOVES == 0 and STOPS == 0,"
                , "  'a unit with no site must neither walk nor stop')"
                ]

        it "still records a site that DOES resolve, from either entry point" $
            runsOk $ lns
                [ prelude
                , newState
                , "place('build', 77, 5)"
                , "local u = logistics.buildNearbyUtility(1, s, PARAMS)"
                , "assert(s.buildTarget == 77,"
                , "  'buildNearbyUtility must cache the resolved site, got '"
                , "  .. tostring(s.buildTarget))"
                , "assert(u > 0, 'a reachable site must score above zero, got ' .. tostring(u))"
                , newState
                , "logistics.buildNearbyExecute(1, s, PARAMS)"
                , "assert(s.buildTarget == 77,"
                , "  'buildNearbyExecute must cache the resolved site, got '"
                , "  .. tostring(s.buildTarget))"
                , "assert(MOVES == 1, 'the unit must walk toward a distant site')"
                ]

        it "clears a stale id even when a DIFFERENT site is out of range" $
            runsOk $ lns
                [ prelude
                , newState
                , "s.buildTarget = STALE"
                -- Beyond build_scan_range: findNearestUnbuilt returns
                -- nil, so this is the no-target branch, not a swap.
                , "place('build', 77, PARAMS.build_scan_range + 10)"
                , "logistics.buildNearbyUtility(1, s, PARAMS)"
                , "assert(s.buildTarget == nil,"
                , "  'an out-of-range site is no site, left ' .. tostring(s.buildTarget))"
                ]

    describe "storeTarget" $ do
        it "is cleared when storeMaterialsUtility resolves no cargo" $
            runsOk $ lns
                [ prelude
                , newState
                , "s.storeTarget = STALE"
                -- A constructing (not built) building is no cargo.
                , "place('build', 9, 3)"
                , "local u = logistics.storeMaterialsUtility(1, s, PARAMS)"
                , "assert(u == -math.huge, 'no cargo must score -inf, got ' .. tostring(u))"
                , "assert(s.storeTarget == nil,"
                , "  'storeMaterialsUtility must clear the cached cargo, left '"
                , "  .. tostring(s.storeTarget))"
                ]

        it "is cleared when storeMaterialsExecute resolves no cargo" $
            runsOk $ lns
                [ prelude
                , newState
                , "s.storeTarget = STALE"
                , "BUILDINGS = {}"
                , "logistics.storeMaterialsExecute(1, s, PARAMS)"
                , "assert(s.storeTarget == nil,"
                , "  'storeMaterialsExecute must clear the cached cargo, left '"
                , "  .. tostring(s.storeTarget))"
                , "assert(MOVES == 0 and DEPOSITS == 0,"
                , "  'a unit with no cargo must neither walk nor deposit')"
                ]

        it "still records a cargo that DOES resolve, from either entry point" $
            runsOk $ lns
                [ prelude
                , newState
                , "place('store', 88, 5)"
                , "local u = logistics.storeMaterialsUtility(1, s, PARAMS)"
                , "assert(s.storeTarget == 88,"
                , "  'storeMaterialsUtility must cache the resolved cargo, got '"
                , "  .. tostring(s.storeTarget))"
                , "assert(u > 0, 'a reachable cargo must score above zero, got ' .. tostring(u))"
                -- Far enough that execute takes the WALK branch: the
                -- arrival branch ends with the pre-existing completion
                -- clear, which would hide the caching write.
                , newState
                , "logistics.storeMaterialsExecute(1, s, PARAMS)"
                , "assert(s.storeTarget == 88,"
                , "  'storeMaterialsExecute must cache the resolved cargo, got '"
                , "  .. tostring(s.storeTarget))"
                , "assert(MOVES == 1, 'the unit must walk toward a distant cargo')"
                ]

        it "keeps the completion clear after a successful deposit run" $
            runsOk $ lns
                [ prelude
                , newState
                -- Adjacent (Chebyshev 1) -> the arrival branch runs the
                -- deposit loop and clears on the way out, unchanged.
                , "place('store', 88, 1)"
                , "logistics.storeMaterialsExecute(1, s, PARAMS)"
                , "assert(DEPOSITS == 1, 'the adjacent branch must attempt a deposit')"
                , "assert(s.storeTarget == nil,"
                , "  'the completion clear must survive, left ' .. tostring(s.storeTarget))"
                ]

    describe "the two fields are independent" $
        it "clearing one leaves the other's live value alone" $
            runsOk $ lns
                [ prelude
                , newState
                -- One constructing site resolves for build; nothing
                -- resolves for store. Only storeTarget may be touched.
                , "place('build', 77, 5)"
                , "s.storeTarget = STALE"
                , "logistics.buildNearbyUtility(1, s, PARAMS)"
                , "assert(s.buildTarget == 77, 'the build side must still cache')"
                , "logistics.storeMaterialsUtility(1, s, PARAMS)"
                , "assert(s.storeTarget == nil, 'the store side must clear')"
                , "assert(s.buildTarget == 77,"
                , "  'clearing storeTarget must not disturb buildTarget, got '"
                , "  .. tostring(s.buildTarget))"
                ]
