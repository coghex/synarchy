{-# LANGUAGE TypeApplications #-}
-- | Woodcutting XP on a fell (#2212): @scripts/unit_ai_chop.lua@ pays
--   for what the fell PRODUCED, not for having swung.
--
--   #2212 gave @world.harvestFloraInstance@ three distinguishable
--   answers where it used to have two:
--
--     * @nil@ — refused, or the target raced away;
--     * an EMPTY table — an accepted fell whose authored phase yield is
--       empty, which is a felled sprout; and
--     * a NON-EMPTY table — the species' roll, a matured or standing-
--       dead tree.
--
--   The action used to grant @chop_xp_per_fell@ unconditionally after
--   the call, so all three paid the same. A sprout costs the colony a
--   fell and yields nothing, and the owner's 2026-09-01 ruling is that
--   it earns no woodcutting XP either; a raced-away target obviously
--   earns none. Only the third case pays.
--
--   Same standalone-Lua-VM pattern as
--   "Test.Headless.Lua.UnitAiHarvest": one self-contained chunk per
--   'it' in a fresh stdlib-only interpreter, asserting inside Lua via
--   @assert()@, with a non-OK 'Lua.Status' surfaced as an hspec failure
--   carrying the Lua message. No engine and no GPU — the clock, the
--   designation authority and the harvest verb are all stubs the chunk
--   drives.
--
--   The engine half of the same contract — that the verb really does
--   answer with an empty table for a sprout and nil for a refusal — is
--   @--match "Chop tag policy"@.
--
--   Run just this gate: @cabal test synarchy-test-headless
--   --test-options='--match "chop fell XP"'@.
module Test.Headless.Lua.ChopFellXp (spec) where

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

-- | One acolyte mid-swing on a designated tree, with every engine call
--   the chop action makes recorded rather than performed.
--
--   @YIELD@ is what @world.harvestFloraInstance@ answers with, and it
--   is the ONE thing the cases vary: @nil@, @{}@, or a spawned list.
--   @XP@ accumulates @unit.addXP@, @CALLS@ counts the verbs, and
--   @fell()@ runs the single execute tick that carries progress past
--   1.0 and therefore fells the tree.
prelude ∷ Text
prelude = lns
    [ "package.loaded['scripts.unit_ai'] = {}"
    , "NOW = 100"
    , "YIELD = nil"
    , "XP = 0"
    , "SKILL = 50.0"
    , "DESIGNATED = true"
    , "CALLS = { harvest = 0, cancel = 0, addXP = 0, setSkill = 0,"
    , "          tags = {} }"
    , "engine = { gameTime = function() return NOW end,"
    , "           logWarn = function() end, logInfo = function() end,"
    , "           logDebug = function() end, logError = function() end }"
    , "unit = {"
    , "  getInfo = function() return { gridX = 4, gridY = 4 } end,"
    , "  exists = function() return true end,"
    , "  getInventory = function() return {} end,"
    , "  getStat = function() return 1.0 end,"
    , "  getSkill = function(_, name)"
    , "    if name ~= 'woodcutting' then return nil end"
    , "    return SKILL end,"
    , "  setSkill = function(_, _, v) CALLS.setSkill = CALLS.setSkill + 1"
    , "    SKILL = v end,"
    , "  addXP = function(_, _, amount)"
    , "    CALLS.addXP = CALLS.addXP + 1; XP = XP + amount end,"
    , "  moveTo = function() end, stop = function() end,"
    , "  setAnimOverride = function() end,"
    , "  clearAnimOverride = function() end }"
    , "item = { listGround = function() return {} end }"
    , "world = {"
    , "  getActiveWorldId = function() return 'w' end,"
    , "  getFloraGrowthAt = function() return {} end,"
    , "  harvestFloraInstance = function(_, _, _, tag)"
    , "    CALLS.harvest = CALLS.harvest + 1"
    , "    CALLS.tags.harvest = tag"
    , "    return YIELD end }"
    , "chop = {"
    , "  getDesignationForInstance = function()"
    , "    if DESIGNATED then return { z = 0 } end"
    , "    return nil end,"
    , "  getDesignationsAt = function() return {} end,"
    , "  nearestDesignation = function() return nil end,"
    , "  cancelDesignation = function() CALLS.cancel = CALLS.cancel + 1 end }"
    , "local chopAi = require('scripts.unit_ai_chop')"
    -- The walk speed the action hands unit.moveTo is not what this gate
    -- is about, and mv.comfort derives it from the whole physiology
    -- stack. Pin it on the module table the action already holds.
    , "require('scripts.movement_speed').comfort = function() return 1.0 end"
    , "PARAMS = { chop_rate = 1.0, chop_bare_speed = 1.0, chop_tools = {},"
    , "           chop_equip_anim = 'equip', chop_work_anim = 'work',"
    , "           chop_equip_seconds = 1.0, chop_claim_timeout = 60.0,"
    , "           chop_scan_range = 24.0, chop_base_utility = 1.0,"
    , "           chop_lock_utility = 9.0, chop_stock_target = 10,"
    , "           chop_stock_floor = 0.1, chop_xp_per_fell = 7.0 }"
    -- A unit already swinging at a claimed, designated tree: the state
    -- the action itself reaches after walking and equipping, written
    -- out so the case is one execute tick rather than a whole approach.
    -- lastChopAt two seconds back is what makes that tick's dt 2.0 and
    -- so carries chopProgress past the 1.0 completion threshold.
    , "local function swinging()"
    , "  return { chopJob = { x = 4, y = 4, iid = 7 },"
    , "           chopPhase = 'chopping', chopProgress = 0.0,"
    , "           chopEquipped = true, lastChopAt = NOW - 2 }"
    , "end"
    , "local function fell()"
    , "  local s = swinging()"
    , "  chopAi.chopExecute(1, s, PARAMS)"
    , "  return s"
    , "end"
    ]

spec ∷ Spec
spec = describe "chop fell XP" $ do

    it "a fell that spawns logs grants the configured per-fell XP" $
        runsOk $ lns
            [ prelude
            , "YIELD = { { id = 'wood_log', gid = 1 },"
            , "          { id = 'wood_log', gid = 2 } }"
            , "local s = fell()"
            -- The fell really happened, so the XP assertion below is
            -- about the grant and not about an execute that bailed.
            , "assert(CALLS.harvest == 1, 'expected one harvest call, got '"
            , "       .. CALLS.harvest)"
            , "assert(CALLS.tags.harvest == 'wood', 'wood-tagged')"
            , "assert(CALLS.cancel == 1, 'the designation must be cancelled')"
            , "assert(XP == 7.0, 'expected 7 XP, got ' .. XP)"
            , "assert(s.chopJob == nil, 'the job must be released')"
            ]

    it "a fell whose authored yield is EMPTY grants none — a sprout \
       \costs a swing and pays nothing" $
        runsOk $ lns
            [ prelude
            , "YIELD = {}"
            , "local s = fell()"
            -- An accepted fell: the harvest happened and the
            -- designation was cleared, so this is a yield question and
            -- not a refusal in disguise.
            , "assert(CALLS.harvest == 1, 'the sprout must still be felled')"
            , "assert(CALLS.cancel == 1, 'the designation must be cancelled')"
            , "assert(s.chopJob == nil, 'the job must be released')"
            , "assert(XP == 0, 'expected no XP, got ' .. XP)"
            , "assert(CALLS.addXP == 0, 'unit.addXP must not be called')"
            -- grantWorkXP SEEDS an absent skill before adding, so a
            -- suppressed grant must not seed one either.
            , "assert(CALLS.setSkill == 0, 'no skill may be seeded')"
            ]

    it "a nil result — the target raced away — grants none and still \
       \completes the job" $
        runsOk $ lns
            [ prelude
            , "YIELD = nil"
            , "local s = fell()"
            , "assert(CALLS.harvest == 1, 'the fell must have been attempted')"
            , "assert(CALLS.cancel == 1, 'the designation must be cancelled')"
            , "assert(s.chopJob == nil, 'the job must be released')"
            , "assert(XP == 0, 'expected no XP, got ' .. XP)"
            ]

    it "seeds an absent woodcutting skill only on a paying fell" $
        runsOk $ lns
            [ prelude
            , "SKILL = nil"
            , "YIELD = { { id = 'wood_log', gid = 1 } }"
            , "fell()"
            , "assert(CALLS.setSkill == 1, 'a paying fell seeds the skill')"
            , "assert(XP == 7.0, 'expected 7 XP, got ' .. XP)"
            ]

    it "does not grant XP before the tree actually falls" $
        runsOk $ lns
            [ prelude
            , "YIELD = { { id = 'wood_log', gid = 1 } }"
            -- One tick's worth of dt too small to cross the threshold:
            -- the swing continues, nothing is felled, nothing is paid.
            , "local s = swinging()"
            , "s.lastChopAt = NOW - 0.1"
            , "chopAi.chopExecute(1, s, PARAMS)"
            , "assert(CALLS.harvest == 0, 'nothing should have been felled')"
            , "assert(XP == 0, 'expected no XP, got ' .. XP)"
            , "assert(s.chopProgress > 0, 'progress must still accumulate')"
            , "assert(s.chopPhase == 'chopping', 'still chopping')"
            ]
