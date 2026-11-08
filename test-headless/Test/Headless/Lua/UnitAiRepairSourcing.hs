{-# LANGUAGE OverloadedStrings #-}
-- | The repair AI's claim order, sourcing and claim lifecycle (#2939).
--
--   These groups are the deterministic replacement for what
--   the retired repair-AI live probe (#302) used to grade against the LIVE
--   @unit_ai@ tick, where claim, fetch and work timing raced the grade
--   and no two failing runs agreed (#724). They drive the production
--   @scripts/unit_ai_repair.lua@ + @unit_ai_repair_target.lua@ through
--   @RP.utility@ \/ @RP.execute@ one explicit tick at a time, exactly as
--   "Test.Headless.Lua.UnitAiRepairGround" does for the ground rung, so
--   an ordering or a hand-back is read off a state the test itself
--   advanced rather than off a poll that may have missed it.
--
--   The real engine supplies the ground, the inventories, the equipment
--   loadout, the transfers, the unit table and the skill verbs. The
--   station and @repair.repairAt@ are the shared fixture's stubs; in
--   'withRestore' mode the stub restores the targeted axis in place
--   through the real @unit.repairItem@ and spends the recipe's one
--   ingredient with the real @unit.removeItem@, so a sequence of jobs
--   can run to completion. WHAT @repairAt@ costs and restores is its own
--   contract (@tools/repair_probe.py@: fuel consumed, axis to 100; and
--   @tools/repair_item_probe.py@: an equipped item is repaired in
--   place) and is deliberately not re-graded here: no group below asserts
--   that an ingredient was spent, only that the AI delivered it.
--
--   @docs/archive/repair_ai_coverage_map.md@ keys every one of the retired
--   probe's 57 checks to the case below that now owns it.
--
--   Run just these groups: @cabal test synarchy-test-headless
--   --test-options='--match "repair sourcing"'@.
module Test.Headless.Lua.UnitAiRepairSourcing (spec) where

import UPrelude
import Test.Hspec
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import Data.IORef (readIORef)
import Data.List (sort)
import Engine.Core.State (EngineEnv(..))
import Engine.Scripting.Lua.Types (LuaBackendState(..))
import Item.Ground (GroundItem(..), GroundItems(..))
import Item.Types (ItemInstance(..))
import Unit.Types (UnitId(..), UnitInstance(..), UnitManager(..))
import World.State.Types (WorldState(..))
import Test.Headless.Lua.UnitAiRepairFixture

-- * Item identities

-- | The own-gear ordering fixture's two candidates; instance ids are
--   distinct per case so a failure names the item that was picked.
brokenAxe, mildGambeson ∷ Word64
brokenAxe    = 801
mildGambeson = 802

-- * Readers

-- | Where an instance is on a unit and its wear, or 'Nothing' when the
--   unit does not carry it at all. @inventory@ is the loose list;
--   @equipped:<slot>@ is the equipment loadout.
heldItem ∷ EngineEnv → UnitId → Word64 → IO (Maybe (Text, Float, Float))
heldItem env uid iid = do
    um ← readIORef (unitManagerRef env)
    pure $ case HM.lookup uid (umInstances um) of
        Nothing → Nothing
        Just u  → case
            [ ("inventory", iiCondition i, iiSharpness i)
            | i ← uiInventory u, iiInstanceId i ≡ iid ]
            <> [ ("equipped:" <> slot, iiCondition i, iiSharpness i)
               | (slot, i) ← HM.toList (uiEquipment u)
               , iiInstanceId i ≡ iid ] of
            (h : _) → Just h
            []      → Nothing

-- | Instance ids in a unit's loose inventory, ascending.
invIds ∷ EngineEnv → UnitId → IO [Word64]
invIds env uid = map (\(i, _, _, _, _, _) → i) ⊚ invOf env uid

-- | How many loose copies of a def a unit carries.
countDef ∷ EngineEnv → UnitId → Text → IO Int
countDef env uid def =
    length ∘ filter (\(_, d, _, _, _, _) → d ≡ def) ⊚ invOf env uid

-- | Defs lying on a page's ground, ascending.
groundDefs ∷ WorldState → IO [Text]
groundDefs ws = sort ∘ map (\(_, d, _, _, _, _) → d) ⊚ groundRows ws

-- | @(instanceId, condition, quality, position)@ for each ground row.
groundInstances ∷ WorldState → IO [(Word64, Float, Float, (Float, Float))]
groundInstances ws = do
    gis ← readIORef (wsGroundItemsRef ws)
    pure $ sort
        [ ( iiInstanceId (giInst gi), iiCondition (giInst gi)
          , iiQuality (giInst gi), (giX gi, giY gi) )
        | gi ← HM.elems (gisItems gis) ]

-- * Scene plumbing

-- | The production repair modules against a scene whose units live on
--   the ACTIVE page (the only page 'findTechnomule' can see). The stub
--   station is put on that same page, so @unit_ai_page@'s real guard is
--   satisfied honestly rather than bypassed.
loadActive ∷ LuaBackendState → IO ()
loadActive ls = do
    loadRepair ls
    _ ← runOk ls $ luaLines
        [ "STATION_PAGE = '" <> pageText pageActive <> "'; return 'ok'" ]
    pure ()

-- | Make the stub @repair.repairAt@ restore the targeted axis and spend
--   the recipe's ingredient, through the real @unit.repairItem@ and
--   @unit.removeItem@.
withRestore ∷ LuaBackendState → IO ()
withRestore ls = do
    _ ← runOk ls "REPAIR_RESTORE = true; return 'ok'"
    pure ()

-- | The station is destroyed: the engine answers @building.getInfo@ with
--   nothing for the id a running job cached.
destroyStation ∷ LuaBackendState → IO ()
destroyStation ls = do
    _ ← runOk ls "building.getInfo = function() return nil end; return 'ok'"
    pure ()

-- | Tick until the job is released (phase @nil@), at most twelve ticks,
--   and report every phase seen. A job that never ends fails the case
--   that asserts the final phase rather than hanging it.
runToEnd ∷ LuaBackendState → Int → IO [Text]
runToEnd ls uid = go (12 ∷ Int)
  where
    go 0 = pure []
    go n = do
        ph ← tick ls uid
        if ph ≡ q "nil" then pure [ph] else (ph :) ⊚ go (n - 1)

phases ∷ [Text] → [Text]
phases = map q

-- | A candidate-ready scene for the own-gear ordering cases: a degraded
--   gambeson LISTED FIRST and a broken axe second, so a pick by list
--   position would be wrong, and two ground lignite chunks for the
--   consumable.
ownGearScene ∷ Setup
ownGearScene = baseSetup
    { suUnitsActive  = True
    , suWorkerInv    = [ mkItem "wool_gambeson" 802 5 100 1.8
                       , mkItem "axe_steel" 801 0 100 1.0 ]
    , suActiveGround = [ (mkItem "lignite_chunk" 810 100 100 0.5, nearAt)
                       , (mkItem "lignite_chunk" 811 100 100 0.5, nearAt) ] }

-- | A degraded axe and its lignite on the technomule, the worker bare.
muleScene ∷ Setup
muleScene = baseSetup
    { suUnitsActive = True
    , suMuleInv = Just [ mkItem "axe_steel" 830 5 100 1.0
                       , mkItem "lignite_chunk" 831 100 100 0.5 ] }

-- * The spec

spec ∷ SpecWith EngineEnv
spec = describe "repair sourcing" $ do
    claimOrderSpec
    priorityOrderSpec
    equippedSpec
    muleSpec
    deadClaimantSpec
    abortSpec
    collisionSpec
    groundTargetSpec
    roleWeightSpec
    priorityGatingSpec

-- | Severity orders the claims: the broken weapon is claimed before the
--   merely degraded armour, and the armour only after it.
claimOrderSpec ∷ SpecWith EngineEnv
claimOrderSpec = describe "claim order by severity" $ do

    it "claims the broken weapon first and the degraded armour only \
       \after it, whatever order the inventory lists them in" $ \env → do
        ls ← newBareLuaBackend env
        scene ← resetScene env ownGearScene
        loadActive ls
        withRestore ls
        -- Both candidates exist before the first selection, and the
        -- ordering is read from the CLAIM, before any job advances.
        scoreWorker ls `shouldReturn` q "axe_steel|801|condition|own"
        claimFor ls 1 `shouldReturn` q "fetch_consumable"
        claimantOf ls 801 `shouldReturn` q "1"
        claimantOf ls 802 `shouldReturn` q "nil"
        -- The ground lignite is fetched during the job.
        tick ls 1 `shouldReturn` q "walking"
        groundDefs (scActive scene) `shouldReturn` ["lignite_chunk"]
        countDef env workerUid "lignite_chunk" `shouldReturn` 1
        runToEnd ls 1 `shouldReturn` phases ["repairing", "nil"]
        heldItem env workerUid brokenAxe
            `shouldReturn` Just ("inventory", 100, 100)
        claimantOf ls 801 `shouldReturn` q "nil"
        -- Only now does the armour become the pick.
        scoreWorker ls `shouldReturn` q "wool_gambeson|802|condition|own"
        claimFor ls 1 `shouldReturn` q "fetch_consumable"
        claimantOf ls 802 `shouldReturn` q "1"
        runToEnd ls 1 `shouldReturn` phases ["walking", "repairing", "nil"]
        heldItem env workerUid mildGambeson
            `shouldReturn` Just ("inventory", 100, 100)
        groundDefs (scActive scene) `shouldReturn` []
        scoreWorker ls `shouldReturn` q "none"

    it "grants smithing work experience for a repair it completes" $ \env → do
        ls ← newBareLuaBackend env
        _ ← resetScene env ownGearScene
        loadActive ls
        withRestore ls
        _ ← runOk ls "unit.setSkill(1, 'smithing', 20); return 'ok'"
        before ← runOk ls "return tostring(unit.getSkill(1, 'smithing'))"
        claimFor ls 1 `shouldReturn` q "fetch_consumable"
        runToEnd ls 1 `shouldReturn` phases ["walking", "repairing", "nil"]
        after ← runOk ls "return tostring(unit.getSkill(1, 'smithing'))"
        let num t = read (T.unpack (T.filter (≢ '"') t)) ∷ Double
        num after `shouldSatisfy` (> num before)

-- | The player's priority flag (#303) beats severity among a unit's own
--   gear, and clears itself once the flagged item is repaired.
priorityOrderSpec ∷ SpecWith EngineEnv
priorityOrderSpec = describe "player repair priority" $ do

    it "claims the flagged lower-severity armour before the broken \
       \weapon, clears the flag on repair, and then takes the weapon" $ \env → do
        ls ← newBareLuaBackend env
        _ ← resetScene env ownGearScene
        loadActive ls
        withRestore ls
        runOk ls "return tostring(UNITAI.isRepairPriority(802))"
            `shouldReturn` q "false"
        -- Without the flag the weapon wins, so the flag is what moves it.
        scoreWorker ls `shouldReturn` q "axe_steel|801|condition|own"
        _ ← runOk ls "UNITAI.setRepairPriority(802, true); return 'ok'"
        runOk ls "return tostring(UNITAI.isRepairPriority(802))"
            `shouldReturn` q "true"
        scoreWorker ls `shouldReturn` q "wool_gambeson|802|condition|own"
        claimFor ls 1 `shouldReturn` q "fetch_consumable"
        claimantOf ls 802 `shouldReturn` q "1"
        claimantOf ls 801 `shouldReturn` q "nil"
        runToEnd ls 1 `shouldReturn` phases ["walking", "repairing", "nil"]
        heldItem env workerUid mildGambeson
            `shouldReturn` Just ("inventory", 100, 100)
        runOk ls "return tostring(UNITAI.isRepairPriority(802))"
            `shouldReturn` q "false"
        scoreWorker ls `shouldReturn` q "axe_steel|801|condition|own"
        claimFor ls 1 `shouldReturn` q "fetch_consumable"
        runToEnd ls 1 `shouldReturn` phases ["walking", "repairing", "nil"]
        heldItem env workerUid brokenAxe
            `shouldReturn` Just ("inventory", 100, 100)

-- | An EQUIPPED target and a GROUND consumable: the loadout scan and
--   the consumable fetch.
equippedSpec ∷ SpecWith EngineEnv
equippedSpec = describe "an equipped weapon with a ground consumable" $ do

    it "sharpens the weapon in its slot with a whetstone fetched off \
       \the ground" $ \env → do
        ls ← newBareLuaBackend env
        scene ← resetScene env baseSetup
            { suUnitsActive  = True
            , suWorkerEquip  = [ ("right_hand", mkItem "axe_steel" 820 100 5 1.0) ]
            , suActiveGround = [ (mkItem "whetstone" 821 100 100 0.3, nearAt) ] }
        loadActive ls
        withRestore ls
        groundDefs (scActive scene) `shouldReturn` ["whetstone"]
        heldItem env workerUid 820
            `shouldReturn` Just ("equipped:right_hand", 100, 5)
        scoreWorker ls `shouldReturn` q "axe_steel|820|sharpness|own"
        claimFor ls 1 `shouldReturn` q "fetch_consumable"
        tick ls 1 `shouldReturn` q "walking"
        groundDefs (scActive scene) `shouldReturn` []
        invIds env workerUid `shouldReturn` [821]
        runToEnd ls 1 `shouldReturn` phases ["repairing", "nil"]
        -- Restored where it sits: still in the slot, never loose.
        heldItem env workerUid 820
            `shouldReturn` Just ("equipped:right_hand", 100, 100)
        invIds env workerUid >>= (`shouldNotContain` [820])

-- | A degraded item on the technomule: fetch it, fetch its consumable
--   off the same mule, repair, and hand it back.
muleSpec ∷ SpecWith EngineEnv
muleSpec = describe "spare gear held by the technomule" $ do

    it "fetches the item and its consumable off the mule, repairs it \
       \and returns it, leaving the worker holding neither" $ \env → do
        ls ← newBareLuaBackend env
        _ ← resetScene env muleScene
        loadActive ls
        withRestore ls
        scoreWorker ls `shouldReturn` q "axe_steel|830|condition|mule"
        claimFor ls 1 `shouldReturn` q "fetch_item"
        claimantOf ls 830 `shouldReturn` q "1"
        tick ls 1 `shouldReturn` q "fetch_consumable"
        invIds env workerUid `shouldReturn` [830]
        invIds env muleUid `shouldReturn` [831]
        tick ls 1 `shouldReturn` q "walking"
        invIds env workerUid `shouldReturn` [830, 831]
        invIds env muleUid `shouldReturn` []
        runToEnd ls 1 `shouldReturn` phases ["repairing", "nil"]
        heldItem env muleUid 830 `shouldReturn` Just ("inventory", 100, 100)
        heldItem env workerUid 830 `shouldReturn` Nothing
        invIds env workerUid `shouldReturn` []

-- | A claimant that dies mid-fetch releases the claim (the stale-claim
--   self-heal chopClaims and constructClaims have), and a second worker
--   finishes the job.
deadClaimantSpec ∷ SpecWith EngineEnv
deadClaimantSpec = describe "a dead claimant on a mule-held item" $ do

    it "holds the claim while the claimant lives, releases it when the \
       \claimant dies, and lets a second worker finish the repair" $ \env → do
        ls ← newBareLuaBackend env
        _ ← resetScene env muleScene { suRival = True }
        loadActive ls
        withRestore ls
        claimFor ls 2 `shouldReturn` q "fetch_item"
        claimantOf ls 830 `shouldReturn` q "2"
        -- A LIVE claimant keeps the item to itself ...
        scoreWorker ls `shouldReturn` q "none"
        killUnit env rivalUid
        -- ... and its death returns it, still on the mule: the claimant
        -- never reached it.
        invIds env muleUid `shouldReturn` [830, 831]
        scoreWorker ls `shouldReturn` q "axe_steel|830|condition|mule"
        claimFor ls 1 `shouldReturn` q "fetch_item"
        claimantOf ls 830 `shouldReturn` q "1"
        runToEnd ls 1 >>= (`shouldSatisfy` ((≡ q "nil") ∘ last))
        heldItem env muleUid 830 `shouldReturn` Just ("inventory", 100, 100)

-- | A job aborted AFTER the item left the mule hands it back instead of
--   leaking it into the worker's own inventory.
abortSpec ∷ SpecWith EngineEnv
abortSpec = describe "aborting after the item was fetched" $ do

    it "returns the fetched instance to the mule while the worker is \
       \still alive, and releases the claim" $ \env → do
        ls ← newBareLuaBackend env
        _ ← resetScene env muleScene
        loadActive ls
        claimFor ls 1 `shouldReturn` q "fetch_item"
        -- The station the job cached is the one destroyed below.
        jobField ls 1 "bid" `shouldReturn` q "900"
        tick ls 1 `shouldReturn` q "fetch_consumable"
        invIds env workerUid `shouldReturn` [830]
        invIds env muleUid `shouldReturn` [831]
        destroyStation ls
        tick ls 1 `shouldReturn` q "nil"
        -- Observed on the LIVE worker, before any teardown.
        invIds env muleUid `shouldReturn` [830, 831]
        heldItem env workerUid 830 `shouldReturn` Nothing
        heldItem env muleUid 830 `shouldReturn` Just ("inventory", 5, 100)
        claimantOf ls 830 `shouldReturn` q "nil"
        jobField ls 1 "instanceId" `shouldReturn` q "nil"

-- | The worker's OWN healthy copy of the same def is never confused with
--   the flagged instance: every hand-off is targeted by instanceId.
collisionSpec ∷ SpecWith EngineEnv
collisionSpec = describe "the worker's own same-def item" $ do

    let scene = muleScene
            { suWorkerInv = [ mkItem "axe_steel" 840 100 100 1.0 ]
            , suMuleInv   = Just [ mkItem "axe_steel" 841 5 100 1.0
                                 , mkItem "lignite_chunk" 842 100 100 0.5 ] }

    it "repairs and returns the flagged mule instance, never the \
       \worker's own axe" $ \env → do
        ls ← newBareLuaBackend env
        _ ← resetScene env scene
        loadActive ls
        withRestore ls
        -- The worker's own axe is healthy, so it is not a candidate.
        scoreWorker ls `shouldReturn` q "axe_steel|841|condition|mule"
        claimFor ls 1 `shouldReturn` q "fetch_item"
        tick ls 1 `shouldReturn` q "fetch_consumable"
        -- Both axes are now in the worker's hands; the own one lists
        -- first, so a defName-only hand-off would pop IT.
        invIds env workerUid `shouldReturn` [840, 841]
        runToEnd ls 1 >>= (`shouldSatisfy` ((≡ q "nil") ∘ last))
        heldItem env muleUid 841 `shouldReturn` Just ("inventory", 100, 100)
        heldItem env workerUid 840 `shouldReturn` Just ("inventory", 100, 100)
        heldItem env workerUid 841 `shouldReturn` Nothing
        countDef env workerUid "axe_steel" `shouldReturn` 1

    it "hands back the flagged instance, not the worker's own axe, when \
       \the job aborts" $ \env → do
        ls ← newBareLuaBackend env
        _ ← resetScene env scene
        loadActive ls
        claimFor ls 1 `shouldReturn` q "fetch_item"
        tick ls 1 `shouldReturn` q "fetch_consumable"
        invIds env workerUid `shouldReturn` [840, 841]
        destroyStation ls
        tick ls 1 `shouldReturn` q "nil"
        heldItem env muleUid 841 `shouldReturn` Just ("inventory", 5, 100)
        invIds env workerUid `shouldReturn` [840]
        heldItem env workerUid 840 `shouldReturn` Just ("inventory", 100, 100)

    it "fetches the flagged instance off a mule that carries another \
       \copy of the def listed ahead of it" $ \env → do
        ls ← newBareLuaBackend env
        _ ← resetScene env scene
            { suWorkerInv = []
            , suMuleInv   = Just [ mkItem "axe_steel" 843 100 100 1.0
                                 , mkItem "axe_steel" 841 5 100 1.0
                                 , mkItem "lignite_chunk" 842 100 100 0.5 ] }
        loadActive ls
        withRestore ls
        scoreWorker ls `shouldReturn` q "axe_steel|841|condition|mule"
        claimFor ls 1 `shouldReturn` q "fetch_item"
        tick ls 1 `shouldReturn` q "fetch_consumable"
        -- A defName-only transfer would have popped the healthy 843.
        invIds env workerUid `shouldReturn` [841]
        invIds env muleUid `shouldReturn` [842, 843]
        runToEnd ls 1 >>= (`shouldSatisfy` ((≡ q "nil") ∘ last))
        heldItem env muleUid 841 `shouldReturn` Just ("inventory", 100, 100)
        heldItem env muleUid 843 `shouldReturn` Just ("inventory", 100, 100)
        invIds env workerUid `shouldReturn` []

-- | The part of the ground rung "Test.Headless.Lua.UnitAiRepairGround"
--   cannot see through its non-restoring station stub: the instance that
--   comes back has actually been repaired.
groundTargetSpec ∷ SpecWith EngineEnv
groundTargetSpec = describe "a ground-sourced target that is repaired" $ do

    it "goes back to the ground at full condition with its quality \
       \unchanged, and the worker is not left holding it" $ \env → do
        ls ← newBareLuaBackend env
        scene ← resetScene env baseSetup
            { suWorkerInv   = [ mkItem "lignite_chunk" 850 100 100 0.5 ]
            , suOwnedGround = [ (mkItem "axe_steel" 851 3 100 1.0, nearAt) ] }
        loadRepair ls
        withRestore ls
        scoreWorker ls `shouldReturn` q "axe_steel|851|condition|ground"
        claimFor ls 1 `shouldReturn` q "fetch_ground"
        runToEnd ls 1 `shouldReturn`
            phases ["fetch_consumable", "walking", "repairing", "nil"]
        runOk ls "return tostring(REPAIRED[1].instanceId)"
            `shouldReturn` q "851"
        -- Instance 851, repaired to 100, quality still mkItem's 88, back
        -- on the worker's own tile.
        groundInstances (scOwned scene) `shouldReturn` [(851, 100, 88, startAt)]
        heldItem env workerUid 851 `shouldReturn` Nothing

-- | Pure Lua: the smith role's weight on repair_job (#265).
roleWeightSpec ∷ SpecWith EngineEnv
roleWeightSpec = describe "role weighting of repair_job" $ do

    it "maps repair_job to the craft family and boosts a smith on it" $ \env → do
        ls ← newBareLuaBackend env
        _ ← resetScene env baseSetup
        loadRepair ls
        runOk ls "return require('scripts.unit_roles').ACTION_FAMILY.repair_job"
            `shouldReturn` q "craft"
        w ls "smith" "repair_job" `shouldReturn` q "1.4"
        w ls "miner" "repair_job" `shouldReturn` q "0.7"
        w ls "laborer" "repair_job" `shouldReturn` q "1.0"

    it "damps a smith's other routine work and keeps a miner's own \
       \boosted" $ \env → do
        ls ← newBareLuaBackend env
        _ ← resetScene env baseSetup
        loadRepair ls
        w ls "smith" "dig_designation" `shouldReturn` q "0.7"
        w ls "miner" "dig_designation" `shouldReturn` q "1.4"
  where
    w ls role action = runOk ls $ luaLines
        [ "local m = require('scripts.unit_roles');"
        , "return tostring(m.weight({ role = '" <> role <> "' }, '"
          <> action <> "'))" ]

-- | Pure Lua: the priority menu is gated on the item needing repair
--   (#303 review).
priorityGatingSpec ∷ SpecWith EngineEnv
priorityGatingSpec = describe "priority is offered only for an item that needs repair" $ do

    let healthy  = "{ instanceId = 999901, condition = 90, sharpness = 100 }"
        degraded = "{ instanceId = 999902, condition = 5, sharpness = 100 }"

    it "judges a healthy item above the threshold and a degraded one below" $ \env → do
        ls ← newBareLuaBackend env
        _ ← resetScene env baseSetup
        loadRepair ls
        runOk ls ("return tostring(UNITAI.itemNeedsRepair(" <> healthy <> "))")
            `shouldReturn` q "false"
        runOk ls ("return tostring(UNITAI.itemNeedsRepair(" <> degraded <> "))")
            `shouldReturn` q "true"

    it "offers, shows and hints nothing for a flagged-but-healthy item, \
       \and still offers a degraded one" $ \env → do
        ls ← newBareLuaBackend env
        _ ← resetScene env baseSetup
        loadRepair ls
        -- Flag the healthy instance at the backend, as any path around
        -- the gated menu could.
        _ ← runOk ls "UNITAI.setRepairPriority(999901, true); return 'ok'"
        let rs = "local rs = require('scripts.ui.repair_status'); "
        runOk ls (rs <> "return tostring(rs.menuItem(" <> healthy <> "))")
            `shouldReturn` q "nil"
        -- The suffix must be the EMPTY string, not merely falsy.
        runOk ls (rs <> "return '[' .. rs.suffix(" <> healthy <> ") .. ']'")
            `shouldReturn` q "[]"
        runOk ls (rs <> "return tostring(rs.hintLine(" <> healthy <> "))")
            `shouldReturn` q "nil"
        runOk ls (rs <> "local m = rs.menuItem(" <> degraded
                      <> "); return tostring(m and m.label)")
            `shouldReturn` q "Prioritize Repair"
