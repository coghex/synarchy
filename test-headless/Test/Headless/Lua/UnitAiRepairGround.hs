{-# LANGUAGE OverloadedStrings #-}
-- | The repair AI's GROUND rung (#1737) — the middle step of #302's
--   "own inventory → ground item → technomule" sourcing ladder, which
--   the merged #302 implementation never delivered.
--
--   Two things had to become true together, and this gate holds both
--   halves against the same live engine.
--
--   The ENGINE half: @item.listGround@ and @item.getGroundForUnit@ now
--   describe a ground item's @instanceId@, @sharpness@ and @kind@
--   alongside the @condition@ they already carried. All three come out
--   of the ONE shared @pushGroundRow@ (#1666), so the two readers
--   cannot describe the same item differently — which is precisely why
--   these cases read the REAL rows the engine builds rather than a Lua
--   fixture's idea of them. A stubbed ground table would agree with
--   itself no matter what the Haskell did.
--
--   The AI half: @scripts/unit_ai_repair_target.lua@ scores, claims,
--   prioritizes, takes and returns a ground instance through exactly
--   the machinery a held or mule-held one goes through. Because
--   severity keys on @condition@ \/ @sharpness@ \/ @kind@ and claims key
--   on @instanceId@, that is only possible with the engine half in
--   place: a ground row missing @sharpness@ would silently only ever
--   find condition-axis targets, which reads as "working".
--
--   Fixture technique is 'Test.Headless.Lua.UnitAiPickupPage''s, for
--   its reason: page selection is load-bearing here (#1673), and a
--   fixture answering every ground query from one stubbed table
--   structurally cannot see a page bug. Page A is ACTIVE and holds a
--   decoy at the SAME gid; the worker stands on live, non-active page
--   B. Ground reads, resolution, capacity, the pickup, the drop and the
--   inventory are all the engine's own.
--
--   Only what this rung is not about is replaced, at
--   @package.loaded@ \/ the global table: @scripts.movement_speed@ (the
--   real one reaches four physiology modules to answer one pace), the
--   @scripts.unit_ai@ singleton the submodules attach to, and the
--   @building@ \/ @repair@ station surface — #301's station↔axis
--   mapping and @repair.repairAt@'s restore are explicitly out of
--   #1737's scope and have their own coverage.
--
--   Run just this gate: @cabal test synarchy-test-headless
--   --test-options='--match "repair ground target"'@.
module Test.Headless.Lua.UnitAiRepairGround (spec) where

import UPrelude
import Test.Hspec
import qualified Data.HashMap.Strict as HM
import Data.IORef (writeIORef, atomicModifyIORef')
import Engine.Core.State (EngineEnv(..))
import Item.Ground (GroundItem(..), GroundItems(..))
import Unit.Types (UnitInstance(..), UnitManager(..))
import World.State.Types (WorldManager(..), WorldState(..), emptyWorldManager)
import Test.Headless.Lua.UnitAiRepairFixture

-- * The spec

spec ∷ SpecWith EngineEnv
spec = describe "repair ground target" $ do
    rowShapeSpec
    selectionSpec
    takeSpec
    returnSpec
    ownershipLossSpec
    durabilitySpec

-- | The engine half. A stubbed ground table would agree with itself
--   whatever the Haskell did, so every assertion here reads rows the
--   real 'pushGroundRow' built.
rowShapeSpec ∷ SpecWith EngineEnv
rowShapeSpec = describe "the row both ground readers build" $ do

    it "gives item.listGround and item.getGroundForUnit the same \
       \instanceId, sharpness and kind for every item on the page" $ \env → do
        ls ← newBareLuaBackend env
        -- The ACTIVE page holds a DIFFERENT instance at the same gid, so
        -- a listing read that never re-resolved would compare page A's
        -- row against page B's item and still find "a" row.
        _ ← resetScene env baseSetup
            { suActiveGround = [ (mkItem "steel_anvil" 900 100 100 60, nearAt)
                               , (mkItem "steel_anvil" 901 100 100 60, nearAt) ]
            , suOwnedGround  = [ (mkItem "wool_gambeson" 501 12 100 1.8, nearAt)
                               , (mkItem "axe_steel" 502 100 9 1.0, walkAt) ] }
        loadRepair ls
        r ← runOk ls $ luaLines
            [ "local out = {};"
            , "for _, g in ipairs(item.listGround() or {}) do"
            , "  local o = item.getGroundForUnit(1, g.id);"
            , "  if not o then out[#out+1] = g.id .. ':unresolved'"
            , "  else out[#out+1] = tostring(g.id) .. ':' .. o.defName"
            , "    .. ':' .. tostring(o.instanceId) .. ':' .. tostring(o.kind)"
            , "    .. ':' .. tostring(o.condition) .. ':' .. tostring(o.sharpness)"
            , "    .. ':' .. tostring(g.instanceId == o.instanceId)"
            , "    .. ':' .. tostring(g.kind == o.kind)"
            , "    .. ':' .. tostring(g.sharpness == o.sharpness) end end;"
            , "table.sort(out); return table.concat(out, ' ')" ]
        -- listGround reads page A (gid 0 = the anvil), getGroundForUnit
        -- reads page B (gid 0 = the gambeson): the SAME builder, two
        -- different items, which is exactly the #1666 hazard the ground
        -- rung has to resolve through rather than around.
        r `shouldBe` q ("0:wool_gambeson:501:armor:12.0:100.0:false:false:true"
                     <> " 1:axe_steel:502:weapon:100.0:9.0:false:false:false")

    it "always carries sharpness and kind, even where qualityTier is \
       \absent because the def declares no quality spec" $ \env → do
        ls ← newBareLuaBackend env
        -- axe_steel's fixture def has NO quality spec, so its row has no
        -- qualityTier. sharpness is universal runtime wear and must not
        -- follow that present-only-when-declared convention.
        _ ← resetScene env baseSetup
            { suOwnedGround = [ (mkItem "axe_steel" 601 100 9 1.0, nearAt) ] }
        loadRepair ls
        r ← runOk ls $ luaLines
            [ "local o = item.getGroundForUnit(1, 0);"
            , "return tostring(o.qualityTier) .. '|' .. tostring(o.sharpness)"
            , "  .. '|' .. tostring(o.kind) .. '|' .. tostring(o.condition)" ]
        r `shouldBe` q "nil|9.0|weapon|100.0"

    it "defaults kind to misc for an item whose def is gone, the same \
       \way unit.getInventory does, so no predicate reads a nil field" $ \env → do
        ls ← newBareLuaBackend env
        _ ← resetScene env baseSetup
            { suOwnedGround = [ (mkItem "ghost_relic" 602 5 5 1.0, nearAt) ] }
        loadRepair ls
        r ← runOk ls $ luaLines
            [ "local o = item.getGroundForUnit(1, 0);"
            , "return tostring(o.kind) .. '|' .. tostring(o.instanceId)" ]
        r `shouldBe` q "misc|602"

-- | Requirement 2, 3 and 9: the same scoring, the same ordering, an
--   absolute ladder, and every field read off the RESOLVED row.
selectionSpec ∷ SpecWith EngineEnv
selectionSpec = describe "choosing a ground target" $ do

    it "picks a condition-axis ground target" $ \env → do
        ls ← newBareLuaBackend env
        _ ← resetScene env baseSetup
            { suOwnedGround = [ (mkItem "axe_steel" 701 11 100 1.0, nearAt) ] }
        loadRepair ls
        scoreWorker ls `shouldReturn` q "axe_steel|701|condition|ground"

    it "picks a sharpness-axis ground target when condition is healthy, \
       \which is only reachable because the row carries sharpness" $ \env → do
        ls ← newBareLuaBackend env
        _ ← resetScene env baseSetup
            { suOwnedGround = [ (mkItem "axe_steel" 702 100 9 1.0, nearAt) ] }
        loadRepair ls
        scoreWorker ls `shouldReturn` q "axe_steel|702|sharpness|ground"

    it "reads the broken-armour band off the row's kind, so a broken \
       \gambeson outranks a broken axe" $ \env → do
        ls ← newBareLuaBackend env
        -- Both are condition 0. Only `kind` separates 2.5 from 1.5, and
        -- `kind` reaches the row only through pushGroundRow's def lookup.
        _ ← resetScene env baseSetup
            { suOwnedGround = [ (mkItem "axe_steel" 703 0 100 1.0, nearAt)
                              , (mkItem "wool_gambeson" 704 0 100 1.8, walkAt) ] }
        loadRepair ls
        scoreWorker ls `shouldReturn` q "wool_gambeson|704|condition|ground"

    it "ignores a listed id that does not resolve on the worker's own \
       \page" $ \env → do
        ls ← newBareLuaBackend env
        -- The degraded item exists ONLY on the ACTIVE page. An
        -- active-page scan would find and claim it; a resolved scan
        -- finds nothing at all.
        _ ← resetScene env baseSetup
            { suActiveGround = [ (mkItem "axe_steel" 705 3 100 1.0, nearAt) ] }
        loadRepair ls
        scoreWorker ls `shouldReturn` q "none"

    it "ignores a resolved row outside repair_scan_range" $ \env → do
        ls ← newBareLuaBackend env
        _ ← resetScene env baseSetup
            { suOwnedGround = [ (mkItem "axe_steel" 706 3 100 1.0, farAt) ] }
        loadRepair ls
        scoreWorker ls `shouldReturn` q "none"

    it "selects nothing at all when the worker has no live page to \
       \resolve against" $ \env → do
        ls ← newBareLuaBackend env
        _ ← resetScene env baseSetup
            { suActiveGround = [ (mkItem "axe_steel" 707 3 100 1.0, nearAt) ]
            , suOwnedGround  = [ (mkItem "axe_steel" 708 3 100 1.0, nearAt) ] }
        loadRepair ls
        unloadOwnedPage env
        scoreWorker ls `shouldReturn` q "none"

    it "prefers its OWN held gear over a more severely damaged ground \
       \instance" $ \env → do
        ls ← newBareLuaBackend env
        -- Held severity 0.36 (condition 20) versus ground 2.5 (broken
        -- armour). The ladder is absolute, so severity does not promote
        -- the lower rung.
        _ ← resetScene env baseSetup
            { suWorkerInv   = [ mkItem "axe_steel" 710 20 100 1.0 ]
            , suOwnedGround = [ (mkItem "wool_gambeson" 711 0 100 1.8, nearAt) ] }
        loadRepair ls
        scoreWorker ls `shouldReturn` q "axe_steel|710|condition|own"

    it "prefers a ground instance over a more severely damaged one on \
       \the technomule" $ \env → do
        ls ← newBareLuaBackend env
        _ ← resetScene env baseSetup
            { suOwnedGround = [ (mkItem "axe_steel" 712 20 100 1.0, nearAt) ]
            , suMuleInv     = Just [ mkItem "wool_gambeson" 713 0 100 1.8 ] }
        loadRepair ls
        scoreWorker ls `shouldReturn` q "axe_steel|712|condition|ground"

    it "skips a ground instance another LIVE worker already claimed" $ \env → do
        ls ← newBareLuaBackend env
        _ ← resetScene env baseSetup
            { suRival       = True
            , suOwnedGround = [ (mkItem "axe_steel" 714 3 100 1.0, nearAt) ] }
        loadRepair ls
        -- The rival claims through the real path, so the entry under
        -- test is the one production writes.
        claimFor ls 2 `shouldReturn` q "fetch_ground"
        scoreWorker ls `shouldReturn` q "none"

    it "self-heals a claim whose claimant is gone and takes the target" $ \env → do
        ls ← newBareLuaBackend env
        _ ← resetScene env baseSetup
            { suRival       = True
            , suOwnedGround = [ (mkItem "axe_steel" 715 3 100 1.0, nearAt) ] }
        loadRepair ls
        claimFor ls 2 `shouldReturn` q "fetch_ground"
        killUnit env rivalUid
        scoreWorker ls `shouldReturn` q "axe_steel|715|condition|ground"

    it "lets a player-prioritized ground instance beat a more severe \
       \unprioritized one, by the same instanceId key held gear uses" $ \env → do
        ls ← newBareLuaBackend env
        _ ← resetScene env baseSetup
            { suOwnedGround = [ (mkItem "wool_gambeson" 716 0 100 1.8, nearAt)
                              , (mkItem "axe_steel" 717 40 100 1.0, walkAt) ] }
        loadRepair ls
        scoreWorker ls `shouldReturn` q "wool_gambeson|716|condition|ground"
        _ ← runOk ls "UNITAI.setRepairPriority(717, true); return 'ok'"
        runOk ls "return tostring(UNITAI.isRepairPriority(717))"
            `shouldReturn` q "true"
        scoreWorker ls `shouldReturn` q "axe_steel|717|condition|ground"

    it "refuses a ground target it could not carry, weighing the row's \
       \LIVE mass rather than the static def weight" $ \env → do
        ls ← newBareLuaBackend env
        -- The instance weighs 40 where its def weighs 1: a preflight on
        -- the def weight would claim a job this worker can never carry.
        _ ← resetScene env baseSetup
            { suWorkerCap   = 12
            , suOwnedGround = [ (mkItem "axe_steel" 718 3 100 40, nearAt) ] }
        loadRepair ls
        scoreWorker ls `shouldReturn` q "none"

-- | Requirements 3, 6 and 7: what happens between claiming a ground
--   target and holding it.
takeSpec ∷ SpecWith EngineEnv
takeSpec = describe "taking a ground target" $ do

    it "walks to a target outside the arrival radius without touching \
       \the ground" $ \env → do
        ls ← newBareLuaBackend env
        scene ← resetScene env baseSetup
            { suOwnedGround = [ (mkItem "axe_steel" 720 3 100 1.0, walkAt) ] }
        loadRepair ls
        before ← groundRows (scOwned scene)
        claimFor ls 1 `shouldReturn` q "fetch_ground"
        tick ls 1 `shouldReturn` q "fetch_ground"
        groundRows (scOwned scene) `shouldReturn` before
        invOf env workerUid `shouldReturn` []

    it "takes the EXACT instance, with its quality, fill, condition and \
       \sharpness intact, and removes it from the page it lay on" $ \env → do
        ls ← newBareLuaBackend env
        scene ← resetScene env baseSetup
            { suActiveGround = [ (mkItem "steel_anvil" 901 100 100 60, nearAt) ]
            , suOwnedGround  = [ (mkItem "axe_steel" 721 3 17 1.0, nearAt) ] }
        loadRepair ls
        decoyBefore ← groundRows (scActive scene)
        claimFor ls 1 `shouldReturn` q "fetch_ground"
        tick ls 1 `shouldReturn` q "fetch_consumable"
        invOf env workerUid `shouldReturn`
            [ (721, "axe_steel", 3, 17, 88, 0) ]
        groundRows (scOwned scene) `shouldReturn` []
        -- The ACTIVE page's same-numbered gid is untouched.
        groundRows (scActive scene) `shouldReturn` decoyBefore
        jobField ls 1 "groundGid" `shouldReturn` q "nil"
        jobField ls 1 "fromGround" `shouldReturn` q "true"
        jobField ls 1 "itemFetched" `shouldReturn` q "true"

    it "treats a gid now naming a DIFFERENT instance as a raced target: \
       \nothing is repaired and nothing is substituted" $ \env → do
        ls ← newBareLuaBackend env
        scene ← resetScene env baseSetup
            { suOwnedGround = [ (mkItem "axe_steel" 722 3 100 1.0, nearAt) ] }
        loadRepair ls
        claimFor ls 1 `shouldReturn` q "fetch_ground"
        -- Someone else took gid 0 and dropped a fresh axe into the same
        -- allocator slot. The claim is on the INSTANCE, so this is not
        -- our target however identical its def looks.
        atomicModifyIORef' (wsGroundItemsRef (scOwned scene)) $ \gis →
            ( gis { gisItems = HM.insert 0
                        (GroundItem { giInst = mkItem "axe_steel" 723 3 100 1.0
                                    , giX = fst nearAt, giY = snd nearAt })
                        (gisItems gis) }, () )
        tick ls 1 `shouldReturn` q "nil"
        invOf env workerUid `shouldReturn` []
        map (\(g, _, i, _, _, _) → (g, i)) ⊚ groundRows (scOwned scene)
            `shouldReturn` [(0, 723)]
        runOk ls "return tostring(UNITAI.getRepairClaimant(722))"
            `shouldReturn` q "nil"

    it "refuses on ARRIVAL when the load it picked up en route no \
       \longer leaves room, leaving the target on the ground" $ \env → do
        ls ← newBareLuaBackend env
        scene ← resetScene env baseSetup
            { suWorkerCap   = 12
            , suOwnedGround = [ (mkItem "axe_steel" 724 3 100 8, walkAt) ] }
        loadRepair ls
        claimFor ls 1 `shouldReturn` q "fetch_ground"
        tick ls 1 `shouldReturn` q "fetch_ground"      -- still walking
        before ← groundRows (scOwned scene)
        -- The worker arrives having been handed a heavy load en route:
        -- the claim-time preflight already passed and cannot be re-run
        -- retroactively, which is why the arrival gate has to weigh
        -- again rather than trust it.
        atomicModifyIORef' (unitManagerRef env) $ \um →
            ( um { umInstances = HM.adjust
                       (\u → u { uiInventory =
                                   [ mkItem "steel_anvil" 725 100 100 9 ] })
                       workerUid (umInstances um) }, () )
        placeUnit env workerUid walkAt
        tick ls 1 `shouldReturn` q "nil"
        groundRows (scOwned scene) `shouldReturn` before
        map (\(i, d, _, _, _, _) → (i, d)) ⊚ invOf env workerUid
            `shouldReturn` [(725, "steel_anvil")]

-- | Requirement 8: the target always ends up back on the ground.
returnSpec ∷ SpecWith EngineEnv
returnSpec = describe "returning a ground target" $ do

    it "puts the exact instance back on the worker's own tile and page \
       \when the job completes" $ \env → do
        ls ← newBareLuaBackend env
        scene ← resetScene env baseSetup
            { suWorkerInv   = [ mkItem "lignite_chunk" 730 100 100 0.5 ]
            , suOwnedGround = [ (mkItem "axe_steel" 731 3 100 1.0, nearAt) ] }
        loadRepair ls
        decoyBefore ← groundRows (scActive scene)
        _ ← runOk ls "UNITAI.setRepairPriority(731, true); return 'ok'"
        claimFor ls 1 `shouldReturn` q "fetch_ground"
        tick ls 1 `shouldReturn` q "fetch_consumable"
        tick ls 1 `shouldReturn` q "walking"
        tick ls 1 `shouldReturn` q "repairing"
        tick ls 1 `shouldReturn` q "nil"
        -- The station saw the instance that lay on the ground.
        runOk ls "return tostring(REPAIRED[1].instanceId)"
            `shouldReturn` q "731"
        -- Back on the ground, at the WORKER's tile rather than where it
        -- was found, on the worker's own page.
        map (\(_, d, i, _, _, p) → (d, i, p)) ⊚ groundRows (scOwned scene)
            `shouldReturn` [("axe_steel", 731, startAt)]
        -- The drop landed on the worker's OWN page; the active page's
        -- same-numbered gid is untouched.
        groundRows (scActive scene) `shouldReturn` decoyBefore
        map (\(i, d, _, _, _, _) → (i, d)) ⊚ invOf env workerUid
            `shouldReturn` [(730, "lignite_chunk")]
        -- #303's flag self-clears on a ground instance exactly as on a
        -- held one.
        runOk ls "return tostring(UNITAI.isRepairPriority(731))"
            `shouldReturn` q "false"

    it "returns it to the ground when the job aborts after pickup, \
       \never handing it to a technomule" $ \env → do
        ls ← newBareLuaBackend env
        scene ← resetScene env baseSetup
            { suWorkerInv   = [ mkItem "lignite_chunk" 732 100 100 0.5 ]
            , suOwnedGround = [ (mkItem "axe_steel" 733 3 100 1.0, nearAt) ]
            , suMuleInv     = Just [] }
        loadRepair ls
        claimFor ls 1 `shouldReturn` q "fetch_ground"
        tick ls 1 `shouldReturn` q "fetch_consumable"
        -- The station is destroyed mid-job: the walking phase aborts.
        _ ← runOk ls "STATION_OK = false; return 'ok'"
        tick ls 1 `shouldReturn` q "walking"
        _ ← runOk ls $ luaLines
            [ "local s = CORE.aiState[1]; s.repairJob.bid = nil;"
            , "return 'ok'" ]
        tick ls 1 `shouldReturn` q "nil"
        map (\(_, d, i, _, _, p) → (d, i, p)) ⊚ groundRows (scOwned scene)
            `shouldReturn` [("axe_steel", 733, startAt)]
        -- The mule was never handed anything.
        invOf env muleUid `shouldReturn` []
        map (\(i, _, _, _, _, _) → i) ⊚ invOf env workerUid
            `shouldReturn` [732]

    it "keeps the job alive and retries when the drop cannot land, so \
       \the target is never stranded in an inventory" $ \env → do
        ls ← newBareLuaBackend env
        scene ← resetScene env baseSetup
            { suWorkerInv   = [ mkItem "lignite_chunk" 734 100 100 0.5 ]
            , suOwnedGround = [ (mkItem "axe_steel" 735 3 100 1.0, nearAt) ] }
        loadRepair ls
        claimFor ls 1 `shouldReturn` q "fetch_ground"
        tick ls 1 `shouldReturn` q "fetch_consumable"
        tick ls 1 `shouldReturn` q "walking"
        tick ls 1 `shouldReturn` q "repairing"
        -- The repair fails AND the worker's page is gone, so
        -- unit.dropItemById cannot resolve anywhere to drop.
        _ ← runOk ls "REPAIR_OK = false; return 'ok'"
        unloadOwnedPage env
        tick ls 1 `shouldReturn` q "returning"
        map (\(i, _, _, _, _, _) → i) ⊚ invOf env workerUid
            `shouldReturn` [734, 735]
        runOk ls "return tostring(UNITAI.getRepairClaimant(735))"
            `shouldReturn` q "1"
        -- A further tick still cannot drop and still must not release.
        tick ls 1 `shouldReturn` q "returning"
        -- The page comes back; the retry lands and the job ends.
        writeIORef (worldManagerRef env) emptyWorldManager
            { wmWorlds  = [ (pageActive, scActive scene)
                          , (pageOwned, scOwned scene) ]
            , wmVisible = [pageActive] }
        tick ls 1 `shouldReturn` q "nil"
        map (\(_, d, i, _, _, _) → (d, i)) ⊚ groundRows (scOwned scene)
            `shouldReturn` [("axe_steel", 735)]
        map (\(i, _, _, _, _, _) → i) ⊚ invOf env workerUid
            `shouldReturn` [734]
        runOk ls "return tostring(UNITAI.getRepairClaimant(735))"
            `shouldReturn` q "nil"

-- | #2531: a ground-sourced job whose exact target has left the
--   worker's LOOSE inventory can never satisfy its return, so it ends
--   the moment that loss is observed instead of retrying an impossible
--   drop forever at the 6.0 repair lock utility.
--
--   The loss and 'returnSpec'\'s legitimate page-unavailable retry differ
--   ONLY in where the instance actually is — @unit.dropItemById@ answers
--   false for both — so every case here moves the item with the
--   production cargo verb against the real unit manager and reads the
--   outcome off the real inventories and the real ground. A fixture that
--   faked either side could not tell the two apart, which is precisely
--   the conflation being fixed.
ownershipLossSpec ∷ SpecWith EngineEnv
ownershipLossSpec = describe "losing a ground target to another owner" $ do

    it "ends the job and releases its own claim when the fetched target \
       \is handed to another owner before the repair lands" $ \env → do
        ls ← newBareLuaBackend env
        scene ← resetScene env baseSetup
            -- A HEALTHY same-def axe rides along in the worker's own
            -- inventory (no severity, so it is never a candidate itself):
            -- ending the job must not drop, release or repair it in place
            -- of the instance that actually left. The second ground axe
            -- is less severe than the first, so it loses the initial
            -- pick and is still there to be chosen afterwards.
            { suWorkerInv   = [ mkItem "lignite_chunk" 750 100 100 0.5
                              , mkItem "axe_steel" 752 100 100 1.0 ]
            , suOwnedGround = [ (mkItem "axe_steel" 751 3 100 1.0, nearAt)
                              , (mkItem "axe_steel" 753 40 100 1.0, walkAt) ]
            , suRival       = True }
        loadRepair ls
        claimFor ls 1 `shouldReturn` q "fetch_ground"
        tick ls 1 `shouldReturn` q "fetch_consumable"
        tick ls 1 `shouldReturn` q "walking"
        tick ls 1 `shouldReturn` q "repairing"
        claimantOf ls 751 `shouldReturn` q "1"
        transferInstance ls 1 2 "axe_steel" 751 `shouldReturn` q "true"
        tick ls 1 `shouldReturn` q "nil"
        jobField ls 1 "instanceId" `shouldReturn` q "nil"
        claimantOf ls 751 `shouldReturn` q "nil"
        -- Requirement 5: the instance stays with its new owner, nothing
        -- was substituted for it, and the station never saw anything.
        repairedCount ls `shouldReturn` q "0"
        map (\(i, d, _, _, _, _) → (i, d)) ⊚ invOf env rivalUid
            `shouldReturn` [(751, "axe_steel")]
        map (\(i, d, _, _, _, _) → (i, d)) ⊚ invOf env workerUid
            `shouldReturn` [(750, "lignite_chunk"), (752, "axe_steel")]
        map (\(_, d, i, _, _, _) → (d, i)) ⊚ groundRows (scOwned scene)
            `shouldReturn` [("axe_steel", 753)]
        -- Requirement 3: the lock utility went with the job, so the
        -- worker scores its next eligible target normally.
        scoreWorker ls `shouldReturn` q "axe_steel|753|condition|ground"

    it "ends a job already in return-retry when the target is handed \
       \away between retries, and issues no further drop" $ \env → do
        ls ← newBareLuaBackend env
        scene ← resetScene env baseSetup
            { suWorkerInv   = [ mkItem "lignite_chunk" 760 100 100 0.5 ]
            , suOwnedGround = [ (mkItem "axe_steel" 761 3 100 1.0, nearAt) ]
            , suRival       = True }
        loadRepair ls
        claimFor ls 1 `shouldReturn` q "fetch_ground"
        tick ls 1 `shouldReturn` q "fetch_consumable"
        tick ls 1 `shouldReturn` q "walking"
        tick ls 1 `shouldReturn` q "repairing"
        -- Park it in return-retry exactly the way returnSpec's control
        -- does: the repair fails and the drop has no page to land on
        -- while the instance is still loose here. That is the state the
        -- job is ENTITLED to retry from, which is what makes the next
        -- step a change of ownership rather than a change of page.
        _ ← runOk ls "REPAIR_OK = false; return 'ok'"
        unloadOwnedPage env
        tick ls 1 `shouldReturn` q "returning"
        tick ls 1 `shouldReturn` q "returning"
        claimantOf ls 761 `shouldReturn` q "1"
        transferInstance ls 1 2 "axe_steel" 761 `shouldReturn` q "true"
        tick ls 1 `shouldReturn` q "nil"
        jobField ls 1 "instanceId" `shouldReturn` q "nil"
        claimantOf ls 761 `shouldReturn` q "nil"
        -- The page comes back, which is what a retained job was waiting
        -- for: it would drop on the very next tick. There is no job left,
        -- so no further tick issues a drop or refreshes the old claim.
        writeIORef (worldManagerRef env) emptyWorldManager
            { wmWorlds  = [ (pageActive, scActive scene)
                          , (pageOwned, scOwned scene) ]
            , wmVisible = [pageActive] }
        tick ls 1 `shouldReturn` q "nil"
        tick ls 1 `shouldReturn` q "nil"
        groundRows (scOwned scene) `shouldReturn` []
        claimantOf ls 761 `shouldReturn` q "nil"
        map (\(i, _, _, _, _, _) → i) ⊚ invOf env rivalUid
            `shouldReturn` [761]
        -- The fetched consumable is not collateral of the termination.
        map (\(i, _, _, _, _, _) → i) ⊚ invOf env workerUid
            `shouldReturn` [760]

    it "leaves a claim its successor legitimately took, instead of \
       \stamping its own uid over that entry on the way out" $ \env → do
        ls ← newBareLuaBackend env
        _ ← resetScene env baseSetup
            { suWorkerInv   = [ mkItem "lignite_chunk" 770 100 100 0.5 ]
            , suOwnedGround = [ (mkItem "axe_steel" 771 3 100 1.0, nearAt) ]
            , suRival       = True }
        loadRepair ls
        claimFor ls 1 `shouldReturn` q "fetch_ground"
        tick ls 1 `shouldReturn` q "fetch_consumable"
        tick ls 1 `shouldReturn` q "walking"
        tick ls 1 `shouldReturn` q "repairing"
        transferInstance ls 1 2 "axe_steel" 771 `shouldReturn` q "true"
        -- The successor claims through the REAL path rather than by
        -- poking repairClaims: the clock passes repair_claim_timeout
        -- (30s), so repairClaimedByOther expires the original entry and
        -- the rival's own repairUtility scores the instance now sitting
        -- in its inventory as own held gear.
        setGameTime env 100
        claimFor ls 2 `shouldReturn` q "fetch_consumable"
        claimantOf ls 771 `shouldReturn` q "2"
        -- The tick that used to refresh the claim before noticing the
        -- loss, and then delete the refreshed entry through
        -- releaseRepairJob's uid guard.
        tick ls 1 `shouldReturn` q "nil"
        jobField ls 1 "instanceId" `shouldReturn` q "nil"
        claimantOf ls 771 `shouldReturn` q "2"
        jobField ls 2 "instanceId" `shouldReturn` q "771"

    it "ends a fetched ground job the post-load reconcile aborts for a \
       \target that is no longer this worker's, rather than parking it \
       \in return-retry" $ \env → do
        ls ← newBareLuaBackend env
        scene ← resetScene env baseSetup
            { suWorkerInv   = [ mkItem "lignite_chunk" 780 100 100 0.5 ]
            , suOwnedGround = [ (mkItem "axe_steel" 781 3 100 1.0, nearAt) ]
            , suRival       = True }
        loadRepair ls
        claimFor ls 1 `shouldReturn` q "fetch_ground"
        tick ls 1 `shouldReturn` q "fetch_consumable"
        transferInstance ls 1 2 "axe_steel" 781 `shouldReturn` q "true"
        -- #1589's reconcile reaches abortRepairJob DIRECTLY rather than
        -- through repairExecute, so the terminal check has to live in
        -- the abort path as well as ahead of the claim refresh. The
        -- durability case below only covers a job that never fetched
        -- anything, which cannot reach the return branch at all.
        r ← runOk ls $ luaLines
            [ "local ctx = { unit = { [1] = true }, building = { [900] = true },"
            , "  item_instance = {},"
            , "  unitPage = { [1] = '" <> pageText pageOwned <> "' },"
            , "  byPage = { craft_bill = {}, ground_item = {"
            , "    ['" <> pageText pageOwned <> "'] = {} },"
            , "    location_instance = {} },"
            , "  activePage = '" <> pageText pageActive <> "' };"
            , "local n = RECON.scrubStaleRefs(1, CORE.aiState[1], ctx,"
            , "                               RECON.DROP_HOOKS);"
            , "return tostring(n) .. '|'"
            , "  .. tostring(CORE.aiState[1].repairJob)"
            , "  .. '|' .. tostring(CORE.aiState[1].repairPhase)"
            , "  .. '|' .. tostring(UNITAI.getRepairClaimant(781))" ]
        r `shouldBe` q "1|nil|nil|nil"
        -- Left where it now is, with nothing dropped and nothing handed
        -- to a mule; a further tick has no job to act on.
        tick ls 1 `shouldReturn` q "nil"
        map (\(i, _, _, _, _, _) → i) ⊚ invOf env rivalUid
            `shouldReturn` [781]
        map (\(i, _, _, _, _, _) → i) ⊚ invOf env workerUid
            `shouldReturn` [780]
        groundRows (scOwned scene) `shouldReturn` []

-- | The durable half (lua.unit_ai v7): provenance outlives the tick
--   that created it, and the gid it carries is a declared reference.
durabilitySpec ∷ SpecWith EngineEnv
durabilitySpec = describe "ground provenance across a save" $ do

    it "keeps fromGround and groundGid through the component snapshot, \
       \which strips only the transient candidate" $ \env → do
        ls ← newBareLuaBackend env
        _ ← resetScene env baseSetup
            { suOwnedGround = [ (mkItem "axe_steel" 740 3 100 1.0, walkAt) ] }
        loadRepair ls
        claimFor ls 1 `shouldReturn` q "fetch_ground"
        r ← runOk ls $ luaLines
            [ "local REFS = require('scripts.unit_ai_save_refs');"
            , "local V = require('scripts.unit_ai_save_validate');"
            , "local wrapped = REFS.wrapAiState({ [1] = CORE.aiState[1] });"
            , "local errs = V.validate(wrapped);"
            , "local w = wrapped[1].repairJob.groundGid;"
            , "local back = REFS.unwrapAiState(wrapped)[1];"
            , "return tostring(errs) .. '|' .. tostring(w.__ref)"
            , "  .. '|' .. tostring(w.id)"
            , "  .. '|' .. tostring(back.repairJob.fromGround)"
            , "  .. '|' .. tostring(back.repairJob.groundGid)"
            , "  .. '|' .. tostring(back.repairCandidate)" ]
        -- The gid crosses the wire as a TYPED ground_item edge, not a
        -- bare number the integrity graph could not check, and comes
        -- back exactly as the live state spells it. repairCandidate is
        -- the transient the snapshot strips; provenance is not.
        r `shouldBe` q "nil|ground_item|0|true|0|nil"

    it "declares groundGid as a ground_item edge, so a post-load \
       \reconcile drops a job whose target vanished and returns \
       \nothing to the ground it never picked up" $ \env → do
        ls ← newBareLuaBackend env
        scene ← resetScene env baseSetup
            { suOwnedGround = [ (mkItem "axe_steel" 741 3 100 1.0, walkAt) ] }
        loadRepair ls
        claimFor ls 1 `shouldReturn` q "fetch_ground"
        before ← groundRows (scOwned scene)
        r ← runOk ls $ luaLines
            [ "local ctx = { unit = { [1] = true }, building = { [900] = true },"
            , "  item_instance = { [741] = true },"
            , "  unitPage = { [1] = '" <> pageText pageOwned <> "' },"
            , "  byPage = { craft_bill = {}, ground_item = {"
            , "    ['" <> pageText pageOwned <> "'] = {} },"
            , "    location_instance = {} },"
            , "  activePage = '" <> pageText pageActive <> "' };"
            , "local n = RECON.scrubStaleRefs(1, CORE.aiState[1], ctx,"
            , "                               RECON.DROP_HOOKS);"
            , "return tostring(n) .. '|'"
            , "  .. tostring(CORE.aiState[1].repairJob)"
            , "  .. '|' .. tostring(UNITAI.getRepairClaimant(741))" ]
        r `shouldBe` q "1|nil|nil"
        groundRows (scOwned scene) `shouldReturn` before
        invOf env workerUid `shouldReturn` []
