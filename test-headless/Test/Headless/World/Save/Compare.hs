{-# LANGUAGE Strict, OverloadedStrings #-}
-- | The @save generation comparison@ gate (#2936): what
--   @synarchy-save-codec compare@, and through it the persistence probes'
--   "all generations are structurally IDENTICAL" check, counts as a
--   difference between a live capture and the saves taken across loads.
--
--   The incident: the sweep saved a live session whose attacker the Lua
--   combat AI held in a @combat_idle_RH_dagger@ override stance, loaded it,
--   and re-saved. The override is runtime-only and cleared by every load, so
--   the re-save recorded the state animation (@idle@) and the strict
--   comparison reported @COMPARE_MISMATCH@ for every re-save, whenever the
--   save happened to land in a stance. Pure: no engine, no codec process.
module Test.Headless.World.Save.Compare (spec) where

import UPrelude
import Test.Hspec
import qualified Data.HashMap.Strict as HM
import Test.Headless.World.Save.Components.Fixture (page1, richSnapshot)
import Unit.Types (UnitId(..))
import World.Save.Compare (divergentGenerations, withoutOverrideDrivenPlayback)
import World.Save.Physiology (withoutRebuiltCaches)
import World.Save.Snapshot (PageSnapshot(..), SessionSnapshot(..))
import World.Save.Types (UnitInstanceSnapshot(..), UnitSnapshot(..))

-- | The fixture session with its one unit rewritten.
withUnit ∷ (UnitInstanceSnapshot → UnitInstanceSnapshot) → SessionSnapshot → SessionSnapshot
withUnit f snap = snap { snapPages = HM.adjust onPage page1 (snapPages snap) }
  where
    onPage pg = pg { pgsUnits = (pgsUnits pg)
                       { usnInstances = HM.adjust f (UnitId 1) (usnInstances (pgsUnits pg)) } }

-- | The unit as the live session had it: in a Lua combat-idle override
--   stance, playback begun at game time 47.16.
liveStance ∷ UnitInstanceSnapshot → UnitInstanceSnapshot
liveStance u = u { uisCurrentAnim = "combat_idle_RH_dagger", uisAnimStart = 47.16 }

-- | The unit as every load resolves it: the override is gone, so the state
--   animation plays, restarted at the loaded game time.
loaded ∷ UnitInstanceSnapshot → UnitInstanceSnapshot
loaded u = u { uisCurrentAnim = "idle", uisAnimStart = 47.5 }

spec ∷ Spec
spec = describe "save generation comparison (#2936)" $ do
    let base = richSnapshot
        live = withUnit liveStance base
        gen2 = withUnit loaded base
        gen3 = gen2
        gen4 = gen2

    it "the incident: a live override stance followed by three identical re-saves \
       \is a fixed point, though the strict comparison it replaces reports a mismatch" $ do
        withoutRebuiltCaches live `shouldNotBe` withoutRebuiltCaches gen2
        divergentGenerations [live, gen2, gen3, gen4] `shouldBe` []

    it "excuses only the two override-driven playback fields" $ do
        withoutOverrideDrivenPlayback live `shouldBe` withoutOverrideDrivenPlayback gen2
        withoutOverrideDrivenPlayback (withUnit (\u → (liveStance u) { uisGridY = 9 }) base)
            `shouldNotBe` withoutOverrideDrivenPlayback live

    it "still rejects a durable difference between the live capture and a re-save" $ do
        let moved  = withUnit (\u → (loaded u) { uisGridX = 3 }) base
            hurt   = withUnit (\u → (loaded u) { uisStats = HM.insert "health" 1 (uisStats u) }) base
            sim    = withUnit (\u → (loaded u) { uisActivity = "walking" }) base
            stride = withUnit (\u → (loaded u) { uisAnimStride = 2 }) base
        forM_ [("position", moved), ("a stat", hurt), ("the sim activity mirror", sim)
              , ("the sim stride mirror", stride)] $ \(_what, bad) →
            divergentGenerations [live, bad, bad, bad] `shouldBe` [1, 2, 3]

    it "keeps the playback fields EXACT between loaded generations: a load that \
       \is not a fixed point still fails" $ do
        let drift = withUnit (\u → u { uisCurrentAnim = "walk" }) base
            restart = withUnit (\u → u { uisAnimStart = 99 }) base
        divergentGenerations [live, gen2, drift, gen4] `shouldBe` [2]
        divergentGenerations [live, gen2, gen3, restart] `shouldBe` [3]

    it "still excuses the rebuilt physiology caches between loaded generations" $ do
        let rebuilt = withUnit (\u → (loaded u)
                          { uisStats = HM.insert "max_hydration" 123 (uisStats u) }) base
        divergentGenerations [live, gen2, rebuilt] `shouldBe` []

    it "a chain with nothing to compare diverges nowhere" $ do
        divergentGenerations [] `shouldBe` []
        divergentGenerations [live] `shouldBe` []
