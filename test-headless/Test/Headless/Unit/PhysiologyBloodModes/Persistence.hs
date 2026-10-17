{-# LANGUAGE Strict, OverloadedStrings #-}
-- | Persistence of the blood modes (#2837; design D-6, I6): the dedicated
--   @uiBlood@ field and the units-v4 override map through the REAL session
--   codec and the real load gate.
--
--   No wire type is added: @uisBlood@ and the override map are unchanged.
--   What is established is that a Simulated unit's partial, zero and full
--   volumes and its pins round-trip exactly with no normalization; that a
--   Nominal unit's stored volume is serialized but IGNORED and rebuilt from
--   the current base body mass silently (a unit saved by current code whose
--   body mass moved afterwards normalizes ZERO times, and a second load
--   finds the field canonical); that a legacy shadow @blood@ stat is scrubbed
--   once and logged; that the paused load reads the canonical volume; and
--   that the transient trail accumulator never survives a load.
module Test.Headless.Unit.PhysiologyBloodModes.Persistence (spec) where

import UPrelude
import Test.Hspec
import Engine.Core.State (EngineEnv)
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import Test.Headless.Unit.PhysiologyFixture (readShippedPhysiologyRegistry)
import Test.Headless.Unit.PhysiologyModes.Harness
import Test.Headless.Unit.PhysiologySpeciesProfiles
    ( codecRoundTrip, gate, onSavePage, pageSnapshotOf, placeUnits
    , shippedDefs, withShipped )
import Unit.Physiology (bloodSystem, resolveMode)
import Unit.Physiology.Types
import Unit.Types
import Equipment.Types (emptyEquipmentClassManager)
import Infection.Types (emptyInfectionManager)
import Item.Types (emptyItemManager)
import Test.Headless.World.Save.Components.Fixture (minimalGlobals, page1)
import World.Save.Physiology (PhysiologyNormalization(..), physiologyLoadGate, withoutRebuiltCaches)
import World.Save.Snapshot (PageSnapshot(..), captureSessionSnapshot)
import World.Save.Types (UnitInstanceSnapshot(..), UnitSnapshot(..), fromUnitSnapshot)

spec ∷ Spec
spec = describe "persistence of the blood modes" $ do
    roundTripSpec
    nominalSpec
    legacySpec
    malformedSpec
    pausedLoadSpec

maxBlood ∷ Float
maxBlood = 5.25

setBlood ∷ EngineEnv → Int → Float → IO ()
setBlood env u v = modifyInstance env u (\i → i { uiBlood = v })

-----------------------------------------------------------------------
-- Simulated: exact
-----------------------------------------------------------------------

roundTripSpec ∷ Spec
roundTripSpec = describe "Simulated volume and pins (I6)" $
    it "round-trips partial, zero and full volumes and every pin, including \
       \one equal to the species default, exactly, with no normalization, and \
       \a second pass is a no-op" $
        withShipped $ \(env, _) → do
            reg ← readShippedPhysiologyRegistry
            defs ← shippedDefs env
            placeUnits env
                [ (1, "acolyte", Nothing, [], [])
                , (2, "red_squirrel", Nothing, [("blood", PhysioSimulated)], [])
                , (3, "bear_brown", Nothing, [], [])
                , (4, "nomad_primitive", Nothing, [("blood", PhysioNominal)], []) ]
            setBlood env 1 2.25
            setBlood env 2 0
            setBlood env 3 maxBlood
            onSavePage env [1, 2, 3, 4]
            before ← mapM (readInstance env) [1, 2, 3, 4]
            pg ← pageSnapshotOf env ≫= codecRoundTrip
            (us, notes) ← gate reg defs (pgsUnits pg)
            notes `shouldBe` []
            let got i = fmap uisBlood (HM.lookup (UnitId i) (usnInstances us))
            got 1 `shouldBe` Just 2.25
            got 2 `shouldBe` Just 0
            got 3 `shouldBe` Just maxBlood
            forM_ [1, 2, 3, 4] $ \i → case HM.lookup (UnitId i) (usnInstances us) of
                Nothing → expectationFailure "a unit vanished"
                Just u → do
                    uisPhysioOverrides u `shouldBe` uiPhysioOverrides (before !! (fromIntegral i - 1))
                    HM.member "blood" (uisStats u) `shouldBe` False
            (us', notes') ← gate reg defs us
            (us', notes') `shouldBe` (us, [])

-----------------------------------------------------------------------
-- Nominal: serialized but ignored, rebuilt silently
-----------------------------------------------------------------------

nominalSpec ∷ Spec
nominalSpec = describe "a Nominal stored volume (serialized but ignored)" $ do
    it "is rebuilt from the CURRENT base body mass with no logged repair, even \
       \when the body mass moved after the field was written, and a second \
       \load finds it canonical" $
        withShipped $ \(env, _) → do
            reg ← readShippedPhysiologyRegistry
            defs ← shippedDefs env
            placeUnits env [(1, "acolyte", Nothing, [("blood", PhysioNominal)], [])]
            -- a stale partial volume, then a body-mass gain (70 -> 140 kg)
            setBlood env 1 0.7
            modifyInstance env 1 (\i → i { uiStats = HM.insert "body_mass" 140 (uiStats i) })
            onSavePage env [1]
            pg ← pageSnapshotOf env ≫= codecRoundTrip
            (us1, notes1) ← gate reg defs (pgsUnits pg)
            notes1 `shouldBe` []
            fmap uisBlood (HM.lookup (UnitId 1) (usnInstances us1))
                `shouldBe` Just (140 * 0.075)
            (us2, notes2) ← gate reg defs us1
            (us2, notes2) `shouldBe` (us1, [])

    it "compares equal across a load that rebuilds it (the real save comparator), \
       \while a Simulated twin's changed volume still differs" $
        withShipped $ \(env, _) → do
            reg ← readShippedPhysiologyRegistry
            defs ← shippedDefs env
            placeUnits env
                [ (1, "acolyte", Nothing, [("blood", PhysioNominal)], [])
                , (2, "acolyte", Nothing, [], []) ]
            forM_ [1, 2] $ \u → modifyInstance env u (\i →
                i { uiStats = HM.insert "body_mass" 140 (uiStats i) })
            setBlood env 2 0.7
            onSavePage env [1, 2]
            pg ← pageSnapshotOf env ≫= codecRoundTrip
            (us1, _) ← gate reg defs (pgsUnits pg)
            let durable p = fmap withoutRebuiltCaches
                                 (captureSessionSnapshot minimalGlobals [p])
                loaded = pg { pgsUnits = us1 }
                drifted = pg { pgsUnits = (pgsUnits pg)
                                 { usnInstances = HM.adjust (\i → i { uisBlood = 0.8 })
                                       (UnitId 2) (usnInstances (pgsUnits pg)) } }
            -- stored 5.25 (pinned at 70 kg), rebuilt 10.5 after the mass doubled
            fmap uisBlood (HM.lookup (UnitId 1) (usnInstances (pgsUnits pg))) `shouldBe` Just 5.25
            fmap uisBlood (HM.lookup (UnitId 1) (usnInstances us1)) `shouldBe` Just 10.5
            fmap uisBlood (HM.lookup (UnitId 2) (usnInstances us1)) `shouldBe` Just 0.7
            durable pg `shouldBe` durable loaded
            -- a Simulated unit's volume is durable: the same comparison tells
            -- a changed one apart
            durable pg `shouldNotBe` durable drifted

    it "is pinned only: no shipped definition defaults blood to Nominal, so the \
       \registry-free save comparator (which sees only a pin) covers every shipped \
       \Nominal blood" $
        withShipped $ \(env, _) → do
            reg ← readShippedPhysiologyRegistry
            defs ← shippedDefs env
            let sys = bloodSystem reg
            isJust sys `shouldBe` True
            [ name | (name, d) ← HM.toList defs, Just s ← [sys]
                   , resolveMode s (udPhysiology d) False Nothing ≡ ModeNominal ]
                `shouldBe` []

    it "never leaks the stale stored value into a read, before or after a load" $
        withScene [ (unitSpec 1 "acolyte" (Just "exempt"))
                      { usBlood = Just 0.7, usOverrides = [("blood", PhysioNominal)] } ] $
            \(_, ls) → do
                readOwned ls 1 "blood" `shouldReturn` "5.25|nominal"
                luaNum ls "unit.getBlood(1).current" >>= near 1e-5 5.25

-----------------------------------------------------------------------
-- Legacy shadow state
-----------------------------------------------------------------------

legacySpec ∷ Spec
legacySpec = describe "a legacy shadow blood stat" $
    it "is scrubbed in every mode, logged once, and a re-save and reload \
       \performs zero further repairs; the stored volume is untouched" $
        withShipped $ \(env, _) → do
            reg ← readShippedPhysiologyRegistry
            defs ← shippedDefs env
            placeUnits env
                [ (1, "acolyte", Nothing, [], [])
                , (2, "acolyte", Nothing, [("blood", PhysioNominal)], [])
                , (3, "white_tailed_deer", Nothing, [], []) ]
            setBlood env 1 2.0
            forM_ [1, 2, 3] $ \u → modifyInstance env u $ \i →
                i { uiStats = HM.insert "blood" 3.3 (uiStats i) }
            onSavePage env [1, 2, 3]
            pg ← pageSnapshotOf env ≫= codecRoundTrip
            (us1, notes1) ← gate reg defs (pgsUnits pg)
            length notes1 `shouldBe` 3
            forM_ [ T.unwords ns | PhysiologyNormalization _ _ _ ns ← notes1 ] $ \n →
                T.isInfixOf "blood" n `shouldBe` True
            forM_ [1, 2, 3] $ \u → case HM.lookup (UnitId u) (usnInstances us1) of
                Nothing → expectationFailure "the unit vanished"
                Just i → HM.member "blood" (uisStats i) `shouldBe` False
            fmap uisBlood (HM.lookup (UnitId 1) (usnInstances us1)) `shouldBe` Just 2.0
            (us2, notes2) ← gate reg defs us1
            (us2, notes2) `shouldBe` (us1, [])

-----------------------------------------------------------------------
-- Storage-aware validation of the dedicated field
-----------------------------------------------------------------------

malformedSpec ∷ Spec
malformedSpec = describe "a malformed dedicated volume" $
    it "rejects a non-finite Simulated uisBlood before publication, accepts and \
       \rewrites a non-finite NOMINAL one (it is ignored), and scrubs a stale \
       \non-finite shadow blood stat beside a valid dedicated volume" $
        withShipped $ \(env, _) → do
            reg ← readShippedPhysiologyRegistry
            defs ← shippedDefs env
            placeUnits env
                [ (1, "acolyte", Nothing, [], [])
                , (2, "acolyte", Nothing, [("blood", PhysioNominal)], [])
                , (3, "acolyte", Nothing, [], [])
                , (4, "acolyte", Nothing, [("blood", PhysioNominal)], []) ]
            setBlood env 1 (0 / 0)
            setBlood env 2 (1 / 0)
            modifyInstance env 3 $ \i →
                i { uiStats = HM.insert "blood" (0 / 0) (uiStats i) }
            modifyInstance env 4 $ \i →
                i { uiStats = HM.insert "body_mass" (1 / 0) (uiStats i) }
            onSavePage env [1, 2, 3, 4]
            pg ← pageSnapshotOf env
            let units = pgsUnits pg
                only keep = units { usnInstances = HM.filterWithKey
                                      (\k _ → k ≡ UnitId keep) (usnInstances units) }
                loadOf u = physiologyLoadGate reg (HM.map udPhysiology defs) 0
                               [(page1, only u)]
            -- a Simulated unit with a NaN dedicated volume is rejected
            case loadOf 1 of
                Left msgs → any (T.isInfixOf "blood") msgs `shouldBe` True
                Right _ → expectationFailure "a non-finite Simulated blood passed the gate"
            -- a Nominal one is ignored and rewritten to the canonical maximum
            case loadOf 2 of
                Right ([(_, us)], notes) → do
                    notes `shouldBe` []
                    fmap uisBlood (HM.lookup (UnitId 2) (usnInstances us)) `shouldBe` Just 5.25
                other → expectationFailure ("Nominal NaN: " <> show (fmap (const ()) other))
            -- a Nominal unit whose base body mass is non-finite has no canonical
            -- maximum to rewrite to: rejected, not rebuilt to infinity
            case loadOf 4 of
                Left msgs → any (T.isInfixOf "blood") msgs `shouldBe` True
                Right _ → expectationFailure "a non-finite Nominal target loaded"
            -- a valid dedicated volume with a stale non-finite shadow: scrubbed, loads
            case loadOf 3 of
                Right ([(_, us)], notes) → do
                    length notes `shouldBe` 1
                    fmap (HM.member "blood" ∘ uisStats) (HM.lookup (UnitId 3) (usnInstances us))
                        `shouldBe` Just False
                other → expectationFailure ("shadow NaN: " <> show (fmap (const ()) other))

-----------------------------------------------------------------------
-- The paused load, and transience
-----------------------------------------------------------------------

pausedLoadSpec ∷ Spec
pausedLoadSpec = describe "the paused load and transient state" $ do
    it "reads the canonical volume on the first paused observation, storing \
       \nothing, for both modes" $
        withScene
            [ (unitSpec 1 "acolyte" (Just "exempt")) { usBlood = Just 0.7
                  , usOverrides = [("blood", PhysioNominal)] }
            , (unitSpec 2 "acolyte" (Just "exempt")) { usBlood = Just 0.7 } ] $
            \(env, ls) → do
                _ ← luaText ls "__resources.onSaveLoaded({ 1, 2 }, {}); return true"
                luaNum ls "unit.getBlood(1).current" >>= near 1e-5 5.25
                luaNum ls "unit.getBlood(2).current" >>= near 1e-5 0.7
                forM_ [1, 2] $ \u → do
                    i ← readInstance env u
                    HM.member "blood" (uiStats i) `shouldBe` False

    it "drops the trail accumulator on a save, a real codec round trip and a \
       \load, under both modes: decals and accumulators are never persisted" $
        withShipped $ \(env, _) → do
            reg ← readShippedPhysiologyRegistry
            defs ← shippedDefs env
            placeUnits env
                [ (1, "acolyte", Nothing, [], [])
                , (2, "acolyte", Nothing, [("blood", PhysioNominal)], []) ]
            forM_ [1, 2] $ \u → modifyInstance env u $ \i →
                i { uiTrailState = Just emptyTrailState { tsPendingVolume = 0.4 } }
            onSavePage env [1, 2]
            pg ← pageSnapshotOf env ≫= codecRoundTrip
            (us, _) ← gate reg defs (pgsUnits pg)
            -- the load reconstructs every unit through the real snapshot
            -- mapping, which has no accumulator to restore
            let (um1, orphans, _, _, _) =
                    fromUnitSnapshot page1 defs emptyInfectionManager
                        emptyEquipmentClassManager emptyItemManager us
            orphans `shouldBe` []
            HM.size (umInstances um1) `shouldBe` 2
            forM_ (HM.elems (umInstances um1)) $ \i →
                uiTrailState i `shouldBe` Nothing
