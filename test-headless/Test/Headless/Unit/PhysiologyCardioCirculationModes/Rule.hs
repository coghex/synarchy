{-# LANGUAGE Strict, OverloadedStrings #-}
-- | The on-demand circulation rule and the blood read it uses (#2836, S9;
--   design S9.2, S9.3, S10.2; T-S9-2): the shared Haskell rule against an
--   INDEPENDENT reference of the pre-change @circulation.compute@ formula,
--   through the pure read and through the installed Lua verbs, and its
--   coherence the instant an input changes, with no tick.
--
--   The reference below is written out here from the formula the old Lua
--   function had. It shares no code with "Unit.Physiology.Circulation": a
--   test that compared the production rule to itself, or the Lua wrapper to
--   the Haskell function it now calls, would prove nothing.
module Test.Headless.Unit.PhysiologyCardioCirculationModes.Rule (spec) where

import UPrelude
import Test.Hspec
import qualified Data.HashMap.Strict as HM
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import Engine.Scripting.Lua.Types (LuaBackendState)
import Test.Headless.Unit.PhysiologyFixture (readShippedPhysiologyRegistry)
import Test.Headless.Unit.PhysiologyModes.Harness
import Unit.Blood
import Unit.Physiology
import Unit.Physiology.Types
import Unit.Types

spec ∷ Spec
spec = describe "the on-demand circulation rule and the blood read" $ do
    referenceSpec
    wrapperSpec
    freshnessSpec
    bloodSpec

-----------------------------------------------------------------------
-- The independent reference (the old Lua formula, in Double)
-----------------------------------------------------------------------

clampD ∷ Double → Double → Double → Double
clampD lo hi = max lo ∘ min hi

-- | @circulation.compute@ as it was: blood fraction, constitution, fat mass,
--   body mass, sepsis, core temperature and the heart rate (Nothing = a
--   Nominal cardio's factor of exactly 1).
reference ∷ Double → Double → Double → Double → Double → Double → Maybe Double
          → Double
reference bloodFrac con fatM bodyM sepsis core mhr =
    clampD 0.05 1.0 (bloodFactor * fitness * sickness * vaso * hrFactor)
  where
    bloodFactor = clampD 0.1 1.0 (bloodFrac * 1.1)
    fatFrac = if bodyM > 0 then fatM / bodyM else 0.2
    fitness = clampD 0.3 1.1
        ((0.5 + 0.5 * con) * (1 - 0.7 * max 0 (fatFrac - 0.20)))
    sickness = clampD 0.4 1.0 (1 - 0.5 * sepsis)
    vaso = clampD 0.45 1.0 (1 - 0.06 * max 0 (36.0 - core))
    hrFactor = maybe 1.0 (\hr → clampD 0.5 1.0 (hr / 70)) mhr

-- | One grid point: the unit's stored inputs.
data Point = Point
    { ptBlood ∷ Double, ptCon ∷ Double, ptFat ∷ Double, ptSepsis ∷ Double
    , ptCore ∷ Double, ptHr ∷ Double }

-- | Spanning every clamp boundary: blood below the floor, mid, and past full
--   (1.1 x 0.91 > 1); constitution from frail to strong; fat fractions
--   either side of the 0.20 onset; sepsis none to total; core either side of
--   the 36 degree onset and below the 0.45 floor; heart rate below the 0.5
--   floor, at it, between, at resting and above.
grid ∷ [Point]
grid = [ Point b c f s k h
       | b ← [0, 0.1, 0.3, 0.5, 0.8, 0.91, 1.0]
       , c ← [0.1, 0.5, 1.0, 1.8]
       , f ← [0.05, 0.2, 0.3, 0.55]
       , s ← [0, 0.3, 0.8, 1.0]
       , k ← [26, 30, 34, 35.5, 36, 37, 38.5]
       , h ← [10, 30, 35, 50, 60, 70, 85, 150] ]

bodyMassKg ∷ Double
bodyMassKg = 70

realToD ∷ Float → Double
realToD = realToFrac

-- | A pure context for one point: thermo and cardio Simulated, blood at the
--   point's fraction of the unmodified maximum.
contextOf ∷ PhysiologyRegistry → Maybe Double → Point → PhysioContext
contextOf reg mhr pt = PhysioContext
    { pcRegistry = reg
    , pcPhysiology = plainUnitPhysiology True Nothing $ HM.fromList
        [ ("thermo", PhysioSimulated)
        , ("cardio", maybe PhysioNominal (const PhysioSimulated) mhr) ]
    , pcOverrides = HM.empty, pcNow = 0
    , pcStats = stats, pcModifiers = HM.empty
    , pcBlood = bloodViewOf stats volume 0 }
  where
    stats = HM.fromList $
        [ ("body_mass", realToFrac bodyMassKg)
        , ("fat_mass", realToFrac (ptFat pt * bodyMassKg))
        , ("constitution", realToFrac (ptCon pt))
        , ("sepsis", realToFrac (ptSepsis pt))
        , ("core_temp", realToFrac (ptCore pt)) ]
        ⧺ [ ("heart_rate", realToFrac h) | Just h ← [mhr] ]
    volume = realToFrac (ptBlood pt * bodyMassKg * 0.075)

circulationOf ∷ PhysioContext → Maybe Double
circulationOf ctx = case physioReadPure ReadEffective ctx "circulation" of
    OutcomeValue _ v → Just (realToD v)
    _                → Nothing

referenceSpec ∷ Spec
referenceSpec = beforeAll readShippedPhysiologyRegistry $
    describe "against an independent reference of the old formula (T-S9-2)" $ do

    it "equals the reference across a pinned grid spanning every clamp \
       \boundary, with a Simulated heart rate" $ \reg → do
        let bad = [ (ptBlood p, ptCon p, ptFat p, ptSepsis p, ptCore p, ptHr p, got, want)
                  | p ← grid
                  , let want = reference (ptBlood p) (ptCon p)
                                  (ptFat p * bodyMassKg) bodyMassKg (ptSepsis p)
                                  (ptCore p) (Just (ptHr p))
                  , let got = circulationOf (contextOf reg (Just (ptHr p)) p)
                  , maybe True (\g → abs (g - want) > 2e-5) got
                  ]
        length grid `shouldBe` 7 * 4 * 4 * 4 * 7 * 8
        take 3 bad `shouldBe` []

    it "equals the reference with the heart-rate factor exactly 1 under a \
       \Nominal cardio, every OTHER factor live" $ \reg → do
        let pts = [ p | p ← grid, ptHr p ≡ 70 ]
            bad = [ (ptBlood p, ptCon p, ptFat p, ptSepsis p, ptCore p, got, want)
                  | p ← pts
                  , let want = reference (ptBlood p) (ptCon p)
                                  (ptFat p * bodyMassKg) bodyMassKg (ptSepsis p)
                                  (ptCore p) Nothing
                  , let got = circulationOf (contextOf reg Nothing p)
                  , maybe True (\g → abs (g - want) > 2e-5) got
                  ]
        take 3 bad `shouldBe` []
        -- live, not a constant: the same inputs with blood gone read low
        let hurt = Point 0.1 1.0 0.2 0 37 70
        fmap (< 0.2) (circulationOf (contextOf reg Nothing hurt))
            `shouldBe` Just True
        -- and a Nominal read differs from a Simulated slow heart's, by
        -- exactly that heart's factor
        let slow = Point 0.5 1.0 0.2 0 37 35
        (/) <$> circulationOf (contextOf reg (Just 35) slow)
            <*> circulationOf (contextOf reg Nothing slow)
            `shouldSatisfy` maybe False (\r → abs (r - 0.5) < 1e-5)

    it "takes the thermo substitute 37 under a Nominal thermo, the resting \
       \heart-rate divisor from the registry, and the old fallbacks for a \
       \missing constitution, fat mass, body mass or sepsis" $ \reg → do
        let base = contextOf reg (Just 70) (Point 1 1 0.2 0 37 70)
            nominalThermo = base
                { pcPhysiology = plainUnitPhysiology True Nothing $ HM.fromList
                    [("thermo", PhysioNominal), ("cardio", PhysioSimulated)]
                , pcStats = HM.delete "core_temp" (pcStats base) }
        -- a cold stored core is ignored (there is none): 37 degrees
        circulationOf nominalThermo `shouldBe` circulationOf base
        -- the divisor is the registry's constant: halve it and the same
        -- heart reads as a fast one
        let slow = contextOf reg (Just 35) (Point 1 1 0.2 0 37 35)
            halved = slow { pcRegistry = reg
                { prConstants = Map.insert "RESTING_HR" 35 (prConstants reg) } }
        (/) <$> circulationOf halved <*> circulationOf slow
            `shouldSatisfy` maybe False (\r → abs (r - 2) < 1e-5)
        let lazyStats = foldl' (flip HM.delete) (pcStats base)
                ["constitution", "fat_mass", "body_mass", "sepsis"]
            want = reference 1 1.0 0 70 0 37 (Just 70)
        fmap (\g → abs (g - want) < 2e-5)
             (circulationOf base { pcStats = lazyStats })
            `shouldBe` Just True

    it "raises an invariant error for a Simulated heart rate or core that is \
       \missing, never a fallback" $ \reg → do
        let ctx = contextOf reg (Just 70) (Point 1 1 0.2 0 37 70)
        forM_ ["heart_rate", "core_temp"] $ \k →
            case physioReadPure ReadEffective
                     ctx { pcStats = HM.delete k (pcStats ctx) } "circulation" of
                OutcomeInvariant why → T.unpack why `shouldContain` T.unpack k
                other → expectationFailure ("expected an invariant, got " <> show other)
        -- a cardio the definition lacks the capability for is Unsupported
        physioReadPure ReadEffective
            ctx { pcPhysiology = noUnitPhysiology } "circulation"
            `shouldBe` OutcomeUnsupported

-----------------------------------------------------------------------
-- Through the installed Lua verbs
-----------------------------------------------------------------------

-- | A unit at one grid point: thermo and cardio Simulated (the @rich@
--   profile), the point's stored inputs, blood at the point's fraction.
unitAt ∷ Int → Point → UnitSpec
unitAt uid pt = (unitSpec uid "acolyte" (Just "rich"))
    { usStats = [ ("constitution", realToFrac (ptCon pt))
                , ("fat_mass", realToFrac (ptFat pt * bodyMassKg))
                , ("sepsis", realToFrac (ptSepsis pt))
                , ("core_temp", realToFrac (ptCore pt))
                , ("heart_rate", realToFrac (ptHr pt)) ]
    , usBlood = Just (realToFrac (ptBlood pt * bodyMassKg * 0.075)) }

circulationNow ∷ LuaBackendState → Int → IO Double
circulationNow ls uid = luaNum ls
    ("unit.readPhysio(" <> tshow uid <> ", 'circulation')")

wrapperSpec ∷ Spec
wrapperSpec = describe "through unit.readPhysio and the Lua readers" $ do
    it "answers the reference for a spread of units, as unit.readPhysio, the \
       \facade, circulation.get and circulation.compute" $ do
        let pts = [ p | (i, p) ← zip [0 ∷ Int ..] grid, i `mod` 997 ≡ 3 ]
            units = zipWith unitAt [1 ..] pts
        length pts `shouldSatisfy` (≥ 20)
        withScene units $ \(_, ls) → forM_ (zip [1 ∷ Int ..] pts) $ \(uid, p) → do
            let want = reference (ptBlood p) (ptCon p) (ptFat p * bodyMassKg)
                           bodyMassKg (ptSepsis p) (ptCore p) (Just (ptHr p))
                u = tshow uid
            circulationNow ls uid >>= near 2e-5 want
            luaNum ls ("__physiology.get(" <> u <> ", 'circulation')")
                >>= near 2e-5 want
            luaNum ls ("require('scripts.circulation').get(" <> u <> ")")
                >>= near 2e-5 want
            luaNum ls ("require('scripts.circulation').compute(" <> u <> ", 12)")
                >>= near 2e-5 want

    it "stores nothing, rolls nothing and refuses a raw write, in either mode" $
        withScene [ unitAt 1 (Point 1 1 0.2 0 37 70)
                  , (unitAt 2 (Point 1 1 0.2 0 37 70))
                        { usOverrides = [("cardio", PhysioNominal)] } ] $
            \(env, ls) → do
                _ ← luaText ls "_G.__rand = 0; local r = math.random; \
                               \math.random = function(...) __rand = __rand + 1; \
                               \return r(...) end; return true"
                before ← mapM (readInstance env) [1, 2]
                _ ← luaText ls "for _ = 1, 200 do for u = 1, 2 do \
                               \unit.readPhysio(u, 'circulation'); \
                               \unit.readPhysioBase(u, 'circulation'); \
                               \__physiology.get(u, 'circulation') end end; \
                               \return true"
                after ← mapM (readInstance env) [1, 2]
                map uiStats after `shouldBe` map uiStats before
                map uiPhysioOverrides after `shouldBe` map uiPhysioOverrides before
                luaText ls "return __rand" `shouldReturn` "0"
                -- the base read of a derived key is the same on-demand value
                luaNum ls "unit.readPhysioBase(1, 'circulation')"
                    >>= \b → circulationNow ls 1 >>= near 1e-9 b
                forM_ [1, 2 ∷ Int] $ \u → do
                    luaText ls ("return tostring(unit.setStat(" <> tshow u
                                <> ", 'circulation', 0.1))") `shouldReturn` "false"
                    i ← readInstance env u
                    HM.member "circulation" (uiStats i) `shouldBe` False
                -- mode names the right mode; a Nominal cardio is not
                -- erased: this unit is healthy, so 1.0, and the next test
                -- shows it moving
                readOwned ls 1 "circulation" >>= (`shouldSatisfy` T.isSuffixOf "|simulated")
                readOwned ls 2 "circulation" >>= (`shouldSatisfy` T.isSuffixOf "|nominal")

-----------------------------------------------------------------------
-- Coherent immediately, with no tick
-----------------------------------------------------------------------

-- | The unit's inputs as the next reads must see them.
data Inputs = Inputs
    { inBlood ∷ Double, inCon ∷ Double, inFat ∷ Double, inBody ∷ Double
    , inSepsis ∷ Double, inCore ∷ Double, inHr ∷ Double }

healthyInputs ∷ Inputs
healthyInputs = Inputs 1 1 14 70 0 37 70

wantNow ∷ Inputs → Double
wantNow i = reference (inBlood i) (inCon i) (inFat i) (inBody i) (inSepsis i)
                      (inCore i) (Just (inHr i))

freshnessSpec ∷ Spec
freshnessSpec = describe "a change to any input is read at once, with no tick" $ do
    it "follows blood, core temperature, heart rate, body composition and a \
       \constitution modifier's expiry" $
        withScene [ (unitSpec 1 "acolyte" (Just "rich"))
                        { usStats = [("fat_mass", 14), ("sepsis", 0)] } ] $
            \(env, ls) → do
                let read1 = circulationNow ls 1
                    expectNow i = read1 >>= near 2e-5 (wantNow i)
                expectNow healthyInputs
                -- blood: the engine's own field, as a bleeding tick leaves it
                modifyInstance env 1 (\u → u { uiBlood = 0.3 * 5.25 })
                let i1 = healthyInputs { inBlood = 0.3 }
                expectNow i1
                -- core temperature and heart rate: Simulated owned keys
                _ ← luaText ls "unit.setStat(1, 'core_temp', 31); return true"
                let i2 = i1 { inCore = 31 }
                expectNow i2
                _ ← luaText ls "unit.setStat(1, 'heart_rate', 35); return true"
                let i3 = i2 { inHr = 35 }
                expectNow i3
                -- body composition: the fat share past its onset
                _ ← luaText ls "unit.setStat(1, 'fat_mass', 28); return true"
                let i4 = i3 { inFat = 28 }
                expectNow i4
                -- an infection
                _ ← luaText ls "unit.setStat(1, 'sepsis', 0.6); return true"
                let i5 = i4 { inSepsis = 0.6 }
                expectNow i5
                -- a constitution modifier, then its expiry
                _ ← luaText ls "unit.addModifier(1, 'constitution', -0.5, \
                               \'frail', 10, 0); return true"
                let i6 = i5 { inCon = 0.5 }
                expectNow i6
                writeGameTime env 11
                expectNow i5
                -- nothing was cached or stored along the way
                inst ← readInstance env 1
                HM.member "circulation" (uiStats inst) `shouldBe` False

    it "uses the EFFECTIVE body mass for the fat share but the BASE body mass \
       \for the blood maximum, so a body-mass modifier never moves the \
       \maximum" $
        withScene [ (unitSpec 1 "acolyte" (Just "rich"))
                        { usModifiers = [("body_mass", [StatModifier 0 "spec" Nothing 1.0])]
                        , usStats = [("fat_mass", 28)]
                        , usBlood = Just (0.5 * 5.25) } ] $ \(_, ls) → do
            -- effective body mass 140: the fat share is 0.2, not 0.4
            circulationNow ls 1 >>= near 2e-5
                (reference 0.5 1 28 140 0 37 (Just 70))
            luaNum ls "unit.readBloodView(1).max" >>= near 1e-6 5.25
            luaNum ls "unit.readBloodView(1).fraction" >>= near 1e-6 0.5

-----------------------------------------------------------------------
-- The blood read (the unmoded bloodView)
-----------------------------------------------------------------------

bloodSpec ∷ Spec
bloodSpec = describe "the unmoded bloodView (S10.2 before S10)" $ do
    it "is exactly the raw stored volume over base body mass x the ratio, \
       \clamped, with the old 70 kg fallback and a fraction of 1 when the \
       \maximum is not positive" $ do
        let at ms vol = bloodViewOf (HM.fromList ms) vol 0
        bvVolume (at [("body_mass", 70)] 2.1) `shouldBe` 2.1
        bvMax (at [("body_mass", 70)] 2.1) `shouldBe` 70 * 0.075
        bvFraction (at [("body_mass", 70)] 2.625) `shouldBe` 0.5
        bvFraction (at [("body_mass", 70)] 9) `shouldBe` 1
        bvFraction (at [("body_mass", 70)] (-1)) `shouldBe` 0
        -- no body mass: the 70 kg fallback, and a bodyless unit's 0 L seed
        bvMax (at [] 0) `shouldBe` 70 * 0.075
        bvFraction (at [] 0) `shouldBe` 0
        -- a non-positive mass has no maximum to divide by
        bvFraction (at [("body_mass", 0)] 3) `shouldBe` 1
        bvFraction (at [("body_mass", -5)] 3) `shouldBe` 1

    it "carries the live wound bleed rate, lazily, and a full-blood unit's \
       \view is the future Nominal one (fraction 1.0)" $ do
        let stats = HM.fromList [("body_mass", 70), ("constitution", 1)]
            full = fullBloodView stats
        bvFraction full `shouldBe` 1
        bvVolume full `shouldBe` bvMax full
        bvBleedRate full `shouldBe` 0

    it "leaves unit.getBlood's record and values untouched, and reads the \
       \same numbers through unit.readBloodView" $
        withScene [ (unitSpec 1 "acolyte" (Just "rich")) { usBlood = Just 3.0 }
                  , (unitSpec 2 "white_tailed_deer" Nothing)
                        { usDropStats = ["body_mass"], usBlood = Just 0 } ] $
            \(_, ls) → do
                luaText ls "local n = 0; for _ in pairs(unit.getBlood(1)) do \
                           \n = n + 1 end; return n" `shouldReturn` "3"
                forM_ [1, 2 ∷ Int] $ \u → do
                    let b = "unit.getBlood(" <> tshow u <> ")"
                        v = "unit.readBloodView(" <> tshow u <> ")"
                    luaNum ls (v <> ".volume") >>= \x → luaNum ls (b <> ".current")
                        >>= near 1e-9 x
                    luaNum ls (v <> ".max") >>= \x → luaNum ls (b <> ".max")
                        >>= near 1e-9 x
                    luaNum ls (v <> ".bleedRate") >>= \x → luaNum ls (b <> ".bleedRate")
                        >>= near 1e-9 x
                luaNum ls "unit.readBloodView(1).fraction" >>= near 1e-6 (3 / 5.25)
                -- the bodyless seed: 0 L against the 70 kg fallback maximum
                luaNum ls "unit.readBloodView(2).max" >>= near 1e-6 5.25
                luaNum ls "unit.readBloodView(2).fraction" >>= near 1e-9 0
                luaText ls "return tostring(unit.readBloodView(99))" `shouldReturn` "nil"
