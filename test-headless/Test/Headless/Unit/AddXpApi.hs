-- | #1733: the non-finite policy of the @unit.addXP@ verb, driven
--   through the REAL registered production API against the REAL unit
--   manager ref.
--
--   'Test.Headless.Unit.Stats''s @applySkillXP@ group covers the pure
--   growth formula; it structurally cannot see this verb's failure
--   shape or prove that a refused call mutated nothing, because the
--   guard lives at the Lua boundary rather than in the formula. Both
--   lookup branches are exercised: @unitAddXPFn@ writes @uiSkills@ and
--   @uiStats@ through separate arms, so a guard added to one only
--   would pass a skill-only test while still corrupting a stat. The stat
--   arm is exercised on an UNOWNED stat (@perception@): since #2838 (S11a)
--   @concentration@ is derived and never stored, so it is no longer an
--   addXP target, and the downstream chain the issue names (a non-finite
--   value reaching 'Combat.Resolution.Common.mentalEffectiveness') is
--   covered through the DERIVED path: a non-finite stored rule input.
--
--   Same bare-Lua-backend technique as
--   'Test.Headless.Unit.TransferApi', whose unit fixture primitives
--   this spec reuses; like that spec it WRITES the unit manager ref,
--   so 'Spec.hs' gives it its own @aroundAll withHeadlessEngine@.
--
--   Run just this gate: @cabal test synarchy-test-headless
--   --test-options='--match \"unit.addXP\"'@ (the describe also names
--   @applySkillXP@, so the issue's @--match \"applySkillXP\"@ command
--   reaches it too).
module Test.Headless.Unit.AddXpApi (spec) where

import UPrelude
import Test.Hspec
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import Data.IORef (readIORef, writeIORef)
import Engine.Core.State (EngineEnv(..))
import Engine.Scripting.Lua.Types (LuaBackendState)
import Test.Headless.Unit.PhysiologyFixture
    (ensurePhysiologyRegistry, mindSeedStats)
import Test.Headless.Unit.TransferApi
    (evalDebug, minimalDef, mkUnit, newBareLuaBackend)
import Unit.Physiology.Types (PhysioMode(..), plainUnitPhysiology)
import Unit.Faction (Faction(..))
import Unit.Stats (applySkillXP)
import Unit.Types
    (UnitDef(..), UnitId(..), UnitInstance(..), UnitManager(..), emptyUnitManager)

subjectUid ∷ UnitId
subjectUid = UnitId 1

-- The two names the verb's two lookup arms own. "smithing" is a skill,
-- "perception" an unowned stat (a registry-owned stat is a stored base whose
-- write the physiology gate governs; the derived brain values cannot be
-- written at all).
startingSkill, startingStat ∷ Float
startingSkill = 4.0
startingStat  = 0.5

-- | A single player acolyte carrying exactly one skill, one unowned stat and
--   a Simulated brain and mind (consciousness and mind Simulated, everything
--   else Nominal, the mind seeded). @caffeine@ is a parameter so the
--   containment cases below can seed a rule input that is ALREADY
--   non-finite — a corruption that reached the manager by some route other
--   than @unit.addXP@, which is exactly what requirement 2 has to contain.
resetUnitsWith ∷ EngineEnv → Float → IO ()
resetUnitsWith env caffeine = do
    ensurePhysiologyRegistry env
    writeIORef (unitManagerRef env) emptyUnitManager
        { umDefs = HM.singleton "acolyte"
            ((minimalDef "acolyte" "Acolyte")
                { udPhysiology = plainUnitPhysiology True Nothing $ HM.fromList
                    [ (s, if s `elem` ["consciousness", "mind"]
                             then PhysioSimulated else PhysioNominal)
                    | s ← ["thermo", "salt", "hydration", "nutrition"
                         , "stamina", "exhaustion", "sleep", "cardio"
                         , "blood", "consciousness", "mind"] ] })
        , umInstances = HM.singleton subjectUid
            ((mkUnit "acolyte" FactionPlayer (10, 10) 100 [] [])
                { uiSkills = HM.singleton "smithing" startingSkill
                , uiStats  = HM.insert "caffeine" caffeine $ HM.fromList
                    (mindSeedStats ⧺ [ ("perception", startingStat)
                                     , ("carrying_capacity", 100) ]) })
        }

resetUnits ∷ EngineEnv → IO ()
resetUnits env = resetUnitsWith env 0

-- | The stored value of a skill, read straight from the manager ref —
--   the only witness that says whether the verb wrote anything.
storedSkill ∷ EngineEnv → Text → IO (Maybe Float)
storedSkill env = storedIn env uiSkills

storedStat ∷ EngineEnv → Text → IO (Maybe Float)
storedStat env = storedIn env uiStats

storedIn ∷ EngineEnv → (UnitInstance → HM.HashMap Text Float) → Text
         → IO (Maybe Float)
storedIn env field key = do
    um ← readIORef (unitManagerRef env)
    pure (HM.lookup subjectUid (umInstances um) ⌦ (HM.lookup key . field))

-- | @unit.addXP(1, name, amount)@, with the amount spelled as a Lua
--   EXPRESSION so the non-finite cases are produced by Lua itself
--   rather than smuggled in as a Haskell value.
addXP ∷ LuaBackendState → Text → Text → IO Text
addXP ls name amount = evalDebug ls $ T.concat
    [ "return tostring(unit.addXP(1, '", name, "', ", amount, "))" ]

-- Debug-console return values arrive JSON-encoded, so a Lua string
-- comes back quoted.
q ∷ Text → Text
q t = "\"" <> t <> "\""

-- | Every amount the verb must refuse. The first three are non-finite
--   in Lua's own double domain; @1e40@ is FINITE there and only
--   overflows to Infinity in the stored 'Float', which is why the
--   guard has to run after the conversion rather than on the Lua
--   number.
refusedAmounts ∷ [(String, Text)]
refusedAmounts =
    [ ("NaN", "0/0")
    , ("+Infinity", "1/0")
    , ("-Infinity", "-1/0")
    , ("a double that overflows Float", "1e40")
    ]

spec ∷ SpecWith EngineEnv
spec = describe "applySkillXP at the unit.addXP Lua boundary (#1733)" $ do

    describe "a finite amount is unaffected (the control)" $ do
        it "still grows a SKILL and returns the new value" $ \env → do
            resetUnits env
            ls ← newBareLuaBackend env
            r  ← addXP ls "smithing" "0.5"
            r `shouldNotBe` q "nil"
            got ← storedSkill env "smithing"
            got `shouldBe` Just (applySkillXP startingSkill 0.5)

        it "still grows a STAT and returns the new value" $ \env → do
            resetUnits env
            ls ← newBareLuaBackend env
            r  ← addXP ls "perception" "0.5"
            r `shouldNotBe` q "nil"
            got ← storedStat env "perception"
            got `shouldBe` Just (applySkillXP startingStat 0.5)

    describe "a non-finite amount is refused" $ do
        it "returns nil and leaves the SKILL byte-identical" $ \env →
            forM_ refusedAmounts $ \(label, lit) → do
                resetUnits env
                ls ← newBareLuaBackend env
                r  ← addXP ls "smithing" lit
                (label, r) `shouldBe` (label, q "nil")
                got ← storedSkill env "smithing"
                (label, got) `shouldBe` (label, Just startingSkill)

        it "returns nil and leaves the STAT byte-identical" $ \env →
            forM_ refusedAmounts $ \(label, lit) → do
                resetUnits env
                ls ← newBareLuaBackend env
                r  ← addXP ls "perception" lit
                (label, r) `shouldBe` (label, q "nil")
                got ← storedStat env "perception"
                (label, got) `shouldBe` (label, Just startingStat)

        it "refuses before the lookup, so an unknown name is not special" $ \env → do
            -- The verb's pre-existing failure shape for a name the unit
            -- does not carry is the SAME nil, so the guard adds no new
            -- vocabulary for callers to learn.
            resetUnits env
            ls      ← newBareLuaBackend env
            unknown ← addXP ls "not_a_real_name" "0.5"
            nonFin  ← addXP ls "smithing" "0/0"
            unknown `shouldBe` q "nil"
            nonFin  `shouldBe` q "nil"

        it "a refused call cannot make the stat map non-finite" $ \env → do
            -- A NaN written into a stat map is what the guard keeps out:
            -- the stat arm of the verb writes straight into uiStats.
            resetUnits env
            ls ← newBareLuaBackend env
            _  ← addXP ls "perception" "0/0"
            got ← storedStat env "perception"
            fmap isNaN got `shouldBe` Just False

    -- Requirement 2 is independent of the ingress above: whatever route
    -- put a non-finite value in a rule input of the DERIVED concentration
    -- (#2838), the live unit.getMentalEffectiveness verb must still answer
    -- inside the documented band. `e == e` is false for a NaN, so the
    -- Lua-side predicate checks finiteness and the band together.
    describe "an ALREADY-corrupt rule input is contained downstream" $
        forM_ [("NaN", 0 / 0), ("+Infinity", 1 / 0), ("-Infinity", -1 / 0)]
            $ \(label, bad) →
                it ("with a stored caffeine = " ⧺ label) $ \env → do
                    resetUnitsWith env bad
                    ls ← newBareLuaBackend env
                    r  ← evalDebug ls $ T.concat
                        [ "local e = unit.getMentalEffectiveness(1); "
                        , "return tostring(e ~= nil and e == e "
                        , "and e >= 0.75 and e <= 1.10)" ]
                    r `shouldBe` q "true"
