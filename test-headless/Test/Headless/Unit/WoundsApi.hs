-- | @unit.getWounds@'s returned table has a fixed, complete 19-key
--   schema (#1969), driven through the REAL registered production API
--   against REAL manager refs.
--
--   The verb writes all 19 keys unconditionally on every wound, but its
--   haddock advertised four of them, so a caller could not discover the
--   other fifteen and nothing failed when one was added, renamed or
--   dropped. Three of the undocumented keys are three DIFFERENT
--   severities, and reading the wrong one changes behavior — @severity@
--   deliberately excludes the necrosis floor so rot does not drive the
--   acute organ-failure meters, while @severityEffective@ includes it
--   because the Haskell bleed / pain / impairment paths do.
--
--   'documentedSchema' below is the oracle for both halves of that: the
--   key-set and Lua-type assertions compare the LIVE table against it,
--   rather than against a restatement, so adding, removing, renaming or
--   retyping a key fails this spec.
--
--   The fixture's four wounds are chosen so no assertion can pass by
--   accident:
--
--     * the newest is a ROTTING subpart stab whose three severities all
--       differ (0.375 acute, 0.75 necrosis-floored, 0.5 inflicted) and
--       whose infection resolves against the catalogue,
--     * the middle one is a clean, undressed, untyped torso wound —
--       vital part, no infection, all three catalogue fields empty,
--     * the third is FESTERING (@heal@ = −0.5, so acute severity climbs
--       ABOVE the inflicted value) on a part the unit's def does not
--       declare, carrying an infection id that is not in the catalogue,
--     * the oldest is LETHAL at @capInjurySeverity@'s 1.6 ceiling, so
--       all three severities come back above 1 — the band a caller that
--       assumed 0..1 would saturate away.
--
--   No worldgen: @unit.getWounds@ reads the unit manager and the
--   infection catalogue and nothing else, so the fixture writes exactly
--   those two refs. The unit and body-part constructors are
--   'Test.Headless.Unit.TransferApi''s own rather than a second copy
--   free to drift from what the projection actually reads.
--
--   Run just this gate: @cabal test synarchy-test-headless
--   --test-options='--match "Unit.WoundsApi"'@.
module Test.Headless.Unit.WoundsApi (spec) where

import UPrelude
import Test.Hspec
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import Data.IORef (writeIORef)
import Data.List (sort, sortOn)
import Engine.Core.State (EngineEnv(..))
import Engine.Scripting.Lua.Types (LuaBackendState(..))
import Infection.Types (InfectionDef(..), InfectionManager(..))
import Unit.Faction (Faction(..))
import Unit.Types
    ( BodyPart(..), UnitDef(..), UnitId(..), UnitInstance(..)
    , UnitManager(..), Wound(..), emptyUnitManager )
import Test.Headless.Unit.TransferApi
    (evalDebug, mkUnit, minimalDef, newBareLuaBackend)

-- * The oracle

-- | Every key 'Engine.Scripting.Lua.API.Units.Combat.unitGetWoundsFn'
--   writes, with the Lua type it carries — the same list its haddock
--   publishes as the COMPLETE set. Nothing below restates it: the
--   key-set and type assertions are both derived from this one value,
--   so a key that appears in the live table and not here (or the
--   reverse) fails, and so does one whose type moved.
documentedSchema ∷ [(Text, Text)]
documentedSchema =
    [ ("part",              "string")
    , ("macro",             "string")
    , ("vital",             "boolean")
    , ("kind",              "string")
    , ("severity",          "number")
    , ("severityEffective", "number")
    , ("severityInflicted", "number")
    , ("heal",              "number")
    , ("at",                "number")
    , ("bandage",           "number")
    , ("clot",              "number")
    , ("dressing",          "string")
    , ("infection",         "number")
    , ("clean",             "boolean")
    , ("infectionType",     "string")
    , ("infectionName",     "string")
    , ("infectionIcon",     "string")
    , ("infectionCategory", "string")
    , ("necrosis",          "number")
    ]

-- | The oracle rendered the way @__wkeys@ renders the live table:
--   sorted, comma-joined. Both sides sort ASCII identifiers, so
--   Lua's @table.sort@ and this agree.
expectedKeys ∷ Text
expectedKeys = T.intercalate "," (sort (map fst documentedSchema))

-- | The oracle rendered the way @__wtypes@ renders the live table.
expectedTypes ∷ Text
expectedTypes = T.intercalate ","
    [ k <> ":" <> t | (k, t) ← sortOn fst documentedSchema ]

-- * Fixture identities

-- | uid 1 carries all three wounds; uid 2 is live and UNWOUNDED, so the
--   empty-array answer can be told apart from the nil one; uid 99 does
--   not exist at all.
woundedUid, unwoundedUid, missingUid ∷ UnitId
woundedUid   = UnitId 1
unwoundedUid = UnitId 2
missingUid   = UnitId 99

-- * Fixture

-- | A three-level body: a VITAL targetable torso, a targetable
--   @left_arm@ under it, and a NON-targetable @left_hand@ under that.
--   A wound on the hand therefore has to climb one level to reach a
--   macro part, which is the rollup the verb performs.
--
--   Built off 'minimalDef''s own part rather than a second literal, so
--   a new 'BodyPart' field cannot silently diverge here.
bodyPart ∷ Text → Maybe Text → Bool → Bool → BodyPart
bodyPart pid parent vital targetable =
    case udBodyParts (minimalDef "template" "Template") of
        (p : _) → p { bpId = pid, bpName = pid, bpParent = parent
                    , bpVital = vital, bpTargetable = targetable }
        []      → error "minimalDef no longer declares a body part"

woundedDef ∷ UnitDef
woundedDef = (minimalDef "acolyte" "Acolyte")
    { udBodyParts =
        [ bodyPart "torso"     Nothing            True  True
        , bodyPart "left_arm"  (Just "torso")     False True
        , bodyPart "left_hand" (Just "left_arm")  False False ] }

-- | Newest. Inflicted 0.5 with heal 0.25 gives an ACUTE severity of
--   0.375, which the 0.75 necrosis floor raises to 0.75 effective — so
--   all three severities differ and a consumer reading the wrong one is
--   visible. Every number is a power-of-two sum, so nothing here
--   depends on float formatting luck.
handWound ∷ Wound
handWound = Wound
    { woundPart = "left_hand", woundKind = "stab", woundSeverity = 0.5
    , woundAt = 200, woundBandage = 0.0625, woundClot = 0.5
    , woundHeal = 0.25, woundDressing = "bandage", woundInfection = 0.75
    , woundClean = False, woundInfectionType = "staph"
    , woundNecrosis = 0.75 }

-- | Middle. Clean, undressed, untyped and on a VITAL part: the control
--   for every "empty when there is nothing to resolve" assertion.
torsoWound ∷ Wound
torsoWound = Wound
    { woundPart = "torso", woundKind = "slash", woundSeverity = 0.25
    , woundAt = 100, woundBandage = 1.0, woundClot = 0.0
    , woundHeal = 0.0, woundDressing = "", woundInfection = 0.0
    , woundClean = True, woundInfectionType = ""
    , woundNecrosis = 0.0 }

-- | Oldest. A NEGATIVE heal — what a festering wound reaches — so acute
--   severity climbs to 0.75, ABOVE the 0.5 inflicted. Its part is not
--   in the def and its infection id is not in the catalogue, so both
--   fallbacks are exercised on one wound.
festeringWound ∷ Wound
festeringWound = Wound
    { woundPart = "phantom_limb", woundKind = "blunt", woundSeverity = 0.5
    , woundAt = 50, woundBandage = 1.0, woundClot = 0.0
    , woundHeal = -0.5, woundDressing = "tourniquet", woundInfection = 0.5
    , woundClean = False, woundInfectionType = "not_in_catalogue"
    , woundNecrosis = 0.0 }

-- | Oldest of all, and the reason no consumer may normalise against 1:
--   a lethal slash at 'Unit.Injury.maxInjurySeverity'. Combat and falls
--   clamp there, not at 1, so all three severities come back ABOVE 1 and
--   a doc or a caller that promised 0..1 is wrong about the injuries
--   that actually kill.
lethalWound ∷ Wound
lethalWound = Wound
    { woundPart = "torso", woundKind = "slash", woundSeverity = 1.6
    , woundAt = 25, woundBandage = 1.0, woundClot = 0.0
    , woundHeal = 0.0, woundDressing = "", woundInfection = 0.0
    , woundClean = False, woundInfectionType = ""
    , woundNecrosis = 0.0 }

staph ∷ InfectionDef
staph = InfectionDef
    { infId = "staph", infName = "Staph Infection", infIcon = "staph_icon"
    , infCategory = "bacterial", infSites = ["surface"], infBaseWeight = 1
    , infTempMin = -50, infTempMax = 50, infMoistMin = 0, infMoistMax = 1
    , infAggressiveness = 1, infInfectability = 1
    , infCurableBy = ["antibiotics"], infCureRate = 1
    , infWoundInfectable = True, infEffects = []
    , infTransmissibility = 0, infTransmission = [] }

-- | The unit manager and the infection catalogue: the only two refs
--   'unitGetWoundsFn' reads. Wounds are stored newest-first, the order
--   the verb preserves.
resetWorld ∷ EngineEnv → IO ()
resetWorld env = do
    writeIORef (infectionManagerRef env)
        (InfectionManager (HM.singleton "staph" staph))
    writeIORef (unitManagerRef env) emptyUnitManager
        { umDefs = HM.singleton "acolyte" woundedDef
        , umInstances = HM.fromList
            [ (woundedUid
              , wounded [handWound, torsoWound, festeringWound, lethalWound])
            , (unwoundedUid, wounded []) ] }
  where
    wounded ws = (mkUnit "acolyte" FactionPlayer (10, 10) 100 [] [])
                     { uiWounds = ws }

-- * Lua plumbing

-- | Readers over the LIVE result of the production verb. Each answers
--   with a flat string so the debug console's JSON encoding stays
--   unambiguous, and each distinguishes @NIL@ (the verb returned nil)
--   from @NOWOUND@ (it returned an array without that index) — the two
--   outcomes an unwounded unit and a missing one must not share.
--
--   @__wnum@ formats with @%.6f@ rather than @tostring@: Lua renders a
--   float at 14 significant digits, which would make every numeric
--   expectation a hostage to the Float→Double widening.
helpersLua ∷ Text
helpersLua = T.concat
    [ "local function keysOf(w) "
    , "  local ks = {}; "
    , "  for k in pairs(w) do ks[#ks + 1] = tostring(k) end; "
    , "  table.sort(ks); "
    , "  return ks; "
    , "end; "
    , "local function woundAt(uid, i) "
    , "  local ws = unit.getWounds(uid); "
    , "  if ws == nil then return nil, 'NIL' end; "
    , "  local w = ws[i]; "
    , "  if w == nil then return nil, 'NOWOUND' end; "
    , "  return w, nil; "
    , "end; "
    , "_G.__wkeys = function(uid, i) "
    , "  local w, err = woundAt(uid, i); "
    , "  if w == nil then return err end; "
    , "  return table.concat(keysOf(w), ','); "
    , "end; "
    , "_G.__wtypes = function(uid, i) "
    , "  local w, err = woundAt(uid, i); "
    , "  if w == nil then return err end; "
    , "  local out = {}; "
    , "  for j, k in ipairs(keysOf(w)) do out[j] = k .. ':' .. type(w[k]) end; "
    , "  return table.concat(out, ','); "
    , "end; "
    , "_G.__wstr = function(uid, i, k) "
    , "  local w, err = woundAt(uid, i); "
    , "  if w == nil then return err end; "
    , "  return tostring(w[k]); "
    , "end; "
    , "_G.__wnum = function(uid, i, k) "
    , "  local w, err = woundAt(uid, i); "
    , "  if w == nil then return err end; "
    , "  if type(w[k]) ~= 'number' then return 'NOTNUM' end; "
    , "  return string.format('%.6f', w[k]); "
    , "end; "
    , "_G.__wcount = function(uid) "
    , "  local ws = unit.getWounds(uid); "
    , "  if ws == nil then return 'NIL' end; "
    , "  return tostring(#ws); "
    , "end; "
    , "return 'wounds_helpers_loaded'"
    ]

-- | The REAL registered API — 'newBareLuaBackend' runs production
--   @registerLuaAPI@ — plus the readers above. The load is asserted, so
--   a helper chunk that failed to compile cannot make the assertions
--   below vacuous.
newHelpers ∷ EngineEnv → IO LuaBackendState
newHelpers env = do
    ls ← newBareLuaBackend env
    evalDebug ls helpersLua `shouldReturn` q "wounds_helpers_loaded"
    pure ls

call ∷ LuaBackendState → Text → IO Text
call ls expr = evalDebug ls ("return " <> expr)

uidOf ∷ UnitId → Text
uidOf (UnitId n) = tshow n

-- | Wound @i@ of the wounded unit, as the reader @fn@ renders it.
readW ∷ LuaBackendState → Text → Int → Text → IO Text
readW ls fn i k = call ls (T.concat
    [fn, "(", uidOf woundedUid, ", ", tshow i, ", '", k, "')"])

-- | @__wkeys@ / @__wtypes@ take no key, so they get their own caller.
readShape ∷ LuaBackendState → Text → UnitId → Int → IO Text
readShape ls fn uid i = call ls (T.concat
    [fn, "(", uidOf uid, ", ", tshow i, ")"])

q ∷ Text → Text
q t = "\"" <> t <> "\""

spec ∷ SpecWith EngineEnv
spec = describe "Unit.WoundsApi" $ do

    describe "the wound table's schema is exactly what the haddock publishes" $ do

        it "carries the documented keys and NO others, on every wound" $ \env → do
            resetWorld env
            ls ← newHelpers env
            forM_ [1 ∷ Int, 2, 3, 4] $ \i →
                readShape ls "__wkeys" woundedUid i
                    `shouldReturn` q expectedKeys

        it "gives each key its documented Lua type, on every wound" $ \env → do
            resetWorld env
            ls ← newHelpers env
            forM_ [1 ∷ Int, 2, 3, 4] $ \i →
                readShape ls "__wtypes" woundedUid i
                    `shouldReturn` q expectedTypes

    describe "the three severities are three different measures" $ do

        it "floors severityEffective on necrosis while severity excludes it" $ \env → do
            resetWorld env
            ls ← newHelpers env
            -- inflicted 0.5, heal 0.25, necrosis 0.75
            readW ls "__wnum" 1 "severityInflicted" `shouldReturn` q "0.500000"
            readW ls "__wnum" 1 "severity"          `shouldReturn` q "0.375000"
            readW ls "__wnum" 1 "severityEffective" `shouldReturn` q "0.750000"
            readW ls "__wnum" 1 "necrosis"          `shouldReturn` q "0.750000"

        it "collapses all three onto one value when nothing has healed or rotted" $ \env → do
            resetWorld env
            ls ← newHelpers env
            -- the control: inflicted 0.25, heal 0, necrosis 0
            readW ls "__wnum" 2 "severityInflicted" `shouldReturn` q "0.250000"
            readW ls "__wnum" 2 "severity"          `shouldReturn` q "0.250000"
            readW ls "__wnum" 2 "severityEffective" `shouldReturn` q "0.250000"

        it "reports the LETHAL band above 1, unclamped" $ \env → do
            resetWorld env
            ls ← newHelpers env
            -- capInjurySeverity's non-blunt ceiling is 1.6, and combat
            -- and falls write that straight onto the wound. All three
            -- severities must come back above 1: clamping any of them
            -- would erase the fatal outcomes.
            readW ls "__wnum" 4 "severityInflicted" `shouldReturn` q "1.600000"
            readW ls "__wnum" 4 "severity"          `shouldReturn` q "1.600000"
            readW ls "__wnum" 4 "severityEffective" `shouldReturn` q "1.600000"

        it "passes a NEGATIVE heal through, so severity climbs above inflicted" $ \env → do
            resetWorld env
            ls ← newHelpers env
            -- a festering wound: heal −0.5 → acute 0.5 × 1.5 = 0.75
            readW ls "__wnum" 3 "heal"              `shouldReturn` q "-0.500000"
            readW ls "__wnum" 3 "severityInflicted" `shouldReturn` q "0.500000"
            readW ls "__wnum" 3 "severity"          `shouldReturn` q "0.750000"
            readW ls "__wnum" 3 "severityEffective" `shouldReturn` q "0.750000"

    describe "the derived and passed-through fields" $ do

        it "rolls a subpart up to its nearest TARGETABLE ancestor" $ \env → do
            resetWorld env
            ls ← newHelpers env
            -- left_hand is not targetable; left_arm above it is
            readW ls "__wstr" 1 "part"  `shouldReturn` q "left_hand"
            readW ls "__wstr" 1 "macro" `shouldReturn` q "left_arm"
            -- an already-targetable part is its own macro
            readW ls "__wstr" 2 "macro" `shouldReturn` q "torso"
            -- a part the def does not declare falls back to itself
            readW ls "__wstr" 3 "macro" `shouldReturn` q "phantom_limb"

        it "reads vital off the wounded part itself, not its macro" $ \env → do
            resetWorld env
            ls ← newHelpers env
            -- left_hand rolls up to a NON-vital arm under a VITAL torso
            readW ls "__wstr" 1 "vital" `shouldReturn` q "false"
            readW ls "__wstr" 2 "vital" `shouldReturn` q "true"
            -- an unknown part is never vital
            readW ls "__wstr" 3 "vital" `shouldReturn` q "false"

        it "resolves the infection catalogue, and empties all three fields when it cannot" $ \env → do
            resetWorld env
            ls ← newHelpers env
            readW ls "__wstr" 1 "infectionType"     `shouldReturn` q "staph"
            readW ls "__wstr" 1 "infectionName"     `shouldReturn` q "Staph Infection"
            readW ls "__wstr" 1 "infectionIcon"     `shouldReturn` q "staph_icon"
            readW ls "__wstr" 1 "infectionCategory" `shouldReturn` q "bacterial"
            -- untyped: the id itself is empty
            readW ls "__wstr" 2 "infectionType"     `shouldReturn` q ""
            readW ls "__wstr" 2 "infectionName"     `shouldReturn` q ""
            readW ls "__wstr" 2 "infectionIcon"     `shouldReturn` q ""
            readW ls "__wstr" 2 "infectionCategory" `shouldReturn` q ""
            -- typed, but the catalogue has no such def: id survives,
            -- the three resolved fields are empty
            readW ls "__wstr" 3 "infectionType"     `shouldReturn` q "not_in_catalogue"
            readW ls "__wstr" 3 "infectionName"     `shouldReturn` q ""
            readW ls "__wstr" 3 "infectionIcon"     `shouldReturn` q ""
            readW ls "__wstr" 3 "infectionCategory" `shouldReturn` q ""

        it "passes the kind, first-aid and infection state through unchanged" $ \env → do
            resetWorld env
            ls ← newHelpers env
            readW ls "__wstr" 1 "kind"       `shouldReturn` q "stab"
            readW ls "__wstr" 1 "dressing"   `shouldReturn` q "bandage"
            readW ls "__wnum" 1 "bandage"    `shouldReturn` q "0.062500"
            readW ls "__wnum" 1 "clot"       `shouldReturn` q "0.500000"
            readW ls "__wnum" 1 "infection"  `shouldReturn` q "0.750000"
            readW ls "__wstr" 1 "clean"      `shouldReturn` q "false"
            readW ls "__wstr" 2 "kind"       `shouldReturn` q "slash"
            readW ls "__wstr" 2 "dressing"   `shouldReturn` q ""
            readW ls "__wnum" 2 "bandage"    `shouldReturn` q "1.000000"
            readW ls "__wstr" 2 "clean"      `shouldReturn` q "true"
            readW ls "__wstr" 3 "kind"       `shouldReturn` q "blunt"
            readW ls "__wstr" 3 "dressing"   `shouldReturn` q "tourniquet"

    describe "the array itself" $ do

        it "is newest-first" $ \env → do
            resetWorld env
            ls ← newHelpers env
            call ls ("__wcount(" <> uidOf woundedUid <> ")")
                `shouldReturn` q "4"
            readW ls "__wnum" 1 "at" `shouldReturn` q "200.000000"
            readW ls "__wnum" 2 "at" `shouldReturn` q "100.000000"
            readW ls "__wnum" 3 "at" `shouldReturn` q "50.000000"
            readW ls "__wnum" 4 "at" `shouldReturn` q "25.000000"

        it "is EMPTY for a live unwounded unit and nil for a missing one" $ \env → do
            resetWorld env
            ls ← newHelpers env
            call ls ("__wcount(" <> uidOf unwoundedUid <> ")")
                `shouldReturn` q "0"
            readShape ls "__wkeys" unwoundedUid 1 `shouldReturn` q "NOWOUND"
            call ls ("__wcount(" <> uidOf missingUid <> ")")
                `shouldReturn` q "NIL"
            readShape ls "__wkeys" missingUid 1 `shouldReturn` q "NIL"
