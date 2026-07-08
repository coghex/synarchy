{-# LANGUAGE Strict, UnicodeSyntax, OverloadedStrings #-}

-- | Per-tick wound subsystem. Runs at ~10 Hz from the combat thread
--   (the combat thread itself runs at 60 Hz but only ticks wounds on
--   every 6th iteration — finer resolution buys nothing visible to
--   the player). Mutates `uiWounds` (severity decay) and `uiBlood`
--   (bleeding drain), promotes alive→Collapsed at the 30%-blood
--   threshold, and alive→Dead at ≤0 blood, emitting an
--   "exsanguination" CombatEvent.
--
-- ## Formula summary (tunable constants at top of file)
--
-- bleed_per_sec(w, part, constitution) =
--     severity^2 × kind_factor × part.bleed_factor × bleedScale
--     / clamp(constitution, 0.5, 2.0)
--
-- heal_per_sec(w, constitution) =
--     healBaseRate × clotFactor(woundClot) × restMult ×
--     clamp(constitution, 0.3, 3.0) × infectionMult × calorieMult
--   clotFactor ramps from healClotFloor (open wound) up to 1.0 (fully
--   clotted); woundClot itself advances elsewhere in this module
--   (bandages/dressings/time), and infection/calorie state gate the
--   rate further.
--
-- Wounds with severity < woundCleanupThreshold are dropped from the
-- list to keep the per-tick scan cheap.
--
-- All blood values are litres; bodyMass-derived max_blood is
-- recomputed live so wasting/regrowth carries through naturally.
module Combat.Wounds
    ( tickAllWounds
    , propagateSevering   -- exposed for unit testing
    , tickOneUnit         -- exposed for unit testing (pure per-unit wound tick)
    , bleedRateFor        -- current L/sec blood loss (for the info panel)
    , kindBleedFactor     -- per-kind bleed multiplier (treat-action ranking)
    , destroyThreshold    -- the "structurally destroyed" severity (#607 impact blood)
    ) where

import UPrelude
import qualified Data.Text as T
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import qualified Data.Sequence as Seq
import qualified Data.List as L
import Data.IORef (readIORef, atomicModifyIORef')
import System.Environment (lookupEnv)
import Combat.Types (CombatEvent(..))
import Engine.Core.State (EngineEnv(..), activeWorldState)
import Engine.Core.Log (logDebug, LogCategory(..))
import qualified Engine.Core.Queue as Q
import Unit.Types (UnitId(..), UnitInstance(..), UnitDef(..)
                  , UnitManager(..), BodyPart(..), Wound(..), woundEffSeverity
                  , Scar(..), bloodMassRatio)
import Unit.Command.Types (UnitCommand(..))
import qualified System.Random as Random
import World.State.Types (WorldState(..))
import World.Generate.Types (WorldGenParams(..))
import World.Weather.Lookup (lookupLocalClimate, LocalClimate(..))
import Infection.Types (InfectionManager, InfectionDef(..)
                       , infectionsForSite, lookupInfection)

-- ----- Tuning constants -----

bleedScale ∷ Float
bleedScale = 1.2
-- Calibration check:
--   sev-1.0 slash on neck (bleed_factor 3.0, kind 1.0) of a bear
--   with constitution 2.5:
--     bleed = 1.0 × 1.0 × 3.0 × 1.2 / 2.5 = 1.44 L/s
--     → 7 L bear drains in ~5 seconds. Matches the user's spec
--   sev-0.05 scratch on finger (bleed_factor 1.0):
--     bleed = 0.0025 × 1.0 × 1.0 × 1.2 / 1.0 = 0.003 L/s — bleeds
--   sev-0.02 scratch on finger:
--     bleed = 0.0004 × 1.2 = 0.00048 L/s → below 0.001 clot
--     threshold, heals naturally.

woundCleanupThreshold ∷ Float
woundCleanupThreshold = 0.01   -- EFFECTIVE severity below this = healed, removed

unconsciousFraction ∷ Float
unconsciousFraction = 0.30   -- pose flips to Collapsed below this

-- ----- Clotting -----
-- Each wound's `woundClot` (0..1) fills over time; the bleed is scaled
-- by (1 − clot), so a fully-clotted wound has stopped bleeding. The
-- rate ACCELERATES as the clot forms and scales DOWN with severity and
-- hard-to-clot kinds, so a scratch seals in well under a minute while a
-- deep arterial bleed barely clots at all (it outpaces clotting and
-- kills first unless a medic intervenes). A bandage's pressure boosts
-- the rate, so treatment both cuts the immediate bleed AND drives the
-- wound to a full clot far sooner.
clotBaseRate ∷ Float
clotBaseRate = 0.14
-- Calibration (untreated, body constitution 1.0):
--   sev-0.15 slash:  resist = 1·(0.85)² = 0.72; accel ramps 0.08→1;
--     clots (→1.0) in ~25 game-s, bleed gone well before.
--   sev-0.5 slash:   resist = 1·0.25 = 0.25 — clots in ~2-3 min.
--   sev-0.5 arterial: resist = 0.05·0.25 = 0.0125 — effectively never
--     self-clots; bleeds out in seconds without treatment.
--   bandaged (seep 0.1): bandBoost = 1+4·0.9 = 4.6× → clot finishes
--     ~5× faster AND bleed already cut to 10%.

clotSeed ∷ Float
clotSeed = 0.08   -- initial clot-accel floor (a fresh wound at clot 0
                  -- still begins to clot; the term grows from here)

clotPressureK ∷ Float
clotPressureK = 4.0   -- bandage pressure boost: rate ×(1 + K·(1−seep))

-- Per-kind self-clot ability (multiplies the clot rate). Ordinary
-- soft-tissue wounds clot normally; a stump clots slowly; internal and
-- (especially) arterial bleeds barely clot on their own. Fracture /
-- concussion barely bleed anyway, so their clot value is moot — leave
-- them at the 1.0 default.
kindClotFactor ∷ Text → Float
kindClotFactor "arterial" = 0.05
kindClotFactor "severed"  = 0.15
kindClotFactor "internal" = 0.10
kindClotFactor _          = 1.0

-- ----- Healing -----
-- A separate progress bar (woundHeal 0..1) from clotting. It fills
-- SLOWLY once the wound has clotted; the wound's effective severity is
-- woundSeverity × (1 − heal), so pain/impairment/residual bleed all ease
-- as it heals. At full heal the wound is removed, leaving a scar if it
-- was severe. UNIFORM rate across wound kinds (the user's call) — only
-- severed is excluded (a lost limb can't regrow). Constitution scales
-- it gently (the existing healCon), and clot gates it (an open wound
-- barely mends). The base rate is deliberately slow.
healBaseRate ∷ Float
healBaseRate = 0.0016
-- Calibration (clotted, constitution 1.0): a sev-0.5 wound reaches
-- effSev < 0.01 at heal ≈ 0.98 — about 0.98 / 0.0016 ≈ 600 s ≈ 10 min of
-- clotted time. A scratch (sev 0.05) heals out at heal ≈ 0.8 → faster.

healClotFloor ∷ Float
healClotFloor = 0.05   -- an un-clotted wound heals at 5 % of the rate
                       -- (clot scales it from this floor up to full)

sleepHealMult ∷ Float
sleepHealMult = 4.0    -- rest/sleep heal speed-up, applied via restMult
                        -- below when uiPose == "sleeping"

scarSeverityThreshold ∷ Float
scarSeverityThreshold = 0.3   -- wounds milder than this heal scar-free

-- Calorie gating: a starving body heals slower (the food system's
-- "starving units heal significantly slower"). The unit's calories/
-- max_calories fraction (the ENERGY STORE, not the stomach meter — a
-- unit with an empty stomach but a fed store heals fine) drives a
-- heal-rate multiplier — full above calorieHealFloorFrac of the pool,
-- ramping down to calorieHealMin at an empty store.
calorieHealFloorFrac ∷ Float
calorieHealFloorFrac = 0.5

calorieHealMin ∷ Float
calorieHealMin = 0.25

-- | Heal-rate multiplier from the unit's calorie-store state. Gated on
--   the PRESENCE of a live "calories" stat — that's the real "this unit
--   runs on calories" signal. max_calories alone isn't: it's a body-
--   derived stat seeded for any unit with a body block (wildlife
--   included), but only food-system units (acolytes) ever get a draining
--   "calories" pool. A unit without it — wildlife, or an acolyte before
--   its first resource tick — heals ungated rather than being mistaken
--   for starving.
calorieHealMultiplier ∷ HM.HashMap Text Float → Float
calorieHealMultiplier stats =
    case (HM.lookup "calories" stats, HM.lookup "max_calories" stats) of
        (Just cur, Just maxH)
            | maxH > 0 →
                let frac = cur / maxH
                in if frac ≥ calorieHealFloorFrac then 1.0
                   else calorieHealMin
                      + (1 - calorieHealMin) * (frac / calorieHealFloorFrac)
        _ → 1.0

-- ----- Infection -----
-- A per-wound `woundInfection` (0..1) bar that grows on an OPEN, undressed
-- wound after a grace period, ∝ effective severity × kind. DETERMINISTIC:
-- a wound marked `woundClean` (antiseptic-disinfected during treatment)
-- never grows it — that's the PREVENTION half of the medical loop.
-- ANTIBIOTICS are the CURE (drive it back down). An infected wound barely
-- heals and, past `infectionWorsenThreshold`, WORSENS — woundHeal reverses
-- so effective severity climbs above the inflicted value. Infection feeds
-- the systemic SEPSIS failure-meter (a delayed death pathway, in
-- unit_resources.lua), so an untreated dirty wound can kill days/"hours"
-- after the fight.
infectionBaseRate ∷ Float
infectionBaseRate = 0.0016
-- Calibration (untreated, sev-0.5 slash, kind 1.0): driver 0.3+0.5 = 0.8 →
-- 0.00128/s → infection 0.5 in ~6.5 min, 1.0 in ~13 min. A clean wound
-- never starts; antibiotics reverse it (see Units.treatInfectionIO).

infectionGraceSec ∷ Double
infectionGraceSec = 60   -- a fresh wound isn't infected for the first minute

-- | Test-only acceleration (#593): with the production rate above, a
--   probe would need to wait real minutes to observe growth. Gated on
--   @SYNARCHY_INFECTION_TEST_MODE@, which nothing but
--   'tools/infection_probe.py' ever sets (for its own dedicated engine
--   subprocess) — unset, `tickAllWounds` passes 'False' and every unit
--   ticks at the production rate/grace above, unchanged.
testInfectionBaseRate ∷ Float
testInfectionBaseRate = 0.05

testInfectionGraceSec ∷ Double
testInfectionGraceSec = 5

infectionTestModeVar ∷ String
infectionTestModeVar = "SYNARCHY_INFECTION_TEST_MODE"

infectionWorsenThreshold ∷ Float
infectionWorsenThreshold = 0.6   -- above this, healing reverses (festers)

infectionWorsenRate ∷ Float
infectionWorsenRate = 0.004   -- how fast a festering wound deteriorates

-- Fever suppression: infection growth × (1 − feverSuppressK·(core−38)), floored.
-- A febrile body (core > 38°C, raised by the thermo fever response) slows the
-- infection — the two-way loop that closes the homeostasis arc.
feverSuppressK ∷ Float
feverSuppressK = 0.25   -- core 40 → ×0.5 growth; core 41 → floored

feverSuppressFloor ∷ Float
feverSuppressFloor = 0.30

-- ----- Necrosis (rot / gangrene) -----
-- A NECROTIC infection (its def has the "necrosis" effect) kills tissue:
-- woundNecrosis accrues (PERMANENTLY) while the infection is established,
-- raising effective severity. At 1.0 a non-vital part rots off (severing
-- cascade); a vital part rotting through kills (gangrene). "gas" (gas
-- gangrene) rots faster. Clearing the infection stops the rot but the dead
-- tissue stays — only a future debridement action would remove it.
necrosisBaseRate ∷ Float
necrosisBaseRate = 0.004   -- dead tissue/s at infection 1.0 (~4 min to rot off)

necrosisInfThreshold ∷ Float
necrosisInfThreshold = 0.4   -- infection must be established before tissue rots

gasNecrosisMult ∷ Float
gasNecrosisMult = 1.6   -- "gas" gangrene rots faster

necrosisDestroyThreshold ∷ Float
necrosisDestroyThreshold = 1.0   -- non-vital part fully rotted → rots off

necrosisLethalVital ∷ Float
necrosisLethalVital = 0.85   -- vital part this rotted → death by gangrene

woundHealFloor ∷ Float
woundHealFloor = -0.5   -- how far a festering wound can worsen past inflicted
                        -- (effSev = sev0 × (1 − heal), so heal = −0.5 → 1.5×)

-- | Per-kind susceptibility to infection (multiplies the growth rate).
--   Deep/dirty wounds (punctures, open stumps) fester worst; a closed
--   fracture or concussion barely can (intact skin); bruises low.
kindInfectFactor ∷ Text → Float
kindInfectFactor "stab"       = 1.2   -- deep dirty puncture — worst
kindInfectFactor "severed"    = 1.3   -- open stump, gangrene
kindInfectFactor "slash"      = 1.0
kindInfectFactor "arterial"   = 1.0
kindInfectFactor "internal"   = 0.6   -- visceral (organFailure also covers it)
kindInfectFactor "blunt"      = 0.3   -- skin mostly intact
kindInfectFactor "fracture"   = 0.2   -- closed
kindInfectFactor "concussion" = 0.0   -- closed head injury, no wound
kindInfectFactor "frostbite"  = 1.0   -- cold-killed tissue infects readily
kindInfectFactor _            = 0.8

-- | Climate's effect on how FAST infection sets in: warm + wet → faster,
--   cold + dry → slower. Range ~0.5 (cold, arid) … ~1.5 (hot, humid).
--   "moisture" = relative humidity (lcHumidity, 0..1).
climateOnsetFactor ∷ Maybe LocalClimate → Float
climateOnsetFactor Nothing  = 1.0   -- no world climate (tests / pre-gen)
climateOnsetFactor (Just c) =
    let warmth  = clamp01 (lcTemp c / 35)        -- 0 °C → 0, 35 °C → 1
        wetness = clamp01 (lcHumidity c)
    in 0.5 + 0.5 * warmth + 0.5 * wetness
  where clamp01 x = max 0 (min 1 x)

-- | How well an infection's favourable band matches the local climate, in
--   [0.1, 1] (floored so a hostile climate still allows *something* — staph
--   doesn't care). Multiplies the def's base_weight in selection.
climateMatchWeight ∷ LocalClimate → InfectionDef → Float
climateMatchWeight c d =
    bandMatch 0.04 (infTempMin d) (infTempMax d) (lcTemp c)
  * bandMatch 1.5  (infMoistMin d) (infMoistMax d) (lcHumidity c)
  where
    bandMatch falloff lo hi x
        | x ≥ lo ∧ x ≤ hi = 1.0
        | otherwise = max 0.1 (1.0 - falloff * (if x < lo then lo - x else x - hi))

-- | Which infection pool a wound draws from: "deep" (anaerobic) if its kind
--   is a puncture/internal injury OR it sits on a deep body part; else
--   "surface" (aerobic skin/soft-tissue). (User-chosen rule.)
woundSiteClass ∷ HM.HashMap T.Text BodyPart → Wound → T.Text
woundSiteClass parts w
    | deepKind ∨ deepPart = "deep"
    | otherwise           = "surface"
  where
    deepKind = woundKind w ≡ "stab" ∨ woundKind w ≡ "internal"
    deepPart = maybe False ((> 0.5) . bpDepth) (HM.lookup (woundPart w) parts)

-- | Weighted-random pick of an infection id from the wound's site pool,
--   weighted by base_weight × climate match. Returns "" if the pool is
--   empty (no climate → no selection; the caller leaves the type unset and
--   growth falls back to aggressiveness 1.0). Consumes the StdGen.
selectInfectionType
    ∷ InfectionManager → LocalClimate → T.Text → Random.StdGen
    → (T.Text, Random.StdGen)
selectInfectionType mgr clim site gen =
    let pool    = infectionsForSite site mgr
        weights = [ (infId d, infBaseWeight d * climateMatchWeight clim d)
                  | d ← pool ]
        total   = sum (map snd weights)
    in if total ≤ 0 then ("", gen)
       else let (r, gen') = Random.randomR (0, total) gen
            in (pickWeighted r weights, gen')
  where
    pickWeighted _ []            = ""
    pickWeighted _ [(i, _)]      = i
    pickWeighted r ((i, wgt):xs)
        | r ≤ wgt   = i
        | otherwise = pickWeighted (r - wgt) xs

-- | A chosen infection's growth-aggressiveness multiplier (1.0 if untyped
--   or unknown).
aggressivenessOf ∷ InfectionManager → T.Text → Float
aggressivenessOf _   "" = 1.0
aggressivenessOf mgr t  = maybe 1.0 infAggressiveness (lookupInfection t mgr)

-- | A chosen infection's infectability multiplier (foothold + resistance to
--   the immune response). 1.0 if untyped/unknown.
infectabilityOf ∷ InfectionManager → T.Text → Float
infectabilityOf _   "" = 1.0
infectabilityOf mgr t  = maybe 1.0 infInfectability (lookupInfection t mgr)

-- ----- Immunity / the immune-response race -----
-- Infection progress is a RACE between two tickers: the infection's own
-- growth (self-accelerating — a bad infection worsens faster), and the
-- unit's SYSTEMIC immune response (uiImmuneResponse), which ramps up while
-- any wound is infected and CLEARS infection. Constitution scales the
-- response; prior acquired immunity (uiImmunities, per type) resists a
-- bug's foothold; antibiotics nudge the response up. Surviving an infection
-- accrues immunity (∝ response × severity, integrated over the fight).

selfAccelGain ∷ Float
selfAccelGain = 1.0    -- growth ×(1 + this×infection): bad infections worsen faster

immuneRampRate ∷ Float
immuneRampRate = 0.018   -- base immune-response ramp/s (×constitution, accel)

immuneSeed ∷ Float
immuneSeed = 0.06    -- response-accel floor (a fresh response still begins)

immuneDecayRate ∷ Float
immuneDecayRate = 0.03   -- response decays/s once nothing is infected

immuneClearRate ∷ Float
immuneClearRate = 0.013   -- infection cleared per unit of response per s

infectionLoadThreshold ∷ Float
infectionLoadThreshold = 0.02   -- total infection above which the response ramps

immGainRate ∷ Float
immGainRate = 0.05   -- acquired immunity gained per (response × infection) per s

immunityDecayRate ∷ Float
immunityDecayRate = 0.00003   -- acquired immunity fades VERY slowly

immunityFloor ∷ Float
immunityFloor = 0.001   -- drop immunities below this (keeps the map small)

-- | How much acquired immunity to a type resists it, scaled by constitution.
--   Capped so immunity never makes a unit perfectly invulnerable.
immuneResistOf ∷ Float → Float → Float
immuneResistOf con immLevel = min 0.9 (immLevel * (0.5 + 0.5 * con))

kindBleedFactor ∷ Text → Float
kindBleedFactor "slash"      = 1.0
kindBleedFactor "stab"       = 0.6
kindBleedFactor "blunt"      = 0.2
-- A closed fracture barely bleeds (some internal); a concussion is a
-- closed-head injury — no external bleed at all. They're dangerous
-- through severity/pain and (for the head, a vital part) the lethal
-- ≥1 rule, not exsanguination.
kindBleedFactor "fracture"   = 0.05
kindBleedFactor "concussion" = 0.0
-- Internal bleeding drains blood with no external wound; a severed limb
-- hemorrhages from the stump (major, but clots over time); an opened
-- artery is the fastest bleed of all (a cut carotid empties you).
kindBleedFactor "internal"   = 0.5
kindBleedFactor "severed"    = 0.6
kindBleedFactor "arterial"   = 1.2
-- Frostbite is frozen/dead tissue — barely bleeds (its danger is necrosis +
-- infection, handled via woundNecrosis + the infection system).
kindBleedFactor "frostbite"  = 0.05
kindBleedFactor _            = 0.5

-- | Current total bleed rate (litres/second) for a unit, summed over its
--   wounds with the SAME per-wound formula 'tickOneUnit' drains by
--   (severity² × kind × part.bleed_factor × bleedScale / clamp(con)).
--   Exposed so the info panel can show "ml/s lost".
bleedRateFor ∷ UnitDef → UnitInstance → Float
bleedRateFor def inst =
    let parts    = HM.fromList [(bpId p, p) | p ← udBodyParts def]
        con      = HM.lookupDefault 1.0 "constitution" (uiStats inst)
        bleedCon = max 0.5 (min 2.0 con)
    in sum [ let effSev = woundEffSeverity w
             in effSev * effSev
                * kindBleedFactor (woundKind w)
                * maybe 1.0 bpBleedFactor (HM.lookup (woundPart w) parts)
                * woundBandage w
                * (1 - woundClot w)
                * bleedScale / bleedCon
           | w ← uiWounds inst ]


-- ----- Entry point -----

-- | One unit's tick outcome. Wound list + blood are mutated in
--   place. Pose changes go through the unit-command queue because
--   uiPose is a mirror of sim usPose (publishToRender overwrites
--   direct writes) — so this module returns the *intent* and the
--   caller enqueues the right command.
data WoundTickOutcome
    = NoChange
    | UnconsciousNow Text   -- worst-bleeding part id
    | DiedNow Text Text     -- (part id, cause): exsanguination | gangrene

-- | Tick all units' wounds by `dt` seconds. Designed for the combat
--   thread to call on its 10 Hz cadence (every 6th 60-Hz iteration).
--   Mutates uiWounds / uiBlood inline; pose changes are enqueued
--   onto the unit command queue.
tickAllWounds ∷ EngineEnv → Float → IO ()
tickAllWounds env dt = do
    gt ← readIORef (gameTimeRef env)
    infMgr ← readIORef (infectionManagerRef env)
    testMode ← isJust ⊚ lookupEnv infectionTestModeVar
    -- Active world's climate, for per-unit infection selection + onset speed.
    -- Nothing before a world exists (menu / pre-gen) → infection stays untyped.
    mClim ← do
        mWs ← activeWorldState env
        case mWs of
            Just ws → do
                mp ← readIORef (wsGenParamsRef ws)
                pure (fmap (\p → (wgpClimateState p, wgpWorldSize p)) mp)
            Nothing → pure Nothing
    -- One independent generator for this tick's infection-type rolls; split
    -- off statRNGRef (advances the master) so we don't race other consumers.
    tickGen ← atomicModifyIORef' (statRNGRef env) Random.splitGen
    -- Snapshot-and-clobber would lose any concurrent write to
    -- umInstances (publishToRender, Lua equip/modifier, the per-attack
    -- wound stamp in Combat.Resolution). Do the fold inside the
    -- atomicModifyIORef' lambda so wound updates merge with the
    -- current map instead of overwriting it. Same pattern as
    -- Unit.Thread.publishToRender.
    outcomes ← atomicModifyIORef' (unitManagerRef env) $ \um →
        let defs = umDefs um
            (updatedMap, os, _) =
                HM.foldlWithKey'
                    (\(acc, xs, gen) uid inst →
                        case HM.lookup (uiDefName inst) defs of
                            Nothing → (HM.insert uid inst acc, xs, gen)
                            Just def →
                                let unitClim = fmap (\(cs, wsz) →
                                        lookupLocalClimate cs wsz
                                            (floor (uiGridX inst))
                                            (floor (uiGridY inst))) mClim
                                    (inst', outcome, gen') =
                                        tickOneUnit gt def dt infMgr unitClim gen inst testMode
                                in case outcome of
                                    NoChange →
                                        (HM.insert uid inst' acc, xs, gen')
                                    _ →
                                        ( HM.insert uid inst' acc
                                        , (uid, outcome) : xs, gen'))
                    (HM.empty, [], tickGen) (umInstances um)
        in (um { umInstances = updatedMap }, os)

    logger ← readIORef (loggerRef env)
    mapM_ (\(uid@(UnitId uidRaw), outcome) → case outcome of
        UnconsciousNow part → do
            Q.writeQueue (unitQueue env) (UnitCollapse uid)
            logDebug logger CatThread $
                "collapsed: " <> T.pack (show uidRaw)
                    <> " (bleeding from " <> part <> ")"
        DiedNow part cause → do
            Q.writeQueue (unitQueue env) (UnitKill uid)
            let ev = CombatEvent
                    { ceTs       = gt
                    , ceKind     = "death"
                    , ceAttacker = Nothing
                    , ceTarget   = Just uidRaw
                    , cePayload  = HM.fromList
                        [ ("cause", cause)
                        , ("part",  part)
                        ]
                    }
            atomicModifyIORef' (combatEventsRef env) $ \buf →
                (buf Seq.|> ev, ())
            logDebug logger CatThread $
                "bled out: " <> T.pack (show uidRaw)
                    <> " from " <> part
        NoChange → pure ()
        ) outcomes

-- ----- Per-unit tick -----

-- | Tick one unit's wounds. Returns the new instance (wounds +
--   blood mutated) plus a pose-transition intent. Pose itself is
--   left unchanged here; the caller enqueues the appropriate
--   UnitCollapse / UnitKill command.
tickOneUnit
    ∷ Double → UnitDef → Float → InfectionManager → Maybe LocalClimate
    → Random.StdGen → UnitInstance → Bool
    → (UnitInstance, WoundTickOutcome, Random.StdGen)
tickOneUnit gt def dt infMgr mClim gen0 inst testMode
    | uiPose inst == "dead" = (inst, NoChange, gen0)
    | null (uiWounds inst)  =
        -- No wounds → no infection, but the immune response must still wind
        -- down and acquired immunity fade (else a recovered unit would keep a
        -- maxed response forever). Skip the allocation when both are already
        -- at rest (the common case for a healthy unit).
        if uiImmuneResponse inst ≤ 0 ∧ HM.null (uiImmunities inst)
          then (inst, NoChange, gen0)
          else let r'   = max 0 (uiImmuneResponse inst - immuneDecayRate * dt)
                   imm' = HM.filter (> immunityFloor)
                        $ HM.map (\v → max 0 (min 1
                                    (v - immunityDecayRate * dt)))
                                 (uiImmunities inst)
               in (inst { uiImmuneResponse = r', uiImmunities = imm' }
                  , NoChange, gen0)
    | otherwise =
        let parts = HM.fromList [(bpId p, p) | p ← udBodyParts def]
            con   = HM.lookupDefault 1.0 "constitution" (uiStats inst)
            bleedCon = max 0.5 (min 2.0 con)
            healCon  = max 0.3 (min 3.0 con)
            -- Climate scales how fast infection sets in (warm+wet faster).
            climateFactor = climateOnsetFactor mClim
            -- #593: test-mode acceleration (see testInfectionBaseRate).
            effBaseRate  = if testMode then testInfectionBaseRate else infectionBaseRate
            effGraceSec  = if testMode then testInfectionGraceSec else infectionGraceSec
            -- Fever suppression: a high core body temperature (the fever the
            -- thermo system raises in response to infection) slows infection
            -- growth — the body cooking the pathogens. core_temp is the stat
            -- the Lua thermo tick maintains; default 37 if it hasn't ticked yet.
            coreTemp = HM.lookupDefault 37.0 "core_temp" (uiStats inst)
            feverSuppress = max feverSuppressFloor
                              (1 - feverSuppressK * max 0 (coreTemp - 38))
            -- A unit at rest heals faster: sleeping pose applies
            -- sleepHealMult, everything else uses 1.0. No AI currently
            -- drives uiPose to "sleeping" (bear_ai.lua's rest cycle
            -- tracks its own bearPosture field separately, not
            -- uiPose), but the moment something does, the bonus
            -- applies with no further wiring needed here.
            restMult = if uiPose inst == "sleeping" then sleepHealMult else 1.0
            -- A starving unit heals slower (calorie gating). Wildlife
            -- without a calorie store reads 1.0.
            calHealMlt = calorieHealMultiplier (uiStats inst)
            -- SYSTEMIC immune response (Ticker B), advanced once per unit.
            -- Ramps up (accelerating, × constitution) while anything is
            -- infected; decays back toward 0 once nothing is. The per-wound
            -- fold below uses this value to CLEAR infection.
            immMap0   = uiImmunities inst
            totalLoad = sum (map woundInfection (uiWounds inst))
            r0        = uiImmuneResponse inst
            newR | totalLoad > infectionLoadThreshold =
                       min 1 (r0 + immuneRampRate * con
                                   * (immuneSeed + r0) * dt)
                 | otherwise = max 0 (r0 - immuneDecayRate * dt)
            (newWounds, totalDrain, worstPart, _worstRate, newScars, genFinal, immDelta) =
                L.foldl' (\(ws, drain, wp, wr, scars, gen, immD) w →
                    let sev0       = woundSeverity w   -- INFLICTED (static)
                        p          = HM.lookup (woundPart w) parts
                        partBleed  = maybe 1.0 bpBleedFactor p
                        -- Advance the clot FIRST (uses this tick's value
                        -- so the bleed reflects fresh progress). Rate
                        -- accelerates as the clot forms (clotSeed +
                        -- current clot), scales down with severity and
                        -- hard-to-clot kinds (arterial/severed barely
                        -- self-clot), and a bandage's pressure boosts it.
                        clotResist = kindClotFactor (woundKind w)
                                   * (1 - sev0) * (1 - sev0)
                        bandBoost  = 1 + clotPressureK * (1 - woundBandage w)
                        clotAccel  = clotSeed + woundClot w
                        newClot    = min 1 (woundClot w
                                     + clotBaseRate * clotResist
                                       * bandBoost * clotAccel * dt)
                        -- Advance INFECTION first (deterministic): an open,
                        -- un-disinfected wound accumulates infection after a
                        -- grace period, ∝ current effective severity × kind ×
                        -- the local CLIMATE and the chosen infection's
                        -- aggressiveness. A clean wound never grows it.
                        curEff     = sev0 * (1 - woundHeal w)
                        infAge     = gt - woundAt w
                        kindInfF   = kindInfectFactor (woundKind w)
                        eligible   = not (woundClean w)
                                   ∧ infAge ≥ effGraceSec
                                   ∧ kindInfF > 0
                        -- Pick the infection TYPE the first tick it festers
                        -- (weighted-random by site + climate). Needs a world
                        -- climate; without one (tests/pre-gen) the type stays
                        -- unset and aggressiveness falls back to 1.0.
                        (infType, gen') = case mClim of
                            Just clim
                                | eligible ∧ woundInfectionType w ≡ "" →
                                    selectInfectionType infMgr clim
                                        (woundSiteClass parts w) gen
                            _ → (woundInfectionType w, gen)
                        aggr       = aggressivenessOf infMgr infType
                        infectab   = infectabilityOf infMgr infType
                        -- Prior acquired immunity to this type resists it.
                        immResist  = immuneResistOf con
                                        (HM.lookupDefault 0 infType immMap0)
                        -- TICKER A — growth: self-accelerating (a bad infection
                        -- worsens faster), scaled by kind/severity/climate ×
                        -- the bug's aggressiveness × infectability, RESISTED by
                        -- prior immunity. Only while eligible (dirty, past grace).
                        selfAccel  = 1 + selfAccelGain * woundInfection w
                        infGrow    = if eligible
                                     then effBaseRate * kindInfF
                                          * (0.3 + curEff) * climateFactor
                                          * aggr * infectab * selfAccel
                                          * (1 - immResist) * feverSuppress
                                     else 0
                        -- TICKER B — the immune response clears infection. The
                        -- race: growth vs clearance. A high response overtakes
                        -- and the infection recedes (→ cleared).
                        infClear   = immuneClearRate * newR
                        newInf     = max 0 (min 1
                                       (woundInfection w + (infGrow - infClear) * dt))
                        -- Acquired immunity accrues while fighting (∝ response ×
                        -- current infection), so a bad/long infection leaves
                        -- more protection. Keyed by the infection type.
                        immD'      = if infType ≡ "" ∨ woundInfection w ≤ 0
                                     then immD
                                     else HM.insertWith (+) infType
                                            (immGainRate * newR * woundInfection w * dt)
                                            immD
                        -- NECROSIS (rot): a necrotic infection (its def has the
                        -- "necrosis" effect) kills tissue once established. Dead
                        -- tissue is PERMANENT (only grows). "gas" rots faster.
                        infEffs    = maybe [] infEffects (lookupInfection infType infMgr)
                        necGrow    = if "necrosis" `elem` infEffs
                                        ∧ newInf > necrosisInfThreshold
                                     then necrosisBaseRate * newInf
                                          * (if "gas" `elem` infEffs
                                               then gasNecrosisMult else 1)
                                     else 0
                        newNec     = min 1 (woundNecrosis w + necGrow * dt)
                        -- Advance healing. Uniform base rate across kinds
                        -- (severed never heals — the limb is gone),
                        -- SCALED by clot (an open wound barely mends), by rest,
                        -- gently by constitution, and GATED by infection (an
                        -- infected wound barely mends; a festering one — past
                        -- the worsen threshold — actively deteriorates as
                        -- woundHeal reverses below zero, so effSev climbs above
                        -- the inflicted value, capped by woundHealFloor).
                        canHeal    = woundKind w /= "severed"
                        clotHealF  = healClotFloor + (1 - healClotFloor) * newClot
                        infHealMlt = max 0 (1 - newInf)
                        healAdv    = if canHeal
                            then healBaseRate * clotHealF * restMult * healCon
                                 * infHealMlt * calHealMlt * dt
                            else 0
                        worsen     = if newInf > infectionWorsenThreshold
                            then infectionWorsenRate
                                 * (newInf - infectionWorsenThreshold) * dt
                            else 0
                        newHeal    = max woundHealFloor
                                         (min 1 (woundHeal w + healAdv - worsen))
                        -- Effective severity drops as the wound heals —
                        -- the single source of "this wound matters less now"
                        -- (and rises above sev0 when a festering wound's
                        -- woundHeal goes negative). Necrosis (dead tissue) is a
                        -- permanent floor: a rotting wound is at least as bad as
                        -- the fraction of tissue that has died.
                        effSev     = max (sev0 * (1 - newHeal)) newNec
                        bleedRate  =
                              (effSev * effSev)
                            * kindBleedFactor (woundKind w)
                            * partBleed
                            * woundBandage w     -- first-aid dressing
                            * (1 - newClot)      -- clotting
                            * bleedScale
                            / bleedCon
                        w'         = w { woundClot = newClot
                                      , woundHeal = newHeal
                                      , woundInfection = newInf
                                      , woundInfectionType = infType
                                      , woundNecrosis = newNec }
                        healedOut  = effSev < woundCleanupThreshold
                        (wp', wr') = if bleedRate > wr
                                     then (woundPart w, bleedRate)
                                     else (wp, wr)
                        -- A healed-out wound that was severe enough (and
                        -- isn't a never-healing severed stump) leaves a scar.
                        scars'     = if healedOut ∧ canHeal
                                        ∧ sev0 ≥ scarSeverityThreshold
                                     then Scar { scarPart     = woundPart w
                                               , scarKind     = woundKind w
                                               , scarSeverity = sev0
                                               , scarAt       = gt } : scars
                                     else scars
                    in ( if healedOut then ws else w' : ws
                       , drain + bleedRate * dt
                       , wp', wr', scars', gen', immD' ))
                ([], 0.0, "torso", 0.0, [], gen0, HM.empty)
                (uiWounds inst)
            -- Merge accrued immunity into the unit's map, then decay all
            -- immunities very slowly; drop negligible entries.
            mergedImm  = HM.unionWith (+) immMap0 immDelta
            newImm     = HM.filter (> immunityFloor)
                       $ HM.map (\v → max 0 (min 1 (v - immunityDecayRate * dt)))
                                mergedImm
            newBlood   = uiBlood inst - totalDrain
            bodyMass   = HM.lookupDefault 70.0 "body_mass" (uiStats inst)
            maxBlood   = bodyMass * bloodMassRatio
            unconsCut  = maxBlood * unconsciousFraction
            newWoundsR = reverse newWounds
            -- Gangrene death: a VITAL part rotted past the lethal threshold.
            -- (Non-vital rot-off is handled by propagateSevering below.)
            gangrenousVital =
                [ woundPart w | w ← newWoundsR
                , woundNecrosis w ≥ necrosisLethalVital
                , maybe False bpVital (HM.lookup (woundPart w) parts) ]
            -- Edge-triggered death guards. Exsanguination only fires on
            -- the tick blood crosses zero; gangrene only fires while the
            -- unit has not yet been published dead. Without these guards,
            -- the combat tick would re-emit DiedNow every tick until the
            -- unit thread's UnitKill processing snaps the render pose,
            -- which is up to one combat tick away. Matches the existing
            -- UnconsciousNow gating, which already checks uiPose ≠
            -- "collapsed" for the same reason.
            outcome
                | uiBlood inst > 0, newBlood ≤ 0 = DiedNow worstPart "exsanguination"
                | uiPose inst /= "dead"
                , (g:_) ← gangrenousVital        = DiedNow g "gangrene"
                | newBlood < unconsCut, uiPose inst /= "collapsed"
                                     = UnconsciousNow worstPart
                | otherwise          = NoChange
            inst' = inst
                { uiWounds = newWoundsR
                , uiScars  = newScars <> uiScars inst
                , uiImmuneResponse = newR
                , uiImmunities = newImm
                , uiBlood  = max 0 newBlood
                }
        in (propagateSevering def inst', outcome, genFinal)

-- | Severity at/above which a NON-VITAL part is "destroyed" (pulverised
--   / cut through). A destroyed part takes its attached children with
--   it: they are SEVERED. (Vital parts hitting this threshold die via
--   the normal injury→death rule instead.)
destroyThreshold ∷ Float
destroyThreshold = 1.0

-- | Reconcile severing: any part that is destroyed — a fracture or
--   severed wound at/above 'destroyThreshold' — severs its child parts
--   (a pulverised arm takes the hand with it). Idempotent: a severed
--   child gets a single severity-1 "severed" wound, which then makes IT
--   "destroyed" too, so the next tick severs ITS children — the loss
--   propagates down the limb over a couple of 10 Hz ticks. Adds nothing
--   when no part is destroyed (the common case), so it's cheap.
propagateSevering ∷ UnitDef → UnitInstance → UnitInstance
propagateSevering def inst =
    let parts     = udBodyParts def
        partIdx   = HM.fromList [(bpId p, p) | p ← parts]
        sevOf w   = woundSeverity w
        -- A part is destroyed by a pulverising fracture / a severing cut, OR
        -- by rotting completely (woundNecrosis ≥ threshold). Necrotic VITAL
        -- parts are excluded here — they kill via the gangrene death rule in
        -- tickOneUnit, they don't "fall off".
        destroyed = HS.fromList $
            [ woundPart w
            | w ← uiWounds inst
            , woundKind w ≡ "fracture" ∨ woundKind w ≡ "severed"
            , sevOf w ≥ destroyThreshold ]
            ++
            [ woundPart w
            | w ← uiWounds inst
            , woundNecrosis w ≥ necrosisDestroyThreshold
            , not (maybe False bpVital (HM.lookup (woundPart w) partIdx)) ]
        -- Children whose parent is destroyed and that aren't already
        -- severed.
        alreadySevered = HS.fromList
            [ woundPart w | w ← uiWounds inst, woundKind w ≡ "severed" ]
        toSever =
            [ bpId p
            | p ← parts
            , Just par ← [bpParent p]
            , HS.member par destroyed
            , not (HS.member (bpId p) alreadySevered) ]
    in if null toSever
       then inst
       else inst { uiWounds =
            [ Wound { woundPart = pid, woundKind = "severed"
                    , woundSeverity = 1.0, woundAt = woundAtNow
                    , woundBandage = 1.0, woundClot = 0.0, woundHeal = 0.0, woundDressing = ""
                    , woundInfection = 0.0, woundClean = False, woundInfectionType = ""
                    , woundNecrosis = 0.0 }
            | pid ← toSever ] <> uiWounds inst }
  where
    -- Reuse the freshest wound's timestamp (avoids threading gameTime
    -- through; severing is driven by existing wounds, so one exists).
    woundAtNow = case uiWounds inst of
        (w:_) → woundAt w
        []    → 0
