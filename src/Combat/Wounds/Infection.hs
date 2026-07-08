{-# LANGUAGE Strict, UnicodeSyntax, OverloadedStrings #-}

-- | Wound infection, necrosis, and immune-response tuning + pure
--   selection logic. See "Combat.Wounds" for the overall formula
--   summary.
module Combat.Wounds.Infection
    ( infectionBaseRate
    , infectionGraceSec
    , testInfectionBaseRate
    , testInfectionGraceSec
    , infectionTestModeVar
    , infectionWorsenThreshold
    , infectionWorsenRate
    , feverSuppressK
    , feverSuppressFloor
    , necrosisBaseRate
    , necrosisInfThreshold
    , gasNecrosisMult
    , necrosisDestroyThreshold
    , necrosisLethalVital
    , woundHealFloor
    , kindInfectFactor
    , climateOnsetFactor
    , climateMatchWeight
    , woundSiteClass
    , selectInfectionType
    , aggressivenessOf
    , infectabilityOf
    , selfAccelGain
    , immuneRampRate
    , immuneSeed
    , immuneDecayRate
    , immuneClearRate
    , infectionLoadThreshold
    , immGainRate
    , immunityDecayRate
    , immunityFloor
    , immuneResistOf
    ) where

import UPrelude
import qualified Data.Text as T
import qualified Data.HashMap.Strict as HM
import qualified System.Random as Random
import Unit.Types (BodyPart(..), Wound(..))
import World.Weather.Lookup (LocalClimate(..))
import Infection.Types (InfectionManager, InfectionDef(..)
                       , infectionsForSite, lookupInfection)

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
