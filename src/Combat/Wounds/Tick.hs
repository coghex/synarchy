{-# LANGUAGE Strict, UnicodeSyntax, OverloadedStrings #-}

-- | The per-tick wound orchestration entry point and the pure per-unit
--   wound tick. See "Combat.Wounds" for the overall formula summary.
module Combat.Wounds.Tick
    ( tickAllWounds
    , tickOneUnit         -- exposed for unit testing (pure per-unit wound tick)
    ) where

import UPrelude
import qualified Data.Text as T
import qualified Data.HashMap.Strict as HM
import qualified Data.Sequence as Seq
import qualified Data.List as L
import Data.IORef (readIORef, atomicModifyIORef')
import System.Environment (lookupEnv)
import Combat.Types (CombatEvent(..))
import Engine.Core.State (EngineEnv(..), activeWorldState)
import Engine.Core.Log (logDebug, LogCategory(..))
import qualified Engine.Core.Queue as Q
import Unit.Types (UnitId(..), UnitInstance(..), UnitDef(..)
                  , UnitManager(..), BodyPart(..), Wound(..)
                  , Scar(..), bloodMassRatio)
import Unit.Command.Types (UnitCommand(..))
import qualified System.Random as Random
import World.State.Types (WorldState(..))
import World.Generate.Types (WorldGenParams(..))
import World.Weather.Lookup (lookupLocalClimate, LocalClimate(..))
import Infection.Types (InfectionManager, InfectionDef(..), lookupInfection)
import Combat.Wounds.Constants
    ( bleedScale, woundCleanupThreshold, unconsciousFraction
    , clotBaseRate, clotSeed, clotPressureK, kindClotFactor
    )
import Combat.Wounds.Healing
    ( healBaseRate, healClotFloor, sleepHealMult, scarSeverityThreshold
    , calorieHealMultiplier
    )
import Combat.Wounds.Infection
    ( infectionBaseRate, infectionGraceSec, testInfectionBaseRate
    , testInfectionGraceSec, infectionTestModeVar, infectionWorsenThreshold
    , infectionWorsenRate, feverSuppressK, feverSuppressFloor
    , necrosisBaseRate, necrosisInfThreshold, gasNecrosisMult
    , necrosisLethalVital, woundHealFloor, kindInfectFactor
    , climateOnsetFactor, woundSiteClass, selectInfectionType
    , aggressivenessOf, infectabilityOf, immuneResistOf, selfAccelGain
    , immuneRampRate, immuneSeed, immuneDecayRate, immuneClearRate
    , infectionLoadThreshold, immGainRate, immunityDecayRate, immunityFloor
    )
import Combat.Wounds.Bleed (kindBleedFactor)
import Combat.Wounds.Sever (propagateSevering)

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
            -- sleepHealMult, everything else uses 1.0. The sleep AI
            -- (#612, PR #640) drives uiPose to "sleeping" via the
            -- go_to_sleep goal's real Pose chain; bears use the same
            -- shared goal since #613 (PR #659), not a bearPosture-only
            -- side channel.
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
