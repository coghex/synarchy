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
--     healingBase × (1 - severity)^2 × clamp(constitution, 0.3, 3.0)
--   (only applied if the wound's bleed_per_sec < bleedClotThreshold —
--   actively-bleeding wounds don't heal yet; Phase 3 layers in
--   clotting time / first-aid.)
--
-- Wounds with severity < woundCleanupThreshold are dropped from the
-- list to keep the per-tick scan cheap.
--
-- All blood values are litres; bodyMass-derived max_blood is
-- recomputed live so wasting/regrowth carries through naturally.
module Combat.Wounds
    ( tickAllWounds
    , propagateSevering   -- exposed for unit testing
    , bleedRateFor        -- current L/sec blood loss (for the info panel)
    , kindBleedFactor     -- per-kind bleed multiplier (treat-action ranking)
    ) where

import UPrelude
import qualified Data.Text as T
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import qualified Data.Sequence as Seq
import qualified Data.List as L
import Data.IORef (readIORef, atomicModifyIORef')
import Combat.Types (CombatEvent(..))
import Engine.Core.State (EngineEnv(..))
import Engine.Core.Log (logDebug, LogCategory(..))
import qualified Engine.Core.Queue as Q
import Unit.Types (UnitId(..), UnitInstance(..), UnitDef(..)
                  , UnitManager(..), BodyPart(..), Wound(..), Scar(..))
import Unit.Command.Types (UnitCommand(..))

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
sleepHealMult = 4.0    -- rest/sleep speed-up (DORMANT — see restMult)

scarSeverityThreshold ∷ Float
scarSeverityThreshold = 0.3   -- wounds milder than this heal scar-free

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
    in sum [ let effSev = woundSeverity w * (1 - woundHeal w)
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
    | DiedNow Text          -- worst-bleeding part id (= cause)

-- | Tick all units' wounds by `dt` seconds. Designed for the combat
--   thread to call on its 10 Hz cadence (every 6th 60-Hz iteration).
--   Mutates uiWounds / uiBlood inline; pose changes are enqueued
--   onto the unit command queue.
tickAllWounds ∷ EngineEnv → Float → IO ()
tickAllWounds env dt = do
    gt ← readIORef (gameTimeRef env)
    -- Snapshot-and-clobber would lose any concurrent write to
    -- umInstances (publishToRender, Lua equip/modifier, the per-attack
    -- wound stamp in Combat.Resolution). Do the fold inside the
    -- atomicModifyIORef' lambda so wound updates merge with the
    -- current map instead of overwriting it. Same pattern as
    -- Unit.Thread.publishToRender.
    outcomes ← atomicModifyIORef' (unitManagerRef env) $ \um →
        let defs = umDefs um
            (updatedMap, os) =
                HM.foldlWithKey'
                    (\(acc, xs) uid inst →
                        case HM.lookup (uiDefName inst) defs of
                            Nothing → (HM.insert uid inst acc, xs)
                            Just def →
                                let (inst', outcome) =
                                        tickOneUnit gt def dt inst
                                in case outcome of
                                    NoChange →
                                        (HM.insert uid inst' acc, xs)
                                    _ →
                                        ( HM.insert uid inst' acc
                                        , (uid, outcome) : xs))
                    (HM.empty, []) (umInstances um)
        in (um { umInstances = updatedMap }, os)

    logger ← readIORef (loggerRef env)
    mapM_ (\(uid@(UnitId uidRaw), outcome) → case outcome of
        UnconsciousNow part → do
            Q.writeQueue (unitQueue env) (UnitCollapse uid)
            logDebug logger CatThread $
                "collapsed: " <> T.pack (show uidRaw)
                    <> " (bleeding from " <> part <> ")"
        DiedNow part → do
            Q.writeQueue (unitQueue env) (UnitKill uid)
            let ev = CombatEvent
                    { ceTs       = gt
                    , ceKind     = "death"
                    , ceAttacker = Nothing
                    , ceTarget   = Just uidRaw
                    , cePayload  = HM.fromList
                        [ ("cause", "exsanguination")
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
    ∷ Double → UnitDef → Float → UnitInstance
    → (UnitInstance, WoundTickOutcome)
tickOneUnit gt def dt inst
    | uiPose inst == "dead" = (inst, NoChange)
    | null (uiWounds inst)  = (inst, NoChange)
    | otherwise =
        let parts = HM.fromList [(bpId p, p) | p ← udBodyParts def]
            con   = HM.lookupDefault 1.0 "constitution" (uiStats inst)
            bleedCon = max 0.5 (min 2.0 con)
            healCon  = max 0.3 (min 3.0 con)
            -- A unit at rest heals faster. DORMANT hook: no unit is
            -- currently "sleeping" (acolytes have no sleep/bed system
            -- yet — only the bear AI uses the word). When one lands,
            -- wire its rest state here and the bonus lights up.
            restMult = if uiPose inst == "sleeping" then sleepHealMult else 1.0
            (newWounds, totalDrain, worstPart, _worstRate, newScars) =
                L.foldl' (\(ws, drain, wp, wr, scars) w →
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
                        -- Advance healing. Uniform base rate across kinds
                        -- (severed never heals — the limb is gone),
                        -- SCALED by clot (an open wound barely mends) and
                        -- by rest, and gently by constitution. Quite slow.
                        canHeal    = woundKind w /= "severed"
                        clotHealF  = healClotFloor + (1 - healClotFloor) * newClot
                        healDelta  = if canHeal
                            then healBaseRate * clotHealF * restMult * healCon * dt
                            else 0
                        newHeal    = min 1 (woundHeal w + healDelta)
                        -- Effective severity drops as the wound heals —
                        -- the single source of "this wound matters less now".
                        effSev     = sev0 * (1 - newHeal)
                        bleedRate  =
                              (effSev * effSev)
                            * kindBleedFactor (woundKind w)
                            * partBleed
                            * woundBandage w     -- first-aid dressing
                            * (1 - newClot)      -- clotting
                            * bleedScale
                            / bleedCon
                        w'         = w { woundClot = newClot
                                      , woundHeal = newHeal }
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
                       , wp', wr', scars' ))
                ([], 0.0, "torso", 0.0, [])
                (uiWounds inst)
            newBlood   = uiBlood inst - totalDrain
            bodyMass   = HM.lookupDefault 70.0 "body_mass" (uiStats inst)
            maxBlood   = bodyMass * 0.075
            unconsCut  = maxBlood * unconsciousFraction
            newWoundsR = reverse newWounds
            -- Edge-triggered: fire DiedNow only on the tick blood
            -- crosses zero. Without the `uiBlood inst > 0` guard, a
            -- unit that died last tick (uiBlood = 0) would re-emit
            -- DiedNow every tick until the unit thread's UnitKill
            -- processing snaps uiPose, which is up to one combat tick
            -- away. Matches the existing UnconsciousNow gating, which
            -- already checks uiPose ≠ "collapsed" for the same reason.
            outcome
                | uiBlood inst > 0, newBlood ≤ 0 = DiedNow worstPart
                | newBlood < unconsCut, uiPose inst /= "collapsed"
                                     = UnconsciousNow worstPart
                | otherwise          = NoChange
            inst' = inst
                { uiWounds = newWoundsR
                , uiScars  = newScars <> uiScars inst
                , uiBlood  = max 0 newBlood
                }
        in (propagateSevering def inst', outcome)

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
        sevOf w   = woundSeverity w
        destroyed = HS.fromList
            [ woundPart w
            | w ← uiWounds inst
            , woundKind w ≡ "fracture" ∨ woundKind w ≡ "severed"
            , sevOf w ≥ destroyThreshold ]
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
                    , woundBandage = 1.0, woundClot = 0.0, woundHeal = 0.0, woundDressing = "" }
            | pid ← toSever ] <> uiWounds inst }
  where
    -- Reuse the freshest wound's timestamp (avoids threading gameTime
    -- through; severing is driven by existing wounds, so one exists).
    woundAtNow = case uiWounds inst of
        (w:_) → woundAt w
        []    → 0
