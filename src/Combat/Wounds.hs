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
                  , UnitManager(..), BodyPart(..), Wound(..))
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

healingBase ∷ Float
healingBase = 0.005   -- severity-0.1 scratch closes in ~20 game seconds

bleedClotThreshold ∷ Float
bleedClotThreshold = 0.001   -- L/sec — below this, the wound "closes"

woundCleanupThreshold ∷ Float
woundCleanupThreshold = 0.01

unconsciousFraction ∷ Float
unconsciousFraction = 0.30   -- pose flips to Collapsed below this

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
    in sum [ (woundSeverity w * woundSeverity w)
                * kindBleedFactor (woundKind w)
                * maybe 1.0 bpBleedFactor (HM.lookup (woundPart w) parts)
                * bleedScale / bleedCon
           | w ← uiWounds inst ]

-- | Per-kind healing-rate multiplier on `healingBase`. Soft-tissue
--   wounds (slash/stab/blunt) heal at the base rate (1.0); broken bones
--   and concussions mend much more slowly, so an injured unit stays
--   impaired for a long time rather than shrugging it off in seconds.
kindHealFactor ∷ Text → Float
kindHealFactor "fracture"   = 0.12
kindHealFactor "concussion" = 0.18
kindHealFactor "internal"   = 0.10
kindHealFactor "arterial"   = 0.15   -- a vessel can close, slowly
-- A severed part never grows back: zero healing keeps the severity
-- pinned at 1.0 (and the wound is never cleaned up), so the limb stays
-- gone for the unit's life. (Stump bleeding above still clots.)
kindHealFactor "severed"    = 0.0
kindHealFactor _            = 1.0

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
                                        tickOneUnit def dt inst
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
    ∷ UnitDef → Float → UnitInstance
    → (UnitInstance, WoundTickOutcome)
tickOneUnit def dt inst
    | uiPose inst == "dead" = (inst, NoChange)
    | null (uiWounds inst)  = (inst, NoChange)
    | otherwise =
        let parts = HM.fromList [(bpId p, p) | p ← udBodyParts def]
            con   = HM.lookupDefault 1.0 "constitution" (uiStats inst)
            bleedCon = max 0.5 (min 2.0 con)
            healCon  = max 0.3 (min 3.0 con)
            (newWounds, totalDrain, worstPart, _worstRate) =
                L.foldl' (\(ws, drain, wp, wr) w →
                    let p          = HM.lookup (woundPart w) parts
                        partBleed  = maybe 1.0 bpBleedFactor p
                        bleedRate  =
                              (woundSeverity w * woundSeverity w)
                            * kindBleedFactor (woundKind w)
                            * partBleed
                            * bleedScale
                            / bleedCon
                        healRate   = if bleedRate < bleedClotThreshold
                            then healingBase
                               * kindHealFactor (woundKind w)
                               * (1 - woundSeverity w)
                               * (1 - woundSeverity w)
                               * healCon
                            else 0
                        newSev     = woundSeverity w - healRate * dt
                        clamped    = max 0 newSev
                        w'         = w { woundSeverity = clamped }
                        (wp', wr') = if bleedRate > wr
                                     then (woundPart w, bleedRate)
                                     else (wp, wr)
                    in ( if clamped < woundCleanupThreshold
                            then ws
                            else w' : ws
                       , drain + bleedRate * dt
                       , wp', wr'))
                ([], 0.0, "torso", 0.0)
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
                    , woundSeverity = 1.0, woundAt = woundAtNow }
            | pid ← toSever ] <> uiWounds inst }
  where
    -- Reuse the freshest wound's timestamp (avoids threading gameTime
    -- through; severing is driven by existing wounds, so one exists).
    woundAtNow = case uiWounds inst of
        (w:_) → woundAt w
        []    → 0
