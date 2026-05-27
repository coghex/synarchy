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
    ) where

import UPrelude
import qualified Data.Text as T
import qualified Data.HashMap.Strict as HM
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
kindBleedFactor "slash" = 1.0
kindBleedFactor "stab"  = 0.6
kindBleedFactor "blunt" = 0.2
kindBleedFactor _       = 0.5

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
            outcome
                | newBlood ≤ 0       = DiedNow worstPart
                | newBlood < unconsCut, uiPose inst /= "collapsed"
                                     = UnconsciousNow worstPart
                | otherwise          = NoChange
            inst' = inst
                { uiWounds = newWoundsR
                , uiBlood  = max 0 newBlood
                }
        in (inst', outcome)
