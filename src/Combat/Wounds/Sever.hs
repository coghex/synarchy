{-# LANGUAGE Strict, UnicodeSyntax, OverloadedStrings #-}

-- | Severing cascade: a destroyed body part takes its attached children
--   with it. See "Combat.Wounds" for the overall formula summary.
module Combat.Wounds.Sever
    ( destroyThreshold
    , propagateSevering
    ) where

import UPrelude
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import Unit.Types (UnitInstance(..), UnitDef(..), BodyPart(..), Wound(..))
import Combat.Wounds.Infection (necrosisDestroyThreshold)

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
