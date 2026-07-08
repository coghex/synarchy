{-# LANGUAGE Strict, UnicodeSyntax, OverloadedStrings #-}

-- | Per-kind bleed-rate tuning and the current-bleed-rate query used by
--   the info panel. See "Combat.Wounds" for the overall formula summary.
module Combat.Wounds.Bleed
    ( kindBleedFactor
    , bleedRateFor
    ) where

import UPrelude
import qualified Data.HashMap.Strict as HM
import Unit.Types (UnitInstance(..), UnitDef(..), BodyPart(..), Wound(..)
                  , woundEffSeverity)
import Combat.Wounds.Constants (bleedScale)

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
