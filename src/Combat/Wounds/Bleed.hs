{-# LANGUAGE Strict, UnicodeSyntax, OverloadedStrings #-}

-- | Per-kind bleed-rate tuning and the current-bleed-rate query used by
--   the info panel. See "Combat.Wounds" for the overall formula summary.
module Combat.Wounds.Bleed
    ( kindBleedFactor
    , bleedRateFor
    , isExternallyBleedingKind
    , externalBleedRateFor
    , dominantExternalBleedWound
    ) where

import UPrelude
import Data.List (maximumBy)
import Data.Ord (comparing)
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

-- | The per-wound bleed-rate term (litres/second), shared by
--   'bleedRateFor', 'externalBleedRateFor', and
--   'dominantExternalBleedWound' — mirrors 'Combat.Wounds.Tick.tickOneUnit's
--   inline formula exactly (severity² × kind × part.bleed_factor ×
--   bandage × clot × bleedScale / clamp(con); see "Combat.Wounds" for
--   the overall formula summary).
perWoundBleedRate ∷ HM.HashMap Text BodyPart → Float → Wound → Float
perWoundBleedRate parts bleedCon w =
    let effSev = woundEffSeverity w
    in effSev * effSev
       * kindBleedFactor (woundKind w)
       * maybe 1.0 bpBleedFactor (HM.lookup (woundPart w) parts)
       * woundBandage w
       * (1 - woundClot w)
       * bleedScale / bleedCon

-- | Body-part lookup + clamped-constitution divisor shared by every
--   per-unit bleed query below — computed once per call site.
bleedContext ∷ UnitDef → UnitInstance → (HM.HashMap Text BodyPart, Float)
bleedContext def inst =
    ( HM.fromList [(bpId p, p) | p ← udBodyParts def]
    , max 0.5 (min 2.0 (HM.lookupDefault 1.0 "constitution" (uiStats inst)))
    )

-- | Current total bleed rate (litres/second) for a unit, summed over
--   EVERY wound (external + internal) — exposed so the info panel can
--   show "ml/s lost".
bleedRateFor ∷ UnitDef → UnitInstance → Float
bleedRateFor def inst =
    let (parts, bleedCon) = bleedContext def inst
    in sum [ perWoundBleedRate parts bleedCon w | w ← uiWounds inst ]

-- | Kinds that show up as VISIBLE external bleeding (drops, pooling, or
--   trail marks on the ground) as opposed to purely internal blood
--   loss. Mirrors docs/blood_decals.md's "Ongoing bleeding behavior"
--   exclusion list and scripts/unit_ai_medic.lua's @TREAT_SKIP_KINDS@:
--   `internal` has no open wound to leak from, a closed `fracture`'s
--   bleed is internal bruising around the break (not a stream), and a
--   `concussion` doesn't bleed at all (its 'kindBleedFactor' is already
--   0). Used by both the wound tick's conserved external-loss
--   accounting ("Combat.Wounds.Tick") and the bleeding-trail emitter
--   ("Blood.Trail", issue #882).
isExternallyBleedingKind ∷ Text → Bool
isExternallyBleedingKind kind = kind ∉ ["internal", "fracture", "concussion"]

-- | Like 'bleedRateFor' but summed over EXTERNALLY-visible wounds only
--   (see 'isExternallyBleedingKind') — the rate "Blood.Trail"'s emitter
--   checks to decide whether a unit still has anything left to track.
externalBleedRateFor ∷ UnitDef → UnitInstance → Float
externalBleedRateFor def inst =
    let (parts, bleedCon) = bleedContext def inst
    in sum [ perWoundBleedRate parts bleedCon w
           | w ← uiWounds inst, isExternallyBleedingKind (woundKind w) ]

-- | Among a unit's currently externally-bleeding wounds, the one
--   contributing the most to 'externalBleedRateFor' right now — the
--   representative wound "Blood.Trail"'s emitter tags a placed mark's
--   texture request with (style/severity mapping is still driven by
--   accumulated VOLUME, never by this wound's own severity — see
--   'Blood.Trail.trailBloodForVolume'). Nothing once nothing qualifies
--   (healed, clotted, or bandaged to a standstill).
dominantExternalBleedWound ∷ UnitDef → UnitInstance → Maybe Wound
dominantExternalBleedWound def inst =
    let (parts, bleedCon) = bleedContext def inst
        qualifying =
            [ (perWoundBleedRate parts bleedCon w, w)
            | w ← uiWounds inst, isExternallyBleedingKind (woundKind w)
            , perWoundBleedRate parts bleedCon w > 0 ]
    in case qualifying of
        [] → Nothing
        _  → Just (snd (maximumBy (comparing fst) qualifying))
