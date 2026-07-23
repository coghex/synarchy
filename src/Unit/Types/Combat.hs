{-# LANGUAGE Strict, UnicodeSyntax, DeriveGeneric, DeriveAnyClass #-}
-- | Combat body/weapon records, split out of "Unit.Types" (#575) —
--   re-exported there so the public API is unchanged.
module Unit.Types.Combat
    ( BodyPart(..)
    , NaturalWeapon(..)
    , StrikeProfile(..)
    , NaturalResistance(..)
    , defaultNaturalResistance
    ) where

import UPrelude
import GHC.Generics (Generic)

-- | A targetable body part on a creature. Body parts are declared
--   per-unit-def (humanoid acolytes ship a 12-part layout, quadruped
--   bears ship an 8-part layout). All numeric fields feed the
--   resolution formulas:
--
--     area_weight       — chance of a random hit landing here
--                         (probability ∝ weight among reachable parts).
--     tactical_value    — intelligence-weighted picker bias toward
--                         high-value parts (vitals usually 1.0,
--                         limbs ~0.3). Combined with area_weight via
--                         the unit's intelligence stat.
--     (part durability is DERIVED from the tissue layers — see
--      Unit.Injury.layerCapacity — not a hand-authored factor.)
--     bleed_factor      — multiplier on the per-wound bleed rate.
--                         Neck > head > torso > limbs (more major
--                         vessels near the centre line).
--     height_low/high   — vertical range of the part (metres above
--                         the unit's foot). Used by the reach-band
--                         filter so a low-stance bear's head is only
--                         reachable by a tall attacker or rearing
--                         posture.
--     vital             — destroying a vital part (severity ≥ 1.0)
--                         is instant death, with the cause string
--                         formatted as "<wound_kind>_<part_id>".
data BodyPart = BodyPart
    { bpId              ∷ !Text
    , bpName            ∷ !Text
      -- ^ Display name for the combat log ("left arm", "forearm",
      --   "radius"). Defaults to bpId when the YAML omits it.
    , bpParent          ∷ !(Maybe Text)
      -- ^ Attached part id (so severing an arm later cascades to
      --   the hand). Traversed by 'Combat.Wounds.propagateSevering'
      --   (destroyed parent → sever child), 'Unit.Fall.fallInjuries'
      --   (macro region → subparts), and 'Combat.Resolution' (subpart
      --   targeting); severity itself stays per-part.
    , bpVital           ∷ !Bool
    , bpAreaWeight      ∷ !Float
    , bpTacticalValue   ∷ !Float
    , bpBleedFactor     ∷ !Float
    , bpHeightLow       ∷ !Float    -- ^ metres above foot
    , bpHeightHigh      ∷ !Float    -- ^ metres above foot
    , bpLayers          ∷ ![(Text, Text, Float)]
      -- ^ Tissue stack, OUTER→INNER: (display name, substance, thickness mm).
      --   A strike penetrates these in order; the innermost is the vital
      --   core. The DISPLAY NAME is the noun the combat log uses ("radius",
      --   "intestines", "skin") — distinct named layers let one part list
      --   several bones/organs; the SUBSTANCE drives the wound kind +
      --   physics. Empty ⇒ a single default flesh layer. (Name defaults to
      --   the substance when the YAML omits it.)
    , bpTargetable      ∷ !Bool
      -- ^ True for the macro-parts an attack aims at (head, torso, an
      --   arm…) and that the combat log names ("…in the arm"). False for
      --   SUBPARTS (skull, carotid, femur, a lung) — these aren't aimed
      --   at directly; a hit on their parent macro-part is ALLOCATED down
      --   to them, and they carry the real tissue + injury detail. The
      --   whole tree is fully data-driven (humanoid → robot → fish): the
      --   engine reads targetable/parent/depth generically.
    , bpAffectsLocomotion ∷ !Bool
      -- ^ True for the direct mobility apparatus (legs, feet — the
      --   quadruped equivalents too). A wound here is what
      --   'Unit.Thread.Command.Body.injurySpeedMult' weights heaviest when
      --   scaling movement speed. Species with different leg-part
      --   naming (a robot's "hindquarter", say) just set this flag
      --   instead of the engine matching a fixed id list.
    , bpAffectsBalance  ∷ !Bool
      -- ^ True for core/balance-relevant parts (torso). A wound here
      --   gets the lesser locomotion penalty 'injurySpeedMult' applies
      --   — dizzying, not disabling.
    , bpDepth            ∷ !Float
      -- ^ 0 = at the surface, 1 = deepest. Drives the slash-swath: a cut
      --   that penetrates to depth D injures every sibling subpart with
      --   depth ≤ D (skin → windpipe → carotid → spine), and a thrust
      --   drives to the deepest subpart it can reach. Ignored for
      --   targetable macro-parts.
    } deriving (Show, Eq, Generic)

-- | A creature's innate (non-equipment) weapon — claws, fangs, fists.
--   Used by units with no equipped weapon, falling back to this
--   definition for attack range, cooldown, and weapon-class skill
--   selection. Acolytes don't declare one (they fight equipped);
--   bears declare an unarmed natural weapon.
data NaturalWeapon = NaturalWeapon
    { nwWeaponClass            ∷ !Text   -- ^ matches a skill name on the unit
    , nwEffectiveBladeLength   ∷ !Float  -- ^ cm; REACH only (which body parts
                                         --   the attack can touch). The
                                         --   damage geometry is per-kind below.
    , nwAttackCooldown         ∷ !Float  -- ^ seconds between swings
    -- Per-kind strike profiles. A natural weapon is really several
    -- distinct tools — a beast slashes/bites with keratin claws and
    -- enamel fangs, bludgeons with a bone-cored paw — so each attack
    -- kind carries its own material + geometry. (Manufactured weapons
    -- use one material for all three kinds; the runtime ResolvedStrike
    -- unifies them.)
    , nwSlash                  ∷ !StrikeProfile
    , nwStab                   ∷ !StrikeProfile
    , nwBlunt                  ∷ !StrikeProfile
    , nwComboAttack            ∷ !Bool
      -- ^ When True the slash/blunt facets fuse into ONE "paw" swing that
      --   delivers slash + blunt + a fraction of stab together (a raking
      --   bludgeon), while the stab facet remains a separate dedicated
      --   "bite". When False each kind is its own single-mechanism attack.
    } deriving (Show, Eq, Generic)

-- | One attack kind of a weapon, resolved to a material + geometry.
--   Cutting kinds (stab, slash) use blade length + sharpness; blunt
--   uses an impact area + striking mass. Fields irrelevant to a given
--   kind are left at 0 (and the combat formula ignores them). The
--   material name is looked up in the 'SubstanceManager' at combat time.
data StrikeProfile = StrikeProfile
    { spEff        ∷ !Float  -- ^ 0..1 effectiveness for this kind
    , spMaterial   ∷ !Text   -- ^ substance name ("keratin", "enamel", …)
    , spBladeCm    ∷ !Float  -- ^ edge/point length, cm (stab, slash)
    , spSharpness  ∷ !Float  -- ^ engineering scale, lower = sharper (stab, slash)
    , spImpactArea ∷ !Float  -- ^ mm² contact patch (blunt); 0 ⇒ derive
    , spMass       ∷ !Float  -- ^ kg mass of the striking appendage (paw/fang)
    , spLength     ∷ !Float  -- ^ cm; lever length of the appendage. 0 ⇒
                             --   fall back to spBladeCm.
    , spCenterOfMass ∷ !Float -- ^ 0..1 along the appendage from the limb
    , spName        ∷ !Text   -- ^ display name of this attack ("claws",
                              --   "fangs", "paw") for the combat log
    } deriving (Show, Eq, Generic)

-- | Innate per-attack-kind damage resistance baked into the unit's
--   hide/skin. Applied multiplicatively in Combat.Resolution before
--   the flat toughness reduction. 0.0 means "no innate resistance"
--   (humans); higher values reduce incoming damage of that kind.
--   Future armor stacks on top with diminishing returns.
data NaturalResistance = NaturalResistance
    { nrSlash ∷ !Float
    , nrStab  ∷ !Float
    , nrBlunt ∷ !Float
    } deriving (Show, Eq, Generic)

defaultNaturalResistance ∷ NaturalResistance
defaultNaturalResistance = NaturalResistance 0.0 0.0 0.0
