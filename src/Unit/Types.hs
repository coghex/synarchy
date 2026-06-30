{-# LANGUAGE Strict, UnicodeSyntax, DeriveGeneric, DeriveAnyClass #-}
module Unit.Types
    ( Animation(..)
    , StatModifier(..)
    , Wound(..)
    , woundEffSeverity
    , Scar(..)
    , BodyPart(..)
    , NaturalWeapon(..)
    , StrikeProfile(..)
    , NaturalResistance(..)
    , defaultNaturalResistance
    , NamePool(..)
    , UnitDef(..)
    , UnitInstance(..)
    , UnitManager(..)
    , UnitId(..)
    , emptyUnitManager
    , nextUnitId
    , unitsOnPages
    , unitsOnPage
    ) where

import UPrelude
import GHC.Generics (Generic)
import Data.Hashable (Hashable)
import Data.Serialize (Serialize)
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import qualified Data.Map.Strict as Map
import qualified Data.Vector as V
import Engine.Asset.Handle (TextureHandle(..))
import Item.Types (ItemInstance(..))
import Unit.Direction (Direction(..))
import World.Page.Types (WorldPageId(..))

-- | A single animation: per-direction frame sequences.
--
--   Missing directions either fall back to T-pose (`aFlip = False`) or
--   are mirrored from their eastern-half counterpart (`aFlip = True` —
--   SW/W/NW pull from SE/E/NE with horizontally swapped UVs). The flag
--   is set per animation in YAML; default is False so an asset author
--   who forgets the flag and lists only 5 dirs sees obvious T-pose
--   fallbacks rather than silently-mirrored weapon hands.
data Animation = Animation
    { aFps    ∷ !Float
    , aLoop   ∷ !Bool
    , aFlip   ∷ !Bool
    , aFrames ∷ !(Map.Map Direction (V.Vector TextureHandle))
    } deriving (Show, Eq)

-- | One modifier on a stat. Multiple modifiers on the same stat
--   compose (after expiry filtering): deltas sum onto the base, then
--   percents sum into one multiplier —
--   @effective = (base + Σdelta) × (1 + Σpercent)@.
--   The (source, stat) pair is unique within a unit — re-adding the
--   same source on the same stat overwrites the previous entry.
--
--   Field order is load-bearing: Generic Serialize is positional, so
--   new fields go at the END (smPercent appended for save v33).
data StatModifier = StatModifier
    { smDelta  ∷ !Float
      -- ^ added to the base; can be negative (debuff).
    , smSource ∷ !Text
      -- ^ logical owner: "poison-A", "age", "wounded-left-arm", etc.
    , smExpiry ∷ !(Maybe Double)
      -- ^ Game-time seconds (gameTimeRef value) when the modifier
      --   becomes inactive. Nothing = permanent (removed only via
      --   removeModifier). Anchored to gameTimeRef rather than POSIX
      --   so the expiry survives save/load.
    , smPercent ∷ !Float
      -- ^ Fractional multiplier contribution: 0.5 = +50%, -0.25 =
      --   -25%, 0 = purely additive. Applied after all deltas.
    } deriving (Show, Eq, Generic, Serialize)

-- | A single wound record on a unit. Slim by design: every derived
--   property (bleed rate, heal rate, pain contribution) is computed
--   live from `woundKind` + `woundSeverity` + body-part metadata, so
--   the schema doesn't have to expand when those formulas change.
--
--   Severity is continuous 0..1; ≥1.0 on a vital part triggers
--   instant death; wounds with severity below 0.01 are removed by
--   the wound tick. `woundAt` lets the UI sort newest-first and
--   show "X seconds ago" labels.
data Wound = Wound
    { woundPart     ∷ !Text
      -- ^ body-part id (matches a `BodyPart.bpId` in the unit-def).
    , woundKind     ∷ !Text
      -- ^ "slash" / "stab" / "blunt" — drives bleed_factor, pain
      --   factor, healing characteristics, and combat-log flavor.
    , woundSeverity ∷ !Float
      -- ^ current 0..1; ticks down via natural healing, ticks up
      --   only when a new hit on the same part is grouped in
      --   (Phase 3 will fold compatible new wounds into existing
      --   ones; Phase 2.1 just appends).
    , woundAt       ∷ !Double
      -- ^ gameTime (seconds) when inflicted.
    , woundBandage  ∷ !Float
      -- ^ first-aid bleed multiplier: the FRACTION of this wound's
      --   natural bleed that still seeps after dressing. 1.0 =
      --   untreated (full bleed, the default for a fresh wound); 0.05
      --   = a competent bandage (5% seeps); 0.0 = a perfect dressing
      --   that stops the bleed entirely. Set by `unit.treatBleeding`
      --   (Combat.Wounds multiplies the per-wound bleed by this). A
      --   well-dressed wound's effective bleed drops below the clot
      --   threshold, so it then heals naturally.
    , woundClot     ∷ !Float
      -- ^ clotting progress 0..1. Fills over time in the wound tick
      --   (Combat.Wounds); the per-wound bleed is multiplied by
      --   (1 − woundClot), so at 1.0 the bleed has stopped entirely.
      --   Starts at 0 on a fresh wound. The clot rate ACCELERATES as
      --   it forms, scales DOWN with severity and dangerous kinds
      --   (arterial/severed barely self-clot), and is boosted by a
      --   bandage (pressure). Distinct from healing: clotting stops the
      --   bleed, healing then mends the tissue.
    , woundHeal     ∷ !Float
      -- ^ healing progress 0..1. Fills slowly in the wound tick once
      --   the wound has clotted (the rate scales with woundClot; an
      --   open wound barely heals). woundSeverity is the INFLICTED
      --   severity (static); the wound's EFFECTIVE severity — what
      --   drives bleed magnitude, pain, and impairment — is
      --   woundSeverity × (1 − woundHeal), so a unit recovers function
      --   as it heals. When effective severity falls below cleanup
      --   threshold the wound is removed, leaving a Scar if it was
      --   severe enough. Severed wounds don't heal (the limb is gone).
    , woundDressing ∷ !Text
      -- ^ what first-aid is on the wound: "" (none), "bandage" (a
      --   proper kit dressing), or "tourniquet" (the improvised
      --   no-supplies fallback — crude, poor seep). Cosmetic/record:
      --   the seep that actually scales bleed is woundBandage. Set by
      --   `unit.treatBleeding`; drives the Status-tab label.
    , woundInfection ∷ !Float
      -- ^ infection level 0..1. Grows in the wound tick (Combat.Wounds)
      --   on an open, undressed wound — proportional to severity and the
      --   wound kind (deep/dirty stab/severed worst; closed
      --   fracture/concussion least). An infected wound barely heals and,
      --   past a threshold, WORSENS (woundHeal reverses → effective
      --   severity climbs above the inflicted value). DETERMINISTIC: a
      --   wound marked `woundClean` (disinfected with antiseptic) never
      --   grows it. Drives the systemic SEPSIS failure-meter (a delayed
      --   death pathway) and is the thing ANTIBIOTICS cure.
    , woundClean   ∷ !Bool
      -- ^ disinfected: True once antiseptic has been applied (during
      --   treatBleeding, or the antibiotics cure). A clean wound does not
      --   accumulate `woundInfection`. This is the PREVENTION half of the
      --   medical loop — promptly cleaning a wound stops infection before
      --   it starts; antibiotics are the CURE for one that already set in.
    , woundInfectionType ∷ !Text
      -- ^ which infection took hold, as an InfectionDef id ("staph",
      --   "clostridium", …) or "" before any. Selected (weighted-random
      --   by the wound's site + the local climate) the moment infection
      --   first festers; drives the per-type growth aggressiveness, the
      --   cure rule (antibiotics treat only `category: bacterial`), and the
      --   display name/icon. Data-driven from data/infections/*.yaml.
    , woundNecrosis  ∷ !Float
      -- ^ dead tissue 0..1 from a NECROTIC infection (one whose def has the
      --   "necrosis" effect — gangrene). Accrues while such an infection is
      --   established (∝ infection level; "gas" rots faster) and is
      --   PERMANENT (dead tissue never recovers — clearing the infection
      --   only stops further rot). Raises effective severity; at 1.0 a
      --   non-vital part rots off (severing cascade), a vital part → death
      --   by gangrene. Future: a debridement action removes it.
    } deriving (Show, Eq, Generic, Serialize)

-- | The authoritative EFFECTIVE severity of a wound — what actually
--   drives bleed magnitude, pain, impairment, medic priority, and the
--   injured-animation flag. Healing eases it (@sev × (1 − heal)@, which
--   climbs back above the inflicted value when a festering wound's heal
--   goes negative), while necrosis (dead tissue) is a PERMANENT floor:
--   a rotting wound is at least as bad as the fraction of tissue that
--   has died. Mirrors the per-tick @effSev@ in
--   'Combat.Wounds.tickOneUnit' (the source of truth) so every
--   downstream consumer stays in lockstep with it.
woundEffSeverity ∷ Wound → Float
woundEffSeverity w =
    max (woundSeverity w * (1 - woundHeal w)) (woundNecrosis w)

-- | A healed-over wound left as a permanent mark. Descriptive record
--   for now (shown in the unit's Status tab); the data is here to hang
--   visuals or minor gameplay effects on later. Created when a wound
--   finishes healing and its inflicted severity was above the scar
--   threshold (scratches heal clean).
data Scar = Scar
    { scarPart     ∷ !Text     -- ^ body-part id the wound was on
    , scarKind     ∷ !Text     -- ^ the wound kind that left it (slash/…)
    , scarSeverity ∷ !Float     -- ^ the wound's INFLICTED severity (how bad)
    , scarAt       ∷ !Double    -- ^ gameTime when it finished healing
    } deriving (Show, Eq, Generic, Serialize)

-- | Unique identifier for a spawned unit instance.
newtype UnitId = UnitId { unUnitId ∷ Word32 }
    deriving (Show, Eq, Ord, Generic, Hashable, Serialize)

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
      --   the hand). For 2.1 the parent chain is read but not yet
      --   acted on; severity is per-part.
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

-- | A pool of names a unit type can draw from at spawn (#264). `npGiven`
--   is required for a unit to be named; `npFamily` is optional — a pool
--   with only given names yields single-token names. An empty pool means
--   the unit type has no personal names and falls back to its species
--   label in the UI. Loaded from data/names/<id>.yaml, referenced from a
--   unit def's `name_pool:` key.
data NamePool = NamePool
    { npGiven  ∷ ![Text]
    , npFamily ∷ ![Text]
    } deriving (Show, Eq, Generic)

-- | A unit definition (loaded from YAML, immutable after init).
--   This is the "template" — one per unit type.
data UnitDef = UnitDef
    { udName       ∷ !Text                            -- ^ e.g. "acolyte"
    , udNamePool   ∷ !(Maybe NamePool)
      -- ^ resolved name pool for this unit type, or Nothing if the def
      --   declares no `name_pool` (animals). When present, spawn draws a
      --   persistent personal name into `uiName` (#264).
    , udDisplayName ∷ !(Maybe Text)
      -- ^ optional human-readable species label ("Brown Bear") for the
      --   UI. Nothing → the prettified def name is used. Surfaced through
      --   unit.getInfo as `displayName`.
    , udTexture    ∷ !TextureHandle                   -- ^ default sprite handle
    , udPortrait   ∷ !(Maybe TextureHandle)
      -- ^ optional authored portrait for the info panel (YAML `portrait:`).
      --   Nothing → the UI falls back to the unit's live animation frame.
    , udDirSprites ∷ !(Map.Map Direction TextureHandle)
      -- ^ directional sprite overrides (may be empty)
    , udBaseWidth  ∷ !Float                           -- ^ ground contact diameter
    , udMaxSpeed   ∷ !Float
      -- ^ Per-species reference movement speed in tiles/sec — the top
      --   speed at agility 1.0. Actual sprint = max_speed × agility
      --   (computed Lua-side), so an exceptionally agile individual
      --   exceeds this. AI candidates derive comfort / ordered / sprint
      --   speeds from it + the unit's stats.
    , udRunThreshold ∷ !Float
      -- ^ Gait threshold as a fraction of udMaxSpeed: the sim renders
      --   the Running animation instead of Walking when the current move
      --   speed exceeds udRunThreshold × udMaxSpeed. Per-species (default
      --   0.6) so a lumbering unit breaks into a run later than a nimble
      --   one. Purely visual — independent of the comfort/sprint regime.
    , udAnimations ∷ !(HM.HashMap Text Animation)
      -- ^ named animation library (may be empty)
    , udStateAnims ∷ !(HM.HashMap Text Text)
      -- ^ state name → animation name (e.g. "idle" → "idle-standing")
    , udEagerStats    ∷ !Bool
      -- ^ if true, roll all stats at spawn; otherwise lazy on first read
    , udStatTemplates ∷ !(HM.HashMap Text (Float, Float))
      -- ^ stat name → (base, range). Empty means no rollable stats.
    , udBodyTemplates ∷ !(HM.HashMap Text (Float, Float))
      -- ^ bulk / bodyfat → (mean, range). Rolled at spawn ONLY,
      --   consumed by seedBodyComposition, then dropped. Kept off
      --   `udStatTemplates` so `unit.getStat(uid, "bulk")` never
      --   lazy-rolls a fresh value disconnected from the body
      --   composition the unit was actually spawned with.
    , udSkillTemplates ∷ !(HM.HashMap Text (Float, Float))
      -- ^ skill name → (base, range). Always eager-rolled at spawn.
      --   Skills are continuous floats; XP is applied via a closed-
      --   form formula (see Unit.Stats.applySkillXP) — there's no
      --   per-level threshold to store.
    , udKnowledgeTemplates ∷ !(HM.HashMap Text (Float, Float))
      -- ^ knowledge name → (base, range), rolled at spawn into uiKnowledge.
      --   Knowledge is like a skill but GATED on being "known": a unit only
      --   trains a knowledge it possesses (acquired from a book/teacher, or
      --   spawned-known via this template). Presence in uiKnowledge = known;
      --   the float is the level. Effective capability = level × the
      --   relevant stat (e.g. intelligence) at the point of use.
    , udStartingInventory ∷ ![(Text, Maybe Float, Int)]
      -- ^ ItemDef name + optional initial fill + drop priority.
      --   Materialised into `uiInventory` at spawn by looking each
      --   entry up in the ItemManager; unknown names are dropped with
      --   a log warning. Drop priority > 0 marks the item sheddable
      --   by the spawn-time capacity check (highest sheds first);
      --   0 = always granted.
    , udEquipmentClass ∷ !(Maybe Text)
      -- ^ Name of the EquipmentClass this unit type uses (e.g.
      --   "humanoid"). Nothing means the unit has no equipment UI
      --   at all (e.g. a wandering animal). Resolved against the
      --   EquipmentClassManager at UI-build time, not at spawn.
    , udStartingEquipment ∷ !(HM.HashMap Text Text)
      -- ^ slot id → item def name. Materialised into `uiEquipment`
      --   at spawn, validating each item's `idKind` against the
      --   slot's `esKind`. Mismatches log a warning and are dropped.
      --   Empty unless the unit type ships pre-equipped (acolytes
      --   start with armor, gauntlets, boots, dagger).
    , udStartingAccessories ∷ ![Text]
      -- ^ Item def names to materialise into `uiAccessories` at
      --   spawn — robes, goggles, etc. Order preserved.
    , udBodyParts ∷ ![BodyPart]
      -- ^ Body parts targetable by combat. Order is preserved
      --   (the YAML's order — typically vital → limbs).
      --   Empty list = no combat targeting yet; resolver bails
      --   cleanly. Acolyte ships 12-part humanoid, bear 8-part
      --   quadruped.
    , udNaturalResistance ∷ !NaturalResistance
      -- ^ Innate hide/skin resistance per attack kind. Defaults to
      --   zero everywhere (defaultNaturalResistance) for units that
      --   don't declare a `natural_resistance:` block in YAML.
    , udNaturalWeapon ∷ !(Maybe NaturalWeapon)
      -- ^ Optional innate weapon (claws/fangs/fists). When present,
      --   combat code uses it whenever no equipped weapon is found.
      --   Acolytes have Nothing here (they're expected to fight with
      --   their equipped dagger); bears declare an "unarmed" natural
      --   weapon.
    , udModifiers ∷ ![(Text, StatModifier)]
      -- ^ Permanent (stat name, modifier) pairs seeded into
      --   `uiModifiers` at spawn — the species' innate buffs
      --   (technomule: carrying_capacity +50% "cybernetic
      --   enhancements"). Show in tooltips like any other modifier.
    } deriving (Show, Eq)

-- | A spawned unit instance in the world.
--   Engine is agnostic to player vs NPC — Lua drives behavior.
data UnitInstance = UnitInstance
    { uiDefName    ∷ !Text           -- ^ which UnitDef this came from
    , uiName       ∷ !Text
      -- ^ persistent per-unit display name, drawn from the def's name
      --   pool at spawn (#264). Empty for unnamed units (animals): the
      --   UI then falls back to the species label. Roundtrips through
      --   SaveData (v57+).
    , uiPage       ∷ !WorldPageId
      -- ^ which world this unit belongs to. Runtime-only (not serialized
      --   — a save holds one world; loaded units are stamped with the
      --   load target page). Scopes queries/selection/render/hit-test so a
      --   unit spawned in one world never leaks into another (#78).
    , uiTexture    ∷ !TextureHandle  -- ^ current display texture (fallback)
    , uiDirSprites ∷ !(Map.Map Direction TextureHandle)
      -- ^ copied from UnitDef at spawn time
    , uiBaseWidth  ∷ !Float
    , uiGridX      ∷ !Float
    , uiGridY      ∷ !Float
    , uiGridZ      ∷ !Int
    -- | Continuous vertical position mirrored from `usRealZ`.
    --   The renderer uses this for smooth Z transitions (climbing,
    --   future features); cull / slice math still uses the integer
    --   uiGridZ. Outside of a climb, uiRealZ == fromIntegral uiGridZ.
    , uiRealZ      ∷ !Float
    , uiFacing     ∷ !Direction      -- ^ world-space facing (from sim)
    , uiCurrentAnim ∷ !Text          -- ^ resolved anim name; "" = T-pose
    , uiAnimStart   ∷ !Double        -- ^ POSIX seconds when anim began
    , uiAnimReverse ∷ !Bool
      -- ^ when True, pickFrame inverts the frame index. Set by
      --   publishToRender for transition activities that move from a
      --   higher-depth pose to a lower-depth one (the same asset is
      --   shared with the forward direction).
    , uiActivity    ∷ !Text
      -- ^ Mirror of sim usState as a Text label: "idle" / "walking" /
      --   "drinking" / "pickup" / "transitioning". Pose is published
      --   separately as `uiPose`.
    , uiPose        ∷ !Text
      -- ^ Mirror of sim usPose: "standing" / "crouching" / "crawling"
      --   / "collapsed". Read by Lua via `unit.getPose`.
    , uiAnimStride  ∷ !Int
      -- ^ Frame stride for the current anim. 1 = play all frames.
      --   N>1 = skip frames (render every Nth), making the anim
      --   complete in 1/N of its normal duration. Set by
      --   publishToRender from the sim-side `usTransitionStride`
      --   while transitioning; otherwise 1.
    , uiStats       ∷ !(HM.HashMap Text Float)
      -- ^ rolled stat values. May be empty if the def is lazy and no
      --   stat has been queried yet; entries are filled on demand.
    , uiModifiers   ∷ !(HM.HashMap Text [StatModifier])
      -- ^ per-name list of active modifiers. The keys are stat OR
      --   skill names — modifiers apply to whichever the unit has
      --   under that name. Expired entries are filtered out at read
      --   time, not on add — keep the list short (a few dozen
      --   entries per unit max).
    , uiSkills      ∷ !(HM.HashMap Text Float)
      -- ^ skill level values. Rolled at spawn from udSkillTemplates.
      --   XP is applied immediately via Unit.Stats.applySkillXP —
      --   each call directly nudges the level by an amount that
      --   shrinks quadratically with the current level. No XP
      --   accumulator is stored.
    , uiKnowledge   ∷ !(HM.HashMap Text Float)
      -- ^ KNOWN knowledge → level. Like uiSkills, but a key's PRESENCE
      --   means the unit knows it (absent = unknown); rolled at spawn from
      --   udKnowledgeTemplates, gained later from a source (book/teacher).
    , uiInventory   ∷ ![ItemInstance]
      -- ^ Item instances carried by the unit. Populated at spawn from
      --   the def's starting_inventory. Mutable: drinking decrements
      --   a canteen's fill, refilling tops it up. Order is preserved
      --   for stable UI display.
    , uiEquipment   ∷ !(HM.HashMap Text ItemInstance)
      -- ^ Slot id → equipped item. Populated at spawn from the def's
      --   `udStartingEquipment` (kind-validated). Mutated by
      --   equipment.equip / equipment.unequip from Lua. Slots not
      --   present in the map are empty. Save/load roundtrips.
    , uiAccessories ∷ ![ItemInstance]
      -- ^ Items the unit is wearing that don't fit a specific slot —
      --   robes, goggles, rings, amulets, etc. Order preserved for
      --   stable UI display. Populated at spawn from the def's
      --   `udStartingAccessories`. Mutated by equipment.equipAccessory
      --   / unequipAccessory.
    , uiFactionId   ∷ !Text
      -- ^ Faction tag used by hostile/friendly checks in the combat
      --   layer. Assigned at spawn-time only (no faction field on
      --   UnitDef): player-spawn paths pass "player"; everything
      --   else defaults to "wildlife". Same-id => friendly; different
      --   ids => attackable. Roundtrips through SaveData (v8+).
    , uiWounds      ∷ ![Wound]
      -- ^ Newest-first wound list. Mutated by Combat.Resolution on
      --   hits and by Combat.Wounds during the per-tick heal pass.
      --   Wounds whose EFFECTIVE severity falls below 0.01 are
      --   auto-removed (healed); vital wounds inflicted ≥1.0 trigger
      --   instant death (set in Combat.Resolution).
    , uiScars       ∷ ![Scar]
      -- ^ Permanent marks left by severe wounds that finished healing.
      --   Appended by Combat.Wounds at the moment a qualifying wound is
      --   removed. Descriptive only for now.
    , uiImmuneResponse ∷ !Float
      -- ^ Systemic immune-response level 0..1 (the body's active fight
      --   against whatever infections it currently has). Ramps up while any
      --   wound is infected (accelerating, scaled by constitution), clears
      --   infection (the second ticker racing infection growth), and decays
      --   back toward 0 once nothing is infected. Antibiotics nudge it up.
    , uiImmunities   ∷ !(HM.HashMap Text Float)
      -- ^ Acquired immunity per infection-type id ("staph" → 0..1). Accrues
      --   while fighting an infection (∝ response × severity, so a bad/long
      --   infection leaves more immunity), persists and decays VERY slowly.
      --   Reduces the foothold + severity of future infections of that type
      --   (scaled by constitution). Shown in the Status tab via immunity.png.
    , uiBlood       ∷ !Float
      -- ^ Current blood volume in litres. Spawn-time seeded to
      --   body_mass × 0.075 (≈7.5 % real-world ratio). Drained by
      --   bleeding wounds in the wound tick. Below 30 % of max
      --   → Collapsed pose; ≤ 0 → Dead pose + exsanguination
      --   death event. Max is recomputed from body_mass each read
      --   so wasting/regrowth carries through naturally.
    , uiLastAttackerUid ∷ !(Maybe Word32)
      -- ^ Runtime-only (NOT in SaveData). Written by Combat.Resolution
      --   on each hit landed against this unit. Read by the AI's
      --   `retaliate` candidate so wildlife / acolytes can react to
      --   being hit by setting an attack goal. Cleared lazily by the
      --   AI when the attacker dies, despawns, or `uiLastAttackerAt`
      --   ages past the retaliate-window threshold.
    , uiLastAttackerAt  ∷ !Double
      -- ^ Runtime-only. gameTime of the last incoming hit. Paired
      --   with `uiLastAttackerUid` for AI retaliation window checks.
    , uiAnimOverride    ∷ !Text
      -- ^ Runtime-only Lua-driven animation override. When non-empty,
      --   `publishToRender` uses this as the unit's current animation
      --   instead of resolving from `udStateAnims` via the sim
      --   (pose, activity) state key. Lets the AI play combat /
      --   posture animations that don't map to a sim activity (attack
      --   swings, combat-idle stances, bear sit/lie/sleep) without
      --   getting clobbered every sim tick. Written by
      --   `unit.setAnimOverride`; cleared by `unit.clearAnimOverride`.
    , uiFrozen      ∷ !Bool
      -- ^ Debug-only freeze flag. When True, `publishToRender` skips
      --   updating uiGridX/Y/Z, uiFacing, uiCurrentAnim, uiAnimStart,
      --   uiAnimReverse, uiAnimStride, uiActivity, uiPose from the
      --   sim state — Lua scripts hold full control of these fields
      --   via setAnim / setFacing / setPos. Used by the debug anim
      --   panel so previewed animations aren't stomped by the
      --   running AI / sim loop. Runtime state only, NOT in SaveData.
    , uiForceLoop   ∷ !Bool
      -- ^ Debug-only force-loop flag. When True, the renderer's
      --   `pickFrame` treats every animation as if its `aLoop` were
      --   True — non-looping anims (attacks, transitions, death) cycle
      --   continuously instead of holding their last frame. Used by
      --   the debug anim panel so previewed one-shot animations loop
      --   within a direction window. Runtime state only, NOT in
      --   SaveData.
    , uiClimbDest   ∷ !(Maybe (Int, Int))
      -- ^ While the unit is mid-climb, the (gx,gy) of the cliff column
      --   it's climbing onto (mirrored from sim `usClimbToTile`);
      --   Nothing otherwise. Lets the renderer occlude a unit climbing
      --   the FAR face behind that column (so it doesn't draw over the
      --   cliff), emerging as the pullup carries it onto the top tile.
      --   Runtime state only, NOT in SaveData.
    } deriving (Show, Eq)

-- | Holds all unit definitions and all spawned instances.
--   Lives behind an IORef in EngineEnv.
--
--   Selection state lives here so it's a single source of truth: the
--   renderer and the info-panel both read from the same struct, and
--   destroy-time cleanup is one atomic modify (delete from both maps).
data UnitManager = UnitManager
    { umDefs      ∷ !(HM.HashMap Text UnitDef)
    , umInstances ∷ !(HM.HashMap UnitId UnitInstance)
    , umSelected  ∷ !(HS.HashSet UnitId)
    , umNextId    ∷ !Word32
    } deriving (Show, Eq)

emptyUnitManager ∷ UnitManager
emptyUnitManager = UnitManager
    { umDefs      = HM.empty
    , umInstances = HM.empty
    , umSelected  = HS.empty
    , umNextId    = 1
    }

nextUnitId ∷ UnitManager → (UnitId, UnitManager)
nextUnitId um =
    let uid = UnitId (umNextId um)
    in (uid, um { umNextId = umNextId um + 1 })

-- | Instances belonging to any of the given world pages — the
--   world-scoping filter for render (the visible set) and queries.
unitsOnPages ∷ HS.HashSet WorldPageId
             → HM.HashMap UnitId UnitInstance
             → HM.HashMap UnitId UnitInstance
unitsOnPages pages = HM.filter (\inst → HS.member (uiPage inst) pages)

-- | Instances belonging to one specific world page (the active world).
unitsOnPage ∷ WorldPageId
            → HM.HashMap UnitId UnitInstance
            → HM.HashMap UnitId UnitInstance
unitsOnPage pid = HM.filter (\inst → uiPage inst ≡ pid)
