{-# LANGUAGE Strict, UnicodeSyntax, DeriveGeneric, DeriveAnyClass #-}
module Unit.Types
    ( Animation(..)
    , StatModifier(..)
    , Wound(..)
    , BodyPart(..)
    , NaturalWeapon(..)
    , NaturalResistance(..)
    , defaultNaturalResistance
    , UnitDef(..)
    , UnitInstance(..)
    , UnitManager(..)
    , UnitId(..)
    , emptyUnitManager
    , nextUnitId
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

-- | One additive modifier on a stat. Multiple modifiers on the same
--   stat sum together (after expiry filtering). The (source, stat)
--   pair is unique within a unit — re-adding the same source on the
--   same stat overwrites the previous entry.
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
--     max_health_factor — part max_hp = body_mass × this factor.
--                         A wound of severity 1.0 ≈ a fatal blow to
--                         the part (head crushed, neck severed, ...).
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
    , bpParent          ∷ !(Maybe Text)
      -- ^ Attached part id (so severing an arm later cascades to
      --   the hand). For 2.1 the parent chain is read but not yet
      --   acted on; severity is per-part.
    , bpVital           ∷ !Bool
    , bpAreaWeight      ∷ !Float
    , bpTacticalValue   ∷ !Float
    , bpMaxHealthFactor ∷ !Float
    , bpBleedFactor     ∷ !Float
    , bpHeightLow       ∷ !Float    -- ^ metres above foot
    , bpHeightHigh      ∷ !Float    -- ^ metres above foot
    } deriving (Show, Eq, Generic)

-- | A creature's innate (non-equipment) weapon — claws, fangs, fists.
--   Used by units with no equipped weapon, falling back to this
--   definition for attack range, cooldown, and weapon-class skill
--   selection. Acolytes don't declare one (they fight equipped);
--   bears declare an unarmed natural weapon.
data NaturalWeapon = NaturalWeapon
    { nwWeaponClass            ∷ !Text   -- ^ matches a skill name on the unit
    , nwEffectiveBladeLength   ∷ !Float  -- ^ cm; treated as a weapon blade for range/damage
    , nwAttackCooldown         ∷ !Float  -- ^ seconds between swings
    , nwStabEff                ∷ !Float  -- ^ 0..1 — same shape as ItemWeapon.iwStabEff
    , nwSlashEff               ∷ !Float
    , nwBluntEff               ∷ !Float
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

-- | A unit definition (loaded from YAML, immutable after init).
--   This is the "template" — one per unit type.
data UnitDef = UnitDef
    { udName       ∷ !Text                            -- ^ e.g. "acolyte"
    , udTexture    ∷ !TextureHandle                   -- ^ default sprite handle
    , udDirSprites ∷ !(Map.Map Direction TextureHandle)
      -- ^ directional sprite overrides (may be empty)
    , udBaseWidth  ∷ !Float                           -- ^ ground contact diameter
    , udMaxSpeed   ∷ !Float
      -- ^ Per-species maximum movement speed in tiles/sec at a full
      --   sprint. AI candidates compute commanded / wander / retreat
      --   speeds as fractions of this. Sim picks Running over Walking
      --   when current move speed crosses 0.6 × udMaxSpeed.
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
    , udStartingInventory ∷ ![(Text, Maybe Float)]
      -- ^ ItemDef name + optional initial fill. Materialised into
      --   `uiInventory` at spawn by looking each entry up in the
      --   ItemManager; unknown names are dropped with a log warning.
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
    } deriving (Show, Eq)

-- | A spawned unit instance in the world.
--   Engine is agnostic to player vs NPC — Lua drives behavior.
data UnitInstance = UnitInstance
    { uiDefName    ∷ !Text           -- ^ which UnitDef this came from
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
      --   Wounds below severity 0.01 are auto-removed; vital wounds
      --   ≥1.0 trigger instant death (set in Combat.Resolution).
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
