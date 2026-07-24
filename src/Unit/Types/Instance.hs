{-# LANGUAGE Strict, UnicodeSyntax, DeriveGeneric, DeriveAnyClass #-}
-- | A spawned unit instance (`UnitInstance`), split out of "Unit.Types"
--   (#575) — re-exported there so the public API is unchanged.
module Unit.Types.Instance
    ( UnitInstance(..)
    ) where

import UPrelude
import qualified Data.HashMap.Strict as HM
import qualified Data.Map.Strict as Map
import Engine.Asset.Handle (TextureHandle(..))
import Item.Types (ItemInstance(..))
import Unit.Direction (Direction(..))
import World.Page.Types (WorldPageId(..))
import Unit.Types.Def (StatModifier(..))
import Unit.Types.Wound (Wound(..), Scar(..))
import Unit.Types.Trail (TrailState(..))

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
      --   body_mass × 'bloodMassRatio'. Drained by
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
    , uiTrailState  ∷ !(Maybe TrailState)
      -- ^ Bleeding-trail emitter state (issue #882, the moving half of
      --   ongoing bleeding — see "Blood.Trail"). Nothing = no active
      --   trail: the unit has never bled externally, or its trail was
      --   cleared (death, `unit.destroy`, or effective external bleed
      --   reaching zero). Written by `Combat.Wounds.Tick` (accumulates
      --   external loss) and consumed by `Unit.Thread.Movement` (pops
      --   marks as the unit moves). Runtime state only, NOT in SaveData
      --   — resets to Nothing on load like `uiClimbDest`.
    } deriving (Show, Eq)
