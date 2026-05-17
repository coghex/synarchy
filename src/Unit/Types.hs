{-# LANGUAGE Strict, UnicodeSyntax #-}
module Unit.Types
    ( Animation(..)
    , StatModifier(..)
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
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import qualified Data.Map.Strict as Map
import qualified Data.Vector as V
import Engine.Asset.Handle (TextureHandle(..))
import Item.Types (ItemInstance(..))
import Unit.Direction (Direction(..))

-- | A single animation: per-direction frame sequences.
--   Missing directions fall back to T-pose at render time.
data Animation = Animation
    { aFps    ∷ !Float
    , aLoop   ∷ !Bool
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
      -- ^ POSIX seconds when the modifier becomes inactive.
      --   Nothing = permanent (removed only via removeModifier).
    } deriving (Show, Eq)

-- | Unique identifier for a spawned unit instance.
newtype UnitId = UnitId { unUnitId ∷ Word32 }
    deriving (Show, Eq, Ord, Generic, Hashable)

-- | A unit definition (loaded from YAML, immutable after init).
--   This is the "template" — one per unit type.
data UnitDef = UnitDef
    { udName       ∷ !Text                            -- ^ e.g. "acolyte"
    , udTexture    ∷ !TextureHandle                   -- ^ default sprite handle
    , udDirSprites ∷ !(Map.Map Direction TextureHandle)
      -- ^ directional sprite overrides (may be empty)
    , udBaseWidth  ∷ !Float                           -- ^ ground contact diameter
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
