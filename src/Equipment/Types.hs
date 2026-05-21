{-# LANGUAGE Strict, UnicodeSyntax, DeriveGeneric, DeriveAnyClass #-}
module Equipment.Types
    ( EquipmentSlot(..)
    , EquipmentClass(..)
    , EquipmentClassManager(..)
    , emptyEquipmentClassManager
    , lookupEquipmentClass
    ) where

import UPrelude
import qualified Data.HashMap.Strict as HM
import Engine.Asset.Handle (TextureHandle(..))

-- | A single equippable slot on an equipment class. Position + size
--   are pixel coordinates relative to the silhouette texture's top-
--   left corner; the renderer applies uiscale at draw time. esKind
--   is the item kind this slot accepts (e.g. "headwear", "weapon");
--   items whose idKind doesn't match are filtered out of the equip
--   popup and rejected by the equip API (Phase 2).
data EquipmentSlot = EquipmentSlot
    { esId   ∷ !Text     -- ^ stable id, e.g. "head", "left_hand"
    , esName ∷ !Text     -- ^ display name, e.g. "Headwear"
    , esKind ∷ !Text     -- ^ accepted item kind, e.g. "weapon"
    , esX    ∷ !Int      -- ^ x offset in silhouette pixels
    , esY    ∷ !Int      -- ^ y offset in silhouette pixels
    , esW    ∷ !Int      -- ^ slot box width  (typically 32)
    , esH    ∷ !Int      -- ^ slot box height (typically 32)
    } deriving (Show, Eq)

-- | A loadout template — defines which slots exist on units of this
--   class. Acolytes (and other humans) share "humanoid"; future
--   quadrupeds / mounts / mechs declare their own. The silhouette is
--   the background art that the slot boxes are positioned over.
data EquipmentClass = EquipmentClass
    { ecName          ∷ !Text          -- ^ unique key, e.g. "humanoid"
    , ecSilhouetteTex ∷ !TextureHandle -- ^ background art handle
    , ecSilhouetteW   ∷ !Int           -- ^ source-pixel width of silhouette
    , ecSilhouetteH   ∷ !Int           -- ^ source-pixel height of silhouette
    , ecSlots         ∷ ![EquipmentSlot]
    } deriving (Show, Eq)

-- | Engine-wide registry of all loaded equipment classes.
newtype EquipmentClassManager = EquipmentClassManager
    { ecmDefs ∷ HM.HashMap Text EquipmentClass
    } deriving (Show, Eq)

emptyEquipmentClassManager ∷ EquipmentClassManager
emptyEquipmentClassManager = EquipmentClassManager HM.empty

lookupEquipmentClass ∷ Text → EquipmentClassManager → Maybe EquipmentClass
lookupEquipmentClass n (EquipmentClassManager m) = HM.lookup n m
