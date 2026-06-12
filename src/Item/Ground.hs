{-# LANGUAGE Strict, UnicodeSyntax, DeriveGeneric, DeriveAnyClass #-}
-- | Items lying in the world.
--
--   A ground item is a full 'ItemInstance' (so inventory → ground →
--   inventory round-trips preserve fill / quality / condition) at a
--   FLOAT world position. Deliberately no stored z: the render pass
--   derives the resting height from the CURRENT terrain surface (and
--   its slope) every frame, so digging the tile under an item drops
--   the item with it automatically — there is no way for a ground
--   item's height to go stale.
--
--   Stored on 'WorldState' (wsGroundItemsRef) and persisted in saves
--   (sdGroundItems, v32). Writers use atomicModifyIORef' — debug
--   spawn (Lua thread), future drop/pickup (unit thread), and future
--   dig yields (world thread) all mutate the same map safely.
module Item.Ground
    ( GroundItem(..)
    , GroundItems(..)
    , emptyGroundItems
    , spawnGroundItem
    , removeGroundItem
    ) where

import UPrelude hiding (get)
import GHC.Generics (Generic)
import Data.Serialize (Serialize)
import qualified Data.HashMap.Strict as HM
import Item.Types (ItemInstance(..))

data GroundItem = GroundItem
    { giInst ∷ !ItemInstance
    , giX    ∷ !Float          -- ^ world tile-space x (float, sub-tile)
    , giY    ∷ !Float          -- ^ world tile-space y
    } deriving (Show, Eq, Generic, Serialize)

data GroundItems = GroundItems
    { gisNextId ∷ !Int
    , gisItems  ∷ !(HM.HashMap Int GroundItem)
    } deriving (Show, Eq, Generic, Serialize)

emptyGroundItems ∷ GroundItems
emptyGroundItems = GroundItems 0 HM.empty

-- | Insert; returns the assigned id.
spawnGroundItem ∷ ItemInstance → Float → Float → GroundItems
                → (GroundItems, Int)
spawnGroundItem inst x y gis =
    let gid = gisNextId gis
        gi  = GroundItem inst x y
    in ( gis { gisNextId = gid + 1
             , gisItems  = HM.insert gid gi (gisItems gis) }
       , gid )

-- | Remove by id; returns the removed item (for pickup flows).
removeGroundItem ∷ Int → GroundItems → (GroundItems, Maybe GroundItem)
removeGroundItem gid gis =
    case HM.lookup gid (gisItems gis) of
        Nothing → (gis, Nothing)
        Just gi → (gis { gisItems = HM.delete gid (gisItems gis) }, Just gi)
