{-# LANGUAGE Strict, UnicodeSyntax #-}
module Unit.Types
    ( UnitDef(..)
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
import qualified Data.Map.Strict as Map
import Engine.Asset.Handle (TextureHandle(..))
import Unit.Direction (Direction(..))

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
    } deriving (Show, Eq)

-- | Holds all unit definitions and all spawned instances.
--   Lives behind an IORef in EngineEnv.
data UnitManager = UnitManager
    { umDefs      ∷ !(HM.HashMap Text UnitDef)
    , umInstances ∷ !(HM.HashMap UnitId UnitInstance)
    , umNextId    ∷ !Word32
    } deriving (Show, Eq)

emptyUnitManager ∷ UnitManager
emptyUnitManager = UnitManager
    { umDefs      = HM.empty
    , umInstances = HM.empty
    , umNextId    = 1
    }

nextUnitId ∷ UnitManager → (UnitId, UnitManager)
nextUnitId um =
    let uid = UnitId (umNextId um)
    in (uid, um { umNextId = umNextId um + 1 })
