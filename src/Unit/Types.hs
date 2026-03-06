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
import Engine.Asset.Handle (TextureHandle(..))

-- | Unique identifier for a spawned unit instance.
newtype UnitId = UnitId { unUnitId ∷ Word32 }
    deriving (Show, Eq, Ord, Generic, Hashable)

-- | A unit definition (loaded from YAML, immutable after init).
--   This is the "template" — one per unit type.
data UnitDef = UnitDef
    { udName      ∷ !Text           -- ^ e.g. "acolyte"
    , udTexture   ∷ !TextureHandle  -- ^ default sprite handle
    , udBaseWidth ∷ !Float          -- ^ ground contact diameter in pixels
    } deriving (Show, Eq)

-- | A spawned unit instance in the world.
--   Engine is agnostic to player vs NPC — Lua drives behavior.
data UnitInstance = UnitInstance
    { uiDefName    ∷ !Text           -- ^ which UnitDef this came from
    , uiTexture    ∷ !TextureHandle  -- ^ current display texture (may differ from def for animations later)
    , uiBaseWidth  ∷ !Float          -- ^ from the def, cached here for rendering
    , uiGridX      ∷ !Float          -- ^ world grid X (fractional for sub-tile positioning)
    , uiGridY      ∷ !Float          -- ^ world grid Y (fractional)
    , uiGridZ      ∷ !Int            -- ^ world grid Z (integer elevation)
    } deriving (Show, Eq)

-- | Holds all unit definitions and all spawned instances.
--   Lives behind an IORef in EngineEnv.
data UnitManager = UnitManager
    { umDefs      ∷ !(HM.HashMap Text UnitDef)        -- ^ name → definition
    , umInstances ∷ !(HM.HashMap UnitId UnitInstance)  -- ^ id → live instance
    , umNextId    ∷ !Word32                             -- ^ monotonic counter
    } deriving (Show, Eq)

emptyUnitManager ∷ UnitManager
emptyUnitManager = UnitManager
    { umDefs      = HM.empty
    , umInstances = HM.empty
    , umNextId    = 1
    }

-- | Allocate a fresh UnitId and return the updated manager.
nextUnitId ∷ UnitManager → (UnitId, UnitManager)
nextUnitId um =
    let uid = UnitId (umNextId um)
    in (uid, um { umNextId = umNextId um + 1 })
