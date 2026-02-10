{-# LANGUAGE Strict #-}
module World.Types where

import UPrelude
import qualified Data.HashMap.Strict as HM
import Data.IORef (IORef, newIORef)
import Engine.Asset.Handle (TextureHandle(..))  -- Add (..) to import constructor
import qualified Engine.Core.Queue as Q

-----------------------------------------------------------
-- World Page ID
-----------------------------------------------------------

newtype WorldPageId = WorldPageId Text
    deriving (Show, Eq, Ord)

-----------------------------------------------------------
-- World Tile Data
-----------------------------------------------------------

data Tile = Tile
    { tileType :: Word8   -- ^ Tile type (grass, stone, etc.)
    , tileElev :: Word8   -- ^ Height/elevation
    } deriving (Show, Eq)

data WorldTileData = WorldTileData
    { wtdTiles :: HM.HashMap (Int, Int) Tile  -- ^ Map of (x,y) -> Tile
    } deriving (Show, Eq)

emptyWorldTileData :: WorldTileData
emptyWorldTileData = WorldTileData
    { wtdTiles = HM.empty
    }

-----------------------------------------------------------
-- World Camera
-----------------------------------------------------------

data WorldCamera = WorldCamera
    { wcX :: Float  -- ^ Camera X position
    , wcY :: Float  -- ^ Camera Y position
    } deriving (Show, Eq)

-----------------------------------------------------------
-- World Textures
-----------------------------------------------------------

data WorldTextures = WorldTextures
    { wtGrassTexture :: TextureHandle
    -- Future: add more terrain textures
    } deriving (Show, Eq)

defaultWorldTextures :: WorldTextures
defaultWorldTextures = WorldTextures
    { wtGrassTexture = TextureHandle 0  -- Default white texture
    }

-----------------------------------------------------------
-- World State
-----------------------------------------------------------

data WorldState = WorldState
    { wsTilesRef    :: IORef WorldTileData
    , wsCameraRef   :: IORef WorldCamera
    , wsTexturesRef :: IORef WorldTextures
    }

emptyWorldState :: IO WorldState
emptyWorldState = do
    tilesRef <- newIORef emptyWorldTileData
    cameraRef <- newIORef (WorldCamera 0 0)
    texturesRef <- newIORef defaultWorldTextures
    return $ WorldState tilesRef cameraRef texturesRef

-----------------------------------------------------------
-- World Manager
-----------------------------------------------------------

data WorldManager = WorldManager
    { wmWorlds  :: [(WorldPageId, WorldState)]  -- ^ All loaded worlds
    , wmVisible :: [WorldPageId]                -- ^ Currently visible worlds
    }

emptyWorldManager :: WorldManager
emptyWorldManager = WorldManager
    { wmWorlds  = []
    , wmVisible = []
    }

-----------------------------------------------------------
-- World Commands
-----------------------------------------------------------

-- | Texture types for world tiles
data WorldTextureType
    = GrassTexture
    -- Future: add more terrain types
    deriving (Show, Eq)

data WorldCommand
    = WorldInit WorldPageId      -- ^ Initialize a world page
    | WorldShow WorldPageId      -- ^ Make world visible
    | WorldHide WorldPageId      -- ^ Hide world
    | WorldTick Double           -- ^ Simulate world (dt)
    | WorldSetTexture WorldPageId WorldTextureType TextureHandle
    deriving (Show, Eq)
