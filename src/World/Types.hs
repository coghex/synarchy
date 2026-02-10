{-# LANGUAGE Strict #-}
module World.Types where

import UPrelude
import Data.List (find, partition)
import Data.Hashable (Hashable(..))
import qualified Data.HashMap.Strict as HM
import Data.IORef (IORef, newIORef)
import Engine.Asset.Handle (TextureHandle(..))
import qualified Engine.Core.Queue as Q

-----------------------------------------------------------
-- World Page ID
-----------------------------------------------------------

newtype WorldPageId = WorldPageId Text
    deriving (Show, Eq, Ord)

-----------------------------------------------------------
-- Chunk Types
-----------------------------------------------------------

data ChunkCoord = ChunkCoord !Int !Int
    deriving (Show, Eq, Ord)

instance Hashable ChunkCoord where
    hashWithSalt s (ChunkCoord x y) = s `hashWithSalt` x `hashWithSalt` y

type Chunk = HM.HashMap (Int, Int, Int) Tile

data LoadedChunk = LoadedChunk
    { lcCoord    :: !ChunkCoord
    , lcTiles    :: !Chunk
    , lcModified :: !Bool
    } deriving (Show, Eq)

-----------------------------------------------------------
-- World Generation Parameters
-----------------------------------------------------------

-- | Pure, serializable world generation parameters.
--   Same params + same ChunkCoord = same Chunk, always.
data WorldGenParams = WorldGenParams
    { wgpSeed       :: !Word64
    , wgpWorldSize  :: !Int     -- ^ World size in chunks (e.g. 64 → 64×64 chunks)
    } deriving (Show, Eq)

defaultWorldGenParams :: WorldGenParams
defaultWorldGenParams = WorldGenParams
    { wgpSeed      = 42
    , wgpWorldSize = 64
    }

-----------------------------------------------------------
-- World Tile Data
-----------------------------------------------------------

data Tile = Tile
    { tileType :: Word8
    } deriving (Show, Eq)

data WorldTileData = WorldTileData
    { wtdChunks :: ![LoadedChunk]
    } deriving (Show, Eq)

emptyWorldTileData :: WorldTileData
emptyWorldTileData = WorldTileData
    { wtdChunks = []
    }

lookupChunk :: ChunkCoord -> WorldTileData -> Maybe LoadedChunk
lookupChunk coord wtd =
    find (\lc -> lcCoord lc == coord) (wtdChunks wtd)

insertChunk :: LoadedChunk -> WorldTileData -> WorldTileData
insertChunk lc wtd =
    let others = filter (\c -> lcCoord c /= lcCoord lc) (wtdChunks wtd)
    in wtd { wtdChunks = lc : others }

promoteChunk :: ChunkCoord -> WorldTileData -> WorldTileData
promoteChunk coord wtd =
    case partition (\c -> lcCoord c == coord) (wtdChunks wtd) of
        ([found], rest) -> wtd { wtdChunks = found : rest }
        _               -> wtd

chunkCount :: WorldTileData -> Int
chunkCount = length . wtdChunks

-----------------------------------------------------------
-- World Camera
-----------------------------------------------------------

data WorldCamera = WorldCamera
    { wcX :: Float
    , wcY :: Float
    } deriving (Show, Eq)

-----------------------------------------------------------
-- World Textures
-----------------------------------------------------------

data WorldTextures = WorldTextures
    { wtGrassTexture :: TextureHandle
    } deriving (Show, Eq)

defaultWorldTextures :: WorldTextures
defaultWorldTextures = WorldTextures
    { wtGrassTexture = TextureHandle 0
    }

-----------------------------------------------------------
-- World State
-----------------------------------------------------------

data WorldState = WorldState
    { wsTilesRef     :: IORef WorldTileData
    , wsCameraRef    :: IORef WorldCamera
    , wsTexturesRef  :: IORef WorldTextures
    , wsGenParamsRef :: IORef (Maybe WorldGenParams)
    }

emptyWorldState :: IO WorldState
emptyWorldState = do
    tilesRef    <- newIORef emptyWorldTileData
    cameraRef   <- newIORef (WorldCamera 0 0)
    texturesRef <- newIORef defaultWorldTextures
    genParamsRef <- newIORef Nothing
    return $ WorldState tilesRef cameraRef texturesRef genParamsRef

-----------------------------------------------------------
-- World Manager
-----------------------------------------------------------

data WorldManager = WorldManager
    { wmWorlds  :: [(WorldPageId, WorldState)]
    , wmVisible :: [WorldPageId]
    }

emptyWorldManager :: WorldManager
emptyWorldManager = WorldManager
    { wmWorlds  = []
    , wmVisible = []
    }

-----------------------------------------------------------
-- World Commands
-----------------------------------------------------------

data WorldTextureType
    = GrassTexture
    deriving (Show, Eq)

data WorldCommand
    = WorldInit WorldPageId Word64 Int
    | WorldShow WorldPageId
    | WorldHide WorldPageId
    | WorldTick Double
    | WorldSetTexture WorldPageId WorldTextureType TextureHandle
    | WorldSetCamera WorldPageId Float Float
    deriving (Show, Eq)
