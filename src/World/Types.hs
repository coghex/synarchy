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

-- | Chunk coordinate in chunk-space.
--   Chunk (0,0) is the center of the world.
data ChunkCoord = ChunkCoord !Int !Int
    deriving (Show, Eq, Ord)

instance Hashable ChunkCoord where
    hashWithSalt s (ChunkCoord x y) = s `hashWithSalt` x `hashWithSalt` y

-- | A chunk's tile data: local coords (lx, ly, z) → Tile.
--   lx, ly ∈ [0, chunkSize-1]. z is unbounded.
type Chunk = HM.HashMap (Int, Int, Int) Tile

-- | A loaded chunk with its coordinate for identification.
data LoadedChunk = LoadedChunk
    { lcCoord    :: !ChunkCoord
    , lcTiles    :: !Chunk
    , lcModified :: !Bool        -- ^ Has the player modified this chunk?
    } deriving (Show, Eq)

-----------------------------------------------------------
-- World Tile Data
-----------------------------------------------------------

data Tile = Tile
    { tileType :: Word8   -- ^ Tile type (grass, stone, etc.)
    } deriving (Show, Eq)

-- | Chunk-based world tile storage.
--   Chunks are stored in MRU order (most recently used at head).
data WorldTileData = WorldTileData
    { wtdChunks :: ![LoadedChunk]  -- ^ Loaded chunks, MRU at head
    } deriving (Show, Eq)

emptyWorldTileData :: WorldTileData
emptyWorldTileData = WorldTileData
    { wtdChunks = []
    }

-- | Look up a chunk by coordinate.
lookupChunk :: ChunkCoord -> WorldTileData -> Maybe LoadedChunk
lookupChunk coord wtd =
    find (\lc -> lcCoord lc == coord) (wtdChunks wtd)

-- | Insert or replace a chunk, placing it at the head (MRU).
insertChunk :: LoadedChunk -> WorldTileData -> WorldTileData
insertChunk lc wtd =
    let others = filter (\c -> lcCoord c /= lcCoord lc) (wtdChunks wtd)
    in wtd { wtdChunks = lc : others }

-- | Promote a chunk to the head of the list (mark as recently used).
promoteChunk :: ChunkCoord -> WorldTileData -> WorldTileData
promoteChunk coord wtd =
    case partition (\c -> lcCoord c == coord) (wtdChunks wtd) of
        ([found], rest) -> wtd { wtdChunks = found : rest }
        _               -> wtd  -- not found, no-op

-- | Number of loaded chunks.
chunkCount :: WorldTileData -> Int
chunkCount = length . wtdChunks

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
    } deriving (Show, Eq)

defaultWorldTextures :: WorldTextures
defaultWorldTextures = WorldTextures
    { wtGrassTexture = TextureHandle 0
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
      -- ^ Initialize: pageId, seed, worldSizeInChunks
    | WorldShow WorldPageId
    | WorldHide WorldPageId
    | WorldTick Double
    | WorldSetTexture WorldPageId WorldTextureType TextureHandle
    | WorldSetCamera WorldPageId Float Float
    deriving (Show, Eq)
