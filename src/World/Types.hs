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
    , wgpPlateCount :: !Int     -- ^ Number of tectonic plates (for worldgen)
    } deriving (Show, Eq)

defaultWorldGenParams :: WorldGenParams
defaultWorldGenParams = WorldGenParams
    { wgpSeed      = 42
    , wgpWorldSize = 64
    , wgpPlateCount = 10
    }

-----------------------------------------------------------
-- World Tile Data
-----------------------------------------------------------

data Tile = Tile
    { tileType :: Word8
    , tileSlopeId :: Word8
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
    { wtGraniteTexture  :: TextureHandle
    , wtGabbroTexture   :: TextureHandle
    , wtDioriteTexture  :: TextureHandle
    , wtIsoFaceMap      :: TextureHandle
    , wtNoTexture       :: TextureHandle
    , wtNoFaceMap       :: TextureHandle
    } deriving (Show, Eq)

defaultWorldTextures :: WorldTextures
defaultWorldTextures = WorldTextures
    { wtGraniteTexture  = TextureHandle 0
    , wtGabbroTexture   = TextureHandle 0
    , wtDioriteTexture  = TextureHandle 0
    , wtNoTexture       = TextureHandle 0
    , wtIsoFaceMap      = TextureHandle 0
    , wtNoFaceMap       = TextureHandle 0
    }

-----------------------------------------------------------
-- World Time
-----------------------------------------------------------

-- | Time of day in the world.
--   hour: 0-23, minute: 0-59
--   sunAngle is derived: 0.0 = midnight, 0.25 = 6am (dawn),
--                         0.5 = noon, 0.75 = 6pm (dusk)
data WorldTime = WorldTime
    { wtHour   :: !Int   -- ^ 0-23
    , wtMinute :: !Int   -- ^ 0-59
    } deriving (Show, Eq)

defaultWorldTime :: WorldTime
defaultWorldTime = WorldTime
    { wtHour   = 10     -- start at 10:00am (pleasant morning light)
    , wtMinute = 0
    }

-- | Convert world time to sun angle (0.0 .. 1.0)
--   Mapping: midnight (0:00) = 0.0, 6am = 0.25, noon = 0.5, 6pm = 0.75
worldTimeToSunAngle :: WorldTime -> Float
worldTimeToSunAngle (WorldTime h m) =
    let totalMinutes = fromIntegral h * 60.0 + fromIntegral m :: Float
    in totalMinutes / 1440.0   -- 1440 = 24 * 60

-- | Advance world time by a number of real seconds, scaled by a speed factor.
--   Returns the new time (wraps at 24:00).
--   timeScale: how many game-minutes pass per real-second.
advanceWorldTime :: Float -> Float -> WorldTime -> WorldTime
advanceWorldTime timeScale dtSeconds (WorldTime h m) =
    let totalMinutes = fromIntegral h * 60 + fromIntegral m :: Float
        newTotal = totalMinutes + timeScale * dtSeconds
        -- Wrap around 1440 minutes (24 hours)
        wrapped = newTotal - 1440.0 * fromIntegral (floor (newTotal / 1440.0) :: Int)
        newH = floor wrapped `div` 60
        newM = floor wrapped `mod` 60
    in WorldTime (newH `mod` 24) (newM `mod` 60)

-- | World date (placeholder for seasons).
--   Currently unused for sun angle calculation.
data WorldDate = WorldDate
    { wdYear  :: !Int
    , wdMonth :: !Int   -- ^ 1-12
    , wdDay   :: !Int   -- ^ 1-31
    } deriving (Show, Eq)

defaultWorldDate :: WorldDate
defaultWorldDate = WorldDate
    { wdYear  = 1
    , wdMonth = 1
    , wdDay   = 1
    }

-----------------------------------------------------------
-- World State
-----------------------------------------------------------

data WorldState = WorldState
    { wsTilesRef     :: IORef WorldTileData
    , wsCameraRef    :: IORef WorldCamera
    , wsTexturesRef  :: IORef WorldTextures
    , wsGenParamsRef :: IORef (Maybe WorldGenParams)
    , wsTimeRef      :: IORef WorldTime
    , wsDateRef      :: IORef WorldDate
    , wsTimeScaleRef :: IORef Float    -- ^ Game-minutes per real-second
    }

emptyWorldState :: IO WorldState
emptyWorldState = do
    tilesRef     <- newIORef emptyWorldTileData
    cameraRef    <- newIORef (WorldCamera 0 0)
    texturesRef  <- newIORef defaultWorldTextures
    genParamsRef <- newIORef Nothing
    timeRef      <- newIORef defaultWorldTime
    dateRef      <- newIORef defaultWorldDate
    timeScaleRef <- newIORef 1.0   -- 1 game-minute per real-second
    return $ WorldState tilesRef cameraRef texturesRef genParamsRef
                        timeRef dateRef timeScaleRef

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
    = GraniteTexture
    | DioriteTexture
    | GabbroTexture
    | NoTexture
    | IsoFaceMap
    | NoFaceMap
    deriving (Show, Eq)

data WorldCommand
    = WorldInit WorldPageId Word64 Int
    | WorldShow WorldPageId
    | WorldHide WorldPageId
    | WorldTick Double
    | WorldSetTexture WorldPageId WorldTextureType TextureHandle
    | WorldSetCamera WorldPageId Float Float
    | WorldSetTime WorldPageId Int Int         -- ^ hour minute
    | WorldSetDate WorldPageId Int Int Int     -- ^ year month day
    | WorldSetTimeScale WorldPageId Float      -- ^ game-minutes per real-second
    deriving (Show, Eq)
