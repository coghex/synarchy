{-# LANGUAGE Strict #-}
-- | The ONE page-owned finite-arena bounds authority (#2708, SCN-08 of
--   epic #2698).
--
--   An arena page is either EXPANDABLE (no bounds: the existing
--   behaviour, chunks on demand without limit) or FINITE: exact
--   @width × height@ horizontal tiles centred on zero. The rectangle is
--   the one #2699 defines ('Scenario.Bounds.mapTileBounds'):
--
--   > minX = -floor(width / 2);  maxX = minX + width - 1
--   > maxY =  floor(height / 2); minY = maxY - height + 1
--
--   so 20×10 is X −10…9, Y −4…5, 21×11 is X −10…10, Y −5…5 and 1×1 is
--   only (0, 0). The numbers are ENGINE TILE coordinates: engine
--   @(gx, gy)@ is the rectangle's @(X, Y)@. Whether a presentation flips
--   Y is the later scenario adapters' business, not this module's; the
--   camera never translates the bounds, and nothing here clamps it.
--
--   __Page-owned, never inferred.__ A page's bounds are the 'Maybe'
--   'ArenaBounds' recorded in its generation parameters
--   ('World.Generate.Types.wgpArenaBounds') and persisted with the page.
--   They are NOT derived from 'World.Generate.Types.isArenaParams' or
--   from the arena's @wgpWorldSize@ sentinel: a finite arena remains an
--   arena for every consumer of that predicate (flat topology, identity
--   canonicalisation, the staged-restore arena branch), and the bounds
--   are an additional filter, not a replacement topology.
--
--   __Validated once.__ 'mkArenaBounds' is the only way to obtain a
--   value. It applies the schema's dimension domain (each dimension an
--   integer in @[1, 'maxScenarioDimension']@) and the checked arithmetic
--   of 'Scenario.Bounds.mapTileBounds', so a native caller, the save
--   decoder and a scenario adapter all go through the same refusal and a
--   caller cannot bypass it with a hand-built record.
--
--   __Partial chunks.__ A chunk that intersects the rectangle without
--   being inside it is admitted with only its in-bounds columns present
--   ('chunkColumnMask'); the rest are ABSENT columns
--   ('World.Generate.Arena.generateArenaChunk' builds them), and a chunk
--   wholly outside is never generated or admitted ('boundedChunkCoords').
module World.Arena.Bounds
    ( -- * The validated value
      ArenaBounds
    , arenaWidth
    , arenaHeight
    , arenaRect
    , mkArenaBounds
    , ArenaBoundsError(..)
    , arenaBoundsErrorText
    , maxArenaDimension
      -- * Tiles and positions
    , arenaAdmitsTile
    , arenaAdmitsPosition
    , positionTile
    , clampPositionIntoArena
      -- * Chunks
    , chunkIntersectsArena
    , chunkInsideArena
    , chunkColumnMask
    , boundedChunkCoords
    , arenaChunkRange
      -- * Clipping and footprint validation (for scenario adapters)
    , TileClip(..)
    , clipTiles
    , clipTileRegion
    , FootprintVerdict(..)
    , footprintVerdict
    , footprintWithin
    ) where

import UPrelude
import Control.DeepSeq (NFData(..))
import qualified Data.Vector.Unboxed as VU
import GHC.Float (castFloatToWord32, castWord32ToFloat)
import Scenario.Bounds
    ( TileBounds(..), mapTileBounds, maxScenarioDimension, inBoundsI
    , clipRegion, footprintInside )
import Scenario.Types (MapDimensions(..), TileRegion(..), Footprint(..))
import World.Chunk.Types (ChunkCoord(..), chunkSize)

-- | A validated finite-arena size, with its tile rectangle. The
--   constructor is private: 'mkArenaBounds' is the only way in.
data ArenaBounds = ArenaBounds
    { abWidth  ∷ !Int
    , abHeight ∷ !Int
    , abRect   ∷ !TileBounds
    } deriving (Show, Eq)

-- | All fields are strict machine integers, so weak-head is normal form.
instance NFData ArenaBounds where
    rnf ab = ab `seq` ()

-- | Why a requested size is not a finite arena.
data ArenaBoundsError
    = DimensionOutOfDomain !Int !Int
      -- ^ width, height: one of them is not an integer in
      --   @[1, 'maxArenaDimension']@, or the rectangle would not fit
      --   'Int'.
    deriving (Show, Eq)

-- | The schema's dimension ceiling (@Scenario.Bounds.maxScenarioDimension@).
maxArenaDimension ∷ Int
maxArenaDimension = maxScenarioDimension

arenaBoundsErrorText ∷ ArenaBoundsError → Text
arenaBoundsErrorText (DimensionOutOfDomain w h) =
    "finite arena size " <> tshow w <> "x" <> tshow h
    <> " is outside the dimension domain: width and height must each be "
    <> "an integer from 1 to " <> tshow maxArenaDimension

-- | Validate a requested size. This is the ONLY constructor: the Lua
--   binding, the save decoder, the test fixtures and the scenario
--   adapters all call it.
mkArenaBounds ∷ Int → Int → Either ArenaBoundsError ArenaBounds
mkArenaBounds w h = case mapTileBounds (MapDimensions w h) of
    Nothing   → Left (DimensionOutOfDomain w h)
    Just rect → Right (ArenaBounds w h rect)

arenaWidth ∷ ArenaBounds → Int
arenaWidth = abWidth

arenaHeight ∷ ArenaBounds → Int
arenaHeight = abHeight

-- | The inclusive tile rectangle. Total: a value only exists once its
--   rectangle was computed with checked arithmetic.
arenaRect ∷ ArenaBounds → TileBounds
arenaRect = abRect

-- * Tiles and positions

-- | May terrain exist at this engine tile? 'Nothing' (an expandable
--   arena, or a page that is not an arena at all) admits every tile.
arenaAdmitsTile ∷ Maybe ArenaBounds → Int → Int → Bool
arenaAdmitsTile Nothing   _  _  = True
arenaAdmitsTile (Just ab) gx gy =
    inBoundsI (abRect ab) (toInteger gx, toInteger gy)

-- | The engine tile a continuous position stands on: @floor@ on both
--   axes, exactly the convention the movement stepper, the surface
--   lookups and the unit lifecycle already use (a unit's tile is the one
--   its coordinate FLOORS to). Unbounded, so a huge finite coordinate
--   stays huge rather than wrapping into the rectangle; a non-finite
--   one has no tile.
positionTile ∷ Float → Float → Maybe (Integer, Integer)
positionTile x y
    | isNaN x ∨ isInfinite x ∨ isNaN y ∨ isInfinite y = Nothing
    | otherwise = Just (floor x, floor y)

-- | May an actor or object stand at this continuous position? A
--   non-finite position is never inside a finite arena.
arenaAdmitsPosition ∷ Maybe ArenaBounds → Float → Float → Bool
arenaAdmitsPosition Nothing   _ _ = True
arenaAdmitsPosition (Just ab) x y = case positionTile x y of
    Nothing       → False
    Just (tx, ty) → inBoundsI (abRect ab) (tx, ty)

-- | The nearest point INSIDE the rectangle to a continuous position: a
--   coordinate is limited to @[minX, maxX + 1)@ (the half-open span of
--   tiles the rectangle's tiles floor from), the upper limit being the
--   largest 'Float' strictly below @maxX + 1@ so the result floors to a
--   tile that is in bounds at any magnitude. A non-finite coordinate has
--   no nearest point and maps to the tile centre of the rectangle's
--   origin side — callers hand it the last good position instead.
clampPositionIntoArena ∷ ArenaBounds → Float → Float → (Float, Float)
clampPositionIntoArena ab x y =
    let TileBounds bx0 by0 bx1 by1 = abRect ab
    in (clampAxis bx0 bx1 x, clampAxis by0 by1 y)
  where
    clampAxis lo hi v
        | isNaN v = fromIntegral (max lo (min hi 0)) + 0.5
        | otherwise = max (fromIntegral lo) (min (belowOne (fromIntegral hi + 1)) v)
    -- The largest Float strictly below a positive one.
    belowOne ∷ Float → Float
    belowOne f = castWord32ToFloat (castFloatToWord32 f - 1)

-- * Chunks

-- | The inclusive tile rectangle a chunk covers, as integers.
chunkTiles ∷ ChunkCoord → (Integer, Integer, Integer, Integer)
chunkTiles (ChunkCoord cx cy) =
    ( toInteger cx * n, toInteger cy * n
    , toInteger cx * n + n - 1, toInteger cy * n + n - 1 )
  where n = toInteger chunkSize

-- | Does any tile of this chunk lie inside the rectangle? Only such
--   chunks are ever generated or admitted.
chunkIntersectsArena ∷ ArenaBounds → ChunkCoord → Bool
chunkIntersectsArena ab c =
    let TileBounds bx0 by0 bx1 by1 = abRect ab
        (cx0, cy0, cx1, cy1) = chunkTiles c
    in cx0 ≤ toInteger bx1 ∧ cx1 ≥ toInteger bx0
       ∧ cy0 ≤ toInteger by1 ∧ cy1 ≥ toInteger by0

-- | Is every tile of this chunk inside the rectangle (a chunk with no
--   absent columns)?
chunkInsideArena ∷ ArenaBounds → ChunkCoord → Bool
chunkInsideArena ab c =
    let TileBounds bx0 by0 bx1 by1 = abRect ab
        (cx0, cy0, cx1, cy1) = chunkTiles c
    in cx0 ≥ toInteger bx0 ∧ cx1 ≤ toInteger bx1
       ∧ cy0 ≥ toInteger by0 ∧ cy1 ≤ toInteger by1

-- | Which columns of a chunk are inside the rectangle, indexed like the
--   chunk (@ly * chunkSize + lx@).
chunkColumnMask ∷ ArenaBounds → ChunkCoord → VU.Vector Bool
chunkColumnMask ab c@(ChunkCoord cx cy)
    | chunkInsideArena ab c = VU.replicate (chunkSize * chunkSize) True
    | otherwise = VU.generate (chunkSize * chunkSize) $ \i →
        let (ly, lx) = i `divMod` chunkSize
        in inBoundsI (abRect ab)
               (toInteger cx * toInteger chunkSize + toInteger lx
               ,toInteger cy * toInteger chunkSize + toInteger ly)

-- | The coordinates that may be demanded on a page with these bounds,
--   order and duplicates preserved. 'Nothing' (expandable) keeps all.
boundedChunkCoords ∷ Maybe ArenaBounds → [ChunkCoord] → [ChunkCoord]
boundedChunkCoords Nothing   = id
boundedChunkCoords (Just ab) = filter (chunkIntersectsArena ab)

-- | The inclusive chunk-coordinate rectangle that intersects the
--   arena: @((minCx, minCy), (maxCx, maxCy))@.
arenaChunkRange ∷ ArenaBounds → ((Int, Int), (Int, Int))
arenaChunkRange ab =
    let TileBounds bx0 by0 bx1 by1 = abRect ab
        chunkOf v = v `div` chunkSize
    in ((chunkOf bx0, chunkOf by0), (chunkOf bx1, chunkOf by1))

-- * Clipping and footprint validation

-- | The outcome of clipping a tile set to the rectangle (D-21): the
--   tiles that stay, and the ones rejected — for the actionable
--   warnings the adapters report. Input order is preserved in both.
data TileClip = TileClip
    { tcKept     ∷ ![(Int, Int)]
    , tcRejected ∷ ![(Int, Int)]
    } deriving (Show, Eq)

clipTiles ∷ Maybe ArenaBounds → [(Int, Int)] → TileClip
clipTiles mb ts =
    let (kept, rejected) = partitionTiles (\(x, y) → arenaAdmitsTile mb x y) ts
    in TileClip kept rejected
  where
    partitionTiles p = foldr (\t (as, bs) →
        if p t then (t : as, bs) else (as, t : bs)) ([], [])

-- | 'Scenario.Bounds.clipRegion' under a page's bounds: the in-bounds
--   part of a patch region, and how many tiles were dropped. An
--   expandable arena keeps the whole region.
clipTileRegion ∷ Maybe ArenaBounds → TileRegion → (Maybe TileRegion, Integer)
clipTileRegion Nothing   r = (Just r, 0)
clipTileRegion (Just ab) r = clipRegion (abRect ab) r

-- | Whether an indivisible object's complete footprint fits (D-21).
data FootprintVerdict
    = FootprintFits
    | FootprintCrosses ![(Integer, Integer)]
      -- ^ The footprint's corners that lie outside, for the warning.
    deriving (Show, Eq)

-- | Judge a footprint anchored at @(x, y)@ with the offsets of a
--   'Footprint'. A building or location is rejected whole if any part
--   crosses the rectangle, BEFORE any terrain stamp, reservation or
--   owned content is applied.
footprintVerdict ∷ Maybe ArenaBounds → (Int, Int) → Footprint → FootprintVerdict
footprintVerdict Nothing   _        _  = FootprintFits
footprintVerdict (Just ab) (x, y) fp@(Footprint dx0 dy0 dx1 dy1)
    | footprintInside (abRect ab) (x, y) fp = FootprintFits
    | otherwise = FootprintCrosses
        [ c | c ← [ (ix + toInteger dx0, iy + toInteger dy0)
                  , (ix + toInteger dx1, iy + toInteger dy0)
                  , (ix + toInteger dx0, iy + toInteger dy1)
                  , (ix + toInteger dx1, iy + toInteger dy1) ]
            , not (inBoundsI (abRect ab) c) ]
  where
    ix = toInteger x
    iy = toInteger y

-- | 'footprintVerdict' as a plain predicate.
footprintWithin ∷ Maybe ArenaBounds → (Int, Int) → Footprint → Bool
footprintWithin mb a fp = footprintVerdict mb a fp ≡ FootprintFits
