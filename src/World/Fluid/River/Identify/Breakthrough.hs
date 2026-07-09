{-# LANGUAGE Strict, UnicodeSyntax #-}

-- | Coastal breakthrough for the river-identification pipeline:
--   finds stranded inland river mouths and, for each, Dijkstra's a
--   carved path to the nearest ocean tile within cost/range budgets.
--   Called once from 'World.Fluid.River.Identify.traceRivers'. See
--   that module's header comment for the full pipeline overview.
module World.Fluid.River.Identify.Breakthrough
    ( addBreakthroughs
    ) where

import UPrelude
import qualified Data.IntMap.Strict as IM
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Unboxed.Mutable as VUM
import Control.Monad (foldM)
import Control.Monad.ST (runST)
import Data.STRef (newSTRef, readSTRef, writeSTRef)
import World.Constants (seaLevel)
import World.Fluid.River.Identify.Common (dirNone)

-- | Tile-radius within which an inland river-mouth will try to
--   breakthrough-carve a path to the nearest ocean. Inland rivers
--   farther from any coast stay endorheic.
breakthroughRange ∷ Int
breakthroughRange = 40

-- | Maximum cumulative carve cost (sum of uphill z plus 1 per step)
--   the breakthrough Dijkstra will accept. Beyond this the breakthrough
--   fails and the river pools at its inland terminus.
breakthroughMaxCarve ∷ Int
breakthroughMaxCarve = 50

-- * Coastal breakthrough

-- | Walk every river tile with @dir = dirNone@ (a true sink, no
-- downhill neighbour) that's above sea level. These are the stranded
-- inland mouths the breakthrough pass tries to extend to ocean.
findStrandedMouths
    ∷ Int                  -- ^ nTiles
    → VU.Vector Bool       -- ^ isRiverTile
    → VU.Vector Word8      -- ^ dir
    → VU.Vector Int        -- ^ terrain
    → [Int]
findStrandedMouths nTiles isRiverTile dir terrain =
    [ i | i ← [0 .. nTiles - 1]
        , isRiverTile VU.! i
        , dir VU.! i ≡ dirNone
        , terrain VU.! i > seaLevel
        ]

-- | Dijkstra from a stranded mouth to the nearest tile in 'worldOcean'.
--   Edge cost is @max 0 (next_z − current_z) + 1@ — uphill is the bulk
--   of the cost, plus a small per-step term so flat extensions don't
--   meander.  Stops when total cost exceeds 'breakthroughMaxCarve' or
--   when distance from start exceeds 'breakthroughRange'.
--
--   Returns @Just (path, totalCost)@ on success — path is from start
--   to ocean tile inclusive.
dijkstraBreakthrough
    ∷ Int                  -- ^ worldTiles
    → Int                  -- ^ start tile idx
    → VU.Vector Int        -- ^ terrain
    → VU.Vector Bool       -- ^ worldOcean
    → Maybe ([Int], Int)
dijkstraBreakthrough worldTiles startIdx terrain worldOcean = runST $ do
    let nTiles = worldTiles * worldTiles
    bestCost ← VUM.replicate nTiles (maxBound ∷ Int)
    parent   ← VUM.replicate nTiles (-1       ∷ Int)
    dist     ← VUM.replicate nTiles (maxBound ∷ Int)
    VUM.write bestCost startIdx 0
    VUM.write dist     startIdx 0
    foundRef ← newSTRef Nothing
    let neighbours i =
            let bx = i `mod` worldTiles
                by = i `div` worldTiles
                east = if bx < worldTiles - 1
                       then i + 1
                       else i + 1 - worldTiles
                west = if bx > 0
                       then i - 1
                       else i - 1 + worldTiles
                north = if by > 0
                        then Just (i - worldTiles) else Nothing
                south = if by < worldTiles - 1
                        then Just (i + worldTiles) else Nothing
            in [Just east, Just west, north, south]
        loop pq
            | IM.null pq = pure ()
            | otherwise = do
                let ((c, (i, rest)), pqAfter) = case IM.findMin pq of
                        (ck, vs) → case vs of
                            (v:rs) → ((ck, (v, rs)), IM.delete ck pq)
                            []     → ((ck, (-1, [])), IM.delete ck pq)
                    pq1 = if null rest
                          then pqAfter
                          else IM.insert c rest pqAfter
                if i < 0 then loop pq1
                else do
                  bc ← VUM.read bestCost i
                  if c > bc
                    then loop pq1
                    else do
                      done ← readSTRef foundRef
                      case done of
                        Just _ → pure ()
                        Nothing →
                          if worldOcean VU.! i
                            then writeSTRef foundRef (Just (i, c))
                            else do
                              d ← VUM.read dist i
                              if d ≥ breakthroughRange
                                then loop pq1
                                else do
                                  pq2 ← expandNeighbours i d c pq1
                                  loop pq2
        expandNeighbours i d c pq =
            foldM (tryStep i d c) pq (neighbours i)
        tryStep i d c pq mn =
            case mn of
                Nothing → pure pq
                Just nIdx → do
                    let nT = terrain VU.! nIdx
                    if nT ≡ minBound
                       then pure pq
                       else do
                         let edgeCost = max 0 (nT - terrain VU.! i) + 1
                             newCost  = c + edgeCost
                         if newCost > breakthroughMaxCarve
                            then pure pq
                            else do
                              bcN ← VUM.read bestCost nIdx
                              if newCost < bcN
                                then do
                                    VUM.write bestCost nIdx newCost
                                    VUM.write parent   nIdx i
                                    VUM.write dist     nIdx (d + 1)
                                    pure (IM.insertWith (++) newCost
                                              [nIdx] pq)
                                else pure pq
    loop (IM.singleton 0 [startIdx])
    fr ← readSTRef foundRef
    case fr of
        Nothing → pure Nothing
        Just (endIdx, c) → do
            let rebuild cur acc = do
                    p ← VUM.read parent cur
                    if p < 0 then pure (cur : acc)
                    else rebuild p (cur : acc)
            path ← rebuild endIdx []
            pure (Just (path, c))

-- | For each stranded inland mouth, run the breakthrough Dijkstra and
--   if it returns a path within the cost cap, mark every tile on the
--   path as river with width 0 and a monotonically descending surface
--   z (last step ends at @seaLevel + 1@).  Updates the compId for
--   path tiles so they get attached to the originating river in the
--   per-chunk index.
addBreakthroughs
    ∷ Int                  -- ^ worldTiles
    → VU.Vector Bool       -- ^ isRiverTile (post-cull)
    → VU.Vector Int        -- ^ compId
    → VU.Vector Word8      -- ^ dir
    → VU.Vector Int        -- ^ terrain
    → VU.Vector Bool       -- ^ worldOcean
    → VU.Vector Int        -- ^ widthRadius
    → VU.Vector Int        -- ^ surfZ
    → VU.Vector Int        -- ^ perpDist
    → ( VU.Vector Bool, VU.Vector Int, VU.Vector Int, VU.Vector Int
      , VU.Vector Int )
addBreakthroughs worldTiles isRiverTile compId dir terrain worldOcean
                 widthRadius surfZ perpDist =
    let nTiles = worldTiles * worldTiles
        mouths = findStrandedMouths nTiles isRiverTile dir terrain
    in runST $ do
        isRM   ← VU.thaw isRiverTile
        compM  ← VU.thaw compId
        widthM ← VU.thaw widthRadius
        surfM  ← VU.thaw surfZ
        perpM  ← VU.thaw perpDist
        forM_ mouths $ \m → do
            let mSurf = surfZ VU.! m
                mCid  = compId VU.! m
            when (mCid ≥ 0) $
                case dijkstraBreakthrough worldTiles m terrain worldOcean of
                    Nothing       → pure ()
                    Just (path, _) → do
                        -- Walk the path past the start (path[0] is the
                        -- mouth, already a river tile). Surface tracks
                        -- the natural terrain so a descending stretch
                        -- doesn't leave a many-z water column behind;
                        -- uphill stretches still descend at least 1 z
                        -- per step so the river never flows uphill.
                        -- Floor at @seaLevel + 1@ so the river hands
                        -- off cleanly to ocean at the path's end.
                        let walk _prevSurf [] = pure ()
                            walk prevSurf (p : rest) = do
                                let preCarve = terrain VU.! p
                                    target =
                                        max (seaLevel + 1)
                                            (min (prevSurf - 1) preCarve)
                                VUM.write isRM   p True
                                curW ← VUM.read widthM p
                                when (curW < 0) (VUM.write widthM p 0)
                                -- Path tiles are their own centreline.
                                curP ← VUM.read perpM p
                                when (curP > 0) (VUM.write perpM p 0)
                                VUM.write compM  p mCid
                                VUM.write surfM  p target
                                walk target rest
                        walk mSurf (drop 1 path)
        isRf   ← VU.unsafeFreeze isRM
        compF  ← VU.unsafeFreeze compM
        widthF ← VU.unsafeFreeze widthM
        surfF  ← VU.unsafeFreeze surfM
        perpF  ← VU.unsafeFreeze perpM
        pure (isRf, compF, widthF, surfF, perpF)
