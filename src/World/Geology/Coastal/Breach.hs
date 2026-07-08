{-# LANGUAGE Strict, UnicodeSyntax #-}

-- | Sealed sub-sea basin breaching: connects large, deep basins that
--   the coastal erosion pass leaves sealed behind the shoreline ridge
--   to the open ocean by carving a narrow channel.
module World.Geology.Coastal.Breach
    ( breachSealedBasins
    ) where

import UPrelude
import Control.Monad (foldM)
import Control.Monad.ST (runST)
import qualified Data.IntMap.Strict as IM
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Unboxed.Mutable as VUM
import World.Types
import World.Material (MaterialId(..), matGlacier)
import World.Plate (wrapGlobalU)

-- * Sealed-Basin Breaching

-- | Minimum tile count for a sealed sub-sea basin to earn a breach.
breachMinSize ∷ Int
breachMinSize = 12

-- | Minimum depth below sea for a sealed basin to earn a breach.
breachMinDepth ∷ Int
breachMinDepth = 4

-- | Longest land channel (tiles) a breach may carve to reach the sea.
--   Generous on purpose: a long narrow mouth through a headland reads
--   as a fjord inlet, and an unbreached deep basin reads as a bug
--   (a sea-level "lake" 40z deep behind the shore).
breachMaxChannel ∷ Int
breachMaxChannel = 16

-- | Connect large, deep sub-sea basins to the open ocean by carving a
--   narrow channel through the shoreline ridge that seals them (#220).
--   Steep coasts keep their natural terrain, which frequently seals a
--   deep pre-coastal basin behind the shore; unbreached, those flood
--   later as sea-level "lakes" tens of z deep (the FLOATING_LAKE
--   family), half-classified as ocean by the oceanic-chunk rule. The
--   old universal flatten eroded those ridges below sea by accident —
--   this makes the same connection deliberately and minimally, and the
--   resulting one-tile mouths read as natural deep inlets on cliff
--   coasts. Channel tiles are LOWERED to sea-1 (they are land above
--   sea by construction, so the pass stays lower-only). Small or
--   shallow sealed ponds are left alone — a lagoon behind a sandbar
--   is a feature.
--
--   Topology note: decisions are made on CANONICAL tiles only (their
--   own 'wrapGlobalU' image) and carves are projected onto every grid
--   copy at the end — running naively on the square grid gives
--   near-seam basins independent canonical/alias component ids, and
--   one copy carves while its twin doesn't (a seam artifact, verified
--   on seed 12321). Adjacency deliberately does NOT wrap across the
--   u-seam: connectivity here must match what the engine's
--   'computeWorldEdgeOcean' (edges-as-walls, no wrap) will later
--   compute, or a basin whose only sub-sea route to the ocean crosses
--   the seam is "connected" to this pass yet sealed to the engine —
--   and floods as a deep sea-level lake anyway. A physically
--   cross-seam-connected basin just gains a redundant local inlet.
breachSealedBasins ∷ Int → VU.Vector MaterialId → VU.Vector Int
                   → VU.Vector Int
breachSealedBasins worldSize matVec elev = runST $ do
    let worldTiles = worldSize * chunkSize
        halfWorld = worldTiles `div` 2
        area = worldTiles * worldTiles
        inGrid gx gy = gx ≥ -halfWorld ∧ gx < halfWorld
                     ∧ gy ≥ -halfWorld ∧ gy < halfWorld
        toIdx gx gy = (gy + halfWorld) * worldTiles + (gx + halfWorld)
        isCanonical i =
            let gx = i `mod` worldTiles - halfWorld
                gy = i `div` worldTiles - halfWorld
                (gx', gy') = wrapGlobalU worldSize gx gy
            in gx' ≡ gx ∧ gy' ≡ gy
        -- Canonical neighbors under the ENGINE's connectivity model:
        -- no u-seam wrap (see topology note above).
        neighborIdxs idx =
            let gx = idx `mod` worldTiles - halfWorld
                gy = idx `div` worldTiles - halfWorld
            in [ nIdx
               | (nx, ny) ← [ (gx - 1, gy), (gx + 1, gy)
                            , (gx, gy - 1), (gx, gy + 1) ]
               , inGrid nx ny
               , let nIdx = toIdx nx ny
               , isCanonical nIdx
               ]

    -- Pass 1: label canonical sub-sea connected components
    -- (4-adjacency, wrap-aware).
    comp ← VUM.replicate area (-1 ∷ Int)
    queue ← VUM.new area
    nComps ← VUM.replicate 1 (0 ∷ Int)
    forM_ [0 .. area - 1] $ \i →
        when (isCanonical i ∧ elev VU.! i ≤ seaLevel) $ do
            ci ← VUM.read comp i
            when (ci ≡ -1) $ do
                cid ← VUM.read nComps 0
                VUM.write nComps 0 (cid + 1)
                VUM.write comp i cid
                VUM.write queue 0 i
                let bfs !qHead !qTail =
                        when (qHead < qTail) $ do
                            idx ← VUM.read queue qHead
                            newTail ← foldM
                                (\ !t nIdx → do
                                    nc ← VUM.read comp nIdx
                                    if nc ≡ -1 ∧ elev VU.! nIdx ≤ seaLevel
                                    then do
                                        VUM.write comp nIdx cid
                                        VUM.write queue t nIdx
                                        pure (t + 1)
                                    else pure t)
                                qTail (neighborIdxs idx)
                            bfs (qHead + 1) newTail
                bfs 0 1

    -- Pass 2: per-component size and minimum elevation.
    n ← VUM.read nComps 0
    if n ≤ 1
    then pure elev  -- zero or one water body: nothing to connect
    else do
        sizes ← VUM.replicate n (0 ∷ Int)
        minEs ← VUM.replicate n (maxBound ∷ Int)
        forM_ [0 .. area - 1] $ \i → do
            ci ← VUM.read comp i
            when (ci ≥ 0) $ do
                sz ← VUM.read sizes ci
                VUM.write sizes ci (sz + 1)
                me ← VUM.read minEs ci
                when (elev VU.! i < me) $
                    VUM.write minEs ci (elev VU.! i)

        -- Open ocean = largest component (first on ties — scan order
        -- is deterministic, and alias copies of near-seam water carry
        -- identical geometry, so the seam sees consistent choices).
        oceanRef ← VUM.replicate 1 (0 ∷ Int)
        forM_ [1 .. n - 1] $ \c → do
            best ← VUM.read oceanRef 0
            bsz ← VUM.read sizes best
            csz ← VUM.read sizes c
            when (csz > bsz) $ VUM.write oceanRef 0 c
        oceanId ← VUM.read oceanRef 0

        -- Pass 3: for each qualifying sealed basin, multi-source BFS
        -- through carvable land (level-order, deterministic) until a
        -- tile adjacent to the open ocean is found, then carve the
        -- parent chain to sea-1. Basins with no route within
        -- 'breachMaxChannel' stay sealed (deep inland depressions).
        qual ← VUM.replicate n False
        forM_ [0 .. n - 1] $ \c →
            when (c ≢ oceanId) $ do
                sz ← VUM.read sizes c
                me ← VUM.read minEs c
                when (sz ≥ breachMinSize ∧ me ≤ seaLevel - breachMinDepth) $
                    VUM.write qual c True

        -- One scan: collect the carvable-land ring around every
        -- qualifying basin, grouped per basin (ascending tile order).
        seedMap ← foldM
            (\ !m i → do
                ci ← VUM.read comp i
                if ci < 0 then pure m
                else do
                    q ← VUM.read qual ci
                    if not q then pure m
                    else do
                        ring ← foldM
                            (\ !acc nIdx →
                                if elev VU.! nIdx > seaLevel
                                   ∧ matVec VU.! nIdx ≢ matGlacier
                                then pure (nIdx : acc)
                                else pure acc)
                            [] (neighborIdxs i)
                        pure (if null ring then m
                              else IM.insertWith (⧺) ci ring m))
            IM.empty [0 .. area - 1]

        carveMask ← VUM.replicate area False
        -- visited/parent are generation-stamped by basin id so they
        -- need no clearing between basins.
        visited ← VUM.replicate area (-1 ∷ Int)
        parent  ← VUM.replicate area (-1 ∷ Int)
        let carvable i =
                pure (elev VU.! i > seaLevel ∧ matVec VU.! i ≢ matGlacier)
            carveChain i = do
                VUM.write carveMask i True
                p ← VUM.read parent i
                when (p ≥ 0) $ carveChain p

        forM_ (IM.toAscList seedMap) $ \(c, ring) → do
            -- Seed the queue (dedupe via the visited stamp).
            seedTail ← foldM
                (\ !t nIdx → do
                    vs ← VUM.read visited nIdx
                    ok ← carvable nIdx
                    if ok ∧ vs ≢ c
                    then do
                        VUM.write visited nIdx c
                        VUM.write parent nIdx (-1)
                        VUM.write queue t nIdx
                        pure (t + 1)
                    else pure t)
                0 ring
            -- Level-order search for a tile touching the ocean.
            let search !qHead !qTail !depth
                  | qHead ≥ qTail ∨ depth > breachMaxChannel = pure (-1)
                  | otherwise = do
                      (newTail, hit) ← foldM
                          (\(!t, !h) qi → do
                              idx ← VUM.read queue qi
                              if h ≥ 0 then pure (t, h)
                              else do
                                touches ← foldM
                                    (\ !acc nIdx → do
                                        nc ← VUM.read comp nIdx
                                        pure (acc ∨ nc ≡ oceanId))
                                    False (neighborIdxs idx)
                                if touches then pure (t, idx)
                                else do
                                  t' ← foldM
                                      (\ !tt nIdx → do
                                          ok ← carvable nIdx
                                          vs ← VUM.read visited nIdx
                                          if ok ∧ vs ≢ c
                                          then do
                                              VUM.write visited nIdx c
                                              VUM.write parent nIdx idx
                                              VUM.write queue tt nIdx
                                              pure (tt + 1)
                                          else pure tt)
                                      t (neighborIdxs idx)
                                  pure (t', -1))
                          (qTail, -1) [qHead .. qTail - 1]
                      if hit ≥ 0
                      then pure hit
                      else search qTail newTail (depth + 1)
            hit ← search 0 seedTail 0
            when (hit ≥ 0) $ carveChain hit

        -- Project canonical carves onto every grid copy (canonical +
        -- u-seam alias) so the per-chunk tables stay alias-consistent.
        out ← VU.thaw elev
        forM_ [0 .. area - 1] $ \i → do
            let gx = i `mod` worldTiles - halfWorld
                gy = i `div` worldTiles - halfWorld
                (gx', gy') = wrapGlobalU worldSize gx gy
            when (inGrid gx' gy') $ do
                m ← VUM.read carveMask (toIdx gx' gy')
                when m $ VUM.write out i (seaLevel - 1)
        VU.unsafeFreeze out
