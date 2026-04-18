{-# LANGUAGE Strict, UnicodeSyntax #-}
module Sim.River.Project
    ( projectRiverToChunk
    ) where

import UPrelude
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU
import World.Fluid.Types (FluidCell(..), FluidType(..))
import World.Fluid.Internal (FluidMap)
import World.River.Types (RiverMask, RiverTileInfo(..))
import World.Constants (seaLevel)

-- | Project river water into a chunk's fluid map from the
--   river mask.  For each tile that the mask says belongs to
--   a river, place River fluid at the precomputed water
--   surface — but only if the water surface is above the
--   terrain (positive depth).
--
--   This replaces chunk-owned river fluid with graph-derived
--   river fluid.  Static fluids (ocean, lake, lava) in the
--   existing map are preserved; only river tiles are touched.
--
--   The projection merges INTO an existing FluidMap so that
--   ocean/lake/lava tiles are not overwritten.  If a tile
--   already has non-river fluid, the existing fluid wins.
projectRiverToChunk ∷ RiverMask
                    → VU.Vector Int    -- ^ terrain surface map
                    → FluidMap         -- ^ existing fluid map (ocean/lake/lava)
                    → FluidMap         -- ^ merged: existing + river overlay
projectRiverToChunk mask terrainMap existing =
    let -- First pass: place river tiles from the mask
        withRivers = V.imap mergeFluid existing
        -- Second pass (iterated): fill 1-tile holes (dry tiles
        -- with ≥3 river neighbors). Iterate so filling one hole
        -- exposes the next.
        sz = chunkSz * chunkSz
        fillHoles prev =
            V.generate sz $ \idx →
                case prev V.! idx of
                    Just fc → Just fc
                    Nothing →
                        let lx = idx `mod` chunkSz
                            ly = idx `div` chunkSz
                            terrZ = terrainMap VU.! idx
                            nbrSurfs = [ fcSurface fc
                                       | (nx, ny) ← [(lx-1,ly),(lx+1,ly),(lx,ly-1),(lx,ly+1)]
                                       , nx ≥ 0, nx < chunkSz, ny ≥ 0, ny < chunkSz
                                       , Just fc ← [prev V.! (ny * chunkSz + nx)]
                                       , fcType fc ≡ River
                                       ]
                            -- Check for ocean/lake neighbors to fill
                            -- dry gaps between river and water bodies.
                            waterBodySurf = foldl' (\acc (nx, ny) →
                                if nx ≥ 0 ∧ nx < chunkSz ∧ ny ≥ 0 ∧ ny < chunkSz
                                then case prev V.! (ny * chunkSz + nx) of
                                    Just fc | fcType fc ≡ Ocean → Just (min (maybe maxBound id acc) (fcSurface fc))
                                    Just fc | fcType fc ≡ Lake  → Just (min (maybe maxBound id acc) (fcSurface fc))
                                    _                           → acc
                                else acc
                                ) Nothing [(lx-1,ly),(lx+1,ly),(lx,ly-1),(lx,ly+1)]
                            hasRiver = not (null nbrSurfs)
                            hasWaterBody = isJust waterBodySurf
                        in if length nbrSurfs ≥ 3
                           then let minS = foldl' min maxBound nbrSurfs
                                in if minS ≥ terrZ
                                   then Just (FluidCell River (terrZ + 1))
                                   else Nothing
                           -- Fill gap between river and ocean/lake.
                           -- Use the water body's type and surface for
                           -- a seamless transition.
                           else if hasRiver ∧ hasWaterBody
                           then case waterBodySurf of
                               Just ws | ws ≥ terrZ → Just (FluidCell River (terrZ + 1))
                               _                    → Nothing
                           else Nothing
        -- Extend river water into low coastal terrain. This
        -- bridges the gap between where the mask ends and
        -- where ocean/lake begins.  Iterative: each pass
        -- extends one tile outward from existing river edges
        -- into dry tiles whose terrain is below the adjacent
        -- river surface.
        extendCoastal prev =
            V.generate sz $ \idx →
                case prev V.! idx of
                    Just fc → Just fc
                    Nothing →
                        let lx = idx `mod` chunkSz
                            ly = idx `div` chunkSz
                            terrZ = terrainMap VU.! idx
                            riverNbrs =
                                [ fcSurface fc
                                | (nx, ny) ← [(lx-1,ly),(lx+1,ly),(lx,ly-1),(lx,ly+1)]
                                , nx ≥ 0, nx < chunkSz, ny ≥ 0, ny < chunkSz
                                , Just fc ← [prev V.! (ny * chunkSz + nx)]
                                , fcType fc ≡ River
                                ]
                            -- Check for ocean/lake neighbors too
                            hasBody = any (\(nx, ny) →
                                nx ≥ 0 ∧ nx < chunkSz ∧ ny ≥ 0 ∧ ny < chunkSz
                                ∧ case prev V.! (ny * chunkSz + nx) of
                                    Just fc → fcType fc ≡ Ocean ∨ fcType fc ≡ Lake
                                    Nothing → False
                                ) [(lx-1,ly),(lx+1,ly),(lx,ly-1),(lx,ly+1)]
                        in case riverNbrs of
                            []    → Nothing
                            (s:_) → let maxS = foldl' max minBound riverNbrs
                                    -- Extend into terrain below the MAX
                                    -- river neighbor surface (not min).
                                    -- Using max prevents the chain from
                                    -- breaking when a filled tile at
                                    -- terrZ+1 can't propagate to terrain
                                    -- at terrZ+1 (equal).
                                    in if terrZ < maxS
                                       then Just (FluidCell River (terrZ + 1))
                                       else Nothing
        -- 10 extensions (bridge river to coast) + hole fill
        ext1  = extendCoastal withRivers
        ext2  = extendCoastal ext1
        ext3  = extendCoastal ext2
        ext4  = extendCoastal ext3
        ext5  = extendCoastal ext4
        ext6  = extendCoastal ext5
        ext7  = extendCoastal ext6
        ext8  = extendCoastal ext7
        ext9  = extendCoastal ext8
        ext10 = extendCoastal ext9
        pass1 = fillHoles ext10
        pass2 = fillHoles pass1
        -- Smooth river mouths: convert near-sea-level river
        -- tiles adjacent to ocean → ocean. Iterate so chains
        -- of coastal river tiles all convert. Applies to ALL
        -- river tiles (mask and extension) — any river tile at
        -- near-sea-level terrain adjacent to ocean becomes ocean.
        smoothMouths prev =
            V.generate sz $ \idx →
                case prev V.! idx of
                    Just fc | fcType fc ≡ River →
                        let lx = idx `mod` chunkSz
                            ly = idx `div` chunkSz
                            adjOcean = any (\(nx, ny) →
                                nx ≥ 0 ∧ nx < chunkSz ∧ ny ≥ 0 ∧ ny < chunkSz
                                ∧ case prev V.! (ny * chunkSz + nx) of
                                    Just nfc → fcType nfc ≡ Ocean
                                    Nothing  → False
                                ) [(lx-1,ly),(lx+1,ly),(lx,ly-1),(lx,ly+1)]
                            terrZ = terrainMap VU.! idx
                        in if adjOcean ∧ terrZ ≤ seaLevel + 2
                           then Just (FluidCell Ocean seaLevel)
                           else Just fc
                    other → other
        -- Surface smoothing: cap each river tile's surface at
        -- (lowest river neighbor surface + 2). Prevents water
        -- cliffs between adjacent river tiles from different
        -- segments. Iterated to propagate across wide rivers.
        smoothSurface prev =
            V.generate sz $ \idx →
                case prev V.! idx of
                    Just fc | fcType fc ≡ River →
                        let lx = idx `mod` chunkSz
                            ly = idx `div` chunkSz
                            terrZ = terrainMap VU.! idx
                            nbrSurfs =
                                [ fcSurface nfc
                                | (nx, ny) ← [(lx-1,ly),(lx+1,ly),(lx,ly-1),(lx,ly+1)]
                                , nx ≥ 0, nx < chunkSz, ny ≥ 0, ny < chunkSz
                                , Just nfc ← [prev V.! (ny * chunkSz + nx)]
                                , fcType nfc ≡ River ∨ fcType nfc ≡ Ocean ∨ fcType nfc ≡ Lake
                                ]
                        in case nbrSurfs of
                            [] → Just fc
                            _  → let minN = foldl' min maxBound nbrSurfs
                                     cap = minN + 2
                                     s = fcSurface fc
                                     clamped = min s cap
                                 in if clamped > terrZ
                                    then Just (FluidCell River clamped)
                                    else Just (FluidCell River (terrZ + 1))
                    other → other
        -- Pool surface upward: raise river tiles toward the highest
        -- adjacent river surface. This fills cliff bases with water
        -- pooled from upstream, preventing the "river flowing into
        -- the ground" visual at coastal cliffs. Each iteration lifts
        -- tiles by at most (maxN - 1), creating a 1-block slope
        -- from the pool top. The terrain is only used as a minimum
        -- floor (water must be above terrain).
        poolUpward prev =
            V.generate sz $ \idx →
                case prev V.! idx of
                    Just fc | fcType fc ≡ River →
                        let lx = idx `mod` chunkSz
                            ly = idx `div` chunkSz
                            terrZ = terrainMap VU.! idx
                            mySurf = fcSurface fc
                            nbrRiverSurfs =
                                [ fcSurface nfc
                                | (nx, ny) ← [(lx-1,ly),(lx+1,ly),(lx,ly-1),(lx,ly+1)]
                                , nx ≥ 0, nx < chunkSz, ny ≥ 0, ny < chunkSz
                                , Just nfc ← [prev V.! (ny * chunkSz + nx)]
                                , fcType nfc ≡ River
                                ]
                        in case nbrRiverSurfs of
                            [] → Just fc
                            _  → let maxN = foldl' max minBound nbrRiverSurfs
                                     raised = maxN - 1
                                 in if raised > mySurf ∧ raised > terrZ
                                    then Just (FluidCell River raised)
                                    else Just fc
                    other → other
        -- Pool water upward FIRST: raise cliff-base river tiles
        -- toward upstream levels. Must run before smoothSurface
        -- or else smooth will drag upstream tiles DOWN to the
        -- cliff-base level, preventing the pool from working.
        pool1 = poolUpward pass2
        pool2 = poolUpward pool1
        pool3 = poolUpward pool2
        pool4 = poolUpward pool3
        pool5 = poolUpward pool4
        pool6 = poolUpward pool5
        pool7 = poolUpward pool6
        pool8 = poolUpward pool7
        sm1 = smoothMouths pool8
        sm2 = smoothMouths sm1
        sm3 = smoothMouths sm2
        sm4 = smoothMouths sm3
        -- Final fill pass to close gaps from mouth conversion,
        -- then one more smooth to catch any re-created coastal tiles.
        fill1 = fillHoles (fillHoles sm4)
        sm5 = smoothMouths fill1
        -- Post-smooth extension: smoothMouths converts coastal
        -- river→ocean, which breaks the extension chain (the
        -- converted tile is now ocean, so the next tile has no
        -- river neighbor to extend from). Re-extend after smooth
        -- to fill these 1-2 tile gaps at river mouths.
        postExt1 = extendCoastal sm5
        postExt2 = extendCoastal postExt1
        postExt3 = extendCoastal postExt2
        filled = fillHoles postExt3
    in filled
  where
    chunkSz = 16
    -- Water surface comes from the worldgen mask (which has
    -- natural depth variation from segment interpolation) but
    -- is clamped to at most terrain + maxDepth.  This preserves
    -- the natural slope profile while preventing extreme depth.
    maxDepth = 3 ∷ Int

    mergeFluid idx mExisting =
        case mExisting of
            -- Existing non-river fluid: keep as-is
            Just fc | fcType fc /= River → Just fc
            -- Existing River but no mask entry: remove (orphan)
            Just fc | fcType fc ≡ River → case mask V.! idx of
                Nothing → Nothing
                Just info → placeRiver idx info
            -- No fluid: check the mask
            Nothing → case mask V.! idx of
                Nothing → Nothing
                Just info → placeRiver idx info

    placeRiver idx info =
        let terrZ    = terrainMap VU.! idx
            wgenSurf = rtiWaterSurf info
            lx = idx `mod` chunkSz
            ly = idx `div` chunkSz
            -- If adjacent to lake/ocean, clamp to its surface+1
            -- so the river transitions smoothly into the body.
            adjBodySurf = foldl' (\acc (nx, ny) →
                if nx ≥ 0 ∧ nx < chunkSz ∧ ny ≥ 0 ∧ ny < chunkSz
                then case existing V.! (ny * chunkSz + nx) of
                    Just fc | fcType fc ≡ Lake  → Just (min (maybe maxBound id acc) (fcSurface fc + 1))
                    Just fc | fcType fc ≡ Ocean → Just (min (maybe maxBound id acc) (fcSurface fc + 1))
                    _                           → acc
                else acc
                ) Nothing [(lx-1,ly),(lx+1,ly),(lx,ly-1),(lx,ly+1)]
            -- Find lowest dry neighbor terrain so water doesn't
            -- form tall columns above lower surrounding land.
            -- Dry neighbor = no river mask entry AND terrain is
            -- near our elevation (not a carved cliff base or
            -- deep valley). Without the elevation filter, cliff
            -- bases drag the river surface down via dryClamp,
            -- causing the "river flowing into the ground" visual
            -- at coastal cliffs.
            dryNbrTerr =
                [ terrainMap VU.! (ny * chunkSz + nx)
                | (nx, ny) ← [(lx-1,ly),(lx+1,ly),(lx,ly-1),(lx,ly+1)]
                , nx ≥ 0, nx < chunkSz, ny ≥ 0, ny < chunkSz
                , isNothing (mask V.! (ny * chunkSz + nx))
                , let nbrT = terrainMap VU.! (ny * chunkSz + nx)
                -- Only clamp against dry tiles near our elevation.
                -- Deep valleys/cliffs (> 5 below) are carved channels
                -- that shouldn't drag our surface down.
                , nbrT ≥ terrZ - 5
                ]
            -- Water surface = min of:
            --   worldgen surface (natural slope)
            --   terrain + maxDepth (depth cap)
            --   lowest dry neighbor + 1 (no tall columns)
            --   adjacent water body surface + 1 (smooth transition)
            dryClamp = case dryNbrTerr of
                [] → terrZ + maxDepth
                _  → foldl' min (terrZ + maxDepth) dryNbrTerr + 1
            bodyClamp = case adjBodySurf of
                Nothing → terrZ + maxDepth
                Just bs → bs
            surf = min wgenSurf (min (terrZ + maxDepth) (min dryClamp bodyClamp))
        in if surf > terrZ
           then Just (FluidCell River surf)
           else Just (FluidCell River (terrZ + 1))
