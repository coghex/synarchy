{-# LANGUAGE UnicodeSyntax #-}
-- | Build the zoom cache at world init time, split (issue #573) into
--   focused submodules under "World.ZoomMap.Cache.*":
--
--     * "World.ZoomMap.Cache.Noise" — zoom-level ice noise.
--     * "World.ZoomMap.Cache.Classify" — majority-material and
--       climate-vegetation chunk classification.
--     * "World.ZoomMap.Cache.Pixels" — per-chunk RGBA pixel generation.
--     * "World.ZoomMap.Cache.Build" — 'buildZoomCache' (entries only).
--     * "World.ZoomMap.Cache.BuildPixels" — 'buildZoomCacheWithPixels'
--       (entries + per-tile pixel data).
--
--   This module re-exports the public API unchanged. Pure
--   world-generation logic – no rendering imports.
module World.ZoomMap.Cache
    ( buildZoomCache
    , buildZoomCacheWithPixels
    , majorityMaterial
    ) where

import World.ZoomMap.Cache.Build (buildZoomCache)
import World.ZoomMap.Cache.BuildPixels (buildZoomCacheWithPixels)
import World.ZoomMap.Cache.Classify (majorityMaterial)
