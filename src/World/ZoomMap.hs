{-# LANGUAGE Strict, UnicodeSyntax #-}
-- | Thin facade – re-exports the three public entry points so that
--   existing call sites ('World.Render') need no import changes.
module World.ZoomMap
    ( generateZoomMapQuads
    , generateBackgroundQuads
    , buildZoomCache
    ) where

import World.ZoomMap.Cache       (buildZoomCache)
import World.Render.Zoom.Quads   (generateZoomMapQuads)
import World.Render.Zoom.Background (generateBackgroundQuads)
