{-# LANGUAGE Strict, UnicodeSyntax #-}
-- | Zero-size (minimized window / minimize-restore) guards for the
--   screen→world projection math shared across the render, pick, and
--   hit-test paths.
--
--   Pixel→world normalization divides by the window width/height and the
--   aspect ratio divides by the framebuffer height. When the window or
--   framebuffer collapses to zero size (a minimized window, or a
--   minimize/restore transition mid-drag) those divisions produce
--   @Infinity@/@NaN@ cursor, hover, camera, hit-test, and culling values.
--
--   This module is the single convention for that guard: projection and
--   hit-test paths treat a degenerate viewport as "no tile" (skip the
--   frame / report nothing), and culling/view-bounds math uses
--   'safeAspect' so it stays finite. The already-guarded
--   @engine.getWorldCoord@ and the input thread's UI click routing use
--   the same condition.
module Engine.Graphics.Viewport
    ( windowDegenerate
    , viewportDegenerate
    , safeAspect
    ) where

import UPrelude

-- | True when the window is zero-size, so the pixel→normalized-coordinate
--   divisions by @winW@/@winH@ would yield non-finite values. Used by the
--   hit-test paths, which derive their aspect ratio from the window size.
windowDegenerate ∷ Int → Int → Bool
windowDegenerate winW winH = winW ≤ 0 ∨ winH ≤ 0

-- | True when the window OR the framebuffer height is zero-size. Used by
--   the paths that normalize by the window size AND derive their aspect
--   ratio from the framebuffer (the world tile pick and the zoom-map
--   chunk pick).
viewportDegenerate ∷ Int → Int → Int → Bool
viewportDegenerate winW winH fbH = windowDegenerate winW winH ∨ fbH ≤ 0

-- | Aspect ratio (framebuffer width / height) guarded against a zero-size
--   framebuffer; returns 1 rather than @Infinity@/@NaN@ so culling and
--   view-bounds math stay finite during a minimize/restore transition.
safeAspect ∷ Int → Int → Float
safeAspect fbW fbH
    | fbH ≤ 0   = 1.0
    | otherwise = fromIntegral fbW / fromIntegral fbH
