{-# LANGUAGE Strict, UnicodeSyntax #-}
-- | Longitude-local solar time (#483). The world clock ('World.Thread.Time')
--   advances one global @sunAngle@, but the world is a cylinder along
--   u = gx − gy (see 'World.Plate.worldWidthTiles' /
--   'World.Plate.wrapGlobalU'): noon should sweep around that
--   circumference once per day, not happen everywhere at once.
--
--   'WorldTime'/'WorldDate' stay global (UTC-like) — only the diurnal
--   PHASE is longitude-local. A tile's local sun angle is the global
--   angle plus a fraction of a day proportional to how far around the
--   cylinder it sits.
module World.Time.Local
    ( localSunAngle
    ) where

import UPrelude
import World.Chunk.Types (chunkSize)

-- | Local sun angle (0..1) at a given tile, given the world size (in
--   chunks) and the current global sun angle. Seam-safe by
--   construction: u and u ± circumference give the same fraction, so
--   wrapped/aliased draws at the seam agree exactly. Degenerate
--   worldSize (≤ 0, e.g. a not-yet-generated arena) falls back to a
--   circumference of 1 chunk-width rather than dividing by zero.
localSunAngle ∷ Int → Int → Int → Float → Float
localSunAngle worldSize gx gy globalSunAngle =
    let circumference = fromIntegral (max 1 worldSize * chunkSize) ∷ Float
        u = fromIntegral (gx - gy) ∷ Float
        raw = globalSunAngle + u / circumference
    in raw - fromIntegral (floor raw ∷ Int)
