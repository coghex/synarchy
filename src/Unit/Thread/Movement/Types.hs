{-# LANGUAGE Strict, UnicodeSyntax #-}
-- | Shared movement primitives used across the climb / fall / leap
--   submodules: the per-unit movement stats snapshot, plus the small
--   set of pure constants and helpers more than one of those submodules
--   needs (gravity, facing). Kept separate from them to avoid import
--   cycles — e.g. Fall's descent and Leap's arc both share the one
--   gravity constant, and climb/fall/leap starts all share facing.
module Unit.Thread.Movement.Types
    ( UnitMoveStats(..)
    , defaultMoveStats
    , baselineUnitHeight
    , fallGravityZ
    , vectorToDirection
    ) where

import UPrelude
import Unit.Direction (Direction(..))
import Unit.Fall (gravity, metresPerZ)

-- | Per-unit movement stats relevant to climb/fall mechanics.
--   Snapshotted once per tick at the top of tickAllMovement so the
--   pure inner functions don't have to round-trip through the unit
--   manager.
data UnitMoveStats = UnitMoveStats
    { umsBodyMass  ∷ !Float   -- ^ kg
    , umsToughness ∷ !Float   -- ^ stat (1.0 = baseline)
    , umsClimbing  ∷ !Float   -- ^ skill 0..100
    , umsDexterity ∷ !Float   -- ^ stat (1.0 = baseline)
    , umsStrength  ∷ !Float   -- ^ stat (1.0 = baseline)
    , umsRunThreshold ∷ !Float -- ^ absolute speed (tiles/s) above which the
                               --   unit renders the Running anim instead of
                               --   Walking = def.run_threshold × def.max_speed.
    , umsHeight    ∷ !Float   -- ^ body height (metres). Drives climb reach:
                               --   how much of a cliff the unit mantles in the
                               --   pullup vs has to wall-climb first.
    }

-- | Baseline human body height in metres — the default when a unit
--   declares no "height" stat.
baselineUnitHeight ∷ Float
baselineUnitHeight = 1.8

defaultMoveStats ∷ UnitMoveStats
defaultMoveStats = UnitMoveStats
    { umsBodyMass  = 70.0
    , umsToughness = 1.0
    , umsClimbing  = 0.0
    , umsDexterity = 1.0
    , umsStrength  = 1.0
    , umsRunThreshold = 1.0e9  -- no def → never auto-run (sentinel high)
    , umsHeight    = baselineUnitHeight
    }

-- | Free-fall gravity in z-levels/s². Derived from the injury model's
--   constants (Unit.Fall.gravity m/s² ÷ metresPerZ) so the motion that
--   brings a unit down stays consistent with the impact energy the fall
--   injuries assume — one source of truth for "how hard gravity pulls."
--   Shared by Fall (descent) and Leap (arc flight time).
fallGravityZ ∷ Float
fallGravityZ = gravity / metresPerZ

-- | Map a tile-grid velocity vector to one of 8 compass directions.
--
-- The sprite labels are aligned with the iso projection's compass, not
-- the raw grid axes:
--   DirS  = tile-grid (+x, +y)  — projects to screen-down
--   DirSE = tile-grid (+x,  0)  — projects to screen-lower-right
--   DirE  = tile-grid (+x, -y)  — projects to screen-right
--   DirNE = tile-grid ( 0, -y)
--   DirN  = tile-grid (-x, -y)
--   DirNW = tile-grid (-x,  0)
--   DirW  = tile-grid (-x, +y)
--   DirSW = tile-grid ( 0, +y)
-- That makes the result in tile-grid frame, which is what the renderer
-- expects — `Unit.Render.resolveTexture` then applies `cameraRotSteps`
-- to keep the on-screen direction consistent through camera rotations.
vectorToDirection ∷ Float → Float → Direction
vectorToDirection nx ny
    | norm < 22.5   = DirSE
    | norm < 67.5   = DirS
    | norm < 112.5  = DirSW
    | norm < 157.5  = DirW
    | norm < 202.5  = DirNW
    | norm < 247.5  = DirN
    | norm < 292.5  = DirNE
    | norm < 337.5  = DirE
    | otherwise     = DirSE
  where
    deg  = atan2 ny nx * (180.0 / pi)
    norm = if deg < 0 then deg + 360 else deg
