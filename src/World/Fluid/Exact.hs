{-# LANGUAGE Strict #-}
-- | The exact fluid-height scale (#2520, DFL-2).
--
--   Every fluid height in the engine is a SIGNED FIXED-POINT absolute
--   surface measured in eighths of a z-level. One constant,
--   'fluidUnitsPerZ', owns the scale; every conversion between that
--   plane, whole z, and same-footprint volume goes through a named
--   total helper here rather than ad-hoc arithmetic at the call site.
--
--   The plane is ABSOLUTE (an exact surface of @0@ is the top of the
--   column at world z 0, not a depth) and SIGNED, so a sub-sea column
--   at a negative z is an ordinary value rather than a special case.
--   Nothing here is partial: every helper is total for every 'Int',
--   including negative surfaces and zero.
--
--   Volume and the exact plane share ONE unit. An active cell's volume
--   is the number of units standing over its own terrain top, so
--   @'exactSurfaceOfZ' terrainZ + volume@ is that cell's exact surface
--   and a difference of two exact surfaces is already a volume — the
--   pressure expressions in "Sim.Fluid.Active" rely on exactly that and
--   must never re-multiply such a difference by the scale.
module World.Fluid.Exact
    ( fluidUnitsPerZ
    , exactSurfaceOfZ
    , exactSurfaceCeilZ
    , exactSurfaceFloorZ
    , exactTopLevel
    , exactVolumeOverTerrain
    ) where

import UPrelude

-- | Exact fluid units per whole z-level. THE scale constant: no other
--   module writes the number, and every threshold or flow rate that
--   used to be written against the old seven-unit level derives from
--   this instead.
fluidUnitsPerZ ∷ Int
fluidUnitsPerZ = 8

-- | A whole z-level as an exact absolute surface — a full column top,
--   fill level 8. Total for negative z.
exactSurfaceOfZ ∷ Int → Int
exactSurfaceOfZ z = z * fluidUnitsPerZ
{-# INLINE exactSurfaceOfZ #-}

-- | The integer CEILING view of an exact surface: the lowest whole z at
--   or above it. This is the compatibility view every integer consumer
--   reads — rendering, flora, ice, soil gates, the cursor, the Lua
--   queries and the dump — so a partially filled top z still reads as
--   a whole occupied level rather than disappearing.
exactSurfaceCeilZ ∷ Int → Int
exactSurfaceCeilZ e = (e + fluidUnitsPerZ - 1) `div` fluidUnitsPerZ
{-# INLINE exactSurfaceCeilZ #-}

-- | The integer FLOOR view of an exact surface: the highest whole z at
--   or below it, i.e. the last COMPLETELY filled level.
exactSurfaceFloorZ ∷ Int → Int
exactSurfaceFloorZ e = e `div` fluidUnitsPerZ
{-# INLINE exactSurfaceFloorZ #-}

-- | How full the TOP z of an exact surface is, in @1 .. 'fluidUnitsPerZ'@.
--   An exact multiple of the scale is a FULL level ('fluidUnitsPerZ'),
--   never @0@: the surface @z * 8@ is the brim of level @z - 1@, not an
--   empty level @z@.
exactTopLevel ∷ Int → Int
exactTopLevel e = ((e - 1) `mod` fluidUnitsPerZ) + 1
{-# INLINE exactTopLevel #-}

-- | The same-footprint volume an absolute exact surface represents over
--   a terrain top: the units standing above that terrain, never
--   negative. A surface at or below the terrain holds no volume.
exactVolumeOverTerrain ∷ Int → Int → Int
exactVolumeOverTerrain terrainZ e = max 0 (e - exactSurfaceOfZ terrainZ)
{-# INLINE exactVolumeOverTerrain #-}
