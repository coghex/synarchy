{-# LANGUAGE Strict #-}
-- | Solar (day\/night) attribution values the render path carries
--   (#483, #1869).
--
--   Longitude-local day\/night gives every vertex its own phase, but
--   until #1869 the two inputs that phase is built from — the base sun
--   angle and the world's u-axis circumference — were single UBO
--   scalars resolved from the HEAD of @wmVisible@. Several pages can be
--   visible at once, each advancing on its own clock
--   ("World.Thread.Time") and generated at its own size, so every page
--   but the head was lit by someone else's time of day and divided by
--   someone else's circumference.
--
--   This module holds the two values that fix it, both deliberately
--   free of any @World.*@ dependency so the render layer can carry them:
--
--     * 'SolarBase' — the process-global base angle, and whether
--       @world.setSunAngle@ is currently overriding it.
--     * 'SolarPageTable' — the per-page (angle, circumference) pairs a
--       frame's geometry is attributed to, indexed by the small slot a
--       vertex carries in @solarPage@.
--
--   Assigning pages to slots needs page identity and therefore lives on
--   the world side, in "World.Render.Solar".
module Engine.Graphics.Solar
    ( -- * The process-global base angle
      SolarBase(..)
    , publishedSolar
    , overriddenSolar
      -- * Per-page attribution
    , SolarPageEntry(..)
    , SolarPageTable(..)
    , emptySolarPageTable
    , maxSolarPages
    , solarPageNone
    , solarSlotVertexValue
    , solarUniformEntries
    ) where

import UPrelude
import qualified Data.Vector as V
import Linear (V4(..))

-- | The process-global base sun angle: the UBO's @sunAngle@ member.
--
--   It is what page-LESS geometry is lit by — generic
--   'Engine.Scene.Batch.Sprite' scene nodes carry no world page, so
--   they keep the single-page behaviour they have always had — and
--   what @world.getClimateAt@-style queries report.
--
--   'sbOverridden' records WHERE the angle came from, which is the
--   whole of @world.setSunAngle@'s defined meaning (#1869 requirement
--   5). "World.Thread.Time" publishes the visible head page's own clock
--   angle with 'sbOverridden' 'False' on every world tick;
--   @world.setSunAngle@ replaces the angle and sets the flag, and while
--   it is set EVERY rendered page takes this angle as its base (each
--   still dividing by its own circumference). The next visible-page
--   clock publication clears it, which is exactly how long the override
--   lasted before #1869 — the scalar it wrote was overwritten by the
--   very next tick.
data SolarBase = SolarBase
    { sbAngle      ∷ !Float
      -- ^ The base angle itself, in turns (0..1).
    , sbOverridden ∷ !Bool
      -- ^ 'True' when @world.setSunAngle@ set 'sbAngle' and no world
      --   tick has republished since.
    } deriving (Show, Eq)

-- | A base angle published from a page's own clock.
publishedSolar ∷ Float → SolarBase
publishedSolar angle = SolarBase angle False

-- | A base angle forced by @world.setSunAngle@.
overriddenSolar ∷ Float → SolarBase
overriddenSolar angle = SolarBase angle True

-- | One visible page's solar inputs.
data SolarPageEntry = SolarPageEntry
    { speSunAngle           ∷ !Float
      -- ^ The page's base angle: its own clock's angle, or the
      --   'SolarBase' angle while @world.setSunAngle@ overrides.
    , speCircumferenceTiles ∷ !Float
      -- ^ The page's OWN u-axis (gx-gy) circumference in tiles, never
      --   another page's. Always at least 1 — the shader divides by it.
    } deriving (Show, Eq)

-- | The frame's per-page solar inputs, in slot order: the entry at
--   index @i@ is what a vertex carrying @solarPage = i + 1@ is lit by.
--
--   Published as part of 'Engine.Scene.Types.Batch.LayeredQuads' rather
--   than through a ref of its own, ON PURPOSE. The main renderer is
--   allowed to observe the previous @worldQuadsRef@ value for a frame
--   ("World.Thread"), so a table travelling separately could describe a
--   different visible set than the vertices being drawn. Travelling
--   inside the same immutable value makes that impossible: whichever
--   generation of quads a frame reads, it reads that generation's table.
newtype SolarPageTable = SolarPageTable
    { sptEntries ∷ V.Vector SolarPageEntry
    } deriving (Show, Eq)

emptySolarPageTable ∷ SolarPageTable
emptySolarPageTable = SolarPageTable V.empty

-- | How many pages one frame can attribute individually.
--
--   The UBO carries exactly this many @vec4@s, so it is a hard cap on
--   both sides. Visible-page sets are one or two pages in every shipped
--   flow; a set larger than this degrades gracefully rather than
--   failing — the pages past the cap fall back to 'solarPageNone', i.e.
--   the pre-#1869 global attribution.
maxSolarPages ∷ Int
maxSolarPages = 8

-- | The @solarPage@ value meaning \"this vertex belongs to no world
--   page\": UI and generic scene sprites, and any page past
--   'maxSolarPages'. Such a vertex is lit by the UBO's global
--   @sunAngle@ \/ @worldCircumferenceTiles@ — the pre-#1869 path,
--   preserved deliberately.
solarPageNone ∷ Word32
solarPageNone = 0

-- | The @solarPage@ value for a zero-based slot index, or
--   'solarPageNone' when the index is past 'maxSolarPages'.
solarSlotVertexValue ∷ Int → Word32
solarSlotVertexValue slot
    | slot < 0 ∨ slot ≥ maxSolarPages = solarPageNone
    | otherwise                       = fromIntegral slot + 1

-- | The table as the UBO's fixed-length @vec4 solarPages[maxSolarPages]@:
--   @x@ = base angle, @y@ = circumference, @z@\/@w@ unused (reserved).
--
--   Every unused slot is filled with the caller's global fallback pair
--   rather than zeroes, so a vertex naming a slot the table does not
--   describe is lit exactly like a page-less one instead of being
--   divided by zero. The shader still guards its index; this makes the
--   guard's outcome meaningful rather than merely safe.
solarUniformEntries ∷ Float            -- ^ fallback base angle
                    → Float            -- ^ fallback circumference in tiles
                    → SolarPageTable
                    → V.Vector (V4 Float)
solarUniformEntries fallbackAngle fallbackCirc (SolarPageTable entries) =
    V.generate maxSolarPages $ \i → case entries V.!? i of
        Just e  → V4 (speSunAngle e) (max 1 (speCircumferenceTiles e)) 0 0
        Nothing → V4 fallbackAngle (max 1 fallbackCirc) 0 0
