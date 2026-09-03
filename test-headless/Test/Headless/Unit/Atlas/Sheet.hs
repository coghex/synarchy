{-# LANGUAGE Strict, OverloadedStrings #-}
-- | The padded, extruded atlas SHEET fixture and the source frames it
--   is composed from (#1259 TEX-3, #2076).
--
--   Shared because two owners measure the same pixels: the freshness
--   owner checks that the compiler\'s digest and the runtime\'s
--   source-frame comparison agree about this sheet, and the consumer
--   owner checks that the geometry reading it lands on the same texels.
--   Duplicating the sheet would let those two answers drift while both
--   kept passing.
--
--   A support leaf: it imports production modules only, never a spec
--   owner.
module Test.Headless.Unit.Atlas.Sheet
    ( fixtureW
    , fixtureH
    , fixtureCellW
    , fixtureCellH
    , fixtureCellPad
    , fixtureSlotW
    , fixtureSlotH
    , extrudedSheet
    , fixturePixels
    , fixtureAtlas
    , legacyFramePixels
    ) where

import UPrelude
import qualified Data.ByteString as BS
import qualified Data.Map.Strict as Map
import Unit.Atlas.Types
    ( AtlasAnimation(..), AtlasDirectionRow(..), AtlasStorageFormat(..) )
import Unit.Direction (Direction(..))

-- | An 8x4 RGBA8 sheet holding two 2x2 cells side by side on one row at
--   the #2076 padded stride: each cell sits at the centre of its own
--   4x4 slot, surrounded by a one-texel gutter that copies the cell's
--   own edge texels outward.
--
--   The two cells hold DIFFERENT art, so the gutter between them is
--   what a linear tap near either cell's inner edge would otherwise
--   cross — which is exactly what
--   "Test.Headless.Unit.Atlas.Consumers"'s isolation gate measures.
fixtureW, fixtureH, fixtureCellW, fixtureCellH ∷ Int
fixtureCellW = 2
fixtureCellH = 2
fixtureW = fixtureCols * fixtureSlotW
fixtureH = fixtureRows * fixtureSlotH

fixtureCellPad, fixtureSlotW, fixtureSlotH, fixtureCols, fixtureRows ∷ Int
fixtureCellPad = 1
fixtureSlotW = fixtureCellW + 2 * fixtureCellPad
fixtureSlotH = fixtureCellH + 2 * fixtureCellPad
fixtureCols = 2
fixtureRows = 1

-- | Compose a padded, extruded sheet from a per-slot source frame —
--   @tools\/pack_atlas.py@'s own @compose_atlas@ layout, so a fixture
--   sheet here is what the compiler would really emit rather than an
--   approximation of it.
--
--   A slot with no frame ('Nothing') stays fully transparent, gutter
--   included: that is the rectangularization padding D-5 leaves
--   unaddressable.
extrudedSheet
    ∷ Int → Int → Int                    -- ^ cell width, cell height, padding
    → Int → Int                          -- ^ columns, rows
    → (Int → Int → Maybe BS.ByteString)  -- ^ row, column → that cell's RGBA8
    → BS.ByteString
extrudedSheet cw ch pad cols rows cellAt = BS.pack
    [ b | y ← [0 .. rows * sh - 1], x ← [0 .. cols * sw - 1], b ← texel x y ]
  where
    sw = cw + 2 * pad
    sh = ch + 2 * pad
    texel x y =
        let (col, lx) = x `divMod` sw
            (row, ly) = y `divMod` sh
            -- Clamping BOTH axes into the cell IS the extrusion rule:
            -- an edge texel for a side, and the single corner texel for
            -- a corner square.
            cx = max 0 (min (cw - 1) (lx - pad))
            cy = max 0 (min (ch - 1) (ly - pad))
        in case cellAt row col of
            Nothing → [0, 0, 0, 0]
            Just px → let o = (cy * cw + cx) * 4
                      in [ BS.index px (o + i) | i ← [0 .. 3] ]

fixturePixels ∷ BS.ByteString
fixturePixels = extrudedSheet fixtureCellW fixtureCellH fixtureCellPad
    fixtureCols fixtureRows (\_ col → Just (legacyFramePixels col))
-- | The atlas metadata describing the fixture sheet as two 2x2 cells on
--   one row (DirS, two frames), at the #2076 padded stride.
fixtureAtlas ∷ AtlasAnimation
fixtureAtlas = AtlasAnimation
    { aaName = "clip", aaFormat = AtlasFormatPng
    , aaPath = "assets/textures/units/acolyte/atlas/clip.png"
    , aaAtlasWidth = fixtureW, aaAtlasHeight = fixtureH
    , aaCellWidth = fixtureCellW, aaCellHeight = fixtureCellH
    , aaCellPadding = fixtureCellPad
    , aaColumns = fixtureCols, aaRows = fixtureRows
    , aaFlip = False, aaFps = 8, aaLoop = True
    , aaDirections = Map.singleton DirS (AtlasDirectionRow DirS 0 2)
    , aaSourceDigest = "src", aaAtlasDigest = "atlas"
    }
-- | The two SOURCE frames, as standalone 2x2 images — now the fixture's
--   primary art, with the sheet composed FROM them by 'extrudedSheet'
--   rather than sliced out of it. Every texel is distinct within a
--   frame and the two frames disagree everywhere, so a wrong sub-rect
--   resolves to visibly different bytes rather than coincidentally
--   matching.
legacyFramePixels ∷ Int → BS.ByteString
legacyFramePixels col = BS.pack
    [ b | y ← [0 .. fixtureCellH - 1], x ← [0 .. fixtureCellW - 1]
        , b ← [ fromIntegral (col * 96 + x * 16 + y * 4 + 1)
              , fromIntegral (255 - (col * 96 + x * 16 + y * 4))
              , fromIntegral ((x * 7 + y * 13 + col * 41) `mod` 256)
              , 255 ] ]
