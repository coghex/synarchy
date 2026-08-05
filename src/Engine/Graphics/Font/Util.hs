module Engine.Graphics.Font.Util
    ( calculateTextWidthScaled
    ) where

import UPrelude
import Engine.Asset.Types (GlyphInfo(..))
import Engine.Graphics.Font.Data (FontAtlas(..))
import Engine.Graphics.Font.Fallback (resolveGlyph)

-- | Calculate text width using font atlas glyph data.
--
--   Goes through 'resolveGlyph' so an unavailable character measures
--   the fallback mark's advance — the same one both layout passes
--   consume, since a disagreement here truncates text and drifts
--   hit-tests (#1097).
calculateTextWidth ∷ FontAtlas → String → Double
calculateTextWidth atlas str =
    sum [ maybe 0 (realToFrac . giAdvance) (resolveGlyph atlas c)
        | c ← str
        ]

-- | Calculate text width at a desired font size (scaled)
calculateTextWidthScaled ∷ FontAtlas → Float → String → Double
calculateTextWidthScaled atlas desiredSize str = 
    let baseSize = fromIntegral $ faFontSize atlas
        scaleFactor = realToFrac desiredSize / baseSize
        baseWidth = calculateTextWidth atlas str
    in baseWidth * scaleFactor
