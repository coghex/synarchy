module Engine.Graphics.Font.Util
    ( calculateTextWidth
    , calculateTextWidthScaled
    ) where

import UPrelude
import qualified Data.Map.Strict as Map
import Engine.Asset.Types (GlyphInfo(..))
import Engine.Graphics.Font.Data (FontAtlas(..))

-- | Calculate text width using font atlas glyph data
calculateTextWidth ∷ FontAtlas → String → Double
calculateTextWidth atlas str = 
    sum [ maybe 0 (realToFrac . giAdvance) (Map.lookup c (faGlyphData atlas)) 
        | c ← str 
        ]

-- | Calculate text width at a desired font size (scaled)
calculateTextWidthScaled :: FontAtlas -> Float -> String -> Double
calculateTextWidthScaled atlas desiredSize str = 
    let baseSize = fromIntegral $ faFontSize atlas
        scaleFactor = realToFrac desiredSize / baseSize
        baseWidth = calculateTextWidth atlas str
    in baseWidth * scaleFactor
