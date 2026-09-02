-- | The authoritative video-configuration domain (#2198): every bound a
--   video setting must satisfy, stated once, plus the leaf checks that
--   apply them. Three boundaries decide validity here and nowhere else:
--   YAML loading ('Engine.Graphics.Config.loadVideoConfig', which
--   defaults an out-of-domain leaf field by field), every Lua setter
--   ('Engine.Scripting.Lua.API.Config', which refuses the call whole)
--   and the exported 'Engine.Graphics.Config.saveVideoConfig' (which
--   refuses the write). The Settings screen mirrors these numbers in
--   @scripts/settings/data.lua@, and the headless
--   @video config validation@ describe fails if the two drift.
--
--   The numeric domain:
--
--   * @resolution.width@ / @resolution.height@: positive integers.
--   * @ui_scale@: finite after narrowing to the stored 'Float', within
--     ['uiScaleMin', 'uiScaleMax'].
--   * @frame_limit@: unlimited (YAML @null@ or omitted; Lua integer 0),
--     or an integer within ['frameLimitMin', 'frameLimitMax'].
--   * @msaa@: one of 'msaaChoices'.
--   * @brightness@: an integer within ['brightnessMin', 'brightnessMax'].
--   * @tooltip_dwell_ms@ / @tooltip_hint_delay_ms@: integers within
--     ['tooltipMsMin', 'tooltipMsMax'].
--
--   The token-valued leaves (@window_mode@, @texture_filter@) are checked
--   beside their types in 'Engine.Graphics.Config' with the same
--   case-insensitive matching they always had; only their domain
--   descriptions live here so every rejection reads the same. VSync and
--   pixel snap are Booleans and have no out-of-domain member.
--
--   This module depends on nothing local but 'UPrelude', so
--   'Engine.Graphics.Config' and its consumers can all import it
--   without a cycle.
module Engine.Graphics.Config.Domain
  ( VideoFieldRejection(..)
  , describeRejection
    -- * Bounds
  , uiScaleMin, uiScaleMax
  , frameLimitMin, frameLimitMax
  , msaaChoices
  , brightnessMin, brightnessMax
  , tooltipMsMin, tooltipMsMax
    -- * Field names (the full YAML path of each leaf)
  , fieldWidth, fieldHeight, fieldWindowMode, fieldUIScale, fieldVSync
  , fieldFrameLimit, fieldMSAA, fieldBrightness, fieldPixelSnap
  , fieldTextureFilter, fieldTooltipDwellMs, fieldTooltipHintDelayMs
    -- * Domain descriptions
  , dimensionDomain, uiScaleDomain, frameLimitDomain, msaaDomain
  , brightnessDomain, tooltipMsDomain, windowModeDomain
  , textureFilterDomain
    -- * Leaf checks
  , narrowUIScale
  , checkDimension
  , checkUIScale
  , checkFrameLimit
  , checkMSAA
  , checkBrightness
  , checkTooltipMs
  ) where

import UPrelude
import qualified Data.Text as T
import GHC.Float (double2Float)

-- | One leaf that missed the domain: which field, the value it carried
--   (rendered for a log line) and the domain it had to satisfy. Every
--   boundary logs exactly this, so a rejected value is always traceable
--   to its field whichever path refused it.
data VideoFieldRejection = VideoFieldRejection
    { vfrField  ∷ Text
    , vfrValue  ∷ Text
    , vfrDomain ∷ Text
    } deriving (Show, Eq)

-- | The one-line rendering every boundary's log line embeds.
describeRejection ∷ VideoFieldRejection → Text
describeRejection r =
    vfrField r <> " = " <> vfrValue r
      <> " is outside the domain (" <> vfrDomain r <> ")"

-- * Bounds

uiScaleMin, uiScaleMax ∷ Float
uiScaleMin = 0.5
uiScaleMax = 4.0

frameLimitMin, frameLimitMax ∷ Int
frameLimitMin = 30
frameLimitMax = 240

msaaChoices ∷ [Int]
msaaChoices = [1, 2, 4, 8]

brightnessMin, brightnessMax ∷ Int
brightnessMin = 50
brightnessMax = 300

tooltipMsMin, tooltipMsMax ∷ Int
tooltipMsMin = 0
tooltipMsMax = 1000

-- * Field names

fieldWidth, fieldHeight, fieldWindowMode, fieldUIScale, fieldVSync
  , fieldFrameLimit, fieldMSAA, fieldBrightness, fieldPixelSnap
  , fieldTextureFilter, fieldTooltipDwellMs, fieldTooltipHintDelayMs ∷ Text
fieldWidth              = "video.resolution.width"
fieldHeight             = "video.resolution.height"
fieldWindowMode         = "video.window_mode"
fieldUIScale            = "video.ui_scale"
fieldVSync              = "video.vsync"
fieldFrameLimit         = "video.frame_limit"
fieldMSAA               = "video.msaa"
fieldBrightness         = "video.brightness"
fieldPixelSnap          = "video.pixel_snap"
fieldTextureFilter      = "video.texture_filter"
fieldTooltipDwellMs     = "video.tooltip_dwell_ms"
fieldTooltipHintDelayMs = "video.tooltip_hint_delay_ms"

-- * Domain descriptions

dimensionDomain ∷ Text
dimensionDomain = "a positive integer"

uiScaleDomain ∷ Text
uiScaleDomain = "a finite number from " <> tshow uiScaleMin
                  <> " to " <> tshow uiScaleMax

-- | The frame-limit domain, given how "unlimited" is spelled at the
--   boundary doing the rejecting: @null@ for YAML, @0@ for Lua.
frameLimitDomain ∷ Text → Text
frameLimitDomain unlimited =
    unlimited <> " for unlimited, or an integer from "
      <> tshow frameLimitMin <> " to " <> tshow frameLimitMax

msaaDomain ∷ Text
msaaDomain = "one of " <> T.intercalate ", " (map tshow msaaChoices)

brightnessDomain ∷ Text
brightnessDomain = "an integer from " <> tshow brightnessMin
                     <> " to " <> tshow brightnessMax

tooltipMsDomain ∷ Text
tooltipMsDomain = "an integer from " <> tshow tooltipMsMin
                    <> " to " <> tshow tooltipMsMax

windowModeDomain ∷ Text
windowModeDomain = "one of fullscreen, borderless, windowed"

textureFilterDomain ∷ Text
textureFilterDomain = "one of nearest, linear"

-- * Leaf checks
--
-- Each returns 'Nothing' for a value inside the domain and the
-- rejection otherwise. The rejection renders the checked value; a
-- caller that knows a better rendering of the SOURCE (the YAML loader
-- reporting the number as written, before narrowing) overrides
-- 'vfrValue'.

-- | Narrow a source 'Double' (a YAML number, a Lua number) to the
--   'Float' the config stores. 'GHC.Float.double2Float' preserves NaN
--   and the infinities, which the unoptimised 'realToFrac' path does
--   not — and 'checkUIScale' has to judge the narrowed value, since a
--   finite source can overflow to infinity here.
narrowUIScale ∷ Double → Float
narrowUIScale = double2Float

checkDimension ∷ Text → Int → Maybe VideoFieldRejection
checkDimension field n
    | n > 0     = Nothing
    | otherwise = Just (VideoFieldRejection field (tshow n) dimensionDomain)

-- | Judged AFTER narrowing: hand this the stored 'Float', never the
--   source 'Double'.
checkUIScale ∷ Float → Maybe VideoFieldRejection
checkUIScale s
    | isNaN s ∨ isInfinite s          = reject
    | s < uiScaleMin ∨ s > uiScaleMax = reject
    | otherwise                       = Nothing
  where
    reject = Just (VideoFieldRejection fieldUIScale (tshow s) uiScaleDomain)

-- | 'Nothing' is unlimited and always valid; the boundary's own
--   sentinel (YAML @null@, Lua @0@) has already been mapped to it. The
--   first argument is that sentinel's spelling, for the log line.
checkFrameLimit ∷ Text → Maybe Int → Maybe VideoFieldRejection
checkFrameLimit _ Nothing = Nothing
checkFrameLimit unlimited (Just n)
    | n ≥ frameLimitMin ∧ n ≤ frameLimitMax = Nothing
    | otherwise = Just (VideoFieldRejection fieldFrameLimit (tshow n)
                                            (frameLimitDomain unlimited))

checkMSAA ∷ Int → Maybe VideoFieldRejection
checkMSAA n
    | n `elem` msaaChoices = Nothing
    | otherwise            = Just (VideoFieldRejection fieldMSAA (tshow n) msaaDomain)

checkBrightness ∷ Int → Maybe VideoFieldRejection
checkBrightness n
    | n ≥ brightnessMin ∧ n ≤ brightnessMax = Nothing
    | otherwise = Just (VideoFieldRejection fieldBrightness (tshow n)
                                            brightnessDomain)

-- | Shared by both tooltip timings; the field name says which.
checkTooltipMs ∷ Text → Int → Maybe VideoFieldRejection
checkTooltipMs field n
    | n ≥ tooltipMsMin ∧ n ≤ tooltipMsMax = Nothing
    | otherwise = Just (VideoFieldRejection field (tshow n) tooltipMsDomain)
