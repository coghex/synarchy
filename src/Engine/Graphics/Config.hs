module Engine.Graphics.Config
  ( VideoConfig(..)
  , VideoConfigFile(..)
  , Resolution(..)
  , WindowMode(..)
  , TextureFilter(..)
  , defaultVideoConfig
  , loadVideoConfig
  , saveVideoConfig
  , windowModeToText
  , windowModeFromText
  , msaaToSampleCount
  , clampSampleCount
  , brightnessToMultiplier
  , textureFilterToVulkan
  , textureFilterToText
  , textureFilterFromText
  ) where

import UPrelude
import qualified Data.Text as T
import qualified Data.Yaml as Yaml
import Data.Aeson ((.:), (.!=), (.=), (.:?), FromJSON(..), ToJSON(..)
                   , Value(..), withText)
import Engine.Core.Log (LoggerState, logWarn, LogCategory(..), logInfo)
import Vulkan.Core10 (SampleCountFlags, SampleCountFlagBits(..), Filter(..))

-- | Convert user-facing MSAA int (1,2,4,8) to Vulkan sample count
msaaToSampleCount ∷ Int → SampleCountFlagBits
msaaToSampleCount 2 = SAMPLE_COUNT_2_BIT
msaaToSampleCount 4 = SAMPLE_COUNT_4_BIT
msaaToSampleCount 8 = SAMPLE_COUNT_8_BIT
msaaToSampleCount _ = SAMPLE_COUNT_1_BIT

-- | Clamp a requested sample count to the highest supported by the device.
-- 'supported' is the framebufferColorSampleCounts bitmask from PhysicalDeviceLimits.
clampSampleCount ∷ SampleCountFlags → SampleCountFlagBits → SampleCountFlagBits
clampSampleCount supported requested =
    -- Try requested first, then fall back to lower counts
    fromMaybe SAMPLE_COUNT_1_BIT $ listToMaybe $ filter isSupported candidates
  where
    candidates = dropWhile (/= requested)
        [ SAMPLE_COUNT_8_BIT
        , SAMPLE_COUNT_4_BIT
        , SAMPLE_COUNT_2_BIT
        , SAMPLE_COUNT_1_BIT
        ]
    isSupported sc = (sc .&. supported) ≢ zeroBits

-- | Window display mode
data WindowMode
    = Fullscreen         -- ^ Exclusive fullscreen
    | BorderlessWindowed -- ^ Borderless windowed (fake fullscreen)
    | Windowed           -- ^ Normal decorated window
    deriving (Show, Eq, Ord, Enum, Bounded)

windowModeToText ∷ WindowMode → Text
windowModeToText Fullscreen         = "fullscreen"
windowModeToText BorderlessWindowed = "borderless"
windowModeToText Windowed           = "windowed"

windowModeFromText ∷ Text → Maybe WindowMode
windowModeFromText t = case T.toLower t of
    "fullscreen" → Just Fullscreen
    "borderless" → Just BorderlessWindowed
    "windowed"   → Just Windowed
    _            → Nothing

instance FromJSON WindowMode where
    parseJSON = withText "WindowMode" $ \t →
        case windowModeFromText t of
            Just wm → pure wm
            Nothing → fail $ "Unknown window mode: " <> T.unpack t

instance ToJSON WindowMode where
    toJSON = toJSON . windowModeToText

-- | Texture filtering mode
data TextureFilter
    = FilterNearest    -- ^ Pixel-perfect (best for pixel art)
    | FilterLinear     -- ^ Smooth bilinear interpolation
    deriving (Show, Eq, Ord, Enum, Bounded)

textureFilterToText ∷ TextureFilter → Text
textureFilterToText FilterNearest = "nearest"
textureFilterToText FilterLinear  = "linear"

textureFilterFromText ∷ Text → Maybe TextureFilter
textureFilterFromText t = case T.toLower t of
    "nearest" → Just FilterNearest
    "linear"  → Just FilterLinear
    _         → Nothing

instance FromJSON TextureFilter where
    parseJSON = withText "TextureFilter" $ \t →
        case textureFilterFromText t of
            Just tf → pure tf
            Nothing → fail $ "Unknown texture filter: " <> T.unpack t

instance ToJSON TextureFilter where
    toJSON = toJSON . textureFilterToText

-- | Convert to Vulkan filter enum
textureFilterToVulkan ∷ TextureFilter → Filter
textureFilterToVulkan FilterNearest = FILTER_NEAREST
textureFilterToVulkan FilterLinear  = FILTER_LINEAR

-- | Video configuration settings
data VideoConfig = VideoConfig
    { vcWidth         ∷ Int
    , vcHeight        ∷ Int
    , vcWindowMode    ∷ WindowMode
    , vcUIScale       ∷ Float
    , vcVSync         ∷ Bool
    , vcFrameLimit    ∷ Maybe Int
    , vcMSAA          ∷ Int
    , vcBrightness    ∷ Int
    , vcPixelSnap     ∷ Bool
    , vcTextureFilter ∷ TextureFilter
    , vcTooltipDwellMs ∷ Int
      -- ^ Milliseconds the cursor must rest on a tooltip-bearing
      --   element before the tooltip appears. Player-tunable from
      --   the settings menu; persisted with the rest of the video
      --   config so it survives restarts.
    , vcTooltipHintDelayMs ∷ Int
      -- ^ Additional delay (after the dwell) before the rich
      --   tooltip (title + separator + hint) replaces the
      --   title-only form. Cumulative with 'vcTooltipDwellMs'.
    } deriving (Show, Eq)

-- | Default video configuration fallback
defaultVideoConfig ∷ VideoConfig
defaultVideoConfig = VideoConfig
    { vcWidth         = 800
    , vcHeight        = 600
    , vcWindowMode    = Windowed
    , vcUIScale       = 1.0
    , vcVSync         = True
    , vcFrameLimit    = Nothing
    , vcMSAA          = 1
    , vcBrightness    = 100
    , vcPixelSnap     = False
    , vcTextureFilter = FilterNearest
    , vcTooltipDwellMs = 400
    , vcTooltipHintDelayMs = 400
    }

-- | Yaml structure for video configuration
data VideoConfigFile = VideoConfigFile
    { vfResolution    ∷ Resolution
    , vfWindowMode    ∷ WindowMode
    , vfUIScale       ∷ Float
    , vfVSync         ∷ Bool
    , vfFrameLimit    ∷ Maybe Int
    , vfMSAA          ∷ Int
    , vfBrightness    ∷ Int
    , vfPixelSnap     ∷ Bool
    , vfTextureFilter ∷ TextureFilter
    , vfTooltipDwellMs ∷ Int
    , vfTooltipHintDelayMs ∷ Int
    } deriving (Show, Eq)

data Resolution = Resolution
    { resWidth  ∷ Int
    , resHeight ∷ Int
    } deriving (Show, Eq)

instance FromJSON Resolution where
    parseJSON (Object v) = Resolution
        ⊚ v .: "width"
        <*> v .: "height"
    parseJSON _ = fail "Expected an object for Resolution"

instance FromJSON VideoConfigFile where
    parseJSON (Object v) = do
      videoObj ← v .: "video"
      -- Try new "window_mode" field first, fall back to legacy "fullscreen" bool
      mWindowMode ← videoObj .:? "window_mode"
      windowMode ← case mWindowMode of
          Just wm → pure wm
          Nothing → do
              fs ← videoObj .:? "fullscreen" .!= False
              pure $ if fs then Fullscreen else Windowed
      -- '.:? key .!= def' is the correct idiom for *optional* fields
      -- with a fallback: '.:' would fail the entire parse when a key
      -- is missing (and the .!= would never get a chance to run),
      -- which crashes loadVideoConfig back to 'defaultVideoConfig' —
      -- silently resetting resolution / ui_scale on the very first
      -- launch after any new field is added. Use '.:?' uniformly so
      -- adding fields later doesn't invalidate older saved files.
      VideoConfigFile
        ⊚ videoObj .:  "resolution"
        <*> pure windowMode
        <*> videoObj .:? "ui_scale" .!= 1.0
        <*> videoObj .:? "vsync" .!= True
        <*> videoObj .:? "frame_limit" .!= Nothing
        <*> videoObj .:? "msaa" .!= 1
        <*> videoObj .:? "brightness" .!= 100
        <*> videoObj .:? "pixel_snap" .!= False
        <*> videoObj .:? "texture_filter" .!= FilterNearest
        <*> videoObj .:? "tooltip_dwell_ms" .!= 400
        <*> videoObj .:? "tooltip_hint_delay_ms" .!= 400
    parseJSON _ = fail "Expected an object for VideoConfigFile"

instance ToJSON Resolution where
    toJSON (Resolution w h) = Yaml.object
        [ "width"  .= w
        , "height" .= h
        ]

instance ToJSON VideoConfigFile where
    toJSON (VideoConfigFile res wm uis vs fl msaa b ps tf dwell hintDelay) = Yaml.object
        [ "video" .= Yaml.object
            [ "resolution"  .= res
            , "window_mode" .= wm
            , "ui_scale"    .= uis
            , "vsync"       .= vs
            , "frame_limit" .= fl
            , "msaa"        .= msaa
            , "brightness"  .= b
            , "pixel_snap"  .= ps
            , "texture_filter" .= textureFilterToText tf
            , "tooltip_dwell_ms" .= dwell
            , "tooltip_hint_delay_ms" .= hintDelay
            ]
        ]

brightnessToMultiplier ∷ Int → Float
brightnessToMultiplier pct = fromIntegral (max 50 (min 300 pct)) / 100.0

-- | Load video configuration from a YAML file
loadVideoConfig ∷ LoggerState → FilePath → IO VideoConfig
loadVideoConfig logger path = do
    result ← Yaml.decodeFileEither path
    case result of
        Left err → do
            logWarn logger CatInit $ "Error loading video config: "
                                   <> T.pack (show err)
            return defaultVideoConfig
        Right vf → return $ VideoConfig
            { vcWidth      = resWidth (vfResolution vf)
            , vcHeight     = resHeight (vfResolution vf)
            , vcWindowMode = vfWindowMode vf
            , vcUIScale    = vfUIScale vf
            , vcVSync      = vfVSync vf
            , vcFrameLimit = vfFrameLimit vf
            , vcMSAA       = vfMSAA vf
            , vcBrightness  = vfBrightness vf
            , vcPixelSnap   = vfPixelSnap vf
            , vcTextureFilter = vfTextureFilter vf
            , vcTooltipDwellMs = vfTooltipDwellMs vf
            , vcTooltipHintDelayMs = vfTooltipHintDelayMs vf
            }

-- | Save video configuration to a YAML file
saveVideoConfig ∷ LoggerState → FilePath → VideoConfig → IO ()
saveVideoConfig logger path config = do
    let videoFile = VideoConfigFile
          { vfResolution = Resolution
              { resWidth = vcWidth config
              , resHeight = vcHeight config
              }
          , vfWindowMode = vcWindowMode config
          , vfUIScale = vcUIScale config
          , vfVSync = vcVSync config
          , vfFrameLimit = vcFrameLimit config
          , vfMSAA = vcMSAA config
          , vfBrightness = vcBrightness config
          , vfPixelSnap = vcPixelSnap config
          , vfTextureFilter = vcTextureFilter config
          , vfTooltipDwellMs = vcTooltipDwellMs config
          , vfTooltipHintDelayMs = vcTooltipHintDelayMs config
          }
    Yaml.encodeFile path videoFile
    logInfo logger CatInit $ "Video config saved to " <> T.pack path
