module Engine.Graphics.Config
  ( VideoConfig(..)
  , VideoConfigFile(..)
  , Resolution(..)
  , WindowMode(..)
  , TextureFilter(..)
  , defaultVideoConfig
  , loadVideoConfig
  , saveVideoConfig
  , validateVideoConfig
  , VideoConfigRaw(..)
  , UIScaleSource(..)
  , resolveVideoConfigRaw
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
import Data.Aeson.Types (typeMismatch)
import Engine.Core.Log (LoggerState, logWarn, LogCategory(..), logInfo)
import Engine.Graphics.Config.Domain
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
    candidates = dropWhile (≢ requested)
        [ SAMPLE_COUNT_8_BIT
        , SAMPLE_COUNT_4_BIT
        , SAMPLE_COUNT_2_BIT
        , SAMPLE_COUNT_1_BIT
        ]
    isSupported sc = (sc ⌃ supported) ≢ zeroBits

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

-- | One video-config document decoded STRUCTURALLY (#2198): every leaf
--   in its on-disk type, the two tokens still unresolved text, and the
--   legacy @fullscreen@ Boolean kept beside a possibly-absent
--   @window_mode@. A wrong type, a missing @resolution@ or a syntax
--   error fails THIS decode, and that is the whole-file failure
--   'loadVideoConfig' answers with 'defaultVideoConfig'. Two consumers:
--   'loadVideoConfig' applies the domain to it leaf by leaf, and the
--   'VideoConfigFile' decoder demands every token resolve — the
--   strictness 'Engine.Core.Init.migrateLegacyConfig' relies on.
data VideoConfigRaw = VideoConfigRaw
    { vrWidth             ∷ Int
    , vrHeight            ∷ Int
    , vrWindowMode        ∷ Maybe Text
    , vrFullscreen        ∷ Bool
    , vrUIScale           ∷ UIScaleSource
    , vrVSync             ∷ Bool
    , vrFrameLimit        ∷ Maybe Int
    , vrMSAA              ∷ Int
    , vrBrightness        ∷ Int
    , vrPixelSnap         ∷ Bool
    , vrTextureFilter     ∷ Text
    , vrTooltipDwellMs    ∷ Int
    , vrTooltipHintDelayMs ∷ Int
    } deriving (Show, Eq)

-- | The @ui_scale@ leaf as written: the number it names, and its
--   spelling for the log line. A finite number that overflows while
--   narrowing to the stored 'Float' is rejected by 'checkUIScale' on
--   the narrowed value and reported with THIS spelling.
--
--   YAML's own non-finite spellings (@.inf@, @-.inf@, @.nan@, in any
--   case) reach this decoder as STRINGS — the yaml library only ever
--   builds a 'Number' from a finite literal — and so do the @+inf@ /
--   @-inf@ forms aeson's own 'Double' instance accepts. Every one of
--   them is a NUMBER the domain has to reject field-locally, not a wrong
--   type, so it decodes here to the non-finite 'Double' it names; any
--   other string is a wrong type and fails the structural decode, which
--   keeps the whole-file fallback for it.
data UIScaleSource = UIScaleSource
    { usValue ∷ Double
    , usText  ∷ Text
    } deriving (Show, Eq)

instance FromJSON UIScaleSource where
    parseJSON v@(Number _) = do
        d ← parseJSON v
        pure (UIScaleSource d (tshow d))
    parseJSON (String t) = case nonFiniteSpelling t of
        Just d  → pure (UIScaleSource d t)
        Nothing → fail ("ui_scale: expected a number, got the string "
                          <> show t)
    parseJSON v = typeMismatch "ui_scale number" v

-- | The non-finite number a scalar spells, if it spells one: YAML 1.1/1.2
--   core-schema (@.inf@, @+.inf@, @-.inf@, @.nan@), aeson's (@+inf@,
--   @-inf@) and Haskell's own 'show' forms, case-insensitively.
nonFiniteSpelling ∷ Text → Maybe Double
nonFiniteSpelling t
    | s `elem` [".inf", "+.inf", "inf", "+inf", "infinity", "+infinity"] = Just (1 / 0)
    | s `elem` ["-.inf", "-inf", "-infinity"]                            = Just (-1 / 0)
    | s `elem` [".nan", "nan"]                                           = Just (0 / 0)
    | otherwise                                                          = Nothing
  where
    s = T.toLower (T.strip t)

instance FromJSON VideoConfigRaw where
    parseJSON (Object v) = do
      videoObj ← v .: "video"
      res ← videoObj .: "resolution"
      -- '.:? key .!= def' is the correct idiom for *optional* fields
      -- with a fallback: '.:' would fail the entire parse when a key
      -- is missing (and the .!= would never get a chance to run),
      -- which crashes loadVideoConfig back to 'defaultVideoConfig' —
      -- silently resetting resolution / ui_scale on the very first
      -- launch after any new field is added. Use '.:?' uniformly so
      -- adding fields later doesn't invalidate older saved files.
      VideoConfigRaw (resWidth res) (resHeight res)
        ⊚ videoObj .:? "window_mode"
        <*> videoObj .:? "fullscreen" .!= False
        <*> videoObj .:? "ui_scale" .!= UIScaleSource 1.0 "1.0"
        <*> videoObj .:? "vsync" .!= True
        <*> videoObj .:? "frame_limit" .!= Nothing
        <*> videoObj .:? "msaa" .!= 1
        <*> videoObj .:? "brightness" .!= 100
        <*> videoObj .:? "pixel_snap" .!= False
        <*> videoObj .:? "texture_filter" .!= "nearest"
        <*> videoObj .:? "tooltip_dwell_ms" .!= 400
        <*> videoObj .:? "tooltip_hint_delay_ms" .!= 400
    parseJSON _ = fail "Expected an object for VideoConfigFile"

-- | The window mode a document resolves to when @window_mode@ is
--   ABSENT: the legacy @fullscreen@ Boolean, itself optional (#433).
--   A PRESENT @window_mode@ always outranks the legacy key, even when
--   it fails to resolve — that case never consults @fullscreen@.
legacyWindowMode ∷ VideoConfigRaw → WindowMode
legacyWindowMode raw = if vrFullscreen raw then Fullscreen else Windowed

-- | The typed document. Structurally the same decode as 'VideoConfigRaw';
--   on top of it every token must resolve, exactly as before #2198, so a
--   document carrying an unknown @window_mode@ or @texture_filter@ still
--   fails this parse. 'Engine.Core.Init.migrateLegacyConfig' decodes
--   this type as its schema-completeness gate, so that strictness is
--   what keeps such a legacy file non-migratable; the per-leaf leniency
--   lives in 'loadVideoConfig' alone.
instance FromJSON VideoConfigFile where
    parseJSON v = do
      raw ← parseJSON v
      windowMode ← case vrWindowMode raw of
          Nothing → pure (legacyWindowMode raw)
          Just t  → case windowModeFromText t of
              Just wm → pure wm
              Nothing → fail $ "Unknown window mode: " <> T.unpack t
      textureFilter ← case textureFilterFromText (vrTextureFilter raw) of
          Just tf → pure tf
          Nothing → fail $ "Unknown texture filter: "
                             <> T.unpack (vrTextureFilter raw)
      pure VideoConfigFile
        { vfResolution    = Resolution (vrWidth raw) (vrHeight raw)
        , vfWindowMode    = windowMode
        , vfUIScale       = narrowUIScale (usValue (vrUIScale raw))
        , vfVSync         = vrVSync raw
        , vfFrameLimit    = vrFrameLimit raw
        , vfMSAA          = vrMSAA raw
        , vfBrightness    = vrBrightness raw
        , vfPixelSnap     = vrPixelSnap raw
        , vfTextureFilter = textureFilter
        , vfTooltipDwellMs = vrTooltipDwellMs raw
        , vfTooltipHintDelayMs = vrTooltipHintDelayMs raw
        }

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

-- | Apply the domain to a structurally valid document, leaf by leaf
--   (#2198): every out-of-domain leaf takes its value from
--   'defaultVideoConfig' and is reported, every other leaf survives
--   unchanged. Width and height are independent leaves. A present but
--   unknown @window_mode@ takes the DEFAULT mode, never the legacy
--   @fullscreen@ key, which only an absent @window_mode@ consults.
--   Each rejection is paired with the rendering of the default that
--   replaced it, for the loader's log line. Pure, so a spec can pin it
--   without a logger.
resolveVideoConfigRaw ∷ VideoConfigRaw → (VideoConfig, [(VideoFieldRejection, Text)])
resolveVideoConfigRaw raw = (config, catMaybes rejections)
  where
    d = defaultVideoConfig
    config = VideoConfig
        { vcWidth         = width
        , vcHeight        = height
        , vcWindowMode    = windowMode
        , vcUIScale       = uiScale
        , vcVSync         = vrVSync raw
        , vcFrameLimit    = frameLimit
        , vcMSAA          = msaa
        , vcBrightness    = brightness
        , vcPixelSnap     = vrPixelSnap raw
        , vcTextureFilter = textureFilter
        , vcTooltipDwellMs = dwell
        , vcTooltipHintDelayMs = hintDelay
        }
    rejections = [rW, rH, rWM, rS, rFL, rM, rB, rTF, rD, rHD]
    (width, rW)  = leaf (checkDimension fieldWidth (vrWidth raw))
                        (vrWidth raw) (vcWidth d) (tshow (vcWidth d))
    (height, rH) = leaf (checkDimension fieldHeight (vrHeight raw))
                        (vrHeight raw) (vcHeight d) (tshow (vcHeight d))
    (windowMode, rWM) = case vrWindowMode raw of
        Nothing → (legacyWindowMode raw, Nothing)
        Just t  → case windowModeFromText t of
            Just wm → (wm, Nothing)
            Nothing → ( vcWindowMode d
                      , Just ( VideoFieldRejection fieldWindowMode t
                                                   windowModeDomain
                             , windowModeToText (vcWindowMode d) ) )
    narrowed = narrowUIScale (usValue (vrUIScale raw))
    (uiScale, rS) = leaf (asSource ⊚ checkUIScale narrowed)
                         narrowed (vcUIScale d) (tshow (vcUIScale d))
    -- Report the number as the file spelled it, not the infinity it
    -- narrowed to.
    asSource r = r { vfrValue = usText (vrUIScale raw) }
    (frameLimit, rFL) = leaf (checkFrameLimit "null" (vrFrameLimit raw))
                             (vrFrameLimit raw) (vcFrameLimit d)
                             (maybe "null" tshow (vcFrameLimit d))
    (msaa, rM) = leaf (checkMSAA (vrMSAA raw))
                      (vrMSAA raw) (vcMSAA d) (tshow (vcMSAA d))
    (brightness, rB) = leaf (checkBrightness (vrBrightness raw))
                            (vrBrightness raw) (vcBrightness d)
                            (tshow (vcBrightness d))
    (textureFilter, rTF) = case textureFilterFromText (vrTextureFilter raw) of
        Just tf → (tf, Nothing)
        Nothing → ( vcTextureFilter d
                  , Just ( VideoFieldRejection fieldTextureFilter
                                               (vrTextureFilter raw)
                                               textureFilterDomain
                         , textureFilterToText (vcTextureFilter d) ) )
    (dwell, rD) = leaf (checkTooltipMs fieldTooltipDwellMs (vrTooltipDwellMs raw))
                       (vrTooltipDwellMs raw) (vcTooltipDwellMs d)
                       (tshow (vcTooltipDwellMs d))
    (hintDelay, rHD) = leaf (checkTooltipMs fieldTooltipHintDelayMs
                                            (vrTooltipHintDelayMs raw))
                            (vrTooltipHintDelayMs raw) (vcTooltipHintDelayMs d)
                            (tshow (vcTooltipHintDelayMs d))
    -- A leaf keeps its value when the check passes and takes the
    -- default (reported alongside its rendering) when it does not.
    leaf ∷ Maybe VideoFieldRejection → α → α → Text
         → (α, Maybe (VideoFieldRejection, Text))
    leaf Nothing  value _    _         = (value, Nothing)
    leaf (Just r) _     dflt dfltText  = (dflt, Just (r, dfltText))

-- | Load video configuration from a YAML file.
--
--   Two failure shapes, deliberately distinct (#2198): a document that
--   does not decode structurally (syntax error, wrong type, missing
--   @resolution@) falls back to 'defaultVideoConfig' whole, as it
--   always did; a document that decodes but carries an out-of-domain
--   leaf keeps every other leaf and defaults only that one, with a
--   warning naming the file, the full field and the rejected value.
loadVideoConfig ∷ LoggerState → FilePath → IO VideoConfig
loadVideoConfig logger path = do
    result ← Yaml.decodeFileEither path
    case result of
        Left err → do
            logWarn logger CatInit $ "Error loading video config: "
                                   <> tshow err
            return defaultVideoConfig
        Right raw → do
            let (config, rejections) = resolveVideoConfigRaw raw
            forM_ rejections $ \(r, dflt) →
                logWarn logger CatInit $
                    "Video config " <> T.pack path <> ": "
                      <> describeRejection r
                      <> "; using the default " <> dflt
            return config

-- | Every field of an in-memory config that is outside the domain.
--   Empty for a valid config. The enumerated leaves (window mode,
--   texture filter) and the Booleans cannot be out of domain.
validateVideoConfig ∷ VideoConfig → [VideoFieldRejection]
validateVideoConfig c = catMaybes
    [ checkDimension fieldWidth (vcWidth c)
    , checkDimension fieldHeight (vcHeight c)
    , checkUIScale (vcUIScale c)
    , checkFrameLimit "unlimited" (vcFrameLimit c)
    , checkMSAA (vcMSAA c)
    , checkBrightness (vcBrightness c)
    , checkTooltipMs fieldTooltipDwellMs (vcTooltipDwellMs c)
    , checkTooltipMs fieldTooltipHintDelayMs (vcTooltipHintDelayMs c)
    ]

-- | Save video configuration to a YAML file. 'True' when the file was
--   written.
--
--   The whole config is validated first (#2198): if any field is out of
--   the domain the write is refused entirely — an existing destination
--   is left byte-for-byte as it was, an absent one stays absent — and
--   every rejected field is logged. Nothing is silently replaced by a
--   default. This holds independently of what the setters admit,
--   because 'VideoConfig(..)' and this function are both exported.
saveVideoConfig ∷ LoggerState → FilePath → VideoConfig → IO Bool
saveVideoConfig logger path config =
    case validateVideoConfig config of
        [] → do
            Yaml.encodeFile path videoFile
            logInfo logger CatInit $ "Video config saved to " <> T.pack path
            return True
        rejections → do
            forM_ rejections $ \r →
                logWarn logger CatInit $
                    "Video config not saved to " <> T.pack path <> ": "
                      <> describeRejection r
            return False
  where
    videoFile = VideoConfigFile
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
