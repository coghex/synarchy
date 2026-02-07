module Engine.Graphics.Config
  ( VideoConfig(..)
  , VideoConfigFile(..)
  , Resolution(..)
  , WindowMode(..)
  , defaultVideoConfig
  , loadVideoConfig
  , saveVideoConfig
  , windowModeToText
  , windowModeFromText
  ) where

import UPrelude
import qualified Data.Text as T
import qualified Data.Yaml as Yaml
import Data.Aeson ((.:), (.!=), (.=), (.:?), FromJSON(..), ToJSON(..)
                   , Value(..), withText)
import Engine.Core.Log (LoggerState, logWarn, LogCategory(..), logInfo)

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

-- | Video configuration settings
data VideoConfig = VideoConfig
    { vcWidth      ∷ Int
    , vcHeight     ∷ Int
    , vcWindowMode ∷ WindowMode
    , vcUIScale    ∷ Float
    , vcVSync      ∷ Bool
    , vcFrameLimit ∷ Maybe Int
    , vcMSAA       ∷ Int
    } deriving (Show, Eq)

-- | Default video configuration fallback
defaultVideoConfig ∷ VideoConfig
defaultVideoConfig = VideoConfig
    { vcWidth      = 800
    , vcHeight     = 600
    , vcWindowMode = Windowed
    , vcUIScale    = 1.0
    , vcVSync      = True
    , vcFrameLimit = Nothing
    , vcMSAA       = 1
    }

-- | Yaml structure for video configuration
data VideoConfigFile = VideoConfigFile
    { vfResolution  ∷ Resolution
    , vfWindowMode  ∷ WindowMode
    , vfUIScale     ∷ Float
    , vfVSync       ∷ Bool
    , vfFrameLimit  ∷ Maybe Int
    , vfMSAA        ∷ Int
    } deriving (Show, Eq)

data Resolution = Resolution
    { resWidth  ∷ Int
    , resHeight ∷ Int
    } deriving (Show, Eq)

instance FromJSON Resolution where
    parseJSON (Object v) = Resolution
        <$> v .: "width"
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
              fs ← videoObj .: "fullscreen" .!= False
              pure $ if fs then Fullscreen else Windowed
      VideoConfigFile
        <$> videoObj .: "resolution"
        <*> pure windowMode
        <*> videoObj .: "ui_scale" .!= 1.0
        <*> videoObj .: "vsync" .!= True
        <*> videoObj .: "frame_limit" .!= Nothing
        <*> videoObj .: "msaa" .!= 1
    parseJSON _ = fail "Expected an object for VideoConfigFile"

instance ToJSON Resolution where
    toJSON (Resolution w h) = Yaml.object
        [ "width"  .= w
        , "height" .= h
        ]

instance ToJSON VideoConfigFile where
    toJSON (VideoConfigFile res wm uis vs fl msaa) = Yaml.object
        [ "video" .= Yaml.object
            [ "resolution"  .= res
            , "window_mode" .= wm
            , "ui_scale"    .= uis
            , "vsync"       .= vs
            , "frame_limit" .= fl
            , "msaa"        .= msaa
            ]
        ]

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
          }
    Yaml.encodeFile path videoFile
    logInfo logger CatInit $ "Video config saved to " <> T.pack path
