module Engine.Graphics.Config where

import UPrelude
import qualified Data.Text as T
import qualified Data.Yaml as Yaml
import Data.Aeson ((.:), (.!=), (.=), FromJSON(..), ToJSON(..), Value(..))
import Engine.Core.Log (LoggerState, logWarn, LogCategory(..), logInfo)

-- | Video configuration settings
data VideoConfig = VideoConfig
    { vcWidth      ∷ Int
    , vcHeight     ∷ Int
    , vcFullscreen ∷ Bool
    , vcVSync      ∷ Bool
    , vcFrameLimit ∷ Maybe Int
    , vcMSAA       ∷ Int
    } deriving (Show, Eq)

-- | Default video configuration fallback
defaultVideoConfig ∷ VideoConfig
defaultVideoConfig = VideoConfig
    { vcWidth      = 800
    , vcHeight     = 600
    , vcFullscreen = False
    , vcVSync      = True
    , vcFrameLimit = Nothing
    , vcMSAA       = 1
    }

-- | Yaml structure for video configuration
data VideoConfigFile = VideoConfigFile
    { vfResolution  ∷ Resolution
    , vfFullscreen  ∷ Bool
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
      VideoConfigFile
        <$> videoObj .: "resolution"
        <*> videoObj .: "fullscreen" .!= False
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
    toJSON (VideoConfigFile res fs vs fl msaa) = Yaml.object
        [ "video" .= Yaml.object
            [ "resolution" .= res
            , "fullscreen" .= fs
            , "vsync" .= vs
            , "frame_limit" .= fl
            , "msaa" .= msaa
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
            , vcFullscreen = vfFullscreen vf
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
          , vfFullscreen = vcFullscreen config
          , vfVSync = vcVSync config
          , vfFrameLimit = vcFrameLimit config
          , vfMSAA = vcMSAA config
          }
    result ← Yaml.encodeFile path videoFile
    logInfo logger CatInit $ "Video config saved to " <> T.pack path
