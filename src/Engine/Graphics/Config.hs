module Engine.Graphics.Config where

import UPrelude
import qualified Data.Text as T
import qualified Data.Yaml as Yaml
import Data.Aeson ((.:), (.!=), FromJSON(..), Value(..))
import Engine.Core.Log (LoggerState, logWarn, LogCategory(..))

-- | Video configuration settings
data VideoConfig = VideoConfig
    { vcWidth      ∷ Int
    , vcHeight     ∷ Int
    , vcFullscreen ∷ Bool
    , vcVSync      ∷ Bool
    , vcMSAA       ∷ Int
    } deriving (Show, Eq)

-- | Default video configuration fallback
defaultVideoConfig ∷ VideoConfig
defaultVideoConfig = VideoConfig
    { vcWidth      = 800
    , vcHeight     = 600
    , vcFullscreen = False
    , vcVSync      = True
    , vcMSAA       = 1
    }

-- | Yaml structure for video configuration
data VideoConfigFile = VideoConfigFile
    { vfResolution  ∷ Resolution
    , vfFullscreen  ∷ Bool
    , vfVSync       ∷ Bool
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
        <*> videoObj .: "msaa" .!= 1
    parseJSON _ = fail "Expected an object for VideoConfigFile"

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
            , vcMSAA       = vfMSAA vf
            }
