{-# LANGUAGE Strict #-}
module World.Generate.Config.IO
    ( loadWorldGenConfig
    ) where

import UPrelude
import qualified Data.Text as T
import qualified Data.Yaml as Yaml
import System.Directory (doesFileExist)
import Engine.Core.Log (LoggerState, logWarn, LogCategory(..))
import World.Generate.Config.Normalize (normalizeWorldGenConfig)
import World.Generate.Config.Types

-- | Load world gen config from YAML, falling back to defaults on error.
--
--   An ABSENT file is normal: the compiled-in defaults, silently. A file
--   that EXISTS but fails to decode is not, and the two are
--   distinguishable in the log (#2286). The fallback stays
--   whole-document — a malformed file yields the complete
--   'defaultWorldGenConfig', never a partial merge — but the loss is
--   announced with one 'LevelWarn' \/ 'CatInit' entry naming the file and
--   carrying the decoder's own error, the way the pathing and video
--   loaders beside this one in 'Engine.Core.Init' do. Path inclusion is
--   this loader's own requirement, not a universal convention: the
--   pathing loader omits it.
loadWorldGenConfig ∷ LoggerState → FilePath → IO WorldGenConfig
loadWorldGenConfig logger path = do
    exists ← doesFileExist path
    if not exists
        then return defaultWorldGenConfig
        else do
            result ← Yaml.decodeFileEither path
            case result of
                Right c → return (normalizeWorldGenConfig c)
                Left err → do
                    logWarn logger CatInit $ "Error loading world gen config "
                                           <> T.pack path <> ": " <> tshow err
                    return defaultWorldGenConfig
