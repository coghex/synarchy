{-# LANGUAGE Strict #-}
module World.Generate.Config.IO
    ( loadWorldGenConfig
    ) where

import UPrelude
import qualified Data.Text as T
import qualified Data.Yaml as Yaml
import System.Directory (doesFileExist)
import Engine.Core.Log (LoggerState, logWarn, LogCategory(..))
import World.Generate.Config.Domain (describeWorldGenRejection)
import World.Generate.Config.Normalize (normalizeWorldGenConfig)
import World.Generate.Config.Types
import World.Generate.Config.Validate (resolveWorldGenConfigRaw)

-- | Load world gen config from YAML.
--
--   Two failure shapes, deliberately distinct (#2288, matching the
--   video loader's #2198 pattern): a document that does not decode
--   structurally (syntax error, wrong type, a leaf that is neither a
--   number nor a recognised non-finite spelling) falls back to
--   'defaultWorldGenConfig' whole, as it always did; a document that
--   decodes but carries an out-of-domain floating-point leaf keeps
--   every other leaf and defaults only that one, with a warning naming
--   the file, the full field and the value as the file spelled it.
loadWorldGenConfig ∷ LoggerState → FilePath → IO WorldGenConfig
loadWorldGenConfig logger path = do
    exists ← doesFileExist path
    if not exists
        then return defaultWorldGenConfig
        else do
            result ← Yaml.decodeFileEither path
            case result of
                Left err → do
                    logWarn logger CatInit $
                        "Error loading world gen config: " <> tshow err
                    return defaultWorldGenConfig
                Right raw → do
                    let (cfg, rejections) = resolveWorldGenConfigRaw raw
                    forM_ rejections $ \(r, dflt) →
                        logWarn logger CatInit $
                            "World gen config " <> T.pack path <> ": "
                              <> describeWorldGenRejection r
                              <> "; using the default " <> dflt
                    return (normalizeWorldGenConfig cfg)
