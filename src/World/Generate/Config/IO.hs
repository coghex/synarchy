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

-- | Load world gen config from YAML. Three outcomes, deliberately
--   distinct, and each says something different in the log.
--
--   An ABSENT file is normal: the compiled-in defaults, silently.
--
--   A file that EXISTS but does not decode STRUCTURALLY is not (#2286).
--   The fallback stays whole-document — a malformed file yields the
--   complete 'defaultWorldGenConfig', never a partial merge — but the
--   loss is announced with one 'LevelWarn' \/ 'CatInit' entry naming the
--   file and carrying the decoder's own error, the way the pathing and
--   video loaders beside this one in 'Engine.Core.Init' do. Path
--   inclusion is this loader's own requirement, not a universal
--   convention: the pathing loader omits it.
--
--   A file that decodes but carries an OUT-OF-DOMAIN floating-point leaf
--   is neither (#2288). That is a field-local fault, so it keeps every
--   other leaf and defaults only the offending one, warning per leaf
--   with the file, the full field, and the value as the FILE spelled it.
--   The two failure shapes stay separate on purpose: one bad number must
--   not cost the file its other authored settings, which is exactly what
--   the whole-document fallback above would do.
--
--   Note that the structural half is judged by 'WorldGenConfigRaw', not
--   'WorldGenConfig': a scalar spelling a non-finite number (@.inf@,
--   @.nan@) decodes there rather than failing the document, so it
--   reaches the domain as the field-local rejection it is.
loadWorldGenConfig ∷ LoggerState → FilePath → IO WorldGenConfig
loadWorldGenConfig logger path = do
    exists ← doesFileExist path
    if not exists
        then return defaultWorldGenConfig
        else do
            result ← Yaml.decodeFileEither path
            case result of
                Left err → do
                    logWarn logger CatInit $ "Error loading world gen config "
                                           <> T.pack path <> ": " <> tshow err
                    return defaultWorldGenConfig
                Right raw → do
                    let (cfg, rejections) = resolveWorldGenConfigRaw raw
                    forM_ rejections $ \(r, dflt) →
                        logWarn logger CatInit $
                            "World gen config " <> T.pack path <> ": "
                              <> describeWorldGenRejection r
                              <> "; using the default " <> dflt
                    return (normalizeWorldGenConfig cfg)
