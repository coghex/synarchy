{-# LANGUAGE Strict, UnicodeSyntax #-}
module World.Generate.Config.IO
    ( loadWorldGenConfig
    ) where

import UPrelude
import qualified Data.Yaml as Yaml
import System.Directory (doesFileExist)
import World.Generate.Config.Normalize (normalizeWorldGenConfig)
import World.Generate.Config.Types

-- | Load world gen config from YAML, falling back to defaults on error
loadWorldGenConfig ∷ FilePath → IO WorldGenConfig
loadWorldGenConfig path = do
    exists ← doesFileExist path
    if not exists
        then return defaultWorldGenConfig
        else do
            result ← Yaml.decodeFileEither path
            case result of
                Left _  → return defaultWorldGenConfig
                Right c → return (normalizeWorldGenConfig c)
