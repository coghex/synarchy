{-# LANGUAGE Strict #-}
-- | Shared body for the family of `load<Thing>Yaml` loaders under
--   `Engine.Asset` that each parse a keyed YAML list file: decode,
--   log a warning and return [] on failure, log a debug count and
--   return the accessor's list on success. See #1008.
module Engine.Asset.YamlList
    ( loadYamlList
    ) where

import UPrelude
import qualified Data.Text as T
import qualified Data.Yaml as Yaml
import Data.Aeson (FromJSON)
import GHC.Stack (HasCallStack)
import Engine.Core.Log (LoggerState, logDebug, logWarn, LogCategory(..))

-- | Decode a keyed YAML list file (`FromJSON f`), extract its list via
--   @toList@, and log the same way every `load<Thing>Yaml` loader in
--   this family always has: a `CatAsset` warning and `[]` on a parse
--   failure, a `CatAsset` debug count on success.
--
--   @parseNoun@ names the failing thing in the warning (e.g. "recipe");
--   @successPhrase@ names what was counted in the debug line (e.g.
--   "recipes", "item definitions") — kept separate since the two
--   messages don't always share a plural.
--
--   The 'HasCallStack' constraint is load-bearing and must stay
--   (#2167). These two calls are the family's ONLY logging calls, so
--   without it the logger's outermost-frame rule
--   ('Engine.Core.Log.extractCallSite', #945) stops here and every
--   loader's entry reports @YamlList@ instead of the domain module that
--   failed — the attribution each loader had before #1008 extracted
--   them. With it the chain runs out to the owning @load\<Thing\>Yaml@,
--   which carries no constraint of its own, so attribution stops there
--   and never reaches that loader's own callers.
loadYamlList
    ∷ (HasCallStack, FromJSON f)
    ⇒ LoggerState
    → Text            -- ^ parseNoun
    → Text            -- ^ successPhrase
    → (f → [a])        -- ^ toList
    → FilePath
    → IO [a]
loadYamlList logger parseNoun successPhrase toList path = do
    result ← Yaml.decodeFileEither path
    case result of
        Left err → do
            logWarn logger CatAsset $ "Failed to parse " <> parseNoun
                <> " YAML " <> T.pack path <> ": " <> tshow err
            return []
        Right f → do
            let xs = toList f
            logDebug logger CatAsset $ "Loaded "
                <> tshow (length xs)
                <> " " <> successPhrase <> " from " <> T.pack path
            return xs
