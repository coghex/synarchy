{-# LANGUAGE Strict #-}
-- | Shared body for the family of `load<Thing>Yaml` loaders under
--   `Engine.Asset` that each parse a keyed YAML list file: decode,
--   log a warning and return [] on failure, log a debug count and
--   return the accessor's list on success. See #1008.
module Engine.Asset.YamlList
    ( loadYamlList
    , loadYamlListOutcome
    ) where

import UPrelude
import qualified Data.Text as T
import qualified Data.Yaml as Yaml
import Data.Aeson (FromJSON)
import GHC.Stack (HasCallStack)
import Engine.Core.Log (LoggerState, logDebug, logWarn, LogCategory(..))

-- | Decode a keyed YAML list file (`FromJSON f`), extract its list via
--   @toList@, and log the same way every `load<Thing>Yaml` loader in
--   this family always has: a `CatAsset` warning on a parse failure, a
--   `CatAsset` debug count on success — with the decode OUTCOME kept
--   rather than flattened.
--
--   'Nothing' is a parse failure; @Just xs@ is a file that decoded,
--   whose list may legitimately be empty. Those two are the same @[]@
--   through 'loadYamlList', which is precisely why startup could reach
--   the main menu on a broken data file (#2203) — a caller that has to
--   tell them apart calls this instead.
--
--   @parseNoun@ names the failing thing in the warning (e.g. "recipe");
--   @successPhrase@ names what was counted in the debug line (e.g.
--   "recipes", "item definitions") — kept separate since the two
--   messages don't always share a plural.
--
--   The 'HasCallStack' constraint is load-bearing and must stay
--   (#2167). The calls below are the family's ONLY logging calls, so
--   without it the logger's outermost-frame rule
--   ('Engine.Core.Log.extractCallSite', #945) stops here and every
--   loader's entry reports @YamlList@ instead of the domain module that
--   failed — the attribution each loader had before #1008 extracted
--   them. Both shared entry points thread the constraint so the chain
--   runs out to the owning domain loader, which carries no constraint
--   of its own; attribution stops there and never reaches that loader's
--   own callers.
loadYamlListOutcome
    ∷ (HasCallStack, FromJSON f)
    ⇒ LoggerState
    → Text            -- ^ parseNoun
    → Text            -- ^ successPhrase
    → (f → [a])        -- ^ toList
    → FilePath
    → IO (Maybe [a])
loadYamlListOutcome logger parseNoun successPhrase toList path = do
    result ← Yaml.decodeFileEither path
    case result of
        Left err → do
            logWarn logger CatAsset $ "Failed to parse " <> parseNoun
                <> " YAML " <> T.pack path <> ": " <> tshow err
            return Nothing
        Right f → do
            let xs = toList f
            logDebug logger CatAsset $ "Loaded "
                <> tshow (length xs)
                <> " " <> successPhrase <> " from " <> T.pack path
            return (Just xs)

-- | 'loadYamlListOutcome' with the parse failure flattened back into
--   the empty list, which is what every caller that cannot act on the
--   difference has always seen. The all-or-nothing decode rule and the
--   two log lines are unchanged.
loadYamlList
    ∷ (HasCallStack, FromJSON f)
    ⇒ LoggerState
    → Text            -- ^ parseNoun
    → Text            -- ^ successPhrase
    → (f → [a])        -- ^ toList
    → FilePath
    → IO [a]
loadYamlList logger parseNoun successPhrase toList path =
    fromMaybe []
        ⊚ loadYamlListOutcome logger parseNoun successPhrase toList path
