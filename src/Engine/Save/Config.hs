{-# LANGUAGE Strict #-}

-- | The @save@ config family (#913): the autosave scheduler's tunables.
--
--   Unlike the video\/keybind\/notification families, this one resolves
--   its effective values as an explicit KEY-LEVEL OVERLAY rather than
--   by "whole local file wins" ('Engine.Core.Init.resolveConfigPath')
--   plus hard-coded parser fallbacks. A sparse
--   @config\/save.local.yaml@ carrying only the one key the player
--   actually changed must keep every OTHER value the tracked
--   @config\/save_default.yaml@ ships — not fall back to a constant
--   buried in this module that a template edit can no longer move.
--
--   The resolution order is therefore:
--
--     1. 'defaultSaveConfig' — last-resort constants, reached only when
--        the tracked template is missing or unreadable;
--     2. every VALID key present in @config\/save_default.yaml@;
--     3. every VALID key present in @config\/save.local.yaml@.
--
--   \"Valid\" is per KEY, not per file: a missing, wrong-typed,
--   fractional, or out-of-range value leaves that ONE key resolving to
--   the layer beneath it (ultimately the constant) and never fails the
--   whole file the way a strict @.:@ decoder would. Writing, by
--   contrast, always clamps into range ('clampSaveConfig'), so a value
--   this engine persists is valid by construction.
--
--   This family is deliberately absent from
--   'Engine.Core.Init.migrateLegacyConfig': it is new in the #786
--   layout and has no pre-#786 tracked path to migrate from.
module Engine.Save.Config
    ( SaveConfig(..)
    , defaultSaveConfig
    , intervalMinutesMin
    , intervalMinutesMax
    , rotationDepthMin
    , rotationDepthMax
    , clampSaveConfig
    , validIntervalMinutes
    , validRotationDepth
    , saveConfigDefaultPath
    , saveConfigLocalPath
    , loadSaveConfig
    , writeSaveConfig
    ) where

import UPrelude
import qualified Data.Text as T
import qualified Data.Yaml as Yaml
import Data.Aeson (Object, Value(..), FromJSON(..), (.:?), (.=), object)
import Data.Aeson.Types (Parser, parseMaybe)
import Control.Exception (SomeException, try)
import System.Directory (doesFileExist)
import Engine.Core.Log (LoggerState, LogCategory(..), logInfo, logWarn)

-- | The effective autosave configuration.
data SaveConfig = SaveConfig
    { scEnabled         ∷ !Bool
      -- ^ Whether the interval scheduler runs at all. Ships FALSE (the
      --   issue's shipped default): repeated developer world restarts
      --   must not accumulate saves.
    , scIntervalMinutes ∷ !Int
      -- ^ Whole minutes between eligible autosave attempts,
      --   'intervalMinutesMin'..'intervalMinutesMax' inclusive.
    , scRotationDepth   ∷ !Int
      -- ^ How many @autosave-\<n\>@ generations the rotation keeps,
      --   'rotationDepthMin'..'rotationDepthMax' inclusive.
    } deriving (Show, Eq)

-- | Last-resort constants — the same values the tracked template ships.
--   They exist so a MISSING or unreadable @save_default.yaml@ still
--   yields a usable, safe configuration (autosave off) instead of a
--   boot failure.
defaultSaveConfig ∷ SaveConfig
defaultSaveConfig = SaveConfig
    { scEnabled         = False
    , scIntervalMinutes = 10
    , scRotationDepth   = 3
    }

intervalMinutesMin, intervalMinutesMax ∷ Int
intervalMinutesMin = 1
intervalMinutesMax = 60

rotationDepthMin, rotationDepthMax ∷ Int
rotationDepthMin = 1
rotationDepthMax = 10

validIntervalMinutes ∷ Int → Bool
validIntervalMinutes n = n ≥ intervalMinutesMin ∧ n ≤ intervalMinutesMax

validRotationDepth ∷ Int → Bool
validRotationDepth n = n ≥ rotationDepthMin ∧ n ≤ rotationDepthMax

-- | Force a configuration into range. Used on every WRITE path
--   (@engine.setSaveConfig@), so a persisted local file always decodes
--   back to exactly what was asked for.
clampSaveConfig ∷ SaveConfig → SaveConfig
clampSaveConfig cfg = cfg
    { scIntervalMinutes =
        max intervalMinutesMin (min intervalMinutesMax (scIntervalMinutes cfg))
    , scRotationDepth   =
        max rotationDepthMin (min rotationDepthMax (scRotationDepth cfg))
    }

saveConfigDefaultPath ∷ FilePath
saveConfigDefaultPath = "config/save_default.yaml"

saveConfigLocalPath ∷ FilePath
saveConfigLocalPath = "config/save.local.yaml"

-- | One layer of the overlay: whichever keys a single file actually
--   supplied, in valid form. An absent field means "this file says
--   nothing about that key", which is exactly what lets a sparse local
--   file inherit the tracked template's value.
data SaveConfigPatch = SaveConfigPatch
    { spEnabled         ∷ !(Maybe Bool)
    , spIntervalMinutes ∷ !(Maybe Int)
    , spRotationDepth   ∷ !(Maybe Int)
    } deriving (Show, Eq)

emptyPatch ∷ SaveConfigPatch
emptyPatch = SaveConfigPatch Nothing Nothing Nothing

-- | Read one key leniently: a key that is absent, of the wrong type, or
--   otherwise unconvertible yields 'Nothing' rather than failing the
--   surrounding parse. This is what keeps ONE bad key from resetting
--   every OTHER key in the same file.
lenientField ∷ FromJSON α ⇒ Object → Text → Parser (Maybe α)
lenientField o key = do
    mv ← o .:? fromString (T.unpack key)
    pure (mv ⌦ parseMaybe parseJSON)

-- | Parse a whole config file's @save:@ block into a patch. A file whose
--   top level is not an object, or which carries no @save:@ mapping at
--   all, contributes nothing rather than erroring — the layer beneath it
--   stands.
patchParser ∷ Value → Parser SaveConfigPatch
patchParser (Object top) = do
    mSave ← lenientField top "save"
    case mSave of
        Just (Object saveObj) → SaveConfigPatch
            ⊚ lenientField saveObj "enabled"
            <*> (rangeChecked validIntervalMinutes
                    ⊚ lenientField saveObj "interval_minutes")
            <*> (rangeChecked validRotationDepth
                    ⊚ lenientField saveObj "rotation_depth")
        _ → pure emptyPatch
  where
    -- Out of range is "invalid", not "clamp": an invalid value resolves
    -- to the EFFECTIVE DEFAULT (the layer beneath this one), never to
    -- the nearest bound. Clamping is a WRITE-side policy — see
    -- 'clampSaveConfig'.
    rangeChecked ok mv = mv ⌦ \v → if ok v then Just v else Nothing
patchParser _ = pure emptyPatch

applyPatch ∷ SaveConfig → SaveConfigPatch → SaveConfig
applyPatch cfg p = SaveConfig
    { scEnabled         = fromMaybe (scEnabled cfg)         (spEnabled p)
    , scIntervalMinutes = fromMaybe (scIntervalMinutes cfg) (spIntervalMinutes p)
    , scRotationDepth   = fromMaybe (scRotationDepth cfg)   (spRotationDepth p)
    }

-- | Decode one file into a patch. A file that does not exist contributes
--   nothing and says nothing (the normal case for the gitignored local
--   file on a fresh clone); a file that EXISTS but is malformed
--   contributes nothing and WARNS, so a typo is visible rather than
--   silently reverting a setting.
readPatch ∷ LoggerState → FilePath → IO SaveConfigPatch
readPatch logger path = do
    exists ← doesFileExist path
    if not exists then pure emptyPatch else do
        result ← Yaml.decodeFileEither path
        case result of
            Left err → do
                logWarn logger CatInit $
                    "save config " <> T.pack path <> " could not be read ("
                    <> T.pack (show err) <> "); falling back to the values \
                       \beneath it"
                pure emptyPatch
            Right value → pure $ fromMaybe emptyPatch (parseMaybe patchParser value)

-- | Resolve the effective configuration: constants, then every valid key
--   from the tracked template, then every valid key from the player's
--   local overrides.
loadSaveConfig ∷ LoggerState → FilePath → FilePath → IO SaveConfig
loadSaveConfig logger defaultPath localPath = do
    defPatch   ← readPatch logger defaultPath
    localPatch ← readPatch logger localPath
    pure $ applyPatch (applyPatch defaultSaveConfig defPatch) localPatch

-- | Persist the player's chosen values to the gitignored local file.
--   Always writes a clamped, COMPLETE @save:@ block — the overlay is
--   about tolerating a sparse file, not about producing one.
writeSaveConfig ∷ LoggerState → FilePath → SaveConfig → IO (Either Text ())
writeSaveConfig logger path cfg0 = do
    let cfg = clampSaveConfig cfg0
        doc = object
            [ "save" .= object
                [ "enabled"          .= scEnabled cfg
                , "interval_minutes" .= scIntervalMinutes cfg
                , "rotation_depth"   .= scRotationDepth cfg
                ]
            ]
    written ← try (Yaml.encodeFile path doc)
    case written ∷ Either SomeException () of
        Left e → do
            let msg = "could not write " <> T.pack path <> ": "
                        <> T.pack (show e)
            logWarn logger CatInit $ "save config: " <> msg
            pure (Left msg)
        Right () → do
            logInfo logger CatInit $ "Save config saved to " <> T.pack path
            pure (Right ())
