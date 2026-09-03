{-# LANGUAGE Strict #-}

-- | The @save@ config family (#913): the autosave scheduler's tunables.
--
--   Unlike the video\/keybind families, this one resolves its effective
--   values as an explicit KEY-LEVEL OVERLAY rather than by "whole local
--   file wins" ('Engine.Core.Init.resolveConfigPath') plus hard-coded
--   parser fallbacks. (Notifications joined the overlay side in #1938:
--   an overrides entry that omits a checkbox leaves that checkbox at its
--   registry default instead of replacing the whole triple. Their base
--   layer is @data\/notification_categories.yaml@ rather than a tracked
--   @_default.yaml@, and their writer still emits complete triples.) A
--   sparse
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
--   this engine persists is valid by construction — and writes only the
--   keys that genuinely DIFFER from the tracked template, so the local
--   file stays what its name says: the player's overrides, not a full
--   copy that would pin them against every future template change.
--
--   This family is deliberately absent from
--   'Engine.Core.Init.migrateLegacyConfig': it is new in the #786
--   layout and has no pre-#786 tracked path to migrate from.
module Engine.Save.Config
    ( SaveConfig(..)
    , defaultSaveConfig
    , rotationDepthMin
    , rotationDepthMax
    , clampSaveConfig
    , saveConfigDefaultPath
    , saveConfigLocalPath
    , loadSaveConfig
    , writeSaveConfig
    ) where

import UPrelude
import qualified Data.Text as T
import qualified Data.Yaml as Yaml
import Data.Aeson
    (Object, Value(..), FromJSON(..), ToJSON(..), (.:?), (.=), object)
import Data.Aeson.Types (Pair, Parser, parseMaybe)
import Engine.Core.ConfigWrite (removeConfigFile, writeConfigYaml)
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
      --   @intervalMinutesMin@..@intervalMinutesMax@ inclusive.
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
                    <> tshow err <> "); falling back to the values \
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

-- | Persist the player's chosen values to the gitignored local file —
--   as OVERRIDES ONLY.
--
--   Only the keys that actually differ from what the tracked template
--   resolves to are written, and a configuration that matches the
--   template in every key removes the local file outright. Writing the
--   full block instead would pin a player who merely enabled autosave to
--   today's interval and depth forever: those values were never their
--   choice, but a local file always wins, so a later change to the
--   tracked default could never reach them again.
--
--   Values are clamped into range first, so anything this writes decodes
--   back to exactly what was asked for.
writeSaveConfig
    ∷ LoggerState → FilePath → FilePath → SaveConfig → IO (Either Text ())
writeSaveConfig logger defaultPath localPath desired0 = do
    -- The baseline a sparse file inherits from: constants overlaid with
    -- the tracked template, and deliberately NOT the current local file
    -- (whose whole content is what is being replaced here).
    baseline ← applyPatch defaultSaveConfig ⊚ readPatch logger defaultPath
    let desired = clampSaveConfig desired0
        overrides = catMaybes
            [ overrideOf "enabled" (scEnabled baseline)
                (scEnabled desired)
            , overrideOf "interval_minutes" (scIntervalMinutes baseline)
                (scIntervalMinutes desired)
            , overrideOf "rotation_depth" (scRotationDepth baseline)
                (scRotationDepth desired)
            ]
    if null overrides
      then clearLocalFile logger localPath
      else writeOverrides logger localPath overrides
  where
    overrideOf ∷ ToJSON α ⇒ Text → α → α → Maybe Pair
    overrideOf key base wanted
        | encode base ≡ encode wanted = Nothing
        | otherwise = Just (fromString (T.unpack key) .= wanted)
      where encode = Yaml.encode ∘ toJSON

-- | Nothing differs from the tracked template any more, so there is no
--   override left to record. Removing the file (rather than writing an
--   empty @save:@ block) is what lets a future template change reach
--   this player.
--
--   The removal is DURABLE (#2202 review round 1). An unlink is a
--   directory-entry change exactly like the write path's publish
--   rename, so 'removeConfigFile' syncs @config/@ before this reports
--   success: without that, a crash after the reported success could
--   leave the old @config/save.local.yaml@ on disk and restore autosave
--   settings the player had just reset back to the template.
clearLocalFile ∷ LoggerState → FilePath → IO (Either Text ())
clearLocalFile logger path = do
    removed ← removeConfigFile path
    case removed of
        Left msg → do
            logWarn logger CatInit $ "save config: " <> msg
            pure (Left msg)
        -- Nothing was there to remove, so nothing changed and there is
        -- no removal to announce.
        Right False → pure (Right ())
        Right True  → do
            logInfo logger CatInit $
                "Save config matches the tracked defaults; removed "
                <> T.pack path
            pure (Right ())

-- | Publish the sparse override document. The write goes through
--   'writeConfigYaml' (#2202), so a crash part way through can never
--   leave a truncated @config/save.local.yaml@ — which
--   'loadSaveConfig''s per-key overlay would silently resolve past,
--   handing the player back the tracked template's autosave interval
--   without ever saying the file was damaged.
--
--   __Live state on a failed write (#2202).__ Unchanged: this family
--   has no live ref of its own. @engine.setSaveConfig@ pushes the
--   applied values to the Lua-side scheduler independently of this
--   write, so a failed persist leaves the running autosave cycle on the
--   values the player just applied and loses them only at the next boot.
writeOverrides ∷ LoggerState → FilePath → [Pair] → IO (Either Text ())
writeOverrides logger path overrides = do
    written ← writeConfigYaml path (object ["save" .= object overrides])
    case written of
        Left msg → do
            logWarn logger CatInit $ "save config: " <> msg
            pure (Left msg)
        Right () → do
            logInfo logger CatInit $ "Save config saved to " <> T.pack path
            pure (Right ())
