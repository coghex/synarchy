{-# LANGUAGE Strict #-}

-- | The reserved autosave slot family and its rotation (#913).
--
--   Autosaves live in the durable slot family @autosave-\<n\>@, where
--   @autosave-1@ is always the NEWEST generation and higher numbers are
--   progressively older, up to the configured rotation depth. Keeping
--   that ordering true is what this module does: before each autosave
--   the whole family shifts down one place, the generation that falls
--   off the configured depth is dropped, and the fresh save is then
--   published into the now-free @autosave-1@.
--
--   == Manual saves are never overwritten
--
--   @autosave-3@ is a perfectly legal name to type into the manual save
--   box, so the family name alone proves nothing about ownership. Every
--   slot the rotation is about to touch is therefore checked against the
--   durable 'World.Save.Types.smAutosave' classification FIRST, and a
--   single non-autosave in range aborts the whole attempt before any
--   directory is renamed or removed. That is deliberately
--   all-or-nothing: falling through to a different slot would scatter
--   generations, and rotating "as far as possible" would leave the
--   family half-shifted with no way to describe what @autosave-2@ now
--   means.
--
--   A slot that exists but cannot be listed at all (corrupt beyond even
--   previous-generation recovery, unreadable, symlinked) is refused for
--   the same reason: this module can not show it is an autosave, and
--   "unverifiable" must fail towards keeping the player's bytes.
--
--   A save NAME can also be occupied by a pre-#762 LEGACY FLAT FILE
--   (@saves\/autosave-3.synworld@), which 'World.Save.Serialize.listSaves'
--   lists and 'World.Save.Serialize.loadWorld' loads under exactly that
--   slot name. Autosaves are only ever published as slot DIRECTORIES, so
--   a flat file at one of these names is never ours; worse, publishing a
--   directory beside it would SHADOW it outright, since @loadWorld@
--   prefers the directory and the flat save would become unreachable by
--   its own name while still sitting on disk. Any legacy flat file in
--   range is therefore a collision too — the rotation refuses rather
--   than quietly making a player's save unloadable.
--
--   == What rotation deliberately leaves alone
--
--   Only indices @1..depth@ are ever touched. Generations ABOVE the
--   configured depth — the ones a player leaves behind by reducing
--   @rotation_depth@, or by disabling autosave entirely — are retained
--   untouched forever: never selected, never renamed into, never
--   deleted. Deleting them is explicitly out of scope for #913 and would
--   be a data-loss surprise, not a cleanup.
--
--   The per-slot @world.synworld.prev@ recovery generation
--   ("World.Save.Storage") is orthogonal to all of this and is carried
--   along by the directory renames exactly as-is.
--
--   == The slot name a rotated generation remembers
--
--   A slot's IDENTITY is its directory name — that is what
--   'World.Save.Serialize.listSaves' reports, what @engine.loadSave@
--   addresses, and what 'World.Save.Storage.publishGeneration' writes
--   into. A rotated generation's own embedded
--   'World.Save.Types.smName' still records the name it was WRITTEN
--   under (always @autosave-1@, since that is the only slot autosaves
--   are ever published to), so after a shift it no longer echoes its
--   directory. Nothing resolves a save through that field — rewriting it
--   would mean re-encoding and re-publishing every generation on every
--   rotation, trading a cosmetic mismatch for real I/O and a fresh
--   corruption window on bytes that are already durable.
module World.Save.Autosave
    ( autosaveSlotPrefix
    , autosaveSlotName
    , rotateAutosaveSlots
    ) where

import UPrelude
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import qualified Data.Text as T
import Control.Exception (IOException, try)
import System.Directory
    ( doesDirectoryExist, doesFileExist, removeDirectoryRecursive
    , renameDirectory )
import System.FilePath ((</>))
import Engine.Core.Log (LoggerState, LogCategory(..), logInfo)
import Engine.Save.Config (rotationDepthMin, rotationDepthMax)
import World.Save.Serialize
    (listSaves, savesDirectory, saveExtension, SaveListing(..))
import World.Save.Types (SaveMetadata(..))
import qualified World.Save.Storage as Storage

-- | The reserved name prefix. Not a namespace the save-name validator
--   enforces — 'World.Save.Serialize.sanitizeSaveName' still accepts it
--   from a player, which is precisely why ownership is decided by the
--   metadata classification instead.
autosaveSlotPrefix ∷ Text
autosaveSlotPrefix = "autosave-"

-- | The slot name for generation @n@ (1 = newest).
autosaveSlotName ∷ Int → Text
autosaveSlotName n = autosaveSlotPrefix <> T.pack (show n)

-- | One slot's pre-rotation facts.
data SlotState = SlotState
    { ssIndex     ∷ !Int
    , ssName      ∷ !Text
    , ssDir       ∷ !FilePath
    , ssDirExists ∷ !Bool
      -- ^ The modern slot-directory form — the only shape rotation can
      --   actually rename or remove.
    , ssLegacyFileExists ∷ !Bool
      -- ^ The pre-#762 flat-file form occupying the SAME slot name.
    , ssClassified ∷ !(Maybe Bool)
      -- ^ 'Just' its durable autosave flag when the slot listed at all;
      --   'Nothing' when it exists on disk but could not be listed.
    }

-- | Shift @autosave-1..depth@ down one place, dropping whatever fell off
--   the end, so the caller can publish a fresh generation into
--   @autosave-1@. Returns 'Left' with a player-facing reason — which the
--   caller reports as an autosave FAILURE through @save_load@ — without
--   having touched anything, unless the failure was an I\/O error partway
--   through the renames themselves.
rotateAutosaveSlots
    ∷ LoggerState → HS.HashSet Text → Int → IO (Either Text ())
rotateAutosaveSlots logger luaKnownNames requestedDepth = do
    let depth = max rotationDepthMin (min rotationDepthMax requestedDepth)
    listings ← listSaves logger luaKnownNames
    let classified = HM.fromList
            [ (slName l, smAutosave (slMetadata l)) | l ← listings ]
    slots ← forM [1 .. depth] $ \i → do
        let name = autosaveSlotName i
            dir  = savesDirectory </> T.unpack name
            flat = savesDirectory </> T.unpack name <> saveExtension
        dirExists  ← doesDirectoryExist dir
        flatExists ← doesFileExist flat
        pure SlotState { ssIndex      = i
                       , ssName       = name
                       , ssDir        = dir
                       , ssDirExists  = dirExists
                       , ssLegacyFileExists = flatExists
                       , ssClassified = HM.lookup name classified
                       }
    -- Check EVERY slot in range before mutating any of them: a manual
    -- save at autosave-3 must stop the shift that would have overwritten
    -- it, not be discovered after autosave-1 has already moved.
    case catMaybes (map ownershipProblem slots) of
        (reason : _) → pure (Left reason)
        [] → do
            linkSafety ← mapM slotLinkSafety (filter ssDirExists slots)
            case [ r | Left r ← linkSafety ] of
                (reason : _) → pure (Left reason)
                [] → performRotation logger depth slots

-- | Why this slot blocks the rotation, if it does.
ownershipProblem ∷ SlotState → Maybe Text
ownershipProblem s
    | ssLegacyFileExists s = Just $
        "autosave rotation refused: a legacy flat save file '"
        <> ssName s <> T.pack saveExtension
        <> "' already occupies that save name -- publishing an autosave \
           \there would shadow it (a slot directory is loaded in \
           \preference to a flat file, so it could no longer be loaded \
           \by name). Rename or remove it to let autosave use that name."
    | not (ssDirExists s) = Nothing
    | otherwise = case ssClassified s of
        Just True  → Nothing
        Just False → Just $
            "autosave rotation refused: save slot '" <> ssName s
            <> "' is a MANUAL save, not an autosave -- rotating would \
               \overwrite it. Rename or delete it to let autosave use \
               \that name again."
        Nothing → Just $
            "autosave rotation refused: save slot '" <> ssName s
            <> "' exists but could not be read, so it cannot be shown to \
               \be an autosave -- refusing to rotate over it."

-- | The same containment rule every other save path applies: never
--   write through a symlinked slot directory.
slotLinkSafety ∷ SlotState → IO (Either Text ())
slotLinkSafety s = do
    safety ← Storage.rejectSymlinkedSlotDir (ssDir s)
    pure $ case safety of
        Left reason → Left ("autosave rotation refused: save slot '"
                            <> ssName s <> "': " <> reason)
        Right ()    → Right ()

-- | Drop the oldest owned generation, then shift the rest down. Runs
--   only after every check above has passed.
performRotation
    ∷ LoggerState → Int → [SlotState] → IO (Either Text ())
performRotation logger depth slots = do
    let byIndex = HM.fromList [ (ssIndex s, s) | s ← slots ]
        slotAt i = HM.lookup i byIndex
    result ← try $ do
        -- The generation that has aged out of the configured depth.
        forM_ (slotAt depth) $ \oldest →
            when (ssDirExists oldest) $ do
                logInfo logger CatWorld $
                    "Autosave rotation: discarding oldest generation '"
                    <> ssName oldest <> "'"
                removeDirectoryRecursive (ssDir oldest)
        -- Descending, so each destination is free before it is used.
        forM_ [depth - 1, depth - 2 .. 1] $ \i →
            case (slotAt i, slotAt (i + 1)) of
                (Just from, Just to) | ssDirExists from →
                    renameDirectory (ssDir from) (ssDir to)
                _ → pure ()
    pure $ case result ∷ Either IOException () of
        Left e  → Left ("autosave rotation failed: " <> T.pack (show e))
        Right () → Right ()
