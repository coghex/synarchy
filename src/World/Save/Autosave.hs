{-# LANGUAGE Strict #-}

-- | The reserved autosave slot family and its rotation (#913).
--
--   Autosaves live in the durable slot family @autosave-\<n\>@, where
--   @autosave-1@ is always the NEWEST generation and higher numbers are
--   progressively older, up to the configured rotation depth.
--
--   == Publish first, rotate second
--
--   Keeping that ordering true means freeing @autosave-1@, which means
--   shifting the whole family down, which means the generation that
--   falls off the end has to go. Doing that BEFORE the new save is
--   written would make every failed autosave destructive: a save that
--   never published would already have discarded the oldest generation
--   and renumbered the rest, with nothing to roll back to.
--
--   So the cycle is inverted. Each autosave publishes to the reserved
--   staging slot 'autosaveIncomingSlotName' — an ordinary slot, written
--   by the ordinary transaction, with its own atomic publication and
--   recovery generation. Only once that has actually succeeded does
--   'finalizeAutosaveRotation' age the family down and rename the staged
--   one into @autosave-1@. A rejected request, a failed encode, a failed
--   disk write: none of them touch a single existing generation.
--
--   The rotation itself is ordered the same way, for the same reason:
--   the aged-out generation is RETIRED by rename into
--   'autosaveRetiredSlotName' first, and only deleted once every other
--   move has succeeded. An interruption or a failing rename therefore
--   leaves a partially shifted family, never a shorter one — every
--   generation is still on disk, and the next cycle finishes the
--   discard the interrupted one intended.
--
--   A staged generation left behind by a crash between those two steps
--   is not lost either — 'prepareAutosaveCycle' rotates it in before
--   starting a new cycle, so the interrupted save still lands rather
--   than being overwritten.
--
--   == The durability boundary (issue #2229)
--
--   Every move a cycle makes — the retire rename, the shift, the staged
--   generation's rename into @autosave-1@, and the single removal —
--   changes @saves\/@'s OWN entry list, and a rename returning says
--   nothing about that list being on disk
--   ('World.Save.Storage.Durable.syncDirectory'). Left unsynced, a
--   rotation reported as complete could be wholly absent after a crash,
--   which is what the ordering above exists to prevent.
--
--   So @saves\/@ is synced at each point where the shape reported as
--   complete changes: after 'clearRetired' actually removes a leftover
--   retired generation, after 'performRotation' finishes its renames and
--   BEFORE the retired generation is removed, and once more after that
--   removal (only when one actually happened — a family that was not
--   full retires nothing and has no removal entry to persist). A sync
--   failure fails the autosave with a message naming the directory,
--   which reaches the player through
--   'Engine.Scripting.Lua.API.Save.autosaveSlotVerb' like any other
--   refusal.
--
--   That placement preserves the resumable-family guarantee rather than
--   weakening it. A failure of the pre-removal sync leaves the retired
--   generation still on disk — the same partially-shifted family an
--   interruption already produces, which the next cycle finishes. A
--   failure of the post-removal sync leaves an already-durable numbered
--   family and, at worst, a retired directory whose disappearance is
--   still in flight; the next cycle's 'clearRetired' discards it again
--   regardless.
--
--   The seams 'prepareAutosaveCycleWithSync' and
--   'finalizeAutosaveRotationWithSync' exist only so the headless gate
--   can inject a failing sync; production always uses the real one.
--
--   == Manual saves are never overwritten
--
--   @autosave-3@ is a perfectly legal name to type into the manual save
--   box, so the family name alone proves nothing about ownership. Every
--   slot a cycle is about to touch is therefore checked against the
--   durable 'World.Save.Types.smAutosave' classification FIRST, and a
--   single non-autosave in range aborts the whole attempt before
--   anything is written, renamed, or removed. That is deliberately
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
--   range is therefore a collision too — the cycle refuses rather than
--   quietly making a player's save unloadable.
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
--   into. A generation's own embedded 'World.Save.Types.smName' still
--   records the name it was WRITTEN under (always
--   'autosaveIncomingSlotName', the only slot autosaves are ever
--   published to), so after the rename it no longer echoes its
--   directory. Nothing resolves a save through that field — rewriting it
--   would mean re-encoding and re-publishing every generation on every
--   rotation, trading a cosmetic mismatch for real I\/O and a fresh
--   corruption window on bytes that are already durable.
module World.Save.Autosave
    ( autosaveSlotName
    , autosaveIncomingSlotName
    , autosaveRetiredSlotName
    , autosaveStagingSlotNames
    , isAutosaveStagingSlot
    , publicSaveListings
    , prepareAutosaveCycle
    , prepareAutosaveCycleWithSync
    , finalizeAutosaveRotation
    , finalizeAutosaveRotationWithSync
    ) where

import UPrelude
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import qualified Data.Text as T
import Control.Exception (IOException, SomeException, try)
import System.Directory
    ( doesDirectoryExist, doesFileExist, removeDirectoryRecursive
    , renameDirectory )
import System.FilePath ((</>))
import Engine.Core.Log (LoggerState, LogCategory(..), logInfo)
import Engine.Save.Config (rotationDepthMin, rotationDepthMax)
import World.Save.Serialize
    (listSaves, savesDirectory, saveExtension, SaveListing(..))
import World.Save.Types (SaveMetadata(..))
import World.Save.Storage.Durable (syncDirectory)
import qualified World.Save.Storage as Storage

-- | The reserved name prefix. Not a namespace the save-name validator
--   enforces — 'World.Save.Serialize.sanitizeSaveName' still accepts it
--   from a player, which is precisely why ownership is decided by the
--   metadata classification instead.
autosaveSlotPrefix ∷ Text
autosaveSlotPrefix = "autosave-"

-- | The slot name for generation @n@ (1 = newest).
autosaveSlotName ∷ Int → Text
autosaveSlotName n = autosaveSlotPrefix <> tshow n

-- | The staging slot every autosave is PUBLISHED to, before the family
--   is rotated and it becomes @autosave-1@. Inside the reserved prefix
--   (so it reads as one of ours) but not of the @autosave-\<n\>@ shape,
--   so it can never collide with a numbered generation.
autosaveIncomingSlotName ∷ Text
autosaveIncomingSlotName = autosaveSlotPrefix <> "incoming"

-- | Where the generation that has aged out of the configured depth is
--   moved ASIDE to, so that the only destructive step of a rotation is
--   its LAST one. Same namespacing argument as the staging slot above.
autosaveRetiredSlotName ∷ Text
autosaveRetiredSlotName = autosaveSlotPrefix <> "retired"

-- | Both reserved staging names, in no meaningful order. Rotation is the
--   only thing that ever writes them, and a generation only ever rests
--   in one while a cycle is between steps (or was interrupted between
--   them).
autosaveStagingSlotNames ∷ [Text]
autosaveStagingSlotNames =
    [autosaveIncomingSlotName, autosaveRetiredSlotName]

-- | #1413: whether this listed slot is rotation machinery that the
--   PUBLIC save listing must hide.
--
--   A staged generation is an ordinary complete slot — it lists and
--   loads exactly like any other — so nothing stopped it from becoming a
--   save-browser row, and (being the newest) the main menu's Continue
--   target. That is not a narrow in-flight window either: a rotation
--   that refuses deliberately leaves the staged generation in place for
--   the NEXT cycle to rotate in, so a quit or crash in between leaves it
--   public across a restart.
--
--   The predicate is deliberately two-part, and NEITHER half alone is
--   correct:
--
--   * The NAME alone is not enough, because a player may type
--     @autosave-incoming@ into the manual save box —
--     'World.Save.Serialize.sanitizeSaveName' accepts it — and
--     'ownershipProblem' then refuses the cycle with a message asking
--     them to rename or delete that very save. Hiding it by name would
--     conceal exactly the save the player is being told to act on.
--     #913 decided autosave ownership by the durable classification
--     rather than the name for the same reason.
--
--   * The CLASSIFICATION alone is not enough, because the numbered
--     @autosave-\<n\>@ family carries it too and must stay listed.
--
--   The name half reads the slot's own identity ('slName' — the
--   directory under @saves\/@), never the generation's embedded
--   'World.Save.Types.smName'. A rotated generation still remembers
--   being WRITTEN as @autosave-incoming@ (see the module header), so
--   matching on the embedded name would hide the entire numbered family.
isAutosaveStagingSlot ∷ SaveListing → Bool
isAutosaveStagingSlot listing =
    smAutosave (slMetadata listing)
        ∧ (slName listing `elem` autosaveStagingSlotNames)

-- | The listing as a PLAYER may see it. Applied once, at the Lua
--   boundary @engine.listSaves()@ — never inside
--   'World.Save.Serialize.listSaves', which 'readSlotStates' below
--   depends on reporting both staging slots so a cycle can classify
--   them, refuse over them, and rotate one in.
--
--   Order is preserved (so the caller's newest-first sort survives) and
--   nothing but the two staging rows is removed, so re-indexing the
--   survivors yields the same dense sequence a consumer's @#@, @[1]@
--   and @ipairs@ already assume.
publicSaveListings ∷ [SaveListing] → [SaveListing]
publicSaveListings = filter (not . isAutosaveStagingSlot)

-- | One slot's pre-cycle facts.
data SlotState = SlotState
    { ssName      ∷ !Text
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

clampDepth ∷ Int → Int
clampDepth d = max rotationDepthMin (min rotationDepthMax d)

-- | Every slot a cycle at this depth may touch: the numbered family plus
--   the staging slot.
cycleSlotNames ∷ Int → [Text]
cycleSlotNames depth =
    map autosaveSlotName [1 .. depth]
        ⧺ [autosaveIncomingSlotName, autosaveRetiredSlotName]

readSlotStates ∷ LoggerState → HS.HashSet Text → [Text] → IO [SlotState]
readSlotStates logger luaKnownNames names = do
    listings ← listSaves logger luaKnownNames
    let classified = HM.fromList
            [ (slName l, smAutosave (slMetadata l)) | l ← listings ]
    forM names $ \name → do
        let dir  = savesDirectory </> T.unpack name
            flat = savesDirectory </> T.unpack name <> saveExtension
        dirExists  ← doesDirectoryExist dir
        flatExists ← doesFileExist flat
        pure SlotState { ssName       = name
                       , ssDir        = dir
                       , ssDirExists  = dirExists
                       , ssLegacyFileExists = flatExists
                       , ssClassified = HM.lookup name classified
                       }

-- | Check EVERY slot in range before anything mutates: a manual save at
--   @autosave-3@ must stop the cycle that would have overwritten it, not
--   be discovered after @autosave-1@ has already moved.
firstProblem ∷ [SlotState] → Maybe Text
firstProblem slots = listToMaybe (catMaybes (map ownershipProblem slots))

-- | Why this slot blocks the cycle, if it does.
ownershipProblem ∷ SlotState → Maybe Text
ownershipProblem s
    | ssLegacyFileExists s = Just $
        "a legacy flat save file '" <> ssName s <> T.pack saveExtension
        <> "' already occupies that save name -- publishing an autosave \
           \there would shadow it (a slot directory is loaded in \
           \preference to a flat file, so it could no longer be loaded \
           \by name). Rename or remove it to let autosave use that name."
    | not (ssDirExists s) = Nothing
    | otherwise = case ssClassified s of
        Just True  → Nothing
        Just False → Just $
            "save slot '" <> ssName s
            <> "' is a MANUAL save, not an autosave -- rotating would \
               \overwrite it. Rename or delete it to let autosave use \
               \that name again."
        Nothing → Just $
            "save slot '" <> ssName s
            <> "' exists but could not be read, so it cannot be shown to \
               \be an autosave -- refusing to rotate over it."

-- | The same containment rule every other save path applies: never write
--   through a symlinked slot directory.
linkSafetyProblem ∷ [SlotState] → IO (Maybe Text)
linkSafetyProblem slots = do
    results ← forM (filter ssDirExists slots) $ \s → do
        safety ← Storage.rejectSymlinkedSlotDir (ssDir s)
        pure $ case safety of
            Left reason → Just ("save slot '" <> ssName s <> "': " <> reason)
            Right ()    → Nothing
    pure (listToMaybe (catMaybes results))

-- | Verify that a new autosave cycle may proceed, and clear the way for
--   it. Nothing here writes a generation: the CALLER publishes to
--   'autosaveIncomingSlotName' next, and calls
--   'finalizeAutosaveRotation' only once that has succeeded.
--
--   A staged generation already sitting in the incoming slot is a
--   completed publish whose rotation never ran (a crash or kill in
--   between). It is rotated in first, so an interrupted autosave still
--   lands instead of being overwritten by the next one.
prepareAutosaveCycle
    ∷ LoggerState → HS.HashSet Text → Int → IO (Either Text ())
prepareAutosaveCycle = prepareAutosaveCycleWithSync syncDirectory

-- | 'prepareAutosaveCycle' with its directory-sync seam supplied by the
--   caller (issue #2229). Production passes
--   'World.Save.Storage.Durable.syncDirectory'; the headless gate
--   injects a failing sync to prove the sync is reached and its failure
--   propagated, without depending on a real @fsync@ ever failing.
prepareAutosaveCycleWithSync
    ∷ (FilePath → IO ()) → LoggerState → HS.HashSet Text → Int
    → IO (Either Text ())
prepareAutosaveCycleWithSync syncDir logger luaKnownNames requestedDepth = do
    let depth = clampDepth requestedDepth
    slots ← readSlotStates logger luaKnownNames (cycleSlotNames depth)
    problem ← cycleProblem slots
    case problem of
        Just reason → pure (Left ("autosave refused: " <> reason))
        Nothing → do
            -- A generation still sitting in the RETIRED slot is one a
            -- previous cycle had already moved out of the family and was
            -- interrupted before deleting. It has genuinely aged out, so
            -- discarding it now is that cycle's own intended outcome
            -- finally completing -- and it has to go before this cycle
            -- can retire anything into that name.
            retiredCleared ← clearRetired syncDir logger slots
            case retiredCleared of
                Left err → pure (Left ("autosave refused: " <> err))
                Right () | any stagedGeneration slots → do
                    logInfo logger CatWorld
                        "Autosave: a previously published generation was \
                        \never rotated in -- doing that first"
                    finalizeAutosaveRotationWithSync syncDir logger
                        luaKnownNames depth
                Right () → pure (Right ())
  where
    stagedGeneration s =
        ssName s ≡ autosaveIncomingSlotName ∧ ssDirExists s

-- | Remove a leftover retired generation, if one is present. Always runs
--   BEFORE a cycle retires anything of its own, since that rename needs
--   the name free.
--
--   The removal is a change to @saves\/@'s OWN entry list, so it is
--   synced before the caller may treat the name as free (issue #2229) —
--   otherwise a rotation that retires into that name reports a family
--   shape a crash could still unwind. Returns the reason WITHOUT a
--   prefix; each caller supplies its own ("autosave refused" versus
--   "autosave rotation refused").
clearRetired
    ∷ (FilePath → IO ()) → LoggerState → [SlotState] → IO (Either Text ())
clearRetired syncDir logger slots =
    case [ s | s ← slots
         , ssName s ≡ autosaveRetiredSlotName, ssDirExists s ] of
        [] → pure (Right ())
        (retired : _) → do
            logInfo logger CatWorld
                "Autosave: discarding the generation a previous rotation \
                \had already retired"
            result ← try (removeDirectoryRecursive (ssDir retired))
            case result ∷ Either IOException () of
                Left e → pure (Left ("could not discard the previously \
                                     \retired generation: " <> tshow e))
                Right () → syncSavesDirectory syncDir

-- | Make @saves\/@'s own entry list durable (issue #2229). Every
--   rename and removal an autosave cycle performs happens IN @saves\/@,
--   so the family shape a cycle reports as complete is only really on
--   disk once this has returned — the same rule
--   'World.Save.Storage.publishGeneration' applies to a slot it creates,
--   and the one 'World.Save.Storage.Durable.syncDirectory' documents.
--   The message always names the directory; the caller prefixes it.
syncSavesDirectory ∷ (FilePath → IO ()) → IO (Either Text ())
syncSavesDirectory syncDir = do
    result ← try (syncDir savesDirectory)
    pure $ case result ∷ Either SomeException () of
        Left e → Left ("could not make the save directory '"
                       <> T.pack savesDirectory <> "' durable: " <> tshow e)
        Right () → Right ()

-- | Ownership first, then containment — both across the WHOLE cycle
--   range, before any of it is touched.
cycleProblem ∷ [SlotState] → IO (Maybe Text)
cycleProblem slots = case firstProblem slots of
    Just reason → pure (Just reason)
    Nothing     → linkSafetyProblem slots

-- | Rotate the just-published staging generation into @autosave-1@:
--   discard the oldest owned generation, shift the rest down, rename the
--   staged one in. Called ONLY after the publish to
--   'autosaveIncomingSlotName' actually succeeded.
finalizeAutosaveRotation
    ∷ LoggerState → HS.HashSet Text → Int → IO (Either Text ())
finalizeAutosaveRotation = finalizeAutosaveRotationWithSync syncDirectory

-- | 'finalizeAutosaveRotation' with its directory-sync seam supplied by
--   the caller (issue #2229) — see 'prepareAutosaveCycleWithSync'.
finalizeAutosaveRotationWithSync
    ∷ (FilePath → IO ()) → LoggerState → HS.HashSet Text → Int
    → IO (Either Text ())
finalizeAutosaveRotationWithSync syncDir logger luaKnownNames requestedDepth = do
    let depth = clampDepth requestedDepth
    slots ← readSlotStates logger luaKnownNames (cycleSlotNames depth)
    let byName = HM.fromList [ (ssName s, s) | s ← slots ]
        slotAt i = HM.lookup (autosaveSlotName i) byName
        incoming = HM.lookup autosaveIncomingSlotName byName
        retired  = HM.lookup autosaveRetiredSlotName byName
    problem ← cycleProblem slots
    case problem of
        Just reason → pure (Left ("autosave rotation refused: " <> reason))
        Nothing → do
            -- The retire rename below needs that name free.
            retiredCleared ← clearRetired syncDir logger slots
            case (retiredCleared, incoming, retired) of
                (Left err, _, _) →
                    pure (Left ("autosave rotation refused: " <> err))
                (Right (), Just inc, Just ret) | ssDirExists inc →
                    performRotation syncDir logger depth slotAt inc ret
                _ → pure (Left "autosave rotation refused: nothing was \
                               \published to rotate in")

-- | Drop the oldest owned generation, shift the rest down, then move the
--   staged generation into @autosave-1@. Runs only after every check
--   above has passed AND a real generation is durably on disk.
--   Every move here has a destination that is FREE at the moment it
--   runs, and the single deletion is last. That combination is what
--   makes an interruption survivable, and it is why the plan is derived
--   from what is actually on disk rather than assumed:
--
--     * The oldest generation is retired ONLY when the family is
--       genuinely full — "rotation replaces only the oldest owned
--       autosave once the configured depth is full", literally. If any
--       index is already free, nothing has aged out and nothing is
--       retired.
--     * The shift then walks DOWN from that first free index, so it only
--       ever moves the contiguous block below it and every destination
--       was just vacated. It never has to reason about which of two
--       occupied slots moves first.
--
--   Together those make a resumed rotation correct without a journal. An
--   interrupted cycle leaves a family with a hole in it; the next one
--   simply finds that hole as its first free index and shifts into it,
--   so the generations that had already moved stay where they are
--   instead of being shifted a second time (which would either clobber
--   or, with the naive positional version, age out a generation that had
--   not actually reached the end of the family).
--
--   Retiring by rename rather than deleting outright is the other half:
--   until step 4 every generation is still on disk, just not all of it
--   inside the numbered family.
performRotation
    ∷ (FilePath → IO ()) → LoggerState → Int → (Int → Maybe SlotState)
    → SlotState → SlotState → IO (Either Text ())
performRotation syncDir logger depth slotAt incoming retired = do
    let occupied i = maybe False ssDirExists (slotAt i)
    result ← try $ do
        -- 1. Room for the shift: the first already-free index, or — only
        --    if the family is FULL — the one the oldest generation
        --    vacates by being retired. Nothing is destroyed either way.
        firstFree ← case [ i | i ← [1 .. depth], not (occupied i) ] of
            (i : _) → pure i
            [] → do
                forM_ (slotAt depth) $ \oldest → do
                    logInfo logger CatWorld $
                        "Autosave rotation: retiring oldest generation '"
                        <> ssName oldest <> "'"
                    renameDirectory (ssDir oldest) (ssDir retired)
                pure depth
        -- 2. Age the contiguous block below it down one place. Descending,
        --    so each destination is the slot the previous move vacated.
        forM_ [firstFree - 1, firstFree - 2 .. 1] $ \i →
            case (slotAt i, slotAt (i + 1)) of
                (Just from, Just to) | ssDirExists from →
                    renameDirectory (ssDir from) (ssDir to)
                _ → pure ()
        -- 3. The staged generation becomes the newest.
        forM_ (slotAt 1) $ \newest →
            renameDirectory (ssDir incoming) (ssDir newest)
    case result ∷ Either IOException () of
        Left e   → pure (Left (rotationFailed (tshow e)))
        Right () → afterRenames
  where
    rotationFailed reason = "autosave rotation failed: " <> reason
    -- 4. Persist the family's new shape BEFORE the only destructive
    --    step runs (issue #2229). Every rename above changed @saves/@'s
    --    own entry list, and nothing has synced it; removing the retired
    --    generation on top of entries that may not survive a crash is
    --    what would turn "an interruption leaves a partially shifted
    --    family" into "an interruption loses a generation". Failing here
    --    therefore leaves the retired generation UNDELETED, exactly the
    --    resumable state an interrupted rotation already produces.
    afterRenames = do
        synced ← syncSavesDirectory syncDir
        case synced of
            Left reason → pure (Left (rotationFailed reason))
            Right ()    → afterFamilySync
    -- 5. Only now, with the whole family in its final shape AND that
    --    shape durable, is the retired generation actually gone.
    afterFamilySync = do
        removal ← try $ do
            stillRetired ← doesDirectoryExist (ssDir retired)
            when stillRetired $ removeDirectoryRecursive (ssDir retired)
            pure stillRetired
        case removal ∷ Either IOException Bool of
            Left e      → pure (Left (rotationFailed (tshow e)))
            -- Nothing aged out (the family was not full), so there is no
            -- removal entry to persist and the sync above already
            -- covered every change this rotation made.
            Right False → pure (Right ())
            Right True  → do
                synced ← syncSavesDirectory syncDir
                pure $ case synced of
                    -- The numbered family is already durable from step
                    -- 4; only the retired directory's disappearance may
                    -- still be in flight, and a resumed cycle's own
                    -- 'clearRetired' discards it again either way.
                    Left reason → Left (rotationFailed reason)
                    Right ()    → Right ()
