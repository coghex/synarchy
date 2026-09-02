{-# LANGUAGE Strict #-}
-- | Reference-aware cleanup of the generated-world library (issue
--   #2024, requirements 6, 7, 8 and 9). Conservative in exactly one
--   direction: it removes only what it has POSITIVELY proven
--   unreferenced or abandoned, and when it cannot prove, it retains.
--
--   Under the library lock — which is what makes every transient
--   present provably abandoned ("World.GeneratedLibrary.Lock") — one
--   run does, in order:
--
--   1. Reconcile ("World.GeneratedLibrary.Registry"): restore any
--      displaced entry whose final is absent, and bring the registry
--      into agreement with the directory. Cleanup never judges from a
--      registry it has not just verified.
--   2. Sweep abandoned staging directories and registry temporaries.
--      Tombstones, and displaced copies whose final IS a complete entry,
--      are swept only when reconciliation proved the matching registry
--      durable. A displaced copy beside a final that is not complete, or
--      beside a registry that could not be repaired, is retained — it may
--      be the only durable recovery copy of that entry.
--   3. Scan references ("World.GeneratedLibrary.References"). If any
--      slot was indeterminate, stop here: no final entry is removed
--      this run ('crDeletionSuppressed'), and the report says which
--      slot prevented it.
--   4. Probe every pin file ("World.GeneratedLibrary.Pins") of an id
--      this process does not itself hold: one whose record lock is
--      still held marks its id live; one nobody holds is abandoned and
--      removed. An id this process holds is live without a probe — its
--      own pin file must never be probed.
--   5. Otherwise, for every complete final whose id is neither
--      referenced nor pinned (here or elsewhere): atomically DETACH
--      it by renaming it to a tombstone, sync the root, rewrite the
--      registry without it, and only then delete the tombstone
--      recursively. An interruption during deletion leaves an
--      identifiable tombstone — never a half-deleted final that a later
--      reader could mistake for an entry — and the next run sweeps it.
--
--   Unreadable finals and unfamiliar names are never touched.
module World.GeneratedLibrary.Cleanup
    ( cleanupUnlocked
    ) where

import UPrelude
import qualified Data.HashSet as HS
import qualified Data.Set as Set
import qualified Data.Text as T
import Control.Exception (IOException, SomeException, try)
import System.Directory (doesFileExist, doesPathExist, renameDirectory)
import System.FilePath ((</>))
import World.Page.GeneratedId (GeneratedWorldId)
import World.Save.Storage.Durable
    (rejectSymlinkedPath, removeIfExists, syncDirectory)
import World.GeneratedLibrary.Types
import World.GeneratedLibrary.Layout
import World.GeneratedLibrary.Registry
import World.GeneratedLibrary.References
import World.GeneratedLibrary.Publish
    (claimTransientName, removeTransientDirectory)
import World.GeneratedLibrary.Pins (PinProbe(..), sweepPinFile)

-- | One cleanup run. Caller holds the library lock. @pinned@ is the set
--   of ids THIS process holds pins on; ids other processes hold pins on
--   are discovered by probing their pin files. Both are retained exactly
--   as referenced ones are.
cleanupUnlocked
    ∷ LibraryConfig → HS.HashSet Text → Set.Set GeneratedWorldId
    → IO (Either LibraryFailure CleanupReport)
cleanupUnlocked cfg luaKnownNames pinned = do
    reconciled ← reconcileUnlocked cfg
    case reconciled of
        Left f → pure (Left f)
        Right (registry, entries, registryDurability, rcReport) → do
            scanned ← scanRoot root
            case scanned of
                Left reason → pure (Left (LibraryFailure LibDirectoryList Nothing (Just root) reason))
                Right scan → do
                    let committedToks = [ leName e | e ← entries, leStatus e ≡ EntryCommitted ]
                        durableRecoveryGarbage = case registryDurability of
                            RegistryDurable → rtTombstones scan
                                ⧺ [ p | (tok, p) ← rtDisplaced scan
                                      , tok `elem` committedToks ]
                            RegistryNotDurable → []
                        directorySweepable = rtStaging scan ⧺ durableRecoveryGarbage
                    (sweptDirectories, directoryWarnings) ←
                        sweepDirectories directorySweepable
                    (sweptRegistryTemps, registryTempWarnings) ←
                        sweepRegistryTemps (rtRegistryTemps scan)
                    let sweptTransients = sweptDirectories ⧺ sweptRegistryTemps
                        sweepWarnings = directoryWarnings ⧺ registryTempWarnings
                    refs ← scanSaveReferences (lcSavesDirectory cfg) luaKnownNames
                    (foreignHeld, sweptPins, pinWarnings) ← probeForeignPins (rtPins scan)
                    let referenced = rsReferenced refs
                        live gid   = gid `Set.member` pinned
                                       ∨ T.pack (entryDirectoryName gid) `Set.member` foreignHeld
                        committed = [ (erId rec, root </> T.unpack (leName e))
                                    | e ← entries, leStatus e ≡ EntryCommitted
                                    , Just rec ← [leRecord e] ]
                        retainedRef = [ gid | (gid, _) ← committed, gid `Set.member` referenced ]
                        retainedPin = [ gid | (gid, _) ← committed
                                            , not (gid `Set.member` referenced)
                                            , live gid ]
                        candidates  = [ c | c@(gid, _) ← committed
                                          , not (gid `Set.member` referenced)
                                          , not (live gid) ]
                        unreadable  = [ root </> T.unpack (leName e)
                                      | e ← entries, EntryUnreadable _ ← [leStatus e] ]
                        suppressed  = not (null (rsIndeterminate refs))
                        base = CleanupReport
                            { crReconcile          = rcReport
                            , crReferences         = refs
                            , crRemoved            = []
                            , crRetainedReferenced = retainedRef
                            , crRetainedPinned     = retainedPin
                            , crRetainedUnreadable = unreadable
                            , crDeletionSuppressed = suppressed
                            , crTransientsRemoved  = sweptTransients ⧺ sweptPins
                            , crWarnings           = sweepWarnings ⧺ pinWarnings
                            }
                    if suppressed ∨ null candidates
                        then pure (Right base)
                        else do
                            (removed, tombstones, detachWarnings) ← detachAll candidates
                            syncWarnings ← syncRoot
                            registryWarnings ←
                                if null removed ∨ not (null syncWarnings) then pure [] else do
                                    let remaining = filter ((`notElem` removed) . rrId) (rfRows registry)
                                    w ← writeRegistryFile root (RegistryFile remaining)
                                    pure (either (: []) (const []) w)
                            (_, deleteWarnings) ←
                                if null syncWarnings ∧ null registryWarnings
                                    then sweepDirectories tombstones
                                    else pure ([], [])
                            finalSync ←
                                if null syncWarnings ∧ null registryWarnings
                                    then syncRoot
                                    else pure []
                            pure (Right base
                                { crRemoved           = removed
                                , crWarnings          = sweepWarnings ⧺ detachWarnings
                                                        ⧺ syncWarnings ⧺ registryWarnings
                                                        ⧺ deleteWarnings ⧺ finalSync
                                })
  where
    root = lcRoot cfg

    -- Probe every pin file of an id this process does not hold. Returns
    -- the tokens whose pins are live (held elsewhere, or unjudgeable —
    -- retained either way), the abandoned pin files removed, and the
    -- reasons for any that could not be judged.
    probeForeignPins pins = do
        results ← forM [ pin | pin@(tok, _) ← pins, not (heldHere tok) ] $ \(tok, path) → do
            probe ← sweepPinFile path
            pure $ case probe of
                PinHeld           → (Set.singleton tok, [], [])
                PinAbandoned      → (Set.empty, [path], [])
                PinUnreadable why → ( Set.singleton tok, []
                                    , [ "retaining pin " <> T.pack path
                                        <> " that could not be judged: " <> why ] )
        pure ( Set.unions [ h | (h, _, _) ← results ]
             , concat [ p | (_, p, _) ← results ]
             , concat [ w | (_, _, w) ← results ] )
    heldHere tok = tok `Set.member` Set.map (T.pack . entryDirectoryName) pinned

    syncRoot = do
        r ← try (syncDirectory root)
        pure $ case r of
            Left (e ∷ SomeException) → [ "library root sync failed: " <> tshow e ]
            Right ()                 → []

    -- Remove each path, keeping the ones that went and the reasons for
    -- the ones that did not.
    sweepDirectories paths = do
        results ← mapM (\p → (,) p ⊚ removeTransientDirectory p) paths
        pure ( [ p | (p, []) ← results ]
             , concat [ w | (_, w) ← results ] )

    -- Registry candidates are files, not entry directories. Refuse a
    -- symlink or an unexpected directory even when its name has the owned
    -- shape; only a regular file can be a crashed registry write.
    sweepRegistryTemps paths = do
        results ← mapM (\p → (,) p ⊚ removeRegistryTemp p) paths
        pure ( [ p | (p, []) ← results ]
             , concat [ w | (_, w) ← results ] )
    removeRegistryTemp path = do
        linkSafe ← rejectSymlinkedPath path
        case linkSafe of
            Left reason → pure [ "not removing " <> T.pack path <> ": " <> reason ]
            Right () → do
                exists ← doesPathExist path
                isFile ← doesFileExist path
                if not exists then pure []
                else if not isFile
                    then pure [ "not removing " <> T.pack path
                                <> ": registry temporary is not a regular file" ]
                    else removeIfExists path

    -- Detach each candidate to a tombstone. A rename that fails leaves
    -- the final in place, retained; it is reported, never forced.
    detachAll candidates = do
        results ← mapM detach candidates
        pure ( [ gid | Right (gid, _) ← results ]
             , [ tomb | Right (_, tomb) ← results ]
             , [ w | Left w ← results ] )
    detach (gid, dir) = do
        tomb ← claimTransientName root TombstoneDir gid
        r ← try (renameDirectory dir tomb)
        pure $ case r of
            Right () → Right (gid, tomb)
            Left (e ∷ IOException) →
                Left ("could not detach " <> T.pack dir <> ": " <> tshow e)
