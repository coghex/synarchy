{-# LANGUAGE Strict #-}
-- | The generated-world library's registry and its reconciliation with
--   the directory (issue #2024, requirements 5 and 8).
--
--   The registry is an INDEX: it lets a caller answer "which entries
--   exist, with what identity and integrity summary" from one file,
--   without opening every entry record. It is never the authority. The
--   authority is the set of complete final directories — each proven by
--   its own 'EntryRecord' ("World.GeneratedLibrary.Entry") — and
--   'reconcileUnlocked' rebuilds the registry from them whenever the two
--   disagree in EITHER direction:
--
--   * a complete final the registry does not index is ADDED;
--   * a registry row whose final is absent is DROPPED;
--   * a row whose summary disagrees with the record is CORRECTED;
--   * a final that is not a complete entry is RETAINED and reported,
--     never indexed as committed and never deleted;
--   * a registry file that is absent, truncated or bit-flipped is
--     REBUILT, with no payload touched.
--
--   Reconciliation is also where an interrupted republish is repaired:
--   a @displaced@ copy whose final is absent (the crash landed between
--   moving the old entry aside and committing the new one) is verified
--   in full and moved back to final, so the library reconciles to the
--   OLD complete entry rather than to nothing. A displaced copy beside
--   a complete final is post-commit garbage and is left for cleanup;
--   one beside an UNREADABLE final is left too, and both are reported —
--   deleting an unreadable final to make room is exactly the "absence
--   of evidence" deletion the spec forbids.
module World.GeneratedLibrary.Registry
    ( RegistryRead(..)
    , RegistryDurability(..)
    , readRegistryFile
    , writeRegistryFile
    , RootScan(..)
    , scanRoot
    , inventoryFromDirectory
    , reconcileUnlocked
    ) where

import UPrelude
import qualified Data.ByteString as BS
import qualified Data.List as L
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import Control.Exception (IOException, SomeException, onException, try)
import System.Directory
    ( doesDirectoryExist, doesFileExist, listDirectory, renameDirectory
    , renameFile )
import System.FilePath ((</>))
import System.IO (openBinaryTempFile)
import World.Page.GeneratedId (GeneratedWorldId)
import World.Save.Storage.Durable
    ( rejectSymlinkedPath, rejectSymlinkedManagedPath, durableFlush
    , syncDirectory, closeQuietly, removeIfExists )
import World.GeneratedLibrary.Types
import World.GeneratedLibrary.Layout
import World.GeneratedLibrary.Entry

-- Registry file ----------------------------------------------------------------

data RegistryRead
    = RegistryAbsent
    | RegistryTorn !Text
    | RegistryPresent !RegistryFile
    deriving (Show, Eq)

-- | Whether reconciliation proved that the on-disk registry now equals
--   the authoritative final-directory inventory. Cleanup may discard a
--   displaced recovery copy only in the durable case.
data RegistryDurability = RegistryDurable | RegistryNotDurable
    deriving (Show, Eq)

-- | Read the registry. Never throws: a symlinked, unreadable, truncated
--   or undecodable file is 'RegistryTorn' with the reason.
readRegistryFile ∷ FilePath → IO RegistryRead
readRegistryFile root = do
    let path = root </> registryFileName
    linkSafe ← rejectSymlinkedPath path
    case linkSafe of
        Left reason → pure (RegistryTorn reason)
        Right () → do
            exists ← doesFileExist path
            if not exists then pure RegistryAbsent else do
                readResult ← try (BS.readFile path)
                pure $ case readResult of
                    Left (e ∷ IOException) → RegistryTorn ("cannot read: " <> tshow e)
                    Right bytes → either RegistryTorn RegistryPresent
                                         (decodeRegistry bytes)

-- | Durably replace the registry: an exclusively created temporary in the
--   root whose handle stays owned through write + @fsync@, re-read and
--   decode, atomic rename onto
--   'registryFileName', directory sync — the same primitives the save
--   transaction uses, in the same order. An expected failure removes its
--   own temporary; a process crash can leave the reserved name behind for
--   cleanup to sweep under the library lock.
writeRegistryFile ∷ FilePath → RegistryFile → IO (Either Text ())
writeRegistryFile root reg = do
    let path  = root </> registryFileName
        bytes = encodeRegistry reg
    r ← try $ do
        (tmp, h) ← openBinaryTempFile root registryTempTemplate
        let cleanupTemp = do
                closeQuietly h
                _ ← removeIfExists tmp
                pure ()
        (do
            BS.hPut h bytes
            durableFlush h
            reread ← BS.readFile tmp
            case decodeRegistry reread of
                Right reg' | reg' ≡ reg → pure ()
                Right _  → ioError (userError "re-read registry differs from the one written")
                Left err → ioError (userError (T.unpack err))
            renameFile tmp path
            syncDirectory root
         ) `onException` cleanupTemp
    pure $ case r of
        Left (e ∷ SomeException) → Left ("registry write failed: " <> tshow e)
        Right ()                 → Right ()

-- Root scan ---------------------------------------------------------------------

-- | Every name in the root, classified. Finals and displaced copies
--   carry the token their name proves; every list is ascending by name
--   so reports are deterministic.
data RootScan = RootScan
    { rtFinals     ∷ ![(Text, FilePath)]
    , rtStaging    ∷ ![FilePath]
    , rtDisplaced  ∷ ![(Text, FilePath)]
    , rtTombstones ∷ ![FilePath]
    , rtRegistryTemps ∷ ![FilePath]
    , rtPins       ∷ ![(Text, FilePath)]
    , rtUnfamiliar ∷ ![FilePath]
    } deriving (Show, Eq)

scanRoot ∷ FilePath → IO (Either Text RootScan)
scanRoot root = do
    listed ← try (listDirectory root)
    pure $ case listed of
        Left (e ∷ IOException) → Left ("cannot list library root: " <> tshow e)
        Right names → Right (L.foldl' place (RootScan [] [] [] [] [] [] []) (L.sort names))
  where
    place scan name = case classifyLibraryName name of
        FinalEntryName tok             → scan { rtFinals = rtFinals scan ⧺ [(tok, root </> name)] }
        TransientName StagingDir _     → scan { rtStaging = rtStaging scan ⧺ [root </> name] }
        TransientName DisplacedDir tok → scan { rtDisplaced = rtDisplaced scan ⧺ [(tok, root </> name)] }
        TransientName TombstoneDir _   → scan { rtTombstones = rtTombstones scan ⧺ [root </> name] }
        PinName tok                    → scan { rtPins = rtPins scan ⧺ [(tok, root </> name)] }
        RegistryName                   → scan
        RegistryTempName               → scan { rtRegistryTemps = rtRegistryTemps scan ⧺ [root </> name] }
        LockName                       → scan
        UnfamiliarName                 → scan { rtUnfamiliar = rtUnfamiliar scan ⧺ [root </> name] }

-- | Every final directory, judged. Read-only.
inventoryFromDirectory ∷ FilePath → IO (Either Text [LibraryEntry])
inventoryFromDirectory root = do
    scanned ← scanRoot root
    case scanned of
        Left reason → pure (Left reason)
        Right scan  → Right ⊚ mapM judge (rtFinals scan)
  where
    judge (tok, dir) = libraryEntryFor tok ⊚ readEntryDirectory dir

-- Reconciliation ------------------------------------------------------------------

-- | Reconcile the registry with the directory. Caller holds the lock.
--   Returns the registry as now written, every final's judgement, and
--   what was done. A registry that already matched is not rewritten, but
--   its directory is synced before cleanup is told recovery copies are
--   discardable: a prior rename whose root sync failed can leave matching
--   bytes visible without having crossed the durability boundary.
reconcileUnlocked
    ∷ LibraryConfig
    → IO (Either LibraryFailure
            (RegistryFile, [LibraryEntry], RegistryDurability, ReconcileReport))
reconcileUnlocked cfg = do
    let root = lcRoot cfg
        failure phase path reason = Left (LibraryFailure phase Nothing (Just path) reason)
    safety ← rejectSymlinkedManagedPath root
    case safety of
        Left reason → pure (failure LibUnsafePath root reason)
        Right () → do
            scanned ← scanRoot root
            case scanned of
                Left reason → pure (failure LibDirectoryList root reason)
                Right scan0 → do
                    (recovered, recoveryWarnings) ← recoverDisplaced root scan0
                    -- Re-scan so the finals list includes what recovery
                    -- restored, judged exactly like every other final.
                    rescanned ← scanRoot root
                    case rescanned of
                        Left reason → pure (failure LibDirectoryList root reason)
                        Right scan → do
                            entries ← mapM (\(tok, dir) → libraryEntryFor tok ⊚ readEntryDirectory dir)
                                           (rtFinals scan)
                            existing ← readRegistryFile root
                            let desired = RegistryFile (L.sortOn rrId (committedRows entries))
                                (rebuilt, oldRows) = case existing of
                                    RegistryPresent r → (Nothing, rfRows r)
                                    RegistryAbsent    → (Just "registry file is absent", [])
                                    RegistryTorn why  → (Just why, [])
                                oldMap = Map.fromList [ (rrId r, r) | r ← oldRows ]
                                newMap = Map.fromList [ (rrId r, r) | r ← rfRows desired ]
                                added     = Map.keys (newMap `Map.difference` oldMap)
                                dropped   = Map.keys (oldMap `Map.difference` newMap)
                                corrected = [ gid | (gid, new) ← Map.toList newMap
                                                  , Just old ← [Map.lookup gid oldMap]
                                                  , old ≢ new ]
                                changed = isJust rebuilt ∨ not (null added ∧ null dropped ∧ null corrected)
                                unreadable = [ (root </> T.unpack (leName e), why)
                                             | e ← entries, EntryUnreadable why ← [leStatus e] ]
                                displacedLeft = [ "displaced copy retained beside a non-complete final: " <> T.pack path
                                                | (tok, path) ← rtDisplaced scan
                                                , tok `elem` [ leName e | e ← entries
                                                                        , not (isCommitted e) ] ]
                            (registryDurability, writeWarnings) ←
                                if changed then do
                                    w ← writeRegistryFile root desired
                                    pure $ case w of
                                        Left warning → (RegistryNotDurable, [warning])
                                        Right ()     → (RegistryDurable, [])
                                else do
                                    synced ← try (syncDirectory root)
                                    pure $ case synced of
                                        Left (e ∷ SomeException) →
                                            ( RegistryNotDurable
                                            , [ "registry durability sync failed: "
                                                <> tshow e ] )
                                        Right () → (RegistryDurable, [])
                            let report = ReconcileReport
                                    { rcAdded      = added
                                    , rcDropped    = dropped
                                    , rcCorrected  = corrected
                                    , rcRecovered  = recovered
                                    , rcUnreadable = unreadable
                                    , rcUnfamiliar = rtUnfamiliar scan
                                    , rcRegistryRebuilt = rebuilt
                                    , rcWarnings   = recoveryWarnings ⧺ displacedLeft ⧺ writeWarnings
                                    }
                            pure (Right (desired, entries, registryDurability, report))
  where
    isCommitted e = leStatus e ≡ EntryCommitted
    committedRows entries =
        [ registryRowFor digest rec
        | e ← entries, isCommitted e
        , Just rec ← [leRecord e], Just digest ← [leDigest e] ]

-- | Restore every displaced copy whose final is ABSENT. Only a copy that
--   passes the DEEP check is promoted, since promotion is the library
--   vouching for it as complete; a displaced copy that fails stays
--   where it is and is reported. When several displaced copies of one
--   id exist (two interrupted republishes), the lexically last name —
--   the highest process/counter suffix, i.e. the most recent — is tried
--   first, and the first that verifies wins.
recoverDisplaced ∷ FilePath → RootScan → IO ([GeneratedWorldId], [Text])
recoverDisplaced root scan = do
    let finalToks = map fst (rtFinals scan)
        orphaned  = Map.toList (Map.fromListWith (⧺)
                        [ (tok, [path]) | (tok, path) ← rtDisplaced scan
                                        , tok `notElem` finalToks ])
    results ← mapM restore orphaned
    pure (concatMap fst results, concatMap snd results)
  where
    restore (tok, paths) = go (reverse (L.sort paths)) []
      where
        finalDir = root </> T.unpack tok
        go [] warnings = pure ([], reverse warnings)
        go (path : rest) warnings = do
            verified ← verifyEntryDirectory path
            case verified of
                Left why → go rest (("displaced copy " <> T.pack path
                                     <> " is not a complete entry: " <> why) : warnings)
                Right (rec, _) → do
                    stillAbsent ← not ⊚ doesDirectoryExist finalDir
                    if not stillAbsent then pure ([], reverse warnings) else do
                        moved ← try $ do
                            renameDirectory path finalDir
                            syncDirectory root
                        case moved of
                            Left (e ∷ SomeException) →
                                go rest (("could not restore displaced copy "
                                          <> T.pack path <> ": " <> tshow e) : warnings)
                            Right () → pure ([erId rec], reverse warnings)
