{-# LANGUAGE Strict #-}
-- | Reference discovery for the generated-world library (issue #2024,
--   requirements 6, 7 and 9): which 'GeneratedWorldId's are referenced
--   by at least one current save, and — just as important — whether
--   that question could be answered at all.
--
--   === What counts as a reference
--
--   Every generation file of every save slot: the authoritative
--   @world.synworld@, the retained @world.synworld.prev@ recovery
--   generation, and a legacy flat @\<name\>.synworld@. Each is read at
--   'World.Save.Serialize.listSaves' DEPTH — envelope structure,
--   checksums and the @"metadata"@ component only, through the same
--   'decodeSaveEnvelopeMetadataClassified' listing uses — and each
--   contributes 'World.Save.Types.smGeneratedWorldIds', which since
--   #2021 is the COMPLETE per-save inventory: one id per page, every
--   page, not just the active one. A previous generation protects what
--   IT references even when the authoritative generation has moved on,
--   because the loader can still fall back to it.
--
--   A generation that decodes to a metadata version predating #2021
--   (v1, v2) contributes a known EMPTY set: it was positively read, and
--   it names nothing.
--
--   === What makes the answer untrustworthy
--
--   This is NOT 'listSaves'. Listing deliberately DROPS a slot it cannot
--   read so a save browser stays usable; a reference census must do the
--   opposite, because a dropped slot might be the one save that
--   references an entry. So every slot that cannot yield a trustworthy
--   complete set is recorded in 'rsIndeterminate' with its reason — a
--   symlinked slot or saves directory, a generation file that is a
--   symlink, unreadable, storage-corrupt, or INCOMPATIBLE (a newer build's
--   save, which may name ids this build cannot see) — and cleanup then
--   removes no final entry at all during that run. Cleanup is
--   conservative in exactly one direction.
--
--   Leftover save-transaction artifacts (@world-synworld-tmp…@,
--   @world-synworld-stale…@) are not generations: the loader never
--   selects them, so they neither reference nor block.
module World.GeneratedLibrary.References
    ( scanSaveReferences
    ) where

import UPrelude
import qualified Data.ByteString as BS
import qualified Data.HashSet as HS
import qualified Data.Set as Set
import qualified Data.Text as T
import Control.Exception (IOException, try)
import System.Directory
    (doesDirectoryExist, doesFileExist, listDirectory)
import System.FilePath ((</>), takeExtension, dropExtension)
import World.Save.Types (SaveMetadata(..))
import World.Save.Envelope
    ( decodeSaveEnvelopeMetadata, decodeSaveEnvelopeMetadataClassified
    , renderGenerationFailure )
import World.Save.Serialize (saveExtension)
import World.Save.Storage
    (authoritativeFileName, previousGenerationFileName)
import World.Save.Storage.Durable (rejectSymlinkedPath, rejectSymlinkedManagedPath)
import World.GeneratedLibrary.Types

-- | Scan the saves directory. @luaKnownNames@ is the live Lua component
--   registry, exactly as 'listSaves' receives it. Never throws; a
--   missing saves directory is an empty, fully determinate scan (no
--   saves reference anything).
scanSaveReferences ∷ FilePath → HS.HashSet Text → IO ReferenceScan
scanSaveReferences savesDir luaKnownNames = do
    linkSafe ← rejectSymlinkedPath savesDir
    case linkSafe of
        Left reason → pure (indeterminate (T.pack savesDir) reason)
        Right () → do
            exists ← doesDirectoryExist savesDir
            if not exists then pure emptyReferenceScan else do
                listed ← try (listDirectory savesDir)
                case listed of
                    Left (e ∷ IOException) →
                        pure (indeterminate (T.pack savesDir)
                                            ("cannot list saves directory: " <> tshow e))
                    Right entries → do
                        scans ← mapM scanEntry entries
                        pure (foldr merge emptyReferenceScan scans)
  where
    indeterminate slot reason = emptyReferenceScan { rsIndeterminate = [(slot, reason)] }

    merge a b = ReferenceScan
        { rsReferenced    = rsReferenced a `Set.union` rsReferenced b
        , rsSourcesRead   = rsSourcesRead a + rsSourcesRead b
        , rsIndeterminate = rsIndeterminate a ⧺ rsIndeterminate b
        }

    scanEntry entry = do
        let fullPath = savesDir </> entry
        isDir ← doesDirectoryExist fullPath
        if isDir
            then scanSlot (T.pack entry) fullPath
            else if takeExtension entry ≡ saveExtension
                then scanLegacy (T.pack (dropExtension entry)) fullPath
                else pure emptyReferenceScan

    -- A slot directory: both generation files, each on its own.
    scanSlot slot dir = do
        safety ← rejectSymlinkedManagedPath dir
        case safety of
            Left reason → pure (indeterminate slot reason)
            Right () → do
                auth ← scanGeneration slot (dir </> authoritativeFileName)
                prev ← scanGeneration slot (dir </> previousGenerationFileName)
                pure (merge auth prev)

    scanGeneration slot path = do
        linkSafe ← rejectSymlinkedPath path
        case linkSafe of
            Left reason → pure (indeterminate slot reason)
            Right () → do
                exists ← doesFileExist path
                if not exists then pure emptyReferenceScan else do
                    readResult ← try (BS.readFile path)
                    pure $ case readResult of
                        Left (e ∷ IOException) →
                            indeterminate slot ("cannot read " <> T.pack path <> ": " <> tshow e)
                        Right bytes →
                            case decodeSaveEnvelopeMetadataClassified luaKnownNames bytes of
                                Right meta → found meta
                                Left failure →
                                    indeterminate slot (T.pack path <> ": "
                                                        <> renderGenerationFailure failure)

    -- A legacy flat file has no slot directory; the containment check
    -- covers the file and the saves directory, as 'listSaves' does.
    scanLegacy slot path = do
        safety ← rejectSymlinkedManagedPath path
        case safety of
            Left reason → pure (indeterminate slot reason)
            Right () → do
                readResult ← try (BS.readFile path)
                pure $ case readResult of
                    Left (e ∷ IOException) →
                        indeterminate slot ("cannot read " <> T.pack path <> ": " <> tshow e)
                    Right bytes →
                        case decodeSaveEnvelopeMetadata luaKnownNames bytes of
                            Right meta → found meta
                            Left err   → indeterminate slot (T.pack path <> ": " <> err)

    found meta = ReferenceScan
        { rsReferenced    = Set.fromList (smGeneratedWorldIds meta)
        , rsSourcesRead   = 1
        , rsIndeterminate = []
        }
