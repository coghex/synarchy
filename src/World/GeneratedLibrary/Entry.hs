{-# LANGUAGE Strict #-}
-- | Reading one entry directory of the generated-world library (issue
--   #2024): the single judgement of whether a directory IS a complete
--   entry, shared by reconciliation, cleanup, lookup and republishing so
--   none of them can disagree about what "complete" means.
--
--   A directory is a complete entry iff, in this order:
--
--   1. neither it nor its immediate parent is a symlink;
--   2. its 'entryRecordFileName' is a regular (non-symlink) file that
--      decodes as an 'EntryRecord';
--   3. the record's id renders to the token the directory's OWN name
--      carries — its whole name for a final, the id portion for a
--      displaced or staged copy — the one place identity text on disk
--      is ever compared to an id, and it is compared by rendering the
--      id, never by parsing the text;
--   4. every file the record lists is present, is not a symlink, and
--      has exactly its recorded size.
--
--   That is the CHEAP check ('readEntryDirectory'), run over every
--   final on every reconciliation; it never reads payload bytes. The
--   DEEP check ('verifyEntryDirectory') additionally hashes every
--   listed file against its recorded digest and is run where the
--   library is about to PROMOTE a directory — restoring a displaced
--   copy to final — and is offered to the map loader (WML-9) for the
--   same reason. Files the record does not list are ignored by both:
--   the library does not own them, so it neither trusts nor removes
--   them.
module World.GeneratedLibrary.Entry
    ( readEntryDirectory
    , verifyEntryDirectory
    , libraryEntryFor
    ) where

import UPrelude
import qualified Crypto.Hash.SHA256 as SHA256
import qualified Data.ByteString as BS
import qualified Data.Text as T
import Control.Exception (IOException, try)
import System.Directory (doesDirectoryExist, doesFileExist, getFileSize)
import System.FilePath ((</>), takeFileName)
import World.Page.GeneratedId (renderGeneratedWorldId)
import World.Save.Storage.Durable (rejectSymlinkedPath, rejectSymlinkedManagedPath)
import World.GeneratedLibrary.Types
import World.GeneratedLibrary.Layout

-- | The cheap completeness check. On success returns the record and the
--   inventory digest computed over it.
readEntryDirectory ∷ FilePath → IO (Either Text (EntryRecord, BS.ByteString))
readEntryDirectory dir = do
    safety ← rejectSymlinkedManagedPath dir
    case safety of
        Left reason → pure (Left reason)
        Right () → do
            isDir ← doesDirectoryExist dir
            if not isDir then pure (Left ("not a directory: " <> T.pack dir)) else do
                recResult ← readRecord dir
                case recResult of
                    Left reason → pure (Left reason)
                    Right rec → do
                        filesResult ← checkListedFiles dir rec
                        pure $ case filesResult of
                            Left reason → Left reason
                            Right ()    → Right (rec, inventoryDigest (erFiles rec))

-- | The deep completeness check: everything 'readEntryDirectory' checks
--   plus a byte-level digest of every listed file.
verifyEntryDirectory ∷ FilePath → IO (Either Text (EntryRecord, BS.ByteString))
verifyEntryDirectory dir = do
    cheap ← readEntryDirectory dir
    case cheap of
        Left reason → pure (Left reason)
        Right ok@(rec, _) → do
            digests ← mapM (checkDigest dir) (erFiles rec)
            pure $ case [ r | Left r ← digests ] of
                (reason : _) → Left reason
                []           → Right ok

-- | The 'LibraryEntry' a final directory named @name@ reports as.
libraryEntryFor ∷ Text → Either Text (EntryRecord, BS.ByteString) → LibraryEntry
libraryEntryFor name result = case result of
    Left reason → LibraryEntry
        { leName = name, leStatus = EntryUnreadable reason
        , leRecord = Nothing, leDigest = Nothing }
    Right (rec, digest) → LibraryEntry
        { leName = name, leStatus = EntryCommitted
        , leRecord = Just rec, leDigest = Just digest }

-- Internals -------------------------------------------------------------------

readRecord ∷ FilePath → IO (Either Text EntryRecord)
readRecord dir = do
    let path = dir </> entryRecordFileName
    linkSafe ← rejectSymlinkedPath path
    case linkSafe of
        Left reason → pure (Left reason)
        Right () → do
            exists ← doesFileExist path
            if not exists
                then pure (Left ("entry record is missing: " <> T.pack path))
                else do
                    readResult ← try (BS.readFile path)
                    pure $ case readResult of
                        Left (e ∷ IOException) →
                            Left ("cannot read entry record " <> T.pack path
                                  <> ": " <> tshow e)
                        Right bytes → do
                            rec ← decodeEntryRecord bytes
                            claimed ← case classifyLibraryName (takeFileName dir) of
                                FinalEntryName tok   → Right tok
                                TransientName _ tok  → Right tok
                                _ → Left ("not a library entry name: " <> T.pack dir)
                            let actual = renderGeneratedWorldId (erId rec)
                            when (actual ≢ claimed) $
                                Left ("entry record names " <> actual
                                      <> " but sits in " <> claimed)
                            pure rec

checkListedFiles ∷ FilePath → EntryRecord → IO (Either Text ())
checkListedFiles dir rec = do
    results ← mapM (checkListed dir) (erFiles rec)
    pure $ case [ r | Left r ← results ] of
        (reason : _) → Left reason
        []           → Right ()

checkListed ∷ FilePath → PayloadDescriptor → IO (Either Text ())
checkListed dir desc = do
    let path = dir </> T.unpack (pdName desc)
    linkSafe ← rejectSymlinkedPath path
    case linkSafe of
        Left reason → pure (Left reason)
        Right () → do
            exists ← doesFileExist path
            if not exists
                then pure (Left ("listed payload file is missing: " <> T.pack path))
                else do
                    sizeResult ← try (getFileSize path)
                    pure $ case sizeResult of
                        Left (e ∷ IOException) →
                            Left ("cannot stat " <> T.pack path <> ": " <> tshow e)
                        Right size
                            | fromIntegral size ≢ pdSize desc →
                                Left ("payload file " <> T.pack path <> " is "
                                      <> tshow size <> " bytes, record says "
                                      <> tshow (pdSize desc))
                            | otherwise → Right ()

checkDigest ∷ FilePath → PayloadDescriptor → IO (Either Text ())
checkDigest dir desc = do
    let path = dir </> T.unpack (pdName desc)
    readResult ← try (BS.readFile path)
    pure $ case readResult of
        Left (e ∷ IOException) →
            Left ("cannot read " <> T.pack path <> ": " <> tshow e)
        Right bytes
            | SHA256.hash bytes ≢ pdDigest desc →
                Left ("payload file " <> T.pack path
                      <> " does not match its recorded digest")
            | otherwise → Right ()
