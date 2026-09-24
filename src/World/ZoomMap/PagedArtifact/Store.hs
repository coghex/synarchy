{-# LANGUAGE Strict #-}
-- | Reading and writing the paged map artifact (issue #2693): the
--   mandatory set through the generated-world library's publication
--   boundary, and optional fine pages as standalone cache files.
--
--   === Mandatory coverage
--
--   'publishMapArtifact' accepts exactly the mandatory pages — cutoff
--   level through root — as decoded RGBA8, encodes and binds each one,
--   round-trips every page file and the manifest through their own
--   readers, and only then hands the complete payload set to
--   'World.GeneratedLibrary.publishEntryWith'. The library stays
--   payload-neutral: it stages, verifies, renames and records the files
--   exactly as it would any bytes, so every atomicity, path-safety and
--   recovery guarantee is the library's own, unchanged.
--
--   'openMapArtifact' checks everything a mandatory reader must before
--   it answers: the entry exists, the manifest is for the expected world
--   and producer and covers exactly the mandatory pages, every page file
--   has the recorded length and digest, binds to its key, has a page's
--   PNG header and decodes, and the library's own entry record agrees
--   with the manifest about every map file (so a page cannot be
--   substituted behind either). Missing or damaged required data is a
--   refusal; regeneration is WML-9's decision, not this module's.
--
--   === Fine pages
--
--   Fine pages (levels below the cutoff) are NEVER listed in the entry
--   record, so they can never make an entry incomplete: the library
--   ignores unlisted files in an entry directory, neither trusting nor
--   removing them. 'writeFinePage' and 'readFinePage' take the directory
--   they use as an argument — its final placement and quota are WML-10's
--   — and a fine page is self-binding (world, producer and key are in its
--   own header, covered by its own SHA-256), so a reader needs no index.
--   Every problem reading one is a 'FinePageMiss'. Republishing or
--   cleaning up an entry removes its whole directory, unlisted fine pages
--   included; that is an accepted cache miss.
module World.ZoomMap.PagedArtifact.Store
    ( -- * Mandatory coverage
      publishMapArtifact
    , publishMapArtifactWith
    , OpenedMapArtifact(..)
    , openMapArtifact
    , readMandatoryPage
    , reassembleMapRoot
      -- * Fine pages
    , FinePageRead(..)
    , FinePageMiss(..)
    , writeFinePage
    , readFinePage
    ) where

import UPrelude
import qualified Crypto.Hash.SHA256 as SHA256
import qualified Data.ByteString as BS
import qualified Data.List as L
import qualified Data.Set as Set
import qualified Data.Text as T
import Control.Exception (IOException, try)
import System.Directory (doesDirectoryExist, doesFileExist, getFileSize, renameFile)
import System.FilePath ((</>), takeFileName)
import System.IO (IOMode(..), hClose, hFlush, openBinaryTempFile, withBinaryFile)
import World.GeneratedLibrary
import World.Map.ImagePlan (MapImagePlan(..), checkUploadPayload)
import World.Page.GeneratedId (GeneratedWorldId)
import World.Save.Storage.Durable (rejectSymlinkedManagedPath, rejectSymlinkedPath)
import World.ZoomMap.Pyramid.Address
import World.ZoomMap.Pyramid.Inventory
import World.ZoomMap.PagedArtifact.Format
import World.ZoomMap.PagedArtifact.Png
import World.ZoomMap.PagedArtifact.Types

-- * Publication

-- | Publish the mandatory artifact for @gid@. See
--   'publishMapArtifactWith'.
publishMapArtifact
    ∷ Library → GeneratedWorldId → MapCompatibility → Int
    → [(MapPageKey, BS.ByteString)] → IO (Either MapArtifactRefusal PublishReport)
publishMapArtifact = publishMapArtifactWith noPublishHooks

-- | Publish exactly the mandatory pages for a world of @worldSize@, as
--   decoded RGBA8, through the library with the given hooks (production
--   passes 'noPublishHooks'; a test's throwing hook models a crash). A
--   candidate with a missing, extra, repeated or invalid page, a page of
--   the wrong length, or any file that does not round-trip through its
--   own reader is refused before the library is touched.
publishMapArtifactWith
    ∷ PublishHooks → Library → GeneratedWorldId → MapCompatibility → Int
    → [(MapPageKey, BS.ByteString)] → IO (Either MapArtifactRefusal PublishReport)
publishMapArtifactWith hooks lib gid compat worldSize pages =
    case checkCandidateKeys of
        Left r → pure (Left r)
        Right () → do
            encoded ← mapM encodeOne (L.sortOn fst pages)
            case sequence encoded of
                Left r → pure (Left r)
                Right files → case manifestBytes files of
                    Left r → pure (Left r)
                    Right mbytes → do
                        published ← publishEntryWith hooks lib gid
                            ( PayloadFile mapManifestFileName mbytes
                            : [ PayloadFile (mapPageFileName k) f | (k, f) ← files ] )
                        pure (either (Left . MapArtifactLibrary) Right published)
  where
    -- Keys first: cheap, and a bad candidate should cost no encoding.
    checkCandidateKeys = do
        inv ← mapArtifactInventory worldSize
        let geom = mpiGeometry inv
            keys = L.sort (map fst pages)
            required = mandatoryPageKeys inv
            wanted = Set.fromList required
        forM_ keys $ \k →
            either (Left . MapArtifactInvalidKey k . mapAddressRefusalText) Right
                (checkMapPageKey geom k)
        forM_ (zip keys (drop 1 keys)) $ \(a, b) →
            when (a ≡ b) $ Left (MapArtifactDuplicateKey b)
        case [ k | k ← required, k `notElem` keys ] of
            (k : _) → Left (MapArtifactMissingRequired (RequiredPage k))
            []      → Right ()
        case [ k | k ← keys, not (Set.member k wanted) ] of
            (k : _) → Left (MapArtifactUnexpectedPage k)
            []      → Right ()

    encodeOne (key, rgba) = do
        let binding = MapPageBinding gid compat key
            what = mapPageFileName key
        case encodePagePng what rgba of
            Left r → pure (Left r)
            Right png → do
                let file = encodeMapPageFile binding png
                -- The writer proves its own reader accepts the file and
                -- restores the exact bytes before anything is published.
                back ← either (pure . Left) (decodePagePng what)
                              (decodeMapPageFile binding file)
                pure $ case back of
                    Left r → Left r
                    Right rgba'
                        | rgba' ≡ rgba → Right (key, file)
                        | otherwise → Left $ MapArtifactMalformed what
                              "does not round-trip to the bytes it encoded"

    manifestBytes files = do
        m ← buildMapManifest gid compat worldSize files
        bytes ← encodeMapManifest m
        m' ← decodeMapManifest gid compat bytes
        unless (m' ≡ m) $
            Left $ MapArtifactMalformed "manifest" "does not round-trip to the manifest it encoded"
        pure bytes

-- * Reading mandatory coverage

-- | A mandatory artifact that passed every check in 'openMapArtifact'.
data OpenedMapArtifact = OpenedMapArtifact
    { omaDirectory ∷ !FilePath
    , omaManifest  ∷ !MapManifest
    } deriving (Eq, Show)

-- | Open and fully verify @gid@'s mandatory artifact against the
--   expected producer compatibility. Pages are decoded one at a time
--   and discarded, so verification never holds more than one page's
--   pixels.
openMapArtifact
    ∷ Library → GeneratedWorldId → MapCompatibility
    → IO (Either MapArtifactRefusal OpenedMapArtifact)
openMapArtifact lib gid compat = do
    looked ← lookupEntry lib gid
    case looked of
        Left f → pure (Left (MapArtifactLibrary f))
        Right Nothing → pure (Left (MapArtifactAbsent gid))
        Right (Just entry) → do
            let dir = entryDirectory lib gid
            -- The entry directory and its parent are checked BEFORE any
            -- payload is opened: a symlinked entry is also an unreadable
            -- one to the library, but reading through it first would
            -- follow the link outside the library root.
            safe ← rejectSymlinkedManagedPath dir
            read' ← either (pure . Left . MapArtifactIO dir) (const $
                        readBounded "manifest" (dir </> T.unpack mapManifestFileName)
                                    mapManifestMaxBytes) safe
            case read' of
                Left r → pure (Left r)
                Right Nothing → pure (Left (MapArtifactMissingRequired RequiredManifest))
                Right (Just mbytes) → case decodeMapManifest gid compat mbytes of
                    Left r → pure (Left r)
                    Right manifest → do
                        pagesOK ← verifyAll dir (mmPages manifest)
                        pure $ do
                            pagesOK
                            checkEntryRecord entry mbytes manifest
                            pure (OpenedMapArtifact dir manifest)
  where
    -- Stops at the first refusal; each page's pixels are dropped as soon
    -- as that page has decoded.
    verifyAll _ [] = pure (Right ())
    verifyAll dir (p : ps) = verifyPage dir gid compat p
        ≫= either (pure . Left) (const (verifyAll dir ps))

-- | Read one mandatory page's exact decoded RGBA8 bytes from an opened
--   artifact, re-checking its length, digest, binding and header on the
--   bytes actually read.
readMandatoryPage
    ∷ OpenedMapArtifact → MapPageKey → IO (Either MapArtifactRefusal BS.ByteString)
readMandatoryPage opened key =
    case [ p | p ← mmPages m, mmpKey p ≡ key ] of
        [] → pure (Left (MapArtifactUnexpectedPage key))
        (p : _) → verifyPage (omaDirectory opened) (mmWorld m) (mmCompat m) p
  where
    m = omaManifest opened

-- | One page file, checked in the order the format fixes: its directory
--   and that directory's parent are not symlinks (re-checked on EVERY
--   read, since the directory can be replaced after 'openMapArtifact'),
--   the file is present, exactly the recorded length, the recorded
--   digest, then its own framing and binding, then its PNG header, then
--   the native decode.
verifyPage
    ∷ FilePath → GeneratedWorldId → MapCompatibility → MapManifestPage
    → IO (Either MapArtifactRefusal BS.ByteString)
verifyPage dir gid compat p = do
    let key = mmpKey p
        what = "page file " <> mmpName p
        declared = toInteger (mmpFileBytes p)
    safe ← rejectSymlinkedManagedPath dir
    -- The file's size is compared with the manifest's declaration before
    -- it is read, and the read is capped at that declaration.
    read' ← either (pure . Left . MapArtifactIO dir) (const $
                readChecked what (dir </> T.unpack (mmpName p)) (fromIntegral (mmpFileBytes p))
                            (checkSize what declared)) safe
    case read' of
        Left r → pure (Left r)
        Right Nothing → pure (Left (MapArtifactMissingRequired (RequiredPage key)))
        Right (Just bytes)
            | toInteger (BS.length bytes) < declared →
                pure (Left (MapArtifactTruncated what declared (toInteger (BS.length bytes))))
            | toInteger (BS.length bytes) > declared →
                pure (Left (MapArtifactLengthMismatch what declared (toInteger (BS.length bytes))))
            | SHA256.hash bytes ≢ mmpDigest p →
                pure (Left (MapArtifactChecksumMismatch what))
            | otherwise → case decodeMapPageFile (MapPageBinding gid compat key) bytes of
                Left r → pure (Left r)
                Right png → decodePagePng what png

-- | The library's record must be the committed record of THIS content:
--   every map-owned file it lists is one the manifest names (or the
--   manifest itself), with the same size and digest, and it lists all
--   of them. Files other slices own are not the map's to judge.
checkEntryRecord ∷ LibraryEntry → BS.ByteString → MapManifest → Either MapArtifactRefusal ()
checkEntryRecord entry mbytes manifest = case (leStatus entry, leRecord entry) of
    (EntryUnreadable why, _) → Left (MapArtifactEntryIncomplete why)
    (EntryCommitted, Nothing) → Left (MapArtifactEntryIncomplete "no record")
    (EntryCommitted, Just rec) → do
        let listed = [ d | d ← erFiles rec, isMapOwnedFileName (pdName d) ]
            expected =
                (mapManifestFileName, fromIntegral (BS.length mbytes), SHA256.hash mbytes)
                : [ (mmpName p, mmpFileBytes p, mmpDigest p) | p ← mmPages manifest ]
        forM_ expected $ \(name, size, digest) →
            case [ d | d ← listed, pdName d ≡ name ] of
                [] → Left (MapArtifactSubstituted name "the entry record does not list it")
                (d : _)
                    | pdSize d ≢ size ∨ pdDigest d ≢ digest →
                        Left (MapArtifactSubstituted name
                                "the entry record gives a different size or digest")
                    | otherwise → Right ()
        let names = Set.fromList [ n | (n, _, _) ← expected ]
        forM_ listed $ \d →
            unless (Set.member (pdName d) names) $
                Left (MapArtifactSubstituted (pdName d)
                        "the entry record lists a map file the manifest does not")

-- | Recover the root level's complete raster — 'mpiRootPlan''s width
--   and height, no gutter — from the root level's decoded mandatory
--   pages: strip each page's one-texel gutter, tile the payloads by
--   (page-u, page-v), and crop the partial-edge padding. The root is
--   stored ONLY as these pages; there is no separate whole-root payload.
reassembleMapRoot
    ∷ MapManifest → [(MapPageKey, BS.ByteString)] → Either MapArtifactRefusal BS.ByteString
reassembleMapRoot m decoded = do
    inv ← mapArtifactInventory (mmWorldSize m)
    pagePlan ← either (Left . MapArtifactImage) Right mapPagePlan
    let root = mpiRootLevel inv
        plan = mpiRootPlan inv
        geom = mpiGeometry inv
        width = mipWidth plan
        height = mipHeight plan
        pagesU = mapLevelPagesU geom root
        pagesV = mapLevelPagesV geom root
    pageBytes ← forM [ (pu, pv) | pv ← [0 .. pagesV - 1], pu ← [0 .. pagesU - 1] ] $
        \(pu, pv) → do
            let key = MapPageKey root pu pv
            case [ b | (k, b) ← decoded, k ≡ key ] of
                [] → Left (MapArtifactMissingRequired (RequiredPage key))
                (b : _) → do
                    either (Left . MapArtifactImage) Right $
                        checkUploadPayload pagePlan (BS.length b)
                    pure ((pu, pv), b)
    let pageAt pu pv = fromMaybe BS.empty (L.lookup (pu, pv) pageBytes)
        segment y pu =
            let pv = y `div` mapPagePayload
                iy = y `mod` mapPagePayload
                x0 = pu * mapPagePayload
                w  = min mapPagePayload (width - x0)
                offset = ((iy + mapPageGutter) * mapPageEdge + mapPageGutter) * 4
            in BS.take (w * 4) (BS.drop offset (pageAt pu pv))
        bytes = BS.concat [ segment y pu | y ← [0 .. height - 1], pu ← [0 .. pagesU - 1] ]
    either (Left . MapArtifactImage) Right $ checkUploadPayload plan (BS.length bytes)
    pure bytes

-- * Fine pages

-- | The result of reading a fine page. Every problem is a miss; a miss
--   never invalidates mandatory coverage.
data FinePageRead
    = FinePageHit !BS.ByteString
      -- ^ The page's exact decoded RGBA8 bytes.
    | FinePageMiss !FinePageMiss
    deriving (Eq, Show)

data FinePageMiss
    = FinePageAbsent
    | FinePageInvalid !MapArtifactRefusal
      -- ^ Present but unusable — damaged, another world's, another
      --   producer's, the wrong key. Discard and regenerate.
    deriving (Eq, Show)

-- | A fine-page request is only well-formed for a valid key on a level
--   below the cutoff. Anything else is the caller's error, not a miss.
checkFineKey ∷ Int → MapPageKey → Either MapArtifactRefusal ()
checkFineKey worldSize key = do
    inv ← mapArtifactInventory worldSize
    either (Left . MapArtifactInvalidKey key . mapAddressRefusalText) Right
        (checkMapPageKey (mpiGeometry inv) key)
    when (mpkLevel key ≥ mpiCoarseCutoffLevel inv) $
        Left (MapArtifactNotFineLevel key (mpiCoarseCutoffLevel inv))

-- | Write one fine page into @dir@ under its canonical name, replacing
--   any previous copy by a single rename. Returns the path written.
--   Refuses a @dir@ that is, or whose parent is, a symlink.
writeFinePage
    ∷ FilePath → GeneratedWorldId → MapCompatibility → Int → MapPageKey
    → BS.ByteString → IO (Either MapArtifactRefusal FilePath)
writeFinePage dir gid compat worldSize key rgba =
    case checkFineKey worldSize key ≫ encodePagePng (mapPageFileName key) rgba of
        Left r → pure (Left r)
        Right png → do
            let file = encodeMapPageFile (MapPageBinding gid compat key) png
                final = dir </> T.unpack (mapPageFileName key)
            safe ← rejectSymlinkedManagedPath dir
            isDir ← doesDirectoryExist dir
            case safe of
                Left why → pure (Left (MapArtifactIO dir why))
                Right ()
                    | not isDir → pure (Left (MapArtifactIO dir "not a directory"))
                    | otherwise → do
                        written ← try $ do
                            (tmp, h) ← openBinaryTempFile dir (takeFileName final <> ".tmp")
                            BS.hPut h file
                            hFlush h
                            hClose h
                            renameFile tmp final
                        pure $ case written of
                            Left (e ∷ IOException) → Left (MapArtifactIO final (tshow e))
                            Right () → Right final

-- | Read one fine page from @dir@. 'Left' only for a malformed request
--   ('checkFineKey'); everything else is a hit or a miss. A @dir@ that
--   is, or whose parent is, a symlink is never read through: that too
--   is a miss ('FinePageInvalid').
readFinePage
    ∷ FilePath → GeneratedWorldId → MapCompatibility → Int → MapPageKey
    → IO (Either MapArtifactRefusal FinePageRead)
readFinePage dir gid compat worldSize key =
    case checkFineKey worldSize key of
        Left r → pure (Left r)
        Right () → rejectSymlinkedManagedPath dir ≫= \case
          Left why → pure (Right (FinePageMiss (FinePageInvalid (MapArtifactIO dir why))))
          Right () → do
            let what = "fine page " <> mapPageFileName key
            read' ← readBounded what (dir </> T.unpack (mapPageFileName key))
                                mapPageFileMaxBytes
            Right ⊚ case read' of
                Left r → pure (FinePageMiss (FinePageInvalid r))
                Right Nothing → pure (FinePageMiss FinePageAbsent)
                Right (Just bytes) →
                    case decodeMapPageFile (MapPageBinding gid compat key) bytes of
                        Left r → pure (FinePageMiss (FinePageInvalid r))
                        Right png → do
                            decoded ← decodePagePng what png
                            pure (either (FinePageMiss . FinePageInvalid) FinePageHit decoded)

-- * Bounded reads

-- | Read a file of at most @bound@ bytes. 'Nothing' when it does not
--   exist. The size is checked BEFORE reading, and the read itself asks
--   for at most one byte more than the bound, so a file that grows
--   between the check and the read still cannot make this allocate past
--   it. A symlink is refused, never followed.
readBounded ∷ Text → FilePath → Int → IO (Either MapArtifactRefusal (Maybe BS.ByteString))
readBounded what path bound = readChecked what path bound (const (Right ()))

-- | 'readBounded' with an extra check of the file's size, run before the
--   format bound and before any byte is read.
readChecked
    ∷ Text → FilePath → Int → (Integer → Either MapArtifactRefusal ())
    → IO (Either MapArtifactRefusal (Maybe BS.ByteString))
readChecked what path bound sizeCheck = do
    safe ← rejectSymlinkedPath path
    case safe of
        Left why → pure (Left (MapArtifactIO path why))
        Right () → do
            result ← try $ do
                exists ← doesFileExist path
                if not exists then pure (Right Nothing) else do
                    size ← getFileSize path
                    either (pure . Left) (const (readUpTo size)) (sizeCheck size)
            pure $ case result of
                Left (e ∷ IOException) → Left (MapArtifactIO path (tshow e))
                Right r → r
  where
    readUpTo size = if size > toInteger bound
        then pure (Left (MapArtifactOversized what (toInteger bound) size))
        else do
            bytes ← withBinaryFile path ReadMode (\h → BS.hGet h (bound + 1))
            pure $ if BS.length bytes > bound
                then Left (MapArtifactOversized what (toInteger bound)
                                               (toInteger (BS.length bytes)))
                else Right (Just bytes)

-- | A mandatory page file must be exactly its manifest-declared length.
checkSize ∷ Text → Integer → Integer → Either MapArtifactRefusal ()
checkSize what declared size
    | size < declared = Left (MapArtifactTruncated what declared size)
    | size > declared = Left (MapArtifactLengthMismatch what declared size)
    | otherwise = Right ()
