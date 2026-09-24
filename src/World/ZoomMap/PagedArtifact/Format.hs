{-# LANGUAGE Strict #-}
-- | The paged map artifact's two byte layouts (issue #2693): the
--   manifest and the page file. Pure — nothing here touches the
--   filesystem — so every acceptance and refusal rule is testable on
--   bytes alone and shared by the writer and every reader.
--
--   Both files are framed the same way: an 8-byte magic, a big-endian
--   'Word32' format version, then the fields, then a trailing SHA-256 of
--   everything before it. A reader checks, in this order and before
--   decoding a single field: length against the format bound, magic,
--   version, declared length against actual length, trailer. So absence,
--   an unknown version, truncation and bit damage are four different
--   refusals rather than one parse failure.
--
--   Every integer is big-endian and fixed-width; every list is written
--   in the canonical order its reader requires. Identical inputs
--   therefore serialise to identical bytes, and a manifest's page order
--   is 'MapPageKey''s derived order — (level, page-u, page-v) —
--   whatever order the pages were supplied in.
module World.ZoomMap.PagedArtifact.Format
    ( -- * Coverage
      mandatoryPageKeys
    , mapArtifactInventory
      -- * Page files
    , MapPageBinding(..)
    , encodeMapPageFile
    , decodeMapPageFile
      -- * Manifests
    , MapManifest(..)
    , MapManifestPage(..)
    , buildMapManifest
    , encodeMapManifest
    , decodeMapManifest
    , validateMapManifest
    ) where

import UPrelude
import qualified Crypto.Hash.SHA256 as SHA256
import qualified Data.ByteString as BS
import qualified Data.List as L
import qualified Data.Serialize as S
import qualified Data.Serialize.Get as G
import qualified Data.Serialize.Put as P
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import World.Map.ImagePlan (MapImagePlan(..))
import World.Page.GeneratedId (GeneratedWorldId)
import World.ZoomMap.Pyramid.Address
import World.ZoomMap.Pyramid.Inventory
import World.ZoomMap.PagedArtifact.Png (checkPagePngHeader)
import World.ZoomMap.PagedArtifact.Types

-- * Coverage

-- | The pyramid inventory for a recorded world size.
mapArtifactInventory ∷ Int → Either MapArtifactRefusal MapPyramidInventory
mapArtifactInventory = either (Left . MapArtifactGeometry) Right . mapPyramidInventory

-- | Every page the mandatory artifact must hold — cutoff level through
--   root, inclusive, as 'mapPyramidMandatoryLevels' selects them — in
--   canonical order. Enumerates only mandatory levels, whose page count
--   is bounded independently of world size; fine levels are never
--   listed.
mandatoryPageKeys ∷ MapPyramidInventory → [MapPageKey]
mandatoryPageKeys inv =
    [ MapPageKey (mplLevel lvl) pu pv
    | lvl ← mapPyramidMandatoryLevels inv
    , pu ← [0 .. mplPagesU lvl - 1]
    , pv ← [0 .. mplPagesV lvl - 1] ]

-- * Page files

-- | What a page file is bound to: its world, the producer
--   compatibility, and its key. The dimensions and encoding are format
--   constants and are checked too.
data MapPageBinding = MapPageBinding
    { mpbWorld  ∷ !GeneratedWorldId
    , mpbCompat ∷ !MapCompatibility
    , mpbKey    ∷ !MapPageKey
    } deriving (Eq, Show)

pageMagic, manifestMagic ∷ BS.ByteString
pageMagic     = "SYNMAPPG"
manifestMagic = "SYNMAPMF"

-- | Wrap an already encoded page PNG in its bound page file. The PNG is
--   whatever 'World.ZoomMap.PagedArtifact.Png.encodePagePng' produced;
--   the caller has already checked it against 'mapPagePngMaxBytes'.
encodeMapPageFile ∷ MapPageBinding → BS.ByteString → BS.ByteString
encodeMapPageFile b png = withTrailer $ P.runPut $ do
    P.putByteString pageMagic
    P.putWord32be mapArtifactFormatVersion
    S.put (mpbWorld b)
    putCompat (mpbCompat b)
    putKey (mpbKey b)
    P.putWord32be (fromIntegral mapPageEdge)
    P.putWord32be (fromIntegral mapPageEdge)
    P.putWord8 mapPageEncodingPng
    P.putWord64be (fromIntegral (BS.length png))
    P.putByteString png

-- | Check a page file against the binding it is being read as, and
--   return its PNG — whose signature and IHDR have been checked, but
--   whose pixels have NOT been decoded. Refuses, in order: a file too
--   short or too long for the format, the wrong magic, an unknown
--   version, a declared PNG length over the bound, a total length that
--   disagrees with the declaration, a failed trailer, the wrong world,
--   an incompatible producer, the wrong key, dimensions or encoding,
--   and a PNG header that is not a page's.
decodeMapPageFile ∷ MapPageBinding → BS.ByteString → Either MapArtifactRefusal BS.ByteString
decodeMapPageFile b bytes = do
    let what = "page file " <> mapPageFileName (mpbKey b)
        actual = BS.length bytes
    when (actual > mapPageFileMaxBytes) $
        Left $ MapArtifactOversized what (toInteger mapPageFileMaxBytes) (toInteger actual)
    checkPrefix what pageMagic bytes
    when (actual < mapPageHeaderBytes) $
        Left $ MapArtifactTruncated what (toInteger (mapPageHeaderBytes + mapPageTrailerBytes))
                                         (toInteger actual)
    (world, compat, key, width, height, encoding, pngLen) ←
        parseWith what (BS.take (mapPageHeaderBytes - 12) (BS.drop 12 bytes)) getPageHeader
    when (pngLen > toInteger mapPagePngMaxBytes) $
        Left $ MapArtifactOversized (what <> " PNG") (toInteger mapPagePngMaxBytes) pngLen
    let expected = toInteger (mapPageHeaderBytes + mapPageTrailerBytes) + pngLen
    checkTotal what expected actual
    checkTrailer what bytes
    checkWorld (mpbWorld b) world
    checkCompat (mpbCompat b) compat
    let bindFail = Left . MapArtifactPageBinding (mpbKey b)
    when (key ≢ mpbKey b) $ bindFail ("names page " <> tshow key)
    when (width ≢ fromIntegral mapPageEdge ∨ height ≢ fromIntegral mapPageEdge) $
        bindFail ("declares " <> tshow width <> "×" <> tshow height)
    when (encoding ≢ mapPageEncodingPng) $
        bindFail ("declares unsupported encoding " <> tshow encoding)
    let png = BS.take (fromInteger pngLen) (BS.drop mapPageHeaderBytes bytes)
    checkPagePngHeader what png
    pure png
  where
    getPageHeader = do
        world ← S.get
        compat ← getCompat
        key ← getKey
        width ← G.getWord32be
        height ← G.getWord32be
        encoding ← G.getWord8
        pngLen ← G.getWord64be
        pure (world, compat, key, width, height, encoding, toInteger pngLen)

-- * Manifests

-- | One page entry: its key, its canonical file name, and the exact
--   length and SHA-256 of the whole PAGE FILE — the same size and
--   digest the generated-world library records for that payload file in
--   its 'World.GeneratedLibrary.PayloadDescriptor', so a reader can
--   cross-check the two.
data MapManifestPage = MapManifestPage
    { mmpKey       ∷ !MapPageKey
    , mmpName      ∷ !Text
    , mmpFileBytes ∷ !Word64
    , mmpDigest    ∷ !BS.ByteString
    } deriving (Eq, Show)

-- | The decoded manifest. The geometry the wire also carries — level
--   numbering, dimensions, page counts, the root plan, the encoding —
--   is DERIVED from 'mmWorldSize' and checked on read, so it is not
--   duplicated here.
data MapManifest = MapManifest
    { mmWorld     ∷ !GeneratedWorldId
    , mmCompat    ∷ !MapCompatibility
    , mmWorldSize ∷ !Int
    , mmPages     ∷ ![MapManifestPage]
      -- ^ Canonical key order, exactly the mandatory coverage.
    } deriving (Eq, Show)

-- | Build the manifest for a set of encoded page files, in canonical
--   order whatever order they were supplied in, and validate it.
buildMapManifest
    ∷ GeneratedWorldId → MapCompatibility → Int → [(MapPageKey, BS.ByteString)]
    → Either MapArtifactRefusal MapManifest
buildMapManifest gid compat worldSize files = do
    let pages = L.sortOn mmpKey
            [ MapManifestPage key (mapPageFileName key)
                  (fromIntegral (BS.length bytes)) (SHA256.hash bytes)
            | (key, bytes) ← files ]
        m = MapManifest gid compat worldSize pages
    _ ← validateMapManifest m
    pure m

-- | Every rule a manifest must satisfy beyond its framing: a
--   supported world size, then per entry a valid key, canonical order
--   without repeats, the canonical name, a plausible file length and a
--   32-byte digest, then exact mandatory coverage. Returns the
--   inventory it validated against.
validateMapManifest ∷ MapManifest → Either MapArtifactRefusal MapPyramidInventory
validateMapManifest m = do
    inv ← mapArtifactInventory (mmWorldSize m)
    let geom = mpiGeometry inv
    forM_ (mmPages m) $ \p → do
        let key = mmpKey p
            what = "manifest entry " <> mapPageFileName key
        either (Left . MapArtifactInvalidKey key . mapAddressRefusalText) Right
            (checkMapPageKey geom key)
        when (mmpName p ≢ mapPageFileName key) $
            Left $ MapArtifactConflictingName key (mapPageFileName key) (mmpName p)
        when (toInteger (mmpFileBytes p) > toInteger mapPageFileMaxBytes) $
            Left $ MapArtifactOversized what (toInteger mapPageFileMaxBytes)
                                             (toInteger (mmpFileBytes p))
        when (toInteger (mmpFileBytes p) < toInteger minimumPageFile) $
            Left $ MapArtifactMalformed what "declares a length too short for a page file"
        when (BS.length (mmpDigest p) ≢ 32) $
            Left $ MapArtifactMalformed what "carries a digest that is not 32 bytes"
    forM_ (zip (mmPages m) (drop 1 (mmPages m))) $ \(a, b) →
        case compare (mmpKey a) (mmpKey b) of
            LT → Right ()
            EQ → Left (MapArtifactDuplicateKey (mmpKey b))
            GT → Left (MapArtifactUnorderedKeys (mmpKey a) (mmpKey b))
    let required = mandatoryPageKeys inv
        present  = Set.fromList (map mmpKey (mmPages m))
        wanted   = Set.fromList required
    case [ k | k ← required, not (Set.member k present) ] of
        (k : _) → Left (MapArtifactMissingRequired (RequiredPage k))
        []      → Right ()
    case [ mmpKey p | p ← mmPages m, not (Set.member (mmpKey p) wanted) ] of
        (k : _) → Left (MapArtifactUnexpectedPage k)
        []      → Right ()
    pure inv
  where
    minimumPageFile = mapPageHeaderBytes + 33 + mapPageTrailerBytes

-- | Serialise a manifest. Refuses one 'validateMapManifest' refuses, so
--   the writer cannot produce a file its own reader rejects.
encodeMapManifest ∷ MapManifest → Either MapArtifactRefusal BS.ByteString
encodeMapManifest m = do
    inv ← validateMapManifest m
    when (toInteger (mmWorldSize m) > toInteger (maxBound ∷ Word32)) $
        Left $ MapArtifactMalformed "manifest" "world size does not fit its Word32 field"
    let root = mpiRootPlan inv
        levels = mapPyramidMandatoryLevels inv
        body = P.runPut $ do
            S.put (mmWorld m)
            putCompat (mmCompat m)
            P.putWord32be (fromIntegral (mmWorldSize m))
            P.putWord8 mapPageEncodingPng
            mapM_ (P.putWord32be . fromIntegral)
                [ mapPageEdge, mapPagePayload, mapPageGutter
                , mpiCoarseCutoffLevel inv, mpiRootLevel inv
                , mipWidth root, mipHeight root ]
            P.putWord32be (fromIntegral (length levels))
            forM_ levels $ \l → mapM_ (P.putWord32be . fromIntegral)
                [ mplLevel l, mplWidth l, mplHeight l, mplPagesU l, mplPagesV l ]
            P.putWord32be (fromIntegral (length (mmPages m)))
            forM_ (mmPages m) $ \p → do
                putKey (mmpKey p)
                let name = TE.encodeUtf8 (mmpName p)
                P.putWord16be (fromIntegral (BS.length name))
                P.putByteString name
                P.putWord64be (mmpFileBytes p)
                P.putByteString (mmpDigest p)
        total = 8 + 4 + 8 + BS.length body + 32
        bytes = withTrailer $ P.runPut $ do
            P.putByteString manifestMagic
            P.putWord32be mapArtifactFormatVersion
            P.putWord64be (fromIntegral total)
            P.putByteString body
    when (BS.length bytes > mapManifestMaxBytes) $
        Left $ MapArtifactOversized "manifest" (toInteger mapManifestMaxBytes)
                                               (toInteger (BS.length bytes))
    pure bytes

-- | Decode and check a manifest against the world and compatibility the
--   caller expects. Self-consistency is not enough: a manifest for
--   another world, or from another producer, is refused by name.
decodeMapManifest
    ∷ GeneratedWorldId → MapCompatibility → BS.ByteString
    → Either MapArtifactRefusal MapManifest
decodeMapManifest gid compat bytes = do
    let what = "manifest"
        actual = BS.length bytes
    when (actual > mapManifestMaxBytes) $
        Left $ MapArtifactOversized what (toInteger mapManifestMaxBytes) (toInteger actual)
    checkPrefix what manifestMagic bytes
    when (actual < 20) $ Left $ MapArtifactTruncated what 52 (toInteger actual)
    declared ← parseWith what (BS.take 8 (BS.drop 12 bytes)) G.getWord64be
    when (toInteger declared > toInteger mapManifestMaxBytes) $
        Left $ MapArtifactOversized what (toInteger mapManifestMaxBytes) (toInteger declared)
    when (declared < 52) $
        Left $ MapArtifactMalformed what "declares a length shorter than its framing"
    checkTotal what (toInteger declared) actual
    checkTrailer what bytes
    let body = BS.take (actual - 20 - 32) (BS.drop 20 bytes)
    (world, recordedCompat, rest) ← parseWith what body $ do
        w ← S.get
        c ← getCompat
        r ← G.remaining ≫= G.getBytes
        pure (w, c, r)
    checkWorld gid world
    checkCompat compat recordedCompat
    (worldSize, geometry, levels, pages) ← parseWith what rest getManifestBody
    let m = MapManifest world recordedCompat worldSize pages
    inv ← validateMapManifest m
    let root = mpiRootPlan inv
        derivedGeometry =
            [ fromIntegral mapPageEncodingPng, mapPageEdge, mapPagePayload, mapPageGutter
            , mpiCoarseCutoffLevel inv, mpiRootLevel inv
            , mipWidth root, mipHeight root ]
        derivedLevels =
            [ [mplLevel l, mplWidth l, mplHeight l, mplPagesU l, mplPagesV l]
            | l ← mapPyramidMandatoryLevels inv ]
    unless (geometry ≡ derivedGeometry) $
        Left $ MapArtifactGeometryMismatch $
            "encoding/page/level/root fields " <> tshow geometry
            <> " where world size " <> tshow worldSize <> " derives "
            <> tshow derivedGeometry
    unless (levels ≡ derivedLevels) $
        Left $ MapArtifactGeometryMismatch $
            "level table " <> tshow levels <> " where world size "
            <> tshow worldSize <> " derives " <> tshow derivedLevels
    pure m

-- | The manifest body after the world and compatibility: counts are
--   bounded against the bytes that remain BEFORE any list is built.
getManifestBody ∷ G.Get (Int, [Int], [[Int]], [MapManifestPage])
getManifestBody = do
    worldSize ← word32
    encoding ← fromIntegral ⊚ G.getWord8
    geometry ← replicateM 7 word32
    levelCount ← word32
    boundedCount "level" levelCount 20 64
    levels ← replicateM levelCount (replicateM 5 word32)
    pageCount ← word32
    boundedCount "page" pageCount (12 + 2 + 8 + 32) mapManifestMaxPages
    pages ← replicateM pageCount $ do
        key ← getKey
        nameLen ← fromIntegral ⊚ G.getWord16be
        when (nameLen > 128) $ fail "page name longer than 128 bytes"
        nameBytes ← G.getBytes nameLen
        name ← either (const (fail "page name is not UTF-8")) pure
                      (TE.decodeUtf8' nameBytes)
        len ← G.getWord64be
        digest ← G.getBytes 32
        pure (MapManifestPage key name len digest)
    done ← G.isEmpty
    unless done $ fail "trailing bytes after the last page entry"
    pure (worldSize, encoding : geometry, levels, pages)
  where
    word32 = fromIntegral ⊚ G.getWord32be
    boundedCount what n minBytes cap = do
        left ← G.remaining
        when (n > cap ∨ n * minBytes > left) $
            fail (what <> " count " <> show n <> " exceeds its bound")

-- * Shared framing

withTrailer ∷ BS.ByteString → BS.ByteString
withTrailer bytes = bytes <> SHA256.hash bytes

-- | Magic, then version — checked before any length rule, so a foreign
--   file and a file from a newer format are named as such.
checkPrefix ∷ Text → BS.ByteString → BS.ByteString → Either MapArtifactRefusal ()
checkPrefix what magic bytes = do
    when (BS.length bytes < 12) $
        if BS.isPrefixOf bytes magic ∨ BS.isPrefixOf magic bytes
            then Left (MapArtifactTruncated what 12 (toInteger (BS.length bytes)))
            else Left (MapArtifactNotRecognised what)
    unless (BS.take 8 bytes ≡ magic) $ Left (MapArtifactNotRecognised what)
    version ← parseWith what (BS.take 4 (BS.drop 8 bytes)) G.getWord32be
    unless (version ≡ mapArtifactFormatVersion) $
        Left (MapArtifactUnknownVersion what version)

checkTotal ∷ Text → Integer → Int → Either MapArtifactRefusal ()
checkTotal what expected actual
    | toInteger actual < expected = Left (MapArtifactTruncated what expected (toInteger actual))
    | toInteger actual > expected = Left (MapArtifactLengthMismatch what expected (toInteger actual))
    | otherwise = Right ()

checkTrailer ∷ Text → BS.ByteString → Either MapArtifactRefusal ()
checkTrailer what bytes =
    let n = BS.length bytes - 32
    in unless (SHA256.hash (BS.take n bytes) ≡ BS.drop n bytes) $
           Left (MapArtifactChecksumMismatch what)

checkWorld ∷ GeneratedWorldId → GeneratedWorldId → Either MapArtifactRefusal ()
checkWorld expected found =
    unless (expected ≡ found) $ Left (MapArtifactWrongWorld expected found)

checkCompat ∷ MapCompatibility → MapCompatibility → Either MapArtifactRefusal ()
checkCompat expected found =
    forM_ mapCompatFields $ \(field, get') →
        unless (get' expected ≡ get' found) $
            Left (MapArtifactIncompatible field (get' expected) (get' found))

parseWith ∷ Text → BS.ByteString → G.Get α → Either MapArtifactRefusal α
parseWith what bytes g =
    either (Left . MapArtifactMalformed what . T.pack) Right (G.runGet g bytes)

putCompat ∷ MapCompatibility → P.Put
putCompat c = mapM_ (\(_, f) → P.putWord32be (f c)) mapCompatFields

getCompat ∷ G.Get MapCompatibility
getCompat = MapCompatibility ⊚ G.getWord32be ⊛ G.getWord32be ⊛ G.getWord32be ⊛ G.getWord32be

putKey ∷ MapPageKey → P.Put
putKey (MapPageKey l pu pv) = mapM_ (P.putWord32be . fromIntegral) [l, pu, pv]

-- | Keys are read as unsigned and widened, so no wire value can become
--   a negative 'Int'; range is 'checkMapPageKey''s to judge.
getKey ∷ G.Get MapPageKey
getKey = do
    l ← G.getWord32be
    pu ← G.getWord32be
    pv ← G.getWord32be
    pure (MapPageKey (fromIntegral l) (fromIntegral pu) (fromIntegral pv))
