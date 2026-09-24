{-# LANGUAGE Strict #-}
-- | Vocabulary of the versioned paged-map artifact (issue #2693,
--   world-map epic #2017, design slice WML-7; decisions D-5, D-7, D-8,
--   D-12, D-17, D-18).
--
--   This module owns the format's constants, the compatibility
--   descriptor a reader compares against, and the one structured
--   refusal type every reader and writer returns. The byte layouts are
--   "World.ZoomMap.PagedArtifact.Format"'s, the codec is
--   "World.ZoomMap.PagedArtifact.Png"'s, and the library integration is
--   "World.ZoomMap.PagedArtifact.Store"'s. The full contract is written
--   up in @docs/world_map_paged_artifact_format.md@.
module World.ZoomMap.PagedArtifact.Types
    ( -- * Compatibility
      MapCompatibility(..)
    , currentMapCompatibility
    , MapCompatField(..)
    , mapCompatFields
    , mapCompatFieldText
      -- * Format constants
    , mapArtifactFormatVersion
    , mapPageEncodingPng
    , mapManifestFileName
    , mapPageFilePrefix
    , mapPageFileName
    , isMapOwnedFileName
    , mapPageHeaderBytes
    , mapPageTrailerBytes
    , mapPagePngMaxBytes
    , mapPageFileMaxBytes
    , mapManifestMaxBytes
    , mapManifestMaxPages
    , mapPagePlan
      -- * Refusals
    , MapRequiredItem(..)
    , MapArtifactRefusal(..)
    , mapArtifactRefusalText
    ) where

import UPrelude
import qualified Data.Text as T
import World.GeneratedLibrary (LibraryFailure, renderLibraryFailure)
import World.Map.ImagePlan
    ( MapImageFormat(..), MapImagePlan, MapImageRefusal
    , MapImageSource(..), mapImageRefusalText, planMapImage )
import World.Page.GeneratedId (GeneratedWorldId, renderGeneratedWorldId)
import World.ZoomMap.Pyramid.Address (MapPageKey(..), mapPageEdge)
import World.ZoomMap.Pyramid.Inventory (MapPyramidRefusal, mapPyramidRefusalText)

-- * Compatibility

-- | Which producer an artifact's pixels came from, recorded SEPARATELY
--   from the world's identity ('GeneratedWorldId') and from byte
--   integrity (the SHA-256 values). A reader computes the value it
--   expects with 'currentMapCompatibility' — before any artifact is
--   opened, and without reference to any world — and every field is
--   compared by exact equality. A field that differs is reported by
--   name ('MapArtifactIncompatible'), never as corruption.
--
--   Each field is a hand-bumped version, not a digest of build inputs,
--   so an ordinary rebuild does NOT invalidate persisted artifacts. The
--   bump rules are the format document's; in short:
--
--   * generator — bump in the same change as any world-generation
--     output change that moves page pixels (the change that rebaselines
--     @tools/baselines/@ or the #2298 finest-page goldens).
--   * content — bump when authored content the page pixels read changes
--     meaningfully: material or vegetation definitions, or the
--     zoom-map/vegetation textures the palette samples.
--   * palette — bump when the colour derivation changes
--     ("World.ZoomMap.ColorPalette", the per-chunk tile colouring).
--   * map schema — bump when page SEMANTICS change: the addressing,
--     page geometry, gutter rule or reduction ("World.ZoomMap.Pyramid").
data MapCompatibility = MapCompatibility
    { mcGenerator ∷ !Word32
    , mcContent   ∷ !Word32
    , mcPalette   ∷ !Word32
    , mcMapSchema ∷ !Word32
    } deriving (Eq, Show)

-- | The compatibility this build produces and accepts. Pure and
--   world-independent, so load-time recovery (WML-9) can compute the
--   expected descriptor before it opens anything.
currentMapCompatibility ∷ MapCompatibility
currentMapCompatibility = MapCompatibility
    { mcGenerator = 1
    , mcContent   = 1
    , mcPalette   = 1
    , mcMapSchema = 1
    }

-- | One compatibility field, for a refusal that names it.
data MapCompatField
    = CompatGenerator
    | CompatContent
    | CompatPalette
    | CompatMapSchema
    deriving (Eq, Ord, Show, Enum, Bounded)

-- | The fields in wire order, with their accessors.
mapCompatFields ∷ [(MapCompatField, MapCompatibility → Word32)]
mapCompatFields =
    [ (CompatGenerator, mcGenerator)
    , (CompatContent,   mcContent)
    , (CompatPalette,   mcPalette)
    , (CompatMapSchema, mcMapSchema) ]

mapCompatFieldText ∷ MapCompatField → Text
mapCompatFieldText CompatGenerator = "generator"
mapCompatFieldText CompatContent   = "content"
mapCompatFieldText CompatPalette   = "palette"
mapCompatFieldText CompatMapSchema = "map schema"

-- * Format constants

-- | The wire version of BOTH record formats this slice defines — the
--   manifest and the page file. A file whose magic matches but whose
--   version is not this one is 'MapArtifactUnknownVersion', which a
--   caller can tell apart from damage and from absence. Bump it for any
--   byte-layout change; a semantic change to the pixels is a
--   'MapCompatibility' bump instead.
mapArtifactFormatVersion ∷ Word32
mapArtifactFormatVersion = 1

-- | The only page encoding: a lossless 8-bit RGBA PNG (the owner's
--   #2303 decision), stored as the page file's payload.
mapPageEncodingPng ∷ Word8
mapPageEncodingPng = 1

-- | The manifest's payload name inside a library entry.
mapManifestFileName ∷ Text
mapManifestFileName = "map.manifest"

-- | Every page file's name starts with this.
mapPageFilePrefix ∷ Text
mapPageFilePrefix = "map-page-"

-- | The ONE canonical file name for a page: derived from its key, never
--   supplied. A manifest recording any other name for a key is refused
--   ('MapArtifactConflictingName'). Zero-padded so names sort like keys
--   at every supported size; a wider index simply prints more digits, so
--   the mapping stays injective past the padding.
mapPageFileName ∷ MapPageKey → Text
mapPageFileName (MapPageKey l pu pv) =
    mapPageFilePrefix <> "l" <> pad 2 l <> "-u" <> pad 5 pu
        <> "-v" <> pad 5 pv <> ".mappage"
  where
    pad n x = let s = tshow x in T.replicate (n - T.length s) "0" <> s

-- | Whether a payload name belongs to the map artifact. Other names in
--   the same library entry (a later slice's base chunks, D-18) are not
--   the map's to judge.
isMapOwnedFileName ∷ Text → Bool
isMapOwnedFileName name =
    name ≡ mapManifestFileName ∨ mapPageFilePrefix `T.isPrefixOf` name

-- | A page file's fixed header: magic 8, version 4, world id 16,
--   compatibility 16, key 12, dimensions 8, encoding 1, PNG length 8.
mapPageHeaderBytes ∷ Int
mapPageHeaderBytes = 73

-- | A page file's trailing SHA-256 over header and PNG.
mapPageTrailerBytes ∷ Int
mapPageTrailerBytes = 32

-- | The largest PNG a page file may carry. A 514-square RGBA8 page's
--   filtered scanlines are 1,057,298 bytes; stored (uncompressed)
--   deflate blocks, the zlib wrapper and PNG chunk framing add well under
--   one kilobyte to that, so 2 MiB bounds even an incompressible page
--   with a wide margin. Enforced on the declared length BEFORE the bytes
--   are read and again on write.
mapPagePngMaxBytes ∷ Int
mapPagePngMaxBytes = 2 * 1024 * 1024

-- | The largest page file: header, the largest PNG, trailer.
mapPageFileMaxBytes ∷ Int
mapPageFileMaxBytes = mapPageHeaderBytes + mapPagePngMaxBytes + mapPageTrailerBytes

-- | The largest manifest a reader will read. Mandatory coverage is
--   bounded independently of world size — the cutoff level's longest
--   axis is at most 4096 texels, so it has at most 8 × 8 pages, and the
--   root at most 4 × 4 — and one page entry is under 100 bytes, so this
--   is generous. Checked against the file size before the file is read.
mapManifestMaxBytes ∷ Int
mapManifestMaxBytes = 64 * 1024

-- | The most page entries a manifest may declare (see
--   'mapManifestMaxBytes'). Checked before any entry is decoded.
mapManifestMaxPages ∷ Int
mapManifestMaxPages = 256

-- | The accepted plan every page decodes to: one 'mapPageEdge'-square
--   RGBA8 tile, exactly as "World.ZoomMap.Pyramid.Inventory" prices it.
mapPagePlan ∷ Either MapImageRefusal MapImagePlan
mapPagePlan = planMapImage MapImageRGBA8 (TiledImageSource 1 mapPageEdge)

-- * Refusals

-- | A required item whose absence is being reported.
data MapRequiredItem
    = RequiredManifest
    | RequiredPage !MapPageKey
    deriving (Eq, Show)

-- | Why an artifact, or one of its files, was refused. Absence
--   ('MapArtifactAbsent', 'MapArtifactMissingRequired'), an unknown
--   version, an incompatible identity, damage (truncation, checksum,
--   decode) and a malformed declaration are separate constructors, so a
--   caller — WML-9's recovery — can decide regeneration versus refusal
--   by pattern, never by message text.
data MapArtifactRefusal
    = MapArtifactAbsent !GeneratedWorldId
      -- ^ The library holds no entry directory for this world.
    | MapArtifactMissingRequired !MapRequiredItem
      -- ^ A mandatory file is absent, or the manifest omits a page the
      --   mandatory coverage requires.
    | MapArtifactUnexpectedPage !MapPageKey
      -- ^ A page outside mandatory coverage where only mandatory pages
      --   belong.
    | MapArtifactNotFineLevel !MapPageKey !Int
      -- ^ A fine-page operation on a mandatory level; the cutoff level.
    | MapArtifactUnknownVersion !Text !Word32
      -- ^ What was read, and the version it declares.
    | MapArtifactNotRecognised !Text
      -- ^ The file does not carry the expected magic.
    | MapArtifactTruncated !Text !Integer !Integer
      -- ^ What, the length it must have, the length it has.
    | MapArtifactLengthMismatch !Text !Integer !Integer
      -- ^ What, the declared length, a LONGER actual length.
    | MapArtifactOversized !Text !Integer !Integer
      -- ^ What, the format bound, the declared or actual length.
    | MapArtifactChecksumMismatch !Text
    | MapArtifactWrongWorld !GeneratedWorldId !GeneratedWorldId
      -- ^ Expected, found.
    | MapArtifactIncompatible !MapCompatField !Word32 !Word32
      -- ^ The field, expected, found.
    | MapArtifactGeometry !MapPyramidRefusal
      -- ^ The recorded world size is not one the pyramid can address.
    | MapArtifactGeometryMismatch !Text
      -- ^ A recorded geometry field disagrees with the geometry derived
      --   from the recorded world size.
    | MapArtifactInvalidKey !MapPageKey !Text
    | MapArtifactDuplicateKey !MapPageKey
    | MapArtifactUnorderedKeys !MapPageKey !MapPageKey
      -- ^ Two consecutive entries, the second not after the first.
    | MapArtifactConflictingName !MapPageKey !Text !Text
      -- ^ Key, the canonical name, the recorded name.
    | MapArtifactPageBinding !MapPageKey !Text
      -- ^ A page file whose own header names a different key,
      --   dimensions or encoding than the one it was read as.
    | MapArtifactMalformed !Text !Text
      -- ^ What, and what is wrong with a body that passed its checksum.
    | MapArtifactPngHeader !Text !Text
      -- ^ What, and why its PNG signature or IHDR was refused before
      --   decode.
    | MapArtifactPngDimensions !Text !(Int, Int) !(Int, Int)
      -- ^ What, expected, declared.
    | MapArtifactInflatedSize !Text !Integer !Integer
      -- ^ What, the exact inflated image-data length a page has, and the
      --   length observed when inflation stopped — at the first output
      --   past the bound, never after inflating the whole stream.
    | MapArtifactDecodeFailure !Text !Text
      -- ^ The native decoder refused (or threw) after every declared
      --   bound had passed.
    | MapArtifactSubstituted !Text !Text
      -- ^ A file the library's entry record describes differently from
      --   the manifest, or a record that does not describe it.
    | MapArtifactEntryIncomplete !Text
      -- ^ The library does not judge the entry complete.
    | MapArtifactImage !MapImageRefusal
    | MapArtifactIO !FilePath !Text
    | MapArtifactLibrary !LibraryFailure
    deriving (Eq, Show)

-- | The log-facing text for a refusal.
mapArtifactRefusalText ∷ MapArtifactRefusal → Text
mapArtifactRefusalText r = "Refusing map artifact: " <> case r of
    MapArtifactAbsent gid →
        "no library entry for " <> renderGeneratedWorldId gid <> "."
    MapArtifactMissingRequired RequiredManifest → "the manifest is missing."
    MapArtifactMissingRequired (RequiredPage key) →
        "required page " <> keyText key <> " is missing."
    MapArtifactUnexpectedPage key →
        "page " <> keyText key <> " is not part of mandatory coverage."
    MapArtifactNotFineLevel key cutoff →
        "page " <> keyText key <> " is mandatory (cutoff level "
        <> tshow cutoff <> "), not a fine page."
    MapArtifactUnknownVersion what v →
        what <> " declares unknown format version " <> tshow v <> "."
    MapArtifactNotRecognised what → what <> " does not carry the expected magic."
    MapArtifactTruncated what expected actual →
        what <> " is truncated: " <> tshow actual <> " of "
        <> tshow expected <> " bytes."
    MapArtifactLengthMismatch what declared actual →
        what <> " is " <> tshow actual <> " bytes, declared "
        <> tshow declared <> "."
    MapArtifactOversized what bound actual →
        what <> " is " <> tshow actual <> " bytes, over the "
        <> tshow bound <> "-byte format bound."
    MapArtifactChecksumMismatch what → what <> " fails its SHA-256 check."
    MapArtifactWrongWorld expected found →
        "it belongs to world " <> renderGeneratedWorldId found
        <> ", not " <> renderGeneratedWorldId expected <> "."
    MapArtifactIncompatible field expected found →
        mapCompatFieldText field <> " compatibility is " <> tshow found
        <> "; this build requires " <> tshow expected <> "."
    MapArtifactGeometry ref → mapPyramidRefusalText ref
    MapArtifactGeometryMismatch why → "recorded geometry disagrees: " <> why <> "."
    MapArtifactInvalidKey key why → "invalid page key " <> keyText key <> ": " <> why
    MapArtifactDuplicateKey key → "page " <> keyText key <> " appears twice."
    MapArtifactUnorderedKeys a b →
        "page " <> keyText b <> " follows " <> keyText a
        <> " out of canonical order."
    MapArtifactConflictingName key canonical recorded →
        "page " <> keyText key <> " is recorded as " <> recorded
        <> " but its canonical name is " <> canonical <> "."
    MapArtifactPageBinding key why → "page file for " <> keyText key <> " " <> why <> "."
    MapArtifactMalformed what why → what <> " is malformed: " <> why <> "."
    MapArtifactPngHeader what why → what <> " has an unacceptable PNG header: " <> why <> "."
    MapArtifactPngDimensions what (ew, eh) (w, h) →
        what <> " declares " <> tshow w <> "×" <> tshow h <> ", not "
        <> tshow ew <> "×" <> tshow eh <> "."
    MapArtifactInflatedSize what expected observed →
        what <> " inflates to " <> (if observed > expected then "more than " else "")
        <> tshow observed <> " image-data bytes, not exactly "
        <> tshow expected <> "."
    MapArtifactDecodeFailure what why → what <> " does not decode: " <> why <> "."
    MapArtifactSubstituted what why → what <> " disagrees with the library record: " <> why <> "."
    MapArtifactEntryIncomplete why → "the library entry is not complete: " <> why
    MapArtifactImage ref → mapImageRefusalText ref
    MapArtifactIO path why → "I/O failure at " <> T.pack path <> ": " <> why
    MapArtifactLibrary f → renderLibraryFailure f
  where
    keyText (MapPageKey l pu pv) =
        "(" <> tshow l <> ", " <> tshow pu <> ", " <> tshow pv <> ")"
