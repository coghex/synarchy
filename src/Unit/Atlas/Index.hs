{-# LANGUAGE Strict, OverloadedStrings, ScopedTypeVariables #-}
-- | Parsing and validation of the generated per-unit atlas index
--   (#1259, TEX-3) that @tools/pack_atlas.py --compile@ (#1258, TEX-2)
--   emits at @assets\/textures\/units\/\<unit\>\/atlas\/index.json@.
--
--   The index is GENERATED and never hand-edited (D-11), which is
--   exactly why it is validated rather than trusted: what reaches this
--   parser is a file on disk, and a stale, truncated, hand-mangled, or
--   foreign one must be REJECTED with the unit, animation, and artifact
--   named — never silently sampled (requirement 5). There is no
--   fallback to legacy frames on failure: an animation the index claims
--   is atlas-backed either loads as an atlas or does not load at all.
--
--   Everything here is pure. 'parseAtlasIndex' answers from the
--   document alone; 'validateAtlasImage' is the second half, checked
--   once the atlas image has actually been decoded — because two of the
--   things a stale index gets wrong ('aaAtlasWidth' \/ 'aaAtlasHeight'
--   against the real image, and 'aaAtlasDigest' against its real
--   pixels) are unanswerable from the document.
--
--   Which digest gets verified where is a deliberate split. The
--   compiler records TWO (see @tools\/pack_atlas.py@'s @digest_stream@):
--
--     * @source_digest@ — over the animation's own SOURCE frames.
--       Recomputing it needs those frames, which the atlas runtime
--       deliberately stops reading; @pack_atlas.py --validate-only@ (in
--       CI) owns that check. It is parsed, required, and carried on
--       'aaSourceDigest' for reporting.
--     * @atlas_digest@ — over the atlas's decoded RGBA8 CONTENT. That
--       IS checkable here, because the loader decodes the image
--       anyway, and it is the check that catches the artifact this
--       index does not actually describe.
module Unit.Atlas.Index
    ( AtlasLoadError(..)
    , renderAtlasLoadError
    , atlasIndexSchemaVersion
    , atlasIndexFileName
    , unitAtlasDir
    , unitAtlasIndexPath
    , parseAtlasIndex
    , YamlAnimFacts(..)
    , planUnitAtlasStorage
    , DecodedImage(..)
    , validateAtlasImage
    , validateSourceFrame
    , atlasCellRows
    , atlasContentDigest
    ) where

import UPrelude
import qualified Crypto.Hash.SHA256 as SHA256
import qualified Data.Aeson as A
import qualified Data.Aeson.Types as A
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as BL
import qualified Data.HashMap.Strict as HM
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as T
import Data.List (sort)
import Numeric (showHex)
import System.FilePath ((</>))
import Unit.Atlas.Types
import Unit.Direction (Direction(..))

-- | The index @schema_version@ this build speaks. A document declaring
--   anything else is rejected: the format is the contract, and reading
--   an unknown one by guessing which fields still mean what is exactly
--   the silent-corruption failure requirement 5 forbids.
atlasIndexSchemaVersion ∷ Int
atlasIndexSchemaVersion = 1

-- | The digest algorithm this build can verify.
supportedDigestAlgorithm ∷ Text
supportedDigestAlgorithm = "sha256"

-- | The compiler's @ATLAS_DIGEST_TAG@ — the domain tag of the content
--   digest. Versioned by the compiler so a change to what goes INTO a
--   digest invalidates every recorded one.
atlasDigestTag ∷ BS.ByteString
atlasDigestTag = "synarchy-atlas-content-v1"

atlasIndexFileName ∷ FilePath
atlasIndexFileName = "index.json"

-- | The compiler-owned output directory for one unit — a SIBLING of
--   @animations\/@, which is what keeps generated artifacts outside the
--   filesystem-first inventory walk (#1257).
unitAtlasDir ∷ Text → FilePath
unitAtlasDir unit = "assets/textures/units" </> T.unpack unit </> "atlas"

unitAtlasIndexPath ∷ Text → FilePath
unitAtlasIndexPath unit = unitAtlasDir unit </> atlasIndexFileName

-- | Why an atlas-backed animation could not be loaded.
--
--   Every rejection names the unit, the animation when the failure is
--   attributable to one, and the artifact it was reading — so a broken
--   asset is diagnosable from the log line alone.
data AtlasLoadError = AtlasLoadError
    { aleUnit      ∷ !Text
    , aleAnimation ∷ !(Maybe Text)
    , aleArtifact  ∷ !FilePath
    , aleReason    ∷ !Text
    } deriving (Show, Eq)

renderAtlasLoadError ∷ AtlasLoadError → Text
renderAtlasLoadError e =
    "unit atlas rejected: unit '" <> aleUnit e <> "'"
        <> maybe "" (\a → ", animation '" <> a <> "'") (aleAnimation e)
        <> ", artifact " <> T.pack (aleArtifact e)
        <> " — " <> aleReason e

indexError ∷ Text → FilePath → Text → AtlasLoadError
indexError unit path reason = AtlasLoadError
    { aleUnit = unit, aleAnimation = Nothing
    , aleArtifact = path, aleReason = reason }

animError ∷ Text → FilePath → Text → Text → AtlasLoadError
animError unit path anim reason = AtlasLoadError
    { aleUnit = unit, aleAnimation = Just anim
    , aleArtifact = path, aleReason = reason }

-- * Parsing

-- | Decode and validate one unit's index document.
--
--   Returns the animations in the order the document lists them. A
--   document that parses is not yet trusted: every structural
--   invariant below is checked before any animation is returned, so a
--   caller can allocate GPU residency knowing the whole index is
--   sound rather than discovering the third animation is broken after
--   uploading two.
parseAtlasIndex
    ∷ Text          -- ^ unit name the caller asked for
    → FilePath      -- ^ index path, for error messages
    → BL.ByteString
    → Either AtlasLoadError [AtlasAnimation]
parseAtlasIndex unit path raw = do
    value ← mapLeft (fail' ∘ ("index is not valid JSON: " <>) ∘ T.pack)
                  (A.eitherDecode' raw)
    doc ← mapLeft (fail' ∘ ("index is malformed: " <>) ∘ T.pack)
                (A.parseEither parseIndexDocument value)
    -- Version first: on an unsupported schema nothing else in the
    -- document is known to mean what this build thinks it means, so
    -- reporting a field error from it would be misleading.
    when (idSchemaVersion doc ≢ atlasIndexSchemaVersion) $
        Left (fail' $ "unsupported index schema_version "
                <> tshow (idSchemaVersion doc) <> " (this build reads "
                <> tshow atlasIndexSchemaVersion <> ")")
    when (idDigestAlgorithm doc ≢ supportedDigestAlgorithm) $
        Left (fail' $ "unsupported digest_algorithm '"
                <> idDigestAlgorithm doc <> "' (this build verifies '"
                <> supportedDigestAlgorithm <> "')")
    when (idUnit doc ≢ unit) $
        Left (fail' $ "index declares unit '" <> idUnit doc
                <> "' but was read as unit '" <> unit <> "'")
    when (null (idAnimations doc)) $
        Left (fail' "index declares no animations")
    let names = map rawName (idAnimations doc)
        dupes = duplicates names
    unless (null dupes) $
        Left (fail' $ "index declares duplicate animation names: "
                <> T.intercalate ", " dupes)
    mapM (validateAnimation unit path) (idAnimations doc)
  where
    fail' = indexError unit path

-- | Validate one animation entry and lift it to an 'AtlasAnimation'.
validateAnimation ∷ Text → FilePath → RawAnimation → Either AtlasLoadError AtlasAnimation
validateAnimation unit path raw = do
    let name = rawName raw
        -- Explicitly polymorphic in the result: the same helper both
        -- aborts a `when`/`unless` check (at `()`) and supplies the
        -- failure branch of a bind (at whatever that binds).
        bad ∷ ∀ α. Text → Either AtlasLoadError α
        bad reason = Left (animError unit path name reason)
    when (T.null name) $
        Left (indexError unit path "index declares an animation with an empty name")
    fmt ← case rawStorageFormat raw of
        "png" → Right AtlasFormatPng
        other → bad ("unsupported storage_format '" <> other <> "'")
    -- Containment: a corrupt or foreign index must not be able to name
    -- an arbitrary file for the engine to load. The atlas has to live
    -- in this unit's own compiler-owned directory, addressed by a
    -- plain file name.
    let atlasPath = T.unpack (rawAtlasPath raw)
        expectedDir = unitAtlasDir unit
    unless (atlasPathContained expectedDir (rawAtlasPath raw)) $
        bad ("atlas_path '" <> rawAtlasPath raw <> "' is not a plain file in "
             <> T.pack expectedDir)
    -- Positive geometry, before anything divides by it.
    let dims = [ ("atlas_width", rawAtlasWidth raw)
               , ("atlas_height", rawAtlasHeight raw)
               , ("cell_width", rawCellWidth raw)
               , ("cell_height", rawCellHeight raw)
               , ("columns", rawColumns raw)
               , ("rows", rawRows raw) ]
    forM_ dims $ \(label, v) →
        when (v ≤ 0) $ bad (label <> " must be positive, got " <> tshow v)
    -- Every reachable cell lies inside the atlas. Containment rather
    -- than equality: the compiler emits an exactly-covered sheet
    -- today, and a future storage format is free to pad the image, but
    -- no addressable cell may ever fall outside it.
    when (rawColumns raw * rawCellWidth raw > rawAtlasWidth raw) $
        bad ("columns x cell_width ("
             <> tshow (rawColumns raw * rawCellWidth raw)
             <> ") exceeds atlas_width " <> tshow (rawAtlasWidth raw))
    when (rawRows raw * rawCellHeight raw > rawAtlasHeight raw) $
        bad ("rows x cell_height ("
             <> tshow (rawRows raw * rawCellHeight raw)
             <> ") exceeds atlas_height " <> tshow (rawAtlasHeight raw))
    -- Playback metadata. `fps` divides elapsed time in the frozen
    -- `pickFrame` arithmetic (D-3), so a non-finite or non-positive
    -- value is rejected here rather than producing a nonsense index.
    unless (isFiniteFloat (rawFps raw) ∧ rawFps raw > 0) $
        bad ("fps must be a positive finite number, got " <> tshow (rawFps raw))
    when (null (rawDirections raw)) $
        bad "animation declares no directions"
    rows ← mapM (validateDirection unit path raw) (rawDirections raw)
    let dirs = map adrDirection rows
        rowIdx = map adrRow rows
    unless (distinct dirs) $
        bad "animation declares the same direction more than once"
    unless (distinct rowIdx) $
        bad "animation declares two directions on the same row"
    when (T.null (rawSourceDigest raw)) $ bad "source_digest is empty"
    when (T.null (rawAtlasDigest raw)) $ bad "atlas_digest is empty"
    pure AtlasAnimation
        { aaName         = name
        , aaFormat       = fmt
        , aaPath         = atlasPath
        , aaAtlasWidth   = rawAtlasWidth raw
        , aaAtlasHeight  = rawAtlasHeight raw
        , aaCellWidth    = rawCellWidth raw
        , aaCellHeight   = rawCellHeight raw
        , aaColumns      = rawColumns raw
        , aaRows         = rawRows raw
        , aaFlip         = rawFlip raw
        , aaFps          = rawFps raw
        , aaLoop         = rawLoop raw
        , aaDirections   = Map.fromList [ (adrDirection r, r) | r ← rows ]
        , aaSourceDigest = rawSourceDigest raw
        , aaAtlasDigest  = rawAtlasDigest raw
        }

validateDirection
    ∷ Text → FilePath → RawAnimation → RawDirection
    → Either AtlasLoadError AtlasDirectionRow
validateDirection unit path raw rd = do
    let name = rawName raw
        bad ∷ ∀ α. Text → Either AtlasLoadError α
        bad reason = Left (animError unit path name reason)
    dir ← maybe (bad $ "unknown direction '" <> rdDirection rd <> "'")
                Right
                (parseAtlasDirection (rdDirection rd))
    when (rdRow rd < 0 ∨ rdRow rd ≥ rawRows raw) $
        bad ("direction '" <> rdDirection rd <> "' declares row "
             <> tshow (rdRow rd) <> ", outside the animation's "
             <> tshow (rawRows raw) <> " rows")
    -- The REAL frame count is the sole frame authority (D-5). It must
    -- fit inside the row: a count above the column capacity would make
    -- padding — or off-sheet pixels — addressable as a frame.
    when (rdFrameCount rd ≤ 0) $
        bad ("direction '" <> rdDirection rd <> "' declares frame_count "
             <> tshow (rdFrameCount rd))
    when (rdFrameCount rd > rawColumns raw) $
        bad ("direction '" <> rdDirection rd <> "' declares frame_count "
             <> tshow (rdFrameCount rd) <> ", above the animation's "
             <> tshow (rawColumns raw) <> " columns")
    pure AtlasDirectionRow
        { adrDirection = dir
        , adrRow = rdRow rd
        , adrFrameCount = rdFrameCount rd
        }

-- | The index's own direction spelling — @tools/pack_atlas.py@'s
--   @ATLAS_DIRECTION_ORDER@ tokens, which are the long lowercase unit
--   YAML keys.
parseAtlasDirection ∷ Text → Maybe Direction
parseAtlasDirection t = case t of
    "south"      → Just DirS
    "south-west" → Just DirSW
    "west"       → Just DirW
    "north-west" → Just DirNW
    "north"      → Just DirN
    "north-east" → Just DirNE
    "east"       → Just DirE
    "south-east" → Just DirSE
    _            → Nothing

-- | The index's own spelling of a direction, for diagnostics.
renderDir ∷ Direction → Text
renderDir d = case d of
    DirS  → "south"      ; DirSW → "south-west"
    DirW  → "west"       ; DirNW → "north-west"
    DirN  → "north"      ; DirNE → "north-east"
    DirE  → "east"       ; DirSE → "south-east"

renderDirs ∷ Set.Set Direction → Text
renderDirs = T.intercalate "/" ∘ map renderDir ∘ Set.toList

-- | An atlas path is acceptable only as @\<unit atlas dir\>\/\<file\>@:
--   relative, no traversal, no nesting, no absolute escape.
atlasPathContained ∷ FilePath → Text → Bool
atlasPathContained expectedDir declared =
    case T.stripPrefix (T.pack expectedDir <> "/") declared of
        Nothing   → False
        Just leaf → not (T.null leaf)
                  ∧ not (T.any (≡ '/') leaf)
                  ∧ leaf ≢ "." ∧ leaf ≢ ".."

-- * Mode selection

-- | What the unit YAML declares about one animation. The three fields
--   the compiler records EFFECTIVE values for, and nothing else — this
--   deliberately takes facts rather than a @UnitYamlAnim@ so the check
--   stays pure and free of the asset-loading layer.
data YamlAnimFacts = YamlAnimFacts
    { yafFps    ∷ !Float
    , yafLoop   ∷ !Bool
    , yafFlip   ∷ !Bool
    , yafFrames ∷ !(Map.Map Direction [FilePath])
      -- ^ The animation's declared SOURCE frames, per direction, in
      --   declaration order — the same list the compiler digested.
    } deriving (Show, Eq)

-- | Decide which of a unit's animations are atlas-backed, given its
--   validated index and what its YAML declares.
--
--   ALL OR NOTHING. The result is one map or one error: there is no
--   partial answer, so a caller that allocates a handle and queues an
--   upload per returned entry can never publish some animations of a
--   unit whose index is broken.
--
--   Beyond the structural validation 'parseAtlasIndex' already did,
--   this is the DECLARATION half of source freshness — everything the
--   compiler's @source_digest@ covers except the frame pixels, which
--   'validateSourceFrame' checks against the atlas itself:
--
--     * An indexed animation the YAML no longer declares is a leftover
--       from a rename or deletion, not something to publish.
--     * @fps@ \\/ @loop@ \\/ @flip@ must agree. The compiler records
--       what the engine would hold, so a disagreement means the
--       artifact predates a YAML edit. The runtime keeps using the
--       YAML's values — this rejects rather than silently picking a
--       winner.
--     * The DIRECTION SET must agree, and each direction's real frame
--       count must equal the number of frames the YAML declares for it,
--       with the column count still the longest row. An added, removed,
--       or re-authored direction, and a frame appended to or dropped
--       from one, are exactly the source edits a stale atlas would
--       otherwise keep serving.
planUnitAtlasStorage
    ∷ Text
    → Map.Map Text YamlAnimFacts
    → [AtlasAnimation]
    → Either AtlasLoadError (HM.HashMap Text AtlasAnimation)
planUnitAtlasStorage unit yamlAnims anims =
    HM.fromList <$> mapM check anims
  where
    check aa = case Map.lookup (aaName aa) yamlAnims of
        Nothing → Left (stale aa
            "the index declares an animation this unit's YAML does not")
        Just ya
            | aaFps aa ≢ yafFps ya → Left (stale aa $
                "index fps " <> tshow (aaFps aa)
                <> " disagrees with the YAML's " <> tshow (yafFps ya))
            | aaLoop aa ≢ yafLoop ya → Left (stale aa $
                "index loop " <> tshow (aaLoop aa)
                <> " disagrees with the YAML's " <> tshow (yafLoop ya))
            | aaFlip aa ≢ yafFlip ya → Left (stale aa $
                "index flip " <> tshow (aaFlip aa)
                <> " disagrees with the YAML's " <> tshow (yafFlip ya))
            | indexDirs ≢ yamlDirs → Left (stale aa $
                "index directions " <> renderDirs indexDirs
                <> " disagree with the YAML's " <> renderDirs yamlDirs)
            | otherwise → case badCount ya of
                Just err → Left (stale aa err)
                Nothing
                    | aaColumns aa ≢ longestRow ya → Left (stale aa $
                        "index columns " <> tshow (aaColumns aa)
                        <> " disagrees with the YAML's longest direction ("
                        <> tshow (longestRow ya) <> " frames)")
                    | otherwise → Right (aaName aa, aa)
          where
            indexDirs = Map.keysSet (aaDirections aa)
            yamlDirs  = Map.keysSet (yafFrames ya)
            longestRow y =
                let ns = map length (Map.elems (yafFrames y))
                in if null ns then 0 else maximum ns
            badCount y = listToMaybe
                [ "direction " <> renderDir d <> " has frame_count "
                    <> tshow (adrFrameCount row) <> " but the YAML declares "
                    <> tshow (length paths) <> " frames"
                | (d, row) ← Map.toList (aaDirections aa)
                , let paths = Map.findWithDefault [] d (yafFrames y)
                , adrFrameCount row ≢ length paths ]
    stale aa reason = AtlasLoadError
        { aleUnit = unit
        , aleAnimation = Just (aaName aa)
        , aleArtifact = aaPath aa
        , aleReason = reason <> "; re-run tools/pack_atlas.py --compile"
        }

-- * Image-side validation

-- | A decoded image: canonical RGBA8 samples, row-major from the
--   top-left — exactly what the compiler digested and what the engine's
--   own upload path produces.
data DecodedImage = DecodedImage
    { diWidth  ∷ !Int
    , diHeight ∷ !Int
    , diPixels ∷ !BS.ByteString
    } deriving (Show, Eq)

-- | The half of validation that needs the DECODED atlas.
validateAtlasImage
    ∷ Text            -- ^ unit
    → AtlasAnimation
    → DecodedImage
    → Either AtlasLoadError ()
validateAtlasImage unit anim (DecodedImage w h pixels)
    | w ≢ aaAtlasWidth anim ∨ h ≢ aaAtlasHeight anim =
        bad $ "atlas image is " <> tshow w <> "x" <> tshow h
              <> " but the index declares "
              <> tshow (aaAtlasWidth anim) <> "x" <> tshow (aaAtlasHeight anim)
    | BS.length pixels ≢ w * h * 4 =
        bad $ "atlas image decoded to " <> tshow (BS.length pixels)
              <> " bytes, expected " <> tshow (w * h * 4) <> " RGBA8 bytes"
    | actual ≢ aaAtlasDigest anim =
        bad $ "atlas content digest " <> actual
              <> " does not match the index's " <> aaAtlasDigest anim
              <> " — the compiled artifact is stale or was modified; "
              <> "re-run tools/pack_atlas.py --compile"
    | otherwise = Right ()
  where
    actual = atlasContentDigest w h pixels
    bad = Left ∘ animError unit (aaPath anim) (aaName anim)

-- | The RGBA8 rows of one atlas cell, top to bottom.
--
--   Slices, not copies: 'BS.take' \/ 'BS.drop' on a strict ByteString
--   are O(1), so walking a whole sheet's cells allocates nothing beyond
--   the row headers.
atlasCellRows ∷ AtlasAnimation → DecodedImage → Int → Int → [BS.ByteString]
atlasCellRows anim (DecodedImage w _ pixels) row col =
    [ BS.take (cw * 4) (BS.drop ((y * w + col * cw) * 4) pixels)
    | let cw = aaCellWidth anim
          ch = aaCellHeight anim
    , y ← [row * ch .. row * ch + ch - 1] ]

-- | The PIXEL half of source freshness: the atlas cell that must hold
--   this source frame really does hold it, decoded sample for decoded
--   sample.
--
--   This is what catches the edit no metadata can — a source PNG
--   repainted while its compiled atlas and index were left in place.
--   It is a DIRECT verification of the compiler's own promise ("every
--   atlas cell is a byte-for-byte copy of its source frame's canonical
--   decoded RGBA8 samples") rather than a recomputation of
--   @source_digest@, and deliberately so:
--
--     * It verifies the property the digest is a proxy FOR, and cannot
--       be satisfied by an artifact that merely hashes the same.
--     * It localizes the failure to one direction and one frame instead
--       of reporting that some input, somewhere, changed.
--     * Recomputing the digest would require reproducing the compiler's
--       field encoding exactly, including @repr()@ of a Python float —
--       whose decimal formatting diverges from Haskell's at exponent
--       extremes. A parity bug there would REJECT valid art, which is a
--       far worse failure than the one it guards.
--
--   Every other @source_digest@ input is checked by name elsewhere:
--   unit and animation by identity, @flip@ \/ @loop@ \/ @fps@ \/ the
--   direction set \/ per-direction frame counts \/ columns by
--   'planUnitAtlasStorage', and cell dimensions here.
validateSourceFrame
    ∷ Text            -- ^ unit
    → AtlasAnimation
    → DecodedImage    -- ^ the decoded atlas
    → Direction
    → Int             -- ^ row
    → Int             -- ^ column (frame index)
    → FilePath        -- ^ the declared source frame path
    → DecodedImage    -- ^ the decoded source frame
    → Either AtlasLoadError ()
validateSourceFrame unit anim atlas dir row col path frame
    | diWidth frame ≢ aaCellWidth anim ∨ diHeight frame ≢ aaCellHeight anim =
        bad $ "source frame " <> T.pack path <> " is "
              <> tshow (diWidth frame) <> "x" <> tshow (diHeight frame)
              <> " but the index's cell is "
              <> tshow (aaCellWidth anim) <> "x" <> tshow (aaCellHeight anim)
              <> staleHint
    | BS.length (diPixels frame) ≢ diWidth frame * diHeight frame * 4 =
        bad $ "source frame " <> T.pack path <> " decoded to "
              <> tshow (BS.length (diPixels frame)) <> " bytes, expected "
              <> tshow (diWidth frame * diHeight frame * 4) <> " RGBA8 bytes"
    | frameRows ≢ atlasCellRows anim atlas row col =
        bad $ "source frame " <> T.pack path <> " (" <> renderDir dir
              <> " frame " <> tshow col
              <> ") does not match the pixels its atlas cell holds"
              <> staleHint
    | otherwise = Right ()
  where
    staleHint = " — the compiled artifact is stale; "
              <> "re-run tools/pack_atlas.py --compile"
    frameRows =
        [ BS.take (diWidth frame * 4) (BS.drop (y * diWidth frame * 4)
              (diPixels frame))
        | y ← [0 .. diHeight frame - 1] ]
    bad = Left ∘ animError unit (aaPath anim) (aaName anim)

-- | The compiler's @content_digest@, reproduced.
--
--   A canonical, length-prefixed stream: the domain tag, then each
--   labelled field as @\<u64 LE length\>\<bytes\>@. The length prefixes
--   are what make it injective — a bare concatenation would let a
--   character move across a field boundary without changing the hash.
atlasContentDigest ∷ Int → Int → BS.ByteString → Text
atlasContentDigest w h pixels =
    hex $ SHA256.finalize $ SHA256.updates SHA256.init $
        [ lengthPrefix (BS.length atlasDigestTag), atlasDigestTag ]
        <> concatMap field
            [ ("width",  BC.pack (show w))
            , ("height", BC.pack (show h))
            , ("pixels", pixels)
            ]
  where
    field (label, value) =
        [ lengthPrefix (BS.length label), label
        , lengthPrefix (BS.length value), value ]
    lengthPrefix n = BS.pack
        [ fromIntegral ((n `shiftR` (8 * i)) ⌃ 0xff) | i ← [0 .. 7 ∷ Int] ]

hex ∷ BS.ByteString → Text
hex = T.pack ∘ concatMap byte ∘ BS.unpack
  where
    byte b = let s = showHex b "" in if length s ≡ 1 then '0' : s else s

-- * The raw document

data RawDirection = RawDirection
    { rdDirection  ∷ !Text
    , rdRow        ∷ !Int
    , rdFrameCount ∷ !Int
    }

data RawAnimation = RawAnimation
    { rawName          ∷ !Text
    , rawStorageFormat ∷ !Text
    , rawAtlasPath     ∷ !Text
    , rawAtlasWidth    ∷ !Int
    , rawAtlasHeight   ∷ !Int
    , rawCellWidth     ∷ !Int
    , rawCellHeight    ∷ !Int
    , rawColumns       ∷ !Int
    , rawRows          ∷ !Int
    , rawFlip          ∷ !Bool
    , rawFps           ∷ !Float
    , rawLoop          ∷ !Bool
    , rawDirections    ∷ ![RawDirection]
    , rawSourceDigest  ∷ !Text
    , rawAtlasDigest   ∷ !Text
    }

data IndexDocument = IndexDocument
    { idSchemaVersion   ∷ !Int
    , idDigestAlgorithm ∷ !Text
    , idUnit            ∷ !Text
    , idAnimations      ∷ ![RawAnimation]
    }

-- | Every field is REQUIRED. A truncated document — one missing a
--   dimension, a digest, or the animation list — fails here rather
--   than defaulting into something samplable.
parseIndexDocument ∷ A.Value → A.Parser IndexDocument
parseIndexDocument = A.withObject "atlas index" $ \o → IndexDocument
    <$> o A..: "schema_version"
    <*> o A..: "digest_algorithm"
    <*> o A..: "unit"
    <*> (o A..: "animations" ⌦ mapM parseRawAnimation)

parseRawAnimation ∷ A.Value → A.Parser RawAnimation
parseRawAnimation = A.withObject "atlas index animation" $ \o → RawAnimation
    <$> o A..: "name"
    <*> o A..: "storage_format"
    <*> o A..: "atlas_path"
    <*> o A..: "atlas_width"
    <*> o A..: "atlas_height"
    <*> o A..: "cell_width"
    <*> o A..: "cell_height"
    <*> o A..: "columns"
    <*> o A..: "rows"
    <*> o A..: "flip"
    <*> o A..: "fps"
    <*> o A..: "loop"
    <*> (o A..: "directions" ⌦ mapM parseRawDirection)
    <*> o A..: "source_digest"
    <*> o A..: "atlas_digest"

parseRawDirection ∷ A.Value → A.Parser RawDirection
parseRawDirection = A.withObject "atlas index direction" $ \o → RawDirection
    <$> o A..: "direction"
    <*> o A..: "row"
    <*> o A..: "frame_count"

-- * Small helpers

tshow ∷ Show a ⇒ a → Text
tshow = T.pack ∘ show

mapLeft ∷ (a → c) → Either a b → Either c b
mapLeft f (Left a)  = Left (f a)
mapLeft _ (Right b) = Right b

distinct ∷ Ord a ⇒ [a] → Bool
distinct xs = length (Set.fromList xs) ≡ length xs

duplicates ∷ Ord a ⇒ [a] → [a]
duplicates = go Set.empty Set.empty ∘ sort
  where
    go _ acc [] = Set.toList acc
    go seen acc (x:xs)
        | Set.member x seen = go seen (Set.insert x acc) xs
        | otherwise         = go (Set.insert x seen) acc xs

-- | 'isNaN'/'isInfinite' as one predicate — JSON admits neither, but a
--   huge exponent decodes to a real 'Float' infinity.
isFiniteFloat ∷ Float → Bool
isFiniteFloat x = not (isNaN x) ∧ not (isInfinite x)
