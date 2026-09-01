{-# LANGUAGE Strict, OverloadedStrings, ScopedTypeVariables #-}
-- | Parsing and validation of the generated per-unit atlas index
--   (#1259, TEX-3) that @tools/pack_atlas.py --compile@ (#1258, TEX-2)
--   emits at @assets\/textures\/units\/\<unit\>\/atlas\/index.json@.
--
--   The index is GENERATED and never hand-edited (D-11), which is
--   exactly why it is validated rather than trusted: what reaches this
--   parser is a file on disk, and a stale, truncated, hand-mangled, or
--   foreign one must be REJECTED with the unit, animation, and artifact
--   named — never silently sampled (requirement 5). There has never
--   been a fallback to the source frames beside it, and since #1261
--   retired per-frame unit-animation loading there is nothing to fall
--   back TO: a unit's animations either load from its compiled atlases
--   or the unit does not load at all.
--
--   Everything here is pure, and it is the whole of validation apart
--   from reading files. 'parseAtlasIndex' answers from the document
--   alone; 'planUnitAtlasStorage' answers it against what the unit YAML
--   declares; and 'validateAtlasImage' \/ 'validateSourceFrame' answer
--   the questions only decoded pixels can — whether the atlas is the
--   image its index describes, and whether it still holds the source
--   art it was compiled from. "Unit.Atlas.Load" runs them in that
--   order.
--
--   BOTH digests the compiler records are verified:
--
--     * @atlas_digest@, over the atlas's decoded RGBA8 CONTENT
--       ('validateAtlasImage') — what catches an artifact this index
--       does not actually describe;
--     * @source_digest@, over everything the animation was compiled
--       FROM ('validateSourceDigest') — what catches a forged digest,
--       and a source edit that changed a frame's PATH without changing
--       its pixels, which nothing else in the index records.
--
--   'validateSourceFrame' still compares each frame against its atlas
--   cell, and still runs first: it localizes a stale artifact to one
--   direction and one frame, where the digest can only report that
--   something among the inputs moved.
module Unit.Atlas.Index
    ( AtlasLoadError(..)
    , renderAtlasLoadError
    , atlasIndexSchemaVersion
    , atlasIndexFileName
    , unitAtlasDir
    , unitAtlasIndexPath
    , atlasTextureName
    , atlasTextureRequests
    , parseAtlasIndex
    , YamlAnimFacts(..)
    , planUnitAtlasStorage
    , DecodedImage(..)
    , validateAtlasImage
    , validateSourceFrame
    , validateSourceDigest
    , atlasCellRows
    , atlasSlotRows
    , expectedSlotRows
    , indexDirectionToken
    , module Unit.Atlas.Digest
    ) where

import UPrelude
import qualified Data.Aeson as A
import qualified Data.Aeson.Types as A
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.HashMap.Strict as HM
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as T
import Data.List (sort, sortOn)
import System.FilePath ((</>))
import Unit.Atlas.Digest
import Unit.Atlas.Types
import Unit.Direction (Direction(..))

-- | The index @schema_version@ this build speaks. A document declaring
--   anything else is rejected: the format is the contract, and reading
--   an unknown one by guessing which fields still mean what is exactly
--   the silent-corruption failure requirement 5 forbids.
--
--   v2 (#2076) added the REQUIRED per-animation @cell_padding@. A v1
--   document describes cells packed at exact cell edges, so reading one
--   under v2's padded stride would sample the wrong texels — which is
--   why the version is checked against the RAW document before it is
--   decoded ('parseAtlasIndex'): a v1 index must be rejected for being
--   v1, not for missing a field v1 never had.
atlasIndexSchemaVersion ∷ Int
atlasIndexSchemaVersion = 2

-- | The extrusion gutter, in texels per side, this build supports —
--   @tools\/pack_atlas.py@'s @CELL_PADDING@ (#2076).
--
--   Exactly one value is supported, and an index declaring any other is
--   rejected rather than strided by. Widening the gutter moves the
--   stride, every cell UV, and every recorded digest at once, so it is
--   a schema change with its own version, never a number the runtime
--   reads and adapts to.
supportedCellPadding ∷ Int
supportedCellPadding = 1

-- | The digest algorithm this build can verify.
supportedDigestAlgorithm ∷ Text
supportedDigestAlgorithm = "sha256"

atlasIndexFileName ∷ FilePath
atlasIndexFileName = "index.json"

-- | The compiler-owned output directory for one unit — a SIBLING of
--   @animations\/@, which is what keeps generated artifacts outside the
--   filesystem-first inventory walk (#1257).
unitAtlasDir ∷ Text → FilePath
unitAtlasDir unit = "assets/textures/units" </> T.unpack unit </> "atlas"

unitAtlasIndexPath ∷ Text → FilePath
unitAtlasIndexPath unit = unitAtlasDir unit </> atlasIndexFileName

-- | The texture-registry name one animation's atlas is registered
--   under. One name, one handle, one upload, one bindless slot per
--   animation (D-2/D-10).
atlasTextureName ∷ Text → Text → Text
atlasTextureName unit anim = "unit_" <> unit <> "_" <> anim <> "_atlas"

-- | The atlas texture requests a unit's selection produces: EXACTLY one
--   per animation, each naming that animation's own atlas.
--
--   This IS the loader's upload set, not a description of it — the unit
--   loader issues one request per element of this list and publishes the
--   'Animation' each element carries — so a selection that does not
--   exist (a rejected index yields none) issues nothing, and nothing
--   the loader publishes can come from outside this list.
--   Deterministically ordered by animation name so it is assertable.
--
--   Each element is @(animation, registry name, that animation's index
--   record)@. Carrying the record rather than just its 'aaPath' is what
--   removes the loader's second lookup: since #1261 every declared
--   animation is atlas-backed, so a lookup that could miss would be a
--   branch nothing can reach and nothing can test.
atlasTextureRequests
    ∷ Text → HM.HashMap Text AtlasAnimation → [(Text, Text, AtlasAnimation)]
atlasTextureRequests unit sel =
    [ (name, atlasTextureName unit name, aa)
    | (name, aa) ← sortOn fst (HM.toList sel) ]

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
    -- Version FIRST, off the raw document, before the full decode.
    -- On an unsupported schema nothing else is known to mean what this
    -- build thinks it means, so reporting a field error from it would
    -- be misleading — and since #2076 it would be actively wrong: a
    -- genuine v1 index legitimately lacks `cell_padding`, and decoding
    -- it first would blame that missing field rather than the version
    -- that explains it. Reading only `schema_version` here is what
    -- keeps the version the reported cause.
    declared ← mapLeft (fail' ∘ ("index is malformed: " <>) ∘ T.pack)
                (A.parseEither parseSchemaVersion value)
    when (declared ≢ atlasIndexSchemaVersion) $
        Left (fail' $ "unsupported index schema_version "
                <> tshow declared <> " (this build reads "
                <> tshow atlasIndexSchemaVersion
                <> "); re-run tools/pack_atlas.py --compile")
    doc ← mapLeft (fail' ∘ ("index is malformed: " <>) ∘ T.pack)
                (A.parseEither parseIndexDocument value)
    -- The peek above and the full decode read the same key, so this
    -- cannot fire; stating it keeps the decoded field the one the rest
    -- of the document is validated under rather than a value nothing
    -- consults.
    when (idSchemaVersion doc ≢ declared) $
        Left (fail' $ "index declares schema_version "
                <> tshow (idSchemaVersion doc)
                <> " where its own document says " <> tshow declared)
    when (idDigestAlgorithm doc ≢ supportedDigestAlgorithm) $
        Left (fail' $ "unsupported digest_algorithm '"
                <> idDigestAlgorithm doc <> "' (this build verifies '"
                <> supportedDigestAlgorithm <> "')")
    when (T.null (T.strip (idGenerator doc))) $
        Left (fail' "generator is empty")
    when (idToolVersion doc < 0) $
        Left (fail' $ "tool_version " <> tshow (idToolVersion doc)
                <> " is negative")
    -- `direction_order` is DOCUMENTATION of the compiler's row order,
    -- not something this build re-derives rows from — each row is read
    -- explicitly. It is still validated, because a document declaring a
    -- different order was produced by a compiler whose layout this build
    -- does not share, and reading its rows as if it did is exactly the
    -- silent mis-sampling requirement 5 forbids.
    when (idDirectionOrder doc ≢ canonicalDirectionOrder) $
        Left (fail' $ "direction_order "
                <> T.intercalate "/" (idDirectionOrder doc)
                <> " is not this build's row order "
                <> T.intercalate "/" canonicalDirectionOrder)
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
    -- Two animations sharing one atlas file would each validate on
    -- their own, and the upload path would then legitimately alias the
    -- second request onto the first's image and bindless slot — leaving
    -- two animations reading one sheet. The canonical-name rule in
    -- 'validateAnimation' already makes that unreachable; this states
    -- the invariant the rest of the pipeline depends on, so a future
    -- relaxation of the naming rule cannot silently reintroduce it.
    let pathDupes = duplicates (map rawAtlasPath (idAnimations doc))
    unless (null pathDupes) $
        Left (fail' $ "index declares one atlas_path for more than one "
                <> "animation: " <> T.intercalate ", " pathDupes)
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
    -- Containment FIRST: a corrupt or foreign index must not be able to
    -- name an arbitrary file for the engine to load. The atlas has to
    -- live in this unit's own compiler-owned directory, addressed by a
    -- plain file name.
    let atlasPath = T.unpack (rawAtlasPath raw)
        expectedDir = unitAtlasDir unit
    unless (atlasPathContained expectedDir (rawAtlasPath raw)) $
        bad ("atlas_path '" <> rawAtlasPath raw <> "' is not a plain file in "
             <> T.pack expectedDir)
    -- Then the CANONICAL name the compiler emits: @<animation>.png@
    -- (its @atlas_file_rel@). Requiring it is what makes "one atlas per
    -- animation" (D-2) hold by construction — animation names are
    -- already unique, so no two animations can name one file, and
    -- therefore no two can be collapsed onto one image and one bindless
    -- slot by the upload path's same-path aliasing (which is otherwise
    -- correct: two requests for one file SHOULD share, D-10).
    let canonicalPath = T.pack (unitAtlasDir unit) <> "/" <> name <> ".png"
    when (rawAtlasPath raw ≢ canonicalPath) $
        bad ("atlas_path '" <> rawAtlasPath raw
             <> "' is not this animation's canonical atlas '"
             <> canonicalPath <> "'")
    -- Positive geometry, before anything divides by it.
    let dims = [ ("atlas_width", rawAtlasWidth raw)
               , ("atlas_height", rawAtlasHeight raw)
               , ("cell_width", rawCellWidth raw)
               , ("cell_height", rawCellHeight raw)
               , ("columns", rawColumns raw)
               , ("rows", rawRows raw) ]
    forM_ dims $ \(label, v) →
        when (v ≤ 0) $ bad (label <> " must be positive, got " <> tshow v)
    -- The extrusion gutter (#2076). Exactly one layout is supported,
    -- and it is REQUIRED rather than defaulted: a document that could
    -- omit it would be strided as if it were unpadded, which reads
    -- every cell but the first from the wrong texels. Checked before
    -- the containment below, which strides by it.
    when (rawCellPadding raw ≢ supportedCellPadding) $
        bad ("cell_padding " <> tshow (rawCellPadding raw)
             <> " is not this build's one supported extrusion gutter of "
             <> tshow supportedCellPadding <> " texel(s) per side")
    -- Every reachable SLOT lies inside the atlas — the padded slot, not
    -- the logical cell, because the gutter is real image area a cell
    -- one texel from the sheet edge would otherwise be read past.
    -- Containment rather than equality: the compiler emits an
    -- exactly-covered sheet today, and a future storage format is free
    -- to pad the image further, but no addressable slot may ever fall
    -- outside it.
    let padded = rawCellPadding raw * 2
        slotW  = rawCellWidth raw + padded
        slotH  = rawCellHeight raw + padded
    when (rawColumns raw * slotW > rawAtlasWidth raw) $
        bad ("columns x (cell_width + 2*cell_padding) ("
             <> tshow (rawColumns raw * slotW)
             <> ") exceeds atlas_width " <> tshow (rawAtlasWidth raw))
    when (rawRows raw * slotH > rawAtlasHeight raw) $
        bad ("rows x (cell_height + 2*cell_padding) ("
             <> tshow (rawRows raw * slotH)
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
        , aaCellPadding  = rawCellPadding raw
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

-- | The compiler's @ATLAS_DIRECTION_ORDER@: the engine's own
--   'Direction' order, in the index's own spelling.
canonicalDirectionOrder ∷ [Text]
canonicalDirectionOrder = map renderDir [minBound .. maxBound]

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

-- | The index's own spelling of a direction — @pack_atlas.py@'s
--   @ATLAS_DIRECTION_ORDER@ tokens. Used for diagnostics AND as a
--   digest input, so it is the one spelling both sides agree on.
indexDirectionToken ∷ Direction → Text
indexDirectionToken = renderDir

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

-- | Decide the atlas backing for every animation a unit declares, given
--   its validated index and what its YAML declares.
--
--   ALL OR NOTHING, AND TOTAL. The result is one map or one error, and
--   on success its key set is EXACTLY @Map.keysSet yamlAnims@ — every
--   declared animation is atlas-backed, because since #1261 there is no
--   other representation to be. A caller may therefore publish straight
--   from this map without a second lookup that could miss.
--
--   Beyond the structural validation 'parseAtlasIndex' already did,
--   this checks the DECLARATIONS against the unit YAML. It is not the
--   last word on freshness — 'validateSourceDigest' recomputes the
--   whole @source_digest@ and subsumes everything here — but it is far
--   cheaper and runs first, so a unit whose YAML has simply moved on is
--   rejected without decoding a single image:
--
--     * An indexed animation the YAML no longer declares is a leftover
--       from a rename or deletion, not something to publish.
--     * A DECLARED animation the index does not name is an
--       uncompiled one. Before #1261 that meant "load this animation
--       from its source frames instead"; with the per-frame path
--       retired it means the compiled artifacts predate a YAML edit,
--       and publishing the unit without that animation would quietly
--       drop art the file asks for.
--     * @fps@ \\/ @loop@ \\/ @flip@ must agree. The compiler records
--       what the engine would hold, so a disagreement means the
--       artifact predates a YAML edit. This rejects rather than
--       silently picking a winner, which is what lets the loader read
--       either side.
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
planUnitAtlasStorage unit yamlAnims anims = do
    sel ← HM.fromList <$> mapM check anims
    case [ n | n ← Map.keys yamlAnims, not (HM.member n sel) ] of
        []      → Right sel
        missing → Left AtlasLoadError
            { aleUnit = unit
            , aleAnimation = listToMaybe missing
            , aleArtifact = unitAtlasIndexPath unit
            , aleReason =
                "the unit YAML declares animation(s) "
                <> T.intercalate ", " (map quoted missing)
                <> " that the index does not name; unit animations are "
                <> "atlas-backed only (#1261) so there is nothing to load "
                <> "them from — re-run tools/pack_atlas.py --compile" }
  where
    quoted n = "'" <> n <> "'"
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

-- | The RGBA8 rows of one atlas cell's LOGICAL area, top to bottom.
--
--   Strides by the padded slot and starts at the cell's inner origin
--   (#2076), so this addresses exactly what 'atlasCellUV' addresses:
--   the extrusion gutter around the cell is NOT part of what a frame
--   must match. 'atlasSlotRows' is the one that includes it.
--
--   Slices, not copies: 'BS.take' \/ 'BS.drop' on a strict ByteString
--   are O(1), so walking a whole sheet's cells allocates nothing beyond
--   the row headers.
atlasCellRows ∷ AtlasAnimation → DecodedImage → Int → Int → [BS.ByteString]
atlasCellRows anim (DecodedImage w _ pixels) row col =
    [ BS.take (cw * 4) (BS.drop ((y * w + x0) * 4) pixels)
    | let cw = aaCellWidth anim
          ch = aaCellHeight anim
          p  = aaCellPadding anim
          x0 = col * (cw + 2 * p) + p
          y0 = row * (ch + 2 * p) + p
    , y ← [y0 .. y0 + ch - 1] ]

-- | The RGBA8 rows of one cell's PHYSICAL slot — the logical cell plus
--   its extrusion gutter on every side — top to bottom.
--
--   This is what the ring check compares against: the gutter is
--   generated, so it is verified rather than assumed, exactly as the
--   cell's own pixels are.
atlasSlotRows ∷ AtlasAnimation → DecodedImage → Int → Int → [BS.ByteString]
atlasSlotRows anim (DecodedImage w _ pixels) row col =
    [ BS.take (sw * 4) (BS.drop ((y * w + x0) * 4) pixels)
    | let p  = aaCellPadding anim
          sw = aaCellWidth anim + 2 * p
          sh = aaCellHeight anim + 2 * p
          x0 = col * sw
          y0 = row * sh
    , y ← [y0 .. y0 + sh - 1] ]

-- | The slot a source frame MUST compile to: the frame at offset
--   @(p, p)@, its border filled by copying the frame's own outermost
--   texels outward — edges from the adjacent edge row or column, each
--   corner from the single corner texel it touches, duplicated across
--   the whole corner square.
--
--   This is @tools\/pack_atlas.py@'s @extruded_slot@, reproduced so the
--   runtime can VERIFY the gutter instead of trusting it. Nothing here
--   blends or resamples: every byte is a copy of a real frame texel,
--   which is what leaves nearest-mode sampling untouched.
expectedSlotRows ∷ Int → DecodedImage → [BS.ByteString]
expectedSlotRows p (DecodedImage fw fh pixels) = top <> middle <> bottom
  where
    rowBytes = fw * 4
    extrude r =
        let l = BS.take 4 r
            t = BS.drop (rowBytes - 4) r
        in BS.concat (replicate p l <> [r] <> replicate p t)
    middle = [ extrude (BS.take rowBytes (BS.drop (y * rowBytes) pixels))
             | y ← [0 .. fh - 1] ]
    top    = replicate p (headDef middle)
    bottom = replicate p (lastDef middle)
    headDef xs = case xs of { (x:_) → x ; [] → BS.empty }
    lastDef xs = case reverse xs of { (x:_) → x ; [] → BS.empty }

-- | The PIXEL half of source freshness: the atlas cell that must hold
--   this source frame really does hold it, decoded sample for decoded
--   sample.
--
--   A DIRECT verification of the compiler's own promise: "every atlas
--   cell is a byte-for-byte copy of its source frame's canonical
--   decoded RGBA8 samples".
--
--   Since #2076 it verifies the cell's EXTRUSION RING too, corners
--   included: the gutter is compiler-generated from this same frame, so
--   an artifact whose ring does not reproduce is exactly as stale as
--   one whose cell does not — and a wrong ring is what would let a
--   linear tap read a neighbour, which is the whole reason the gutter
--   exists.
--
--   'validateSourceDigest' covers this too — the frame's pixels are
--   among the inputs it digests — so this is not the freshness
--   authority. It runs first because it is the DIAGNOSTIC one: it names
--   the direction and the frame that moved, where a digest mismatch can
--   only say that something among the inputs did. It also verifies the
--   cell dimensions against the frame, which is the check that makes a
--   later cell read meaningful.
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
    -- The gutter is GENERATED, so it is verified rather than assumed
    -- (#2076). Comparing the whole slot subsumes the cell check above
    -- and adds the ring, corners included; the cell check still runs
    -- first because it is the sharper diagnostic — "the art moved"
    -- rather than "the compiled slot is not what this frame produces".
    | atlasSlotRows anim atlas row col
          ≢ expectedSlotRows (aaCellPadding anim) frame =
        bad $ "source frame " <> T.pack path <> " (" <> renderDir dir
              <> " frame " <> tshow col
              <> ") does not carry the one-texel extrusion ring its "
              <> "atlas slot must hold"
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

-- | Verify one animation's @source_digest@ against everything it was
--   compiled from.
--
--   This is the LAST validation step and the most expensive, so it runs
--   only once every cheaper check has passed. It subsumes them: the
--   digest covers the animation's identity, its @flip@ \/ @loop@ \/
--   @fps@, its cell geometry and column count, its direction set with
--   each row and real frame count, and every frame's DECLARED PATH,
--   size, and decoded pixels. Two things only it can catch:
--
--     * a forged or corrupt digest — every other check would pass an
--       index carrying an arbitrary value here;
--     * a source edit that changed a frame's PATH without changing its
--       pixels. Nothing else in the index records paths, so nothing
--       else can notice.
validateSourceDigest
    ∷ Text → AtlasAnimation → SourceAnimInput → Either AtlasLoadError ()
validateSourceDigest unit anim inputs
    | actual ≡ aaSourceDigest anim = Right ()
    | otherwise = Left ∘ animError unit (aaPath anim) (aaName anim) $
        "source digest " <> actual <> " does not match the index's "
        <> aaSourceDigest anim
        <> " — the compiled artifact is stale or the index was modified; "
        <> "re-run tools/pack_atlas.py --compile"
  where
    actual = sourceDigest inputs

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
    , rawCellPadding   ∷ !Int
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
    , idGenerator       ∷ !Text
    , idToolVersion     ∷ !Int
    , idDigestAlgorithm ∷ !Text
    , idUnit            ∷ !Text
    , idDirectionOrder  ∷ ![Text]
    , idAnimations      ∷ ![RawAnimation]
    }

-- | Every field the compiler emits is REQUIRED — the whole top-level
--   schema, not just the parts this build happens to consume. A
--   truncated document is a truncated document: one missing a
--   dimension, a digest, the generator, the tool version, the direction
--   order, or the animation list fails here rather than defaulting into
--   something samplable.
-- | @schema_version@ ALONE, off the raw document.
--
--   Deliberately reads nothing else: it runs before the full decode so
--   that an index from another schema is rejected for its version
--   rather than for whichever field that schema happened not to carry.
parseSchemaVersion ∷ A.Value → A.Parser Int
parseSchemaVersion =
    A.withObject "atlas index" (\o → o A..: "schema_version")

parseIndexDocument ∷ A.Value → A.Parser IndexDocument
parseIndexDocument = A.withObject "atlas index" $ \o → IndexDocument
    <$> o A..: "schema_version"
    <*> o A..: "generator"
    <*> o A..: "tool_version"
    <*> o A..: "digest_algorithm"
    <*> o A..: "unit"
    <*> o A..: "direction_order"
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
    <*> o A..: "cell_padding"
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
