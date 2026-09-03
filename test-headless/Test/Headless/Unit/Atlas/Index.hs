{-# LANGUAGE Strict, OverloadedStrings #-}
-- | Pure index and SCHEMA tests for the compiled unit-animation atlas
--   (#1259, TEX-3): what a well-formed generated index decodes to, the
--   format-neutral storage boundary (D-10), the rejection of every
--   malformed or foreign document, and the requirement that the whole
--   generated top-level schema be present.
--
--   These answer from the DOCUMENT alone. Anything that needs decoded
--   pixels, a source frame, or a file on disk belongs to
--   "Test.Headless.Unit.Atlas.Freshness"; anything that reads a frame
--   through the runtime geometry belongs to
--   "Test.Headless.Unit.Atlas.Consumers".
module Test.Headless.Unit.Atlas.Index (spec) where

import UPrelude
import Test.Hspec
import qualified Data.ByteString.Lazy as BL
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import Test.Headless.Unit.Atlas.Document
    ( arr, directionEntry, dropField, goodIndex, idleFields, indexWith
    , indexWithout, parse, setField, str, swingFields )
import Test.Headless.Unit.Atlas.Rejection
    ( isRejected, rejection, shouldReject )
import Unit.Atlas.Index
import Unit.Atlas.Types
import Unit.Direction (Direction(..))

spec ∷ Spec
spec = do
    describe "Unit.Atlas.Index — a well-formed index" $ do
        it "accepts the canonical document and reports both animations" $
            case parse goodIndex of
                Left e → expectationFailure (T.unpack (renderAtlasLoadError e))
                Right anims → map aaName anims `shouldBe` ["idle", "swing"]

        it "reads geometry, playback, and both digests verbatim" $
            case parse goodIndex of
                Right (idle:_) → do
                    aaFormat idle `shouldBe` AtlasFormatPng
                    aaPath idle `shouldBe`
                        "assets/textures/units/acolyte/atlas/idle.png"
                    (aaCellWidth idle, aaCellHeight idle) `shouldBe` (32, 48)
                    (aaColumns idle, aaRows idle) `shouldBe` (4, 5)
                    aaFlip idle `shouldBe` True
                    aaFps idle `shouldBe` 8
                    aaLoop idle `shouldBe` True
                    aaSourceDigest idle `shouldBe` "aaaa"
                    aaAtlasDigest idle `shouldBe` "bbbb"
                other → expectationFailure ("expected idle first, got " ⧺ show (fmap (map aaName) other))

        it "reads each direction's OWN row and real frame count" $
            case parse goodIndex of
                Right [_, swing] → do
                    Map.lookup DirNW (aaDirections swing)
                        `shouldBe` Just (AtlasDirectionRow DirNW 3 1)
                    Map.lookup DirS (aaDirections swing)
                        `shouldBe` Just (AtlasDirectionRow DirS 0 6)
                    -- Rows are NOT re-derived from a direction order:
                    -- `east` sits on row 6 because the document says so.
                    Map.lookup DirE (aaDirections swing)
                        `shouldBe` Just (AtlasDirectionRow DirE 6 4)
                other → expectationFailure ("expected two animations, got " ⧺ show (fmap (map aaName) other))

        it "does not require an animation to author all eight directions" $
            case parse goodIndex of
                Right (idle:_) → Map.size (aaDirections idle) `shouldBe` 5
                _ → expectationFailure "expected idle"

    -- D-10 keeps WHICH encoding an atlas uses behind an explicit,
    -- closed set rather than an inferred file extension, so deferred
    -- TEX-5's KTX2 slots in as a constructor. What that boundary owes
    -- TODAY, with PNG the only representation, is: accept the one
    -- token this build emits, refuse everything else outright, and
    -- never substitute a fallback for a representation it cannot read.
    -- The last part is the load-bearing one — a loader that quietly
    -- skipped an unreadable animation, or guessed PNG from the path,
    -- would publish a unit missing art and look healthy doing it.
    describe "Unit.Atlas — the format-neutral storage boundary (D-10)" $ do
        let withFormat v = indexWith [] [setField "storage_format" v idleFields]

        it "accepts the token pack_atlas.py emits, and reads it per \
           \animation" $
            -- Per ANIMATION, not once per unit: the index records a
            -- format on every record, which is the shape that lets one
            -- session hold different representations for different
            -- animations when TEX-5 lands.
            case parse goodIndex of
                Right anims → map aaFormat anims
                    `shouldBe` replicate (length anims) AtlasFormatPng
                Left e → expectationFailure (T.unpack (renderAtlasLoadError e))

        it "round-trips its own name, so the token it emits is the token \
           \it accepts" $
            atlasStorageFormatName AtlasFormatPng `shouldBe` "png"

        it "refuses every unknown representation rather than choosing one" $
            forM_ ["ktx2", "basis", "dds", "astc", "raw", ""] $ \token →
                parse (withFormat (str token))
                    `shouldReject` ("unsupported storage_format '"
                                    <> token <> "'")

        it "is an exact token, never case-folded or trimmed" $
            -- A tolerant match here would be a silent second spelling
            -- of a format, and the compiler emits exactly one.
            forM_ ["PNG", "Png", " png", "png "] $ \token →
                parse (withFormat (str token))
                    `shouldReject` "unsupported storage_format"

        it "refuses a non-string representation rather than coercing it" $
            forM_ ["1", "true", "null", "[\"png\"]", "{}"] $ \token →
                parse (withFormat token) `shouldReject` "malformed"

        it "never infers the representation from the atlas path" $ do
            -- The artifact really is the .png the compiler wrote; only
            -- the DECLARED format is unknown. An extension-sniffing
            -- fallback would accept this, which is exactly the guess
            -- D-10 forbids.
            let doc = indexWith [] [setField "storage_format" (str "ktx2")
                                        idleFields]
            lookup "atlas_path" idleFields
                `shouldBe` Just (str "assets/textures/units/acolyte/atlas/idle.png")
            parse doc `shouldReject` "unsupported storage_format 'ktx2'"

        it "rejects the WHOLE unit when one animation's representation is \
           \unreadable" $ do
            -- No partial publication and no synthetic fallback: an
            -- index whose OTHER animation is a perfectly good PNG must
            -- still yield nothing, or a unit would register missing an
            -- animation its YAML declares.
            let mixed = indexWith []
                    [idleFields, setField "storage_format" (str "ktx2")
                                     swingFields]
            parse mixed `shouldReject` "unsupported storage_format 'ktx2'"
            rejection (parse mixed) `shouldSatisfy` T.isInfixOf "swing"

        it "names the animation whose representation it could not read" $ do
            -- The first record is the good one, so a diagnostic naming
            -- 'idle' would be reporting the wrong animation.
            let msg = rejection (parse (indexWith []
                          [idleFields, setField "storage_format" (str "ktx2")
                                           swingFields]))
            msg `shouldSatisfy` T.isInfixOf "swing"
            msg `shouldSatisfy` not ∘ T.isInfixOf "'idle'"
    describe "Unit.Atlas.Index — a malformed index is rejected, never sampled" $ do
        it "rejects bytes that are not JSON" $
            parse "not json at all" `shouldReject` "not valid JSON"

        it "rejects a truncated document" $
            parse (BL.take 60 goodIndex) `shouldReject` "not valid JSON"

        it "rejects an unsupported schema_version" $
            parse (indexWith [("schema_version", "3")] [idleFields])
                `shouldReject` "unsupported index schema_version 3"

        -- #2076's format bump, tested against a document that really is
        -- the previous schema — edge-adjacent dimensions and no
        -- `cell_padding` at all — not merely a v2 one with the number
        -- changed. The VERSION must be the reported cause: the field
        -- v1 legitimately lacks is exactly what a decode-then-check
        -- order would blame instead, which would send a reader looking
        -- for a corrupt index rather than an outdated one.
        it "rejects a genuine schema-v1 index on its VERSION, not its fields" $ do
            let v1 = indexWith
                    [("schema_version", "1"), ("tool_version", "1")]
                    [ dropField "cell_padding"
                        (setField "atlas_width" "128"
                            (setField "atlas_height" "240" idleFields)) ]
                msg = rejection (parse v1)
            msg `shouldSatisfy` T.isInfixOf "unsupported index schema_version 1"
            msg `shouldSatisfy` T.isInfixOf "pack_atlas.py --compile"
            msg `shouldSatisfy` (not ∘ T.isInfixOf "cell_padding")
            msg `shouldSatisfy` (not ∘ T.isInfixOf "malformed")

        it "rejects an index that omits the required cell_padding" $
            parse (indexWith [] [dropField "cell_padding" idleFields])
                `shouldReject` "malformed"

        it "rejects a cell_padding this build does not implement" $ do
            parse (indexWith [] [setField "cell_padding" "0" idleFields])
                `shouldReject` "cell_padding 0 is not this build's one supported"
            parse (indexWith [] [setField "cell_padding" "2" idleFields])
                `shouldReject` "cell_padding 2 is not this build's one supported"

        it "rejects an unsupported digest_algorithm" $
            parse (indexWith [("digest_algorithm", str "md5")] [idleFields])
                `shouldReject` "unsupported digest_algorithm"

        it "rejects an index belonging to another unit" $
            parse (indexWith [("unit", str "bear_brown")] [idleFields])
                `shouldReject` "declares unit 'bear_brown'"

        it "rejects an index that declares no animations" $
            parse (indexWith [] []) `shouldReject` "no animations"

        it "rejects duplicate animation names" $
            parse (indexWith [] [idleFields, idleFields])
                `shouldReject` "duplicate animation names"

        it "rejects a missing required field" $
            parse (indexWith [] [dropField "cell_width" idleFields])
                `shouldReject` "malformed"

        it "rejects an unsupported storage format" $
            parse (indexWith [] [setField "storage_format" (str "ktx2") idleFields])
                `shouldReject` "unsupported storage_format 'ktx2'"

        it "names the unit, the animation, AND the artifact" $ do
            let msg = rejection (parse (indexWith []
                          [setField "storage_format" (str "ktx2") idleFields]))
            msg `shouldSatisfy` T.isInfixOf "acolyte"
            msg `shouldSatisfy` T.isInfixOf "idle"
            msg `shouldSatisfy` T.isInfixOf "index.json"

        it "rejects a non-positive dimension" $ do
            parse (indexWith [] [setField "cell_width" "0" idleFields])
                `shouldReject` "cell_width must be positive"
            parse (indexWith [] [setField "atlas_height" "-48" idleFields])
                `shouldReject` "atlas_height must be positive"

        it "rejects a grid that would address pixels outside the atlas" $ do
            parse (indexWith [] [setField "columns" "5" idleFields])
                `shouldReject` "exceeds atlas_width"
            parse (indexWith [] [setField "rows" "6" idleFields])
                `shouldReject` "exceeds atlas_height"

        -- Containment strides by the padded SLOT, not the logical cell.
        -- A sheet sized for four edge-adjacent 32-wide cells (128) is
        -- one texel short of holding four padded ones (136), and the
        -- shortfall is entirely gutter — so a check that measured cells
        -- alone would accept a sheet whose last column's extrusion runs
        -- off the right edge.
        it "measures containment by the padded slot, not the bare cell" $ do
            parse (indexWith [] [setField "atlas_width" "128" idleFields])
                `shouldReject` "exceeds atlas_width"
            parse (indexWith [] [setField "atlas_height" "240" idleFields])
                `shouldReject` "exceeds atlas_height"

        it "rejects a non-positive or non-finite fps" $ do
            parse (indexWith [] [setField "fps" "0" idleFields])
                `shouldReject` "fps must be a positive finite number"
            parse (indexWith [] [setField "fps" "-8" idleFields])
                `shouldReject` "fps must be a positive finite number"
            -- JSON has no infinity literal, but an exponent this large
            -- decodes to one in the Float the engine holds.
            parse (indexWith [] [setField "fps" "1e400" idleFields])
                `shouldReject` "fps must be a positive finite number"

        it "rejects an unknown direction name" $
            parse (indexWith []
                [setField "directions"
                    (arr [directionEntry "up" 0 4]) idleFields])
                `shouldReject` "unknown direction 'up'"

        it "rejects a row outside the animation's row count" $
            parse (indexWith []
                [setField "directions"
                    (arr [directionEntry "south" 5 4]) idleFields])
                `shouldReject` "outside the animation's 5 rows"

        it "rejects two directions sharing one row" $
            parse (indexWith []
                [setField "directions"
                    (arr [ directionEntry "south" 0 4
                         , directionEntry "north" 0 4 ]) idleFields])
                `shouldReject` "same row"

        it "rejects a duplicated direction" $
            parse (indexWith []
                [setField "directions"
                    (arr [ directionEntry "south" 0 4
                         , directionEntry "south" 1 4 ]) idleFields])
                `shouldReject` "more than once"

        -- D-5: the real count is the frame authority, so a count above
        -- the row's capacity would make padding — or off-sheet pixels —
        -- addressable as a frame.
        it "rejects a frame_count above the row capacity" $
            parse (indexWith []
                [setField "directions"
                    (arr [directionEntry "south" 0 5]) idleFields])
                `shouldReject` "above the animation's 4 columns"

        it "rejects a zero frame_count" $
            parse (indexWith []
                [setField "directions"
                    (arr [directionEntry "south" 0 0]) idleFields])
                `shouldReject` "frame_count 0"

        it "rejects an animation with no directions at all" $
            parse (indexWith [] [setField "directions" (arr []) idleFields])
                `shouldReject` "no directions"

        it "rejects an empty digest" $
            parse (indexWith [] [setField "atlas_digest" (str "") idleFields])
                `shouldReject` "atlas_digest is empty"

        -- D-2 is one atlas per ANIMATION. Two animations naming one
        -- file each validate on their own, and the upload path would
        -- then legitimately alias the second onto the first's image and
        -- bindless slot — two animations reading one sheet.
        it "rejects two animations sharing one atlas_path" $
            let stepAsIdle = setField "atlas_path"
                    (str "assets/textures/units/acolyte/atlas/idle.png")
                    swingFields
            in parse (indexWith [] [idleFields, stepAsIdle])
                `shouldReject` "one atlas_path for more than one animation"

        -- The rule that makes the collision unreachable in the first
        -- place: the file is the animation's own canonical name, which
        -- is exactly what the compiler emits.
        it "rejects an atlas_path that is not the animation's canonical file" $ do
            parse (indexWith [] [setField "atlas_path"
                    (str "assets/textures/units/acolyte/atlas/walk.png")
                    idleFields])
                `shouldReject` "is not this animation's canonical atlas"
            parse (indexWith [] [setField "atlas_path"
                    (str "assets/textures/units/acolyte/atlas/idle.PNG")
                    idleFields])
                `shouldReject` "is not this animation's canonical atlas"

        it "accepts the canonical file the compiler emits" $
            case parse goodIndex of
                Right anims → map aaPath anims `shouldBe`
                    [ "assets/textures/units/acolyte/atlas/idle.png"
                    , "assets/textures/units/acolyte/atlas/swing.png" ]
                Left e → expectationFailure (T.unpack (renderAtlasLoadError e))

        -- A corrupt index must not be able to make the engine load an
        -- arbitrary file.
        it "rejects an atlas_path that escapes the unit's atlas directory" $ do
            let esc p = parse (indexWith []
                    [setField "atlas_path" (str p) idleFields])
            esc "assets/textures/units/acolyte/atlas/../../../secret.png"
                `shouldReject` "not a plain file"
            esc "/etc/passwd" `shouldReject` "not a plain file"
            esc "assets/textures/units/bear_brown/atlas/idle.png"
                `shouldReject` "not a plain file"
            esc "assets/textures/units/acolyte/atlas/nested/idle.png"
                `shouldReject` "not a plain file"

    -- The whole generated schema is the contract, not just the parts
    -- this build consumes: a truncated document is truncated.
    describe "Unit.Atlas.Index — every generated top-level field is required" $ do
        it "rejects a document missing any one of them" $
            forM_ [ "schema_version", "generator", "tool_version"
                  , "digest_algorithm", "unit", "direction_order"
                  , "animations" ] $ \field →
                parse (indexWithout field) `shouldSatisfy` isRejected

        it "rejects an empty generator" $
            parse (indexWith [("generator", str "  ")] [idleFields])
                `shouldReject` "generator is empty"

        it "rejects a negative tool_version" $
            parse (indexWith [("tool_version", "-1")] [idleFields])
                `shouldReject` "tool_version -1 is negative"

        it "rejects a non-numeric tool_version rather than defaulting" $
            parse (indexWith [("tool_version", str "one")] [idleFields])
                `shouldReject` "malformed"

        -- The row order is documentation here — rows are read
        -- explicitly — but a document declaring a DIFFERENT order came
        -- from a compiler whose layout this build does not share.
        it "rejects a direction_order that is not this build's row order" $ do
            parse (indexWith [("direction_order", arr (map str
                    [ "south", "west", "south-west", "north-west"
                    , "north", "north-east", "east", "south-east" ]))]
                    [idleFields])
                `shouldReject` "is not this build's row order"
            parse (indexWith [("direction_order", arr (map str
                    ["south", "north"]))] [idleFields])
                `shouldReject` "is not this build's row order"
