{-# LANGUAGE Strict #-}
-- | Locating, reading, and fully validating one unit's compiled atlas
--   artifacts before any of them is published or given GPU residency
--   (#1259, TEX-3).
--
--   THE INDEX IS THE WHOLE ANSWER. A unit's compiled index
--   (@assets\/textures\/units\/\<unit\>\/atlas\/index.json@) describes
--   every animation that unit's YAML declares, and since #1261 (TEX-6)
--   retired per-frame unit-animation loading there is no second
--   representation for one to be in: the index either covers the YAML
--   exactly or the unit does not load.
--
--   FAILURE IS FAILURE. Artifacts that are missing, incomplete, stale,
--   unsupported, or invalid REJECT, naming the unit, the animation and
--   the artifact. There has never been a fallback to the source frames
--   sitting beside them, and now there is nothing to fall back to.
--
--   FRESHNESS IS CHECKED AGAINST THE SOURCE ART, not just the index.
--   'loadUnitAtlasIndex' runs three passes, cheapest first, and stops at
--   the first failure:
--
--     1. the index parses and is structurally sound
--        ('Unit.Atlas.Index.parseAtlasIndex');
--     2. it still describes what the unit YAML declares — animation set,
--        @fps@ \/ @loop@ \/ @flip@, direction set, per-direction frame
--        counts, columns ('Unit.Atlas.Index.planUnitAtlasStorage');
--     3. each atlas decodes to the image the index describes; every
--        declared SOURCE frame decodes to exactly the pixels its atlas
--        cell holds, and its slot carries the one-texel extrusion ring
--        compiled from that same frame
--        ('Unit.Atlas.Index.validateSourceFrame'); and the
--        animation's whole @source_digest@ is recomputed from those
--        same inputs and compared
--        ('Unit.Atlas.Index.validateSourceDigest').
--
--   Pass 3 is what catches a source PNG repainted while its compiled
--   atlas and index were left in place: the atlas is still internally
--   consistent and its own digest still matches, so nothing short of
--   reading the source art can see it. The per-frame comparison runs
--   BEFORE the digest so a stale artifact is reported against one
--   direction and one frame; the digest then closes what a per-field
--   comparison cannot — a forged digest, and a frame whose PATH changed
--   while its pixels did not.
--
--   Pass 3 SURVIVED TEX-6, which #1259 expected to retire it. Its cost
--   is the whole reason it was provisional, and that cost was measured
--   here rather than assumed: decoding the shipped gameplay corpus totals
--   ~1.8 s of one-time unit-def loading (bear_brown, the largest, 0.74 s),
--   paid on the Lua thread while
--   YAMLs load and not on any frame. Since the source PNGs remain the
--   tracked, hand-edited artwork (D-1), they remain something a
--   developer can repaint without recompiling, and CI's asset gate only
--   runs on a push — so this stays the check that catches it locally,
--   in the same run that would otherwise have drawn the stale art.
--   'tools/pack_atlas.py --validate-only --strict' is the same
--   comparison at the compile boundary, not a replacement for it.
--
--   ALL OR NOTHING, BEFORE PUBLICATION. Every declared animation is
--   checked before any of them is returned, so a caller never allocates
--   a handle, queues an upload, or publishes an 'Animation' for a unit
--   whose index turns out to be broken three animations later.
--
--   Every shipped GAMEPLAY unit has a compiled index: @acolyte@ from
--   #1260 (TEX-4)'s pilot and the original remaining six from #1261
--   (TEX-6). Asset-only declarations are validated and previewed but never
--   reach this gameplay registration boundary.
module Unit.Atlas.Load
    ( loadUnitAtlasIndex
    , loadUnitAtlasIndexIn
    , decodeImageFile
    ) where

import UPrelude
import qualified Codec.Picture as JP
import qualified Data.ByteString as BS
import qualified Data.ByteString.Internal as BSI
import qualified Data.ByteString.Lazy as BL
import qualified Data.HashMap.Strict as HM
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import qualified Data.Vector.Storable as SV
import Control.Exception (SomeException, try)
import System.Directory (doesDirectoryExist, doesFileExist)
import System.FilePath ((</>))
import Unit.Atlas.Index
import Unit.Atlas.Types

-- | Read, validate, and select one unit's compiled atlas animations.
--
--   * @Right m@ — every declared animation parsed, still matches the
--     YAML, decoded to the image its index describes, and holds its
--     declared source art. @m@ covers EXACTLY the animations the unit
--     YAML declares (see 'planUnitAtlasStorage').
--   * @Right mempty@ — the unit declares no animations at all and ships
--     no @atlas\/@ directory. That is the one absence left: with the
--     per-frame path retired (#1261) a unit that declares animations
--     but ships no compiled artifacts has nothing to render them from.
--   * @Left err@ — the artifacts are missing, incomplete, or unusable.
--     Nothing about this unit's animations may be published.
loadUnitAtlasIndex
    ∷ Text
    → Map.Map Text YamlAnimFacts     -- ^ what the unit YAML declares
    → IO (Either AtlasLoadError (HM.HashMap Text AtlasAnimation))
loadUnitAtlasIndex = loadUnitAtlasIndexIn ""

-- | 'loadUnitAtlasIndex' against an explicit filesystem ROOT.
--
--   Production passes @\"\"@: every resource path is already relative to
--   the resource root the executable chdir'd into ("App.ResourceRoot"),
--   so prefixing nothing is the real behaviour. A root is supplied only
--   to point the loader at a fixture tree, which is what makes the
--   whole read-parse-decode-verify pipeline testable without a live
--   engine. The root prefixes only where a file is OPENED — declared
--   paths, and therefore every diagnostic, stay resource-relative.
loadUnitAtlasIndexIn
    ∷ FilePath
    → Text
    → Map.Map Text YamlAnimFacts
    → IO (Either AtlasLoadError (HM.HashMap Text AtlasAnimation))
loadUnitAtlasIndexIn root unit yamlAnims = do
    let indexPath = unitAtlasIndexPath unit
        atlasDir  = unitAtlasDir unit
    present ← doesFileExist (under root indexPath)
    dirPresent ← doesDirectoryExist (under root atlasDir)
    if not present
        then pure $ if not dirPresent ∧ Map.null yamlAnims
            -- A unit with no animations at all needs no artifacts, and
            -- the compiler writes it none.
            then Right HM.empty
            else Left (missingIndex indexPath atlasDir dirPresent)
        else do
            eRaw ← readFileBytes (under root indexPath)
            case eRaw of
                Left msg → pure ∘ Left $ AtlasLoadError
                    { aleUnit = unit, aleAnimation = Nothing
                    , aleArtifact = indexPath
                    , aleReason = "cannot read index: " <> msg }
                Right raw → case parseAtlasIndex unit indexPath raw of
                    Left err → pure (Left err)
                    Right anims → case planUnitAtlasStorage unit yamlAnims anims of
                        Left err → pure (Left err)
                        Right plan → do
                            checked ← checkAll root unit yamlAnims anims
                            pure (plan <$ checked)
  where
    -- No index. Before #1261 an ABSENT `atlas/` directory meant "this
    -- unit is on the per-frame path"; there is no such path any more,
    -- so the only thing an absent index can mean is that the unit's
    -- artifacts were never compiled — and an atlas DIRECTORY without
    -- its index is the wreckage of an interrupted or partly deleted
    -- compile, which the compiler never writes. Both reject; the
    -- diagnostic distinguishes them because the repairs differ.
    missingIndex indexPath atlasDir dirPresent = AtlasLoadError
        { aleUnit = unit, aleAnimation = Nothing
        , aleArtifact = indexPath
        , aleReason = if dirPresent
            then "the unit has a compiler-owned " <> T.pack atlasDir
                <> " directory but no index; the compiled artifacts are "
                <> "incomplete — re-run tools/pack_atlas.py --compile"
            else "the unit declares "
                <> tshow (Map.size yamlAnims)
                <> " animation(s) but ships no compiled atlas artifacts; "
                <> "unit animations are atlas-backed only (#1261) — run "
                <> "tools/pack_atlas.py --compile" }

-- | Resolve a resource-relative path for OPENING only.
under ∷ FilePath → FilePath → FilePath
under root p = if null root then p else root </> p

-- | Decode and check every declared atlas. Stops at the first failure —
--   there is nothing useful to do with the rest of a broken index.
checkAll
    ∷ FilePath → Text → Map.Map Text YamlAnimFacts → [AtlasAnimation]
    → IO (Either AtlasLoadError ())
checkAll _ _ _ [] = pure (Right ())
checkAll root unit yamlAnims (a:rest) = do
    r ← checkOne root unit yamlAnims a
    case r of
        Left err → pure (Left err)
        Right () → checkAll root unit yamlAnims rest

checkOne
    ∷ FilePath → Text → Map.Map Text YamlAnimFacts → AtlasAnimation
    → IO (Either AtlasLoadError ())
checkOne root unit yamlAnims anim = do
    let path = aaPath anim
        reject reason = Left AtlasLoadError
            { aleUnit = unit, aleAnimation = Just (aaName anim)
            , aleArtifact = path, aleReason = reason }
    present ← doesFileExist (under root path)
    if not present
        then pure (reject "indexed atlas is missing from disk")
        else do
            eImg ← decodeImageFile (under root path)
            case eImg of
                Left msg → pure (reject ("cannot decode atlas: " <> msg))
                Right atlas → case validateAtlasImage unit anim atlas of
                    Left err → pure (Left err)
                    Right () → checkSourceFrames root unit yamlAnims anim atlas

-- | Pass 3: every declared source frame must decode to exactly the
--   pixels its atlas cell holds.
checkSourceFrames
    ∷ FilePath → Text → Map.Map Text YamlAnimFacts → AtlasAnimation
    → DecodedImage → IO (Either AtlasLoadError ())
checkSourceFrames root unit yamlAnims anim atlas =
    -- `planUnitAtlasStorage` already proved the animation is declared
    -- and that its direction set and per-direction counts agree, so
    -- every lookup below is total; a missing one would be a caller
    -- ordering bug, and reporting it is more useful than assuming it
    -- cannot happen.
    case Map.lookup (aaName anim) yamlAnims of
        Nothing → pure (Left (miss "the unit YAML declares no such animation"))
        Just ya → do
            -- `Map.toAscList` over a `Map Direction _` is already the
            -- compiler's atlas row order (the engine's own constructor
            -- order restricted to authored directions), which is the
            -- order `sourceDigest` requires.
            r ← go (Map.toAscList (aaDirections anim)) ya []
            pure $ case r of
                Left err → Left err
                Right dirInputs →
                    validateSourceDigest unit anim SourceAnimInput
                        { saiUnit        = unit
                        , saiName        = aaName anim
                        , saiFlip        = aaFlip anim
                        , saiLoop        = aaLoop anim
                        , saiFps         = aaFps anim
                        , saiCellWidth   = aaCellWidth anim
                        , saiCellHeight  = aaCellHeight anim
                        , saiCellPadding = aaCellPadding anim
                        , saiColumns     = aaColumns anim
                        , saiDirections  = reverse dirInputs
                        }
  where
    miss reason = AtlasLoadError
        { aleUnit = unit, aleAnimation = Just (aaName anim)
        , aleArtifact = aaPath anim, aleReason = reason }

    go [] _ acc = pure (Right acc)
    go ((dir, row) : rest) ya acc =
        case Map.lookup dir (yafFrames ya) of
            Nothing → pure (Left (miss ("the unit YAML declares no frames for "
                                        <> tshow dir)))
            Just paths → do
                r ← goFrames dir row (zip [0 ..] paths) []
                case r of
                    Left err → pure (Left err)
                    Right frames → go rest ya
                        (SourceDirectionInput
                            { sdiDirection = indexDirectionToken dir
                            , sdiRow = adrRow row
                            , sdiFrames = reverse frames } : acc)

    goFrames _ _ [] acc = pure (Right acc)
    goFrames dir row ((col, framePath) : rest) acc = do
        present ← doesFileExist (under root framePath)
        if not present
            then pure ∘ Left $ AtlasLoadError
                { aleUnit = unit, aleAnimation = Just (aaName anim)
                , aleArtifact = framePath
                , aleReason = "declared source frame is missing from disk" }
            else do
                eImg ← decodeImageFile (under root framePath)
                case eImg of
                    Left msg → pure ∘ Left $ AtlasLoadError
                        { aleUnit = unit, aleAnimation = Just (aaName anim)
                        , aleArtifact = framePath
                        , aleReason = "cannot decode source frame: " <> msg }
                    Right frame →
                        case validateSourceFrame unit anim atlas dir
                                 (adrRow row) col framePath frame of
                            Left err → pure (Left err)
                            Right () → goFrames dir row rest
                                (SourceFrameInput
                                    { sfiPath = T.pack framePath
                                    , sfiWidth = diWidth frame
                                    , sfiHeight = diHeight frame
                                    , sfiPixels = diPixels frame } : acc)

-- | Decode an image to its canonical RGBA8 samples — the same
--   'JP.convertRGBA8' normalization the upload path
--   ('Engine.Scripting.Lua.Message.Texture') applies, and the one the
--   compiler digested. Anything else would compare a different image.
--
--   Only 'AtlasFormatPng' reaches here today; a later transcoded format
--   (TEX-5) adds its own decode beside this one rather than replacing
--   it, which is what D-10's format neutrality buys.
decodeImageFile ∷ FilePath → IO (Either Text DecodedImage)
decodeImageFile path = do
    r ← try (JP.readImage path) ∷ IO (Either SomeException (Either String JP.DynamicImage))
    pure $ case r of
        Left e          → Left (tshow e)
        Right (Left m)  → Left (T.pack m)
        Right (Right d) →
            let img = JP.convertRGBA8 d
            in Right (DecodedImage (JP.imageWidth img) (JP.imageHeight img)
                          (packRGBA8 (JP.imageData img)))

-- | Reinterpret JuicyPixels' storable sample vector as bytes without
--   copying it through a list — an atlas is megabytes of RGBA8 and this
--   runs once per animation at load.
packRGBA8 ∷ SV.Vector Word8 → BS.ByteString
packRGBA8 v = let (fp, len) = SV.unsafeToForeignPtr0 v
              in BSI.fromForeignPtr fp 0 len

readFileBytes ∷ FilePath → IO (Either Text BL.ByteString)
readFileBytes path = do
    r ← try (BS.readFile path) ∷ IO (Either SomeException BS.ByteString)
    pure $ case r of
        Left e   → Left (tshow e)
        Right bs → Right (BL.fromStrict bs)
