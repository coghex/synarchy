{-# LANGUAGE Strict #-}
-- | Locating, reading, and fully validating one unit's compiled atlas
--   artifacts before any of them is published or given GPU residency
--   (#1259, TEX-3).
--
--   MODE SELECTION IS THE INDEX. A unit's compiled index
--   (@assets\/textures\/units\/\<unit\>\/atlas\/index.json@) is the
--   explicit, deterministic declaration of which animations are
--   atlas-backed: an animation named there loads as an atlas, and every
--   other animation of that unit loads as legacy per-frame textures.
--   The two are never mixed WITHIN one animation (requirement 6) —
--   'Unit.Atlas.Types.AnimStorage' cannot represent a mixture.
--
--   FAILURE IS FAILURE. A selected atlas mode that is missing, stale,
--   unsupported, or invalid does NOT fall back to legacy frames: it
--   rejects, naming the unit, the animation and the artifact. Silently
--   loading the per-frame path instead would hide a broken compile
--   behind art that still renders, which is precisely the substitution
--   requirement 5 forbids.
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
--     3. each atlas decodes to the image the index describes, AND every
--        declared SOURCE frame decodes to exactly the pixels its atlas
--        cell holds ('Unit.Atlas.Index.validateSourceFrame').
--
--   Pass 3 is what catches a source PNG repainted while its compiled
--   atlas and index were left in place: the atlas is still internally
--   consistent and its own digest still matches, so nothing short of
--   reading the source art can see it. Together, passes 2 and 3 verify
--   every input the compiler's @source_digest@ is taken over — see
--   'Unit.Atlas.Index.validateSourceFrame' for why they verify those
--   inputs directly instead of recomputing the digest.
--
--   Reading the source frames here is a MIGRATION-PHASE cost, and an
--   accepted one: the legacy per-frame path is still live (requirement
--   6) and every unit still ships its frames. TEX-6, which removes
--   source loading, is where this check's replacement becomes the
--   compile-time gate's alone.
--
--   ALL OR NOTHING, BEFORE PUBLICATION. Every declared animation is
--   checked before any of them is returned, so a caller never allocates
--   a handle, queues an upload, or publishes an 'Animation' for a unit
--   whose index turns out to be broken three animations later.
--
--   No unit ships a compiled index yet (#1258 requirement 7 keeps
--   production atlases uncommitted until TEX-4), so on today's asset
--   tree every unit takes the 'Nothing' branch and nothing about
--   loading changes.
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
import System.Directory (doesFileExist)
import System.FilePath ((</>))
import Unit.Atlas.Index
import Unit.Atlas.Types

-- | Read, validate, and select one unit's compiled atlas animations.
--
--   * @Right Nothing@ — the unit has no index: every animation is
--     legacy. This is the ONLY tolerated absence, and it is the state
--     every shipped unit is in today.
--   * @Right (Just m)@ — every declared animation parsed, still matches
--     the YAML, decoded to the image its index describes, and holds its
--     declared source art.
--   * @Left err@ — the index exists but is unusable. Nothing about this
--     unit's animations may be published.
loadUnitAtlasIndex
    ∷ Text
    → Map.Map Text YamlAnimFacts     -- ^ what the unit YAML declares
    → IO (Either AtlasLoadError (Maybe (HM.HashMap Text AtlasAnimation)))
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
    → IO (Either AtlasLoadError (Maybe (HM.HashMap Text AtlasAnimation)))
loadUnitAtlasIndexIn root unit yamlAnims = do
    let indexPath = unitAtlasIndexPath unit
    present ← doesFileExist (under root indexPath)
    if not present
        then pure (Right Nothing)
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
                            pure (Just plan <$ checked)

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
        Just ya → go (Map.toList (aaDirections anim)) ya
  where
    miss reason = AtlasLoadError
        { aleUnit = unit, aleAnimation = Just (aaName anim)
        , aleArtifact = aaPath anim, aleReason = reason }

    go [] _ = pure (Right ())
    go ((dir, row) : rest) ya =
        case Map.lookup dir (yafFrames ya) of
            Nothing → pure (Left (miss ("the unit YAML declares no frames for "
                                        <> T.pack (show dir))))
            Just paths → do
                r ← goFrames dir row (zip [0 ..] paths)
                case r of
                    Left err → pure (Left err)
                    Right () → go rest ya

    goFrames _ _ [] = pure (Right ())
    goFrames dir row ((col, framePath) : rest) = do
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
        Left e          → Left (T.pack (show e))
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
        Left e   → Left (T.pack (show e))
        Right bs → Right (BL.fromStrict bs)
