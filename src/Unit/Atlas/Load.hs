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
--   ALL OR NOTHING, BEFORE PUBLICATION. 'loadUnitAtlasIndex' decodes
--   and checks EVERY declared animation before returning any of them,
--   so a caller never allocates a handle, queues an upload, or
--   publishes an 'Animation' for a unit whose index turns out to be
--   broken three animations later.
--
--   No unit ships a compiled index yet (#1258 requirement 7 keeps
--   production atlases uncommitted until TEX-4), so on today's asset
--   tree every unit takes the 'Nothing' branch and nothing about
--   loading changes.
module Unit.Atlas.Load
    ( loadUnitAtlasIndex
    ) where

import UPrelude
import qualified Codec.Picture as JP
import qualified Data.ByteString as BS
import qualified Data.ByteString.Internal as BSI
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import qualified Data.Vector.Storable as SV
import Control.Exception (SomeException, try)
import System.Directory (doesFileExist)
import Unit.Atlas.Index
import Unit.Atlas.Types

-- | Read one unit's compiled atlas index, if it has one.
--
--   * @Right Nothing@ — the unit has no index: every animation is
--     legacy. This is the ONLY tolerated absence, and it is the state
--     every shipped unit is in today.
--   * @Right (Just anims)@ — the index parsed, validated, and each
--     declared atlas decoded to exactly the image it describes.
--   * @Left err@ — the index exists but is unusable. Nothing about
--     this unit's animations may be published.
loadUnitAtlasIndex ∷ Text → IO (Either AtlasLoadError (Maybe [AtlasAnimation]))
loadUnitAtlasIndex unit = do
    let indexPath = unitAtlasIndexPath unit
    present ← doesFileExist indexPath
    if not present
        then pure (Right Nothing)
        else do
            eRaw ← readFileBytes indexPath
            case eRaw of
                Left msg → pure ∘ Left $ AtlasLoadError
                    { aleUnit = unit, aleAnimation = Nothing
                    , aleArtifact = indexPath
                    , aleReason = "cannot read index: " <> msg }
                Right raw → case parseAtlasIndex unit indexPath raw of
                    Left err → pure (Left err)
                    Right anims → do
                        checked ← checkAll unit anims
                        pure (Just anims <$ checked)

-- | Decode and check every declared atlas. Stops at the first failure —
--   there is nothing useful to do with the rest of a broken index.
checkAll ∷ Text → [AtlasAnimation] → IO (Either AtlasLoadError ())
checkAll _ [] = pure (Right ())
checkAll unit (a:rest) = do
    r ← checkOne unit a
    case r of
        Left err → pure (Left err)
        Right () → checkAll unit rest

checkOne ∷ Text → AtlasAnimation → IO (Either AtlasLoadError ())
checkOne unit anim = do
    let path = aaPath anim
        reject reason = Left AtlasLoadError
            { aleUnit = unit, aleAnimation = Just (aaName anim)
            , aleArtifact = path, aleReason = reason }
    present ← doesFileExist path
    if not present
        then pure (reject "indexed atlas is missing from disk")
        else do
            eImg ← decodeAtlas path
            pure $ case eImg of
                Left msg → reject ("cannot decode atlas: " <> msg)
                Right (w, h, pixels) →
                    validateAtlasImage unit anim w h pixels

-- | Decode an atlas to its canonical RGBA8 samples — the same
--   'JP.convertRGBA8' normalization the upload path
--   ('Engine.Scripting.Lua.Message.Texture') applies, and the one the
--   compiler digested. Anything else would compare a different image
--   to the recorded digest.
--
--   Only 'AtlasFormatPng' reaches here today; a later transcoded format
--   (TEX-5) adds its own decode beside this one rather than replacing
--   it, which is what D-10's format neutrality buys.
decodeAtlas ∷ FilePath → IO (Either Text (Int, Int, BS.ByteString))
decodeAtlas path = do
    r ← try (JP.readImage path) ∷ IO (Either SomeException (Either String JP.DynamicImage))
    pure $ case r of
        Left e          → Left (T.pack (show e))
        Right (Left m)  → Left (T.pack m)
        Right (Right d) →
            let img = JP.convertRGBA8 d
            in Right ( JP.imageWidth img
                     , JP.imageHeight img
                     , packRGBA8 (JP.imageData img) )

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
