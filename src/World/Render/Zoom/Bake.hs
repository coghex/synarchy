{-# OPTIONS_GHC -fprof-auto #-}
{-# LANGUAGE Strict, UnicodeSyntax #-}
-- | Bake ZoomChunkEntry vectors into render-ready BakedZoomEntry vectors.
module World.Render.Zoom.Bake
    ( bakeEntries
    , bakeEntriesAtlas
    , ensureBaked
    , ensureBakedAtlas
    ) where

import UPrelude
import Data.IORef (readIORef, writeIORef, IORef)
import qualified Data.Vector as V
import Engine.Asset.Handle (TextureHandle(..))
import Engine.Graphics.Camera (CameraFacing(..))
import Engine.Graphics.Vulkan.Types.Vertex (Vertex(..), Vec2(..), Vec4(..))
import World.Types
import World.Grid (gridToWorld)

bakeEntries ∷ CameraFacing → V.Vector ZoomChunkEntry
            → (Word8 → Int → TextureHandle)
            → (TextureHandle → Int)
            → Float
            → V.Vector BakedZoomEntry
bakeEntries facing cache texPicker lookupSlot defFmSlot =
    V.map bakeOne cache
  where
    bakeOne entry =
        let texHandle = texPicker (zceTexIndex entry) (zceElev entry)
            actualSlot = fromIntegral (lookupSlot texHandle)
            baseGX = zceBaseGX entry
            baseGY = zceBaseGY entry
            (x0,y0) = gridToWorld facing baseGX baseGY
            (x1,y1) = gridToWorld facing (baseGX + chunkSize) baseGY
            (x2,y2) = gridToWorld facing baseGX (baseGY + chunkSize)
            (x3,y3) = gridToWorld facing (baseGX + chunkSize) (baseGY + chunkSize)
            drawX = min x0 (min x1 (min x2 x3))
            drawY = min y0 (min y1 (min y2 y3))
            w = max x0 (max x1 (max x2 x3)) - drawX
            h = max y0 (max y1 (max y2 y3)) - drawY
            white = Vec4 1.0 1.0 1.0 1.0
        in BakedZoomEntry
            { bzeChunkX  = zceChunkX entry
            , bzeChunkY  = zceChunkY entry
            , bzeDrawX   = drawX
            , bzeDrawY   = drawY
            , bzeWidth   = w
            , bzeHeight  = h
            , bzeSortKey = fromIntegral (zceChunkY entry)
                         + fromIntegral (zceChunkX entry) * 0.0001
            , bzeV0      = Vertex (Vec2 drawX drawY)            (Vec2 0 0) white actualSlot defFmSlot
            , bzeV1      = Vertex (Vec2 (drawX + w) drawY)       (Vec2 1 0) white actualSlot defFmSlot
            , bzeV2      = Vertex (Vec2 (drawX + w) (drawY + h)) (Vec2 1 1) white actualSlot defFmSlot
            , bzeV3      = Vertex (Vec2 drawX (drawY + h))       (Vec2 0 1) white actualSlot defFmSlot
            , bzeTexture = texHandle
            , bzeIsOcean = zceIsOcean entry
            , bzeHasLava = zceHasLava entry
            , bzeElev    = zceElev entry
            }

-- | Bake entries using the zoom atlas texture.
--   Each chunk gets UV coordinates into its tile within the atlas.
bakeEntriesAtlas ∷ CameraFacing → V.Vector ZoomChunkEntry
                 → ZoomAtlasInfo
                 → (TextureHandle → Int)
                 → Float
                 → V.Vector BakedZoomEntry
bakeEntriesAtlas facing cache atlasInfo lookupSlot defFmSlot =
    V.imap bakeOne cache
  where
    atlasHandle = zaiTexture atlasInfo
    atlasSlot   = fromIntegral (lookupSlot atlasHandle)
    aw          = fromIntegral (zaiWidth atlasInfo) ∷ Float
    ah          = fromIntegral (zaiHeight atlasInfo) ∷ Float
    cpr         = zaiChunksPerRow atlasInfo
    cs          = fromIntegral zoomTileSize ∷ Float

    bakeOne idx entry =
        let baseGX = zceBaseGX entry
            baseGY = zceBaseGY entry
            (x0,y0) = gridToWorld facing baseGX baseGY
            (x1,y1) = gridToWorld facing (baseGX + chunkSize) baseGY
            (x2,y2) = gridToWorld facing baseGX (baseGY + chunkSize)
            (x3,y3) = gridToWorld facing (baseGX + chunkSize) (baseGY + chunkSize)
            drawX = min x0 (min x1 (min x2 x3))
            drawY = min y0 (min y1 (min y2 y3))
            w = max x0 (max x1 (max x2 x3)) - drawX
            h = max y0 (max y1 (max y2 y3)) - drawY
            white = Vec4 1.0 1.0 1.0 1.0
            -- Atlas UV coordinates for this chunk
            col = idx `mod` cpr
            row = idx `div` cpr
            u0  = fromIntegral col * cs / aw
            v0  = fromIntegral row * cs / ah
            u1  = (fromIntegral col * cs + cs) / aw
            v1  = (fromIntegral row * cs + cs) / ah
        in BakedZoomEntry
            { bzeChunkX  = zceChunkX entry
            , bzeChunkY  = zceChunkY entry
            , bzeDrawX   = drawX
            , bzeDrawY   = drawY
            , bzeWidth   = w
            , bzeHeight  = h
            , bzeSortKey = fromIntegral (zceChunkY entry)
                         + fromIntegral (zceChunkX entry) * 0.0001
            , bzeV0      = Vertex (Vec2 drawX drawY)            (Vec2 u0 v0) white atlasSlot defFmSlot
            , bzeV1      = Vertex (Vec2 (drawX + w) drawY)       (Vec2 u1 v0) white atlasSlot defFmSlot
            , bzeV2      = Vertex (Vec2 (drawX + w) (drawY + h)) (Vec2 u1 v1) white atlasSlot defFmSlot
            , bzeV3      = Vertex (Vec2 drawX (drawY + h))       (Vec2 u0 v1) white atlasSlot defFmSlot
            , bzeTexture = atlasHandle
            , bzeIsOcean = zceIsOcean entry
            , bzeHasLava = zceHasLava entry
            , bzeElev    = zceElev entry
            }

ensureBaked ∷ IORef (V.Vector BakedZoomEntry, WorldTextures, CameraFacing)
              → V.Vector ZoomChunkEntry → WorldTextures
              → CameraFacing
              → (WorldTextures → Word8 → Int → TextureHandle)
              → (TextureHandle → Int) → Float
              → IO (V.Vector BakedZoomEntry)
ensureBaked bakedRef rawCache textures facing texPicker lookupSlot defFmSlot = do
    (existing, bakedWith, bakedFacing) ← readIORef bakedRef
    let texturesChanged = bakedWith ≢ textures
        facingChanged   = bakedFacing ≢ facing
        needsBake = not (V.null rawCache)
                  ∧ (V.null existing ∨ texturesChanged ∨ facingChanged)
    if needsBake
        then do
            let baked = bakeEntries facing rawCache
                            (\mat elev → texPicker textures mat elev)
                            lookupSlot defFmSlot
            writeIORef bakedRef (baked, textures, facing)
            return baked
        else return existing

-- | Like ensureBaked, but uses the atlas texture when available.
--   Falls back to the per-material texture picker when no atlas.
--   Rebakes when: cache empty, textures changed, facing changed,
--   or atlas just became available (atlas handle in baked entries
--   doesn't match current atlas).
ensureBakedAtlas ∷ IORef (V.Vector BakedZoomEntry, WorldTextures, CameraFacing)
                 → V.Vector ZoomChunkEntry → WorldTextures
                 → CameraFacing → Maybe ZoomAtlasInfo
                 → (WorldTextures → Word8 → Int → TextureHandle)
                 → (TextureHandle → Int) → Float
                 → IO (V.Vector BakedZoomEntry)
ensureBakedAtlas bakedRef rawCache textures facing mAtlas texPicker lookupSlot defFmSlot = do
    (existing, bakedWith, bakedFacing) ← readIORef bakedRef
    let texturesChanged = bakedWith ≢ textures
        facingChanged   = bakedFacing ≢ facing
        -- Check if the atlas state changed: either we now have an atlas
        -- that wasn't baked in, or the baked entries use the old texPicker
        atlasChanged = case mAtlas of
            Just atlas →
                -- If baked is empty or first entry uses a different texture
                V.null existing
                ∨ (not (V.null existing)
                   ∧ bzeTexture (V.head existing) ≢ zaiTexture atlas)
            Nothing → False
        needsBake = not (V.null rawCache)
                  ∧ (V.null existing ∨ texturesChanged
                     ∨ facingChanged ∨ atlasChanged)
    if needsBake
        then do
            let baked = case mAtlas of
                    Just atlas → bakeEntriesAtlas facing rawCache atlas
                                     lookupSlot defFmSlot
                    Nothing    → bakeEntries facing rawCache
                                     (\mat elev → texPicker textures mat elev)
                                     lookupSlot defFmSlot
            writeIORef bakedRef (baked, textures, facing)
            return baked
        else return existing
