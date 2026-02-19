{-# LANGUAGE Strict, UnicodeSyntax #-}
module World.Render.Quads
    ( renderWorldQuads
    ) where

import UPrelude
import qualified Data.HashMap.Strict as HM
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector as V
import Data.Maybe (isJust)
import Data.IORef (readIORef)
import Engine.Core.State (EngineEnv(..))
import Engine.Scene.Types (SortableQuad(..))
import Engine.Graphics.Camera (CameraFacing(..), Camera2D(..))
import Engine.Graphics.Vulkan.Texture.Types (BindlessTextureSystem(..))
import Engine.Graphics.Vulkan.Texture.Bindless (getTextureSlotIndex)
import World.Types
import World.Fluids (FluidCell(..), FluidType(..), seaLevel)
import World.Generate (chunkToGlobal, viewDepth)
import World.Grid (gridToScreen, tileSideHeight, tileWidth, tileHeight)
import World.Render.ViewBounds (computeViewBounds, isTileVisible)
import World.Render.ChunkCulling (isChunkRelevantForSlice, isChunkVisibleWrapped)
import World.Render.TileQuads
    ( tileToQuad
    , blankTileToQuad
    , oceanTileToQuad
    , lavaTileToQuad
    , freshwaterTileToQuad
    )

-----------------------------------------------------------
-- Render World Quads
-----------------------------------------------------------

renderWorldQuads ∷ EngineEnv → WorldState → Float → WorldCameraSnapshot
  → IO (V.Vector SortableQuad)
renderWorldQuads env worldState zoomAlpha snap = do
    tileData ← readIORef (wsTilesRef worldState)
    textures ← readIORef (wsTexturesRef worldState)
    paramsM ← readIORef (wsGenParamsRef worldState)
    camera ← readIORef (cameraRef env)

    let (fbW, fbH) = wcsFbSize snap
        facing = camFacing camera

    mBindless ← readIORef (textureSystemRef env)
    defFmSlotWord ← readIORef (defaultFaceMapSlotRef env)
    let lookupSlot texHandle = fromIntegral $ case mBindless of
            Just bindless → getTextureSlotIndex texHandle bindless
            Nothing       → 0
        defFmSlot = fromIntegral defFmSlotWord
        lookupFmSlot texHandle =
            let s = lookupSlot texHandle
            in if s ≡ 0 then defFmSlot else fromIntegral s
        worldSize = case paramsM of
                      Nothing → 128
                      Just params → wgpWorldSize params
    let zSlice = camZSlice camera
        zoom   = camZoom camera
        chunks = HM.elems (wtdChunks tileData)
        (camX, _camY) = camPosition camera

        effectiveDepth = min viewDepth (max 8 (round (zoom * 80.0 + 8.0 ∷ Float)))

        vb = computeViewBounds camera fbW fbH effectiveDepth

        visibleChunksWithOffset =
            [ (lc, offset)
            | lc ← chunks
            , isChunkRelevantForSlice zSlice lc
            , Just offset ← [isChunkVisibleWrapped facing worldSize vb camX (lcCoord lc)]
            ]

    let chunkVectors = map (\(lc, xOffset) →
            let coord  = lcCoord lc
                tileMap = lcTiles lc
                surfMap = lcSurfaceMap lc
                fluidMap = lcFluidMap lc
                chunkHasFluid = V.any isJust fluidMap
                terrainSurfMap = lcTerrainSurfaceMap lc

                !realQuads = V.ifoldl' (\acc idx col →
                        let lx = idx `mod` chunkSize
                            ly = idx `div` chunkSize
                            mFluid = fluidMap V.! idx
                            (gx, gy) = chunkToGlobal coord lx ly
                            (rawX, rawY) = gridToScreen facing gx gy
                            isUnderLava = case mFluid of
                                Just fc → fcType fc ≡ Lava ∧ fcSurface fc > zSlice - effectiveDepth
                                Nothing → False

                            -- Clamp z iteration to the intersection of
                            -- the render window and the column's actual range
                            colLen  = VU.length (ctMats col)
                            colMinZ = ctStartZ col
                            colMaxZ = colMinZ + colLen - 1
                            zLo = max (zSlice - effectiveDepth) colMinZ
                            zHi = min zSlice colMaxZ

                        in if colLen ≡ 0 ∨ zLo > zHi
                           then acc
                           else foldl' (\acc2 z →
                                let i   = z - colMinZ
                                    mat = ctMats col VU.! i
                                in if mat ≡ 0  -- matAir
                                   then acc2
                                   else if isUnderLava ∧ z < maybe 0 fcSurface mFluid
                                   then acc2
                                   else
                                   let relativeZ = z - zSlice
                                       heightOffset = fromIntegral relativeZ * tileSideHeight
                                       drawX = rawX + xOffset
                                       drawY = rawY - heightOffset
                                   in if isTileVisible vb drawX drawY
                                      then let slopeId = ctSlopes col VU.! i
                                               tile = Tile mat slopeId
                                           in tileToQuad lookupSlot lookupFmSlot textures facing
                                                gx gy z tile zSlice effectiveDepth zoomAlpha xOffset
                                                mFluid chunkHasFluid : acc2
                                      else acc2
                                ) acc [zLo .. zHi]
                    ) [] tileMap

                !blankQuads =
                    [ blankTileToQuad lookupSlot lookupFmSlot textures facing
                        gx gy zSlice zSlice zoomAlpha xOffset
                    | lx ← [0 .. chunkSize - 1]
                    , ly ← [0 .. chunkSize - 1]
                    , let idx = columnIndex lx ly
                          surfZ = surfMap VU.! idx
                          terrainZ = terrainSurfMap VU.! idx
                    , terrainZ > zSlice
                    , let col = tileMap V.! idx
                          i = zSlice - ctStartZ col
                          hasTile = i ≥ 0
                                  ∧ i < VU.length (ctMats col)
                                  ∧ ctMats col VU.! i ≠ 0
                    , not hasTile
                    , let (gx, gy) = chunkToGlobal coord lx ly
                          (rawX, rawY) = gridToScreen facing gx gy
                          drawX = rawX + xOffset
                          drawY = rawY
                    , isTileVisible vb drawX drawY
                    ]

                mkFreshwaterQuad gx gy ft fc =
                        freshwaterTileToQuad lookupSlot lookupFmSlot textures facing
                            gx gy (fcSurface fc) ft zSlice effectiveDepth
                            zoomAlpha xOffset

                (!oceanQuads, !lavaQuads, !freshwaterQuads) =
                    V.ifoldl' (\(!oAcc, !lAcc, !fAcc) idx mFluid ->
                        case mFluid of
                            Nothing -> (oAcc, lAcc, fAcc)
                            Just fc ->
                                if fcSurface fc > zSlice ∨ fcSurface fc < (zSlice - effectiveDepth)
                                then (oAcc, lAcc, fAcc)
                                else
                                    let lx = idx `mod` chunkSize
                                        ly = idx `div` chunkSize
                                        (gx, gy) = chunkToGlobal coord lx ly
                                        (rawX, rawY) = gridToScreen facing gx gy
                                        relativeZ = fcSurface fc - zSlice
                                        heightOffset = fromIntegral relativeZ * tileSideHeight
                                        drawX = rawX + xOffset
                                        drawY = rawY - heightOffset
                                    in if not (isTileVisible vb drawX drawY)
                                       then (oAcc, lAcc, fAcc)
                                       else case fcType fc of
                                            Ocean ->
                                                ( oceanTileToQuad lookupSlot lookupFmSlot textures facing
                                                    gx gy (fcSurface fc) zSlice effectiveDepth zoomAlpha xOffset
                                                  : oAcc
                                                , lAcc
                                                , fAcc
                                                )
                                            Lava  ->
                                                ( oAcc
                                                , lavaTileToQuad lookupSlot lookupFmSlot textures facing
                                                    gx gy (fcSurface fc) zSlice effectiveDepth zoomAlpha xOffset
                                                  : lAcc
                                                , fAcc
                                                )
                                            Lake  ->
                                                ( oAcc
                                                , lAcc
                                                , freshwaterTileToQuad lookupSlot lookupFmSlot textures facing
                                                    gx gy (fcSurface fc) Lake zSlice effectiveDepth
                                                    zoomAlpha xOffset
                                                  : fAcc
                                                )
                                            River ->
                                                ( oAcc
                                                , lAcc
                                                , freshwaterTileToQuad lookupSlot lookupFmSlot textures facing
                                                    gx gy (fcSurface fc) River zSlice effectiveDepth
                                                    zoomAlpha xOffset
                                                  : fAcc
                                                )
                    ) ([], [], []) fluidMap

            in V.fromList (realQuads <> blankQuads <> oceanQuads <> lavaQuads <> freshwaterQuads)
            ) visibleChunksWithOffset

    return $! V.concat chunkVectors
