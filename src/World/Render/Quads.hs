{-# LANGUAGE Strict, UnicodeSyntax #-}
module World.Render.Quads
    ( renderWorldQuads
    , renderWorldCursorQuads
    ) where

import UPrelude
import qualified Data.HashMap.Strict as HM
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector as V
import Data.Maybe (isJust)
import Data.IORef (readIORef, writeIORef)
import Engine.Core.State (EngineEnv(..))
import Engine.Scene.Types (SortableQuad(..))
import Engine.Graphics.Camera (CameraFacing(..), Camera2D(..))
import Engine.Graphics.Vulkan.Texture.Types (BindlessTextureSystem(..))
import Engine.Graphics.Vulkan.Texture.Bindless (getTextureSlotIndex)
import World.Types
import World.Constants (seaLevel)
import World.Fluids (FluidCell(..), FluidType(..))
import World.Generate (chunkToGlobal, viewDepth)
import World.Generate.Coordinates (globalToChunk)
import World.Grid (gridToScreen, tileSideHeight, tileWidth, tileHeight, worldToGrid)
import World.Render.ViewBounds (computeViewBounds, isTileVisible)
import World.Render.ChunkCulling (isChunkRelevantForSlice, isChunkVisibleWrapped)
import World.Render.TileQuads
    ( tileToQuad, blankTileToQuad, oceanTileToQuad, lavaTileToQuad
    , freshwaterTileToQuad, worldCursorToQuad, worldCursorBgToQuad
    , vegToQuad
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

                            zLo = max (ctStartZ col) (zSlice - effectiveDepth)
                            zHi = min (ctStartZ col + VU.length (ctMats col) - 1) zSlice

                            surfZ = terrainSurfMap VU.! idx
                        in if isUnderLava ∨ zHi < zLo
                           then acc
                           else foldl' (\acc2 z →
                                let mat = ctMats col VU.! (z - ctStartZ col)
                                    drawY' = rawY - fromIntegral (z - zSlice) * tileSideHeight
                                in if mat ≡ 0 ∨ not (isTileVisible vb (rawX + xOffset) drawY')
                                   then acc2
                                   else let slopeId = ctSlopes col VU.! (z - ctStartZ col)
                                            tile = Tile mat slopeId
                                            tq = tileToQuad lookupSlot lookupFmSlot textures facing
                                                    gx gy z tile zSlice effectiveDepth zoomAlpha xOffset
                                                    mFluid chunkHasFluid

                                            -- Vegetation: only on surface tile, only when
                                            -- no fluid covers it
                                            vegQ = if z ≡ surfZ ∧ not (isJust mFluid)
                                                   then let i = z - ctStartZ col
                                                            vegId = ctVeg col VU.! i
                                                            slopeId = ctSlopes col VU.! i
                                                        in vegToQuad lookupSlot lookupFmSlot textures facing
                                                               gx gy z vegId slopeId zSlice effectiveDepth
                                                               zoomAlpha xOffset
                                                   else Nothing

                                        in case vegQ of
                                            Just vq → vq : tq : acc2
                                            Nothing → tq : acc2
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

-----------------------------------------------------------
-- World Cursor Quads (generated every frame, not cached)
-----------------------------------------------------------

renderWorldCursorQuads ∷ EngineEnv → WorldState → Float → IO (V.Vector SortableQuad)
renderWorldCursorQuads env worldState tileAlpha = do
    camera   ← readIORef (cameraRef env)
    tileData ← readIORef (wsTilesRef worldState)
    textures ← readIORef (wsTexturesRef worldState)
    paramsM  ← readIORef (wsGenParamsRef worldState)
    cs       ← readIORef (wsCursorRef worldState)
    toolMode ← readIORef (wsToolModeRef worldState)

    (winW, winH) ← readIORef (windowSizeRef env)
    (fbW, fbH)   ← readIORef (framebufferSizeRef env)

    mBindless    ← readIORef (textureSystemRef env)
    defFmSlotWord ← readIORef (defaultFaceMapSlotRef env)

    if toolMode ≠ InfoTool
    then return V.empty
    else do
        let lookupSlot texHandle = fromIntegral $ case mBindless of
                Just bindless → getTextureSlotIndex texHandle bindless
                Nothing       → 0
            defFmSlot = fromIntegral defFmSlotWord
            lookupFmSlot texHandle =
                let s = lookupSlot texHandle
                in if s ≡ 0 then defFmSlot else fromIntegral s
            facing    = camFacing camera
            zoom      = camZoom camera
            zSlice    = camZSlice camera
            (camX, camY) = camPosition camera
            worldSize = case paramsM of
                          Nothing     → 128
                          Just params → wgpWorldSize params
            effectiveDepth = min viewDepth (max 8 (round (zoom * 80.0 + 8.0 ∷ Float)))
            vb = computeViewBounds camera fbW fbH effectiveDepth

        -- Hit-test: unproject screen pixel considering elevation.
        -- For each Z from zSlice down, compute which (gx,gy) would place
        -- a tile at that Z under the mouse, then check if that tile is solid.
        let hitTest pixX pixY =
                let aspect = fromIntegral fbW / fromIntegral fbH
                    vw     = zoom * aspect
                    vh     = zoom
                    normX  = fromIntegral pixX / fromIntegral winW
                    normY  = fromIntegral pixY / fromIntegral winH
                    viewX  = (normX * 2.0 - 1.0) * vw
                    viewY  = (normY * 2.0 - 1.0) * vh
                    worldX = viewX + camX
                    worldY = viewY + camY

                    -- For a tile at elevation z, its screen Y is offset by
                    -- (z - zSlice) * tileSideHeight upward.  So the world-space
                    -- Y that would produce that screen position is:
                    --   worldY = tileWorldY - (z - zSlice) * tileSideHeight
                    --   tileWorldY = worldY + (z - zSlice) * tileSideHeight
                    zMin = zSlice - effectiveDepth

                    tryZ z
                      | z < zMin  = Nothing
                      | otherwise =
                        let relZ = z - zSlice
                            adjustedWorldY = worldY + fromIntegral relZ * tileSideHeight
                                           - tileHeight * 0.5
                            (gx, gy) = worldToGrid facing worldX adjustedWorldY
                            (chunkCoord, (lx, ly)) = globalToChunk gx gy
                        in case HM.lookup chunkCoord (wtdChunks tileData) of
                            Nothing → tryZ (z - 1)
                            Just lc →
                                let idx = columnIndex lx ly
                                    col = lcTiles lc V.! idx
                                    colLen  = VU.length (ctMats col)
                                    colMinZ = ctStartZ col
                                    i = z - colMinZ
                                in if i < 0 ∨ i >= colLen
                                   then tryZ (z - 1)
                                   else if ctMats col VU.! i ≠ 0
                                        then case isChunkVisibleWrapped facing worldSize vb camX chunkCoord of
                                               Just xOff → Just (gx, gy, z, xOff)
                                               Nothing   → tryZ (z - 1)
                                        else tryZ (z - 1)

                in tryZ zSlice

        -- Compute hover tile
        let hoverResult = case worldCursorPos cs of
                Nothing           → Nothing
                Just (pixX, pixY) → hitTest pixX pixY

        -- If select flag is set, snapshot hover → selected
        -- If select flag is set, snapshot hover → selected (including Z)
        cs' ← if worldSelectNow cs
               then do
                   let newCs = cs { worldSelectNow = False
                                  , worldSelectedTile = case hoverResult of
                                        Just (gx, gy, z, _) → Just (gx, gy, z)
                                        Nothing             → worldSelectedTile cs
                                  }
                   writeIORef (wsCursorRef worldState) newCs
                   return newCs
               else return cs

        -- Build hover quads (bg + fg)
        let hoverQuads = case hoverResult of
                Just (gx, gy, hz, xOff) →
                    let fgQuad = case worldHoverTexture cs' of
                            Just tex → V.singleton $
                                worldCursorToQuad lookupSlot lookupFmSlot
                                  textures facing gx gy hz zSlice effectiveDepth
                                  tileAlpha xOff tex
                            Nothing → V.empty
                        bgQuad = case worldHoverBgTexture cs' of
                            Just tex → V.singleton $
                                worldCursorBgToQuad lookupSlot lookupFmSlot textures facing
                                    gx gy hz zSlice effectiveDepth tileAlpha xOff tex
                            Nothing → V.empty
                    in bgQuad <> fgQuad
                _ → V.empty

        -- Build select quads (bg + fg)
        let selectQuads = case (worldSelectedTile cs', worldCursorTexture cs', worldCursorBgTexture cs') of
                (Just (sgx, sgy, sz), _, _) →
                    let (chunkCoord, _) = globalToChunk sgx sgy
                    in case isChunkVisibleWrapped facing worldSize vb camX chunkCoord of
                        Just xOff →
                            let fgQuad = case worldCursorTexture cs' of
                                    Just tex → V.singleton $
                                        worldCursorToQuad lookupSlot lookupFmSlot
                                                          textures facing sgx sgy sz
                                                          zSlice effectiveDepth
                                                          tileAlpha xOff tex
                                    Nothing → V.empty
                                bgQuad = case worldCursorBgTexture cs' of
                                    Just tex → V.singleton $
                                        worldCursorBgToQuad lookupSlot lookupFmSlot textures facing
                                            sgx sgy sz zSlice effectiveDepth tileAlpha xOff tex
                                    Nothing → V.empty
                            in bgQuad <> fgQuad
                        Nothing → V.empty
                _ → V.empty

        return $ hoverQuads <> selectQuads
