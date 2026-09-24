{-# LANGUAGE Strict #-}
module Test.Headless.World.Render.FluidLevels (spec) where

import UPrelude
import Test.Hspec
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU
import qualified Data.HashMap.Strict as HM
import Engine.Asset.Handle (TextureHandle(..), toInt)
import Engine.Graphics.Camera (CameraFacing(..))
import Engine.Graphics.Vulkan.Types.Vertex (Vertex(..), Vec2(..))
import Engine.Scene.Types (SortableQuad(..))
import World.Chunk.Types (ChunkCoord(..), LoadedChunk(..), chunkSize, columnIndex)
import World.Fluid.Types (FluidCell(..), FluidType(..), IceCell(..), IceMode(..), IceLevelGrid(..))
import World.Grid (gridToScreen, tileHeight, tileSideHeight, tileHalfDiamondHeight)
import World.Render.ChunkLookup (canonicalChunkLookup)
import World.Render.FluidTopQuads (fluidTopQuads)
import World.Render.QuadContext (QuadContext(..), ZSlice(..), EffectiveDepth(..))
import World.Render.SideDecoQuads (waterSideFaceQuads, fluidSideIntervals)
import World.Render.Textures.Types (WorldTextures(..), defaultWorldTextures)
import World.Render.ViewBounds (ViewBounds(..))

import qualified HsLua as Lua
import qualified Data.Text.Encoding as TE
import Data.IORef (readIORef, writeIORef)
import Test.Headless.Harness.Isolation (withIsolatedResourceRoot)
import Test.Headless.Harness.Log (initializeEngineHeadlessQuiet)
import Engine.Core.Init (EngineInitResult(..))
import Engine.Core.State (EngineEnv(..))
import Engine.Core.Capability.WorldSim (WorldSimCapability(..), toWorldSimCapability)
import qualified Engine.Core.Queue as Q
import Engine.Scripting.Lua.API.World.Clock (worldSetTextureFn)
import World.Command.Types (WorldCommand(..))
import World.Thread.Command.Texture (handleWorldSetTextureCommand)
import World.Page.Types (WorldPageId(..))
import World.State.Types (WorldState(..), WorldManager(..), emptyWorldState, emptyWorldManager)
import World.Render.Textures (getFluidFaceMapTexture)
import World.Render.Zoom.Types (ZoomQuadCache(..), ZoomCameraSnapshot(..))
import World.Fluid.Ice (computeChunkIce)
import World.Plate (generatePlates)
import World.Weather.Types (ClimateState(..), initClimateState)

import Engine.Scripting.Lua.API.Debug (setFluidSurfaceFn)
import World.Thread.Command.Edit.Fluid (handleWorldDebugSetFluidSurfaceCommand)
import World.Generate.Arena (generateFlatChunk)
import World.Generate.Types (WorldGenParams(..), defaultWorldGenParams)
import World.Tile.Types (WorldTileData(..))
import World.Edit.Types (WorldEdit(..))
import Sim.Command.Types (SimCommand(..))

textures ∷ WorldTextures
textures = defaultWorldTextures
    { wtIsoFaceMap = TextureHandle 99
    , wtFluidLevelFaceMap1 = TextureHandle 101
    , wtFluidLevelFaceMap2 = TextureHandle 102
    , wtFluidLevelFaceMap3 = TextureHandle 103
    , wtFluidLevelFaceMap4 = TextureHandle 104
    , wtFluidLevelFaceMap5 = TextureHandle 105
    , wtFluidLevelFaceMap6 = TextureHandle 106
    , wtFluidLevelFaceMap7 = TextureHandle 107
    , wtFluidLevelFaceMap8 = TextureHandle 108
    , wtSideFaceMapLeft = TextureHandle 201
    , wtSideFaceMapRight = TextureHandle 202
    }

ctx ∷ QuadContext
ctx = QuadContext (fromIntegral ∘ toInt) (fromIntegral ∘ toInt)
          textures FaceSouth (ZSlice 4) (EffectiveDepth 8) 1 (0, 0)

visible ∷ ViewBounds
visible = ViewBounds (-1.0e9) 1.0e9 (-1.0e9) 1.0e9

fluids ∷ [((Int, Int), FluidType, Int)] → V.Vector (Maybe FluidCell)
fluids cells = V.replicate (chunkSize * chunkSize) Nothing V.//
    [(columnIndex x y, Just (FluidCell kind surface)) | ((x, y), kind, surface) ← cells]

noIce ∷ V.Vector (Maybe IceCell)
noIce = V.replicate (chunkSize * chunkSize) Nothing

tops ∷ QuadContext → FluidType → Int → [SortableQuad]
tops c kind surface =
    let (a, b, d) = fluidTopQuads c (ChunkCoord 0 0)
                       (fluids [((8, 8), kind, surface)]) noIce visible
    in a <> b <> d

near ∷ Float → Float → Expectation
near actual expected = actual `shouldSatisfy` (\x → abs (x - expected) < 1.0e-5)

oneQuad ∷ [SortableQuad] → IO SortableQuad
oneQuad [q] = pure q
oneQuad qs = expectationFailure ("expected one quad, got " <> show (length qs)) ≫ fail "quad count"

spec ∷ Spec
spec = do
    bindingSpec
    iceSpec
    debugEditSpec
    describe "exact tops through the production pass" $ do
        it "selects every level for every fluid, including negative planes and full multiples" $
            forM_ [Ocean, Lake, River, Lava] $ \kind →
                forM_ [-2 .. 2] $ \z → forM_ [1 .. 8] $ \level → do
                    let qs = tops ctx kind (z * 8 + level)
                    length qs `shouldBe` 1
                    map (faceMapId ∘ sqV0) qs `shouldBe` [fromIntegral (100 + level)]
        it "places vertices at fractional heights in every facing" $
            forM_ [FaceSouth, FaceEast, FaceNorth, FaceWest] $ \facing →
                forM_ [-9 .. 16] $ \surface → do
                    q ← oneQuad (tops ctx { qcFacing = facing } Lake surface)
                    let (_, rawY) = gridToScreen facing 8 8
                        Vec2 _ y = pos (sqV0 q)
                    near y (rawY - (fromIntegral surface / 8 - 4) * tileSideHeight)
        it "culls using fractional placement rather than the owning ceiling" $ do
            let c = ctx { qcZSlice = ZSlice 0 }
                (_, rawY) = gridToScreen FaceSouth 8 8
                cutoff = rawY + tileSideHeight / 2
                bounds = ViewBounds (-1.0e9) 1.0e9 (-1.0e9) cutoff
                run u = let (a, b, d) = fluidTopQuads c (ChunkCoord 0 0)
                                (fluids [((8, 8), Lake, u)]) noIce bounds
                        in a <> b <> d
            length (run (-1)) `shouldBe` 1
            length (run (-7)) `shouldBe` 0
        it "uses the ceiling for the actual slice/depth decision, including zero as wet" $ do
            let c = ctx { qcZSlice = ZSlice 2, qcEffectiveDepth = EffectiveDepth 2 }
            forM_ [-1, 0, 1, 16] $ \u → length (tops c Lake u) `shouldBe` 1
            forM_ [-8, -9, 17] $ \u → length (tops c Lake u) `shouldBe` 0
        it "sorts exact tops above their own sides" $ do
            let fm = fluids [((8, 8), Lake, 19)]
                side = waterSideFaceQuads ctx (ChunkCoord 0 0) fm
                           (VU.replicate 256 0) (const Nothing) (const Nothing) visible
            q ← oneQuad (tops ctx Lake 19)
            side `shouldNotSatisfy` null
            map sqSortKey side `shouldSatisfy` all (< sqSortKey q)
        it "keeps covered partial Lake/Ocean suppressed and River/Lava visible" $
            forM_ [Ocean, Lake, River, Lava] $ \kind → do
                let ice = noIce V.// [(columnIndex 8 8, Just (IceCell 4 DrapeIce))]
                    (a, b, d) = fluidTopQuads ctx (ChunkCoord 0 0)
                                  (fluids [((8, 8), kind, 3)]) ice visible
                length (a <> b <> d) `shouldBe` if kind `elem` [Ocean, Lake] then 0 else 1

    describe "mask and separate-side coverage" $ do
        it "starts below the mask slab and terminates at an exact partial neighbour" $ do
            fluidSideIntervals 27 5 8 16 `shouldBe` [(5, 8), (8, 16), (16, 24)]
            fluidSideIntervals 24 5 8 16 `shouldBe` [(5, 8), (8, 16)]
            fluidSideIntervals 3 (-19) 8 16 `shouldBe` [(-19, -16), (-16, -8), (-8, 0)]
        it "does not duplicate the mask slab for same-z mixed levels or full one-z drops" $ do
            fluidSideIntervals 7 1 8 16 `shouldBe` []
            fluidSideIntervals 8 0 8 16 `shouldBe` []
        it "clips actual side intervals at both depth boundaries" $ do
            fluidSideIntervals 27 (-19) 2 2 `shouldBe` [(0, 8), (8, 16)]
            fluidSideIntervals 27 16 2 2 `shouldBe` []
        it "covers without gaps, duplicates, or above-neighbour overshoot across the signed range" $
            forM_ [-24 .. 32] $ \u → forM_ [-32 .. u + 8] $ \n → do
                let intervals = fluidSideIntervals u n 8 16
                    actual = concatMap (\(lo, hi) → [lo .. hi - 1]) intervals
                    -- Explicit slab floor independent of the production helper.
                    maskBottom = u - (if u `mod` 8 ≡ 0 then 8 else u `mod` 8)
                actual `shouldBe` [n .. maskBottom - 1]
        it "gives left and right sides independent fractional endpoints and Ocean parity" $ do
            let scene kind = fluids [((8, 8), kind, 27), ((8, 9), kind, 5), ((9, 8), kind, 11)]
                run kind = waterSideFaceQuads ctx (ChunkCoord 0 0) (scene kind)
                    (VU.replicate 256 4) (const Nothing) (const Nothing) visible
                qs = run Lake
                left = filter ((≡ 201) ∘ faceMapId ∘ sqV0) qs
                right = filter ((≡ 202) ∘ faceMapId ∘ sqV0) qs
            length left `shouldBe` 3
            length right `shouldBe` 2
            -- Reconstruct the plane from actual emitted vertex positions.
            let lower q =
                    let Vec2 _ y0 = pos (sqV0 q)
                        Vec2 _ y1 = pos (sqV1 q)
                        Vec2 _ y3 = pos (sqV3 q)
                        u = if faceMapId (sqV0 q) ≡ 201 then 0.25 else 0.75
                        edgeY = 1.5 * tileHalfDiamondHeight
                        v = (edgeY + tileSideHeight) / tileHeight
                        y = y0 + u * (y1 - y0) + v * (y3 - y0)
                        (_, rawY) = gridToScreen FaceSouth 8 8
                    in 4 + (rawY + edgeY - y) / tileSideHeight
            mapM_ (\q → near (lower q) (5 / 8)) (take 1 left)
            mapM_ (\q → near (lower q) (11 / 8)) (take 1 right)
            map tshow (run Ocean) `shouldBe` map tshow qs
        it "emits nothing for equal/higher neighbours and an enclosed equal-height cell" $ do
            let fm = fluids [((x, y), Lake, 19) | x ← [7..9], y ← [7..9]]
            waterSideFaceQuads ctx (ChunkCoord 0 0) fm (VU.replicate 256 4)
                (const Nothing) (const Nothing) visible `shouldSatisfy` null

    describe "production neighbour lookup" $ do
        it "matches loaded ordinary and U seams in all four facings and omits unloaded edges" $
            forM_ [FaceSouth, FaceEast, FaceNorth, FaceWest] $ \facing → do
                let c = ctx { qcFacing = facing }
                    fm = fluids [((15, 8), Lake, 27)]
                    terrain = VU.replicate 256 4
                    neighbour = fluids [((0, 8), Lake, 5)]
                    run home stored = waterSideFaceQuads c home fm terrain
                        (canonicalChunkLookup 64 (HM.singleton stored neighbour))
                        (canonicalChunkLookup 64 (HM.singleton stored terrain)) visible
                    ordinary = run (ChunkCoord 0 0) (ChunkCoord 1 0)
                    wrapped = run (ChunkCoord 16 (-15)) (ChunkCoord (-15) 17)
                    heights = map (\q →
                        let Vec2 _ a = pos (sqV0 q)
                            Vec2 _ b = pos (sqV3 q)
                        in b - a)
                length ordinary `shouldBe` if facing `elem` [FaceSouth, FaceEast] then 3 else 0
                length wrapped `shouldBe` length ordinary
                sequence_ (zipWith near (heights wrapped) (heights ordinary))
                waterSideFaceQuads c (ChunkCoord 16 (-15)) fm terrain
                    (const Nothing) (const Nothing) visible `shouldSatisfy` null

-- Execute the actual Lua binders, Lua API/parser, queue and texture command.
-- Only texture loading and world creation are stubbed: no GPU or worldgen.
bindingSpec ∷ Spec
bindingSpec = describe "initial and late structural binding" $
    it "binds all eight distinct handles and invalidates every render cache" $
        withIsolatedResourceRoot $ do
            EngineInitResult env ← initializeEngineHeadlessQuiet
            ws ← emptyWorldState
            let page = WorldPageId "main_world"
                wsc = toWorldSimCapability env
                cached = Just (ZoomQuadCache (ZoomCameraSnapshot (0, 0) 1 (800, 600)) 1 V.empty)
                drain base = do
                    commands ← Q.flushQueue (wsWorldQueue wsc)
                    commands `shouldNotSatisfy` null
                    logger ← readIORef (loggerRef env)
                    forM_ commands $ \case
                        WorldSetTexture pid kind handle → do
                            gen ← readIORef (wsQuadCacheGenRef ws)
                            writeIORef (wsZoomQuadCacheRef ws) cached
                            writeIORef (wsBgQuadCacheRef ws) cached
                            handleWorldSetTextureCommand wsc logger pid kind handle
                            readIORef (wsQuadCacheGenRef ws) ≫= (`shouldBe` (gen + 1))
                            readIORef (wsZoomQuadCacheRef ws) ≫= (`shouldSatisfy` isNothing)
                            readIORef (wsBgQuadCacheRef ws) ≫= (`shouldSatisfy` isNothing)
                        _ → expectationFailure "unexpected non-texture command"
                    wt ← readIORef (wsTexturesRef ws)
                    forM_ [1 .. 8] $ \level →
                        getFluidFaceMapTexture wt level `shouldBe` TextureHandle (base + level)
            writeIORef (wsWorldManagerRef wsc) emptyWorldManager { wmWorlds = [(page, ws)] }
            Lua.run @Lua.Exception $ do
                Lua.openlibs
                runLua $ mconcat
                    [ "engine={logInfo=function()end,logWarn=function()end,"
                    , "getTextureHandle=function()return -1 end,loadTexture=function(path) "
                    , "local n=path:match('isoface_level_(%d).png'); return n and 100+tonumber(n) or 1 end};"
                    , "world={init=function()return true end,show=function()end};"
                    , "package.loaded['scripts.tutorial_progress']={reset=function()end};"
                    ]
                _ ← Lua.getglobal "world"
                Lua.pushHaskellFunction (worldSetTextureFn wsc)
                Lua.setfield (-2) "setTexture"
                Lua.pop 1
                runLua "wv=require('scripts.world_view'); wv.ensureStructuralTextures(); require('scripts.world_manager').createWorld({structural=wv.structuralTextures})"
                Lua.liftIO (drain 100)
                runLua "for n=1,8 do wv.structuralTextures['fluidLevelFaceMap'..n]=200+n end; wv.rebindStructural('main_world')"
                Lua.liftIO (drain 200)
  where
    runLua code = do
        result ← Lua.dostring (TE.encodeUtf8 code)
        when (result ≢ Lua.OK) $ do
            err ← Lua.tostring (-1)
            Lua.liftIO $ expectationFailure (show err)

iceSpec ∷ Spec
iceSpec = describe "unchanged ice base and independent elevation" $ do
    let climate = (initClimateState 64) { csGlobalTemp = -100 }
        compute grid terrain fluid = computeChunkIce 42 (generatePlates 42 64 4) climate 64 (ChunkCoord 0 0)
            grid (VU.replicate 256 terrain) (V.replicate 256 fluid) V.! 0
        drape = IceLevelGrid 0 16 VU.empty
        basin z = IceLevelGrid 1 16 (VU.singleton z)
    it "keeps dry drape one z above terrain and dry basin fill/cap" $ do
        compute drape 0 Nothing `shouldBe` Just (IceCell 1 DrapeIce)
        compute (basin 7) 0 Nothing `shouldBe` Just (IceCell 7 BasinIce)
        compute (basin 100) 0 Nothing `shouldBe` Just (IceCell 20 BasinIce)
    it "uses the fluid ceiling only for the base, including partial and full planes" $
        forM_ [1, 3, 8] $ \u → do
            compute drape 0 (Just (FluidCell Lake u)) `shouldBe` Just (IceCell 2 DrapeIce)
            compute (basin 100) 0 (Just (FluidCell Ocean u)) `shouldBe` Just (IceCell 21 BasinIce)
            compute drape 4 (Just (FluidCell Lake u)) `shouldBe` Just (IceCell 5 DrapeIce)

debugEditSpec ∷ Spec
debugEditSpec = describe "exact capture edit hook" $
    it "validates arguments, canonicalizes aliases, records exact edits and fences stale sim output" $
        withIsolatedResourceRoot $ do
            EngineInitResult env ← initializeEngineHeadlessQuiet
            ws ← emptyWorldState
            let page = WorldPageId "fixture"
                wsc = toWorldSimCapability env
                coord = ChunkCoord 0 0
                cell = Just (FluidCell Lake (-3))
                lc = (generateFlatChunk coord) { lcTerrainSurfaceMap = VU.replicate 256 (-4) }
            writeIORef (wsWorldManagerRef wsc) emptyWorldManager { wmWorlds = [(page, ws)] }
            writeIORef (wsGenParamsRef ws) (Just defaultWorldGenParams { wgpWorldSize = 64 })
            writeIORef (wsTilesRef ws) (WorldTileData (HM.singleton coord lc) 1)
            Lua.run @Lua.Exception $ do
                Lua.openlibs
                Lua.pushHaskellFunction (setFluidSurfaceFn wsc)
                Lua.setglobal "edit"
                result ← Lua.dostring $ mconcat
                    [ "assert(not edit('fixture',0,0,'unknown',3));"
                    , "assert(not edit('fixture',0,0,'lake',1.5));"
                    , "assert(not edit('fixture',0,0,'lake'));"
                    , "assert(edit('fixture',512,-512,'lake',-3));"
                    ]
                when (result ≢ Lua.OK) $ do
                    err ← Lua.tostring (-1)
                    Lua.liftIO $ expectationFailure (show err)
            commands ← Q.flushQueue (wsWorldQueue wsc)
            length commands `shouldBe` 1
            logger ← readIORef (loggerRef env)
            forM_ commands $ \case
                WorldDebugSetFluidSurface pid x y kind units →
                    handleWorldDebugSetFluidSurfaceCommand env logger pid x y kind units
                _ → expectationFailure "wrong command"
            td ← readIORef (wsTilesRef ws)
            (HM.lookup coord (wtdChunks td) ≫= (\chunk → lcFluidMap chunk V.! 0)) `shouldBe` cell
            readIORef (wsEditsRef ws) ≫= (\edits →
                HM.lookup coord edits `shouldBe` Just [WeSetFluidSnapshot 0 0 Lake (-3)])
            readIORef (wsChunkEditGenRef ws) ≫= (\gens → HM.lookup coord gens `shouldBe` Just 1)
            readIORef (wsQuadCacheGenRef ws) ≫= (`shouldBe` 1)
            messages ← Q.flushQueue (wsSimQueue wsc)
            length messages `shouldBe` 1
            forM_ messages $ \case
                SimChunkEdited pid _ _ cc generation fm _ → do
                    pid `shouldBe` page
                    cc `shouldBe` coord
                    generation `shouldBe` 1
                    fm V.! 0 `shouldBe` cell
                _ → expectationFailure "no authoritative sim reseed"
