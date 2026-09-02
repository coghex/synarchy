{-# LANGUAGE Strict #-}
-- | Chop's exact-identity commit authority (#1856), driven against a
--   real headless engine.
--
--   'Test.Headless.World.Chop.Selection' pins which trees a gesture
--   CHOOSES; this pins what the world thread does with that choice.
--   The two halves are deliberately separate: the selection rule is
--   screen-space and pure, while the commit is a live-state re-check —
--   a tree can be felled, or start regrowing, between the gesture and
--   the drain, and the queue carries only identities, so the commit has
--   to resolve every one of them against the world as it is now.
--
--   Run just this gate: @cabal test synarchy-test-headless
--   --test-options='--match "Chop authority"'@.
module Test.Headless.World.Chop.Authority (spec) where

import UPrelude
import Test.Hspec
import qualified Data.HashMap.Strict as HM
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU
import qualified Data.Map as Map
import Data.IORef (readIORef, writeIORef, modifyIORef')
import Data.List (sort)
import Engine.Asset.Handle (TextureHandle(..))
import Engine.Core.Init (initializeEngineHeadless, EngineInitResult(..))
import Engine.Core.Capability.RenderView
    (RenderViewCapability(..), toRenderViewCapability)
import Engine.Core.State (EngineEnv(..), loggerRef, worldManagerRef)
import Engine.Graphics.Camera (Camera2D(..), CameraFacing(..))
import World.Flora.HitTest (FloraHitView(..), floraHitView)
import World.Generate (viewDepth)
import World.Render.Camera (cameraChanged)
import World.Render.Camera.Types
    (WorldCameraSnapshot(..), WorldQuadCache(..))
import Structure.Types (emptyChunkStructures)
import World.Chop.Types (chopDesignationTile)
import World.Chunk.Types
    (ChunkCoord(..), ColumnTiles(..), LoadedChunk(..), chunkSize)
import World.Flora.Identity
    (FloraInstanceId, generatedFloraInstanceId, plantedFloraInstanceId)
import World.Flora.Types
import World.Fluid.Types (emptyIceMap)
import World.Generate.Types (defaultWorldGenParams, WorldGenParams(..))
import World.Page.Types (WorldPageId(..))
import World.State.Types (WorldState(..), emptyWorldState)
import World.Thread.Command.Cursor.Chop
    (handleWorldDesignateChopInstancesCommand
    , handleWorldEraseChopInstancesCommand)
import World.Tile.Types (WorldTileData(..))
import World.Types (WorldManager(..), emptyWorldManager)

fixturePage ∷ WorldPageId
fixturePage = WorldPageId "chop_authority_probe"

zSlice ∷ Int
zSlice = 12

fixtureChunk ∷ ChunkCoord
fixtureChunk = ChunkCoord 0 0

woodId, berryId ∷ FloraId
woodId  = FloraId 1
berryId = FloraId 2

-- | @oak@ is choppable; @thicket@ is harvestable but carries no @wood@
--   tag, so the commit's own eligibility re-check must drop it however
--   it got into the request.
catalog ∷ FloraCatalog
catalog =
    insertSpecies berryId
        (newFloraSpecies "thicket" (TextureHandle 3))
            { fsHarvest = Just (harvest ["fruit"]) }
    $ insertSpecies woodId
        (newFloraSpecies "oak" (TextureHandle 2))
            { fsHarvest = Just (harvest ["wood"]) }
      emptyFloraCatalog
  where
    harvest tags = FloraHarvest
        { fhTags = tags, fhYield = [], fhRegrowth = 86400
        , fhHarvestedTexture = TextureHandle 0 }

plantId ∷ Int → FloraInstanceId
plantId = generatedFloraInstanceId "chop_authority" 0 0 "oak"

-- | Two oaks and one berry bush, each on its own tile.
oakA, oakB, berry ∷ FloraInstance
oakA  = plant 1 woodId (4, 4)
oakB  = plant 2 woodId (5, 4)
berry = plant 3 berryId (6, 4)

plant ∷ Int → FloraId → (Int, Int) → FloraInstance
plant ordinal species (lx, ly) = FloraInstance
    { fiSpecies = species
    , fiTileX = fromIntegral lx, fiTileY = fromIntegral ly
    , fiOffU = 0, fiOffV = 0, fiZ = zSlice
    , fiAge = 100, fiHealth = 1, fiVariant = 0, fiBaseWidth = 16
    , fiInstanceId = plantId ordinal
    , fiChopDesignated = False
    }

fixtureTiles ∷ WorldTileData
fixtureTiles =
    let area = chunkSize * chunkSize
        col = ColumnTiles
            { ctStartZ = 0
            , ctMats   = VU.replicate (zSlice + 1) 1
            , ctSlopes = VU.replicate (zSlice + 1) 0
            , ctVeg    = VU.replicate (zSlice + 1) 0 }
        lc = LoadedChunk
            { lcCoord = fixtureChunk
            , lcTiles = V.replicate area col
            , lcSurfaceMap = VU.replicate area zSlice
            , lcTerrainSurfaceMap = VU.replicate area zSlice
            , lcFluidMap = V.replicate area Nothing
            , lcIceMap = emptyIceMap
            , lcFlora = FloraChunkData [oakA, oakB, berry]
            , lcSideDeco = VU.empty, lcWaterTableMap = VU.empty
            , lcMagma = Nothing, lcStructures = emptyChunkStructures }
    in WorldTileData { wtdChunks = HM.fromList [(fixtureChunk, lc)]
                     , wtdMaxChunks = 200 }

-- | A well-formed PLANTED-namespace id that names no resident plant.
strayId ∷ FloraInstanceId
strayId = plantedFloraInstanceId 999999

resetPage ∷ EngineEnv → IO WorldState
resetPage env = do
    ws ← emptyWorldState
    writeIORef (wsGenParamsRef ws)
        (Just defaultWorldGenParams { wgpWorldSize = 0 })
    writeIORef (wsTilesRef ws) fixtureTiles
    writeIORef (floraCatalogRef env) catalog
    writeIORef (worldManagerRef env) emptyWorldManager
        { wmWorlds = [(fixturePage, ws)], wmVisible = [fixturePage] }
    pure ws

designated ∷ WorldState → IO [FloraInstanceId]
designated ws = sort . HM.keys <$> readIORef (wsChopDesignationsRef ws)

-- | Every loaded instance's 'fiChopDesignated' mirror, by id.
mirrors ∷ WorldState → IO [(FloraInstanceId, Bool)]
mirrors ws = do
    td ← readIORef (wsTilesRef ws)
    pure $ sort
        [ (fiInstanceId fi, fiChopDesignated fi)
        | lc ← HM.elems (wtdChunks td)
        , fi ← fcdInstances (lcFlora lc) ]

spec ∷ Spec
spec = describe "Chop authority" $ beforeAll setup $ do

    it "designates exactly the named plants" $ \env → do
        ws ← resetPage env
        logger ← readIORef (loggerRef env)
        handleWorldDesignateChopInstancesCommand env logger fixturePage
            [plantId 1] "wood"
        designated ws `shouldReturn` [plantId 1]

    it "records each plant's own live tile and surface z" $ \env → do
        ws ← resetPage env
        logger ← readIORef (loggerRef env)
        handleWorldDesignateChopInstancesCommand env logger fixturePage
            [plantId 1, plantId 2] "wood"
        m ← readIORef (wsChopDesignationsRef ws)
        sort (map chopDesignationTile (HM.elems m)) `shouldBe` [(4, 4), (5, 4)]

    it "moves the loaded mirror with the durable map" $ \env → do
        -- #1854 requirement 8: the two authorities are written together
        -- or not at all, so no consumer can observe them disagreeing.
        ws ← resetPage env
        logger ← readIORef (loggerRef env)
        handleWorldDesignateChopInstancesCommand env logger fixturePage
            [plantId 2] "wood"
        mirrors ws `shouldReturn`
            sort [ (plantId 1, False), (plantId 2, True), (plantId 3, False) ]

    it "re-checks eligibility and drops a non-wood plant" $ \env → do
        ws ← resetPage env
        logger ← readIORef (loggerRef env)
        handleWorldDesignateChopInstancesCommand env logger fixturePage
            [plantId 1, plantId 3] "wood"
        designated ws `shouldReturn` [plantId 1]

    it "drops a plant that has started regrowing since the gesture" $ \env → do
        ws ← resetPage env
        logger ← readIORef (loggerRef env)
        writeIORef (wsFloraHarvestsRef ws) (HM.fromList [(plantId 1, 500)])
        handleWorldDesignateChopInstancesCommand env logger fixturePage
            [plantId 1, plantId 2] "wood"
        designated ws `shouldReturn` [plantId 2]

    it "drops an id naming no resident plant" $ \env → do
        ws ← resetPage env
        logger ← readIORef (loggerRef env)
        handleWorldDesignateChopInstancesCommand env logger fixturePage
            [plantId 1, strayId] "wood"
        designated ws `shouldReturn` [plantId 1]

    it "honours the requested tag rather than assuming wood" $ \env → do
        ws ← resetPage env
        logger ← readIORef (loggerRef env)
        handleWorldDesignateChopInstancesCommand env logger fixturePage
            [plantId 1, plantId 3] "fruit"
        designated ws `shouldReturn` [plantId 3]

    it "is idempotent: designating twice leaves one entry" $ \env → do
        ws ← resetPage env
        logger ← readIORef (loggerRef env)
        forM_ [1 ∷ Int, 2] $ \_ →
            handleWorldDesignateChopInstancesCommand env logger fixturePage
                [plantId 1] "wood"
        designated ws `shouldReturn` [plantId 1]

    it "does nothing at all for a page that does not exist" $ \env → do
        ws ← resetPage env
        logger ← readIORef (loggerRef env)
        handleWorldDesignateChopInstancesCommand env logger
            (WorldPageId "no_such_page") [plantId 1] "wood"
        designated ws `shouldReturn` []

    describe "the picker's placement camera" $ do

        -- 'floraHitView' is the IO assembly the pure selection spec
        -- deliberately bypasses, so the wiring that reads the quad
        -- cache is only covered here. A cached quad's wrap alias is
        -- baked into its world coordinates, so placement has to come
        -- from the camera the cache was BUILT with, not the live one.
        let liveCam = (11.0, 22.0) ∷ (Float, Float)
            cachedCam = (99.0, -44.0) ∷ (Float, Float)
            cacheAt pos = WorldQuadCache
                { wqcGen = 0
                , wqcCamera = WorldCameraSnapshot
                    { wcsPosition = pos, wcsZoom = 12
                    , wcsZSlice = zSlice, wcsFbSize = (800, 600)
                    , wcsFacing = FaceSouth }
                , wqcSolarSlot = 0
                , wqcQuads = Map.empty }

        it "takes placement from the cache the drawn quads came from" $ \env → do
            ws ← resetPage env
            let rv = toRenderViewCapability env
            modifyIORef' (rvCameraRef rv) $ \c →
                c { camPosition = liveCam }
            writeIORef (wsQuadCacheRef ws) (Just (cacheAt cachedCam))
            view ← floraHitView env ws
            (fhvPlaceCamX view, fhvPlaceCamY view) `shouldBe` cachedCam
            -- …while the VIEW transform stays live: cached world
            -- coordinates are looked at through the live camera.
            (fhvCamX view, fhvCamY view) `shouldBe` liveCam

        it "takes the z-band CULL from the cache's zoom, not the live one" $ \env → do
            -- effDepth = min viewDepth (max 8 (round (zoom*80 + 8))), so
            -- it steps every 0.0125 of zoom — while 'cameraChanged'
            -- reuses a cache across a zoom delta of camEpsilon (0.075),
            -- six steps. A cull taken live would let the picker consider
            -- a tree the cached run omitted, or skip one it drew.
            ws ← resetPage env
            let rv = toRenderViewCapability env
                cachedZoom = 1.0 ∷ Float
                -- Inside camEpsilon of it, but several depth steps away.
                liveZoom = cachedZoom + 0.05
            modifyIORef' (rvCameraRef rv) $ \c → c { camZoom = liveZoom }
            writeIORef (wsQuadCacheRef ws) $ Just (cacheAt (0, 0))
                { wqcCamera = (wqcCamera (cacheAt (0, 0)))
                    { wcsZoom = cachedZoom } }
            view ← floraHitView env ws
            let depthOf z = min viewDepth
                    (max 8 (round (z * 80.0 + 8.0 ∷ Float)))
            -- The fixture must really straddle a step, or this proves
            -- nothing…
            depthOf cachedZoom `shouldNotBe` depthOf liveZoom
            -- …and the pan must really REUSE the cache.
            cameraChanged (wqcCamera (cacheAt (0, 0))) { wcsZoom = cachedZoom }
                          ((wqcCamera (cacheAt (0, 0))) { wcsZoom = liveZoom })
                `shouldBe` False
            fhvEffDepth view `shouldBe` depthOf cachedZoom

        it "falls back to the live camera when no cache exists yet" $ \env → do
            ws ← resetPage env
            let rv = toRenderViewCapability env
            modifyIORef' (rvCameraRef rv) $ \c →
                c { camPosition = liveCam }
            writeIORef (wsQuadCacheRef ws) Nothing
            view ← floraHitView env ws
            (fhvPlaceCamX view, fhvPlaceCamY view) `shouldBe` liveCam

    describe "erase" $ do

        it "clears exactly the named plants" $ \env → do
            ws ← resetPage env
            logger ← readIORef (loggerRef env)
            handleWorldDesignateChopInstancesCommand env logger fixturePage
                [plantId 1, plantId 2] "wood"
            handleWorldEraseChopInstancesCommand env logger fixturePage
                [plantId 1]
            designated ws `shouldReturn` [plantId 2]

        it "clears the loaded mirror with it" $ \env → do
            ws ← resetPage env
            logger ← readIORef (loggerRef env)
            handleWorldDesignateChopInstancesCommand env logger fixturePage
                [plantId 1] "wood"
            handleWorldEraseChopInstancesCommand env logger fixturePage
                [plantId 1]
            mirrors ws `shouldReturn`
                sort [ (plantId 1, False), (plantId 2, False)
                     , (plantId 3, False) ]

        it "clears a designation whose tree is no longer ADD-eligible" $ \env → do
            -- D-12: erase filters by what is designated. A tree that
            -- started regrowing after being designated must still be
            -- clearable, or the player is stuck with a standing mark.
            ws ← resetPage env
            logger ← readIORef (loggerRef env)
            handleWorldDesignateChopInstancesCommand env logger fixturePage
                [plantId 1] "wood"
            writeIORef (wsFloraHarvestsRef ws) (HM.fromList [(plantId 1, 500)])
            handleWorldEraseChopInstancesCommand env logger fixturePage
                [plantId 1]
            designated ws `shouldReturn` []

        it "is idempotent on a plant that is not designated" $ \env → do
            ws ← resetPage env
            logger ← readIORef (loggerRef env)
            handleWorldDesignateChopInstancesCommand env logger fixturePage
                [plantId 1] "wood"
            handleWorldEraseChopInstancesCommand env logger fixturePage
                [plantId 2, strayId]
            designated ws `shouldReturn` [plantId 1]

        it "undoes exactly what the matching add did" $ \env → do
            ws ← resetPage env
            logger ← readIORef (loggerRef env)
            before ← designated ws
            handleWorldDesignateChopInstancesCommand env logger fixturePage
                [plantId 1, plantId 2, plantId 3] "wood"
            handleWorldEraseChopInstancesCommand env logger fixturePage
                [plantId 1, plantId 2, plantId 3]
            designated ws `shouldReturn` before
            mirrors ws `shouldReturn`
                sort [ (plantId 1, False), (plantId 2, False)
                     , (plantId 3, False) ]
  where
    setup = do
        EngineInitResult env ← initializeEngineHeadless
        pure env
