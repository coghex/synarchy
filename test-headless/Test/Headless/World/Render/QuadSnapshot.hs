{-# LANGUAGE Strict #-}
{-# LANGUAGE OverloadedStrings #-}
-- | "The cached quad pass is built from the snapshot it is stamped
--   with" (#1720).
--
--   'World.Render.updateWorldTiles' captures one 'WorldCameraSnapshot'
--   before iterating the visible pages and stamps every cache entry with
--   it. 'World.Render.Quads.renderWorldQuads' used to perform a SECOND
--   @readIORef rvCameraRef@ and take facing, z-slice, zoom, effective
--   depth, position and the pre-margin view bounds from that later read
--   — while only the framebuffer size and the pan margins came from the
--   snapshot. The camera has concurrent writers (the main thread's pan
--   integration rewrites it at frame rate; 'updateWorldTiles' itself
--   writes @camZSlice@ during z-tracking) and the world thread runs this
--   pass, so the two reads can genuinely disagree.
--
--   A position disagreement does not repair itself:
--   'World.Render.Camera.cameraChanged' licenses reuse anywhere within
--   the STAMP's margin, so geometry centred somewhere else leaves an
--   uncovered strip that triggers no rebuild.
--
--   The gate here is behavioural rather than structural: the live camera
--   is mutated AFTER the snapshot is captured and the builder must
--   produce byte-identical quads anyway. Each example pins that against
--   a control proving the mutation is one the old implementation would
--   have reacted to — a snapshot carrying the mutated value produces a
--   DIFFERENT result, so an implementation reading the live camera could
--   not have passed.
--
--   No worldgen: the page is an in-memory 'emptyWorldState' carrying one
--   synthetic chunk with three solid tiles, chosen so a zoom or position
--   change genuinely re-culls them.
--
--   Run just this gate: @cabal test synarchy-test-headless
--   --test-options='--match "cached quad pass"'@.
module Test.Headless.World.Render.QuadSnapshot (spec) where

import UPrelude
import Test.Hspec
import Test.Headless.Harness.Isolation (withIsolatedResourceRoot)
import Data.IORef (readIORef, writeIORef)
import qualified Data.HashMap.Strict as HM
import qualified Data.Map as Map
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU

import Engine.Core.Init (EngineInitResult(..))
import Test.Headless.Harness.Log (initializeEngineHeadlessQuiet)
import Engine.Core.Capability.RenderView
    (RenderViewCapability(..), toRenderViewCapability)
import Engine.Core.State (EngineEnv(..))
import Engine.Graphics.Camera (Camera2D(..), CameraFacing(..), defaultCamera)
import Engine.Scene.Types (SortableQuad(..), setQuadSolarPage)
import Engine.Graphics.Solar (solarPageNone)
import Structure.Types (emptyChunkStructures)
import World.Chunk.Types
    (ChunkCoord(..), ColumnTiles(..), LoadedChunk(..), chunkSize)
import World.Flora.Types (emptyFloraChunkData)
import World.Fluid.Types (emptyIceMap)
import World.Generate.Types (WorldGenParams(..), defaultWorldGenParams)
import World.Page.Types (WorldPageId(..))
import World.Render (updateWorldTiles)
import World.Render.Quads (renderWorldQuads)
import World.Render.Camera.Types
    (WorldCameraSnapshot(..), WorldQuadCache(..))
import World.State.Types
    (WorldManager(..), WorldState(..), emptyWorldManager, emptyWorldState)
import World.Tile.Types (WorldTileData(..))

-- * Fixture geometry

-- | Two pages carrying identical content. The second exists only for
--   requirement 4: two pages built in one pass must be stamped alike
--   AND describe the same view.
pageOne, pageTwo ∷ WorldPageId
pageOne = WorldPageId "quad_snap_one"
pageTwo = WorldPageId "quad_snap_two"

-- | Large enough that the u-wrap alias search never displaces the
--   fixture chunk (its periods dwarf every coordinate below), so
--   nothing here depends on seam behaviour.
worldSizeChunks ∷ Int
worldSizeChunks = 128

testFb ∷ (Int, Int)
testFb = (1920, 1080)

-- | The solid tiles, as chunk-local coords. They sit at three
--   distinct screen offsets so a zoom or position change re-culls
--   some but not others — which is what keeps the controls below
--   non-vacuous instead of comparing empty against empty.
solidTiles ∷ [(Int, Int)]
solidTiles = [(0, 0), (15, 0), (15, 15)]

-- | Solid columns carry two stacked cells (z 0 and 1); everything else
--   is a single air cell, which the quad loop skips on @mat ≡ 0@.
--   The terrain surface map stays at 0 so the blank-tile fill (which
--   triggers on @terrainZ > zSlice@) never fires and the emitted set is
--   exactly the solid tiles.
fixtureChunk ∷ LoadedChunk
fixtureChunk =
    let area = chunkSize * chunkSize
        solid = ColumnTiles
            { ctStartZ = 0
            , ctMats   = VU.fromList [1, 1]
            , ctSlopes = VU.fromList [0, 0]
            , ctVeg    = VU.fromList [0, 0]
            }
        air = ColumnTiles
            { ctStartZ = 0
            , ctMats   = VU.singleton 0
            , ctSlopes = VU.singleton 0
            , ctVeg    = VU.singleton 0
            }
        columnAt idx =
            -- The same decomposition the quad loop uses for its own
            -- (lx, ly), so "solid" here means solid there.
            let lx = idx `mod` chunkSize
                ly = idx `div` chunkSize
            in if (lx, ly) `elem` solidTiles then solid else air
    in LoadedChunk
        { lcCoord             = ChunkCoord 0 0
        , lcTiles             = V.generate area columnAt
        -- Non-minBound somewhere is all 'isChunkRelevantForSlice' asks.
        , lcSurfaceMap        = VU.replicate area 0
        , lcTerrainSurfaceMap = VU.replicate area 0
        , lcFluidMap          = V.replicate area Nothing
        , lcIceMap            = emptyIceMap
        , lcFlora             = emptyFloraChunkData
        , lcSideDeco          = VU.replicate area 0
        , lcWaterTableMap     = VU.replicate area 0
        , lcMagma             = Nothing
        , lcStructures        = emptyChunkStructures
        }

fixtureTiles ∷ WorldTileData
fixtureTiles = WorldTileData
    { wtdChunks    = HM.singleton (lcCoord fixtureChunk) fixtureChunk
    , wtdMaxChunks = 1
    }

genParams ∷ WorldGenParams
genParams = defaultWorldGenParams { wgpWorldSize = worldSizeChunks }

-- * Cameras and snapshots

-- | The camera the reference build is captured from.
cameraA ∷ Camera2D
cameraA = defaultCamera
    { camPosition = (0, 0)
    , camZoom     = 1.2
    , camZSlice   = 1
    , camFacing   = FaceSouth
    }

-- | The camera the live reference is mutated to after capture. It
--   differs in EVERY field the cached builder consumes, so a build that
--   consulted the live camera for any one of them would diverge.
cameraB ∷ Camera2D
cameraB = defaultCamera
    { camPosition = (6.0, -3.0)
    , camZoom     = 0.05
    , camZSlice   = 0
    , camFacing   = FaceEast
    }

snapshotOf ∷ Camera2D → WorldCameraSnapshot
snapshotOf camera = WorldCameraSnapshot
    { wcsPosition = camPosition camera
    , wcsZoom     = camZoom camera
    , wcsZSlice   = camZSlice camera
    , wcsFbSize   = testFb
    , wcsFacing   = camFacing camera
    }

snapA, snapB ∷ WorldCameraSnapshot
snapA = snapshotOf cameraA
snapB = snapshotOf cameraB

-- * Scene

-- | Both pages installed with identical content, 'pageOne' first in
--   the visible list, the live camera set to @camera@.
resetScene ∷ EngineEnv → Camera2D → IO (WorldState, WorldState)
resetScene env camera = do
    wsOne ← emptyWorldState
    wsTwo ← emptyWorldState
    forM_ [wsOne, wsTwo] $ \ws → do
        writeIORef (wsTilesRef ws) fixtureTiles
        writeIORef (wsGenParamsRef ws) (Just genParams)
        writeIORef (wsQuadCacheRef ws) Nothing
    writeIORef (worldManagerRef env) emptyWorldManager
        { wmWorlds  = [(pageOne, wsOne), (pageTwo, wsTwo)]
        , wmVisible = [pageOne, pageTwo] }
    setLiveCamera env camera
    writeIORef (rvFramebufferSizeRef (toRenderViewCapability env)) testFb
    pure (wsOne, wsTwo)

setLiveCamera ∷ EngineEnv → Camera2D → IO ()
setLiveCamera env camera =
    writeIORef (rvCameraRef (toRenderViewCapability env)) camera

-- | Build one page's cached quads for @snap@, with the live camera
--   reference currently holding @live@.
buildWith ∷ EngineEnv → WorldState → Camera2D → WorldCameraSnapshot
          → IO [Text]
buildWith env ws live snap = do
    setLiveCamera env live
    quadPrints <$> renderWorldQuads env ws 1.0 snap

-- | A total, exact fingerprint of a quad run: 'SortableQuad' has no
--   'Eq', and every field that renders is inside its 'Show'.
--
--   The per-page solar slot (#1869) is normalised away first. It is
--   stamped by 'updateWorldTiles' once the owning page is known, so a
--   run taken from a page's cache carries it and one built straight
--   from 'renderWorldQuads' does not — a difference about page
--   ATTRIBUTION, which is a different gate
--   ("Test.Headless.World.Render.SolarAttribution") and would otherwise
--   make every comparison here fail for the wrong reason.
quadPrints ∷ V.Vector SortableQuad → [Text]
quadPrints = map (tshow . setQuadSolarPage solarPageNone) . V.toList

-- * Spec

spec ∷ Spec
spec = describe "The cached quad pass is built from the snapshot it is stamped with (#1720)"
     $ aroundAll setup $ do
    fixtureSpec
    postCaptureSpec
    perFieldSpec
    onePassSpec
  where
    -- Isolation wraps the boot (#1357): engine init is itself a config
    -- writer.
    setup act = withIsolatedResourceRoot $ do
        EngineInitResult env ← initializeEngineHeadlessQuiet
        act env

-- | The fixture emits quads at all, and emits them because of the
--   camera state rather than regardless of it. Without this, every
--   equality below could hold by both sides being empty.
fixtureSpec ∷ SpecWith EngineEnv
fixtureSpec = describe "the fixture" $ do

    it "emits quads for the reference snapshot" $ \env → do
        (wsOne, _) ← resetScene env cameraA
        quads ← buildWith env wsOne cameraA snapA
        length quads `shouldBe` 2 * length solidTiles

    it "emits a DIFFERENT run for the mutated camera's own snapshot" $
        \env → do
            (wsOne, _) ← resetScene env cameraA
            reference ← buildWith env wsOne cameraA snapA
            mutated   ← buildWith env wsOne cameraB snapB
            mutated `shouldNotBe` reference

-- | Requirement 3 (and requirement 7's direct assertion): mutating the
--   live camera after capture cannot change the produced geometry.
postCaptureSpec ∷ SpecWith EngineEnv
postCaptureSpec = describe "a live-camera mutation after capture" $ do

    it "leaves the quads built from the captured snapshot unchanged" $
        \env → do
            (wsOne, _) ← resetScene env cameraA
            -- Capture, then let the "main thread" rewrite the camera
            -- underneath the world thread before it builds.
            reference ← buildWith env wsOne cameraA snapA
            afterMutation ← buildWith env wsOne cameraB snapA
            afterMutation `shouldBe` reference

    it "cannot be observed by the build at all — any live camera agrees" $
        \env → do
            (wsOne, _) ← resetScene env cameraA
            reference ← buildWith env wsOne cameraA snapA
            forM_ [cameraB, cameraA { camFacing = FaceNorth }
                  , cameraA { camPosition = (99, 99) }
                  , cameraB { camZSlice = 40 }] $ \live → do
                built ← buildWith env wsOne live snapA
                built `shouldBe` reference

    it "leaves the stamp and the geometry describing one camera" $
        \env → do
            -- The real cache path: the stamp a page ends up carrying is
            -- the snapshot its quads were built from, so a later reader
            -- comparing against that stamp is comparing against the
            -- camera the geometry actually covers.
            (wsOne, _) ← resetScene env cameraA
            _ ← updateWorldTiles env
            cached ← readIORef (wsQuadCacheRef wsOne)
            case cached of
                Nothing → expectationFailure "no cache entry was published"
                Just wqc → do
                    let stamp = wqcCamera wqc
                    stamp `shouldBe` snapA
                    stamped ← buildWith env wsOne cameraB stamp
                    concatMap quadPrints (Map.elems (wqcQuads wqc))
                        `shouldMatchList` stamped

-- | Requirement 1, field by field: each camera-derived input is taken
--   from the snapshot. A partial fix that still read one of them live
--   fails exactly the example naming it.
perFieldSpec ∷ SpecWith EngineEnv
perFieldSpec = describe "every camera-derived input comes from the snapshot" $ do
    let field name change = it name $ \env → do
            (wsOne, _) ← resetScene env cameraA
            reference ← buildWith env wsOne cameraA snapA
            -- The changed value really does change the geometry...
            viaSnapshot ← buildWith env wsOne cameraA (change snapA)
            viaSnapshot `shouldNotBe` reference
            -- ...and it is read from the snapshot, not the live camera.
            viaLive ← buildWith env wsOne (liveOf (change snapA)) snapA
            viaLive `shouldBe` reference

    field "facing"   (\s → s { wcsFacing = FaceEast })
    field "z-slice"  (\s → s { wcsZSlice = 0 })
    field "zoom"     (\s → s { wcsZoom = 0.05 })
    field "position" (\s → s { wcsPosition = (6.0, -3.0) })
  where
    -- The live camera that carries the snapshot's own values, so the
    -- old second-read implementation would have produced the SNAPSHOT
    -- variant's geometry for it.
    liveOf s = cameraA
        { camPosition = wcsPosition s
        , camZoom     = wcsZoom s
        , camZSlice   = wcsZSlice s
        , camFacing   = wcsFacing s
        }

-- | Requirement 4: one pass, one snapshot, every page.
onePassSpec ∷ SpecWith EngineEnv
onePassSpec = describe "one updateWorldTiles pass" $ do

    it "stamps every visible page with the same snapshot" $ \env → do
        (wsOne, wsTwo) ← resetScene env cameraA
        _ ← updateWorldTiles env
        stampOne ← stampOf wsOne
        stampTwo ← stampOf wsTwo
        stampOne `shouldBe` Just snapA
        stampTwo `shouldBe` stampOne

    it "gives two identically-stocked pages identical geometry" $ \env → do
        (wsOne, wsTwo) ← resetScene env cameraA
        _ ← updateWorldTiles env
        quadsOne ← cachedPrints wsOne
        quadsTwo ← cachedPrints wsTwo
        quadsOne `shouldNotBe` []
        quadsTwo `shouldBe` quadsOne
  where
    stampOf ws = fmap wqcCamera <$> readIORef (wsQuadCacheRef ws)
    cachedPrints ws = do
        cached ← readIORef (wsQuadCacheRef ws)
        pure $ maybe [] (concatMap quadPrints . Map.elems . wqcQuads) cached
