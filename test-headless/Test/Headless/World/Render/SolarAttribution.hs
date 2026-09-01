{-# LANGUAGE Strict #-}
{-# LANGUAGE OverloadedStrings #-}
-- | "Every visible page is lit by its own clock and its own
--   circumference" (#1869).
--
--   @wmVisible@ is a list, and 'World.Thread.Time.tickWorldTime'
--   advances every page on it against that page's own
--   @wsTimeScaleRef@ — so two visible pages legitimately hold
--   different times of day, and they can have been generated at
--   different sizes. Before #1869 the frame carried ONE @sunAngle@ and
--   ONE @worldCircumferenceTiles@, both resolved from the head of that
--   list, so every page but the head was lit by someone else's time and
--   divided by someone else's world.
--
--   The fix gives each vertex a small @solarPage@ slot and publishes a
--   per-page table beside the quads. This gate pins the whole of that,
--   without a GPU, against two in-memory pages that differ in BOTH
--   inputs — a page lit correctly by accident (same clock, or same
--   size) would prove nothing.
--
--   No worldgen anywhere: both pages are 'emptyWorldState's carrying
--   one synthetic chunk, the same fixture style
--   "Test.Headless.World.Render.QuadSnapshot" uses.
--
--   Run just this gate: @cabal test synarchy-test-headless
--   --test-options='--match "per-page solar attribution"'@.
module Test.Headless.World.Render.SolarAttribution (spec) where

import UPrelude
import Test.Hspec
import Test.Headless.Harness.Isolation (withIsolatedResourceRoot)
import Data.IORef (readIORef, writeIORef)
import Data.List (sort, nub)
import Data.Serialize (encode, decode)
import qualified Data.HashMap.Strict as HM
import qualified Data.Map as Map
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU

import Engine.Core.Init (initializeEngineHeadless, EngineInitResult(..))
import Engine.Core.Capability.RenderView
    (RenderViewCapability(..), toRenderViewCapability)
import Engine.Core.Capability.WorldSim
    (WorldSimCapability(..), toWorldSimCapability)
import Engine.Core.State (EngineEnv(..))
import Linear (V4(..))
import Engine.Graphics.Camera (Camera2D(..), CameraFacing(..), defaultCamera)
import Engine.Graphics.Solar
    ( SolarBase, SolarPageEntry(..), SolarPageTable(..)
    , publishedSolar, overriddenSolar, maxSolarPages, solarPageNone
    , solarSlotVertexValue, solarUniformEntries )
import Engine.Scene.Types (LayeredQuads(..), SortableQuad(..))
import Engine.Graphics.Vulkan.Types.Vertex (Vertex(..))
import Structure.Types (emptyChunkStructures)
import World.Chunk.Types
    (ChunkCoord(..), ColumnTiles(..), LoadedChunk(..), chunkSize)
import World.Flora.Types (emptyFloraChunkData)
import World.Fluid.Types (emptyIceMap)
import World.Generate.Types (WorldGenParams(..), defaultWorldGenParams)
import World.Page.Types (WorldPageId(..))
import World.Render (updateWorldTiles)
import World.Render.Camera.Types (WorldQuadCache(..))
import World.Render.Solar (solarSlotAssignment, circumferenceTilesFor)
import World.Render.Zoom.Types (ZoomMapMode(..))
import World.Save.Component.Page
    ( PageCoreDTO(..), WorldPagesDTO(..)
    , toWorldGenParamsDTO, fromWorldGenParamsDTO )
import World.State.Types
    (WorldManager(..), WorldState(..), emptyWorldManager, emptyWorldState)
import World.Thread.Command.UI (handleWorldShowCommand)
import World.Thread.Time (tickWorldTime)
import World.Tile.Types (WorldTileData(..), emptyWorldTileData)
import World.Time.Types (WorldTime(..), worldTimeToSunAngle)
import World.ZoomMap.Types (ZoomChunkEntry(..))
import Test.Headless.Harness.GeneratedIds (fixtureGeneratedWorldIdForPage)

-- * The two pages

-- | Ids chosen so their SORTED order — which is what
--   'solarSlotAssignment' assigns over — is @one@ then @two@,
--   independent of whatever order @wmVisible@ holds them in.
pageOne, pageTwo ∷ WorldPageId
pageOne = WorldPageId "solar_one"
pageTwo = WorldPageId "solar_two"

-- | Different SIZES, so a page divided by the other's circumference is
--   visibly wrong rather than accidentally right.
sizeOne, sizeTwo ∷ Int
sizeOne = 64
sizeTwo = 128

-- | Different CLOCKS, likewise. Dawn against evening.
timeOne, timeTwo ∷ WorldTime
timeOne = WorldTime 6 0
timeTwo = WorldTime 18 30

-- | What each page must be lit by: its OWN angle and its OWN
--   circumference, spelled out here independently of the code under
--   test.
entryOne, entryTwo ∷ SolarPageEntry
entryOne = SolarPageEntry (worldTimeToSunAngle timeOne)
                          (fromIntegral (sizeOne * chunkSize))
entryTwo = SolarPageEntry (worldTimeToSunAngle timeTwo)
                          (fromIntegral (sizeTwo * chunkSize))

slotOne, slotTwo ∷ Word32
slotOne = 1
slotTwo = 2

testFb ∷ (Int, Int)
testFb = (1920, 1080)

-- | The angle @world.setSunAngle@ forces in the override examples.
--   Deliberately neither page's own.
forcedAngle ∷ Float
forcedAngle = 0.9

-- | The page-LESS fallback circumference the UBO carries beside the
--   table — what a vertex naming no page divides by.
fallbackCirc ∷ Float
fallbackCirc = circumferenceTilesFor Nothing

-- | The first @n@ uploaded @vec4@s, which is what the shader reads.
uploaded ∷ Int → SolarBase → SolarPageTable → [V4 Float]
uploaded n base = take n . V.toList . solarUniformEntries base fallbackCirc

-- | The uploaded entry a page with these inputs must produce.
uploadedEntry ∷ Float → SolarPageEntry → V4 Float
uploadedEntry angle entry = V4 angle (speCircumferenceTiles entry) 0 0

-- * Fixture geometry

solidTiles ∷ [(Int, Int)]
solidTiles = [(0, 0), (15, 0), (15, 15)]

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
            let lx = idx `mod` chunkSize
                ly = idx `div` chunkSize
            in if (lx, ly) `elem` solidTiles then solid else air
    in LoadedChunk
        { lcCoord             = ChunkCoord 0 0
        , lcTiles             = V.generate area columnAt
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

-- | A zoom-map chunk entry at the world origin.
zoomEntryAt ∷ Int → Int → ZoomChunkEntry
zoomEntryAt cx cy = ZoomChunkEntry
    { zceChunkX      = cx
    , zceChunkY      = cy
    , zceBaseGX      = cx * chunkSize
    , zceBaseGY      = cy * chunkSize
    , zceTexIndex    = 1
    , zceElev        = 0
    , zceIsOcean     = False
    , zceHasLava     = False
    , zceVegCategory = 0
    , zceHasIce      = False
    }

-- * Cameras

-- | Close in: terrain draws, the zoom map does not
--   (@zoomFadeStart = 1.2@).
terrainCamera ∷ Camera2D
terrainCamera = defaultCamera
    { camPosition = (0, 0)
    , camZoom     = 1.2
    , camZSlice   = 1
    , camFacing   = FaceSouth
    }

-- | Zoomed out past @zoomFadeEnd = 1.6@: the zoom map draws and terrain
--   does not, so the dynamic run is the zoom pass alone.
zoomCamera ∷ Camera2D
zoomCamera = terrainCamera { camZoom = 2.0 }

-- * Scene

-- | Install two pages with the given visible list, each carrying its
--   own clock and its own generated size.
resetScene ∷ EngineEnv → Camera2D → [WorldPageId]
           → IO (WorldState, WorldState)
resetScene env camera visible = do
    wsOne ← emptyWorldState
    wsTwo ← emptyWorldState
    setUpPage wsOne sizeOne timeOne
    setUpPage wsTwo sizeTwo timeTwo
    installPages env camera visible [(pageOne, wsOne), (pageTwo, wsTwo)]
    pure (wsOne, wsTwo)

setUpPage ∷ WorldState → Int → WorldTime → IO ()
setUpPage ws worldSize time = do
    writeIORef (wsTilesRef ws) fixtureTiles
    writeIORef (wsGenParamsRef ws)
        (Just defaultWorldGenParams { wgpWorldSize = worldSize })
    writeIORef (wsTimeRef ws) time
    writeIORef (wsQuadCacheRef ws) Nothing

installPages ∷ EngineEnv → Camera2D → [WorldPageId]
             → [(WorldPageId, WorldState)] → IO ()
installPages env camera visible worlds = do
    writeIORef (worldManagerRef env) emptyWorldManager
        { wmWorlds = worlds, wmVisible = visible }
    writeIORef (rvCameraRef (toRenderViewCapability env)) camera
    writeIORef (rvFramebufferSizeRef (toRenderViewCapability env)) testFb
    writeIORef (rvWindowSizeRef (toRenderViewCapability env)) testFb
    -- The base angle a world tick would have published from the head
    -- page, so nothing below depends on a stale boot value.
    writeIORef (sunAngleRef env) $ publishedSolar $ case visible of
        (p:_) | p ≡ pageTwo → worldTimeToSunAngle timeTwo
        _                   → worldTimeToSunAngle timeOne

-- * Reading the result

tableOf ∷ LayeredQuads → [SolarPageEntry]
tableOf = V.toList . sptEntries . lqSolar

-- | Every @solarPage@ slot present in a run of quads, sorted.
slotsOf ∷ V.Vector SortableQuad → [Word32]
slotsOf = sort . concatMap quadSlots . V.toList
  where
    quadSlots q = map solarPage [sqV0 q, sqV1 q, sqV2 q, sqV3 q]

staticSlots ∷ LayeredQuads → [Word32]
staticSlots = concatMap slotsOf . Map.elems . lqStatic

-- | A page's own cached run: the slot it was stamped with, and the
--   slots its vertices actually carry. Reading the page's OWN cache is
--   what makes the assertion per-page — the published static map merges
--   every page into shared layer runs.
cacheOf ∷ WorldState → IO (Maybe (Word32, [Word32]))
cacheOf ws = do
    cached ← readIORef (wsQuadCacheRef ws)
    pure $ (\wqc → ( wqcSolarSlot wqc
                   , concatMap slotsOf (Map.elems (wqcQuads wqc)) ))
             ⊚ cached

-- | Erase both pages' terrain. A rebuild after this produces NOTHING,
--   so any quad still coming back proves the cache was reused rather
--   than rebuilt.
blankTiles ∷ WorldState → WorldState → IO ()
blankTiles wsOne wsTwo = forM_ [wsOne, wsTwo] $ \ws →
    writeIORef (wsTilesRef ws) emptyWorldTileData

-- * Spec

spec ∷ Spec
spec = describe "per-page solar attribution (#1869)" $ aroundAll setup $ do
    assignmentSpec
    twoPageSpec
    zoomSpec
    reorderSpec
    livenessSpec
    degenerateSpec
    limitSpec
    overrideSpec
    restoreSpec
  where
    -- Isolation wraps the boot (#1357): engine init is itself a config
    -- writer.
    setup act = withIsolatedResourceRoot $ do
        EngineInitResult env ← initializeEngineHeadless
        act env

-- | The pure assignment, on its own.
assignmentSpec ∷ SpecWith EngineEnv
assignmentSpec = describe "the slot assignment" $ do

    it "numbers the visible pages from 1, in sorted id order" $ \_ →
        solarSlotAssignment [pageTwo, pageOne]
            `shouldBe` HM.fromList [(pageOne, slotOne), (pageTwo, slotTwo)]

    it "is identical however the visible list is ordered" $ \_ →
        solarSlotAssignment [pageTwo, pageOne]
            `shouldBe` solarSlotAssignment [pageOne, pageTwo]

    it "measures a circumference in TILES, not chunks" $ \_ → do
        circumferenceTilesFor (Just sizeOne)
            `shouldBe` speCircumferenceTiles entryOne
        -- A page whose generation parameters have not landed yet keeps
        -- the same 128-chunk default the pre-#1869 global path used.
        circumferenceTilesFor Nothing `shouldBe` fromIntegral (128 * chunkSize)

    -- Unreachable in a running engine — 'limitSpec' below covers why —
    -- but the assignment stays total over any list it is handed.
    it "gives pages past the table's length the page-less slot" $ \_ → do
        let ids = [ WorldPageId ("page_" <> tshow (100 + i ∷ Int))
                  | i ← [0 .. maxSolarPages] ]
            assigned = solarSlotAssignment ids
        length ids `shouldBe` maxSolarPages + 1
        map (\i → HM.lookupDefault solarPageNone i assigned) ids
            `shouldBe` map solarSlotVertexValue [0 .. maxSolarPages - 1]
                         ⧺ [solarPageNone]

-- | Requirement 1: two visible pages, each lit by its own state.
twoPageSpec ∷ SpecWith EngineEnv
twoPageSpec = describe "two visible pages with different clocks and sizes" $ do

    it "publishes one table entry per page, each with its own inputs" $
        \env → do
            _ ← resetScene env terrainCamera [pageOne, pageTwo]
            quads ← updateWorldTiles env
            tableOf quads `shouldBe` [entryOne, entryTwo]

    it "makes those entries genuinely different, so nothing passes by luck" $
        \_ → do
            speSunAngle entryOne `shouldNotBe` speSunAngle entryTwo
            speCircumferenceTiles entryOne
                `shouldNotBe` speCircumferenceTiles entryTwo

    it "stamps each page's cached terrain with that page's own slot" $
        \env → do
            (wsOne, wsTwo) ← resetScene env terrainCamera [pageOne, pageTwo]
            _ ← updateWorldTiles env
            one ← cacheOf wsOne
            two ← cacheOf wsTwo
            let expected slot n = Just (slot, replicate n slot)
                corners = 4 * 2 * length solidTiles
            one `shouldBe` expected slotOne corners
            two `shouldBe` expected slotTwo corners

    it "leaves no published world quad unattributed" $ \env → do
        _ ← resetScene env terrainCamera [pageOne, pageTwo]
        quads ← updateWorldTiles env
        let slots = staticSlots quads
        slots `shouldNotBe` []
        filter (≡ solarPageNone) slots `shouldBe` []
        sort (nub slots) `shouldBe` [slotOne, slotTwo]

-- | The zoom map draws through the same longitude-lit pipeline (its
--   bake stamps packed world coordinates), so it is attributed too.
zoomSpec ∷ SpecWith EngineEnv
zoomSpec = describe "the zoom map" $

    it "attributes each page's zoom quads to that page" $ \env → do
        (wsOne, wsTwo) ← resetScene env zoomCamera [pageOne, pageTwo]
        -- Distinct entry COUNTS identify which page a quad came from
        -- without depending on any geometry detail.
        writeIORef (wsZoomCacheRef wsOne) (V.fromList [zoomEntryAt 0 0])
        writeIORef (wsZoomCacheRef wsTwo)
            (V.fromList [zoomEntryAt 0 0, zoomEntryAt 0 1])
        quads ← updateWorldTiles env
        -- Terrain is fully faded out at this zoom, so the dynamic run is
        -- the zoom pass alone.
        Map.null (lqStatic quads) `shouldBe` True
        let slots = slotsOf (lqDynamic quads)
        slots `shouldBe` replicate 4 slotOne ⧺ replicate 8 slotTwo

-- | Requirement 2.
reorderSpec ∷ SpecWith EngineEnv
reorderSpec = describe "reordering the visible list" $

    it "changes no page's lighting and rebuilds no page's geometry" $
        \env → do
            (wsOne, wsTwo) ← resetScene env terrainCamera [pageOne, pageTwo]
            before ← updateWorldTiles env
            beforeOne ← cacheOf wsOne
            beforeTwo ← cacheOf wsTwo
            -- Erase the terrain: a rebuild now would produce nothing.
            blankTiles wsOne wsTwo
            installPages env terrainCamera [pageTwo, pageOne]
                [(pageOne, wsOne), (pageTwo, wsTwo)]
            after ← updateWorldTiles env
            tableOf after `shouldBe` tableOf before
            staticSlots after `shouldBe` staticSlots before
            cacheOf wsOne `shouldReturn` beforeOne
            cacheOf wsTwo `shouldReturn` beforeTwo

-- | The sun has to keep moving inside geometry that is NOT rebuilt —
--   'World.Thread.Time' does not invalidate the quad cache for an
--   ordinary minute.
livenessSpec ∷ SpecWith EngineEnv
livenessSpec = describe "advancing a page's clock" $

    it "relights the reused cached geometry instead of rebuilding it" $
        \env → do
            (wsOne, wsTwo) ← resetScene env terrainCamera [pageOne, pageTwo]
            before ← updateWorldTiles env
            beforeOne ← cacheOf wsOne
            blankTiles wsOne wsTwo
            let laterOne = WorldTime 7 15
            writeIORef (wsTimeRef wsOne) laterOne
            -- Reinstall to restore the camera the caches were stamped
            -- against: the pass itself writes @camZSlice@ while
            -- z-tracking, and a moved camera is a rebuild reason of its
            -- own that would mask the one under test.
            installPages env terrainCamera [pageOne, pageTwo]
                [(pageOne, wsOne), (pageTwo, wsTwo)]
            writeIORef (sunAngleRef env)
                (publishedSolar (worldTimeToSunAngle laterOne))
            after ← updateWorldTiles env
            -- The geometry is the untouched cache…
            staticSlots after `shouldBe` staticSlots before
            cacheOf wsOne `shouldReturn` beforeOne
            -- …and the light it is drawn with has moved anyway.
            tableOf after `shouldBe`
                [ entryOne { speSunAngle = worldTimeToSunAngle laterOne }
                , entryTwo ]
            tableOf after `shouldNotBe` tableOf before

-- | Requirement 3's two ends: one page, and none.
degenerateSpec ∷ SpecWith EngineEnv
degenerateSpec = describe "the single-page and empty-visible cases" $ do

    it "gives a lone visible page slot 1 and its own inputs" $ \env → do
        (wsOne, wsTwo) ← resetScene env terrainCamera [pageOne]
        quads ← updateWorldTiles env
        tableOf quads `shouldBe` [entryOne]
        staticSlots quads `shouldNotBe` []
        sort (nub (staticSlots quads)) `shouldBe` [slotOne]
        cacheOf wsTwo `shouldReturn` Nothing
        cacheOf wsOne ⌦ \cached →
            fst ⊚ cached `shouldBe` Just slotOne

    it "publishes an empty table and no quads with nothing visible" $
        \env → do
            (wsOne, wsTwo) ← resetScene env terrainCamera []
            installPages env terrainCamera []
                [(pageOne, wsOne), (pageTwo, wsTwo)]
            quads ← updateWorldTiles env
            tableOf quads `shouldBe` []
            staticSlots quads `shouldBe` []
            lqDynamic quads `shouldSatisfy` V.null

-- | Attributing EVERY visible page is a contract, not a best effort, so
--   the number of pages that can be visible is the number one frame can
--   describe. 'World.Thread.Command.UI.handleWorldShowCommand' — which
--   is also the handler a load restores visibility through — enforces
--   that, so the page-less overflow branch in 'solarSlotAssignment' is
--   unreachable rather than a silent degradation.
limitSpec ∷ SpecWith EngineEnv
limitSpec = describe "the visible-page limit" $

    it "refuses a show past what one frame can light, so no page overflows" $
        \env → do
            logger ← readIORef (loggerRef env)
            pages ← forM [0 .. maxSolarPages] $ \i → do
                ws ← emptyWorldState
                setUpPage ws sizeOne timeOne
                pure (WorldPageId ("limit_" <> tshow (100 + i ∷ Int)), ws)
            installPages env terrainCamera [] pages
            forM_ pages $ \(pid, _) →
                handleWorldShowCommand (toWorldSimCapability env) logger pid
            visible ← wmVisible ⊚ readIORef (worldManagerRef env)
            -- One more page than the table holds was offered…
            length pages `shouldBe` maxSolarPages + 1
            -- …and exactly the last one attempted stayed hidden.
            length visible `shouldBe` maxSolarPages
            (fst (last pages) `elem` visible) `shouldBe` False
            -- So every page that IS visible owns a distinct slot.
            let slots = HM.elems (solarSlotAssignment visible)
            filter (≡ solarPageNone) slots `shouldBe` []
            length (nub slots) `shouldBe` maxSolarPages

-- | Requirement 5: what the page-less @world.setSunAngle@ means now.
--
--   The override is applied where the table is UPLOADED, not where it
--   is built. The two happen on different threads at different rates —
--   the renderer may draw a table a tick old, and the very next tick
--   clears the override — so an override baked in at build time could
--   only ever reach the GPU by landing inside the microseconds between
--   a tick publishing its clock and that same tick building its table.
overrideSpec ∷ SpecWith EngineEnv
overrideSpec = describe "world.setSunAngle's override window" $ do

    it "is not baked into the published table" $ \env → do
        _ ← resetScene env terrainCamera [pageOne, pageTwo]
        writeIORef (sunAngleRef env) (overriddenSolar forcedAngle)
        quads ← updateWorldTiles env
        tableOf quads `shouldBe` [entryOne, entryTwo]

    it "overlays every page of the table the frame is actually drawing" $
        \env → do
            _ ← resetScene env terrainCamera [pageOne, pageTwo]
            -- Published FIRST; the override arrives afterwards, which is
            -- the only ordering a Lua caller can reliably produce.
            published ← updateWorldTiles env
            writeIORef (sunAngleRef env) (overriddenSolar forcedAngle)
            base ← readIORef (sunAngleRef env)
            uploaded 2 base (lqSolar published) `shouldBe`
                [ uploadedEntry forcedAngle entryOne
                , uploadedEntry forcedAngle entryTwo ]

    it "keeps each page's own circumference while it stands" $ \env → do
        _ ← resetScene env terrainCamera [pageOne, pageTwo]
        published ← updateWorldTiles env
        let forced = uploaded 2 (overriddenSolar forcedAngle) (lqSolar published)
        map (\(V4 _ circ _ _) → circ) forced
            `shouldBe` [ speCircumferenceTiles entryOne
                       , speCircumferenceTiles entryTwo ]
        -- Non-vacuous: the two circumferences differ, so "kept its own"
        -- is a real claim.
        speCircumferenceTiles entryOne
            `shouldNotBe` speCircumferenceTiles entryTwo

    it "leaves every page on its own angle while none stands" $ \env → do
        _ ← resetScene env terrainCamera [pageOne, pageTwo]
        published ← updateWorldTiles env
        uploaded 2 (publishedSolar (worldTimeToSunAngle timeOne))
                 (lqSolar published)
            `shouldBe` [ uploadedEntry (speSunAngle entryOne) entryOne
                       , uploadedEntry (speSunAngle entryTwo) entryTwo ]

    it "fills slots the publication does not describe with the page-less pair" $
        \env → do
            _ ← resetScene env terrainCamera [pageOne]
            published ← updateWorldTiles env
            let base = publishedSolar (worldTimeToSunAngle timeOne)
                rest = drop 1 (uploaded maxSolarPages base (lqSolar published))
            rest `shouldBe` replicate (maxSolarPages - 1)
                (V4 (worldTimeToSunAngle timeOne) fallbackCirc 0 0)

    it "lasts until the next visible-page clock publication" $ \env → do
        _ ← resetScene env terrainCamera [pageOne, pageTwo]
        published ← updateWorldTiles env
        writeIORef (sunAngleRef env) (overriddenSolar forcedAngle)
        -- A world tick with the clock frozen: it advances nothing, and
        -- republishing the head page's angle is the whole of what ends
        -- the override.
        writeIORef (wsEnginePausedRef (toWorldSimCapability env)) True
        tickWorldTime env 0
        base ← readIORef (sunAngleRef env)
        base `shouldBe` publishedSolar (worldTimeToSunAngle timeOne)
        uploaded 2 base (lqSolar published) `shouldBe`
            [ uploadedEntry (speSunAngle entryOne) entryOne
            , uploadedEntry (speSunAngle entryTwo) entryTwo ]

-- | Requirement 4. A real save carries a page's clock and its generated
--   size through 'PageCoreDTO'; this puts both pages' through that
--   type's own wire encoding and rebuilds the session from what comes
--   back, with the restored primary at the head of the visible list the
--   way 'World.Load.Publish' orders it.
restoreSpec ∷ SpecWith EngineEnv
restoreSpec = describe "a restored multi-page session" $

    it "lights each decoded page by its own decoded clock and size" $
        \env → do
            let dto pageId params time = PageCoreDTO
                    { pcPageId     = pageId
                    , pcGenParams  = toWorldGenParamsDTO params
                    , pcCameraX    = 0
                    , pcCameraY    = 0
                    , pcTimeHour   = wtHour time
                    , pcTimeMinute = wtMinute time
                    , pcDateYear   = 1
                    , pcDateMonth  = 1
                    , pcDateDay    = 1
                    , pcMapMode    = ZMDefault
                    , pcIdentity   = Nothing
                    , pcGeneratedId =
                        Just (fixtureGeneratedWorldIdForPage pageId)
                    }
                paramsFor n = defaultWorldGenParams { wgpWorldSize = n }
                written = WorldPagesDTO
                    [ dto pageOne (paramsFor sizeOne) timeOne
                    , dto pageTwo (paramsFor sizeTwo) timeTwo ]
            case decode (encode written) of
                Left err → expectationFailure
                    ("world-pages payload did not decode: " ⧺ err)
                Right (WorldPagesDTO decoded) → do
                    restored ← forM decoded $ \page → do
                        ws ← emptyWorldState
                        writeIORef (wsTilesRef ws) fixtureTiles
                        writeIORef (wsGenParamsRef ws)
                            (Just (fromWorldGenParamsDTO (pcGenParams page)))
                        writeIORef (wsTimeRef ws)
                            (WorldTime (pcTimeHour page) (pcTimeMinute page))
                        pure (pcPageId page, ws)
                    -- The restored PRIMARY heads the visible list, which
                    -- under the old head-page attribution is exactly what
                    -- decided every page's lighting.
                    installPages env terrainCamera [pageTwo, pageOne] restored
                    quads ← updateWorldTiles env
                    tableOf quads `shouldBe` [entryOne, entryTwo]
                    sort (nub (staticSlots quads))
                        `shouldBe` [slotOne, slotTwo]
