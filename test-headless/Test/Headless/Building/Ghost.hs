{-# LANGUAGE OverloadedStrings #-}
-- | "Both ghost states are the building's own art" (#1845, DTV-10).
--
--   A planned building used to be a generic 96x64 category marker
--   repeated once per footprint tile, at a fixed 45 % dressed as a
--   progress ramp; the placement preview meanwhile already drew the
--   real sprite, and more opaquely than the commitment it previewed.
--   D-19 makes the three ghost states of a building — PREVIEW,
--   committed DESIGNATION, and the staked pre-delivery INSTANCE — one
--   picture at two opacities, and this is the gate on that.
--
--   Two halves, deliberately:
--
--     * The PURE half asserts the presentation itself against
--       'Building.Render.buildingGhostQuad',
--       'Building.Render.ghostToQuad' and
--       'Building.Render.buildingToQuad'. No engine, no GPU, no texture
--       system — those are the three functions the three states are
--       drawn by, so agreement between them is the contract.
--     * The ENGINE half drives the real
--       'World.Render.CursorQuads.renderWorldCursorQuadsScanned' over a
--       synthetic page (the 'Test.Headless.World.Render.SceneStats'
--       fixture shape), because "one quad, not six" and "no quad at
--       all" are claims about the render PASS, not about a quad builder
--       it might not call.
--
--   Note what the engine half can and cannot see. The cursor pass
--   builds its ghost from a pure function and therefore DOES emit
--   headlessly; 'Building.Render.renderBuildingQuadsScanned' returns
--   nothing without a texture system, which is the normal GPU-free
--   state. So the staked instance's own quad is asserted through the
--   pure 'buildingToQuad' here, and the pixels are
--   'tools/construction_blueprint_footprint_probe.py'\'s job.
--
--   Run just this gate: @cabal test synarchy-test-headless
--   --test-options='--match "building ghost"'@.
module Test.Headless.Building.Ghost (spec) where

import UPrelude
import Test.Hspec
import Test.Headless.Harness.Isolation (withIsolatedResourceRoot)
import Test.Headless.Harness.Log (initializeEngineHeadlessQuiet)
import Data.IORef (readIORef, writeIORef)
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import qualified Data.Map.Strict as Map
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU
import System.Directory (doesFileExist)

import Building.Render (buildingGhostQuad, buildingToQuad, ghostToQuad)
import Building.Schema
import Building.Types
    ( BuildingAnimation(..), BuildingDef(..), BuildingGhost(..), BuildingId(..)
    , BuildingInstance(..), BuildingManager(..), emptyBuildingManager )
import Building.Visual
    ( BuildingQuadRect(..), BuildingVisual(..), buildingQuadRect
    , buildingStakedAt, designatedGhostAlpha
    , ghostPieceTint, placedBuildingVisual, previewGhostAlpha
    , previewBuildingTexture, spriteAnchorOffset )
import Building.Types (BuildingActivity(..), currentActivity)
import Engine.Asset.Handle (TextureHandle(..))
import Engine.Core.Init (EngineInitResult(..))
import Engine.Core.State (EngineEnv(..))
import Engine.Core.Capability.RenderView
    (RenderViewCapability(..), toRenderViewCapability)
import Engine.Graphics.Camera (Camera2D(..), CameraFacing(..), defaultCamera)
import Item.Types (ItemInstance(..))
import Engine.Graphics.Vulkan.Types.Vertex
    (Vec2(..), Vec4(..), Vertex(..), noFaceMapVertexId)
import Engine.Scene.Types (SortableQuad(..))
import World.Chunk.Types
    (ChunkCoord(..), ColumnTiles(..), LoadedChunk(..), chunkSize)
import World.Construct.Attempt (firstConstructAttemptId)
import World.Construct.Types
    ( ConstructDesignation(..), ConstructStatus(..), ConstructTarget(..)
    , StructurePiece(..), newConstructDesignation )
import World.Cursor.Types (CursorState(..), emptyCursorState)
import World.Flora.Types (emptyFloraChunkData)
import World.Fluid.Types (emptyIceMap)
import World.Generate.Types (WorldGenParams(..), defaultWorldGenParams)
import World.Page.Types (WorldPageId(..))
import World.Render.CursorQuads (renderWorldCursorQuadsScanned)
import World.State.Types
    (WorldManager(..), WorldState(..), emptyWorldManager, emptyWorldState)
import Structure.Types (emptyChunkStructures)
import World.Tile.Types (WorldTileData(..))
import World.Tool.Types (ToolMode(..))

-- * Fixture definitions

-- | The plan under test: a 2x3 building whose sprite is deliberately
--   neither the 96x64 base tile nor the retired placeholder's size, and
--   whose sprite anchor is @tile_bottom@. Both matter — a per-tile
--   marker could express neither, so a fixture that skipped them would
--   pass on a ghost that still drew a diamond.
plannedHandle ∷ TextureHandle
plannedHandle = TextureHandle 41

-- | Its per-facing views are all one path (a legacy declaration), so
--   the facing never changes the handle and every assertion below is
--   about ghost policy rather than about #2088's view selection, which
--   'Test.Headless.Building.CameraFacing' already owns.
plannedDef ∷ BuildingDef
plannedDef = fixtureDef "planned_hall" 2 3 plannedHandle "tile_bottom" 0

-- | A def with real build work and an outstanding material, so a placed
--   instance of it is the PRE-DELIVERY ghost.
workDef ∷ BuildingDef
workDef = (fixtureDef "worked_hall" 2 3 plannedHandle "tile_bottom" 8)
    { bdMaterials = HM.singleton "plank" 2 }

-- | A zero-@build_work@ def: the portal's time-driven appearance
--   lifecycle, which must stay semantically distinct from construction
--   progress (requirement 6).
appearDef ∷ BuildingDef
appearDef = (fixtureDef "appearing_hall" 1 1 plannedHandle "diamond_bottom" 0)
    { bdRoleAnims  = Map.singleton RoleAppearance "appear"
    , bdAnimations = HM.singleton "appear" BuildingAnimation
        { banFps    = 4
        , banLoop   = False
        , banFrames = legacyAssets (V.replicate 8 plannedHandle) }
    }

fixtureDef ∷ Text → Int → Int → TextureHandle → Text → Float → BuildingDef
fixtureDef name w h tex anchor work = BuildingDef
    { bdName            = name
    , bdDisplayName     = name
    , bdCategory        = "Test"
    , bdDescription     = ""
    , bdTextures        = legacyAssets tex
    , bdIconTexture     = TextureHandle 0
    , bdTileW           = w
    , bdTileH           = h
    , bdPlacement       = "flat_ground"
    , bdIsStarting      = False
    , bdRace            = "human"
    , bdSpriteAnchor    = anchor
    , bdBuildWork       = work
    , bdMaterials       = HM.empty
    , bdStorageCapacity = 0
    , bdOperations      = []
    , bdAnimations      = HM.empty
    , bdRoleAnims       = Map.empty
    , bdVisualClass     = FreestandingInstallation
    , bdPowerDrain      = 0
    , bdPowerNode       = Nothing
    }

-- | The same instance with 'workDef's material bill paid, which is what
--   closes the pre-delivery ghost.
delivered ∷ BuildingInstance → BuildingInstance
delivered inst = inst
    { biMaterialsDelivered = HM.singleton "plank"
        [ fixtureItem 1, fixtureItem 2 ] }

fixtureItem ∷ Word64 → ItemInstance
fixtureItem iid = ItemInstance
    { iiDefName     = "plank"
    , iiCurrentFill = 0
    , iiQuality     = 100
    , iiCondition   = 100
    , iiWeight      = 1
    , iiSharpness   = 100
    , iiContents    = []
    , iiInstanceId  = iid
    , iiTemp        = Nothing
    , iiBulk        = Nothing
    , iiStorage     = Nothing
    }

instanceOf ∷ BuildingDef → Int → Int → Int → Float → BuildingInstance
instanceOf def ax ay az progress = BuildingInstance
    { biDefName            = bdName def
    , biPage               = fixturePage
    , biTexture            = plannedHandle
    , biAnchorX            = ax
    , biAnchorY            = ay
    , biGridZ              = az
    , biSpawnedAt          = 0
    , biTileW              = bdTileW def
    , biTileH              = bdTileH def
    , biSpawnRemaining     = -1
    , biBuildProgress      = progress
    , biMaterialsDelivered = HM.empty
    , biStorage            = []
    }

-- * Shared render inputs

facing ∷ CameraFacing
facing = FaceSouth

zSlice ∷ Int
zSlice = 1

effDepth ∷ Int
effDepth = 32

-- | Deliberately NOT 1: requirement 2 makes both factors multipliers
--   over the frame's own zoom fade, and a unit tileAlpha would let a
--   ghost that ignored the fade pass.
tileAlpha ∷ Float
tileAlpha = 0.8

texSizes ∷ HM.HashMap TextureHandle (Int, Int)
texSizes = HM.singleton plannedHandle (192, 192)

-- The tile the whole spec plans and stakes on, and its stored surface z.
anchorTile ∷ (Int, Int)
anchorTile = (0, 0)

anchorZ ∷ Int
anchorZ = 0

designationGhost ∷ BuildingDef → SortableQuad
designationGhost def =
    buildingGhostQuad (const 0) noFaceMapVertexId facing zSlice texSizes
                      tileAlpha designatedGhostAlpha True def
                      (fst anchorTile) (snd anchorTile) anchorZ

previewGhost ∷ BuildingDef → Bool → SortableQuad
previewGhost def valid =
    ghostToQuad (const 0) noFaceMapVertexId facing zSlice texSizes tileAlpha
        BuildingGhost { bgDefName = bdName def
                      , bgGridX = fst anchorTile
                      , bgGridY = snd anchorTile
                      , bgGridZ = anchorZ
                      , bgValid = valid }
        def

-- | The staked pre-delivery instance's own quad, from the PLACED path.
stakedGhost ∷ BuildingDef → Maybe SortableQuad
stakedGhost def =
    buildingToQuad (const 0) noFaceMapVertexId facing zSlice effDepth
                   tileAlpha False
                   (instanceOf def (fst anchorTile) (snd anchorTile) anchorZ 0)
                   (Just def) 0 texSizes

-- * Quad readers

quadBounds ∷ SortableQuad → (Float, Float, Float, Float)
quadBounds q =
    let ps = [ p | Vertex { pos = p } ← [sqV0 q, sqV1 q, sqV2 q, sqV3 q] ]
        xs = [ x | Vec2 x _ ← ps ]
        ys = [ y | Vec2 _ y ← ps ]
    in (minimum xs, minimum ys, maximum xs, maximum ys)

quadTint ∷ SortableQuad → Vec4
quadTint q = let Vertex { color = c } = sqV0 q in c

quadAlpha ∷ SortableQuad → Float
quadAlpha q = let Vec4 _ _ _ a = quadTint q in a

closeTo ∷ Float → Float → Bool
closeTo want got = abs (want - got) < 1e-4

boundsAgree ∷ (Float, Float, Float, Float) → (Float, Float, Float, Float)
            → Bool
boundsAgree (a0, b0, c0, d0) (a1, b1, c1, d1) =
    all (uncurry closeTo) [(a0, a1), (b0, b1), (c0, c1), (d0, d1)]

-- * The pure half

presentationSpec ∷ Spec
presentationSpec = describe "the two building ghost states" $ do

    it "resolve the identical texture and geometry, differing only in\
       \ opacity" $ do
        let p = previewGhost plannedDef True
            d = designationGhost plannedDef
        -- Asserted as the two RESOLVED values being equal, so the paths
        -- cannot drift into separate conventions later.
        sqTexture p `shouldBe` sqTexture d
        sqTexture d `shouldBe` previewBuildingTexture facing plannedDef
        quadBounds p `shouldSatisfy` boundsAgree (quadBounds d)
        sqSortKey p `shouldBe` sqSortKey d
        sqLayer p `shouldBe` sqLayer d
        quadTint p `shouldNotBe` quadTint d

    it "draw at exactly tileAlpha*0.25 (preview) and tileAlpha*0.60\
       \ (designated)" $ do
        quadAlpha (previewGhost plannedDef True)
            `shouldSatisfy` closeTo (tileAlpha * 0.25)
        quadAlpha (designationGhost plannedDef)
            `shouldSatisfy` closeTo (tileAlpha * 0.60)
        -- Named, so the two constants and the two call sites agree.
        previewGhostAlpha `shouldBe` 0.25
        designatedGhostAlpha `shouldBe` 0.60
        -- D-19's whole point: the commitment reads as the more solid of
        -- the two.
        previewGhostAlpha `shouldSatisfy` (< designatedGhostAlpha)

    it "size the quad from the def's OWN sprite and sprite anchor" $ do
        let d = designationGhost plannedDef
            placed = buildingQuadRect facing zSlice texSizes
                         (spriteAnchorOffset (Just plannedDef))
                         (fst anchorTile) (snd anchorTile) anchorZ
                         plannedHandle
        quadBounds d `shouldSatisfy` boundsAgree
            (bqX placed, bqY placed, bqX placed + bqW placed
            , bqY placed + bqH placed)
        -- tile_bottom really is in play, so the anchor handling is
        -- observable rather than incidentally zero.
        spriteAnchorOffset (Just plannedDef) `shouldSatisfy` (> 0)

    it "tint an invalid PREVIEW red" $ do
        let invalid = previewGhost plannedDef False
            valid   = previewGhost plannedDef True
        quadTint invalid `shouldBe`
            ghostPieceTint tileAlpha previewGhostAlpha False
        quadTint invalid `shouldNotBe` quadTint valid
        quadTint valid `shouldBe`
            ghostPieceTint tileAlpha previewGhostAlpha True

    it "make staking produce no geometry or opacity jump" $ do
        -- Requirement 3 / the "no jump at staking" acceptance: the
        -- committed designation and the staked pre-delivery instance
        -- are the same picture, so the hand-off between them is
        -- invisible.
        d ← maybe (expectationFailure "no staked quad" >> pure undefined)
                  pure (stakedGhost workDef)
        let plan = designationGhost workDef
        quadBounds plan `shouldSatisfy` boundsAgree (quadBounds d)
        sqTexture plan `shouldBe` sqTexture d
        sqSortKey plan `shouldBe` sqSortKey d
        quadAlpha plan `shouldSatisfy` closeTo (quadAlpha d)
        quadAlpha d `shouldSatisfy` closeTo (tileAlpha * designatedGhostAlpha)
        -- It really is the pre-delivery state, not an already-built one.
        bvGhost (placedBuildingVisual facing 0
                    (instanceOf workDef 0 0 0 0) (Just workDef))
            `shouldBe` True

    it "hand off to progress art once delivery completes and work\
       \ begins" $ do
        -- Requirement 6: the ghost ends when the MATERIALS gate opens —
        -- that is what "pre-delivery" means — and the existing
        -- bdBuildWork-driven frame selection takes over from there.
        let waiting = instanceOf workDef 0 0 anchorZ 0
            started = delivered (instanceOf workDef 0 0 anchorZ 1)
        bvGhost (placedBuildingVisual facing 0 waiting (Just workDef))
            `shouldBe` True
        bvGhost (placedBuildingVisual facing 0 started (Just workDef))
            `shouldBe` False
        currentActivity 0 started workDef `shouldBe` Constructing
        -- …and a zero-build_work def is NOT on that path at all: its
        -- appearance is driven by the CLOCK against its own declared
        -- animation (8 frames at 4 fps = 2 s), never by build progress,
        -- and it is never a pre-delivery ghost because it has no
        -- materials gate to be outstanding. Unchanged by this slice.
        let appearing = instanceOf appearDef 0 0 anchorZ 0
        bvGhost (placedBuildingVisual facing 0 appearing (Just appearDef))
            `shouldBe` False
        currentActivity 1.0 appearing appearDef `shouldBe` Appearing
        currentActivity 3.0 appearing appearDef `shouldBe` Built
        -- Pouring "progress" into it moves nothing: that dial belongs
        -- to construction and this lifecycle does not read it.
        currentActivity 1.0 (appearing { biBuildProgress = 99 }) appearDef
            `shouldBe` Appearing

-- * The engine half

renderSpec ∷ SpecWith EngineEnv
renderSpec = describe "the committed designation render pass" $ do

    it "emits ONE quad for a 2x3 building, not one per footprint tile" $
      \env → do
        ws ← scene env
        plan ws plannedDef
        (scanned, quads) ← cursorPass env ws
        V.length quads `shouldBe` 1
        -- The candidate count moved with the behaviour: the map entry
        -- IS the candidate now.
        scanned `shouldBe` 1
        bdTileW plannedDef * bdTileH plannedDef `shouldSatisfy` (> 1)
        quadBounds (V.head quads)
            `shouldSatisfy` boundsAgree (quadBounds (designationGhost plannedDef))
        quadAlpha (V.head quads)
            `shouldSatisfy` closeTo (tileAlpha * designatedGhostAlpha)

    it "emits nothing for a def it cannot resolve, and fabricates no\
       \ anchor tile" $ \env → do
        ws ← scene env
        -- The other definitions stay REGISTERED: "no fallback" has to
        -- mean no quad at all, not merely no quad when the manager
        -- happens to be empty. An implementation that substituted some
        -- other def's art would pass against an empty registry.
        defs ← bmDefs <$> readIORef (buildingManagerRef env)
        HM.size defs `shouldSatisfy` (> 0)
        writeIORef (wsConstructDesignationsRef ws) $ HM.singleton anchorTile
            (newConstructDesignation anchorZ (CtBuilding "no_such_building")
                                     firstConstructAttemptId)
        (scanned, quads) ← cursorPass env ws
        V.length quads `shouldBe` 0
        -- Still a candidate that was evaluated and rejected — that gap
        -- is what the telemetry is for.
        scanned `shouldBe` 1
        -- The warning is deduplicated per unknown def per session, so
        -- the cursor state records it exactly once however many frames
        -- run.
        _ ← cursorPass env ws
        _ ← cursorPass env ws
        cs ← readIORef (wsCursorRef ws)
        constructMissingDefsWarned cs
            `shouldBe` HS.singleton "no_such_building"

    it "yields to the staked instance instead of drawing a second 60%\
       \ ghost" $ \env → do
        -- The staking hand-off crosses two queues, so BOTH orders of
        -- application have to render exactly one 60 % ghost.
        ws ← scene env
        plan ws workDef
        -- Before the stake, the designation owns the pixel.
        (_, planned) ← cursorPass env ws
        V.length planned `shouldBe` 1
        -- Order A — the spawn applied first, the designation removal
        -- still queued: designation AND instance are both present, and
        -- the pass must not add a second 60 % quad over the first.
        stake env workDef
        (_, both) ← cursorPass env ws
        V.length both `shouldBe` 0
        -- …and the one quad in that frame is the instance's, which is
        -- the same 60 % picture the designation was drawing.
        (quadAlpha <$> stakedGhost workDef) `shouldSatisfy`
            maybe False (closeTo (tileAlpha * designatedGhostAlpha))
        -- Order B — the designation removed first: the AI holds the
        -- completion until the instance is observable
        -- (scripts/unit_ai_construct_site.stakeBuilding), so the state
        -- with neither present is not reachable from a stake. With the
        -- designation gone the pass draws nothing, as it must.
        writeIORef (wsConstructDesignationsRef ws) HM.empty
        (_, afterRemoval) ← cursorPass env ws
        V.length afterRemoval `shouldBe` 0
        -- The yield is keyed on this designation's OWN stake: another
        -- definition at the same anchor is not it.
        buildingStakedAt fixturePage (bdName workDef) anchorTile
            . bmInstances <$> readIORef (buildingManagerRef env)
            >>= (`shouldBe` True)
        buildingStakedAt fixturePage "someone_else" anchorTile
            . bmInstances <$> readIORef (buildingManagerRef env)
            >>= (`shouldBe` False)
        buildingStakedAt (WorldPageId "other_page") (bdName workDef)
            anchorTile . bmInstances <$> readIORef (buildingManagerRef env)
            >>= (`shouldBe` False)

    it "never tints a designation, and never ramps it with progress" $
      \env → do
        -- Requirement 5: the red is placement feedback and a committed
        -- job has no placement left to refuse — so the tint is neutral
        -- under every status a designation can hold.
        -- Requirement 4: the 0.45 + 0.55*cdProgress ramp is DELETED,
        -- not retuned, so progress moves nothing. (It never moved
        -- anything either: a CtBuilding accrues its progress on the
        -- staked instance, never on the designation.)
        ws ← scene env
        let states = [ (st, pr) | st ← [CsPending, CsClaimed, CsPlacing
                                       , CsComplete]
                                , pr ← [0, 0.5, 1] ]
        forM_ states $ \(st, pr) → do
            writeIORef (wsConstructDesignationsRef ws) $
                HM.singleton anchorTile
                    (newConstructDesignation anchorZ
                        (CtBuilding (bdName plannedDef))
                        firstConstructAttemptId)
                            { cdStatus = st, cdProgress = pr }
            (_, quads) ← cursorPass env ws
            V.length quads `shouldBe` 1
            let Vec4 r g b a = quadTint (V.head quads)
            (r, g, b) `shouldBe` (1, 1, 1)
            a `shouldSatisfy` closeTo (tileAlpha * designatedGhostAlpha)

    it "leaves a structure designation to its own ghost pass" $ \env → do
        ws ← scene env
        writeIORef (wsConstructDesignationsRef ws) $ HM.singleton anchorTile
            (newConstructDesignation anchorZ
                (CtStructure StructurePiece { spPack = "dungeon_1"
                                            , spKind = "floor"
                                            , spEdge = Nothing })
                firstConstructAttemptId)
        (scanned, quads) ← cursorPass env ws
        -- No building ghost at all: the structure half is #1846's, and
        -- it counts and draws its own candidates through
        -- 'World.Render.StructureGhost' (which resolves no art here,
        -- there being no registered catalogue).
        V.length quads `shouldBe` 0
        -- ONE candidate, contributed by that structure builder alone.
        -- The building fold must add nothing for a structure target, or
        -- every structure site would be counted twice.
        scanned `shouldBe` 1

    it "no longer ships the retired category placeholder" $ \_env → do
        exists ← doesFileExist
            "assets/textures/ui/hud/utility/construct_designate_building.png"
        exists `shouldBe` False

-- * Engine fixture

fixturePage ∷ WorldPageId
fixturePage = WorldPageId "building_ghost_page"

worldSizeChunks ∷ Int
worldSizeChunks = 128

-- | One in-memory page with a single flat chunk, visible and alone.
scene ∷ EngineEnv → IO WorldState
scene env = do
    ws ← emptyWorldState
    writeIORef (wsTilesRef ws) fixtureTiles
    writeIORef (wsGenParamsRef ws)
        (Just defaultWorldGenParams { wgpWorldSize = worldSizeChunks })
    writeIORef (wsToolModeRef ws) InfoTool
    writeIORef (wsCursorRef ws) emptyCursorState
    writeIORef (worldManagerRef env) emptyWorldManager
        { wmWorlds = [(fixturePage, ws)], wmVisible = [fixturePage] }
    writeIORef (buildingManagerRef env) emptyBuildingManager
        { bmDefs = HM.fromList
            [ (bdName d, d) | d ← [plannedDef, workDef, appearDef] ] }
    writeIORef (rvCameraRef (toRenderViewCapability env)) defaultCamera
        { camPosition = (0, 0), camZoom = 1.2, camZSlice = zSlice
        , camFacing = facing, camZTracking = False }
    writeIORef (rvFramebufferSizeRef (toRenderViewCapability env)) (1920, 1080)
    writeIORef (rvWindowSizeRef (toRenderViewCapability env)) (1920, 1080)
    -- The ghost sizes from the def's OWN sprite, so the pass has to be
    -- told how big that sprite is; without this it would silently fall
    -- back to the base tile and every geometry claim below would be
    -- about a 96x64 diamond again.
    writeIORef (rvTextureSizeRef (toRenderViewCapability env)) texSizes
    pure ws

fixtureTiles ∷ WorldTileData
fixtureTiles = WorldTileData
    { wtdChunks = HM.singleton (ChunkCoord 0 0) chunk, wtdMaxChunks = 1 }
  where
    area = chunkSize * chunkSize
    solid = ColumnTiles { ctStartZ = 0
                        , ctMats = VU.fromList [1]
                        , ctSlopes = VU.fromList [0]
                        , ctVeg = VU.fromList [0] }
    chunk = LoadedChunk
        { lcCoord             = ChunkCoord 0 0
        , lcTiles             = V.replicate area solid
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

plan ∷ WorldState → BuildingDef → IO ()
plan ws def = writeIORef (wsConstructDesignationsRef ws) $
    HM.singleton anchorTile
        (newConstructDesignation anchorZ (CtBuilding (bdName def))
                                 firstConstructAttemptId)

stake ∷ EngineEnv → BuildingDef → IO ()
stake env def = do
    bm ← readIORef (buildingManagerRef env)
    writeIORef (buildingManagerRef env) bm
        { bmInstances = HM.singleton (BuildingId 1)
            (instanceOf def (fst anchorTile) (snd anchorTile) anchorZ 0) }

cursorPass ∷ EngineEnv → WorldState → IO (Int, V.Vector SortableQuad)
cursorPass env ws =
    renderWorldCursorQuadsScanned env fixturePage ws tileAlpha

spec ∷ Spec
spec = describe "building ghost" $ do
    presentationSpec
    aroundAll setup renderSpec
  where
    -- Isolation wraps the boot (#1357): engine init is itself a config
    -- writer.
    setup act = withIsolatedResourceRoot $ do
        EngineInitResult env ← initializeEngineHeadlessQuiet
        act env
