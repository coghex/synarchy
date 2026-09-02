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
import qualified HsLua as Lua
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
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
    (Vec2(..), Vec4(..), Vertex(..), WorldUV, noFaceMapVertexId)
import Engine.Scene.Types (SortableQuad(..))
import World.Chunk.Types
    (ColumnTiles(..), LoadedChunk(..), chunkSize)
import World.Construct.Attempt (firstConstructAttemptId)
import World.Construct.Types
    ( ConstructDesignation(..), ConstructStatus(..), ConstructTarget(..)
    , StructurePiece(..), newConstructDesignation )
import World.Cursor.Types (CursorState(..), emptyCursorState)
import World.Flora.Types (emptyFloraChunkData)
import World.Fluid.Types (emptyIceMap)
import World.Generate.Types (WorldGenParams(..), defaultWorldGenParams)
import World.Page.Types (WorldPageId(..))
import World.Generate (viewDepth)
import World.Grid (gridToWorld)
import World.Render.ChunkCulling (isChunkVisibleWrapped)
import World.Render.CursorQuads (renderWorldCursorQuadsScanned)
import World.Render.ViewBounds (computeViewBounds)
import World.Chunk.Types (ChunkCoord(..))
import Structure.Render (translateQuad)
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
designationGhost def = designationGhostAt def anchorZ

-- | The designation ghost at an arbitrary grid z, so the camera band
--   can be exercised on its own.
designationGhostAt ∷ BuildingDef → Int → SortableQuad
designationGhostAt def z =
    fromMaybe (error "the fixture ghost was culled by the camera band")
              (designationGhostMaybe def z)

designationGhostMaybe ∷ BuildingDef → Int → Maybe SortableQuad
designationGhostMaybe def z =
    buildingGhostQuad (const 0) noFaceMapVertexId facing zSlice effDepth
                      texSizes tileAlpha designatedGhostAlpha True def
                      (fst anchorTile) (snd anchorTile) z

previewGhost ∷ BuildingDef → Bool → SortableQuad
previewGhost def valid =
    fromMaybe (error "the fixture preview was culled by the camera band")
              (previewGhostAt def valid anchorZ)

previewGhostAt ∷ BuildingDef → Bool → Int → Maybe SortableQuad
previewGhostAt def valid z =
    ghostToQuad (const 0) noFaceMapVertexId facing zSlice effDepth texSizes
        tileAlpha
        BuildingGhost { bgDefName = bdName def
                      , bgGridX = fst anchorTile
                      , bgGridY = snd anchorTile
                      , bgGridZ = z
                      , bgValid = valid }
        def

-- | The staked pre-delivery instance's own quad, from the PLACED path.
stakedGhost ∷ BuildingDef → Maybe SortableQuad
stakedGhost def = stakedGhostAt def anchorZ

stakedGhostAt ∷ BuildingDef → Int → Maybe SortableQuad
stakedGhostAt def z =
    buildingToQuad (const 0) noFaceMapVertexId facing zSlice effDepth
                   tileAlpha False
                   (instanceOf def (fst anchorTile) (snd anchorTile) z 0)
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

quadWorldUV ∷ SortableQuad → WorldUV
quadWorldUV q = let Vertex { worldUV = w } = sqV0 q in w

-- | A world small enough that a camera parked one world-width away
--   reaches the fixture chunk only through a non-identity u-alias.
seamWorldChunks ∷ Int
seamWorldChunks = 8

-- | That camera position, in world units: parked on the fixture
--   anchor's u-ALIAS one whole world to the east, the same way
--   'Test.Headless.World.DesignationSeam' frames its own seam view.
--   Reaching the real chunk from here needs a non-identity alias.
seamCamPos ∷ (Float, Float)
seamCamPos = gridToWorld FaceSouth
                 (fst anchorTile + seamWorldChunks * chunkSize)
                 (snd anchorTile)

-- | The untranslated ghost the seam example compares against, built
--   with that example's own camera and depth.
designationGhostAt' ∷ Camera2D → Int → BuildingDef → Int → SortableQuad
designationGhostAt' cam effD def z =
    fromMaybe (error "the seam fixture ghost was culled by the camera band") $
        buildingGhostQuad (const 0) noFaceMapVertexId (camFacing cam)
                          (camZSlice cam) effD texSizes tileAlpha
                          designatedGhostAlpha True def
                          (fst anchorTile) (snd anchorTile) z

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

    it "appear and disappear at the SAME camera band as the building" $ do
        -- Requirement 3 under the camera band, not only at one z: a
        -- designation that draws where its own staked building would be
        -- culled produces the blank hand-off this slice forbids — the
        -- ghost is suppressed by the stake, and the stake draws nothing.
        forM_ [zSlice + 1, zSlice - effDepth - 1] $ \z → do
            designationGhostMaybe workDef z `shouldSatisfy` isNothing
            previewGhostAt workDef True z   `shouldSatisfy` isNothing
            stakedGhostAt workDef z         `shouldSatisfy` isNothing
        forM_ [zSlice, zSlice - effDepth] $ \z → do
            designationGhostMaybe workDef z `shouldSatisfy` isJust
            previewGhostAt workDef True z   `shouldSatisfy` isJust
            stakedGhostAt workDef z         `shouldSatisfy` isJust

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

    it "follows the ground the stake will land on, not the level stored\
       \ at designation time" $ \env → do
        -- The elevation-drift case. `cdZ` is captured when the player
        -- clicks; `building.spawn` stamps `biGridZ` from the terrain as
        -- it is at STAKE time. A uniform edit keeps the footprint
        -- placeable and moves only the level, so a ghost drawn from the
        -- stored value would sit where the building is not — and the
        -- hand-off would move it. It draws from
        -- 'Building.Placement.buildingAnchorZ', the very read the spawn
        -- makes, so there is no drift left to jump over.
        ws ← scene env
        plan ws workDef
        (_, before) ← cursorPass env ws
        V.length before `shouldBe` 1
        quadBounds (V.head before)
            `shouldSatisfy` boundsAgree (quadBounds (designationGhostAt workDef 0))
        -- The ground under the site drops three levels; the designation
        -- still says 0.
        raiseTerrain ws (-3)
        designations ← readIORef (wsConstructDesignationsRef ws)
        fmap cdZ (HM.lookup anchorTile designations) `shouldBe` Just 0
        (_, after) ← cursorPass env ws
        V.length after `shouldBe` 1
        quadBounds (V.head after)
            `shouldSatisfy` boundsAgree (quadBounds (designationGhostAt workDef (-3)))
        quadBounds (V.head after)
            `shouldNotSatisfy` boundsAgree (quadBounds (V.head before))
        -- …and the stake that lands at the moved level is still
        -- recognized as THIS designation's, so the pass yields to it
        -- rather than drawing a second 60 % quad beside it.
        bm ← readIORef (buildingManagerRef env)
        writeIORef (buildingManagerRef env) bm
            { bmInstances = HM.singleton (BuildingId 1)
                (instanceOf workDef (fst anchorTile) (snd anchorTile) (-3) 0) }
        (_, staked) ← cursorPass env ws
        V.length staked `shouldBe` 0
        -- The staked instance draws exactly where the designation just
        -- did: the hand-off moved nothing.
        fmap quadBounds (stakedGhostAt workDef (-3))
            `shouldSatisfy` maybe False (boundsAgree (quadBounds (V.head after)))

    it "draws through the nearest u-alias at the seam" $ \env → do
        -- Chunks are stored u-wrapped, so the visibility test answers
        -- through the nearest alias and hands back that alias's screen
        -- offset. Building the quad at the tile's own coordinates and
        -- then DROPPING that offset selects a designation as visible
        -- through one alias and draws it at the other — typically
        -- offscreen. 'translateQuad' moves positions only.
        ws ← scene env
        writeIORef (wsGenParamsRef ws)
            (Just defaultWorldGenParams { wgpWorldSize = seamWorldChunks })
        -- Park the camera a whole world-width away, so the fixture
        -- chunk is only reachable through a non-identity alias.
        cam ← readIORef (rvCameraRef (toRenderViewCapability env))
        let seamCam = cam { camPosition = seamCamPos }
        writeIORef (rvCameraRef (toRenderViewCapability env)) seamCam
        plan ws workDef
        -- PRECONDITION, pinned rather than assumed: this fixture really
        -- is at the seam. Without it a world-size or camera change would
        -- quietly make the whole example an identity comparison.
        (fbW, fbH) ← readIORef (rvFramebufferSizeRef (toRenderViewCapability env))
        let effD = min viewDepth
                     (max 8 (round (camZoom seamCam * 80.0 + 8.0 ∷ Float)))
            vb = computeViewBounds seamCam fbW fbH effD
            (cx, cy) = camPosition seamCam
            offset = isChunkVisibleWrapped (camFacing seamCam) seamWorldChunks
                         vb cx cy (ChunkCoord 0 0)
        offset `shouldSatisfy` maybe False (≢ (0, 0))
        (_, quads) ← cursorPass env ws
        V.length quads `shouldBe` 1
        let drawn = V.head quads
            raw = designationGhostAt' seamCam effD workDef 0
        -- Translated by exactly that offset…
        Just (quadBounds drawn)
            `shouldBe` fmap (\o → quadBounds (translateQuad o raw)) offset
        -- …and demonstrably not left at the canonical position.
        quadBounds drawn `shouldNotBe` quadBounds raw
        -- translateQuad moves positions ONLY: the seam-side ghost keeps
        -- sorting and lighting as the tile it plans on.
        sqSortKey drawn `shouldBe` sqSortKey raw
        quadWorldUV drawn `shouldBe` quadWorldUV raw
        sqTexture drawn `shouldBe` sqTexture raw

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


-- * The staking hand-off (Lua)

-- | Same standalone-Lua-VM pattern as
--   'Test.Headless.Lua.WorkClaimCapacity': one self-contained chunk per
--   example in a fresh interpreter, asserting inside Lua, with a non-OK
--   status surfaced as an hspec failure carrying the Lua message.
runsOk ∷ Text → Expectation
runsOk chunkText = do
    result ← Lua.run @Lua.Exception $ do
        Lua.openlibs
        status ← Lua.dostring (TE.encodeUtf8 chunkText)
        case status of
            Lua.OK → return Nothing
            _ → do
                err ← Lua.tostring (-1)
                return (Just (maybe "<no message>" TE.decodeUtf8Lenient err))
    case result of
        Nothing  → pure ()
        Just msg → expectationFailure (T.unpack msg)

lns ∷ [Text] → Text
lns = T.intercalate "\n"

-- | Everything @unit_ai_construct_site.stakeBuilding@ reaches, stubbed,
--   plus a recorder for the two engine calls whose CHOICE is the whole
--   contract: a completion retires the job, a cancellation discards it,
--   and confusing them either strands a built site or silently loses an
--   unbuilt one.
--
--   @spawnResult@ and @world@ are what each case varies: whether the
--   engine accepts the queued spawn, and which buildings are standing
--   on which page.
stakePrelude ∷ Text
stakePrelude = lns
    [ "package.loaded['scripts.unit_ai_fetch'] = {"
    , "  inventoryCountOf = function() return 0 end,"
    , "  groundCountOf = function() return 0 end,"
    , "  findTechnomule = function() return nil end,"
    , "  loadFeasible = function() return true end }"
    , "package.loaded['scripts.movement_speed'] = {"
    , "  comfort = function() return 1.0 end }"
    , "package.loaded['scripts.unit_ai_core'] = {"
    , "  distance = function(ax, ay, bx, by)"
    , "    return math.sqrt((ax-bx)^2 + (ay-by)^2) end,"
    , "  reportFailure = function(_uid, msg) calls[#calls+1] ="
    , "    { 'failure', msg } end }"
    , "calls = {}"
    , "moved = 0"
    , "unit = { moveTo = function() moved = moved + 1 end,"
    , "         stop = function() end }"
    , "construction = {"
    , "  setJobStatus = function(w, x, y, st, at)"
    , "    calls[#calls+1] = { 'status', x, y, st, at, w } end,"
    -- The page-scoped, exact-attempt pop. The page-BLIND
    -- cancelDesignation is recorded separately so a regression to it is
    -- visible rather than silently equivalent.
    , "  cancelDesignationForRefund = function(w, x, y, at)"
    , "    calls[#calls+1] = { 'cancel', x, y, at, w }; return nil end,"
    , "  cancelDesignation = function(x, y, at)"
    , "    calls[#calls+1] = { 'cancel-pageblind', x, y, at } end }"
    , "engine = { loadYaml = function() return nil end }"
    , "structure = nil"
    -- `world` is the building manager this fixture answers
    -- getActiveIds/getInfo from; `spawnResult` is what the engine says
    -- about the queued spawn.
    , "world = {}"
    , "spawnResult = 1"
    , "spawned = 0"
    , "spawnPage = nil"
    -- A page-less spawn resolves whatever page is ACTIVE, which is not
    -- necessarily this job's. The fixture refuses one outright so a
    -- regression to the three-argument call cannot pass.
    , "building = {"
    , "  spawn = function(_d, _x, _y, page)"
    , "    spawned = spawned + 1"
    , "    spawnPage = page"
    , "    assert(page ~= nil,"
    , "      'a staking spawn must name the job\\'s own page')"
    , "    return spawnResult end,"
    -- Deliberately NOT provided: `getActiveIds` snapshots the ACTIVE
    -- page, and stake observation must not depend on which page that
    -- is. Its absence here is the assertion.
    , "  getInfo = function(bid) return world[bid] end }"
    , "site = require('scripts.unit_ai_construct_site')"
    , "local params = { construct_stake_visible_timeout = 5.0 }"
    , "local function job()"
    , "  return { category = 'building', building = 'hall',"
    , "           x = 4, y = 7, attempt = 11 }"
    , "end"
    , "local function at(gx, gy) return { gridX = gx, gridY = gy } end"
    , "local site = site"
    , "local function stake(j, info, now)"
    , "  return site.stakeBuilding('p1', j, 1, info, now, params)"
    , "end"
    , "local function only(kind)"
    , "  assert(#calls == 1, 'expected exactly one engine call, got '"
    , "    .. #calls)"
    , "  assert(calls[1][1] == kind, 'expected a ' .. kind .. ' call, got '"
    , "    .. calls[1][1])"
    , "  return calls[1]"
    , "end"
    ]

stakeHandoffSpec ∷ Spec
stakeHandoffSpec =
  describe "the staking hand-off holds the designation until the building\
           \ is observable" $ do

    it "walks to the site before staking anything" $ runsOk $ lns
        [ stakePrelude
        , "local j = job()"
        , "assert(stake(j, at(40, 40), 0) == 'working')"
        , "assert(moved == 1, 'a distant worker must walk')"
        , "assert(spawned == 0, 'and must not stake from across the map')"
        , "assert(#calls == 0, 'and must not touch the designation')"
        ]

    it "does NOT complete the designation on the tick it stakes" $ runsOk $ lns
        [ stakePrelude
        -- The whole point: building.spawn returns on QUEUED. Completing
        -- here removes the designation ghost before the building it
        -- planned exists to draw its own, and the site blinks empty.
        , "local j = job()"
        , "assert(stake(j, at(4, 7), 100) == 'working')"
        , "assert(spawned == 1, 'the stake is queued')"
        , "assert(#calls == 0, 'but the job is NOT reported complete yet')"
        , "assert(j.staking == 100, 'the wait records its own clock')"
        ]

    it "keeps waiting while the queued stake is not observable yet" $ runsOk $ lns
        [ stakePrelude
        , "local j = job()"
        , "stake(j, at(4, 7), 100)"
        , "assert(stake(j, at(4, 7), 102) == 'working')"
        , "assert(spawned == 1, 'and never re-stakes while it waits')"
        , "assert(#calls == 0)"
        ]

    it "completes the moment the staked building is observable" $ runsOk $ lns
        [ stakePrelude
        , "local j = job()"
        , "stake(j, at(4, 7), 100)"
        , "assert(j.stakedBid == 1, 'the job holds its own spawned id')"
        , "world[1] = { defName = 'hall', gridX = 4, gridY = 7, page = 'p1' }"
        , "assert(stake(j, at(4, 7), 101) == 'done')"
        , "local c = only('status')"
        , "assert(c[4] == 'complete' and c[2] == 4 and c[3] == 7"
        , "       and c[5] == 11, 'the exact attempt is completed')"
        ]

    it "observes its stake WITHOUT enumerating the active page" $ runsOk $ lns
        [ stakePrelude
        -- `building.getActiveIds` answers for whatever page is selected,
        -- so a job on A could not see its own A-page stake once the
        -- selection moved to B: the wait would expire and cancel a
        -- designation whose building had really landed. The fixture
        -- provides no such verb at all, and the observation still works.
        , "assert(building.getActiveIds == nil,"
        , "  'the fixture deliberately offers no active-page enumeration')"
        , "local j = job()"
        , "stake(j, at(4, 7), 100)"
        , "world[1] = { defName = 'hall', gridX = 4, gridY = 7, page = 'p1' }"
        , "assert(stake(j, at(4, 7), 101) == 'done')"
        , "assert(only('status')[4] == 'complete')"
        ]

    it "does not read a stranger's building as its own stake" $ runsOk $ lns
        [ stakePrelude
        -- Designation admission does not check building occupancy, so a
        -- player can designate over an existing identical building.
        -- Matching page/definition/anchor alone would read that stranger
        -- as this job's work and complete a designation nothing was
        -- built for.
        , "local j = job()"
        , "world[1] = { defName = 'hall', gridX = 4, gridY = 7, page = 'p1' }"
        , "spawnResult = nil"
        , "assert(stake(j, at(4, 7), 100) == 'gone')"
        , "local kinds = {}"
        , "for _, c in ipairs(calls) do kinds[c[1]] = true end"
        , "assert(kinds['cancel'], 'the refused job is cancelled')"
        , "assert(not kinds['status'],"
        , "  'and never completed off a building it did not stake')"
        ]

    it "does not mistake another page's building for this job's stake" $
      runsOk $ lns
        [ stakePrelude
        -- #1673: getActiveIds snapshots whatever page is active, which
        -- is not necessarily the job's own.
        , "local j = job()"
        , "stake(j, at(4, 7), 100)"
        , "world[1] = { defName = 'hall', gridX = 4, gridY = 7,"
        , "             page = 'somewhere_else' }"
        , "assert(stake(j, at(4, 7), 101) == 'working')"
        , "assert(#calls == 0, 'nothing is completed on another page')"
        -- Nor another definition, nor another anchor, on this one.
        , "world[1] = { defName = 'other', gridX = 4, gridY = 7, page = 'p1' }"
        , "assert(stake(j, at(4, 7), 101) == 'working')"
        , "world[1] = { defName = 'hall', gridX = 9, gridY = 9, page = 'p1' }"
        , "assert(stake(j, at(4, 7), 101) == 'working')"
        , "assert(#calls == 0)"
        ]

    it "CANCELS, never completes, when the accepted stake never lands" $
      runsOk $ lns
        [ stakePrelude
        -- The deadline is for a spawn the queue dropped outright. Nothing
        -- was built, so completing would retire a job that produced
        -- nothing and lose the site silently.
        , "local j = job()"
        , "stake(j, at(4, 7), 100)"
        , "assert(stake(j, at(4, 7), 200) == 'gone')"
        , "local kinds = {}"
        , "for _, c in ipairs(calls) do kinds[c[1]] = true end"
        , "assert(kinds['cancel'], 'the unbuilt designation is cancelled')"
        , "assert(not kinds['status'], 'and never reported complete')"
        , "assert(kinds['failure'], 'and the player is told')"
        ]

    it "completes a RESUMED job whose stake already landed before the save" $
      runsOk $ lns
        [ stakePrelude
        -- unit_ai_save strips `staking`, so a job reloaded mid-hand-off
        -- looks like a fresh one — and the load discarded the building
        -- queue its spawn was riding. The building either stands there
        -- (this case) or never will. Re-spawning is refused by the
        -- occupancy check, and that refusal must be read as "already
        -- done", not as "unbuildable".
        , "local j = job()"
        , "j.stakedBid = 1"
        , "assert(j.staking == nil, 'a reloaded job carries no clock')"
        , "world[1] = { defName = 'hall', gridX = 4, gridY = 7, page = 'p1' }"
        , "spawnResult = nil"
        , "assert(stake(j, at(4, 7), 500) == 'done')"
        , "local c = only('status')"
        , "assert(c[4] == 'complete', 'the finished job is completed')"
        ]

    it "scans for work on the job's page, never the active selection" $
      runsOk $ lns
        [ stakePrelude
        -- The whole lifecycle is page-scoped or none of it is: a scan
        -- answered for the selected page, then claimed and completed
        -- against the actor's own, mutates one world's designations
        -- with another world's coordinates.
        , "local seen = nil"
        , "construction.getPendingJobs = function(_a, _b, _c, _d, page)"
        , "  seen = page; return {} end"
        , "site.pendingJobsOn('p1', 1, 2, 3, 4)"
        , "assert(seen == 'p1', 'the scan names the job page, got '"
        , "  .. tostring(seen))"
        -- …and a query that answers nothing is an empty list, not nil,
        -- so the caller's own emptiness check stays the only one.
        , "construction.getPendingJobs = function() return nil end"
        , "local got = site.pendingJobsOn('p1', 1, 2, 3, 4)"
        , "assert(type(got) == 'table' and #got == 0)"
        ]

    it "takes the job's page from the ACTING UNIT, never from the active\
       \ selection" $ runsOk $ lns
        [ stakePrelude
        -- #1673 and the caller half of it: the construct lifecycle spans
        -- many ticks (scan, claim, walk, stake), and reading
        -- world.getActiveWorldId at each of them independently lets a job
        -- scanned on one page be staked and completed on another — the
        -- building lands on the wrong world and the original designation
        -- is never finished. The acting unit's own projection does not
        -- move underneath the job.
        , "unit.getInfo = function(_uid) return { page = 'p1' } end"
        , "world = nil"
        , "_G.world = { getActiveWorldId = function() return 'p2' end }"
        , "assert(site.jobPage(1) == 'p1',"
        , "  'the job page is the unit\\'s, got ' .. tostring(site.jobPage(1)))"
        -- …and it fails CLOSED rather than falling back to the active
        -- page, which is the whole point of not reading that one.
        , "unit.getInfo = function(_uid) return nil end"
        , "assert(site.jobPage(1) == nil,"
        , "  'an actor with no establishable page selects no page at all')"
        ]

    it "spawns, completes and cancels on the JOB's page, never the\
       \ active one" $ runsOk $ lns
        [ stakePrelude
        -- #1673: `building.spawn` without a page resolves whatever page
        -- is active, and active-page selection can move between two Lua
        -- calls in one tick. Every page-scoped step of the hand-off —
        -- the spawn, the completion, the cancellation and the stake
        -- recognition — names 'p1', so a selection change cannot land
        -- the building on one page while the job finishes on another.
        , "local j = job()"
        , "stake(j, at(4, 7), 100)"
        , "assert(spawnPage == 'p1', 'the spawn names the job page, got '"
        , "  .. tostring(spawnPage))"
        , "world[1] = { defName = 'hall', gridX = 4, gridY = 7, page = 'p1' }"
        , "assert(stake(j, at(4, 7), 101) == 'done')"
        , "assert(only('status')[6] == 'p1', 'the completion names it too')"
        -- …and the cancel path takes the page-scoped verb.
        , "calls = {}"
        , "local k = job()"
        , "world = {}"
        , "spawnResult = nil"
        , "assert(stake(k, at(4, 7), 200) == 'gone')"
        , "local seen = {}"
        , "for _, c in ipairs(calls) do seen[c[1]] = c end"
        , "assert(seen['cancel'], 'the page-scoped cancel is used')"
        , "assert(seen['cancel'][5] == 'p1', 'and names the job page')"
        , "assert(not seen['cancel-pageblind'],"
        , "  'the page-blind cancelDesignation must not be reached')"
        ]

    it "still cancels a refused stake when nothing was built" $ runsOk $ lns
        [ stakePrelude
        , "local j = job()"
        , "spawnResult = nil"
        , "assert(stake(j, at(4, 7), 500) == 'gone')"
        , "local kinds = {}"
        , "for _, c in ipairs(calls) do kinds[c[1]] = true end"
        , "assert(kinds['cancel'] and kinds['failure'],"
        , "  'an unbuildable site is cancelled and reported')"
        , "assert(not kinds['status'], 'and never reported complete')"
        ]

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

-- | Move the whole fixture chunk's terrain surface to @z@ — the
--   "uniform edit that keeps the footprint placeable" case.
raiseTerrain ∷ WorldState → Int → IO ()
raiseTerrain ws z = do
    wtd ← readIORef (wsTilesRef ws)
    let bump lc = lc { lcTerrainSurfaceMap =
                         VU.replicate (chunkSize * chunkSize) z }
    writeIORef (wsTilesRef ws) wtd
        { wtdChunks = HM.map bump (wtdChunks wtd) }

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
    stakeHandoffSpec
    aroundAll setup renderSpec
  where
    -- Isolation wraps the boot (#1357): engine init is itself a config
    -- writer.
    setup act = withIsolatedResourceRoot $ do
        EngineInitResult env ← initializeEngineHeadlessQuiet
        act env
