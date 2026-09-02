{-# LANGUAGE OverloadedStrings #-}
-- | BDA-2 (#2088): a building is drawn and hit-tested from the ACTIVE
--   camera facing's own declared view, through one shared boundary
--   ('Building.Visual').
--
--   Every fixture here is ASYMMETRIC — four distinct static handles,
--   four distinct frame lists per lifecycle role, and a different
--   authored canvas size per facing — so a selection that fell back to
--   south, mirrored a view or reused one handle would fail rather than
--   pass by coincidence. Nothing needs a GPU or production art: the
--   quads come from the pure 'buildingToQuad' / 'ghostToQuad', the
--   target rect from 'placedBuildingQuad', and the last group drives
--   the REAL 'hitTestBuildingAt' on a headless engine whose texture
--   size table is the fixture's.
--
--   Run just this gate: @cabal test synarchy-test-headless
--   --test-options='--match "Building camera-facing visuals"'@.
module Test.Headless.Building.CameraFacing (spec) where

import UPrelude
import Test.Hspec
import Data.IORef (writeIORef, atomicModifyIORef')
import Data.List (nub)
import qualified Data.HashMap.Strict as HM
import qualified Data.Map.Strict as Map
import qualified Data.Vector as V
import Building.HitTest (hitTestBuildingAt)
import Building.Render (buildingToQuad, ghostToQuad)
import Building.Schema
import Building.Types
import Building.Visual
import Engine.Asset.Handle (TextureHandle(..))
import Engine.Core.Capability.RenderView
    (RenderViewCapability(..), toRenderViewCapability)
import Engine.Core.State (EngineEnv(..))
import Engine.Graphics.Camera (Camera2D(..), CameraFacing(..), defaultCamera)
import Engine.Graphics.Vulkan.Types.Vertex (Vec2(..), Vec4(..), Vertex(..))
import Engine.Scene.Types (SortableQuad(..))
import Item.Types (ItemInstance(..))
import Test.Headless.Harness (withHeadlessEngineNoWorld)
import Test.Headless.Harness.Isolation (withIsolatedResourceRoot)
import World.Grid (tileSideHeight)
import World.Page.Types (WorldPageId(..))
import World.Save.Types
    (fromBuildingInstanceSnapshot, toBuildingInstanceSnapshot)
import World.State.Types (WorldManager(..), emptyWorldManager, emptyWorldState)

-- * Fixture

handle ∷ Int → TextureHandle
handle = TextureHandle

-- | The static view per facing: four distinct handles, four distinct
--   canvases. South is the LARGEST in one axis and not the other, so
--   at every other facing the stale south bounds contain a pixel the
--   facing's own bounds do not.
staticHandle ∷ CameraFacing → TextureHandle
staticHandle FaceSouth = handle 1
staticHandle FaceWest  = handle 2
staticHandle FaceNorth = handle 3
staticHandle FaceEast  = handle 4

-- | The authored canvas per facing, applied to that facing's static
--   view AND every frame of its animations.
canvasOf ∷ CameraFacing → (Int, Int)
canvasOf FaceSouth = (128, 96)
canvasOf FaceWest  = (96, 64)
canvasOf FaceNorth = (96, 128)
canvasOf FaceEast  = (160, 64)

-- | A same-footprint building whose every view is the base 96x64
--   canvas: the reference the asymmetric one is compared against for
--   everything a canvas change must NOT move.
referenceHandle ∷ CameraFacing → TextureHandle
referenceHandle FaceSouth = handle 5
referenceHandle FaceWest  = handle 6
referenceHandle FaceNorth = handle 7
referenceHandle FaceEast  = handle 8

-- | Frame handles encode (facing, role, index) so an assertion names
--   exactly which frame was picked.
frameBase ∷ BuildingRole → CameraFacing → Int
frameBase role facing = roleBase + facingStep
  where
    roleBase = case role of
        RoleConstruction → 10
        RoleAppearance   → 50
        RoleBuilt        → 90
        RoleDestruction  → 130
    facingStep = case facing of
        FaceSouth → 0
        FaceWest  → 10
        FaceNorth → 20
        FaceEast  → 30

frameHandle ∷ BuildingRole → CameraFacing → Int → TextureHandle
frameHandle role facing i = handle (frameBase role facing + i)

clipFor ∷ BuildingRole → Float → Bool → BuildingAnimation
clipFor role fps loop = BuildingAnimation
    { banFps = fps, banLoop = loop
    , banFrames = canonicalAssets FacingSet
        { fsSouth = frames FaceSouth, fsWest = frames FaceWest
        , fsNorth = frames FaceNorth, fsEast = frames FaceEast } }
  where
    frames f = V.fromList [ frameHandle role f i | i ← [0 .. 3] ]

legacyStatic ∷ TextureHandle
legacyStatic = handle 200

legacyFrame ∷ Int → TextureHandle
legacyFrame i = handle (210 + i)

-- | Every handle above, sized. Frames take their facing's canvas; the
--   reference and legacy handles take the base tile.
texSizes ∷ HM.HashMap TextureHandle (Int, Int)
texSizes = HM.fromList $
    [ (staticHandle f, canvasOf f) | f ← canonicalFacings ]
    ⧺ [ (referenceHandle f, (96, 64)) | f ← canonicalFacings ]
    ⧺ [ (frameHandle role f i, canvasOf f)
      | role ← [RoleConstruction, RoleAppearance, RoleBuilt]
      , f ← canonicalFacings, i ← [0 .. 3] ]
    ⧺ [ (legacyStatic, (96, 96)) ]
    ⧺ [ (legacyFrame i, (96, 96)) | i ← [0 .. 3] ]

baseDef ∷ BuildingDef
baseDef = BuildingDef
    { bdName = "facing_hall", bdDisplayName = "Facing Hall"
    , bdCategory = "Test", bdDescription = ""
    , bdTextures = canonicalAssets FacingSet
        { fsSouth = staticHandle FaceSouth, fsWest = staticHandle FaceWest
        , fsNorth = staticHandle FaceNorth, fsEast = staticHandle FaceEast }
    , bdIconTexture = handle 9
    , bdTileW = 2, bdTileH = 1
    , bdPlacement = "flat_ground", bdIsStarting = False, bdRace = "acolyte"
    , bdSpriteAnchor = "diamond_bottom", bdBuildWork = 0
    , bdMaterials = HM.empty, bdStorageCapacity = 0, bdOperations = []
    , bdAnimations = HM.empty, bdRoleAnims = Map.empty
    , bdVisualClass = FreestandingInstallation
    , bdPowerDrain = 0, bdPowerNode = Nothing
    }

-- | Static only: no role animation ever applies.
staticDef ∷ BuildingDef
staticDef = baseDef

-- | Worker-driven behind a materials gate: 120 s of work, a 4-frame
--   construction clip, two planks before work can start.
workerDef ∷ BuildingDef
workerDef = baseDef
    { bdName = "facing_worker"
    , bdBuildWork = 120
    , bdMaterials = HM.singleton "plank" 2
    , bdRoleAnims = Map.singleton RoleConstruction "raise"
    , bdAnimations = HM.singleton "raise" (clipFor RoleConstruction 4 False)
    }

tileBottomDef ∷ BuildingDef
tileBottomDef = staticDef { bdName = "facing_tile_bottom"
                          , bdSpriteAnchor = "tile_bottom" }

-- | 'tileBottomDef' on the base canvas at every facing (still four
--   distinct handles), so the ONLY difference between its un-dropped
--   rect and its drawn one is the side-face drop.
tileBottomRefDef ∷ BuildingDef
tileBottomRefDef = referenceDef { bdName = "facing_tile_bottom_ref"
                                , bdSpriteAnchor = "tile_bottom" }

-- | Zero-work: a 4-frame appearance at 2 fps (2 game-seconds), then a
--   looping 4-frame built clip at 4 fps.
timedDef ∷ BuildingDef
timedDef = baseDef
    { bdName = "facing_timed"
    , bdRoleAnims = Map.fromList [ (RoleAppearance, "unfold")
                                 , (RoleBuilt, "hum") ]
    , bdAnimations = HM.fromList
        [ ("unfold", clipFor RoleAppearance 2 False)
        , ("hum",    clipFor RoleBuilt 4 True) ]
    }

referenceDef ∷ BuildingDef
referenceDef = baseDef
    { bdName = "facing_reference"
    , bdTextures = canonicalAssets FacingSet
        { fsSouth = referenceHandle FaceSouth, fsWest = referenceHandle FaceWest
        , fsNorth = referenceHandle FaceNorth, fsEast = referenceHandle FaceEast }
    }

-- | #2080's compatibility form: one path through all four views, one
--   frame list through all four.
legacyDef ∷ BuildingDef
legacyDef = baseDef
    { bdName = "facing_legacy"
    , bdTextures = legacyAssets legacyStatic
    , bdBuildWork = 120
    , bdRoleAnims = Map.singleton RoleConstruction "raise"
    , bdAnimations = HM.singleton "raise" BuildingAnimation
        { banFps = 4, banLoop = False
        , banFrames = legacyAssets (V.fromList (map legacyFrame [0 .. 3])) }
    }

fixturePage ∷ WorldPageId
fixturePage = WorldPageId "facing_page"

plank ∷ ItemInstance
plank = ItemInstance
    { iiDefName = "plank", iiCurrentFill = 0, iiQuality = 100
    , iiCondition = 100, iiWeight = 1, iiSharpness = 100, iiContents = []
    , iiInstanceId = 1, iiTemp = Nothing, iiBulk = Just 1, iiStorage = Nothing }

-- | An instance of @def@ at an off-origin anchor (so no rotated term is
--   a zero that would agree whatever the code did), stamped with the
--   south view the way 'Building.Thread.Command' stamps it.
instanceOf ∷ BuildingDef → Double → Float → Bool → BuildingInstance
instanceOf def spawnedAt progress delivered = BuildingInstance
    { biDefName = bdName def, biPage = fixturePage
    , biTexture = bdSouthTexture def
    , biAnchorX = 3, biAnchorY = 1, biGridZ = 0
    , biSpawnedAt = spawnedAt, biTileW = bdTileW def, biTileH = bdTileH def
    , biSpawnRemaining = 0, biBuildProgress = progress
    , biMaterialsDelivered =
        if delivered then HM.singleton "plank" [plank, plank] else HM.empty
    , biStorage = []
    }

zSlice ∷ Int
zSlice = 0

effDepth ∷ Int
effDepth = 8

tileAlpha ∷ Float
tileAlpha = 0.8

renderQuad ∷ CameraFacing → Bool → Double → BuildingInstance → Maybe BuildingDef
           → Maybe SortableQuad
renderQuad facing sel now inst mDef =
    buildingToQuad (fromIntegral ∘ (\(TextureHandle h) → h)) 0 facing zSlice
                   effDepth tileAlpha sel inst mDef now texSizes

renderedOrFail ∷ CameraFacing → Bool → Double → BuildingInstance
               → Maybe BuildingDef → IO SortableQuad
renderedOrFail facing sel now inst mDef =
    case renderQuad facing sel now inst mDef of
        Just q  → pure q
        Nothing → do
            expectationFailure "buildingToQuad culled an in-band instance"
            pure (error "unreachable")

hitRect ∷ CameraFacing → Double → BuildingInstance → Maybe BuildingDef
        → BuildingQuadRect
hitRect facing now inst mDef =
    snd (placedBuildingQuad facing now zSlice texSizes inst mDef)

type Bounds = (Float, Float, Float, Float)

quadBounds ∷ SortableQuad → Bounds
quadBounds q =
    let ps = [ p | Vertex { pos = p } ← [sqV0 q, sqV1 q, sqV2 q, sqV3 q] ]
        xs = [ x | Vec2 x _ ← ps ]
        ys = [ y | Vec2 _ y ← ps ]
    in (minimum xs, minimum ys, maximum xs, maximum ys)

rectBounds ∷ BuildingQuadRect → Bounds
rectBounds r = (bqX r, bqY r, bqX r + bqW r, bqY r + bqH r)

closeTo ∷ Float → Float → Bool
closeTo a b = abs (a - b) < 1.0e-6

boundsAgree ∷ Bounds → Bounds → Bool
boundsAgree (a, b, c, d) (a', b', c', d') =
    closeTo a a' ∧ closeTo b b' ∧ closeTo c c' ∧ closeTo d d'

quadTint ∷ SortableQuad → Vec4
quadTint q = let Vertex { color = c } = sqV0 q in c

quadFlags ∷ SortableQuad → Word32
quadFlags q = let Vertex { renderFlags = f } = sqV0 q in f

inside ∷ BuildingQuadRect → (Float, Float) → Bool
inside r (x, y) =
    x ≥ bqX r ∧ x ≤ bqX r + bqW r ∧ y ≥ bqY r ∧ y ≤ bqY r + bqH r

bottomEdge ∷ BuildingQuadRect → Float
bottomEdge r = bqY r + bqH r

centreX ∷ BuildingQuadRect → Float
centreX r = bqX r + bqW r * 0.5

-- * Selection

selectionSpec ∷ Spec
selectionSpec = describe "static view selection" $ do
    it "each CameraFacing selects its own static handle" $ do
        forM_ canonicalFacings $ \f → do
            pickBuildingFrame f 0 (instanceOf staticDef 0 0 False) staticDef
                `shouldBe` staticHandle f
            previewBuildingTexture f staticDef `shouldBe` staticHandle f
        -- Four DIFFERENT handles: the directional assertion is only
        -- meaningful when nothing collapses.
        length (nub (map (`previewBuildingTexture` staticDef) canonicalFacings))
            `shouldBe` 4

    it "maps the facing directly, never through a stored orientation" $
        -- The instance carries no facing field to compose with, so the
        -- same instance yields every view purely from the camera.
        forM_ canonicalFacings $ \f →
            bvTexture (placedBuildingVisual f 0 (instanceOf staticDef 0 0 False)
                                              (Just staticDef))
                `shouldBe` staticHandle f

-- * Frames

frameSpec ∷ Spec
frameSpec = describe "lifecycle frame selection across facings" $ do
    it "construction picks the same semantic frame index from every direction list" $
        forM_ [ (0, 0), (30, 1), (60, 2), (119, 3), (120, 3) ] $ \(progress, idx) →
            forM_ canonicalFacings $ \f →
                pickBuildingFrame f 0 (instanceOf workerDef 0 progress True) workerDef
                    `shouldBe` frameHandle RoleConstruction f idx

    it "construction ignores the clock at every facing" $
        forM_ canonicalFacings $ \f →
            pickBuildingFrame f 9999 (instanceOf workerDef 0 30 True) workerDef
                `shouldBe` frameHandle RoleConstruction f 1

    it "appearance and the built loop pick the same index from every direction list" $ do
        let inst = instanceOf timedDef 0 0 False
        forM_ [ (0, 0), (0.5, 1), (1.0, 2), (1.9, 3) ] $ \(now, idx) →
            forM_ canonicalFacings $ \f →
                pickBuildingFrame f now inst timedDef
                    `shouldBe` frameHandle RoleAppearance f idx
        -- Built from 2.0 s: the loop at 4 fps wraps every second.
        forM_ [ (2.0, 0), (2.75, 3), (3.0, 0), (3.5, 2) ] $ \(now, idx) →
            forM_ canonicalFacings $ \f →
                pickBuildingFrame f now inst timedDef
                    `shouldBe` frameHandle RoleBuilt f idx

    it "keeps lifecycle phase facing-independent" $ do
        -- currentActivity takes no facing at all; the boundary between
        -- Constructing / Appearing / Built is the same clock and
        -- progress question from every camera.
        currentActivity 1.9 (instanceOf timedDef 0 0 False) timedDef
            `shouldBe` Appearing
        currentActivity 2.0 (instanceOf timedDef 0 0 False) timedDef
            `shouldBe` Built
        forM_ canonicalFacings $ \f → do
            -- The pinned last frame stays the SAME index at every facing
            -- once Built without a built clip.
            pickBuildingFrame f 0 (instanceOf workerDef 0 120 True) workerDef
                `shouldBe` frameHandle RoleConstruction f 3

    it "falls back to the facing's own static view when a direction has no frames" $ do
        -- BDA-13 owns count enforcement, so an empty direction list is
        -- reachable: it shows THAT facing's static sprite, never south's.
        let holed = workerDef
                { bdAnimations = HM.singleton "raise" BuildingAnimation
                    { banFps = 4, banLoop = False
                    , banFrames = canonicalAssets FacingSet
                        { fsSouth = V.fromList [ frameHandle RoleConstruction FaceSouth i | i ← [0 .. 3] ]
                        , fsWest  = V.empty
                        , fsNorth = V.fromList [ frameHandle RoleConstruction FaceNorth i | i ← [0 .. 1] ]
                        , fsEast  = V.fromList [ frameHandle RoleConstruction FaceEast i | i ← [0 .. 3] ]
                        } } }
            inst = instanceOf holed 0 60 True
        pickBuildingFrame FaceWest 0 inst holed `shouldBe` staticHandle FaceWest
        -- A shorter direction derives its index from ITS OWN count at
        -- the same progress fraction: half-way through 2 frames is 1.
        pickBuildingFrame FaceNorth 0 inst holed
            `shouldBe` frameHandle RoleConstruction FaceNorth 1
        pickBuildingFrame FaceSouth 0 inst holed
            `shouldBe` frameHandle RoleConstruction FaceSouth 2

-- * Rotation invariants

rotationSpec ∷ Spec
rotationSpec = describe "rotation changes only the directional handle" $ do
    it "the placed quad differs from a base-canvas reference only in texture and canvas" $
        forM_ canonicalFacings $ \f → do
            let inst = instanceOf workerDef 0 60 True
                ref  = instanceOf referenceDef 0 0 False
            q  ← renderedOrFail f True 0 inst (Just workerDef)
            rq ← renderedOrFail f True 0 ref  (Just referenceDef)
            sqTexture q `shouldBe` frameHandle RoleConstruction f 2
            sqTexture rq `shouldBe` referenceHandle f
            -- Sort key is texture-independent: a differently sized
            -- canvas at the same footprint sorts identically.
            sqSortKey q `shouldBe` sqSortKey rq
            quadTint q `shouldBe` quadTint rq
            quadFlags q `shouldBe` quadFlags rq
            sqLayer q `shouldBe` sqLayer rq
            let Vertex { worldUV = w } = sqV0 q
                Vertex { worldUV = w' } = sqV0 rq
            w `shouldBe` w'

    it "the selection flag and tint are the same at every facing" $ do
        let inst = instanceOf workerDef 0 60 True
        qs ← mapM (\f → renderedOrFail f True 0 inst (Just workerDef)) canonicalFacings
        length (nub (map sqTexture qs)) `shouldBe` 4
        length (nub (map quadFlags qs)) `shouldBe` 1
        length (nub (map quadTint qs)) `shouldBe` 1
        mapM_ (\q → quadFlags q `shouldSatisfy` (≢ 0)) qs

    it "culls identically at every facing" $
        forM_ canonicalFacings $ \f → do
            let above = (instanceOf staticDef 0 0 False) { biGridZ = zSlice + 1 }
                deep  = (instanceOf staticDef 0 0 False) { biGridZ = zSlice - effDepth - 1 }
                edge  = (instanceOf staticDef 0 0 False) { biGridZ = zSlice - effDepth }
            renderQuad f False 0 above (Just staticDef) `shouldSatisfy` isNothing
            renderQuad f False 0 deep  (Just staticDef) `shouldSatisfy` isNothing
            renderQuad f False 0 edge  (Just staticDef) `shouldSatisfy` isJust

-- * Ghosts

ghostSpec ∷ Spec
ghostSpec = describe "ghost paths" $ do
    it "the placement preview uses the facing-selected static handle" $
        forM_ canonicalFacings $ \f → do
            let ghost = BuildingGhost { bgDefName = bdName workerDef
                                      , bgGridX = 3, bgGridY = 1, bgGridZ = 0
                                      , bgValid = True }
                q = ghostToQuad (const 0) 0 f zSlice texSizes tileAlpha
                                ghost workerDef
            sqTexture q `shouldBe` staticHandle f
            quadTint q `shouldBe`
                ghostPieceTint tileAlpha previewGhostAlpha True
            quadTint (ghostToQuad (const 0) 0 f zSlice texSizes tileAlpha
                                  ghost { bgValid = False } workerDef)
                `shouldBe` ghostPieceTint tileAlpha previewGhostAlpha False

    it "the preview sits exactly where the placed building will" $
        forM_ canonicalFacings $ \f → do
            let ghost = BuildingGhost { bgDefName = bdName tileBottomDef
                                      , bgGridX = 3, bgGridY = 1, bgGridZ = 0
                                      , bgValid = True }
                q = ghostToQuad (const 0) 0 f zSlice texSizes tileAlpha
                                ghost tileBottomDef
                placed = instanceOf tileBottomDef 0 0 False
            quadBounds q `shouldSatisfy`
                boundsAgree (rectBounds (hitRect f 0 placed (Just tileBottomDef)))

    it "a placed pre-delivery ghost uses the facing-selected static handle at ghost opacity" $
        forM_ canonicalFacings $ \f → do
            let inst = instanceOf workerDef 0 0 False
            placedBuildingVisual f 0 inst (Just workerDef)
                `shouldBe` BuildingVisual (staticHandle f) True
            q ← renderedOrFail f False 0 inst (Just workerDef)
            sqTexture q `shouldBe` staticHandle f
            let Vec4 _ _ _ a = quadTint q
            a `shouldSatisfy` closeTo (tileAlpha * designatedGhostAlpha)

    it "beginning work hands off to the construction frame for the same facing" $
        forM_ canonicalFacings $ \f → do
            let started = instanceOf workerDef 0 0 True
            placedBuildingVisual f 0 started (Just workerDef)
                `shouldBe` BuildingVisual (frameHandle RoleConstruction f 0) False
            q ← renderedOrFail f False 0 started (Just workerDef)
            let Vec4 _ _ _ a = quadTint q
            a `shouldSatisfy` closeTo tileAlpha
            -- The handoff changed no lifecycle progress.
            biBuildProgress started `shouldBe` 0
            currentActivity 0 started workerDef `shouldBe` Constructing

-- * Geometry

geometrySpec ∷ Spec
geometrySpec = describe "render geometry follows the selected texture" $ do
    it "sizes the quad from the active facing's canvas" $
        forM_ canonicalFacings $ \f → do
            let r   = hitRect f 0 (instanceOf staticDef 0 0 False) (Just staticDef)
                ref = hitRect f 0 (instanceOf referenceDef 0 0 False) (Just referenceDef)
                (cw, ch) = canvasOf f
            bqW r `shouldSatisfy` closeTo (bqW ref * fromIntegral cw / 96)
            bqH r `shouldSatisfy` closeTo (bqH ref * fromIntegral ch / 64)

    it "keeps the footprint ground-contact anchor whatever the canvas" $
        forM_ canonicalFacings $ \f → do
            let r   = hitRect f 0 (instanceOf staticDef 0 0 False) (Just staticDef)
                ref = hitRect f 0 (instanceOf referenceDef 0 0 False) (Just referenceDef)
            bottomEdge r `shouldSatisfy` closeTo (bottomEdge ref)
            centreX r `shouldSatisfy` closeTo (centreX ref)
            bqIsoDepth r `shouldBe` bqIsoDepth ref

    it "tile_bottom drops the quad by one side face at every facing" $
        forM_ canonicalFacings $ \f → do
            let diamond = hitRect f 0 (instanceOf staticDef 0 0 False) (Just staticDef)
                tileBot = hitRect f 0 (instanceOf tileBottomDef 0 0 False) (Just tileBottomDef)
            (bottomEdge tileBot - bottomEdge diamond)
                `shouldSatisfy` closeTo tileSideHeight
            bqW tileBot `shouldBe` bqW diamond
            bqH tileBot `shouldBe` bqH diamond

    it "sizes frames from their own canvas, not the static view's" $
        forM_ canonicalFacings $ \f → do
            -- Same canvas per facing in this fixture, so the frame and
            -- the static view agree — through the frame handle's own
            -- table entry, which the assertion below proves is read.
            let r = hitRect f 0 (instanceOf workerDef 0 60 True) (Just workerDef)
                shrunk = HM.insert (frameHandle RoleConstruction f 2) (48, 32) texSizes
                r' = snd (placedBuildingQuad f 0 zSlice shrunk
                              (instanceOf workerDef 0 60 True) (Just workerDef))
            bqW r' `shouldSatisfy` closeTo (bqW r * 0.5 * 96 / fromIntegral (fst (canvasOf f)))
            bottomEdge r' `shouldSatisfy` closeTo (bottomEdge r)

-- * Render / hit-test agreement

agreementSpec ∷ Spec
agreementSpec = describe "render and hit-test bounds agree" $ do
    let cases =
            [ ("static",             instanceOf staticDef 0 0 False,      Just staticDef)
            , ("tile_bottom",        instanceOf tileBottomDef 0 0 False,  Just tileBottomDef)
            , ("mid-construction",   instanceOf workerDef 0 60 True,      Just workerDef)
            , ("pre-delivery ghost", instanceOf workerDef 0 0 False,      Just workerDef)
            , ("appearing",          instanceOf timedDef 0 0 False,       Just timedDef)
            , ("built loop",         instanceOf timedDef 0 0 False,       Just timedDef)
            , ("def missing",        instanceOf staticDef 0 0 False,      Nothing)
            ]
    forM_ cases $ \(label, inst, mDef) →
        it ("at all four facings — " ⧺ label) $
            forM_ canonicalFacings $ \f → do
                let now = if label ≡ "built loop" then 2.5 else 0
                q ← renderedOrFail f False now inst mDef
                quadBounds q `shouldSatisfy`
                    boundsAgree (rectBounds (hitRect f now inst mDef))
                sqTexture q `shouldBe`
                    bvTexture (placedBuildingVisual f now inst mDef)

    it "a def-less instance keeps its stamped handle, facing-blind, on both sides" $
        forM_ canonicalFacings $ \f → do
            let inst = instanceOf staticDef 0 0 False
            placedBuildingVisual f 0 inst Nothing
                `shouldBe` BuildingVisual (biTexture inst) False
            q ← renderedOrFail f False 0 inst Nothing
            sqTexture q `shouldBe` biTexture inst

    it "a pixel inside only the stale south bounds is outside the visible quad after rotation" $
        forM_ [FaceWest, FaceNorth, FaceEast] $ \f → do
            let inst = instanceOf staticDef 0 0 False
                visible = hitRect f 0 inst (Just staticDef)
                stale = buildingQuadRect f zSlice texSizes 0 (biAnchorX inst)
                            (biAnchorY inst) (biGridZ inst) (biTexture inst)
            case staleOnlyPoint stale visible of
                Nothing → expectationFailure
                    ("fixture gives no stale-only pixel at " ⧺ show f)
                Just p → do
                    inside stale p `shouldBe` True
                    inside visible p `shouldBe` False

-- | A probe pixel inside the stale bounds and outside the visible ones:
--   the stale quad's top strip when it is taller, else its left strip
--   when it is wider.
staleOnlyPoint ∷ BuildingQuadRect → BuildingQuadRect → Maybe (Float, Float)
staleOnlyPoint stale visible =
    let top  = (centreX stale, bqY stale + 0.1 * bqH stale)
        left = (bqX stale + 0.05 * bqW stale, bqY stale + 0.5 * bqH stale)
    in case filter (\p → inside stale p ∧ not (inside visible p)) [top, left] of
        (p:_) → Just p
        []    → Nothing

-- * Legacy

legacySpec ∷ Spec
legacySpec = describe "#2080 legacy assets" $ do
    it "render FaceSouth's output at every facing" $
        forM_ canonicalFacings $ \f → do
            let ghost = instanceOf legacyDef 0 0 False
                mid   = instanceOf legacyDef 0 60 True
                done  = instanceOf legacyDef 0 120 True
            placedBuildingVisual f 0 ghost (Just legacyDef)
                `shouldBe` placedBuildingVisual FaceSouth 0 ghost (Just legacyDef)
            placedBuildingVisual f 0 mid (Just legacyDef)
                `shouldBe` BuildingVisual (legacyFrame 2) False
            placedBuildingVisual f 0 done (Just legacyDef)
                `shouldBe` BuildingVisual (legacyFrame 3) False
            let r  = hitRect f 0 mid (Just legacyDef)
                rs = hitRect FaceSouth 0 mid (Just legacyDef)
            bqW r `shouldBe` bqW rs
            bqH r `shouldBe` bqH rs

-- * Persistence

persistenceSpec ∷ Spec
persistenceSpec = describe "persistence stays facing-blind" $
    it "a reloaded instance selects the same visual at every facing" $ do
        let live = instanceOf workerDef 0 60 True
            back = fromBuildingInstanceSnapshot fixturePage workerDef
                       (toBuildingInstanceSnapshot live)
        forM_ canonicalFacings $ \f →
            placedBuildingVisual f 0 back (Just workerDef)
                `shouldBe` placedBuildingVisual f 0 live (Just workerDef)

-- * The real hit test

-- | 800x800 at zoom 1: the window centre projects EXACTLY onto the
--   camera position, so aiming the camera at a world point and clicking
--   the centre tests that point without restating the projection.
winSize ∷ (Int, Int)
winSize = (800, 800)

installScene ∷ EngineEnv → CameraFacing → [(Word32, BuildingInstance)] → IO ()
installScene env facing insts = do
    ws ← emptyWorldState
    writeIORef (worldManagerRef env) emptyWorldManager
        { wmWorlds = [(fixturePage, ws)], wmVisible = [fixturePage] }
    writeIORef (buildingManagerRef env) emptyBuildingManager
        { bmDefs = HM.fromList [ (bdName d, d)
                               | d ← [ staticDef, workerDef, tileBottomDef
                                     , tileBottomRefDef, timedDef
                                     , referenceDef, legacyDef ] ]
        , bmInstances = HM.fromList [ (BuildingId i, inst) | (i, inst) ← insts ] }
    writeIORef (gameTimeRef env) 0
    let rv = toRenderViewCapability env
    writeIORef (rvTextureSizeRef rv) texSizes
    writeIORef (rvWindowSizeRef rv) winSize
    writeIORef (rvCameraRef rv) defaultCamera
        { camZoom = 1, camZSlice = zSlice, camFacing = facing
        , camZTracking = False }

clickAt ∷ EngineEnv → (Float, Float) → IO (Maybe BuildingId)
clickAt env p = do
    let rv = toRenderViewCapability env
    atomicModifyIORef' (rvCameraRef rv) $ \c → (c { camPosition = p }, ())
    let (w, h) = winSize
    hitTestBuildingAt env (fromIntegral w / 2) (fromIntegral h / 2)

hitTestSpec ∷ Spec
hitTestSpec = describe "hitTestBuildingAt targets the visible quad" $
    aroundAll setup $ do
        it "hits the facing's own bounds and not the stale south bounds" $ \env →
            forM_ [FaceWest, FaceNorth, FaceEast] $ \f → do
                let inst = instanceOf staticDef 0 0 False
                    visible = hitRect f 0 inst (Just staticDef)
                    stale = buildingQuadRect f zSlice texSizes 0 (biAnchorX inst)
                                (biAnchorY inst) (biGridZ inst) (biTexture inst)
                installScene env f [(1, inst)]
                clickAt env (centreX visible, bqY visible + 0.5 * bqH visible)
                    `shouldReturn` Just (BuildingId 1)
                case staleOnlyPoint stale visible of
                    Nothing → expectationFailure "fixture gives no stale-only pixel"
                    Just p  → clickAt env p `shouldReturn` Nothing

        it "applies the tile_bottom drop at every facing" $ \env →
            forM_ canonicalFacings $ \f → do
                let inst = instanceOf tileBottomRefDef 0 0 False
                    visible = hitRect f 0 inst (Just tileBottomRefDef)
                    -- The rect the hit test used to build: this facing's
                    -- projection of the base canvas with NO drop.
                    stale = buildingQuadRect f zSlice texSizes 0 (biAnchorX inst)
                                (biAnchorY inst) (biGridZ inst)
                                (referenceHandle f)
                    -- Just inside the drawn quad's bottom strip, which
                    -- the un-dropped rect does not reach…
                    lowPixel  = (centreX visible, bottomEdge visible - 0.5 * tileSideHeight)
                    -- …and just inside the un-dropped rect's top strip,
                    -- above anything drawn.
                    highPixel = (centreX stale, bqY stale + 0.5 * tileSideHeight)
                inside visible lowPixel `shouldBe` True
                inside stale lowPixel `shouldBe` False
                inside stale highPixel `shouldBe` True
                inside visible highPixel `shouldBe` False
                installScene env f [(1, inst)]
                clickAt env lowPixel `shouldReturn` Just (BuildingId 1)
                clickAt env highPixel `shouldReturn` Nothing

        it "targets the pre-delivery ghost's static view and then the construction frame" $ \env →
            forM_ canonicalFacings $ \f → do
                let ghost   = instanceOf workerDef 0 0 False
                    started = instanceOf workerDef 0 60 True
                    gr = hitRect f 0 ghost (Just workerDef)
                    sr = hitRect f 0 started (Just workerDef)
                installScene env f [(1, ghost)]
                clickAt env (centreX gr, bqY gr + 0.5 * bqH gr)
                    `shouldReturn` Just (BuildingId 1)
                installScene env f [(1, started)]
                clickAt env (centreX sr, bqY sr + 0.5 * bqH sr)
                    `shouldReturn` Just (BuildingId 1)

        it "keeps the policy around the quad: active world, z order, ties, degenerate window" $ \env → do
            let f = FaceWest
                inst = instanceOf staticDef 0 0 False
                r = hitRect f 0 inst (Just staticDef)
                centre = (centreX r, bqY r + 0.5 * bqH r)
            -- Only the active world is clickable.
            installScene env f [(1, inst { biPage = WorldPageId "elsewhere" })]
            clickAt env centre `shouldReturn` Nothing
            -- Highest grid z wins where two quads overlap.
            installScene env f [(1, inst), (2, inst { biGridZ = -1 })]
            clickAt env centre `shouldReturn` Just (BuildingId 1)
            -- Equal z: the closer quad centre wins. North's canvas is
            -- twice the base height on the same footprint, so the two
            -- quads share a bottom edge and their centres sit half the
            -- short height apart; probe inside BOTH, once nearer each.
            let tall  = instanceOf staticDef 0 0 False
                short = instanceOf referenceDef 0 0 False
                tr = hitRect FaceNorth 0 tall (Just staticDef)
                sr = hitRect FaceNorth 0 short (Just referenceDef)
                nearShort = (centreX sr, bottomEdge sr - 0.3 * bqH sr)
                nearTall  = (centreX sr, bottomEdge sr - 0.8 * bqH sr)
            installScene env FaceNorth [(1, tall), (2, short)]
            bqH tr `shouldSatisfy` (> bqH sr)
            forM_ [nearShort, nearTall] $ \p → do
                inside tr p `shouldBe` True
                inside sr p `shouldBe` True
            clickAt env nearShort `shouldReturn` Just (BuildingId 2)
            clickAt env nearTall `shouldReturn` Just (BuildingId 1)
            -- A degenerate window reports no hit.
            installScene env f [(1, inst)]
            writeIORef (rvWindowSizeRef (toRenderViewCapability env)) (0, 0)
            clickAt env centre `shouldReturn` Nothing
  where
    -- Isolation wraps the boot (#1357): engine init is itself a config
    -- writer.
    setup act = withIsolatedResourceRoot (withHeadlessEngineNoWorld act)

spec ∷ Spec
spec = describe "Building camera-facing visuals (#2088)" $ do
    selectionSpec
    frameSpec
    rotationSpec
    ghostSpec
    geometrySpec
    agreementSpec
    legacySpec
    persistenceSpec
    hitTestSpec
