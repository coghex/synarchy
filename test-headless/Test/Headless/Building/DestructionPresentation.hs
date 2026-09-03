{-# LANGUAGE OverloadedStrings #-}
-- | "Building destruction presentation lifecycle" (#2091, BDA-3): a
--   demolition removes the building at once and leaves behind a
--   transient, render-only presentation that plays the declared
--   @destruction@ role once and expires on the game clock.
--
--   Every fixture clip is ASYMMETRIC — a distinct handle for every
--   (facing, frame) pair and a different authored canvas per facing —
--   so a selection that fell back to south, reused a handle, or read
--   another lifecycle's art would fail rather than pass by coincidence.
--   Nothing needs a GPU: the timing, facing and expiry contracts are
--   asserted through the pure 'Building.Destruction' functions and the
--   pure 'destructionToQuad', and the engine-backed groups drive the
--   REAL 'processAllBuildingCommands' drain (the same call
--   "Unit.Thread" makes every tick) on a headless engine whose clock
--   this spec owns — so "paused" is exactly "the clock did not move".
--
--   Run just this gate: @cabal test synarchy-test-headless
--   --test-options='--match "Building destruction presentation lifecycle"'@.
module Test.Headless.Building.DestructionPresentation (spec) where

import UPrelude
import Test.Hspec
import Data.IORef (readIORef, writeIORef)
import Data.List (nub, sort)
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU
import System.Mem.StableName (makeStableName)
import Building.Command.Types (BuildingCommand(..))
import Building.Destruction
import Building.HitTest (hitTestBuildingAt)
import Building.Knowledge
    ( ContainerKnowledge(..), knownContainerIds, observeContainer )
import Building.Placement (PlacementResult(..), canPlaceAt)
import Building.Render
    ( buildingToQuad, destructionToQuad, placedBuildingSortKey
    , renderBuildingQuadsScanned )
import Building.Schema
import Building.Thread.Command (processAllBuildingCommands)
import Building.Types
import Building.Visual
import Engine.Asset.Handle (TextureHandle(..))
import Engine.Core.Capability.Building (toBuildingCapability)
import Engine.Core.Capability.ContentRegistriesView
    (toContentRegistriesViewCapability)
import Engine.Core.Capability.RenderView
    (RenderViewCapability(..), toRenderViewCapability)
import Engine.Core.Capability.WorldSim (toWorldSimCapability)
import qualified Engine.Core.Queue as Q
import Engine.Core.State (EngineEnv(..))
import Engine.Graphics.Camera (Camera2D(..), CameraFacing(..), defaultCamera)
import Engine.Graphics.Vulkan.Types.Vertex
    (Vec2(..), Vec4(..), Vertex(..), WorldUV)
import Engine.Scene.Types (SortableQuad(..))
import Item.Types (ItemInstance(..), emptyItemManager)
import Location.Instance (emptyLocationInstances)
import Power.Network (consumersOn)
import Structure.Types (emptyChunkStructures)
import Test.Headless.Harness (withHeadlessEngineNoWorld)
import Test.Headless.Harness.Isolation (withIsolatedResourceRoot)
import World.Chunk.Types
    (ChunkCoord(..), ColumnTiles(..), LoadedChunk(..), chunkSize)
import World.Flora.Types (emptyFloraChunkData)
import World.Fluid.Types (emptyIceMap)
import World.Grid (tileSideHeight)
import World.Page.Types (WorldPageId(..))
import World.Save.Types
    (BuildingSnapshot(..), fromBuildingSnapshot, toBuildingSnapshot)
import World.State.Types
    (WorldManager(..), WorldState(..), emptyWorldManager, emptyWorldState)
import World.Tile.Types (WorldTileData(..))

-- * Fixture

handle ∷ Int → TextureHandle
handle = TextureHandle

-- | The static view per facing: four distinct handles.
staticHandle ∷ CameraFacing → TextureHandle
staticHandle FaceSouth = handle 1
staticHandle FaceWest  = handle 2
staticHandle FaceNorth = handle 3
staticHandle FaceEast  = handle 4

-- | The authored canvas per facing, applied to that facing's static
--   view AND every frame of every clip — so geometry is asymmetric too.
canvasOf ∷ CameraFacing → (Int, Int)
canvasOf FaceSouth = (128, 96)
canvasOf FaceWest  = (96, 64)
canvasOf FaceNorth = (96, 128)
canvasOf FaceEast  = (160, 64)

facingStep ∷ CameraFacing → Int
facingStep FaceSouth = 0
facingStep FaceWest  = 10
facingStep FaceNorth = 20
facingStep FaceEast  = 30

-- | Construction frames, 4 per facing: the art a fallback would reach
--   for (reversed, or as a substitute) and must never touch.
raiseHandle ∷ CameraFacing → Int → TextureHandle
raiseHandle f i = handle (100 + facingStep f + i)

-- | Destruction frames, 3 per facing: a distinct handle for every
--   (facing, frame) pair, so an assertion names exactly what was
--   selected.
collapseHandle ∷ CameraFacing → Int → TextureHandle
collapseHandle f i = handle (200 + facingStep f + i)

-- | Frames of a `demolish` animation that is DECLARED but mapped to no
--   role — the folder-name temptation nothing may infer a clip from.
demolishHandle ∷ CameraFacing → Int → TextureHandle
demolishHandle f i = handle (300 + facingStep f + i)

collapseFrames ∷ Int
collapseFrames = 3

collapseFps ∷ Float
collapseFps = 2

-- | @collapseFrames / collapseFps@.
collapseDuration ∷ Double
collapseDuration = 1.5

clipOf ∷ (CameraFacing → Int → TextureHandle) → Int → Float → Bool
       → BuildingAnimation
clipOf frameOf n fps loop = BuildingAnimation
    { banFps = fps, banLoop = loop
    , banFrames = canonicalAssets FacingSet
        { fsSouth = frames FaceSouth, fsWest = frames FaceWest
        , fsNorth = frames FaceNorth, fsEast = frames FaceEast } }
  where
    frames f = V.fromList [ frameOf f i | i ← [0 .. n - 1] ]

raiseClip, collapseClip, demolishClip ∷ BuildingAnimation
raiseClip    = clipOf raiseHandle 4 4 False
collapseClip = clipOf collapseHandle collapseFrames collapseFps False
demolishClip = clipOf demolishHandle 4 4 False

texSizes ∷ HM.HashMap TextureHandle (Int, Int)
texSizes = HM.fromList $
    [ (staticHandle f, canvasOf f) | f ← canonicalFacings ]
    ⧺ [ (h f i, canvasOf f)
      | h ← [raiseHandle, collapseHandle, demolishHandle]
      , f ← canonicalFacings, i ← [0 .. 3] ]

baseDef ∷ BuildingDef
baseDef = BuildingDef
    { bdName = "collapse_hall", bdDisplayName = "Collapse Hall"
    , bdCategory = "Test", bdDescription = ""
    , bdTextures = canonicalAssets FacingSet
        { fsSouth = staticHandle FaceSouth, fsWest = staticHandle FaceWest
        , fsNorth = staticHandle FaceNorth, fsEast = staticHandle FaceEast }
    , bdIconTexture = handle 9
    , bdTileW = 2, bdTileH = 1
    , bdPlacement = "flat_ground", bdIsStarting = False, bdRace = "acolyte"
    , bdSpriteAnchor = "tile_bottom", bdBuildWork = 120
    , bdMaterials = HM.singleton "plank" 2, bdStorageCapacity = 50
    , bdOperations = []
    , bdAnimations = HM.fromList [ ("raise", raiseClip)
                                 , ("collapse", collapseClip) ]
    , bdRoleAnims = Map.fromList [ (RoleConstruction, "raise")
                                 , (RoleDestruction, "collapse") ]
    , bdVisualClass = FreestandingInstallation
    , bdPowerDrain = 5, bdPowerNode = Nothing
    }

-- | The building under demolition: worker-built behind a materials
--   gate, a construction clip, a storage capacity, a flat power drain,
--   a @tile_bottom@ anchor, and a declared destruction clip.
demolishDef ∷ BuildingDef
demolishDef = baseDef

-- | Same building with the default sprite anchor: the only difference
--   between its effect's rect and 'demolishDef's is the side-face drop.
diamondDef ∷ BuildingDef
diamondDef = baseDef { bdName = "collapse_diamond"
                     , bdSpriteAnchor = "diamond_bottom" }

-- | Zero-work, timed appearance — a different activity at the moment of
--   demolition, same destruction clip.
appearDef ∷ BuildingDef
appearDef = baseDef
    { bdName = "collapse_timed", bdBuildWork = 0, bdMaterials = HM.empty
    , bdRoleAnims = Map.fromList [ (RoleAppearance, "raise")
                                 , (RoleDestruction, "collapse") ] }

-- | No destruction role, but a construction clip AND an animation named
--   @demolish@ — every fallback a lazy implementation might reach for.
silentDef ∷ BuildingDef
silentDef = baseDef
    { bdName = "silent_hall"
    , bdAnimations = HM.fromList [ ("raise", raiseClip)
                                 , ("demolish", demolishClip) ]
    , bdRoleAnims = Map.singleton RoleConstruction "raise" }

-- | No animations at all.
staticDef ∷ BuildingDef
staticDef = baseDef { bdName = "static_hall"
                    , bdAnimations = HM.empty, bdRoleAnims = Map.empty }

-- | The invalid declarations, each with the destruction role pointing
--   at a clip that breaks exactly one rule.
withCollapse ∷ Text → BuildingAnimation → BuildingDef
withCollapse name clip = baseDef
    { bdName = name
    , bdAnimations = HM.fromList [ ("raise", raiseClip), ("collapse", clip) ] }

loopingDef, zeroFpsDef, negativeFpsDef, nanFpsDef, infFpsDef ∷ BuildingDef
loopingDef     = withCollapse "looping_hall"  collapseClip { banLoop = True }
zeroFpsDef     = withCollapse "zero_fps_hall" collapseClip { banFps = 0 }
negativeFpsDef = withCollapse "neg_fps_hall"  collapseClip { banFps = -4 }
nanFpsDef      = withCollapse "nan_fps_hall"  collapseClip { banFps = 0 / 0 }
infFpsDef      = withCollapse "inf_fps_hall"  collapseClip { banFps = 1 / 0 }

emptyFramesDef ∷ BuildingDef
emptyFramesDef = withCollapse "empty_hall" collapseClip
    { banFrames = canonicalAssets (FacingSet V.empty V.empty V.empty V.empty) }

-- | The role names an animation the definition never declares.
missingAnimDef ∷ BuildingDef
missingAnimDef = baseDef
    { bdName = "missing_hall"
    , bdRoleAnims = Map.fromList [ (RoleConstruction, "raise")
                                 , (RoleDestruction, "vanish") ] }

allDefs ∷ [BuildingDef]
allDefs = [ demolishDef, diamondDef, appearDef, silentDef, staticDef
          , loopingDef, zeroFpsDef, negativeFpsDef, nanFpsDef, infFpsDef
          , emptyFramesDef, missingAnimDef ]

fixturePage, hiddenPage ∷ WorldPageId
fixturePage = WorldPageId "collapse_page"
hiddenPage  = WorldPageId "collapse_hidden"

plank ∷ ItemInstance
plank = ItemInstance
    { iiDefName = "plank", iiCurrentFill = 0, iiQuality = 100
    , iiCondition = 100, iiWeight = 1, iiSharpness = 100, iiContents = []
    , iiInstanceId = 1, iiTemp = Nothing, iiBulk = Just 1, iiStorage = Nothing }

anchor ∷ (Int, Int)
anchor = (3, 1)

-- | An instance of @def@ on @page@ at an off-origin anchor, stamped with
--   the south view the way the drain stamps it.
instanceOn ∷ WorldPageId → BuildingDef → Double → Float → Bool
           → BuildingInstance
instanceOn page def spawnedAt progress delivered = BuildingInstance
    { biDefName = bdName def, biPage = page
    , biTexture = bdSouthTexture def
    , biAnchorX = fst anchor, biAnchorY = snd anchor, biGridZ = 0
    , biSpawnedAt = spawnedAt, biTileW = bdTileW def, biTileH = bdTileH def
    , biSpawnRemaining = 0, biBuildProgress = progress
    , biMaterialsDelivered =
        if delivered then HM.singleton "plank" [plank, plank] else HM.empty
    , biStorage = [plank]
    }

instanceOf ∷ BuildingDef → Double → Float → Bool → BuildingInstance
instanceOf = instanceOn fixturePage

-- | A Built, delivered instance — the ordinary demolition subject.
builtInstance ∷ BuildingDef → BuildingInstance
builtInstance def = instanceOf def 0 120 True

theId ∷ BuildingId
theId = BuildingId 7

startClock ∷ Double
startClock = 10

zSlice ∷ Int
zSlice = 0

effDepth ∷ Int
effDepth = 8

tileAlpha ∷ Float
tileAlpha = 0.8

-- | The effect a demolition of @inst@ at 'startClock' captures, or the
--   example fails naming why.
captured ∷ BuildingInstance → BuildingDef → IO DestructionEffect
captured inst def = case captureDestructionEffect startClock theId inst def of
    Right (Just eff) → pure eff
    other → do
        expectationFailure ("expected a captured effect, got " ⧺ show other)
        pure (error "unreachable")

builtEffect ∷ IO DestructionEffect
builtEffect = captured (builtInstance demolishDef) demolishDef

effectQuad ∷ CameraFacing → Double → DestructionEffect → Maybe SortableQuad
effectQuad facing now eff =
    destructionToQuad (fromIntegral ∘ (\(TextureHandle h) → h)) 0 facing zSlice
                      effDepth tileAlpha eff now texSizes

effectQuadOrFail ∷ CameraFacing → Double → DestructionEffect → IO SortableQuad
effectQuadOrFail facing now eff = case effectQuad facing now eff of
    Just q  → pure q
    Nothing → do
        expectationFailure "destructionToQuad emitted nothing for a live, in-band effect"
        pure (error "unreachable")

placedQuad ∷ CameraFacing → Bool → Double → BuildingInstance → BuildingDef
           → Maybe SortableQuad
placedQuad facing sel now inst def =
    buildingToQuad (fromIntegral ∘ (\(TextureHandle h) → h)) 0 facing zSlice
                   effDepth tileAlpha sel inst (Just def) now texSizes

placedQuadOrFail ∷ CameraFacing → Bool → Double → BuildingInstance → BuildingDef
                 → IO SortableQuad
placedQuadOrFail facing sel now inst def = case placedQuad facing sel now inst def of
    Just q  → pure q
    Nothing → do
        expectationFailure "buildingToQuad culled an in-band instance"
        pure (error "unreachable")

type Bounds = (Float, Float, Float, Float)

quadBounds ∷ SortableQuad → Bounds
quadBounds q =
    let ps = [ p | Vertex { pos = p } ← [sqV0 q, sqV1 q, sqV2 q, sqV3 q] ]
        xs = [ x | Vec2 x _ ← ps ]
        ys = [ y | Vec2 _ y ← ps ]
    in (minimum xs, minimum ys, maximum xs, maximum ys)

bottomOf ∷ SortableQuad → Float
bottomOf q = let (_, _, _, y1) = quadBounds q in y1

centreXOf ∷ SortableQuad → Float
centreXOf q = let (x0, _, x1, _) = quadBounds q in (x0 + x1) * 0.5

quadTint ∷ SortableQuad → Vec4
quadTint q = let Vertex { color = c } = sqV0 q in c

quadAlpha ∷ SortableQuad → Float
quadAlpha q = let Vec4 _ _ _ a = quadTint q in a

quadFlags ∷ SortableQuad → Word32
quadFlags q = let Vertex { renderFlags = f } = sqV0 q in f

quadWorldUV ∷ SortableQuad → WorldUV
quadWorldUV q = let Vertex { worldUV = w } = sqV0 q in w

closeTo ∷ Float → Float → Bool
closeTo a b = abs (a - b) < 1.0e-6

-- | The message an invalid declaration produces, or the example fails
--   with what came back instead.
rejection ∷ BuildingDef → IO Text
rejection def = case resolveDestructionClip def of
    Left msg → pure msg
    other → do
        expectationFailure ("expected a rejection, got " ⧺ show other)
        pure (error "unreachable")

-- * Resolving the declared role

resolveSpec ∷ Spec
resolveSpec = describe "resolving the declared destruction role" $ do
    it "resolves a valid declaration to its own clip and nothing else" $ do
        clip ← case resolveDestructionClip demolishDef of
            Right (Just c) → pure c
            other → do
                expectationFailure ("expected a clip, got " ⧺ show other)
                pure (error "unreachable")
        dcFps clip `shouldBe` collapseFps
        dcFrameCount clip `shouldBe` collapseFrames
        dcFrames clip `shouldBe` banFrames collapseClip
        destructionDuration clip `shouldBe` collapseDuration

    it "a definition with no destruction role has no clip — not a static, reversed-construction or folder-named one" $ do
        resolveDestructionClip silentDef `shouldBe` Right Nothing
        resolveDestructionClip staticDef `shouldBe` Right Nothing
        -- The temptation is real in the fixture: the art exists.
        HM.member "demolish" (bdAnimations silentDef) `shouldBe` True
        Map.member RoleConstruction (bdRoleAnims silentDef) `shouldBe` True

    it "rejects a looping declaration, naming the building and animation" $ do
        msg ← rejection loopingDef
        msg `shouldSatisfy` T.isInfixOf "looping_hall"
        msg `shouldSatisfy` T.isInfixOf "collapse"
        msg `shouldSatisfy` T.isInfixOf "loop"

    it "rejects zero, negative and non-finite fps with the same context" $ do
        forM_ [ (zeroFpsDef, "fps 0.0"), (negativeFpsDef, "fps -4.0")
              , (nanFpsDef, "non-finite"), (infFpsDef, "non-finite") ] $
            \(def, phrase) → do
                msg ← rejection def
                msg `shouldSatisfy` T.isInfixOf (bdName def)
                msg `shouldSatisfy` T.isInfixOf "collapse"
                msg `shouldSatisfy` T.isInfixOf phrase

    it "rejects a role naming an undeclared animation, and a clip with no frames" $ do
        missing ← rejection missingAnimDef
        missing `shouldSatisfy` T.isInfixOf "missing_hall"
        missing `shouldSatisfy` T.isInfixOf "vanish"
        missing `shouldSatisfy` T.isInfixOf "not declared"
        empty ← rejection emptyFramesDef
        empty `shouldSatisfy` T.isInfixOf "empty_hall"
        empty `shouldSatisfy` T.isInfixOf "no frames"

    it "captures the immutable render identity and frame zero at the demolition clock" $ do
        eff ← builtEffect
        deBuildingId eff `shouldBe` theId
        deDefName eff `shouldBe` bdName demolishDef
        dePage eff `shouldBe` fixturePage
        (deAnchorX eff, deAnchorY eff) `shouldBe` anchor
        deGridZ eff `shouldBe` 0
        deAnchorOffset eff `shouldBe` tileSideHeight
        deStartedAt eff `shouldBe` startClock
        -- Frame zero is the DEMOLITION clock, never the spawn clock.
        biSpawnedAt (builtInstance demolishDef) `shouldBe` 0

    it "captures the same effect whatever the building's activity was" $ do
        built    ← captured (builtInstance demolishDef) demolishDef
        ghosted  ← captured (instanceOf demolishDef 0 0 False) demolishDef
        midBuild ← captured (instanceOf demolishDef 0 60 True) demolishDef
        ghosted `shouldBe` built
        midBuild `shouldBe` built
        -- And a timed definition still Appearing at the moment of
        -- demolition: same clip, same start, only the def name differs.
        let justSpawned = instanceOf appearDef startClock 0 False
        appearing ← captured justSpawned appearDef
        currentActivity startClock justSpawned appearDef `shouldBe` Appearing
        deClip appearing `shouldBe` deClip built
        deStartedAt appearing `shouldBe` deStartedAt built

-- * Playback timing

timingSpec ∷ Spec
timingSpec = describe "playback timing on the game clock" $ do
    it "gives every frame of a three-frame clip its full 1/fps interval, then expires" $ do
        eff ← builtEffect
        forM_ [ (0, Just 0), (0.25, Just 0), (0.49, Just 0)
              , (0.5, Just 1), (0.99, Just 1)
              , (1.0, Just 2), (1.25, Just 2), (1.49, Just 2)
              , (1.5, Nothing), (2.0, Nothing), (100, Nothing) ] $
            \(dt, expected) →
                destructionFrameIndex (startClock + dt) eff `shouldBe` expected

    it "never wraps, and clamps only while inside the clip" $ do
        eff ← builtEffect
        -- At the last instant the index is the last frame, not a wrap
        -- to frame 0.
        destructionFrameIndex (startClock + 1.4999) eff `shouldBe` Just 2
        destructionExpired (startClock + 1.4999) eff `shouldBe` False
        destructionExpired (startClock + collapseDuration) eff `shouldBe` True
        -- Where a loop would have produced frame 1 again, nothing plays.
        destructionFrameIndex (startClock + 1.75) eff `shouldBe` Nothing

    it "reads a clock before the start as frame zero, never as expired" $ do
        eff ← builtEffect
        destructionElapsed (startClock - 5) eff `shouldBe` 0
        destructionFrameIndex (startClock - 5) eff `shouldBe` Just 0
        destructionExpired (startClock - 5) eff `shouldBe` False

    it "a frozen clock freezes the phase" $ do
        eff ← builtEffect
        let frozen = startClock + 0.6
        forM_ [1 .. 5 ∷ Int] $ \_ →
            destructionFrameIndex frozen eff `shouldBe` Just 1

    it "expiry is facing-independent: one duration from the clip length" $ do
        -- A hand-built clip whose facings differ in length (the decoder
        -- refuses this; BDA-13 enforces it for shipped art) still has
        -- ONE duration — the longest facing's — so rotating can never
        -- re-time playback.
        let uneven = collapseClip
                { banFrames = canonicalAssets FacingSet
                    { fsSouth = V.fromList [ collapseHandle FaceSouth i | i ← [0 .. 2] ]
                    , fsWest  = V.fromList [ collapseHandle FaceWest 0 ]
                    , fsNorth = V.fromList [ collapseHandle FaceNorth i | i ← [0 .. 1] ]
                    , fsEast  = V.fromList [ collapseHandle FaceEast i | i ← [0 .. 2] ] } }
            def = withCollapse "uneven_hall" uneven
        eff ← captured (builtInstance def) def
        dcFrameCount (deClip eff) `shouldBe` 3
        destructionDuration (deClip eff) `shouldBe` collapseDuration
        let late = startClock + 1.25
        destructionFrameIndex late eff `shouldBe` Just 2
        -- Every facing is still playing at the same instant; the short
        -- ones pin their own last frame at the shared index.
        destructionFrame FaceSouth late eff `shouldBe` Just (collapseHandle FaceSouth 2)
        destructionFrame FaceWest  late eff `shouldBe` Just (collapseHandle FaceWest 0)
        destructionFrame FaceNorth late eff `shouldBe` Just (collapseHandle FaceNorth 1)
        destructionFrame FaceEast  late eff `shouldBe` Just (collapseHandle FaceEast 2)
        forM_ canonicalFacings $ \f →
            destructionFrame f (startClock + collapseDuration) eff `shouldBe` Nothing

-- * Facings

facingSpec ∷ Spec
facingSpec = describe "camera facing selects only the directional handle" $ do
    it "all four facings select distinct handles at the same semantic index" $ do
        eff ← builtEffect
        forM_ [ (0, 0), (0.5, 1), (1.0, 2) ] $ \(dt, idx) → do
            let now = startClock + dt
            forM_ canonicalFacings $ \f →
                destructionFrame f now eff `shouldBe` Just (collapseHandle f idx)
            length (nub [ destructionFrame f now eff | f ← canonicalFacings ])
                `shouldBe` 4

    it "never selects static, construction or folder-named art" $ do
        eff ← builtEffect
        let everyFrame = [ h | dt ← [0, 0.5, 1.0], f ← canonicalFacings
                             , Just h ← [destructionFrame f (startClock + dt) eff] ]
            forbidden = [ staticHandle f | f ← canonicalFacings ]
                      ⧺ [ raiseHandle f i | f ← canonicalFacings, i ← [0 .. 3] ]
                      ⧺ [ demolishHandle f i | f ← canonicalFacings, i ← [0 .. 3] ]
        everyFrame `shouldSatisfy` all (`notElem` forbidden)

    it "rotating during playback changes neither start, index, identity, anchor, z nor sort ownership" $ do
        eff ← builtEffect
        let now = startClock + 0.75
        qs ← mapM (\f → effectQuadOrFail f now eff) canonicalFacings
        length (nub (map sqTexture qs)) `shouldBe` 4
        -- The sort key is the anchor tile's iso depth under the facing
        -- projection, so it legitimately differs per facing — what
        -- rotation must not change is WHOSE key it is: the placed
        -- building's, at every facing.
        forM_ (zip canonicalFacings qs) $ \(f, q) → do
            pq ← placedQuadOrFail f False now (builtInstance demolishDef) demolishDef
            sqSortKey q `shouldBe` sqSortKey pq
        length (nub (map quadWorldUV qs)) `shouldBe` 1
        length (nub (map quadTint qs)) `shouldBe` 1
        length (nub (map quadFlags qs)) `shouldBe` 1
        length (nub (map sqLayer qs)) `shouldBe` 1
        -- The effect itself is immutable under rotation: the facing is
        -- an input to the frame lookup, never to the record.
        forM_ canonicalFacings $ \f → do
            destructionFrameIndex now eff `shouldBe` Just 1
            destructionFrame f now eff `shouldBe` Just (collapseHandle f 1)
        deStartedAt eff `shouldBe` startClock

-- * The quad

quadSpec ∷ Spec
quadSpec = describe "the effect quad through the shared geometry boundary" $ do
    it "keeps the demolished building's ground contact, world UV and sort ownership at every facing" $ do
        eff ← builtEffect
        forM_ canonicalFacings $ \f → do
            q  ← effectQuadOrFail f startClock eff
            pq ← placedQuadOrFail f False startClock (builtInstance demolishDef) demolishDef
            bottomOf q `shouldSatisfy` closeTo (bottomOf pq)
            centreXOf q `shouldSatisfy` closeTo (centreXOf pq)
            quadWorldUV q `shouldBe` quadWorldUV pq
            sqSortKey q `shouldBe` sqSortKey pq
            sqLayer q `shouldBe` sqLayer pq
            -- The frame the effect draws is sized from ITS canvas.
            sqTexture q `shouldBe` collapseHandle f 0

    it "the sort key is exactly the placed building's formula" $ do
        eff ← builtEffect
        forM_ canonicalFacings $ \f → do
            q ← effectQuadOrFail f startClock eff
            let (_, rect) = placedBuildingQuad f startClock zSlice texSizes
                                (builtInstance demolishDef) (Just demolishDef)
            sqSortKey q `shouldBe` placedBuildingSortKey (bqIsoDepth rect) (0 - zSlice)

    it "applies the captured sprite-anchor drop" $ do
        tileBot ← builtEffect
        diamond ← captured (builtInstance diamondDef) diamondDef
        deAnchorOffset diamond `shouldBe` 0
        forM_ canonicalFacings $ \f → do
            a ← effectQuadOrFail f startClock tileBot
            b ← effectQuadOrFail f startClock diamond
            (bottomOf a - bottomOf b) `shouldSatisfy` closeTo tileSideHeight

    it "takes the scene alpha exactly: no ghost opacity, even for a pre-delivery ghost" $ do
        let ghosted = instanceOf demolishDef 0 0 False
        isPreDeliveryGhost ghosted demolishDef `shouldBe` True
        eff ← captured ghosted demolishDef
        forM_ canonicalFacings $ \f → do
            pq ← placedQuadOrFail f True startClock ghosted demolishDef
            quadAlpha pq `shouldSatisfy` closeTo (tileAlpha * 0.6)
            q ← effectQuadOrFail f startClock eff
            quadAlpha q `shouldSatisfy` closeTo tileAlpha
            quadTint q `shouldBe` Vec4 1 1 1 tileAlpha

    it "never sets the selection outline" $ do
        eff ← builtEffect
        forM_ canonicalFacings $ \f → do
            pq ← placedQuadOrFail f True startClock (builtInstance demolishDef) demolishDef
            quadFlags pq `shouldSatisfy` (≢ 0)
            q ← effectQuadOrFail f startClock eff
            quadFlags q `shouldBe` 0

    it "culls by the same z band as a placed building, and emits nothing once expired" $ do
        eff ← builtEffect
        forM_ canonicalFacings $ \f → do
            effectQuad f startClock eff { deGridZ = zSlice + 1 } `shouldSatisfy` isNothing
            effectQuad f startClock eff { deGridZ = zSlice - effDepth - 1 } `shouldSatisfy` isNothing
            effectQuad f startClock eff { deGridZ = zSlice - effDepth } `shouldSatisfy` isJust
            effectQuad f (startClock + collapseDuration) eff `shouldSatisfy` isNothing
            effectQuad f (startClock + 1.49) eff `shouldSatisfy` isJust

-- * Pruning

pruneSpec ∷ Spec
pruneSpec = describe "pruning against the clock" $ do
    it "drops exactly the expired effects" $ do
        early ← builtEffect
        let late = early { deBuildingId = BuildingId 8
                         , deStartedAt = startClock + 0.4 }
            effects = HM.fromList [ (theId, early), (BuildingId 8, late) ]
        HM.keys (pruneExpiredDestructions (startClock + 1.49) effects)
            `shouldMatchList` [theId, BuildingId 8]
        HM.keys (pruneExpiredDestructions (startClock + 1.5) effects)
            `shouldBe` [BuildingId 8]
        HM.keys (pruneExpiredDestructions (startClock + 1.9) effects)
            `shouldBe` []

    it "scopes effects to pages the way instances are scoped" $ do
        eff ← builtEffect
        let elsewhere = eff { deBuildingId = BuildingId 8, dePage = hiddenPage }
            effects = HM.fromList [ (theId, eff), (BuildingId 8, elsewhere) ]
        HM.keys (destructionsOnPages (HS.singleton fixturePage) effects)
            `shouldBe` [theId]
        HM.keys (destructionsOnPages HS.empty effects) `shouldBe` []

-- * The real drain

-- | A flat, fluid-free chunk at (0,0) with a real per-tile column, so
--   'canPlaceAt' can answer the occupancy question.
flatChunk ∷ LoadedChunk
flatChunk =
    let area = chunkSize * chunkSize
        col  = ColumnTiles
            { ctStartZ = 0
            , ctMats   = VU.singleton 1
            , ctSlopes = VU.singleton 0
            , ctVeg    = VU.singleton 0
            }
    in LoadedChunk
        { lcCoord             = ChunkCoord 0 0
        , lcTiles             = V.replicate area col
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

flatTiles ∷ WorldTileData
flatTiles = WorldTileData
    { wtdChunks    = HM.singleton (lcCoord flatChunk) flatChunk
    , wtdMaxChunks = 1
    }

worldSizeChunks ∷ Int
worldSizeChunks = 64

winSize ∷ (Int, Int)
winSize = (800, 800)

-- | Install the scene: two in-memory pages (only 'fixturePage'
--   visible), every fixture def registered, the given instances, the
--   clock at 'startClock', and the render view the hit test reads.
installScene ∷ EngineEnv → CameraFacing → [(BuildingId, BuildingInstance)]
             → Maybe BuildingId → IO (WorldState, WorldState)
installScene env facing insts selected = do
    wsHome ← emptyWorldState
    wsHidden ← emptyWorldState
    writeIORef (wsTilesRef wsHome) flatTiles
    writeIORef (wsTilesRef wsHidden) flatTiles
    writeIORef (worldManagerRef env) emptyWorldManager
        { wmWorlds = [(fixturePage, wsHome), (hiddenPage, wsHidden)]
        , wmVisible = [fixturePage] }
    writeIORef (buildingManagerRef env) emptyBuildingManager
        { bmDefs = HM.fromList [ (bdName d, d) | d ← allDefs ]
        , bmInstances = HM.fromList insts
        , bmSelected = selected
        , bmNextId = 100 }
    writeIORef (gameTimeRef env) startClock
    writeIORef (enginePausedRef env) False
    let rv = toRenderViewCapability env
    writeIORef (rvTextureSizeRef rv) texSizes
    writeIORef (rvWindowSizeRef rv) winSize
    writeIORef (rvCameraRef rv) defaultCamera
        { camZoom = 1, camZSlice = zSlice, camFacing = facing
        , camZTracking = False }
    pure (wsHome, wsHidden)

enqueue ∷ EngineEnv → BuildingCommand → IO ()
enqueue env = Q.writeQueue (buildingQueue env)

-- | Run the REAL building-command drain once — the same call
--   "Unit.Thread" makes every tick.
drain ∷ EngineEnv → IO ()
drain env = processAllBuildingCommands
    (loggerRef env)
    (toWorldSimCapability env)
    (toContentRegistriesViewCapability env)
    (toBuildingCapability env)

setClock ∷ EngineEnv → Double → IO ()
setClock env = writeIORef (gameTimeRef env)

manager ∷ EngineEnv → IO BuildingManager
manager env = readIORef (buildingManagerRef env)

effectsIn ∷ EngineEnv → IO (HM.HashMap BuildingId DestructionEffect)
effectsIn env = bmDestructions ⊚ manager env

-- | Aim the camera at a world point and click the window centre — at
--   800x800 / zoom 1 the centre projects exactly onto the camera.
clickAt ∷ EngineEnv → (Float, Float) → IO (Maybe BuildingId)
clickAt env p = do
    let rv = toRenderViewCapability env
    cam ← readIORef (rvCameraRef rv)
    writeIORef (rvCameraRef rv) cam { camPosition = p }
    let (w, h) = winSize
    hitTestBuildingAt env (fromIntegral w / 2) (fromIntegral h / 2)

-- | The centre of the building's / effect's quad at @facing@.
quadCentre ∷ CameraFacing → IO (Float, Float)
quadCentre facing = do
    let (_, r) = placedBuildingQuad facing startClock zSlice texSizes
                     (builtInstance demolishDef) (Just demolishDef)
    pure (bqX r + bqW r * 0.5, bqY r + bqH r * 0.5)

scannedBuildings ∷ EngineEnv → IO (Int, Int)
scannedBuildings env = do
    -- #1845: the pass takes the FRAME's building-manager snapshot, so
    -- the cursor pass's staking yield and this pass's own quad cannot
    -- be decided against two different worlds. Read live here, which is
    -- what one frame does at its own start.
    bm ← readIORef (buildingManagerRef env)
    (scanned, quads) ← renderBuildingQuadsScanned env bm (const 0) FaceSouth
                                                  zSlice effDepth tileAlpha
    pure (scanned, V.length quads)

drainSpec ∷ Spec
drainSpec = describe "the real destroy drain" $ aroundAll setup $ do
    it "removes the instance and selection at once and captures exactly one effect" $ \env → do
        (wsHome, _) ← installScene env FaceWest [(theId, builtInstance demolishDef)]
                                   (Just theId)
        -- Seed what a live building owns, so its disappearance is
        -- observable: remembered contents, a power draw, occupancy.
        writeIORef (wsContainerKnowledgeRef wsHome) $ ContainerKnowledge
            (HM.singleton theId (observeContainer emptyItemManager startClock [plank]))
        before ← manager env
        HM.keys (consumersOn fixturePage startClock before) `shouldBe` [theId]
        canPlaceAt before flatTiles emptyLocationInstances worldSizeChunks
                   staticDef (fst anchor) (snd anchor)
            `shouldSatisfy` (≢ Placeable)
        centre ← quadCentre FaceWest
        clickAt env centre `shouldReturn` Just theId

        enqueue env (BuildingDestroy theId)
        drain env
        bm ← manager env
        -- The functional boundary: gone from every gameplay surface.
        HM.member theId (bmInstances bm) `shouldBe` False
        bmSelected bm `shouldBe` Nothing
        HM.keys (consumersOn fixturePage startClock bm) `shouldBe` []
        canPlaceAt bm flatTiles emptyLocationInstances worldSizeChunks
                   staticDef (fst anchor) (snd anchor)
            `shouldBe` Placeable
        clickAt env centre `shouldReturn` Nothing
        knowledge ← readIORef (wsContainerKnowledgeRef wsHome)
        knownContainerIds knowledge `shouldBe` []
        -- The presentation: exactly one, keyed by the demolished id,
        -- started at the drain's clock, carrying the render identity.
        HM.keys (bmDestructions bm) `shouldBe` [theId]
        expected ← builtEffect
        HM.lookup theId (bmDestructions bm) `shouldBe` Just expected
        -- The effect owns nothing a live building owned.
        toBuildingSnapshot fixturePage bm `shouldBe`
            BuildingSnapshot { bsnInstances = HM.empty, bsnNextId = 100 }
        -- And it counts in the building pass while the map is empty.
        scannedBuildings env `shouldReturn` (1, 0)

    it "an unknown or repeated destroy creates no effect and cannot restart playback" $ \env → do
        _ ← installScene env FaceSouth [(theId, builtInstance demolishDef)] Nothing
        enqueue env (BuildingDestroy (BuildingId 999))
        drain env
        effectsIn env `shouldReturn` HM.empty
        bm ← manager env
        HM.member theId (bmInstances bm) `shouldBe` True

        enqueue env (BuildingDestroy theId)
        drain env
        first ← effectsIn env
        HM.keys first `shouldBe` [theId]
        -- Later, with the clock advanced, a second destroy of the same
        -- id finds no live instance: the ONE effect keeps its start.
        setClock env (startClock + 0.7)
        enqueue env (BuildingDestroy theId)
        enqueue env (BuildingDestroy (BuildingId 999))
        drain env
        again ← effectsIn env
        again `shouldBe` first
        fmap deStartedAt (HM.lookup theId again) `shouldBe` Just startClock

    it "a definition with no destruction role, or none in the manager, is removed with no visual" $ \env → do
        let orphan = (builtInstance staticDef) { biDefName = "no_such_def" }
        _ ← installScene env FaceSouth
                [ (BuildingId 1, builtInstance silentDef)
                , (BuildingId 2, builtInstance staticDef)
                , (BuildingId 3, orphan) ] (Just (BuildingId 2))
        forM_ [1, 2, 3] $ \i → enqueue env (BuildingDestroy (BuildingId i))
        drain env
        bm ← manager env
        HM.keys (bmInstances bm) `shouldBe` []
        bmSelected bm `shouldBe` Nothing
        bmDestructions bm `shouldBe` HM.empty

    it "an invalid destruction declaration still removes the building, with no visual" $ \env → do
        let subjects = zip [1 ..] [ loopingDef, zeroFpsDef, negativeFpsDef
                                  , nanFpsDef, infFpsDef, emptyFramesDef
                                  , missingAnimDef ]
        _ ← installScene env FaceSouth
                [ (BuildingId i, builtInstance d) | (i, d) ← subjects ] Nothing
        forM_ subjects $ \(i, _) → enqueue env (BuildingDestroy (BuildingId i))
        drain env
        bm ← manager env
        HM.keys (bmInstances bm) `shouldBe` []
        bmDestructions bm `shouldBe` HM.empty

    it "a frozen clock freezes phase and cleanup; the drain prunes at the full duration" $ \env → do
        _ ← installScene env FaceSouth [(theId, builtInstance demolishDef)] Nothing
        enqueue env (BuildingDestroy theId)
        drain env
        -- Paused: the unit tick keeps draining but the clock stands
        -- still, which is all "paused" means to this collection.
        writeIORef (enginePausedRef env) True
        forM_ [1 .. 3 ∷ Int] $ \_ → drain env
        frozen ← effectsIn env
        HM.keys frozen `shouldBe` [theId]
        forM_ (HM.elems frozen) $ \eff →
            destructionFrameIndex startClock eff `shouldBe` Just 0
        -- Unpaused, the clock moves: still inside the clip, still here.
        writeIORef (enginePausedRef env) False
        setClock env (startClock + 1.49)
        drain env
        inside ← effectsIn env
        HM.keys inside `shouldBe` [theId]
        forM_ (HM.elems inside) $ \eff →
            destructionFrameIndex (startClock + 1.49) eff `shouldBe` Just 2
        -- At the full duration the drain prunes it.
        setClock env (startClock + collapseDuration)
        drain env
        effectsIn env `shouldReturn` HM.empty

    it "hidden and culled effects expire without ever being rendered" $ \env → do
        let deep = (instanceOn fixturePage demolishDef 0 120 True)
                       { biGridZ = zSlice - effDepth - 1 }
        _ ← installScene env FaceSouth
                [ (BuildingId 1, instanceOn hiddenPage demolishDef 0 120 True)
                , (BuildingId 2, deep) ] Nothing
        enqueue env (BuildingDestroy (BuildingId 1))
        enqueue env (BuildingDestroy (BuildingId 2))
        drain env
        effects ← effectsIn env
        sort (HM.keys effects) `shouldBe` [BuildingId 1, BuildingId 2]
        -- Neither would produce a quad: one is on a page the render
        -- pass never walks (it filters by visible page before the quad
        -- function sees an effect), the other is outside the camera
        -- band.
        HM.keys (destructionsOnPages (HS.singleton fixturePage) effects)
            `shouldBe` [BuildingId 2]
        forM_ (HM.lookup (BuildingId 2) effects) $ \eff →
            forM_ canonicalFacings $ \f →
                effectQuad f startClock eff `shouldSatisfy` isNothing
        -- The building pass still COUNTS both.
        scannedBuildings env `shouldReturn` (2, 0)
        -- Made visible before expiry, the hidden one resumes at the
        -- clock's phase rather than restarting.
        forM_ (HM.lookup (BuildingId 1) effects) $ \eff → do
            deStartedAt eff `shouldBe` startClock
            destructionFrameIndex (startClock + 1.0) eff `shouldBe` Just 2
        -- Cleanup depends on none of that.
        setClock env (startClock + collapseDuration)
        drain env
        effectsIn env `shouldReturn` HM.empty

    it "an idle tick writes the manager only when something expired" $ \env → do
        _ ← installScene env FaceSouth [(theId, builtInstance demolishDef)] Nothing
        -- No effects: no write.
        idle ← manager env
        drain env
        idleAfter ← manager env
        sameValue ← (≡) <$> makeStableName idle <*> makeStableName idleAfter
        sameValue `shouldBe` True
        -- A live, unexpired effect: no write either.
        enqueue env (BuildingDestroy theId)
        drain env
        live ← manager env
        setClock env (startClock + 0.5)
        drain env
        liveAfter ← manager env
        stillSame ← (≡) <$> makeStableName live <*> makeStableName liveAfter
        stillSame `shouldBe` True
        -- Expiry: the one write, and it removes the effect.
        setClock env (startClock + collapseDuration)
        drain env
        pruned ← manager env
        changed ← (≡) <$> makeStableName live <*> makeStableName pruned
        changed `shouldBe` False
        bmDestructions pruned `shouldBe` HM.empty

    it "BuildingClearAll clears every effect and spawns none" $ \env → do
        _ ← installScene env FaceSouth
                [ (BuildingId 1, builtInstance demolishDef)
                , (BuildingId 2, instanceOn hiddenPage demolishDef 0 120 True) ]
                (Just (BuildingId 1))
        enqueue env (BuildingDestroy (BuildingId 1))
        drain env
        HM.keys ⊚ effectsIn env `shouldReturn` [BuildingId 1]
        enqueue env BuildingClearAll
        drain env
        bm ← manager env
        bmInstances bm `shouldBe` HM.empty
        bmDestructions bm `shouldBe` HM.empty
        bmSelected bm `shouldBe` Nothing

    it "fresh initialization, a mid-playback save and its load carry no effect" $ \env → do
        bmDestructions emptyBuildingManager `shouldBe` HM.empty
        _ ← installScene env FaceSouth
                [ (theId, builtInstance demolishDef)
                , (BuildingId 8, builtInstance staticDef) ] Nothing
        enqueue env (BuildingDestroy theId)
        drain env
        bm ← manager env
        HM.keys (bmDestructions bm) `shouldBe` [theId]
        -- The snapshot holds the survivor and nothing of the demolition.
        let snap = toBuildingSnapshot fixturePage bm
        HM.keys (bsnInstances snap) `shouldBe` [BuildingId 8]
        -- Restoring it produces no residue.
        let (restored, orphans) = fromBuildingSnapshot fixturePage (bmDefs bm) snap
        orphans `shouldBe` []
        HM.keys (bmInstances restored) `shouldBe` [BuildingId 8]
        bmDestructions restored `shouldBe` HM.empty
        bmSelected restored `shouldBe` Nothing
  where
    -- Isolation wraps the boot (#1357): engine init is itself a config
    -- writer.
    setup act = withIsolatedResourceRoot (withHeadlessEngineNoWorld act)

spec ∷ Spec
spec = describe "Building destruction presentation lifecycle (#2091)" $ do
    resolveSpec
    timingSpec
    facingSpec
    quadSpec
    pruneSpec
    drainSpec
