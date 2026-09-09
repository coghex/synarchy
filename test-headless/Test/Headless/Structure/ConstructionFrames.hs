{-# LANGUAGE OverloadedStrings #-}
-- | Structure construction PRESENTATION (#2488): from material payment
--   until the finished piece appears, a paid structure site draws the
--   frame its own progress selects — or, where its appearance declares
--   none, keeps drawing nothing.
--
--   == What "the same as the placed piece" means here
--
--   Almost every example compares a construction quad set against the
--   PLACED-piece producer for the same appearance at the same z, reached
--   through the texture-palette entry point the construction pass never
--   touches ('Structure.Render.structurePieceQuads'). The two must agree
--   on everything except the three things a construction frame is
--   allowed to change — its texture handle, the atlas id that handle
--   bakes to, and the lifecycle render flag. That single comparison
--   covers the final grid z, the slot geometry, #1712's camera rotation,
--   #415's front-wall strips, 'postToQuad'\'s inset, the sort keys and —
--   requirement 4 — the facemap the target appearance resolved, all at
--   once, and none of it is the renderer checked against itself.
--
--   == What is deliberately NOT proven here
--
--   Whether a frame pixel outside the reused facemap's alpha silhouette
--   actually SURVIVES is a fragment-shader question and no quad
--   assertion can answer it; that is
--   @tools\/structure_construction_probe.py@'s offscreen evidence. What
--   this module proves is that the flag which asks for that behaviour is
--   on the quads, and only on them.
--
--   Run just this gate:
--   @cabal test synarchy-test-headless --test-options='--match "structure construction frames"'@
module Test.Headless.Structure.ConstructionFrames (spec) where

import UPrelude
import Test.Hspec
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import qualified Data.List as L
import Data.Maybe (fromJust)
import qualified Data.Serialize as Cereal
import qualified Data.Text as T
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU

import Engine.Asset.Handle (TextureHandle(..), toInt)
import Engine.Graphics.Camera
    (Camera2D(..), CameraFacing(..), defaultCamera)
import Engine.Graphics.Vulkan.Types.Vertex
    (Vertex(..), Vec2(..), renderFlagLifecycleAlpha)
import Engine.Scene.Types (SortableQuad(..))
import Structure.ArtCatalog
import Structure.Facing
    (WallCaps(..), WallEdge(..), screenWallEdge, rotateWallCaps)
import Structure.Palette (emptyTexPalette, internPath)
import Structure.Render (structurePieceQuads)
import Structure.Types
    ( ChunkStructures, StagedStructurePiece(..), StructurePieceData(..)
    , StructureSlot(..), StructureStage(..), StructureStageToken(..)
    , emptyChunkStructures, emptyStructureStage )
import Structure.Wire
    (WireNeighbors(..), WireShape(..), allWireShapes, wireShapeFor
    , wireShapeName)
import World.Chunk.Types
    (ChunkCoord(..), ColumnTiles(..), LoadedChunk(..), chunkSize)
import World.Construct.Attempt (firstConstructAttemptId)
import World.Construct.Receipt (ConstructPayment(..), mkMaterialReceipt)
import World.Construct.Types
    ( ConstructDesignation(..), ConstructDesignations, ConstructTarget(..)
    , StructurePiece(..), newConstructDesignation )
import World.Flora.Types (emptyFloraChunkData)
import World.Fluid.Types (emptyIceMap)
import World.Generate (viewDepth)
import World.Grid (gridToWorld)
import World.Render.StructureGhost
import World.Render.ViewBounds (ViewBounds, computeViewBounds)
import World.Save.Component.PageActivity
    (ConstructDesignationDTO, fromConstructDTO, toConstructDTO)
import World.Tile.Types (WorldTileData(..))
import World.Construct.Plan (PlanWorld(..))

import Test.Headless.Render.StructureGhostFixture (handleForPath)
import Test.Headless.Structure.ConstructionFixture

-- * Fixture geography (the ghost suite's, so the two are comparable)

worldSize, surfaceZ, zSlice ∷ Int
worldSize = 64
surfaceZ  = 10
zSlice    = 14

zoom ∷ Float
zoom = 4.0

fbW, fbH ∷ Int
(fbW, fbH) = (800, 600)

effDepth ∷ Int
effDepth = min viewDepth (max 8 (round (zoom * 80.0 + 8.0 ∷ Float)))

tileAlpha ∷ Float
tileAlpha = 0.8

homeTile ∷ (Int, Int)
homeTile = (4 * chunkSize + 3, 4 * chunkSize + 3)

homeChunkCoord ∷ ChunkCoord
homeChunkCoord = ChunkCoord 4 4

allFacings ∷ [CameraFacing]
allFacings = [FaceSouth, FaceWest, FaceNorth, FaceEast]

-- | The grid z each kind's piece lands at, restated from
--   @scripts\/structures.lua@ rather than read out of the resolver.
floorGridZ ∷ Int
floorGridZ = surfaceZ + 1

-- * Spec

spec ∷ Spec
spec = describe "structure construction frames" $ do
    indexSpec
    appearanceSpec
    refusalSpec
    renderSpec
    rotationSpec
    handoffSpec
    absenceSpec
    roundTripSpec

-- * The progress → frame convention

indexSpec ∷ Spec
indexSpec = describe "the progress index" $ do
    it "uses the building convention: floor (progress * n), clamped" $
        map (constructionFrameIndex 4) [0, 0.24, 0.25, 0.49, 0.5, 0.99, 1.0]
            `shouldBe` [0, 0, 1, 1, 2, 3, 3]

    it "selects the LAST frame at progress 1.0, never one past it" $
        forM_ [1 .. 8 ∷ Int] $ \n →
            constructionFrameIndex n 1.0 `shouldBe` n - 1

    it "clamps a progress outside 0..1 rather than indexing outside the \
       \sequence" $
        map (constructionFrameIndex 5) [-3.0, -0.001, 1.5, 99.0]
            `shouldBe` [0, 0, 4, 4]

    it "selects the first frame for a progress that is not a number" $
        constructionFrameIndex 5 (0 / 0) `shouldBe` 0

    it "walks every frame of a sequence as progress rises" $ do
        let cs = fromJust (sequenceFor (AppearanceKey Nothing ApCeiling))
            n  = V.length (csFrames cs)
        n `shouldBe` 5
        L.nub [ aaPath (constructionFrameAt p cs)
              | p ← [0, 0.2, 0.4, 0.6, 0.8, 1.0] ]
            `shouldBe` map aaPath (V.toList (csFrames cs))

-- * Which sequence an appearance resolves

appearanceSpec ∷ Spec
appearanceSpec = describe "an appearance" $ do
    it "resolves its OWN frames and no other appearance's" $
        forM_ declaredDefaults $ \ak → do
            let cs = resolveConstructionSequence fixtureCatalog
                         (packOf ak) ak
            fmap (map aaPath ∘ V.toList ∘ csFrames) cs
                `shouldBe` Just (framePathsFor ak)

    it "resolves nothing when it declares nothing" $
        forM_ [ AppearanceKey Nothing (ApWire w)
              | w ← allWireShapes, w `notElem` framedWireShapes ] $ \ak →
            resolveConstructionSequence fixtureCatalog fixtureWirePack ak
                `shouldBe` Nothing

    it "keeps a variant's sequence separate from the default's" $ do
        let dmg = AppearanceKey (Just damagedVariant) ApFloor
            def = AppearanceKey Nothing ApFloor
            paths k = fmap (map aaPath ∘ V.toList ∘ csFrames)
                          (resolveConstructionSequence fixtureCatalog
                               fixturePack k)
        paths dmg `shouldBe` Just (framePathsFor dmg)
        paths def `shouldBe` Just (framePathsFor def)
        paths dmg `shouldNotBe` paths def

    it "gives a variant that declares none NOTHING, never the default's" $
        forM_ [ApCeiling, ApPost, ApWall WallNW, ApWall WallSE] $ \slot → do
            let dmg = AppearanceKey (Just damagedVariant) slot
            resolveConstructionSequence fixtureCatalog fixturePack dmg
                `shouldBe` Nothing
            -- …while the DEFAULT appearance of the same slot does
            -- declare one, so this is absence and not an empty pack.
            resolveConstructionSequence fixtureCatalog fixturePack
                (AppearanceKey Nothing slot)
                `shouldNotBe` Nothing

    it "keeps the four DEFAULT wall directions distinct but equally long" $ do
        let seqs = [ fromJust (resolveConstructionSequence fixtureCatalog
                                   fixturePack (AppearanceKey Nothing
                                                    (ApWall e)))
                   | e ← allWallEdges ]
            paths = map (map aaPath ∘ V.toList ∘ csFrames) seqs
        L.nub (map length paths) `shouldBe` [4]
        length (L.nub (concat paths)) `shouldBe` 16

    it "resolves nothing for an unregistered pack" $
        resolveConstructionSequence fixtureCatalog "no_such_pack"
            (AppearanceKey Nothing ApFloor) `shouldBe` Nothing

    it "resolves nothing once the pack's art has terminally failed — and \
       \leaves the OTHER pack alone" $ do
        let (failed, report) = failPackArtPath "fx/floor.png" "missing"
                                   fixtureCatalog
        afrTracked report `shouldBe` True
        forM_ [ ak | ak ← declaredDefaults, packOf ak ≡ fixturePack ] $ \ak →
            resolveConstructionSequence failed fixturePack ak
                `shouldBe` Nothing
        forM_ [ ak | ak ← declaredDefaults, packOf ak ≡ fixtureWirePack ] $ \ak →
            resolveConstructionSequence failed fixtureWirePack ak
                `shouldNotBe` Nothing

    it "fails the whole pack when a declared FRAME fails to load, naming \
       \the appearance and the frame position" $ do
        let framePath = framePathsFor (AppearanceKey Nothing ApCeiling) !! 3
            (failed, report) = failPackArtPath framePath "decode error"
                                   fixtureCatalog
        afrTracked report `shouldBe` True
        let message = artAssetFailureMessage (fromJust (afrFailure report))
        message `shouldSatisfy` T.isInfixOf fixturePack
        message `shouldSatisfy` T.isInfixOf "ceiling construction frame 4"
        -- One frame down takes the WHOLE pack with it, static art
        -- included — the same all-or-nothing rule a failed sprite is
        -- under.
        resolveConstructionSequence failed fixturePack
            (AppearanceKey Nothing ApFloor) `shouldBe` Nothing
        packArtResolves failed fixturePack `shouldBe` False

    it "fails the whole pack when a VARIANT's own static sprite fails, \
       \even though the catalogue stores no art for it" $ do
        -- A variant's sprite is carried ONLY by its construction
        -- sequence — the catalogue stores default art — so it is
        -- reachable through nothing else. Before this it matched neither
        -- the art scan nor the frame scan, and the pack went on
        -- resolving everything.
        let ak = AppearanceKey (Just damagedVariant) ApFloor
            path = staticPathFor ak
            (failed, report) = failPackArtPath path "missing" fixtureCatalog
        -- The fixture really does keep this path out of the static art…
        map (aaPath ∘ paTexture ∘ snd) (parEntries fixtureRegistration)
            `shouldNotSatisfy` elem path
        afrTracked report `shouldBe` True
        let message = artAssetFailureMessage (fromJust (afrFailure report))
        message `shouldSatisfy` T.isInfixOf fixturePack
        message `shouldSatisfy`
            T.isInfixOf "variant 'damaged' floor construction static sprite"
        packArtResolves failed fixturePack `shouldBe` False
        resolveConstructionSequence failed fixturePack
            (AppearanceKey Nothing ApFloor) `shouldBe` Nothing
        resolveConstructionSequence failed fixturePack ak `shouldBe` Nothing

    it "still names the STATIC ART slot when a default appearance's \
       \sprite fails, not its sequence" $ do
        -- The default half is reachable both ways, and the art scan
        -- must keep answering for it — the familiar diagnostic is
        -- unchanged for every pack that declares no frames at all.
        let (_, report) = failPackArtPath "fx/floor.png" "missing"
                              fixtureCatalog
            message = artAssetFailureMessage (fromJust (afrFailure report))
        message `shouldSatisfy` T.isInfixOf "asset 'floor texture'"
        message `shouldNotSatisfy` T.isInfixOf "construction static sprite"

    it "reports each failed frame once and stays silent on a repeat" $ do
        let framePath = firstOf (framePathsFor (AppearanceKey Nothing ApPost))
            (once, r1) = failPackArtPath framePath "gone" fixtureCatalog
            (_,    r2) = failPackArtPath framePath "gone" once
        afrFailure r1 `shouldSatisfy` isJust
        afrFailure r2 `shouldBe` Nothing

    it "lists exactly the DEFAULT appearances that declare nothing" $ do
        let stored = fromJust (HM.lookup fixtureWirePack
                                  (sacPacks fixtureCatalog))
        undeclaredConstructionAppearances stored `shouldMatchList`
            [ AppearanceKey Nothing (ApWire w)
            | w ← allWireShapes, w `notElem` framedWireShapes ]

    it "names the pack and the appearance in the once-per-appearance \
       \report" $ do
        let ak  = AppearanceKey Nothing (ApWire WireTeeN)
            msg = missingConstructionMessage fixtureWirePack ak
        msg `shouldSatisfy` T.isInfixOf fixtureWirePack
        msg `shouldSatisfy` T.isInfixOf ("wire connection "
                                           <> wireShapeName WireTeeN)

-- | Every DEFAULT appearance the fixture declares a sequence for.
declaredDefaults ∷ [AppearanceKey]
declaredDefaults =
    [ AppearanceKey Nothing s
    | s ← [ApFloor, ApCeiling, ApPost] ⧺ map ApWall allWallEdges
            ⧺ map ApWire framedWireShapes ]

packOf ∷ AppearanceKey → Text
packOf ak = case apSlot ak of
    ApWire _ → fixtureWirePack
    _        → fixturePack

-- * Registration refusals

refusalSpec ∷ Spec
refusalSpec = describe "registration" $ do
    it "accepts the fixture pack whole" $
        outcomeOf fixtureRegistration `shouldBe` ArtRegistered

    it "refuses an EMPTY frame list, naming the pack and the appearance" $
        refusal (AppearanceKey Nothing ApFloor)
            (Just (ConstructionSequence (staticAsset ApFloor) V.empty))
            `shouldSatisfy` faultSays ["fixture_dungeon", "floor", "empty"]

    it "refuses a list that names the same image twice" $ do
        let p = firstOf (framePathsFor (AppearanceKey Nothing ApFloor))
        refusal (AppearanceKey Nothing ApFloor)
            (Just (ConstructionSequence (staticAsset ApFloor)
                       (V.fromList [pathAsset p, pathAsset p])))
            `shouldSatisfy` faultSays ["fixture_dungeon", "floor", "more than once"]

    it "refuses a frame path that escapes the resource root" $
        forM_ [ "../secrets/floor.png", "/etc/floor.png"
              , "fx/../../floor.png", "C:/art/floor.png"
              , "assets\\floor.png", "~/floor.png" ] $ \bad →
            refusal (AppearanceKey Nothing ApFloor)
                (Just (ConstructionSequence (staticAsset ApFloor)
                           (V.fromList [pathAsset bad])))
                `shouldSatisfy` faultSays ["fixture_dungeon", "escapes"]

    it "accepts an ordinary nested relative path" $
        outcomeOf (withSequence (AppearanceKey Nothing ApFloor)
            (Just (ConstructionSequence (staticAsset ApFloor)
                       (V.fromList [pathAsset "fx/build/floor_0.png"])))
            fixtureRegistration) `shouldBe` ArtRegistered

    it "refuses a frame whose handle was never loaded" $
        refusal (AppearanceKey Nothing ApFloor)
            (Just (ConstructionSequence (staticAsset ApFloor)
                       (V.fromList [ ArtAsset "fx/floor_build_0.png"
                                         (TextureHandle 0) ])))
            `shouldSatisfy` faultSays ["fixture_dungeon", "not a loaded handle"]

    it "refuses a LAST frame whose canvas differs from the static sprite" $ do
        let ak  = AppearanceKey Nothing ApFloor
            cs  = fromJust (sequenceFor ak)
            reg = (withSequence ak (Just cs) fixtureRegistration)
                    { parSizes = HM.insert (aaPath (V.last (csFrames cs)))
                                     (96, 96) fixtureSizesOf }
        outcomeOf reg `shouldSatisfy`
            faultSays ["fixture_dungeon", "96x96", "96x64"]

    it "accepts a NON-last frame whose canvas differs — only the handoff \
       \frame has to match" $ do
        let ak  = AppearanceKey Nothing ApFloor
            cs  = fromJust (sequenceFor ak)
            reg = (withSequence ak (Just cs) fixtureRegistration)
                    { parSizes = HM.insert (aaPath (V.head (csFrames cs)))
                                     (48, 32) fixtureSizesOf }
        outcomeOf reg `shouldBe` ArtRegistered

    it "refuses a sequence whose static sprite could not be measured" $ do
        let ak  = AppearanceKey Nothing ApFloor
            reg = fixtureRegistration
                    { parSizes = HM.delete (staticPathFor ak) fixtureSizesOf }
        outcomeOf reg `shouldSatisfy`
            faultSays ["fixture_dungeon", "could not be measured"]

    it "refuses a DEFAULT sequence that hands off to a sprite the pack \
       \does not declare for that appearance" $
        refusal (AppearanceKey Nothing ApFloor)
            (Just (ConstructionSequence (pathAsset "fx/ceiling.png")
                       (V.fromList (map pathAsset
                           (framePathsFor (AppearanceKey Nothing ApFloor))))))
            `shouldSatisfy` faultSays ["fixture_dungeon", "does not declare"]

    it "refuses frames for a kind the registration never declared" $ do
        let reg = fixtureRegistration
                    { parKinds  = [ e | e@(k, _, _) ← parKinds fixtureRegistration
                                      , k ≢ KCeiling ]
                    , parEntries = [ e | e@(k, _) ← parEntries fixtureRegistration
                                       , k ≢ AkCeiling ] }
        outcomeOf reg `shouldSatisfy`
            faultSays ["fixture_dungeon", "does not declare"]

    it "refuses a wall family whose declared directions run to different \
       \lengths" $ do
        let ak = AppearanceKey Nothing (ApWall WallNE)
            cs = fromJust (sequenceFor ak)
            short = cs { csFrames = V.take 2 (csFrames cs) }
        refusal ak (Just short) `shouldSatisfy`
            faultSays ["fixture_dungeon", "different lengths", "ne: 2"]

    it "accepts a wall family that declares only SOME directions, as long \
       \as those agree" $ do
        let reg = foldr (\e r → withSequence (AppearanceKey Nothing (ApWall e))
                                    Nothing r)
                        fixtureRegistration [WallNW, WallSE]
        outcomeOf reg `shouldBe` ArtRegistered

    it "keeps a VARIANT family's lengths independent of the default's" $ do
        -- The default walls run to 4 and `damaged` ne to 3; both stand.
        let dmg = AppearanceKey (Just damagedVariant) (ApWall WallNE)
        fmap (V.length ∘ csFrames)
             (resolveConstructionSequence fixtureCatalog fixturePack dmg)
            `shouldBe` Just 3

    it "is idempotent for an identical repeat" $
        snd (registerPackArt fixtureRegistration fixtureCatalog)
            `shouldBe` ArtAlreadyRegistered

    it "refuses a repeat that differs only in its construction frames, and \
       \keeps the stored pack" $ do
        let ak  = AppearanceKey Nothing ApFloor
            cs  = fromJust (sequenceFor ak)
            reg = withSequence ak
                      (Just cs { csFrames = V.take 2 (csFrames cs) })
                      fixtureRegistration
            (after, outcome) = registerPackArt reg fixtureCatalog
        outcome `shouldSatisfy`
            faultSays ["fixture_dungeon", "construction frames"]
        -- Unchanged: the stored declaration wins.
        fmap (V.length ∘ csFrames)
             (resolveConstructionSequence after fixturePack ak)
            `shouldBe` Just 3

    it "refuses the same appearance declared twice" $ do
        let ak  = AppearanceKey Nothing ApFloor
            cs  = fromJust (sequenceFor ak)
            reg = fixtureRegistration
                    { parFrames = (ak, cs) : parFrames fixtureRegistration }
        outcomeOf reg `shouldSatisfy`
            faultSays ["fixture_dungeon", "more than once"]

fixtureSizesOf ∷ HM.HashMap Text (Int, Int)
fixtureSizesOf = parSizes fixtureRegistration

staticAsset ∷ AppearanceSlot → ArtAsset
staticAsset slot = pathAsset (staticPathFor (AppearanceKey Nothing slot))

pathAsset ∷ Text → ArtAsset
pathAsset p = ArtAsset p (handleForPath p)

outcomeOf ∷ PackArtRegistration → RegistrationOutcome
outcomeOf reg = snd (registerPackArt reg emptyStructureArtCatalog)

refusal ∷ AppearanceKey → Maybe ConstructionSequence → RegistrationOutcome
refusal ak mcs = outcomeOf (withSequence ak mcs fixtureRegistration)

-- | Does this outcome refuse, with a message naming every one of these?
faultSays ∷ [Text] → RegistrationOutcome → Bool
faultSays needles outcome = case outcome of
    ArtRegistrationRefused f →
        all (`T.isInfixOf` artFaultMessage f) needles
    _ → False

-- * What the construction pass draws

renderSpec ∷ Spec
renderSpec = describe "a paid designation" $ do
    forM_ everyKind $ \(label, sp, slot, gridZ, committed) →
        it (label ⧺ " draws its own frame, at the placed piece's geometry \
                    \and facemap, flagged") $
            forM_ [0.0, 0.4, 1.0] $ \progress → do
                let ge = envFor FaceSouth (paidAt sp progress) committed
                             emptyStructureStage
                    got = snd (structureConstructionGhosts ge)
                    frame = expectedFrame ge sp progress
                map sqTexture (V.toList got)
                    `shouldBe` replicate (V.length got) (aaHandle frame)
                lifecycleShapes got `shouldBe`
                    lifecycleShapes (V.fromList (placedQuads FaceSouth slot
                                                    sp gridZ))
                allFlags got `shouldBe` [renderFlagLifecycleAlpha]

    it "carries the TARGET appearance's facemap, which the cap state \
       \changes and the frame does not" $
        forM_ [([], WallCaps False False), ([SPostN, SPostE], WallCaps True True)]
            $ \(posts, caps) → do
                let sp = wallPiece WallNE
                    ge = envFor FaceSouth (paidAt sp 0.5)
                             (structuresAt (SFloor : posts)) emptyStructureStage
                    got = snd (structureConstructionGhosts ge)
                    face = handleForPath ("fx/wallface_ne_"
                                            <> capsCode caps <> ".png")
                got `shouldNotSatisfy` V.null
                L.nub (map faceMapId (concatMap quadVerts (V.toList got)))
                    `shouldBe` [fromIntegral (toInt face)]
                -- …while the frame itself is the edge's, cap-independent.
                L.nub (map sqTexture (V.toList got)) `shouldBe`
                    [aaHandle (expectedFrame ge sp 0.5)]

    it "slices a SCREEN-front wall into the same depth strips a placed one \
       \gets" $ do
        let sp = wallPiece WallSE
            ge = envFor FaceSouth (paidAt sp 0.5) homeStructures
                     emptyStructureStage
            got = snd (structureConstructionGhosts ge)
        V.length got `shouldBe` 16
        lifecycleShapes got `shouldBe`
            lifecycleShapes (V.fromList (placedQuads FaceSouth SWallSE sp
                                             floorGridZ))
        allFlags got `shouldBe` [renderFlagLifecycleAlpha]

    it "leaves the unpaid designated ghost untouched — unflagged, and the \
       \static sprite" $ do
        let sp = wallPiece WallNE
            ge = envFor FaceSouth (unpaidAt sp) homeStructures
                     emptyStructureStage
            ghost = snd (structureDesignationGhosts ge)
        ghost `shouldNotSatisfy` V.null
        allFlags ghost `shouldBe` [0]
        L.nub (map sqTexture (V.toList ghost)) `shouldBe`
            [handleForPath "fx/wall_ne.png"]
        -- …and the construction pass ignores it, because it is unpaid.
        snd (structureConstructionGhosts ge) `shouldSatisfy` V.null

    it "draws NOTHING for an unpaid designation and the designated ghost \
       \draws nothing for a paid one — the two passes are disjoint" $ do
        let sp = wallPiece WallNE
            paidEnv = envFor FaceSouth (paidAt sp 0.5) homeStructures
                          emptyStructureStage
        snd (structureDesignationGhosts paidEnv) `shouldSatisfy` V.null
        snd (structureConstructionGhosts paidEnv) `shouldNotSatisfy` V.null

-- * Rotation

rotationSpec ∷ Spec
rotationSpec = describe "a wall under construction" $ do
    it "shows the SCREEN edge's own frames as the camera turns" $
        forM_ allWallEdges $ \authored →
            forM_ allFacings $ \facing → do
                let sp = wallPiece authored
                    ge = envFor facing (paidAt sp 0.5) homeStructures
                             emptyStructureStage
                    screen = screenWallEdge facing authored
                    want = framePathsFor (AppearanceKey Nothing (ApWall screen))
                             !! 2
                L.nub (map sqTexture
                          (V.toList (snd (structureConstructionGhosts ge))))
                    `shouldBe` [handleForPath want]

    it "selects the SAME progress stage at all four facings" $
        forM_ allWallEdges $ \authored →
            forM_ [0.0, 0.3, 0.6, 1.0] $ \progress → do
                let indexAt facing =
                        let sp = wallPiece authored
                            ge = envFor facing (paidAt sp progress)
                                     homeStructures emptyStructureStage
                            screen = screenWallEdge facing authored
                            paths = framePathsFor
                                        (AppearanceKey Nothing (ApWall screen))
                            drawn = L.nub (map sqTexture (V.toList
                                        (snd (structureConstructionGhosts ge))))
                        in [ i | (i, p) ← zip [0 ∷ Int ..] paths
                               , [handleForPath p] ≡ drawn ]
                L.nub (map indexAt allFacings) `shouldBe`
                    [[constructionFrameIndex 4 progress]]

    it "pairs the rotated cap facemap with the rotated frame, never one \
       \direction's mask with another's art" $
        forM_ allFacings $ \facing → do
            let authored = WallNE
                sp = wallPiece authored
                ge = envFor facing (paidAt sp 0.5)
                         (structuresAt [SFloor, SPostN]) emptyStructureStage
                got = snd (structureConstructionGhosts ge)
                caps = WallCaps True False
                screen = screenWallEdge facing authored
                wantFace = handleForPath
                    ("fx/wallface_" <> edgeCodeOf screen <> "_"
                       <> capsCode (rotateWallCaps facing authored caps)
                       <> ".png")
            got `shouldNotSatisfy` V.null
            L.nub (map faceMapId (concatMap quadVerts (V.toList got)))
                `shouldBe` [fromIntegral (toInt wantFace)]

-- * The handoff

handoffSpec ∷ Spec
handoffSpec = describe "the handoff at progress 1.0" $ do
    let sp   = wallPiece WallNE
        last' = framePathsFor (AppearanceKey Nothing (ApWall WallNE)) !! 3
        piecePlaced = StructurePieceData 0 0 floorGridZ
        stagedStage = emptyStructureStage
            { ssEntries = HM.singleton (structKey SWallNE)
                              (StagedStructurePiece (StructureStageToken 1)
                                                    piecePlaced) }
        committed = HM.insert (structKey SWallNE) piecePlaced homeStructures

    it "still shows the LAST frame before the worker places anything" $ do
        let ge = envFor FaceSouth (paidAt sp 1.0) homeStructures
                     emptyStructureStage
        L.nub (map sqTexture (V.toList (snd (structureConstructionGhosts ge))))
            `shouldBe` [handleForPath last']

    it "still shows it while the placement is STAGED but not committed — \
       \the rendered scene has nothing there yet" $ do
        let ge = envFor FaceSouth (paidAt sp 1.0) homeStructures stagedStage
        L.nub (map sqTexture (V.toList (snd (structureConstructionGhosts ge))))
            `shouldBe` [handleForPath last']

    it "stops the moment the piece is COMMITTED, even though the \
       \designation is still there" $ do
        let ge = envFor FaceSouth (paidAt sp 1.0) committed emptyStructureStage
        snd (structureConstructionGhosts ge) `shouldSatisfy` V.null

    it "stops when the designation itself goes" $ do
        let ge = envFor FaceSouth HM.empty committed emptyStructureStage
        snd (structureConstructionGhosts ge) `shouldSatisfy` V.null

    it "never draws the frame and the finished piece at once" $ do
        let ge = envFor FaceSouth (paidAt sp 1.0) committed emptyStructureStage
            placed = placedQuads FaceSouth SWallNE sp floorGridZ
        -- The structure pass draws the committed piece…
        placed `shouldNotSatisfy` null
        -- …and the construction pass adds nothing beside it.
        V.length (snd (structureConstructionGhosts ge)) `shouldBe` 0

    it "leaves no gap: every stage from 0 to committed draws something" $ do
        let stages =
                [ V.length (snd (structureConstructionGhosts
                      (envFor FaceSouth (paidAt sp p) homeStructures
                           emptyStructureStage)))
                | p ← [0, 0.25, 0.5, 0.75, 1.0] ]
                ⧺ [ V.length (snd (structureConstructionGhosts
                      (envFor FaceSouth (paidAt sp 1.0) homeStructures
                           stagedStage))) ]
        stages `shouldSatisfy` all (> 0)

-- * No declaration

absenceSpec ∷ Spec
absenceSpec = describe "a paid designation whose appearance declares no \
                       \frames" $ do
    it "draws nothing at all, at every progress and every facing" $
        forM_ [ w | w ← allWireShapes, w `notElem` framedWireShapes ] $ \shape →
            forM_ allFacings $ \facing →
                forM_ [0.0, 0.5, 1.0] $ \progress → do
                    let sp = StructurePiece fixtureWirePack "wire" Nothing
                        ge = wireEnvFor facing (paidAt sp progress) shape
                    snd (structureConstructionGhosts ge) `shouldSatisfy` V.null

    it "draws its declared frames for a shape that HAS them, so the \
       \absence above is a declaration difference and not a dead pass" $
        forM_ framedWireShapes $ \shape → do
            let sp = StructurePiece fixtureWirePack "wire" Nothing
                ge = wireEnvFor FaceSouth (paidAt sp 0.5) shape
                want = framePathsFor (AppearanceKey Nothing (ApWire shape)) !! 3
            L.nub (map sqTexture
                      (V.toList (snd (structureConstructionGhosts ge))))
                `shouldBe` [handleForPath want]

    it "draws nothing for an undeclared wall DIRECTION of a family that \
       \declares others" $ do
        -- The `damaged` variant declares ne only; a designation never
        -- selects a variant, so this is asserted on the catalogue.
        forM_ [WallNW, WallSE, WallSW] $ \e →
            resolveConstructionFrame fixtureCatalog fixturePack
                (AppearanceKey (Just damagedVariant) (ApWall e)) 0.5
                `shouldBe` Nothing
        resolveConstructionFrame fixtureCatalog fixturePack
            (AppearanceKey (Just damagedVariant) (ApWall WallNE)) 0.5
            `shouldSatisfy` isJust

-- * Save round trip

roundTripSpec ∷ Spec
roundTripSpec = describe "a designation round-tripped through the \
                         \page-activity component" $
    it "re-selects the frame it was showing, from cdProgress alone" $
        forM_ [0.0, 0.17, 0.5, 0.83, 1.0] $ \progress →
            forM_ everyKind $ \(label, sp, _, _, committed) → do
                -- Through the component's OWN map codec and real bytes,
                -- so this is the shape a save carries and not a hand
                -- copy of one field.
                let before = paidAt sp progress
                    bytes  = Cereal.encode (toConstructDTO before)
                decoded ← either (fail ∘ (label ⧺)) pure
                    (Cereal.decode bytes
                        ∷ Either String
                              (HM.HashMap (Int, Int) ConstructDesignationDTO))
                let after = fromConstructDTO decoded
                fmap cdProgress (HM.lookup homeTile after)
                    `shouldBe` fmap cdProgress (HM.lookup homeTile before)
                let ge0 = envFor FaceSouth before committed emptyStructureStage
                    ge1 = envFor FaceSouth after  committed emptyStructureStage
                map sqTexture (V.toList (snd (structureConstructionGhosts ge1)))
                    `shouldBe`
                    map sqTexture (V.toList (snd (structureConstructionGhosts ge0)))

-- * Fixture plumbing

-- | Every kind the fixture packs offer, with its slot and the grid z the
--   placer would use.
--   The committed set is the tile's state BEFORE this piece lands: a
--   post needs its supporting floor, and every other kind needs its own
--   slot to be EMPTY — a site whose piece is already committed has
--   finished, which is the handoff group's business, not this one's.
everyKind ∷ [(String, StructurePiece, StructureSlot, Int, ChunkStructures)]
everyKind =
    [ ("floor",   piece fixturePack "floor"   Nothing,   SFloor,   surfaceZ + 1
      , emptyChunkStructures)
    , ("ceiling", piece fixturePack "ceiling" Nothing,   SCeiling, surfaceZ + 2
      , emptyChunkStructures)
    , ("post n",  piece fixturePack "post" (Just "n"),   SPostN,   floorGridZ
      , structuresAt [SFloor])
    , ("wall ne", piece fixturePack "wall" (Just "ne"),  SWallNE,  surfaceZ + 1
      , emptyChunkStructures)
    , ("wall nw", piece fixturePack "wall" (Just "nw"),  SWallNW,  surfaceZ + 1
      , emptyChunkStructures)
    ]
  where piece = StructurePiece

wallPiece ∷ WallEdge → StructurePiece
wallPiece e = StructurePiece fixturePack "wall" (Just (edgeCodeOf e))

edgeCodeOf ∷ WallEdge → Text
edgeCodeOf e = case e of
    WallNE → "ne"; WallNW → "nw"; WallSE → "se"; WallSW → "sw"

capsCode ∷ WallCaps → Text
capsCode (WallCaps l r) = bit l <> bit r
  where bit b = if b then "1" else "0"

-- | The frame the fixture says this piece shows at @progress@, resolved
--   through the same appearance rule the pass uses so a change to that
--   rule fails an example rather than agreeing with itself.
expectedFrame ∷ GhostEnv → StructurePiece → Float → ArtAsset
expectedFrame ge sp progress =
    fromMaybe (error "fixture declared no frames for this appearance") $ do
        ak ← constructionAppearanceAt (gePlan ge) (geFacing ge) sp homeTile
        resolveConstructionFrame fixtureCatalog (spPack sp) ak progress

paidAt ∷ StructurePiece → Float → ConstructDesignations
paidAt sp progress = HM.singleton homeTile
    (baseDesignation sp) { cdProgress = progress
                         , cdPayment  = CpPaid receipt }
  where
    receipt = mkMaterialReceipt [("steel_plate", 1)]

unpaidAt ∷ StructurePiece → ConstructDesignations
unpaidAt sp = HM.singleton homeTile (baseDesignation sp)

baseDesignation ∷ StructurePiece → ConstructDesignation
baseDesignation sp =
    newConstructDesignation surfaceZ (CtStructure sp) firstConstructAttemptId

structKey ∷ StructureSlot → (Int, Int, Word8)
structKey slot =
    (fst homeTile, snd homeTile, fromIntegral (fromEnum slot))

homeStructures ∷ ChunkStructures
homeStructures = structuresAt [SFloor]

-- | The home tile's committed pieces. A floor is always there because a
--   post takes its supporting floor's z.
structuresAt ∷ [StructureSlot] → ChunkStructures
structuresAt slots = HM.fromList
    [ (structKey slot, StructurePieceData 0 0 floorGridZ) | slot ← slots ]

envFor ∷ CameraFacing → ConstructDesignations → ChunkStructures
       → StructureStage → GhostEnv
envFor facing designs structures stage =
    ghostEnv facing designs structures stage HS.empty

-- | A wire environment whose four cardinal neighbours are arranged to
--   produce exactly @shape@, so the pass resolves the connection variant
--   from the world the way the placer does.
wireEnvFor ∷ CameraFacing → ConstructDesignations → WireShape → GhostEnv
wireEnvFor facing designs shape =
    ghostEnv facing designs (wireNeighbourStructures shape) emptyStructureStage
        HS.empty

-- | Committed wire on whichever neighbours @shape@ needs. Derived by
--   searching the shape rule rather than restating a sixteen-way table.
wireNeighbourStructures ∷ WireShape → ChunkStructures
wireNeighbourStructures shape = HM.fromList
    [ ((gx + dx, gy + dy, fromIntegral (fromEnum SWire))
      , StructurePieceData 0 0 floorGridZ)
    | ((dx, dy), present) ← zip [(0, -1), (1, 0), (0, 1), (-1, 0)] mask
    , present ]
  where
    (gx, gy) = homeTile
    mask = firstOf $ [ m
                     | m ← replicateM 4 [False, True]
                     , shapeOf m ≡ shape ]
                     ⧺ [replicate 4 False]
    shapeOf [n, e, s, w] = wireShapeOfMask n e s w
    shapeOf _            = WireIsolated

wireShapeOfMask ∷ Bool → Bool → Bool → Bool → WireShape
wireShapeOfMask n e s w = wireShapeFor (WireNeighbors n e s w)

ghostEnv ∷ CameraFacing → ConstructDesignations → ChunkStructures
         → StructureStage → HS.HashSet (Int, Int) → GhostEnv
ghostEnv facing designs structures stage proposed = GhostEnv
    { geCatalog    = fixtureWallCatalog
    , geLookupSlot = \h → fromIntegral (toInt h)
    , geTexSizes   = fixtureTexSizes
    , geFacing     = facing
    , geZSlice     = zSlice
    , geEffDepth   = effDepth
    , geTileAlpha  = tileAlpha
    , geViewBounds = boundsFor cam
    , geCamX       = fst (camPosition cam)
    , geCamY       = snd (camPosition cam)
    , gePlan       = PlanWorld
        { pwWorldSize    = worldSize
        , pwTiles        = tileDataWith structures
        , pwStage        = stage
        , pwDesignations = designs
        , pwCatalog      = fixtureCatalog
        , pwProposedWire = proposed
        }
    }
  where cam = cameraAt facing homeTile

cameraAt ∷ CameraFacing → (Int, Int) → Camera2D
cameraAt facing (gx, gy) =
    let (wx, wy) = gridToWorld facing gx gy
    in defaultCamera { camPosition = (wx, wy), camZoom = zoom
                     , camFacing = facing, camZSlice = zSlice }

boundsFor ∷ Camera2D → ViewBounds
boundsFor cam = computeViewBounds cam fbW fbH effDepth

tileDataWith ∷ ChunkStructures → WorldTileData
tileDataWith structures = WorldTileData
    { wtdChunks = HM.fromList
        [ (lcCoord lc, lc)
        | lc ← [ chunkAt homeChunkCoord structures
               , chunkAt (ChunkCoord 4 3) emptyChunkStructures
               , chunkAt (ChunkCoord 3 4) emptyChunkStructures ] ]
    , wtdMaxChunks = 200 }

chunkAt ∷ ChunkCoord → ChunkStructures → LoadedChunk
chunkAt coord structures =
    let area = chunkSize * chunkSize
        col = ColumnTiles { ctStartZ = 0
                          , ctMats   = VU.replicate 20 1
                          , ctSlopes = VU.replicate 20 0
                          , ctVeg    = VU.replicate 20 0 }
    in LoadedChunk
        { lcCoord = coord
        , lcTiles = V.replicate area col
        , lcSurfaceMap = VU.replicate area surfaceZ
        , lcTerrainSurfaceMap = VU.replicate area surfaceZ
        , lcFluidMap = V.replicate area Nothing
        , lcIceMap = emptyIceMap, lcFlora = emptyFloraChunkData
        , lcSideDeco = VU.empty, lcWaterTableMap = VU.empty
        , lcMagma = Nothing, lcStructures = structures
        }

-- | The PLACED-piece producer for the same appearance at the same z,
--   through the palette entry point the construction pass never touches.
placedQuads ∷ CameraFacing → StructureSlot → StructurePiece → Int
            → [SortableQuad]
placedQuads facing slot sp gridZ =
    structurePieceQuads fixtureWallCatalog palette handles
        (\h → fromIntegral (toInt h)) fixtureTexSizes facing zSlice effDepth
        1.0 (fst homeTile) (snd homeTile) slot
        (StructurePieceData texId faceId gridZ)
  where
    art = fromMaybe (error "fixture static art did not resolve") $
        resolveUnplacedArt fixtureCatalog (spPack sp) (spKind sp) (spEdge sp)
            defaultPieceArtContext
    (texId, p1) = internPath (aaPath (paTexture art)) emptyTexPalette
    (faceId, palette) = internPath (aaPath (paFacemap art)) p1
    handles = HM.fromList
        [ (texId,  aaHandle (paTexture art))
        , (faceId, aaHandle (paFacemap art)) ]

-- | One quad projected onto everything a construction frame may NOT
--   change: position, UV, facemap, world UV, sort key and layer. The
--   texture handle, its baked atlas id and the render flags are the
--   three fields it may.
type LifecycleShape =
    (Float, String, [(Float, Float, Float, Float, Float, String)])

lifecycleShapes ∷ V.Vector SortableQuad → [LifecycleShape]
lifecycleShapes = map shapeOf ∘ V.toList
  where
    shapeOf sq =
        ( sqSortKey sq
        , show (sqLayer sq)
        , [ (px, py, tu, tv, faceMapId v, show (worldUV v))
          | v ← quadVerts sq
          , let Vec2 px py = pos v
          , let Vec2 tu tv = tex v ] )

allFlags ∷ V.Vector SortableQuad → [Word32]
allFlags = L.nub ∘ map renderFlags ∘ concatMap quadVerts ∘ V.toList

quadVerts ∷ SortableQuad → [Vertex]
quadVerts sq = [sqV0 sq, sqV1 sq, sqV2 sq, sqV3 sq]

-- | The first element, with a fixture-specific failure rather than
--   @head@'s. Every caller has already established the list is
--   non-empty from the fixture's own declaration.
firstOf ∷ [α] → α
firstOf = fromMaybe (error "fixture list was unexpectedly empty") ∘ listToMaybe
