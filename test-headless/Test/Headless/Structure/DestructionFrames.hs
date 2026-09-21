{-# LANGUAGE OverloadedStrings #-}
-- | Structure teardown PRESENTATION (#2491), pure half: what a pack may
--   declare, what a cleared piece captures, when a clip's frame changes
--   and when it stops existing — all without an engine.
--
--   == What "the same as the placed piece" means here
--
--   The render examples compare an effect's quads against the PLACED
--   piece producer for the same appearance at the same tile, z and slot,
--   reached through the texture-palette entry point the teardown pass
--   never touches ('Structure.Render.structurePieceQuads'). The two must
--   agree on everything except the three things a lifecycle frame is
--   allowed to change — its texture handle, the atlas id that handle
--   bakes to, and the lifecycle render flag. That one comparison covers
--   the grid z, the slot geometry, #1712's camera rotation, #415's
--   front-wall strips, the post inset, the sort keys and the facemap the
--   appearance resolved, and none of it is the renderer checked against
--   itself.
--
--   == What is deliberately NOT proven here
--
--   Anything that needs a live session: the capture ORDER against the
--   edit log, the queries reading absent, the bulk paths, snapshots,
--   page replacement and the 'ScStructures' telemetry are
--   "Test.Headless.World.StructureDestruction"'s, through the real
--   handlers.
--
--   Run just this gate: @cabal test synarchy-test-headless
--   --test-options='--match "structure destruction presentation lifecycle"'@
module Test.Headless.Structure.DestructionFrames (spec) where

import UPrelude
import Test.Hspec
import qualified Data.HashMap.Strict as HM
import qualified Data.List as L
import qualified Data.Text as T
import qualified Data.Vector as V

import Engine.Asset.Handle (TextureHandle(..), toInt)
import Engine.Graphics.Camera
    (Camera2D(..), CameraFacing(..), defaultCamera)
import Engine.Graphics.Vulkan.Types.Vertex
    (Vertex(..), Vec2(..), renderFlagLifecycleAlpha)
import Engine.Scene.Types (SortableQuad(..))
import Structure.ArtCatalog
import Structure.Destruction
import Structure.Facing (WallCaps(..), WallEdge(..), screenWallEdge)
import Structure.Palette (TexPalette, emptyTexPalette, internPath, lookupPath)
import Structure.Render (structurePieceQuads)
import Structure.Types (StructurePieceData(..), StructureSlot(..))
import Structure.Wire (WireShape(..), allWireShapes)
import World.Chunk.Types (chunkSize)
import World.Generate (viewDepth)
import World.Grid (gridToWorld)
import World.Page.Types (WorldPageId(..))
import World.Render.StructureDestruction (structureDestructionQuads)
import World.Render.ViewBounds (ViewBounds, computeViewBounds)

import Test.Headless.Render.StructureGhostFixture (handleForPath)
import Test.Headless.Structure.ConstructionFixture
import Test.Headless.Structure.DestructionFixture

-- * Fixture geography (the construction suite's, so the two compare)

worldSize, surfaceZ, zSlice, pieceZ ∷ Int
worldSize = 64
surfaceZ  = 10
zSlice    = 14
pieceZ    = surfaceZ + 1

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

allFacings ∷ [CameraFacing]
allFacings = [FaceSouth, FaceWest, FaceNorth, FaceEast]

fixturePage ∷ WorldPageId
fixturePage = WorldPageId "destruction_frames_page"

-- | The camera the render examples run at: centred on 'homeTile' in
--   THIS facing's own world frame, exactly as the construction suite's
--   is. Centring matters — the teardown pass culls its effects through
--   'World.Render.ChunkCulling.isChunkVisibleWrapped' like every other
--   per-page producer, so a camera parked at the origin would leave the
--   home chunk off screen at three of the four facings and the rotation
--   examples would pass by drawing nothing.
camera ∷ CameraFacing → Camera2D
camera facing =
    let (wx, wy) = gridToWorld facing (fst homeTile) (snd homeTile)
    in defaultCamera { camPosition = (wx, wy), camZoom = zoom
                     , camFacing = facing, camZSlice = zSlice }

viewBounds ∷ CameraFacing → ViewBounds
viewBounds facing = computeViewBounds (camera facing) fbW fbH effDepth

camXY ∷ CameraFacing → (Float, Float)
camXY = camPosition ∘ camera

lookupSlotId ∷ TextureHandle → Word32
lookupSlotId = fromIntegral ∘ toInt

-- * The palette a placed piece is stored through

-- | Every static path the fixture packs declare, in a fixed order so an
--   id is stable across examples.
palettePaths ∷ [Text]
palettePaths = L.nub $ concat
    [ [ aaPath (paTexture art), aaPath (paFacemap art) ]
    | (_, art) ← parEntries wreckRegistration
                   ⧺ parEntries wreckWireRegistration ]
    ⧺ [ staticPathFor ak | ak ← wreckAppearances ]

fixturePalette ∷ TexPalette
fixturePalette = foldl (\p path → snd (internPath path p))
                       emptyTexPalette palettePaths

paletteIdOf ∷ Text → Int
paletteIdOf path = case [ i | (i, p) ← idPairs, p ≡ path ] of
    (i : _) → i
    []      → error ("fixture palette has no id for " <> T.unpack path)
  where
    idPairs = [ (i, p) | i ← [0 .. length palettePaths - 1]
                       , Just p ← [lookupPath i fixturePalette] ]

-- | The live handle map: every fixture path resolved.
fixtureHandles ∷ HM.HashMap Int TextureHandle
fixtureHandles = HM.fromList
    [ (paletteIdOf path, handleForPath path) | path ← palettePaths ]

-- * Placed pieces, and the effects clearing them captures

-- | The static art one appearance is PLACED with: its own sprite, and
--   the facemap its art slot declares (an uncapped wall's "00" mask).
placedArtFor ∷ AppearanceKey → (Text, Text)
placedArtFor ak = case apSlot ak of
    ApWall e →
        ( staticPathFor ak
        , firstOf [ aaPath (paFacemap art)
               | (AkWall e' c, art) ← parEntries wreckRegistration
               , e' ≡ e, c ≡ WallCaps False False ] )
    ApWire w →
        ( staticPathFor ak
        , firstOf [ aaPath (paFacemap art)
               | (AkWire w', art) ← parEntries wreckWireRegistration
               , w' ≡ w ] )
    _ →
        ( staticPathFor ak
        , firstOf [ aaPath (paFacemap art)
               | (key, art) ← parEntries wreckRegistration
               , defaultAppearance key ≡ AppearanceKey Nothing (apSlot ak) ] )

pieceDataFor ∷ AppearanceKey → StructurePieceData
pieceDataFor ak = StructurePieceData (paletteIdOf tex) (paletteIdOf face) pieceZ
  where (tex, face) = placedArtFor ak

slotOf ∷ AppearanceKey → StructureSlot
slotOf ak = case apSlot ak of
    ApFloor   → SFloor
    ApCeiling → SCeiling
    ApPost    → SPostN
    ApWall e  → case e of
        WallNE → SWallNE; WallNW → SWallNW
        WallSE → SWallSE; WallSW → SWallSW
    ApWire _  → SWire

slotTagOf ∷ AppearanceKey → Word8
slotTagOf = fromIntegral ∘ fromEnum ∘ slotOf

-- | The capture the production handler would make, at game time @now@.
captureAt ∷ Double → AppearanceKey → DestructionCapture
captureAt now ak = captureStructureDestruction wreckCatalog fixturePalette
    fixtureHandles now fixturePage (fst homeTile) (snd homeTile)
    (slotTagOf ak) (pieceDataFor ak)

effectFor ∷ AppearanceKey → StructureDestructionEffect
effectFor ak = case captureAt 0 ak of
    CapturedEffect e → e
    other → error ("fixture appearance captured nothing: " <> show other)

-- * Quads

effectQuadsAt ∷ CameraFacing → Double → StructureDestructionEffect
              → [SortableQuad]
effectQuadsAt facing now eff =
    structureDestructionQuads wreckCatalog fixtureWallCatalog fixtureHandles
        lookupSlotId wreckTexSizes facing zSlice effDepth tileAlpha
        worldSize (viewBounds facing) camX camY now [eff]
  where (camX, camY) = camXY facing

placedQuadsAt ∷ CameraFacing → AppearanceKey → [SortableQuad]
placedQuadsAt facing ak =
    structurePieceQuads fixtureWallCatalog fixturePalette fixtureHandles
        lookupSlotId wreckTexSizes facing zSlice effDepth tileAlpha
        (fst homeTile) (snd homeTile) (slotOf ak) (pieceDataFor ak)

-- | Everything about a quad that a lifecycle frame may NOT change:
--   the sort key, the layer, and each vertex's position, UV, resolved
--   facemap slot and packed world UV. Deliberately excludes the three
--   things it may — the texture handle, the atlas id that handle bakes
--   to, and the render flags — so the comparison is a real one rather
--   than a tautology. Mirrors the construction suite's own shape
--   ('Test.Headless.Structure.ConstructionFrames'), because the two
--   lifecycles must be answerable by the same question.
--
--   'SortableQuad' has no 'Eq' instance, which is why this is a
--   projection to comparable values rather than a normalised quad.
type QuadShape =
    (Float, String, [(Float, Float, Float, Float, Float, String)])

quadShapes ∷ [SortableQuad] → [QuadShape]
quadShapes = map shapeOf
  where
    shapeOf sq =
        ( sqSortKey sq
        , show (sqLayer sq)
        , [ (px, py, tu, tv, faceMapId v, show (worldUV v))
          | v ← quadVerts sq
          , let Vec2 px py = pos v
          , let Vec2 tu tv = tex v ] )

quadVerts ∷ SortableQuad → [Vertex]
quadVerts sq = [sqV0 sq, sqV1 sq, sqV2 sq, sqV3 sq]

allFlags ∷ [SortableQuad] → [Word32]
allFlags = L.nub ∘ map renderFlags ∘ concatMap quadVerts

-- * Spec

spec ∷ Spec
spec = describe "structure destruction presentation lifecycle" $ do
    declarationSpec
    captureSpec
    timingSpec
    renderSpec
    rotationSpec
    refusalSpec

-- * What a pack declares, and what resolves it

declarationSpec ∷ Spec
declarationSpec = describe "a declared clip" $ do

    it "resolves its OWN frames and no other appearance's" $
        forM_ wreckAppearances $ \ak → do
            let ds = resolveDestructionSequence wreckCatalog (packOf ak) ak
            fmap (map aaPath ∘ V.toList ∘ dsFrames) ds
                `shouldBe` Just (wreckFramePathsFor ak)

    it "keeps its own declared fps" $
        forM_ wreckAppearances $ \ak →
            fmap dsFps (resolveDestructionSequence wreckCatalog (packOf ak) ak)
                `shouldBe` Just (wreckFps ak)

    it "resolves nothing for an appearance that declares none" $
        forM_ ( AppearanceKey Nothing ApCeiling
                  : [ AppearanceKey Nothing (ApWire w)
                    | w ← allWireShapes
                    , isNothing (wreckFrameCount
                                    (AppearanceKey Nothing (ApWire w))) ] )
              $ \ak →
            resolveDestructionSequence wreckCatalog (packOf ak) ak
                `shouldBe` Nothing

    it "never lets a VARIANT inherit the default's clip, or the default \
       \inherit a variant's" $ do
        let variantFloor = AppearanceKey (Just damagedVariant) ApFloor
            defaultFloor = AppearanceKey Nothing ApFloor
            frames ak = fmap (map aaPath ∘ V.toList ∘ dsFrames)
                             (resolveDestructionSequence wreckCatalog
                                  fixturePack ak)
        frames variantFloor `shouldNotBe` frames defaultFloor
        frames (AppearanceKey (Just damagedVariant) ApPost) `shouldBe` Nothing
        frames (AppearanceKey Nothing ApPost) `shouldSatisfy` isJust

    it "resolves nothing at all once the pack's art has terminally failed" $ do
        let (failed, _) = failPackArtPath "fx/floor.png" "missing" wreckCatalog
        resolveDestructionSequence failed fixturePack
            (AppearanceKey Nothing ApFloor) `shouldBe` Nothing
        appearanceForTexturePath failed "fx/floor.png" `shouldBe` Nothing

    it "invalidates the whole pack when a destruction FRAME fails to \
       \load, naming the frame's position" $ do
        let path = firstOf (wreckFramePathsFor (AppearanceKey Nothing ApPost))
            (failed, report) = failPackArtPath path "missing" wreckCatalog
        afrTracked report `shouldBe` True
        let message = artAssetFailureMessage (fromJust' (afrFailure report))
        message `shouldSatisfy` T.isInfixOf fixturePack
        message `shouldSatisfy` T.isInfixOf "post destruction frame 1"
        packArtResolves failed fixturePack `shouldBe` False

    it "reports a missing declaration once per (pack, appearance) and \
       \then stays silent" $ do
        let ak = AppearanceKey Nothing ApCeiling
            (once, m1) = noteMissingDestruction fixturePack ak wreckCatalog
            (_,    m2) = noteMissingDestruction fixturePack ak once
        m1 `shouldSatisfy` isJust
        fromJust' m1 `shouldSatisfy` T.isInfixOf fixturePack
        fromJust' m1 `shouldSatisfy` T.isInfixOf "ceiling"
        fromJust' m1 `shouldSatisfy` T.isInfixOf "no destruction frames"
        m2 `shouldBe` Nothing

    it "keeps the dedup per APPEARANCE, not per pack" $ do
        let ak1 = AppearanceKey Nothing ApCeiling
            ak2 = AppearanceKey Nothing (ApWall WallNE)
            (once, _) = noteMissingDestruction fixturePack ak1 wreckCatalog
        snd (noteMissingDestruction fixturePack ak2 once)
            `shouldSatisfy` isJust

    it "maps every declared appearance's static sprite back to it" $
        forM_ wreckAppearances $ \ak →
            appearanceForTexturePath wreckCatalog (staticPathFor ak)
                `shouldBe` Just (packOf ak, ak)

    it "maps a path no pack declares to nothing" $
        appearanceForTexturePath wreckCatalog "fx/not_a_pack_sprite.png"
            `shouldBe` Nothing

    it "maps an AUTHORED variant appearance back to itself even when it \
       \declares no lifecycle frames at all" $ do
        -- The state a variant is invisible in unless its static art is
        -- registered: overridden sprite, no construction sequence and no
        -- teardown clip. Without it a clear of such a piece is silent
        -- and requirement 6's report is never owed, which is the whole
        -- point of carrying 'parVariants'.
        apVariant wreckOverriddenNoClip `shouldBe` Just damagedVariant
        wreckFrameCount wreckOverriddenNoClip `shouldBe` Nothing
        appearanceForTexturePath wreckCatalog
                (staticPathFor wreckOverriddenNoClip)
            `shouldBe` Just (fixturePack, wreckOverriddenNoClip)
        resolveDestructionSequence wreckCatalog fixturePack
            wreckOverriddenNoClip `shouldBe` Nothing

    it "never resolves an INHERITED variant sprite as the default \
       \appearance, so neither claimant's clip plays" $ do
        -- A variant that does NOT override this appearance is placed
        -- with the default's own sprite, so a palette id cannot tell the
        -- two apart. Answering "the default" would play the default's
        -- clip for a variant's piece, which requirement 1 forbids — so
        -- the path answers NOTHING, for both claimants.
        let inherited = AppearanceKey (Just damagedVariant) ApCeiling
            shared    = staticPathFor (AppearanceKey Nothing ApCeiling)
            reg = wreckRegistration
                    { parVariants = (inherited, artAsset shared)
                                      : parVariants wreckRegistration }
            cat = registerOrFail reg emptyStructureArtCatalog
        appearanceForTexturePath cat shared `shouldBe` Nothing
        -- …and the pack says so, once, naming the sprite.
        ambiguousAppearancePaths cat fixturePack `shouldBe` [shared]
        ambiguousAppearanceMessage fixturePack shared
            `shouldSatisfy` T.isInfixOf shared

    it "leaves every UNSHARED sprite resolving exactly as it did" $ do
        -- The ambiguity above is per-PATH, not per-pack: a contested
        -- ceiling must not stop a floor from resolving.
        let inherited = AppearanceKey (Just damagedVariant) ApCeiling
            shared    = staticPathFor (AppearanceKey Nothing ApCeiling)
            reg = wreckRegistration
                    { parVariants = (inherited, artAsset shared)
                                      : parVariants wreckRegistration }
            cat = registerOrFail reg emptyStructureArtCatalog
            full = registerOrFail wreckWireRegistration cat
        forM_ wreckAppearances $ \ak →
            appearanceForTexturePath full (staticPathFor ak)
                `shouldBe` Just (packOf ak, ak)

-- * Capture

captureSpec ∷ Spec
captureSpec = describe "capturing a cleared piece" $ do

    it "records the piece's exact former identity" $ do
        let ak  = AppearanceKey Nothing ApFloor
            eff = effectFor ak
            (tex, face) = placedArtFor ak
        sdePage eff       `shouldBe` fixturePage
        (sdeGX eff, sdeGY eff) `shouldBe` homeTile
        sdeSlotTag eff    `shouldBe` slotTagOf ak
        sdeGridZ eff      `shouldBe` pieceZ
        sdeTexPath eff    `shouldBe` tex
        sdeFacePath eff   `shouldBe` Just face
        sdeTexHandle eff  `shouldBe` Just (handleForPath tex)
        sdeFaceHandle eff `shouldBe` Just (handleForPath face)
        sdePack eff       `shouldBe` fixturePack
        sdeAppearance eff `shouldBe` ak
        sdeFrameCount eff `shouldBe` fromJust' (wreckFrameCount ak)
        sdeFps eff        `shouldBe` wreckFps ak
        sdeStartedAt eff  `shouldBe` 0

    it "keys the effect at the canonical tile and slot it was cleared at" $
        destructionSlotKey (effectFor (AppearanceKey Nothing ApPost))
            `shouldBe` (fst homeTile, snd homeTile
                       , slotTagOf (AppearanceKey Nothing ApPost))

    it "captures a VARIANT's own appearance, not the default's" $ do
        let ak = AppearanceKey (Just damagedVariant) ApFloor
        sdeAppearance (effectFor ak) `shouldBe` ak
        sdeFps (effectFor ak) `shouldBe` wreckFps ak

    it "captures nothing and asks for a report when the appearance \
       \declares no clip" $
        captureAt 3 (AppearanceKey Nothing ApCeiling)
            `shouldBe` CaptureUndeclared fixturePack
                           (AppearanceKey Nothing ApCeiling)

    it "captures nothing and says nothing for art no pack declares" $ do
        let loose = StructurePieceData (paletteIdOf "fx/floor.png" + 9999)
                                       (paletteIdOf "fx/floorface.png") pieceZ
        captureStructureDestruction wreckCatalog fixturePalette fixtureHandles
            0 fixturePage (fst homeTile) (snd homeTile) 0 loose
            `shouldBe` CaptureSilent

    it "still captures with an EMPTY handle map, keeping the clip and \
       \the start time" $ do
        let ak = AppearanceKey Nothing ApFloor
            capture = captureStructureDestruction wreckCatalog fixturePalette
                          HM.empty 7 fixturePage (fst homeTile) (snd homeTile)
                          (slotTagOf ak) (pieceDataFor ak)
        case capture of
            CapturedEffect eff → do
                sdeTexHandle eff  `shouldBe` Nothing
                sdeFaceHandle eff `shouldBe` Nothing
                sdeFrameCount eff `shouldBe` fromJust' (wreckFrameCount ak)
                sdeFps eff        `shouldBe` wreckFps ak
                sdeStartedAt eff  `shouldBe` 7
            other → expectationFailure
                        ("an unresolved handle must not lose the effect: "
                           <> show other)

-- * Timing

timingSpec ∷ Spec
timingSpec = describe "playback timing" $ do

    it "lasts frames / fps game seconds" $
        forM_ wreckAppearances $ \ak →
            destructionEffectDuration (effectFor ak) `shouldBe`
                fromIntegral (fromJust' (wreckFrameCount ak)) / wreckFps ak

    it "selects floor (elapsed * fps), clamped, at three clock offsets" $ do
        -- The default POST clip: 5 frames at 8 fps, so a frame lasts
        -- 0.125 s and the clip lasts 0.625 s.
        let eff = effectFor (AppearanceKey Nothing ApPost)
        map (`destructionEffectFrameIndex` eff) [0, 0.3, 0.6]
            `shouldBe` [Just 0, Just 2, Just 4]

    it "reads the same phase at a DIFFERENT fps for a different clip" $ do
        -- The default FLOOR clip: 3 frames at 12 fps. The same three
        -- clock readings land differently, so an index taken from the
        -- wrong appearance is visible.
        let eff = effectFor (AppearanceKey Nothing ApFloor)
        map (`destructionEffectFrameIndex` eff) [0, 0.0834, 0.2]
            `shouldBe` [Just 0, Just 1, Just 2]

    it "plays once and forward, never wrapping past the last frame" $ do
        let eff = effectFor (AppearanceKey Nothing ApPost)
            n   = sdeFrameCount eff
            step = 1 / (sdeFps eff * 4)
            seen = [ i | t ← [0, step .. destructionEffectDuration eff - step]
                       , Just i ← [destructionEffectFrameIndex t eff] ]
        L.nub seen `shouldBe` [0 .. n - 1]
        seen `shouldBe` L.sort seen

    it "expires exactly at the clip's end, not before it" $ do
        let eff = effectFor (AppearanceKey Nothing ApPost)
            d   = destructionEffectDuration eff
        destructionEffectExpired (d - 0.0001) eff `shouldBe` False
        destructionEffectExpired d eff            `shouldBe` True
        destructionEffectFrameIndex d eff         `shouldBe` Nothing

    it "freezes at its phase while the clock does not advance" $ do
        -- A paused page's clock is rewritten with the value it read, so
        -- every tick asks the same question and must get the same
        -- answer — including "not expired yet".
        let eff = effectFor (AppearanceKey Nothing ApPost)
        L.nub [ destructionEffectFrameIndex 0.3 eff | _ ← [1 .. 5 ∷ Int] ]
            `shouldBe` [Just 2]
        L.nub [ destructionEffectExpired 0.3 eff | _ ← [1 .. 5 ∷ Int] ]
            `shouldBe` [False]

    it "reads frame zero for a clock that has not reached the start" $ do
        let eff = effectFor (AppearanceKey Nothing ApPost)
        destructionEffectElapsed (-5) eff      `shouldBe` 0
        destructionEffectFrameIndex (-5) eff   `shouldBe` Just 0

    it "prunes exactly the effects that are out of time" $ do
        let short = effectFor (AppearanceKey Nothing ApFloor)   -- 0.25 s
            long  = effectFor (AppearanceKey Nothing ApPost)    -- 0.625 s
            live  = insertDestructionEffect long
                        (insertDestructionEffect short
                            emptyStructureDestructions)
        HM.size live `shouldBe` 2
        anyDestructionExpired 0.1 live `shouldBe` False
        anyDestructionExpired 0.3 live `shouldBe` True
        HM.size (pruneExpiredDestructionEffects 0.3 live) `shouldBe` 1
        HM.size (pruneExpiredDestructionEffects 1.0 live) `shouldBe` 0

-- * Rendering

renderSpec ∷ Spec
renderSpec = describe "an effect's quads" $ do

    it "are the PLACED piece's, with only the frame, its atlas id and \
       \the lifecycle flag changed" $
        forM_ [ AppearanceKey Nothing ApFloor
              , AppearanceKey Nothing ApPost
              , AppearanceKey Nothing (ApWire WireCross) ] $ \ak → do
            let eff    = effectFor ak
                drawn  = effectQuadsAt FaceSouth 0 eff
                placed = placedQuadsAt FaceSouth ak
            drawn `shouldNotSatisfy` null
            quadShapes drawn `shouldBe` quadShapes placed

    it "draw the frame the elapsed clock selects, never the static sprite" $ do
        let ak     = AppearanceKey Nothing ApPost
            eff    = effectFor ak
            frames = wreckFramePathsFor ak
            handleAt t = sqTexture <$> take 1 (effectQuadsAt FaceSouth t eff)
        handleAt 0     `shouldBe` [handleForPath (frames !! 0)]
        handleAt 0.3   `shouldBe` [handleForPath (frames !! 2)]
        handleAt 0.6   `shouldBe` [handleForPath (frames !! 4)]
        map sqTexture (placedQuadsAt FaceSouth ak) `shouldNotSatisfy`
            elem (handleForPath (firstOf frames))

    it "carry the lifecycle alpha flag, which a placed piece does not" $ do
        let eff = effectFor (AppearanceKey Nothing ApFloor)
        allFlags (effectQuadsAt FaceSouth 0 eff)
            `shouldBe` [renderFlagLifecycleAlpha]
        allFlags (placedQuadsAt FaceSouth (AppearanceKey Nothing ApFloor))
            `shouldBe` [0]

    it "emit NOTHING once the clip has expired" $ do
        let eff = effectFor (AppearanceKey Nothing ApFloor)
        effectQuadsAt FaceSouth (destructionEffectDuration eff) eff
            `shouldSatisfy` null

    it "emit NOTHING while the piece's handles are unresolved, without \
       \the effect ceasing to exist" $ do
        let ak  = AppearanceKey Nothing ApFloor
            eff = case captureStructureDestruction wreckCatalog fixturePalette
                           HM.empty 0 fixturePage (fst homeTile) (snd homeTile)
                           (slotTagOf ak) (pieceDataFor ak) of
                      CapturedEffect e → e
                      other → error (show other)
            unresolved = structureDestructionQuads wreckCatalog
                             fixtureWallCatalog HM.empty lookupSlotId
                             wreckTexSizes FaceSouth zSlice effDepth
                             tileAlpha worldSize (viewBounds FaceSouth)
                             (fst (camXY FaceSouth)) (snd (camXY FaceSouth))
                             0 [eff]
        unresolved `shouldSatisfy` null
        -- The same effect, once the handle map catches up, draws — with
        -- its ORIGINAL start time still deciding the frame.
        map sqTexture (take 1 (effectQuadsAt FaceSouth 0 eff))
            `shouldBe` [handleForPath (firstOf (wreckFramePathsFor ak))]

    it "draw with no chunk anywhere in sight: an effect is page state, \
       \not chunk state" $ do
        -- The producer takes no chunk, no tile map and no residency
        -- argument at all, so an effect whose source chunk was evicted
        -- after the capture is drawn exactly as one whose chunk is
        -- still resident. Nothing here loads a chunk.
        let eff = effectFor (AppearanceKey Nothing ApFloor)
        effectQuadsAt FaceSouth 0 eff `shouldNotSatisfy` null

    it "NEVER substitutes another appearance's clip for a missing one" $ do
        -- A hand-built effect naming an appearance with no declaration:
        -- the one state capture refuses to produce, asserted at the
        -- render boundary too so a future caller cannot smuggle one in.
        let base = effectFor (AppearanceKey Nothing ApFloor)
            orphan = base { sdeAppearance = AppearanceKey Nothing ApCeiling }
        effectQuadsAt FaceSouth 0 orphan `shouldSatisfy` null

-- * Rotation

rotationSpec ∷ Spec
rotationSpec = describe "a wall's teardown at a rotated camera" $ do

    it "plays the clip of the edge whose art is really DRAWN" $
        forM_ allFacings $ \facing →
            forM_ allWallEdges $ \edge → do
                let ak    = AppearanceKey Nothing (ApWall edge)
                    eff   = effectFor ak
                    drawn = AppearanceKey Nothing
                                (ApWall (screenWallEdge facing edge))
                    frame = firstOf (wreckFramePathsFor drawn)
                map sqTexture (take 1 (effectQuadsAt facing 0 eff))
                    `shouldBe` [handleForPath frame]

    it "keeps the frame INDEX facing-blind" $ do
        let eff = effectFor (AppearanceKey Nothing (ApWall WallNE))
        L.nub [ destructionEffectFrameIndex 0.25 eff | _ ← allFacings ]
            `shouldBe` [Just 2]
        -- …and the four facings' clips agree on length and rate, which
        -- is what makes that index mean the same thing at each.
        L.nub [ (V.length (dsFrames ds), dsFps ds)
              | e ← allWallEdges
              , Just ds ← [ resolveDestructionSequence wreckCatalog fixturePack
                                (AppearanceKey Nothing (ApWall e)) ] ]
            `shouldSatisfy` ((≡ 1) ∘ length)

    it "matches the placed piece's own geometry at all four facings" $
        forM_ allFacings $ \facing →
            forM_ allWallEdges $ \edge → do
                let ak = AppearanceKey Nothing (ApWall edge)
                    drawn  = effectQuadsAt facing 0 (effectFor ak)
                    placed = placedQuadsAt facing ak
                drawn `shouldNotSatisfy` null
                quadShapes drawn `shouldBe` quadShapes placed

    it "expires at the same instant whatever the camera is doing" $ do
        let eff = effectFor (AppearanceKey Nothing (ApWall WallSE))
            d   = destructionEffectDuration eff
        forM_ allFacings $ \facing →
            effectQuadsAt facing d eff `shouldSatisfy` null

-- * Registration refusals

refusalSpec ∷ Spec
refusalSpec = describe "registering a destruction clip" $ do

    it "accepts the fixture pack whole" $
        outcomeOf wreckRegistration `shouldBe` ArtRegistered

    it "refuses an EMPTY frame list" $
        refusal (AppearanceKey Nothing ApFloor)
            (Just (wreckOf ApFloor) { dsFrames = V.empty })
            `shouldSatisfy` faultSays ["fixture_dungeon", "floor", "empty"]

    it "refuses a list that names the same image twice" $ do
        let p = firstOf (wreckFramePathsFor (AppearanceKey Nothing ApFloor))
        refusal (AppearanceKey Nothing ApFloor)
            (Just (wreckOf ApFloor)
                { dsFrames = V.fromList [pathAsset p, pathAsset p] })
            `shouldSatisfy` faultSays ["fixture_dungeon", "more than once"]

    it "refuses a frame path that escapes the resource root" $
        forM_ [ "../secrets/floor.png", "/etc/floor.png"
              , "fx/../../floor.png", "C:/art/floor.png"
              , "assets\\floor.png", "~/floor.png" ] $ \bad →
            refusal (AppearanceKey Nothing ApFloor)
                (Just (wreckOf ApFloor)
                    { dsFrames = V.fromList [pathAsset bad] })
                `shouldSatisfy` faultSays ["fixture_dungeon", "escapes"]

    it "refuses a frame whose handle was never loaded" $
        refusal (AppearanceKey Nothing ApFloor)
            (Just (wreckOf ApFloor)
                { dsFrames = V.fromList
                    [ArtAsset "fx/floor_break_0.png" (TextureHandle 0)] })
            `shouldSatisfy` faultSays ["fixture_dungeon", "not a loaded handle"]

    it "refuses a non-positive or non-finite fps" $
        forM_ [0, -12, 0 / 0, 1 / 0] $ \bad →
            refusal (AppearanceKey Nothing ApFloor)
                (Just (wreckOf ApFloor) { dsFps = bad })
                `shouldSatisfy`
                    faultSays ["fixture_dungeon", "finite positive"]

    it "refuses variant art for a kind the registration never declared" $ do
        let reg = wreckRegistration
                    { parVariants = [ ( AppearanceKey (Just damagedVariant)
                                            (ApWire WireCross)
                                      , artAsset "fx/wire_cross.png" ) ] }
        outcomeOf reg `shouldSatisfy`
            faultSays ["fixture_dungeon", "does not declare"]

    it "refuses a variant entry that names no variant" $ do
        let reg = wreckRegistration
                    { parVariants = [ ( AppearanceKey Nothing ApFloor
                                      , artAsset "fx/floor.png" ) ] }
        outcomeOf reg `shouldSatisfy`
            faultSays ["fixture_dungeon", "not as a variant"]

    it "refuses the same variant appearance twice" $ do
        let ak  = AppearanceKey (Just damagedVariant) ApFloor
            reg = wreckRegistration
                    { parVariants = (ak, artAsset (staticPathFor ak))
                                      : parVariants wreckRegistration }
        outcomeOf reg `shouldSatisfy`
            faultSays ["fixture_dungeon", "more than once"]

    it "refuses a variant sprite whose handle was never loaded" $ do
        let ak  = AppearanceKey (Just damagedVariant) ApFloor
            reg = wreckRegistration
                    { parVariants = [ (ak, ArtAsset (staticPathFor ak)
                                               (TextureHandle 0)) ] }
        outcomeOf reg `shouldSatisfy`
            faultSays ["fixture_dungeon", "not a loaded handle"]

    it "names variant art when a repeat CONFLICTS on it alone" $ do
        let (stored, _) = registerPackArt wreckRegistration
                              emptyStructureArtCatalog
            altered = wreckRegistration { parVariants = [] }
        snd (registerPackArt altered stored) `shouldSatisfy`
            faultSays ["variant art"]

    it "refuses clips for a kind the registration never declared" $ do
        let reg = wreckRegistration
                    { parKinds  = [ e | e@(k, _, _) ← parKinds wreckRegistration
                                      , k ≢ KPost ]
                    , parEntries = [ e | e@(k, _) ← parEntries wreckRegistration
                                       , k ≢ AkPost ]
                    , parFrames  = [ e | e@(ak, _) ← parFrames wreckRegistration
                                       , apSlot ak ≢ ApPost ] }
        outcomeOf reg `shouldSatisfy`
            faultSays ["fixture_dungeon", "does not declare"]

    it "refuses a wall family that declares only SOME directions" $ do
        let reg = withDestruction (AppearanceKey Nothing (ApWall WallSW))
                      Nothing wreckRegistration
        outcomeOf reg `shouldSatisfy`
            faultSays ["fixture_dungeon", "but not sw"]

    it "refuses a wall family whose directions run to different lengths" $ do
        let ak = AppearanceKey Nothing (ApWall WallNE)
            ds = wreckOf (ApWall WallNE)
        refusal ak (Just ds { dsFrames = V.take 2 (dsFrames ds) })
            `shouldSatisfy` faultSays ["fixture_dungeon", "disagree", "ne: 2"]

    it "refuses a wall family whose directions disagree about fps" $ do
        let ak = AppearanceKey Nothing (ApWall WallNW)
        refusal ak (Just (wreckOf (ApWall WallNW)) { dsFps = 30 })
            `shouldSatisfy` faultSays ["fixture_dungeon", "disagree", "30"]

    it "accepts a pack that declares NO wall clips at all" $ do
        let reg = foldr (\e r → withDestruction
                                    (AppearanceKey Nothing (ApWall e)) Nothing r)
                        wreckRegistration allWallEdges
        outcomeOf reg `shouldBe` ArtRegistered

    it "names destruction frames when a repeat CONFLICTS on them alone" $ do
        let (stored, _) = registerPackArt wreckRegistration
                              emptyStructureArtCatalog
            ak = AppearanceKey Nothing ApFloor
            ds = wreckOf ApFloor
            altered = withDestruction ak (Just ds { dsFps = 30 })
                          (withDestruction (AppearanceKey Nothing (ApWall WallNE))
                              (Just (wreckOf (ApWall WallNE)))
                              wreckRegistration)
        snd (registerPackArt altered stored) `shouldSatisfy`
            faultSays ["destruction frames"]

    it "treats an identical repeat as an idempotent no-op" $ do
        let (stored, _) = registerPackArt wreckRegistration
                              emptyStructureArtCatalog
        snd (registerPackArt wreckRegistration stored)
            `shouldBe` ArtAlreadyRegistered

-- * Refusal helpers

wreckOf ∷ AppearanceSlot → DestructionSequence
wreckOf slot = fromJust' (wreckSequenceFor (AppearanceKey Nothing slot))

pathAsset ∷ Text → ArtAsset
pathAsset p = ArtAsset p (handleForPath p)

outcomeOf ∷ PackArtRegistration → RegistrationOutcome
outcomeOf reg = snd (registerPackArt reg emptyStructureArtCatalog)

refusal ∷ AppearanceKey → Maybe DestructionSequence → RegistrationOutcome
refusal ak mds = outcomeOf (withDestruction ak mds wreckRegistration)

faultSays ∷ [Text] → RegistrationOutcome → Bool
faultSays needles outcome = case outcome of
    ArtRegistrationRefused f →
        all (`T.isInfixOf` artFaultMessage f) needles
    _ → False

packOf ∷ AppearanceKey → Text
packOf ak = case apSlot ak of
    ApWire _ → fixtureWirePack
    _        → fixturePack

fromJust' ∷ Maybe α → α
fromJust' (Just a) = a
fromJust' Nothing  = error "fromJust': Nothing"

-- | The first element, with a fixture-specific failure rather than
--   'head'\'s. Every caller has already established the list is
--   non-empty from the fixture's own declaration.
firstOf ∷ [α] → α
firstOf = fromMaybe (error "fixture list was unexpectedly empty") ∘ listToMaybe
