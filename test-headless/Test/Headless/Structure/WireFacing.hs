{-# LANGUAGE OverloadedStrings #-}
-- | Wire art follows the camera (#2901).
--
--   A wire sprite is a fixed picture whose spokes run to fixed screen
--   positions, so choosing it from the WORLD neighbours left a run
--   drawn as disconnected segments at every facing but 'FaceSouth'.
--   The sprite is now chosen from the SCREEN connections
--   ('Structure.Facing.screenWireShape'); the stored shape stays
--   world-relative.
--
--   Three groups, all GPU-free:
--
--     * @"Wire screen facing"@ — the conversion itself, as a table over
--       all sixteen shapes at all four facings. Its oracle is NOT the
--       implementation's own permutation: it projects each neighbour
--       offset through 'World.Grid.applyFacing' and the iso screen
--       formula and asks which 'FaceSouth' side lands on the same screen
--       delta, so a wrong table fails against the geometry the camera
--       really uses.
--     * @"Wire placed and destroyed facing"@ — the committed-piece and
--       destruction-clip quad producers at every facing, current and
--       pre-#2516 palette pairs, and a camera turn mid-playback.
--     * the ghost, designation, preview and construction paths are in
--       the @"structure ghost"@ group (@wireFacingSpec@).
--
--   Run: @cabal test synarchy-test-headless --test-options='--match "Wire screen facing"'@
--   and the same with @"Wire placed and destroyed facing"@.
module Test.Headless.Structure.WireFacing
    ( spec
    , screenSideOracle
    , expectedScreenNeighbors
    ) where

import UPrelude
import Test.Hspec
import qualified Data.HashMap.Strict as HM
import qualified Data.List as L
import qualified Data.Text as T
import qualified Data.Vector as V

import Engine.Asset.Handle (TextureHandle, toInt)
import Engine.Graphics.Camera (CameraFacing(..), Camera2D(..), defaultCamera)
import Engine.Graphics.Vulkan.Types.Vertex
import Engine.Scene.Types (SortableQuad(..))
import Structure.ArtCatalog
import Structure.Destruction
import Structure.Facing
    (screenWireNeighbors, screenWireShape)
import Structure.Palette (TexPalette, emptyTexPalette, internPath, lookupPath)
import Structure.Render
    (noPlacedArtOverride, structurePieceQuads, structurePieceQuadsWith)
import Structure.Types (StructurePieceData(..), StructureSlot(..))
import Structure.WallCatalog (emptyStructureWallCatalog)
import Structure.Wire
import World.Chunk.Types (chunkSize)
import World.Generate (viewDepth)
import World.Grid (applyFacing, gridToWorld)
import World.Page.Types (WorldPageId(..))
import World.Render.StructureDestruction (structureDestructionQuads)
import World.Render.ViewBounds (computeViewBounds)

import Test.Headless.Render.StructureGhostFixture
    ( PackFixture(..), handleForPath, loadShippedPacks, packCatalog )

allFacings ∷ [CameraFacing]
allFacings = [FaceSouth, FaceWest, FaceNorth, FaceEast]

-- * The oracle

-- | The four cardinal sides in the engine's neighbour order, as the
--   grid offset each one is (@World.Slope@: N = gy-1, E = gx+1,
--   S = gy+1, W = gx-1).
sides ∷ [(Text, (Int, Int))]
sides = [("n", (0, -1)), ("e", (1, 0)), ("s", (0, 1)), ("w", (-1, 0))]

-- | Where a neighbour offset lands on screen at a facing: the iso
--   projection of 'applyFacing' (@sx = a-b@, @sy = a+b@), the very
--   formula 'World.Grid.gridToScreen' uses.
screenDelta ∷ CameraFacing → (Int, Int) → (Int, Int)
screenDelta facing (dx, dy) = let (a, b) = applyFacing facing dx dy in (a - b, a + b)

-- | The side a world side is DRAWN on at a facing: the 'FaceSouth' side
--   whose screen delta equals the one the camera actually produces.
screenSideOracle ∷ CameraFacing → Text → Text
screenSideOracle facing side = case
        [ n | (n, off) ← sides
            , screenDelta FaceSouth off ≡ screenDelta facing (offOf side) ] of
    [n] → n
    other → error ("no unique screen side for " <> T.unpack side <> ": " <> show other)
  where offOf s = fromMaybe (error "bad side") (lookup s sides)

-- | The screen connections the oracle expects for a world neighbour
--   set.
expectedScreenNeighbors ∷ CameraFacing → WireNeighbors → WireNeighbors
expectedScreenNeighbors facing (WireNeighbors n e s w) = WireNeighbors
    (lit "n") (lit "e") (lit "s") (lit "w")
  where
    worldSet = [ side | (side, True) ← zip ["n", "e", "s", "w"] [n, e, s, w] ]
    drawn = map (screenSideOracle facing) worldSet
    lit side = side `elem` drawn

neighborsList ∷ WireNeighbors → [Bool]
neighborsList (WireNeighbors n e s w) = [n, e, s, w]

-- * Spec

spec ∷ Spec
spec = do
    screenSpec
    beforeAll loadShippedPacks placedSpec

screenSpec ∷ Spec
screenSpec = describe "Wire screen facing" $ do
    it "the oracle itself reproduces the issue's FaceWest table \
       \(N→W, E→N, S→E, W→S) and is the identity at FaceSouth" $ do
        map (screenSideOracle FaceWest) ["n", "e", "s", "w"]
            `shouldBe` ["w", "n", "e", "s"]
        map (screenSideOracle FaceNorth) ["n", "e", "s", "w"]
            `shouldBe` ["s", "w", "n", "e"]
        map (screenSideOracle FaceEast) ["n", "e", "s", "w"]
            `shouldBe` ["e", "s", "w", "n"]
        map (screenSideOracle FaceSouth) ["n", "e", "s", "w"]
            `shouldBe` ["n", "e", "s", "w"]

    it "draws, for all 16 world shapes × 4 facings, the variant whose \
       \screen spokes are the facing-rotated world connections" $
        forM_ allWireShapes $ \shape → forM_ allFacings $ \facing → do
            let world = must (wireShapeNeighbors shape)
                drawn = screenWireShape facing shape
                spokes = must (wireShapeNeighbors drawn)
            (shape, facing, spokes)
                `shouldBe` (shape, facing, expectedScreenNeighbors facing world)

    it "FaceSouth is the identity on every shape, risers included" $
        forM_ allWireArtShapes $ \shape →
            screenWireShape FaceSouth shape `shouldBe` shape

    it "keeps the connection COUNT and is a bijection at every facing" $
        forM_ allFacings $ \facing → do
            let drawn = map (screenWireShape facing) allWireShapes
            L.sort drawn `shouldBe` L.sort allWireShapes
            forM_ allWireShapes $ \shape →
                length (filter id (neighborsList (must (wireShapeNeighbors
                                        (screenWireShape facing shape)))))
                    `shouldBe` length (filter id (neighborsList (must
                                        (wireShapeNeighbors shape))))

    it "turning the camera a quarter at a time composes: West∘West is \
       \North, West∘North is East" $
        forM_ allWireShapes $ \shape → do
            screenWireShape FaceWest (screenWireShape FaceWest shape)
                `shouldBe` screenWireShape FaceNorth shape
            screenWireShape FaceWest (screenWireShape FaceNorth shape)
                `shouldBe` screenWireShape FaceEast shape

    it "leaves the four risers alone (declaration-only; they need the same \
       \mapping once something places them)" $
        forM_ allWireRisers $ \r → forM_ allFacings $ \facing →
            screenWireShape facing r `shouldBe` r

    it "keeps a joint continuous: for two adjacent tiles, the spoke each \
       \draws toward the other meets the other's at every facing" $
        forM_ allFacings $ \facing → forM_ (map fst sides) $ \side → do
            -- tile A reaches its neighbour on `side`; the neighbour
            -- reaches back on the opposite world side.
            let opposite = case side of
                    "n" → "s"; "s" → "n"; "e" → "w"; _ → "e"
                aSpoke = screenSideOracle facing side
                bSpoke = screenSideOracle facing opposite
                -- the two screen deltas are negatives of each other, so
                -- the drawn spokes point at each other
                neg (x, y) = (negate x, negate y)
            screenDelta FaceSouth (offOfSide aSpoke)
                `shouldBe` neg (screenDelta FaceSouth (offOfSide bSpoke))
            -- and the implementation draws exactly those spokes
            let only s = case s of
                    "n" → WireNeighbors True False False False
                    "e" → WireNeighbors False True False False
                    "s" → WireNeighbors False False True False
                    _   → WireNeighbors False False False True
            screenWireNeighbors facing (only side) `shouldBe` only aSpoke
            screenWireNeighbors facing (only opposite) `shouldBe` only bSpoke
  where
    offOfSide s = fromMaybe (error "bad side") (lookup s sides)

-- * Placed pieces and destruction clips

worldSize, surfaceZ, zSlice, pieceZ ∷ Int
worldSize = 64
surfaceZ  = 10
zSlice    = 14
pieceZ    = surfaceZ + 1

homeTile ∷ (Int, Int)
homeTile = (4 * chunkSize + 3, 4 * chunkSize + 3)

zoom, tileAlpha ∷ Float
zoom = 4.0
tileAlpha = 0.8

effDepth ∷ Int
effDepth = min viewDepth (max 8 (round (zoom * 80.0 + 8.0 ∷ Float)))

colourPath, facePath ∷ WireShape → Text
colourPath s = "assets/textures/structures/wire/" <> wireShapeName s <> ".png"
facePath   s = "assets/textures/facemap/wire/" <> wireShapeName s <> ".png"

legacyFace ∷ Text
legacyFace = "assets/textures/facemap/floorface.png"

lookupSlotId ∷ TextureHandle → Word32
lookupSlotId = fromIntegral ∘ toInt

slotOf ∷ Text → Float
slotOf = fromIntegral ∘ toInt ∘ handleForPath

faceIds ∷ [SortableQuad] → [Float]
faceIds = L.nub ∘ map faceMapId ∘ concatMap quadVerts

quadVerts ∷ SortableQuad → [Vertex]
quadVerts sq = [sqV0 sq, sqV1 sq, sqV2 sq, sqV3 sq]

must ∷ Maybe α → α
must = fromMaybe (error "unexpected Nothing in a fixture lookup")

wirePack ∷ [PackFixture] → StructureArtCatalog
wirePack packs = packCatalog [ p | p ← packs, pfName p ≡ "wire" ]

-- | A placed wire of @shape@ stored with the given facemap path, plus
--   the palette and live-handle map a placement would have produced.
--   The live texture handle is deliberately NOT the catalogue's, so an
--   example can tell "kept what was placed" from "took the catalogue's".
storedPiece ∷ WireShape → Text
            → (StructurePieceData, TexPalette, HM.HashMap Int TextureHandle)
storedPiece shape face =
    ( StructurePieceData tid fid pieceZ, pal
    , HM.fromList [ (tid, handleForPath ("live/" <> colourPath shape))
                  , (fid, handleForPath face) ] )
  where
    (tid, pal0) = internPath (colourPath shape) emptyTexPalette
    (fid, pal)  = internPath face pal0

placedAt ∷ (Text → Maybe PieceArt) → CameraFacing
         → (StructurePieceData, TexPalette, HM.HashMap Int TextureHandle)
         → [SortableQuad]
placedAt override facing (spd, pal, handles) =
    structurePieceQuadsWith override emptyStructureWallCatalog pal handles
        lookupSlotId HM.empty facing zSlice effDepth tileAlpha
        (fst homeTile) (snd homeTile) SWire spd

placedSpec ∷ SpecWith [PackFixture]
placedSpec = describe "Wire placed and destroyed facing" $ do
    it "resolves a placed wire's drawn art to the SCREEN variant's own \
       \colour and facemap, for all 16 shapes × 4 facings" $ \packs → do
        let cat = wirePack packs
        forM_ allWireShapes $ \shape → forM_ allFacings $ \facing → do
            let want = screenWireShape facing shape
                got = drawnWireArt cat facing (colourPath shape)
            (shape, facing, aaPath ∘ paTexture ⊚ got, aaPath ∘ paFacemap ⊚ got)
                `shouldBe` (shape, facing, Just (colourPath want), Just (facePath want))

    it "at FaceSouth is exactly the registered art of the stored shape" $ \packs → do
        let cat = wirePack packs
        forM_ allWireShapes $ \shape →
            drawnWireArt cat FaceSouth (colourPath shape)
                `shouldBe` placedWireArt cat (colourPath shape)

    it "answers nothing for a non-wire sprite" $ \packs → do
        let cat = wirePack packs
        drawnWireArt cat FaceWest "assets/other.png" `shouldBe` Nothing

    forM_ [ ("a current-palette", facePath)
          , ("a pre-#2516 (shared floor facemap)", const legacyFace) ] $
      \(label, faceFor) →
        it ("draws " <> label <> " placed wire as the screen variant — its \
            \sprite AND facemap together — at every facing, rewriting nothing") $
          \packs → do
            let cat = wirePack packs
            forM_ allWireShapes $ \shape → forM_ allFacings $ \facing → do
                let stored@(spd, pal, _) = storedPiece shape (faceFor shape)
                    want = screenWireShape facing shape
                    quads = placedAt (drawnWireArt cat facing) facing stored
                -- the screen variant's sprite and its own facemap
                L.nub (map sqTexture quads) `shouldBe`
                    [ if want ≡ shape
                        then handleForPath ("live/" <> colourPath shape)
                        else handleForPath (colourPath want) ]
                faceIds quads `shouldBe` [slotOf (facePath want)]
                -- the stored record and palette are what they were
                lookupPath (spdTexId spd) pal `shouldBe` Just (colourPath shape)
                lookupPath (spdFaceId spd) pal `shouldBe` Just (faceFor shape)

    it "leaves a FaceSouth piece bit-for-bit as before: same quads as the \
       \facing-blind override" $ \packs → do
        let cat = wirePack packs
        forM_ allWireShapes $ \shape → do
            let stored = storedPiece shape legacyFace
                shapeOfQuads = map (\q → (sqSortKey q, toInt (sqTexture q)
                                         , map faceMapId (quadVerts q)))
            shapeOfQuads (placedAt (drawnWireArt cat FaceSouth) FaceSouth stored)
                `shouldBe` shapeOfQuads (placedAt (placedWireArt cat) FaceSouth stored)

    it "keeps the quad GEOMETRY and sort key facing-independent of the \
       \variant choice: only the sprite and facemap change" $ \packs → do
        let cat = wirePack packs
        forM_ allFacings $ \facing → forM_ allWireShapes $ \shape → do
            let stored = storedPiece shape (facePath shape)
                geometry = map (\q → (sqSortKey q, map (\v → (pos v, tex v))
                                                       (quadVerts q)))
            geometry (placedAt (drawnWireArt cat facing) facing stored)
                `shouldBe` geometry (placedAt noPlacedArtOverride facing stored)

    it "plays the screen variant's destruction clip from the CAPTURED \
       \appearance at every facing, with the same frame at any camera" $ \packs → do
        let cat = wirePack packs
        forM_ allWireShapes $ \shape → forM_ allFacings $ \facing →
          forM_ [0, 0.15, 0.31, 0.49] $ \now → do
            let (spd, pal, handles) = storedPiece shape (facePath shape)
                eff = case captureStructureDestruction cat pal handles 0
                               (WorldPageId "wire_facing") (fst homeTile)
                               (snd homeTile) (fromIntegral (fromEnum SWire)) spd of
                    CapturedEffect e → e
                    other → error ("wire captured nothing: " <> show other)
                want = screenWireShape facing shape
                ds = must (resolveDestructionSequence cat "wire"
                               (AppearanceKey Nothing (ApWire want)))
                idx = must (destructionEffectFrameIndex now eff)
                quads = effectQuads cat pal handles facing now eff
            -- captured as the WORLD shape, never rewritten
            apSlot (sdeAppearance eff) `shouldBe` ApWire shape
            L.nub (map sqTexture quads)
                `shouldBe` [aaHandle (dsFrames ds V.! idx)]
            faceIds quads `shouldBe`
                [fromIntegral (toInt (aaHandle
                    (must (dsFacemaps ds) V.! idx)))]

    it "does not reset or skip the clip when the camera turns mid-playback: \
       \the frame index depends on the clock alone" $ \packs → do
        let cat = wirePack packs
            shape = WireStraightNS
            (spd, pal, handles) = storedPiece shape (facePath shape)
            eff = case captureStructureDestruction cat pal handles 0
                           (WorldPageId "wire_facing") (fst homeTile)
                           (snd homeTile) (fromIntegral (fromEnum SWire)) spd of
                CapturedEffect e → e
                other → error ("wire captured nothing: " <> show other)
        forM_ [0, 0.12, 0.27, 0.41] $ \now → do
            let frameAt facing =
                    let ds = must (resolveDestructionSequence cat "wire"
                                    (AppearanceKey Nothing
                                        (ApWire (screenWireShape facing shape))))
                        qs = effectQuads cat pal handles facing now eff
                    in [ i | q ← take 1 qs
                           , (i, f) ← zip [0 ∷ Int ..] (V.toList (dsFrames ds))
                           , aaHandle f ≡ sqTexture q ]
            map frameAt allFacings `shouldSatisfy` \xs →
                length (L.nub xs) ≡ 1 ∧ all (not ∘ null) xs

    it "draws the placed wire through structurePieceQuads like the \
       \palette entry point at FaceSouth" $ \packs → do
        let cat = wirePack packs
            (spd, pal, handles) = storedPiece WireCross (facePath WireCross)
            direct = structurePieceQuads emptyStructureWallCatalog pal handles
                lookupSlotId HM.empty FaceSouth zSlice effDepth tileAlpha
                (fst homeTile) (snd homeTile) SWire spd
        length direct `shouldBe`
            length (placedAt (drawnWireArt cat FaceSouth)
                             FaceSouth (spd, pal, handles))
  where
    effectQuads cat _pal handles facing now eff =
        structureDestructionQuads cat emptyStructureWallCatalog handles
            lookupSlotId HM.empty facing zSlice effDepth tileAlpha worldSize
            (computeViewBounds (cameraAt facing) 800 600 effDepth)
            (fst (camPositionOf facing)) (snd (camPositionOf facing)) now [eff]
    cameraAt facing =
        let (wx, wy) = gridToWorld facing (fst homeTile) (snd homeTile)
        in defaultCamera { camPosition = (wx, wy), camZoom = zoom
                         , camFacing = facing, camZSlice = zSlice }
    camPositionOf = camPosition ∘ cameraAt
