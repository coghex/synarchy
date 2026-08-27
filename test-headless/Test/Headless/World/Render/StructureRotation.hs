{-# LANGUAGE Strict #-}
-- | Pure tests for #1712: a structure wall's IDENTITY — its sprite, its
--   cap facemap, whether it takes the #415 depth-strip path, its sort
--   anchors and tie-breaks — rotating with the camera, and a corner post
--   staying on its own physical corner while doing so.
--
--   The fixture is deliberately ASYMMETRIC, because symmetric geometry
--   passes a wrong rotation by construction. One tile carries all four
--   wall edges, a ceiling and all four posts; every wall has its own
--   texture handle, its own sixteen cap-facemap handles, and an UNEQUAL
--   cap pair (@"10"@ — canvas-left capped, canvas-right not), so a
--   mapping that is off by 90°, 180° or that loses the endpoint order
--   produces a handle no expectation names.
--
--   Every expectation below is written out INDEPENDENTLY of the
--   production tables — the authored-edge → screen-edge permutation, the
--   corner permutation, the cap-swap set, the tile-vertex offsets and the
--   iso depth function are all restated here from the issue's derivation,
--   so an edit to either side has to be made twice to pass.
module Test.Headless.World.Render.StructureRotation (spec) where

import UPrelude
import Test.Hspec
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import Engine.Asset.Handle (TextureHandle(..))
import Engine.Graphics.Camera (CameraFacing(..))
import Engine.Graphics.Vulkan.Types.Vertex (Vertex(..), Vec2(..))
import Engine.Scene.Types (SortableQuad(..))
import Structure.Facing
    ( WallEdge(..), PostCorner(..), WallCaps(..), wallCapsCode )
import Structure.Palette (TexPalette, emptyTexPalette, internPath)
import Structure.Render (structurePieceQuads)
import Structure.Types (StructureSlot(..), StructurePieceData(..))
import Structure.WallCatalog
    ( StructureWallCatalog, WallArtEntry(..)
    , emptyStructureWallCatalog, registerWallFamily, rotatedWallArt )

-- * Independent restatement of the rotation contract

allFacings ∷ [CameraFacing]
allFacings = [FaceSouth, FaceWest, FaceNorth, FaceEast]

-- | Authored world edge → the screen edge it occupies (issue #1712's
--   pinned table, derived from 'World.Grid.applyFacing').
expectScreenEdge ∷ CameraFacing → WallEdge → WallEdge
expectScreenEdge FaceSouth e = e
expectScreenEdge FaceWest  e = case e of
    WallNE → WallNW ; WallNW → WallSW ; WallSE → WallNE ; WallSW → WallSE
expectScreenEdge FaceNorth e = case e of
    WallNE → WallSW ; WallNW → WallSE ; WallSE → WallNW ; WallSW → WallNE
expectScreenEdge FaceEast  e = case e of
    WallNE → WallSE ; WallNW → WallNE ; WallSE → WallSW ; WallSW → WallNW

-- | Authored world corner → the screen corner it occupies.
expectScreenCorner ∷ CameraFacing → PostCorner → PostCorner
expectScreenCorner FaceSouth c = c
expectScreenCorner FaceWest  c = case c of
    CornerN → CornerW ; CornerE → CornerN ; CornerS → CornerE ; CornerW → CornerS
expectScreenCorner FaceNorth c = case c of
    CornerN → CornerS ; CornerE → CornerW ; CornerS → CornerN ; CornerW → CornerE
expectScreenCorner FaceEast  c = case c of
    CornerN → CornerE ; CornerE → CornerS ; CornerS → CornerW ; CornerW → CornerN

-- | Which PHYSICAL corner is drawn at a screen corner — the inverse of
--   the table above, written out rather than computed from it.
expectPhysCorner ∷ CameraFacing → PostCorner → PostCorner
expectPhysCorner FaceSouth c = c
expectPhysCorner FaceWest  c = case c of
    CornerW → CornerN ; CornerN → CornerE ; CornerE → CornerS ; CornerS → CornerW
expectPhysCorner FaceNorth c = case c of
    CornerS → CornerN ; CornerW → CornerE ; CornerN → CornerS ; CornerE → CornerW
expectPhysCorner FaceEast  c = case c of
    CornerE → CornerN ; CornerS → CornerE ; CornerW → CornerS ; CornerN → CornerW

-- | The (facing, authored edge) pairs whose target screen edge REVERSES
--   the wall's endpoint order, so the @"<left><right>"@ cap suffix has
--   to be swapped. Hand-derived from the two tables above and the
--   canvas-end ordering @ne=(n,e) nw=(w,n) se=(s,e) sw=(w,s)@.
expectCapSwap ∷ CameraFacing → WallEdge → Bool
expectCapSwap FaceSouth _ = False
expectCapSwap FaceWest  e = e ≡ WallNW ∨ e ≡ WallSE
expectCapSwap FaceNorth _ = True
expectCapSwap FaceEast  e = e ≡ WallNE ∨ e ≡ WallSW

-- | Iso painter depth of a grid point — @a + b@ of
--   'World.Grid.applyFacing', restated.
depthAt ∷ CameraFacing → (Float, Float) → Float
depthAt FaceSouth (x, y) = x + y
depthAt FaceWest  (x, y) = y - x
depthAt FaceNorth (x, y) = negate (x + y)
depthAt FaceEast  (x, y) = x - y

-- | The canvas-x span a wall sprite's art occupies. The N and S vertices
--   sit at canvas centre (u 0.5), W at 0 and E at 1.
screenEdgeSpan ∷ WallEdge → (Float, Float)
screenEdgeSpan e = case e of
    WallNW → (0, 0.5) ; WallSW → (0, 0.5) ; WallNE → (0.5, 1) ; WallSE → (0.5, 1)

-- | The per-screen-edge sort tie-break, restated from the original
--   per-slot table (SE 0.0006, SW 0.0005, NE 0.0004, NW 0.0003).
expectTieBreak ∷ WallEdge → Float
expectTieBreak e = case e of
    WallNW → 0.00030 ; WallNE → 0.00040 ; WallSW → 0.00050 ; WallSE → 0.00060

isFrontScreenEdge ∷ WallEdge → Bool
isFrontScreenEdge e = e ≡ WallSE ∨ e ≡ WallSW

-- * The asymmetric fixture

allEdges ∷ [WallEdge]
allEdges = [WallNE, WallNW, WallSE, WallSW]

allCorners ∷ [PostCorner]
allCorners = [CornerN, CornerE, CornerS, CornerW]

allCaps ∷ [WallCaps]
allCaps = [ WallCaps l r | l ← [False, True], r ← [False, True] ]

edgeName ∷ WallEdge → Text
edgeName e = case e of
    WallNE → "ne" ; WallNW → "nw" ; WallSE → "se" ; WallSW → "sw"

wallSlot ∷ WallEdge → StructureSlot
wallSlot e = case e of
    WallNE → SWallNE ; WallNW → SWallNW ; WallSE → SWallSE ; WallSW → SWallSW

postSlot ∷ PostCorner → StructureSlot
postSlot c = case c of
    CornerN → SPostN ; CornerE → SPostE ; CornerS → SPostS ; CornerW → SPostW

texPath ∷ WallEdge → Text
texPath e = "pack/wall_" <> edgeName e <> ".png"

facePath ∷ WallEdge → WallCaps → Text
facePath e c = "pack/wallface_" <> edgeName e <> "_" <> wallCapsCode c <> ".png"

-- | Distinct, individually recognisable handles: one per sprite and one
--   per (edge, cap) facemap. Nothing collides, so a wrong pick is a
--   wrong number rather than an accidental match.
texHandle ∷ WallEdge → TextureHandle
texHandle e = TextureHandle (100 + edgeIx e)

faceHandle ∷ WallEdge → WallCaps → TextureHandle
faceHandle e c = TextureHandle (200 + edgeIx e * 4 + capIx c)

edgeIx ∷ WallEdge → Int
edgeIx e = case e of
    WallNE → 0 ; WallNW → 1 ; WallSE → 2 ; WallSW → 3

capIx ∷ WallCaps → Int
capIx (WallCaps l r) = (if l then 2 else 0) + (if r then 1 else 0)

-- | Non-wall art, outside every registered family.
floorTex, floorFace, ceilTex, ceilFace, postTex, postFace ∷ TextureHandle
floorTex  = TextureHandle 300 ; floorFace = TextureHandle 301
ceilTex   = TextureHandle 302 ; ceilFace  = TextureHandle 303
postTex   = TextureHandle 304 ; postFace  = TextureHandle 305

familyEntries ∷ [WallArtEntry]
familyEntries =
    [ WallArtEntry e Nothing (texPath e) (texHandle e) | e ← allEdges ]
    <> [ WallArtEntry e (Just c) (facePath e c) (faceHandle e c)
       | e ← allEdges, c ← allCaps ]

catalog ∷ StructureWallCatalog
catalog = fromMaybe (error "fixture family is incomplete")
                    (registerWallFamily familyEntries emptyStructureWallCatalog)

-- | Every fixture path interned, plus the non-wall art, so
--   'structurePieceQuads' can resolve each piece's ids back to paths.
paletteAndIds ∷ (TexPalette, HM.HashMap Text Int)
paletteAndIds = foldl' step (emptyTexPalette, HM.empty) allPaths
  where
    step (pal, m) p = let (i, pal') = internPath p pal in (pal', HM.insert p i m)
    allPaths = [ texPath e | e ← allEdges ]
            <> [ facePath e c | e ← allEdges, c ← allCaps ]
            <> [ "pack/floor.png", "pack/floorface.png"
               , "pack/ceiling.png", "pack/ceilingface.png"
               , "pack/post.png", "pack/postface.png" ]

palette ∷ TexPalette
palette = fst paletteAndIds

pathId ∷ Text → Int
pathId p = fromMaybe (error ("unregistered fixture path " <> T.unpack p))
                     (HM.lookup p (snd paletteAndIds))

-- | palette id → runtime handle, exactly as the engine's translation
--   table would hold it after placement.
handleTable ∷ HM.HashMap Int TextureHandle
handleTable = HM.fromList $
    [ (pathId (texPath e), texHandle e) | e ← allEdges ]
    <> [ (pathId (facePath e c), faceHandle e c) | e ← allEdges, c ← allCaps ]
    <> [ (pathId "pack/floor.png",     floorTex)
       , (pathId "pack/floorface.png", floorFace)
       , (pathId "pack/ceiling.png",   ceilTex)
       , (pathId "pack/ceilingface.png", ceilFace)
       , (pathId "pack/post.png",      postTex)
       , (pathId "pack/postface.png",  postFace) ]

-- | Every wall is placed with the SAME unequal cap pair, so a lost
--   endpoint order shows up as a facemap handle from the wrong cap.
placedCaps ∷ WallCaps
placedCaps = WallCaps True False

-- | The fixture tile and z. z ≡ zSlice keeps the sort key's z-term at
--   zero, so an expected key is depth + tie-break alone.
fixGX, fixGY, fixZ ∷ Int
fixGX = 3 ; fixGY = 5 ; fixZ = 7

piece ∷ Text → Text → StructurePieceData
piece t f = StructurePieceData (pathId t) (pathId f) fixZ

wallPiece ∷ WallEdge → StructurePieceData
wallPiece e = piece (texPath e) (facePath e placedCaps)

-- | Run the production per-piece pipeline for one slot at one facing.
quadsFor ∷ CameraFacing → StructureSlot → StructurePieceData → [SortableQuad]
quadsFor facing slot spd =
    structurePieceQuads catalog palette handleTable slotOf HM.empty
                        facing fixZ 8 1.0 fixGX fixGY slot spd
  where
    -- Identity bindless mapping: a quad's atlas/facemap payload then
    -- carries the handle number itself, which is what the assertions read.
    slotOf (TextureHandle h) = fromIntegral h ∷ Word32

wallQuads ∷ CameraFacing → WallEdge → [SortableQuad]
wallQuads facing e = quadsFor facing (wallSlot e) (wallPiece e)

-- | The expected rotated art for a wall on authored edge @e@.
expectedArt ∷ CameraFacing → WallEdge → (TextureHandle, TextureHandle)
expectedArt facing e =
    let s = expectScreenEdge facing e
        WallCaps l r = placedCaps
        caps = if expectCapSwap facing e then WallCaps r l else WallCaps l r
    in (texHandle s, faceHandle s caps)

-- | The tile's own painter depth — what the TERRAIN pass keys this tile
--   at ('World.Render.TileQuads' sorts on @applyFacing facing gx gy@),
--   and the origin every structure anchor below is measured forward from.
tileDepth ∷ CameraFacing → Float
tileDepth facing = depthAt facing (fromIntegral fixGX, fromIntegral fixGY)

-- | Expected first and last strip sort keys of a screen-front wall.
--   The strips' UV-x follows the SPRITE, and their depth runs from ONE
--   step in front of the tile (the outer end of the edge) to TWO (the end
--   at the sprite's canvas centre, which is the screen-bottom corner) —
--   the same two steps at every facing, so the two ends are not
--   interchangeable and a reversed interpolation shows up here.
expectedStripEnds ∷ CameraFacing → WallEdge → (Float, Float)
expectedStripEnds facing e =
    let s        = expectScreenEdge facing e
        (u0, u1) = screenEdgeSpan s
        (kL, kR) = if u1 ≡ 0.5 then (1, 2) else (2, 1)
        keyAt i  = let uc = (fromIntegral (i ∷ Int) + 0.5) / 16
                       t  = clamp01 ((uc - u0) / (u1 - u0))
                   in tileDepth facing + kL + t * (kR - kL) + expectTieBreak s
    in (keyAt 0, keyAt 15)

-- | Total accessors: hspec reports the message rather than a partial
--   pattern's exception if a fixture ever stops emitting quads.
firstQuad, lastQuad ∷ [SortableQuad] → SortableQuad
firstQuad (q : _) = q
firstQuad []      = error "fixture emitted no quads"
lastQuad qs = case reverse qs of
    (q : _) → q
    []      → error "fixture emitted no quads"

faceMapOf ∷ SortableQuad → TextureHandle
faceMapOf q = TextureHandle (round (faceMapId (sqV0 q)))

topLeft ∷ SortableQuad → (Float, Float)
topLeft q = let Vec2 px py = pos (sqV0 q) in (px, py)

near ∷ Float → Float → Bool
near a b = abs (a - b) < 1.0e-4

spec ∷ Spec
spec = do
    describe "the authored-edge → screen-edge mapping" $ do
        it "is the identity at FaceSouth, a 4-cycle otherwise" $
            forM_ allFacings $ \facing → forM_ allEdges $ \e →
                fst (expectedArt facing e) `shouldBe`
                    texHandle (expectScreenEdge facing e)

        it "resolves a registered wall's art to its screen edge's sprite" $
            forM_ allFacings $ \facing → forM_ allEdges $ \e →
                rotatedWallArt catalog facing e
                    (texPath e) (facePath e placedCaps)
                    `shouldBe` Just (expectedArt facing e)

        it "keeps a wall's texture and cap facemap on the SAME edge" $
            forM_ allFacings $ \facing → forM_ allEdges $ \e → do
                let s = expectScreenEdge facing e
                rotatedWallArt catalog facing e (texPath e) (facePath e placedCaps)
                    `shouldSatisfy` \r → case r of
                        Just (th, fh) →
                            th ≡ texHandle s
                            ∧ (fh ≡ faceHandle s (WallCaps True False)
                               ∨ fh ≡ faceHandle s (WallCaps False True))
                        Nothing → False

        it "preserves which PHYSICAL end is capped, re-encoding the order" $
            forM_ allFacings $ \facing → forM_ allEdges $ \e → do
                let s = expectScreenEdge facing e
                    expectCaps | expectCapSwap facing e = WallCaps False True
                               | otherwise              = WallCaps True False
                snd (expectedArt facing e) `shouldBe` faceHandle s expectCaps
                rotatedWallArt catalog facing e (texPath e) (facePath e placedCaps)
                    `shouldBe` Just (texHandle s, faceHandle s expectCaps)

        it "refuses a texture/facemap pair from DIFFERENT authored edges" $
            -- Never a sprite from one direction with a mask from another:
            -- the mismatched pair rotates not at all.
            rotatedWallArt catalog FaceWest WallNE
                (texPath WallNE) (facePath WallSW placedCaps)
                `shouldBe` Nothing

        it "leaves art from outside any registered pack exactly as placed" $
            rotatedWallArt catalog FaceWest WallNE
                "somewhere/else.png" (facePath WallNE placedCaps)
                `shouldBe` Nothing

    describe "FaceSouth is unchanged" $ do
        it "resolves every wall back to the art it was placed with" $
            forM_ allEdges $ \e →
                rotatedWallArt catalog FaceSouth e
                    (texPath e) (facePath e placedCaps)
                    `shouldBe` Just (texHandle e, faceHandle e placedCaps)

        it "strips exactly the authored SE/SW pair, single-quads NE/NW" $ do
            length (wallQuads FaceSouth WallSE) `shouldBe` 16
            length (wallQuads FaceSouth WallSW) `shouldBe` 16
            length (wallQuads FaceSouth WallNE) `shouldBe` 1
            length (wallQuads FaceSouth WallNW) `shouldBe` 1

        it "keeps the original per-slot tie-breaks and back-wall anchor" $ do
            -- Back walls anchor at the tile centre (depth gx+gy+1) and
            -- carry NW 0.0003 / NE 0.0004.
            let key e = sqSortKey (firstQuad (wallQuads FaceSouth e))
                centre = fromIntegral fixGX + fromIntegral fixGY + 1.0 ∷ Float
            key WallNW `shouldSatisfy` near (centre + 0.00030)
            key WallNE `shouldSatisfy` near (centre + 0.00040)

        it "keeps the ceiling on the front corner, over every wall" $ do
            let ceilKey = sqSortKey (firstQuad (quadsFor FaceSouth SCeiling
                              (piece "pack/ceiling.png" "pack/ceilingface.png")))
                front   = fromIntegral fixGX + fromIntegral fixGY + 2.0 ∷ Float
            ceilKey `shouldSatisfy` near (front + 0.00070)

    describe "at every facing" $ forM_ allFacings $ \facing →
        describe (show facing) $ do
            it "strips exactly the SCREEN-front pair" $
                forM_ allEdges $ \e →
                    length (wallQuads facing e) `shouldBe`
                        (if isFrontScreenEdge (expectScreenEdge facing e)
                         then 16 else 1)

            it "emits the rotated texture and facemap handles" $
                forM_ allEdges $ \e → do
                    let qs = wallQuads facing e
                        (eTex, eFace) = expectedArt facing e
                    forM_ qs $ \q → do
                        sqTexture q `shouldBe` eTex
                        faceMapOf q `shouldBe` eFace

            it "anchors a front wall's first and last strip on its own \
               \physical endpoints" $
                forM_ [ e | e ← allEdges
                          , isFrontScreenEdge (expectScreenEdge facing e) ] $ \e → do
                    let qs = wallQuads facing e
                        (eFirst, eLast) = expectedStripEnds facing e
                    sqSortKey (firstQuad qs) `shouldSatisfy` near eFirst
                    sqSortKey (lastQuad qs)  `shouldSatisfy` near eLast
                    -- A collapsed or reversed interpolation would make
                    -- these equal or swap them (#417).
                    eFirst `shouldNotSatisfy` near eLast

            it "keeps every piece a FIXED number of depth steps in front \
               \of its own tile's terrain key" $ do
                -- The regression the four-facing offscreen capture caught:
                -- a fixed grid offset rotated by applyFacingF changes its
                -- gap to the terrain pass (+1 at FaceSouth, 0 at
                -- FaceWest/FaceEast, −1 at FaceNorth), which sinks a
                -- rotated floor under its own terrain tile. The gap must
                -- not move.
                let base = tileDepth facing
                    keyOf slot spd = sqSortKey (firstQuad (quadsFor facing slot spd))
                    floorKey = keyOf SFloor
                                   (piece "pack/floor.png" "pack/floorface.png")
                    ceilKey  = keyOf SCeiling
                                   (piece "pack/ceiling.png" "pack/ceilingface.png")
                floorKey `shouldSatisfy` near (base + 1 + 0.00020)
                ceilKey  `shouldSatisfy` near (base + 2 + 0.00070)
                forM_ [ e | e ← allEdges
                          , not (isFrontScreenEdge (expectScreenEdge facing e)) ] $ \e →
                    sqSortKey (firstQuad (wallQuads facing e)) `shouldSatisfy`
                        near (base + 1 + expectTieBreak (expectScreenEdge facing e))

            it "sorts the screen-front pair over the screen-back pair, and \
               \the ceiling over all four" $ do
                let maxKey e = maximum (map sqSortKey (wallQuads facing e))
                    fronts = [ maxKey e | e ← allEdges
                             , isFrontScreenEdge (expectScreenEdge facing e) ]
                    backs  = [ maxKey e | e ← allEdges
                             , not (isFrontScreenEdge (expectScreenEdge facing e)) ]
                    ceilKey = sqSortKey (firstQuad (quadsFor facing SCeiling
                                  (piece "pack/ceiling.png" "pack/ceilingface.png")))
                minimum fronts `shouldSatisfy` (> maximum backs)
                ceilKey `shouldSatisfy` (> maximum fronts)

            it "stands each post on the physical corner it still occupies" $ do
                -- Offset from the floor quad isolates the post's canvas
                -- placement from the tile's own screen position, so it can
                -- be compared straight across facings: the post authored at
                -- corner C sits where the post authored at C's SCREEN corner
                -- sits at FaceSouth.
                let floorAt f = topLeft (firstQuad (quadsFor f SFloor
                                    (piece "pack/floor.png" "pack/floorface.png")))
                    postAt f c = let (px, py) = topLeft (firstQuad (quadsFor f (postSlot c)
                                       (piece "pack/post.png" "pack/postface.png")))
                                     (fx, fy) = floorAt f
                                 in (px - fx, py - fy)
                forM_ allCorners $ \c → do
                    let (ax, ay) = postAt facing c
                        (bx, by) = postAt FaceSouth (expectScreenCorner facing c)
                    ax `shouldSatisfy` near bx
                    ay `shouldSatisfy` near by

            it "sorts the screen-front post ahead of the front walls and the \
               \screen-back post behind the back walls" $ do
                let postKey c = sqSortKey (firstQuad (quadsFor facing (postSlot c)
                                    (piece "pack/post.png" "pack/postface.png")))
                    -- The authored corners currently drawn at screen S / N.
                    frontPost = postKey (expectPhysCorner facing CornerS)
                    backPost  = postKey (expectPhysCorner facing CornerN)
                    maxKey e  = maximum (map sqSortKey (wallQuads facing e))
                    fronts = [ maxKey e | e ← allEdges
                             , isFrontScreenEdge (expectScreenEdge facing e) ]
                    backs  = [ maxKey e | e ← allEdges
                             , not (isFrontScreenEdge (expectScreenEdge facing e)) ]
                frontPost `shouldSatisfy` (> maximum fronts)
                backPost  `shouldSatisfy` (< minimum backs)

    describe "unresolved art" $
        it "emits nothing until both palette ids have a runtime handle" $
            structurePieceQuads catalog palette HM.empty
                (\(TextureHandle h) → fromIntegral h) HM.empty
                FaceSouth fixZ 8 1.0 fixGX fixGY SWallSE (wallPiece WallSE)
                `shouldSatisfy` null
