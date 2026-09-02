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
    ( StructureWallCatalog(..), WallArtEntry(..)
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
    [ WallArtEntry e Nothing (texPath e) (texHandle e) True | e ← allEdges ]
    <> [ WallArtEntry e (Just c) (facePath e c) (faceHandle e c) True
       | e ← allEdges, c ← allCaps ]

catalog ∷ StructureWallCatalog
catalog = fromMaybe (error "fixture family is incomplete")
                    (registerWallFamily familyEntries emptyStructureWallCatalog)

-- * A PARTIAL variant, the shape a pack is explicitly allowed to author
--
--   @data/structure_packs/*.yaml@'s @variants@ may override any SUBSET of
--   the wall art; whatever it omits it INHERITS. This variant declares one
--   new sprite (NE) and inherits the other three plus every cap facemap,
--   so it shares fifteen of its twenty paths with the default family.

variantTexPath ∷ Text
variantTexPath = "pack/worn/wall_ne.png"

variantTexHandle ∷ TextureHandle
variantTexHandle = TextureHandle 400

-- | The variant registered SECOND, which is the order that used to let it
--   overwrite the default family's claim on the paths it merely inherits.
variantCatalog ∷ StructureWallCatalog
variantCatalog =
    fromMaybe (error "fixture variant family is incomplete")
              (registerWallFamily variantEntries catalog)
  where
    variantEntries =
        [ if e ≡ WallNE
            then WallArtEntry e Nothing variantTexPath variantTexHandle True
            else WallArtEntry e Nothing (texPath e) (texHandle e) False
        | e ← allEdges ]
        <> [ WallArtEntry e (Just c) (facePath e c) (faceHandle e c) False
           | e ← allEdges, c ← allCaps ]

-- | The nastier partial variant: it declares one SPRITE (NE) and one
--   edge's worth of cap FACEMAPS (NW), inheriting everything else. A wall
--   placed from it therefore stores its own NE sprite next to an
--   INHERITED NE facemap, so the sprite is the only thing naming the
--   variant — and rotating that wall onto screen NW has to reach the
--   variant's own NW masks, not the default's.
maskTexPath ∷ Text
maskTexPath = "pack/fancy/wall_ne.png"

maskTexHandle ∷ TextureHandle
maskTexHandle = TextureHandle 410

maskFacePath ∷ WallCaps → Text
maskFacePath c = "pack/fancy/face_nw_" <> wallCapsCode c <> ".png"

maskFaceHandle ∷ WallCaps → TextureHandle
maskFaceHandle c = TextureHandle (420 + capIx c)

maskCatalog ∷ StructureWallCatalog
maskCatalog =
    fromMaybe (error "fixture mask-variant family is incomplete")
              (registerWallFamily maskEntries catalog)
  where
    maskEntries =
        [ if e ≡ WallNE
            then WallArtEntry e Nothing maskTexPath maskTexHandle True
            else WallArtEntry e Nothing (texPath e) (texHandle e) False
        | e ← allEdges ]
        <> [ if e ≡ WallNW
               then WallArtEntry e (Just c) (maskFacePath c) (maskFaceHandle c) True
               else WallArtEntry e (Just c) (facePath e c) (faceHandle e c) False
           | e ← allEdges, c ← allCaps ]

-- * The ONE-HALF collision (#2160)
--
--   A second family that shares exactly ONE half of the default family's
--   NE art — either the sprite or the four cap facemaps — and authors the
--   other half, plus every other edge, for itself. A default NE wall is
--   then placed with one path the two families contest and one that names
--   the default alone, which is the state the pair-level reduction used to
--   resolve in the default's favour instead of refusing.
--
--   The shared entries carry the DEFAULT's handles, so the catalogue's
--   first-registration-wins handle table comes out identical whichever
--   order the two families are registered in.

-- | @shareTex@ picks which half is shared; @sharedOwned@ picks whether
--   this family DECLARES that shared path (contradictory pack data) or
--   merely INHERITS it (the ordinary partial-variant shape, which claims
--   nothing).
halfClashEntries ∷ Bool → Bool → [WallArtEntry]
halfClashEntries shareTex sharedOwned =
    [ if shareTex ∧ e ≡ WallNE
        then WallArtEntry e Nothing (texPath e) (texHandle e) sharedOwned
        else WallArtEntry e Nothing (ownTexPath e) (ownTexHandle e) True
    | e ← allEdges ]
    <> [ if shareTex ∨ e ≢ WallNE
           then WallArtEntry e (Just c) (ownFacePath e c) (ownFaceHandle e c) True
           else WallArtEntry e (Just c) (facePath e c) (faceHandle e c) sharedOwned
       | e ← allEdges, c ← allCaps ]
  where
    ownTexPath  e     = "pack/half/wall_" <> edgeName e <> ".png"
    ownTexHandle e    = TextureHandle (500 + edgeIx e)
    ownFacePath e c   = "pack/half/face_" <> edgeName e <> "_"
                                          <> wallCapsCode c <> ".png"
    ownFaceHandle e c = TextureHandle (600 + edgeIx e * 4 + capIx c)

-- | The default family and one half-clashing family, registered in BOTH
--   orders. Nothing guarantees pack load order, so every conclusion drawn
--   from ownership has to hold in either.
halfClashCatalogs ∷ Bool → Bool → [(String, StructureWallCatalog)]
halfClashCatalogs shareTex sharedOwned =
    [ ("default family first",  build [familyEntries, clashing])
    , ("clashing family first", build [clashing, familyEntries]) ]
  where
    clashing = halfClashEntries shareTex sharedOwned
    build    = foldl' (\cat es → fromMaybe (error "half-clash fixture is incomplete")
                                           (registerWallFamily es cat))
                      emptyStructureWallCatalog

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
                    (texPath e, texHandle e) (facePath e placedCaps, faceHandle e placedCaps)
                    `shouldBe` Just (expectedArt facing e)

        it "keeps a wall's texture and cap facemap on the SAME edge" $
            forM_ allFacings $ \facing → forM_ allEdges $ \e → do
                let s = expectScreenEdge facing e
                rotatedWallArt catalog facing e
                        (texPath e, texHandle e)
                        (facePath e placedCaps, faceHandle e placedCaps)
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
                rotatedWallArt catalog facing e
                        (texPath e, texHandle e)
                        (facePath e placedCaps, faceHandle e placedCaps)
                    `shouldBe` Just (texHandle s, faceHandle s expectCaps)

        it "refuses a texture/facemap pair from DIFFERENT authored edges" $
            -- Never a sprite from one direction with a mask from another:
            -- the mismatched pair rotates not at all.
            rotatedWallArt catalog FaceWest WallNE
                (texPath WallNE, texHandle WallNE)
                (facePath WallSW placedCaps, faceHandle WallSW placedCaps)
                `shouldBe` Nothing

        it "leaves art from outside any registered pack exactly as placed" $
            rotatedWallArt catalog FaceWest WallNE
                ("somewhere/else.png", TextureHandle 999)
                (facePath WallNE placedCaps, faceHandle WallNE placedCaps)
                `shouldBe` Nothing

    describe "FaceSouth is unchanged" $ do
        it "resolves every wall back to the art it was placed with" $
            forM_ allEdges $ \e →
                rotatedWallArt catalog FaceSouth e
                    (texPath e, texHandle e) (facePath e placedCaps, faceHandle e placedCaps)
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

    describe "a partial pack variant (#1794 review)" $ do
        it "leaves a DEFAULT wall rotating through the DEFAULT family" $
            -- The variant inherits wall_nw.png/wall_se.png/wall_sw.png, so
            -- a wall placed with one of them is indistinguishable from a
            -- default wall. It must keep resolving to the default art at
            -- every facing, never the variant's own NE sprite.
            forM_ allFacings $ \facing →
                forM_ [WallNW, WallSE, WallSW] $ \e →
                    rotatedWallArt variantCatalog facing e
                        (texPath e, texHandle e)
                        (facePath e placedCaps, faceHandle e placedCaps)
                        `shouldBe` Just (expectedArt facing e)

        it "rotates the variant's OWN wall through the variant family, \
           \inherited art included" $
            forM_ allFacings $ \facing → do
                let s = expectScreenEdge facing WallNE
                    expectCaps | expectCapSwap facing WallNE = WallCaps False True
                               | otherwise                   = WallCaps True False
                    -- Its own sprite when the screen edge is still NE, the
                    -- INHERITED default sprite otherwise — which is exactly
                    -- what a variant that authored only NE art has.
                    expectTex | s ≡ WallNE = variantTexHandle
                              | otherwise  = texHandle s
                rotatedWallArt variantCatalog facing WallNE
                    (variantTexPath, variantTexHandle)
                    (facePath WallNE placedCaps, faceHandle WallNE placedCaps)
                    `shouldBe` Just (expectTex, faceHandle s expectCaps)

        it "registering the same family twice changes nothing" $
            -- Idempotent, so a second registration cannot make a variant's
            -- own paths look contradictory.
            forM_ allFacings $ \facing → forM_ allEdges $ \e →
                rotatedWallArt (fromMaybe variantCatalog
                                   (registerWallFamily familyEntries variantCatalog))
                               facing e
                               (texPath e, texHandle e)
                               (facePath e placedCaps, faceHandle e placedCaps)
                    `shouldBe` Just (expectedArt facing e)

        it "takes BOTH rotated assets from the variant a wall's SPRITE \
           \names, even where the placed facemap is inherited" $
            -- The #1794 round-2 case: this variant owns the NE sprite but
            -- inherits the NE masks, so a wall it placed stores an
            -- inherited facemap. Rotating it onto screen NW must reach the
            -- variant's OWN NW masks — resolving the facemap through the
            -- family that happens to own the placed one would silently
            -- fall back to the default's.
            forM_ allFacings $ \facing → do
                let s = expectScreenEdge facing WallNE
                    expectCaps | expectCapSwap facing WallNE = WallCaps False True
                               | otherwise                   = WallCaps True False
                    expectTex | s ≡ WallNE = maskTexHandle
                              | otherwise  = texHandle s
                    expectFace | s ≡ WallNW = maskFaceHandle expectCaps
                               | otherwise  = faceHandle s expectCaps
                rotatedWallArt maskCatalog facing WallNE
                    (maskTexPath, maskTexHandle)
                    (facePath WallNE placedCaps, faceHandle WallNE placedCaps)
                    `shouldBe` Just (expectTex, expectFace)

        it "leaves a DEFAULT wall alone when a variant overrides only masks" $
            -- The other side of the same fixture: the default's own NW
            -- wall still resolves to the default's NW masks at every
            -- facing, even though a variant declares masks for that edge.
            forM_ allFacings $ \facing →
                forM_ [WallNW, WallSE, WallSW] $ \e →
                    rotatedWallArt maskCatalog facing e
                        (texPath e, texHandle e)
                        (facePath e placedCaps, faceHandle e placedCaps)
                        `shouldBe` Just (expectedArt facing e)

        it "keeps the placed pair's OWN handles when a variant re-loads the \
           \art it inherits" $ do
            -- @engine.loadTexture@ mints a fresh handle per call and does
            -- not dedupe by path, so a variant re-loading an inherited
            -- facemap really does hand over a SECOND handle for a path the
            -- default already registered. A default wall must keep the
            -- handle its own palette entry resolved to — not the
            -- duplicate, whose GPU upload may not even have landed.
            let dupHandle e c = TextureHandle (700 + edgeIx e * 4 + capIx c)
                reloading =
                    [ if e ≡ WallNE
                        then WallArtEntry e Nothing variantTexPath variantTexHandle True
                        else WallArtEntry e Nothing (texPath e) (texHandle e) False
                    | e ← allEdges ]
                    <> [ WallArtEntry e (Just c) (facePath e c) (dupHandle e c) False
                       | e ← allEdges, c ← allCaps ]
                dupCat = fromMaybe (error "re-loading fixture is incomplete")
                             (registerWallFamily reloading catalog)
            forM_ allFacings $ \facing → forM_ allEdges $ \e →
                rotatedWallArt dupCat facing e
                    (texPath e, texHandle e)
                    (facePath e placedCaps, faceHandle e placedCaps)
                    `shouldBe` Just (expectedArt facing e)
            -- ...and the catalogue itself kept the FIRST handle, so even a
            -- rotation that does change the art never reaches the
            -- duplicate.
            forM_ [ (e, c) | e ← allEdges, c ← allCaps ] $ \(e, c) →
                HM.lookup (facePath e c) (swcHandles dupCat)
                    `shouldBe` Just (faceHandle e c)
            -- And a piece REPLAYED from a save resolves its palette id
            -- through engine.loadTexture a THIRD time, matching neither
            -- registration. FaceSouth must still render exactly what that
            -- palette entry resolved to, so the rotation that changes
            -- nothing changes nothing.
            let replayedTex  e = TextureHandle (800 + edgeIx e)
                replayedFace e = TextureHandle (900 + edgeIx e)
            forM_ allEdges $ \e →
                rotatedWallArt dupCat FaceSouth e
                    (texPath e, replayedTex e)
                    (facePath e placedCaps, replayedFace e)
                    `shouldBe` Just (replayedTex e, replayedFace e)

        it "refuses a placed pair two families both carry AND both own" $ do
            -- Contradictory pack data: this second family declares the
            -- default's NE sprite AND the default's NE masks as its own,
            -- so nothing in the placement says which was meant. The pair
            -- stops rotating instead of registration ORDER deciding.
            let clashing =
                    [ WallArtEntry e Nothing
                          (if e ≡ WallNE
                             then texPath WallNE
                             else "pack/clash/wall_" <> edgeName e <> ".png")
                          (TextureHandle (500 + edgeIx e)) True
                    | e ← allEdges ]
                    <> [ if e ≡ WallNE
                           then WallArtEntry e (Just c) (facePath e c)
                                    (faceHandle e c) True
                           else WallArtEntry e (Just c)
                                    ("pack/clash/face_" <> edgeName e <> "_"
                                                        <> wallCapsCode c <> ".png")
                                    (TextureHandle (600 + edgeIx e * 4 + capIx c)) True
                       | e ← allEdges, c ← allCaps ]
                clashCat = fromMaybe (error "clashing fixture is incomplete")
                               (registerWallFamily clashing catalog)
            rotatedWallArt clashCat FaceWest WallNE
                (texPath WallNE, texHandle WallNE)
                (facePath WallNE placedCaps, faceHandle WallNE placedCaps)
                `shouldBe` Nothing
            -- A pair only ONE family carries is untouched by that.
            rotatedWallArt clashCat FaceWest WallNW
                (texPath WallNW, texHandle WallNW)
                (facePath WallNW placedCaps, faceHandle WallNW placedCaps)
                `shouldBe` Just (expectedArt FaceWest WallNW)

    describe "one-half ambiguous ownership (#2160)" $
        forM_ ([("sprite", True), ("cap facemap", False)] ∷ [(String, Bool)]) $
          \(half, shareTex) → do
            forM_ (halfClashCatalogs shareTex True) $ \(order, cat) → do
                it ("marks exactly the placed " <> half <> " ambiguous, " <> order) $ do
                    -- Without this the refusal below would prove nothing: a
                    -- fixture contesting BOTH halves is the case #1794
                    -- already refused, and one contesting NEITHER never
                    -- reaches the reduction at all.
                    let texOwner  = HM.lookup (texPath WallNE) (swcTexOwner cat)
                        faceOwner = HM.lookup (facePath WallNE placedCaps)
                                              (swcFaceOwner cat)
                        contested = if shareTex then texOwner else faceOwner
                        uncontested = if shareTex then faceOwner else texOwner
                        -- An uncontested default path, so "the default
                        -- family" is named without depending on its index.
                        defaultOwner = HM.lookup (texPath WallNW) (swcTexOwner cat)
                    defaultOwner `shouldSatisfy` \o → case o of
                        Just (Just _) → True
                        _             → False
                    contested   `shouldBe` Just Nothing
                    uncontested `shouldBe` defaultOwner

                it ("refuses the pair whose " <> half <> " is ambiguously owned \
                    \even though its companion names one family, " <> order) $
                    forM_ allFacings $ \facing →
                        rotatedWallArt cat facing WallNE
                            (texPath WallNE, texHandle WallNE)
                            (facePath WallNE placedCaps, faceHandle WallNE placedCaps)
                            `shouldBe` Nothing

                it ("leaves a pair only one family carries rotating, " <> order) $
                    forM_ allFacings $ \facing →
                        forM_ [WallNW, WallSE, WallSW] $ \e →
                            rotatedWallArt cat facing e
                                (texPath e, texHandle e)
                                (facePath e placedCaps, faceHandle e placedCaps)
                                `shouldBe` Just (expectedArt facing e)

            forM_ (halfClashCatalogs shareTex False) $ \(order, cat) →
                it ("keeps rotating when the shared " <> half <> " is INHERITED \
                    \rather than declared, " <> order) $ do
                    -- An ABSENT claim is not an ambiguous one. These are the
                    -- same two families differing only in whether the second
                    -- DECLARES the shared path, and this is the ordinary
                    -- partial-variant shape, so the declaring family's art
                    -- still answers at every facing.
                    let sharedOwner
                          | shareTex  = HM.lookup (texPath WallNE) (swcTexOwner cat)
                          | otherwise = HM.lookup (facePath WallNE placedCaps)
                                                  (swcFaceOwner cat)
                    sharedOwner `shouldBe` HM.lookup (texPath WallNW) (swcTexOwner cat)
                    forM_ allFacings $ \facing →
                        rotatedWallArt cat facing WallNE
                            (texPath WallNE, texHandle WallNE)
                            (facePath WallNE placedCaps, faceHandle WallNE placedCaps)
                            `shouldBe` Just (expectedArt facing WallNE)

    describe "unresolved art" $
        it "emits nothing until both palette ids have a runtime handle" $
            structurePieceQuads catalog palette HM.empty
                (\(TextureHandle h) → fromIntegral h) HM.empty
                FaceSouth fixZ 8 1.0 fixGX fixGY SWallSE (wallPiece WallSE)
                `shouldSatisfy` null
