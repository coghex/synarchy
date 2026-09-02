{-# LANGUAGE OverloadedStrings #-}
-- | Construction ghosts for structure pieces (#1846).
--
--   Both ghost states used to be one 96x64 category placeholder drawn at
--   the designation's RAW surface z, and there was no preview at all
--   until the first anchor click. A floor, a ceiling, four wall edges,
--   four posts and wire were the same red diamond, one z-level below
--   where any of them would land.
--
--   The suite's central move is to compare each ghost against the
--   PLACED-piece producer for the same art at the z the placer would use
--   — 'Structure.Render.structurePieceQuads', reached through the
--   texture-palette entry point the ghost never touches. A ghost and a
--   built piece must be the same quads modulo tint, which is one
--   assertion covering the art, the facemap, the final z, #1712's
--   camera rotation, #415's front-wall strips and 'postToQuad's inset at
--   once. It is not the renderer checked against itself: the reference
--   side is built from the pack YAML's own paths and a z this module
--   derives, neither of which the ghost path can influence.
module Test.Headless.Render.StructureGhost (spec) where

import UPrelude
import Test.Hspec
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import qualified Data.List as L
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU

import Building.Render (ghostTint)
import Engine.Asset.Handle (toInt)
import Engine.Graphics.Camera
    (Camera2D(..), CameraFacing(..), defaultCamera)
import Engine.Graphics.Vulkan.Types.Vertex (Vertex(..), Vec2(..), Vec4(..))
import Engine.Scene.Types (SortableQuad(..))
import Structure.ArtCatalog
    ( ArtAsset(..), PackArtRegistration(..), PieceArt(..)
    , resolveUnplacedArt, defaultPieceArtContext, PieceArtContext(..) )
import Structure.Facing (WallCaps(..), WallEdge(..))
import Structure.Palette (emptyTexPalette, internPath)
import Structure.Render (structurePieceQuads)
import Structure.Types
    ( ChunkStructures, StructurePieceData(..), StructureSlot(..)
    , StructureStage, emptyStructureStage )
import Structure.Wire (WireShape(..))
import World.Chunk.Types
    (ChunkCoord(..), ColumnTiles(..), LoadedChunk(..), chunkSize)
import World.Construct.Attempt (ConstructAttemptId(..))
import World.Construct.Extent (structureDragExtent)
import World.Construct.Plan (PlanWorld(..))
import World.Construct.Receipt (ConstructPayment(..), MaterialReceipt(..))
import World.Construct.Types
    ( ConstructDesignation(..), ConstructDesignations, ConstructTarget(..)
    , StructurePiece(..), newConstructDesignation )
import World.Flora.Types (emptyFloraChunkData)
import World.Fluid.Types (emptyIceMap)
import World.Generate (viewDepth)
import World.Generate.Coordinates (canonicalTile)
import World.Grid (gridToWorld)
import World.Render.StructureGhost
import World.Render.ViewBounds (ViewBounds, computeViewBounds)
import World.Tile.Types (WorldTileData(..))

import Test.Headless.Render.StructureGhostFixture

-- * Fixture geography
--
--   A wrapping page 64 chunks across, so "across the seam" means the
--   same thing it does in "Test.Headless.World.Render.StructureSeam".

worldSize, surfaceZ, zSlice ∷ Int
worldSize = 64
surfaceZ  = 10
zSlice    = 14      -- above every ghost z, so nothing is sliced away

zoom ∷ Float
zoom = 4.0

fbW, fbH ∷ Int
(fbW, fbH) = (800, 600)

effDepth ∷ Int
effDepth = min viewDepth (max 8 (round (zoom * 80.0 + 8.0 ∷ Float)))

tileAlpha ∷ Float
tileAlpha = 0.8

-- | The interior tile every non-seam example works at.
homeTile ∷ (Int, Int)
homeTile = (4 * chunkSize + 3, 4 * chunkSize + 3)

allFacings ∷ [CameraFacing]
allFacings = [FaceSouth, FaceWest, FaceNorth, FaceEast]

dungeonPack, wirePack ∷ Text
dungeonPack = "dungeon_1"
wirePack    = "wire"

-- * Spec

spec ∷ Spec
spec = beforeAll loadShippedPacks $ describe "structure ghost" $ do
    artSpec
    zSpec
    capSpec
    rotationSpec
    statesSpec
    hoverSpec
    mixedSpec
    floorlessPostSpec
    missingArtSpec
    wireSpec
    workSpec
    lineModeSpec
    seamSpec
    residueSpec

-- * Right art, every kind

-- | Every kind the two shipped packs offer, with the slot it targets and
--   the grid z the PLACER would put it at (@scripts/structures.lua@'s
--   @placeKind@ / @placeWall@ and @scripts/wire.lua@'s @placeSelf@ —
--   restated here rather than read out of the resolver, so a resolver
--   that changed its mind fails this suite).
--
--   The four post CORNERS are covered even though the shipped picker
--   only offers an edgeless post: a designation may carry any of them,
--   and the ghost has to draw the slot the descriptor names.
everyKind ∷ [(String, StructurePiece, StructureSlot, Int)]
everyKind =
    [ ("floor",   piece dungeonPack "floor"   Nothing,     SFloor,   surfaceZ + 1)
    , ("ceiling", piece dungeonPack "ceiling" Nothing,     SCeiling, surfaceZ + 2)
    , ("wall ne", piece dungeonPack "wall" (Just "ne"), SWallNE, surfaceZ + 1)
    , ("wall nw", piece dungeonPack "wall" (Just "nw"), SWallNW, surfaceZ + 1)
    , ("wall se", piece dungeonPack "wall" (Just "se"), SWallSE, surfaceZ + 1)
    , ("wall sw", piece dungeonPack "wall" (Just "sw"), SWallSW, surfaceZ + 1)
    , ("post n",  piece dungeonPack "post" (Just "n"),  SPostN,  floorGridZ)
    , ("post e",  piece dungeonPack "post" (Just "e"),  SPostE,  floorGridZ)
    , ("post s",  piece dungeonPack "post" (Just "s"),  SPostS,  floorGridZ)
    , ("post w",  piece dungeonPack "post" (Just "w"),  SPostW,  floorGridZ)
    , ("wire",    piece wirePack "wire" Nothing,        SWire,   surfaceZ + 1)
    ]
  where piece = StructurePiece

-- | The z of the floor a post stands on. A post takes its SUPPORTING
--   floor's z, not a terrain-derived one, so the fixture places a real
--   floor and this is that floor's own value.
floorGridZ ∷ Int
floorGridZ = surfaceZ + 1

artSpec ∷ SpecWith [PackFixture]
artSpec = describe "draws the piece's own art at the placer's z" $
    forM_ everyKind $ \(label, sp, slot, gridZ) →
        it (label ⧺ " previews and designates exactly as the placer \
                    \would build it") $ \packs →
            forM_ allFacings $ \facing → do
                let ge = ghostEnvAt packs facing (designationsFor sp)
                    reference = placedQuads packs facing slot
                                    (artFor ge sp) gridZ
                -- The DESIGNATED state.
                stripTints (snd (structureDesignationGhosts ge))
                    `shouldBe` stripTints (V.fromList reference)
                -- …and the PREVIEW state, which is the same art.
                stripTints (snd (previewAt packs facing sp))
                    `shouldBe` stripTints (V.fromList reference)

-- * Right z

zSpec ∷ SpecWith [PackFixture]
zSpec = describe "sits at the z the placer will use, not the \
                 \designation's raw surface z" $
    forM_ everyKind $ \(label, sp, slot, gridZ) →
        it (label ⧺ " is drawn at grid z " ⧺ show gridZ) $ \packs → do
            let ge = ghostEnvAt packs FaceSouth (designationsFor sp)
                ghost = snd (structureDesignationGhosts ge)
                atZ z = placedQuads packs FaceSouth slot
                            (artFor ge sp) z
            -- The raw surface z is what the old ghost used, and for
            -- every kind here it is a DIFFERENT answer, so this is not
            -- vacuous.
            gridZ `shouldNotBe` surfaceZ
            stripTints ghost `shouldBe` stripTints (V.fromList (atZ gridZ))
            stripTints ghost
                `shouldNotBe` stripTints (V.fromList (atZ surfaceZ))

-- * Wall caps

-- | Every same-tile post combination against every wall edge: the ghost
--   must pick the cap facemap PLACEMENT picks, which is the pack's
--   @"\<left\>\<right\>"@ entry for the posts standing at that edge's
--   two end corners.
capSpec ∷ SpecWith [PackFixture]
capSpec = describe "resolves the wall cap from the posts on its own tile" $
    forM_ [ (edge, slot) | (edge, slot) ←
              [ (WallNE, SWallNE), (WallNW, SWallNW)
              , (WallSE, SWallSE), (WallSW, SWallSW) ] ] $ \(edge, slot) →
        it ("wall " ⧺ show edge ⧺ " caps against every post combination") $
          \packs →
            forM_ postCombinations $ \posts → do
                let sp = StructurePiece dungeonPack "wall" (Just (edgeText edge))
                    ge = ghostEnvWith packs FaceSouth (designationsFor sp)
                             (postStructures posts) emptyStructureStage
                    art = artFor ge sp
                    reference = placedQuads packs FaceSouth slot art
                                    (surfaceZ + 1)
                stripTints (snd (structureDesignationGhosts ge))
                    `shouldBe` stripTints (V.fromList reference)
  where
    -- All sixteen post sets, so every one of the pack's four cap
    -- facemaps per edge is exercised from both of its ends.
    postCombinations = L.subsequences [SPostN, SPostE, SPostS, SPostW]

-- * Camera rotation

rotationSpec ∷ SpecWith [PackFixture]
rotationSpec =
    it "a wall ghost rotates onto the sprite its authored edge occupies, \
       \at every facing" $ \packs →
        forM_ [ (WallNE, SWallNE), (WallNW, SWallNW)
              , (WallSE, SWallSE), (WallSW, SWallSW) ] $ \(edge, slot) → do
            let sp = StructurePiece dungeonPack "wall" (Just (edgeText edge))
                drawn facing =
                    let ge = ghostEnvAt packs facing (designationsFor sp)
                    in map sqTexture (V.toList
                           (snd (structureDesignationGhosts ge)))
            -- Every facing draws SOMETHING…
            forM_ allFacings $ \facing →
                drawn facing `shouldSatisfy` not ∘ null
            -- …and the four facings do not all draw the same sprite,
            -- which is exactly what "the art rotates" means. A ghost
            -- that skipped the catalogue would show one handle four
            -- times.
            length (L.nub (concatMap drawn allFacings)) `shouldSatisfy` (> 1)
            -- The rotated pair is the PLACED pair, whatever it is.
            forM_ allFacings $ \facing → do
                let ge = ghostEnvAt packs facing (designationsFor sp)
                stripTints (snd (structureDesignationGhosts ge))
                    `shouldBe` stripTints (V.fromList
                        (placedQuads packs facing slot (artFor ge sp)
                                     (surfaceZ + 1)))

-- * Two states, one art

statesSpec ∷ SpecWith [PackFixture]
statesSpec = describe "the two ghost states" $ do
    it "resolve identical geometry and differ only in alpha" $ \packs → do
        -- A CEILING: the fixture tile's floor slot is already filled, so
        -- a floor candidate would preview red and this example would be
        -- comparing the invalid tint instead of the two lifecycle ones.
        let sp = StructurePiece dungeonPack "ceiling" Nothing
            designated = snd (structureDesignationGhosts
                                 (ghostEnvAt packs FaceSouth
                                     (designationsFor sp)))
            preview = snd (previewAt packs FaceSouth sp)
        stripTints designated `shouldBe` stripTints preview
        quadTints designated `shouldSatisfy`
            allTints (1, 1, 1) (tileAlpha * designatedGhostAlpha)
        quadTints preview `shouldSatisfy`
            allTints (1, 1, 1) (tileAlpha * previewGhostAlpha)

    it "make the preview lighter than the commitment (D-19)" $ \_ →
        previewGhostAlpha `shouldSatisfy` (< designatedGhostAlpha)

    it "never tint a committed designation, whatever its plan says" $
      \packs → do
        -- The tile's surface has drifted under a committed designation,
        -- so the resolver calls it invalid. D-20: a committed ghost
        -- still shows no invalid feedback.
        let sp = StructurePiece dungeonPack "floor" Nothing
            designs = HM.singleton (canon homeTile)
                (newConstructDesignation (surfaceZ - 3) (CtStructure sp)
                     (ConstructAttemptId 1))
            ge = ghostEnvAt packs FaceSouth designs
            tints = quadTints (snd (structureDesignationGhosts ge))
        tints `shouldSatisfy` all (\(Vec4 r g b _) → (r, g, b) ≡ (1, 1, 1))

-- * Hover before anchor

hoverSpec ∷ SpecWith [PackFixture]
hoverSpec = describe "the single-piece hover preview" $ do
    it "renders exactly one candidate piece for one hovered tile" $
      \packs → do
        let sp = StructurePiece dungeonPack "floor" Nothing
            (scanned, quads) = structurePreviewGhosts
                (ghostEnvAt packs FaceSouth HM.empty) sp surfaceZ [homeTile]
        scanned `shouldBe` 1
        quads `shouldSatisfy` (not ∘ V.null)
        stripTints quads `shouldBe` stripTints (V.fromList
            (placedQuads packs FaceSouth SFloor
                (artFor (ghostEnvAt packs FaceSouth HM.empty) sp)
                (surfaceZ + 1)))

    it "draws nothing at all when no piece is armed" $ \packs → do
        let (scanned, quads) = structurePreviewGhosts
                (ghostEnvAt packs FaceSouth HM.empty)
                (StructurePiece dungeonPack "floor" Nothing) surfaceZ []
        scanned `shouldBe` 0
        quads `shouldSatisfy` V.null

-- * Mixed validity

mixedSpec ∷ SpecWith [PackFixture]
mixedSpec =
    it "a rectangle of valid, invalid and missing-art candidates draws \
       \each per the resolver's own outcome" $ \packs → do
        -- occupied: the floor slot is already filled, so the resolver
        -- refuses it — VISIBLE-invalid, drawn red.
        let occupied = (fst homeTile + 1, snd homeTile)
            valid    = homeTile
            sp       = StructurePiece dungeonPack "floor" Nothing
            missing  = StructurePiece "no_such_pack" "floor" Nothing
            ge = ghostEnvWith packs FaceSouth HM.empty
                     (HM.union (structuresAt occupied [SFloor])
                               (structuresAt homeTile [SFloor, SPostN
                                                      , SPostE, SPostS
                                                      , SPostW]))
                     emptyStructureStage
        -- A tile whose slot is taken still draws — in red.
        let (_, occupiedQuads) =
                structurePreviewGhosts ge sp surfaceZ [occupied]
        quadTints occupiedQuads `shouldBe`
            [invalidTint]
        -- The valid neighbour draws untinted at the same alpha… but its
        -- own floor slot is filled by the fixture, so use a kind whose
        -- slot is free.
        let wallPiece = StructurePiece dungeonPack "wall" (Just "ne")
            (_, validQuads) =
                structurePreviewGhosts ge wallPiece surfaceZ [valid]
        quadTints validQuads `shouldBe`
            [Vec4 1 1 1 (tileAlpha * previewGhostAlpha)]
        -- Missing art draws NOTHING — no generic shape, no red fallback.
        let (missingScanned, missingQuads) =
                structurePreviewGhosts ge missing surfaceZ [valid]
        missingScanned `shouldBe` 1
        missingQuads `shouldSatisfy` V.null
        -- And an unresolved-terrain candidate is simply absent.
        let offWorld = (fst homeTile, snd homeTile + 40 * chunkSize)
            (_, unresolved) =
                structurePreviewGhosts ge sp surfaceZ [offWorld]
        unresolved `shouldSatisfy` V.null

-- * Wire

wireSpec ∷ SpecWith [PackFixture]
wireSpec = describe "a wire ghost runs the autotile rule speculatively" $ do
    it "a dragged run previews as one connected line, not isolated stubs" $
      \packs → do
        let sp = StructurePiece wirePack "wire" Nothing
            run = [ (fst homeTile + i, snd homeTile) | i ← [0 .. 3] ]
            ge = ghostEnvAt packs FaceSouth HM.empty
        -- Ends at both extremes, straights between: exactly what the
        -- placer's own shapeFor answers for a 4-long east-west run.
        -- 'wireShapeFor' names an END by the side it CONNECTS on (unlike
        -- a tee, which names its gap), so the west end reaches east and
        -- is WireEndE.
        wireShapesOf packs ge sp run `shouldBe`
            [ Just WireEndE, Just WireStraightEW
            , Just WireStraightEW, Just WireEndW ]

    it "a committed wire designation connects a neighbour's ghost" $
      \packs → do
        let sp = StructurePiece wirePack "wire" Nothing
            east = (fst homeTile + 1, snd homeTile)
            isolated = ghostEnvAt packs FaceSouth HM.empty
            withNeighbour = ghostEnvAt packs FaceSouth
                                (designationsFor' east sp)
        wireShapesOf packs isolated sp [homeTile] `shouldBe` [Just WireIsolated]
        wireShapesOf packs withNeighbour sp [homeTile] `shouldBe` [Just WireEndE]

    it "a PLACED wire neighbour connects a ghost" $ \packs → do
        let sp = StructurePiece wirePack "wire" Nothing
            east = (fst homeTile + 1, snd homeTile)
            alone = ghostEnvWith packs FaceSouth HM.empty HM.empty
                        emptyStructureStage
            beside = ghostEnvWith packs FaceSouth HM.empty
                         (structuresAt east [SWire]) emptyStructureStage
        wireShapesOf packs alone sp [homeTile] `shouldBe` [Just WireIsolated]
        wireShapesOf packs beside sp [homeTile] `shouldBe` [Just WireEndE]

    it "an INVALID proposed candidate contributes no connectivity (D-22)" $
      \packs → do
        -- The discriminating case. The middle tile of a three-long run
        -- is refused because it carries an outstanding NON-wire
        -- designation — and carries no wire of its own, placed or
        -- designated. So the ONLY thing that could connect its two
        -- neighbours is the proposal itself, and D-22 says a proposal
        -- that will not be built must not.
        --
        -- Written this way deliberately: an earlier version made the
        -- middle invalid by placing wire there, which proves nothing —
        -- the neighbours would read that real wire whether or not the
        -- proposed set were filtered, so the example passed for an
        -- implementation that fed EVERY candidate into 'pwProposedWire'.
        let sp   = StructurePiece wirePack "wire" Nothing
            mid  = (fst homeTile + 1, snd homeTile)
            far  = (fst homeTile + 2, snd homeTile)
            run  = [homeTile, mid, far]
            -- A floor designation: an outstanding designation at the
            -- tile, which 'PlanForPlacement' refuses, and NOT wire, so
            -- 'wireDesignatedAt' does not see it either.
            blocked = HM.singleton (canon mid)
                (newConstructDesignation surfaceZ
                    (CtStructure (StructurePiece dungeonPack "floor" Nothing))
                    (ConstructAttemptId 7))
            ge = ghostEnvWith packs FaceSouth blocked HM.empty
                     emptyStructureStage
        -- ONE call over the WHOLE run, because that is the only shape
        -- of this example that can fail: asking for each tile
        -- separately would leave the middle out of its neighbours'
        -- candidate set for reasons unrelated to its outcome, and the
        -- assertion would hold for any implementation at all.
        --
        -- The control: nothing blocking, all three proposed-valid, and
        -- the run reads as one line.
        wireShapesOf packs (ghostEnvAt packs FaceSouth HM.empty) sp run
            `shouldBe` [Just WireEndE, Just WireStraightEW, Just WireEndW]
        -- Blocked: the two survivors go ISOLATED, because the only
        -- thing that ever connected them was a proposal that will not
        -- be built. An implementation feeding every candidate into
        -- 'pwProposedWire' regardless of outcome answers
        -- [EndE, StraightEW, EndW] here — the control's own answer —
        -- which is exactly the bug, and exactly what this catches.
        --
        -- The middle still draws (in red, below) and still resolves a
        -- straight from its two VALID neighbours: being refused stops
        -- it contributing connectivity, not receiving it.
        wireShapesOf packs ge sp run
            `shouldBe` [Just WireIsolated, Just WireStraightEW, Just WireIsolated]
        -- The refused candidate draws, and only it is red (D-25).
        let (_, quads) = structurePreviewGhosts ge sp surfaceZ run
        V.length quads `shouldBe` 3
        map (color ∘ sqV0) (V.toList quads) `shouldBe`
            [validPreviewTint, invalidTint, validPreviewTint]

-- | A post whose supporting FLOOR is absent has no final grid z at all
--   — a post takes its floor's z, so "no floor" and "nowhere to draw"
--   are the same fact. It is therefore ABSENT rather than red, which is
--   D-25's own carve-out ("a position whose world location cannot be
--   resolved ... remains absent") and NOT a missed invalid-tint case.
--
--   Pinned explicitly because it is visible in play: hovering a post
--   over bare ground shows nothing at all until a floor is under it.
floorlessPostSpec ∷ SpecWith [PackFixture]
floorlessPostSpec = describe "a post with no supporting floor" $ do
    it "draws nothing, because it has no z to be drawn at" $ \packs → do
        let sp = StructurePiece dungeonPack "post" (Just "n")
            -- A bare tile: no floor, no posts.
            ge = ghostEnvWith packs FaceSouth HM.empty HM.empty
                     emptyStructureStage
            (scanned, quads) = structurePreviewGhosts ge sp surfaceZ [homeTile]
        scanned `shouldBe` 1
        quads `shouldSatisfy` V.null

    it "draws as soon as a floor is under it" $ \packs → do
        let sp = StructurePiece dungeonPack "post" (Just "n")
            ge = ghostEnvWith packs FaceSouth HM.empty
                     (structuresAt homeTile [SFloor]) emptyStructureStage
        snd (structurePreviewGhosts ge sp surfaceZ [homeTile])
            `shouldSatisfy` (not ∘ V.null)

-- | A COMMITTED designation is held to the same missing-art rule as a
--   preview: no quad, no generic fallback. #1842's deduplicated
--   diagnostic is the only thing that reports it.
missingArtSpec ∷ SpecWith [PackFixture]
missingArtSpec =
    it "a committed designation whose art cannot resolve draws nothing" $
      \packs → do
        let sp = StructurePiece "no_such_pack" "floor" Nothing
            ge = ghostEnvAt packs FaceSouth (designationsFor sp)
            (scanned, quads) = structureDesignationGhosts ge
        -- It WAS enumerated — it is simply not drawable.
        scanned `shouldBe` 1
        quads `shouldSatisfy` V.null

-- * Work hides the site

workSpec ∷ SpecWith [PackFixture]
workSpec =
    it "a paid designation draws nothing until the finished piece \
       \appears" $ \packs → do
        let sp = StructurePiece dungeonPack "floor" Nothing
            unpaid = newConstructDesignation surfaceZ (CtStructure sp)
                         (ConstructAttemptId 1)
            paid = unpaid { cdPayment = CpPaid (MaterialReceipt []) }
            ghosts d = snd (structureDesignationGhosts
                (ghostEnvAt packs FaceSouth (HM.singleton (canon homeTile) d)))
        ghosts unpaid `shouldSatisfy` (not ∘ V.null)
        ghosts paid `shouldSatisfy` V.null

-- * Line mode

lineModeSpec ∷ SpecWith [PackFixture]
lineModeSpec =
    it "the line-mode preview still matches what commits" $ \packs → do
        let sp = StructurePiece wirePack "wire" Nothing
            anchor = homeTile
            end = (fst homeTile + 5, snd homeTile + 2)
            tiles = structureDragExtent worldSize True anchor end
            ge = ghostEnvAt packs FaceSouth HM.empty
            (scanned, quads) = structurePreviewGhosts ge sp surfaceZ tiles
        -- The candidate set IS the shared helper's, so the preview
        -- cannot name a tile the commit would not.
        scanned `shouldBe` length tiles
        length tiles `shouldBe` 6            -- 1-wide line along x
        V.length quads `shouldBe` 6

-- * Seam

seamSpec ∷ SpecWith [PackFixture]
seamSpec = describe "at the cylindrical U seam" $
    forM_ [ ("a committed designation", \packs facing →
                snd (structureDesignationGhosts
                        (seamGhostEnv packs facing
                            (designationsFor' seamTile seamPiece))))
          , ("the pre-anchor hover", \packs facing →
                snd (structurePreviewGhosts
                        (seamGhostEnv packs facing HM.empty)
                        seamPiece surfaceZ [seamTile]))
          , ("an anchored drag", \packs facing →
                snd (structurePreviewGhosts
                        (seamGhostEnv packs facing HM.empty)
                        seamPiece surfaceZ
                        (structureDragExtent worldSize False seamTile
                             seamTile)))
          ] $ \(label, render) →
        it (label ⧺ " renders through the nearest visible alias without \
                    \changing sort keys or payloads") $ \packs →
            forM_ allFacings $ \facing → do
                let ghost = V.toList (render packs facing)
                    ge = seamGhostEnv packs facing HM.empty
                    reference = placedQuadsAt packs seamTile facing SFloor
                                    (artFor ge seamPiece) (surfaceZ + 1)
                -- Something is drawn at all…
                ghost `shouldSatisfy` not ∘ null
                length ghost `shouldBe` length reference
                -- …and everything but the vertex POSITIONS is identical
                -- to the unshifted piece: sort keys, texture, layer,
                -- payloads.
                map quadShapeNoPos ghost
                    `shouldBe` map quadShapeNoPos reference

seamPiece ∷ StructurePiece
seamPiece = StructurePiece dungeonPack "floor" Nothing

-- * No palette residue

residueSpec ∷ SpecWith [PackFixture]
residueSpec =
    it "draws real pack art with no texture palette in play" $ \packs → do
        -- The residue #1675 forbids is a path INTERNED into the saved
        -- 'Structure.Palette' by something that placed nothing. The
        -- ghost producers take no palette at all, so they cannot intern
        -- — and this is what makes that a real capability rather than
        -- an omission: with no palette anywhere, both states still draw
        -- the SHIPPED pack's own registered handles.
        --
        -- The end-to-end half stays where it already is, on the real
        -- 'structure.place' boundary:
        -- @--match \"interns nothing\"@.
        let sp = StructurePiece dungeonPack "wall" (Just "ne")
            ge = ghostEnvAt packs FaceSouth (designationsFor sp)
            registered = concat
                [ [ aaHandle (paTexture a), aaHandle (paFacemap a) ]
                | pf ← packs, (_, a) ← parEntries (pfRegistration pf) ]
            drawnBy f = map sqTexture (V.toList (snd f))
        drawnBy (structureDesignationGhosts ge)
            `shouldSatisfy` (\hs → not (null hs) ∧ all (`elem` registered) hs)
        drawnBy (structurePreviewGhosts ge sp surfaceZ [homeTile])
            `shouldSatisfy` (\hs → not (null hs) ∧ all (`elem` registered) hs)

-- * Fixture plumbing

canon ∷ (Int, Int) → (Int, Int)
canon (gx, gy) = canonicalTile worldSize gx gy

edgeText ∷ WallEdge → Text
edgeText e = case e of
    WallNE → "ne"; WallNW → "nw"; WallSE → "se"; WallSW → "sw"

-- | One committed designation for this piece at the home tile.
designationsFor ∷ StructurePiece → ConstructDesignations
designationsFor = designationsFor' homeTile

designationsFor' ∷ (Int, Int) → StructurePiece → ConstructDesignations
designationsFor' tile sp = HM.singleton (canon tile)
    (newConstructDesignation surfaceZ (CtStructure sp) (ConstructAttemptId 1))

-- | The home tile carries a real floor and all four posts, so a POST
--   ghost has the supporting floor its final z comes from and a WALL
--   ghost has caps to resolve against. The occupied floor slot is why
--   the floor and wire examples designate at the neighbouring tile.
homeStructures ∷ ChunkStructures
homeStructures = structuresAt homeTile [SFloor, SPostN, SPostE, SPostS, SPostW]

structuresAt ∷ (Int, Int) → [StructureSlot] → ChunkStructures
structuresAt (gx, gy) slots = HM.fromList
    [ ((gx, gy, fromIntegral (fromEnum slot))
      , StructurePieceData 0 0 floorGridZ)
    | slot ← slots ]

postStructures ∷ [StructureSlot] → ChunkStructures
postStructures posts = structuresAt homeTile (SFloor : posts)

-- | The synthetic page: one chunk holding the home tile, flat at
--   'surfaceZ'.
homeChunk ∷ ChunkStructures → LoadedChunk
homeChunk structures = chunkAt (ChunkCoord 4 4) structures

seamChunkCoord ∷ ChunkCoord
seamChunkCoord = ChunkCoord (-15) 17

-- | A tile in the chunk stored one past the canonical u range — the
--   same seam fixture "Test.Headless.World.Render.StructureSeam" uses.
seamTile ∷ (Int, Int)
seamTile = ((-15) * chunkSize, 17 * chunkSize)

chunkAt ∷ ChunkCoord → ChunkStructures → LoadedChunk
chunkAt coord structures =
    let area = chunkSize * chunkSize
        col = ColumnTiles
                { ctStartZ = 0
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

tileDataWith ∷ [LoadedChunk] → WorldTileData
tileDataWith chunks = WorldTileData
    { wtdChunks = HM.fromList [ (lcCoord lc, lc) | lc ← chunks ]
    , wtdMaxChunks = 200 }

cameraAt ∷ CameraFacing → (Int, Int) → Camera2D
cameraAt facing (gx, gy) =
    let (wx, wy) = gridToWorld facing gx gy
    in defaultCamera { camPosition = (wx, wy), camZoom = zoom
                     , camFacing = facing, camZSlice = zSlice }

boundsFor ∷ Camera2D → ViewBounds
boundsFor cam = computeViewBounds cam fbW fbH effDepth

ghostEnvAt ∷ [PackFixture] → CameraFacing → ConstructDesignations → GhostEnv
ghostEnvAt packs facing designs =
    ghostEnvWith packs facing designs homeStructures emptyStructureStage

ghostEnvWith ∷ [PackFixture] → CameraFacing → ConstructDesignations
             → ChunkStructures → StructureStage → GhostEnv
ghostEnvWith packs facing designs structures stage =
    ghostEnv packs facing homeTile [homeChunk structures] designs stage

seamGhostEnv ∷ [PackFixture] → CameraFacing → ConstructDesignations → GhostEnv
seamGhostEnv packs facing designs =
    ghostEnv packs facing seamTile
        [chunkAt seamChunkCoord (structuresAt seamTile [])] designs
        emptyStructureStage

ghostEnv ∷ [PackFixture] → CameraFacing → (Int, Int) → [LoadedChunk]
         → ConstructDesignations → StructureStage → GhostEnv
ghostEnv packs facing focus chunks designs stage = GhostEnv
    { geCatalog    = packWallCatalog packs
    , geLookupSlot = \h → fromIntegral (toInt h)
    , geTexSizes   = HM.empty
    , geFacing     = facing
    , geZSlice     = zSlice
    , geEffDepth   = effDepth
    , geTileAlpha  = tileAlpha
    , geViewBounds = boundsFor cam
    , geCamX       = fst (camPosition cam)
    , geCamY       = snd (camPosition cam)
    , gePlan       = PlanWorld
        { pwWorldSize    = worldSize
        , pwTiles        = tileDataWith chunks
        , pwStage        = stage
        , pwDesignations = designs
        , pwCatalog      = packCatalog packs
        , pwProposedWire = HS.empty
        }
    }
  where cam = cameraAt facing focus

-- | The preview over the home tile alone, for the states that only need
--   one candidate.
--
--   NO designations: a preview is what the player sees BEFORE committing,
--   and an outstanding designation at the tile is one of the things the
--   resolver refuses a new candidate for.
previewAt ∷ [PackFixture] → CameraFacing → StructurePiece
          → (Int, V.Vector SortableQuad)
previewAt packs facing sp =
    structurePreviewGhosts (ghostEnvAt packs facing HM.empty)
        sp surfaceZ [homeTile]

-- | The art the ghost would resolve, taken from the catalogue the same
--   way it does — so the reference below is built from #1842's answer
--   and not from a second guess at which cap or wire variant applies.
artFor ∷ GhostEnv → StructurePiece → PieceArt
artFor ge sp = fromMaybe (error "fixture art did not resolve") $
    resolveUnplacedArt (pwCatalog (gePlan ge)) (spPack sp) (spKind sp)
        (spEdge sp) (contextFor ge sp)

-- | The context the resolver derives. Restated from #1842's own rule so
--   a change to it fails here rather than agreeing with itself.
contextFor ∷ GhostEnv → StructurePiece → PieceArtContext
contextFor ge sp = case spKind sp of
    "wall" → defaultPieceArtContext
        { pacWallCaps = capsAt (spEdge sp) }
    _ → defaultPieceArtContext
  where
    capsAt mEdge =
        let (l, r) = wallEnds mEdge
        in WallCaps (postThere l) (postThere r)
    -- 'Structure.Facing.wallEdgeEnds', restated (canvas-left first) so
    -- a change to that table fails this suite rather than agreeing with
    -- itself.
    wallEnds mEdge = case mEdge of
        Just "nw" → (SPostW, SPostN)
        Just "se" → (SPostS, SPostE)
        Just "sw" → (SPostW, SPostS)
        _         → (SPostN, SPostE)
    postThere slot = HM.member
        (fst homeTile, snd homeTile, fromIntegral (fromEnum slot))
        (maybe HM.empty lcStructures
            (HM.lookup (ChunkCoord 4 4)
                (wtdChunks (pwTiles (gePlan ge)))))

-- | The PLACED-piece producer for the same art at the same z, reached
--   through the texture-palette entry point the ghost never uses. This
--   is what a ghost is compared against.
placedQuads ∷ [PackFixture] → CameraFacing → StructureSlot → PieceArt → Int
            → [SortableQuad]
placedQuads packs = placedQuadsAt packs homeTile

placedQuadsAt ∷ [PackFixture] → (Int, Int) → CameraFacing → StructureSlot
              → PieceArt → Int → [SortableQuad]
placedQuadsAt packs tile facing slot art gridZ =
    structurePieceQuads (packWallCatalog packs) palette handles
        (\h → fromIntegral (toInt h)) HM.empty facing zSlice effDepth 1.0
        (fst tile) (snd tile) slot (StructurePieceData texId faceId gridZ)
  where
    (texId, p1)  = internPath (aaPath (paTexture art)) emptyTexPalette
    (faceId, palette) = internPath (aaPath (paFacemap art)) p1
    handles = HM.fromList
        [ (texId,  aaHandle (paTexture art))
        , (faceId, aaHandle (paFacemap art)) ]

-- | Which wire CONNECTION VARIANT each candidate's ghost drew, named by
--   the shape rather than by a bare handle — the catalogue answers one
--   texture per variant, so the handle identifies the shape uniquely and
--   the reverse lookup is what makes a failure readable.
wireShapesOf ∷ [PackFixture] → GhostEnv → StructurePiece → [(Int, Int)]
             → [Maybe WireShape]
wireShapesOf packs ge sp tiles =
    [ lookup h byHandle
    | h ← map sqTexture (V.toList
              (snd (structurePreviewGhosts ge sp surfaceZ tiles))) ]
  where
    byHandle =
        [ (aaHandle (paTexture a), shape)
        | shape ← [minBound .. maxBound]
        , Just a ← [ resolveUnplacedArt (packCatalog packs) wirePack "wire"
                         Nothing
                         (defaultPieceArtContext { pacWireShape = shape }) ] ]

validPreviewTint ∷ Vec4
validPreviewTint = Vec4 1 1 1 (tileAlpha * previewGhostAlpha)

invalidTint ∷ Vec4
invalidTint =
    let Vec4 r g b _ = ghostTint False
    in Vec4 r g b (tileAlpha * previewGhostAlpha)

-- | Do all these tints carry this RGB and (within Float slop) this
--   alpha? The alphas are products of two Float factors, so an exact
--   literal comparison would be pinning a rounding mode rather than the
--   contract.
allTints ∷ (Float, Float, Float) → Float → [Vec4] → Bool
allTints (er, eg, eb) ea tints = not (null tints) ∧ all ok tints
  where
    ok (Vec4 r g b a) =
        (r, g, b) ≡ (er, eg, eb) ∧ abs (a - ea) < 1.0e-5

-- | Every quad's tint, deduplicated in encounter order — a ghost's
--   whole quad set shares one.
quadTints ∷ V.Vector SortableQuad → [Vec4]
quadTints = L.nub ∘ map (color ∘ sqV0) ∘ V.toList

-- | One quad, projected onto everything it carries EXCEPT its tint, so
--   two ghost states — or a ghost and the placed piece it previews — can
--   be compared on all of it at once. 'SortableQuad' has no 'Eq'
--   instance, which is why this is spelled out rather than derived.
type QuadShape =
    (Float, Int, String, [(Float, Float, Float, Float, Float, Float, Word32, String)])

stripTints ∷ V.Vector SortableQuad → [QuadShape]
stripTints = map quadShape ∘ V.toList

quadShape ∷ SortableQuad → QuadShape
quadShape sq =
    ( sqSortKey sq
    , toInt (sqTexture sq)
    , show (sqLayer sq)
    , map vertexShape (quadVerts sq) )

-- | The same projection with the screen POSITION dropped — what a seam
--   translation is allowed to change, and nothing else.
quadShapeNoPos ∷ SortableQuad → (Float, Int, String, [(Float, Float, Float, Float, Word32, String)])
quadShapeNoPos sq =
    ( sqSortKey sq, toInt (sqTexture sq), show (sqLayer sq)
    , [ (tu, tv, atlasId v, faceMapId v, renderFlags v, show (worldUV v))
      | v ← quadVerts sq, let Vec2 tu tv = tex v ] )

vertexShape ∷ Vertex
            → (Float, Float, Float, Float, Float, Float, Word32, String)
vertexShape v =
    let Vec2 px py = pos v
        Vec2 tu tv = tex v
    in (px, py, tu, tv, atlasId v, faceMapId v, renderFlags v
       , show (worldUV v))

quadVerts ∷ SortableQuad → [Vertex]
quadVerts sq = [sqV0 sq, sqV1 sq, sqV2 sq, sqV3 sq]
