{-# LANGUAGE Strict #-}
-- | Pure tests for Chop's screen-space selection oracle
--   ('World.Flora.HitTest'), issue #1856.
--
--   D-9 rejected keeping the tile-coordinate rectangle under the new
--   press-drag gesture: around cliffs a tile rectangle's selected set
--   disagrees with the box the player actually drew, because elevation,
--   sub-tile offsets and sprite geometry move a tree's rendered position
--   away from its tile's. These examples drive the real oracle over
--   synthetic chunks and pin that the SCREEN-SPACE rule is the one in
--   force — including a cliff case constructed so the two rules give
--   different answers, which is what makes the assertion meaningful
--   rather than merely consistent.
--
--   Run just this gate: @cabal test synarchy-test-headless
--   --test-options='--match "Chop selection"'@.
module Test.Headless.World.Chop.Selection (spec) where

import UPrelude
import Test.Hspec
import qualified Data.HashMap.Strict as HM
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU
import Data.List (sort, nub)
import Data.Maybe (mapMaybe)
import Control.Arrow ((&&&))
import qualified Codec.Picture as JP
import qualified Data.ByteString as BS
import Engine.Asset.Handle (TextureHandle(..), toInt)
import Engine.Graphics.Camera (CameraFacing(..), Camera2D(..), defaultCamera)
import Structure.Types
    (StructureSlot(..), StructurePieceData(..), emptyChunkStructures)
import World.Chop.Types (ChopDesignations, newChopDesignation)
import World.Chunk.Types
    (ChunkCoord(..), ColumnTiles(..), LoadedChunk(..), chunkSize, columnIndex)
import World.Flora.HitTest
import World.Flora.Identity (FloraInstanceId, generatedFloraInstanceId)
import World.Flora.Types
import World.Fluid.Types (emptyIceMap)
import World.Generate (chunkToGlobal)
import World.Grid (gridToWorld, tileHeight, tileWidth)
import World.Render.FloraDraws (FloraDraw(..), chunkFloraDraws)
import World.Render.FloraMarker (floraMarkerQuad)
import World.Render.FloraProjection (FloraGeom(..))
import World.Render.SpriteDepth
    (frameFrontWallLift, liftSpriteSortKey, noFrontWallLift
    , structureFrontWallClear)
import World.Render.Textures.Types (defaultWorldTextures)
import World.Render.TileQuads (worldCursorToQuad)
import qualified Data.Map as Map
import Engine.Scene.Types (SortableQuad(..))
import Engine.Scene.Types.Batch (sortQuadsByLayer, quadPainterOrder)
import World.Render.FloraQuads (floraToQuad)
import Engine.Graphics.Vulkan.Types.Vertex
    (Vec2(..), Vertex, pos, faceMapId, noFaceMapVertexId)
import World.Render.ViewBounds (computeViewBounds)
import World.Tile.Types (WorldTileData(..))

-- * Fixture

worldSize, zSlice, effDepth, fbW, fbH, winW, winH ∷ Int
worldSize = 64
zSlice    = 24
effDepth  = 250
fbW = 800
fbH = 600
-- A window far larger than the framebuffer: the pixel→world step stays
-- well under a tile, so a rounded target pixel cannot land on a
-- neighbour and the ASSERTIONS are about the rule, not about rounding.
winW = 8000
winH = 6000

zoom ∷ Float
zoom = 20.0

-- | Handles: one tree texture and one shorter shrub texture, so the
--   quad-size half of the projection is actually exercised.
treeTex, treeTallTex, shrubTex ∷ TextureHandle
treeTex     = TextureHandle 11
shrubTex    = TextureHandle 12
-- | A different HANDLE at the SAME pixel size as 'treeTex', so a
--   fixture can tie two sprites' whole geometry and leave the texture
--   as the only separator.
treeTallTex = TextureHandle 13

-- | 96x128 trees and 96x64 shrubs — the shrub is exactly one tile, the
--   tree twice as tall, as real flora art is.
texSizes ∷ HM.HashMap TextureHandle (Int, Int)
texSizes = HM.fromList
    [(treeTex, (96, 128)), (treeTallTex, (96, 128)), (shrubTex, (96, 64))]

woodId, woodTallId, shrubId, mossId ∷ FloraId
woodId     = FloraId 1
shrubId    = FloraId 2
mossId     = FloraId 3
-- | A SECOND choppable species, so a fixture can stack two Chop
--   candidates at one spot and have only their texture tell them apart.
woodTallId = FloraId 4

-- | @oak@ is the choppable species: harvestable, tagged @wood@.
--   @thicket@ is harvestable but tagged @fruit@ — a berry bush, never a
--   chop target. @moss@ has no harvest block at all.
catalog ∷ FloraCatalog
catalog =
    insertSpecies mossId (newFloraSpecies "moss" shrubTex)
    $ insertSpecies shrubId
        (newFloraSpecies "thicket" shrubTex)
            { fsHarvest = Just (harvest ["fruit"]) }
    $ insertSpecies woodTallId
        (newFloraSpecies "elm" treeTallTex)
            { fsHarvest = Just (harvest ["wood"]) }
    $ insertSpecies woodId
        (newFloraSpecies "oak" treeTex)
            { fsHarvest = Just (harvest ["wood"]) }
      emptyFloraCatalog
  where
    -- A real depleted texture: a regrowing plant with none is drawn
    -- as bare ground by the renderer and is therefore unclickable by
    -- construction. The erase cases below depend on it still being on
    -- screen.
    harvest tags = FloraHarvest
        { fhTags = tags, fhYield = [], fhRegrowth = 86400
        , fhHarvestedTexture = shrubTex }

instanceId ∷ Int → FloraInstanceId
instanceId n = generatedFloraInstanceId "spec" 0 0 "oak" n

-- | A plant on a chunk-local tile with no sub-tile offset unless given.
plantAt ∷ Int → FloraId → (Int, Int) → FloraInstance
plantAt ordinal species (lx, ly) = FloraInstance
    { fiSpecies = species
    , fiTileX = fromIntegral lx, fiTileY = fromIntegral ly
    , fiOffU = 0, fiOffV = 0
    , fiZ = zSlice            -- overwritten by the live surface z
    , fiAge = 100, fiHealth = 1, fiVariant = 0
    , fiBaseWidth = 16
    , fiInstanceId = instanceId ordinal
    , fiChopDesignated = False
    }

-- | A chunk whose columns are solid up to @heightAt@, carrying @plants@.
--   @heightAt@ is what builds the CLIFF: two adjacent tiles at wildly
--   different surface z draw their trees at wildly different screen Y.
chunkWith
    ∷ ChunkCoord → ((Int, Int) → Int) → [FloraInstance] → LoadedChunk
chunkWith coord heightAt plants =
    let area = chunkSize * chunkSize
        tileOfIndex i = (i `mod` chunkSize, i `div` chunkSize)
        colAt (lx, ly) =
            let top = heightAt (lx, ly)
            in ColumnTiles
                { ctStartZ = 0
                , ctMats   = VU.replicate (top + 1) 1
                , ctSlopes = VU.replicate (top + 1) 0
                , ctVeg    = VU.replicate (top + 1) 0
                }
        -- Addressed through 'columnIndex' itself, so a fixture cliff
        -- lands on the tile the renderer reads rather than its
        -- transpose.
        cols = V.generate area $ \i → colAt (tileOfIndex i)
        surf = VU.generate area $ \i → heightAt (tileOfIndex i)
    in LoadedChunk
        { lcCoord = coord
        , lcTiles = cols
        , lcSurfaceMap = surf
        , lcTerrainSurfaceMap = surf
        , lcFluidMap = V.replicate area Nothing
        , lcIceMap = emptyIceMap
        , lcFlora = FloraChunkData plants
        , lcSideDeco = VU.empty, lcWaterTableMap = VU.empty
        , lcMagma = Nothing, lcStructures = emptyChunkStructures
        }

-- | 'columnIndex' is the authority for how a column is addressed, so
--   the fixture's own generators must agree with it or a \"cliff\" would
--   sit on a tile the renderer never reads.
columnOrderAgrees ∷ Bool
columnOrderAgrees = and
    [ columnIndex lx ly ≡ ly * chunkSize + lx
    | lx ← [0 .. chunkSize - 1], ly ← [0 .. chunkSize - 1] ]

viewOf
    ∷ CameraFacing → (Float, Float) → WorldTileData → ChopDesignations
    → FloraHitView
viewOf facing camPos tiles designated =
    (viewOfWith facing camPos tiles designated)
        { fhvFrontWall = noFrontWallLift }

-- | 'viewOf' keeping the REAL front-wall lift the production snapshot
--   builds — what the structure-overlap case needs, and what
--   'viewOf' deliberately neutralises so every other case pins the
--   unlifted geometry.
viewOfWith
    ∷ CameraFacing → (Float, Float) → WorldTileData → ChopDesignations
    → FloraHitView
viewOfWith facing (camX, camY) tiles designated =
    let cam = defaultCamera { camPosition = (camX, camY)
                            , camZoom = zoom
                            , camFacing = facing
                            , camZSlice = zSlice }
    in FloraHitView
        { fhvFacing = facing, fhvZoom = zoom, fhvZSlice = zSlice
        , fhvCamX = camX, fhvCamY = camY
        , fhvFbW = fbW, fhvFbH = fbH, fhvWinW = winW, fhvWinH = winH
        , fhvWorldSize = worldSize, fhvEffDepth = effDepth
        , fhvViewBounds = computeViewBounds cam fbW fbH effDepth
        , fhvTiles = tiles, fhvCatalog = catalog
        , fhvHarvests = HM.empty, fhvDesignated = designated
        , fhvTexSizes = texSizes
        , fhvDaysPerYear = 360, fhvAbsDay = 100
        , fhvFrontWall = frameFrontWallLift facing worldSize zSlice
                             effDepth (wtdChunks tiles)
        }

-- | World coordinate → the window pixel the oracle unprojects back to
--   it. The exact inverse of 'World.Flora.HitTest''s own projection, so
--   a case can aim at a tree's anchor rather than guess at a pixel.
pixelOf ∷ FloraHitView → (Float, Float) → (Float, Float)
pixelOf view (wx, wy) =
    let aspect = fromIntegral fbW / fromIntegral fbH ∷ Float
        vw = zoom * aspect
        vh = zoom
    in ( fromIntegral winW * (((wx - fhvCamX view) / vw) + 1) / 2
       , fromIntegral winH * (((wy - fhvCamY view) / vh) + 1) / 2 )

-- | The anchor the oracle computed for one instance, as a window pixel.
anchorPixel ∷ FloraHitView → FloraInstanceId → Maybe (Float, Float)
anchorPixel view iid = listToMaybe
    [ pixelOf view (fgAnchorX g, fgAnchorY g)
    | (p, g) ← floraSelectCandidates view (SelectChoppable "wood")
    , fpInstanceId p ≡ iid ]

boxAround ∷ (Float, Float) → Float → (Float, Float, Float, Float)
boxAround (px, py) r = (px - r, py - r, px + r, py + r)

tilesOf ∷ [LoadedChunk] → WorldTileData
tilesOf lcs = WorldTileData
    { wtdChunks = HM.fromList [(lcCoord lc, lc) | lc ← lcs]
    , wtdMaxChunks = 200 }

flatChunk ∷ ChunkCoord
flatChunk = ChunkCoord 0 0

flat ∷ (Int, Int) → Int
flat _ = zSlice

-- | Camera parked on a chunk-local tile of the origin chunk.
camOn ∷ CameraFacing → (Int, Int) → (Float, Float)
camOn facing (lx, ly) =
    let (gx, gy) = chunkToGlobal flatChunk lx ly
    in gridToWorld facing gx gy

spec ∷ Spec
spec = describe "Chop selection" $ do

    it "the fixture's column addressing matches columnIndex" $
        columnOrderAgrees `shouldBe` True

    describe "click selection" $ do

        it "picks the tree whose rendered sprite contains the pointer" $ do
            let oak = plantAt 1 woodId (8, 8)
                view = viewOf FaceNorth (camOn FaceNorth (8, 8))
                           (tilesOf [chunkWith flatChunk flat [oak]])
                           HM.empty
            Just px ← pure (anchorPixel view (instanceId 1))
            -- The anchor is the trunk base; the sprite covers it.
            fmap fpInstanceId
                (pickFloraAt view (SelectChoppable "wood") (fst px) (snd px))
                `shouldBe` Just (instanceId 1)

        it "picks nothing where no sprite is drawn" $ do
            let oak = plantAt 1 woodId (8, 8)
                view = viewOf FaceNorth (camOn FaceNorth (8, 8))
                           (tilesOf [chunkWith flatChunk flat [oak]])
                           HM.empty
            Just (px, py) ← pure (anchorPixel view (instanceId 1))
            -- Far to the right of the sprite, still on screen.
            pickFloraAt view (SelectChoppable "wood") (px + 3000) py
                `shouldBe` Nothing

        it "picks the topmost of two overlapping sprites, by painter depth" $ do
            -- Two oaks on adjacent tiles along the depth axis. The one
            -- the renderer draws LAST (larger sort key) must win.
            let near = plantAt 1 woodId (8, 8)
                far  = plantAt 2 woodId (7, 7)
                tiles = tilesOf [chunkWith flatChunk flat [far, near]]
                view = viewOf FaceNorth (camOn FaceNorth (8, 8))
                           tiles HM.empty
                cands = floraSelectCandidates view (SelectChoppable "wood")
                keyOf iid = listToMaybe
                    [ fgSortKey g | (p, g) ← cands, fpInstanceId p ≡ iid ]
            -- Which of the two the renderer paints LAST is the
            -- projection's answer, not the fixture's assumption — the
            -- test is that the picker agrees with it.
            let topmost = if keyOf (instanceId 1) > keyOf (instanceId 2)
                            then instanceId 1 else instanceId 2
                other   = if topmost ≡ instanceId 1
                            then instanceId 2 else instanceId 1
            keyOf topmost `shouldSatisfy` \k → k > keyOf other
            Just (px, py) ← pure (anchorPixel view topmost)
            -- A point inside BOTH quads: just above the topmost tree's
            -- anchor is still inside its neighbour's taller quad.
            fmap fpInstanceId
                (pickFloraAt view (SelectChoppable "wood") px (py - 4))
                `shouldBe` Just topmost

    describe "box selection" $ do

        it "selects the trees whose ground anchors are inside the box" $ do
            let a = plantAt 1 woodId (8, 8)
                b = plantAt 2 woodId (4, 4)
                view = viewOf FaceNorth (camOn FaceNorth (8, 8))
                           (tilesOf [chunkWith flatChunk flat [a, b]])
                           HM.empty
            Just pa ← pure (anchorPixel view (instanceId 1))
            let (x1, y1, x2, y2) = boxAround pa 40
            map fpInstanceId
                (pickFloraInRect view (SelectChoppable "wood") x1 y1 x2 y2)
                `shouldBe` [instanceId 1]

        it "normalizes either drag direction to the same set" $ do
            let a = plantAt 1 woodId (8, 8)
                view = viewOf FaceNorth (camOn FaceNorth (8, 8))
                           (tilesOf [chunkWith flatChunk flat [a]]) HM.empty
            Just pa ← pure (anchorPixel view (instanceId 1))
            let (x1, y1, x2, y2) = boxAround pa 40
                forward  = pickFloraInRect view (SelectChoppable "wood")
                               x1 y1 x2 y2
                backward = pickFloraInRect view (SelectChoppable "wood")
                               x2 y2 x1 y1
            map fpInstanceId backward `shouldBe` map fpInstanceId forward

        it "uses CLOSED bounds — a tree exactly on the edge is inside" $ do
            let a = plantAt 1 woodId (8, 8)
                view = viewOf FaceNorth (camOn FaceNorth (8, 8))
                           (tilesOf [chunkWith flatChunk flat [a]]) HM.empty
            Just (px, py) ← pure (anchorPixel view (instanceId 1))
            map fpInstanceId
                (pickFloraInRect view (SelectChoppable "wood")
                    px py (px + 50) (py + 50))
                `shouldBe` [instanceId 1]

        it "selects nothing through a degenerate viewport" $ do
            let a = plantAt 1 woodId (8, 8)
                base = viewOf FaceNorth (camOn FaceNorth (8, 8))
                           (tilesOf [chunkWith flatChunk flat [a]]) HM.empty
                dead = base { fhvWinW = 0, fhvWinH = 0 }
            pickFloraInRect dead (SelectChoppable "wood") 0 0 9999 9999
                `shouldBe` []
            pickFloraAt dead (SelectChoppable "wood") 10 10
                `shouldBe` Nothing

        it "selects nothing through a zero-width framebuffer" $ do
            let a = plantAt 1 woodId (8, 8)
                base = viewOf FaceNorth (camOn FaceNorth (8, 8))
                           (tilesOf [chunkWith flatChunk flat [a]]) HM.empty
                dead = base { fhvFbW = 0 }
            pickFloraInRect dead (SelectChoppable "wood") 0 0 9999 9999
                `shouldBe` []

    describe "the cliff case (D-9)" $

        it "disagrees with a tile rectangle, and the screen rule wins" $ do
            -- Two oaks on ADJACENT tiles, one on a plateau 12 z-levels
            -- above the other. A tile rectangle covering both tiles
            -- would take both trees; on screen the high one is lifted a
            -- long way out of a box drawn around the low one.
            let lowTile  = (8, 8)
                highTile = (8, 9)
                heights t = if t ≡ highTile then zSlice else zSlice - 12
                -- Both columns stay solid ground; only their
                -- SURFACE differs, which is what a cliff is.
                low  = plantAt 1 woodId lowTile
                high = plantAt 2 woodId highTile
                tiles = tilesOf [chunkWith flatChunk heights [low, high]]
                view = viewOf FaceNorth (camOn FaceNorth lowTile)
                           tiles HM.empty
            -- Both are eligible and both are drawn: the box is the only
            -- thing separating them.
            sort (map (fpInstanceId . fst)
                      (floraSelectCandidates view (SelectChoppable "wood")))
                `shouldBe` sort [instanceId 1, instanceId 2]
            Just pLow ← pure (anchorPixel view (instanceId 1))
            let (x1, y1, x2, y2) = boxAround pLow 30
            map fpInstanceId
                (pickFloraInRect view (SelectChoppable "wood") x1 y1 x2 y2)
                `shouldBe` [instanceId 1]
            -- And the tile rule really would have taken both: the two
            -- trees are one tile apart.
            let tileOf iid = listToMaybe
                    [ (fpGX p, fpGY p)
                    | (p, _) ← floraSelectCandidates view
                                   (SelectChoppable "wood")
                    , fpInstanceId p ≡ iid ]
            (,) <$> tileOf (instanceId 1) <*> tileOf (instanceId 2)
                `shouldSatisfy` \m → case m of
                    Just ((ax, ay), (bx, by)) →
                        abs (ax - bx) ≤ 1 ∧ abs (ay - by) ≤ 1
                    Nothing → False

    describe "facings and the seam" $ do

        it "selects the same tree at every camera facing" $
            forM_ [FaceNorth, FaceEast, FaceSouth, FaceWest] $
              \facing → do
                let a = plantAt 1 woodId (8, 8)
                    view = viewOf facing (camOn facing (8, 8))
                               (tilesOf [chunkWith flatChunk flat [a]])
                               HM.empty
                Just pa ← pure (anchorPixel view (instanceId 1))
                let (x1, y1, x2, y2) = boxAround pa 30
                map fpInstanceId
                    (pickFloraInRect view (SelectChoppable "wood")
                        x1 y1 x2 y2)
                    `shouldBe` [instanceId 1]

        it "selects a tree stored across the U seam, at every facing" $
            -- Chunk u = 32 is one past the canonical range and is STORED
            -- under u = -32. The wrap offset the oracle takes from the
            -- renderer's own culling is what puts the sprite where the
            -- player sees it; without it, this box selects nothing.
            forM_ [FaceNorth, FaceEast, FaceSouth, FaceWest] $
              \facing → do
                let seamChunk = ChunkCoord (-32) 3
                    a = plantAt 1 woodId (8, 8)
                    tiles = tilesOf [chunkWith seamChunk flat [a]]
                    (gx, gy) = chunkToGlobal seamChunk 8 8
                    view = viewOf facing (gridToWorld facing gx gy)
                               tiles HM.empty
                Just pa ← pure (anchorPixel view (instanceId 1))
                let (x1, y1, x2, y2) = boxAround pa 30
                map fpInstanceId
                    (pickFloraInRect view (SelectChoppable "wood")
                        x1 y1 x2 y2)
                    `shouldBe` [instanceId 1]

    describe "eligibility is unchanged" $ do

        it "never selects a species with no harvest block" $ do
            let m = plantAt 1 mossId (8, 8)
                view = viewOf FaceNorth (camOn FaceNorth (8, 8))
                           (tilesOf [chunkWith flatChunk flat [m]]) HM.empty
            floraSelectCandidates view (SelectChoppable "wood") `shouldSatisfy` null

        it "never selects a harvestable species without the wood tag" $ do
            let b = plantAt 1 shrubId (8, 8)
                view = viewOf FaceNorth (camOn FaceNorth (8, 8))
                           (tilesOf [chunkWith flatChunk flat [b]]) HM.empty
            floraSelectCandidates view (SelectChoppable "wood") `shouldSatisfy` null

        it "never selects a tree with a live regrowth timer" $ do
            let a = plantAt 1 woodId (8, 8)
                base = viewOf FaceNorth (camOn FaceNorth (8, 8))
                           (tilesOf [chunkWith flatChunk flat [a]]) HM.empty
                regrowing = base
                    { fhvHarvests = HM.fromList [(instanceId 1, 500)] }
            floraSelectCandidates regrowing (SelectChoppable "wood")
                `shouldSatisfy` null

        it "never selects a tree outside the visible z band" $ do
            let a = plantAt 1 woodId (8, 8)
                deep t = if t ≡ (8, 8) then 1 else zSlice
                view = (viewOf FaceNorth (camOn FaceNorth (8, 8))
                           (tilesOf [chunkWith flatChunk deep [a]]) HM.empty)
                       { fhvEffDepth = 2 }
            floraSelectCandidates view (SelectChoppable "wood")
                `shouldSatisfy` null

    describe "exact identities on one tile" $

        it "designates only the tree whose anchor was inside the box" $ do
            -- Two oaks sharing ONE tile, separated only by their
            -- sub-tile offsets — the case a tile key cannot express at
            -- all (#1854), and the reason selection must be per-sprite.
            let east = (plantAt 1 woodId (8, 8)) { fiOffU = 0.5 }
                west = (plantAt 2 woodId (8, 8)) { fiOffU = -0.5 }
                view = viewOf FaceNorth (camOn FaceNorth (8, 8))
                           (tilesOf [chunkWith flatChunk flat [east, west]])
                           HM.empty
            Just pe ← pure (anchorPixel view (instanceId 1))
            Just pw ← pure (anchorPixel view (instanceId 2))
            -- The fixture must really separate them on screen, or the
            -- box below would not be distinguishing anything.
            abs (fst pe - fst pw) `shouldSatisfy` (> 8)
            let (x1, y1, x2, y2) = boxAround pe 3
            map fpInstanceId
                (pickFloraInRect view (SelectChoppable "wood") x1 y1 x2 y2)
                `shouldBe` [instanceId 1]

    describe "the erase candidate set" $ do

        it "is what is designated, not what is add-eligible" $ do
            -- A regrowing tree is NOT add-eligible, but a designation
            -- standing on it must still be clearable (D-12).
            let a = plantAt 1 woodId (8, 8)
                designated = HM.fromList
                    [(instanceId 1, newChopDesignation zSlice 8 8)]
                view = (viewOf FaceNorth (camOn FaceNorth (8, 8))
                           (tilesOf [chunkWith flatChunk flat [a]])
                           designated)
                       { fhvHarvests = HM.fromList [(instanceId 1, 500)] }
            floraSelectCandidates view (SelectChoppable "wood")
                `shouldSatisfy` null
            map (fpInstanceId . fst)
                (floraSelectCandidates view SelectDesignated)
                `shouldBe` [instanceId 1]

        it "erases exactly what an add over the same input would have added" $ do
            let a = plantAt 1 woodId (8, 8)
                b = plantAt 2 woodId (4, 4)
                tiles = tilesOf [chunkWith flatChunk flat [a, b]]
                add = viewOf FaceNorth (camOn FaceNorth (8, 8))
                          tiles HM.empty
            Just pa ← pure (anchorPixel add (instanceId 1))
            let (x1, y1, x2, y2) = boxAround pa 40
                added = pickFloraInRect add (SelectChoppable "wood")
                            x1 y1 x2 y2
                designated = HM.fromList
                    [ (fpInstanceId p, newChopDesignation zSlice 0 0)
                    | p ← added ]
                erase = viewOf FaceNorth (camOn FaceNorth (8, 8))
                            tiles designated
            map fpInstanceId (pickFloraInRect erase SelectDesignated
                                  x1 y1 x2 y2)
                `shouldBe` map fpInstanceId added

    describe "quad geometry" $

        it "sizes the click box from the frame's own texture, not a tile" $ do
            -- A 96x128 tree quad is two tile-heights tall; the oracle
            -- must read the real dimensions or a click near the canopy
            -- misses the tree entirely.
            let a = plantAt 1 woodId (8, 8)
                view = viewOf FaceNorth (camOn FaceNorth (8, 8))
                           (tilesOf [chunkWith flatChunk flat [a]]) HM.empty
            case floraSelectCandidates view (SelectChoppable "wood") of
                [(_, g)] → do
                    fgQuadW g `shouldBe` tileWidth
                    fgQuadH g `shouldBe` tileHeight * 2
                _ → expectationFailure "expected exactly one candidate"

    describe "painter depth is the renderer's own" $ do

        it "ranks a front-wall-lifted tree where the renderer painted it" $ do
            -- #418: a sprite in front of a structure's front wall is
            -- raised to draw over it. That lift is part of its FINAL
            -- painter depth, so a picker reading the unlifted key would
            -- rank a lifted tree behind a sprite the renderer painted
            -- underneath it. The oracle shares the very function the
            -- render pass applies.
            let treeTile = (8, 8)
                (tgx, tgy) = chunkToGlobal flatChunk (fst treeTile) (snd treeTile)
                -- The geometry 'Test.Headless.World.Render.FrontWallLift'
                -- pins as the interior baseline: a screen-front SE wall
                -- two tiles west of the sprite, whose own anchor depth
                -- ((wgx+1) + (wgy+1)) equals the sprite's, at the
                -- sprite's z so it is inside the slice band.
                wall = HM.fromList
                    [ ((tgx - 2, tgy, seTag), StructurePieceData 0 0 zSlice) ]
                oak = plantAt 1 woodId treeTile
                lc = (chunkWith flatChunk flat [oak]) { lcStructures = wall }
                tiles = tilesOf [lc]
                view = viewOfWith FaceSouth (camOn FaceSouth treeTile)
                           tiles HM.empty
                plain = viewOf FaceSouth (camOn FaceSouth treeTile)
                            tiles HM.empty
                keyIn v = listToMaybe
                    [ fgSortKey g
                    | (pk, g) ← floraSelectCandidates v (SelectChoppable "wood")
                    , fpInstanceId pk ≡ instanceId 1 ]
            -- The fixture must actually trigger a lift, or this proves
            -- nothing about sharing it.
            structureFrontWallClear FaceSouth worldSize zSlice effDepth
                (\cc → if cc ≡ flatChunk then Just wall else Nothing) tgx tgy
                `shouldSatisfy` isJust
            -- …and the oracle must report the LIFTED key, not the base.
            keyIn view `shouldSatisfy` \k → k > keyIn plain
            -- Which is exactly what the shared lift computes.
            case (keyIn plain, keyIn view) of
                (Just base, Just lifted) →
                    lifted `shouldBe` liftSpriteSortKey
                        (fhvFrontWall view) flatChunk tgx tgy base
                _ → expectationFailure "the tree was not a candidate"

        it "leaves a tree nowhere near a structure at its own key" $ do
            let oak = plantAt 1 woodId (8, 8)
                tiles = tilesOf [chunkWith flatChunk flat [oak]]
                keyIn v = listToMaybe
                    [ fgSortKey g
                    | (pk, g) ← floraSelectCandidates v (SelectChoppable "wood")
                    , fpInstanceId pk ≡ instanceId 1 ]
            keyIn (viewOfWith FaceSouth (camOn FaceSouth (8, 8)) tiles HM.empty)
                `shouldBe` keyIn (viewOf FaceSouth (camOn FaceSouth (8, 8))
                                      tiles HM.empty)

        it "agrees with what the REAL sorter drew last, at an equal depth" $ do
            -- The whole claim, tested end to end rather than asserted:
            -- build the two co-tenants' quads through the production
            -- 'floraToQuad', push them through the production
            -- 'sortQuadsByLayer', and check the picker names the one
            -- that came out LAST. Their 'sqSortKey's are equal (same
            -- tile, same z, same fiOffV), so before #1856's total order
            -- this compared an unstable sort's arbitrary output against
            -- an invented rule.
            let a = (plantAt 1 woodId (8, 8)) { fiOffU = -0.4, fiOffV = 0 }
                b = (plantAt 2 woodId (8, 8)) { fiOffU =  0.4, fiOffV = 0 }
                tiles = tilesOf [chunkWith flatChunk flat [a, b]]
                view = viewOf FaceNorth (camOn FaceNorth (8, 8)) tiles HM.empty
                cands = floraSelectCandidates view (SelectChoppable "wood")
                (tgx, tgy) = chunkToGlobal flatChunk 8 8
                quadFor inst = floraToQuad (fromIntegral . toInt)
                    defaultWorldTextures FaceNorth tgx tgy
                    (inst { fiZ = zSlice }) treeTex zSlice effDepth 1.0 (0, 0)
                    texSizes
                quads = V.fromList (mapMaybe quadFor [a, b])
            V.length quads `shouldBe` 2
            -- The fixture must really tie, or the sort key alone would
            -- have separated them and this proves nothing.
            nub (map sqSortKey (V.toList quads)) `shouldSatisfy` \ks →
                length ks ≡ 1
            let sorted = Map.foldr (\v acc → V.toList v ⧺ acc) []
                             (sortQuadsByLayer quads)
                drawnLast = last sorted
                -- Which instance that quad belongs to, by its top-left
                -- corner — the very number the shared order compares.
                lastInstance = listToMaybe
                    [ fpInstanceId pk
                    | (pk, g) ← cands
                    , let Vec2 qx _ = pos (sqV0 drawnLast)
                    , abs (fgDrawX g - qx) < 0.0001 ]
            Just px ← pure (anchorPixel view (instanceId 1))
            Just pw ← pure (anchorPixel view (instanceId 2))
            -- A point inside BOTH quads: midway between the anchors.
            let midX = (fst px + fst pw) / 2
            fmap fpInstanceId
                (pickFloraAt view (SelectChoppable "wood") midX (snd px - 4))
                `shouldBe` lastInstance

        it "separates an EXACT-rectangle tie by texture, in the render key" $ do
            -- Two DIFFERENT species stacked at exactly the same tile,
            -- z and sub-tile offsets: identical depth AND identical
            -- rect, so only the texture is left to order them — and it
            -- has to be in the RENDER key, not just the picker's, or a
            -- click would disagree with whichever the sorter drew last.
            --
            -- The shrub is given the wood tag here so both are Chop
            -- candidates; what is under test is ordering, not
            -- eligibility.
            let oak   = plantAt 1 woodId  (8, 8)
                other = plantAt 2 woodTallId (8, 8)
                tiles = tilesOf [chunkWith flatChunk flat [oak, other]]
                view = viewOf FaceNorth (camOn FaceNorth (8, 8)) tiles HM.empty
                cands = floraSelectCandidates view (SelectChoppable "wood")
                (tgx, tgy) = chunkToGlobal flatChunk 8 8
                quadFor inst tex = floraToQuad (fromIntegral . toInt)
                    defaultWorldTextures FaceNorth tgx tgy
                    (inst { fiZ = zSlice }) tex zSlice effDepth 1.0 (0, 0)
                    texSizes
                quads = V.fromList (mapMaybe id
                    [quadFor oak treeTex, quadFor other treeTallTex])
            V.length quads `shouldBe` 2
            -- The fixture really is an exact depth AND rect tie.
            nub (map sqSortKey (V.toList quads)) `shouldSatisfy` \ks →
                length ks ≡ 1
            nub (map (rectOf . sqV0 &&& rectOf . sqV2) (V.toList quads))
                `shouldSatisfy` \rs → length rs ≡ 1
            -- Only the texture separates them, and it does.
            nub (map sqTexture (V.toList quads)) `shouldSatisfy` \ts →
                length ts ≡ 2
            -- The load-bearing assertion: the RENDER key must separate
            -- them. Without this the sort below is comparing EQ and its
            -- output is arbitrary, so \"the picker matched what came out
            -- last\" could pass on a coin flip.
            case V.toList quads of
                [qa, qb] → quadPainterOrder qa `shouldNotBe` quadPainterOrder qb
                _ → expectationFailure "expected exactly two quads"
            let sorted = Map.foldr (\v acc → V.toList v ⧺ acc) []
                             (sortQuadsByLayer quads)
                drawnLast = last sorted
                lastInstance = listToMaybe
                    [ fpInstanceId pk
                    | (pk, g) ← cands
                    , fgTexture g ≡ sqTexture drawnLast ]
            Just px ← pure (anchorPixel view (instanceId 1))
            fmap fpInstanceId
                (pickFloraAt view (SelectChoppable "wood") (fst px) (snd px))
                `shouldBe` lastInstance

        it "leaves a fully identical pair rendering identically either way" $ do
            -- The one case the render key still ties on: same depth,
            -- same rect, SAME texture. Every remaining vertex field
            -- follows from those, so the frame is byte-identical
            -- whichever the unstable sort drew last — which is exactly
            -- what makes the picker's instance-id backstop unobservable
            -- rather than a disagreement.
            let a = plantAt 1 woodId (8, 8)
                b = plantAt 2 woodId (8, 8)
                (tgx, tgy) = chunkToGlobal flatChunk 8 8
                quadFor inst = floraToQuad (fromIntegral . toInt)
                    defaultWorldTextures FaceNorth tgx tgy
                    (inst { fiZ = zSlice }) treeTex zSlice effDepth 1.0 (0, 0)
                    texSizes
            case (quadFor a, quadFor b) of
                (Just qa, Just qb) → do
                    quadPainterOrder qa `shouldBe` quadPainterOrder qb
                    -- Identical in every rendered field, not merely in
                    -- the ordering key.
                    map ($ qa) [sqV0, sqV1, sqV2, sqV3]
                        `shouldBe` map ($ qb) [sqV0, sqV1, sqV2, sqV3]
                    sqTexture qa `shouldBe` sqTexture qb
                    sqLayer qa `shouldBe` sqLayer qb
                _ → expectationFailure "expected two drawable quads"

        it "resolves an EXACT depth tie deterministically, by identity" $ do
            -- Two co-tenants on one tile at one z with equal fiOffV
            -- carry the SAME key, and the scene sorter
            -- (Engine.Scene.Types.Batch.sortQuadsByLayer) is unstable —
            -- so the renderer has no order here for a picker to agree
            -- with. What is pinned is that the picker's own answer is
            -- reproducible and matches the documented rule.
            let a = (plantAt 1 woodId (8, 8)) { fiOffU = 0.2, fiOffV = 0 }
                b = (plantAt 2 woodId (8, 8)) { fiOffU = 0.2, fiOffV = 0 }
                view = viewOf FaceNorth (camOn FaceNorth (8, 8))
                           (tilesOf [chunkWith flatChunk flat [a, b]]) HM.empty
                keys = [ (fpInstanceId pk, fgSortKey g)
                       | (pk, g) ← floraSelectCandidates view
                                       (SelectChoppable "wood") ]
            -- The fixture really does tie.
            nub (map snd keys) `shouldSatisfy` \ks → length ks ≡ 1
            Just px ← pure (anchorPixel view (instanceId 1))
            let winner = max (instanceId 1) (instanceId 2)
            fmap fpInstanceId
                (pickFloraAt view (SelectChoppable "wood") (fst px) (snd px))
                `shouldBe` Just winner
            -- Reproducible: the same view answers the same way however
            -- the candidates happened to be enumerated.
            fmap fpInstanceId
                (pickFloraAt view (SelectChoppable "wood") (fst px) (snd px))
                `shouldBe` Just winner

    describe "the committed marker" $ do

        let markerTex = TextureHandle 21
            iconSize  = (32, 32) ∷ (Float, Float)
            oak       = plantAt 1 woodId (8, 8)
            view      = viewOf FaceNorth (camOn FaceNorth (8, 8))
                            (tilesOf [chunkWith flatChunk flat [oak]])
                            HM.empty
            geomOf = listToMaybe
                [ g | (p, g) ← floraSelectCandidates view
                                   (SelectChoppable "wood")
                    , fpInstanceId p ≡ instanceId 1 ]

        it "is centred on the tree's own ground-contact anchor" $
            case geomOf of
                Nothing → expectationFailure "no candidate to mark"
                Just g → do
                    let q = floraMarkerQuad (fromIntegral . toInt) g
                                iconSize 1.0 0 0 markerTex
                        (x0, x1) = xSpan q
                    -- Horizontally centred on the anchor, to within the
                    -- float error of one add.
                    abs (((x0 + x1) * 0.5) - fgAnchorX g)
                        `shouldSatisfy` (< 0.0001)

        it "sits immediately ABOVE the anchor, never below it" $
            case geomOf of
                Nothing → expectationFailure "no candidate to mark"
                Just g → do
                    let q = floraMarkerQuad (fromIntegral . toInt) g
                                iconSize 1.0 0 0 markerTex
                        (y0, y1) = ySpan q
                    -- Screen Y grows downward, so "above the anchor"
                    -- means the quad's BOTTOM edge is the anchor.
                    abs (y1 - fgAnchorY g) `shouldSatisfy` (< 0.0001)
                    y0 `shouldSatisfy` (< fgAnchorY g)

        it "draws over the tree it annotates, never behind it" $
            case geomOf of
                Nothing → expectationFailure "no candidate to mark"
                Just g → do
                    let q = floraMarkerQuad (fromIntegral . toInt) g
                                iconSize 1.0 0 0 markerTex
                    sqSortKey q `shouldSatisfy` (> fgSortKey g)

        it "is NOT the full-tile ground overlay it replaced" $
            case geomOf of
                Nothing → expectationFailure "no candidate to mark"
                Just g → do
                    let q = floraMarkerQuad (fromIntegral . toInt) g
                                iconSize 1.0 0 0 markerTex
                        (gx, gy) = (fpGXOf, fpGYOf)
                        ground = worldCursorToQuad (fromIntegral . toInt)
                                     (const 0) defaultWorldTextures
                                     FaceNorth gx gy zSlice zSlice effDepth
                                     1.0 (0, 0) markerTex
                    xSpan q `shouldNotBe` xSpan ground
                    ySpan q `shouldNotBe` ySpan ground

        it "carries the neutral face map, so no terrain mask touches it" $
            case geomOf of
                Nothing → expectationFailure "no candidate to mark"
                Just g → do
                    let q = floraMarkerQuad (fromIntegral . toInt) g
                                iconSize 1.0 0 0 markerTex
                    map faceMapOf [sqV0 q, sqV1 q, sqV2 q, sqV3 q]
                        `shouldBe` replicate 4 noFaceMapVertexId

        it "sizes from the ICON, not from the tree's own quad" $
            case geomOf of
                Nothing → expectationFailure "no candidate to mark"
                Just g → do
                    let q = floraMarkerQuad (fromIntegral . toInt) g
                                iconSize 1.0 0 0 markerTex
                        (x0, x1) = xSpan q
                    (x1 - x0) `shouldSatisfy` (< fgQuadW g)

        it "is one flat DESIGNED alpha, like every other marker" $ do
            -- The translucency is a chosen value baked into the file,
            -- not a render-time tint: this project bakes all colour
            -- into textures, and 'floraMarkerQuad' passes a plain white
            -- tint carrying only the whole-layer zoom fade. 150 is the
            -- designation family's solid-marker alpha, the value the
            -- retired full-tile chop overlay carried.
            icon ← readRGBA
                "assets/textures/ui/hud/utility/chop_designate_tree.png"
            imageSize icon `shouldBe` (44, 44)
            visibleAlphas icon `shouldBe` [150]

        it "vanishes with the tree: a felled plant leaves nothing to draw" $ do
            -- The marker pass is driven by the LIVE instance, so a
            -- chunk that no longer holds the plant produces no draw and
            -- therefore no annotation — the marker cannot outlive its
            -- tree (requirement 8). The durable entry is separately
            -- swept by World.Flora.Designation.forgetFloraInstances.
            let felled = chunkWith flatChunk flat []
                draws = chunkFloraDraws catalog 360 100 HM.empty
                            flatChunk felled
            map (fiInstanceId . fdInstance) draws `shouldBe` []

-- | The marker's horizontal / vertical extent, read straight off the
--   emitted quad rather than recomputed.
xSpan, ySpan ∷ SortableQuad → (Float, Float)
xSpan q = let xs = map xOf [sqV0 q, sqV1 q, sqV2 q, sqV3 q]
          in (minimum xs, maximum xs)
ySpan q = let ys = map yOf [sqV0 q, sqV1 q, sqV2 q, sqV3 q]
          in (minimum ys, maximum ys)

xOf, yOf, faceMapOf ∷ Vertex → Float
xOf v = case pos v of Vec2 x _ → x
yOf v = case pos v of Vec2 _ y → y
faceMapOf = faceMapId

-- | The tile the fixture's single oak stands on, in global coords.
fpGXOf, fpGYOf ∷ Int
(fpGXOf, fpGYOf) = chunkToGlobal flatChunk 8 8

-- | The marker PNG, decoded to RGBA8.
readRGBA ∷ FilePath → IO (JP.Image JP.PixelRGBA8)
readRGBA path = do
    bytes ← BS.readFile path
    case JP.decodePng bytes of
        Left err  → fail (path ⧺ ": " ⧺ err)
        Right dyn → pure (JP.convertRGBA8 dyn)

imageSize ∷ JP.Image JP.PixelRGBA8 → (Int, Int)
imageSize img = (JP.imageWidth img, JP.imageHeight img)

-- | Every distinct alpha among the pixels that are visible at all.
visibleAlphas ∷ JP.Image JP.PixelRGBA8 → [Word8]
visibleAlphas img = sort . nub $
    [ a
    | y ← [0 .. JP.imageHeight img - 1]
    , x ← [0 .. JP.imageWidth img - 1]
    , let JP.PixelRGBA8 _ _ _ a = JP.pixelAt img x y
    , a > 0 ]

-- | The south-east wall slot's tag, as 'ChunkStructures' keys it.
seTag ∷ Word8
seTag = fromIntegral (fromEnum SWallSE)

-- | A vertex's position, for comparing two quads' rects.
rectOf ∷ Vertex → (Float, Float)
rectOf v = case pos v of Vec2 x y → (x, y)
