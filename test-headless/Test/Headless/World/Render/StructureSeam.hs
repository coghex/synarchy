{-# LANGUAGE Strict #-}
-- | Structure quads at the cylindrical U seam (issue #1706).
--
--   Chunks are STORED u-wrapped, so a chunk whose nearest image is
--   across the seam has to be DRAWN through that image.
--   'World.Render.ChunkCulling.isChunkVisibleWrapped' is the one
--   decision for which image that is, and it promises visibility and
--   placement come from the same pair — the terrain, spoil, blood,
--   cursor, ground-item and hit-test passes all consume it.
--
--   The structure pass consumed it nowhere. It flattened
--   @HM.elems (wtdChunks td)@ straight to pieces, discarding the owning
--   'lcCoord' the wrap decision would have keyed on, applied no
--   visibility test at all, and projected each piece's raw stored
--   coordinate. So the floor of a room wrapped into view while the
--   walls standing on it stayed a whole world away — 76.8 screen-world
--   units in X at south/north, 38.4 in Y at west/east for a 64-chunk
--   world, far outside any view.
--
--   This is the structure analogue of
--   "Test.Headless.World.Render.GroundItemSeam". It drives the pure
--   'structureChunkQuads' rather than the IO pass, because the IO pass
--   deliberately emits nothing until the texture system exists and a
--   headless engine never has one — a property 'engineSpec' below pins
--   rather than works around.
--
--   Every displacement asserted here is derived from 'gridToWorld', the
--   projection itself, never restated as a constant this fixture could
--   get wrong; and the expected front-wall pair per facing is restated
--   from #1712's own table rather than read back out of the renderer,
--   so a producer that silently stopped running would fail the count.
module Test.Headless.World.Render.StructureSeam (spec, engineSpec) where

import UPrelude
import Test.Hspec
import Test.Headless.Harness.Isolation (withIsolatedResourceRoot)
import Data.IORef (writeIORef)
import qualified Data.HashMap.Strict as HM
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU
import Engine.Asset.Handle (TextureHandle(..))
import Engine.Core.Init (initializeEngineHeadless, EngineInitResult(..))
import Engine.Core.State (EngineEnv(..))
import Engine.Graphics.Camera (Camera2D(..), CameraFacing(..), defaultCamera)
import Engine.Graphics.Vulkan.Types.Vertex
    (Vertex(..), Vec2(..), Vec4(..), WorldUV)
import Engine.Scene.Base (LayerId)
import Engine.Scene.Types (SortableQuad(..))
import Structure.Palette (TexPalette, emptyTexPalette, internPath)
import Structure.Render
    (renderStructureQuads, structureChunkQuads, structurePieceQuads)
import Structure.Types
    (ChunkStructures, StructurePieceData(..), StructureSlot(..))
import Structure.WallCatalog (emptyStructureWallCatalog)
import World.Chunk.Types
    (ChunkCoord(..), ColumnTiles(..), LoadedChunk(..), chunkSize)
import World.Flora.Types (emptyFloraChunkData)
import World.Fluid.Types (emptyIceMap)
import World.Generate (viewDepth)
import World.Generate.Types (WorldGenParams(..), defaultWorldGenParams)
import World.Grid (gridToWorld)
import World.Render.ChunkCulling (isChunkVisibleWrapped)
import World.Render.ViewBounds (ViewBounds, computeViewBounds)
import World.State.Types (WorldState(..), emptyWorldState)
import World.Tile.Types (WorldTileData(..))

-- | worldSize 64 chunks → canonical chunk u ∈ [-32, 32).
worldSize ∷ Int
worldSize = 64

zSlice ∷ Int
zSlice = 10

-- | The same seam fixture 'GroundItemSeam' uses: chunk (17,-15) has
--   u = 32, one past the canonical range, and is STORED under
--   ChunkCoord (-15) 17. The tile shift between the two frames is a
--   whole world, (-512, +512) tiles.
stored ∷ ChunkCoord
stored = ChunkCoord (-15) 17

-- | A chunk whose u = 31 is inside the canonical range: its own key,
--   nothing shifts.
interior ∷ ChunkCoord
interior = ChunkCoord 16 (-15)

rawTile, canonTile, interiorTile ∷ (Int, Int)
rawTile      = (17 * chunkSize, (-15) * chunkSize)
canonTile    = ((-15) * chunkSize, 17 * chunkSize)
interiorTile = (16 * chunkSize, (-15) * chunkSize)

allFacings ∷ [CameraFacing]
allFacings = [FaceSouth, FaceWest, FaceNorth, FaceEast]

zoom ∷ Float
zoom = 4.0

fbW, fbH ∷ Int
(fbW, fbH) = (800, 600)

effDepth ∷ Int
effDepth = min viewDepth (max 8 (round (zoom * 80.0 + 8.0 ∷ Float)))

-- * The fixture's pieces
--
--   One tile carrying every slot the three producers answer for: a
--   floor and the four authored walls (two of which are SCREEN-front at
--   any facing and take the #415 strip path, two of which do not), plus
--   all four corner posts.

fixtureSlots ∷ [StructureSlot]
fixtureSlots =
    [ SFloor
    , SWallNE, SWallNW, SWallSE, SWallSW
    , SPostN, SPostE, SPostS, SPostW
    ]

wallSlots ∷ [StructureSlot]
wallSlots = [SWallNE, SWallNW, SWallSE, SWallSW]

-- | Authored world edge → screen edge, restated from #1712's pinned
--   table (as 'FrontWallLift' restates it) rather than read back out of
--   'Structure.Render'. Screen SE/SW is the front pair, and only that
--   pair takes the strip producer.
expectFront ∷ CameraFacing → StructureSlot → Bool
expectFront FaceSouth s = s ≡ SWallSE ∨ s ≡ SWallSW
expectFront FaceWest  s = s ≡ SWallNW ∨ s ≡ SWallSW   -- NW↦SW, SW↦SE
expectFront FaceNorth s = s ≡ SWallNE ∨ s ≡ SWallNW   -- NE↦SW, NW↦SE
expectFront FaceEast  s = s ≡ SWallNE ∨ s ≡ SWallSE   -- NE↦SE, SE↦SW

-- | Strips per screen-front wall — 'Structure.Render.wallStripCount',
--   restated here so a silent change to it shows up as a failure rather
--   than as agreement.
stripsPerFrontWall ∷ Int
stripsPerFrontWall = 16

piece ∷ StructurePieceData
piece = StructurePieceData 0 0 zSlice

structuresAt ∷ (Int, Int) → ChunkStructures
structuresAt (gx, gy) = HM.fromList
    [ ((gx, gy, fromIntegral (fromEnum slot)), piece)
    | slot ← fixtureSlots ]

-- | A chunk carrying the fixture's pieces on its origin tile. Only
--   'lcCoord' and 'lcStructures' matter to the pass; the rest is the
--   same inert filling 'GroundItemSeam' uses.
chunkWith ∷ ChunkCoord → (Int, Int) → LoadedChunk
chunkWith coord tile =
    let area = chunkSize * chunkSize
        col  = ColumnTiles
                 { ctStartZ = 0
                 , ctMats   = VU.replicate 20 1
                 , ctSlopes = VU.replicate 20 0
                 , ctVeg    = VU.replicate 20 0
                 }
    in LoadedChunk
        { lcCoord = coord
        , lcTiles = V.replicate area col
        , lcSurfaceMap = VU.replicate area zSlice
        , lcTerrainSurfaceMap = VU.replicate area zSlice
        , lcFluidMap = V.replicate area Nothing
        , lcIceMap = emptyIceMap, lcFlora = emptyFloraChunkData
        , lcSideDeco = VU.empty, lcWaterTableMap = VU.empty
        , lcMagma = Nothing, lcStructures = structuresAt tile
        }

seamChunk, interiorChunk ∷ LoadedChunk
seamChunk     = chunkWith stored   canonTile
interiorChunk = chunkWith interior interiorTile

fixturePalette ∷ TexPalette
fixturePalette = snd (internPath "seam/placeholder.png" emptyTexPalette)

fixtureHandles ∷ HM.HashMap Int TextureHandle
fixtureHandles = HM.fromList [(0, TextureHandle 1)]

-- | The pass under test, with everything but the camera pinned.
chunkQuads ∷ CameraFacing → ViewBounds → (Float, Float) → [LoadedChunk]
           → [SortableQuad]
chunkQuads facing vb (camX, camY) =
    structureChunkQuads emptyStructureWallCatalog fixturePalette
        fixtureHandles (const 1) HM.empty facing zSlice effDepth 1.0
        worldSize vb camX camY

-- | What ONE piece emits, straight out of the per-piece producer — the
--   unshifted reference the seam output must reproduce exactly except
--   in its vertex positions. This is literally the call the pass made
--   before #1706.
pieceQuads ∷ CameraFacing → (Int, Int) → StructureSlot → [SortableQuad]
pieceQuads facing (gx, gy) slot =
    structurePieceQuads emptyStructureWallCatalog fixturePalette
        fixtureHandles (const 1) HM.empty facing zSlice effDepth 1.0
        gx gy slot piece

-- | The whole chunk's unshifted emission, in the pass's own iteration
--   order over the same map, so the two lists pair up element by
--   element.
referenceQuads ∷ CameraFacing → LoadedChunk → [SortableQuad]
referenceQuads facing lc =
    [ sq
    | ((gx, gy, slotTag), _) ← HM.toList (lcStructures lc)
    , sq ← pieceQuads facing (gx, gy)
               (toEnum (fromIntegral slotTag) ∷ StructureSlot)
    ]

cameraAt ∷ CameraFacing → (Int, Int) → Camera2D
cameraAt facing (gx, gy) =
    let (wx, wy) = gridToWorld facing gx gy
    in defaultCamera { camPosition = (wx, wy), camZoom = zoom
                     , camFacing = facing, camZSlice = zSlice }

boundsFor ∷ Camera2D → ViewBounds
boundsFor cam = computeViewBounds cam fbW fbH effDepth

-- | The TRUE screen displacement of one u-wrap at this facing, taken
--   from the projection rather than from any constant restated here.
wrapDisplacement ∷ CameraFacing → (Float, Float)
wrapDisplacement facing =
    let (aliasWX, aliasWY) = uncurry (gridToWorld facing) rawTile
        (canonWX, canonWY) = uncurry (gridToWorld facing) canonTile
    in (aliasWX - canonWX, aliasWY - canonWY)

-- | Everything about a vertex EXCEPT its position: the fields a
--   screen-space translation must leave alone.
vertexPayload ∷ Vertex → (Vec2, Vec4, Float, Float, Word32, WorldUV)
vertexPayload v =
    (tex v, color v, atlasId v, faceMapId v, renderFlags v, worldUV v)

quadPayload ∷ SortableQuad
            → ( Float, TextureHandle, LayerId
              , [(Vec2, Vec4, Float, Float, Word32, WorldUV)] )
quadPayload q =
    ( sqSortKey q
    , sqTexture q
    , sqLayer q
    , map vertexPayload [sqV0 q, sqV1 q, sqV2 q, sqV3 q] )

positions ∷ SortableQuad → [(Float, Float)]
positions q = [ (x, y) | v ← [sqV0 q, sqV1 q, sqV2 q, sqV3 q]
                       , let Vec2 x y = pos v ]

spec ∷ Spec
spec = do

  describe "the fixture exercises all three producers (req 3)" $
    forM_ allFacings $ \facing → describe (show facing) $ do
      let counts = [ (slot, length (pieceQuads facing canonTile slot))
                   | slot ← fixtureSlots ]

      it "front walls take the strip producer, back walls do not" $
        forM_ wallSlots $ \slot →
          (slot, lookup slot counts) `shouldBe`
            (slot, Just (if expectFront facing slot
                         then stripsPerFrontWall else 1))

      it "the floor and all four posts emit one quad each" $
        forM_ (SFloor : [SPostN, SPostE, SPostS, SPostW]) $ \slot →
          (slot, lookup slot counts) `shouldBe` (slot, Just 1)

  describe "a structure chunk resolved across the U seam (#1706)" $
    forM_ allFacings $ \facing → describe (show facing) $ do
      let (expX, expY) = wrapDisplacement facing
          cam = cameraAt facing rawTile
          vb  = boundsFor cam
          got = chunkQuads facing vb (camPosition cam) [seamChunk]
          ref = referenceQuads facing seamChunk

      it "precondition: a u-wrap moves exactly one screen axis" $ do
        -- One component is identically zero and the other world-sized:
        -- WHICH is the whole point, so it is derived, never hardcoded.
        min (abs expX) (abs expY) `shouldBe` 0
        max (abs expX) (abs expY) `shouldSatisfy` (> 1.0)

      it "precondition: the shared decision resolves it through that alias" $
        case isChunkVisibleWrapped facing worldSize vb
                 (fst (camPosition cam)) (snd (camPosition cam)) stored of
          Nothing → expectationFailure "chunk culled at its own alias"
          Just (offX, offY) → do
            abs (offX - expX) `shouldSatisfy` (< 0.001)
            abs (offY - expY) `shouldSatisfy` (< 0.001)

      it "emits every piece exactly once — no alias duplicates it (req 6)" $ do
        -- Not merely "non-empty": the count must equal the reference's,
        -- so a second alias copy of any piece fails here.
        length got `shouldBe` length ref
        length got `shouldBe`
            1                                            -- floor
          + length [ () | s ← wallSlots, not (expectFront facing s) ]
          + stripsPerFrontWall
              * length [ () | s ← wallSlots, expectFront facing s ]
          + 4                                            -- posts

      it "translates every vertex by that displacement, on both axes" $
        forM_ (zip got ref) $ \(g, r) →
          forM_ (zip (positions g) (positions r)) $ \((gx, gy), (rx, ry)) → do
            abs ((gx - rx) - expX) `shouldSatisfy` (< 0.001)
            abs ((gy - ry) - expY) `shouldSatisfy` (< 0.001)

      it "leaves sort keys, UVs and every other payload untouched (req 4)" $
        -- The offset is a SCREEN translation: painter depth stays
        -- grid-derived, and UVs / tint / atlas + facemap slots / flags /
        -- packed world UV / texture must be bit-identical.
        map quadPayload got `shouldBe` map quadPayload ref

  describe "away from the seam the emission is unchanged (req 5)" $
    forM_ allFacings $ \facing → describe (show facing) $ do
      let cam = cameraAt facing interiorTile
          vb  = boundsFor cam
          got = chunkQuads facing vb (camPosition cam) [interiorChunk]
          ref = referenceQuads facing interiorChunk

      it "precondition: the offset is exactly (0, 0)" $
        isChunkVisibleWrapped facing worldSize vb
            (fst (camPosition cam)) (snd (camPosition cam)) interior
          `shouldBe` Just (0, 0)

      it "emits the identical quads, positions included" $ do
        length got `shouldBe` length ref
        map positions got `shouldBe` map positions ref
        map quadPayload got `shouldBe` map quadPayload ref

  describe "a structure whose chunk is not visible emits nothing (req 2)" $
    forM_ allFacings $ \facing → describe (show facing) $ do
      -- Off-screen along v, which does NOT wrap — so no u-alias can
      -- bring the chunk back into view, unlike a pure u displacement.
      let (cgx, cgy) = canonTile
          cam = cameraAt facing (cgx + 4000, cgy + 4000)
          vb  = boundsFor cam

      it "precondition: the shared decision culls it" $
        isChunkVisibleWrapped facing worldSize vb
            (fst (camPosition cam)) (snd (camPosition cam)) stored
          `shouldBe` Nothing

      it "emits no quads at all" $
        chunkQuads facing vb (camPosition cam) [seamChunk]
          `shouldSatisfy` null

  describe "the wrap decision is taken per chunk (req 1)" $
    forM_ allFacings $ \facing → describe (show facing) $ do
      -- The camera sits on the seam chunk's own canonical tile, so THAT
      -- chunk resolves at the identity while its neighbour one chunk
      -- across the seam resolves through a wrapped alias. Both are in
      -- the same call, so a single pass-wide offset cannot serve both.
      let cam = cameraAt facing canonTile
          vb  = boundsFor cam
          cp  = camPosition cam
          resolve c = isChunkVisibleWrapped facing worldSize vb
                          (fst cp) (snd cp) c
          both  = chunkQuads facing vb cp [seamChunk, interiorChunk]
          only1 = chunkQuads facing vb cp [seamChunk]
          only2 = chunkQuads facing vb cp [interiorChunk]

      it "precondition: the two chunks resolve to different offsets" $ do
        resolve stored `shouldBe` Just (0, 0)
        resolve interior `shouldSatisfy` \r → case r of
            Just off → off ≢ (0, 0)
            Nothing  → False

      it "gives each chunk its own, and concatenates them" $ do
        map positions both `shouldBe`
            (map positions only1 <> map positions only2)
        map quadPayload both `shouldBe`
            (map quadPayload only1 <> map quadPayload only2)

      it "draws the chunk under the camera unshifted" $
        map positions only1
          `shouldBe` map positions (referenceQuads facing seamChunk)

      it "draws the wrapped neighbour shifted, by one offset throughout" $
        case resolve interior of
          Nothing → expectationFailure "neighbour culled at its own alias"
          Just (offX, offY) → do
            length only2 `shouldBe` length (referenceQuads facing interiorChunk)
            forM_ (zip only2 (referenceQuads facing interiorChunk)) $ \(g, r) →
              forM_ (zip (positions g) (positions r)) $
                \((gx, gy), (rx, ry)) → do
                  abs ((gx - rx) - offX) `shouldSatisfy` (< 0.001)
                  abs ((gy - ry) - offY) `shouldSatisfy` (< 0.001)

-- | The IO pass must keep emitting NOTHING before the texture system
--   exists — a headless engine never initializes one, so this is the
--   property that forces the coverage above onto the pure helper rather
--   than an artefact of it. A pass that started emitting early would be
--   publishing quads whose bindless slots are not yet resolvable.
engineSpec ∷ Spec
engineSpec = beforeAll initEnv $
  describe "the structure pass before the texture system exists" $
    forM_ allFacings $ \facing →
      it ("emits nothing at " <> show facing) $ \env → do
        ws ← emptyWorldState
        writeIORef (wsGenParamsRef ws)
            (Just defaultWorldGenParams { wgpWorldSize = worldSize })
        writeIORef (wsTilesRef ws) WorldTileData
            { wtdChunks = HM.fromList [(stored, seamChunk)]
            , wtdMaxChunks = 200 }
        writeIORef (cameraRef env) (cameraAt facing rawTile)
        writeIORef (framebufferSizeRef env) (fbW, fbH)
        quads ← renderStructureQuads env ws facing zSlice effDepth 1.0
        V.toList quads `shouldSatisfy` null
  where
    -- Isolation wraps the boot (#1357): engine init is itself a
    -- @config/@ writer.
    initEnv = withIsolatedResourceRoot $ do
        EngineInitResult env ← initializeEngineHeadless
        pure env
