{-# LANGUAGE Strict #-}
-- | Pure tests for 'World.Render.Quads.structureFrontWallClear' — the
--   #418 flora/vegetation lift over structure front walls.
--
--   The regression under test (issue #423): loaded chunks are keyed by
--   canonical (u-wrapped) coords, and a chunk's structures by tile
--   coords in that canonical frame. The lift used to probe neighbours
--   with a raw 'globalToChunk' lookup, so a wall just across the
--   cylindrical U seam was silently missed and the sprite kept its
--   unlifted key (the #418 straddle came back at the seam). The fix
--   canonicalises the probed chunk coord AND shifts the structure tile
--   key by the same wrap delta.
--
--   Since #1712 the lift is also ROTATION-aware: WHICH slots are
--   screen-front, the tie-break each contributes, and the grid vertex a
--   wall's clamped strips reach their deepest key at all follow the
--   camera. The all-facings block below walks every authored wall slot
--   at every facing and pins the exact front pair, the rejection of the
--   back pair, and the emitted key against the maximum strip key
--   'Structure.Render' itself produces for that wall — so the lift
--   cannot drift from the strips it is supposed to clear. The
--   cylindrical-seam cases above it are unchanged.
--
--   No engine needed: the function is pure. Structure lookups are fed
--   from a hand-built @ChunkCoord → ChunkStructures@ map, mirroring how
--   'renderWorldQuads' backs it with the loaded-chunk map.
module Test.Headless.World.Render.FrontWallLift (spec) where

import UPrelude
import Test.Hspec
import qualified Data.HashMap.Strict as HM
import Engine.Asset.Handle (TextureHandle(..))
import Engine.Graphics.Camera (CameraFacing(..))
import Engine.Scene.Types (SortableQuad(..))
import Structure.Palette (TexPalette, emptyTexPalette, internPath)
import Structure.Render (structurePieceQuads)
import Structure.Types (StructureSlot(..), StructurePieceData(..), ChunkStructures)
import Structure.WallCatalog (emptyStructureWallCatalog)
import World.Chunk.Types (ChunkCoord(..), chunkSeamChebyshev)
import World.Render.Quads (structureFrontWallClear)

seTag ∷ Word8
seTag = fromIntegral (fromEnum SWallSE)

-- | Lookup backed by a plain map: one 'ChunkStructures' per chunk coord.
lookupFrom ∷ [(ChunkCoord, [((Int, Int, Word8), StructurePieceData)])]
           → (ChunkCoord → Maybe ChunkStructures)
lookupFrom chunks cc = HM.lookup cc (HM.fromList [ (c, HM.fromList ps) | (c, ps) ← chunks ])

-- | A wall piece at world z 5 (palette ids irrelevant to sort keys).
wallZ5 ∷ StructurePieceData
wallZ5 = StructurePieceData 0 0 5

-- | worldSize 8 chunks → 128 tiles around the cylinder, canonical chunk
--   u ∈ [-4, 4). All scenarios use zSlice 5 = the wall z, so the key's
--   z-term is zero and the expected key is depth + tieBreak alone.
ws, zSlice, effDepth ∷ Int
ws = 8
zSlice = 5
-- | The fixture's effective depth. 8 is the production formula's floor
--   ('World.Render.Quads' @effectiveDepth@, which ranges 8..250 with the
--   zoom); the wall sits AT 'zSlice' in every scenario above, so the
--   band is satisfied at any depth and the historical keys are
--   untouched. The #1715 block below drives the value itself.
effDepth = 8

-- | Interior reference layout: SE wall at (-1,2) in chunk (-1,0), sprite
--   at (1,2) — two tiles east in grid space, spatially in front of the
--   wall's south-vertex anchor (0,3) at FaceSouth (depth 3 vs 3).
interiorLookup ∷ ChunkCoord → Maybe ChunkStructures
interiorLookup = lookupFrom [ (ChunkCoord (-1) 0, [((-1, 2, seTag), wallZ5)]) ]

-- | Seam layout with the SAME relative geometry: sprite at (-31,34)
--   (chunk (-2,2), tile u = -65 in its chunk's frame), wall on the
--   physically adjacent tile two steps east ACROSS the U seam. That
--   tile's raw coords (-33,34) live in raw chunk (-3,2), u = -5 —
--   non-canonical; the chunk is loaded as (1,-2) and the wall stored
--   under the wrapped tile key (31,-30).
seamLookup ∷ ChunkCoord → Maybe ChunkStructures
seamLookup = lookupFrom [ (ChunkCoord 1 (-2), [((31, -30, seTag), wallZ5)]) ]

spec ∷ Spec
spec = do
    it "lifts a sprite in front of an interior front wall (#418 baseline)" $ do
        let k = structureFrontWallClear FaceSouth ws zSlice effDepth interiorLookup 1 2
        k `shouldSatisfy` isJust
        -- Pin the strip-key formula: anchor depth 3, z-term 0 (wall z ≡
        -- zSlice), SE tieBreak 0.0006.
        k `shouldSatisfy` maybe False (\v → abs (v - 3.0006) < 1.0e-4)

    it "does not lift a sprite behind the wall" $ do
        -- depth 1+0 = 1 < wall anchor depth 3 → spatially behind.
        structureFrontWallClear FaceSouth ws zSlice effDepth interiorLookup 1 0
            `shouldBe` Nothing

    it "does not lift when no wall is near" $ do
        structureFrontWallClear FaceSouth ws zSlice effDepth (const Nothing) 1 2
            `shouldBe` Nothing

    it "lifts across the U seam exactly like the interior case (#423)" $ do
        let seamK     = structureFrontWallClear FaceSouth ws zSlice effDepth seamLookup (-31) 34
            interiorK = structureFrontWallClear FaceSouth ws zSlice effDepth interiorLookup 1 2
        seamK `shouldSatisfy` isJust
        -- The u-wrap preserves v = gx+gy, so the seam pair must produce
        -- the SAME strip key as the interior pair with identical relative
        -- geometry: depth 3 + z-term 0 + SE tieBreak.
        seamK `shouldBe` interiorK

    it "skips the cross-seam wall at east/west facings" $ do
        -- Depth follows u at FaceEast; the wall's stored frame sits a
        -- whole world width away, so the lift must not fire.
        structureFrontWallClear FaceEast ws zSlice effDepth seamLookup (-31) 34
            `shouldBe` Nothing

    it "still lifts at east/west facings away from the seam" $ do
        -- FaceEast depth = gx - gy: sprite (1,0) depth 1, wall anchor
        -- (0,3) depth -3 → sprite in front; frames agree (no wrap).
        structureFrontWallClear FaceEast ws zSlice effDepth interiorLookup 1 0
            `shouldSatisfy` isJust

    it "chunk gate: the seam sprite/wall chunks are wrap-adjacent" $ do
        -- The per-chunk gate in renderWorldQuads admits sprite chunks
        -- within seam-aware Chebyshev 1 of a structure-bearing chunk;
        -- this is the exact pair from the seam scenario above (raw
        -- distance 4, physical distance 1).
        chunkSeamChebyshev ws (ChunkCoord 1 (-2)) (ChunkCoord (-2) 2)
            `shouldBe` 1

    describe "rotating the camera (#1712)" rotationSpec

    describe "bounding the lift to the drawn slice band (#1715)" sliceBandSpec

-- * All-facings coverage (#1712)

-- | The wall's tile, and the sprite tile that probes it. "In front of"
--   rotates with the camera, so the sprite is placed two tiles along
--   whichever grid direction increases iso depth at this facing — which
--   is also within the lift's own ±2 candidate scan.
wallGX, wallGY ∷ Int
wallGX = 0 ; wallGY = 0

spriteTile ∷ CameraFacing → (Int, Int)
spriteTile facing =
    let (dx, dy) = case facing of
            FaceSouth → ( 1,  1)   -- depth = gx + gy
            FaceWest  → (-1,  1)   -- depth = gy - gx
            FaceNorth → (-1, -1)   -- depth = -(gx + gy)
            FaceEast  → ( 1, -1)   -- depth = gx - gy
    in (wallGX + 2 * dx, wallGY + 2 * dy)

-- | The four authored wall slots.
wallSlots ∷ [StructureSlot]
wallSlots = [SWallNE, SWallNW, SWallSE, SWallSW]

slotTag ∷ StructureSlot → Word8
slotTag = fromIntegral . fromEnum

-- | Authored world edge → screen edge, restated from #1712's pinned
--   table so this spec does not simply echo the production mapping.
--   Screen SE/SW is the front pair.
expectFront ∷ CameraFacing → StructureSlot → Bool
expectFront FaceSouth s = s ≡ SWallSE ∨ s ≡ SWallSW
expectFront FaceWest  s = s ≡ SWallNW ∨ s ≡ SWallSW   -- NW↦SW, SW↦SE
expectFront FaceNorth s = s ≡ SWallNE ∨ s ≡ SWallNW   -- NE↦SW, NW↦SE
expectFront FaceEast  s = s ≡ SWallNE ∨ s ≡ SWallSE   -- NE↦SE, SE↦SW

-- | One wall of the given slot, on the tile the sprite probes.
oneWall ∷ StructureSlot → (ChunkCoord → Maybe ChunkStructures)
oneWall slot =
    lookupFrom [ (ChunkCoord 0 0, [((wallGX, wallGY, slotTag slot), wallZ5)]) ]

-- | That wall's own maximum strip key, straight out of the renderer.
--   Palette ids and handles are placeholders: with an EMPTY wall
--   catalogue the ART is never rotated, which leaves the sort keys — the
--   only thing this spec compares — untouched.
maxStripKey ∷ CameraFacing → StructureSlot → Maybe Float
maxStripKey facing slot =
    case structurePieceQuads emptyStructureWallCatalog liftPalette
             (HM.fromList [(0, TextureHandle 1)]) (const 1) HM.empty
             facing zSlice effDepth 1.0 wallGX wallGY slot (StructurePieceData 0 0 zSlice) of
        [] → Nothing
        qs → Just (maximum (map sqSortKey qs))

liftPalette ∷ TexPalette
liftPalette = snd (internPath "lift/placeholder.png" emptyTexPalette)

allFacings ∷ [CameraFacing]
allFacings = [FaceSouth, FaceWest, FaceNorth, FaceEast]

-- | The extra examples #1712 adds, kept in their own describe so the
--   cylindrical-seam cases above stay exactly as they were.
rotationSpec ∷ Spec
rotationSpec = forM_ allFacings $ \facing → describe (show facing) $ do
    let (sgx, sgy) = spriteTile facing
    it "lifts over exactly the SCREEN-front pair, never the back pair" $
        forM_ wallSlots $ \slot → do
            let k = structureFrontWallClear facing ws zSlice effDepth (oneWall slot) sgx sgy
            (slot, isJust k) `shouldBe` (slot, expectFront facing slot)

    it "emits that wall's own maximum strip key" $
        forM_ [ s | s ← wallSlots, expectFront facing s ] $ \slot →
            case ( structureFrontWallClear facing ws zSlice effDepth (oneWall slot) sgx sgy
                 , maxStripKey facing slot ) of
                (Just got, Just want) →
                    abs (got - want) `shouldSatisfy` (< 1.0e-4)
                other → expectationFailure
                    ("expected both a lift key and strips, got " <> show other)

-- * Slice-band coverage (#1715)

-- | The interior reference layout, with the wall's stored z as a
--   parameter. Deliberately the SAME geometry the baseline case above
--   pins — sprite (1,2) two tiles east of an SE wall at (-1,2), one
--   chunk, one wall — so the spatial guard (@spriteDepth < localDepth@)
--   and the seam-frame guard (@scDepth ≢ localDepth@) are both known to
--   pass and no second wall can supply the key. Every 'Nothing' below
--   therefore isolates the z rejection.
wallAtZ ∷ Int → (ChunkCoord → Maybe ChunkStructures)
wallAtZ z = lookupFrom [ (ChunkCoord (-1) 0, [((-1, 2, seTag), StructurePieceData 0 0 z)]) ]

-- | That layout's lift, at an arbitrary effective depth and wall z.
liftAt ∷ Int → Int → Maybe Float
liftAt d z = structureFrontWallClear FaceSouth ws zSlice d (wallAtZ z) 1 2

-- | The key an ELIGIBLE wall at that z must produce: the baseline's
--   anchor depth 3 and SE tie-break 0.0006, plus the formula's z-term.
expectedKey ∷ Int → Float
expectedKey z = 3.0 + fromIntegral (z - zSlice) * 0.001 + 0.0006

shouldBeKeyFor ∷ Maybe Float → Int → Expectation
shouldBeKeyFor got z = case got of
    Just v | abs (v - expectedKey z) < 1.0e-4 → pure ()
    other → expectationFailure
        ("wall z " <> show z <> ": expected key " <> show (expectedKey z)
         <> ", got " <> show other)

-- | One wall of the given slot at (0,0) — the rotation block's layout —
--   with its stored z as a parameter, for the renderer-parity sweep.
oneWallAtZ ∷ StructureSlot → Int → (ChunkCoord → Maybe ChunkStructures)
oneWallAtZ slot z =
    lookupFrom [ (ChunkCoord 0 0, [((wallGX, wallGY, slotTag slot), StructurePieceData 0 0 z)]) ]

-- | Does the renderer actually EMIT anything for that wall at this
--   depth? Straight through 'structurePieceQuads', the production
--   dispatcher, so the comparison is against 'frontWallStrips' itself
--   and not a restatement of its gate.
rendererEmits ∷ CameraFacing → StructureSlot → Int → Int → Bool
rendererEmits facing slot d z =
    not $ null $ structurePieceQuads emptyStructureWallCatalog liftPalette
        (HM.fromList [(0, TextureHandle 1)]) (const 1) HM.empty
        facing zSlice d 1.0 wallGX wallGY slot (StructurePieceData 0 0 z)

-- | The depths the sweeps run at. 8 is the production floor, 250 the
--   ceiling ('viewDepth'), 4 and 40 interior values — several of them
--   NOT the fixture's own 'effDepth', so a hard-coded depth cannot pass.
sweepDepths ∷ [Int]
sweepDepths = [4, 8, 40, 250]

sliceBandSpec ∷ Spec
sliceBandSpec = do
    it "lifts a wall AT the camera slice" $
        liftAt effDepth zSlice `shouldBeKeyFor` zSlice

    it "does not lift a wall one level ABOVE the slice" $
        -- Lowering the slice below a room's walls is the ordinary way to
        -- look inside it. The wall stops being drawn, and its z-term is
        -- then POSITIVE — it used to lift a neighbouring sprite HARDER
        -- than a drawn wall would.
        liftAt effDepth (zSlice + 1) `shouldBe` Nothing

    it "lifts a wall at EXACTLY zSlice - effectiveDepth" $
        -- The band is inclusive at the far end, matching the renderer's
        -- own @gridZ < zSlice - effDepth@ rejection.
        liftAt effDepth (zSlice - effDepth) `shouldBeKeyFor` (zSlice - effDepth)

    it "does not lift a wall one level BELOW that floor" $
        liftAt effDepth (zSlice - effDepth - 1) `shouldBe` Nothing

    it "moves both bounds with the effective depth, not a fixed one" $
        forM_ sweepDepths $ \d → do
            (d, isJust (liftAt d (zSlice - d)))     `shouldBe` (d, True)
            (d, isJust (liftAt d (zSlice - d - 1))) `shouldBe` (d, False)
            (d, isJust (liftAt d (zSlice + 1)))     `shouldBe` (d, False)

    it "flips ONE fixed wall's eligibility between two depths" $ do
        -- The zoom-dependence itself: a wall stored at z -3 is inside
        -- the zoomed-OUT window (floor 5-8 = -3) and outside the
        -- zoomed-IN one (floor 5-4 = 1). Nothing about the wall or the
        -- sprite changes between these two lines.
        isJust (liftAt 8 (-3)) `shouldBe` True
        isJust (liftAt 4 (-3)) `shouldBe` False

    it "keeps the in-slice key identical at every depth" $
        -- Requirement 4: inside the band the gate is a no-op, so the
        -- baseline key is depth-invariant.
        forM_ sweepDepths $ \d → liftAt d zSlice `shouldBeKeyFor` zSlice

    it "agrees with the strips the renderer emits, over the whole band" $
        -- Requirement 2, checked against 'structurePieceQuads' rather
        -- than against a second copy of the predicate: no wall emits
        -- strips without being liftable, and none lifts without
        -- emitting strips.
        forM_ allFacings $ \facing →
            forM_ [ s | s ← wallSlots, expectFront facing s ] $ \slot →
                forM_ sweepDepths $ \d →
                    forM_ [zSlice - d - 2 .. zSlice + 2] $ \z → do
                        let (sgx, sgy) = spriteTile facing
                            lifted = isJust (structureFrontWallClear facing ws
                                        zSlice d (oneWallAtZ slot z) sgx sgy)
                        ((facing, slot, d, z), lifted)
                            `shouldBe` ((facing, slot, d, z), rendererEmits facing slot d z)
