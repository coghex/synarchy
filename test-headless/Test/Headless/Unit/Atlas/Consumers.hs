{-# LANGUAGE Strict, OverloadedStrings #-}
-- | Pure RUNTIME-CONSUMER tests for the compiled unit-animation atlas
--   (#1259, TEX-3): the geometry that reads a frame out of a sheet.
--
--   Deliberately not metadata-only. A \'FrameSample\' that reports the
--   right cell size and the right UV rect is worth nothing if the
--   renderer keeps measuring the whole sheet, so these gate
--   \'Unit.Render.unitToQuad\'\'s emitted vertices,
--   \'Unit.HitTest.unitHitRect\'\'s rect, and
--   \'UI.Render.renderSpriteBatch\'\'s UVs, and they compare the TEXELS
--   an atlas cell resolves to against the texels its legacy frame
--   would. The extrusion ring, the sampler policy, the upload-cache
--   policy split and the atomic UI publication sit here for the same
--   reason: each is a consumer of the same fixture sheet.
--
--   Whether that sheet is FRESH is
--   "Test.Headless.Unit.Atlas.Freshness"\'s question, and whether the
--   index describing it is well formed is
--   "Test.Headless.Unit.Atlas.Index"\'s.
module Test.Headless.Unit.Atlas.Consumers (spec) where

import UPrelude
import Test.Hspec
import qualified Data.ByteString as BS
import qualified Data.HashMap.Strict as HM
import qualified Data.Map.Strict as Map
import qualified Data.Vector as V
import qualified Data.Vector.Storable as SV
import Engine.Asset.Handle (TextureHandle(..))
import Engine.Graphics.Camera (CameraFacing(..))
import Engine.Scripting.Lua.Message.Texture
    (UploadSampler(..), cacheEntryReusable, unitAtlasUploadSampler)
import Engine.Graphics.Vulkan.Texture.Handle (BindlessTextureHandle(..))
import Engine.Graphics.Vulkan.Texture.Rebind
    (FilterRebindPlan(..), SlotRebind(..), planFilterRebind)
import Engine.Graphics.Vulkan.Types.Vertex (Vertex(..), Vec2(..))
import Engine.Graphics.Vulkan.Texture.Slot (TextureSlot(..))
import Vulkan.Core10 (ImageView(..), Sampler(..))
import Engine.Scene.Types (SortableQuad(..))
import UI.Render (renderSpriteBatch)
import Engine.Scene.Base (LayerId(..))
import Engine.Scene.Types.Batch (RenderBatch(..))
import Test.Headless.Unit.Atlas.Rejection (shouldReject)
import Test.Headless.Unit.Atlas.Sheet
    ( extrudedSheet, fixtureAtlas, fixtureCellH, fixtureCellPad, fixtureCellW
    , fixtureH, fixturePixels, fixtureSlotH, fixtureSlotW, fixtureW
    , legacyFramePixels )
import Unit.Atlas.Index
import Unit.Atlas.Types
import Unit.Direction (Direction(..))
import Unit.Faction (Faction(..))
import UI.Manager
    ( createPage, createSprite, getElement, setSpriteFrame, setSpriteTexture )
import UI.Types
    ( UIElement(..), UILayer(..), UIRenderData(..), UISpriteStyle(..)
    , emptyUIPageManager )
import Unit.HitTest (unitHitRect)
import Unit.Render (unitToQuad)
import Unit.Types
import World.Page.Types (WorldPageId(..))

-- | Read one RGBA texel out of a sheet.
texelAt ∷ Int → Int → BS.ByteString → Int → Int → [Word8]
texelAt w _h px x y =
    let o = (y * w + x) * 4
    in [BS.index px (o + i) | i ← [0 .. 3]]

-- | The texels a quad spanning @fw x fh@ screen pixels resolves to
--   under NEAREST sampling of @uv@ within a @w x h@ sheet, in screen
--   order. This is the GPU's own rule: fragment centre @i + 0.5@ maps
--   to @u@, and the sampler takes @floor (u * w)@.
--
--   'flipX' mirrors within the sub-rect, exactly as the renderer's
--   vertex assignment does.
sampleFrame
    ∷ Int → Int → BS.ByteString      -- ^ sheet
    → UVRect → Bool
    → Int → Int                      -- ^ frame pixel size
    → [[Word8]]
sampleFrame w h px (u0, v0, u1, v1) flipX fw fh =
    [ texelAt w h px (clampI 0 (w - 1) tx) (clampI 0 (h - 1) ty)
    | j ← [0 .. fh - 1], i ← [0 .. fw - 1]
    , let fx = (fromIntegral i + 0.5) / fromIntegral fw ∷ Float
          fy = (fromIntegral j + 0.5) / fromIntegral fh ∷ Float
          (uL, uR) = if flipX then (u1, u0) else (u0, u1)
          u = uL + fx * (uR - uL)
          v = v0 + fy * (v1 - v0)
          tx = floor (u * fromIntegral w)
          ty = floor (v * fromIntegral h)
    ]
  where
    clampI lo hi = max lo ∘ min hi
fixtureStorage ∷ AnimStorage
fixtureStorage = StorageAtlas (ResidentAtlas fixtureAtlas (TextureHandle 900))

-- | The fixture's frame-1 cell in UV, and the row's V span — derived
--   from the geometry rather than written out, so a stride change moves
--   the expectations with the layout instead of leaving stale literals.
frame1U0, frame1U1, cellV0, cellV1 ∷ Float
frame1U0 = fromIntegral (fixtureSlotW + fixtureCellPad)
             / fromIntegral fixtureW
frame1U1 = fromIntegral (fixtureSlotW + fixtureCellPad + fixtureCellW)
             / fromIntegral fixtureW
cellV0 = fromIntegral fixtureCellPad / fromIntegral fixtureH
cellV1 = fromIntegral (fixtureCellPad + fixtureCellH) / fromIntegral fixtureH
-- * A unit instance the render / hit-test consumers can be run on

atlasTex ∷ TextureHandle
atlasTex = TextureHandle 900

sheetSizes ∷ HM.HashMap TextureHandle (Int, Int)
sheetSizes = HM.fromList
    [ (atlasTex, (fixtureW, fixtureH))
    , (TextureHandle 5, (fixtureCellW, fixtureCellH)) ]

atlasSample ∷ FrameSample
atlasSample = fixtureSampleAt 1

frame0Sample ∷ FrameSample
frame0Sample = fixtureSampleAt 0

fixtureSampleAt ∷ Int → FrameSample
fixtureSampleAt i = case storageSampleAt fixtureStorage DirS i False of
    Just s  → s
    Nothing → error ("fixture atlas must resolve DirS frame " ⧺ show i)

legacySample ∷ FrameSample
legacySample = wholeImageSample (TextureHandle 5) False

testInstance ∷ UnitInstance
testInstance = UnitInstance
    { uiDefName = "u", uiName = "", uiPage = WorldPageId "p"
    , uiTexture = TextureHandle 5, uiDirSprites = Map.empty
    , uiBaseWidth = 0
    , uiGridX = 0, uiGridY = 0, uiGridZ = 0, uiRealZ = 0
    , uiFacing = DirS, uiCurrentAnim = "", uiAnimStart = 0
    , uiAnimReverse = False, uiActivity = "idle", uiPose = "standing"
    , uiAnimStride = 1
    , uiStats = HM.empty, uiModifiers = HM.empty, uiSkills = HM.empty
    , uiKnowledge = HM.empty, uiInventory = [], uiEquipment = HM.empty
    , uiAccessories = [], uiFactionId = FactionNeutral
    , uiWounds = [], uiScars = [], uiImmuneResponse = 0
    , uiImmunities = HM.empty, uiBlood = 0
    , uiLastAttackerUid = Nothing, uiLastAttackerAt = 0
    , uiAnimOverride = "", uiFrozen = False, uiForceLoop = False
    , uiClimbDest = Nothing, uiTrailState = Nothing
    }

-- | A def whose only animation is the fixture atlas, so 'unitToQuad'
--   resolves through the real 'pickFrame'.
atlasDef ∷ UnitDef
atlasDef = UnitDef
    { udName = "u", udNamePool = Nothing, udDisplayName = Nothing
    , udTexture = TextureHandle 5, udPortrait = Nothing
    , udDirSprites = Map.empty, udBaseWidth = 0
    , udMaxSpeed = 1, udRunThreshold = 0.6
    , udAnimations = HM.singleton "clip" Animation
        { aFps = 8, aLoop = True, aFlip = False, aStorage = fixtureStorage }
    , udStateAnims = HM.empty, udEagerStats = False
    , udStatTemplates = HM.empty, udBodyTemplates = HM.empty
    , udSkillTemplates = HM.empty, udKnowledgeTemplates = HM.empty
    , udStartingInventory = [], udEquipmentClass = Nothing
    , udStartingEquipment = HM.empty, udStartingAccessories = []
    , udBodyParts = [], udNaturalResistance = defaultNaturalResistance
    , udNaturalWeapon = Nothing, udModifiers = []
    }

quadUVs ∷ SortableQuad → [(Float, Float)]
quadUVs q = [ (u, v) | Vertex { tex = Vec2 u v } ←
                  [sqV0 q, sqV1 q, sqV2 q, sqV3 q] ]

quadWidth ∷ SortableQuad → Float
quadWidth q =
    let xs = [ x | Vertex { pos = Vec2 x _ } ←
                   [sqV0 q, sqV1 q, sqV2 q, sqV3 q] ]
    in maximum xs - minimum xs

quadHeight ∷ SortableQuad → Float
quadHeight q =
    let ys = [ y | Vertex { pos = Vec2 _ y } ←
                   [sqV0 q, sqV1 q, sqV2 q, sqV3 q] ]
    in maximum ys - minimum ys

renderQuad ∷ Double → UnitInstance → Maybe UnitDef → Maybe SortableQuad
renderQuad now inst mDef =
    unitToQuad (const 0) 0 FaceSouth 0 8 1.0 False inst mDef now sheetSizes

-- | At fps 8, elapsed 0.125 s is exactly frame 1 — the SECOND cell, so
--   a sub-rect that silently stayed at the sheet's left edge fails.
frame1Time ∷ Double
frame1Time = 0.125

spec ∷ Spec
spec = do
    describe "Unit.Atlas — the render quad uses cell geometry, not the sheet" $ do
        it "sizes the quad from the CELL even though the sheet is wider" $ do
            let atlasQ = renderQuad frame1Time
                             (testInstance { uiCurrentAnim = "clip" })
                             (Just atlasDef)
                legacyQ = renderQuad 0 testInstance Nothing
            case (atlasQ, legacyQ) of
                (Just aq, Just lq) → do
                    -- The legacy T-pose texture IS 2x2 (the cell size),
                    -- so an atlas frame of the same logical size must
                    -- produce the identical quad size. Measuring the 4x2
                    -- sheet instead would double the width.
                    quadWidth aq `shouldBe` quadWidth lq
                    quadHeight aq `shouldBe` quadHeight lq
                _ → expectationFailure "expected both quads"

        it "emits the frame's own UV sub-rect on the vertices" $
            case renderQuad frame1Time
                     (testInstance { uiCurrentAnim = "clip" })
                     (Just atlasDef) of
                Just q → quadUVs q `shouldBe`
                    -- Frame 1 of a two-column PADDED sheet: the logical
                    -- cell is x 5..7 of 8 and y 1..3 of 4, so the gutter
                    -- lies outside the quad on every side.
                    [ (frame1U0, cellV0), (frame1U1, cellV0)
                    , (frame1U1, cellV1), (frame1U0, cellV1) ]
                Nothing → expectationFailure "expected a quad"

        it "a legacy frame still spans its whole image, byte for byte" $
            case renderQuad 0 testInstance Nothing of
                Just q → quadUVs q `shouldBe` [(0, 0), (1, 0), (1, 1), (0, 1)]
                Nothing → expectationFailure "expected a quad"

        it "a mirrored atlas frame swaps U WITHIN its own cell" $ do
            let mirrored = atlasSample { fsFlipX = True }
            -- The renderer's assignment, restated: left vertices take
            -- the sub-rect's right edge. Mirroring across the sheet
            -- (1-u) would give 0.375 and 0.125 — the OTHER cell, and at
            -- the padded stride not even a cell boundary.
            let (u0, _, u1, _) = fsUV mirrored
            (u1, u0) `shouldBe` (frame1U1, frame1U0)

    describe "Unit.Atlas — the hit rect uses cell geometry too" $ do
        it "click and box selection share ONE rect helper" $
            -- Both hitTestUnitAt and hitTestUnitsInRect call this; a
            -- single implementation is what stops them drifting.
            unitHitRect FaceSouth 0 sheetSizes atlasSample testInstance
                `shouldBe`
                unitHitRect FaceSouth 0 sheetSizes legacySample testInstance

        it "sizes from the cell, not the whole sheet" $ do
            let (_, _, wCell, hCell) =
                    unitHitRect FaceSouth 0 sheetSizes atlasSample testInstance
                -- A sample that reported no cell size falls through to
                -- the sheet's own 4x2 entry — visibly wider.
                sheetSample = atlasSample { fsCell = Nothing }
                (_, _, wSheet, _) =
                    unitHitRect FaceSouth 0 sheetSizes sheetSample testInstance
            wSheet `shouldSatisfy` (> wCell)
            (wCell, hCell) `shouldSatisfy` \(a, b) → a > 0 ∧ b > 0

        it "agrees with the render quad's size for the same frame" $
            case renderQuad frame1Time
                     (testInstance { uiCurrentAnim = "clip" })
                     (Just atlasDef) of
                Just q →
                    let (_, _, wq, hq) = unitHitRect FaceSouth 0 sheetSizes
                                             atlasSample testInstance
                    in (wq, hq) `shouldBe` (quadWidth q, quadHeight q)
                Nothing → expectationFailure "expected a quad"

    describe "Unit.Atlas — an atlas cell resolves to its legacy frame's texels" $ do
        it "frame 0 samples exactly the legacy frame 0 image" $ do
            let s0 = frame0Sample
            sampleFrame fixtureW fixtureH fixturePixels (fsUV s0) False
                        fixtureCellW fixtureCellH
                `shouldBe`
                sampleFrame fixtureCellW fixtureCellH (legacyFramePixels 0)
                            wholeImageUV False fixtureCellW fixtureCellH

        it "frame 1 samples exactly the legacy frame 1 image" $
            sampleFrame fixtureW fixtureH fixturePixels (fsUV atlasSample) False
                        fixtureCellW fixtureCellH
                `shouldBe`
                sampleFrame fixtureCellW fixtureCellH (legacyFramePixels 1)
                            wholeImageUV False fixtureCellW fixtureCellH

        it "a MIRRORED atlas cell matches the mirrored legacy frame" $
            sampleFrame fixtureW fixtureH fixturePixels (fsUV atlasSample) True
                        fixtureCellW fixtureCellH
                `shouldBe`
                sampleFrame fixtureCellW fixtureCellH (legacyFramePixels 1)
                            wholeImageUV True fixtureCellW fixtureCellH

        it "frame 0 and frame 1 are genuinely different pixels" $ do
            let s0 = frame0Sample
            sampleFrame fixtureW fixtureH fixturePixels (fsUV s0) False
                        fixtureCellW fixtureCellH
                `shouldNotBe`
                sampleFrame fixtureW fixtureH fixturePixels (fsUV atlasSample)
                            False fixtureCellW fixtureCellH

    -- #2076: the whole point of the gutter. A LINEAR sample taken
    -- anywhere inside a logical cell reaches at most one texel past its
    -- edge, so every texel within one texel of the cell boundary must
    -- hold that cell's own edge colour rather than the neighbour's.
    --
    -- The fixture is a 3x2 grid of DELIBERATELY distinct cells with one
    -- rectangularization slot, so every adjacency this has to survive
    -- is present at once: horizontal and vertical neighbours, all four
    -- sides and all four corners, cells on the sheet's own edge, and an
    -- authored cell beside an unreachable transparent slot.
    describe "Unit.Atlas — a cell's extrusion ring isolates it under linear" $ do
        let cols = 3
            rows = 2
            -- (row, col) -> art, with (1, 2) left as rectangularization
            -- padding: DirN authors two frames in a three-column sheet.
            authored r c = r * cols + c < cols * rows - 1
            art r c = BS.pack
                [ fromIntegral ((r * 91 + c * 37 + x * 19 + y * 7 + ch * 61)
                                    `mod` 251 + 4)
                | y ← [0 .. fixtureCellH - 1], x ← [0 .. fixtureCellW - 1]
                , ch ← [0 .. 3 ∷ Int] ]
            cellAt r c = if authored r c then Just (art r c) else Nothing
            sheetW = cols * fixtureSlotW
            sheetH = rows * fixtureSlotH
            pixels = extrudedSheet fixtureCellW fixtureCellH fixtureCellPad
                         cols rows cellAt
            sheet = DecodedImage sheetW sheetH pixels
            anim = fixtureAtlas
                { aaAtlasWidth = sheetW, aaAtlasHeight = sheetH
                , aaColumns = cols, aaRows = rows
                , aaDirections = Map.fromList
                    [ (DirS, AtlasDirectionRow DirS 0 cols)
                    , (DirN, AtlasDirectionRow DirN 1 (cols - 1)) ] }
            texel x y = [ BS.index pixels ((y * sheetW + x) * 4 + i)
                        | i ← [0 .. 3] ]
            -- The cell's own texel a coordinate extrudes FROM: clamp
            -- into the cell on both axes.
            ownTexel r c lx ly =
                let px = art r c
                    cx = max 0 (min (fixtureCellW - 1) lx)
                    cy = max 0 (min (fixtureCellH - 1) ly)
                    o  = (cy * fixtureCellW + cx) * 4
                in [ BS.index px (o + i) | i ← [0 .. 3] ]

        it "every texel a linear tap can reach belongs to its own cell" $
            forM_ [ (r, c) | r ← [0 .. rows - 1], c ← [0 .. cols - 1]
                           , authored r c ] $ \(r, c) →
                -- The reachable neighbourhood is the cell expanded by
                -- one texel on every side: exactly the slot.
                forM_ [ (lx, ly)
                      | ly ← [negate fixtureCellPad
                                  .. fixtureCellH + fixtureCellPad - 1]
                      , lx ← [negate fixtureCellPad
                                  .. fixtureCellW + fixtureCellPad - 1] ]
                    $ \(lx, ly) → do
                        let x = c * fixtureSlotW + fixtureCellPad + lx
                            y = r * fixtureSlotH + fixtureCellPad + ly
                        ((r, c, lx, ly), texel x y)
                            `shouldBe` ((r, c, lx, ly), ownTexel r c lx ly)

        -- Vulkan linear filtering translates a normalized coordinate to
        -- texel space with a -0.5 offset, then blends the surrounding 2x2
        -- footprint. Across the CLOSED logical-cell UV interval, the lower
        -- tap can therefore occupy each coordinate from one texel before
        -- the cell through its final texel. These are every DISTINCT
        -- footprint any interior or edge sample can produce; mirroring only
        -- reverses their order and clipping can only select a subset.
        it "every distinct bilinear footprint inside a cell uses only that cell" $
            forM_ [ (r, c) | r ← [0 .. rows - 1], c ← [0 .. cols - 1]
                           , authored r c ] $ \(r, c) → do
                let left = c * fixtureSlotW + fixtureCellPad
                    top  = r * fixtureSlotH + fixtureCellPad
                forM_ [ (x0, y0)
                      | y0 ← [top - 1 .. top + fixtureCellH - 1]
                      , x0 ← [left - 1 .. left + fixtureCellW - 1] ]
                    $ \(x0, y0) → do
                        let actual =
                                [ texel x y
                                | y ← [y0, y0 + 1], x ← [x0, x0 + 1] ]
                            expected =
                                [ ownTexel r c (x - left) (y - top)
                                | y ← [y0, y0 + 1], x ← [x0, x0 + 1] ]
                        ((r, c, x0, y0), actual)
                            `shouldBe` ((r, c, x0, y0), expected)

        -- Stated as a NEGATIVE too, so the check above cannot pass by
        -- every cell coincidentally agreeing: each authored cell's ring
        -- must genuinely differ from the neighbour it shields against.
        it "a neighbouring cell's art really is different" $ do
            -- Horizontal: (0,0) beside (0,1).
            art 0 0 `shouldNotBe` art 0 1
            -- Vertical: (0,0) above (1,0).
            art 0 0 `shouldNotBe` art 1 0
            -- Diagonal, which is what the corner squares shield.
            art 0 0 `shouldNotBe` art 1 1

        it "an authored cell beside a transparent padding slot is unaffected" $ do
            -- (1, 2) is the rectangularization slot; (1, 1) is its
            -- authored left neighbour and (0, 2) its authored neighbour
            -- above. Neither may pick up transparency in its own ring.
            let slotTexels = [ texel (2 * fixtureSlotW + i) (fixtureSlotH + j)
                             | j ← [0 .. fixtureSlotH - 1]
                             , i ← [0 .. fixtureSlotW - 1] ]
            -- The unaddressable slot IS fully transparent, gutter and
            -- all — the sheet is rectangular for free.
            all (≡ [0, 0, 0, 0]) slotTexels `shouldBe` True
            -- And its authored neighbours' rings hold their own art,
            -- which the per-cell sweep above has already established
            -- pointwise; restated here on the specific right/bottom
            -- edges that touch the transparent slot.
            forM_ [0 .. fixtureCellH - 1] $ \ly →
                texel (fixtureSlotW + fixtureCellPad + fixtureCellW)
                      (fixtureSlotH + fixtureCellPad + ly)
                    `shouldBe` ownTexel 1 1 fixtureCellW ly
            forM_ [0 .. fixtureCellW - 1] $ \lx →
                texel (2 * fixtureSlotW + fixtureCellPad + lx)
                      (fixtureCellPad + fixtureCellH)
                    `shouldBe` ownTexel 0 2 lx fixtureCellH

        -- Sheet-edge cells have no neighbour to bleed from, but they
        -- still need the ring: without it a linear tap at the outer
        -- edge would clamp or wrap to whatever the sampler decides.
        it "cells on the sheet's own edge carry a full ring too" $ do
            let corner = texel 0 0
            corner `shouldBe` ownTexel 0 0 (negate 1) (negate 1)
            texel (sheetW - 1) 0
                `shouldBe` ownTexel 0 (cols - 1) fixtureCellW (negate 1)

        -- The runtime's own freshness check must ACCEPT this layout and
        -- REJECT a sheet whose ring was not built from the frame — the
        -- half a cell-only comparison cannot see, and the half that
        -- would silently reintroduce bleeding.
        it "validateSourceFrame accepts the ring and rejects a wrong one" $ do
            forM_ [ (DirS, 0, c) | c ← [0 .. cols - 1] ]
                $ \(d, r, c) →
                    validateSourceFrame "acolyte" anim sheet d r c "f.png"
                        (DecodedImage fixtureCellW fixtureCellH (art r c))
                        `shouldBe` Right ()
            -- Rebuild the sheet with cell (0,1)'s gutter taken from its
            -- NEIGHBOUR instead of itself — the exact defect the ring
            -- exists to prevent — leaving every logical cell untouched.
            let bledPixels = BS.pack
                    [ b
                    | y ← [0 .. sheetH - 1], x ← [0 .. sheetW - 1]
                    , let (c, lx) = x `divMod` fixtureSlotW
                          (r, ly) = y `divMod` fixtureSlotH
                          inCell = lx ≥ fixtureCellPad
                                 ∧ lx < fixtureCellPad + fixtureCellW
                                 ∧ ly ≥ fixtureCellPad
                                 ∧ ly < fixtureCellPad + fixtureCellH
                          src = if (r, c) ≡ (0, 1) ∧ not inCell
                                    then art 0 0 else art r c
                          cx = max 0 (min (fixtureCellW - 1)
                                          (lx - fixtureCellPad))
                          cy = max 0 (min (fixtureCellH - 1)
                                          (ly - fixtureCellPad))
                          o = (cy * fixtureCellW + cx) * 4
                    , b ← if authored r c
                              then [ BS.index src (o + i) | i ← [0 .. 3] ]
                              else [0, 0, 0, 0] ]
                bled = DecodedImage sheetW sheetH bledPixels
            -- The LOGICAL cell is still exactly right...
            BS.concat (atlasCellRows anim bled 0 1)
                `shouldBe` BS.concat (atlasCellRows anim sheet 0 1)
            -- ...and the frame is still rejected, on the ring.
            validateSourceFrame "acolyte" anim bled DirS 0 1 "f.png"
                (DecodedImage fixtureCellW fixtureCellH (art 0 1))
                `shouldReject` "extrusion ring"

        -- Requirement 5, restated where it can be measured: the UV rect
        -- that addresses this cell resolves under NEAREST to the cell's
        -- own texels and nothing else, mirrored or not.
        it "NEAREST sampling of the cell's UV reads only the cell" $
            forM_ [ (r, c) | r ← [0 .. rows - 1], c ← [0 .. cols - 1]
                           , authored r c ] $ \(r, c) →
                forM_ [False, True] $ \flipX →
                    sampleFrame sheetW sheetH pixels
                        (atlasCellUV anim r c) flipX
                        fixtureCellW fixtureCellH
                        `shouldBe`
                    sampleFrame fixtureCellW fixtureCellH (art r c)
                        wholeImageUV flipX fixtureCellW fixtureCellH

    -- The UI half of the #887 flip×clip ordering, now that a sprite can
    -- be a sub-rect: a clip may only HIDE part of a mirrored atlas cell,
    -- never change which texel a screen position shows, and it must
    -- never widen the sample back to the whole sheet.
    describe "Unit.Atlas — a clipped mirrored atlas cell keeps its texel mapping" $ do
        let cell = (0.5, 0, 1, 1) ∷ UVRect
            sprite flipX clip = renderSpriteBatch (TextureHandle 900) (1, 1, 1, 1)
                cell flipX 50 0 100 50 (LayerId 0) clip
            uAt pick b = pick [ u | Vertex { tex = Vec2 u _ } ←
                                    SV.toList (rbVertices b) ]

        it "an unclipped mirrored cell spans the cell backwards" $
            case V.toList (fst (sprite True Nothing)) of
                [b] → (uAt maximum b, uAt minimum b) `shouldBe` (1, 0.5)
                other → expectationFailure ("expected one batch, got " ⧺ show (length other))

        it "a half clip keeps the left edge and lands on the cell's midpoint" $
            case (V.toList (fst (sprite True Nothing))
                 , V.toList (fst (sprite True (Just (0, 0, 100, 100))))) of
                ([_], [c]) → do
                    -- Screen x 50..150 mirrored samples u=1 at x=50 and
                    -- u=0.5 at x=150, so u=0.75 at x=100. Clipping to
                    -- x<=100 must leave exactly 0.75..1 — the cell's own
                    -- midpoint, NOT 0 or 0.5 (which is what reflecting
                    -- across the whole image, or reversing the surviving
                    -- interval, would produce).
                    uAt maximum c `shouldBe` 1
                    uAt minimum c `shouldBe` 0.75
                other → expectationFailure ("expected one batch each, got " ⧺ show other)

        it "an unmirrored clipped cell stays inside the cell as well" $
            case V.toList (fst (sprite False (Just (0, 0, 100, 100)))) of
                [b] → do
                    uAt minimum b `shouldBe` 0.5
                    uAt maximum b `shouldBe` 0.75
                other → expectationFailure ("expected one batch, got " ⧺ show (length other))

        it "a whole-image sprite is unchanged by the generalization" $
            case V.toList (fst (renderSpriteBatch (TextureHandle 1) (1, 1, 1, 1)
                     wholeImageUV True 50 0 100 50 (LayerId 0) Nothing)) of
                [b] → (uAt maximum b, uAt minimum b) `shouldBe` (1, 0)
                other → expectationFailure ("expected one batch, got " ⧺ show (length other))

    -- #2085: gameplay unit art is scene art and follows either value of
    -- the player's global sampler. The atlas must therefore stay OUT of
    -- btsPinned; #2076's extrusion ring above is what makes the linear
    -- case safe. A derived world-preview slot remains pinned, proving the
    -- global toggle still distinguishes the two populations. (The single
    -- mip level is structural: the shared image allocator creates one,
    -- and the atlas path reuses it unchanged.)
    describe "Unit.Atlas — an atlas slot follows the global filter" $ do
        let atlasHandle' = TextureHandle 900
            preview      = TextureHandle 901
            atlasView    = ImageView 0xA11A5
            previewView  = ImageView 0x0DD
            previewNearest = Sampler 0x0E4E57
            globalNearest  = Sampler 0x0EA2
            globalLinear   = Sampler 0x11EA4
            handleMap = Map.fromList
                [ (atlasHandle', BindlessTextureHandle (TextureSlot 1 0) atlasHandle')
                , (preview,      BindlessTextureHandle (TextureSlot 2 0) preview) ]
            imageViews = Map.fromList
                [ (atlasHandle', atlasView), (preview, previewView) ]
            pinned = Map.singleton preview previewNearest
            plan newGlobal = planFilterRebind handleMap imageViews pinned newGlobal

        it "declares the global upload policy at the real atlas batch seam" $
            unitAtlasUploadSampler `shouldBe` UploadGlobalSampler

        it "repaints the atlas under both global modes while preview stays pinned" $
            forM_ [globalNearest, globalLinear] $ \newGlobal → do
                frpRebinds (plan newGlobal) `shouldBe`
                    [ SlotRebind 1 atlasView newGlobal
                    , SlotRebind 2 previewView previewNearest ]
                frpUnrecoverable (plan newGlobal) `shouldBe` []

    -- Reuse across an upload-policy boundary would hand the new handle
    -- the wrong sampler. Since #2075 the path cache is itself keyed by
    -- (path, policy), so a lookup cannot cross that boundary; this
    -- predicate is the GPU-side check that a canonical really was
    -- registered the way its cache key claims.
    describe "Unit.Atlas — the texture cache will not reuse across policies" $ do
        let pinnedMap = Map.singleton (TextureHandle 7) (Sampler 0x0E4E57)
            reuse policy h' = cacheEntryReusable policy pinnedMap h'
        it "a pinned UI request may reuse only an already-pinned entry" $ do
            reuse UploadPinnedNearest (TextureHandle 7) `shouldBe` True
            -- A UI request inheriting a scene slot would follow global
            -- filter toggles and stop being nearest.
            reuse UploadPinnedNearest (TextureHandle 8) `shouldBe` False
        it "an ordinary request may reuse only an unpinned entry" $ do
            reuse UploadGlobalSampler (TextureHandle 8) `shouldBe` True
            reuse UploadGlobalSampler (TextureHandle 7) `shouldBe` False

    -- A frame is a texture AND a sub-rect AND a mirror flag. Publishing
    -- them one at a time lets the render thread, which reads the manager
    -- concurrently, land between the writes and draw the wrong cell —
    -- or the whole sheet.
    describe "Unit.Atlas — a frame is published to the UI in one transition" $ do
        let build =
                let (pg, m1) = createPage "hud" LayerHUD emptyUIPageManager
                    (el, m2) = createSprite "portrait" 32 32
                                   (TextureHandle 1) (1, 1, 1, 1) pg m1
                in (el, m2)
            styleOf el mgr = case getElement el mgr of
                Just e → case ueRenderData e of
                    RenderSprite st → Just (ussTexture st, ussUV st, ussFlipX st)
                    _ → Nothing
                Nothing → Nothing

        it "a fresh sprite starts on the whole image, unmirrored" $
            let (el, mgr) = build
            in styleOf el mgr `shouldBe` Just (TextureHandle 1, (0, 0, 1, 1), False)

        it "setSpriteFrame lands texture, sub-rect and mirror together" $
            let (el, mgr) = build
                mgr' = setSpriteFrame el (TextureHandle 900) (0.5, 0, 1, 1) True mgr
            in styleOf el mgr' `shouldBe`
                Just (TextureHandle 900, (0.5, 0, 1, 1), True)

        it "publishing a whole-image portrait resets the sub-rect and flip" $
            let (el, mgr) = build
                atlasFrame = setSpriteFrame el (TextureHandle 900)
                                 (0.5, 0, 1, 1) True mgr
                portrait = setSpriteFrame el (TextureHandle 42) (0, 0, 1, 1)
                               False atlasFrame
            in styleOf el portrait `shouldBe`
                Just (TextureHandle 42, (0, 0, 1, 1), False)

        -- The regression the atomic verb exists to prevent: the
        -- intermediate state a texture-then-UV sequence passes through
        -- pairs the new atlas handle with the OLD frame's rect, which
        -- for a first switch from a portrait is the entire sheet.
        it "the separate setters really do expose that intermediate state" $
            let (el, mgr) = build
                halfway = setSpriteTexture el (TextureHandle 900) mgr
            in do
                styleOf el halfway `shouldBe`
                    Just (TextureHandle 900, (0, 0, 1, 1), False)
                styleOf el halfway `shouldNotBe`
                    styleOf el (setSpriteFrame el (TextureHandle 900)
                                    (0.5, 0, 1, 1) False mgr)
