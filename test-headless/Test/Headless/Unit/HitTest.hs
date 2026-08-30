{-# LANGUAGE Strict, OverloadedStrings #-}
-- | Pure tests for the unit hit box's PLACEMENT (#1957).
--
--   'Unit.HitTest.unitHitRect' is the one hit-box geometry click and
--   box selection share, and every term of it mirrors the quad
--   'Unit.Render.unitToQuad' emits. The height offset used to be the
--   exception — integer @uiGridZ@ against the renderer's continuous
--   @uiRealZ@ — which put the rect @|uiRealZ - uiGridZ| *
--   tileSideHeight@ away from the sprite for the whole length of a
--   climb, fall or leap.
--
--   So these call BOTH functions on the same instance, with the same
--   frame sample selected the way the production hit-test call sites
--   select it ('Unit.HitTest.frameSampleOf'), and compare the rect's
--   returned Y with the emitted quad's top-edge vertex Y. Restating the
--   placement formula here instead would gate nothing: it is the two
--   implementations agreeing that matters.
module Test.Headless.Unit.HitTest (spec) where

import UPrelude
import Test.Hspec
import qualified Data.HashMap.Strict as HM
import qualified Data.Map.Strict as Map
import Engine.Asset.Handle (TextureHandle(..))
import Engine.Graphics.Camera (CameraFacing(..))
import Engine.Graphics.Vulkan.Types.Vertex (Vertex(..), Vec2(..))
import Engine.Scene.Types (SortableQuad(..))
import Unit.Direction (Direction(..))
import Unit.Faction (Faction(..))
import Unit.HitTest (frameSampleOf, unitHitRect)
import Unit.Render (unitToQuad)
import Unit.Types
import World.Grid (tileSideHeight)
import World.Page.Types (WorldPageId(..))

-- * Fixture
--
--   A 96x64 sprite — the shipped tile size — so the quad is exactly
--   @tileHeight@ (0.1) tall and one z-level of offset
--   (@tileSideHeight@, 0.025) is a quarter of it. Four levels is the
--   whole sprite; that is the "click selects nothing" case below.

spriteTex ∷ TextureHandle
spriteTex = TextureHandle 5

texSizes ∷ HM.HashMap TextureHandle (Int, Int)
texSizes = HM.fromList [(spriteTex, (96, 64))]

-- | No unit def is loaded, so both the renderer and 'frameSampleOf'
--   take the same directional T-pose fallback — the sample is the same
--   one by construction, which is what leaves PLACEMENT as the only
--   thing these tests can be measuring.
defs ∷ HM.HashMap Text UnitDef
defs = HM.empty

facing ∷ CameraFacing
facing = FaceSouth

-- | Same clock both sides read.
now ∷ Double
now = 0

-- | The camera slice. Every case below keeps @uiGridZ@ inside
--   @[zSlice - effDepth, zSlice]@, because outside it 'unitToQuad'
--   emits 'Nothing' and there is no rendered placement to agree with.
zSlice ∷ Int
zSlice = 4

effDepth ∷ Int
effDepth = 8

-- | Off-origin tile coords and a real base width, so @rawX@/@rawY@ and
--   the @baseRadius@ anchor are non-zero terms rather than zeroes that
--   would agree whatever the code did.
unitAt ∷ Int → Float → UnitInstance
unitAt gz rz = UnitInstance
    { uiDefName = "u", uiName = "", uiPage = WorldPageId "p"
    , uiTexture = spriteTex, uiDirSprites = Map.empty
    , uiBaseWidth = 24
    , uiGridX = 3.25, uiGridY = 1.75, uiGridZ = gz, uiRealZ = rz
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

-- | The rect the hit-testers get, built exactly as 'hitTestUnitAt' and
--   'hitTestUnitsInRect' build it.
hitRect ∷ UnitInstance → (Float, Float, Float, Float)
hitRect inst =
    unitHitRect facing zSlice texSizes (frameSampleOf now facing defs inst) inst

rectY ∷ UnitInstance → Float
rectY inst = let (_, y, _, _) = hitRect inst in y

rectHeight ∷ UnitInstance → Float
rectHeight inst = let (_, _, _, h) = hitRect inst in h

renderedQuad ∷ UnitInstance → Maybe SortableQuad
renderedQuad inst =
    unitToQuad (const 0) 0 facing zSlice effDepth 1.0 False inst Nothing
               now texSizes

-- | The quad's top edge — the corner 'rectCorners' was handed, and the
--   value 'unitHitRect' returns as its own Y.
quadTopY ∷ SortableQuad → Float
quadTopY q = minimum [ y | Vertex { pos = Vec2 _ y } ←
                           [sqV0 q, sqV1 q, sqV2 q, sqV3 q] ]

placementAgrees ∷ String → UnitInstance → Spec
placementAgrees label inst =
    it ("places the rect where the renderer draws the quad — " ⧺ label) $
        case renderedQuad inst of
            Nothing → expectationFailure
                "unitToQuad culled the instance; the case must stay in the \
                \visibility band so a rendered placement exists"
            Just q  → rectY inst `shouldBe` quadTopY q

spec ∷ Spec
spec = describe "Unit.HitTest — the hit box sits where the sprite is drawn" $ do

    placementAgrees "grounded (uiRealZ == uiGridZ)"      (unitAt 2 2.0)
    placementAgrees "mid-climb, a sub-level offset"      (unitAt 2 2.4)
    placementAgrees "mid-climb, five levels above grid"  (unitAt 2 7.0)
    placementAgrees "falling, the sprite below the grid" (unitAt 3 0.75)

    it "the multi-level case is one where the old rect missed entirely" $ do
        -- unitAt 2 7.0 is five z-levels up from its still-integer
        -- uiGridZ. The rect the integer offset produced for it is
        -- literally the rect a unit STANDING at z 2 gets, so this
        -- compares two real outputs of the shipped function rather than
        -- restating the retired formula. 5 * 0.025 = 0.125 clears the
        -- 0.1-tall sprite: the two do not touch, which is a click on
        -- the visible unit selecting nothing.
        let climbing = unitAt 2 7.0
            legacyY  = rectY (unitAt 2 2.0)
        legacyY `shouldSatisfy` (≥ rectY climbing + rectHeight climbing)

    it "a grounded unit keeps the placement it has always had" $
        -- Frozen numbers for the fixture above, so the continuous
        -- offset cannot quietly move a unit that is not climbing,
        -- falling or leaping (#1957 requirement 3).
        hitRect (unitAt 2 2.0) `shouldBe` (0.0375, 0.15624999, 0.15, 0.1)

    it "the rect steps exactly one side-face height per z-level" $ do
        let y2 = rectY (unitAt 2 2.0)
            y3 = rectY (unitAt 3 3.0)
        abs ((y2 - y3) - tileSideHeight) `shouldSatisfy` (< 1.0e-6)

    it "an out-of-band unit is culled by the renderer, not by the rect" $ do
        -- Requirement 2: the visibility band is the hit-testers' own
        -- integer-uiGridZ check and unitHitRect never applied it. It
        -- still returns a rect for an instance the renderer culls.
        let above = unitAt (zSlice + 1) (fromIntegral zSlice + 1)
        renderedQuad above `shouldSatisfy` isNothing
        rectHeight above `shouldSatisfy` (> 0)
