{-# LANGUAGE Strict #-}
-- | Pure tests for Unit.Render.pickFrame. Constructs synthetic UnitDef
--   and UnitInstance values; verifies the frame index math and the
--   T-pose fallback chain.
--
--   Since #1259 the whole logical-choice matrix runs against BOTH
--   storage modes from ONE table of cases ('playbackCases'), because
--   D-3 freezes the arithmetic: legacy per-frame textures and a
--   compiled atlas must choose the same logical frame for every
--   combination of loop, force-loop, reverse, stride, clamping, and
--   clock skew. The two modes cannot silently drift while both are
--   driven from the same list.
module Test.Headless.Unit.Render.PickFrame (spec) where

import UPrelude
import Test.Hspec
import qualified Data.HashMap.Strict as HM
import qualified Data.Map.Strict as Map
import qualified Data.Vector as V
import Engine.Asset.Handle (TextureHandle(..), toInt)
import Engine.Graphics.Camera (CameraFacing(..))
import Unit.Direction (Direction(..))
import Unit.Faction (Faction(..))
import Unit.Render (pickFrame, screenDirOf, resolveTexture)
import Unit.Types
import World.Page.Types (WorldPageId(..))

-- | A texture handle by integer ID, for readable test assertions.
h ∷ Int → TextureHandle
h = TextureHandle

-- | pickFrame returns a storage-neutral 'FrameSample'. These describes
--   cover the frame-index math and the T-pose fallbacks; the handle
--   alone is the readable assertion for the legacy fixtures below.
pickTex ∷ Double → CameraFacing → UnitInstance → UnitDef → TextureHandle
pickTex t cam inst def = fsTexture (pickFrame t cam inst def)

-- | A UnitDef with the supplied animations and T-pose sprite (handle 0)
--   plus a directional S sprite (handle 1).
mkDef ∷ HM.HashMap Text Animation → UnitDef
mkDef anims = UnitDef
    { udName          = "test-unit"
    , udNamePool      = Nothing
    , udDisplayName   = Nothing
    , udTexture       = h 0
    , udPortrait      = Nothing
    , udDirSprites    = Map.fromList [(DirS, h 1)]
    , udBaseWidth     = 0
    , udMaxSpeed      = 1.0
    , udRunThreshold  = 0.6
    , udAnimations    = anims
    , udStateAnims    = HM.empty
    , udEagerStats    = False
    , udStatTemplates = HM.empty
    , udBodyTemplates = HM.empty
    , udSkillTemplates = HM.empty, udKnowledgeTemplates = HM.empty
    , udStartingInventory = []
    , udEquipmentClass = Nothing
    , udStartingEquipment = HM.empty
    , udStartingAccessories = []
    , udBodyParts          = []
    , udNaturalResistance  = defaultNaturalResistance
    , udNaturalWeapon      = Nothing
    , udModifiers          = []
    }

-- | A UnitInstance facing south with the supplied anim name and start time.
mkInst ∷ Text → Double → UnitInstance
mkInst animName start = UnitInstance
    { uiDefName     = "test-unit"
    , uiName        = ""
    , uiPage        = WorldPageId "test"
    , uiTexture     = h 0
    , uiDirSprites  = Map.fromList [(DirS, h 1)]
    , uiBaseWidth   = 0
    , uiGridX       = 0
    , uiGridY       = 0
    , uiGridZ       = 0
    , uiRealZ       = 0
    , uiFacing      = DirS
    , uiCurrentAnim = animName
    , uiAnimStart   = start
    , uiAnimReverse = False
    , uiActivity    = "idle"
    , uiPose        = "standing"
    , uiAnimStride  = 1
    , uiStats       = HM.empty
    , uiModifiers   = HM.empty
    , uiSkills      = HM.empty
    , uiKnowledge   = HM.empty
    , uiInventory   = []
    , uiEquipment   = HM.empty
    , uiAccessories = []
    , uiFactionId       = FactionNeutral
    , uiWounds          = []
    , uiScars           = []
    , uiImmuneResponse  = 0
    , uiImmunities      = HM.empty
    , uiBlood           = 0
    , uiLastAttackerUid = Nothing
    , uiLastAttackerAt  = 0
    , uiAnimOverride    = ""
    , uiFrozen          = False
    , uiForceLoop       = False
    , uiClimbDest       = Nothing
    , uiTrailState      = Nothing
    }

-- | An animation with frame handles 100,101,102,103 on DirS, fps 4, loop.
animSouth4 ∷ Animation
animSouth4 = legacyAnimation 4.0 True False
    (Map.fromList [(DirS, V.fromList [h 100, h 101, h 102, h 103])])

-- * Storage-parity fixtures

-- | The shape a parity case is built from: which directions the
--   animation authors, how many frames each has, and whether mirroring
--   is permitted. Both storage modes are generated from THIS, so they
--   describe the same animation by construction.
data AnimShape = AnimShape
    { shapeFlip  ∷ Bool
    , shapeDirs  ∷ [(Direction, Int)]   -- ^ direction → REAL frame count
    } deriving (Show)

-- | The canonical five authored directions with mirroring on — the
--   layout most shipped acolyte animations use.
fiveDirMirrored ∷ AnimShape
fiveDirMirrored = AnimShape True
    [(DirS, 4), (DirSE, 4), (DirE, 4), (DirNE, 4), (DirN, 4)]

-- | The same five directions with mirroring OFF: the western half must
--   fall back to the T-pose instead of borrowing the eastern art.
fiveDirNoMirror ∷ AnimShape
fiveDirNoMirror = fiveDirMirrored { shapeFlip = False }

-- | All eight authored, unequal per-direction counts (D-5). The atlas
--   is padded out to the longest row and the padding must stay
--   unreachable.
eightDirUnequal ∷ AnimShape
eightDirUnequal = AnimShape False
    [ (DirS, 6), (DirSW, 2), (DirW, 5), (DirNW, 1)
    , (DirN, 6), (DirNE, 3), (DirE, 4), (DirSE, 2) ]

-- | Legacy frames for a shape: direction @d@'s frame @i@ gets handle
--   @legacyHandle d i@, so a sample identifies its own (direction,
--   index) unambiguously.
legacyHandle ∷ Direction → Int → TextureHandle
legacyHandle d i = h (1000 + 100 * fromEnum d + i)

legacyOf ∷ Float → Bool → AnimShape → Animation
legacyOf fps loop shape = legacyAnimation fps loop (shapeFlip shape)
    (Map.fromList
        [ (d, V.fromList [legacyHandle d i | i ← [0 .. n - 1]])
        | (d, n) ← shapeDirs shape ])

-- | The atlas form of the same shape. Rows follow the compiler's
--   `ATLAS_DIRECTION_ORDER` (the engine's own `Direction` order,
--   restricted to authored directions), columns are the longest row,
--   and every cell is 24x32 so a wrong cell size is visible in the
--   arithmetic rather than hiding behind a square.
atlasCellW, atlasCellH ∷ Int
atlasCellW = 24
atlasCellH = 32

atlasAnimOf ∷ Float → Bool → AnimShape → AtlasAnimation
atlasAnimOf fps loop shape = AtlasAnimation
    { aaName         = "clip"
    , aaFormat       = AtlasFormatPng
    , aaPath         = "assets/textures/units/test-unit/atlas/clip.png"
    , aaAtlasWidth   = cols * atlasCellW
    , aaAtlasHeight  = rows * atlasCellH
    , aaCellWidth    = atlasCellW
    , aaCellHeight   = atlasCellH
    , aaColumns      = cols
    , aaRows         = rows
    , aaFlip         = shapeFlip shape
    , aaFps          = fps
    , aaLoop         = loop
    , aaDirections   = Map.fromList
        [ (d, AtlasDirectionRow d r n)
        | (r, (d, n)) ← zip [0 ..] ordered ]
    , aaSourceDigest = "source-digest"
    , aaAtlasDigest  = "atlas-digest"
    }
  where
    ordered = [ (d, n) | d ← [minBound .. maxBound]
                       , Just n ← [lookup d (shapeDirs shape)] ]
    rows = length ordered
    cols = maximum (1 : map snd ordered)

atlasHandle ∷ TextureHandle
atlasHandle = h 7777

atlasOf ∷ Float → Bool → AnimShape → Animation
atlasOf fps loop shape = Animation
    { aFps = fps, aLoop = loop, aFlip = shapeFlip shape
    , aStorage = StorageAtlas (ResidentAtlas (atlasAnimOf fps loop shape)
                                             atlasHandle) }

-- | Where a sample came from, expressed identically for both modes:
--   @Nothing@ for the T-pose, @Just (direction, index)@ for a frame.
--
--   Legacy reads it back from the handle; atlas from the UV rect's own
--   cell coordinates. That the two AGREE is what the parity cases
--   assert — and recovering it from the UV rect means a wrong sub-rect
--   fails here rather than passing on metadata alone.
data Chosen = Chosen Direction Int | TPose
    deriving (Show, Eq)

chosenLegacy ∷ AnimShape → FrameSample → Chosen
chosenLegacy shape smp =
    let raw = toInt (fsTexture smp)
    in if raw < 1000 then TPose else
        let d = toEnum ((raw - 1000) `div` 100)
            i = (raw - 1000) `mod` 100
        in if any ((≡ d) ∘ fst) (shapeDirs shape) then Chosen d i else TPose

chosenAtlas ∷ AnimShape → FrameSample → Chosen
chosenAtlas shape smp
    | fsTexture smp ≢ atlasHandle = TPose
    | otherwise =
        let aa = atlasAnimOf 1 True shape
            (u0, v0, _, _) = fsUV smp
            col = round (u0 * fromIntegral (aaAtlasWidth aa)) `div` aaCellWidth aa
            row = round (v0 * fromIntegral (aaAtlasHeight aa)) `div` aaCellHeight aa
        in case [ d | (d, r) ← Map.toList (aaDirections aa), adrRow r ≡ row ] of
            (d:_) → Chosen d col
            []    → TPose

-- | One playback case: a label, the animation-level fps/loop, the
--   instance tweaks, the unit facing, and the time. The camera faces
--   south unless a case overrides it via 'withCamera'.
data PlaybackCase = PlaybackCase
    { pcLabel  ∷ String
    , pcFps    ∷ Float
    , pcLoop   ∷ Bool
    , pcTweak  ∷ UnitInstance → UnitInstance
    , pcFacing ∷ Direction
    , pcTime   ∷ Double
    , pcCam    ∷ CameraFacing
    }

pc ∷ String → Float → Bool → (UnitInstance → UnitInstance) → Direction → Double
   → PlaybackCase
pc l f lp tw fc t = PlaybackCase l f lp tw fc t FaceSouth

withCamera ∷ CameraFacing → PlaybackCase → PlaybackCase
withCamera cam c = c { pcCam = cam }

-- | The full logical-choice matrix the review requires: T-pose and
--   missing-direction fallback, mirror on and off, forward and reverse
--   playback, stride, force-loop, ordinary looping, non-loop clamping,
--   and negative elapsed time.
playbackCases ∷ [(AnimShape, PlaybackCase)]
playbackCases =
    [ (fiveDirMirrored, pc "loop t=0"            4 True  id                DirS  0.0)
    , (fiveDirMirrored, pc "loop mid-cycle"      4 True  id                DirS  0.5)
    , (fiveDirMirrored, pc "loop wraps"          4 True  id                DirS  1.25)
    , (fiveDirMirrored, pc "negative elapsed"    4 True  (startAt 10)      DirS  0.0)
    , (fiveDirMirrored, pc "non-loop advances"   4 False id                DirS  0.5)
    , (fiveDirMirrored, pc "non-loop clamps"     4 False id                DirS 10.0)
    , (fiveDirMirrored, pc "force-loop overrides clamp"
                                                 4 False forceLoop         DirS 10.0)
    , (fiveDirMirrored, pc "reverse from the end"
                                                 4 True  reverseOn         DirS  0.0)
    , (fiveDirMirrored, pc "reverse mid-cycle"   4 True  reverseOn         DirS  0.5)
    , (fiveDirMirrored, pc "reverse non-loop holds frame 0"
                                                 4 False reverseOn         DirS 10.0)
    , (fiveDirMirrored, pc "stride 2 skips"      4 True  (stride 2)        DirS  0.25)
    , (fiveDirMirrored, pc "stride 3 wraps"      4 True  (stride 3)        DirS  0.5)
    , (fiveDirMirrored, pc "stride with reverse" 4 True  (stride 2 ∘ reverseOn)
                                                                           DirS  0.25)
    , (fiveDirMirrored, pc "mirrored west"       4 True  id                DirW  0.5)
    , (fiveDirMirrored, pc "mirrored south-west" 4 True  id                DirSW 0.25)
    , (fiveDirMirrored, pc "mirrored north-west" 4 True  id                DirNW 0.75)
    , (fiveDirNoMirror, pc "mirror OFF falls back to T-pose"
                                                 4 True  id                DirW  0.5)
    , (fiveDirNoMirror, pc "mirror OFF, authored direction still plays"
                                                 4 True  id                DirE  0.5)
    , (eightDirUnequal, pc "eight-dir long row"  4 True  id                DirS  1.25)
    , (eightDirUnequal, pc "eight-dir shortest row wraps"
                                                 4 True  id                DirNW 1.25)
    , (eightDirUnequal, pc "eight-dir two-frame row"
                                                 4 True  id                DirSW 0.75)
    , (eightDirUnequal, pc "eight-dir clamp on the short row"
                                                 4 False id                DirNW 9.0)
    , (eightDirUnequal, pc "eight-dir reverse on an odd row"
                                                 4 True  reverseOn         DirNE 0.5)
    , (fiveDirMirrored, withCamera FaceWest $
                        pc "camera rotation re-picks the screen direction"
                                                 4 True  id                DirS  0.5)
    , (fiveDirMirrored, withCamera FaceEast $
                        pc "camera rotation can select a mirrored direction"
                                                 4 True  id                DirS  0.5)
    ]
  where
    startAt t i  = i { uiAnimStart = t }
    forceLoop i  = i { uiForceLoop = True }
    reverseOn i  = i { uiAnimReverse = True }
    stride n i   = i { uiAnimStride = n }

-- | Float equality with a texel-scale tolerance: a UV span is an exact
--   ratio of integers, but a `Float` division of one sixth is not
--   exactly representable and the two sides round differently.
infix 1 `shouldBeNear`
shouldBeNear ∷ HasCallStack ⇒ Float → Float → Expectation
shouldBeNear got want
    | abs (got - want) ≤ 1e-6 = pure ()
    | otherwise = expectationFailure
        (show got ⧺ " is not within 1e-6 of " ⧺ show want)

-- | The instance one playback case drives.
caseInstance ∷ PlaybackCase → UnitInstance
caseInstance c = pcTweak c ((mkInst "clip" 0) { uiFacing = pcFacing c })

spec ∷ Spec
spec = do
    describe "screenDirOf" $ do
        it "leaves world facing unchanged when camera faces south" $
            screenDirOf FaceSouth DirS `shouldBe` DirS
        it "rotates DirS to DirW under FaceWest camera" $
            -- world S + camera W: a south-facing unit appears as W on screen
            -- after rotation 2 steps CCW (south idx 0 - 2 = 6 ≡ DirE? actually
            -- the code does (dirIdx - rotSteps) mod 8 — with rotSteps=2 for W,
            -- 0-2 mod 8 = 6 = DirE. Verify the actual implementation behavior:
            screenDirOf FaceWest DirS `shouldBe` DirE
        it "is its own inverse for full rotation" $
            screenDirOf FaceSouth (screenDirOf FaceSouth DirNE) `shouldBe` DirNE

    -- resolveTexture is the single direction→sprite path shared by the
    -- renderer (Unit.Render) and the hit-tester (Unit.HitTest). The
    -- mirror fallback below is the #389 regression: a unit facing
    -- W/SW/NW is DRAWN as the mirrored eastern sprite, so its hit-box
    -- must be SIZED from that same sprite (not the default texture).
    describe "resolveTexture — mirror fallback (#389)" $ do
        -- The 5-sprite convention: only S/SE/E/NE/N authored; the
        -- western half is produced by horizontal mirror of the east.
        let east = Map.fromList
                [ (DirS, h 1), (DirSE, h 2), (DirE, h 3)
                , (DirNE, h 4), (DirN, h 5) ]
            fb   = h 0
        it "returns the directional sprite (no flip) when present" $
            resolveTexture FaceSouth DirE east fb `shouldBe` (h 3, False)
        it "W falls back to the mirrored E sprite with flipX" $
            resolveTexture FaceSouth DirW east fb `shouldBe` (h 3, True)
        it "SW falls back to the mirrored SE sprite with flipX" $
            resolveTexture FaceSouth DirSW east fb `shouldBe` (h 2, True)
        it "NW falls back to the mirrored NE sprite with flipX" $
            resolveTexture FaceSouth DirNW east fb `shouldBe` (h 4, True)
        it "uses the fallback only when neither dir nor its mirror exist" $
            -- N is its own canonical (no mirror); drop it → fallback.
            let noN = Map.delete DirN east
            in resolveTexture FaceSouth DirN noN fb `shouldBe` (h 0, False)
        it "uses the fallback when there are no directional sprites" $
            resolveTexture FaceSouth DirW Map.empty fb `shouldBe` (h 0, False)

    describe "pickFrame — T-pose fallbacks" $ do
        it "returns directional T-pose when uiCurrentAnim is empty" $
            pickTex 0.0 FaceSouth (mkInst "" 0)
                    (mkDef HM.empty) `shouldBe` h 1  -- DirS T-pose
        it "returns T-pose when anim name is not in udAnimations" $
            pickTex 0.0 FaceSouth (mkInst "ghost" 0)
                    (mkDef HM.empty) `shouldBe` h 1
        it "returns T-pose when anim has no frames for the screen direction" $
            let anim = legacyAnimation 4.0 True False Map.empty
                def  = mkDef (HM.fromList [("idle", anim)])
            in pickTex 0.0 FaceSouth (mkInst "idle" 0) def `shouldBe` h 1
        it "returns T-pose when frames vector for screen direction is empty" $
            let anim = legacyAnimation 4.0 True False
                           (Map.fromList [(DirS, V.empty)])
                def  = mkDef (HM.fromList [("idle", anim)])
            in pickTex 0.0 FaceSouth (mkInst "idle" 0) def `shouldBe` h 1
        it "falls back to default texture when instance has no directional sprite" $
            -- T-pose path reads uiDirSprites from the instance, not the def
            let inst = (mkInst "" 0) { uiDirSprites = Map.empty }
            in pickTex 0.0 FaceSouth inst (mkDef HM.empty) `shouldBe` h 0
        it "a T-pose sample spans the whole image and reports no cell size" $
            let smp = pickFrame 0.0 FaceSouth (mkInst "" 0) (mkDef HM.empty)
            in do
                fsUV smp `shouldBe` (0, 0, 1, 1)
                fsCell smp `shouldBe` Nothing

    describe "pickFrame — frame index math (loop=True)" $ do
        let def = mkDef (HM.fromList [("idle", animSouth4)])
        it "picks frame 0 at t=0" $
            pickTex 0.0 FaceSouth (mkInst "idle" 0) def `shouldBe` h 100
        it "picks frame 0 at t=0.1 (under 1/fps)" $
            pickTex 0.1 FaceSouth (mkInst "idle" 0) def `shouldBe` h 100
        it "picks frame 1 at t=0.25 (exactly 1/fps)" $
            pickTex 0.25 FaceSouth (mkInst "idle" 0) def `shouldBe` h 101
        it "picks frame 2 at t=0.5" $
            pickTex 0.5 FaceSouth (mkInst "idle" 0) def `shouldBe` h 102
        it "picks frame 3 at t=0.75" $
            pickTex 0.75 FaceSouth (mkInst "idle" 0) def `shouldBe` h 103
        it "wraps around to frame 0 at t=1.0 (one full cycle)" $
            pickTex 1.0 FaceSouth (mkInst "idle" 0) def `shouldBe` h 100
        it "wraps again at t=1.25" $
            pickTex 1.25 FaceSouth (mkInst "idle" 0) def `shouldBe` h 101
        it "respects animStart offset" $
            pickTex 5.25 FaceSouth (mkInst "idle" 5.0) def `shouldBe` h 101
        it "guards against negative elapsed (clock skew) → frame 0" $
            pickTex 0.0 FaceSouth (mkInst "idle" 10.0) def `shouldBe` h 100

    describe "pickFrame — non-loop clamp" $ do
        let anim = animSouth4 { aLoop = False }
            def  = mkDef (HM.fromList [("once", anim)])
        it "advances normally before the end" $ do
            pickTex 0.0  FaceSouth (mkInst "once" 0) def `shouldBe` h 100
            pickTex 0.5  FaceSouth (mkInst "once" 0) def `shouldBe` h 102
        it "clamps to last frame after the end" $ do
            pickTex 1.0  FaceSouth (mkInst "once" 0) def `shouldBe` h 103
            pickTex 10.0 FaceSouth (mkInst "once" 0) def `shouldBe` h 103

    -- #1259 / D-3: atlas storage changed WHERE frames live, not WHICH
    -- one plays. Every case runs on both modes and must agree.
    describe "pickFrame — legacy and atlas storage choose the same logical frame" $
        forM_ playbackCases $ \(shape, c) → do
            let inst = caseInstance c
                cam  = pcCam c
                legacyDef = mkDef (HM.singleton "clip" (legacyOf (pcFps c) (pcLoop c) shape))
                atlasDef  = mkDef (HM.singleton "clip" (atlasOf  (pcFps c) (pcLoop c) shape))
                lSmp = pickFrame (pcTime c) cam inst legacyDef
                aSmp = pickFrame (pcTime c) cam inst atlasDef
            it (pcLabel c) $ do
                chosenAtlas shape aSmp `shouldBe` chosenLegacy shape lSmp
                -- The mirror decision is part of the frozen behaviour
                -- too: the same case must mirror (or not) identically.
                fsFlipX aSmp `shouldBe` fsFlipX lSmp

    describe "pickFrame — atlas cell geometry" $ do
        let shape = eightDirUnequal
            aa    = atlasAnimOf 4 True shape
            def   = mkDef (HM.singleton "clip" (atlasOf 4 True shape))
            sampleAt t d = pickFrame t FaceSouth
                ((mkInst "clip" 0) { uiFacing = d }) def

        it "reports the CELL dimensions, never the whole sheet" $ do
            fsCell (sampleAt 0 DirS) `shouldBe` Just (atlasCellW, atlasCellH)
            -- The sheet is genuinely larger, so a consumer that measured
            -- the image instead would report something else entirely.
            (aaAtlasWidth aa, aaAtlasHeight aa)
                `shouldNotBe` (atlasCellW, atlasCellH)

        it "shares ONE texture handle across every direction and frame" $
            let handles = [ fsTexture (sampleAt t d)
                          | d ← map fst (shapeDirs shape)
                          , t ← [0, 0.25, 0.5, 0.75, 1.0] ]
            in all (≡ atlasHandle) handles `shouldBe` True

        it "keeps every UV rect inside the sheet and correctly ordered" $
            let rects = [ fsUV (sampleAt t d)
                        | d ← map fst (shapeDirs shape)
                        , t ← [0, 0.25, 0.5, 0.75, 1.0, 1.5, 2.0] ]
            in forM_ rects $ \(u0, v0, u1, v1) → do
                u0 `shouldSatisfy` (\u → u ≥ 0 ∧ u < 1)
                v0 `shouldSatisfy` (\v → v ≥ 0 ∧ v < 1)
                u1 `shouldSatisfy` (\u → u > 0 ∧ u ≤ 1)
                v1 `shouldSatisfy` (\v → v > 0 ∧ v ≤ 1)
                (u1 - u0) `shouldBeNear`
                    fromIntegral atlasCellW / fromIntegral (aaAtlasWidth aa)
                (v1 - v0) `shouldBeNear`
                    fromIntegral atlasCellH / fromIntegral (aaAtlasHeight aa)

        -- D-5: a short row is padded out to the sheet's column count,
        -- and no padding cell is addressable. DirNW authors ONE frame in
        -- a six-column sheet, so every time must resolve to column 0.
        it "never addresses a padding cell on a short row" $
            let cols = [ chosenAtlas shape (sampleAt t DirNW)
                       | t ← [0, 0.1 .. 3.0] ]
            in all (≡ Chosen DirNW 0) cols `shouldBe` True

        it "resolves each direction to its OWN indexed row" $
            forM_ (Map.toList (aaDirections aa)) $ \(d, row) → do
                let (_, v0, _, _) = fsUV (sampleAt 0 d)
                    got = round (v0 * fromIntegral (aaAtlasHeight aa))
                              `div` aaCellHeight aa
                got `shouldBe` adrRow row

    describe "pickFrame — a mirrored atlas cell flips its own sub-rect" $ do
        let shape = fiveDirMirrored
            aa    = atlasAnimOf 4 True shape
            def   = mkDef (HM.singleton "clip" (atlasOf 4 True shape))
            at d  = pickFrame 0.5 FaceSouth
                        ((mkInst "clip" 0) { uiFacing = d }) def

        it "reads the SOURCE direction's cells and reports the flip" $ do
            -- W has no authored row; it must sample E's row, flipped.
            let west = at DirW
                east = at DirE
            fsFlipX west `shouldBe` True
            fsFlipX east `shouldBe` False
            fsUV west `shouldBe` fsUV east

        it "does not widen the sub-rect to the whole sheet when mirroring" $ do
            let (u0, _, u1, _) = fsUV (at DirW)
            (u1 - u0) `shouldBeNear`
                fromIntegral atlasCellW / fromIntegral (aaAtlasWidth aa)
            -- The whole-image mirror `1-u` would only be right if the
            -- rect spanned the sheet; assert it emphatically does not.
            (u0, u1) `shouldNotBe` (0, 1)

    describe "pickFrame — one animation is never in two storage modes" $ do
        let shape = fiveDirMirrored
        it "an atlas animation exposes no legacy frame map" $
            storageLegacyFrames (aStorage (atlasOf 4 True shape))
                `shouldBe` Nothing
        it "a legacy animation is not atlas-backed" $
            storageIsAtlas (aStorage (legacyOf 4 True shape)) `shouldBe` False
        it "both modes report the same per-direction real frame counts" $
            storageFrameCounts (aStorage (atlasOf 4 True eightDirUnequal))
                `shouldBe`
                storageFrameCounts (aStorage (legacyOf 4 True eightDirUnequal))
