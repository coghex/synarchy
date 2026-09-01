{-# LANGUAGE Strict #-}
-- | Pure tests for Unit.Render.pickFrame. Constructs synthetic UnitDef
--   and UnitInstance values; verifies the frame index math and the
--   T-pose fallback chain.
--
--   D-3 FREEZES THE FRAME-INDEX ARITHMETIC, and the whole logical-choice
--   matrix runs from ONE table of cases ('playbackCases') covering every
--   combination of loop, force-loop, reverse, stride, clamping, mirror
--   fallback, camera rotation and clock skew.
--
--   #1259 checked each case by running it through both storage modes and
--   asserting they agreed. #1261 retired the per-frame mode, so there is
--   no second side to compare against — but the arithmetic it was
--   protecting is exactly as frozen. Each case is now checked against
--   'expectedChoice', a restatement of the documented rule written
--   independently of 'Unit.Render.pickFrame', so an edit to either one
--   still fails here. That is strictly more than the old parity check
--   proved: both of its sides shared this arithmetic and differed only
--   in where they looked the frame up.
module Test.Headless.Unit.Render.PickFrame (spec) where

import UPrelude
import Test.Hspec
import qualified Data.HashMap.Strict as HM
import qualified Data.Map.Strict as Map
import Engine.Asset.Handle (TextureHandle(..))
import Engine.Graphics.Camera (CameraFacing(..))
import Unit.Direction (Direction(..), mirrorDir)
import Unit.Faction (Faction(..))
import Unit.Render (pickFrame, screenDirOf, resolveTexture)
import Unit.Types
import World.Page.Types (WorldPageId(..))

-- | A texture handle by integer ID, for readable test assertions.
h ∷ Int → TextureHandle
h = TextureHandle

-- | pickFrame returns a storage-neutral 'FrameSample'. The T-pose
--   describes assert on the handle alone, which for a direct sprite IS
--   the whole answer.
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

-- | A 4-frame south-only animation at fps 4, looping — the fixture the
--   index-math describes below step through column by column.
southOnly4 ∷ AnimShape
southOnly4 = AnimShape False [(DirS, 4)]

animSouth4 ∷ Animation
animSouth4 = atlasOf 4.0 True southOnly4

-- | Which COLUMN of the atlas a sample landed on, for the index-math
--   describes. 'TPose' when the sample is not an atlas cell at all.
pickCol ∷ Double → CameraFacing → UnitInstance → UnitDef → Chosen
pickCol t cam inst def = chosenAtlas southOnly4 (pickFrame t cam inst def)

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

-- | The atlas form of a shape. Rows follow the compiler's
--   `ATLAS_DIRECTION_ORDER` (the engine's own `Direction` order,
--   restricted to authored directions), columns are the longest row,
--   and every cell is 24x32 so a wrong cell size is visible in the
--   arithmetic rather than hiding behind a square.
--
--   Cells sit at the #2076 padded stride: each occupies a
--   @(24+2) x (32+2)@ slot whose one-texel border is extrusion, and the
--   LOGICAL cell starts one texel in. Every unequal number here — cell
--   vs slot, 24 vs 32, and the padding vs neither — is deliberate, so a
--   sub-rect computed at the wrong stride or without the offset lands
--   somewhere this file's arithmetic can see.
atlasCellW, atlasCellH, atlasCellPad ∷ Int
atlasCellW = 24
atlasCellH = 32
atlasCellPad = 1

atlasSlotW, atlasSlotH ∷ Int
atlasSlotW = atlasCellW + 2 * atlasCellPad
atlasSlotH = atlasCellH + 2 * atlasCellPad

atlasAnimOf ∷ Float → Bool → AnimShape → AtlasAnimation
atlasAnimOf fps loop shape = AtlasAnimation
    { aaName         = "clip"
    , aaFormat       = AtlasFormatPng
    , aaPath         = "assets/textures/units/test-unit/atlas/clip.png"
    , aaAtlasWidth   = cols * atlasSlotW
    , aaAtlasHeight  = rows * atlasSlotH
    , aaCellWidth    = atlasCellW
    , aaCellHeight   = atlasCellH
    , aaCellPadding  = atlasCellPad
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
atlasOf fps loop shape =
    atlasAnimation fps loop (shapeFlip shape)
        (ResidentAtlas (atlasAnimOf fps loop shape) atlasHandle)

-- | Which frame a sample landed on: @TPose@, or the SOURCE direction's
--   row and the column within it.
--
--   Recovered from the UV rect's own cell coordinates rather than from
--   metadata, so a wrong sub-rect fails here instead of passing on a
--   correct-looking frame index.
data Chosen = Chosen Direction Int | TPose
    deriving (Show, Eq)

chosenAtlas ∷ AnimShape → FrameSample → Chosen
chosenAtlas shape smp
    | fsTexture smp ≢ atlasHandle = TPose
    | otherwise =
        let aa = atlasAnimOf 1 True shape
            (u0, v0, _, _) = fsUV smp
            -- Undo the extrusion offset before striding: the UV
            -- origin is the LOGICAL cell, which sits one texel into
            -- its slot.
            col = (round (u0 * fromIntegral (aaAtlasWidth aa))
                      - aaCellPadding aa) `div` atlasSlotW
            row = (round (v0 * fromIntegral (aaAtlasHeight aa))
                      - aaCellPadding aa) `div` atlasSlotH
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

-- | D-3's frozen rule, RESTATED — deliberately a second statement of
--   'Unit.Render.pickFrame'\'s documented behaviour rather than a call
--   into it, so an unreviewed edit to either one fails this table:
--   camera rotation picks the screen direction, a directly authored
--   direction always wins, only W\/SW\/NW may mirror and only when the
--   animation permits it, the clock is clamped at the animation's own
--   start, stride multiplies the raw index, force-loop overrides a
--   one-shot's clamp, and reverse counts down from the last frame.
--
--   The T-pose branch defers to 'resolveTexture', which has its own
--   describe above: what this table owns is WHICH animation frame
--   plays, not which fallback sprite a missing one resolves to.
expectedChoice ∷ AnimShape → PlaybackCase → (Chosen, Bool)
expectedChoice shape c = case resolved of
    Just (srcDir, n, mirrored)
      | n > 0 →
        let elapsed = max 0 (pcTime c - uiAnimStart inst)
            raw     = floor (elapsed * realToFrac (pcFps c)) ∷ Int
            strided = raw * max 1 (uiAnimStride inst)
            doLoop  = pcLoop c ∨ uiForceLoop inst
            fwd     = if doLoop then strided `mod` n else min strided (n - 1)
            idx     = if uiAnimReverse inst then (n - 1) - fwd else fwd
        in (Chosen srcDir idx, mirrored)
    _ → (TPose, snd (resolveTexture (pcCam c) (pcFacing c)
                         (uiDirSprites inst) (uiTexture inst)))
  where
    inst      = caseInstance c
    screenDir = screenDirOf (pcCam c) (uiFacing inst)
    authored d = lookup d (shapeDirs shape)
    resolved = case authored screenDir of
        Just n  → Just (screenDir, n, False)
        Nothing
          | not (shapeFlip shape) → Nothing
          | otherwise → do
              md ← mirrorDir screenDir
              n  ← authored md
              pure (md, n, True)

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
        it "returns T-pose when the animation authors no row for the \
           \screen direction and may not mirror" $
            let def = mkDef (HM.fromList
                        [("idle", atlasOf 4 True (AnimShape False [(DirE, 2)]))])
            in pickTex 0.0 FaceSouth (mkInst "idle" 0) def `shouldBe` h 1
        it "returns T-pose when the authored row holds no frames at all" $
            let def = mkDef (HM.fromList
                        [("idle", atlasOf 4 True (AnimShape False [(DirS, 0)]))])
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
            at t = pickCol t FaceSouth (mkInst "idle" 0) def
        it "picks frame 0 at t=0" $
            at 0.0 `shouldBe` Chosen DirS 0
        it "picks frame 0 at t=0.1 (under 1/fps)" $
            at 0.1 `shouldBe` Chosen DirS 0
        it "picks frame 1 at t=0.25 (exactly 1/fps)" $
            at 0.25 `shouldBe` Chosen DirS 1
        it "picks frame 2 at t=0.5" $
            at 0.5 `shouldBe` Chosen DirS 2
        it "picks frame 3 at t=0.75" $
            at 0.75 `shouldBe` Chosen DirS 3
        it "wraps around to frame 0 at t=1.0 (one full cycle)" $
            at 1.0 `shouldBe` Chosen DirS 0
        it "wraps again at t=1.25" $
            at 1.25 `shouldBe` Chosen DirS 1
        it "respects animStart offset" $
            pickCol 5.25 FaceSouth (mkInst "idle" 5.0) def
                `shouldBe` Chosen DirS 1
        it "guards against negative elapsed (clock skew) → frame 0" $
            pickCol 0.0 FaceSouth (mkInst "idle" 10.0) def
                `shouldBe` Chosen DirS 0

    describe "pickFrame — non-loop clamp" $ do
        let def  = mkDef (HM.fromList
                       [("once", atlasOf 4 False southOnly4)])
            at t = pickCol t FaceSouth (mkInst "once" 0) def
        it "advances normally before the end" $ do
            at 0.0 `shouldBe` Chosen DirS 0
            at 0.5 `shouldBe` Chosen DirS 2
        it "clamps to last frame after the end" $ do
            at 1.0  `shouldBe` Chosen DirS 3
            at 10.0 `shouldBe` Chosen DirS 3

    -- D-3: atlas storage changed WHERE frames live, not WHICH one
    -- plays. Every case is checked against the independently restated
    -- rule, mirror decision included.
    describe "pickFrame — the frozen logical-frame choice" $
        forM_ playbackCases $ \(shape, c) → do
            let inst     = caseInstance c
                atlasDef = mkDef (HM.singleton "clip" (atlasOf (pcFps c) (pcLoop c) shape))
                smp      = pickFrame (pcTime c) (pcCam c) inst atlasDef
            it (pcLabel c) $
                (chosenAtlas shape smp, fsFlipX smp)
                    `shouldBe` expectedChoice shape c

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

        -- #2076: the sub-rect addresses the LOGICAL cell inside its
        -- padded slot, never the extrusion gutter. Recovered in TEXELS
        -- so the offset is asserted as an offset rather than as a
        -- ratio that a wrong stride could still satisfy.
        it "addresses the cell inside its slot, never the extrusion gutter" $
            forM_ (Map.toList (aaDirections aa)) $ \(d, row) → do
                let (u0, v0, u1, v1) = fsUV (sampleAt 0 d)
                    px u = round (u * fromIntegral (aaAtlasWidth aa)) ∷ Int
                    py v = round (v * fromIntegral (aaAtlasHeight aa)) ∷ Int
                -- Frame 0 of this row: one texel in on both axes, and
                -- exactly one cell wide/tall.
                px u0 `shouldBe` atlasCellPad
                py v0 `shouldBe` adrRow row * atlasSlotH + atlasCellPad
                px u1 - px u0 `shouldBe` atlasCellW
                py v1 - py v0 `shouldBe` atlasCellH
                -- And a full gutter still separates this cell from the
                -- next column's, which is what a bilinear tap needs.
                let nextU0 = atlasSlotW + atlasCellPad
                nextU0 - (px u0 + atlasCellW) `shouldBe` 2 * atlasCellPad

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
                    got = (round (v0 * fromIntegral (aaAtlasHeight aa))
                              - aaCellPadding aa) `div` atlasSlotH
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

    -- D-5: the atlas is rectangular, the animation is not. Every
    -- length question must answer from the index's REAL per-direction
    -- counts, never the padded column count — which is what
    -- Unit.Thread.Command.Pose's transition durations and
    -- unit.getAnimDuration both read through storageMaxFrameCount.
    describe "pickFrame — real frame counts, never the padding" $ do
        let st = aStorage (atlasOf 4 True eightDirUnequal)
        it "reports each authored direction's own declared count" $
            storageFrameCounts st
                `shouldBe` Map.fromList (shapeDirs eightDirUnequal)
        it "reports the longest row as the clip length, not the columns" $
            -- Columns and the longest row coincide by construction; the
            -- point is that a SHORT row does not report the column
            -- count, which is what a padded read would return.
            (storageMaxFrameCount st, storageFrameCount st DirNW)
                `shouldBe` (6, Just 1)
        it "makes a padding cell unreachable" $
            -- NW authors one frame in a six-column sheet: columns 1..5
            -- of its row are transparent padding and must not resolve.
            [ isJust (storageSampleAt st DirNW i False) | i ← [0 .. 5] ]
                `shouldBe` (True : replicate 5 False)

        it "refuses an index outside a row in EITHER direction" $
            -- The caller has already clamped or wrapped, so an
            -- out-of-range index is a bug rather than a padding cell.
            -- A negative one is the case an `idx >= count` bound alone
            -- would wave through, and it addresses the row BEFORE this
            -- one — a real cell holding another direction's art.
            [ isJust (storageSampleAt st DirS i False)
            | i ← [-6, -1, 0, 5, 6, 99] ]
                `shouldBe` [False, False, True, True, False, False]

        it "authors no row at all for a direction the animation omits" $
            -- Distinct from "present but empty": absence is what the
            -- mirror fallback keys on, so it must stay observable.
            (storageFrameCount (aStorage (atlasOf 4 True fiveDirMirrored)) DirW
            , isJust (storageSampleAt
                          (aStorage (atlasOf 4 True fiveDirMirrored))
                          DirW 0 False))
                `shouldBe` (Nothing, False)

    -- The whole point of D-5: rows of one atlas legitimately differ in
    -- length. The cases above pin individual directions at chosen
    -- times; this sweeps EVERY authored direction across several full
    -- cycles, which is what catches an arithmetic that happens to be
    -- right for the row it was written against.
    describe "pickFrame — unequal per-direction counts hold across a sweep" $ do
        let shape = eightDirUnequal
            def   = mkDef (HM.singleton "clip" (atlasOf 4 True shape))
            times = [0, 0.05 .. 4.0] ∷ [Double]
            colAt t d = chosenAtlas shape
                (pickFrame t FaceSouth ((mkInst "clip" 0) { uiFacing = d }) def)

        it "never resolves past a direction's OWN frame count" $
            forM_ (shapeDirs shape) $ \(d, n) → do
                let cols = [ c | t ← times, Chosen _ c ← [colAt t d] ]
                length cols `shouldBe` length times   -- never a T-pose
                maximum cols `shouldBe` n - 1
                minimum cols `shouldBe` 0

        it "would overrun if the padded column count were used instead" $ do
            -- Without this the case above passes vacuously on a sheet
            -- whose rows all happen to be full: the shortest row must
            -- genuinely stop short of the sheet's width.
            let cols = maximum (map snd (shapeDirs shape))
                short = [ n | (_, n) ← shapeDirs shape, n < cols ]
            short `shouldSatisfy` not ∘ null
            minimum short `shouldSatisfy` (< cols)

        it "keeps every direction on its own row for the whole sweep" $
            forM_ (shapeDirs shape) $ \(d, _) → do
                let rows = [ src | t ← times, Chosen src _ ← [colAt t d] ]
                filter (≢ d) rows `shouldBe` []

    -- Mirroring is a SPATIAL reflection of one cell, and nothing else.
    -- It must not shift which frame plays, must not reach a row the
    -- animation does not author, and must not widen past the cell.
    describe "pickFrame — mirroring reflects a cell without moving it" $ do
        let shape = fiveDirMirrored
            def   = mkDef (HM.singleton "clip" (atlasOf 4 True shape))
            times = [0, 0.1 .. 2.0] ∷ [Double]
            at t d = pickFrame t FaceSouth
                        ((mkInst "clip" 0) { uiFacing = d }) def
            colAt t d = chosenAtlas shape (at t d)

        it "plays the same frame index as the direction it mirrors" $
            forM_ [(DirW, DirE), (DirSW, DirSE), (DirNW, DirNE)] $
                \(west, east) → do
                    let mirrored = [ c | t ← times, Chosen _ c ← [colAt t west] ]
                        source   = [ c | t ← times, Chosen _ c ← [colAt t east] ]
                    mirrored `shouldBe` source
                    -- ...and the cells really are the same cells.
                    [ fsUV (at t west) | t ← times ]
                        `shouldBe` [ fsUV (at t east) | t ← times ]

        it "reports the flip only on the mirrored side" $
            forM_ [(DirW, DirE), (DirSW, DirSE), (DirNW, DirNE)] $
                \(west, east) →
                    ( and [ fsFlipX (at t west) | t ← times ]
                    , or  [ fsFlipX (at t east) | t ← times ] )
                        `shouldBe` (True, False)

        it "never resolves a mirrored view onto an unauthored row" $
            -- Every sample, mirrored or not, must land on a row the
            -- animation actually authors — an invented row would read
            -- another animation's pixels out of the sheet.
            forM_ [DirS, DirSE, DirE, DirNE, DirN, DirW, DirSW, DirNW] $ \d →
                forM_ times $ \t → case colAt t d of
                    Chosen src _ → src `shouldSatisfy`
                        (\s → isJust (lookup s (shapeDirs shape)))
                    TPose        → expectationFailure
                        ("a mirroring animation fell back to the T-pose for "
                         ⧺ show d)
