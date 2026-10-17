{-# LANGUAGE Strict, OverloadedStrings #-}
-- | The one blood adapter and the reads built on it (#2837, S10; T-S10-5):
--   the raw and Simulated views against an INDEPENDENT reference of the
--   pre-change values, the Nominal branch, the registry-less and Unsupported
--   fallbacks, and the Lua face (@unit.getBlood@, @unit.readBloodView@, the
--   owned and derived blood keys) in every mode.
--
--   The reference below is written out here from the literals every blood
--   reader had before S10 (stored 'uiBlood'; base @body_mass@, 70 kg
--   fallback, times 0.075; the fraction clamped to 0..1, 1.0 when the maximum
--   is not positive; the per-wound bleed term). It shares no code with
--   "Unit.Blood", so a test that compared the adapter to itself, or to a
--   wrapper of itself, would prove nothing.
module Test.Headless.Unit.PhysiologyBloodModes.Adapter (spec) where

import UPrelude
import Test.Hspec
import qualified Data.HashMap.Strict as HM
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import Engine.Asset.Handle (TextureHandle(..))
import Test.Headless.Unit.PhysiologyFixture (readShippedPhysiologyRegistry)
import Test.Headless.Unit.PhysiologyModes.Harness
import Unit.Blood
import Unit.Direction (Direction(..))
import Unit.Faction (Faction(..))
import Unit.Faction.Membership (resolveLegacyFaction)
import Unit.Physiology
import Unit.Physiology.Types
import Unit.Types
import Test.Headless.Unit.TransferApi (minimalDef)
import World.Page.Types (WorldPageId(..))

spec ∷ Spec
spec = describe "the one blood adapter and its reads" $ do
    gridSpec
    luaSpec

-----------------------------------------------------------------------
-- T-S10-5: the pinned grid against the independent reference
-----------------------------------------------------------------------

-- | The wounds of the grid and their per-wound bleed term, written out from
--   the old formula: effective severity squared x kind x part x bandage x
--   (1 - clot) x 1.2 / clamp(constitution, 0.5, 2).
gridWounds ∷ [[Wound]]
gridWounds = [ [], [w "slash" 0.5 1.0 0.0], [w "stab" 0.8 0.2 0.5, w "internal" 0.3 1.0 0.0] ]
  where
    w k sev band clot = Wound
        { woundPart = "torso", woundKind = k, woundSeverity = sev
        , woundAt = 0, woundBandage = band, woundClot = clot, woundHeal = 0.0
        , woundDressing = "", woundInfection = 0.0, woundClean = True
        , woundInfectionType = "", woundNecrosis = 0.0 }

refRate ∷ Float → [Wound] → Float
refRate con ws = sum
    [ sev * sev * kindFactor (woundKind w) * woundBandage w * (1 - woundClot w)
        * 1.2 / max 0.5 (min 2.0 con)
    | w ← ws, let sev = woundSeverity w ]
  where
    kindFactor k = case k of
        "slash" → 1.0; "stab" → 0.6; "internal" → 0.5; _ → 0.5 ∷ Float

refView ∷ Maybe Float → Float → Float → (Float, Float, Float)
refView bodyMass vol _ = (vol, mx, frac)
  where
    mx = fromMaybe 70 bodyMass * 0.075
    frac | mx > 0 = max 0 (min 1 (vol / mx))
         | otherwise = 1

gridDef ∷ Bool → UnitDef
gridDef bodied = (minimalDef "blood_grid" "blood_grid")
    { udPhysiology = if bodied
        then plainUnitPhysiology True Nothing (HM.fromList [("blood", PhysioSimulated)])
        else noUnitPhysiology
    , udBodyParts = [BodyPart
        { bpId = "torso", bpName = "torso", bpParent = Nothing
        , bpVital = False, bpAreaWeight = 1.0, bpTacticalValue = 0.5
        , bpBleedFactor = 1.0, bpHeightLow = 0, bpHeightHigh = 1
        , bpLayers = [], bpTargetable = True, bpDepth = 0.0
        , bpAffectsLocomotion = False, bpAffectsBalance = False }] }

gridInst ∷ Maybe Float → Float → [Wound] → [(Text, PhysioMode)] → UnitInstance
gridInst bodyMass vol ws pins = UnitInstance
    { uiDefName = "blood_grid", uiName = "", uiPage = WorldPageId "grid"
    , uiTexture = TextureHandle 0, uiDirSprites = Map.empty
    , uiBaseWidth = 0, uiGridX = 0, uiGridY = 0, uiGridZ = 0, uiRealZ = 0
    , uiFacing = DirS, uiCurrentAnim = "", uiAnimStart = 0, uiAnimReverse = False
    , uiActivity = "idle", uiPose = "standing", uiAnimStride = 1
    , uiStats = HM.fromList ([("constitution", 1.0)]
                             ⧺ [ ("body_mass", m) | Just m ← [bodyMass] ])
    , uiModifiers = HM.empty, uiSkills = HM.empty, uiKnowledge = HM.empty
    , uiInventory = [], uiEquipment = HM.empty, uiAccessories = []
    , uiFaction = resolveLegacyFaction [] FactionPlayer, uiWounds = ws
    , uiScars = [], uiImmuneResponse = 0, uiImmunities = HM.empty
    , uiBlood = vol, uiLastAttackerUid = Nothing, uiLastAttackerAt = 0
    , uiAnimOverride = "", uiFrozen = False, uiForceLoop = False
    , uiClimbDest = Nothing, uiPhysioOverrides = HM.fromList pins
    , uiTrailState = Nothing }

gridPoints ∷ [(Maybe Float, Float, [Wound])]
gridPoints = [ (m, v, ws)
             | m ← [Nothing, Just 0, Just (-5), Just 0.3, Just 70, Just 150]
             , v ← [-1, 0, 0.5, 2.1, 5.25, 9]
             , ws ← gridWounds ]

closeTo ∷ Float → Float → Bool
closeTo a b = abs (a - b) < 1e-5

gridSpec ∷ Spec
gridSpec = beforeAll readShippedPhysiologyRegistry $
    describe "bloodView against the pre-change values (T-S10-5)" $ do
    it "the raw view, a Simulated blood, an Unsupported blood, a definition \
       \the registry does not know and a registry with no blood system ALL \
       \equal the independent reference over the pinned grid" $ \reg → do
        let bad = [ (m, v, label)
                  | (m, v, ws) ← gridPoints
                  , (label, view) ←
                      [ ("raw", bloodView (gridDef True) (gridInst m v ws []))
                      , ("simulated"
                        , instanceBloodView reg (gridDef True) (gridInst m v ws []))
                      , ("pinned simulated"
                        , instanceBloodView reg (gridDef True)
                              (gridInst m v ws [("blood", PhysioSimulated)]))
                      , ("unsupported"
                        , instanceBloodView reg (gridDef False) (gridInst m v ws []))
                      , ("no blood system"
                        , instanceBloodView emptyPhysiologyRegistry (gridDef True)
                              (gridInst m v ws [("blood", PhysioNominal)])) ]
                  , let (rv, rm, rf) = refView m v 0
                        rr = refRate 1.0 ws
                  , not (closeTo (bvVolume view) rv ∧ closeTo (bvMax view) rm
                         ∧ closeTo (bvFraction view) rf
                         ∧ closeTo (bvBleedRate view) rr) ]
        length gridPoints `shouldBe` 6 * 6 * 3
        take 3 bad `shouldBe` []

    it "a Nominal blood reads volume = maximum and fraction 1.0 whatever the \
       \stored volume, with the live bleed rate, and a body-mass gain after \
       \the field was written cannot make it partial" $ \reg → do
        let bad = [ (m, v)
                  | (m, v, ws) ← gridPoints
                  , let view = instanceBloodView reg (gridDef True)
                                   (gridInst m v ws [("blood", PhysioNominal)])
                        (_, rm, _) = refView m v 0
                  , not (closeTo (bvVolume view) rm ∧ closeTo (bvMax view) rm
                         ∧ bvFraction view ≡ 1.0
                         ∧ closeTo (bvBleedRate view) (refRate 1.0 ws)) ]
        take 3 bad `shouldBe` []

-----------------------------------------------------------------------
-- The Lua face
-----------------------------------------------------------------------

-- | A unit of the @rich@ profile (every system Simulated) carrying a stale
--   stored volume and the given pin.
blooded ∷ Int → Maybe PhysioMode → Float → UnitSpec
blooded uid pin vol = (unitSpec uid "acolyte" (Just "rich"))
    { usBlood = Just vol
    , usOverrides = maybe [] (\m → [("blood", m)]) pin }

luaSpec ∷ Spec
luaSpec = describe "unit.getBlood, unit.readBloodView and the owned and derived keys" $ do
    it "answers today's {current, max, bleedRate} record in every mode, \
       \mode-aware: a Nominal unit reads current = max whatever is stored" $
        withScene [ blooded 1 Nothing 2.0, blooded 2 (Just PhysioNominal) 2.0
                  , blooded 3 (Just PhysioSimulated) 0 ] $ \(_, ls) → do
            forM_ [1, 2, 3 ∷ Int] $ \u →
                luaText ls ("local n = 0; for _ in pairs(unit.getBlood(" <> tshow u
                            <> ")) do n = n + 1 end; return n") `shouldReturn` "3"
            luaNum ls "unit.getBlood(1).current" >>= near 1e-5 2.0
            luaNum ls "unit.getBlood(1).max" >>= near 1e-5 5.25
            luaNum ls "unit.getBlood(2).current" >>= near 1e-5 5.25
            luaNum ls "unit.getBlood(2).max" >>= near 1e-5 5.25
            luaNum ls "unit.getBlood(3).current" >>= near 1e-9 0
            -- readBloodView is the same adapter
            luaNum ls "unit.readBloodView(2).fraction" >>= near 1e-9 1
            luaNum ls "unit.readBloodView(1).fraction" >>= near 1e-5 (2.0 / 5.25)
            luaText ls "return tostring(unit.getBlood(99))" `shouldReturn` "nil"

    it "reads the owned volume and both derived keys through the registry: \
       \Simulated stored, Nominal healthy, with the right mode" $
        withScene [blooded 1 Nothing 2.0, blooded 2 (Just PhysioNominal) 2.0] $
            \(_, ls) → do
                readOwned ls 1 "blood" `shouldReturn` "2|simulated"
                readOwned ls 1 "blood_max" `shouldReturn` "5.25|simulated"
                readOwned ls 1 "blood_fraction" >>= (`shouldSatisfy` T.isSuffixOf "|simulated")
                readOwned ls 2 "blood" `shouldReturn` "5.25|nominal"
                readOwned ls 2 "blood_max" `shouldReturn` "5.25|nominal"
                readOwned ls 2 "blood_fraction" `shouldReturn` "1|nominal"
                -- the base read of a field-backed key is the same
                luaNum ls "unit.readPhysioBase(1, 'blood')" >>= near 1e-6 2.0

    it "never stores a shadow pool: a raw write of the volume or a derived key \
       \is refused in every mode, and no uiStats entry exists" $
        withScene [blooded 1 Nothing 2.0, blooded 2 (Just PhysioNominal) 2.0] $
            \(env, ls) → forM_ [1, 2 ∷ Int] $ \u → do
                luaText ls ("return table.concat({ tostring(unit.setStat(" <> tshow u
                            <> ", 'blood', 1)), tostring(unit.setStat(" <> tshow u
                            <> ", 'blood_max', 1)), tostring(unit.setStat(" <> tshow u
                            <> ", 'blood_fraction', 1)) }, ',')")
                    `shouldReturn` "false,false,false"
                i ← readInstance env u
                forM_ ["blood", "blood_max", "blood_fraction"] $ \k →
                    (k, HM.member k (uiStats i)) `shouldBe` (k, False)
                uiBlood i `shouldBe` 2.0

    it "keeps an Unsupported (bodyless) unit's blood record at today's raw \
       \values while every physiology read of it is nil" $
        withScene [(unitSpec 1 "white_tailed_deer" Nothing) { usBlood = Just 0 }] $
            \(_, ls) → do
                forM_ ["blood", "blood_max", "blood_fraction"] $ \k →
                    readOwned ls 1 k `shouldReturn` "nil|nil"
                luaText ls "return tostring(unit.getPhysioMode(1, 'blood'))"
                    `shouldReturn` "nil"
                -- today's behavior: the raw volume against the 70 kg maximum
                luaNum ls "unit.getBlood(1).current" >>= near 1e-9 0
                luaNum ls "unit.getBlood(1).max" >>= near 1e-5 5.25

    it "leaves the blood maximum at the BASE body mass under a body-mass \
       \modifier, in both modes" $
        withScene
            [ (blooded 1 Nothing 5.25)
                { usModifiers = [("body_mass", [StatModifier 0 "spec" Nothing 1.0])] }
            , (blooded 2 (Just PhysioNominal) 1.0)
                { usModifiers = [("body_mass", [StatModifier 0 "spec" Nothing 1.0])] } ] $
            \(_, ls) → forM_ [1, 2 ∷ Int] $ \u → do
                luaNum ls ("unit.getBlood(" <> tshow u <> ").max") >>= near 1e-5 5.25
                luaNum ls ("unit.readPhysio(" <> tshow u <> ", 'blood_max')")
                    >>= near 1e-5 5.25
