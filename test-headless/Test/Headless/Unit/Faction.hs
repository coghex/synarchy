{-# LANGUAGE Strict #-}
-- | The "Unit faction model" gate (#912): the typed faction's wire tags,
--   its PROPERTIES (player-owned / player-commandable /
--   unrestricted-combat), the total symmetric ally-neutral-hostile
--   RELATION, and the units-component save path that carries a faction
--   as 'Text' either side of the typed runtime field.
--
--   Pure fixtures only, no engine. See 'Test.Headless.Lua.Faction' for
--   the same model asserted through the Lua API, and
--   'Test.Headless.Location.Discovery' /
--   'Test.Headless.World.LocationDiscovery' for the ownership-vs-alliance
--   regression that is the reason those two stay distinct.
--
--   Run just this gate: @cabal test synarchy-test-headless
--   --test-options='--match "Unit faction model"'@.
module Test.Headless.Unit.Faction (spec) where

import UPrelude
import Test.Hspec
import qualified Data.HashMap.Strict as HM
import qualified Data.List as L
import qualified Data.Map.Strict as Map
import qualified Data.Serialize as S
import Engine.Asset.Handle (TextureHandle(..))
import Unit.Direction (Direction(..))
import Unit.Faction
import Unit.Types
    ( BodyPart(..), UnitDef(..), UnitId(..), UnitInstance(..)
    , UnitManager(..), defaultNaturalResistance, emptyUnitManager )
import Infection.Types (emptyInfectionManager)
import World.Page.Types (WorldPageId(..))
import World.Save.Component (saveComponentRegistry)
import World.Save.Component.Entities
    ( UnitInstanceDTO(..), fromUnitInstanceDTO, toUnitInstanceDTO )
import World.Save.Component.Types (RegisteredComponent(..))
import World.Save.Envelope.Types (ComponentId(..))
import World.Save.Types
    ( ImmunityScrub, UnitInstanceSnapshot(..), UnitSnapshot(..)
    , fromUnitSnapshot, toUnitSnapshot )

-- * Fixtures

pageA ∷ WorldPageId
pageA = WorldPageId "faction_page"

defs ∷ HM.HashMap Text UnitDef
defs = HM.singleton "t" minimalDef

-- | Mirrors 'Test.Headless.Blood.Trail.minimalDef' — only the fields
--   'fromUnitSnapshot' re-resolves carry any weight here.
minimalDef ∷ UnitDef
minimalDef = UnitDef
    { udName = "t", udNamePool = Nothing, udDisplayName = Nothing
    , udTexture = TextureHandle 0, udPortrait = Nothing
    , udDirSprites = Map.empty
    , udBaseWidth = 0, udMaxSpeed = 1.0, udRunThreshold = 0.6
    , udAnimations = HM.empty, udStateAnims = HM.empty, udEagerStats = False
    , udStatTemplates = HM.empty, udBodyTemplates = HM.empty
    , udSkillTemplates = HM.empty, udKnowledgeTemplates = HM.empty
    , udStartingInventory = []
    , udEquipmentClass = Nothing, udStartingEquipment = HM.empty
    , udStartingAccessories = []
    , udBodyParts =
        [ BodyPart
            { bpId = "torso", bpName = "torso", bpParent = Nothing
            , bpVital = False, bpAreaWeight = 1.0, bpTacticalValue = 0.5
            , bpBleedFactor = 1.0, bpHeightLow = 0, bpHeightHigh = 1
            , bpLayers = [], bpTargetable = True, bpDepth = 0.0
            , bpAffectsLocomotion = False, bpAffectsBalance = False } ]
    , udNaturalResistance = defaultNaturalResistance
    , udNaturalWeapon = Nothing, udModifiers = [] }

-- | A live instance carrying @f@; every other field is inert.
instWith ∷ Faction → UnitInstance
instWith f = UnitInstance
    { uiDefName = "t", uiName = "", uiPage = pageA
    , uiTexture = TextureHandle 0, uiDirSprites = Map.empty
    , uiBaseWidth = 0, uiGridX = 0, uiGridY = 0, uiGridZ = 0
    , uiRealZ = 0, uiFacing = DirS
    , uiCurrentAnim = "", uiAnimStart = 0, uiAnimReverse = False
    , uiActivity = "idle", uiPose = "standing", uiAnimStride = 1
    , uiStats = HM.empty, uiModifiers = HM.empty, uiSkills = HM.empty
    , uiKnowledge = HM.empty, uiInventory = [], uiEquipment = HM.empty
    , uiAccessories = [], uiFactionId = f, uiWounds = []
    , uiScars = [], uiImmuneResponse = 0, uiImmunities = HM.empty
    , uiBlood = 5.0, uiLastAttackerUid = Nothing, uiLastAttackerAt = 0
    , uiAnimOverride = "", uiFrozen = False, uiForceLoop = False
    , uiClimbDest = Nothing, uiTrailState = Nothing
    }

-- | Take the given (unit id, raw faction tag) pairs all the way through
--   the REAL units-component path — snapshot adapter, component DTO,
--   cereal encode/decode, DTO adapter, snapshot restore — and hand back
--   what the load side produced. Overriding 'uisFactionId' after the
--   snapshot adapter is what lets a tag OUTSIDE the vocabulary be fed in:
--   a live 'UnitInstance' can no longer hold one.
loadTags ∷ [(UnitId, Text)] → (UnitManager, [UnitId], [Text], ImmunityScrub)
loadTags tagged =
    let um0   = emptyUnitManager
                  { umDefs = defs
                  , umInstances = HM.fromList
                      [ (uid, instWith FactionNeutral) | (uid, _) ← tagged ] }
        tags  = HM.fromList tagged
        retag uid s = maybe s (\t → s { uisFactionId = t }) (HM.lookup uid tags)
        snap0 = toUnitSnapshot pageA um0
        wire  = snap0 { usnInstances =
                          HM.mapWithKey retag (usnInstances snap0) }
        snap1 = wire { usnInstances =
                         HM.map throughComponent (usnInstances wire) }
    in fromUnitSnapshot pageA defs emptyInfectionManager snap1

-- | One instance snapshot through the units component's own DTO and its
--   derived cereal layout — the bytes a @world.synworld@ actually holds.
--   A decode failure is surfaced as a poisoned tag rather than an
--   exception so the assertion reports it.
throughComponent ∷ UnitInstanceSnapshot → UnitInstanceSnapshot
throughComponent s =
    case S.decode (S.encode (toUnitInstanceDTO s)) of
        Right dto → fromUnitInstanceDTO (dto ∷ UnitInstanceDTO)
        Left  err → s { uisFactionId = "<decode failed: "
                                        <> fromString err <> ">" }

factionOf ∷ UnitManager → UnitId → Maybe Faction
factionOf um uid = uiFactionId <$> HM.lookup uid (umInstances um)

-- | Every faction's wire tag, as the load path receives them.
allTags ∷ [(UnitId, Text)]
allTags = zip (map UnitId [1 ..]) (map factionTag allFactions)

spec ∷ Spec
spec = describe "Unit faction model" $ do

    describe "wire tags" $ do
        it "every faction has a distinct canonical tag" $
            length (L.nub (map factionTag allFactions))
                `shouldBe` length allFactions

        it "every canonical tag parses back to its own faction" $
            forM_ allFactions $ \f →
                parseFaction (factionTag f) `shouldBe` Just f

        it "the vocabulary is exactly the five tags the issue names" $
            map factionTag allFactions
                `shouldMatchList`
                ["player", "wildlife", "hostile", "neutral", "debug"]

        it "an unrecognized tag has no strict parse" $
            forM_ ["", "Player", "player ", "wildlife2", "debug_faction"] $
                \t → parseFaction t `shouldBe` Nothing

        it "an unrecognized tag resolves to the documented fallback" $
            forM_ ["", "Player", "debug_faction"] $ \t →
                factionFromTag t `shouldBe` fallbackFaction

        it "the fallback is neutral — inert in every direction" $ do
            fallbackFaction `shouldBe` FactionNeutral
            isPlayerOwned fallbackFaction         `shouldBe` False
            isPlayerCommandable fallbackFaction   `shouldBe` False
            hasUnrestrictedCombat fallbackFaction `shouldBe` False
            -- Nothing is hostile to it, so a bad tag can never make a
            -- unit newly attackable except via debug's own override.
            forM_ allFactions $ \f →
                canAttack fallbackFaction f `shouldBe` hasUnrestrictedCombat f

        it "the tag-less unit.spawn default is wildlife" $
            defaultSpawnFaction `shouldBe` FactionWildlife

    describe "properties" $ do
        it "only the player faction is player-OWNED" $
            filter isPlayerOwned allFactions `shouldBe` [FactionPlayer]

        it "player and debug are player-COMMANDABLE" $
            filter isPlayerCommandable allFactions
                `shouldMatchList` [FactionPlayer, FactionDebug]

        it "only debug has unrestricted combat" $
            filter hasUnrestrictedCombat allFactions `shouldBe` [FactionDebug]

        it "debug is player-allied and player-commandable but NOT \
           \player-owned — the distinction discovery depends on" $ do
            areAllies FactionDebug FactionPlayer `shouldBe` True
            isPlayerCommandable FactionDebug     `shouldBe` True
            isPlayerOwned FactionDebug           `shouldBe` False

    describe "relation table" $ do
        it "is total — every ordered pair answers with a real relation" $
            length [ ()
                   | a ← allFactions, b ← allFactions
                   , factionRelation a b
                       `elem` [RelAlly, RelNeutral, RelHostile] ]
                `shouldBe` length allFactions * length allFactions

        it "is symmetric for every pair" $
            forM_ [(a, b) | a ← allFactions, b ← allFactions] $ \(a, b) →
                factionRelation a b `shouldBe` factionRelation b a

        it "a faction is allied with itself — which is what keeps \
           \wildlife one mutually-allied faction" $
            forM_ allFactions $ \f →
                factionRelation f f `shouldBe` RelAlly

        it "player and debug are allied — the medic pairing, now declared" $ do
            factionRelation FactionPlayer FactionDebug `shouldBe` RelAlly
            factionRelation FactionDebug FactionPlayer `shouldBe` RelAlly

        it "any distinct pair involving neutral is neutral" $
            forM_ [f | f ← allFactions, f ≢ FactionNeutral] $ \f → do
                factionRelation FactionNeutral f `shouldBe` RelNeutral
                factionRelation f FactionNeutral `shouldBe` RelNeutral

        it "every other distinct pair is hostile" $
            forM_ [ (FactionPlayer,   FactionWildlife)
                  , (FactionPlayer,   FactionHostile)
                  , (FactionWildlife, FactionHostile)
                  , (FactionDebug,    FactionWildlife)
                  , (FactionDebug,    FactionHostile)
                  ] $ \(a, b) → factionRelation a b `shouldBe` RelHostile

        it "areAllies agrees with the relation table everywhere" $
            forM_ [(a, b) | a ← allFactions, b ← allFactions] $ \(a, b) →
                areAllies a b `shouldBe` (factionRelation a b ≡ RelAlly)

    describe "attack permission" $ do
        it "preserves every direction the debug overlay allows today" $
            forM_ [ (FactionPlayer, FactionDebug)
                  , (FactionDebug,  FactionPlayer)
                  , (FactionDebug,  FactionDebug)
                  ] $ \(a, b) → canAttack a b `shouldBe` True

        it "a player unit may attack wildlife and hostiles" $ do
            canAttack FactionPlayer FactionWildlife `shouldBe` True
            canAttack FactionPlayer FactionHostile  `shouldBe` True

        it "a player unit may not attack another player unit" $
            canAttack FactionPlayer FactionPlayer `shouldBe` False

        it "is hostility, or unrestricted combat on either side" $
            forM_ [(a, b) | a ← allFactions, b ← allFactions] $ \(a, b) →
                canAttack a b `shouldBe`
                    (  factionRelation a b ≡ RelHostile
                     ∨ hasUnrestrictedCombat a
                     ∨ hasUnrestrictedCombat b )

    describe "units-component save path (the wire stays Text)" $ do
        it "all five recognized tags survive the real component path \
           \unchanged, with nothing reported" $ do
            let (um, orphans, unknowns, _) = loadTags allTags
            orphans  `shouldBe` []
            unknowns `shouldBe` []
            map (factionOf um . fst) allTags
                `shouldBe` map (Just . factionFromTag . snd) allTags

        it "a live unit re-serializes to the canonical lowercase TAG, \
           \not a positional enum — the component format is unchanged" $ do
            let um0 = emptyUnitManager
                        { umDefs = defs
                        , umInstances = HM.fromList
                            (zip (map UnitId [1 ..])
                                 (map instWith allFactions)) }
            map uisFactionId (HM.elems (usnInstances (toUnitSnapshot pageA um0)))
                `shouldMatchList` map factionTag allFactions

        it "unrecognized tags load as the fallback and are reported ONCE \
           \per distinct tag, however many units carry them" $ do
            let (um, orphans, unknowns, _) = loadTags
                    [ (UnitId 1, "player")
                    , (UnitId 2, "made_up")
                    , (UnitId 3, "made_up")
                    , (UnitId 4, "made_up")
                    , (UnitId 5, "also_bogus")
                    , (UnitId 6, "debug")
                    ]
            orphans  `shouldBe` []
            unknowns `shouldBe` ["also_bogus", "made_up"]
            factionOf um (UnitId 1) `shouldBe` Just FactionPlayer
            forM_ [2, 3, 4, 5] $ \n →
                factionOf um (UnitId n) `shouldBe` Just fallbackFaction
            factionOf um (UnitId 6) `shouldBe` Just FactionDebug

        it "a unit loaded from an unrecognized tag re-serializes as the \
           \fallback's canonical tag" $ do
            let (um, _, _, _) = loadTags [(UnitId 1, "made_up")]
                back = toUnitSnapshot pageA um { umDefs = defs }
            map uisFactionId (HM.elems (usnInstances back))
                `shouldBe` [factionTag fallbackFaction]

        it "typing the runtime field is not a wire change — the units \
           \component still accepts the version it did before, and the \
           \faction tag is still carried as Text" $ do
            -- The original assertion pinned the component at v1. #1233
            -- later bumped it to v2 for an unrelated reason (the item
            -- tree gained physical values), so what this case actually
            -- means is that the FACTION work added no version of its own:
            -- v1 is still an accepted input, and the tag round-trips
            -- through it unchanged.
            [ rcInputVers c
              | c ← saveComponentRegistry
              , rcId c ≡ ComponentId "units" ] `shouldBe` [[1, 2]]
            let (um, _, _, _) = loadTags [(UnitId 1, "player")]
                back = toUnitSnapshot pageA um { umDefs = defs }
            map uisFactionId (HM.elems (usnInstances back))
                `shouldBe` [factionTag FactionPlayer]
