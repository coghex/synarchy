{-# LANGUAGE Strict #-}
{-# LANGUAGE OverloadedStrings #-}
-- | "Unit faction profile wire" (#2515, FTS-3 of #2496): the faction
--   profile every live and persisted unit carries.
--
--   * the legacy adapter — exact on the D-26 profiles, @neutral@ for a
--     culture-only one;
--   * the @units@ v1 and v2 shapes — every legacy string, for a
--     definition with defaults and one without, decoded by the REAL
--     component dispatch and resolved by D-26 at the restore boundary;
--     unknown strings degrading inert and reported once each; re-encode
--     emitting v3 only;
--   * exact v3 preservation through bytes, runtime restoration and the
--     next capture, including provenance a definition no longer
--     declares;
--   * spawn ingress through the real @unit.spawn@ against the shipped
--     acolyte, bear and tiller definitions.
--
--   Run just this gate: @cabal test synarchy-test-headless
--   --test-options='--match "Unit faction profile wire"'@.
module Test.Headless.Unit.FactionProfileWire (spec) where

import UPrelude
import Test.Hspec
import Data.IORef (atomicModifyIORef', newIORef, writeIORef)
import Data.List (sort)
import qualified Data.ByteString as BS
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import qualified Data.Map.Strict as Map
import qualified Data.Serialize as S
import qualified Data.Text as T
import System.FilePath ((</>))

import Engine.Asset.Discovery (walkFilesWithExtension)
import Engine.Asset.Handle (TextureHandle(..))
import Engine.Core.Init (EngineInitResult(..))
import Engine.Core.State (EngineEnv(..))
import Engine.Core.Thread (ThreadControl(..))
import qualified Engine.Core.Queue as Q
import Engine.Scripting.Lua.API (registerLuaAPI)
import Engine.Scripting.Lua.Thread (createLuaBackendState)
import Engine.Scripting.Lua.Thread.Console (executeDebugLua)
import Engine.Scripting.Lua.Types (LuaBackendState(..))
import Equipment.Types (emptyEquipmentClassManager)
import Infection.Types (emptyInfectionManager)
import Item.Types (emptyItemManager)
import Test.Headless.Harness.Isolation (withIsolatedResourceRoot)
import Test.Headless.Harness.Log (initializeEngineHeadlessQuiet)
import Unit.Command.Types (UnitCommand(..))
import Unit.Direction (Direction(..))
import Unit.Faction (Faction(..), allFactions, factionTag)
import Unit.Faction.Membership
import Unit.Faction.Profile
    ( FactionCapability(..), FactionTag, aiController, mkFactionTag
    , tagAcolyte, tagLegacyHostile, tagWildlife )
import Unit.Types
    ( BodyPart(..), UnitDef(..), UnitId(..), UnitInstance(..)
    , UnitManager(..), defaultNaturalResistance, emptyUnitManager )
import World.Page.Types (WorldPageId(..))
import World.Save.Component.Entities
    ( PageUnitsDTO(..), PageUnitsDTOv1(..), PageUnitsDTOv2(..)
    , UnitInstanceDTO(..), UnitsDTO(..), UnitsDTOv1(..), UnitsDTOv2(..)
    , fromUnitInstanceDTO, toUnitInstanceDTO, toUnitInstanceDTOv1
    , toUnitInstanceDTOv2, unitsCodec )
import World.Save.Component.Types (ComponentCodec(..))
import World.Save.Compat.SessionV90
    ( SaveDataV90(..), UnitSnapshotV90(..), WorldPageSaveV90(..)
    , migrateSessionV90 )
import World.Save.Envelope (decodeSessionEnvelope)
import Test.Headless.World.Save.Components.Fixture
    ( minimalSaveDataV90, minimalSaveMetadataV90, minimalWorldPageSaveV90 )
import World.Save.Snapshot (PageSnapshot(..), SessionSnapshot(..))
import World.Save.Types
    ( UnitInstanceSnapshot(..), UnitSnapshot(..), fromUnitSnapshot
    , toUnitSnapshot )
import World.Save.UnitFaction
    ( UnitFactionDTO(..), UnitFactionSnapshot(..), fromUnitFactionDTO
    , toUnitFactionDTO )
import World.State.Types (WorldManager(..), emptyWorldManager, emptyWorldState)

-- * Pure fixtures

pageA ∷ WorldPageId
pageA = WorldPageId "profile_wire_page"

tagNomad ∷ FactionTag
tagNomad = fromMaybe (error "nomad is a valid tag") (mkFactionTag "nomad")

-- | @cultured@ declares the acolyte default; @bare@ declares none.
culturedName, bareName ∷ Text
culturedName = "cultured"
bareName     = "bare"

defWith ∷ Text → [FactionTag] → UnitDef
defWith n tags = UnitDef
    { udName = n, udNamePool = Nothing, udDisplayName = Nothing
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
    , udNaturalWeapon = Nothing, udModifiers = [], udFactionTags = tags }

defs ∷ HM.HashMap Text UnitDef
defs = HM.fromList
    [ (culturedName, defWith culturedName [tagAcolyte])
    , (bareName,     defWith bareName     []) ]

instOf ∷ Text → UnitFactionProfile → UnitInstance
instOf defName p = UnitInstance
    { uiDefName = defName, uiName = "", uiPage = pageA
    , uiTexture = TextureHandle 0, uiDirSprites = Map.empty
    , uiBaseWidth = 0, uiGridX = 0, uiGridY = 0, uiGridZ = 0
    , uiRealZ = 0, uiFacing = DirS
    , uiCurrentAnim = "", uiAnimStart = 0, uiAnimReverse = False
    , uiActivity = "idle", uiPose = "standing", uiAnimStride = 1
    , uiStats = HM.empty, uiModifiers = HM.empty, uiSkills = HM.empty
    , uiKnowledge = HM.empty, uiInventory = [], uiEquipment = HM.empty
    , uiAccessories = [], uiFaction = p, uiWounds = []
    , uiScars = [], uiImmuneResponse = 0, uiImmunities = HM.empty
    , uiBlood = 5.0, uiLastAttackerUid = Nothing, uiLastAttackerAt = 0
    , uiAnimOverride = "", uiFrozen = False, uiForceLoop = False
    , uiClimbDest = Nothing, uiTrailState = Nothing
    }

-- | Instance snapshots for @(unit id, definition, legacy string)@,
--   carrying that string pending exactly as a v1\/v2 payload would.
legacySnapshots ∷ [(UnitId, Text, Text)]
                → HM.HashMap UnitId UnitInstanceSnapshot
legacySnapshots rows =
    let um = emptyUnitManager
            { umDefs = defs
            , umInstances = HM.fromList
                [ (uid, instOf d inertUnitProfile) | (uid, d, _) ← rows ] }
        snaps = usnInstances (toUnitSnapshot pageA um)
        strings = HM.fromList [ (uid, t) | (uid, _, t) ← rows ]
    in HM.mapWithKey
           (\uid s → maybe s (\t → s { uisFaction = FactionLegacyPending t })
                           (HM.lookup uid strings))
           snaps

-- | The REAL @units@ decode dispatch at an encoded version, from bytes.
decodeUnitsAt ∷ Word32 → S.Put → Either Text UnitsDTO
decodeUnitsAt ver put =
    either (Left ∘ tshow) Right (ccDecode unitsCodec ver (S.runPut put))

-- | A whole @units@ payload at v1 or v2 for the given rows, decoded,
--   restored against 'defs': the restored manager and the unknown
--   strings the restore reported.
loadLegacy ∷ Word32 → [(UnitId, Text, Text)]
           → Either Text (UnitManager, [Text])
loadLegacy ver rows = do
    let snaps = legacySnapshots rows
        put = case ver of
            1 → S.put (UnitsDTOv1 [PageUnitsDTOv1 pageA
                                      (HM.map toUnitInstanceDTOv1 snaps)])
            _ → S.put (UnitsDTOv2 [PageUnitsDTOv2 pageA
                                      (HM.map toUnitInstanceDTOv2 snaps)])
    UnitsDTO slices ← decodeUnitsAt ver put
    pure (restoreSlices slices)

restoreSlices ∷ [PageUnitsDTO] → (UnitManager, [Text])
restoreSlices slices =
    let snap = UnitSnapshot
            { usnInstances = HM.unions
                [ HM.map fromUnitInstanceDTO (puInstances s) | s ← slices ]
            , usnNextId = 100 }
        (um, _, unknowns, _, _) =
            fromUnitSnapshot pageA defs emptyInfectionManager
                emptyEquipmentClassManager emptyItemManager snap
    in (um, unknowns)

-- | One live instance's snapshot, through the real capture adapter.
toUnitInstanceSnapshotOf ∷ UnitInstance → UnitInstanceSnapshot
toUnitInstanceSnapshotOf u =
    case HM.elems (usnInstances (toUnitSnapshot pageA
            emptyUnitManager { umInstances = HM.singleton (UnitId 1) u })) of
        [s] → s
        _   → error "one instance in, one snapshot out"

profileOf ∷ UnitManager → UnitId → Maybe UnitFactionProfile
profileOf um uid = uiFaction <$> HM.lookup uid (umInstances um)

legacyRows ∷ Text → [(UnitId, Text, Text)]
legacyRows defName =
    [ (UnitId n, defName, factionTag f)
    | (n, f) ← zip [1 ..] allFactions ]

-- | Every profile shape this slice can hold, and one it cannot mint yet
--   but must still persist exactly: an AI controller, an authored
--   default, two runtime-owned memberships, and a capability.
richProfile ∷ UnitFactionProfile
richProfile = mkUnitFactionProfile (Just (aiController "raiders"))
    [ TagMembership tagNomad MemberDefinitionDefault
    , TagMembership tagLegacyHostile (MemberRuntimeOwner legacyMappingOwner)
    , TagMembership tagAcolyte (MemberRuntimeOwner "fight_scene") ]
    [CapUnrestrictedCombat]

spec ∷ Spec
spec = describe "Unit faction profile wire" $ do
    adapterSpec
    legacyWireSpec
    exactWireSpec
    ingressSpec

-- * The adapter

adapterSpec ∷ Spec
adapterSpec = describe "the legacy adapter" $ do
    it "is the exact inverse of D-26 on every legacy profile of a \
       \definition without defaults and one declaring wildlife" $
        forM_ [[], [tagWildlife]] $ \ds → forM_ allFactions $ \f →
            legacyFactionOf (resolveLegacyFaction ds f) `shouldBe` f

    it "is exact on player, hostile, neutral and debug whatever the \
       \definition declares" $
        forM_ [[tagAcolyte], [tagNomad]] $ \ds →
            forM_ [FactionPlayer, FactionHostile, FactionNeutral, FactionDebug]
                $ \f → legacyFactionOf (resolveLegacyFaction ds f) `shouldBe` f

    it "answers neutral for a culture-only profile — an uncontrolled \
       \acolyte or nomad" $ do
        legacyFactionOf (resolveLegacyFaction [tagNomad] FactionWildlife)
            `shouldBe` FactionNeutral
        legacyFactionOf (resolveLegacyFaction [tagAcolyte] FactionWildlife)
            `shouldBe` FactionNeutral
        legacyFactionOf (mkUnitFactionProfile Nothing
                            [TagMembership tagNomad MemberDefinitionDefault] [])
            `shouldBe` FactionNeutral

    it "reads the precedence controller, debug, legacy_hostile, wildlife" $ do
        let both = [CapLocalCommandable, CapUnrestrictedCombat]
            hostileM = TagMembership tagLegacyHostile MemberDefinitionDefault
            wildM    = TagMembership tagWildlife MemberDefinitionDefault
        legacyFactionOf (mkUnitFactionProfile (Just localController)
                            [hostileM, wildM] both) `shouldBe` FactionPlayer
        legacyFactionOf (mkUnitFactionProfile Nothing [hostileM, wildM] both)
            `shouldBe` FactionDebug
        legacyFactionOf (mkUnitFactionProfile Nothing [hostileM, wildM]
                            [CapLocalCommandable]) `shouldBe` FactionHostile
        legacyFactionOf (mkUnitFactionProfile Nothing [wildM] [])
            `shouldBe` FactionWildlife
        -- a controller that is not the local one owns nothing locally
        legacyFactionOf (mkUnitFactionProfile (Just (aiController "x")) [] [])
            `shouldBe` FactionNeutral

    it "D-26 keeps authored provenance apart from the mapping's own tags" $ do
        resolveLegacyFaction [tagAcolyte] FactionPlayer `shouldBe`
            mkUnitFactionProfile (Just localController)
                [TagMembership tagAcolyte MemberDefinitionDefault] []
        resolveLegacyFaction [] FactionWildlife `shouldBe`
            mkUnitFactionProfile Nothing
                [TagMembership tagWildlife
                     (MemberRuntimeOwner legacyMappingOwner)] []
        resolveLegacyFaction [tagAcolyte] FactionHostile `shouldBe`
            mkUnitFactionProfile Nothing
                [TagMembership tagLegacyHostile
                     (MemberRuntimeOwner legacyMappingOwner)] []
        resolveLegacyFaction [tagAcolyte] FactionDebug `shouldBe`
            mkUnitFactionProfile Nothing []
                [CapLocalCommandable, CapUnrestrictedCombat]
        resolveLegacyFaction [tagAcolyte] FactionNeutral
            `shouldBe` inertUnitProfile

-- * v1 and v2

legacyWireSpec ∷ Spec
legacyWireSpec = describe "units v1 and v2 migration" $ do
    forM_ [1, 2] $ \ver → forM_ [culturedName, bareName] $ \d →
        it ("v" <> show ver <> ": all five legacy strings on '"
            <> T.unpack d <> "' resolve to the D-26 profile against that \
            \definition's defaults") $
            case loadLegacy ver (legacyRows d) of
                Left err → expectationFailure (T.unpack err)
                Right (um, unknowns) → do
                    unknowns `shouldBe` []
                    let ds = maybe [] udFactionTags (HM.lookup d defs)
                    forM_ (zip [1 ..] allFactions) $ \(n, f) →
                        profileOf um (UnitId n)
                            `shouldBe` Just (resolveLegacyFaction ds f)

    forM_ [1, 2] $ \ver →
        it ("v" <> show ver <> ": unknown strings load inert and are \
            \reported once per distinct value") $
            case loadLegacy ver
                    [ (UnitId 1, culturedName, "made_up")
                    , (UnitId 2, bareName,     "made_up")
                    , (UnitId 3, culturedName, "Player")
                    , (UnitId 4, bareName,     "player") ] of
                Left err → expectationFailure (T.unpack err)
                Right (um, unknowns) → do
                    unknowns `shouldBe` ["Player", "made_up"]
                    forM_ [1, 2, 3] $ \n →
                        profileOf um (UnitId n) `shouldBe` Just inertUnitProfile
                    profileOf um (UnitId 4) `shouldBe`
                        Just (resolveLegacyFaction [] FactionPlayer)

    it "a migrated session re-encodes as concrete v3 profiles only" $
        case loadLegacy 2 (legacyRows culturedName ⧺
                           [(UnitId 9, bareName, "made_up")]) of
            Left err → expectationFailure (T.unpack err)
            Right (um, _) → do
                ccVersion unitsCodec `shouldBe` 3
                let dtos = HM.elems (HM.map toUnitInstanceDTO
                              (usnInstances (toUnitSnapshot pageA um)))
                length dtos `shouldBe` 6
                [ () | d ← dtos, UnitFactionLegacyDTO _ ← [uidFaction d] ]
                    `shouldBe` []
                -- and those bytes come back through the v3 decoder
                -- unchanged.
                let back = decodeUnitsAt 3 $ S.put $ UnitsDTO
                        [PageUnitsDTO pageA (HM.map toUnitInstanceDTO
                            (usnInstances (toUnitSnapshot pageA um)))]
                fmap (fst ∘ restoreSlices ∘ udPages) back
                    `shouldSatisfy` either (const False)
                        (\um' → HM.map uiFaction (umInstances um')
                                  ≡ HM.map uiFaction (umInstances um))

    it "a B1 session's faction strings take the same pending → D-26 \
       \path at the load boundary" $ do
        let mainPage = WorldPageId "main_world"
            snaps = legacySnapshots (legacyRows culturedName
                                     ⧺ [(UnitId 9, bareName, "made_up")])
            oldPage = (minimalWorldPageSaveV90 mainPage)
                { wp90Units = UnitSnapshotV90
                    (HM.map toUnitInstanceDTOv1 snaps) 100 }
        case migrateSessionV90 minimalSaveMetadataV90
                 minimalSaveDataV90 { sd90Worlds = [oldPage] } of
            Left err → expectationFailure (show err)
            Right snap → do
                let us = maybe (UnitSnapshot HM.empty 0) pgsUnits
                               (HM.lookup mainPage (snapPages snap))
                    (um, _, unknowns, _, _) =
                        fromUnitSnapshot mainPage defs emptyInfectionManager
                            emptyEquipmentClassManager emptyItemManager us
                unknowns `shouldBe` ["made_up"]
                forM_ (zip [1 ..] allFactions) $ \(n, f) →
                    profileOf um (UnitId n)
                        `shouldBe` Just (resolveLegacyFaction [tagAcolyte] f)
                profileOf um (UnitId 9) `shouldBe` Just inertUnitProfile

    it "a v2 player and a player spawned afterwards share the one local \
       \controller" $
        case loadLegacy 2 [(UnitId 1, culturedName, "player")] of
            Left err → expectationFailure (T.unpack err)
            Right (um, _) → do
                (ufpController <$> profileOf um (UnitId 1))
                    `shouldBe` Just (Just localController)
                ufpController (resolveSpawnFaction [tagAcolyte]
                                                   (Just FactionPlayer))
                    `shouldBe` Just localController

-- * v3 exactness

exactWireSpec ∷ Spec
exactWireSpec = describe "units v3 exactness" $ do
    it "a profile with a controller, authored and runtime-owned \
       \memberships and a capability survives encode and decode exactly" $ do
        let snap = FactionProfileSnap richProfile
        fmap fromUnitFactionDTO (S.decode (S.encode (toUnitFactionDTO snap)))
            `shouldBe` Right snap
        -- through the whole v3 instance DTO as well
        let inst = (toUnitInstanceSnapshotOf (instOf culturedName richProfile))
        fmap (uisFaction ∘ fromUnitInstanceDTO)
             (S.decode (S.encode (toUnitInstanceDTO inst)))
            `shouldBe` Right snap

    it "set order never distinguishes two profiles, nor their bytes" $ do
        let ms = [ TagMembership tagNomad MemberDefinitionDefault
                 , TagMembership tagAcolyte (MemberRuntimeOwner "a")
                 , TagMembership tagWildlife (MemberRuntimeOwner "b") ]
            cs = [CapUnrestrictedCombat, CapLocalCommandable]
            p1 = mkUnitFactionProfile Nothing ms cs
            p2 = mkUnitFactionProfile Nothing (reverse ms) (reverse cs)
            bytes p = S.encode (FactionProfileSnap p)
        p1 `shouldBe` p2
        bytes p1 `shouldBe` bytes p2

    it "restoration keeps saved provenance and capabilities the \
       \definition no longer declares, and the next capture is identical" $ do
        let um0 = emptyUnitManager
                { umDefs = defs
                , umInstances = HM.fromList
                    [ (UnitId 1, instOf culturedName richProfile)
                    , (UnitId 2, instOf bareName richProfile)
                    , (UnitId 3, instOf culturedName
                        (resolveLegacyFaction [tagAcolyte] FactionPlayer)) ] }
            snap0 = toUnitSnapshot pageA um0
            bytes = S.runPut (S.put (UnitsDTO
                        [PageUnitsDTO pageA
                            (HM.map toUnitInstanceDTO (usnInstances snap0))]))
        case ccDecode unitsCodec 3 bytes of
            Left err → expectationFailure (show err)
            Right (UnitsDTO slices) → do
                let (um, unknowns) = restoreSlices slices
                unknowns `shouldBe` []
                HM.map uiFaction (umInstances um)
                    `shouldBe` HM.map uiFaction (umInstances um0)
                usnInstances (toUnitSnapshot pageA um)
                    `shouldBe` usnInstances snap0

    it "the tracked units v3 fixture (z6) decodes every legacy spawn \
       \source's profile from real stored bytes" $ do
        bytes ← BS.readFile
            "test-headless/data/save-compat/z6-unit-faction-profile.bin"
        let luaNames = HS.fromList ["unit_ai", "building_spawn"]
        case decodeSessionEnvelope luaNames luaNames bytes of
            Left err → expectationFailure (T.unpack err)
            Right (_, snap, _, _) → do
                let factions = HM.unions
                        [ HM.map uisFaction (usnInstances (pgsUnits p))
                        | p ← HM.elems (snapPages snap) ]
                    acolyte = resolveSpawnFaction [tagAcolyte] ∘ Just
                -- the spawns tools/save_compat_audit.py recorded, in id
                -- order (docs/save_compat/manifest.json, z6)
                factions `shouldBe` HM.fromList (zip (map UnitId [1 ..])
                    (map FactionProfileSnap
                        [ acolyte FactionPlayer
                        , acolyte FactionHostile
                        , acolyte FactionDebug
                        , acolyte FactionNeutral
                        , resolveSpawnFaction [tagWildlife] Nothing
                        , resolveSpawnFaction [] Nothing
                        , resolveSpawnFaction [tagAcolyte] Nothing ]))

-- * Spawn ingress

ingressPage ∷ WorldPageId
ingressPage = WorldPageId "profile_wire_ingress"

newBareLuaBackend ∷ EngineEnv → IO LuaBackendState
newBareLuaBackend env = do
    ls ← createLuaBackendState (luaToEngineQueue env) (luaQueue env)
                                (assetPoolRef env) (nextObjectIdRef env)
                                (inputStateRef env) (loggerRef env)
    stateRef ← newIORef ThreadRunning
    registerLuaAPI (lbsLuaState ls) env ls stateRef
    pure ls

evalDebug ∷ LuaBackendState → Text → IO Text
evalDebug ls src =
    T.dropAround (≡ '"') <$> executeDebugLua (lbsLuaState ls) src

loadWith ∷ LuaBackendState → Text → FilePath → IO ()
loadWith ls verb path = void $ evalDebug ls $ T.concat
    [ "engine.", verb, "('", T.pack path, "'); return 'loaded'" ]

-- | The shipped definitions under test, in the startup loader's order
--   (factions before units, #2506).
loadShippedContent ∷ LuaBackendState → IO ()
loadShippedContent ls = do
    items ← sort ⊚ walkFilesWithExtension "data/items" ".yaml"
    forM_ items $ loadWith ls "loadItemYaml" ∘ ("data/items" </>)
    loadWith ls "loadEquipmentYaml" "data/equipment/humanoid.yaml"
    loadWith ls "loadFactionYaml" "data/factions/base.yaml"
    forM_ ["acolyte", "bear_brown", "tiller"] $ \u →
        loadWith ls "loadUnitYaml" ("data/units/" <> u <> ".yaml")

drainUnitQueue ∷ EngineEnv → IO [UnitCommand]
drainUnitQueue env = go []
  where
    go acc = Q.tryReadQueue (unitQueue env) ≫= \case
        Nothing  → pure (reverse acc)
        Just cmd → go (cmd : acc)

-- | The profile @unit.spawn(def, 1, 1, 0, tag, page)@ queued, with the
--   tag omitted for 'Nothing'.
spawnedProfile ∷ EngineEnv → LuaBackendState → Text → Maybe Text
               → IO [UnitFactionProfile]
spawnedProfile env ls def mTag = do
    _ ← drainUnitQueue env
    _ ← evalDebug ls $ T.concat
        [ "unit.spawn('", def, "', 1, 1, 0, "
        , maybe "nil" (\t → "'" <> t <> "'") mTag
        , ", '", unWorldPageId ingressPage, "'); return 'ok'" ]
    cmds ← drainUnitQueue env
    pure [ p | UnitSpawn _ _ _ _ _ p _ _ _ ← cmds ]

ingressSpec ∷ Spec
ingressSpec = describe "spawn ingress" $ aroundAll setup $ do
    it "an omitted tag takes the definition's defaults, falling back to \
       \wildlife only for a definition without any" $ \(env, ls) → do
        spawnedProfile env ls "acolyte" Nothing `shouldReturn`
            [mkUnitFactionProfile Nothing
                [TagMembership tagAcolyte MemberDefinitionDefault] []]
        spawnedProfile env ls "bear_brown" Nothing `shouldReturn`
            [mkUnitFactionProfile Nothing
                [TagMembership tagWildlife MemberDefinitionDefault] []]
        spawnedProfile env ls "tiller" Nothing `shouldReturn`
            [mkUnitFactionProfile Nothing
                [TagMembership tagWildlife
                     (MemberRuntimeOwner legacyMappingOwner)] []]

    it "an omitted tag reads through the adapter as neutral for the \
       \acolyte and wildlife for the bear and tiller" $ \(env, ls) → do
        forM_ [ ("acolyte", FactionNeutral), ("bear_brown", FactionWildlife)
              , ("tiller", FactionWildlife) ] $ \(d, f) →
            map legacyFactionOf ⊚ spawnedProfile env ls d Nothing
                `shouldReturn` [f]

    it "every legacy tag resolves by D-26 against the spawned \
       \definition's defaults" $ \(env, ls) →
        forM_ [ ("acolyte", [tagAcolyte]), ("bear_brown", [tagWildlife])
              , ("tiller", []) ] $ \(d, ds) → forM_ allFactions $ \f →
            spawnedProfile env ls d (Just (factionTag f))
                `shouldReturn` [resolveLegacyFaction ds f]

    it "the five spawn sources keep their legacy answers" $ \(env, ls) →
        forM_ ["acolyte", "bear_brown", "tiller"] $ \d → do
            forM_ [FactionPlayer, FactionHostile, FactionNeutral, FactionDebug]
                $ \f → map legacyFactionOf
                         ⊚ spawnedProfile env ls d (Just (factionTag f))
                         `shouldReturn` [f]

    it "an unrecognized tag still spawns, with the inert profile" $
        \(env, ls) →
            forM_ ["acolyte", "bear_brown", "tiller"] $ \d →
                spawnedProfile env ls d (Just "made_up")
                    `shouldReturn` [inertUnitProfile]
  where
    setup act = withIsolatedResourceRoot $ do
        EngineInitResult env ← initializeEngineHeadlessQuiet
        ls ← newBareLuaBackend env
        loadShippedContent ls
        ws ← emptyWorldState
        writeIORef (worldManagerRef env) emptyWorldManager
            { wmWorlds = [(ingressPage, ws)], wmVisible = [ingressPage] }
        atomicModifyIORef' (unitManagerRef env) $ \um →
            (um { umInstances = HM.empty }, ())
        act (env, ls)
