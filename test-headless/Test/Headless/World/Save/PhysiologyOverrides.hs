{-# LANGUAGE Strict, OverloadedStrings, ScopedTypeVariables #-}
-- | The @physiology override persistence@ gate (#2829, S3 of the
--   physiology stat-modes epic #2827): explicit per-system mode intent
--   (@uiPhysioOverrides@) carried through the live instance, the unit
--   snapshot, the @units@ v4 wire, capture and restore; the frozen v1,
--   v2 and v3 shapes that yield EMPTY overrides; and the load-time
--   rejection policy for decoded physiology state (D-6 cases (d), (e)
--   and (h)).
--
--   Everything runs through the production codec and the real snapshot
--   adapters. Nothing here ticks a physiology step, rolls a generator or
--   builds a registry by hand except where a test says so: the shipped
--   registry is read through the production decoder.
--
--   __S3 carries intent, S4 acts on it.__ This gate proves exact transport
--   and the wire migrations, and the rejections (d), (e) and (h). The
--   activated storage invariants and the deterministic load normalization
--   ((b), (c), (f), (g), (i), (j)) are S4's (#2831), asserted by
--   "Test.Headless.Unit.PhysiologyModes.Persistence".
--
--   Run just this gate: @cabal test synarchy-test-headless
--   --test-options='--match "physiology override persistence"'@.
module Test.Headless.World.Save.PhysiologyOverrides (spec) where

import UPrelude
import Test.Hspec
import qualified Data.ByteString as BS
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import qualified Data.Serialize as S
import qualified Data.Text as T
import qualified Data.Yaml as Yaml
import Data.List (sortOn)
import qualified Data.Text.Encoding as TE
import Engine.Asset.YamlPhysiology
    (PhysiologyYamlDoc, PhysiologyRefusal(..), admitPhysiologyDoc)
import Equipment.Types (emptyEquipmentClassManager)
import Infection.Types (emptyInfectionManager)
import Item.Types (emptyItemManager)
import Test.Headless.Unit.PhysiologyFixture (readShippedPhysiologyRegistry)
import Test.Headless.Unit.TransferApi (minimalDef)
import Test.Headless.World.Save.Components.Fixture
    ( minimalGlobals, minimalPage, minimalSimState, minimalUnitInstance
    , page1, page2, isLeft, minimalSaveMetadataV90, minimalSaveDataV90
    , minimalWorldPageSaveV90 )
import Unit.Physiology.Types
import Unit.Types
    ( UnitDef(..), UnitId(..), UnitInstance(..), UnitManager(..)
    )
import World.Page.Types (WorldPageId(..))
import World.Save.Compat.SessionV90
    (SaveDataV90(..), WorldPageSaveV90(..), UnitSnapshotV90(..)
    , migrateSessionV90)
import World.Save.Component.Entities
import World.Save.Component.Types
    ( ComponentCodec(..), ComponentError(..), ComponentPhase(..) )
import World.Save.Envelope
    ( encodeSessionSnapshot, decodeSessionEnvelope
    , metadataComponentId, currentEnvelopeVersion )
import World.Save.Envelope.Codec (DecodedEnvelope(..), decodeEnvelope)
import World.Save.Envelope.Types
    ( ComponentId(..), ComponentDescriptor(..), EnvelopeManifest(..)
    , defaultEnvelopeLimits )
import World.Save.Component (componentKnownIds, componentRequiredIds)
import World.Save.Physiology
    ( PhysiologyLoadRejection(..), physiologyLoadRejections
    , renderPhysiologyRejection )
import World.Save.Snapshot
import World.Save.Snapshot.Adapter
    (SaveRequestMeta(..), snapshotSaveMetadata)
import World.Save.Types
    ( SaveMetadata, UnitSnapshot(..), UnitInstanceSnapshot(..)
    , fromUnitSnapshot, toUnitSnapshot )

-----------------------------------------------------------------------
-- Fixtures
-----------------------------------------------------------------------

unitWith ∷ Text → [(Text, PhysioMode)] → [(Text, Float)]
         → UnitInstanceSnapshot
unitWith def overrides stats = (minimalUnitInstance [])
    { uisDefName = def
    , uisPhysioOverrides = HM.fromList overrides
    , uisStats = HM.fromList stats }

pageWith ∷ WorldPageId → [(Word32, UnitInstanceSnapshot)] → PageSnapshot
pageWith pid us = (minimalPage pid)
    { pgsUnits = UnitSnapshot
        { usnInstances = HM.fromList [ (UnitId n, u) | (n, u) ← us ]
        , usnNextId = 10 }
    , pgsUnitSimStates = HM.fromList [ (UnitId n, minimalSimState)
                                     | (n, _) ← us ] }

-- | Three units across two pages: unit 1 holds both modes and a present
--   ZERO salt reserve, unit 2 holds nothing and a PARTIAL reserve, unit
--   3 holds one override on the other page.
u1, u2, u3 ∷ UnitInstanceSnapshot
u1 = unitWith "acolyte"
    [("hydration", PhysioSimulated), ("thermo", PhysioNominal)]
    [("salt_store", 0), ("hydration", 21)]
u2 = unitWith "acolyte" [] [("salt_store", 12.5)]
u3 = unitWith "bear_brown" [("salt", PhysioSimulated)] [("salt_store", 140)]

session ∷ SessionSnapshot
session = case captureSessionSnapshot minimalGlobals
        { sgVisiblePages = [page1, page2] }
        [ pageWith page1 [(1, u1), (2, u2)], pageWith page2 [(3, u3)] ] of
    Right s   → s
    Left errs → error ("session invalid: " ⧺ show errs)

meta ∷ SessionSnapshot → SaveMetadata
meta = snapshotSaveMetadata
    (SaveRequestMeta { srmSlotName = "slot", srmTimestamp = "ts"
                     , srmAutosave = False })

luaNames ∷ HS.HashSet T.Text
luaNames = HS.fromList ["unit_ai", "building_spawn"]

luaCid ∷ T.Text → ComponentId
luaCid n = ComponentId ("lua." <> n)

roundTrip ∷ SessionSnapshot → Either String SessionSnapshot
roundTrip snap =
    case decodeSessionEnvelope HS.empty HS.empty
             (encodeSessionSnapshot (meta snap) snap []) of
        Left err → Left (show err)
        Right (_, back, _, _) → Right back

unitsOf ∷ SessionSnapshot → WorldPageId
        → HM.HashMap UnitId UnitInstanceSnapshot
unitsOf snap pid = maybe HM.empty (usnInstances ∘ pgsUnits)
                         (HM.lookup pid (snapPages snap))

overridesOf ∷ SessionSnapshot → WorldPageId
            → [(Word32, [(T.Text, PhysioMode)])]
overridesOf snap pid =
    [ (n, sortOn fst (HM.toList (uisPhysioOverrides u)))
    | (UnitId n, u) ← sortOn (unUnitId ∘ fst) (HM.toList (unitsOf snap pid)) ]

-- | The component's version as the envelope declares it.
unitsVersion ∷ [T.Text] → BS.ByteString → Either String Word32
unitsVersion lua bytes = case decodeEnvelope defaultEnvelopeLimits
        currentEnvelopeVersion knownIds requiredIds bytes of
    Left err → Left (show err)
    Right de → case [ cdVersion d | d ← emComponents (deManifest de)
                                  , cdId d ≡ ComponentId "units" ] of
        (v : _) → Right v
        []      → Left "no units component"
  where
    luaIds = HS.fromList (map luaCid lua)
    knownIds = HS.insert metadataComponentId
                         (HS.union componentKnownIds luaIds)
    requiredIds = HS.union (HS.insert metadataComponentId componentRequiredIds)
                           luaIds

decodeUnits ∷ Word32 → BS.ByteString → Either ComponentError UnitsDTO
decodeUnits = ccDecode unitsCodec

dtoOverrides ∷ UnitsDTO → [HM.HashMap T.Text PhysioMode]
dtoOverrides (UnitsDTO slices) =
    [ uidPhysioOverrides i | s ← slices, i ← HM.elems (puInstances s) ]

-----------------------------------------------------------------------
-- The gate
-----------------------------------------------------------------------

spec ∷ Spec
spec = describe "physiology override persistence" $ do
    v4WireSpec
    liveRestoreSpec
    legacyWireSpec
    fixtureSpec
    beforeAll readShippedPhysiologyRegistry $ do
        rejectionSpec
        declarationsSpec

-----------------------------------------------------------------------
-- Requirement 1: units v4
-----------------------------------------------------------------------

v4WireSpec ∷ Spec
v4WireSpec = describe "the units v4 wire (requirements 1 and 2)" $ do

    it "declares units v4 and accepts every earlier version through its \
       \own decoder" $ do
        ccVersion unitsCodec `shouldBe` 4
        ccInputVers unitsCodec `shouldBe` [1, 2, 3, 4]

    it "emits units v4 in an envelope" $
        unitsVersion [] (encodeSessionSnapshot (meta session) session [])
            `shouldBe` Right 4

    it "round-trips empty and nonempty maps, both modes, several units \
       \and pages, exactly — not merely the effective modes" $
        case roundTrip session of
            Left err → expectationFailure err
            Right back → do
                back `shouldBe` session
                overridesOf back page1 `shouldBe`
                    [ (1, [("hydration", PhysioSimulated)
                          ,("thermo", PhysioNominal)])
                    , (2, []) ]
                overridesOf back page2 `shouldBe`
                    [(3, [("salt", PhysioSimulated)])]
                -- the durable stats ride the same wire untouched: a
                -- PRESENT ZERO and a PARTIAL reserve (#2761), and
                -- nothing was initialized to carry the map
                HM.lookup "salt_store" ∘ uisStats <$>
                    HM.lookup (UnitId 1) (unitsOf back page1)
                    `shouldBe` Just (Just 0)
                HM.lookup "salt_store" ∘ uisStats <$>
                    HM.lookup (UnitId 2) (unitsOf back page1)
                    `shouldBe` Just (Just 12.5)
                fmap uisStats (HM.lookup (UnitId 1) (unitsOf back page1))
                    `shouldBe` Just (HM.fromList
                        [("salt_store", 0), ("hydration", 21)])

    it "carries the map through the DTO conversion, so an override \
       \dropped by either direction is observable" $ do
        let dto = toUnitInstanceDTO u1
        uidPhysioOverrides dto `shouldBe` uisPhysioOverrides u1
        uisPhysioOverrides (fromUnitInstanceDTO dto)
            `shouldBe` uisPhysioOverrides u1
        fromUnitInstanceDTO dto `shouldBe` u1

-----------------------------------------------------------------------
-- Requirement 1: capture and restore through the live instance
-----------------------------------------------------------------------

liveRestoreSpec ∷ Spec
liveRestoreSpec = describe "capture and restore (requirement 1)" $ do

    let defs species = HM.fromList
            [ ("acolyte", (minimalDef "acolyte" "Acolyte")
                  { udPhysiology = plainUnitPhysiology True Nothing species })
            , ("bear_brown", minimalDef "bear_brown" "Bear") ]
        restore page snap defMap = case fromUnitSnapshot page defMap
                emptyInfectionManager emptyEquipmentClassManager
                emptyItemManager snap of
            (um, orphans, _, _, _) → (um, orphans)

    it "restores the stored map onto the live instance and captures it \
       \back unchanged, including an override EQUAL to the species \
       \default" $ do
        -- hydration defaults Simulated, so unit 1's override is the
        -- default restated: still explicit intent, kept exactly.
        let species = HM.fromList [("hydration", PhysioSimulated)]
            snap = UnitSnapshot
                (HM.fromList [(UnitId 1, u1), (UnitId 2, u2)]) 10
            (um, orphans) = restore page1 snap (defs species)
        orphans `shouldBe` []
        fmap uiPhysioOverrides (HM.lookup (UnitId 1) (umInstances um))
            `shouldBe` Just (HM.fromList
                [("hydration", PhysioSimulated), ("thermo", PhysioNominal)])
        -- the absent override stays absent and follows the default
        fmap uiPhysioOverrides (HM.lookup (UnitId 2) (umInstances um))
            `shouldBe` Just HM.empty
        usnInstances (toUnitSnapshot page1 um) `shouldBe` usnInstances snap
        -- restore adds no stat: no pool was seeded to carry the map
        fmap uiStats (HM.lookup (UnitId 1) (umInstances um))
            `shouldBe` Just (uisStats u1)

    it "round-trips live instances across two pages through the real \
       \codec" $ do
        let species = HM.fromList [("salt", PhysioSimulated)]
            snapOf _ us = UnitSnapshot
                (HM.fromList [ (UnitId n, u) | (n, u) ← us ]) 10
            (umA, _) = restore page1 (snapOf page1 [(1, u1), (2, u2)])
                           (defs species)
            (umB, _) = restore page2 (snapOf page2 [(3, u3)]) (defs species)
            page pid um = (minimalPage pid)
                { pgsUnits = toUnitSnapshot pid um
                , pgsUnitSimStates = HM.fromList
                    [ (uid, minimalSimState)
                    | uid ← HM.keys (usnInstances (toUnitSnapshot pid um)) ] }
            captured = captureSessionSnapshot minimalGlobals
                { sgVisiblePages = [page1, page2] }
                [page page1 umA, page page2 umB]
        case captured of
            Left errs → expectationFailure (show errs)
            Right snap → case roundTrip snap of
                Left err → expectationFailure err
                Right back → do
                    back `shouldBe` snap
                    overridesOf back page2
                        `shouldBe` [(3, [("salt", PhysioSimulated)])]

-----------------------------------------------------------------------
-- Requirement 2: the frozen shapes
-----------------------------------------------------------------------

legacyWireSpec ∷ Spec
legacyWireSpec = describe "historical units shapes (requirement 2)" $ do

    let inst = u1 { uisPhysioOverrides = HM.empty }

    it "units v1 decodes through its own frozen graph and yields EMPTY \
       \overrides" $ do
        let bytes = S.encode (UnitsDTOv1
                [ PageUnitsDTOv1 page1
                    (HM.singleton (UnitId 1) (toUnitInstanceDTOv1 inst)) ])
        fmap dtoOverrides (decodeUnits 1 bytes)
            `shouldBe` Right [HM.empty]

    it "units v2 decodes through its own frozen graph and yields EMPTY \
       \overrides" $ do
        let bytes = S.encode (UnitsDTOv2
                [ PageUnitsDTOv2 page1
                    (HM.singleton (UnitId 1) (toUnitInstanceDTOv2 inst)) ])
        fmap dtoOverrides (decodeUnits 2 bytes)
            `shouldBe` Right [HM.empty]

    it "units v3 decodes through the FROZEN v3 graph, yields EMPTY \
       \overrides and loses nothing else" $ do
        let bytes = S.encode (UnitsDTOv3
                [ PageUnitsDTOv3 page1
                    (HM.singleton (UnitId 1) (toUnitInstanceDTOv3 inst)) ])
        fmap dtoOverrides (decodeUnits 3 bytes) `shouldBe` Right [HM.empty]
        case decodeUnits 3 bytes of
            Right (UnitsDTO slices) →
                map fromUnitInstanceDTO
                    [ i | s ← slices, i ← HM.elems (puInstances s) ]
                    `shouldBe` [inst]
            Left err → expectationFailure (show err)

    it "dispatches on the declared version: current bytes are not a \
       \valid v3 payload, nor v3 bytes a valid v4 one" $ do
        let current = S.encode (UnitsDTO
                [ PageUnitsDTO page1
                    (HM.singleton (UnitId 1) (toUnitInstanceDTO u1)) ])
            frozen = S.encode (UnitsDTOv3
                [ PageUnitsDTOv3 page1
                    (HM.singleton (UnitId 1) (toUnitInstanceDTOv3 u1)) ])
        -- v3 bytes are not a v4 payload …
        isLeft (decodeUnits 4 frozen) `shouldBe` True
        -- … and current bytes put under a forged v3 label do NOT carry
        -- the map: the frozen graph has no such field, which is why the
        -- v3 evidence below comes from the pre-v4 codec, not from here.
        fmap dtoOverrides (decodeUnits 3 current) `shouldBe` Right [HM.empty]
        -- and each decodes at its own version
        fmap dtoOverrides (decodeUnits 4 current)
            `shouldBe` Right [uisPhysioOverrides u1]

    it "re-encodes any older units payload as v4" $ do
        let bytes = S.encode (UnitsDTOv3
                [ PageUnitsDTOv3 page1
                    (HM.singleton (UnitId 1) (toUnitInstanceDTOv3 inst)) ])
        case decodeUnits 3 bytes of
            Left err → expectationFailure (show err)
            Right _ → ccVersion unitsCodec `shouldBe` 4

    it "the B1 SessionV90 adapter yields EMPTY overrides" $ do
        let mainPage = WorldPageId "main_world"
            oldPage = (minimalWorldPageSaveV90 mainPage)
                { wp90Units = UnitSnapshotV90
                    (HM.singleton (UnitId 1) (toUnitInstanceDTOv1 inst)) 100 }
        case migrateSessionV90 minimalSaveMetadataV90
                 minimalSaveDataV90 { sd90Worlds = [oldPage] } of
            Left err → expectationFailure (show err)
            Right snap → map (HM.null ∘ uisPhysioOverrides)
                             (HM.elems (unitsOf snap mainPage))
                `shouldBe` [True]

-----------------------------------------------------------------------
-- Requirement 5: the retained fixtures
-----------------------------------------------------------------------

fixtureSpec ∷ Spec
fixtureSpec = describe "retained fixtures (requirement 5)" $ do
    forM_ [ ("z6-unit-faction-profile", 3 ∷ Word32, False)
          , ("z7-units-v3-salt-store", 3, True) ] $ \(name, ver, reserve) →
        it (T.unpack name ⧺ " is a real units-v" ⧺ show ver
            ⧺ " session: it decodes through the frozen v3 graph, yields \
              \empty overrides and re-encodes as v4") $ do
            bytes ← BS.readFile ("test-headless/data/save-compat/"
                                 ⧺ T.unpack name ⧺ ".bin")
            -- the envelope really declares units at the old version, so
            -- this is not a current payload under a forged number
            unitsVersion ["unit_ai", "building_spawn"] bytes
                `shouldBe` Right ver
            case decodeSessionEnvelope luaNames luaNames bytes of
                Left err → expectationFailure (show err)
                Right (md, snap, lua, _) → do
                    let allUnits = concat
                            [ HM.elems (usnInstances (pgsUnits p))
                            | p ← HM.elems (snapPages snap) ]
                    allUnits `shouldSatisfy` (not ∘ null)
                    all (HM.null ∘ uisPhysioOverrides) allUnits
                        `shouldBe` True
                    when reserve $ do
                        let reserves = [ HM.lookup "salt_store" (uisStats u)
                                       | p ← HM.elems (snapPages snap)
                                       , (UnitId n, u) ←
                                           HM.toList (usnInstances (pgsUnits p))
                                       , n ≤ 2 ]
                        reserves `shouldMatchList` [Just 0, Just 12.5]
                    let again = encodeSessionSnapshot md snap lua
                    unitsVersion ["unit_ai", "building_spawn"] again
                        `shouldBe` Right 4
                    -- re-encoding is reproducible, and the v4 bytes decode
                    -- to the same session
                    again `shouldBe` encodeSessionSnapshot md snap lua
                    fmap (\(_, s, _, _) → s)
                         (decodeSessionEnvelope luaNames luaNames again)
                        `shouldBe` Right snap

-----------------------------------------------------------------------
-- Requirement 3: rejection
-----------------------------------------------------------------------

-- | Definitions' physiology for the rejection examples: a bodied
--   species that defaults hydration Simulated, and a bodyless one.
defPhysiology ∷ HM.HashMap T.Text UnitPhysiology
defPhysiology = HM.fromList
    [ ("acolyte", plainUnitPhysiology True Nothing
          (HM.fromList [("hydration", PhysioSimulated)]))
    , ("bear_brown", plainUnitPhysiology True Nothing HM.empty)
    , ("tiller", noUnitPhysiology) ]

rejectionsFor ∷ PhysiologyRegistry → [(Word32, UnitInstanceSnapshot)]
              → [PhysiologyLoadRejection]
rejectionsFor reg us = physiologyLoadRejections reg defPhysiology
    [ (page1, UnitSnapshot
          (HM.fromList [ (UnitId n, u) | (n, u) ← us ]) 10) ]

rejectionSpec ∷ SpecWith PhysiologyRegistry
rejectionSpec = describe "decoded-payload rejection (requirement 3)" $ do

    it "rejects an override naming a system the registry does not \
       \declare, identifying the unit and the system" $ \reg → do
        let bad = unitWith "acolyte" [("ghost", PhysioSimulated)] []
        case rejectionsFor reg [(7, bad)] of
            [r@UndeclaredOverrideSystem{}] → do
                (plrUnit r, plrDef r, plrSystem r)
                    `shouldBe` (7, "acolyte", "ghost")
                let msg = renderPhysiologyRejection r
                forM_ ["unit #7", "acolyte", "ghost", "page1"] $ \w →
                    msg `shouldSatisfy` T.isInfixOf w
            other → expectationFailure ("unexpected: " ⧺ show other)

    it "accepts an override for every declared system, in either mode, \
       \including one equal to the default" $ \reg → do
        let ok = unitWith "acolyte"
                [ ("hydration", PhysioSimulated), ("salt", PhysioNominal)
                , ("thermo", PhysioSimulated), ("nutrition", PhysioNominal) ]
                []
        rejectionsFor reg [(1, ok)] `shouldBe` []

    it "does not mistake an unknown system for a Nominal or Unsupported \
       \one: a bodyless unit's override is still judged by its id" $
        \reg → do
            let bad = unitWith "tiller" [("ghost", PhysioNominal)] []
            map plrSystem (rejectionsFor reg [(1, bad)]) `shouldBe` ["ghost"]

    forM_ [ ("NaN", 0 / 0 ∷ Float), ("+Infinity", 1 / 0)
          , ("-Infinity", -1 / 0) ] $ \(label, bad) →
        it ("rejects a " ⧺ label ⧺ " owned value of a Simulated system, \
            \naming the unit, system and key") $ \reg → do
            let u = unitWith "acolyte" [] [("hydration", bad)]
            case rejectionsFor reg [(4, u)] of
                [r@NonFiniteSimulatedValue{}] → do
                    (plrUnit r, plrSystem r, plrKey r)
                        `shouldBe` (4, "hydration", "hydration")
                    renderPhysiologyRejection r
                        `shouldSatisfy` T.isInfixOf "malformed"
                other → expectationFailure ("unexpected: " ⧺ show other)

    it "rejects a non-finite failure METER of a Simulated system" $ \reg → do
        let u = unitWith "acolyte" [("thermo", PhysioSimulated)]
                    [("hypothermia", 0 / 0)]
        map plrKey (rejectionsFor reg [(1, u)]) `shouldBe` ["hypothermia"]

    it "decides ownership by the RESOLVER, not by key presence: an \
       \override can make a system Simulated, and another can make it \
       \Nominal" $ \reg → do
        -- bear: thermo has no default, so Nominal — NaN is not judged …
        let nominal = unitWith "bear_brown" [] [("core_temp", 0 / 0)]
        rejectionsFor reg [(1, nominal)] `shouldBe` []
        -- … until an override makes it Simulated.
        let simulated = unitWith "bear_brown" [("thermo", PhysioSimulated)]
                            [("core_temp", 0 / 0)]
        map plrSystem (rejectionsFor reg [(1, simulated)])
            `shouldBe` ["thermo"]
        -- acolyte hydration defaults Simulated; a Nominal override frees it
        let freed = unitWith "acolyte" [("hydration", PhysioNominal)]
                        [("hydration", 0 / 0)]
        rejectionsFor reg [(1, freed)] `shouldBe` []

    it "retains FINITE out-of-range values exactly, never judges a \
       \derived key, an unrelated stat, or an Unsupported species, and \
       \adds no clamp" $ \reg → do
        let wild = unitWith "acolyte" [("hydration", PhysioSimulated)]
                [ ("hydration", -5), ("salt", 1e30), ("salt_store", -1)
                , ("hunger", 1e38), ("core_temp", -300) ]
        rejectionsFor reg [(1, wild)] `shouldBe` []
        -- a legacy stored salt_conc is S4's case (i): never rejected
        let legacy = unitWith "acolyte" [("salt", PhysioSimulated)]
                [("salt_conc", 1 / 0), ("salt", 35)]
        rejectionsFor reg [(1, legacy)] `shouldBe` []
        -- an unrelated stat is not this slice's to validate
        let unrelated = unitWith "acolyte" [] [("stamina", 0 / 0)]
        rejectionsFor reg [(1, unrelated)] `shouldBe` []
        -- a bodyless species resolves Unsupported, so nothing is Simulated
        let bodyless = unitWith "tiller" [] [("hydration", 0 / 0)]
        rejectionsFor reg [(1, bodyless)] `shouldBe` []
        -- the survivors really are stored unchanged
        uisStats wild `shouldBe` HM.fromList
            [ ("hydration", -5), ("salt", 1e30), ("salt_store", -1)
            , ("hunger", 1e38), ("core_temp", -300) ]

    it "skips a unit whose definition is not registered (the \
       \missing-definition check reports it)" $ \reg → do
        let orphan = unitWith "vanished" [("ghost", PhysioNominal)]
                         [("hydration", 0 / 0)]
        rejectionsFor reg [(1, orphan)] `shouldBe` []

    it "reports every offender in unit-id order" $ \reg → do
        let a = unitWith "acolyte" [("zeta", PhysioNominal)] []
            b = unitWith "acolyte" [("alpha", PhysioNominal)] []
        map (\r → (plrUnit r, plrSystem r)) (rejectionsFor reg [(9, a), (2, b)])
            `shouldBe` [(2, "alpha"), (9, "zeta")]

    it "fails an unknown PhysioMode tag as a malformed payload, never \
       \reinterpreting it as an existing mode" $ \_ → do
        let dto = toUnitInstanceDTO u3
            bytes = S.encode (UnitsDTO
                [PageUnitsDTO page1 (HM.singleton (UnitId 3) dto)])
            -- the override map is the instance's LAST field, so the
            -- payload ends with the one entry's one-byte mode tag
            forged tag = BS.snoc (BS.init bytes) tag
        BS.last bytes `shouldBe` 1               -- PhysioSimulated
        fmap dtoOverrides (decodeUnits 4 bytes)
            `shouldBe` Right [uisPhysioOverrides u3]
        forM_ [2, 7, 255] $ \tag →
            case decodeUnits 4 (forged tag) of
                Left e → cePhase e `shouldBe` DecodePhase
                Right _ → expectationFailure
                    ("tag " ⧺ show tag ⧺ " decoded as a mode")

-----------------------------------------------------------------------
-- Requirement 3: retired and renamed declarations
-----------------------------------------------------------------------

-- | The shipped registry with one retired and one renamed declaration,
--   built through the production decoder from YAML, so recognition is
--   exercised against what a real file would declare.
withDeclarations ∷ PhysiologyRegistry → PhysiologyRegistry
withDeclarations reg = reg
    { prRetired = ["legacy_water"]
    , prSystems =
        [ if psId s ≡ "hydration" then s { psRenamedFrom = ["thirst"] } else s
        | s ← prSystems reg ] }

declarationsSpec ∷ SpecWith PhysiologyRegistry
declarationsSpec = describe "retired and renamed systems (requirement 3)" $ do

    it "recognizes a declared retired or renamed id: neither rejected \
       \as undeclared nor normalized — restored verbatim" $ \shipped → do
        let reg = withDeclarations shipped
            u = unitWith "acolyte"
                    [ ("legacy_water", PhysioSimulated)
                    , ("thirst", PhysioNominal) ] []
        rejectionsFor reg [(1, u)] `shouldBe` []
        -- … and the very same ids are undeclared against the shipped one
        map plrSystem (rejectionsFor shipped [(1, u)])
            `shouldMatchList` ["legacy_water", "thirst"]
        -- verbatim: the map round-trips exactly through the wire
        let snap = captureSessionSnapshot minimalGlobals
                [pageWith page1 [(1, u)]]
        case snap of
            Left errs → expectationFailure (show errs)
            Right s → fmap (\b → overridesOf b page1) (roundTrip s)
                `shouldBe` Right [(1, [ ("legacy_water", PhysioSimulated)
                                      , ("thirst", PhysioNominal) ])]

    it "still rejects an id that is merely similar to a declared one" $
        \shipped → do
            let reg = withDeclarations shipped
                u = unitWith "acolyte" [("thirsty", PhysioNominal)] []
            map plrSystem (rejectionsFor reg [(1, u)]) `shouldBe` ["thirsty"]

    it "parses retired: and renamed_from: and refuses a declared id that \
       \is also an active system" $ \_ → do
        let doc retired renamed = unlines
                [ "systems:"
                , "  - id: alpha"
                , "    renamed_from: " ⧺ renamed
                , "    owns:"
                , "      - { key: a_pool, healthy: { const: 1.0 } }"
                , "retired: " ⧺ retired ]
            admit txt = case Yaml.decodeEither' (TE.encodeUtf8 (T.pack txt)) of
                Left e → Left (show e)
                Right (d ∷ PhysiologyYamlDoc) →
                    Right (admitPhysiologyDoc "spec.yaml" d)
        case admit (doc "[old_one]" "[older]") of
            Right (Right r) → do
                prRetired r `shouldBe` ["old_one"]
                map psRenamedFrom (prSystems r) `shouldBe` [["older"]]
            other → expectationFailure ("not admitted: " ⧺ show other)
        let refused txt = fmap (either (Left ∘ refusalReason) (const (Right ())))
                               (admit txt)
        refused (doc "[alpha]" "[]") `shouldBe`
            Right (Left "retired physiology system is still active")
        refused (doc "[]" "[alpha]") `shouldBe`
            Right (Left "retired physiology system is still active")
        refused (doc "[dup]" "[dup]") `shouldBe`
            Right (Left "duplicate retired physiology system")
        refused (doc "[\"two words\"]" "[]") `shouldBe`
            Right (Left "malformed retired physiology system id")
