{-# LANGUAGE Strict #-}
-- | "Location container shells" (#2505, epic #1231 PLC-14): the PENDING
--   container shells a placed location spawns as incidental content, and
--   the strict intermediate lifecycle that keeps one addressable until
--   PLC-15 (#2510) realizes it.
--
--   Three layers, because a regression in one is invisible from the
--   other two:
--
--   * 'pureSpec' — placement, the authoring RULE SET, the decode rules,
--     the session-wide provenance graph, the load-time profile check,
--     and the v11→v12 migration. No engine.
--   * 'yamlSpec' — the same authoring rules driven through the REAL
--     @engine.loadLocationYaml@ verb against the live item and
--     loot-profile registries, which is the only layer that can show the
--     loader consults them at all and rejects the whole FILE rather than
--     the offending definition.
--   * 'engineSpec' — the real @world.spawnLocationContainer@ binding and
--     the real @item.pickupGround@ refusal, driven through their own Lua
--     functions against a live 'EngineEnv', the pattern
--     'Test.Headless.World.LocationDiscovery's #917 coverage established.
--   * 'luaSpec' — @scripts/locations.lua@'s incidental dispatch, in a
--     standalone stubbed VM, the pattern
--     'Test.Headless.Location.Stamping' established. That is the only
--     layer that can see "a failed spawn still lets the instance-level
--     contents_spawned marker be written", because the marker is written
--     by the script, once, after the whole pass.
--
--   The load-time profile refusal has no layer here on purpose: it lives
--   in @continueLoad@'s @allMissing@ gate and needs a real envelope, so
--   @tools/location_content_probe.py@'s last phase owns it.
--
--   Run just this gate: @cabal test synarchy-test-headless
--   --test-options='--match "Location container shells"'@.
module Test.Headless.Location.ContainerShells
    ( pureSpec
    , yamlSpec
    , engineSpec
    , luaSpec
    ) where

import UPrelude
import Test.Hspec
import qualified Data.ByteString.Char8 as BS
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import qualified Data.List as L
import qualified Data.Serialize as S
import qualified Data.Text as T
import qualified Data.Map.Strict as Map
import qualified Data.Text.Encoding as TE
import qualified HsLua as Lua
import Data.IORef
    (modifyIORef', newIORef, readIORef, writeIORef, atomicModifyIORef')
import Engine.Asset.Handle (TextureHandle(..))
import Engine.Asset.YamlLocations
    ( LocationYamlContent(..), LocationYamlDef(..), containerContentErrors )
import Control.Exception (finally)
import Engine.Core.Init (EngineInitResult(..))
import Engine.Core.Log
    ( LogBackend(..), LogConfig(..), LogEntry(..), defaultLogConfig
    , initLogger )
import Engine.Core.Capability.Core (toCoreCapability)
import Engine.Core.Capability.ContentRegistries
    (ContentRegistriesCapability(..), toContentRegistriesCapability)
import Engine.Scripting.Lua.API.Locations (loadLocationYamlFn)
import Engine.Scripting.Lua.API (registerLuaAPI)
import Engine.Scripting.Lua.Thread (createLuaBackendState)
import Engine.Scripting.Lua.Types (LuaBackendState(..))
import Engine.Core.Thread (ThreadControl(..))
import LootProfile.Types
    ( LootProfileDef(..), LootProfileEntry(..)
    , emptyLootProfileRegistry, registerLootProfile )
import System.FilePath ((</>))
import Engine.Core.State (EngineEnv(..))
import Engine.Core.Capability.UnitCombat
    (UnitCombatCapability(..), toUnitCombatCapability)
import Engine.Scripting.Lua.API.Items.Ground
    (pickupGroundOnPage, spawnSalvageOnPage, worldSpawnLocationContainerFn)
import Engine.Scripting.Lua.API.WorldQuery.Location
    (worldGetLocationInstanceFn)
import Item.Ground
    (GroundItem(..), GroundItems(..), emptyGroundItems, spawnGroundItem)
import Item.Types
import Language.Semantic.Types (ConceptId(..))
import Location.Bounds (RelBounds(..))
import Location.Instance
import Location.Types
    ( LocationContent(..), LocationDef(..), LocationNaming(..)
    , emptyLocationRegistry, lookupLocation )
import Test.Headless.Harness.GeneratedIds (fixtureGeneratedWorldIdForPage)
import Test.Headless.Harness.Isolation (withExclusiveTempDirectory)
import Test.Headless.Harness.Log (initializeEngineHeadlessQuiet)
import Test.Headless.Location.Bounds (decodeDef)
import Building.Types (BuildingId(..))
import Unit.Direction (Direction(..))
import Unit.Faction (Faction(..))
import Unit.Types
    ( UnitId(..), UnitInstance(..), UnitManager(..), emptyUnitManager )
import World.Chunk.Types (ChunkCoord(..))
import World.Cursor.Types (CursorState(..))
import World.Generate.Types (WorldGenParams(..), defaultWorldGenParams)
import World.Page.Types (WorldPageId(..))
import World.Save.Component.Page
    ( PageCoreDTO(..), PageCoreDTOv11(..), WorldPagesDTO(..)
    , WorldPagesDTOv11(..), WorldPages(..), basePageSnapshots
    , migrateWorldPagesV11, toWorldGenParamsDTO, toWorldGenParamsDTOv8 )
import World.Save.Snapshot.Adapter (SaveRequestMeta(..), snapshotToSaveData)
import World.Save.Types (SaveData(..))
import Test.Headless.World.Save.Integrity
    (buildSnap, minimalBuilding, minimalPage, minimalUnit)
import World.Save.Integrity
    (IntegrityError(..), sessionIntegrityErrors, sessionIntegrityWarnings)
import World.Save.Snapshot (PageSnapshot(..), SessionSnapshot(..))
import World.Save.Types
    ( WorldPageSave(..), UnitSnapshot(..), UnitInstanceSnapshot(..)
    , BuildingSnapshot(..), BuildingInstanceSnapshot(..)
    , MissingContainerProfileRef(..)
    , missingContainerProfileReferences, renderMissingContainerProfileRef )
import World.State.Types
    ( WorldState(..), WorldManager(..), emptyWorldState, emptyWorldManager )
import World.Render.Zoom.Types (ZoomMapMode(..))

-- * Authored fixtures -------------------------------------------------

testNaming ∷ LocationNaming
testNaming = LocationNaming [ConceptId "KEEP"] [ConceptId "ASH"]

-- | One authored content entry, every field named explicitly: the
--   fields of 'LocationContent' are STRICT, so a field added to it fails
--   this fixture to compile rather than riding through on a default.
content ∷ Text → Text → Int → LocationContent
content kind cid count = LocationContent
    { lconKind        = kind
    , lconId          = cid
    , lconCount       = count
    , lconPosition    = Nothing
    , lconFaction     = Nothing
    , lconRolls       = 1
    , lconCountRange  = Nothing
    , lconClearance   = Nothing
    , lconSignificant = False
    , lconProfile     = Nothing
    }

containerContent ∷ Text → Text → Int → LocationContent
containerContent cid profile count =
    (content "container" cid count) { lconProfile = Just profile }

mkDef ∷ Text → [LocationContent] → LocationDef
mkDef lid contents = LocationDef
    { ldId         = lid
    , ldLabel      = "Fixture"
    , ldType       = "ruin"
    , ldBuilder    = "room_small"
    , ldAnchor     = []
    , ldMaxCount   = 0
    , ldMinSpacing = 0
    , ldContents   = contents
    , ldBounds     = RelBounds (-2) (-2) 2 2
    , ldMapIcon    = Nothing
    , ldNaming     = testNaming
    }

-- | The shipped pairing shape: one fixture crate against one fixture
--   profile, beside incidental salvage and a significant obligation —
--   the two so a rule keyed on "every item entry" or on the significant
--   slot numbering would fail here.
crateDef ∷ LocationDef
crateDef = mkDef "fixture_crate_site"
    [ content "loot_table" "ruin_common" 1
    , (content "item" "processing_unit" 1) { lconSignificant = True }
    , containerContent "fixture_crate" "fixture_salvage" 1
    ]

-- | Two container entries, the second with a count of two, so slot
--   numbering across entries is a real outcome rather than a
--   single-entry tautology.
twoEntryDef ∷ LocationDef
twoEntryDef = mkDef "two_entries"
    [ containerContent "fixture_crate" "fixture_salvage" 1
    , content "item" "rations" 1
    , containerContent "fixture_locker" "fixture_tools" 2
    ]

noContainerDef ∷ LocationDef
noContainerDef = mkDef "no_container"
    [ content "loot_table" "ruin_common" 1
    , content "item" "rations" 3
    ]

iid ∷ LocationInstanceId
iid = LocationInstanceId 1

tableFor ∷ LocationDef → LocationInstances
tableFor def =
    let inst = either (error ∘ show) id
            (newLocationInstance Nothing iid (ChunkCoord 0 0) def)
    in emptyLocationInstances
        { lisNextId = unLocationInstanceId iid + 1
        , lisById   = HM.singleton iid inst
        }

instOf ∷ LocationInstances → LocationInstance
instOf = fromMaybe (error "fixture instance missing")
       ∘ lookupLocationInstance iid

-- * Item fixtures -----------------------------------------------------

fixtureDef ∷ Text → ItemDef
fixtureDef name = ItemDef
    { idName = name, idDisplayName = name
    , idTexture = TextureHandle 0, idIconTexture = TextureHandle 0
    , idWeight = 0.4, idWeightSpec = Nothing
    , idBulk = 0.4, idStorage = Nothing, idKind = "misc"
    , idCategory = "Materials", idMake = "", idMaterial = ""
    , idQualitySpec = Nothing, idQualityTiers = []
    , idContainer = Nothing
    , idDefaultContents = [], idFood = Nothing, idWeapon = Nothing
    , idArmor = Nothing, idUnequippable = False, idBuffs = []
    , idInsulation = 0, idSourcePath = "test-fixture"
    }

-- | The fixture crate: a real portable container — internal storage, and
--   ONE authored default content. Design D-22 says a paired container
--   mints through the materializer unchanged, authored defaults
--   included, and "unrolled" means no PROFILE draw rather than an empty
--   tree — so the default is what distinguishes those two outcomes.
crateItemDef ∷ ItemDef
crateItemDef = (fixtureDef "fixture_crate")
    { idWeight  = 6
    , idBulk    = 60
    , idStorage = Just (ItemStorage 40 50)
    , idDefaultContents =
        [ ItemContentEntry
            { iceItem = "packing_straw", iceCount = 1
            , iceFill = Nothing, iceContents = Nothing } ]
    }

containerItemDefs ∷ ItemManager
containerItemDefs = ItemManager $ HM.fromList
    [ ("fixture_crate", crateItemDef)
    , ("packing_straw", fixtureDef "packing_straw")
    , ("rations", fixtureDef "rations")
    ]

-- * Pure spec ---------------------------------------------------------

pureSpec ∷ Spec
pureSpec = describe "Location container shells (#2505)" $ do

    describe "placement derives slots" $ do
        it "creates one slot per container occurrence, in authored order \
           \then count, numbered independently of the significant slots" $ do
            let slots = liContainers (instOf (tableFor twoEntryDef))
            map lcsSlot slots `shouldBe` [1, 2, 3]
            map lcsItemDefName slots
                `shouldBe` ["fixture_crate", "fixture_locker", "fixture_locker"]
            map lcsProfile slots
                `shouldBe` ["fixture_salvage", "fixture_tools", "fixture_tools"]
            -- The whole point of the slice: the descriptor exists with
            -- NO shell bound and NOTHING realized.
            map lcsInstanceId slots `shouldBe` [Nothing, Nothing, Nothing]
            map lcsRealized slots `shouldBe` [False, False, False]

        it "numbers container and significant slots in separate address \
           \spaces, so one instance can carry slot 1 of each" $ do
            let inst = instOf (tableFor crateDef)
            map lsiSlot (liSignificant inst) `shouldBe` [1]
            map lcsSlot (liContainers inst) `shouldBe` [1]

        it "ignores every other content kind" $
            liContainers (instOf (tableFor noContainerDef)) `shouldBe` []

        it "derives no slot from a container entry carrying no profile, \
           \rather than one naming the empty profile" $
            -- Unreachable from authored data (the YAML boundary makes
            -- `profile` mandatory on this kind); the belt-and-braces half
            -- for a def injected some other way. A slot naming "" would
            -- be a shell nothing could ever realize AND a load this build
            -- would then refuse.
            containerSlotsFromDef
                (mkDef "no_profile" [ content "container" "fixture_crate" 2 ])
                `shouldBe` []

        it "does NOT make a container a clearance condition (D-18)" $ do
            let inst = instOf (tableFor (mkDef "only_container"
                            [ containerContent "fixture_crate"
                                               "fixture_salvage" 1 ]))
            -- A location whose only content is a crate authors no
            -- clearance at all, so it never clears — exactly as it
            -- behaved before container content existed.
            locationSignificantCondition inst `shouldBe` Nothing
            locationAuthorsClearance inst `shouldBe` False
            locationClearanceSatisfied inst `shouldBe` False

    describe "the YAML boundary" $ do
        it "accepts a container entry carrying both ids" $
            containerKinds (decodeOrFail containerYaml) `shouldBe` [("fixture_crate", Just "fixture_salvage")]

        it "rejects a container entry with no profile, naming the entry \
           \and its id" $
            decodeDef (yamlWith "    profile: ~\n")
                `shouldSatisfy` rejectedMentioning
                    ["location 'crate_site'", "content entry 1", "fixture_crate"
                    , "requires a 'profile'"]

        it "rejects a profile on a non-container kind, naming the kind" $
            decodeDef (BS.unlines
                [ "id: crate_site"
                , "builder: room_small"
                , "bounds: { min_x: -2, min_y: -2, max_x: 2, max_y: 2 }"
                , "naming: { heads: [KEEP], modifiers: [ASH] }"
                , "contents:"
                , "  - kind: item"
                , "    id: rations"
                , "    profile: fixture_salvage"
                ]) `shouldSatisfy` rejectedMentioning
                    [ "location 'crate_site'", "content entry 1"
                    , "'profile' is supported only for container content"
                    , "not 'item'" ]

        it "rejects 'significant' on a container entry" $
            decodeDef (yamlWith "    significant: true\n")
                `shouldSatisfy` rejectedMentioning
                    [ "location 'crate_site'"
                    , "'significant' is supported only for item content"
                    , "not 'container'" ]

        it "names the unresolved container definition AND the unresolved \
           \profile, one message each" $ do
            let defs = [decodeOrFail containerYaml]
            containerContentErrors (HS.fromList ["fixture_crate"])
                                   (HS.fromList ["fixture_salvage"]) defs
                `shouldBe` []
            containerContentErrors HS.empty HS.empty defs `shouldBe`
                [ "location 'crate_site': container content 'fixture_crate' \
                  \names no registered item definition"
                , "location 'crate_site': container content 'fixture_crate' \
                  \names no registered loot profile 'fixture_salvage'" ]
            -- Each half rejects on its own, so a check that resolved both
            -- ids against ONE registry would fail here.
            containerContentErrors (HS.fromList ["fixture_crate"]) HS.empty defs
                `shouldBe`
                [ "location 'crate_site': container content 'fixture_crate' \
                  \names no registered loot profile 'fixture_salvage'" ]

    describe "decode rules" $ do
        it "rejects a slot below one, a bound id of zero, and a realized \
           \slot naming no shell" $ do
            containerSlotEntryErrors (slotAt 0) `shouldSatisfy`
                any ("below the first valid slot" `T.isInfixOf`)
            containerSlotEntryErrors ((slotAt 1) { lcsInstanceId = Just 0 })
                `shouldSatisfy` any ("no allocator can ever have minted"
                                        `T.isInfixOf`)
            containerSlotEntryErrors ((slotAt 1) { lcsRealized = True })
                `shouldSatisfy` any ("marked realized but names no item"
                                        `T.isInfixOf`)
            containerSlotEntryErrors (slotAt 1) `shouldBe` []

        it "rejects a duplicated slot number on one instance" $
            locationContainerSlotErrors
                (withSlots [slotAt 1, slotAt 1]) `shouldSatisfy`
                    any ("declares container slot 1 more than once"
                            `T.isInfixOf`)

        it "rejects one physical id owned by a container slot AND a \
           \significant obligation on the same page" $ do
            -- Cross-family: item ids come from ONE global allocator, so
            -- the two claims can never be two real items. A walk that
            -- only compared container slots to each other would miss it.
            let table = withSlotsAndObligation
                    [ (slotAt 1) { lcsInstanceId = Just 77 } ]
                    [ LocationSignificantItem 1 "processing_unit"
                                              (Just 77) False ]
            locationContainerSlotErrors table `shouldSatisfy`
                any ("owned by more than one location slot" `T.isInfixOf`)
            -- …and the significant-only walk stays quiet about it, so the
            -- two partition the cases instead of double-reporting.
            locationSignificantItemErrors table `shouldBe` []

        it "stays quiet about two significant obligations sharing an id, \
           \which is the OTHER walk's case" $ do
            let table = withSlotsAndObligation []
                    [ LocationSignificantItem 1 "processing_unit" (Just 77) False
                    , LocationSignificantItem 2 "processing_unit" (Just 77) False ]
            locationContainerSlotErrors table `shouldBe` []
            locationSignificantItemErrors table `shouldSatisfy`
                any ("owned by more than one location obligation"
                        `T.isInfixOf`)

    describe "the binding boundary" $ do
        it "binds an unbound slot once and refuses a second binding" $ do
            let table = tableFor crateDef
                bound = mustBind 1 "fixture_crate" 900 table
            map lcsInstanceId (liContainers (instOf bound))
                `shouldBe` [Just 900]
            registerLocationContainerSpawn iid 1 "fixture_crate" 901 bound
                `shouldBe` Nothing

        it "refuses a shell of the wrong definition" $
            registerLocationContainerSpawn iid 1 "rations" 900
                (tableFor crateDef) `shouldBe` Nothing

        it "refuses a shell whose id is already owed by a SIGNIFICANT \
           \obligation" $ do
            let table = tableFor crateDef
                owed = fromMaybe (error "significant binding refused")
                    (registerLocationSignificantSpawn iid 1 "processing_unit"
                        900 table)
            registerLocationContainerSpawn iid 1 "fixture_crate" 900 owed
                `shouldBe` Nothing

        it "reports an unbound slot and hides a bound one" $ do
            let table = tableFor crateDef
            lcsProfile <$> pendingContainerSlotFor iid 1 table
                `shouldBe` Just "fixture_salvage"
            pendingContainerSlotFor iid 2 table `shouldBe` Nothing
            pendingContainerSlotFor iid 1
                (mustBind 1 "fixture_crate" 900 table) `shouldBe` Nothing

    describe "pending-slot provenance" $ do
        it "accepts a shell lying on its own page's ground" $
            sessionIntegrityErrors (snapshotWith (Just 900) pendingTable)
                `shouldBe` []

        it "hard-fails a shell held in an inventory on its own page" $
            codesOf (sessionIntegrityErrors
                        (inInventory (snapshotWith (Just 900) pendingTable)))
                `shouldBe` ["wrong-scope-reference"]

        it "hard-fails a shell nested inside another ground container, \
           \which no pickup could ever lift as its own ground item" $
            codesOf (sessionIntegrityErrors
                        (nestedOnGround (snapshotWith (Just 900) pendingTable)))
                `shouldBe` ["wrong-scope-reference"]

        it "hard-fails a shell sitting in a BUILDING's storage on its own \
           \page" $
            -- A distinct carrier from the inventory case, and a distinct
            -- one from the nested case: a crate in a cargo hold resolves
            -- through the page's building projection rather than its unit
            -- or ground ones, so a rule that only walked units would pass
            -- it while the shell is provably not on the ground.
            codesOf (sessionIntegrityErrors
                        (inBuildingStorage
                            (snapshotWith (Just 900) pendingTable)))
                `shouldBe` ["wrong-scope-reference"]

        it "hard-fails a shell resolving on a different page" $
            codesOf (sessionIntegrityErrors (otherPage 900 pendingTable))
                `shouldBe` ["wrong-scope-reference"]

        it "hard-fails a bound shell that is the WRONG definition" $
            codesOf (sessionIntegrityErrors
                        (snapshotWithDef "rations" (Just 900) pendingTable))
                `shouldBe` ["wrong-scope-reference"]

        it "hard-fails a duplicate owner across the two slot families" $
            codesOf (sessionIntegrityErrors
                        (snapshotWith (Just 900) crossOwnedTable))
                `shouldSatisfy` elem "duplicate-identity"

        it "hard-fails a bound id at or above the session's item cursor" $
            codesOf (sessionIntegrityErrors
                        ((snapshotWith (Just 900) pendingTable)
                            { snapNextItemId = 900 }))
                `shouldSatisfy` elem "unmintable-identity"

        it "TOLERATES a shell absent from the session, as a warning" $ do
            -- Reachable by ordinary play: item.removeGround DELETES a
            -- ground item rather than moving it, so a scripted removal
            -- of a pending shell leaves exactly this bound-but-absent
            -- slot. The location's contents_spawned is already set, so
            -- nothing re-spawns it and the slot stays pending for ever.
            let snap = removedFromGround
                    (snapshotWith (Just 900) pendingTable)
            sessionIntegrityErrors snap `shouldBe` []
            codesOf (sessionIntegrityWarnings snap)
                `shouldBe` ["dangling-reference"]

        it "applies none of those rules once the slot is REALIZED" $ do
            -- D-3 discards the profile and the source at realization, and
            -- the shell becomes an ordinary item that may be carried,
            -- stored or destroyed. Nothing in THIS slice produces a
            -- realized slot; the rule is pinned so PLC-15 cannot land
            -- against a graph that would refuse its own output.
            let realized = mapSlots (map (\s → s { lcsRealized = True }))
                               pendingTable
            sessionIntegrityErrors
                (inInventory (snapshotWith (Just 900) realized)) `shouldBe` []
            sessionIntegrityWarnings
                (removedFromGround (snapshotWith (Just 900) realized))
                `shouldBe` []

    describe "load-time profile validation" $ do
        it "rejects a load whose UNBOUND pending slot names an \
           \unregistered profile, naming page, instance, slot and id" $ do
            let refs = missingContainerProfileReferences
                           (HS.fromList ["other_profile"]) [pageSave pendingTable]
            refs `shouldBe` [ MissingContainerProfileRef cratePage 1 1
                                "fixture_salvage" ]
            map renderMissingContainerProfileRef refs `shouldSatisfy`
                all (\msg → all (`T.isInfixOf` msg)
                    [ "location #1", "crate_page", "slot 1"
                    , "fixture_salvage" ])

        it "rejects it for a BOUND pending slot too — the divergence from \
           \the significant check" $ do
            let bound = mapSlots (map (\s → s { lcsInstanceId = Just 900 }))
                            pendingTable
            map mcprSlot (missingContainerProfileReferences HS.empty
                              [pageSave bound]) `shouldBe` [1]

        it "accepts a registered profile, and exempts a REALIZED slot \
           \whose stored profile is no longer a reference to anything" $ do
            missingContainerProfileReferences
                (HS.fromList ["fixture_salvage"]) [pageSave pendingTable]
                `shouldBe` []
            let realized = mapSlots
                    (map (\s → s { lcsRealized = True
                                 , lcsInstanceId = Just 900 })) pendingTable
            missingContainerProfileReferences HS.empty [pageSave realized]
                `shouldBe` []

    describe "the v11 wire shape" $ do
        it "migrates a frozen pre-#2505 page with NO container slots" $ do
            let dto = WorldPagesDTOv11 [legacyPageCore]
            case S.decode (S.encode dto) ∷ Either String WorldPagesDTOv11 of
                Left err → expectationFailure err
                Right dto' → do
                    let pages = migrateWorldPagesV11 dto'
                    concatMap liContainers (instancesOfPages pages)
                        `shouldBe` []
                    -- Every stored value still rides across, so the
                    -- absence above is a migration outcome rather than a
                    -- page that failed to arrive.
                    map liDefId (instancesOfPages pages)
                        `shouldBe` ["fixture_crate_site"]
                    wpIdsFromPayload pages `shouldBe` True

        it "round-trips a POPULATED slot through the CURRENT shape, so \
           \the absence above is a real decode outcome" $ do
            let bound = mapSlots (map (\s → s { lcsInstanceId = Just 900 }))
                            pendingTable
                dto = WorldPagesDTO
                    [ currentPageCore { pcGenParams =
                          toWorldGenParamsDTO (paramsWith bound) } ]
            case S.decode (S.encode dto) ∷ Either String WorldPagesDTO of
                Left err → expectationFailure err
                Right dto' → do
                    let slots = concatMap liContainers
                            (instancesOfPages (basePageSnapshots dto'))
                    map lcsSlot slots `shouldBe` [1]
                    map lcsItemDefName slots `shouldBe` ["fixture_crate"]
                    map lcsProfile slots `shouldBe` ["fixture_salvage"]
                    map lcsInstanceId slots `shouldBe` [Just 900]
                    map lcsRealized slots `shouldBe` [False]

        it "keeps an instance that has SPAWNED its contents but still \
           \carries an unbound slot, which decode must not reject" $ do
            -- D-18's incidental lifecycle in one shape: a failed
            -- container spawn logs and continues, so the marker is
            -- written with the slot still empty. The #917 obligation rule
            -- ("contents spawned ⇒ every slot bound") must NOT have been
            -- copied across.
            let table = mapInstance (\i → i { liContentsSpawned = True })
                            pendingTable
            locationContainerSlotErrors table `shouldBe` []

-- * Engine spec -------------------------------------------------------

engineSpec ∷ Spec
engineSpec = beforeAll initEnv $
    describe "Location container shells (#2505) — the spawn boundary" $ do

    it "spawns exactly ONE shell carrying the definition's authored \
       \contents and no rolled cargo, and binds it synchronously" $ \env → do
        let pageId = WorldPageId "shell_spawn"
        ws ← newContainerPage env pageId pendingTable
        -- No world thread runs in this suite, so a QUEUED binding would
        -- still be unapplied when the verb returned — which is the state
        -- a racing pickup would find.
        boundIds ws `shouldReturn` [Nothing]
        spawnContainer env pageId 1 1 (8, 8) `shouldReturn` True
        bound ← boundIds ws
        bound `shouldSatisfy` all isJust
        gis ← readIORef (wsGroundItemsRef ws)
        case HM.elems (gisItems gis) of
            [gi] → do
                iiDefName (giInst gi) `shouldBe` "fixture_crate"
                -- D-22: the shell mints through the materializer
                -- unchanged, authored defaults included. "Unrolled"
                -- means no PROFILE draw, not an empty tree — so the one
                -- authored child must be here and nothing else.
                map iiDefName (iiContents (giInst gi))
                    `shouldBe` ["packing_straw"]
                iiStorage (giInst gi) `shouldBe` Just (ItemStorage 40 50)
                -- …and the bound id is THIS item, never one looked back
                -- up off the ground map.
                bound `shouldBe` [Just (iiInstanceId (giInst gi))]
            other → expectationFailure
                ("expected exactly one ground item, got " <> show (length other))

    it "refuses a slot that is already bound, and spawns nothing for it" $
       \env → do
        let pageId = WorldPageId "shell_bound"
        ws ← newContainerPage env pageId pendingTable
        spawnContainer env pageId 1 1 (8, 8) `shouldReturn` True
        before ← groundCount ws
        spawnContainer env pageId 1 1 (8, 8) `shouldReturn` False
        groundCount ws `shouldReturn` before

    it "refuses an unknown page, instance, slot and definition, spawning \
       \nothing in any of them" $ \env → do
        let pageId = WorldPageId "shell_refusals"
        ws ← newContainerPage env pageId pendingTable
        spawnContainer env (WorldPageId "no_such_page") 1 1 (8, 8)
            `shouldReturn` False
        spawnContainer env pageId 99 1 (8, 8) `shouldReturn` False
        spawnContainer env pageId 1 7 (8, 8) `shouldReturn` False
        -- A slot whose stored definition is no longer registered: the
        -- lookup fails before anything is materialized.
        writeIORef (itemManagerRef env) (ItemManager HM.empty)
        spawnContainer env pageId 1 1 (8, 8) `shouldReturn` False
        writeIORef (itemManagerRef env) containerItemDefs
        groundCount ws `shouldReturn` 0
        boundIds ws `shouldReturn` [Nothing]

    it "refuses non-finite coordinates BEFORE allocating an id or \
       \drawing anything" $ \env → do
        let pageId = WorldPageId "shell_coords"
        ws ← newContainerPage env pageId pendingTable
        before ← readIORef (nextItemInstanceIdRef env)
        -- The shared stat RNG is snapshotted too, not just the two
        -- counters: 'spawnSalvageOnPage' draws quality and condition
        -- from it before anything is allocated, so a coordinate check
        -- that ran after those rolls would leave the counters untouched
        -- while still having consumed the draw — and every later roll in
        -- the session would come out different.
        rngBefore ← show <$> readIORef (ucStatRNGRef (toUnitCombatCapability env))
        forM_ [ (0 / 0, 8), (1 / 0, 8), (8, -(1 / 0)), (8, 1e39) ] $
            \(x, y) → spawnContainer env pageId 1 1 (x, y)
                          `shouldReturn` False
        groundCount ws `shouldReturn` 0
        boundIds ws `shouldReturn` [Nothing]
        -- Neither the item-instance counter nor the page's ground-id
        -- allocator moved, which is what "consumes no allocation or
        -- random draw before coordinate validation" cashes out to.
        readIORef (nextItemInstanceIdRef env) `shouldReturn` before
        gis ← readIORef (wsGroundItemsRef ws)
        gisNextId gis `shouldBe` 0
        rngAfter ← show <$> readIORef (ucStatRNGRef (toUnitCombatCapability env))
        rngAfter `shouldBe` rngBefore

    it "leaves the stat RNG ADVANCED after a spawn that really happens, \
       \so the untouched generator above is a real refusal" $ \env → do
        -- The control for the snapshot above. Without it, an assertion
        -- that the RNG is unchanged would also pass against a verb that
        -- never drew from it at all.
        let pageId = WorldPageId "shell_coords_control"
        _ ← newContainerPage env pageId pendingTable
        rngBefore ← show <$> readIORef (ucStatRNGRef (toUnitCombatCapability env))
        spawnContainer env pageId 1 1 (8, 8) `shouldReturn` True
        rngAfter ← show <$> readIORef (ucStatRNGRef (toUnitCombatCapability env))
        rngAfter `shouldNotBe` rngBefore

    it "answers Nothing and spawns nothing when MATERIALIZATION fails, \
       \leaving the ground and its allocator untouched" $ \env → do
        -- Driven at 'spawnSalvageOnPage', the production core the verb
        -- composes, rather than through the verb: the verb resolves the
        -- slot's definition against the same live registry
        -- 'materializeItem' then resolves the NAME against, so from Lua
        -- the two agree by construction and this branch is unreachable.
        -- It is reachable HERE because the helper takes the def and the
        -- name separately — which is also the shape a deregistration
        -- landing between the verb's two reads would produce.
        let pageId = WorldPageId "shell_materialize"
        ws ← newContainerPage env pageId pendingTable
        before ← readIORef (nextItemInstanceIdRef env)
        spawned ← spawnSalvageOnPage env ws crateItemDef "no_such_crate"
                      8 8 Nothing Nothing Nothing Nothing
        isJust spawned `shouldBe` False
        groundCount ws `shouldReturn` 0
        gis ← readIORef (wsGroundItemsRef ws)
        gisNextId gis `shouldBe` 0
        -- The two salvage rolls happen before the materialize, so the
        -- RNG legitimately moved; the ITEM-INSTANCE counter must not
        -- have, because no instance was ever built.
        readIORef (nextItemInstanceIdRef env) `shouldReturn` before

    it "still spawns through that same core when the name DOES resolve, \
       \so the failure above is the materializer's and not the call's" $
       \env → do
        let pageId = WorldPageId "shell_materialize_control"
        ws ← newContainerPage env pageId pendingTable
        spawned ← spawnSalvageOnPage env ws crateItemDef "fixture_crate"
                      8 8 Nothing Nothing Nothing Nothing
        case spawned of
            Nothing → expectationFailure "the control spawn was refused"
            Just (_, inst) → do
                iiDefName inst `shouldBe` "fixture_crate"
                groundCount ws `shouldReturn` 1

    it "removes the just-spawned shell when the binding fails" $ \env → do
        -- The binding is refused because the id this spawn is ABOUT to
        -- mint is already owed by a significant obligation — a real
        -- cross-family collision rather than a mocked failure. The
        -- colliding id is read off the live allocator rather than
        -- hardcoded: this suite shares one engine, so the next id
        -- depends on how many items the examples before it minted.
        let pageId = WorldPageId "shell_bind_fail"
        shellId ← nextShellId env
        ws ← newContainerPage env pageId (poisonedTable shellId)
        spawnContainer env pageId 1 1 (8, 8) `shouldReturn` False
        -- The shell WAS minted (the collision is only visible at the
        -- binding), so this is the rollback, not a refusal before the
        -- spawn: nothing may be left lying on the ground.
        groundCount ws `shouldReturn` 0
        boundIds ws `shouldReturn` [Nothing]

    it "REFUSES an ordinary pickup of a bound pending shell, without \
       \touching the ground, the inventory, the cursor or the slot" $
       \env → do
        let pageId = WorldPageId "shell_pickup"
        ws ← newContainerPage env pageId pendingTable
        writeIORef (unitManagerRef env) $ emptyUnitManager
            { umInstances = HM.singleton (UnitId 611)
                (containerUnit pageId) }
        spawnContainer env pageId 1 1 (8, 8) `shouldReturn` True
        gid ← onlyGroundId ws
        atomicModifyIORef' (wsCursorRef ws) $ \cs →
            (cs { selectedGroundItem = Just gid }, ())
        before ← readIORef (wsGroundItemsRef ws)
        beforeBound ← boundIds ws
        pickupGroundOnPage env ws (UnitId 611) gid `shouldReturn` False
        after ← readIORef (wsGroundItemsRef ws)
        gisNextId after `shouldBe` gisNextId before
        HM.keys (gisItems after) `shouldBe` HM.keys (gisItems before)
        map (iiInstanceId ∘ giInst) (HM.elems (gisItems after))
            `shouldBe` map (iiInstanceId ∘ giInst)
                           (HM.elems (gisItems before))
        inventoryOf env (UnitId 611) `shouldReturn` []
        cs ← readIORef (wsCursorRef ws)
        selectedGroundItem cs `shouldBe` Just gid
        boundIds ws `shouldReturn` beforeBound

    it "still permits an ordinary pickup of ordinary salvage lying \
       \beside it, so the refusal is keyed on the SLOT" $ \env → do
        let pageId = WorldPageId "shell_pickup_other"
        ws ← newContainerPage env pageId pendingTable
        writeIORef (unitManagerRef env) $ emptyUnitManager
            { umInstances = HM.singleton (UnitId 612)
                (containerUnit pageId) }
        spawnContainer env pageId 1 1 (8, 8) `shouldReturn` True
        decoy ← atomicModifyIORef' (wsGroundItemsRef ws)
            (spawnGroundItem (plainInstance 4242 "rations") 9 9)
        pickupGroundOnPage env ws (UnitId 612) decoy `shouldReturn` True
        map iiDefName <$> inventoryOf env (UnitId 612)
            `shouldReturn` ["rations"]

    it "reports the slot through world.getLocationInstance, and omits \
       \the array entirely for an instance that has none" $ \env → do
        let pageId = WorldPageId "shell_query"
        _ ← newContainerPage env pageId pendingTable
        spawnContainer env pageId 1 1 (8, 8) `shouldReturn` True
        readContainers env pageId 1 `shouldReturn` Just
            "1|fixture_crate|fixture_salvage|false|bound"
        -- An instance with no container slots omits the key, so
        -- `entry.containers` reads as nil rather than as an empty table.
        _ ← newContainerPage env (WorldPageId "shell_query_none")
                (tableFor noContainerDef)
        readContainers env (WorldPageId "shell_query_none") 1
            `shouldReturn` Nothing

-- * Engine helpers ----------------------------------------------------

initEnv ∷ IO EngineEnv
initEnv = do
    EngineInitResult env ← initializeEngineHeadlessQuiet
    writeIORef (itemManagerRef env) containerItemDefs
    pure env

-- | A fresh page carrying @table@ as its only location instance, made
--   the manager's sole world. Each example gets its own page id, so one
--   example's ground and slots can never be read by the next.
newContainerPage
    ∷ EngineEnv → WorldPageId → LocationInstances → IO WorldState
newContainerPage env pageId table = do
    writeIORef (itemManagerRef env) containerItemDefs
    ws ← emptyWorldState
    writeIORef (wsGenParamsRef ws) (Just (paramsWith table))
    writeIORef (worldManagerRef env) $ emptyWorldManager
        { wmWorlds = [(pageId, ws)], wmVisible = [pageId] }
    pure ws

-- | Call @world.spawnLocationContainer(instanceId, slot, x, y, pageId)@
--   through the real Lua binding and answer what it handed back.
--
--   Driven through the binding rather than through a Haskell helper for
--   the same reason #917's own coverage is: this is the ONE call that
--   spawns AND binds, and only the real verb can show that the binding
--   is applied by the time it RETURNS — which is what closes the window
--   a racing pickup would otherwise find.
spawnContainer
    ∷ EngineEnv → WorldPageId → Int → Int → (Float, Float) → IO Bool
spawnContainer env (WorldPageId page) instanceId slot (x, y) = Lua.run $ do
    Lua.openlibs
    Lua.pushinteger (fromIntegral instanceId)
    Lua.pushinteger (fromIntegral slot)
    Lua.pushnumber (realToFrac x)
    Lua.pushnumber (realToFrac y)
    Lua.pushstring (TE.encodeUtf8 page)
    _ ← worldSpawnLocationContainerFn env
    Lua.toboolean Lua.top

-- | The 'iiInstanceId' the NEXT shell this fixture mints will carry.
--
--   Measured rather than predicted: 'Item.Materialize.materializeItem'
--   draws ids for a container's authored default contents as well as for
--   the container itself, and this suite shares one engine, so neither
--   the allocator's current value nor a hardcoded constant names the
--   root's id. A throwaway page mints one shell to learn the offset from
--   the allocator, and the same offset then names the id the next
--   identical mint will take.
nextShellId ∷ EngineEnv → IO Word64
nextShellId env = do
    let probePage = WorldPageId "shell_id_probe"
    probeWs ← newContainerPage env probePage pendingTable
    before ← readIORef (nextItemInstanceIdRef env)
    spawned ← spawnContainer env probePage 1 1 (8, 8)
    unless spawned (error "nextShellId: the probe spawn was refused")
    gis ← readIORef (wsGroundItemsRef probeWs)
    offset ← case map (iiInstanceId ∘ giInst) (HM.elems (gisItems gis)) of
        [rootId] → pure (rootId - before)
        other → error ("nextShellId: expected one shell, got "
                          <> show (length other))
    (+ offset) <$> readIORef (nextItemInstanceIdRef env)

-- | Every container slot's bound shell id on a page, in instance order.
boundIds ∷ WorldState → IO [Maybe Word64]
boundIds ws = do
    mp ← readIORef (wsGenParamsRef ws)
    pure [ lcsInstanceId slot
         | p ← maybeToList mp
         , inst ← instancesToList (wgpLocationInstances p)
         , slot ← liContainers inst ]

groundCount ∷ WorldState → IO Int
groundCount ws = HM.size ∘ gisItems <$> readIORef (wsGroundItemsRef ws)

-- | The one ground item on a page. Fails loudly rather than silently
--   picking one of several.
onlyGroundId ∷ WorldState → IO Int
onlyGroundId ws = do
    gis ← readIORef (wsGroundItemsRef ws)
    case HM.keys (gisItems gis) of
        [gid] → pure gid
        other → error ("expected exactly one ground item, got "
                          <> show (length other))

inventoryOf ∷ EngineEnv → UnitId → IO [ItemInstance]
inventoryOf env uid = do
    um ← readIORef (unitManagerRef env)
    pure (maybe [] uiInventory (HM.lookup uid (umInstances um)))

-- | The container array 'worldGetLocationInstanceFn' pushes, rendered
--   through the REAL registered Lua function into one comparable
--   string — or 'Nothing' when the field is absent entirely, which is
--   the distinction the query's omission contract turns on.
readContainers ∷ EngineEnv → WorldPageId → Int → IO (Maybe Text)
readContainers env (WorldPageId page) instanceId = Lua.run $ do
    Lua.openlibs
    Lua.pushHaskellFunction (worldGetLocationInstanceFn env)
    Lua.setglobal "realLocation"
    status ← Lua.dostring $ TE.encodeUtf8 $ T.unlines
        [ "local inst = realLocation(" <> tshow instanceId
            <> ", '" <> page <> "')"
        , "assert(inst, 'no such instance')"
        , "if inst.containers == nil then return nil end"
        , "local out = {}"
        , "for _, c in ipairs(inst.containers) do"
        , "  out[#out + 1] = table.concat({ c.slot, c.item, c.profile,"
        , "    tostring(c.realized),"
        , "    c.item_instance_id and 'bound' or 'unbound' }, '|')"
        , "end"
        , "return table.concat(out, ';')"
        ]
    case status of
        Lua.OK → do
            isNil ← Lua.isnil Lua.top
            if isNil then pure Nothing
                     else fmap TE.decodeUtf8Lenient <$> Lua.tostring Lua.top
        _ → do
            err ← Lua.tostring Lua.top
            error ("readContainers failed: "
                      <> maybe "<no message>" show err)

-- | A player unit standing on the shell's tile, so a pickup that was
--   going to be permitted would be. Every field is named: 'UnitInstance'
--   is strict, and a default would let a field added to it ride through
--   a fixture whose whole job is to be an ordinary pickup candidate.
containerUnit ∷ WorldPageId → UnitInstance
containerUnit page = UnitInstance
    { uiDefName = "test", uiName = "", uiPage = page
    , uiTexture = TextureHandle 0, uiDirSprites = Map.empty
    , uiBaseWidth = 0, uiGridX = 8, uiGridY = 8, uiGridZ = 5
    , uiRealZ = 5, uiFacing = DirS
    , uiCurrentAnim = "", uiAnimStart = 0, uiAnimReverse = False
    , uiActivity = "idle", uiPose = "standing", uiAnimStride = 1
    , uiStats = HM.empty, uiModifiers = HM.empty, uiSkills = HM.empty
    , uiKnowledge = HM.empty, uiInventory = [], uiEquipment = HM.empty
    , uiAccessories = [], uiFactionId = FactionPlayer, uiWounds = []
    , uiScars = [], uiImmuneResponse = 0, uiImmunities = HM.empty
    , uiBlood = 5.0, uiLastAttackerUid = Nothing, uiLastAttackerAt = 0
    , uiAnimOverride = "", uiFrozen = False, uiForceLoop = False
    , uiClimbDest = Nothing
    , uiTrailState = Nothing
    }

-- * The YAML boundary, through the real registered verb --------------

-- | The authoring rules, driven through @engine.loadLocationYaml@ rather
--   than through 'containerContentErrors' directly.
--
--   The pure spec above pins WHAT the rule set says; this pins that the
--   production loader actually consults it, against the LIVE item and
--   loot-profile registries, and rejects the whole FILE rather than the
--   offending definition. Neither half is visible from a direct call:
--   the loader could resolve the container id against the wrong registry,
--   skip the check entirely, or register the good definitions in a file
--   it then reports as rejected, and every example in 'pureSpec' would
--   still be green.
yamlSpec ∷ Spec
yamlSpec = beforeAll initYamlEnv $
    describe "Location container shells (#2505) — the YAML boundary" $ do

    it "registers a valid container definition, with both ids reaching \
       \the registered def" $ \fx → withCleanLocationRegistry fx $ do
        fst <$> loadLocationFile fx validLocationYaml `shouldReturn` Just 1
        contentsOf fx "crate_site" `shouldReturn`
            [("container", "fixture_crate", Just "fixture_salvage")]

    it "rejects the whole file when the container names an UNREGISTERED \
       \item definition, and registers nothing" $ \fx →
        withCleanLocationRegistry fx $ do
            (count, logged) ← loadLocationFile fx
                (locationYamlNaming "no_such_crate" "fixture_salvage")
            count `shouldBe` Just 0
            contentsOf fx "crate_site" `shouldReturn` []
            logged `shouldSatisfy` mentioning
                [ "crate_site", "no_such_crate"
                , "no registered item definition" ]

    it "rejects the whole file when the container names an UNREGISTERED \
       \loot profile, and registers nothing" $ \fx →
        withCleanLocationRegistry fx $ do
            (count, logged) ← loadLocationFile fx
                (locationYamlNaming "fixture_crate" "no_such_profile")
            count `shouldBe` Just 0
            contentsOf fx "crate_site" `shouldReturn` []
            logged `shouldSatisfy` mentioning
                [ "crate_site", "no_such_profile"
                , "no registered loot profile" ]

    it "is ALL-OR-NOTHING: one bad definition takes the file's good ones \
       \down with it" $ \fx → withCleanLocationRegistry fx $ do
        -- The rejection is reported for the whole file, so a loader that
        -- skipped the offending def and registered the rest would leave
        -- a world half-populated from a file its author believes failed.
        fst <$> loadLocationFile fx twoDefLocationYaml `shouldReturn` Just 0
        contentsOf fx "crate_site" `shouldReturn` []
        contentsOf fx "good_site" `shouldReturn` []

    it "rejects a container entry with no profile at the DECODER, before \
       \either registry is consulted" $ \fx →
        withCleanLocationRegistry fx $ do
            -- A different rejection path from the two above: this one
            -- fails the file's decode, so the verb reports it did not
            -- PARSE rather than reporting a zero-count registration.
            fst <$> loadLocationFile fx noProfileLocationYaml
                `shouldReturn` Nothing
            contentsOf fx "crate_site" `shouldReturn` []

-- | An engine plus a bare registered Lua backend, so the location loader
--   can be called exactly as @engine.loadLocationYaml@ calls it.
initYamlEnv ∷ IO YamlFixture
initYamlEnv = do
    EngineInitResult env ← initializeEngineHeadlessQuiet
    ls ← createLuaBackendState (luaToEngineQueue env) (luaQueue env)
             (assetPoolRef env) (nextObjectIdRef env)
             (inputStateRef env) (loggerRef env)
    stateRef ← newIORef ThreadRunning
    registerLuaAPI (lbsLuaState ls) env ls stateRef
    let regs = toContentRegistriesCapability env
    -- The two registries the container check resolves against, populated
    -- the way boot populates them: items, then loot profiles, then
    -- locations.
    writeIORef (crItemManagerRef regs) containerItemDefs
    writeIORef (crLootProfileRegistryRef regs)
        (registerLootProfile fixtureProfile emptyLootProfileRegistry)
    pure (YamlFixture env ls)

data YamlFixture = YamlFixture
    { yfEnv     ∷ EngineEnv
    , yfBackend ∷ LuaBackendState
    }

-- | The fixture profile the valid file names. Minimal but real: one
--   entry against a registered item, which is all the container check
--   asks of it (nothing here ever DRAWS from it).
fixtureProfile ∷ LootProfileDef
fixtureProfile = LootProfileDef
    { lpdId            = "fixture_salvage"
    , lpdMultiplierMin = 1
    , lpdMultiplierMax = 2
    , lpdEntries       = [LootProfileEntry "rations" 0.5 1]
    }

-- | Run @action@ with the live location registry EMPTY, restoring
--   whatever it held. The ref is shared with every other spec riding
--   this engine, so it is borrowed rather than reassigned — the pattern
--   'Test.Headless.Location.LootDeterminism' established.
withCleanLocationRegistry ∷ YamlFixture → IO a → IO a
withCleanLocationRegistry fx action = do
    let ref = crLocationDefsRef (toContentRegistriesCapability (yfEnv fx))
    before ← readIORef ref
    writeIORef ref emptyLocationRegistry
    action `finally` writeIORef ref before

-- | One @engine.loadLocationYaml(path)@ call over a temporary file
--   holding @src@, through the REAL registered verb.
--
--   Answers the verb's own two-value reply reduced to what the examples
--   care about: 'Nothing' when the file did not PARSE, @Just n@ when it
--   decoded and registered @n@ definitions. A file rejected by the
--   registry checks decoded fine, so it is @Just 0@ — which is exactly
--   the distinction #2203 introduced and a bare boolean would lose.
--   It also answers everything the loader LOGGED, captured off the
--   engine's own logger ref for the duration of the call, so an example
--   can assert the rejection still names which id it refused — the half
--   of the diagnostic a count can never show.
loadLocationFile ∷ YamlFixture → Text → IO (Maybe Int, [Text])
loadLocationFile fx src =
    withExclusiveTempDirectory "container-yaml" $ \dir → do
        let path = dir </> "locations.yaml"
        writeFile path (T.unpack src)
        withCapturedLog fx $ Lua.run $ do
            Lua.openlibs
            Lua.pushstring (TE.encodeUtf8 (T.pack path))
            -- The parse outcome is OPT-IN (#2203): a truthy SECOND
            -- argument is what makes the verb answer two values instead
            -- of the bare count every other caller reads. This is the
            -- form scripts/startup_loader.lua uses, and it is the only
            -- one that can tell a file that did not DECODE from one that
            -- decoded and was then refused by the registry checks.
            Lua.pushboolean True
            _ ← loadLocationYamlFn (toCoreCapability (yfEnv fx))
                     (toContentRegistriesCapability (yfEnv fx))
                     (yfEnv fx) (yfBackend fx)
            parsed ← Lua.toboolean Lua.top
            count  ← Lua.tointeger (Lua.nth 2)
            pure (if parsed then Just (maybe 0 fromIntegral count) else Nothing)

-- | Run @action@ with the engine's logger swapped for a capturing one,
--   answering its result beside every line it produced. The ref is
--   BORROWED and restored, like every other live ref this module reaches.
withCapturedLog ∷ YamlFixture → IO a → IO (a, [Text])
withCapturedLog fx action = do
    entriesRef ← newIORef []
    capturing ← initLogger defaultLogConfig
        { lcBackend = LogToCallback (\e → modifyIORef' entriesRef (e :)) }
    before ← readIORef (loggerRef (yfEnv fx))
    result ← (writeIORef (loggerRef (yfEnv fx)) capturing >> action)
                 `finally` writeIORef (loggerRef (yfEnv fx)) before
    entries ← readIORef entriesRef
    pure (result, reverse (map leMessage entries))

mentioning ∷ [Text] → [Text] → Bool
mentioning fragments =
    any (\line → all (`T.isInfixOf` line) fragments)

-- | The @(kind, id, profile)@ of every content entry the LIVE registry
--   holds for @lid@ — empty when no such definition registered.
contentsOf ∷ YamlFixture → Text → IO [(Text, Text, Maybe Text)]
contentsOf fx lid = do
    reg ← readIORef (crLocationDefsRef (toContentRegistriesCapability (yfEnv fx)))
    pure [ (lconKind c, lconId c, lconProfile c)
         | Just def ← [lookupLocation lid reg], c ← ldContents def ]

validLocationYaml ∷ Text
validLocationYaml = locationYamlNaming "fixture_crate" "fixture_salvage"

locationYamlNaming ∷ Text → Text → Text
locationYamlNaming itemId profileId = T.unlines
    [ "locations:"
    , "  - id: crate_site"
    , "    builder: room_small"
    , "    bounds: { min_x: -2, min_y: -2, max_x: 2, max_y: 2 }"
    , "    naming: { heads: [KEEP], modifiers: [ASH] }"
    , "    contents:"
    , "      - { kind: container, id: " <> itemId
        <> ", profile: " <> profileId <> ", count: 1 }"
    ]

-- | One file, two definitions: the second names an unknown profile. The
--   FIRST is perfectly valid and must still not register.
twoDefLocationYaml ∷ Text
twoDefLocationYaml = T.unlines
    [ "locations:"
    , "  - id: good_site"
    , "    builder: room_small"
    , "    bounds: { min_x: -2, min_y: -2, max_x: 2, max_y: 2 }"
    , "    naming: { heads: [KEEP], modifiers: [ASH] }"
    , "    contents:"
    , "      - { kind: item, id: rations, count: 1 }"
    , "  - id: crate_site"
    , "    builder: room_small"
    , "    bounds: { min_x: -2, min_y: -2, max_x: 2, max_y: 2 }"
    , "    naming: { heads: [KEEP], modifiers: [ASH] }"
    , "    contents:"
    , "      - { kind: container, id: fixture_crate, "
        <> "profile: no_such_profile, count: 1 }"
    ]

noProfileLocationYaml ∷ Text
noProfileLocationYaml = T.unlines
    [ "locations:"
    , "  - id: crate_site"
    , "    builder: room_small"
    , "    bounds: { min_x: -2, min_y: -2, max_x: 2, max_y: 2 }"
    , "    naming: { heads: [KEEP], modifiers: [ASH] }"
    , "    contents:"
    , "      - { kind: container, id: fixture_crate, count: 1 }"
    ]

-- * Lua spec ----------------------------------------------------------

luaSpec ∷ Spec
luaSpec = describe "Location container shells (#2505) — incidental dispatch" $ do

    it "spawns one shell per authored occurrence, in derived slot order" $
        runsOk $ lns
            [ harness
            , "defs = { craeDef() }"
            , "local L = require('scripts.locations')"
            , "L.spawnContents('crate_site', 0, 0, 'p')"
            , "assert(#rec.spawns == 3, 'expected 3 spawns, got ' .. #rec.spawns)"
            , "assert(rec.spawns[1] == '1@fixture_crate')"
            , "assert(rec.spawns[2] == '2@fixture_locker')"
            , "assert(rec.spawns[3] == '3@fixture_locker')"
            ]

    it "still marks the instance-level contents_spawned flag when a \
       \spawn FAILS, warns, and continues to the next entry" $
        runsOk $ lns
            [ harness
            , "defs = { craeDef() }"
            , "failSlot = 1"
            , "local L = require('scripts.locations')"
            , "L.spawnContents('crate_site', 0, 0, 'p')"
            -- D-18: incidental. The failure is a warning, not a return —
            -- contrast the significant pass, which returns and leaves the
            -- marker unwritten so the next chunk load retries.
            , "assert(rec.marked == 1, 'contents_spawned was withheld')"
            , "assert(#rec.spawns == 3, 'the pass stopped at the failure')"
            , "assert(#rec.warns >= 1, 'no warning was logged')"
            , "assert(rec.warns[1]:find('fixture_crate', 1, true),"
            , "       'the warning does not name the entry: ' .. rec.warns[1])"
            ]

    it "skips an ALREADY-BOUND slot, so a retry cannot duplicate a shell" $
        runsOk $ lns
            [ harness
            , "defs = { craeDef() }"
            , "boundSlots = { [2] = 4242 }"
            , "local L = require('scripts.locations')"
            , "L.spawnContents('crate_site', 0, 0, 'p')"
            , "assert(#rec.spawns == 2, 'a bound slot was re-spawned')"
            , "assert(rec.spawns[1] == '1@fixture_crate')"
            , "assert(rec.spawns[2] == '3@fixture_locker')"
            ]

    it "spawns nothing and says why for a hand-stamped location with no \
       \placed instance" $
        runsOk $ lns
            [ harness
            , "defs = { craeDef() }"
            , "placed = nil"
            , "local L = require('scripts.locations')"
            , "L.spawnContents('crate_site', 0, 0, 'p')"
            , "assert(#rec.spawns == 0, 'a hand-stamped ruin spawned a shell')"
            , "assert(rec.marked == 1, 'contents_spawned was withheld')"
            , "assert(#rec.warns >= 1 and"
            , "       rec.warns[1]:find('no placed instance', 1, true),"
            , "       'the skip was silent')"
            ]

-- * Lua harness -------------------------------------------------------

runsOk ∷ Text → Expectation
runsOk chunkText = do
    result ← Lua.run $ do
        Lua.openlibs
        status ← Lua.dostring (TE.encodeUtf8 chunkText)
        case status of
            Lua.OK → return Nothing
            _ → do
                err ← Lua.tostring (-1)
                return (Just (maybe "<no message>" TE.decodeUtf8Lenient err))
    case result ∷ Maybe Text of
        Nothing  → pure ()
        Just msg → expectationFailure (T.unpack msg)

lns ∷ [Text] → Text
lns = T.intercalate "\n"

-- | Stubs for every engine global @scripts/locations.lua@ reaches on the
--   content path. @world.spawnLocationContainer@ records @slot\@def@ and
--   consults @failSlot@; @boundSlots@ seeds already-bound slots, which is
--   what the placed instance's own @containers@ array reports.
harness ∷ Text
harness = lns
    [ "rec = { warns = {}, spawns = {}, marked = 0 }"
    , "defs = {}"
    , "failSlot = nil"
    , "boundSlots = {}"
    , "placed = true"
    , "engine = {"
    , "  logWarn = function(m) rec.warns[#rec.warns + 1] = m end,"
    , "  logInfo = function() end,"
    , "  logDebug = function() end,"
    , "  listLocationDefs = function() return defs end,"
    , "}"
    , "local function containersOf()"
    , "  local out = {}"
    , "  for _, s in ipairs({ { 1, 'fixture_crate' }, { 2, 'fixture_locker' },"
    , "                       { 3, 'fixture_locker' } }) do"
    , "    out[#out + 1] = { slot = s[1], item = s[2],"
    , "                      profile = 'fixture_salvage',"
    , "                      item_instance_id = boundSlots[s[1]],"
    , "                      realized = false }"
    , "  end"
    , "  return out"
    , "end"
    , "world = {"
    , "  getSeed = function() return 7 end,"
    , "  hasSpawnedLocationContents = function() return false end,"
    , "  markLocationContentsSpawned = function() rec.marked = rec.marked + 1 end,"
    , "  listPlacedLocations = function()"
    , "    if not placed then return {} end"
    , "    return { { gx = 0, gy = 0, instance_id = 1,"
    , "               containers = containersOf() } }"
    , "  end,"
    , "  spawnLocationContainer = function(_, slot, _, _, _)"
    , "    local def = ({ 'fixture_crate', 'fixture_locker', 'fixture_locker' })[slot]"
    , "    rec.spawns[#rec.spawns + 1] = slot .. '@' .. tostring(def)"
    , "    return slot ~= failSlot"
    , "  end,"
    , "}"
    , "item = { spawnGround = function() return 1 end }"
    , "loot = { rollFor = function() return 'rations' end }"
    -- The container entries are the only ones under test; a bare
    -- `rations` item entry rides alongside so a cursor that advanced on
    -- EVERY entry rather than on container entries alone would misnumber
    -- the third slot.
    , "function craeDef()"
    , "  return { id = 'crate_site', label = 'Crate Site',"
    , "           builder = 'room_small',"
    , "           bounds = { min_x = -2, min_y = -2, max_x = 2, max_y = 2 },"
    , "           contents = {"
    , "             { kind = 'container', id = 'fixture_crate',"
    , "               profile = 'fixture_salvage', count = 1,"
    , "               position = { x = 0, y = 0 }, significant = false },"
    , "             { kind = 'item', id = 'rations', count = 1,"
    , "               position = { x = 1, y = 0 }, significant = false },"
    , "             { kind = 'container', id = 'fixture_locker',"
    , "               profile = 'fixture_salvage', count = 2,"
    , "               position = { x = 2, y = 0 }, significant = false },"
    , "           } }"
    , "end"
    ]

-- * Pure helpers ------------------------------------------------------

slotAt ∷ Int → LocationContainerSlot
slotAt n = LocationContainerSlot
    { lcsSlot        = n
    , lcsItemDefName = "fixture_crate"
    , lcsProfile     = "fixture_salvage"
    , lcsInstanceId  = Nothing
    , lcsRealized    = False
    }

withSlots ∷ [LocationContainerSlot] → LocationInstances
withSlots slots = withSlotsAndObligation slots []

withSlotsAndObligation
    ∷ [LocationContainerSlot] → [LocationSignificantItem] → LocationInstances
withSlotsAndObligation slots owed =
    mapInstance (\i → i { liContainers = slots, liSignificant = owed })
                (tableFor noContainerDef)

mapInstance
    ∷ (LocationInstance → LocationInstance)
    → LocationInstances → LocationInstances
mapInstance = adjustLocationInstance iid

mapSlots
    ∷ ([LocationContainerSlot] → [LocationContainerSlot])
    → LocationInstances → LocationInstances
mapSlots f = mapInstance (\i → i { liContainers = f (liContainers i) })

-- | Bind a slot through the real registration boundary, failing loudly
--   rather than silently answering the unbound table — which is what a
--   refused binding would otherwise look like to the assertion after it.
mustBind ∷ Int → Text → Word64 → LocationInstances → LocationInstances
mustBind slot defName itemId table =
    fromMaybe (error ("container binding refused for slot " <> show slot))
        (registerLocationContainerSpawn iid slot defName itemId table)

-- | One instance owing exactly one PENDING container slot, and nothing
--   else — the shape every provenance and load check below mutates.
pendingTable ∷ LocationInstances
pendingTable =
    mapInstance (\i → i { liSignificant = [] }) (tableFor crateDef)

-- | The same, with item 900 additionally owed by a significant
--   obligation: the cross-family duplicate.
crossOwnedTable ∷ LocationInstances
crossOwnedTable = mapInstance
    (\i → i { liSignificant =
                  [ LocationSignificantItem 1 "fixture_crate" (Just 900) False ] })
    pendingTable

-- | A table whose container slot's eventual shell id is ALREADY owed by
--   a significant obligation, so the engine binding is refused for a
--   real reason rather than a mocked one. The id is supplied by the
--   caller, which reads it off the live allocator.
poisonedTable ∷ Word64 → LocationInstances
poisonedTable shellId = mapInstance
    (\i → i { liSignificant =
                  [ LocationSignificantItem 1 "fixture_crate"
                                            (Just shellId) False ] })
    pendingTable

paramsWith ∷ LocationInstances → WorldGenParams
paramsWith table =
    defaultWorldGenParams { wgpLocationInstances = table }

crateGround ∷ Text → Word64 → GroundItems
crateGround defName itemId =
    fst (spawnGroundItem (plainInstance itemId defName) 8 8 emptyGroundItems)

plainInstance ∷ Word64 → Text → ItemInstance
plainInstance itemId defName = ItemInstance
    { iiDefName     = defName
    , iiCurrentFill = 0
    , iiQuality     = 100
    , iiCondition   = 100
    , iiWeight      = 6
    , iiSharpness   = 100
    , iiContents    = []
    , iiInstanceId  = itemId
    , iiTemp        = Nothing
    , iiBulk        = Just 60
    , iiStorage     = Just (ItemStorage 40 50)
    }

-- | Bind every UNBOUND pending slot to @mShell@, leaving an
--   already-bound one alone — so a fixture that pre-bound a slot keeps
--   its own id.
boundTable ∷ Maybe Word64 → LocationInstances → LocationInstances
boundTable mShell = mapSlots (map bind)
  where
    bind s = case lcsInstanceId s of
        Just _  → s
        Nothing → s { lcsInstanceId = mShell }

-- | The one page id every snapshot fixture here uses.
cratePage ∷ WorldPageId
cratePage = WorldPageId "crate_page"

-- | The snapshot's page, as the transitional 'WorldPageSave' bridge the
--   LOAD boundary's own checks take — derived through the real
--   'snapshotToSaveData' rather than hand-built, so the two boundaries
--   see the same page.
pageSave ∷ LocationInstances → (WorldPageId, WorldPageSave)
pageSave table = case sdWorlds (snapshotToSaveData req (snapshotWith Nothing table)) of
    (w : _) → (wpsPageId w, w)
    []      → error "pageSave: no page"
  where
    req = SaveRequestMeta { srmSlotName = "container_shells"
                          , srmTimestamp = "ts", srmAutosave = False }

-- | A one-page snapshot whose location table is @table@ and whose ground
--   optionally holds the bound shell, built on the SHARED minimal
--   fixture 'Test.Headless.World.Save.Integrity' maintains.
snapshotWith ∷ Maybe Word64 → LocationInstances → SessionSnapshot
snapshotWith = snapshotWithDef "fixture_crate"

snapshotWithDef
    ∷ Text → Maybe Word64 → LocationInstances → SessionSnapshot
snapshotWithDef defName mShell table = buildSnap cratePage [page]
  where
    page = (minimalPage cratePage)
        { pgsGenParams   = paramsWith (boundTable mShell table)
        , pgsGroundItems = maybe emptyGroundItems (crateGround defName) mShell
        }

-- | The same session, with the shell moved off the ground and into a
--   unit's inventory on that same page — the state an untaken slot must
--   never be in, and the one this slice's pickup refusal makes
--   unreachable by play.
inInventory ∷ SessionSnapshot → SessionSnapshot
inInventory = moveShell $ \page →
    page { pgsGroundItems = emptyGroundItems
         , pgsUnits = (pgsUnits page)
             { usnInstances = HM.singleton (UnitId 1) carrier } }
  where
    carrier = minimalUnit
        { uisInventory = [plainInstance 900 "fixture_crate"] }

-- | The shell moved off the ground and into a BUILDING's storage on the
--   same page — it cannot be there without having been picked up, which
--   this slice refuses outright.
inBuildingStorage ∷ SessionSnapshot → SessionSnapshot
inBuildingStorage = moveShell $ \page →
    page { pgsGroundItems = emptyGroundItems
         , pgsBuildings = (pgsBuildings page)
             { bsnInstances = HM.singleton (BuildingId 1) holder } }
  where
    holder = minimalBuilding
        { bisStorage = [plainInstance 900 "fixture_crate"] }

-- | The shell nested INSIDE another ground container rather than lying
--   on the ground as itself. It exists on the page, so the flattened
--   item set resolves it — but it is not an outer ground item, so no
--   pickup could ever lift it as one, and PLC-15 could never realize it.
nestedOnGround ∷ SessionSnapshot → SessionSnapshot
nestedOnGround = moveShell $ \page →
    page { pgsGroundItems = fst (spawnGroundItem outer 8 8 emptyGroundItems) }
  where
    outer = (plainInstance 901 "fixture_crate")
        { iiContents = [plainInstance 900 "fixture_crate"] }

-- | The bound shell lying on a DIFFERENT page's ground.
otherPage ∷ Word64 → LocationInstances → SessionSnapshot
otherPage shellId table = buildSnap cratePage
    [ (minimalPage cratePage)
        { pgsGenParams = paramsWith (boundTable (Just shellId) table) }
    , (minimalPage elsewhere)
        { pgsGroundItems = crateGround "fixture_crate" shellId }
    ]
  where elsewhere = WorldPageId "elsewhere"

-- | The bound shell deleted outright, the way @item.removeGround@
--   deletes one — nowhere in the session at all.
removedFromGround ∷ SessionSnapshot → SessionSnapshot
removedFromGround = moveShell $ \page →
    page { pgsGroundItems = emptyGroundItems }

moveShell
    ∷ (PageSnapshot → PageSnapshot) → SessionSnapshot → SessionSnapshot
moveShell f snap = snap
    { snapPages = HM.adjust f cratePage (snapPages snap) }

-- | The page core this module's wire examples encode. Its gen params are
--   supplied per example; every other field is a fixed, distinguishable
--   value so a migration that dropped one is visible.
currentPageCore ∷ PageCoreDTO
currentPageCore = PageCoreDTO
    { pcPageId        = cratePage
    , pcGenParams     = toWorldGenParamsDTO (paramsWith pendingTable)
    , pcCameraX       = 1
    , pcCameraY       = 2
    , pcTimeHour      = 12
    , pcTimeMinute    = 30
    , pcTimeRemainder = 0.25
    , pcDateYear      = 1
    , pcDateMonth     = 2
    , pcDateDay       = 3
    , pcMapMode       = ZMDefault
    , pcIdentity      = Nothing
    , pcGeneratedId   = Nothing
    }

-- | The frozen pre-#2505 (v11) shape of the same page: byte-identical
--   field list, but its gen params are 'WorldGenParamsDTOv8', whose
--   location instances have no container slots at all.
--
--   Built from a table that DOES carry a pending slot, so the emptiness
--   the migration reports is the WIRE SHAPE dropping it rather than a
--   fixture that never had one.
legacyPageCore ∷ PageCoreDTOv11
legacyPageCore = PageCoreDTOv11
    { pc11PageId        = pcPageId currentPageCore
    , pc11GenParams     = toWorldGenParamsDTOv8 (paramsWith pendingTable)
    , pc11CameraX       = pcCameraX currentPageCore
    , pc11CameraY       = pcCameraY currentPageCore
    , pc11TimeHour      = pcTimeHour currentPageCore
    , pc11TimeMinute    = pcTimeMinute currentPageCore
    , pc11TimeRemainder = pcTimeRemainder currentPageCore
    , pc11DateYear      = pcDateYear currentPageCore
    , pc11DateMonth     = pcDateMonth currentPageCore
    , pc11DateDay       = pcDateDay currentPageCore
    , pc11MapMode       = pcMapMode currentPageCore
    , pc11Identity      = pcIdentity currentPageCore
    , pc11GeneratedId   = Just (fixtureGeneratedWorldIdForPage cratePage)
    }

instancesOfPages ∷ WorldPages → [LocationInstance]
instancesOfPages pages =
    [ inst
    | page ← HM.elems (wpBase pages)
    , inst ← instancesToList (wgpLocationInstances (pgsGenParams page)) ]

codesOf ∷ [IntegrityError] → [Text]
codesOf = L.nub ∘ map ieCode

containerKinds ∷ LocationYamlDef → [(Text, Maybe Text)]
containerKinds d =
    [ (lycId c, lycProfile c) | c ← lydContents d, lycKind c ≡ "container" ]

decodeOrFail ∷ BS.ByteString → LocationYamlDef
decodeOrFail raw = case decodeDef raw of
    Right d  → d
    Left err → error ("container fixture failed to decode: " <> err)

rejectedMentioning ∷ [Text] → Either String a → Bool
rejectedMentioning fragments =
    either (\err → all (`T.isInfixOf` T.pack err) fragments) (const False)

containerYaml ∷ BS.ByteString
containerYaml = yamlWith ""

yamlWith ∷ BS.ByteString → BS.ByteString
yamlWith extra = BS.concat
    [ BS.unlines
        [ "id: crate_site"
        , "builder: room_small"
        , "bounds: { min_x: -2, min_y: -2, max_x: 2, max_y: 2 }"
        , "naming: { heads: [KEEP], modifiers: [ASH] }"
        , "contents:"
        , "  - kind: container"
        , "    id: fixture_crate"
        , "    profile: fixture_salvage"
        ]
    , extra
    ]
