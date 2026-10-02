{-# LANGUAGE Strict #-}
-- | "Location container shells" — realization (#2510, epic #1231
--   PLC-15): the one @Pending → Realized@ transition a container shell
--   makes, driven through the real @item.realizeGround@ and
--   @item.pickupGround@ Lua functions against a live 'EngineEnv', with a
--   fixture location, container and profile.
--
--   What this layer pins, and why it has to be engine-level:
--
--   * the transition happens IN PLACE (same ground id, same instance id,
--     same position) and latches the slot while discarding its profile,
--     in the step the verb returns from — no world thread runs here, so
--     a queued write would still be unapplied;
--   * it happens EXACTLY ONCE, whichever verb asks and however often:
--     repeats answer @"already-realized"@ and spend no instance id;
--   * the pickup boundary realizes a still-pending shell before it moves
--     it, so a shell never enters an inventory unrealized;
--   * every way realization can fail to complete — an unregistered
--     profile, a shell with no 'iiStorage', a PLC-13 refusal — refuses
--     BOTH verbs with nothing moved and the failure logged;
--   * the realized cargo is byte-equal to PLC-13's pinned vector for the
--     same context (ids masked), which is what ties the engine's context
--     (the page's persisted seed, the slot's instance id and number) to
--     the contract "Test.Headless.Loot.Realization" pins.
--
--   The save\/load half (codec and integrity) lives with the pure spec in
--   "Test.Headless.Location.ContainerShells"; exactly-once across a real
--   save, quit and load, and across two fresh processes, is
--   @tools/location_content_probe.py@'s.
--
--   Run just this gate: @cabal test synarchy-test-headless
--   --test-options='--match "Location container shells"'@.
module Test.Headless.Location.ContainerRealization
    ( spec
    ) where

import UPrelude
import Test.Hspec
import qualified Data.HashMap.Strict as HM
import qualified Data.List as L
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified HsLua as Lua
import Control.Exception (finally)
import Data.IORef
    (atomicModifyIORef', modifyIORef', newIORef, readIORef, writeIORef)
import Engine.Core.Log
    (LogBackend(..), LogConfig(..), LogEntry(..), defaultLogConfig, initLogger)
import Engine.Core.State (EngineEnv(..))
import Engine.Scripting.Lua.API.Items.Ground
    ( itemGetGroundForUnitFn, itemListGroundFn, itemPickupGroundFn
    , itemRealizeGroundFn, pickupGroundOnPage )
import Item.Ground (GroundItem(..), GroundItems(..), spawnGroundItem)
import Item.Types
import Location.Instance
import Location.Types (LocationDef)
import LootProfile.Realize (RealizeContext(..))
import LootProfile.Types
    ( LootProfileDef(..), LootProfileEntry(..), LootProfileRegistry
    , emptyLootProfileRegistry, registerLootProfile )
import Test.Headless.Location.ContainerShells
    ( containerContent, containerItemDefs, containerUnit, initEnv, mkDef
    , onlyGroundId, inventoryOf, paramsWith, plainInstance, spawnContainer )
import Test.Headless.Loot.Realization
    (pinnedVectors, probeItems, renderContents, vectorContexts, vectorProfile)
import Unit.Types (UnitId(..), UnitManager(..), emptyUnitManager)
import World.Chunk.Types (ChunkCoord(..))
import World.Cursor.Types (CursorState(..))
import World.Generate.Types (WorldGenParams(..))
import World.Page.Types (WorldPageId(..))
import World.State.Types
    (WorldState(..), WorldManager(..), emptyWorldState, emptyWorldManager)

spec ∷ Spec
spec = beforeAll initEnv $
    describe "Location container shells (#2510) — realization" $ do

    it "realizes a pending shell IN PLACE through item.realizeGround — \
       \same ground id, instance id and position — latching the slot and \
       \discarding its profile" $ \env → do
        ws ← onePage env "realize_in_place" "fixture_salvage"
        gid ← onlyGroundId ws
        before ← groundItem ws gid
        realizeVia env gid Nothing `shouldReturn` Just "realized"
        after ← groundItem ws gid
        iiInstanceId (giInst after) `shouldBe` iiInstanceId (giInst before)
        (giX after, giY after) `shouldBe` (giX before, giY before)
        -- D-22: the authored default is kept and the cargo is APPENDED;
        -- the salvage profile always proposes exactly two rations.
        map iiDefName (iiContents (giInst after))
            `shouldBe` ["packing_straw", "rations", "rations"]
        slotOf ws `shouldReturn`
            (Just (iiInstanceId (giInst before)), True, Nothing)
        -- The cargo's nested identities came from the engine's REAL
        -- allocator: distinct, never zero, and below the cursor a save
        -- would persist — what the integrity graph's duplicate and
        -- allocator checks require of every item in a session.
        cursor ← readIORef (nextItemInstanceIdRef env)
        let ids = treeIds (giInst after)
        L.nub ids `shouldBe` ids
        ids `shouldSatisfy` all (\i → i > 0 ∧ i < cursor)

    it "answers already-realized for every repeat, from either verb, and \
       \never draws a second cargo tree" $ \env → do
        ws ← onePage env "realize_repeat" "fixture_salvage"
        installUnit env "realize_repeat" 621
        gid ← onlyGroundId ws
        realizeVia env gid Nothing `shouldReturn` Just "realized"
        cargo ← iiContents ∘ giInst <$> groundItem ws gid
        cursor ← readIORef (nextItemInstanceIdRef env)
        realizeVia env gid Nothing `shouldReturn` Just "already-realized"
        realizeVia env gid (Just "realize_repeat")
            `shouldReturn` Just "already-realized"
        pickupVia env 621 gid `shouldReturn` True
        held ← inventoryOf env (UnitId 621)
        map iiContents held `shouldBe` [cargo]
        -- No id was spent after the first realization: the repeats and
        -- the pickup drew nothing.
        readIORef (nextItemInstanceIdRef env) `shouldReturn` cursor

    it "realizes a still-pending shell picked up through item.pickupGround, \
       \so it arrives in the inventory realized with the latch set" $ \env → do
        ws ← onePage env "pickup_realizes" "fixture_salvage"
        installUnit env "pickup_realizes" 622
        gid ← onlyGroundId ws
        shellId ← iiInstanceId ∘ giInst <$> groundItem ws gid
        pickupVia env 622 gid `shouldReturn` True
        held ← inventoryOf env (UnitId 622)
        map iiInstanceId held `shouldBe` [shellId]
        map (map iiDefName ∘ iiContents) held
            `shouldBe` [["packing_straw", "rations", "rations"]]
        slotOf ws `shouldReturn` (Just shellId, True, Nothing)
        HM.size ∘ gisItems <$> readIORef (wsGroundItemsRef ws) `shouldReturn` 0

    it "refuses BOTH paths, moving nothing and logging the location and \
       \slot, when the slot's profile is no longer registered" $ \env → do
        ws ← onePage env "refuse_profile" "fixture_gone"
        refusedBothWays env ws 623 "refuse_profile"
            ["location #1", "slot 1", "fixture_gone", "no longer registered"]

    it "refuses both paths when the shell decodes with NO iiStorage" $
       \env → do
        ws ← onePage env "refuse_storage" "fixture_salvage"
        gid ← onlyGroundId ws
        atomicModifyIORef' (wsGroundItemsRef ws) $ \g →
            ( g { gisItems = HM.adjust
                    (\gi → gi { giInst = (giInst gi) { iiStorage = Nothing } })
                    gid (gisItems g) }
            , () )
        refusedBothWays env ws 624 "refuse_storage"
            ["location #1", "slot 1", "shell_not_storage"]

    it "refuses both paths when PLC-13 itself refuses — a profile entry \
       \naming an unknown item" $ \env → do
        ws ← onePage env "refuse_entry" "fixture_unknown_entry"
        refusedBothWays env ws 625 "refuse_entry"
            ["location #1", "slot 1", "unknown_entry_item"]

    it "completes an EMPTY realization: latch set, authored default kept, \
       \no cargo added, and a repeat re-rolls nothing" $ \env → do
        ws ← onePage env "realize_empty" "fixture_never"
        gid ← onlyGroundId ws
        before ← giInst <$> groundItem ws gid
        cursor ← readIORef (nextItemInstanceIdRef env)
        realizeVia env gid Nothing `shouldReturn` Just "realized"
        after ← giInst <$> groundItem ws gid
        after `shouldBe` before
        slotOf ws `shouldReturn` (Just (iiInstanceId before), True, Nothing)
        realizeVia env gid Nothing `shouldReturn` Just "already-realized"
        giInst <$> groundItem ws gid `shouldReturn` before
        readIORef (nextItemInstanceIdRef env) `shouldReturn` cursor

    it "answers not-pending for an ordinary item, and false for a missing \
       \gid, an unknown page, and a mistyped argument" $ \env → do
        ws ← onePage env "realize_answers" "fixture_salvage"
        ordinary ← atomicModifyIORef' (wsGroundItemsRef ws)
            (spawnGroundItem (plainInstance 4243 "rations") 9 9)
        realizeVia env ordinary Nothing `shouldReturn` Just "not-pending"
        realizeVia env 999 Nothing `shouldReturn` Nothing
        realizeVia env ordinary (Just "no_such_page") `shouldReturn` Nothing
        -- 'Lua.tointeger' and 'Lua.tostring' coerce, so these are the
        -- values that would otherwise address gid 0 and a page spelled
        -- with digits.
        gid ← onlyShell ws
        realizeRawVia env (Lua.pushstring (TE.encodeUtf8 (tshow gid)))
                          (pure ()) `shouldReturn` Nothing
        realizeRawVia env (Lua.pushinteger (fromIntegral gid))
                          (Lua.pushinteger 7) `shouldReturn` Nothing
        slotOf ws ⌦ \(_, realized, _) → realized `shouldBe` False

    it "selects exactly the named page, with no fallback, when ground ids \
       \collide across two live pages" $ \env → do
        (wsA, wsB) ← twoPages env "collide_a" "collide_b"
        gidA ← onlyGroundId wsA
        gidB ← onlyGroundId wsB
        gidA `shouldBe` gidB
        realizeVia env gidB (Just "collide_b") `shouldReturn` Just "realized"
        realizedOf wsA `shouldReturn` False
        realizedOf wsB `shouldReturn` True
        -- …and an omitted page is the ACTIVE one, which is A.
        realizeVia env gidA Nothing `shouldReturn` Just "realized"
        realizedOf wsA `shouldReturn` True

    it "realizes only the ACTING unit's page on a pickup, with colliding \
       \ground ids" $ \env → do
        (wsA, wsB) ← twoPages env "pickup_a" "pickup_b"
        installUnit env "pickup_b" 626
        gid ← onlyGroundId wsB
        pickupVia env 626 gid `shouldReturn` True
        realizedOf wsB `shouldReturn` True
        realizedOf wsA `shouldReturn` False
        HM.size ∘ gisItems <$> readIORef (wsGroundItemsRef wsA)
            `shouldReturn` 1

    it "reports the realized tree's recursive weight and contents at once \
       \through item.listGround and item.getGroundForUnit" $ \env → do
        ws ← onePage env "realize_weight" "fixture_salvage"
        installUnit env "realize_weight" 627
        gid ← onlyGroundId ws
        before ← groundRowVia env 627 gid
        realizeVia env gid Nothing `shouldReturn` Just "realized"
        realized ← giInst <$> groundItem ws gid
        let weight = itemTotalWeight containerItemDefs realized
            row = (weight, itemContentsSig realized)
        fst before `shouldSatisfy` (< weight)
        listedRowVia env gid `shouldReturn` row
        groundRowVia env 627 gid `shouldReturn` row

    it "restores the exact REALIZED tree when the unit vanishes between \
       \removal and insertion, and a later pickup draws nothing" $ \env → do
        ws ← onePage env "realize_rollback" "fixture_salvage"
        gid ← onlyGroundId ws
        shellId ← iiInstanceId ∘ giInst <$> groundItem ws gid
        -- No unit 9999 exists: the realization runs, the removal runs,
        -- the insert fails, and the rollback puts the shell back.
        pickupGroundOnPage env ws (UnitId 9999) gid `shouldReturn` False
        restoredGid ← onlyGroundId ws
        restored ← giInst <$> groundItem ws restoredGid
        iiInstanceId restored `shouldBe` shellId
        map iiDefName (iiContents restored)
            `shouldBe` ["packing_straw", "rations", "rations"]
        slotOf ws `shouldReturn` (Just shellId, True, Nothing)
        cursor ← readIORef (nextItemInstanceIdRef env)
        installUnit env "realize_rollback" 628
        pickupVia env 628 restoredGid `shouldReturn` True
        map iiContents <$> inventoryOf env (UnitId 628)
            `shouldReturn` [iiContents restored]
        readIORef (nextItemInstanceIdRef env) `shouldReturn` cursor

    it "realizes into exactly PLC-13's pinned vector for the same context, \
       \ids masked" $ \env → do
        -- The third pinned context is (seed 99, instance 7, slot 3): a
        -- real page seed, a real instance id and a real slot number, so
        -- the engine can reach it exactly. Guarded, so a re-pin there
        -- cannot silently point this example at a different context.
        let ctx = vectorContexts !! 2
        ctx `shouldBe` RealizeContext
            { rcWorldSeed = 99, rcInstanceId = 7, rcSlot = 3 }
        let pageId = WorldPageId "realize_vector"
            vectorCrate = fromMaybe (error "probeItems has no crate")
                (lookupItemDef "crate" probeItems)
            items = ItemManager $ HM.insert "crate"
                (vectorCrate { idStorage = Just (ItemStorage 1000 1000) })
                (case probeItems of ItemManager m → m)
            table = instanceTable 7
                (mkDef "vector_site" [containerContent "crate" "probe_vectors" 3])
        ws ← emptyWorldState
        writeIORef (wsGenParamsRef ws)
            (Just (paramsWith table) { wgpSeed = 99 })
        installPages env [(pageId, ws)]
        writeIORef (itemManagerRef env) items
        writeIORef (lootProfileRegistryRef env)
            (registerLootProfile vectorProfile emptyLootProfileRegistry)
        spawnContainer env pageId 7 3 (8, 8) `shouldReturn` True
        gid ← onlyGroundId ws
        realizeVia env gid Nothing `shouldReturn` Just "realized"
        shell ← giInst <$> groundItem ws gid
        renderContents shell `shouldBe` (pinnedVectors !! 2)
        writeIORef (itemManagerRef env) containerItemDefs

-- * Fixtures ------------------------------------------------------------

-- | Two rations, always: two lots of one, so a realized shell's contents
--   are a known list rather than a distribution.
salvageProfile ∷ LootProfileDef
salvageProfile = profileOf "fixture_salvage" [("rations", 1.0, 1)] 2

-- | Nothing ever appears: the EMPTY realization.
neverProfile ∷ LootProfileDef
neverProfile = profileOf "fixture_never" [("rations", 0.0, 1)] 1

-- | An entry no item registry knows, which PLC-13 refuses outright.
unknownEntryProfile ∷ LootProfileDef
unknownEntryProfile =
    profileOf "fixture_unknown_entry" [("no_such_item", 1.0, 1)] 1

profileOf ∷ Text → [(Text, Float, Int)] → Int → LootProfileDef
profileOf pid entries mult = LootProfileDef
    { lpdId = pid, lpdMultiplierMin = mult, lpdMultiplierMax = mult
    , lpdEntries =
        [ LootProfileEntry { lpeItem = i, lpeChance = c
                           , lpeQuantityFactor = f }
        | (i, c, f) ← entries ] }

-- | Every fixture profile EXCEPT @fixture_gone@, which a slot names
--   precisely so that it does not resolve.
fixtureProfiles ∷ LootProfileRegistry
fixtureProfiles = foldr registerLootProfile emptyLootProfileRegistry
    [salvageProfile, neverProfile, unknownEntryProfile]

-- | A table holding one placed instance @n@ of @def@.
instanceTable ∷ Int → LocationDef → LocationInstances
instanceTable n def =
    let liid = LocationInstanceId n
        inst = either (error ∘ show) id
            (newLocationInstance Nothing liid (ChunkCoord 0 0) def)
    in emptyLocationInstances
        { lisNextId = n + 1, lisById = HM.singleton liid inst }

-- | One crate slot, on instance 1, naming @profile@.
crateTable ∷ Text → LocationInstances
crateTable profile = instanceTable 1
    (mkDef "realize_site" [containerContent "fixture_crate" profile 1])

-- | A fresh page holding one placed crate location whose slot names
--   @profile@, made the manager's sole (and so active) world, with the
--   fixture registries installed and its ONE shell already spawned and
--   bound through the real verb.
onePage ∷ EngineEnv → Text → Text → IO WorldState
onePage env page profile = do
    ws ← pageFor env profile
    installPages env [(WorldPageId page, ws)]
    spawnContainer env (WorldPageId page) 1 1 (8, 8) `shouldReturn` True
    pure ws

-- | Two such pages, A active, each with its own shell — and, since each
--   page's ground allocator starts at zero, the two shells share a
--   ground id.
twoPages ∷ EngineEnv → Text → Text → IO (WorldState, WorldState)
twoPages env a b = do
    wsA ← pageFor env "fixture_salvage"
    wsB ← pageFor env "fixture_salvage"
    installPages env [(WorldPageId a, wsA), (WorldPageId b, wsB)]
    spawnContainer env (WorldPageId a) 1 1 (8, 8) `shouldReturn` True
    spawnContainer env (WorldPageId b) 1 1 (8, 8) `shouldReturn` True
    pure (wsA, wsB)

pageFor ∷ EngineEnv → Text → IO WorldState
pageFor env profile = do
    writeIORef (itemManagerRef env) containerItemDefs
    writeIORef (lootProfileRegistryRef env) fixtureProfiles
    ws ← emptyWorldState
    writeIORef (wsGenParamsRef ws) (Just (paramsWith (crateTable profile)))
    pure ws

installPages ∷ EngineEnv → [(WorldPageId, WorldState)] → IO ()
installPages env pages = writeIORef (worldManagerRef env) $ emptyWorldManager
    { wmWorlds = pages, wmVisible = take 1 (map fst pages) }

-- | A player unit standing on the shell's tile of @page@, and no other.
installUnit ∷ EngineEnv → Text → Int → IO ()
installUnit env page n = writeIORef (unitManagerRef env) $ emptyUnitManager
    { umInstances = HM.singleton (UnitId (fromIntegral n))
        (containerUnit (WorldPageId page)) }

-- | Every instance id in a tree, its root included.
treeIds ∷ ItemInstance → [Word64]
treeIds i = iiInstanceId i : concatMap treeIds (iiContents i)

groundItem ∷ WorldState → Int → IO GroundItem
groundItem ws gid = do
    gis ← readIORef (wsGroundItemsRef ws)
    maybe (fail ("no ground item " <> show gid)) pure
        (HM.lookup gid (gisItems gis))

-- | The ground id of the page's ONE bound shell, among other items.
onlyShell ∷ WorldState → IO Int
onlyShell ws = do
    (shellId, _, _) ← slotOf ws
    gis ← readIORef (wsGroundItemsRef ws)
    case [ gid | (gid, gi) ← HM.toList (gisItems gis)
               , Just (iiInstanceId (giInst gi)) ≡ shellId ] of
        [gid] → pure gid
        other → fail ("expected one shell, got " <> show (length other))

-- | The page's one container slot: its bound id, its latch, its profile.
slotOf ∷ WorldState → IO (Maybe Word64, Bool, Maybe Text)
slotOf ws = do
    mp ← readIORef (wsGenParamsRef ws)
    case [ s | p ← maybeToList mp
             , inst ← instancesToList (wgpLocationInstances p)
             , s ← liContainers inst ] of
        [s] → pure (lcsInstanceId s, lcsRealized s, lcsProfile s)
        other → fail ("expected one container slot, got " <> show (length other))

realizedOf ∷ WorldState → IO Bool
realizedOf ws = (\(_, r, _) → r) <$> slotOf ws

-- | Both verbs refuse, and NOTHING moves: the ground map (ids, contents
--   and allocator), the unit's inventory, the selection, the slot and
--   the item-instance cursor are all exactly as they were, and the
--   refusal was logged with every one of @fragments@.
refusedBothWays ∷ EngineEnv → WorldState → Int → Text → [Text] → Expectation
refusedBothWays env ws n page fragments = do
    installUnit env page n
    gid ← onlyGroundId ws
    atomicModifyIORef' (wsCursorRef ws) $ \cs →
        (cs { selectedGroundItem = Just gid }, ())
    groundBefore ← readIORef (wsGroundItemsRef ws)
    slotBefore ← slotOf ws
    cursor ← readIORef (nextItemInstanceIdRef env)
    (answers, logged) ← withCapturedLog env $
        (,) <$> realizeVia env gid Nothing <*> pickupVia env n gid
    answers `shouldBe` (Nothing, False)
    groundAfter ← readIORef (wsGroundItemsRef ws)
    gisNextId groundAfter `shouldBe` gisNextId groundBefore
    HM.map giInst (gisItems groundAfter)
        `shouldBe` HM.map giInst (gisItems groundBefore)
    inventoryOf env (UnitId (fromIntegral n)) `shouldReturn` []
    selectedGroundItem <$> readIORef (wsCursorRef ws) `shouldReturn` Just gid
    slotOf ws `shouldReturn` slotBefore
    readIORef (nextItemInstanceIdRef env) `shouldReturn` cursor
    -- Logged once per refused verb, and each line names the location
    -- instance, the slot and the reason.
    length (filter (\line → all (`T.isInfixOf` line) fragments) logged)
        `shouldBe` 2

withCapturedLog ∷ EngineEnv → IO a → IO (a, [Text])
withCapturedLog env action = do
    entriesRef ← newIORef []
    capturing ← initLogger defaultLogConfig
        { lcBackend = LogToCallback (\e → modifyIORef' entriesRef (e :)) }
    before ← readIORef (loggerRef env)
    result ← (writeIORef (loggerRef env) capturing >> action)
                 `finally` writeIORef (loggerRef env) before
    entries ← readIORef entriesRef
    pure (result, reverse (map leMessage entries))

-- * The real Lua verbs ---------------------------------------------------

-- | @item.realizeGround(gid[, pageId])@: 'Just' the string it answered,
--   or 'Nothing' for its @false@.
realizeVia ∷ EngineEnv → Int → Maybe Text → IO (Maybe Text)
realizeVia env gid mPage = realizeRawVia env
    (Lua.pushinteger (fromIntegral gid))
    (forM_ mPage (Lua.pushstring ∘ TE.encodeUtf8))

realizeRawVia
    ∷ EngineEnv → Lua.LuaE Lua.Exception () → Lua.LuaE Lua.Exception ()
    → IO (Maybe Text)
realizeRawVia env pushGid pushPage = Lua.run $ do
    Lua.openlibs
    pushGid
    pushPage
    _ ← itemRealizeGroundFn env
    ty ← Lua.ltype Lua.top
    case ty of
        Lua.TypeString →
            fmap TE.decodeUtf8Lenient <$> Lua.tostring Lua.top
        _ → do
            b ← Lua.toboolean Lua.top
            pure (if b then Just "<true>" else Nothing)

-- | @item.pickupGround(uid, gid)@.
pickupVia ∷ EngineEnv → Int → Int → IO Bool
pickupVia env uid gid = Lua.run $ do
    Lua.openlibs
    Lua.pushinteger (fromIntegral uid)
    Lua.pushinteger (fromIntegral gid)
    _ ← itemPickupGroundFn env
    Lua.toboolean Lua.top

-- | The @{weight, contentsKey}@ of ground row @gid@ in
--   @item.listGround()@ (the ACTIVE page's listing).
listedRowVia ∷ EngineEnv → Int → IO (Float, Text)
listedRowVia env gid = rowVia env (itemListGroundFn env) $ T.unlines
    [ "for _, r in ipairs(real()) do"
    , "  if r.id == " <> tshow gid <> " then"
    , "    return r.weight, r.contentsKey"
    , "  end"
    , "end"
    , "error('no such row')" ]

-- | The same pair through @item.getGroundForUnit(uid, gid)@.
groundRowVia ∷ EngineEnv → Int → Int → IO (Float, Text)
groundRowVia env uid gid = rowVia env (itemGetGroundForUnitFn env) $ T.unlines
    [ "local r = real(" <> tshow uid <> ", " <> tshow gid <> ")"
    , "assert(r, 'no such row')"
    , "return r.weight, r.contentsKey" ]

rowVia
    ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults → Text
    → IO (Float, Text)
rowVia _ fn chunk = Lua.run $ do
    Lua.openlibs
    Lua.pushHaskellFunction fn
    Lua.setglobal "real"
    status ← Lua.dostring (TE.encodeUtf8 chunk)
    case status of
        Lua.OK → do
            w ← Lua.tonumber (Lua.nth 2)
            k ← Lua.tostring Lua.top
            case (w, k) of
                (Just (Lua.Number d), Just bs) →
                    pure (realToFrac d, TE.decodeUtf8Lenient bs)
                _ → error "rowVia: the row lacked weight or contentsKey"
        _ → do
            err ← Lua.tostring Lua.top
            error ("rowVia failed: " <> maybe "<no message>" show err)
