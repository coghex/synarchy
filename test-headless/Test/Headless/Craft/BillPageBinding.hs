{-# LANGUAGE Strict #-}
-- | Every craft-bill verb an AI job drives resolves its bill on the
--   ACTING UNIT's own world page, never the active one (#2325).
--
--   Bill ids are PER PAGE and every page's allocator starts at the same
--   number ('Craft.Bills.emptyCraftBills' is @CraftBills HM.empty 1@),
--   so bill 1 exists on every page by construction. The craft AI stores
--   one bare @job.billId@ and spends many later ticks calling verbs with
--   it (@scripts\/unit_ai_craft.lua@), while the APPLIED visible page
--   moves on the WORLD thread. Every one of those verbs used to
--   re-resolve 'Engine.Core.State.activeWorldPageFrom' for itself, so a
--   page switch landed a release, a progress pour, a completed cycle or a
--   working flag on a stranger's identically numbered bill — page A's
--   bill stayed claimed by a unit that had dropped the job while page B's
--   crafter lost its claim mid-cycle. #1673's station guard does not
--   cover it: it checks @job.bid@, not @job.billId@, and it is not atomic
--   with the calls that follow it.
--
--   The fixture is therefore the collision itself, held still: TWO live
--   pages, each holding a bill numbered 1, with the crafter on one page
--   and the OTHER page ACTIVE for the whole spec. Nothing here is
--   timing-dependent — the interleaving the defect needs is simply the
--   fixture's resting state.
--
--   Two halves, and both are load-bearing:
--
--     * every actor-qualified verb — @getBill@, @claimBill@,
--       @releaseBill@, @setBillWorking@, @addBillProgress@,
--       @completeBillCycle@, and @executeAt@ when a bill id is supplied —
--       lands on the CRAFTER's bill, and the BYSTANDER bill on the active
--       page is byte-identical afterwards. Each verb is checked on its
--       own, because each resolved the page independently;
--     * the ACTIVE-page family — @getBills@, @addBill@, @cancelBill@,
--       @reorderBill@, @setBillPaused@ — still targets the visible page,
--       which is the page the player is looking at. Without this half a
--       blanket "resolve everything on the unit" change would pass.
--
--   Plus the fail-closed rule: a missing unit, and a unit whose recorded
--   page has no live 'World.State.Types.WorldState', are REFUSED. There
--   is deliberately no active-page fallback
--   ('Engine.Scripting.Lua.API.Units.Page.unitOwningWorldState'), so
--   both cases must leave both pages untouched rather than quietly
--   mutating the visible one.
--
--   Same bare-Lua-backend technique as
--   'Test.Headless.Craft.OutputIdentity' and
--   'Test.Headless.Unit.CargoApi': real registered verbs over real
--   manager refs, with two in-memory pages and no worldgen.
--
--   Run just this gate: @cabal test synarchy-test-headless
--   --test-options='--match "Craft bill page binding"'@.
module Test.Headless.Craft.BillPageBinding (spec) where

import UPrelude
import Test.Hspec
import qualified Data.HashMap.Strict as HM
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import Data.IORef (newIORef, readIORef, writeIORef)
import Data.List (sort, sortOn)
import Building.Schema
import Building.Types
    ( BuildingDef(..), BuildingId(..), BuildingInstance(..)
    , BuildingManager(..), emptyBuildingManager )
import Craft.Bills
    ( BillId(..), CraftBill(..), CraftBills(..), addBill, lookupBill )
import Craft.Types
    ( RecipeDef(..), RecipeIngredient(..), RecipeManager(..) )
import Engine.Asset.Handle (TextureHandle(..))
import Engine.Core.State (EngineEnv(..))
import Engine.Core.Thread (ThreadControl(..))
import Engine.Scripting.Lua.API (registerLuaAPI)
import Engine.Scripting.Lua.Thread (createLuaBackendState)
import Engine.Scripting.Lua.Thread.Console (executeDebugLua)
import Engine.Scripting.Lua.Types (LuaBackendState(..))
import Item.Types (ItemDef(..), ItemInstance(..), ItemManager(..))
import Unit.Direction (Direction(..))
import Unit.Faction (Faction(..))
import Unit.Types
    ( UnitDef(..), UnitId(..), UnitInstance(..), UnitManager(..)
    , defaultNaturalResistance, emptyUnitManager )
import World.Page.Types (WorldPageId(..))
import World.State.Types
    ( WorldManager(..), WorldState(..), emptyWorldState, emptyWorldManager )

-- * Fixture identities

-- | @actorPage@ is where the crafter stands and holds bill 1.
--   @visiblePage@ is ACTIVE for every example and holds its OWN bill 1 —
--   a genuinely live bystander, because the defect is a cross-page
--   MUTATION and not a dangling reference. @ghostPage@ is recorded on a
--   unit but deliberately absent from @wmWorlds@: the "unit whose page
--   has no live world" case.
actorPage, visiblePage, ghostPage ∷ WorldPageId
actorPage   = WorldPageId "bill_page_binding_actor"
visiblePage = WorldPageId "bill_page_binding_visible"
ghostPage   = WorldPageId "bill_page_binding_ghost"

-- | uid 1 on @actorPage@, adjacent to its own furnace. uid 2 stands on
--   @ghostPage@; uid 999 does not exist at all.
crafterUid, ghostUid ∷ UnitId
crafterUid = UnitId 1
ghostUid   = UnitId 2

-- | bid 7 is the crafter's furnace on @actorPage@; bid 8 is its twin on
--   @visiblePage@, at the same coordinates, so nothing but the page
--   distinguishes the two stations either.
actorStation, visibleStation ∷ BuildingId
actorStation   = BuildingId 7
visibleStation = BuildingId 8

-- | Both pages allocate this same id, which is the whole point.
billOne ∷ BillId
billOne = BillId 1

-- * Content

itemDef ∷ Text → ItemDef
itemDef name = ItemDef
    { idName = name, idDisplayName = name
    , idTexture = TextureHandle 0, idIconTexture = TextureHandle 0
    , idWeight = 1.0, idWeightSpec = Nothing, idBulk = 1.0
    , idStorage = Nothing, idKind = "misc"
    , idCategory = "Materials", idMake = "", idMaterial = ""
    , idQualitySpec = Nothing, idQualityTiers = []
    , idContainer = Nothing, idDefaultContents = []
    , idFood = Nothing, idWeapon = Nothing, idArmor = Nothing
    , idUnequippable = False, idBuffs = [], idInsulation = 0
    , idSourcePath = "test-fixture"
    }

testItems ∷ ItemManager
testItems = ItemManager $ HM.fromList
    [ ("steel_bar", itemDef "steel_bar")
    , ("granite_chunk", itemDef "granite_chunk") ]

-- | Untagged and unpowered, so nothing but the page rule stands between
--   the fixture and a completed craft.
smeltRecipe ∷ RecipeDef
smeltRecipe = RecipeDef
    { rdId = "page_smelt", rdName = "Page Smelt", rdStation = "smelt"
    , rdInputs = [RecipeIngredient "steel_bar" 1]
    , rdFuel = Nothing, rdWork = 2
    , rdOutputs = [RecipeIngredient "granite_chunk" 2]
    , rdKnowledge = Nothing, rdSkill = Nothing
    , rdRepairAxis = Nothing, rdOutputTemp = Nothing, rdPowerDraw = 0
    }

-- | @bdBuildWork@ 0 with no state animations reports Built the instant it
--   is spawned, and the "smelt" operation is what @page_smelt@ asks for.
furnaceDef ∷ BuildingDef
furnaceDef = BuildingDef
    { bdName = "furnace", bdDisplayName = "Furnace", bdCategory = "Test"
    , bdDescription = "", bdTextures = legacyAssets (TextureHandle 0)
    , bdIconTexture = TextureHandle 0
    , bdTileW = 1, bdTileH = 1, bdPlacement = "flat_ground"
    , bdIsStarting = False, bdRace = "acolyte_cult"
    , bdSpriteAnchor = "diamond_bottom", bdBuildWork = 0
    , bdMaterials = HM.empty, bdStorageCapacity = 0
    , bdOperations = ["smelt"], bdAnimations = HM.empty
    , bdRoleAnims = Map.empty
    , bdVisualClass = FreestandingInstallation
    , bdPowerDrain = 0, bdPowerNode = Nothing
    }

acolyteDef ∷ UnitDef
acolyteDef = UnitDef
    { udName = "acolyte", udNamePool = Nothing
    , udDisplayName = Just "Acolyte"
    , udTexture = TextureHandle 0, udPortrait = Nothing
    , udDirSprites = Map.empty
    , udBaseWidth = 0, udMaxSpeed = 1.0, udRunThreshold = 0.6
    , udAnimations = HM.empty, udStateAnims = HM.empty, udEagerStats = False
    , udStatTemplates = HM.empty, udBodyTemplates = HM.empty
    , udSkillTemplates = HM.empty, udKnowledgeTemplates = HM.empty
    , udStartingInventory = []
    , udEquipmentClass = Nothing, udStartingEquipment = HM.empty
    , udStartingAccessories = []
    , udBodyParts = []
    , udNaturalResistance = defaultNaturalResistance
    , udNaturalWeapon = Nothing, udModifiers = []
    }

mkItem ∷ Text → Word64 → ItemInstance
mkItem name iid = ItemInstance
    { iiDefName = name, iiCurrentFill = 0
    , iiQuality = 100, iiCondition = 100
    , iiWeight = 1.0, iiSharpness = 100
    , iiContents = [], iiInstanceId = iid
    , iiTemp = Nothing, iiBulk = Just 1, iiStorage = Nothing
    }

mkUnit ∷ WorldPageId → [ItemInstance] → UnitInstance
mkUnit pg inv = UnitInstance
    { uiDefName = "acolyte", uiName = "", uiPage = pg
    , uiTexture = TextureHandle 0, uiDirSprites = Map.empty
    , uiBaseWidth = 0, uiGridX = 10, uiGridY = 10, uiGridZ = 0
    , uiRealZ = 0, uiFacing = DirS
    , uiCurrentAnim = "", uiAnimStart = 0, uiAnimReverse = False
    , uiActivity = "idle", uiPose = "standing", uiAnimStride = 1
    , uiStats = HM.singleton "carrying_capacity" 100
    , uiModifiers = HM.empty, uiSkills = HM.empty
    , uiKnowledge = HM.empty, uiInventory = inv, uiEquipment = HM.empty
    , uiAccessories = [], uiFactionId = FactionPlayer, uiWounds = []
    , uiScars = [], uiImmuneResponse = 0, uiImmunities = HM.empty
    , uiBlood = 5.0, uiLastAttackerUid = Nothing, uiLastAttackerAt = 0
    , uiAnimOverride = "", uiFrozen = False, uiForceLoop = False
    , uiClimbDest = Nothing, uiTrailState = Nothing
    }

mkStation ∷ WorldPageId → BuildingInstance
mkStation pg = BuildingInstance
    { biDefName = "furnace", biPage = pg, biTexture = TextureHandle 0
    , biAnchorX = 11, biAnchorY = 10, biGridZ = 0, biSpawnedAt = 0
    , biTileW = 1, biTileH = 1, biSpawnRemaining = 0, biBuildProgress = 0
    , biMaterialsDelivered = HM.empty, biStorage = []
    }

-- * Fixture

-- | Rebuild both live pages, every manager ref, and both bill stores.
--
--   The two @addBill@ calls are the collision: each page's allocator is
--   at 1, so each returns 'billOne'. @visiblePage@ gets a SECOND bill so
--   @reorderBill@ has a neighbour to swap with, and the two pages'
--   @remaining@ counts differ (3 vs 5) so a reading can never be
--   ambiguous about which store answered.
--
--   @wmVisible@ names @visiblePage@, so 'activeWorldPageFrom' resolves to
--   the page the crafter is NOT on for the whole spec.
resetPages ∷ EngineEnv → IO ()
resetPages env = do
    wsActor   ← emptyWorldState
    wsVisible ← emptyWorldState
    writeIORef (worldManagerRef env) emptyWorldManager
        { wmWorlds  = [(actorPage, wsActor), (visiblePage, wsVisible)]
        , wmVisible = [visiblePage] }
    writeIORef (itemManagerRef env) testItems
    writeIORef (recipeManagerRef env)
        (RecipeManager (HM.singleton "page_smelt" smeltRecipe))
    writeIORef (unitManagerRef env) emptyUnitManager
        { umDefs = HM.singleton "acolyte" acolyteDef
        , umInstances = HM.fromList
            [ (crafterUid, mkUnit actorPage [mkItem "steel_bar" 101])
            , (ghostUid,   mkUnit ghostPage [mkItem "steel_bar" 201]) ] }
    writeIORef (buildingManagerRef env) emptyBuildingManager
        { bmDefs = HM.singleton "furnace" furnaceDef
        , bmInstances = HM.fromList
            [ (actorStation,   mkStation actorPage)
            , (visibleStation, mkStation visiblePage) ] }
    writeIORef (wsCraftBillsRef wsActor)
        (fst (addBill actorStation "page_smelt" 3 emptyBillsOf))
    writeIORef (wsCraftBillsRef wsVisible) $
        let (one, _) = addBill visibleStation "page_smelt" 5 emptyBillsOf
        in fst (addBill visibleStation "page_smelt" 7 one)
  where
    emptyBillsOf = CraftBills HM.empty 1

-- * Live-state readers

-- | The bill store of one named page, straight out of the live world
--   manager — never through a second Lua hop, which would re-ask the very
--   resolution under test.
billsOn ∷ EngineEnv → WorldPageId → IO CraftBills
billsOn env pg = do
    wm ← readIORef (worldManagerRef env)
    case lookup pg (wmWorlds wm) of
        Nothing → expectationFailure ("no live page " <> show pg) >> pure (CraftBills HM.empty 1)
        Just ws → readIORef (wsCraftBillsRef ws)

-- | Everything a mutation could change about one bill, as one comparable
--   value: claimant, working flag, progress, remaining and paused. A
--   bystander assertion compares the WHOLE tuple, so a case cannot pass
--   because it happened to check the one field a stray write missed.
data BillView = BillView
    { bvClaimant  ∷ Maybe Word32
    , bvWorking   ∷ Bool
    , bvProgress  ∷ Float
    , bvRemaining ∷ Int
    , bvPaused    ∷ Bool
    , bvStation   ∷ Word32
    } deriving (Show, Eq)

viewBill ∷ EngineEnv → WorldPageId → BillId → IO (Maybe BillView)
viewBill env pg bid = do
    bills ← billsOn env pg
    pure $ flip fmap (lookupBill bid bills) $ \b → BillView
        { bvClaimant  = (\(UnitId u) → u) <$> cbClaimant b
        , bvWorking   = cbWorking b
        , bvProgress  = cbProgress b
        , bvRemaining = cbRemaining b
        , bvPaused    = cbPaused b
        , bvStation   = unBuildingId (cbStation b)
        }

-- | Which bill ids a page holds, ascending.
billIdsOn ∷ EngineEnv → WorldPageId → IO [Word32]
billIdsOn env pg = do
    bills ← billsOn env pg
    pure (sort [ unBillId (cbId b) | b ← HM.elems (cbsBills bills) ])

-- | The manual-reorder sort keys, by bill id — what @reorderBill@ writes.
billSeqsOn ∷ EngineEnv → WorldPageId → IO [(Word32, Int)]
billSeqsOn env pg = do
    bills ← billsOn env pg
    pure (sortOn fst [ (unBillId (cbId b), cbSeq b) | b ← HM.elems (cbsBills bills) ])

-- | The crafter's inventory as (id, defName) pairs — the proof that a
--   refused @executeAt@ consumed nothing and produced nothing.
crafterInventory ∷ EngineEnv → IO [(Word64, Text)]
crafterInventory env = do
    um ← readIORef (unitManagerRef env)
    pure $ maybe [] (map (\i → (iiInstanceId i, iiDefName i)) ∘ uiInventory)
                    (HM.lookup crafterUid (umInstances um))

-- * Lua plumbing

newBareLuaBackend ∷ EngineEnv → IO LuaBackendState
newBareLuaBackend env = do
    ls ← createLuaBackendState (luaToEngineQueue env) (luaQueue env)
                                (assetPoolRef env) (nextObjectIdRef env)
                                (inputStateRef env) (loggerRef env)
    stateRef ← newIORef ThreadRunning
    registerLuaAPI (lbsLuaState ls) env ls stateRef
    pure ls

-- | Evaluate one console line, failing the example on a Lua error rather
--   than letting @"error: ..."@ read as an ordinary refusal string.
runOk ∷ LuaBackendState → Text → IO Text
runOk ls src = do
    r ← executeDebugLua (lbsLuaState ls) src
    r `shouldNotSatisfy` isLuaError
    pure r
  where
    isLuaError t = "error:" `T.isPrefixOf` t ∨ "syntax error:" `T.isPrefixOf` t

-- | @tostring@ so a boolean or nil comes back as a quoted JSON string,
--   the same discipline the sibling Lua-surface specs use.
callStr ∷ LuaBackendState → Text → IO Text
callStr ls expr = runOk ls ("return tostring(" <> expr <> ")")

q ∷ Text → Text
q t = "\"" <> t <> "\""

-- * Spec

spec ∷ SpecWith EngineEnv
spec = describe "Craft bill page binding (#2325)" $ do

    describe "the fixture really is the collision" $ do

        it "gives both live pages a bill numbered 1, with the crafter on \
           \one and the OTHER page active" $ \env → do
            resetPages env
            billIdsOn env actorPage   `shouldReturn` [1]
            billIdsOn env visiblePage `shouldReturn` [1, 2]
            ls ← newBareLuaBackend env
            -- The ACTIVE page is the one the crafter is not on: getBills
            -- is the active-page listing, and it sees page B's two bills.
            callStr ls "#craft.getBills()" `shouldReturn` q "2"

    describe "an actor-qualified verb lands on the ACTING unit's own bill" $ do

        it "getBill reads the crafter's bill, not the active page's twin" $ \env → do
            resetPages env
            ls ← newBareLuaBackend env
            -- remaining 3 is page A's count; page B's bill 1 has 5.
            callStr ls "craft.getBill(1, 1).remaining" `shouldReturn` q "3"
            callStr ls "craft.getBill(1, 1).station" `shouldReturn` q "7"

        it "claimBill claims the crafter's bill and leaves the bystander \
           \unclaimed" $ \env → do
            resetPages env
            ls ← newBareLuaBackend env
            before' ← viewBill env visiblePage billOne
            callStr ls "craft.claimBill(1, 1, 30)" `shouldReturn` q "true"
            (bvClaimant <$>) <$> viewBill env actorPage billOne
                `shouldReturn` Just (Just 1)
            viewBill env visiblePage billOne `shouldReturn` before'

        it "setBillWorking flags the crafter's bill only" $ \env → do
            resetPages env
            ls ← newBareLuaBackend env
            before' ← viewBill env visiblePage billOne
            callStr ls "craft.setBillWorking(1, 1, true)" `shouldReturn` q "true"
            (bvWorking <$>) <$> viewBill env actorPage billOne
                `shouldReturn` Just True
            viewBill env visiblePage billOne `shouldReturn` before'

        it "addBillProgress pours into the crafter's bill only" $ \env → do
            resetPages env
            ls ← newBareLuaBackend env
            before' ← viewBill env visiblePage billOne
            callStr ls "craft.addBillProgress(1, 1, 0.25)" `shouldReturn` q "0.25"
            (bvProgress <$>) <$> viewBill env actorPage billOne
                `shouldReturn` Just 0.25
            viewBill env visiblePage billOne `shouldReturn` before'

        it "completeBillCycle advances the crafter's bill only" $ \env → do
            resetPages env
            ls ← newBareLuaBackend env
            before' ← viewBill env visiblePage billOne
            callStr ls "craft.completeBillCycle(1, 1)" `shouldReturn` q "2"
            (bvRemaining <$>) <$> viewBill env actorPage billOne
                `shouldReturn` Just 2
            viewBill env visiblePage billOne `shouldReturn` before'

        it "releaseBill hands back the crafter's claim, never the \
           \bystander's" $ \env → do
            resetPages env
            ls ← newBareLuaBackend env
            -- Both pages' bill 1 claimed, by different units, so a
            -- misrouted release has something real to break.
            _ ← runOk ls "craft.claimBill(1, 1, 30); return 'ok'"
            billsV ← billsOn env visiblePage
            wm ← readIORef (worldManagerRef env)
            case lookup visiblePage (wmWorlds wm) of
                Nothing → expectationFailure "no visible page"
                Just ws → writeIORef (wsCraftBillsRef ws) $ billsV
                    { cbsBills = HM.adjust
                        (\b → b { cbClaimant = Just ghostUid, cbClaimedAt = 0 })
                        billOne (cbsBills billsV) }
            before' ← viewBill env visiblePage billOne
            callStr ls "craft.releaseBill(1, 1)" `shouldReturn` q "true"
            (bvClaimant <$>) <$> viewBill env actorPage billOne
                `shouldReturn` Just Nothing
            viewBill env visiblePage billOne `shouldReturn` before'

        it "executeAt runs for a bill on the crafter's page" $ \env → do
            resetPages env
            ls ← newBareLuaBackend env
            beforeV ← viewBill env visiblePage billOne
            callStr ls "craft.executeAt(1, 'page_smelt', 7, 1)"
                `shouldReturn` q "true"
            -- The bar went, two chunks arrived.
            inv ← crafterInventory env
            map snd inv `shouldBe` ["granite_chunk", "granite_chunk"]
            viewBill env visiblePage billOne `shouldReturn` beforeV

        it "executeAt REFUSES a bill id that exists only on the active \
           \page, consuming nothing" $ \env → do
            resetPages env
            -- Page A keeps its station but loses bill 1; page B still has
            -- one. On master the id resolves nowhere at all and the craft
            -- runs regardless — the bill argument was never validated.
            wm ← readIORef (worldManagerRef env)
            case lookup actorPage (wmWorlds wm) of
                Nothing → expectationFailure "no actor page"
                Just ws → writeIORef (wsCraftBillsRef ws) (CraftBills HM.empty 2)
            ls ← newBareLuaBackend env
            beforeInv ← crafterInventory env
            beforeV   ← viewBill env visiblePage billOne
            r ← runOk ls $ T.concat
                [ "local ok, err = craft.executeAt(1, 'page_smelt', 7, 1); "
                , "return tostring(ok) .. '|' .. tostring(err)" ]
            r `shouldBe` q "false|no bill 1 on the crafting unit's world page"
            crafterInventory env `shouldReturn` beforeInv
            viewBill env visiblePage billOne `shouldReturn` beforeV

        it "executeAt with NO bill id is unaffected — the ad-hoc caller \
           \never had one to validate" $ \env → do
            resetPages env
            wm ← readIORef (worldManagerRef env)
            case lookup actorPage (wmWorlds wm) of
                Nothing → expectationFailure "no actor page"
                Just ws → writeIORef (wsCraftBillsRef ws) (CraftBills HM.empty 2)
            ls ← newBareLuaBackend env
            callStr ls "craft.executeAt(1, 'page_smelt', 7)"
                `shouldReturn` q "true"
            inv ← crafterInventory env
            map snd inv `shouldBe` ["granite_chunk", "granite_chunk"]

    describe "resolution fails CLOSED — never back to the active page" $ do

        it "refuses every actor-qualified verb for a unit that does not \
           \exist, leaving both pages untouched" $ \env → do
            resetPages env
            ls ← newBareLuaBackend env
            beforeA ← viewBill env actorPage billOne
            beforeV ← viewBill env visiblePage billOne
            callStr ls "craft.getBill(999, 1)"                `shouldReturn` q "nil"
            callStr ls "craft.claimBill(1, 999, 30)"          `shouldReturn` q "false"
            callStr ls "craft.releaseBill(999, 1)"            `shouldReturn` q "false"
            callStr ls "craft.setBillWorking(999, 1, true)"   `shouldReturn` q "false"
            callStr ls "craft.addBillProgress(999, 1, 0.5)"   `shouldReturn` q "nil"
            callStr ls "craft.completeBillCycle(999, 1)"      `shouldReturn` q "nil"
            viewBill env actorPage billOne   `shouldReturn` beforeA
            viewBill env visiblePage billOne `shouldReturn` beforeV

        it "refuses them all for a unit whose recorded page has no live \
           \world" $ \env → do
            resetPages env
            ls ← newBareLuaBackend env
            beforeA ← viewBill env actorPage billOne
            beforeV ← viewBill env visiblePage billOne
            callStr ls "craft.getBill(2, 1)"                `shouldReturn` q "nil"
            callStr ls "craft.claimBill(1, 2, 30)"          `shouldReturn` q "false"
            callStr ls "craft.releaseBill(2, 1)"            `shouldReturn` q "false"
            callStr ls "craft.setBillWorking(2, 1, true)"   `shouldReturn` q "false"
            callStr ls "craft.addBillProgress(2, 1, 0.5)"   `shouldReturn` q "nil"
            callStr ls "craft.completeBillCycle(2, 1)"      `shouldReturn` q "nil"
            viewBill env actorPage billOne   `shouldReturn` beforeA
            viewBill env visiblePage billOne `shouldReturn` beforeV

        it "refuses a bill id absent from the crafter's own page even \
           \though the ACTIVE page has one" $ \env → do
            resetPages env
            ls ← newBareLuaBackend env
            beforeV ← viewBill env visiblePage billOne
            -- Page B has bill 2. Page A does not.
            callStr ls "craft.getBill(1, 2)"              `shouldReturn` q "nil"
            callStr ls "craft.claimBill(2, 1, 30)"        `shouldReturn` q "false"
            callStr ls "craft.releaseBill(1, 2)"          `shouldReturn` q "false"
            callStr ls "craft.setBillWorking(1, 2, true)" `shouldReturn` q "false"
            callStr ls "craft.addBillProgress(1, 2, 0.5)" `shouldReturn` q "nil"
            callStr ls "craft.completeBillCycle(1, 2)"    `shouldReturn` q "nil"
            viewBill env visiblePage (BillId 2) `shouldReturn`
                (Just (BillView Nothing False 0 7 False 8))
            viewBill env visiblePage billOne `shouldReturn` beforeV

        it "refuses an old-arity call rather than silently resolving the \
           \active page" $ \env → do
            resetPages env
            ls ← newBareLuaBackend env
            beforeA ← viewBill env actorPage billOne
            beforeV ← viewBill env visiblePage billOne
            -- Exactly what an un-migrated caller would send: the bill id
            -- alone, which now reads as (uid = 1, billId = nil).
            callStr ls "craft.getBill(1)"              `shouldReturn` q "nil"
            callStr ls "craft.releaseBill(1)"          `shouldReturn` q "false"
            callStr ls "craft.setBillWorking(1, true)" `shouldReturn` q "false"
            callStr ls "craft.addBillProgress(1, 0.5)" `shouldReturn` q "nil"
            callStr ls "craft.completeBillCycle(1)"    `shouldReturn` q "nil"
            viewBill env actorPage billOne   `shouldReturn` beforeA
            viewBill env visiblePage billOne `shouldReturn` beforeV

    describe "the ACTIVE-page family still targets the visible page" $ do

        it "getBills lists the visible page's queue, not the crafter's" $ \env → do
            resetPages env
            ls ← newBareLuaBackend env
            callStr ls "craft.getBills()[1].station" `shouldReturn` q "8"
            callStr ls "#craft.getBills(8)"          `shouldReturn` q "2"
            callStr ls "#craft.getBills(7)"          `shouldReturn` q "0"

        it "addBill queues on the visible page" $ \env → do
            resetPages env
            ls ← newBareLuaBackend env
            callStr ls "craft.addBill(8, 'page_smelt', 2)" `shouldReturn` q "3"
            billIdsOn env visiblePage `shouldReturn` [1, 2, 3]
            billIdsOn env actorPage   `shouldReturn` [1]
            -- And a station on the CRAFTER's page is refused, because it
            -- is not on the page the player is looking at.
            r ← runOk ls $ T.concat
                [ "local id, err = craft.addBill(7, 'page_smelt', 2); "
                , "return tostring(id) .. '|' .. tostring(err)" ]
            r `shouldBe` q "nil|station is on another world page"

        it "cancelBill removes the VISIBLE page's bill 1, and the \
           \crafter's survives" $ \env → do
            resetPages env
            ls ← newBareLuaBackend env
            callStr ls "craft.cancelBill(1)" `shouldReturn` q "true"
            billIdsOn env visiblePage `shouldReturn` [2]
            billIdsOn env actorPage   `shouldReturn` [1]

        it "setBillPaused pauses the VISIBLE page's bill 1" $ \env → do
            resetPages env
            ls ← newBareLuaBackend env
            beforeA ← viewBill env actorPage billOne
            callStr ls "craft.setBillPaused(1, true)" `shouldReturn` q "true"
            (bvPaused <$>) <$> viewBill env visiblePage billOne
                `shouldReturn` Just True
            viewBill env actorPage billOne `shouldReturn` beforeA

        it "reorderBill swaps the VISIBLE page's neighbours" $ \env → do
            resetPages env
            ls ← newBareLuaBackend env
            billSeqsOn env visiblePage `shouldReturn` [(1, 1), (2, 2)]
            callStr ls "craft.reorderBill(1, 'down')" `shouldReturn` q "true"
            billSeqsOn env visiblePage `shouldReturn` [(1, 2), (2, 1)]
            billSeqsOn env actorPage   `shouldReturn` [(1, 1)]
