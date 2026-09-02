-- | The LIVE Lua surface of the transfer contract (#1085, epic #1013
--   phase A2): @unit.checkTransfer@, @unit.commitTransfer@,
--   @unit.transferContract@ and @unit.transferEndpointInfo@ driven
--   through the REAL registered production API against REAL manager
--   refs.
--
--   Distinct from 'Test.Headless.Unit.Transfer', which exercises the
--   pure policy directly. Everything gated here is wiring the pure
--   suite structurally cannot see: named-request parsing, the signed
--   instance-id boundary, result encoding and ordering, and the four
--   live manager-mutation paths with their real 'IORef' writes.
--
--   Same bare-Lua-backend technique as
--   'Test.Headless.UI.TransferContextMenu' — a real Lua backend with
--   the full Lua API registered — but this spec WRITES to the engine's
--   unit/building/item manager refs, so 'Spec.hs' gives it its own
--   @aroundAll withHeadlessEngine@ block rather than the shared
--   worldgen engine.
--
--   The fixture primitives below are shared with
--   'Test.Headless.Unit.TransferOrderApi' (#1247), which needs the same
--   kind of scene at DIFFERENT geometry (a counterpart twenty tiles
--   away, a multi-tile footprint, a second page). One set of unit /
--   building / item constructors for both, rather than a second copy
--   free to drift from what the projections actually read.
--
--   Run just this gate: @cabal test synarchy-test-headless
--   --test-options='--match "Unit transfer Lua API"'@ — which reaches
--   the order spec too, by way of its own describe name.
module Test.Headless.Unit.TransferApi
    ( spec
    , minimalDef, storageDef, mkUnit, mkBuilding, mkItem
    , newBareLuaBackend, evalDebug
    ) where

import UPrelude
import Test.Hspec
import qualified Data.Map.Strict as Map
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import Data.IORef (modifyIORef', newIORef, readIORef, writeIORef)
import Building.Schema
import Building.Types
    ( BuildingDef(..), BuildingId(..), BuildingInstance(..)
    , BuildingManager(..), emptyBuildingManager )
import Engine.Asset.Handle (TextureHandle(..))
import Engine.Core.State (EngineEnv(..))
import Engine.Core.Thread (ThreadControl(..))
import Engine.Scripting.Lua.API (registerLuaAPI)
import Engine.Scripting.Lua.Thread (createLuaBackendState)
import Engine.Scripting.Lua.API.Units.Transfer
    (PushStep, commitCross, popBuilding, popUnit)
import Engine.Scripting.Lua.Thread.Console (executeDebugLua)
import Engine.Scripting.Lua.Types (LuaBackendState(..))
import Item.Types (ItemInstance(..), emptyItemManager)
import Unit.Transfer
    ( TransferEndpoint(..), TransferItemRef(..), TransferReason(..)
    , staleFailure )
import Unit.Direction (Direction(..))
import Unit.Faction (Faction(..))
import Unit.Types
    ( BodyPart(..), UnitDef(..), UnitId(..)
    , UnitInstance(..), UnitManager(..), defaultNaturalResistance
    , emptyUnitManager )
import World.Page.Types (WorldPageId(..))
import World.State.Types (WorldManager(..), emptyWorldState, emptyWorldManager)

-- * Fixture ids

acolyteUid ∷ UnitId
acolyteUid = UnitId 1

muleUid ∷ UnitId
muleUid = UnitId 2

wolfUid ∷ UnitId
wolfUid = UnitId 3

holdBid ∷ BuildingId
holdBid = BuildingId 7

depotBid ∷ BuildingId
depotBid = BuildingId 8

siteBid ∷ BuildingId
siteBid = BuildingId 9

fixturePage ∷ WorldPageId
fixturePage = WorldPageId "transfer_api_page"

-- * Fixtures

mkItem ∷ Text → Word64 → Float → ItemInstance
mkItem name iid w = ItemInstance
    { iiDefName     = name
    , iiCurrentFill = 0
    , iiQuality     = 100
    , iiCondition   = 100
    , iiWeight      = w
    , iiSharpness   = 100
    , iiContents    = []
    , iiInstanceId  = iid
    , iiTemp        = Nothing
    , iiBulk        = Just 1
    , iiStorage     = Nothing
    }

-- | Mirrors 'Test.Headless.Unit.Faction.minimalDef': only the fields
--   the transfer projection reads carry any weight here.
minimalDef ∷ Text → Text → UnitDef
minimalDef name display = UnitDef
    { udName = name, udNamePool = Nothing, udDisplayName = Just display
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

-- | A live unit at @(gx, gy)@ of the given faction, with a
--   @carrying_capacity@ stat, loose @inv@ and worn @worn@.
mkUnit ∷ Text → Faction → (Float, Float) → Float → [ItemInstance]
       → [ItemInstance] → UnitInstance
mkUnit defName f (gx, gy) cap inv worn = UnitInstance
    { uiDefName = defName, uiName = "", uiPage = fixturePage
    , uiTexture = TextureHandle 0, uiDirSprites = Map.empty
    , uiBaseWidth = 0, uiGridX = gx, uiGridY = gy, uiGridZ = 0
    , uiRealZ = 0, uiFacing = DirS
    , uiCurrentAnim = "", uiAnimStart = 0, uiAnimReverse = False
    , uiActivity = "idle", uiPose = "standing", uiAnimStride = 1
    , uiStats = HM.singleton "carrying_capacity" cap
    , uiModifiers = HM.empty, uiSkills = HM.empty
    , uiKnowledge = HM.empty, uiInventory = inv, uiEquipment = HM.empty
    , uiAccessories = worn, uiFactionId = f, uiWounds = []
    , uiScars = [], uiImmuneResponse = 0, uiImmunities = HM.empty
    , uiBlood = 5.0, uiLastAttackerUid = Nothing, uiLastAttackerAt = 0
    , uiAnimOverride = "", uiFrozen = False, uiForceLoop = False
    , uiClimbDest = Nothing, uiTrailState = Nothing
    }

-- | A storage def. @work@ 0 with no state animations reports Built the
--   instant it is spawned; a positive @work@ with zero progress stays
--   under construction (the same two branches
--   'Building.Types.currentActivity' takes).
storageDef ∷ Text → Text → (Int, Int) → Float → Float → BuildingDef
storageDef name display (w, h) work cap = BuildingDef
    { bdName = name, bdDisplayName = display, bdCategory = "Test"
    , bdDescription = "", bdTextures = legacyAssets (TextureHandle 0), bdIconTexture = TextureHandle 0
    , bdTileW = w, bdTileH = h, bdPlacement = "flat_ground"
    , bdIsStarting = False, bdRace = "acolyte_cult"
    , bdSpriteAnchor = "diamond_bottom", bdBuildWork = work
    , bdMaterials = HM.empty, bdStorageCapacity = cap
    , bdOperations = [], bdAnimations = HM.empty
    , bdRoleAnims = Map.empty
    , bdVisualClass     = FreestandingInstallation, bdPowerDrain = 0, bdPowerNode = Nothing
    }

mkBuilding ∷ Text → (Int, Int) → (Int, Int) → [ItemInstance] → BuildingInstance
mkBuilding defName (ax, ay) (w, h) stored = BuildingInstance
    { biDefName = defName, biPage = fixturePage, biTexture = TextureHandle 0
    , biAnchorX = ax, biAnchorY = ay, biGridZ = 0, biSpawnedAt = 0
    , biTileW = w, biTileH = h, biSpawnRemaining = 0, biBuildProgress = 0
    , biMaterialsDelivered = HM.empty, biStorage = stored
    }

-- | Reset all three manager refs to a known world:
--
--     * acolyte  uid 1 at (10, 10), player, capacity 100
--     * mule     uid 2 at (11, 11), player, capacity 250.5
--     * wolf     uid 3 at (11, 11), wildlife, capacity 100
--     * hold     bid 7 at (11, 10), 1x1, Built, capacity 200
--     * depot    bid 8 at (12, 10), 1x1, Built, capacity 200
--     * site     bid 9 at (10, 11), 1x1, UNDER CONSTRUCTION, capacity 200
--
--   Every pair among those is within Chebyshev 1 of the acolyte, so a
--   scenario only has to vary what it is actually testing.
resetWorld ∷ EngineEnv → [ItemInstance] → [ItemInstance] → [ItemInstance]
           → [ItemInstance] → IO ()
resetWorld env acolyteInv acolyteWorn holdStorage depotStorage = do
    -- A REAL page, because the #1087 container-knowledge store hangs off
    -- the WorldState the building's own biPage resolves to: without one
    -- registered, both revealContainerForUnit and
    -- building.getContainerKnowledge answer "no such page" and the
    -- reveal assertions below would pass vacuously. emptyWorldState is
    -- in-memory, so this costs no worldgen (the technique
    -- Test.Headless.Building.Knowledge's own scene uses).
    ws ← emptyWorldState
    writeIORef (worldManagerRef env) emptyWorldManager
        { wmWorlds = [(fixturePage, ws)], wmVisible = [fixturePage] }
    writeIORef (itemManagerRef env) emptyItemManager
    writeIORef (unitManagerRef env) emptyUnitManager
        { umDefs = HM.fromList
            [ ("acolyte", minimalDef "acolyte" "Acolyte")
            , ("technomule", minimalDef "technomule" "Technomule")
            , ("wolf", minimalDef "wolf" "Wolf") ]
        , umInstances = HM.fromList
            [ (acolyteUid, mkUnit "acolyte" FactionPlayer (10, 10) 100
                                  acolyteInv acolyteWorn)
            , (muleUid, mkUnit "technomule" FactionPlayer (11, 11) 250.5 [] [])
            , (wolfUid, mkUnit "wolf" FactionWildlife (11, 11) 100 [] []) ]
        }
    writeIORef (buildingManagerRef env) emptyBuildingManager
        { bmDefs = HM.fromList
            [ ("cargo_hold", storageDef "cargo_hold" "Cargo Hold" (1, 1) 0 200)
            , ("depot", storageDef "depot" "Depot" (1, 1) 0 200)
            , ("site", storageDef "site" "Build Site" (1, 1) 100 200) ]
        , bmInstances = HM.fromList
            [ (holdBid, mkBuilding "cargo_hold" (11, 10) (1, 1) holdStorage)
            , (depotBid, mkBuilding "depot" (12, 10) (1, 1) depotStorage)
            , (siteBid, mkBuilding "site" (10, 11) (1, 1) []) ]
        }

-- * Live-state readers

unitLoose ∷ EngineEnv → UnitId → IO [(Word64, Text)]
unitLoose env uid = do
    um ← readIORef (unitManagerRef env)
    pure $ case HM.lookup uid (umInstances um) of
        Nothing → []
        Just u  → [(iiInstanceId i, iiDefName i) | i ← uiInventory u]

buildingLoose ∷ EngineEnv → BuildingId → IO [(Word64, Text)]
buildingLoose env bid = do
    bm ← readIORef (buildingManagerRef env)
    pure $ case HM.lookup bid (bmInstances bm) of
        Nothing → []
        Just b  → [(iiInstanceId i, iiDefName i) | i ← biStorage b]

-- | Presence, which the *Loose readers deliberately cannot report: an
--   absent instance and a present-but-empty one both read as @[]@, so
--   #1274's "the source really is gone" assertion needs its own query.
unitPresent ∷ EngineEnv → UnitId → IO Bool
unitPresent env uid = do
    um ← readIORef (unitManagerRef env)
    pure (HM.member uid (umInstances um))

buildingPresent ∷ EngineEnv → BuildingId → IO Bool
buildingPresent env bid = do
    bm ← readIORef (buildingManagerRef env)
    pure (HM.member bid (bmInstances bm))

-- * Lua plumbing

newBareLuaBackend ∷ EngineEnv → IO LuaBackendState
newBareLuaBackend env = do
    ls ← createLuaBackendState (luaToEngineQueue env) (luaQueue env)
                                (assetPoolRef env) (nextObjectIdRef env)
                                (inputStateRef env) (loggerRef env)
    stateRef ← newIORef ThreadRunning
    registerLuaAPI (lbsLuaState ls) env ls stateRef
    loaded ← executeDebugLua (lbsLuaState ls) formatterLua
    loaded `shouldNotSatisfy` isLuaError
    pure ls

evalDebug ∷ LuaBackendState → Text → IO Text
evalDebug ls = executeDebugLua (lbsLuaState ls)

isLuaError ∷ Text → Bool
isLuaError t = "error:" `T.isPrefixOf` t ∨ "syntax error:" `T.isPrefixOf` t

-- | Debug-console return values come back JSON-encoded, so a Lua string
--   arrives quoted. Every assertion below compares a flat formatted
--   string rather than decoding JSON: an EMPTY Lua array serialises as
--   a JSON OBJECT, which would make @outcomes = {}@ ambiguous.
q ∷ Text → Text
q t = "\"" <> t <> "\""

-- | @__fmt(result)@ flattens a transfer result to
--   @"<completion>|<id>:<def>:<state>[:<reason>][/<cause>] …"@, or
--   @"rejected:<reason>"@ for a whole-request error, or @"nil"@.
--   @__ep(info)@ does the same for an endpoint-info table.
formatterLua ∷ Text
formatterLua = T.concat
    [ "_G.__fmt = function(r) "
    , "  if r == nil then return 'nil' end; "
    , "  if r.accepted ~= true then "
    , "    return 'rejected:' .. tostring(r.reason) "
    , "      .. '|' .. tostring(#(r.outcomes or {})) end; "
    , "  local parts = {}; "
    , "  for i, o in ipairs(r.outcomes) do "
    , "    parts[i] = tostring(o.instanceId) .. ':' .. tostring(o.defName) "
    , "      .. ':' .. tostring(o.state) "
    , "      .. (o.reason and (':' .. o.reason) or '') "
    , "      .. (o.cause and ('/' .. o.cause) or ''); "
    , "  end; "
    , "  return tostring(r.completion) .. '|' .. table.concat(parts, ' '); "
    , "end; "
    , "_G.__ep = function(info) "
    , "  if info == nil then return 'nil' end; "
    , "  local rows = {}; "
    , "  for i, it in ipairs(info.contents or {}) do "
    , "    rows[i] = tostring(it.instanceId) .. ':' .. tostring(it.defName) "
    , "      .. ':' .. string.format('%.1f', it.weight) "
    , "      .. ':' .. tostring(it.contentsKey ~= nil); "
    , "  end; "
    , "  return tostring(info.eligible) .. '|' .. tostring(info.displayName) "
    , "    .. '|' .. tostring(info.page) "
    , "    .. '|' .. tostring(info.gridX) .. ',' .. tostring(info.gridY) "
    , "    .. '|' .. string.format('%.1f', info.capacity) "
    , "    .. '|' .. string.format('%.1f', info.storedWeight) "
    , "    .. '|' .. table.concat(rows, ' '); "
    , "end; "
    , "return 'ok'"
    ]

-- | A request literal. @items@ is written out verbatim so a scenario
--   can express a malformed one too.
req ∷ Text → Int → Text → Int → Text → Text
req srcKind srcId dstKind dstId items = T.concat
    [ "{ source = { kind = '", srcKind, "', id = ", tshow srcId, " }, "
    , "destination = { kind = '", dstKind, "', id = ", tshow dstId, " }, "
    , "items = { ", items, " } }" ]

itemLit ∷ Int → Text → Text
itemLit iid defName =
    "{ instanceId = " <> tshow iid <> ", defName = '" <> defName <> "' }"

-- | A destination that always refuses AFTER the source has popped, so
--   'commitCross' has to run its rollback.
refusingPush ∷ PushStep
refusingPush _ _ _ = pure (Left ReasonReceiverFull)

-- | The same refusal, but it first DESTROYS the source — standing in
--   for @handleUnitDestroyCommand@ landing on another thread inside
--   'commitCross''s pop→restore window (#1274). Teardown deletes the
--   whole instance with its inventory, exactly as this does, so the
--   rollback that follows genuinely has nowhere to splice the item
--   back into.
vanishingUnitPush ∷ UnitId → PushStep
vanishingUnitPush uid env _ _ = do
    modifyIORef' (unitManagerRef env) $ \um →
        um { umInstances = HM.delete uid (umInstances um) }
    pure (Left ReasonReceiverFull)

-- | The building mirror: @BuildingDestroy@ removing the instance with
--   its whole 'biStorage'.
vanishingBuildingPush ∷ BuildingId → PushStep
vanishingBuildingPush bid env _ _ = do
    modifyIORef' (buildingManagerRef env) $ \bm →
        bm { bmInstances = HM.delete bid (bmInstances bm) }
    pure (Left ReasonReceiverFull)

check ∷ LuaBackendState → Text → IO Text
check ls request =
    evalDebug ls ("return _G.__fmt(unit.checkTransfer(" <> request <> "))")

commit ∷ LuaBackendState → Text → IO Text
commit ls request =
    evalDebug ls ("return _G.__fmt(unit.commitTransfer(" <> request <> "))")

endpointInfo ∷ LuaBackendState → Text → Int → IO Text
endpointInfo ls kind eid = evalDebug ls $ T.concat
    [ "return _G.__ep(unit.transferEndpointInfo({ kind = '", kind
    , "', id = ", tshow eid, " }))" ]

spec ∷ SpecWith EngineEnv
spec = describe "Unit transfer Lua API" $ do

    describe "named request parsing" $ do
        it "accepts the named request table and answers per item" $ \env → do
            resetWorld env [mkItem "ration" 101 0.5] [] [] []
            ls ← newBareLuaBackend env
            r  ← check ls (req "unit" 1 "building" 7 (itemLit 101 "ration"))
            r `shouldBe` q "all|101:ration:queued"

        it "returns nil (an ARGUMENT error) for a malformed call" $ \env → do
            resetWorld env [mkItem "ration" 101 0.5] [] [] []
            ls ← newBareLuaBackend env
            -- No table at all.
            r1 ← evalDebug ls "return _G.__fmt(unit.checkTransfer(42))"
            r1 `shouldBe` q "nil"
            -- Missing endpoint field.
            r2 ← evalDebug ls
                "return _G.__fmt(unit.checkTransfer({ destination = { kind = 'building', id = 7 }, items = {} }))"
            r2 `shouldBe` q "nil"
            -- Unknown endpoint kind.
            r3 ← check ls (req "ground" 1 "building" 7 (itemLit 101 "ration"))
            r3 `shouldBe` q "nil"
            -- Negative endpoint id must never wrap into a live id.
            r4 ← check ls (req "unit" (-1) "building" 7 (itemLit 101 "ration"))
            r4 `shouldBe` q "nil"
            -- Missing items array.
            r5 ← evalDebug ls
                "return _G.__fmt(unit.checkTransfer({ source = { kind = 'unit', id = 1 }, destination = { kind = 'building', id = 7 } }))"
            r5 `shouldBe` q "nil"
            -- A malformed item entry.
            r6 ← check ls (req "unit" 1 "building" 7 "{ defName = 'ration' }")
            r6 `shouldBe` q "nil"
            -- Wrong TYPES are malformed, not silently coerced: Lua
            -- would turn a number into the string "5" and the string
            -- "101" into an integer given half a chance.
            r7 ← check ls (req "unit" 1 "building" 7
                              "{ instanceId = 101, defName = 5 }")
            r7 `shouldBe` q "nil"
            r8 ← check ls (req "unit" 1 "building" 7
                              "{ instanceId = '101', defName = 'ration' }")
            r8 `shouldBe` q "nil"
            r9 ← evalDebug ls
                "return _G.__fmt(unit.checkTransfer({ source = { kind = 'unit', id = '1' }, destination = { kind = 'building', id = 7 }, items = {} }))"
            r9 `shouldBe` q "nil"

        it "rejects a SPARSE items table instead of silently dropping entries" $ \env → do
            -- rawlen returns a BORDER, not a count: { [1] = a, [3] = b }
            -- can report length 1, and a plain 1..n loop would then move
            -- `a`, drop `b` and emit no outcome for it — breaking the
            -- contract that accepted = true carries exactly one outcome
            -- per requested item. Which length a hole layout reports is
            -- a table-internals detail, so the shape is rejected whole.
            resetWorld env [mkItem "ration" 101 0.5, mkItem "ration" 103 0.5]
                           [] [] []
            ls ← newBareLuaBackend env
            let sparse = "[1] = " <> itemLit 101 "ration"
                       <> ", [3] = " <> itemLit 103 "ration"
            r ← check ls (req "unit" 1 "building" 7 sparse)
            r `shouldBe` q "nil"
            -- ...and it moved nothing on the way to saying so.
            r2 ← commit ls (req "unit" 1 "building" 7 sparse)
            r2 `shouldBe` q "nil"
            src ← unitLoose env acolyteUid
            dst ← buildingLoose env holdBid
            src `shouldBe` [(101, "ration"), (103, "ration")]
            dst `shouldBe` []

        it "rejects an items table carrying a stray associative key" $ \env → do
            resetWorld env [mkItem "ration" 101 0.5] [] [] []
            ls ← newBareLuaBackend env
            r ← check ls (req "unit" 1 "building" 7
                             (itemLit 101 "ration" <> ", extra = 1"))
            r `shouldBe` q "nil"

        it "still accepts a dense array of any length" $ \env → do
            let inv = [mkItem "ration" (fromIntegral i) 0.5 | i ← [101 .. 103 ∷ Int]]
            resetWorld env inv [] [] []
            ls ← newBareLuaBackend env
            let dense = T.intercalate ", " [itemLit i "ration" | i ← [101 .. 103]]
            r ← check ls (req "unit" 1 "building" 7 dense)
            r `shouldBe` q ("all|101:ration:queued 102:ration:queued"
                             <> " 103:ration:queued")

        it "keeps an argument error distinct from a policy refusal" $ \env → do
            resetWorld env [] [] [] []
            ls ← newBareLuaBackend env
            -- Structurally valid, policy-refused: a TABLE, not nil.
            r ← check ls (req "unit" 1 "building" 7 (itemLit 101 "ration"))
            r `shouldBe` q "none|101:ration:failed:instance_missing"

    describe "whole-request errors" $ do
        it "rejects an empty batch with no outcomes" $ \env → do
            resetWorld env [mkItem "ration" 101 0.5] [] [] []
            ls ← newBareLuaBackend env
            r ← check ls (req "unit" 1 "building" 7 "")
            r `shouldBe` q "rejected:empty_batch|0"

        it "rejects a duplicate instance id with no outcomes" $ \env → do
            resetWorld env [mkItem "ration" 101 0.5] [] [] []
            ls ← newBareLuaBackend env
            let items = T.intercalate ", "
                    [itemLit 101 "ration", itemLit 101 "ration"]
            r ← check ls (req "unit" 1 "building" 7 items)
            r `shouldBe` q "rejected:duplicate_instance|0"

        it "mutates nothing when the whole request is rejected" $ \env → do
            resetWorld env [mkItem "ration" 101 0.5] [] [] []
            ls ← newBareLuaBackend env
            let items = T.intercalate ", "
                    [itemLit 101 "ration", itemLit 101 "ration"]
            _      ← commit ls (req "unit" 1 "building" 7 items)
            before ← unitLoose env acolyteUid
            stored ← buildingLoose env holdBid
            before `shouldBe` [(101, "ration")]
            stored `shouldBe` []

    describe "the signed instance-id boundary" $ do
        it "reports a zero id as instance_unspecified, per item" $ \env → do
            resetWorld env [mkItem "ration" 101 0.5] [] [] []
            ls ← newBareLuaBackend env
            let items = T.intercalate ", "
                    [itemLit 0 "ration", itemLit 101 "ration"]
            r ← check ls (req "unit" 1 "building" 7 items)
            r `shouldBe` q ("partial|0:ration:failed:instance_unspecified"
                            <> " 101:ration:queued")

        it "reports a NEGATIVE id as instance_unspecified, never a wrap" $ \env → do
            -- readRequest keeps the SIGNED value: converting -1 to
            -- Word64 first would produce 18446744073709551615 and read
            -- as an ordinary (merely missing) instance id.
            resetWorld env [mkItem "ration" 101 0.5] [] [] []
            ls ← newBareLuaBackend env
            let items = T.intercalate ", "
                    [itemLit (-1) "ration", itemLit 101 "ration"]
            r ← check ls (req "unit" 1 "building" 7 items)
            r `shouldBe` q ("partial|-1:ration:failed:instance_unspecified"
                            <> " 101:ration:queued")

    describe "result encoding" $ do
        it "reports outcomes in request order with all/partial/none" $ \env → do
            resetWorld env [mkItem "ration" 101 0.5, mkItem "ration" 102 0.5]
                           [] [] []
            ls ← newBareLuaBackend env
            let both = T.intercalate ", "
                    [itemLit 101 "ration", itemLit 102 "ration"]
                mixed = T.intercalate ", "
                    [itemLit 102 "ration", itemLit 999 "ration"]
                neither = T.intercalate ", "
                    [itemLit 998 "ration", itemLit 999 "ration"]
            rAll ← check ls (req "unit" 1 "building" 7 both)
            rAll `shouldBe` q "all|101:ration:queued 102:ration:queued"
            rPart ← check ls (req "unit" 1 "building" 7 mixed)
            rPart `shouldBe` q ("partial|102:ration:queued"
                                 <> " 999:ration:failed:instance_missing")
            rNone ← check ls (req "unit" 1 "building" 7 neither)
            rNone `shouldBe` q ("none|998:ration:failed:instance_missing"
                                 <> " 999:ration:failed:instance_missing")

        it "encodes the direct reason, with no stale cause attached" $ \env → do
            -- 'cause' is populated only for became_stale; a create-time
            -- or commit-time refusal reports its own reason bare.
            resetWorld env [mkItem "anvil" 101 500.0] [] [] []
            ls ← newBareLuaBackend env
            r ← check ls (req "unit" 1 "building" 7 (itemLit 101 "anvil"))
            r `shouldBe` q "none|101:anvil:failed:receiver_full"

        it "reports one outcome per requested item, even for never-attempted ones" $ \env → do
            resetWorld env [mkItem "ration" 101 0.5] [] [] []
            ls ← newBareLuaBackend env
            let items = T.intercalate ", "
                    [ itemLit 999 "ration", itemLit 101 "ration"
                    , itemLit 0 "ration" ]
            r ← commit ls (req "unit" 1 "building" 7 items)
            r `shouldBe` q ("partial|999:ration:failed:instance_missing"
                             <> " 101:ration:completed"
                             <> " 0:ration:failed:instance_unspecified")

    describe "contract vocabulary" $ do
        it "publishes endpoint kinds as a NAMED set, not an array" $ \env → do
            ls ← newBareLuaBackend env
            r  ← evalDebug ls $ T.concat
                [ "local c = unit.transferContract(); "
                , "return tostring(c.endpointKinds.unit) .. '|' "
                , ".. tostring(c.endpointKinds.building) .. '|' "
                , ".. tostring(c.endpointKinds[1]) .. '|' "
                , ".. tostring(c.operations)" ]
            r `shouldBe` q "true|true|nil|nil"

        it "advertises the whole-request error ids alongside the per-item ones" $ \env → do
            ls ← newBareLuaBackend env
            r  ← evalDebug ls $ T.concat
                [ "local c = unit.transferContract(); "
                , "return table.concat(c.requestErrors, ',') .. '|' "
                , ".. table.concat(c.states, ',')" ]
            r `shouldBe` q ("empty_batch,duplicate_instance|queued,in_transit,"
                             <> "ready_to_commit,completed,cancelled,failed")

        it "no longer advertises quantity or operation-mismatch reasons" $ \env → do
            ls ← newBareLuaBackend env
            r  ← evalDebug ls $ T.concat
                [ "local c = unit.transferContract(); "
                , "local s = ',' .. table.concat(c.reasons, ',') .. ','; "
                , "return tostring(s:find(',quantity_unsupported,', 1, true) ~= nil) "
                , ".. '|' .. tostring(s:find(',operation_mismatch,', 1, true) ~= nil) "
                , ".. '|' .. tostring(s:find(',source_ineligible,', 1, true) ~= nil)" ]
            r `shouldBe` q "false|false|true"

    describe "endpoint info" $ do
        it "projects a building endpoint's capacity, weight and ordered contents" $ \env → do
            resetWorld env [] []
                [mkItem "ration" 201 0.5, mkItem "steel_bar" 202 2.5] []
            ls ← newBareLuaBackend env
            r  ← endpointInfo ls "building" 7
            r `shouldBe` q ("true|Cargo Hold|transfer_api_page|11,10"
                             <> "|200.0|3.0"
                             <> "|201:ration:0.5:true 202:steel_bar:2.5:true")

        it "projects a unit endpoint, counting worn gear in the weight only" $ \env → do
            -- The capacity gate measures inventory + equipment +
            -- accessories; the CONTENTS list is loose inventory alone.
            resetWorld env [mkItem "ration" 101 0.5]
                           [mkItem "acolyte_robe" 111 1.5] [] []
            ls ← newBareLuaBackend env
            r  ← endpointInfo ls "unit" 1
            r `shouldBe` q ("true|Acolyte|transfer_api_page|10,10"
                             <> "|100.0|2.0|101:ration:0.5:true")

        it "reports an under-construction building as ineligible" $ \env → do
            resetWorld env [] [] [] []
            ls ← newBareLuaBackend env
            r  ← endpointInfo ls "building" 9
            r `shouldBe` q "false|Build Site|transfer_api_page|10,11|200.0|0.0|"

        it "reports a non-commandable unit as ineligible" $ \env → do
            resetWorld env [] [] [] []
            ls ← newBareLuaBackend env
            r  ← endpointInfo ls "unit" 3
            r `shouldBe` q "false|Wolf|transfer_api_page|11,11|100.0|0.0|"

        it "reports an ordinary player acolyte as ELIGIBLE (A2's widening)" $ \env → do
            -- With the transfer_receiver marker gone, an empty-handed
            -- acolyte is as valid an endpoint as the technomule.
            resetWorld env [] [] [] []
            ls ← newBareLuaBackend env
            r  ← endpointInfo ls "unit" 1
            r `shouldBe` q "true|Acolyte|transfer_api_page|10,10|100.0|0.0|"

        it "returns nil for an unknown kind, a bad id, or a dead endpoint" $ \env → do
            resetWorld env [] [] [] []
            ls ← newBareLuaBackend env
            r1 ← endpointInfo ls "ground" 1
            r2 ← endpointInfo ls "unit" 4242
            r3 ← endpointInfo ls "unit" (-1)
            r4 ← evalDebug ls "return _G.__ep(unit.transferEndpointInfo('unit', 1))"
            [r1, r2, r3, r4] `shouldBe` replicate 4 (q "nil")

    describe "live manager mutation" $ do
        it "moves an instance unit → building storage" $ \env → do
            resetWorld env [mkItem "ration" 101 0.5, mkItem "steel_bar" 102 2.5]
                           [] [] []
            ls ← newBareLuaBackend env
            r  ← commit ls (req "unit" 1 "building" 7 (itemLit 101 "ration"))
            r `shouldBe` q "all|101:ration:completed"
            src ← unitLoose env acolyteUid
            dst ← buildingLoose env holdBid
            src `shouldBe` [(102, "steel_bar")]
            dst `shouldBe` [(101, "ration")]

        it "moves an instance building storage → unit" $ \env → do
            resetWorld env [] [] [mkItem "ration" 201 0.5] []
            ls ← newBareLuaBackend env
            r  ← commit ls (req "building" 7 "unit" 1 (itemLit 201 "ration"))
            r `shouldBe` q "all|201:ration:completed"
            src ← buildingLoose env holdBid
            dst ← unitLoose env acolyteUid
            src `shouldBe` []
            dst `shouldBe` [(201, "ration")]

        it "moves an instance unit → unit" $ \env → do
            resetWorld env [mkItem "ration" 101 0.5] [] [] []
            ls ← newBareLuaBackend env
            r  ← commit ls (req "unit" 1 "unit" 2 (itemLit 101 "ration"))
            r `shouldBe` q "all|101:ration:completed"
            src ← unitLoose env acolyteUid
            dst ← unitLoose env muleUid
            src `shouldBe` []
            dst `shouldBe` [(101, "ration")]

        it "moves an instance building storage → building storage" $ \env → do
            resetWorld env [] [] [mkItem "ration" 201 0.5] []
            ls ← newBareLuaBackend env
            r  ← commit ls (req "building" 7 "building" 8 (itemLit 201 "ration"))
            r `shouldBe` q "all|201:ration:completed"
            src ← buildingLoose env holdBid
            dst ← buildingLoose env depotBid
            src `shouldBe` []
            dst `shouldBe` [(201, "ration")]

        it "refuses a non-commandable unit DESTINATION, mutating nothing" $ \env → do
            resetWorld env [mkItem "ration" 101 0.5] [] [] []
            ls ← newBareLuaBackend env
            r  ← commit ls (req "unit" 1 "unit" 3 (itemLit 101 "ration"))
            r `shouldBe` q "none|101:ration:failed:receiver_ineligible"
            src  ← unitLoose env acolyteUid
            wolf ← unitLoose env wolfUid
            src `shouldBe` [(101, "ration")]
            wolf `shouldBe` []

        it "refuses a non-commandable unit SOURCE with its own reason" $ \env → do
            -- The wolf's own stash is not the player's to move, and the
            -- refusal names the SOURCE side rather than reusing the
            -- receiver family.
            resetWorld env [] [] [] []
            ls ← newBareLuaBackend env
            r  ← commit ls (req "unit" 3 "unit" 1 (itemLit 301 "ration"))
            r `shouldBe` q "none|301:ration:failed:source_ineligible"

        it "refuses an under-construction building source as source-ineligible" $ \env → do
            resetWorld env [] [] [] []
            ls ← newBareLuaBackend env
            r  ← commit ls (req "building" 9 "unit" 1 (itemLit 401 "ration"))
            r `shouldBe` q "none|401:ration:failed:source_ineligible"

        it "keeps the FIRST items of an over-capacity batch, in order" $ \env → do
            -- The hold has 3 kg of room and the batch wants 5 kg: the
            -- first three land, the rest report receiver_full, and the
            -- source keeps exactly the ones that did not move.
            let inv = [mkItem "ration" (fromIntegral i) 1.0 | i ← [101 .. 105 ∷ Int]]
            resetWorld env inv [] [mkItem "ballast" 290 197.0] []
            ls ← newBareLuaBackend env
            let items = T.intercalate ", "
                    [itemLit i "ration" | i ← [101 .. 105]]
            r ← commit ls (req "unit" 1 "building" 7 items)
            r `shouldBe` q ("partial|101:ration:completed 102:ration:completed"
                             <> " 103:ration:completed"
                             <> " 104:ration:failed:receiver_full"
                             <> " 105:ration:failed:receiver_full")
            src ← unitLoose env acolyteUid
            dst ← buildingLoose env holdBid
            src `shouldBe` [(104, "ration"), (105, "ration")]
            -- Building storage prepends, so the newest is first.
            dst `shouldBe` [ (103, "ration"), (102, "ration")
                           , (101, "ration"), (290, "ballast") ]

        it "removes only the named instances, preserving source order" $ \env → do
            -- The middle item is WORN, so it is refused while its
            -- neighbours move: the remaining loose list must still be
            -- in its original relative order with nothing shifted onto
            -- the wrong index.
            let inv = [ mkItem "ration" 101 0.5, mkItem "canteen" 102 0.5
                      , mkItem "steel_bar" 103 2.5, mkItem "rope" 104 1.0 ]
            resetWorld env inv [mkItem "acolyte_robe" 111 1.5] [] []
            ls ← newBareLuaBackend env
            let items = T.intercalate ", "
                    [ itemLit 101 "ration", itemLit 111 "acolyte_robe"
                    , itemLit 103 "steel_bar" ]
            r ← commit ls (req "unit" 1 "building" 7 items)
            r `shouldBe` q ("partial|101:ration:completed"
                             <> " 111:acolyte_robe:failed:item_not_transferable"
                             <> " 103:steel_bar:completed")
            src ← unitLoose env acolyteUid
            dst ← buildingLoose env holdBid
            src `shouldBe` [(102, "canteen"), (104, "rope")]
            dst `shouldBe` [(103, "steel_bar"), (101, "ration")]

        it "leaves both endpoints byte-identical when every item is refused" $ \env → do
            -- The depot at (12, 10) is Chebyshev 2 from the acolyte at
            -- (10, 10), so BOTH cross-manager directions are out of
            -- range and neither side may be touched.
            let inv    = [ mkItem "ration" 101 0.5, mkItem "canteen" 102 0.5
                         , mkItem "steel_bar" 103 2.5 ]
                stored = [mkItem "rope" 201 1.0]
            resetWorld env inv [] [] stored
            ls ← newBareLuaBackend env
            out ← commit ls (req "unit" 1 "building" 8 (itemLit 101 "ration"))
            out `shouldBe` q "none|101:ration:failed:out_of_range"
            back ← commit ls (req "building" 8 "unit" 1 (itemLit 201 "rope"))
            back `shouldBe` q "none|201:rope:failed:out_of_range"
            srcU ← unitLoose env acolyteUid
            srcB ← buildingLoose env depotBid
            srcU `shouldBe` [(101, "ration"), (102, "canteen"), (103, "steel_bar")]
            srcB `shouldBe` [(201, "rope")]

        -- The rollback branch of the cross-manager path. It cannot be
        -- reached through unit.commitTransfer in a single-threaded
        -- test: the plan and the push re-read the same refs inside one
        -- call and therefore always agree, so the branch exists for a
        -- genuinely concurrent mutation. Driving 'commitCross' with the
        -- REAL pop/restore and a stub push is what makes the guarantee
        -- gateable at all.
        it "restores a popped UNIT item at its ORIGINAL index when the push fails" $ \env → do
            let inv = [ mkItem "ration" 101 0.5, mkItem "canteen" 102 0.5
                      , mkItem "steel_bar" 103 2.5 ]
            resetWorld env inv [] [] []
            r ← commitCross env (EndpointUnit acolyteUid)
                                (EndpointBuilding holdBid)
                                (popUnit acolyteUid) refusingPush
                                (TransferItemRef 102 "canteen")
            fmap iiInstanceId r `shouldBe`
                Left (staleFailure ReasonReceiverFull)
            src ← unitLoose env acolyteUid
            -- Spliced back at index 1, not appended.
            src `shouldBe` [ (101, "ration"), (102, "canteen")
                           , (103, "steel_bar") ]
            dst ← buildingLoose env holdBid
            dst `shouldBe` []

        it "restores a popped BUILDING item at its ORIGINAL index when the push fails" $ \env → do
            let stored = [ mkItem "ration" 201 0.5, mkItem "canteen" 202 0.5
                         , mkItem "steel_bar" 203 2.5 ]
            resetWorld env [] [] stored []
            r ← commitCross env (EndpointBuilding holdBid)
                                (EndpointUnit acolyteUid)
                                (popBuilding holdBid) refusingPush
                                (TransferItemRef 202 "canteen")
            fmap iiInstanceId r `shouldBe`
                Left (staleFailure ReasonReceiverFull)
            src ← buildingLoose env holdBid
            src `shouldBe` [ (201, "ration"), (202, "canteen")
                           , (203, "steel_bar") ]
            dst ← unitLoose env acolyteUid
            dst `shouldBe` []

        -- #1274: the SAME rollback branch, with the source torn down
        -- between the pop and the restore. The item is consumed with
        -- the source — teardown deletes a unit's whole uiInventory and
        -- a building's whole biStorage without spilling either, so an
        -- item in flight ends up where the rest of the contents did.
        -- What must not happen is reporting the destination's refusal
        -- as though the splice-back had completed. Both source kinds
        -- have their own restoration branch, so both are gated.
        it "reports source_missing when the UNIT source vanishes mid-rollback (#1274)" $ \env → do
            let inv = [ mkItem "ration" 101 0.5, mkItem "canteen" 102 0.5
                      , mkItem "steel_bar" 103 2.5 ]
            resetWorld env inv [] [] []
            r ← commitCross env (EndpointUnit acolyteUid)
                                (EndpointBuilding holdBid)
                                (popUnit acolyteUid)
                                (vanishingUnitPush acolyteUid)
                                (TransferItemRef 102 "canteen")
            -- Precedence: the source-side truth OVERRIDES the
            -- destination's receiver_full, which the surviving-source
            -- case above still returns.
            fmap iiInstanceId r `shouldBe`
                Left (staleFailure ReasonSourceMissing)
            present ← unitPresent env acolyteUid
            present `shouldBe` False
            dst ← buildingLoose env holdBid
            dst `shouldBe` []

        it "reports source_missing when the BUILDING source vanishes mid-rollback (#1274)" $ \env → do
            let stored = [ mkItem "ration" 201 0.5, mkItem "canteen" 202 0.5
                         , mkItem "steel_bar" 203 2.5 ]
            resetWorld env [] [] stored []
            r ← commitCross env (EndpointBuilding holdBid)
                                (EndpointUnit acolyteUid)
                                (popBuilding holdBid)
                                (vanishingBuildingPush holdBid)
                                (TransferItemRef 202 "canteen")
            fmap iiInstanceId r `shouldBe`
                Left (staleFailure ReasonSourceMissing)
            present ← buildingPresent env holdBid
            present `shouldBe` False
            dst ← unitLoose env acolyteUid
            dst `shouldBe` []

        -- A3 (#1087) reveals a container's contents to the player when
        -- one of their units interacts with it, and hooked the strict
        -- transfer path alongside depositToCargo/withdrawFromCargo. That
        -- hook lived inside the unit->building function A2 replaced, so
        -- nothing but these cases stops a later rewrite dropping it.
        it "a successful deposit reveals the container's contents (#1087)" $ \env → do
            resetWorld env [mkItem "ration" 101 0.5] [] [] []
            ls ← newBareLuaBackend env
            before ← evalDebug ls
                "local k = building.getContainerKnowledge(7); return k and k.state or 'nil'"
            before `shouldBe` q "unknown"
            r ← commit ls (req "unit" 1 "building" 7 (itemLit 101 "ration"))
            r `shouldBe` q "all|101:ration:completed"
            after ← evalDebug ls $ T.concat
                [ "local k = building.getContainerKnowledge(7); "
                , "return tostring(k.state) .. '|' .. tostring(#k.items) "
                , ".. '|' .. tostring(k.items[1] and k.items[1].instanceId)" ]
            after `shouldBe` q "known|1|101"

        it "a successful WITHDRAWAL reveals it too (#1087 covers both directions)" $ \env → do
            resetWorld env [] [] [mkItem "ration" 201 0.5, mkItem "rope" 202 1.0] []
            ls ← newBareLuaBackend env
            r ← commit ls (req "building" 7 "unit" 1 (itemLit 201 "ration"))
            r `shouldBe` q "all|201:ration:completed"
            -- Snapshots the FINAL post-commit storage: the withdrawn
            -- ration is gone, the rope remains.
            after ← evalDebug ls $ T.concat
                [ "local k = building.getContainerKnowledge(7); "
                , "return tostring(k.state) .. '|' .. tostring(#k.items) "
                , ".. '|' .. tostring(k.items[1] and k.items[1].instanceId)" ]
            after `shouldBe` q "known|1|202"

        it "a REFUSED transfer reveals nothing" $ \env → do
            -- The reveal is on the success branch only, so a refusal
            -- (and the rollback behind it) leaves the record untouched.
            resetWorld env [mkItem "anvil" 101 500.0] [] [] []
            ls ← newBareLuaBackend env
            r ← commit ls (req "unit" 1 "building" 7 (itemLit 101 "anvil"))
            r `shouldBe` q "none|101:anvil:failed:receiver_full"
            after ← evalDebug ls
                "local k = building.getContainerKnowledge(7); return k and k.state or 'nil'"
            after `shouldBe` q "unknown"

        it "a non-commandable unit's transfer reveals nothing" $ \env → do
            -- revealContainerForUnit gates on the ACTING unit, so a
            -- wolf rummaging teaches the player nothing.
            resetWorld env [] [] [mkItem "ration" 201 0.5] []
            ls ← newBareLuaBackend env
            r ← commit ls (req "building" 7 "unit" 3 (itemLit 201 "ration"))
            r `shouldBe` q "none|201:ration:failed:receiver_ineligible"
            after ← evalDebug ls
                "local k = building.getContainerKnowledge(7); return k and k.state or 'nil'"
            after `shouldBe` q "unknown"

        it "mutates nothing at all from checkTransfer" $ \env → do
            resetWorld env [mkItem "ration" 101 0.5, mkItem "steel_bar" 102 2.5]
                           [] [mkItem "rope" 201 1.0] []
            ls ← newBareLuaBackend env
            r ← check ls (req "unit" 1 "building" 7
                              (T.intercalate ", " [ itemLit 101 "ration"
                                                  , itemLit 102 "steel_bar" ]))
            r `shouldBe` q "all|101:ration:queued 102:steel_bar:queued"
            src ← unitLoose env acolyteUid
            dst ← buildingLoose env holdBid
            src `shouldBe` [(101, "ration"), (102, "steel_bar")]
            dst `shouldBe` [(201, "rope")]
