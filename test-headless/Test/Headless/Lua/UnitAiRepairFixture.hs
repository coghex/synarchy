{-# LANGUAGE OverloadedStrings #-}
-- | The scene, unit and Lua plumbing the repair AI's headless specs share.
--
--   Extracted from "Test.Headless.Lua.UnitAiRepairGround" (#1737) so the
--   sourcing, ordering and claim-lifecycle groups of
--   "Test.Headless.Lua.UnitAiRepairSourcing" (#2939) run against the same
--   two-page world and the same production
--   @scripts/unit_ai_repair.lua@ \/ @unit_ai_repair_target.lua@ the ground
--   rung does, instead of a second copy of it. Nothing here asserts; the
--   groups own every expectation.
--
--   What it replaces is what the groups are not about: @movement_speed@,
--   the @scripts.unit_ai@ singleton and the @building@ \/ @repair@
--   station surface (see 'loadRepair'). @repair.repairAt@'s own contract
--   (station↔axis mapping, ingredient cost, restore) has its own gates:
--   @tools/repair_probe.py@ and @tools/repair_item_probe.py@.
module Test.Headless.Lua.UnitAiRepairFixture where

import UPrelude
import Test.Hspec
import qualified Data.Map.Strict as Map
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import qualified Engine.Core.Queue as Q
import Data.List (sortOn)
import Data.IORef (newIORef, readIORef, writeIORef, atomicModifyIORef')
import Engine.Asset.Handle (TextureHandle(..))
import Engine.Core.State (EngineEnv(..))
import Engine.Core.Thread (ThreadControl(..))
import Engine.Scripting.Lua.API (registerLuaAPI)
import Engine.Scripting.Lua.Thread (createLuaBackendState)
import Engine.Scripting.Lua.Thread.Console (executeDebugLua)
import Engine.Scripting.Lua.Types (LuaBackendState(..))
import Item.Ground (GroundItem(..), GroundItems(..), spawnGroundItem)
import Item.Types
    ( ItemDef(..), ItemInstance(..), ItemManager(..) )
import Unit.Direction (Direction(..))
import Unit.Faction (Faction(..))
import Unit.Faction.Membership (resolveLegacyFaction)
import Unit.Types
    ( UnitDef(..), UnitId(..), UnitInstance(..)
    , UnitManager(..), defaultNaturalResistance, emptyUnitManager , noUnitPhysiology, emptyPhysioOverrides)
import World.Page.Types (WorldPageId(..))
import World.State.Types
    ( WorldManager(..), WorldState(..), emptyWorldState, emptyWorldManager )

-- * Fixture identities

-- | ACTIVE throughout. Its ground is the bystander: a decoy at the same
--   gid the worker's own page uses, so an active-page read is off by an
--   entire world rather than by a rounding error.
pageActive ∷ WorldPageId
pageActive = WorldPageId "repair_ground_active"

-- | Live but NOT active — where the worker, the mule and the target are.
pageOwned ∷ WorldPageId
pageOwned = WorldPageId "repair_ground_owned"

workerUid, rivalUid, muleUid ∷ UnitId
workerUid = UnitId 1
rivalUid  = UnitId 2
muleUid   = UnitId 3

-- | Where the worker starts. One tile from 'nearAt', so the arrival
--   gate (@pickup_arrival_tiles@ = 1.2 in the fixture params) is
--   already satisfied and a case can reach the pickup in one tick.
startAt ∷ (Float, Float)
startAt = (10, 10)

-- | Within the arrival radius of 'startAt'.
nearAt ∷ (Float, Float)
nearAt = (10.5, 10)

-- | Inside @repair_scan_range@ but well outside the arrival radius, so
--   a candidate here must WALK before it can be taken.
walkAt ∷ (Float, Float)
walkAt = (22, 10)

-- | Outside @repair_scan_range@ (30 in the fixture params).
farAt ∷ (Float, Float)
farAt = (90, 10)

-- * Item fixtures

-- | Kinds are load-bearing: @repairSeverity@ reads @it.kind@ to pick
--   the broken-ARMOUR band over the broken-WEAPON one, and that field
--   reaches a ground row only because @pushGroundRow@ resolves the def.
--   @wool_gambeson@ additionally declares a quality spec so the
--   @qualityTier@-versus-@sharpness@ distinction has a subject.
itemDefs ∷ ItemManager
itemDefs = ItemManager $ HM.fromList
    [ ("wool_gambeson", (bareDef "wool_gambeson" "armor" 1.8)
                          { idQualitySpec = Just (70, 95) })
    , ("axe_steel",     bareDef "axe_steel" "weapon" 1.0)
    , ("steel_anvil",   bareDef "steel_anvil" "misc" 60.0)
    , ("whetstone",     bareDef "whetstone" "misc" 0.3)
    , ("lignite_chunk", bareDef "lignite_chunk" "misc" 0.5)
    ]

bareDef ∷ Text → Text → Float → ItemDef
bareDef name kind w = ItemDef
    { idName = name, idDisplayName = name, idTexture = TextureHandle 0, idIconTexture = TextureHandle 0
    , idWeight = w, idWeightSpec = Nothing, idBulk = 1.0
    , idStorage = Nothing, idKind = kind
    , idCategory = "Misc", idMake = "", idMaterial = ""
    , idQualitySpec = Nothing, idQualityTiers = []
    , idContainer = Nothing, idDefaultContents = []
    , idFood = Nothing, idWeapon = Nothing, idArmor = Nothing
    , idUnequippable = False, idBuffs = [], idInsulation = 0
    , idSourcePath = "test-fixture"
    }

-- | @condition@ and @sharpness@ are the two wear axes severity scores;
--   @iiWeight@ is what the row reports as its live @weight@ and so what
--   both capacity gates weigh.
mkItem ∷ Text → Word64 → Float → Float → Float → ItemInstance
mkItem name iid cond sharp w = ItemInstance
    { iiDefName     = name
    , iiCurrentFill = 0
    , iiQuality     = 88
    , iiCondition   = cond
    , iiWeight      = w
    , iiSharpness   = sharp
    , iiContents    = []
    , iiInstanceId  = iid
    , iiTemp        = Nothing
    , iiBulk        = Just 1
    , iiStorage     = Nothing
    }

-- * Unit fixtures

minimalDef ∷ Text → UnitDef
minimalDef name = UnitDef
    { udName = name, udNamePool = Nothing, udDisplayName = Just name
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
    , udNaturalWeapon = Nothing, udModifiers = [] , udFactionTags = [], udPhysiology = noUnitPhysiology }

mkUnit ∷ Text → WorldPageId → (Float, Float) → Float → [ItemInstance]
       → UnitInstance
mkUnit defName pg (gx, gy) capacity inv = UnitInstance
    { uiDefName = defName, uiName = "Nael", uiPage = pg
    , uiTexture = TextureHandle 0, uiDirSprites = Map.empty
    , uiBaseWidth = 0, uiGridX = gx, uiGridY = gy, uiGridZ = 0
    , uiRealZ = 0, uiFacing = DirS
    , uiCurrentAnim = "", uiAnimStart = 0, uiAnimReverse = False
    , uiActivity = "idle", uiPose = "standing", uiAnimStride = 1
    , uiStats = HM.singleton "carrying_capacity" capacity
    , uiModifiers = HM.empty, uiSkills = HM.empty
    , uiKnowledge = HM.empty, uiInventory = inv, uiEquipment = HM.empty
    , uiAccessories = [], uiFaction = resolveLegacyFaction [] FactionPlayer, uiWounds = []
    , uiScars = [], uiImmuneResponse = 0, uiImmunities = HM.empty
    , uiBlood = 5.0, uiLastAttackerUid = Nothing, uiLastAttackerAt = 0
    , uiAnimOverride = "", uiFrozen = False, uiForceLoop = False
    , uiClimbDest = Nothing, uiPhysioOverrides = emptyPhysioOverrides, uiTrailState = Nothing
    }

-- | Everything a case can vary, so each one reads as a scenario rather
--   than as a wall of manager surgery.
data Setup = Setup
    { suActiveGround ∷ [(ItemInstance, (Float, Float))]
    , suOwnedGround  ∷ [(ItemInstance, (Float, Float))]
    , suWorkerInv    ∷ [ItemInstance]
    , suWorkerCap    ∷ Float
    , suWorkerAt     ∷ (Float, Float)
    , suRival        ∷ Bool                -- ^ a second live worker
    , suMuleInv      ∷ Maybe [ItemInstance]
    , suWorkerEquip  ∷ [(Text, ItemInstance)]
      -- ^ equipment slot → instance, carried by the worker
    , suUnitsActive  ∷ Bool
      -- ^ the units live on the ACTIVE page instead of the non-active
      --   one. @unit.getAllIds@ lists the active page only, so
      --   'findTechnomule' can reach a mule only from there; every
      --   mule scenario sets it, and the ground fixtures leave it off so
      --   the page asymmetry they are about stays in force.
    }

baseSetup ∷ Setup
baseSetup = Setup
    { suActiveGround = [], suOwnedGround = []
    , suWorkerInv = [], suWorkerCap = 100, suWorkerAt = startAt
    , suRival = False, suMuleInv = Nothing
    , suWorkerEquip = [], suUnitsActive = False }

data Scene = Scene { scActive ∷ WorldState, scOwned ∷ WorldState }

-- | An ACTIVE-page decoy: DEGRADED, so an implementation that scored
--   the LISTED row instead of the RESOLVED one would find a candidate
--   here, and sitting where the worker already stands, so it would even
--   pass the arrival gate. Its instance ids start at 9000 so a failure
--   naming one is unambiguous.
decoy ∷ Int → (ItemInstance, (Float, Float))
decoy n = (mkItem "steel_anvil" (fromIntegral (9000 + n)) 3 3 60, nearAt)

-- | Two live pages, ACTIVE first, the worker on the non-active one.
--   Each page's own allocator hands out ids from 0, so a same-numbered
--   gid on both pages is the DEFAULT here rather than something the
--   fixture contrives.
--
--   The ACTIVE page is PADDED to at least the owned page's row count,
--   because enumeration is active-page scoped and resolution is not.
--   That asymmetry is #1666's deliberate design — @item.listGround@ is
--   "the world the player is looking at", there is no owning-page
--   LISTING, and @scripts/unit_ai_fetch.lua@'s ground rung has the same
--   shape — and it is honest here because the AI only ever ticks for
--   units on the active page, where the two id sets are the same set.
--   Padding reproduces that without giving up the two-page fixture, and
--   every padded row is a degraded decoy the scan must never select.
resetScene ∷ EngineEnv → Setup → IO Scene
resetScene env su = do
    wsA ← emptyWorldState
    wsO ← emptyWorldState
    let unitPage = if suUnitsActive su then pageActive else pageOwned
        explicit = suActiveGround su
        padding  = [ decoy n
                   | n ← [1 .. length (suOwnedGround su) - length explicit] ]
    forM_ (explicit <> padding) $ \(it, (x, y)) →
        atomicModifyIORef' (wsGroundItemsRef wsA) $ spawnGroundItem it x y
    forM_ (suOwnedGround su) $ \(it, (x, y)) →
        atomicModifyIORef' (wsGroundItemsRef wsO) $ spawnGroundItem it x y
    writeIORef (worldManagerRef env) emptyWorldManager
        { wmWorlds  = [(pageActive, wsA), (pageOwned, wsO)]
        , wmVisible = [pageActive] }
    writeIORef (itemManagerRef env) itemDefs
    -- The clock is shared across this whole spec file, and #2531's
    -- successor case ages a claim past repair_claim_timeout by moving
    -- it. Pin it here so no case inherits another's game time.
    writeIORef (gameTimeRef env) 0
    writeIORef (unitManagerRef env) emptyUnitManager
        { umDefs = HM.fromList
            [ ("acolyte", minimalDef "acolyte")
            , ("technomule", minimalDef "technomule") ]
        , umInstances = HM.fromList $
            [ (workerUid, (mkUnit "acolyte" unitPage (suWorkerAt su)
                                  (suWorkerCap su) (suWorkerInv su))
                            { uiEquipment = HM.fromList (suWorkerEquip su) }) ]
            <> [ (rivalUid, mkUnit "acolyte" unitPage startAt 100 [])
               | suRival su ]
            <> [ (muleUid, mkUnit "technomule" unitPage startAt 400 inv)
               | Just inv ← [suMuleInv su] ] }
    _ ← Q.flushQueue (unitQueue env)
    pure (Scene wsA wsO)

-- * Live-state readers

-- | Every ground instance on a page as
--   @(gid, defName, instanceId, condition, sharpness, position)@, so a
--   failure names the instance rather than diffing a whole record.
groundRows ∷ WorldState
           → IO [(Int, Text, Word64, Float, Float, (Float, Float))]
groundRows ws = do
    gis ← readIORef (wsGroundItemsRef ws)
    pure $ sortOn (\(g, _, _, _, _, _) → g)
        [ ( gid, iiDefName (giInst gi), iiInstanceId (giInst gi)
          , iiCondition (giInst gi), iiSharpness (giInst gi)
          , (giX gi, giY gi) )
        | (gid, gi) ← HM.toList (gisItems gis) ]

-- | The worker's inventory as @(instanceId, defName, condition,
--   sharpness, quality, fill)@ — every field #1737 requirement 6 says
--   an instance-preserving pickup must carry across untouched.
invOf ∷ EngineEnv → UnitId
      → IO [(Word64, Text, Float, Float, Float, Float)]
invOf env uid = do
    um ← readIORef (unitManagerRef env)
    pure $ case HM.lookup uid (umInstances um) of
        Nothing → []
        Just u  → sortOn (\(i, _, _, _, _, _) → i)
            [ ( iiInstanceId i, iiDefName i, iiCondition i
              , iiSharpness i, iiQuality i, iiCurrentFill i )
            | i ← uiInventory u ]

-- | Move a unit without going through the movement system: these cases
--   are about which page and which instance a job reads, not pathing.
placeUnit ∷ EngineEnv → UnitId → (Float, Float) → IO ()
placeUnit env uid (x, y) =
    atomicModifyIORef' (unitManagerRef env) $ \um →
        ( um { umInstances = HM.adjust
                   (\u → u { uiGridX = x, uiGridY = y }) uid (umInstances um) }
        , () )

-- | Take a unit out of the manager entirely — how a claimant dies.
killUnit ∷ EngineEnv → UnitId → IO ()
killUnit env uid =
    atomicModifyIORef' (unitManagerRef env) $ \um →
        ( um { umInstances = HM.delete uid (umInstances um) }, () )

-- | Take the worker's page out of @wmWorlds@, leaving the ACTIVE page —
--   and its same-numbered gid — in place. The shape "failure to resolve
--   a page is not an answer" is about.
unloadOwnedPage ∷ EngineEnv → IO ()
unloadOwnedPage env =
    atomicModifyIORef' (worldManagerRef env) $ \wm →
        ( wm { wmWorlds = filter ((≢ pageOwned) ∘ fst) (wmWorlds wm) }, () )

-- * Lua plumbing

newBareLuaBackend ∷ EngineEnv → IO LuaBackendState
newBareLuaBackend env = do
    ls ← createLuaBackendState (luaToEngineQueue env) (luaQueue env)
                                (assetPoolRef env) (nextObjectIdRef env)
                                (inputStateRef env) (loggerRef env)
    stateRef ← newIORef ThreadRunning
    registerLuaAPI (lbsLuaState ls) env ls stateRef
    pure ls

-- | The debug console is single-line, so a snippet is joined with
--   spaces rather than newlines.
luaLines ∷ [Text] → Text
luaLines = T.intercalate " "

isLuaError ∷ Text → Bool
isLuaError t = "error:" `T.isPrefixOf` t ∨ "syntax error:" `T.isPrefixOf` t

runOk ∷ LuaBackendState → Text → IO Text
runOk ls src = do
    r ← executeDebugLua (lbsLuaState ls) src
    r `shouldNotSatisfy` isLuaError
    pure r

-- | Load the production repair modules against this backend.
--
--   @building@ and @repair@ are replaced WHOLESALE rather than
--   per-function: #301's station↔axis mapping and @repair.repairAt@'s
--   restore are out of #1737's scope, and a station that always answers
--   keeps every case here about the ground rung. @STATION_BID@ is a
--   page-matched building on the worker's own page, so @unit_ai_page@'s
--   real #1673 guard is still satisfied honestly rather than bypassed.
--
--   @REPAIRED@ records what @repair.repairAt@ was actually asked to
--   restore, so a case can assert the EXACT instance reached the
--   station rather than merely that a repair happened.
loadRepair ∷ LuaBackendState → IO ()
loadRepair ls = do
    _ ← runOk ls $ luaLines
        [ "package.loaded['scripts.unit_ai'] = {};"
        , "package.loaded['scripts.movement_speed'] ="
        , "  { comfort = function() return 1.0 end,"
        , "    ordered = function() return 1.15 end,"
        , "    meander = function() return 0.5 end,"
        , "    sprint  = function() return 2.0 end };"
        , "_G.REPAIRED = {};"
        , "_G.STATION_OK = true;"
        , "_G.REPAIR_OK = true;"
        , "_G.REPAIR_RESTORE = false;"
        , "_G.STATION_PAGE = '" <> pageId pageOwned <> "';"
        , "_G.building = { findStation = function()"
        , "      if STATION_OK then return 900 end return nil end,"
        , "    getInfo = function(bid) return { page = "
        , "      STATION_PAGE, gridX = 10, gridY = 11,"
        , "      tileW = 1, tileH = 1 } end };"
        , "_G.repair = { get = function(id)"
        , "      if id == 'repair_condition' then"
        , "        return { inputs = { { item = 'lignite_chunk', count = 1 } } } end"
        , "      return { inputs = { { item = 'whetstone', count = 1 } } } end,"
        , "    repairAt = function(uid, recipeId, iid, bid)"
        , "      REPAIRED[#REPAIRED + 1] = { uid = uid, recipe = recipeId,"
        , "                                  instanceId = iid, bid = bid };"
        , "      if not REPAIR_OK then return nil, 'station offline' end;"
        , "      if REPAIR_RESTORE then"
        , "        local input = repair.get(recipeId).inputs[1];"
        , "        if not unit.removeItem(uid, input.item) then"
        , "          return nil, 'missing ' .. input.item end;"
        , "        local cond = (recipeId == 'repair_condition') and 1000 or 0;"
        , "        unit.repairItem(uid, iid, cond, 1000 - cond) end;"
        , "      return { ok = true } end };"
        , "_G.RP = require('scripts.unit_ai_repair');"
        , "_G.TARGETS = require('scripts.unit_ai_repair_target');"
        , "_G.CORE = require('scripts.unit_ai_core');"
        , "_G.RECON = require('scripts.unit_ai_reconcile');"
        , "_G.UNITAI = package.loaded['scripts.unit_ai'];"
        , "_G.PARAMS = require('scripts.unit_ai_tunables').acolyte;"
        , "return 'ok'" ]
    pure ()
  where
    pageId (WorldPageId p) = p

-- | Score the worker and report the candidate the ladder picked, as
--   @defName|instanceId|axis|source@ — flat so a failure prints the
--   pick rather than a table address. @none@ when nothing was picked.
--
--   Deliberately goes through the REAL @repairUtility@ rather than the
--   scan function directly: the capacity preflight, the station gate
--   and the claim/priority context all live there, and a case that
--   bypassed it would prove the scan alone.
scoreWorker ∷ LuaBackendState → IO Text
scoreWorker ls = runOk ls $ luaLines
    [ "local s = CORE.aiState[1] or {}; CORE.aiState[1] = s;"
    , "s.repairCandidate = nil;"
    , "local u = RP.utility(1, s, PARAMS);"
    , "local c = s.repairCandidate;"
    , "if not c or u == -math.huge then return 'none' end;"
    , "return c.defName .. '|' .. tostring(c.instanceId) .. '|' .. c.axis"
    , "  .. '|' .. (c.onGround and 'ground' or (c.onMule and 'mule' or 'own'))" ]

-- | One @repairExecute@ tick for a unit, reporting the phase it left
--   behind (@none@ when the job was released).
tick ∷ LuaBackendState → Int → IO Text
tick ls uid = runOk ls $ luaLines
    [ "local s = CORE.aiState[" <> uidText uid <> "]"
    , "  or {}; CORE.aiState[" <> uidText uid <> "] = s;"
    , "RP.execute(" <> uidText uid <> ", s, PARAMS);"
    , "return tostring(s.repairPhase)" ]

-- | Score then execute, which is how the AI loop reaches a fresh claim:
--   @repairUtility@ leaves the candidate and @repairExecute@ commits it.
claimFor ∷ LuaBackendState → Int → IO Text
claimFor ls uid = do
    _ ← runOk ls $ luaLines
        [ "local s = CORE.aiState[" <> uidText uid <> "]"
        , "  or {}; CORE.aiState[" <> uidText uid <> "] = s;"
        , "s.repairCandidate = nil;"
        , "RP.utility(" <> uidText uid <> ", s, PARAMS); return 'ok'" ]
    tick ls uid

jobField ∷ LuaBackendState → Int → Text → IO Text
jobField ls uid f = runOk ls $ luaLines
    [ "local s = CORE.aiState[" <> uidText uid <> "];"
    , "return tostring(s and s.repairJob and s.repairJob." <> f <> ")" ]

uidText ∷ Int → Text
uidText = T.pack ∘ show

iidText ∷ Word64 → Text
iidText = T.pack ∘ show

q ∷ Text → Text
q t = "\"" <> t <> "\""

pageText ∷ WorldPageId → Text
pageText (WorldPageId p) = p

-- | Who holds this instance's repair claim, as the UI reads it.
claimantOf ∷ LuaBackendState → Word64 → IO Text
claimantOf ls iid = runOk ls $
    "return tostring(UNITAI.getRepairClaimant(" <> iidText iid <> "))"

-- | How many times @repair.repairAt@ has been asked to restore anything.
repairedCount ∷ LuaBackendState → IO Text
repairedCount ls = runOk ls "return tostring(#REPAIRED)"

-- | Hand one EXACT instance to another owner through the production
--   cargo verb, which is how a player Store order (#1249) or an AI
--   hand-off moves a loose item out from under a suspended job.
--
--   #1673 refuses a cross-page endpoint pair, so every receiver below
--   stands on 'pageOwned' with the worker: one elsewhere would silently
--   never move the item and make the case vacuous. The verb compares
--   the two units' own pages rather than resolving either, so it still
--   lands while 'unloadOwnedPage' is in effect.
transferInstance ∷ LuaBackendState → Int → Int → Text → Word64 → IO Text
transferInstance ls from to defName iid = runOk ls $ luaLines
    [ "return tostring(unit.transferItemToUnit("
    , uidText from <> ", " <> uidText to <> ", '" <> defName <> "',"
    , iidText iid <> "))" ]

-- | Move the game clock the claim table ages against. @engine.gameTime@
--   reads this ref directly, so a case can expire a claim without
--   sleeping and without writing @repairClaims@ by hand.
setGameTime ∷ EngineEnv → Double → IO ()
setGameTime env = writeIORef (gameTimeRef env)
