{-# LANGUAGE OverloadedStrings #-}
-- | The repair AI's GROUND rung (#1737) — the middle step of #302's
--   "own inventory → ground item → technomule" sourcing ladder, which
--   the merged #302 implementation never delivered.
--
--   Two things had to become true together, and this gate holds both
--   halves against the same live engine.
--
--   The ENGINE half: @item.listGround@ and @item.getGroundForUnit@ now
--   describe a ground item's @instanceId@, @sharpness@ and @kind@
--   alongside the @condition@ they already carried. All three come out
--   of the ONE shared @pushGroundRow@ (#1666), so the two readers
--   cannot describe the same item differently — which is precisely why
--   these cases read the REAL rows the engine builds rather than a Lua
--   fixture's idea of them. A stubbed ground table would agree with
--   itself no matter what the Haskell did.
--
--   The AI half: @scripts/unit_ai_repair_target.lua@ scores, claims,
--   prioritizes, takes and returns a ground instance through exactly
--   the machinery a held or mule-held one goes through. Because
--   severity keys on @condition@ \/ @sharpness@ \/ @kind@ and claims key
--   on @instanceId@, that is only possible with the engine half in
--   place: a ground row missing @sharpness@ would silently only ever
--   find condition-axis targets, which reads as "working".
--
--   Fixture technique is 'Test.Headless.Lua.UnitAiPickupPage''s, for
--   its reason: page selection is load-bearing here (#1673), and a
--   fixture answering every ground query from one stubbed table
--   structurally cannot see a page bug. Page A is ACTIVE and holds a
--   decoy at the SAME gid; the worker stands on live, non-active page
--   B. Ground reads, resolution, capacity, the pickup, the drop and the
--   inventory are all the engine's own.
--
--   Only what this rung is not about is replaced, at
--   @package.loaded@ \/ the global table: @scripts.movement_speed@ (the
--   real one reaches four physiology modules to answer one pace), the
--   @scripts.unit_ai@ singleton the submodules attach to, and the
--   @building@ \/ @repair@ station surface — #301's station↔axis
--   mapping and @repair.repairAt@'s restore are explicitly out of
--   #1737's scope and have their own coverage.
--
--   Run just this gate: @cabal test synarchy-test-headless
--   --test-options='--match "repair ground target"'@.
module Test.Headless.Lua.UnitAiRepairGround (spec) where

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
import Unit.Types
    ( UnitDef(..), UnitId(..), UnitInstance(..)
    , UnitManager(..), defaultNaturalResistance, emptyUnitManager )
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
    , udNaturalWeapon = Nothing, udModifiers = [] }

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
    , uiAccessories = [], uiFactionId = FactionPlayer, uiWounds = []
    , uiScars = [], uiImmuneResponse = 0, uiImmunities = HM.empty
    , uiBlood = 5.0, uiLastAttackerUid = Nothing, uiLastAttackerAt = 0
    , uiAnimOverride = "", uiFrozen = False, uiForceLoop = False
    , uiClimbDest = Nothing, uiTrailState = Nothing
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
    }

baseSetup ∷ Setup
baseSetup = Setup
    { suActiveGround = [], suOwnedGround = []
    , suWorkerInv = [], suWorkerCap = 100, suWorkerAt = startAt
    , suRival = False, suMuleInv = Nothing }

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
    let explicit = suActiveGround su
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
    writeIORef (unitManagerRef env) emptyUnitManager
        { umDefs = HM.fromList
            [ ("acolyte", minimalDef "acolyte")
            , ("technomule", minimalDef "technomule") ]
        , umInstances = HM.fromList $
            [ (workerUid, mkUnit "acolyte" pageOwned (suWorkerAt su)
                                 (suWorkerCap su) (suWorkerInv su)) ]
            <> [ (rivalUid, mkUnit "acolyte" pageOwned startAt 100 [])
               | suRival su ]
            <> [ (muleUid, mkUnit "technomule" pageOwned startAt 400 inv)
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
        , "_G.building = { findStation = function()"
        , "      if STATION_OK then return 900 end return nil end,"
        , "    getInfo = function(bid) return { page = "
        , "      '" <> pageId pageOwned <> "', gridX = 10, gridY = 11,"
        , "      tileW = 1, tileH = 1 } end };"
        , "_G.repair = { get = function(id)"
        , "      if id == 'repair_condition' then"
        , "        return { inputs = { { item = 'lignite_chunk', count = 1 } } } end"
        , "      return { inputs = { { item = 'whetstone', count = 1 } } } end,"
        , "    repairAt = function(uid, recipeId, iid, bid)"
        , "      REPAIRED[#REPAIRED + 1] = { uid = uid, recipe = recipeId,"
        , "                                  instanceId = iid, bid = bid };"
        , "      if REPAIR_OK then return { ok = true } end"
        , "      return nil, 'station offline' end };"
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

q ∷ Text → Text
q t = "\"" <> t <> "\""

-- * The spec

spec ∷ SpecWith EngineEnv
spec = describe "repair ground target" $ do
    rowShapeSpec
    selectionSpec
    takeSpec
    returnSpec
    durabilitySpec

-- | The engine half. A stubbed ground table would agree with itself
--   whatever the Haskell did, so every assertion here reads rows the
--   real 'pushGroundRow' built.
rowShapeSpec ∷ SpecWith EngineEnv
rowShapeSpec = describe "the row both ground readers build" $ do

    it "gives item.listGround and item.getGroundForUnit the same \
       \instanceId, sharpness and kind for every item on the page" $ \env → do
        ls ← newBareLuaBackend env
        -- The ACTIVE page holds a DIFFERENT instance at the same gid, so
        -- a listing read that never re-resolved would compare page A's
        -- row against page B's item and still find "a" row.
        _ ← resetScene env baseSetup
            { suActiveGround = [ (mkItem "steel_anvil" 900 100 100 60, nearAt)
                               , (mkItem "steel_anvil" 901 100 100 60, nearAt) ]
            , suOwnedGround  = [ (mkItem "wool_gambeson" 501 12 100 1.8, nearAt)
                               , (mkItem "axe_steel" 502 100 9 1.0, walkAt) ] }
        loadRepair ls
        r ← runOk ls $ luaLines
            [ "local out = {};"
            , "for _, g in ipairs(item.listGround() or {}) do"
            , "  local o = item.getGroundForUnit(1, g.id);"
            , "  if not o then out[#out+1] = g.id .. ':unresolved'"
            , "  else out[#out+1] = tostring(g.id) .. ':' .. o.defName"
            , "    .. ':' .. tostring(o.instanceId) .. ':' .. tostring(o.kind)"
            , "    .. ':' .. tostring(o.condition) .. ':' .. tostring(o.sharpness)"
            , "    .. ':' .. tostring(g.instanceId == o.instanceId)"
            , "    .. ':' .. tostring(g.kind == o.kind)"
            , "    .. ':' .. tostring(g.sharpness == o.sharpness) end end;"
            , "table.sort(out); return table.concat(out, ' ')" ]
        -- listGround reads page A (gid 0 = the anvil), getGroundForUnit
        -- reads page B (gid 0 = the gambeson): the SAME builder, two
        -- different items, which is exactly the #1666 hazard the ground
        -- rung has to resolve through rather than around.
        r `shouldBe` q ("0:wool_gambeson:501:armor:12.0:100.0:false:false:true"
                     <> " 1:axe_steel:502:weapon:100.0:9.0:false:false:false")

    it "always carries sharpness and kind, even where qualityTier is \
       \absent because the def declares no quality spec" $ \env → do
        ls ← newBareLuaBackend env
        -- axe_steel's fixture def has NO quality spec, so its row has no
        -- qualityTier. sharpness is universal runtime wear and must not
        -- follow that present-only-when-declared convention.
        _ ← resetScene env baseSetup
            { suOwnedGround = [ (mkItem "axe_steel" 601 100 9 1.0, nearAt) ] }
        loadRepair ls
        r ← runOk ls $ luaLines
            [ "local o = item.getGroundForUnit(1, 0);"
            , "return tostring(o.qualityTier) .. '|' .. tostring(o.sharpness)"
            , "  .. '|' .. tostring(o.kind) .. '|' .. tostring(o.condition)" ]
        r `shouldBe` q "nil|9.0|weapon|100.0"

    it "defaults kind to misc for an item whose def is gone, the same \
       \way unit.getInventory does, so no predicate reads a nil field" $ \env → do
        ls ← newBareLuaBackend env
        _ ← resetScene env baseSetup
            { suOwnedGround = [ (mkItem "ghost_relic" 602 5 5 1.0, nearAt) ] }
        loadRepair ls
        r ← runOk ls $ luaLines
            [ "local o = item.getGroundForUnit(1, 0);"
            , "return tostring(o.kind) .. '|' .. tostring(o.instanceId)" ]
        r `shouldBe` q "misc|602"

-- | Requirement 2, 3 and 9: the same scoring, the same ordering, an
--   absolute ladder, and every field read off the RESOLVED row.
selectionSpec ∷ SpecWith EngineEnv
selectionSpec = describe "choosing a ground target" $ do

    it "picks a condition-axis ground target" $ \env → do
        ls ← newBareLuaBackend env
        _ ← resetScene env baseSetup
            { suOwnedGround = [ (mkItem "axe_steel" 701 11 100 1.0, nearAt) ] }
        loadRepair ls
        scoreWorker ls `shouldReturn` q "axe_steel|701|condition|ground"

    it "picks a sharpness-axis ground target when condition is healthy, \
       \which is only reachable because the row carries sharpness" $ \env → do
        ls ← newBareLuaBackend env
        _ ← resetScene env baseSetup
            { suOwnedGround = [ (mkItem "axe_steel" 702 100 9 1.0, nearAt) ] }
        loadRepair ls
        scoreWorker ls `shouldReturn` q "axe_steel|702|sharpness|ground"

    it "reads the broken-armour band off the row's kind, so a broken \
       \gambeson outranks a broken axe" $ \env → do
        ls ← newBareLuaBackend env
        -- Both are condition 0. Only `kind` separates 2.5 from 1.5, and
        -- `kind` reaches the row only through pushGroundRow's def lookup.
        _ ← resetScene env baseSetup
            { suOwnedGround = [ (mkItem "axe_steel" 703 0 100 1.0, nearAt)
                              , (mkItem "wool_gambeson" 704 0 100 1.8, walkAt) ] }
        loadRepair ls
        scoreWorker ls `shouldReturn` q "wool_gambeson|704|condition|ground"

    it "ignores a listed id that does not resolve on the worker's own \
       \page" $ \env → do
        ls ← newBareLuaBackend env
        -- The degraded item exists ONLY on the ACTIVE page. An
        -- active-page scan would find and claim it; a resolved scan
        -- finds nothing at all.
        _ ← resetScene env baseSetup
            { suActiveGround = [ (mkItem "axe_steel" 705 3 100 1.0, nearAt) ] }
        loadRepair ls
        scoreWorker ls `shouldReturn` q "none"

    it "ignores a resolved row outside repair_scan_range" $ \env → do
        ls ← newBareLuaBackend env
        _ ← resetScene env baseSetup
            { suOwnedGround = [ (mkItem "axe_steel" 706 3 100 1.0, farAt) ] }
        loadRepair ls
        scoreWorker ls `shouldReturn` q "none"

    it "selects nothing at all when the worker has no live page to \
       \resolve against" $ \env → do
        ls ← newBareLuaBackend env
        _ ← resetScene env baseSetup
            { suActiveGround = [ (mkItem "axe_steel" 707 3 100 1.0, nearAt) ]
            , suOwnedGround  = [ (mkItem "axe_steel" 708 3 100 1.0, nearAt) ] }
        loadRepair ls
        unloadOwnedPage env
        scoreWorker ls `shouldReturn` q "none"

    it "prefers its OWN held gear over a more severely damaged ground \
       \instance" $ \env → do
        ls ← newBareLuaBackend env
        -- Held severity 0.36 (condition 20) versus ground 2.5 (broken
        -- armour). The ladder is absolute, so severity does not promote
        -- the lower rung.
        _ ← resetScene env baseSetup
            { suWorkerInv   = [ mkItem "axe_steel" 710 20 100 1.0 ]
            , suOwnedGround = [ (mkItem "wool_gambeson" 711 0 100 1.8, nearAt) ] }
        loadRepair ls
        scoreWorker ls `shouldReturn` q "axe_steel|710|condition|own"

    it "prefers a ground instance over a more severely damaged one on \
       \the technomule" $ \env → do
        ls ← newBareLuaBackend env
        _ ← resetScene env baseSetup
            { suOwnedGround = [ (mkItem "axe_steel" 712 20 100 1.0, nearAt) ]
            , suMuleInv     = Just [ mkItem "wool_gambeson" 713 0 100 1.8 ] }
        loadRepair ls
        scoreWorker ls `shouldReturn` q "axe_steel|712|condition|ground"

    it "skips a ground instance another LIVE worker already claimed" $ \env → do
        ls ← newBareLuaBackend env
        _ ← resetScene env baseSetup
            { suRival       = True
            , suOwnedGround = [ (mkItem "axe_steel" 714 3 100 1.0, nearAt) ] }
        loadRepair ls
        -- The rival claims through the real path, so the entry under
        -- test is the one production writes.
        claimFor ls 2 `shouldReturn` q "fetch_ground"
        scoreWorker ls `shouldReturn` q "none"

    it "self-heals a claim whose claimant is gone and takes the target" $ \env → do
        ls ← newBareLuaBackend env
        _ ← resetScene env baseSetup
            { suRival       = True
            , suOwnedGround = [ (mkItem "axe_steel" 715 3 100 1.0, nearAt) ] }
        loadRepair ls
        claimFor ls 2 `shouldReturn` q "fetch_ground"
        killUnit env rivalUid
        scoreWorker ls `shouldReturn` q "axe_steel|715|condition|ground"

    it "lets a player-prioritized ground instance beat a more severe \
       \unprioritized one, by the same instanceId key held gear uses" $ \env → do
        ls ← newBareLuaBackend env
        _ ← resetScene env baseSetup
            { suOwnedGround = [ (mkItem "wool_gambeson" 716 0 100 1.8, nearAt)
                              , (mkItem "axe_steel" 717 40 100 1.0, walkAt) ] }
        loadRepair ls
        scoreWorker ls `shouldReturn` q "wool_gambeson|716|condition|ground"
        _ ← runOk ls "UNITAI.setRepairPriority(717, true); return 'ok'"
        runOk ls "return tostring(UNITAI.isRepairPriority(717))"
            `shouldReturn` q "true"
        scoreWorker ls `shouldReturn` q "axe_steel|717|condition|ground"

    it "refuses a ground target it could not carry, weighing the row's \
       \LIVE mass rather than the static def weight" $ \env → do
        ls ← newBareLuaBackend env
        -- The instance weighs 40 where its def weighs 1: a preflight on
        -- the def weight would claim a job this worker can never carry.
        _ ← resetScene env baseSetup
            { suWorkerCap   = 12
            , suOwnedGround = [ (mkItem "axe_steel" 718 3 100 40, nearAt) ] }
        loadRepair ls
        scoreWorker ls `shouldReturn` q "none"

-- | Requirements 3, 6 and 7: what happens between claiming a ground
--   target and holding it.
takeSpec ∷ SpecWith EngineEnv
takeSpec = describe "taking a ground target" $ do

    it "walks to a target outside the arrival radius without touching \
       \the ground" $ \env → do
        ls ← newBareLuaBackend env
        scene ← resetScene env baseSetup
            { suOwnedGround = [ (mkItem "axe_steel" 720 3 100 1.0, walkAt) ] }
        loadRepair ls
        before ← groundRows (scOwned scene)
        claimFor ls 1 `shouldReturn` q "fetch_ground"
        tick ls 1 `shouldReturn` q "fetch_ground"
        groundRows (scOwned scene) `shouldReturn` before
        invOf env workerUid `shouldReturn` []

    it "takes the EXACT instance, with its quality, fill, condition and \
       \sharpness intact, and removes it from the page it lay on" $ \env → do
        ls ← newBareLuaBackend env
        scene ← resetScene env baseSetup
            { suActiveGround = [ (mkItem "steel_anvil" 901 100 100 60, nearAt) ]
            , suOwnedGround  = [ (mkItem "axe_steel" 721 3 17 1.0, nearAt) ] }
        loadRepair ls
        decoyBefore ← groundRows (scActive scene)
        claimFor ls 1 `shouldReturn` q "fetch_ground"
        tick ls 1 `shouldReturn` q "fetch_consumable"
        invOf env workerUid `shouldReturn`
            [ (721, "axe_steel", 3, 17, 88, 0) ]
        groundRows (scOwned scene) `shouldReturn` []
        -- The ACTIVE page's same-numbered gid is untouched.
        groundRows (scActive scene) `shouldReturn` decoyBefore
        jobField ls 1 "groundGid" `shouldReturn` q "nil"
        jobField ls 1 "fromGround" `shouldReturn` q "true"
        jobField ls 1 "itemFetched" `shouldReturn` q "true"

    it "treats a gid now naming a DIFFERENT instance as a raced target: \
       \nothing is repaired and nothing is substituted" $ \env → do
        ls ← newBareLuaBackend env
        scene ← resetScene env baseSetup
            { suOwnedGround = [ (mkItem "axe_steel" 722 3 100 1.0, nearAt) ] }
        loadRepair ls
        claimFor ls 1 `shouldReturn` q "fetch_ground"
        -- Someone else took gid 0 and dropped a fresh axe into the same
        -- allocator slot. The claim is on the INSTANCE, so this is not
        -- our target however identical its def looks.
        atomicModifyIORef' (wsGroundItemsRef (scOwned scene)) $ \gis →
            ( gis { gisItems = HM.insert 0
                        (GroundItem { giInst = mkItem "axe_steel" 723 3 100 1.0
                                    , giX = fst nearAt, giY = snd nearAt })
                        (gisItems gis) }, () )
        tick ls 1 `shouldReturn` q "nil"
        invOf env workerUid `shouldReturn` []
        map (\(g, _, i, _, _, _) → (g, i)) ⊚ groundRows (scOwned scene)
            `shouldReturn` [(0, 723)]
        runOk ls "return tostring(UNITAI.getRepairClaimant(722))"
            `shouldReturn` q "nil"

    it "refuses on ARRIVAL when the load it picked up en route no \
       \longer leaves room, leaving the target on the ground" $ \env → do
        ls ← newBareLuaBackend env
        scene ← resetScene env baseSetup
            { suWorkerCap   = 12
            , suOwnedGround = [ (mkItem "axe_steel" 724 3 100 8, walkAt) ] }
        loadRepair ls
        claimFor ls 1 `shouldReturn` q "fetch_ground"
        tick ls 1 `shouldReturn` q "fetch_ground"      -- still walking
        before ← groundRows (scOwned scene)
        -- The worker arrives having been handed a heavy load en route:
        -- the claim-time preflight already passed and cannot be re-run
        -- retroactively, which is why the arrival gate has to weigh
        -- again rather than trust it.
        atomicModifyIORef' (unitManagerRef env) $ \um →
            ( um { umInstances = HM.adjust
                       (\u → u { uiInventory =
                                   [ mkItem "steel_anvil" 725 100 100 9 ] })
                       workerUid (umInstances um) }, () )
        placeUnit env workerUid walkAt
        tick ls 1 `shouldReturn` q "nil"
        groundRows (scOwned scene) `shouldReturn` before
        map (\(i, d, _, _, _, _) → (i, d)) ⊚ invOf env workerUid
            `shouldReturn` [(725, "steel_anvil")]

-- | Requirement 8: the target always ends up back on the ground.
returnSpec ∷ SpecWith EngineEnv
returnSpec = describe "returning a ground target" $ do

    it "puts the exact instance back on the worker's own tile and page \
       \when the job completes" $ \env → do
        ls ← newBareLuaBackend env
        scene ← resetScene env baseSetup
            { suWorkerInv   = [ mkItem "lignite_chunk" 730 100 100 0.5 ]
            , suOwnedGround = [ (mkItem "axe_steel" 731 3 100 1.0, nearAt) ] }
        loadRepair ls
        decoyBefore ← groundRows (scActive scene)
        _ ← runOk ls "UNITAI.setRepairPriority(731, true); return 'ok'"
        claimFor ls 1 `shouldReturn` q "fetch_ground"
        tick ls 1 `shouldReturn` q "fetch_consumable"
        tick ls 1 `shouldReturn` q "walking"
        tick ls 1 `shouldReturn` q "repairing"
        tick ls 1 `shouldReturn` q "nil"
        -- The station saw the instance that lay on the ground.
        runOk ls "return tostring(REPAIRED[1].instanceId)"
            `shouldReturn` q "731"
        -- Back on the ground, at the WORKER's tile rather than where it
        -- was found, on the worker's own page.
        map (\(_, d, i, _, _, p) → (d, i, p)) ⊚ groundRows (scOwned scene)
            `shouldReturn` [("axe_steel", 731, startAt)]
        -- The drop landed on the worker's OWN page; the active page's
        -- same-numbered gid is untouched.
        groundRows (scActive scene) `shouldReturn` decoyBefore
        map (\(i, d, _, _, _, _) → (i, d)) ⊚ invOf env workerUid
            `shouldReturn` [(730, "lignite_chunk")]
        -- #303's flag self-clears on a ground instance exactly as on a
        -- held one.
        runOk ls "return tostring(UNITAI.isRepairPriority(731))"
            `shouldReturn` q "false"

    it "returns it to the ground when the job aborts after pickup, \
       \never handing it to a technomule" $ \env → do
        ls ← newBareLuaBackend env
        scene ← resetScene env baseSetup
            { suWorkerInv   = [ mkItem "lignite_chunk" 732 100 100 0.5 ]
            , suOwnedGround = [ (mkItem "axe_steel" 733 3 100 1.0, nearAt) ]
            , suMuleInv     = Just [] }
        loadRepair ls
        claimFor ls 1 `shouldReturn` q "fetch_ground"
        tick ls 1 `shouldReturn` q "fetch_consumable"
        -- The station is destroyed mid-job: the walking phase aborts.
        _ ← runOk ls "STATION_OK = false; return 'ok'"
        tick ls 1 `shouldReturn` q "walking"
        _ ← runOk ls $ luaLines
            [ "local s = CORE.aiState[1]; s.repairJob.bid = nil;"
            , "return 'ok'" ]
        tick ls 1 `shouldReturn` q "nil"
        map (\(_, d, i, _, _, p) → (d, i, p)) ⊚ groundRows (scOwned scene)
            `shouldReturn` [("axe_steel", 733, startAt)]
        -- The mule was never handed anything.
        invOf env muleUid `shouldReturn` []
        map (\(i, _, _, _, _, _) → i) ⊚ invOf env workerUid
            `shouldReturn` [732]

    it "keeps the job alive and retries when the drop cannot land, so \
       \the target is never stranded in an inventory" $ \env → do
        ls ← newBareLuaBackend env
        scene ← resetScene env baseSetup
            { suWorkerInv   = [ mkItem "lignite_chunk" 734 100 100 0.5 ]
            , suOwnedGround = [ (mkItem "axe_steel" 735 3 100 1.0, nearAt) ] }
        loadRepair ls
        claimFor ls 1 `shouldReturn` q "fetch_ground"
        tick ls 1 `shouldReturn` q "fetch_consumable"
        tick ls 1 `shouldReturn` q "walking"
        tick ls 1 `shouldReturn` q "repairing"
        -- The repair fails AND the worker's page is gone, so
        -- unit.dropItemById cannot resolve anywhere to drop.
        _ ← runOk ls "REPAIR_OK = false; return 'ok'"
        unloadOwnedPage env
        tick ls 1 `shouldReturn` q "returning"
        map (\(i, _, _, _, _, _) → i) ⊚ invOf env workerUid
            `shouldReturn` [734, 735]
        runOk ls "return tostring(UNITAI.getRepairClaimant(735))"
            `shouldReturn` q "1"
        -- A further tick still cannot drop and still must not release.
        tick ls 1 `shouldReturn` q "returning"
        -- The page comes back; the retry lands and the job ends.
        writeIORef (worldManagerRef env) emptyWorldManager
            { wmWorlds  = [ (pageActive, scActive scene)
                          , (pageOwned, scOwned scene) ]
            , wmVisible = [pageActive] }
        tick ls 1 `shouldReturn` q "nil"
        map (\(_, d, i, _, _, _) → (d, i)) ⊚ groundRows (scOwned scene)
            `shouldReturn` [("axe_steel", 735)]
        map (\(i, _, _, _, _, _) → i) ⊚ invOf env workerUid
            `shouldReturn` [734]
        runOk ls "return tostring(UNITAI.getRepairClaimant(735))"
            `shouldReturn` q "nil"

-- | The durable half (lua.unit_ai v7): provenance outlives the tick
--   that created it, and the gid it carries is a declared reference.
durabilitySpec ∷ SpecWith EngineEnv
durabilitySpec = describe "ground provenance across a save" $ do

    it "keeps fromGround and groundGid through the component snapshot, \
       \which strips only the transient candidate" $ \env → do
        ls ← newBareLuaBackend env
        _ ← resetScene env baseSetup
            { suOwnedGround = [ (mkItem "axe_steel" 740 3 100 1.0, walkAt) ] }
        loadRepair ls
        claimFor ls 1 `shouldReturn` q "fetch_ground"
        r ← runOk ls $ luaLines
            [ "local REFS = require('scripts.unit_ai_save_refs');"
            , "local V = require('scripts.unit_ai_save_validate');"
            , "local wrapped = REFS.wrapAiState({ [1] = CORE.aiState[1] });"
            , "local errs = V.validate(wrapped);"
            , "local w = wrapped[1].repairJob.groundGid;"
            , "local back = REFS.unwrapAiState(wrapped)[1];"
            , "return tostring(errs) .. '|' .. tostring(w.__ref)"
            , "  .. '|' .. tostring(w.id)"
            , "  .. '|' .. tostring(back.repairJob.fromGround)"
            , "  .. '|' .. tostring(back.repairJob.groundGid)"
            , "  .. '|' .. tostring(back.repairCandidate)" ]
        -- The gid crosses the wire as a TYPED ground_item edge, not a
        -- bare number the integrity graph could not check, and comes
        -- back exactly as the live state spells it. repairCandidate is
        -- the transient the snapshot strips; provenance is not.
        r `shouldBe` q "nil|ground_item|0|true|0|nil"

    it "declares groundGid as a ground_item edge, so a post-load \
       \reconcile drops a job whose target vanished and returns \
       \nothing to the ground it never picked up" $ \env → do
        ls ← newBareLuaBackend env
        scene ← resetScene env baseSetup
            { suOwnedGround = [ (mkItem "axe_steel" 741 3 100 1.0, walkAt) ] }
        loadRepair ls
        claimFor ls 1 `shouldReturn` q "fetch_ground"
        before ← groundRows (scOwned scene)
        r ← runOk ls $ luaLines
            [ "local ctx = { unit = { [1] = true }, building = { [900] = true },"
            , "  item_instance = { [741] = true },"
            , "  unitPage = { [1] = '" <> pageText pageOwned <> "' },"
            , "  byPage = { craft_bill = {}, ground_item = {"
            , "    ['" <> pageText pageOwned <> "'] = {} },"
            , "    location_instance = {} },"
            , "  activePage = '" <> pageText pageActive <> "' };"
            , "local n = RECON.scrubStaleRefs(1, CORE.aiState[1], ctx,"
            , "                               RECON.DROP_HOOKS);"
            , "return tostring(n) .. '|'"
            , "  .. tostring(CORE.aiState[1].repairJob)"
            , "  .. '|' .. tostring(UNITAI.getRepairClaimant(741))" ]
        r `shouldBe` q "1|nil|nil"
        groundRows (scOwned scene) `shouldReturn` before
        invOf env workerUid `shouldReturn` []
  where
    pageText (WorldPageId p) = p
