{-# LANGUAGE OverloadedStrings #-}
-- | Every phase of @unitAi.commandPickup@'s order resolves its ground
--   entry on the CARRIER'S OWN page (#1666).
--
--   #1208 moved @item.pickupGround@ onto the unit's owning page,
--   because ground-item ids are per-page allocators and a
--   same-numbered gid on another page "is a different item entirely".
--   The Lua order layer above it kept SELECTING and MEASURING that
--   entry from @item.listGround@ — the ACTIVE page — so the two halves
--   of one contract could describe two different items: the command
--   gate weighed one instance, the arrival gate measured a distance to
--   another, and the commit moved a third. @unit.getInfo@ is a global
--   lookup, so an off-page carrier even answered with coordinates in
--   its own page's frame while the item's came from somewhere else.
--
--   Driven through a REAL 'EngineEnv' with the REAL registered Lua API
--   and the PRODUCTION @scripts/unit_ai_pickup.lua@ — the same
--   bare-Lua-backend technique as
--   'Test.Headless.Item.GroundPageOwnership', whose two-live-page
--   fixture this extends, and for the same reason: the defect lives in
--   page SELECTION, which a fixture that answers every ground query
--   from one stubbed table structurally cannot see. Pages are in-memory
--   'emptyWorldState's, so two live worlds cost no worldgen.
--
--   Page A is ACTIVE throughout, and the carrier stands on live,
--   NON-active page B. Only two things are replaced, neither of them on
--   the page path: @scripts.movement_speed@ (the real one reaches four
--   physiology modules to answer one walking pace) and the
--   @scripts.unit_ai@ singleton table the submodule attaches itself to.
--   The ground reads, the capacity reads, the position read, the move
--   command, the commit and the emitted events are all the engine's
--   own: move targets come off the engine's unit command queue and
--   events off the real event ring, so an assertion cannot pass against
--   a recording seam this fixture installed. The two notification
--   categories the order emits under are PINNED in the fixture, so the
--   log assertions do not depend on the developer's
--   @config/notifications.local.yaml@.
--
--   Run just this gate: @cabal test synarchy-test-headless
--   --test-options='--match "pickup order page ownership"'@.
module Test.Headless.Lua.UnitAiPickupPage (spec) where

import UPrelude
import Test.Hspec
import qualified Data.Map.Strict as Map
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import qualified Engine.Core.Queue as Q
import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TVar (modifyTVar')
import Data.List (sortOn)
import Data.IORef (newIORef, readIORef, writeIORef, atomicModifyIORef')
import Engine.Asset.Handle (TextureHandle(..))
import Engine.Core.State (EngineEnv(..))
import Engine.Core.Thread (ThreadControl(..))
import Engine.Scripting.Lua.API (registerLuaAPI)
import Engine.PlayerEvent (CategoryCfg(..), clearEventStoreRows)
import Engine.PlayerEvent.Emit (PlayerEvent(..), StoredEvent(..), readEventLog)
import Engine.Scripting.Lua.Thread (createLuaBackendState)
import Engine.Scripting.Lua.Thread.Console (executeDebugLua)
import Engine.Scripting.Lua.Types (LuaBackendState(..))
import Item.Ground (GroundItem(..), GroundItems(..), spawnGroundItem)
import Item.Types (ItemInstance(..), emptyItemManager)
import Unit.Command.Types (UnitCommand(..))
import Unit.Direction (Direction(..))
import Unit.Faction (Faction(..))
import Unit.Types
    ( BodyPart(..), UnitDef(..), UnitId(..), UnitInstance(..)
    , UnitManager(..), defaultNaturalResistance, emptyUnitManager )
import World.Page.Types (WorldPageId(..))
import World.State.Types
    ( WorldManager(..), WorldState(..), emptyWorldState, emptyWorldManager )

-- * Fixture identities

-- | The ACTIVE page. Its ground is the bystander throughout: a decoy
--   the order must never read, measure, walk toward, or remove.
pageActive ∷ WorldPageId
pageActive = WorldPageId "pickup_active"

-- | Live but NOT active — where the carrier stands.
pageOwned ∷ WorldPageId
pageOwned = WorldPageId "pickup_owned"

-- | The carrier under test.
carrierUid ∷ UnitId
carrierUid = UnitId 1

-- | Where page A's decoy sits. Deliberately far from page B's item, so
--   a distance or a move target computed from the wrong page is off by
--   tens of tiles rather than by a rounding error.
decoyAt ∷ (Float, Float)
decoyAt = (40, 40)

-- | Where page B's real target sits.
targetAt ∷ (Float, Float)
targetAt = (7, 9)

-- | Where the carrier starts: far enough from 'targetAt' that the
--   first execute must walk rather than commit.
startAt ∷ (Float, Float)
startAt = (0, 0)

-- * Fixtures

-- | @iiWeight@ is what @item.listGround@ / @item.getGroundForUnit@
--   report as the row's live @weight@, and therefore what both
--   capacity gates weigh.
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
    , iiBulk        = Just w
    , iiStorage     = Nothing
    }

-- | Mirrors 'Test.Headless.Item.GroundPageOwnership.minimalDef': the
--   order reads no def field, so only enough to make a live unit.
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
    , udBodyParts =
        [ BodyPart
            { bpId = "torso", bpName = "torso", bpParent = Nothing
            , bpVital = False, bpAreaWeight = 1.0, bpTacticalValue = 0.5
            , bpBleedFactor = 1.0, bpHeightLow = 0, bpHeightHigh = 1
            , bpLayers = [], bpTargetable = True, bpDepth = 0.0
            , bpAffectsLocomotion = False, bpAffectsBalance = False } ]
    , udNaturalResistance = defaultNaturalResistance
    , udNaturalWeapon = Nothing, udModifiers = [] }

mkUnit ∷ WorldPageId → (Float, Float) → Float → UnitInstance
mkUnit page (gx, gy) capacity = UnitInstance
    { uiDefName = "acolyte", uiName = "Nael", uiPage = page
    , uiTexture = TextureHandle 0, uiDirSprites = Map.empty
    , uiBaseWidth = 0, uiGridX = gx, uiGridY = gy, uiGridZ = 0
    , uiRealZ = 0, uiFacing = DirS
    , uiCurrentAnim = "", uiAnimStart = 0, uiAnimReverse = False
    , uiActivity = "idle", uiPose = "standing", uiAnimStride = 1
    , uiStats = HM.singleton "carrying_capacity" capacity
    , uiModifiers = HM.empty, uiSkills = HM.empty
    , uiKnowledge = HM.empty, uiInventory = [], uiEquipment = HM.empty
    , uiAccessories = [], uiFactionId = FactionPlayer, uiWounds = []
    , uiScars = [], uiImmuneResponse = 0, uiImmunities = HM.empty
    , uiBlood = 5.0, uiLastAttackerUid = Nothing, uiLastAttackerAt = 0
    , uiAnimOverride = "", uiFrozen = False, uiForceLoop = False
    , uiClimbDest = Nothing, uiTrailState = Nothing
    }

-- | A notification category that only ever reaches the log ring, with
--   both coalescing windows off.
logOnlyCategory ∷ Text → CategoryCfg
logOnlyCategory cid = CategoryCfg
    { ccId = cid, ccDisplayName = cid, ccDescription = ""
    , ccTextColor = (1, 1, 1, 1)
    , ccLog = True, ccPopup = False, ccPause = False
    , ccPopupCoalesceWindow = 0, ccLogCoalesceWindow = 0 }

data Scene = Scene
    { scActive ∷ WorldState
    , scOwned  ∷ WorldState
    }

-- | Two live pages, ACTIVE first, and the carrier on whichever page
--   the case names.
--
--   Ground is spawned in list order into fresh in-memory pages, so each
--   page's own allocator hands out ids from 0 — a same-numbered gid on
--   both pages is the DEFAULT here, not something the fixture has to
--   contrive.
resetScene ∷ EngineEnv
           → [(ItemInstance, (Float, Float))]
           → [(ItemInstance, (Float, Float))]
           → WorldPageId → (Float, Float) → Float
           → IO Scene
resetScene env activeGround ownedGround carrierPage carrierAt capacity = do
    wsA ← emptyWorldState
    wsO ← emptyWorldState
    forM_ activeGround $ \(it, (x, y)) →
        atomicModifyIORef' (wsGroundItemsRef wsA) $ spawnGroundItem it x y
    forM_ ownedGround $ \(it, (x, y)) →
        atomicModifyIORef' (wsGroundItemsRef wsO) $ spawnGroundItem it x y
    writeIORef (worldManagerRef env) emptyWorldManager
        { wmWorlds  = [(pageActive, wsA), (pageOwned, wsO)]
        , wmVisible = [pageActive] }
    writeIORef (itemManagerRef env) emptyItemManager
    -- The two categories this order emits under, pinned to log-only, so
    -- a local notifications override cannot decide whether an assertion
    -- about the event ring can see anything. Coalescing is off for the
    -- same reason: unit_warning ships a 1 s game-time log window, and
    -- every example here emits at game time 0.
    writeIORef (notificationCfgRef env) $ HM.fromList
        [ (c, logOnlyCategory c) | c ← ["unit_event", "unit_warning"] ]
    atomically $ modifyTVar' (eventStoreRef env) clearEventStoreRows
    writeIORef (unitManagerRef env) emptyUnitManager
        { umDefs = HM.singleton "acolyte" (minimalDef "acolyte")
        , umInstances = HM.singleton carrierUid
                            (mkUnit carrierPage carrierAt capacity) }
    _ ← Q.flushQueue (unitQueue env)
    pure (Scene wsA wsO)

-- * Live-state readers

-- | Every ground instance on a page, keyed by gid, with the position it
--   sits at — so a failure names the item and the page rather than
--   diffing a whole record.
groundRows ∷ WorldState → IO [(Int, Text, Word64, (Float, Float))]
groundRows ws = do
    gis ← readIORef (wsGroundItemsRef ws)
    pure $ sortOn (\(g, _, _, _) → g)
        [ (gid, iiDefName (giInst gi), iiInstanceId (giInst gi)
          , (giX gi, giY gi))
        | (gid, gi) ← HM.toList (gisItems gis) ]

invOf ∷ EngineEnv → UnitId → IO [(Word64, Text)]
invOf env uid = do
    um ← readIORef (unitManagerRef env)
    pure $ case HM.lookup uid (umInstances um) of
        Nothing → []
        Just u  → [(iiInstanceId i, iiDefName i) | i ← uiInventory u]

-- | Move the carrier without going through the movement system: these
--   cases are about which page an order READS, not about pathing.
placeUnit ∷ EngineEnv → UnitId → (Float, Float) → IO ()
placeUnit env uid (x, y) =
    atomicModifyIORef' (unitManagerRef env) $ \um →
        ( um { umInstances = HM.adjust
                   (\u → u { uiGridX = x, uiGridY = y }) uid (umInstances um) }
        , () )

-- | Take the carrier's page out of @wmWorlds@, leaving the ACTIVE page
--   — and its same-numbered gid — in place. This is the shape the
--   "failure to resolve the page is not an answer" rule is about.
unloadOwnedPage ∷ EngineEnv → IO ()
unloadOwnedPage env =
    atomicModifyIORef' (worldManagerRef env) $ \wm →
        ( wm { wmWorlds = filter ((≢ pageOwned) . fst) (wmWorlds wm) }, () )

-- | The REAL event ring, as @(category, text, uid, coords, page)@ —
--   including 'peSourcePage', which is the whole point: a unit event's
--   coordinates are in the frame of the page that unit stands on, and
--   an event that named the ACTIVE page instead would offer to pan the
--   wrong world (#1588's click rule, #1666's page).
eventRows ∷ EngineEnv
          → IO [(Text, Text, Maybe Word32, Maybe (Int, Int), Maybe Text)]
eventRows env = do
    evs ← map seEvent ⊚ readEventLog env
    pure [ (peCategory e, peText e, peUid e, peCoords e, peSourcePage e)
         | e ← evs ]

-- | Every @unit.moveTo@ the engine actually received, as
--   @(uid, x, y)@. Read from the engine's own command queue rather than
--   from a Lua spy, so the assertion is about what crossed the
--   boundary.
drainMoves ∷ EngineEnv → IO [(Word32, Float, Float)]
drainMoves env = do
    cmds ← Q.flushQueue (unitQueue env)
    pure [ (u, x, y) | UnitMoveTo (UnitId u) x y _ _ ← cmds ]

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

evalDebug ∷ LuaBackendState → Text → IO Text
evalDebug ls = executeDebugLua (lbsLuaState ls)

isLuaError ∷ Text → Bool
isLuaError t = "error:" `T.isPrefixOf` t ∨ "syntax error:" `T.isPrefixOf` t

runOk ∷ LuaBackendState → Text → IO Text
runOk ls src = do
    r ← evalDebug ls src
    r `shouldNotSatisfy` isLuaError
    pure r

q ∷ Text → Text
q t = "\"" <> t <> "\""


-- | Load the production pickup module against this backend.
--
--   @scripts.movement_speed@ is replaced at @package.loaded@ for the
--   reason 'Test.Headless.Lua.UnitAiHold' replaces it: the pace a walk
--   picks is not what this gate is about, and the real module reaches
--   four physiology modules' worth of stats to answer one number.
--   Nothing on the event path is replaced — @engine.emitEventForUnit@
--   is the engine's own, and the assertions read the ring it writes.
loadPickup ∷ LuaBackendState → IO ()
loadPickup ls = do
    _ ← runOk ls $ luaLines
        [ "package.loaded['scripts.unit_ai'] = {};"
        , "package.loaded['scripts.movement_speed'] ="
        , "  { comfort = function() return 1.0 end,"
        , "    ordered = function() return 1.15 end,"
        , "    meander = function() return 0.5 end,"
        , "    sprint  = function() return 2.0 end };"
        , "_G.PICKUP = require('scripts.unit_ai_pickup');"
        , "_G.CORE = require('scripts.unit_ai_core');"
        , "_G.UNITAI = package.loaded['scripts.unit_ai'];"
        , "_G.PARAMS = { pickup_timeout = 30, pickup_utility = 7.5,"
        , "              pickup_arrival_tiles = 1.5 };"
        , "return 'ok'" ]
    pure ()

-- | @nil@-safe reads of the live order, as flat strings so a failure
--   prints the value rather than a table address.
orderGid ∷ LuaBackendState → IO Text
orderGid ls = runOk ls
    "local s = CORE.aiState[1]; return tostring(s and s.pickupOrder and s.pickupOrder.gid)"

-- * The spec

spec ∷ SpecWith EngineEnv
spec = describe "pickup order page ownership" $ do

    describe "unitAi.commandPickup refuses an id off the carrier's page" $ do

        it "refuses a gid that exists only on the ACTIVE page, storing \
           \no order and saying nothing to the player" $ \env → do
            ls ← newBareLuaBackend env
            -- gid 1 exists on the ACTIVE page and nowhere else; the
            -- carrier is on page B, which holds only gid 0. Capacity is
            -- ample, so the OLD active-page read would have ACCEPTED
            -- this order outright — the refusal cannot be mistaken for
            -- a capacity refusal that happened to agree.
            scene ← resetScene env
                [ (mkItem "page_a_bar" 100 1, decoyAt)
                , (mkItem "page_a_rope" 101 1, decoyAt) ]
                [ (mkItem "page_b_radio" 200 1, targetAt) ]
                pageOwned startAt 100
            loadPickup ls
            beforeA ← groundRows (scActive scene)
            beforeO ← groundRows (scOwned scene)
            r ← runOk ls "return UNITAI.commandPickup(1, 1)"
            r `shouldBe` "false"
            orderGid ls `shouldReturn` q "nil"
            eventRows env `shouldReturn` []
            groundRows (scActive scene) `shouldReturn` beforeA
            groundRows (scOwned scene) `shouldReturn` beforeO
            invOf env carrierUid `shouldReturn` []
            drainMoves env `shouldReturn` []

        it "does not weigh the active-page collision, so its refusal \
           \never files an over-capacity warning" $ \env → do
            ls ← newBareLuaBackend env
            -- The carrier can lift 2 kg. The ACTIVE page's gid 1 weighs
            -- 50, so a capacity gate that inspected it would report
            -- over capacity and file the player-facing warning. #1666's
            -- refusal describes a CALLER error instead: no warning, no
            -- event of any kind.
            _ ← resetScene env
                [ (mkItem "page_a_bar" 100 1, decoyAt)
                , (mkItem "page_a_anvil" 101 50, decoyAt) ]
                [ (mkItem "page_b_radio" 200 1, targetAt) ]
                pageOwned startAt 2
            loadPickup ls
            r ← runOk ls "return UNITAI.commandPickup(1, 1)"
            r `shouldBe` "false"
            eventRows env `shouldReturn` []
            orderGid ls `shouldReturn` q "nil"

        it "leaves a position hold and nextActionAt exactly as it found \
           \them" $ \env → do
            ls ← newBareLuaBackend env
            _ ← resetScene env
                [ (mkItem "page_a_bar" 100 1, decoyAt)
                , (mkItem "page_a_rope" 101 1, decoyAt) ]
                [ (mkItem "page_b_radio" 200 1, targetAt) ]
                pageOwned startAt 100
            loadPickup ls
            -- An accepted order clears a hold (#1216); a refused one
            -- must not, and must not reset the decision clock either.
            r ← runOk ls $ luaLines
                [ "local s = CORE.ensureState(1);"
                , "s.holdAnchor = { x = 5, y = 5 };"
                , "s.nextActionAt = 1234;"
                , "local ok = UNITAI.commandPickup(1, 1);"
                , "return tostring(ok) .. ',' .. tostring(s.holdAnchor ~= nil)"
                , "  .. ',' .. tostring(s.nextActionAt)"
                , "  .. ',' .. tostring(s.pickupOrder)" ]
            r `shouldBe` q "false,true,1234,nil"

        it "refuses when the carrier's page is not live at all, without \
           \falling back to the active page's same-numbered id" $ \env → do
            ls ← newBareLuaBackend env
            _ ← resetScene env
                [ (mkItem "page_a_bar" 100 1, decoyAt) ]
                [ (mkItem "page_b_radio" 200 1, targetAt) ]
                pageOwned startAt 100
            loadPickup ls
            unloadOwnedPage env
            r ← runOk ls "return UNITAI.commandPickup(1, 0)"
            r `shouldBe` "false"
            orderGid ls `shouldReturn` q "nil"
            eventRows env `shouldReturn` []

    describe "an accepted order resolves the same (page, id) throughout" $ do

        it "accepts the carrier's own gid, walks to ITS page's \
           \coordinates, and commits that exact instance" $ \env → do
            ls ← newBareLuaBackend env
            -- gid 0 on BOTH pages, at coordinates 40+ tiles apart. Page
            -- A is active the whole way through.
            scene ← resetScene env
                [ (mkItem "page_a_bar" 100 1, decoyAt) ]
                [ (mkItem "page_b_radio" 200 1, targetAt) ]
                pageOwned startAt 100
            loadPickup ls
            runOk ls "return UNITAI.commandPickup(1, 0)"
                `shouldReturn` "true"
            orderGid ls `shouldReturn` q "0"

            -- Utility: measured against page B's item, so the order's
            -- own best-approach distance is the page-B distance
            -- (sqrt(7^2 + 9^2)), not the 50+ tiles page A's decoy sits
            -- away. currentAction names this order the way one real
            -- thought tick does, which is what engages the #1291
            -- eligible-time accounting that records bestDist.
            u ← runOk ls $ luaLines
                [ "local s = CORE.aiState[1];"
                , "s.currentAction = 'pickup_ground';"
                , "local v = PICKUP.pickupUtility(1, s, PARAMS);"
                , "return tostring(v) .. ',' .. string.format('%.3f',"
                , "  s.pickupOrder.bestDist)" ]
            u `shouldBe` q "7.5,11.402"

            -- Execute far away: it walks, and the destination is page
            -- B's coordinates, never the decoy's.
            _ ← runOk ls "PICKUP.pickupExecute(1, CORE.aiState[1], PARAMS); return 'ok'"
            drainMoves env `shouldReturn` [(1, fst targetAt, snd targetAt)]
            -- Nothing has moved yet on either page.
            invOf env carrierUid `shouldReturn` []

            -- Arrive and execute again: the commit takes page B's
            -- instance and leaves page A's same-numbered one alone.
            placeUnit env carrierUid targetAt
            beforeA ← groundRows (scActive scene)
            _ ← runOk ls "PICKUP.pickupExecute(1, CORE.aiState[1], PARAMS); return 'ok'"
            invOf env carrierUid `shouldReturn` [(200, "page_b_radio")]
            groundRows (scOwned scene) `shouldReturn` []
            groundRows (scActive scene) `shouldReturn` beforeA
            orderGid ls `shouldReturn` q "nil"

            -- Requirement 4: the success event names the instance that
            -- was committed, at the coordinates it was committed at —
            -- AND in the page frame those coordinates are indexed in.
            -- Page A is still the active one here, so an event that
            -- named the active page would be offering to pan the wrong
            -- world (#1588).
            eventRows env `shouldReturn`
                [ ( "unit_event", "Nael picked up page_b_radio", Just 1
                  , Just ( floor (fst targetAt), floor (snd targetAt) )
                  , Just (unWorldPageId pageOwned) ) ]

        it "measures the ARRIVAL capacity gate against the carrier's \
           \own page, not the active page's collision" $ \env → do
            ls ← newBareLuaBackend env
            -- Page B's target is light and page A's same-numbered decoy
            -- is far too heavy: a gate reading the active page would
            -- refuse on arrival and file a warning.
            scene ← resetScene env
                [ (mkItem "page_a_anvil" 100 500, decoyAt) ]
                [ (mkItem "page_b_radio" 200 1, targetAt) ]
                pageOwned targetAt 10
            loadPickup ls
            runOk ls "return UNITAI.commandPickup(1, 0)"
                `shouldReturn` "true"
            _ ← runOk ls "PICKUP.pickupExecute(1, CORE.aiState[1], PARAMS); return 'ok'"
            invOf env carrierUid `shouldReturn` [(200, "page_b_radio")]
            groundRows (scActive scene) `shouldReturn`
                [(0, "page_a_anvil", 100, decoyAt)]

    describe "quiet retirement needs the carrier's own page to say so" $ do

        it "retires the order when that page really has lost the item" $
            \env → do
                ls ← newBareLuaBackend env
                scene ← resetScene env
                    [ (mkItem "page_a_bar" 100 1, decoyAt) ]
                    [ (mkItem "page_b_radio" 200 1, targetAt) ]
                    pageOwned startAt 100
                loadPickup ls
                runOk ls "return UNITAI.commandPickup(1, 0)"
                    `shouldReturn` "true"
                -- Someone else took it, on the carrier's own page.
                writeIORef (wsGroundItemsRef (scOwned scene))
                    (GroundItems { gisNextId = 1, gisItems = HM.empty })
                u ← runOk ls $ luaLines
                    [ "local s = CORE.aiState[1];"
                    , "PICKUP.pickupUtility(1, s, PARAMS);"
                    , "return tostring(s.pickupOrder)" ]
                u `shouldBe` q "nil"
                eventRows env `shouldReturn` []

        it "holds the order when that page cannot be resolved, even \
           \though the ACTIVE page has the same id" $ \env → do
            ls ← newBareLuaBackend env
            _ ← resetScene env
                [ (mkItem "page_a_bar" 100 1, decoyAt) ]
                [ (mkItem "page_b_radio" 200 1, targetAt) ]
                pageOwned startAt 100
            loadPickup ls
            runOk ls "return UNITAI.commandPickup(1, 0)"
                `shouldReturn` "true"
            unloadOwnedPage env
            u ← runOk ls $ luaLines
                [ "local s = CORE.aiState[1];"
                , "local v = PICKUP.pickupUtility(1, s, PARAMS);"
                , "PICKUP.pickupExecute(1, s, PARAMS);"
                , "return tostring(v == -math.huge) .. ',' .."
                , "  tostring(s.pickupOrder and s.pickupOrder.gid)" ]
            u `shouldBe` q "true,0"
            -- And it certainly never walked toward, or took, the decoy.
            drainMoves env `shouldReturn` []
            invOf env carrierUid `shouldReturn` []

    describe "same-page operation is unchanged" $ do

        it "round-trips the exact instance through the real \
           \pickup_ground action when the carrier is on the ACTIVE page" $
            \env → do
                ls ← newBareLuaBackend env
                scene ← resetScene env
                    [ (mkItem "page_a_radio" 100 1, targetAt) ]
                    [ (mkItem "page_b_bar" 200 1, decoyAt) ]
                    pageActive startAt 100
                loadPickup ls
                runOk ls "return UNITAI.commandPickup(1, 0)"
                    `shouldReturn` "true"
                _ ← runOk ls "PICKUP.pickupExecute(1, CORE.aiState[1], PARAMS); return 'ok'"
                drainMoves env `shouldReturn` [(1, fst targetAt, snd targetAt)]
                placeUnit env carrierUid targetAt
                _ ← runOk ls "PICKUP.pickupExecute(1, CORE.aiState[1], PARAMS); return 'ok'"
                invOf env carrierUid `shouldReturn` [(100, "page_a_radio")]
                groundRows (scActive scene) `shouldReturn` []
                groundRows (scOwned scene) `shouldReturn`
                    [(0, "page_b_bar", 200, decoyAt)]
                -- Same page, so the attribution the off-page case
                -- corrects is a no-op here: still the active page,
                -- because that is the page the carrier is on.
                eventRows env `shouldReturn`
                    [ ( "unit_event", "Nael picked up page_a_radio", Just 1
                      , Just ( floor (fst targetAt), floor (snd targetAt) )
                      , Just (unWorldPageId pageActive) ) ]

        it "still refuses an over-capacity order with the player-facing \
           \warning that names the carrier and the item" $ \env → do
            ls ← newBareLuaBackend env
            _ ← resetScene env
                [ (mkItem "page_a_anvil" 100 500, targetAt) ]
                [ (mkItem "page_b_bar" 200 1, decoyAt) ]
                pageActive startAt 10
            loadPickup ls
            r ← runOk ls "return UNITAI.commandPickup(1, 0)"
            r `shouldBe` "false"
            orderGid ls `shouldReturn` q "nil"
            rows ← eventRows env
            case rows of
                [(cat, text, uid, coords, page)] → do
                    cat `shouldBe` "unit_warning"
                    uid `shouldBe` Just 1
                    coords `shouldBe` Just (floor (fst startAt), floor (snd startAt))
                    page `shouldBe` Just (unWorldPageId pageActive)
                    text `shouldSatisfy` T.isInfixOf "page_a_anvil"
                    text `shouldSatisfy` T.isInfixOf "Nael"
                _ → expectationFailure
                        ("expected one unit_warning, got " <> show rows)
