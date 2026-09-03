-- | #2291 gate: Exit to Menu ends the session's event history and its
--   game clock, the way a load already did.
--
--   'World.Load.Publish.resetTransientState' is the fresh-session reset
--   a LOAD runs; Exit to Menu reached none of it. So the player-event
--   ring carried the previous session's rows into the next game — rows
--   the event-log panel renders and the player can click to pan the NEW
--   world to the OLD one's coordinates, because @WorldPageId@ is a
--   reusable logical name and #1588's page check therefore passes — and
--   the game clock kept every prior session's accumulated seconds, which
--   the same panel renders as its time column and every save records as
--   @sdGameTime@.
--
--   Three fixture choices are load-bearing:
--
--   * __The boundary is driven through its production caller.__ Every
--     case calls the real 'handleWorldDestroyAllCommand' and then the
--     real 'unitTickWith'; nothing here enqueues the boundary's messages
--     by hand or calls the reset directly. The defect being gated is
--     precisely that the destroy-all path reached no reset, so a case
--     that reset the state itself would gate nothing.
--   * __The tick is run explicitly, because the handler only ENQUEUES.__
--     The headless harness starts no unit worker (it starts a world
--     worker, or none — 'Test.Headless.Harness'), and the unit worker is
--     what drains both @UnitClearAll@ and @BuildingClearAll@. Asserting
--     straight after the handler returns would only prove that messages
--     were queued, so every case asserts the unqueued state FIRST and
--     then ticks.
--   * __No case assumes the store starts at sequence 1.__ The counter is
--     deliberately process-lifetime and the harness boot may itself
--     emit, so each case reads the store's own high-water mark first and
--     asserts relative to it.
module Test.Headless.World.SessionEpoch (spec) where

import UPrelude
import Test.Hspec
import qualified Data.HashMap.Strict as HM
import Data.IORef (newIORef, readIORef, writeIORef)
import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TVar (modifyTVar')
import Building.Command.Types (BuildingCommand(..))
import Building.Types (BuildingId(..), BuildingManager(..), emptyBuildingManager)
import Engine.Core.Capability.Building
    (BuildingCapability(..), toBuildingCapability)
import Engine.Core.Capability.UnitCombat
    (UnitCombatCapability(..), toUnitCombatCapability)
import qualified Engine.Core.Queue as Q
import Engine.Core.SessionEpoch (freshSessionGameTime)
import Engine.Core.State
    ( EngineEnv, buildingManagerRef, eventStoreRef, gameTimeRef, loggerRef
    , notificationCfgRef, unitManagerRef, worldManagerRef )
import Engine.PlayerEvent
    (CategoryCfg(..), PlayerEvent(..), StoredEvent(..), clearEventStoreRows)
import Engine.PlayerEvent.Emit (emitEvent, readEventLogProgress)
import Test.Headless.Harness (withHeadlessEngineNoWorld)
import Unit.Command.Types (UnitCommand(..))
import Unit.Direction (Direction(..))
import Unit.Faction (Faction(..))
import Unit.Thread
    (UnitTickSeams(..), productionUnitTickSeams, unitTickWith)
import Unit.Types
import World.Load.Publish (resetTransientState)
import World.Page.Types (WorldPageId(..))
import World.State.Types (WorldManager(..), emptyWorldManager, emptyWorldState)
import World.Thread.Command.Basic (handleWorldDestroyAllCommand)
import qualified Data.Map.Strict as Map
import Engine.Asset.Handle (TextureHandle(..))

-- * Fixture

sessionPage ∷ WorldPageId
sessionPage = WorldPageId "session_epoch_page"

sessionUnit ∷ UnitId
sessionUnit = UnitId 1

sessionBuilding ∷ BuildingId
sessionBuilding = BuildingId 1

-- | The clock reading the outgoing session is left at. Far enough from
--   'freshSessionGameTime' that "reset" and "never advanced" cannot be
--   confused, and far enough from zero that a surviving value shows up
--   as an absurd time column rather than a rounding difference.
outgoingClock ∷ Double
outgoingClock = 5000

-- | One live unit, so a @UnitClearAll@ that runs is unmistakable.
sessionUnitInstance ∷ UnitInstance
sessionUnitInstance = UnitInstance
    { uiDefName = "session_epoch_test", uiName = "", uiPage = sessionPage
    , uiTexture = TextureHandle 0, uiDirSprites = Map.empty
    , uiBaseWidth = 0, uiGridX = 0, uiGridY = 0, uiGridZ = 0
    , uiRealZ = 0, uiFacing = DirS
    , uiCurrentAnim = "", uiAnimStart = 0, uiAnimReverse = False
    , uiActivity = "idle", uiPose = "standing", uiAnimStride = 1
    , uiStats = HM.empty, uiModifiers = HM.empty, uiSkills = HM.empty
    , uiKnowledge = HM.empty, uiInventory = [], uiEquipment = HM.empty
    , uiAccessories = [], uiFactionId = FactionPlayer, uiWounds = []
    , uiScars = [], uiImmuneResponse = 0, uiImmunities = HM.empty
    , uiBlood = 5.0, uiLastAttackerUid = Nothing, uiLastAttackerAt = 0
    , uiAnimOverride = "", uiFrozen = False, uiForceLoop = False
    , uiClimbDest = Nothing, uiTrailState = Nothing
    }

-- | A category that only LOGS, installed rather than borrowed from
--   @data/notification_categories.yaml@ so a later edit there cannot
--   turn these emits into popups, pauses or coalesced rows.
epochCategory ∷ CategoryCfg
epochCategory = CategoryCfg
    { ccId = epochCat, ccDisplayName = epochCat, ccDescription = ""
    , ccTextColor = (1, 1, 1, 1)
    , ccLog = True, ccPopup = False, ccPause = False
    , ccPopupCoalesceWindow = 0, ccLogCoalesceWindow = 0 }

epochCat ∷ Text
epochCat = "session_epoch_probe"

-- | A session with one page, one unit, one selected building, a clock
--   well past zero and three event rows — everything the boundary is
--   supposed to end. Answers the store's high-water mark at that point,
--   which every counter assertion is relative to.
installSession ∷ EngineEnv → IO Int
installSession env = do
    ws ← emptyWorldState
    writeIORef (worldManagerRef env) emptyWorldManager
        { wmWorlds = [(sessionPage, ws)], wmVisible = [sessionPage] }
    writeIORef (unitManagerRef env) emptyUnitManager
        { umInstances = HM.singleton sessionUnit sessionUnitInstance
        , umNextId = 2 }
    writeIORef (buildingManagerRef env) emptyBuildingManager
        { bmSelected = Just sessionBuilding }
    writeIORef (notificationCfgRef env) $
        HM.singleton epochCat epochCategory
    -- Start from a known-empty ring through the production helper, so a
    -- row left over from harness boot cannot be counted as this
    -- session's. The counter deliberately survives that, which is why
    -- the base is read AFTER it rather than assumed.
    atomically $ modifyTVar' (eventStoreRef env) clearEventStoreRows
    writeIORef (gameTimeRef env) outgoingClock
    forM_ ["one", "two", "three"] $ \t →
        emitEvent env epochCat "Test.SessionEpoch" t
    (_, high) ← readEventLogProgress env
    pure high

-- | One unit tick with no real clock, movement or sleep. @dt@ is zero,
--   so the tick's own clock advance cannot be mistaken for the reset.
runUnitTick ∷ EngineEnv → IO ()
runUnitTick env = do
    samples ← newIORef [0, 0]
    lastRef ← newIORef 0
    let seams = productionUnitTickSeams
            { tickClock    = do
                current ← readIORef samples
                case current of
                    (x:rest) → writeIORef samples rest >> pure x
                    []       → pure 0
            , tickMovement = \_ _ _ → pure ()
            , tickSleep    = \_ → pure ()
            }
    _ ← unitTickWith seams env lastRef (ucUtsRef (toUnitCombatCapability env))
    pure ()

destroyAll ∷ EngineEnv → IO ()
destroyAll env = do
    logger ← readIORef (loggerRef env)
    handleWorldDestroyAllCommand env logger

liveUnits ∷ EngineEnv → IO Int
liveUnits env = HM.size ∘ umInstances <$> readIORef (unitManagerRef env)

selectedBuilding ∷ EngineEnv → IO (Maybe BuildingId)
selectedBuilding env = bmSelected <$> readIORef (buildingManagerRef env)

clock ∷ EngineEnv → IO Double
clock env = readIORef (gameTimeRef env)

rowTexts ∷ EngineEnv → IO [Text]
rowTexts env = map (peText ∘ seEvent) ∘ fst <$> readEventLogProgress env

highWater ∷ EngineEnv → IO Int
highWater env = snd <$> readEventLogProgress env

-- | Queue depth without consuming it — the flush is put straight back,
--   in order.
queuedUnit ∷ EngineEnv → IO Int
queuedUnit env = depthOf (ucUnitQueue (toUnitCombatCapability env))

queuedBuilding ∷ EngineEnv → IO Int
queuedBuilding env = depthOf (bcBuildingQueue (toBuildingCapability env))

depthOf ∷ Q.Queue α → IO Int
depthOf q = do
    pending ← Q.flushQueue q
    mapM_ (Q.writeQueue q) pending
    pure (length pending)

-- * Spec

spec ∷ Spec
spec = describe "Exit to Menu session epoch (issue #2291)" $ do

    it "the destroy-all handler only ENQUEUES: nothing it touches has \
       \changed by the time it returns" $
        withHeadlessEngineNoWorld $ \env → do
        base ← installSession env
        destroyAll env
        -- The whole premise of the tick assertions below. If this ever
        -- becomes false the boundary has grown a second, unsynchronised
        -- writer of the clock on the world thread, which is the race
        -- 'Unit.Thread.endSessionEpoch' exists to avoid.
        clock env      `shouldReturn` outgoingClock
        rowTexts env   `shouldReturn` ["one", "two", "three"]
        highWater env  `shouldReturn` base
        liveUnits env  `shouldReturn` 1
        selectedBuilding env `shouldReturn` Just sessionBuilding
        -- Each queue carries its clear followed by its boundary marker.
        queuedUnit env     `shouldReturn` 2
        queuedBuilding env `shouldReturn` 2

    it "one tick after the boundary: the ring is empty, the sequence \
       \counter is intact, and the clock is back at the fresh-session \
       \epoch" $ withHeadlessEngineNoWorld $ \env → do
        base ← installSession env
        destroyAll env
        runUnitTick env
        rowTexts env  `shouldReturn` []
        highWater env `shouldReturn` base
        clock env     `shouldReturn` freshSessionGameTime

    it "the clock the boundary restores is the one a freshly booted \
       \process starts at, not merely the constant it is written from" $
        withHeadlessEngineNoWorld $ \env → do
        -- Requirement 3 is a claim about two SESSIONS agreeing, so the
        -- expected value is read off a second, untouched boot. Asserting
        -- against 'freshSessionGameTime' alone is a tautology: the
        -- production write and the assertion would share the one symbol,
        -- so a wrong value — even one equal to the outgoing clock —
        -- would satisfy it.
        bootClock ← withHeadlessEngineNoWorld clock
        bootClock `shouldNotBe` outgoingClock
        _ ← installSession env
        destroyAll env
        runUnitTick env
        clock env `shouldReturn` bootClock

    it "the entity clears the reset depends on have RUN, not merely been \
       \queued, by the time the clock moves" $
        withHeadlessEngineNoWorld $ \env → do
        _ ← installSession env
        destroyAll env
        runUnitTick env
        -- Both drains reached their clear in the same tick that reset
        -- the clock. A record stamped on the outgoing clock — a unit's
        -- anim start, a building's destruction expiry — can therefore
        -- never be measured against the incoming one.
        liveUnits env        `shouldReturn` 0
        selectedBuilding env `shouldReturn` Nothing
        clock env            `shouldReturn` freshSessionGameTime

    it "a row emitted after the boundary takes the retained next \
       \sequence, so it still outranks a cursor held from before it" $
        withHeadlessEngineNoWorld $ \env → do
        base ← installSession env
        destroyAll env
        runUnitTick env
        emitEvent env epochCat "Test.SessionEpoch" "after"
        (rows, high) ← readEventLogProgress env
        map (peText ∘ seEvent) rows `shouldBe` ["after"]
        map seSequence rows         `shouldBe` [base + 1]
        high                        `shouldBe` base + 1

    it "work queued behind the boundary waits for the next tick, so it \
       \is never applied on the outgoing clock" $
        withHeadlessEngineNoWorld $ \env → do
        _ ← installSession env
        destroyAll env
        -- Stand-ins for whatever the next session enqueues first. Both
        -- sit behind their queue's boundary marker.
        Q.writeQueue (ucUnitQueue (toUnitCombatCapability env)) UnitClearAll
        Q.writeQueue (bcBuildingQueue (toBuildingCapability env))
                     BuildingClearAll
        runUnitTick env
        -- The reset happened; the post-boundary work did not.
        clock env          `shouldReturn` freshSessionGameTime
        queuedUnit env     `shouldReturn` 1
        queuedBuilding env `shouldReturn` 1
        -- Deferred, not dropped: the very next tick runs it, now on the
        -- new epoch.
        runUnitTick env
        queuedUnit env     `shouldReturn` 0
        queuedBuilding env `shouldReturn` 0

    it "a load still installs the save's own clock: the load-publish \
       \transient reset leaves it alone" $
        withHeadlessEngineNoWorld $ \env → do
        _ ← installSession env
        -- The load path writes ssGameTime BEFORE resetTransientState
        -- runs (World.Load.Publish), so a reset that zeroed the clock
        -- would silently discard every save's recorded playtime. This
        -- is the case that would catch it.
        resetTransientState env
        clock env    `shouldReturn` outgoingClock
        rowTexts env `shouldReturn` []
