{-# LANGUAGE Strict #-}
-- | The coordination "Scenario.Load" (#2713) drives the PRODUCTION service
--   with: the engine of "Test.Headless.Scenario.Placement.Fixtures" (real
--   definitions, real world thread, a real Lua state), the production
--   Lua message pump ('processLuaMsgs'), and stand-ins for the owner
--   threads a headless harness never starts.
--
--   Nothing here fakes a layer the service touches. The save barrier is
--   the real one; the owners that would acknowledge it are small loops
--   that follow the same protocol the real ones do (acknowledge at the
--   tick boundary, mutate only while 'ownerGated' is False) and COUNT
--   what they did, so a test can assert that a parked owner did nothing.
module Test.Headless.Scenario.Load.Harness
    ( H(..)
    , withH
    , Owners(..)
    , withOwners
    , ownerCounts
    , holdOwner
    , holdSimulation
    , pendingSimulationAcks
    , Pump
    , startPump
    , stopPump
    , pumpUntilTerminal
    , runLoad
    , runLoadWith
    , request
    , await
    , statusOf
    , Hold(..)
    , newHold
    , holdAt
    , failAt
    , armSeams
    , armSeamsWith
    , armSeamsSimWait
    , waitCondition
    , disarm
    , writeScenario
    , scratchFile
    , activeArena
    , liveUnits
    , liveBuildings
    , sessionFingerprint
    , doc
    , registerLuaModule
    , unregister
    , lua
    , loadStatusRefOf
    ) where

import UPrelude
import Control.Concurrent (ThreadId, forkIO, killThread, threadDelay)
import Control.Concurrent.MVar
import Control.Exception (bracket, throwIO, ErrorCall(..))
import Data.IORef
import GHC.Clock (getMonotonicTime)
import qualified Data.HashMap.Strict as HM
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import qualified Engine.Core.Queue as Q
import Sim.Command.Types (SimCommand(..))
import Sim.Memory (SimMemory(..))
import System.Directory (createDirectoryIfMissing, getTemporaryDirectory)
import System.FilePath ((</>))
import Test.Hspec (expectationFailure)
import Control.Concurrent.STM (atomically, modifyTVar')
import qualified Data.Text.Encoding as TE
import qualified HsLua as Lua
import Building.Types
import Engine.Core.Capability.SaveLoad (SaveLoadCapability(..), toSaveLoadCapability)
import Engine.Core.Capability.WorldSim (WorldSimCapability(..), toWorldSimCapability)
import Engine.Core.State (EngineEnv(..))
import Engine.Core.Thread (ThreadControl(..))
import Engine.Load.Status
import Engine.Save.Barrier (SaveOwner(..), acknowledgeCurrent, ownerGated)
import Engine.Scripting.Lua.Thread.Dispatch (processLuaMsgs)
import Engine.Scripting.Lua.Types (LuaBackendState(..), LuaScript(..))
import Scenario.Load.Service (requestScenarioLoad)
import Scenario.Load.Session (arenaPageId)
import Scenario.Load.Types
import Scenario.Schema (ScenarioFormat)
import Test.Headless.Scenario.Placement.Fixtures
import Test.Headless.Scenario.World.Fixtures (catalog)
import Unit.Types (UnitId, UnitInstance, UnitManager(..))
import World.State.Types
import Location.Types (lookupLocation)
import World.Flora.Types (FloraCatalog)

-- | The engine, and the control cell the production message pump takes.
data H = H { hFx :: Fx, hCtl :: IORef ThreadControl, hOwners :: Owners }

-- | Boot the fixture engine with the stand-in owners running.
withH ∷ (H → IO ()) → IO ()
withH act = withFx $ \fx → do
    -- the flora species the world-content examples name
    writeIORef (floraCatalogRef (fxEnv fx)) floraFixture
    registerCrypt fx
    ctl ← newIORef ThreadRunning
    withOwners (fxEnv fx) $ \owners → act (H fx ctl owners)

-- * Owners

-- | The owners the harness never starts, as the save barrier sees them.
--   Per owner: ticks it MUTATED on (the barrier did not gate it), ticks it
--   was PARKED on, and a hold that withholds its acknowledgements.
data Owners = Owners
    { owMutated ∷ IORef (Map.Map SaveOwner Int)
    , owParked  ∷ IORef (Map.Map SaveOwner Int)
    , owHeld    ∷ IORef (Map.Map SaveOwner Bool)
    , owSimHeld ∷ IORef Bool
      -- ^ the simulation stand-in withholds its answer to the
      --   acknowledged-consumption round trip
    , owSimPending ∷ IORef [MVar (Maybe SimMemory)]
      -- ^ the round trips it has received and not answered
    }

standIns ∷ [SaveOwner]
standIns = [SaveUnit, SaveBuilding, SaveCombat, SaveSimulation, SaveRender]

withOwners ∷ EngineEnv → (Owners → IO α) → IO α
withOwners env act = do
    owners ← Owners <$> newIORef Map.empty <*> newIORef Map.empty <*> newIORef Map.empty
                    <*> newIORef False <*> newIORef []
    let barrier = slSaveBarrierRef (toSaveLoadCapability env)
        simQ = wsSimQueue (toWorldSimCapability env)
        -- the simulation thread's one observable duty here: consume its queue
        -- in order and answer the observational read when (and only when) it is
        -- not being held. Everything else it consumes is dropped, as an
        -- unobserved simulation would drop it.
        simLoop = forever $ do
            held ← readIORef (owSimHeld owners)
            unless held $ do
                pending ← atomicModifyIORef' (owSimPending owners) (\ps → ([], ps))
                forM_ pending $ \r → tryPutMVar r (Just (SimMemory 0 []))
            m ← Q.readQueueTimeout 1000 simQ
            case m of
                Just (SimReadMemory reply) → do
                    h ← readIORef (owSimHeld owners)
                    if h then atomicModifyIORef' (owSimPending owners) (\ps → (reply : ps, ()))
                         else void (tryPutMVar reply (Just (SimMemory 0 [])))
                _ → pure ()
        loop o = forever $ do
            gated ← ownerGated barrier o
            modifyIORef' (if gated then owParked owners else owMutated owners)
                (Map.insertWith (+) o 1)
            held ← Map.findWithDefault False o <$> readIORef (owHeld owners)
            unless held (acknowledgeCurrent barrier o)
            threadDelay 500
    bracket (mapM forkIO (simLoop : map loop standIns)) (mapM_ killThread) (const (act owners))

-- | (mutated, parked) tick counts of one owner so far.
ownerCounts ∷ Owners → SaveOwner → IO (Int, Int)
ownerCounts ow o = do
    m ← readIORef (owMutated ow)
    p ← readIORef (owParked ow)
    pure (Map.findWithDefault 0 o m, Map.findWithDefault 0 o p)

-- | Withhold (True) or resume (False) the simulation stand-in's answers to
--   the acknowledged-consumption round trip.
holdSimulation ∷ Owners → Bool → IO ()
holdSimulation ow = writeIORef (owSimHeld ow)

-- | How many round trips the held simulation has received and not answered.
pendingSimulationAcks ∷ Owners → IO Int
pendingSimulationAcks = fmap length ∘ readIORef ∘ owSimPending

-- | Withhold (True) or resume (False) one owner's acknowledgements.
holdOwner ∷ Owners → SaveOwner → Bool → IO ()
holdOwner ow o b = modifyIORef' (owHeld ow) (Map.insert o b)

-- * The pump

-- | The production Lua message pump on its own thread, so a request held
--   at a seam blocks THAT thread and the test thread can assert.
data Pump = Pump { pumpThread ∷ ThreadId }

startPump ∷ H → IO Pump
startPump h = Pump <$> forkIO (forever (processLuaMsgs (fxEnv (hFx h)) (fxLua (hFx h)) (hCtl h)
                                        >> threadDelay 500))

stopPump ∷ Pump → IO ()
stopPump = killThread ∘ pumpThread

-- | Drain the queue on THIS thread until request @n@ is terminal. Fails the
--   example (never skips) after @secs@ seconds.
pumpUntilTerminal ∷ H → Int → Double → IO ScenarioLoadStatus
pumpUntilTerminal h n secs = do
    start ← getMonotonicTime
    let go = do
            processLuaMsgs (fxEnv (hFx h)) (fxLua (hFx h)) (hCtl h)
            st ← statusOf h
            case st of
                Just s | slsRequestId s ≡ n ∧ isJust (slsOutcome s) → pure s
                _ → do
                    now ← getMonotonicTime
                    when (now - start > secs) $ do
                        expectationFailure ("scenario load #" ⧺ show n ⧺ " did not finish: " ⧺ show st)
                        throwIO (ErrorCall "timeout")
                    threadDelay 500
                    go
    go

-- | Wait (with the pump running elsewhere) for request @n@ to be terminal.
await ∷ H → Int → Double → IO ScenarioLoadStatus
await h n secs = do
    start ← getMonotonicTime
    let go = do
            st ← statusOf h
            case st of
                Just s | slsRequestId s ≡ n ∧ isJust (slsOutcome s) → pure s
                _ → do
                    now ← getMonotonicTime
                    when (now - start > secs) $ do
                        expectationFailure ("scenario load #" ⧺ show n ⧺ " did not finish: " ⧺ show st)
                        throwIO (ErrorCall "timeout")
                    threadDelay 1000
                    go
    go

statusOf ∷ H → IO (Maybe ScenarioLoadStatus)
statusOf = readScenarioStatus ∘ slLoadStatusRef ∘ toSaveLoadCapability ∘ fxEnv ∘ hFx

-- | Ask for a load and pump it to its end on this thread.
runLoad ∷ H → ScenarioSource → IO ScenarioLoadStatus
runLoad h src = runLoadWith h src (pure ())

-- | 'runLoad' with an action run between acceptance and the first pump
--   (to queue a stale command, for instance).
runLoadWith ∷ H → ScenarioSource → IO () → IO ScenarioLoadStatus
runLoadWith h src between = do
    n ← request h src
    between
    pumpUntilTerminal h n 60

-- | Ask for a load; the request must be accepted.
request ∷ H → ScenarioSource → IO Int
request h src = requestScenarioLoad (fxEnv (hFx h)) src ≫= \case
    Right n → pure n
    Left busy → do
        expectationFailure ("the request was refused: " ⧺ show busy)
        throwIO (ErrorCall "refused")

-- * Seams

-- | A held seam: reached (filled when the service arrives) and released
--   (filled by the test to let it go).
data Hold = Hold { holdReached ∷ MVar (), holdRelease ∷ MVar () }

newHold ∷ IO Hold
newHold = Hold <$> newEmptyMVar <*> newEmptyMVar

-- | Hold the service at @point@: the first time it arrives it signals
--   'holdReached' and waits for 'holdRelease'.
holdAt ∷ ScenarioLoadPoint → Hold → ScenarioLoadPoint → IO ()
holdAt point hold p = when (p ≡ point) $ do
    _ ← tryPutMVar (holdReached hold) ()
    readMVar (holdRelease hold)

-- | Fail the service at @point@ with a synchronous exception.
failAt ∷ ScenarioLoadPoint → Text → ScenarioLoadPoint → IO ()
failAt point why p = when (p ≡ point) (throwIO (ErrorCall (T.unpack why)))

armSeams ∷ H → (ScenarioLoadPoint → IO ()) → IO ()
armSeams h f = armScenarioSeams (loadRef h) noSeams { seamAt = f }

armSeamsWith ∷ H → (ScenarioLoadPoint → IO ()) → Maybe ScenarioFormat → IO ()
armSeamsWith h f fmt = armScenarioSeams (loadRef h) noSeams { seamAt = f, seamFormat = fmt }

disarm ∷ H → IO ()
disarm = disarmScenarioSeams ∘ loadRef

loadRef ∷ H → LoadStatusRef
loadRef = slLoadStatusRef ∘ toSaveLoadCapability ∘ fxEnv ∘ hFx

-- * Sources

-- | Write a scenario document under a scratch directory and return its path.
writeScenario ∷ String → String → IO FilePath
writeScenario name text = do
    path ← scratchFile name
    writeFile path text
    pure path

scratchFile ∷ String → IO FilePath
scratchFile name = do
    tmp ← getTemporaryDirectory
    let dir = tmp </> "synarchy-scenario-load"
    createDirectoryIfMissing True dir
    pure (dir </> name)

doc ∷ [String] → String
doc ls = unlines ("version: 3" : ls)

-- * Reading the session

-- | The arena page the service publishes, once it stands.
activeArena ∷ H → IO (Maybe WorldState)
activeArena h = lookup arenaPageId ∘ wmWorlds <$> readIORef (wsWorldManagerRef (toWorldSimCapability (fxEnv (hFx h))))

liveUnits ∷ H → IO [(UnitId, UnitInstance)]
liveUnits h = HM.toList ∘ umInstances <$> readIORef (unitManagerRef (fxEnv (hFx h)))

liveBuildings ∷ H → IO [BuildingInstance]
liveBuildings h = HM.elems ∘ bmInstances <$> readIORef (buildingManagerRef (fxEnv (hFx h)))

-- | The session's identity, as far as a preserved session must keep it: the
--   pages it holds (by generated-world identity), the unit and building
--   registries, and the pause flag.
sessionFingerprint ∷ H → IO (Text, Int, Int, Bool)
sessionFingerprint h = do
    let env = fxEnv (hFx h)
    wm ← readIORef (worldManagerRef env)
    ids ← forM (wmWorlds wm) $ \(pid, ws) → do
        gid ← readIORef (wsGeneratedIdRef ws)
        pure (show pid ⧺ "=" ⧺ show gid)
    um ← readIORef (unitManagerRef env)
    bm ← readIORef (buildingManagerRef env)
    paused ← readIORef (enginePausedRef env)
    pure (T.pack (unwords ids), HM.size (umInstances um), HM.size (bmInstances bm), paused)

loadStatusRefOf ∷ H → LoadStatusRef
loadStatusRefOf = loadRef

-- | Evaluate a Lua chunk in the fixture's Lua state, answering its first
--   return value as the debug console renders it.
lua ∷ H → Text → IO Text
lua h = evalDebug (fxLua (hFx h))

-- | Register a Lua module whose callbacks the reconciliation broadcast
--   reaches, exactly as a loaded script is.
registerLuaModule ∷ H → Word32 → FilePath → Text → IO ()
registerLuaModule h sid path chunk = do
    let ls = fxLua (hFx h)
    ref ← Lua.runWith (lbsLuaState ls) $ do
        status ← Lua.dostring (TE.encodeUtf8 chunk) ∷ Lua.LuaE Lua.Exception Lua.Status
        case status of
            Lua.OK → Lua.ref Lua.registryindex
            _      → error ("fixture chunk failed to load: " ⧺ path)
    atomically $ modifyTVar' (lbsScripts ls) $ Map.insert sid LuaScript
        { scriptId = sid, scriptPath = path, scriptTickRate = 1000000
        , scriptNextTick = 1000000, scriptModuleRef = ref, scriptPaused = False }

-- | Remove registered modules again.
unregister ∷ H → [Word32] → IO ()
unregister h ids = atomically $
    modifyTVar' (lbsScripts (fxLua (hFx h))) (\m → foldr Map.delete m ids)

floraFixture ∷ FloraCatalog
floraFixture = catalog

-- | The crypt definition the container examples bind shells of, derived
--   from the shipped @ruin_small@.
registerCrypt ∷ Fx → IO ()
registerCrypt fx = do
    ruin ← locationRegistry (fxEnv fx) ≫= maybe (fail "ruin_small is not registered") pure ∘ lookupLocation "ruin_small"
    addLocationDef (fxEnv fx) (cryptDef ruin)

-- | Arm only the simulation wait bound.
armSeamsSimWait ∷ H → Double → IO ()
armSeamsSimWait h secs = armScenarioSeams (loadRef h) noSeams { seamSimWait = Just secs }

-- | Poll (bounded) until a condition holds; fail the example, never skip,
--   if it does not.
waitCondition ∷ Double → IO Bool → IO ()
waitCondition secs cond = do
    start ← getMonotonicTime
    let go = do
            ok ← cond
            unless ok $ do
                now ← getMonotonicTime
                if now - start > secs
                    then expectationFailure "the awaited condition never held"
                    else threadDelay 500 >> go
    go
