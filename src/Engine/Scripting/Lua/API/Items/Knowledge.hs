{-# LANGUAGE Strict #-}
-- | The Lua surface for the PORTABLE-container knowledge layer (#2512,
--   epic #1231 PLC-7): one READ verb, two OBSERVE verbs, and one FORGET
--   verb, all keyed by an item's own 'Item.Types.iiInstanceId'.
--
--   The building-keyed counterpart is
--   "Engine.Scripting.Lua.API.Buildings.Knowledge", and the READ verb
--   here answers with the SAME field names it does — @state@, @items@,
--   @storedWeight@, @revealedAt@, @capacity@ — so the shared container
--   window (PLC-9) consumes either projection without knowing which
--   kind of container it is looking at. The portable projection adds
--   @weighedAt@, which the building one has no analogue for: a building
--   is revealed all at once, while a crate can be hefted without ever
--   being opened.
--
--   __The three mutating verbs ENQUEUE.__ Each measures its
--   observation here — from the instance it located and the clock it
--   read, so the record describes the crate as it was when the player
--   looked — and hands the MERGE to the world thread, which owns the
--   session state 'World.State.Types.WorldManager' carries. A write
--   from this thread would race the two places that replace that state
--   wholesale (a load publish, an Exit-to-Menu teardown) and could land
--   a departed session's crate memory in the session that replaced it.
--   The same reasoning @world.markLocationContentsSpawned@ documents
--   for 'World.Types.wsGenParamsRef'.
--
--   So a @true@ means ACCEPTED, and the record is readable once the
--   world queue drains. The @false@ answers below are still decided
--   synchronously — locatability is a read, and a verb that cannot find
--   the instance enqueues nothing at all.
--
--   __PLC-7 ships no caller.__ Nothing in the shipped game observes a
--   container yet — pickup and open are PLC-8's, the window is PLC-9's
--   — so these verbs exist to be driven by tests, the console, and
--   those later slices. Registering them now is what lets the
--   persistence contract they carry be proven before any gameplay
--   depends on it.
module Engine.Scripting.Lua.API.Items.Knowledge
    ( itemGetContainerKnowledgeFn
    , itemObserveContainerWeightFn
    , itemObserveContainerContentsFn
    , itemForgetContainerKnowledgeFn
    ) where

import UPrelude
import qualified Data.Text.Encoding as TE
import qualified HsLua as Lua
import Data.IORef (readIORef)
import Engine.Core.Capability.Building
    (BuildingCapability(..), toBuildingCapability)
import Engine.Core.Capability.ContentRegistriesView
    (ContentRegistriesViewCapability(..), toContentRegistriesViewCapability)
import Engine.Core.Capability.UnitCombat
    (UnitCombatCapability(..), toUnitCombatCapability)
import Engine.Core.Capability.WorldSim
    (WorldSimCapability(..), toWorldSimCapability)
import Engine.Core.ReadOnlyRef (readReadOnlyRef)
import Engine.Core.State (EngineEnv)
import Engine.Scripting.Lua.API.Equipment (pushItemInstance)
import Item.Knowledge
import Item.Types (ItemInstance(..), ItemManager, ItemStorage(..))
import qualified Engine.Core.Queue as Q
import World.Command.Types (WorldCommand(..))
import World.Item.Locate (LocatedItem(..), locateItemInstanceIn, sessionGroundItems)
import World.State.Types (WorldManager(..))

-- | @item.getContainerKnowledge(instanceId)@ →
--   @{ state, items, storedWeight, weighedAt, revealedAt, capacity }@.
--
--   * @state@ is @"unknown"@ / @"weight-only"@ / @"empty"@ / @"known"@
--     — four DISTINCT answers. A consumer must render @"unknown"@ as
--     unknown and @"weight-only"@ as "you know how heavy it is, not
--     what is in it"; neither may be drawn as an empty list.
--   * __Every other field is present only when it is actually known.__
--     @items@ appears exactly when the contents have been observed (an
--     EMPTY table for @"empty"@ — observed and found empty, which is
--     not the same as absent), @storedWeight@ and @weighedAt@ exactly
--     when the container has been weighed, @revealedAt@ exactly when it
--     has been opened. Reading a missing field as @0@ would turn "never
--     learned" into "measured as nothing", which is the conflation the
--     four states exist to prevent.
--   * @storedWeight@ is the whole crate's remembered
--     'Item.Types.itemTotalWeight' — its own mass, its fill and
--     everything nested — because that is what a unit lifting it
--     actually feels. (The building projection's @storedWeight@ is the
--     weight of the STORED contents alone; a building is not liftable.)
--   * @capacity@ is ALWAYS LIVE, read from the located instance's
--     'Item.Types.iiStorage', and is simply ABSENT when the instance
--     cannot be located or declares no internal storage — never a
--     fabricated @0@, which "Item.Ownership" reads as a real
--     accepts-nothing capacity.
--
--   Answers a table for ANY instance id, including one nothing live
--   carries: "this crate is unknown to you" is a perfectly good answer
--   about an item you dropped three pages ago, and the caller has
--   'itemObserveContainerWeightFn' below to tell locatable from not.
--   nil only when the argument is not a positive number — a string, a
--   nil, or the never-minted id 0 — which is a caller bug rather than a
--   fact about any container.
itemGetContainerKnowledgeFn
    ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
itemGetContainerKnowledgeFn env = do
    idArg ← argInstanceId 1
    case idArg of
        Nothing → Lua.pushnil >> return 1
        Just iid → do
            (mRecord, mLocated, itemMgr) ← Lua.liftIO $ do
                mgr ← readIORef (wsWorldManagerRef (toWorldSimCapability env))
                located ← locateFor env iid
                im ← readReadOnlyRef (crvItemManagerRef
                                     (toContentRegistriesViewCapability env))
                pure (lookupPortable iid (wmPortableKnowledge mgr), located, im)
            Lua.newtable
            pushTextField "state"
                (portableKnowledgeStateId (portableRecordState mRecord))
            forM_ (mRecord ⌦ prContents) $ \c → do
                Lua.newtable
                forM_ (zip [1 ∷ Int ..] (coItems c)) $ \(i, item) → do
                    Lua.newtable
                    pushItemInstance item itemMgr
                    Lua.rawseti (-2) (fromIntegral i)
                Lua.setfield (-2) "items"
                pushNumberField "revealedAt" (coAt c)
            forM_ (mRecord ⌦ prWeight) $ \w → do
                pushNumberField "storedWeight" (realToFrac (woWeight w))
                pushNumberField "weighedAt" (woAt w)
            forM_ (mLocated ⌦ (iiStorage ∘ liInstance)) $ \st →
                pushNumberField "capacity" (realToFrac (isWeightCapacity st))
            return 1

-- | @item.observeContainerWeight(instanceId)@ → bool. Record that the
--   player has just HEFTED this container: its whole recursive weight
--   and the game-time second it was weighed, and nothing else. Any
--   existing contents observation survives untouched with its own older
--   stamp.
--
--   false — and nothing enqueued at all — when the id names nothing
--   live in the session. The observation is OF a physical item, so
--   there is nothing to weigh.
itemObserveContainerWeightFn
    ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
itemObserveContainerWeightFn = observeWith weighPortable

-- | @item.observeContainerContents(instanceId)@ → bool. Record that the
--   player has just OPENED this container: COPIES of its
--   'Item.Types.iiContents' as they are now, its weight, and both
--   stamps at the current game time. Replaces the whole record — an
--   open is a fresh look, never a diff.
--
--   A container nested INSIDE the one observed gets no record of its
--   own: its contents ride along inside the copy, but nobody opened it,
--   so it stays never-inspected until it is itself observed.
--
--   false, and nothing enqueued, when the id names nothing live.
itemObserveContainerContentsFn
    ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
itemObserveContainerContentsFn = observeWith openPortable

-- | @item.forgetContainerKnowledge(instanceId)@ → bool. Drop one
--   container's record; afterwards it reads as @"unknown"@ again.
--
--   Deliberately does NOT locate the instance: forgetting is about the
--   MEMORY, and the memory of a crate that has since been destroyed is
--   exactly the record a caller most wants to be able to clear. Answers
--   whether a record was there to drop when the verb ran; the drop
--   itself lands with the world thread's next drain, like the two
--   observations above.
itemForgetContainerKnowledgeFn
    ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
itemForgetContainerKnowledgeFn env = do
    idArg ← argInstanceId 1
    case idArg of
        Nothing → Lua.pushboolean False >> return 1
        Just iid → do
            dropped ← Lua.liftIO $ do
                let wsc = toWorldSimCapability env
                mgr ← readIORef (wsWorldManagerRef wsc)
                let had = isJust (lookupPortable iid (wmPortableKnowledge mgr))
                Q.writeQueue (wsWorldQueue wsc)
                    (WorldRecordPortableKnowledge iid Nothing)
                pure had
            Lua.pushboolean dropped
            return 1

-- | The shape both observation verbs share: locate, MEASURE, enqueue.
--
--   Measuring here rather than in the handler is the whole point — the
--   located instance and the clock are read together, so what is
--   remembered is the crate as it was at that instant rather than
--   whatever it holds by the time the world thread drains. Nothing here
--   writes to the live item, or to the map: observing a crate must
--   change neither.
observeWith
    ∷ (ItemManager → Double → ItemInstance → PortableObservation)
    → EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
observeWith build env = do
    idArg ← argInstanceId 1
    case idArg of
        Nothing → Lua.pushboolean False >> return 1
        Just iid → do
            ok ← Lua.liftIO $ do
                mLocated ← locateFor env iid
                case mLocated of
                    Nothing → pure False
                    Just located → do
                        let wsc = toWorldSimCapability env
                        now ← readIORef (wsGameTimeRef wsc)
                        itemMgr ← readReadOnlyRef (crvItemManagerRef
                            (toContentRegistriesViewCapability env))
                        Q.writeQueue (wsWorldQueue wsc)
                            (WorldRecordPortableKnowledge iid
                                (Just (build itemMgr now
                                             (liInstance located))))
                        pure True
            Lua.pushboolean ok
            return 1

-- | Read an item-instance id argument.
--
--   The 'Lua.TypeNumber' check is not redundant with 'Lua.tointeger':
--   that function COERCES, so a bare @Lua.tointeger@ accepts the STRING
--   @"47"@ as the id 47 and lets a caller passing an id it read out of a
--   text field silently act on a real crate. An id is a number or it is
--   not an id.
--
--   A negative value is refused for the same reason rather than wrapped
--   into the top of the 'Word64' range, and 0 is the never-minted
--   sentinel: neither can name a real instance, so both answer
--   'Nothing' — which every verb here reports as "no" rather than as an
--   error.
argInstanceId ∷ Lua.StackIndex → Lua.LuaE Lua.Exception (Maybe Word64)
argInstanceId i = do
    ty ← Lua.ltype i
    case ty of
        Lua.TypeNumber → do
            mn ← Lua.tointeger i
            pure $ case mn of
                Just n | n > 0 → Just (fromIntegral n)
                _              → Nothing
        _ → pure Nothing

-- | Locate one live instance across the whole session — every page's
--   ground items, unit inventories/equipment/accessories, and building
--   materials/storage, recursively.
locateFor ∷ EngineEnv → Word64 → IO (Maybe LocatedItem)
locateFor env iid = do
    mgr ← readIORef (wsWorldManagerRef (toWorldSimCapability env))
    ground ← sessionGroundItems mgr
    bm ← readIORef (bcBuildingManagerRef (toBuildingCapability env))
    um ← readIORef (ucUnitManagerRef (toUnitCombatCapability env))
    pure (locateItemInstanceIn ground bm um iid)

pushTextField ∷ Lua.Name → Text → Lua.LuaE Lua.Exception ()
pushTextField key val = do
    Lua.pushstring (TE.encodeUtf8 val)
    Lua.setfield (-2) key

pushNumberField ∷ Lua.Name → Double → Lua.LuaE Lua.Exception ()
pushNumberField key val = do
    Lua.pushnumber (Lua.Number val)
    Lua.setfield (-2) key
