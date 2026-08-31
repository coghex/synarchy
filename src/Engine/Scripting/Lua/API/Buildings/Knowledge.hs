{-# LANGUAGE Strict #-}
-- | The Lua surface for the container-knowledge layer (#1087, epic
--   #1013 phase A3): one READ verb C1 renders a container panel from,
--   and one REFRESH verb C3 calls when a Mode A session opens. A3 ships
--   no UI itself.
--
--   Both are keyed by container identity and both are PAGE-CORRECT —
--   they resolve the building's own 'Building.Types.biPage' rather than
--   whichever page happens to be active, so a cargo hold on a hidden
--   page reports and refreshes its own page's memory.
module Engine.Scripting.Lua.API.Buildings.Knowledge
    ( buildingGetContainerKnowledgeFn
    , buildingGetRememberedItemContentsFn
    , buildingRefreshContainerKnowledgeFn
    ) where

import UPrelude
import Engine.Core.Capability.Building
    (BuildingCapability(..), toBuildingCapability)
import Engine.Core.Capability.ContentRegistriesView
    (ContentRegistriesViewCapability(..), toContentRegistriesViewCapability)
import Engine.Core.Capability.WorldSim (toWorldSimCapability)
import qualified Data.Text.Encoding as TE
import qualified Data.HashMap.Strict as HM
import qualified HsLua as Lua
import Data.IORef (readIORef)
import Engine.Core.ReadOnlyRef (readReadOnlyRef)
import Engine.Core.State (EngineEnv)
import Building.Knowledge
import Building.Knowledge.Live
    (ContainerObserver, containerObserver, readContainerKnowledge
    , revealContainer)
import Building.Types
    (BuildingId(..), BuildingDef(..), BuildingInstance(..), BuildingManager(..))
import Engine.Scripting.Lua.API.Equipment (pushItemInstance)
import Engine.Scripting.Lua.API.Items.Contents
    (pushGroupedContents, readInstanceIdPath, resolveContainedItem)
import Item.Types (ItemInstance(..))

-- | @building.getContainerKnowledge(bid)@ →
--   @{ state, items, storedWeight, capacity, revealedAt }@ | nil.
--
--   * @state@ is @"unknown"@ / @"empty"@ / @"known"@ — three DISTINCT
--     answers. A consumer must render @"unknown"@ as "unknown", never
--     as an empty list: never-inspected and known-empty are different
--     facts about the world.
--   * @items@ are the REMEMBERED instances (same table shape as
--     @building.getStorage@ / @unit.getInventory@ entries, so C1 can
--     reuse one renderer), copied at reveal time — quality, condition,
--     fill, sharpness, nested contents and instance id as they were
--     THEN, not as they are now. Empty for @"unknown"@ and @"empty"@.
--   * @storedWeight@ is the remembered total mass, derived from that
--     same remembered list. 0 for @"unknown"@.
--   * @capacity@ is ALWAYS LIVE, read straight from the def — the
--     player knows how big a thing they built, so only the contents go
--     stale.
--   * @revealedAt@ is the GAME-TIME second the observation was taken
--     (the clock @unit.getInfo@\'s @animStart@ and @building.spawn@\'s
--     own timestamp use), for C1/D1's "as of…" rendering. nil for
--     @"unknown"@.
--
--   nil (rather than a table) only when @bid@ names no live building,
--   or its page is gone — the same "don't offer this at all" answer
--   @unit.transferReceiverInfo@ gives.
--
--   Since #1237 this is a LIVE read: @scripts/cargo_inventory_panel.lua@
--   builds a building endpoint's whole view from it (contents, stored
--   weight, capacity and the "as of…" age), so the three-state
--   distinction and the nil @revealedAt@ above are load-bearing rather
--   than prospective. Its own gate is
--   @--match \"LAST-KNOWN container contents\"@ plus
--   @tools\/item_list_widget_probe.py@.
buildingGetContainerKnowledgeFn
    ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
buildingGetContainerKnowledgeFn env = do
    idArg ← Lua.tointeger 1
    case idArg of
        Nothing → Lua.pushnil >> return 1
        Just n → do
            let bid = BuildingId (fromIntegral n)
            mKnown ← Lua.liftIO $ readContainerKnowledge (observerFor env) bid
            case mKnown of
                Nothing → Lua.pushnil >> return 1
                Just mRecord → do
                    (cap, itemMgr) ← Lua.liftIO $ do
                        bm ← readIORef
                            (bcBuildingManagerRef (toBuildingCapability env))
                        im ← readReadOnlyRef (crvItemManagerRef
                                            (toContentRegistriesViewCapability env))
                        let c = fromMaybe 0 $ do
                                inst ← HM.lookup bid (bmInstances bm)
                                def  ← HM.lookup (biDefName inst) (bmDefs bm)
                                pure (bdStorageCapacity def)
                        pure (c, im)
                    Lua.newtable
                    pushTextField "state"
                        (containerKnowledgeStateId (recordState mRecord))
                    Lua.newtable
                    forM_ (zip [1 ∷ Int ..] (maybe [] crItems mRecord)) $
                        \(i, item) → do
                            Lua.newtable
                            pushItemInstance item itemMgr
                            Lua.rawseti (-2) (fromIntegral i)
                    Lua.setfield (-2) "items"
                    pushNumberField "storedWeight"
                        (realToFrac (maybe 0 crStoredWeight mRecord))
                    pushNumberField "capacity" (realToFrac cap)
                    case mRecord of
                        Just r  → pushNumberField "revealedAt" (crRevealedAt r)
                        Nothing → pure ()
                    return 1

-- | @building.getRememberedItemContents(bid, path)@ →
--   @{ items, revealedAt }@ | nil (#1238).
--
--   The REMEMBERED contents of an item-container stored inside a
--   container building — the read the window stack opens a
--   building-side nested level from. @path@ is a dense array of
--   instance ids descending through the record's own remembered items:
--   the first selects among the top-level remembered instances, each
--   later one descends that item's nested contents.
--
--   Three properties this must keep, all of them load-bearing:
--
--     * __It is the MEMORY, never the live building.__ Everything
--       comes out of the 'Building.Knowledge.ContainerRecord' the last
--       reveal wrote — so a nested level keeps showing what the player
--       last saw even after a unit has emptied the real container, and
--       reports the PARENT record's @revealedAt@, which is what lets
--       the level carry the same \"as of…\" age as the window it
--       opened from.
--     * __It reveals nothing.__ A pure read through
--       'readContainerKnowledge', exactly like
--       'buildingGetContainerKnowledgeFn' above; opening a window at
--       any depth must never refresh the memory it is rendering.
--     * __It descends by exact instance identity.__ Two same-def kits
--       remembered side by side answer with their own contents, and an
--       id that is no longer in the record answers nil rather than a
--       sibling's — the caller closes that level instead of silently
--       retargeting.
--
--   nil when @bid@ names no live building or its page is gone, when
--   nothing has ever been observed there, when @path@ is malformed or
--   empty, or when it does not resolve. The @items@ rows are the SAME
--   grouped shape @unit.getItemContents@ answers with
--   ('Engine.Scripting.Lua.API.Items.Contents'), so one Lua level
--   renderer draws a live level and a remembered one alike.
buildingGetRememberedItemContentsFn
    ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
buildingGetRememberedItemContentsFn env = do
    idArg ← Lua.tointeger 1
    mPath ← readInstanceIdPath 2
    case (idArg, mPath) of
        (Just n, Just path) | not (null path) → do
            let bid = BuildingId (fromIntegral n)
            mKnown ← Lua.liftIO $ readContainerKnowledge (observerFor env) bid
            case mKnown of
                Just (Just record)
                  | Just held ← resolveContainedItem path (crItems record) → do
                    itemMgr ← Lua.liftIO $ readReadOnlyRef
                        (crvItemManagerRef (toContentRegistriesViewCapability env))
                    Lua.newtable
                    pushGroupedContents itemMgr (iiContents held)
                    Lua.setfield (-2) "items"
                    pushNumberField "revealedAt" (crRevealedAt record)
                    return 1
                _ → Lua.pushnil >> return 1
        _ → Lua.pushnil >> return 1

-- | @building.refreshContainerKnowledge(bid)@ → bool. Take a fresh
--   observation of the container's CURRENT contents and replace the
--   whole record — the verb #1013's C3 calls when a Mode A session
--   opens on the container. Since #1250 that caller exists:
--   @scripts\/transfer_session.lua@ calls it exactly once, on the
--   session's transition to the open\/held state, so the escort's
--   container panel shows a fresh snapshot. Nothing else in the shipped
--   game calls it — merely opening the ordinary container window must
--   still reveal nothing.
--
--   Deliberately NOT unit-gated: this is the player themself looking
--   inside through an opened session, not a unit's interaction, so
--   there is no acting unit whose faction could be asked. The reveals
--   that DO have an acting unit
--   ('Building.Knowledge.Live.revealContainerForUnit') all go through
--   the one 'Unit.Faction.isPlayerCommandable' gate instead.
--
--   Returns false when @bid@ names no live building or its page is
--   gone.
buildingRefreshContainerKnowledgeFn
    ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
buildingRefreshContainerKnowledgeFn env = do
    idArg ← Lua.tointeger 1
    case idArg of
        Nothing → Lua.pushboolean False >> return 1
        Just n → do
            ok ← Lua.liftIO $ revealContainer (observerFor env)
                                              (BuildingId (fromIntegral n))
            Lua.pushboolean ok
            return 1

observerFor ∷ EngineEnv → ContainerObserver
observerFor env = containerObserver
    (toBuildingCapability env) (toWorldSimCapability env)
    (toContentRegistriesViewCapability env)

pushTextField ∷ Lua.Name → Text → Lua.LuaE Lua.Exception ()
pushTextField key val = do
    Lua.pushstring (TE.encodeUtf8 val)
    Lua.setfield (-2) key

pushNumberField ∷ Lua.Name → Double → Lua.LuaE Lua.Exception ()
pushNumberField key val = do
    Lua.pushnumber (Lua.Number val)
    Lua.setfield (-2) key
