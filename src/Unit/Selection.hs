{-# LANGUAGE Strict, UnicodeSyntax #-}
-- | Selection helpers on top of UnitManager. Selection state lives in
--   `UnitManager.umSelected` so the renderer and the info panel read from
--   the same atomic snapshot. All mutations go through atomicModifyIORef'.
--
--   "getSelected" filters out IDs that no longer have a live instance —
--   defensive against any code path that deletes from `umInstances`
--   without also touching `umSelected`. The primary cleanup happens in
--   `Unit.Thread.Command::handleUnitCommand` on `UnitDestroy`.
module Unit.Selection
    ( selectUnit
    , deselectUnit
    , clearSelection
    , setSelection
    , isSelected
    , getSelected
    ) where

import UPrelude
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import Data.IORef (readIORef, atomicModifyIORef')
import Engine.Core.State (EngineEnv(..), activeWorldPage)
import World.Page.Types (WorldPageId(..))
import Unit.Types (UnitManager(..), UnitInstance(..), UnitId(..))

-- | True when @uid@ is a live instance belonging to the active world —
--   the selection world-scoping predicate (#78). A unit in another world
--   can neither be selected nor reported as selected.
onActivePage ∷ Maybe WorldPageId → UnitManager → UnitId → Bool
onActivePage mPage um uid = case (mPage, HM.lookup uid (umInstances um)) of
    (Just pid, Just inst) → uiPage inst ≡ pid
    _                     → False

-- | Set the selection to exactly one unit, replacing any previous selection.
--   No-op if the unit doesn't exist or belongs to another world.
selectUnit ∷ EngineEnv → UnitId → IO Bool
selectUnit env uid = do
    mActive ← activeWorldPage env
    let mPage = fst <$> mActive
    atomicModifyIORef' (unitManagerRef env) $ \um →
        if onActivePage mPage um uid
        then (um { umSelected = HS.singleton uid }, True)
        else (um, False)

-- | Remove a single unit from the selection.
deselectUnit ∷ EngineEnv → UnitId → IO ()
deselectUnit env uid =
    atomicModifyIORef' (unitManagerRef env) $ \um →
        (um { umSelected = HS.delete uid (umSelected um) }, ())

-- | Empty the selection.
clearSelection ∷ EngineEnv → IO ()
clearSelection env =
    atomicModifyIORef' (unitManagerRef env) $ \um →
        (um { umSelected = HS.empty }, ())

-- | Replace the selection with a specific set. Filters out any IDs that
--   don't currently have a live unit instance in the active world.
setSelection ∷ EngineEnv → HS.HashSet UnitId → IO ()
setSelection env new = do
    mActive ← activeWorldPage env
    let mPage = fst <$> mActive
    atomicModifyIORef' (unitManagerRef env) $ \um →
        let live = HS.filter (onActivePage mPage um) new
        in (um { umSelected = live }, ())

isSelected ∷ EngineEnv → UnitId → IO Bool
isSelected env uid = do
    um ← readIORef (unitManagerRef env)
    mActive ← activeWorldPage env
    pure (HS.member uid (umSelected um) ∧ onActivePage (fst <$> mActive) um uid)

-- | Returns the current selection, filtered to live units of the active
--   world (selection is global state; reads are world-scoped, #78).
getSelected ∷ EngineEnv → IO (HS.HashSet UnitId)
getSelected env = do
    um ← readIORef (unitManagerRef env)
    mActive ← activeWorldPage env
    let mPage = fst <$> mActive
    pure (HS.filter (onActivePage mPage um) (umSelected um))
