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
import Engine.Core.State (EngineEnv(..))
import Unit.Types (UnitManager(..), UnitId(..))

-- | Set the selection to exactly one unit, replacing any previous selection.
--   No-op if the unit doesn't exist.
selectUnit ∷ EngineEnv → UnitId → IO Bool
selectUnit env uid =
    atomicModifyIORef' (unitManagerRef env) $ \um →
        if HM.member uid (umInstances um)
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
--   don't currently have a live unit instance.
setSelection ∷ EngineEnv → HS.HashSet UnitId → IO ()
setSelection env new =
    atomicModifyIORef' (unitManagerRef env) $ \um →
        let live = HS.filter (`HM.member` umInstances um) new
        in (um { umSelected = live }, ())

isSelected ∷ EngineEnv → UnitId → IO Bool
isSelected env uid = do
    um ← readIORef (unitManagerRef env)
    pure (HS.member uid (umSelected um))

-- | Returns the current selection, filtered to only live unit IDs.
getSelected ∷ EngineEnv → IO (HS.HashSet UnitId)
getSelected env = do
    um ← readIORef (unitManagerRef env)
    pure (HS.filter (`HM.member` umInstances um) (umSelected um))
