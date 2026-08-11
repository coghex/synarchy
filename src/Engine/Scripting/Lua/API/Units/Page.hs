{-# LANGUAGE Strict #-}
-- | The one owning-page resolver the ground↔inventory verbs share
--   (#1208).
--
--   Ground items are page-local ('World.State.Types.wsGroundItemsRef')
--   and every unit records the page it lives on
--   ('Unit.Types.Instance.uiPage'), but @item.pickupGround@,
--   @unit.dropEquipmentToGround@, @unit.dropItemToGround@ and
--   @unit.dropItemById@ used to resolve the ACTIVE world and then look
--   the unit up globally — so an off-active-page unit picked from, and
--   dropped onto, a page it is not standing on, teleporting an exact
--   'Item.Types.ItemInstance' between worlds with no traversal.
--
--   All four now resolve through 'unitOwningWorldState': look the live
--   unit up by uid, take its @uiPage@, and find exactly that page in
--   @wmWorlds@ — with NO active-world fallback, so a unit whose page
--   has no live world fails rather than landing on someone else's. It
--   is one function on purpose: four private copies are exactly how the
--   defect got in, and Requirement 2 of #1208 asks that they cannot
--   drift apart again.
--
--   Precedent: 'Engine.Scripting.Lua.API.Units.Inventory.unitAmbientTemp'
--   already reads the unit's own page this way, and the strict transfer
--   policy ('Unit.Transfer') independently refuses cross-page endpoints.
module Engine.Scripting.Lua.API.Units.Page
    ( unitOwningWorldState
    ) where

import UPrelude
import Engine.Core.Capability.UnitCombat
    (UnitCombatCapability(..), toUnitCombatCapability)
import Engine.Core.Capability.WorldSim
    (WorldSimCapability(..), toWorldSimCapability)
import qualified Data.HashMap.Strict as HM
import Data.IORef (readIORef)
import Engine.Core.State (EngineEnv)
import Unit.Types (UnitId, UnitInstance(..), UnitManager(..))
import World.Types (WorldManager(..), WorldState, wmWorlds)

-- | The live 'WorldState' of the page a unit is ON, or 'Nothing' when
--   the unit does not exist or its page has no live world.
--
--   Deliberately NOT an active-world fallback: a caller that cannot
--   find the unit's own page must fail, because the active page is
--   asynchronous state that has nothing to do with where this unit
--   stands.
unitOwningWorldState ∷ EngineEnv → UnitId → IO (Maybe WorldState)
unitOwningWorldState env uid = do
    um ← readIORef (ucUnitManagerRef (toUnitCombatCapability env))
    case HM.lookup uid (umInstances um) of
        Nothing   → pure Nothing
        Just inst → do
            wm ← readIORef (wsWorldManagerRef (toWorldSimCapability env))
            pure $ lookup (uiPage inst) (wmWorlds wm)
