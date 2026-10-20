{-# LANGUAGE Strict #-}
-- | The unit-id-based ownership, commandability and unrestricted-combat
--   queries (#2518, FTS-4 of the faction-tag arc #2496):
--
--   * @unit.isPlayerOwned(uid)@         → bool
--   * @unit.isPlayerCommandable(uid)@   → bool
--   * @unit.hasUnrestrictedCombat(uid)@ → bool
--
--   The names are the design's Runtime API ones
--   (@docs\/faction_tag_system_design.md@ §"Runtime API"), asked of a
--   UNIT rather than of a faction string: each reads the unit's live
--   faction profile and answers through
--   'Unit.Faction.Membership.unitIsOwnedBy',
--   'Unit.Faction.Membership.unitIsCommandableBy' and
--   'Unit.Faction.Membership.unitHasUnrestrictedCombat' — FTS-1's
--   profile queries and nothing else. Ownership and commandability are
--   observer-relative; these verbs are the Lua entry point, so they name
--   the observer, 'Unit.Faction.Membership.localController'.
--   Unrestricted combat takes no observer at all.
--
--   The string-taking @faction.isPlayerOwned@, @faction.isPlayerCommandable@
--   and @faction.hasUnrestrictedCombat@ verbs stay for the compatibility
--   window (FTS-6) and gain no callers (D-33).
--
--   __Never throws.__ A missing argument, a non-number (a numeric
--   STRING included: 'Lua.tointeger' would coerce @"47"@), a fractional
--   or out-of-range number, and an id with no live unit all answer
--   @false@. The unit is looked up in the GLOBAL roster, so a unit on an
--   inactive page answers exactly as one on the active page does —
--   @scripts\/tutorial_eval.lua@ evaluates hidden pages.
--
--   Registered directly from "Engine.Scripting.Lua.API.Register.Unit"
--   rather than through the 'Engine.Scripting.Lua.API.Units' facade,
--   whose export list is frozen at the pre-split surface — the same
--   route the transfer (#1000) and medical (#2297) verbs take.
module Engine.Scripting.Lua.API.Units.Ownership
  ( unitIsPlayerOwnedFn
  , unitIsPlayerCommandableFn
  , unitHasUnrestrictedCombatFn
  ) where

import UPrelude
import Engine.Core.Capability.UnitCombat
    (UnitCombatCapability(..), toUnitCombatCapability)
import Engine.Core.State (EngineEnv)
import qualified Data.HashMap.Strict as HM
import qualified HsLua as Lua
import Data.IORef (readIORef)
import Unit.Faction.Membership
    ( UnitFactionProfile, localController, unitHasUnrestrictedCombat
    , unitIsCommandableBy, unitIsOwnedBy )
import Unit.Types

-- | unit.isPlayerOwned(uid) → bool
--   Is this the local player's OWN unit? Narrower than "allied with the
--   player": a debug spawn and an uncontrolled acolyte are both false.
unitIsPlayerOwnedFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
unitIsPlayerOwnedFn = unitProperty (unitIsOwnedBy localController)

-- | unit.isPlayerCommandable(uid) → bool
--   May this unit receive the local player's orders? Ownership, or the
--   local-commandability capability a debug spawn carries.
unitIsPlayerCommandableFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
unitIsPlayerCommandableFn = unitProperty (unitIsCommandableBy localController)

-- | unit.hasUnrestrictedCombat(uid) → bool
--   Does this unit ignore friendly-fire restrictions entirely?
unitHasUnrestrictedCombatFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
unitHasUnrestrictedCombatFn = unitProperty unitHasUnrestrictedCombat

-- | One profile predicate over the unit named by argument 1, @false@
--   for anything that does not name a live unit.
unitProperty ∷ (UnitFactionProfile → Bool) → EngineEnv
             → Lua.LuaE Lua.Exception Lua.NumResults
unitProperty p env = do
    ty ← Lua.ltype 1
    mN ← if ty ≡ Lua.TypeNumber then Lua.tointeger 1 else pure Nothing
    answer ← case mN of
        Just n | n ≥ 0, n ≤ fromIntegral (maxBound ∷ Word32) → Lua.liftIO $ do
            um ← readIORef (ucUnitManagerRef (toUnitCombatCapability env))
            pure $ maybe False (p ∘ uiFaction)
                         (HM.lookup (UnitId (fromIntegral n)) (umInstances um))
        _ → pure False
    Lua.pushboolean answer
    return 1
