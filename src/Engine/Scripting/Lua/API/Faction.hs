{-# LANGUAGE Strict, UnicodeSyntax #-}
-- | The @faction@ Lua global (#912): the faction PROPERTIES and the
--   faction RELATION, exposed so unit AI asks the engine's own questions
--   instead of reimplementing them with string comparisons per script.
--
--   Every function here is pure over canonical faction tags — the same
--   strings @unit.getFaction@ returns — so the whole table needs no
--   'Engine.Core.State.EngineEnv' and can be registered into a bare Lua
--   state (which is what lets the headless suite exercise it directly).
--   An unrecognized tag resolves to 'Unit.Faction.fallbackFaction'
--   exactly as it does on the engine side, so Lua and Haskell can never
--   disagree about a tag neither recognizes.
module Engine.Scripting.Lua.API.Faction
  ( factionRelationFn
  , factionAreAlliesFn
  , factionCanAttackFn
  , factionIsPlayerOwnedFn
  , factionIsPlayerCommandableFn
  , factionHasUnrestrictedCombatFn
  ) where

import UPrelude
import qualified Data.Text.Encoding as TE
import qualified HsLua as Lua
import Unit.Faction
    ( Faction, areAllies, canAttack, factionFromTag, factionRelation
    , hasUnrestrictedCombat, isPlayerCommandable, isPlayerOwned
    , relationTag )

-- | Read a faction tag off the stack. A missing/non-string argument is
--   treated the same as an unrecognized tag — the inert fallback —
--   rather than erroring, so a nil from @unit.getFaction@ on a
--   just-destroyed unit degrades instead of tearing down a callback.
argFaction ∷ Lua.StackIndex → Lua.LuaE Lua.Exception Faction
argFaction i = do
    ty ← Lua.ltype i
    case ty of
        Lua.TypeString → do
            mbs ← Lua.tostring i
            pure $ maybe (factionFromTag "") (factionFromTag . TE.decodeUtf8Lenient) mbs
        _ → pure (factionFromTag "")

-- | Push a one-argument predicate over a faction tag.
predicate1 ∷ (Faction → Bool) → Lua.LuaE Lua.Exception Lua.NumResults
predicate1 p = do
    f ← argFaction 1
    Lua.pushboolean (p f)
    return 1

-- | Push a two-argument predicate over a pair of faction tags.
predicate2 ∷ (Faction → Faction → Bool) → Lua.LuaE Lua.Exception Lua.NumResults
predicate2 p = do
    a ← argFaction 1
    b ← argFaction 2
    Lua.pushboolean (p a b)
    return 1

-- | faction.relation(a, b) → "ally" | "neutral" | "hostile"
--   How faction @a@ regards faction @b@. Total and symmetric.
factionRelationFn ∷ Lua.LuaE Lua.Exception Lua.NumResults
factionRelationFn = do
    a ← argFaction 1
    b ← argFaction 2
    Lua.pushstring (TE.encodeUtf8 (relationTag (factionRelation a b)))
    return 1

-- | faction.areAllies(a, b) → bool
--   Would units of these two factions fight on the same side? Medic
--   triage and swarm rallying both ask exactly this.
factionAreAlliesFn ∷ Lua.LuaE Lua.Exception Lua.NumResults
factionAreAlliesFn = predicate2 areAllies

-- | faction.canAttack(attackerFaction, targetFaction) → bool
--   May a unit of the first faction be ordered to attack one of the
--   second? Hostility is the normal permission; a faction with
--   unrestricted combat on EITHER side overrides it.
factionCanAttackFn ∷ Lua.LuaE Lua.Exception Lua.NumResults
factionCanAttackFn = predicate2 canAttack

-- | faction.isPlayerOwned(f) → bool
--   Is this the player's OWN unit? Narrower than "allied with the
--   player" — see "Unit.Faction".
factionIsPlayerOwnedFn ∷ Lua.LuaE Lua.Exception Lua.NumResults
factionIsPlayerOwnedFn = predicate1 isPlayerOwned

-- | faction.isPlayerCommandable(f) → bool
--   Can a unit of this faction receive player orders?
factionIsPlayerCommandableFn ∷ Lua.LuaE Lua.Exception Lua.NumResults
factionIsPlayerCommandableFn = predicate1 isPlayerCommandable

-- | faction.hasUnrestrictedCombat(f) → bool
--   Does this faction ignore friendly-fire restrictions entirely?
factionHasUnrestrictedCombatFn ∷ Lua.LuaE Lua.Exception Lua.NumResults
factionHasUnrestrictedCombatFn = predicate1 hasUnrestrictedCombat
