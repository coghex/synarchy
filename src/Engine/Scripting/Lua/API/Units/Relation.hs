{-# LANGUAGE Strict #-}
-- | The unit-id-based relation, alliance and attack-permission queries
--   (#2521, FTS-5 of the faction-tag arc #2496), beside FTS-4's property
--   queries in "Engine.Scripting.Lua.API.Units.Ownership":
--
--   * @unit.relationFromTo(a, b)@  → @"ally"@ | @"neutral"@ | @"hostile"@
--   * @unit.isFriendlyToward(a, b)@ → bool
--   * @unit.canAttack(a, b)@       → bool
--
--   All three are DIRECTED — @a@'s regard of @b@ — and are answered by
--   "Unit.Faction.Membership"'s 'unitRelationFromTo' /
--   'unitIsFriendlyToward' / 'unitCanAttack', i.e. FTS-1's
--   'Unit.Faction.Profile.relationFromTo' over both live units' policy
--   profiles, the shipped catalogue's base table
--   ('Unit.Faction.Catalogue.cataloguePolicy'), and a live overlay.
--   Scripts never inspect tags or reproduce the precedence.
--
--   @unit.canAttack@ is the PERMISSION of requirement 3 — directed
--   hostility, or unrestricted combat on either side. The design's
--   Runtime API names the local-awareness variant @canAttackNow@; that
--   awareness is owned by the Lua combat-policy seam
--   (@scripts\/unit_ai_combat_policy.lua@), not by this verb, hence the
--   different name.
--
--   __The live overlay is a registration-time input.__ The runtime has
--   no producer of live causes yet (FTS-6 owns attack orders, incidents
--   and diplomacy), so production registers these verbs with an EMPTY
--   overlay ('Engine.Scripting.Lua.API.Register.Unit.registerUnitAPI').
--   A fixture that needs one calls the same registrar,
--   'Engine.Scripting.Lua.API.Register.Unit.registerUnitAPIWith', with
--   its causes; there is no mutation verb and no engine state for it.
--
--   __Fails closed.__ Anything that does not name two live units — a
--   missing or non-number argument (a numeric string included), a
--   fractional or out-of-range id, an id with no unit — answers
--   @"neutral"@ / @false@ and never raises. Units are looked up in the
--   global roster, so a unit on an inactive page answers too.
module Engine.Scripting.Lua.API.Units.Relation
  ( unitRelationFromToFn
  , unitIsFriendlyTowardFn
  , unitCanAttackFn
  ) where

import UPrelude
import Engine.Core.Capability.ContentRegistries
    (ContentRegistriesCapability(..), toContentRegistriesCapability)
import Engine.Core.Capability.UnitCombat
    (UnitCombatCapability(..), toUnitCombatCapability)
import Engine.Core.State (EngineEnv)
import qualified Data.HashMap.Strict as HM
import qualified Data.Text.Encoding as TE
import qualified HsLua as Lua
import Data.IORef (readIORef)
import Unit.Faction (FactionRelation(..), relationTag)
import Unit.Faction.Catalogue (cataloguePolicy)
import Unit.Faction.Membership
    ( UnitFactionProfile, unitCanAttack, unitIsFriendlyToward
    , unitRelationFromTo )
import Unit.Faction.Profile
    (FactionPolicy, LiveRelationCause, withLiveCauses)
import Unit.Types

-- | unit.relationFromTo(a, b) → "ally" | "neutral" | "hostile"
unitRelationFromToFn ∷ EngineEnv → [LiveRelationCause]
                     → Lua.LuaE Lua.Exception Lua.NumResults
unitRelationFromToFn env overlay = do
    r ← pairQuery env overlay RelNeutral unitRelationFromTo
    Lua.pushstring (TE.encodeUtf8 (relationTag r))
    return 1

-- | unit.isFriendlyToward(a, b) → bool
unitIsFriendlyTowardFn ∷ EngineEnv → [LiveRelationCause]
                       → Lua.LuaE Lua.Exception Lua.NumResults
unitIsFriendlyTowardFn env overlay = do
    b ← pairQuery env overlay False unitIsFriendlyToward
    Lua.pushboolean b
    return 1

-- | unit.canAttack(a, b) → bool
unitCanAttackFn ∷ EngineEnv → [LiveRelationCause]
                → Lua.LuaE Lua.Exception Lua.NumResults
unitCanAttackFn env overlay = do
    b ← pairQuery env overlay False unitCanAttack
    Lua.pushboolean b
    return 1

-- | Resolve arguments 1 and 2 to live profiles and answer @q@ under the
--   catalogue's policy plus @overlay@; @fallback@ for anything else.
pairQuery ∷ EngineEnv → [LiveRelationCause] → α
          → (FactionPolicy → UnitFactionProfile → UnitFactionProfile → α)
          → Lua.LuaE Lua.Exception α
pairQuery env overlay fallback q = do
    ma ← unitIdArg 1
    mb ← unitIdArg 2
    case (ma, mb) of
        (Just a, Just b) → Lua.liftIO $ do
            um  ← readIORef (ucUnitManagerRef (toUnitCombatCapability env))
            cat ← readIORef (crFactionCatalogueRef
                                (toContentRegistriesCapability env))
            let pol = withLiveCauses overlay (cataloguePolicy cat)
                prof u = uiFaction <$> HM.lookup u (umInstances um)
            pure $ case (prof a, prof b) of
                (Just pa, Just pb) → q pol pa pb
                _                  → fallback
        _ → pure fallback

-- | A unit id argument: a Lua NUMBER (never a coerced string) holding an
--   integer inside the 'Word32' range.
unitIdArg ∷ Lua.StackIndex → Lua.LuaE Lua.Exception (Maybe UnitId)
unitIdArg i = do
    ty ← Lua.ltype i
    mN ← if ty ≡ Lua.TypeNumber then Lua.tointeger i else pure Nothing
    pure $ case mN of
        Just n | n ≥ 0, n ≤ fromIntegral (maxBound ∷ Word32)
               → Just (UnitId (fromIntegral n))
        _      → Nothing
