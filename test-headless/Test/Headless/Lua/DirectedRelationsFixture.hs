{-# LANGUAGE Strict #-}
-- | Shared fixtures for the unit-id relation verbs' specs: "Lua faction
--   model" › "directed relation consumers (FTS-5)" (#2521) and "Faction
--   relation store" (#2548). A roster written into the unit manager, a
--   full production Lua session with the shipped faction catalogue and
--   the real unit AI, and the live relation-cause store as the overlay.
--
--   What is stubbed is never a relation answer; see
--   "Test.Headless.Lua.DirectedRelations" for the list of inputs no
--   relation question reads.
module Test.Headless.Lua.DirectedRelationsFixture
    ( fixturePage
    , prof
    , legacy
    , legacyOrder
    , resetWorld
    , hit
    , setPose
    , removeUnit
    , session
    , setCauses
    , cause
    , ok
    , val
    ) where

import UPrelude
import Test.Hspec
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import Data.IORef (atomicModifyIORef', writeIORef)
import Engine.Core.State (EngineEnv(..))
import Engine.Scripting.Lua.Types (LuaBackendState(..))
import Test.Headless.Unit.TransferApi
    (evalDebug, minimalDef, mkUnit, newBareLuaBackend)
import Test.Headless.Faction.RelationStore.Fixture (cause)
import Unit.Faction (Faction(..))
import Unit.Faction.Membership
    ( MembershipSource(..), TagMembership(..), UnitFactionProfile
    , localController, mkUnitFactionProfile, resolveLegacyFaction )
import Unit.Faction.Profile
    ( FactionCapability(..), LiveRelationCause
    , mkFactionTag, tagAcolyte )
import Unit.Faction.RelationStore
    (emptyRelationCauseStore, relationCauseStoreFromList)
import Unit.Types
import World.Page.Types (WorldPageId(..))
import World.State.Types (WorldManager(..), emptyWorldState, emptyWorldManager)

fixturePage ∷ WorldPageId
fixturePage = WorldPageId "fts5_page"

-- * Profiles

-- | A live profile from tag text, every tag a definition default. Fails
--   loudly on a bad fixture tag rather than silently dropping it.
prof ∷ Bool → [Text] → [FactionCapability] → UnitFactionProfile
prof controlled ts = mkUnitFactionProfile
    (if controlled then Just localController else Nothing)
    [ TagMembership (fromMaybe (error ("bad tag " ⧺ show t)) (mkFactionTag t))
                    MemberDefinitionDefault
    | t ← ts ]

-- | The approved D-26 legacy profile for a legacy faction value: the
--   acolyte definition's @acolyte@ default carried by @player@, no
--   defaults for the rest (so @wildlife@ falls back to its own tag).
legacy ∷ Faction → UnitFactionProfile
legacy f = resolveLegacyFaction (if f ≡ FactionPlayer then [tagAcolyte] else []) f

legacyOrder ∷ [Faction]
legacyOrder = [FactionPlayer, FactionWildlife, FactionHostile, FactionNeutral, FactionDebug]

-- * World fixtures

-- | Replace the roster with @rows@ (uid, profile, position), all on the
--   one visible page.
resetWorld ∷ EngineEnv → [(Int, UnitFactionProfile, (Float, Float))] → IO ()
resetWorld env rows = do
    ws ← emptyWorldState
    writeIORef (worldManagerRef env) emptyWorldManager
        { wmWorlds = [(fixturePage, ws)], wmVisible = [fixturePage] }
    writeIORef (unitManagerRef env) emptyUnitManager
        { umDefs = HM.fromList [("acolyte", minimalDef "acolyte" "Acolyte")]
        , umInstances = HM.fromList
            [ ( UnitId (fromIntegral uid)
              , (mkUnit "acolyte" FactionNeutral pos 100 [] [])
                    { uiFaction = p, uiPage = fixturePage } )
            | (uid, p, pos) ← rows ]
        , umNextId = 1000 }

-- | Record that @attacker@ struck @victim@ at game time @at@, as
--   Combat.Resolution does.
hit ∷ EngineEnv → Int → Int → Double → IO ()
hit env victim attacker at = atomicModifyIORef' (unitManagerRef env) $ \um →
    ( um { umInstances = HM.adjust
             (\u → u { uiLastAttackerUid = Just (fromIntegral attacker)
                     , uiLastAttackerAt  = at })
             (UnitId (fromIntegral victim)) (umInstances um) }
    , () )

setPose ∷ EngineEnv → Int → Text → IO ()
setPose env uid pose = atomicModifyIORef' (unitManagerRef env) $ \um →
    ( um { umInstances = HM.adjust (\u → u { uiPose = pose })
                                   (UnitId (fromIntegral uid)) (umInstances um) }
    , () )

removeUnit ∷ EngineEnv → Int → IO ()
removeUnit env uid = atomicModifyIORef' (unitManagerRef env) $ \um →
    (um { umInstances = HM.delete (UnitId (fromIntegral uid)) (umInstances um) }, ())

-- * Lua session

-- | A fresh Lua state with the full production API, the shipped faction
--   catalogue, the real unit AI loaded, an owned clock, and helpers.
session ∷ EngineEnv → IO LuaBackendState
session env = do
    -- A fresh session starts with NO live cause: the store is the
    -- engine's, not the Lua state's, so it would otherwise carry the
    -- previous case's overlay into this one.
    writeIORef (relationCauseStoreRef env) emptyRelationCauseStore
    ls ← newBareLuaBackend env
    ok ls $ T.unlines
        [ "assert(engine.loadFactionYaml('data/factions/base.yaml') ~= false)"
        , "AI = require('scripts.unit_ai')"
        , "CORE = require('scripts.unit_ai_core')"
        , "COMBAT = require('scripts.unit_ai_combat')"
        , "ATTACK = require('scripts.unit_ai_combat_attack')"
        , "POLICY = require('scripts.unit_ai_combat_policy')"
        -- A fixture that drops a unit's AI row to start it afresh also
        -- drops the seam's uid-keyed bookkeeping with CLAIMS.resetAll().
        , "CLAIMS = require('scripts.unit_ai_claims')"
        , "NOW = 0; engine.gameTime = function() return NOW end"
        , "function S(uid) return CORE.ensureState(uid) end"
        -- One autonomous engage decision; the chosen target or false.
        , "function engage(uid) local s = S(uid)"
        , "  if COMBAT.engageUtility(uid, s, {}) == -math.huge then return false end"
        , "  COMBAT.engageExecute(uid, s, {}); return s.attackTargetUid or false end"
        -- One attack_target decision; the target still held, or false.
        , "function tick(uid) local s = S(uid)"
        , "  ATTACK.attackTargetExecute(uid, s, {})"
        , "  if not CORE.isGoalActive(s, 'attack') then return false end"
        , "  return s.attackTargetUid or false end"
        , "return true"
        ]
    pure ls

-- | Install a live overlay through the PRODUCTION owner: the persisted
--   relation-cause store (#2548) the unit verbs read on every call.
setCauses ∷ EngineEnv → [LiveRelationCause] → IO ()
setCauses env =
    writeIORef (relationCauseStoreRef env) ∘ relationCauseStoreFromList

-- | Run a chunk that must return @true@.
ok ∷ LuaBackendState → Text → IO ()
ok ls chunk = evalDebug ls chunk `shouldReturn` "true"

-- | Evaluate an expression.
val ∷ LuaBackendState → Text → IO Text
val ls expr = evalDebug ls ("return " <> expr)
