-- | Test-only debug verb for the live gameplay-tag query (#2705),
--   registered on the @debug@ table beside @debug.armLoadStageGate@ —
--   deliberately NOT on a player-facing namespace, following
--   @engine.debugThrow@'s precedent for a test-only binding that ships
--   in engine code.
--
--   It exists so @tools/gameplay_tag_persistence_probe.py@ can show,
--   through a real engine, what the PRODUCTION live query
--   ('Gameplay.Tags.Live.runLiveTagQueryIn' over
--   'Gameplay.Tags.Live.captureLiveTagView') returns for a tile or plant
--   tag while its chunk is evicted and after readmission. Read-only: it
--   captures one view and evaluates one query, and writes nothing.
module Engine.Scripting.Lua.API.TagQueryDebug
  ( debugLiveTagQueryFn
  ) where

import UPrelude
import Data.Maybe (mapMaybe)
import qualified Data.Text.Encoding as TE
import Engine.Core.Capability.Building (BuildingCapability(..), toBuildingCapability)
import Engine.Core.Capability.UnitCombat (UnitCombatCapability(..), toUnitCombatCapability)
import Engine.Core.Capability.WorldSim (WorldSimCapability(..), toWorldSimCapability)
import Engine.Core.State (EngineEnv)
import Gameplay.Tags.Live (captureLiveTagView, runLiveTagQueryIn)
import Gameplay.Tags.Query (tagged, targetSetsToList)
import Gameplay.Tags.Types
import World.Flora.Identity (floraInstanceIdToLua)
import World.Page.Types (WorldPageId(..))
import qualified HsLua as Lua

-- | @debug.liveTagQuery(tag) → { target, ... }@
--
--   The live TILE and PLANT targets carrying @tag@, one string each in
--   the query's result order: @"tile:<page>:<x>,<y>,<z>"@,
--   @"plant:<page>:<instanceId>"@ or @"plot:<page>:<x>,<y>"@. A
--   non-string or empty tag answers an empty table.
debugLiveTagQueryFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
debugLiveTagQueryFn env = do
    isStr ← (≡ Lua.TypeString) ⊚ Lua.ltype 1
    raw ← if isStr then Lua.tostring 1 else pure Nothing
    let mTag = raw ⌦ (mkGameplayTag . TE.decodeUtf8Lenient)
    targets ← case mTag of
        Nothing → pure []
        Just tg → Lua.liftIO $ do
            v ← captureLiveTagView
                    (ucUnitManagerRef (toUnitCombatCapability env))
                    (bcBuildingManagerRef (toBuildingCapability env))
                    (wsWorldManagerRef (toWorldSimCapability env))
            pure (targetSetsToList
                    (runLiveTagQueryIn [TileCategory, PlantCategory] v id (tagged tg)))
    Lua.newtable
    forM_ (zip [1 ..] (mapMaybe render targets)) $ \(i, s) → do
        Lua.pushstring (TE.encodeUtf8 s)
        Lua.rawseti (-2) i
    pure 1
  where
    render = \case
        TargetTile t → Just $ "tile:" <> unWorldPageId (tileTargetPage t) <> ":"
            <> tshow (tileTargetX t) <> "," <> tshow (tileTargetY t) <> "," <> tshow (tileTargetZ t)
        TargetPlant p → Just $ case (plantTargetId p, plantTargetPlotTile p) of
            (Just i, _) → "plant:" <> unWorldPageId (plantTargetPage p) <> ":" <> tshow (floraInstanceIdToLua i)
            (_, Just (x, y)) → "plot:" <> unWorldPageId (plantTargetPage p) <> ":" <> tshow x <> "," <> tshow y
            _ → "plant:" <> unWorldPageId (plantTargetPage p) <> ":?"
        _ → Nothing
