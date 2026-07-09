{-# LANGUAGE Strict, UnicodeSyntax #-}
-- | Shared read-only lookup helpers for the WorldQuery API family:
--   resolving the active world's tile data / gen params, the
--   currently VISIBLE world (for screen hit-testing), or a named
--   page's world state. Depended on by every WorldQuery.* submodule
--   that needs to resolve "which world" a query targets.
module Engine.Scripting.Lua.API.WorldQuery.Lookup
    ( getWorldTileData
    , mVisibleWorldState
    , worldStateByPage
    , getWorldGenParams
    ) where

import UPrelude
import Data.IORef (readIORef)
import Engine.Core.State (EngineEnv(..), activeWorldState)
import World.Types

-- | Helper: get the first active world's tile data
getWorldTileData ∷ EngineEnv → IO (Maybe WorldTileData)
getWorldTileData env = do
    mWs ← activeWorldState env
    case mWs of
        Just ws → Just <$> readIORef (wsTilesRef ws)
        Nothing → pure Nothing

-- | Helper: the WorldState of the currently VISIBLE world (head of
--   wmVisible), looked up in wmWorlds. This is the world rendering and
--   building operate on; a hidden page can sit at the wmWorlds head, so
--   the raw head is not a safe proxy for "what the player sees".
mVisibleWorldState ∷ WorldManager → Maybe WorldState
mVisibleWorldState manager = case wmVisible manager of
    (pageId:_) → lookup pageId (wmWorlds manager)
    []         → Nothing

-- | The 'WorldState' of a named page (any page in wmWorlds), or Nothing.
worldStateByPage ∷ EngineEnv → Text → IO (Maybe WorldState)
worldStateByPage env pidText = do
    mgr ← readIORef (worldManagerRef env)
    pure (lookup (WorldPageId pidText) (wmWorlds mgr))

-- | Helper: get the first active world's gen params
getWorldGenParams ∷ EngineEnv → IO (Maybe WorldGenParams)
getWorldGenParams env = do
    mWs ← activeWorldState env
    case mWs of
        Just ws → readIORef (wsGenParamsRef ws)
        Nothing → pure Nothing
