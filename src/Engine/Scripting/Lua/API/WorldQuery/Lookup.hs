{-# LANGUAGE Strict #-}
-- | Shared read-only lookup helpers for the WorldQuery API family:
--   resolving the active world's tile data / gen params, the
--   currently VISIBLE world (for screen hit-testing), or a named
--   page's world state. Depended on by every WorldQuery.* submodule
--   that needs to resolve "which world" a query targets.
--
--   Narrowed to the @world-sim-render-handoff@ world\/sim capability
--   (#893, epic #537): every lookup here resolves through
--   'WorldSimCapability'\'s world-manager handle, never an 'EngineEnv'.
module Engine.Scripting.Lua.API.WorldQuery.Lookup
    ( getWorldTileData
    , worldStateByPage
    , targetWorldState
    , getWorldGenParams
    ) where

import UPrelude
import Data.IORef (readIORef)
import Engine.Core.Capability.WorldSim
    (WorldSimCapability(..))
import Engine.Core.State (activeWorldStateFrom)
import World.Types

-- | Helper: get the first active world's tile data
getWorldTileData ∷ WorldSimCapability → IO (Maybe WorldTileData)
getWorldTileData wsc = do
    mWs ← activeWorldStateFrom (wsWorldManagerRef wsc)
    case mWs of
        Just ws → Just <$> readIORef (wsTilesRef ws)
        Nothing → pure Nothing

-- | The 'WorldState' of a named page (any page in wmWorlds), or Nothing.
worldStateByPage ∷ WorldSimCapability → Text → IO (Maybe WorldState)
worldStateByPage wsc pidText = do
    mgr ← readIORef (wsWorldManagerRef wsc)
    pure (lookup (WorldPageId pidText) (wmWorlds mgr))

-- | Helper: get the first active world's gen params
getWorldGenParams ∷ WorldSimCapability → IO (Maybe WorldGenParams)
getWorldGenParams wsc = do
    mWs ← activeWorldStateFrom (wsWorldManagerRef wsc)
    case mWs of
        Just ws → readIORef (wsGenParamsRef ws)
        Nothing → pure Nothing

-- | The 'WorldState' a page-scoped query targets: the named page when
--   one is given, the active world otherwise.
--
--   Returning the STATE rather than just its tiles is what lets a point
--   query canonicalize before it looks a chunk up — the u-wrap width is
--   a property of the page ('pageWrapWorldSize'), so a query that only
--   ever saw the tile map could not resolve a seam alias at all.
targetWorldState ∷ WorldSimCapability → Maybe Text → IO (Maybe WorldState)
targetWorldState wsc = maybe (activeWorldStateFrom (wsWorldManagerRef wsc))
                             (worldStateByPage wsc)
