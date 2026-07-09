{-# LANGUAGE Strict, UnicodeSyntax #-}
-- | Shared read-only helpers for the Forage API family (#94/#332):
--   resolving the flora instances on a tile and the world's growth
--   clock. Depended on by every Forage.* submodule that needs to look
--   up flora state.
module Engine.Scripting.Lua.API.Forage.Lookup
    ( floraAt
    , growthClock
    ) where

import UPrelude
import Data.IORef (readIORef)
import Engine.Core.State (EngineEnv(..))
import World.Types
import World.Generate.Coordinates (globalToChunk)

-- | Every flora instance on tile (gx, gy) of the active world's loaded
--   chunks, joined with its species. Empty when the chunk isn't loaded.
floraAt ∷ EngineEnv → WorldState → Int → Int
        → IO [(FloraInstance, FloraSpecies)]
floraAt env ws gx gy = do
    tileData ← readIORef (wsTilesRef ws)
    cat ← readIORef (floraCatalogRef env)
    let (coord, (lx, ly)) = globalToChunk gx gy
    pure $ case lookupChunk coord tileData of
        Nothing → []
        Just lc →
            [ (i, sp)
            | i ← fcdInstances (lcFlora lc)
            , fromIntegral (fiTileX i) ≡ lx
            , fromIntegral (fiTileY i) ≡ ly
            , Just sp ← [lookupSpecies (fiSpecies i) cat]
            ]

-- | The world's growth clock (#332): (day-of-year, absolute day),
--   converted through the page's calendar.
growthClock ∷ WorldState → IO (Int, Int)
growthClock ws = do
    paramsM ← readIORef (wsGenParamsRef ws)
    date ← readIORef (wsDateRef ws)
    let calendar = maybe defaultCalendarConfig wgpCalender paramsM
    pure ( worldDateToDayOfYear calendar date
         , worldAbsoluteDay calendar date )
