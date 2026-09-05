{-# LANGUAGE Strict #-}
-- | Shared read-only helpers for the Forage API family (#94/#332):
--   resolving the flora instances on a tile and canonicalizing a tile
--   coord. Depended on by every Forage.* submodule that needs to look
--   up flora state. The growth CLOCK moved down to
--   "World.Flora.Clock" with #2212, so the world thread's Chop commit
--   evaluates the shared eligibility predicate on the same reading
--   these verbs do.
module Engine.Scripting.Lua.API.Forage.Lookup
    ( floraAt
    , canonicalPageTile
    ) where

import UPrelude
import Data.IORef (readIORef)
import Engine.Core.Capability.WorldSim
    (WorldSimCapability(..))
import World.Types
import World.Generate.Coordinates (canonicalTile, globalToChunk)

-- | The canonical (stored-frame) image of a global tile coord on this
--   page (#1175/#1707). Chunks are STORED u-wrapped, so a coord that was
--   not itself read out of stored world data — an AI scan stepped
--   outward from a unit, a coord round-tripped through a pre-#1175 save,
--   a Lua caller working in a raw frame — can name an ALIAS of the
--   stored key, and every tile-keyed forage map (flora harvests, crop
--   plots) is canonical. Identity inland, and on an arena / zero-size
--   page.
canonicalPageTile ∷ WorldState → Int → Int → IO (Int, Int)
canonicalPageTile ws gx gy = do
    worldSize ← pageWrapWorldSize ws
    pure (canonicalTile worldSize gx gy)

-- | Every flora instance on tile (gx, gy) of the active world's loaded
--   chunks, joined with its species. Empty when the chunk isn't loaded.
--
--   Accepts any u-alias of the tile (#1707): the raw coord is resolved
--   into the stored frame first, so a seam-side caller reads the same
--   instances the canonical coord reports rather than an empty tile.
floraAt ∷ WorldSimCapability → WorldState → Int → Int
        → IO [(FloraInstance, FloraSpecies)]
floraAt wsc ws rawGX rawGY = do
    tileData ← readIORef (wsTilesRef ws)
    cat ← readIORef (wsFloraCatalogRef wsc)
    (gx, gy) ← canonicalPageTile ws rawGX rawGY
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
