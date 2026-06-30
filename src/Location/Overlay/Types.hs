{-# LANGUAGE Strict, UnicodeSyntax #-}
module Location.Overlay.Types
    ( LocationOverlay
    , emptyLocationOverlay
    , overlayLookup
    , overlayToList
    ) where

import UPrelude
import qualified Data.HashMap.Strict as HM
import Data.List (sortOn)
import World.Chunk.Types (ChunkCoord(..))

-- | Sparse map: chunk coordinate → placed location id (the
--   'Location.Types.ldId' of a 'LocationDef'). Computed once at world
--   init by the deterministic placement pass ('Location.Overlay') and
--   carried in 'World.Generate.Types.WorldGenParams', so it serializes
--   into the save and a loaded world keeps the exact layout it was
--   generated with — never recomputed (#89).
--
--   A plain 'HM.HashMap' keyed by 'ChunkCoord' (same shape as
--   'World.Ocean.Types.OceanDistMap') so it reuses the existing
--   'Serialize' / 'NFData' instances WorldGenParams already relies on.
type LocationOverlay = HM.HashMap ChunkCoord Text

emptyLocationOverlay ∷ LocationOverlay
emptyLocationOverlay = HM.empty

-- | The location id placed in a chunk, if any.
overlayLookup ∷ ChunkCoord → LocationOverlay → Maybe Text
overlayLookup = HM.lookup

-- | All placed entries, sorted by (cx, cy). 'HM.HashMap' iteration
--   order is unspecified, so callers that surface the overlay (the
--   @world.listPlacedLocations@ query) sort through this for stable,
--   deterministic output across runs and machines.
overlayToList ∷ LocationOverlay → [(ChunkCoord, Text)]
overlayToList = sortOn (\(ChunkCoord cx cy, _) → (cx, cy)) . HM.toList
