{-# LANGUAGE Strict, DeriveGeneric, DeriveAnyClass #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- | Stable per-instance flora identity (#1854).
--
--   Every 'World.Flora.Types.FloraInstance' carries one of these, and
--   every mutable per-plant authority — Chop designations
--   ('World.Chop.Types.ChopDesignations'), regrowth timers
--   ('World.Flora.Harvest.FloraHarvests'), and the Lua chop claims
--   built on them — is keyed by it rather than by the tile the plant
--   stands on. Two wood-tagged trees can legitimately share one tile,
--   and a tile key cannot tell them apart.
--
--   The id is OPAQUE, and structurally so: the constructor is not
--   exported, so the only ways to obtain one are the two allocators
--   below and 'floraInstanceIdFromLua', which refuses a number that
--   belongs to neither namespace. Nothing outside this module can
--   manufacture the reserved non-identity value or an id outside the
--   space. 'floraInstanceIdToLua' is the matching way out, for the
--   script boundary and for diagnostics.
--
--   == Namespaces
--
--   Bit 62 tags the ORIGIN, so the two allocation schemes are disjoint
--   by construction (#1854 requirement 4) rather than by convention:
--
--     [@bit 62 clear@] GENERATED — a 62-bit digest of the plant's own
--       placement provenance. Deterministic: the same physical plant
--       gets the same id every time its chunk is regenerated.
--     [@bit 62 set@]   PLANTED — a page-scoped allocator counter
--       ('World.Save.Types.wpsPlantedFloraCursor'), persisted so a
--       reload never reissues a live id.
--
--   Bit 63 is left clear in BOTH namespaces so the whole space fits in
--   a positive 'Lua.Integer' (Int64): the Lua forage/chop API hands
--   these ids to scripts, and a negative id would round-trip
--   confusingly through every script, log line and JSON dump.
--
--   Zero is RESERVED and belongs to neither namespace:
--   'floraInstanceIdNone' marks a value that carries no identity at
--   all. Its one producer is 'World.Flora.CropPlot.cropPlotInstance',
--   which synthesizes a placement-shaped 'FloraInstance' purely so the
--   growth math can run over a tile-keyed crop plot — that synthetic
--   value is not a plant, is never stored in chunk data, and must
--   never key a designation, claim or harvest timer.
module World.Flora.Identity
    ( -- * The identity
      --
      --   Exported ABSTRACTLY (#1854 round-1 review): the constructor
      --   stays in this module, so no consumer can manufacture a value
      --   outside the two namespaces — the reserved non-identity zero
      --   included. The only ways in are the two allocators below and
      --   'floraInstanceIdFromLua', which refuses anything else.
      FloraInstanceId
    , floraInstanceIdNone
    , isFloraInstanceIdNone
    , generatedFloraInstanceId
    , plantedFloraInstanceId
    , isGeneratedFloraInstanceId
    , isPlantedFloraInstanceId
    , firstPlantedFloraCursor
    , nextPlantedFloraCursor
    , plantedFloraCursorAbove
      -- * Boundary encoding
    , floraInstanceIdToLua
    , floraInstanceIdFromLua
    ) where

import UPrelude
import Control.DeepSeq (NFData(..))
import GHC.Generics (Generic)
import Data.Serialize (Serialize)
import Data.Hashable (Hashable(..))
import qualified Data.Text.Encoding as TE
import Data.Int (Int64)
import qualified Data.ByteString as BS

-- | An opaque stable flora-instance identity. The 'Word64' payload is
--   an implementation detail of this module (see the namespace layout
--   in the module header); consumers compare, hash and store it.
newtype FloraInstanceId = FloraInstanceId { unFloraInstanceId ∷ Word64 }
    deriving stock (Show, Eq, Ord, Generic)
    deriving newtype (NFData, Hashable)
    deriving anyclass (Serialize)

-- | Send an id across the Lua\/diagnostic boundary. Lossless and
--   always positive: bit 63 is clear in both namespaces by
--   construction, so the whole space fits in an 'Int64' and round-trips
--   through Lua, JSON and the debug console unchanged.
floraInstanceIdToLua ∷ FloraInstanceId → Int64
floraInstanceIdToLua (FloraInstanceId w) = fromIntegral w

-- | Read an id back from Lua. The ONLY way to build a
--   'FloraInstanceId' from a number, and deliberately partial: a value
--   that is in neither namespace — the reserved zero, or anything with
--   bit 63 set — names no plant that could ever exist, so it is refused
--   here rather than turned into an id that silently matches nothing
--   (or, worse, matches the crop-plot adapter's reserved value).
floraInstanceIdFromLua ∷ Int64 → Maybe FloraInstanceId
floraInstanceIdFromLua n
    | isGeneratedFloraInstanceId fid ∨ isPlantedFloraInstanceId fid = Just fid
    | otherwise                                                     = Nothing
  where fid = FloraInstanceId (fromIntegral n)

-- | The reserved non-identity value (see the module header). Belongs to
--   neither namespace, so it can never collide with a generated or
--   planted id.
floraInstanceIdNone ∷ FloraInstanceId
floraInstanceIdNone = FloraInstanceId 0

isFloraInstanceIdNone ∷ FloraInstanceId → Bool
isFloraInstanceIdNone (FloraInstanceId w) = w ≡ 0

-- | Bit 62: set on planted ids, clear on generated ones.
plantedBit ∷ Word64
plantedBit = 0x4000000000000000

-- | The 62 bits a generated digest is allowed to occupy.
generatedMask ∷ Word64
generatedMask = 0x3FFFFFFFFFFFFFFF

isGeneratedFloraInstanceId ∷ FloraInstanceId → Bool
isGeneratedFloraInstanceId (FloraInstanceId w) = w ≢ 0 ∧ (w ⌃ plantedBit) ≡ 0

isPlantedFloraInstanceId ∷ FloraInstanceId → Bool
isPlantedFloraInstanceId (FloraInstanceId w) = (w ⌃ plantedBit) ≢ 0

-- | The id of a WORLDGEN-placed plant, derived from its own placement
--   provenance (#1854 requirements 2 and 3):
--
--     * the PAGE it was placed on (its 'World.Page.Types.WorldPageId'
--       text — passed as 'Text' so this module stays dependency-free);
--     * the CANONICAL global tile it stands on — chunks are stored
--       u-wrapped, so a seam tile has several names and only the
--       canonical one is stable ("World.Render.HitTest"'s frame
--       contract);
--     * the species' own stable YAML @name@;
--     * that species' local instance ORDINAL on the tile (@0@-based,
--       'World.Flora.Placement.placeTileFlora'\'s @j@).
--
--   Deliberately NOT derived from anything positional: the
--   registration-order 'World.Flora.Types.FloraId', the
--   'Data.HashMap.Strict.toList' traversal order behind
--   'World.Flora.Types.worldGenSpecies', or @placeTileFlora@'s index
--   into that whole list. All three shift when an unrelated species is
--   added or reordered, which would rename plants nobody touched.
generatedFloraInstanceId ∷ Text → Int → Int → Text → Int → FloraInstanceId
generatedFloraInstanceId pageKey cgx cgy speciesName ordinal =
    FloraInstanceId (nonZero (avalanche folded ⌃ generatedMask))
  where
    -- Each provenance component enters through its own odd multiplier,
    -- so no two of them can trade places and produce the same digest.
    folded = 0x9E3779B97F4A7C15
        `xor` (hashText pageKey     * 0xFF51AFD7ED558CCD)
        `xor` (fromIntegral cgx     * 0xC4CEB9FE1A85EC53)
        `xor` (fromIntegral cgy     * 0x9E3779B97F4A7C15)
        `xor` (hashText speciesName * 0xBF58476D1CE4E5B9)
        `xor` (fromIntegral ordinal * 0x94D049BB133111EB)
    -- 0 is the reserved non-identity value, so a digest that lands
    -- there is nudged to 1. One pair of provenance tuples out of 2^62
    -- therefore shares an id, which is far below the birthday-collision
    -- floor the 62-bit space already carries.
    nonZero w = if w ≡ 0 then 1 else w

-- | The splitmix64/murmur3 finalizer: spread every input bit across
--   the whole word before the namespace mask throws the top two away.
avalanche ∷ Word64 → Word64
avalanche x0 =
    let x1 = (x0 `xor` (x0 `shiftR` 33)) * 0xFF51AFD7ED558CCD
        x2 = (x1 `xor` (x1 `shiftR` 33)) * 0xC4CEB9FE1A85EC53
    in x2 `xor` (x2 `shiftR` 33)

-- | FNV-1a over the UTF-8 bytes. Written out rather than reaching for
--   'Data.Hashable.hash' on purpose: @hashable@'s 'Text' instance is
--   salted per process (and has changed between releases), and these
--   ids are PERSISTED — the same plant must hash to the same id on
--   every run, machine and build.
hashText ∷ Text → Word64
hashText t = BS.foldl' step 0xCBF29CE484222325 (TE.encodeUtf8 t)
  where step acc b = (acc `xor` fromIntegral b) * 0x100000001B3

-- | The id for a page-scoped planted-flora allocator cursor value.
--   Cursors start at 'firstPlantedFloraCursor' (1), so no planted id is
--   ever the reserved zero.
plantedFloraInstanceId ∷ Word64 → FloraInstanceId
plantedFloraInstanceId n = FloraInstanceId (plantedBit ⌄ max 1 n)

-- | Where a fresh page's planted-flora allocator starts. Every other
--   engine allocator except ground items is 1-based
--   (@docs\/persistence_contract.md@); this one follows them.
firstPlantedFloraCursor ∷ Word64
firstPlantedFloraCursor = 1

-- | Take the next planted id, returning it with the advanced cursor.
nextPlantedFloraCursor ∷ Word64 → (FloraInstanceId, Word64)
nextPlantedFloraCursor cursor =
    let n = max firstPlantedFloraCursor cursor
    in (plantedFloraInstanceId n, n + 1)

-- | The smallest cursor value that is strictly above every planted id
--   in the list (#1854 requirement 5). Generated ids and the reserved
--   zero contribute nothing — they are not this allocator's to be above.
plantedFloraCursorAbove ∷ [FloraInstanceId] → Word64
plantedFloraCursorAbove ids =
    foldl' step firstPlantedFloraCursor ids
  where
    step acc fid@(FloraInstanceId w)
        | isPlantedFloraInstanceId fid = max acc ((w ⌃ generatedMask) + 1)
        | otherwise                    = acc
