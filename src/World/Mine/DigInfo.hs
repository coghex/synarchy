{-# LANGUAGE Strict #-}
-- | What a designated mine tile is made of, how fast each tool cuts
--   it, whether the spoil it would produce has anywhere to go — and the
--   candidate walk that asks those questions in the right ORDER.
--
--   ONE implementation, two readers. @world.getDigInfoAt@ answers all
--   of it for a single tile the AI has already picked, and
--   @world.nearestWorkableMineDesignation@ (#2538) drives
--   'firstWorkableDesignation' over distance-ordered candidates while
--   looking for the nearest designation a worker can actually dig. The
--   walk's whole contract is that it applies the SAME rejection rules
--   the per-tile query reports — a worker must never select a tile the
--   per-tile query would have refused, nor skip one it would have
--   accepted — so those rules live here rather than being written
--   twice.
--
--   THE SPLIT IS THE COST CONTRACT, not tidiness. 'digMaterialAt' is a
--   resident tile read plus a registry lookup; 'spoilBlockedFor'
--   re-derives the spoil capacity around the tile, sweeping 81
--   vertices' worth of piles, tile legality and neighbouring
--   designations. So the expensive one takes a 'DigMaterial' the cheap
--   one produced and is never reachable except through it, and
--   'firstWorkableDesignation' calls it only for a candidate the
--   carried tools have ALREADY admitted. Folding the two into one
--   strict record would evaluate the sweep for every candidate,
--   including every tile the toolset rejects outright — which is
--   exactly what a pick-carrying miner walking a field of shovel-only
--   tiles would pay on every thought tick.
--
--   Every read is of already-resident state: an unloaded chunk answers
--   'Nothing' rather than queueing a load, which is what keeps ranking
--   many candidates from becoming a chunk-load amplifier. An unloaded
--   candidate is therefore indistinguishable from an undesignated one
--   here, and both are simply not workable.
module World.Mine.DigInfo
    ( DigInfo(..)
    , DigMaterial(..)
    , digInfoAt
    , digMaterialAt
    , spoilBlockedFor
    , usableTool
    , firstWorkableDesignation
    ) where

import UPrelude
import qualified Data.HashMap.Strict as HM
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU
import World.Chunk.Types (ColumnTiles(..), LoadedChunk(..), columnIndex)
import World.Generate.Coordinates (canonicalTileFrame)
import World.Material
    (MaterialId(..), MaterialProps(..), MaterialRegistry, getMaterialProps
    , materialIdByName)
import World.Mine.Types (MineDesignation(..), MineDesignations)
import World.Spoil.Logic (spoilBlockedAt)
import World.Spoil.Types (SpoilPiles)
import World.Tile.Types (WorldTileData, lookupChunk)

-- | The CHEAP half: what a designated tile is made of and how fast each
--   tool cuts it. A chunk read and a registry lookup, no spoil work.
--
--   Carries the tile's canonical key and the designation's z so the
--   expensive half never has to resolve the alias a second time.
data DigMaterial = DigMaterial
    { dmTile        ∷ !(Int, Int)
      -- ^ The designation's stored CANONICAL key, whatever alias was
      --   asked about.
    , dmZ           ∷ !Int
      -- ^ The designation's z — the level being dug, which is also the
      --   level the spoil has to reach.
    , dmMaterial    ∷ !Word8
      -- ^ Raw id of the material at that z. The column can expose a
      --   different one as digging drops through strata, so this is a
      --   snapshot, not a property of the designation.
    , dmPickSpeed   ∷ !Float
      -- ^ Dig-rate multiplier with a pick; 0 means a pick cannot work
      --   this material at all.
    , dmShovelSpeed ∷ !Float
      -- ^ The same for a shovel.
    } deriving (Show, Eq)

-- | Both halves together — what @world.getDigInfoAt@ reports for one
--   tile the caller has already chosen, where the spoil answer is
--   always wanted.
data DigInfo = DigInfo
    { diMaterial     ∷ !Word8
    , diPickSpeed    ∷ !Float
    , diShovelSpeed  ∷ !Float
    , diSpoilBlocked ∷ !Bool
      -- ^ True when this material produces spoil and the piles around
      --   the tile have no room for it. The dig command would refuse
      --   every tick, so a worker must not take the job at all.
    } deriving (Show, Eq)

-- | 'DigMaterial' for a global tile coord, which may be ANY u-alias of
--   the designated tile (#1175) — the designation map, the tile store
--   and the dig command itself all resolve the same stored key, and so
--   does this. 'Nothing' when the tile is not designated, its chunk is
--   not resident, or the designation's z has fallen outside the column.
digMaterialAt ∷ MaterialRegistry → WorldTileData → MineDesignations
              → Int              -- ^ world size in chunks (0 = no wrap)
              → (Int, Int)       -- ^ global tile, any u-alias
              → Maybe DigMaterial
digMaterialAt registry td desigs worldSize (rawGX, rawGY) = do
    let (coord, (lx, ly), (dgx, dgy)) =
            canonicalTileFrame worldSize rawGX rawGY
        gx = rawGX + dgx
        gy = rawGY + dgy
    md ← HM.lookup (gx, gy) desigs
    lc ← lookupChunk coord td
    let col  = lcTiles lc V.! columnIndex lx ly
        relZ = mdZ md - ctStartZ col
    if relZ < 0 ∨ relZ ≥ VU.length (ctMats col)
        then Nothing
        else do
            let matId = ctMats col VU.! relZ
                props = getMaterialProps registry (MaterialId matId)
            pure DigMaterial
                { dmTile        = (gx, gy)
                , dmZ           = mdZ md
                , dmMaterial    = matId
                , dmPickSpeed   = mpPickSpeed props
                , dmShovelSpeed = mpShovelSpeed props
                }

-- | The EXPENSIVE half: has the spoil this tile would produce anywhere
--   to go? Takes the 'DigMaterial' the cheap read already produced, so
--   it cannot be reached without one — see the module note on why that
--   is a contract rather than a convenience.
--
--   False for a material that produces no spoil at all, and for one
--   whose @dig_spoil@ names nothing this registry knows: neither can
--   fill a pile, so neither can be blocked by one.
--
--   Measured from the tile CENTRE; the per-tick dig gate re-checks with
--   the digger's real position.
spoilBlockedFor ∷ MaterialRegistry → WorldTileData → MineDesignations
                → SpoilPiles → DigMaterial → Bool
spoilBlockedFor registry td desigs piles dm =
    case mpDigSpoil props of
        Nothing        → False
        Just spoilName → case materialIdByName registry spoilName of
            Nothing      → False
            Just spoilId → spoilBlockedAt td desigs piles spoilId (dmZ dm)
                               ( fromIntegral gx + 0.5
                               , fromIntegral gy + 0.5 )
                               (gx, gy)
  where
    props    = getMaterialProps registry (MaterialId (dmMaterial dm))
    (gx, gy) = dmTile dm

-- | Both halves for one tile — the per-tile query's answer, where the
--   spoil verdict is always wanted so ordering buys nothing.
digInfoAt ∷ MaterialRegistry → WorldTileData → MineDesignations → SpoilPiles
          → Int              -- ^ world size in chunks (0 = no wrap)
          → (Int, Int)       -- ^ global tile, any u-alias
          → Maybe DigInfo
digInfoAt registry td desigs piles worldSize tile = do
    dm ← digMaterialAt registry td desigs worldSize tile
    pure DigInfo
        { diMaterial     = dmMaterial dm
        , diPickSpeed    = dmPickSpeed dm
        , diShovelSpeed  = dmShovelSpeed dm
        , diSpoilBlocked = spoilBlockedFor registry td desigs piles dm
        }

-- | The tool a worker carrying these classes would dig this tile with,
--   and its speed — or 'Nothing' when neither carried class can cut the
--   material at all. The argument is @(carries a pick, carries a
--   shovel)@; the returned name is the @dig_tools@ key
--   @scripts/unit_ai_dig.lua@ indexes its animations by.
--
--   The shovel is chosen unless a carried pick is STRICTLY faster,
--   which is exactly what @bestDigTool@ did in Lua before #2538 (it set
--   the shovel first and let the pick override only on @>@). A tie
--   therefore still goes to the shovel.
--
--   Spoil is deliberately NOT consulted here: this is the cheap gate
--   that decides whether asking about spoil is worth it at all.
usableTool ∷ (Bool, Bool) → DigMaterial → Maybe (Text, Float)
usableTool (hasPick, hasShovel) dm
    | pickSpeed > shovelSpeed = admit ("pick", pickSpeed)
    | otherwise               = admit ("shovel", shovelSpeed)
  where
    pickSpeed   = if hasPick   then dmPickSpeed dm   else 0
    shovelSpeed = if hasShovel then dmShovelSpeed dm else 0
    admit (tool, speed) = if speed > 0 then Just (tool, speed) else Nothing

-- | Walk distance-ordered candidates and stop at the first one this
--   toolset can work and whose spoil has somewhere to go.
--
--   The ORDER of the tests is the cost contract. A worker carrying no
--   digging tool reads nothing at all; a candidate is read cheaply
--   through @materialOf@, and @blocked@ — the spoil sweep — runs ONLY
--   for a candidate 'usableTool' has already admitted. Nothing past the
--   winner is touched, so an ordinary selection costs one cheap read
--   and one sweep.
--
--   @candidates@ must already be ordered nearest-first and already
--   filtered by range and by the caller's exclusion set; this decides
--   workability and nothing else.
firstWorkableDesignation
    ∷ (Bool, Bool)                              -- ^ (carries pick, carries shovel)
    → ((Int, Int) → Maybe DigMaterial)          -- ^ the cheap read
    → (DigMaterial → Bool)                      -- ^ the expensive spoil test
    → [(Float, (Int, Int))]                     -- ^ (dist², tile), nearest first
    → Maybe ((Int, Int), Float, Text, Float)
firstWorkableDesignation tools materialOf blocked candidates
    | not (fst tools ∨ snd tools) = Nothing
    | otherwise                   = go candidates
  where
    go []               = Nothing
    go ((d2, k) : rest) = case materialOf k of
        Nothing → go rest
        Just dm → case usableTool tools dm of
            Just (tool, speed)
                | not (blocked dm) → Just (k, d2, tool, speed)
            _                      → go rest
