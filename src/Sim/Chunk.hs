{-# LANGUAGE Strict #-}

-- | Seeding and activating one world's sim chunks: the pure state
--   transitions behind 'Sim.Command.Types.SimChunkLoaded' and
--   'Sim.Command.Types.SimChunkEdited'.
--
--   Split out of "Sim.Thread" (#2044) so the edit path's neighbour
--   activation — the half that has to resolve the u seam — is reachable
--   without a running worker thread.
module Sim.Chunk
    ( newChunkSettleTicks
    , reactivateSettleTicks
    , activateChunk
    , loadedChunkState
    , applyChunkEdit
    , applyReactionCommit
    ) where

import UPrelude
import qualified Data.HashMap.Strict as HM
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU
import World.Chunk.Types (ChunkCoord(..), chunkSize)
import World.Fluid.Internal (FluidMap)
import Sim.State.Types (SimWorldState(..), SimChunkState(..))
import Sim.Fluid.Types (ActiveFluidCell(..), fluidCellToActive, volumePerLevel)
import Sim.Topology (simCardinalNeighbors)

-- | Settle-tick countdown for a freshly generated/loaded chunk. Newly
--   generated fluid starts further from equilibrium than a
--   previously-settled chunk being nudged awake, so it gets a longer
--   countdown than 'reactivateSettleTicks'.
newChunkSettleTicks ∷ Int
newChunkSettleTicks = 64

-- | Settle-tick countdown for a chunk that was already settled and is
--   being re-agitated: a world reactivation, a tile edit, or an edit
--   that lands before the chunk has ever loaded. Shorter than
--   'newChunkSettleTicks' since it's re-equilibrating, not settling
--   from scratch.
reactivateSettleTicks ∷ Int
reactivateSettleTicks = 24

-- | Activate a passive chunk for volume-based simulation. Idempotent:
--   an already-active chunk keeps its live volume grid.
activateChunk ∷ SimChunkState → SimChunkState
activateChunk scs
    | scsActive scs = scs  -- already active
    | otherwise =
        let terrV = scsTerrain scs
            fluidV = scsFluid scs
            activeFluid = V.imap (\idx mfc →
                case mfc of
                    Nothing → Nothing
                    Just fc → fluidCellToActive (terrV VU.! idx) fc
                ) fluidV
        in scs { scsActive      = True
               , scsActiveFluid = activeFluid
               , scsEquilTicks  = 0
               }

-- | A freshly loaded chunk's sim state.
loadedChunkState ∷ FluidMap → VU.Vector Int → SimChunkState
loadedChunkState fluidMap terrainMap = SimChunkState
    { scsFluid       = fluidMap
    , scsTerrain     = terrainMap
    , scsSettleTicks = newChunkSettleTicks
    , scsActive      = False
    , scsActiveFluid = V.replicate sz Nothing
    , scsEquilTicks  = 0
    , scsSideDeco    = VU.replicate sz 0
    -- A freshly loaded chunk starts at the page's own baseline
    -- generation: 'World.Thread.ChunkLoading' deletes an evicted chunk's
    -- entry, so the world side reads 0 for it too (#1596).
    , scsEditGen     = 0
    }
  where sz = chunkSize * chunkSize

-- | Re-seed one chunk from the authoritative post-edit tiles and wake it
--   (and its four physically cardinal-adjacent chunks) so the new fluid
--   actually flows.
--
--   The neighbours are resolved through the world's own
--   'swsTopology', so a chunk beside the u seam wakes the chunk STORED
--   on the far side rather than a raw @(cx±1, cy)@ key nothing holds
--   (#2044); 'Sim.Fluid.Active.reconcileSeams' needs both sides active
--   before anything can cross. Away from a seam, and on a flat page,
--   every neighbour key is the raw one.
applyChunkEdit ∷ ChunkCoord → Word64 → FluidMap → VU.Vector Int
               → SimWorldState → SimWorldState
applyChunkEdit coord editGen fluidMap terrainMap sws =
    sws { swsChunks = withNbrs }
  where
    -- Build on the existing sim chunk if present, else create one (an
    -- edit can land before the sim has loaded that chunk).
    --
    -- Both branches adopt the carried generation: it is what makes the
    -- writebacks this chunk produces from here on acceptable to the
    -- world thread again (#1596), including for an edit that lands
    -- before the chunk has ever loaded.
    base = case HM.lookup coord (swsChunks sws) of
        Just scs → scs { scsFluid       = fluidMap
                       , scsTerrain     = terrainMap
                       , scsSettleTicks = reactivateSettleTicks
                       , scsEditGen     = editGen
                       }
        Nothing  → (loadedChunkState fluidMap terrainMap)
                       { scsSettleTicks = reactivateSettleTicks
                       , scsEditGen     = editGen
                       }
    -- Force a fresh activation so the volume grid is rebuilt from the
    -- NEW fluid: activateChunk no-ops on an already-active chunk, so
    -- clear the flag first. Without this the edited chunk kept the new
    -- snapshot but never flowed (#60).
    activated = activateChunk (base { scsActive = False })
    -- HM.adjust is a no-op for an unloaded neighbour; activateChunk is
    -- idempotent for an already-active one.
    withSelf = HM.insert coord activated (swsChunks sws)
    withNbrs = foldl' (\m nc → HM.adjust activateChunk nc m) withSelf
                      (simCardinalNeighbors (swsTopology sws) coord)

-- | Adopt one participating chunk of a COMMITTED reaction result
--   (#2485): the authoritative post-edit terrain and generation, with
--   the chunk's EXACT live active volumes kept rather than rebuilt.
--
--   This is the whole reason the commit does not reuse 'applyChunkEdit'.
--   That path re-seeds the active grid from the passive
--   'World.Fluid.Internal.FluidMap' through 'fluidCellToActive', whose
--   @depth * volumePerLevel@ rounding turns the 1 unit a reaction left
--   in the contacting water cell into 7 — volume the reaction destroyed,
--   handed straight back. So an ACTIVE chunk keeps the grid it already
--   holds and only empties the cells that became stone; the exact
--   remainder survives because it is never converted at all.
--
--   An INACTIVE or absent chunk has no exact volumes to keep — its
--   grid is empty and its truth is the passive map — so it re-seeds
--   from @fluidMap@ exactly as 'applyChunkEdit' does. Both branches
--   adopt @editGen@ and wake the chunk and its physically cardinal
--   neighbours, resolved through the page's own seam frame (#2044), so
--   the surviving fluid flows around the new stone.
--
--   The solidified cells are DISPLACED in both branches, not emptied.
--   The terrain under them rose by exactly one z, so exactly one level's
--   worth of volume no longer fits; whatever stood above that still
--   does. Blanket-clearing would contradict both halves of the contract
--   around it: 'World.Edit.Apply' keeps fluid whose surface remains above
--   the raised terrain, and a cell emptied by annihilation is an ordinary
--   empty destination that the SAME tick may refill (#2481), so the cell
--   named by an event is not necessarily empty by the time the commit
--   lands. Clearing it would then delete water the world's own tiles
--   still record — and a later, generation-correct writeback would carry
--   that deletion back into them.
applyReactionCommit ∷ ChunkCoord → Word64 → FluidMap → VU.Vector Int
                    → [Int] → SimWorldState → SimWorldState
applyReactionCommit coord editGen fluidMap terrainMap solidified sws =
    sws { swsChunks = withNbrs }
  where
    existing = HM.lookup coord (swsChunks sws)

    committed = case existing of
        Just scs | scsActive scs → scs
            { scsFluid       = fluidMap
            , scsTerrain     = terrainMap
            , scsActiveFluid = emptySolidified (scsActiveFluid scs)
            , scsSettleTicks = reactivateSettleTicks
            , scsEquilTicks  = 0
            , scsEditGen     = editGen
            }
        _ →
            let base = case existing of
                    Just scs → scs { scsFluid       = fluidMap
                                   , scsTerrain     = terrainMap
                                   , scsSettleTicks = reactivateSettleTicks
                                   , scsEditGen     = editGen
                                   }
                    Nothing  → (loadedChunkState fluidMap terrainMap)
                                   { scsSettleTicks = reactivateSettleTicks
                                   , scsEditGen     = editGen
                                   }
                -- Force a fresh activation so the volume grid is rebuilt
                -- from the NEW fluid, same reason as 'applyChunkEdit'.
                activated = activateChunk (base { scsActive = False })
            in activated
                { scsActiveFluid = emptySolidified (scsActiveFluid activated) }

    -- The same rule 'World.Edit.Apply' applies to the passive cell,
    -- in volume terms: a cell whose fluid reached no higher than the
    -- newly filled level is displaced entirely, and a deeper one keeps
    -- its surface and loses exactly that level's worth of volume.
    emptySolidified grid = grid V.// [ (i, displace (grid V.! i))
                                     | i ← solidified
                                     , i ≥ 0, i < V.length grid ]
    displace Nothing = Nothing
    displace (Just afc)
        | afcVolume afc ≤ fromIntegral volumePerLevel = Nothing
        | otherwise = Just afc
            { afcVolume = afcVolume afc - fromIntegral volumePerLevel }

    withSelf = HM.insert coord committed (swsChunks sws)
    withNbrs = foldl' (\m nc → HM.adjust activateChunk nc m) withSelf
                      (simCardinalNeighbors (swsTopology sws) coord)
