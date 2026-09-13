{-# LANGUAGE Strict #-}

-- | What the lava-water reaction does to whatever was standing on the
--   cell it turns to stone (#2490, FR-3 of epic #2480).
--
--   #2485 commits each solidification event as a @WeAddTile@ through
--   the same path a player's add-tile takes, and that path settles
--   occupants by LIFTING them: 'Unit.Command.Types.UnitReGround'
--   re-snaps an idle unit's z to the new surface, and a ground item has
--   no stored z at all, so it is redrawn resting on whatever the column
--   now ends at. Neither is what the owner decided (epic #2480) should
--   happen when the terrain arriving is molten rock closing over your
--   head: anything occupying a solidifying cell is destroyed instantly.
--   Displacement and damage-and-lift were both considered and rejected.
--
--   This module is that destruction, and it runs on the WORLD thread
--   because the commit does. Two halves, split by who owns the state:
--
--   * __Ground items are removed here.__ They live on the page's
--     'World.State.Types.wsGroundItemsRef', the world thread owns that
--     page, and 'World.GroundItems.takeGroundItemsOnPageWhere' takes
--     the page's ground-item lock for the whole read-decide-write
--     exactly as a selection does. (Item removal is not exclusively a
--     world-thread act — @item.removeGround@ reaches the same helper
--     from the Lua thread — so what matters is the LOCK, not the
--     thread. This reaction's own removals stay on the thread that
--     commits its stone.)
--
--   * __Units are only NAMED here.__ Their sim state belongs to the
--     unit thread (#1890), so the kill rides that thread's own queue
--     and its own handler; see "Unit.Thread.Command.Solidify". What the
--     world thread contributes is the VICTIM SET, resolved from the
--     manager while the stone is landing.
--
--   __Why the victim set travels with the message.__ The unit queue is
--   drained on the unit thread's next tick, which is an unbounded delay
--   from here. Naming the tile alone and letting the handler select
--   would kill whoever is standing there THEN — a unit that walked on
--   afterwards, caught by a reaction it was never in — and would let a
--   unit that walked off escape one it was. The set is therefore
--   resolved at the edit and carried; the handler re-reads only each
--   named victim's own pose, so one that died of something else in the
--   meantime is settled rather than killed twice.
--
--   __Where the positions come from.__ @utsSimStates@ — the
--   AUTHORITATIVE simulation coordinates, read through
--   'Engine.Core.Capability.UnitCombat.ucUtsRef'. That record lives on
--   'Engine.Core.State.EngineEnv' rather than inside the unit thread
--   precisely so another thread can read it (the save capture and
--   @unit.getInfo@ both already do); this is a READ, and the unit
--   thread remains its only writer.
--
--   @umInstances@ answers ONE question here: which units belong to
--   this page. It is never asked for a position.
--   @Unit.Thread.publishToRender@ republishes @usRealX@/@usRealY@ into
--   @uiGridX@/@uiGridY@ once per unit tick, so the mirror is up to a
--   whole tick behind — and a unit that crossed OFF the cell inside
--   that tick would have been killed by a reaction it was no longer
--   in, while one that crossed ONTO it would have escaped.
--
--   This runs FIRST in 'World.Thread.Command.Reaction.publishCommit',
--   ahead of the generation advance, the sim handoff, the zoom refresh
--   and the designation revalidation, so the positions it reads are as
--   close to the edit as the world thread can get them. The unit
--   thread is genuinely concurrent, so "as close as possible" is the
--   honest claim rather than "atomic": what it buys is that a mover
--   travels at most a fraction of one tick between the stone landing
--   and the set being taken, instead of one tick per queued command
--   until the drain.
--
--   __Occupancy is a floor in the canonical frame.__ A unit's authored
--   position is a sub-tile float, so the tile it is ON is the floor of
--   it — which is what makes a unit mid-crossing occupy the tile it is
--   currently over rather than the one it is heading for. Both sides
--   are then moved into the stored frame with
--   'World.Generate.Coordinates.canonicalTile', because a position near
--   the cylindrical seam can name an alias of the solidified tile
--   (§Tile-coordinate seam frame); away from the seam every step here
--   is the identity.
module World.Reaction.Occupants
    ( destroySolidificationOccupants
    , occupiesTile
    ) where

import UPrelude
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import Data.IORef (readIORef)
import qualified Engine.Core.Queue as Q
import Engine.Core.Capability.UnitCombat (UnitCombatCapability(..))
import Engine.Core.Log (logDebug, LogCategory(..), LoggerState)
import Item.Ground (GroundItem(..))
import Unit.Command.Types (UnitCommand(..))
import Unit.Sim.Types (UnitSimState(..), UnitThreadState(..))
import Unit.Types (UnitManager(..), unitsOnPage)
import World.Generate.Coordinates (canonicalTile)
import World.GroundItems (takeGroundItemsOnPageWhere)
import World.Page.Types (WorldPageId(..))
import World.State.Types (WorldState(..), pageWrapWorldSize)

-- | Destroy everything caught at the tiles one reaction commit turned
--   to stone, on the page that committed it.
--
--   Called by 'World.Thread.Command.Reaction.publishCommit' INSTEAD of
--   the add-tile path's @UnitReGround@, once per commit, with the
--   canonical tiles that actually received stone. A commit that
--   solidified nothing reaches this with an empty list and does
--   nothing; a page whose gen params are not loaded yet has no wrap to
--   canonicalize against, and 'pageWrapWorldSize' answering 0 is the
--   identity, which is the right answer for a page holding no chunks
--   either.
destroySolidificationOccupants
    ∷ UnitCombatCapability → LoggerState → WorldPageId → WorldState
    → [(Int, Int)] → IO ()
destroySolidificationOccupants uc logger pageId ws tiles
    | null tiles = pure ()
    | otherwise = do
        worldSize ← pageWrapWorldSize ws
        let canonical = HS.fromList [ canonicalTile worldSize gx gy
                                    | (gx, gy) ← tiles ]
        -- Items first, and unconditionally: their removal is this
        -- thread's own work, and it must not be skipped because the
        -- page happens to be holding no units.
        removed ← takeGroundItemsOnPageWhere ws $ \gi →
            HS.member (canonicalTile worldSize (floor (giX gi))
                                               (floor (giY gi)))
                      canonical
        -- ONE read of each, so two tiles of the same delivery cannot
        -- be judged against different positions of the same walking
        -- unit. Page ownership from the manager (@uiPage@ is the
        -- instance's own field), position from the sim state.
        um  ← readIORef (ucUnitManagerRef uc)
        uts ← readIORef (ucUtsRef uc)
        let onPage = HS.fromList
                         (HM.keys (unitsOnPage pageId (umInstances um)))
            plan = [ ( canonicalTile worldSize gx gy
                     , [ uid
                       | (uid, ss) ← HM.toList (utsSimStates uts)
                       , HS.member uid onPage
                       , occupiesTile worldSize (gx, gy)
                                      (usRealX ss) (usRealY ss) ] )
                   | (gx, gy) ← tiles ]
        forM_ plan $ \((cgx, cgy), victims) →
            -- The message is sent even with no victims: it is what
            -- carries "this tile solidified" to the unit thread, and a
            -- handler given an empty set is a cheap no-op. Sending it
            -- unconditionally keeps the solidification path from
            -- quietly re-acquiring the lift it replaced.
            Q.writeQueue (ucUnitQueue uc)
                (UnitSolidifyOccupants pageId cgx cgy victims)
        logDebug logger CatWorld $
            "Solidification destroyed " <> tshow (length removed)
            <> " ground item(s) and named "
            <> tshow (sum (map (length ∘ snd) plan))
            <> " unit occupant(s) across " <> tshow (length tiles)
            <> " stone tile(s) on page " <> unWorldPageId pageId

-- | Is a unit at authoritative sim position @(ux, uy)@ standing on
--   @tile@, in the canonical frame?
--
--   Exported so the contract is checkable against the function the
--   commit actually calls rather than a restatement of it. It takes
--   the bare coordinates rather than a record, so no caller can reach
--   it with the render mirror's lagging copy of them by accident. The
--   page is NOT re-checked here — the caller has already narrowed to
--   one page's units, and a coordinate match on another page is not an
--   occupant of this tile at all (#1593).
occupiesTile ∷ Int → (Int, Int) → Float → Float → Bool
occupiesTile worldSize (gx, gy) ux uy =
    canonicalTile worldSize (floor ux) (floor uy)
        ≡ canonicalTile worldSize gx gy
