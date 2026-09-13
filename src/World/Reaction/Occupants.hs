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
--   __Where the positions come from.__ @umInstances@, which is the
--   only unit position the world thread may read: @utsSimStates@
--   belongs to the unit thread and nothing outside it may touch that.
--   It is not a separate reading — @Unit.Thread.publishToRender@
--   copies @usRealX@\/@usRealY@ into @uiGridX@\/@uiGridY@ verbatim, so
--   this IS the simulation's own authored position, republished once
--   per unit tick. What it can be is up to one tick old, which is the
--   nearest thing to "at the edit" any cross-thread reader can have,
--   and strictly nearer than what the alternative would give: letting
--   the handler select at the drain would read a position an unbounded
--   number of ticks later, which is the substitution the carried set
--   exists to prevent.
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
import Unit.Types (UnitInstance(..), UnitManager(..), unitsOnPage)
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
        -- ONE read of the manager for the whole commit, so two tiles
        -- of the same delivery cannot be judged against different
        -- positions of the same walking unit.
        um ← readIORef (ucUnitManagerRef uc)
        let onPage = unitsOnPage pageId (umInstances um)
            plan = [ ( canonicalTile worldSize gx gy
                     , [ uid | (uid, inst) ← HM.toList onPage
                             , occupiesTile worldSize (gx, gy) inst ] )
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

-- | Is this instance standing on @tile@, in the canonical frame?
--
--   Exported so the contract is checkable against the function the
--   commit actually calls rather than a restatement of it. The page is
--   NOT re-checked here — the caller has already narrowed to one page's
--   instances, and a coordinate match on another page is not an
--   occupant of this tile at all (#1593).
occupiesTile ∷ Int → (Int, Int) → UnitInstance → Bool
occupiesTile worldSize (gx, gy) inst =
    canonicalTile worldSize (floor (uiGridX inst)) (floor (uiGridY inst))
        ≡ canonicalTile worldSize gx gy
