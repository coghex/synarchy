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
--     and its own handler; see "Unit.Thread.Command.Solidify".
--
--   __The victim set is a SNAPSHOT, taken before the first stone.__
--   'snapshotSolidificationOccupants' runs at the very top of
--   'World.Thread.Command.Reaction.commitReactions', ahead of every
--   'World.Edit.Apply.applyEdit' of the delivery, and its result is
--   carried to 'destroySolidificationOccupants' and from there onto the
--   unit queue. Two things follow, and both are the point:
--
--   * Nothing this commit DOES can move the answer. The terrain writes,
--     the item removals, the generation advance, the sim handoff, the
--     zoom refresh and the designation revalidation all happen after
--     the roster has been read, so none of them can be the reason a
--     unit is in or out of the set. Reading it later would have made
--     the victim set depend on how long the commit's own bookkeeping
--     took.
--   * Every event of one delivery is judged against ONE roster. Two
--     tiles of the same commit cannot be graded against different
--     positions of the same walking unit.
--
--   What it is NOT is atomic with respect to the unit thread, and no
--   lock-free arrangement could be: @utsSimStates@ is written by
--   @Unit.Thread@'s movement tick, which runs on its own thread and
--   takes nothing this one could hold. What IS guaranteed is the shape
--   of the residual window. Positions change only in that movement
--   tick, so this reads the positions as of the last movement tick
--   before the snapshot — one well-defined instant, not a smear —
--   and the alternative it replaces (selecting at the drain) would
--   read the last movement tick before the DRAIN, an unbounded number
--   of ticks later and behind however much of the queue was already
--   waiting.
--
--   __Where the positions come from.__ @utsSimStates@ — the
--   AUTHORITATIVE simulation coordinates, read through
--   'Engine.Core.Capability.UnitCombat.ucUtsRef'. That record lives on
--   'Engine.Core.State.EngineEnv' rather than inside the unit thread
--   precisely so another thread can read it (the save capture and
--   @unit.getInfo@ both already do); this is a READ, and the unit
--   thread remains its only writer. One 'readIORef' of an immutable map
--   is a consistent whole-roster snapshot, so no victim can be seen
--   half-moved.
--
--   @umInstances@ answers ONE question here: which units belong to this
--   page. It is never asked for a position.
--   @Unit.Thread.publishToRender@ republishes @usRealX@/@usRealY@ into
--   @uiGridX@/@uiGridY@ once per unit tick, so the mirror is up to a
--   whole tick behind — and a unit that crossed OFF the cell inside
--   that tick would have been killed by a reaction it was no longer in,
--   while one that crossed ONTO it would have escaped.
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
    ( SolidificationVictims(..)
    , snapshotSolidificationOccupants
    , destroySolidificationOccupants
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
import Unit.Types (UnitId(..), UnitManager(..), unitsOnPage)
import World.Chunk.Residency (ChunkGeneration)
import World.Generate.Coordinates (canonicalTile)
import World.GroundItems (takeGroundItemsOnPageWhere)
import World.Page.Types (WorldPageId(..))
import World.State.Types (WorldState(..), pageWrapWorldSize)

-- | Who was standing where, at the instant the commit began.
--
--   Carried from 'snapshotSolidificationOccupants' to
--   'destroySolidificationOccupants' as a VALUE rather than re-derived
--   at the second call, which is the whole reason the two are split:
--   a second read would be a second instant, and the commit's own work
--   sits between them.
--
--   One entry per solidified tile, each naming that tile in the
--   CANONICAL frame (so the unit thread never has to canonicalize
--   again) beside the units occupying it — dead ones included, since a
--   corpse the stone closed over still has to be kept out of the rock.
newtype SolidificationVictims = SolidificationVictims
    { svTiles ∷ [((Int, Int), [UnitId])] }
    deriving (Show, Eq)

-- | Read the occupants of the tiles this commit is ABOUT to turn to
--   stone, before it turns any of them.
--
--   Reads only. The caller applies the edits afterwards and then hands
--   this straight to 'destroySolidificationOccupants'; see the module
--   header for why the order is load-bearing rather than incidental.
--
--   A page whose gen params are not loaded yet has no wrap to
--   canonicalize against, and 'pageWrapWorldSize' answering 0 is the
--   identity — the right answer for a page holding no chunks either.
snapshotSolidificationOccupants
    ∷ UnitCombatCapability → WorldPageId → WorldState → [(Int, Int)]
    → IO SolidificationVictims
snapshotSolidificationOccupants uc pageId ws tiles
    | null tiles = pure (SolidificationVictims [])
    | otherwise = do
        worldSize ← pageWrapWorldSize ws
        -- ONE read of each. Page ownership from the manager (@uiPage@ is
        -- the instance's own field), position from the sim state.
        um  ← readIORef (ucUnitManagerRef uc)
        uts ← readIORef (ucUtsRef uc)
        let onPage = HS.fromList
                         (HM.keys (unitsOnPage pageId (umInstances um)))
        pure $ SolidificationVictims
            [ ( canonicalTile worldSize gx gy
              , [ uid
                | (uid, ss) ← HM.toList (utsSimStates uts)
                , HS.member uid onPage
                , occupiesTile worldSize (gx, gy) (usRealX ss) (usRealY ss) ] )
            | (gx, gy) ← tiles ]

-- | Destroy everything caught at the tiles this commit turned to stone.
--
--   Called by 'World.Thread.Command.Reaction.publishCommit' INSTEAD of
--   the add-tile path's @UnitReGround@, once per commit, with the
--   snapshot taken before the first edit landed. A commit that
--   solidified nothing reaches this with an empty snapshot and does
--   nothing.
--
--   @epoch@ is the page's incarnation ('World.Chunk.Admit.pageIncarnation'),
--   stamped onto every message so a queued kill cannot land on a
--   DIFFERENT page that has since been registered under the same name
--   (#2476/#2477) — the same fence @UnitSpawn@ carries, for the same
--   reason.
destroySolidificationOccupants
    ∷ UnitCombatCapability → LoggerState → WorldPageId → ChunkGeneration
    → WorldState → SolidificationVictims → IO ()
destroySolidificationOccupants uc logger pageId epoch ws (SolidificationVictims plan)
    | null plan = pure ()
    | otherwise = do
        worldSize ← pageWrapWorldSize ws
        let canonical = HS.fromList (map fst plan)
        -- Items are matched here rather than in the snapshot because
        -- nothing moves a ground item on its own: they are placed and
        -- taken by explicit acts, so there is no equivalent of a mover
        -- crossing the cell between the two calls.
        removed ← takeGroundItemsOnPageWhere ws $ \gi →
            HS.member (canonicalTile worldSize (floor (giX gi))
                                               (floor (giY gi)))
                      canonical
        forM_ plan $ \((cgx, cgy), victims) →
            -- Sent even with no victims: it is what carries "this tile
            -- solidified" to the unit thread, and a handler given an
            -- empty set is a cheap no-op. Sending it unconditionally
            -- keeps the solidification path from quietly re-acquiring
            -- the lift it replaced.
            Q.writeQueue (ucUnitQueue uc)
                (UnitSolidifyOccupants pageId epoch cgx cgy victims)
        logDebug logger CatWorld $
            "Solidification destroyed " <> tshow (length removed)
            <> " ground item(s) and named "
            <> tshow (sum (map (length ∘ snd) plan))
            <> " unit occupant(s) across " <> tshow (length plan)
            <> " stone tile(s) on page " <> unWorldPageId pageId

-- | Is a unit at authoritative sim position @(ux, uy)@ standing on
--   @tile@, in the canonical frame?
--
--   Exported so the contract is checkable against the function the
--   commit actually calls rather than a restatement of it. It takes the
--   bare coordinates rather than a record, so no caller can reach it
--   with the render mirror's lagging copy of them by accident. The page
--   is NOT re-checked here — the caller has already narrowed to one
--   page's units, and a coordinate match on another page is not an
--   occupant of this tile at all (#1593).
occupiesTile ∷ Int → (Int, Int) → Float → Float → Bool
occupiesTile worldSize (gx, gy) ux uy =
    canonicalTile worldSize (floor ux) (floor uy)
        ≡ canonicalTile worldSize gx gy
