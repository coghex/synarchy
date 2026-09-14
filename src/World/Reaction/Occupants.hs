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
--     page, and 'World.GroundItems.takeGroundItemsOnPage' takes the
--     page's ground-item lock for the whole read-decide-write exactly
--     as a selection does. WHICH items is decided at the snapshot
--     below, not at the removal — the same cutoff the units get. (Item removal is not exclusively a
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
--   __The two stores are read under one lock.__ Membership lives in
--   @umInstances@ and position in @utsSimStates@, and reading them
--   separately is not a snapshot of anything: a spawn commit landing
--   between the two reads is visible in neither the roster that was
--   already read nor — if an existing occupant is also stepped off the
--   cell in that window — in the positions read afterwards, and a tile
--   occupied throughout would yield no victims at all. So both reads
--   happen inside 'withPageLifecycle'. Nothing blocks in that section:
--   two 'readIORef's and pure work, no second lock, which is the
--   contract 'Engine.Core.State.pageLifecycleLock' states for every
--   holder.
--
--   __Exactly what the lock buys, and what it does not.__ It is
--   narrower than "the roster and the positions are frozen", and the
--   narrow version is the one to rely on:
--
--   * NEW MEMBERSHIP cannot appear. The one site that puts a new
--     'Unit.Types.UnitId' into @umInstances@ in a live session is
--     @Unit.Thread.Command.Spawn@'s commit, and it holds this same
--     mutex. (A load publish replaces the whole roster outside the
--     lock; a reaction cannot survive one either way — the
--     page-incarnation fence on the kill refuses it.)
--   * A page REINCARNATION cannot straddle the two reads either, but
--     for the opposite reason: it adds nothing. What it does is RETIRE
--     the outgoing incarnation's rows
--     (@World.Thread.Command.Init.registerPageIncarnation@ calls
--     @retirePageUnits@), and it is a holder, so that removal lands on
--     one side of the pair or the other.
--   * REMOVALS are NOT excluded, and this lock does not make them
--     harmless. @Unit.Thread.Command.Lifecycle@'s @UnitDestroy@
--     bypasses it and retires the roster row and the sim row in two
--     separate 'Data.IORef.atomicModifyIORef'' calls — in the same
--     order this reads them — so a destroy landing between the two
--     reads is seen present in BOTH and enters the victim list as a
--     STALE CANDIDATE. What covers that is the CONSUMER:
--     'Unit.Thread.Command.Solidify' re-reads the roster and the page
--     epoch before acting on any name carried here, and skips one the
--     roster no longer holds — the "Gone" case it documents. So the
--     pair is not "one instant" of the roster, and nothing rests on it
--     being one: it rests on ADDITIONS being excluded here and stale
--     names being filtered there.
--   * POSITIONS are not frozen. @Unit.Thread@'s movement tick and
--     @UnitTeleport@ write the horizontal position; the re-ground
--     handlers and this reaction's own corpse settle write the
--     vertical. Every one of them runs on the unit thread and takes
--     nothing this thread could hold, and no lock-free arrangement
--     could change that. One 'readIORef' of that map is still one
--     coherent instant of every position at once, so no unit is seen
--     half-moved — WHICH instant is the residual window below.
--
--   That window is bounded by the unit thread's own cadence rather than
--   by anything this thread does, and it is far narrower than what it
--   replaces: selecting at the drain would read positions an unbounded
--   number of ticks later, behind however much of the queue was already
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
--   half-moved. It does NOT follow that the second store's read agrees
--   with the first's — a concurrent removal can disagree, and the
--   bullets below say what does and does not survive that.
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
import Data.List (sort)
import Data.IORef (readIORef)
import qualified Engine.Core.Queue as Q
import Engine.Core.Capability.UnitCombat (UnitCombatCapability(..))
import Engine.Core.Capability.WorldSim
    (WorldSimCapability, withPageLifecycle)
import Engine.Core.Log (logDebug, LogCategory(..), LoggerState)
import Item.Ground (GroundItem(..), GroundItems(..))
import Unit.Command.Types (UnitCommand(..))
import Unit.Sim.Types (UnitSimState(..), UnitThreadState(..))
import Unit.Types (UnitId(..), UnitManager(..), unitsOnPage)
import World.Chunk.Residency (ChunkGeneration)
import qualified Data.Vector.Unboxed as VU
import World.Generate.Coordinates (canonicalTile, canonicalTileFrame)
import World.Tile.Types (lookupChunk)
import World.Chunk.Types (LoadedChunk(..), columnIndex)
import World.GroundItems (takeGroundItemsOnPage)
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
data SolidificationVictims = SolidificationVictims
    { svTiles ∷ [((Int, Int), [UnitId])]
    , svItems ∷ [Int]
      -- ^ The page-local ids of the ground items lying on those tiles
      --   when the snapshot was taken. Ids rather than a predicate to
      --   re-evaluate later, for the same reason the unit set is a list
      --   of 'UnitId's: an item dropped onto the cell after the commit
      --   began never occupied the cell this reaction caught, and
      --   re-deciding at removal time would destroy it anyway.
    }
    deriving (Show, Eq)

-- | Read the occupants of the tiles this commit is ABOUT to turn to
--   stone, before it turns any of them.
--
--   Reads only. The caller applies the edits afterwards and then hands
--   this straight to 'destroySolidificationOccupants'; see the module
--   header for why the order is load-bearing rather than incidental,
--   and for exactly what the lock around the two reads does and does
--   not exclude. In short: no addition and no page reincarnation can
--   straddle them; a concurrent removal can, and the names it strands
--   are filtered by the consumer rather than here.
--
--   A page whose gen params are not loaded yet has no wrap to
--   canonicalize against, and 'pageWrapWorldSize' answering 0 is the
--   identity — the right answer for a page holding no chunks either.
snapshotSolidificationOccupants
    ∷ UnitCombatCapability → WorldSimCapability → IO () → WorldPageId
    → WorldState → [(Int, Int)] → IO SolidificationVictims
snapshotSolidificationOccupants uc wsc betweenReads pageId ws tiles
    | null tiles = pure (SolidificationVictims [] [])
    | otherwise = do
        -- Outside the lock: it reads only this page's own gen params,
        -- which no roster transition touches.
        worldSize ← pageWrapWorldSize ws
        -- ONE read of each, both under the lifecycle lock. That
        -- excludes exactly two things from landing between them: an
        -- ADDITION (the spawn commit is a holder) and a page
        -- REINCARNATION (also a holder, and it only retires). It does
        -- NOT exclude a @UnitDestroy@, which bypasses the lock and
        -- retires the two stores in two separate writes — in this very
        -- order — so one can leave a STALE CANDIDATE in the result. The
        -- consumer filters those: 'Unit.Thread.Command.Solidify'
        -- re-reads the roster and the page epoch before acting on any
        -- name, and skips one the roster no longer holds.
        --
        -- Page ownership from the manager (@uiPage@ is the instance's
        -- own field), position from the sim state.
        withPageLifecycle wsc $ do
            um  ← readIORef (ucUnitManagerRef uc)
            -- Production passes @pure ()@. A test lands a roster
            -- transition here, and what it is asserting is that the
            -- transition CANNOT complete while this section is open.
            betweenReads
            uts ← readIORef (ucUtsRef uc)
            -- The ground items are captured HERE too, at the same
            -- cutoff as the units, and by id. They are not read under
            -- the ground-item lock and could not usefully be: a spawn
            -- does not take that lock, so holding it would exclude
            -- nothing this cutoff does not already exclude by being
            -- EARLY. Anything dropped onto a doomed cell after this
            -- point simply is not in the set.
            gis ← readIORef (wsGroundItemsRef ws)
            let onPage = HS.fromList
                             (HM.keys (unitsOnPage pageId (umInstances um)))
                canonical = HS.fromList
                    [ canonicalTile worldSize gx gy | (gx, gy) ← tiles ]
            pure $ SolidificationVictims
                [ ( canonicalTile worldSize gx gy
                  , [ uid
                    | (uid, ss) ← HM.toList (utsSimStates uts)
                    , HS.member uid onPage
                    , occupiesTile worldSize (gx, gy)
                                   (usRealX ss) (usRealY ss) ] )
                | (gx, gy) ← tiles ]
                (sort [ gid
                      | (gid, gi) ← HM.toList (gisItems gis)
                      , HS.member (canonicalTile worldSize
                                       (floor (giX gi)) (floor (giY gi)))
                                  canonical ])

-- | Destroy everything caught at the tiles this commit turned to stone.
--
--   Called by @World.Thread.Command.Reaction.publishCommit@ INSTEAD of
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
destroySolidificationOccupants uc logger pageId epoch ws victims
    | null plan = pure ()
    | otherwise = do
        worldSize ← pageWrapWorldSize ws
        -- Exactly the ids the snapshot captured, removed under the
        -- page's ground-item lock. NOT a fresh scan of the map: this
        -- runs after every stone of the delivery has landed, so a scan
        -- here would also catch an item dropped onto the cell in
        -- between — one that never occupied the cell the reaction
        -- caught, and that the unit half would never have selected.
        removed ← takeGroundItemsOnPage ws (svItems victims)
        -- The tiles as this commit LEFT them. Read here, on the world
        -- thread that owns them and while the chunk is certainly still
        -- loaded, because the handler's own lookup may not be able to:
        -- the queue delay is unbounded and the chunk can be evicted in
        -- the meantime.
        td ← readIORef (wsTilesRef ws)
        forM_ plan $ \((cgx, cgy), uids) → do
            let (coord, (lx, ly), _) = canonicalTileFrame worldSize cgx cgy
                committedTop = case lookupChunk coord td of
                    Just lc → lcTerrainSurfaceMap lc VU.! columnIndex lx ly
                    -- Unreachable: this commit just edited that column.
                    -- Answering the floor of the z domain rather than
                    -- raising keeps a lost chunk from turning a
                    -- tidy-up into a crash, and leaves the handler's
                    -- own live lookup to do the work if it can.
                    Nothing → minBound
            -- Sent even with no victims: it is what carries "this tile
            -- solidified" to the unit thread, and a handler given an
            -- empty set is a cheap no-op. Sending it unconditionally
            -- keeps the solidification path from quietly re-acquiring
            -- the lift it replaced.
            Q.writeQueue (ucUnitQueue uc)
                (UnitSolidifyOccupants pageId epoch cgx cgy committedTop
                                       uids)
        logDebug logger CatWorld $
            "Solidification destroyed " <> tshow (length removed)
            <> " ground item(s) and named "
            <> tshow (sum (map (length ∘ snd) plan))
            <> " unit occupant(s) across " <> tshow (length plan)
            <> " stone tile(s) on page " <> unWorldPageId pageId
  where
    plan = svTiles victims

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
