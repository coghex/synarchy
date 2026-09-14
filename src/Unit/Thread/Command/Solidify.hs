{-# LANGUAGE Strict #-}

-- | Killing and settling the units caught where the lava-water reaction
--   turned a cell to stone (#2490, FR-3 of epic #2480).
--
--   The unit half of "World.Reaction.Occupants". That module resolves
--   the victims while the commit is landing on the world thread and
--   sends one 'Unit.Command.Types.UnitSolidifyOccupants' per solidified
--   tile; this handler is what the unit thread does with it, and it is
--   here — on the thread that owns @utsSimStates@ — because #1890 makes
--   the unit thread the only writer of unit sim state.
--
--   Three outcomes, one per named victim, decided from the
--   AUTHORITATIVE roster and pose rather than from the render mirror
--   the world thread could have read:
--
--   * __Alive.__ The unit dies exactly as
--     'Unit.Thread.Command.Pose.handleUnitKillCommand' kills it — that
--     handler is CALLED, not restated, so the terminal state cannot
--     drift from the one every other death produces — and the death is
--     recorded twice: an injury-stream @"death"@ naming the reaction
--     and the tile, and a player event-log row attributed to the unit
--     at the reaction page's coordinates.
--   * __Already dead.__ Nothing is killed and nothing is recorded: a
--     corpse the stone closed over died of whatever killed it, and a
--     second @"death"@ row would report it twice.
--   * __Gone.__ A victim the ROSTER no longer holds on this page is
--     skipped entirely. That is not merely defensive: a page teardown
--     or a same-id re-init removes the manager rows immediately and
--     leaves the sim rows for a queued @UnitClearPage@ that may still
--     be behind this message, so a sim-state-only check would kill and
--     report an orphan of an incarnation that is already gone. The
--     whole message is fenced on the page's incarnation for the same
--     window, exactly as @UnitSpawn@ is (#2476/#2477).
--
--   Every surviving row among the named victims — a fresh corpse and an
--   older one alike — then gets the one thing the replaced
--   'Unit.Command.Types.UnitReGround' was there for: enough height not
--   to be buried. Two rules make that the MINIMUM correction rather
--   than a lift:
--
--   * It is resolved against the victim's OWN CURRENT column, on the
--     victim's own page — not against the solidified tile the message
--     names. A victim can have moved between the commit and this drain
--     (that is the very delay the carried set exists to survive), and
--     correcting it to the stone column's height would float it over
--     lower ground or leave it buried under higher.
--   * It is a @max@, never a snap: it lifts a body the terrain would
--     otherwise swallow and leaves alone one already standing above
--     it, and it never changes a horizontal coordinate.
--
--   It is deliberately NOT the ordinary lift — that one carries LIVING
--   units up with the terrain, and on this path there are no living
--   units left to carry.
module Unit.Thread.Command.Solidify
    ( handleUnitSolidifyOccupantsCommand
    , handleUnitSolidifyOccupantsCommandWith
    , SolidifySeams(..)
    , productionSolidifySeams
    , solidificationDeathCause
    , solidificationDeathText
    , solidificationEventCategory
    ) where

import UPrelude
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import Data.IORef (IORef, readIORef, atomicModifyIORef')
import Engine.Core.Capability.UnitCombat
    (UnitCombatCapability(..), toUnitCombatCapability)
import Engine.Core.Capability.WorldSim
    (WorldSimCapability(..), toWorldSimCapability)
import Engine.Core.State (EngineEnv)
import Engine.PlayerEvent.Emit (emitEventFullOnPage)
import Combat.Types (pushInjuryEvent)
import Unit.Types
import Unit.Sim.Types
import Unit.Thread.Command.Lifecycle (lookupTerrainTopZ)
import Unit.Thread.Command.Pose (handleUnitKillCommand)
import Engine.Core.Capability.WorldSim (withPageLifecycle)
import World.Chunk.Admit (pageIncarnation)
import World.Generate.Coordinates (canonicalTile)
import World.State.Types (pageWrapWorldSize)
import World.Chunk.Residency (ChunkGeneration)
import World.Page.Types (WorldPageId(..))
import World.Types (WorldManager(..), wmWorlds)

-- | The notification category a solidification death is filed under.
--
--   An EXISTING category whose shipped defaults log it
--   (@data\/notification_categories.yaml@): losing a unit to the world
--   closing over it is a failure the player should notice while
--   scanning the log, which is what @unit_warning@ is for. #2490
--   explicitly adds no category of its own.
solidificationEventCategory ∷ Text
solidificationEventCategory = "unit_warning"

-- | The injury stream's @cause@ for one of these deaths.
--
--   @scripts\/injury_log.lua@'s @deathLine@ renders a @"death"@ event as
--   \"\<Name\> died of \<cause\>.\", so the cause is a NOUN PHRASE and it
--   names both halves requirement 2 asks for: the reaction that killed
--   the unit, and the tile it happened at.
solidificationDeathCause ∷ Int → Int → Text
solidificationDeathCause gx gy =
    "being entombed in solidifying lava at " <> tshow (gx, gy)

-- | The player-event log's own sentence for the same death.
solidificationDeathText ∷ Text → Int → Int → Text
solidificationDeathText name gx gy =
    (if T.null name then "A unit" else name)
    <> " was entombed by solidifying lava at " <> tshow (gx, gy) <> "."

-- | The one point a test may interpose on, so the
--   check-then-replace-then-act schedule can be built deterministically
--   instead of raced for.
--
--   The same shape and the same purpose as
--   'Unit.Thread.Command.Spawn.SpawnSeams': the production entry point
--   supplies 'productionSolidifySeams', so what a test drives is this
--   module's real body with one hook filled in, never a
--   reimplementation of it.
newtype SolidifySeams = SolidifySeams
    { seamAfterEpochCheck ∷ IO ()
      -- ^ Runs after the handler's FIRST epoch check and before it
      --   enters the lifecycle lock. A test lands a same-id re-init
      --   here; production does nothing, which is what makes the
      --   commit fence's revalidation the only thing standing between
      --   the two.
    }

productionSolidifySeams ∷ SolidifySeams
productionSolidifySeams = SolidifySeams { seamAfterEpochCheck = pure () }

handleUnitSolidifyOccupantsCommand
    ∷ EngineEnv → IORef UnitThreadState → WorldPageId → ChunkGeneration
    → Int → Int → Int → [UnitId] → IO ()
handleUnitSolidifyOccupantsCommand = handleUnitSolidifyOccupantsCommandWith
    productionSolidifySeams

handleUnitSolidifyOccupantsCommandWith
    ∷ SolidifySeams → EngineEnv → IORef UnitThreadState → WorldPageId
    → ChunkGeneration → Int → Int → Int → [UnitId] → IO ()
handleUnitSolidifyOccupantsCommandWith
        seams env utsRef pageId epoch gx gy committedTop victims = do
    -- The #2476/#2477 fence. A page id is a reusable NAME, so without
    -- it a kill resolved against one incarnation could land on the
    -- replacement registered under that name: it would kill rows the
    -- queued UnitClearPage is about to retire anyway, and — worse —
    -- file their deaths at coordinates and under a page name that now
    -- mean the NEW world's tiles.
    --
    -- Read here only as a cheap early-out. It is a time-of-CHECK, and
    -- the decision that matters is taken again below.
    current ← pageIncarnationOf env pageId
    when (current ≡ Just epoch) $ do
        seamAfterEpochCheck seams
        -- Two phases, and the split between them is a LOCK-ORDER rule,
        -- not a style choice. Everything that has to be atomic against
        -- a page replacement happens in the first; everything that can
        -- block happens in the second, after the lock is released.
        killed ← commitKills
        reportKills killed
  where
    worldSim = toWorldSimCapability env
    combat   = toUnitCombatCapability env

    -- | Phase one: revalidate and mutate, inside the page lifecycle
    --   lock, in one critical section — exactly as
    --   'Unit.Thread.Command.Spawn.handleUnitSpawnCommandWith' does.
    --   @World.Thread.Command.Init.registerPageIncarnation@ holds this
    --   same lock across retiring the outgoing incarnation's rows and
    --   registering the replacement, so a transition cannot interleave
    --   between the revalidation and the kills. Without it the early-out
    --   above is pure time-of-check-to-time-of-use: a replacement
    --   landing after it would leave this handler killing an orphan off
    --   its own captured list and attributing the death to the page that
    --   replaced it.
    --
    --   Everything inside is an 'IORef' read or an 'atomicModifyIORef''
    --   — the manager, the sim states and the page's tiles read; the two
    --   unit records and the transfer-order stores written — and no
    --   other lock is taken, which is what keeps this within
    --   'Engine.Core.State.pageLifecycleLock''s documented contract.
    --   Answers the victims it killed, with the display name each had
    --   at that moment, so phase two reports exactly what phase one did.
    commitKills = withPageLifecycle worldSim $ do
        live ← pageIncarnationOf env pageId
        if live ≢ Just epoch then pure [] else do
            um ← readIORef (ucUnitManagerRef combat)
            uts0 ← readIORef utsRef
            -- Present in BOTH records, and still on this page. The
            -- roster half is what makes the "Gone" case real rather
            -- than documented: a teardown drops the instance at once
            -- and leaves the sim row for a clear queued behind this.
            let onPage uid =
                    case ( HM.lookup uid (umInstances um)
                         , HM.lookup uid (utsSimStates uts0) ) of
                        (Just inst, Just ss) | uiPage inst ≡ pageId →
                            Just (inst, ss)
                        _ → Nothing
                named  = [ (uid, pair) | uid ← victims
                                       , Just pair ← [onPage uid] ]
                living = [ (uid, uiName inst)
                         | (uid, (inst, ss)) ← named, usPose ss ≢ Dead ]
            forM_ (map fst living) (handleUnitKillCommand env utsRef)
            -- Read AFTER the kills, so a unit whose death moved nothing
            -- is still measured from where it actually lies.
            forM_ (map fst named)
                  (settleClearOfTerrain env utsRef pageId gx gy committedTop)
            pure living

    -- | Phase two: file each death on the two surfaces requirement 2
    --   names, with the lifecycle lock RELEASED.
    --
    --   It is out here because it can block. A category whose
    --   notification settings turn @pause@ on sends
    --   'Engine.PlayerEvent.Emit.emitEventFullOnPage' through
    --   'World.Pause.imposePause', which takes the pause epoch's own
    --   mutex — and taking a second lock under the outermost commit
    --   boundary is what 'Engine.Core.State.pageLifecycleLock' forbids.
    --   The shipped @unit_warning@ settings do not enable it, but they
    --   are player-editable (@config\/notifications.local.yaml@), so a
    --   correct handler cannot depend on that.
    --
    --   Reporting after the fact loses nothing: the kills are already
    --   committed, the names were captured with them, and this thread is
    --   the only producer of these rows, so their order is preserved.
    --   A page replaced in between changes nothing either — these rows
    --   describe deaths that really happened on the incarnation the
    --   commit ran against, and they name that page explicitly rather
    --   than resolving one now.
    reportKills killed
        | null killed = pure ()
        | otherwise = do
            now ← readIORef (wsGameTimeRef worldSim)
            forM_ killed $ \(UnitId raw, name) → do
                pushInjuryEvent (ucInjuryEventsRef combat) now raw "death"
                    [ ("cause", solidificationDeathCause gx gy) ]
                -- The page is passed EXPLICITLY rather than derived from
                -- the unit or snapshotted from whichever page is active:
                -- this reaction can commit on a loaded page nobody is
                -- looking at, and those coordinates are in that page's
                -- frame ('Engine.PlayerEvent.Emit.resolveEventPage' case
                -- 1).
                --
                -- The source tag names the subsystem the row came from,
                -- and that is this handler: the reaction decided the
                -- tile, but only the unit thread decided there was a
                -- death to report.
                emitEventFullOnPage env solidificationEventCategory
                    "Unit.Solidify"
                    (solidificationDeathText name gx gy)
                    (Just (gx, gy)) (Just raw) (Just (unWorldPageId pageId))

-- | The incarnation the page registered under @pageId@ currently
--   stands at, or 'Nothing' when no page is registered under that name.
pageIncarnationOf ∷ EngineEnv → WorldPageId → IO (Maybe ChunkGeneration)
pageIncarnationOf env pageId = do
    wm ← readIORef (wsWorldManagerRef (toWorldSimCapability env))
    traverse pageIncarnation (lookup pageId (wmWorlds wm))

-- | Raise one row clear of the terrain it is standing on, if it is
--   below it — in the sim state and the render-facing instance
--   together, the same pair
--   'Unit.Thread.Command.Lifecycle.handleUnitReGroundCommand' keeps in
--   step and for the same reason: a corpse whose visual z lagged a tick
--   would be drawn inside the rock it is resting on.
--
--   Which column, and from where, depends on whether the body is still
--   on the cell that solidified:
--
--   * __Still there.__ The floor is @committedTop@, the terrain top the
--     commit itself left, MAXed with a live lookup if one succeeds. The
--     carried value is what makes this correction survive an eviction:
--     the queue delay is unbounded, the world thread's own tick can
--     evict the reaction chunk in it, and a live lookup would then
--     answer nothing at all — leaving a body that never moved embedded
--     one z under the stone as soon as the durable edit is replayed.
--   * __Moved.__ Its own current column on its own page, live. Nothing
--     this reaction did buried it there, so if that lookup cannot be
--     made there is nothing this handler owes it, and the carried
--     height would be the wrong answer rather than a fallback.
--
--   The terrain top in both cases, never the resolved surface: a
--   solidified cell may still hold fluid above its stone (engine
--   contracts §Fluid reaction — an active chunk's cell is displaced by
--   one level, not emptied), and correcting to that would float the
--   body on the water instead of resting it on the rock.
settleClearOfTerrain
    ∷ EngineEnv → IORef UnitThreadState → WorldPageId → Int → Int → Int
    → UnitId → IO ()
settleClearOfTerrain env utsRef pageId gx gy committedTop uid = do
    um ← readIORef (ucUnitManagerRef (toUnitCombatCapability env))
    uts ← readIORef utsRef
    case ( HM.lookup uid (umInstances um), HM.lookup uid (utsSimStates uts) ) of
        (Just inst, Just ss) → do
            let vx = floor (usRealX ss)
                vy = floor (usRealY ss)
            onSolidified ← sameTileOnPage env pageId (vx, vy) (gx, gy)
            mLive ← lookupTerrainTopZ env (uiPage inst) vx vy
            let mTop | onSolidified ∧ uiPage inst ≡ pageId =
                         Just (maybe committedTop (max committedTop) mLive)
                     | otherwise = mLive
            -- Unconditional: 'raiseTo' owns the "is it below?" rule,
            -- for both the discrete and the continuous height, and is a
            -- no-op when neither is. Deciding here as well would be a
            -- second copy of it — and the copy this replaces tested
            -- only the grid z, which is exactly the case below.
            forM_ mTop $ \z → raiseTo env utsRef z uid
        _ → pure ()

-- | Do these two tiles name the same physical cell of @pageId@?
--
--   Canonical, because a position near the cylindrical seam can name an
--   alias of the solidified tile (§Tile-coordinate seam frame) — and
--   the carried height belongs to the CELL, not to a spelling of it.
--   A page with no gen params yet wraps nothing, which is the identity.
sameTileOnPage ∷ EngineEnv → WorldPageId → (Int, Int) → (Int, Int) → IO Bool
sameTileOnPage env pageId a b = do
    wm ← readIORef (wsWorldManagerRef (toWorldSimCapability env))
    case lookup pageId (wmWorlds wm) of
        Nothing → pure False
        Just ws → do
            worldSize ← pageWrapWorldSize ws
            pure $ uncurry (canonicalTile worldSize) a
                 ≡ uncurry (canonicalTile worldSize) b

-- | Raise one row to terrain top @z@ where it stands below it — the
--   discrete @gridZ@ and the continuous @realZ@ each by their own
--   @max@ — on both surfaces, the sim state and the render-facing
--   instance. Idempotent, and a no-op for a row already clear.
raiseTo ∷ EngineEnv → IORef UnitThreadState → Int → UnitId → IO ()
raiseTo env utsRef z uid = do
    atomicModifyIORef' utsRef $ \uts →
        case HM.lookup uid (utsSimStates uts) of
            Just ss | belowEither (usGridZ ss) (usRealZ ss) →
                ( uts { utsSimStates = HM.insert uid
                            ss { usGridZ = max (usGridZ ss) z
                               , usRealZ = max (usRealZ ss) realZ }
                            (utsSimStates uts) }
                , () )
            _ → (uts, ())
    atomicModifyIORef' (ucUnitManagerRef (toUnitCombatCapability env)) $ \um →
        case HM.lookup uid (umInstances um) of
            Just inst | belowEither (uiGridZ inst) (uiRealZ inst) →
                ( um { umInstances = HM.insert uid
                         inst { uiGridZ = max (uiGridZ inst) z
                              , uiRealZ = max (uiRealZ inst) realZ }
                         (umInstances um) }
                , () )
            _ → (um, ())
  where
    realZ = fromIntegral z
    -- BOTH heights, because they are separate fields that separate
    -- things read, and a unit killed mid-ascent can have one already
    -- clear while the other is not. @usRealZ@ is the CONTINUOUS
    -- position, equal to the grid z except during a climb, where it
    -- lerps from the start z to the top: a one-level pull-up commits
    -- the grid z to the ledge while the body is still visibly below it.
    -- Correcting on the grid z alone would then decline to touch
    -- either, and 'Unit.Thread.Command.Pose.handleUnitKillCommand' has
    -- just cleared the climb endpoints and the transition timer, so no
    -- later tick will ever finish the lerp — the corpse would render
    -- inside the rock permanently.
    --
    -- Each field is raised by its OWN @max@, so clearing one never
    -- drags the other down: a body whose real z already stands above
    -- the new top keeps it.
    belowEither gridZ contZ = gridZ < z ∨ contZ < realZ
