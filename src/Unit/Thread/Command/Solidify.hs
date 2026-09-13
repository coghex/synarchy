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
import World.Chunk.Admit (pageIncarnation)
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

handleUnitSolidifyOccupantsCommand
    ∷ EngineEnv → IORef UnitThreadState → WorldPageId → ChunkGeneration
    → Int → Int → [UnitId] → IO ()
handleUnitSolidifyOccupantsCommand env utsRef pageId epoch gx gy victims = do
    -- The #2476/#2477 fence, first and for the whole message. A page id
    -- is a reusable NAME, so without this a kill resolved against one
    -- incarnation could land on the replacement registered under that
    -- name: it would kill rows the queued UnitClearPage is about to
    -- retire anyway, and — worse — file their deaths at coordinates and
    -- under a page name that now mean the NEW world's tiles.
    current ← pageIncarnationOf env pageId
    when (current ≡ Just epoch) $ do
        um ← readIORef (ucUnitManagerRef (toUnitCombatCapability env))
        uts0 ← readIORef utsRef
        -- Present in BOTH records, and still on this page. The roster
        -- half is what makes the "Gone" case real rather than
        -- documented: a teardown drops the instance at once and leaves
        -- the sim row for a clear that may be queued behind this.
        let live uid = case ( HM.lookup uid (umInstances um)
                            , HM.lookup uid (utsSimStates uts0) ) of
                (Just inst, Just ss) | uiPage inst ≡ pageId → Just ss
                _                                           → Nothing
            named  = [ (uid, ss) | uid ← victims, Just ss ← [live uid] ]
            living = [ uid | (uid, ss) ← named, usPose ss ≢ Dead ]
        forM_ living $ \uid → do
            handleUnitKillCommand env utsRef uid
            recordSolidificationDeath env pageId gx gy uid
        -- Against each body's OWN column, read AFTER the kills so a
        -- unit whose death moved nothing is still measured from where
        -- it actually lies.
        forM_ (map fst named) (settleClearOfTerrain env utsRef)

-- | The incarnation the page registered under @pageId@ currently
--   stands at, or 'Nothing' when no page is registered under that name.
pageIncarnationOf ∷ EngineEnv → WorldPageId → IO (Maybe ChunkGeneration)
pageIncarnationOf env pageId = do
    wm ← readIORef (wsWorldManagerRef (toWorldSimCapability env))
    traverse pageIncarnation (lookup pageId (wmWorlds wm))

-- | File one death on both surfaces requirement 2 names.
recordSolidificationDeath
    ∷ EngineEnv → WorldPageId → Int → Int → UnitId → IO ()
recordSolidificationDeath env pageId gx gy uid@(UnitId raw) = do
    now ← readIORef (wsGameTimeRef (toWorldSimCapability env))
    pushInjuryEvent (ucInjuryEventsRef (toUnitCombatCapability env))
        now raw "death" [ ("cause", solidificationDeathCause gx gy) ]
    um ← readIORef (ucUnitManagerRef (toUnitCombatCapability env))
    let name = maybe "" uiName (HM.lookup uid (umInstances um))
    -- The page is passed EXPLICITLY rather than derived from the unit
    -- or snapshotted from whichever page is active: this reaction can
    -- commit on a loaded page nobody is looking at, and those
    -- coordinates are in that page's frame
    -- ('Engine.PlayerEvent.Emit.resolveEventPage' case 1).
    --
    -- The source tag names the subsystem the row came from, and that
    -- is this handler: the reaction decided the tile, but only the
    -- unit thread decided there was a death to report.
    emitEventFullOnPage env solidificationEventCategory
        "Unit.Solidify"
        (solidificationDeathText name gx gy)
        (Just (gx, gy)) (Just raw) (Just (unWorldPageId pageId))

-- | Raise one row clear of the terrain it is standing on, if it is
--   below it — in the sim state and the render-facing instance
--   together, the same pair
--   'Unit.Thread.Command.Lifecycle.handleUnitReGroundCommand' keeps in
--   step and for the same reason: a corpse whose visual z lagged a tick
--   would be drawn inside the rock it is resting on.
--
--   The column is resolved from the unit's OWN current position and its
--   OWN page, so a victim that moved between the commit and this drain
--   is corrected where it actually lies. The TERRAIN top, never the
--   resolved surface: a solidified cell may still hold fluid above its
--   stone (engine contracts §Fluid reaction — an active chunk's cell is
--   displaced by one level, not emptied), and correcting to that would
--   float the body on the water instead of resting it on the rock.
--
--   Silent when the page or chunk answers no top: the deaths are the
--   contract and the height is the tidy-up.
settleClearOfTerrain ∷ EngineEnv → IORef UnitThreadState → UnitId → IO ()
settleClearOfTerrain env utsRef uid = do
    um ← readIORef (ucUnitManagerRef (toUnitCombatCapability env))
    uts ← readIORef utsRef
    case ( HM.lookup uid (umInstances um), HM.lookup uid (utsSimStates uts) ) of
        (Just inst, Just ss) → do
            mTop ← lookupTerrainTopZ env (uiPage inst)
                       (floor (usRealX ss)) (floor (usRealY ss))
            forM_ mTop $ \z → when (usGridZ ss < z) $ raiseTo env utsRef z uid
        _ → pure ()

-- | Commit the height @z@ to both surfaces. Split from the decision so
--   the decision reads as one expression.
raiseTo ∷ EngineEnv → IORef UnitThreadState → Int → UnitId → IO ()
raiseTo env utsRef z uid = do
    atomicModifyIORef' utsRef $ \uts →
        case HM.lookup uid (utsSimStates uts) of
            Just ss | usGridZ ss < z →
                ( uts { utsSimStates = HM.insert uid
                            ss { usGridZ = z, usRealZ = fromIntegral z }
                            (utsSimStates uts) }
                , () )
            _ → (uts, ())
    atomicModifyIORef' (ucUnitManagerRef (toUnitCombatCapability env)) $ \um →
        case HM.lookup uid (umInstances um) of
            Nothing → (um, ())
            Just inst
                | uiGridZ inst < z →
                    ( um { umInstances = HM.insert uid
                             inst { uiGridZ = z, uiRealZ = fromIntegral z }
                             (umInstances um) }
                    , () )
                | otherwise → (um, ())
