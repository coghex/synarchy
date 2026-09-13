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
--   Three outcomes, one per victim, decided from the AUTHORITATIVE pose
--   rather than from the render mirror the world thread read:
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
--   * __Gone.__ A victim the manager no longer holds (destroyed between
--     the commit and this drain) is skipped.
--
--   Every surviving row at the tile — a fresh corpse and an older one
--   alike — then gets the one thing the replaced
--   'Unit.Command.Types.UnitReGround' was there for: enough height not
--   to be buried. That correction is a @max@, never a snap: it lifts a
--   body the new stone would otherwise swallow and leaves alone one
--   already standing above it, which is the minimum this issue's
--   requirement 4 asks for. It is deliberately NOT the ordinary lift —
--   that one carries LIVING units up with the terrain, and on this path
--   there are no living units left to carry.
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
import World.Page.Types (WorldPageId(..))

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
    ∷ EngineEnv → IORef UnitThreadState → WorldPageId → Int → Int
    → [UnitId] → IO ()
handleUnitSolidifyOccupantsCommand env utsRef pageId gx gy victims = do
    -- Resolved against the POST-commit tiles, so the height a corpse is
    -- corrected to is the one the new stone left. The TERRAIN top, not
    -- the resolved surface: a solidified cell may still hold fluid
    -- above its stone (engine contracts §Fluid reaction — an active
    -- chunk's cell is displaced by one level, not emptied), and
    -- correcting to that would float the body on the water instead of
    -- resting it on the rock. Nothing here is conditional on it: a page
    -- or chunk that answers no top still kills, because the deaths are
    -- the contract and the height is the tidy-up.
    mSurf ← lookupTerrainTopZ env pageId gx gy
    uts0 ← readIORef utsRef
    -- A victim the roster dropped between the commit and this drain has
    -- no sim state either, so it is neither killed nor settled.
    let aliveNow uid = case HM.lookup uid (utsSimStates uts0) of
            Just ss → usPose ss ≢ Dead
            Nothing → False
        living = filter aliveNow victims
    forM_ living $ \uid → do
        handleUnitKillCommand env utsRef uid
        recordSolidificationDeath env pageId gx gy uid
    forM_ mSurf $ \z → forM_ victims (raiseAbove env utsRef z)

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
    -- The source tag names the subsystem the row came from, and that
    -- is this handler: the reaction decided the tile, but only the
    -- unit thread decided there was a death to report.
    emitEventFullOnPage env solidificationEventCategory
        "Unit.Solidify"
        (solidificationDeathText name gx gy)
        (Just (gx, gy)) (Just raw) (Just (unWorldPageId pageId))

-- | Lift one row to @z@ if it is below it, in the sim state and in the
--   render-facing instance together — the same pair
--   'Unit.Thread.Command.Lifecycle.handleUnitReGroundCommand' keeps in
--   step, for the same reason: a corpse whose visual z lagged a tick
--   would be drawn inside the stone it is resting on.
raiseAbove ∷ EngineEnv → IORef UnitThreadState → Int → UnitId → IO ()
raiseAbove env utsRef z uid = do
    raised ← atomicModifyIORef' utsRef $ \uts →
        case HM.lookup uid (utsSimStates uts) of
            Just ss | usGridZ ss < z →
                ( uts { utsSimStates = HM.insert uid
                            ss { usGridZ = z, usRealZ = fromIntegral z }
                            (utsSimStates uts) }
                , True )
            _ → (uts, False)
    when raised $
        atomicModifyIORef' (ucUnitManagerRef (toUnitCombatCapability env)) $ \um →
            case HM.lookup uid (umInstances um) of
                Nothing → (um, ())
                Just inst →
                    ( um { umInstances = HM.insert uid
                             inst { uiGridZ = z, uiRealZ = fromIntegral z }
                             (umInstances um) }
                    , () )
