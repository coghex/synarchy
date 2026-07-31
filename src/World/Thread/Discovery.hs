{-# LANGUAGE Strict #-}
-- | Per-page location-discovery tick (#780). Runs beside the world
--   clock ('World.Thread.Time.tickWorldTime'), once per LOADED page per
--   world-thread iteration — including a hidden (non-visible) page, and
--   independent of the pause flag, since a freshly loaded save can come
--   up already standing inside a location's discovery margin and must
--   discover it immediately rather than waiting for an unpause. Unlike
--   the visible-only calendar/flora/power ticks, this never reads
--   game-scaled dt: discovery is a positional check against whatever
--   the unit and world-manager threads have already published this
--   instant, not something that advances with simulated time.
module World.Thread.Discovery
    ( tickLocationDiscovery
    ) where

import UPrelude
import Engine.Core.Capability.UnitCombat
    (UnitCombatCapability(..), toUnitCombatCapability)
import Engine.Core.Capability.WorldSim
    (WorldSimCapability(..), toWorldSimCapability)
import qualified Data.HashMap.Strict as HM
import Data.List (sortOn)
import Data.IORef (readIORef, atomicModifyIORef')
import Engine.Core.State (EngineEnv, activeWorldPageFrom)
import Engine.PlayerEvent.Emit (emitEventFullOnPage)
import Location.Discovery (DiscoveryHit(..), findDiscoveries)
import Location.Instance (LocationLifecycle(..), setLocationLifecycle)
import Unit.Types (UnitInstance(..), UnitManager(..), UnitId(..))
import World.Types (WorldGenParams(..), WorldPageId(..), WorldState(..))

-- | Check one page's placed locations against every currently-known
--   PLAYER-OWNED unit on it, mark any newly-qualifying location
--   discovered, and emit one attributable player event per transition.
--   A no-op when the page has no live gen params yet (mirrors
--   'World.Thread.ItemTemp.tickItemTemperatures'). Which units count is
--   NOT decided here: this hands every unit on the page to
--   'findDiscoveries', which applies 'Unit.Faction.isPlayerOwned' — the
--   shared definition of "the player's own unit" (#912). Ownership is
--   narrower than alliance on purpose, so a debug unit (allied with the
--   player, commandable by them, but not owned) still never discovers a
--   location by moving through it.
--
--   Every emitted event names its 'peSourcePage' (#780) since this
--   tick runs on every loaded page, not just the active one — but a
--   discovery on a page other than the currently active one omits
--   'peCoords' entirely rather than risk the popup's click-to-pan
--   silently panning the ACTIVE page's camera to a hidden page's
--   coordinates.
tickLocationDiscovery ∷ EngineEnv → WorldPageId → WorldState → IO ()
tickLocationDiscovery env pageId@(WorldPageId pageText) ws = do
    mParams ← readIORef (wsGenParamsRef ws)
    case mParams of
        Nothing → pure ()
        Just p → do
            -- Every discovery input — bounds, margin, display name,
            -- lifecycle — is stored on the instance itself (#911), so
            -- this tick no longer reads the location-def registry at all.
            um ← readIORef (ucUnitManagerRef (toUnitCombatCapability env))
            mActive ← activeWorldPageFrom (wsWorldManagerRef (toWorldSimCapability env))
            let isActivePage = case mActive of
                    Just (activePageId, _) → activePageId ≡ pageId
                    Nothing → False
                pageUnits =
                    [ (uid, uiFactionId inst, floor (uiGridX inst), floor (uiGridY inst))
                    | (uid, inst) ← sortOn fst (HM.toList (umInstances um))
                    , uiPage inst ≡ pageId
                    ]
                hits = findDiscoveries (wgpWorldSize p)
                                        (wgpLocationInstances p)
                                        pageUnits
            forM_ hits $ \hit → do
                -- The lifecycle promotion is the persisted transition
                -- (#911) — 'setLocationLifecycle' refuses a backward or
                -- same-state move, so an instance already discovered (or
                -- past it) neither changes nor re-emits. The event fires
                -- only on a promotion that actually landed, preserving
                -- #780's exactly-once contract even if two ticks raced.
                promoted ← atomicModifyIORef' (wsGenParamsRef ws) $ \mP → case mP of
                    Just p' → case setLocationLifecycle (dhInstance hit)
                                        LifecycleDiscovered
                                        (wgpLocationInstances p') of
                        Just instances' →
                            (Just p' { wgpLocationInstances = instances' }, True)
                        Nothing → (mP, False)
                    Nothing → (mP, False)
                when promoted $
                    emitEventFullOnPage env "location_discovery" "World.Thread.Discovery"
                        ("Discovered: " <> dhLabel hit)
                        (if isActivePage then Just (dhAnchor hit) else Nothing)
                        (Just (unUnitId (dhUnit hit)))
                        (Just pageText)
