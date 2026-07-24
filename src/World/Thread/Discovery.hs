{-# LANGUAGE Strict, UnicodeSyntax, OverloadedStrings #-}
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
import qualified Data.HashSet as HS
import qualified Data.HashMap.Strict as HM
import Data.List (sortOn)
import Data.IORef (readIORef, atomicModifyIORef')
import Engine.Core.State (EngineEnv(..), activeWorldPage)
import Engine.Core.Capability.ContentRegistries
    (ContentRegistriesCapability(..), toContentRegistriesCapability)
import Engine.PlayerEvent.Emit (emitEventFullOnPage)
import Location.Discovery (DiscoveryHit(..), findDiscoveries)
import Unit.Types (UnitInstance(..), UnitManager(..), UnitId(..))
import World.Types (WorldGenParams(..), WorldPageId(..), WorldState(..))

-- | Check one page's placed locations against every currently-known
--   PLAYER-faction unit on it, mark any newly-qualifying location
--   discovered, and emit one attributable player event per transition.
--   A no-op when the page has no live gen params yet (mirrors
--   'World.Thread.ItemTemp.tickItemTemperatures'). "Player-controlled"
--   is the current player-control faction contract: 'uiFactionId' ==
--   "player" (portal-spawned units use this tag) — hostile, wildlife,
--   neutral, and unrelated debug factions never discover a location by
--   moving through it.
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
            -- Location defs (their discovery margins) come through the
            -- `content-registries` capability (#890).
            registry ← readIORef
                (crLocationDefsRef (toContentRegistriesCapability env))
            um ← readIORef (unitManagerRef env)
            mActive ← activeWorldPage env
            let isActivePage = case mActive of
                    Just (activePageId, _) → activePageId ≡ pageId
                    Nothing → False
                pageUnits =
                    [ (uid, uiFactionId inst, floor (uiGridX inst), floor (uiGridY inst))
                    | (uid, inst) ← sortOn fst (HM.toList (umInstances um))
                    , uiPage inst ≡ pageId
                    ]
                hits = findDiscoveries (wgpWorldSize p) registry
                                        (wgpLocationOverlay p)
                                        (wgpLocationDiscovered p)
                                        pageUnits
            forM_ hits $ \hit → do
                atomicModifyIORef' (wsGenParamsRef ws) $ \mP → case mP of
                    Just p' →
                        ( Just p'
                            { wgpLocationDiscovered =
                                HS.insert (dhCoord hit) (wgpLocationDiscovered p')
                            }
                        , ()
                        )
                    Nothing → (mP, ())
                emitEventFullOnPage env "location_discovery" "World.Thread.Discovery"
                    ("Discovered: " <> dhLabel hit)
                    (if isActivePage then Just (dhAnchor hit) else Nothing)
                    (Just (unUnitId (dhUnit hit)))
                    (Just pageText)
