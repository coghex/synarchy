{-# LANGUAGE Strict #-}
module Unit.Thread.Command.Lifecycle
    ( handleUnitDestroyCommand
    , handleUnitClearAllCommand
    , handleUnitTeleportCommand
    , handleUnitReGroundCommand
    , lookupSurfaceZ
    ) where

import UPrelude
import Engine.Core.Capability.UnitCombat
    (UnitCombatCapability(..), toUnitCombatCapability)
import Engine.Core.Capability.WorldSim
    (WorldSimCapability(..), toWorldSimCapability)
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import qualified Data.Vector.Unboxed as VU
import Data.IORef (IORef, readIORef, atomicModifyIORef')
import Engine.Core.State (EngineEnv)
import Unit.Types
import Unit.Sim.Types
import Unit.Thread.Command.MotionGuard (motionPayloadOk)
import Unit.Transfer.Live (retireTransferOrdersEverywhere)
import World.Types (WorldManager(..), WorldState(..), LoadedChunk(..), columnIndex, lookupChunk)
import World.Page.Types (WorldPageId(..))
import World.Generate (globalToChunk)

handleUnitDestroyCommand ∷ EngineEnv → IORef UnitThreadState → UnitId → IO ()
handleUnitDestroyCommand env utsRef uid = do
    -- Single atomic modify removes the unit from instances AND clears
    -- it from the selection set, so no observer ever sees a "selected
    -- but dead" state.
    atomicModifyIORef' (ucUnitManagerRef (toUnitCombatCapability env)) $ \um →
        (um { umInstances = HM.delete uid (umInstances um)
            , umSelected  = HS.delete uid (umSelected um)
            }, ())
    atomicModifyIORef' utsRef $ \uts →
        (uts { utsSimStates = HM.delete uid (utsSimStates uts) }, ())
    -- A durable transfer order (#1246) outliving its carrier is an
    -- ORPHAN: the executor prunes an order at its terminal transition,
    -- but a destroyed unit never ticks again, so nothing else would ever
    -- retire it and it would ride every later save with a dangling
    -- acting-unit reference (#1253). Runs AFTER the instance is gone,
    -- like the demolition twins in "Building.Thread.Command", so no
    -- reader can observe an order whose carrier is half-removed.
    retireTransferOrdersEverywhere (wsWorldManagerRef (toWorldSimCapability env))
                                   uid

handleUnitClearAllCommand ∷ EngineEnv → IORef UnitThreadState → IO ()
handleUnitClearAllCommand env utsRef = do
    -- Wipe all units + selection + sim state. Processed in queue order, so
    -- it runs AFTER any UnitSpawns queued before Exit to Menu — those
    -- insert first, then this clears, leaving no orphans (#58).
    atomicModifyIORef' (ucUnitManagerRef (toUnitCombatCapability env)) $ \um →
        (um { umInstances = HM.empty, umSelected = HS.empty }, ())
    atomicModifyIORef' utsRef $ \uts →
        (uts { utsSimStates = HM.empty }, ())

-- | Snap a unit to (gx, gy), grounding it at @mGz@ or at the surface.
--
--   A non-finite coordinate is DROPPED here rather than installed
--   (#2290): 'floor' does not throw and 'World.Generate.globalToChunk'
--   maps any 'Int' somewhere, so a NaN would land silently in
--   @usRealX@\/@usRealY@ and be persisted from there. @unit.setPos@
--   already refuses one at the scripting boundary; this is the same
--   domain enforced at the authoritative end of the queue.
handleUnitTeleportCommand ∷ EngineEnv → IORef UnitThreadState → UnitId
                          → Float → Float → Maybe Int → IO ()
handleUnitTeleportCommand env utsRef uid gx gy mGz = do
  ok ← motionPayloadOk env "UnitTeleport" [("gridX", gx), ("gridY", gy)] []
  when ok $ do
    gz ← case mGz of
        Just z  → return z
        Nothing → do
            -- Ground on the TELEPORTED UNIT'S OWN page (#1593), not on
            -- whichever page happens to be visible first: this uid has
            -- always carried the page that answers the question. A unit
            -- the manager no longer holds names no page and so gets no
            -- surface — the same z = 0 fallback an unloaded chunk gets.
            mPage ← unitPageOf env uid
            let gxi = floor gx ∷ Int
                gyi = floor gy ∷ Int
            mSurf ← case mPage of
                Nothing     → pure Nothing
                Just pageId → lookupSurfaceZ env pageId gxi gyi
            case mSurf of
                Just z  → return z
                Nothing → return 0

    atomicModifyIORef' utsRef $ \uts →
        let simStates = utsSimStates uts
        in case HM.lookup uid simStates of
            Nothing → (uts, ())
            Just ss →
                let ss' = ss { usRealX     = gx
                             , usRealY     = gy
                             , usGridZ     = gz
                             , usRealZ     = fromIntegral gz
                             , usTarget    = Nothing
                             , usState     = Idle
                             , usLocalPath = []
                             , usDrinkUntil      = Nothing
                             , usEatUntil        = Nothing
                             , usPickupUntil     = Nothing
                             , usTransitionUntil = Nothing
                             }
                in (uts { utsSimStates = HM.insert uid ss' simStates }, ())

    atomicModifyIORef' (ucUnitManagerRef (toUnitCombatCapability env)) $ \um →
        let insts = umInstances um
        in case HM.lookup uid insts of
            Nothing → (um, ())
            Just inst →
                let inst' = inst { uiGridX = gx
                                 , uiGridY = gy
                                 , uiGridZ = gz
                                 , uiRealZ = fromIntegral gz
                                 }
                in (um { umInstances = HM.insert uid inst' insts }, ())

handleUnitReGroundCommand ∷ EngineEnv → IORef UnitThreadState → WorldPageId
                          → Int → Int → IO ()
handleUnitReGroundCommand env utsRef pageId gx gy = do
    -- Terrain at (gx, gy) changed under our feet (dig / delete-tile) ON
    -- ONE PAGE — the emitting edit handler always knew which, and since
    -- #1593 it says so. Re-snap idle units standing on that tile OF THAT
    -- PAGE to the new surface; moving units re-ground themselves on every
    -- tile crossing, and transitioning/falling/climbing units are
    -- mid-state-machine and must not be teleport-snapped. A
    -- coordinate-matched unit on any OTHER page is not standing on the
    -- edited tile at all and must not move.
    mSurf ← lookupSurfaceZ env pageId gx gy
    case mSurf of
        Nothing → pure ()
        Just z → do
            -- Ownership lives on the instance, so the set of uids on the
            -- edited page is resolved once, before the sim-state swap.
            onPage ← unitIdsOnPage env pageId
            snapped ← atomicModifyIORef' utsRef $ \uts →
                let simStates = utsSimStates uts
                    affects uid ss =
                        HS.member uid onPage
                      ∧ floor (usRealX ss) ≡ gx
                      ∧ floor (usRealY ss) ≡ gy
                      ∧ usState ss ≡ Idle
                      ∧ usGridZ ss ≢ z
                    snap uid ss
                        | affects uid ss = ss { usGridZ = z
                                              , usRealZ = fromIntegral z }
                        | otherwise      = ss
                    hit = [ uid | (uid, ss) ← HM.toList simStates
                                , affects uid ss ]
                in (uts { utsSimStates = HM.mapWithKey snap simStates }, hit)
            -- Mirror into the render-facing instances so the visual z
            -- updates this frame, same as UnitTeleport does.
            forM_ snapped $ \uid →
                atomicModifyIORef' (ucUnitManagerRef (toUnitCombatCapability env)) $ \um →
                    case HM.lookup uid (umInstances um) of
                        Nothing → (um, ())
                        Just inst →
                            let inst' = inst { uiGridZ = z
                                             , uiRealZ = fromIntegral z }
                            in (um { umInstances =
                                    HM.insert uid inst' (umInstances um) }, ())

-- | The unit's own world page, or 'Nothing' when the manager no longer
--   holds it — the ownership lookup #1593's two lifecycle commands both
--   resolve their page through.
unitPageOf ∷ EngineEnv → UnitId → IO (Maybe WorldPageId)
unitPageOf env uid = do
    um ← readIORef (ucUnitManagerRef (toUnitCombatCapability env))
    pure $ uiPage <$> HM.lookup uid (umInstances um)

-- | Every unit id currently standing on one page.
unitIdsOnPage ∷ EngineEnv → WorldPageId → IO (HS.HashSet UnitId)
unitIdsOnPage env pageId = do
    um ← readIORef (ucUnitManagerRef (toUnitCombatCapability env))
    pure $ HS.fromList (HM.keys (unitsOnPage pageId (umInstances um)))

-- | The surface z of one NAMED page's own tiles at (gx, gy) (#1593).
--
--   Resolves through @wmWorlds@, so a loaded page answers whether or not
--   it is visible, and returns 'Nothing' — never another page's z — when
--   that page is not loaded or holds no loaded chunk at the coordinates.
--   This replaces the pre-#1593 @wmVisible@ scan, which returned the
--   first visible page with a matching chunk and so let a terrain edit on
--   one page snap coordinate-matched units on another.
lookupSurfaceZ ∷ EngineEnv → WorldPageId → Int → Int → IO (Maybe Int)
lookupSurfaceZ env pageId gx gy = do
    wm ← readIORef (wsWorldManagerRef (toWorldSimCapability env))
    case lookup pageId (wmWorlds wm) of
        Nothing → return Nothing
        Just ws → do
            td ← readIORef (wsTilesRef ws)
            return $ case lookupChunk chunkCoord td of
                Just lc → Just (lcSurfaceMap lc VU.! columnIndex lx ly)
                Nothing → Nothing
  where
    (chunkCoord, (lx, ly)) = globalToChunk gx gy
