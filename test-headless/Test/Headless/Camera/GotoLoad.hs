{-# LANGUAGE Strict #-}
-- | @camera.goToTile@ drives the repaired glacier-rim loader (#1953).
--
--   "Test.Headless.Camera.GotoClamp" pins the retained bounds purely.
--   This spec is the other half: the teleport that the retired #297
--   fence used to PREVENT is now performed for real, on the world size
--   that fence collapsed hardest — the 8-chunk minimum, where the old
--   6-chunk buffer exceeded the half-size and pinned every target to the
--   centre in map view with z-tracking off.
--
--   One example, because the point is one causal chain and each link
--   only means something after the one before it:
--
--     1. A private w8 page is initialised, shown, and left ACTIVE with
--        the camera at the origin. World init seeds the load-radius box
--        around chunk (0,0) ('World.Chunk.Queue.initialChunkQueue'), and
--        the camera-visible loader holds exactly that box while the
--        camera stays there — so the target window's outer ring is
--        provably absent, not merely unobserved.
--     2. The REAL registered @camera.goToTile@ runs, through the same
--        Lua surface the event log and popups reach it by.
--     3. The camera lands on the retained pan/drag bound at tile zoom
--        with z-tracking on — the behaviour requirement 2 restores.
--     4. That outer ring then LOADS. Only the world worker generates it
--        ('World.Thread.ChunkLoading.updateChunkLoading', gated on
--        @camZoom < zoomFadeEnd + 0.5@, which step 3's zoom satisfies),
--        so chunks appearing is a live worker doing the work the old
--        fence existed to prevent. The harness's own worker-health check
--        ('Test.Headless.Harness.checkHeadlessWorkers') independently
--        fails this group if that worker ever fail-stopped.
--
--   Run just this gate: @cabal test synarchy-test-headless
--   --test-options='--match "camera.gotoTile rim-ward"'@.
module Test.Headless.Camera.GotoLoad (spec) where

import UPrelude
import Test.Hspec
import Control.Concurrent (threadDelay)
import Data.IORef (newIORef, readIORef)
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import Engine.Core.State (EngineEnv(..))
import Engine.Core.Thread (ThreadControl(..))
import Engine.Graphics.Camera (Camera2D(..), CameraFacing(..))
import Engine.Loop.Camera (cameraYLimitChunks, cameraGlacierBufferChunks)
import Engine.Scripting.Lua.API (registerLuaAPI)
import Engine.Scripting.Lua.Thread (createLuaBackendState)
import Engine.Scripting.Lua.Thread.Console (executeDebugLua)
import Engine.Scripting.Lua.Types (LuaBackendState(..))
import Test.Headless.Harness
import World.Types

-- | The private page. Seed 7 / size 8 / 3 plates is the same w8 world
--   "Test.Headless.WorldGen.Exposure"'s #298 regression generates, so
--   this spec adds no new worldgen shape to the suite.
page ∷ WorldPageId
page = WorldPageId (T.pack "goto_load_w8")

worldSizeChunks ∷ Int
worldSizeChunks = 8

-- | The teleport target: far outside the world on the v axis, and
--   exactly on the u axis' origin.
--
--   @gridToWorld FaceSouth gx gy@ is
--   @((gx - gy)·tileHalfWidth, (gx + gy)·tileHalfDiamondHeight)@, so an
--   equal pair puts the whole request on the clamped v axis and leaves
--   the cylindrical u axis at 0 — which keeps the landing chunk (and so
--   the expected load window) independent of the seam wrap.
targetTile ∷ Int
targetTile = 40

-- | Where the teleport must land: the retained pan/drag bound, unclamped
--   on u. 'clampF' returns the bound itself once the request passes it,
--   and the request (80 half-diamonds) passes it comfortably, so this is
--   an exact equality rather than an approximation.
expectedPosition ∷ (Float, Float)
expectedPosition =
    (0, cameraYLimitChunks cameraGlacierBufferChunks worldSizeChunks)

-- | The canonical chunks the post-teleport load window gains and the
--   pre-teleport one cannot hold.
--
--   Landing at v = 32 tiles puts the camera on chunk (1,1), so the
--   loader's window becomes the @(-1..3)²@ Chebyshev square minus the
--   coords its @inBoundsV@ filter drops (@|cx + cy| > 4@ on this world),
--   u-wrapped into storage coords. Against the init box @(-2..2)²@ that
--   is a gain of exactly these five — (-1,3) reached both directly and
--   as the u-alias of (3,-1) — and every one of them stays inside the
--   eviction radius of the new camera chunk.
newlyLoadedChunks ∷ [ChunkCoord]
newlyLoadedChunks =
    [ ChunkCoord 3 0, ChunkCoord 3 1
    , ChunkCoord 0 3, ChunkCoord 1 3, ChunkCoord (-1) 3 ]

spec ∷ SpecWith EngineEnv
spec = describe "camera.gotoTile rim-ward load on the minimum world (#1953)" $

    it "teleports to the retained bound at tile zoom and loads the\
       \ camera-visible window there" $ \env → do
        -- (1) A private, ACTIVE, visible w8 page with the camera parked
        --     at the origin, so the loader's window is the init box.
        moveCamera env 0 0
        sendWorldCommand env (WorldInit page 7 worldSizeChunks 3 Nothing)
        ws ← waitForWorldInit env page 120
        sendWorldCommand env (WorldShow page)
        visible ← waitFor 60 $ do
            mgr ← readIORef (worldManagerRef env)
            pure (page `elem` wmVisible mgr)
        visible `shouldBe` True

        before ← readIORef (cameraRef env)
        -- The tile arithmetic above is written for the default facing;
        -- say so, rather than silently landing somewhere else.
        camFacing before `shouldBe` FaceSouth

        -- The outer ring cannot be loaded yet: nothing has ever asked
        -- for it, and the loader's window around the origin excludes it.
        loadedBefore ← mapM (chunkLoaded ws) newlyLoadedChunks
        loadedBefore `shouldBe` map (const False) newlyLoadedChunks

        -- (2) The real registered API, not a direct call to the clamp.
        lua ← newBareLuaBackend env
        reply ← executeDebugLua (lbsLuaState lua) $
            T.pack ("camera.goToTile(" ⧺ show targetTile ⧺ ", "
                    ⧺ show targetTile ⧺ ")")
        reply `shouldSatisfy` (not ∘ isLuaError)

        -- (3) The retained bound, at tile zoom, tracking z.
        after ← readIORef (cameraRef env)
        camPosition after `shouldBe` expectedPosition
        camZoom after `shouldBe` 0.5
        camZTracking after `shouldBe` True

        -- (4) …and the world worker generates the window that lands on.
        loadedAfter ← mapM (\c → waitForChunksAt ws c 120) newlyLoadedChunks
        loadedAfter `shouldBe` map (const True) newlyLoadedChunks

-- | Is this canonical chunk coord present in the page's tile store?
chunkLoaded ∷ WorldState → ChunkCoord → IO Bool
chunkLoaded ws coord = HM.member coord ∘ wtdChunks ⊚ getWorldTileData ws

-- | Poll @check@ at 50 ms until it holds or @timeoutSecs@ elapses.
waitFor ∷ Int → IO Bool → IO Bool
waitFor timeoutSecs check = go (timeoutSecs * 20)
  where
    go n
      | n ≤ 0 = pure False
      | otherwise = do
          ok ← check
          if ok then pure True else threadDelay 50000 >> go (n - 1)

-- | A Lua interpreter carrying the production API against this engine,
--   the shape "Test.Headless.Power.Placement" uses. No @scripts/@ are
--   loaded: @camera@ is a registered engine table, so the global exists
--   the moment 'registerLuaAPI' returns.
newBareLuaBackend ∷ EngineEnv → IO LuaBackendState
newBareLuaBackend env = do
    lua ← createLuaBackendState (luaToEngineQueue env) (luaQueue env)
                                (assetPoolRef env) (nextObjectIdRef env)
                                (inputStateRef env) (loggerRef env)
    stateRef ← newIORef ThreadRunning
    registerLuaAPI (lbsLuaState lua) env lua stateRef
    pure lua

isLuaError ∷ Text → Bool
isLuaError t = T.pack "error:" `T.isPrefixOf` t
             ∨ T.pack "syntax error:" `T.isPrefixOf` t
