-- test/Test/Engine/Graphics/Window/GLFW.hs
--
-- Two kinds of example live here, and the split is deliberate (#1573).
--
-- 1. PROJECT-OWNED COVERAGE. @createWindowSpec@ drives
--    'Engine.Graphics.Window.GLFW.createWindow' -- the window path
--    @app/App/Graphical.hs@ and @app/App/Preview.hs@ boot through, which
--    nothing else in either suite reaches -- against
--    'createWindowConfig', a config this module supplies, and asserts the
--    six state mutations it performs: the three geometry refs, the
--    applied-mode update, and the two Lua queue writes.
--
--    Every one of those six is made observable INDEPENDENTLY of engine
--    initialization. 'Engine.Core.Init' already seeds the size and
--    framebuffer refs from the video config, the position ref to @(0,0)@
--    and @wsAppliedMode@ to 'Windowed', so a test that merely checked the
--    ordinary non-fullscreen outcome would stay green with the write
--    deleted. 'withCreatedWindow' therefore stamps a distinct, impossible
--    sentinel into each observer first and drains the Lua queue, and each
--    example shows the live value differs from that sentinel before
--    comparing the ref against it. Removing any single write turns the
--    corresponding example red.
--
--    Raw @Graphics.UI.GLFW@ getters appear inside that coverage purely as
--    observation oracles for what the project code did; they are not
--    preflight checks and are not covered by the rule below.
--
-- 2. SUPPORTED-ENVIRONMENT PREFLIGHT. @environmentSpec@ holds the raw
--    upstream checks that survive, and each one names the environment
--    question it answers and what in this suite depends on the answer. A
--    raw-binding example with no such question was removed rather than
--    kept: the window-position example asserted @x >= 0 && y >= 0@, which
--    is false for a monitor left of or above the primary and is now
--    covered for real by the position-ref example above; the monitor-name
--    and GLFW-clock examples exercised bindings no production module
--    calls at all (@grep@ finds no @getMonitorName@, @getTime@ or
--    @setTime@ under @src/@ or @app/@), so they answered no question this
--    project's supported environment has to satisfy.
--
-- This module stays display-required and therefore stays in
-- @synarchy-test-graphical@ (#1153): 'createWindow' opens a real window.
-- The pure window-mode policy remains covered GPU-free by
-- 'Test.Headless.Graphics.WindowMode'.
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Test.Engine.Graphics.Window.GLFW
  ( spec
  , withCreatedWindow
  , CreateWindowObservation(..)
  , createWindowConfig
  , sentinelWindowSize
  , sentinelFramebufferSize
  , sentinelWindowPos
  , sentinelAppliedMode
  ) where

import UPrelude
import qualified Data.Text as T
import Data.IORef (newIORef, modifyIORef', readIORef, writeIORef)
import Test.Hspec
import Engine.Core.Defaults (defaultWindowConfig)
import Engine.Core.Monad (EngineM, unEngineM)
import Engine.Core.State
import Engine.Core.Capability.Render (RenderCapability(..), toRenderCapability)
import qualified Engine.Core.Queue as Q
import Engine.Graphics.Config (WindowMode(..), defaultVideoConfig)
import Engine.Graphics.Window.GLFW (createWindow)
import Engine.Graphics.Window.Types (Window(..), WindowConfig(..))
import Engine.Scripting.Lua.Types (LuaMsg(..))
import qualified Graphics.UI.GLFW as GLFW

-- | The window 'withCreatedWindow' asks 'createWindow' for. Deliberately
--   NEITHER fullscreen nor borderless (requirement: the applied mode
--   must come out 'Windowed'), and deliberately not a size any video
--   config would produce, so it cannot be confused with the geometry
--   'Engine.Core.Init' seeded the refs with.
--
--   Both monitor-sized modes are excluded for the same reason: this
--   suite runs attached to a human's desktop, and asking for either
--   would resize and undecorate a real window mid-run. The pure
--   creation-outcome policy — including #1731's borderless branch and
--   its windowed-cache seed — is covered GPU-free by
--   'Test.Headless.Graphics.WindowMode'.
createWindowConfig ∷ WindowConfig
createWindowConfig = WindowConfig
  { wcWidth      = 641
  , wcHeight     = 481
  , wcFullscreen = False
  , wcBorderless = False
  , wcTitle      = T.pack "Synarchy createWindow coverage"
  , wcResizable  = True
  , wcVisible    = True
  , wcFocused    = True
  }

-- | Pre-call sentinels. Each observer gets its own so that a write
--   landing in the WRONG ref is caught too, and all are impossible as
--   real GLFW geometry (a negative size, and a position no virtual
--   screen reaches).
sentinelWindowSize, sentinelFramebufferSize, sentinelWindowPos ∷ (Int, Int)
sentinelWindowSize      = (-9001, -9002)
sentinelFramebufferSize = (-9003, -9004)
sentinelWindowPos       = (-9005, -9006)

-- | Pre-call sentinel for @wsAppliedMode@. Distinct from the mode
--   'appliedModeAtCreation' gives for THIS config, so a deleted update
--   fails; the assertion below additionally pins the exact expected
--   outcome, so a wrong-branch update fails too.
sentinelAppliedMode ∷ WindowMode
sentinelAppliedMode = BorderlessWindowed

-- | Everything one 'createWindow' invocation is judged on, captured
--   inside its resource scope while the window it created is still
--   alive.
data CreateWindowObservation = CreateWindowObservation
  { cwoLiveWindowSize      ∷ (Int, Int)
    -- ^ 'GLFW.getWindowSize' of the window 'createWindow' returned.
  , cwoLiveFramebufferSize ∷ (Int, Int)
  , cwoLiveWindowPos       ∷ (Int, Int)
  , cwoRefWindowSize       ∷ (Int, Int)
    -- ^ @rcWindowSizeRef@ after the call.
  , cwoRefFramebufferSize  ∷ (Int, Int)
  , cwoRefWindowPos        ∷ (Int, Int)
  , cwoAppliedMode         ∷ WindowMode
    -- ^ @wsAppliedMode@ after the call.
  , cwoQueueDrainedBefore  ∷ [LuaMsg]
    -- ^ The pre-invocation baseline, drained and discarded so that
    --   everything in 'cwoQueueAfter' is attributable to this call.
  , cwoQueueAfter          ∷ [LuaMsg]
    -- ^ Everything the Lua queue received during the invocation.
  } deriving (Show)

-- | Run 'createWindow' and hand the observation to @k@ from INSIDE the
--   resource continuation.
--
--   That placement is load-bearing. 'createWindow' registers
--   'GLFW.terminate' in its outermost 'Engine.Core.Resource.allocResource'
--   scope, and 'allocResource' runs cleanup when the continuation exits
--   (@src/Engine/Core/Resource.hs:24-26@) -- so calling it from inside a
--   single hspec example would tear GLFW down, and with it the shared
--   window @test/Spec.hs@ created, under every example that follows,
--   including the Vulkan surface and device specs. @test/Spec.hs@ instead
--   wraps the WHOLE @hspec@ run in this bracket: the created window and
--   the GLFW initialization stay alive for every graphical example, and
--   the destroy/terminate cleanup still runs, once, after the test run
--   exits.
--
--   A failed creation is reported to the caller rather than thrown, so
--   the rest of the suite still runs and the failure surfaces as an
--   ordinary example failure.
withCreatedWindow
  ∷ ∀ α. EngineEnv → EngineState
  → (Either Text CreateWindowObservation → IO α) → IO α
withCreatedWindow env state k = do
    stateRef ← newIORef state
    let env'  = env { engineStateRef = stateRef }
        cap   = toRenderCapability env'
        queue = luaQueue env'
    -- Establish observers that CANNOT already satisfy the postconditions.
    writeIORef (rcWindowSizeRef cap) sentinelWindowSize
    writeIORef (rcFramebufferSizeRef cap) sentinelFramebufferSize
    writeIORef (rcWindowPosRef cap) sentinelWindowPos
    modifyIORef' (rcWindowStateRef cap) $ \ws →
      ws { wsAppliedMode = sentinelAppliedMode }
    drainedBefore ← Q.flushQueue queue
    resultRef ← newIORef Nothing
    let action ∷ EngineM EngineState Window
        action = createWindow createWindowConfig
    _ ← unEngineM action env' $ \outcome → do
        observation ← case outcome of
            Left err → pure $ Left $ T.pack $ "createWindow failed: " ⧺ show err
            Right (Window win) → do
                liveSize ← GLFW.getWindowSize win
                liveFb   ← GLFW.getFramebufferSize win
                livePos  ← GLFW.getWindowPos win
                refSize  ← readIORef (rcWindowSizeRef cap)
                refFb    ← readIORef (rcFramebufferSizeRef cap)
                refPos   ← readIORef (rcWindowPosRef cap)
                ws       ← readIORef (rcWindowStateRef cap)
                queued   ← Q.flushQueue queue
                pure $ Right CreateWindowObservation
                    { cwoLiveWindowSize      = liveSize
                    , cwoLiveFramebufferSize = liveFb
                    , cwoLiveWindowPos       = livePos
                    , cwoRefWindowSize       = refSize
                    , cwoRefFramebufferSize  = refFb
                    , cwoRefWindowPos        = refPos
                    , cwoAppliedMode         = wsAppliedMode ws
                    , cwoQueueDrainedBefore  = drainedBefore
                    , cwoQueueAfter          = queued
                    }
        value ← k observation
        writeIORef resultRef (Just value)
        pure state
    stashed ← readIORef resultRef
    case stashed of
        Just value → pure value
        Nothing    → error "withCreatedWindow: continuation produced no result"

-- | Main test specification for GLFW functionality
spec ∷ EngineEnv → EngineState → Either Text CreateWindowObservation → Spec
spec _env state createObservation =
    case glfwWindow (graphicsState state) of
        Just _win@(Window glfwWin) → do
            createWindowSpec createObservation
            environmentSpec glfwWin

        Nothing → describe "GLFW Window" $
            it "exists" $ expectationFailure "Window not found in state"

-- | Project-owned coverage of 'createWindow'. Every example reads the
--   one observation captured by 'withCreatedWindow'; see this module's
--   header for why the invocation lives there and not in an example.
createWindowSpec ∷ Either Text CreateWindowObservation → Spec
createWindowSpec createObservation =
    describe "Engine.Graphics.Window.GLFW.createWindow" $ do
        it "creates a window at the configured size" $ withObservation $ \obs →
            cwoLiveWindowSize obs `shouldBe`
                (wcWidth createWindowConfig, wcHeight createWindowConfig)

        it "records the live window size in rcWindowSizeRef" $
            withObservation $ \obs → do
                cwoLiveWindowSize obs `shouldNotBe` sentinelWindowSize
                cwoRefWindowSize obs `shouldBe` cwoLiveWindowSize obs

        it "records the live framebuffer size in rcFramebufferSizeRef" $
            withObservation $ \obs → do
                cwoLiveFramebufferSize obs `shouldNotBe` sentinelFramebufferSize
                cwoRefFramebufferSize obs `shouldBe` cwoLiveFramebufferSize obs

        it "records the live window position in rcWindowPosRef" $
            withObservation $ \obs → do
                cwoLiveWindowPos obs `shouldNotBe` sentinelWindowPos
                cwoRefWindowPos obs `shouldBe` cwoLiveWindowPos obs

        -- The config asks for a plain window — neither fullscreen nor
        -- borderless — so the outcome 'createWindow' records must be the
        -- one 'appliedModeAtCreation' gives for 'CreatedPlain'.
        it "records Windowed as the applied mode for a plain config" $
            withObservation $ \obs → do
                wcFullscreen createWindowConfig `shouldBe` False
                wcBorderless createWindowConfig `shouldBe` False
                cwoAppliedMode obs `shouldNotBe` sentinelAppliedMode
                cwoAppliedMode obs `shouldBe` Windowed
                cwoAppliedMode obs `shouldBe` appliedModeAtCreation CreatedPlain

        -- The queue was drained immediately before the call, so this
        -- attributes both messages -- and their absence -- to this
        -- invocation alone. Asserting the exact list also pins that
        -- neither is enqueued twice.
        it "enqueues one LuaWindowResize and one LuaFramebufferResize" $
            withObservation $ \obs → do
                let (w, h)   = cwoLiveWindowSize obs
                    (fw, fh) = cwoLiveFramebufferSize obs
                filter isResizeMsg (cwoQueueAfter obs) `shouldBe`
                    [ LuaWindowResize w h, LuaFramebufferResize fw fh ]
  where
    withObservation ∷ (CreateWindowObservation → Expectation) → Expectation
    withObservation assertion = case createObservation of
        Left err  → expectationFailure $ T.unpack err
        Right obs → assertion obs

    isResizeMsg ∷ LuaMsg → Bool
    isResizeMsg (LuaWindowResize _ _)      = True
    isResizeMsg (LuaFramebufferResize _ _) = True
    isResizeMsg _                          = False

-- | Supported-environment preflight checks. These call
--   @Graphics.UI.GLFW@ directly on purpose: each answers a question about
--   the machine the suite is being run by hand on, and a failure here
--   means the environment cannot support the project-owned examples
--   above (or the Vulkan specs that follow them) rather than that
--   Synarchy regressed.
environmentSpec ∷ GLFW.Window → Spec
environmentSpec glfwWin = do
    describe "GLFW Window (supported-environment preflight)" $ do
        -- Question: does this environment hand back a window at the size
        -- that was asked for, or clamp it? Every size assertion in
        -- 'createWindowSpec' presumes it does not clamp.
        it "honours the requested window size" $ do
            let requested = defaultWindowConfig defaultVideoConfig
            size ← GLFW.getWindowSize glfwWin
            size `shouldBe` (wcWidth requested, wcHeight requested)

        -- Question: does this environment back the window with a real
        -- drawable surface? A degenerate framebuffer would make the
        -- framebuffer-ref example above meaningless and leaves the
        -- Vulkan surface spec nothing to draw into.
        it "reports a non-degenerate framebuffer" $ do
            (fbWidth, fbHeight) ← GLFW.getFramebufferSize glfwWin
            fbWidth `shouldSatisfy` (> 0)
            fbHeight `shouldSatisfy` (> 0)

    describe "GLFW Monitor (supported-environment preflight)" $
        -- Question: is a monitor attached? 'createWindow''s fullscreen
        -- branch reads 'GLFW.getPrimaryMonitor', as does
        -- 'Engine.Scripting.Lua.Message.Video'; with none attached that
        -- path only ever exercises its degrade-to-windowed fallback.
        it "has a primary monitor" $ do
            monitor ← GLFW.getPrimaryMonitor
            monitor `shouldSatisfy` isJust

    describe "GLFW Vulkan Support (supported-environment preflight)" $
        -- Question: is this GLFW build's Vulkan loader usable? Every
        -- Vulkan spec in this suite -- instance, surface, device -- is
        -- unreachable without it.
        it "has Vulkan support" $ do
            supported ← GLFW.vulkanSupported
            supported `shouldBe` True
