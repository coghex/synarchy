{-# LANGUAGE UnicodeSyntax, OverloadedStrings #-}
-- | Window-mode transition / windowed-geometry cache tests (issue #907).
--
--   The regression: 'Engine.Scripting.Lua.Message.Video.handleSetWindowMode'
--   decided whether to cache the live windowed geometry by reading
--   @vcWindowMode@ — a field
--   'Engine.Scripting.Lua.API.Config.setWindowModeFn' has ALREADY
--   overwritten with the TARGET mode on the Lua thread by the time the
--   handler drains the queue on the render thread a frame later. So
--   leaving @windowed@ saw @borderless@ and skipped the cache, and
--   returning to @windowed@ saw @windowed@, cached the borderless
--   monitor geometry, and "restored" that.
--
--   The fix keys every decision off 'wsAppliedMode' — the mode the render
--   thread last actually applied — so these tests deliberately pin the
--   reported ordering: each case is written as if @vcWindowMode@ already
--   holds the target, which is exactly why the config never appears
--   below. Nothing here needs a window, a GPU, or a live fullscreen
--   switch: 'applyWindowModeTransition' is the whole decision.
module Test.Headless.Graphics.WindowMode (spec) where

import UPrelude
import Test.Hspec
import Engine.Core.State
  ( WindowState(..), defaultWindowState, bootAppliedWindowMode
  , leavingWindowedMode, applyWindowModeTransition )
import Engine.Graphics.Config (WindowMode(..))

-- | A windowed session: the user's window, as the render thread would
--   find it on screen before the first switch away.
windowedSession ∷ WindowState
windowedSession = WindowState
  { wsWindowedPos  = (317, 208)
  , wsWindowedSize = (1024, 768)
  , wsAppliedMode  = Windowed
  }

userPos, userSize ∷ (Int, Int)
userPos  = (317, 208)
userSize = (1024, 768)

-- | What a borderless window looks like live: the whole monitor at the
--   origin. Caching THIS is the bug's signature.
monitorPos, monitorSize ∷ (Int, Int)
monitorPos  = (0, 0)
monitorSize = (2560, 1440)

-- | One queued request: the target mode plus the geometry the render
--   thread samples before applying it. Folding a list of these is exactly
--   what 'handleSetWindowMode' does as 'processLuaMessages' drains the
--   queue, one message per call, in order.
type Request = (WindowMode, (Int, Int), (Int, Int))

drain ∷ WindowState → [Request] → WindowState
drain = foldl' (\ws (target, pos, size) →
                  applyWindowModeTransition target pos size ws)

spec ∷ Spec
spec = do
    describe "leavingWindowedMode" $ do
        it "caches when leaving windowed for borderless" $
            leavingWindowedMode Windowed BorderlessWindowed `shouldBe` True

        it "caches when leaving windowed for fullscreen" $
            leavingWindowedMode Windowed Fullscreen `shouldBe` True

        it "never caches when entering windowed" $ do
            leavingWindowedMode BorderlessWindowed Windowed `shouldBe` False
            leavingWindowedMode Fullscreen Windowed `shouldBe` False

        it "never caches when moving between the non-windowed modes" $ do
            leavingWindowedMode BorderlessWindowed Fullscreen `shouldBe` False
            leavingWindowedMode Fullscreen BorderlessWindowed `shouldBe` False

        it "never caches when re-applying the mode already applied" $ do
            leavingWindowedMode Windowed Windowed `shouldBe` False
            leavingWindowedMode BorderlessWindowed BorderlessWindowed
                `shouldBe` False

    describe "applyWindowModeTransition" $ do
        it "records the windowed geometry on the way out to borderless" $ do
            let ws = applyWindowModeTransition BorderlessWindowed
                                               userPos userSize windowedSession
            wsWindowedPos ws `shouldBe` userPos
            wsWindowedSize ws `shouldBe` userSize
            wsAppliedMode ws `shouldBe` BorderlessWindowed

        -- The half a live GUI check cannot exercise without switching the
        -- monitor's video mode out from under the user.
        it "records the windowed geometry on the way out to fullscreen" $ do
            let ws = applyWindowModeTransition Fullscreen
                                               userPos userSize windowedSession
            wsWindowedPos ws `shouldBe` userPos
            wsWindowedSize ws `shouldBe` userSize
            wsAppliedMode ws `shouldBe` Fullscreen

        it "leaves the cache alone when entering windowed" $ do
            let borderless = applyWindowModeTransition BorderlessWindowed
                                userPos userSize windowedSession
                back = applyWindowModeTransition Windowed
                                monitorPos monitorSize borderless
            wsWindowedPos back `shouldBe` userPos
            wsWindowedSize back `shouldBe` userSize
            wsAppliedMode back `shouldBe` Windowed

        it "leaves the cache alone moving between non-windowed modes" $ do
            let ws = drain windowedSession
                       [ (BorderlessWindowed, userPos, userSize)
                       , (Fullscreen, monitorPos, monitorSize) ]
            wsWindowedPos ws `shouldBe` userPos
            wsWindowedSize ws `shouldBe` userSize
            wsAppliedMode ws `shouldBe` Fullscreen

    describe "window-mode round trips (#907)" $ do
        -- The exact reproduction from the issue.
        it "windowed -> borderless -> windowed restores pos AND size" $ do
            let ws = drain windowedSession
                       [ (BorderlessWindowed, userPos, userSize)
                       , (Windowed, monitorPos, monitorSize) ]
            (wsWindowedPos ws, wsWindowedSize ws)
                `shouldBe` (userPos, userSize)

        it "windowed -> fullscreen -> windowed restores pos AND size" $ do
            let ws = drain windowedSession
                       [ (Fullscreen, userPos, userSize)
                       , (Windowed, monitorPos, monitorSize) ]
            (wsWindowedPos ws, wsWindowedSize ws)
                `shouldBe` (userPos, userSize)

        -- Ordered back-to-back requests: whatever interleaving the Lua
        -- thread's config writes happened to take, the queue is drained
        -- in order and the geometry survives every hop.
        it "survives a long back-to-back chain through both other modes" $ do
            let ws = drain windowedSession
                       [ (BorderlessWindowed, userPos, userSize)
                       , (Fullscreen, monitorPos, monitorSize)
                       , (BorderlessWindowed, monitorPos, monitorSize)
                       , (Fullscreen, monitorPos, monitorSize)
                       , (Windowed, monitorPos, monitorSize) ]
            (wsWindowedPos ws, wsWindowedSize ws)
                `shouldBe` (userPos, userSize)
            wsAppliedMode ws `shouldBe` Windowed

        -- Two switches away without an intervening return: the FIRST one
        -- owns the cache, and a redundant repeat cannot clobber it.
        it "a repeated switch away does not re-cache the new geometry" $ do
            let ws = drain windowedSession
                       [ (BorderlessWindowed, userPos, userSize)
                       , (BorderlessWindowed, monitorPos, monitorSize)
                       , (Windowed, monitorPos, monitorSize) ]
            (wsWindowedPos ws, wsWindowedSize ws)
                `shouldBe` (userPos, userSize)

        it "re-caches a window the user moved between round trips" $ do
            let movedPos  = (42, 96)
                movedSize = (1440, 900)
                ws = drain windowedSession
                       [ (BorderlessWindowed, userPos, userSize)
                       , (Windowed, monitorPos, monitorSize)
                       -- ... user drags/resizes the restored window ...
                       , (BorderlessWindowed, movedPos, movedSize)
                       , (Windowed, monitorPos, monitorSize) ]
            (wsWindowedPos ws, wsWindowedSize ws)
                `shouldBe` (movedPos, movedSize)

    describe "bootAppliedWindowMode" $ do
        -- defaultWindowConfig only asks GLFW for fullscreen; a
        -- borderless-configured boot still comes up as a plain decorated
        -- window, so its first switch away must cache the real geometry.
        it "reports windowed for a borderless-configured boot" $
            bootAppliedWindowMode BorderlessWindowed `shouldBe` Windowed

        it "reports windowed for a windowed-configured boot" $
            bootAppliedWindowMode Windowed `shouldBe` Windowed

        it "reports fullscreen for a fullscreen-configured boot" $
            bootAppliedWindowMode Fullscreen `shouldBe` Fullscreen

        it "starts the default cache in the applied windowed state" $
            wsAppliedMode defaultWindowState `shouldBe` Windowed

        -- A borderless-configured boot's first toggle to borderless is a
        -- real departure from windowed, not a no-op.
        it "lets a borderless-configured boot cache on its first switch" $ do
            let boot = defaultWindowState
                  { wsAppliedMode = bootAppliedWindowMode BorderlessWindowed }
                ws = drain boot
                       [ (BorderlessWindowed, userPos, userSize)
                       , (Windowed, monitorPos, monitorSize) ]
            (wsWindowedPos ws, wsWindowedSize ws)
                `shouldBe` (userPos, userSize)
