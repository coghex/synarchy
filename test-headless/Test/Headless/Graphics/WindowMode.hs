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
--
--   Since #1731 this file also covers what CREATION establishes.
--   'Engine.Core.Defaults.defaultWindowConfig' now asks GLFW for
--   borderless as well as fullscreen, so a boot has three
--   distinguishable outcomes rather than one Boolean; and a creation
--   that APPLIED either non-windowed mode must additionally SEED the
--   windowed-geometry cache, because applying the mode at creation
--   consumes the first-switch caching opportunity the transition path
--   otherwise relies on. #1731 covered borderless and #1882 extended
--   the same seed to fullscreen, whose decorated window is sampled at
--   the same moment by the same caller. 'applyWindowCreation' is that
--   whole decision, and it is equally window-free.
module Test.Headless.Graphics.WindowMode (spec) where

import UPrelude
import Test.Hspec
import Engine.Core.State
  ( WindowState(..), defaultWindowState, appliedModeAtCreation
  , WindowCreationOutcome(..), applyWindowCreation
  , leavingWindowedMode, windowModeAlreadyApplied
  , applyWindowModeTransition )
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

-- | The DECORATED window GLFW just made, sampled before any mode
--   mutation: the geometry 'Engine.Graphics.Window.GLFW.createWindow'
--   hands 'applyWindowCreation'. Deliberately distinct from both
--   'userPos'\/'userSize' and 'monitorPos'\/'monitorSize', so a seed
--   taken from the wrong source is visible.
bootPos, bootSize ∷ (Int, Int)
bootPos  = (64, 48)
bootSize = (1280, 720)

-- | The WindowState the render thread holds right after
--   'Engine.Graphics.Window.GLFW.createWindow', given what GLFW actually
--   did with the requested mode.
createdWith ∷ WindowCreationOutcome → WindowState
createdWith outcome =
  applyWindowCreation outcome bootPos bootSize defaultWindowState

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

    -- Review round 1 on PR #908: keying the cache off wsAppliedMode made
    -- a REDUNDANT request meaningful where the old target-config guard
    -- had made it inert. `scripts/settings/data.lua`'s Defaults path
    -- calls engine.setWindowMode unconditionally, so a windowed session
    -- pressing Defaults would have run the Windowed branch's restore
    -- against a cache no switch away from windowed had ever filled —
    -- teleporting the live window onto defaultWindowState's 800x600 at
    -- (100,100). The handler short-circuits on this predicate instead.
    describe "windowModeAlreadyApplied" $ do
        it "reports a redundant request for each mode" $ do
            let applied m = defaultWindowState { wsAppliedMode = m }
            windowModeAlreadyApplied (applied Windowed) Windowed
                `shouldBe` True
            windowModeAlreadyApplied (applied BorderlessWindowed)
                                     BorderlessWindowed `shouldBe` True
            windowModeAlreadyApplied (applied Fullscreen) Fullscreen
                `shouldBe` True

        it "does not short-circuit a real switch" $ do
            windowModeAlreadyApplied windowedSession BorderlessWindowed
                `shouldBe` False
            windowModeAlreadyApplied windowedSession Fullscreen
                `shouldBe` False

        -- The exact regression: a boot-fresh windowed session whose cache
        -- still holds the defaults must not act on a redundant request.
        it "short-circuits a redundant windowed request on a fresh boot" $ do
            windowModeAlreadyApplied (createdWith CreatedPlain) Windowed
                `shouldBe` True

        -- Since #1731 a borderless-configured boot that GLFW could
        -- satisfy comes up BORDERLESS, so the settings screen's
        -- unconditional Defaults re-apply is redundant there too.
        it "short-circuits a redundant borderless request on a fresh boot" $ do
            windowModeAlreadyApplied (createdWith CreatedBorderless)
                                     BorderlessWindowed `shouldBe` True

        -- A borderless request GLFW could NOT satisfy leaves a plain
        -- decorated window, so asking for borderless again is a real
        -- switch — the same graceful-degradation shape fullscreen has.
        it "does not short-circuit a failed borderless boot's request" $ do
            windowModeAlreadyApplied (createdWith CreatedPlain)
                                     BorderlessWindowed `shouldBe` False

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

    describe "appliedModeAtCreation" $ do
        -- Round 2 on PR #908: the seed must come from what GLFW actually
        -- did, not from the configured mode. createWindow degrades BOTH
        -- a fullscreen and (since #1731) a borderless request to the
        -- plain window it just created when no primary monitor or video
        -- mode is available.
        it "reports fullscreen only when setFullscreen actually ran" $
            appliedModeAtCreation CreatedFullscreen `shouldBe` Fullscreen

        it "reports borderless only when the borderless mutation ran" $
            appliedModeAtCreation CreatedBorderless
                `shouldBe` BorderlessWindowed

        it "reports windowed for a plain creation" $
            appliedModeAtCreation CreatedPlain `shouldBe` Windowed

        it "seeds the pre-window default in the applied windowed state" $
            wsAppliedMode defaultWindowState `shouldBe` Windowed

        -- A fullscreen request that FAILED leaves a live plain window.
        -- Recording Fullscreen there would make the next windowed request
        -- a real switch that restores the never-filled default cache,
        -- teleporting that window to 800x600 at (100,100).
        it "leaves a failed fullscreen boot's windowed request inert" $ do
            let boot = createdWith CreatedPlain
            wsAppliedMode boot `shouldBe` Windowed
            windowModeAlreadyApplied boot Windowed `shouldBe` True

        -- A fullscreen boot that SUCCEEDED does switch, and since #1882
        -- it has real geometry to restore: the decorated window
        -- createWindow sampled before setFullscreen ran, not the
        -- pre-window fallback.
        it "treats a real fullscreen boot's windowed request as a switch" $ do
            let boot = createdWith CreatedFullscreen
            wsAppliedMode boot `shouldBe` Fullscreen
            windowModeAlreadyApplied boot Windowed `shouldBe` False
            leavingWindowedMode (wsAppliedMode boot) Windowed `shouldBe` False
            (wsWindowedPos boot, wsWindowedSize boot)
                `shouldBe` (bootPos, bootSize)

    -- #1731. Before it, `defaultWindowConfig` asked GLFW only for
    -- fullscreen: a borderless-configured boot came up as an ordinary
    -- decorated window, `wsAppliedMode` recorded `Windowed`, and the
    -- reported mode and the applied mode disagreed until some later
    -- explicit request performed the real transition.
    describe "borderless creation (#1731)" $ do
        it "records BorderlessWindowed when the mutation ran" $
            wsAppliedMode (createdWith CreatedBorderless)
                `shouldBe` BorderlessWindowed

        -- Requirement 4: no primary monitor / no video mode leaves the
        -- plain decorated window createWindow already made.
        it "falls back to Windowed when the mutation could not run" $
            wsAppliedMode (createdWith CreatedPlain) `shouldBe` Windowed

        -- Requirement 3, first half. Applying borderless at creation
        -- means the first switch to Windowed is an ENTRY, and
        -- applyWindowModeTransition never caches on the way in — so the
        -- cache has to be seeded here, from the live decorated window.
        it "seeds the windowed cache from the pre-mutation decorated window" $ do
            let boot = createdWith CreatedBorderless
            (wsWindowedPos boot, wsWindowedSize boot)
                `shouldBe` (bootPos, bootSize)

        it "does not seed the cache when the mutation could not run" $ do
            let boot = createdWith CreatedPlain
            (wsWindowedPos boot, wsWindowedSize boot)
                `shouldBe` ( wsWindowedPos defaultWindowState
                           , wsWindowedSize defaultWindowState )

        -- Requirement 3, second half: the seed is what the FIRST
        -- transition to Windowed actually restores, rather than
        -- defaultWindowState's (100,100) / 800x600 fallback.
        it "restores that seed on the first switch to windowed" $ do
            let ws = drain (createdWith CreatedBorderless)
                       [ (Windowed, monitorPos, monitorSize) ]
            (wsWindowedPos ws, wsWindowedSize ws)
                `shouldBe` (bootPos, bootSize)
            wsAppliedMode ws `shouldBe` Windowed

        -- And the seed is not sticky: once the user has a real windowed
        -- window, the ordinary transition cache takes over from it.
        it "hands the cache back to the transition path after that" $ do
            let ws = drain (createdWith CreatedBorderless)
                       [ (Windowed, monitorPos, monitorSize)
                       -- ... user drags/resizes the restored window ...
                       , (BorderlessWindowed, userPos, userSize)
                       , (Windowed, monitorPos, monitorSize) ]
            (wsWindowedPos ws, wsWindowedSize ws)
                `shouldBe` (userPos, userSize)

        -- A failed borderless boot IS the windowed state, so its own
        -- first switch away caches the live geometry exactly as before.
        it "lets a failed borderless boot cache on its first switch" $ do
            let ws = drain (createdWith CreatedPlain)
                       [ (BorderlessWindowed, userPos, userSize)
                       , (Windowed, monitorPos, monitorSize) ]
            (wsWindowedPos ws, wsWindowedSize ws)
                `shouldBe` (userPos, userSize)

    -- #1882. A successful FULLSCREEN creation is structurally the same
    -- case as the borderless one above: `setFullscreen` runs on the
    -- decorated window `createWindow` just made, so applying the mode at
    -- creation consumes the first-switch caching opportunity and the
    -- boot's first `Windowed` request is an ENTRY that
    -- `applyWindowModeTransition` never caches for. #1731 seeded only
    -- borderless, leaving that request to restore `defaultWindowState`'s
    -- (100,100) / 800x600 fallback and teleport a user who had saved a
    -- custom resolution.
    describe "fullscreen creation (#1882)" $ do
        -- Requirement 1. The seed comes from the live pre-mutation
        -- decorated window, which is why `bootPos`/`bootSize` are
        -- distinct from both the user and monitor fixtures: a seed taken
        -- from the wrong source, or not taken at all, is visible here.
        it "seeds the windowed cache from the pre-mutation decorated window" $ do
            let boot = createdWith CreatedFullscreen
            (wsWindowedPos boot, wsWindowedSize boot)
                `shouldBe` (bootPos, bootSize)

        -- Requirement 2. A fullscreen request that could not be applied
        -- leaves the plain decorated window on screen, so it records
        -- `Windowed`, its first `Windowed` request stays inert, and there
        -- is nothing to seed — graceful degradation is unchanged.
        it "does not seed the cache when setFullscreen could not run" $ do
            let boot = createdWith CreatedPlain
            wsAppliedMode boot `shouldBe` Windowed
            windowModeAlreadyApplied boot Windowed `shouldBe` True
            (wsWindowedPos boot, wsWindowedSize boot)
                `shouldBe` ( wsWindowedPos defaultWindowState
                           , wsWindowedSize defaultWindowState )

        -- The seed is what the first transition to Windowed actually
        -- restores — the symptom the issue reports, stated end to end.
        it "restores that seed on the first switch to windowed" $ do
            let ws = drain (createdWith CreatedFullscreen)
                       [ (Windowed, monitorPos, monitorSize) ]
            (wsWindowedPos ws, wsWindowedSize ws)
                `shouldBe` (bootPos, bootSize)
            wsAppliedMode ws `shouldBe` Windowed

        -- And it is not sticky: once the user has a real windowed
        -- window, the ordinary transition cache takes over from it.
        it "hands the cache back to the transition path after that" $ do
            let ws = drain (createdWith CreatedFullscreen)
                       [ (Windowed, monitorPos, monitorSize)
                       -- ... user drags/resizes the restored window ...
                       , (Fullscreen, userPos, userSize)
                       , (Windowed, monitorPos, monitorSize) ]
            (wsWindowedPos ws, wsWindowedSize ws)
                `shouldBe` (userPos, userSize)

        -- Requirement 3: a plain boot is untouched. It IS the windowed
        -- state, so its own first switch away caches the live geometry
        -- exactly as before and the creation seed never applies.
        it "lets a plain boot cache on its first switch away" $ do
            let ws = drain (createdWith CreatedPlain)
                       [ (Fullscreen, userPos, userSize)
                       , (Windowed, monitorPos, monitorSize) ]
            (wsWindowedPos ws, wsWindowedSize ws)
                `shouldBe` (userPos, userSize)
