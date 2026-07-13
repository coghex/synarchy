{-# LANGUAGE UnicodeSyntax, OverloadedStrings #-}
-- | Regression for #813: a zoom-map (chunk) selection binds to the
--   clicked chunk at click time instead of re-resolving from shared
--   hover state at render time.
--
--   Before this fix, a zoom-map left click armed 'zoomSelectNow' and
--   left the chunk to be resolved from whatever 'zoomCursorPos' /
--   camera happened to be current whenever 'makeCursorQuad' next ran
--   (World.Render.Zoom.Cursor). A later per-tick hover push
--   ('hud.update' → 'world.setZoomCursorHover') or a camera pan/zoom
--   between the click and that render pass could retarget the
--   selection away from what the player actually clicked.
--
--   The fix mirrors the already-shipped zoomed-in tile path (#123,
--   #367): 'world.pickChunk' resolves the chunk under the click pixel
--   synchronously (backed by the pure 'pixelToChunkOrigin'), and
--   'world.selectChunk' — queuing 'WorldSelectChunkByCoord', handled by
--   'handleWorldSelectChunkByCoordCommand' — commits it in one atomic
--   write alongside the opposing tile-selection clear (#135). This
--   spec drives those real handlers directly (mirroring
--   Test.Headless.World.SelectTileZ), so it fails if the click path
--   ever goes back to arming a deferred, shared-hover-state commit.
module Test.Headless.World.SelectChunk (spec) where

import UPrelude
import Test.Hspec
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.IORef (readIORef, writeIORef, modifyIORef')
import Engine.Core.Init (initializeEngineHeadless, EngineInitResult(..))
import Engine.Core.State (EngineEnv(..))
import Engine.Asset.Handle (TextureHandle)
import Engine.Graphics.Camera (Camera2D(..), defaultCamera)
import World.Cursor.Types (CursorState(..), emptyCursorState)
import World.Generate.Types (WorldGenParams(..), defaultWorldGenParams)
import World.State.Types (WorldState(..), emptyWorldState, WorldManager(..))
import World.Page.Types (WorldPageId(..))
import World.Thread.Command.Cursor
    ( handleWorldSelectChunkByCoordCommand
    , handleWorldSetZoomCursorHoverCommand
    )
import World.Thread.Command.UI (handleWorldHideCommand)
import World.Render.Zoom.Cursor (pixelToChunkOrigin, makeCursorQuad)

pidA, pidB ∷ WorldPageId
pidA = WorldPageId "select_chunk_test_a"
pidB = WorldPageId "select_chunk_test_b"

-- | Small, fixed world size so pixelToChunkOrigin has real geometry to
--   work with (the click-side tests don't touch tiles/textures).
testParams ∷ WorldGenParams
testParams = defaultWorldGenParams { wgpWorldSize = 64 }

-- | A page registered as the sole visible world, with its own cursor.
freshPage ∷ IO WorldState
freshPage = do
    ws ← emptyWorldState
    writeIORef (wsGenParamsRef ws) (Just testParams)
    pure ws

initEnv ∷ IO EngineEnv
initEnv = do
    EngineInitResult env ← initializeEngineHeadless
    pure env

-- | Register a single page as the whole world manager and give the
--   engine a concrete, non-degenerate viewport + camera to pick against.
setSinglePage ∷ EngineEnv → WorldPageId → WorldState → IO ()
setSinglePage env pid ws = do
    writeIORef (worldManagerRef env) (WorldManager [(pid, ws)] [pid])
    writeIORef (cameraRef env) defaultCamera
    writeIORef (windowSizeRef env) (800, 600)
    writeIORef (framebufferSizeRef env) (800, 600)

lookupSlot ∷ TextureHandle → Int
lookupSlot _ = 0

-- | Slice out the full 'hud.onMouseDown' function body — from its
--   definition up to (not including) the next top-level function
--   definition — so the Lua-source contract test below can't be
--   confused by the unrelated "zoomed_out" branch inside hud.update
--   (the continuous per-tick hover push, which legitimately still
--   calls setZoomCursorHover and must not be flagged).
onMouseDownBody ∷ Text → Text
onMouseDownBody src =
    let marker = "function hud.onMouseDown"
        (_, afterStart) = T.breakOn marker src
        body = T.drop (T.length marker) afterStart
        (fnBody, _) = T.breakOn "\nfunction " body
    in fnBody

-- | Further narrow to just the "zoomed_out" left/right-click branch
--   inside onMouseDown, excluding the function's leading gameplay-gate
--   doc comment (which mentions "world.setZoomCursorSelect" in prose
--   while explaining an unrelated #153 concern, and would otherwise
--   false-positive the negative check below).
zoomedOutClickBranch ∷ Text → Text
zoomedOutClickBranch src =
    let startMarker = "if hud.currentView == \"zoomed_out\" then"
        endMarker   = "elseif hud.currentView == \"zoomed_in\" then"
        fnBody = onMouseDownBody src
        (_, afterStart) = T.breakOn startMarker fnBody
        body = T.drop (T.length startMarker) afterStart
        (branch, _) = T.breakOn endMarker body
    in branch

spec ∷ Spec
spec = beforeAll initEnv $ describe "zoom cursor selection (#813)" $ do

    it "commits the clicked chunk synchronously, immune to a later hover update" $ \env → do
        ws ← freshPage
        setSinglePage env pidA ws
        logger ← readIORef (loggerRef env)
        camera ← readIORef (cameraRef env)
        (winW, winH) ← readIORef (windowSizeRef env)
        (fbW, fbH)   ← readIORef (framebufferSizeRef env)

        -- Ground truth: what a click at this pixel resolves to right now.
        case pixelToChunkOrigin (camFacing camera) camera winW winH fbW fbH
                                 (wgpWorldSize testParams) 500 350 of
            Nothing → expectationFailure "test pixel unexpectedly off-map"
            Just (gx, gy) → do
                -- A pre-existing zoomed-in tile selection must be dropped
                -- atomically with the chunk commit (#135).
                modifyIORef' (wsCursorRef ws) $ \cs →
                    cs { worldSelectedTile = Just (1, 1, 1) }

                handleWorldSelectChunkByCoordCommand env logger pidA gx gy
                afterSelect ← readIORef (wsCursorRef ws)
                zoomSelectedPos afterSelect `shouldBe` Just (gx, gy)
                worldSelectedTile afterSelect `shouldBe` Nothing

                -- A later per-tick hover push (hud.update) must move the
                -- hover position but leave the committed selection alone.
                handleWorldSetZoomCursorHoverCommand env logger pidA 10 10
                afterHover ← readIORef (wsCursorRef ws)
                zoomCursorPos afterHover `shouldBe` Just (10, 10)
                zoomSelectedPos afterHover `shouldBe` Just (gx, gy)

    it "camera movement after the click does not retarget the accepted selection" $ \env → do
        ws ← freshPage
        setSinglePage env pidA ws
        logger ← readIORef (loggerRef env)
        camera1 ← readIORef (cameraRef env)
        (winW, winH) ← readIORef (windowSizeRef env)
        (fbW, fbH)   ← readIORef (framebufferSizeRef env)

        case pixelToChunkOrigin (camFacing camera1) camera1 winW winH fbW fbH
                                 (wgpWorldSize testParams) 500 350 of
            Nothing → expectationFailure "test pixel unexpectedly off-map"
            Just (gx, gy) → do
                handleWorldSelectChunkByCoordCommand env logger pidA gx gy

                -- Move the camera far enough that the SAME pixel would now
                -- resolve to a different chunk (proves the move is
                -- meaningful, not a no-op).
                let camera2 = camera1 { camPosition = (500.0, 500.0) }
                writeIORef (cameraRef env) camera2
                let recomputed = pixelToChunkOrigin (camFacing camera2) camera2
                                    winW winH fbW fbH (wgpWorldSize testParams) 500 350
                recomputed `shouldNotBe` Just (gx, gy)

                -- Render with the NEW camera: the committed selection must
                -- still be the ORIGINALLY clicked chunk, not re-derived.
                _ ← makeCursorQuad (camFacing camera2) camera2 winW winH fbW fbH
                        (wgpWorldSize testParams) (wsCursorRef ws) lookupSlot (-1)
                afterRender ← readIORef (wsCursorRef ws)
                zoomSelectedPos afterRender `shouldBe` Just (gx, gy)

    it "an off-map / degenerate click is a full no-op, preserving any existing selection" $ \env → do
        ws ← freshPage
        setSinglePage env pidA ws

        -- Pre-existing chunk + tile selections, sentinel values.
        writeIORef (wsCursorRef ws) $ emptyCursorState
            { zoomSelectedPos   = Just (32, 48)
            , worldSelectedTile = Just (3, 4, 5)
            }

        -- A degenerate viewport (zero-size window) always resolves off-map
        -- — what world.pickChunk returns for a minimized window.
        camera ← readIORef (cameraRef env)
        pixelToChunkOrigin (camFacing camera) camera 0 0 0 0
                           (wgpWorldSize testParams) 500 350
            `shouldBe` Nothing

        -- hud.onMouseDown's guard ("if gx and gy then selectChunk") means
        -- a nil pick never calls the select command at all — nothing to
        -- drive here, and the state must be untouched.
        untouched ← readIORef (wsCursorRef ws)
        zoomSelectedPos untouched `shouldBe` Just (32, 48)
        worldSelectedTile untouched `shouldBe` Just (3, 4, 5)

        -- The lower-level arm/render-commit path (world.setZoomCursorSelect,
        -- still exposed for any other caller) must ALSO treat an off-map
        -- armed commit as a full no-op, not clear an existing selection
        -- merely because it was armed — the bug the #813 review flagged
        -- in makeCursorQuad's old off-map handling.
        writeIORef (wsCursorRef ws) $ emptyCursorState
            { zoomSelectedPos   = Just (32, 48)
            , worldSelectedTile = Just (3, 4, 5)
            , zoomSelectNow     = True
            , zoomCursorPos     = Just (500, 350)
            }
        (winW, winH) ← readIORef (windowSizeRef env)
        (fbW, fbH)   ← readIORef (framebufferSizeRef env)
        _ ← makeCursorQuad (camFacing camera) camera 0 0 0 0
                (wgpWorldSize testParams) (wsCursorRef ws) lookupSlot (-1)
        afterOffMapArm ← readIORef (wsCursorRef ws)
        zoomSelectedPos afterOffMapArm `shouldBe` Just (32, 48)
        worldSelectedTile afterOffMapArm `shouldBe` Just (3, 4, 5)
        zoomSelectNow afterOffMapArm `shouldBe` False
        -- Sanity: with a real (non-degenerate) viewport the SAME arm DOES
        -- commit — proves the no-op above is specific to the off-map case.
        writeIORef (wsCursorRef ws) $ emptyCursorState
            { zoomSelectedPos   = Just (32, 48)
            , worldSelectedTile = Just (3, 4, 5)
            , zoomSelectNow     = True
            , zoomCursorPos     = Just (500, 350)
            }
        _ ← makeCursorQuad (camFacing camera) camera winW winH fbW fbH
                (wgpWorldSize testParams) (wsCursorRef ws) lookupSlot (-1)
        afterRealArm ← readIORef (wsCursorRef ws)
        zoomSelectedPos afterRealArm `shouldNotBe` Just (32, 48)
        worldSelectedTile afterRealArm `shouldBe` Nothing

    it "a fresh direct selection wins outright over a lingering deferred arm" $ \env → do
        ws ← freshPage
        setSinglePage env pidA ws
        logger ← readIORef (loggerRef env)
        camera ← readIORef (cameraRef env)
        (winW, winH) ← readIORef (windowSizeRef env)
        (fbW, fbH)   ← readIORef (framebufferSizeRef env)

        case pixelToChunkOrigin (camFacing camera) camera winW winH fbW fbH
                                 (wgpWorldSize testParams) 500 350 of
            Nothing → expectationFailure "test pixel unexpectedly off-map"
            Just (gx, gy) → do
                -- Simulate a stale, still-pending arm from an earlier
                -- world.setZoomCursorSelect that no render pass has
                -- resolved yet, hovering a DIFFERENT pixel than the fresh
                -- click below.
                writeIORef (wsCursorRef ws) $ emptyCursorState
                    { zoomSelectNow = True
                    , zoomCursorPos = Just (10, 10)
                    }

                handleWorldSelectChunkByCoordCommand env logger pidA gx gy
                afterSelect ← readIORef (wsCursorRef ws)
                zoomSelectedPos afterSelect `shouldBe` Just (gx, gy)
                zoomSelectNow afterSelect `shouldBe` False

                -- A subsequent render pass must NOT resolve the
                -- now-cleared arm and clobber the fresh selection with
                -- whatever chunk the stale hover pixel maps to.
                _ ← makeCursorQuad (camFacing camera) camera winW winH fbW fbH
                        (wgpWorldSize testParams) (wsCursorRef ws) lookupSlot (-1)
                afterRender ← readIORef (wsCursorRef ws)
                zoomSelectedPos afterRender `shouldBe` Just (gx, gy)

    it "hud.lua's zoomed-out click branch drives the synchronous pick+select API" $ \_env → do
        src ← TIO.readFile "scripts/hud.lua"
        let branch = zoomedOutClickBranch src
        -- Pins the actual click-handling source: a regression back to
        -- arming setZoomCursorHover/setZoomCursorSelect (the shared,
        -- deferred-hover design this issue fixes) would fail here even
        -- though the command-handler tests above never touch hud.lua.
        branch `shouldSatisfy` T.isInfixOf "world.pickChunk"
        branch `shouldSatisfy` T.isInfixOf "world.selectChunk"
        branch `shouldSatisfy` (not ∘ T.isInfixOf "world.setZoomCursorSelect(")

    it "world hide teardown clears an already-committed chunk selection" $ \env → do
        ws ← freshPage
        setSinglePage env pidA ws
        logger ← readIORef (loggerRef env)
        camera ← readIORef (cameraRef env)
        (winW, winH) ← readIORef (windowSizeRef env)
        (fbW, fbH)   ← readIORef (framebufferSizeRef env)

        case pixelToChunkOrigin (camFacing camera) camera winW winH fbW fbH
                                 (wgpWorldSize testParams) 500 350 of
            Nothing → expectationFailure "test pixel unexpectedly off-map"
            Just (gx, gy) → do
                handleWorldSelectChunkByCoordCommand env logger pidA gx gy
                committed ← readIORef (wsCursorRef ws)
                zoomSelectedPos committed `shouldBe` Just (gx, gy)

                handleWorldHideCommand env logger pidA
                afterHide ← readIORef (wsCursorRef ws)
                zoomSelectedPos afterHide `shouldBe` Nothing
                zoomSelectNow afterHide `shouldBe` False
                worldSelectedTile afterHide `shouldBe` Nothing
                worldSelectNow afterHide `shouldBe` False

    it "keeps chunk selection page-scoped — a select for one page never touches another" $ \env → do
        wsA ← freshPage
        wsB ← freshPage
        writeIORef (wsCursorRef wsB) $ emptyCursorState
            { zoomSelectedPos   = Just (9, 9)
            , worldSelectedTile = Just (7, 7, 7)
            }
        writeIORef (worldManagerRef env)
            (WorldManager [(pidA, wsA), (pidB, wsB)] [pidA, pidB])
        writeIORef (cameraRef env) defaultCamera
        writeIORef (windowSizeRef env) (800, 600)
        writeIORef (framebufferSizeRef env) (800, 600)
        logger ← readIORef (loggerRef env)
        camera ← readIORef (cameraRef env)

        case pixelToChunkOrigin (camFacing camera) camera 800 600 800 600
                                 (wgpWorldSize testParams) 500 350 of
            Nothing → expectationFailure "test pixel unexpectedly off-map"
            Just (gx, gy) → do
                handleWorldSelectChunkByCoordCommand env logger pidA gx gy

                afterA ← readIORef (wsCursorRef wsA)
                zoomSelectedPos afterA `shouldBe` Just (gx, gy)
                worldSelectedTile afterA `shouldBe` Nothing

                -- Page B's cursor state must be completely untouched.
                afterB ← readIORef (wsCursorRef wsB)
                zoomSelectedPos afterB `shouldBe` Just (9, 9)
                worldSelectedTile afterB `shouldBe` Just (7, 7, 7)

                -- A select for a nonexistent page is a no-op, not a crash.
                handleWorldSelectChunkByCoordCommand env logger
                    (WorldPageId "select_chunk_test_missing") gx gy
                afterMissing ← readIORef (wsCursorRef wsA)
                zoomSelectedPos afterMissing `shouldBe` Just (gx, gy)
                worldSelectedTile afterMissing `shouldBe` Nothing
