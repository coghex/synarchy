-- | The missing-page branch of the six cursor-texture commands
--   (#1366).
--
--   @scripts\/hud.lua@'s @createUI()@ submits six texture commands
--   against @hud.worldId@ every time it runs, and
--   "World.Thread.Command.Cursor.Select" resolves each one through
--   @wmWorlds@. Landing on a page that is not there is a real
--   diagnostic — the texture is dropped, so that cursor renders
--   untextured for the rest of the session — and the handler is right
--   to warn about it.
--
--   Three HUD-booting fixtures were reaching that branch 942 times per
--   headless run purely because they booted the HUD with no world at
--   all, which is what 'Test.Headless.Harness.installHudWorldPage'
--   repairs. This spec is why that repair could not instead be "stop
--   warning": it pins, at the level that actually matters — the emitted
--   'LogEntry' — the contract those fixtures were only exercising by
--   accident, so a later change that downgrades, retexts or drops any
--   of the six warnings fails here rather than going unnoticed.
--
--   Both directions come out of ONE table, because a handler that
--   warned on a page that IS present would satisfy a missing-page-only
--   spec while making the flood unfixable:
--
--     * absent page  → exactly one 'LevelWarn' \/ 'CatWorld' entry whose
--       message is that handler's own text followed by the page id;
--     * present page → the handle lands in that command's own
--       'CursorState' field, and nothing is logged at all.
--
--   No world thread is started: these are the raw handlers called
--   directly, the same shape 'Test.Headless.World.SelectChunk' uses, so
--   nothing races the assertions.
module Test.Headless.World.CursorTextureDispatch (spec) where

import UPrelude
import Test.Hspec
import Data.IORef (IORef, newIORef, readIORef, writeIORef, modifyIORef')
import Engine.Asset.Handle (TextureHandle(..))
import Engine.Core.Capability.WorldSim
    (WorldSimCapability, toWorldSimCapability)
import Engine.Core.Init (EngineInitResult(..))
import Test.Headless.Harness.Log (initializeEngineHeadlessQuiet)
import Engine.Core.Log
    ( initLogger, defaultLogConfig, LogConfig(..), LogBackend(..)
    , LogCategory(..), LogLevel(..), LogEntry(..), LoggerState
    )
import Engine.Core.State (EngineEnv(..))
import World.Cursor.Types (CursorState(..))
import World.Page.Types (WorldPageId(..))
import World.State.Types
    (WorldManager(..), WorldState(..), emptyWorldState, emptyWorldManager)
import World.Thread.Command.Cursor.Select
    ( handleWorldSetZoomCursorSelectTextureCommand
    , handleWorldSetZoomCursorHoverTextureCommand
    , handleWorldSetWorldCursorSelectTextureCommand
    , handleWorldSetWorldCursorHoverTextureCommand
    , handleWorldSetWorldCursorSelectBgTextureCommand
    , handleWorldSetWorldCursorHoverBgTextureCommand
    )

-- | One cursor-texture command: how to dispatch it, the warning text it
--   owns, and the 'CursorState' field it writes on a page that exists.
--
--   The two background commands deliberately share their foreground
--   sibling's warning text — that is what production says today, and
--   restating it per command is what makes a future divergence visible.
data Cmd = Cmd
    { cmdName    ∷ String
    , cmdRun     ∷ WorldSimCapability → LoggerState → WorldPageId
                 → TextureHandle → IO ()
    , cmdWarning ∷ Text
    , cmdField   ∷ CursorState → Maybe TextureHandle
    }

-- | All six, in the order @hud.createUI()@ submits them
--   (@scripts\/hud.lua:389-398@).
commands ∷ [Cmd]
commands =
    [ Cmd "setZoomCursorSelectTexture"
          handleWorldSetZoomCursorSelectTextureCommand
          "World not found for zoom cursor texture update: "
          zoomCursorTexture
    , Cmd "setZoomCursorHoverTexture"
          handleWorldSetZoomCursorHoverTextureCommand
          "World not found for zoom cursor hover texture update: "
          zoomHoverTexture
    , Cmd "setWorldCursorSelectTexture"
          handleWorldSetWorldCursorSelectTextureCommand
          "World not found for cursor texture update: "
          worldCursorTexture
    , Cmd "setWorldCursorHoverTexture"
          handleWorldSetWorldCursorHoverTextureCommand
          "World not found for cursor hover texture update: "
          worldHoverTexture
    , Cmd "setWorldCursorSelectBgTexture"
          handleWorldSetWorldCursorSelectBgTextureCommand
          "World not found for cursor texture update: "
          worldCursorBgTexture
    , Cmd "setWorldCursorHoverBgTexture"
          handleWorldSetWorldCursorHoverBgTextureCommand
          "World not found for cursor hover texture update: "
          worldHoverBgTexture
    ]

-- | The page the scene installs, and the one it never does. The absent
--   id is spelled like the real offender so the pinned message reads
--   exactly as the line the headless log used to carry 942 times.
presentPage, absentPage ∷ WorldPageId
presentPage = WorldPageId "cursor_texture_present"
absentPage  = WorldPageId "main_world"

-- | Distinguishable from the 'Nothing' every field starts at, which is
--   all this spec asks of it.
handle ∷ TextureHandle
handle = TextureHandle 7

spec ∷ Spec
spec = beforeAll scene $ describe "cursor texture dispatch (#1366)" $ do
    it "warns once, at LevelWarn/CatWorld, naming the absent page" $
        \(wsc, _) → mapM_ (missingCase wsc) commands

    it "writes the handle and logs nothing when the page is present" $
        \(wsc, ws) → mapM_ (presentCase wsc ws) commands

-- | A capability whose manager holds 'presentPage' and nothing else.
scene ∷ IO (WorldSimCapability, WorldState)
scene = do
    EngineInitResult env ← initializeEngineHeadlessQuiet
    ws ← emptyWorldState
    writeIORef (worldManagerRef env) emptyWorldManager
        { wmWorlds = [(presentPage, ws)], wmVisible = [] }
    pure (toWorldSimCapability env, ws)

-- | Asserted as one list comparison so "exactly one entry" and its
--   level/category/text are a single expectation, tagged with the
--   command name so a failure says which of the six broke.
missingCase ∷ WorldSimCapability → Cmd → IO ()
missingCase wsc cmd = do
    (logger, entriesRef) ← callbackLogger
    cmdRun cmd wsc logger absentPage handle
    entries ← readIORef entriesRef
    let observed = [ (leLevel e, leCategory e, leMessage e) | e ← entries ]
    (cmdName cmd, observed) `shouldBe`
        ( cmdName cmd
        , [(LevelWarn, CatWorld, cmdWarning cmd <> unPageId absentPage)] )

presentCase ∷ WorldSimCapability → WorldState → Cmd → IO ()
presentCase wsc ws cmd = do
    (logger, entriesRef) ← callbackLogger
    before ← cmdField cmd ⊚ readIORef (wsCursorRef ws)
    (cmdName cmd, before) `shouldBe` (cmdName cmd, Nothing)
    cmdRun cmd wsc logger presentPage handle
    after ← cmdField cmd ⊚ readIORef (wsCursorRef ws)
    (cmdName cmd, after) `shouldBe` (cmdName cmd, Just handle)
    entries ← readIORef entriesRef
    (cmdName cmd, map leMessage entries) `shouldBe` (cmdName cmd, [])

unPageId ∷ WorldPageId → Text
unPageId (WorldPageId t) = t

-- | A logger whose backend appends every emitted 'LogEntry' to an
--   'IORef', so this spec reads what production logging DID instead of
--   printing the very warnings it exists to keep out of the log.
callbackLogger ∷ IO (LoggerState, IORef [LogEntry])
callbackLogger = do
    entriesRef ← newIORef []
    logger ← initLogger defaultLogConfig
        { lcBackend = LogToCallback (\e → modifyIORef' entriesRef (e :)) }
    pure (logger, entriesRef)
