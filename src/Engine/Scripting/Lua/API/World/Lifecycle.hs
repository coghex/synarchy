{-# LANGUAGE Strict, UnicodeSyntax #-}
module Engine.Scripting.Lua.API.World.Lifecycle
    ( worldInitFn
    , worldGetIdentityFn
    , worldInitArenaFn
    , worldInitArenaDoneFn
    , worldOpenArenaFn
    , worldShowFn
    , worldHideFn
    , worldGetInitProgressFn
    , worldWaitForInitFn
    , worldDestroyFn
    , worldDestroyAllFn
    ) where

import UPrelude
import qualified Data.Text as T
import qualified HsLua as Lua
import qualified Data.Text.Encoding as TE
import Data.IORef (readIORef)
import Control.Concurrent (threadDelay)
import qualified Engine.Core.Queue as Q
import Engine.Core.State (EngineEnv(..), activeWorldState)
import Engine.Core.Log (LogCategory(..), logWarn)
import Engine.Scripting.Lua.Types (LuaMsg(..))
import World.Types
import World.Generate.Config
    (minimumWorldSize, normalizePlateCount, normalizeWorldSize)
import World.Plate (defaultPlatesFor)

-- | world.init(pageId, seed, worldSizeInChunks, plateCount
--             [, displayName[, gloss]])
--   The optional trailing arguments (#707) give the page a player-facing
--   identity: a display name plus an optional English gloss. They are
--   display TEXT (spaces/punctuation welcome, no save-name rules); each
--   is trimmed of leading/trailing whitespace and an omitted, nil, or
--   whitespace-only display name creates an unnamed page (discarding any
--   gloss). Read it back with world.getIdentity(pageId).
worldInitFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
worldInitFn env = do
    pageIdArg ← Lua.tostring 1
    seedArg   ← Lua.tointeger 2
    sizeArg   ← Lua.tointeger 3
    platesArg ← Lua.tointeger 4
    nameArg   ← Lua.tostring 5
    glossArg  ← Lua.tostring 6

    case pageIdArg of
        Just pageIdBS → Lua.liftIO $ do
            let pageId = WorldPageId (TE.decodeUtf8 pageIdBS)
                seed   = maybe 42 fromIntegral seedArg
                identity = mkWorldIdentity (TE.decodeUtf8 ⊚ nameArg)
                                           (TE.decodeUtf8 ⊚ glossArg)
                rawSize = maybe 64 fromIntegral sizeArg
                size = normalizeWorldSize rawSize
                -- Plate count scales with worldSize when caller
                -- doesn't supply one — fixes the "10 plates for any
                -- world" issue (audit #17). Explicit user values
                -- still honored after minimum-count normalization.
                rawPlates = maybe (defaultPlatesFor size) fromIntegral platesArg
                plates = normalizePlateCount rawPlates
            when (size /= rawSize ∨ plates /= rawPlates) $ do
                logger ← readIORef (loggerRef env)
                logWarn logger CatWorld $
                    "world.init normalized worldgen inputs: worldSize "
                    <> T.pack (show rawSize) <> " → "
                    <> T.pack (show size) <> ", plateCount "
                    <> T.pack (show rawPlates) <> " → "
                    <> T.pack (show plates)
                    <> " (worldSize minimum/multiple "
                    <> T.pack (show minimumWorldSize)
                    <> ", plateCount min 1)."
            Q.writeQueue (worldQueue env)
                (WorldInit pageId seed size plates identity)
        Nothing → pure ()

    return 0

-- | world.getIdentity(pageId) → { name, gloss? } | nil
--   Read-only query for a page's player-facing identity (#707). Returns
--   a table with the display name (and the gloss when one was stored)
--   for a named page; nil when the page does not exist or has no
--   identity (unnamed 4-argument world.init pages, arenas). There is
--   deliberately no setter — identity changes only by loading saved
--   state.
worldGetIdentityFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
worldGetIdentityFn env = do
    pageIdArg ← Lua.tostring 1
    mIdentity ← Lua.liftIO $ case pageIdArg of
        Just pageIdBS → do
            mgr ← readIORef (worldManagerRef env)
            case lookup (WorldPageId (TE.decodeUtf8 pageIdBS)) (wmWorlds mgr) of
                Just ws → readIORef (wsIdentityRef ws)
                Nothing → pure Nothing
        Nothing → pure Nothing
    case mIdentity of
        Just ident → do
            Lua.newtable
            Lua.pushstring (TE.encodeUtf8 (wiName ident))
            Lua.setfield (-2) "name"
            forM_ (wiGloss ident) $ \g → do
                Lua.pushstring (TE.encodeUtf8 g)
                Lua.setfield (-2) "gloss"
        Nothing → Lua.pushnil
    return 1

-- | world.initArena(pageId) — create flat test arena, no geology
worldInitArenaFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
worldInitArenaFn env = do
    pageIdArg ← Lua.tostring 1
    let pageId = case pageIdArg of
            Just bs → WorldPageId (TE.decodeUtf8 bs)
            Nothing → WorldPageId "test_arena"    -- default when called with no args
    Lua.liftIO $ Q.writeQueue (worldQueue env) (WorldInitArena pageId)
    return 0

-- | world.initArenaDone(pageId) — signal that all arena textures have been sent
worldInitArenaDoneFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
worldInitArenaDoneFn env = do
    pageIdArg ← Lua.tostring 1
    let pageId = case pageIdArg of
            Just bs → WorldPageId (TE.decodeUtf8 bs)
            Nothing → WorldPageId "test_arena"
    Lua.liftIO $ Q.writeQueue (worldQueue env) (WorldInitArenaDone pageId)
    return 0

-- | world.openArena() — convenience function that broadcasts to Lua
worldOpenArenaFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
worldOpenArenaFn env = do
    Lua.liftIO $ Q.writeQueue (luaQueue env) (LuaOpenArena)
    return 0

-- | world.show(pageId)
worldShowFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
worldShowFn env = do
    pageIdArg ← Lua.tostring 1

    case pageIdArg of
        Just pageIdBS → Lua.liftIO $ do
            let pageId = WorldPageId (TE.decodeUtf8 pageIdBS)
            Q.writeQueue (worldQueue env) (WorldShow pageId)
        Nothing → pure ()

    return 0

-- | world.hide(pageId)
worldHideFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
worldHideFn env = do
    pageIdArg ← Lua.tostring 1

    case pageIdArg of
        Just pageIdBS → Lua.liftIO $ do
            let pageId = WorldPageId (TE.decodeUtf8 pageIdBS)
            Q.writeQueue (worldQueue env) (WorldHide pageId)
        Nothing → pure ()

    return 0

-- | world.getInitProgress() → (phase, current, total, stage)
--   phase: 0=idle, 1=setup, 2=chunks, 3=done
--   current/total: numeric progress within current phase
--   stage: human-readable string ("idle", "setup", "chunks", "done")
--
--   Returns 4 values for backward compatibility: existing Lua scripts
--   use `local phase, current, total = world.getInitProgress()` and
--   the 4th value (stage) is simply ignored by those callers.
worldGetInitProgressFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
worldGetInitProgressFn env = do
    mWs ← Lua.liftIO $ activeWorldState env
    case mWs of
        Just worldState → do
            phase ← Lua.liftIO $ readIORef (wsLoadPhaseRef worldState)
            case phase of
                LoadIdle → do
                    Lua.pushinteger 0
                    Lua.pushinteger 0
                    Lua.pushinteger 0
                    Lua.pushstring "idle"
                LoadPhase1 current total → do
                    Lua.pushinteger 1
                    Lua.pushinteger (fromIntegral current)
                    Lua.pushinteger (fromIntegral total)
                    Lua.pushstring "setup"
                LoadPhase2 remaining total → do
                    Lua.pushinteger 2
                    Lua.pushinteger (fromIntegral (total - remaining))
                    Lua.pushinteger (fromIntegral total)
                    Lua.pushstring "chunks"
                LoadDone → do
                    Lua.pushinteger 3
                    Lua.pushinteger 1
                    Lua.pushinteger 1
                    Lua.pushstring "done"
            return 4
        Nothing → do
            Lua.pushinteger 0
            Lua.pushinteger 0
            Lua.pushinteger 0
            Lua.pushstring "idle"
            return 4

-- | world.waitForInit(timeout_seconds) → table (same as getInitProgress)
--   Blocks until world generation is complete or timeout is reached.
--   Default timeout: 600 seconds (10 minutes).
--   Returns the final progress table.
worldWaitForInitFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
worldWaitForInitFn env = do
    timeoutArg ← Lua.tointeger 1
    let timeoutSec = case timeoutArg of
            Just t | t > 0 → fromIntegral t ∷ Int
            _              → 600
        maxIter = timeoutSec * 4  -- poll at 250ms intervals
    Lua.liftIO $ waitLoop maxIter
    worldGetInitProgressFn env
  where
    waitLoop 0 = return ()
    waitLoop n = do
        mWs ← activeWorldState env
        case mWs of
            Just ws → do
                phase ← readIORef (wsLoadPhaseRef ws)
                case phase of
                    LoadDone → return ()
                    _        → do
                        threadDelay 250000
                        waitLoop (n - 1)
            Nothing → do
                threadDelay 250000
                waitLoop (n - 1)

-- | world.destroy(pageId)
-- Removes the world from the world manager entirely, freeing its state.
worldDestroyFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
worldDestroyFn env = do
    pageIdArg ← Lua.tostring 1

    case pageIdArg of
        Just pageIdBS → Lua.liftIO $ do
            let pageId = WorldPageId (TE.decodeUtf8 pageIdBS)
            Q.writeQueue (worldQueue env) (WorldDestroy pageId)
        Nothing → pure ()

    return 0

-- | world.destroyAll() — tear down every world (Exit to Menu). Clears
--   wmWorlds/wmVisible (so no hidden world resolves as the implicit active
--   world behind the menu), sim-deactivates each, and resets the global
--   unit/building managers. (#58)
worldDestroyAllFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
worldDestroyAllFn env = do
    Lua.liftIO $ Q.writeQueue (worldQueue env) WorldDestroyAll
    return 0
