{-# LANGUAGE TypeApplications #-}
-- | The standalone-VM mechanics every example of the "Lua persistence
--   components" gate is built on (issue #2047), extracted unchanged
--   from 'Test.Headless.Lua.SaveModules' when that module was split
--   along its four contract owners.
--
--   Nothing here is domain-specific: it is the fresh interpreter, the
--   minimal @engine@ stub loaded into it, the two chunk runners, and
--   the line joiner. Every domain fixture lives with the owner that
--   uses it.
--
--   NB: this is deliberately NOT
--   'Test.Headless.Lua.SharedHelpers'. That module defines its own
--   @runsOk@ and @lns@ which install NO @engine@ global and assert as
--   much. The Lua this gate drives -- @scripts\/lib\/data_codec.lua@,
--   @scripts\/lib\/save_modules.lua@ and the real components
--   registered through them -- does reach @engine.logWarn@,
--   @engine.logInfo@, @engine.logDebug@ and @engine.gameTime@ outside a
--   real boot, so the runners here load 'engineStub' first. The two sets are not
--   interchangeable and must not be consolidated.
module Test.Headless.Lua.SaveModules.Support
    ( engineStub
    , runsOk
    , runsOkWithPayloads
    , lns
    ) where

import UPrelude
import Test.Hspec
import qualified HsLua as Lua
import qualified Data.ByteString as BS
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE

-- | A minimal @engine@ global -- everything these modules, and the real
--   registrations driven through them, call outside of a real engine
--   boot: @engine.logWarn@ (from @save_modules.snapshotAll@'s
--   optional-component-omitted warning and @applyEntityRows@' absent-owner
--   drop), @engine.logInfo@, @engine.logDebug@ (issue #2174 moved the
--   module lifecycle narration these registrations emit at require time
--   -- @building_spawn.init@'s among them -- from Info to Debug), and
--   @engine.gameTime@.
--
--   @gameTime@ arrived with issue #2055: @lua.unit_ai@'s @apply@ now
--   normalizes each retained row against @scripts/unit_ai_defaults.lua@,
--   and one of the three runtime defaults it supplies is
--   @actionStartedAt = engine.gameTime()@. It reads a FROZEN @NOW@ rather
--   than a real clock so a filled value is checkable by value, not merely
--   by presence.
engineStub ∷ Text
engineStub = lns
    [ "NOW = 1000.0"
    , "engine = { logWarn = function(...) end, logInfo = function(...) end,"
    , "  logDebug = function(...) end,"
    , "  gameTime = function() return NOW end }"
    ]

-- | Run one self-contained Lua chunk in a fresh interpreter (stdlib +
-- 'engineStub' loaded first). The chunk must signal failure via Lua's
-- own @assert()@/@error()@ -- a non-OK 'Lua.Status' becomes an hspec
-- 'expectationFailure' carrying the Lua error message (file:line
-- included, since chunks are loaded with a name below).
runsOk ∷ Text → Expectation
runsOk chunkText = do
    result ← Lua.run $ do
        Lua.openlibs
        _ ← Lua.dostring (TE.encodeUtf8 engineStub)
        status ← Lua.dostring (TE.encodeUtf8 chunkText)
        case status of
            Lua.OK → return Nothing
            _ → do
                err ← Lua.tostring (-1)
                return (Just (maybe "<no message>" TE.decodeUtf8Lenient err))
    case result of
        Nothing  → pure ()
        Just msg → expectationFailure (T.unpack msg)

lns ∷ [Text] → Text
lns = T.intercalate "\n"

-- | Same as 'runsOk', but first pushes each (name, bytes) pair as a
--   GLOBAL Lua string (via 'Lua.pushstring' -- a Lua string is an
--   arbitrary byte string, not required to be UTF-8, exactly like the
--   real @data_codec.lua@ wire payload this exists to inject) before
--   running the chunk. Lets a chunk reference a tracked fixture's REAL
--   on-disk bytes (e.g. @FIXTURE@) by name instead of re-synthesizing
--   the payload inline via @codec.encode@ -- proving the tracked file
--   itself, not merely this test's own encoder output, is what
--   @saveModules.prepareLoad@ accepts (issue #766, save-overhaul C4).
runsOkWithPayloads ∷ [(Text, BS.ByteString)] → Text → Expectation
runsOkWithPayloads payloads chunkText = do
    result ← Lua.run @Lua.Exception $ do
        Lua.openlibs
        _ ← Lua.dostring (TE.encodeUtf8 engineStub)
        forM_ payloads $ \(name, bytes) → do
            Lua.pushstring bytes
            Lua.setglobal (Lua.Name (TE.encodeUtf8 name))
        status ← Lua.dostring (TE.encodeUtf8 chunkText)
        case status of
            Lua.OK → return Nothing
            _ → do
                err ← Lua.tostring (-1)
                return (Just (maybe "<no message>" TE.decodeUtf8Lenient err))
    case result of
        Nothing  → pure ()
        Just msg → expectationFailure (T.unpack msg)
