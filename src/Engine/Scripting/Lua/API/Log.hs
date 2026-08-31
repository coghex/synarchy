-- | The four @engine.log*@ Lua functions and the source field of the
--   @[source:line]@ prefix they share.
module Engine.Scripting.Lua.API.Log
  ( logInfoFn
  , logWarnFn
  , logErrorFn
  , logDebugFn
    -- * The prefix's source field
    -- $sourcefield
  , logSourceField
  , shortenChunkPath
  ) where

import UPrelude
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified HsLua as Lua
import Engine.Core.Capability.Core (CoreCapability(..))
import Engine.Core.Log
import Data.IORef (readIORef)

import Engine.Scripting.Lua.Debug
    (getChunkSourceInfo, ChunkSourceInfo(..), ChunkKind(..))

-- $sourcefield
--
-- Exported for the @\"Lua log source\"@ headless group, which pins the
-- transformation directly rather than through a log line.

-- | Render the source field of a Lua log line's @[source:line]@ prefix
--   from the chunk metadata 'getChunkSourceInfo' reports. Shared by all
--   four log fns (info\/warn\/error\/debug), which is what keeps their
--   prefixes identical.
--
--   Only a FILE-backed chunk names a path, and only that case is
--   shortened by 'shortenChunkPath'. Every other kind is a LABEL:
--   the debug console's verbatim entered code (Lua's @=@ convention,
--   'Engine.Scripting.Lua.Thread.Console.executeDebugLua'), the
--   @[string \"...\"]@ rendering 'HsLua.Core.loadstring' gives the in-game
--   shell, or anything else Lua invents. A label reaches the log line
--   unchanged, and a frame Lua reports nothing for is @\<unknown\>@.
--
--   Before #1960 every source went through the path shortener, which
--   erased a slashless label to @\"\"@ (so a console command logged
--   @[:1]@) and replaced a label containing a @\/@ with whatever
--   followed it (so @local a=8\/2; engine.logInfo(..)@ logged the tail of
--   the operator's own command as the source). The decision no longer
--   turns on whether a @\/@ happens to occur in the text.
logSourceField ∷ Maybe ChunkSourceInfo → String
logSourceField Nothing    = "<unknown>"
logSourceField (Just csi) = case csiKind csi of
    ChunkFile → shortenChunkPath (csiSource csi)
    _         → csiSource csi

-- | Shorten a file-backed chunk's path for display: drop any leading
--   @\".\/\"@, then drop everything up to and including the FIRST
--   remaining @\/@.
--
--   The nested segment is retained, because it is the useful part:
--   @\".\/scripts\/unit_ai.lua\"@ becomes @\"unit_ai.lua\"@ while
--   @\".\/scripts\/ui\/panel.lua\"@ becomes @\"ui\/panel.lua\"@. A path with
--   no directory to drop is returned unchanged (@\"foo.lua\"@), and an
--   absolute path loses only its root (@\"\/abs\/foo.lua\"@ becomes
--   @\"abs\/foo.lua\"@). Never applied to a chunk that is not a file:
--   see 'logSourceField'.
shortenChunkPath ∷ String → String
shortenChunkPath src = case break (≡ '/') (dropCurDir src) of
    (_, _:rest) → rest
    (whole, _)  → whole
  where
    dropCurDir ('.':'/':ss) = dropCurDir ss
    dropCurDir ss           = ss

logInfoFn ∷ CoreCapability → Lua.LuaE Lua.Exception Lua.NumResults
logInfoFn core = do
    msg ← Lua.tostring 1
    -- Level 2: 0=C function, 1=logInfoFn wrapper, 2=Lua caller
    mInfo ← getChunkSourceInfo 2
    let srcFileStripped = logSourceField mInfo
        srcLine = maybe 0 csiCurrentLine mInfo
    case msg of
        Just msgBS → Lua.liftIO $ do
            logger ← readIORef (ccLoggerRef core)
            let msgText = TE.decodeUtf8Lenient msgBS
                fullMsg = "[" <> T.pack srcFileStripped <> ":"
                              <> tshow srcLine <> "] " <> msgText
            logThreadInfo logger CatLua fullMsg
        Nothing → pure ()
    return 0

logWarnFn ∷ CoreCapability → Lua.LuaE Lua.Exception Lua.NumResults
logWarnFn core = do
    msg ← Lua.tostring 1
    -- Level 2: 0=C function, 1=logWarnFn wrapper, 2=Lua caller
    mInfo ← getChunkSourceInfo 2
    let srcFileStripped = logSourceField mInfo
        srcLine = maybe 0 csiCurrentLine mInfo
    case msg of
        Just msgBS → Lua.liftIO $ do
            logger ← readIORef (ccLoggerRef core)
            let msgText = TE.decodeUtf8Lenient msgBS
                fullMsg = "[" <> T.pack srcFileStripped <> ":"
                              <> tshow srcLine <> "] " <> msgText
            logThreadWarn logger CatLua fullMsg
        Nothing → pure ()
    return 0

logErrorFn ∷ CoreCapability → Lua.LuaE Lua.Exception Lua.NumResults
logErrorFn core = do
    msg ← Lua.tostring 1
    -- Level 2: 0=C function, 1=logErrorFn wrapper, 2=Lua caller
    mInfo ← getChunkSourceInfo 2
    let srcFileStripped = logSourceField mInfo
        srcLine = maybe 0 csiCurrentLine mInfo
    case msg of
        Just msgBS → Lua.liftIO $ do
            logger ← readIORef (ccLoggerRef core)
            let msgText = TE.decodeUtf8Lenient msgBS
                fullMsg = "[" <> T.pack srcFileStripped <> ":"
                              <> tshow srcLine <> "] " <> msgText
            logThreadError logger CatLua fullMsg
        Nothing → pure ()
    return 0

logDebugFn ∷ CoreCapability → Lua.LuaE Lua.Exception Lua.NumResults
logDebugFn core = do
    msg ← Lua.tostring 1
    
    -- Level 2: 0=C function, 1=logInfoFn wrapper, 2=Lua caller
    mInfo ← getChunkSourceInfo 2
    
    let srcFileStripped = logSourceField mInfo
        srcLine = maybe 0 csiCurrentLine mInfo

    case msg of
        Just msgBS → Lua.liftIO $ do
            logger ← readIORef (ccLoggerRef core)
            let msgText = TE.decodeUtf8Lenient msgBS
                fullMsg = "[" <> T.pack srcFileStripped <> ":" <> tshow srcLine <> "] " <> msgText
            logThreadDebug logger CatLua fullMsg
        Nothing → pure ()
    
    return 0
