module Engine.Scripting.Lua.API.Log
  ( logInfoFn
  , logWarnFn
  , logErrorFn
  , logDebugFn
  ) where

import UPrelude
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified HsLua as Lua
import Engine.Core.Capability.Core (CoreCapability(..))
import Engine.Core.Log
import Data.IORef (readIORef)

import Engine.Scripting.Lua.Debug (getSourceInfo, SourceInfo(..))

-- | Strip the leading directory (and any "./" prefix) from a Lua chunk
--   source path so the log prefix shows just the script segment, not the
--   full on-disk path. Shared by all four log fns (info/warn/error/debug).
dropDir ∷ String → String
dropDir ('.':'/':ss) = dropDir ss
dropDir ('/':ss)     = ss
dropDir (_:ss)       = dropDir ss
dropDir _            = ""

logInfoFn ∷ CoreCapability → Lua.LuaE Lua.Exception Lua.NumResults
logInfoFn core = do
    msg ← Lua.tostring 1
    -- Level 2: 0=C function, 1=logInfoFn wrapper, 2=Lua caller
    mInfo ← getSourceInfo 2
    let (srcFile, srcLine) = case mInfo of
            Just info → (siSource info, siCurrentLine info)
            Nothing   → ("<unknown>", 0)
        srcFileStripped = dropDir srcFile
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
    mInfo ← getSourceInfo 2
    let (srcFile, srcLine) = case mInfo of
            Just info → (siSource info, siCurrentLine info)
            Nothing   → ("<unknown>", 0)
        srcFileStripped = dropDir srcFile
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
    mInfo ← getSourceInfo 2
    let (srcFile, srcLine) = case mInfo of
            Just info → (siSource info, siCurrentLine info)
            Nothing   → ("<unknown>", 0)
        srcFileStripped = dropDir srcFile
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
    mInfo ← getSourceInfo 2
    
    let (srcFile, srcLine) = case mInfo of
            Just info → (siSource info, siCurrentLine info)
            Nothing   → ("<unknown>", 0)
        srcFileStripped = dropDir srcFile

    case msg of
        Just msgBS → Lua.liftIO $ do
            logger ← readIORef (ccLoggerRef core)
            let msgText = TE.decodeUtf8Lenient msgBS
                fullMsg = "[" <> T.pack srcFileStripped <> ":" <> tshow srcLine <> "] " <> msgText
            logThreadDebug logger CatLua fullMsg
        Nothing → pure ()
    
    return 0
