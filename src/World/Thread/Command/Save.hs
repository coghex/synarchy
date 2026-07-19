{-# LANGUAGE Strict, UnicodeSyntax #-}

-- | Save/load command handlers.
--
--     * "World.Thread.Command.Save.WriteWorld" — the save path
--       (snapshot every live page, write to disk).
--     * 'handleWorldLoadTransactionCommand' / 'handleWorldLoadPublishCommand'
--       below — the whole-session LOAD transaction (issue #763,
--       save-overhaul C2): stage the complete replacement session
--       without touching any live ref ("World.Load.Stage"), then
--       atomically publish it once authorized by the save barrier's
--       capture lock ("World.Load.Publish"). Replaces the pre-#763
--       incrementally-mutating "…LoadPage"/"…LoadWorld"/"…RestoreIds"
--       trio, which merged a save into the live session page by page.
module World.Thread.Command.Save
    ( handleWorldSaveCommand
    , handleWorldLoadTransactionCommand
    , handleWorldLoadPublishCommand
    ) where

import UPrelude
import qualified Data.Text as T
import Data.IORef (readIORef, writeIORef)
import Engine.Core.State (EngineEnv(..))
import Engine.Core.Log (logInfo, logError, logWarn, LogCategory(..), LoggerState)
import qualified Engine.Core.Queue as Q
import Engine.Scripting.Lua.Types (LuaMsg(..))
import Engine.Load.Status (advanceLoad, failLoad, finishLoad, LoadPhase(..))
import Engine.Save.Barrier
    (releaseCaptureLock, finishSave, failSave, readSaveStatus, ssRequestId)
import World.Types (SaveData)
import World.Load.Stage (stageSession, renderStageError)
import World.Load.Publish (publishStagedSession)
import World.Thread.Command.Save.WriteWorld (handleWorldSaveCommand)

-- | Stage a decoded, already content-validated save into a complete
--   replacement session, entirely off to the side of the live session
--   (requirement 6). Never touches 'worldManagerRef' or any other live
--   ref; failure leaves the current session completely unaffected and
--   still paused (requirement 15).
handleWorldLoadTransactionCommand
    ∷ EngineEnv → LoggerState → Int → SaveData → IO ()
handleWorldLoadTransactionCommand env logger requestId saveData = do
    logInfo logger CatWorld $
        "Staging load transaction #" <> tShow requestId
    result ← stageSession env logger saveData
    case result of
        Left err → do
            let msg = renderStageError err
            logWarn logger CatWorld $
                "Load transaction #" <> tShow requestId <> " staging failed: " <> msg
            failLoad (loadStatusRef env) requestId msg
        Right staged → do
            writeIORef (pendingLoadRef env) (Just (requestId, staged))
            advanceLoad (loadStatusRef env) requestId LoadStaged
            -- Hand off to the Lua thread, which drives the publish
            -- barrier (Engine.Scripting.Lua.Thread.Dispatch) — staging
            -- never engages the save barrier itself (requirement 6: the
            -- old session stays fully usable throughout).
            Q.writeQueue (luaQueue env) (LuaLoadStaged requestId)

-- | Atomically publish a previously-staged session. Issued ONLY while
--   the save barrier's capture lock is held (mirrors 'WorldSave''s
--   authorized-command handling in "World.Thread") — every other
--   state-owner thread is quiesced for the duration, so
--   'World.Load.Publish.publishStagedSession' is the sole place any
--   gameplay consumer's view of the session actually changes.
handleWorldLoadPublishCommand ∷ EngineEnv → LoggerState → Int → IO ()
handleWorldLoadPublishCommand env logger requestId = do
    pending ← readIORef (pendingLoadRef env)
    case pending of
        Just (rid, staged) | rid ≡ requestId → do
            publishStagedSession env logger staged
            writeIORef (pendingLoadRef env) Nothing
            releaseCaptureLock' env
            completeSaveTransaction env
            finishLoad (loadStatusRef env) requestId
            logInfo logger CatWorld $
                "Load transaction #" <> tShow requestId <> " published"
        _ → do
            let msg = "internal error: no staged session matches requestId #"
                        <> tShow requestId <> " at publish"
            logError logger CatWorld msg
            releaseCaptureLock' env
            failSaveTransaction env msg
            failLoad (loadStatusRef env) requestId msg

tShow ∷ Int → Text
tShow = T.pack . show

-- | #758-style: release the save barrier's capture lock, reading the
--   current transaction id off 'saveBarrierRef' itself (only one
--   transaction — save or load-publish — is ever active at a time, so
--   this is unambiguous). Mirrors
--   "World.Thread.Command.Save.WriteWorld"'s private helper of the same
--   shape.
releaseCaptureLock' ∷ EngineEnv → IO ()
releaseCaptureLock' env = do
    current ← readSaveStatus (saveBarrierRef env)
    forM_ current $ \s → releaseCaptureLock (saveBarrierRef env) (ssRequestId s)

completeSaveTransaction ∷ EngineEnv → IO ()
completeSaveTransaction env = do
    current ← readSaveStatus (saveBarrierRef env)
    forM_ current $ \s → finishSave (saveBarrierRef env) (ssRequestId s)

failSaveTransaction ∷ EngineEnv → Text → IO ()
failSaveTransaction env err = do
    current ← readSaveStatus (saveBarrierRef env)
    forM_ current $ \s → failSave (saveBarrierRef env) (ssRequestId s) err
