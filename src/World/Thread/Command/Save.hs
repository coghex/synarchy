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
import Control.Exception (SomeException, try)
import Data.IORef (readIORef, writeIORef)
import Engine.Core.State (EngineEnv(..))
import Engine.Core.Log (logInfo, logError, logWarn, LogCategory(..), LoggerState)
import qualified Engine.Core.Queue as Q
import Engine.Scripting.Lua.Types (LuaMsg(..))
import Engine.Load.Status (advanceLoad, failLoad, LoadPhase(..))
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
--
--   'stageSession' does substantial pure work (worldgen chunk
--   regeneration, zoom-cache/atlas construction, ...) forced via
--   'Control.Exception.evaluate' — a genuinely malformed save can throw
--   a real Haskell exception there rather than returning a tidy 'Left'
--   (round 2 review). Uncaught, that exception would propagate out of
--   this handler, through 'World.Thread.handleWorldCommand'/
--   'processAuthorizedSave', into 'World.Thread'\'s own top-level
--   'Control.Exception.catch' — which treats ANY exception as fatal for
--   the WHOLE world thread ('lifecycleRef' → 'CleaningUp'), crashing
--   the still-live, still-paused OLD session over what should have
--   been an ordinary failed load. 'try' here converts it into the same
--   'failLoad' outcome as a normal staging 'Left', with the old session
--   left completely untouched either way.
handleWorldLoadTransactionCommand
    ∷ EngineEnv → LoggerState → Int → SaveData → IO ()
handleWorldLoadTransactionCommand env logger requestId saveData = do
    logInfo logger CatWorld $
        "Staging load transaction #" <> tShow requestId
    outcome ← try (stageSession env logger saveData)
    case outcome of
        Left (ex ∷ SomeException) → do
            let msg = "internal error during staging: " <> T.pack (show ex)
            logWarn logger CatWorld $
                "Load transaction #" <> tShow requestId <> " staging crashed: " <> msg
            failLoad (loadStatusRef env) requestId msg
        Right (Left err) → do
            let msg = renderStageError err
            logWarn logger CatWorld $
                "Load transaction #" <> tShow requestId <> " staging failed: " <> msg
            failLoad (loadStatusRef env) requestId msg
        Right (Right staged) → do
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
--
--   Deliberately does NOT call 'finishLoad' on success (round 2
--   review, requirement 9): the Haskell-side session is fully live the
--   instant 'publishStagedSession' returns, but the transaction is not
--   reported 'LoadPublished' to 'engine.getLoadStatus()' until the Lua
--   thread also finishes the 'LuaSaveLoaded' reconciliation broadcast
--   this queues — see 'Engine.Scripting.Lua.Thread.Dispatch', which
--   calls 'finishLoad' itself right after that broadcast completes.
--   The save barrier's own completion ('completeSaveTransaction') is
--   NOT deferred the same way: it only gates a NEW save/load request
--   via 'Engine.Save.Barrier.saveInProgress', and 'loadInProgress'
--   (still true until the deferred 'finishLoad' above runs) already
--   covers that case on its own.
handleWorldLoadPublishCommand ∷ EngineEnv → LoggerState → Int → IO ()
handleWorldLoadPublishCommand env logger requestId = do
    pending ← readIORef (pendingLoadRef env)
    case pending of
        Just (rid, staged) | rid ≡ requestId → do
            publishStagedSession env logger requestId staged
            writeIORef (pendingLoadRef env) Nothing
            releaseCaptureLock' env
            completeSaveTransaction env
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
