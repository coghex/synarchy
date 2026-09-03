-- | Lua → engine message dispatch (#558). This module is the per-frame
--   entry point ('processLuaMessages') and the dispatch table
--   ('handleLuaMessage'); the handler bodies themselves live in
--   focused submodules split by concern:
--
--   * "Engine.Scripting.Lua.Message.Video" — window/VSync/MSAA/
--     brightness/pixel-snap/texture-filter settings.
--   * "Engine.Scripting.Lua.Message.Texture" — batched GPU texture
--     file loads and SDF font loads.
--   * "Engine.Scripting.Lua.Message.Scene" — scene-graph object
--     spawn/update/destroy (text, sprites).
--   * "Engine.Scripting.Lua.Message.WorldTexture" — world-preview and
--     zoom-atlas GPU uploads from raw pixel bytes.
module Engine.Scripting.Lua.Message
  ( processLuaMessages
  , discardStaleLuaToEngineWork
    -- * Exposed for regression coverage
  , spanTextureLoads
  ) where

import UPrelude
import qualified Data.Text as T
import Engine.Core.Log (LogCategory(..))
import Engine.Core.Log.Monad (logDebugM, logDebugSM, logWarnM)
import Engine.Core.Monad
import Engine.Core.State
import Engine.Core.Types (ecHeadless)
import qualified Engine.Core.Queue as Q
import Engine.Scripting.Lua.Message.Scene ( handleSpawnText, handleSetText
                                           , handleSpawnSprite, handleSetPos
                                           , handleSetColor, handleSetSize
                                           , handleSetVisible, handleDestroy)
import Engine.Scripting.Lua.Message.Texture ( handleLoadTextureBatch
                                             , handleLoadAtlasTextureBatch
                                             , handleLoadTexture
                                             , handleLoadFont)
import Engine.Scripting.Lua.Message.Video ( handleSetResolution
                                           , handleSetWindowMode
                                           , handleSetVSync, handleSetMSAA
                                           , handleSetBrightness
                                           , handleSetPixelSnap
                                           , handleSetTextureFilter)
import Engine.Scripting.Lua.Message.WorldTexture ( handleWorldPreview
                                                   , handleZoomAtlasUpload)
import Engine.Asset.Handle (TextureHandle)
import Engine.Graphics.Vulkan.Texture.Policy (UploadSampler)
import Engine.Scripting.Lua.Types
import World.Render.BloodQuads (uploadBloodTextures, disposeQueuedBloodTextures)

processLuaMessages ∷ EngineM σ ()
processLuaMessages = do
    env ← ask
    messages ← liftIO $ Q.flushQueue (luaToEngineQueue env)

    when (not $ null messages) $
        logDebugSM CatLua "Processing Lua messages"
            [("count", tshow $ length messages)]

    process messages
    whenGraphical handleWorldPreview
    whenGraphical handleZoomAtlasUpload
    whenGraphical uploadBloodTextures
    -- NOT whenGraphical: must drain bloodDisposeQueue in every mode so a
    -- world teardown never leaks the queued handle refs (#788). Internally
    -- a no-op with no device / no queued records — the headless case.
    disposeQueuedBloodTextures
  where
    process [] = pure ()
    -- One burst carries ONE policy. A run of ordinary loads extends only
    -- while the declared 'UploadSampler' stays the same (#2075): the
    -- batch registers every slot in it with one sampler, and its
    -- within-batch same-path dedup folds a later request into an earlier
    -- request's slot, so a mixed burst would hand some request the other
    -- category's filtering. Adjacent runs of different policies are just
    -- consecutive batches, which is what the queue already was before
    -- policies existed.
    process (LuaLoadTextureRequest handle path policy : rest) = do
        let (burst, rest') = spanTextureLoads policy rest
            requests = (handle, path) : burst
        whenGraphical $ handleLoadTextureBatch policy requests
        process rest'
    -- Atlases batch among THEMSELVES, never with ordinary textures:
    -- beyond the sampler, an atlas request also carries D-2's
    -- one-image-per-animation upload contract.
    process (LuaLoadAtlasTextureRequest handle path : rest) = do
        let (burst, rest') = span isAtlasLoad rest
            requests = (handle, path) : unwrapAtlasLoads burst
        whenGraphical $ handleLoadAtlasTextureBatch requests
        process rest'
    process (msg : rest) = do
        handleLuaMessage msg
        process rest

    isAtlasLoad (LuaLoadAtlasTextureRequest _ _) = True
    isAtlasLoad _                                = False

    unwrapAtlasLoads msgs =
        [ (handle, path) | LuaLoadAtlasTextureRequest handle path ← msgs ]

-- | Peel the leading run of ordinary texture loads that all declare
--   @policy@ off a message list, unwrapped into upload requests.
--
--   The burst stops at the FIRST message that is not an ordinary texture
--   load under this exact policy — a different policy ends it just as an
--   unrelated message does. That is the whole of #2075's batching rule,
--   and it is not cosmetic: 'handleLoadTextureBatch' registers every
--   slot in one batch with one sampler, and its within-batch same-path
--   dedup folds a later request into an earlier request's slot, so a
--   burst carrying two policies would hand some request the other
--   category's filtering. Adjacent runs of different policies simply
--   become consecutive batches.
spanTextureLoads
  ∷ UploadSampler
  → [LuaToEngineMsg]
  → ([(TextureHandle, FilePath)], [LuaToEngineMsg])
spanTextureLoads policy msgs =
    ( [ (handle, path) | LuaLoadTextureRequest handle path _ ← burst ]
    , rest )
  where
    (burst, rest) = span isTextureLoadUnder msgs

    isTextureLoadUnder (LuaLoadTextureRequest _ _ p) = p ≡ policy
    isTextureLoadUnder _                             = False

-- | The load publication's CUTOVER on @luaToEngineQueue@: drop
--   everything the replaced session left queued, and nothing else
--   (#2221). Returns how many were dropped, for the caller's log.
--
--   This is a plain flush because its ONE caller,
--   'Engine.Scripting.Lua.Thread.Dispatch.commitLoadPublish', is the
--   only place where "everything currently queued" and "the replaced
--   session's work" are the same set. Three facts hold there and
--   nowhere else, which is why the decision cannot be re-derived from
--   engine state at some later moment:
--
--   * The publication is COMMITTED. @applyLuaLoad@ has already
--     succeeded, so the load can no longer abort back onto the old
--     session — which, by @docs\/persistence_contract.md@, would have
--     survived unchanged with this work still owed a run.
--   * No producer is running. @luaToEngineQueue@ is written by the Lua
--     API, which runs on the Lua thread — the very thread executing
--     this call inside @handleLoadStaged@.
--   * No NEW-session work exists yet. @WorldLoadPublish@ has not been
--     queued, so 'World.Load.Publish.publishStagedSession' has not run,
--     so the @LuaSaveLoaded@ reconciliation it queues — whose
--     @onSaveLoaded@ handlers legitimately enqueue new-session work such
--     as @LuaLoadTextureRequest@s — cannot have produced anything.
--
--   A later flush satisfies none of the last two. In particular a
--   render-thread flush keyed off any load state cannot: the world
--   thread publishes and releases on its own schedule, so by the time
--   that thread next ticks, the replacement session may already have
--   queued work the flush would destroy along with the backlog.
--
--   The consumer ('processLuaMessages', on the render\/headless owner)
--   cannot be draining concurrently either: that owner is a registered
--   'Engine.Save.Barrier.SaveOwner' of this transaction, parked since
--   its final-pass acknowledgement, and the barrier reached its
--   boundary only because that acknowledgement had already landed AFTER
--   its last drain completed.
--
--   A normal save has no generation replacement and never reaches this
--   path, so it deliberately retains its queued work.
discardStaleLuaToEngineWork ∷ EngineEnv → IO Int
discardStaleLuaToEngineWork env =
    length <$> Q.flushQueue (luaToEngineQueue env)

-- | Run a GPU-touching action unless the engine is headless: skipped when
--   'ecHeadless' is true (no device), run otherwise. Lets the single
--   'handleLuaMessage' serve every boot mode's message loop — the
--   scene-graph and pure-IORef cases always run; only GPU operations are
--   gated. (Before this, a separate 'handleLuaMessageHeadless' duplicated
--   every scene-graph case and had already drifted from this one.)
whenGraphical ∷ EngineM σ () → EngineM σ ()
whenGraphical act = do
    env ← ask
    if ecHeadless (engineConfig env) then pure () else act

handleLuaMessage ∷ LuaToEngineMsg → EngineM σ ()
handleLuaMessage msg = do
    case msg of
        LuaSetWindowMode mode → whenGraphical $ do
            logDebugM CatLua $ "Setting window mode: " <> tshow mode
            handleSetWindowMode mode

        LuaSetResolution w h → whenGraphical $ do
            logDebugSM CatLua "Setting resolution"
                [("width", tshow w)
                ,("height", tshow h)]
            handleSetResolution w h

        LuaSetVSync enabled → whenGraphical $ do
            logDebugSM CatLua "Setting VSync"
                [("enabled", if enabled then "true" else "false")]
            handleSetVSync enabled

        LuaSetMSAA msaa → whenGraphical $ do
            logDebugSM CatLua "Setting MSAA"
                [("samples", tshow msaa)]
            handleSetMSAA msaa

        LuaSetBrightness brightness → do
            logDebugSM CatLua "Setting brightness"
                [("brightness", tshow brightness)]
            handleSetBrightness brightness

        LuaSetPixelSnap enabled → do
            logDebugSM CatLua "Setting pixel snap"
                [("enabled", if enabled then "true" else "false")]
            handleSetPixelSnap enabled

        LuaSetTextureFilter tf → whenGraphical $ handleSetTextureFilter tf

        LuaLoadFontRequest handle path size → whenGraphical $ do
            logDebugSM CatLua "Loading font"
                [("path", T.pack path)
                ,("size", tshow size)
                ,("handle", tshow handle)]
            handleLoadFont handle path size

        LuaLoadTextureRequest handle path policy → whenGraphical $ do
            logDebugSM CatLua "Loading texture"
                [("path", T.pack path)
                ,("handle", tshow handle)
                ,("policy", tshow policy)]
            handleLoadTexture handle path policy

        LuaLoadAtlasTextureRequest handle path → whenGraphical $ do
            logDebugSM CatLua "Loading unit animation atlas"
                [("path", T.pack path)
                ,("handle", tshow handle)]
            handleLoadAtlasTextureBatch [(handle, path)]

        LuaSpawnTextRequest objId x y font text color layer size → do
            logDebugSM CatLua "Spawning text"
                [("objId", tshow objId)
                ,("pos", tshow x <> "," <> tshow y)
                ,("text", T.take 20 text)
                ,("layer", tshow layer)
                ,("size", tshow size)]
            handleSpawnText objId x y font text color layer size

        LuaSetTextRequest objId text → do
            logDebugSM CatLua "Setting text"
                [("objId", tshow objId)
                ,("text", T.take 20 text)]
            handleSetText objId text

        LuaSpawnSpriteRequest objId x y w h tex layer → do
            logDebugSM CatLua "Spawning sprite"
                [("objId", tshow objId)
                ,("pos", tshow x <> "," <> tshow y)
                ,("size", tshow w <> "x" <> tshow h)
                ,("layer", tshow layer)]
            handleSpawnSprite objId x y w h tex layer

        LuaSetPosRequest objId x y → do
            logDebugSM CatLua "Moving object"
                [("objId", tshow objId)
                ,("pos", tshow x <> "," <> tshow y)]
            handleSetPos objId x y

        LuaSetColorRequest objId color → do
            logDebugM CatLua $ "Setting color for object " <> tshow objId
            handleSetColor objId color

        LuaSetSizeRequest objId w h → do
            logDebugSM CatLua "Setting size"
                [("objId", tshow objId)
                ,("size", tshow w <> "x" <> tshow h)]
            handleSetSize objId w h

        LuaSetVisibleRequest objId visible → do
            logDebugSM CatLua "Setting visibility"
                [("objId", tshow objId)
                ,("visible", if visible then "true" else "false")]
            handleSetVisible objId visible

        LuaDestroyRequest objId → do
            logDebugM CatLua $ "Destroying object " <> tshow objId
            handleDestroy objId

        -- The remaining 'LuaToEngineMsg' constructors (logging, focus,
        -- sprite-scale, etc.) are consumed by the Lua thread's own
        -- 'processLuaMsg' on a different queue, never this engine queue.
        -- If one ever arrives here it's a routing bug — log it rather
        -- than crash (this case used to be a partial match).
        other →
            logWarnM CatLua $
                "handleLuaMessage: unexpected message on engine queue: "
                <> tshow other
