-- | debug.captureScreenshot(path) — write a PNG of the most recently
--   rendered frame (#643). The actor channel of the UX playtest
--   harness (#641): the player agent perceives the game exclusively
--   through this verb.
--
--   The capture itself happens on the render thread (see
--   "Engine.Graphics.Vulkan.Screenshot"): this function enqueues a
--   request, blocks with a timeout for the raw pixels, then encodes
--   and writes the PNG here on the Lua thread so the render loop
--   never stalls on the encode.
--
--   Returns a table (JSON over the debug console):
--     success → { path = <written path>, width = W, height = H }
--     failure → { error = "<clear message>" }
--
--   width/height are FRAMEBUFFER pixels (the swapchain extent) — the
--   same space engine.getFramebufferSize reports. Input injection
--   (F2, #644) must target this same space; on HiDPI displays it
--   differs from window coordinates by the DPI scale.
module Engine.Scripting.Lua.API.Screenshot
  ( captureScreenshotFn
  ) where

import UPrelude
import Control.Exception (SomeException, displayException, try)
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified HsLua as Lua
import qualified Engine.Core.Queue as Q
import Engine.Core.State (EngineEnv(..))
import Engine.Core.Types (EngineConfig(..))
import Engine.Graphics.Screenshot (grabToPngBytes)
import Engine.Graphics.Types (ScreenshotGrab(..), ScreenshotRequest(..))

-- | How long the verb waits for the render thread to deliver a frame.
--   Generous: a live render loop answers within a frame or two; only a
--   stalled/absent one runs this out.
captureTimeoutMicros ∷ Int
captureTimeoutMicros = 10 * 1000 * 1000

-- | debug.captureScreenshot(path) → { path, width, height } | { error }
--   Relative paths resolve against the resource root (the engine
--   chdirs there at startup), so repo-root-relative paths work.
captureScreenshotFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
captureScreenshotFn env = do
    mPath ← Lua.tostring 1
    case mPath of
        Nothing → pushError "captureScreenshot: path argument required"
        Just pathBS
            | ecHeadless (engineConfig env) → pushError $
                "captureScreenshot: no render target — this engine is "
                <> "running --headless (GPU-less); screenshots need a "
                <> "rendering instance (windowed or --offscreen)"
            | otherwise → do
                let path = T.unpack (TE.decodeUtf8Lenient pathBS)
                result ← Lua.liftIO $ requestCapture env path
                case result of
                    Left err → pushError err
                    Right (w, h) → do
                        Lua.newtable
                        Lua.pushstring pathBS
                        Lua.setfield (-2) "path"
                        Lua.pushinteger (fromIntegral w)
                        Lua.setfield (-2) "width"
                        Lua.pushinteger (fromIntegral h)
                        Lua.setfield (-2) "height"
                        return 1

-- | Round-trip one capture: enqueue, await raw pixels, encode + write.
requestCapture ∷ EngineEnv → FilePath → IO (Either Text (Int, Int))
requestCapture env path = do
    reply ← Q.newQueue
    Q.writeQueue (screenshotRequestQueue env) (ScreenshotRequest reply)
    mReply ← Q.readQueueTimeout captureTimeoutMicros reply
    case mReply of
        Nothing → pure $ Left $
            "captureScreenshot: timed out waiting for a frame — is "
            <> "the render loop running (window not minimized)?"
        Just (Left err) → pure (Left err)
        Just (Right grab) → do
            written ← try (BSL.writeFile path (grabToPngBytes grab))
            case written of
                Left (e ∷ SomeException) → pure $ Left $ T.pack $
                    "captureScreenshot: failed to write " ⧺ path
                    ⧺ ": " ⧺ displayException e
                Right () → pure $ Right (sgWidth grab, sgHeight grab)

pushError ∷ Text → Lua.LuaE Lua.Exception Lua.NumResults
pushError msg = do
    Lua.newtable
    Lua.pushstring (TE.encodeUtf8 msg)
    Lua.setfield (-2) "error"
    return 1
