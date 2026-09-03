{-# LANGUAGE Strict #-}
-- | The @engine.*@ video-configuration verbs.
--
--   Every setter here validates against the one authoritative domain
--   ('Engine.Graphics.Config.Domain', #2198) before it touches anything,
--   and reports the same way: a valid call returns @true@; a
--   type-invalid or out-of-domain call logs the rejected field and
--   value, returns @false@, and mutates NOTHING — not @videoConfigRef@,
--   not a companion mirror (@rvTextureFilterRef@, @rvPixelSnapRef@, the
--   tooltip style in @uicUiManagerRef@), and it enqueues no render
--   message. A valid per-field call keeps the live mirror and queue
--   effects it always had; the valid combined 'setVideoConfigFn' stays
--   a config-only atomic write that enqueues nothing
--   (@tools/video_window_check.py@ restores through it).
--
--   Validation only: Lua's own string→number coercion is deliberately
--   preserved, so @engine.setResolution("1920", "1080")@ is type-valid
--   and only the resulting NUMBER is domain-checked.
module Engine.Scripting.Lua.API.Config
  ( -- Config functions
    getVideoConfigFn
  , setVideoConfigFn
  , saveVideoConfigFn
  , loadDefaultConfigFn
  , setUIScaleFn
  , setFrameLimitFn
  , setResolutionFn
  , setWindowModeFn
  , setVSyncFn
  , setMSAAFn
  , setBrightnessFn
  , setPixelSnapFn
  , setTextureFilterFn
  , getTooltipDwellMsFn
  , setTooltipDwellMsFn
  , getTooltipHintDelayMsFn
  , setTooltipHintDelayMsFn
  ) where

import UPrelude
import qualified HsLua as Lua
import qualified Data.Text.Encoding as TE
import qualified Engine.Core.Queue as Q
import Data.IORef (readIORef, writeIORef, atomicModifyIORef')
import Engine.Core.State (EngineEnv, loggerRef, luaToEngineQueue)
import Engine.Core.Capability.Ui (UiCapability(..), toUiCapability)
import Engine.Core.Capability.RenderView
  (RenderViewCapability(..), toRenderViewCapability)
import Engine.Core.Log (logInfo, logWarn, LogCategory(..))
import Engine.Graphics.Config
import Engine.Graphics.Config.Domain
import Engine.Scripting.Lua.Types (LuaToEngineMsg(..))
import UI.Types (UIPageManager(..), TooltipState(..), TooltipStyle(..))

-- | Push the ten-value @getVideoConfig@ shape.
pushVideoConfig ∷ VideoConfig → Lua.LuaE Lua.Exception Lua.NumResults
pushVideoConfig config = do
    let scale = realToFrac (vcUIScale config) ∷ Double
    Lua.pushinteger (fromIntegral $ vcWidth config)
    Lua.pushinteger (fromIntegral $ vcHeight config)
    Lua.pushstring (TE.encodeUtf8 $ windowModeToText $ vcWindowMode config)
    Lua.pushnumber (Lua.Number scale)
    Lua.pushboolean (vcVSync config)
    -- 0 is the Lua spelling of an unlimited frame rate; the YAML file
    -- spells it @null@. 'setFrameLimitFn' accepts exactly this 0 back.
    Lua.pushinteger (maybe 0 fromIntegral $ vcFrameLimit config)
    Lua.pushinteger (fromIntegral $ vcMSAA config)
    Lua.pushinteger (fromIntegral $ vcBrightness config)
    Lua.pushboolean (vcPixelSnap config)
    Lua.pushstring (TE.encodeUtf8 $ textureFilterToText $ vcTextureFilter config)
    return 10

getVideoConfigFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
getVideoConfigFn env = do
    config ← Lua.liftIO $ readIORef (rvVideoConfigRef (toRenderViewCapability env))
    pushVideoConfig config

-- * Rejection plumbing

-- | The argument at @idx@ rendered for a log line, without converting
--   the slot: numbers and strings as written, @nil@ for a missing
--   argument, the type name for anything else.
argText ∷ Lua.StackIndex → Lua.LuaE Lua.Exception Text
argText idx = do
    t ← Lua.ltype idx
    case t of
        Lua.TypeNone    → pure "nil"
        Lua.TypeNil     → pure "nil"
        Lua.TypeBoolean → (\b → if b then "true" else "false") ⊚ Lua.toboolean idx
        Lua.TypeNumber  → maybe "?" TE.decodeUtf8Lenient ⊚ Lua.tostring idx
        Lua.TypeString  → maybe "?" (\s → "\"" <> TE.decodeUtf8Lenient s <> "\"")
                            ⊚ Lua.tostring idx
        _               → TE.decodeUtf8Lenient ⊚ Lua.typename t

-- | The rejection for an argument Lua could not even convert to the
--   verb's type (a non-number where a number was due, a missing
--   argument, a table): the raw argument is the reported value.
typeRejection ∷ Text → Text → Lua.StackIndex
              → Lua.LuaE Lua.Exception VideoFieldRejection
typeRejection field domain idx = do
    value ← argText idx
    pure (VideoFieldRejection field value domain)

-- | Either every rejection the call earned, or the effect to run.
type Outcome = Either [VideoFieldRejection] (IO ())

-- | The one exit for every setter: log each rejection under the verb's
--   name and return @false@ having done nothing, or run the effect and
--   return @true@.
finish ∷ EngineEnv → Text → Outcome → Lua.LuaE Lua.Exception Lua.NumResults
finish env verb outcome = do
    case outcome of
        Left rejections → do
            Lua.liftIO $ do
                logger ← readIORef (loggerRef env)
                forM_ rejections $ \r →
                    logWarn logger CatLua $
                        "engine." <> verb <> " rejected " <> describeRejection r
            Lua.pushboolean False
        Right apply → do
            Lua.liftIO apply
            Lua.pushboolean True
    return 1

modifyConfig ∷ EngineEnv → (VideoConfig → VideoConfig) → IO ()
modifyConfig env f =
    atomicModifyIORef' (rvVideoConfigRef (toRenderViewCapability env)) $ \c →
        (f c, ())

-- | Lua integer 0 is the unlimited frame rate; everything else is a
--   candidate limit for 'checkFrameLimit'.
luaFrameLimit ∷ Lua.Integer → Maybe Int
luaFrameLimit 0 = Nothing
luaFrameLimit n = Just (fromIntegral n)

-- | Resolve a window-mode argument: absent or unconvertible is a type
--   rejection, an unknown token a domain rejection. Matching is
--   case-insensitive, as it always was.
windowModeArg ∷ Lua.StackIndex
              → Lua.LuaE Lua.Exception (Either VideoFieldRejection WindowMode)
windowModeArg idx = do
    modeArg ← Lua.tostring idx
    case modeArg of
        Nothing → Left ⊚ typeRejection fieldWindowMode windowModeDomain idx
        Just bs → do
            let t = TE.decodeUtf8Lenient bs
            pure $ case windowModeFromText t of
                Just wm → Right wm
                Nothing → Left (VideoFieldRejection fieldWindowMode t
                                                    windowModeDomain)

textureFilterArg ∷ Lua.StackIndex
                 → Lua.LuaE Lua.Exception (Either VideoFieldRejection TextureFilter)
textureFilterArg idx = do
    filterArg ← Lua.tostring idx
    case filterArg of
        Nothing → Left ⊚ typeRejection fieldTextureFilter textureFilterDomain idx
        Just bs → do
            let t = TE.decodeUtf8Lenient bs
            pure $ case textureFilterFromText t of
                Just tf → Right tf
                Nothing → Left (VideoFieldRejection fieldTextureFilter t
                                                    textureFilterDomain)

-- | An integer argument judged by a leaf check.
integerArg ∷ Text → Text → (Int → Maybe VideoFieldRejection) → Lua.StackIndex
           → Lua.LuaE Lua.Exception (Either VideoFieldRejection Int)
integerArg field domain check idx = do
    arg ← Lua.tointeger idx
    case arg of
        Nothing → Left ⊚ typeRejection field domain idx
        Just n  → let v = fromIntegral n
                  in pure $ maybe (Right v) Left (check v)

-- | The UI-scale argument, narrowed to the stored 'Float' BEFORE it is
--   judged, and reported as the number Lua passed.
uiScaleArg ∷ Lua.StackIndex
           → Lua.LuaE Lua.Exception (Either VideoFieldRejection Float)
uiScaleArg idx = do
    arg ← Lua.tonumber idx
    case arg of
        Nothing → Left ⊚ typeRejection fieldUIScale uiScaleDomain idx
        Just (Lua.Number d) →
            let s = narrowUIScale d
            in pure $ case checkUIScale s of
                Nothing → Right s
                Just r  → Left r { vfrValue = tshow d }

frameLimitArg ∷ Lua.StackIndex
              → Lua.LuaE Lua.Exception (Either VideoFieldRejection (Maybe Int))
frameLimitArg idx = do
    arg ← Lua.tointeger idx
    case arg of
        Nothing → Left ⊚ typeRejection fieldFrameLimit (frameLimitDomain "0") idx
        Just n  → let fl = luaFrameLimit n
                  in pure $ maybe (Right fl) Left (checkFrameLimit "0" fl)

-- * The combined setter

-- | @engine.setVideoConfig(w, h, mode, scale, vsync, frameLimit, msaa,
--   brightness, pixelSnap, filter)@ — a config-only write of all ten
--   fields. Every domain-bearing argument is judged independently and
--   the call is atomic: one invalid argument and nothing is written, all
--   rejections are logged, and the result is @false@. A valid call
--   enqueues NO render work; that is what lets a caller restore a
--   captured config without re-driving the window.
setVideoConfigFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
setVideoConfigFn env = do
    width      ← integerArg fieldWidth dimensionDomain (checkDimension fieldWidth) 1
    height     ← integerArg fieldHeight dimensionDomain (checkDimension fieldHeight) 2
    windowMode ← windowModeArg 3
    uiScale    ← uiScaleArg 4
    vsync      ← Lua.toboolean 5
    frameLimit ← frameLimitArg 6
    msaa       ← integerArg fieldMSAA msaaDomain checkMSAA 7
    brightness ← integerArg fieldBrightness brightnessDomain checkBrightness 8
    pixelSnap  ← Lua.toboolean 9
    textureFilter ← textureFilterArg 10
    let rejections = lefts
          [ void width, void height, void windowMode, void uiScale
          , void frameLimit, void msaa, void brightness, void textureFilter ]
        outcome = case ( width, height, windowMode, uiScale
                       , frameLimit, msaa, brightness, textureFilter ) of
          (Right w, Right h, Right wm, Right s, Right fl, Right m, Right b, Right tf)
            → Right $ modifyConfig env $ \c → c
                { vcWidth = w
                , vcHeight = h
                , vcWindowMode = wm
                , vcUIScale = s
                , vcVSync = vsync
                , vcFrameLimit = fl
                , vcMSAA = m
                , vcBrightness = b
                , vcPixelSnap = pixelSnap
                , vcTextureFilter = tf
                }
          _ → Left rejections
    finish env "setVideoConfig" outcome
  where
    lefts = foldr (\e acc → either (: acc) (const acc) e) []

-- | @engine.saveVideoConfig()@ → @true@ when @config/video.local.yaml@
--   was written, @false@ when 'saveVideoConfig' refused the in-memory
--   config (every rejected field is logged there).
saveVideoConfigFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
saveVideoConfigFn env = do
    written ← Lua.liftIO $ do
        config ← readIORef (rvVideoConfigRef (toRenderViewCapability env))
        logger ← readIORef (loggerRef env)
        saveVideoConfig logger "config/video.local.yaml" config
    Lua.pushboolean written
    return 1

loadDefaultConfigFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
loadDefaultConfigFn env = do
    Lua.liftIO $ do
        logger ← readIORef (loggerRef env)
        defaultConfig ← loadVideoConfig logger "config/video_default.yaml"
        writeIORef (rvVideoConfigRef (toRenderViewCapability env)) defaultConfig
        logInfo logger CatInit "Loaded default video config"
    config ← Lua.liftIO $ readIORef (rvVideoConfigRef (toRenderViewCapability env))
    pushVideoConfig config

-- * Per-field setters

setUIScaleFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
setUIScaleFn env = do
    scale ← uiScaleArg 1
    finish env "setUIScale" $ case scale of
        Left r  → Left [r]
        Right s → Right $ modifyConfig env $ \c → c { vcUIScale = s }

-- | @engine.setFrameLimit(n)@: 0 stores an unlimited frame rate
--   (@vcFrameLimit = Nothing@); 30–240 stores that limit; a negative
--   integer, 1–29, or anything above 240 is rejected.
setFrameLimitFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
setFrameLimitFn env = do
    frameLimit ← frameLimitArg 1
    finish env "setFrameLimit" $ case frameLimit of
        Left r   → Left [r]
        Right fl → Right $ modifyConfig env $ \c → c { vcFrameLimit = fl }

setResolutionFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
setResolutionFn env = do
    width  ← integerArg fieldWidth dimensionDomain (checkDimension fieldWidth) 1
    height ← integerArg fieldHeight dimensionDomain (checkDimension fieldHeight) 2
    finish env "setResolution" $ case (width, height) of
        (Right w, Right h) → Right $ do
            modifyConfig env $ \c → c { vcWidth = w, vcHeight = h }
            Q.writeQueue (luaToEngineQueue env) (LuaSetResolution w h)
        _ → Left (either pure (const []) width ⧺ either pure (const []) height)

setWindowModeFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
setWindowModeFn env = do
    mode ← windowModeArg 1
    finish env "setWindowMode" $ case mode of
        Left r   → Left [r]
        Right wm → Right $ do
            Q.writeQueue (luaToEngineQueue env) (LuaSetWindowMode wm)
            modifyConfig env $ \c → c { vcWindowMode = wm }

-- | A Boolean has no out-of-domain member: every call is valid.
setVSyncFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
setVSyncFn env = do
    vsync ← Lua.toboolean 1
    finish env "setVSync" $ Right $ do
        Q.writeQueue (luaToEngineQueue env) (LuaSetVSync vsync)
        modifyConfig env $ \c → c { vcVSync = vsync }

-- | @engine.setMSAA(n)@, n one of 1, 2, 4, 8. A missing or non-integer
--   argument is rejected rather than read as 0.
setMSAAFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
setMSAAFn env = do
    msaa ← integerArg fieldMSAA msaaDomain checkMSAA 1
    finish env "setMSAA" $ case msaa of
        Left r  → Left [r]
        Right m → Right $ do
            Q.writeQueue (luaToEngineQueue env) (LuaSetMSAA m)
            modifyConfig env $ \c → c { vcMSAA = m }

-- | @engine.setBrightness(pct)@, 50–300. A missing or non-integer
--   argument is rejected rather than read as 100.
setBrightnessFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
setBrightnessFn env = do
    brightness ← integerArg fieldBrightness brightnessDomain checkBrightness 1
    finish env "setBrightness" $ case brightness of
        Left r  → Left [r]
        Right b → Right $ do
            Q.writeQueue (luaToEngineQueue env) (LuaSetBrightness b)
            modifyConfig env $ \c → c { vcBrightness = b }

-- | A Boolean has no out-of-domain member: every call is valid.
setPixelSnapFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
setPixelSnapFn env = do
    enabled ← Lua.toboolean 1
    finish env "setPixelSnap" $ Right $ do
        writeIORef (rvPixelSnapRef (toRenderViewCapability env)) enabled
        modifyConfig env $ \c → c { vcPixelSnap = enabled }

setTextureFilterFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
setTextureFilterFn env = do
    textureFilter ← textureFilterArg 1
    finish env "setTextureFilter" $ case textureFilter of
        Left r   → Left [r]
        Right tf → Right $ do
            writeIORef (rvTextureFilterRef (toRenderViewCapability env)) tf
            modifyConfig env $ \c → c { vcTextureFilter = tf }
            Q.writeQueue (luaToEngineQueue env) (LuaSetTextureFilter tf)

-- | engine.getTooltipDwellMs() -> integer
getTooltipDwellMsFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
getTooltipDwellMsFn env = do
    config ← Lua.liftIO $ readIORef (rvVideoConfigRef (toRenderViewCapability env))
    Lua.pushinteger (fromIntegral $ vcTooltipDwellMs config)
    return 1

-- | engine.setTooltipDwellMs(ms) — an integer from 0 to 1000; anything
--   else is rejected (never clamped or substituted). Persists into the
--   video config AND updates the live tooltip style atomically so the
--   change takes effect on the very next hover (no restart needed).
setTooltipDwellMsFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
setTooltipDwellMsFn env = do
    dwell ← integerArg fieldTooltipDwellMs tooltipMsDomain
                       (checkTooltipMs fieldTooltipDwellMs) 1
    finish env "setTooltipDwellMs" $ case dwell of
        Left r  → Left [r]
        Right n → Right $ do
            modifyConfig env $ \c → c { vcTooltipDwellMs = n }
            atomicModifyIORef' (uicUiManagerRef (toUiCapability env)) $ \mgr →
                let tts = upmTooltip mgr
                    newStyle = (ttsStyle tts) { tsDwellMs = fromIntegral n }
                in (mgr { upmTooltip = tts { ttsStyle = newStyle } }, ())

-- | engine.getTooltipHintDelayMs() -> integer
getTooltipHintDelayMsFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
getTooltipHintDelayMsFn env = do
    config ← Lua.liftIO $ readIORef (rvVideoConfigRef (toRenderViewCapability env))
    Lua.pushinteger (fromIntegral $ vcTooltipHintDelayMs config)
    return 1

-- | engine.setTooltipHintDelayMs(ms) — an integer from 0 to 1000;
--   anything else is rejected. Mirrors setTooltipDwellMs: writes both
--   the persisted video config and the live tooltip style.
setTooltipHintDelayMsFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
setTooltipHintDelayMsFn env = do
    delay ← integerArg fieldTooltipHintDelayMs tooltipMsDomain
                       (checkTooltipMs fieldTooltipHintDelayMs) 1
    finish env "setTooltipHintDelayMs" $ case delay of
        Left r  → Left [r]
        Right n → Right $ do
            modifyConfig env $ \c → c { vcTooltipHintDelayMs = n }
            atomicModifyIORef' (uicUiManagerRef (toUiCapability env)) $ \mgr →
                let tts = upmTooltip mgr
                    newStyle = (ttsStyle tts) { tsHintDelayMs = fromIntegral n }
                in (mgr { upmTooltip = tts { ttsStyle = newStyle } }, ())
