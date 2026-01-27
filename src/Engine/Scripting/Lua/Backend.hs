module Engine.Scripting.Lua.Backend
  ( LuaBackend(..)
  , createLuaBackend
  , startLuaThread
  ) where

import UPrelude
import Engine.Scripting.Backend
import Engine.Scripting.Types
import Engine.Scripting.Lua.Types
import Engine.Asset.Types
import Engine.Asset.Manager
import Engine.Asset.Handle
import Engine.Core.Thread
import Engine.Core.State
import Engine.Input.Bindings
import Engine.Input.Types
import Engine.Scene.Base
import Engine.Graphics.Vulkan.Types.Vertex
import Engine.Graphics.Camera
import Engine.Graphics.Font.Data
import Engine.Graphics.Font.Load
import Engine.Graphics.Font.Draw
import qualified Graphics.UI.GLFW as GLFW
import qualified Engine.Core.Queue as Q
import qualified HsLua as Lua
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Map as Map
import qualified Data.ByteString.Char8 as BS
import Data.Dynamic (toDyn, fromDynamic)
import Data.IORef (IORef, newIORef, readIORef, writeIORef, atomicModifyIORef')
import Data.Text.Encoding as TE
import Control.Concurrent (threadDelay, forkIO)
import Control.Concurrent.STM (atomically, modifyTVar, readTVarIO)
import Control.Concurrent.STM.TVar (newTVarIO)
import Control.Exception (SomeException, catch)
import Control.Monad (forM_, when, unless)
import Control.Monad.Logger (Loc(..), LogLevel(..), toLogStr, defaultLoc)
import Data.Time.Clock (getCurrentTime, diffUTCTime, utctDayTime)
import System.Timeout (timeout)
import UI.Focus (FocusId(..), registerFocusTarget, setFocus
                , clearFocus, fmCurrentFocus)

-- | Lua scripting backend
data LuaBackend = LuaBackend

instance ScriptBackend LuaBackend where
  initBackend _ = do
    lst <- Lua.newstate
    _ <- Lua.runWith lst Lua.openlibs
    return $ ScriptContext (toDyn lst)
  
  closeBackend _ (ScriptContext dyn) = 
    case fromDynamic dyn of
      Just lst -> Lua.close lst
      Nothing  -> error "Invalid Lua context"
  
  loadScript _ (ScriptContext dyn) path =
    case fromDynamic dyn of
      Just lst -> do
        result <- Lua.runWith lst $ Lua.dofileTrace $ Just path
        case result of
          Lua.OK -> return $ ScriptSuccess []
          _      -> do
            err <- Lua.runWith lst $ Lua.tostring (-1)
            return $ ScriptError (T.pack $ show err)
      Nothing -> return $ ScriptError "Invalid Lua context"
  
  reloadScript backend ctx path = loadScript backend ctx path
  
  callFunction _ (ScriptContext dyn) funcName args =
    case fromDynamic dyn of
      Just lst -> do
        result <- Lua.runWith lst $ callLuaFunction funcName args
        case result of
          Lua.OK -> return $ ScriptSuccess []
          _      -> return $ ScriptError "Function call failed"
      Nothing -> return $ ScriptError "Invalid Lua context"
  
  registerFunction _ _ _ _ = 
    -- TODO: Implement function registration
    return ()
  
  backendName _    = "Lua"
  backendVersion _ = "5.5"

registerLuaAPI ∷ Lua.State → EngineEnv → LuaBackendState → IO ()
registerLuaAPI lst env backendState = Lua.runWith lst $ do
  Lua.newtable 
  -- engine.logInfo
  Lua.pushHaskellFunction (logInfoFn ∷ Lua.LuaE Lua.Exception Lua.NumResults)
  Lua.setfield (-2) (Lua.Name "logInfo")
  -- engine.setTickInterval
  Lua.pushHaskellFunction (setTickIntervalFn ∷ Lua.LuaE Lua.Exception Lua.NumResults)
  Lua.setfield (-2) (Lua.Name "setTickInterval")
  -- engine.loadTexture(path)
  Lua.pushHaskellFunction (loadTextureFn ∷ Lua.LuaE Lua.Exception Lua.NumResults)
  Lua.setfield (-2) (Lua.Name "loadTexture")
  -- engine.spawnSprite(x, y, width, height, textureHandle, layer)
  Lua.pushHaskellFunction (spawnSpriteFn ∷ Lua.LuaE Lua.Exception Lua.NumResults)
  Lua.setfield (-2) (Lua.Name "spawnSprite")
  -- engine.moveSprite(objectId, x, y)
  Lua.pushHaskellFunction (moveSpriteFn ∷ Lua.LuaE Lua.Exception Lua.NumResults)
  Lua.setfield (-2) (Lua.Name "moveSprite")
  -- engine.setSpriteColor(objectId, r, g, b, a)
  Lua.pushHaskellFunction (setSpriteColorFn ∷ Lua.LuaE Lua.Exception Lua.NumResults)
  Lua.setfield (-2) (Lua.Name "setSpriteColor")
  -- engine.setVisible(objectId, visible)
  Lua.pushHaskellFunction (setVisibleFn ∷ Lua.LuaE Lua.Exception Lua.NumResults)
  Lua.setfield (-2) (Lua.Name "setVisible")
  -- engine.destroySprite(objectId)
  Lua.pushHaskellFunction (destroySpriteFn ∷ Lua.LuaE Lua.Exception Lua.NumResults)
  Lua.setfield (-2) (Lua.Name "destroySprite")
  -- engine.isKeyDown(keyName)
  Lua.pushHaskellFunction (isKeyDownFn ∷ Lua.LuaE Lua.Exception Lua.NumResults)
  Lua.setfield (-2) (Lua.Name "isKeyDown")
  -- engine.isActionDown(actionName)
  Lua.pushHaskellFunction (isActionDownFn ∷ Lua.LuaE Lua.Exception Lua.NumResults)
  Lua.setfield (-2) (Lua.Name "isActionDown")
  -- engine.getMousePosition()
  Lua.pushHaskellFunction (getMousePositionFn ∷ Lua.LuaE Lua.Exception Lua.NumResults)
  Lua.setfield (-2) (Lua.Name "getMousePosition")
  -- engine.isMouseButtonDown(button)
  Lua.pushHaskellFunction (isMouseButtonDownFn ∷ Lua.LuaE Lua.Exception Lua.NumResults)
  Lua.setfield (-2) (Lua.Name "isMouseButtonDown")
  -- engine.getWindowSize()
  Lua.pushHaskellFunction (getWindowSizeFn ∷ Lua.LuaE Lua.Exception Lua.NumResults)
  Lua.setfield (-2) (Lua.Name "getWindowSize")
  -- engine.getFramebufferSize()
  Lua.pushHaskellFunction (getFramebufferSizeFn ∷ Lua.LuaE Lua.Exception Lua.NumResults)
  Lua.setfield (-2) (Lua.Name "getFramebufferSize")
  -- engine.getWorldCoord(screenX, screenY)
  Lua.pushHaskellFunction (getWorldCoordFn ∷ Lua.LuaE Lua.Exception Lua.NumResults)
  Lua.setfield (-2) (Lua.Name "getWorldCoord")
  -- engine.loadFont(path, size)
  Lua.pushHaskellFunction (loadFontFn ∷ Lua.LuaE Lua.Exception Lua.NumResults)
  Lua.setfield (-2) (Lua.Name "loadFont")
  -- engine.spawnText(x,y,fontHandle,text,layer)
  Lua.pushHaskellFunction (spawnTextFn ∷ Lua.LuaE Lua.Exception Lua.NumResults)
  Lua.setfield (-2) (Lua.Name "spawnText")
  -- engine.setText(objectId, text)
  Lua.pushHaskellFunction (setTextFn ∷ Lua.LuaE Lua.Exception Lua.NumResults)
  Lua.setfield (-2) (Lua.Name "setText")
  -- engine.getText(objectId)
  Lua.pushHaskellFunction (getTextFn ∷ Lua.LuaE Lua.Exception Lua.NumResults)
  Lua.setfield (-2) (Lua.Name "getText")
  -- engine.shellExecute(code)
  Lua.pushHaskellFunction (shellExecuteFn ∷ Lua.LuaE Lua.Exception Lua.NumResults)
  Lua.setfield (-2) (Lua.Name "shellExecute")
  -- engine.registerFocusable(acceptsText, tabIndex) -> focusId
  Lua.pushHaskellFunction (registerFocusableFn ∷ Lua.LuaE Lua.Exception Lua.NumResults)
  Lua.setfield (-2) (Lua.Name "registerFocusable")
  -- engine.requestFocus(focusId)
  Lua.pushHaskellFunction (requestFocusFn ∷ Lua.LuaE Lua.Exception Lua.NumResults)
  Lua.setfield (-2) (Lua.Name "requestFocus")
  -- engine.releaseFocus()
  Lua.pushHaskellFunction (releaseFocusFn ∷ Lua.LuaE Lua.Exception Lua.NumResults)
  Lua.setfield (-2) (Lua.Name "releaseFocus")
  -- engine.getFocusId() -> focusId or nil
  Lua.pushHaskellFunction (getFocusIdFn ∷ Lua.LuaE Lua.Exception Lua.NumResults)
  Lua.setfield (-2) (Lua.Name "getFocusId")
  -- set global 'engine' table
  Lua.setglobal (Lua.Name "engine")
  where
    logInfoFn = do
      msg ← Lua.tostring 1
      let lf = logFunc env
      case msg of
        Just bs → Lua.liftIO $ lf defaultLoc "lua" LevelInfo (toLogStr $ TE.decodeUtf8 bs)
        Nothing → return ()
      return 0

    setTickIntervalFn = do
      interval ← Lua.tonumber 1
      case interval of
        Just (Lua.Number seconds) → Lua.liftIO $ do
          scriptPathMaybe ← Lua.runWith lst $ do
            _ ← Lua.getfield Lua.registryindex (Lua.Name "_CURRENT_SCRIPT") ∷  Lua.LuaE Lua.Exception Lua.Type
            Lua.tostring (-1)
          case scriptPathMaybe of
            Just pathBS → do
              let scriptPath = TE.decodeUtf8 pathBS
                  lf = logFunc env
              currentTime ← getCurrentTime
              let currentSecs = realToFrac $ utctDayTime currentTime
              atomically $ modifyTVar (lbsScripts backendState) $ Map.adjust
                (\s -> s { scriptTickRate = seconds
                         , scriptNextTick = currentSecs + seconds
                         }) (T.unpack scriptPath)
              lf defaultLoc "lua" LevelInfo $ toLogStr $
                "Tick interval for script " ⧺ (show scriptPath) ⧺ " set to " ⧺ (show seconds) ⧺ " seconds."
            Nothing → do
              let lf = logFunc env
              lf defaultLoc "lua" LevelError "setTickInterval called outside of script context."
        Nothing → return ()
      return 0

    loadTextureFn = do
      path ← Lua.tostring 1
      case path of
        Just pathBS → do
          (handle, pathStr) ← Lua.liftIO $ do
            let pathStr = TE.decodeUtf8 pathBS
                (lteq, _) = lbsMsgQueues backendState
            -- generate handle using shared asset pool
            pool ← readIORef (lbsAssetPool backendState)
            handle ← generateHandle @TextureHandle pool
            -- mark as loading
            updateAssetState @TextureHandle handle
              (AssetLoading (T.unpack pathStr) [] 0.0) pool
            -- write updated pool back
            writeIORef (lbsAssetPool backendState) pool
            -- send message to main thread
            Q.writeQueue lteq (LuaLoadTextureRequest handle (T.unpack pathStr))
            -- return handle to lua
            return (handle, pathStr)
          let (TextureHandle n) = handle
          Lua.pushnumber (Lua.Number (fromIntegral n))
        Nothing → Lua.pushnil
      return 1

    spawnSpriteFn = do
      -- Get arguments from Lua stack
      x ← Lua.tonumber 1
      y ← Lua.tonumber 2
      width ← Lua.tonumber 3
      height ← Lua.tonumber 4
      texHandleNum ← Lua.tointeger 5
      layer ← Lua.tointeger 6
      
      case (x, y, width, height, texHandleNum) of
        (Just xVal, Just yVal, Just wVal, Just hVal, Just texNum) -> do
          let layerId = LayerId $ fromIntegral $ fromMaybe 0 layer
          -- Generate ObjectId and send message (all in IO)
          objId <- Lua.liftIO $ do
            -- Generate unique ObjectId
            objId <- atomicModifyIORef' (lbsNextObjectId backendState) 
              (\n -> (n + 1, ObjectId n))
            
            let (lteq, _) = lbsMsgQueues backendState
                texHandle = TextureHandle (fromIntegral texNum)
                msg = LuaSpawnSpriteRequest
                  { lssObjectId      = objId
                  , lssX             = realToFrac xVal
                  , lssY             = realToFrac yVal
                  , lssWidth         = realToFrac wVal
                  , lssHeight        = realToFrac hVal
                  , lssTextureHandle = texHandle
                  , lssLayer         = layerId
                  }
            
            -- Send message to main thread
            Q.writeQueue lteq msg
            
            return objId
          
          -- Return ObjectId to Lua
          let (ObjectId n) = objId
          Lua.pushinteger (Lua.Integer $ fromIntegral n)
          
        _ -> do
          -- Invalid arguments
          Lua.liftIO $ do
            let lf = logFunc env
            lf defaultLoc "lua" LevelError 
              "spawnSprite requires 5 arguments:  x, y, width, height, textureHandle"
          Lua.pushnil
      
      return 1

    moveSpriteFn = do
      objIdNum ← Lua.tointeger 1
      x ← Lua.tonumber 2
      y ← Lua.tonumber 3
      case (objIdNum, x, y) of
        (Just idVal, Just xVal, Just yVal) → do
          Lua.liftIO $ do
            let (lteq, _) = lbsMsgQueues backendState
                msg = LuaMoveSpriteRequest (ObjectId (fromIntegral idVal))
                  (realToFrac xVal) (realToFrac yVal)
            Q.writeQueue lteq msg
          return 0
        _ → do
          Lua.liftIO $ do
            let lf = logFunc env
            lf defaultLoc "lua" LevelError 
              "moveSprite requires 3 arguments: objectId, x, y"
          return 0

    setSpriteColorFn = do
      objIdNum ← Lua.tointeger 1
      r ← Lua.tonumber 2
      g ← Lua.tonumber 3
      b ← Lua.tonumber 4
      a ← Lua.tonumber 5
      case (objIdNum, r, g, b, a) of
        (Just idVal, Just rVal, Just gVal, Just bVal, Just aVal) → do
          Lua.liftIO $ do
            let (lteq, _) = lbsMsgQueues backendState
                color     = Vec4 (realToFrac rVal) (realToFrac gVal)
                                 (realToFrac bVal) (realToFrac aVal)
                msg = LuaSetSpriteColorRequest (ObjectId (fromIntegral idVal)) color
            Q.writeQueue lteq msg
          return 0
        _ → do
          Lua.liftIO $ do
            let lf = logFunc env
            lf defaultLoc "lua" LevelError 
              "setSpriteColor requires 5 arguments: objectId, r, g, b, a"
          return 0

    setVisibleFn = do
      objIdNum ← Lua.tointeger 1
      visible ← Lua.toboolean 2
      case objIdNum of
        Just idVal → do
          Lua.liftIO $ do
            let (lteq, _) = lbsMsgQueues backendState
                msg = LuaSetVisibleRequest (ObjectId (fromIntegral idVal)) visible
            Q.writeQueue lteq msg
          return 0
        _ → do
          Lua.liftIO $ do
            let lf = logFunc env
            lf defaultLoc "lua" LevelError 
              "setVisible requires 2 arguments: objectId, visible"
          return 0

    destroySpriteFn = do
      objIdNum ← Lua.tointeger 1
      case objIdNum of
        Just idVal → do
          Lua.liftIO $ do
            let (lteq, _) = lbsMsgQueues backendState
                msg = LuaDestroySpriteRequest (ObjectId (fromIntegral idVal))
            Q.writeQueue lteq msg
          return 0
        _ → do
          Lua.liftIO $ do
            let lf = logFunc env
            lf defaultLoc "lua" LevelError 
              "destroySprite requires 1 argument: objectId"
          return 0

    isKeyDownFn = do
      keyStr ← Lua.tostring 1
      case keyStr of
        Just keyBS → do
          isDown ← Lua.liftIO $ do
            inputState ← readIORef (lbsInputState backendState)
            let keyName = TE.decodeUtf8 keyBS
            return $ checkKeyDown keyName inputState
          Lua.pushboolean isDown
        Nothing → Lua.pushboolean False
      return 1

    isActionDownFn = do
      actionStr ← Lua.tostring 1
      case actionStr of
        Just actionBS → do
          isDown ← Lua.liftIO $ do
            inputState ← readIORef (lbsInputState backendState)
            bindings ← readIORef (keyBindingsRef env)
            let actionName = TE.decodeUtf8 actionBS
            return $ isActionDown actionName bindings inputState
          Lua.pushboolean isDown
        Nothing → Lua.pushboolean False
      return 1

    getMousePositionFn = do
      (x,y) ← Lua.liftIO $ do
        inputState ← readIORef (lbsInputState backendState)
        return $ inpMousePos inputState
      Lua.pushnumber (Lua.Number x)
      Lua.pushnumber (Lua.Number y)
      return 2

    isMouseButtonDownFn = do
      buttonNum ← Lua.tointeger 1
      case buttonNum of
        Just btn → do
          isDown ← Lua.liftIO $ do
            inputState ← readIORef (lbsInputState backendState)
            let button = case btn of
                  1 → GLFW.MouseButton'1
                  2 → GLFW.MouseButton'2
                  3 → GLFW.MouseButton'3
                  _ → GLFW.MouseButton'1
            return $ Map.findWithDefault False button (inpMouseBtns inputState)
          Lua.pushboolean isDown
        Nothing → Lua.pushboolean False
      return 1

    getWindowSizeFn = do
      (w, h) ← Lua.liftIO $ do
        inputState ← readIORef (lbsInputState backendState)
        let (winW, winH) = inpWindowSize inputState
        return (fromIntegral winW, fromIntegral winH)
      Lua.pushnumber (Lua.Number w)
      Lua.pushnumber (Lua.Number h)
      return 2

    getFramebufferSizeFn = do
      (w, h) ← Lua.liftIO $ do
        inputState ← readIORef (lbsInputState backendState)
        let (winW, winH) = inpFramebufferSize inputState
        return (fromIntegral winW, fromIntegral winH)
      Lua.pushnumber (Lua.Number w)
      Lua.pushnumber (Lua.Number h)
      return 2

    getWorldCoordFn = do
      sx ← Lua.tonumber 1
      sy ← Lua.tonumber 2
      case (sx, sy) of
        (Just (Lua.Number screenX), Just (Lua.Number screenY)) → do
          -- screenX and screenY are now Double
          (worldX, worldY) ← Lua.liftIO $ do
            inputState ← readIORef (lbsInputState backendState)
            camera ← readIORef (cameraRef env)
            let (winW, winH) = inpWindowSize inputState
                (fbW, fbH) = inpFramebufferSize inputState
                -- Convert everything to Double
                aspect = fromIntegral fbW / fromIntegral fbH
                zoom = realToFrac (camZoom camera)
                viewWidth = zoom * aspect
                viewHeight = zoom
                -- Screen to normalized
                normX = screenX / fromIntegral winW
                normY = screenY / fromIntegral winH
                -- NDC to view space
                viewX = (normX * 2.0 - 1.0) * viewWidth
                viewY = (normY * 2.0 - 1.0) * viewHeight
                -- add camera position
                (camX, camY) = camPosition camera
                worldX = viewX + realToFrac camX
                worldY = viewY + realToFrac camY
            -- worldX and worldY are Double
            return (worldX, worldY)
          -- Wrap back into Lua.Number for pushnumber
          Lua.pushnumber (Lua.Number worldX)
          Lua.pushnumber (Lua.Number worldY)
        _ → do
          Lua.pushnil
          Lua.pushnil
      return 2

    loadFontFn = do
      path ← Lua.tostring 1
      size ← Lua.tointeger 2
      case (path, size) of
        (Just pathBS, Just sizeVal) → do
          handle ← Lua.liftIO $ do
            let pathStr = TE.decodeUtf8 pathBS
                (lteq, _) = lbsMsgQueues backendState
            pool ← readIORef (lbsAssetPool backendState)
            handle ← generateHandle @FontHandle pool
            updateAssetState @FontHandle handle
              (AssetLoading (T.unpack pathStr) [] 0.0) pool
            writeIORef (lbsAssetPool backendState) pool
            Q.writeQueue lteq (LuaLoadFontRequest handle (T.unpack pathStr) (fromIntegral sizeVal))
            return handle
          let (FontHandle n) = handle
          Lua.pushinteger (Lua.Integer $ fromIntegral n)
        _ → Lua.pushnil
      return 1

    spawnTextFn = do
      x ← Lua.tonumber 1
      y ← Lua.tonumber 2
      fontHandleNum ← Lua.tointeger 3
      text ← Lua.tostring 4
      layer ← Lua.tointeger 5
      case (x,y,fontHandleNum,text) of
        (Just xVal, Just yVal, Just fh, Just textBS) → do
          let layerId = LayerId $ fromIntegral $ fromMaybe 0 layer
          objId ← Lua.liftIO $ do
            objId ← atomicModifyIORef' (lbsNextObjectId backendState) 
                (\n -> (n + 1, ObjectId n))
            let fontHandle = FontHandle $ fromIntegral fh
                textStr = TE.decodeUtf8 textBS
                msg = LuaSpawnTextRequest objId (realToFrac xVal) (realToFrac yVal)
                      fontHandle textStr layerId
                lteq = luaToEngineQueue env
            Lua.liftIO $ Q.writeQueue lteq msg
            return objId
          let (ObjectId n) = objId
          Lua.pushinteger (Lua.Integer $ fromIntegral n)
        _ → do
          Lua.pushstring "drawText requires 5 arguments: objectId, x, y, fontHandle, text"
          Lua.pushnil
      return 1

    setTextFn = do
      objIdNum ← Lua.tointeger 1
      text ← Lua.tostring 2
      case (objIdNum, text) of
        (Just idVal, Just textBS) → do
          Lua.liftIO $ do
            let textStr = TE.decodeUtf8 textBS
                msg = LuaSetTextRequest (ObjectId (fromIntegral idVal)) textStr
                lteq = luaToEngineQueue env
            Q.writeQueue lteq msg
          return 0
        _ → do
          Lua.liftIO $ do
            let lf = logFunc env
            lf defaultLoc "lua" LevelError 
              "setText requires 2 arguments: objectId, text"
          return 0

    getTextFn = do
      objIdNum ← Lua.tointeger 1
      case objIdNum of
        Just idVal → do
          mText ← Lua.liftIO $ do
            buffers ← readIORef (textBuffersRef env)
            return $ Map.lookup (ObjectId (fromIntegral idVal)) buffers
          case mText of
            Just txt → Lua.pushstring (TE.encodeUtf8 txt)
            Nothing → Lua.pushnil
        _ → Lua.pushnil
      return 1

    shellExecuteFn = do
      code ← Lua.tostring 1
      case code of
        Just codeBS → do
          let codeStr = TE.decodeUtf8 codeBS
          (result, isError) ← shellExecuteInSandbox codeStr
          Lua.pushstring (TE.encodeUtf8 result)
          Lua.pushboolean isError
          return 2
        Nothing → do
          Lua.pushstring "No code provided"
          Lua.pushboolean True
          return 2

    shellExecuteInSandbox ∷ Text → Lua.LuaE Lua.Exception (Text, Bool)
    shellExecuteInSandbox code = do
        let exprCode = "return " <> code
        exprResult ← shellTryLoadAndRun exprCode
        case exprResult of
            Right val → return (val, False)
            Left _ → do
                stmtResult ← shellTryLoadAndRun code
                case stmtResult of
                    Right val → return (val, False)
                    Left err → return (err, True)

    shellTryLoadAndRun ∷ Text → Lua.LuaE Lua.Exception (Either Text Text)
    shellTryLoadAndRun src = do
        status ← Lua.loadstring (TE.encodeUtf8 src)
        case status of
            Lua.OK → do
                _ ← Lua.getglobal (Lua.Name "shellSandbox")
                _ ← Lua.setupvalue (-2) 1
                callStatus ← Lua.pcall 0 1 Nothing
                case callStatus of
                    Lua.OK → do
                        isNil ← Lua.isnil (-1)
                        if isNil
                            then do
                                Lua.pop 1
                                return $ Right "nil"
                            else do
                                result ← Lua.tostring (-1)
                                Lua.pop 1
                                return $ Right $ maybe "nil" TE.decodeUtf8 result
                    _ → do
                        err ← Lua.tostring (-1)
                        Lua.pop 1
                        return $ Left $ maybe "Unknown error" TE.decodeUtf8 err
            _ → do
                err ← Lua.tostring (-1)
                Lua.pop 1
                return $ Left $ maybe "Parse error" TE.decodeUtf8 err

    registerFocusableFn = do
      acceptsText ← Lua.toboolean 1
      tabIndex ← Lua.tointeger 2
      case tabIndex of
        Just (Lua.Integer idx) → do
          fid ← Lua.liftIO $ atomicModifyIORef' (focusManagerRef env) $ \fm →
            let (newFid, newFm) = registerFocusTarget acceptsText (fromIntegral idx) fm
            in (newFm, newFid)
          let (FocusId n) = fid
          Lua.pushinteger (Lua.Integer $ fromIntegral n)
        Nothing → do
          Lua.pushnil
      return 1

    requestFocusFn = do
      mFid ← Lua.tointeger 1
      case mFid of
        Just (Lua.Integer n) → Lua.liftIO $
          atomicModifyIORef' (focusManagerRef env) $ \fm →
            (setFocus (FocusId $ fromIntegral n) fm, ())
        Nothing → return ()
      return 0

    releaseFocusFn = do
      Lua.liftIO $ atomicModifyIORef' (focusManagerRef env) $ \fm →
          (clearFocus fm, ())
      return 0

    getFocusIdFn = do
      fm ← Lua.liftIO $ readIORef (focusManagerRef env)
      case fmCurrentFocus fm of
        Just (FocusId n) -> Lua.pushinteger (Lua.Integer $ fromIntegral n)
        Nothing          -> Lua.pushnil
      return 1

    -- | Helper to call Lua function with explicit type
callLuaFunction :: T.Text -> [ScriptValue] -> Lua.LuaE Lua.Exception Lua.Status
callLuaFunction funcName args = do
  let name = Lua.Name (TE.encodeUtf8 funcName)
  _ <- Lua.getglobal name
  -- push arguments onto the lua stack
  forM_ args $ \arg → case arg of
    ScriptNumber n → Lua.pushnumber (Lua.Number n)
    ScriptString s → Lua.pushstring (TE.encodeUtf8 s)
    ScriptBool b   → Lua.pushboolean b
    ScriptNil      → Lua.pushnil
    _              → Lua.pushnil -- unsupported types push nil for now
  let numArgs = fromIntegral (length args)
  Lua.call numArgs Lua.multret
  return Lua.OK

-- | Create a sandboxed environment for shell execution
-- This creates a global 'shellSandbox' table with only safe functions
setupShellSandbox ∷ Lua.State → IO ()
setupShellSandbox lst = Lua.runWith lst $ do
    -- Create sandbox table
    Lua.newtable
    
    -- Safe basic functions
    copyGlobal "print"
    copyGlobal "tostring"
    copyGlobal "tonumber"
    copyGlobal "type"
    copyGlobal "pairs"
    copyGlobal "ipairs"
    copyGlobal "next"
    copyGlobal "select"
    copyGlobal "pcall"
    copyGlobal "xpcall"
    copyGlobal "error"
    copyGlobal "assert"
    copyGlobal "unpack"
    
    -- Safe tables (copy entire table)
    copyGlobalTable "math"
    copyGlobalTable "string"
    copyGlobalTable "table"
    
    -- Engine API (your safe functions)
    copyGlobalTable "engine"
    
    -- Safe subset of os
    Lua.newtable
    copyFromTable "os" "time"
    copyFromTable "os" "date"
    copyFromTable "os" "clock"
    copyFromTable "os" "difftime"
    Lua.setfield (-2) (Lua.Name "os")
    
    -- Set as global 'shellSandbox'
    Lua.setglobal (Lua.Name "shellSandbox")
  where
    -- Copy a global function to the sandbox table (at stack top)
    copyGlobal ∷ BS.ByteString → Lua.LuaE Lua.Exception ()
    copyGlobal name = do
        _ ← Lua.getglobal (Lua.Name name)
        Lua.setfield (-2) (Lua.Name name)
    
    -- Copy an entire global table to the sandbox
    copyGlobalTable ∷ BS.ByteString → Lua.LuaE Lua.Exception ()
    copyGlobalTable name = do
        _ ← Lua.getglobal (Lua.Name name)
        Lua.setfield (-2) (Lua.Name name)
    
    -- Copy a specific function from a global table
    copyFromTable ∷ BS.ByteString → BS.ByteString → Lua.LuaE Lua.Exception ()
    copyFromTable tableName funcName = do
        _ ← Lua.getglobal (Lua.Name tableName)
        _ ← Lua.getfield (-1) (Lua.Name funcName)
        Lua.remove (-2)  -- Remove the table, keep the function
        Lua.setfield (-2) (Lua.Name funcName)

-- | Create Lua backend
createLuaBackend :: IO LuaBackend
createLuaBackend = return LuaBackend

-- | Start Lua thread (existing thread management)
startLuaThread ∷ EngineEnv → IO ThreadState
startLuaThread env = do
    let apRef        = assetPoolRef env
        objIdRef     = nextObjectIdRef env
        inputSRef    = inputStateRef env
    stateRef ← newIORef ThreadRunning
    threadId ← catch 
        (do
            let lf = logFunc env
            lf defaultLoc "lua" LevelInfo "Starting lua thread..."
            let lteq = luaToEngineQueue env
                etlq = luaQueue env
            backendState ← createLuaBackendState lteq etlq apRef objIdRef inputSRef
            -- register lua api
            registerLuaAPI (lbsLuaState backendState) env backendState
            lf defaultLoc "lua" LevelInfo "Lua API registered."
            setupShellSandbox (lbsLuaState backendState)
            lf defaultLoc "lua" LevelInfo "Shell sandbox set up."
            let scriptPath = "scripts/init.lua"
            Lua.runWith (lbsLuaState backendState) $ do
              Lua.pushstring (TE.encodeUtf8 $ T.pack scriptPath)
              Lua.setfield Lua.registryindex (Lua.Name "_CURRENT_SCRIPT") ∷  Lua.LuaE Lua.Exception ()
            currentTime ← getCurrentTime 
            let currentSecs = realToFrac $ utctDayTime currentTime
                defaultScript = LuaScript
                  { scriptPath     = scriptPath
                  , scriptTickRate = 1.0 -- default 1 second tick rate
                  , scriptNextTick = currentSecs + 1.0 }
            atomically $ modifyTVar (lbsScripts backendState) $
              Map.insert scriptPath defaultScript
            backend ← createLuaBackend
            let scriptCtx = ScriptContext (toDyn (lbsLuaState backendState))
            loadResult ← loadScript backend scriptCtx scriptPath
            case loadResult of
              ScriptSuccess _ → lf defaultLoc "lua" LevelInfo $ toLogStr $
                                  (T.pack $ (show scriptPath) ⧺ " loaded successfully.")
              ScriptError err → lf defaultLoc "lua" LevelError $ toLogStr $
                                  T.pack $ "Failed to load " ⧺ scriptPath ⧺ ": " ⧺ (show err)
            initResult ← callFunction backend scriptCtx "init" []
            case initResult of
              ScriptSuccess _ → lf defaultLoc "lua" LevelInfo
                                  "init() function executed successfully."
              ScriptError err → lf defaultLoc "lua" LevelError $ toLogStr $
                                  "Failed to execute init(): " ⧺ (show err)
            tid ← forkIO $ runLuaLoop env backendState stateRef
            return tid
        ) 
        (\(e :: SomeException) → do
            let lf = logFunc env
            lf defaultLoc "lua" LevelError $
                "Failed to start lua thread: " <> (toLogStr (show e))
            error "Lua thread failed to start"
        )
    return $ ThreadState stateRef threadId

-- | Create Lua backend state
createLuaBackendState ::  Q.Queue LuaToEngineMsg -> Q.Queue LuaMsg
                      -> IORef AssetPool -> IORef Word32
                      -> IORef InputState -> IO LuaBackendState
createLuaBackendState ltem etlm apRef objIdRef inputStateRef = do
  lState ← Lua.newstate
  _ ← Lua.runWith lState $ Lua.openlibs
  scriptsVar ← newTVarIO Map.empty
  return LuaBackendState
    { lbsLuaState     = lState
    , lbsScripts      = scriptsVar
    , lbsMsgQueues    = (ltem, etlm)
    , lbsAssetPool    = apRef
    , lbsNextObjectId = objIdRef
    , lbsInputState   = inputStateRef
    }

-- | Lua event loop
runLuaLoop ∷ EngineEnv → LuaBackendState → IORef ThreadControl → IO ()
runLuaLoop env ls stateRef = do
    control ← readIORef stateRef
    case control of
        ThreadStopped → do
            let lf = logFunc env
            lf defaultLoc "lua" LevelInfo "Stopping lua thread..."
            Lua.close (lbsLuaState ls)
            pure ()
        ThreadPaused → do
            threadDelay 100000
            runLuaLoop env ls stateRef
        ThreadRunning → do
            currentTime ← getCurrentTime
            let currentSecs = realToFrac $ utctDayTime currentTime
            -- 1. caclulate next script wake time
            scriptsMap ← readTVarIO (lbsScripts ls)
            let scripts = Map.elems scriptsMap
                nextWakeTimes = map scriptNextTick scripts
                nextWakeTime = if null nextWakeTimes
                               then currentSecs + 1.0
                               else minimum nextWakeTimes
                sleeptime = max 0.001 (nextWakeTime - currentSecs) -- at least 1ms
            -- wait for msg or timeout (whichever comes first)
            let maxSleepMicros = min 16666 (floor (sleeptime * 1000000))
                (_, etlq) = lbsMsgQueues ls
            mMsg ← timeout maxSleepMicros (Q.readQueue etlq)
            case mMsg of
              -- got a message
              Just msg → do
                  processLuaMsg env ls stateRef msg
                  processLuaMsgs env ls stateRef
                  runLuaLoop env ls stateRef
              -- check if scripts need to run
              Nothing → do
                  currentTime' ← getCurrentTime
                  let currentSecs' = realToFrac $ utctDayTime currentTime'
                  -- execute all scripts that are due
                  scriptsMap' ← readTVarIO (lbsScripts ls)
                  forM_ (Map.toList scriptsMap') $ \(path, script) → do
                    when (currentSecs' ≥ scriptNextTick script) $ do
                      Lua.runWith (lbsLuaState ls) $ do
                        Lua.pushstring (TE.encodeUtf8 $ T.pack path)
                        Lua.setfield Lua.registryindex (Lua.Name "_CURRENT_SCRIPT") ∷  Lua.LuaE Lua.Exception ()
                      backend ← createLuaBackend
                      let scriptCtx = ScriptContext (toDyn (lbsLuaState ls))
                          dt        = scriptTickRate script
                      _ ← callFunction backend scriptCtx "update" [ScriptNumber dt]
                      -- update next tick time (preserving overshoot)
                      atomically $ modifyTVar (lbsScripts ls) $
                        Map.adjust (\s -> s { scriptNextTick = scriptNextTick s + scriptTickRate s }) path
                  runLuaLoop env ls stateRef

-- | Process messages from anywhere to lua
processLuaMsgs ∷ EngineEnv → LuaBackendState → IORef ThreadControl → IO ()
processLuaMsgs env ls stateRef = do
    let (_, etlq) = lbsMsgQueues ls
    mMsg ← Q.tryReadQueue etlq
    case mMsg of
        Just msg → do
            currentTime ← getCurrentTime
            let lf = logFunc env
            lf defaultLoc "lua" LevelDebug $ 
                "LUA RECV " <> toLogStr (show currentTime) <> "] got msg: " <> toLogStr (show msg)
            processLuaMsg env ls stateRef msg
            processLuaMsgs env ls stateRef
        Nothing → return ()
-- | Process a single lua message
processLuaMsg ∷ EngineEnv → LuaBackendState → IORef ThreadControl → LuaMsg → IO ()
processLuaMsg env ls stateRef msg = case msg of
  LuaTextureLoaded handle assetId → do
    -- TODO: Handle texture loaded callback
    let lf = logFunc env
    lf defaultLoc "lua" LevelInfo $ 
        "Texture loaded:  " <> toLogStr (show handle) <> " -> " <> toLogStr (show assetId)
    return ()
  LuaFontLoaded handle → do
    let lf = logFunc env
    lf defaultLoc "lua" LevelInfo $ 
        "Font loaded:  " <> toLogStr (show handle)
    return ()
  LuaFontLoadFailed err → do
    let lf = logFunc env
    lf defaultLoc "lua" LevelError $ 
        "Font load failed:  " <> toLogStr (show err)
    return ()
  LuaThreadKill → writeIORef stateRef ThreadStopped
  LuaMouseDownEvent button x y → do
    let buttonNum = case button of
          GLFW.MouseButton'1 → 1
          GLFW.MouseButton'2 → 2
          GLFW.MouseButton'3 → 3
          _                  → 0
    tryCallLuaHandler ls "onMouseDown"
      [ ScriptNumber (fromIntegral buttonNum)
      , ScriptNumber x
      , ScriptNumber y ]
  LuaMouseUpEvent button x y → do
    let buttonNum = case button of
          GLFW.MouseButton'1 → 1
          GLFW.MouseButton'2 → 2
          GLFW.MouseButton'3 → 3
          _                  → 0
    tryCallLuaHandler ls "onMouseUp"
      [ ScriptNumber (fromIntegral buttonNum)
      , ScriptNumber x
      , ScriptNumber y ]
  LuaKeyDownEvent key → do
    let keyName = keyToText key
    tryCallLuaHandler ls "onKeyDown" [ScriptString keyName]
  LuaKeyUpEvent key → do
    let keyName = keyToText key
    tryCallLuaHandler ls "onKeyUp" [ScriptString keyName]
  LuaShellToggle → do
    tryCallLuaHandler ls "onShellToggle" []
  LuaCharInput fid c → do
    tryCallLuaHandler ls "onCharInput"
      [ ScriptNumber (fromIntegral fid)
      , ScriptString (T.singleton c) ]
  LuaTextBackspace fid → do
    tryCallLuaHandler ls "onTextBackspace"
      [ ScriptNumber (fromIntegral fid) ]
  LuaTextSubmit fid → do
    tryCallLuaHandler ls "onTextSubmit"
      [ ScriptNumber (fromIntegral fid) ]
  LuaFocusLost fid → do
    tryCallLuaHandler ls "onFocusLost"
      [ ScriptNumber (fromIntegral fid) ]
  _ → return () -- other messages can be handled here

-- | helper function to call lua event handlers safely
tryCallLuaHandler ∷ LuaBackendState → T.Text → [ScriptValue] → IO ()
tryCallLuaHandler ls funcName args = do
  -- check if handler exists
  exists ← Lua.runWith (lbsLuaState ls) $ do
    _ ← Lua.getglobal (Lua.Name $ TE.encodeUtf8 funcName) ∷  Lua.LuaE Lua.Exception Lua.Type
    isFunc ← Lua.isfunction (-1)
    Lua.pop 1
    return isFunc
  when exists $ do
    backend ← createLuaBackend
    let scriptCtx = ScriptContext (toDyn (lbsLuaState ls))
    result ← callFunction backend scriptCtx funcName args
    case result of
      ScriptSuccess _ → return ()
      ScriptError err → do
        putStrLn $ "Error calling Lua handler " ⧺ (T.unpack funcName) ⧺ ": " ⧺ (show err)
