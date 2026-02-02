module Engine.Scripting.Lua.API.Text
  ( loadFontFn
  , spawnTextFn
  , setTextFn
  , getTextFn
  , getTextWidthFn
  ) where

import UPrelude
import Math (colorToVec4)
import Engine.Scripting.Lua.Types (LuaBackendState(..), LuaToEngineMsg(..))
import Engine.Asset.Manager (updateFontState, generateFontHandle)
import Engine.Asset.Handle (FontHandle(..), AssetState(..))
import Engine.Scene.Base (ObjectId(..), LayerId(..))
import Engine.Graphics.Font.Data (FontCache(..), fcFonts)
import Engine.Graphics.Font.Util (calculateTextWidth)
import Engine.Core.State (EngineEnv(..))
import Engine.Core.Log (LogCategory(..), logWarn)
import qualified Engine.Core.Queue as Q
import qualified HsLua as Lua
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Map as Map
import Data.IORef (readIORef, writeIORef, atomicModifyIORef')
import Control.Monad.Logger (LogLevel(..), defaultLoc)
import Control.Monad.IO.Class (liftIO)

loadFontFn ∷ LuaBackendState → Lua.LuaE Lua.Exception Lua.NumResults
loadFontFn backendState = do
  path ← Lua.tostring 1
  size ← Lua.tointeger 2
  case (path, size) of
    (Just pathBS, Just sizeVal) → do
      handle ← Lua.liftIO $ do
        let pathStr = TE.decodeUtf8 pathBS
            (lteq, _) = lbsMsgQueues backendState
        pool ← readIORef (lbsAssetPool backendState)
        handle ← generateFontHandle pool
        updateFontState handle
          (AssetLoading (T.unpack pathStr) [] 0.0) pool
        writeIORef (lbsAssetPool backendState) pool
        Q.writeQueue lteq (LuaLoadFontRequest handle (T.unpack pathStr) (fromIntegral sizeVal))
        return handle
      let (FontHandle n) = handle
      Lua.pushinteger (Lua.Integer $ fromIntegral n)
    _ → Lua.pushnil
  return 1

spawnTextFn ∷ EngineEnv → LuaBackendState → Lua.LuaE Lua.Exception Lua.NumResults
spawnTextFn env backendState = do
  x ← Lua.tonumber 1
  y ← Lua.tonumber 2
  fontHandleNum ← Lua.tointeger 3
  text ← Lua.tostring 4
  color ← Lua.tostring 5
  layer ← Lua.tointeger 6
  case (x, y, fontHandleNum, color, text) of
    (Just xVal, Just yVal, Just fh, Just c, Just textBS) → do
      let layerId = LayerId $ fromIntegral $ fromMaybe 0 layer
      objId ← Lua.liftIO $ do
        objId ← atomicModifyIORef' (lbsNextObjectId backendState) 
            (\n → (n + 1, ObjectId n))
        let fontHandle = FontHandle $ fromIntegral fh
            textStr = TE.decodeUtf8 textBS
            cStr = T.unpack $ TE.decodeUtf8 c
            msg = LuaSpawnTextRequest objId (realToFrac xVal) (realToFrac yVal)
                  fontHandle textStr (colorToVec4 cStr) layerId
            lteq = luaToEngineQueue env
        Q.writeQueue lteq msg
        return objId
      let (ObjectId n) = objId
      Lua.pushinteger (Lua.Integer $ fromIntegral n)
    _ → do
      Lua.pushstring "spawnText requires 6 arguments: x, y, fontHandle, text, color, layer"
      Lua.pushnil
  return 1

setTextFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
setTextFn env = do
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
    _ → liftIO $ do
      logger ← readIORef (loggerRef env)
      logWarn logger CatLua
        "setText requires 2 arguments: objectId, text"
      return 0

getTextFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
getTextFn env = do
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

getTextWidthFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
getTextWidthFn env = do
  fontHandleNum ← Lua.tointeger 1
  text ← Lua.tostring 2
  case (fontHandleNum, text) of
      (Just fh, Just textBS) → do
          width ← Lua.liftIO $ do
              let fontHandle = FontHandle (fromIntegral fh)
                  textStr = T.unpack $ TE.decodeUtf8 textBS
              fontCache ← readIORef (fontCacheRef env)
              case Map.lookup fontHandle (fcFonts fontCache) of
                  Nothing → return 0.0
                  Just atlas → return $ calculateTextWidth atlas textStr
          Lua.pushnumber (Lua.Number width)
      _ → Lua.pushnumber (Lua.Number 0)
  return 1
