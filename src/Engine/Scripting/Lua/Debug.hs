{-# LANGUAGE ForeignFunctionInterface, UnicodeSyntax #-}
module Engine.Scripting.Lua.Debug
    ( getSourceInfo
    , SourceInfo(..)
    ) where

import UPrelude
import Foreign.C.Types (CInt(..))
import Foreign.C.String (CString)
import qualified HsLua.Core as Lua
import HsLua.Core.Types (LuaE, State(..))

-- | Source information from Lua debug API
data SourceInfo = SourceInfo
    { siSource      ∷ String
    , siCurrentLine ∷ Int
    } deriving (Show, Eq)

foreign import ccall unsafe "get_lua_caller_info"
    c_get_lua_caller_info ∷ Ptr () → CInt → CString → CInt → Ptr CInt → IO CInt

-- | Size of the caller-supplied buffer for the short_src string
-- (lua_Debug.short_src is char[LUA_IDSIZE], LUA_IDSIZE is typically 60).
sourceBufSize ∷ Int
sourceBufSize = 256

-- | Get source info for a given stack level
-- Level 0 = current function, 1 = caller, 2 = caller's caller, etc.
getSourceInfo ∷ Int → LuaE e (Maybe SourceInfo)
getSourceInfo level = do
    State lPtr ← Lua.state
    Lua.liftIO $ allocaArray sourceBufSize $ \sourceBuf →
        alloca $ \linePtr → do
            result ← c_get_lua_caller_info lPtr (fromIntegral level) sourceBuf (fromIntegral sourceBufSize) linePtr
            if result ≡ 0
                then return Nothing
                else do
                    source ← peekCString sourceBuf
                    line ← peek linePtr
                    return $ Just SourceInfo
                        { siSource = source
                        , siCurrentLine = fromIntegral line
                        }
