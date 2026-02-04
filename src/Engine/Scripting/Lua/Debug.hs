{-# LANGUAGE ForeignFunctionInterface #-}
module Engine.Scripting.Lua.Debug
    ( getSourceInfo
    , SourceInfo(..)
    ) where

import UPrelude
import Foreign.C.Types (CInt(..))
import Foreign.C.String (CString, peekCString)
import Foreign.Ptr (Ptr)
import Foreign.Marshal.Alloc (alloca)
import Foreign.Storable (peek)
import qualified HsLua.Core as Lua
import HsLua.Core.Types (LuaE, State(..))

-- | Source information from Lua debug API
data SourceInfo = SourceInfo
    { siSource      :: String
    , siCurrentLine :: Int
    } deriving (Show, Eq)

foreign import ccall unsafe "get_lua_caller_info"
    c_get_lua_caller_info :: Ptr () -> CInt -> Ptr CString -> Ptr CInt -> IO CInt

-- | Get source info for a given stack level
-- Level 0 = current function, 1 = caller, 2 = caller's caller, etc.
getSourceInfo :: Int -> LuaE e (Maybe SourceInfo)
getSourceInfo level = do
    State lPtr <- Lua.state
    Lua.liftIO $ alloca $ \sourcePtr ->
        alloca $ \linePtr -> do
            result <- c_get_lua_caller_info lPtr (fromIntegral level) sourcePtr linePtr
            if result == 0
                then return Nothing
                else do
                    sourceCStr <- peek sourcePtr
                    source <- peekCString sourceCStr
                    line <- peek linePtr
                    return $ Just SourceInfo
                        { siSource = source
                        , siCurrentLine = fromIntegral line
                        }
