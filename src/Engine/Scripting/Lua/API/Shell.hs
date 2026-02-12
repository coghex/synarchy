module Engine.Scripting.Lua.API.Shell
  ( shellExecuteFn
  , setupShellSandbox
  ) where

import UPrelude
import qualified HsLua as Lua
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.ByteString.Char8 as BS

-- -----------------------------------------------------------
-- Shell execution
-- -----------------------------------------------------------

shellExecuteFn ∷ Lua.LuaE Lua.Exception Lua.NumResults
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

shellExecuteInSandbox ∷ T.Text → Lua.LuaE Lua.Exception (T.Text, Bool)
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

shellTryLoadAndRun ∷ T.Text → Lua.LuaE Lua.Exception (Either T.Text T.Text)
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

-- -----------------------------------------------------------
-- Sandbox setup
-- -----------------------------------------------------------

-- | Create a sandboxed environment for shell execution
setupShellSandbox ∷ Lua.State → IO ()
setupShellSandbox lst = Lua.runWith lst $ do
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
    copyGlobalTable "UI"
    copyGlobalTable "camera"
    copyGlobalTable "world"
    
    -- Safe subset of os
    Lua.newtable
    copyFromTable "os" "time"
    copyFromTable "os" "date"
    copyFromTable "os" "clock"
    copyFromTable "os" "difftime"
    Lua.setfield (-2) (Lua.Name "os")
    Lua.setglobal (Lua.Name "shellSandbox")
  where
    copyGlobal ∷ BS.ByteString → Lua.LuaE Lua.Exception ()
    copyGlobal name = do
        _ ← Lua.getglobal (Lua.Name name)
        Lua.setfield (-2) (Lua.Name name)
    copyGlobalTable ∷ BS.ByteString → Lua.LuaE Lua.Exception ()
    copyGlobalTable name = do
        _ ← Lua.getglobal (Lua.Name name)
        Lua.setfield (-2) (Lua.Name name)
    copyFromTable ∷ BS.ByteString → BS.ByteString → Lua.LuaE Lua.Exception ()
    copyFromTable tableName funcName = do
        _ ← Lua.getglobal (Lua.Name tableName)
        _ ← Lua.getfield (-1) (Lua.Name funcName)
        Lua.remove (-2)
        Lua.setfield (-2) (Lua.Name funcName)
