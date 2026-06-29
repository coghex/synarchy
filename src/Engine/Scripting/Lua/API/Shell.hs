module Engine.Scripting.Lua.API.Shell
  ( shellExecuteFn
  , setupShellSandbox
  , luaValueToText
  ) where

import UPrelude
import qualified HsLua as Lua
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Read as T
import qualified Data.ByteString.Char8 as BS
import Data.List (sortBy, sort)
import Numeric (showHex)

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
                            -- Keep "nil" (not luaValueToText's "null"):
                            -- shell.lua's result post-processing keys
                            -- on the literal string "nil" for its
                            -- OK / undefined-identifier display.
                            return $ Right "nil"
                        else do
                            -- Same serializer as the TCP debug console:
                            -- tables render as JSON instead of
                            -- Lua.tostring's Nothing → "nil" → "OK"
                            -- (which silently ate every table-returning
                            -- call in the in-game shell).
                            txt ← luaValueToText 0 (-1)
                            Lua.pop 1
                            return $ Right txt
                _ → do
                    err ← Lua.tostring (-1)
                    Lua.pop 1
                    return $ Left $ maybe "Unknown error" TE.decodeUtf8 err
        _ → do
            err ← Lua.tostring (-1)
            Lua.pop 1
            return $ Left $ maybe "Parse error" TE.decodeUtf8 err

-- | Create a sandboxed environment for shell execution
-- This creates a global 'shellSandbox' table with only safe functions
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
    
    -- Engine API tables. Keep in sync with the setglobal list in
    -- Engine.Scripting.Lua.API — a table missing here is invisible
    -- from the in-game shell ("attempt to index a nil value") even
    -- though scripts and the TCP debug console (no sandbox) see it.
    copyGlobalTable "engine"
    copyGlobalTable "UI"
    copyGlobalTable "camera"
    copyGlobalTable "world"
    copyGlobalTable "unit"
    copyGlobalTable "building"
    copyGlobalTable "equipment"
    copyGlobalTable "substance"
    copyGlobalTable "item"
    copyGlobalTable "flora"
    copyGlobalTable "combat"
    
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
    
    -- Shallow-copy the table so shell assignments (string.format = nil)
    -- can't mutate the real global and break every loaded script. The
    -- values (functions) are still shared; only the table is fresh.
    copyGlobalTable ∷ BS.ByteString → Lua.LuaE Lua.Exception ()
    copyGlobalTable name = do
        _ ← Lua.getglobal (Lua.Name name)        -- sandbox src
        isTab ← Lua.istable (-1)
        if not isTab
            then Lua.pop 1
            else do
                Lua.newtable                     -- sandbox src dst
                Lua.pushnil                      -- sandbox src dst nil
                copyPairs
                Lua.remove (-2)                  -- sandbox dst
                Lua.setfield (-2) (Lua.Name name)
      where
        copyPairs = do
            more ← Lua.next (-3)                 -- … src dst key value
            when more $ do
                Lua.pushvalue (-2)               -- … src dst key value key
                Lua.insert (-2)                  -- … src dst key key value
                Lua.rawset (-4)                  -- … src dst key
                copyPairs
    
    copyFromTable ∷ BS.ByteString → BS.ByteString → Lua.LuaE Lua.Exception ()
    copyFromTable tableName funcName = do
        _ ← Lua.getglobal (Lua.Name tableName)
        _ ← Lua.getfield (-1) (Lua.Name funcName)
        Lua.remove (-2)
        Lua.setfield (-2) (Lua.Name funcName)

-- | Convert a Lua value at the given stack index to a Text representation.
--   Tables are recursively serialized to JSON format.
--   Depth limit prevents infinite recursion on circular references.
luaValueToText ∷ Int → Lua.StackIndex → Lua.LuaE Lua.Exception Text
luaValueToText depth idx
    | depth > 8 = return "\"<max depth>\""
    | otherwise = do
        ty ← Lua.ltype idx
        case ty of
            Lua.TypeNil     → return "null"
            Lua.TypeBoolean → do
                b ← Lua.toboolean idx
                return $ if b then "true" else "false"
            Lua.TypeNumber  → do
                -- Non-finite numbers (inf/-inf/nan) are not valid JSON, and
                -- math.huge is a live sentinel in game code. Emit a quoted
                -- stand-in so the headless → JSON → python pipeline still parses.
                -- NB: unwrap the constructor rather than realToFrac — the
                -- latter routes through toRational, which mangles inf/nan.
                mNum ← Lua.tonumber idx
                case mNum of
                    Just (Lua.Number d)
                        | isInfinite d → return $ if d > 0 then "\"inf\"" else "\"-inf\""
                        | isNaN d      → return "\"nan\""
                    _ → do
                        Lua.pushvalue idx
                        mStr ← Lua.tostring (-1)
                        Lua.pop 1
                        return $ maybe "0" TE.decodeUtf8 mStr
            Lua.TypeString  → do
                mStr ← Lua.tostring idx
                return $ case mStr of
                    Just bs → "\"" <> escapeJsonText (TE.decodeUtf8 bs) <> "\""
                    Nothing → "\"\""
            Lua.TypeTable   → luaTableToJson depth idx
            _               → do
                -- Function, userdata, thread, etc.
                Lua.pushvalue idx
                mStr ← Lua.tostring (-1)
                Lua.pop 1
                return $ case mStr of
                    Just bs → TE.decodeUtf8 bs
                    Nothing → "\"<" <> T.pack (show ty) <> ">\""

-- | Serialize a Lua table to JSON. Detects arrays vs objects:
--   if all keys are consecutive integers starting at 1, emit [...],
--   otherwise emit {...}.
luaTableToJson ∷ Int → Lua.StackIndex → Lua.LuaE Lua.Exception Text
luaTableToJson depth idx = do
    -- First pass: check if it's an array (keys are EXACTLY the
    -- consecutive run 1..n). A purely positive-integer-keyed but sparse
    -- table (e.g. {[1]=a,[5]=b}) is an object — emitting it as an array
    -- would silently drop the gaps and renumber the survivors.
    let absIdx = if idx < 0 then idx - 1 else idx
    Lua.pushnil  -- first key
    pairs ← collectTablePairs (depth + 1) absIdx []
    let parsedKeys = traverse (\(k, _) → case T.decimal k of
                        Right (n, rest) | T.null rest → Just (n ∷ Int)
                        _                             → Nothing) pairs
        isArray = case parsedKeys of
            Just ks@(_:_) → sort ks ≡ [1 .. length ks]
            _             → False
    if isArray
        then do
            -- Sort by integer key and emit as array
            let readInt t = case T.decimal t of
                    Right (n, _) → n ∷ Int
                    _            → 0
                sorted = sortBy (\(a,_) (b,_) → compare (readInt a) (readInt b)) pairs
            return $ "[" <> T.intercalate "," (map snd sorted) <> "]"
        else do
            let entries = map (\(k, v) → "\"" <> escapeJsonText k <> "\":" <> v) pairs
            return $ "{" <> T.intercalate "," entries <> "}"

-- | Collect all key-value pairs from a table. Leaves stack clean.
collectTablePairs ∷ Int → Lua.StackIndex → [(Text, Text)]
                  → Lua.LuaE Lua.Exception [(Text, Text)]
collectTablePairs depth tableIdx acc = do
    hasNext ← Lua.next tableIdx
    if not hasNext
        then return (reverse acc)
        else do
            -- Stack: ... table ... key value
            valText ← luaValueToText depth (-1)
            -- Get key as text (careful: tostring on key would break next())
            keyText ← do
                keyTy ← Lua.ltype (-2)
                case keyTy of
                    Lua.TypeNumber → do
                        Lua.pushvalue (-2)
                        mStr ← Lua.tostring (-1)
                        Lua.pop 1
                        return $ maybe "0" TE.decodeUtf8 mStr
                    Lua.TypeString → do
                        mStr ← Lua.tostring (-2)
                        return $ maybe "" TE.decodeUtf8 mStr
                    _ → return "<key>"
            Lua.pop 1  -- pop value, keep key for next iteration
            collectTablePairs depth tableIdx ((keyText, valText) : acc)

-- | Escape special characters for JSON string values.
--   All C0 control chars (U+0000–U+001F) must be escaped per the JSON spec;
--   the common ones get named escapes, the rest a \\u00XX form.
escapeJsonText ∷ Text → Text
escapeJsonText = T.concatMap $ \c → case c of
    '"'  → "\\\""
    '\\' → "\\\\"
    '\n' → "\\n"
    '\r' → "\\r"
    '\t' → "\\t"
    '\b' → "\\b"
    '\f' → "\\f"
    _ | c < '\x20' → "\\u" <> T.pack (let h = showHex (fromEnum c) ""
                                      in replicate (4 - length h) '0' <> h)
      | otherwise  → T.singleton c
