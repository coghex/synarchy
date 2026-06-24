{-# LANGUAGE Strict, UnicodeSyntax, OverloadedStrings #-}
-- | Generic YAML → Lua table loader. `engine.loadYaml(path)` decodes a YAML
--   file into a Lua value: objects → string-keyed tables, arrays → 1-indexed
--   tables, scalars → string / number / bool / nil. Returns nil on a parse or
--   read error.
--
--   This keeps data-driven defs that live Lua-side trivial — no per-file
--   Haskell loader. First user: the structure packs
--   (data/structure_packs/*.yaml), which `scripts/structures.lua` reads to
--   discover its texture/facemap paths instead of hardcoding them.
module Engine.Scripting.Lua.API.Yaml
    ( loadYamlFn
    ) where

import UPrelude
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Yaml as Yaml
import qualified Data.Aeson as A
import qualified Data.Aeson.KeyMap as KM
import qualified Data.Aeson.Key as K
import qualified Data.Vector as V
import qualified HsLua as Lua

-- | Push an Aeson value onto the Lua stack as the corresponding Lua value.
--   Leaves exactly one value on the stack.
pushAeson ∷ A.Value → Lua.LuaE Lua.Exception ()
pushAeson v = case v of
    A.Null     → Lua.pushnil
    A.Bool b   → Lua.pushboolean b
    A.String s → Lua.pushstring (TE.encodeUtf8 s)
    A.Number n →
        let d = realToFrac n ∷ Double
            i = round d ∷ Int
        in if fromIntegral i == d   -- whole number → push as a Lua integer
           then Lua.pushinteger (fromIntegral i)
           else Lua.pushnumber (Lua.Number d)
    A.Array a  → do
        Lua.newtable
        V.imapM_ (\i x → pushAeson x >> Lua.rawseti (-2) (fromIntegral (i + 1))) a
    A.Object o → do
        Lua.newtable
        mapM_ (\(k, x) → pushAeson x
                         >> Lua.setfield (-2) (Lua.Name (TE.encodeUtf8 (K.toText k))))
              (KM.toList o)

-- | engine.loadYaml(path) → table | nil
loadYamlFn ∷ Lua.LuaE Lua.Exception Lua.NumResults
loadYamlFn = do
    pathA ← Lua.tostring 1
    case pathA of
        Nothing → Lua.pushnil >> return 1
        Just pathBS → do
            let path = T.unpack (TE.decodeUtf8 pathBS)
            r ← Lua.liftIO (Yaml.decodeFileEither path)
            case (r ∷ Either Yaml.ParseException A.Value) of
                Left _  → Lua.pushnil >> return 1
                Right v → pushAeson v >> return 1
