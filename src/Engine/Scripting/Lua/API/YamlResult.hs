{-# LANGUAGE Strict #-}
-- | The one shape every @engine.load*Yaml@ binding answers Lua with
--   (#2203).
--
--   Before this, a binding pushed a bare count and a broken data file
--   was indistinguishable from a file that legitimately held nothing:
--   both arrived in Lua as @0@, so @scripts/startup_loader.lua@ could
--   drain its whole queue over a corrupt @data/@ tree and still reach
--   the main menu.
--
--   The count STAYS the default answer, and stays the only one. A
--   binding's parse outcome is OPT-IN, requested by passing a truthy
--   SECOND argument, because @engine.load*Yaml(path)@'s single numeric
--   result is load-bearing well outside the startup loader:
--   'Engine.Scripting.Lua.Thread.Console.executeDebugLua' tab-joins
--   every value a chunk returns, so an unconditional second result
--   would silently rewrite what probes like @tools/craft_probe.py@ read
--   back from a bare @return engine.loadRecipeYaml(path)@.
module Engine.Scripting.Lua.API.YamlResult
    ( pushYamlResult
    ) where

import UPrelude
import qualified HsLua as Lua

-- | Answer one @engine.load*Yaml@ call: the count always, and the
--   parse outcome after it only when the caller asked for it.
--
--   @parsed@ is about the DECODE alone — whether the file yielded a
--   definition list at all. A file that decoded and was then rejected
--   by a family's own schema validation (an unresolvable location
--   naming scheme, say) parsed: it reports @True@ with whatever count
--   that rejection left, exactly as it always has.
pushYamlResult ∷ Bool → Int → Lua.LuaE Lua.Exception Lua.NumResults
pushYamlResult parsed count = do
    wantOutcome ← Lua.toboolean 2
    Lua.pushnumber (Lua.Number (fromIntegral count))
    if wantOutcome
        then do
            Lua.pushboolean parsed
            return 2
        else return 1
