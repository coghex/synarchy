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
    , YamlRefusal(..)
    , pushYamlRefusal
    ) where

import UPrelude
import qualified Data.Text.Encoding as TE
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

-- | One post-decode SEMANTIC refusal, as Lua receives it (#2241,
--   widened by #2506).
--
--   @yrReason@ is the short phrase the terminal startup line leads with
--   — \"duplicate definition name\", \"undeclared faction tag\" — and
--   @yrDetail@ is the offending identifier. They travel as two values
--   rather than one pre-rendered sentence because
--   @scripts\/startup_loader.lua@ owns the spelling of that line and
--   quotes the identifier itself.
data YamlRefusal = YamlRefusal
    { yrReason ∷ !Text
    , yrDetail ∷ !Text
    } deriving (Show, Eq)

-- | Answer one @engine.load*Yaml@ call that DECODED its file and then
--   refused the whole of it on a semantic collision (#2241): a
--   duplicate flora name, or — since #2506 — a faction-tag rule.
--
--   The count is zero, because a refusal registers nothing. @parsed@ is
--   'True', because the file parsed — that field's meaning is not
--   widened here, for the same reason it was narrowed in the first
--   place: the other families rely on it meaning exactly \"the decode
--   produced a definition list\".
--
--   What carries the refusal is a THIRD value, @detail@ — the offending
--   identifier — and a FOURTH, @reason@, pushed only when the caller
--   opted in to the outcome at all. A healthy call still answers with
--   one value bare and two when asked; only a refusal answers with four,
--   so @scripts\/startup_loader.lua@ can name the file, the identifier
--   AND why it was refused without any other caller's arity moving.
--
--   The fourth value is what #2506 added. Before it the startup loader
--   had one refusal vocabulary — \"duplicate definition name\" — and
--   described every third-result refusal that way, which would have
--   reported an undeclared faction tag as a duplicate name.
pushYamlRefusal ∷ YamlRefusal → Lua.LuaE Lua.Exception Lua.NumResults
pushYamlRefusal refusal = do
    wantOutcome ← Lua.toboolean 2
    Lua.pushnumber (Lua.Number 0)
    if wantOutcome
        then do
            Lua.pushboolean True
            Lua.pushstring (TE.encodeUtf8 (yrDetail refusal))
            Lua.pushstring (TE.encodeUtf8 (yrReason refusal))
            return 4
        else return 1
