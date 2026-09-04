-- | The "Lua persistence components" gate (issue #761, save-overhaul
--   B3): a standalone Lua VM (no engine, no world/unit threads, no
--   HsLua-side marshalling of the registry's internals) exercising
--   @scripts/lib/data_codec.lua@ and @scripts/lib/save_modules.lua@
--   directly, the same pattern this suite already uses for pure
--   Haskell logic ("Test.Headless.UI.Clipping" etc.) applied to Lua:
--   each 'it' runs one self-contained Lua chunk via 'Lua.dostring' in
--   a fresh interpreter (stdlib + a minimal @engine@ stub — the only
--   global these two modules ever reach outside a real engine boot),
--   asserting inside Lua via @assert()@/@error()@ and surfacing a
--   non-OK 'Lua.Status' as an hspec failure with the Lua message.
--
--   Runs with @cabal test@'s CWD at the repo root (same as every other
--   repo-root-relative Lua path in this codebase), so
--   @require("scripts.lib.*")@ resolves via Lua's own default
--   @package.path@ with no extra setup.
--
--   A composition facade (issue #2047): it owns no test bodies and no
--   fixtures, and adds no @describe@ level of its own, so every
--   example's full hspec path under the unchanged
--   @Lua persistence components@ description is exactly what it was
--   before the split. It composes the gate's four contract owners in
--   the order their groups have always run:
--
--   * 'Test.Headless.Lua.SaveModules.DataCodec' — @data_codec.lua@'s
--     wire format.
--   * 'Test.Headless.Lua.SaveModules.Registry' — @save_modules.lua@'s
--     registration and lifecycle, rollback double faults, and the
--     real @applyLuaLoad@ bridge.
--   * 'Test.Headless.Lua.SaveModules.EntityApplication' — per-entity
--     application and restored-entity context isolation.
--   * 'Test.Headless.Lua.SaveModules.Components' — the real
--     @unit_ai@/@building_spawn@ components, reconciliation, version
--     bounds, and the tracked v1 fixtures on disk.
--
--   Their shared standalone-VM mechanics — the @engine@ stub and the
--   two chunk runners — live in
--   'Test.Headless.Lua.SaveModules.Support'.
--
--   This FILE PATH is load-bearing beyond the module name:
--   @docs\/save_compat\/manifest.json@'s
--   @b3-lua-versioned-hspec-coverage@ entry names it, and
--   @tools\/save_compat_audit.py@ fails when a manifest fixture's path
--   does not exist. Moving or renaming it fails a blocking CI gate.
--
--   Run just this gate: @cabal test synarchy-test-headless
--   --test-options='--match "Lua persistence components"'@.
module Test.Headless.Lua.SaveModules (spec) where

-- NB: no UPrelude import. This facade names no value beyond the four
-- child specs and hspec's own Spec monad, and the suite builds with
-- -Werror=unused-imports.
import Test.Hspec

import qualified Test.Headless.Lua.SaveModules.DataCodec as DataCodec
import qualified Test.Headless.Lua.SaveModules.Registry as Registry
import qualified Test.Headless.Lua.SaveModules.EntityApplication
    as EntityApplication
import qualified Test.Headless.Lua.SaveModules.Components as Components

spec ∷ Spec
spec = do
    DataCodec.spec           -- data_codec wire format
    Registry.spec            -- registry, rollback, applyLuaLoad bridge
    EntityApplication.spec   -- per-entity application, context isolation
    Components.spec          -- real components, reconciliation, fixtures
