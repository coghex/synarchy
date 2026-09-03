{-# LANGUAGE Strict #-}
{-# LANGUAGE OverloadedStrings #-}
-- | "Build placement page binding" (#1602): ONE build placement is bound
--   to the page its click hit-tested, from the synchronous pick through
--   validation to commit.
--
--   The contract has two halves, and both are exercised here against the
--   REAL registered Lua API and the REAL @scripts/build_tool.lua@ /
--   @scripts/build_tool_remote_warning.lua@ paths:
--
--     * __Page coherence within one call.__ @building.canPlaceAt@ and
--       @building.setGhost@ each resolve the world manager exactly once
--       and derive everything from that single resolution — page id,
--       page-scoped occupancy, placed locations, u-wrap world size,
--       canonical coordinates and terrain. Both fixture pages differ in
--       every one of those, so an answer assembled from two reads is
--       distinguishable from one assembled from a single read.
--     * __Freshness across calls.__ @world.pickTile@ reports the page it
--       hit-tested together with the page-SELECTION generation it
--       resolved under, and a placement that carries that pair is
--       refused once selection has moved — including an A→B→A sequence
--       that ends on the same page id, which no page-id comparison can
--       see.
--
--   This module is the FAÇADE. It owns the only fixture lifecycle and
--   the aggregate group; the examples belong to four owners, each a
--   fixture-consuming 'SpecWith' that starts no engine, Lua state or
--   resource root of its own:
--
--     * "Test.Headless.Building.PageBinding.Resolution" — the fixture
--       discriminators, what the pick reports, the single-resolution
--       guarantee, and empty-visible behaviour.
--     * "Test.Headless.Building.PageBinding.SynchronousStaleness" — a
--       selection change landing INSIDE one click.
--     * "Test.Headless.Building.PageBinding.ApplyTime" — the binding
--       re-checked and discharged on the world thread.
--     * "Test.Headless.Building.PageBinding.PendingProjection" — a
--       selection change enqueued but not yet applied.
--
--   Whatever more than one of them needs lives in
--   "Test.Headless.Building.PageBinding.Support", which DEFINES the
--   engine-free half of the fixture but never runs it.
--
--   The engine here is this module's own ('initializeEngineHeadlessQuiet',
--   like 'Test.Headless.World.DesignationSeam'\'s engine-backed half):
--   it runs NO worker threads, so a queued 'BuildingSpawn' or
--   'WorldDesignateConstruct' stays in its queue and "nothing was
--   committed" is asserted on the queue itself rather than raced
--   against a drainer.
--
--   Neither page costs worldgen: both are in-memory 'emptyWorldState'
--   pages carrying synthetic flat chunks, the same stand-in
--   @tools/remote_warning_page_guard_probe.py@ makes with two arenas.
--
--   Run just this gate: @cabal test synarchy-test-headless
--   --test-options='--match "Build placement page binding"'@.
module Test.Headless.Building.PageBinding (spec) where

import UPrelude
import Test.Hspec
import Test.Headless.Harness.Isolation (withIsolatedResourceRoot)

import Engine.Core.Init (EngineInitResult(..))
import Test.Headless.Harness.Log (initializeEngineHeadlessQuiet)
import Test.Headless.Building.PageBinding.ApplyTime (applyTimeSpec)
import Test.Headless.Building.PageBinding.PendingProjection (pendingSpec)
import Test.Headless.Building.PageBinding.Resolution
    (apiCoherenceSpec, emptyVisibleSpec, fixtureSpec, pickBindingSpec)
import Test.Headless.Building.PageBinding.Support
    (installPageSwitch, newBareLuaBackend, rememberRealVerbs)
import Test.Headless.Building.PageBinding.SynchronousStaleness (staleSpec)

-- * Spec

spec ∷ Spec
spec = describe "Build placement page binding (#1602)" $ aroundAll setup $ do
    fixtureSpec
    pickBindingSpec
    apiCoherenceSpec
    emptyVisibleSpec
    staleSpec
    applyTimeSpec
    pendingSpec
  where
    -- Isolation wraps the boot, not the other way round (#1357): engine
    -- init is itself a config writer, so a scratch resource root
    -- established afterwards would already be too late. It stays open
    -- for the whole group because the engine booted inside it —
    -- @scripts/@ is symlinked there, so the real build-tool Lua still
    -- loads.
    setup act = withIsolatedResourceRoot $ do
        EngineInitResult env ← initializeEngineHeadlessQuiet
        ls ← newBareLuaBackend env
        installPageSwitch env ls
        _ ← rememberRealVerbs ls
        act (env, ls)
