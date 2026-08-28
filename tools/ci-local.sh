#!/usr/bin/env bash
#
# Local mirror of the CI gate (#527). `make ci` runs this; it executes the
# same checks .github/workflows/ci.yml's test-and-audits worker runs, in the same
# order, so a green run here predicts a green run in CI ("green locally =>
# green in CI").
#
# That parity is enforced, not merely intended (#1355): the last step runs
# tools/ci_parity_audit.py, which compares this file's `python3 tools/*.py`
# invocations against that job's, at command-and-arguments granularity and
# in BOTH directions, and fails on any difference outside its hard-coded,
# reason-carrying exemption list. Adding a check to one file without the
# other now fails immediately instead of surfacing after a push. Two
# things the audit deliberately does not compare: conditional control flow
# (CI path-selects the graphical build, the unit-asset gate, world_check
# and -- since #1364 -- the hspec step's SYNARCHY_FULL_TESTS=1 full tier
# on PRs; this file runs all four unconditionally, which is what makes it
# conservative), and the non-Python steps around them.
#
# -Werror is part of synarchy.cabal's checked-in warning policy now
# (#1057), so every build of the `synarchy` package -- here, in CI, or a
# plain `cabal build` -- already compiles under it. That closes the hole
# this comment used to describe: a module compiled warm *without*
# -Werror (because the flag was only ever injected here, at gate-run
# time, via a scoped cabal.project.local) was never re-checked just
# because -Werror got added afterwards, since GHC's recompilation
# avoidance doesn't treat warning flags as affecting object code
# (confirmed by hand: this let an unused-field warning ship past
# `make ci` and fail CI, issue #869).
#
# Still scoped here via a temporary cabal.project.local: -fforce-recomp,
# needed for the remaining gap -- cross-module interactions in files that
# don't get recompiled, the same one .github/workflows/ci.yml's
# no-clean-backstop trade-off note accepts for CI's cache. It forces
# every module of the `synarchy` package to be genuinely rechecked every
# run, trading warm-build reuse for a result you can actually trust;
# already-built dependencies are unaffected and stay cached, so this is
# not as costly as a full clean build. The exe it builds is the one
# world_check drives.
#
# Any pre-existing cabal.project.local is backed up and restored on exit,
# so your dev config is left untouched whether the gate passes or fails.
set -euo pipefail

# Run from the repo root regardless of caller CWD.
cd "$(dirname "$0")/.."

LOCAL=cabal.project.local
BACKUP=

restore() {
  if [ -n "$BACKUP" ]; then
    mv -f "$BACKUP" "$LOCAL"
  else
    rm -f "$LOCAL"
  fi
}
trap restore EXIT

if [ -e "$LOCAL" ]; then
  BACKUP="$(mktemp)"
  cp "$LOCAL" "$BACKUP"
fi

# Resolve THIS working tree's own changed paths before the scratch
# cabal.project.local below exists (#1360). The order is load-bearing,
# not incidental: cabal.project.local is not gitignored, so a change can
# legitimately track one -- and CI's save-compat gate selects on it,
# because cabal would apply it there. Capturing after the write would
# report this gate's own scratch edit to a tracked file as if it were
# the candidate's, which is exactly what requirement 7 forbids;
# capturing before reports the candidate's real edit and nothing else.
# The list feeds the save-compat decision at step 11.
SAVE_COMPAT_PATHS="$(python3 tools/ci_expensive_gates.py --local-changed-paths)"

# -fforce-recomp so a warm build can't mask a warning a fresh build would
# catch; see the header comment above for why -Werror itself no longer
# needs to be injected here.
printf 'package synarchy\n  ghc-options: -fforce-recomp\n' > "$LOCAL"

echo "==> [1/23] build (library + executable, -Werror)"
cabal build all

echo "==> [2/23] build test suites"
cabal build synarchy-test-headless
cabal build synarchy-test-graphical

echo "==> [3/23] headless hspec suite (full tier)"
# SYNARCHY_FULL_TESTS=1 turns the full-tier examples from pending into
# real runs (#1364) -- today exactly one, the w128 seed-42 volcano
# exposure regression in test-headless/Test/Headless/WorldGen/Exposure.hs.
# CI applies it only when its worldgen selector fires; this file runs
# every gate unconditionally (see the header note), so it applies it
# always and stays the conservative side of that parity. It must be set
# on the `cabal test` process itself, and it must be `1` rather than ''
# -- the test's guard treats ANY present value, empty string included, as
# enabled.
SYNARCHY_FULL_TESTS=1 cabal test synarchy-test-headless --test-show-details=direct

echo "==> [4/23] test audit"
python3 tools/test_audit.py

echo "==> [5/23] lua module line budget"
python3 tools/lua_module_budget.py

echo "==> [6/23] lua duplicate function audit"
python3 tools/test_lua_duplicate_function_audit.py
python3 tools/lua_duplicate_function_audit.py

echo "==> [7/23] haskell module line budget"
python3 tools/test_haskell_module_budget.py
python3 tools/haskell_module_budget.py

echo "==> [8/23] unicode operator audit"
python3 tools/test_unicode_operator_audit.py
python3 tools/unicode_operator_audit.py

echo "==> [9/23] lua strict-decode audit"
python3 tools/lua_strict_decode_audit.py --self-test
python3 tools/lua_strict_decode_audit.py

echo "==> [10/23] persistence inventory audit"
python3 tools/test_persistence_inventory_audit.py
python3 tools/persistence_inventory_audit.py

echo "==> [11/23] EngineEnv capability inventory audit"
python3 tools/test_engine_env_capability_audit.py
python3 tools/engine_env_capability_audit.py

# Local `make ci` keeps both commands unconditional. CI runs them for
# every non-docs-only change and every save-compat input change, but skips
# this Cabal-backed step after its docs-only selector proves the range is
# unrelated documentation.
# --without-reproducibility (#1360) drops exactly ONE member of the
# self-test module -- the one that spawns its own `cabal repl` to build
# two timestamp variants -- and the block below runs that member when
# this working tree's own changes touch a path that can move its result.
# Every other member and the whole real audit still run on every local
# invocation.
echo "==> [12/23] save compatibility audit"
python3 tools/test_save_compat_audit.py --without-reproducibility
python3 tools/save_compat_audit.py

# The reproducibility member, selected by the SAME decision CI takes
# (#1360, requirement 7): $SAVE_COMPAT_PATHS was resolved at the top of
# this script, against the merge base with the default branch and before
# the scratch cabal.project.local existed, and is piped here into the
# very same `ci_expensive_gates.py --stdin --gate save-compat` command
# .github/workflows/ci.yml runs. Not a second matcher that could drift:
# one script, one pattern table, one answer. An unresolvable base yields
# the selector's conservative sentinel, which selects the gate, so a
# detached or shallow checkout runs the coverage rather than skipping it.
# tools/ci_parity_audit.py fails if the two files stop agreeing about
# which gate decides this or which command it guards.
# >>> save-compat reproducibility selection >>>
# tools/ci_parity_audit.py EXTRACTS the lines between these two markers
# and EXECUTES them against synthetic changed-path lists (supplied as
# $SAVE_COMPAT_PATHS), so this block's real behaviour -- not a grep for
# its text -- is what proves `make ci` selects the reproducibility
# member from the same decision CI's `if:` takes. Keep the markers, and
# keep the block reading $SAVE_COMPAT_PATHS rather than re-deriving it:
# re-deriving it here would put the resolution back after the scratch
# write.
SAVE_COMPAT_REPRO="$(printf '%s\n' "$SAVE_COMPAT_PATHS" | python3 tools/ci_expensive_gates.py --stdin --gate save-compat)"
if [ "$SAVE_COMPAT_REPRO" = true ]; then
  echo "==> [12/23] save compatibility fixture reproducibility (selected)"
  python3 tools/test_save_compat_audit.py --only-reproducibility
else
  echo "==> [12/23] save compatibility fixture reproducibility: skipped (no save-format, fixture, save-tooling or Cabal path changed)"
fi
# <<< save-compat reproducibility selection <<<

echo "==> [13/23] enum append-only audit"
python3 tools/enum_append_only_audit.py --self-test
python3 tools/enum_append_only_audit.py

echo "==> [14/23] cabal library module inventory audit"
python3 tools/test_cabal_module_audit.py
python3 tools/cabal_module_audit.py

echo "==> [15/23] material id/name correspondence audit"
python3 tools/material_id_audit.py --self-test
python3 tools/material_id_audit.py

# Cheap, no-engine guard (issue #1717): fails if a concept id
# data/language/concepts.yaml has ever shipped is missing from it, or if
# a new one arrived without being ratcheted into
# docs/language/concept_id_baseline.json. A ConceptId is persisted
# identity -- Language.Etymology reports one the catalogue no longer
# carries as EtyInvalidConcept, losing that etymology in every existing
# save -- and Language.Generated.Hash seeds each concept's native root
# from the id string, so a rename re-roots the concept. Nothing else
# names an individual id: the headless suite's catalogue checks are all
# aggregates (version, count, domains, balance, forms), and the naming
# goldens cover only the handful of concepts their samples use.
# Unconditional rather than path-selective: it is a two-file comparison
# costing milliseconds, and either side can drift alone.
echo "==> [16/23] concept id inventory audit"
python3 tools/concept_id_inventory_audit.py --self-test
python3 tools/concept_id_inventory_audit.py

echo "==> [17/23] findings report status audit"
python3 tools/test_findings_report_audit.py
python3 tools/findings_report_audit.py

# One command, three checks: the #1257 inventory, #1258's freshness
# comparison against a fresh regeneration, and #1262's image/slot and
# resident-memory budgets. --strict is what makes a budget breach fail
# rather than merely print.
echo "==> [18/23] unit asset inventory, freshness and budget"
python3 tools/test_pack_atlas.py
python3 tools/pack_atlas.py --validate-only --strict

echo "==> [19/23] world_check --quick"
python3 tools/world_check.py --quick

# Validate the probe-runner harness itself (cheap, no engine, no GPU) --
# the same checks, in the same order, as ci.yml's "probe runner
# self-tests" step. ci_probes/ci_expensive_gates cover the path->probe
# and path->gate mappings, which would otherwise only surface after a
# push as a PR mis-selecting its own gates; test_run_probes covers
# run_probes.py's process-group teardown and --exact key rejection;
# test_persistence_contract_sweep covers the cross-probe registry-drift
# guard; test_action_outcome_probe covers action_outcome_probe.py's
# fixture classification against a fake console, so its branches are
# checked without that probe's own ~8-minute real engine, and
# (#1793) find_mixed_box's anchor and 5x5 fluid reads;
# test_tillable_fluid_filter is #1793's own: the tillable-tile
# scans in till_probe, plant_probe and farm_ai_probe read
# world.getFluidAt through its ARITY contract, so a wet tile is
# never returned as tillable. It owns the fake console that
# reproduces the debug console's tab-joined multi-return, which
# test_action_outcome_probe imports rather than re-deriving. Those
# three probes each boot a real engine and generate a world; this
# companion boots nothing;
# test_probelib pins probelib.send_json's result contract against a real
# socket and fails if a probe grows a private JSON console wrapper again
# (#1160), and owns probe_engine.py's launcher contract -- runner mode
# execs the resolved binary, direct mode keeps the `cabal run` fallback
# (#1570); test_probe_flake mutation-covers probe_protocol.py,
# probe_flake.py and probe_census.py's parsers, and every #1426 protocol
# migration extends it (#1475); test_probe_census is the census's own
# self-test -- the record, its atomic writer, the declared schema, and
# #1429's cohort, freshness and staleness semantics -- against synthetic
# documents in throwaway temporary trees, touching no docs worktree;
# test_probe_claim is #1434's -- the atomic per-probe claim, its lease,
# ownership-safe takeover and release, and the claim-aware orchestration
# boundary -- racing real interpreters against a shared barrier file and
# SIGKILLing one of them, because a claim that must hold between OS
# processes cannot be proved by threads.
# test_probe_resource_lock is #1436's cross-process reader/writer lock --
# the half run_probes' in-process ledger cannot provide -- proved with
# separate interpreters and one SIGKILLed holder; test_deflake is the
# /deflake orchestrator, driven entirely through injected adapters, with
# probe_census and probe_flake.Measurement themselves real against
# throwaway censuses. No probe is ever executed by any of them, and the
# real engine-booting ten-run measurement is deliberately NOT wired into
# this gate or CI (tools/README.md states why).
# test_location_embark_probe is #1569's: the artifact ownership of
# tools/location_embark_probe.py -- one invocation-owned directory,
# --resource-root on every boot, release on a pass, a phase-0 return, an
# exception and a boot abort, residue as a failing check, a pre-existing
# same-named save slot left byte-identical, and a read-only checkout
# still yielding a removable tree. That probe is manual-only needs-gpu,
# so without this companion the contract is only ever observed by a GPU
# run neither gate can make; the companion boots nothing.
# test_movement_probe is #1586's: tools/movement_probe.py --list is a
# metadata query answered from scripts/movement_arena.lua before any
# boot(), for every --mode, and the derived view is held to the runtime
# M.listCourses() by every real course run. That probe is manual-only, so
# without this companion a reintroduced boot() on the listing path, a
# mode dispatch that preempts --list again, or a silently empty inventory
# would only ever be noticed by hand; the companion boots nothing.
#
# tools/test_deflake_diagnosis.py (#1437) is deliberately absent from
# this list as well, and from the CI job it mirrors: that issue's
# approved rereview amendment scopes the diagnosis lab's own self-test
# to manual invocation. It is engine-free and takes seconds -- run it by
# hand when touching tools/deflake_diagnosis.py.
echo "==> [20/23] probe runner self-tests"
python3 tools/ci_probes.py --self-test
python3 tools/ci_expensive_gates.py --self-test
python3 tools/ci_docs_fast_path.py --self-test
python3 tools/test_run_probes.py
python3 tools/test_persistence_contract_sweep.py
python3 tools/test_action_outcome_probe.py
python3 tools/test_tillable_fluid_filter.py
python3 tools/test_probelib.py
python3 tools/test_probe_flake.py
python3 tools/test_probe_census.py
python3 tools/test_probe_claim.py
python3 tools/test_probe_resource_lock.py
python3 tools/test_deflake.py
python3 tools/test_location_embark_probe.py
python3 tools/test_movement_probe.py

# The decision .github/workflows/review-gate.yml makes on every
# synchronize push: keep `reviewed:approve` only when the push left the
# PR's own patch identical (#1679). That job runs on a different event
# and mutates a label rather than producing a check, so nothing else
# here would notice it regressing; this self-test is the only thing that
# observes its policy. It builds throwaway commit graphs in a temporary
# directory -- no engine, no network, no GitHub, about a second.
echo "==> [21/23] review-gate decision self-test"
python3 tools/review_gate_decision.py --self-test

# Cheap, no-engine self-test of CI's cache-outcome report (#1358). The
# report itself runs only in CI -- `make ci` restores no GitHub Actions
# cache, so it has no outcome to classify -- but its classification and
# the ci.yml wiring that classification reads are checked here, because a
# mis-wired reporter is indistinguishable from a healthy cache: reverting
# either cache step to the combined `actions/cache` action would empty
# `cache-matched-key` and turn every prefix hit into a reported cold
# cache, with nothing failing.
echo "==> [22/23] CI cache policy and report self-tests"
python3 tools/ci_cache_epoch.py --self-test
python3 tools/ci_cache_cleanup.py --self-test
python3 tools/ci_cache_report.py --self-test

# The gate that keeps this file honest (#1355): fails if a
# `python3 tools/*.py` check runs in ci.yml's test-and-audits worker and not
# here, or here and not there, outside the audit's hard-coded exemption
# list. Without it the two drift silently, and they already had --- the
# original five of the probe-runner self-tests above ran only in CI.
echo "==> [23/23] CI/local gate parity audit"
python3 tools/ci_parity_audit.py --self-test
python3 tools/ci_parity_audit.py

echo "==> make ci: all gates passed"
