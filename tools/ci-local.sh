#!/usr/bin/env bash
#
# Local mirror of the CI gate (#527). `make ci` runs this; it executes the
# same checks .github/workflows/ci.yml's build-test job runs, in the same
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
# (CI path-selects the graphical build, the unit-asset gate and
# world_check on PRs; this file runs all three unconditionally, which is
# what makes it conservative), and the non-Python steps around them.
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

# -fforce-recomp so a warm build can't mask a warning a fresh build would
# catch; see the header comment above for why -Werror itself no longer
# needs to be injected here.
printf 'package synarchy\n  ghc-options: -fforce-recomp\n' > "$LOCAL"

echo "==> [1/19] build (library + executable, -Werror)"
cabal build all

echo "==> [2/19] build test suites"
cabal build synarchy-test-headless
cabal build synarchy-test-graphical

echo "==> [3/19] headless hspec suite"
cabal test synarchy-test-headless --test-show-details=direct

echo "==> [4/19] test audit"
python3 tools/test_audit.py

echo "==> [5/19] lua module line budget"
python3 tools/lua_module_budget.py

echo "==> [6/19] lua duplicate function audit"
python3 tools/test_lua_duplicate_function_audit.py
python3 tools/lua_duplicate_function_audit.py

echo "==> [7/19] haskell module line budget"
python3 tools/test_haskell_module_budget.py
python3 tools/haskell_module_budget.py

echo "==> [8/19] unicode operator audit"
python3 tools/test_unicode_operator_audit.py
python3 tools/unicode_operator_audit.py

echo "==> [9/19] persistence inventory audit"
python3 tools/test_persistence_inventory_audit.py
python3 tools/persistence_inventory_audit.py

echo "==> [10/19] EngineEnv capability inventory audit"
python3 tools/test_engine_env_capability_audit.py
python3 tools/engine_env_capability_audit.py

echo "==> [11/19] save compatibility audit"
python3 tools/test_save_compat_audit.py
python3 tools/save_compat_audit.py

echo "==> [12/19] enum append-only audit"
python3 tools/enum_append_only_audit.py --self-test
python3 tools/enum_append_only_audit.py

echo "==> [13/19] cabal library module inventory audit"
python3 tools/test_cabal_module_audit.py
python3 tools/cabal_module_audit.py

echo "==> [14/19] material id/name correspondence audit"
python3 tools/material_id_audit.py --self-test
python3 tools/material_id_audit.py

echo "==> [15/19] findings report status audit"
python3 tools/test_findings_report_audit.py
python3 tools/findings_report_audit.py

# One command, three checks: the #1257 inventory, #1258's freshness
# comparison against a fresh regeneration, and #1262's image/slot and
# resident-memory budgets. --strict is what makes a budget breach fail
# rather than merely print.
echo "==> [16/19] unit asset inventory, freshness and budget"
python3 tools/test_pack_atlas.py
python3 tools/pack_atlas.py --validate-only --strict

echo "==> [17/19] world_check --quick"
python3 tools/world_check.py --quick

# Validate the probe-runner harness itself (cheap, no engine, no GPU) --
# the same five checks, in the same order, as ci.yml's "probe runner
# self-tests" step. ci_probes/ci_expensive_gates cover the path->probe
# and path->gate mappings, which would otherwise only surface after a
# push as a PR mis-selecting its own gates; test_run_probes covers
# run_probes.py's process-group teardown and --exact key rejection;
# test_persistence_contract_sweep covers the cross-probe registry-drift
# guard; test_action_outcome_probe covers action_outcome_probe.py's
# fixture classification against a fake console, so its branches are
# checked without that probe's own ~8-minute real engine.
echo "==> [18/19] probe runner self-tests"
python3 tools/ci_probes.py --self-test
python3 tools/ci_expensive_gates.py --self-test
python3 tools/test_run_probes.py
python3 tools/test_persistence_contract_sweep.py
python3 tools/test_action_outcome_probe.py

# The gate that keeps this file honest (#1355): fails if a
# `python3 tools/*.py` check runs in ci.yml's build-test job and not
# here, or here and not there, outside the audit's hard-coded exemption
# list. Without it the two drift silently, and they already had --- the
# five checks above ran only in CI.
echo "==> [19/19] CI/local gate parity audit"
python3 tools/ci_parity_audit.py --self-test
python3 tools/ci_parity_audit.py

echo "==> make ci: all gates passed"
