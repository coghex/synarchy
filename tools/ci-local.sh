#!/usr/bin/env bash
#
# Local mirror of the CI gate (#527). `make ci` runs this; it executes the
# same checks .github/workflows/ci.yml runs, in the same order, so a green
# run here predicts a green run in CI ("green locally => green in CI").
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

echo "==> [1/13] build (library + executable, -Werror)"
cabal build all

echo "==> [2/13] build test suites"
cabal build synarchy-test-headless
cabal build synarchy-test-graphical

echo "==> [3/13] headless hspec suite"
cabal test synarchy-test-headless --test-show-details=direct

echo "==> [4/13] test audit"
python3 tools/test_audit.py

echo "==> [5/13] lua module line budget"
python3 tools/lua_module_budget.py

echo "==> [6/13] lua duplicate function audit"
python3 tools/lua_duplicate_function_audit.py

echo "==> [7/13] haskell module line budget"
python3 tools/test_haskell_module_budget.py
python3 tools/haskell_module_budget.py

echo "==> [8/13] unicode operator audit"
python3 tools/test_unicode_operator_audit.py
python3 tools/unicode_operator_audit.py

echo "==> [9/13] persistence inventory audit"
python3 tools/test_persistence_inventory_audit.py
python3 tools/persistence_inventory_audit.py

echo "==> [10/13] EngineEnv capability inventory audit"
python3 tools/test_engine_env_capability_audit.py
python3 tools/engine_env_capability_audit.py

echo "==> [11/13] save compatibility audit"
python3 tools/test_save_compat_audit.py
python3 tools/save_compat_audit.py

echo "==> [12/13] cabal library module inventory audit"
python3 tools/test_cabal_module_audit.py
python3 tools/cabal_module_audit.py

echo "==> [13/13] world_check --quick"
python3 tools/world_check.py --quick

echo "==> make ci: all gates passed"
