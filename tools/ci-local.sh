#!/usr/bin/env bash
#
# Local mirror of the CI gate (#527). `make ci` runs this; it executes the
# same checks .github/workflows/ci.yml runs, in the same order, so a green
# run here predicts a green run in CI ("green locally => green in CI").
#
# -Werror is applied to the `synarchy` package exactly the way CI does it
# (a scoped `cabal.project.local`), so dependency warnings can't fail the
# build and a clean-checkout dependency build isn't compiled with -Werror.
# Any pre-existing cabal.project.local is backed up and restored on exit,
# so your dev config is left untouched whether the gate passes or fails.
#
# Also scoped alongside -Werror: -fforce-recomp. GHC's recompilation
# avoidance does not treat warning flags as affecting object code, so a
# module already compiled warm *without* -Werror is never re-checked just
# because -Werror is added afterwards via cabal.project.local -- a warning
# that would fail a clean CI checkout can silently pass here otherwise
# (confirmed by hand: this let an unused-field warning ship past `make ci`
# and fail CI, issue #869). -fforce-recomp forces every module of the
# `synarchy` package to be genuinely rechecked every run, trading warm-
# build reuse for a result you can actually trust; already-built
# dependencies are unaffected and stay cached, so this is not as costly as
# a full clean build. The exe it builds is the one world_check drives.
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

# Like the CI "Configure" step (-Werror for the local package only), plus
# -fforce-recomp so a warm build can't mask a warning CI would catch fresh.
printf 'package synarchy\n  ghc-options: -Werror -fforce-recomp\n' > "$LOCAL"

echo "==> [1/11] build (library + executable, -Werror)"
cabal build all

echo "==> [2/11] build test suites"
cabal build synarchy-test-headless
cabal build synarchy-test-graphical

echo "==> [3/11] headless hspec suite"
cabal test synarchy-test-headless --test-show-details=direct

echo "==> [4/11] test audit"
python3 tools/test_audit.py

echo "==> [5/11] lua module line budget"
python3 tools/lua_module_budget.py

echo "==> [6/11] lua duplicate function audit"
python3 tools/lua_duplicate_function_audit.py

echo "==> [7/11] haskell module line budget"
python3 tools/haskell_module_budget.py

echo "==> [8/11] persistence inventory audit"
python3 tools/test_persistence_inventory_audit.py
python3 tools/persistence_inventory_audit.py

echo "==> [9/11] EngineEnv capability inventory audit"
python3 tools/test_engine_env_capability_audit.py
python3 tools/engine_env_capability_audit.py

echo "==> [10/11] save compatibility audit"
python3 tools/test_save_compat_audit.py
python3 tools/save_compat_audit.py

echo "==> [11/11] world_check --quick"
python3 tools/world_check.py --quick

echo "==> make ci: all gates passed"
