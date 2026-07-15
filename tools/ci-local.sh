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
# Uses the default (prod) build profile and the default dist-newstyle, so
# it reuses your warm build instead of forcing a rebuild, and the exe it
# builds is the one world_check drives.
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

# Identical to the CI "Configure" step: -Werror for the local package only.
printf 'package synarchy\n  ghc-options: -Werror\n' > "$LOCAL"

echo "==> [1/8] build (library + executable, -Werror)"
cabal build all

echo "==> [2/8] build test suites"
cabal build synarchy-test-headless
cabal build synarchy-test-graphical

echo "==> [3/8] headless hspec suite"
cabal test synarchy-test-headless --test-show-details=direct

echo "==> [4/8] test audit"
python3 tools/test_audit.py

echo "==> [5/9] lua module line budget"
python3 tools/lua_module_budget.py

echo "==> [6/9] lua duplicate function audit"
python3 tools/lua_duplicate_function_audit.py

echo "==> [7/9] haskell module line budget"
python3 tools/haskell_module_budget.py

echo "==> [8/9] persistence inventory audit"
python3 tools/test_persistence_inventory_audit.py
python3 tools/persistence_inventory_audit.py

echo "==> [9/9] world_check --quick"
python3 tools/world_check.py --quick

echo "==> make ci: all gates passed"
