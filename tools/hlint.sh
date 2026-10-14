#!/usr/bin/env bash
# Run from any directory. HLint hints intentionally retain their nonzero exit.
set -euo pipefail
cd "$(dirname "$0")/.."
expected=$(cat .hlint-version)
if ! command -v hlint >/dev/null 2>&1; then
  echo "Install HLint $expected as a standalone tool; see docs/hlint.md." >&2
  exit 2
fi
actual=$(hlint --numeric-version)
if [[ "$actual" != "$expected" ]]; then
  echo "Expected HLint $expected, found $actual; see docs/hlint.md." >&2
  exit 2
fi
cpp=()
if [[ "$(uname -s)" == Darwin ]]; then
  cpp+=(--cpp-define=DARWIN)
fi
# --git selects tracked and non-ignored untracked Haskell, excluding ignored output.
# Use --cpp-define=DEVELOPMENT for the development branch of CPP.
exec hlint --git -j4 "${cpp[@]}" "$@"
