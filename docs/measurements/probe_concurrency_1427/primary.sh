#!/usr/bin/env bash
# Primary N4 matrix: 3 probes x concurrency {1,2,4,8}, 8 total attempts per cell.
# Cohort order: concurrency-major ascending, probes in a fixed order within each level.
#
#   primary.sh <output-root> [repo-root]
#
# <output-root> is REQUIRED and has no default. It must be outside every
# git working tree; `cohort.sh` checks that and refuses otherwise. This
# script deliberately cannot target its own checked-in directory: that
# dataset is a historical record of one run, and a later characterization
# gets its own output root rather than overwriting it.
set -u
SP="$(cd "$(dirname "$0")" && pwd)"
if [ "$#" -lt 1 ]; then
  echo "usage: $0 <output-root> [repo-root]" >&2
  echo "  <output-root>  fresh directory for this run's cohorts, outside any checkout" >&2
  echo "  [repo-root]    the synarchy checkout to run probes from (default: this file's)" >&2
  exit 2
fi
ROOT="$1"
REPO="${2:-$(git -C "$SP" rev-parse --show-toplevel)}"

# Raw probe output must never be written into a git working tree. The
# checked-in dataset under `docs/measurements/` is a historical record of
# one run: a rerun that landed on top of it would silently replace the
# evidence the report cites and leave `summary.json` describing data that
# no longer exists. `probe_flake.check_artifact_root` refuses a
# repository artifact root for the same reason; this is the same rule for
# the cohort directory the launcher owns.
#
# Judged on the deepest EXISTING ancestor, before anything is created, so
# a refused path leaves no stray directory behind.
refuse_if_in_checkout() {  # <path>
  local probe="$1" tree
  while [ ! -d "$probe" ] && [ "$probe" != "/" ] && [ "$probe" != "." ]; do
    probe="$(dirname "$probe")"
  done
  if tree="$(git -C "$probe" rev-parse --show-toplevel 2>/dev/null)"; then
    echo "refusing to write cohort output into the git working tree at $tree." >&2
    echo "Pass an output directory outside every checkout, e.g. /tmp/probe-1427." >&2
    return 1
  fi
  return 0
}
refuse_if_in_checkout "$ROOT" || exit 2

for conc in 1 2 4 8; do
  for probe in role thermo_altitude position_hold; do
    cell="$ROOT/n4-c${conc}-${probe}"
    echo "=== START $(date -u +%H:%M:%S) $cell"
    bash "$SP/cohort.sh" "$probe" 4 "$conc" 8 "$cell" "$REPO" || echo "COHORT LAUNCHER FAILED"
    echo "=== END   $(date -u +%H:%M:%S) $cell"
  done
done
# Drift / test-retest control: repeat the first cell last, identically.
cell="$ROOT/n4-c1-role-retest"
echo "=== START $(date -u +%H:%M:%S) $cell"
bash "$SP/cohort.sh" role 4 1 8 "$cell" "$REPO" || echo "COHORT LAUNCHER FAILED"
echo "=== END   $(date -u +%H:%M:%S) $cell"
echo "PRIMARY MATRIX COMPLETE $(date -u +%Y-%m-%dT%H:%M:%SZ)"
