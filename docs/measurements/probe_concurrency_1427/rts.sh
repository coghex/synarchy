#!/usr/bin/env bash
# RTS-sensitivity subset. Every cell uses the SAME 8 total requested
# attempts as the primary matrix, so each is directly comparable to its
# N4 counterpart there.
#
#   RTS-A  all three probes, rts-caps {1,8}, concurrency 4
#          -> compares against the primary n4-c4-<probe> cells
#   RTS-B  thermo_altitude only, rts-caps {1,8}, concurrency 1
#          -> the no-contention reference completing a full
#             caps x concurrency sub-block for one probe
#
#   rts.sh <output-root> [repo-root]
#
# <output-root> is REQUIRED and has no default; see primary.sh.
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

run_cell() {  # probe caps conc
  cell="$ROOT/n${2}-c${3}-${1}"
  echo "=== START $(date -u +%H:%M:%S) $cell"
  bash "$SP/cohort.sh" "$1" "$2" "$3" 8 "$cell" "$REPO" || echo "COHORT LAUNCHER FAILED"
  echo "=== END   $(date -u +%H:%M:%S) $cell"
}
for caps in 1 8; do
  for probe in role thermo_altitude position_hold; do
    run_cell "$probe" "$caps" 4
  done
done
for caps in 1 8; do
  run_cell thermo_altitude "$caps" 1
done
echo "RTS SUBSET COMPLETE $(date -u +%Y-%m-%dT%H:%M:%SZ)"
