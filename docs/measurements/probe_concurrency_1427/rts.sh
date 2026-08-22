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
set -u
SP="$(dirname "$0")"
ROOT="$SP/cohorts"
cd /Users/vincentcoghlan/worktrees/coghex/synarchy/issue-1427-probe-concurrency-characterization || exit 1
run_cell() {  # probe caps conc
  cell="$ROOT/n${2}-c${3}-${1}"
  echo "=== START $(date -u +%H:%M:%S) $cell"
  bash "$SP/cohort.sh" "$1" "$2" "$3" 8 "$cell" || echo "COHORT LAUNCHER FAILED"
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
