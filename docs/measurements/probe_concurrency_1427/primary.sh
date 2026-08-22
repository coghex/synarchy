#!/usr/bin/env bash
# Primary N4 matrix: 3 probes x concurrency {1,2,4,8}, 8 total attempts per cell.
# Cohort order: concurrency-major ascending, probes in a fixed order within each level.
set -u
SP="$(dirname "$0")"
ROOT="$SP/cohorts"
cd /Users/vincentcoghlan/worktrees/coghex/synarchy/issue-1427-probe-concurrency-characterization || exit 1
for conc in 1 2 4 8; do
  for probe in role thermo_altitude position_hold; do
    cell="$ROOT/n4-c${conc}-${probe}"
    echo "=== START $(date -u +%H:%M:%S) $cell"
    bash "$SP/cohort.sh" "$probe" 4 "$conc" 8 "$cell" || echo "COHORT LAUNCHER FAILED"
    echo "=== END   $(date -u +%H:%M:%S) $cell"
  done
done
# Drift / test-retest control: repeat the first cell last, identically.
cell="$ROOT/n4-c1-role-retest"
echo "=== START $(date -u +%H:%M:%S) $cell"
bash "$SP/cohort.sh" role 4 1 8 "$cell" || echo "COHORT LAUNCHER FAILED"
echo "=== END   $(date -u +%H:%M:%S) $cell"
echo "PRIMARY MATRIX COMPLETE $(date -u +%Y-%m-%dT%H:%M:%SZ)"
