#!/usr/bin/env bash
# cohort.sh <probe> <rts_caps> <concurrency> <total_attempts> <outdir>
#
# Launches <concurrency> simultaneous tools/probe_flake.py invocations,
# each requesting <total_attempts>/<concurrency> sequential runs, so the
# cohort's TOTAL requested attempts is <total_attempts> whatever the
# concurrency. Refuses to start while any unrelated probe_flake harness
# is registered machine-wide.
set -u
probe=$1; caps=$2; conc=$3; total=$4; out=$5
if (( total % conc != 0 )); then echo "total must divide by conc"; exit 9; fi
runs=$(( total / conc ))
mkdir -p "$out"
live=$(ls /tmp/synarchy-probe-flake-live-*.json 2>/dev/null | wc -l | tr -d ' ')
if [ "$live" != "0" ]; then echo "REGISTRY NOT EMPTY ($live live)"; exit 9; fi
{
  echo "cohort probe=$probe rts_caps=$caps concurrency=$conc total_attempts=$total runs_per_invocation=$runs"
  echo "repo=$(pwd)"
  echo "commit=$(git rev-parse HEAD)"
  echo "tree_clean=$([ -z "$(git status --porcelain)" ] && echo yes || echo no)"
  echo "started_utc=$(date -u +%Y-%m-%dT%H:%M:%SZ)"
  echo "epoch_start=$(date +%s)"
} > "$out/cohort.txt"
pids=()
for i in $(seq 1 "$conc"); do
  python3 tools/probe_flake.py --probe "$probe" --runs "$runs" --rts-caps "$caps" \
    --artifact-root "$HOME/probe-flake-1427-artifacts" \
    --result "$out/inv${i}.json" > "$out/inv${i}.stdout" 2> "$out/inv${i}.stderr" &
  pids+=($!)
done
: > "$out/exit_codes.txt"
for i in "${!pids[@]}"; do
  wait "${pids[$i]}"; echo "inv$((i+1)) exit=$?" >> "$out/exit_codes.txt"
done
{
  echo "finished_utc=$(date -u +%Y-%m-%dT%H:%M:%SZ)"
  echo "epoch_end=$(date +%s)"
} >> "$out/cohort.txt"
cat "$out/exit_codes.txt"
