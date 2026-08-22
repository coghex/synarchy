#!/usr/bin/env bash
# cohort.sh <probe> <rts_caps> <concurrency> <total_attempts> <outdir> [repo-root]
#
# Launches <concurrency> simultaneous tools/probe_flake.py invocations,
# each requesting <total_attempts>/<concurrency> sequential runs, so the
# cohort's TOTAL requested attempts is <total_attempts> whatever the
# concurrency. Refuses to start while any unrelated probe_flake harness
# is registered machine-wide.
#
# <outdir> must be OUTSIDE every git working tree, and that is checked
# rather than trusted -- see the guard below. Retained probe artifacts
# land in `<outdir>/../probe-flake-artifacts` for the same reason.
set -u
if [ "$#" -lt 5 ]; then
  echo "usage: $0 <probe> <rts_caps> <concurrency> <total_attempts> <outdir> [repo-root]" >&2
  exit 9
fi
probe=$1; caps=$2; conc=$3; total=$4; out=$5
repo="${6:-$(git -C "$(dirname "$0")" rev-parse --show-toplevel)}"
if (( total % conc != 0 )); then echo "total must divide by conc" >&2; exit 9; fi
runs=$(( total / conc ))
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
refuse_if_in_checkout "$out" || exit 9
mkdir -p "$out"
out="$(cd "$out" && pwd)"
artifacts="$(dirname "$out")/probe-flake-artifacts"

live=$(ls /tmp/synarchy-probe-flake-live-*.json 2>/dev/null | wc -l | tr -d ' ')
if [ "$live" != "0" ]; then echo "REGISTRY NOT EMPTY ($live live)" >&2; exit 9; fi
{
  echo "cohort probe=$probe rts_caps=$caps concurrency=$conc total_attempts=$total runs_per_invocation=$runs"
  echo "repo=$repo"
  echo "commit=$(git -C "$repo" rev-parse HEAD)"
  echo "tree_clean=$([ -z "$(git -C "$repo" status --porcelain)" ] && echo yes || echo no)"
  echo "started_utc=$(date -u +%Y-%m-%dT%H:%M:%SZ)"
  echo "epoch_start=$(date +%s)"
} > "$out/cohort.txt"
pids=()
for i in $(seq 1 "$conc"); do
  ( cd "$repo" && python3 tools/probe_flake.py --probe "$probe" --runs "$runs" \
      --rts-caps "$caps" --artifact-root "$artifacts" \
      --result "$out/inv${i}.json" ) > "$out/inv${i}.stdout" 2> "$out/inv${i}.stderr" &
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
