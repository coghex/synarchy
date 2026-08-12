#!/usr/bin/env bash
# Land one or more docs from the docs-wip worktree onto master.
#
# Why this exists: docs land on master by direct push (see CLAUDE.md
# "Working-tree discipline"), and the naive recipe rebases with
# --autostash. That stashes EVERY dirty file, not just the ones being
# landed, and replays them over a moved master -- so landing doc A while
# doc B is half-written can conflict on B. This script:
#
#   * commits ONLY the paths you name, leaving other dirty files alone;
#   * skips the rebase entirely when master has not moved (the common
#     case), so there is nothing to stash and nothing to conflict;
#   * warns BEFORE doing anything if a dirty file you are NOT landing
#     also changed upstream -- that is the exact autostash-conflict
#     predictor;
#   * judges success by rev-list, never by push output, because the
#     protected-ref warning prints on every successful admin-bypass push;
#   * fast-forwards the primary checkout only when it is clean.
#
# Usage:
#   tools/docs_land.sh -m "Commit subject" docs/foo.md [docs/bar.md ...]
#   tools/docs_land.sh -n -m "..." docs/foo.md      # dry run
#   tools/docs_land.sh -f -m "..." docs/foo.md      # ignore the risk warning
set -euo pipefail

DRY=0
FORCE=0
MSG=""
while getopts "m:nfh" opt; do
  case "$opt" in
    m) MSG="$OPTARG" ;;
    n) DRY=1 ;;
    f) FORCE=1 ;;
    h) sed -n '2,30p' "$0"; exit 0 ;;
    *) exit 2 ;;
  esac
done
shift $((OPTIND - 1))

[ $# -gt 0 ] || { echo "error: name at least one path to land" >&2; exit 2; }
[ -n "$MSG" ] || { echo "error: -m \"commit subject\" is required" >&2; exit 2; }

# Resolve the docs worktree by BRANCH, never a hard-coded path.
DOCS_WT="$(git worktree list --porcelain \
  | awk '/^worktree /{p=substr($0,10)} /^branch refs\/heads\/docs-wip$/{print p; exit}')"
if [ -z "$DOCS_WT" ]; then
  echo "error: no worktree on branch docs-wip. Create one with:" >&2
  echo "  git worktree add ~/work/synarchy-docs -b docs-wip origin/master" >&2
  exit 1
fi
cd "$DOCS_WT"

run() { if [ "$DRY" = 1 ]; then echo "  would run: $*"; else "$@"; fi; }

git fetch -q origin

# --- Pre-flight: predict an autostash conflict before touching anything ---
# Files that are dirty, NOT being landed, and ALSO changed on master since
# our merge base are exactly the ones a rebase --autostash could conflict on.
# Written for bash 3.2 (macOS /bin/bash): no mapfile, no bare expansion of a
# possibly-empty array under `set -u`.
BASE="$(git merge-base HEAD origin/master)"
RISK=""
while IFS= read -r f; do
  [ -n "$f" ] || continue
  landing=0
  for p in "$@"; do [ "$f" = "$p" ] && landing=1; done
  [ "$landing" = 1 ] && continue
  if git diff --name-only "$BASE" origin/master | grep -qxF -- "$f"; then
    case "$RISK" in *"|$f|"*) ;; *) RISK="$RISK|$f|" ;; esac
  fi
done < <( { git diff --name-only; git diff --cached --name-only; } | sort -u )
if [ -n "$RISK" ]; then
  echo "WARNING: these files are dirty here AND changed on master:" >&2
  printf '%s\n' "$RISK" | tr '|' '\n' | grep -v '^$' | sed 's/^/  /' >&2
  echo "A rebase would stash and replay them, which can conflict." >&2
  echo "Land or commit them first, or accept the risk and re-run with -f." >&2
  [ "$FORCE" = 1 ] || [ "$DRY" = 1 ] || exit 3
fi

# --- Commit only the named paths ---
# Both git calls are scoped to "$@" on purpose. A bare `git commit` records the
# WHOLE index and a bare `git diff --cached --quiet` inspects it, so an unrelated
# file left staged in this worktree would otherwise ride along -- or, when the
# named paths are unchanged, be pushed to master entirely on its own. The
# pathspec form commits only the named paths and leaves other index entries
# staged, which is what makes the named-path-only promise above hold no matter
# what state the index was already in.
run git add -- "$@"
if [ "$DRY" = 0 ] && git diff --cached --quiet -- "$@"; then
  # Not an early exit: a prior interrupted run may already have made the commit,
  # and the push/verify steps below still need to finish that landing.
  echo "nothing staged from the named paths; already committed?"
else
  run git commit -q -m "$MSG" -- "$@"
fi

# --- Rebase ONLY if master actually moved (no move => nothing to stash) ---
if git merge-base --is-ancestor origin/master HEAD; then
  echo "master has not moved; no rebase needed"
else
  echo "master moved; rebasing"
  if [ "$DRY" = 0 ]; then
    if ! git rebase --autostash origin/master; then
      echo "" >&2
      echo "Rebase stopped. Your commit is SAFE and this is contained to" >&2
      echo "the docs worktree -- it cannot wedge the PR drainer." >&2
      echo "Resolve, 'git add' the files, then 'git rebase --continue'," >&2
      echo "and re-run this script. If the autostash failed to reapply," >&2
      echo "your other edits are in 'git stash list'." >&2
      exit 4
    fi
  else
    echo "  would run: git rebase --autostash origin/master"
  fi
fi

# --- Push. The protected-ref warning is expected and is NOT a failure. ---
run git push origin docs-wip:master

# --- Verify by rev-list, not by push output ---
if [ "$DRY" = 0 ]; then
  git fetch -q origin
  set -- $(git rev-list --left-right --count HEAD...origin/master)
  ahead="$1"; behind="$2"
  if [ "$ahead" != 0 ] || [ "$behind" != 0 ]; then
    echo "error: docs-wip and origin/master disagree (ahead=$ahead behind=$behind)" >&2
    exit 5
  fi
  echo "landed: docs-wip == origin/master"
fi

# --- Fast-forward the primary checkout, but only if it is clean ---
PRIMARY="$(git worktree list --porcelain \
  | awk '/^worktree /{p=substr($0,10)} /^branch refs\/heads\/master$/{print p; exit}')"
if [ -n "$PRIMARY" ]; then
  if [ -n "$(git -C "$PRIMARY" status --porcelain)" ]; then
    echo "note: primary checkout is dirty; not fast-forwarding it"
  else
    run git -C "$PRIMARY" merge --ff-only origin/master
    [ "$DRY" = 1 ] || echo "primary checkout fast-forwarded"
  fi
fi
