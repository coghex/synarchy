#!/usr/bin/env python3
"""Derive CI's project-build cache epoch from master history.

GitHub Actions caches are immutable.  A key containing only the Cabal plan
therefore freezes the first ``dist-newstyle`` snapshot for that plan forever,
and every later build recompiles all project changes since that snapshot.

This tool divides build-relevant first-parent changes after a fixed anchor into
groups of eight.  Changes 0..7 use epoch 0, change 8 advances to epoch 1, and
change 16 advances to epoch 2.  CI
computes a pull request's epoch from its base SHA and a master push's epoch from
the pushed SHA.  Consequently PRs restore a cache authored by trusted master
CI; they do not advance the epoch or publish branch-scoped project caches.

Only inputs that can change compiled project products count.  Documentation,
Lua, assets and data do not age ``dist-newstyle``.  Missing or rewritten
history fails closed: the command exits non-zero instead of silently reusing an
arbitrary epoch.

Usage:
  python3 tools/ci_cache_epoch.py --ref <commit>
  python3 tools/ci_cache_epoch.py --ref <commit> --github-output "$GITHUB_OUTPUT"
  python3 tools/ci_cache_epoch.py --self-test
"""

from __future__ import annotations

import argparse
import fnmatch
import subprocess
import sys
import tempfile
from dataclasses import dataclass
from pathlib import Path


REPO_ROOT = Path(__file__).resolve().parent.parent

# Master immediately before the eight-change policy was introduced.  Keeping
# the anchor in source makes the epoch reproducible from Git alone: no mutable
# repository variable, artifact, cache or API counter can race between merges.
EPOCH_ANCHOR = "dd221f522276901ed08d5cb7574e9ce67ba9e2d9"
EPOCH_SIZE = 8

# Python's fnmatch '*' crosses '/', matching the repository's other CI path
# selectors.  This is a freshness policy, not a correctness boundary: Cabal
# still recompiles a dirty input even if a future path is accidentally absent.
BUILD_INPUT_GLOBS: tuple[str, ...] = (
    ".github/ci/*",
    ".github/workflows/ci.yml",
    ".github/workflows/ci-image.yml",
    "Setup.hs",
    "app/*",
    "cbits/*",
    "src/*",
    "test/*",
    "test-headless/*",
    "synarchy.cabal",
    "cabal.project*",
)

RECORD_PREFIX = "CI_CACHE_EPOCH"


class EpochError(RuntimeError):
    """The requested history cannot produce a trustworthy epoch."""


@dataclass(frozen=True)
class EpochResult:
    anchor: str
    ref: str
    relevant_count: int
    epoch: int
    position: int
    next_refresh_in: int


def _git(repo: Path, *args: str, check: bool = True) -> subprocess.CompletedProcess[str]:
    proc = subprocess.run(
        ["git", *args], cwd=repo, capture_output=True, text=True)
    if check and proc.returncode != 0:
        detail = (proc.stderr or proc.stdout).strip()
        raise EpochError(f"git {' '.join(args)} failed: {detail}")
    return proc


def is_build_relevant(paths: list[str]) -> bool:
    """Whether a first-parent change touches a compiled-product input."""
    return any(fnmatch.fnmatch(path, glob)
               for path in paths for glob in BUILD_INPUT_GLOBS)


def first_parent_changes(repo: Path, anchor: str, ref: str) -> list[str]:
    """Every first-parent commit in ``anchor..ref``, oldest first."""
    for label, commit in (("anchor", anchor), ("ref", ref)):
        if _git(repo, "cat-file", "-e", f"{commit}^{{commit}}",
                check=False).returncode != 0:
            raise EpochError(f"cache-epoch {label} {commit!r} is not a commit")
    if _git(repo, "merge-base", "--is-ancestor", anchor, ref,
            check=False).returncode != 0:
        raise EpochError(
            f"cache-epoch anchor {anchor} is not an ancestor of {ref}; "
            "refusing to guess after rewritten or incomplete history")
    text = _git(repo, "rev-list", "--first-parent", "--reverse",
                f"{anchor}..{ref}").stdout
    return [line.strip() for line in text.splitlines() if line.strip()]


def changed_paths(repo: Path, commit: str) -> list[str]:
    """Paths changed by ``commit`` relative to its first parent."""
    parent = _git(repo, "rev-parse", f"{commit}^1").stdout.strip()
    text = _git(repo, "diff", "--no-renames", "--name-only",
                parent, commit).stdout
    return [line.strip() for line in text.splitlines() if line.strip()]


def derive_epoch(repo: Path, ref: str,
                 anchor: str = EPOCH_ANCHOR) -> EpochResult:
    commits = first_parent_changes(repo, anchor, ref)
    relevant = sum(
        1 for commit in commits if is_build_relevant(changed_paths(repo, commit)))
    # The snapshot at the anchor is the start of epoch 0. Each eighth relevant
    # first-parent change advances the key, so the refresh happens after eight
    # merges rather than on the ninth build that follows them.
    epoch = relevant // EPOCH_SIZE
    position = relevant % EPOCH_SIZE
    next_refresh = EPOCH_SIZE - position
    resolved_ref = _git(repo, "rev-parse", ref).stdout.strip()
    resolved_anchor = _git(repo, "rev-parse", anchor).stdout.strip()
    return EpochResult(
        anchor=resolved_anchor,
        ref=resolved_ref,
        relevant_count=relevant,
        epoch=epoch,
        position=position,
        next_refresh_in=next_refresh,
    )


def record(result: EpochResult) -> str:
    return (
        f"{RECORD_PREFIX} epoch={result.epoch} "
        f"relevant_count={result.relevant_count} "
        f"position={result.position}/{EPOCH_SIZE} "
        f"next_refresh_in={result.next_refresh_in} "
        f"anchor={result.anchor} ref={result.ref}")


def write_github_output(path: Path, result: EpochResult) -> None:
    with path.open("a", encoding="utf-8") as handle:
        for name, value in (
                ("epoch", result.epoch),
                ("relevant-count", result.relevant_count),
                ("position", result.position),
                ("epoch-size", EPOCH_SIZE),
                ("next-refresh-in", result.next_refresh_in),
                ("anchor", result.anchor),
                ("ref", result.ref)):
            handle.write(f"{name}={value}\n")


def write_github_summary(path: Path, result: EpochResult) -> None:
    with path.open("a", encoding="utf-8") as handle:
        handle.write(
            "### Project build cache epoch\n\n"
            "| Epoch | Relevant changes | Since refresh | Next refresh | Source |\n"
            "|---:|---:|---:|---:|---|\n"
            f"| {result.epoch} | {result.relevant_count} | "
            f"{result.position}/{EPOCH_SIZE} | "
            f"{result.next_refresh_in} change(s) | "
            f"`{result.ref[:12]}` |\n\n")


def _commit(repo: Path, path: str, text: str, message: str) -> str:
    target = repo / path
    target.parent.mkdir(parents=True, exist_ok=True)
    target.write_text(text, encoding="utf-8")
    _git(repo, "add", path)
    _git(repo, "commit", "--quiet", "-m", message)
    return _git(repo, "rev-parse", "HEAD").stdout.strip()


def self_test() -> int:
    failures: list[str] = []

    def check(condition: bool, message: str) -> None:
        if not condition:
            failures.append(message)

    positives = [
        ".github/ci/Dockerfile",
        ".github/workflows/ci.yml",
        "app/Main.hs",
        "cbits/stb.c",
        "src/World/Types.hs",
        "test/Spec.hs",
        "test-headless/Spec.hs",
        "synarchy.cabal",
        "cabal.project.freeze",
    ]
    for path in positives:
        check(is_build_relevant([path]), f"build input {path!r} must count")
    for path in ("docs/a.md", "scripts/init.lua", "assets/x.png",
                 "data/materials.yaml", "tools/ci_probes.py"):
        check(not is_build_relevant([path]),
              f"runtime/non-build input {path!r} must not age dist-newstyle")

    with tempfile.TemporaryDirectory() as tmp:
        repo = Path(tmp)
        _git(repo, "init", "--quiet", "-b", "master", ".")
        _git(repo, "config", "user.email", "cache-epoch@example.invalid")
        _git(repo, "config", "user.name", "cache epoch self-test")
        anchor = _commit(repo, "docs/base.md", "base\n", "anchor")

        doc_ref = _commit(repo, "docs/a.md", "doc\n", "docs only")
        got = derive_epoch(repo, doc_ref, anchor)
        check((got.relevant_count, got.epoch, got.position,
               got.next_refresh_in) == (0, 0, 0, 8),
              f"docs-only history must stay at the seed boundary, got {got}")

        refs: list[str] = []
        for number in range(1, EPOCH_SIZE + 2):
            refs.append(_commit(
                repo, f"src/M{number}.hs", f"module M{number} where\n",
                f"relevant {number}"))
        seventh = derive_epoch(repo, refs[EPOCH_SIZE - 2], anchor)
        check((seventh.relevant_count, seventh.epoch, seventh.position,
               seventh.next_refresh_in) == (7, 0, 7, 1),
              f"the seventh change must be one away from refresh, got {seventh}")
        eighth = derive_epoch(repo, refs[EPOCH_SIZE - 1], anchor)
        check((eighth.relevant_count, eighth.epoch, eighth.position,
               eighth.next_refresh_in) == (8, 1, 0, 8),
              f"the eighth change must seed epoch 1, got {eighth}")
        ninth = derive_epoch(repo, refs[EPOCH_SIZE], anchor)
        check((ninth.relevant_count, ninth.epoch, ninth.position,
               ninth.next_refresh_in) == (9, 1, 1, 7),
              f"the ninth change must reuse epoch 1, got {ninth}")

        other = repo / "other"
        other.mkdir()
        _git(other, "init", "--quiet", "-b", "master", ".")
        _git(other, "config", "user.email", "cache-epoch@example.invalid")
        _git(other, "config", "user.name", "cache epoch self-test")
        unrelated = _commit(other, "src/X.hs", "module X where\n", "other")
        try:
            derive_epoch(repo, ninth.ref, unrelated)
        except EpochError as error:
            check("not an ancestor" in str(error) or "not a commit" in str(error),
                  f"unrelated anchor diagnostic must be explicit, got {error}")
        else:
            failures.append("an unrelated anchor must fail closed")

    if failures:
        print("ci_cache_epoch self-test: FAILED")
        for failure in failures:
            print(f"  - {failure}")
        return 1
    print("ci_cache_epoch self-test: all cases pass")
    return 0


def main(argv: list[str] | None = None) -> int:
    parser = argparse.ArgumentParser(
        description="Derive the eight-change project-cache epoch from "
                    "first-parent repository history.")
    parser.add_argument("--ref", help="commit whose master history sets the epoch")
    parser.add_argument("--anchor", default=EPOCH_ANCHOR,
                        help="fixed epoch anchor (default: checked-in policy anchor)")
    parser.add_argument("--repo-root", type=Path, default=REPO_ROOT)
    parser.add_argument("--github-output", type=Path,
                        help="append epoch fields to this GitHub output file")
    parser.add_argument("--github-summary", type=Path,
                        help="append an epoch table to this GitHub summary file")
    parser.add_argument("--self-test", action="store_true")
    args = parser.parse_args(argv)

    if args.self_test:
        return self_test()
    if not args.ref:
        parser.error("--ref is required unless --self-test is used")
    try:
        result = derive_epoch(args.repo_root.resolve(), args.ref, args.anchor)
    except EpochError as error:
        print(f"ci_cache_epoch: {error}", file=sys.stderr)
        return 1
    print(record(result))
    if args.github_output:
        write_github_output(args.github_output, result)
    if args.github_summary:
        write_github_summary(args.github_summary, result)
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
