#!/usr/bin/env python3
"""The global probe census manifest and its inventory validation (#1425).

`docs/probe_census.json` is the durable migration inventory for the
de-flake lab: every registered probe exactly once, with its script, its
CI-eligible/manual-only classification, and its protocol status
(`legacy` or `probe-result/v1`). #1428 extends the SAME file with
measurements, acceptable-failure policy, locking and history; this
module only seeds and validates the inventory.

The manifest lives in the worktree whose branch is `docs-wip` and is
NOT published as part of this work, so it is resolved BY BRANCH the way
`tools/docs_land.sh` does — never a hard-coded path, never the primary
checkout, and never created implicitly. That is also why nothing at
runtime may depend on it: `tools/probe_flake.py` decides protocol
status from `probe_flake.PROTOCOL_PROBES` and check identity from each
probe's own descriptor, so a fresh checkout with no docs worktree
behaves identically.

Usage:
  python3 tools/probe_census.py --print            # the manifest, to stdout
  python3 tools/probe_census.py --seed             # write it into docs-wip
  python3 tools/probe_census.py --validate         # check the docs-wip copy
"""
from __future__ import annotations

import argparse
import json
import os
import subprocess
import sys
from pathlib import Path

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
import ci_probes  # noqa: E402
import probe_flake  # noqa: E402
import run_probes  # noqa: E402

MANIFEST_SCHEMA = "probe-census/v1"
MANIFEST_RELPATH = "docs/probe_census.json"
DOCS_BRANCH = "docs-wip"

CI_ELIGIBLE = "ci-eligible"
MANUAL_ONLY = "manual-only"
LEGACY = "legacy"


def classification(key: str) -> str:
    """The authoritative CI classification, read from `tools/ci_probes.py`."""
    return CI_ELIGIBLE if key in ci_probes.CI_ELIGIBLE else MANUAL_ONLY


def build_manifest() -> dict:
    """The manifest the live registry currently implies."""
    return {
        "schema": MANIFEST_SCHEMA,
        "probes": [
            {
                "key": key,
                "script": script,
                "classification": classification(key),
                "protocol": probe_flake.protocol_status(key),
            }
            for key, script, _purpose in run_probes.PROBES
        ],
    }


def render_manifest(manifest: dict | None = None) -> str:
    return json.dumps(manifest or build_manifest(), indent=2,
                      sort_keys=True) + "\n"


def validate_manifest(manifest) -> list[str]:
    """Every disagreement between `manifest` and the live registry.

    Rejects a missing, duplicate, or extra entry, and any row whose
    classification or protocol status disagrees with `run_probes.PROBES`,
    `tools/ci_probes.py`, and `probe_flake.PROTOCOL_PROBES`. An empty
    list means the manifest is a faithful inventory.
    """
    problems: list[str] = []
    if not isinstance(manifest, dict):
        return [f"manifest must be a JSON object, got {type(manifest).__name__}"]
    schema = manifest.get("schema")
    if schema != MANIFEST_SCHEMA:
        problems.append(
            f"manifest schema is {schema!r}, expected {MANIFEST_SCHEMA!r}")
    entries = manifest.get("probes")
    if not isinstance(entries, list):
        return problems + ["manifest `probes` must be a list"]

    expected = {key: (script, classification(key), probe_flake.protocol_status(key))
                for key, script, _purpose in run_probes.PROBES}
    seen: set[str] = set()
    for position, entry in enumerate(entries):
        if not isinstance(entry, dict):
            problems.append(f"entry {position} is not an object: {entry!r}")
            continue
        key = entry.get("key")
        if not isinstance(key, str):
            problems.append(f"entry {position} has no string `key`: {entry!r}")
            continue
        if key in seen:
            problems.append(f"duplicate entry for probe {key!r}")
            continue
        seen.add(key)
        if key not in expected:
            problems.append(
                f"extra entry {key!r}: not registered in run_probes.PROBES")
            continue
        script, expected_class, expected_protocol = expected[key]
        if entry.get("script") != script:
            problems.append(
                f"probe {key!r}: manifest script {entry.get('script')!r} "
                f"disagrees with the registry ({script!r})")
        if entry.get("classification") != expected_class:
            problems.append(
                f"probe {key!r}: manifest classification "
                f"{entry.get('classification')!r} disagrees with "
                f"tools/ci_probes.py ({expected_class!r})")
        if entry.get("protocol") != expected_protocol:
            problems.append(
                f"probe {key!r}: manifest protocol status "
                f"{entry.get('protocol')!r} disagrees with the in-repo "
                f"registry ({expected_protocol!r})")
    for key in expected:
        if key not in seen:
            problems.append(f"missing entry for registered probe {key!r}")
    return problems


# --------------------------------------------------------------------------
# The docs worktree
# --------------------------------------------------------------------------
class DocsWorktreeMissing(Exception):
    """No worktree is on `docs-wip`; the caller must create one."""


def resolve_docs_worktree(repo_root: str | None = None) -> Path:
    """The worktree whose branch is `docs-wip`, resolved BY BRANCH.

    The same idiom `tools/docs_land.sh` uses. A missing docs worktree is
    an actionable stop, never a silent fall back to the primary checkout
    (which the PR drainer must be able to fast-forward) and never an
    implicit `git worktree add` performed as a side effect.
    """
    root = repo_root or run_probes.REPO_ROOT
    try:
        done = subprocess.run(["git", "worktree", "list", "--porcelain"],
                              cwd=root, text=True, capture_output=True,
                              timeout=30)
    except (OSError, subprocess.SubprocessError) as error:
        raise DocsWorktreeMissing(
            f"could not list git worktrees ({error})") from None
    if done.returncode != 0:
        raise DocsWorktreeMissing(
            f"could not list git worktrees: {done.stderr.strip()}")
    current: str | None = None
    for line in done.stdout.splitlines():
        if line.startswith("worktree "):
            current = line[len("worktree "):]
        elif line.strip() == f"branch refs/heads/{DOCS_BRANCH}" and current:
            return Path(current)
    raise DocsWorktreeMissing(
        f"no worktree is on branch {DOCS_BRANCH}. Create one with:\n"
        f"  git worktree add ~/work/synarchy-docs -b {DOCS_BRANCH} origin/master")


def manifest_path(repo_root: str | None = None) -> Path:
    return resolve_docs_worktree(repo_root) / MANIFEST_RELPATH


def seed(repo_root: str | None = None) -> Path:
    path = manifest_path(repo_root)
    path.parent.mkdir(parents=True, exist_ok=True)
    path.write_text(render_manifest(), encoding="utf-8")
    return path


def load(path: Path):
    try:
        return json.loads(path.read_text(encoding="utf-8"))
    except OSError as error:
        raise DocsWorktreeMissing(
            f"manifest {path} is unreadable ({error})") from None
    except ValueError as error:
        raise ValueError(f"manifest {path} is not valid JSON: {error}") from None


def main(argv: list[str] | None = None) -> int:
    ap = argparse.ArgumentParser(
        description=__doc__,
        formatter_class=argparse.RawDescriptionHelpFormatter)
    group = ap.add_mutually_exclusive_group(required=True)
    group.add_argument("--print", dest="do_print", action="store_true",
                       help="print the manifest the live registry implies")
    group.add_argument("--seed", action="store_true",
                       help=f"write {MANIFEST_RELPATH} into the {DOCS_BRANCH} worktree")
    group.add_argument("--validate", action="store_true",
                       help=f"validate the {DOCS_BRANCH} worktree's manifest")
    args = ap.parse_args(argv)

    if args.do_print:
        sys.stdout.write(render_manifest())
        return 0
    try:
        if args.seed:
            path = seed()
            print(f"seeded {path} ({len(run_probes.PROBES)} probes)")
            return 0
        path = manifest_path()
        problems = validate_manifest(load(path))
    except DocsWorktreeMissing as error:
        print(f"probe_census: {error}", file=sys.stderr)
        return 2
    except ValueError as error:
        print(f"probe_census: {error}", file=sys.stderr)
        return 1
    if problems:
        for problem in problems:
            print(f"probe_census: {problem}", file=sys.stderr)
        return 1
    print(f"{path}: {len(run_probes.PROBES)} probes, inventory agrees with "
          f"run_probes.PROBES and tools/ci_probes.py")
    return 0


if __name__ == "__main__":
    sys.exit(main())
