"""Revision resolution and export for tools/current_save_schema_audit.py
(issue #2916): the only module that talks to git.

The comparison base is the MERGE-BASE of the requested ref and HEAD, so
a target branch that moved on after this one forked is not read as this
change reverting its work. Both the ref's tip and the merge-base are
resolved to full SHAs and reported.
"""
from __future__ import annotations

import subprocess
from pathlib import Path

from current_save_schema_compare import Evidence

# Everything the model reads. `docs/` is deliberately absent: the record
# is a projection of the candidate and never an input to the base.
EXPORT_PATHS = ("src", "app", "scripts", "synarchy.cabal")


class EvidenceError(Exception):
    """The comparison base or candidate could not be established."""


def _git(root: Path, *args: str, binary: bool = False):
    proc = subprocess.run(["git", "-C", str(root), *args],
                          capture_output=True)
    if proc.returncode != 0:
        raise EvidenceError(
            f"git {' '.join(args)} failed: "
            f"{proc.stderr.decode(errors='replace').strip()}")
    return proc.stdout if binary else proc.stdout.decode().strip()


def resolve(root: Path, base_ref: str) -> Evidence:
    if not base_ref or base_ref.startswith("-"):
        raise EvidenceError(f"unusable base ref {base_ref!r}")
    tip = _git(root, "rev-parse", "--verify", f"{base_ref}^{{commit}}")
    head = _git(root, "rev-parse", "--verify", "HEAD^{commit}")
    base = _git(root, "merge-base", tip, head)
    status = _git(root, "status", "--porcelain", "--", *EXPORT_PATHS)
    return Evidence(base_ref=f"{base_ref} ({tip[:12]}) merge-base",
                    base_sha=base, candidate_sha=head, dirty=bool(status))


def export_tree(root: Path, sha: str, dest: Path) -> None:
    archive = _git(root, "archive", sha, "--", *EXPORT_PATHS, binary=True)
    proc = subprocess.run(["tar", "-x", "-C", str(dest)], input=archive,
                          capture_output=True)
    if proc.returncode != 0:
        raise EvidenceError(
            f"extracting {sha}: {proc.stderr.decode(errors='replace')}")
