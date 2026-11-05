"""Revision resolution, base-record reading and bootstrap eligibility for
tools/current_save_schema_audit.py (issue #2916): the only module that
talks to git.
"""
from __future__ import annotations

import hashlib
import subprocess
from dataclasses import dataclass, field
from pathlib import Path

RECORD_REL = "docs/save_compat/current_schema.json"

# Everything the guard consists of. A base revision containing ANY of
# these has introduced the guard, so its record must exist and validate;
# only a base containing NONE is the pre-guard base that may be bootstrapped.
GUARD_ARTIFACTS = (
    "tools/current_save_schema_audit.py",
    RECORD_REL,
    "src/World/Save/Wire.hs",
    "test-headless/Test/Headless/World/Save/CurrentSchema.hs",
)

# Paths whose candidate-side changes the bootstrap adapter may carry onto
# the pre-guard base: the inspection hooks and the compiled helper.
ADAPTER_PATHS = (
    "src/World/Save/Component/Types.hs",
    "src/World/Save/Envelope.hs",
    "src/World/Save/Wire.hs",
    "src/World/Save/Wire",
    "test-headless/Spec.hs",
    "test-headless/Test/Headless/World/Save/Components/Assembly.hs",
    "test-headless/Test/Headless/World/Save/Components/Registry.hs",
    "test-headless/Test/Headless/World/Save/CurrentSchema.hs",
    "test-headless/Test/Headless/World/Save/CurrentSchema",
    "synarchy.cabal",
)

# Lines the adapter may REMOVE from existing files. The hooks are
# additive except where a declaration gains a constraint or a field; any
# other removed line would change a base wire declaration, version value
# or decoder, and the bootstrap refuses it.
ADAPTER_MAY_REMOVE = frozenset([
    # the only rewritten declaration: componentCodec gains a WireSchema
    # constraint so the registry can keep the DTO's compiled shape
    "componentCodec ∷ S.Serialize d ⇒ ComponentSpec d a → ComponentCodec a",
])


class EvidenceError(Exception):
    """The comparison base or candidate could not be established."""


@dataclass
class Revisions:
    base_ref: str
    base_tip: str
    base_sha: str          # the merge-base the comparison reads
    candidate_sha: str
    dirty: bool
    edit_identity: str
    notes: list[str] = field(default_factory=list)


def _git(root: Path, *args: str, binary: bool = False, check: bool = True):
    proc = subprocess.run(["git", "-C", str(root), *args],
                          capture_output=True)
    if check and proc.returncode != 0:
        raise EvidenceError(
            f"git {' '.join(args)} failed: "
            f"{proc.stderr.decode(errors='replace').strip()}")
    if proc.returncode != 0:
        return None
    return proc.stdout if binary else proc.stdout.decode().strip()


def working_edit_identity(root: Path) -> tuple[bool, str]:
    """Whether tracked files differ from HEAD, and a digest of exactly
    how (the candidate's tracked working-edit identity)."""
    diff = _git(root, "diff", "HEAD", "--binary", binary=True)
    status = _git(root, "status", "--porcelain", "--untracked-files=no")
    if not diff and not status:
        return False, "clean"
    return True, hashlib.sha256(diff).hexdigest()[:16]


def resolve(root: Path, base_ref: str) -> Revisions:
    if not base_ref or base_ref.startswith("-"):
        raise EvidenceError(f"unusable base ref {base_ref!r}")
    tip = _git(root, "rev-parse", "--verify", f"{base_ref}^{{commit}}")
    head = _git(root, "rev-parse", "--verify", "HEAD^{commit}")
    base = _git(root, "merge-base", tip, head)
    dirty, identity = working_edit_identity(root)
    return Revisions(base_ref=base_ref, base_tip=tip, base_sha=base,
                     candidate_sha=head, dirty=dirty, edit_identity=identity)


def check_distinct(rev: Revisions) -> list[str]:
    """A candidate compared to itself proves nothing."""
    if not rev.base_sha or not rev.candidate_sha:
        return ["comparison evidence missing: the base or candidate "
                "revision did not resolve to a commit"]
    if rev.base_sha == rev.candidate_sha and not rev.dirty:
        return [f"candidate-to-itself comparison: base {rev.base_ref} "
                f"resolves to {rev.base_sha}, the candidate commit, with "
                f"no uncommitted changes -- compare against the revision "
                f"the change started from"]
    return []


def read_blob(root: Path, sha: str, rel: str) -> str | None:
    out = _git(root, "show", f"{sha}:{rel}", binary=True, check=False)
    return None if out is None else out.decode()


def blob_id(root: Path, sha: str, rel: str) -> str | None:
    return _git(root, "rev-parse", "--verify", f"{sha}:{rel}", check=False)


def guard_artifacts_at(root: Path, sha: str) -> list[str]:
    """Which of the guard's artifacts the tree at `sha` contains."""
    return [rel for rel in GUARD_ARTIFACTS
            if _git(root, "cat-file", "-e", f"{sha}:{rel}",
                    binary=True, check=False) is not None]


def adapter_patch(root: Path, base_sha: str, head_sha: str) -> bytes:
    """The candidate's hook-and-helper changes, as a patch onto the base."""
    return _git(root, "diff", "--binary", base_sha, head_sha, "--",
                *ADAPTER_PATHS, binary=True)


def adapter_problems(patch: bytes) -> list[str]:
    """The adapter must not alter a base wire declaration, version value
    or decoder: outside new files, it may remove only the allowlisted
    hook-rewrite lines."""
    problems: list[str] = []
    current = ""
    new_file = False
    for raw in patch.decode(errors="replace").splitlines():
        if raw.startswith("diff --git"):
            current = raw.split(" b/")[-1]
            new_file = False
        elif raw.startswith("new file mode"):
            new_file = True
        elif raw.startswith("-") and not raw.startswith("---") \
                and not new_file:
            text = raw[1:].strip()
            if text and text not in ADAPTER_MAY_REMOVE:
                problems.append(f"{current}: the adapter would remove "
                                f"`{text[:80]}`")
    return problems
