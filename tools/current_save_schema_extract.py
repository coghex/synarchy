"""Compiled-evidence extraction for tools/current_save_schema_audit.py
(issue #2916): builds the headless test target and runs its
"current save schema" group, which writes the record.

The tree being extracted is always built from source here, immediately
before the run (the same reason `save_compat_audit_codec` builds before
`cabal list-bin`): `list-bin` names a path whether or not that file is
current, and accepting a binary because it EXISTS would compare against
yesterday's code. After the build, cabal's own dry run must report
nothing left to build, or the binary is rejected.
"""
from __future__ import annotations

import hashlib
import json
import os
import re
import subprocess
import tempfile
from dataclasses import dataclass, field
from pathlib import Path

from current_save_schema_compare import RecordError, parse_record
from current_save_schema_git import (
    ADAPTER_PATHS, EvidenceError, Revisions, adapter_patch, adapter_problems)

TARGET = "synarchy-test-headless"
GROUP = "current save schema"


@dataclass
class Evidence:
    record: dict
    toolchain: dict
    examples: int
    failures: int
    executable: str
    executable_sha256: str
    fixtures: list = field(default_factory=list)


def _run(cmd, cwd: Path, env=None, timeout=None) -> subprocess.CompletedProcess:
    return subprocess.run(cmd, cwd=cwd, env=env, text=True,
                          capture_output=True, timeout=timeout)


def assert_up_to_date(root: Path, flags: list[str]) -> None:
    """Cabal's own plan is the evidence that the helper is current: after
    the build, a dry run must have nothing left to build. (A timestamp
    scan would also trip on files the target never compiles, such as
    app/Main.hs.)"""
    dry = _run(["cabal", "build", TARGET, "--dry-run", *flags], root)
    text = dry.stdout + dry.stderr
    if dry.returncode != 0 or "Up to date" not in text:
        raise EvidenceError(
            f"stale candidate binary: cabal still has work to do for "
            f"{TARGET} after the build:\n{text[-800:]}")


def parse_counts(output: str) -> tuple[int, int]:
    """(examples, failures) from hspec's summary line."""
    m = re.findall(r"(\d+) examples?, (\d+) failures?", output)
    if not m:
        raise EvidenceError("the helper printed no hspec summary line; "
                            "extraction is incomplete")
    return int(m[-1][0]), int(m[-1][1])


def build_and_run(root: Path, builddir: Path | None = None,
                  want_fixtures: bool = False) -> Evidence:
    """Compile the target in `root` and run its extraction group."""
    flags = ["--builddir", str(builddir)] if builddir else []
    build = _run(["cabal", "build", TARGET, *flags], root)
    if build.returncode != 0:
        raise EvidenceError("building the extractor failed:\n"
                            + (build.stdout + build.stderr)[-2000:])
    where = _run(["cabal", "list-bin", TARGET, *flags], root)
    exe = Path(where.stdout.strip().splitlines()[-1]) if where.stdout.strip() \
        else None
    if where.returncode != 0 or exe is None or not exe.is_file():
        raise EvidenceError(f"cabal list-bin {TARGET} found no executable")
    assert_up_to_date(root, flags)
    digest = hashlib.sha256(exe.read_bytes()).hexdigest()
    with tempfile.TemporaryDirectory() as tmp:
        out = Path(tmp) / "evidence.json"
        fx = Path(tmp) / "fixtures.json"
        env = dict(os.environ, SYNARCHY_CURRENT_SCHEMA_EVIDENCE=str(out),
                   SYNARCHY_CURRENT_SCHEMA_FIXTURES=str(fx))
        run = _run([str(exe), "--match", GROUP], root, env=env)
        text = run.stdout + run.stderr
        examples, failures = parse_counts(text)
        if run.returncode != 0 or failures:
            raise EvidenceError(
                f"the `{GROUP}` group failed ({examples} examples, "
                f"{failures} failures):\n{text[-3000:]}")
        if examples == 0:
            raise EvidenceError(f"the `{GROUP}` group ran zero examples")
        if not out.is_file():
            raise EvidenceError("the helper wrote no evidence file")
        payload = json.loads(out.read_text())
        try:
            record = parse_record(json.dumps(payload["record"]),
                                  "compiled evidence")
        except RecordError as exc:
            raise EvidenceError(str(exc)) from exc
        fixtures = json.loads(fx.read_text()) if want_fixtures and fx.is_file() else []
    toolchain = dict(payload.get("toolchain", {}))
    ghc = _run(["ghc", "--numeric-version"], root)
    toolchain["ghc"] = ghc.stdout.strip()
    return Evidence(record=record, toolchain=toolchain, examples=examples,
                    failures=failures, executable=str(exe),
                    executable_sha256=digest, fixtures=fixtures)


def bootstrap_base(root: Path, rev: Revisions) -> tuple[dict, dict]:
    """First introduction only: extract the ACTUAL pre-guard base.

    The base revision has no helper, so the candidate's inspection hooks
    and compiled helper are laid onto an export of it as a patch. The
    patch is refused unless it only adds hooks: any removal of a base
    line outside the declared rewrite list could change a wire
    declaration, version value or decoder behaviour. Provenance (the
    base sha, the patch digest, the files the patch touches) is returned
    for the run output.
    """
    if rev.dirty:
        raise EvidenceError("the bootstrap needs a committed candidate: "
                            "commit the tracked changes first so the "
                            "adapter patch is reproducible")
    patch = adapter_patch(root, rev.base_sha, rev.candidate_sha)
    if not patch.strip():
        raise EvidenceError("the bootstrap adapter is empty: the candidate "
                            "adds no hooks to the base")
    problems = adapter_problems(patch)
    if problems:
        raise EvidenceError("the bootstrap adapter would alter the base's "
                            "declarations:\n  " + "\n  ".join(problems))
    with tempfile.TemporaryDirectory(prefix="csschema-base-") as tmp:
        tree = Path(tmp) / "tree"
        tree.mkdir()
        archive = subprocess.run(
            ["git", "-C", str(root), "archive", rev.base_sha],
            capture_output=True, check=True).stdout
        subprocess.run(["tar", "-x", "-C", str(tree)], input=archive, check=True)
        applied = subprocess.run(["git", "apply", "--whitespace=nowarn", "-"],
                                 cwd=tree, input=patch, capture_output=True)
        if applied.returncode != 0:
            raise EvidenceError("the bootstrap adapter does not apply to the "
                                "base: " + applied.stderr.decode()[-500:])
        for sub in ("config",):
            src = root / sub
            if src.is_dir() and not (tree / sub).exists():
                (tree / sub).symlink_to(src)
        ev = build_and_run(tree, builddir=Path(tmp) / "dist")
    prov = {"mode": "bootstrap",
            "adapterPatchSha256": hashlib.sha256(patch).hexdigest(),
            "adapterPaths": list(ADAPTER_PATHS),
            "baseSha": rev.base_sha,
            "examples": ev.examples,
            "extractorSha256": ev.executable_sha256,
            "note": "the pre-guard base was built with the candidate's "
                    "additive hooks laid onto it; its wire declarations, "
                    "versions and decoders are the base's own"}
    return ev.record, prov
