#!/usr/bin/env python3
"""The retained artifacts one production-defect issue quotes (#1438).

`tools/deflake_issue.py` files ONE tracker issue for a diagnosed
production defect, and an issue whose only log evidence is a
machine-local pathname is not reviewable: nobody but the machine that
measured it can open one. So this module reads the artifacts
`tools/probe_flake.py` retained for each non-PASS run and hands back
bounded, quotable excerpts — and refuses when there are none left,
rather than letting the route file on paths alone.

    python3 tools/test_deflake_diagnosis.py       # the deterministic gate

Its ownership runs from an accepted handoff to a list of evidence
blocks: failing-run discovery in role order, the descriptor-relative
traversal below each declared artifact root, the bounded tail reads, the
run and engine-log excerpts, every count and size bound above, and the
refusal when nothing reviewable remains. It renders nothing, publishes
nothing, and records nothing, so it imports neither the façade nor the
document, tracker or record owners.

Traversal, not resolution
-------------------------
What is found under the artifact root is QUOTED INTO A PUBLISHED ISSUE,
so the tree is walked component by component with `O_NOFOLLOW` at every
step below the root, and the engine directory is listed by DESCRIPTOR.
Only the root itself is opened by path: it is the anchor the producer
record declares, and `deflake_handoff.require_artifact_reference` has
already refused one inside a worktree. `O_NOFOLLOW` on the final file
alone would not be enough — a symlinked `engine` directory, or a run
directory substituted after #1437's own canonical-path check passed,
would have every listing and open land somewhere else and publish
whatever regular files live there as this probe's failure evidence.
Refusing at every component closes that, and closes the component race
with it: nothing is re-resolved by path after the first open. Each file
is then opened `O_NONBLOCK` and required to be regular, so a FIFO
planted at one of those names cannot block the workflow on an open.
"""
from __future__ import annotations

import os
import stat
import sys
from pathlib import Path

# `tools/` carries no `__init__.py`, so it is an implicit namespace
# package: under the repository-root spelling `import
# tools.deflake_issue_evidence` this directory is NOT on `sys.path`, and
# the sibling imports below resolve only because the pre-split module
# put its own directory there first. Every owner in this family carries
# the same bootstrap ahead of its own sibling imports.
sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
import deflake_handoff  # noqa: E402
import probe_flake  # noqa: E402

ROLES = deflake_handoff.ROLES
NonSuccess = deflake_handoff.NonSuccess


# How much retained log evidence one issue carries. Bounded on every
# axis, because the body is a review surface and a whole engine log is
# not one: at most this many non-PASS runs, this many files from each,
# and this many trailing lines and characters from each file. The TAIL
# is what is quoted — a probe that fails aborts, so the end of its
# stream is where the failure is.
MAX_EVIDENCE_RUNS = 3
MAX_EVIDENCE_FILES_PER_RUN = 4
MAX_EXCERPT_LINES = 24
MAX_EXCERPT_CHARS = 2400
# Read only the tail off disk. An engine log can be large and only its
# end is quoted, so the whole file is never held in memory.
MAX_READ_BYTES = 262144


# The per-run artifact layout `probe_flake.measure` creates, in the
# order a reader wants it: what the probe declared, what it printed, and
# then whatever the engine logged.
RUN_EVIDENCE_FILES = ("events.jsonl", "stdout.txt")
ENGINE_LOG_DIR = "engine"


def _open_directory(name, *, dir_fd=None):
    """One directory component, opened WITHOUT following a symlink."""
    try:
        return os.open(name, os.O_RDONLY | os.O_DIRECTORY | os.O_NOFOLLOW,
                       dir_fd=dir_fd)
    except (OSError, ValueError):
        return None


def open_run_directory(artifact_root: str, run_dir: str):
    """A descriptor for one retained run directory, or None.

    Walked component by component from the DECLARED artifact root, with
    `O_NOFOLLOW` on every step below it. Only the root itself is opened
    by path: it is the anchor the producer record declares, and
    `require_artifact_reference` has already refused one inside any
    worktree.

    Everything under it is traversed by descriptor because what is
    found here is QUOTED INTO A PUBLISHED ISSUE. `O_NOFOLLOW` on the
    final file alone is not enough — a symlinked `engine` directory, or
    a symlinked run directory, would have `os.listdir` and every open
    below it land somewhere else entirely, and whatever regular files
    live there would be read and published as this probe's failure
    evidence. Refusing at every component closes that, and closes the
    component race with it: nothing is re-resolved by path after the
    first open.
    """
    try:
        relative = Path(run_dir).relative_to(Path(artifact_root))
    except ValueError:
        # #1437's artifact topology puts every run directory under the
        # root its own result declares, so this is unreachable for an
        # accepted handoff — and a path that is not under the anchor is
        # one this has no safe way to walk.
        return None
    fd = _open_directory(artifact_root)
    for part in relative.parts:
        if fd is None:
            return None
        nxt = _open_directory(part, dir_fd=fd)
        os.close(fd)
        fd = nxt
    return fd


def _tail_text(name: str, *, dir_fd) -> str | None:
    """The tail of one retained artifact, or None if it cannot be read.

    Opened by NAME within its own directory's descriptor and with
    `O_NOFOLLOW`, so neither the file nor any directory above it can be
    a symlink to somewhere this has no business quoting.
    `O_NONBLOCK` and the regular-file check are the other half: a FIFO
    planted at one of these names would otherwise block the open until
    someone wrote to it, and a directory would read as an error late
    rather than a skip early. Decoding replaces undecodable bytes
    instead of raising — a macOS engine log carries GLFW's junk, and
    evidence that exists must not be dropped because one byte is not
    UTF-8.
    """
    try:
        fd = os.open(name, os.O_RDONLY | os.O_NOFOLLOW | os.O_NONBLOCK,
                     dir_fd=dir_fd)
    except (OSError, ValueError):
        return None
    try:
        if not stat.S_ISREG(os.fstat(fd).st_mode):
            return None
        size = os.lseek(fd, 0, os.SEEK_END)
        start = max(0, size - MAX_READ_BYTES)
        os.lseek(fd, start, os.SEEK_SET)
        raw = os.read(fd, MAX_READ_BYTES)
    except OSError:
        return None
    finally:
        os.close(fd)
    text = raw.decode("utf-8", errors="replace")
    if start:
        # The first line of a mid-file read is a fragment; dropping it
        # is what keeps every quoted line a whole one.
        text = text.split("\n", 1)[1] if "\n" in text else ""
    return text


def excerpt(path: Path, name: str, *, dir_fd) -> dict | None:
    """One bounded, quotable excerpt of one retained artifact.

    `path` is the label the issue prints so a reader can find the whole
    artifact; `name` and `dir_fd` are what is actually opened.
    """
    text = _tail_text(name, dir_fd=dir_fd)
    if text is None:
        return None
    lines = [line for line in text.splitlines() if line.strip()]
    if not lines:
        return None
    kept = lines[-MAX_EXCERPT_LINES:]
    body = "\n".join(kept)
    clipped = len(kept) < len(lines)
    if len(body) > MAX_EXCERPT_CHARS:
        body = body[-MAX_EXCERPT_CHARS:]
        clipped = True
    return {"path": str(path), "lines": len(kept), "clipped": clipped,
            "text": body}


def run_excerpts(artifact_root: str, run_dir: str) -> list:
    """The bounded excerpts one retained run directory yields, in order.

    The protocol stream first — it is what the checks were scored from —
    then the probe's own stdout, then whatever the engine logged. The
    engine directory is listed rather than guessed at, because its file
    names come from the probe being measured; it is listed BY
    DESCRIPTOR, so a symlink standing in for it reaches nothing.
    """
    excerpts: list = []
    run_fd = open_run_directory(artifact_root, run_dir)
    if run_fd is None:
        return excerpts
    display = Path(run_dir)
    try:
        for name in RUN_EVIDENCE_FILES:
            if len(excerpts) >= MAX_EVIDENCE_FILES_PER_RUN:
                return excerpts
            found = excerpt(display / name, name, dir_fd=run_fd)
            if found is not None:
                excerpts.append(found)
        engine_fd = _open_directory(ENGINE_LOG_DIR, dir_fd=run_fd)
        if engine_fd is None:
            return excerpts
        try:
            names = sorted(os.listdir(engine_fd))
        except OSError:
            names = []
        try:
            for name in names:
                if len(excerpts) >= MAX_EVIDENCE_FILES_PER_RUN:
                    break
                found = excerpt(display / ENGINE_LOG_DIR / name, name,
                                dir_fd=engine_fd)
                if found is not None:
                    excerpts.append(found)
        finally:
            os.close(engine_fd)
    finally:
        os.close(run_fd)
    return excerpts


def failing_runs(handoff) -> list:
    """Every non-PASS run this attempt retained, in role order.

    `probe_flake.measure` deletes a run's directory the moment it passes
    and keeps every unsuccessful one, so these are exactly the runs with
    something to read. A harness-error run is deliberately not among
    them: it is kept out of `runs`, and a measurement carrying one is
    refused by `require_supported` before any evidence is collected.
    """
    found = []
    for role in ROLES:
        measurement = handoff.measurement(role)
        if measurement is None:
            continue
        for run in measurement.result["runs"]:
            if run["outcome"] == probe_flake.RUN_PASS:
                continue
            directory = run.get("artifact_dir")
            if not directory:
                continue
            found.append({"role": role, "index": run["index"],
                          "outcome": run["outcome"],
                          "artifact_dir": directory,
                          # The anchor its own measurement declared, so
                          # the walk below starts from a path the
                          # producer record vouches for rather than from
                          # whatever the run directory's parents are
                          # today.
                          "artifact_root": measurement.result[
                              "artifact_root"]})
    return found


def collect_evidence(handoff) -> list:
    """The bounded excerpts this issue will carry, or the refusal.

    An issue whose only log evidence is a machine-local pathname is not
    reviewable — the approved correction says so explicitly — so an
    attempt whose retained artifacts have all been pruned is refused
    here, BEFORE anything is published, rather than filed on paths
    alone.
    """
    blocks = []
    for run in failing_runs(handoff)[:MAX_EVIDENCE_RUNS]:
        excerpts = run_excerpts(run["artifact_root"], run["artifact_dir"])
        if excerpts:
            blocks.append({**run, "excerpts": excerpts})
    if not blocks:
        raise NonSuccess(
            f"no retained artifact of this attempt could be read, so the "
            f"issue would carry machine-local pathnames and nothing a "
            f"reviewer can open; the evidence is what makes the filed issue "
            f"reviewable, so re-measure the probe with "
            f"`python3 tools/probe_flake.py --probe {handoff.probe}` and "
            f"hand off the fresh artifacts rather than filing on paths")
    return blocks
