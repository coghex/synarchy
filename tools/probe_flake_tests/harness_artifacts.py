#!/usr/bin/env python3
"""Artifact roots, their refusals, and the durable failure-evidence rule
(#2087, #2931).
"""
from __future__ import annotations

import os
import tempfile
from pathlib import Path

from .support import probe_engine, probe_flake
from .support import SyntheticTree, run_synthetic, expect_raises, skip, expect

import probe_evidence  # noqa: E402

def test_artifacts() -> None:
    print("\n-- artifacts --")
    with SyntheticTree() as tree:
        m = run_synthetic(tree, "pass", runs=2)
        expect(not m.invocation_dir.exists(),
               "raw artifacts for successful runs are deleted, and the "
               "invocation directory they left empty goes with them")
        expect(m.retained_artifacts() == [],
               "a clean measurement retains nothing")

        m = run_synthetic(tree, "fail", runs=1)
        kept = m.runs[0].artifact_dir
        expect(kept is not None and kept.exists(),
               "a FAIL run's artifacts are retained")
        expect(kept is not None and (kept / "stdout.txt").exists()
               and (kept / "events.jsonl").exists()
               and (kept / "engine" / "engine.log").exists(),
               "stdout, protocol events and every engine log are retained")
        expect(kept is not None and
               "-N4" in (kept / "engine" / "engine.log").read_text(),
               "the engine log proves the probe received +RTS -N4 -RTS")

        m = run_synthetic(tree, "hang", runs=1, timeout=3.0)
        expect(m.runs[0].artifact_dir is not None
               and m.runs[0].artifact_dir.exists(),
               "a TIMEOUT run's artifacts are retained")

        os.environ["SYNTHETIC_RAW_PATH"] = str(tree.root / "bad.jsonl")
        (tree.root / "bad.jsonl").write_text("not json\n", encoding="utf-8")
        try:
            m = run_synthetic(tree, "raw", runs=1)
        finally:
            os.environ.pop("SYNTHETIC_RAW_PATH", None)
        kept = list(m.invocation_dir.iterdir())
        expect(kept and (kept[0] / "events.jsonl").exists(),
               "a harness-error run's artifacts are retained for inspection")

        with _failure_root_env(tree) as durable:
            expect(probe_flake.default_artifact_root() == durable,
                   "the default artifact root is the durable failure root")

        # The lease namespace is machine-wide: overriding --artifact-root
        # must not move it, or two harnesses could lease the same port.
        before = probe_flake.LEASE_ROOT
        run_synthetic(tree, "pass", runs=1,
                      artifact_root=tree.artifact_root / "elsewhere")
        expect(probe_flake.LEASE_ROOT == before,
               "an --artifact-root override never moves the port-lease namespace")

        expect_raises(probe_flake.Rejection,
                      lambda: probe_flake.check_artifact_root(
                          Path(probe_engine.REPO_ROOT) / "artifacts"),
                      "an artifact root inside a working tree is refused",
                      "inside the working tree")

        # An unusable root is a clean pre-execution rejection, not a
        # traceback: /dev/null is a character device, so nothing can be
        # created beneath it.
        expect_raises(probe_flake.Rejection,
                      lambda: probe_flake.check_artifact_root(
                          Path("/dev/null/probe-artifacts")),
                      "an uncreatable artifact root is rejected, not a crash",
                      "cannot be created")
        rc = probe_flake.main(["--probe", "synthetic", "--runs", "1",
                               "--artifact-root", "/dev/null/probe-artifacts"])
        expect(rc == probe_flake.EXIT_REJECTED,
               "an uncreatable artifact root exits with the rejection code")
        # Unlike the ownership cases above, this one cannot be rebuilt
        # for root at all: no mode makes a directory unwritable to uid
        # 0, so there is nothing to construct and the check under test
        # is correct to let it through. A clear skip, never a failure.
        if os.getuid() == 0:
            skip("running as root, so no directory mode can make an "
                 "artifact root unwritable to us")
        else:
            unwritable = tree.artifact_root / "readonly"
            unwritable.mkdir()
            unwritable.chmod(0o500)
            try:
                expect_raises(probe_flake.Rejection,
                              lambda: probe_flake.check_artifact_root(
                                  unwritable / "under"),
                              "an unwritable artifact root is rejected",
                              "cannot be created")
            finally:
                unwritable.chmod(0o700)


def test_no_tmpdir_default() -> None:
    print("\n-- artifact root with no TMPDIR --")
    saved = {name: os.environ.get(name)
             for name in ("TMPDIR", probe_evidence.ENV_ROOT)}
    os.environ.pop("TMPDIR", None)
    os.environ.pop(probe_evidence.ENV_ROOT, None)
    try:
        # tempfile caches its answer; clear it so nothing stale decides.
        tempfile.tempdir = None
        root = probe_flake.default_artifact_root()
        expect(root.is_absolute() and str(root) != "/synarchy-probe-flake",
               "with no TMPDIR the default is still an absolute directory")
        expect(root == Path.home() / ".local" / "state" / "synarchy"
               / "probe-failures",
               "the default is ~/.local/state/synarchy/probe-failures, "
               "which a reboot or a per-session TMPDIR cannot clear")
    finally:
        tempfile.tempdir = None
        for name, value in saved.items():
            if value is not None:
                os.environ[name] = value


import contextlib  # noqa: E402


@contextlib.contextmanager
def _failure_root_env(tree, keep=None):
    """Point the durable root (and optionally the keep count) at the tree."""
    names = (probe_evidence.ENV_ROOT, probe_evidence.ENV_KEEP)
    saved = {name: os.environ.get(name) for name in names}
    durable = tree.artifact_root / "durable"
    os.environ[probe_evidence.ENV_ROOT] = str(durable)
    os.environ.pop(probe_evidence.ENV_KEEP, None)
    if keep is not None:
        os.environ[probe_evidence.ENV_KEEP] = str(keep)
    try:
        yield durable
    finally:
        for name, value in saved.items():
            if value is None:
                os.environ.pop(name, None)
            else:
                os.environ[name] = value


def test_failure_evidence_root() -> None:
    print("\n-- durable failure evidence (#2931) --")
    with SyntheticTree() as tree, _failure_root_env(tree) as durable:
        m = run_synthetic(tree, "fail", runs=1, artifact_root=None)
        kept = m.runs[0].artifact_dir
        expect(m.artifact_root == durable.resolve() / "synthetic",
               "a default measurement keeps its probe's invocations in "
               "<durable root>/<probe>/")
        expect(kept is not None and kept.exists()
               and m.invocation_dir.parent == m.artifact_root,
               "the failed run is kept, the invocation a DIRECT child of the "
               "artifact root the result document names")
        expect((m.invocation_dir / probe_evidence.MARKER).exists(),
               "a kept invocation is marked finalized, which is what lets it "
               "(and only it) be pruned later")

        m = run_synthetic(tree, "pass", runs=2, artifact_root=None)
        expect(not m.invocation_dir.exists(),
               "a passing measurement leaves nothing behind at the durable "
               "root")

        other = tree.artifact_root / "operator-chosen"
        m = run_synthetic(tree, "fail", runs=1, artifact_root=other)
        expect(m.artifact_root == other.resolve()
               and m.invocation_dir.parent == other.resolve(),
               "--artifact-root beats the environment variable and is used "
               "as given, with no <probe> directory added")

        with _failure_root_env(tree):
            os.environ[probe_evidence.ENV_ROOT] = str(
                Path(probe_engine.REPO_ROOT) / "evidence")
            expect_raises(
                probe_flake.Rejection,
                lambda: run_synthetic(tree, "pass", artifact_root=None),
                "a durable root inside a worktree is refused before a probe "
                "runs", "inside the working tree")

        with _failure_root_env(tree, keep="zero"):
            expect_raises(
                probe_flake.Rejection,
                lambda: run_synthetic(tree, "pass", artifact_root=None),
                "an unusable keep count is refused, not defaulted",
                probe_evidence.ENV_KEEP)


def test_failure_evidence_pruning() -> None:
    print("\n-- pruning keeps the newest failures (#2931) --")
    with SyntheticTree() as tree, _failure_root_env(tree, keep=2) as durable:
        names = []
        for _ in range(4):
            m = run_synthetic(tree, "fail", runs=1, artifact_root=None)
            names.append(m.invocation_dir.name)
        probe_dir = durable.resolve() / "synthetic"
        left = sorted(entry.name for entry in probe_dir.iterdir())
        expect(left == sorted(names[-2:]),
               f"only the newest 2 failed invocations remain ({left})")

        active = probe_dir / "synthetic-active-no-marker"
        (active / "run-001").mkdir(parents=True)
        outside = tree.artifact_root / "outside"
        outside.mkdir()
        (outside / probe_evidence.MARKER).write_text(
            '{"schema": "probe-evidence/v1", "finalized_at": "0"}\n')
        (probe_dir / "linked").symlink_to(outside)
        for _ in range(2):
            run_synthetic(tree, "fail", runs=1, artifact_root=None)
        expect(active.exists(),
               "an unmarked (active or foreign) directory is never pruned")
        expect(outside.exists() and (outside / probe_evidence.MARKER).exists(),
               "a symlink is skipped, never followed out of the owned tree")

        explicit = tree.artifact_root / "explicit"
        for _ in range(3):
            run_synthetic(tree, "fail", runs=1, artifact_root=explicit)
        expect(len(list(explicit.iterdir())) == 3,
               "an operator-chosen --artifact-root is never pruned")


def test_a_harness_error_reports_an_archive_failure() -> None:
    print("\n-- a HARNESS_ERROR run reports a failed engine-log archive (#2931)")
    import io
    import shutil
    with SyntheticTree() as tree, _failure_root_env(tree):
        fixed = tree.root / "fixed-engine.log"
        bad = tree.root / "bad.jsonl"
        bad.write_text("not json\n", encoding="utf-8")
        os.environ["SYNTHETIC_FIXED_LOG"] = str(fixed)
        os.environ["SYNTHETIC_RAW_PATH"] = str(bad)
        real = shutil.copyfile
        seen = io.StringIO()

        def refuse(*args, **kwargs):
            raise OSError("synthetic: no space left")

        shutil.copyfile = refuse
        try:
            import contextlib
            with contextlib.redirect_stderr(seen):
                m = run_synthetic(tree, "raw", runs=1, artifact_root=None)
        finally:
            shutil.copyfile = real
            os.environ.pop("SYNTHETIC_FIXED_LOG", None)
            os.environ.pop("SYNTHETIC_RAW_PATH", None)
        expect(m.status == "harness-error" and m.error_run is not None
               and m.error_run.artifact_dir.exists(),
               "the malformed run is a retained harness error")
        text = seen.getvalue()
        expect(str(fixed) in text and "still at" in text
               and "no space left" in text,
               f"the failed archive names the log, the error and where the "
               f"original still is ({text!r})")
        expect(fixed.read_text() == "engine output\n",
               "and the original is untouched")


def test_failure_evidence_pruning_counts_failed_runs() -> None:
    print("\n-- the keep limit counts failed runs, not invocations (#2931)")
    with SyntheticTree() as tree, _failure_root_env(tree, keep=2) as durable:
        m = run_synthetic(tree, "fail", runs=4, artifact_root=None)
        left = sorted(p.name for p in m.invocation_dir.iterdir()
                      if p.name.startswith("run-"))
        expect(left == ["run-003", "run-004"],
               f"a four-failure batch keeps only its newest two runs ({left})")
        m2 = run_synthetic(tree, "fail", runs=1, artifact_root=None)
        probe_dir = durable.resolve() / "synthetic"
        runs = sorted(str(p.relative_to(probe_dir))
                      for p in probe_dir.glob("*/run-*"))
        expect(len(runs) == 2 and runs[-1].startswith(m2.invocation_dir.name),
               f"the limit holds across batches too ({runs})")
        expect(m.invocation_dir.exists() and (m.invocation_dir /
               probe_evidence.MARKER).exists(),
               "an invocation that still holds a run keeps its marker")
        m3 = run_synthetic(tree, "fail", runs=2, artifact_root=None)
        expect(not m.invocation_dir.exists() and not m2.invocation_dir.exists()
               and sorted(p.name for p in m3.invocation_dir.iterdir()
                          if p.name.startswith("run-")) == ["run-001", "run-002"],
               "an invocation whose last run is pruned goes with it")


def test_a_failed_removal_after_strict_cleanup_fails_the_run() -> None:
    print("\n-- a strict cleanup the harness cannot finish is not a pass (#2931)")
    import io
    import contextlib
    import shutil
    with SyntheticTree() as tree, _failure_root_env(tree):
        fixed = tree.root / "fixed-engine.log"
        os.environ["SYNTHETIC_FIXED_LOG"] = str(fixed)
        real = shutil.rmtree

        def refuse(path, *args, **kwargs):
            if "run-" in str(path):
                raise OSError("synthetic: directory busy")
            return real(path, *args, **kwargs)

        try:
            for mode, strict in (("scratch_strict", True),
                                 ("scratch_pass", False)):
                seen = io.StringIO()
                shutil.rmtree = refuse
                try:
                    with contextlib.redirect_stderr(seen):
                        m = run_synthetic(tree, mode, runs=1,
                                          artifact_root=None)
                finally:
                    shutil.rmtree = real
                outcome = m.runs[0].outcome
                if strict:
                    expect(outcome == probe_flake.RUN_FAIL
                           and m.runs[0].artifact_dir is not None
                           and "busy" in seen.getvalue()
                           and "[FAIL]" in (m.runs[0].artifact_dir
                                            / "stdout.txt").read_text(),
                           f"retire() then an undeletable directory fails "
                           f"the run, keeps it and says why ({outcome})")
                else:
                    expect(outcome == probe_flake.RUN_PASS,
                           "a lenient discard_scratch keeps its old meaning: "
                           "a leftover is not a failure")
        finally:
            os.environ.pop("SYNTHETIC_FIXED_LOG", None)
            shutil.rmtree = real


def test_probe_owned_scratch_is_kept_for_the_harness() -> None:
    print("\n-- probe-owned cleanup defers to the harness (#2931) --")
    with SyntheticTree() as tree, _failure_root_env(tree) as durable:
        fixed = tree.root / "fixed-engine.log"
        os.environ["SYNTHETIC_FIXED_LOG"] = str(fixed)
        try:
            m = run_synthetic(tree, "scratch_fail", runs=1, artifact_root=None)
            kept = m.runs[0].artifact_dir
            scratch = (sorted((kept / probe_evidence.RETIRED_DIR)
                                  .glob("scratch_*")) if kept else [])
            expect(len(scratch) == 1
                   and (scratch[0] / "evidence.txt").read_text()
                   == "the save slot\n",
                   "the probe's own scratch tree survives its own cleanup "
                   "under a failing run (retired into the attempt, so the "
                   "path is gone to the probe)")
            logs = sorted((kept / probe_evidence.ENGINE_LOG_DIR).glob("*"))
            expect(len(logs) == 1 and logs[0].read_text() == "engine output\n",
                   "an engine log at a fixed path is copied into the kept run")

            fixed.unlink()
            m = run_synthetic(tree, "scratch_pass", runs=1, artifact_root=None)
            expect(not m.invocation_dir.exists(),
                   "a passing run's scratch goes with it: nothing is left "
                   "behind at the durable root")
        finally:
            os.environ.pop("SYNTHETIC_FIXED_LOG", None)


TESTS = (
    test_artifacts,
    test_no_tmpdir_default,
    test_failure_evidence_root,
    test_failure_evidence_pruning,
    test_failure_evidence_pruning_counts_failed_runs,
    test_a_harness_error_reports_an_archive_failure,
    test_a_failed_removal_after_strict_cleanup_fails_the_run,
    test_probe_owned_scratch_is_kept_for_the_harness,
)
