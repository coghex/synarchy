#!/usr/bin/env python3
"""Artifact roots, their refusals and the no-`TMPDIR` default (#2087).
"""
from __future__ import annotations

import os
import tempfile
from pathlib import Path

from .support import probe_engine, probe_flake
from .support import SyntheticTree, run_synthetic, expect_raises, skip, expect

def test_artifacts() -> None:
    print("\n-- artifacts --")
    with SyntheticTree() as tree:
        m = run_synthetic(tree, "pass", runs=2)
        run_dirs = list(m.invocation_dir.iterdir())
        expect(run_dirs == [],
               "raw artifacts for successful runs are deleted")
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

        expect(probe_flake.default_artifact_root() ==
               Path(tempfile.gettempdir()) / "synarchy-probe-flake",
               "the default artifact root resolves through the platform temp dir")

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
    saved = os.environ.get("TMPDIR")
    os.environ.pop("TMPDIR", None)
    try:
        # tempfile caches its answer, so ask the same way probe_flake does
        # after clearing the cache.
        tempfile.tempdir = None
        root = probe_flake.default_artifact_root()
        expect(root.is_absolute() and str(root) != "/synarchy-probe-flake",
               "with no TMPDIR the default is the platform temp dir, not /")
    finally:
        tempfile.tempdir = None
        if saved is not None:
            os.environ["TMPDIR"] = saved


TESTS = (
    test_artifacts,
    test_no_tmpdir_default,
)
