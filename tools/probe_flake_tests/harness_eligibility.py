#!/usr/bin/env python3
"""Eligibility and descriptor-mismatch rejection, before anything runs (#2087).

The load-bearing property is that each rejection starts no subprocess at
all, which the synthetic probe's marker file witnesses.
"""
from __future__ import annotations

import os
import shutil
import tempfile
import textwrap
from pathlib import Path

from .support import ci_probes, probe_flake, probe_protocol, probe_runner_registry
from .support import SYNTHETIC_PROBE, SyntheticTree, TOOLS_DIR, expect_raises, expect

def test_eligibility() -> None:
    print("\n-- eligibility rejection (no probe is ever started) --")
    marker = Path(tempfile.mkdtemp(prefix="probe-flake-marker-")) / "ran.txt"
    os.environ["SYNTHETIC_RAN_MARKER"] = str(marker)
    try:
        with SyntheticTree(keys=("synthetic", "legacyprobe", "cieligible")) as tree:
            probe_flake.PROTOCOL_PROBES = {
                "synthetic": probe_protocol.PROTOCOL_VERSION}
            ci_probes.CI_ELIGIBLE = {"cieligible"}

            expect_raises(probe_flake.Rejection,
                          lambda: probe_flake.resolve_probe("nosuchprobe"),
                          "an unknown probe key is rejected", "unknown probe")
            expect_raises(probe_flake.Rejection,
                          lambda: probe_flake.resolve_probe("cieligible"),
                          "a CI-eligible probe is rejected", "CI-eligible")
            expect_raises(probe_flake.Rejection,
                          lambda: probe_flake.resolve_probe("legacyprobe"),
                          "a legacy probe is rejected by name",
                          "requires migration to probe-result/v1")
            expect(probe_flake.resolve_probe("synthetic") == "synthetic_probe.py",
                   "a migrated probe resolves to its script")

            # The load-bearing part: none of those rejections started the
            # probe. The synthetic script appends to the marker file the
            # moment it runs anything past --describe.
            for key in ("nosuchprobe", "cieligible", "legacyprobe"):
                try:
                    probe_flake.measure(key, 1, artifact_root=tree.artifacts())
                except probe_flake.Rejection:
                    pass
            expect(not marker.exists(),
                   "rejecting unknown/CI-eligible/legacy probes starts no "
                   "subprocess that could boot an engine")

            expect_raises(probe_flake.Rejection,
                          lambda: probe_flake.measure(
                              "synthetic", 0, artifact_root=tree.artifacts()),
                          "a non-positive run count is rejected",
                          "positive count")
            expect_raises(probe_flake.Rejection,
                          lambda: probe_flake.measure(
                              "synthetic", -3, artifact_root=tree.artifacts()),
                          "a negative run count is rejected", "positive count")
            expect_raises(probe_flake.Rejection,
                          lambda: probe_flake.measure(
                              "synthetic", 1, rts_caps=0,
                              artifact_root=tree.artifacts()),
                          "a non-positive RTS capability count is rejected",
                          "positive capability count")
            expect(not marker.exists(),
                   "count validation also happens before any probe starts")
    finally:
        os.environ.pop("SYNTHETIC_RAN_MARKER", None)
        shutil.rmtree(marker.parent, ignore_errors=True)


def test_descriptor_mismatch_rejection() -> None:
    print("\n-- descriptor mismatch rejection --")
    with SyntheticTree(keys=("synthetic",)) as tree:
        # A probe registered under one key whose descriptor names another.
        impostor = tree.root / "tools" / "impostor_probe.py"
        impostor.write_text(
            SYNTHETIC_PROBE.format(tools=TOOLS_DIR, key="somethingelse"),
            encoding="utf-8")
        probe_runner_registry.PROBES = tree.probes + [
            ("impostor", "impostor_probe.py", "synthetic"),
            ("v2probe", "v2_probe.py", "synthetic"),
            ("noflag", "noflag_probe.py", "synthetic")]
        probe_flake.PROTOCOL_PROBES = {
            k: probe_protocol.PROTOCOL_VERSION
            for k in ("synthetic", "impostor", "v2probe", "noflag")}
        (tree.root / "tools" / "v2_probe.py").write_text(textwrap.dedent('''\
            import sys, json
            if "--describe" in sys.argv:
                print(json.dumps({"protocol": "probe-result/v2",
                                  "probe": "v2probe",
                                  "checks": [{"id": "alpha", "label": "a"}]}))
                raise SystemExit(0)
        '''), encoding="utf-8")
        (tree.root / "tools" / "noflag_probe.py").write_text(textwrap.dedent('''\
            import sys
            print("I do not know --describe", file=sys.stderr)
            raise SystemExit(2)
        '''), encoding="utf-8")

        expect_raises(probe_flake.Rejection,
                      lambda: probe_flake.fetch_descriptor(
                          "impostor", "impostor_probe.py"),
                      "a descriptor naming the wrong probe key is rejected",
                      "was requested")
        expect_raises(probe_flake.Rejection,
                      lambda: probe_flake.fetch_descriptor(
                          "v2probe", "v2_probe.py"),
                      "an unsupported protocol version is rejected",
                      "supports only")
        expect_raises(probe_flake.Rejection,
                      lambda: probe_flake.fetch_descriptor(
                          "noflag", "noflag_probe.py"),
                      "a probe with no --describe path is rejected",
                      "does not implement")


TESTS = (
    test_eligibility,
    test_descriptor_mismatch_rejection,
)
