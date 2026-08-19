#!/usr/bin/env python3
"""Focused self-test for the probe-result protocol and flakiness harness (#1425).

Deterministic, engine-free and GPU-free: every "probe" here is a
synthetic script in a throwaway tree that writes a protocol event stream
and exits, so nothing boots Vulkan, generates a world, or runs a
registered probe. The real `tools/probe_protocol.py`,
`tools/probe_flake.py` and `tools/probe_census.py` are imported and
driven — with `run_probes.REPO_ROOT`/`PROBES`, `ci_probes.CI_ELIGIBLE`,
`probe_flake.PROTOCOL_PROBES` and `probe_flake.LEASE_ROOT` pointed at
the temp tree — so this exercises the shipped code paths rather than a
copy. `LEASE_ROOT` is the one deliberate redirection the module allows:
it is machine-wide precisely so an `--artifact-root` override cannot
split the lease namespace, and only this file may move it.

Mutation coverage is the point for the three parsers. Every descriptor,
event-stream and result-document rule is exercised from BOTH sides — a
document that must be accepted and a minimally mutated one that must be
rejected — because a validator that only ever sees valid input proves
nothing.

The manifest cases run against a self-owned fixture, never the real
`docs/probe_census.json`: that file is written only into the `docs-wip`
worktree and is deliberately never published, so a fresh checkout, a
fresh worktree and CI all lack it. The REAL manifest is validated by a
separate case that runs only when a `docs-wip` worktree resolves and
reports a clear skip otherwise.

Usage:
  python3 tools/test_probe_flake.py
Exit codes: 0 = all tests passed, 1 = one or more failed.
"""
from __future__ import annotations

import json
import os
import shutil
import subprocess
import sys
import tempfile
import textwrap
import threading
import time
from pathlib import Path

sys.path.insert(0, str(Path(__file__).resolve().parent))
import ci_probes  # type: ignore  # noqa: E402
import probe_census  # type: ignore  # noqa: E402
import probe_flake  # type: ignore  # noqa: E402
import probe_protocol  # type: ignore  # noqa: E402
import run_probes  # type: ignore  # noqa: E402

FAILURES: list[str] = []
SKIPS: list[str] = []


def expect(cond: bool, msg: str) -> None:
    if not cond:
        FAILURES.append(msg)
        print(f"  FAIL: {msg}")
    else:
        print(f"  OK:   {msg}")


def expect_raises(exc, fn, msg: str, substring: str | None = None) -> None:
    try:
        fn()
    except exc as error:
        if substring is not None and substring not in str(error):
            FAILURES.append(f"{msg} (raised {exc.__name__} but not about "
                            f"{substring!r}: {error})")
            print(f"  FAIL: {msg} — wrong message: {error}")
            return
        print(f"  OK:   {msg}")
        return
    except Exception as error:  # noqa: BLE001 - a wrong exception is a failure
        FAILURES.append(f"{msg} (raised {type(error).__name__}: {error})")
        print(f"  FAIL: {msg} — raised {type(error).__name__}: {error}")
        return
    FAILURES.append(f"{msg} (nothing raised)")
    print(f"  FAIL: {msg} — nothing raised")


def skip(msg: str) -> None:
    SKIPS.append(msg)
    print(f"  SKIP: {msg}")


# ==========================================================================
# Synthetic protocol probe
# ==========================================================================
# A stand-in for a migrated probe: it declares a fixed check sequence via
# --describe (no engine, no subprocess of its own) and then replays a
# scripted event script into the harness-supplied event stream. Every
# behavior the harness has to reconcile — a clean pass, a failed check,
# a nonzero exit with partial checks, an early abort, a hang that must
# time out, a malformed line, a forbidden stdout marker — is one
# SYNTHETIC_SCRIPT mode, so no real probe is ever run.
SYNTHETIC_PROBE = textwrap.dedent('''\
    import argparse, json, os, sys, time
    sys.path.insert(0, {tools!r})
    import probe_protocol

    CHECKS = [("alpha", "the first check"),
              ("beta", "the second check"),
              ("gamma", "the third check")]
    DESCRIPTOR = probe_protocol.build_descriptor({key!r}, CHECKS)

    ap = argparse.ArgumentParser()
    ap.add_argument("--port", type=int, default=9999)
    ap.add_argument("--describe", action="store_true")
    args = ap.parse_args()
    if args.describe:
        print(DESCRIPTOR.to_json())
        raise SystemExit(0)

    # Prove the harness handed over its wiring, and that a rejected probe
    # never gets this far.
    marker = os.environ.get("SYNTHETIC_RAN_MARKER")
    if marker:
        open(marker, "a").write("ran\\n")

    mode = os.environ.get("SYNTHETIC_MODE", "pass")
    rep = probe_protocol.reporter_from_env(DESCRIPTOR)
    caps = os.environ.get(probe_protocol.ENV_RTS_CAPS)
    logdir = os.environ.get(probe_protocol.ENV_ENGINE_LOG_DIR)
    if logdir:
        open(rep.engine_log_path("engine.log", "/tmp/unused.log"), "w").write(
            "rts=" + " ".join(rep.engine_args()) + "\\n")

    if mode == "raw":
        # Bypass the reporter entirely to forge malformed streams.
        path = os.environ["SYNTHETIC_RAW_PATH"]
        open(os.environ[probe_protocol.ENV_EVENTS], "w").write(
            open(path).read())
        raise SystemExit(int(os.environ.get("SYNTHETIC_RC", "0")))

    if mode == "marker":
        rep.check("alpha", True, "the first check")
        print("  [pass] a forbidden second result channel")
        raise SystemExit(0)

    if mode == "brackets_ok":
        rep.check("alpha", True, "the first check")
        rep.check("beta", True, "the second check")
        rep.check("gamma", True, "the third check")
        print("[1, 2, 3] is data, not a marker")
        print('{{"a": [1]}}')
        print("   [2, 3] leading whitespace then data")
        raise SystemExit(0)

    if mode == "abort":
        rep.check("alpha", True, "the first check")
        rep.abort("setup failed before the remaining checks")
        raise SystemExit(1)

    if mode == "fail":
        rep.check("alpha", True, "the first check")
        rep.check("beta", False, "the second check", {{"observed": "wrong"}})
        rep.check("gamma", True, "the third check")
        raise SystemExit(1)

    if mode == "nonzero_partial":
        rep.check("alpha", True, "the first check")
        rep.check("beta", True, "the second check")
        raise SystemExit(3)

    if mode == "diagnostics":
        rep.info("an informational note", {{"port": args.port}})
        rep.check("alpha", True, "the first check", {{"caps": caps}})
        rep.warn("a warning that is not a check")
        rep.check("beta", True, "the second check")
        rep.skip("a skipped optional step")
        rep.check("gamma", True, "the third check")
        raise SystemExit(0)

    if mode == "hang":
        rep.check("alpha", True, "the first check")
        rep.check("beta", True, "the second check")
        time.sleep(600)
        raise SystemExit(0)

    if mode == "bad_describe_key":
        raise SystemExit(0)

    rep.check("alpha", True, "the first check", {{"port": args.port}})
    rep.check("beta", True, "the second check")
    rep.check("gamma", True, "the third check")
    raise SystemExit(0)
''')

TOOLS_DIR = str(Path(__file__).resolve().parent)


class SyntheticTree:
    """A temp checkout whose `tools/` holds only synthetic probes."""

    def __init__(self, keys=("synthetic",)):
        self.root = Path(tempfile.mkdtemp(prefix="probe-flake-tree-"))
        # OUTSIDE the synthetic checkout: `check_artifact_root` refuses
        # any root inside a working tree, and the synthetic tree stands
        # in for one.
        self.artifact_root = Path(tempfile.mkdtemp(prefix="probe-flake-art-"))
        (self.root / "tools").mkdir()
        self.keys = keys
        for key in keys:
            script = self.root / "tools" / f"{key}_probe.py"
            script.write_text(
                SYNTHETIC_PROBE.format(tools=TOOLS_DIR, key=key),
                encoding="utf-8")
        self.probes = [(key, f"{key}_probe.py", "synthetic") for key in keys]
        self._saved: dict[str, object] = {}

    def __enter__(self):
        self._saved = {
            "REPO_ROOT": run_probes.REPO_ROOT,
            "PROBES": run_probes.PROBES,
            "CI_ELIGIBLE": ci_probes.CI_ELIGIBLE,
            "PROTOCOL_PROBES": probe_flake.PROTOCOL_PROBES,
            "LEASE_ROOT": probe_flake.LEASE_ROOT,
        }
        run_probes.REPO_ROOT = str(self.root)
        run_probes.PROBES = self.probes
        ci_probes.CI_ELIGIBLE = set()
        probe_flake.PROTOCOL_PROBES = {
            key: probe_protocol.PROTOCOL_VERSION for key in self.keys}
        probe_flake.LEASE_ROOT = self.root / "leases"
        return self

    def __exit__(self, *exc):
        run_probes.REPO_ROOT = self._saved["REPO_ROOT"]
        run_probes.PROBES = self._saved["PROBES"]
        ci_probes.CI_ELIGIBLE = self._saved["CI_ELIGIBLE"]
        probe_flake.PROTOCOL_PROBES = self._saved["PROTOCOL_PROBES"]
        probe_flake.LEASE_ROOT = self._saved["LEASE_ROOT"]
        shutil.rmtree(self.root, ignore_errors=True)
        shutil.rmtree(self.artifact_root, ignore_errors=True)
        return None

    def artifacts(self) -> Path:
        return self.artifact_root / "artifacts"


def run_synthetic(tree: SyntheticTree, mode: str, runs: int = 1, **kwargs):
    """Drive `probe_flake.measure` with SYNTHETIC_MODE set."""
    previous = os.environ.get("SYNTHETIC_MODE")
    os.environ["SYNTHETIC_MODE"] = mode
    try:
        return probe_flake.measure(
            kwargs.pop("probe", "synthetic"), runs,
            artifact_root=kwargs.pop("artifact_root", tree.artifacts()),
            **kwargs)
    finally:
        if previous is None:
            os.environ.pop("SYNTHETIC_MODE", None)
        else:
            os.environ["SYNTHETIC_MODE"] = previous


# ==========================================================================
# Descriptor
# ==========================================================================
GOOD_DESCRIPTOR = {
    "protocol": "probe-result/v1",
    "probe": "synthetic",
    "checks": [{"id": "alpha", "label": "the first"},
               {"id": "beta", "label": "the second"}],
}


def test_descriptor() -> None:
    print("\n-- descriptor --")
    d = probe_protocol.parse_descriptor(json.dumps(GOOD_DESCRIPTOR),
                                        expected_probe="synthetic")
    expect(d.probe == "synthetic" and d.ids == ("alpha", "beta"),
           "a well-formed descriptor parses with its declared order intact")
    expect(json.loads(d.to_json()) == GOOD_DESCRIPTOR,
           "a descriptor round-trips through to_json unchanged")

    # --- mutations, one rule at a time ---
    def mutate(**changes):
        doc = json.loads(json.dumps(GOOD_DESCRIPTOR))
        doc.update(changes)
        return json.dumps(doc)

    expect_raises(probe_protocol.ProtocolError,
                  lambda: probe_protocol.parse_descriptor("not json"),
                  "a non-JSON descriptor is a protocol error", "valid JSON")
    expect_raises(probe_protocol.ProtocolError,
                  lambda: probe_protocol.parse_descriptor("[]"),
                  "a non-object descriptor is a protocol error", "JSON object")
    expect_raises(probe_protocol.ProtocolError,
                  lambda: probe_protocol.parse_descriptor(
                      mutate(protocol="probe-result/v2")),
                  "an unsupported protocol version is rejected", "supports only")
    expect_raises(probe_protocol.ProtocolError,
                  lambda: probe_protocol.parse_descriptor(
                      json.dumps({k: v for k, v in GOOD_DESCRIPTOR.items()
                                  if k != "protocol"})),
                  "a descriptor with no protocol version is rejected",
                  "supports only")
    expect_raises(probe_protocol.ProtocolError,
                  lambda: probe_protocol.parse_descriptor(
                      json.dumps(GOOD_DESCRIPTOR), expected_probe="other"),
                  "a descriptor for the wrong probe key is rejected",
                  "was requested")
    expect_raises(probe_protocol.ProtocolError,
                  lambda: probe_protocol.parse_descriptor(mutate(probe="")),
                  "a descriptor with an empty probe key is rejected",
                  "non-empty string")
    expect_raises(probe_protocol.ProtocolError,
                  lambda: probe_protocol.parse_descriptor(mutate(checks=[])),
                  "a descriptor declaring no checks is rejected",
                  "declares no checks")
    expect_raises(probe_protocol.ProtocolError,
                  lambda: probe_protocol.parse_descriptor(mutate(checks={})),
                  "a non-list `checks` is rejected", "must be a list")
    expect_raises(probe_protocol.ProtocolError,
                  lambda: probe_protocol.parse_descriptor(mutate(checks=[
                      {"id": "alpha", "label": "one"},
                      {"id": "alpha", "label": "two"}])),
                  "duplicate check identifiers are rejected", "duplicate")
    expect_raises(probe_protocol.ProtocolError,
                  lambda: probe_protocol.parse_descriptor(mutate(checks=[
                      {"id": "alpha"}])),
                  "a check with no label is rejected", "`id` and `label`")
    expect_raises(probe_protocol.ProtocolError,
                  lambda: probe_protocol.parse_descriptor(mutate(checks=[
                      {"id": "alpha", "label": ""}])),
                  "a check with an empty label is rejected", "no label")

    # Stable identifiers may not carry runtime values. A DIGIT is the
    # only way one can get in, so the protocol's own prohibited
    # examples must be refused rather than merely discouraged.
    unstable = ("role_miner_60", "unit_4711", "unit4711", "alpha_1", "beta2",
                "Alpha", "unit-4711", "alpha 1", "9alpha", "alpha.beta", "",
                "_alpha", "alpha_", "alpha__beta")
    for bad in unstable:
        expect_raises(probe_protocol.ProtocolError,
                      lambda bad=bad: probe_protocol.build_descriptor(
                          "synthetic", [(bad, "label")]),
                      f"identifier {bad!r} is refused as unstable",
                      "stable check identifier")
    ok = probe_protocol.build_descriptor(
        "synthetic", [("alpha", "l"), ("phase_two", "l"), ("a_b_c", "l")])
    expect(ok.ids == ("alpha", "phase_two", "a_b_c"),
           "lowercase words joined by single underscores are accepted")


# ==========================================================================
# Event stream
# ==========================================================================
def _descriptor():
    return probe_protocol.build_descriptor(
        "synthetic", [("alpha", "a"), ("beta", "b"), ("gamma", "c")])


def _line(**payload) -> str:
    return json.dumps(payload) + "\n"


def test_event_stream() -> None:
    print("\n-- event stream --")
    d = _descriptor()

    stream = (_line(event="check", id="alpha", outcome="PASS") +
              _line(event="check", id="beta", outcome="FAIL",
                    detail={"observed": 3}) +
              _line(event="check", id="gamma", outcome="PASS"))
    events, outcomes = probe_protocol.parse_event_stream(stream, d)
    expect(outcomes == {"alpha": "PASS", "beta": "FAIL", "gamma": "PASS"},
           "a complete in-order stream reconciles every declared check")
    expect(any(isinstance(e, probe_protocol.CheckEvent) and
               e.detail == {"observed": 3} for e in events),
           "dynamic runtime values ride in detail beside a stable identifier")

    partial = _line(event="check", id="alpha", outcome="PASS")
    _events, outcomes = probe_protocol.parse_event_stream(partial, d)
    expect(outcomes == {"alpha": "PASS", "beta": "MISSING", "gamma": "MISSING"},
           "checks a stopped run never emitted reconcile to MISSING")
    _events, outcomes = probe_protocol.parse_event_stream("", d)
    expect(set(outcomes.values()) == {"MISSING"},
           "an empty stream leaves every declared check MISSING")

    diagnostics = (_line(event="diagnostic", level="INFO", message="note") +
                   _line(event="check", id="alpha", outcome="PASS") +
                   _line(event="diagnostic", level="WARN", message="careful") +
                   _line(event="diagnostic", level="SKIP", message="skipped"))
    events, outcomes = probe_protocol.parse_event_stream(diagnostics, d)
    expect(outcomes["alpha"] == "PASS" and outcomes["beta"] == "MISSING",
           "INFO/WARN/SKIP diagnostics carry no check outcome of their own")
    expect(sum(isinstance(e, probe_protocol.DiagnosticEvent) for e in events) == 3,
           "all three supported diagnostic levels parse")

    def bad(stream: str):
        return lambda: probe_protocol.parse_event_stream(stream, d)

    expect_raises(probe_protocol.ProtocolError,
                  bad('{"event": "check", "id": "alpha"'),
                  "a truncated final line is a protocol error", "truncated")
    expect_raises(probe_protocol.ProtocolError, bad("not json\n"),
                  "a malformed line is a protocol error", "malformed JSON")
    expect_raises(probe_protocol.ProtocolError, bad("[1, 2]\n"),
                  "a non-object event line is a protocol error",
                  "not a JSON object")
    expect_raises(probe_protocol.ProtocolError,
                  bad(_line(event="check", id="delta", outcome="PASS")),
                  "an undeclared check identifier is a protocol error",
                  "is not declared")
    expect_raises(probe_protocol.ProtocolError,
                  bad(_line(event="check", id="alpha", outcome="PASS") +
                      _line(event="check", id="alpha", outcome="PASS")),
                  "a duplicate check event is a protocol error", "duplicate")
    expect_raises(probe_protocol.ProtocolError,
                  bad(_line(event="check", id="beta", outcome="PASS")),
                  "a check arriving before its declared predecessor is an error",
                  "arrived before the declared check")
    expect_raises(probe_protocol.ProtocolError,
                  bad(_line(event="check", id="alpha", outcome="PASS") +
                      _line(event="check", id="gamma", outcome="PASS") +
                      _line(event="check", id="beta", outcome="PASS")),
                  "a mid-sequence skip then backfill is out of order",
                  "arrived before the declared check")
    expect_raises(probe_protocol.ProtocolError,
                  bad(_line(event="check", id="alpha", outcome="MISSING")),
                  "MISSING is never a reportable check outcome", "expected one of")
    expect_raises(probe_protocol.ProtocolError,
                  bad(_line(event="check", id="alpha", outcome="pass")),
                  "a lowercase outcome is a protocol error", "expected one of")
    expect_raises(probe_protocol.ProtocolError,
                  bad(_line(event="diagnostic", level="DEBUG", message="x")),
                  "an unsupported diagnostic level is a protocol error",
                  "is not one of")
    expect_raises(probe_protocol.ProtocolError,
                  bad(_line(event="diagnostic", level="INFO")),
                  "a diagnostic with no message is a protocol error",
                  "no string `message`")
    expect_raises(probe_protocol.ProtocolError,
                  bad(_line(event="progress", id="alpha")),
                  "an unknown event kind is unclassifiable", "unclassifiable")
    # A non-string `id` must be a PROTOCOL error, not a crash: an
    # unhashable one (`[]`, `{}`) would otherwise raise TypeError out of
    # the dictionary membership test, escape every handler, and
    # traceback the harness instead of producing a harness error.
    for value in ([], {}, 5, None, True, ["alpha"], {"id": "alpha"}):
        expect_raises(probe_protocol.ProtocolError,
                      bad(_line(event="check", id=value, outcome="PASS")),
                      f"check id {value!r} is a protocol error, not a crash",
                      "must be a string")
    # And nothing a probe can put on a line may leak a non-ProtocolError.
    hostile = [
        _line(event="check", id=[], outcome="PASS"),
        _line(event="check", id={}, outcome=[]),
        _line(event="diagnostic", level=[], message={}),
        _line(event=[], id="alpha"),
        _line(event="check", id="alpha", outcome="PASS", detail=[[]]),
        '{"event": {"nested": {"deep": [1, 2]}}}\n',
        '{"event": "check", "id": "\\ud800", "outcome": "PASS"}\n',
    ]
    for line in hostile:
        try:
            probe_protocol.parse_event_stream(line, d)
            leaked = "accepted"
        except probe_protocol.ProtocolError:
            leaked = None
        except Exception as error:  # noqa: BLE001
            leaked = f"{type(error).__name__}: {error}"
        expect(leaked is None,
               f"hostile event line {line.strip()[:48]!r} is a ProtocolError "
               f"({leaked})")
    # A present `detail` must be an OBJECT. Every falsey non-object is
    # its own case: a truthiness fallback would coerce each to `{}` and
    # let a malformed event be counted as a pass.
    for value in ("not-an-object", [], "", 0, False, None, 1, [1, 2]):
        expect_raises(probe_protocol.ProtocolError,
                      bad(_line(event="check", id="alpha", outcome="PASS",
                                detail=value)),
                      f"check detail {value!r} is a protocol error",
                      "must be an object")
        expect_raises(probe_protocol.ProtocolError,
                      bad(_line(event="diagnostic", level="INFO",
                                message="m", detail=value)),
                      f"diagnostic detail {value!r} is a protocol error",
                      "must be an object")
    # An ABSENT key is the only thing that means "no detail".
    _events, outcomes = probe_protocol.parse_event_stream(
        _line(event="check", id="alpha", outcome="PASS"), d)
    expect(outcomes["alpha"] == "PASS",
           "an absent detail key is accepted as no detail")
    events, _outcomes = probe_protocol.parse_event_stream(
        _line(event="check", id="alpha", outcome="PASS", detail={}), d)
    expect(events[0].detail == {},
           "an explicitly empty detail object is accepted")


def test_trusted_prefix() -> None:
    print("\n-- trusted prefix of a broken stream --")
    d = _descriptor()
    good = _line(event="check", id="alpha", outcome="PASS")
    for name, tail in (("malformed", "not json\n"),
                       ("truncated", '{"event": "che'),
                       ("duplicate", _line(event="check", id="alpha",
                                           outcome="PASS")),
                       ("unknown id", _line(event="check", id="delta",
                                            outcome="PASS")),
                       ("bad level", _line(event="diagnostic", level="DEBUG",
                                           message="x"))):
        events, outcomes, error = probe_protocol.scan_event_stream(good + tail, d)
        expect(error is not None,
               f"a {name} tail is still an error")
        expect(outcomes == {"alpha": "PASS", "beta": "MISSING",
                            "gamma": "MISSING"},
               f"the valid prefix before a {name} tail is preserved "
               f"(got {outcomes})")
        expect(len(events) == 1,
               f"only the trusted prefix's events survive a {name} tail")
    # A clean stream scans with no error at all.
    _events, outcomes, error = probe_protocol.scan_event_stream(good, d)
    expect(error is None and outcomes["alpha"] == "PASS",
           "a clean stream scans without an error")
    # An out-of-order FIRST line has no valid prefix to keep.
    _events, outcomes, error = probe_protocol.scan_event_stream(
        _line(event="check", id="gamma", outcome="PASS"), d)
    expect(error is not None and set(outcomes.values()) == {"MISSING"},
           "a fault on the first line leaves nothing salvageable")


def test_forbidden_markers() -> None:
    print("\n-- forbidden stdout markers --")
    caught = ["[PASS] a check", "[FAIL] a check", "[pass] lowercase",
              "  [INFO] indented", "\t[WARN] tabbed", "[SKIP] skipped",
              "[diag] a legacy diagnostic", "[whatever]", "[UNKNOWN] form"]
    for line in caught:
        expect(probe_protocol.forbidden_marker_lines(line) == [line],
               f"marker line {line!r} is detected")
    allowed = ['[1, 2, 3]', '{"a": [1]}', 'result: [PASS] mid-line',
               '[]', '[3] numeric', 'plain text', '[a,b] not word-like',
               '[PASS]x no separator', '["a"]']
    for line in allowed:
        expect(probe_protocol.forbidden_marker_lines(line) == [],
               f"non-marker line {line!r} is left alone")
    multi = "ok\n[PASS] one\nmore\n[FAIL] two\n"
    expect(probe_protocol.forbidden_marker_lines(multi) ==
           ["[PASS] one", "[FAIL] two"],
           "every marker line in a multi-line capture is reported")


# ==========================================================================
# Eligibility, before anything runs
# ==========================================================================
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
        run_probes.PROBES = tree.probes + [
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


# ==========================================================================
# Run reconciliation
# ==========================================================================
def test_reconciliation() -> None:
    print("\n-- run reconciliation --")
    with SyntheticTree() as tree:
        m = run_synthetic(tree, "pass", runs=2)
        expect(m.valid and [r.outcome for r in m.runs] == ["PASS", "PASS"],
               "a complete passing run reconciles to PASS")
        expect(all(v == "PASS" for r in m.runs for v in r.checks.values()),
               "every declared check passes in a complete passing run")
        expect(m.failure_count == 0 and m.failure_rate == 0.0,
               "a clean measurement reports a 0% failure rate")

        m = run_synthetic(tree, "fail", runs=1)
        expect([r.outcome for r in m.runs] == ["FAIL"],
               "a parsed check failure makes the run FAIL")
        expect(m.runs[0].checks == {"alpha": "PASS", "beta": "FAIL",
                                    "gamma": "PASS"},
               "the failing check is the only one recorded FAIL")

        m = run_synthetic(tree, "nonzero_partial", runs=1)
        expect([r.outcome for r in m.runs] == ["FAIL"],
               "a nonzero exit with only passing checks still FAILs the run")
        expect(m.runs[0].checks["gamma"] == "MISSING",
               "the check a nonzero exit prevented is MISSING")

        m = run_synthetic(tree, "abort", runs=1)
        expect([r.outcome for r in m.runs] == ["FAIL"],
               "an early setup abort makes the run FAIL")
        expect(m.runs[0].checks == {"alpha": "PASS", "beta": "MISSING",
                                    "gamma": "MISSING"},
               "an early abort leaves its remaining checks MISSING")

        m = run_synthetic(tree, "diagnostics", runs=1)
        expect([r.outcome for r in m.runs] == ["PASS"],
               "INFO/WARN/SKIP diagnostics never make a run fail")

        m = run_synthetic(tree, "brackets_ok", runs=1)
        expect(m.valid and [r.outcome for r in m.runs] == ["PASS"],
               "bracketed DATA on stdout is not a marker and passes")

        m = run_synthetic(tree, "hang", runs=1, timeout=3.0)
        expect([r.outcome for r in m.runs] == ["TIMEOUT"],
           "a hung probe reconciles to TIMEOUT")
        expect(m.runs[0].checks == {"alpha": "PASS", "beta": "PASS",
                                    "gamma": "MISSING"},
               "partial checks emitted before a timeout are kept, the rest MISSING")
        expect(m.timeout_count == 1 and m.failure_count == 1,
               "a timeout counts toward the failure rate and stays separately visible")

        # TIMEOUT wins over a FAIL check in the same run.
        raw = tree.root / "timeout_fail.jsonl"
        raw.write_text(_line(event="check", id="alpha", outcome="FAIL"),
                       encoding="utf-8")
        outcome, outcomes = probe_flake.reconcile(
            _descriptor(), ok=False, timed_out=True,
            events_text=raw.read_text(), stdout_text="")
        expect(outcome == "TIMEOUT" and outcomes["alpha"] == "FAIL",
               "TIMEOUT takes precedence over a failed check in the same run")


def test_harness_errors() -> None:
    print("\n-- harness errors --")
    with SyntheticTree() as tree:
        m = run_synthetic(tree, "marker", runs=1)
        expect(not m.valid and m.status == "harness-error",
               "a forbidden bracketed stdout marker is a harness error")
        expect(m.failure_rate is None,
               "a harness error reports no failure rate at all")
        expect("bracketed stdout markers" in (m.error or ""),
               "the harness error names the second result channel")
        expect(m.error_run is not None
               and m.error_run.checks.get("alpha") == "PASS"
               and m.error_run.checks.get("beta") == "MISSING",
               "a marker error still reports the valid partial check data "
               "its stream did parse")
        text = probe_flake.render(m)
        expect("every run succeeded" not in text
               and str(m.error_run.artifact_dir) in text,
               "the table names the retained artifacts instead of claiming "
               "success")

        raws = {
            "truncated": '{"event": "check", "id": "alpha"',
            "malformed": "this is not json\n",
            "prefix_then_malformed": (
                _line(event="check", id="alpha", outcome="PASS") +
                "this is not json\n"),
            "prefix_then_truncated": (
                _line(event="check", id="alpha", outcome="PASS") +
                '{"event": "check", "id": "be'),
            "duplicate": (_line(event="check", id="alpha", outcome="PASS") +
                          _line(event="check", id="alpha", outcome="PASS")),
            "unexpected": _line(event="check", id="delta", outcome="PASS"),
            "outoforder": _line(event="check", id="gamma", outcome="PASS"),
            "unclassifiable": _line(event="mystery", id="alpha"),
        }
        for name, body in raws.items():
            path = tree.root / f"raw-{name}.jsonl"
            path.write_text(body, encoding="utf-8")
            os.environ["SYNTHETIC_RAW_PATH"] = str(path)
            try:
                m = run_synthetic(tree, "raw", runs=2)
            finally:
                os.environ.pop("SYNTHETIC_RAW_PATH", None)
            expect(not m.valid and m.status == "harness-error",
                   f"a {name} protocol stream is a harness error, not a probe PASS")
            expect(m.failure_rate is None,
                   f"a {name} stream yields no flake rate")
            expect(len(m.runs) < 2,
                   f"a {name} stream stops the measurement rather than continuing")
            expect(m.error_run is not None,
                   f"a {name} stream still REPORTS the run it discarded")
            expect(m.error_run is not None and m.retained_artifacts()
                   and m.error_run.artifact_dir is not None
                   and m.error_run.artifact_dir.exists(),
                   f"a {name} run's retained artifacts are named in the result")
            expect(m.error_run is not None
                   and m.error_run.outcome == "HARNESS_ERROR"
                   and m.error_run not in m.runs,
                   f"a {name} run is not counted as a probe outcome")
            expect("every run succeeded" not in probe_flake.render(m),
                   f"the {name} table never claims every run succeeded")
            if name.startswith("prefix_then_"):
                expect(m.error_run is not None
                       and m.error_run.checks.get("alpha") == "PASS"
                       and m.error_run.checks.get("beta") == "MISSING",
                       f"a {name} stream still reports the trusted prefix it "
                       f"parsed before the fault")


# ==========================================================================
# Ports and concurrency
# ==========================================================================
def test_ports() -> None:
    print("\n-- ports --")
    with SyntheticTree() as tree:
        expect_raises(probe_flake.Rejection,
                      lambda: probe_flake.PortLease.try_acquire(
                          probe_flake.FORBIDDEN_PORT),
                      "port 8008 is always forbidden", "always forbidden")

        saved = (probe_flake.PORT_MIN, probe_flake.PORT_MAX)
        try:
            # A three-port range makes increment, wraparound and
            # exhaustion all directly observable.
            probe_flake.PORT_MIN, probe_flake.PORT_MAX = 8009, 8011
            lease_a, cursor = probe_flake.acquire_port(8009)
            expect(lease_a.port == 8009 and cursor == 8010,
                   "the first lease takes the range floor and advances the cursor")
            lease_b, cursor = probe_flake.acquire_port(cursor)
            expect(lease_b.port == 8010 and cursor == 8011,
                   "the cursor increments per run")
            lease_c, cursor = probe_flake.acquire_port(cursor)
            expect(lease_c.port == 8011 and cursor == 8009,
                   "the cursor wraps back to the range floor")
            expect_raises(probe_flake.PortExhausted,
                          lambda: probe_flake.acquire_port(cursor),
                          "a fully leased range is exhausted, cleanly",
                          "complete scan")
            # A concurrent harness (a second lease attempt on a held
            # port) is refused by the atomic O_EXCL create.
            expect(probe_flake.PortLease.try_acquire(8010) is None,
                   "a port another harness holds cannot be leased twice")
            lease_b.release()
            regained, _ = probe_flake.acquire_port(8010)
            expect(regained.port == 8010,
                   "a released port becomes leasable again")
            regained.release()
            lease_a.release()
            lease_c.release()

            # 8008 is skipped even if the range is edited to include it.
            probe_flake.PORT_MIN, probe_flake.PORT_MAX = 8008, 8009
            lease, _ = probe_flake.acquire_port(8008)
            expect(lease.port == 8009,
                   "8008 is skipped even when the range would contain it")
            lease.release()
        finally:
            probe_flake.PORT_MIN, probe_flake.PORT_MAX = saved

        # A leftover lease FILE is not a lease: the lock is, and a dead
        # owner holds none. No age heuristic, nothing to unlink.
        leftover = probe_flake._lease_dir() / "8009.lease"
        leftover.write_text(json.dumps({"pid": _dead_pid(), "port": 8009}),
                            encoding="utf-8")
        lease = probe_flake.PortLease.try_acquire(8009)
        expect(lease is not None,
               "a leftover lease file from a dead harness is immediately "
               "acquirable")
        if lease:
            lease.release()

        # A held lease is never stolen, however old its file looks.
        held = probe_flake.PortLease.try_acquire(8009)
        os.utime(leftover, (0, 0))
        expect(probe_flake.PortLease.try_acquire(8009) is None,
               "a held lease is never treated as stale, whatever its mtime")
        if held:
            held.release()
        expect(leftover.exists(),
               "releasing a lease leaves its diagnostic file in place — "
               "unlinking it is what would reintroduce the recovery race")


def test_concurrent_leasing() -> None:
    print("\n-- concurrent lease acquisition --")
    with SyntheticTree() as tree:
        port = 8042
        path = probe_flake._lease_dir() / f"{port}.lease"
        # A leftover file from an abnormally terminated harness: the
        # exact state in which two racing harnesses used to be able to
        # both "recover" it and both end up on this port.
        path.write_text(json.dumps({"pid": _dead_pid(), "port": port}),
                        encoding="utf-8")

        racers = 12
        ready = threading.Barrier(racers)
        winners: list[object] = []
        lock = threading.Lock()

        def race() -> None:
            ready.wait(timeout=30)
            lease = probe_flake.PortLease.try_acquire(port)
            if lease is not None:
                with lock:
                    winners.append(lease)

        threads = [threading.Thread(target=race) for _ in range(racers)]
        for thread in threads:
            thread.start()
        for thread in threads:
            thread.join(timeout=60)
        expect(len(winners) == 1,
               f"exactly one of {racers} racers recovers and holds the port "
               f"(got {len(winners)})")
        for lease in winners:
            lease.release()

        # Cross-PROCESS exclusion, and recovery when the holder is
        # killed outright — which is what an `flock` gives for free and
        # a create-then-delete protocol has to guess at.
        holder_src = tree.root / "holder.py"
        holder_src.write_text(textwrap.dedent(f'''\
            import sys, time
            sys.path.insert(0, {TOOLS_DIR!r})
            import probe_flake
            from pathlib import Path
            probe_flake.LEASE_ROOT = Path({str(probe_flake.LEASE_ROOT)!r})
            lease = probe_flake.PortLease.try_acquire({port})
            print("held" if lease else "missed", flush=True)
            time.sleep(300)
        '''), encoding="utf-8")
        holder = subprocess.Popen([sys.executable, str(holder_src)],
                                  stdout=subprocess.PIPE, text=True)
        try:
            first = holder.stdout.readline().strip()
            expect(first == "held",
                   f"another process acquires the port first (said {first!r})")
            expect(probe_flake.PortLease.try_acquire(port) is None,
                   "a lease held by another PROCESS blocks this one")
        finally:
            holder.kill()
            holder.wait(timeout=30)
        recovered = None
        for _ in range(50):
            recovered = probe_flake.PortLease.try_acquire(port)
            if recovered is not None:
                break
            time.sleep(0.1)
        expect(recovered is not None,
               "killing the holder outright releases the lease with no "
               "staleness heuristic at all")
        if recovered:
            recovered.release()


def _dead_pid() -> int:
    """A pid that is certainly not running: spawn one and reap it."""
    proc = subprocess.Popen([sys.executable, "-c", "pass"])
    proc.wait()
    return proc.pid


def test_concurrency_accounting() -> None:
    print("\n-- concurrency accounting --")
    with SyntheticTree() as tree:
        with probe_flake.LiveRegistry() as solo:
            expect(solo.sample() == 1,
                   "a solo harness observes a concurrency of 1 (itself)")
            with probe_flake.LiveRegistry() as other:
                expect(solo.sample() == 2,
                       "a second live invocation is counted by the first")
                expect(other.sample() == 2,
                       "and by the second")
            expect(solo.peak == 2,
                   "the recorded concurrency is the PEAK, not the current count")
            expect(solo.sample() == 1,
                   "a departed invocation stops being counted")

        # A concurrent startup must never lose a live registration: a
        # harness publishing its own entry while others scan the same
        # directory used to be readable as an empty, "corrupt" file and
        # be unlinked, erasing a still-running harness from every later
        # sample.
        harnesses = 8
        ready = threading.Barrier(harnesses)
        observed: list[int] = []
        errors: list[str] = []
        lock = threading.Lock()

        def start_one() -> None:
            try:
                with probe_flake.LiveRegistry() as registry:
                    ready.wait(timeout=30)
                    for _ in range(20):
                        registry.sample()
                    with lock:
                        observed.append(registry.peak)
                        if not registry.path.exists():
                            errors.append(f"{registry.path} was unlinked while "
                                          f"its owner was still live")
            except Exception as error:  # noqa: BLE001
                with lock:
                    errors.append(f"{type(error).__name__}: {error}")

        threads = [threading.Thread(target=start_one) for _ in range(harnesses)]
        for thread in threads:
            thread.start()
        for thread in threads:
            thread.join(timeout=60)
        expect(not errors, f"concurrent startups keep every registration "
                           f"({errors[:3]})")
        expect(len(observed) == harnesses and all(v == harnesses
                                                  for v in observed),
               f"every concurrent harness sees all {harnesses} of them "
               f"(observed {sorted(observed)})")
        leftovers = [e for e in (probe_flake.LEASE_ROOT / "live").iterdir()]
        expect(leftovers == [],
               f"every departed harness cleans its registration up ({leftovers})")

        # A registration that looks corrupt but is YOUNG is another
        # harness mid-startup, not garbage: leave it, and count it.
        live_dir = probe_flake.LEASE_ROOT / "live"
        live_dir.mkdir(parents=True, exist_ok=True)
        young = live_dir / "12345-partial.json"
        young.write_text("", encoding="utf-8")
        with probe_flake.LiveRegistry() as registry:
            expect(registry.sample() == 2,
                   "a young unreadable registration is counted, not discarded")
        expect(young.exists(),
               "a young unreadable registration is never unlinked")
        young.unlink()

        # Stale registration from an abnormally terminated harness.
        live_dir = probe_flake.LEASE_ROOT / "live"
        live_dir.mkdir(parents=True, exist_ok=True)
        stale = live_dir / "999999-deadbeef.json"
        stale.write_text(json.dumps({"pid": _dead_pid(), "started": 0.0}),
                         encoding="utf-8")
        os.utime(stale, (0, 0))
        garbage = live_dir / "garbage.json"
        garbage.write_text("not json", encoding="utf-8")
        os.utime(garbage, (0, 0))          # old enough to be nobody's
        with probe_flake.LiveRegistry() as registry:
            expect(registry.sample() == 1,
                   "a stale registration is recovered, not counted")
        expect(not stale.exists() and not garbage.exists(),
               "stale and OLD unreadable registrations are removed")

        m = run_synthetic(tree, "pass", runs=1)
        expect(m.peak_concurrency >= 1,
               "a measurement records its own peak observed concurrency")


# ==========================================================================
# Artifacts
# ==========================================================================
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
                          Path(run_probes.REPO_ROOT) / "artifacts"),
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


# ==========================================================================
# Result serialization
# ==========================================================================
def test_result_document() -> None:
    print("\n-- probe-flake-result/v1 --")
    with SyntheticTree() as tree:
        m = run_synthetic(tree, "fail", runs=3)
        out = tree.root / "result.json"
        probe_flake.write_result(m, str(out))
        doc = json.loads(out.read_text(encoding="utf-8"))

        required = {"schema", "probe", "status", "error", "requested_runs",
                    "completed_runs", "runs", "checks", "check_counts",
                    "failure_count", "failure_rate", "timeout_count",
                    "worst_elapsed_seconds", "timestamp_utc", "commit_sha",
                    "rts_capabilities", "peak_concurrency",
                    "retained_artifacts", "artifact_root", "invocation_dir"}
        missing = required - set(doc)
        expect(not missing, f"the result document carries every required field "
                            f"(missing: {sorted(missing)})")
        expect(doc["schema"] == "probe-flake-result/v1",
               "the result document is versioned")
        expect(doc["requested_runs"] == 3 and doc["completed_runs"] == 3,
               "requested and completed run counts are both reported")
        expect(len(doc["runs"]) == 3,
               "the complete valid per-run outcome list has one entry per run")
        expect(all("elapsed_seconds" in r and "checks" in r
                   for r in doc["runs"]),
               "each run reports its elapsed duration and check outcomes")
        expect(doc["rts_capabilities"] == 4,
               "the effective RTS capability count is recorded")
        expect(doc["failure_count"] == 3 and doc["failure_rate"] == 1.0,
               "the aggregate failure rate counts every failing run")

        for cid, counts in doc["check_counts"].items():
            total = sum(counts.values())
            expect(total == doc["requested_runs"],
                   f"check {cid}: PASS+FAIL+MISSING == requested runs ({total})")

        # Timeouts ride in the failure numerator while staying visible.
        m = run_synthetic(tree, "hang", runs=1, timeout=3.0)
        doc = m.to_document()
        expect(doc["timeout_count"] == 1 and doc["failure_count"] == 1
               and doc["failure_rate"] == 1.0,
               "a timeout is in the failure numerator and separately visible")
        expect(doc["worst_elapsed_seconds"] >= 0.0,
               "the worst elapsed duration is reported")

        # A harness error keeps the valid partial data but no rate.
        os.environ["SYNTHETIC_RAW_PATH"] = str(tree.root / "bad2.jsonl")
        (tree.root / "bad2.jsonl").write_text("nope\n", encoding="utf-8")
        try:
            m = run_synthetic(tree, "raw", runs=4)
        finally:
            os.environ.pop("SYNTHETIC_RAW_PATH", None)
        doc = m.to_document()
        expect(doc["status"] == "harness-error" and doc["failure_rate"] is None,
               "an invalid measurement declares its status and reports no rate")
        expect(isinstance(doc["error"], str) and doc["error"],
               "an invalid measurement carries error detail")


def test_exit_codes() -> None:
    print("\n-- harness exit codes --")
    with SyntheticTree() as tree:
        # A valid measurement exits 0 whatever it observed. Driving
        # main() needs the module state the SyntheticTree installed, so
        # call it in-process rather than as a subprocess.
        for mode, label in (("pass", "0% observed"), ("fail", "100% observed")):
            previous = os.environ.get("SYNTHETIC_MODE")
            os.environ["SYNTHETIC_MODE"] = mode
            try:
                rc = probe_flake.main(["--probe", "synthetic", "--runs", "1",
                                       "--artifact-root", str(tree.artifacts())])
            finally:
                if previous is None:
                    os.environ.pop("SYNTHETIC_MODE", None)
                else:
                    os.environ["SYNTHETIC_MODE"] = previous
            expect(rc == 0, f"a valid measurement exits 0 ({label})")

        rc = probe_flake.main(["--probe", "nosuchprobe", "--runs", "1",
                               "--artifact-root", str(tree.artifacts())])
        expect(rc == probe_flake.EXIT_REJECTED,
               "a pre-execution rejection exits nonzero")

        os.environ["SYNTHETIC_MODE"] = "marker"
        try:
            rc = probe_flake.main(["--probe", "synthetic", "--runs", "1",
                                   "--artifact-root", str(tree.artifacts())])
        finally:
            os.environ.pop("SYNTHETIC_MODE", None)
        expect(rc == probe_flake.EXIT_HARNESS_ERROR,
               "a harness error exits nonzero")

        # The whole point of the class: malformed protocol input reaches
        # the documented harness-error exit rather than a traceback.
        for name, body in (("unhashable id", '{"event": "check", "id": [], '
                                             '"outcome": "PASS"}\n'),
                           ("object id", '{"event": "check", "id": {}, '
                                         '"outcome": "PASS"}\n')):
            raw = tree.root / f"hostile-{name.replace(' ', '-')}.jsonl"
            raw.write_text(body, encoding="utf-8")
            os.environ["SYNTHETIC_RAW_PATH"] = str(raw)
            os.environ["SYNTHETIC_MODE"] = "raw"
            try:
                rc = probe_flake.main(
                    ["--probe", "synthetic", "--runs", "1",
                     "--artifact-root", str(tree.artifacts())])
            finally:
                os.environ.pop("SYNTHETIC_MODE", None)
                os.environ.pop("SYNTHETIC_RAW_PATH", None)
            expect(rc == probe_flake.EXIT_HARNESS_ERROR,
                   f"a stream with an {name} exits {probe_flake.EXIT_HARNESS_ERROR}, "
                   f"not a traceback (got {rc})")

        saved = (probe_flake.PORT_MIN, probe_flake.PORT_MAX)
        held = None
        try:
            probe_flake.PORT_MIN = probe_flake.PORT_MAX = 8009
            held = probe_flake.PortLease.try_acquire(8009)
            os.environ["SYNTHETIC_MODE"] = "pass"
            rc = probe_flake.main(["--probe", "synthetic", "--runs", "1",
                                   "--artifact-root", str(tree.artifacts())])
        finally:
            os.environ.pop("SYNTHETIC_MODE", None)
            if held:
                held.release()
            probe_flake.PORT_MIN, probe_flake.PORT_MAX = saved
        expect(rc == probe_flake.EXIT_NO_PORT,
               "port-range exhaustion exits nonzero without starting a probe")


def test_render() -> None:
    print("\n-- human-readable table --")
    with SyntheticTree() as tree:
        m = run_synthetic(tree, "fail", runs=2)
        text = probe_flake.render(m)
        for needle in ("alpha", "beta", "gamma", "MISS", "failures",
                       "timeouts", "RTS capabilities", "peak concurrency",
                       "retained artifacts"):
            expect(needle in text, f"the table reports {needle!r}")


# ==========================================================================
# Census manifest
# ==========================================================================
def test_manifest_fixture() -> None:
    print("\n-- census manifest (self-owned fixture) --")
    with SyntheticTree(keys=("synthetic", "legacyprobe")) as tree:
        probe_flake.PROTOCOL_PROBES = {
            "synthetic": probe_protocol.PROTOCOL_VERSION}
        ci_probes.CI_ELIGIBLE = {"legacyprobe"}
        manifest = probe_census.build_manifest()
        expect(probe_census.validate_manifest(manifest) == [],
               "a freshly built manifest validates against the live registry")
        expect({e["key"] for e in manifest["probes"]} ==
               {"synthetic", "legacyprobe"},
               "the manifest lists every registered probe exactly once")
        expect([e for e in manifest["probes"]
                if e["key"] == "synthetic"][0]["protocol"] == "probe-result/v1",
               "a migrated probe is recorded as probe-result/v1")
        expect([e for e in manifest["probes"]
                if e["key"] == "legacyprobe"][0]["protocol"] == "legacy",
               "an unmigrated probe stays visibly legacy")
        expect([e for e in manifest["probes"]
                if e["key"] == "legacyprobe"][0]["classification"] ==
               "ci-eligible",
               "the classification comes from tools/ci_probes.py")

        def mutated(fn):
            doc = json.loads(json.dumps(manifest))
            fn(doc)
            return probe_census.validate_manifest(doc)

        expect(any("missing entry" in p for p in
                   mutated(lambda d: d["probes"].pop(0))),
               "a missing entry is rejected")
        expect(any("duplicate" in p for p in
                   mutated(lambda d: d["probes"].append(d["probes"][0]))),
               "a duplicate entry is rejected")
        expect(any("extra entry" in p for p in
                   mutated(lambda d: d["probes"].append(
                       {"key": "ghost", "script": "ghost_probe.py",
                        "classification": "manual-only", "protocol": "legacy"}))),
               "an extra entry naming no registered probe is rejected")
        expect(any("classification" in p for p in
                   mutated(lambda d: d["probes"][0].update(
                       {"classification": "manual-only"
                        if d["probes"][0]["classification"] == "ci-eligible"
                        else "ci-eligible"}))),
               "a classification disagreeing with ci_probes.py is rejected")
        expect(any("protocol status" in p for p in
                   mutated(lambda d: d["probes"][0].update(
                       {"protocol": "probe-result/v9"}))),
               "a protocol status disagreeing with the in-repo registry is rejected")
        expect(any("script" in p for p in
                   mutated(lambda d: d["probes"][0].update(
                       {"script": "wrong_probe.py"}))),
               "a script name disagreeing with the registry is rejected")
        expect(any("schema" in p for p in
                   mutated(lambda d: d.update({"schema": "probe-census/v9"}))),
               "an unexpected manifest schema is rejected")
        expect(probe_census.validate_manifest([]) != [],
               "a non-object manifest is rejected")
        expect(probe_census.validate_manifest({"schema": "probe-census/v1"}) != [],
               "a manifest with no probes list is rejected")

        # Seeding writes into the resolved docs worktree, never elsewhere.
        # A real git repository with no `docs-wip` worktree is the case
        # that must name the repair; a directory that is not a
        # repository at all is a different, also-reported failure.
        scratch = Path(tempfile.mkdtemp(prefix="probe-flake-git-"))
        try:
            subprocess.run(["git", "init", "-q", str(scratch)],
                           check=True, capture_output=True)
            expect_raises(probe_census.DocsWorktreeMissing,
                          lambda: probe_census.resolve_docs_worktree(str(scratch)),
                          "with no docs-wip worktree, seeding stops with an "
                          "actionable error", "git worktree add")
        finally:
            shutil.rmtree(scratch, ignore_errors=True)
        expect_raises(probe_census.DocsWorktreeMissing,
                      lambda: probe_census.resolve_docs_worktree(str(tree.root)),
                      "outside a git repository the manifest is never written "
                      "anyway", "could not list git worktrees")


def test_manifest_real_registry() -> None:
    print("\n-- census manifest (real registry, 86 probes) --")
    manifest = probe_census.build_manifest()
    expect(len(manifest["probes"]) == len(run_probes.PROBES),
           f"the manifest lists all {len(run_probes.PROBES)} registered probes")
    expect(len({e["key"] for e in manifest["probes"]}) == len(run_probes.PROBES),
           "each registered probe appears exactly once")
    expect(probe_census.validate_manifest(manifest) == [],
           "the built manifest agrees with run_probes.PROBES and ci_probes.py")
    ci = sum(1 for e in manifest["probes"]
             if e["classification"] == "ci-eligible")
    expect(ci == len(ci_probes.CI_ELIGIBLE),
           f"{ci} entries are CI-eligible, matching tools/ci_probes.py")
    migrated = [e["key"] for e in manifest["probes"]
                if e["protocol"] != "legacy"]
    expect(migrated == ["role"],
           f"role is the only probe-result/v1 probe (got {migrated})")

    # The REAL docs-wip manifest, only when one is resolvable.
    try:
        path = probe_census.manifest_path()
    except probe_census.DocsWorktreeMissing as error:
        skip(f"no docs-wip worktree resolvable, so the real manifest is not "
             f"validated here ({str(error).splitlines()[0]})")
        return
    if not path.exists():
        skip(f"{path} has not been seeded yet")
        return
    problems = probe_census.validate_manifest(probe_census.load(path))
    expect(problems == [], f"the seeded {path} agrees with the live registry "
                           f"({problems[:3]})")


# ==========================================================================
# role's standalone behavior is preserved
# ==========================================================================
def test_role_standalone() -> None:
    print("\n-- role probe migration --")
    repo_root = Path(__file__).resolve().parent.parent
    done = subprocess.run(
        [sys.executable, "tools/role_probe.py", "--describe"],
        cwd=repo_root, text=True, capture_output=True, timeout=60)
    expect(done.returncode == 0,
           "role --describe exits 0 without booting anything")
    try:
        descriptor = probe_protocol.parse_descriptor(done.stdout,
                                                     expected_probe="role")
    except probe_protocol.ProtocolError as error:
        expect(False, f"role's descriptor is valid probe-result/v1 ({error})")
        return
    expect(len(descriptor.ids) == 10,
           f"role declares its ten checks (got {len(descriptor.ids)})")
    expect(len(set(descriptor.ids)) == len(descriptor.ids),
           "role's check identifiers are unique")
    expect(all(probe_protocol.CHECK_ID_RE.match(cid) for cid in descriptor.ids),
           "role's identifiers are all stable, word-like identifiers")
    # The labels these replaced interpolated skill numbers and observed
    # roles; identity must carry none of that.
    expect(not any(any(ch.isdigit() for ch in cid) for cid in descriptor.ids),
           "role's identifiers carry no runtime values")

    # Standalone mode still prints the bracketed human markers, and
    # protocol mode never does.
    import role_probe  # type: ignore
    import io
    stream = io.StringIO()
    rep = probe_protocol.Reporter(role_probe.DESCRIPTOR, stream=stream)
    rep.check("derive_miner", True, "mining 60 -> miner: miner")
    rep.abort("no wood-harvestable flora")
    expect("[PASS] mining 60 -> miner: miner" in stream.getvalue(),
           "standalone role still prints its bracketed [PASS] line")
    expect("[FAIL] no wood-harvestable flora" in stream.getvalue(),
           "standalone role still prints a setup abort as [FAIL]")
    expect(rep.engine_args() == [],
           "standalone role passes no RTS override")
    expect(rep.engine_log_path("role_probe_engine.log", "/tmp/x.log")
           == "/tmp/x.log",
           "standalone role keeps its own engine-log path")

    with tempfile.TemporaryDirectory() as tmp:
        events = Path(tmp) / "events.jsonl"
        protocol_rep = probe_protocol.Reporter(
            role_probe.DESCRIPTOR, events_path=str(events),
            engine_log_dir=tmp, rts_caps=4, stream=stream)
        before = stream.getvalue()
        protocol_rep.check("derive_initial", True, "human text", {"role": "miner"})
        protocol_rep.abort("setup failed")
        protocol_rep.close()
        expect(stream.getvalue() == before,
               "protocol mode prints nothing to stdout")
        expect(protocol_rep.engine_args() == ["+RTS", "-N4", "-RTS"],
               "protocol mode pins the engine to the harness's RTS capabilities")
        expect(protocol_rep.engine_log_path("role_probe_engine.log", "/tmp/x.log")
               == os.path.join(tmp, "role_probe_engine.log"),
               "protocol mode stops role overwriting its shared /tmp engine log")
        text = events.read_text(encoding="utf-8")
        _events, outcomes = probe_protocol.parse_event_stream(
            text, role_probe.DESCRIPTOR)
        expect(outcomes["derive_initial"] == "PASS",
               "the protocol event stream carries the check outcome")
        expect(probe_protocol.forbidden_marker_lines(text) == [],
               "the event stream itself holds no bracketed marker lines")
        expect('"level": "WARN"' in text,
               "a setup abort is a WARN diagnostic in protocol mode, so the "
               "checks it prevented stay MISSING")


def test_run_one_defaults() -> None:
    print("\n-- run_one's extended interface --")
    expect(run_probes.probe_protocol_env() == {},
           "no protocol wiring produces no environment override")
    env = run_probes.probe_protocol_env(
        event_path="/e", artifact_dir="/a", engine_log_dir="/l", rts_caps=4)
    expect(env == {probe_protocol.ENV_EVENTS: "/e",
                   probe_protocol.ENV_ARTIFACT_DIR: "/a",
                   probe_protocol.ENV_ENGINE_LOG_DIR: "/l",
                   probe_protocol.ENV_RTS_CAPS: "4"},
           "every protocol parameter reaches the child through the environment")
    # An operator's stale export must not push an ordinary run_probes.py
    # run into protocol mode.
    stale = "/tmp/should-be-ignored.jsonl"
    saved = os.environ.get(probe_protocol.ENV_EVENTS)
    os.environ[probe_protocol.ENV_EVENTS] = stale
    try:
        with SyntheticTree() as tree:
            script = tree.root / "tools" / "echoenv_probe.py"
            script.write_text(textwrap.dedent(f'''\
                import os, sys
                sys.path.insert(0, {TOOLS_DIR!r})
                import probe_protocol
                print(repr(os.environ.get(probe_protocol.ENV_EVENTS)))
            '''), encoding="utf-8")
            _ok, _t, _e, out = run_probes.run_one("echoenv_probe.py", None, 60.0)
            expect(out.strip() == "None",
                   f"an inherited SYNARCHY_PROBE_EVENTS is stripped from an "
                   f"ordinary run (got {out.strip()!r})")
            _ok, _t, _e, out = run_probes.run_one(
                "echoenv_probe.py", None, 60.0, event_path="/tmp/wanted.jsonl")
            expect(out.strip() == "'/tmp/wanted.jsonl'",
                   f"the harness's own event path wins (got {out.strip()!r})")
    finally:
        if saved is None:
            os.environ.pop(probe_protocol.ENV_EVENTS, None)
        else:
            os.environ[probe_protocol.ENV_EVENTS] = saved

    import inspect
    signature = inspect.signature(run_probes.run_one)
    positional = [n for n, p in signature.parameters.items()
                  if p.kind is inspect.Parameter.POSITIONAL_OR_KEYWORD]
    expect(positional == ["script", "port", "timeout", "groups"],
           "run_one's positional interface is unchanged for existing callers")
    expect(all(signature.parameters[n].default is None
               for n in ("event_path", "artifact_dir", "engine_log_dir",
                         "rts_caps")),
           "every new parameter is keyword-only with a default")


# ==========================================================================
def main() -> int:
    for test in (test_descriptor, test_event_stream, test_trusted_prefix,
                 test_forbidden_markers,
                 test_eligibility, test_descriptor_mismatch_rejection,
                 test_reconciliation, test_harness_errors, test_ports,
                 test_concurrent_leasing,
                 test_concurrency_accounting, test_artifacts,
                 test_no_tmpdir_default, test_result_document,
                 test_exit_codes, test_render, test_manifest_fixture,
                 test_manifest_real_registry, test_role_standalone,
                 test_run_one_defaults):
        test()
    print()
    if SKIPS:
        print(f"{len(SKIPS)} skipped:")
        for message in SKIPS:
            print(f"  - {message}")
    if FAILURES:
        print(f"\n{len(FAILURES)} FAILED:")
        for message in FAILURES:
            print(f"  - {message}")
        return 1
    print("probe_flake self-test: all cases pass")
    return 0


if __name__ == "__main__":
    sys.exit(main())
