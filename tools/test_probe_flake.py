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

import argparse
import json
import os
import re
import shutil
import socket
import stat
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


# uid 0 is exempt from `_check_shared_dir`'s ownership rule BY DESIGN
# (a root-owned namespace is the case /tmp itself is), and everything
# root creates it also owns — so under root the "owned by a third
# party" scenario cannot be built the way an unprivileged run builds
# it, by asking about a directory we own as if we were someone else.
# Root can chown, though, so it is built the other way round rather
# than skipped: hand the directory to an account that is neither root
# nor us, and ask as ourselves. CI's container runs as root (#1475),
# which is the only place that branch is taken.
THIRD_PARTY_UID = 65534


def hand_to_third_party(path: Path) -> int:
    """Leave `path` owned by neither root nor the uid this returns.

    The returned uid is the one to ask `_check_shared_dir` with, so the
    same two assertions exercise the same rejection under either
    privilege level.
    """
    if os.getuid() == 0:
        os.chown(path, THIRD_PARTY_UID, -1)
        return os.getuid()
    return os.getuid() + 1


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

    if mode == "rawbytes":
        # Bytes that are not valid UTF-8 in any position, built without
        # a single escape — this source lives inside a triple-quoted
        # template that would eat one level of them.
        payload = (b'{{"event": "check", "id": "' + bytes([0xff, 0xfe])
                   + b'", "outcome": "PASS"}}' + bytes([10]))
        with open(os.environ[probe_protocol.ENV_EVENTS], "wb") as fh:
            fh.write(payload)
        raise SystemExit(0)

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
        # Stands in for `/tmp`: the harness requires a sticky directory
        # it does not have to repair, so the fixture builds one.
        leases = self.root / "leases"
        leases.mkdir()
        leases.chmod(0o1777)
        probe_flake.LEASE_ROOT = leases
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
        m = run_synthetic(tree, "rawbytes", runs=1)
        expect(not m.valid and m.status == "harness-error",
               "an event stream of invalid UTF-8 is a harness error")
        expect(m.failure_rate is None and m.error_run is not None
               and m.error_run.artifact_dir is not None
               and m.error_run.artifact_dir.exists(),
               "and its run is retained and reported, with no rate")
        expect("UnicodeDecodeError" in (m.error or ""),
               f"the harness error names the decoding failure ({m.error})")

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
        leftover = probe_flake._lease_path(8009)
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


def _held_ports(base: int, width: int) -> list[int]:
    """Which of `base .. base + width - 1` cannot be leased right now.

    `flock` conflicts between open file DESCRIPTIONS, not processes, so
    this answers correctly about a lease this same process is holding —
    which is what makes it a usable probe for "the span is held".
    Anything it could take is given straight back.
    """
    held: list[int] = []
    for port in range(base, base + width):
        lease = probe_flake.PortLease.try_acquire(port)
        if lease is None:
            held.append(port)
        else:
            lease.release()
    return held


def test_port_spans() -> None:
    print("\n-- multi-port spans (#1571) --")
    with SyntheticTree() as tree:
        saved = (probe_flake.PORT_MIN, probe_flake.PORT_MAX)
        try:
            # A five-port range makes every boundary directly observable.
            probe_flake.PORT_MIN, probe_flake.PORT_MAX = 8009, 8013

            expect_raises(probe_flake.Rejection,
                          lambda: probe_flake.acquire_span(8009, 0),
                          "a zero-width span is rejected", "positive")
            expect_raises(probe_flake.PortExhausted,
                          lambda: probe_flake.acquire_span(8009, 6),
                          "a span wider than the whole range is exhaustion, "
                          "not a partial lease", "does not fit")

            # -- the complete span is leased, and the cursor advances by it.
            leases, cursor = probe_flake.acquire_span(8009, 2)
            expect([lease.port for lease in leases] == [8009, 8010],
                   f"a two-port span leases BOTH ports, contiguously "
                   f"(got {[lease.port for lease in leases]})")
            expect(cursor == 8011,
                   f"and the cursor advances by the full span (got {cursor})")
            expect(_held_ports(8009, 2) == [8009, 8010],
                   "both members are really held while the span is out")

            # -- a span overlapping a held one is refused, base or member.
            #    8009 is held (a base clash) and 8010 is held (a SECONDARY
            #    clash for base 8009), so the next two-port span must start
            #    at 8011.
            second, cursor2 = probe_flake.acquire_span(8009, 2)
            expect([lease.port for lease in second] == [8011, 8012],
                   f"the next span skips every base whose span overlaps a "
                   f"held one (got {[lease.port for lease in second]})")
            for lease in second:
                lease.release()

            # -- partial acquisition is released, not kept. Base 8011 is
            #    free but 8012 is not, so the attempt at 8011 must give
            #    8011 back before moving on -- otherwise the free port
            #    would be stranded by a span that never started.
            blocker = probe_flake.PortLease.try_acquire(8012)
            expect(blocker is not None, "the fixture can hold 8012")
            expect_raises(probe_flake.PortExhausted,
                          lambda: probe_flake.acquire_span(8011, 2),
                          "no two-port span survives 8009/8010/8012 being held",
                          "complete scan")
            expect(_held_ports(8011, 1) == [],
                   "and the partially acquired 8011 was given back")
            if blocker:
                blocker.release()

            # -- an OCCUPIED secondary is refused the same way a leased one
            #    is: the lease is available but something outside this
            #    harness is listening there.
            for lease in leases:
                lease.release()
            occupied = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
            occupied.bind(("127.0.0.1", 8010))
            occupied.listen(4)
            try:
                got, _ = probe_flake.acquire_span(8009, 2)
                expect([lease.port for lease in got] == [8011, 8012],
                       f"a span whose SECOND port is occupied is skipped "
                       f"(got {[lease.port for lease in got]})")
                expect(_held_ports(8009, 1) == [],
                       "and the base it partially acquired was released")
                for lease in got:
                    lease.release()
            finally:
                occupied.close()

            # -- a span never wraps the range end. 8013 is PORT_MAX, so a
            #    two-port span cannot start there; it wraps the SCAN to
            #    8009 instead of leasing 8013+8014.
            wrapped, _ = probe_flake.acquire_span(8013, 2)
            expect([lease.port for lease in wrapped] == [8009, 8010],
                   f"a base whose span would pass PORT_MAX is skipped, and "
                   f"the scan wraps to a base that fits "
                   f"(got {[lease.port for lease in wrapped]})")
            for lease in wrapped:
                lease.release()

            # -- span 1 is exactly the old single-port behaviour.
            one, cursor3 = probe_flake.acquire_span(8009, 1)
            expect([lease.port for lease in one] == [8009] and cursor3 == 8010,
                   f"span 1 leases one port and advances by one "
                   f"(got {[lease.port for lease in one]}, cursor {cursor3})")
            for lease in one:
                lease.release()
        finally:
            probe_flake.PORT_MIN, probe_flake.PORT_MAX = saved


def test_measure_leases_the_probes_whole_declared_span() -> None:
    print("\n-- measure leases the DECLARED span, and only lets go after the "
          "reap --")
    with SyntheticTree() as tree:
        saved_spans = run_probes.PROBE_PORT_SPANS
        real_run_one = run_probes.run_one
        seen: dict[str, object] = {}

        def spy(script, port, timeout, groups, **kwargs):
            # Inside `run_one` the probe is live, so this is the window
            # the span has to cover. `run_one` reaps the probe's whole
            # process group before it returns, so a lease still held
            # here and released after cannot hand a port to anyone while
            # an engine still owns it.
            seen["base"] = port
            seen["held"] = _held_ports(port, 3)
            return real_run_one(script, port, timeout, groups, **kwargs)

        run_probes.PROBE_PORT_SPANS = {"synthetic": 2}
        run_probes.run_one = spy
        try:
            measurement = run_synthetic(tree, "pass", runs=1)
        finally:
            run_probes.run_one = real_run_one
            run_probes.PROBE_PORT_SPANS = saved_spans

        base = seen.get("base")
        expect(isinstance(base, int) and base is not None,
               f"the probe was launched with a base port (got {base!r})")
        expect(seen.get("held") == [base, base + 1],
               f"its declared TWO-port span was held for the whole run, and "
               f"nothing beyond it (got {seen.get('held')}, base {base})")
        expect(measurement.runs and measurement.runs[0].port == base,
               f"the run records the BASE of the span it was given "
               f"(got {[r.port for r in measurement.runs]}, base {base})")
        expect(_held_ports(base, 2) == [],
               "and every member is released once the measurement is over")


def test_concurrent_leasing() -> None:
    print("\n-- concurrent lease acquisition --")
    with SyntheticTree() as tree:
        port = 8042
        path = probe_flake._lease_path(port)
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


# Source for a child harness that publishes a live registration and
# holds it until killed — the only way to exercise "another PROCESS
# owns this entry" and, after a SIGKILL, "its owner is gone" without
# consulting a pid.
REGISTRY_HOLDER_SRC = """\
import sys, time
sys.path.insert(0, {tools!r})
import probe_flake
from pathlib import Path
probe_flake.LEASE_ROOT = Path({root!r})
with probe_flake.LiveRegistry() as registry:
    print(registry.path, flush=True)
    time.sleep(120)
"""


# A port deliberately OUTSIDE 8009-8999, so the cross-TMPDIR test below
# contends in the REAL machine-wide lease namespace — which is the only
# place the defect it covers can be observed — without ever touching a
# port a real harness might be measuring on.
TMPDIR_TEST_PORT = 65000

# Source for a child harness that resolves LEASE_ROOT ITSELF. Rebinding
# it here would defeat the point: what is under test is whether two
# invocations under different TMPDIRs agree on the namespace.
HOLDER_SRC = """\
import sys, time
sys.path.insert(0, {tools!r})
import probe_flake
lease = probe_flake.PortLease.try_acquire({port})
print(('held ' if lease else 'missed ') + str(probe_flake.LEASE_ROOT),
      flush=True)
if lease:
    time.sleep(120)
"""


def test_lease_root_is_tmpdir_independent() -> None:
    print("\n-- lease namespace vs TMPDIR --")
    saved = os.environ.get("TMPDIR")
    try:
        first = probe_flake._machine_wide_scratch()
        os.environ["TMPDIR"] = tempfile.mkdtemp(prefix="probe-flake-tmpa-")
        tempfile.tempdir = None
        second = probe_flake._machine_wide_scratch()
        moved_artifacts = probe_flake.default_artifact_root()
        os.environ["TMPDIR"] = tempfile.mkdtemp(prefix="probe-flake-tmpb-")
        tempfile.tempdir = None
        third = probe_flake._machine_wide_scratch()
        expect(first == second == third,
               f"the lease root is the same under any TMPDIR "
               f"({first}, {second}, {third})")
        expect(first == Path("/tmp"),
               f"the lease root is anchored at the fixed shared /tmp ({first})")
        expect(moved_artifacts != probe_flake.default_artifact_root(),
               "the ARTIFACT root does follow TMPDIR — only the lease "
               "namespace is pinned")
    finally:
        tempfile.tempdir = None
        if saved is None:
            os.environ.pop("TMPDIR", None)
        else:
            os.environ["TMPDIR"] = saved

    # Cross-USER, too: a TCP port is host-global, so a uid in the path
    # would let two accounts lease "the same" port through different
    # files. A second account cannot be created from a self-test, so
    # the properties that make sharing WORK are asserted instead.
    root = probe_flake._machine_wide_scratch()
    expect(str(os.getuid()) not in root.name,
           f"the lease root carries no uid, so every account resolves the "
           f"same one ({root})")
    expect(root == Path("/tmp"),
           f"the lease root is /tmp ITSELF ({root})")
    # `stat`, not `lstat`: /tmp IS a symlink to /private/tmp on macOS,
    # and what has to be root-owned and sticky is the directory it names.
    info = root.stat()
    expect(info.st_uid == 0 and stat.S_IMODE(info.st_mode) & stat.S_ISVTX,
           f"which is root-owned and sticky here, so no unprivileged "
           f"account owns the namespace (uid {info.st_uid}, mode "
           f"{stat.S_IMODE(info.st_mode):04o})")
    expect(probe_flake._check_shared_dir(root) == root,
           "and the real /tmp passes every namespace check, symlinked or not")
    expect(probe_flake._lease_path(8009).parent == root,
           "lease files are FLAT in it — a harness-created subdirectory "
           "would be owned by whoever made it, and a directory's owner may "
           "unlink entries in it whatever the sticky bit says")
    with SyntheticTree() as shared:
        lease = probe_flake.PortLease.try_acquire(8009)
        try:
            file_mode = stat.S_IMODE(lease.path.lstat().st_mode)
            expect(file_mode & 0o066 == 0o066,
                   f"a lease file is readable and writable by other users, "
                   f"because a lock nobody else can open coordinates nobody "
                   f"else (mode {file_mode:04o})")
        finally:
            lease.release()
        with probe_flake.LiveRegistry() as registry:
            reg_mode = stat.S_IMODE(registry.path.lstat().st_mode)
            expect(reg_mode & 0o066 == 0o066,
                   f"so is a live-invocation registration (mode {reg_mode:04o})")
        # A symlinked root is FOLLOWED — `/tmp` is one on macOS — and
        # then judged on what it landed on, so a link into a directory
        # that fails any namespace check is still refused.
        elsewhere = shared.root / "elsewhere"
        elsewhere.mkdir()
        elsewhere.chmod(0o777)
        hostile = shared.root / "hostile"
        hostile.symlink_to(elsewhere)
        expect_raises(probe_flake.Rejection,
                      lambda: probe_flake._check_shared_dir(hostile),
                      "a symlink into a non-sticky directory is refused",
                      "is not sticky")
        elsewhere.chmod(0o1777)
        expect(probe_flake._check_shared_dir(hostile) == hostile,
               "while a symlink to a sound one is accepted, which is exactly "
               "how /tmp is reached on macOS")
        # A NON-STICKY directory is refused rather than repaired: in a
        # directory without the sticky bit any local user may unlink any
        # entry, so a held lease means nothing. Nothing here chmods a
        # shared directory — quietly widening someone else's permissions
        # would be a worse answer than stopping.
        loose = shared.root / "loose-leases"
        loose.mkdir()
        loose.chmod(0o777)
        expect_raises(probe_flake.Rejection,
                      lambda: probe_flake._check_shared_dir(loose),
                      "a non-sticky scratch directory is refused",
                      "is not sticky")
        expect(stat.S_IMODE(loose.lstat().st_mode) == 0o777,
               "and is left exactly as it was found")

        # THE PATHNAME-REPLACEMENT HOLE. A sticky directory still lets
        # its OWNER unlink anyone's entry, so a namespace owned by
        # another unprivileged account is refused outright — that
        # account could remove a held lease pathname and recreate it,
        # leaving two harnesses holding locks on different inodes for
        # one port. `uid` is a parameter precisely so this is testable:
        # a second local account cannot be created from a self-test.
        sticky = shared.root / "someone-elses-leases"
        sticky.mkdir()
        sticky.chmod(0o1777)
        expect(probe_flake._check_shared_dir(sticky, uid=os.getuid()) == sticky,
               "a sticky directory this user owns is accepted")
        asker = hand_to_third_party(sticky)
        expect_raises(probe_flake.Rejection,
                      lambda: probe_flake._check_shared_dir(
                          sticky, uid=asker),
                      "a sticky directory owned by ANOTHER unprivileged user "
                      "is refused, because its owner could replace a held "
                      "lease pathname",
                      "neither root nor this user")
        # Root-owned is the real case, and is accepted for any user.
        expect(probe_flake._check_shared_dir(
                   Path("/tmp"), uid=os.getuid() + 1) == Path("/tmp"),
               "a root-owned sticky directory is accepted whoever is running")

        # THE SYMLINK OVERWRITE. The lease directory is world-writable,
        # so a local user can plant a symlink at an unused port's lease
        # name pointing at a file a harness user can write. Following it
        # would fchmod, truncate and overwrite that target.
        victim = shared.root / "victim.txt"
        victim.write_text("precious", encoding="utf-8")
        victim.chmod(0o600)
        planted = probe_flake._lease_path(8100)
        planted.symlink_to(victim)
        expect(probe_flake.PortLease.try_acquire(8100) is None,
               "a symlinked lease name makes the port unavailable, never "
               "an overwrite")
        expect(victim.read_text(encoding="utf-8") == "precious"
               and stat.S_IMODE(victim.lstat().st_mode) == 0o600,
               f"the symlink's target is untouched — not truncated, not "
               f"chmodded (mode "
               f"{stat.S_IMODE(victim.lstat().st_mode):04o}, "
               f"{victim.read_text(encoding='utf-8')!r})")
        planted.unlink()

        # A planted HARD link is the same attack without a symlink.
        hardlinked = probe_flake._lease_path(8101)
        os.link(victim, hardlinked)
        expect(probe_flake.PortLease.try_acquire(8101) is None,
               "a hard-linked lease name makes the port unavailable too")
        expect(victim.read_text(encoding="utf-8") == "precious",
               "and its target survives as well")
        hardlinked.unlink()

        # So is a non-regular file (a fifo stands in for any of them).
        fifo = probe_flake._lease_path(8102)
        os.mkfifo(fifo)
        expect(probe_flake.PortLease.try_acquire(8102) is None,
               "a non-regular lease entry makes the port unavailable")
        fifo.unlink()

        # And a planted symlink in the registry is never counted live
        # nor followed.
        decoy = (probe_flake.LEASE_ROOT /
                 f"{probe_flake.SHARED_PREFIX}-live-1-decoy.json")
        decoy.symlink_to(victim)
        expect(probe_flake._registration_is_live(decoy) is False,
               "a symlinked registration is never counted as a live harness")
        expect(victim.exists() and victim.read_text(encoding="utf-8")
               == "precious",
               "and its target is neither read as a registration nor removed")
        decoy.unlink()

        # And the hole itself, demonstrated rather than argued: in a
        # directory whose OWNER is not root, unlinking a held lease's
        # pathname and recreating it leaves TWO harnesses holding locks
        # on different inodes for one port. This is what the ownership
        # check above refuses to operate in.
        owned = shared.root / "owner-can-replace"
        owned.mkdir()
        owned.chmod(0o1777)
        saved_root = probe_flake.LEASE_ROOT
        probe_flake.LEASE_ROOT = owned
        try:
            first = probe_flake.PortLease.try_acquire(8009)
            expect(first is not None, "a lease is held in the owned directory")
            expect(probe_flake.PortLease.try_acquire(8009) is None,
                   "and blocks a second acquire while the pathname stands")
            # The directory's owner may unlink it regardless of sticky.
            probe_flake._lease_path(8009).unlink()
            second = probe_flake.PortLease.try_acquire(8009)
            expect(first is not None and second is not None,
                   "but once the pathname is replaced BOTH harnesses hold a "
                   "lease for port 8009 — the hole a user-owned namespace "
                   "leaves open, and the reason /tmp itself is used")
            for lease in (first, second):
                if lease:
                    lease.release()
        finally:
            probe_flake.LEASE_ROOT = saved_root
        asker = hand_to_third_party(owned)
        expect_raises(probe_flake.Rejection,
                      lambda: probe_flake._check_shared_dir(
                          owned, uid=asker),
                      "so such a directory is refused for anyone who does not "
                      "own it", "neither root nor this user")

        # A regular file where the directory belongs is refused outright.
        notadir = shared.root / "not-a-dir"
        notadir.write_text("", encoding="utf-8")
        expect_raises(probe_flake.Rejection,
                      lambda: probe_flake._check_shared_dir(notadir),
                      "a non-directory scratch path is refused",
                      "is not a directory")

    # The regression itself, cross-PROCESS and cross-TMPDIR: two
    # harnesses whose TMPDIRs differ must still contend for one port.
    scratch = Path(tempfile.mkdtemp(prefix="probe-flake-xtmp-"))
    try:
        holder = scratch / "holder.py"
        holder.write_text(
            HOLDER_SRC.format(tools=TOOLS_DIR, port=TMPDIR_TEST_PORT),
            encoding="utf-8")
        (scratch / "a").mkdir()
        (scratch / "b").mkdir()
        env_a = {**os.environ, "TMPDIR": str(scratch / "a")}
        env_b = {**os.environ, "TMPDIR": str(scratch / "b")}
        first_proc = subprocess.Popen([sys.executable, str(holder)],
                                      stdout=subprocess.PIPE, text=True,
                                      env=env_a)
        try:
            said = first_proc.stdout.readline().strip()
            expect(said.startswith("held "),
                   f"the first harness (TMPDIR A) holds the port ({said!r})")
            done = subprocess.run([sys.executable, str(holder)],
                                  capture_output=True, text=True,
                                  env=env_b, timeout=60)
            other = (done.stdout.strip().splitlines() or [""])[0]
            expect(other.startswith("missed "),
                   f"a harness under a DIFFERENT TMPDIR is blocked by it "
                   f"({other!r})")
            expect(" " in said and " " in other
                   and said.split(" ", 1)[1] == other.split(" ", 1)[1],
                   f"both resolved the SAME lease namespace "
                   f"({said!r} vs {other!r})")
        finally:
            first_proc.kill()
            first_proc.wait(timeout=30)
    finally:
        shutil.rmtree(scratch, ignore_errors=True)


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
        leftovers = list(probe_flake.LEASE_ROOT.glob(
            probe_flake._registration_glob()))
        expect(leftovers == [],
               f"every departed harness cleans its registration up ({leftovers})")

        live_dir = probe_flake.LEASE_ROOT
        PREFIX = probe_flake.SHARED_PREFIX

        # THE PID-REUSE CASE. An abandoned registration naming a pid the
        # operating system has since handed to an unrelated live
        # process — modelled exactly, and most sharply, by naming THIS
        # process's own pid. A pid-and-age test reads it as a second
        # live harness forever and inflates every later measurement;
        # the lock test sees an unheld file and reaps it.
        recycled = live_dir / f"{PREFIX}-live-{os.getpid()}-recycled.json"
        recycled.write_text(
            json.dumps({"pid": os.getpid(), "started": 0.0}), encoding="utf-8")
        os.utime(recycled, (0, 0))
        with probe_flake.LiveRegistry() as registry:
            expect(registry.sample() == 1,
                   "a registration naming a REUSED (live) pid is not counted")
        expect(not recycled.exists(),
               "a registration naming a reused pid is reaped, not trusted")

        # The same, with a pid that is merely gone, and with a corrupt
        # entry: neither needs an age heuristic any more.
        stale = live_dir / f"{PREFIX}-live-999999-deadbeef.json"
        stale.write_text(json.dumps({"pid": _dead_pid(), "started": 0.0}),
                         encoding="utf-8")
        garbage = live_dir / f"{PREFIX}-live-garbage.json"
        garbage.write_text("not json", encoding="utf-8")
        fresh = live_dir / f"{PREFIX}-live-77777-justwritten.json"
        fresh.write_text("", encoding="utf-8")          # current mtime
        with probe_flake.LiveRegistry() as registry:
            expect(registry.sample() == 1,
                   "abandoned registrations are not counted whatever they say")
        expect(not stale.exists() and not garbage.exists()
               and not fresh.exists(),
               "an unheld registration is reaped immediately — being recent "
               "no longer protects it, because a live one is always locked")

        # And the converse: a registration a LIVE process holds is
        # counted even though this process could never verify its pid,
        # and killing that process outright makes it reapable with no
        # staleness window at all.
        holder_src = tree.root / "registry_holder.py"
        holder_src.write_text(
            REGISTRY_HOLDER_SRC.format(tools=TOOLS_DIR,
                                       root=str(probe_flake.LEASE_ROOT)),
            encoding="utf-8")
        holder = subprocess.Popen([sys.executable, str(holder_src)],
                                  stdout=subprocess.PIPE, text=True)
        held_path = None
        try:
            held_path = Path(holder.stdout.readline().strip())
            expect(held_path.exists(),
                   f"another process published its registration ({held_path})")
            with probe_flake.LiveRegistry() as registry:
                expect(registry.sample() == 2,
                       "a registration held by a live PROCESS is counted")
            expect(held_path.exists(),
                   "and is never reaped while its owner runs")
        finally:
            holder.kill()
            holder.wait(timeout=30)
        counted = None
        for _ in range(50):
            with probe_flake.LiveRegistry() as registry:
                counted = registry.sample()
            if counted == 1:
                break
            time.sleep(0.1)
        expect(counted == 1,
               "killing the owner outright drops its registration with no "
               "staleness heuristic at all")
        expect(held_path is not None and not held_path.exists(),
               "and the abandoned file is reaped")

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

        # Undecodable event bytes are malformed protocol input, so they
        # must reach the harness-error exit rather than raising
        # UnicodeDecodeError out of the measurement.
        os.environ["SYNTHETIC_MODE"] = "rawbytes"
        try:
            rc = probe_flake.main(["--probe", "synthetic", "--runs", "1",
                                   "--artifact-root", str(tree.artifacts())])
        finally:
            os.environ.pop("SYNTHETIC_MODE", None)
        expect(rc == probe_flake.EXIT_HARNESS_ERROR,
               f"an event stream of invalid UTF-8 exits "
               f"{probe_flake.EXIT_HARNESS_ERROR}, not a traceback (got {rc})")

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
    expect(migrated == ["lua_strict_msg", "position_hold", "role",
                        "text_encoding", "thermo_altitude"],
           f"lua_strict_msg, position_hold, role, text_encoding and "
           f"thermo_altitude are the probe-result/v1 probes, in "
           f"run_probes.PROBES order (got {migrated})")

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


# ==========================================================================
# position_hold's standalone behavior is preserved
# ==========================================================================
def test_position_hold_standalone() -> None:
    print("\n-- position_hold probe migration --")
    repo_root = Path(__file__).resolve().parent.parent
    done = subprocess.run(
        [sys.executable, "tools/position_hold_probe.py", "--describe"],
        cwd=repo_root, text=True, capture_output=True, timeout=60)
    expect(done.returncode == 0,
           "position_hold --describe exits 0 without booting anything")
    try:
        descriptor = probe_protocol.parse_descriptor(
            done.stdout, expected_probe="position_hold")
    except probe_protocol.ProtocolError as error:
        expect(False,
               f"position_hold's descriptor is valid probe-result/v1 ({error})")
        return
    expect(len(descriptor.ids) == 12,
           f"position_hold declares its twelve checks (got {len(descriptor.ids)})")
    expect(len(set(descriptor.ids)) == len(descriptor.ids),
           "position_hold's check identifiers are unique")
    expect(all(probe_protocol.CHECK_ID_RE.match(cid) for cid in descriptor.ids),
           "position_hold's identifiers are all stable, word-like identifiers")
    # The human labels these replaced interpolate observed tiles and
    # elapsed seconds; identity must carry none of that.
    expect(not any(any(ch.isdigit() for ch in cid) for cid in descriptor.ids),
           "position_hold's identifiers carry no runtime values")

    # Standalone mode still prints the bracketed human markers, and
    # protocol mode never does.
    import position_hold_probe  # type: ignore
    import io
    stream = io.StringIO()
    rep = probe_protocol.Reporter(position_hold_probe.DESCRIPTOR, stream=stream)
    rep.check("hold_sustained", True, "stayed within 0.05 tiles of the anchor")
    rep.abort("the commanded acolyte never arrived and never held")
    expect("[PASS] stayed within 0.05 tiles of the anchor" in stream.getvalue(),
           "standalone position_hold still prints its bracketed [PASS] line")
    expect("[FAIL] the commanded acolyte never arrived and never held"
           in stream.getvalue(),
           "standalone position_hold still prints a setup abort as [FAIL]")
    expect(rep.engine_args() == [],
           "standalone position_hold passes no RTS override")
    expect(rep.engine_log_path("position_hold_engine.log", "/tmp/x.log")
           == "/tmp/x.log",
           "standalone position_hold keeps its own engine-log path")

    with tempfile.TemporaryDirectory() as tmp:
        events = Path(tmp) / "events.jsonl"
        protocol_rep = probe_protocol.Reporter(
            position_hold_probe.DESCRIPTOR, events_path=str(events),
            engine_log_dir=tmp, rts_caps=4, stream=stream)
        before = stream.getvalue()
        protocol_rep.check("hold_created", True, "human text",
                           {"anchor": [4, 4]})
        protocol_rep.abort("setup failed")
        protocol_rep.close()
        expect(stream.getvalue() == before,
               "protocol mode prints nothing to stdout")
        expect(protocol_rep.engine_args() == ["+RTS", "-N4", "-RTS"],
               "protocol mode pins the engine to the harness's RTS capabilities")
        expect(protocol_rep.engine_log_path(
                   "position_hold_engine.log", "/tmp/x.log")
               == os.path.join(tmp, "position_hold_engine.log"),
               "protocol mode stops position_hold overwriting its shared /tmp "
               "engine log")
        text = events.read_text(encoding="utf-8")
        _events, outcomes = probe_protocol.parse_event_stream(
            text, position_hold_probe.DESCRIPTOR)
        expect(outcomes["hold_created"] == "PASS",
               "the protocol event stream carries the check outcome")
        expect(probe_protocol.forbidden_marker_lines(text) == [],
               "the event stream itself holds no bracketed marker lines")
        expect('"level": "WARN"' in text,
               "a setup abort is a WARN diagnostic in protocol mode, so the "
               "checks it prevented stay MISSING")


# ==========================================================================
# lua_strict_msg's standalone behavior is preserved
# ==========================================================================
def _drive_lua_strict_msg(rep, *, alive=True, console_error=None):
    """Drive the real strict-message control flow without booting an engine."""
    import lua_strict_msg_probe as strict_msg  # type: ignore

    launches: dict = {"commands": [], "sleeps": []}

    class FakeProc:
        def poll(self):
            return None if alive else 1

    def fake_boot(port, log=None, args=None, **_kw):
        launches["engine"] = {"port": port, "log": log,
                              "args": list(args or [])}
        return FakeProc()

    def fake_send(_port, lua, **kwargs):
        launches["commands"].append({"lua": lua, "kwargs": kwargs})
        if "engine.setText" in lua:
            return ""
        if "return 1+1" in lua:
            if console_error is not None:
                raise OSError(console_error)
            return "2"
        raise AssertionError(f"unexpected console command: {lua!r}")

    saved = (strict_msg.boot, strict_msg.quit_engine, strict_msg.send,
             strict_msg.time.sleep)
    strict_msg.boot = fake_boot
    strict_msg.quit_engine = lambda *a, **k: None
    strict_msg.send = fake_send
    strict_msg.time.sleep = lambda seconds: launches["sleeps"].append(seconds)
    try:
        rc = strict_msg._run(9622, rep)
    finally:
        (strict_msg.boot, strict_msg.quit_engine, strict_msg.send,
         strict_msg.time.sleep) = saved
    return rc, launches


def test_lua_strict_msg_standalone() -> None:
    print("\n-- lua_strict_msg probe migration --")
    repo_root = Path(__file__).resolve().parent.parent
    done = subprocess.run(
        [sys.executable, "tools/lua_strict_msg_probe.py", "--describe"],
        cwd=repo_root, text=True, capture_output=True, timeout=60)
    expect(done.returncode == 0,
           "lua_strict_msg --describe exits 0 without booting anything")
    try:
        descriptor = probe_protocol.parse_descriptor(
            done.stdout, expected_probe="lua_strict_msg")
    except probe_protocol.ProtocolError as error:
        expect(False, f"lua_strict_msg's descriptor is valid "
                      f"probe-result/v1 ({error})")
        return
    expect(descriptor.ids == ("engine_alive", "console_responsive"),
           f"lua_strict_msg declares its two stable checks in dependency "
           f"order (got {descriptor.ids})")

    import io
    import lua_strict_msg_probe as strict_msg  # type: ignore

    standalone = io.StringIO()
    standalone_rep = probe_protocol.Reporter(strict_msg.DESCRIPTOR,
                                             stream=standalone)
    rc, launches = _drive_lua_strict_msg(standalone_rep)
    expect(rc == 0, f"the standalone probe still exits 0 (got {rc})")
    expect(standalone.getvalue().count("[PASS]") == 2,
           "standalone mode prints one human [PASS] line per check")
    expect(launches["engine"]["args"] == [],
           "standalone mode passes no RTS override")
    expect(launches["engine"]["log"] == strict_msg.LOG,
           "standalone mode keeps its historical engine-log path")
    expect(launches["sleeps"] == [1.0],
           "the post-message crash window remains one second")
    malformed = launches["commands"][0]
    expect("caf\\195" in malformed["lua"]
           and malformed["kwargs"].get("expect_result") is False,
           "the malformed fire-and-forget message is unchanged")

    with tempfile.TemporaryDirectory() as tmp:
        events = Path(tmp) / "events.jsonl"
        protocol_stream = io.StringIO()
        protocol_rep = probe_protocol.Reporter(
            strict_msg.DESCRIPTOR, events_path=str(events),
            engine_log_dir=tmp, rts_caps=4, stream=protocol_stream)
        rc, launches = _drive_lua_strict_msg(protocol_rep)
        protocol_rep.close()
        expect(rc == 0, f"a clean protocol run exits 0 (got {rc})")
        expect(protocol_stream.getvalue() == "",
               "protocol mode prints nothing to stdout")
        expect(launches["engine"]["args"] == ["+RTS", "-N4", "-RTS"],
               "the harness's RTS capabilities reach the engine")
        expect(launches["engine"]["log"]
               == os.path.join(tmp, strict_msg.LOG_NAME),
               "the engine log is isolated inside the harness run")
        event_text = events.read_text(encoding="utf-8")
        _events, outcomes = probe_protocol.parse_event_stream(
            event_text, strict_msg.DESCRIPTOR)
        expect(outcomes == {"engine_alive": "PASS",
                            "console_responsive": "PASS"},
               f"the real probe flow reports both checks in order "
               f"(got {outcomes})")
        expect(probe_protocol.forbidden_marker_lines(event_text) == [],
               "the protocol event stream has no bracketed result markers")

    with tempfile.TemporaryDirectory() as tmp:
        events = Path(tmp) / "events.jsonl"
        dead_rep = probe_protocol.Reporter(
            strict_msg.DESCRIPTOR, events_path=str(events),
            engine_log_dir=tmp, rts_caps=4)
        rc, launches = _drive_lua_strict_msg(dead_rep, alive=False)
        dead_rep.close()
        event_text = events.read_text(encoding="utf-8")
        _events, outcomes = probe_protocol.parse_event_stream(
            event_text, strict_msg.DESCRIPTOR)
        expect(rc == 1, f"an exited engine makes the probe fail (got {rc})")
        expect(outcomes == {"engine_alive": "FAIL",
                            "console_responsive": "MISSING"},
               f"dead-engine attribution is FAIL then MISSING "
               f"(got {outcomes})")
        expect(len(launches["commands"]) == 1,
               "the probe does not query a console after its engine exited")
        expect('"level": "SKIP"' in event_text,
               "the uncheckable responsiveness assertion is diagnosed as SKIP")

    with tempfile.TemporaryDirectory() as tmp:
        events = Path(tmp) / "events.jsonl"
        unreachable_rep = probe_protocol.Reporter(
            strict_msg.DESCRIPTOR, events_path=str(events),
            engine_log_dir=tmp, rts_caps=4)
        rc, _launches = _drive_lua_strict_msg(
            unreachable_rep, console_error="connection dropped")
        unreachable_rep.close()
        _events, outcomes = probe_protocol.parse_event_stream(
            events.read_text(encoding="utf-8"), strict_msg.DESCRIPTOR)
        expect(rc == 1, f"an unreachable console makes the probe fail (got {rc})")
        expect(outcomes == {"engine_alive": "PASS",
                            "console_responsive": "FAIL"},
               f"console failure is attributed after liveness passes "
               f"(got {outcomes})")


# ==========================================================================
# text_encoding's standalone behavior is preserved
# ==========================================================================
def _drive_text_encoding(rep, *, malformed_call_result="no_error",
                         alive=True):
    """Drive the real text-encoding control flow without booting an engine."""
    import text_encoding_probe as text_encoding  # type: ignore

    launches: dict = {"commands": []}

    class FakeProc:
        def poll(self):
            return None if alive else 1

    def fake_boot(port, log=None, args=None, **_kw):
        launches["engine"] = {"port": port, "log": log,
                              "args": list(args or [])}
        return FakeProc()

    def fake_send(_port, lua, **_kw):
        launches["commands"].append(lua)
        if "engine.setText(1" in lua:
            return "no_error"
        if "engine.getText(1)" in lua:
            return "hello"
        if "engine.setText(2" in lua:
            return malformed_call_result
        if "engine.getText(2)" in lua:
            return "caf\ufffd"
        if 'world.show("no_such_page")' in lua:
            return "no_error"
        if 'world.show("caf' in lua and "no_such_page" not in lua:
            return "no_error"
        if "return 1+1" in lua:
            return "2"
        raise AssertionError(f"unexpected console command: {lua!r}")

    saved = (text_encoding.boot, text_encoding.quit_engine,
             text_encoding.send)
    text_encoding.boot = fake_boot
    text_encoding.quit_engine = lambda *a, **k: None
    text_encoding.send = fake_send
    try:
        args = argparse.Namespace(port=9618, describe=False)
        rc = text_encoding._run(args, args.port, rep)
    finally:
        (text_encoding.boot, text_encoding.quit_engine,
         text_encoding.send) = saved
    return rc, launches


def test_text_encoding_standalone() -> None:
    print("\n-- text_encoding probe migration --")
    repo_root = Path(__file__).resolve().parent.parent
    done = subprocess.run(
        [sys.executable, "tools/text_encoding_probe.py", "--describe"],
        cwd=repo_root, text=True, capture_output=True, timeout=60)
    expect(done.returncode == 0,
           "text_encoding --describe exits 0 without booting anything")
    try:
        descriptor = probe_protocol.parse_descriptor(
            done.stdout, expected_probe="text_encoding")
    except probe_protocol.ProtocolError as error:
        expect(False, f"text_encoding's descriptor is valid "
                      f"probe-result/v1 ({error})")
        return
    expect(len(descriptor.ids) == 8,
           f"text_encoding declares its eight checks "
           f"(got {len(descriptor.ids)})")
    expect(len(set(descriptor.ids)) == len(descriptor.ids),
           "text_encoding's check identifiers are unique")
    expect(all(probe_protocol.CHECK_ID_RE.match(cid) for cid in descriptor.ids),
           "text_encoding's identifiers are stable word-like identifiers")
    expect(not any(any(ch.isdigit() for ch in cid) for cid in descriptor.ids),
           "text_encoding's identifiers carry no runtime values")

    import io
    import text_encoding_probe as text_encoding  # type: ignore

    standalone = io.StringIO()
    standalone_rep = probe_protocol.Reporter(text_encoding.DESCRIPTOR,
                                             stream=standalone)
    rc, launches = _drive_text_encoding(standalone_rep)
    expect(rc == 0, f"the standalone probe still exits 0 (got {rc})")
    expect(standalone.getvalue().count("[PASS]") == 8,
           "standalone mode prints one human [PASS] line per check")
    expect(launches["engine"]["args"] == [],
           "standalone mode passes no RTS override")
    expect(launches["engine"]["log"] == text_encoding.LOG,
           "standalone mode keeps its historical engine-log path")

    with tempfile.TemporaryDirectory() as tmp:
        events = Path(tmp) / "events.jsonl"
        protocol_stream = io.StringIO()
        protocol_rep = probe_protocol.Reporter(
            text_encoding.DESCRIPTOR, events_path=str(events),
            engine_log_dir=tmp, rts_caps=4, stream=protocol_stream)
        rc, launches = _drive_text_encoding(protocol_rep)
        protocol_rep.close()
        expect(rc == 0, f"a clean protocol run exits 0 (got {rc})")
        expect(protocol_stream.getvalue() == "",
               "protocol mode prints nothing to stdout")
        expect(launches["engine"]["args"] == ["+RTS", "-N4", "-RTS"],
               "the harness's RTS capabilities reach the engine")
        expect(launches["engine"]["log"]
               == os.path.join(tmp, text_encoding.LOG_NAME),
               "the engine log is isolated inside the harness run")
        event_text = events.read_text(encoding="utf-8")
        _events, outcomes = probe_protocol.parse_event_stream(
            event_text, text_encoding.DESCRIPTOR)
        expect(all(outcome == "PASS" for outcome in outcomes.values()),
               f"the real probe flow reports all eight checks in order "
               f"(got {outcomes})")
        expect(probe_protocol.forbidden_marker_lines(event_text) == [],
               "the protocol event stream has no bracketed result markers")

    # One behavior failure is a check failure, not a harness error, and
    # does not prevent later independent checks from being reported.
    with tempfile.TemporaryDirectory() as tmp:
        events = Path(tmp) / "events.jsonl"
        failing_rep = probe_protocol.Reporter(
            text_encoding.DESCRIPTOR, events_path=str(events),
            engine_log_dir=tmp, rts_caps=4)
        rc, _launches = _drive_text_encoding(
            failing_rep, malformed_call_result="lua_error")
        failing_rep.close()
        _events, outcomes = probe_protocol.parse_event_stream(
            events.read_text(encoding="utf-8"), text_encoding.DESCRIPTOR)
        expect(rc == 1, f"a malformed-setText regression exits 1 (got {rc})")
        expect(outcomes["malformed_text_call"] == "FAIL",
               "the malformed setText regression is attributed to its check")
        expect(outcomes["console_responsive"] == "PASS",
               "later independent checks are still reported after a failure")

    # If the process has died, the liveness check fails and the dependent
    # final responsiveness check honestly remains MISSING.
    with tempfile.TemporaryDirectory() as tmp:
        events = Path(tmp) / "events.jsonl"
        dead_rep = probe_protocol.Reporter(
            text_encoding.DESCRIPTOR, events_path=str(events),
            engine_log_dir=tmp, rts_caps=4)
        rc, launches = _drive_text_encoding(dead_rep, alive=False)
        dead_rep.close()
        event_text = events.read_text(encoding="utf-8")
        _events, outcomes = probe_protocol.parse_event_stream(
            event_text, text_encoding.DESCRIPTOR)
        expect(rc == 1, f"an exited engine makes the probe fail (got {rc})")
        expect(outcomes["engine_alive"] == "FAIL"
               and outcomes["console_responsive"] == "MISSING",
               f"dead-engine attribution is FAIL then MISSING (got {outcomes})")
        expect(not any("return 1+1" in cmd for cmd in launches["commands"]),
               "the probe does not query a console after its engine exited")
        expect('"level": "SKIP"' in event_text,
               "the uncheckable responsiveness assertion is diagnosed as SKIP")


# ==========================================================================
# thermo_altitude's standalone behavior is preserved
# ==========================================================================
def _thermo_console(sweep: str):
    """A canned debug console for `thermo_altitude_probe._run`.

    Engine-free on purpose: the migration's sequencing and its MISSING
    outcome are decided entirely by the probe's own control flow, so
    they can be driven without generating a 128-tile world.
    """
    def fake_send(_port, lua, **_kw):
        if "world.init(" in lua:
            return "ok"
        if "getInitProgress" in lua:
            return "3"
        if "world.initArena(" in lua:
            return "ok"
        if "world.show(" in lua:
            return "shown"
        if "world.hide(" in lua:
            return "hidden"
        if "string.format" in lua:
            return f'"{sweep}"'
        if "getActiveWorldId" in lua:
            return '"t308"'
        if "getClimateAt(0,0)" in lua:
            return "10.00"
        if "getAmbientAt(0,0)" in lua:
            return "10.00"
        if "getAmbientAt(" in lua:
            return "-4.00"
        raise AssertionError(f"unexpected console command: {lua[:80]!r}")
    return fake_send


def _drive_thermo(rep, sweep: str, dump_returncode: int = 0,
                  dump_stdout: str = "[]", seed: int = 42, size: int = 128):
    """Run the real `_run` against fake launches; return `(rc, launches)`.

    `seed`/`size` are the requested generation inputs, so a caller can
    drive a size the engine NORMALIZES (#1757).
    """
    import types
    import thermo_altitude_probe as thermo  # type: ignore

    launches: dict = {}
    console_lua: list[str] = []

    def fake_boot(port, log=None, args=None, **_kw):
        launches["console"] = {"port": port, "log": log, "args": list(args or [])}
        return object()

    def fake_run(cmd, stdout=None, stderr=None, text=None, **_kw):
        launches["dump"] = {"cmd": list(cmd), "stderr": getattr(stderr, "name", None),
                            "stdout_piped": stdout is subprocess.PIPE}
        return types.SimpleNamespace(returncode=dump_returncode,
                                     stdout=dump_stdout)

    console = _thermo_console(sweep)

    def recording_send(port, lua, **kw):
        # The console half of the world-parameter comparison: `world.init`
        # is a formatted Lua string, so the only way to see what the FIRST
        # launch generated is to capture what was sent (#1757).
        console_lua.append(lua)
        return console(port, lua, **kw)

    saved = (thermo.boot, thermo.quit_engine, thermo.send, thermo.time,
             thermo.subprocess)
    thermo.boot = fake_boot
    thermo.quit_engine = lambda *a, **k: None
    thermo.send = recording_send
    thermo.time = types.SimpleNamespace(sleep=lambda _s: None)
    thermo.subprocess = types.SimpleNamespace(run=fake_run,
                                              PIPE=subprocess.PIPE)
    try:
        args = argparse.Namespace(port=9171, seed=seed, size=size,
                                  describe=False)
        rc = thermo._run(args, args.port, rep)
    finally:
        (thermo.boot, thermo.quit_engine, thermo.send, thermo.time,
         thermo.subprocess) = saved
    launches["console_lua"] = console_lua
    return rc, launches


def _thermo_init_params(console_lua):
    """`(seed, size, plates)` the console launch's `world.init` asked for."""
    for lua in console_lua:
        match = re.search(r"world\.init\(\s*\"[^\"]*\"\s*,\s*"
                          r"(-?\d+)\s*,\s*(-?\d+)\s*,\s*(-?\d+)\s*\)", lua)
        if match:
            return tuple(int(group) for group in match.groups())
    return None


def _thermo_reported_line(printed):
    """The one standalone line naming the world-generation parameters."""
    lines = [line for line in printed.splitlines()
             if "seed" in line and "plates" in line]
    return lines


def _thermo_spoken(line, labels=("seed", "world size", "plates")):
    """The integers a standalone parameter line names, by label."""
    return tuple(int(found.group(1)) if found else None
                 for found in (re.search(rf"{label}\s+(-?\d+)", line)
                               for label in labels))


def _thermo_dump_params(cmd):
    """`(seed, size, plates)` the dump launch's argv asked for.

    Reads the values positionally out of the real argv rather than
    trusting a formatted string, so a flag renamed or dropped shows up as
    a missing value instead of a silent default (#1757).
    """
    values = {}
    for flag, key in (("--seed", "seed"), ("--worldSize", "size"),
                      ("--plates", "plates")):
        if flag in cmd:
            index = cmd.index(flag)
            if index + 1 < len(cmd):
                try:
                    values[key] = int(cmd[index + 1])
                except ValueError:
                    return None
    if set(values) != {"seed", "size", "plates"}:
        return None
    return values["seed"], values["size"], values["plates"]


def test_thermo_altitude_standalone() -> None:
    print("\n-- thermo_altitude probe migration --")
    repo_root = Path(__file__).resolve().parent.parent
    done = subprocess.run(
        [sys.executable, "tools/thermo_altitude_probe.py", "--describe"],
        cwd=repo_root, text=True, capture_output=True, timeout=60)
    expect(done.returncode == 0,
           "thermo_altitude --describe exits 0 without booting anything")
    try:
        descriptor = probe_protocol.parse_descriptor(
            done.stdout, expected_probe="thermo_altitude")
    except probe_protocol.ProtocolError as error:
        expect(False, f"thermo_altitude's descriptor is valid "
                      f"probe-result/v1 ({error})")
        return
    expect(len(descriptor.ids) == 5,
           f"thermo_altitude declares its five checks (got {len(descriptor.ids)})")
    expect(len(set(descriptor.ids)) == len(descriptor.ids),
           "thermo_altitude's check identifiers are unique")
    expect(all(probe_protocol.CHECK_ID_RE.match(cid) for cid in descriptor.ids),
           "thermo_altitude's identifiers are all stable, word-like identifiers")
    # The labels these replaced led with their ordinal (`1 safety`,
    # `4 ice agreement`) and interpolated observed temperatures.
    expect(not any(any(ch.isdigit() for ch in cid) for cid in descriptor.ids),
           "thermo_altitude's identifiers carry no runtime values")
    # Ice agreement is the ONE check allowed to end up MISSING, and
    # `Reporter.skip` does not advance the declared sequence, so it must
    # be declared last or a following check would be a harness error.
    expect(descriptor.ids[-1] == "ice_agreement",
           f"ice agreement is the last declared check, so its skip cannot "
           f"strand a successor (got {descriptor.ids})")

    # Standalone mode still prints the bracketed human markers, and
    # protocol mode never does.
    import thermo_altitude_probe as thermo  # type: ignore
    import io
    stream = io.StringIO()
    rep = probe_protocol.Reporter(thermo.DESCRIPTOR, stream=stream)
    rep.check("safety", True, "getAmbientAt never exceeds the regional mean")
    rep.abort("world never finished generating")
    expect("[PASS] getAmbientAt never exceeds the regional mean"
           in stream.getvalue(),
           "standalone thermo_altitude still prints its bracketed [PASS] line")
    expect("[FAIL] world never finished generating" in stream.getvalue(),
           "standalone thermo_altitude still prints a setup abort as [FAIL]")
    expect(rep.engine_args() == [],
           "standalone thermo_altitude passes no RTS override")
    expect(rep.engine_log_path(thermo.CONSOLE_LOG_NAME, thermo.CONSOLE_LOG)
           == thermo.CONSOLE_LOG,
           "standalone thermo_altitude keeps its own console engine-log path")
    expect(rep.engine_log_path(thermo.DUMP_LOG_NAME, thermo.DUMP_LOG)
           == thermo.DUMP_LOG,
           "standalone thermo_altitude keeps its own dump engine-log path")

    # The whole run, engine-free: both launches wired through the
    # reporter, four checks reported, and the fifth left MISSING.
    sweep = "0|-300,-300,-12.00|100,100,25.00,24.00|200,240,5.00,-2.00"
    with tempfile.TemporaryDirectory() as tmp:
        events = Path(tmp) / "events.jsonl"
        protocol_rep = probe_protocol.Reporter(
            thermo.DESCRIPTOR, events_path=str(events),
            engine_log_dir=tmp, rts_caps=4, stream=stream)
        before = stream.getvalue()
        rc, launches = _drive_thermo(protocol_rep, sweep)
        protocol_rep.close()
        expect(stream.getvalue() == before,
               "protocol mode prints nothing to stdout")
        expect(rc == 0,
               f"a run whose only unreported check was skipped still exits 0 "
               f"(got {rc})")

        # BOTH engine launches, not just the console one: the dump is
        # the specific way this migration goes wrong.
        console = launches.get("console", {})
        dump = launches.get("dump", {})
        expect(console.get("args") == ["+RTS", "-N4", "-RTS"],
               f"the console engine gets the harness's RTS capabilities "
               f"(got {console.get('args')})")
        expect(dump.get("cmd", [])[-3:] == ["+RTS", "-N4", "-RTS"],
               f"the ice-dump engine gets them too "
               f"(got {dump.get('cmd', [])[-3:]})")
        expect(console.get("log") == os.path.join(tmp, thermo.CONSOLE_LOG_NAME),
               f"the console engine logs into the harness's run directory "
               f"(got {console.get('log')})")
        expect(dump.get("stderr") == os.path.join(tmp, thermo.DUMP_LOG_NAME),
               f"the ice-dump engine's stderr does too "
               f"(got {dump.get('stderr')})")
        expect(console.get("log") != dump.get("stderr"),
               "the two launches use DISTINCT reporter-selected engine logs, "
               "so neither overwrites the other")
        expect(dump.get("stdout_piped") is True,
               "the dump's stdout stays a pipe, since its JSON is the payload")

        # ONE WORLD, TWO LAUNCHES (#1757). `ice_agreement` reads ice
        # coordinates out of the dump and samples ambient in the console
        # world, so the two engines must be given the same seed, world
        # size AND plate count. The dump used to be handed no plate count
        # at all and resolved the engine's `defaultPlatesFor` (9 at size
        # 128) against the console's literal 5.
        init_params = _thermo_init_params(launches.get("console_lua", []))
        dump_params = _thermo_dump_params(dump.get("cmd", []))
        expect(init_params is not None,
               f"the console launch's world.init names seed, world size and "
               f"plate count (got {launches.get('console_lua', [])[:1]})")
        expect(dump_params is not None,
               f"the dump launch's argv names --seed, --worldSize AND "
               f"--plates (got {dump.get('cmd', [])})")
        expect(init_params == dump_params,
               f"both engine launches generate the SAME world: console "
               f"{init_params} vs dump {dump_params} (seed, size, plates)")
        expect(init_params == (42, 128, thermo.PLATE_COUNT),
               f"both launches use the probe's single plate-count source "
               f"(got {init_params}, PLATE_COUNT={thermo.PLATE_COUNT})")
        expect("--plates" in dump.get("cmd", [])
               and "--ages" not in dump.get("cmd", []),
               "the dump uses the canonical --plates flag, not the legacy "
               "--ages alias")

        text = events.read_text(encoding="utf-8")
        _events, outcomes = probe_protocol.parse_event_stream(
            text, thermo.DESCRIPTOR)
        expect(all(outcomes[cid] == "PASS" for cid in
                   ("safety", "bug_fix", "monotone", "arena_safety")),
               f"every check preceding ice agreement is reported (got {outcomes})")
        expect(outcomes["ice_agreement"] == "MISSING",
               f"an unsampleable ice region leaves ice agreement MISSING "
               f"rather than vacuously passing (got {outcomes['ice_agreement']})")
        expect('"level": "SKIP"' in text,
               "the empty ice sample is reported as a SKIP diagnostic")
        expect(probe_protocol.forbidden_marker_lines(text) == [],
               "the event stream itself holds no bracketed marker lines")

        # The parameter report reaches the STRUCTURED channel, and does
        # so on this very run — the one whose ice_agreement is MISSING,
        # which is where a reader most needs to know which world was
        # measured (#1757).
        reported = [event for event in _events
                    if isinstance(event, probe_protocol.DiagnosticEvent)
                    and {"seed", "world_size", "plates"} <= set(event.detail)]
        expect(len(reported) == 1,
               f"exactly one diagnostic event carries the world-generation "
               f"parameters (got {len(reported)})")
        if reported:
            detail = reported[0].detail
            expect((detail["seed"], detail["world_size"], detail["plates"])
                   == init_params,
                   f"the reported parameters are the ones both launches "
                   f"actually used (reported {detail}, launched {init_params})")

    # A FAILED second launch is a setup failure, never a MISSING check:
    # nonzero exit, undecodable stdout, and a non-list payload each abort.
    for label, rc_in, stdout_in in (
            ("a nonzero dump exit", 1, "[]"),
            ("undecodable dump stdout", 0, "cabal: error\n"),
            ("a non-list dump payload", 0, '{"tiles": []}')):
        with tempfile.TemporaryDirectory() as tmp:
            events = Path(tmp) / "events.jsonl"
            failing_rep = probe_protocol.Reporter(
                thermo.DESCRIPTOR, events_path=str(events),
                engine_log_dir=tmp, rts_caps=4, stream=stream)
            rc, _launches = _drive_thermo(failing_rep, sweep,
                                          dump_returncode=rc_in,
                                          dump_stdout=stdout_in)
            failing_rep.close()
            text = events.read_text(encoding="utf-8")
            _events, outcomes = probe_protocol.parse_event_stream(
                text, thermo.DESCRIPTOR)
            expect(rc == 1, f"{label} exits the probe nonzero (got {rc})")
            expect(outcomes["ice_agreement"] == "MISSING",
                   f"{label} leaves ice agreement MISSING")
            expect('"level": "SKIP"' not in text,
                   f"{label} is never reported as a legitimate skip")
            expect('"level": "WARN"' in text,
                   f"{label} is reported as a setup abort")

    # ...and the SAME parameters reach standalone output, where
    # `Reporter._diagnostic` prints only the human text and drops the
    # detail dict entirely (#1757).
    standalone = io.StringIO()
    standalone_rep = probe_protocol.Reporter(thermo.DESCRIPTOR,
                                             stream=standalone)
    rc, standalone_launches = _drive_thermo(standalone_rep, sweep)
    printed = standalone.getvalue()
    standalone_init = _thermo_init_params(
        standalone_launches.get("console_lua", []))
    expect(rc == 0, f"the standalone drive still exits 0 (got {rc})")
    expect(standalone_init == (42, 128, thermo.PLATE_COUNT),
           f"the standalone drive generates the same single-sourced world "
           f"(got {standalone_init})")
    parameter_lines = _thermo_reported_line(printed)
    expect(len(parameter_lines) == 1,
           f"standalone output carries exactly one world-parameter line "
           f"(got {parameter_lines})")
    line = parameter_lines[0] if parameter_lines else ""
    spoken = _thermo_spoken(line)
    expect(spoken == standalone_init,
           f"the standalone parameter line names seed, world size and plate "
           f"count by value (read {spoken} from {line!r}, launched "
           f"{standalone_init})")
    expect("requested" not in line,
           f"a size the engine does not normalize is reported plainly, with "
           f"no request/effective split (got {line!r})")

    # A size the engine NORMALIZES: `normalizeWorldSize` rounds 129 up to
    # 136, so reporting the REQUEST would name a world that was never
    # generated. Both launches still receive the same raw request and
    # normalize it identically, so they still generate ONE world (#1757).
    expect((thermo.normalize_world_size(129),
            thermo.normalize_world_size(128),
            thermo.normalize_world_size(1),
            thermo.normalize_world_size(thermo.MINIMUM_WORLD_SIZE))
           == (136, 128, thermo.MINIMUM_WORLD_SIZE, thermo.MINIMUM_WORLD_SIZE),
           "the probe mirrors normalizeWorldSize: round up to a multiple of "
           "the minimum, and clamp below it")
    expect(thermo.normalize_plate_count(thermo.PLATE_COUNT)
           == thermo.PLATE_COUNT,
           f"the probe's own plate count is already normal "
           f"(got {thermo.normalize_plate_count(thermo.PLATE_COUNT)})")

    normalizing = io.StringIO()
    with tempfile.TemporaryDirectory() as tmp:
        events = Path(tmp) / "events.jsonl"
        normalizing_rep = probe_protocol.Reporter(
            thermo.DESCRIPTOR, events_path=str(events), engine_log_dir=tmp,
            stream=normalizing)
        rc, odd_launches = _drive_thermo(normalizing_rep, sweep, size=129)
        normalizing_rep.close()
        odd_events, _outcomes = probe_protocol.parse_event_stream(
            events.read_text(encoding="utf-8"), thermo.DESCRIPTOR)
        expect(rc == 0, f"the normalizing drive still exits 0 (got {rc})")
        odd_init = _thermo_init_params(odd_launches.get("console_lua", []))
        odd_dump = _thermo_dump_params(odd_launches.get("dump", {}).get("cmd", []))
        expect(odd_init == odd_dump == (42, 129, thermo.PLATE_COUNT),
               f"both launches still request the identical world, normalizing "
               f"or not (console {odd_init}, dump {odd_dump})")
        odd_reported = [event.detail for event in odd_events
                        if isinstance(event, probe_protocol.DiagnosticEvent)
                        and {"seed", "world_size", "plates"} <= set(event.detail)]
        expect(len(odd_reported) == 1,
               f"the normalizing run reports its parameters exactly once "
               f"(got {len(odd_reported)})")
        if odd_reported:
            expect(odd_reported[0].get("world_size") == 136
                   and odd_reported[0].get("requested_world_size") == 129,
                   f"the structured report names the GENERATED size 136 and "
                   f"keeps the requested 129 (got {odd_reported[0]})")

    odd_lines = _thermo_reported_line(normalizing.getvalue())
    expect(len(odd_lines) == 0,
           f"protocol mode still prints no parameter line (got {odd_lines})")

    plain = io.StringIO()
    rc, _ = _drive_thermo(
        probe_protocol.Reporter(thermo.DESCRIPTOR, stream=plain),
        sweep, size=129)
    odd_lines = _thermo_reported_line(plain.getvalue())
    expect(len(odd_lines) == 1,
           f"the normalizing standalone run prints one parameter line "
           f"(got {odd_lines})")
    odd_line = odd_lines[0] if odd_lines else ""
    expect(_thermo_spoken(odd_line) == (42, 136, thermo.PLATE_COUNT),
           f"standalone names the GENERATED world size, not the request "
           f"(read {_thermo_spoken(odd_line)} from {odd_line!r})")
    expect(_thermo_spoken(odd_line, ("requested",)) == (129,),
           f"and still names the request that produced it "
           f"(got {odd_line!r})")

    # The non-empty sampling path, also engine-free: a warm ice tile fails.
    stream = io.StringIO()
    rep = probe_protocol.Reporter(thermo.DESCRIPTOR, stream=stream)
    tiles = [{"x": 10, "y": 20, "iceSurf": 4},
             {"x": 11, "y": 20, "iceSurf": 4, "glacierZone": True},
             {"x": 12, "y": 20, "iceSurf": None}]
    expect(len(thermo.interior_ice(tiles)) == 1,
           "polar glacier bands and ice-free tiles are excluded from the sample")
    expect(thermo.report_ice_agreement(rep, tiles, (0, 0, 1, 1),
                                       lambda _x, _y: -3.0) is True,
           "ice tiles at/below freezing pass ice agreement")
    expect(thermo.report_ice_agreement(rep, tiles, (0, 0, 1, 1),
                                       lambda _x, _y: 9.0) is False,
           "an ice tile reading above freezing fails ice agreement")
    expect(thermo.report_ice_agreement(rep, tiles, (0, 0, 1, 1),
                                       lambda _x, _y: None) is False,
           "an unreadable ambient on an ice tile fails ice agreement")


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
                 test_port_spans,
                 test_measure_leases_the_probes_whole_declared_span,
                 test_concurrent_leasing,
                 test_lease_root_is_tmpdir_independent,
                 test_concurrency_accounting, test_artifacts,
                 test_no_tmpdir_default, test_result_document,
                 test_exit_codes, test_render, test_manifest_fixture,
                 test_manifest_real_registry, test_role_standalone,
                 test_position_hold_standalone,
                 test_lua_strict_msg_standalone,
                 test_text_encoding_standalone,
                 test_thermo_altitude_standalone,
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
