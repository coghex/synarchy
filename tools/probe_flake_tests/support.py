#!/usr/bin/env python3
"""The fixtures and helpers the flake self-test's owners share (#2087).

`tools/test_probe_flake.py` stays the aggregate command; this module
holds what more than one of its owners needs, so the harness owners and
the twenty-two per-probe migration owners single-source it instead of
carrying a copy each.

Three things live here:

  the synthetic protocol probe  `SYNTHETIC_PROBE`, `SyntheticTree` and
                                `run_synthetic` — the throwaway checkout
                                every harness owner drives `probe_flake`
                                against, with the module globals it
                                redirects and restores;
  the assertion helpers         `expect_raises` and the shared `SKIPS`
                                list `skip` appends to, which the
                                aggregate and every focused run report
                                from;
  the migration drivers         `migration_descriptor`, which subprocesses
                                one real `tools/<probe>_probe.py
                                --describe` from the real repository root,
                                and `batch_contract`, the one compatibility
                                contract the ten batch-migrated probes
                                share.

This module imports no case owner, so importing it runs nothing and
registers nothing. It is `NON_OWNER_MODULES` in the facade, which
cross-checks that roster against the modules actually on disk.

Anchoring note: every path here is derived from `TOOLS_DIR`/`REPO_ROOT`
below rather than from a case owner's own `__file__`, because the owners
sit one directory deeper than the single file they were split out of and
a relative `.parent` would silently move.
"""
from __future__ import annotations

import argparse
import json
import os
import shutil
import subprocess
import sys
import tempfile
import textwrap
from pathlib import Path

TOOLS_DIR = str(Path(__file__).resolve().parent.parent)
REPO_ROOT = Path(__file__).resolve().parent.parent.parent

sys.path.insert(0, TOOLS_DIR)
# Every production module the owners drive is imported HERE and
# re-exported, so all of them -- and the module-global seams the harness
# fixtures redirect -- are one set of objects for every owner, whatever
# spelling loaded this package.
import ci_probes  # type: ignore  # noqa: E402
import probe_census  # type: ignore  # noqa: E402
import probe_engine  # type: ignore  # noqa: E402
import probe_flake  # type: ignore  # noqa: E402
import probe_protocol  # type: ignore  # noqa: E402
import probe_runner_lifecycle  # type: ignore  # noqa: E402
import probe_runner_registry  # type: ignore  # noqa: E402

import selftestlib  # noqa: E402
from selftestlib import expect  # noqa: E402

#: Appended to by `skip` and reported once by the facade, so a focused
#: run and the aggregate render the same skip block from one list.
SKIPS: list[str] = []

def expect_raises(exc, fn, msg: str, substring: str | None = None) -> None:
    # The conditions, the registered text and the printed detail are all
    # unchanged; only the reporting goes through the shared helper, so
    # this counts as one assertion and stays quiet when it holds (#1922).
    try:
        fn()
    except exc as error:
        if substring is not None and substring not in str(error):
            selftestlib.record_fail(
                f"{msg} (raised {exc.__name__} but not about "
                f"{substring!r}: {error})",
                f"{msg} — wrong message: {error}")
            return
        selftestlib.record_pass(msg)
        return
    except Exception as error:  # noqa: BLE001 - a wrong exception is a failure
        selftestlib.record_fail(
            f"{msg} (raised {type(error).__name__}: {error})",
            f"{msg} — raised {type(error).__name__}: {error}")
        return
    selftestlib.record_fail(f"{msg} (nothing raised)", f"{msg} — nothing raised")


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
            "REPO_ROOT": probe_engine.REPO_ROOT,
            "PROBES": probe_runner_registry.PROBES,
            "CI_ELIGIBLE": ci_probes.CI_ELIGIBLE,
            "PROTOCOL_PROBES": probe_flake.PROTOCOL_PROBES,
            "LEASE_ROOT": probe_flake.LEASE_ROOT,
        }
        probe_engine.REPO_ROOT = str(self.root)
        probe_runner_registry.PROBES = self.probes
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
        probe_engine.REPO_ROOT = self._saved["REPO_ROOT"]
        probe_runner_registry.PROBES = self._saved["PROBES"]
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



def migration_descriptor(script: str, probe: str, expected_ids: tuple[str, ...]):
    done = subprocess.run(
        [sys.executable, f"tools/{script}", "--describe"],
        cwd=REPO_ROOT, text=True, capture_output=True, timeout=60)
    expect(done.returncode == 0,
           f"{probe} --describe exits 0 without booting anything")
    try:
        descriptor = probe_protocol.parse_descriptor(
            done.stdout, expected_probe=probe)
    except probe_protocol.ProtocolError as error:
        expect(False, f"{probe}'s descriptor is valid probe-result/v1 ({error})")
        return None
    expect(descriptor.ids == expected_ids,
           f"{probe} declares its stable checks in execution order "
           f"(got {descriptor.ids})")
    return descriptor



# ==========================================================================
# The batch-migrated probes' shared compatibility contract
# ==========================================================================
class StopBeforeEngine(BaseException):
    """Raised by the fake `boot` so no probe ever reaches a real engine.

    `BaseException` rather than `Exception` deliberately: a probe's own
    `_run` may wrap its body in `except Exception`, and this has to cut
    through that rather than be swallowed and reported as a probe
    failure.
    """


def default_invoke(module, port: int, rep) -> None:
    """`_run(port, reporter)` — the shape most migrated probes use."""
    module._run(port, rep)


def namespace_invoke(**fields):
    """`_run(Namespace(port=..., **fields), reporter)` for the rest.

    The probes whose `_run` takes a parsed argument namespace each name
    their own extra options here, in their own module, instead of a
    branch in one shared driver.
    """
    def invoke(module, port: int, rep) -> None:
        module._run(argparse.Namespace(port=port, **fields), rep)
    return invoke


def batch_contract(key: str, script: str, port: int, ids, *,
                   invoke=default_invoke, patch=None) -> None:
    """One batch-migrated probe's descriptor, reporting and wiring contract.

    Identical for every probe that shares it, which is why it is single-
    sourced here: the descriptor is pure and matches the declared check
    order, standalone reporting stays bracketed and human-readable,
    a failed assertion is attributed to a stable check id in the event
    stream, and the probe's real `_run` reaches its first engine boot
    with standalone wiring in standalone mode and the harness's isolated
    log and RTS capability count in protocol mode.

    `ids` may be a tuple or a zero-argument callable, so a probe that
    derives its identifiers from its own `PROBE_CHECKS` table does that
    when the contract runs rather than at import time. `patch` is an
    optional callable taking the probe module and returning a restore
    callable, for a probe whose `_run` would otherwise touch the working
    tree before it boots.
    """
    import io

    print(f"\n-- {key} probe migration --")
    expected = tuple(ids() if callable(ids) else ids)
    descriptor = migration_descriptor(script, key, expected)
    if descriptor is None:
        return
    module = __import__(script.removesuffix(".py"))

    stream = io.StringIO()
    standalone = probe_protocol.Reporter(descriptor, stream=stream)
    standalone.check(expected[0], False, descriptor.label(expected[0]))
    standalone.close()
    expect("[FAIL]" in stream.getvalue(),
           f"{key} standalone failures remain human-readable")

    with tempfile.TemporaryDirectory(prefix=f"{key}-migration-") as tmp:
        events = Path(tmp) / "events.jsonl"
        protocol = probe_protocol.Reporter(
            descriptor, events_path=str(events), stream=io.StringIO())
        protocol.check(expected[0], False, descriptor.label(expected[0]),
                       {"synthetic": True})
        protocol.close()
        _, outcomes = probe_protocol.parse_event_stream(
            events.read_text(encoding="utf-8"), descriptor)
        expect(outcomes[expected[0]] == probe_protocol.FAIL,
               f"{key} attributes a failed assertion to its stable first id")

        launches = []
        saved_boot = module.boot
        restore_patch = None

        def fake_boot(got_port, *pos, **kwargs):
            launches.append({
                "port": got_port,
                "log": kwargs.get("log", pos[0] if pos else None),
                "args": list(kwargs.get("args") or []),
            })
            raise StopBeforeEngine()

        module.boot = fake_boot
        if patch is not None:
            restore_patch = patch(module)

        try:
            for rep in (
                probe_protocol.Reporter(descriptor, stream=io.StringIO()),
                probe_protocol.Reporter(
                    descriptor, engine_log_dir=tmp, rts_caps=3,
                    stream=io.StringIO()),
            ):
                try:
                    invoke(module, port, rep)
                except StopBeforeEngine:
                    pass
                finally:
                    rep.close()
        finally:
            module.boot = saved_boot
            if restore_patch is not None:
                restore_patch()

        expect(len(launches) == 2,
               f"{key} reaches the same engine launch in standalone and protocol modes")
        if len(launches) == 2:
            expect(launches[0]["args"] == [],
                   f"{key} standalone run preserves the default RTS settings")
            expect(launches[0]["log"] == module.LOG,
                   f"{key} standalone run preserves its historical engine log")
            expect(launches[1]["args"] == ["+RTS", "-N3", "-RTS"],
                   f"{key} protocol run applies the harness RTS capability count")
            expect(launches[1]["log"] == os.path.join(tmp, module.LOG_NAME),
                   f"{key} protocol run isolates its engine log")


def probe_checks(probe_module_name: str):
    """`PROBE_CHECKS`'s identifiers, read when the contract runs.

    A zero-argument callable so a per-probe owner can name its own
    identifier source without importing the probe at module-import time.
    """
    def ids():
        return tuple(check_id for check_id, _ in
                     __import__(probe_module_name).PROBE_CHECKS)
    return ids


def synthetic_descriptor():
    """The three-check descriptor the synthetic probe declares.

    Shared because the event-stream owner builds streams against it and
    the reconciliation owner reconciles runs against the same shape.
    """
    return probe_protocol.build_descriptor(
        "synthetic", [("alpha", "a"), ("beta", "b"), ("gamma", "c")])


def event_line(**payload) -> str:
    """One `probe-result/v1` event, serialized the way a probe writes it."""
    return json.dumps(payload) + "\n"
