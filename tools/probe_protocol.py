#!/usr/bin/env python3
"""The shared `probe-result/v1` probe protocol (#1425).

The ~85 headless behavior probes each print their own free-form
pass/fail prose, so nothing outside a human can tell which checks a run
was even supposed to make, let alone which ones it made. A flakiness
harness (`tools/probe_flake.py`) needs both, so this module defines the
machine-readable contract a probe opts into and owns both halves of it:

* a **descriptor** — a no-engine, ordered declaration of every check the
  probe intends to make, each with a stable identifier and a
  human-readable label. Printed by `python3 tools/<name>_probe.py
  --describe`, which must never boot anything.
* an **event stream** — one flushed JSON object per line, written into
  the per-run artifact directory the harness supplies. Check events
  carry a declared stable identifier and PASS/FAIL; diagnostics carry an
  explicit INFO/WARN/SKIP level and never affect a check's outcome.

Identity is STATIC and detail is DYNAMIC: a stable identifier may never
carry a runtime value (`role_miner_60`, `unit_4711`), because two runs
of the same check must be the same check. Everything that varies goes in
an event's optional `detail` object.

A probe stays fully usable on its own. `reporter_from_env` returns a
reporter in STANDALONE mode when the harness supplied no event path, and
that mode prints exactly the bracketed `[PASS]`/`[FAIL]` lines probes
have always printed, so `python3 tools/role_probe.py --port N` and
`run_probes.py --only role` behave as before. The prohibition on
bracketed stdout markers applies only in PROTOCOL mode, where a second
result channel could disagree with the event stream.

The harness supplies its per-run wiring through the environment, so a
migrated probe needs no new command-line flags beyond `--describe`:

    SYNARCHY_PROBE_EVENTS           protocol event stream path (its
                                    presence is what selects protocol mode)
    SYNARCHY_PROBE_ARTIFACT_DIR     the run's artifact directory
    SYNARCHY_PROBE_ENGINE_LOG_DIR   where every engine log for this run goes
    SYNARCHY_PROBE_RTS_CAPS         RTS capability count for every engine

Import it from a probe (probes run as `python3 tools/<name>_probe.py`,
so `tools/` is on `sys.path`)::

    import probe_protocol
    DESCRIPTOR = probe_protocol.build_descriptor("role", CHECKS)
"""
from __future__ import annotations

import json
import os
import re
import sys
from dataclasses import dataclass, field

PROTOCOL_VERSION = "probe-result/v1"

# --- The harness -> probe environment contract -----------------------------
ENV_EVENTS = "SYNARCHY_PROBE_EVENTS"
ENV_ARTIFACT_DIR = "SYNARCHY_PROBE_ARTIFACT_DIR"
ENV_ENGINE_LOG_DIR = "SYNARCHY_PROBE_ENGINE_LOG_DIR"
ENV_RTS_CAPS = "SYNARCHY_PROBE_RTS_CAPS"

# Every variable in that contract, so a launcher can strip the lot from
# an inherited environment before supplying its own.
PROTOCOL_ENV_VARS = (ENV_EVENTS, ENV_ARTIFACT_DIR, ENV_ENGINE_LOG_DIR,
                     ENV_RTS_CAPS)

# A stable check identifier: lowercase, word-like, and deliberately
# unable to spell a runtime value that a caller might interpolate (no
# digits-only components, no punctuation beyond `_`). Dynamic values
# belong in an event's `detail`, never in its identity.
CHECK_ID_RE = re.compile(r"^[a-z][a-z0-9_]*$")

PASS = "PASS"
FAIL = "FAIL"
MISSING = "MISSING"
CHECK_OUTCOMES = (PASS, FAIL)
CHECK_RESULTS = (PASS, FAIL, MISSING)
DIAGNOSTIC_LEVELS = ("INFO", "WARN", "SKIP")

EVENT_CHECK = "check"
EVENT_DIAGNOSTIC = "diagnostic"

# A stdout line whose FIRST non-whitespace token is a bracketed word,
# immediately followed by whitespace or the end of the line. That is
# exactly the shape of the legacy result markers (`[PASS]`, `[pass]`,
# `[FAIL]`, `[INFO]`, `[WARN]`, `[SKIP]`, `[diag]`), which must never
# become a second result channel beside the event stream. It is
# deliberately NOT "any line containing brackets": ordinary bracketed
# DATA — a Python list repr `[1, 2, 3]`, a JSON array, `{"a": [1]}` — is
# not a marker and must not turn a legitimate measurement into a
# harness error.
_MARKER_RE = re.compile(r"^\s*\[[A-Za-z_][A-Za-z0-9_]*\](\s|$)")


class ProtocolError(Exception):
    """A descriptor or event stream that violates `probe-result/v1`."""


# --------------------------------------------------------------------------
# Descriptor
# --------------------------------------------------------------------------
@dataclass(frozen=True)
class Descriptor:
    """One probe's ordered, stable declaration of the checks it makes."""

    probe: str
    checks: tuple[tuple[str, str], ...]

    @property
    def ids(self) -> tuple[str, ...]:
        return tuple(cid for cid, _ in self.checks)

    def label(self, check_id: str) -> str:
        for cid, text in self.checks:
            if cid == check_id:
                return text
        raise KeyError(check_id)

    def index(self, check_id: str) -> int:
        return self.ids.index(check_id)

    def to_document(self) -> dict:
        return {
            "protocol": PROTOCOL_VERSION,
            "probe": self.probe,
            "checks": [{"id": cid, "label": text} for cid, text in self.checks],
        }

    def to_json(self) -> str:
        return json.dumps(self.to_document(), indent=2, sort_keys=True)


def build_descriptor(probe: str, checks) -> Descriptor:
    """Validate `(id, label)` pairs and freeze them into a Descriptor."""
    if not isinstance(probe, str) or not probe.strip():
        raise ProtocolError("descriptor: `probe` must be a non-empty string")
    pairs: list[tuple[str, str]] = []
    seen: set[str] = set()
    for entry in checks:
        try:
            cid, label = entry
        except (TypeError, ValueError):
            raise ProtocolError(
                f"descriptor for {probe!r}: each check must be an "
                f"(id, label) pair, got {entry!r}") from None
        if not isinstance(cid, str) or not CHECK_ID_RE.match(cid):
            raise ProtocolError(
                f"descriptor for {probe!r}: {cid!r} is not a stable check "
                f"identifier (expected {CHECK_ID_RE.pattern}); runtime "
                f"values belong in event detail, not in identity")
        if not isinstance(label, str) or not label.strip():
            raise ProtocolError(
                f"descriptor for {probe!r}: check {cid!r} has no label")
        if cid in seen:
            raise ProtocolError(
                f"descriptor for {probe!r}: duplicate check identifier {cid!r}")
        seen.add(cid)
        pairs.append((cid, label))
    if not pairs:
        raise ProtocolError(
            f"descriptor for {probe!r}: declares no checks")
    return Descriptor(probe=probe, checks=tuple(pairs))


def parse_descriptor(text: str, expected_probe: str | None = None) -> Descriptor:
    """Parse and fully validate a `--describe` document.

    `expected_probe` binds the descriptor to the probe key the caller
    asked for: a descriptor naming a different probe is a protocol
    error, not a silently accepted substitution.
    """
    try:
        document = json.loads(text)
    except (TypeError, ValueError) as error:
        raise ProtocolError(f"descriptor is not valid JSON: {error}") from None
    if not isinstance(document, dict):
        raise ProtocolError("descriptor must be a JSON object")
    version = document.get("protocol")
    if version != PROTOCOL_VERSION:
        raise ProtocolError(
            f"descriptor declares protocol {version!r}; this harness "
            f"supports only {PROTOCOL_VERSION!r}")
    probe = document.get("probe")
    if not isinstance(probe, str) or not probe.strip():
        raise ProtocolError("descriptor: `probe` must be a non-empty string")
    if expected_probe is not None and probe != expected_probe:
        raise ProtocolError(
            f"descriptor names probe {probe!r} but {expected_probe!r} was "
            f"requested")
    raw_checks = document.get("checks")
    if not isinstance(raw_checks, list):
        raise ProtocolError("descriptor: `checks` must be a list")
    pairs = []
    for entry in raw_checks:
        if not isinstance(entry, dict) or "id" not in entry or "label" not in entry:
            raise ProtocolError(
                f"descriptor for {probe!r}: each check must be an object with "
                f"`id` and `label`, got {entry!r}")
        pairs.append((entry["id"], entry["label"]))
    return build_descriptor(probe, pairs)


# --------------------------------------------------------------------------
# Event stream
# --------------------------------------------------------------------------
@dataclass(frozen=True)
class CheckEvent:
    id: str
    outcome: str
    detail: dict = field(default_factory=dict)


@dataclass(frozen=True)
class DiagnosticEvent:
    level: str
    message: str
    detail: dict = field(default_factory=dict)


class EventWriter:
    """Writes one flushed JSON object per line to the protocol stream.

    Flushed per event on purpose: the harness reads what a killed or
    timed-out probe managed to emit, so a buffered tail would silently
    become MISSING checks.
    """

    def __init__(self, path: str):
        self._path = path
        self._fh = open(path, "w", encoding="utf-8")

    def _emit(self, payload: dict) -> None:
        self._fh.write(json.dumps(payload, sort_keys=True) + "\n")
        self._fh.flush()

    def check(self, check_id: str, passed: bool, detail: dict | None = None) -> None:
        payload = {"event": EVENT_CHECK, "id": check_id,
                   "outcome": PASS if passed else FAIL}
        if detail:
            payload["detail"] = detail
        self._emit(payload)

    def diagnostic(self, level: str, message: str,
                   detail: dict | None = None) -> None:
        if level not in DIAGNOSTIC_LEVELS:
            raise ProtocolError(
                f"diagnostic level {level!r} is not one of {DIAGNOSTIC_LEVELS}")
        payload = {"event": EVENT_DIAGNOSTIC, "level": level, "message": message}
        if detail:
            payload["detail"] = detail
        self._emit(payload)

    def close(self) -> None:
        try:
            self._fh.close()
        except OSError:
            pass


def parse_event_stream(text: str, descriptor: Descriptor):
    """Parse a run's event stream against `descriptor`.

    Returns `(events, outcomes)` where `outcomes` maps every declared
    check identifier to PASS, FAIL, or MISSING — MISSING being every
    declared check that emitted no check event, whether the probe
    aborted early, timed out, or simply stopped.

    Every deviation is a `ProtocolError`, never a probe outcome: a
    malformed or truncated line, an unknown event kind, an unknown or
    duplicate check identifier, a check arriving out of the declared
    order, an unrecognised outcome, or an unrecognised diagnostic level.
    A check event arriving while an earlier declared check has not been
    emitted means the probe deviated from the sequence it declared,
    which a reliability harness must surface rather than reconcile away.
    """
    events: list[CheckEvent | DiagnosticEvent] = []
    outcomes: dict[str, str] = {cid: MISSING for cid in descriptor.ids}
    if text and not text.endswith("\n"):
        raise ProtocolError(
            "protocol event stream ends mid-line (truncated): "
            f"{text.splitlines()[-1][:120]!r}")
    next_index = 0
    for number, line in enumerate(text.splitlines(), start=1):
        if not line.strip():
            continue
        try:
            payload = json.loads(line)
        except (TypeError, ValueError) as error:
            raise ProtocolError(
                f"protocol event line {number} is malformed JSON "
                f"({error}): {line[:120]!r}") from None
        if not isinstance(payload, dict):
            raise ProtocolError(
                f"protocol event line {number} is not a JSON object: "
                f"{line[:120]!r}")
        kind = payload.get("event")
        if kind == EVENT_CHECK:
            check_id = payload.get("id")
            outcome = payload.get("outcome")
            if check_id not in outcomes:
                raise ProtocolError(
                    f"protocol event line {number}: check identifier "
                    f"{check_id!r} is not declared by probe "
                    f"{descriptor.probe!r}")
            if outcome not in CHECK_OUTCOMES:
                raise ProtocolError(
                    f"protocol event line {number}: check {check_id!r} "
                    f"reported outcome {outcome!r}, expected one of "
                    f"{CHECK_OUTCOMES}")
            index = descriptor.index(check_id)
            if index < next_index:
                raise ProtocolError(
                    f"protocol event line {number}: check {check_id!r} was "
                    f"already reported (duplicate check event)")
            if index > next_index:
                missed = descriptor.ids[next_index]
                raise ProtocolError(
                    f"protocol event line {number}: check {check_id!r} "
                    f"arrived before the declared check {missed!r}; the "
                    f"probe deviated from the sequence it declared")
            detail = payload.get("detail") or {}
            if not isinstance(detail, dict):
                raise ProtocolError(
                    f"protocol event line {number}: `detail` must be an "
                    f"object, got {detail!r}")
            outcomes[check_id] = outcome
            next_index = index + 1
            events.append(CheckEvent(check_id, outcome, detail))
        elif kind == EVENT_DIAGNOSTIC:
            level = payload.get("level")
            message = payload.get("message")
            if level not in DIAGNOSTIC_LEVELS:
                raise ProtocolError(
                    f"protocol event line {number}: diagnostic level "
                    f"{level!r} is not one of {DIAGNOSTIC_LEVELS}")
            if not isinstance(message, str):
                raise ProtocolError(
                    f"protocol event line {number}: diagnostic has no "
                    f"string `message`")
            detail = payload.get("detail") or {}
            if not isinstance(detail, dict):
                raise ProtocolError(
                    f"protocol event line {number}: `detail` must be an "
                    f"object, got {detail!r}")
            events.append(DiagnosticEvent(level, message, detail))
        else:
            raise ProtocolError(
                f"protocol event line {number}: unclassifiable event kind "
                f"{kind!r}")
    return events, outcomes


def forbidden_marker_lines(stdout_text: str) -> list[str]:
    """Stdout lines that would be a second result channel (see `_MARKER_RE`)."""
    return [line for line in (stdout_text or "").splitlines()
            if _MARKER_RE.match(line)]


# --------------------------------------------------------------------------
# Probe-side reporter
# --------------------------------------------------------------------------
class Reporter:
    """A probe's one output funnel, in either standalone or protocol mode.

    Standalone mode prints exactly the bracketed human lines probes have
    always printed. Protocol mode writes structured events instead and
    prints nothing bracketed, so the event stream is the sole result
    channel.
    """

    def __init__(self, descriptor: Descriptor, events_path: str | None = None,
                 engine_log_dir: str | None = None, rts_caps: int | None = None,
                 artifact_dir: str | None = None, stream=None):
        self.descriptor = descriptor
        self.engine_log_dir = engine_log_dir
        self.rts_caps = rts_caps
        self.artifact_dir = artifact_dir
        self._stream = stream if stream is not None else sys.stdout
        self._writer = EventWriter(events_path) if events_path else None

    @property
    def protocol_mode(self) -> bool:
        return self._writer is not None

    def engine_args(self) -> list[str]:
        """The RTS block every engine this probe boots must receive.

        A command-line `+RTS ... -RTS` block is consumed by the GHC RTS
        before `getArgs` ever sees it, so it is safe to append after the
        engine's own flags. It overrides only `-N`; the executable's
        baked `-A128M` still applies.
        """
        if self.rts_caps is None:
            return []
        return ["+RTS", f"-N{self.rts_caps}", "-RTS"]

    def engine_log_path(self, name: str, default: str) -> str:
        """Where an engine log goes: the harness's run directory, or `default`.

        This is what stops a harnessed run from overwriting a probe's
        shared `/tmp` log — every run of every concurrent harness would
        otherwise fight over the same file.
        """
        if self.engine_log_dir:
            return os.path.join(self.engine_log_dir, name)
        return default

    def check(self, check_id: str, passed: bool, human: str,
              detail: dict | None = None) -> bool:
        """Report one declared check; returns `passed` for `passed &= ...`."""
        if check_id not in self.descriptor.ids:
            raise ProtocolError(
                f"probe {self.descriptor.probe!r} reported undeclared check "
                f"{check_id!r}")
        if self._writer is not None:
            self._writer.check(check_id, passed, detail)
        else:
            print(f"  [{PASS if passed else FAIL}] {human}", file=self._stream)
        return passed

    def _diagnostic(self, level: str, human: str, detail: dict | None) -> None:
        if self._writer is not None:
            self._writer.diagnostic(level, human, detail)
        else:
            print(f"  [{level}] {human}", file=self._stream)

    def info(self, human: str, detail: dict | None = None) -> None:
        self._diagnostic("INFO", human, detail)

    def warn(self, human: str, detail: dict | None = None) -> None:
        self._diagnostic("WARN", human, detail)

    def skip(self, human: str, detail: dict | None = None) -> None:
        self._diagnostic("SKIP", human, detail)

    def abort(self, human: str, detail: dict | None = None) -> None:
        """A setup failure that stops the run before its remaining checks.

        Not a check event: the checks it prevented stay MISSING, which
        is exactly what the harness must see. Standalone mode keeps the
        `[FAIL]` line probes have always printed for these.
        """
        if self._writer is not None:
            self._writer.diagnostic("WARN", human, detail)
        else:
            print(f"  [{FAIL}] {human}", file=self._stream)

    def note(self, human: str) -> None:
        """A plain, unbracketed human line; suppressed in protocol mode."""
        if self._writer is None:
            print(human, file=self._stream)

    def close(self) -> None:
        if self._writer is not None:
            self._writer.close()


def reporter_from_env(descriptor: Descriptor, env=None) -> Reporter:
    """Build the reporter this invocation's environment asks for.

    No `SYNARCHY_PROBE_EVENTS` means a human ran the probe directly, so
    the reporter stays in standalone mode and the probe behaves exactly
    as it did before it was migrated.
    """
    environ = os.environ if env is None else env
    caps_raw = environ.get(ENV_RTS_CAPS)
    caps = None
    if caps_raw:
        try:
            caps = int(caps_raw)
        except ValueError:
            raise ProtocolError(
                f"{ENV_RTS_CAPS}={caps_raw!r} is not an integer") from None
        if caps < 1:
            raise ProtocolError(
                f"{ENV_RTS_CAPS}={caps_raw!r} must be a positive capability count")
    return Reporter(
        descriptor,
        events_path=environ.get(ENV_EVENTS) or None,
        engine_log_dir=environ.get(ENV_ENGINE_LOG_DIR) or None,
        rts_caps=caps,
        artifact_dir=environ.get(ENV_ARTIFACT_DIR) or None,
    )
