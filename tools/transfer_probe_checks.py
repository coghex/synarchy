#!/usr/bin/env python3
"""Stage-aware check recording and the persistence-integrity log oracle for
the transfer probes (`transfer_context_menu_probe.py`,
`transfer_order_probe.py`) - the part of the retired end-to-end
transfer probe's harness (#1255, #2048, #2802, #1487) that outlived it
(`docs/archive/transfer_end_to_end_probe_coverage_map.md`).

Two things, both library code (no CLI, no engine boot):

  * `Checks` - every check is attributed to one named STAGE, so a failure
    says which part of the system broke rather than only that something
    did, and every FAILED check is also a durable failure record
    (`probe_runner_diagnostics.FailureEmitter`, #2802) whose detail leads
    with the stage (`[stage] label (detail)`), because the record wire
    format has no stage field. A check recorded against the `setup` stage
    uses the `setup` record kind.
  * `integrity_diagnostics` / `check_boundary_log` - the #1487 oracle: an
    engine's log, read once its process is gone, must carry no
    `integrity diagnostic` line. The log being UNREADABLE is a failure,
    never "no diagnostics": a boundary whose log cannot be read has not
    been shown to be clean.

`failure` is a parameter so the engine-free tests can drive the real
emitter; omitting it constructs the named producer's own.
"""
from __future__ import annotations

import os
import sys

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
from probe_runner_diagnostics import FailureEmitter

# --------------------------------------------------------------------------
# Persistence integrity oracle (#1487)
# --------------------------------------------------------------------------
# This exact substring is what the engine writes for the TOLERATED,
# non-blocking half of the shared integrity graph (#764/#1246), from all
# four emitting sites - `World.Thread.Command.Save.WriteWorld` (the
# Lua-reference pass and the `sessionIntegrityWarnings` pass),
# `Engine.Scripting.Lua.API.Save` and `World.Load.Stage`'s transfer-order
# pass. The BLOCKING half aborts the save instead of logging, so a line
# carrying this needle is precisely the class that would otherwise pass
# unnoticed.
INTEGRITY_NEEDLE = "integrity diagnostic"

# Diagnostics a FIXTURE genuinely intends, as (substring, why) pairs.
# Empty, and meant to stay that way: a member must name ONE specific
# reference - the id or the field expected to dangle - and say why that
# fixture means it. `INTEGRITY_NEEDLE` itself, a message prefix, a save
# name or anything else matching a CLASS of diagnostics is not
# admissible, and neither is an engine defect.
EXPECTED_INTEGRITY_DIAGNOSTICS: tuple[tuple[str, str], ...] = ()

assert all(INTEGRITY_NEEDLE not in substring
           for substring, _ in EXPECTED_INTEGRITY_DIAGNOSTICS), \
    ("an expectation must name one specific reference, never the whole "
     "diagnostic class")


class Checks:
    """The stage-aware recorder. `stages` is the ordered stage vocabulary;
    the first one is current until `enter` says otherwise."""

    def __init__(self, producer: str, stages: list[str],
                 failure: FailureEmitter | None = None) -> None:
        self.failure = FailureEmitter(producer) if failure is None else failure
        self.stages = list(stages)
        self.failed = 0
        self.stage = self.stages[0]
        self.by_stage: dict[str, list[int]] = {s: [0, 0] for s in self.stages}
        self.reached: list[str] = []

    def enter(self, stage: str, title: str = "") -> None:
        assert stage in self.stages, stage
        self.stage = stage
        if stage not in self.reached:
            self.reached.append(stage)
        print(f"\n=== [{stage}] {title} ===" if title
              else f"\n=== [{stage}] ===", flush=True)

    def ok(self, cond: bool, label: str, detail: str = "",
           setup: bool = False) -> bool:
        cond = bool(cond)
        # Recording against a stage counts as reaching it, so an
        # operational failure raised before its own enter() still reports
        # as FAIL rather than as NOT REACHED.
        if self.stage not in self.reached:
            self.reached.append(self.stage)
        self.by_stage[self.stage][0 if cond else 1] += 1
        print(f"  [{'PASS' if cond else 'FAIL'}][{self.stage}] {label}"
              + (f"  ({detail})" if detail and not cond else ""), flush=True)
        if not cond:
            self.failed += 1
            # `self.stage` is already the attributed stage here: `ok_at`
            # sets it to its target before delegating, so a teardown-path
            # boundary-log failure is retained under its own boundary.
            record = (self.failure.setup if setup or self.stage == "setup"
                      else self.failure.check)
            record(f"[{self.stage}] {label}"
                   + (f" ({detail})" if detail else ""))
        return cond

    def ok_at(self, stage: str, cond: bool, label: str,
              detail: str = "") -> bool:
        """Record a check against a stage other than the current one.

        The boundary-log oracle runs on the TEARDOWN path, where the
        current stage is whichever one raised, and must be attributed to
        the boundary it inspects. A PASS deliberately does not mark an
        unreached stage as reached: a session that never got as far as
        `save` saved nothing, so a clean log certifies nothing about that
        stage and it must keep reporting NOT REACHED. A FAIL always
        surfaces there."""
        assert stage in self.stages, stage
        previous, was_reached = self.stage, stage in self.reached
        self.stage = stage
        try:
            return self.ok(cond, label, detail)
        finally:
            if bool(cond) and not was_reached and stage in self.reached:
                self.reached.remove(stage)
            self.stage = previous

    def outcomes(self) -> dict[str, str]:
        """Per-stage pass/fail outcomes only, never a measurement."""
        out = {}
        for stage in self.stages:
            _passed, failed = self.by_stage[stage]
            out[stage] = ("not-reached" if stage not in self.reached
                          else ("fail" if failed else "pass"))
        return out

    def report(self, subject: str = "the transfer system") -> None:
        print("\n--- stage summary ---", flush=True)
        broken, failing = [], []
        for stage in self.stages:
            passed, failed = self.by_stage[stage]
            if failed:
                status = f"FAIL ({failed} of {passed + failed} checks)"
                broken.append(stage)
                failing.append(stage)
            elif stage not in self.reached:
                status = "NOT REACHED"
                broken.append(stage)
            else:
                status = f"pass ({passed} checks)"
            print(f"  {stage:<9} {status}", flush=True)
        if broken:
            culprit = failing[0] if failing else broken[0]
            print(f"\n--- FAIL: {subject} broke at stage '{culprit}' "
                  f"(stages affected: {', '.join(broken)}) ---", flush=True)
        else:
            print(f"\n--- PASS: {subject} holds at every stage ---",
                  flush=True)


def integrity_diagnostics(path: str,
                          needle: str = INTEGRITY_NEEDLE
                          ) -> tuple[list[str], str | None]:
    """Every unexpected `needle` line in one engine log.

    Returns `(lines, error)`. A missing or unreadable log is an ERROR,
    never an empty match list: a boundary whose log cannot be read has
    not been SHOWN to be clean, and collapsing that to "no diagnostics"
    is exactly what would let this oracle pass vacuously."""
    try:
        with open(path, "r", encoding="utf-8", errors="replace") as handle:
            lines = [ln.rstrip("\n") for ln in handle if needle in ln]
    except OSError as exc:
        return [], f"{type(exc).__name__}: {exc}"
    return ([ln for ln in lines
             if not any(substring in ln
                        for substring, _ in EXPECTED_INTEGRITY_DIAGNOSTICS)],
            None)


def check_boundary_log(chk: Checks, stage: str, label: str, path: str,
                       proc, needle: str = INTEGRITY_NEEDLE) -> None:
    """Fail the run on any unexpected persistence integrity diagnostic one
    engine logged (#1487).

    Called from the SAME `finally` that quits that engine, so a stage which
    raised or returned early still gets its boundary inspected; the checks
    are attributed to `stage` rather than to whatever stage was current
    when it broke, because what they name is the BOUNDARY that produced the
    diagnostic.

    `quit_engine` is the flush barrier: it waits for the process to
    terminate (killing and waiting if it must), and the engine's stdout IS
    this file, so once the process is gone everything it wrote has landed.
    That is re-confirmed rather than assumed - a log still being written to
    has not been shown to be complete.

    Records checks and never raises: this runs on a teardown path, where an
    exception would replace the failure that got us here."""
    try:
        alive = proc is not None and proc.poll() is None
        chk.ok_at(stage, not alive,
                  f"{label}: the engine process is gone, so everything it "
                  f"wrote is in its log before the log is read",
                  f"pid {proc.pid if proc else '?'} was still running after "
                  f"quit_engine returned, so {path} may be truncated")
        lines, error = integrity_diagnostics(path, needle)
        if error is not None:
            chk.ok_at(stage, False,
                      f"{label}: the boundary log is readable, so the "
                      f"boundary can be shown to be clean",
                      f"{path}: {error}")
            return
        chk.ok_at(stage, not lines,
                  f"{label}: no unexpected persistence integrity diagnostic "
                  f"at this boundary",
                  f"{len(lines)} in {path}: {lines[:3]!r}")
    except Exception as exc:  # noqa: BLE001
        chk.ok_at(stage, False,
                  f"{label}: the boundary log could be inspected for "
                  f"persistence integrity diagnostics",
                  f"unexpected {type(exc).__name__}: {exc}")
