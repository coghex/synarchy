#!/usr/bin/env python3
"""Stable constants, authored fixture content, stage-aware check
recording, resource-root isolation, engine boot and the persistence
integrity oracle for `tools/unified_transfer_probe.py` (#2048).

The lowest layer of the probe: it imports no sibling module and owns the
things every other one reads — the eight `STAGES`, the item and building
YAML the run registers, the debug-console expressions naming the live
container-window instances, the `Checks` recorder every assertion goes
through, and the #1487 boundary-log oracle.

`boot_offscreen` lives here because engine boot is this module's
concern, but it is CALLED from the facade alone: one engine A and one
fresh engine B per run, and no stage may add a third.
"""
from __future__ import annotations

import os
import shutil
import sys

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
from probelib import boot

REPO = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))

SLOT = "probe_unified_transfer"

# --------------------------------------------------------------------------
# Persistence integrity oracle (#1487)
# --------------------------------------------------------------------------
# Both engines were already booted with their own log file and neither
# log was ever read, so a save or a load could report a dangling
# persisted reference and the run would still print PASS. It did: the
# approved assessment run passed 333 checks while these logs carried
# three dangling-reference warnings at save and the same three at load.
#
# This exact substring is what the engine writes for the TOLERATED,
# non-blocking half of the shared integrity graph (#764/#1246), from all
# four emitting sites — `World.Thread.Command.Save.WriteWorld` (the
# Lua-reference pass at :287 and the `sessionIntegrityWarnings` pass at
# :304), `Engine.Scripting.Lua.API.Save` (:892) and `World.Load.Stage`'s
# transfer-order pass (:286). The BLOCKING half aborts the save instead
# of logging, so a line carrying this needle is precisely the class that
# would otherwise pass unnoticed.
INTEGRITY_NEEDLE = "integrity diagnostic"

# Diagnostics a FIXTURE genuinely intends, as (substring, why) pairs.
# Empty, and meant to stay that way: a member must name ONE specific
# reference — the id or the field expected to dangle — and say why that
# fixture means it. `INTEGRITY_NEEDLE` itself, a message prefix, a save
# name or anything else matching a CLASS of diagnostics is not
# admissible, and neither is an engine defect: the dangling `buildTarget`
# references this oracle currently surfaces are #1484's to fix at their
# source, never to expect away here.
EXPECTED_INTEGRITY_DIAGNOSTICS: tuple[tuple[str, str], ...] = ()

assert all(INTEGRITY_NEEDLE not in substring
           for substring, _ in EXPECTED_INTEGRITY_DIAGNOSTICS), \
    ("an expectation must name one specific reference, never the whole "
     "diagnostic class")

STAGES = ["setup", "knowledge", "modeB", "modeA", "batch", "widget",
          "save", "load"]

# --------------------------------------------------------------------------
# Throwaway content
# --------------------------------------------------------------------------
# ONE def per LEG. A merged row's singular gesture names the row's
# representative, which is its FIRST member — so a source holding two
# instances of the same def would let "Store 1" move an instance this
# probe did not choose, and the leg would still pass. With one def per
# leg the source holds exactly one instance of it, the row is
# single-membered, and the entry the gesture creates can be compared
# against an EXACT instance id. (It also exercises #1249's rule that a
# single-instance row shows the singular entry alone.)
#
# 0.25 kg is deliberately a BINARY fraction, and the partial hold's
# 2.0 kg capacity is another: capacity is a 32-bit Float and the gate is
# `load + weight <= capacity` summed one accepted item at a time, so a
# decimal-friendly fixture lands a few ulps over and fits seven — an
# arithmetic artefact that reads exactly like a partial-batch bug. Eight
# of 0.25 sum to precisely 2.0.
#
# `bulk` is REQUIRED by `Engine.Asset.YamlItems` and one bad definition
# rejects the whole FILE; it is consumed by nothing here (the transfer
# capacity gate is weight-only).
LEG_DEFS_B = [f"probe_ut_b{i}" for i in range(1, 7)]
LEG_DEFS_A = [f"probe_ut_a{i}" for i in range(1, 7)]
DEF_BATCH = "probe_ut_batch"
DEF_STALE = "probe_ut_stale_item"
DEF_SAVE = "probe_ut_save_item"
ALL_ITEM_DEFS = LEG_DEFS_B + LEG_DEFS_A + [DEF_BATCH, DEF_STALE, DEF_SAVE]

ITEMS_YAML = "items:\n" + "".join(f"""\
  - name: "{d}"
    display_name: "Probe UT {d}"
    sprite: "assets/textures/items/material/bar_steel.png"
    weight: 0.25
    bulk: 0.25
    category: Materials
""" for d in ALL_ITEM_DEFS)

DEF_HOLD = "probe_ut_hold"
DEF_PARTIAL = "probe_ut_partial"
DEF_STALE_HOLD = "probe_ut_stale_hold"
DEF_UNSEEN = "probe_ut_unseen"

# `build_work: 0.0` takes the other branch of `Building.Types.currentActivity`
# (and with no `state_animations` block the computed appear duration is 0),
# so a `building.spawn`ed instance reports Built immediately — the shipped
# `cargo_hold_S`'s real 240 s of worker-driven work would leave one stuck
# "constructing" forever with no construct_job AI running.
#
# DEF_UNSEEN is the exception, and deliberately so: worker-built and left
# at zero progress, it never reaches Built, so A3's seed-at-completion
# trigger never fires and its knowledge record genuinely does not exist.
# That is the never-inspected state obtained without calling a single
# knowledge-mutating verb — manufacturing it with one would make this
# probe assert its own writes.
BUILDINGS_YAML = "".join([
    "buildings:\n",
    f"""\
  - name: "{DEF_HOLD}"
    display_name: "Probe UT Hold"
    category: "Test"
    description: "Throwaway #1255 test fixture - not shipped content."
    sprite: "assets/textures/buildings/cargo_hold_S/default.png"
    visual_class: "freestanding_installation"
    tile_size: {{ x: 1, y: 1 }}
    placement: "flat_ground"
    race: "acolyte_cult"
    build_work: 0.0
    storage_capacity: 400.0
  - name: "{DEF_PARTIAL}"
    display_name: "Probe UT Partial Hold"
    category: "Test"
    description: "Throwaway #1255 test fixture - not shipped content."
    sprite: "assets/textures/buildings/cargo_hold_S/default.png"
    visual_class: "freestanding_installation"
    tile_size: {{ x: 1, y: 1 }}
    placement: "flat_ground"
    race: "acolyte_cult"
    build_work: 0.0
    storage_capacity: 2.0
  - name: "{DEF_STALE_HOLD}"
    display_name: "Probe UT Stale Hold"
    category: "Test"
    description: "Throwaway #1255 test fixture - not shipped content."
    sprite: "assets/textures/buildings/cargo_hold_S/default.png"
    visual_class: "freestanding_installation"
    tile_size: {{ x: 1, y: 1 }}
    placement: "flat_ground"
    race: "acolyte_cult"
    build_work: 0.0
    storage_capacity: 200.0
  - name: "{DEF_UNSEEN}"
    display_name: "Probe UT Unseen Hold"
    category: "Test"
    description: "Throwaway #1255 test fixture - not shipped content."
    sprite: "assets/textures/buildings/cargo_hold_S/default.png"
    visual_class: "freestanding_installation"
    tile_size: {{ x: 1, y: 1 }}
    placement: "flat_ground"
    race: "acolyte_cult"
    build_work: 240.0
    storage_capacity: 300.0
"""])

CHUNK_TILES = 16
SEARCH_RADIUS = 60

# Debug-console expressions naming the live widget instances this probe
# reads. The container window owns a STACK of levels (#1238), so every
# read goes through the manager's own accessor because a level may not
# exist.
WINDOW = "require('scripts.cargo_inventory_panel')"
BASE_LEVEL = f"({WINDOW}.getLevel(1) or {{src={{}}}})"
CARGO_LIST_ID = f"{BASE_LEVEL}.listId"
UNIT_INV_LIST_ID = "require('scripts.unit_info_v2').invListId"


def pane_list_id(pane_key: str) -> str:
    """A debug-console expression naming one escort pane's list instance,
    addressed through the manager's own pane accessor rather than by
    indexing `panes` positionally."""
    return ("(function() local c = require('scripts.cargo_inventory_panel');"
            f" local p = c.getPane(c.getLevel(1), '{pane_key}');"
            " return p and p.listId end)()")


class SetupError(RuntimeError):
    """The scenario could not reach the state it tests."""


# --------------------------------------------------------------------------
# Stage-aware check recorder
# --------------------------------------------------------------------------
class Checks:
    """Every check is attributed to one of the eight STAGES, so a failure
    says WHICH part of the system broke instead of only that it did."""

    def __init__(self) -> None:
        self.failed = 0
        self.stage = STAGES[0]
        self.by_stage: dict[str, list[int]] = {s: [0, 0] for s in STAGES}
        self.reached: list[str] = []

    def enter(self, stage: str, title: str) -> None:
        assert stage in STAGES, stage
        self.stage = stage
        if stage not in self.reached:
            self.reached.append(stage)
        print(f"\n=== [{stage}] {title} ===", flush=True)

    def ok(self, cond: bool, label: str, detail: str = "") -> bool:
        cond = bool(cond)
        # Recording against a stage counts as reaching it, so an
        # operational failure raised before its own enter() still reports
        # as FAIL rather than as NOT REACHED.
        if self.stage not in self.reached:
            self.reached.append(self.stage)
        slot = self.by_stage[self.stage]
        slot[0 if cond else 1] += 1
        print(f"  [{'PASS' if cond else 'FAIL'}][{self.stage}] {label}"
              + (f"  ({detail})" if detail and not cond else ""), flush=True)
        if not cond:
            self.failed += 1
        return cond

    def ok_at(self, stage: str, cond: bool, label: str,
              detail: str = "") -> bool:
        """Record a check against a stage other than the current one.

        The boundary-log oracle (#1487) runs on the TEARDOWN path, where
        the current stage is whichever one raised, and must be attributed
        to the boundary it inspects so the eight-stage report names it.

        A PASS deliberately does not mark an unreached stage as reached:
        a session that never got as far as `save` saved nothing, so a
        clean log certifies nothing about that stage and it must keep
        reporting NOT REACHED. A FAIL always surfaces there.
        """
        assert stage in STAGES, stage
        previous, was_reached = self.stage, stage in self.reached
        self.stage = stage
        try:
            return self.ok(cond, label, detail)
        finally:
            if bool(cond) and not was_reached and stage in self.reached:
                self.reached.remove(stage)
            self.stage = previous

    def outcomes(self) -> dict[str, str]:
        """Per-stage pass/fail, for the run fingerprint. Deliberately
        outcomes only and no measurements: two runs of the same seed must
        agree on WHAT happened, while a sampled weight or a wall time is
        a measurement and will differ."""
        out = {}
        for stage in STAGES:
            passed, failed = self.by_stage[stage]
            out[stage] = ("not-reached" if stage not in self.reached
                          else ("fail" if failed else "pass"))
        return out

    def report(self) -> None:
        print("\n--- stage summary ---", flush=True)
        broken, failing = [], []
        for stage in STAGES:
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
            print(f"\n--- FAIL: the unified transfer system broke at stage "
                  f"'{culprit}' (stages affected: {', '.join(broken)}) ---",
                  flush=True)
        else:
            print("\n--- PASS: both modes, all three endpoint classes, both "
                  "directions, batches, knowledge, one widget and "
                  "persistence hold together ---", flush=True)


# --------------------------------------------------------------------------
# Boot / isolation
# --------------------------------------------------------------------------
def make_isolated_root(base: str) -> str:
    """A throwaway resource root: the read-only content families
    symlinked, `config/` COPIED (the real UI flow writes settings and
    must never touch the developer's own), and its OWN empty saves/.

    The copy deliberately EXCLUDES `*.local.yaml`, so the run starts from
    the tracked defaults a fresh checkout has rather than from whatever
    this developer's settings happen to be. That is not tidiness: the
    partial-batch stage asserts on the `unit_warning` text in the EVENT
    LOG, and a category reaches the log only when its notifications YAML
    says `log: true` — so a personal override could otherwise decide
    whether this gate passes. Excluding them also makes the run
    reproducible between two checkouts of the same commit, which is what
    the FINGERPRINT comparison is worth anything for.
    """
    root = os.path.join(base, "root")
    os.makedirs(root, exist_ok=True)
    for family in ("scripts", "assets", "data"):
        target = os.path.join(root, family)
        if not os.path.exists(target):
            os.symlink(os.path.join(REPO, family), target)
    config_dst = os.path.join(root, "config")
    if not os.path.exists(config_dst):
        shutil.copytree(os.path.join(REPO, "config"), config_dst,
                        ignore=shutil.ignore_patterns("*.local.yaml"))
    os.makedirs(os.path.join(root, "saves"), exist_ok=True)
    return root


def boot_offscreen(root: str, port: int, size: str, log: str, label: str):
    return boot(port, log=log, label=label, ready_timeout=240,
                mode=("--offscreen",),
                args=["--size", size, "--resource-root", root])


def integrity_diagnostics(path: str) -> tuple[list[str], str | None]:
    """Every unexpected `INTEGRITY_NEEDLE` line in one engine log.

    Returns `(lines, error)`. A missing or unreadable log is an ERROR,
    never an empty match list: a boundary whose log cannot be read has
    not been SHOWN to be clean, and collapsing that to "no diagnostics"
    is exactly what would let this oracle pass vacuously. (The adjacent
    `tools/transfer_order_probe.py:log_lines` — the model for the needle
    and the reporting — does collapse it, which is the one thing not
    copied from it.)
    """
    try:
        with open(path, "r", encoding="utf-8", errors="replace") as handle:
            lines = [ln.rstrip("\n") for ln in handle
                     if INTEGRITY_NEEDLE in ln]
    except OSError as exc:
        return [], f"{type(exc).__name__}: {exc}"
    return ([ln for ln in lines
             if not any(substring in ln
                        for substring, _ in EXPECTED_INTEGRITY_DIAGNOSTICS)],
            None)


def check_boundary_log(chk: Checks, stage: str, label: str, path: str,
                       proc) -> None:
    """Fail the run on any unexpected persistence integrity diagnostic
    one engine logged (#1487).

    Called from the SAME `finally` that quits that engine, so a stage
    which raised or returned early still gets its boundary inspected;
    the checks are attributed to `stage` rather than to whatever stage
    was current when it broke, because what they name is the BOUNDARY
    that produced the diagnostic.

    `quit_engine` is the flush barrier: it waits for the process to
    terminate (killing and waiting if it must), and the engine's stdout
    IS this file, so once the process is gone everything it wrote has
    landed. That is re-confirmed rather than assumed — a log still being
    written to has not been shown to be complete, and a diagnostic
    emitted late in the boundary is the case this whole check exists
    for.

    Records checks and never raises: this runs on a teardown path, where
    an exception would replace the failure that got us here.
    """
    try:
        alive = proc is not None and proc.poll() is None
        chk.ok_at(stage, not alive,
                  f"{label}: the engine process is gone, so everything it "
                  f"wrote is in its log before the log is read",
                  f"pid {proc.pid if proc else '?'} was still running after "
                  f"quit_engine returned, so {path} may be truncated")
        lines, error = integrity_diagnostics(path)
        if error is not None:
            # Not "no diagnostics": a boundary whose log cannot be read
            # has not been shown to be clean.
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
