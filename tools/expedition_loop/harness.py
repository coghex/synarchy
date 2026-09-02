#!/usr/bin/env python3
"""The run's substrate: check accounting, isolation, boot, cleanup, and
the record the stage owners hand each other (#2092).

Everything here is about running the scenario rather than about the
expedition: which stage a check belongs to, how a failure that
interrupted a stage is attributed to it, the throwaway resource root,
the bootstrap both engines share, and the two shared stage-aware check
helpers (`check_ai_tick_clean`, `assert_real_travel`) that more than one
stage owner calls.

`ExpeditionState` is requirement 5's single source: the selected site
and ruin, the party identities, the recovered item's identity, the
sampled control observations and the save handoff each live on it
exactly once, and the facade hands the same record to every owner in
turn. `Fingerprint` is the one accumulator — stage owners contribute
keys through it and nobody else serializes it.

Nothing here boots the SEQUENCE: `boot_probe` opens one engine when the
facade tells it to, and only the facade ever calls it.
"""
from __future__ import annotations

import glob
import json
import os
import shutil
from dataclasses import dataclass, field

from probelib import boot, send

from .constants import MAX_STEP_TILES, REPO, STAGES
from .readers import dist


# --------------------------------------------------------------------------
# Stage-aware check recorder
# --------------------------------------------------------------------------
class Checks:
    """Every check is attributed to one of the eight STAGES, so a failure
    says WHICH part of the loop broke instead of only that it did."""

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

    def ok(self, cond: bool, label: str) -> bool:
        cond = bool(cond)
        # Recording against a stage counts as reaching it, so a failure
        # attributed here reports as FAIL rather than NOT REACHED — which
        # matters for an operational failure raised before its own
        # enter().
        if self.stage not in self.reached:
            self.reached.append(self.stage)
        slot = self.by_stage[self.stage]
        slot[0 if cond else 1] += 1
        print(f"  [{'PASS' if cond else 'FAIL'}][{self.stage}] {label}", flush=True)
        if not cond:
            self.failed += 1
        return cond

    def fail_setup(self, label: str) -> None:
        """A stage could not even be reached — reported the same way a
        failing check is, so the summary still names a stage."""
        self.ok(False, label)

    def outcomes(self) -> dict[str, str]:
        """Per-stage pass/fail, for the run fingerprint. Deliberately
        outcomes only and no measurements: two runs of the same seed must
        agree on WHAT happened, while a sampled physiological delta is a
        measurement and will differ in its last digits."""
        out = {}
        for stage in STAGES:
            passed, failed = self.by_stage[stage]
            if stage not in self.reached:
                out[stage] = "not-reached"
            else:
                out[stage] = "fail" if failed else "pass"
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
            # Name the first stage that actually FAILED a check; a stage
            # that was never reached is a consequence, not the cause.
            culprit = failing[0] if failing else broken[0]
            print(f"\n--- FAIL: the expedition loop broke at stage "
                  f"'{culprit}' (stages affected: {', '.join(broken)}) ---",
                  flush=True)
        else:
            print("\n--- PASS: the first expedition runs end to end "
                  "(prepare, travel, discover, extract, return, invest) ---",
                  flush=True)


# --------------------------------------------------------------------------
# How a run ends early, and what the stages hand each other
# --------------------------------------------------------------------------
class SetupError(RuntimeError):
    """The scenario could not reach the state it tests."""


class StageAbort(Exception):
    """A stage refused to go on, and the run ends with exit status 2.

    The pre-split `main` spelled this as a bare `return 2` from eleven
    places inside its own body; once the stage bodies became functions
    that had to become a value the facade propagates. An exception
    rather than a sentinel return, because the refusal points sit deep
    inside a stage (five of them inside `setup` alone) and a sentinel
    would have to be threaded back out through each of them.

    Deliberately SILENT: every one of those sites has already recorded
    its own failing check, so the facade catches this and adds nothing.
    It is caught BEFORE the `except Exception` clause, which would
    otherwise record a second, spurious operational failure.
    """


class Fingerprint:
    """The deterministic run identity, accumulated in one place.

    Stage owners contribute keys through `update`/`__setitem__`; the
    facade prints it once, from its guaranteed `finally`, so an
    early-refusal exit still emits a partially populated line exactly as
    the pre-split probe did. `sort_keys` makes the line a function of
    the contents alone, so two runs can be diffed as one line.
    """

    def __init__(self, seed: int, size: int, plates: int) -> None:
        self._data: dict = {"seed": seed, "size": size, "plates": plates}

    def update(self, **kw) -> None:
        self._data.update(kw)

    def __setitem__(self, key: str, value) -> None:
        self._data[key] = value

    def line(self) -> str:
        return f"FINGERPRINT {json.dumps(self._data, sort_keys=True)}"


@dataclass
class ExpeditionState:
    """What one stage owner established that a later one needs.

    Every field was a local in the pre-split `main`, threaded between
    its inline stage bodies by scope. Making it an explicit record is
    what lets the stages have separate owners without any of them
    reaching into a sibling, and what makes requirement 5's
    single-sourcing checkable: each identity below is written by exactly
    one stage and read by name afterwards.
    """

    #: The live console port. One per invocation; the facade allocates
    #: it and both engines are reached through it in turn.
    port: int
    #: The run's one fingerprint accumulator.
    fp: Fingerprint
    #: The world's generation parameters, as `setup` passes them to
    #: `world.init`.
    seed: int
    size: int
    plates: int

    # ---- [setup]: the world, the ruin, the colony, the party --------
    #: The selected ruin's placement row, as `pick_site` read it.
    ruin: dict = field(default_factory=dict)
    #: Its stable location-instance id (#911), and its anchor.
    ruin_id: int = -1
    ruin_xy: tuple = (0.0, 0.0)
    #: The colony site row, including its water tile and dry shore.
    site: dict = field(default_factory=dict)
    #: The colony tile the portal stands on.
    home: tuple = (0, 0)
    portal_bid: int = -1
    #: The four party identities, assigned by uid order from the
    #: portal's own fixed spawn sequence.
    scout: int = -1
    prepared: int = -1
    control: int = -1
    mule: int = -1
    #: The colonists that are not travelling — the per-unit knowledge
    #: layer's never-went-there control (#915).
    stay_home: list = field(default_factory=list)
    #: Colony storage, the tile to deposit from, and its footprint.
    storage_bid: int = -1
    deposit_spot: tuple = ()
    foot: tuple = ()
    #: The ruin's own two incidental loot rolls, and the one chosen to
    #: be carried home.
    loot: list = field(default_factory=list)
    target: dict = field(default_factory=dict)

    # ---- [travel]: the paired observations the control is scored on --
    #: The region that counts as "at the ruin".
    box: tuple = ()
    #: Instances of the target def the carrier already held before the
    #: leg, so the pickup is identified by exclusion.
    already: set = field(default_factory=set)
    #: Vitals and ration counts at the two shared observation points,
    #: captured inside the paused windows and never re-read.
    depart: dict = field(default_factory=dict)
    depart_food: dict = field(default_factory=dict)
    arrive: dict = field(default_factory=dict)
    arrive_food: dict = field(default_factory=dict)
    #: Whether each traveller was caught eating live — corroboration
    #: beside the durable ration count.
    ate: dict = field(default_factory=dict)
    #: Stay-at-home colonists observed at the ruin during the leg, and
    #: therefore excluded from the knowledge assertion.
    visited_ruin: set = field(default_factory=set)

    # ---- [extract]: what was recovered ------------------------------
    #: The recovered target's inventory row and its physical instance
    #: id — the identity the return, save and load stages re-check.
    recovered: dict = field(default_factory=dict)
    instance_id: int = -1
    #: #917's guaranteed significant item, by the physical instance id
    #: its own obligation row reports.
    sig_phys: int = -1


# --------------------------------------------------------------------------
# Isolation, boot and bootstrap
# --------------------------------------------------------------------------
YAML_LOADERS = [
    ("data/substances/*.yaml", "engine.loadSubstanceYaml"),
    ("data/items/*.yaml", "engine.loadItemYaml"),
    ("data/equipment/*.yaml", "engine.loadEquipmentYaml"),
    ("data/materials/*.yaml", "engine.loadMaterialYaml"),
    ("data/flora/*.yaml", "engine.loadFloraYaml"),
    ("data/units/*.yaml", "engine.loadUnitYaml"),
    ("data/buildings/*.yaml", "engine.loadBuildingYaml"),
    ("data/loot_tables/*.yaml", "engine.loadLootTableYaml"),
]

#: The AI stack, the portal's own spawn sequencer, and the tutorial
#: runtime — the same modules scripts/init_loader.lua loads in a real
#: session, at the same z-orders. Headless has no loading screen, so
#: both engines do it by hand.
SCRIPTS = [
    ("scripts/unit_stats.lua", 0.1),
    ("scripts/unit_resources.lua", 0.2),
    ("scripts/unit_ai.lua", 0.1),
    ("scripts/building_spawn.lua", 0.3),
    ("scripts/tutorial_progress.lua", 1.0),
    ("scripts/tutorial_eval.lua", 1.0),
]


def make_isolated_root(base: str) -> str:
    """A throwaway resource root: the real read-only content families
    symlinked, plus its OWN empty saves/."""
    root = os.path.join(base, "root")
    os.makedirs(root, exist_ok=True)
    for family in ("scripts", "assets", "data", "config"):
        target = os.path.join(root, family)
        if not os.path.exists(target):
            os.symlink(os.path.join(REPO, family), target)
    os.makedirs(os.path.join(root, "saves"), exist_ok=True)
    return root


def bootstrap(port: int) -> None:
    for pattern, fn in YAML_LOADERS:
        for path in sorted(glob.glob(os.path.join(REPO, pattern))):
            send(port, f"{fn}('{os.path.relpath(path, REPO)}'); return 'ok'",
                 timeout=20.0)
    send(port, "engine.loadLocationYaml('data/locations/ruin_small.yaml'); "
               "return 'ok'", timeout=20.0)
    got = send(port, "return engine.loadTutorialDir('data/tutorials')",
               timeout=20.0)
    if got.strip() in ("", "nil", "false"):
        raise SetupError(f"engine.loadTutorialDir failed: {got!r}")
    for script, z in SCRIPTS:
        send(port, f"engine.loadScript('{script}', {z}); return 'ok'", timeout=20.0)
    tree = send(port, "local t = require('scripts.tutorial_progress').ensureTree(); "
                      "return t and t.id or 'nil'", timeout=15.0)
    if tree != "first_session":
        raise SetupError(f"expected the first_session tutorial tree, got {tree!r}")
    # Scenario condition, applied to the whole session and therefore to
    # BOTH travellers symmetrically (see the facade's module
    # docstring): retire
    # the #94 forage ladder so an unprovisioned traveller cannot eat the
    # landscape instead of its pack.
    send(port, "require('scripts.unit_ai_tunables').acolyte.forage_max_fraction "
               "= -1; return 'ok'", timeout=15.0)


def boot_probe(root: str, port: int, log: str, label: str):
    return boot(port, log=log, label=label, ready_timeout=240,
                args=["--resource-root", root])


# --------------------------------------------------------------------------
# Shared stage-aware checks -- both are called by more than one owner
# --------------------------------------------------------------------------
def check_ai_tick_clean(chk: Checks, log_path: str, label: str) -> None:
    """No `Lua error in update()` in the engine log.

    A raise out of the unit_ai update tick kills EVERY unit's AI for that
    tick, not just the action that raised — so it fails silently as
    "nothing moved" rather than as an error anyone is looking at."""
    try:
        with open(log_path) as fh:
            bad = [ln.strip() for ln in fh if "Lua error in update()" in ln]
    except OSError as exc:
        chk.ok(False, f"{label}: could not read the engine log ({exc})")
        return
    uniq = sorted(set(bad))
    chk.ok(not uniq,
           f"{label}: the unit_ai update tick raised nothing "
           f"({len(bad)} error line(s){': ' + uniq[0] if uniq else ''})")


def assert_real_travel(chk: Checks, samples: list, goal, label: str,
                       min_samples: int, min_closed: float) -> None:
    """The journey happened over many ticks and closed on its
    destination — not a teleport, not a stalled unit."""
    if not samples:
        chk.ok(False, f"{label} produced no position samples")
        return
    steps = [dist(samples[i], samples[i + 1]) for i in range(len(samples) - 1)]
    biggest = max(steps) if steps else 0.0
    chk.ok(len(samples) >= min_samples and 0.05 < biggest <= MAX_STEP_TILES,
           f"{label} is real multi-tick travel — moving, and not a teleport "
           f"({len(samples)} samples, largest single step {biggest:.2f} tiles)")
    chk.ok(dist(samples[-1], goal) < dist(samples[0], goal) - min_closed,
           f"{label} closed on its destination "
           f"({dist(samples[0], goal):.1f} -> {dist(samples[-1], goal):.1f} tiles)")


def remove_root(base: str, keep_root: bool) -> None:
    """Release the invocation's throwaway tree, or name it and keep it.

    Reached from the facade's guaranteed `finally`, so it runs on every
    exit path — a pass, a failed check, an early refusal, and an
    operational failure alike.
    """
    if keep_root:
        print(f"kept resource root: {base}", flush=True)
    else:
        shutil.rmtree(base, ignore_errors=True)
