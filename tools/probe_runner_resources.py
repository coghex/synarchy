#!/usr/bin/env python3
"""What a probe run may TOUCH at once, in-process and across processes.

The reader/writer conflict model over shared repository-relative
resources (#1322, #1444): the two declaration tables, the in-process
`ResourceLedger`, the cross-process interest calculation and holds taken
through `tools/probe_resource_lock.py` (#1436), the ancestor holds a
nested runner inherits through the environment (#1570), the repository
namespace those holds are keyed by, and the single engine-executable
preflight that resolves the binary every probe execs.

Dependencies (#2074 requirement 11): `probe_engine` and
`probe_resource_lock`, plus the registry owner's declarations when a
future resource rule needs them. Nothing here imports the lifecycle, the
scheduler, or the runner command.

`ENGINE_EXECUTABLE` lives here because this owner resolves it: the
preflight below fills it in, and `probe_runner_lifecycle.run_one` reads
THIS cell at call time to hand the child its `probe_engine.ENV_ENGINE_EXE`
(#2074 requirement 14). There is exactly one such cell, and
`tools/deflake.py` writes it here for the de-flake lab's own runs.

`REPO_ROOT` is deliberately NOT redefined here: `probe_engine.REPO_ROOT`
is the one authoritative cell every owner reads at call time, so a test
that repoints it repoints it for all of them at once.
"""
from __future__ import annotations
import contextlib
import os

import probe_engine
import probe_resource_lock

# ---------------------------------------------------------------------------
# Shared repository-relative resources: the reader/writer conflict model
# (#1322, #1444)
#
# Two tables, one per interest. `IMPLICIT_SHARED_RESOURCES` is what EVERY
# registered probe holds SHARED; `EXCLUSIVE_RESOURCES` is what a named probe
# holds to itself. Shared holders coexist freely; an exclusive holder
# coexists with nothing else that names the same resource. The scheduler
# below reads only these two tables and knows nothing about any particular
# probe, so a future probe that must run alone is one row here. Since #1436
# `tools/probe_resource_lock.py` reads the same two tables through the same
# two accessors, so one conflict model covers both inside this process and
# between processes.
#
# `repo-config` is the tracked `config/` directory of THIS checkout.
#
# EXCLUSIVE side. `config_migration_probe.py` and `config_state_probe.py`
# each move the same three tracked legacy files
# (`config/{video,keybinds,notifications}.yaml`) aside into their OWN fixed
# /tmp backup directory and each delete the same three `config/*.local.yaml`
# paths before restoring — so run together, one probe's cleanup deletes or
# overwrites state the other owns and can leave a tracked file missing or
# holding fixture content in the primary checkout. (`config_state_probe.py`
# additionally owns `config/save.local.yaml`, which
# `config_migration_probe.py` does not touch; the three legacy paths and the
# other three local paths are shared.) Isolating them behind
# `--resource-root` is deliberately NOT the fix: `config_state_probe.py`
# asserts against the real tracked tree because proving the engine never
# dirties it is that probe's whole purpose (#638), and under an isolated
# root those assertions go vacuous.
#
# SHARED side (#1444). Those two are not the whole conflict set, because
# ENGINE INIT ITSELF writes `config/` when a local file is absent, and
# absent-local is precisely the fixture state both config probes install:
# `Engine.Asset.YamlNotifications.loadOverrides` materializes
# `config/notifications.local.yaml` from registry defaults, and
# `Engine.Core.Init.migrateLegacyConfig` copies a present legacy file over
# an absent local one for video/keybinds/notifications — or, for a
# video/keybinds placeholder it judges neutral (#1937), writes a
# `config/*.legacy-neutral.local.yaml` record instead. A foreign engine
# booting while a config probe has removed the local files but has not yet
# removed or installed every legacy file can therefore copy stale legacy
# content, or materialize registry defaults, that the config probe then
# reads back as its own result — a spurious verdict, not a corrupted
# checkout (both probes clear-then-restore from backups).
#
# So every probe declares the shared interest, rather than an enumerated
# engine-booting subset: the subset would be a second list to keep in sync
# with ~85 probes, and a probe that GAINS an engine boot must not silently
# lose the guard. Shared-against-shared never blocks, so the conservative
# reading costs the undeclared probes nothing.
#
# `cabal-build` is this checkout's shared Cabal build state — the one
# `dist-newstyle` every probe's engine comes out of (#1570). Since the
# preflight below resolves the executable once and every probe execs that
# binary, an ordinary probe only READS it, which is the shared interest.
# Three registered probes still drive Cabal themselves, and they are the
# reason the resource exists: `persistence_contract` and
# `persistence_contract_sweep` run `cabal repl test:synarchy-test-headless`
# through `persistence_snapshot.compare_session_files`, and
# `save_compat_migration` (and the other two) through
# `save_compat_audit.dump_canonical_summary`. A `cabal repl` recompiles into
# the same inplace package database whose concurrent mutation is the defect,
# so each of them takes `cabal-build` EXCLUSIVELY: two of them cannot
# overlap each other, and neither overlaps a probe reading the binary they
# may be relinking. They are deliberately RETAINED rather than converted —
# a GHCi consumer is not an engine boot and has no prebuilt equivalent.
#: The shared Cabal build state, named once so the preflight below, the
#: two declaration tables, and the direct path's own preparation (#1913)
#: cannot drift apart. The name is `probe_engine`'s, because that module
#: owns every Cabal contact a probe makes.
BUILD_RESOURCE = probe_engine.BUILD_RESOURCE

IMPLICIT_SHARED_RESOURCES: tuple[str, ...] = ("repo-config", BUILD_RESOURCE)

EXCLUSIVE_RESOURCES: dict[str, tuple[str, ...]] = {
    "config_migration": ("repo-config",),
    "config_state": ("repo-config",),
    "persistence_contract": (BUILD_RESOURCE,),
    "persistence_contract_sweep": (BUILD_RESOURCE,),
    "save_compat_migration": (BUILD_RESOURCE,),
}


def exclusive_resources(key: str) -> set[str]:
    """The resources probe ``key`` needs exclusively; empty when it declares none."""
    return set(EXCLUSIVE_RESOURCES.get(key, ()))


def shared_resources(key: str) -> set[str]:
    """The resources probe ``key`` touches but does not need exclusively.

    A resource the probe declared EXCLUSIVELY is subtracted rather than
    held twice: an interest is one or the other, never both, so releasing
    one cannot leave the other behind.
    """
    return set(IMPLICIT_SHARED_RESOURCES) - exclusive_resources(key)


class ResourceLedger:
    """Which resources the running probes hold, and in which interest.

    One reader/writer lock per resource name: any number of probes may hold
    a resource SHARED at once, while an EXCLUSIVE holder runs only when no
    one else holds it at all — in either interest, in either direction. The
    shared side is a COUNT per resource, not a set: three concurrent readers
    must all release before a writer may start.

    Not thread-safe by design — the scheduler owns it from its own thread
    and the workers never touch it, which is what keeps a blocked probe out
    of a worker slot instead of parked inside one.

    In-process ONLY, and deliberately so: it is a plain object in one
    runner's memory, so it says nothing about a second runner or a
    `/deflake` measurement. `tools/probe_resource_lock.py` is the
    cross-process half (#1436), taken at the same dispatch point from the
    same two declaration tables; see the note below it.
    """

    def __init__(self) -> None:
        self._exclusive: set[str] = set()
        self._shared: dict[str, int] = {}

    def blocked(self, need_exclusive: set[str], need_shared: set[str]) -> bool:
        """True when these interests cannot be granted right now."""
        if need_exclusive & (self._exclusive | set(self._shared)):
            return True
        return bool(need_shared & self._exclusive)

    def acquire(self, need_exclusive: set[str], need_shared: set[str]) -> None:
        self._exclusive |= need_exclusive
        for name in need_shared:
            self._shared[name] = self._shared.get(name, 0) + 1

    def release(self, need_exclusive: set[str], need_shared: set[str]) -> None:
        self._exclusive -= need_exclusive
        for name in need_shared:
            remaining = self._shared.get(name, 0) - 1
            if remaining > 0:
                self._shared[name] = remaining
            else:
                # Dropped rather than kept at zero, so `blocked` can read
                # the keys as "held shared by someone" without counting.
                self._shared.pop(name, None)

    def idle(self) -> bool:
        """True when nothing is held, and therefore nothing can be blocked."""
        return not self._exclusive and not self._shared


# ---------------------------------------------------------------------------
# The same reader/writer model BETWEEN processes (#1436)
#
# The ledger above coordinates the probes inside ONE runner. It cannot see a
# second `run_probes.py`, and it cannot see a `/deflake` measurement, yet
# every one of them drives the same checkout's tracked `config/` tree — so
# `config_state_probe.py` holding `repo-config` exclusively stopped nothing
# outside its own sweep. `tools/probe_resource_lock.py` is the cross-process
# half, keyed by the same two declaration tables above so there is one
# conflict model rather than two.
#
# Both layers apply, and they compose in one direction: the ledger decides
# what may overlap WITHIN this sweep, and the lock decides whether it may
# overlap something outside it. A probe the ledger holds back never attempts
# the cross-process acquisition at all, which is what keeps this process from
# conflicting with itself — two of its own probes asking for the same
# resource in incompatible interests would otherwise meet each other's flock.
RESOURCE_WAIT_POLL = 5.0

# The namespace every worktree of this repository shares, resolved from git
# rather than from a path. `None` means "resolve it from
# `probe_engine.REPO_ROOT`"; it is
# overridable only so `tools/test_run_probes.py` can isolate its synthetic
# sweep from the real repository's live locks. Production never sets it.
RESOURCE_NAMESPACE: str | None = None


def resource_namespace() -> str:
    """The cross-process resource namespace for this checkout's repository."""
    if RESOURCE_NAMESPACE is not None:
        return RESOURCE_NAMESPACE
    return probe_resource_lock.repository_namespace(probe_engine.REPO_ROOT)


@contextlib.contextmanager
def resource_hold(key: str, namespace, *, announce=None):
    """Hold `key`'s cross-process interests around ONE probe execution.

    Entered immediately before the probe process is launched and left only
    once `run_one` has returned, which is after it has reaped the probe's
    whole process group — so a foreign holder never starts while this
    probe's engine is still up, exactly as the in-process ledger already
    guarantees within a sweep.

    Waiting happens HERE, in front of the launch, so it is never charged to
    the probe: `run_one` starts its own clock after this returns, so a
    probe's reported `elapsed` and its `--timeout` cover execution alone and
    a queued probe can never be reported as a TIMEOUT.

    `namespace` of None disables the cross-process layer entirely for
    callers that have no repository to name (the module's own helpers used
    as a library); the in-process ledger is unaffected either way.
    """
    if namespace is None:
        yield None
        return
    need_exclusive, need_shared = cross_process_interests(key, namespace)
    hold = probe_resource_lock.wait_acquire(
        exclusive=need_exclusive, shared=need_shared,
        namespace=namespace, purpose=f"run_probes {key}",
        poll=RESOURCE_WAIT_POLL, announce=announce)
    try:
        yield hold
    finally:
        hold.release()


# ---------------------------------------------------------------------------
# Resources an ANCESTOR already holds on this process's behalf (#1570)
#
# `persistence_contract_sweep` is a registered probe that itself invokes
# `run_probes.py` for the probes it cross-references. Once the sweep holds
# `cabal-build` EXCLUSIVELY, the flock its own nested runner takes for a
# child probe — in EITHER interest — conflicts with its ancestor's, and the
# nested runner would then wait forever for a holder that is itself blocked
# waiting on the nested runner.
#
# So a runner exports what it holds EXCLUSIVELY to every probe it launches,
# and a nested runner drops those names from its CROSS-PROCESS requests: it
# is inside its ancestor's exclusion, not competing with it. Its own
# in-process ledger is untouched, so a nested sweep still serialises its own
# probes against each other exactly as before.
#
# Only EXCLUSIVE holds are exported, and that is the whole rule. An
# ancestor's exclusive hold already excludes every foreign process, so a
# descendant inside it needs nothing further. An ancestor's SHARED hold
# cannot stand in for a descendant's exclusive request and must not be
# skipped — while a descendant's SHARED request against it is granted by the
# kernel anyway (LOCK_SH beside LOCK_SH), so there is nothing to export.
#
# The namespace rides along and is compared before anything is inherited: a
# name means nothing outside the repository it was taken in.
ENV_HELD_EXCLUSIVE = "SYNARCHY_PROBE_HELD_EXCLUSIVE"
ENV_HELD_NAMESPACE = "SYNARCHY_PROBE_HELD_NAMESPACE"

#: Every variable THIS runner owns in a probe's environment, so
#: `run_one` can strip the lot from what it inherited before
#: supplying its own — the same rule `probe_protocol` states for its
#: own four. A nested runner re-derives each of them, so nothing is
#: lost by dropping a value the parent set.
RUNNER_ENV_VARS: tuple[str, ...] = (probe_engine.ENV_ENGINE_EXE,
                                    ENV_HELD_EXCLUSIVE,
                                    ENV_HELD_NAMESPACE)


def descendant_hold_env(key: str, namespace: str | None) -> dict[str, str]:
    """What probe `key`'s own descendants may treat as already excluded.

    The probe's own exclusive declarations PLUS whatever this runner
    itself inherited, so the rule survives a second level of nesting.
    Empty when there is nothing to inherit, which is the ordinary case
    and passes no environment override at all.
    """
    if namespace is None:
        return {}
    held = exclusive_resources(key) | inherited_exclusive_resources(namespace)
    if not held:
        return {}
    return {ENV_HELD_EXCLUSIVE: ",".join(sorted(held)),
            ENV_HELD_NAMESPACE: namespace}


def inherited_exclusive_resources(namespace: str | None,
                                  environ=None) -> set[str]:
    """Resources an ancestor process already holds exclusively for us."""
    env = os.environ if environ is None else environ
    if namespace is None or env.get(ENV_HELD_NAMESPACE) != namespace:
        return set()
    raw = env.get(ENV_HELD_EXCLUSIVE) or ""
    return {name.strip() for name in raw.split(",") if name.strip()}


def cross_process_interests(key: str, namespace: str | None,
                            environ=None) -> tuple[set[str], set[str]]:
    """`key`'s (exclusive, shared) interests for the CROSS-PROCESS layer.

    The in-process ledger keeps using `exclusive_resources` /
    `shared_resources` unchanged; only the flocks drop what an ancestor
    is already holding exclusively on this process's behalf.
    """
    inherited = inherited_exclusive_resources(namespace, environ)
    return (exclusive_resources(key) - inherited,
            shared_resources(key) - inherited)


# ---------------------------------------------------------------------------
# The engine executable: resolved ONCE per run (#1570)
#
# Overridable only so `tools/test_run_probes.py` can drive the preflight
# with a deterministic subprocess double instead of a real toolchain.
# Production leaves it None and `main` fills it in.
ENGINE_EXECUTABLE: str | None = None

#: The subprocess entry point `engine_preflight` resolves through. Tests
#: substitute a recording double; production leaves it None, which is
#: `subprocess.run`.
ENGINE_PREFLIGHT_RUNNER = None


@contextlib.contextmanager
def preflight_hold(namespace, *, announce=None, environ=None):
    """Hold the shared Cabal build state EXCLUSIVELY across the preflight.

    The preflight is itself a Cabal WRITER — one `cabal build` into the
    same `dist-newstyle` every probe's engine comes out of — so resolving
    the executable outside the exclusion would leave exactly the race
    this issue is about, one level up: two aggregate runs preflighting at
    once, or one runner's build landing inside another runner's
    `persistence_contract` / `save_compat_migration` `cabal repl`. Nothing
    inside a single run can see that; only the cross-process lock can.

    Held for the build alone and released before any probe is dispatched,
    so the sweep's own probes are never queued behind it. `namespace` of
    None disables the cross-process layer, exactly as `resource_hold`
    does, and an ancestor already holding this resource exclusively is
    inherited rather than waited on — `persistence_contract_sweep`'s
    nested runner is inside its ancestor's exclusion, not competing with
    it.
    """
    if (namespace is None
            or BUILD_RESOURCE in inherited_exclusive_resources(namespace,
                                                                environ)):
        yield None
        return
    hold = probe_resource_lock.wait_acquire(
        exclusive={BUILD_RESOURCE}, namespace=namespace,
        purpose="run_probes engine preflight",
        poll=RESOURCE_WAIT_POLL, announce=announce)
    try:
        yield hold
    finally:
        hold.release()


def engine_preflight(namespace=None, environ=None, *, announce=None) -> str:
    """The one Cabal contact an aggregate run makes, before any probe.

    Adopts an executable an ANCESTOR already resolved when there is one —
    that is how `persistence_contract_sweep`'s nested runner reaches its
    own probes without a second build, and it takes no lock because it
    builds nothing. Otherwise it runs one freshness build plus one `cabal
    list-bin`, INSIDE `preflight_hold` so no other runner or GHCi consumer
    is in the build directory at the same time. Raises
    `EngineExecutableError`, which `main` reports as a nonzero exit before
    a probe is spawned, a retry allocated, or any probe assertion
    attributed to it.
    """
    inherited = probe_engine.runner_executable(environ)
    if inherited is not None:
        return inherited
    with preflight_hold(namespace, announce=announce, environ=environ):
        return probe_engine.resolve_executable(
            probe_engine.REPO_ROOT, run=ENGINE_PREFLIGHT_RUNNER)
