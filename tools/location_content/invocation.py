#!/usr/bin/env python3
"""Invocation infrastructure for the location-content probe (#2095).

Everything one run of `tools/location_content_probe.py` needs that is
NOT a scenario assertion: the single artifact directory the invocation
owns (#1884), the throwaway resource root inside it (#1620), the one
boot funnel every engine goes through, the disposal that guarantees no
engine outlives the tree it was writing into, the request-specific
save/load completion helpers, and the `ScenarioState` record the facade
threads from one scenario owner to the next.

Split out of the probe itself by #2095. Nothing here decides what a
scenario proves, and nothing here boots on its own behalf --
`location_content_probe.run` owns the process sequence and calls
`boot_isolated` at each of its seven call sites.
"""
from __future__ import annotations

import os
import shutil
import stat
import subprocess
from dataclasses import dataclass, field
from pathlib import Path

from probelib import (boot, capture_request_id, send, wait_load_published,
                      wait_save_complete)
from probe_runner_diagnostics import FailureEmitter   # durable failure records (#1982)

#: The repository root. This module sits two directories deeper than the
#: probe it was split out of (`tools/location_content/`), so the walk up
#: is one longer -- the VALUE is the same repository root
#: `tools/test_location_probe_config_isolation.py` pins.
REPO = Path(__file__).resolve().parent.parent.parent
#: #1982 — this run's durable failure records, built at import so the
#: offset each carries is measured from the probe's own start.
FAILURE = FailureEmitter("location_content_probe")
#: How a save/load failure message refers to the engine log when the
#: caller supplied no path. Spelled as a constant so the f-strings below
#: need no escaped quote inside their expression, which only Python 3.12
#: onwards accepts (PEP 701).
THIS_RUNS_LOG = "this run's engine log"
#: Prefix of this invocation's throwaway resource root. The random
#: suffix mkdtemp appends to it is also the per-invocation save-slot
#: token, so the root and the slots it holds share one identity.
ROOT_PREFIX = "location_content_probe_"


# --------------------------------------------------------------------------
# Isolated resource root + request-specific save/load completion (#1620)
# --------------------------------------------------------------------------
class _PhaseAborted(Exception):
    """A phase's save or load never reached its own terminal successful
    status, so the phase must STOP rather than assert against whatever
    session happens to be live — the reader is exactly what must not
    start (#1620 requirements 2-4). The failure is already recorded; the
    surrounding `finally` still shuts the engine down in order."""


def _make_owner_writable(top: str) -> None:
    """Add owner write (and directory search) permission throughout the
    freshly copied `config/` tree.

    `shutil.copytree` reproduces the SOURCE's mode bits, so a checkout
    whose `config/` is read-only — a read-only mount, a CI cache
    restored read-only, an archive unpacked without write bits — would
    otherwise hand this run a private copy it can neither write nor
    delete: a directory needs owner write+search before any of its
    entries can be unlinked, so `remove_isolated_root` would report
    residue on a run that did nothing wrong. Only THIS invocation's copy
    is relaxed; the source's own mode bits are never touched (#1729; the
    tools/location_embark_probe.py pattern from #1569).
    """
    for path, dirs, files in os.walk(top):
        for name in [None, *dirs, *files]:
            target = path if name is None else os.path.join(path, name)
            try:
                mode = os.lstat(target).st_mode
                if stat.S_ISLNK(mode):
                    continue
                extra = (stat.S_IRWXU if stat.S_ISDIR(mode)
                         else stat.S_IRUSR | stat.S_IWUSR)
                os.chmod(target, stat.S_IMODE(mode) | extra)
            except OSError:
                # Best effort: a mode this process cannot change is
                # reported by the cleanup that actually trips over it,
                # naming the path it failed on, rather than here.
                pass


def make_isolated_root(base: str) -> str:
    """A throwaway resource root for THIS invocation: the repository's
    real scripts/assets/data symlinked in (read-only content, safe to
    share), its `config/` COPIED without the developer's `*.local.yaml`
    overrides, plus its OWN empty saves/ directory.

    Every boot belonging to the invocation is handed this root, so the
    fixtures saved and loaded here are never written into, and can never
    be satisfied by, a slot reachable from a normal `cabal run` (#1620
    requirement 5; the tools/chop_probe.py pattern). The CONTENT
    families are SYMLINKS, and shutil.rmtree unlinks a symlink rather
    than descending it, so teardown can never reach the repository's own
    directories.

    `config/` is NOT one of those families and must never be symlinked
    (#1729). Engine initialization is itself a `config/` writer:
    src/Engine/Asset/YamlNotifications.hs materializes
    `notifications.local.yaml` from registry defaults whenever that file
    is absent, and src/Engine/Core/Init.hs migrates tracked legacy
    configuration into absent local files. Through an aliased `config/`
    those writes land in the developer's checkout, and teardown — which
    only unlinks the alias — leaves them there. Copying also keeps a
    personal override from deciding what this run observes. The copy is
    made owner-writable so a read-only source cannot produce a tree this
    run is unable to remove.
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
        _make_owner_writable(config_dst)
    os.makedirs(os.path.join(root, "saves"), exist_ok=True)
    return root


def remove_isolated_root(base: str) -> str | None:
    """Remove this invocation's root, REPORTING a failure instead of
    swallowing it — the "no save artifact the run created remains on
    disk" guarantee (#1620 requirement 6) is only real if a removal that
    did not happen is visible. Returns None on success, else the message
    to fail with.
    """
    try:
        shutil.rmtree(base)
    except OSError as exc:
        return (f"could not remove this run's temporary resource root "
                f"{base}: {exc}")
    if os.path.exists(base):
        return (f"this run's temporary resource root {base} still exists "
                f"after removal")
    return None


class RunArtifacts:
    """Every file one invocation of this probe creates, under a single
    directory that invocation owns (#1884).

    `base` comes from `tempfile.mkdtemp`, so it is this process's alone
    and disjoint from every other invocation's -- which is what makes
    the logical names inside it (`engine.log`, `bogus.yaml`,
    `quinoa.yaml`, ...) safe to keep fixed. #1620 had already moved the
    SAVE slots into `root`; the fixtures and the log stayed behind as
    fixed `/tmp` names -- `/tmp/location_content_engine.log` and the
    five `/tmp/loc_content_probe_*.yaml` -- each written with a
    truncating `open(..., "w")` (`probelib.boot` opens the log the same
    way) and removed by nothing. Two concurrent runs collided on all
    six: one could overwrite a fixture between another's write and the
    engine-side read of it, and both interleaved into one truncated
    log. That last one is not merely untidy here, because the log is
    ASSERTED against -- the integrity-diagnostic read in phase 2 and
    the warning read in phase 3 -- so a foreign truncation could turn a
    passing phase into a failure or a failure into a pass.

    Nothing this process did not create is ever named, so a file of the
    same name outside the tree -- a developer's own
    `/tmp/loc_content_probe_bogus.yaml` -- is not opened for writing,
    truncated, modified or removed.
    """

    def __init__(self, base: str) -> None:
        self.base = base
        self.root = os.path.join(base, "root")
        self.logs = os.path.join(base, "logs")
        self.fixtures = os.path.join(base, "fixtures")
        #: Every engine `boot_isolated` has LAUNCHED, registered the
        #: statement after `probelib.boot`'s own `Popen` rather than
        #: only once it returns -- which on a hung boot is
        #: `ready_timeout` (three minutes) later. `main`'s guard walks
        #: this before it removes anything, so no engine is still
        #: writing into a tree that is being deleted.
        self.launched: list = []

    def build(self) -> str:
        """Stage this invocation's throwaway resource root (#1620) and
        the two artifact directories beside it, and answer with the root.

        The root itself is still `make_isolated_root`'s -- unchanged, and
        still the builder `tools/portal_ghost_probe.py` imports and
        `tools/test_location_probe_config_isolation.py` pins.
        """
        root = make_isolated_root(self.base)
        os.makedirs(self.logs, exist_ok=True)
        os.makedirs(self.fixtures, exist_ok=True)
        return root

    @property
    def engine_log(self) -> str:
        """The engine's stdout/stderr capture, shared by every boot of
        this invocation and truncated by each of them.

        `probelib.boot` opens it `"w"`, so a boot still starts the log
        empty -- the per-boot isolation phase 2's diagnostic read
        depends on is exactly what it was. What changed is WHOSE log it
        is: a second invocation now truncates its own, not this one's.
        """
        return os.path.join(self.logs, "engine.log")

    def fixture(self, name: str) -> str:
        """One of the probe's own inline YAML fixtures. The path is
        handed to the ENGINE, which has chdir'd into `root`
        (`App.ResourceRoot`), so it stays absolute for the same reason
        it always was."""
        return os.path.join(self.fixtures, f"{name}.yaml")


def _describe_held(path: str) -> str:
    """How the retention summary describes one artifact directory.

    A path that does not EXIST is not the same as one that is empty: a
    run that failed part-way through staging never created it, and
    calling it empty would send the reader looking for a directory that
    is not there.
    """
    if not os.path.exists(path):
        return " (never created -- the run ended before staging reached it)"
    try:
        held = sorted(os.listdir(path))
    except OSError as exc:
        return f" (unreadable: {exc})"
    return f" ({', '.join(held)})" if held else " (empty)"


def abandon_engine(proc) -> None:
    """Make sure an engine this run launched is dead, without talking to
    the port. A no-op on a handle that has already exited.

    Every phase below already shuts its own engine down through
    `quit_engine` in a `finally`. This is the backstop for the windows
    that reach past one: `probelib.boot` hands the process over the
    moment it exists (`on_launch`) and only decides about READY up to
    three minutes later, so an interrupt in that span leaves a live
    engine no `proc = boot_isolated(...)` assignment will ever name; and
    `quit_engine` is itself interruptible -- it sends, waits, then
    hard-kills -- so a Ctrl-C inside it unwinds with the engine possibly
    still running. Either way the process would still be writing into
    the tree `main` is about to delete.

    Deliberately NOT `quit_engine`: that sends `engine.quit()` to the
    PORT, and a boot fails on a busy port precisely because the port
    belongs to somebody else's instance. `kill()` is a single syscall
    and SIGKILL cannot be caught, so once it has landed the process is
    dead whatever happens to the reap that follows.
    """
    if proc.poll() is not None:
        return
    proc.kill()
    try:
        proc.wait(timeout=10)
    except subprocess.TimeoutExpired:
        FAILURE.check(f"the engine this run launched (pid {proc.pid}) did "
                      f"not die when killed")


def release_artifacts(art: RunArtifacts, keep: bool) -> str | None:
    """Retire this invocation's artifact directory -- fixtures, engine
    log and resource root together -- and report a failure to remove it
    rather than swallowing one. Returns None when the run may still
    report its own result, else the message to fail with.

    `--keep-artifacts` is the intentional exception to that removal
    (#1884 requirement 5): the tree is retained and named, and the run
    keeps whatever result its own checks produced. Without the flag the
    removal is mandatory and #1620 requirement 6's reporting is exactly
    as it was, because this delegates to `remove_isolated_root`.
    """
    if keep:
        # Each line names what this run ACTUALLY produced. A run that
        # died before READY holds no fixtures and no save slot, and
        # saying otherwise would send the reader looking for files the
        # failure is the reason they do not have.
        print(f"\nretained this run's artifacts (--keep-artifacts): "
              f"{art.base}")
        for label, path in (("engine log", art.logs),
                            ("fixtures", art.fixtures),
                            ("saves", os.path.join(art.root, "saves"))):
            print(f"  {label:14} {path}{_describe_held(path)}")
        print(f"  {'resource root':14} {art.root}"
              + ("" if os.path.isdir(art.root) else " (never created)"))
        return None
    return remove_isolated_root(art.base)


def boot_isolated(port: int, art: RunArtifacts, **kwargs):
    """The one funnel every boot in this file goes through, so the log
    path and the launched-engine registration are decided once."""
    return boot(port, log=art.engine_log,
                args=["--resource-root", art.root],
                on_launch=art.launched.append, **kwargs)


def save_and_wait(port: int, page: str, slot: str,
                  failures: list[str], log: str | None = None) -> bool:
    """engine.saveWorld, then tie completion to THIS request's own id.

    saveWorld only ACCEPTS synchronously — it returns false on a
    validation failure (the detailed reason goes to the engine log) and
    true once the command is queued, while the encode and disk write run
    afterwards. So a sleep proves nothing, and a fixed slot name means a
    PRIOR generation of the same slot could satisfy the reader
    (#1620). Every reader here starts only after this returns True.

    `log` names the engine log the failure message points at. It stays a
    caller-supplied path -- `tools/portal_ghost_probe.py` passes its own
    -- but no longer defaults to a module-global one, because since
    #1884 the log belongs to the INVOCATION and only a caller holding
    its `RunArtifacts` knows where it is.
    """
    accepted = send(port, f"return engine.saveWorld('{page}', '{slot}')").strip()
    if accepted != "true":
        failures.append(
            f"engine.saveWorld(page '{page}', slot '{slot}') was not accepted "
            f"(returned {accepted!r}); the validation reason is logged in "
            f"{log or THIS_RUNS_LOG}")
        return False
    request_id = capture_request_id(port, "return engine.getSaveStatus()")
    if request_id is None:
        failures.append(
            f"engine.getSaveStatus() never reported a request id for "
            f"saveWorld(page '{page}', slot '{slot}')")
        return False
    ok, status = wait_save_complete(port, request_id)
    if not ok:
        failures.append(
            f"save of page '{page}' to slot '{slot}' (request {request_id}) "
            f"did not reach SaveCaptureComplete: {status}")
        return False
    print(f"  saved '{slot}' (request {request_id}, {status.get('phase')})")
    return True


def load_and_wait(port: int, slot: str, failures: list[str],
                  seconds: float = 60.0, log: str | None = None) -> bool:
    """engine.loadSave, then wait for THAT request id to publish.

    Issue #763: loadSave only ACCEPTS synchronously, and the saved page
    does not exist live until the transaction publishes. Passing the
    captured id to wait_load_published is what stops a terminal status
    left behind by an earlier transaction from satisfying this wait.
    """
    accepted = send(port, f"return engine.loadSave('{slot}')").strip()
    if accepted != "true":
        failures.append(
            f"engine.loadSave('{slot}') was not accepted (returned "
            f"{accepted!r}); the reason is logged in "
            f"{log or THIS_RUNS_LOG}")
        return False
    request_id = capture_request_id(port, "return engine.getLoadStatus()")
    if request_id is None:
        failures.append(
            f"engine.getLoadStatus() never reported a request id for "
            f"loadSave('{slot}')")
        return False
    published, status = wait_load_published(port, seconds,
                                            request_id=request_id)
    if not published:
        failures.append(f"load of '{slot}' (request {request_id}) did not "
                        f"publish: {status}")
        return False
    return True


@dataclass
class ScenarioState:
    """What one scenario owner learned that a later one needs (#2095).

    The facade owns this record and hands it to each owner in turn, so
    cross-scenario handoff is an explicit argument rather than a module
    global that any import could reach or mutate out of order. Every
    field here is a value `run` used to accumulate in a local variable
    across its phases.

    Deliberately NOT here: the import-time `FAILURE` emitter, `REPO`,
    `ROOT_PREFIX` and the five fixture bodies. Those are invocation
    infrastructure and immutable configuration -- not state one scenario
    produces for another -- and moving them into this record would
    rebuild them per run for no gain.
    """

    #: Every location the initial world placed, as `placed_ready` saw
    #: it -- the list `pick_far_tile` needs to find a tile outside every
    #: sight box.
    placed_all: list[dict] = field(default_factory=list)
    #: The `ruin_small` subset of it, which the later processes
    #: re-derive their own worlds against.
    ruins: list[dict] = field(default_factory=list)
    #: Spawn counts after the initial process settled, REFRESHED once
    #: the discovery/knowledge units exist so the no-respawn comparison
    #: accounts for them.
    counts1: dict = field(default_factory=dict)
    #: (gx, gy) -> (floors, walls, posts) for each initial ruin.
    geoms1: dict = field(default_factory=dict)
    #: instance id -> sorted ground-item defName multiset (#948).
    loot1: dict[int, list[str]] = field(default_factory=dict)
    #: The "<page>#<instance id>" memory key the discoverer learned, and
    #: the units that hold it (#915).
    r0mem_key: str = ""
    mem_uids: tuple[int, ...] = ()
    #: The unit carrying the deliberately unresolvable memory, and the
    #: two resolving siblings that must survive its removal.
    dangling_uid: int = -1
    sibling_keys: tuple[str, ...] = ()
    #: Whether each fresh-process phase has a save to read.
    saved_content: bool = False
    saved_naming: bool = False
    #: instance id -> (name, gloss) on the named world (#1101).
    named: dict = field(default_factory=dict)
