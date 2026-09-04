#!/usr/bin/env python3
"""What one invocation of the embark probe owns, rather than what it
proves (#2164).

Three things that are all the same thing — the run itself, not any
scenario:

  * the aggregate failure ledger and the engine-log context a failing
    check quotes (`failures`, `check`, `set_log`). ONE list, reached by
    every owner and by the cleanup path below, because
    `location_embark_probe.report` reads exactly that list to decide the
    run's exit code;
  * the single directory this invocation owns, the throwaway resource
    root inside it, and the release that removes the whole tree again
    (#1569);
  * the request-specific save publication both durable slots go through
    (#1746).

Nothing here boots an engine or decides what a session asserts:
`location_embark_probe.run_probe` owns the ordered process lifecycle and
hands each owner the port it opened.
"""
from __future__ import annotations

import os
import shutil
import stat
from typing import NamedTuple

from probelib import capture_request_id, send, wait_save_complete

#: The repository root. This module sits two directories deeper than the
#: probe it was split out of (`tools/location_embark/`), so the walk up
#: is two longer -- the VALUE is the same root the facade used, and it is
#: the name `tools/test_location_embark_probe.py` patches to stand a
#: read-only checkout in front of `RunArtifacts.build`.
REPO = os.path.dirname(os.path.dirname(os.path.dirname(os.path.abspath(__file__))))


# --------------------------------------------------------------------------
# The aggregate failure ledger and engine-log context (#1982)
# --------------------------------------------------------------------------
failures: list[str] = []
_current_log: list[str | None] = [None]


def set_log(path: str | None) -> None:
    _current_log[0] = path


def current_log() -> str | None:
    """The engine log the run is booted against RIGHT NOW, or None.

    The reader half of `set_log`: `check` below quotes its tail beside a
    failing check, and `location_embark_probe.report` records it as the
    run's final context (#1982). Exposed as a function rather than as
    the list itself so the facade and every owner read the one cell this
    module owns, instead of each holding a binding that a later
    `set_log` would not reach.
    """
    return _current_log[0]


def _tail(path: str, n: int = 15) -> str:
    try:
        with open(path) as f:
            return "".join(f.readlines()[-n:])
    except OSError:
        return "(log unavailable)"


def check(name: str, ok: bool, detail: str = "") -> bool:
    print(f"  [{'PASS' if ok else 'FAIL'}] {name}"
          + (f" — {detail}" if detail and not ok else ""))
    if not ok:
        failures.append(name if not detail else f"{name} — {detail}")
        if _current_log[0]:
            print(f"    recent engine log ({_current_log[0]}):")
            for line in _tail(_current_log[0]).splitlines():
                print(f"      {line}")
    return ok



# --------------------------------------------------------------------------
# Invocation-owned artifacts and isolation (#1569)
# --------------------------------------------------------------------------
def _make_owner_writable(top: str) -> None:
    """Add owner write (and directory search) permission throughout a
    freshly copied tree.

    `shutil.copytree` reproduces the SOURCE's mode bits, so a checkout
    whose `config/` is read-only — a CI cache restored read-only, a
    read-only mount, an archive unpacked without write bits — yields a
    private `config/` this run cannot use and cannot delete: a directory
    needs owner write+search before any of its entries can be unlinked,
    so `release_artifacts` would report residue and leave the whole tree
    behind on a run that did nothing wrong. The copy is THIS
    invocation's, so it is made writable regardless of what the source
    happened to be; the source itself is never touched.
    """
    for path, dirs, files in os.walk(top):
        for name in [None, *dirs, *files]:
            target = path if name is None else os.path.join(path, name)
            try:
                mode = os.lstat(target).st_mode
                if stat.S_ISLNK(mode):
                    continue
                extra = stat.S_IRWXU if stat.S_ISDIR(mode) else stat.S_IRUSR | stat.S_IWUSR
                os.chmod(target, stat.S_IMODE(mode) | extra)
            except OSError:
                # Best effort: a mode this process cannot change is
                # reported by the cleanup that actually trips over it,
                # with the path it failed on, rather than here.
                pass


class RunArtifacts:
    """Every file one invocation of this probe creates, under a single
    directory that invocation owns.

    `base` comes from `tempfile.mkdtemp`, so it is this process's alone
    and disjoint from every other invocation's — which is what makes the
    logical names inside it (`engine_prep.log`, `location_embark_base`,
    `icon_discovered.png`) safe to keep fixed. Two concurrent runs on
    different `--port` values write two different trees; a developer
    save slot of the same name lives in the checkout's root and is never
    opened at all.
    """

    def __init__(self, base: str) -> None:
        self.base = base
        self.root = os.path.join(base, "root")
        self.logs = os.path.join(base, "logs")
        self.shots = os.path.join(base, "screenshots")

    def build(self) -> None:
        """Materialise the throwaway resource root and the two artifact
        directories beside it.

        The read-only content families are symlinked; `config/` is
        COPIED without the developer's `*.local.yaml` overrides, so a
        personal setting can neither be changed by this run nor decide
        what it observes; `saves/` starts empty and belongs to this run.
        `app/App/ResourceRoot.hs` chdirs each engine into `root`, so
        every relative write the sessions below make — the two save
        slots above especially — lands inside this tree.
        """
        os.makedirs(self.root, exist_ok=True)
        for family in ("scripts", "assets", "data"):
            target = os.path.join(self.root, family)
            if not os.path.exists(target):
                os.symlink(os.path.join(REPO, family), target)
        config_dst = os.path.join(self.root, "config")
        if not os.path.exists(config_dst):
            shutil.copytree(os.path.join(REPO, "config"), config_dst,
                            ignore=shutil.ignore_patterns("*.local.yaml"))
            _make_owner_writable(config_dst)
        os.makedirs(os.path.join(self.root, "saves"), exist_ok=True)
        os.makedirs(self.logs, exist_ok=True)
        os.makedirs(self.shots, exist_ok=True)

    def log(self, name: str) -> str:
        return os.path.join(self.logs, f"{name}.log")

    def boot_args(self, extra: list[str] | None = None) -> list[str]:
        """Engine CLI args pinning the boot to THIS run's root. Every
        boot the probe makes — including each phase-0 seed retry — goes
        through here, so none of them can fall back to the cwd."""
        return [*(extra or []), "--resource-root", self.root]


def release_artifacts(art: RunArtifacts, keep: bool) -> None:
    """Retire this invocation's artifact directory, once every engine it
    booted has been through `quit_engine`.

    Without `--keep-artifacts` the whole tree goes away and anything
    that SURVIVES is recorded as a failing check: a green result sitting
    beside leftover saves is exactly the outcome this isolation exists
    to prevent, so it must not be reported as a pass. That residue
    report is not the diagnostic opt-in — it names what is left over
    precisely because the run did not intend to leave it.

    Only ever removes the directory this process made with
    `tempfile.mkdtemp`; `rmtree` unlinks the symlinked content families
    rather than recursing into them, so the real `scripts/`, `assets/`
    and `data/` are never followed.
    """
    if keep:
        # Each line names what this run ACTUALLY produced. A run that
        # failed at phase 0 holds no save slot and no screenshot, and
        # saying otherwise would send the reader looking for files the
        # failure is the reason they do not have.
        saves = os.path.join(art.root, "saves")
        print(f"\nretained this run's artifacts (--keep-artifacts): {art.base}")
        for label, path in (("engine logs", art.logs),
                            ("screenshots", art.shots),
                            ("saves", saves)):
            try:
                held = sorted(os.listdir(path))
            except OSError:
                held = []
            print(f"  {label:14} {path}"
                  + (f" ({', '.join(held)})" if held else " (empty)"))
        print(f"  {'resource root':14} {art.root}")
        return
    try:
        shutil.rmtree(art.base)
    except OSError as exc:
        failures.append(f"could not remove this run's artifact directory "
                        f"{art.base}: {exc}")
        return
    if os.path.exists(art.base):
        failures.append(f"this run's artifact directory survived removal: "
                        f"{art.base}")


# --------------------------------------------------------------------------
# Durable saves (#1746)
# --------------------------------------------------------------------------
def save_and_wait(port: int, page: str, slot: str, label: str) -> bool:
    """`engine.saveWorld`, then tie completion to THIS request's own id.

    `engine.saveWorld` only ACCEPTS synchronously
    (src/Engine/Scripting/Lua/API/Save.hs): it returns false on a
    validation failure — with the reason going to the engine log, not
    to the console — and true once the command is queued, while the
    encode and the disk write run afterwards behind the save barrier.
    So the API's own Boolean is the only acceptance signal, and
    `SaveCaptureComplete` (or the terminal `SaveFailed`) for THIS
    request id is the only durability signal; a fixed sleep proves
    neither, and a status left behind by an earlier save answers for
    the wrong request.

    Returns True only when this slot is on disk. Every reader of the
    slot — a later session, a fresh process — must start only after
    that, so a caller gates its dependent work on the result. A false
    return still permits the caller's `finally` shutdown: quitting the
    engine that failed to save is cleanup, not a dependent read.
    """
    accepted = send(port, f"return engine.saveWorld('{page}', '{slot}')").strip()
    if not check(f"{label}: engine.saveWorld('{page}', '{slot}') accepted",
                 accepted.lower() == "true",
                 f"returned {accepted!r}; the validation reason is logged in "
                 f"{_current_log[0]}"):
        return False
    request_id = capture_request_id(port, "return engine.getSaveStatus()")
    if not check(f"{label}: engine.getSaveStatus() reports a request id for "
                 f"'{slot}'",
                 request_id is not None,
                 f"no request id was ever observed for "
                 f"engine.saveWorld('{page}', '{slot}'); see {_current_log[0]}"):
        return False
    ok, status = wait_save_complete(port, request_id)
    if not check(f"{label}: save of '{slot}' (request {request_id}) reaches "
                 f"SaveCaptureComplete",
                 ok,
                 f"engine.saveWorld('{page}', '{slot}') request {request_id} "
                 f"ended at {status}"):
        return False
    print(f"    saved '{slot}' (request {request_id}, phase "
          f"{status.get('phase')})")
    return True



# --------------------------------------------------------------------------
# The facts the three offscreen sessions share
# --------------------------------------------------------------------------
class SessionContext(NamedTuple):
    """Everything the three sessions need that the run resolved once.

    Built by `location_embark_probe.run_probe` after phase 0 has picked
    a seed and named the two ruins, and threaded unchanged from one
    session to the next: the seed, the target/control identities, the
    expected location count, the framebuffer dimensions and the
    screenshot root are facts about THIS run, so no session may
    re-derive one and none of them can drift between (a), (b) and (c).
    """

    port: int
    w: int
    h: int
    shots: str
    target: dict
    control: dict
    seed: int
    expected_total: int

    @property
    def centre(self) -> tuple[int, int]:
        """The screen-centre pixel every real click and camera
        convergence in this probe resolves against. Derived here rather
        than in each session, so one framebuffer size cannot produce
        three answers."""
        return self.w // 2, self.h // 2
