#!/usr/bin/env python3
"""The executable preflight and the cross-process resource model (#2130).

Twenty groups over `probe_runner_resources`:

  the engine executable is resolved ONCE, before any probe (#1570) --
  the preflight precedes every parallel and every sequential probe, a
  failed preflight spawns nothing, an unusable resolved path is refused
  rather than ignored, the `--list` and refusal paths stay build-free,
  and the resolved path reaches every attempt including a solo retry;
  a nested runner adopts that executable without rebuilding, an
  ancestor's exclusive hold is not waited on, and a nested preflight does
  not deadlock against its ancestor;
  the preflight build excludes a foreign runner and waits for one;
  the hold environment names what a probe holds, and a probe is handed
  its runner's exclusive holds;
  no registered probe launches the engine through Cabal;
  the resource ledger is a reader/writer lock, and the cross-process half
  of it behaves against a FOREIGN holder in all four combinations.

The foreign-holder fixtures -- `FOREIGN_TRY_SRC`, `foreign_interest`,
`FOREIGN_HOLDER_SRC` and `ForeignHolder` -- are this family's own, as is
the source scan behind the Cabal-launch audit.
"""
from __future__ import annotations

import ast
import shutil
import subprocess
import sys
import tempfile
import textwrap
import threading
import time
import uuid
from pathlib import Path

from .support import (
    PreflightRecorder,
    TOOLS_DIR,
    Tree,
    clear_namespace,
    main_refusal,
    main_with_open,
    overlaps,
    patched,
    wait_file,
)

import probe_engine  # noqa: E402
import probe_resource_lock  # noqa: E402
import probe_runner_registry  # noqa: E402
import probe_runner_resources  # noqa: E402
from selftestlib import expect  # noqa: E402


# --------------------------------------------------------------------------
# The engine executable is resolved ONCE, before any probe (#1570)
#
# These drive the REAL `run_probes.main` against the synthetic tree, with
# only the preflight's subprocess entry point doubled — so the ordering,
# the call count, the refusal path and the environment handed to each
# probe are the shipped code's, not a restatement of it.
# --------------------------------------------------------------------------
def preflight_argvs(recorder: PreflightRecorder) -> list[list[str]]:
    return [list(argv) for argv in recorder.argvs]


def first_start(tree: Tree, name: str) -> float | None:
    """When this probe's FIRST attempt began, or None if it never ran."""
    windows = tree.intervals(name)
    return windows[0][0] if windows else None


def test_one_preflight_precedes_every_parallel_probe() -> None:
    print("\n-- a --jobs sweep makes ONE Cabal contact, before any probe starts")
    tree = Tree()
    try:
        for name in ("alpha", "beta", "gamma"):
            tree.add(name, exit_code=0)
        recorder = PreflightRecorder(tree.executable)
        with patched(tree, preflight=recorder):
            rc, out = main_with_open(
                tree, ["--only", "alpha,beta,gamma", "--exact", "--jobs", "3"])
        expect(rc == 0, f"every probe still passes (got {rc})\n{out}")
        expect(preflight_argvs(recorder) == [
                   ["cabal", "build", "exe:synarchy"],
                   ["cabal", "list-bin", "exe:synarchy"]],
               f"exactly one freshness build and one read-only query, in that "
               f"order (got {preflight_argvs(recorder)})")
        starts = [first_start(tree, name) for name in ("alpha", "beta", "gamma")]
        expect(all(when is not None for when in starts),
               f"all three probes really ran (starts: {starts})")
        expect(all(when is not None and recorder.finished_at <= when
                   for when in starts),
               f"and the preflight finished before the earliest of them "
               f"(preflight {recorder.finished_at}, starts {starts})")
    finally:
        tree.cleanup()


def test_one_preflight_precedes_every_sequential_probe() -> None:
    print("\n-- and a sequential sweep makes the same one contact, first")
    tree = Tree()
    try:
        tree.add("alpha", exit_code=0)
        tree.add("beta", exit_code=0)
        recorder = PreflightRecorder(tree.executable)
        with patched(tree, preflight=recorder):
            rc, out = main_with_open(tree, ["--only", "alpha,beta", "--exact"])
        expect(rc == 0, f"both probes still pass (got {rc})\n{out}")
        expect(len(recorder.calls) == 2,
               f"the sequential path preflights once too, not per probe "
               f"(calls: {preflight_argvs(recorder)})")
        starts = [first_start(tree, name) for name in ("alpha", "beta")]
        expect(all(when is not None and recorder.finished_at <= when
                   for when in starts),
               f"before either of them (preflight {recorder.finished_at}, "
               f"starts {starts})")
    finally:
        tree.cleanup()


def test_a_failed_preflight_spawns_nothing() -> None:
    print("\n-- a preflight that fails starts no probe, allocates no retry")
    for failing_step in ("build", "locate"):
        tree = Tree()
        try:
            tree.add("alpha", exit_code=0)
            recorder = PreflightRecorder(tree.executable, fail=failing_step,
                                          message="no such package")
            with patched(tree, preflight=recorder):
                rc, out = main_with_open(
                    tree, ["--only", "alpha", "--exact", "--retries", "2"])
            expect(rc == 2,
                   f"the {failing_step} step failing exits 2 (got {rc})")
            expect("cabal" in out and "no such package" in out,
                   f"and says which command failed, and why (got {out!r})")
            expect(not tree.started("alpha"),
                   "no probe process was spawned")
            expect(tree.intervals("alpha") == [],
                   f"so no attempt was recorded either "
                   f"(got {tree.intervals('alpha')})")
            expect("PASS" not in out and "FAIL" not in out,
                   f"and no probe verdict was reported (got {out!r})")
        finally:
            tree.cleanup()


def test_an_unusable_resolved_path_is_refused_not_ignored() -> None:
    print("\n-- an executable that cannot be run is a refusal, not a fallback")
    tree = Tree()
    try:
        tree.add("alpha", exit_code=0)
        missing = tree.root / "not-built-yet"
        recorder = PreflightRecorder(missing)
        with patched(tree, preflight=recorder):
            rc, out = main_with_open(tree, ["--only", "alpha", "--exact"])
        expect(rc == 2, f"a list-bin answer naming no file exits 2 (got {rc})")
        expect(str(missing) in out,
               f"and names the path it could not use (got {out!r})")
        expect(not tree.started("alpha"), "and no probe was spawned")
    finally:
        tree.cleanup()


def test_list_and_rejected_selections_stay_build_free() -> None:
    print("\n-- --list and a selection that runs nothing never reach Cabal")
    tree = Tree()
    try:
        tree.add("alpha", exit_code=0)
        for argv, why in (
                (["--list"], "--list"),
                (["--only", "nosuchprobe", "--exact"], "an all-invalid --exact"),
                (["--only", "alpha,nosuchprobe", "--exact"],
                 "a MIXED --exact selection"),
                (["--only", "nosuchsubstring"], "a substring matching nothing")):
            recorder = PreflightRecorder(tree.executable)
            with patched(tree, preflight=recorder):
                _rc, _out = main_with_open(tree, argv)
            expect(not recorder.calls,
                   f"{why} builds nothing (calls: {preflight_argvs(recorder)})")
        expect(not tree.started("alpha"),
               "and the mixed selection still ran no probe")
    finally:
        tree.cleanup()


def test_gui_port_refusal_still_precedes_the_build() -> None:
    print("\n-- a refused port plan is refused before anything is built")
    tree = Tree()
    try:
        tree.add("alpha", exit_code=0)
        recorder = PreflightRecorder(tree.executable)
        with patched(tree, preflight=recorder):
            rc, _out = main_refusal(
                tree, ["--only", "alpha", "--exact", "--port",
                       str(probe_runner_registry.GUI_PORT)])
        expect(rc != 0, f"the GUI port is still refused (got {rc})")
        expect(not recorder.calls,
               f"and nothing was built first "
               f"(calls: {preflight_argvs(recorder)})")
    finally:
        tree.cleanup()


def test_the_resolved_executable_reaches_every_attempt() -> None:
    print("\n-- every probe process is handed the one resolved executable")
    tree = Tree()
    try:
        tree.add("alpha", exit_code=0)
        # Fails once, so the parallel batch's SOLO retry is a second
        # attempt that must be handed the same executable.
        tree.add("flaky", exit_code=1)
        recorder = PreflightRecorder(tree.executable)
        with patched(tree, preflight=recorder):
            _rc, out = main_with_open(
                tree, ["--only", "alpha,flaky", "--exact", "--jobs", "2",
                       "--retries", "1"])
        want = str(tree.executable)
        expect(tree.engine_exes("alpha") == [want],
               f"the parallel attempt got it (got {tree.engine_exes('alpha')})")
        expect(tree.engine_exes("flaky") == [want, want],
               f"and so did BOTH the parallel attempt and its solo retry "
               f"(got {tree.engine_exes('flaky')})\n{out}")
        expect(len(recorder.calls) == 2,
               f"the retry built nothing further "
               f"(calls: {preflight_argvs(recorder)})")
    finally:
        tree.cleanup()

    tree = Tree()
    try:
        tree.add("solo", exit_code=1)
        recorder = PreflightRecorder(tree.executable)
        with patched(tree, preflight=recorder):
            _rc, _out = main_with_open(
                tree, ["--only", "solo", "--exact", "--retries", "2"])
        want = str(tree.executable)
        expect(tree.engine_exes("solo") == [want, want, want],
               f"the sequential path's inline retries got it too "
               f"(got {tree.engine_exes('solo')})")
        expect(len(recorder.calls) == 2,
               f"still one preflight (calls: {preflight_argvs(recorder)})")
    finally:
        tree.cleanup()


def test_a_nested_runner_adopts_the_executable_without_rebuilding() -> None:
    print("\n-- a nested runner reuses what its ancestor already resolved")
    tree = Tree()
    try:
        recorder = PreflightRecorder(tree.executable)
        with patched(tree, preflight=recorder):
            adopted = probe_runner_resources.engine_preflight(
                environ={probe_engine.ENV_ENGINE_EXE: str(tree.executable)})
        expect(adopted == str(tree.executable),
               f"the inherited executable is adopted verbatim (got {adopted})")
        expect(not recorder.calls,
               f"with no second build (calls: {preflight_argvs(recorder)})")

        recorder = PreflightRecorder(tree.executable)
        with patched(tree, preflight=recorder):
            resolved = probe_runner_resources.engine_preflight(environ={})
        expect(resolved == str(tree.executable),
               "and with nothing inherited it resolves one itself")
        expect(len(recorder.calls) == 2,
               f"through exactly one build and one query "
               f"(calls: {preflight_argvs(recorder)})")
    finally:
        tree.cleanup()


def test_an_ancestors_exclusive_hold_is_not_waited_on() -> None:
    print("\n-- a nested runner never waits on its own ancestor's hold")
    namespace = f"selftest{uuid.uuid4().hex[:12]}"
    env = {probe_runner_resources.ENV_HELD_NAMESPACE: namespace,
           probe_runner_resources.ENV_HELD_EXCLUSIVE: "cabal-build"}
    lock_exclusive, lock_shared = probe_runner_resources.cross_process_interests(
        "chop", namespace, env)
    expect("cabal-build" not in lock_shared,
           f"an inherited exclusive drops out of a shared request "
           f"(got {sorted(lock_shared)})")
    expect("repo-config" in lock_shared,
           f"while everything else is still requested "
           f"(got {sorted(lock_shared)})")
    nested_exclusive, _ = probe_runner_resources.cross_process_interests(
        "save_compat_migration", namespace, env)
    expect(not nested_exclusive,
           f"and out of an exclusive request too "
           f"(got {sorted(nested_exclusive)})")

    # The in-process ledger keeps the FULL declarations, so a nested sweep
    # still serializes its own probes against each other.
    expect("cabal-build" in probe_runner_resources.exclusive_resources(
               "save_compat_migration"),
           "the declaration itself is untouched")

    # A DIFFERENT namespace inherits nothing: a resource name means
    # nothing outside the repository its lock was taken in.
    foreign = dict(env, **{probe_runner_resources.ENV_HELD_NAMESPACE: "somewhere-else"})
    _fx, foreign_shared = probe_runner_resources.cross_process_interests(
        "chop", namespace, foreign)
    expect("cabal-build" in foreign_shared,
           f"a hold from another namespace is ignored "
           f"(got {sorted(foreign_shared)})")

    # And the hold really is grantable: take `cabal-build` exclusively
    # here, then acquire the nested request against the same namespace.
    ancestor = probe_resource_lock.acquire(
        exclusive={"cabal-build"}, namespace=namespace,
        purpose="selftest ancestor")
    try:
        try:
            nested = probe_resource_lock.acquire(
                exclusive=lock_exclusive, shared=lock_shared,
                namespace=namespace, purpose="selftest nested")
        except probe_resource_lock.ResourceBusy as busy:
            expect(False, f"the nested request still blocked on {busy.resource!r}")
        else:
            expect(True, "the nested request is granted under the ancestor's "
                         "exclusive hold")
            nested.release()
    finally:
        ancestor.release()
        clear_namespace(namespace)


FOREIGN_TRY_SRC = textwrap.dedent("""\
    # Try ONCE, without waiting, to take one resource in one interest.
    # Prints "busy" when a live holder refuses it and "free" when it is
    # granted (released again immediately). A SEPARATE process, because
    # an flock conflict between two open file descriptions is exactly
    # what the cross-process layer is made of, and asking from inside
    # the holding process would prove nothing about another runner.
    import sys
    sys.path.insert(0, sys.argv[1])
    import probe_resource_lock
    namespace, resource, interest = sys.argv[2], sys.argv[3], sys.argv[4]
    want = {resource}
    kwargs = ({"exclusive": want} if interest == "exclusive"
              else {"shared": want})
    try:
        hold = probe_resource_lock.acquire(namespace=namespace,
                                           purpose="selftest probe", **kwargs)
    except probe_resource_lock.ResourceBusy:
        print("busy")
    else:
        hold.release()
        print("free")
    """)


def foreign_interest(namespace: str, resource: str, interest: str) -> str:
    """"busy" or "free": whether ANOTHER process could take it right now."""
    with tempfile.TemporaryDirectory() as tmp:
        script = Path(tmp) / "try_acquire.py"
        script.write_text(FOREIGN_TRY_SRC)
        done = subprocess.run(
            [sys.executable, str(script), TOOLS_DIR, namespace, resource,
             interest],
            capture_output=True, text=True, timeout=60)
        return done.stdout.strip() or f"error: {done.stderr.strip()[:200]}"


def test_the_preflight_build_excludes_a_foreign_runner() -> None:
    print("\n-- while the preflight builds, no other runner is in the tree")
    tree = Tree()
    try:
        observed: list[str] = []
        namespace = f"selftest{uuid.uuid4().hex[:12]}"
        recorder = PreflightRecorder(tree.executable)

        def watching(argv, cwd=None, capture_output=False, text=False):
            # Asked from INSIDE the build, which is the only instant that
            # answers the question the concern is about.
            observed.append(foreign_interest(namespace,
                                              probe_runner_resources.BUILD_RESOURCE,
                                              "shared"))
            observed.append(foreign_interest(namespace,
                                              probe_runner_resources.BUILD_RESOURCE,
                                              "exclusive"))
            return recorder(argv, cwd=cwd, capture_output=capture_output,
                            text=text)

        tree.add("alpha", exit_code=0)
        with patched(tree, namespace=namespace, preflight=watching):
            rc, out = main_with_open(tree, ["--only", "alpha", "--exact"])
        expect(rc == 0, f"the sweep still passes (got {rc})\n{out}")
        expect(observed and all(answer == "busy" for answer in observed),
               f"every foreign interest in the build state was refused for "
               f"the whole preflight (got {observed})")
        expect(foreign_interest(namespace, probe_runner_resources.BUILD_RESOURCE,
                                 "exclusive") == "free",
               "and the hold is released once the preflight is done, so the "
               "sweep's own probes are never queued behind it")
    finally:
        tree.cleanup()
        clear_namespace(namespace)


def test_the_preflight_build_waits_for_a_foreign_runner() -> None:
    print("\n-- and it waits for a foreign holder rather than building beside it")
    tree = Tree()
    namespace = f"selftest{uuid.uuid4().hex[:12]}"
    holder = None
    try:
        recorder = PreflightRecorder(tree.executable)
        resolved: list[str] = []
        failed: list[BaseException] = []
        holder = ForeignHolder(namespace, "exclusive",
                               probe_runner_resources.BUILD_RESOURCE)
        expect(holder.wait_until_held(), "the foreign runner holds the "
                                          "build state exclusively")

        def resolve() -> None:
            try:
                with patched(tree, namespace=namespace, preflight=recorder):
                    resolved.append(probe_runner_resources.engine_preflight(namespace,
                                                                 environ={}))
            except BaseException as error:      # reported, never swallowed
                failed.append(error)

        worker = threading.Thread(target=resolve, daemon=True)
        worker.start()
        worker.join(timeout=4.0)
        expect(worker.is_alive(),
               "the preflight is still waiting, not building")
        expect(not recorder.calls,
               f"so no Cabal command ran beside the foreign holder "
               f"(calls: {preflight_argvs(recorder)})")
        holder.stop()
        holder = None
        worker.join(timeout=90.0)
        expect(not worker.is_alive(), "and it proceeds once that holder lets go")
        expect(not failed, f"without raising ({failed})")
        expect(resolved == [str(tree.executable)],
               f"resolving the executable it was going to resolve "
               f"(got {resolved})")
        expect(len(recorder.calls) == 2,
               f"through the same one build and one query "
               f"(calls: {preflight_argvs(recorder)})")
    finally:
        if holder is not None:
            holder.stop()
        tree.cleanup()
        clear_namespace(namespace)


def test_a_nested_preflight_does_not_wait_on_its_ancestor() -> None:
    print("\n-- a nested runner's preflight is inside its ancestor's hold")
    tree = Tree()
    namespace = f"selftest{uuid.uuid4().hex[:12]}"
    ancestor = None
    try:
        recorder = PreflightRecorder(tree.executable)
        ancestor = probe_resource_lock.acquire(
            exclusive={probe_runner_resources.BUILD_RESOURCE}, namespace=namespace,
            purpose="selftest ancestor")
        # The environment a nested runner is handed: no executable (so it
        # really does build), but its ancestor's exclusive hold declared.
        env = {probe_runner_resources.ENV_HELD_NAMESPACE: namespace,
               probe_runner_resources.ENV_HELD_EXCLUSIVE: probe_runner_resources.BUILD_RESOURCE}
        with patched(tree, namespace=namespace, preflight=recorder):
            resolved = probe_runner_resources.engine_preflight(namespace, environ=env)
        expect(resolved == str(tree.executable),
               f"it resolved without waiting on its ancestor (got {resolved})")
        expect(len(recorder.calls) == 2,
               f"having really built (calls: {preflight_argvs(recorder)})")
    finally:
        if ancestor is not None:
            ancestor.release()
        tree.cleanup()
        clear_namespace(namespace)


def test_the_hold_environment_names_what_a_probe_holds() -> None:
    print("\n-- a probe is told what its runner holds exclusively for it")
    namespace = "selftest-hold-env"
    env = probe_runner_resources.descendant_hold_env("save_compat_migration", namespace)
    expect(env.get(probe_runner_resources.ENV_HELD_EXCLUSIVE) == "cabal-build",
           f"an exclusive holder exports its resource (got {env!r})")
    expect(env.get(probe_runner_resources.ENV_HELD_NAMESPACE) == namespace,
           f"qualified by the namespace it was taken in (got {env!r})")
    expect(probe_runner_resources.descendant_hold_env("chop", namespace) == {},
           "a probe holding nothing exclusively exports nothing")
    expect(probe_runner_resources.descendant_hold_env("save_compat_migration", None) == {},
           "and without a namespace there is nothing to export")


def test_a_probe_is_handed_its_runners_exclusive_holds() -> None:
    print("\n-- and that environment really reaches the probe process")
    tree = Tree()
    try:
        # Named for a shipped EXCLUSIVE holder, so the real declaration is
        # what decides — the same trick the scheduling tests above use.
        tree.add("config_state", exit_code=0)
        with patched(tree) as fixture:
            rc, out = main_with_open(
                tree, ["--only", "config_state", "--exact"])
        expect(rc == 0, f"the probe passed (got {rc})\n{out}")
        lines = tree.env_lines("config_state")
        expect(len(lines) == 1, f"it ran once (got {lines})")
        if lines:
            _exe, held, held_ns = lines[0]
            expect(held == "repo-config",
                   f"and was told what its runner holds for it (got {held!r})")
            expect(held_ns == fixture.namespace,
                   f"in the runner's own namespace (got {held_ns!r})")
    finally:
        tree.cleanup()


def registered_probe_sources() -> dict[str, str]:
    """Every registered probe script's source text, keyed by probe key."""
    tools = Path(TOOLS_DIR)
    out = {}
    for key, script, _ in probe_runner_registry.PROBES:
        path = tools / script
        if path.is_file():
            out[key] = path.read_text(encoding="utf-8")
    return out


def cabal_run_launchers(source: str) -> list[str]:
    """Sequence literals in `source` that spell a `cabal run` launch.

    Structural, over the parsed tree rather than the text: a list or
    tuple whose first element is the string "cabal" and whose second is
    "run". That is exactly the engine launch #1570 removed from every
    probe, and it stays out of reach of a probe that merely MENTIONS
    cabal in prose or runs a different cabal subcommand behind the
    runner-supplied-executable check (`resource_root_probe.py`).
    """
    found = []
    for node in ast.walk(ast.parse(source)):
        if not isinstance(node, (ast.List, ast.Tuple)):
            continue
        head = [element.value for element in node.elts[:2]
                if isinstance(element, ast.Constant)
                and isinstance(element.value, str)]
        if head[:2] == ["cabal", "run"]:
            found.append(ast.unparse(node))
    return found


def test_no_registered_probe_spells_a_cabal_engine_launch() -> None:
    print("\n-- no registered probe launches its engine through `cabal run`")
    offenders = {key: launchers
                 for key, source in registered_probe_sources().items()
                 if (launchers := cabal_run_launchers(source))}
    expect(not offenders,
           f"every engine launch goes through probe_engine.engine_command "
           f"(offenders: {offenders})")
    for shared in ("probelib.py", "probe_engine.py"):
        source = (Path(TOOLS_DIR) / shared).read_text(encoding="utf-8")
        launchers = cabal_run_launchers(source)
        if shared == "probelib.py":
            expect(not launchers,
                   f"probelib no longer spells one either (got {launchers})")
        else:
            expect(len(launchers) == 1,
                   f"probe_engine owns the ONE remaining fallback spelling "
                   f"(got {launchers})")

    # Mutation: the guard has to FIRE on a reintroduced launcher, not
    # merely agree that today's tree is clean.
    reintroduced = ('cmd = ["cabal", "run", "-v0", "exe:synarchy", "--", '
                    '"--headless"]\n')
    expect(cabal_run_launchers(reintroduced),
           "a reintroduced `cabal run` launcher is caught")
    expect(not cabal_run_launchers('r = ["cabal", "list-bin", "exe:synarchy"]\n'),
           "while a non-launching cabal subcommand is not mistaken for one")
    expect(not cabal_run_launchers('note = "run this with cabal run"\n'),
           "and neither is prose that merely mentions it")


def test_resource_ledger_is_a_reader_writer_lock() -> None:
    print("\n-- the ledger grants many readers at once and a writer only alone")
    ledger = probe_runner_resources.ResourceLedger()
    shared, exclusive = {"repo-config"}, {"repo-config"}
    expect(ledger.idle(), "a fresh ledger holds nothing")
    expect(not ledger.blocked(exclusive, set()),
           "so an exclusive interest is grantable")

    # Three readers, then a writer: the shared side must COUNT, not merely
    # remember that someone held it, or the writer starts after the first
    # release with two engines still up.
    for _ in range(3):
        expect(not ledger.blocked(set(), shared),
               "another reader may join while readers hold it")
        ledger.acquire(set(), shared)
    for held in (2, 1):
        expect(ledger.blocked(exclusive, set()),
               f"a writer is blocked while {held + 1} reader(s) hold it")
        ledger.release(set(), shared)
    expect(ledger.blocked(exclusive, set()),
           "still blocked with the last reader holding it")
    ledger.release(set(), shared)
    expect(ledger.idle() and not ledger.blocked(exclusive, set()),
           "and grantable only once every reader has released")

    # And the mirror: a writer excludes readers and writers alike.
    ledger.acquire(exclusive, set())
    expect(ledger.blocked(set(), shared),
           "a reader is blocked while a writer holds it")
    expect(ledger.blocked(exclusive, set()),
           "and so is a second writer")
    ledger.release(exclusive, set())
    expect(ledger.idle(), "releasing the writer empties the ledger")

    # A probe that declares nothing shared is never blocked by anything,
    # which is what keeps an unrelated resource out of this decision.
    ledger.acquire(exclusive, set())
    expect(not ledger.blocked(set(), {"some-other-resource"}),
           "an interest in a different resource is unaffected")
    ledger.release(exclusive, set())


# --------------------------------------------------------------------------
# Exact selection: unknown keys are rejected rather than silently dropped
# (#1321)
# --------------------------------------------------------------------------
# --------------------------------------------------------------------------
# The cross-process half of the same reader/writer model (#1436)
# --------------------------------------------------------------------------
FOREIGN_HOLDER_SRC = textwrap.dedent("""\
    # A separate PROCESS holding one probe resource, because that is the
    # thing the in-process ledger cannot see. Signals readiness by
    # creating a file, then holds until a release file appears.
    import sys, time
    from pathlib import Path
    sys.path.insert(0, sys.argv[1])
    import probe_resource_lock as lock
    namespace, interest, resource, ready, release = sys.argv[2:7]
    kwargs = ({"exclusive": {resource}} if interest == "exclusive"
              else {"shared": {resource}})
    hold = lock.acquire(namespace=namespace, purpose="foreign holder", **kwargs)
    Path(ready).write_text("held")
    deadline = time.time() + 120
    while not Path(release).exists() and time.time() < deadline:
        time.sleep(0.05)
    hold.release()
""")


class ForeignHolder:
    """A real second process holding one resource for the duration."""

    def __init__(self, namespace: str, interest: str,
                 resource: str = "repo-config") -> None:
        self.dir = Path(tempfile.mkdtemp(prefix="foreign_holder_"))
        script = self.dir / "holder.py"
        script.write_text(FOREIGN_HOLDER_SRC)
        self.ready = self.dir / "ready"
        self.release_flag = self.dir / "release"
        self.proc = subprocess.Popen(
            [sys.executable, str(script), TOOLS_DIR, namespace, interest,
             resource, str(self.ready), str(self.release_flag)])

    def wait_until_held(self, seconds: float = 30.0) -> bool:
        return wait_file(self.ready, seconds)

    def stop(self) -> None:
        try:
            self.release_flag.write_text("go")
        except OSError:
            pass
        try:
            self.proc.wait(timeout=30)
        except subprocess.TimeoutExpired:
            self.proc.kill()
            self.proc.wait(timeout=10)
        shutil.rmtree(self.dir, ignore_errors=True)


def test_a_foreign_exclusive_holder_makes_the_sweep_wait() -> None:
    print("\n-- a foreign EXCLUSIVE holder stalls every probe without "
          "crashing the scheduler")
    tree = Tree()
    namespace = f"selftest{uuid.uuid4().hex[:12]}"
    holder = ForeignHolder(namespace, "exclusive")
    seen: dict = {}
    try:
        seen["took_lock"] = holder.wait_until_held()
        # Every registered probe holds `repo-config` SHARED, so ONE foreign
        # exclusive holder conflicts with the whole roster. Before #1436 the
        # scheduler's "nothing running, work pending" guard raised
        # RuntimeError here and took the sweep down.
        tree.add("unrelated_a", dwell=0.2, descendant=False)
        tree.add("unrelated_b", dwell=0.2, descendant=False)
        saved_poll = probe_runner_resources.RESOURCE_WAIT_POLL
        probe_runner_resources.RESOURCE_WAIT_POLL = 0.2
        result: dict = {}

        def sweep() -> None:
            with patched(tree, namespace=namespace):
                result["rc"], result["out"] = main_with_open(tree, ["--jobs", "2"])

        thread = threading.Thread(target=sweep, daemon=True)
        thread.start()
        # Nothing is asserted while the sweep runs: it has redirected
        # stdout, so a message printed here would land in its buffer.
        # Observations are recorded and judged after the join.
        time.sleep(2.0)
        seen["still_waiting"] = thread.is_alive()
        seen["nothing_started"] = (not tree.started("unrelated_a")
                                   and not tree.started("unrelated_b"))
        holder.stop()
        thread.join(timeout=90)
        probe_runner_resources.RESOURCE_WAIT_POLL = saved_poll

        expect(seen["took_lock"], "the foreign process took the lock")
        expect(seen["still_waiting"],
               "the sweep is still waiting rather than having crashed or "
               "finished")
        expect(seen["nothing_started"],
               "and no probe started while the foreign holder was in the way")
        expect(not thread.is_alive(), "the sweep finishes once the lock frees")
        expect(result.get("rc") == 0,
               f"and every probe then passes (exit {result.get('rc')})")
        expect("waiting for 'repo-config'" in (result.get("out") or "")
               and "exclusive" in (result.get("out") or ""),
               "the runner said WHICH resource it was waiting on and in which "
               "interest")
        for name in ("unrelated_a", "unrelated_b"):
            got = tree.intervals(name)
            expect(len(got) == 1 and got[0][1] is not None,
                   f"{name} ran exactly once, after the wait (windows: {got})")
    finally:
        holder.stop()
        clear_namespace(namespace)
        tree.cleanup()


def test_waiting_for_a_foreign_holder_is_not_charged_to_the_probe() -> None:
    print("\n-- a queued probe's elapsed time and timeout cover execution "
          "only, never the wait")
    tree = Tree()
    namespace = f"selftest{uuid.uuid4().hex[:12]}"
    holder = ForeignHolder(namespace, "exclusive")
    seen: dict = {}
    try:
        seen["took_lock"] = holder.wait_until_held()
        tree.add("unrelated_a", dwell=0.2, descendant=False)
        saved_poll = probe_runner_resources.RESOURCE_WAIT_POLL
        probe_runner_resources.RESOURCE_WAIT_POLL = 0.2
        result: dict = {}

        def sweep() -> None:
            with patched(tree, namespace=namespace):
                # SEQUENTIAL, and with a timeout far shorter than the wait
                # below: if the wait were inside the probe's own clock this
                # would be reported TIMEOUT instead of PASS.
                result["rc"], result["out"] = main_with_open(
                    tree, ["--jobs", "1", "--timeout", "5"])

        thread = threading.Thread(target=sweep, daemon=True)
        thread.start()
        time.sleep(8.0)
        seen["still_waiting"] = thread.is_alive()
        holder.stop()
        thread.join(timeout=90)
        probe_runner_resources.RESOURCE_WAIT_POLL = saved_poll
        out = result.get("out") or ""

        expect(seen["took_lock"], "the foreign process took the lock")
        expect(seen["still_waiting"], "the sweep waited rather than running")
        expect(result.get("rc") == 0,
               f"the probe passes after an 8s wait against a 5s timeout "
               f"(exit {result.get('rc')})")
        expect("TIMEOUT" not in out,
               "and the wait is never reported as a TIMEOUT")
        window = tree.window("unrelated_a")
        expect(window[1] is not None and (window[1] - window[0]) < 5.0,
               f"the probe's own occupancy window is its execution alone "
               f"({window})")
    finally:
        holder.stop()
        clear_namespace(namespace)
        tree.cleanup()


def test_a_foreign_shared_holder_never_blocks_a_shared_probe() -> None:
    print("\n-- a foreign SHARED holder does not serialize ordinary probes")
    tree = Tree()
    namespace = f"selftest{uuid.uuid4().hex[:12]}"
    holder = ForeignHolder(namespace, "shared")
    try:
        took = holder.wait_until_held()
        tree.add("unrelated_a", dwell=0.8, descendant=False)
        tree.add("unrelated_b", dwell=0.8, descendant=False)
        with patched(tree, namespace=namespace):
            rc, out = main_with_open(tree, ["--jobs", "2"])
        expect(took, "the foreign process took the lock")
        expect(rc == 0, f"both probes pass beside the shared holder (exit {rc})")
        first, second = tree.window("unrelated_a"), tree.window("unrelated_b")
        expect(overlaps(first, second),
               f"and they still run concurrently: shared holders coexist "
               f"(unrelated_a {first}, unrelated_b {second})")
        expect("waiting for" not in out, "nothing waited on anything")
    finally:
        holder.stop()
        clear_namespace(namespace)
        tree.cleanup()


def test_a_run_probes_exclusive_probe_blocks_a_foreign_shared_acquirer() -> None:
    print("\n-- the conflict is detected in the other direction too: a "
          "run_probes exclusive probe blocks a foreign shared acquirer")
    tree = Tree()
    namespace = f"selftest{uuid.uuid4().hex[:12]}"
    seen: dict = {}
    try:
        tree.add("config_state", dwell=3.0, descendant=False)
        result: dict = {}

        def sweep() -> None:
            with patched(tree, namespace=namespace):
                result["rc"], result["out"] = main_with_open(tree, ["--jobs", "1"])

        thread = threading.Thread(target=sweep, daemon=True)
        thread.start()
        seen["started"] = wait_file(tree.root / "config_state.started", 60.0)
        # While it runs, a /deflake-shaped acquirer must be refused even for
        # a SHARED interest -- the direction the in-process ledger could
        # never enforce, because the acquirer is not in its process.
        try:
            spurious = probe_resource_lock.acquire(
                shared={"repo-config"}, namespace=namespace,
                purpose="foreign shared acquirer")
        except probe_resource_lock.ResourceBusy as busy:
            seen["busy"] = busy
        else:
            spurious.release()
        thread.join(timeout=120)

        expect(seen.get("started") is True, "the exclusive probe started")
        expect("busy" in seen,
               "a foreign SHARED acquirer is refused while run_probes holds "
               "the resource exclusively")
        busy = seen.get("busy")
        if busy is not None:
            expect(busy.resource == "repo-config" and busy.interest == "shared",
                   f"and the refusal names the resource and the interest "
                   f"({busy.resource!r}, {busy.interest})")
            expect(any(holder.get("interest") == "exclusive"
                       for holder in busy.holders),
                   f"and reports the exclusive holder ({busy.holders})")
        expect(result.get("rc") == 0,
               f"the sweep itself is unaffected (exit {result.get('rc')})")
        # And once it is over, the same acquisition succeeds.
        after = probe_resource_lock.acquire(shared={"repo-config"},
                                            namespace=namespace)
        after.release()
        expect(True, "the same acquisition succeeds once the probe is done")
    finally:
        clear_namespace(namespace)
        tree.cleanup()


#: The one-time executable preflight and the build-free paths beside it.
TESTS_PREFLIGHT = (
    test_one_preflight_precedes_every_parallel_probe,
    test_one_preflight_precedes_every_sequential_probe,
    test_a_failed_preflight_spawns_nothing,
    test_an_unusable_resolved_path_is_refused_not_ignored,
    test_list_and_rejected_selections_stay_build_free,
    test_gui_port_refusal_still_precedes_the_build,
    test_the_resolved_executable_reaches_every_attempt,
)

#: Propagation of the resolved executable, inherited holds, and the
#: reader/writer ledger those holds are taken in.
TESTS_PROPAGATION_AND_HOLDS = (
    test_a_nested_runner_adopts_the_executable_without_rebuilding,
    test_an_ancestors_exclusive_hold_is_not_waited_on,
    test_the_preflight_build_excludes_a_foreign_runner,
    test_the_preflight_build_waits_for_a_foreign_runner,
    test_a_nested_preflight_does_not_wait_on_its_ancestor,
    test_the_hold_environment_names_what_a_probe_holds,
    test_a_probe_is_handed_its_runners_exclusive_holds,
    test_no_registered_probe_spells_a_cabal_engine_launch,
    test_resource_ledger_is_a_reader_writer_lock,
)

#: The cross-process half: a holder this runner did not start.
TESTS_FOREIGN_HOLDERS = (
    test_a_foreign_exclusive_holder_makes_the_sweep_wait,
    test_waiting_for_a_foreign_holder_is_not_charged_to_the_probe,
    test_a_foreign_shared_holder_never_blocks_a_shared_probe,
    test_a_run_probes_exclusive_probe_blocks_a_foreign_shared_acquirer,
)

#: This family's complete ordered inventory: its fragments, in the order
#: the aggregate runs them, which is also the order `--family resources`
#: runs them in.
TESTS = (TESTS_PREFLIGHT + TESTS_PROPAGATION_AND_HOLDS
         + TESTS_FOREIGN_HOLDERS)
