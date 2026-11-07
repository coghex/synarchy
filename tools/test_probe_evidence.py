#!/usr/bin/env python3
"""Self-test for the failing-probe evidence rule (#2931).

`python3 tools/test_probe_evidence.py` covers `tools/probe_evidence.py` and
its two consumers without booting an engine, a world or a real probe:

  * the helper itself -- allocation, the PASS/FAIL rule, pruning to the
    newest N, marker-gated and symlink-safe deletion, the worktree refusal,
    the environment overrides, archival and removal failures, and the
    probe-side `retire`/`discard_scratch`/`release_tree` that defer to a
    runner;
  * the legacy `tools/run_probes.py` path through synthetic probes -- a
    failing run keeps its directory at the durable root and prints the
    path, a passing one leaves nothing, a TIMEOUT is kept, a failed attempt
    survives a retry that passes, parallel attempts never collide, the path
    is printed whatever `--tail` says, and the engine log a probe booted at
    a fixed path is kept;
  * the four probes that carry `--keep-artifacts` -- they release through
    the one shared rule rather than their own copies of it.

The `probe_flake` half (default root, precedence, per-invocation pruning,
harness-error runs, the fixed-path engine log) lives with that harness's own
suite, `tools/probe_flake_tests/harness_artifacts.py`.
"""
from __future__ import annotations

import inspect
import os
import shutil
import sys
import tempfile
import threading
import textwrap
from pathlib import Path

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
import probe_evidence  # noqa: E402
import probe_runner_lifecycle  # noqa: E402
import selftestlib  # noqa: E402
from probe_runner_tests.support import (  # noqa: E402
    Tree, main_with, main_with_open, patched)
from selftestlib import FAILURES, expect  # noqa: E402

REPO = Path(__file__).resolve().parents[1]

# A synthetic registered probe that does what a real one's `finally` does:
# make a scratch tree holding a "save", boot an "engine" whose log has a
# fixed path, record that log the way `probelib.boot` does, then clean the
# tree up. `behave` is a file next to the probe: the first line is the exit
# code for attempt one, an optional second line the code for later attempts.
SCRATCH_PROBE = textwrap.dedent('''\
    import argparse, os, sys, tempfile, time
    sys.path.insert(0, {tools!r})
    import probe_evidence
    ap = argparse.ArgumentParser()
    ap.add_argument('--port', type=int, default=0)
    ap.parse_args()
    counter = {counter!r}
    attempt = 1
    if os.path.exists(counter):
        attempt = int(open(counter).read()) + 1
    open(counter, 'w').write(str(attempt))
    codes = open({behave!r}).read().split()
    code = codes[min(attempt, len(codes)) - 1]
    scratch = tempfile.mkdtemp(prefix='scratch_')
    open(os.path.join(scratch, 'evidence.txt'), 'w').write('save %d\\n' % attempt)
    ownedfile = {owned!r}
    if os.path.exists(ownedfile):
        owned = open(ownedfile).read().strip()
        os.makedirs(owned, exist_ok=True)
        probe_evidence.register_owned_path(owned)
        open(os.path.join(owned, 'world.synworld'), 'w').write('the save\\n')
    fixed = {fixed!r}
    open(fixed, 'w').write('engine output %d\\n' % attempt)
    probe_evidence.record_engine_log(fixed)
    print('all good' if code in ('0', 'strict') else 'FAIL: a check failed')
    if code == 'hang':
        time.sleep(600)
    if code == 'strict':
        probe_evidence.retire(scratch)      # a cleanup contract
        sys.exit(0)
    probe_evidence.discard_scratch(scratch)
    sys.exit(0 if code == '0' else 1)
''')


def add_scratch_probe(tree: Tree, name: str, behave: str) -> Path:
    """Register a synthetic probe; return the fixed engine-log path it uses."""
    fixed = tree.root / f"{name}.fixed-engine.log"
    behave_file = tree.root / f"{name}.behave"
    behave_file.write_text(behave, encoding="utf-8")
    script = f"{name}_probe.py"
    (tree.root / "tools" / script).write_text(SCRATCH_PROBE.format(
        tools=str(Path(__file__).resolve().parent),
        counter=str(tree.root / f"{name}.counter"),
        behave=str(behave_file), fixed=str(fixed),
        owned=str(tree.root / f"{name}.owned")), encoding="utf-8")
    tree.probes.append((name, script, f"synthetic {name}"))
    return fixed


def attempts(tree: Tree, name: str) -> list[Path]:
    probe_dir = tree.evidence_root / name
    return sorted(p for p in probe_dir.iterdir()) if probe_dir.is_dir() else []


def test_a_failing_run_keeps_its_directory_and_prints_the_path() -> None:
    print("\n-- a failing run keeps its evidence and names it")
    tree = Tree()
    try:
        fixed = add_scratch_probe(tree, "bad", "1")
        rc, out = main_with(tree, ["--only", "bad", "--exact"])
        kept = attempts(tree, "bad")
        expect(rc == 1 and len(kept) == 1, f"one failed attempt is kept ({kept})")
        if not kept:
            return
        where = kept[0]
        expect(str(where.resolve()) in out and "evidence kept:" in out,
               "its path is printed with the failure")
        expect(f"bad_probe.py: {where.resolve()}" in out and "FAILURE EVIDENCE" in out,
               "and again in the final summary")
        expect(where.parent == tree.evidence_root.resolve() / "bad"
               or where.parent == tree.evidence_root / "bad",
               "under <durable root>/<probe>/")
        expect((where / probe_evidence.MARKER).is_file(),
               "marked finalized")
        scratch = sorted((where / probe_evidence.RETIRED_DIR).glob("scratch_*"))
        expect(len(scratch) == 1
               and (scratch[0] / "evidence.txt").read_text() == "save 1\n",
               "the probe's own scratch tree survived its own cleanup")
        logs = sorted((where / probe_evidence.ENGINE_LOG_DIR).glob("*"))
        expect(len(logs) == 1 and logs[0].read_text() == "engine output 1\n",
               "the engine log at a fixed path was copied in")
        fixed.unlink()
        expect(logs[0].exists(), "and survives the original being removed")
        expect("FAIL: a check failed" in
               (where / probe_evidence.CAPTURE_FILE).read_text(),
               "the attempt's complete captured output is saved with it")
    finally:
        tree.cleanup()
        shutil.rmtree(tree.root, ignore_errors=True)


def test_a_passing_run_leaves_nothing_behind() -> None:
    print("\n-- a passing run leaves nothing behind")
    tree = Tree()
    try:
        add_scratch_probe(tree, "good", "0")
        rc, out = main_with(tree, ["--only", "good", "--exact"])
        expect(rc == 0 and attempts(tree, "good") == [],
               f"no attempt directory remains ({attempts(tree, 'good')})")
        expect("evidence kept" not in out and "FAILURE EVIDENCE" not in out,
               "and nothing is announced")
        leftovers = list(tree.evidence_root.rglob("scratch_*"))
        expect(leftovers == [], "the probe's scratch went with the attempt")
    finally:
        tree.cleanup()
        shutil.rmtree(tree.root, ignore_errors=True)


def test_a_timeout_keeps_what_the_killed_probe_left() -> None:
    print("\n-- a TIMEOUT keeps the tree the killed probe never cleaned")
    tree = Tree()
    try:
        add_scratch_probe(tree, "slow", "hang")
        rc, out = main_with(tree, ["--only", "slow", "--exact",
                                   "--timeout", "3"])
        kept = attempts(tree, "slow")
        expect(rc == 1 and "TIMEOUT" in out and len(kept) == 1,
               f"the timed-out attempt is kept ({kept}, rc {rc})")
        if kept:
            scratch = sorted((kept[0] / "tmp").glob("scratch_*"))
            expect(len(scratch) == 1
                   and (scratch[0] / "evidence.txt").exists(),
                   "the scratch tree sits in the attempt's TMPDIR, because "
                   "the probe was killed before it could clean up")
            expect(kept[0].resolve().name in out, "and the path is printed")
    finally:
        tree.cleanup()
        shutil.rmtree(tree.root, ignore_errors=True)


def test_a_failed_attempt_outlives_a_retry_that_passes() -> None:
    print("\n-- each attempt is its own evidence, retry or not")
    tree = Tree()
    try:
        add_scratch_probe(tree, "flaky", "1 0")
        rc, out = main_with(tree, ["--only", "flaky", "--exact",
                                   "--retries", "1"])
        kept = attempts(tree, "flaky")
        expect(rc == 0 and "passed on retry" in out,
               f"the retry passed (rc {rc})")
        expect(len(kept) == 1, f"the FAILED first attempt is kept ({kept})")
        if kept:
            expect(str(kept[0].resolve()) in out, "its path is printed")
            expect(list((kept[0] / probe_evidence.RETIRED_DIR)
                        .glob("scratch_*/evidence.txt"))[0].read_text()
                   == "save 1\n",
                   "and holds the FIRST attempt's data, not the retry's")
            expect("FAIL: a check failed" in
                   (kept[0] / probe_evidence.CAPTURE_FILE).read_text(),
                   "and the FIRST attempt's output, which the retry's "
                   "success would otherwise have replaced")
        expect("FAILURE EVIDENCE" in out,
               "the summary lists it although the probe passed")
    finally:
        tree.cleanup()
        shutil.rmtree(tree.root, ignore_errors=True)


def test_the_path_is_printed_whatever_tail_says() -> None:
    print("\n-- --tail 0 does not hide where the evidence is")
    tree = Tree()
    try:
        add_scratch_probe(tree, "bad", "1")
        rc, out = main_with(tree, ["--only", "bad", "--exact", "--tail", "0"])
        kept = attempts(tree, "bad")
        expect(rc == 1 and kept and str(kept[0].resolve()) in out
               and "FAIL: a check failed" not in out,
               "the tail is suppressed and the path is not")
        expect(kept and "FAIL: a check failed" in
               (kept[0] / probe_evidence.CAPTURE_FILE).read_text(),
               "and the output the tail would have shown is saved anyway")
    finally:
        tree.cleanup()
        shutil.rmtree(tree.root, ignore_errors=True)


def test_parallel_attempts_never_collide() -> None:
    print("\n-- --jobs keeps one directory per failing attempt")
    tree = Tree()
    try:
        for name in ("pa", "pb", "pc"):
            add_scratch_probe(tree, name, "1")
        add_scratch_probe(tree, "ok", "0")
        rc, out = main_with(tree, ["--jobs", "3"])
        for name in ("pa", "pb", "pc"):
            kept = attempts(tree, name)
            expect(len(kept) == 1 and str(kept[0].resolve()) in out,
                   f"{name} kept and printed its own attempt ({kept})")
            expect(kept and (kept[0] / probe_evidence.CAPTURE_FILE).is_file(),
                   f"{name}'s parallel attempt saved its output")
        expect(attempts(tree, "ok") == [], "the passing probe kept nothing")
        expect(rc == 1, "and the run still fails")
    finally:
        tree.cleanup()
        shutil.rmtree(tree.root, ignore_errors=True)


def test_a_root_inside_a_worktree_is_refused() -> None:
    print("\n-- a root inside a worktree is refused before any probe runs")
    inside = REPO / "probe-evidence-selftest-root"
    try:
        probe_evidence.Retention.from_environment(
            {probe_evidence.ENV_ROOT: str(inside)})
        expect(False, "the helper refused a root inside this worktree")
    except probe_evidence.RootRejected as error:
        expect("inside the working tree" in str(error),
               f"the helper names the worktree ({error})")
    expect(not inside.exists(), "and created nothing there")
    tree = Tree()
    try:
        add_scratch_probe(tree, "bad", "1")
        with patched(tree):
            os.environ[probe_evidence.ENV_ROOT] = str(inside)
            rc, out = main_with_open(tree, ["--only", "bad", "--exact"])
        expect(rc == 2 and "inside the working tree" in out
               and probe_evidence.ENV_ROOT in out,
               f"run_probes refuses with the documented usage code ({rc})")
        expect(not (tree.root / "bad.counter").exists() and not inside.exists(),
               "before any probe started")
    finally:
        tree.cleanup()
        shutil.rmtree(tree.root, ignore_errors=True)
        shutil.rmtree(inside, ignore_errors=True)


def test_a_failed_removal_after_strict_cleanup_fails_the_run() -> None:
    print("\n-- strict cleanup the runner cannot finish fails a passing run")
    tree = Tree()
    real = shutil.rmtree
    try:
        add_scratch_probe(tree, "strict", "strict")
        add_scratch_probe(tree, "lenient", "0")

        def refuse(path, *args, **kwargs):
            if "failure-evidence" in str(path) and "20" in Path(path).name:
                raise OSError("synthetic: directory busy")
            return real(path, *args, **kwargs)

        shutil.rmtree = refuse
        try:
            rc_strict, out_strict = main_with(
                tree, ["--only", "strict", "--exact"])
            rc_lenient, out_lenient = main_with(
                tree, ["--only", "lenient", "--exact"])
        finally:
            shutil.rmtree = real
        expect(rc_strict == 1 and "retired scratch could not be removed"
               in out_strict and "busy" in out_strict,
               f"retire() followed by an undeletable attempt fails the run "
               f"({rc_strict})")
        expect(len(attempts(tree, "strict")) == 1
               and str(attempts(tree, "strict")[0].resolve()) in out_strict,
               "and the leftover is kept and named")
        strict_dir = attempts(tree, "strict")[0]
        expect((strict_dir / probe_evidence.MARKER).is_file(),
               "finalized like any failure, so it can be pruned later")
        expect("retired scratch could not be removed" in
               (strict_dir / probe_evidence.CAPTURE_FILE).read_text()
               and "all good" in
               (strict_dir / probe_evidence.CAPTURE_FILE).read_text(),
               "with the probe's captured output and the cleanup diagnostic")
        expect(len(list((strict_dir / probe_evidence.ENGINE_LOG_DIR)
                        .glob("*"))) == 1,
               "and the engine log it booted at a fixed path")
        expect(rc_lenient == 0,
               "a lenient discard_scratch leftover does not fail a pass, as "
               "before")
    finally:
        shutil.rmtree = real
        tree.cleanup()
        shutil.rmtree(tree.root, ignore_errors=True)


def test_saves_outside_the_attempt_are_copied_before_removal() -> None:
    print("\n-- repo-relative saves are kept as evidence, then removed")
    import tutorial_probe  # noqa: E402
    saved = os.environ.pop(probe_evidence.ENV_EVIDENCE_DIR, None)
    saved_root = tutorial_probe.REPO_ROOT
    try:
        with tempfile.TemporaryDirectory(prefix="evidence-saves-") as temp:
            temp = Path(temp).resolve()
            attempt = temp / "attempt"
            attempt.mkdir()
            fake_repo = temp / "repo"
            slot = fake_repo / "saves" / tutorial_probe.SLOT
            slot.mkdir(parents=True)
            (slot / "world.synworld").write_text("the save")
            tutorial_probe.REPO_ROOT = str(fake_repo)

            tutorial_probe.remove_probe_slot()
            expect(not slot.exists(),
                   "run directly, the slot is simply removed")

            slot.mkdir(parents=True)
            (slot / "world.synworld").write_text("the save")
            os.environ[probe_evidence.ENV_EVIDENCE_DIR] = str(attempt)
            tutorial_probe.remove_probe_slot()
            kept = sorted((attempt / probe_evidence.RETIRED_DIR).glob(
                f"{tutorial_probe.SLOT}-*/world.synworld"))
            expect(not slot.exists() and len(kept) == 1
                   and kept[0].read_text() == "the save",
                   "under a runner the probe's real cleanup path leaves the "
                   "slot gone from saves/ but copies it into the attempt")

            other = fake_repo / "saves" / "other"
            other.mkdir()
            (other / "f").write_text("x")
            probe_evidence.retire_saves(other)
            expect(not other.exists() and len(list(
                (attempt / probe_evidence.RETIRED_DIR).glob("other-*"))) == 1,
                "and so do the lenient removers (save_pause, multiworld_save, "
                "lua_orphan_prune, pause_speed)")
    finally:
        tutorial_probe.REPO_ROOT = saved_root
        os.environ.pop(probe_evidence.ENV_EVIDENCE_DIR, None)
        if saved is not None:
            os.environ[probe_evidence.ENV_EVIDENCE_DIR] = saved


def test_a_timeout_keeps_checkout_saves_the_probe_registered() -> None:
    print("\n-- run_probes: a killed probe's registered saves are archived")
    tree = Tree()
    try:
        add_scratch_probe(tree, "killed", "hang")
        owned = tree.root / "checkout-saves" / "probe_slot"
        (tree.root / "killed.owned").write_text(str(owned))
        rc, out = main_with(tree, ["--only", "killed", "--exact",
                                   "--timeout", "3"])
        kept = attempts(tree, "killed")
        copies = (sorted((kept[0] / probe_evidence.OWNED_DIR)
                         .glob("*/world.synworld")) if kept else [])
        expect(rc == 1 and len(copies) == 1
               and copies[0].read_text() == "the save\n",
               f"the save the killed probe never cleaned up was copied in "
               f"({kept})")
        expect((owned / "world.synworld").exists(),
               "and the original stays where the probe made it")
    finally:
        tree.cleanup()
        shutil.rmtree(tree.root, ignore_errors=True)


def test_the_persistence_sweep_s_real_cleanup_keeps_its_tree() -> None:
    print("\n-- persistence_contract_sweep.main's cleanup paths keep the tree")
    import persistence_contract_sweep as sweep  # noqa: E402
    saved_env = os.environ.pop(probe_evidence.ENV_EVIDENCE_DIR, None)
    saved_tempdir = tempfile.tempdir
    real_root = sweep.make_isolated_root
    try:
        for label, extra in (("uninstrumented", []), ("instrumented", ["dest"])):
            with tempfile.TemporaryDirectory(prefix="evidence-sweep-") as temp:
                temp = Path(temp).resolve()
                attempt = temp / "attempt"
                (attempt / "tmp").mkdir(parents=True)
                os.environ[probe_evidence.ENV_EVIDENCE_DIR] = str(attempt)
                tempfile.tempdir = str(attempt / "tmp")

                def boom(base):
                    Path(base, "world.synworld").write_text("the save")
                    Path(base, "engineA.log").write_text("engine log")
                    raise RuntimeError("synthetic failure inside the sweep")

                sweep.make_isolated_root = boom
                argv = ["--skip-cross-probes"]
                if extra:
                    argv += ["--keep-generations", str(temp / "dest")]
                saved_argv = sys.argv
                sys.argv = ["persistence_contract_sweep.py", *argv]
                try:
                    try:
                        sweep.main()
                    except RuntimeError:
                        pass
                finally:
                    sys.argv = saved_argv
                    tempfile.tempdir = saved_tempdir
                saves = list((attempt / probe_evidence.RETIRED_DIR).glob(
                    "persistence_contract_sweep_*/world.synworld"))
                logs = list((attempt / probe_evidence.RETIRED_DIR).glob(
                    "persistence_contract_sweep_*/engineA.log"))
                expect(len(saves) == 1 and len(logs) == 1
                       and saves[0].read_text() == "the save",
                       f"{label}: the real finally path retired the tree "
                       f"with its save and engine log inside")
    finally:
        sweep.make_isolated_root = real_root
        tempfile.tempdir = saved_tempdir
        os.environ.pop(probe_evidence.ENV_EVIDENCE_DIR, None)
        if saved_env is not None:
            os.environ[probe_evidence.ENV_EVIDENCE_DIR] = saved_env


def test_a_failed_save_copy_leaves_the_original_in_place() -> None:
    print("\n-- retire_saves never destroys the only copy of a save")
    saved = os.environ.pop(probe_evidence.ENV_EVIDENCE_DIR, None)
    real = shutil.copytree
    try:
        with tempfile.TemporaryDirectory(prefix="evidence-copyfail-") as temp:
            temp = Path(temp).resolve()
            attempt = temp / "attempt"
            attempt.mkdir()
            slot = temp / "saves" / "slot"
            slot.mkdir(parents=True)
            (slot / "world.synworld").write_text("the save")
            os.environ[probe_evidence.ENV_EVIDENCE_DIR] = str(attempt)

            def refuse(*args, **kwargs):
                raise OSError("synthetic: no space left")

            shutil.copytree = refuse
            import contextlib
            import io
            seen = io.StringIO()
            try:
                with contextlib.redirect_stderr(seen):
                    probe_evidence.retire_saves(slot)
                lenient_raised = False
            except OSError:
                lenient_raised = True
            expect(not lenient_raised and (slot / "world.synworld").exists(),
                   "a lenient remover keeps the original when it cannot copy")
            expect(str(slot) in seen.getvalue()
                   and "left in place" in seen.getvalue()
                   and "no space left" in seen.getvalue(),
                   f"and names where it is and why ({seen.getvalue()!r})")
            try:
                probe_evidence.retire_saves(slot, strict=True)
                strict_raised = False
            except OSError as error:
                strict_raised = "left in place" in str(error)
            expect(strict_raised and (slot / "world.synworld").exists(),
                   "a strict remover fails, since its cleanup did not happen")
    finally:
        shutil.copytree = real
        os.environ.pop(probe_evidence.ENV_EVIDENCE_DIR, None)
        if saved is not None:
            os.environ[probe_evidence.ENV_EVIDENCE_DIR] = saved


def test_the_expedition_cleanup_helper_retires_the_tree() -> None:
    print("\n-- expedition_loop's nested remove_root keeps the tree under a runner")
    from expedition_loop import harness  # noqa: E402
    saved = os.environ.pop(probe_evidence.ENV_EVIDENCE_DIR, None)
    try:
        with tempfile.TemporaryDirectory(prefix="evidence-expedition-") as temp:
            temp = Path(temp).resolve()
            attempt = temp / "attempt"
            (attempt / "tmp").mkdir(parents=True)
            base = attempt / "tmp" / "synarchy_expedition_loop_x"
            (base / "saves").mkdir(parents=True)
            (base / "saves" / "world.synworld").write_text("the save")
            os.environ[probe_evidence.ENV_EVIDENCE_DIR] = str(attempt)
            harness.remove_root(str(base), False)
            kept = list((attempt / probe_evidence.RETIRED_DIR).glob(
                "synarchy_expedition_loop_x-*/saves/world.synworld"))
            expect(not base.exists() and len(kept) == 1
                   and kept[0].read_text() == "the save",
                   "the real helper leaves the save in the attempt")
    finally:
        os.environ.pop(probe_evidence.ENV_EVIDENCE_DIR, None)
        if saved is not None:
            os.environ[probe_evidence.ENV_EVIDENCE_DIR] = saved


def test_directly_launched_engine_logs_survive_finalization() -> None:
    print("\n-- engines launched without probelib.boot are still kept")
    import debug_console_boot_probe as dcb  # noqa: E402
    saved = os.environ.pop(probe_evidence.ENV_EVIDENCE_DIR, None)
    real = dcb.probe_engine.engine_command
    try:
        with tempfile.TemporaryDirectory(prefix="evidence-launch-") as temp:
            temp = Path(temp).resolve()
            retention = probe_evidence.Retention(temp / "root", 5, trees=[])
            retention.root.mkdir()
            attempt = retention.begin("debug_console_boot")
            os.environ[probe_evidence.ENV_EVIDENCE_DIR] = str(attempt.path)
            port = 9941
            dcb.probe_engine.engine_command = lambda args: [
                sys.executable, "-c",
                "import sys; print('launch stdout line'); "
                "sys.stderr.write('launch stderr line\\n')"]
            outputs = []
            for call in (lambda: dcb.check_successful_bind(port),
                         lambda: dcb.observe_widget_modules(port, "", "stub")):
                import contextlib
                import io
                with contextlib.redirect_stdout(io.StringIO()):
                    call()
            listed = (attempt.path / probe_evidence.ENGINE_LOG_MANIFEST
                      ).read_text().split()
            expect(len(listed) == 4 and all(p.startswith("/tmp/") for p in listed),
                   f"both launchers listed both of their streams ({listed})")
            attempt.finish("FAIL")
            kept = sorted((attempt.path / probe_evidence.ENGINE_LOG_DIR
                           ).glob("*"))
            texts = [p.read_text() for p in kept]
            expect(len(kept) == 4
                   and sum("launch stdout line" in t for t in texts) == 2
                   and sum("launch stderr line" in t for t in texts) == 2,
                   "and a failure keeps all four with their contents")
            for path in listed:
                with contextlib.suppress(OSError):
                    os.unlink(path)
    finally:
        dcb.probe_engine.engine_command = real
        os.environ.pop(probe_evidence.ENV_EVIDENCE_DIR, None)
        if saved is not None:
            os.environ[probe_evidence.ENV_EVIDENCE_DIR] = saved
    for name in ("resource_root", "scenario_entrypoint"):
        text = (Path(__file__).parent / f"{name}_probe.py").read_text()
        expect("probe_evidence.record_engine_log(" in text,
               f"{name}_probe.py lists the log of the engine it launches itself")


def test_a_symlinked_probe_directory_cannot_reach_a_worktree() -> None:
    print("\n-- <root>/<probe> may not be a way into a checkout")
    target = REPO / "probe-evidence-link-target"
    with tempfile.TemporaryDirectory(prefix="evidence-link-") as temp:
        root = Path(temp).resolve() / "root"
        retention = probe_evidence.Retention.from_environment(
            {probe_evidence.ENV_ROOT: str(root)})
        target.mkdir(exist_ok=True)
        try:
            (root / "crop").symlink_to(target)
            (root / "wander").symlink_to(Path(temp).resolve())
            for probe, why in (("crop", "inside the working tree"),
                               ("wander", "outside the root")):
                try:
                    retention.check_probes([probe])
                    expect(False, f"{probe} should be refused")
                except probe_evidence.RootRejected as error:
                    expect(why in str(error), f"{probe}: {error}")
                try:
                    retention.begin(probe)
                    expect(False, f"{probe} must not allocate")
                except probe_evidence.RootRejected:
                    pass
            expect(list(target.iterdir()) == [],
                   "nothing was created inside the checkout")

            tree = Tree()
            try:
                add_scratch_probe(tree, "crop", "1")
                with patched(tree):
                    os.environ[probe_evidence.ENV_ROOT] = str(root)
                    rc, out = main_with_open(
                        tree, ["--only", "crop", "--exact"])
                expect(rc == 2 and "inside the working tree" in out
                       and not (tree.root / "crop.counter").exists(),
                       f"run_probes refuses before any probe starts ({rc})")
            finally:
                tree.cleanup()
                shutil.rmtree(tree.root, ignore_errors=True)
        finally:
            shutil.rmtree(target, ignore_errors=True)


def test_a_bad_keep_count_is_refused_not_defaulted() -> None:
    print("\n-- SYNARCHY_PROBE_FAILURE_KEEP is validated")
    for raw in ("0", "-2", "many"):
        try:
            probe_evidence.keep_count({probe_evidence.ENV_KEEP: raw})
            expect(False, f"{raw!r} should be refused")
        except probe_evidence.RootRejected as error:
            expect(probe_evidence.ENV_KEEP in str(error),
                   f"{raw!r} is refused by name")
    expect(probe_evidence.keep_count({}) == probe_evidence.DEFAULT_KEEP == 5,
           "the default is the newest five")
    expect(probe_evidence.keep_count({probe_evidence.ENV_KEEP: "2"}) == 2,
           "and a good override applies")
    expect(probe_evidence.configured_root({}) == Path.home() / ".local"
           / "state" / "synarchy" / "probe-failures",
           "the default root is ~/.local/state/synarchy/probe-failures")
    expect(probe_evidence.configured_root(
        {probe_evidence.ENV_ROOT: "/elsewhere"}) == Path("/elsewhere"),
        "and the variable overrides it")


def test_pruning_keeps_the_newest_n_and_only_what_it_owns() -> None:
    print("\n-- pruning keeps the newest N, and only marked real directories")
    with tempfile.TemporaryDirectory(prefix="evidence-prune-") as temp:
        retention = probe_evidence.Retention(Path(temp).resolve() / "root", 3)
        retention.root.mkdir()
        names = []
        for index in range(6):
            attempt = retention.begin("p")
            (attempt.path / "log").write_text(str(index))
            expect(attempt.finish("FAIL") == attempt.path,
                   "a failure is kept")
            names.append(attempt.path.name)
        left = sorted(c.name for c in (retention.root / "p").iterdir())
        expect(left == sorted(names[-3:]),
               f"exactly the newest 3 remain ({left} vs {names[-3:]})")

        active = retention.begin("p")                  # not finalized
        outside = Path(temp) / "outside"
        outside.mkdir()
        (outside / probe_evidence.MARKER).write_text(
            '{"schema": "probe-evidence/v1", "finalized_at": "0"}\n')
        (outside / "precious").write_text("do not delete")
        link = retention.root / "p" / "a-link"
        link.symlink_to(outside)
        for _ in range(4):
            attempt = retention.begin("p")
            attempt.finish("FAIL")
        expect(active.path.is_dir(),
               "an active (unmarked) attempt is never pruned")
        expect((outside / "precious").exists() and link.is_symlink(),
               "a symlink is skipped, its target untouched")
        other = retention.begin("q")
        other.finish("FAIL")
        expect(len(list((retention.root / "q").iterdir())) == 1,
               "another probe's evidence is its own budget")


def test_pruning_counts_failed_runs_inside_an_invocation() -> None:
    print("\n-- one invocation of many failed runs cannot exceed the limit")
    with tempfile.TemporaryDirectory(prefix="evidence-runs-") as temp:
        probe_dir = Path(temp).resolve() / "root" / "p"
        invocations = []
        for batch in range(3):
            invocation = probe_dir / f"p-batch{batch}"
            for index in range(1, 5):
                (invocation / f"run-{index:03d}").mkdir(parents=True)
            probe_evidence.finalize_invocation(
                invocation, probe="p", retained=True, keep=3)
            invocations.append(invocation)
        left = sorted(str(p.relative_to(probe_dir))
                      for p in probe_dir.glob("*/run-*"))
        expect(len(left) == 3 and all(x.startswith("p-batch2") for x in left),
               f"only the newest three failed runs remain ({left})")
        expect(not invocations[0].exists() and not invocations[1].exists(),
               "emptied invocations are removed, not left as clutter")


def test_scratch_cleanup_failures_still_propagate() -> None:
    print("\n-- temporary_directory keeps TemporaryDirectory's strictness")
    real = shutil.rmtree

    def refuse(path, *args, **kwargs):
        raise OSError("synthetic: busy")

    saved = os.environ.pop(probe_evidence.ENV_EVIDENCE_DIR, None)
    try:
        shutil.rmtree = refuse
        raised = False
        try:
            with probe_evidence.temporary_directory(prefix="strict_"):
                pass
        except OSError:
            raised = True
        finally:
            shutil.rmtree = real
        expect(raised, "a failed cleanup raises, so the probe cannot exit "
               "successfully with scratch left behind")
        with probe_evidence.temporary_directory(prefix="gone_") as made:
            real(made)
        expect(True, "a tree that already vanished is not an error")
        expect("probe_evidence.retire(temporary)" in
               (Path(__file__).parent / "audio_null_probe.py").read_text(),
               "audio_null's strict cleanup still raises")
    finally:
        shutil.rmtree = real
        if saved is not None:
            os.environ[probe_evidence.ENV_EVIDENCE_DIR] = saved


def test_invocation_owned_trees_are_never_deleted_directly() -> None:
    print("\n-- no migrated probe deletes an invocation-owned tree itself")
    import re
    migrated = ("autosave", "chop", "construction", "crop", "farm_ai",
                "foraging", "gameplay_tag_persistence", "item_instance",
                "item_temp", "location_overlay", "location_stamp_idempotent",
                "persistence_contract", "persistence_integrity", "plant",
                "power", "save_barrier", "save_storage", "scenario_entrypoint",
                "till", "transactional_load", "transfer_order",
                "unified_transfer", "fluid_exact_restart",
                "fluid_exact_restart_render", "fluid_reaction",
                "fluid_reaction_visual", "debug_console_boot", "audio_null",
                "save_pause", "multiworld_save", "lua_orphan_prune",
                "pause_speed", "tutorial", "tutorial_hud",
                "expedition_loop", "persistence_contract_sweep")
    call = re.compile(r"^[^#\n]*\bshutil\.rmtree\(", re.M)
    for name in migrated:
        script = (f"{name}.py" if name == "persistence_contract_sweep"
                  else f"{name}_probe.py")
        text = (Path(__file__).parent / script).read_text()
        expect(call.search(text) is None,
               f"{script} has no direct shutil.rmtree call: its "
               f"teardown retires trees through probe_evidence, so the saves "
               f"and logs inside them survive a failure")


def test_concurrent_allocation_and_pruning() -> None:
    print("\n-- concurrent attempts get distinct directories; pruning is safe")
    with tempfile.TemporaryDirectory(prefix="evidence-race-") as temp:
        retention = probe_evidence.Retention(Path(temp).resolve() / "root", 4)
        retention.root.mkdir()
        errors, made = [], []

        def worker():
            try:
                for _ in range(6):
                    attempt = retention.begin("race")
                    made.append(attempt.path)
                    attempt.finish("FAIL")
            except Exception as error:  # noqa: BLE001 - reported below
                errors.append(repr(error))

        threads = [threading.Thread(target=worker) for _ in range(8)]
        for thread in threads:
            thread.start()
        for thread in threads:
            thread.join()
        expect(errors == [], f"no worker raised ({errors})")
        expect(len(set(made)) == 48, "every one of 48 attempts is distinct")
        expect(len(list((retention.root / "race").iterdir())) == 4,
               "and the probe ends with exactly the newest 4")


def test_archival_and_removal_failures_are_reported_not_swallowed() -> None:
    print("\n-- failures to archive or remove keep the evidence and say where")
    with tempfile.TemporaryDirectory(prefix="evidence-fail-") as temp:
        retention = probe_evidence.Retention(Path(temp).resolve() / "root", 5)
        retention.root.mkdir()
        log = Path(temp) / "engine.log"
        log.write_text("original")
        attempt = retention.begin("a")
        (attempt.path / probe_evidence.ENGINE_LOG_MANIFEST).write_text(
            str(log) + "\n")
        (attempt.path / probe_evidence.ENGINE_LOG_DIR).write_text("in the way")
        said = []
        kept = attempt.finish("FAIL", announce=said.append)
        expect(kept == attempt.path and attempt.path.is_dir(),
               "a failed archive still keeps the attempt")
        expect(any(str(log) in line and "still at" in line for line in said),
               f"and names where the original still is ({said})")
        expect(log.read_text() == "original", "the original is untouched")
        expect((attempt.path / probe_evidence.MARKER).exists(),
               "and the attempt is still marked finalized")

        attempt = retention.begin("b")
        real = shutil.rmtree
        said = []

        def refuse(path, *args, **kwargs):
            raise OSError("synthetic: directory busy")

        shutil.rmtree = refuse
        try:
            left = attempt.finish("PASS", announce=said.append)
        finally:
            shutil.rmtree = real
        expect(left == attempt.path and attempt.path.exists(),
               "a passing run whose removal fails returns the leftover path")
        expect(any(str(attempt.path) in line and "busy" in line
                   for line in said),
               f"and reports the path and the error ({said})")


def test_probe_side_helpers_defer_to_a_runner() -> None:
    print("\n-- retire / discard_scratch / release_tree")
    names = (probe_evidence.ENV_EVIDENCE_DIR,)
    saved = {name: os.environ.pop(name, None) for name in names}
    try:
        with tempfile.TemporaryDirectory(prefix="evidence-probe-") as temp:
            temp = Path(temp).resolve()
            lone = temp / "lone"
            lone.mkdir()
            (lone / "f").write_text("x")
            probe_evidence.discard_scratch(lone)
            expect(not lone.exists(), "outside a runner it is rmtree")

            attempt = temp / "attempt"
            (attempt / "tmp").mkdir(parents=True)
            os.environ[probe_evidence.ENV_EVIDENCE_DIR] = str(attempt)
            owned = attempt / "tmp" / "owned"
            owned.mkdir()
            (owned / "save").write_text("data")
            elsewhere = temp / "elsewhere"
            elsewhere.mkdir()
            probe_evidence.discard_scratch(owned)
            retired = list((attempt / probe_evidence.RETIRED_DIR).iterdir())
            expect(not owned.exists() and len(retired) == 1
                   and (retired[0] / "save").read_text() == "data",
                   "under a runner a tree inside the attempt is retired, "
                   "so the path is gone to the probe but the data is kept")
            probe_evidence.discard_scratch(elsewhere)
            expect(not elsewhere.exists(),
                   "a tree OUTSIDE the attempt (a stale export) is still the "
                   "probe's own to remove")
            expect(not probe_evidence.is_runner_owned(temp / "other"),
                   "ownership is containment, not the variable alone")
            try:
                probe_evidence.retire(attempt)
                expect(False, "the attempt itself must not be retired")
            except OSError:
                expect(attempt.is_dir(), "the evidence directory itself is "
                       "never retired")
            with probe_evidence.temporary_directory(prefix="td_") as made:
                inside = made
                (Path(made) / "x").write_text("y")
            expect(not os.path.exists(inside), "temporary_directory retires")

            # release_tree: the shared core of the four probes' copies.
            base = attempt / "tmp" / "run"
            (base / "logs").mkdir(parents=True)
            said = []
            message = probe_evidence.release_tree(
                str(base), keep=True, describe=lambda say: say("  engine log"))
            expect(message is None and base.is_dir(),
                   "--keep-artifacts retains the tree on any outcome")
            os.environ.pop(probe_evidence.ENV_EVIDENCE_DIR)
            message = probe_evidence.release_tree(str(base))
            expect(message is None and not base.exists(),
                   "without the flag the tree is removed")
            base.mkdir()
            real = shutil.rmtree
            shutil.rmtree = lambda *a, **k: (_ for _ in ()).throw(
                OSError("busy"))
            try:
                message = probe_evidence.release_tree(str(base))
            finally:
                shutil.rmtree = real
            expect(message is not None and "could not remove" in message
                   and "busy" in message,
                   "a removal that fails is a reported failure")
            shutil.rmtree = lambda *a, **k: None
            try:
                message = probe_evidence.release_tree(
                    str(base), survived="still {base}")
            finally:
                shutil.rmtree = real
            expect(message == f"still {base}",
                   "and a tree that survives is another, in the probe's words")
    finally:
        for name, value in saved.items():
            if value is None:
                os.environ.pop(name, None)
            else:
                os.environ[name] = value


def test_the_child_environment_is_the_runners_alone() -> None:
    print("\n-- run_one hands the evidence directory over, and only then")
    tree = Tree()
    try:
        script = tree.root / "tools" / "env_probe.py"
        script.write_text(textwrap.dedent('''\
            import os, sys
            print("EVIDENCE=" + os.environ.get("SYNARCHY_PROBE_EVIDENCE_DIR", ""))
            print("TMPDIR=" + os.environ.get("TMPDIR", ""))
            '''), encoding="utf-8")
        os.environ[probe_evidence.ENV_EVIDENCE_DIR] = "/stale/export"
        try:
            with patched(tree):
                _ok, _t, _e, out = probe_runner_lifecycle.run_one(
                    "env_probe.py", None, 30)
                held = tree.root / "held"
                _ok, _t, _e, with_dir = probe_runner_lifecycle.run_one(
                    "env_probe.py", None, 30, evidence_dir=str(held))
        finally:
            os.environ.pop(probe_evidence.ENV_EVIDENCE_DIR, None)
        expect("EVIDENCE=\n" in out,
               "a stale inherited export never decides where evidence goes")
        expect(f"EVIDENCE={held}\n" in with_dir
               and f"TMPDIR={held}/tmp\n" in with_dir
               and (held / "tmp").is_dir(),
               "an evidence directory sets both the variable and TMPDIR")
        expect(f"TMPDIR={held}" not in out,
               "and without one TMPDIR is left alone")
    finally:
        tree.cleanup()
        shutil.rmtree(tree.root, ignore_errors=True)


def test_record_engine_log_only_acts_under_a_runner() -> None:
    print("\n-- record_engine_log")
    saved = os.environ.pop(probe_evidence.ENV_EVIDENCE_DIR, None)
    try:
        with tempfile.TemporaryDirectory(prefix="evidence-log-") as temp:
            probe_evidence.record_engine_log("/tmp/a.log")
            expect(list(Path(temp).iterdir()) == [],
                   "outside a runner it writes nothing")
            os.environ[probe_evidence.ENV_EVIDENCE_DIR] = temp
            probe_evidence.record_engine_log("/tmp/a.log")
            probe_evidence.record_engine_log("/tmp/b.log")
            listed = (Path(temp) / probe_evidence.ENGINE_LOG_MANIFEST
                      ).read_text().split()
            expect(listed == ["/tmp/a.log", "/tmp/b.log"],
                   f"each boot appends its log ({listed})")
    finally:
        os.environ.pop(probe_evidence.ENV_EVIDENCE_DIR, None)
        if saved is not None:
            os.environ[probe_evidence.ENV_EVIDENCE_DIR] = saved


def test_the_four_keep_artifacts_probes_share_one_rule() -> None:
    print("\n-- flora_growth, flora_condition, location_embark and "
          "location_content release through the shared rule")
    import flora_condition_probe  # noqa: E402
    import flora_growth_probe  # noqa: E402
    from location_content import invocation as content  # noqa: E402
    from location_embark import invocation as embark  # noqa: E402
    expect(flora_condition_probe.release_artifacts
           is flora_growth_probe.release_artifacts,
           "flora_condition still imports flora_growth's function, not a copy")
    for label, function in (("flora_growth", flora_growth_probe.release_artifacts),
                            ("location_embark", embark.release_artifacts),
                            ("location_content", content.release_artifacts)):
        source = inspect.getsource(function)
        expect("probe_evidence.release_tree" in source
               and "shutil.rmtree" not in source,
               f"{label}.release_artifacts delegates to release_tree and "
               f"keeps no removal logic of its own")
        expect(list(inspect.signature(function).parameters) == ["art", "keep"],
               f"{label}.release_artifacts keeps its signature")
    expect("probe_evidence.retire" in inspect.getsource(
        content.remove_isolated_root),
        "and the isolated-root remover the other location probes borrow "
        "retires too")


TESTS = (
    test_a_failing_run_keeps_its_directory_and_prints_the_path,
    test_a_passing_run_leaves_nothing_behind,
    test_a_timeout_keeps_what_the_killed_probe_left,
    test_a_failed_attempt_outlives_a_retry_that_passes,
    test_the_path_is_printed_whatever_tail_says,
    test_parallel_attempts_never_collide,
    test_a_root_inside_a_worktree_is_refused,
    test_a_failed_removal_after_strict_cleanup_fails_the_run,
    test_saves_outside_the_attempt_are_copied_before_removal,
    test_a_timeout_keeps_checkout_saves_the_probe_registered,
    test_the_persistence_sweep_s_real_cleanup_keeps_its_tree,
    test_a_failed_save_copy_leaves_the_original_in_place,
    test_the_expedition_cleanup_helper_retires_the_tree,
    test_directly_launched_engine_logs_survive_finalization,
    test_a_symlinked_probe_directory_cannot_reach_a_worktree,
    test_a_bad_keep_count_is_refused_not_defaulted,
    test_pruning_keeps_the_newest_n_and_only_what_it_owns,
    test_pruning_counts_failed_runs_inside_an_invocation,
    test_concurrent_allocation_and_pruning,
    test_scratch_cleanup_failures_still_propagate,
    test_invocation_owned_trees_are_never_deleted_directly,
    test_archival_and_removal_failures_are_reported_not_swallowed,
    test_probe_side_helpers_defer_to_a_runner,
    test_the_child_environment_is_the_runners_alone,
    test_record_engine_log_only_acts_under_a_runner,
    test_the_four_keep_artifacts_probes_share_one_rule,
)


def main() -> int:
    selftestlib.parse_verbose()
    for test in TESTS:
        test()
    if FAILURES:
        print(f"\n{len(FAILURES)} check(s) failed:")
        for failure in FAILURES:
            print(f"  {failure}")
        return selftestlib.concluded(1)
    return selftestlib.concluded(
        0, f"\nAll {len(TESTS)} failure-evidence test groups passed")


if __name__ == "__main__":
    raise SystemExit(main())
