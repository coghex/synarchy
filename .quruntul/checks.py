"""Checks that .quruntul/adapter.py still describes Synarchy's tests faithfully.

Run with `python3 .quruntul/checks.py`. A stub context stands in for quruntul's
(same `Suite`, `Prepared`, `digest` surface), so these need neither quruntul nor
an engine: they compare the adapter with the probe registry, the CI classifier
and the protocol list it reads, and exercise its result and seed readers on
fixed documents. Only the protocol probes' `--describe` paths run, and those
never boot anything.
"""
from __future__ import annotations

import ast
import contextlib
from dataclasses import dataclass, field
import hashlib
import importlib.util
import json
import os
from pathlib import Path
import subprocess
import sys
import tempfile
import unittest

ROOT = Path(__file__).resolve().parents[1]
sys.dont_write_bytecode = True
sys.path.insert(0, str(ROOT / "tools"))

import ci_probes  # noqa: E402
import probe_flake  # noqa: E402
import probe_runner_registry  # noqa: E402


@dataclass
class Suite:
    id: str
    kind: str
    framework: str
    description: str
    area: str = ""
    platforms: list = field(default_factory=lambda: ["Darwin", "Linux"])
    desktop: bool = False
    trial_seconds: int = 900
    batch_seconds: int = 7200
    identity: str = ""
    rts: list = field(default_factory=list)
    checks: list = field(default_factory=list)
    priority: int = 10
    batch_tests: int = 0
    data: dict = field(default_factory=dict)


@dataclass
class Prepared:
    argv: list
    cwd: str
    environment: dict
    provenance: dict = field(default_factory=dict)
    wrapper: list = field(default_factory=list)
    launches_executable: bool = True


class Context:
    Suite = Suite
    Prepared = Prepared

    def __init__(self):
        self.checkout = ROOT
        self.revision = subprocess.run(["git", "rev-parse", "HEAD"], cwd=ROOT, capture_output=True, text=True,
                                       check=True).stdout.strip()
        self.platform = "Darwin"
        self.calls = []

    def run(self, argv, name, timeout, cwd=None, environment=None):
        """Stands in for quruntul's guardian; a preflight names two executables."""
        self.calls.append(dict(argv=argv, name=name, timeout=timeout))
        log = Path(tempfile.mkdtemp()) / f"{name}.log"
        log.write_text("QURUNTUL_EXECUTABLES " + json.dumps(
            {"SYNARCHY_PROBE_ENGINE_EXE": "/bin/engine", "SYNARCHY_SAVE_CODEC_EXE": "/bin/codec"}) + "\n")
        return dict(outcome="passed", log=str(log), command=argv)

    @staticmethod
    def digest(value):
        return hashlib.sha256(json.dumps(value, sort_keys=True, separators=(",", ":")).encode()).hexdigest()


def load():
    spec = importlib.util.spec_from_file_location("synarchy_quruntul_adapter", ROOT / ".quruntul" / "adapter.py")
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module


class AdapterChecks(unittest.TestCase):
    @classmethod
    def setUpClass(cls):
        cls.module = load()
        cls.adapter = cls.module.adapter()
        cls.suites = {s.id: s for s in cls.adapter.suites(Context())}
        cls.census = cls.module._census(ROOT)

    def probes(self):
        return {s.data["key"]: s for s in self.suites.values() if "key" in s.data}

    def test_every_registered_probe_is_one_suite_unless_deferred_or_direct_only(self):
        deferred = {k for k, p in self.census.items() if (p.get("census") or {}).get("deferred")}
        expected = ({key for key, _, _ in probe_runner_registry.PROBES} - deferred
                    - set(self.module.DIRECT_ONLY) - set(self.module.OPT_IN_ONLY))
        self.assertEqual(set(self.probes()), expected)
        self.assertEqual(len(self.suites), len(set(self.suites)))

    def test_expedition_loop_is_registered_but_opt_in_only(self):
        # #2934: the lab never offers it, yet it stays registered with its long timeout.
        key = "expedition_loop"
        self.assertIn(key, self.module.OPT_IN_ONLY)
        self.assertNotIn(key, self.probes())
        self.assertIn(key, {name for name, _, _ in probe_runner_registry.PROBES})
        self.assertEqual(probe_runner_registry.effective_timeout(key), 3600)

    def test_retired_migration_key_is_unknown_before_engine_launch(self):
        key = "save_compat_migration"
        self.assertNotIn(key, self.probes())
        self.assertNotIn(key, {name for name, _, _ in probe_runner_registry.PROBES})
        completed = subprocess.run(
            [sys.executable, str(ROOT / "tools" / "run_probes.py"),
             "--only", key, "--exact", "--jobs", "1", "--port", "9324"],
            cwd=ROOT, capture_output=True, text=True, check=False, timeout=30)
        self.assertEqual(completed.returncode, 2)
        self.assertIn("matched no probes", completed.stderr)
        self.assertNotIn("READY", completed.stdout + completed.stderr)

    def test_classification_follows_the_ci_classifier(self):
        for key, suite in self.probes().items():
            self.assertEqual(suite.kind, "ci" if key in ci_probes.CI_ELIGIBLE else "probe", key)

    def test_tracked_census_classification_follows_the_ci_classifier(self):
        # The TRACKED census (not the docs-wip copy the adapter prefers) must
        # agree with ci_probes, since no other gate compares the two (#2809).
        tracked = json.loads((ROOT / "docs" / "probe_census.json").read_text())
        for row in tracked["probes"]:
            want = "ci-eligible" if row["key"] in ci_probes.CI_ELIGIBLE else "manual-only"
            self.assertEqual(row["classification"], want, row["key"])

    def test_simulation_probes_are_local_probe_suites(self):
        # #2809: the three owner-policy simulations are manual-only, so the
        # lab offers each as a local `probe` suite run through run_probes.py
        # (unless the census defers it), never as a `ci` suite.
        self.assertEqual(ci_probes.SIMULATION_ONLY_KEYS, {"fluid_exact_restart", "infection", "medic_coord"})
        for key in sorted(ci_probes.SIMULATION_ONLY_KEYS):
            self.assertNotIn(key, ci_probes.CI_ELIGIBLE, key)
            suite = self.probes().get(key)
            if suite is None:
                self.assertTrue((self.census.get(key, {}).get("census") or {}).get("deferred"), key)
                continue
            self.assertEqual((suite.kind, suite.framework), ("probe", "exit"), key)
            prepared = self.adapter.prepare(Context(), suite)
            self.assertEqual(prepared.argv[1:5], ["tools/run_probes.py", "--only", key, "--exact"], key)

    def test_protocol_probes_measure_their_declared_checks(self):
        for key, suite in self.probes().items():
            if key in probe_flake.PROTOCOL_PROBES and key not in ci_probes.CI_ELIGIBLE:
                self.assertEqual(suite.framework, "command", key)
                self.assertTrue(suite.checks, key)
            elif key in ci_probes.gpu_probe_keys() and key not in ci_probes.CI_ELIGIBLE:
                # #2932: only a command suite can record "the GPU wait ran out" as blocked.
                self.assertEqual((suite.framework, suite.checks, suite.data.get("run_probes")),
                                 ("command", [self.module.RUN_CHECK], True), key)
            else:
                self.assertEqual(suite.framework, "exit", key)

    def test_hspec_suites_are_sliced_and_the_graphical_one_is_a_desktop_probe(self):
        headless, graphical = self.suites["synarchy-test-headless"], self.suites["synarchy-test-graphical"]
        self.assertEqual((headless.kind, headless.desktop, headless.batch_tests), ("ci", False, 400))
        self.assertEqual((graphical.kind, graphical.desktop), ("probe", True))

    def test_preview_is_the_only_desktop_probe_even_with_hidden_windows(self):
        # Hidden GLFW preview windows still require the lab's desktop claim.
        for key, suite in self.probes().items():
            self.assertEqual(suite.desktop, key == "preview", key)
        self.assertTrue(self.probes()["preview"].desktop)

    def test_launches_go_through_synarchys_runners(self):
        for key, suite in self.probes().items():
            prepared = self.adapter.prepare(Context(), suite)
            if suite.data.get("run_probes"):
                # #2932: the bounded, blocked-not-failed GPU wait wraps the same runner command.
                self.assertTrue(prepared.argv[1].endswith("probe_trial.py"), key)
                self.assertEqual(prepared.argv[2:4], [key, "--run-probes"], key)
                self.assertEqual(prepared.argv[4], str(suite.data["port"]), key)
                self.assertNotEqual(int(prepared.argv[-1]), 8008)
            elif suite.framework == "exit":
                self.assertEqual(prepared.argv[1:7], ["tools/run_probes.py", "--only", key, "--exact", "--jobs", "1"])
                self.assertNotEqual(int(prepared.argv[-1]), 8008)
            else:
                self.assertTrue(prepared.argv[1].endswith("probe_trial.py"))

    def test_outcomes_read_a_probe_flake_result(self):
        with tempfile.TemporaryDirectory() as temp:
            prefix = str(Path(temp) / "trial-0001")
            suite = next(s for s in self.suites.values() if s.framework == "command")
            self.assertIsNone(self.adapter.outcomes(Context(), suite, dict(prefix=prefix)))
            Path(prefix + ".probe-flake.json").write_text(json.dumps({
                "schema": "probe-flake-result/v1", "status": "ok", "runs": [{"outcome": "FAIL"}],
                "check_counts": {"a": {"PASS": 1, "FAIL": 0, "MISSING": 0},
                                 "b": {"PASS": 0, "FAIL": 1, "MISSING": 0},
                                 "c": {"PASS": 0, "FAIL": 0, "MISSING": 1}}}))
            self.assertEqual(self.adapter.outcomes(Context(), suite, dict(prefix=prefix)),
                             {"a": "passed", "b": "failed", "c": "missing", "run": "failed"})

    def test_seeds_only_state_what_the_census_measured(self):
        for key, suite in self.probes().items():
            seeds = self.adapter.seed(Context(), suite, {})
            current = ((self.census.get(key) or {}).get("census") or {}).get("current")
            if not current:
                self.assertEqual(seeds, {}, key)
            for check, seed in seeds.items():
                self.assertIn(seed["status"], ("stable", "flaky"))
                self.assertIn(check, suite.checks)

    def test_exit_suites_have_disjoint_port_spans(self):
        spans = sorted((s.data["port"], s.data["port"] + s.data["span"], k)
                       for k, s in self.probes().items() if s.framework == "exit")
        for (start, end, key), (next_start, _, next_key) in zip(spans, spans[1:]):
            self.assertLessEqual(end, next_start, f"{key} and {next_key} overlap")
        self.assertTrue(all(not start <= 8008 < end for start, end, _ in spans), "port 8008 is the user's GUI")
        ports = {k: s.data["port"] for k, s in self.probes().items() if s.framework == "exit"}
        # The pairs CRC32 placement collided on.
        for a, b in (("canteen_instance", "follow_command_priority"), ("etymology", "transfer_context_menu"),
                     ("cargo_capacity", "startup_asset_logging")):
            if a in ports and b in ports:
                self.assertNotEqual(ports[a], ports[b], (a, b))
        for key, suite in self.probes().items():
            if suite.framework == "exit" and key not in ci_probes.gpu_probe_keys():
                self.assertEqual(self.adapter.prepare(Context(), suite).argv[-2:], ["--port", str(suite.data["port"])])

    def test_identities_cover_runner_tool_and_build_inputs(self):
        inputs = self.module._identity_inputs(ROOT, Context().revision)
        for required in (".quruntul", "tools", "synarchy.cabal", "cabal.project", "Setup.hs",
                         "docs/save_compat", "docs/audio_authoring.md"):
            self.assertIn(required, inputs)
        original = self.module._git
        for changed in (".quruntul", "tools", "cabal.project", "Setup.hs", "docs/save_compat",
                        "docs/audio_authoring.md"):
            def altered(checkout, *args, _path=changed):
                value = original(checkout, *args)
                return value[::-1] if args[:1] == ("rev-parse",) and args[1].endswith(":" + _path) else value
            self.module._git = altered
            try:
                again = {s.id: s.identity for s in self.adapter.suites(Context())}
            finally:
                self.module._git = original
            self.assertTrue(all(again[k] != s.identity for k, s in self.suites.items()), changed)

    def test_a_harness_error_measurement_stops_the_batch(self):
        # The status probe_flake really writes for malformed or truncated protocol output.
        self.assertIn('measurement.status = "harness-error"', (ROOT / "tools" / "probe_flake.py").read_text())
        suite = next(s for s in self.suites.values() if s.framework == "command")
        with tempfile.TemporaryDirectory() as temp:
            prefix = str(Path(temp) / "trial-0001")
            zero = {c: {"PASS": 0, "FAIL": 0, "MISSING": 0} for c in suite.checks}
            for status, runs, expected in (("harness-error", [], None), ("ok", [], None),
                                           ("ok", [{"outcome": "PASS"}], {"run": "passed"})):
                Path(prefix + ".probe-flake.json").write_text(json.dumps(
                    {"schema": "probe-flake-result/v1", "status": status, "runs": runs,
                     "completed_runs": len(runs), "check_counts": zero}))
                self.assertEqual(self.adapter.outcomes(Context(), suite, dict(prefix=prefix)), expected, status)

    def test_seeds_count_every_sample_of_the_current_cohort(self):
        command = next(s for s in self.probes().values() if s.framework == "command")
        exit_suite = next(s for s in self.probes().values() if s.framework == "exit")
        check = command.checks[0]
        failed_then_clean = {"current": {"commit_sha": "c", "samples": [
            {"completed_runs": 10, "failure_count": 1,
             "check_counts": {check: {"PASS": 9, "FAIL": 1, "MISSING": 0}}},
            {"completed_runs": 10, "failure_count": 0,
             "check_counts": {check: {"PASS": 10, "FAIL": 0, "MISSING": 0}}}]}}
        original = self.module._census
        self.module._census = lambda checkout: {command.data["key"]: {"census": failed_then_clean},
                                                exit_suite.data["key"]: {"census": failed_then_clean}}
        try:
            self.assertEqual(self.adapter.seed(Context(), command, {})[check]["status"], "flaky")
            self.assertEqual(self.adapter.seed(Context(), exit_suite, {})["run"]["status"], "flaky")
        finally:
            self.module._census = original

    def _run_trial(self, argv_tail, *, gpu, busy=False, returncode=0):
        """Run probe_trial.py in process against doubles; return (exit code, calls, written files)."""
        import runpy
        import types
        calls = []

        class Hold:
            def release(self):
                calls.append(("release",))

        class Busy(Exception):
            def describe(self):
                return "gpu-desktop busy"

        lock = types.ModuleType("probe_resource_lock")
        lock.repository_namespace = lambda root: "ns"
        lock.ResourceBusy = Busy

        def wait_acquire(**kwargs):
            calls.append(("wait", kwargs))
            if busy:
                raise Busy()
            return Hold()

        lock.wait_acquire = wait_acquire
        engine = types.ModuleType("probe_engine")
        engine.runner_executable = lambda environ=None, env_var="SYNARCHY_PROBE_ENGINE_EXE": (environ or os.environ).get(env_var)
        codec = types.ModuleType("save_compat_audit_codec")
        codec.ENV_CODEC_EXE = "SYNARCHY_SAVE_CODEC_EXE"
        resources = types.ModuleType("probe_runner_resources")
        resources.GPU_RESOURCE = "gpu-desktop"
        resources.RESOURCE_WAIT_POLL = 5.0
        resources.ENV_HELD_HOST_FDS = "SYNARCHY_PROBE_HELD_HOST_FDS"
        resources.engine_preflight = lambda namespace, environ=None, announce=None: calls.append(("engine",)) or os.environ["SYNARCHY_PROBE_ENGINE_EXE"]
        resources.codec_preflight = lambda namespace, environ=None, announce=None: calls.append(("codec",)) or os.environ["SYNARCHY_SAVE_CODEC_EXE"]
        resources.cross_process_interests = lambda key, namespace: (
            ({"gpu-desktop"} if gpu else set()), {"cabal-build", "repo-config"})
        resources.descendant_hold_env = lambda key, namespace: {"SYNARCHY_PROBE_HELD_HOST": "gpu-desktop"} if gpu else {}

        @contextlib.contextmanager
        def exported_host_hold(held, environ=None):
            calls.append(("export",))
            yield

        resources.exported_host_hold = exported_host_hold
        gpu_hold = types.ModuleType("probe_gpu_hold")
        gpu_hold.child_fds = lambda: [7] if gpu else []
        flake = types.ModuleType("probe_flake")
        measured = []
        flake.main = lambda argv: calls.append(("measure",)) or measured.append(list(argv)) or 0
        names = ("probe_flake", "probe_resource_lock", "probe_runner_resources", "probe_engine",
                 "save_compat_audit_codec", "probe_gpu_hold")
        saved = {name: sys.modules.get(name) for name in names}
        sys.modules.update(probe_flake=flake, probe_resource_lock=lock, probe_runner_resources=resources,
                           probe_engine=engine, save_compat_audit_codec=codec, probe_gpu_hold=gpu_hold)
        argv, cwd, environ = sys.argv, os.getcwd(), dict(os.environ)
        real_run = subprocess.run

        def fake_run(cmd, **kw):
            calls.append(("run", list(cmd), kw))
            return types.SimpleNamespace(returncode=returncode)

        subprocess.run = fake_run
        code = None
        try:
            with tempfile.TemporaryDirectory() as temp:
                os.environ["QURUNTUL_TRIAL_PREFIX"] = str(Path(temp) / "trial-0001")
                os.environ.update(SYNARCHY_PROBE_ENGINE_EXE="/bin/engine", SYNARCHY_SAVE_CODEC_EXE="/bin/codec")
                sys.argv = ["probe_trial.py", *argv_tail]
                with self.assertRaises(SystemExit) as done:
                    runpy.run_path(str(ROOT / ".quruntul" / "probe_trial.py"), run_name="__main__")
                code = done.exception.code
                written = sorted(p.name for p in Path(temp).iterdir())
        finally:
            subprocess.run = real_run
            sys.argv = argv
            os.chdir(cwd)
            os.environ.clear()
            os.environ.update(environ)
            for name, module in saved.items():
                if module is None:
                    sys.modules.pop(name, None)
                else:
                    sys.modules[name] = module
        return code, calls, measured, written

    def test_the_trial_adopts_prepared_executables_and_takes_its_whole_set_in_one_wait(self):
        # Without prepared executables the trial refuses rather than building.
        import runpy
        saved = dict(os.environ)
        argv = sys.argv
        try:
            os.environ["QURUNTUL_TRIAL_PREFIX"] = "/nonexistent/trial"
            os.environ.pop("SYNARCHY_PROBE_ENGINE_EXE", None)
            sys.argv = ["probe_trial.py", "blood_impact"]
            with self.assertRaises(SystemExit) as refused:
                runpy.run_path(str(ROOT / ".quruntul" / "probe_trial.py"), run_name="__main__")
            self.assertEqual(refused.exception.code, 3)
        finally:
            sys.argv = argv
            os.environ.clear()
            os.environ.update(saved)
        # An ordinary probe: ONE bounded wait for its repository set, then the measurement.
        code, calls, measured, _ = self._run_trial(["blood_impact"], gpu=False)
        waits = [c[1] for c in calls if c[0] == "wait"]
        self.assertEqual(len(waits), 1, "a trial waits only for the probe's own resources")
        self.assertIsNotNone(waits[0].get("deadline"))
        self.assertEqual(sorted(waits[0]["shared"]), ["cabal-build", "repo-config"])
        self.assertEqual([c[0] for c in calls], ["engine", "codec", "wait", "export", "measure", "release"])
        # #2931: a lab trial takes the durable failure-evidence policy, so it
        # must not supply a temporary artifact root of its own.
        self.assertEqual(len(measured), 1)
        self.assertNotIn("--artifact-root", measured[0])

    def test_a_gpu_trial_takes_gpu_and_repository_interests_in_one_atomic_wait(self):
        # #2932: two waits (GPU, then the build state) would let a queued runner and a queued
        # build form a cycle through the fairness queue with this trial in the middle.
        code, calls, measured, _ = self._run_trial(["blood_impact"], gpu=True)
        waits = [c[1] for c in calls if c[0] == "wait"]
        self.assertEqual(len(waits), 1, "ONE acquisition covers the GPU and the repository set")
        self.assertEqual(sorted(waits[0]["exclusive"]), ["gpu-desktop"])
        self.assertEqual(sorted(waits[0]["shared"]), ["cabal-build", "repo-config"])
        self.assertIsNotNone(waits[0].get("deadline"))
        self.assertEqual([c[0] for c in calls], ["engine", "codec", "wait", "export", "measure", "release"])
        # While it waits the trial says what it is waiting for and who holds it.
        import io
        import types
        busy = types.SimpleNamespace(
            resource="gpu-desktop", interest="exclusive",
            describe=lambda: "held by me pid 42 [run_probes offscreen] since 12:00:00 (3m00s ago)")
        out = io.StringIO()
        with contextlib.redirect_stdout(out):
            waits[0]["announce"](busy)
        self.assertIn("waiting for 'gpu-desktop' (exclusive)", out.getvalue())
        self.assertIn("pid 42 [run_probes offscreen] since 12:00:00", out.getvalue())

    def test_an_exit_style_gpu_trial_forwards_its_hold_to_the_runner_it_launches(self):
        code, calls, _, written = self._run_trial(["offscreen", "--run-probes", "9123"], gpu=True, returncode=7)
        self.assertEqual(code, 7, "the runner's status is the trial's")
        self.assertEqual([c[0] for c in calls], ["engine", "codec", "wait", "export", "run", "release"])
        waits = [c[1] for c in calls if c[0] == "wait"]
        self.assertEqual(len(waits), 1)
        command, options = next((c[1], c[2]) for c in calls if c[0] == "run")
        self.assertEqual(command[1:], ["tools/run_probes.py", "--only", "offscreen", "--exact", "--jobs", "1",
                                       "--port", "9123"])
        # The nested runner must not ask for what this trial already holds, and must carry the
        # actual lock descriptor so the exclusion outlives this process if it is killed.
        self.assertEqual(options["env"].get("SYNARCHY_PROBE_HELD_HOST"), "gpu-desktop")
        self.assertEqual(options["env"].get("SYNARCHY_PROBE_HELD_HOST_FDS"), "7")
        self.assertEqual(tuple(options["pass_fds"]), (7,))
        self.assertEqual(written, ["trial-0001.run-probes.json"])

    def test_giving_up_on_the_gpu_leaves_no_result_so_the_lab_records_a_blocked_trial(self):
        for tail in (["offscreen"], ["offscreen", "--run-probes", "9123"]):
            code, calls, measured, written = self._run_trial(tail, gpu=True, busy=True)
            self.assertEqual(code, 3)
            self.assertEqual([c[0] for c in calls if c[0] in ("run", "measure", "release")], [], "nothing ran")
            self.assertEqual(written, [], "and no result document exists for the adapter to read as a verdict")
        # The adapter turns the absence into None, which the lab records as a blocked trial.
        suite = next(s for s in self.suites.values() if s.data.get("run_probes"))
        with tempfile.TemporaryDirectory() as temp:
            trial = {"prefix": str(Path(temp) / "trial-0001")}
            self.assertIsNone(self.adapter.outcomes(Context(), suite, trial))
            Path(trial["prefix"] + ".run-probes.json").write_text(json.dumps({"returncode": 1}))
            self.assertEqual(self.adapter.outcomes(Context(), suite, trial), {self.module.RUN_CHECK: "failed"})
            Path(trial["prefix"] + ".run-probes.json").write_text(json.dumps({"returncode": 0}))
            self.assertEqual(self.adapter.outcomes(Context(), suite, trial), {self.module.RUN_CHECK: "passed"})

    def test_direct_only_probes_really_cannot_run_through_run_probes(self):
        # run_probes.py passes only --port; these exit before testing anything.
        registry = {key: script for key, script, _ in probe_runner_registry.PROBES}
        for key in self.module.DIRECT_ONLY:
            done = subprocess.run([sys.executable, str(ROOT / "tools" / registry[key]), "--port", "9"],
                                  cwd=ROOT, capture_output=True, text=True, timeout=60)
            self.assertEqual(done.returncode, 2, (key, done.stderr[-300:]))

    def test_a_run_that_fails_with_every_check_passing_is_a_failure(self):
        suite = next(s for s in self.suites.values() if s.framework == "command")
        self.assertIn(self.module.RUN_CHECK, suite.checks)
        declared = [c for c in suite.checks if c != self.module.RUN_CHECK]
        with tempfile.TemporaryDirectory() as temp:
            prefix = str(Path(temp) / "trial-0001")
            for outcome, expected in (("FAIL", "failed"), ("TIMEOUT", "failed"), ("PASS", "passed")):
                Path(prefix + ".probe-flake.json").write_text(json.dumps({
                    "schema": "probe-flake-result/v1", "status": "ok",
                    "runs": [{"outcome": outcome, "checks": {c: "PASS" for c in declared}}],
                    "check_counts": {c: {"PASS": 1, "FAIL": 0, "MISSING": 0} for c in declared}}))
                read = self.adapter.outcomes(Context(), suite, dict(prefix=prefix))
                self.assertEqual(read[self.module.RUN_CHECK], expected, outcome)
                self.assertTrue(all(read[c] == "passed" for c in declared))
        census = {"current": {"commit_sha": "c", "samples": [{"completed_runs": 10, "failure_count": 1,
                  "check_counts": {c: {"PASS": 10, "FAIL": 0, "MISSING": 0} for c in declared}}]}}
        original = self.module._census
        self.module._census = lambda checkout: {suite.data["key"]: {"census": census}}
        try:
            self.assertEqual(self.adapter.seed(Context(), suite, {})[self.module.RUN_CHECK]["status"], "flaky")
        finally:
            self.module._census = original

    def test_prepare_builds_and_a_trial_only_waits_for_resources_and_runs(self):
        for key, suite in self.probes().items():
            ctx = Context()
            prepared = self.adapter.prepare(ctx, suite)
            preflight = ctx.calls[0]
            self.assertTrue(preflight["argv"][-1].endswith("preflight.py"), key)
            self.assertGreaterEqual(preflight["timeout"], self.module.LOCK_WAIT_SECONDS + self.module.BUILD_SECONDS)
            self.assertEqual(prepared.environment, {"SYNARCHY_PROBE_ENGINE_EXE": "/bin/engine",
                                                    "SYNARCHY_SAVE_CODEC_EXE": "/bin/codec"})
            # A late resource release still leaves the probe its whole timeout.
            self.assertGreaterEqual(suite.trial_seconds, probe_runner_registry.effective_timeout(key)
                                    + self.module.LOCK_WAIT_SECONDS, key)

    def test_a_gpu_probes_trial_limit_covers_waiting_for_the_machine_wide_hold(self):
        # #2932: waiting for another checkout's GPU probe is never charged to the probe.
        gpu = ci_probes.gpu_probe_keys()
        probes = self.probes()
        for key, suite in probes.items():
            base = (probe_runner_registry.effective_timeout(key) + self.module.LOCK_WAIT_SECONDS
                    + self.module.TRIAL_MARGIN_SECONDS)
            if key in gpu:
                self.assertGreaterEqual(suite.trial_seconds, base + self.module.GPU_WAIT_SECONDS, key)
            else:
                self.assertLess(suite.trial_seconds, base + 1, key)
        self.assertTrue(gpu & probes.keys(), "no registered GPU probe is offered to the lab")
        # The adapter and the trial runner must agree on the allowance they split.
        trial = (ROOT / ".quruntul" / "probe_trial.py").read_text()
        self.assertIn(f"GPU_WAIT_SECONDS = {self.module.GPU_WAIT_SECONDS // 3600} * 3600", trial)

    def test_the_adapter_imports_nothing_from_quruntul(self):
        tree = ast.parse((ROOT / ".quruntul" / "adapter.py").read_text())
        imported = [a.name for n in ast.walk(tree) if isinstance(n, ast.Import) for a in n.names]
        imported += [n.module or "" for n in ast.walk(tree) if isinstance(n, ast.ImportFrom)]
        self.assertEqual([m for m in imported if m.split(".")[0] == "quruntul"], [])


if __name__ == "__main__":
    unittest.main()
