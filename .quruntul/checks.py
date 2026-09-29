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
from dataclasses import dataclass, field
import hashlib
import importlib.util
import json
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

    def test_every_registered_probe_is_one_suite_unless_the_census_defers_it(self):
        deferred = {k for k, p in self.census.items() if (p.get("census") or {}).get("deferred")}
        expected = {key for key, _, _ in probe_runner_registry.PROBES} - deferred
        self.assertEqual(set(self.probes()), expected)
        self.assertEqual(len(self.suites), len(set(self.suites)))

    def test_classification_follows_the_ci_classifier(self):
        for key, suite in self.probes().items():
            self.assertEqual(suite.kind, "ci" if key in ci_probes.CI_ELIGIBLE else "probe", key)

    def test_protocol_probes_measure_their_declared_checks(self):
        for key, suite in self.probes().items():
            if key in probe_flake.PROTOCOL_PROBES and key not in ci_probes.CI_ELIGIBLE:
                self.assertEqual(suite.framework, "command", key)
                self.assertTrue(suite.checks, key)
            else:
                self.assertEqual(suite.framework, "exit", key)

    def test_hspec_suites_are_sliced_and_the_graphical_one_is_a_desktop_probe(self):
        headless, graphical = self.suites["synarchy-test-headless"], self.suites["synarchy-test-graphical"]
        self.assertEqual((headless.kind, headless.desktop, headless.batch_tests), ("ci", False, 400))
        self.assertEqual((graphical.kind, graphical.desktop), ("probe", True))

    def test_launches_go_through_synarchys_runners(self):
        for key, suite in self.probes().items():
            prepared = self.adapter.prepare(Context(), suite)
            if suite.framework == "exit":
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
                "schema": "probe-flake-result/v1", "status": "ok",
                "check_counts": {"a": {"PASS": 1, "FAIL": 0, "MISSING": 0},
                                 "b": {"PASS": 0, "FAIL": 1, "MISSING": 0},
                                 "c": {"PASS": 0, "FAIL": 0, "MISSING": 1}}}))
            self.assertEqual(self.adapter.outcomes(Context(), suite, dict(prefix=prefix)),
                             {"a": "passed", "b": "failed", "c": "missing"})

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
            if suite.framework == "exit":
                self.assertEqual(self.adapter.prepare(Context(), suite).argv[-2:], ["--port", str(suite.data["port"])])

    def test_identities_cover_runner_tool_and_build_inputs(self):
        inputs = self.module._identity_inputs(ROOT, Context().revision)
        for required in (".quruntul", "tools", "synarchy.cabal", "cabal.project", "Setup.hs"):
            self.assertIn(required, inputs)
        original = self.module._git
        for changed in (".quruntul", "tools", "cabal.project", "Setup.hs"):
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
            for status, expected in (("harness-error", None), ("ok", {})):
                Path(prefix + ".probe-flake.json").write_text(json.dumps(
                    {"schema": "probe-flake-result/v1", "status": status, "completed_runs": 0, "check_counts": zero}))
                self.assertEqual(self.adapter.outcomes(Context(), suite, dict(prefix=prefix)), expected, status)

    def test_the_adapter_imports_nothing_from_quruntul(self):
        tree = ast.parse((ROOT / ".quruntul" / "adapter.py").read_text())
        imported = [a.name for n in ast.walk(tree) if isinstance(n, ast.Import) for a in n.names]
        imported += [n.module or "" for n in ast.walk(tree) if isinstance(n, ast.ImportFrom)]
        self.assertEqual([m for m in imported if m.split(".")[0] == "quruntul"], [])


if __name__ == "__main__":
    unittest.main()
