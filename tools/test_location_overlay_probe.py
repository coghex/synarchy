#!/usr/bin/env python3
"""Engine-free regression gate for overlay log retention (#2794).

Run the real main/run, boot funnel, save/load failures and cleanup against
truncating launch stubs. No engine, sockets, GPU or world generation.
"""
from __future__ import annotations

import builtins
import contextlib
import importlib
import io
import sys
import tempfile
import unittest
from pathlib import Path
from unittest.mock import patch

from probe_runner_diagnostics import FAILURE_LOG_TAIL_LINES, failure_records


class OverlayLogsTest(unittest.TestCase):
    @classmethod
    def setUpClass(cls):
        # Guard the FIRST import, including its transitive imports. Reading
        # Python source is fine; allocating directories or writing is not.
        real_open = builtins.open

        def read_only_open(file, mode="r", *args, **kwargs):
            if any(flag in mode for flag in "wax+"):
                raise AssertionError(f"import wrote a file: {file}")
            return real_open(file, mode, *args, **kwargs)

        with (patch("tempfile.mkdtemp", side_effect=AssertionError("import allocated")),
              patch("os.mkdir", side_effect=AssertionError("import mkdir")),
              patch("builtins.open", side_effect=read_only_open)):
            cls.probe = importlib.import_module("location_overlay_probe")

    def setUp(self):
        self.scratch = tempfile.TemporaryDirectory(prefix="test_overlay_logs_")
        self.addCleanup(self.scratch.cleanup)
        self.top = Path(self.scratch.name)
        self.repo = self.top / "repo"
        for family in ("config", "scripts", "assets", "data"):
            (self.repo / family).mkdir(parents=True)
        (self.repo / "config/default.yaml").write_text("tracked: default\n")
        (self.repo / "config/private.local.yaml").write_text("developer: private\n")
        self.unrelated = self.top / "location_overlay_engine.log"
        self.unrelated.write_text("unrelated pre-existing log\n")
        self.allocations = []
        self.real_mkdtemp = tempfile.mkdtemp

    def invoke(self, failure=None, abort_boot=None, preparation=False,
               console_exception=False, root_failure=False):
        probe = self.probe
        paths, bodies, launches, stopped, commands, roots = [], [], [], [], [], []
        generations, fixtures = [], []
        places = [{"id": "ruin_small", "cx": i, "cy": i, "gx": i * 16,
                   "gy": i * 16} for i in (1, 2)]

        def allocate(*args, **kwargs):
            path = self.real_mkdtemp(*args, dir=self.top, **kwargs)
            self.allocations.append(Path(path))
            return path

        def launch(port, log=None, args=None):
            self.assertEqual(port, 9189)
            self.assertIsNotNone(log, "real boot funnel must pass its log")
            path = Path(log)
            # Protect external files even when testing a regressed allocator.
            self.assertTrue(path.is_relative_to(self.top), str(path))
            paths.append(path)
            root = Path(args[args.index("--resource-root") + 1])
            roots.append(root)
            ordinal = len(paths)
            if preparation and ordinal == abort_boot:
                Path(str(path) + ".prepare").write_text("preparation refused\n")
                raise SystemExit(17)
            body = "".join(f"boot {ordinal} sentinel {n}\n" for n in range(25))
            with open(path, "w") as handle:  # shared launcher's truncating open
                handle.write(body)
            bodies.append(body)
            (root / "config/runtime.local.yaml").write_text("runtime config\n")
            (root / "saves/fixture").write_text("runtime save\n")
            self.assertFalse((root / "config/private.local.yaml").exists())
            if ordinal == abort_boot:
                raise SystemExit(17)
            proc = object()
            launches.append(proc)
            return proc

        def console(port, lua, **kwargs):
            commands.append(lua)
            if "engine.saveWorld(" in lua:
                return "false" if failure == "save" and len(paths) == 1 else "true"
            if "engine.loadSave(" in lua:
                if console_exception and len(paths) == 2:
                    raise RuntimeError("synthetic console failure")
                return "false" if failure == "load" and len(paths) == 2 else "true"
            if "world.getChunkInfo(" in lua:
                return "y"
            if "world.getActiveWorldId()" in lua:
                return "arena"
            if "world.getTerrainAt(" in lua:
                return "29"
            if "structure.floorZAt(" in lua:
                return "30" if "'sw'" in lua else "1"
            if "local phase = world.waitForInit(900)" in lua:
                return "3"
            return "ok"

        out, err = io.StringIO(), io.StringIO()
        with contextlib.ExitStack() as stack:
            replacements = {
                "REPO": self.repo, "THIN_YAML": str(self.top / "thin.yaml"),
                "DENSE_YAML": str(self.top / "dense.yaml"),
                # On the baseline, sandbox the fixed path to prove the bug
                # without touching the operator's real /tmp capture.
                "boot": launch, "send": console,
                "quit_engine": lambda port, proc: stopped.append(proc),
                "gen_world": lambda *args: generations.append(args),
                "load_fixture_yaml": lambda *args: fixtures.append(args),
                "placed_ready": lambda *args: places,
                "placed_on_page": lambda *args: places,
                "has_floor": lambda *args: True,
                "is_ocean": lambda *args: False,
                "footprint_water": lambda *args: "dry",
                "load_chunk": lambda *args: None,
                "wait_stamped": lambda port, ruins: len(ruins),
                "has_loc_on": lambda *args, **kwargs: True,
                "wait_floor": lambda *args, **kwargs: True,
                "capture_request_id": lambda *args: 41,
                "wait_save_complete": lambda *args: (True, {"phase": "SaveCaptureComplete"}),
                "wait_load_published": lambda *args, **kwargs: (True, {"phase": "published"}),
            }
            if hasattr(probe, "LOG"):
                replacements["LOG"] = str(self.unrelated)
            if root_failure:
                real_remove = probe.remove_isolated_root
                def remove_with_report(base):
                    self.assertIsNone(real_remove(base))
                    return "synthetic resource-root removal leftover"
                replacements["remove_isolated_root"] = remove_with_report
            for name, value in replacements.items():
                stack.enter_context(patch.object(probe, name, value))
            stack.enter_context(patch.object(probe.tempfile, "mkdtemp", side_effect=allocate))
            stack.enter_context(patch.object(probe.time, "sleep"))
            stack.enter_context(patch.object(sys, "argv", ["location_overlay_probe.py"]))
            stack.enter_context(contextlib.redirect_stdout(out))
            stack.enter_context(contextlib.redirect_stderr(err))
            try:
                code = probe.main()
                exception = None
            except (SystemExit, RuntimeError) as exc:
                code, exception = None, exc
        self.assertEqual(err.getvalue(), "")
        self.assertEqual(stopped, launches, "every handed-off engine is quit exactly once")
        self.assertTrue(paths, "launch stub was actually reached")
        self.assertTrue(all(not root.parent.exists() for root in roots),
                        "real cleanup removes config/save resource roots")
        self.assertEqual(self.unrelated.read_text(), "unrelated pre-existing log\n")
        self.assertEqual((self.repo / "config/private.local.yaml").read_text(),
                         "developer: private\n")
        created = paths[:-1] if preparation else paths
        self.assertEqual(len(set(paths)), len(paths), "reused port never reuses a path")
        for path, body in zip(created, bodies):
            self.assertTrue(path.is_file(), f"capture survived cleanup: {path}")
            self.assertEqual(path.read_text(), body, "later boots preserve each sentinel")
            self.assertIn(str(path), out.getvalue(), "every actual capture is reported")
        return code, exception, out.getvalue(), paths, commands, generations, fixtures

    def test_completion_and_separate_invocations(self):
        first = self.invoke()
        second = self.invoke()
        for code, exception, out, paths, commands, generations, fixtures in (first, second):
            self.assertEqual(code, 0)
            self.assertIsNone(exception)
            self.assertEqual(len(paths), 6 + len(self.probe.PLACEMENT_MATRIX))
            self.assertEqual(generations, [(9189, page, 42, 64, 3)
                                          for page in ("wa", "wb", "wc", "wd")])
            self.assertTrue(fixtures, "fixture stub reached")
            matrix = [line for line in commands if "world.init('mx'" in line]
            self.assertEqual(matrix, [f"world.init('mx', {seed}, {size}, {plates}); return 'ok'"
                                     for seed, size, plates, _ in self.probe.PLACEMENT_MATRIX])
            self.assertIn("phase 1:", out)
            self.assertIn("phase 9:", out)
        self.assertFalse(set(first[3]) & set(second[3]), "invocations share no log path")

    def test_refusals_capture_the_source_boot_after_later_boots(self):
        for failure, source_index in (("save", 0), ("load", 1)):
            with self.subTest(failure=failure):
                code, exc, out, paths, commands, *_ = self.invoke(failure=failure)
                self.assertEqual(code, 1)
                self.assertIsNone(exc)
                self.assertGreater(len(paths), source_index + 1)
                records = failure_records(out)
                failed = [r for r in records if r.kind == "check" and "was not accepted" in r.detail]
                self.assertEqual(len(failed), 1)
                source = paths[source_index]
                self.assertIn(str(source), failed[0].detail)
                self.assertNotIn(str(paths[-1]), failed[0].detail)
                self.assertIn(f"engine log {source_index + 1:02d} (phase", failed[0].detail)
                contexts = [r for r in records if r.kind == "context"]
                self.assertEqual([r.detail for r in contexts if not r.identity.endswith(" tail")],
                                 [str(source)])
                tails = [r.detail for r in contexts if r.identity.endswith(" tail")]
                self.assertEqual(tails, source.read_text().splitlines()[-FAILURE_LOG_TAIL_LINES:])
                self.assertTrue(any(f"engine.{failure}" in line for line in commands))

    def test_exception_retains_logs_and_cleans_the_root(self):
        code, exc, out, paths, *_ = self.invoke(console_exception=True)
        self.assertIsNone(code)
        self.assertIsInstance(exc, RuntimeError)
        self.assertEqual(len(paths), 2)
        self.assertTrue(any(r.kind == "context" and r.detail == str(paths[1])
                            for r in failure_records(out)))

    def test_system_exit_before_ready_reports_the_created_log(self):
        code, exc, out, paths, *_ = self.invoke(abort_boot=3)
        self.assertIsNone(code)
        self.assertIsInstance(exc, SystemExit)
        self.assertEqual(exc.code, 17)
        self.assertEqual(len(paths), 3)
        self.assertTrue(any(r.kind == "context" and r.detail == str(paths[2])
                            for r in failure_records(out)))

    def test_preparation_failure_never_claims_an_unopened_log(self):
        code, exc, out, paths, *_ = self.invoke(abort_boot=3, preparation=True)
        self.assertIsNone(code)
        self.assertIsInstance(exc, SystemExit)
        self.assertFalse(paths[-1].exists())
        self.assertNotIn(str(paths[-1]), out)
        self.assertIn("engine log 03 (phase 3:", out)
        self.assertIn("no engine log created", out)
        self.assertFalse(failure_records(out))

    def test_prior_failure_survives_a_later_boot_abort(self):
        code, exc, out, paths, *_ = self.invoke(failure="load", abort_boot=3)
        self.assertIsNone(code)
        self.assertIsInstance(exc, SystemExit)
        records = failure_records(out)
        failures = [r for r in records if r.kind == "check"]
        self.assertEqual(len(failures), 1)
        self.assertIn(str(paths[1]), failures[0].detail)
        contexts = [r.detail for r in records if r.kind == "context"
                    and not r.identity.endswith(" tail")]
        self.assertEqual(contexts, [str(paths[1]), str(paths[2])])

    def test_root_cleanup_failure_has_no_engine_source(self):
        code, exc, out, *_ = self.invoke(root_failure=True)
        self.assertEqual(code, 1)
        self.assertIsNone(exc)
        failures = [r for r in failure_records(out) if r.kind == "check"]
        self.assertEqual(len(failures), 1)
        self.assertEqual(failures[0].detail, "synthetic resource-root removal leftover")
        self.assertFalse(any(r.kind == "context" for r in failure_records(out)))


if __name__ == "__main__":
    result = unittest.TextTestRunner(stream=sys.stdout, verbosity=2).run(
        unittest.defaultTestLoader.loadTestsFromTestCase(OverlayLogsTest))
    raise SystemExit(0 if result.wasSuccessful() else 1)
