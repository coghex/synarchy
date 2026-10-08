#!/usr/bin/env python3
"""Engine-free gate for opt-in dry-site diagnostics (#2806).

Drive the real allocator and _run's success/failure reporting boundary with
console/terrain stubs. No engine, GPU, sockets or generated world.
"""
from __future__ import annotations

import contextlib
import io
import json
import re
import sys
import tempfile
import types
import unittest
from pathlib import Path
from unittest.mock import patch

import transfer_context_menu_probe as probe

PREFIX = "  [diagnose-sites] "
ROLES = ("building", "mule", "acolyte", "acolyte2", "wildlife")
REASONS = ("bulk-fluid-exclusion", "insufficient-separation",
           "surface-unavailable-or-unparseable", "non-dry-surface",
           "accepted-dry-site")


class FixtureBoundaryReached(Exception):
    """Stop the positive control after reporting, before transfer scenarios."""


class CapturedOutput(io.StringIO):
    def __init__(self):
        super().__init__()
        self.flushes = 0

    def flush(self):
        self.flushes += 1
        super().flush()


class FakeConsole:
    def __init__(self, surfaces, wet, seed):
        self.surfaces, self.wet, self.seed = surfaces, wet, seed
        self.center = None
        self.calls = []

    def send(self, port, lua, **kwargs):
        self.calls.append(("send", port, lua, kwargs))
        if lua == "return world.getSeed()":
            if isinstance(self.seed, Exception):
                raise self.seed
            return self.seed
        if "world.getInitProgress()" in lua:
            return "3"
        if "engine.loadBuildingYaml(" in lua:
            return "1"
        if "world.getSurfaceAt(" in lua:
            x, y = map(int, re.search(r"getSurfaceAt\((-?\d+), (-?\d+)\)", lua).groups())
            return self.surfaces.get((self.center, (x, y)), "nil")
        if "world.loadChunksInRegion(" in lua or "world.waitForChunks(" in lua:
            return "ok"
        if "unit.spawn(" in lua:
            raise FixtureBoundaryReached
        raise AssertionError(f"unexpected console call: {lua}")

    def send_json(self, port, lua, **kwargs):
        self.calls.append(("send_json", port, lua, kwargs))
        match = re.fullmatch(r"return world.getAreaFluid\((-?\d+), (-?\d+), (\d+)\)", lua)
        if not match:
            raise AssertionError(f"unexpected JSON query: {lua}")
        x, y, radius = map(int, match.groups())
        assert radius == 60
        self.center = (x, y)
        return [{"x": x, "y": y} for x, y in self.wet.get(self.center, [])]


def records(out):
    return [json.loads(line[len(PREFIX):]) for line in out.splitlines()
            if line.startswith(PREFIX)]


def ordinary_output(out):
    return "\n".join(line for line in out.splitlines()
                     if not line.startswith(PREFIX))


class DrySiteDiagnosticsTest(unittest.TestCase):
    def setUp(self):
        # Both modes use the same owned fixture path so calls compare exactly.
        self.scratch = tempfile.TemporaryDirectory(prefix="test_transfer_sites_")
        self.addCleanup(self.scratch.cleanup)

    def drive(self, enabled, centers=None, offsets=None, surfaces=None, wet=None,
              seed="-9007199254740993"):
        console = FakeConsole(surfaces or {}, wet or {}, seed)
        selected = []
        actual_allocator = probe.allocate_dry_anchors
        original_failures = probe.failures
        probe.failures = 0
        continues = 0

        def widget(port, label):
            nonlocal continues
            if label == "Continue":
                continues += 1
                return {} if continues > 2 else {"label": label}
            return {"label": label}

        def allocate(*args, **kwargs):
            sites = actual_allocator(*args, **kwargs)
            selected.append(sites)
            return sites

        out, err = CapturedOutput(), io.StringIO()
        with contextlib.nullcontext(self.scratch.name) as tmp:
            with contextlib.ExitStack() as stack:
                replacements = {
                    "send": console.send, "send_json": console.send_json,
                    "allocate_dry_anchors": allocate,
                    "find_widget": widget, "click_widget_center": lambda *args: None,
                    "poll_until": lambda seconds, fn, **kwargs: fn(),
                    "viewport": lambda *args, **kwargs: {
                        "win_w": 1024, "win_h": 768, "fb_w": 1024, "fb_h": 768},
                    "TEST_BUILDING_YAML": str(Path(tmp) / "building.yaml"),
                }
                if centers is not None:
                    replacements["_search_centres"] = lambda: centers
                if offsets is not None:
                    replacements["_candidate_grid"] = lambda *args: offsets
                for name, value in replacements.items():
                    stack.enter_context(patch.object(probe, name, value))
                stack.enter_context(patch.object(probe.time, "sleep"))
                stack.enter_context(patch("socket.create_connection",
                                          side_effect=AssertionError("real socket forbidden")))
                stack.enter_context(contextlib.redirect_stdout(out))
                stack.enter_context(contextlib.redirect_stderr(err))
                try:
                    code = probe._run(9425, types.SimpleNamespace(
                        size="1024x768", diagnose_sites=enabled))
                except FixtureBoundaryReached:
                    code = "fixture-boundary"
                finally:
                    failures = probe.failures
                    probe.failures = original_failures
        self.assertEqual(err.getvalue(), "")
        self.assertEqual(len(selected), 1, "real allocator must be reached exactly once")
        self.assertGreater(len(console.calls), 3, "real console stubs must be reached")
        self.assertGreaterEqual(out.flushes, len(records(out.getvalue())))
        return code, selected[0], out.getvalue(), console.calls, failures

    def compare_modes(self, **fixture):
        disabled = self.drive(False, **fixture)
        enabled = self.drive(True, **fixture)
        self.assertEqual(disabled[:2], enabled[:2])
        self.assertEqual(disabled[4], enabled[4])
        self.assertEqual(ordinary_output(disabled[2]), ordinary_output(enabled[2]))
        self.assertEqual(disabled[3], [call for call in enabled[3]
                                      if call[2] != "return world.getSeed()"])
        self.assertFalse(records(disabled[2]))
        self.assertFalse(any(call[2] == "return world.getSeed()" for call in disabled[3]))
        return disabled, enabled

    @staticmethod
    def successful_fixture():
        center = (0, 0)
        offsets = [(-60, 0), (-48, 0), (-36, 0), (-24, 0), (-12, 0),
                   (-8, 0), (0, 0), (0, 12), (12, 12), (12, 24)]
        surfaces = {(center, point): "3 1 nil" for point in offsets}
        surfaces[(center, (-48, 0))] = "nil"
        surfaces[(center, (-36, 0))] = "unparseable 1 nil"
        surfaces[(center, (-24, 0))] = "3 1 river"
        return dict(centers=[center], offsets=offsets, surfaces=surfaces,
                    wet={center: [(-60, 0)]})

    def test_success_records_real_categories_seed_and_role_specific_sites(self):
        disabled, enabled = self.compare_modes(**self.successful_fixture())
        code, sites, out, calls, failure_count = enabled
        self.assertEqual(code, "fixture-boundary")
        self.assertEqual(failure_count, 0)
        self.assertEqual(sites, [(-12, 0), (0, 0), (0, 12), (12, 12), (12, 24)])
        recs = records(out)
        self.assertEqual(recs[0], {"kind": "seed", "evidence": "available",
                                   "generated_seed": -9007199254740993})
        self.assertEqual(recs[1], {"kind": "search", "requested_sites": 5,
                                   "radius": 60, "min_separation": 12,
                                   "candidate_detail_limit": 128})
        candidates = [r for r in recs if r["kind"] == "candidate"]
        self.assertEqual([r["candidate"] for r in candidates],
                         [list(p) for p in self.successful_fixture()["offsets"]])
        self.assertTrue(all(r["center"] == [0, 0] for r in candidates))
        self.assertEqual({r["reason"] for r in candidates}, set(REASONS))
        counts = dict(zip(REASONS, (1, 1, 2, 1, 5)))
        summary = next(r for r in recs if r["kind"] == "summary")
        self.assertEqual(summary["counts"], counts)
        self.assertEqual(summary["examined"], 10)
        self.assertEqual(summary["omitted_candidate_details"], 0)
        self.assertEqual(recs[-1]["fixture_sites"], dict(zip(ROLES, map(list, sites))))
        self.assertTrue(recs[-1]["complete"])
        self.assertLess(out.index('"kind": "seed"'), out.index("found five separated"))
        init = next(i for i, call in enumerate(calls) if "getInitProgress" in call[2])
        seed = next(i for i, call in enumerate(calls) if "getSeed" in call[2])
        terrain = next(i for i, call in enumerate(calls) if "loadChunksInRegion" in call[2])
        self.assertLess(init, seed)
        self.assertLess(seed, terrain)
        # Rejections made before the existing surface query do not gain one.
        queried = [call[2] for call in calls if "getSurfaceAt" in call[2]]
        self.assertNotIn("return world.getSurfaceAt(-60, 0)", queried)
        self.assertNotIn("return world.getSurfaceAt(-8, 0)", queried)
        self.assertEqual(len(queried), 8)
        self.assertIn("  (fixture sites: building=(-12, 0) mule=(0, 0) "
                      "acolyte=(0, 12) acolyte2=(12, 12) wildlife=(12, 24))", disabled[2])

    def test_exhaustion_reports_before_existing_return_and_resets_each_center(self):
        centers, offsets = [(0, 0), (8, 0)], [(0, 0), (4, 0), (12, 0)]
        surfaces = {(center, center): "2 1 null" for center in centers}
        surfaces[((8, 0), (20, 0))] = "2 1 nil"  # exact separation threshold
        disabled, enabled = self.compare_modes(centers=centers, offsets=offsets,
                                                surfaces=surfaces)
        self.assertEqual(enabled[0], 1)
        self.assertIsNone(enabled[1])
        self.assertEqual(enabled[4], 1)
        recs = records(enabled[2])
        self.assertEqual([r["center"] for r in recs if r["kind"] == "center"],
                         [[0, 0], [8, 0]])
        summaries = [r for r in recs if r["kind"] == "center-summary"]
        self.assertEqual([r["selected_at_this_center"] for r in summaries],
                         [[[0, 0]], [[8, 0], [20, 0]]])
        self.assertTrue(all(not r["complete"] for r in summaries))
        self.assertFalse(recs[-1]["complete"])
        self.assertNotIn("fixture_sites", recs[-1])
        self.assertIn("no complete five-site selection obtained", enabled[2])
        self.assertLess(enabled[2].index("no complete five-site"),
                        enabled[2].index("[FAIL] found five separated"))
        self.assertNotIn("fixture sites:", enabled[2])
        self.assertTrue(disabled[2].endswith(
            "  (no usable land around (8, 0): 0 of 3 candidate tiles are fluid, "
            "2 dry site(s) found)\n  [FAIL] found five separated dry sites for the fixtures\n"))
        self.assertNotIn("unloaded", enabled[2])

    def test_global_detail_cap_keeps_counting_querying_and_selecting(self):
        centers = [(0, 0), (120, 0), (240, 0)]
        offsets = [(x, y) for x in range(-60, 61, 12) for y in range(-60, 61, 12)]
        sites = [(240 + x, y) for x, y in offsets[-5:]]
        surfaces = {(centers[-1], site): "4 1 nil" for site in sites}
        _, enabled = self.compare_modes(centers=centers, offsets=offsets, surfaces=surfaces)
        self.assertEqual(enabled[0], "fixture-boundary")
        self.assertEqual(enabled[1], sites)
        recs = records(enabled[2])
        details = [r for r in recs if r["kind"] == "candidate"]
        self.assertEqual(len(details), 128)
        self.assertEqual([r["ordinal"] for r in recs if r["kind"] == "center"], [1, 2, 3])
        summary = next(r for r in recs if r["kind"] == "summary")
        self.assertEqual(summary["examined"], 363)
        self.assertEqual(summary["candidate_details"], 128)
        self.assertEqual(summary["omitted_candidate_details"], 235)
        self.assertEqual(summary["counts"]["surface-unavailable-or-unparseable"], 358)
        self.assertEqual(summary["counts"]["accepted-dry-site"], 5)
        per_center = [r for r in recs if r["kind"] == "center-summary"]
        self.assertEqual([r["omitted_candidate_details"] for r in per_center], [0, 114, 121])
        self.assertEqual(len([c for c in enabled[3] if "getSurfaceAt" in c[2]]), 363)
        self.assertEqual(recs[-1]["fixture_sites"], dict(zip(ROLES, map(list, sites))))

    def test_real_candidate_and_center_order_is_preserved_without_synthetic_grids(self):
        offsets = probe._candidate_grid(4, 60)
        centers = probe._search_centres()
        # Fail at origin, then find the five sites in the next production center.
        center = centers[1]
        surfaces = {(center, (center[0] + x, center[1] + y)): "5 1 nil"
                    for x, y in offsets}
        _, enabled = self.compare_modes(surfaces=surfaces)
        recs = records(enabled[2])
        self.assertEqual([r["center"] for r in recs if r["kind"] == "center"],
                         [list(c) for c in centers[:2]])
        details = [r for r in recs if r["kind"] == "candidate"]
        self.assertEqual([r["candidate"] for r in details], [list(p) for p in offsets[:128]])
        self.assertEqual(enabled[1], [(120, 0), (108, 0), (120, -12), (120, 12), (132, 0)])

    def test_missing_seed_evidence_never_changes_success_or_failure(self):
        for seed in ("nil", "null", "garbage", "3.5", "true", "", OSError("synthetic socket error"),
                     RuntimeError("synthetic console error")):
            for successful in (False, True):
                with self.subTest(seed=seed, successful=successful):
                    fixture = self.successful_fixture() if successful else {
                        "centers": [(0, 0)], "offsets": [(0, 0)]}
                    _, enabled = self.compare_modes(seed=seed, **fixture)
                    seed_record = records(enabled[2])[0]
                    self.assertEqual(seed_record["evidence"], "missing")
                    self.assertNotIn("generated_seed", seed_record)
                    self.assertEqual(enabled[0], "fixture-boundary" if successful else 1)

    def test_cli_is_opt_in_and_help_names_the_finite_bound(self):
        for flag, want in (([], False), (["--diagnose-sites"], True)):
            with (patch.object(sys, "argv", ["transfer_context_menu_probe.py", *flag]),
                  patch.object(probe, "boot", return_value=object()) as boot,
                  patch.object(probe, "quit_engine") as quit_engine,
                  patch.object(probe, "_run", return_value=0) as run,
                  contextlib.redirect_stdout(io.StringIO())):
                self.assertEqual(probe.main(), 0)
            self.assertIs(run.call_args.args[1].diagnose_sites, want)
            self.assertEqual(boot.call_count, 1)
            self.assertEqual(quit_engine.call_count, 1)
        out = io.StringIO()
        with (patch.object(sys, "argv", ["transfer_context_menu_probe.py", "--help"]),
              patch.object(probe, "boot") as boot,
              contextlib.redirect_stdout(out)):
            with self.assertRaises(SystemExit) as exc:
                probe.main()
        self.assertEqual(exc.exception.code, 0)
        self.assertIn("--diagnose-sites", out.getvalue())
        self.assertIn("128", out.getvalue())
        boot.assert_not_called()


if __name__ == "__main__":
    result = unittest.TextTestRunner(stream=sys.stdout, verbosity=2).run(
        unittest.defaultTestLoader.loadTestsFromTestCase(DrySiteDiagnosticsTest))
    raise SystemExit(0 if result.wasSuccessful() else 1)
