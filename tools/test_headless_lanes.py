#!/usr/bin/env python3
"""Self-test for tools/headless_lanes.py (#2744), on synthetic input only.

No build, no executable, no engine: the inventories are made-up text and
the executable is a stand-in runner. It shows the check passing on a true
partition — including an example path the whole suite legitimately holds
twice — and failing, naming the example, on each defect the check exists
for: an example in no lane, one run twice in a lane, one in two lanes, one
the whole suite does not run, a failed command, malformed or truncated
inventory output, a total that disagrees with the listing, an unknown
lane that is not refused, and a failing `--lane-self-test`.

Usage: python3 tools/test_headless_lanes.py
"""
from __future__ import annotations

import io
import json
import sys
import unittest
from pathlib import Path

sys.path.insert(0, str(Path(__file__).resolve().parent))

import headless_lanes as hl  # noqa: E402

A = ("shared", "a")
B = ("shared", "b")
C = ("rest", "c")
DUP = ("rest", "same name")  # the whole suite holds this path twice


def inventory(paths, total=None) -> str:
    lines = ["engine chatter that is not part of the inventory"]
    lines += [hl.ITEM_MARKER + json.dumps(list(p)) for p in paths]
    lines.append(f"{hl.TOTAL_MARKER}{len(paths) if total is None else total}")
    return "\n".join(lines) + "\n"


class FakeExe:
    """Answers the check's commands from a table of lane inventories."""

    def __init__(self, whole, lanes, default="rest", *, refuse_unknown=True,
                 fail=None, full_tier_extra=(), self_test_failures=0):
        self.whole, self.lanes, self.default = whole, lanes, default
        self.refuse_unknown, self.fail = refuse_unknown, fail
        self.self_test_failures = self_test_failures
        self.full_tier_extra = list(full_tier_extra)
        self.calls = []

    def __call__(self, args, tier):
        self.calls.append((tuple(args), tier))
        args = list(args)
        if args[:1] == ["--lane-self-test"]:
            n = self.self_test_failures
            return hl.Completed(1 if n else 0, f"\n7 examples, {n} failures\n", "")
        if args == ["--list-lanes"]:
            out = "".join(f"{n}{' (default)' if n == self.default else ''}\n" for n in self.lanes)
            return hl.Completed(0, out, "")
        lane = args[1] if args[:1] == ["--lane"] else None
        if self.fail is not None and lane == self.fail:
            return hl.Completed(1, "", "boom: the runner crashed\n")
        extra = self.full_tier_extra if tier is not None else []
        if lane is None:
            return hl.Completed(0, inventory(self.whole + extra), "")
        if lane not in self.lanes:
            if self.refuse_unknown:
                return hl.Completed(1, "", f'unknown headless lane "{lane}"\n')
            return hl.Completed(0, inventory([]), "")
        paths = self.lanes[lane] + (extra if lane == self.default else [])
        return hl.Completed(0, inventory(paths), "")


def run_check(exe) -> tuple[int, str]:
    out = io.StringIO()
    code = hl.check(exe, out)
    return code, out.getvalue()


WHOLE = [A, B, C, DUP, DUP]
GOOD = {"shared": [A, B], "rest": [C, DUP, DUP]}


class Parsing(unittest.TestCase):
    def test_inventory_round_trip_ignores_chatter(self):
        self.assertEqual(hl.parse_inventory(inventory(WHOLE)), WHOLE)

    def test_truncated_inventory_fails(self):
        text = "".join(hl.ITEM_MARKER + json.dumps(list(p)) + "\n" for p in WHOLE)
        with self.assertRaisesRegex(hl.InventoryError, "truncated"):
            hl.parse_inventory(text)

    def test_total_disagreeing_with_listing_fails(self):
        with self.assertRaisesRegex(hl.InventoryError, "says 6 examples but 5"):
            hl.parse_inventory(inventory(WHOLE, total=6))

    def test_malformed_example_fails(self):
        for bad in ('["unterminated', '"not a list"', "[]", "[1, 2]"):
            with self.subTest(bad=bad), self.assertRaises(hl.InventoryError):
                hl.parse_inventory(f"{hl.ITEM_MARKER}{bad}\n{hl.TOTAL_MARKER}1\n")

    def test_example_after_total_or_second_total_fails(self):
        with self.assertRaisesRegex(hl.InventoryError, "after the total"):
            hl.parse_inventory(inventory([A]) + hl.ITEM_MARKER + json.dumps(list(B)) + "\n")
        with self.assertRaisesRegex(hl.InventoryError, "second total"):
            hl.parse_inventory(inventory([A]) + f"{hl.TOTAL_MARKER}1\n")

    def test_lane_list(self):
        self.assertEqual(hl.parse_lane_list("world\nrest (default)\n"), (["world", "rest"], "rest"))
        for bad in ("", "world\nrest\n", "a (default)\nb (default)\n", "a\na (default)\n", "two words\n"):
            with self.subTest(bad=bad), self.assertRaises(hl.InventoryError):
                hl.parse_lane_list(bad)


class Comparison(unittest.TestCase):
    def test_true_partition_passes_with_multiplicity(self):
        self.assertEqual(hl.compare(WHOLE, GOOD), [])

    def test_example_in_no_lane(self):
        problems = hl.compare(WHOLE, {"shared": [A], "rest": [C, DUP, DUP]})
        self.assertEqual(problems, ["in no lane (1 of 1 copy missing): shared / b"])

    def test_one_copy_of_a_repeated_path_missing(self):
        problems = hl.compare(WHOLE, {"shared": [A, B], "rest": [C, DUP]})
        self.assertEqual(problems, ["in no lane (1 of 2 copies missing): rest / same name"])

    def test_duplicate_within_one_lane(self):
        problems = hl.compare(WHOLE, {"shared": [A, B, B], "rest": [C, DUP, DUP]})
        self.assertEqual(problems, [
            "runs more than once (2 runs across shared, 1 in the whole suite): shared / b"])

    def test_example_in_two_lanes(self):
        problems = hl.compare(WHOLE, {"shared": [A, B, C], "rest": [C, DUP, DUP]})
        self.assertEqual(problems, [
            "runs more than once (2 runs across shared, rest, 1 in the whole suite): rest / c",
            "in more than one lane (shared, rest): rest / c"])

    def test_example_the_whole_suite_does_not_run(self):
        problems = hl.compare(WHOLE, {"shared": [A, B, ("ghost", "x")], "rest": [C, DUP, DUP]})
        self.assertEqual(problems, ["not in the whole suite, but run by shared: ghost / x"])


class EndToEnd(unittest.TestCase):
    def test_pass_reports_each_lane_and_both_tiers(self):
        exe = FakeExe(WHOLE, GOOD, full_tier_extra=[("rest", "full tier example")])
        code, out = run_check(exe)
        self.assertEqual(code, 0, out)
        self.assertIn("SYNARCHY_FULL_TESTS unset:", out)
        self.assertIn("SYNARCHY_FULL_TESTS=1:", out)
        self.assertRegex(out, r"shared\s+2 examples\n")
        self.assertRegex(out, r"rest\s+3 examples \(default\)")
        self.assertRegex(out, r"rest\s+4 examples \(default\)")
        self.assertRegex(out, r"lanes\s+5 examples; whole suite 5")
        self.assertRegex(out, r"lanes\s+6 examples; whole suite 6")
        self.assertTrue(out.rstrip().endswith("OK: every example runs in exactly one lane"))
        self.assertEqual({tier for _, tier in exe.calls}, {None, "1"})

    def test_omission_fails_naming_the_example(self):
        code, out = run_check(FakeExe(WHOLE, {"shared": [A], "rest": [C, DUP, DUP]}))
        self.assertEqual(code, 1)
        self.assertIn("FAIL in no lane (1 of 1 copy missing): shared / b", out)

    def test_overlap_fails(self):
        code, out = run_check(FakeExe(WHOLE, {"shared": [A, B, C], "rest": [C, DUP, DUP]}))
        self.assertEqual(code, 1)
        self.assertIn("FAIL in more than one lane (shared, rest): rest / c", out)

    def test_failed_command_fails_rather_than_shrinking(self):
        code, out = run_check(FakeExe(WHOLE, GOOD, fail="shared"))
        self.assertEqual(code, 1)
        self.assertIn("cannot trust the inventory: lane 'shared''s inventory exited 1", out)
        self.assertIn("boom: the runner crashed", out)

    def test_lane_self_test_runs_and_must_pass(self):
        exe = FakeExe(WHOLE, GOOD)
        code, out = run_check(exe)
        self.assertEqual(code, 0, out)
        self.assertIn("lane self-test: 7 examples, 0 failures", out)
        self.assertIn((hl.SELF_TEST_ARGS, None), exe.calls)
        code, out = run_check(FakeExe(WHOLE, GOOD, self_test_failures=2))
        self.assertEqual(code, 1)
        self.assertIn("lane self-test FAILED: --lane-self-test exited 1", out)

    def test_unknown_lane_must_be_refused(self):
        code, out = run_check(FakeExe(WHOLE, GOOD, refuse_unknown=False))
        self.assertEqual(code, 1)
        self.assertIn("FAIL an unknown lane ('__no_such_lane__') was not refused", out)


if __name__ == "__main__":
    unittest.main()
