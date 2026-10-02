#!/usr/bin/env python3
"""Coverage check for the headless suite's lanes (#2744, CIR-15).

`test-headless/Spec.hs` registers the suite as an ordered list of named
lanes (`Test.Headless.Lanes`); `--lane NAME` runs one of them and no
`--lane` runs them all. The lanes must partition the suite: every example
the whole suite runs runs in exactly one lane, and no lane runs anything
the whole suite does not.

This check proves that WITHOUT running a single example. It asks the built
`synarchy-test-headless` executable for

* its lanes (`--list-lanes`), and
* the inventory of every example it would run (`--dry-run
  --format=inventory`, one JSON path per example plus a total line) — once
  for the whole suite and once per lane,

then compares complete example paths with their multiplicity: two
examples whose `describe`/`it` paths coincide count as two. It fails,
naming every offending example, on an example in no lane, one that runs
more often across the lanes than in the whole suite (including twice in
one lane), one in more than one lane, and one a lane runs that the whole
suite does not. A failed command, a malformed or truncated inventory, or a
total line that disagrees with the examples listed fails the check rather
than shrinking the comparison.

The comparison runs twice: with `SYNARCHY_FULL_TESTS` unset and set to
`1`, because the full tier registers its examples only when the variable is
present. Inherited `HSPEC_*` variables, `.hspec` files and any
`SYNARCHY_FULL_TESTS` value are kept out of every inventory, so nothing can
narrow the whole-suite baseline. It also checks that an unknown lane name
is refused before anything runs, and runs the lane machinery's own
regression (`--lane-self-test`: selection, the default lane as the home of
an unassigned group, flag parsing, on synthetic lanes). That regression is
in no lane, so the suite's examples stay exactly those the single block
registered.

Usage:
  cabal build synarchy-test-headless
  python3 tools/headless_lanes.py [--exe PATH]

Without `--exe` the executable is found with `cabal list-bin` (the
`CABAL` environment variable names the cabal binary; default `cabal`).
This check builds nothing. Exit codes: 0 = the lanes partition the suite
in both tiers; 1 = they do not, or an inventory could not be trusted.
Self-test: `python3 tools/test_headless_lanes.py`.
"""
from __future__ import annotations

import argparse
import json
import os
import subprocess
import sys
from collections import Counter
from dataclasses import dataclass
from pathlib import Path
from typing import Callable, Sequence

REPO_ROOT = Path(__file__).resolve().parent.parent

ITEM_MARKER = "headless-inventory-item "
TOTAL_MARKER = "headless-inventory-total "
INVENTORY_ARGS = ("--ignore-dot-hspec", "--dry-run", "--format=inventory", "--no-color")
UNKNOWN_LANE = "__no_such_lane__"
SELF_TEST_ARGS = ("--lane-self-test", "--ignore-dot-hspec", "--no-color",
                  "--format=failed-examples")
# (label, SYNARCHY_FULL_TESTS value or None for unset)
TIERS: tuple[tuple[str, str | None], ...] = (
    ("SYNARCHY_FULL_TESTS unset", None),
    ("SYNARCHY_FULL_TESTS=1", "1"),
)

ExamplePath = tuple[str, ...]


class InventoryError(Exception):
    """An inventory that cannot be trusted as complete."""


@dataclass(frozen=True)
class Completed:
    returncode: int
    stdout: str
    stderr: str


# A runner takes the executable's arguments and the SYNARCHY_FULL_TESTS
# value (None = unset) and returns the finished process.
Runner = Callable[[Sequence[str], "str | None"], Completed]


# --------------------------------------------------------------------- parsing


def parse_inventory(text: str) -> list[ExamplePath]:
    """Every example path an inventory lists, in order, after checking
    that it ends with exactly one total line agreeing with them."""
    items: list[ExamplePath] = []
    total: int | None = None
    for n, line in enumerate(text.splitlines(), 1):
        if line.startswith(ITEM_MARKER):
            if total is not None:
                raise InventoryError(f"line {n}: an example after the total line")
            try:
                path = json.loads(line[len(ITEM_MARKER):])
            except json.JSONDecodeError as exc:
                raise InventoryError(f"line {n}: malformed example path ({exc})") from None
            if (not isinstance(path, list) or not path
                    or not all(isinstance(p, str) for p in path)):
                raise InventoryError(f"line {n}: an example path must be a non-empty list of strings")
            items.append(tuple(path))
        elif line.startswith(TOTAL_MARKER):
            if total is not None:
                raise InventoryError(f"line {n}: a second total line")
            raw = line[len(TOTAL_MARKER):].strip()
            if not raw.isdigit():
                raise InventoryError(f"line {n}: malformed total {raw!r}")
            total = int(raw)
        # Anything else is the executable's own chatter and is ignored.
    if total is None:
        raise InventoryError("no total line: the inventory is truncated")
    if total != len(items):
        raise InventoryError(f"the total line says {total} examples but {len(items)} were listed")
    return items


def parse_lane_list(text: str) -> tuple[list[str], str]:
    """The lane names `--list-lanes` printed, in order, and the default."""
    names: list[str] = []
    default: str | None = None
    for line in text.splitlines():
        line = line.strip()
        if not line:
            continue
        name, marked = line, False
        if line.endswith(" (default)"):
            name, marked = line[: -len(" (default)")], True
        if not name or any(c.isspace() for c in name):
            raise InventoryError(f"malformed lane name {line!r}")
        if name in names:
            raise InventoryError(f"lane {name!r} listed twice")
        if marked:
            if default is not None:
                raise InventoryError("more than one default lane")
            default = name
        names.append(name)
    if not names:
        raise InventoryError("no lanes listed")
    if default is None:
        raise InventoryError("no default lane listed")
    return names, default


# ------------------------------------------------------------------ comparison


def _label(path: ExamplePath) -> str:
    return " / ".join(path)


def compare(baseline: Sequence[ExamplePath],
            lanes: dict[str, Sequence[ExamplePath]]) -> list[str]:
    """Every way the lanes fail to partition `baseline`, one line each."""
    whole = Counter(baseline)
    per_lane = {name: Counter(items) for name, items in lanes.items()}
    across: Counter[ExamplePath] = Counter()
    for counts in per_lane.values():
        across.update(counts)
    problems: list[str] = []
    for path in sorted(set(whole) | set(across)):
        expected, got = whole[path], across[path]
        holders = [name for name, counts in per_lane.items() if counts[path]]
        label = _label(path)
        if expected == 0:
            problems.append(f"not in the whole suite, but run by {', '.join(holders)}: {label}")
            continue
        if got < expected:
            problems.append(f"in no lane ({expected - got} of {expected} cop"
                            f"{'y' if expected == 1 else 'ies'} missing): {label}")
        elif got > expected:
            problems.append(f"runs more than once ({got} runs across {', '.join(holders)}, "
                            f"{expected} in the whole suite): {label}")
        if len(holders) > 1:
            problems.append(f"in more than one lane ({', '.join(holders)}): {label}")
    return problems


# --------------------------------------------------------------------- running


def subprocess_runner(exe: str) -> Runner:
    def run(args: Sequence[str], tier: str | None) -> Completed:
        env = {k: v for k, v in os.environ.items()
               if not k.startswith("HSPEC_") and k != "SYNARCHY_FULL_TESTS"}
        if tier is not None:
            env["SYNARCHY_FULL_TESTS"] = tier
        proc = subprocess.run([exe, *args], cwd=REPO_ROOT, env=env, capture_output=True,
                              text=True, encoding="utf-8", errors="replace")
        return Completed(proc.returncode, proc.stdout, proc.stderr)
    return run


def _require(run: Runner, args: Sequence[str], tier: str | None, what: str) -> str:
    done = run(args, tier)
    if done.returncode != 0:
        tail = "\n".join(done.stderr.strip().splitlines()[-5:])
        raise InventoryError(f"{what} exited {done.returncode}" + (f":\n{tail}" if tail else ""))
    return done.stdout


@dataclass
class TierResult:
    label: str
    lanes: list[str]
    default: str
    counts: dict[str, int]
    total: int
    problems: list[str]


def check_tier(run: Runner, label: str, tier: str | None) -> TierResult:
    names, default = parse_lane_list(_require(run, ["--list-lanes"], tier, "--list-lanes"))
    baseline = parse_inventory(_require(run, INVENTORY_ARGS, tier, "the whole-suite inventory"))
    lanes = {name: parse_inventory(_require(run, ["--lane", name, *INVENTORY_ARGS], tier,
                                            f"lane {name!r}'s inventory"))
             for name in names}
    problems = compare(baseline, lanes)
    refused = run(["--lane", UNKNOWN_LANE, *INVENTORY_ARGS], tier)
    if refused.returncode == 0 or "unknown headless lane" not in refused.stderr:
        problems.append(f"an unknown lane ({UNKNOWN_LANE!r}) was not refused")
    return TierResult(label, names, default,
                      {name: len(items) for name, items in lanes.items()},
                      len(baseline), problems)


def check_self_test(run: Runner) -> str:
    """Run the lane machinery's regression; its summary line, or a
    failure."""
    done = run(SELF_TEST_ARGS, None)
    summary = next((line for line in reversed(done.stdout.splitlines())
                    if " examples, " in line), "")
    if done.returncode != 0 or not summary.endswith(" 0 failures") or summary.startswith("0 "):
        tail = "\n".join((done.stdout + done.stderr).strip().splitlines()[-20:])
        raise InventoryError(f"--lane-self-test exited {done.returncode}:\n{tail}")
    return summary


def check(run: Runner, out=sys.stdout) -> int:
    failed = False
    try:
        print(f"lane self-test: {check_self_test(run)}", file=out)
    except InventoryError as exc:
        print(f"lane self-test FAILED: {exc}", file=out)
        failed = True
    for label, tier in TIERS:
        try:
            result = check_tier(run, label, tier)
        except InventoryError as exc:
            print(f"{label}: cannot trust the inventory: {exc}", file=out)
            failed = True
            continue
        print(f"{label}:", file=out)
        width = max(len(n) for n in result.lanes)
        for name in result.lanes:
            mark = " (default)" if name == result.default else ""
            print(f"  {name:<{width}}  {result.counts[name]:>6} examples{mark}", file=out)
        summed = sum(result.counts.values())
        print(f"  {'lanes':<{width}}  {summed:>6} examples; whole suite {result.total}", file=out)
        for problem in result.problems:
            print(f"  FAIL {problem}", file=out)
        failed = failed or bool(result.problems)
    print("FAIL: the lanes do not partition the suite" if failed
          else "OK: every example runs in exactly one lane", file=out)
    return 1 if failed else 0


def find_executable() -> str:
    cabal = os.environ.get("CABAL", "cabal")
    proc = subprocess.run([cabal, "list-bin", "-v0", "test:synarchy-test-headless"],
                          cwd=REPO_ROOT, capture_output=True, text=True)
    exe = proc.stdout.strip()
    if proc.returncode != 0 or not exe:
        raise SystemExit(f"cannot locate synarchy-test-headless with `{cabal} list-bin`: "
                         f"{proc.stderr.strip()}")
    if not Path(exe).is_file():
        raise SystemExit(f"{exe} does not exist; run `cabal build synarchy-test-headless` first")
    return exe


def main(argv: Sequence[str] | None = None) -> int:
    parser = argparse.ArgumentParser(description=__doc__.split("\n\n")[0])
    parser.add_argument("--exe", help="the synarchy-test-headless executable "
                                      "(default: found with `cabal list-bin`)")
    args = parser.parse_args(argv)
    exe = args.exe or find_executable()
    return check(subprocess_runner(exe))


if __name__ == "__main__":
    sys.exit(main())
