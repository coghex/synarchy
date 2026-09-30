#!/usr/bin/env python3
"""Measure where the headless suite's CI time goes (#2743, CIR-14).

`docs/headless_suite_lane_measurement.md` is the report this produces.
Two halves, both rerunnable by a later agent:

``instrument``
    Rewrites a checkout's `test-headless/Spec.hs` and
    `test-headless/Test/Headless/Harness.hs` IN PLACE into the temporary
    collection form. It never touches `.github/workflows/ci.yml` or
    `tools/ci-local.sh`, and the rewritten files are only ever committed
    on intermediate commits that are reverted before review (#2743
    requirement 9). What it adds, and nothing else:

    * every top-level statement of `main`'s do-block is wrapped in
      ``describe "@G<line>"``, and every statement of a top-level
      do-block statement additionally in ``describe "@S<line>"``, where
      ``<line>`` is that statement's 1-based line in the UNMODIFIED
      `Spec.hs`. A describe wrapper changes an example's path, not what
      it runs or the order it runs in, so every example lands in exactly
      one group without re-deriving the call graph of imported specs;
    * `main` runs the same spec through hspec's own primitives
      (`evalSpec`, `readConfig`, `runSpecForest`, `evaluateResult`) and
      tees every `ItemDone` event, rendered with its derived `Show`
      instance, to stdout as ``@@HSPEC-ITEM ... @@END``. That carries the
      example's full path, source location and unrounded duration --
      `--print-slow-items` floors to whole milliseconds and drops every
      example that floors to zero;
    * the harness prints ``@@SHARED-WORLD-GEN`` when `sharedWorld`
      actually generates a key, ``@@SHARED-WORLD-HIT`` when it returns
      an already-generated one, and ``@@ENGINE-BOOT`` /
      ``@@ENGINE-TEARDOWN`` around each headless engine's setup and
      teardown, each with its own monotonic duration.

    ``--full-tier`` additionally sets ``SYNARCHY_FULL_TESTS=1`` from
    `main` itself, so a full-tier collection needs no workflow edit.

``aggregate``
    Downloads each named run's `test-and-audits` job log with `gh`,
    parses those records plus Hspec's footer, and prints the tables the
    report carries as Markdown (or ``--json``).
"""

from __future__ import annotations

import argparse
import json
import re
import statistics
import subprocess
import sys
from dataclasses import dataclass, field
from pathlib import Path

SPEC = Path("test-headless/Spec.hs")
HARNESS = Path("test-headless/Test/Headless/Harness.hs")
MAIN_HEAD = "main ∷ IO ()\nmain = hspec $ do\n"
MARK = "@@HSPEC-ITEM "
END = " @@END"
CI_JOB = "test-and-audits"
HEADLESS_STEP = "Headless test suite"


class MeasureError(Exception):
    pass


# --------------------------------------------------------------------------
# instrument

def _indent(line: str) -> int:
    return len(line) - len(line.lstrip(" "))


def _is_code(line: str, indent: int) -> bool:
    stripped = line.strip()
    return (bool(stripped) and not stripped.startswith("--")
            and _indent(line) == indent)


def _statements(lines: list[str], lo: int, hi: int, indent: int):
    """(start, end) index spans of the statements at `indent` in [lo, hi)."""
    starts = [i for i in range(lo, hi) if _is_code(lines[i], indent)]
    for i in range(lo, hi):
        stripped = lines[i].strip()
        if stripped and not stripped.startswith("--") \
                and _indent(lines[i]) < indent:
            raise MeasureError(f"Spec.hs:{i + 1}: code shallower than the "
                               f"block's statements: {lines[i]!r}")
    return [(s, starts[k + 1] if k + 1 < len(starts) else hi)
            for k, s in enumerate(starts)]


def _shift(line: str, by: int) -> str:
    return (" " * by + line) if line.strip() else line


def _block_members(lines: list[str], start: int, end: int) -> tuple[int, int]:
    """First member line and member indent of the do-block opened at start."""
    for i in range(start + 1, end):
        stripped = lines[i].strip()
        if stripped and not stripped.startswith("--"):
            if _indent(lines[i]) <= 4:
                break
            return i, _indent(lines[i])
    raise MeasureError(f"Spec.hs:{start + 1}: do-block without statements")


def wrap_spec_body(lines: list[str], lo: int, hi: int) -> list[str]:
    """Wrap `main`'s statements [lo, hi) in @G / @S describe groups."""
    first = next(i for i in range(lo, hi) if _is_code(lines[i], 4))
    out: list[str] = list(lines[lo:first])
    for start, end in _statements(lines, first, hi, 4):
        out.append(f'    describe "@G{start + 1}" $ do')
        head = lines[start]
        nested = head.rstrip().endswith("$ do")
        if not nested:
            out.extend(_shift(line, 4) for line in lines[start:end])
            continue
        out.append(_shift(head, 4))
        first_inner, depth = _block_members(lines, start, end)
        out.extend(_shift(line, 4) for line in lines[start + 1:first_inner])
        for s, e in _statements(lines, first_inner, end, depth):
            out.append(" " * (depth + 4) + f'describe "@S{s + 1}" $ do')
            out.extend(_shift(line, 8) for line in lines[s:e])
    return out


INSTRUMENTED_MAIN = """\
main ∷ IO ()
main = do
{full_tier}    (measureConfig0, measureForest) ← MeasureRunner.evalSpec
        MeasureRunner.defaultConfig measuredSpec
    measureConfig ← MeasureRunner.readConfig measureConfig0
        =≪ MeasureEnv.getArgs
    let measureTee mk fc = do
            base ← mk fc
            pure $ \\event → do
                () ← base event
                let rendered = show event
                when ("ItemDone " `MeasureList.isPrefixOf` rendered) $
                    MeasureIO.hPutStrLn MeasureIO.stdout
                        ("{mark}" ⧺ rendered ⧺ "{end}")
        measureConfig' = measureConfig
            {{ MeasureRunner.configFormat =
                measureTee ⊚ MeasureRunner.configFormat measureConfig }}
    MeasureEnv.withArgs [] (MeasureRunner.runSpecForest measureForest
        measureConfig') ≫= MeasureRunner.evaluateResult

measuredSpec ∷ Spec
measuredSpec = do
"""

MAIN_IMPORTS = """\
import qualified Data.List as MeasureList
import qualified System.Environment as MeasureEnv
import qualified System.IO as MeasureIO
import qualified Test.Hspec.Runner as MeasureRunner
"""


def _body_lines(head: str, body: str) -> tuple[list[str], int, int]:
    """Index-aligned lines: `lines[i]` is Spec.hs line i + 1 in the body."""
    offset = head.count("\n") + MAIN_HEAD.count("\n")
    body_lines = body.split("\n")
    if body_lines and body_lines[-1] == "":
        body_lines.pop()
    lines = [""] * offset + body_lines
    return lines, offset, len(lines)


def instrument_spec(text: str, full_tier: bool) -> str:
    if "measuredSpec" in text:
        raise MeasureError("Spec.hs is already instrumented")
    head, sep, body = text.partition(MAIN_HEAD)
    if not sep:
        raise MeasureError("Spec.hs: `main = hspec $ do` not found")
    lines, offset, hi = _body_lines(head, body)
    for i in range(offset, hi):
        if lines[i].strip() and _indent(lines[i]) == 0:
            raise MeasureError(f"Spec.hs:{i + 1}: a top-level declaration "
                               "follows main; the body is not the file's end")
    wrapped = wrap_spec_body(lines, offset, hi)
    anchor = "import Test.Hspec\n"
    if anchor not in head:
        raise MeasureError("Spec.hs: `import Test.Hspec` not found")
    head = head.replace(anchor, anchor + MAIN_IMPORTS, 1)
    full = ('    MeasureEnv.setEnv "SYNARCHY_FULL_TESTS" "1"\n'
            if full_tier else "")
    main = INSTRUMENTED_MAIN.format(full_tier=full, mark=MARK, end=END)
    return head + main + "\n".join(wrapped) + "\n"


HARNESS_EDITS = [
    ("import UPrelude\n",
     "import UPrelude\n"
     "import qualified GHC.Clock as MeasureClock\n"
     "import qualified System.IO as MeasureIO\n"),
    ("""        Just _ → waitForWorldInit env pid 300
""",
     """        Just _ → do
            MeasureIO.hPutStrLn MeasureIO.stdout $ "@@SHARED-WORLD-HIT "
                ⧺ show (seed, size, plateCount) ⧺ " @@END"
            waitForWorldInit env pid 300
"""),
    ("""        Nothing → do
            sendWorldCommand env (WorldInit pid seed size plateCount Nothing)
            waitForWorldInit env pid 300
""",
     """        Nothing → do
            measureT0 ← MeasureClock.getMonotonicTime
            sendWorldCommand env (WorldInit pid seed size plateCount Nothing)
            measureWs ← waitForWorldInit env pid 300
            measureT1 ← MeasureClock.getMonotonicTime
            MeasureIO.hPutStrLn MeasureIO.stdout $ "@@SHARED-WORLD-GEN "
                ⧺ show (seed, size, plateCount) ⧺ " "
                ⧺ show (measureT1 - measureT0) ⧺ " @@END"
            pure measureWs
"""),
    ("""    bracket setup teardown $ \\(env, workers) →
        withHeadlessWorkerCheck expectedStopped workers (action env)
  where
    setup = do
""",
     """    bracket (measureTimed "boot world" setup)
             (measureTimed "teardown world" ∘ teardown) $ \\(env, workers) →
        withHeadlessWorkerCheck expectedStopped workers (action env)
  where
    setup = do
"""),
    ("""withHeadlessEngineNoWorld = bracket setup teardown
""",
     """withHeadlessEngineNoWorld = bracket (measureTimed "boot noworld" setup)
                                    (measureTimed "teardown noworld" ∘ teardown)
"""),
]

HARNESS_HELPER = """
-- | Temporary #2743 collection helper: time one harness phase.
measureTimed ∷ String → IO α → IO α
measureTimed label act = do
    t0 ← MeasureClock.getMonotonicTime
    r ← act
    t1 ← MeasureClock.getMonotonicTime
    MeasureIO.hPutStrLn MeasureIO.stdout $ "@@ENGINE " ⧺ label ⧺ " "
        ⧺ show (t1 - t0) ⧺ " @@END"
    pure r
"""


def instrument_harness(text: str) -> str:
    if "measureTimed" in text:
        raise MeasureError("Harness.hs is already instrumented")
    for old, new in HARNESS_EDITS:
        if text.count(old) != 1:
            raise MeasureError(f"Harness.hs: anchor not found exactly once: "
                               f"{old.splitlines()[0]!r}")
        text = text.replace(old, new, 1)
    return text + HARNESS_HELPER


def run_instrument(args: argparse.Namespace) -> int:
    root = Path(args.root)
    spec, harness = root / SPEC, root / HARNESS
    new_spec = instrument_spec(spec.read_text(encoding="utf-8"),
                               args.full_tier)
    new_harness = instrument_harness(harness.read_text(encoding="utf-8"))
    spec.write_text(new_spec, encoding="utf-8")
    harness.write_text(new_harness, encoding="utf-8")
    print(f"instrumented {spec} and {harness}"
          + (" (full tier forced)" if args.full_tier else ""))
    return 0


# --------------------------------------------------------------------------
# Haskell `Show` output

_ASCII_NAMES = {
    "NUL": 0, "SOH": 1, "STX": 2, "ETX": 3, "EOT": 4, "ENQ": 5, "ACK": 6,
    "BEL": 7, "BS": 8, "HT": 9, "LF": 10, "VT": 11, "FF": 12, "CR": 13,
    "SO": 14, "SI": 15, "DLE": 16, "DC1": 17, "DC2": 18, "DC3": 19,
    "DC4": 20, "NAK": 21, "SYN": 22, "ETB": 23, "CAN": 24, "EM": 25,
    "SUB": 26, "ESC": 27, "FS": 28, "GS": 29, "RS": 30, "US": 31, "SP": 32,
    "DEL": 127,
}
_SIMPLE = {"a": "\a", "b": "\b", "f": "\f", "n": "\n", "r": "\r",
           "t": "\t", "v": "\v", "\\": "\\", '"': '"', "'": "'"}


def read_hs_string(text: str, pos: int) -> tuple[str, int]:
    """Decode the Haskell string literal starting at text[pos] == '"'."""
    if text[pos] != '"':
        raise MeasureError(f"expected a string literal at {pos}")
    out: list[str] = []
    i = pos + 1
    while True:
        if i >= len(text):
            raise MeasureError("unterminated string literal")
        c = text[i]
        if c == '"':
            return "".join(out), i + 1
        if c != "\\":
            out.append(c)
            i += 1
            continue
        i += 1
        c = text[i]
        if c in _SIMPLE:
            out.append(_SIMPLE[c])
            i += 1
        elif c == "&":
            i += 1
        elif c.isdigit():
            m = re.match(r"\d+", text[i:])
            out.append(chr(int(m.group())))
            i += m.end()
        elif c == "x":
            m = re.match(r"[0-9a-fA-F]+", text[i + 1:])
            out.append(chr(int(m.group(), 16)))
            i += 1 + m.end()
        elif c == "o":
            m = re.match(r"[0-7]+", text[i + 1:])
            out.append(chr(int(m.group(), 8)))
            i += 1 + m.end()
        elif c == "^":
            out.append(chr(ord(text[i + 1]) - 64))
            i += 2
        else:
            for name in sorted(_ASCII_NAMES, key=len, reverse=True):
                if text.startswith(name, i):
                    out.append(chr(_ASCII_NAMES[name]))
                    i += len(name)
                    break
            else:
                raise MeasureError(f"unknown escape \\{c} at {i}")


@dataclass
class ItemRecord:
    groups: list[str]          # the ["@G..", "@S.."] prefix
    path: list[str]            # the rest of the describe path
    description: str
    file: str | None
    line: int | None
    seconds: float
    status: str                # Success / Pending / Failure
    order: int = 0

    @property
    def group(self) -> str:
        return self.groups[0]

    @property
    def subgroup(self) -> str | None:
        return self.groups[1] if len(self.groups) > 1 else None

    @property
    def location(self) -> str:
        return f"{self.file}:{self.line}" if self.file else "?"

    @property
    def full_path(self) -> str:
        return " / ".join(self.path + [self.description])


_ITEM_HEAD = re.compile(r"ItemDone \(\[")
_LOCATION = re.compile(r'itemLocation = (Nothing|Just \(Location '
                       r'\{locationFile = ("(?:[^"\\]|\\.)*"), '
                       r'locationLine = (\d+), locationColumn = \d+\}\))')
_DURATION = re.compile(r"itemDuration = Seconds (-?[0-9.e+-]+),")
_RESULT = re.compile(r"itemResult = (Success|Pending|Failure)")


def parse_item(record: str, order: int) -> ItemRecord:
    m = _ITEM_HEAD.match(record)
    if not m:
        raise MeasureError(f"not an ItemDone record: {record[:80]!r}")
    i = m.end()
    parts: list[str] = []
    while record[i] != "]":
        s, i = read_hs_string(record, i)
        parts.append(s)
        if record[i] == ",":
            i += 1
    if record[i:i + 2] != "],":
        raise MeasureError(f"malformed path in {record[:120]!r}")
    description, i = read_hs_string(record, i + 2)
    rest = record[i:]
    loc = _LOCATION.search(rest)
    dur = _DURATION.search(rest)
    res = _RESULT.search(rest)
    if not (loc and dur and res):
        raise MeasureError(f"malformed item fields in {record[:200]!r}")
    file = line = None
    if loc.group(1) != "Nothing":
        file = read_hs_string(loc.group(2), 0)[0]
        line = int(loc.group(3))
    # @G is always the outermost component; @S sits inside its top-level
    # statement, below any describe that statement's head itself opens.
    if not parts or not re.fullmatch(r"@G\d+", parts[0]):
        raise MeasureError(f"item outside any @G group: {parts}")
    groups = [parts.pop(0)]
    subs = [k for k, p in enumerate(parts) if re.fullmatch(r"@S\d+", p)]
    if len(subs) > 1:
        raise MeasureError(f"item in more than one @S group: {parts}")
    if subs:
        groups.append(parts.pop(subs[0]))
    return ItemRecord(groups, parts, description, file, line,
                      float(dur.group(1)), res.group(1), order)


# --------------------------------------------------------------------------
# job logs

_TS = re.compile(r"^\d{4}-\d\d-\d\dT[0-9:.]+Z ")
_FINISHED = re.compile(r"Finished in ([0-9.]+) seconds")
_FOOTER = re.compile(r"(\d+) examples?, (\d+) failures?(?:, (\d+) pending)?")
_WORLD = re.compile(r"@@SHARED-WORLD-GEN \((\d+),(\d+),(\d+)\) "
                    r"([0-9.e+-]+) @@END")
_HIT = re.compile(r"@@SHARED-WORLD-HIT \((\d+),(\d+),(\d+)\) @@END")
_ENGINE = re.compile(r"@@ENGINE (boot|teardown) (world|noworld) "
                     r"([0-9.e+-]+) @@END")


@dataclass
class WorldGen:
    key: tuple[int, int, int]
    seconds: float
    item_order: int            # the ItemDone that follows it


@dataclass
class RunData:
    run_id: int
    attempt: int
    head_sha: str
    event: str
    conclusion: str
    job_id: int
    step_seconds: float | None
    job_seconds: float | None
    finished: float
    examples: int
    failures: int
    pending: int
    items: list[ItemRecord]
    worlds: list[WorldGen]
    hits: list[tuple[tuple[int, int, int], int]]
    engine: dict[str, list[float]] = field(default_factory=dict)
    image: str | None = None
    caches: list[str] = field(default_factory=list)
    full_tier: bool = False


def parse_log(text: str) -> dict:
    items: list[ItemRecord] = []
    worlds: list[WorldGen] = []
    engine: dict[str, list[float]] = {}
    pending_worlds: list[tuple[tuple[int, int, int], float]] = []
    hits: list[tuple[tuple[int, int, int], int]] = []
    pending_hits: list[tuple[int, int, int]] = []
    finished = footer = None
    image = None
    caches: list[str] = []
    full_tier = False
    for raw in text.splitlines():
        line = _TS.sub("", raw, count=1)
        k = line.find(MARK)
        if k >= 0:
            body = line[k + len(MARK):]
            if not body.endswith(END):
                raise MeasureError(f"truncated item record: {body[:120]!r}")
            item = parse_item(body[:-len(END)], len(items))
            items.append(item)
            for key, secs in pending_worlds:
                worlds.append(WorldGen(key, secs, item.order))
            hits.extend((key, item.order) for key in pending_hits)
            pending_worlds, pending_hits = [], []
            continue
        m = _HIT.search(line)
        if m:
            pending_hits.append(tuple(int(m.group(n)) for n in (1, 2, 3)))
            continue
        m = _WORLD.search(line)
        if m:
            key = tuple(int(m.group(n)) for n in (1, 2, 3))
            pending_worlds.append((key, float(m.group(4))))
            continue
        m = _ENGINE.search(line)
        if m:
            engine.setdefault(f"{m.group(1)} {m.group(2)}", []).append(
                float(m.group(3)))
            continue
        m = _FINISHED.search(line)
        if m:
            finished = float(m.group(1))
            continue
        m = _FOOTER.search(line)
        if m and finished is not None and footer is None:
            footer = (int(m.group(1)), int(m.group(2)),
                      int(m.group(3) or 0))
            continue
        if "CI_CACHE_REPORT" in line:
            caches.append(line[line.find("CI_CACHE_REPORT"):].strip())
        if "running the full tier (SYNARCHY_FULL_TESTS=1)" in line:
            full_tier = True
        m = re.search(r"(ghcr\.io/coghex/synarchy-ci:[\w.-]+)", line)
        if m and image is None:
            image = m.group(1)
    if finished is None or footer is None:
        raise MeasureError("no Hspec `Finished in` footer in the log")
    if pending_worlds or pending_hits:
        raise MeasureError("a shared-world record follows the last item")
    return dict(items=items, worlds=worlds, hits=hits, engine=engine,
                finished=finished, examples=footer[0], failures=footer[1],
                pending=footer[2], image=image, caches=caches,
                full_tier=full_tier)


def gh(args: list[str]) -> str:
    proc = subprocess.run(["gh", *args], capture_output=True, text=True)
    if proc.returncode != 0:
        raise MeasureError(f"gh {' '.join(args)}: {proc.stderr.strip()}")
    return proc.stdout


def _seconds(start: str | None, end: str | None) -> float | None:
    from datetime import datetime
    if not start or not end:
        return None
    f = "%Y-%m-%dT%H:%M:%SZ"
    return (datetime.strptime(end, f) - datetime.strptime(start, f)) \
        .total_seconds()


def fetch_run(repo: str, run_id: int, attempt: int | None) -> RunData:
    meta = json.loads(gh(["api", f"repos/{repo}/actions/runs/{run_id}"]))
    attempt = attempt or meta["run_attempt"]
    jobs = json.loads(gh(["api", f"repos/{repo}/actions/runs/{run_id}"
                          f"/attempts/{attempt}/jobs?per_page=100"]))["jobs"]
    job = next((j for j in jobs if j["name"] == CI_JOB), None)
    if job is None:
        raise MeasureError(f"run {run_id}: no {CI_JOB} job")
    step = next((s for s in job["steps"] if s["name"] == HEADLESS_STEP),
                None)
    if step is None or step["conclusion"] != "success":
        raise MeasureError(f"run {run_id}: {HEADLESS_STEP} did not succeed")
    log = gh(["api", f"repos/{repo}/actions/jobs/{job['id']}/logs"])
    parsed = parse_log(log)
    return RunData(run_id=run_id, attempt=attempt, head_sha=meta["head_sha"],
                   event=meta["event"], conclusion=meta["conclusion"] or "",
                   job_id=job["id"],
                   step_seconds=_seconds(step["started_at"],
                                         step["completed_at"]),
                   job_seconds=_seconds(job["started_at"],
                                        job["completed_at"]),
                   **parsed)


# --------------------------------------------------------------------------
# Spec.hs groups at the measured commit

@dataclass
class Group:
    label: str                 # "@G463"
    line: int
    text: str                  # the statement's first code line, stripped
    members: list["Group"] = field(default_factory=list)


def spec_groups(spec_text: str) -> list[Group]:
    head, sep, body = spec_text.partition(MAIN_HEAD)
    if not sep:
        raise MeasureError("Spec.hs: `main = hspec $ do` not found")
    lines, offset, hi = _body_lines(head, body)
    first = next(i for i in range(offset, hi) if _is_code(lines[i], 4))
    groups: list[Group] = []
    for start, end in _statements(lines, first, hi, 4):
        g = Group(f"@G{start + 1}", start + 1, lines[start].strip())
        if lines[start].rstrip().endswith("$ do"):
            first_inner, depth = _block_members(lines, start, end)
            for s, _e in _statements(lines, first_inner, end, depth):
                g.members.append(Group(f"@S{s + 1}", s + 1,
                                       lines[s].strip()))
        groups.append(g)
    return groups


# --------------------------------------------------------------------------
# self-test

def self_test() -> int:
    s, _ = read_hs_string('"a\\8594b\\\\c\\"d\\&1\\SOH"', 0)
    assert s == "a→b\\c\"d1\x01", s
    rec = ('ItemDone (["@G463","@S464","World Generation","x/y"],"it \\"q\\"") '
           '(Item {itemLocation = Just (Location {locationFile = '
           '"test-headless/Test/Headless/WorldGen.hs", locationLine = 79, '
           'locationColumn = 9}), itemDuration = Seconds 1.25e-3, itemInfo = "", '
           'itemResult = Success})')
    item = parse_item(rec, 0)
    assert item.groups == ["@G463", "@S464"], item.groups
    assert item.path == ["World Generation", "x/y"], item.path
    assert item.description == 'it "q"', item.description
    assert item.line == 79 and abs(item.seconds - 1.25e-3) < 1e-12
    spec = ("module Main where\nimport Test.Hspec\n\n" + MAIN_HEAD +
            "    describe \"A\" A.spec\n"
            "    -- comment\n"
            "    aroundAll w $ do\n"
            "        describe \"B\" B.spec\n"
            "        C.spec\n"
            "    aroundAll w $\n"
            "        D.spec\n")
    out = instrument_spec(spec, False)
    assert '    describe "@G6" $ do\n        describe "A" A.spec' in out, out
    assert '            describe "@S9" $ do\n' \
           '                describe "B" B.spec' in out, out
    assert '            describe "@S10" $ do\n                C.spec' in out
    assert '    describe "@G11" $ do\n        aroundAll w $\n' \
           '            D.spec' in out, out
    groups = spec_groups(spec)
    assert [g.label for g in groups] == ["@G6", "@G8", "@G11"], groups
    assert [m.label for m in groups[1].members] == ["@S9", "@S10"]
    log = ("2026-09-29T10:00:00.0000000Z @@SHARED-WORLD-GEN (42,64,3) "
           "9.5 @@END\n"
           "2026-09-29T10:00:00.5000000Z @@SHARED-WORLD-HIT (42,64,3) @@END\n"
           f"2026-09-29T10:00:01.0000000Z {MARK}{rec}{END}\n"
           "Finished in 12.5000 seconds\n1 example, 0 failures\n")
    parsed = parse_log(log)
    assert parsed["worlds"][0].key == (42, 64, 3)
    assert parsed["worlds"][0].item_order == 0
    assert parsed["hits"] == [((42, 64, 3), 0)], parsed["hits"]
    assert parsed["finished"] == 12.5 and parsed["examples"] == 1
    print("headless_lane_measurement self-test: OK")
    return 0


# --------------------------------------------------------------------------

def main(argv: list[str] | None = None) -> int:
    parser = argparse.ArgumentParser(description=__doc__.split("\n")[0])
    sub = parser.add_subparsers(dest="cmd", required=True)
    p = sub.add_parser("instrument", help="rewrite a checkout for collection")
    p.add_argument("--root", default=".")
    p.add_argument("--full-tier", action="store_true")
    p = sub.add_parser("aggregate", help="tabulate measured runs")
    p.add_argument("--repo", default="coghex/synarchy")
    p.add_argument("--run", action="append", default=[], metavar="ID[:ATTEMPT]",
                   help="an ordinary-tier measurement run")
    p.add_argument("--full-run", action="append", default=[],
                   metavar="ID[:ATTEMPT]",
                   help="a full-tier measurement run")
    p.add_argument("--spec-commit", required=True,
                   help="the commit whose Spec.hs the @G/@S lines index")
    p.add_argument("--json", action="store_true")
    sub.add_parser("self-test")
    args = parser.parse_args(argv)
    try:
        if args.cmd == "instrument":
            return run_instrument(args)
        if args.cmd == "self-test":
            return self_test()
        from headless_lane_report import run_aggregate  # noqa: E402
        return run_aggregate(args)
    except MeasureError as error:
        print(f"headless_lane_measurement: {error}", file=sys.stderr)
        return 1


if __name__ == "__main__":
    sys.exit(main())
