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
      an already-generated one, and ``@@ENGINE boot|teardown
      world|noworld`` around each headless engine's setup and teardown,
      each with its own monotonic duration. Every such record belongs to
      the example whose ``ItemDone`` follows it.

    ``--full-tier`` additionally sets ``SYNARCHY_FULL_TESTS=1`` from
    `main` itself, so a full-tier collection needs no workflow edit.
    ``--lane-match`` / ``--lane-skip`` prepend Hspec ``--match`` /
    ``--skip`` filters for the named top-level groups, so one proposed
    lane can be timed alone in its own process.

``aggregate``
    Downloads each named run's `test-and-audits` job log with `gh`,
    parses those records plus Hspec's footer, and prints the report's
    tables as Markdown. It refuses a run whose item records disagree with
    the footer, whose examples sit outside a `Spec.hs` statement at
    ``--spec-commit``, or (for ordinary runs) whose example order differs
    from the first run's. ``--baseline-run`` reads only job timings and
    the `Finished in` line of unmodified runs; ``--master-run`` reads the
    twenty-item slow list existing master logs already carry.
"""

from __future__ import annotations

import argparse
import json
import re
import statistics
import subprocess
import sys
from collections import Counter
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
        {lane_args}=≪ MeasureEnv.getArgs
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


def instrument_spec(text: str, full_tier: bool,
                    lane: tuple[str, list[str]] | None = None) -> str:
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
    lane_args = ""
    if lane:
        flag, labels = lane
        lane_args = "∘ ([" + ", ".join(
            f'"--{flag}", "/{label}/"' for label in labels) + "] ⧺) "
    main = INSTRUMENTED_MAIN.format(full_tier=full, mark=MARK, end=END,
                                    lane_args=lane_args)
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
    lane = None
    if args.lane_match or args.lane_skip:
        if args.lane_match and args.lane_skip:
            raise MeasureError("--lane-match and --lane-skip are exclusive")
        flag = "match" if args.lane_match else "skip"
        labels = (args.lane_match or args.lane_skip).split(",")
        if not all(re.fullmatch(r"@G\d+", x) for x in labels):
            raise MeasureError(f"lane labels must be @G<line>: {labels}")
        lane = (flag, labels)
    new_spec = instrument_spec(spec.read_text(encoding="utf-8"),
                               args.full_tier, lane)
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
_CACHE = re.compile(r"CI_CACHE_REPORT cache=(\S+) outcome=(\S+)")
_IMAGE = re.compile(r"(ghcr\.io/coghex/synarchy-ci:ci-[0-9a-f]+)(?![-\w])")
_FLAGS = re.compile(r"cabal test synarchy-test-headless -v0 "
                    r"--test-show-details=direct --test-options='([^']*)'")
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
    engine: list[tuple[str, float, int]] = field(default_factory=list)
    image: str | None = None
    caches: list[str] = field(default_factory=list)
    full_tier: bool = False
    flags: str | None = None
    setup_seconds: float | None = None
    wall_seconds: float | None = None


def parse_log(text: str) -> dict:
    items: list[ItemRecord] = []
    worlds: list[WorldGen] = []
    engine: list[tuple[str, float, int]] = []
    pending_engine: list[tuple[str, float]] = []
    pending_worlds: list[tuple[tuple[int, int, int], float]] = []
    hits: list[tuple[tuple[int, int, int], int]] = []
    pending_hits: list[tuple[int, int, int]] = []
    finished = footer = None
    image = None
    caches: list[str] = []
    full_tier = False
    flags = None
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
            engine.extend((kind, secs, item.order)
                          for kind, secs in pending_engine)
            pending_worlds, pending_hits, pending_engine = [], [], []
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
            pending_engine.append((f"{m.group(1)} {m.group(2)}",
                                   float(m.group(3))))
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
        m = _CACHE.search(line)
        if m:
            caches.append(f"{m.group(1)}={m.group(2)}")
        if "running the full tier (SYNARCHY_FULL_TESTS=1)" in line:
            full_tier = True
        m = _IMAGE.search(line)
        if m and image is None:
            image = m.group(1)
        m = _FLAGS.search(line)
        if m and flags is None:
            flags = m.group(1)
    if finished is None or footer is None:
        raise MeasureError("no Hspec `Finished in` footer in the log")
    if pending_worlds or pending_hits or pending_engine:
        raise MeasureError("a harness record follows the last item")
    return dict(items=items, worlds=worlds, hits=hits, engine=engine,
                finished=finished, examples=footer[0], failures=footer[1],
                pending=footer[2], image=image, caches=caches,
                full_tier=full_tier, flags=flags)


def gh(args: list[str]) -> str:
    proc = subprocess.run(["gh", *args], capture_output=True, text=True)
    if proc.returncode != 0:
        raise MeasureError(f"gh {' '.join(args)}: {proc.stderr.strip()}")
    return proc.stdout


def gh_cached(path: str, cache: Path | None) -> str:
    """`gh api <path>`, memoized under `cache`.

    Callers pass only completed attempts' jobs and completed jobs' logs,
    which GitHub never changes afterwards.
    """
    target = cache / (re.sub(r"[^\w.-]+", "_", path) + ".txt") \
        if cache else None
    if target and target.exists():
        return target.read_text(encoding="utf-8")
    text = gh(["api", path])
    if target:
        target.parent.mkdir(parents=True, exist_ok=True)
        target.write_text(text, encoding="utf-8")
    return text


def _stamp(text: str | None):
    from datetime import datetime, timezone
    if not text:
        return None
    return datetime.strptime(text, "%Y-%m-%dT%H:%M:%SZ") \
        .replace(tzinfo=timezone.utc)


def _seconds(start: str | None, end: str | None) -> float | None:
    a, b = _stamp(start), _stamp(end)
    return (b - a).total_seconds() if a and b else None


def parse_run_ref(text: str) -> tuple[int, int | None]:
    run, _, attempt = text.partition(":")
    return int(run), (int(attempt) if attempt else None)


def fetch_jobs(repo: str, run_id: int, attempt: int | None,
               cache: Path | None) -> tuple[dict, list[dict]]:
    """A run attempt's metadata and jobs (the latest attempt if None).

    Metadata is always fetched live; jobs are memoized only once the
    attempt has completed, so a cache never freezes a run mid-flight.
    """
    meta = json.loads(gh(["api", f"repos/{repo}/actions/runs/{run_id}"
                          + (f"/attempts/{attempt}" if attempt else "")]))
    if meta["status"] != "completed":
        raise MeasureError(f"run {run_id} is still {meta['status']}")
    attempt = attempt or meta["run_attempt"]
    jobs = json.loads(gh_cached(f"repos/{repo}/actions/runs/{run_id}"
                                f"/attempts/{attempt}/jobs?per_page=100",
                                cache))["jobs"]
    return meta, jobs


def headless_job(run_id: int, jobs: list[dict]) -> tuple[dict, dict]:
    job = next((j for j in jobs if j["name"] == CI_JOB), None)
    if job is None:
        raise MeasureError(f"run {run_id}: no {CI_JOB} job")
    step = next((s for s in job["steps"] if s["name"] == HEADLESS_STEP),
                None)
    if step is None or step["conclusion"] != "success":
        raise MeasureError(f"run {run_id}: {HEADLESS_STEP} did not succeed")
    return job, step


def fetch_run(repo: str, ref: str, cache: Path | None) -> RunData:
    run_id, attempt = parse_run_ref(ref)
    meta, jobs = fetch_jobs(repo, run_id, attempt, cache)
    job, step = headless_job(run_id, jobs)
    log = gh_cached(f"repos/{repo}/actions/jobs/{job['id']}/logs", cache)
    parsed = parse_log(log)
    return RunData(run_id=run_id, attempt=meta["run_attempt"],
                   head_sha=meta["head_sha"], event=meta["event"],
                   conclusion=meta["conclusion"] or "", job_id=job["id"],
                   step_seconds=_seconds(step["started_at"],
                                         step["completed_at"]),
                   job_seconds=_seconds(job["started_at"],
                                        job["completed_at"]),
                   setup_seconds=_seconds(job["started_at"],
                                          step["started_at"]),
                   wall_seconds=max(
                       _seconds(meta["run_started_at"], j["completed_at"])
                       for j in jobs
                       if j["completed_at"] and j["conclusion"] != "skipped"),
                   **parsed)


_SLOW = re.compile(r"^\s+(\S+:\d+):\d+: /(.*)/ \((\d+)ms\)$")


def slow_items(text: str) -> tuple[float, list[tuple[str, str, int]]]:
    """`Finished in` and the `--print-slow-items` list of an unmodified log."""
    finished, out, inside = None, [], False
    for raw in text.splitlines():
        line = _TS.sub("", raw, count=1)
        if line.startswith("Slow spec items:"):
            inside = True
            continue
        m = _FINISHED.search(line)
        if m:
            finished = float(m.group(1))
        # stderr (the list) and stdout (the footer) interleave, so the
        # list's lines are recognised by shape, not by adjacency.
        if inside:
            m = _SLOW.match(line)
            if m:
                out.append((m.group(1), m.group(2), int(m.group(3))))
    if finished is None:
        raise MeasureError("no `Finished in` footer in the log")
    return finished, out


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
# aggregation

SHARED_BLOCK_TEXT = "aroundAll withHeadlessEngine $ do"
TARGET_RUN_SECONDS = 1200.0


def _mean(xs):
    return sum(xs) / len(xs) if xs else 0.0


def _fmt(x: float | None, digits: int = 1) -> str:
    return "—" if x is None else f"{x:.{digits}f}"


def check_identical(runs: list[RunData]) -> None:
    """Every run must have run the same examples in the same order."""
    ref = [(i.groups, i.path, i.description) for i in runs[0].items]
    for run in runs[1:]:
        got = [(i.groups, i.path, i.description) for i in run.items]
        if got != ref:
            raise MeasureError(f"run {run.run_id} did not run the same "
                               f"examples in the same order as "
                               f"{runs[0].run_id}")


def accounting(run: RunData) -> dict:
    total = sum(i.seconds for i in run.items)
    pending = sum(1 for i in run.items if i.status == "Pending")
    failed = sum(1 for i in run.items if i.status == "Failure")
    if len(run.items) != run.examples:
        raise MeasureError(f"run {run.run_id}: {len(run.items)} item "
                           f"records against a footer of {run.examples} "
                           "examples")
    if pending != run.pending or failed != run.failures:
        raise MeasureError(f"run {run.run_id}: pending/failure records "
                           "disagree with the footer")
    return dict(items=total, residual=run.finished - total,
                residual_pct=100 * (run.finished - total) / run.finished,
                pending=pending,
                wrapper=(run.step_seconds - run.finished
                         if run.step_seconds is not None else None))


def group_times(run: RunData) -> dict[str, float]:
    out: dict[str, float] = {}
    for i in run.items:
        out[i.group] = out.get(i.group, 0.0) + i.seconds
        if i.subgroup:
            key = f"{i.group}{i.subgroup}"
            out[key] = out.get(key, 0.0) + i.seconds
    return out


def group_counts(run: RunData) -> dict[str, int]:
    out: dict[str, int] = {}
    for i in run.items:
        out[i.group] = out.get(i.group, 0) + 1
        if i.subgroup:
            key = f"{i.group}{i.subgroup}"
            out[key] = out.get(key, 0) + 1
    return out


def shared_block(groups: list[Group]) -> Group:
    blocks = [g for g in groups if g.text == SHARED_BLOCK_TEXT and g.members
              and any("WorldGen.spec" in m.text for m in g.members)]
    if len(blocks) != 1:
        raise MeasureError("cannot identify the one shared-world block")
    return blocks[0]


def world_keys(runs: list[RunData]) -> dict:
    """Per key: generating example and every consuming (sub)group."""
    keys: dict[tuple, dict] = {}
    for run in runs:
        for w in run.worlds:
            item = run.items[w.item_order]
            entry = keys.setdefault(w.key, dict(
                generator=None, gen_seconds=[], gen_item_seconds=[],
                consumers=set(), generators=set()))
            entry["generator"] = entry["generator"] or item
            entry["generators"].add((item.group, item.subgroup,
                                     item.full_path))
            entry["gen_seconds"].append(w.seconds)
            entry["gen_item_seconds"].append(item.seconds)
            entry["consumers"].add((item.group, item.subgroup))
        for key, order in run.hits:
            item = run.items[order]
            keys.setdefault(key, dict(generator=None, gen_seconds=[],
                                      gen_item_seconds=[], consumers=set(),
                                      generators=set()))
            keys[key]["consumers"].add((item.group, item.subgroup))
    for key, entry in keys.items():
        if len(entry["generators"]) != 1:
            raise MeasureError(f"key {key}: generated by "
                               f"{sorted(entry['generators'])} across runs")
    return keys


def affinity_components(block: Group, keys: dict) -> list[dict]:
    """Shared-block members joined transitively through a common key."""
    parent = {m.label: m.label for m in block.members}

    def find(x):
        while parent[x] != x:
            parent[x] = parent[parent[x]]
            x = parent[x]
        return x
    key_members: dict[tuple, set[str]] = {}
    for key, entry in keys.items():
        members = {sub for grp, sub in entry["consumers"]
                   if grp == block.label and sub}
        outside = {grp for grp, sub in entry["consumers"]
                   if grp != block.label}
        if outside:
            raise MeasureError(f"key {key} is consumed outside the shared "
                               f"block: {sorted(outside)}")
        key_members[key] = members
        members = sorted(members)
        for other in members[1:]:
            parent[find(other)] = find(members[0])
    comps: dict[str, dict] = {}
    for m in block.members:
        root = find(m.label)
        comps.setdefault(root, dict(members=[], keys=set()))
        comps[root]["members"].append(m.label)
    for key, members in key_members.items():
        for sub in members:
            comps[find(sub)]["keys"].add(key)
    return sorted(comps.values(), key=lambda c: c["members"][0])


def contiguous(costs: list[float], lanes: int) -> list[tuple[int, int]]:
    """Split `costs` (in order) into `lanes` ranges minimising the largest.

    Exact dynamic programme over prefix sums; returns [lo, hi) ranges.
    """
    n = len(costs)
    prefix = [0.0]
    for c in costs:
        prefix.append(prefix[-1] + c)
    inf = float("inf")
    best = [[inf] * (n + 1) for _ in range(lanes + 1)]
    cut = [[0] * (n + 1) for _ in range(lanes + 1)]
    best[0][0] = 0.0
    for k in range(1, lanes + 1):
        for j in range(1, n + 1):
            for i in range(k - 1, j):
                v = max(best[k - 1][i], prefix[j] - prefix[i])
                if v < best[k][j]:
                    best[k][j], cut[k][j] = v, i
    ranges, j = [], n
    for k in range(lanes, 0, -1):
        i = cut[k][j]
        ranges.append((i, j))
        j = i
    return list(reversed(ranges))


def baseline_overhead(repo: str, refs: list[str], cache: Path | None) -> dict:
    """Median lane-job overhead from ordinary successful PR runs."""
    rows, skipped = [], []
    for ref in refs:
        run_id, attempt = parse_run_ref(ref)
        meta, jobs = fetch_jobs(repo, run_id, attempt, cache)
        if meta["conclusion"] != "success" or meta["event"] != "pull_request":
            raise MeasureError(f"baseline run {run_id} is not a successful "
                               "pull_request run")
        if meta["run_attempt"] != 1:
            # A rerun attempt carries the earlier attempt's untouched jobs
            # with their original timestamps, so offsets from this
            # attempt's start are meaningless.
            skipped.append(run_id)
            continue
        job, step = headless_job(run_id, jobs)
        log = gh_cached(f"repos/{repo}/actions/jobs/{job['id']}/logs", cache)
        finished, _ = slow_items(log)
        start = meta["run_started_at"]
        ends = {j["name"]: _seconds(start, j["completed_at"]) for j in jobs
                if j["completed_at"] and j["conclusion"] != "skipped"}
        rows.append(dict(
            run=run_id,
            wall=max(ends.values()),
            pre=_seconds(start, job["started_at"]),
            setup_build=_seconds(job["started_at"], step["started_at"]),
            step=_seconds(step["started_at"], step["completed_at"]),
            finished=finished,
            wrapper=_seconds(step["started_at"], step["completed_at"])
            - finished,
            post=_seconds(step["completed_at"], job["completed_at"]),
            tail=max(ends.values()) - _seconds(start, job["completed_at"]),
            probes=ends.get("behavior-probes"),
            static=ends.get("static-audits")))
    med = {k: statistics.median(r[k] for r in rows if r[k] is not None)
           for k in rows[0] if k != "run"}
    return dict(rows=rows, median=med, skipped=skipped)


# --------------------------------------------------------------------------
# report

def _cell(text: str, width: int = 64) -> str:
    text = text if len(text) <= width else text[:width - 1] + "…"
    return "`" + text.replace("|", "\\|").replace("`", "'") + "`"


def _quantile(xs: list[float], q: float) -> float:
    """R-7 (linear interpolation), the estimator ci_timing_report uses."""
    xs = sorted(xs)
    h = (len(xs) - 1) * q
    lo = int(h)
    return xs[lo] + (h - lo) * (xs[min(lo + 1, len(xs) - 1)] - xs[lo])


def _ms(xs: list[float]) -> str:
    """One cell: mean (min–max)."""
    return (f"{_fmt(_mean(xs))} ({_fmt(min(xs))}–{_fmt(max(xs))})"
            if xs else "—")


def rid(run: RunData) -> str:
    return f"`{run.run_id}#{run.attempt}`"


def _stats(xs: list[float]) -> str:
    return (f"{_fmt(_mean(xs))} | {_fmt(min(xs))}–{_fmt(max(xs))}"
            if xs else "— | —")


def render(args, runs: list[RunData], full: list[RunData],
           groups: list[Group], base: dict | None,
           masters: list[tuple[int, float, list]],
           lanes: list[RunData] = ()) -> list[str]:
    out: list[str] = []
    w = out.append
    labels = {g.label: g for g in groups}
    for g in groups:
        for m in g.members:
            labels[f"{g.label}{m.label}"] = m
    block = shared_block(groups)
    acc = [accounting(r) for r in runs]
    times = [group_times(r) for r in runs]
    counts = group_counts(runs[0])
    for key in counts:
        if key not in labels:
            raise MeasureError(f"measured group {key} is not a statement of "
                               f"Spec.hs at {args.spec_commit}")
    ids = ", ".join(rid(r) for r in runs)

    w("## Measured runs\n")
    w("| Run | Attempt | Tested SHA | Image | Tier | Caches | GitHub step s "
      "| `Finished in` s | Step − Finished s | Examples | Pending | Σ items s "
      "| Unattributed s | % |")
    w("|---|---|---|---|---|---|---|---|---|---|---|---|---|---|")
    for r, a in zip(runs + full, [*acc, *[accounting(f) for f in full]]):
        tier = "full" if r in full else "ordinary"
        w(f"| `{r.run_id}` | {r.attempt} | `{r.head_sha[:9]}` "
          f"| `{(r.image or '?').split(':')[-1]}` | {tier} "
          f"| {', '.join(r.caches) or '—'} | {_fmt(r.step_seconds, 0)} "
          f"| {r.finished:.1f} | {_fmt(a['wrapper'])} | {r.examples} "
          f"| {a['pending']} | {a['items']:.1f} | {a['residual']:.2f} "
          f"| {a['residual_pct']:.2f} |")
    flags = {r.flags for r in runs + full}
    w("")
    w("Collection flags (Hspec `--test-options`): "
      + ", ".join(f"`{f}`" for f in sorted(x or "?" for x in flags)) + ".\n")

    w("## Every top-level group of `test-headless/Spec.hs`\n")
    w(f"Seconds of example time per group, summed from every example's own "
      f"duration, for runs {ids}. The shared-world block is one group here "
      f"(its members follow in the next table). Lines are "
      f"`test-headless/Spec.hs` at `{args.spec_commit[:9]}`.\n")
    head = " | ".join(f"{rid(r)} s" for r in runs)
    w(f"| Group | Statement | Examples | {head} | Mean s | Spread s |")
    w("|---|---|---|" + "---|" * len(runs) + "---|---|")
    for g in groups:
        per = [t.get(g.label, 0.0) for t in times]
        name = f"**{g.label}** (subtotal)" if g is block else g.label
        w(f"| {name} | {_cell(g.text)} | {counts.get(g.label, 0)} | "
          + " | ".join(f"{x:.1f}" for x in per) + f" | {_stats(per)} |")
    tot = [sum(t.get(g.label, 0.0) for g in groups) for t in times]
    w(f"| **all groups** | | {len(runs[0].items)} | "
      + " | ".join(f"{x:.1f}" for x in tot) + f" | {_stats(tot)} |")
    w("")

    w("### Largest groups\n")
    w(f"| Group | Statement | Examples | Mean s | Spread s | Share of Σ |")
    w("|---|---|---|---|---|---|")
    ranked = sorted(groups, key=lambda g: -_mean([t.get(g.label, 0.0)
                                                   for t in times]))
    for g in ranked[:20]:
        per = [t.get(g.label, 0.0) for t in times]
        w(f"| {g.label} | {_cell(g.text)} | {counts.get(g.label, 0)} "
          f"| {_stats(per)} | {100 * _mean(per) / _mean(tot):.1f} % |")
    w("")

    w(f"## Shared-world block members (`Spec.hs:{block.line}`)\n")
    w(f"| Member | Statement | Examples | {head} | Mean s | Spread s |")
    w("|---|---|---|" + "---|" * len(runs) + "---|---|")
    for m in block.members:
        key = f"{block.label}{m.label}"
        per = [t.get(key, 0.0) for t in times]
        w(f"| {m.label} | {_cell(m.text)} | {counts.get(key, 0)} | "
          + " | ".join(f"{x:.1f}" for x in per) + f" | {_stats(per)} |")
    w("")

    keys = world_keys(runs)
    w("## Shared-world keys\n")
    w("| Key `(seed,size,plates)` | Generating example (ordinary run) "
      "| Location | Generation s, mean (min–max) | Generating example s, "
      "mean (min–max) | Consuming groups |")
    w("|---|---|---|---|---|---|")
    for key in sorted(keys):
        e = keys[key]
        gen = e["generator"]
        cons = sorted({f"{g}{s or ''}" for g, s in e["consumers"]})
        names = ", ".join(f"{c} {_cell(labels[c].text, 40)}" for c in cons)
        w(f"| `{key}` | {gen.subgroup or gen.group}: "
          f"{_cell(gen.full_path, 90)} | `{gen.location}` "
          f"| {_ms(e['gen_seconds'])} | {_ms(e['gen_item_seconds'])} "
          f"| {names} |")
    w("")

    comps = affinity_components(block, keys)
    w("### Key affinity inside the shared-world block\n")
    w("Members joined transitively through a common key must share a lane.\n")
    w("| Component | Keys | Members | Mean s |")
    w("|---|---|---|---|")
    comp_cost = []
    n = 0
    for c in comps:
        cost = _mean([sum(t.get(f"{block.label}{m}", 0.0)
                          for m in c["members"]) for t in times])
        comp_cost.append(cost)
        if c["keys"]:
            n += 1
            w(f"| {n} | {', '.join(f'`{k}`' for k in sorted(c['keys']))} "
              f"| {', '.join(c['members'])} | {cost:.1f} |")
    free = [c for c in comps if not c["keys"]]
    free_cost = sum(cost for c, cost in zip(comps, comp_cost)
                    if not c["keys"])
    w(f"| — | none | {len(free)} members that consume no key | "
      f"{free_cost:.1f} |")
    w("")

    w("## Engine setup and teardown\n")
    w("Harness timers around `withHeadlessEngine` / "
      "`withHeadlessEngineNoWorld` setup and teardown. Each is INSIDE some "
      "example's duration: hspec 2.11 charges an `aroundAll` acquire to the "
      "first example under it and its release to the last.\n")
    w("| Run | World boots | Σ s | No-world boots | Σ s | Teardowns Σ s "
      "| Shared-block boot s |")
    w("|---|---|---|---|---|---|---|")
    boot_block = []
    for r in runs:
        def pick(kind):
            return [s for k, s, _ in r.engine if k == kind]
        first = next(i.order for i in r.items if i.group == block.label)
        bb = [s for k, s, o in r.engine if k == "boot world" and o == first]
        boot_block.append(bb[0] if bb else 0.0)
        tear = pick("teardown world") + pick("teardown noworld")
        w(f"| {rid(r)} | {len(pick('boot world'))} "
          f"| {sum(pick('boot world')):.1f} | {len(pick('boot noworld'))} "
          f"| {sum(pick('boot noworld')):.1f} | {sum(tear):.1f} "
          f"| {boot_block[-1]:.2f} |")
    w("")

    if full:
        w("## Full tier\n")
        ordinary = {(tuple(i.groups), tuple(i.path), i.description): i
                    for i in runs[0].items}
        mean_by = {}
        for r in runs:
            for i in r.items:
                mean_by.setdefault((tuple(i.groups), tuple(i.path),
                                    i.description), []).append(i.seconds)
        w("| Run | Full-tier example | Location | s |")
        w("|---|---|---|---|")
        extra = []
        for f in full:
            for i in f.items:
                k = (tuple(i.groups), tuple(i.path), i.description)
                if ordinary[k].status == "Pending" and i.status != "Pending":
                    extra.append((f, i))
                    w(f"| {rid(f)} | {_cell(i.full_path, 90)} "
                      f"| `{i.location}` | {i.seconds:.1f} |")
        w("")
        fkeys = world_keys(full)
        w("| Key | Ordinary-tier generator | Full-tier generator |")
        w("|---|---|---|")
        for key in sorted(fkeys):
            o, fg = keys.get(key, {}).get("generator"), fkeys[key]["generator"]
            if o is None or o.full_path != fg.full_path:
                w(f"| `{key}` | {_cell(o.full_path, 70) if o else '—'} "
                  f"| {_cell(fg.full_path, 70)} |")
        w("")
        w("A full-tier run can land on a faster or slower runner than the "
          "ordinary runs, so raw per-example differences mix the tier's cost "
          "with runner speed. The comparison below is restricted to the "
          "examples the tier can affect -- the full-tier examples and every "
          "example that generated or reused a key whose generator moved -- "
          "and scales them by the runner-speed factor measured on every "
          "OTHER example.\n")
        for f in full:
            fk = world_keys([f])
            moved = {k for k in fk
                     if keys.get(k, {}).get("generator") is None
                     or keys[k]["generator"].full_path
                     != fk[k]["generator"].full_path}
            touched = {w_.item_order for w_ in f.worlds if w_.key in moved}
            touched |= {o for k, o in f.hits if k in moved}
            ordinary_touch = set()
            for r in runs:
                ordinary_touch |= {w_.item_order for w_ in r.worlds
                                   if w_.key in moved}
                ordinary_touch |= {o for k, o in r.hits if k in moved}
            affected = {(tuple(f.items[o].groups), tuple(f.items[o].path),
                         f.items[o].description) for o in touched}
            affected |= {(tuple(runs[0].items[o].groups),
                          tuple(runs[0].items[o].path),
                          runs[0].items[o].description)
                         for o in ordinary_touch}
            affected |= {(tuple(i.groups), tuple(i.path), i.description)
                         for _, i in extra if _ is f}
            base_other = full_other = 0.0
            rows = []
            for i in f.items:
                k = (tuple(i.groups), tuple(i.path), i.description)
                if k in affected:
                    rows.append((i, _mean(mean_by[k])))
                else:
                    base_other += _mean(mean_by[k])
                    full_other += i.seconds
            factor = full_other / base_other
            w(f"Runner-speed factor for {rid(f)} (Σ full / Σ ordinary mean "
              f"over the {len(f.items) - len(rows)} unaffected examples): "
              f"**{factor:.3f}**.\n")
            w("| Example | Location | Ordinary mean s | Full s | Full s at "
              "ordinary speed | Δ s |")
            w("|---|---|---|---|---|---|")
            net = 0.0
            for i, o in rows:
                scaled = i.seconds / factor
                net += scaled - o
                w(f"| {_cell(i.full_path, 80)} | `{i.location}` | {o:.1f} "
                  f"| {i.seconds:.1f} | {scaled:.1f} | {scaled - o:+.1f} |")
            w(f"| **incremental full-tier cost at ordinary speed** | | | | | "
              f"**{net:+.1f}** |")
            for key in sorted(moved):
                gens = [w_.seconds for w_ in f.worlds if w_.key == key]
                w("")
                w(f"`{key}` generation inside the full-tier generator: "
                  f"{gens[0]:.1f} s ({gens[0] / factor:.1f} s at ordinary "
                  f"speed; {_mean(keys[key]['gen_seconds']):.1f} s in the "
                  f"ordinary runs).")
            w("")
    if masters:
        w("### Existing master-push logs (full tier, twenty slowest only)\n")
        w("| Run | `Finished in` s | Full-tier volcano example s "
          "| MapPyramid worldSize-128 golden s |")
        w("|---|---|---|---|")
        for run_id, fin, items in masters:
            vol = [ms for loc, p, ms in items if "volcano" in p]
            w128 = [ms for loc, p, ms in items if "worldSize 128" in p]
            w(f"| `{run_id}` | {fin:.1f} "
              f"| {vol[0] / 1000:.1f} | "
              + (f"{w128[0] / 1000:.1f}" if w128 else "not listed")
              + " |")
        w("")

    if base:
        med = base["median"]
        res = _mean([a["residual"] for a in acc])
        fixed = med["pre"] + med["setup_build"] + med["wrapper"] + med["tail"]
        budget = TARGET_RUN_SECONDS - fixed
        w("## Lane budget\n")
        w(f"Medians over {len(base['rows'])} successful first-attempt "
          f"`pull_request` runs of the unmodified workflow: "
          + ", ".join(f"`{r['run']}`" for r in base["rows"])
          + (f". Excluded as rerun attempts: "
             + ", ".join(f"`{x}`" for x in base["skipped"])
             if base["skipped"] else "") + ".\n")
        w("| Component | Median s |")
        w("|---|---|")
        for k, label in [
                ("wall", "run wall time (start → last job end)"),
                ("pre", "run start → `test-and-audits` job start"),
                ("setup_build", "job start → `Headless test suite` start "
                 "(container, checkout, plan, caches, library + executable "
                 "+ test-suite build)"),
                ("step", "`Headless test suite` step"),
                ("finished", "Hspec `Finished in`"),
                ("wrapper", "step − `Finished in` (cabal wrapper)"),
                ("post", "steps after the suite (audits)"),
                ("tail", "`test-and-audits` end → last job end"),
                ("probes", "run start → `behavior-probes` end"),
                ("static", "run start → `static-audits` end")]:
            w(f"| {label} | {med[k]:.0f} |")
        w("")
        w(f"Per-lane fixed cost = pre + setup/build + wrapper + tail = "
          f"{med['pre']:.0f} + {med['setup_build']:.0f} + "
          f"{med['wrapper']:.0f} + {med['tail']:.0f} = **{fixed:.0f} s**, so "
          f"a lane's Hspec time plus any post-suite steps it carries must "
          f"stay under {TARGET_RUN_SECONDS:.0f} − {fixed:.0f} = "
          f"**{budget:.0f} s** for the run to finish in twenty minutes.\n")
        boot = _mean(boot_block)
        mean_of = {g.label: _mean([t.get(g.label, 0.0) for t in times])
                   for g in groups}
        max_of = {g.label: max(t.get(g.label, 0.0) for t in times)
                  for g in groups}
        post = med["post"]
        w(f"Every lane runs its own Hspec process, so each pays its own "
          f"unattributed residual (mean {res:.1f} s, max "
          f"{max(a['residual'] for a in acc):.1f} s here), counted in full "
          f"per lane. The post-suite audit steps ({post:.0f} s median) stay "
          f"in one job; they are placed on the least-loaded lane. A split "
          f"shared block would pay one more shared-engine boot per piece "
          f"(measured {boot:.2f} s mean) plus regenerating nothing, since "
          f"no key crosses a component boundary.\n")

        w("### Contiguous partitions\n")
        w("Each lane is one contiguous range of top-level statements in "
          "Spec.hs order, which is the cheapest selector for CIR-15 to "
          "implement and keeps the shared-world block (one statement) "
          "whole by construction. The range split is the exact minimum of "
          "the longest lane for that lane count.\n")
        w("| Lanes | Longest lane Hspec s (mean runs) | … (slowest run) "
          "| Predicted run s (mean) | … (slowest run) | ≤ 1200 s at "
          "median build |")
        w("|---|---|---|---|---|---|")
        order = [g.label for g in groups]
        plans = []
        for n in range(2, 7):
            ranges = contiguous([mean_of[x] for x in order], n)
            loads = [sum(mean_of[x] for x in order[i:j]) for i, j in ranges]
            loads_max = [sum(max_of[x] for x in order[i:j])
                         for i, j in ranges]
            k = min(range(n), key=lambda q: loads[q])
            lane = [l + (post if q == k else 0.0) + res
                    for q, l in enumerate(loads)]
            lane_max = [l + (post if q == k else 0.0) + res
                        for q, l in enumerate(loads_max)]
            plans.append(dict(n=n, ranges=ranges, loads=loads, lane=lane,
                              lane_max=lane_max, audits=k))
            w(f"| {n} | {max(lane):.0f} | {max(lane_max):.0f} "
              f"| {fixed + max(lane):.0f} | {fixed + max(lane_max):.0f} "
              f"| {'yes' if fixed + max(lane) <= TARGET_RUN_SECONDS else 'no'}"
              f" |")
        w("")
        chosen = next((p for p in plans
                       if fixed + max(p["lane_max"]) <= TARGET_RUN_SECONDS),
                      None)
        if chosen is None:
            w("No contiguous partition up to six lanes meets the budget, "
              "even at the mean.\n")
        else:
            p = chosen
            w(f"### Proposed partition: {p['n']} lanes\n")
            w(f"The smallest lane count whose longest lane fits the budget "
              f"even with every group at its slowest measured time.\n")
            w("| Lane | Spec.hs statements | Groups | Examples "
              "| Predicted Hspec s (mean) | Slowest run s | Largest groups |")
            w("|---|---|---|---|---|---|---|")
            for q, (i, j) in enumerate(p["ranges"]):
                members = order[i:j]
                big = sorted(members, key=lambda x: -mean_of[x])[:3]
                ex = sum(counts.get(x, 0) for x in members)
                note = " + post-suite audits" if q == p["audits"] else ""
                w(f"| {q + 1} | `Spec.hs:{labels[members[0]].line}`–"
                  f"`{labels[members[-1]].line}` | {len(members)}{note} "
                  f"| {ex} | {p['lane'][q]:.0f} | {p['lane_max'][q]:.0f} | "
                  + ", ".join(f"{x} ({mean_of[x]:.0f} s)" for x in big)
                  + " |")
            w("")
            longest = max(range(p["n"]), key=lambda q: p["lane"][q])
            w(f"**Longest lane: lane {longest + 1}**, {p['lane'][longest]:.0f} "
              f"s predicted (Hspec example time + its residual"
              + (" + audits" if longest == p["audits"] else "")
              + f"; {p['lane_max'][longest]:.0f} s at the slowest run), "
              f"against a per-lane budget of **{budget:.0f} s** → predicted "
              f"run {fixed + p['lane'][longest]:.0f} s at today's median "
              f"build.\n")
            builds = sorted(r["setup_build"] for r in base["rows"])
            rest = med["pre"] + med["wrapper"] + med["tail"]
            limit = TARGET_RUN_SECONDS - rest - p["lane"][longest]
            w("Sensitivity to the lane job's setup and build time "
              "(job start → suite start), all else at its median:\n")
            w("| Setup + build | s | Predicted run s | ≤ 1200 s |")
            w("|---|---|---|---|")
            for label, q in [("p25", 0.25), ("median", 0.5), ("p75", 0.75),
                             ("p90", 0.9), ("max", 1.0)]:
                v = _quantile(builds, q)
                run = rest + v + p["lane"][longest]
                w(f"| {label} | {v:.0f} | {run:.0f} "
                  f"| {'yes' if run <= TARGET_RUN_SECONDS else 'no'} |")
            w("")
            w(f"The proposal stays under twenty minutes while a lane's setup "
              f"and build take at most **{limit:.0f} s**; "
              f"{sum(1 for b in builds if b <= limit)} of the "
              f"{len(builds)} baseline runs were at or under that.\n")
    if lanes:
        w("## Lane validation\n")
        w("Each proposed lane run ALONE in its own process on CI (the "
          "`--lane-match` / `--lane-skip` collection commits), against the "
          "prediction from the whole-suite runs above.\n")
        w("| Run | Groups | Examples | Predicted Σ s | Measured Σ items s "
          "| `Finished in` s | Δ (Finished − predicted Σ) s "
          "| GitHub step s | Job setup + build s | Run wall s |")
        w("|---|---|---|---|---|---|---|---|---|---|")
        mean_of = {g.label: _mean([t.get(g.label, 0.0) for t in times])
                   for g in groups}
        seen: Counter = Counter()
        for lr in lanes:
            got = {i.group for i in lr.items}
            pred = sum(mean_of[x] for x in got)
            a = accounting(lr)
            w(f"| {rid(lr)} | {len(got)} | {lr.examples} | {pred:.1f} "
              f"| {a['items']:.1f} | {lr.finished:.1f} "
              f"| {lr.finished - pred:+.1f} | {_fmt(lr.step_seconds, 0)} "
              f"| {_fmt(lr.setup_seconds, 0)} "
              f"| {_fmt(lr.wall_seconds, 0)} |")
            seen.update((tuple(i.groups), tuple(i.path), i.description)
                        for i in lr.items)
        whole = Counter((tuple(i.groups), tuple(i.path), i.description)
                        for i in runs[0].items)
        missing = sum((whole - seen).values())
        twice = sum((seen - whole).values())
        w("")
        w(f"Coverage, compared as multisets of example paths (two paths "
          f"occur twice in the whole suite): the lane runs together ran "
          f"{sum(seen.values())} examples against the whole suite's "
          f"{sum(whole.values())}; {missing} missing, {twice} extra or "
          f"repeated across lanes.\n")
        w("Every group over 5 s, alone against its whole-suite mean. A "
          "slower or faster runner scales CPU-bound groups together while "
          "groups that mostly wait on wall-clock timers stay near 1.00; a "
          "first-use cost moved into a lane would instead show up as a "
          "fixed excess on that lane's earliest groups.\n")
        w("| Run | Group | Statement | Whole-suite mean s | Alone s | Ratio |")
        w("|---|---|---|---|---|---|")
        for lr in lanes:
            t = group_times(lr)
            for g in groups:
                if g.label in t and mean_of[g.label] > 5:
                    w(f"| {rid(lr)} | {g.label} | {_cell(g.text, 50)} "
                      f"| {mean_of[g.label]:.1f} | {t[g.label]:.1f} "
                      f"| {t[g.label] / mean_of[g.label]:.3f} |")
            small = [g.label for g in groups
                     if g.label in t and mean_of[g.label] <= 5]
            w(f"| {rid(lr)} | {len(small)} groups ≤ 5 s | | "
              f"{sum(mean_of[x] for x in small):.1f} "
              f"| {sum(t[x] for x in small):.1f} "
              f"| {sum(t[x] for x in small) / sum(mean_of[x] for x in small):.3f} |")
        w("")
    return out


def run_aggregate(args: argparse.Namespace) -> int:
    cache = Path(args.cache) if args.cache else None
    spec_text = subprocess.run(
        ["git", "show", f"{args.spec_commit}:{SPEC}"], capture_output=True,
        text=True, check=True).stdout
    groups = spec_groups(spec_text)
    runs = [fetch_run(args.repo, ref, cache) for ref in args.run]
    full = [fetch_run(args.repo, ref, cache) for ref in args.full_run]
    if len(runs) < 1:
        raise MeasureError("at least one --run is required")
    check_identical(runs)
    for r in runs:
        if r.conclusion != "success":
            raise MeasureError(f"run {r.run_id} concluded {r.conclusion}")
    base = (baseline_overhead(args.repo, args.baseline_run, cache)
            if args.baseline_run else None)
    masters = []
    for ref in args.master_run:
        run_id, attempt = parse_run_ref(ref)
        _meta, jobs = fetch_jobs(args.repo, run_id, attempt, cache)
        job, _step = headless_job(run_id, jobs)
        fin, items = slow_items(gh_cached(
            f"repos/{args.repo}/actions/jobs/{job['id']}/logs", cache))
        masters.append((run_id, fin, items))
    lanes = [fetch_run(args.repo, ref, cache) for ref in args.lane_run]
    print("\n".join(render(args, runs, full, groups, base, masters, lanes)))
    return 0


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
    assert "measureConfig0\n        =≪ MeasureEnv.getArgs" in out, out
    laned = instrument_spec(spec, True, ("skip", ["@G6", "@G8"]))
    assert '∘ (["--skip", "/@G6/", "--skip", "/@G8/"] ⧺)' in laned, laned
    assert 'setEnv "SYNARCHY_FULL_TESTS" "1"' in laned
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
    assert _quantile([1, 2, 3, 4], 0.5) == statistics.median([1, 2, 3, 4])
    assert _quantile([10, 20], 0.25) == 12.5
    assert contiguous([1, 2, 3, 4, 5, 6, 7, 8, 9], 3) == [(0, 5), (5, 7),
                                                          (7, 9)]
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
    p.add_argument("--lane-match", metavar="@G<line>,...",
                   help="run only these top-level groups (a lane alone)")
    p.add_argument("--lane-skip", metavar="@G<line>,...",
                   help="run every top-level group except these")
    p = sub.add_parser("aggregate", help="tabulate measured runs")
    p.add_argument("--repo", default="coghex/synarchy")
    p.add_argument("--run", action="append", default=[], metavar="ID[:ATTEMPT]",
                   help="an ordinary-tier measurement run")
    p.add_argument("--full-run", action="append", default=[],
                   metavar="ID[:ATTEMPT]",
                   help="a full-tier measurement run")
    p.add_argument("--spec-commit", required=True,
                   help="the commit whose Spec.hs the @G/@S lines index")
    p.add_argument("--lane-run", action="append", default=[],
                   metavar="ID[:ATTEMPT]",
                   help="a --lane-match/--lane-skip collection run")
    p.add_argument("--baseline-run", action="append", default=[],
                   metavar="ID[:ATTEMPT]",
                   help="an unmodified successful PR run for the budget")
    p.add_argument("--master-run", action="append", default=[],
                   metavar="ID[:ATTEMPT]",
                   help="an existing master push whose slow items to read")
    p.add_argument("--cache", help="directory memoizing gh api responses")
    sub.add_parser("self-test")
    args = parser.parse_args(argv)
    try:
        if args.cmd == "instrument":
            return run_instrument(args)
        if args.cmd == "self-test":
            return self_test()
        return run_aggregate(args)
    except MeasureError as error:
        print(f"headless_lane_measurement: {error}", file=sys.stderr)
        return 1


if __name__ == "__main__":
    sys.exit(main())
