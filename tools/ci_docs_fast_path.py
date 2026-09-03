#!/usr/bin/env python3
"""Decide whether a PR or master push may take CI's docs-only fast path (#1490).

CI's `test-and-audits` worker spends ~22 minutes on a cabal build plus the
headless hspec suite. Neither can be affected by a plain documentation
change: no Haskell target compiles those docs, and documentation-shaped
save-compatibility inputs are excluded below. A qualifying change may
therefore skip the Haskell half while retaining every engine-free Python
audit -- which is the half a documentation change CAN break.

That distinction is the whole point, and it is not cosmetic. Issue #1490
exists because master `559e946f` changed only
`docs/code_health_findings.md` and thereby broke
`tools/test_findings_report_audit.py`, whose self-test reads that report.
A fast path that skipped the audits would have hidden that failure
outright rather than merely delaying it, which is strictly worse than the
cancellation bug it was meant to fix. So: skip the BUILD, retain the
engine-free audits, and exclude docs whose own audit needs Cabal.

Since #2272 that retention is STRUCTURAL rather than something this
decision arranges: the engine-free audits live in their own
`static-audits` job that carries no condition at all, so this selector's
answer cannot reach them. What it still decides is the Cabal half of
`test-and-audits` -- the build, the test suites, the headless suite,
world_check, and (with the save-compat selector) that job's save
audit. Widening eligibility here therefore no longer risks skipping an
audit, but it does still risk skipping a real build.

Eligibility is deliberately conservative -- it fails CLOSED, because the
cost of a wrong "eligible" is an unverified master commit while the cost
of a wrong "not eligible" is one slow-but-correct run:

  * Every changed path must live under `docs/`.
  * The change status of every path must be ADD or MODIFY. A delete, a
    rename, a copy or a type change reverts to the full job even when
    both endpoints are documentation.
  * `docs/save_compat/*` is EXCLUDED however it is touched. The manifest
    is read by the Haskell save-compat suite, and the directory's other
    machine-readable contracts select a Python audit that decodes real
    fixtures through `cabal repl`. Calling these files documentation does
    not make them Cabal-free. (The fixture blobs live under
    `test-headless/data/`, which is outside `docs/` and already forces the
    full job.)
  * An empty or unreadable range is NOT eligible.

The caller supplies `git diff --no-renames --name-status BASE AFTER`
over a PR's complete base range or a push's complete pushed range, so the
whole change is judged as one. `--no-renames` is what turns a rename into
the delete+add pair the status rule above rejects; without it a doc
renamed onto a source path would read as a single eligible entry.
"""

from __future__ import annotations

import argparse
import sys

# Documentation-shaped compatibility inputs whose checks invoke Cabal.
SAVE_COMPAT_PREFIX = "docs/save_compat/"

DOCS_PREFIX = "docs/"

# Statuses that keep a range eligible. Everything else -- D(elete),
# R(ename), C(opy), T(ype change), U(nmerged), X (unknown) -- reverts to
# the full job.
ELIGIBLE_STATUSES = frozenset({"A", "M"})


def parse_name_status(text: str) -> list[tuple[str, str]]:
    """Parse `git diff --name-status` output into (status, path) pairs.

    Git writes one record per line as STATUS<TAB>PATH, and for the
    rename/copy forms STATUS<TAB>OLD<TAB>NEW. Callers are expected to
    pass --no-renames so the latter cannot appear, but parsing it
    correctly anyway means a caller that forgets still yields real paths
    to judge rather than a silently mangled one.

    A status may carry a similarity score (R100, C075); the leading
    letter is the status.
    """
    records: list[tuple[str, str]] = []
    for raw in text.splitlines():
        line = raw.strip()
        if not line:
            continue
        fields = line.split("\t")
        if len(fields) < 2:
            # Not a name-status record. Refuse to guess.
            raise ValueError(f"unparsable --name-status record: {raw!r}")
        status = fields[0].strip()
        if not status:
            raise ValueError(f"empty status in record: {raw!r}")
        # For a rename/copy, every listed endpoint matters.
        for path in fields[1:]:
            path = path.strip()
            if path:
                records.append((status[0], path))
    return records


def is_docs_only(records: list[tuple[str, str]]) -> bool:
    """True only when this whole range is eligible documentation."""
    if not records:
        # No changes parsed. A push always changes something, so an empty
        # range means the caller could not resolve it -- run everything.
        return False
    for status, path in records:
        if status not in ELIGIBLE_STATUSES:
            return False
        if path.startswith(SAVE_COMPAT_PREFIX):
            return False
        if not path.startswith(DOCS_PREFIX):
            return False
    return True


def explain(records: list[tuple[str, str]]) -> str:
    """A one-line reason, so a skipped build says why in the CI log."""
    if not records:
        return "no resolvable changed-path range; running the full job"
    for status, path in records:
        if status not in ELIGIBLE_STATUSES:
            return (f"{path} has status {status} (not a plain add/modify); "
                    "running the full job")
        if path.startswith(SAVE_COMPAT_PREFIX):
            return (f"{path} requires the Cabal-backed save-compat gate; "
                    "running the full job")
        if not path.startswith(DOCS_PREFIX):
            return f"{path} is outside docs/; running the full job"
    return (f"all {len(records)} changed path(s) are documentation; "
            "skipping Cabal and running every engine-free Python audit")


def _self_test() -> int:
    failures: list[str] = []

    def check(label: str, got: object, want: object) -> None:
        if got != want:
            failures.append(f"{label}: expected {want!r}, got {got!r}")

    def verdict(text: str) -> bool:
        return is_docs_only(parse_name_status(text))

    # -- the fast path is taken only for plain documentation edits --
    check("a single modified doc",
          verdict("M\tdocs/code_health_findings.md"), True)
    check("several modified docs",
          verdict("M\tdocs/a.md\nM\tdocs/b.md"), True)
    check("an added doc", verdict("A\tdocs/new_report.md"), True)
    check("a doc in a nested directory",
          verdict("M\tdocs/history/claude_md_2026-08-20_pretrim.md"), True)
    # -- save-compat docs are executable compatibility inputs --
    manifest = f"{SAVE_COMPAT_PREFIX}manifest.json"
    enum_baseline = f"{SAVE_COMPAT_PREFIX}enum_baseline.json"
    check("the save-compat manifest alone",
          verdict(f"M\t{manifest}"), False)
    check("the manifest beside an ordinary doc",
          verdict(f"M\tdocs/a.md\nM\t{manifest}"), False)
    check("an added manifest", verdict(f"A\t{manifest}"), False)
    check("the enum baseline", verdict(f"M\t{enum_baseline}"), False)

    # -- anything outside docs/ reverts to the full job --
    check("a source file", verdict("M\tsrc/World/Types.hs"), False)
    check("a mixed docs+source range",
          verdict("M\tdocs/a.md\nM\tsrc/World/Types.hs"), False)
    check("a tools change", verdict("M\ttools/findings_report_audit.py"), False)
    check("the workflow itself", verdict("M\t.github/workflows/ci.yml"), False)
    check("a root-level doc is NOT under docs/",
          verdict("M\tCLAUDE.md"), False)
    check("a path merely starting with the word docs",
          verdict("M\tdocsomething/a.md"), False)

    # -- non-add/modify statuses revert, even for documentation --
    check("a deleted doc", verdict("D\tdocs/old.md"), False)
    check("a deleted doc beside a modified one",
          verdict("M\tdocs/a.md\nD\tdocs/b.md"), False)
    check("a type change", verdict("T\tdocs/a.md"), False)
    # --no-renames makes git emit this pair instead of a single R record;
    # the D is what rejects it. This is the case that would otherwise let
    # a doc renamed ONTO a source path slip through.
    check("a rename, as --no-renames spells it",
          verdict("D\tdocs/a.md\nA\tsrc/World/Types.hs"), False)
    # ...and if a caller forgets --no-renames, the R record still rejects
    # rather than being read as an eligible single path.
    check("a rename, as git spells it WITH rename detection",
          verdict("R100\tdocs/a.md\tdocs/b.md"), False)
    check("a rename out of docs/ with rename detection",
          verdict("R100\tdocs/a.md\tsrc/World/Types.hs"), False)

    # -- degenerate input fails closed --
    check("an empty range", verdict(""), False)
    check("a blank-line-only range", verdict("\n\n"), False)

    # -- the parser refuses to guess rather than mangling a record --
    for bad in ("docs/a.md", "M", ""):
        if bad == "":
            continue
        try:
            parse_name_status(bad)
        except ValueError:
            pass
        else:
            failures.append(
                f"a record with no tab ({bad!r}) should raise, not parse")

    # -- explain() names the offender, so a full run says why --
    reason = explain(parse_name_status(
        f"M\tdocs/a.md\nM\t{manifest}"))
    if manifest not in reason:
        failures.append(f"explain() should name the manifest, got {reason!r}")
    reason = explain(parse_name_status("M\tdocs/a.md\nM\tsrc/World/Types.hs"))
    if "src/World/Types.hs" not in reason:
        failures.append(f"explain() should name the source file, got {reason!r}")

    for failure in failures:
        print(f"  FAIL: {failure}")
    if failures:
        print(f"\n{len(failures)} ci_docs_fast_path self-test case(s) failed")
        return 1
    print("ci_docs_fast_path self-test: all cases pass")
    return 0


def main() -> int:
    parser = argparse.ArgumentParser(
        description="Decide whether a PR or master push may skip CI's Haskell "
                    "build (#1490). Reads `git diff --no-renames "
                    "--name-status BASE AFTER` over the complete change "
                    "range and prints true/false.")
    parser.add_argument(
        "--stdin", action="store_true",
        help="read the --name-status range from standard input")
    parser.add_argument(
        "--explain", action="store_true",
        help="also print a one-line reason to stderr")
    parser.add_argument("--self-test", action="store_true")
    args = parser.parse_args()

    if args.self_test:
        return _self_test()
    if not args.stdin:
        parser.error("--stdin is required unless --self-test is used")

    try:
        records = parse_name_status(sys.stdin.read())
    except ValueError as error:
        # An unparsable range is not a licence to skip the build.
        print(f"ci_docs_fast_path: {error}", file=sys.stderr)
        print("false")
        return 0

    if args.explain:
        print(f"ci_docs_fast_path: {explain(records)}", file=sys.stderr)
    print("true" if is_docs_only(records) else "false")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
