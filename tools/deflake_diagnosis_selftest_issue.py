#!/usr/bin/env python3
"""#1438's cases: filing an issue when the bug is in the engine.

The 34 cases covering `tools/deflake_issue.py` and its owners — the
evidence a production-defect issue must carry, the body it renders,
the stable publication key a resume reconciles against, and the
tracker boundary that stays a fake so no case can reach `gh` or the
network.

This is the one section that writes outside a temporary directory: it
stages the retained artifact tree a failing batch would have left,
under a fixture-owned path in `/tmp`, because this workflow READS
those artifacts and a filed issue whose only evidence is a pathname is
the thing it exists to prevent.

The outcome-shaped helpers these cases build a defect handoff from —
`outcome_handoff`, `measurement_entries`, `rebind_references`,
`record_outcome`, `census_file`, `stored_outcomes` and the identity
constants — are imported from `deflake_diagnosis_selftest_support`, which is
their single source; they are not borrowed from the outcome unit and
not copied here.

Not a gate of its own. Run through the facade:

  python3 tools/test_deflake_diagnosis.py --only issue
"""
from __future__ import annotations

import ast
import contextlib
import copy
import importlib
import json
import os
import re
import shutil
import subprocess
import sys
import tempfile
from pathlib import Path

sys.path.insert(0, str(Path(__file__).resolve().parent))
import deflake_diagnosis as dd  # type: ignore  # noqa: E402
import deflake_handoff  # type: ignore  # noqa: E402
import deflake_issue as di  # type: ignore  # noqa: E402
# The unfittable-body case below substitutes `MAX_BODY_CHARS`, and
# `issue_body` reads it out of the DOCUMENT owner's globals (#2157), not
# the facade's — so that is the object these cases name.
import deflake_issue_document as did  # type: ignore  # noqa: E402
import deflake_issue_evidence as die  # type: ignore  # noqa: E402
import deflake_outcome as do  # type: ignore  # noqa: E402
import probe_census  # type: ignore  # noqa: E402
import probe_flake  # type: ignore  # noqa: E402
import selftestlib  # noqa: E402
from deflake_diagnosis_selftest_support import (  # noqa: E402
    census_file, CLEAN_WT, _DEFAULT, diagnosis_document,
    elsewhere_failure_result, evaluate, expect, FAIL,
    forged_aggregate_result, measurement_entries, MISSING, outcome_handoff,
    OUTSIDE, PASS, PRIMARY_WT, PROBE, rebind_references, record_outcome,
    short_result, spotless_result, stored_outcomes, verification_result,
    WORKTREES)


# ==========================================================================
# #1438: filing an issue when the bug is in the engine
# ==========================================================================
#
# `tools/deflake_issue.py` consumes the SAME `deflake-outcome-handoff/v1`
# envelope #1439 does, on the one route #1439 refuses: #1437's
# `production-defect`. It files one review-ready tracker issue carrying
# the measured evidence, records that issue in the probe's census row,
# and stops — the probe is not touched and no pull request is opened.
#
# The tracker is a fake at the publication boundary, so "exactly one
# issue" is a counted fact rather than a hope; the census, its schema,
# and #1437's own evaluator are real.
DEFECT_ATTEMPT = "role-20260829T090000Z-4711-beefcafe"


DEFECT_NOW = "2026-08-29T09:00:00Z"


DEFECT_SUMMARY = ("ten controlled runs at the handoff's own commit "
                  "reproduced the ordering in four of them")


# A diagnosis of PRODUCTION behaviour, which is what this route is for.
# The shared fixture's default names a probe-side setup precondition —
# #1437's repair route — and says nothing about the engine.
DEFECT_DIAGNOSIS = {
    "category": None,
    "summary": ("World.Thread publishes a chunk before its tile map is "
                "installed, so a query issued immediately after "
                "loadChunksInRegion reads a flat column"),
    "evidence": [
        "run 1's engine log records the publish ahead of the install",
        "runs 2 and 3 show the same ordering, and the passing runs do not",
    ],
}


DEFECT_EVENTS = ('{"schema":"probe-event/v1","check":"beta",'
                 '"result":"FAIL","detail":"tile z was 0"}\n')


DEFECT_STDOUT = "probe: beta failed: expected a loaded column\n"


DEFECT_ENGINE_LOG = ("[World] chunk 3,4 published\n"
                     "[World] chunk 3,4 tile map installed\n")


def defect_handoff(*, attempt: str = DEFECT_ATTEMPT,
                   summary: str = DEFECT_SUMMARY, diagnosis=_DEFAULT,
                   document=None, measurements=None,
                   rebind: bool = True) -> dict:
    """One `deflake-outcome-handoff/v1` on #1438's own route.

    The producer record is PRODUCED by #1437's evaluator rather than
    hand-assembled, for the reason every other fixture here is: a
    hand-written envelope agrees with whatever the consumer happens to
    require, which is how a consumer that could never read a real
    producer record keeps a green suite.
    """
    block = DEFECT_DIAGNOSIS if diagnosis is _DEFAULT else diagnosis
    if document is None:
        document = diagnosis_document(route=dd.ROUTE_PRODUCTION_DEFECT,
                                      diagnosis=block)
    record = evaluate(document).to_document()
    entries = (measurement_entries(dd.ROUTE_PRODUCTION_DEFECT, document)
               if measurements is None else measurements)
    if measurements is not None and rebind:
        record = rebind_references(record, entries)
    return copy.deepcopy({
        "schema": di.HANDOFF_SCHEMA,
        "attempt": attempt,
        "summary": summary,
        "diagnosis_outcome": record,
        "measurements": entries,
    })


@contextlib.contextmanager
def staged_evidence(document, *, events=DEFECT_EVENTS,
                    stdout=DEFECT_STDOUT, engine=DEFECT_ENGINE_LOG,
                    only=None):
    """The retained artifacts a real failing batch would have left.

    Every other fixture here treats an artifact path as a STRING,
    because #1439 stores references and never opens one. This workflow
    reads them, so the tree has to exist. `only` stages a prefix of the
    run list, which is how "the artifacts were pruned" is expressed.
    """
    paths = [Path(entry) for entry
             in document["diagnosis_outcome"]["retained_artifacts"]]
    staged = paths if only is None else paths[:only]
    try:
        for directory in staged:
            (directory / "engine").mkdir(parents=True, exist_ok=True)
            (directory / "events.jsonl").write_text(events, encoding="utf-8")
            (directory / "stdout.txt").write_text(stdout, encoding="utf-8")
            (directory / "engine" / "engine-9101.log").write_text(
                engine, encoding="utf-8")
        yield staged
    finally:
        for directory in staged:
            shutil.rmtree(directory, ignore_errors=True)
        # Up to and including the fixture root, so a run of this suite
        # leaves nothing behind in `/tmp`. `rmdir` refuses a non-empty
        # directory, so this can only remove what the fixture made.
        for directory in staged:
            for parent in directory.parents:
                if not str(parent).startswith(OUTSIDE):
                    break
                with contextlib.suppress(OSError):
                    parent.rmdir()


class FakePublication(di.Publication):
    """The tracker, faked at the only boundary that reaches it.

    It counts, so "exactly one issue" is observed rather than assumed;
    it stores what it was given under the publication key the BODY
    carries, so a reconcile finds an issue only if the publisher really
    wrote the marker; and it can be told to fail either operation, which
    is how a publication failure is exercised without a network.
    """

    def __init__(self, *, find_error=None, create_error=None, issues=None,
                 answer=None):
        self.finds: list = []
        self.creates: list = []
        # `key -> {"number", "url", "body"}`, because the real boundary
        # answers with the body: it is what proves the match is the
        # marker LINE rather than a quotation, and where the issue's own
        # origin brand is read from.
        self.issues = dict(issues or {})
        self.find_error = find_error
        self.create_error = create_error
        self.answer = answer
        self.next_number = 901

    @staticmethod
    def filed_key(body: str):
        head = f"<!-- {di.PUBLICATION_MARKER}: "
        for line in body.splitlines():
            if line.startswith(head) and line.endswith(" -->"):
                return line[len(head):-len(" -->")]
        return None

    def find(self, key: str):
        self.finds.append(key)
        if self.find_error is not None:
            raise self.find_error
        return copy.deepcopy(self.issues.get(key))

    def create(self, *, title: str, body: str):
        self.creates.append({"title": title, "body": body})
        if self.create_error is not None:
            raise self.create_error
        if self.answer is not None:
            return copy.deepcopy(self.answer)
        number = self.next_number
        self.next_number += 1
        issue = {"number": number,
                 "url": f"https://github.com/coghex/synarchy/issues/{number}"}
        key = self.filed_key(body)
        if key is not None:
            self.issues[key] = dict(issue, body=body)
        return copy.deepcopy(issue)


class Spy:
    """A forbidden boundary, injected so its silence is provable."""

    def __init__(self) -> None:
        self.calls: list = []

    def __call__(self, record) -> None:
        self.calls.append(record)


def defect_key(document) -> str:
    return di.publication_key(
        di.accept(document, worktrees=WORKTREES, primary=PRIMARY_WT))


def file_defect(document, path, *, publication=None, origin: str = "claude",
                now: str = DEFECT_NOW, probe_spy=None, pr_spy=None):
    """Run the whole workflow over one handoff, with every boundary faked."""
    publication = FakePublication() if publication is None else publication
    probe_spy = Spy() if probe_spy is None else probe_spy
    pr_spy = Spy() if pr_spy is None else pr_spy
    defect = di.accept(document, worktrees=WORKTREES, primary=PRIMARY_WT)
    published = di.publish(defect, census_path=path, now=now,
                           publication=publication, origin=origin,
                           probe_publisher=probe_spy,
                           pull_request_publisher=pr_spy)
    return published, publication, probe_spy, pr_spy


def expect_not_filed(thunk, fragment: str, msg: str) -> None:
    """`thunk` is a well-formed handoff whose ending files nothing."""
    try:
        thunk()
    except di.NonSuccess as error:
        expect(fragment in str(error),
               f"{msg}: refused, but for {str(error)!r} rather than "
               f"{fragment!r}")
        return
    except di.HandoffError as error:
        selftestlib.record_fail(
            f"{msg}: rejected the INPUT ({error}) where the "
            f"EVIDENCE should have been refused")
        return
    selftestlib.record_fail(f"{msg}: filed an issue")


def expect_nothing_published(path, before: bytes, publication, msg: str,
                             *, searched=None) -> None:
    """The assertion every non-success owes: no issue, no record, no trace."""
    expect(publication.creates == [],
           f"{msg}: an issue was created anyway")
    expect(Path(path).read_bytes() == before,
           f"{msg}: the census bytes changed")
    expect(not stored_outcomes(path),
           f"{msg}: an outcome was recorded anyway")
    if searched is not None:
        expect(len(publication.finds) == searched,
               f"{msg}: the tracker was searched {len(publication.finds)} "
               f"time(s), not {searched}")


def test_1438_owns_exactly_the_route_1437_hands_it() -> None:
    """Parity with the producer, from both sides of the sibling split."""
    owned = {route for route, owner in dd.ROUTE_OWNER.items()
             if owner == di.OWNER_ISSUE}
    expect(owned == {dd.ROUTE_PRODUCTION_DEFECT},
           f"#1437 hands #1438 exactly the production-defect route; got "
           f"{sorted(owned)}")
    expect(set(di.OWNED.roles) == owned,
           f"and this workflow claims exactly it; got "
           f"{sorted(di.OWNED.roles)}")
    expect(di.OWNED.outcomes == (di.OUTCOME_PRODUCTION_DEFECT,),
           f"reaching one outcome; got {di.OWNED.outcomes}")
    expect(di.OUTCOME_PRODUCTION_DEFECT not in do.STABLE_OUTCOMES,
           "and it is not one of #1439's stable outcomes")
    roles = di.OWNED.roles[dd.ROUTE_PRODUCTION_DEFECT]
    expect(roles["designated"] in roles["required"],
           f"the route is judged on a measurement it requires; got {roles}")
    expect(not (set(roles["required"]) & set(roles["forbidden"])),
           f"and does not both require and forbid a role; got {roles}")
    expect(do.ROLE_VERIFICATION in roles["forbidden"],
           "and runs no verification batch: #1437 refuses that route a "
           "verification outright, because one would mean a repair was "
           "attempted")
    # Constructing this workflow's ownership leaves the sibling's alone.
    expect(do.OWNED.issue == 1439
           and set(do.OWNED.roles) == set(do.ROUTE_TO_OUTCOME),
           f"#1439's own ownership is untouched; got {do.OWNED.issue} over "
           f"{sorted(do.OWNED.roles)}")


def test_a_production_defect_files_one_issue_carrying_its_evidence() -> None:
    """The whole acceptance case, in one pass.

    Exactly one issue; every measurement fact the approved amendment
    names; log evidence a reviewer can READ rather than a path only the
    measuring machine can open; the returned identity recorded; and the
    ending terminal.
    """
    document = defect_handoff()
    key = defect_key(document)
    with staged_evidence(document), census_file() as path:
        published, publication, probe_spy, pr_spy = file_defect(
            document, path)
        expect(len(publication.creates) == 1,
               f"exactly one issue is created; got "
               f"{len(publication.creates)}")
        expect(publication.finds == [key],
               f"reconciled against the publication key BEFORE creating; "
               f"got {publication.finds}")
        body = publication.creates[0]["body"]
        title = publication.creates[0]["title"]
        expect(PROBE in title and "production defect" in title,
               f"the title names the probe and the diagnosis; got {title!r}")

        result = [entry for entry in document["measurements"]
                  if entry["role"] == do.ROLE_BASELINE][0]["result"]
        required = {
            "the probe": f"`{PROBE}`",
            "the failure numerator and denominator":
                f"{result['failure_count']}/{result['requested_runs']}",
            "the failure rate": f"rate {result['failure_rate']}",
            "the timeout count": f"Timeouts: {result['timeout_count']}",
            "the measured commit": f"`{result['commit_sha']}`",
            "the completed run count":
                f"{result['completed_runs']} completed of "
                f"{result['requested_runs']} requested",
            "the RTS capability setting":
                f"+RTS -N{result['rts_capabilities']}",
            "the diagnosed production behaviour":
                DEFECT_DIAGNOSIS["summary"],
            "the attempt identity": DEFECT_ATTEMPT,
            "the acceptable-failure ceiling": "Acceptable failures (X): 0",
        }
        for what, fragment in required.items():
            expect(fragment in body,
                   f"the filed body states {what} ({fragment!r})")
        for cid, tally in sorted(result["check_counts"].items()):
            row = (f"| `{cid}` | {tally[PASS]} | {tally[FAIL]} "
                   f"| {tally[MISSING]} |")
            expect(row in body,
                   f"and every declared check's PASS/FAIL/MISSING tally "
                   f"({row!r})")
        expect(DEFECT_STDOUT.strip() in body
               and DEFECT_ENGINE_LOG.splitlines()[0] in body,
               "and bounded excerpts of the retained failure artifacts, not "
               "their pathnames alone")
        expect(result["retained_artifacts"][0] in body,
               "beside the artifact path the full log can be found at")

        issue = published.record["issue"]
        expect(issue["number"] == 901
               and issue["url"].endswith("/issues/901"),
               f"the returned issue identity is recorded; got {issue}")
        expect(issue["publication_key"] == key
               and issue["origin"] == "claude",
               f"under the key and brand it was filed with; got {issue}")
        stored = stored_outcomes(path)
        expect(len(stored) == 1
               and stored[0]["outcome"] == di.OUTCOME_PRODUCTION_DEFECT
               and stored[0]["issue"] == issue,
               f"and the census holds exactly that one outcome; got "
               f"{stored}")
        expect(stored[0]["recommendation"] is None
               and stored[0]["comparison"] is None,
               "with the two route-specific fields this ending has nothing "
               "to say in stored as an explicit null")
        report = published.to_document()
        expect(report["terminal"] is True
               and report["created_issue"] is True
               and report["reconciled_issue"] is False
               and report["resumed"] is False,
               f"the report says what actually happened; got {report}")
        expect(probe_spy.calls == [] and pr_spy.calls == [],
               "and neither forbidden boundary was reached")


def test_filing_reaches_neither_the_probe_nor_the_pull_request() -> None:
    """The two boundaries, from both sides.

    Both tables are consulted on the one route this workflow owns, so
    the silence below is a branch that really ran — flipping an entry
    makes the injected spy fire, which is what stops the assertion being
    vacuous.
    """
    expect(set(di.CHANGES_THE_PROBE) == {di.OUTCOME_PRODUCTION_DEFECT}
           and not any(di.CHANGES_THE_PROBE.values()),
           f"the filed defect changes no probe; got {di.CHANGES_THE_PROBE}")
    expect(set(di.OPENS_PULL_REQUEST) == {di.OUTCOME_PRODUCTION_DEFECT}
           and not any(di.OPENS_PULL_REQUEST.values()),
           f"and opens no pull request; got {di.OPENS_PULL_REQUEST}")

    document = defect_handoff()
    with staged_evidence(document), census_file() as path:
        published, _publication, probe_spy, pr_spy = file_defect(
            document, path)
        expect(probe_spy.calls == [] and pr_spy.calls == [],
               "neither publisher was called on the shipped policy")
        expect(published.changed_probe is False
               and published.opened_pull_request is False,
               "and the report says what the boundaries DID, not what the "
               "policy says")

    for table, attribute in ((di.CHANGES_THE_PROBE, "changed_probe"),
                             (di.OPENS_PULL_REQUEST, "opened_pull_request")):
        saved = dict(table)
        table[di.OUTCOME_PRODUCTION_DEFECT] = True
        try:
            with staged_evidence(document), census_file() as path:
                published, _pub, probe_spy, pr_spy = file_defect(
                    document, path)
                fired = (probe_spy.calls if attribute == "changed_probe"
                         else pr_spy.calls)
                expect(len(fired) == 1
                       and getattr(published, attribute) is True,
                       f"the {attribute} boundary is consulted rather than "
                       f"absent, so `never called` is an observed fact")
        finally:
            table.clear()
            table.update(saved)

    # And both defaults refuse rather than quietly succeeding.
    for publisher in (di.forbidden_probe_change, di.forbidden_pull_request):
        try:
            publisher({"outcome": di.OUTCOME_PRODUCTION_DEFECT})
        except di.NonSuccess:
            continue
        selftestlib.record_fail(f"{publisher.__name__} accepted a call")


def test_resuming_a_filed_defect_touches_the_tracker_not_at_all() -> None:
    """A recorded outcome is the completion marker.

    Not merely "creates no second issue": a completed attempt must not
    reach the tracker at all, so a resume is free of network traffic and
    of whatever the tracker happens to answer that day. The clock and
    the origin differ deliberately — both are reused from the stored
    record, so the rebuilt record is a replay rather than a conflict.
    """
    document = defect_handoff()
    with staged_evidence(document), census_file() as path:
        first, _publication, _probe, _pr = file_defect(document, path)
        before = Path(path).read_bytes()
        again, publication, probe_spy, pr_spy = file_defect(
            document, path, origin="codex", now="2026-08-30T10:00:00Z")
        expect(publication.finds == [] and publication.creates == [],
               f"the tracker was not consulted at all; got "
               f"{publication.finds} / {len(publication.creates)}")
        expect(again.resumed is True and again.created is False
               and again.reconciled is False,
               f"the resume says so; got {again.to_document()}")
        expect(again.record == first.record,
               "and installs the identical record, stamp, issue and origin "
               "included")
        expect(Path(path).read_bytes() == before,
               "leaving the census byte-identical")
        expect(len(stored_outcomes(path)) == 1,
               "with one outcome recorded, not two")
        expect(probe_spy.calls == [] and pr_spy.calls == [],
               "and no forbidden boundary reached on the resume either")


def test_an_issue_created_before_a_crash_is_reconciled_not_duplicated()\
        -> None:
    """The window a completion marker alone cannot close.

    Creation takes effect and its identity is never durably recorded —
    a timeout, a crash, or a census write that refuses in between. The
    resume must find the issue the first attempt filed, record it once,
    and create nothing.
    """
    document = defect_handoff()
    with staged_evidence(document), census_file() as path:
        publication = FakePublication()
        healthy = json.loads(Path(path).read_text(encoding="utf-8"))
        # A census on a schema this writer refuses: the issue is
        # created, and then nothing is recorded. That is the crash
        # window, made deterministic.
        Path(path).write_text(
            json.dumps(dict(healthy, schema=probe_census.SEED_SCHEMA)),
            encoding="utf-8")
        before = Path(path).read_bytes()
        expect_not_filed(
            lambda: file_defect(document, path, publication=publication),
            "exists", "a census that refused after the issue was created")
        expect(len(publication.creates) == 1,
               f"the issue was created before the census refused; got "
               f"{len(publication.creates)}")
        expect(Path(path).read_bytes() == before,
               "and the refusal left the census byte-identical")

        # The resume, against a census that works again.
        Path(path).write_text(json.dumps(healthy), encoding="utf-8")
        published, _pub, probe_spy, pr_spy = file_defect(
            document, path, publication=publication)
        expect(len(publication.creates) == 1,
               f"the resume creates nothing; got "
               f"{len(publication.creates)} creation(s) in total")
        expect(published.reconciled is True and published.created is False
               and published.resumed is False,
               f"it reconciles the existing issue; got "
               f"{published.to_document()}")
        expect(published.record["issue"]["number"] == 901,
               f"recording the issue that already existed; got "
               f"{published.record['issue']}")
        recorded = stored_outcomes(path)
        expect(len(recorded) == 1
               and recorded[0]["issue"]["number"] == 901,
               f"exactly once; got {recorded}")
        expect(probe_spy.calls == [] and pr_spy.calls == [],
               "and still neither forbidden boundary")

        # A third invocation is an ordinary resume again.
        third, publication, _probe, _pr = file_defect(document, path)
        expect(third.resumed is True and publication.creates == []
               and len(stored_outcomes(path)) == 1,
               "and the attempt stays settled afterwards")


def test_an_attempt_identity_already_used_elsewhere_files_nothing() -> None:
    """One attempt identity identifies one attempt, across both siblings.

    The completion marker this workflow reads is "the census holds this
    attempt". A record the SIBLING wrote under the same identity is not
    that, and reusing its issue is unrepresentable — it has none — so it
    is refused before anything reaches the tracker rather than
    discovered as an attribute error on the way to one.
    """
    sibling = outcome_handoff(attempt=DEFECT_ATTEMPT)
    document = defect_handoff()
    with staged_evidence(document), census_file() as path:
        record_outcome(sibling, path)
        before = Path(path).read_bytes()
        publication = FakePublication()
        expect_not_filed(
            lambda: file_defect(document, path, publication=publication),
            "already recorded",
            "an attempt identity the sibling workflow already used")
        expect(publication.creates == [] and publication.finds == [],
               "the tracker was not consulted")
        expect(Path(path).read_bytes() == before,
               "and the census was left byte-identical")


def test_a_reconcile_answering_with_an_unusable_identity_files_nothing()\
        -> None:
    """The tracker's answer is validated, not taken on trust."""
    document = defect_handoff()
    key = defect_key(document)
    expect(not di.carries_key(None, key),
           "a missing body matches no publication key")
    with staged_evidence(document), census_file() as path:
        before = Path(path).read_bytes()
        publication = FakePublication(issues={key: {"number": 0,
                                                    "url": "not-a-url"}})
        expect_not_filed(
            lambda: file_defect(document, path, publication=publication),
            "not a positive integer",
            "a reconcile answering with an unusable issue number")
        expect_nothing_published(path, before, publication,
                                 "an unusable reconcile")


def test_the_publication_key_is_derived_from_the_attempt() -> None:
    """Stable across invocations, and different for a different attempt."""
    document = defect_handoff()
    first = defect_key(document)
    again = defect_key(copy.deepcopy(document))
    expect(first == again and re.fullmatch(r"[0-9a-f]{64}", first),
           f"the key is a stable sha256 of the attempt; got {first!r} and "
           f"{again!r}")
    other = defect_handoff(attempt="role-20260829T090000Z-4711-0000face")
    expect(defect_key(other) != first,
           "and another attempt of the same probe files under its own")


def test_the_filed_issue_enters_the_canonical_review_gate() -> None:
    """The routing metadata, spelled the way the gate reads it."""
    document = defect_handoff()
    with staged_evidence(document), census_file() as path:
        _published, publication, _probe, _pr = file_defect(
            document, path, origin="codex")
        body = publication.creates[0]["body"]
        marker = re.compile(r"<!--\s*issue-origin:(claude|codex)\s*-->")
        found = marker.search(body)
        expect(found is not None and found.group(1) == "codex",
               "the filed body carries the issue-origin marker the review "
               "gate routes on, spelling the brand it was filed by")
        expect(body.rstrip().endswith(di.origin_marker("codex")),
               "as its last line, where an origin marker belongs")
    with staged_evidence(document), census_file() as path:
        before = Path(path).read_bytes()
        publication = FakePublication()
        try:
            file_defect(document, path, publication=publication,
                        origin="nobody")
        except di.HandoffError as error:
            expect("issue-origin" in str(error),
                   f"an unknown brand is rejected naming the marker; got "
                   f"{error}")
        else:
            selftestlib.record_fail("an unknown issue origin was accepted")
        expect_nothing_published(path, before, publication,
                                 "an unknown issue origin", searched=0)


def test_the_origin_vocabulary_is_one_vocabulary() -> None:
    """The module and the schema spell the review gate's brands alike.

    `origin` is the one enum the census schema declares that also lives
    in a module constant, so the two are held to each other rather than
    left to drift the day a third brand appears.
    """
    declared = probe_census.load_schema()["$defs"]["outcome_issue"]
    expect(tuple(declared["properties"]["origin"]["enum"]) == di.ORIGINS,
           f"the schema enumerates {declared['properties']['origin']['enum']} "
           f"where the module knows {list(di.ORIGINS)}")
    expect(set(declared["required"])
           == {"number", "url", "publication_key", "origin"},
           f"and the stored issue identity is exactly what "
           f"require_issue_identity builds; got {declared['required']}")


def test_an_issue_with_no_readable_evidence_is_not_filed() -> None:
    """A machine-local pathname alone is not reviewable log evidence."""
    document = defect_handoff()
    with census_file() as path:
        before = Path(path).read_bytes()
        publication = FakePublication()
        expect_not_filed(
            lambda: file_defect(document, path, publication=publication),
            "no retained artifact",
            "an attempt whose artifacts have all been pruned")
        # Searched, and correctly so: the reconcile runs BEFORE anything
        # is rendered, so a retry whose issue already exists recovers
        # even with its artifacts gone. Only the case with no issue to
        # find reaches the evidence, and only then is there something to
        # file at all.
        expect_nothing_published(path, before, publication,
                                 "pruned artifacts", searched=1)
    # One readable run is enough, and only what was read is quoted.
    with staged_evidence(document, only=1), census_file() as path:
        _published, publication, _probe, _pr = file_defect(document, path)
        body = publication.creates[0]["body"]
        expect(body.count("#### baseline run ") == 1,
               "one readable run is evidence enough, and only it is quoted")


def test_a_symlinked_artifact_component_reaches_nothing() -> None:
    """This module QUOTES what it finds into a published issue.

    So a symlink under the declared artifact root is not a layout to
    read through: `engine -> elsewhere` would otherwise have every
    listing and open below it land there and publish whatever regular
    files live there as this probe's failure evidence. #1437's own
    canonical-path rule catches a run directory that was ALREADY a
    symlink when the handoff was validated; every component is opened
    `O_NOFOLLOW` anyway, which covers the rest of the tree and the race
    between that validation and this read.
    """
    document = defect_handoff()
    elsewhere = Path(tempfile.mkdtemp(prefix="deflake_elsewhere_"))
    try:
        (elsewhere / "secret.log").write_text("PRIVATE HOST STATE\n",
                                              encoding="utf-8")
        # Named the way a run directory's own files are named, so a
        # substituted directory would really be READ by an
        # implementation that followed the link rather than merely
        # finding nothing there.
        (elsewhere / "stdout.txt").write_text("PRIVATE HOST STATE\n",
                                              encoding="utf-8")
        runs = [Path(entry) for entry
                in document["diagnosis_outcome"]["retained_artifacts"]]

        # (a) `engine` is a symlink to somewhere else entirely.
        with staged_evidence(document), census_file() as path:
            shutil.rmtree(runs[0] / "engine")
            (runs[0] / "engine").symlink_to(elsewhere)
            _published, publication, _probe, _pr = file_defect(document, path)
            body = publication.creates[0]["body"]
            expect("PRIVATE HOST STATE" not in body,
                   "a symlinked engine directory is not descended")
            expect(DEFECT_STDOUT.strip() in body,
                   "while the run's own real files are still quoted")

        # (b) a symlinked artifact FILE, and a non-regular one. An open
        # that blocked on the FIFO would hang the workflow, not merely
        # read the wrong bytes.
        with staged_evidence(document, only=1), census_file() as path:
            (runs[0] / "stdout.txt").unlink()
            (runs[0] / "stdout.txt").symlink_to(elsewhere / "secret.log")
            log = runs[0] / "engine" / "engine-9101.log"
            log.unlink()
            if hasattr(os, "mkfifo"):
                os.mkfifo(log)
            _published, publication, _probe, _pr = file_defect(document, path)
            body = publication.creates[0]["body"]
            expect("PRIVATE HOST STATE" not in body,
                   "a symlinked artifact file is not read")
            expect(DEFECT_EVENTS.strip() in body,
                   "and the real protocol stream beside it still is")

        # (c) the run directory ITSELF substituted after validation —
        # the race #1437's gate cannot see, since it validated the real
        # path. Read directly, because a handoff declaring one would be
        # refused at the gate instead of reaching this.
        with staged_evidence(document, only=1):
            root = str(runs[0].parent.parent)
            substitute = runs[0].parent / "run-009"
            substitute.symlink_to(elsewhere)
            try:
                expect(di.run_excerpts(root, str(substitute)) == [],
                       "a substituted run directory yields no excerpt")
                expect(di.open_run_directory(root, str(elsewhere)) is None,
                       "and a run directory outside the declared root is "
                       "not walked at all")
                expect(len(di.run_excerpts(root, str(runs[0]))) >= 2,
                       "while the genuine directory still reads")
            finally:
                substitute.unlink()
    finally:
        shutil.rmtree(elsewhere, ignore_errors=True)


def test_the_quoted_evidence_is_bounded() -> None:
    """A whole engine log is not a review surface."""
    noisy = "".join(f"[World] line {index}\n" for index in range(5000))
    document = defect_handoff()
    with staged_evidence(document, engine=noisy), census_file() as path:
        _published, publication, _probe, _pr = file_defect(document, path)
        body = publication.creates[0]["body"]
        expect(len(body) <= did.MAX_BODY_CHARS,
               f"the body fits the tracker's limit; got {len(body)}")
        expect("[World] line 4999" in body and "[World] line 0\n" not in body,
               "and the TAIL of the log is what is quoted, which is where a "
               "failing run stops")
        expect(body.count("#### baseline run ") <= di.MAX_EVIDENCE_RUNS,
               f"over at most {di.MAX_EVIDENCE_RUNS} runs")
    # An engine log is arbitrary bytes and may contain a fence of its
    # own; quoting it in a three-backtick block would end the block early
    # and render the rest of the log as markdown.
    fenced = "before\n```\nstill the log\n````\nend of log\n"
    with staged_evidence(document, engine=fenced), census_file() as path:
        _published, publication, _probe, _pr = file_defect(document, path)
        body = publication.creates[0]["body"]
        expect("end of log" in body and "`````" in body,
               "a log carrying its own fence is quoted inside a longer one")


def test_a_route_this_workflow_does_not_own_files_nothing() -> None:
    """The sibling split, from #1438's side."""
    for route in (dd.ROUTE_CANNOT_REPRODUCE, dd.ROUTE_NO_CONFIDENT_FIX,
                  dd.ROUTE_PARTIAL_IMPROVEMENT, dd.ROUTE_NO_TARGET):
        document = outcome_handoff(route)
        with census_file() as path:
            before = Path(path).read_bytes()
            publication = FakePublication()
            expect_not_filed(
                lambda d=document, p=path, pub=publication: file_defect(
                    d, p, publication=pub),
                "#1439", f"the {route!r} route filed as a production defect")
            expect_nothing_published(path, before, publication,
                                     f"the {route!r} route", searched=0)


def test_untrustworthy_or_unreproduced_evidence_is_never_filed() -> None:
    """The evidence is judged BEFORE anything reaches the tracker."""
    def only_baseline(result, exit_code=probe_flake.EXIT_OK) -> list:
        return [{"role": do.ROLE_BASELINE, "exit_code": exit_code,
                 "result": result}]

    cases = (
        ("an incomplete run set",
         lambda: defect_handoff(measurements=only_baseline(short_result())),
         "completed 9 of 10"),
        ("an aggregate that contradicts its own run list",
         lambda: defect_handoff(
             measurements=only_baseline(forged_aggregate_result())),
         "measurement reports"),
        ("failures confined to checks nobody targeted",
         lambda: defect_handoff(
             measurements=only_baseline(elsewhere_failure_result())),
         "did not reproduce the pattern"),
        ("a baseline that reproduced nothing at all",
         lambda: defect_handoff(
             measurements=only_baseline(spotless_result())),
         "reproduced nothing to attribute"),
    )
    for label, build, fragment in cases:
        document = build()
        with staged_evidence(document), census_file() as path:
            before = Path(path).read_bytes()
            publication = FakePublication()
            expect_not_filed(
                lambda d=document, p=path, pub=publication: file_defect(
                    d, p, publication=pub),
                fragment, label)
            expect_nothing_published(path, before, publication, label,
                                     searched=0)


def test_a_publication_failure_leaves_the_attempt_pending() -> None:
    """Neither boundary failure records anything, and neither falls through."""
    document = defect_handoff()
    for label, publication, fragment in (
            ("a reconcile that failed",
             FakePublication(find_error=di.PublicationFailed(
                 "gh issue list exited 1")), "gh issue list"),
            ("a creation that failed",
             FakePublication(create_error=di.PublicationFailed(
                 "gh issue create exited 1")), "gh issue create"),
            ("a creation that answered with no issue number",
             FakePublication(answer={"url": "https://example.com/issues/7"}),
             "not a positive integer"),
    ):
        with staged_evidence(document), census_file() as path:
            before = Path(path).read_bytes()
            probe_spy, pr_spy = Spy(), Spy()
            expect_not_filed(
                lambda p=path, pub=publication: file_defect(
                    document, p, publication=pub, probe_spy=probe_spy,
                    pr_spy=pr_spy),
                fragment, label)
            expect(Path(path).read_bytes() == before
                   and not stored_outcomes(path),
                   f"{label}: something was recorded anyway")
            expect(probe_spy.calls == [] and pr_spy.calls == [],
                   f"{label}: a failure fell through to a forbidden "
                   f"boundary")


def test_a_malformed_defect_handoff_is_rejected_without_filing() -> None:
    """The shared entry gate, reached through this workflow's own route."""
    def broken(mutate):
        document = defect_handoff()
        mutate(document)
        return document

    cases = (
        ("a handoff on another schema",
         lambda: broken(lambda d: d.__setitem__("schema", "nope")),
         f"expected {di.HANDOFF_SCHEMA!r}"),
        ("a handoff with no attempt identity",
         lambda: broken(lambda d: d.pop("attempt")),
         "`attempt` identity"),
        ("a producer record with no diagnosis block",
         lambda: broken(lambda d: d["diagnosis_outcome"].pop("diagnosis")),
         "states no `diagnosis` block"),
        ("a diagnosis with no evidence",
         lambda: broken(lambda d: d["diagnosis_outcome"]["diagnosis"]
                        .__setitem__("evidence", [])),
         "records no evidence"),
        ("a diagnosis with no summary",
         lambda: broken(lambda d: d["diagnosis_outcome"]["diagnosis"]
                        .__setitem__("summary", "  ")),
         "states no `summary`"),
        ("a retained artifact inside a comparison worktree",
         lambda: broken(lambda d: d["diagnosis_outcome"].__setitem__(
             "retained_artifacts", [f"{CLEAN_WT}/artifacts/run-001"])),
         "inside the worktree"),
        ("a measurement taken at another instant",
         lambda: broken(lambda d: d["measurements"][0]["result"]
                        .__setitem__("timestamp_utc",
                                     "2026-08-22T09:30:00Z")),
         "timestamp_utc"),
        ("a verification batch this route never runs",
         lambda: broken(lambda d: d["measurements"].append(
             {"role": do.ROLE_VERIFICATION,
              "exit_code": probe_flake.EXIT_OK,
              "result": verification_result()})),
         "runs no verification batch"),
    )
    for label, build, fragment in cases:
        document = build()
        with census_file() as path:
            before = Path(path).read_bytes()
            publication = FakePublication()
            try:
                file_defect(document, path, publication=publication)
            except di.HandoffError as error:
                expect(fragment in str(error),
                       f"{label}: rejected, but for {str(error)!r} rather "
                       f"than {fragment!r}")
            except di.NonSuccess as error:
                selftestlib.record_fail(
                    f"{label}: refused the EVIDENCE ({error}) "
                    f"where the input should have been rejected")
            else:
                selftestlib.record_fail(f"{label}: accepted")
            expect_nothing_published(path, before, publication, label,
                                     searched=0)


def test_the_census_schema_pairs_the_outcome_with_its_issue() -> None:
    """Declared, so neither half can be recorded without the other."""
    document = defect_handoff()
    with staged_evidence(document), census_file() as path:
        published, _pub, _probe, _pr = file_defect(document, path)
        record = copy.deepcopy(published.record)

    with census_file() as path:
        without = {key: value for key, value in record.items()
                   if key != "issue"}
        try:
            probe_census.record_outcome(path, PROBE, without)
        except probe_census.CensusError:
            pass
        else:
            selftestlib.record_fail(
                "a production defect was recorded with no issue")
        expect(not stored_outcomes(path),
               "and nothing was stored by the refusal")

    with census_file() as path:
        stable = copy.deepcopy(record)
        stable["outcome"] = do.OUTCOME_CANNOT_REPRODUCE
        stable["recommendation"] = {"action": "de-list", "advisory": True,
                                    "detail": "nothing reproduced"}
        try:
            probe_census.record_outcome(path, PROBE, stable)
        except probe_census.CensusError:
            pass
        else:
            selftestlib.record_fail(
                "a stable outcome was recorded carrying an issue")
        expect(not stored_outcomes(path),
               "and nothing was stored by that refusal either")


def test_a_recorded_defect_resumes_after_its_artifacts_are_pruned() -> None:
    """The durable record outlives the evidence it was built from.

    Retained artifacts live in the harness's tree outside every worktree
    and are swept like any other scratch. A resume that re-collected
    evidence would fail on exactly the thing the census record exists to
    make unnecessary, so completion is checked before anything is
    rendered at all.
    """
    document = defect_handoff()
    with census_file() as path:
        with staged_evidence(document):
            first, _publication, _probe, _pr = file_defect(document, path)
        # The artifact tree is gone now, and `collect_evidence` would
        # refuse over it. The completed attempt must not care.
        before = Path(path).read_bytes()
        again, publication, probe_spy, pr_spy = file_defect(
            document, path, origin="codex", now="2026-09-01T08:00:00Z")
        expect(again.resumed is True and again.record == first.record,
               f"a recorded attempt resumes on the record alone; got "
               f"{again.to_document()}")
        expect(publication.finds == [] and publication.creates == [],
               "without reaching the tracker")
        expect(Path(path).read_bytes() == before
               and len(stored_outcomes(path)) == 1,
               "and without touching the census")
        expect(probe_spy.calls == [] and pr_spy.calls == [],
               "or any forbidden boundary")


def test_a_crash_window_retry_recovers_after_the_artifacts_are_swept()\
        -> None:
    """The recovery path must not depend on the evidence either.

    Issue creation took effect, the census refused, and the artifact
    tree was swept before anyone retried. The issue is durable and the
    publication key is on it, so the retry has everything it needs — but
    only if the reconcile runs BEFORE the body is rendered. Rendering
    first would refuse for want of evidence and strand an issue that
    already exists.
    """
    document = defect_handoff()
    publication = FakePublication()
    with census_file() as path:
        with staged_evidence(document):
            healthy = json.loads(Path(path).read_text(encoding="utf-8"))
            Path(path).write_text(
                json.dumps(dict(healthy, schema=probe_census.SEED_SCHEMA)),
                encoding="utf-8")
            expect_not_filed(
                lambda: file_defect(document, path,
                                    publication=publication),
                "exists", "a census that refused after the issue was created")
            expect(len(publication.creates) == 1,
                   "the issue was created before the census refused")

        # The artifacts are gone now, and the census works again.
        Path(path).write_text(json.dumps(healthy), encoding="utf-8")
        published, _pub, probe_spy, pr_spy = file_defect(
            document, path, publication=publication)
        expect(published.reconciled is True and published.created is False,
               f"the retry reconciles the issue that already exists; got "
               f"{published.to_document()}")
        expect(len(publication.creates) == 1,
               f"creating nothing; got {len(publication.creates)} creation(s)")
        recorded = stored_outcomes(path)
        expect(len(recorded) == 1
               and recorded[0]["issue"]["number"] == 901,
               f"and records it exactly once; got {recorded}")
        expect(probe_spy.calls == [] and pr_spy.calls == [],
               "with neither forbidden boundary reached")


def test_a_reconciled_issue_supplies_its_own_review_brand() -> None:
    """The brand is the ISSUE's, not the resuming invocation's.

    A Claude-origin creation whose census write failed, resumed under a
    Codex invocation, still routes to Claude's opposite brand. Recording
    the retry's own brand would put a second, false answer in the
    durable history — and it is the answer the review gate acts on.
    """
    document = defect_handoff()
    key = defect_key(document)
    with staged_evidence(document), census_file() as path:
        publication = FakePublication()
        healthy = json.loads(Path(path).read_text(encoding="utf-8"))
        Path(path).write_text(
            json.dumps(dict(healthy, schema=probe_census.SEED_SCHEMA)),
            encoding="utf-8")
        expect_not_filed(
            lambda: file_defect(document, path, publication=publication,
                                origin="claude"),
            "exists", "a census that refused after the issue was created")

        Path(path).write_text(json.dumps(healthy), encoding="utf-8")
        published, _pub, _probe, _pr = file_defect(
            document, path, publication=publication, origin="codex")
        expect(published.reconciled is True
               and published.record["issue"]["origin"] == "claude",
               f"the reconciled issue's own brand is recorded; got "
               f"{published.record['issue']}")

    # An issue carrying the key but no readable origin marker is not one
    # this workflow filed, so it is a publication failure rather than
    # something to record under the caller's guess.
    with staged_evidence(document), census_file() as path:
        before = Path(path).read_bytes()
        publication = FakePublication(issues={key: {
            "number": 77,
            "url": "https://github.com/coghex/synarchy/issues/77",
            "body": f"someone else's issue\n{di.key_marker(key)}\n"}})
        expect_not_filed(
            lambda: file_defect(document, path, publication=publication),
            di.ORIGIN_MARKER,
            "a reconciled issue with no origin marker")
        expect_nothing_published(path, before, publication,
                                 "an unbranded reconcile")


def test_a_key_quoted_inside_a_code_fence_is_not_a_reconcile() -> None:
    """A filed issue QUOTES engine logs, and a log can say anything.

    So the marker has to be a standalone line outside every fence: a
    duplicate report that pasted this body into a code block would
    otherwise be reconciled as the publication, and the real defect
    would never be filed.
    """
    document = defect_handoff()
    key = defect_key(document)
    marker = di.key_marker(key)
    expect(di.carries_key(f"prose\n{marker}\nmore", key),
           "a standalone marker line is what a filed issue carries")
    expect(not di.carries_key(f"```\n{marker}\n```\n", key),
           "one inside a fence is a quotation of some other issue")
    expect(not di.carries_key(f"see {marker} above", key),
           "and one embedded in a sentence is not a marker line at all")
    expect(di.carries_key(f"````\nlog\n````\n{marker}\n", key),
           "a longer fence closes, so what follows it is read again")
    expect(not di.carries_key(f"```\nlog\n{marker}\n", key),
           "while an unterminated fence swallows the rest, which is the "
           "safe direction")
    expect(di.body_origin(f"```\n{di.origin_marker('codex')}\n```\n") is None
           and di.body_origin(di.origin_marker("codex")) == "codex",
           "and the origin marker is read under the same rule")

    with staged_evidence(document), census_file() as path:
        before = Path(path).read_bytes()
        quoted = FakePublication(issues={key: {
            "number": 88,
            "url": "https://github.com/coghex/synarchy/issues/88",
            "body": f"a duplicate report:\n\n```\n{marker}\n```\n"}})
        expect_not_filed(
            lambda: file_defect(document, path, publication=quoted),
            "carries no", "an issue that only quotes the key in a fence")
        expect_nothing_published(path, before, quoted,
                                 "a quoted-key reconcile")


def test_the_diagnosis_prose_is_bounded_at_the_gate() -> None:
    """#1437 bounds neither the summary nor the evidence list; this does.

    Refused rather than trimmed: the summary is the issue's own claim
    and the evidence is what makes it reviewable, so a body that cut
    either down would publish a defect report whose claim had been
    edited by the publisher.
    """
    cases = (
        ("a summary longer than a body",
         {"summary": "z" * (di.MAX_DIAGNOSIS_SUMMARY + 1),
          "evidence": ["run 1's log"], "category": None},
         "`summary` is"),
        ("more evidence lines than a body carries",
         {"summary": "the world thread raced",
          "evidence": ["line"] * (di.MAX_DIAGNOSIS_EVIDENCE + 1),
          "category": None},
         "evidence lines, over the"),
        ("one evidence line longer than a body carries",
         {"summary": "the world thread raced",
          "evidence": ["z" * (di.MAX_DIAGNOSIS_EVIDENCE_ITEM + 1)],
          "category": None},
         "evidence line 1 is"),
    )
    for label, block, fragment in cases:
        document = defect_handoff(diagnosis=block)
        with census_file() as path:
            before = Path(path).read_bytes()
            publication = FakePublication()
            try:
                file_defect(document, path, publication=publication)
            except di.HandoffError as error:
                expect(fragment in str(error),
                       f"{label}: rejected, but for {str(error)!r} rather "
                       f"than {fragment!r}")
            except di.NonSuccess as error:
                selftestlib.record_fail(
                    f"{label}: refused the EVIDENCE ({error}) "
                    f"where the input should have been rejected")
            else:
                selftestlib.record_fail(f"{label}: accepted")
            expect_nothing_published(path, before, publication, label,
                                     searched=0)
    # And a body that still cannot fit refuses rather than publishing one
    # with its measurements or its log evidence sliced away.
    document = defect_handoff()
    saved = did.MAX_BODY_CHARS
    did.MAX_BODY_CHARS = 400
    try:
        with staged_evidence(document), census_file() as path:
            before = Path(path).read_bytes()
            publication = FakePublication()
            expect_not_filed(
                lambda: file_defect(document, path, publication=publication),
                "every part of it that is left is required",
                "a body no trimming can fit")
            expect_nothing_published(path, before, publication,
                                     "an unfittable body", searched=1)
    finally:
        did.MAX_BODY_CHARS = saved


def test_the_tail_read_is_whole_lines_and_tolerates_junk_bytes() -> None:
    """The two properties of the bounded tail read (#1438, #2157).

    Only `MAX_READ_BYTES` is read off the end of an engine log, so the
    first line of that window is whatever straddled the boundary — a
    FRAGMENT. Publishing it would quote half a line as this probe's
    failure evidence, so it is dropped and every quoted line is a whole
    one. And a macOS engine log carries GLFW's junk, so the window is
    decoded with `errors="replace"` rather than raising: evidence that
    exists must not be discarded because one byte is not UTF-8.

    Driven through `run_excerpts` on a log LARGER than the read window,
    because neither property is reachable otherwise. `MAX_EXCERPT_LINES`
    and `MAX_EXCERPT_CHARS` bound the excerpt of a small log to the same
    shape without the window ever moving off zero, so a fixture that
    fits in one read proves nothing about either.
    """
    document = defect_handoff()
    tail = "".join(f"[World] tail line {index}\n" for index in range(5))
    # The straddling line ends in a token nothing else carries, and the
    # bounds keep the END of a clipped excerpt — so the token survives
    # `MAX_EXCERPT_CHARS` and its absence is the partial-line rule's
    # doing rather than the character bound's.
    giant = "G" * die.MAX_READ_BYTES + "FRAGMENT-END\n"
    prefix = "[World] before the window\n" * 8
    with staged_evidence(document, only=1) as staged:
        run = staged[0]
        root = str(run.parent.parent)
        log = run / "engine" / "engine-9101.log"
        log.write_text(prefix + giant + tail, encoding="utf-8")
        size = log.stat().st_size
        start = size - die.MAX_READ_BYTES
        expect(len(prefix) < start < len(prefix) + len(giant),
               f"the fixture must put the read window's start INSIDE the "
               f"straddling line, or the partial-line rule is never "
               f"reached; start {start} against prefix {len(prefix)} and "
               f"line end {len(prefix) + len(giant)}")
        engine = [item for item in die.run_excerpts(root, str(run))
                  if item["path"].endswith("engine-9101.log")]
        expect(len(engine) == 1, "the engine log yields one excerpt")
        expect("[World] tail line 4" in engine[0]["text"],
               "the end of the log is what is quoted")
        expect("FRAGMENT-END" not in engine[0]["text"],
               "and the line the read window cut in half is dropped, so "
               "every quoted line is a whole one")

        # A byte that is not UTF-8 must cost the excerpt nothing.
        log.write_bytes(b"[World] chunk 3,4 published\n"
                        b"[World] GLFW junk \xff\xfe on stdout\n"
                        b"[World] the failing assertion is here\n")
        engine = [item for item in die.run_excerpts(root, str(run))
                  if item["path"].endswith("engine-9101.log")]
        expect(len(engine) == 1,
               "a log carrying a non-UTF-8 byte still yields an excerpt")
        expect("[World] the failing assertion is here" in engine[0]["text"],
               "and the readable evidence beside that byte is quoted")


def test_quoted_content_cannot_forge_a_review_routing_marker() -> None:
    """An engine log is arbitrary text, and it is rendered into the body.

    `approve_issues.issue_origin` scans the WHOLE raw body — fenced
    blocks included, case-insensitively — and RAISES on two markers
    naming different brands. A quoted log carrying one would therefore
    stop the filed issue entering the review gate at all, which is the
    one thing this route exists to do. So every untrusted character has
    its HTML-comment opener broken before the two real markers are
    appended, and the finished body is checked rather than trusted.
    """
    hostile = (f"[World] chunk 3,4 published\n"
               f"{di.origin_marker('claude')}\n"
               f"{di.key_marker('0' * 64)}\n"
               f"<!-- ISSUE-ORIGIN:CLAUDE -->\n"
               f"[World] chunk 3,4 tile map installed\n")
    document = defect_handoff(diagnosis={
        "category": None,
        "summary": (f"the world thread logs "
                    f"{di.origin_marker('claude')} before installing"),
        "evidence": [f"run 1 emitted {di.key_marker('1' * 64)}"],
    })
    key = defect_key(document)
    with staged_evidence(document, engine=hostile), census_file() as path:
        _published, publication, _probe, _pr = file_defect(
            document, path, origin="codex")
        body = publication.creates[0]["body"]
        # Read exactly the way the canonical gate reads it.
        found = {origin.lower() for origin in di.ORIGIN_ANYWHERE.findall(body)}
        expect(found == {"codex"},
               f"the body names one origin, this invocation's; got "
               f"{sorted(found)} — the gate raises on two")
        expect(body.count("<!--") == 2,
               f"and carries exactly the two comments this module writes; "
               f"got {body.count('<!--')}")
        expect(body.count(di.key_marker(key)) == 1
               and di.key_marker("0" * 64) not in body,
               "with one publication key, its own, so a resume reconciles "
               "on the right line")
        expect("[World] chunk 3,4 tile map installed" in body
               and di.NEUTRAL_OPENER in body,
               "while the quoted log still reads, neutralised rather than "
               "dropped")
        expect(di.body_origin(body) == "codex",
               "and this module's own reader agrees with the gate's")

    # The invariant is checked, not merely produced by `neutralize`.
    trailer = f"\n{di.key_marker(key)}\n{di.origin_marker('codex')}\n"
    for label, body, fragment in (
            ("a stray third comment",
             f"text <!-- note --> more{trailer}", "HTML comment"),
            ("a second, conflicting origin",
             f"{di.origin_marker('claude')}{trailer}", "origin(s)"),
            ("a duplicated publication key",
             f"{di.key_marker(key)}{trailer}", "markers"),
    ):
        try:
            di.require_one_marker_each(body, key=key, origin="codex")
        except di.NonSuccess as error:
            expect(fragment in str(error),
                   f"{label}: refused, but for {str(error)!r} rather than "
                   f"{fragment!r}")
        else:
            selftestlib.record_fail(f"{label}: accepted")
    di.require_one_marker_each(f"clean body{trailer}", key=key,
                               origin="codex")


def test_the_defect_command_line_reports_each_ending() -> None:
    """The endings this workflow has, through the shipped entry point."""
    tool = str(Path(__file__).resolve().parent / "deflake_issue.py")
    document = defect_handoff()
    with staged_evidence(document), census_file() as path:
        root = Path(path).parent
        accepted = root / "defect.json"
        accepted.write_text(json.dumps(document), encoding="utf-8")

        before = Path(path).read_bytes()
        done = subprocess.run(
            [sys.executable, tool, "--handoff", str(accepted),
             "--census", str(path), "--dry-run", "--json",
             "--origin", "claude"],
            capture_output=True, text=True, timeout=120)
        expect(done.returncode == di.EXIT_OK,
               f"a dry run exits 0; got {done.returncode} "
               f"({done.stderr.strip()[:200]})")
        try:
            rendered = json.loads(done.stdout)
        except json.JSONDecodeError:
            rendered = {}
        expect(rendered.get("published") is False
               and DEFECT_DIAGNOSIS["summary"] in rendered.get("body", ""),
               f"rendering the issue without filing it; got "
               f"{done.stdout[:200]}")
        expect(Path(path).read_bytes() == before
               and not stored_outcomes(path),
               "and recording nothing")

        for extra in ([], ["--dry-run"]):
            done = subprocess.run(
                [sys.executable, tool, "--handoff", str(accepted),
                 "--census", str(path), *extra],
                capture_output=True, text=True, timeout=120)
            expect(done.returncode == di.EXIT_REJECTED
                   and "issue-origin" in done.stderr,
                   f"a run with no origin{' (dry)' if extra else ''} is "
                   f"rejected naming the marker; got {done.returncode} "
                   f"({done.stderr.strip()[:200]})")

        sibling = root / "sibling.json"
        sibling.write_text(json.dumps(outcome_handoff()), encoding="utf-8")
        done = subprocess.run(
            [sys.executable, tool, "--handoff", str(sibling),
             "--census", str(path), "--origin", "claude"],
            capture_output=True, text=True, timeout=120)
        expect(done.returncode == di.EXIT_NON_SUCCESS
               and "#1439" in done.stderr,
               f"a sibling route exits 3 naming its owner; got "
               f"{done.returncode} ({done.stderr.strip()[:200]})")

        malformed = root / "malformed.json"
        malformed.write_text(json.dumps({"schema": "nope"}), encoding="utf-8")
        done = subprocess.run(
            [sys.executable, tool, "--handoff", str(malformed),
             "--census", str(path), "--origin", "claude"],
            capture_output=True, text=True, timeout=120)
        expect(done.returncode == di.EXIT_REJECTED,
               f"a malformed handoff exits 2; got {done.returncode}")
        expect("Traceback" not in done.stderr,
               f"and never as a traceback\n{done.stderr[:400]}")
        expect(Path(path).read_bytes() == before,
               "and none of those endings touched the census")


def test_the_handoff_facade_exports_the_canonical_objects() -> None:
    """#2180: the façade binds its owners' objects, it does not copy them.

    `tools/deflake_handoff.py` is a re-export façade over four internal
    owners, so every name a consumer reads through it has to be the ONE
    object its owner defines. A copied alias would be a second
    definition free to drift: `except deflake_outcome.HandoffError`
    would stop catching what `deflake_handoff` raises, and an
    `isinstance` against either `Measurement` would answer differently
    depending on which module the caller imported.

    Asserted here rather than left to inspection because #2097's
    compatibility bindings in `deflake_outcome.py` are what the rest of
    the repository imports, and nothing else executes the claim.
    """
    for name in ("HandoffError", "NonSuccess", "Measurement", "Handoff",
                 "RouteOwnership"):
        expect(getattr(do, name) is getattr(deflake_handoff, name),
               f"deflake_outcome.{name} must BE deflake_handoff.{name}, "
               f"not a copy")
    for name in deflake_handoff.__all__:
        expect(hasattr(deflake_handoff, name),
               f"the façade declares {name} in __all__ but does not bind it")
    owners = ("deflake_handoff_grammar", "deflake_handoff_measurement",
              "deflake_handoff_producer", "deflake_handoff_assembly")
    modules = {name: importlib.import_module(name) for name in owners}
    for name in deflake_handoff.__all__:
        bound = getattr(deflake_handoff, name)
        defining = [module for module in modules.values()
                    if getattr(module, name, None) is bound]
        expect(defining,
               f"{name} is on the façade but no internal owner defines it")


def test_the_handoff_owners_stay_one_way() -> None:
    """#2180: the four owners form an acyclic chain, and nothing above it.

    The whole point of extracting them is that grammar, measurement,
    producer binding and assembly can change independently. A back-edge
    would restore exactly the entanglement #2097 removed: an owner that
    imported the façade would be importing its own siblings through a
    module whose only job is to re-export them, and an owner that
    imported either consumer would make the two consumers each other's
    prerequisite again.

    The reverse reference `require_reproduced` needs — its `Handoff`
    annotation — is pinned rather than excused: it must sit inside a
    `TYPE_CHECKING` guard, where it is evaluated by a type checker and
    never at run time, so the runtime graph stays one-way.
    """
    order = ["deflake_handoff_grammar", "deflake_handoff_measurement",
             "deflake_handoff_producer", "deflake_handoff_assembly"]
    forbidden = {"deflake_handoff", "deflake_outcome", "deflake_issue"}
    directory = Path(dd.__file__).resolve().parent
    for position, owner in enumerate(order):
        source = (directory / f"{owner}.py").read_text(encoding="utf-8")
        tree = ast.parse(source)
        guarded, runtime = set(), set()
        for node in ast.walk(tree):
            if not isinstance(node, (ast.Import, ast.ImportFrom)):
                continue
            names = ({alias.name for alias in node.names}
                     if isinstance(node, ast.Import) else {node.module or ""})
            runtime |= names
        for node in ast.walk(tree):
            if not isinstance(node, ast.If):
                continue
            test = node.test
            if not (isinstance(test, ast.Name) and test.id == "TYPE_CHECKING"):
                continue
            for inner in ast.walk(node):
                if isinstance(inner, ast.Import):
                    guarded |= {alias.name for alias in inner.names}
                elif isinstance(inner, ast.ImportFrom):
                    guarded.add(inner.module or "")
        runtime -= guarded
        for name in sorted(runtime & forbidden):
            expect(False,
                   f"{owner} imports {name}; an internal owner depends on "
                   f"neither the façade nor either consumer")
        for name in sorted(guarded & forbidden):
            expect(False,
                   f"{owner} type-imports {name}; the façade and the "
                   f"consumers are off-limits even under TYPE_CHECKING")
        later = set(order[position + 1:])
        for name in sorted(runtime & later):
            expect(False,
                   f"{owner} imports {name} at run time, which is later in "
                   f"the one-way order {' -> '.join(order)}")
        for name in sorted(guarded & later):
            expect(name == "deflake_handoff_assembly"
                   and owner == "deflake_handoff_measurement",
                   f"{owner} type-imports {name}; the only permitted "
                   f"reverse reference is require_reproduced's Handoff "
                   f"annotation")


def test_the_handoff_family_imports_as_repository_modules() -> None:
    """#2180: every owner resolves under the `tools.` package spelling too.

    `tools/` carries no `__init__.py`, so it is an implicit namespace
    package: `import tools.deflake_handoff` from the repository root is
    a supported spelling, and under it the directory holding these
    modules is NOT on `sys.path`. Sibling imports by bare name resolve
    anyway only because each module inserts its own directory first —
    which the pre-split `deflake_handoff.py` did before importing
    `deflake_diagnosis`, and which the façade must keep doing before the
    first of its re-exports, since those run at import time.

    Asserted for the whole family rather than the façade alone because
    the same bootstrap is what makes each owner importable on its own,
    and a new owner added without one would fail the same way.
    """
    root = Path(dd.__file__).resolve().parent.parent
    family = ("tools.deflake_handoff", "tools.deflake_handoff_grammar",
              "tools.deflake_handoff_measurement",
              "tools.deflake_handoff_producer",
              "tools.deflake_handoff_assembly",
              "tools.deflake_outcome", "tools.deflake_issue")
    environment = dict(os.environ)
    environment.pop("PYTHONPATH", None)
    for module in family:
        done = subprocess.run(
            [sys.executable, "-c", f"import {module}"],
            cwd=str(root), capture_output=True, text=True, timeout=120,
            env=environment)
        expect(done.returncode == 0,
               f"`import {module}` from the repository root must resolve; "
               f"exited {done.returncode}\n{done.stderr[-400:]}")


def issue_family_dependencies(source: str) -> set:
    """Every `tools/` module one owner of the issue family depends on.

    An import-node scan is not enough here. The family resolves each
    dependency with `_sibling("<name>")` so that the `tools.` and bare
    spellings of a module are the SAME object (#2157), and a scan that
    only walked `ast.Import` would see no edges at all — passing the
    acyclicity and no-implementation cases vacuously while a back-edge
    sat in plain sight. So the literal argument of every `_sibling` call
    counts as a dependency, exactly as an import would.
    """
    found = set()
    for node in ast.walk(ast.parse(source)):
        if isinstance(node, ast.Import):
            found |= {alias.name for alias in node.names}
        elif isinstance(node, ast.ImportFrom):
            found.add(node.module or "")
        elif (isinstance(node, ast.Call)
              and isinstance(node.func, ast.Name)
              and node.func.id == "_sibling"
              and node.args
              and isinstance(node.args[0], ast.Constant)
              and isinstance(node.args[0].value, str)):
            found.add(node.args[0].value)
    return found


def test_the_issue_facade_exports_the_canonical_objects() -> None:
    """#2157: the façade binds its owners' objects, it does not copy them.

    `tools/deflake_issue.py` is the route's public import façade over
    four owners, so every name a consumer reads through it has to be the
    ONE object its owner defines. Two of them are load-bearing by name:
    `PublicationFailed` is caught with `except` against the façade
    spelling while the tracker raises the owner's, so a second class
    definition would silently stop matching; and `Publication` is
    SUBCLASSED by this file's own `FakePublication`, so the façade name
    must be the interface `GitHubPublication` implements.

    `MAX_BODY_CHARS` is asserted ABSENT for the opposite reason. It is
    the one constant of the family a caller substitutes, `issue_body`
    reads it out of the document owner's globals, and a façade binding
    would take the assignment and change nothing — leaving the
    unfittable-body refusal unexercised while the case appeared to drive
    it.
    """
    owners = ("deflake_issue_evidence", "deflake_issue_document",
              "deflake_issue_tracker", "deflake_issue_record")
    modules = {name: importlib.import_module(name) for name in owners}
    expect(di.PublicationFailed is modules["deflake_issue_tracker"]
           .PublicationFailed,
           "deflake_issue.PublicationFailed must BE the tracker owner's "
           "class, not a copy; `except` matches by identity")
    expect(di.Publication is modules["deflake_issue_tracker"].Publication,
           "deflake_issue.Publication must BE the tracker owner's "
           "interface; FakePublication subclasses it")
    expect(issubclass(modules["deflake_issue_tracker"].GitHubPublication,
                      di.Publication),
           "the gh-backed publisher must implement the interface the "
           "façade exports")
    defined_here = {"CHANGES_THE_PROBE", "Defect", "EXIT_NON_SUCCESS",
                    "EXIT_OK", "EXIT_REJECTED", "HANDOFF_SCHEMA",
                    "HandoffError", "NonSuccess", "OPENS_PULL_REQUEST",
                    "OUTCOME_PRODUCTION_DEFECT", "OWNED", "OWNER_ISSUE",
                    "ROLES", "ROLE_BASELINE", "ROLE_HANDOFF",
                    "ROLE_VERIFICATION", "ROUTE", "accept",
                    "forbidden_probe_change", "forbidden_pull_request",
                    "main", "publish", "render", "require_defect_diagnosis",
                    "require_handoff", "require_origin"}
    for name in di.__all__:
        expect(hasattr(di, name),
               f"the façade declares {name} in __all__ but does not bind it")
        if name in defined_here:
            continue
        bound = getattr(di, name)
        defining = [owner for owner, module in modules.items()
                    if getattr(module, name, None) is bound]
        expect(defining,
               f"{name} is re-exported by the façade but no owner defines "
               f"that exact object; a compatibility export must be the "
               f"canonical one")
    expect(not hasattr(di, "MAX_BODY_CHARS"),
           "MAX_BODY_CHARS must NOT be bound on the façade: it is the "
           "substituted constant, and an inert alias would swallow the "
           "assignment that exercises the unfittable-body refusal")
    for name in sorted(defined_here):
        expect(name in di.__all__,
               f"{name} is the façade's own and belongs in __all__")


def test_the_issue_owners_stay_one_way() -> None:
    """#2157: the four owners are acyclic, with one permitted sibling edge.

    The whole point of extracting them is that artifact traversal,
    issue rendering, the tracker boundary and the durable record can
    change independently. An owner that imported the façade would be
    importing its own siblings through a module whose other job is to
    orchestrate them, and an owner that imported either sibling
    consumer would make the two consumers each other's prerequisite.

    Exactly one edge between two extracted owners is permitted, and it
    is required rather than tolerated: the tracker CALLS the document
    owner's `carries_key` and `body_origin` instead of restating them,
    because a second spelling of the standalone-marker rule would let a
    search-index match be recorded as this attempt's publication.
    """
    family = {"deflake_issue_evidence", "deflake_issue_document",
              "deflake_issue_tracker", "deflake_issue_record"}
    permitted = {("deflake_issue_tracker", "deflake_issue_document")}
    forbidden = {"deflake_issue", "deflake_outcome"}
    directory = Path(dd.__file__).resolve().parent
    for owner in sorted(family):
        source = (directory / f"{owner}.py").read_text(encoding="utf-8")
        imported = issue_family_dependencies(source)
        expect(imported,
               f"{owner} reads as depending on nothing at all, which means "
               f"the dependency scan has stopped seeing this family's "
               f"edges rather than that the file has none")
        for name in sorted(imported & forbidden):
            expect(False,
                   f"{owner} imports {name}; an extracted owner depends on "
                   f"neither the façade nor the sibling consumer")
        for name in sorted((imported & family) - {owner}):
            expect((owner, name) in permitted,
                   f"{owner} imports {name}; the only permitted edge "
                   f"between two owners is tracker -> document")
    tracker = importlib.import_module("deflake_issue_tracker")
    document = importlib.import_module("deflake_issue_document")
    for name in ("carries_key", "body_origin"):
        expect(getattr(tracker, name) is getattr(document, name),
               f"the tracker must CALL the document owner's {name}, not "
               f"carry a second copy of the standalone-marker rule")


def test_the_issue_facade_keeps_only_what_it_composes() -> None:
    """#2157: the extracted implementations have one home each.

    The façade keeps route admission, `render`'s composition across
    three owners, `publish`'s exact statement order and the command
    line. What it must NOT still carry is a second copy of anything
    extracted — a walker, a body renderer, a `gh` adapter or a census
    record builder left behind would be a definition free to drift from
    the one its owner exports, and the compatibility bindings above
    would go on resolving to the owner while the façade's own callers
    used the stale twin.
    """
    directory = Path(dd.__file__).resolve().parent
    facade = ast.parse((directory / "deflake_issue.py")
                       .read_text(encoding="utf-8"))
    defined = {node.name for node in facade.body
               if isinstance(node, (ast.FunctionDef, ast.ClassDef))}
    moved = {
        "open_run_directory": "deflake_issue_evidence",
        "run_excerpts": "deflake_issue_evidence",
        "collect_evidence": "deflake_issue_evidence",
        "failing_runs": "deflake_issue_evidence",
        "excerpt": "deflake_issue_evidence",
        "issue_body": "deflake_issue_document",
        "issue_title": "deflake_issue_document",
        "publication_key": "deflake_issue_document",
        "neutralize": "deflake_issue_document",
        "require_one_marker_each": "deflake_issue_document",
        "carries_key": "deflake_issue_document",
        "body_origin": "deflake_issue_document",
        "prose_lines": "deflake_issue_document",
        "GitHubPublication": "deflake_issue_tracker",
        "Publication": "deflake_issue_tracker",
        "PublicationFailed": "deflake_issue_tracker",
        "require_issue_identity": "deflake_issue_tracker",
        "require_reconciled_issue": "deflake_issue_tracker",
        "outcome_record": "deflake_issue_record",
        "stored_record": "deflake_issue_record",
        "reuse_stored_publication": "deflake_issue_record",
        "require_supported": "deflake_issue_record",
        "Published": "deflake_issue_record",
    }
    for name, owner in sorted(moved.items()):
        expect(name not in defined,
               f"the façade still defines {name}; it belongs to {owner} and "
               f"a second definition is free to drift from it")
    for name in ("require_defect_diagnosis", "require_origin",
                 "require_handoff", "accept", "render", "publish", "main",
                 "forbidden_probe_change", "forbidden_pull_request"):
        expect(name in defined,
               f"the façade must still define {name}: route admission, "
               f"composition, ordering and the CLI stay here")
    # `subprocess` and `tempfile` are the `gh` adapter's, `stat` the
    # walker's, and `hashlib` the publication key's. None of the four
    # has a caller left on the façade, and an import of one is the first
    # sign an implementation came back.
    imported = issue_family_dependencies(
        (directory / "deflake_issue.py").read_text(encoding="utf-8"))
    for name in ("deflake_issue_document", "deflake_issue_evidence",
                 "deflake_issue_record", "deflake_issue_tracker"):
        expect(name in imported,
               f"the façade must compose {name}; a scan that cannot see "
               f"that edge cannot see a stale implementation either")
    for name in ("subprocess", "tempfile", "stat", "hashlib", "probe_flake",
                 "probe_protocol", "probe_runner_registry"):
        expect(name not in imported,
               f"the façade imports {name}, which only an extracted "
               f"implementation needs")


def test_the_issue_family_is_one_module_under_either_spelling() -> None:
    """#2157: `tools.<name>` and the bare name must not be two modules.

    `tools/` is an implicit namespace package, so every file in it has
    two import spellings and Python treats them as different modules. A
    façade loaded as `tools.deflake_issue` that resolved its owners by
    BARE name would therefore load a second copy of each, and every
    guarantee this split rests on would be false in that process:
    `tools.deflake_issue.issue_body is not
    tools.deflake_issue_document.issue_body`, `except
    tools.deflake_issue.PublicationFailed` would stop catching what
    `tools.deflake_issue_tracker` raises, and lowering
    `tools.deflake_issue_document.MAX_BODY_CHARS` would leave the module
    that actually renders untouched.

    Asserted in ONE fresh interpreter per spelling, because that is the
    only place the defect exists — each module imports fine on its own,
    and the compatibility cases above run under the bare spelling where
    a bare-name resolution looks correct.
    """
    root = Path(dd.__file__).resolve().parent.parent
    environment = dict(os.environ)
    environment.pop("PYTHONPATH", None)
    programs = {
        "the tools. spelling": """
import sys
import tools.deflake_issue_document as document
import tools.deflake_issue_evidence as evidence
import tools.deflake_issue_record as record
import tools.deflake_issue_tracker as tracker
import tools.deflake_issue as facade
assert facade.issue_body is document.issue_body, "issue_body"
assert facade.publication_key is document.publication_key, "publication_key"
assert facade.PublicationFailed is tracker.PublicationFailed, "PublicationFailed"
assert facade.Publication is tracker.Publication, "Publication"
assert facade.run_excerpts is evidence.run_excerpts, "run_excerpts"
assert facade.Published is record.Published, "Published"
assert tracker.carries_key is document.carries_key, "tracker->document"
stray = sorted(name for name in sys.modules
               if name.startswith("deflake_issue"))
assert not stray, f"bare copies loaded beside the package ones: {stray}"
""",
        "the bare spelling": """
import sys
sys.path.insert(0, "tools")
import deflake_issue_document as document
import deflake_issue_tracker as tracker
import deflake_issue as facade
assert facade.issue_body is document.issue_body, "issue_body"
assert facade.PublicationFailed is tracker.PublicationFailed, "PublicationFailed"
stray = sorted(name for name in sys.modules
               if name.startswith("tools.deflake_issue"))
assert not stray, f"package copies loaded beside the bare ones: {stray}"
""",
    }
    for label, program in programs.items():
        done = subprocess.run([sys.executable, "-c", program], cwd=str(root),
                              capture_output=True, text=True, timeout=120,
                              env=environment)
        expect(done.returncode == 0,
               f"under {label} the façade and its owners must be ONE set of "
               f"modules; exited {done.returncode}\n{done.stderr[-400:]}")

    # The substituted constant is the case that matters most, since a
    # duplicated document owner would leave the gate's own mutation seam
    # pointing at a module nothing renders through.
    seam = """
import tools.deflake_issue_document as document
import tools.deflake_issue as facade
document.MAX_BODY_CHARS = 400
source = facade.issue_body.__globals__["MAX_BODY_CHARS"]
assert source == 400, f"the renderer still reads {source}"
"""
    done = subprocess.run([sys.executable, "-c", seam], cwd=str(root),
                          capture_output=True, text=True, timeout=120,
                          env=environment)
    expect(done.returncode == 0,
           f"lowering `tools.deflake_issue_document.MAX_BODY_CHARS` must "
           f"reach the `issue_body` the façade composes; exited "
           f"{done.returncode}\n{done.stderr[-400:]}")


def test_the_issue_family_imports_as_repository_modules() -> None:
    """#2157: every owner resolves under the `tools.` package spelling too.

    `tools/` carries no `__init__.py`, so it is an implicit namespace
    package: `import tools.deflake_issue` from the repository root is a
    supported spelling, and under it the directory holding these modules
    is NOT on `sys.path`. Sibling imports by bare name resolve anyway
    only because each module inserts its own directory first — which the
    pre-split `deflake_issue.py` did, and which the façade must keep
    doing before the first of its re-exports, since those run at import
    time.
    """
    root = Path(dd.__file__).resolve().parent.parent
    family = ("tools.deflake_issue", "tools.deflake_issue_evidence",
              "tools.deflake_issue_document", "tools.deflake_issue_tracker",
              "tools.deflake_issue_record")
    environment = dict(os.environ)
    environment.pop("PYTHONPATH", None)
    for module in family:
        done = subprocess.run(
            [sys.executable, "-c", f"import {module}"],
            cwd=str(root), capture_output=True, text=True, timeout=120,
            env=environment)
        expect(done.returncode == 0,
               f"`import {module}` from the repository root must resolve; "
               f"exited {done.returncode}\n{done.stderr[-400:]}")


def _collect() -> tuple:
    """This owner's tests, in source order, out of its own namespace.

    Derived rather than hand-listed, so a case added below joins the
    registry by existing: a hand-maintained roster is exactly the
    fourth list that could silently drop a case while the run still
    exited zero.

    `__module__` is checked because the names imported from the shared
    support module are in these globals too, and a helper that ever
    started with `test_` would otherwise be claimed by whichever owner
    imported it.
    """
    return tuple(value for name, value in globals().items()
                 if name.startswith("test_") and callable(value)
                 and getattr(value, "__module__", None) == __name__)


#: This owner's registry. The facade collects from exactly this, for
#: both the focused invocation and the aggregate one, so the two can
#: never disagree about what this owner declares.
TESTS = _collect()
