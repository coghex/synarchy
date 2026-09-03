#!/usr/bin/env python3
"""Canonical probe-identity cases for the in-flight self-test (#2141).

Owns the five cases that pin how a probe key is recognized: humanized
aliases, registered prefix families, the refusal to substring-match, the
deliberate common-word over-exclusion, and ambiguous-subject exclusion
with its evidence retained.

Not independently runnable: it parses no arguments, executes nothing at
import time and exposes no command-line interface. `CASES` is its whole
public surface, and the only entry point is
`tools/test_probe_inflight.py`, which runs these inside the global
`Offline` boundary.
"""
from __future__ import annotations

import os
import sys
import tempfile
from pathlib import Path

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
import probe_inflight as inflight  # noqa: E402
from test_probe_inflight_support import (  # noqa: E402
    FakeGitHub,
    build_reports,
    check,
    check_equal,
    evaluate,
    issue,
    sources_of,
)

# ==========================================================================
# Canonical identity
# ==========================================================================

def test_humanized_aliases_all_name_one_probe() -> None:
    """Case- and separator-normalized human forms resolve to one key."""
    index = inflight.build_identity_index()
    for spelling in ("injury_log", "injury-log", "Injury-Log", "INJURY LOG",
                     "injury log probe", "injury_log_probe", "injury_log_probe.py",
                     "Injury-log probe never gates a real fall's event emission",
                     "fix `injury_log_probe.py` at last"):
        check(bool(inflight.subject_matches(spelling, "injury_log", index)),
              f"{spelling!r} names injury_log")
    for spelling in ("injurylog", "injury", "log probe", "the injury of a log",
                     "injury_logging", ""):
        check_equal(inflight.subject_matches(spelling, "injury_log", index), [],
                    f"{spelling!r} does not name injury_log")


def test_prefix_families_stay_distinct() -> None:
    """Longest-registered-identity wins, PER OCCURRENCE, not per subject."""
    index = inflight.build_identity_index()
    families = (("repair", "repair_ai"), ("repair", "repair_item"),
                ("power", "power_workshop"),
                ("persistence_contract", "persistence_contract_sweep"))
    for shorter, longer in families:
        subject = f"the {longer} probe is flaky"
        check(bool(inflight.subject_matches(subject, longer, index)),
              f"{subject!r} names {longer}")
        check_equal(inflight.subject_matches(subject, shorter, index), [],
                    f"a single {longer} mention never also credits {shorter}")

    # The two-mention shape is the ONLY place a per-subject suppression
    # rule and a per-occurrence rule differ, so it is pinned explicitly.
    both = "repair_ai probe regressed after the repair probe changed"
    check_equal(len(inflight.subject_matches(both, "repair_ai", index)), 1,
                "the longer identity matches its own occurrence")
    check_equal(len(inflight.subject_matches(both, "repair", index)), 1,
                "and a separate standalone repair occurrence matches too")
    check_equal(inflight.subject_matches(both, "repair_item", index), [],
                "an unmentioned family member still does not match")

    # Longest-match is positional, so order does not rescue the prefix.
    reversed_order = "the repair probe and then repair_ai probe"
    check_equal(len(inflight.subject_matches(reversed_order, "repair", index)), 1,
                "prefix first still yields exactly one repair occurrence")
    check_equal(len(inflight.subject_matches(reversed_order, "repair_ai", index)), 1,
                "and one repair_ai occurrence")


def test_substring_matching_is_not_used() -> None:
    """A registered key embedded in a longer word is not a match."""
    index = inflight.build_identity_index()
    check_equal(inflight.subject_matches("powerful workshop tooling", "power", index),
                [], "`powerful` does not contain the power probe")
    check_equal(inflight.subject_matches("chopping block", "chop", index), [],
                "`chopping` does not contain the chop probe")
    check(bool(inflight.subject_matches("power probe", "power", index)),
          "the real identity still matches")


def test_a_common_word_key_over_excludes_by_design() -> None:
    """A single-word registered key matches an incidental title mention.

    This is the required direction of error, not a defect: a false
    exclusion costs the selector one skipped candidate, a false clear
    costs an hour of duplicated measurement colliding with live work. It
    is pinned here so it stays a stated contract rather than drifting
    into an accident — and so that any later narrowing has to change a
    test that says out loud what it is giving up.
    """
    index = inflight.build_identity_index()
    title = ("Move the power node role and rating from Haskell into the "
             "building YAML schema")
    for key in ("power", "role"):
        occurrences = inflight.subject_matches(title, key, index)
        check_equal([o["text"] for o in occurrences], [key],
                    f"an incidental {key!r} in a title still matches")
    check_equal(inflight.subject_matches(title, "power_workshop", index), [],
                "and the longer family member still does not")

    with tempfile.TemporaryDirectory() as tmp:
        root = build_reports(Path(tmp) / "repo")
        api = FakeGitHub(issues=[issue(1148, title)])
        document = evaluate("power", repo_root=root, github=api,
                            state_root=Path(tmp) / "none")
        check_equal(document["result"], inflight.RESULT_IN_FLIGHT,
                    "the incidental mention excludes the candidate")
        match = sources_of(document, inflight.SOURCE_ISSUE)[0]
        check_equal(match["matched_text"], ["power"],
                    "and matched_text shows exactly what it matched on, so a "
                    "reader can judge the exclusion")


def test_subject_ambiguity_excludes_and_keeps_its_evidence() -> None:
    """Subject-match ambiguity yields a MATCH, never a source-error.

    Two registered probes sharing one identity form is unlikely but
    representable, and the honest answer is to credit both and retain
    what caused it. That is a different outcome from the
    source-STRUCTURE ambiguity covered further down, and the two must
    never be folded into one path.
    """
    colliding = [("alpha_thing", "alpha_thing_probe.py", "one"),
                 ("alpha", "alpha_thing.py", "two")]
    index = inflight.build_identity_index(colliding)
    occurrences = inflight.find_occurrences("the alpha_thing is unreliable", index)
    check_equal(len(occurrences), 1, "one occurrence at the colliding position")
    check_equal(occurrences[0]["probes"], ["alpha", "alpha_thing"],
                "both owners are credited")
    check(occurrences[0]["ambiguous"], "and the occurrence is marked ambiguous")

    # Where the longest match DOES separate them, it still does.
    unambiguous = inflight.find_occurrences("the alpha_thing probe", index)
    check_equal(unambiguous[0]["probes"], ["alpha_thing"],
                "a longer distinguishing form still resolves cleanly")
    check(not unambiguous[0]["ambiguous"], "and is not marked ambiguous")

    # Through the shipped report source, an ambiguous subject EXCLUDES and
    # keeps its evidence — an `in-flight` match, never a source error.
    with tempfile.TemporaryDirectory() as tmp:
        root = build_reports(Path(tmp), {
            "NCT": [(1, "The alpha_thing is unreliable", "", False)]})
        matches = inflight.evaluate_reports("alpha", index, repo_root=root,
                                            docs_root=None)
        check_equal(len(matches), 1, "an ambiguous subject still matches")
        check(matches[0]["ambiguous"], "the match records the ambiguity")
        check_equal(matches[0]["competing_probes"], ["alpha_thing"],
                    "and names what it was ambiguous with")
        check_equal(matches[0]["source"], inflight.SOURCE_REPORT,
                    "reported as a report match, not as a broken source")


CASES = (
    test_humanized_aliases_all_name_one_probe,
    test_prefix_families_stay_distinct,
    test_substring_matching_is_not_used,
    test_a_common_word_key_over_excludes_by_design,
    test_subject_ambiguity_excludes_and_keeps_its_evidence,
)
