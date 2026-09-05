#!/usr/bin/env python3
"""Identity and coordinator-state cases for `test_probe_external_evidence.py` (#2187).

The ten cases here own probe/test identity and the interpretation of
coordinator state: the stable hyphenated `probe:` and `probe-flake:`
identities and their exact (never prefix or substring) matching, the
raw heartbeat, scoped diagnostics, the absent / present / unexaminable
distinction at the state root and in `entry_state`, damaged run
identity, and unknown-key rejection.

`CASES` is this owner's inventory in the order the aggregate runs it,
which is not the source order below. This module holds case bodies and
that inventory only; `python3 tools/test_probe_external_evidence.py
--only identity` is the way to run it.
"""
from __future__ import annotations

import json
import os
import stat
import tempfile
from pathlib import Path

from probe_external_evidence_test_support import (  # noqa: E402
    MISSING, NonInteraction, build_state, check, check_equal, evidence,
    make_run, read, report_path, run_with_identity,
)
import probe_runner_registry  # noqa: E402


def test_identity_mapping() -> None:
    """Keys map to `$test` ids by key, not by stripping a script suffix."""
    check_equal(evidence.test_id_for_probe("transfer_order"), "probe:transfer-order",
                "transfer_order maps to probe:transfer-order")
    check_equal(evidence.test_id_for_probe("persistence_contract_sweep"),
                "probe:persistence-contract-sweep",
                "persistence_contract_sweep maps by key")
    check_equal(evidence.probe_script("persistence_contract_sweep"),
                "persistence_contract_sweep.py",
                "persistence_contract_sweep has no _probe suffix to strip")
    check_equal(evidence.probe_script("transfer_order"), "transfer_order_probe.py",
                "transfer_order's registered script")

    keys = evidence.probe_keys()
    check_equal(keys, [k for k, _s, _p in probe_runner_registry.PROBES],
                "probe_keys mirrors probe_runner_registry.PROBES order")
    ids = [evidence.test_id_for_probe(k) for k in keys]
    check_equal(len(set(ids)), len(ids), "every registered key maps to a distinct id")
    check(all(i.startswith("probe:") and "_" not in i for i in ids),
          "every mapped id is a hyphenated probe: id")


def test_both_identities_map_to_one_probe() -> None:
    """`probe:` and `probe-flake:` are two identities of the same probe."""
    both = evidence.test_ids_for_probe("transfer_order")
    check_equal(both, {"run": "probe:transfer-order",
                       "flake": "probe-flake:transfer-order"},
                "transfer_order's two stable identities")
    check_equal(evidence.flake_test_id_for_probe("persistence_contract_sweep"),
                "probe-flake:persistence-contract-sweep",
                "the measurement identity is derived from the key too")

    keys = evidence.probe_keys()
    every = [i for k in keys for i in evidence.test_ids_for_probe(k).values()]
    check_equal(len(set(every)), len(every),
                "all identities across both namespaces stay distinct")
    check(all("_" not in i for i in every), "every identity is hyphenated")

    # The reverse mapping is by exact generated identity, never by
    # undoing the hyphenation — which is what keeps `probe:transfer_order`
    # from resolving onto the registered `transfer_order`.
    check_equal(evidence.probe_for_test_id("probe:transfer-order"),
                ("transfer_order", evidence.TEST_KIND_RUN),
                "an ordinary id resolves to its probe and kind")
    check_equal(evidence.probe_for_test_id("probe-flake:transfer-order"),
                ("transfer_order", evidence.TEST_KIND_FLAKE),
                "a measurement id resolves to the SAME probe, a different kind")
    for rejected in ("probe:transfer_order", "probe-flake:transfer_order",
                     "probe:transfer-order-extra", "gameplay:transfer-order",
                     "PROBE:TRANSFER-ORDER", "probe:", "probe-flake:", "",
                     "flake:transfer-order"):
        check_equal(evidence.probe_for_test_id(rejected), None,
                    f"{rejected!r} resolves to no probe")
    for rejected in (None, 17, ["probe:transfer-order"]):
        check_equal(evidence.probe_for_test_id(rejected), None,
                    f"a non-string test id ({type(rejected).__name__}) resolves to none")

    # Prefix families stay distinct across BOTH namespaces.
    for shorter, longer in (("repair", "repair_ai"), ("repair", "repair_item"),
                            ("power", "power_workshop"),
                            ("persistence_contract", "persistence_contract_sweep")):
        short_ids = set(evidence.test_ids_for_probe(shorter).values())
        long_ids = set(evidence.test_ids_for_probe(longer).values())
        check(not (short_ids & long_ids),
              f"{shorter} and {longer} share no identity")


def test_a_measurement_run_is_the_same_probes_work() -> None:
    """A `probe-flake:` run matches, and stays labelled as a measurement."""
    with tempfile.TemporaryDirectory() as tmp:
        runs = [
            make_run("probe:transfer-order", "ordinary",
                     claimed_at="2026-08-12T10:00:00Z"),
            make_run("probe-flake:transfer-order", "measurement",
                     claimed_at="2026-08-13T10:00:00Z"),
            make_run("probe-flake:transfer-order-extra", "prefixed"),
            make_run("probe-flake:transfer_order", "underscored"),
            make_run("probe-flake:role", "another-probe"),
        ]
        state = build_state(Path(tmp), runs, {})
        with NonInteraction(state) as guard:
            result = read(state, "transfer_order")
            guard.assert_untouched("both-identity matching")

        check_equal([r["run_id"] for r in result["runs"]],
                    ["measurement", "ordinary"],
                    "both identities match, newest first")
        by_id = {r["run_id"]: r for r in result["runs"]}
        check_equal(by_id["ordinary"]["test_id"], "probe:transfer-order",
                    "the ordinary run reports its own id")
        check_equal(by_id["ordinary"]["test_kind"], evidence.TEST_KIND_RUN,
                    "and is labelled an ordinary run")
        check_equal(by_id["measurement"]["test_id"], "probe-flake:transfer-order",
                    "the measurement reports its own id")
        check_equal(by_id["measurement"]["test_kind"], evidence.TEST_KIND_FLAKE,
                    "and is labelled a measurement")
        check_equal(result["test_id"], "probe:transfer-order",
                    "the document's test_id stays the ordinary identity")
        check_equal(result["test_ids"],
                    {"run": "probe:transfer-order",
                     "flake": "probe-flake:transfer-order"},
                    "and test_ids carries both")
        check_equal(result["diagnostics"], [], "the read is diagnostic-free")
        rendered = evidence.render(result)
        check("probe-flake:transfer-order" in rendered,
              "the measurement identity renders", rendered)
        check("(flake)" in rendered and "(run)" in rendered,
              "each run renders its kind", rendered)


def test_the_heartbeat_is_reported_raw() -> None:
    """`heartbeat_at` is surfaced un-interpreted, as text or as None."""
    with tempfile.TemporaryDirectory() as tmp:
        runs = [
            make_run("probe:role", "beating", status="running",
                     claimed_at="2026-08-13T10:00:00Z",
                     heartbeat_at="2026-08-13T10:04:00Z"),
            make_run("probe:role", "silent", status="running",
                     claimed_at="2026-08-12T10:00:00Z"),
            make_run("probe:role", "malformed", status="running",
                     claimed_at="2026-08-11T10:00:00Z", heartbeat_at=1234),
        ]
        state = build_state(Path(tmp), runs, {})
        with NonInteraction(state) as guard:
            result = read(state, "role")
            guard.assert_untouched("heartbeat reporting")
        by_id = {r["run_id"]: r for r in result["runs"]}
        check_equal(by_id["beating"]["heartbeat_at"], "2026-08-13T10:04:00Z",
                    "a recorded heartbeat is reported verbatim")
        check_equal(by_id["silent"]["heartbeat_at"], None,
                    "an absent heartbeat is unavailable, never fabricated")
        check_equal(by_id["malformed"]["heartbeat_at"], None,
                    "a non-string heartbeat is unavailable too")
        check_equal(by_id["beating"]["run_state"], "running",
                    "the raw run state is reported; activity is not decided here")

        # `recorded_fields` separates "not recorded" from "recorded but
        # unusable", which the normalized `null` alone cannot.
        check("heartbeat_at" in by_id["malformed"]["recorded_fields"],
              "a malformed heartbeat is still listed as recorded")
        check("heartbeat_at" not in by_id["silent"]["recorded_fields"],
              "an absent heartbeat is not listed as recorded")
        check_equal(by_id["silent"]["recorded_fields"],
                    sorted(by_id["silent"]["recorded_fields"]),
                    "recorded_fields is sorted")
        check(all("claimed_at" in r["recorded_fields"] for r in result["runs"]),
              "every synthetic run records claimed_at")


def test_a_damaged_state_root_is_not_an_absent_one() -> None:
    """A state root that is THERE but unusable is damage, never absence.

    `Path.is_dir()` swallows `OSError` and answers False, so a regular
    file, a dangling symlink and an unstattable path at the state root
    all used to read as the normal "Codex is not installed here" result
    — an empty run list with NO diagnostic, which a consumer failing
    closed on unreadable active-run state would accept as a clean read.
    """
    for label, build in (
            ("a regular file", lambda p: p.write_text("not a state tree")),
            ("a dangling symlink", lambda p: p.symlink_to(p.parent / "gone"))):
        with tempfile.TemporaryDirectory() as tmp:
            root = Path(tmp) / evidence.STATE_DIRNAME
            build(root)
            with NonInteraction(Path(tmp)) as guard:
                result = read(root, "role")
                guard.assert_untouched(f"{label} at the state root")
            check_equal(result["state"], evidence.STATE_PRESENT,
                        f"{label} at the state root is present, not absent")
            check_equal(result["runs"], [], f"{label} yields no runs")
            check_equal([d["scope"] for d in result["diagnostics_detail"]],
                        [evidence.SCOPE_REGISTRY],
                        f"{label} is diagnosed as active-run state, so a "
                        f"fail-closed consumer sees it")
            check(any("is not a directory" in d for d in result["diagnostics"]),
                  f"{label} says what is wrong", str(result["diagnostics"]))

    # A path that cannot be examined at all is the reader's rejection.
    with tempfile.TemporaryDirectory() as tmp:
        blocker = Path(tmp) / evidence.STATE_DIRNAME
        blocker.write_text("a file where a directory belongs", encoding="utf-8")
        try:
            read(blocker / "nested" / evidence.STATE_DIRNAME, "role")
            check(False, "an unstattable state root is rejected")
        except evidence.EvidenceRejected as exc:
            check("cannot stat the $test state root" in str(exc),
                  "and says so", str(exc))

    # A genuinely absent root is still the normal, diagnostic-free result.
    with tempfile.TemporaryDirectory() as tmp:
        result = read(Path(tmp) / "nothing-here", "role")
        check_equal(result["state"], evidence.STATE_ABSENT, "absence is absence")
        check_equal(result["diagnostics"], [], "and carries no diagnostic")


def test_entry_state_separates_absent_from_unexaminable() -> None:
    """The shared primitive keeps three answers apart, not two."""
    with tempfile.TemporaryDirectory() as tmp:
        base = Path(tmp)
        regular = base / "file"
        regular.write_text("x", encoding="utf-8")
        directory = base / "dir"
        directory.mkdir()
        dangling = base / "dangling"
        dangling.symlink_to(base / "gone")
        good_link = base / "link"
        good_link.symlink_to(regular)

        present, mode, failure = evidence.entry_state(regular)
        check((present, failure), (True, None), "a regular file is present")
        check(mode is not None and stat.S_ISREG(mode), "and reports S_ISREG")

        present, mode, failure = evidence.entry_state(directory)
        check(mode is not None and stat.S_ISDIR(mode), "a directory reports S_ISDIR")

        present, mode, failure = evidence.entry_state(good_link)
        check(mode is not None and stat.S_ISREG(mode),
              "a symlink is judged by its target's kind")

        present, mode, failure = evidence.entry_state(dangling)
        check_equal((present, mode, failure), (True, None, None),
                    "a dangling symlink is PRESENT with no usable kind")

        present, mode, failure = evidence.entry_state(base / "missing")
        check_equal((present, mode, failure), (False, None, None),
                    "a missing path is absent, with no failure")

        present, mode, failure = evidence.entry_state(regular / "child")
        check(present and failure is not None,
              "a path under a non-directory is a stat FAILURE, not an absence",
              f"{present!r} {failure!r}")
        check(not os.path.lexists(regular / "child"),
              "which is exactly what lexists cannot tell you")


def test_an_unreadable_run_identity_is_record_damage() -> None:
    """A record whose `test_id` cannot be read is diagnosed, not dropped.

    `test_id` is arbitrary external JSON. An UNHASHABLE value crashed the
    whole read outright (`TypeError` from the set membership test),
    taking every valid run with it; a missing or empty one was silently
    skipped, which let an otherwise-active run of indeterminate ownership
    pass as no evidence at all.
    """
    shapes = [
        ("an unhashable list", []),
        ("an unhashable dict", {}),
        ("a number", 17),
        ("a boolean", True),
        ("an absent field", MISSING),
        ("null", None),
        ("an empty string", ""),
        ("whitespace", "   "),
    ]
    for label, value in shapes:
        with tempfile.TemporaryDirectory() as tmp:
            runs = [
                run_with_identity("damaged", value, status="running"),
                make_run("probe:role", "healthy"),
            ]
            state = build_state(Path(tmp), runs, {})
            with NonInteraction(state) as guard:
                result = read(state, "role")
                guard.assert_untouched(f"{label} test_id")
            check_equal([r["run_id"] for r in result["runs"]], ["healthy"],
                        f"{label} does not crash the read, and the valid run "
                        f"is still reported")
            check_equal([d["scope"] for d in result["diagnostics_detail"]],
                        [evidence.SCOPE_RECORD],
                        f"{label} is diagnosed as record damage")
            detail = result["diagnostics"][0]
            check("no usable test_id" in detail, f"{label} says what is wrong",
                  detail)
            check("damaged" in detail, f"{label} names the run", detail)
            json.dumps(result)
            check(True, f"{label} leaves the document serializable")

    # A well-formed identity for ANOTHER probe is not damage; it is just
    # a non-match, and must stay diagnostic-free.
    with tempfile.TemporaryDirectory() as tmp:
        state = build_state(Path(tmp), [
            make_run("gameplay:role", "other-namespace"),
            make_run("probe:transfer-order", "other-probe")], {})
        result = read(state, "role")
        check_equal(result["runs"], [], "neither matches")
        check_equal(result["diagnostics"], [],
                    "and a non-match is never diagnosed as damage")


def test_diagnostics_carry_the_state_they_concern() -> None:
    """Every diagnostic is scoped, so a consumer can fail closed precisely."""
    # A damaged REGISTRY is active-run state.
    with tempfile.TemporaryDirectory() as tmp:
        state = build_state(Path(tmp), [make_run("probe:role", "run")], {})
        (state / evidence.REGISTRY_FILENAME).write_text("{not json",
                                                        encoding="utf-8")
        with NonInteraction(state) as guard:
            result = read(state, "role")
            guard.assert_untouched("unparseable registry")
        check_equal([d["scope"] for d in result["diagnostics_detail"]],
                    [evidence.SCOPE_REGISTRY],
                    "an unparseable registry is scoped `registry`")
        check_equal(result["diagnostics"],
                    [d["message"] for d in result["diagnostics_detail"]],
                    "the flat list mirrors the detailed one, in order")

    # A damaged RECORD is active-run state too.
    with tempfile.TemporaryDirectory() as tmp:
        state = build_state(Path(tmp), ["not-an-object",
                                        make_run("probe:role", "run")], {})
        with NonInteraction(state) as guard:
            result = read(state, "role")
            guard.assert_untouched("malformed record")
        check_equal([d["scope"] for d in result["diagnostics_detail"]],
                    [evidence.SCOPE_RECORD],
                    "a non-object run record is scoped `record`")
        check_equal([r["run_id"] for r in result["runs"]], ["run"],
                    "the valid record is still read")

    # A damaged REPORT is NOT active-run state.
    with tempfile.TemporaryDirectory() as tmp:
        runs = [make_run("probe:role", "run")]
        state = build_state(Path(tmp), runs, {})
        runs[0]["report_path"] = report_path(state, "run")
        build_state(Path(tmp), runs, {})
        # A DIRECTORY where the report belongs: it exists, so it is
        # damage rather than a report that was simply never written.
        (state / evidence.REPORTS_DIRNAME / ("run" + evidence.REPORT_SUFFIX)).mkdir()
        with NonInteraction(state) as guard:
            result = read(state, "role")
            guard.assert_untouched("damaged report")
        check_equal([d["scope"] for d in result["diagnostics_detail"]],
                    [evidence.SCOPE_REPORT],
                    "a damaged report is scoped `report`, never `registry`")

    # A clean read carries neither list.
    with tempfile.TemporaryDirectory() as tmp:
        state = build_state(Path(tmp), [make_run("probe:role", "run")], {})
        result = read(state, "role")
        check_equal(result["diagnostics_detail"], [],
                    "a clean read has no scoped diagnostics")

    # The absent-state result still carries both keys.
    with tempfile.TemporaryDirectory() as tmp:
        result = read(Path(tmp) / "nothing-here", "role")
        check_equal(result["state"], evidence.STATE_ABSENT, "absent state")
        check_equal(result["diagnostics_detail"], [],
                    "an absent state tree is not damage")
        check_equal(result["test_ids"]["flake"], "probe-flake:role",
                    "both identities are reported even with no state")


def test_unknown_key_is_rejected() -> None:
    """An unregistered key is a rejection, not a no-evidence answer."""
    with tempfile.TemporaryDirectory() as tmp:
        state = build_state(Path(tmp), [], {})
        try:
            read(state, "definitely_not_a_probe")
            check(False, "unknown key raises EvidenceRejected")
        except evidence.EvidenceRejected as exc:
            check("definitely_not_a_probe" in str(exc),
                  "the rejection names the offending key", str(exc))
            check("probe_runner_registry.PROBES" in str(exc),
                  "the rejection names the authoritative registry", str(exc))

        code = evidence.main(["--probe", "definitely_not_a_probe"])
        check_equal(code, evidence.EXIT_REJECTED, "CLI exits 2 on an unknown key")
        check_equal(evidence.main(["--probe", "role", "--state-root", str(state)]),
                    evidence.EXIT_OK, "CLI exits 0 on a registered key")


def test_exact_matching() -> None:
    """Only the exactly-mapped `$test` id matches."""
    with tempfile.TemporaryDirectory() as tmp:
        runs = [
            make_run("probe:transfer-order", "exact"),
            make_run("probe:transfer-order-extra", "prefixed"),
            make_run("probe:transfer_order", "underscored"),
            make_run("gameplay:transfer-order", "other-namespace"),
            make_run("probe:transfer", "shorter"),
            make_run("PROBE:TRANSFER-ORDER", "uppercased"),
        ]
        state = build_state(Path(tmp), runs, {})
        with NonInteraction(state) as guard:
            result = read(state, "transfer_order")
            guard.assert_untouched("exact matching")
        check_equal([r["run_id"] for r in result["runs"]], ["exact"],
                    "only the exact test id matches")
        check_equal(result["state"], evidence.STATE_PRESENT, "state reported present")
        check_equal(result["diagnostics"], [], "an exact-match read is diagnostic-free")


#: This owner's cases, in the order the aggregate has always run them.
CASES = (
    test_identity_mapping,
    test_both_identities_map_to_one_probe,
    test_a_measurement_run_is_the_same_probes_work,
    test_the_heartbeat_is_reported_raw,
    test_diagnostics_carry_the_state_they_concern,
    test_a_damaged_state_root_is_not_an_absent_one,
    test_an_unreadable_run_identity_is_record_damage,
    test_entry_state_separates_absent_from_unexaminable,
    test_unknown_key_is_rejected,
    test_exact_matching,
)
