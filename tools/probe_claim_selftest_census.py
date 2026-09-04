#!/usr/bin/env python3
"""Census and collaborator-boundary cases (#1434), split out by #2100.

The five cases covering acquisition recording, the separation of the
claim log from the measurement log, lossless schema migration,
`probe_flake` staying usable with no `docs-wip` worktree and no census
dependency at all, and the direction the claim's own three owners
depend in (#2148).

The last two are BOUNDARY cases rather than census cases in the narrow
sense. One keeps the mandatory claim in the orchestration path instead
of letting it leak into the low-level measurement API; the other keeps
storage a filesystem leaf and the command a command. Both belong to the
owner that also owns the census collaborator.

Not a gate of its own. Run through the aggregate:

  python3 tools/test_probe_claim.py --only census
"""
from __future__ import annotations

import ast
import json
import sys
from pathlib import Path

sys.path.insert(0, str(Path(__file__).resolve().parent))
import probe_census  # type: ignore  # noqa: E402
# `_deep_copy` is named at its owner rather than through the facade's
# compatibility re-export: #2131 made `tools/probe_census_records.py`
# the module that defines it.
import probe_census_records as census_records  # type: ignore  # noqa: E402
import probe_engine  # type: ignore  # noqa: E402
import probe_flake  # type: ignore  # noqa: E402
import probe_protocol  # type: ignore  # noqa: E402
import probe_claim as facade  # type: ignore  # noqa: E402
import probe_claim_lease  # type: ignore  # noqa: E402
import probe_claim_orchestration  # type: ignore  # noqa: E402
import probe_claim_storage  # type: ignore  # noqa: E402
from probe_claim_selftest_support import (  # noqa: E402
    COMMIT_A, COMMIT_B, SYNTHETIC, TOOLS, expect, expect_raises,
    fake_measurement, registry, scratch, scratch_repo, seeded_census)

#: The claim's own modules, lowest owner first. The index of a module in
#: this tuple is how far up the stack it sits, so an import edge is
#: legal exactly when it points at a STRICTLY lower index.
OWNER_ORDER = (
    "probe_claim_storage",
    "probe_claim_lease",
    "probe_claim_orchestration",
    "probe_claim",
)


def test_census_claim_collection() -> None:
    """Claims are their own append-only collection, keyed by token."""
    print("\n-- the census records acquisitions, idempotently --")
    with registry(), scratch_repo() as (_main, _other, census):
        seeded_census(census)
        record = {"token": "tok-1", "timestamp_utc": "2026-08-21T05:00:00Z",
                  "commit_sha": COMMIT_A, "owner": "dev@host:1",
                  "host": "host", "pid": 1, "lease_seconds": 3600.0,
                  "requested_runs": 10}
        probe_census.record_claim(census, "alpha", record)
        document = json.loads(census.read_text(encoding="utf-8"))
        row = probe_census.find_entry(document, "alpha")["census"]
        expect(row["claims"] == [record],
               "one acquisition appends exactly one claim record")
        expect(row["attempts"] == [] and row["current"] is None,
               "and touches neither the attempt log nor the measurements")

        before = census.read_bytes()
        probe_census.record_claim(census, "alpha", dict(record))
        expect(census.read_bytes() == before,
               "replaying the SAME acquisition token is a byte-for-byte no-op")

        conflicting = dict(record, requested_runs=3)
        expect_raises(
            probe_census.CensusError,
            lambda: probe_census.record_claim(census, "alpha", conflicting),
            "the same token with different metadata is refused",
            "tok-1", "different metadata")
        expect(census.read_bytes() == before,
               "...having written nothing")

        second = dict(record, token="tok-2", requested_runs=3)
        probe_census.record_claim(census, "alpha", second)
        document = json.loads(census.read_text(encoding="utf-8"))
        expect(probe_census.find_entry(document, "alpha")["census"]["claims"]
               == [record, second],
               "a genuinely new acquisition appends beside the first")

        expect_raises(
            probe_census.CensusError,
            lambda: probe_census.record_claim(census, "alpha", {"owner": "x"}),
            "a claim with no token is refused", "token")
        expect_raises(
            probe_census.CensusError,
            lambda: probe_census.record_claim(census, "nonesuch", record),
            "a claim for a probe with no census row is refused", "nonesuch")


def test_claims_are_not_measurements() -> None:
    """Neither aspect may reach into the other's fields."""
    print("\n-- the claim log and the measurement log stay separate --")
    with registry(), scratch_repo() as (_main, _other, census):
        seeded_census(census)
        record = {"token": "tok-1", "timestamp_utc": "2026-08-21T05:00:00Z",
                  "commit_sha": COMMIT_A, "owner": "dev@host:1",
                  "host": "host", "pid": 1, "lease_seconds": 60.0,
                  "requested_runs": 2}
        probe_census.record_claim(census, "alpha", record)
        probe_census.record_result(census,
                                   fake_measurement("alpha").to_document())
        document = json.loads(census.read_text(encoding="utf-8"))
        row = probe_census.find_entry(document, "alpha")["census"]
        expect(row["claims"] == [record],
               "ingesting a measurement leaves the claim log alone")
        expect(len(row["attempts"]) == 1 and row["current"] is not None,
               "and the measurement still landed")

        # The preservation guard is what makes those promises real, so
        # drive it directly from both directions.
        def claim_touching_measurements(before):
            candidate = census_records._deep_copy(before)
            target = probe_census.find_entry(candidate, "alpha")["census"]
            target["attempts"] = target["attempts"] + [dict(target["attempts"][0])]
            return candidate, {"alpha": {"claims"}}

        def measurement_touching_claims(before):
            candidate = census_records._deep_copy(before)
            target = probe_census.find_entry(candidate, "alpha")["census"]
            target["claims"] = target["claims"] + [dict(record, token="tok-9")]
            return candidate, {"alpha": {"measurements"}}

        for mutate, msg, fragment in (
                (claim_touching_measurements,
                 "a claim operation may not append an attempt", "attempts"),
                (measurement_touching_claims,
                 "a measurement operation may not append a claim", "claims")):
            expect_raises(probe_census.CensusError,
                          lambda m=mutate: probe_census.update(census, m),
                          msg, fragment)

        def drops_a_claim(before):
            candidate = census_records._deep_copy(before)
            probe_census.find_entry(candidate, "alpha")["census"]["claims"] = []
            return candidate, {"alpha": {"claims"}}

        expect_raises(probe_census.CensusError,
                      lambda: probe_census.update(census, drops_a_claim),
                      "the claim log is append-only: a candidate that "
                      "discards one is refused", "append-only")


def test_schema_migration_is_lossless() -> None:
    """v1 and v2 censuses migrate keeping every accumulated field.

    The claim log arrived in `probe-census/v3`, which is the version
    this pins; the CURRENT schema moves on as later issues extend the
    record, and the migration this covers has to keep working every
    time it does.
    """
    print("\n-- the v2 -> current migration loses nothing --")
    with registry():
        expect(probe_census.CLAIM_SCHEMA == "probe-census/v3",
               "the claim log arrived in probe-census/v3")
        expect(probe_census.CENSUS_SCHEMA in probe_census.MIGRATABLE_SCHEMAS
               and probe_census.MIGRATABLE_SCHEMAS.index(
                   probe_census.CLAIM_SCHEMA)
               < probe_census.MIGRATABLE_SCHEMAS.index(
                   probe_census.CENSUS_SCHEMA),
               f"...and every later schema still migrates from it; got "
               f"{probe_census.MIGRATABLE_SCHEMAS}")

        cohort = {"commit_sha": COMMIT_A, "samples": []}
        attempt = {"timestamp_utc": "2026-08-20T05:00:00Z",
                   "commit_sha": COMMIT_A, "status": "harness-error",
                   "accepted": False, "requested_runs": 2, "completed_runs": 1,
                   "error": "run 2 broke", "retained_artifacts": []}
        v2 = {
            "schema": "probe-census/v2",
            "probes": [{
                "key": "alpha", "script": "alpha_probe.py",
                "classification": "manual-only",
                "protocol": probe_protocol.PROTOCOL_VERSION,
                "census": {
                    "acceptable_failures": 3,
                    "acceptable_failures_justification": "three known races",
                    "estimated_worst_case_seconds": 480,
                    "current": {"commit_sha": COMMIT_B, "samples": []},
                    "history": [cohort],
                    "attempts": [attempt],
                },
            }],
        }
        probe_census.validate_document(v2, "probe-census/v2", "a stored v2")
        migrated = probe_census.migrate_document(v2)
        probe_census.validate_document(migrated, probe_census.CENSUS_SCHEMA,
                                       "the migrated census")
        record = migrated["probes"][0]["census"]
        expect(migrated["schema"] == probe_census.CENSUS_SCHEMA,
               f"the migrated document is "
               f"{probe_census.CENSUS_SCHEMA}")
        expect(record["claims"] == [] and record["outcomes"] == []
               and record["deferred"] is None,
               "the migration adds an EMPTY claim log, and #1439's equally "
               "empty outcome log plus v5's null deferral beside it")
        for field, value in (("acceptable_failures", 3),
                             ("acceptable_failures_justification",
                              "three known races"),
                             ("estimated_worst_case_seconds", 480),
                             ("history", [cohort]), ("attempts", [attempt])):
            expect(record[field] == value,
                   f"the migration preserves `{field}` exactly")
        expect(record["current"] == {"commit_sha": COMMIT_B, "samples": []},
               "the migration preserves the current cohort exactly")
        expect(v2["probes"][0]["census"].get("claims") is None
               and v2["probes"][0]["census"].get("outcomes") is None
               and v2["probes"][0]["census"].get("deferred") is None,
               "and it does not mutate the document it migrated FROM")

        again = probe_census.migrate_document(migrated)
        expect(again == migrated,
               "re-migrating an already-migrated census is a no-op")

        with_claims = census_records._deep_copy(migrated)
        kept = {"token": "tok-1", "timestamp_utc": "2026-08-21T05:00:00Z",
                "commit_sha": COMMIT_A, "owner": "dev@host:1", "host": "host",
                "pid": 1, "lease_seconds": 60.0, "requested_runs": 2}
        with_claims["probes"][0]["census"]["claims"] = [kept]
        expect(probe_census.migrate_document(with_claims)["probes"][0]
               ["census"]["claims"] == [kept],
               "re-migrating a current census never truncates its existing "
               "claims")

        v1 = {"schema": "probe-census/v1",
              "probes": [{"key": "alpha", "script": "alpha_probe.py",
                          "classification": "manual-only",
                          "protocol": "legacy"}]}
        from_v1 = probe_census.migrate_document(v1)
        probe_census.validate_document(from_v1, probe_census.CENSUS_SCHEMA,
                                       "the migrated v1 census")
        expect(from_v1["probes"][0]["census"] == probe_census.empty_census(),
               "a v1 seed still migrates straight to the empty current "
               "record")

    with registry(), scratch_repo() as (_main, _other, census):
        # The migration through the real writer, on disk, is what an
        # operator actually runs.
        census.parent.mkdir(parents=True, exist_ok=True)
        v2["probes"] = [dict(v2["probes"][0])] + [
            {"key": key, "script": script, "classification": "manual-only",
             "protocol": probe_protocol.PROTOCOL_VERSION,
             "census": probe_census.empty_census()}
            for key, script, _p in SYNTHETIC[1:]]
        for row in v2["probes"][1:]:
            row["census"].pop("claims")
            row["census"].pop("outcomes")
            row["census"].pop("deferred")
        census.write_text(json.dumps(v2, indent=2, sort_keys=True) + "\n",
                          encoding="utf-8")
        probe_census.ensure_document(census)
        stored = json.loads(census.read_text(encoding="utf-8"))
        expect(stored["schema"] == probe_census.CENSUS_SCHEMA,
               "`--seed` migrates a stored v2 census in place")
        alpha = probe_census.find_entry(stored, "alpha")["census"]
        expect(alpha["acceptable_failures"] == 3
               and alpha["history"] == [cohort] and alpha["attempts"] == [attempt],
               "...keeping every policy field, cohort and attempt it held")
        expect(all(probe_census.find_entry(stored, key)["census"]["claims"] == []
                   and probe_census.find_entry(
                       stored, key)["census"]["outcomes"] == []
                   and probe_census.find_entry(
                       stored, key)["census"]["deferred"] is None
                   for key, _s, _p in SYNTHETIC),
               "...and giving every row empty claim/outcome logs and a null "
               "deferral")


def test_probe_flake_needs_no_docs_worktree() -> None:
    """The low-level measurement API stays usable without a census.

    `probe_flake.py` guarantees that a fresh checkout with no `docs-wip`
    worktree behaves identically, and the census-backed claim must not
    quietly take that away: the mandatory claim belongs to the
    ORCHESTRATION path, not to the measurement API.
    """
    print("\n-- probe_flake stays usable with no docs worktree --")
    with registry(), scratch() as elsewhere:
        saved = probe_engine.REPO_ROOT
        probe_engine.REPO_ROOT = str(elsewhere)
        try:
            expect_raises(probe_census.DocsWorktreeMissing,
                          lambda: probe_census.manifest_path(),
                          "the scratch tree really has no docs-wip census")
            # Every pre-execution decision the harness makes is reachable.
            expect(probe_flake.protocol_status("alpha")
                   == probe_protocol.PROTOCOL_VERSION,
                   "probe_flake resolves protocol status with no census")
            expect(probe_flake.resolve_probe("alpha") == "alpha_probe.py",
                   "and resolves a probe to its script with no census")
            expect_raises(probe_flake.Rejection,
                          lambda: probe_flake.measure("alpha", 0),
                          "and its own argument checking still refuses first",
                          "positive")
            expect("probe_census" not in probe_flake.__dict__,
                   "probe_flake does not import the census at all")
        finally:
            probe_engine.REPO_ROOT = saved


def _imported_claim_modules(module_name: str) -> set:
    """Every claim-family module `module_name` names at module level.

    Both spellings count -- `import probe_claim_storage as storage` and
    `from probe_claim_storage import ...` -- because either one creates
    the dependency requirement 11 constrains, and the WHOLE tree is
    walked rather than its top level, because a deferred import in a
    function body, or one hidden behind `if TYPE_CHECKING:`, is the same
    edge written where a top-level scan would not look. Only import
    nodes are collected, so the module's own docstring -- which names
    its siblings deliberately -- contributes nothing.
    """
    source = Path(TOOLS, f"{module_name}.py").read_text(encoding="utf-8")
    named = set()
    for node in ast.walk(ast.parse(source)):
        if isinstance(node, ast.Import):
            named.update(alias.name for alias in node.names)
        elif isinstance(node, ast.ImportFrom) and node.module and not node.level:
            named.add(node.module)
    return named & set(OWNER_ORDER)


def test_owner_dependencies_run_one_way() -> None:
    """Storage is the leaf, the command is the root, and nothing loops.

    Requirement 11's acyclic direction is the one structural claim the
    split rests on, and inspection is not a gate: a `from
    probe_claim_orchestration import EXIT_OK` added to storage to save a
    line would compile, pass every behavioral case, and silently make
    the leaf depend on the whole stack.

    The companion claim is that the command re-exports NOTHING. A
    facade alias -- `acquire = lease.acquire` -- would still work for a
    reader, but assigning to it would no longer reach the state the
    implementation reads, which is exactly the dead seam #2074 avoided
    and this split must not reintroduce.
    """
    print("\n-- the claim's owners depend one way, and the command "
          "re-exports nothing --")
    edges = {name: _imported_claim_modules(name) for name in OWNER_ORDER}
    # Non-vacuity first: an AST scan that found nothing would satisfy
    # every exclusion below while inspecting an empty set.
    expect(sum(len(found) for found in edges.values()) >= 6,
           f"the scan found the family's own import edges ({edges})")
    for consumer, found in edges.items():
        rank = OWNER_ORDER.index(consumer)
        upward = sorted(name for name in found
                        if OWNER_ORDER.index(name) >= rank)
        expect(not upward,
               f"{consumer} imports only owners below it "
               f"(it also names {upward})")
    expect(edges["probe_claim_storage"] == set(),
           "storage is the filesystem LEAF: it imports neither of the "
           "other two")
    expect(edges["probe_claim_lease"] == {"probe_claim_storage"},
           "the lease owner consumes storage and nothing above it")
    expect(edges["probe_claim_orchestration"]
           == {"probe_claim_storage", "probe_claim_lease"},
           "orchestration consumes both owners below it")
    expect(edges["probe_claim"] == {"probe_claim_storage",
                                    "probe_claim_lease",
                                    "probe_claim_orchestration"},
           "and the command consumes all three")

    defined = {}
    for name in OWNER_ORDER[:-1]:
        source = Path(TOOLS, f"{name}.py").read_text(encoding="utf-8")
        names = set()
        for node in ast.parse(source).body:
            if isinstance(node, (ast.FunctionDef, ast.ClassDef)):
                names.add(node.name)
            elif isinstance(node, ast.Assign):
                names.update(target.id for target in node.targets
                             if isinstance(target, ast.Name))
        defined[name] = {value for value in names
                         if not value.startswith("_")}
    expect(all(len(names) >= 5 for names in defined.values()),
           f"every owner declares a surface worth checking "
           f"({ {key: len(value) for key, value in defined.items()} })")
    for name, names in defined.items():
        leaked = sorted(names & set(vars(facade)))
        expect(not leaked,
               f"the command re-exports none of {name}'s names "
               f"(found {leaked})")
    expect(facade.storage is probe_claim_storage
           and facade.lease is probe_claim_lease
           and facade.orchestration is probe_claim_orchestration,
           "the command reaches each owner as the module OBJECT, so a seam "
           "patched on an owner is the one the command calls")


#: This owner's inventory, in the relative order these cases hold within
#: the aggregate's run sequence -- which is NOT contiguous there: the
#: first three run early, then the two boundary cases run near the end,
#: immediately before the CLI case.
CASES = (
    test_census_claim_collection,
    test_claims_are_not_measurements,
    test_schema_migration_is_lossless,
    test_probe_flake_needs_no_docs_worktree,
    test_owner_dependencies_run_one_way,
)
