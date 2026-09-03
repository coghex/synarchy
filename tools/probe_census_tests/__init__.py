"""The census self-test's case owners and their fixtures (#2129).

`tools/test_probe_census.py` stays the aggregate command CI and
`tools/ci-local.sh` invoke, unchanged and unqualified; this package
holds the test bodies it composes, divided into the five owners that
were left after #2034 took promotion out:

  `storage`     12  the record's shape and serialization, v1 to v5
                    migration, seeding, reconciliation, ingestion, path
                    substitution, atomic replacement, the preservation
                    guard, independent-process contention, and the
                    unusable docs worktree;
  `policy`      11  #1430's acceptable-failure policy and its CLI, the
                    generic refusal contract, malformed and duplicate
                    target rows, and the mutation CLI;
  `validation`   5  #1492's declared JSON Schema, a malformed schema
                    file, an absent `jsonschema`, the adversarial sweep,
                    and #1493's cross-field invariants;
  `cohort`       9  #1429's cohort accumulation, append order, head
                    movement, staleness, the unmeasured and zero-rate
                    states, history-only statistics, the semantic
                    refusals, and the summary;
  `outcomes`     2  #1439's append-only outcome log and the deferral
                    gate.

alongside `support`, which is only what more than one of them reads.
The sixth family, `promotion`, is #2034's
`tools/test_probe_census_promotion.py` and stays where it is: it is
separately runnable, and the aggregate runs it from that module's own
`CASES` inventory.

Importing this package runs no test and imports no case owner: the
facade imports the five families itself, so `--family cohort` need not
pay for `validation`'s exhaustive schema sweep, and nothing here can
register a group by side effect.

**These modules are outside the tree-wide helper audit's reach.**
`tools/test_selftestlib.py` builds both of its rosters from a
NON-recursive `TOOLS.glob("*.py")`, so nothing in this directory is seen
by `test_no_importer_keeps_a_local_helper`,
`test_no_importer_registers_a_failure_behind_the_count` or
`test_the_narrating_body_survives_only_in_the_module`. That is why the
facade's own composition guard re-checks those three properties over
this package before it runs anything (#2129): no module here may define
an assertion helper, keep a failure accumulator of its own, or narrate a
passing assertion. Everything asserts through `support.expect`, which is
`selftestlib`'s one function object.
"""
