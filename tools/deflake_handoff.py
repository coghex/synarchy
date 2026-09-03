#!/usr/bin/env python3
"""The `deflake-outcome-handoff/v1` contract both of epic #1426's consumers read.

`tools/deflake_diagnosis.py` (#1437) decides mechanically whether one
measurement handoff supports a probe-side repair, and emits a
`deflake-diagnosis-outcome/v1` record naming the ROUTE it took. Only one
of those routes opens a pull request; the rest are split across two
sibling consumers of ONE envelope:

* `tools/deflake_outcome.py` (#1439) records the three stable
  non-success outcomes — `cannot-reproduce`, `no-confident-fix` and
  `partial-improvement` — durably in the probe's census row.
* `tools/deflake_issue.py` (#1438) files the tracker issue the
  `production-defect` ending calls for.

This module owns what the two SHARE, and nothing either one decides on
its own. It has no CLI, records nothing, publishes nothing, and imports
neither consumer: everything below reads documents and answers questions
about them, so the two workflows cannot come to disagree about what the
envelope means, and neither is the other's prerequisite. A consumer
supplies its own `RouteOwnership` — the one part of the gate that
differs — and keeps its own outcome vocabulary, its own boundaries and
its own CLI.

    python3 tools/test_deflake_diagnosis.py       # the deterministic gate

Its ownership runs from the envelope to the point a validated `Handoff`
is handed back: the schema and the measurement-role vocabulary, route
ownership and what each of #1437's endings IS, the two exception
classifications, the `Measurement` and `Handoff` representations, every
producer-record, identity, manifest, invocation, path, worktree,
descriptor, artifact and producer-binding rule, the shared
reproduced-failure predicate, and the clock and retry reconciler a
durable record is stamped with.

A document that exists establishes nothing
------------------------------------------
`tools/probe_flake.py`'s exit contract is preserved rather than
paraphrased, and the handoff must state each measurement's exit code so
this can be checked instead of inferred:

* exit 0 — a VALID measurement, whatever failure rate it observed. It
  writes a document whose own `status` is `ok`.
* exit 2 — a rejection before execution. It writes NO document.
* exit 3 — port exhaustion. It writes NO document.
* exit 4 — an untrustworthy harness result. It DOES write a document:
  `probe_flake` renders and writes one whenever it reached the
  measurement loop, and returns `EXIT_HARNESS_ERROR` only from
  `return EXIT_OK if measurement.valid else EXIT_HARNESS_ERROR`. That
  document carries `status: "harness-error"`, a populated `error` and
  `error_run`, and a null `failure_rate`.

So a document that merely EXISTS and PARSES establishes nothing.
`Measurement.trustworthiness_problems` reads the document's own
`status`, `error_run`, `completed_runs` and per-check tallies, and any
of exit 2, 3 or 4, an incomplete run set, malformed measurement data or
an inconsistent aggregate is a batch no consumer may conclude anything
from. That is the load-bearing distinction of the whole envelope: "we
could not make it fail" and "we could not measure it" are opposite
conclusions that a careless reading collapses into one.

Every declared measurement goes through #1437's CANONICAL result gate
first, not just the census validator it starts with:
`probe_census.validate_result` owns declared shape and the cross-field
invariants, while `deflake_diagnosis.require_result` adds the rules that
make a document one `probe_flake.measure` could have written — the run
indices, the artifact topology, and the retention pairing in particular.
`measure` deletes a run's directory the moment it passes and keeps every
unsuccessful one, so a non-PASS run with a null `artifact_dir` is
producer-impossible, and a conclusion drawn from one would be a failure
nobody can diagnose stored as the evidence FOR a diagnosis.

`error_run` gets its own clause because `probe_flake` deliberately keeps
a harness-error run OUT of `runs`. A reader inspecting only `runs` would
see an all-PASS list for a measurement nobody can trust.

A document that contradicts ITSELF is refused the same way, and before
any route is classified rather than only before one of them.
`probe_census.validate_result` binds `check_counts` to `runs` and
refuses a PASS run carrying a FAIL check, but nothing there binds
`failure_count`, `timeout_count` or `failure_rate` to the run list — so
an all-PASS batch under a forged failure count is schema-valid, and it
would read as a REPRODUCED failure, which is the evidence every route
past the `cannot-reproduce` fork rests on. The three totals are
therefore reconciled against the run list using
`probe_flake.Measurement`'s own arithmetic, and so is `completed_runs`,
which the producer writes as `len(runs)` and which makes the rest mean
anything: a nine-run batch claiming ten completed satisfies
`completed_runs == requested_runs` and would be STORED as ten of ten. A
mismatch in any of the four is an untrustworthy measurement.

The record has to agree with itself first
-----------------------------------------
#1437's record states the input identity TWICE: its `handoff` section
carries the probe, the commit, X and the targets of the `/deflake`
invocation consumed, and the top-level fields are derived from that same
handoff — `baseline_sha` IS `handoff.commit_sha`. Validating each side
alone leaves the pair free to disagree, and a record whose handoff
identifies one commit while its top-level field, its baseline reference
and the supplied measurement all name another would satisfy every other
check here. Each duplicated field is re-parsed with the grammar its twin
was parsed with and required to match, so agreement is established
between two VALIDATED values rather than two strings.

One attempt reports against one declared contract
-------------------------------------------------
A batch's DESCRIPTOR is what it reports against, and the identity
binding below cannot see it: a result can keep its probe, its targets,
its commit, its instant and every artifact path while swapping or
relabelling an unrelated declared check, which #1437 rejects rather than
routing anywhere. The record carries the WHOLE ordered descriptor — #1437
serializes it beside the identifier list for this — and every supplied
measurement is held to it through that module's own
`require_descriptor`, so identifiers, order and LABELS are compared by
one definition. Against the RECORD rather than against a sibling
measurement, because the routes that carry one baseline have no sibling,
and those are the routes whose consumers record a de-list
recommendation: a self-consistent handoff that relabelled a check would
otherwise recommend de-listing on the strength of a different asserted
check. A label is the check's stated meaning. The targets are held to
that same descriptor: a target it never declared cannot be one of the
measurement's own non-PASS identifiers.

The measurement is the one that diagnosis judged
------------------------------------------------
Binding a declared measurement to its PROBE alone would admit any
well-formed batch of that probe: one taken at another commit, or another
instant, supplied under a diagnosis that judged a different one, leaving
the census holding two conflicting accounts of a single attempt. So each
declared measurement is held to the producer record's own reference for
its role — commit and instant — and the pre-fix roles are held again to
the `baseline_sha` the census row is about to record. Two independent
statements, because a producer record whose reference and `baseline_sha`
disagreed would satisfy either one alone. A role the producer ran no
batch for carries a `null` reference, and a measurement supplied for it
describes work the invocation did not do.

The run count is the document's, not a literal
----------------------------------------------
Completeness is `completed_runs == requested_runs` and the ceiling is X
out of the measurement's OWN requested run count. Ten is the standard
configured N (`probe_census.POLICY_RUN_COUNT`) and nothing here changes
a measurement semantic, but hard-coding it would silently misclassify a
measurement taken at any other run count — which the handoff carries as
an input.

Paths are references, and they stay outside every worktree
----------------------------------------------------------
Raw stdout, protocol streams and engine logs stay in the harness's
artifact tree outside every worktree; only their path references are
stored, exactly as `probe_census.summarize_sample` already does for a
measurement — and every path a consumer would store is REFUSED if it
lies inside one, which `probe_flake.check_artifact_root` enforces at
measurement time and this enforces again at the point a path can enter a
durable record. The comparison worktrees the producer record declares
are checked beside the live registered ones, because `/deflake` removes
them when it finishes and an artifact that sat inside one was still
inside a worktree when it was written.

Absolute is not the same as usable. Every path goes through
`deflake_diagnosis.require_path` first — including the ones a consumer
stores without ever resolving, a command token and a manifest entry
among them, because those are evidence too. An embedded NUL makes
`Path.resolve()` raise `ValueError` from `lstat` rather than `OSError`,
so such a string passes an absoluteness test, names no location for the
containment check to find, and would be stored as an artifact reference
or an invocation directory while the CLI printed a traceback.

The producer record's own artifact list is REBUILT rather than believed.
#1437 produces it by accumulating every batch it ran, in role order,
deduplicated — so it is derived, and validating each path individually
only asks whether each names a legal place, never whether the list is
the one the invocation produced. An unrelated directory appended to it
alone would otherwise be stored as this attempt's evidence.

The two inputs no census field has ever held are validated here too: the
configuration manifest both batches read, and the exact command and
directory of the `/deflake` invocation the diagnosis consumed.
`probe_census.ingest_result` drops the command and the invocation
directory, and nothing in the census stores the manifest at all, so a
record without them could not say what was run or under what state —
which is the first thing anyone resuming an attempt needs.

The clock is shared because the resume rule is
----------------------------------------------
Both consumers write a durable record keyed on the attempt identity, and
both are idempotent on it. Exactly one field of such a record is not
derived from the handoff — `timestamp_utc` comes from a clock, which
reads differently on a retry — so `utc_now` and `reuse_stored_timestamp`
live here rather than once per consumer. A second reconciler is how one
of them would come to restamp a replay into a conflict.

Where each rule now lives
-------------------------
This module is the stable public import façade and the contract
narrative above; the implementation has four internal owners, in one
one-way order. Nothing below imports this module or either consumer.

* `deflake_handoff_grammar.py` — the envelope vocabulary: the schemas,
  the roles, `ROUTE_ENDING`, `RouteOwnership`, `EXIT_CONTRACT`, the
  limits, `HandoffError` and `NonSuccess`, the object/text/identity/
  list/NUL/usable-path grammar, `delegate_census_grammar`, and the
  artifact-reference, configuration, input-identity and
  invocation-identity validators. A leaf WITHIN this family; the
  upstream rules it applies are called, never copied.
* `deflake_handoff_measurement.py` — `Measurement`,
  `require_measurement` and `require_reproduced`: trustworthiness, the
  run and aggregate reconciliation, the per-check reading, the durable
  summary. Consumes the grammar owner and #1437's result validators.
* `deflake_handoff_producer.py` — `require_diagnosis_outcome`,
  `REFERENCE_FIELDS`, the per-batch reference parsing, the
  measurement-to-producer binding, `require_worktree_boundary`,
  `declared_worktrees`, the artifact-list rebuild, and BOTH descriptor
  rules. Consumes the grammar and measurement owners.
* `deflake_handoff_assembly.py` — `Handoff`, `require_handoff`,
  `utc_now` and `reuse_stored_timestamp`. Consumes all three, and
  INVOKES the producer owner's descriptor and target rules rather than
  defining a second copy.

The two consumers stay `tools/deflake_outcome.py` (#1439) and
`tools/deflake_issue.py` (#1438), each supplying its own
`RouteOwnership` and depending on neither the other nor any internal
owner above. The deterministic verification command is unchanged:

    python3 tools/test_deflake_diagnosis.py
"""
from __future__ import annotations

import os
import sys

# The pre-split module put its own directory on `sys.path` before
# importing anything beside it, which is what makes the repository-root
# spelling `import tools.deflake_handoff` resolve: `tools/` has no
# `__init__.py`, so it is an implicit namespace package whose modules
# still import each other by bare name. This façade's re-exports run at
# import time, so the bootstrap has to happen before the first of them
# or the public contract would resolve only for callers who had already
# put `tools/` on the path themselves. The four internal owners each
# carry the same bootstrap ahead of their own sibling imports.
sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
from deflake_handoff_assembly import (  # noqa: E402
    Handoff,
    require_handoff,
    reuse_stored_timestamp,
    utc_now,
)
from deflake_handoff_grammar import (  # noqa: E402
    DIAGNOSIS_OUTCOME_SCHEMA,
    EXIT_CONTRACT,
    HANDOFF_SCHEMA,
    HandoffError,
    MAX_IDENTITY,
    MAX_SUMMARY,
    NonSuccess,
    PRE_FIX_ROLES,
    ROLES,
    ROLE_BASELINE,
    ROLE_HANDOFF,
    ROLE_VERIFICATION,
    ROUTE_ENDING,
    RouteOwnership,
    delegate_census_grammar,
    require_artifact_reference,
    require_configuration,
    require_input_identity,
    require_invocation_identity,
)
from deflake_handoff_measurement import (  # noqa: E402
    Measurement,
    require_measurement,
    require_reproduced,
)
from deflake_handoff_producer import (  # noqa: E402
    REFERENCE_FIELDS,
    declared_worktrees,
    require_diagnosis_outcome,
    require_worktree_boundary,
)

# The complete public surface of `deflake-outcome-handoff/v1`. Every
# name below is the CANONICAL object its owner defines, bound here and
# not copied, so `deflake_outcome.HandoffError is
# deflake_handoff.HandoffError` and the same holds for `NonSuccess`,
# `Measurement`, `Handoff` and `RouteOwnership`. A consumer importing
# through this façade and an owner importing its sibling directly get
# one and the same object.
__all__ = [
    "DIAGNOSIS_OUTCOME_SCHEMA",
    "EXIT_CONTRACT",
    "HANDOFF_SCHEMA",
    "Handoff",
    "HandoffError",
    "MAX_IDENTITY",
    "MAX_SUMMARY",
    "Measurement",
    "NonSuccess",
    "PRE_FIX_ROLES",
    "REFERENCE_FIELDS",
    "ROLES",
    "ROLE_BASELINE",
    "ROLE_HANDOFF",
    "ROLE_VERIFICATION",
    "ROUTE_ENDING",
    "RouteOwnership",
    "declared_worktrees",
    "delegate_census_grammar",
    "require_artifact_reference",
    "require_configuration",
    "require_diagnosis_outcome",
    "require_handoff",
    "require_input_identity",
    "require_invocation_identity",
    "require_measurement",
    "require_reproduced",
    "require_worktree_boundary",
    "reuse_stored_timestamp",
    "utc_now",
]
