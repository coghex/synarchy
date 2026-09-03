#!/usr/bin/env python3
"""Focused self-test for the per-probe in-flight eligibility check (#1433).

Deterministic, engine-free, GPU-free and OFFLINE. Every case runs against
synthetic fixtures in a throwaway temporary directory: a synthetic
`codex-test` registry shaped like the real `codex-test-coordinator/v1`
document, synthetic findings reports in both a checked-out and a
`docs-wip` role, and a fake GitHub transport. Nothing here boots an
engine, runs a registered probe, invokes `gh`, opens a socket, or touches
the developer's real machine-local `$test` state. The real
`tools/probe_inflight.py` is imported and driven, so this exercises the
shipped code paths — and the shipped CLI — rather than a copy.

This module is the AGGREGATE COMMAND and the only public entry point.
The cases themselves live with their evidence-source owners (#2141), and
the support every owner shares has one implementation:

    test_probe_inflight_support.py       assertions, tripwires, fixtures
    test_probe_inflight_identity.py      canonical probe identity
    test_probe_inflight_runstate.py      source 1: active `$test` runs
    test_probe_inflight_tracker.py       sources 2 and 3: issues, pulls
    test_probe_inflight_report.py        source 4: the findings reports
    test_probe_inflight_integration.py   cross-source evaluation and CLI

The owners are siblings: each may import the shared support and the
production modules, none imports another owner or this facade.

Two contracts are proved MECHANICALLY rather than inferred from output:

* NON-INTERACTION. Every file under a fixture tree is digested before and
  after each evaluation and must be byte-for-byte identical, path set
  included — registry, reports and lock files alike. `subprocess.run` /
  `Popen`, `fcntl.flock` and `fcntl.lockf` are replaced with tripwires,
  so a coordinator invocation of any subcommand, or any lock, fails the
  test rather than passing quietly. A state root that does not exist is
  checked afterwards to still not exist, so a resolution that CREATED one
  would fail too.

* OFFLINE-NESS. `socket.socket` and `socket.create_connection` are
  tripwires for the whole run, and so is
  `probe_inflight.default_github_transport`. A case that forgot to inject
  a transport therefore FAILS rather than silently skipping or reaching
  the network — which is the difference between the pagination,
  open-versus-closed and draft-versus-merged coverage below meaning
  something and passing vacuously. This facade installs that boundary
  around the ENTIRE case run, so it covers every owner's cases alike.

The three cases that legitimately shell out to `git`
(`test_target_repository_resolution`,
`test_docs_worktree_absence_is_normal_but_damage_is_not` and
`test_the_shipped_cli`) build their own scratch repositories and run
outside the subprocess tripwire, but stay inside the `gh`/socket
tripwires.

Usage:
  python3 tools/test_probe_inflight.py
Exit codes: 0 = all tests passed, 1 = one or more failed.
"""
from __future__ import annotations

import os
import sys

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
import test_probe_inflight_support as support  # noqa: E402
import test_probe_inflight_identity as identity  # noqa: E402
import test_probe_inflight_runstate as runstate  # noqa: E402
import test_probe_inflight_tracker as tracker  # noqa: E402
import test_probe_inflight_report as report  # noqa: E402
import test_probe_inflight_integration as integration  # noqa: E402

# Every owner that must contribute, in the order their cases have always
# executed in. Kept separate from `OWNERS` so that dropping an entry from
# the aggregation below is caught rather than silently running a shorter
# suite that still reports a clean exit.
REQUIRED_OWNERS = ("identity", "runstate", "tracker", "report", "integration")

OWNERS = (
    ("identity", identity),
    ("runstate", runstate),
    ("tracker", tracker),
    ("report", report),
    ("integration", integration),
)


class AggregationError(RuntimeError):
    """An owner collection is missing, empty, or contributes a duplicate.

    Raised — never routed through `check` — so the guard can neither add
    to the passed-check count nor to `FAILURES`, both of which the
    summary line reports verbatim.
    """


def aggregate_cases(owners=OWNERS) -> list:
    """Concatenate every owner's ordered collection, exactly once each.

    Fails loudly rather than quietly running a short set. An owner
    omitted from the aggregation, an extra or reordered one, a missing
    or non-sequence `CASES`, an empty one, and a case contributed by
    more than one collection each raise `AggregationError`.
    """
    contributing = tuple(name for name, _ in owners)
    if contributing != REQUIRED_OWNERS:
        raise AggregationError(
            f"aggregation must run exactly {list(REQUIRED_OWNERS)} in that "
            f"order; got {list(contributing)}")
    cases: list = []
    seen: dict[str, str] = {}
    for name, owner in owners:
        collection = getattr(owner, "CASES", None)
        if not isinstance(collection, (list, tuple)):
            raise AggregationError(
                f"owner {name} exposes no ordered CASES collection: "
                f"{collection!r}")
        if not collection:
            raise AggregationError(f"owner {name} contributes an empty CASES")
        for case in collection:
            if not callable(case):
                raise AggregationError(
                    f"owner {name} contributes a non-callable case: {case!r}")
            if case.__name__ in seen:
                raise AggregationError(
                    f"case {case.__name__} is contributed twice: by "
                    f"{seen[case.__name__]} and again by {name}")
            seen[case.__name__] = name
            cases.append(case)
    return cases


def main() -> int:
    cases = aggregate_cases()
    with support.Offline():
        for case in cases:
            try:
                case()
            except Exception as error:                       # noqa: BLE001
                support.FAILURES.append(
                    f"{case.__name__} raised {type(error).__name__}: {error}")

    # Read the counter THROUGH the support module: `check` rebinds
    # `PASSED` there, so a name bound at import would report zero.
    print(f"probe_inflight self-test: {support.PASSED} checks passed, "
          f"{len(support.FAILURES)} failed")
    for failure in support.FAILURES:
        print(f"  FAIL {failure}")
    return 1 if support.FAILURES else 0


if __name__ == "__main__":
    sys.exit(main())
