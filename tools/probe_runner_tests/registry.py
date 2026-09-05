#!/usr/bin/env python3
"""Registry data and the selection it answers (#2130).

Fifteen groups over `probe_runner_registry` -- the declarations
themselves, checked against the SHIPPED tables rather than a synthetic
restatement of them, plus the selection behaviour those declarations
decide:

  every timeout override names a registered probe and is a validated
  number;
  every expected duration does too, and `dispatch_order` turns those
  declarations into the order `--jobs` considers probes in -- longest
  first, undeclared last, ties stable, and always the same items back;
  every exclusive-resource declaration is data about real probes, and
  every probe declares what an exclusive holder takes;
  exact selection refuses an unknown key before listing and never runs
  the valid half of a mixed request, an all-invalid request keeps its
  existing diagnostic, an all-valid one is unaffected, duplicates
  collapse, and substring selection stays permissive;
  port spans are data about real probes, a parallel allocation never
  overlaps, a GUI port is refused across the WHOLE span, and `--port`
  bases the parallel allocation.
"""
from __future__ import annotations

from .support import (
    Tree,
    free_port_span,
    main_refusal,
    main_with,
    overlaps,
)

import probe_runner_registry  # noqa: E402
import probe_runner_resources  # noqa: E402
from selftestlib import expect  # noqa: E402


def test_timeout_overrides_are_validated_registry_data() -> None:
    print("\n-- per-probe timeout defaults are validated registry data")
    expect(probe_runner_registry.timeout_override_problems() == [],
           "the shipped timeout declarations are valid")
    expect(probe_runner_registry.effective_timeout("save_compat_migration") == 3600.0,
           "save_compat_migration receives its declared 3600s default")
    expect(probe_runner_registry.effective_timeout("movement")
           == probe_runner_registry.DEFAULT_TIMEOUT,
           "an ordinary registered probe keeps the shared default")
    expect(probe_runner_registry.effective_timeout("save_compat_migration", 17.0) == 17.0,
           "an explicit CLI value wins over the key-specific default")

    unknown = probe_runner_registry.timeout_override_problems(
        overrides={"not_registered": 1.0})
    expect(any("unknown probe key" in problem for problem in unknown),
           f"an unknown declaration is rejected ({unknown})")
    for bad in (0, -1, float("inf"), float("nan"), True, "900"):
        problems = probe_runner_registry.timeout_override_problems(
            overrides={"movement": bad})
        expect(any("finite and positive" in problem for problem in problems),
               f"an unusable timeout {bad!r} is rejected ({problems})")

    tree = Tree()
    try:
        tree.add("ordinary", exit_code=0)
        for bad in ("0", "-1", "nan", "inf"):
            rc, out = main_with(tree, ["--timeout", bad])
            expect(rc == 2 and "finite and positive" in out,
                   f"CLI --timeout {bad!r} is rejected before execution ({out!r})")
        expect(not tree.started("ordinary"),
               "no probe starts for an invalid explicit timeout")
    finally:
        tree.cleanup()


def test_expected_durations_are_validated_registry_data() -> None:
    print("\n-- per-probe expected durations are validated registry data")
    expect(probe_runner_registry.expected_duration_problems() == [],
           "the shipped expected-duration declarations are valid")
    # Every declared key is a real probe and every real probe either has a
    # number or honestly has nothing; there is no default standing in for a
    # measurement nobody took.
    for key, seconds in probe_runner_registry.PROBE_EXPECTED_SECONDS.items():
        expect(probe_runner_registry.expected_seconds(key) == seconds,
               f"{key} reads back the duration it declares ({seconds})")
    expect(probe_runner_registry.expected_seconds("definitely_not_a_probe")
           is None,
           "an unregistered key has no expectation rather than a default")

    unknown = probe_runner_registry.expected_duration_problems(
        expectations={"not_registered": 1.0})
    expect(any("unknown probe key" in problem for problem in unknown),
           f"an unknown declaration is rejected ({unknown})")
    for bad in (0, -1, float("inf"), float("nan"), True, False, "90"):
        problems = probe_runner_registry.expected_duration_problems(
            expectations={"movement": bad})
        expect(any("finite and positive" in problem for problem in problems),
               f"an unusable expectation {bad!r} is rejected ({problems})")


def test_dispatch_order_is_a_stable_longest_first_reordering() -> None:
    print("\n-- dispatch_order sorts longest-first, keeps ties stable, and "
          "returns the same items")
    saved = probe_runner_registry.PROBE_EXPECTED_SECONDS
    try:
        # Synthetic declarations, so the case states the ordering rule
        # rather than restating whichever probes happen to be slow today.
        probe_runner_registry.PROBE_EXPECTED_SECONDS = {
            "a": 10.0, "b": 30.0, "d": 30.0, "e": 5.0,
        }
        items = [("a", "a.py", ""), ("b", "b.py", ""), ("c", "c.py", ""),
                 ("d", "d.py", ""), ("e", "e.py", ""), ("f", "f.py", "")]
        got = [key for key, _, _ in
               probe_runner_registry.dispatch_order(items)]
        expect(got == ["b", "d", "a", "e", "c", "f"],
               f"longest first, the 30.0 tie in its original order, then "
               f"the undeclared probes in theirs (got {got})")

        # The scheduler's own shape: each probe paired with the positional
        # index its reserved port span was allocated against. The pairing
        # must survive, or a reordered dispatch would hand a probe another
        # probe's port span (#1571).
        indexed = list(enumerate(items))
        reordered = probe_runner_registry.dispatch_order(
            indexed, key=lambda item: item[1][0])
        expect([item[1][0] for item in reordered] == got,
               f"the same order through a custom key extractor "
               f"(got {[item[1][0] for item in reordered]})")
        expect(all(items[position] is probe for position, probe in reordered),
               f"and every probe still carries its own allocation index "
               f"(got {[(position, probe[0]) for position, probe in reordered]})")

        # A REORDERING, not a filter: nothing may be dropped or duplicated
        # on the way through, which is what confines the change to which
        # probe starts next.
        expect(sorted(reordered) == sorted(indexed),
               "the same items come back, none lost and none duplicated")
        expect(probe_runner_registry.dispatch_order([]) == [],
               "an empty selection reorders to an empty selection")

        probe_runner_registry.PROBE_EXPECTED_SECONDS = {}
        bare = [key for key, _, _ in
                probe_runner_registry.dispatch_order(items)]
        expect(bare == ["a", "b", "c", "d", "e", "f"],
               f"with nothing declared the input order is preserved exactly "
               f"(got {bare})")
    finally:
        probe_runner_registry.PROBE_EXPECTED_SECONDS = saved


def test_exclusive_resource_declaration_is_data_about_real_probes() -> None:
    print("\n-- the shipped EXCLUSIVE_RESOURCES table names registered probes")
    known = {p[0] for p in probe_runner_registry.PROBES}
    unknown = sorted(k for k in probe_runner_resources.EXCLUSIVE_RESOURCES if k not in known)
    expect(not unknown,
           f"every declared key names a registered probe (unknown: {unknown})")
    empty = sorted(k for k, v in probe_runner_resources.EXCLUSIVE_RESOURCES.items() if not v)
    expect(not empty,
           f"and every declaration names at least one resource (empty: {empty})")
    both = (probe_runner_resources.exclusive_resources("config_migration")
            & probe_runner_resources.exclusive_resources("config_state"))
    expect(bool(both),
           f"the two config probes still declare an intersecting resource "
           f"(shared: {sorted(both)})")
    expect(not probe_runner_resources.exclusive_resources("combat_anim"),
           "an undeclared probe needs nothing exclusively")


def test_every_probe_declares_what_an_exclusive_holder_takes() -> None:
    print("\n-- the shipped declaration serializes EVERY exclusive holder "
          "against the whole registry (#1444, #1570)")
    expect(bool(probe_runner_resources.IMPLICIT_SHARED_RESOURCES),
           "there is an implicit shared interest at all "
           f"(got {probe_runner_resources.IMPLICIT_SHARED_RESOURCES!r})")
    declared = set(probe_runner_resources.EXCLUSIVE_RESOURCES)
    config_probes = {"config_migration", "config_state"}
    expect(config_probes <= declared,
           f"both config probes are still exclusive holders (#1322/#1444) "
           f"(declared: {sorted(declared)})")
    for key in sorted(config_probes):
        expect("repo-config" in probe_runner_resources.exclusive_resources(key),
               f"{key} still takes repo-config exclusively")
    # The three probes that still drive Cabal themselves -- a `cabal repl`
    # through persistence_snapshot / save_compat_audit, which is NOT an
    # engine boot and has no prebuilt equivalent (#1570).
    ghci = {"persistence_contract", "persistence_contract_sweep",
            "save_compat_migration"}
    expect(ghci <= declared,
           f"every GHCi consumer is an exclusive holder too "
           f"(missing: {sorted(ghci - declared)})")
    for key in sorted(ghci):
        expect("cabal-build" in probe_runner_resources.exclusive_resources(key),
               f"{key} takes the shared Cabal build state exclusively")
    for key in sorted(declared):
        # An interest is one or the other, never both, or a release would
        # drop the exclusive half and leave the shared count behind.
        overlap = (probe_runner_resources.exclusive_resources(key)
                   & probe_runner_resources.shared_resources(key))
        expect(not overlap,
               f"{key} holds no resource in both interests "
               f"(both: {sorted(overlap)})")
    # The whole registry, per holder, not a sample: this is what makes
    # "nothing else may run beside an exclusive holder" a property of the
    # shipped data rather than of the synthetic probes the scheduling
    # tests above drive. An interest in EITHER direction counts -- two
    # exclusive holders of one resource exclude each other too, which is
    # how the three GHCi consumers stay off each other's `cabal repl`.
    for key in sorted(declared):
        taken = probe_runner_resources.exclusive_resources(key)
        unguarded = sorted(
            other for other, _, _ in probe_runner_registry.PROBES
            if other != key
            and not taken <= (probe_runner_resources.shared_resources(other)
                              | probe_runner_resources.exclusive_resources(other)))
        expect(not unguarded,
               f"every other registered probe declares an interest in "
               f"everything {key} takes, so none can be scheduled beside it "
               f"(unguarded: {unguarded})")


def test_exact_mixed_selection_is_rejected_before_listing() -> None:
    print("\n-- --exact + --list with unknown keys alongside a valid one is "
          "rejected, not partially listed")
    tree = Tree()
    try:
        tree.add("good", exit_code=0)
        rc, out = main_with(
            tree, ["--only", "good,not_a_probe,also_bad", "--exact", "--list"])
        expect(rc != 0,
               f"a mixed valid/invalid --exact selection must fail (got {rc})")
        expect("not_a_probe" in out and "also_bad" in out,
               f"the diagnostic names every unknown key, got: {out!r}")
        expect("good_probe.py" not in out,
               f"no partial listing of the valid probe leaks through, got: {out!r}")
    finally:
        tree.cleanup()


def test_exact_mixed_selection_never_runs_the_valid_probe() -> None:
    print("\n-- the same rejection happens before RUNNING anything, not just listing")
    tree = Tree()
    try:
        tree.add("good", exit_code=0)
        rc, out = main_with(tree, ["--only", "good,not_a_probe", "--exact"])
        expect(rc != 0, f"the mixed selection is rejected (got {rc})")
        expect(not tree.started("good"), "the valid probe never actually started")
    finally:
        tree.cleanup()


def test_exact_all_invalid_selection_keeps_existing_diagnostic() -> None:
    print("\n-- an all-invalid --exact selection keeps the pre-existing "
          "empty-selection error and exit code")
    tree = Tree()
    try:
        tree.add("good", exit_code=0)
        rc, out = main_with(tree, ["--only", "not_a_probe", "--exact", "--list"])
        expect(rc == 2, f"an all-invalid --exact selection still exits 2 (got {rc})")
        expect("matched no probes" in out,
               f"and keeps the existing 'matched no probes' diagnostic, got: {out!r}")
    finally:
        tree.cleanup()


def test_exact_all_valid_selection_is_unaffected() -> None:
    print("\n-- a wholly valid --exact selection lists in registry order, unchanged")
    tree = Tree()
    try:
        tree.add("alpha", exit_code=0)
        tree.add("beta", exit_code=0)
        rc, out = main_with(tree, ["--only", "beta,alpha", "--exact", "--list"])
        expect(rc == 0, f"a wholly valid --exact selection still exits 0 (got {rc})")
        expect(out.index("alpha_probe.py") < out.index("beta_probe.py"),
               f"registry order survives regardless of request order, got: {out!r}")
    finally:
        tree.cleanup()


def test_exact_duplicate_valid_keys_still_collapse() -> None:
    print("\n-- a wholly valid --exact selection with a duplicated key lists it once")
    tree = Tree()
    try:
        tree.add("alpha", exit_code=0)
        rc, out = main_with(tree, ["--only", "alpha,alpha", "--exact", "--list"])
        expect(rc == 0, f"still exits 0 (got {rc})")
        expect(out.count("alpha_probe.py") == 1,
               f"a duplicated valid key is listed exactly once, got: {out!r}")
    finally:
        tree.cleanup()


def test_substring_selection_stays_permissive() -> None:
    print("\n-- substring (non --exact) selection still ignores an unmatched needle")
    tree = Tree()
    try:
        tree.add("craft", exit_code=0)
        rc, out = main_with(tree, ["--only", "craft,not_a_probe", "--list"])
        expect(rc == 0,
               f"substring selection with one unmatched needle still succeeds (got {rc})")
        expect("craft_probe.py" in out, "the matching probe is still listed")
    finally:
        tree.cleanup()


def test_port_span_declaration_is_data_about_real_probes() -> None:
    print("\n-- the shipped PROBE_PORT_SPANS table names registered probes")
    known = {p[0] for p in probe_runner_registry.PROBES}
    unknown = sorted(k for k in probe_runner_registry.PROBE_PORT_SPANS if k not in known)
    expect(not unknown,
           f"every declared key names a registered probe (unknown: {unknown})")
    bad = sorted(k for k, v in probe_runner_registry.PROBE_PORT_SPANS.items()
                 if not isinstance(v, int) or isinstance(v, bool) or v < 1)
    expect(not bad,
           f"every declaration is a positive port COUNT (bad: {bad})")
    expect(probe_runner_registry.port_span("debug_console_boot") == 2,
           "debug_console_boot declares two ports -- it binds base and base+1")
    expect(probe_runner_registry.port_span("offscreen") == 2,
           "offscreen declares two ports -- its second engine runs alongside "
           "the first")
    expect(probe_runner_registry.port_span("combat_anim") == probe_runner_registry.DEFAULT_PORT_SPAN
           == 1,
           "an undeclared probe reserves its base alone")
    expect(list(probe_runner_registry.reserved_ports("debug_console_boot", 9400))
           == [9400, 9401],
           "a declared count N reserves base .. base+N-1, contiguously")
    expect(list(probe_runner_registry.reserved_ports("combat_anim", 9400)) == [9400],
           "and an undeclared probe reserves exactly its base")


def test_parallel_allocation_never_overlaps() -> None:
    print("\n-- the parallel allocation lays declared spans end to end")
    # Every registered probe at once: whatever the table says, no two
    # selected probes may be handed a port the other may bind.
    ports = probe_runner_registry.allocate_parallel_ports(probe_runner_registry.PROBES)
    expect(len(ports) == len(probe_runner_registry.PROBES),
           "every selected probe gets exactly one base")
    claimed: dict[int, str] = {}
    overlaps: list[str] = []
    for (key, _, _), base in zip(probe_runner_registry.PROBES, ports):
        for port in probe_runner_registry.reserved_ports(key, base):
            if port in claimed:
                overlaps.append(f"{key} and {claimed[port]} both reserve {port}")
            claimed[port] = key
    expect(not overlaps,
           f"no two probes reserve the same port (overlaps: {overlaps[:3]})")
    expect(ports == sorted(ports) and len(set(ports)) == len(ports),
           "bases are handed out in registry order, each strictly after the last")
    expect(ports[0] == probe_runner_registry.PARALLEL_PORT_BASE,
           f"the default origin is still {probe_runner_registry.PARALLEL_PORT_BASE} "
           f"(got {ports[0]})")

    # The exact pair from #1571, in the order that broke: a two-port
    # probe immediately before a one-port one.
    pair = [("debug_console_boot", "a.py", ""), ("transactional_load", "b.py", "")]
    got = probe_runner_registry.allocate_parallel_ports(pair, 9400)
    expect(got == [9400, 9402],
           f"debug_console_boot's neighbour starts past its span (got {got})")
    expect(probe_runner_registry.allocate_parallel_ports(pair, 9500) == [9500, 9502],
           "and the layout follows a caller-supplied origin")


def test_gui_port_refusal_covers_the_whole_span() -> None:
    print("\n-- a span that REACHES the GUI port is refused, not just a base "
          "that equals it")
    expect(probe_runner_registry.GUI_PORT == 8008,
           f"the GUI port is still 8008 (got {probe_runner_registry.GUI_PORT})")
    spans = {"wide": 2}
    saved = probe_runner_registry.PROBE_PORT_SPANS
    probe_runner_registry.PROBE_PORT_SPANS = spans
    try:
        conflicts = probe_runner_registry.gui_port_conflicts(
            [("wide", 8007), ("narrow", 8007), ("wide", 9400)])
        expect(conflicts == [("wide", 8007)],
               f"only the span that actually covers 8008 conflicts "
               f"(got {conflicts})")
        text = probe_runner_registry.describe_gui_conflicts(conflicts)
        expect("8007-8008" in text and "wide" in text and "8008" in text,
               f"the refusal names the probe and the span (got {text!r})")
        expect(probe_runner_registry.gui_port_conflicts(
                   [("wide", 8007), ("wide", 8007)]) == [("wide", 8007)],
               "the same probe at the same base is one conflict, not two -- a "
               "parallel plan lists it twice (allocation and solo-retry origin)")
    finally:
        probe_runner_registry.PROBE_PORT_SPANS = saved

    for jobs in ("1", "2"):
        tree = Tree()
        try:
            tree.add("wide", dwell=0.0)
            tree.add("narrow", dwell=0.0)
            rc, out = main_refusal(
                tree, ["--only", "wide,narrow", "--exact", "--jobs", jobs,
                       "--port", "8007"],
                spans={"wide": 2})
            expect(rc == 2,
                   f"--jobs {jobs} --port 8007 with a two-port probe is a bad "
                   f"invocation (got {rc})")
            expect("8008" in out and "wide" in out,
                   f"and says which probe reaches the GUI port (got {out!r})")
            expect(not tree.started("wide") and not tree.started("narrow"),
                   "nothing was launched -- the plan is validated before any "
                   "subprocess exists")
        finally:
            tree.cleanup()

    # The pre-#1571 exact-base refusal is unchanged.
    tree = Tree()
    try:
        tree.add("narrow")
        rc, out = main_refusal(tree, ["--only", "narrow", "--exact",
                                       "--port", "8008"])
        expect(rc != 0 and "8008" in out,
               f"--port 8008 itself is still refused (got {rc}, {out!r})")
        expect(not tree.started("narrow"), "and still starts nothing")
    finally:
        tree.cleanup()


def test_port_with_jobs_bases_the_parallel_allocation() -> None:
    print("\n-- --port is HONOURED with --jobs: it is the allocation origin")
    tree = Tree()
    try:
        base = free_port_span(4)
        tree.add("wide", bind_span=2)
        tree.add("narrow", bind_span=1)
        rc, out = main_with(tree, ["--only", "wide,narrow", "--exact",
                                    "--jobs", "2", "--retries", "0",
                                    "--port", str(base)],
                             spans={"wide": 2})
        expect(rc == 0, f"both probes passed (got {rc})\n{out}")
        expect(tree.ports("wide") == [base],
               f"the first probe is based at --port itself "
               f"(got {tree.ports('wide')}, wanted [{base}])")
        expect(tree.ports("narrow") == [base + 2],
               f"the second starts past the first's TWO-port span "
               f"(got {tree.ports('narrow')}, wanted [{base + 2}])")
    finally:
        tree.cleanup()

    # Sequential is unchanged: one base, handed to every probe.
    tree = Tree()
    try:
        base = free_port_span(2)
        tree.add("wide", bind_span=2)
        tree.add("narrow", bind_span=1)
        rc, out = main_with(tree, ["--only", "wide,narrow", "--exact",
                                    "--port", str(base)],
                             spans={"wide": 2})
        expect(rc == 0, f"sequentially both still pass (got {rc})\n{out}")
        expect(tree.ports("wide") == [base] and tree.ports("narrow") == [base],
               f"and both still get the same base "
               f"(wide {tree.ports('wide')}, narrow {tree.ports('narrow')})")
    finally:
        tree.cleanup()

    # Unset, the parallel allocation still starts at the default origin.
    tree = Tree()
    try:
        tree.add("alpha")
        tree.add("beta")
        rc, _ = main_with(tree, ["--only", "alpha,beta", "--exact",
                                  "--jobs", "2"])
        expect(rc == 0, f"the default-origin run still passes (got {rc})")
        expect(tree.ports("alpha") == [probe_runner_registry.PARALLEL_PORT_BASE]
               and tree.ports("beta") == [probe_runner_registry.PARALLEL_PORT_BASE + 1],
               f"unset --port keeps the 9400 origin "
               f"(alpha {tree.ports('alpha')}, beta {tree.ports('beta')})")
    finally:
        tree.cleanup()


#: Timeout-override and expected-duration declarations, and the dispatch
#: order the latter decides (#2275).
TESTS_TIMEOUT_DECLARATIONS = (
    test_timeout_overrides_are_validated_registry_data,
    test_expected_durations_are_validated_registry_data,
    test_dispatch_order_is_a_stable_longest_first_reordering,
)

#: Exclusive-resource declaration completeness.
TESTS_EXCLUSIVE_DECLARATIONS = (
    test_exclusive_resource_declaration_is_data_about_real_probes,
    test_every_probe_declares_what_an_exclusive_holder_takes,
)

#: Exact selection's refusals, its duplicate collapse, and the permissive
#: substring path beside them.
TESTS_EXACT_SELECTION = (
    test_exact_mixed_selection_is_rejected_before_listing,
    test_exact_mixed_selection_never_runs_the_valid_probe,
    test_exact_all_invalid_selection_keeps_existing_diagnostic,
    test_exact_all_valid_selection_is_unaffected,
    test_exact_duplicate_valid_keys_still_collapse,
    test_substring_selection_stays_permissive,
)

#: Reserved port spans (#1571) and the allocation they decide.
TESTS_PORT_SPANS = (
    test_port_span_declaration_is_data_about_real_probes,
    test_parallel_allocation_never_overlaps,
    test_gui_port_refusal_covers_the_whole_span,
    test_port_with_jobs_bases_the_parallel_allocation,
)

#: This family's complete ordered inventory: its fragments, in the order
#: the aggregate runs them, which is also the order `--family registry`
#: runs them in.
TESTS = (TESTS_TIMEOUT_DECLARATIONS + TESTS_EXCLUSIVE_DECLARATIONS
         + TESTS_EXACT_SELECTION + TESTS_PORT_SPANS)
