#!/usr/bin/env python3
"""Unit tests for the per-boot engine logs of the two multi-boot probes (#1763)
and for `tools/preview_probe.py`'s family dispatcher (#2089).

`tools/preview_probe.py` and `tools/offscreen_probe.py` both launch several
fresh engines in one run, and `probelib.boot` opens its log truncating. When
two launches shared one path, every boot after the first destroyed its
predecessor's capture — preview kept only its last, and offscreen's
port-reusing restart overwrote the long session (loading screen, worldgen,
gameplay, icon discovery) with the brief load-and-check that follows it.

Both probes need a GPU and are manual-only, so their real acceptance is a
long dev-machine run. What is pinned here is the half that can regress
silently and be checked in milliseconds:

  * Every allocation is a distinct path, INCLUDING a repeated phase. The
    preview run browses `icons`, `units/acolyte` and `structures/wire` in
    more than one phase, so a target-derived name alone is not unique.
  * A truncating open per allocated path leaves every earlier capture
    intact — the defect stated in the terms it actually appeared in.
  * Offscreen's three-engine lifecycle keeps its restart on the FIRST
    engine's port, and that restart no longer truncates the first log.
  * The phase->path map survives a failed run: `probelib.boot` raises
    SystemExit when an engine dies before READY, which is exactly the run
    whose log a reader needs, so both probes report from a `finally`.
  * Every preview engine launch, in every family owner under
    `tools/preview/`, allocates its own log: the owners never call
    `probelib.boot` at all, and the one launcher they share does so
    through `preview.harness.LOGS`. A new call site added around it would
    silently fall back to probelib's shared per-port default and collide
    again.

Since #2089 the preview probe is a facade over five family owners, and its
dispatcher is pinned here too, entirely from the facade's own `FAMILIES`
inventory rather than a list written down in this file (which is how the
previous "full run" phase list drifted to an obsolete synthetic roster
and lost the zoom family): the default run invokes every family exactly
once, in inventory order; each `--only` selector invokes only its own
family; an unknown selector exits non-zero having allocated nothing and
booted nothing; the inventory names every scenario-shaped function in
every owner exactly once; a scenario that fails is reported as a FAIL
exit rather than swallowed; and every launch of a run keeps its own
capture, with the closing summary counting exactly the launches made.

No engine, no world, no GPU: everything here runs against temporary files
and stubbed launches.

Usage:
  python3 tools/test_probe_boot_logs.py
Exit codes: 0 = all tests passed, 1 = one or more failed.
"""
from __future__ import annotations

import contextlib
import inspect
import io
import re
import sys
import tempfile
from pathlib import Path

TOOLS = Path(__file__).resolve().parent
sys.path.insert(0, str(TOOLS))
import offscreen_probe  # type: ignore  # noqa: E402
import preview_probe  # type: ignore  # noqa: E402
from preview import harness as preview_harness  # type: ignore  # noqa: E402

import selftestlib  # noqa: E402
from selftestlib import FAILURES, expect  # noqa: E402

#: The five family names #2089 fixes, in the order the aggregate runs
#: them. Spelled out here ON PURPOSE: this is the one place a test may
#: state the contract independently of the inventory, so a renamed or
#: reordered family fails rather than being read back from the facade.
PREVIEW_FAMILY_NAMES = ("simple", "units", "buildings", "dispatch", "zoom")


@contextlib.contextmanager
def scratch_logs():
    """A throwaway log directory for both probes' allocators.

    The preview allocator is the single shared instance every family
    launches through, `preview.harness.LOGS` (#2089 moved it there from
    `preview_probe.LOGS`); swapping that one attribute redirects every
    owner, because none of them binds it at import.
    """
    with tempfile.TemporaryDirectory() as tmp:
        preview_original = preview_harness.LOGS
        offscreen_original = offscreen_probe.Engines.LOG_DIR
        preview_harness.LOGS = preview_harness.BootLogs(directory=tmp)
        offscreen_probe.Engines.LOG_DIR = tmp
        try:
            yield tmp
        finally:
            preview_harness.LOGS = preview_original
            offscreen_probe.Engines.LOG_DIR = offscreen_original


def truncating_write(path: str, text: str) -> None:
    """Write the way `probelib.boot` opens a log: mode 'w'."""
    with open(path, "w") as fh:
        fh.write(text)


def all_scenarios() -> list[tuple[str, str]]:
    """(family, scenario) for every entry of the facade's inventory, in
    the order the aggregate runs them — derived, never restated."""
    return [(family.name, scenario)
            for family in preview_probe.FAMILIES
            for scenario in family.scenarios]


@contextlib.contextmanager
def stubbed_preview_run(failing: set[str] = frozenset()):
    """Replace every inventoried scenario with a recorder that performs one
    launch through the REAL shared launcher, with `probelib.boot` itself
    faked to write the log a real engine would.

    Yields the launch record: (family, scenario, port, log path) per
    launch, in call order. A scenario named in ``failing`` records its
    launch and then reports False, the way a real check failure does.
    """
    launched: list[tuple[str, str, int, str]] = []
    originals: list[tuple[object, str, object]] = []
    original_boot = preview_harness.boot

    def fake_boot(port, log=None, **_kw):
        truncating_write(log, f"launch {len(launched) + 1} on port {port}")
        return object()

    preview_harness.boot = fake_boot
    try:
        for family in preview_probe.FAMILIES:
            for scenario in family.scenarios:
                originals.append((family.module, scenario,
                                  getattr(family.module, scenario)))

                def recorder(port, _family=family.name, _scenario=scenario):
                    path = preview_harness.LOGS.allocate(
                        f"{_family}: {_scenario}")
                    # The same path the real launcher hands probelib: go
                    # through it so the test exercises the shared funnel.
                    preview_harness.boot(port, log=path)
                    launched.append((_family, _scenario, port, path))
                    return _scenario not in failing

                setattr(family.module, scenario, recorder)
        yield launched
    finally:
        preview_harness.boot = original_boot
        for module, scenario, original in originals:
            setattr(module, scenario, original)


def run_facade(argv: list[str]) -> tuple[int | None, str, str]:
    """Run `preview_probe.main(argv)` capturing stdout/stderr; returns
    (exit code — the SystemExit code when it raised, else the return —
    stdout, stderr)."""
    out, err = io.StringIO(), io.StringIO()
    with contextlib.redirect_stdout(out), contextlib.redirect_stderr(err):
        try:
            code: int | None = preview_probe.main(argv)
        except SystemExit as exc:
            code = exc.code if isinstance(exc.code, int) else 1
    return code, out.getvalue(), err.getvalue()


# ---------------------------------------------------------------------------
# preview_probe: one log per boot, and the family dispatcher (#2089)
# ---------------------------------------------------------------------------
def test_a_repeated_phase_still_gets_its_own_log() -> None:
    print("preview: a repeated phase gets its own log")
    with scratch_logs():
        logs = preview_harness.LOGS
        first = logs.allocate("9. sweep units/acolyte")
        second = logs.allocate("9. sweep units/acolyte")
        expect(first != second,
               "the same phase allocated twice yields two distinct paths")


def test_the_inventory_names_every_scenario_exactly_once() -> None:
    print("preview: the inventory is the five families, each owning its "
          "scenario-shaped functions exactly once")
    families = preview_probe.FAMILIES
    expect(tuple(f.name for f in families) == PREVIEW_FAMILY_NAMES,
           f"the families are {PREVIEW_FAMILY_NAMES} in that order "
           f"(got {tuple(f.name for f in families)})")
    expect(preview_probe.FAMILY_NAMES == PREVIEW_FAMILY_NAMES,
           "FAMILY_NAMES — what argparse's choices are built from — "
           "matches the inventory")
    names = [scenario for _, scenario in all_scenarios()]
    expect(len(names) == len(set(names)),
           f"no scenario appears in two families or twice in one: {names}")
    modules = [f.module for f in families]
    expect(len(modules) == len(set(modules)),
           "every family has its own owner module")
    for family in families:
        expect(family.module.__name__ == f"preview.{family.name}",
               f"family {family.name!r} is owned by preview.{family.name} "
               f"(got {family.module.__name__})")
        # A scenario is a module-level `check_*` function defined IN the
        # owner (not imported into it) taking exactly (port). Every such
        # function must be inventoried, and nothing else may be.
        shaped = sorted(
            name for name, fn in vars(family.module).items()
            if name.startswith("check_") and inspect.isfunction(fn)
            and fn.__module__ == family.module.__name__
            and list(inspect.signature(fn).parameters) == ["port"])
        expect(shaped == sorted(family.scenarios),
               f"{family.name}: the inventory names exactly the "
               f"scenario-shaped functions of its owner (inventory="
               f"{sorted(family.scenarios)} owner={shaped})")
        for scenario in family.scenarios:
            expect(callable(getattr(family.module, scenario, None)),
                   f"{family.name}.{scenario} resolves on its owner")


def test_the_default_run_invokes_every_family_once_in_order() -> None:
    print("preview: the default run invokes every family exactly once, in "
          "inventory order, each launch keeping its own capture")
    with scratch_logs(), stubbed_preview_run() as launched:
        code, printed, _ = run_facade([])
        # Read the captures while the scratch directory still exists.
        kept = [Path(path).read_text() for _, _, _, path in launched]
    expect(code == 0, f"an all-pass run exits 0 (got {code})")
    expect([(family, scenario) for family, scenario, _, _ in launched]
           == all_scenarios(),
           "the scenarios ran exactly once each, in the inventory's order "
           f"(got {[(f, s) for f, s, _, _ in launched]})")
    ports = {port for _, _, port, _ in launched}
    expect(ports == {9150}, f"every launch used the default port (got {ports})")
    paths = [path for _, _, _, path in launched]
    expect(len(set(paths)) == len(paths),
           f"{len(paths)} launches allocated {len(paths)} distinct paths "
           f"(got {len(set(paths))})")
    expect(kept == [f"launch {index + 1} on port 9150"
                    for index in range(len(paths))],
           "each log still holds the output of exactly its own launch")
    expect(f"engine logs from this run ({len(paths)} boots)" in printed,
           f"the closing summary counts exactly the launches made: "
           f"{printed!r}")
    expect("PASS: --preview real-boot browser\n" in printed,
           "the aggregate summary line is unchanged by the split")


def test_each_selector_invokes_only_its_family() -> None:
    print("preview: each --only selector invokes only its own family")
    for name in PREVIEW_FAMILY_NAMES:
        with scratch_logs(), stubbed_preview_run() as launched:
            code, printed, _ = run_facade(["--only", name, "--port", "9177"])
        want = [(family, scenario) for family, scenario in all_scenarios()
                if family == name]
        expect(code == 0, f"--only {name}: an all-pass run exits 0 (got {code})")
        expect([(family, scenario) for family, scenario, _, _ in launched]
               == want,
               f"--only {name}: exactly its own scenarios ran, in order "
               f"(got {[(f, s) for f, s, _, _ in launched]})")
        expect({port for _, _, port, _ in launched} == {9177},
               f"--only {name}: --port is still honoured")
        expect(f"engine logs from this run ({len(want)} boot" in printed,
               f"--only {name}: the summary counts only this family's launches")
        expect(f"[--only {name}]" in printed,
               f"--only {name}: the final line names the focused family")


def test_an_unknown_selector_invokes_nothing() -> None:
    print("preview: an unknown --only value is rejected before any log or "
          "engine exists")
    with scratch_logs(), stubbed_preview_run() as launched:
        code, printed, errors = run_facade(["--only", "everything"])
        allocated = list(preview_harness.LOGS._allocated)
        summary = io.StringIO()
        with contextlib.redirect_stdout(summary):
            preview_harness.LOGS.report()
    expect(code not in (0, None), f"the run exits non-zero (got {code})")
    expect(launched == [], f"no scenario ran (got {launched})")
    expect(allocated == [], f"no log path was allocated (got {allocated})")
    expect("no engine was booted" in summary.getvalue(),
           "the allocator's report takes its no-boot branch")
    expect("PASS" not in printed and "engine logs from this run" not in printed,
           f"nothing was reported as run: {printed!r}")
    expect(all(name in errors for name in PREVIEW_FAMILY_NAMES),
           f"the rejection names the five valid choices: {errors!r}")


def test_the_help_documents_exactly_the_five_families() -> None:
    print("preview: --help lists exactly the five focused choices")
    code, printed, _ = run_facade(["--help"])
    expect(code == 0, f"--help exits 0 (got {code})")
    match = re.search(r"--only \{([^}]*)\}", printed)
    expect(match is not None
           and tuple(match.group(1).split(",")) == PREVIEW_FAMILY_NAMES,
           f"--only documents {PREVIEW_FAMILY_NAMES} and nothing else "
           f"(got {match.group(1) if match else None!r})")


def test_a_failing_scenario_is_reported_not_swallowed() -> None:
    print("preview: one failing scenario fails the run and the rest still ran")
    victim = all_scenarios()[3][1]
    with scratch_logs(), stubbed_preview_run(failing={victim}) as launched:
        code, printed, _ = run_facade([])
    expect(code == 1, f"a run with one failed scenario exits 1 (got {code})")
    expect(len(launched) == len(all_scenarios()),
           "every scenario still ran — a failure does not short-circuit")
    expect("FAIL: --preview real-boot browser — see failures above" in printed,
           f"the aggregate line reports the failure: {printed!r}")


def test_the_report_names_every_log_against_its_phase() -> None:
    print("preview: the closing summary names every log against its phase")
    with scratch_logs():
        logs = preview_harness.LOGS
        phases = ["1. icons list", "3. units/acolyte", "9. sweep icons"]
        allocated = [logs.allocate(phase) for phase in phases]
        buffer = io.StringIO()
        with contextlib.redirect_stdout(buffer):
            logs.report()
        printed = buffer.getvalue()
        expect(all(path in printed for path in allocated),
               "every allocated path appears in the summary")
        for phase, path in zip(phases, allocated):
            expect(re.search(rf"{re.escape(phase)}: {re.escape(path)}", printed)
                   is not None,
                   f"the summary pairs {phase!r} with its own path")


def test_the_report_survives_a_run_that_dies_mid_boot() -> None:
    print("preview: the summary still prints when a boot exits the probe")
    # No scenario is stubbed here: the REAL first scenario runs up to the
    # shared launcher, which allocates its log and then hands off to a
    # `probelib.boot` that dies the way a real one does before READY.
    original_boot = preview_harness.boot

    def dies(_port, log=None, **_kw):
        raise SystemExit(f"preview engine exited before READY; see {log}")

    with scratch_logs():
        preview_harness.boot = dies
        try:
            code, printed, _ = run_facade([])
            allocated = list(preview_harness.LOGS._allocated)
        finally:
            preview_harness.boot = original_boot
    expect(code not in (0, None), f"the dead boot exits the probe (got {code})")
    expect(len(allocated) == 1
           and allocated[0][0] == "1. icons list",
           f"exactly the first scenario's log was allocated, under its "
           f"verbatim phase label (got {allocated})")
    expect(allocated and allocated[0][1] in printed,
           "the log allocated before the failure is still named on exit")
    expect("engine logs from this run (1 boot)" in printed,
           f"the summary reports the one launch attempted: {printed!r}")


def test_every_boot_across_all_family_owners_allocates_its_own_log() -> None:
    print("preview: every family owner launches only through the shared "
          "log-allocating launcher")
    owners = {family.name: Path(inspect.getsourcefile(family.module))
              for family in preview_probe.FAMILIES}
    owners["facade"] = TOOLS / "preview_probe.py"
    launcher_sites = 0
    for name, path in owners.items():
        source = path.read_text(encoding="utf-8")
        direct = re.findall(r"(?<![\w.])boot\(", source)
        expect(not direct,
               f"{name} ({path.name}) never calls probelib.boot directly "
               f"(found {len(direct)} call site(s))")
        expect(not re.search(r"^from probelib import[^\n]*\bboot\b",
                             source, re.M),
               f"{name} ({path.name}) does not import probelib.boot")
        launcher_sites += len(re.findall(r"boot_preview\(port,", source))
    scenarios = len(all_scenarios())
    expect(launcher_sites >= scenarios,
           f"the owners hold at least one shared-launcher call per scenario "
           f"(found {launcher_sites} for {scenarios} scenarios)")

    harness_source = Path(inspect.getsourcefile(preview_harness)) \
        .read_text(encoding="utf-8")
    call_sites = re.findall(r"(?<![\w.])boot\(port,[^)]*", harness_source)
    expect(len(call_sites) == 1,
           f"the harness holds the ONE probelib.boot call site "
           f"(found {len(call_sites)})")
    expect(all("log=LOGS.allocate(" in site for site in call_sites),
           f"that call site allocates its own log: {call_sites}")


# ---------------------------------------------------------------------------
# offscreen_probe: three engines, one of them restarting on a used port
# ---------------------------------------------------------------------------
@contextlib.contextmanager
def recorded_boots(fail_on: int | None = None):
    """Replace `boot`/`quit_engine` so the lifecycle runs with no engine.

    Each stubbed boot writes to its log the way a real one would, so a
    truncating collision would show up as lost content rather than as an
    equal-paths assertion alone.
    """
    original_boot = offscreen_probe.boot
    original_quit = offscreen_probe.quit_engine
    launched: list[tuple[int, str]] = []

    def fake_boot(port, log=None, **_kw):
        launched.append((port, log))
        if fail_on is not None and len(launched) == fail_on:
            raise SystemExit(f"offscreen engine exited before READY; see {log}")
        truncating_write(log, f"engine on port {port}, launch {len(launched)}")
        return object()

    offscreen_probe.boot = fake_boot
    offscreen_probe.quit_engine = lambda *_a, **_kw: None
    try:
        yield launched
    finally:
        offscreen_probe.boot = original_boot
        offscreen_probe.quit_engine = original_quit


def test_the_port_reusing_restart_keeps_the_first_session_log() -> None:
    print("offscreen: the restart no longer truncates the first engine's log")
    with scratch_logs():
        with recorded_boots() as launched:
            engines = offscreen_probe.Engines()
            # The real lifecycle: main engine, parallel second, stop the
            # second, stop the first, then restart on the FIRST one's port.
            engines.start(9418, phase="main session (menu, worldgen, gameplay)",
                          mode=("--offscreen",))
            engines.start(9419, phase="parallel second instance",
                          mode=("--offscreen",))
            engines.stop(9419)
            engines.stop(9418)
            engines.start(9418, phase="icon-reload restart",
                          mode=("--offscreen",))
            engines.stop(9418)
        ports = [port for port, _ in launched]
        paths = [path for _, path in launched]
        expect(ports == [9418, 9419, 9418],
               f"the three-engine lifecycle is unchanged, restart included "
               f"(got {ports})")
        expect(len(set(paths)) == 3,
               f"all three launches wrote to distinct paths (got {paths})")
        expect(Path(paths[0]).read_text() ==
               "engine on port 9418, launch 1",
               "the first session's log survives the restart on its port")
        expect(Path(paths[2]).read_text() ==
               "engine on port 9418, launch 3",
               "the restart's own log holds the restart's output")


def test_a_skipped_worldgen_run_reports_only_the_launches_it_made() -> None:
    print("offscreen: the summary counts launches made, not call sites")
    with scratch_logs():
        with recorded_boots():
            engines = offscreen_probe.Engines()
            engines.start(9418, phase="main session (menu, worldgen, gameplay)",
                          mode=("--offscreen",))
            engines.start(9419, phase="parallel second instance",
                          mode=("--offscreen",))
            buffer = io.StringIO()
            with contextlib.redirect_stdout(buffer):
                engines.report_logs()
        printed = buffer.getvalue()
        expect("2 boots" in printed,
               f"the summary reports two launches, not three: {printed!r}")
        expect("icon-reload restart" not in printed,
               "the conditional third launch is absent when it never ran")


def test_a_failed_offscreen_boot_is_still_reported() -> None:
    print("offscreen: a launch that dies before READY is still named")
    with scratch_logs():
        with recorded_boots(fail_on=2) as launched:
            engines = offscreen_probe.Engines()
            engines.start(9418, phase="main session (menu, worldgen, gameplay)",
                          mode=("--offscreen",))
            try:
                engines.start(9419, phase="parallel second instance",
                              mode=("--offscreen",))
            except SystemExit:
                pass
            buffer = io.StringIO()
            with contextlib.redirect_stdout(buffer):
                engines.report_logs()
        printed = buffer.getvalue()
        expect(launched[1][1] in printed,
               "the failed launch's log path is named in the summary")
        expect("parallel second instance" in printed,
               "and is named against the phase that tried to launch it")


def main() -> int:
    selftestlib.parse_verbose()
    test_a_repeated_phase_still_gets_its_own_log()
    test_the_inventory_names_every_scenario_exactly_once()
    test_the_default_run_invokes_every_family_once_in_order()
    test_each_selector_invokes_only_its_family()
    test_an_unknown_selector_invokes_nothing()
    test_the_help_documents_exactly_the_five_families()
    test_a_failing_scenario_is_reported_not_swallowed()
    test_the_report_names_every_log_against_its_phase()
    test_the_report_survives_a_run_that_dies_mid_boot()
    test_every_boot_across_all_family_owners_allocates_its_own_log()
    test_the_port_reusing_restart_keeps_the_first_session_log()
    test_a_skipped_worldgen_run_reports_only_the_launches_it_made()
    test_a_failed_offscreen_boot_is_still_reported()
    if FAILURES:
        print(f"\n{len(FAILURES)} test(s) failed:")
        for failure in FAILURES:
            print(f"  {failure}")
        return selftestlib.concluded(1)
    return selftestlib.concluded(0, "\nAll per-boot engine log tests passed")


if __name__ == "__main__":
    raise SystemExit(main())
