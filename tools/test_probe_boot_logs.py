#!/usr/bin/env python3
"""Unit tests for the per-boot engine logs of the two multi-boot probes (#1763).

`tools/preview_probe.py` and `tools/offscreen_probe.py` both launch several
fresh engines in one run, and `probelib.boot` opens its log truncating. When
two launches shared one path, every boot after the first destroyed its
predecessor's capture — preview kept only its last of ~22, and offscreen's
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
  * Every `boot(` in preview_probe.py allocates its own log. A new call
    site added without one would silently fall back to probelib's shared
    per-port default and collide again.

No engine, no world, no GPU: everything here runs against temporary files.

Usage:
  python3 tools/test_probe_boot_logs.py
Exit codes: 0 = all tests passed, 1 = one or more failed.
"""
from __future__ import annotations

import contextlib
import io
import re
import sys
import tempfile
from pathlib import Path

TOOLS = Path(__file__).resolve().parent
sys.path.insert(0, str(TOOLS))
import offscreen_probe  # type: ignore  # noqa: E402
import preview_probe  # type: ignore  # noqa: E402

import selftest  # noqa: E402
from selftest import FAILURES, expect  # noqa: E402


@contextlib.contextmanager
def scratch_logs():
    """A throwaway log directory for both probes' allocators."""
    with tempfile.TemporaryDirectory() as tmp:
        preview_original = preview_probe.LOGS
        offscreen_original = offscreen_probe.Engines.LOG_DIR
        preview_probe.LOGS = preview_probe.BootLogs(directory=tmp)
        offscreen_probe.Engines.LOG_DIR = tmp
        try:
            yield tmp
        finally:
            preview_probe.LOGS = preview_original
            offscreen_probe.Engines.LOG_DIR = offscreen_original


def truncating_write(path: str, text: str) -> None:
    """Write the way `probelib.boot` opens a log: mode 'w'."""
    with open(path, "w") as fh:
        fh.write(text)


# ---------------------------------------------------------------------------
# preview_probe: one log per boot across ~22 launches
# ---------------------------------------------------------------------------
def test_a_repeated_phase_still_gets_its_own_log() -> None:
    print("preview: a repeated phase gets its own log")
    with scratch_logs():
        logs = preview_probe.LOGS
        first = logs.allocate("9. sweep units/acolyte")
        second = logs.allocate("9. sweep units/acolyte")
        expect(first != second,
               "the same phase allocated twice yields two distinct paths")


def test_every_launch_of_a_full_run_keeps_its_own_capture() -> None:
    print("preview: a full run's launches each retain their own output")
    # The phases a complete run allocates: the fixed checks, one roster
    # boot per remaining shipped unit, and the sweep — whose `icons` and
    # `units/acolyte` targets repeat phases browsed earlier.
    phases = [
        "1. icons list",
        "2. icons/skill/climbing.png",
        "3. units/acolyte",
        "4. units/tiller",
        *[f"4b. units/{unit}" for unit in ("nomad", "beast", "wolf", "deer")],
        "5. buildings/acolyte_portal",
        "6. buildings/cargo_hold_S",
        "7. buildings/dungeon_1",
        "8. flora/berry_bush",
        "8. structures/wire",
        *[f"9. sweep {target}" for target in
          ("icons", "items", "ui", "world", "units/acolyte", "flora/berry_bush",
           "buildings/workbench", "structures/wire")],
    ]
    with scratch_logs():
        logs = preview_probe.LOGS
        allocated = [logs.allocate(phase) for phase in phases]
        expect(len(set(allocated)) == len(phases),
               f"{len(phases)} launches allocate {len(phases)} distinct paths "
               f"(got {len(set(allocated))})")
        for index, path in enumerate(allocated):
            truncating_write(path, f"boot {index} output")
        kept = [Path(path).read_text() for path in allocated]
        expect(kept == [f"boot {index} output" for index in range(len(phases))],
               "each log still holds the output of exactly its own boot")


def test_the_report_names_every_log_against_its_phase() -> None:
    print("preview: the closing summary names every log against its phase")
    with scratch_logs():
        logs = preview_probe.LOGS
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
    with scratch_logs():
        logs = preview_probe.LOGS
        path = logs.allocate("1. icons list")
        original = preview_probe.check_simple_list_mode
        original_argv = sys.argv

        def dies(_port):
            raise SystemExit("preview engine exited before READY")

        preview_probe.check_simple_list_mode = dies
        sys.argv = ["preview_probe.py"]
        buffer = io.StringIO()
        try:
            with contextlib.redirect_stdout(buffer):
                try:
                    preview_probe.main()
                except SystemExit:
                    pass
        finally:
            preview_probe.check_simple_list_mode = original
            sys.argv = original_argv
        expect(path in buffer.getvalue(),
               "the log allocated before the failure is still named on exit")


def test_no_preview_boot_falls_back_to_the_shared_default() -> None:
    print("preview: every boot call site allocates its own log")
    source = (TOOLS / "preview_probe.py").read_text(encoding="utf-8")
    call_sites = re.findall(r"boot\(port,[^)]*", source)
    expect(len(call_sites) >= 10,
           f"the source still holds the probe's boot call sites "
           f"(found {len(call_sites)})")
    without_allocation = [site for site in call_sites
                          if "log=LOGS.allocate(" not in site]
    expect(not without_allocation,
           f"no boot call site omits its own log allocation: "
           f"{without_allocation}")


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
    selftest.parse_verbose()
    test_a_repeated_phase_still_gets_its_own_log()
    test_every_launch_of_a_full_run_keeps_its_own_capture()
    test_the_report_names_every_log_against_its_phase()
    test_the_report_survives_a_run_that_dies_mid_boot()
    test_no_preview_boot_falls_back_to_the_shared_default()
    test_the_port_reusing_restart_keeps_the_first_session_log()
    test_a_skipped_worldgen_run_reports_only_the_launches_it_made()
    test_a_failed_offscreen_boot_is_still_reported()
    if FAILURES:
        print(f"\n{len(FAILURES)} test(s) failed:")
        for failure in FAILURES:
            print(f"  {failure}")
        return selftest.concluded(1)
    return selftest.concluded(0, "\nAll per-boot engine log tests passed")


if __name__ == "__main__":
    raise SystemExit(main())
