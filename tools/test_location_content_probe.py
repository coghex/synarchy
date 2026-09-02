#!/usr/bin/env python3
"""Artifact and scenario ownership for the location-content probe
(issues #1884, #2095).

`tools/location_content_probe.py` is manual-only. It boots engines from
seven `boot_isolated` CALL SITES, one of which runs twice -- once
visiting the ruins in the same order, once in the exact reverse -- so an
observable run LAUNCHES eight engine processes across several generated
worlds, and its own acceptance can only be seen by a run nothing in CI
can make. The contract this file pins is the half that is pure Python
and would otherwise regress silently: every file one invocation creates
lives under ONE directory that invocation owns, and the whole tree goes
away again on every handled exit — unless `--keep-artifacts` says
otherwise.

Since #2095 the scenario assertions live in owners under
`tools/location_content/` and the probe file is the façade over them, so
every structural check below scans the COMPLETE reorganized surface --
the façade plus every module it imports from that package -- and asserts
its own non-vacuity first. Rooted at the façade alone, the
exclusion-style properties ("no bare `boot`", "no raw fixture `send`",
"every log read is this invocation's") would all evaluate True over an
empty node set: they would report OK while inspecting nothing.

Before #1884 the probe's five fixture YAMLs and its engine log were the
fixed, process-global names `/tmp/loc_content_probe_bogus.yaml`,
`/tmp/loc_content_probe_bogus_loot.yaml`,
`/tmp/loc_content_probe_quinoa.yaml`,
`/tmp/loc_content_probe_quinoa_loot.yaml`,
`/tmp/loc_content_probe_dense.yaml` and
`/tmp/location_content_engine.log`. Each was written with a truncating
`open(..., "w")` (`probelib.boot` opens the log the same way), none
carried a PID, port or any other invocation identity, and nothing
removed any of them. #1620 had already moved the SAVE slots into an
invocation-owned resource root and explicitly left these six behind.

Two concurrent runs — a supported mode: `run_probes.py --jobs N`, and
`probe_flake.py`'s machine-wide port lease — collided on all six. The
log collision is the sharp one: the probe ASSERTS against that log
twice (the integrity diagnostic in phase 2, the two unknown-content
warnings in phase 3), so a foreign truncation could turn a passing phase
into a failure or a failure into a pass.

The properties asserted directly, because each is a way the probe would
leak, collide, or stop proving what it claims:

  * Two invocations share no path — none of the five fixtures, not the
    log, not the root — so the fixed logical names inside each tree are
    safe. Every one of those paths is absolute and inside the run's own
    directory, because the engine is chdir'd into the isolated resource
    root (`App.ResourceRoot`) and resolves a relative path against it.
  * No artifact keeps a legacy fixed `/tmp` name, and a real run leaves
    all six of them exactly as it found them — absent if they were
    absent, byte-identical if a developer has one. Nor is any same-named
    file elsewhere opened, truncated or removed.
  * The tree is released after a pass, an early return, an exception, a
    `probelib.boot` abort, a `_PhaseAborted` and a handled Ctrl-C.
  * Every engine the run LAUNCHED is dead before any of that is removed,
    and the disposal is a direct kill rather than an `engine.quit()`
    aimed at whoever holds the port.
  * Every boot goes through the one funnel that hands it this
    invocation's log and registers the process as it is launched, and
    both log-reading ASSERTIONS read that same log.
  * Only the façade boots at all, from exactly seven call sites, and the
    regeneration site is still a loop over the two visit orders -- so the
    run still LAUNCHES eight processes. A call-site count alone would
    accept that loop being unrolled, flattened to one case, or grown to
    three, each of which changes the process count.
  * The façade still offers exactly one `run(args, art, token)` for
    `main` to call, with that parameter order: substituting it is the
    sole mechanism behind eight of the lifecycle tests below.
  * The reorganized surface is complete -- the façade imports every
    extracted module, and every scan runs over all of them.
  * Every PASS diagnostic and every recorded failure belongs to a
    scenario owner rather than the façade or the shared infrastructure,
    and the counts are the pre-split file's exactly.
  * No scenario owner keeps cross-scenario state in a mutable module
    global; the values `run` used to accumulate across phases are
    fields of the one handoff record the façade threads.
  * Retention is opt-in, keeps the run's own result, names where the
    artifacts are, and describes what the run ACTUALLY produced rather
    than what a finished run usually would.
  * A default failing run says its log went with the tree and points at
    the flag, instead of leaving the operator chasing a deleted path.
  * Cleanup that cannot finish makes an otherwise passing run non-zero,
    through #1620's own `remove_isolated_root` reporting.
  * Fixture registration ORDER and CONTENTS are unchanged — bogus
    location, bogus loot, quinoa location, quinoa loot for phase 3, then
    `dense` alone for phase 4 — because placement and loot draws are
    order- and content-sensitive; and every one still goes through
    `load_fixture_yaml`, so a fixture that registers nothing still stops
    the probe at setup (#1342).
  * The public helpers other probes import — `make_isolated_root`,
    `remove_isolated_root`, `save_and_wait` — are still the SAME
    function objects, with the shapes `tools/portal_ghost_probe.py` and
    `tools/test_location_probe_config_isolation.py` depend on. Identity,
    not merely a name that resolves: a delegating wrapper would satisfy
    `hasattr` and break both.

No engine, no world, no worldgen, no GPU: every test here runs against
temporary directories in about a second.

Usage:
  python3 tools/test_location_content_probe.py
Exit codes: 0 = all tests passed, 1 = one or more failed.
"""
from __future__ import annotations

import ast
import contextlib
import hashlib
import importlib
import inspect
import io
import os
import shutil
import sys
import tempfile
from pathlib import Path

TOOLS = Path(__file__).resolve().parent
sys.path.insert(0, str(TOOLS))
import probelib  # type: ignore  # noqa: E402
import location_content_probe as probe  # type: ignore  # noqa: E402
import portal_ghost_probe as portal  # type: ignore  # noqa: E402
from location_content import invocation  # type: ignore  # noqa: E402

import selftestlib  # noqa: E402
from selftestlib import FAILURES, expect  # noqa: E402

#: The six process-global names the probe used before #1884. Nothing it
#: writes may resolve to one of them again, and a real run must leave
#: each exactly as it found it.
LEGACY_PATHS = (
    "/tmp/loc_content_probe_bogus.yaml",
    "/tmp/loc_content_probe_bogus_loot.yaml",
    "/tmp/loc_content_probe_quinoa.yaml",
    "/tmp/loc_content_probe_quinoa_loot.yaml",
    "/tmp/loc_content_probe_dense.yaml",
    "/tmp/location_content_engine.log",
)

#: The five fixtures, by the logical name `RunArtifacts.fixture` is
#: asked for, in the order the probe REGISTERS them: phase 3's four
#: (bogus location, bogus loot, quinoa location, quinoa loot) and then
#: phase 4's `dense` alone.
FIXTURE_NAMES = ("bogus", "bogus_loot", "quinoa", "quinoa_loot", "dense")

#: The loader each of those is registered through, in the same order.
FIXTURE_LOADERS = (
    "engine.loadLocationYaml",
    "engine.loadLootTableYaml",
    "engine.loadLocationYaml",
    "engine.loadLootTableYaml",
    "engine.loadLocationYaml",
)

#: The package the scenario owners live in (#2095).
PACKAGE = TOOLS / "location_content"

#: Every module the reorganization created, from the FILESYSTEM. Ground
#: truth: a scan built from this can never resolve to fewer files than
#: exist, which is the way a post-split structural check would otherwise
#: go quietly vacuous.
EXTRACTED = tuple(sorted(path for path in PACKAGE.glob("*.py")
                         if path.name != "__init__.py"))

#: The complete reorganized surface every structural scan below runs
#: over: the façade plus the package, `__init__` included.
SURFACE = (Path(probe.__file__).resolve(),
           PACKAGE / "__init__.py", *EXTRACTED)

#: The scenario owners, as distinct from the shared infrastructure the
#: façade also imports. Named because the checks that say WHERE a
#: contract lives need both halves of the distinction.
SCENARIO_OWNERS = ("content", "dispatch", "knowledge", "naming")
INFRASTRUCTURE = ("engine_queries", "invocation")

#: #2095 requirement 11 and the acceptance's process count. Seven
#: `boot_isolated` call sites, one of them inside a two-element loop over
#: the visit orders, so a run launches eight engine processes.
BOOT_CALL_SITES = 7
PROCESS_LAUNCHES = 8

#: The whole surface's diagnostic totals, recounted across every owner.
#: Moving an assertion between owners is a visible edit here; losing
#: one, or duplicating one, is a failure. The pre-split file's own
#: totals were 45/67; #917's `check_significant_contents` added the six
#: PASS lines and eight failure records of the guaranteed-contents and
#: compound-clearance scenario.
TOTAL_PASS_DIAGNOSTICS = 51
TOTAL_FAILURE_RECORDS = 75

#: The values `run` used to accumulate in local variables across its
#: phases (#2095's cross-scenario handoff). Each is now a field of the
#: one record the façade threads between owners; `failures` stays the
#: list the façade owns and passes to every one of them.
HANDOFF_FIELDS = (
    "placed_all", "ruins", "counts1", "geoms1", "loot1", "r0mem_key",
    "mem_uids", "dangling_uid", "sibling_keys", "saved_content",
    "saved_naming", "named",
)


def module_source(path: Path) -> str:
    return path.read_text(encoding="utf-8")


#: Parsed ONCE. Two scans that want to talk about the same node -- "is
#: this `boot(...)` the funnel's own?", "is this call site inside the
#: regeneration loop?" -- can only compare node identity if they read
#: the same trees, and a fresh `ast.parse` per call silently answers no
#: to every such question.
_SURFACE_TREES = [(path, ast.parse(module_source(path)))
                  for path in SURFACE]


def surface_trees() -> list[tuple[Path, ast.Module]]:
    """(path, parsed module) for every file on the reorganized surface.

    Every scan below goes through this rather than through one module's
    `run`, and then asserts its own non-vacuity: an exclusion-style
    property ("no bare boot", "no raw send") is True over an empty node
    set, so a scan that stopped seeing the code would report OK.
    """
    return _SURFACE_TREES


def facade_tree() -> ast.Module:
    """The façade's own parsed module, from the shared cache — so a node
    found here is the SAME object `surface_calls` returns."""
    facade = Path(probe.__file__).resolve()
    return next(tree for path, tree in surface_trees() if path == facade)


def facade_package_imports() -> tuple[str, ...]:
    """The package modules the FAÇADE itself imports, derived from its
    own source rather than a list maintained here."""
    names: set[str] = set()
    for node in ast.walk(facade_tree()):
        if isinstance(node, ast.ImportFrom) and node.module:
            parts = node.module.split(".")
            if parts[0] != PACKAGE.name:
                continue
            if len(parts) > 1:
                names.add(parts[1])
            else:
                names.update(alias.name for alias in node.names)
        elif isinstance(node, ast.Import):
            for alias in node.names:
                parts = alias.name.split(".")
                if parts[0] == PACKAGE.name and len(parts) > 1:
                    names.add(parts[1])
    return tuple(sorted(names))


def surface_calls(name: str, *, attribute: bool = False):
    """Every `name(...)` (or `.name(...)`) call across the surface, in a
    deterministic (file, line, column) order."""
    found = []
    for path, tree in surface_trees():
        for node in ast.walk(tree):
            if not isinstance(node, ast.Call):
                continue
            func = node.func
            if attribute:
                match = isinstance(func, ast.Attribute) and func.attr == name
            else:
                match = isinstance(func, ast.Name) and func.id == name
            if match:
                found.append((path, node))
    found.sort(key=lambda pair: (str(pair[0]), pair[1].lineno,
                                 pair[1].col_offset))
    return found


def pass_diagnostic_text(node: ast.Call) -> str:
    """The literal head of a `print(...)` argument, whichever of the
    three spellings the probe uses (plain, f-string, or an f-string
    joined to a computed tail)."""
    if not (isinstance(node.func, ast.Name) and node.func.id == "print"
            and node.args):
        return ""
    first = node.args[0]
    if isinstance(first, ast.BinOp):
        first = first.left
    if isinstance(first, ast.Constant) and isinstance(first.value, str):
        return first.value
    if isinstance(first, ast.JoinedStr):
        return "".join(part.value for part in first.values
                       if isinstance(part, ast.Constant)
                       and isinstance(part.value, str))
    return ""


#: The fixture bodies are load-bearing content, not scaffolding: which
#: content ids they name is what phase 3's unknown-id checks read back,
#: the single-entry loot tables are what make a specific item spawn
#: whatever the draw selects, and the fixed `position` is what phase 3
#: asserts to the exact tile. Pinning the bytes makes an edit a
#: deliberate, visible act rather than a silent change to what the probe
#: proves. These digests are of the bodies as they stood before #1884
#: moved WHERE they are written.
FIXTURE_DIGESTS = {
    "BOGUS_LOCATION_YAML":
        "407ebb91d16a57c874dee8d9395d620ccc6f640f4b067a2be7570c1a1357d8f9",
    "BOGUS_LOOT_YAML":
        "9589056a6a07c51f0096047050be628f1cec417c547f4a0138bfb983e7d908d9",
    "QUINOA_LOCATION_YAML":
        "1ca6a930511393416f545ac954cefe8eff3df3e044fa6ce0ffbaceb02c5b5dc7",
    "QUINOA_LOOT_YAML":
        "09bc563d2e3daf2c7fbfadca995ef164e0af9ff7d91d145120659d0f76a7bf5a",
    "DENSE_LOCATION_YAML":
        "3e0fc0dbd0b9abf46ba05f85c00b0446b39799393aec0179c520d811226104d0",
}


def fixture_definitions(name: str) -> list[Path]:
    """Every module on the surface that DEFINES `name` at module level.

    Requirement 7 is single-sourcing, not availability: a re-export
    satisfies `getattr` while a second copy quietly drifts, so the digest
    audit proves there is exactly one definition before it hashes it.
    """
    found = []
    for path, tree in surface_trees():
        for node in tree.body:
            targets = []
            if isinstance(node, ast.Assign):
                targets = node.targets
            elif isinstance(node, ast.AnnAssign):
                targets = [node.target]
            if any(isinstance(target, ast.Name) and target.id == name
                   for target in targets):
                found.append(path)
    return found


def fixture_body(name: str) -> str:
    """The fixture constant `name`, resolved at whichever module owns it.

    FAILS rather than skipping or defaulting when no module on the
    surface defines it: a constant that moved to a module this test does
    not read is the case the digest audit exists to catch, and answering
    "unchanged" for a body it never found would be the worst possible
    reading.
    """
    owners = fixture_definitions(name)
    if len(owners) != 1:
        raise AssertionError(
            f"{name} must be defined exactly once across the reorganized "
            f"surface, found {[path.name for path in owners]}")
    module = importlib.import_module(f"{PACKAGE.name}.{owners[0].stem}")
    return getattr(module, name)


@contextlib.contextmanager
def fresh_run():
    """A built `RunArtifacts` on a temporary base, always cleaned up
    afterwards however the test leaves it — including when the test is
    about a removal that deliberately did not happen."""
    art = probe.RunArtifacts(tempfile.mkdtemp(prefix="test_loc_content_"))
    try:
        art.build()
        yield art
    finally:
        shutil.rmtree(art.base, ignore_errors=True)


def run_main(argv: list[str], body) -> tuple[int | None, BaseException | None,
                                             str, str | None]:
    """Drive `probe.main()` with `run` replaced by `body`, and report
    `(exit code, what propagated, merged output, the base the run
    owned)`.

    Substituting `run` is what lets these tests exercise the guard's
    real paths — an early return, an exception, `probelib.boot`'s
    `SystemExit` abort, a `_PhaseAborted`, a Ctrl-C — without booting an
    engine. Both streams are merged deliberately: what matters is what
    the operator sees, and the probe prints its checks on stdout while
    its failures and a propagating exception land on stderr.

    `code` is None when nothing was returned, which is itself the
    assertion for the cases that propagate: `raise SystemExit(main())`
    is never reached, so the interpreter exits non-zero on the
    exception.
    """
    seen: dict[str, object] = {}

    def wrapper(args, art, token):
        seen["base"] = art.base
        seen["art"] = art
        seen["token"] = token
        return body(art)

    original_run, original_argv = probe.run, sys.argv
    probe.run = wrapper
    sys.argv = ["location_content_probe.py", *argv]
    out = io.StringIO()
    code: int | None = None
    raised: BaseException | None = None
    try:
        with contextlib.redirect_stdout(out), contextlib.redirect_stderr(out):
            code = probe.main()
    except BaseException as exc:  # noqa: BLE001 - several cases are about this
        raised = exc
    finally:
        probe.run, sys.argv = original_run, original_argv
    return code, raised, out.getvalue(), seen.get("base")  # type: ignore[return-value]


# ---------------------------------------------------------------------
# Invocation-unique paths
# ---------------------------------------------------------------------
def test_two_invocations_share_no_path() -> None:
    print("\ntest_two_invocations_share_no_path")
    with fresh_run() as first, fresh_run() as second:
        expect(first.base != second.base,
               "two invocations own two directories")
        for name in FIXTURE_NAMES:
            expect(first.fixture(name) != second.fixture(name),
                   f"the {name} fixture resolves to disjoint paths, so one "
                   f"run cannot overwrite the other's between its write and "
                   f"the engine-side read")
        expect(first.engine_log != second.engine_log,
               "two concurrent runs cannot truncate one another's engine "
               "log — which two checks ASSERT against")
        expect(first.root != second.root,
               "and each keeps its own resource root, so its save slots too")


def test_every_fixture_path_is_absolute_and_owned() -> None:
    print("\ntest_every_fixture_path_is_absolute_and_owned")
    # The paths are handed to the ENGINE, which has chdir'd into the
    # isolated resource root, so a relative one would resolve somewhere
    # else entirely (#1884 requirement 8).
    with fresh_run() as art:
        for name in FIXTURE_NAMES:
            path = art.fixture(name)
            expect(os.path.isabs(path), f"the {name} fixture path is absolute")
            expect(path.startswith(art.fixtures + os.sep),
                   f"...and lands in the run's own fixtures directory")
        expect(os.path.isabs(art.engine_log)
               and art.engine_log.startswith(art.logs + os.sep),
               "the engine log does too")
        expect(art.root.startswith(art.base + os.sep)
               and art.logs.startswith(art.base + os.sep)
               and art.fixtures.startswith(art.base + os.sep),
               "and all three live under the one directory the invocation "
               "owns, so removing it removes them")

    # …and the five names really are the five the probe asks for, over
    # the WHOLE surface: a sixth fixture that skipped `RunArtifacts`, or
    # one asked for from a module this scan does not read, would not be
    # covered by any of the above.
    calls = surface_calls("fixture", attribute=True)
    expect(calls, "the surface really contains `art.fixture(...)` calls — "
                  "an empty scan would make the order check below vacuous")
    owners = {path for path, _ in calls}
    expect(len(owners) == 1,
           f"all five fixtures are asked for by ONE owner, so their source "
           f"order is their registration order (got "
           f"{sorted(path.name for path in owners)})")
    fixtures = [node.args[0].value for _path, node in calls
                if node.args and isinstance(node.args[0], ast.Constant)]
    expect(tuple(fixtures) == FIXTURE_NAMES,
           f"the probe asks for exactly these five fixtures, in this order "
           f"(got {fixtures})")


def test_no_artifact_keeps_a_legacy_fixed_tmp_name() -> None:
    print("\ntest_no_artifact_keeps_a_legacy_fixed_tmp_name")
    with fresh_run() as art:
        chosen = {art.fixture(name) for name in FIXTURE_NAMES}
        chosen.add(art.engine_log)
        for legacy in LEGACY_PATHS:
            expect(legacy not in chosen,
                   f"nothing this run writes resolves to {legacy}")
        expect(os.path.realpath(art.base) != os.path.realpath("/tmp"),
               "the run's own directory is not /tmp itself")
    # Over the WHOLE surface (#2095): rooted at the façade alone this
    # would pass while a `/tmp` literal sat in an extracted module.
    scanned = surface_trees()
    expect(len(scanned) > 1,
           f"the scan covers the façade AND the extracted modules "
           f"(got {len(scanned)} file(s))")
    literals = [(path.name, node.value)
                for path, tree in scanned
                for node in ast.walk(tree)
                if isinstance(node, ast.Constant)
                and isinstance(node.value, str)
                and node.value.startswith("/tmp/")]
    expect(not literals,
           f"no /tmp path literal is left anywhere on the reorganized "
           f"surface (got {literals})")


def test_a_real_run_leaves_every_legacy_path_as_it_found_it() -> None:
    print("\ntest_a_real_run_leaves_every_legacy_path_as_it_found_it")
    # Non-destructive by construction: whatever the six legacy paths
    # hold on this machine — nothing, or a developer's own files — is
    # recorded and compared, never created or removed here. Opening one
    # `"w"` is the quiet failure #1884 names, so the bytes are what is
    # compared, not merely existence.
    def snapshot() -> dict[str, bytes | None]:
        state: dict[str, bytes | None] = {}
        for path in LEGACY_PATHS:
            try:
                with open(path, "rb") as handle:
                    state[path] = handle.read()
            except OSError:
                state[path] = None
        return state

    before = snapshot()
    code, raised, _text, base = run_main([], lambda art: 0)
    after = snapshot()
    expect(code == 0 and raised is None,
           f"the run itself finished cleanly (code {code}, {raised!r})")
    expect(base is not None and not os.path.exists(base),
           "and released its own tree")
    for path in LEGACY_PATHS:
        expect(before[path] == after[path],
               f"{path} is exactly as the run found it "
               f"({'absent' if before[path] is None else 'byte-identical'})")


def test_release_never_touches_what_the_run_did_not_create() -> None:
    print("\ntest_release_never_touches_what_the_run_did_not_create")
    outside = tempfile.mkdtemp(prefix="test_loc_content_decoy_")
    try:
        decoys = {}
        for name in (*(f"{n}.yaml" for n in FIXTURE_NAMES), "engine.log"):
            path = os.path.join(outside, name)
            payload = f"a developer's {name}, not the probe's".encode()
            with open(path, "wb") as handle:
                handle.write(payload)
            decoys[path] = payload
        code, raised, _text, base = run_main([], lambda art: 0)
        expect(code == 0 and raised is None,
               f"the run itself finished cleanly (code {code}, {raised!r})")
        expect(base is not None and not os.path.exists(base),
               "and released its own tree")
        for path, payload in decoys.items():
            expect(os.path.isfile(path),
                   f"the same-named {os.path.basename(path)} outside the run "
                   f"is still there")
            with open(path, "rb") as handle:
                expect(handle.read() == payload, "...and byte-identical")
    finally:
        shutil.rmtree(outside, ignore_errors=True)


def test_release_does_not_follow_the_content_symlinks() -> None:
    print("\ntest_release_does_not_follow_the_content_symlinks")
    with fresh_run() as art:
        before = sorted(os.listdir(os.path.join(probe.REPO, "scripts")))
        expect(probe.release_artifacts(art, keep=False) is None,
               "a clean removal reports no leftover")
        expect(not os.path.exists(art.base), "the run's own tree is gone")
        expect(sorted(os.listdir(os.path.join(probe.REPO, "scripts"))) == before,
               "the real scripts/ is untouched — rmtree unlinked the symlink")


# ---------------------------------------------------------------------
# The guard around the whole run
# ---------------------------------------------------------------------
def test_a_passing_run_leaves_nothing() -> None:
    print("\ntest_a_passing_run_leaves_nothing")
    code, raised, text, base = run_main([], lambda art: 0)
    expect(code == 0 and raised is None,
           f"a run whose checks all passed exits 0 (got {code}, {raised!r})")
    expect(base is not None and not os.path.exists(base),
           "a passing run leaves no artifact directory")
    expect(base is not None and base not in text,
           "and its summary does not point at a directory that was deleted")
    expect("retained this run's artifacts" not in text,
           "...and does not claim to have retained anything")
    expect("--keep-artifacts" not in text,
           "a passing run does not offer the diagnostic flag it did not need")


def test_a_failing_run_still_releases_and_points_at_the_flag() -> None:
    print("\ntest_a_failing_run_still_releases_and_points_at_the_flag")
    code, raised, text, base = run_main([], lambda art: 1)
    expect(code == 1 and raised is None,
           f"a run with failures exits non-zero (got {code}, {raised!r})")
    expect(base is not None and not os.path.exists(base),
           "a failing run leaves no artifact directory either")
    expect("re-run with --keep-artifacts" in text,
           "and tells the operator how to keep the log it just deleted")


def test_an_early_return_still_releases() -> None:
    print("\ntest_an_early_return_still_releases")

    def early_return(art):
        # What the probe does when a phase gives up: some fixtures
        # written, most phases never run.
        with open(art.fixture("bogus"), "w") as handle:
            handle.write(fixture_body("BOGUS_LOCATION_YAML"))
        return 1

    code, raised, _text, base = run_main([], early_return)
    expect(code == 1 and raised is None,
           f"an early return exits non-zero (got {code}, {raised!r})")
    expect(base is not None and not os.path.exists(base),
           "an early return leaves no fixture behind")


def test_an_exception_mid_run_still_releases() -> None:
    print("\ntest_an_exception_mid_run_still_releases")

    def blow_up(art):
        raise RuntimeError("kaboom")

    code, raised, _text, base = run_main([], blow_up)
    expect(isinstance(raised, RuntimeError) and code is None,
           f"an unexpected exception propagates with its traceback, so "
           f"`raise SystemExit(main())` never runs and the interpreter "
           f"exits non-zero (got {code}, {raised!r})")
    expect(base is not None and not os.path.exists(base),
           "an unexpected exception leaves no artifact directory")


def test_a_phase_abort_still_releases() -> None:
    print("\ntest_a_phase_abort_still_releases")

    def abort_phase(art):
        # `_PhaseAborted` is caught by the two phases that raise it, but
        # the guard must hold for one that escapes anyway.
        raise probe._PhaseAborted()

    code, raised, _text, base = run_main([], abort_phase)
    expect(isinstance(raised, probe._PhaseAborted) and code is None,
           f"a _PhaseAborted still ends the run (got {code}, {raised!r})")
    expect(base is not None and not os.path.exists(base),
           "a _PhaseAborted leaves no artifact directory")


def test_a_boot_abort_still_releases_and_points_at_the_flag() -> None:
    print("\ntest_a_boot_abort_still_releases_and_points_at_the_flag")

    def abort(art):
        # How `probelib.boot` ends a run whose engine died before READY.
        # Its message names the log path verbatim — the path the release
        # below is about to delete.
        raise SystemExit(f"engine exited before READY; see {art.engine_log}")

    code, raised, text, base = run_main([], abort)
    expect(code == 1 and raised is None,
           f"a boot abort exits non-zero (got {code}, {raised!r})")
    expect(base is not None and not os.path.exists(base),
           "a boot abort leaves no artifact directory")
    expect("aborted before finishing" in text,
           "the summary reports the abort rather than swallowing it")
    expect("re-run with --keep-artifacts" in text,
           "and tells the operator how to keep the log it just named")


def test_a_keyboard_interrupt_still_releases() -> None:
    print("\ntest_a_keyboard_interrupt_still_releases")

    def interrupt(art):
        raise KeyboardInterrupt()

    code, raised, _text, base = run_main([], interrupt)
    expect(isinstance(raised, KeyboardInterrupt) and code is None,
           f"a handled Ctrl-C still ends the run as an interrupt "
           f"(got {code}, {raised!r})")
    expect(base is not None and not os.path.exists(base),
           "a handled Ctrl-C leaves no artifact directory")


def test_cleanup_failure_fails_an_otherwise_clean_run() -> None:
    print("\ntest_cleanup_failure_fails_an_otherwise_clean_run")
    # #1620 requirement 6's reporting, now covering the fixtures and the
    # log as well as the save slots: a green result sitting beside
    # leftover artifacts is precisely what this isolation exists to
    # prevent, so it must not be reported as a pass.
    for broken, phrase in (("noop", "still exists after removal"),
                           ("raises", "could not remove")):
        real_rmtree = shutil.rmtree
        if broken == "noop":
            shutil.rmtree = lambda *a, **k: None
        else:
            def refuse(*_a, **_k):
                raise OSError("permission denied")

            shutil.rmtree = refuse
        try:
            code, raised, text, base = run_main([], lambda art: 0)
        finally:
            shutil.rmtree = real_rmtree
        try:
            expect(code == 1 and raised is None,
                   f"[{broken}] a run that cannot remove its own tree does "
                   f"not report a pass (got {code}, {raised!r})")
            expect(phrase in text and base is not None and base in text,
                   f"[{broken}] the summary identifies the residue "
                   f"('{phrase}')")
            expect(base is not None and os.path.exists(base),
                   f"[{broken}] the refused removal really did leave the "
                   f"tree, so the check above is not vacuous")
        finally:
            if base:
                real_rmtree(base, ignore_errors=True)


# ---------------------------------------------------------------------
# Engines this run launched
# ---------------------------------------------------------------------
class _StandInEngine:
    """Stands in for the `subprocess.Popen` `probelib.boot` launches, so
    a teardown assertion can name the exact handle that was disposed of
    and say HOW."""

    pid = 4242

    def __init__(self, alive: bool = True) -> None:
        self.alive = alive
        self.killed = False

    def poll(self):
        return None if self.alive else 0

    def kill(self) -> None:
        self.killed = True
        self.alive = False

    def wait(self, timeout=None) -> int:
        return 0

    def __repr__(self) -> str:  # pragma: no cover - diagnostics only
        return f"<stand-in engine alive={self.alive} killed={self.killed}>"


def test_every_boot_is_handed_this_runs_log_and_root() -> None:
    print("\ntest_every_boot_is_handed_this_runs_log_and_root")
    seen: dict = {}
    launched = _StandInEngine()

    def stub_boot(port, log=None, args=None, on_launch=None, **kwargs):
        seen.update(port=port, log=log, args=args)
        if on_launch is not None:
            on_launch(launched)
        return launched

    with fresh_run() as art:
        # The funnel lives in the invocation module since #2095, and
        # `boot` is looked up in ITS globals — patching the façade's
        # re-export would leave the real `probelib.boot` in the path.
        original = invocation.boot
        invocation.boot = stub_boot
        try:
            invocation.boot_isolated(9190, art)
        finally:
            invocation.boot = original
        expect(seen.get("log") == art.engine_log,
               f"the boot writes into this invocation's own log "
               f"(got {seen.get('log')!r})")
        expect(seen.get("args") == ["--resource-root", art.root],
               f"...and into this invocation's own resource root "
               f"(got {seen.get('args')!r})")
        expect(art.launched == [launched],
               "and the process is registered as it is LAUNCHED, so the "
               "span boot spends waiting for READY is covered too")

    # Every boot on the whole surface really does go through that one
    # funnel: a bare `boot(...)` would pick its own log and register
    # nothing, and a scenario owner booting for itself would sit outside
    # the shared lifecycle entirely (#2095 requirement 3).
    funnel_nodes: set[int] = set()
    for _path, tree in surface_trees():
        for node in ast.walk(tree):
            if isinstance(node, ast.FunctionDef) and node.name == "boot_isolated":
                funnel_nodes.update(id(inner) for inner in ast.walk(node))
    bare = [(path, node) for path, node in surface_calls("boot")
            if id(node) not in funnel_nodes]
    excluded = [pair for pair in surface_calls("boot")
                if id(pair[1]) in funnel_nodes]
    expect(len(excluded) == 1,
           f"the funnel's own single `boot(...)` is what was excluded, so "
           f"the check below is not hiding a renamed funnel (got "
           f"{len(excluded)})")
    funnelled = surface_calls("boot_isolated")
    expect(not bare,
           f"nothing on the surface boots outside boot_isolated (got "
           f"{[(path.name, node.lineno) for path, node in bare]})")
    expect(len(funnelled) == BOOT_CALL_SITES,
           f"all {BOOT_CALL_SITES} boot call sites go through it "
           f"(got {len(funnelled)})")
    bootstrappers = {path for path, _ in funnelled}
    expect(bootstrappers == {Path(probe.__file__).resolve()},
           f"...and every one of them is the façade's, so no scenario "
           f"owner boots an engine of its own (got "
           f"{sorted(path.name for path in bootstrappers)})")


def test_launched_engines_are_dead_before_anything_is_removed() -> None:
    print("\ntest_launched_engines_are_dead_before_anything_is_removed")
    # `probelib.boot` hands the handle over the statement after its
    # `Popen` and only decides about READY up to three minutes later, and
    # `quit_engine` is itself interruptible at every step — so a run can
    # reach teardown with a live engine still writing into the tree that
    # is about to be deleted.
    order: list[str] = []
    stranded = _StandInEngine()
    real_rmtree = shutil.rmtree

    def watched_rmtree(*a, **k):
        order.append("rmtree")
        return real_rmtree(*a, **k)

    def strand(art):
        art.launched.append(stranded)
        raise KeyboardInterrupt()

    original_kill = _StandInEngine.kill

    def watched_kill(self):
        order.append("kill")
        original_kill(self)

    _StandInEngine.kill = watched_kill  # type: ignore[method-assign]
    shutil.rmtree = watched_rmtree
    try:
        code, raised, _text, base = run_main([], strand)
    finally:
        shutil.rmtree = real_rmtree
        _StandInEngine.kill = original_kill  # type: ignore[method-assign]
    expect(stranded.killed,
           f"an engine stranded inside boot is disposed of (got {stranded})")
    expect(order[:2] == ["kill", "rmtree"],
           f"...and it is dead BEFORE the tree it was writing into is "
           f"removed (got {order})")
    expect(isinstance(raised, KeyboardInterrupt) and code is None,
           f"and the interrupt still ends the run (got {code}, {raised!r})")
    expect(base is not None and not os.path.exists(base),
           "with nothing left behind")

    # A handle that has already exited — the ordinary case, every phase
    # having quit its own engine — is not killed a second time.
    finished = _StandInEngine(alive=False)
    run_main([], lambda art: (art.launched.append(finished), 0)[1])
    expect(not finished.killed,
           "a handle that has already exited is left alone")


# ---------------------------------------------------------------------
# Opt-in retention
# ---------------------------------------------------------------------
def test_retention_keeps_a_passing_run_passing() -> None:
    print("\ntest_retention_keeps_a_passing_run_passing")
    code, raised, text, base = run_main(["--keep-artifacts"], lambda art: 0)
    try:
        expect(code == 0 and raised is None,
               f"the passing run still exits 0 (got {code}, {raised!r})")
        expect(base is not None and os.path.isdir(base),
               "--keep-artifacts retains a passing run's artifacts too")
        expect("retained this run's artifacts (--keep-artifacts)" in text
               and base is not None and f": {base}\n" in text,
               "...and the summary names the retained directory itself, not "
               "merely paths inside it")
    finally:
        if base:
            shutil.rmtree(base, ignore_errors=True)


def test_retention_keeps_a_failing_run_failing() -> None:
    print("\ntest_retention_keeps_a_failing_run_failing")
    code, raised, text, base = run_main(["--keep-artifacts"], lambda art: 1)
    try:
        expect(code == 1 and raised is None,
               f"the failing run still exits non-zero (got {code}, {raised!r})")
        expect(base is not None and os.path.isdir(base),
               "--keep-artifacts retains a failing run's artifacts — the "
               "engine log is its primary evidence")
        expect("retained this run's artifacts (--keep-artifacts)" in text
               and base is not None and f": {base}\n" in text,
               "...and the summary names the retained directory itself")
        expect("re-run with --keep-artifacts" not in text,
               "...and does not tell the operator to pass the flag it "
               "already has")
    finally:
        if base:
            shutil.rmtree(base, ignore_errors=True)


def test_retention_after_a_boot_abort_keeps_the_log() -> None:
    print("\ntest_retention_after_a_boot_abort_keeps_the_log")
    # The case the flag exists for: the abort message names the log, and
    # with the flag that path is still there to read.
    def abort(art):
        with open(art.engine_log, "w") as handle:
            handle.write("engine said something useful\n")
        raise SystemExit(f"engine exited before READY; see {art.engine_log}")

    code, raised, text, base = run_main(["--keep-artifacts"], abort)
    try:
        expect(code == 1 and raised is None,
               f"the abort still exits non-zero (got {code}, {raised!r})")
        expect(base is not None and os.path.isdir(base),
               "the tree is retained")
        expect(base is not None
               and os.path.isfile(os.path.join(base, "logs", "engine.log")),
               "and the log the abort message named is still readable")
        expect("engine.log" in text,
               f"...and the summary names it (got {text!r})")
    finally:
        if base:
            shutil.rmtree(base, ignore_errors=True)


def test_retention_reports_what_the_run_actually_produced() -> None:
    print("\ntest_retention_reports_what_the_run_actually_produced")
    with fresh_run() as art:
        out = io.StringIO()
        with contextlib.redirect_stdout(out):
            expect(probe.release_artifacts(art, keep=True) is None,
                   "retention is not a cleanup failure")
        text = out.getvalue()
        expect(os.path.isdir(art.base), "--keep-artifacts retains the tree")
        expect(text.count("(empty)") == 3,
               f"a pre-READY failure's retained tree is reported as empty "
               f"rather than as holding fixtures and save slots "
               f"(got {text.count('(empty)')} empty lines)")
        for label, path in (("the artifact directory", art.base),
                            ("the engine log directory", art.logs),
                            ("the fixtures", art.fixtures),
                            ("the saves", os.path.join(art.root, "saves")),
                            ("the isolated root", art.root)):
            expect(path in text, f"the summary names {label}")

    with fresh_run() as art:
        open(art.engine_log, "w").close()
        for name in FIXTURE_NAMES:
            open(art.fixture(name), "w").close()
        os.makedirs(os.path.join(art.root, "saves", "loc_content_probe_abc"))
        out = io.StringIO()
        with contextlib.redirect_stdout(out):
            probe.release_artifacts(art, keep=True)
        text = out.getvalue()
        for name in ("engine.log", *(f"{n}.yaml" for n in FIXTURE_NAMES),
                     "loc_content_probe_abc"):
            expect(name in text, f"a finished run's summary names {name}")
        expect("(empty)" not in text, "...and reports nothing as empty")


def test_retention_after_a_staging_failure_names_nothing_absent() -> None:
    print("\ntest_retention_after_a_staging_failure_names_nothing_absent")
    # `build()` stages incrementally, so a permission, source or
    # disk-space failure part-way through leaves a tree whose log,
    # fixture and saves directories were never created. Reporting those
    # as "(empty)" would name artifacts that do not exist at all.
    base = tempfile.mkdtemp(prefix="test_loc_content_partial_")
    art = probe.RunArtifacts(base)
    try:
        os.makedirs(art.root)  # staging died right after the root itself
        out = io.StringIO()
        with contextlib.redirect_stdout(out):
            expect(probe.release_artifacts(art, keep=True) is None,
                   "retention after a staging failure is not a cleanup "
                   "failure")
        text = out.getvalue()
        expect("(empty)" not in text,
               f"a directory that was never created is not reported as "
               f"empty (got {text!r})")
        expect(text.count("never created") == 3,
               f"each of the three absent artifact directories says so "
               f"(got {text.count('never created')})")
        expect(art.root in text and "resource root" in text,
               "the root that DOES exist is still named")
    finally:
        shutil.rmtree(base, ignore_errors=True)


# ---------------------------------------------------------------------
# What the probe still proves
# ---------------------------------------------------------------------
def test_the_fixture_bodies_are_byte_for_byte_unchanged() -> None:
    print("\ntest_the_fixture_bodies_are_byte_for_byte_unchanged")
    # Resolved at whichever module owns each constant now (#2095
    # requirement 7 moved them to the scenario that consumes them), and
    # `fixture_body` refuses a name it cannot find rather than skipping
    # it — an unresolvable constant is a failure, never a quiet pass.
    for name, digest in FIXTURE_DIGESTS.items():
        owners = fixture_definitions(name)
        expect(len(owners) == 1,
               f"{name} is defined exactly once across the surface, so a "
               f"re-export cannot become a second copy that drifts (got "
               f"{[path.name for path in owners]})")
        body = fixture_body(name).encode("utf-8")
        expect(hashlib.sha256(body).hexdigest() == digest,
               f"{name} is byte-for-byte what it was — moving which module "
               f"a fixture lives in must not change what it says")


def test_registration_order_and_loaders_are_unchanged() -> None:
    print("\ntest_registration_order_and_loaders_are_unchanged")
    # Placement and loot draws are order-sensitive: phase 3 registers
    # bogus location, bogus loot, quinoa location, quinoa loot, and
    # phase 4 registers dense alone (#1884 requirement 6).
    loads = surface_calls("load_fixture_yaml")
    expect(loads, "the surface really registers fixtures — an empty scan "
                  "would make both order checks below vacuous")
    owners = {path for path, _ in loads}
    expect(len(owners) == 1,
           f"one owner registers all five, so its source order IS the "
           f"registration order (got {sorted(path.name for path in owners)})")
    loaders = [node.args[1].value for _path, node in loads
               if isinstance(node.args[1], ast.Constant)]
    targets = [node.args[2].id for _path, node in loads
               if isinstance(node.args[2], ast.Name)]
    expect(targets == [f"{n}_yaml" for n in FIXTURE_NAMES],
           f"the five fixtures register in the unchanged order "
           f"(got {targets})")
    expect(loaders == list(FIXTURE_LOADERS),
           f"...each through its own loader (got {loaders})")


def test_every_fixture_still_goes_through_load_fixture_yaml() -> None:
    print("\ntest_every_fixture_still_goes_through_load_fixture_yaml")
    # #1342 / #1884 requirement 7: `load_fixture_yaml` raises when a
    # fixture registered nothing, so a file that is invalid for the
    # current schema stops the probe at SETUP instead of surfacing as
    # downstream behavioural failures.
    loads = surface_calls("load_fixture_yaml")
    expect(len(loads) == len(FIXTURE_NAMES),
           f"every one of the five fixtures is loaded through the checking "
           f"helper, and nothing else is (got {len(loads)})")
    sends = surface_calls("send")
    expect(sends, "the surface really calls send() — the exclusion below "
                  "is True over an empty node set")
    registering = [(path, node) for path, node in sends
                   if any("loadLocationYaml" in c.value
                          or "loadLootTableYaml" in c.value
                          for c in ast.walk(node)
                          if isinstance(c, ast.Constant)
                          and isinstance(c.value, str))]
    # A registration through a bare `send` is one naming a path THIS RUN
    # produced: an `art.fixture(...)` result, or one of the `<name>_yaml`
    # variables holding one. Those are the calls that would skip
    # `load_fixture_yaml`'s zero-count rejection and let an invalid
    # fixture surface as downstream behavioural failures instead.
    raw = [(path, node) for path, node in registering
           if any((isinstance(inner, ast.Name) and inner.id.endswith("_yaml"))
                  or (isinstance(inner, ast.Call)
                      and isinstance(inner.func, ast.Attribute)
                      and inner.func.attr == "fixture")
                  for inner in ast.walk(node))]
    expect(not raw,
           f"no fixture is registered through a bare send() that would skip "
           f"the registration check (got "
           f"{[(path.name, node.lineno) for path, node in raw]})")
    # The bare registrations that DO remain are the shipped catalogs the
    # setup helpers load, never this run's own files — spelled out so the
    # exclusion above is a justified boundary rather than a hole.
    shipped = [(path, node) for path, node in registering
               if any(isinstance(inner, ast.Constant)
                      and isinstance(inner.value, str)
                      and "data/" in inner.value
                      for inner in ast.walk(node))]
    expect(shipped and len(shipped) == len(registering),
           f"...and every bare registration left names a shipped data/ "
           f"catalog ({len(shipped)} of {len(registering)})")
    expect(probelib.FixtureNotRegistered is probe.FixtureNotRegistered,
           "and the probe still imports that helper's own failure type, so "
           "a rejected fixture ends the run rather than a traceback")


def test_both_log_assertions_read_this_invocations_log() -> None:
    print("\ntest_both_log_assertions_read_this_invocations_log")
    # #1884 requirement 9. Two checks ASSERT against the engine log —
    # the integrity diagnostic in phase 2 and the two unknown-content
    # warnings in phase 3 — so a read of anything but this invocation's
    # own log could report another run's evidence as this one's.
    opens = surface_calls("open")
    expect(opens, "the surface really opens files — an empty scan would "
                  "make every shape check below vacuous")
    reads = [(path, node) for path, node in opens
             if not any(isinstance(a, ast.Constant) and a.value == "w"
                        for a in node.args[1:])]
    writes = [pair for pair in opens if pair not in reads]
    expect(len(reads) == 2,
           f"the probe reads the log in exactly the two places that assert "
           f"against it (got {[(p.name, n.lineno) for p, n in reads]})")
    # The two now sit with the owners that assert on them — the
    # knowledge owner's integrity diagnostic and the dispatch owner's
    # unknown-content warnings — and each still takes the invocation's
    # `RunArtifacts` rather than reaching for a log of its own.
    expect(len({path for path, _ in reads}) == 2,
           f"...one in each of the two owners that read it (got "
           f"{sorted(path.name for path, _ in reads)})")
    expect(all(isinstance(node.args[0], ast.Attribute)
               and node.args[0].attr == "engine_log"
               and isinstance(node.args[0].value, ast.Name)
               and node.args[0].value.id == "art"
               for _path, node in reads),
           "and both read this invocation's own log")
    expect(len(writes) == len(FIXTURE_NAMES)
           and all(isinstance(node.args[0], ast.Name)
                   and node.args[0].id.endswith("_yaml")
                   for _path, node in writes),
           f"and every truncating write on the surface is one of the five "
           f"fixtures (got {len(writes)})")


def test_the_public_helpers_other_probes_import_are_intact() -> None:
    print("\ntest_the_public_helpers_other_probes_import_are_intact")
    # `tools/portal_ghost_probe.py` imports the first two and
    # `tools/test_location_probe_config_isolation.py` pins that sharing;
    # `save_and_wait` is called from there with its own `log`.
    expect(portal.make_isolated_root is probe.make_isolated_root
           and portal.remove_isolated_root is probe.remove_isolated_root
           and portal.save_and_wait is probe.save_and_wait,
           "portal_ghost_probe still shares all three helpers")
    # #2095: the façade RE-EXPORTS these rather than wrapping them. A
    # delegating wrapper would satisfy every `hasattr` above while
    # breaking this identity and the signature pin below, so assert the
    # objects really are the invocation module's own.
    expect(probe.make_isolated_root is invocation.make_isolated_root
           and probe.remove_isolated_root is invocation.remove_isolated_root
           and probe.save_and_wait is invocation.save_and_wait,
           "the façade re-exports the invocation module's own function "
           "objects, not wrappers around them")
    # …and the nine names other probes import all resolve on the façade,
    # which is the module they import from.
    missing = [name for name in (
        "load_defs", "gen_world", "placed_ready", "wait_floor",
        "make_isolated_root", "remove_isolated_root", "save_and_wait",
        "ruin_geometry", "spawn_counts") if not hasattr(probe, name)]
    expect(not missing,
           f"every helper another probe imports from this module is still "
           f"there (missing {missing})")
    save = inspect.signature(probe.save_and_wait).parameters
    expect(list(save) == ["port", "page", "slot", "failures", "log"],
           f"save_and_wait keeps its parameter names and order "
           f"(got {list(save)})")
    expect(save["log"].default is None,
           "with `log` now caller-supplied rather than a module global, "
           "because the log belongs to the invocation")
    with fresh_run() as art:
        expect(probe.make_isolated_root(art.base) == art.root,
               "make_isolated_root still answers with <base>/root, which is "
               "what RunArtifacts.build hands the engine")


# ---------------------------------------------------------------------
# Scenario ownership behind the façade (#2095)
# ---------------------------------------------------------------------
def test_the_reorganized_surface_is_complete() -> None:
    print("\ntest_the_reorganized_surface_is_complete")
    # Every structural scan above runs over SURFACE. If that set could
    # resolve to fewer files than the reorganization created, each of
    # those scans would report OK while inspecting less than the code —
    # so prove the set is real, and that the façade reaches all of it.
    expect(EXTRACTED,
           f"the scenario owners really are extracted into "
           f"{PACKAGE.name}/ (found {[p.name for p in EXTRACTED]})")
    expect(len(SURFACE) == len(EXTRACTED) + 2,
           f"the surface is the façade + __init__ + every extracted "
           f"module (got {len(SURFACE)} for {len(EXTRACTED)} extracted)")
    expect(all(path.is_file() for path in SURFACE),
           f"...and every file on it exists "
           f"{[str(p) for p in SURFACE if not p.is_file()]}")
    imported = facade_package_imports()
    on_disk = tuple(path.stem for path in EXTRACTED)
    expect(imported == on_disk,
           f"the façade imports exactly the modules that exist, so a scan "
           f"derived from either can never be the smaller one (imports "
           f"{list(imported)}, on disk {list(on_disk)})")
    expect(set(on_disk) == set(SCENARIO_OWNERS) | set(INFRASTRUCTURE),
           f"...and each is either a scenario owner or shared "
           f"infrastructure (got {list(on_disk)})")


def test_the_facade_keeps_one_run_entry_point() -> None:
    print("\ntest_the_facade_keeps_one_run_entry_point")
    # Substituting `probe.run` is the sole mechanism behind eight of the
    # lifecycle tests above (a passing run, a failing run, an early
    # return, a mid-run exception, a probelib.boot SystemExit abort, a
    # _PhaseAborted, a KeyboardInterrupt, and cleanup failure).
    # Delegating from `main` to per-scenario entry points instead would
    # remove that coverage silently.
    tree = facade_tree()
    runs = [node for node in tree.body
            if isinstance(node, ast.FunctionDef) and node.name == "run"]
    expect(len(runs) == 1,
           f"the façade defines exactly one module-level run (got "
           f"{len(runs)})")
    params = [arg.arg for arg in runs[0].args.args]
    expect(params == ["args", "art", "token"],
           f"...with the parameter order run_main's wrapper supplies "
           f"(got {params})")
    expect(inspect.signature(probe.run).parameters and
           list(inspect.signature(probe.run).parameters) == params,
           "and the live object agrees with the source")
    called = [node for node in ast.walk(tree)
              if isinstance(node, ast.Call)
              and isinstance(node.func, ast.Name) and node.func.id == "run"]
    expect(len(called) == 1,
           f"`main` reaches the scenarios through that one call and no "
           f"other (got {len(called)})")


def test_the_regeneration_boot_runs_once_per_visit_order() -> None:
    print("\ntest_the_regeneration_boot_runs_once_per_visit_order")
    # #2095 requirement 11. Seven static call sites, eight LAUNCHES,
    # because one site is the body of a two-element loop. Asserting only
    # the call-site count would accept that loop being unrolled,
    # flattened to a single iteration, or grown to three — each of which
    # changes the process count the acceptance pins.
    loops = [node for node in ast.walk(facade_tree())
             if isinstance(node, ast.For)
             and any(isinstance(inner, ast.Call)
                     and isinstance(inner.func, ast.Name)
                     and inner.func.id == "boot_isolated"
                     for inner in ast.walk(node))]
    expect(len(loops) == 1,
           f"exactly one boot site is inside a loop (got {len(loops)})")
    if not loops:
        return
    iterable = loops[0].iter
    expect(isinstance(iterable, ast.Tuple),
           f"...over a literal tuple, so its length is readable here "
           f"(got {type(iterable).__name__})")
    cases = getattr(iterable, "elts", [])
    expect(len(cases) == 2,
           f"...naming exactly the two visit orders (got {len(cases)})")
    literals = {node.value for node in ast.walk(iterable)
                if isinstance(node, ast.Constant)}
    expect({"same order", False, "reversed order", True} <= literals,
           f"...the SAME order and the REVERSED one, which is what proves "
           f"a stable instance keeps its own loot rather than consuming a "
           f"shared stream (got {sorted(map(str, literals))})")
    sites = surface_calls("boot_isolated")
    in_loop = [pair for pair in sites
               if any(inner is pair[1] for inner in ast.walk(loops[0]))]
    expect(len(in_loop) == 1,
           f"the loop holds one of them (got {len(in_loop)})")
    launches = (len(sites) - len(in_loop)) + len(cases) * len(in_loop)
    expect(len(sites) == BOOT_CALL_SITES and launches == PROCESS_LAUNCHES,
           f"{BOOT_CALL_SITES} call sites launch {PROCESS_LAUNCHES} engine "
           f"processes (got {len(sites)} sites, {launches} launches)")


def test_every_scenario_assertion_lives_with_its_owner() -> None:
    print("\ntest_every_scenario_assertion_lives_with_its_owner")
    # "The façade contains CLI/orchestration and compatibility exports
    # rather than the scenario assertion bodies." A probe's assertions
    # ARE its PASS diagnostics and its recorded failures, so count both,
    # per file, and compare against the pre-split file's own totals: a
    # lost assertion and a duplicated one both fail here.
    per_file: dict[str, tuple[int, int]] = {}
    for path, tree in surface_trees():
        passes = sum(1 for node in ast.walk(tree)
                     if isinstance(node, ast.Call)
                     and pass_diagnostic_text(node).startswith("PASS:"))
        records = sum(1 for node in ast.walk(tree)
                      if isinstance(node, ast.Call)
                      and isinstance(node.func, ast.Attribute)
                      and node.func.attr == "append"
                      and isinstance(node.func.value, ast.Name)
                      and node.func.value.id == "failures")
        per_file[path.stem] = (passes, records)
    expect(sum(p for p, _ in per_file.values()) == TOTAL_PASS_DIAGNOSTICS,
           f"the surface still prints exactly the pre-split file's "
           f"{TOTAL_PASS_DIAGNOSTICS} PASS diagnostics (got {per_file})")
    expect(sum(r for _, r in per_file.values()) == TOTAL_FAILURE_RECORDS,
           f"...and records exactly its {TOTAL_FAILURE_RECORDS} failures "
           f"(got {per_file})")
    facade_passes, facade_records = per_file[Path(probe.__file__).stem]
    expect(facade_passes == 0,
           f"the façade asserts nothing itself (got {facade_passes} PASS "
           f"diagnostic(s))")
    expect(facade_records == 1,
           f"...beyond the one orchestration failure it owns, the skipped "
           f"phase (got {facade_records})")
    for name in SCENARIO_OWNERS:
        expect(per_file[name][0] > 0 and per_file[name][1] > 0,
               f"{name} owns assertions of its own (got {per_file[name]})")
    for name in INFRASTRUCTURE:
        expect(per_file[name][0] == 0,
               f"{name} is infrastructure and asserts no scenario "
               f"contract (got {per_file[name][0]} PASS diagnostic(s))")


def test_no_scenario_owner_keeps_cross_scenario_state_in_a_module_global()\
        -> None:
    print("\ntest_no_scenario_owner_keeps_cross_scenario_state_in_a_"
          "module_global")
    # Cross-scenario state travels through the façade's one handoff
    # record. A module-level mutable container in an owner would be a
    # second, invisible channel — and one an import order could change
    # the meaning of. Immutable configuration (the fixture bodies, the
    # ids and radii) is deliberately NOT what this forbids.
    mutable = (ast.List, ast.Dict, ast.Set, ast.ListComp, ast.DictComp,
               ast.SetComp)
    offenders = []
    for path, tree in surface_trees():
        if path.stem not in SCENARIO_OWNERS:
            continue
        for node in tree.body:
            targets = (node.targets if isinstance(node, ast.Assign)
                       else [node.target] if isinstance(node, ast.AnnAssign)
                       else [])
            if not targets or node.value is None:
                continue
            builder = (isinstance(node.value, ast.Call)
                       and isinstance(node.value.func, ast.Name)
                       and node.value.func.id in {"list", "dict", "set"})
            if isinstance(node.value, mutable) or builder:
                offenders.extend(
                    f"{path.name}:{target.id}" for target in targets
                    if isinstance(target, ast.Name))
    expect(not offenders,
           f"no scenario owner holds mutable module-level state "
           f"(got {offenders})")


def test_the_handoff_record_carries_every_threaded_value() -> None:
    print("\ntest_the_handoff_record_carries_every_threaded_value")
    # The twelve values `run` used to accumulate in local variables
    # across its phases. Each must be a field of the record the façade
    # threads, and each must default to something a skipped phase can
    # leave alone — which is what makes the dependent phases skip rather
    # than assert against a value nothing produced.
    state = probe.ScenarioState()
    missing = [name for name in HANDOFF_FIELDS if not hasattr(state, name)]
    expect(not missing,
           f"the handoff record carries every threaded value "
           f"(missing {missing})")
    expect(not state.ruins and not state.loot1 and not state.named
           and state.saved_content is False and state.saved_naming is False
           and state.dangling_uid == -1,
           f"...and a fresh one reads as 'no phase has run yet' "
           f"(got {state})")
    fresh = probe.ScenarioState()
    fresh.ruins.append({"id": "probe"})
    expect(probe.ScenarioState().ruins == [],
           "each run gets its own containers — a shared class-level "
           "default would leak one invocation's state into the next")


def main() -> int:
    selftestlib.parse_verbose()
    test_two_invocations_share_no_path()
    test_every_fixture_path_is_absolute_and_owned()
    test_no_artifact_keeps_a_legacy_fixed_tmp_name()
    test_a_real_run_leaves_every_legacy_path_as_it_found_it()
    test_release_never_touches_what_the_run_did_not_create()
    test_release_does_not_follow_the_content_symlinks()
    test_a_passing_run_leaves_nothing()
    test_a_failing_run_still_releases_and_points_at_the_flag()
    test_an_early_return_still_releases()
    test_an_exception_mid_run_still_releases()
    test_a_phase_abort_still_releases()
    test_a_boot_abort_still_releases_and_points_at_the_flag()
    test_a_keyboard_interrupt_still_releases()
    test_cleanup_failure_fails_an_otherwise_clean_run()
    test_every_boot_is_handed_this_runs_log_and_root()
    test_launched_engines_are_dead_before_anything_is_removed()
    test_retention_keeps_a_passing_run_passing()
    test_retention_keeps_a_failing_run_failing()
    test_retention_after_a_boot_abort_keeps_the_log()
    test_retention_reports_what_the_run_actually_produced()
    test_retention_after_a_staging_failure_names_nothing_absent()
    test_the_fixture_bodies_are_byte_for_byte_unchanged()
    test_registration_order_and_loaders_are_unchanged()
    test_every_fixture_still_goes_through_load_fixture_yaml()
    test_both_log_assertions_read_this_invocations_log()
    test_the_public_helpers_other_probes_import_are_intact()
    test_the_reorganized_surface_is_complete()
    test_the_facade_keeps_one_run_entry_point()
    test_the_regeneration_boot_runs_once_per_visit_order()
    test_every_scenario_assertion_lives_with_its_owner()
    test_no_scenario_owner_keeps_cross_scenario_state_in_a_module_global()
    test_the_handoff_record_carries_every_threaded_value()
    if FAILURES:
        print(f"\n{len(FAILURES)} check(s) failed:")
        for failure in FAILURES:
            print(f"  {failure}")
        return selftestlib.concluded(1)
    return selftestlib.concluded(
        0, "\nAll location_content_probe artifact-ownership tests passed")


if __name__ == "__main__":
    raise SystemExit(main())
