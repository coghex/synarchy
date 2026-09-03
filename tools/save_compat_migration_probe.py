#!/usr/bin/env python3
"""Fresh-process save-compatibility migration probe (issue #766,
save-overhaul C4).

Real-engine, real-restart coverage of the ONE thing the pure hspec gates
("save components", "save migrations") cannot prove: that every REAL
tracked complete-session fixture on disk actually loads through the
normal #763 whole-session transaction, publishes, survives a paused
dwell, and re-saves/reloads in current format across a real process
restart -- end to end, not merely "the pure decode function returns
Right".

Iterates EVERY "complete-session" fixture declared across
docs/save_compat/manifest.json's baselines -- 20 of them as of issue
#1485, spanning the legacy pre-#760 B1 envelope, the #764
raw-to-typed-reference v1 shapes (MODERN envelopes decoded through the
ordinary registry-driven path, never the legacy fallback), and every
later baseline's own session fixture. That roster is never hard-coded
here: the manifest is the single source of it, and this run's trailing
summary names how many fixtures it actually covered.

Every one of them is provisioned with the SAME registry families
production's startup loader registers, in production's order (issue
#1485) -- see BOOTSTRAP_LOADERS. A headless boot runs no loading
screen, so nothing else supplies them, and the load path's own
content-reference validation rejects a whole load whose saved entities
name an unregistered definition. Before #1485 the bootstrap skipped
locations, and the 12 fixtures carrying a `ruin_small` never reached a
single migration assertion.

Flow per fixture (isolated resource root -- never touches a real
player's saves, see make_isolated_root):
  1. Place the fixture's bytes directly at saves/<slot>/world.synworld.
  2. Boot engine A, engine.loadSave(slot), wait for the load transaction
     to publish through the normal #763 staging/publish path (proving
     the migrated/decoded session is NOT special-cased at that
     boundary). Both halves of that are PREREQUISITES: if the load is
     not accepted, or does not publish, this fixture's scenario stops
     there (issue #1486). Everything below would otherwise run against
     a session that does not exist -- and some of it PASSES there, most
     vacuously step 3's paused dwell, which would compare an absent
     page's date against itself. The failed prerequisite is the last
     check reported for the fixture, the stages it made unreachable are
     accounted for as `[SKIP]` diagnostics (tools/probe_protocol.py's
     existing vocabulary -- a skip is never a pass and never a
     failure), the per-fixture cleanup below runs unchanged, and the
     remaining fixtures in the sweep are untouched. The same rule
     guards step 5's reload boundary.
  3. Assert the resulting session's structural state matches the
     fixture's OWN expectedCanonicalSummary: active page, paused, and a
     short paused dwell advances no gameplay date.
  4. Save the session under a NEW slot name -- the only way to observe
     "this now writes current-format bytes going forward" without
     inspecting cereal internals directly.
  5. Quit engine A, boot a FRESH engine B (same isolated root),
     engine.loadSave the NEW current-format slot, wait for publish.
  6. Compare structural state across the restart: active page
     unchanged, still begins paused.
  7. Unpause (via scripts.pause, the paired
     engine.setPaused+world.setTimeScale contract every other save/load
     probe already uses) to confirm the default time scale -- never
     comparing any subsequent random gameplay outcome -- and, for a
     fixture that declares them, to run its live-tick checks (below).

Additionally (issue #2055), a fixture may declare "liveTickChecks" in
its OWN expectedCanonicalSummary -- a list of {"unitId"} entries -- and
each named unit must then complete one real thought tick in the
UNPAUSED reloaded session of step 7. Every other assertion this probe
makes is evaluated while the session is PAUSED: the declared
luaStateChecks run at both load boundaries, and the second resave
follows the second one immediately. A migrated row can therefore carry
every correct persisted VALUE and still be unable to execute, which is
exactly the regression this phase exists for -- a sparse legacy unit_ai
row carried no `nextActionAt`, survived decode, canonical comparison,
resave, restart and reload, and then errored on `engine.gameTime() <
s.nextActionAt` the first time the AI tried to think. What is asserted
is deliberately outcome-independent (a registered action name, and
nextActionAt advanced past its immediate-decision default), and any Lua
`update` error logged during that unpaused window fails the fixture --
the pcall isolation keeps the engine running and only warns, so nothing
else here would notice.

Additionally (round-4 review), a fixture may declare "luaStateChecks" in
its OWN expectedCanonicalSummary -- a list of {"expr", "expected"} pairs
evaluated live through the debug console at EVERY load boundary above
(right after the initial migration-load, and again after the resave/
restart/reload round trip). Aggregate page/pause/time-scale checks alone
can never prove a migrated Lua module's actual unwrapped values and
reference edges came out correct, only that loading didn't outright
fail -- see declared_complete_session_fixtures' docstring. The
b3-lua-versioned-session-v1 fixture uses this to confirm its legacy v1
unit_ai/building_spawn payloads really did unwrap to the exact bare
numbers the v1 payload encoded, inside a genuine running engine, not
merely that the pure hspec decode path accepted the envelope structurally.

Additionally (round-6 review), at BOTH load boundaries this probe visits
(right after the initial migration-load's resave, and again after a
SECOND resave taken from engine B's freshly-reloaded state), the real
saved bytes on disk are run through the same Haskell decode-and-dump
logic tools/save_compat_audit.py's --generate-session command uses
(dump_canonical_summary, a real `cabal repl` subprocess), and the result
is structurally compared -- metadata, allocators, camera, EVERY page's
clock/map-mode, and every declared building/unit/unit-sim-state/craft-
bill/power-node -- against the fixture's own expectedCanonicalSummary.
Aggregate page/pause/time-scale checks (and fixture-declared Lua-state
checks) alone can never prove a migration didn't silently lose or
corrupt some OTHER piece of persistent Haskell state (a second page, an
entity, a craft bill) during publish/resave/reload -- only that loading
didn't outright fail.

Usage:
  python3 tools/save_compat_migration_probe.py [--port 9276]
  python3 tools/save_compat_migration_probe.py --self-test

--self-test boots no engine at all: it verifies the registry bootstrap
plan below and the startup-loader parser that keeps it honest, which is
what a reviewer can run in a second instead of grepping this file for
loader names, the live-tick phase's Lua-error marker against the engine
sources it is assembled from, plus both branches of the
load-prerequisite stop above (driven through injected doubles, so a
rejection and an accepted-but-unpublished load are provable without an
engine). A normal run performs the bootstrap-plan and marker
verifications first, before placing a fixture or booting anything.

Exit 0 = every check above passed, for every declared complete-session
fixture.
"""
from __future__ import annotations

import argparse
import contextlib
import io
import json
import os
import re
import shutil
import sys
import tempfile
import time
from pathlib import Path

from probelib import boot, quit_engine, send, send_json, wait_load_published
from save_compat_audit import dump_canonical_summary

REPO = Path(__file__).resolve().parent.parent
MANIFEST_PATH = REPO / "docs" / "save_compat" / "manifest.json"

# Fields whose value legitimately differs between a fixture's own
# ORIGINAL bytes and ANY real resave of it -- a resave is always
# CURRENT-format (never legacy) and always carries live Lua state from
# a real running VM (a migrated legacy session has none, pre-#761),
# so comparing them against a real post-resave dump would be comparing
# apples to oranges, not catching an actual migration bug. Also strips
# the two keys that aren't part of the dump schema at all.
_DUMP_COMPARE_EXCLUDED_KEYS = frozenset(
    {"$comment", "luaStateChecks", "liveTickChecks",
     "isMigratedLegacyBaseline", "luaComponentCount"})


STARTUP_LOADER_PATH = REPO / "scripts" / "startup_loader.lua"

#: The registry families this probe provisions into every headless
#: engine it boots, as ``(directory, engine verb, recursive)`` triples in
#: the order they are loaded (issue #1485).
#:
#: A headless boot reaches neither scripts/loading_screen.lua nor
#: scripts/ui_manager_boot.lua, so scripts/startup_loader.lua never runs
#: and NOTHING registers a definition -- which is why this helper exists
#: at all. But the load path's content-reference validation
#: (Engine.Scripting.Lua.API.Save, issue #760 requirement 9) rejects the
#: WHOLE load when a saved entity names a definition that is not
#: registered, so any family this list omits turns every fixture
#: referencing it into a content-validation rejection that never reaches
#: a migration assertion. Before #1485 this list held seven families and
#: omitted locations; the 12 tracked complete-session fixtures carrying
#: a `ruin_small` all failed there.
#:
#: This is deliberately production's OWN sequence rather than a
#: hand-picked subset: `verify_bootstrap_plan` asserts it equals
#: startup_loader.lua's `queueNormalProfile` exactly, so a family added
#: there is a loud, deterministic failure here instead of a fixture that
#: silently stops being covered. Ordering is load-bearing in one
#: direction -- a family whose definitions name ids from another must
#: come after it, which is why locations are LAST (their content ids,
#: incl. loot_table ids, reference the registries above; #90).
#:
#: `recursive` mirrors production's addYamlTree/addYamlDir split:
#: items are the one family whose definitions may live in
#: subdirectories (#1232), enumerated at any depth in the canonical
#: byte order of the path relative to the family root.
BOOTSTRAP_LOADERS: list[tuple[str, str, bool]] = [
    ("data/materials",   "engine.loadMaterialYaml",   False),
    ("data/vegetation",  "engine.loadVegetationYaml", False),
    ("data/flora",       "engine.loadFloraYaml",      False),
    ("data/substances",  "engine.loadSubstanceYaml",  False),
    ("data/infections",  "engine.loadInfectionYaml",  False),
    ("data/recipes",     "engine.loadRecipeYaml",     False),
    ("data/items",       "engine.loadItemYaml",       True),
    ("data/equipment",   "engine.loadEquipmentYaml",  False),
    ("data/buildings",   "engine.loadBuildingYaml",   False),
    ("data/units",       "engine.loadUnitYaml",       False),
    ("data/loot_tables", "engine.loadLootTableYaml",  False),
    ("data/locations",   "engine.loadLocationYaml",   False),
]

#: Every engine verb whose registry the load path's content-reference
#: validation can reject a load on. Engine.Scripting.Lua.API.Save folds
#: TEN checks into `allMessages` -- unit/building def, item,
#: significant-obligation item (#917), recipe, bill-output item,
#: construct def, material, flora, location, infection -- over these
#: EIGHT registries: the def check spans units and buildings, and the
#: bill-output, significant-obligation and construct checks re-read the
#: item, item and building registries respectively.
#:
#: Requirement 2 of #1485: the bootstrap must cover all of them, so a
#: fixture that LATER carries a flora or infection reference is already
#: provisioned rather than needing this same repair again. Loot tables
#: and vegetation are NOT validated on the load path; they are in
#: BOOTSTRAP_LOADERS because production loads them and locations
#: resolve loot_table ids at spawn time (#90), not because a load can
#: be rejected on them.
LOAD_VALIDATED_LOADERS = frozenset({
    "engine.loadUnitYaml",
    "engine.loadBuildingYaml",
    "engine.loadItemYaml",
    "engine.loadRecipeYaml",
    "engine.loadMaterialYaml",
    "engine.loadFloraYaml",
    "engine.loadLocationYaml",
    "engine.loadInfectionYaml",
})

#: The ASCII spine of the content-validation rejection message
#: Engine.Scripting.Lua.API.Save builds ("... reference a gameplay
#: definition no longer registered - aborting the entire load ...").
#: Matched against the load status's own `outcome` to tell a bootstrap
#: gap apart from a real migration failure; see `load_failure_reason`.
MISSING_DEF_OUTCOME_MARKER = "gameplay definition no longer registered"


class BootstrapPlanError(RuntimeError):
    """scripts/startup_loader.lua's registry sequence could not be read.

    A SETUP failure, raised rather than returned so an unreadable or
    restructured startup loader can never be mistaken for "production
    loads nothing" -- which would let `verify_bootstrap_plan` certify
    any plan at all, including one that provisions nothing (#1342's
    lesson: a setup check that passes while checking nothing is worse
    than no check).
    """


_QUEUE_NORMAL_PROFILE_RE = re.compile(
    r"^local function queueNormalProfile\(\)$(?P<body>.*?)^end$",
    re.MULTILINE | re.DOTALL)

#: The three `addYaml...(dir, label, loader)` verbs `queueNormalProfile`
#: enqueues a registry family with. `DirCanonical` (#2241) is a FLAT
#: directory loaded in canonical byte order -- flora's, whose sequential
#: FloraIds a save's numeric references name -- so it is a `Dir` as far
#: as this probe's `recursive` flag is concerned; only `Tree` recurses.
#: Longest alternative first, or `Dir` would match the prefix of
#: `DirCanonical` and leave the rest of the pattern to fail.
_ADD_YAML_CALL_RE = re.compile(
    r"^[ \t]*addYaml(?P<kind>DirCanonical|Dir|Tree)\(\s*"
    r"\"(?P<dir>[^\"]+)\"\s*,\s*\"[^\"]*\"\s*,\s*"
    r"(?P<loader>engine\.load[A-Za-z]+)\s*\)",
    re.MULTILINE)


def production_registry_sequence(source: str | None = None
                                  ) -> list[tuple[str, str, bool]]:
    """The registry families scripts/startup_loader.lua's
    `queueNormalProfile` actually enqueues, in ITS order, as the same
    ``(directory, engine verb, recursive)`` triples BOOTSTRAP_LOADERS
    uses.

    Reads only that one function's body, so `queueArenaProfile`'s much
    smaller dev-boot subset cannot leak in, and only its
    `addYamlDir`/`addYamlDirCanonical`/`addYamlTree` calls, so the
    tutorial directory load
    (an `addItem`, not a registry family -- and self-contained authored
    data that references no registry) and the texture-only phases are
    excluded. A commented-out call is excluded too: the pattern anchors
    the verb to the start of its line, past leading whitespace only.

    Raises BootstrapPlanError when the function cannot be located or
    enqueues no YAML family at all.
    """
    if source is None:
        try:
            source = STARTUP_LOADER_PATH.read_text(encoding="utf-8")
        except OSError as error:
            raise BootstrapPlanError(
                f"could not read {STARTUP_LOADER_PATH} to compare this "
                f"probe's registry bootstrap against production's: "
                f"{error}") from None
    match = _QUEUE_NORMAL_PROFILE_RE.search(source)
    if match is None:
        raise BootstrapPlanError(
            f"could not locate `local function queueNormalProfile()` in "
            f"{STARTUP_LOADER_PATH}; this probe's registry bootstrap can "
            f"no longer be checked against production's and must be "
            f"re-derived by hand")
    found = [
        (call.group("dir"), call.group("loader"), call.group("kind") == "Tree")
        for call in _ADD_YAML_CALL_RE.finditer(match.group("body"))
    ]
    if not found:
        raise BootstrapPlanError(
            f"`queueNormalProfile` in {STARTUP_LOADER_PATH} enqueues no "
            f"addYamlDir/addYamlDirCanonical/addYamlTree registry family; "
            f"the parse found "
            f"nothing to compare against, which is a broken check rather "
            f"than a production that loads no registries")
    return found


def yaml_files(directory: str, recursive: bool) -> list[str]:
    """One family's YAML files as ENGINE-relative paths (`data/...`).

    Enumerated from the repository rather than the probe's own working
    directory -- the engine resolves these under its isolated resource
    root, whose `data` symlinks back here (see make_isolated_root), so
    the paths are correct wherever this script was invoked from.

    `recursive` mirrors production's addYamlTree, including its ordering
    rule: startupLoader.canonicalFileOrder sorts by the UTF-8 BYTES of
    the path relative to the family root, deliberately not by any
    locale-dependent collation, so this sorts on the encoded bytes too.
    """
    root = REPO / directory
    if not root.is_dir():
        return []
    paths = root.rglob("*.yaml") if recursive else root.glob("*.yaml")
    rels = sorted((path.relative_to(root).as_posix() for path in paths),
                  key=lambda rel: rel.encode("utf-8"))
    return [f"{directory}/{rel}" for rel in rels]


def verify_bootstrap_plan() -> list[str]:
    """Check BOOTSTRAP_LOADERS before any engine is booted, returning one
    string per problem (empty ⇒ usable).

    Deterministic and engine-free, and run at the top of every normal
    probe run: a bootstrap that has silently stopped provisioning a
    family the fixtures need would otherwise surface only as whichever
    load happens to be rejected first, arbitrarily far from the cause.
    """
    problems: list[str] = []
    try:
        production = production_registry_sequence()
    except BootstrapPlanError as error:
        return [str(error)]

    if BOOTSTRAP_LOADERS != production:
        for index, expected in enumerate(production):
            actual = (BOOTSTRAP_LOADERS[index]
                      if index < len(BOOTSTRAP_LOADERS) else None)
            if actual != expected:
                problems.append(
                    f"BOOTSTRAP_LOADERS diverges from "
                    f"queueNormalProfile at position {index}: production "
                    f"enqueues {expected!r}, this probe has {actual!r}")
                break
        else:
            problems.append(
                f"BOOTSTRAP_LOADERS carries {len(BOOTSTRAP_LOADERS)} "
                f"families but queueNormalProfile enqueues "
                f"{len(production)}: extra entries "
                f"{BOOTSTRAP_LOADERS[len(production):]!r}")

    if not BOOTSTRAP_LOADERS:
        problems.append("BOOTSTRAP_LOADERS is empty")
    elif BOOTSTRAP_LOADERS[-1][1] != "engine.loadLocationYaml":
        problems.append(
            f"locations must be provisioned LAST (their content ids "
            f"reference the registries above, #90); the last family is "
            f"{BOOTSTRAP_LOADERS[-1][0]!r}")

    provisioned = {loader for _, loader, _ in BOOTSTRAP_LOADERS}
    for loader in sorted(LOAD_VALIDATED_LOADERS - provisioned):
        problems.append(
            f"{loader} is not in BOOTSTRAP_LOADERS, but the load path's "
            f"content-reference validation can reject a fixture's load "
            f"on that registry")

    for directory, loader, recursive in BOOTSTRAP_LOADERS:
        if not yaml_files(directory, recursive):
            problems.append(
                f"{directory}/ enumerated no .yaml file, so {loader} "
                f"would register nothing from it")
    return problems


def bootstrap_defs(port: int) -> None:
    """Provision BOOTSTRAP_LOADERS into a freshly booted headless engine.

    One `send` per file, exactly as production enqueues one queue item
    per file, so each loader still sees each definition file once and in
    the same order. `verify_bootstrap_plan` has already established that
    the plan is the production one and that every family enumerates
    files, so this stays a straight-line replay.
    """
    for directory, loader, recursive in BOOTSTRAP_LOADERS:
        for path in yaml_files(directory, recursive):
            send(port, f"{loader}('{path}'); return 'ok'")


def load_failure_reason(status) -> str:
    """Why a load did not publish, saying WHICH KIND of failure it is.

    Requirement 5 of #1485: a fixture rejected because this probe never
    registered a definition family is a HARNESS setup failure, and must
    never read like the migration itself is broken. The engine already
    separates the two -- a content-reference rejection reports
    `failedAtPhase == "LoadContentValidated"` with an unknown-definition
    outcome, while a decode or migration failure keeps its own earlier
    phase and its own outcome -- so this reads that distinction back out
    rather than guessing from the message alone.
    """
    if not isinstance(status, dict):
        return ("the load never reported a terminal status (no "
                "engine.getLoadStatus() table was observed)")
    phase = status.get("failedAtPhase") or status.get("phase")
    outcome = str(status.get("outcome", ""))
    if (status.get("failedAtPhase") == "LoadContentValidated"
            and MISSING_DEF_OUTCOME_MARKER in outcome):
        return ("SETUP FAILURE, not a migration failure: the load was "
                "rejected at content validation because the fixture "
                "references a definition this probe never registered. "
                "Fix BOOTSTRAP_LOADERS (issue #1485), not the fixture. "
                f"Engine outcome: {outcome}")
    return (f"MIGRATION FAILURE: the load reached {phase!r} and did not "
            f"publish. Engine outcome: {outcome or '(none reported)'}")


def make_isolated_root(base: str) -> str:
    """A throwaway resource root: real scripts/assets/data/config
    (symlinked -- read-only content, safe to share) plus its OWN empty
    saves/ directory, mirroring tools/save_storage_probe.py's helper."""
    root = os.path.join(base, "root")
    os.makedirs(root, exist_ok=True)
    for family in ("scripts", "assets", "data", "config"):
        target = os.path.join(root, family)
        if not os.path.exists(target):
            os.symlink(os.path.join(REPO, family), target)
    os.makedirs(os.path.join(root, "saves"), exist_ok=True)
    return root


def declared_complete_session_fixtures() -> list[dict]:
    """Every "complete-session" fixture declared across every baseline in
    the manifest, each paired with its OWN expected canonical summary
    (active page, in particular, differs per fixture -- the B1 baseline's
    is "main_world", the #764 baseline's is "page1")."""
    manifest = json.loads(MANIFEST_PATH.read_text(encoding="utf-8"))
    out = []
    for baseline in manifest["baselines"]:
        for fixture in baseline.get("fixtures", []):
            if fixture.get("kind") != "complete-session":
                continue
            summary_path = fixture.get("expectedCanonicalSummary")
            if not summary_path:
                sys.exit(f"FAIL: complete-session fixture "
                          f"'{fixture.get('id')}' has no "
                          f"expectedCanonicalSummary")
            summary = json.loads((REPO / summary_path).read_text(encoding="utf-8"))
            out.append({
                "baseline_id": baseline["id"],
                "fixture_id": fixture["id"],
                "path": REPO / fixture["path"],
                "active_page": summary["activePage"],
                # Round-6 review: the FULL declared canonical summary,
                # for dump_and_compare -- not just activePage.
                "expected_summary": summary,
                # Round-4 review: aggregate page/pause/time-scale checks
                # alone never prove a fixture's ACTUAL persistent state
                # (in particular a migrated Lua module's real, unwrapped
                # values and reference edges) came out correct -- just
                # that loading/publishing/resaving didn't outright fail.
                # A fixture opts into this by declaring "luaStateChecks"
                # in its OWN expectedCanonicalSummary: a list of
                # {"expr": <Lua expression, evaluated via the debug
                # console>, "expected": <expected return-value string>}
                # pairs, each re-checked at every load boundary this
                # probe already visits (right after the initial
                # migration-load AND after the resave/restart/reload) --
                # so a migration that produces a self-consistent but
                # WRONG value (which the pure hspec re-encode/decode
                # equivalence check alone cannot catch either) fails
                # here for real, inside a genuine running engine.
                "lua_state_checks": summary.get("luaStateChecks", []),
                # Issue #2055: the checks above run at both load
                # boundaries, but BOTH of those are evaluated while the
                # session is still paused -- and the second one is
                # followed immediately by the second resave. A migrated
                # row's values can therefore all be right and the unit
                # still be unable to TICK, which is precisely the
                # regression this fixture reproduced: a sparse legacy
                # unit_ai row survived decode, comparison, resave,
                # restart and reload and then errored on its first live
                # thought tick. So a fixture may additionally declare
                # "liveTickChecks" -- a list of {"unitId"} entries -- to
                # opt into a POST-UNPAUSE phase that watches those units
                # actually complete a decision cycle in a running,
                # unpaused engine. See run_live_tick_checks.
                "live_tick_checks": summary.get("liveTickChecks", []),
            })
    if not out:
        sys.exit("FAIL: docs/save_compat/manifest.json declares no "
                  "complete-session fixtures")
    return out


def boot_probe(root: str, port: int, log: str):
    return boot(port, log=log, args=["--resource-root", root], ready_timeout=180)


class Checks:
    def __init__(self) -> None:
        self.failed = 0
        self.skipped = 0

    def ok(self, cond: bool, label: str) -> None:
        print(f"  [{'PASS' if cond else 'FAIL'}] {label}")
        if not cond:
            self.failed += 1

    def skip(self, label: str) -> None:
        """A check that could not run, reported in the SKIP vocabulary
        tools/probe_protocol.py already defines -- its `SKIP`
        DIAGNOSTIC_LEVELS entry (:83) and the `[SKIP]` line marker its
        standalone `reporter.skip` prints (:506) -- rather than a third
        vocabulary invented here (issue #1486 requirement 3; this probe
        is deliberately NOT migrated to probe-result/v1, which is #1471
        and #1474's kind of work).

        A skipped check is neither a pass nor a failure, so this never
        touches `self.failed` (requirement 4): the run's exit status
        stays driven by real failed checks, and a fixture whose load
        prerequisite failed still exits non-zero because that
        prerequisite is itself a genuine FAILED check.
        """
        print(f"  [SKIP] {label}")
        self.skipped += 1

    def skip_stages(self, prerequisite: str, stages) -> None:
        """Account for every scenario stage a failed prerequisite made
        unreachable, naming that prerequisite on each line -- so what
        did not run is visible rather than silently absent."""
        for stage in stages:
            self.skip(f"{stage} -- not run: {prerequisite}")


def await_load_prerequisite(chk: Checks, port: int, slot: str, accepted,
                            published: str, unreachable,
                            *, sender=send, waiter=wait_load_published
                            ) -> bool:
    """One load boundary's two prerequisites -- `engine.loadSave(slot)`
    being accepted synchronously, and the transaction it starts actually
    publishing -- reported as the same two checks this probe has always
    reported, and answering whether the fixture's scenario may continue
    (issue #1486).

    True only when the load was BOTH accepted and published. A caller
    that gets False returns from the fixture immediately, because every
    later check would otherwise run against a session that does not
    exist -- and some of them PASS there: the 2s paused dwell compares
    `world.getDate` for an absent page against itself, reporting a
    preserved invariant about a session that was never loaded. Returning
    is cleanup-safe and sweep-safe: it passes through
    `run_one_fixture`'s own `finally` (both engines quit, temporary root
    removed) and `main` iterates the remaining fixtures regardless.

    The failing check is the LAST check outcome printed for the fixture;
    only `[SKIP]` diagnostics follow it, since SKIP is a DIAGNOSTIC
    level in tools/probe_protocol.py's vocabulary and not a check
    result. `unreachable` names the downstream stages, grouped by stage
    rather than reproducing every dynamic check label. A synchronous
    rejection additionally skips the publication check and never calls
    `waiter`: no transaction was started, so waiting could only report a
    timeout as though a real load had stalled.

    `sender`/`waiter` are injectable ONLY so `self_test` can drive both
    failure branches with no engine at all; production always uses this
    module's own `send`/`wait_load_published`.
    """
    response = sender(port, f"return engine.loadSave('{slot}')")
    if response.strip() != "true":
        chk.ok(False, accepted(response))
        chk.skip_stages(
            f"engine.loadSave('{slot}') did not accept the load "
            f"(got {response!r})",
            (published,) + tuple(unreachable))
        return False
    chk.ok(True, accepted(response))
    did_publish, status = waiter(port)
    if not did_publish:
        chk.ok(False, f"{published} -- {load_failure_reason(status)}")
        chk.skip_stages(
            f"the load of '{slot}' was accepted but never published",
            unreachable)
        return False
    chk.ok(True, published)
    return True


def _values_match(actual: str, expected) -> bool:
    """Numeric-tolerant comparison: the debug console's number-to-string
    formatting can render an integer-valued Lua number as either "1" or
    "1.0" depending on path (mirrors this same file's existing
    ("1", "1.0")-tolerant world.getTimeScale check) -- comparing as
    floats when both sides parse avoids a spurious failure over pure
    formatting, while still catching an actually wrong value."""
    try:
        return float(actual) == float(expected)
    except (TypeError, ValueError):
        return actual.strip('"') == str(expected)


def run_lua_state_checks(chk: Checks, port: int, checks: list[dict], when: str) -> None:
    """Evaluate each fixture-declared {"expr", "expected"} pair for real,
    through the live engine's debug console -- see
    declared_complete_session_fixtures' docstring for why this exists.
    `when` (e.g. "after initial migration-load" / "after resave/reload")
    only labels the printed check, since the SAME expression is checked
    at multiple load boundaries."""
    for check in checks:
        expr = check["expr"]
        expected = check["expected"]
        actual = send(port, f"return {expr}").strip()
        chk.ok(_values_match(actual, expected),
               f"{when}: `{expr}` == {expected!r} (got {actual!r})")


def _canonicalize_for_compare(d: dict) -> dict:
    return {k: v for k, v in d.items() if k not in _DUMP_COMPARE_EXCLUDED_KEYS}


def _first_diff(actual, expected, path: str = "") -> str:
    """A short description of the first structural difference between an
    ACTUAL (real-dumped) and EXPECTED (fixture-declared) canonical-
    summary value, recursing into nested dicts/lists -- or "" if they
    match. Not a full diff, just enough for a probe failure message to
    point at the specific field a migration lost or corrupted."""
    if isinstance(expected, dict):
        if not isinstance(actual, dict):
            return f"{path}: expected an object, got {actual!r}"
        for k, v in expected.items():
            if k not in actual:
                return f"{path}.{k}: missing from the real dump"
            d = _first_diff(actual[k], v, f"{path}.{k}")
            if d:
                return d
        return ""
    if isinstance(expected, list):
        if not isinstance(actual, list):
            return f"{path}: expected an array, got {actual!r}"
        if len(actual) != len(expected):
            return (f"{path}: real dump has {len(actual)} entries, "
                     f"expected {len(expected)}")
        for i, (a, e) in enumerate(zip(actual, expected)):
            d = _first_diff(a, e, f"{path}[{i}]")
            if d:
                return d
        return ""
    if isinstance(expected, (int, float)) and isinstance(actual, (int, float)):
        if float(actual) != float(expected):
            return f"{path}: real dump has {actual!r}, expected {expected!r}"
        return ""
    if actual != expected:
        return f"{path}: real dump has {actual!r}, expected {expected!r}"
    return ""


def dump_and_compare(chk: Checks, tmp_dir: str, file_path: str,
                      expected_summary: dict, when: str) -> None:
    """Derive `file_path`'s REAL canonical summary via the SAME Haskell
    decode-and-dump logic tools/save_compat_audit.py's --generate-session
    uses (World.Save.Envelope.decodeSessionEnvelope, run through a real
    `cabal repl` subprocess), then structurally compare EVERY field the
    fixture declares -- metadata, allocators, camera, and every page's
    clock/map-mode/entity slices -- against it (round-6 review: the
    aggregate page/pause/time-scale checks above, and fixture-declared
    Lua-state checks, can never prove a migration didn't lose or corrupt
    some OTHER piece of persistent Haskell state -- only that loading/
    publishing/resaving didn't outright fail)."""
    out_path = os.path.join(tmp_dir, f"dumped_{abs(hash(when))}.json")
    ok, tail = dump_canonical_summary(Path(file_path), Path(out_path))
    if not ok:
        chk.ok(False, f"{when}: dump_canonical_summary failed: {tail}")
        return
    dumped = json.loads(Path(out_path).read_text(encoding="utf-8"))
    diff = _first_diff(_canonicalize_for_compare(dumped),
                        _canonicalize_for_compare(expected_summary))
    chk.ok(diff == "",
           f"{when}: real dumped Haskell state matches the fixture's "
           f"declared canonical summary"
           + (f" -- MISMATCH at {diff}" if diff else ""))


#: The engine's own wording when a Lua callback raises under the pcall
#: isolation in Engine.Scripting.Lua.Script.callModuleFunctionReportingError
#: -- the exact text a failed per-tick `update` produces.
#:
#: A literal match against a log is only as good as the literal, and this
#: one is assembled in Haskell from two separate places, so a silent
#: mismatch would turn this probe's Lua-error scan into a check that can
#: never fire. `verify_lua_error_marker` (run by --self-test AND at the
#: top of every real run, beside the bootstrap-plan check) asserts BOTH
#: halves still exist in the engine sources, so a rename there is a loud,
#: engine-free failure here instead.
LUA_UPDATE_ERROR_MARKER = "Lua error in update()"

#: The two engine sources `LUA_UPDATE_ERROR_MARKER` is composed from, and
#: the fragment each must still contain: the format string that words the
#: warning, and the scheduler call that names `update` as the callback.
LUA_ERROR_MARKER_SOURCES: list[tuple[str, str]] = [
    ("src/Engine/Scripting/Lua/Script.hs",
     '"Lua error in " <> funcName <> "(): "'),
    ("src/Engine/Scripting/Lua/Thread.hs",
     'callModuleFunction ls (scriptModuleRef script) "update"'),
]


def verify_lua_error_marker() -> list[str]:
    """Both halves of `LUA_UPDATE_ERROR_MARKER` still exist in the engine.

    Returns a list of problems (empty when the marker is still what the
    engine emits). Reads source text only -- no build, no engine, no GPU.
    """
    problems: list[str] = []
    for rel, fragment in LUA_ERROR_MARKER_SOURCES:
        path = REPO / rel
        try:
            text = path.read_text(encoding="utf-8")
        except OSError as error:
            problems.append(f"could not read {rel} ({error})")
            continue
        if fragment not in text:
            problems.append(
                f"{rel} no longer contains {fragment!r} -- "
                f"LUA_UPDATE_ERROR_MARKER ({LUA_UPDATE_ERROR_MARKER!r}) is "
                f"assembled from it, so the post-unpause Lua-error scan "
                f"would silently match nothing; re-derive the marker from "
                f"the engine's current wording")
    return problems

#: How long a declared live-tick unit gets to complete one decision
#: cycle after the session is unpaused. The AI's own cadence is ~1s
#: (thought_interval, scripts/unit_ai_tunables.lua) and the script ticks
#: at 0.1s, so this is two orders of magnitude of headroom -- generous
#: on purpose, because the assertion this guards is "did it tick at
#: all", and a tight bound would turn a slow CI host into a false
#: regression.
LIVE_TICK_TIMEOUT_SEC = 30.0


def run_live_tick_checks(chk: Checks, port: int, checks: list[dict],
                         log_path: str, log_offset: int) -> None:
    """Watch each declared unit complete one live thought tick in the
    UNPAUSED, freshly-reloaded session (issue #2055).

    Every other assertion this probe makes is evaluated while the
    session is paused: `run_lua_state_checks` runs at both load
    boundaries, and the second resave follows the second one
    immediately. That is exactly how a migrated row whose every
    persisted VALUE is correct can still be unable to run -- a sparse
    legacy unit_ai row carried no `nextActionAt`, and the first live
    tick's `engine.gameTime() < s.nextActionAt` errored on the nil,
    after the fixture had already passed every check here.

    What is asserted is deliberately outcome-INDEPENDENT, because the
    AI's choice is a real utility contest over live physiology and a
    jittered clock:

      * the unit's `currentAction` is a name its OWN species actually
        registered (scripts/unit_ai_actions.lua's inventory, which is
        recorded by the one function that builds every action list), and
      * `nextActionAt` has advanced past the immediate-decision default
        of 0, which only `scheduleNext` at the END of the decision block
        can do.

    Which action it picked, and where it walked, are never compared.

    A timeout fails, and so does ANY Lua `update` error the engine
    logged after `log_offset` -- the byte position the caller recorded
    before unpausing, so a pre-existing warning from earlier in this
    same run can neither mask nor manufacture a failure here.
    """
    if not checks:
        return
    deadline = time.time() + LIVE_TICK_TIMEOUT_SEC
    pending = {int(check["unitId"]) for check in checks}
    observed: dict[int, dict] = {}
    while pending and time.time() < deadline:
        for uid in sorted(pending):
            state = _live_tick_state(port, uid)
            observed[uid] = state
            if state.get("ticked"):
                pending.discard(uid)
        if pending:
            time.sleep(0.5)

    for check in checks:
        uid = int(check["unitId"])
        state = observed.get(uid, {})
        chk.ok(uid not in pending,
               f"after unpausing the reloaded session: unit {uid} "
               f"completed a live thought tick within "
               f"{LIVE_TICK_TIMEOUT_SEC:.0f}s -- currentAction is a "
               f"registered action and nextActionAt advanced past the "
               f"immediate-decision default (observed {state!r})")

    # A tick that RAN but raised is still a failure, and it is the shape
    # the pre-#2055 regression actually took: the pcall isolation keeps
    # the engine alive and only warns, so nothing above would notice.
    appended = ""
    try:
        with open(log_path, "r", errors="replace") as handle:
            handle.seek(log_offset)
            appended = handle.read()
    except OSError as error:
        chk.ok(False, f"could not re-read {log_path} to scan for Lua "
                      f"update errors after unpausing: {error}")
        return
    offending = [line for line in appended.splitlines()
                 if LUA_UPDATE_ERROR_MARKER in line]
    chk.ok(not offending,
           "no Lua update() error was logged after unpausing the "
           "reloaded session"
           + (f" -- got {len(offending)}, first: {offending[0].strip()!r}"
              if offending else ""))


def _live_tick_state(port: int, uid: int) -> dict:
    """One unit's live decision state, read through the debug console.

    Single-line by necessity (the console takes one Lua line), and
    deliberately derived from production's own sources: `aiState` is the
    table the AI ticks, and the registered-action set comes from
    scripts/unit_ai_actions.lua's inventory keyed by the unit's REAL
    defName, never a hard-coded action list here.
    """
    expr = (
        "local ua=require('scripts.unit_ai'); local s=ua.aiState[%d]; "
        "if not s then return { row=false } end; "
        "local info=unit.getInfo(%d); "
        "local acts=require('scripts.unit_ai_actions')"
        ".byDef[info and info.defName or ''] or {}; "
        "local n=s.nextActionAt; "
        "return { row=true, action=tostring(s.currentAction), "
        "registered=(acts[s.currentAction]==true), "
        "nextActionAt=(n or -1), "
        "ticked=((acts[s.currentAction]==true) and n ~= nil and n > 0) }"
    ) % (uid, uid)
    state = send_json(port, "return (function() " + expr + " end)()")
    # send_json hands back TEXT for anything that isn't valid JSON (and
    # None for an empty reply), so a console hiccup reads as "not ticked
    # yet" and the polling loop keeps trying until the deadline, rather
    # than crashing this phase on a transport problem.
    return state if isinstance(state, dict) else {"unparsed": state}


def run_one_fixture(chk: Checks, port: int, fixture: dict) -> None:
    fixture_bytes = fixture["path"].read_bytes()
    active_page = fixture["active_page"]
    legacy_slot = f"probe_{fixture['fixture_id']}_legacy"
    resaved_slot = f"probe_{fixture['fixture_id']}_resaved"
    tmpdir = tempfile.mkdtemp(prefix="save_compat_migration_probe_")
    logA = f"/tmp/save_compat_migration_probe_{fixture['fixture_id']}_A.log"
    logB = f"/tmp/save_compat_migration_probe_{fixture['fixture_id']}_B.log"
    procA = procB = None

    print(f"\n=== {fixture['baseline_id']} / {fixture['fixture_id']} "
          f"(expected active page: {active_page}) ===")
    try:
        root = make_isolated_root(tmpdir)
        legacy_dir = os.path.join(root, "saves", legacy_slot)
        os.makedirs(legacy_dir, exist_ok=True)
        with open(os.path.join(legacy_dir, "world.synworld"), "wb") as f:
            f.write(fixture_bytes)
        print(f"placed {len(fixture_bytes)}-byte fixture at "
              f"{legacy_dir}/world.synworld")

        # ── Engine A: load the fixture, verify, re-save ─────────────────
        procA = boot_probe(root, port, logA)
        bootstrap_defs(port)
        lua_checks = len(fixture["lua_state_checks"])
        live_checks = len(fixture["live_tick_checks"])
        # Everything below this boundary needs a session that actually
        # published; a failure here stops THIS fixture and nothing else
        # (issue #1486).
        if not await_load_prerequisite(
                chk, port, legacy_slot,
                accepted=lambda got: (
                    f"engine.loadSave('{legacy_slot}') accepted the fixture "
                    f"(got {got!r})"),
                published="fixture's load transaction published through the "
                          "normal #763 staging/publish path",
                unreachable=(
                    "engine A's migrated-session structural checks (active "
                    "page, begins paused, and no gameplay date advancing "
                    "across the 2s paused dwell)",
                    f"this fixture's {lua_checks} declared Lua-state "
                    f"check(s) after the initial migration-load",
                    "the resave under a new slot, and the real-dumped "
                    "Haskell state comparison against the fixture's "
                    "declared canonical summary",
                    "engine B's whole fresh-process reload of that resave: "
                    "its structural checks, its declared Lua-state checks, "
                    "the second resave and dump comparison, the default "
                    "time-scale check, and its declared live-tick checks")):
            return

        active = send(port, "return world.getActiveWorldId()").strip('"')
        chk.ok(active == active_page,
               f"session's active page is '{active_page}' (got {active!r})")
        chk.ok(send(port, "return engine.isPaused()") == "true",
               "session begins paused, same as any loaded session")

        date_a = send(port, f"return world.getDate('{active_page}')")
        time.sleep(2.0)
        date_b = send(port, f"return world.getDate('{active_page}')")
        chk.ok(date_a == date_b,
               f"no gameplay time advanced during a 2s paused dwell "
               f"({date_a} -> {date_b})")

        run_lua_state_checks(chk, port, fixture["lua_state_checks"],
                              "after the initial migration-load")

        saved = send(port, f"return engine.saveWorld('{active_page}', '{resaved_slot}')")
        chk.ok(saved.strip() == "true",
               f"re-saving the session under a new slot succeeded "
               f"(got {saved!r})")
        resaved_path = os.path.join(root, "saves", resaved_slot, "world.synworld")
        for _ in range(100):
            if os.path.isfile(resaved_path):
                break
            time.sleep(0.1)
        chk.ok(os.path.isfile(resaved_path),
               f"re-saved current-format file appeared at {resaved_path}")
        if os.path.isfile(resaved_path):
            resaved_bytes = open(resaved_path, "rb").read()
            # The re-saved bytes must be a genuinely CURRENT-format
            # encode -- never a byte-for-byte copy of the input fixture
            # (that would mean nothing actually re-encoded it).
            chk.ok(resaved_bytes != fixture_bytes,
                   "re-saved file is a real current-format re-encode, not "
                   "a copy of the input fixture's bytes")
            dump_and_compare(chk, tmpdir, resaved_path,
                              fixture["expected_summary"],
                              "after the initial migration-load")

        quit_engine(port, procA)
        procA = None

        # ── Engine B: fresh process, load the RE-SAVED current-format
        #    file, compare structural state across the restart ─────────
        procB = boot_probe(root, port, logB)
        bootstrap_defs(port)
        pre = send(port, "return world.getActiveWorldId()")
        chk.ok(pre in ("nil", "null", '""', ""),
               f"fresh engine B has no pre-load active world (got {pre!r})")

        # The same rule at the fixture scenario's SECOND load boundary:
        # engine B's checks below are exactly as absent-session-blind as
        # engine A's were.
        if not await_load_prerequisite(
                chk, port, resaved_slot,
                accepted=lambda got: (
                    f"engine.loadSave('{resaved_slot}') accepted the "
                    f"re-saved current-format file (got {got!r})"),
                published="re-saved file's load transaction published",
                unreachable=(
                    "engine B's reloaded-session structural checks (active "
                    "page survived the restart, begins paused)",
                    f"this fixture's {lua_checks} declared Lua-state "
                    f"check(s) after the resave/restart/reload round trip",
                    "the second resave from engine B's reloaded state, and "
                    "its real-dumped Haskell state comparison",
                    "the default time-scale check after unpausing",
                    f"this fixture's {live_checks} declared live-tick "
                    f"check(s) in the unpaused reloaded session")):
            return

        active_b = send(port, "return world.getActiveWorldId()").strip('"')
        chk.ok(active_b == active_page,
               f"active page survived the restart -> reload round trip "
               f"(got {active_b!r}, expected {active_page!r})")
        chk.ok(send(port, "return engine.isPaused()") == "true",
               "reloaded session begins paused")

        run_lua_state_checks(chk, port, fixture["lua_state_checks"],
                              "after the resave/restart/reload round trip")

        # A second resave, from engine B's freshly-reloaded state, gives
        # dump_and_compare real bytes reflecting the RELOAD boundary --
        # the first dump (above) only proves migrate-then-resave was
        # correct, never that a restart+reload round trip preserves it.
        resaved_slot_b = f"probe_{fixture['fixture_id']}_resaved_b"
        saved_b = send(port, f"return engine.saveWorld('{active_page}', '{resaved_slot_b}')")
        chk.ok(saved_b.strip() == "true",
               f"re-saving the reloaded session under a new slot "
               f"succeeded (got {saved_b!r})")
        resaved_path_b = os.path.join(root, "saves", resaved_slot_b, "world.synworld")
        for _ in range(100):
            if os.path.isfile(resaved_path_b):
                break
            time.sleep(0.1)
        chk.ok(os.path.isfile(resaved_path_b),
               f"reloaded session's re-saved file appeared at {resaved_path_b}")
        if os.path.isfile(resaved_path_b):
            dump_and_compare(chk, tmpdir, resaved_path_b,
                              fixture["expected_summary"],
                              "after the resave/restart/reload round trip")

        # Unpause. Historically this was ONLY to confirm the default
        # time scale; since #2055 it is also the one moment this probe
        # has a RUNNING migrated session, which is where a fixture's
        # declared live-tick checks are evaluated. Neither compares any
        # random gameplay outcome.
        #
        # The log offset is taken BEFORE unpausing, so the Lua-error
        # scan below sees exactly what this unpaused window produced --
        # a warning from earlier in the same run can neither mask a real
        # failure nor manufacture one.
        try:
            log_offset = os.path.getsize(logB)
        except OSError:
            log_offset = 0
        send(port, "require('scripts.pause').set(false); return 'ok'",
             expect_result=False)
        time.sleep(0.5)
        ts = send(port, f"return world.getTimeScale('{active_page}')")
        chk.ok(ts.strip() in ("1", "1.0"),
               f"unpausing the reloaded session uses the default time "
               f"scale (got {ts})")

        run_live_tick_checks(chk, port, fixture["live_tick_checks"],
                             logB, log_offset)

    finally:
        if procA is not None:
            quit_engine(port, procA)
        if procB is not None:
            quit_engine(port, procB)
        shutil.rmtree(tmpdir, ignore_errors=True)


#: A synthetic startup loader for `self_test`: every shape
#: `production_registry_sequence` must include or exclude, in one file.
#: A neighbouring profile's families, a commented-out call, the tutorial
#: `addItem`, and the texture-only phases must ALL stay out; the
#: addYamlDir/addYamlTree split and the enqueue order must survive.
_SELF_TEST_LOADER = """
local function queueArenaProfile()
    addYamlDir("data/arena",      "Loading arena...",      engine.loadArenaYaml)
end

local function queueNormalProfile()
    addYamlDir("data/materials",  "Loading materials...",  engine.loadMaterialYaml)
    -- addYamlDir("data/retired",  "Loading retired...",   engine.loadRetiredYaml)
    addYamlTree("data/items",     "Loading items...",      engine.loadItemYaml)
    addItem("Loading tutorial...", function()
        engine.loadTutorialDir("data/tutorials")
    end)
    for _, sub in ipairs({ "stat", "skill" }) do
        addTextureDir("assets/textures/icons/" .. sub, "Loading icons...")
    end
    addYamlDir("data/locations",  "Loading locations...",  engine.loadLocationYaml)
end

local function queueLaterProfile()
    addYamlDir("data/later",      "Loading later...",      engine.loadLaterYaml)
end
"""


#: The downstream-stage descriptions `_run_prerequisite_branch` hands
#: `await_load_prerequisite`. Their WORDING is irrelevant to what the
#: self-test proves (which stages are skipped, and that skipping is
#: neither a pass nor a failure), so they stay deliberately generic
#: rather than duplicating run_one_fixture's real ones.
_SELF_TEST_STAGES = ("first downstream stage", "second downstream stage")


def _run_prerequisite_branch(load_response: str, publish_result):
    """Drive `await_load_prerequisite` with no engine, capturing what a
    REAL `Checks` printed (issue #1486).

    `publish_result` is the `(published, status)` pair the injected
    waiter returns, or None to assert the waiter is never called at all.
    Returns `(proceeded, markers, lines, waiter_calls, chk)`, where
    `markers` is the ordered `PASS`/`FAIL`/`SKIP` sequence of the real
    printed lines -- which is how "the failed prerequisite is the last
    check reported, and only SKIP diagnostics follow it" is checked
    without matching prose.
    """
    calls = {"waiter": 0}

    def fake_sender(port, expr):
        assert "engine.loadSave(" in expr, expr
        return load_response

    def fake_waiter(port):
        calls["waiter"] += 1
        if publish_result is None:
            raise AssertionError("the waiter must not be called after a "
                                 "synchronous rejection")
        return publish_result

    chk = Checks()
    buffer = io.StringIO()
    with contextlib.redirect_stdout(buffer):
        proceeded = await_load_prerequisite(
            chk, 0, "probe_slot",
            accepted=lambda got: f"accepted the load (got {got!r})",
            published="the load transaction published",
            unreachable=_SELF_TEST_STAGES,
            sender=fake_sender, waiter=fake_waiter)
    lines = [line.strip() for line in buffer.getvalue().splitlines()
             if line.strip()]
    markers = [line.split("]")[0].lstrip("[") for line in lines]
    return proceeded, markers, lines, calls["waiter"], chk


def self_test() -> int:
    """Prove the bootstrap plan and the parser that checks it, with no
    engine, no GPU and no fixture (issue #1485) -- and both branches of
    the load-prerequisite stop (issue #1486).

    Three parts, all deterministic. The parser is exercised against
    `_SELF_TEST_LOADER`, whose expected answer is written out
    independently of the regex, plus the two ways reading production can
    fail; then the REAL plan is verified against the REAL startup
    loader, which is the assertion that replaces grepping this file for
    loader names; then `await_load_prerequisite` is driven through a
    synchronous rejection, an accepted-but-unpublished load and a clean
    load, via injected doubles -- the branches a real failing fixture
    would otherwise be the only way to observe.
    """
    failures = 0

    def check(cond: bool, label: str) -> None:
        nonlocal failures
        print(f"  [{'PASS' if cond else 'FAIL'}] {label}")
        if not cond:
            failures += 1

    print("=== parsing scripts/startup_loader.lua ===")
    parsed = production_registry_sequence(_SELF_TEST_LOADER)
    expected = [
        ("data/materials", "engine.loadMaterialYaml", False),
        ("data/items",     "engine.loadItemYaml",     True),
        ("data/locations", "engine.loadLocationYaml", False),
    ]
    check(parsed == expected,
          f"reads queueNormalProfile's registry families, in order, with "
          f"the Dir/Tree split -- and excludes the other profiles, the "
          f"commented-out call, the tutorial addItem and the texture "
          f"phases (got {parsed!r})")

    for label, source in (
            ("a startup loader with no queueNormalProfile",
             "local function queueArenaProfile()\nend\n"),
            ("a queueNormalProfile that enqueues no registry family",
             "local function queueNormalProfile()\n"
             "    addTextureList(\"Loading HUD...\", hudPaths)\n"
             "end\n")):
        try:
            production_registry_sequence(source)
        except BootstrapPlanError:
            check(True, f"{label} raises BootstrapPlanError")
        else:
            check(False, f"{label} raises BootstrapPlanError")

    print("\n=== the real bootstrap plan ===")
    problems = verify_bootstrap_plan()
    check(not problems,
          "BOOTSTRAP_LOADERS matches production's queueNormalProfile, "
          "provisions every load-validated registry, ends with "
          "locations, and enumerates files for every family"
          + ("" if not problems else ": " + "; ".join(problems)))

    print("\n=== the Lua update-error marker (#2055) ===")
    marker_problems = verify_lua_error_marker()
    check(not marker_problems,
          f"LUA_UPDATE_ERROR_MARKER ({LUA_UPDATE_ERROR_MARKER!r}) is still "
          f"assembled from wording that exists in the engine sources"
          + ("" if not marker_problems else ": " + "; ".join(marker_problems)))

    print("\n=== the load-prerequisite stop (#1486) ===")

    proceeded, markers, lines, waiter_calls, chk = _run_prerequisite_branch(
        "false", None)
    check(proceeded is False,
          f"a synchronously REJECTED load stops the fixture's scenario "
          f"(got {proceeded!r})")
    check(waiter_calls == 0,
          f"a synchronously rejected load never waits for a publication "
          f"that was never started ({waiter_calls} waiter call(s))")
    check(markers == ["FAIL"] + ["SKIP"] * (1 + len(_SELF_TEST_STAGES)),
          f"the rejection is the LAST check reported, and only SKIP "
          f"diagnostics follow it -- covering the publication check and "
          f"every downstream stage (got {markers!r})")
    check("'false'" in lines[0],
          f"the raw acceptance response is preserved as the failure's "
          f"detail (got {lines[0]!r})")
    check(all("not run:" in line for line in lines[1:]),
          "every skipped stage names the prerequisite that stopped it")
    check((chk.failed, chk.skipped) == (1, 1 + len(_SELF_TEST_STAGES)),
          f"the rejection counts as exactly one FAILED check and the "
          f"skips count as neither passes nor failures (failed="
          f"{chk.failed}, skipped={chk.skipped})")

    for label, status, expected_detail in (
            ("a terminal LoadFailed",
             {"failedAtPhase": "LoadFailed", "outcome": "decode exploded"},
             "decode exploded"),
            ("a terminal LoadReconciliationFailed",
             {"failedAtPhase": "LoadReconciliationFailed", "outcome": "ref"},
             "LoadReconciliationFailed"),
            ("a publication TIMEOUT reporting no status at all",
             None,
             "never reported a terminal status")):
        proceeded, markers, lines, waiter_calls, chk = (
            _run_prerequisite_branch("true", (False, status)))
        check(proceeded is False and waiter_calls == 1,
              f"{label}: an ACCEPTED but unpublished load stops the "
              f"fixture's scenario (got {proceeded!r} after "
              f"{waiter_calls} waiter call(s))")
        check(markers == ["PASS", "FAIL"] + ["SKIP"] * len(_SELF_TEST_STAGES),
              f"{label}: the publication failure is the LAST check "
              f"reported, with the downstream stages skipped after it "
              f"(got {markers!r})")
        check(expected_detail in lines[1],
              f"{label}: the waiter's own returned status is preserved in "
              f"the failure (expected {expected_detail!r} in {lines[1]!r})")
        check((chk.failed, chk.skipped) == (1, len(_SELF_TEST_STAGES)),
              f"{label}: one FAILED check, and skips that are neither "
              f"passes nor failures (failed={chk.failed}, "
              f"skipped={chk.skipped})")

    proceeded, markers, lines, waiter_calls, chk = _run_prerequisite_branch(
        "true", (True, {"phase": "LoadPublished"}))
    check(proceeded is True and waiter_calls == 1,
          f"an accepted, published load lets the fixture's scenario "
          f"continue (got {proceeded!r})")
    check(markers == ["PASS", "PASS"] and (chk.failed, chk.skipped) == (0, 0),
          f"a successful boundary reports exactly the two checks it "
          f"always did, with nothing skipped (got {markers!r}, failed="
          f"{chk.failed}, skipped={chk.skipped})")

    print(f"\n{'PASS' if failures == 0 else 'FAIL'}: {failures} "
          f"self-test check(s) failed")
    return 0 if failures == 0 else 1


def main() -> int:
    ap = argparse.ArgumentParser(description=__doc__,
                                 formatter_class=argparse.RawDescriptionHelpFormatter)
    ap.add_argument("--port", type=int, default=9276)
    ap.add_argument("--self-test", action="store_true",
                    help="verify the registry bootstrap plan and the "
                         "startup-loader parser behind it; boots no engine")
    args = ap.parse_args()

    if args.self_test:
        return self_test()

    # Before any engine boots: a bootstrap that has stopped provisioning
    # a family the fixtures reference would otherwise surface only as
    # whichever load happens to be rejected first, far from the cause
    # and an engine boot at a time (issue #1485).
    problems = verify_bootstrap_plan()
    if problems:
        print("FAIL: the registry bootstrap plan is unusable, so no "
              "fixture could be provisioned correctly:")
        for problem in problems:
            print(f"  - {problem}")
        return 1
    print(f"registry bootstrap: {len(BOOTSTRAP_LOADERS)} families "
          f"mirroring startup_loader.queueNormalProfile, locations last")

    # Same rule, same moment, for the other thing this probe reads out of
    # production rather than restating: a live-tick fixture's Lua-error
    # scan is worthless if the engine's wording has moved (#2055).
    marker_problems = verify_lua_error_marker()
    if marker_problems:
        print("FAIL: the Lua update-error marker no longer matches the "
              "engine, so a failing live tick could pass unnoticed:")
        for problem in marker_problems:
            print(f"  - {problem}")
        return 1

    fixtures = declared_complete_session_fixtures()
    chk = Checks()
    for fixture in fixtures:
        run_one_fixture(chk, args.port, fixture)

    print(f"\n{'PASS' if chk.failed == 0 else 'FAIL'}: "
          f"{chk.failed} check(s) failed across {len(fixtures)} declared "
          f"complete-session fixture(s)"
          + (f", and {chk.skipped} check(s) were skipped after a failed "
             f"load prerequisite" if chk.skipped else ""))
    # A skipped check is neither a pass nor a failure (#1486): the exit
    # status is still driven purely by real failed checks -- of which a
    # fixture whose load prerequisite failed always has at least one.
    return 0 if chk.failed == 0 else 1


if __name__ == "__main__":
    sys.exit(main())
