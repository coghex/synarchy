#!/usr/bin/env python3
"""Save-compatibility audit + fixture registration tool (issue #766,
save-overhaul C4, requirements 13/14).

Guards docs/save_compat/manifest.json -- the machine-readable record of
every save-format baseline this build is declared to keep loadable
through explicit migrations -- against silent drift:

  - Every fixture the manifest declares actually exists on disk.
  - A tracked binary fixture's bytes have not been hand-edited (its
    sha256 matches the manifest's recorded value).
  - The manifest's envelopeFramingVersion agrees with
    World.Save.Envelope.currentEnvelopeVersion -- a framing bump without
    an explicit manifest update (a new format epoch) fails loudly rather
    than silently reinterpreting every tracked fixture under a changed
    contract.
  - Every baseline's frozen-DTO source (World.Save.Compat.SessionV90)
    fingerprint matches what the manifest recorded when that baseline was
    declared -- refactoring a frozen type changes its fingerprint, so an
    editor must consciously re-run --add-baseline (or acknowledge the
    change) rather than silently altering historical bytes.
  - Every baseline's declared components[] cross-checks against the REAL
    current Haskell (World.Save.Component.*'s ccVersion/ccInputVers/
    ccRequired) and Lua (scripts/unit_ai_save.lua's/scripts/
    building_spawn.lua's version/inputVersions/required) registries (see
    real_component_registry() / audit_component_versions()): a declared
    component/version must still exist and still be an accepted input
    version (catches a decoder silently dropped); every REQUIRED
    component -- regardless of how many versions it accepts -- must be
    tracked by at least one baseline (catches a brand-new required
    component shipping with no baseline ever proving it has an accounted
    default/migration policy, which a version-count-only check can never
    see for a component that has only ever had one version); and every
    component with more than one accepted input version must additionally
    have its OLDEST one tracked by some baseline (catches a version bump
    that shipped with no compatibility fixture ever validating the
    historical shape it migrates from).
  - Round-6 review's per-baseline (not merely aggregate-across-baselines)
    required-component coverage (see audit_modern_baseline_components_
    complete() / audit_b1_migration_covers_page_scoped_components()):
    every "current"-target baseline whose components[] doesn't declare
    the frozen legacy "session" id is a MODERN per-component-registry
    session, and a valid one of those can never structurally omit any
    required component (decodeEnvelope refuses an incomplete modern
    envelope outright) -- so its components[] must declare ALL of them,
    or the manifest is under-documenting what its own fixture actually
    contains. The b1-initial-session baseline can never declare that
    full set (it IS the frozen {metadata, session} alternative), so its
    real guarantee is checked differently: World.Save.Compat.SessionV90.
    migrateSessionV90's own source must still reference the named apply*
    helper for every current page-scoped component -- the closest a
    static Python audit can get to "this legacy migration still threads
    every required component through", short of literally compiling it.

This is a static presence/fingerprint check, not itself a proof that a
fixture migrates correctly -- that real decode/migrate/assemble/
canonical-result cross-check (requirement 14) lives in test-headless's
"save migrations" hspec gate ("manifest-declared fixtures decode and
migrate to their expected canonical result", which reads this SAME
manifest and every fixture/expectedCanonicalSummary it declares), backed
by tools/save_compat_migration_probe.py's real-engine round trip. Run:
cabal test synarchy-test-headless --test-options='--match "save migrations"'

Usage:
  python3 tools/save_compat_audit.py                # blocking audit (CI)

  # Register a fixture on an EXISTING baseline (checksum + summary,
  # atomically):
  python3 tools/save_compat_audit.py --add-baseline \\
      --baseline-id b1-initial-session --fixture-id my-fixture \\
      --path test-headless/data/save-compat/my-fixture.bin \\
      --kind complete-session \\
      --summary test-headless/data/save-compat/my-fixture.expected.json

  # Register a fixture AND create its baseline entry together (id not
  # yet declared):
  python3 tools/save_compat_audit.py --add-baseline \\
      --baseline-id my-new-baseline --fixture-id my-fixture \\
      --path test-headless/data/save-compat/my-fixture.bin \\
      --kind complete-session \\
      --summary test-headless/data/save-compat/my-fixture.expected.json \\
      --description "..." --migration-target current \\
      --migrated-by "World.Save.Compat.SessionV90.migrateSessionV90" \\
      --components '[{"id":"metadata","version":1,"required":true}, ...]'

  Either form refuses to overwrite an already-registered fixture id
  without --force. The raw fixture BYTES and --summary JSON must already
  exist (generated through the real codec -- see the manifest's own
  "provenance" fields for worked examples, and tools/README.md /
  docs/save_compat -- for a Haskell "complete-session" fixture that
  means a real headless-engine boot + engine.saveWorld, or a GHCi/cabal
  repl session calling World.Save.Envelope.Codec.encodeEnvelope
  directly; for a Lua "component-focused" fixture, a GHCi/cabal repl
  session driving a real HsLua VM through scripts/lib/data_codec.lua's
  M.encode -- see test-headless/data/save-compat/lua-unit-ai-v1.bin's
  manifest provenance for a worked example); this command performs the
  atomic bookkeeping (checksum, size, manifest/summary wiring) AND, for
  a "complete-session" fixture, immediately runs it through the SAME
  real codec test-headless's CI gate uses (cabal test
  synarchy-test-headless --test-options='--match "save migrations"'),
  automatically rolling the manifest back to its exact prior content if
  that fails -- so a bad fixture registration is never left committed
  even locally. Pass --skip-validation to register without running that
  check (e.g. no cabal toolchain available here); a "component-focused"
  fixture has no generic gate to run this way at all (see
  _finalize_manifest_write's docstring) and needs its own hand-written
  hspec test instead.

  # GENERATE a brand-new CURRENT-format complete-session fixture through
  # the real codec end to end (requirement 21: a real generation mode,
  # not just validation of already-hand-built bytes) -- boots an actual
  # headless engine, inits a world, optionally spawns ONE building and/
  # or ONE unit, calls engine.saveWorld (the SAME production save path
  # real gameplay uses), then derives its canonical summary DIRECTLY
  # from the real decoded snapshot (see dump_canonical_summary) rather
  # than hand-transcribing values -- then registers + validates exactly
  # like --add-baseline above (this literally delegates to it once the
  # bytes/summary exist):
  python3 tools/save_compat_audit.py --generate-session \\
      --baseline-id my-new-baseline --fixture-id my-fixture \\
      --path test-headless/data/save-compat/my-fixture.bin \\
      --summary test-headless/data/save-compat/my-fixture.expected.json \\
      --seed 42 --world-size 8 --plate-count 3 \\
      --spawn-building cargo_hold_S --spawn-unit acolyte \\
      --description "..." --migration-target current \\
      --migrated-by "..." --components '[...]'

  This can only ever produce a fixture at the CURRENT wire format -- a
  live engine never writes a historical shape. A baseline documenting an
  OLDER version (a frozen legacy DTO, or a component spliced back to an
  earlier ccInputVers) is inherently a distinct, bespoke operation (there
  is no "generate a v1 payload" button in the live game either), and
  stays the manual decode/splice-then---add-baseline workflow this
  manifest's own fixtures' "provenance" fields document (see
  b3-lua-versioned-session-v1 for the most recent worked example).

  Stages the fixture, its summary, AND the manifest together (round-6
  review): a failure at ANY stage -- generation, canonical-summary
  derivation, or the real-codec registration/validation --add-baseline
  itself runs -- restores ALL THREE to their exact prior state (or
  removes whichever ones didn't exist before this invocation), never
  leaving an orphaned or stale-but-checksum-mismatched file behind.

Exit codes: 0 = every declared fixture/fingerprint is intact,
1 = one or more violations (see printed detail).
"""
from __future__ import annotations

import argparse
import hashlib
import json
import os
import re
import shutil
import subprocess
import sys
import tempfile
import time
from pathlib import Path

REPO_ROOT = Path(__file__).resolve().parent.parent
MANIFEST_PATH = REPO_ROOT / "docs" / "save_compat" / "manifest.json"
ENVELOPE_SOURCE_PATH = REPO_ROOT / "src" / "World" / "Save" / "Envelope.hs"
SESSION_V90_SOURCE_PATH = (
    REPO_ROOT / "src" / "World" / "Save" / "Compat" / "SessionV90.hs")

# Every source file that declares a Haskell-owned gameplay component's
# ComponentId literal and/or its ComponentCodec (serializeCodec or a
# hand-rolled multi-version record) -- see real_component_registry().
HASKELL_COMPONENT_SOURCE_PATHS = [
    REPO_ROOT / "src" / "World" / "Save" / "Component" / "Types.hs",
    REPO_ROOT / "src" / "World" / "Save" / "Component" / "Session.hs",
    REPO_ROOT / "src" / "World" / "Save" / "Component" / "Page.hs",
    REPO_ROOT / "src" / "World" / "Save" / "Component" / "Entities.hs",
]
LUA_UNIT_AI_SOURCE_PATH = REPO_ROOT / "scripts" / "unit_ai_save.lua"
LUA_BUILDING_SPAWN_SOURCE_PATH = REPO_ROOT / "scripts" / "building_spawn.lua"

CURRENT_ENVELOPE_VERSION_RE = re.compile(
    r"^currentEnvelopeVersion\s*=\s*(\d+)", re.MULTILINE)
METADATA_COMPONENT_VERSION_RE = re.compile(
    r"^metadataComponentVersion\s*=\s*(\d+)", re.MULTILINE)
SESSION_COMPONENT_VERSION_RE = re.compile(
    r"^sessionComponentVersion\s*=\s*(\d+)", re.MULTILINE)
COMPONENT_ID_LITERAL_RE = re.compile(
    r"(\w+)\s*=\s*ComponentId\s*\"([^\"]+)\"")
SERIALIZE_CODEC_RE = re.compile(
    r"(\w+)\s*=\s*serializeCodec\s*\n?\s*(\w+)\s+(\d+)\s+(True|False)")
RECORD_CODEC_RE = re.compile(
    r"ccId\s*=\s*(\w+)\s*\n\s*,\s*ccVersion\s*=\s*(\d+)\s*\n\s*,\s*"
    r"ccInputVers\s*=\s*\[([^\]]*)\]\s*\n\s*,\s*ccRequired\s*=\s*(True|False)")
LUA_MODULE_VERSION_RE = re.compile(r"\bversion\s*=\s*(\d+)")
LUA_MODULE_INPUT_VERSIONS_RE = re.compile(
    r"\binputVersions\s*=\s*\{([^}]*)\}")
LUA_MODULE_REQUIRED_RE = re.compile(r"\brequired\s*=\s*(true|false)")


def load_manifest(path: Path = MANIFEST_PATH) -> dict:
    return json.loads(path.read_text(encoding="utf-8"))


def frozen_dto_fingerprint(source_path: Path = SESSION_V90_SOURCE_PATH) -> str:
    """A stable fingerprint over the frozen DTO type declarations in
    World.Save.Compat.SessionV90 -- every `data ... = ...` block up to
    its closing `deriving` line. Comment/haddock changes don't move this
    (a documentation-only edit shouldn't force a manifest update);
    reordering, adding, or removing a FIELD does, since that changes the
    positional cereal wire layout requirement 10 is guarding."""
    text = source_path.read_text(encoding="utf-8")
    blocks = re.findall(
        r"^data \w+ = \w+.*?deriving\s*\([^)]*\)", text,
        re.MULTILINE | re.DOTALL)
    if not blocks:
        raise ValueError(
            f"no frozen `data ... deriving (...)` blocks found in "
            f"{source_path} -- did the module get restructured?")

    def normalize(block: str) -> str:
        no_comments = "\n".join(
            re.sub(r"--.*$", "", line) for line in block.splitlines())
        return re.sub(r"\s+", " ", no_comments).strip()

    normalized = "\n---\n".join(normalize(b) for b in blocks)
    return hashlib.sha256(normalized.encode("utf-8")).hexdigest()


def current_envelope_version(path: Path = ENVELOPE_SOURCE_PATH) -> int:
    text = path.read_text(encoding="utf-8")
    m = CURRENT_ENVELOPE_VERSION_RE.search(text)
    if not m:
        raise ValueError(f"could not find currentEnvelopeVersion in {path}")
    return int(m.group(1))


def real_component_registry() -> dict[str, dict]:
    """Every save component this build's REAL source currently declares,
    id -> {"currentVersion": int, "inputVersions": [int, ...],
    "required": bool} -- parsed directly from the Haskell/Lua source, not
    hand-maintained here, so a schema bump, a removed historical decoder,
    or a newly required component shows up the next time this runs, not
    only when someone remembers to update this tool too (requirement 19's
    version cross-check).

    Raises if a source file's expected declarations can't be found at
    all (the parser itself is stale, e.g. after a real rename) -- that
    must fail loudly, not silently audit against an empty registry."""
    registry: dict[str, dict] = {}

    envelope_text = ENVELOPE_SOURCE_PATH.read_text(encoding="utf-8")
    m = METADATA_COMPONENT_VERSION_RE.search(envelope_text)
    if not m:
        raise ValueError(
            f"could not find metadataComponentVersion in {ENVELOPE_SOURCE_PATH}")
    # metadata is unconditionally required -- every envelope carries it
    # (World.Save.Envelope's own decode refuses one that doesn't).
    registry["metadata"] = {
        "currentVersion": int(m.group(1)), "inputVersions": [int(m.group(1))],
        "required": True}

    # "session" is the ONE frozen legacy component (World.Save.Compat.
    # SessionV90) -- its current version is its only ever version; a
    # further schema change adds a new frozen type instead of bumping
    # this one (the frozen-DTO boundary rule), so inputVersions is
    # always the singleton [currentVersion]. Unconditionally required --
    # it IS the whole legacy envelope's one gameplay component.
    session_text = SESSION_V90_SOURCE_PATH.read_text(encoding="utf-8")
    m = SESSION_COMPONENT_VERSION_RE.search(session_text)
    if not m:
        raise ValueError(
            f"could not find sessionComponentVersion in {SESSION_V90_SOURCE_PATH}")
    registry["session"] = {
        "currentVersion": int(m.group(1)), "inputVersions": [int(m.group(1))],
        "required": True}

    id_literals: dict[str, str] = {}
    for path in HASKELL_COMPONENT_SOURCE_PATHS:
        text = path.read_text(encoding="utf-8")
        for ident, sid in COMPONENT_ID_LITERAL_RE.findall(text):
            id_literals[ident] = sid

    for path in HASKELL_COMPONENT_SOURCE_PATHS:
        text = path.read_text(encoding="utf-8")
        for _codec_name, cid_ident, ver, req in SERIALIZE_CODEC_RE.findall(text):
            sid = id_literals.get(cid_ident)
            if sid is None:
                raise ValueError(
                    f"{path}: serializeCodec references unknown component "
                    f"id identifier '{cid_ident}' -- did a ComponentId "
                    f"binding get renamed without updating this parser?")
            registry[sid] = {
                "currentVersion": int(ver), "inputVersions": [int(ver)],
                "required": req == "True"}
        for cid_ident, ver, vers_str, req in RECORD_CODEC_RE.findall(text):
            sid = id_literals.get(cid_ident)
            if sid is None:
                raise ValueError(
                    f"{path}: hand-rolled ComponentCodec references unknown "
                    f"component id identifier '{cid_ident}'")
            input_versions = [int(v.strip()) for v in vers_str.split(",")
                               if v.strip()]
            registry[sid] = {
                "currentVersion": int(ver), "inputVersions": input_versions,
                "required": req == "True"}

    for path, lua_id in [(LUA_UNIT_AI_SOURCE_PATH, "unit_ai"),
                          (LUA_BUILDING_SPAWN_SOURCE_PATH, "building_spawn")]:
        text = path.read_text(encoding="utf-8")
        vm = LUA_MODULE_VERSION_RE.search(text)
        ivm = LUA_MODULE_INPUT_VERSIONS_RE.search(text)
        rm = LUA_MODULE_REQUIRED_RE.search(text)
        if not vm or not ivm or not rm:
            raise ValueError(
                f"could not find version/inputVersions/required in {path} "
                f"-- did saveMods.register('{lua_id}', {{...}})'s "
                f"declaration shape change?")
        input_versions = [int(v.strip()) for v in ivm.group(1).split(",")
                           if v.strip()]
        registry[f"lua.{lua_id}"] = {
            "currentVersion": int(vm.group(1)), "inputVersions": input_versions,
            "required": rm.group(1) == "true"}

    return registry


def audit_component_versions(manifest: dict, real_registry: dict) -> list[str]:
    """Cross-check every baseline's declared components[] against the
    REAL current registry (requirement 19):

      - a declared component id must still exist in the real registry
        (catches a component renamed/removed without retiring or
        updating the baseline that references it);
      - a declared version must still be one of the real codec's
        accepted input versions (catches "removal of a declared
        decoder" -- ccInputVers/inputVersions shrinking out from under
        a tracked historical fixture);
      - every REQUIRED component (round-3 review: reads ccRequired/Lua's
        required flag, not just version counts) must be tracked by AT
        LEAST ONE baseline's components[], regardless of how many
        input versions it accepts -- a brand-new required component
        with a single input version was previously invisible to this
        audit entirely (it never has ">1 input version", the ONLY case
        the prior check looked at), so it could ship with genuinely NO
        baseline ever proving it has an accounted default/migration
        policy for a session that predates it, while this audit still
        passed. An OPTIONAL component has no such obligation -- a
        legacy save legitimately lacking it is exactly requirement 9's
        "unknown/absent optional component" case, not a compatibility
        gap;
      - every component with more than one accepted input version (a
        real migration exists) must have its OLDEST accepted version
        tracked by at least one baseline's components[] -- proving
        that historical migration is exercised by a real fixture, not
        merely present in code (catches "a component-version bump
        without a migration/frozen DTO" ever being validated)."""
    violations: list[str] = []
    tracked_versions: dict[str, set[int]] = {}

    for baseline in manifest.get("baselines", []):
        bid = baseline.get("id")
        for comp in baseline.get("components", []):
            comp_id = comp.get("id")
            comp_ver = comp.get("version")
            tracked_versions.setdefault(comp_id, set()).add(comp_ver)
            real = real_registry.get(comp_id)
            if real is None:
                violations.append(
                    f"baseline '{bid}' declares component '{comp_id}', which "
                    f"no longer exists in the real component registry -- a "
                    f"component was renamed or removed without retiring or "
                    f"updating this baseline")
                continue
            if comp_ver not in real["inputVersions"]:
                violations.append(
                    f"baseline '{bid}' declares component '{comp_id}' at "
                    f"version {comp_ver}, but the real codec's currently "
                    f"accepted input versions are "
                    f"{sorted(real['inputVersions'])} -- support for that "
                    f"historical version was removed without retiring or "
                    f"updating this baseline")

    for comp_id, real in real_registry.items():
        tracked = tracked_versions.get(comp_id, set())
        if real.get("required") and not tracked:
            violations.append(
                f"component '{comp_id}' is REQUIRED (accepted input "
                f"versions {sorted(real['inputVersions'])}) but is not "
                f"tracked by ANY manifest baseline's components[] -- a "
                f"required component with no baseline exercising it has "
                f"no proof its default/migration policy for a session "
                f"predating it was ever considered; add it to some "
                f"baseline's components[] (backed by a fixture covering "
                f"it) before this gap ships")
            continue
        if len(real["inputVersions"]) <= 1:
            continue
        oldest = min(real["inputVersions"])
        if oldest not in tracked:
            violations.append(
                f"component '{comp_id}' accepts input versions "
                f"{sorted(real['inputVersions'])} (a migration exists from "
                f"v{oldest}), but no manifest baseline declares/tracks a "
                f"fixture at v{oldest} -- that historical migration has no "
                f"tracked compatibility coverage")

    return violations


# The b1-initial-session baseline's ONLY migration path is
# World.Save.Compat.SessionV90.migrateSessionV90, which threads every
# page-scoped modern component's construction through one of these named
# helpers (see the function's own source: `base = basePageSnapshots
# (...); afterEdits <- applyWorldEdits 1 (...) base; ...`) -- global
# fields (core-session's allocators/camera, texture-palette) are instead
# built directly as part of the one SessionSnapshot record literal
# GHC already forces to be total, so THEY can never silently go
# unconstructed the way a forgotten apply* call for a NEW page-scoped
# component could.
SESSION_V90_APPLY_HELPER_FOR_COMPONENT = {
    "world-edits":    "applyWorldEdits",
    "world-activity": "applyWorldActivity",
    "buildings":      "applyBuildings",
    "units":          "applyUnits",
    "unit-sim":       "applyUnitSim",
    "craft-bills":    "applyCraftBills",
    "power-nodes":    "applyPowerNodes",
}


def audit_b1_migration_covers_page_scoped_components(
        source_path: Path = SESSION_V90_SOURCE_PATH) -> list[str]:
    """Requirement 5 (issue #766): "introducing a new required component
    requires a migration/default policy for every supported older
    baseline". The b1-initial-session baseline can never simply declare
    coverage for a page-scoped component in its manifest components[]
    (it is structurally {metadata, session} only) -- its ACTUAL
    guarantee lives in migrateSessionV90's source threading every
    current page-scoped component through its own named apply* helper.
    This is the closest thing a static Python audit can check without
    literally compiling Haskell: if a future required page-scoped
    component's helper name isn't referenced anywhere in this file,
    something was renamed/removed/forgotten with nothing left to prove
    B1 sessions still migrate it."""
    text = source_path.read_text(encoding="utf-8")
    return [
        f"World.Save.Compat.SessionV90.migrateSessionV90 (the "
        f"b1-initial-session baseline's ONLY migration path) no longer "
        f"references '{helper}' for component '{comp_id}' -- a new "
        f"required page-scoped component, or one whose construction "
        f"helper was renamed/removed, would silently have no accounted "
        f"default/migration policy for a session predating it"
        for comp_id, helper in SESSION_V90_APPLY_HELPER_FOR_COMPONENT.items()
        if helper not in text
    ]


def audit_modern_baseline_components_complete(
        manifest: dict, real_registry: dict) -> list[str]:
    """Requirement 5's other half: a baseline that is NOT b1-shaped (its
    components[] doesn't declare the frozen legacy "session" component)
    is, by construction, a MODERN per-component-registry session -- and
    every one of those components (except "session" itself, which is
    B1-exclusive: a session is either the frozen legacy blob or the
    modern per-component split, never both) is unconditionally REQUIRED
    (decodeEnvelope refuses a modern envelope missing one outright, see
    componentRequiredIds/MissingRequiredComponent). So a valid tracked
    "current"-target modern-shaped fixture cannot possibly omit any of
    them -- if a baseline's own components[] doesn't declare one, that
    baseline's manifest entry is under-documenting what its own fixture
    genuinely contains, precisely the gap round-5 review flagged (a
    future required component could be added to only ONE such baseline
    and never show up as a coverage gap in the OTHERS, since the
    all-baselines-aggregate check alone can't see that)."""
    modern_required_ids = {
        cid for cid, info in real_registry.items()
        if info.get("required") and cid != "session"
    }
    violations: list[str] = []
    for baseline in manifest.get("baselines", []):
        bid = baseline.get("id")
        declared_ids = {c.get("id") for c in baseline.get("components", [])}
        if "session" in declared_ids:
            continue  # b1-shaped: the frozen legacy alternative, exempt
        if baseline.get("migrationTarget") != "current":
            continue  # e.g. decode-only historical evidence, not a
                       # migration-acceptance baseline at all
        missing = sorted(modern_required_ids - declared_ids)
        if missing:
            violations.append(
                f"baseline '{bid}' is modern-shaped (its components[] "
                f"omits the legacy 'session' id) and targets 'current', "
                f"but a valid modern complete-session fixture cannot "
                f"structurally omit a required component -- yet its "
                f"components[] doesn't declare {missing}. Either this "
                f"baseline's own tracked fixture genuinely lacks them "
                f"(impossible for a real modern session) or its "
                f"components[] list is under-documented relative to "
                f"what the fixture actually contains -- add them")
    return violations


def _iter_fixtures(manifest: dict):
    for baseline in manifest.get("baselines", []):
        for fixture in baseline.get("fixtures", []):
            yield baseline, fixture


def audit(manifest: dict) -> list[str]:
    violations: list[str] = []

    declared_framing = manifest.get("envelopeFramingVersion")
    real_framing = current_envelope_version()
    if declared_framing != real_framing:
        violations.append(
            f"manifest envelopeFramingVersion ({declared_framing}) disagrees "
            f"with World.Save.Envelope.currentEnvelopeVersion ({real_framing}) "
            f"-- a framing bump is a new format epoch and must update the "
            f"manifest deliberately, never silently")

    declared_fingerprint = manifest.get("frozenDtoFingerprint")
    real_fingerprint = frozen_dto_fingerprint()
    if declared_fingerprint != real_fingerprint:
        violations.append(
            f"manifest frozenDtoFingerprint ({declared_fingerprint}) disagrees "
            f"with the current World.Save.Compat.SessionV90 frozen-DTO field "
            f"layout ({real_fingerprint}) -- a field was added/removed/"
            f"reordered on an already-shipped frozen DTO (requirement 10), or "
            f"the manifest needs a deliberate update alongside the change")

    real_registry = real_component_registry()
    violations.extend(audit_component_versions(manifest, real_registry))
    violations.extend(audit_modern_baseline_components_complete(manifest, real_registry))
    violations.extend(audit_b1_migration_covers_page_scoped_components())

    for baseline, fixture in _iter_fixtures(manifest):
        fid = fixture.get("id", "<unnamed>")
        path_str = fixture.get("path")
        if not path_str:
            violations.append(
                f"baseline '{baseline.get('id')}' fixture '{fid}' has no path")
            continue
        fpath = REPO_ROOT / path_str
        if not fpath.exists():
            violations.append(
                f"baseline '{baseline.get('id')}' fixture '{fid}' path "
                f"'{path_str}' does not exist")
            continue
        expected_sha = fixture.get("sha256")
        if expected_sha is None:
            # A component-focused fixture recorded as inline source (e.g.
            # recovered git history embedded as a hex literal) rather than
            # a tracked binary blob -- nothing to checksum here; its own
            # provenance field is the audit trail.
            continue
        actual_sha = hashlib.sha256(fpath.read_bytes()).hexdigest()
        if actual_sha != expected_sha:
            violations.append(
                f"baseline '{baseline.get('id')}' fixture '{fid}' at "
                f"'{path_str}' has drifted: sha256 {actual_sha} != manifest's "
                f"recorded {expected_sha} -- tracked fixtures must never be "
                f"hand-edited; regenerate through the real codec and "
                f"re-register with --add-baseline")
            continue
        expected_size = fixture.get("sizeBytes")
        actual_size = fpath.stat().st_size
        if expected_size is not None and expected_size != actual_size:
            violations.append(
                f"baseline '{baseline.get('id')}' fixture '{fid}' size "
                f"{actual_size} != manifest's recorded {expected_size}")

        summary_path_str = fixture.get("expectedCanonicalSummary")
        if summary_path_str:
            summary_path = REPO_ROOT / summary_path_str
            if not summary_path.exists():
                violations.append(
                    f"baseline '{baseline.get('id')}' fixture '{fid}' "
                    f"declares expectedCanonicalSummary '{summary_path_str}' "
                    f"which does not exist")

    for baseline in manifest.get("baselines", []):
        if not baseline.get("fixtures"):
            violations.append(
                f"baseline '{baseline.get('id')}' has no fixtures -- every "
                f"declared baseline needs at least one (requirement 14)")

    return violations


def cmd_audit(args: argparse.Namespace) -> int:
    manifest = load_manifest(MANIFEST_PATH)
    violations = audit(manifest)
    if violations:
        print(f"{len(violations)} save-compatibility violation(s):")
        for v in violations:
            print(f"  - {v}")
        return 1
    n_baselines = len(manifest.get("baselines", []))
    n_fixtures = sum(len(b.get("fixtures", [])) for b in manifest.get("baselines", []))
    print(f"save-compatibility audit: {n_baselines} baseline(s), "
          f"{n_fixtures} fixture(s) all intact")
    return 0


def _write_manifest_atomically(manifest: dict, manifest_path: Path = MANIFEST_PATH) -> None:
    """Write the manifest via a same-directory temp file + atomic rename,
    so a crash/interruption mid-write can never leave a half-written,
    unparseable manifest.json behind."""
    tmp = manifest_path.with_name(manifest_path.name + ".tmp")
    tmp.write_text(json.dumps(manifest, indent=2) + "\n", encoding="utf-8")
    tmp.replace(manifest_path)


def _build_fixture_entry(args: argparse.Namespace) -> dict:
    fpath = REPO_ROOT / args.path
    if not fpath.exists():
        raise SystemExit(f"path '{args.path}' does not exist -- generate the "
                          f"fixture through the real codec FIRST (see "
                          f"docs/save_compat/manifest.json's own "
                          f"'provenance' fields for worked examples), then "
                          f"run this command to register it")
    data = fpath.read_bytes()
    entry = {
        "id": args.fixture_id,
        "path": args.path,
        "kind": args.kind,
        "sha256": hashlib.sha256(data).hexdigest(),
        "sizeBytes": len(data),
        "provenance": args.provenance or "(fill in: how was this fixture generated?)",
        "expectedCanonicalSummary": None,
    }
    if args.summary:
        summary_path = REPO_ROOT / args.summary
        if not summary_path.exists():
            raise SystemExit(f"--summary path '{args.summary}' does not "
                              f"exist -- author the expected-canonical-"
                              f"summary JSON first (see an existing "
                              f"*.expected.json for the schema), then "
                              f"register together")
        try:
            json.loads(summary_path.read_text(encoding="utf-8"))
        except ValueError as e:
            raise SystemExit(f"--summary path '{args.summary}' is not valid "
                              f"JSON: {e}")
        entry["expectedCanonicalSummary"] = args.summary
    elif args.kind == "complete-session":
        raise SystemExit("a 'complete-session' fixture needs --summary "
                          "(requirement 12/14: every complete-session "
                          "fixture must have an expected canonical result "
                          "to validate against)")
    return entry


def _run_real_codec_validation() -> tuple[bool, str]:
    """Run the SAME "save migrations" hspec gate CI already runs
    (test-headless's manifest-driven "manifest-declared fixtures decode
    and migrate to their expected canonical result" test), scoped via
    --match: the real, already-existing proof that every complete-
    session fixture the manifest declares actually decodes/migrates/
    assembles correctly through World.Save.Envelope/Component's real
    registry and matches its own expectedCanonicalSummary -- not merely
    that its bytes are present and checksummed (which is all the rest of
    this module's audit() can ever prove, since only Haskell can run the
    cereal codec). Returns (passed, last-40-lines-of-output)."""
    try:
        proc = subprocess.run(
            ["cabal", "test", "synarchy-test-headless",
             "--test-options=--match \"save migrations\""],
            cwd=REPO_ROOT, capture_output=True, text=True, timeout=1800)
    except FileNotFoundError:
        return False, ("'cabal' was not found on PATH -- cannot validate "
                        "through the real codec in this environment; pass "
                        "--skip-validation if that is expected here (the "
                        "checked-in CI gate will still catch a bad fixture "
                        "on the next push)")
    output = (proc.stdout or "") + (proc.stderr or "")
    tail = "\n".join(output.splitlines()[-40:])
    return proc.returncode == 0, tail


def _finalize_manifest_write(
        manifest: dict, manifest_path: Path, kind: str,
        skip_validation: bool, success_message: str) -> int:
    """Write the manifest, then -- for a "complete-session" fixture,
    unless --skip-validation was passed -- run it through the REAL
    codec (requirement 21: "an explicit real-codec generation mode/
    helper that validates the output"), rolling the manifest back to its
    exact prior content if that validation fails. A "component-focused"
    fixture (e.g. a single Lua component payload) has no generic gate to
    run -- test-headless's manifest-driven test only ever iterates
    complete-session fixtures -- so this is a documented no-op for that
    kind: write a hand-authored hspec test exercising it directly
    instead (see Test.Headless.Lua.SaveModules's "tracked v1 fixtures
    from disk" for the pattern this repo already follows)."""
    previous_text = (manifest_path.read_text(encoding="utf-8")
                      if manifest_path.exists() else None)
    _write_manifest_atomically(manifest, manifest_path)

    if kind != "complete-session":
        print(f"{success_message}\n(kind='{kind}': no generic real-codec "
              f"validation gate exists for this kind -- add or extend a "
              f"hand-written hspec test exercising it directly, mirroring "
              f"Test.Headless.Lua.SaveModules's \"tracked v1 fixtures from "
              f"disk\" pattern, then run it yourself)")
        return 0
    if skip_validation:
        print(f"{success_message}\n(--skip-validation passed: NOT run "
              f"through the real codec here -- the checked-in CI gate "
              f"will still catch a bad fixture on the next push)")
        return 0

    ok, tail = _run_real_codec_validation()
    if ok:
        print(f"{success_message}\nvalidated through the real codec: "
              f"cabal test synarchy-test-headless --test-options="
              f"'--match \"save migrations\"' PASSED")
        return 0

    if previous_text is None:
        manifest_path.unlink()
    else:
        manifest_path.write_text(previous_text, encoding="utf-8")
    print(f"REAL-CODEC VALIDATION FAILED -- rolled '{manifest_path}' back "
          f"to its previous content (the fixture bytes/summary files "
          f"already on disk were left untouched; only this registration "
          f"was undone). Fix the fixture and re-run --add-baseline. Last "
          f"lines of `cabal test`'s output:\n{tail}", file=sys.stderr)
    return 1


class GenerationError(Exception):
    """A real-engine fixture-generation step failed (requirement 21)."""


def _make_isolated_gen_root(base: str) -> str:
    """A throwaway resource root: real scripts/assets/data/config
    (symlinked -- read-only content, safe to share) plus its OWN empty
    saves/ directory -- mirrors tools/save_compat_migration_probe.py's
    make_isolated_root/tools/save_storage_probe.py's own helper, so a
    generated fixture never touches a real player's saves."""
    root = os.path.join(base, "root")
    os.makedirs(root, exist_ok=True)
    for family in ("scripts", "assets", "data", "config"):
        target = os.path.join(root, family)
        if not os.path.exists(target):
            os.symlink(os.path.join(REPO_ROOT, family), target)
    os.makedirs(os.path.join(root, "saves"), exist_ok=True)
    return root


def _bootstrap_gen_defs(send, port: int) -> None:
    """Load the defs a headless boot skips (no loading screen) but
    engine.saveWorld's own content still needs to resolve real
    building/unit/recipe names -- mirrors tools/multiworld_save_probe.py/
    tools/save_compat_migration_probe.py's identical helper. Only needed
    when actually spawning something (an entity-free session never
    references any def at all)."""
    import glob
    loaders = [
        ("data/substances/*.yaml", "engine.loadSubstanceYaml"),
        ("data/items/*.yaml",      "engine.loadItemYaml"),
        ("data/equipment/*.yaml",  "engine.loadEquipmentYaml"),
        ("data/materials/*.yaml",  "engine.loadMaterialYaml"),
        ("data/units/*.yaml",      "engine.loadUnitYaml"),
        ("data/buildings/*.yaml",  "engine.loadBuildingYaml"),
        ("data/recipes/*.yaml",    "engine.loadRecipeYaml"),
    ]
    for pattern, fn in loaders:
        for path in sorted(glob.glob(pattern)):
            send(port, f"{fn}('{path}'); return 'ok'")


def generate_current_format_session(
        port: int, page_id: str, seed: int, world_size: int, plate_count: int,
        spawn_building: str, spawn_unit: str, out_path: Path) -> None:
    """Boot a REAL headless engine (isolated resource root -- see
    _make_isolated_gen_root), init a world, optionally spawn ONE building
    and/or ONE unit through the SAME engine.saveWorld/building.spawn/
    unit.spawn verbs every other probe in this repo already uses, then
    save it -- producing genuine CURRENT-format envelope bytes through
    the real World.Save.Storage/Envelope.Codec production path (the
    exact same one an ordinary player save takes), not a hand-built or
    spliced value. Raises GenerationError on any rejected step.

    This can only ever produce a fixture at the CURRENT wire format -- a
    live engine never writes a historical shape (see this module's own
    docstring for why a historical baseline stays a manual operation)."""
    from probelib import boot, send, quit_engine
    tmpdir = tempfile.mkdtemp(prefix="save_compat_gen_")
    slot = "generated"
    proc = None
    try:
        root = _make_isolated_gen_root(tmpdir)
        proc = boot(port, log=f"/tmp/save_compat_gen_{page_id}.log",
                    args=["--resource-root", root], ready_timeout=180)
        if spawn_building or spawn_unit:
            _bootstrap_gen_defs(send, port)
        inited = send(port, f"world.init('{page_id}', {seed}, {world_size}, "
                             f"{plate_count}); return 'ok'")
        if "ok" not in inited:
            raise GenerationError(f"world.init failed: {inited!r}")
        time.sleep(1.0)  # let generation settle before saving/spawning

        # world.show (not just world.init) puts the page in wmVisible --
        # mirrors tools/multiworld_save_probe.py's identical note: without
        # it, building.spawn/canPlaceAt's snapshotVisibleWorldTiles read
        # can reject a spawn, and the saved snapshot's own visiblePages/
        # live-camera-owner-page would come out empty/null instead of
        # matching an ordinary player session's shape.
        send(port, f"world.show('{page_id}'); return 'ok'")
        active_deadline = time.time() + 10.0
        while time.time() < active_deadline:
            if send(port, "return world.getActiveWorldId()").strip('"') == page_id:
                break
            time.sleep(0.2)
        else:
            raise GenerationError(f"'{page_id}' never became the active world")

        def as_int(s: str):
            try:
                return int(float(s))
            except (TypeError, ValueError):
                return None

        # unit.spawn/building.spawn return the new entity's id (a
        # non-negative integer, as a string) on success, not a boolean --
        # mirrors tools/multiworld_save_probe.py's as_int/bid<0 convention.
        if spawn_building:
            r = send(port, f"return building.spawn('{spawn_building}', 0, 0)")
            bid = as_int(r)
            if bid is None or bid < 0:
                raise GenerationError(
                    f"building.spawn('{spawn_building}') rejected: {r!r}")
        if spawn_unit:
            r = send(port, f"return unit.spawn('{spawn_unit}', 0, 0, 0, 'player')")
            uid = as_int(r)
            if uid is None or uid < 0:
                raise GenerationError(
                    f"unit.spawn('{spawn_unit}') rejected: {r!r}")
        saved = send(port, f"return engine.saveWorld('{page_id}', '{slot}')")
        if saved.strip() != "true":
            raise GenerationError(f"engine.saveWorld failed: {saved!r}")
        saved_path = os.path.join(root, "saves", slot, "world.synworld")
        for _ in range(100):
            if os.path.isfile(saved_path):
                break
            time.sleep(0.1)
        if not os.path.isfile(saved_path):
            raise GenerationError(f"saved file never appeared at {saved_path}")
        out_path.parent.mkdir(parents=True, exist_ok=True)
        shutil.copyfile(saved_path, out_path)
    finally:
        if proc is not None:
            quit_engine(port, proc)
        shutil.rmtree(tmpdir, ignore_errors=True)


# A small, permanent GHCi program (run via `cabal repl` subprocess) that
# derives a fixture's canonical-summary JSON DIRECTLY from its real,
# decoded SessionSnapshot/SaveMetadata -- not from live engine queries,
# several of which (hour/minute of day, in particular) have no debug-
# console verb to read at all. Mirrors EXACTLY the schema
# test-headless/Test/Headless/World/Save/Compat.hs's ExpectedSummary/
# ExpectedPage/Expected* Aeson types parse -- the two must be kept in
# sync by hand if that schema ever grows a field.
GHCI_DUMP_SUMMARY_TEMPLATE = r"""
:set -XOverloadedStrings -XTypeApplications
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import qualified Data.Aeson as Aeson
import Data.Aeson ((.=))
import qualified Data.Text as T
import Data.List (sortOn)
import World.Save.Envelope (decodeSessionEnvelope)
import World.Save.Snapshot
import World.Save.Types
import World.Page.Types (WorldPageId(..))
import Building.Types (BuildingId(..))
import Unit.Types (UnitId(..))
import Unit.Sim.Types (UnitSimState(..))
import Craft.Bills (CraftBills(..), CraftBill(..), BillId(..))
import Power.Types (PowerNodes(..), PowerNode(..), PowerNodeId(..))
import Item.Ground (GroundItems(..))

bytes <- BS.readFile "{fixture_path}"

:{{
let luaNames = HS.fromList ["unit_ai", "building_spawn"]
    decoded = decodeSessionEnvelope luaNames luaNames bytes
:}}

:{{
case decoded of
  Left err -> putStrLn ("DUMP_FAILED: decode: " ++ T.unpack err)
  Right (meta, snap, luaComponents, isMig) -> do
    let dumpBuilding (bid, b) = Aeson.object
          [ "id" .= unBuildingId bid, "defName" .= bisDefName b
          , "anchorX" .= bisAnchorX b, "anchorY" .= bisAnchorY b
          , "gridZ" .= bisGridZ b, "buildProgress" .= bisBuildProgress b ]
        dumpUnit (uid, u) = Aeson.object
          [ "id" .= unUnitId uid, "defName" .= uisDefName u
          , "gridX" .= uisGridX u, "gridY" .= uisGridY u
          , "gridZ" .= uisGridZ u, "facing" .= T.pack (show (uisFacing u))
          , "activity" .= uisActivity u, "pose" .= uisPose u ]
        dumpSim (uid, s) = Aeson.object
          [ "unitId" .= unUnitId uid, "realX" .= usRealX s, "realY" .= usRealY s
          , "gridZ" .= usGridZ s, "pose" .= T.pack (show (usPose s))
          , "state" .= T.pack (show (usState s))
          , "facing" .= T.pack (show (usFacing s)) ]
        dumpBill b = Aeson.object
          [ "id" .= unBillId (cbId b), "station" .= unBuildingId (cbStation b)
          , "recipe" .= cbRecipe b, "remaining" .= cbRemaining b
          , "claimant" .= fmap unUnitId (cbClaimant b)
          , "mode" .= T.pack (show (cbMode b)) ]
        dumpNode n = Aeson.object
          [ "id" .= unPowerNodeId (pnId n), "building" .= unBuildingId (pnBuilding n)
          , "role" .= T.pack (show (pnRole n)), "peakWatts" .= pnPeakWatts n
          , "capacityWh" .= pnCapacityWh n, "storedWh" .= pnStoredWh n ]
        dumpPage (WorldPageId pid, page) = Aeson.object
          [ "pageId" .= pid
          , "buildingCount" .= HM.size (bsnInstances (pgsBuildings page))
          , "unitCount" .= HM.size (usnInstances (pgsUnits page))
          , "unitSimStateCount" .= HM.size (pgsUnitSimStates page)
          , "craftBillCount" .= HM.size (cbsBills (pgsCraftBills page))
          , "powerNodeCount" .= HM.size (pnsNodes (pgsPowerNodes page))
          , "groundItemCount" .= HM.size (gisItems (pgsGroundItems page))
          , "timeHour" .= pgsTimeHour page, "timeMinute" .= pgsTimeMinute page
          , "dateYear" .= pgsDateYear page, "dateMonth" .= pgsDateMonth page
          , "dateDay" .= pgsDateDay page
          , "mapMode" .= T.pack (show (pgsMapMode page))
          , "buildings" .= map dumpBuilding
              (sortOn (unBuildingId . fst)
                 (HM.toList (bsnInstances (pgsBuildings page))))
          , "units" .= map dumpUnit
              (sortOn (unUnitId . fst) (HM.toList (usnInstances (pgsUnits page))))
          , "unitSimStates" .= map dumpSim
              (sortOn (unUnitId . fst) (HM.toList (pgsUnitSimStates page)))
          , "craftBills" .= map dumpBill
              (sortOn cbId (HM.elems (cbsBills (pgsCraftBills page))))
          , "powerNodes" .= map dumpNode
              (sortOn pnId (HM.elems (pnsNodes (pgsPowerNodes page))))
          ]
        cam = snapLiveCamera snap
        WorldPageId activePageText = snapActivePage snap
        summary = Aeson.object
          [ "metadata" .= Aeson.object
              [ "seed" .= smSeed meta, "worldSize" .= smWorldSize meta
              , "plateCount" .= smPlateCount meta, "worldName" .= smWorldName meta
              , "worldGloss" .= smWorldGloss meta ]
          , "gameTime" .= snapGameTime snap
          , "nextItemId" .= snapNextItemId snap
          , "nextBuildingId" .= snapNextBuildingId snap
          , "nextUnitId" .= snapNextUnitId snap
          , "camera" .= Aeson.object
              [ "ownerPage" .= fmap (\(WorldPageId p) -> p) (lcsOwnerPage cam)
              , "x" .= lcsX cam, "y" .= lcsY cam, "zoom" .= lcsZoom cam
              , "facing" .= T.pack (show (lcsFacing cam)) ]
          , "activePage" .= activePageText
          , "visiblePages" .= map (\(WorldPageId p) -> p) (snapVisiblePages snap)
          , "pages" .= map dumpPage
              (sortOn (\(WorldPageId p, _) -> p) (HM.toList (snapPages snap)))
          , "luaComponentCount" .= length luaComponents
          , "isMigratedLegacyBaseline" .= isMig
          ]
    BSL.writeFile "{output_path}" (Aeson.encode summary)
    putStrLn "DUMP_OK"
:}}
"""


def dump_canonical_summary(fixture_path: Path, output_path: Path) -> tuple[bool, str]:
    """Run GHCI_DUMP_SUMMARY_TEMPLATE via a `cabal repl` subprocess to
    derive fixture_path's canonical summary and write it to output_path.
    Returns (ok, diagnostic-tail-on-failure)."""
    script = GHCI_DUMP_SUMMARY_TEMPLATE.format(
        fixture_path=str(fixture_path), output_path=str(output_path))
    try:
        proc = subprocess.run(
            ["cabal", "repl", "test:synarchy-test-headless"],
            input=script, cwd=REPO_ROOT, capture_output=True, text=True,
            timeout=1800)
    except FileNotFoundError:
        return False, "'cabal' was not found on PATH"
    output = (proc.stdout or "") + (proc.stderr or "")
    if "DUMP_OK" not in output or not output_path.exists():
        return False, "\n".join(output.splitlines()[-60:])
    return True, ""


def cmd_generate(args: argparse.Namespace) -> int:
    """--generate-session: produce a brand-new CURRENT-format complete-
    session fixture through the real engine + real codec end to end,
    then delegate straight to cmd_add_baseline for the SAME atomic
    registration + real-codec validation --add-baseline already does
    (this only ever produces a "complete-session" fixture, so args.kind
    is fixed here rather than asked for).

    Round-6 review: stages fixture + summary + manifest together and
    rolls ALL of them back on ANY downstream failure (dump derivation or
    manifest real-codec validation) -- not just the manifest. Without
    this, a validation failure left new fixture/summary bytes sitting on
    disk unregistered, or (with --force) clobbered a PREVIOUSLY-tracked
    fixture's bytes with new-but-invalid content while the manifest
    (correctly rolled back on its own) still pointed at the OLD
    checksum -- either way, a state the NEXT audit run would immediately
    flag as drifted, or that would simply litter the repo with orphaned
    files."""
    fixture_path = REPO_ROOT / args.path
    summary_path = REPO_ROOT / args.summary
    if (fixture_path.exists() or summary_path.exists()) and not args.force:
        print(f"refusing to overwrite an existing file at '{args.path}' "
              f"or '{args.summary}' -- pass --force if this is deliberate",
              file=sys.stderr)
        return 1

    # Captured BEFORE any write, so a failure at ANY stage below can
    # restore both files to their EXACT prior state (or remove them, if
    # they didn't exist before this invocation) -- never leaving a
    # half-written or stale-but-mismatched pair behind.
    orig_fixture = fixture_path.read_bytes() if fixture_path.exists() else None
    orig_summary = summary_path.read_text(encoding="utf-8") if summary_path.exists() else None

    def restore_files() -> None:
        if orig_fixture is None:
            fixture_path.unlink(missing_ok=True)
        else:
            fixture_path.write_bytes(orig_fixture)
        if orig_summary is None:
            summary_path.unlink(missing_ok=True)
        else:
            summary_path.write_text(orig_summary, encoding="utf-8")

    try:
        generate_current_format_session(
            port=args.port, page_id=args.page_id, seed=args.seed,
            world_size=args.world_size, plate_count=args.plate_count,
            spawn_building=args.spawn_building, spawn_unit=args.spawn_unit,
            out_path=fixture_path)
    except GenerationError as e:
        # generate_current_format_session only ever writes fixture_path
        # as its LAST step (shutil.copyfile), after every real-engine
        # check above already succeeded -- a GenerationError here means
        # fixture_path was never touched, so there is nothing to restore.
        print(f"fixture generation failed: {e}", file=sys.stderr)
        return 1

    ok, tail = dump_canonical_summary(fixture_path, summary_path)
    if not ok:
        restore_files()
        print(f"canonical-summary derivation failed (fixture/summary "
              f"restored to their prior state): {tail}", file=sys.stderr)
        return 1

    args.kind = "complete-session"
    if not args.provenance:
        args.provenance = (
            f"Generated through the real codec (tools/save_compat_audit.py "
            f"--generate-session): a real headless engine booted in an "
            f"isolated resource root, world.init('{args.page_id}', "
            f"{args.seed}, {args.world_size}, {args.plate_count})"
            + (f", building.spawn('{args.spawn_building}', 0, 0)"
               if args.spawn_building else "")
            + (f", unit.spawn('{args.spawn_unit}', 0, 0, 0, 'player')"
               if args.spawn_unit else "")
            + f", then engine.saveWorld -- the exact production save path "
              f"an ordinary player save takes. Its canonical summary was "
              f"derived directly from the real decoded SessionSnapshot "
              f"(dump_canonical_summary), not hand-transcribed.")
    rc = cmd_add_baseline(args)
    if rc != 0:
        restore_files()
        print(f"registration/validation failed -- fixture/summary "
              f"restored to their prior state too (not just the "
              f"manifest)", file=sys.stderr)
    return rc


def cmd_add_baseline(args: argparse.Namespace) -> int:
    manifest = load_manifest(MANIFEST_PATH)
    existing_baseline = next(
        (b for b in manifest.get("baselines", []) if b.get("id") == args.baseline_id),
        None)

    try:
        new_fixture = _build_fixture_entry(args)
    except SystemExit as e:
        print(e, file=sys.stderr)
        return 1

    if existing_baseline is None:
        # A brand new baseline: requires the full descriptor so the
        # manifest entry is complete on creation, never a bare fixture
        # with no declared components/migration target to check it
        # against.
        missing = [flag for flag, val in
                   [("--description", args.description),
                    ("--migration-target", args.migration_target),
                    ("--migrated-by", args.migrated_by),
                    ("--components", args.components)]
                   if not val]
        if missing:
            print(f"baseline '{args.baseline_id}' does not exist yet -- "
                  f"creating a NEW baseline also requires: {', '.join(missing)}",
                  file=sys.stderr)
            return 1
        try:
            components = json.loads(args.components)
        except ValueError as e:
            print(f"--components is not valid JSON: {e}", file=sys.stderr)
            return 1
        manifest.setdefault("baselines", []).append({
            "id": args.baseline_id,
            "description": args.description,
            "declaredAt": args.declared_at or "(fill in: YYYY-MM-DD)",
            "declaredByIssue": args.declared_by_issue,
            "supportStatus": "supported",
            "migrationTarget": args.migration_target,
            "migratedBy": args.migrated_by,
            "components": components,
            "fixtures": [new_fixture],
        })
        return _finalize_manifest_write(
            manifest, MANIFEST_PATH, args.kind, args.skip_validation,
            f"created baseline '{args.baseline_id}' with fixture "
            f"'{args.fixture_id}': sha256={new_fixture['sha256']} "
            f"sizeBytes={new_fixture['sizeBytes']}")

    existing_fixture = next(
        (f for f in existing_baseline.get("fixtures", [])
         if f.get("id") == args.fixture_id), None)
    if existing_fixture is not None and not args.force:
        print(f"refusing to overwrite existing fixture '{args.fixture_id}' "
              f"on baseline '{args.baseline_id}' -- pass --force if this is "
              f"a deliberate re-registration (e.g. after regenerating "
              f"through the real codec)", file=sys.stderr)
        return 1
    if existing_fixture is not None:
        existing_baseline["fixtures"] = [
            new_fixture if f.get("id") == args.fixture_id else f
            for f in existing_baseline["fixtures"]]
    else:
        existing_baseline.setdefault("fixtures", []).append(new_fixture)
    return _finalize_manifest_write(
        manifest, MANIFEST_PATH, args.kind, args.skip_validation,
        f"registered fixture '{args.fixture_id}' on baseline "
        f"'{args.baseline_id}': sha256={new_fixture['sha256']} "
        f"sizeBytes={new_fixture['sizeBytes']}")


def main() -> int:
    ap = argparse.ArgumentParser(description=__doc__,
                                 formatter_class=argparse.RawDescriptionHelpFormatter)
    ap.add_argument("--add-baseline", action="store_true",
                     help="atomically register a fixture (and, if new, its "
                          "whole baseline entry) instead of auditing")
    ap.add_argument("--generate-session", action="store_true",
                     help="generate a brand-new CURRENT-format complete-"
                          "session fixture through a real headless engine, "
                          "then register it exactly like --add-baseline "
                          "(requirement 21's real generation mode)")
    ap.add_argument("--seed", type=int, default=42,
                     help="--generate-session only, default 42")
    ap.add_argument("--world-size", type=int, default=8,
                     help="--generate-session only, default 8")
    ap.add_argument("--plate-count", type=int, default=3,
                     help="--generate-session only, default 3")
    ap.add_argument("--page-id", default="generated_page",
                     help="--generate-session only, default 'generated_page'")
    ap.add_argument("--spawn-building", default=None,
                     help="--generate-session only: a real building def "
                          "name to spawn at (0,0), e.g. cargo_hold_S")
    ap.add_argument("--spawn-unit", default=None,
                     help="--generate-session only: a real unit def name "
                          "to spawn at (0,0), e.g. acolyte")
    ap.add_argument("--port", type=int, default=9280,
                     help="--generate-session only: debug-console port "
                          "for the generation engine boot")
    ap.add_argument("--baseline-id", help="baseline id (new or existing)")
    ap.add_argument("--fixture-id", help="fixture id within that baseline")
    ap.add_argument("--path", help="fixture file path, repo-relative -- "
                                    "already generated through the real codec")
    ap.add_argument("--kind", choices=["complete-session", "component-focused"],
                     help="fixture kind (requirement 11)")
    ap.add_argument("--summary", help="expected-canonical-summary JSON path, "
                                        "repo-relative (required for "
                                        "complete-session fixtures)")
    ap.add_argument("--provenance", help="how this fixture was generated "
                                           "(free text, recorded verbatim)")
    ap.add_argument("--description", help="baseline description (new baseline only)")
    ap.add_argument("--migration-target", help="e.g. 'current' (new baseline only)")
    ap.add_argument("--migrated-by", help="the migration function/codec path "
                                            "(new baseline only)")
    ap.add_argument("--components", help="JSON array of {id,version,required} "
                                           "(new baseline only)")
    ap.add_argument("--declared-at", help="YYYY-MM-DD (new baseline only)")
    ap.add_argument("--declared-by-issue", type=int, default=766,
                     help="new baseline only, default 766")
    ap.add_argument("--force", action="store_true",
                     help="allow re-registering an already-recorded fixture id")
    ap.add_argument("--skip-validation", action="store_true",
                     help="don't run the new/updated complete-session "
                          "fixture through the real codec (cabal test "
                          "synarchy-test-headless --test-options='--match "
                          "\"save migrations\"') before keeping the "
                          "registration -- only for environments with no "
                          "cabal toolchain; the checked-in CI gate still "
                          "catches a bad fixture on the next push")
    args = ap.parse_args()
    if args.generate_session:
        if not args.baseline_id or not args.fixture_id or not args.path \
                or not args.summary:
            ap.error("--generate-session requires --baseline-id, "
                     "--fixture-id, --path, and --summary")
        return cmd_generate(args)
    if args.add_baseline:
        if not args.baseline_id or not args.fixture_id or not args.path or not args.kind:
            ap.error("--add-baseline requires --baseline-id, --fixture-id, "
                     "--path, and --kind")
        return cmd_add_baseline(args)
    return cmd_audit(args)


if __name__ == "__main__":
    sys.exit(main())
