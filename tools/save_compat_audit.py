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
    current Haskell (World.Save.Component.*'s ccVersion/ccInputVers) and
    Lua (scripts/unit_ai_save.lua's/scripts/building_spawn.lua's version/
    inputVersions) registries (see real_component_registry() /
    audit_component_versions()): a declared component/version must still
    exist and still be an accepted input version (catches a decoder
    silently dropped), and every component with more than one accepted
    input version must have its OLDEST one tracked by some baseline
    (catches a version bump that shipped with no compatibility fixture
    ever validating the historical shape it migrates from).

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

Exit codes: 0 = every declared fixture/fingerprint is intact,
1 = one or more violations (see printed detail).
"""
from __future__ import annotations

import argparse
import hashlib
import json
import re
import subprocess
import sys
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
    r"ccInputVers\s*=\s*\[([^\]]*)\]")
LUA_MODULE_VERSION_RE = re.compile(r"\bversion\s*=\s*(\d+)")
LUA_MODULE_INPUT_VERSIONS_RE = re.compile(
    r"\binputVersions\s*=\s*\{([^}]*)\}")


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
    id -> {"currentVersion": int, "inputVersions": [int, ...]} -- parsed
    directly from the Haskell/Lua source, not hand-maintained here,
    so a schema bump or a removed historical decoder shows up the next
    time this runs, not only when someone remembers to update this
    tool too (requirement 19's version cross-check).

    Raises if a source file's expected declarations can't be found at
    all (the parser itself is stale, e.g. after a real rename) -- that
    must fail loudly, not silently audit against an empty registry."""
    registry: dict[str, dict] = {}

    envelope_text = ENVELOPE_SOURCE_PATH.read_text(encoding="utf-8")
    m = METADATA_COMPONENT_VERSION_RE.search(envelope_text)
    if not m:
        raise ValueError(
            f"could not find metadataComponentVersion in {ENVELOPE_SOURCE_PATH}")
    registry["metadata"] = {
        "currentVersion": int(m.group(1)), "inputVersions": [int(m.group(1))]}

    # "session" is the ONE frozen legacy component (World.Save.Compat.
    # SessionV90) -- its current version is its only ever version; a
    # further schema change adds a new frozen type instead of bumping
    # this one (the frozen-DTO boundary rule), so inputVersions is
    # always the singleton [currentVersion].
    session_text = SESSION_V90_SOURCE_PATH.read_text(encoding="utf-8")
    m = SESSION_COMPONENT_VERSION_RE.search(session_text)
    if not m:
        raise ValueError(
            f"could not find sessionComponentVersion in {SESSION_V90_SOURCE_PATH}")
    registry["session"] = {
        "currentVersion": int(m.group(1)), "inputVersions": [int(m.group(1))]}

    id_literals: dict[str, str] = {}
    for path in HASKELL_COMPONENT_SOURCE_PATHS:
        text = path.read_text(encoding="utf-8")
        for ident, sid in COMPONENT_ID_LITERAL_RE.findall(text):
            id_literals[ident] = sid

    for path in HASKELL_COMPONENT_SOURCE_PATHS:
        text = path.read_text(encoding="utf-8")
        for _codec_name, cid_ident, ver, _req in SERIALIZE_CODEC_RE.findall(text):
            sid = id_literals.get(cid_ident)
            if sid is None:
                raise ValueError(
                    f"{path}: serializeCodec references unknown component "
                    f"id identifier '{cid_ident}' -- did a ComponentId "
                    f"binding get renamed without updating this parser?")
            registry[sid] = {
                "currentVersion": int(ver), "inputVersions": [int(ver)]}
        for cid_ident, ver, vers_str in RECORD_CODEC_RE.findall(text):
            sid = id_literals.get(cid_ident)
            if sid is None:
                raise ValueError(
                    f"{path}: hand-rolled ComponentCodec references unknown "
                    f"component id identifier '{cid_ident}'")
            input_versions = [int(v.strip()) for v in vers_str.split(",")
                               if v.strip()]
            registry[sid] = {
                "currentVersion": int(ver), "inputVersions": input_versions}

    for path, lua_id in [(LUA_UNIT_AI_SOURCE_PATH, "unit_ai"),
                          (LUA_BUILDING_SPAWN_SOURCE_PATH, "building_spawn")]:
        text = path.read_text(encoding="utf-8")
        vm = LUA_MODULE_VERSION_RE.search(text)
        ivm = LUA_MODULE_INPUT_VERSIONS_RE.search(text)
        if not vm or not ivm:
            raise ValueError(
                f"could not find version/inputVersions in {path} -- did "
                f"saveMods.register('{lua_id}', {{...}})'s declaration "
                f"shape change?")
        input_versions = [int(v.strip()) for v in ivm.group(1).split(",")
                           if v.strip()]
        registry[f"lua.{lua_id}"] = {
            "currentVersion": int(vm.group(1)), "inputVersions": input_versions}

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
        if len(real["inputVersions"]) <= 1:
            continue
        oldest = min(real["inputVersions"])
        if oldest not in tracked_versions.get(comp_id, set()):
            violations.append(
                f"component '{comp_id}' accepts input versions "
                f"{sorted(real['inputVersions'])} (a migration exists from "
                f"v{oldest}), but no manifest baseline declares/tracks a "
                f"fixture at v{oldest} -- that historical migration has no "
                f"tracked compatibility coverage")

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

    violations.extend(
        audit_component_versions(manifest, real_component_registry()))

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
    if args.add_baseline:
        if not args.baseline_id or not args.fixture_id or not args.path or not args.kind:
            ap.error("--add-baseline requires --baseline-id, --fixture-id, "
                     "--path, and --kind")
        return cmd_add_baseline(args)
    return cmd_audit(args)


if __name__ == "__main__":
    sys.exit(main())
