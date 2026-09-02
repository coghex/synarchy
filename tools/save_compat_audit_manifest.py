#!/usr/bin/env python3
"""The blocking save-compatibility audit (issue #2049, requirement 9).

The ONE owner of the aggregate audit: it loads the manifest, iterates
its fixtures, and produces the single list of violations the façade's
default command prints. It consumes the three leaf services --
save_compat_audit_components (the real registry and its version-coverage
policy), save_compat_audit_fingerprint (the framing/frozen-DTO
fingerprints), and save_compat_audit_codec (the real decoded fixture
descriptors) -- and implements none of them (requirement 16).

It owns, in one place:

  - manifest loading (`load_manifest`) and fixture iteration
    (`_iter_fixtures`);
  - the per-fixture checksum, size, path and summary checks, and the
    manifest-vs-source framing-version/fingerprint checks (`audit`);
  - modern-baseline completeness
    (`audit_modern_baseline_components_complete`);
  - B1 migration-helper coverage
    (`audit_b1_migration_covers_page_scoped_components`);
  - orphaned fixture detection (`audit_no_orphaned_fixture_files`);
  - real decoded-descriptor verification (`verify_fixture_descriptors`);
  - aggregate violation reporting (`audit`, `cmd_audit`).

Requirement 10: nothing here weakened in the split. In particular
`verify_fixture_descriptors` still returns `(None, [diagnostic])` when
the descriptor dump itself fails, so an unavailable real-codec
verification is reported as its own violation and every check it powers
falls back visibly -- it never silently validates a manifest claim.

The public façade is tools/save_compat_audit.py.
"""
from __future__ import annotations

import hashlib
import json
from pathlib import Path

import save_compat_audit_codec as codec
import save_compat_audit_common as common
import save_compat_audit_components as components
import save_compat_audit_fingerprint as fingerprint

def load_manifest(path: Path | None = None) -> dict:
    """The manifest as a plain dict.

    @path@ defaults to 'common.MANIFEST_PATH', resolved HERE rather than
    bound as a default argument, so the self-test's rebinding of that
    module attribute is actually seen (issue #2049 requirement 18)."""
    path = common.MANIFEST_PATH if path is None else path
    return json.loads(path.read_text(encoding="utf-8"))

# The b1-initial-session baseline's ONLY migration path is
# World.Save.Compat.SessionV90.migrateSessionV90, which threads every
# page-scoped modern component's construction through one of these named
# helpers (see the function's own source: `base = basePageSnapshots
# (...); afterEdits <- applyWorldEdits 1 (...) base; ...`) -- this
# mapping is a maintained LOOKUP (Python cannot itself discover a
# Haskell helper's name), but round-13 review: whether a REQUIRED
# component even NEEDS an entry here at all is no longer trusted to
# whoever edits this dict by hand -- audit_b1_migration_covers_
# page_scoped_components (below) now derives the set of components that
# need SOME accounted policy directly from the REAL registry, so a
# brand-new required component that nobody added here shows up as its
# own violation, not silent gap.
SESSION_V90_APPLY_HELPER_FOR_COMPONENT = {
    "world-edits":    "applyWorldEdits",
    "world-activity": "applyWorldActivity",
    "buildings":      "applyBuildings",
    "units":          "applyUnits",
    "unit-sim":       "applyUnitSim",
    "craft-bills":    "applyCraftBills",
    "power-nodes":    "applyPowerNodes",
}

# Components migrateSessionV90 constructs WITHOUT a named per-component
# apply* helper, because they are built directly as part of the ONE
# SessionSnapshot/PageSnapshot record literal GHC already forces to be
# total (so they can never silently go unconstructed the way a
# forgotten apply* call for a page-scoped component could):
#   - "core-session"/"texture-palette": global (not page-scoped) fields
#     -- allocators/camera and the texture palette -- set directly in
#     that one record literal.
#   - "world-pages": the FOUNDATION every page is built from
#     (basePageSnapshots, from the frozen v90 blob's own worldgen
#     params) -- not a component layered ON TOP of that foundation via
#     an apply* call the way every other page-scoped component is.
# "metadata"/"session" are B1's own INPUT being migrated FROM, not a
# component migrateSessionV90 constructs; "lua-state"/"lua.*" are B2/B3
# concerns entirely -- B1 predates Lua persistence altogether and
# always defaults every current Lua module via isMigratingLegacyBaseline,
# unrelated to any page-scoped Haskell helper.
SESSION_V90_GLOBAL_OR_INPUT_COMPONENTS = {
    "core-session", "texture-palette", "world-pages", "metadata", "session",
    "lua-state",
}


def audit_b1_migration_covers_page_scoped_components(
        real_registry: dict, source_path: Path | None = None,
) -> list[str]:
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
    B1 sessions still migrate it.

    Round-13 review: previously only checked the FIXED
    SESSION_V90_APPLY_HELPER_FOR_COMPONENT dict's own entries against the
    source -- a brand-new required Haskell component that nobody
    remembered to ALSO add to that dict was invisible to this audit
    entirely (modern-baseline completeness explicitly exempts the
    b1-shaped baseline from needing full components[] coverage, so
    nothing else would catch it either). Now derives the set of
    components requiring SOME accounted B1 policy directly from
    real_registry (every REQUIRED, non-Lua id minus the explicitly
    justified SESSION_V90_GLOBAL_OR_INPUT_COMPONENTS exemptions above),
    so an unclassified new required component is its own violation
    rather than a silent gap.

    @source_path@ defaults to 'common.SESSION_V90_SOURCE_PATH', resolved
    HERE rather than bound as a default argument (issue #2049
    requirement 18)."""
    source_path = (common.SESSION_V90_SOURCE_PATH if source_path is None
                   else source_path)
    text = source_path.read_text(encoding="utf-8")
    violations = [
        f"World.Save.Compat.SessionV90.migrateSessionV90 (the "
        f"b1-initial-session baseline's ONLY migration path) no longer "
        f"references '{helper}' for component '{comp_id}' -- a new "
        f"required page-scoped component, or one whose construction "
        f"helper was renamed/removed, would silently have no accounted "
        f"default/migration policy for a session predating it"
        for comp_id, helper in SESSION_V90_APPLY_HELPER_FOR_COMPONENT.items()
        if helper not in text
    ]
    for comp_id, real in real_registry.items():
        if (not real.get("required") or comp_id.startswith("lua.")
                or comp_id in SESSION_V90_GLOBAL_OR_INPUT_COMPONENTS
                or comp_id in SESSION_V90_APPLY_HELPER_FOR_COMPONENT):
            continue
        violations.append(
            f"component '{comp_id}' is REQUIRED in the real registry but "
            f"has NO known migration-helper mapping in "
            f"SESSION_V90_APPLY_HELPER_FOR_COMPONENT and is not listed in "
            f"SESSION_V90_GLOBAL_OR_INPUT_COMPONENTS either -- B1 "
            f"compatibility for this component has never been verified at "
            f"all (round-13 review: this is exactly the 'a brand-new "
            f"required component nobody classified' gap this audit exists "
            f"to catch); add its migrateSessionV90 helper name to "
            f"SESSION_V90_APPLY_HELPER_FOR_COMPONENT, or -- if it is "
            f"genuinely built directly into the base record literal like "
            f"core-session/texture-palette/world-pages -- add it to "
            f"SESSION_V90_GLOBAL_OR_INPUT_COMPONENTS instead")
    return violations


def audit_modern_baseline_components_complete(
        manifest: dict, real_registry: dict) -> list[str]:
    """Requirement 5's other half: a baseline that is NOT b1-shaped (its
    components[] doesn't declare the frozen legacy "session" component)
    is, by construction, a MODERN per-component-registry session -- and
    every one of those components (except "session"/"lua-state", each
    RETIRED, mutually-exclusive ALTERNATE representations for state the
    modern registry also covers: the frozen v90 blob vs the split
    Haskell components, and the single opaque pre-#761 Lua blob vs
    lua.unit_ai/lua.building_spawn -- a baseline declaring one is exempt
    from needing the modern equivalent it stands in for) is
    unconditionally REQUIRED (decodeEnvelope refuses a modern envelope
    missing one outright, see componentRequiredIds/
    MissingRequiredComponent). So a valid tracked "current"-target
    modern-shaped fixture cannot possibly omit any of them -- if a
    baseline's own components[] doesn't declare one, that baseline's
    manifest entry is under-documenting what its own fixture genuinely
    contains, precisely the gap round-5 review flagged (a future
    required component could be added to only ONE such baseline and
    never show up as a coverage gap in the OTHERS, since the
    all-baselines-aggregate check alone can't see that)."""
    modern_required_ids = {
        cid for cid, info in real_registry.items()
        if info.get("required") and cid not in ("session", "lua-state")
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
        exempt_ids = ({"lua.unit_ai", "lua.building_spawn"}
                      if "lua-state" in declared_ids else set())
        missing = sorted(modern_required_ids - exempt_ids - declared_ids)
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


def audit_no_orphaned_fixture_files(
        manifest: dict, fixture_dir: Path | None = None) -> list[str]:
    """Round-19 (post-approval) review: every check above verifies a
    DECLARED fixture's path exists and matches -- none of them verify the
    other direction, that every file actually sitting in fixture_dir is
    declared by SOME baseline. An orphaned file (left over from a rename,
    or a --generate-session/--add-baseline run that wrote bytes but was
    never wired into a baseline) gives no compatibility guarantee at all:
    it is never decoded, never migrated, never checksummed -- silently
    inert, yet sitting right alongside real tracked fixtures where it
    looks tracked.

    @fixture_dir@ defaults to 'common.FIXTURE_DATA_DIR', resolved HERE
    rather than bound as a default argument (issue #2049 requirement
    18)."""
    fixture_dir = (common.FIXTURE_DATA_DIR if fixture_dir is None
                   else fixture_dir)
    if not fixture_dir.is_dir():
        return []
    referenced = set()
    for _baseline, fixture in _iter_fixtures(manifest):
        for key in ("path", "expectedCanonicalSummary"):
            val = fixture.get(key)
            if val:
                referenced.add((common.REPO_ROOT / val).resolve())
    violations = []
    for path in sorted(fixture_dir.iterdir()):
        if not path.is_file():
            continue
        if path.resolve() not in referenced:
            violations.append(
                f"'{path.relative_to(common.REPO_ROOT)}' exists under "
                f"{fixture_dir.relative_to(common.REPO_ROOT)}/ but is not "
                f"referenced by any manifest baseline's fixture 'path' or "
                f"'expectedCanonicalSummary' -- an orphaned fixture file is "
                f"never decoded, migrated, or checksummed by this audit or "
                f"the manifest-driven hspec gate, giving no compatibility "
                f"guarantee at all despite looking tracked; register it in "
                f"a baseline's fixtures[] (see --add-baseline) or delete it")
    return violations


def verify_fixture_descriptors(
        manifest: dict) -> tuple[dict[str, set[int]] | None, list[str]]:
    """Round-12 review: ground audit_component_versions' coverage checks
    in REAL fixture bytes, not a baseline's self-reported components[]
    claim. Decodes every "complete-session" fixture with a tracked
    checksum (the only fixtures whose bytes genuinely carry a full,
    real component manifest -- a "component-focused" fixture may be an
    isolated Lua payload or inline source, not a full envelope) and:

      - flags any baseline components[] entry (id, version, required)
        that does NOT match ANY of that baseline's own real, decoded
        fixtures' descriptors -- catching a manifest edit that claims a
        version bump with no fixture ever actually re-encoded at it;
      - returns the VERIFIED (fixture-backed) id -> {tracked versions}
        map for audit_component_versions to use instead of trusting the
        manifest JSON directly.

    Returns (None, []) when there is nothing to verify against at all
    (no complete-session fixture anywhere -- e.g. a synthetic test
    manifest exercising an unrelated check), telling the caller to fall
    back to the manifest-JSON-trusting behavior unchanged. Returns
    (None, [diagnostic]) if the decode step itself fails (no cabal on
    PATH, a genuinely corrupt fixture, etc.) -- the caller's other,
    unrelated checks still run normally, but every version-coverage
    check this powers falls back too until fixed, with that fact
    surfaced as its own violation rather than silently trusting
    unverified claims."""
    complete_session_paths: dict[str, tuple[str, Path]] = {}
    for baseline, fixture in _iter_fixtures(manifest):
        if fixture.get("kind") != "complete-session" or not fixture.get("sha256"):
            continue
        path_str = fixture.get("path")
        if not path_str:
            continue
        fpath = common.REPO_ROOT / path_str
        if not fpath.exists():
            continue
        complete_session_paths[path_str] = (baseline.get("id"), fpath)

    if not complete_session_paths:
        return None, []

    dumped, tail = codec.dump_fixture_descriptors(
        [p for (_, p) in complete_session_paths.values()])
    if dumped is None:
        return None, [
            "could not verify manifest components[] against real fixture "
            "descriptors (every version-coverage check below falls back to "
            "trusting the manifest's own claim until this is fixed): " + tail]

    violations: list[str] = []
    verified_tracked: dict[str, set[int]] = {}
    for path_str, (bid, fpath) in complete_session_paths.items():
        descs = dumped.get(str(fpath))
        if descs is None:
            violations.append(
                f"baseline '{bid}' fixture at '{path_str}' was not decoded "
                f"(missing from the descriptor dump's own output) -- "
                f"cannot verify its declared components[] against real bytes")
            continue
        for d in descs:
            verified_tracked.setdefault(d["id"], set()).add(d["version"])

    for baseline in manifest.get("baselines", []):
        bid = baseline.get("id")
        this_baseline_descs = [
            dumped[str(fpath)]
            for (owner_bid, fpath) in complete_session_paths.values()
            if owner_bid == bid and str(fpath) in dumped
        ]
        if not this_baseline_descs:
            # Nothing to verify against (e.g. this baseline's only
            # fixtures are "component-focused" or checksum-less) --
            # audit_component_versions' own existing checks still cover
            # id/version-validity against the real codec registry.
            continue
        for comp in baseline.get("components", []):
            comp_id, comp_ver, comp_req = (
                comp.get("id"), comp.get("version"), comp.get("required"))
            if not any(
                    any(d["id"] == comp_id and d["version"] == comp_ver
                        and d["required"] == comp_req for d in descs)
                    for descs in this_baseline_descs):
                violations.append(
                    f"baseline '{bid}' declares component '{comp_id}' at "
                    f"version {comp_ver} (required={comp_req}), but NONE of "
                    f"its own real, decoded fixtures actually carry a "
                    f"matching descriptor -- this baseline's components[] "
                    f"claim is not backed by any tracked fixture's bytes "
                    f"(round-12 review: a manifest-only version bump with "
                    f"no fixture re-encoded at it must not silently satisfy "
                    f"this baseline's own coverage)")

    return verified_tracked, violations


def audit(manifest: dict, fixture_dir: Path | None = None) -> list[str]:
    """Every save-compatibility violation in one manifest, aggregated.

    @fixture_dir@ defaults to 'common.FIXTURE_DATA_DIR', resolved HERE
    rather than bound as a default argument (issue #2049 requirement
    18)."""
    fixture_dir = (common.FIXTURE_DATA_DIR if fixture_dir is None
                   else fixture_dir)
    violations: list[str] = []

    declared_framing = manifest.get("envelopeFramingVersion")
    real_framing = fingerprint.current_envelope_version()
    if declared_framing != real_framing:
        violations.append(
            f"manifest envelopeFramingVersion ({declared_framing}) disagrees "
            f"with World.Save.Envelope.currentEnvelopeVersion ({real_framing}) "
            f"-- a framing bump is a new format epoch and must update the "
            f"manifest deliberately, never silently")

    declared_fingerprint = manifest.get("frozenDtoFingerprint")
    real_fingerprint = fingerprint.frozen_dto_fingerprint()
    if declared_fingerprint != real_fingerprint:
        violations.append(
            f"manifest frozenDtoFingerprint ({declared_fingerprint}) disagrees "
            f"with the current World.Save.Compat.SessionV90 frozen-DTO field "
            f"layout ({real_fingerprint}) -- a field was added/removed/"
            f"reordered on an already-shipped frozen DTO (requirement 10), or "
            f"the manifest needs a deliberate update alongside the change")

    declared_framing_fingerprint = manifest.get("envelopeFramingFingerprint")
    real_framing_fingerprint = fingerprint.envelope_framing_fingerprint()
    if declared_framing_fingerprint != real_framing_fingerprint:
        violations.append(
            f"manifest envelopeFramingFingerprint ({declared_framing_fingerprint}) "
            f"disagrees with the current World.Save.Envelope.Codec/.Types wire "
            f"layout ({real_framing_fingerprint}) -- round-15 review: "
            f"envelopeFramingVersion alone is just an integer someone has to "
            f"remember to bump; this fingerprint catches an actual byte-layout "
            f"change (ComponentDescriptor's fields, the magic bytes, the "
            f"checksum algorithm, encodeEnvelope/decodeEnvelope's header "
            f"construction) shipping with envelopeFramingVersion left "
            f"untouched -- a new wire format with no format epoch. A moved "
            f"fingerprint is NOT by itself proof the wire format changed: it "
            f"reacts to any structural edit surviving normalization, and "
            f"plenty of those (a renamed local binding, a refactored helper) "
            f"leave the bytes identical. So decide first which happened. If "
            f"the on-disk layout really did change, bump "
            f"envelopeFramingVersion -- a deliberate, reviewed format epoch -- "
            f"and update this fingerprint together, or revert the change. If "
            f"the bytes are unchanged, record the new fingerprint alone and "
            f"leave envelopeFramingVersion exactly where it is")

    real_registry = components.real_component_registry()
    verified_tracked, descriptor_violations = verify_fixture_descriptors(manifest)
    violations.extend(descriptor_violations)
    violations.extend(
        components.audit_component_versions(
            manifest, real_registry, verified_tracked))
    violations.extend(audit_modern_baseline_components_complete(manifest, real_registry))
    violations.extend(audit_b1_migration_covers_page_scoped_components(real_registry))
    violations.extend(audit_no_orphaned_fixture_files(manifest, fixture_dir))

    for baseline, fixture in _iter_fixtures(manifest):
        fid = fixture.get("id", "<unnamed>")
        path_str = fixture.get("path")
        if not path_str:
            violations.append(
                f"baseline '{baseline.get('id')}' fixture '{fid}' has no path")
            continue
        fpath = common.REPO_ROOT / path_str
        if not fpath.exists():
            violations.append(
                f"baseline '{baseline.get('id')}' fixture '{fid}' path "
                f"'{path_str}' does not exist")
            continue
        fixture_kind = fixture.get("kind")
        expected_sha = fixture.get("sha256")
        summary_path_str = fixture.get("expectedCanonicalSummary")
        if fixture_kind == "complete-session":
            missing = [
                name for name, val in
                (("sha256", expected_sha), ("expectedCanonicalSummary", summary_path_str))
                if not val]
            if missing:
                # Round-9 review: a checksum-less and/or summary-less
                # "complete-session" entry bypasses BOTH this audit (the
                # checksum/summary checks below are skipped entirely when
                # sha256 is None) AND Test.Headless.World.Save.Compat's own
                # manifest-driven hspec gate (which only iterates
                # complete-session fixtures that HAVE a checksum) --
                # letting a baseline claim full end-to-end migration
                # coverage with no tracked binary, no expected canonical
                # summary, and no decode/migrate/assemble validation ever
                # run against it. Only "component-focused" fixtures (a
                # real hspec gate elsewhere is the audit trail instead --
                # see b3-lua-versioned-hspec-coverage/historical-b1-
                # session-recovered) may legitimately omit either.
                violations.append(
                    f"baseline '{baseline.get('id')}' fixture '{fid}' is "
                    f"declared \"kind\": \"complete-session\" but is "
                    f"missing {' and '.join(missing)} -- a complete-session "
                    f"fixture with no tracked checksum and/or no expected "
                    f"canonical summary is never actually decoded/migrated/"
                    f"validated by ANY gate (this audit skips checksum-less "
                    f"fixtures entirely, and the hspec manifest gate only "
                    f"selects complete-session fixtures WITH a checksum) -- "
                    f"either supply both through --add-baseline, or mark "
                    f"this fixture \"component-focused\" if its real "
                    f"coverage genuinely lives elsewhere")
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

        if summary_path_str:
            summary_path = common.REPO_ROOT / summary_path_str
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
    manifest = load_manifest(common.MANIFEST_PATH)
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
