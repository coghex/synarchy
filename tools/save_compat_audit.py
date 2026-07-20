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
  "provenance" fields for worked examples); this command only performs
  the atomic bookkeeping (checksum, size, manifest/summary wiring), since
  only Haskell can run the cereal codec that produces them.

Exit codes: 0 = every declared fixture/fingerprint is intact,
1 = one or more violations (see printed detail).
"""
from __future__ import annotations

import argparse
import hashlib
import json
import re
import sys
from pathlib import Path

REPO_ROOT = Path(__file__).resolve().parent.parent
MANIFEST_PATH = REPO_ROOT / "docs" / "save_compat" / "manifest.json"
ENVELOPE_SOURCE_PATH = REPO_ROOT / "src" / "World" / "Save" / "Envelope.hs"
SESSION_V90_SOURCE_PATH = (
    REPO_ROOT / "src" / "World" / "Save" / "Compat" / "SessionV90.hs")

CURRENT_ENVELOPE_VERSION_RE = re.compile(
    r"^currentEnvelopeVersion\s*=\s*(\d+)", re.MULTILINE)


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
        _write_manifest_atomically(manifest, MANIFEST_PATH)
        print(f"created baseline '{args.baseline_id}' with fixture "
              f"'{args.fixture_id}': sha256={new_fixture['sha256']} "
              f"sizeBytes={new_fixture['sizeBytes']}")
        return 0

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
    _write_manifest_atomically(manifest, MANIFEST_PATH)
    print(f"registered fixture '{args.fixture_id}' on baseline "
          f"'{args.baseline_id}': sha256={new_fixture['sha256']} "
          f"sizeBytes={new_fixture['sizeBytes']}")
    return 0


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
    args = ap.parse_args()
    if args.add_baseline:
        if not args.baseline_id or not args.fixture_id or not args.path or not args.kind:
            ap.error("--add-baseline requires --baseline-id, --fixture-id, "
                     "--path, and --kind")
        return cmd_add_baseline(args)
    return cmd_audit(args)


if __name__ == "__main__":
    sys.exit(main())
