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

This is a static presence/fingerprint check, not a proof that a fixture
actually migrates correctly -- see test-headless's "save components" /
"save compatibility" hspec gates and tools/save_compat_migration_probe.py
for the real decode/migrate/assemble/integrity coverage this audit does
not replicate.

Usage:
  python3 tools/save_compat_audit.py                # blocking audit (CI)
  python3 tools/save_compat_audit.py --add-baseline \\
      --id my-fixture --path path/to/fixture.bin     # register a NEW
                                                        fixture's checksum
                                                        (refuses to
                                                        overwrite an
                                                        existing id)

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
    manifest = load_manifest()
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


def cmd_add_baseline(args: argparse.Namespace) -> int:
    manifest_path = MANIFEST_PATH
    manifest = load_manifest(manifest_path)
    for _, fixture in _iter_fixtures(manifest):
        if fixture.get("id") == args.id:
            if not args.force:
                print(f"refusing to overwrite existing fixture '{args.id}' "
                      f"-- pass --force if this is a deliberate re-registration "
                      f"(e.g. after regenerating through the real codec)",
                      file=sys.stderr)
                return 1
            fpath = REPO_ROOT / args.path
            if not fpath.exists():
                print(f"path '{args.path}' does not exist", file=sys.stderr)
                return 1
            data = fpath.read_bytes()
            fixture["sha256"] = hashlib.sha256(data).hexdigest()
            fixture["sizeBytes"] = len(data)
            manifest_path.write_text(
                json.dumps(manifest, indent=2) + "\n", encoding="utf-8")
            print(f"updated fixture '{args.id}': sha256={fixture['sha256']} "
                  f"sizeBytes={fixture['sizeBytes']}")
            return 0
    print(f"fixture id '{args.id}' is not declared in any baseline in "
          f"{manifest_path} yet -- add its manifest entry (baseline id, "
          f"components, description) by hand first, THEN run --add-baseline "
          f"to compute and record its checksum. This tool never invents a "
          f"new baseline's identity on its own.", file=sys.stderr)
    return 1


def main() -> int:
    ap = argparse.ArgumentParser(description=__doc__,
                                 formatter_class=argparse.RawDescriptionHelpFormatter)
    ap.add_argument("--add-baseline", action="store_true",
                     help="record a fixture's checksum instead of auditing")
    ap.add_argument("--id", help="fixture id (with --add-baseline)")
    ap.add_argument("--path", help="fixture file path, repo-relative (with --add-baseline)")
    ap.add_argument("--force", action="store_true",
                     help="allow re-registering an already-recorded fixture id")
    args = ap.parse_args()
    if args.add_baseline:
        if not args.id or not args.path:
            ap.error("--add-baseline requires --id and --path")
        return cmd_add_baseline(args)
    return cmd_audit(args)


if __name__ == "__main__":
    sys.exit(main())
