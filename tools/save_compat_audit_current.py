#!/usr/bin/env python3
"""Current-save audit, isolated from the historical compatibility corpus.

The current record is deliberately stored outside ``docs/save_compat`` and
``test-headless/data/save-compat``: those paths belong to the historical
audit and its fixture-discovery tests until PSP-5 retires them.
"""
from __future__ import annotations

import hashlib
import json
import tempfile
from pathlib import Path

import save_compat_audit_codec as codec
import save_compat_audit_common as common
import save_compat_audit_components as components
import save_compat_audit_fingerprint as fingerprint


CURRENT_RECORD_PATH = common.REPO_ROOT / "tools" / "save_compat_current" / "record.json"
LEGACY_ONLY_COMPONENTS = {"session", "lua-state"}


def load_record(path: Path | None = None) -> dict:
    """Read the replaceable current-fixture record only."""
    record_path = CURRENT_RECORD_PATH if path is None else path
    return json.loads(record_path.read_text(encoding="utf-8"))


def _sha256(path: Path) -> str:
    return hashlib.sha256(path.read_bytes()).hexdigest()


def _safe_path(root: Path, value: object, where: str,
               violations: list[str]) -> Path | None:
    if not isinstance(value, str) or not value:
        violations.append(f"{where} has no path")
        return None
    candidate = (root / value).resolve()
    if not candidate.is_relative_to(root.resolve()):
        violations.append(f"{where} path escapes the repository: {value}")
        return None
    return candidate


def _expected_descriptors(fixture: dict, where: str,
                          violations: list[str]) -> list[dict] | None:
    expected = fixture.get("expectedDescriptors")
    if not isinstance(expected, list) or not expected:
        violations.append(f"{where} has no nonempty independent expectedDescriptors")
        return None
    ids: set[str] = set()
    for descriptor in expected:
        if not isinstance(descriptor, dict):
            violations.append(f"{where} has a malformed expected descriptor")
            return None
        cid, version, required = (descriptor.get("id"), descriptor.get("version"),
                                  descriptor.get("required"))
        if (not isinstance(cid, str) or not cid or type(version) is not int
                or type(required) is not bool):
            violations.append(f"{where} has a malformed expected descriptor: {descriptor!r}")
            return None
        if cid in ids:
            violations.append(f"{where} repeats expected component {cid}")
        ids.add(cid)
    return expected


def audit_record(record: dict, *, root: Path | None = None,
                 registry: dict | None = None) -> list[str]:
    """Check record integrity, current declarations and real codec output.

    This path never loads the historical manifest or scans its fixture
    directory. A codec decode is reached only for the explicitly recorded
    current fixture paths.
    """
    root = common.REPO_ROOT if root is None else root
    violations: list[str] = []
    if not isinstance(record, dict) or record.get("schemaVersion") != 1:
        return ["current fixture record must be an object with schemaVersion 1"]
    if not isinstance(record.get("provenance"), str) or not record["provenance"].strip():
        violations.append("current fixture record has no provenance")
    try:
        current_framing = fingerprint.current_envelope_version()
    except Exception as error:
        current_framing = None
        violations.append(f"could not read current envelope framing version: {error}")
    if record.get("framingVersion") != current_framing:
        violations.append(
            f"current fixture record framingVersion ({record.get('framingVersion')}) "
            f"does not match currentEnvelopeVersion ({current_framing}); replace current fixtures")

    if registry is None:
        try:
            registry = components.real_component_registry()
        except Exception as error:
            violations.append(f"could not discover live save registrations: {error}")
            registry = {}
    live = {cid: spec for cid, spec in registry.items()
            if cid not in LEGACY_ONLY_COMPONENTS}

    fixtures = record.get("fixtures")
    if not isinstance(fixtures, list) or not fixtures:
        violations.append("current fixture record must select at least one fixture")
        return violations
    fixture_ids: set[str] = set()
    fixture_paths: set[str] = set()
    decode_paths: list[tuple[dict, Path, list[dict]]] = []
    for fixture in fixtures:
        if not isinstance(fixture, dict):
            violations.append("current fixture record contains a malformed fixture")
            continue
        fid = fixture.get("id")
        where = f"current fixture {fid!r}"
        if not isinstance(fid, str) or not fid:
            violations.append("current fixture has no id")
            continue
        if fid in fixture_ids:
            violations.append(f"duplicate current fixture id {fid}")
        fixture_ids.add(fid)
        if not isinstance(fixture.get("provenance"), str) or not fixture["provenance"].strip():
            violations.append(f"{where} has no provenance")
        expected = _expected_descriptors(fixture, where, violations)
        path = _safe_path(root, fixture.get("path"), where, violations)
        summary_path = _safe_path(root, fixture.get("summary"), where + " summary", violations)
        if path is None or summary_path is None:
            continue
        relpath = path.relative_to(root.resolve()).as_posix()
        if relpath in fixture_paths:
            violations.append(f"duplicate current fixture path {relpath}")
        fixture_paths.add(relpath)
        if not path.is_file():
            violations.append(f"{where} bytes are missing at {relpath}")
            continue
        if path.stat().st_size <= 0:
            violations.append(f"{where} bytes are empty")
            continue
        if fixture.get("sizeBytes") != path.stat().st_size:
            violations.append(f"{where} sizeBytes does not match the recorded file")
        if fixture.get("sha256") != _sha256(path):
            violations.append(f"{where} checksum mismatch; replace the current fixture record")
            continue
        if not summary_path.is_file() or summary_path.stat().st_size <= 0:
            violations.append(f"{where} summary is missing or empty")
            continue
        if fixture.get("summarySha256") != _sha256(summary_path):
            violations.append(f"{where} summary checksum mismatch")
            continue
        try:
            summary = json.loads(summary_path.read_text(encoding="utf-8"))
        except (OSError, json.JSONDecodeError) as error:
            violations.append(f"{where} summary is not readable JSON: {error}")
            continue
        if not isinstance(summary, dict) or not summary.get("pages"):
            violations.append(f"{where} summary has no nonempty session data")
            continue
        if expected is None:
            continue
        expected_ids = {item["id"] for item in expected}
        for descriptor in expected:
            cid = descriptor["id"]
            actual_live = live.get(cid)
            if actual_live is None:
                violations.append(f"{where} declares unregistered current component {cid}")
                continue
            if (descriptor["version"] != actual_live["currentVersion"]
                    or descriptor["required"] != actual_live["required"]):
                violations.append(
                    f"{where} component {cid} is stale; expected current version "
                    f"{actual_live['currentVersion']} required={actual_live['required']}; "
                    "replace the current fixture")
        missing_required = sorted(
            cid for cid, spec in live.items()
            if spec["required"] and cid not in expected_ids)
        if missing_required:
            violations.append(f"{where} omits required current components: "
                              f"{', '.join(missing_required)}")
        if set(expected_ids) - set(live):
            continue
        decode_paths.append((fixture, path, expected))

    if decode_paths:
        decoded, tail = codec.dump_fixture_descriptors([path for _, path, _ in decode_paths])
        if decoded is None:
            violations.append(f"compiled codec could not decode current fixtures: {tail}")
        else:
            for fixture, path, expected in decode_paths:
                actual = decoded.get(str(path))
                if actual != expected:
                    violations.append(
                        f"current fixture {fixture.get('id')} real descriptors differ from "
                        "the independent expectedDescriptors record; replace the fixture")
                    continue
                with tempfile.TemporaryDirectory(dir=root) as temp_dir:
                    actual_summary = Path(temp_dir) / "summary.json"
                    ok, tail = codec.dump_canonical_summary(path, actual_summary)
                    if not ok:
                        violations.append(
                            f"compiled codec could not summarize current fixture "
                            f"{fixture.get('id')}: {tail}")
                        continue
                    try:
                        recorded_summary = json.loads(
                            _safe_path(root, fixture.get("summary"),
                                       f"current fixture {fixture.get('id')} summary",
                                       violations).read_text(encoding="utf-8"))
                        decoded_summary = json.loads(actual_summary.read_text(encoding="utf-8"))
                    except (OSError, json.JSONDecodeError, AttributeError) as error:
                        violations.append(f"current fixture summary comparison failed: {error}")
                        continue
                    if decoded_summary != recorded_summary:
                        violations.append(
                            f"current fixture {fixture.get('id')} decoded summary differs "
                            "from its committed regression summary")
    return violations


def cmd_audit() -> int:
    try:
        record = load_record()
    except (OSError, json.JSONDecodeError) as error:
        print(f"current save-compat record could not be read: {error}")
        return 1
    violations = audit_record(record)
    if violations:
        print(f"{len(violations)} current save-compat violation(s):")
        for violation in violations:
            print(f"  - {violation}")
        return 1
    fixtures = record["fixtures"]
    names = ", ".join(item["id"] for item in fixtures)
    print(f"current save-compat audit: {len(fixtures)} current fixture(s) verified "
          f"({names}); compiled codec descriptors and summaries match")
    return 0
