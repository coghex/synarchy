#!/usr/bin/env python3
"""Current-mode save audit tests; no historical fixture discovery."""
from __future__ import annotations

import hashlib
import json
import os
import shutil
import sys
import tempfile
from pathlib import Path
from unittest import mock

sys.path.insert(0, str(Path(__file__).resolve().parent))
import save_compat_audit_codec as codec  # noqa: E402
import save_compat_audit_common as common  # noqa: E402
import save_compat_audit_current as current  # noqa: E402
import save_compat_audit_fingerprint as fingerprint  # noqa: E402
from selftestlib import expect  # noqa: E402


def test_current_record_checks_real_compiled_codec_and_records_identity() -> None:
    print("current selection: committed fixture identity, checksum, independent descriptors, "
          "live registrations and compiled-codec summary")
    record = current.load_record()
    violations = current.audit_record(record)
    expect(violations == [], f"expected current audit to pass, got {violations}")
    expect(len(record.get("fixtures", [])) > 0,
           "current self-test must select at least one fixture")
    fixture = record["fixtures"][0]
    expect(fixture.get("id") == "current-session-v1",
           f"expected the recorded current fixture identity, got {fixture.get('id')!r}")
    expect(len(fixture.get("expectedDescriptors", [])) > 0,
           "current fixture must have an independent nonempty descriptor oracle")


def test_current_selection_does_not_read_historical_manifest_or_files() -> None:
    print("synthetic tree with no historical manifest or fixture corpus passes current selection")
    source_record = current.load_record()
    fixture_entry = source_record["fixtures"][0]
    codec_exe, resolve_error = codec.resolve_codec_exe()
    expect(codec_exe is not None, f"compiled codec must already be built: {resolve_error}")
    live_registry = current.components.real_component_registry()
    with tempfile.TemporaryDirectory() as temp:
        root = Path(temp)
        for field in ("path", "summary"):
            destination = root / fixture_entry[field]
            destination.parent.mkdir(parents=True, exist_ok=True)
            shutil.copyfile(common.REPO_ROOT / fixture_entry[field], destination)
        record = json.loads(json.dumps(source_record))
        with mock.patch.object(current, "CURRENT_RECORD_PATH", root / "current-record.json"), \
             mock.patch.object(common, "REPO_ROOT", root), \
             mock.patch.dict(os.environ, {"SYNARCHY_SAVE_CODEC_EXE": codec_exe or ""}), \
             mock.patch.object(fingerprint, "current_envelope_version", return_value=1):
            # Deliberately provide only the current record and fixture.
            expect(current.audit_record(record, root=root, registry=live_registry) == [],
                   "current selection must pass without opening historical inputs")


def test_stale_or_dishonest_current_descriptors_fail_closed() -> None:
    print("current fixtures cannot stay selected after schema bumps or an oracle lie")
    with tempfile.TemporaryDirectory() as temp:
        root = Path(temp)
        data = b"current-fixture"
        (root / "fixture.bin").write_bytes(data)
        expected_summary = {"pages": [{"name": "page"}]}
        (root / "summary.json").write_text(json.dumps(expected_summary))
        registry = {
            "required": {"currentVersion": 2, "inputVersions": [2], "required": True},
            "missing": {"currentVersion": 1, "inputVersions": [1], "required": True},
        }
        fixture = {
            "id": "fixture", "path": "fixture.bin", "sizeBytes": len(data),
            "sha256": hashlib.sha256(data).hexdigest(), "summary": "summary.json",
            "summarySha256": hashlib.sha256((root / "summary.json").read_bytes()).hexdigest(),
            "provenance": "synthetic current fixture",
            "expectedDescriptors": [{"id": "required", "version": 1, "required": True}],
        }
        record = {"schemaVersion": 1, "framingVersion": 1,
                  "provenance": "synthetic", "fixtures": [fixture]}
        def descriptor_dump(paths):
            dishonest_actual = [{"id": "required", "version": 3, "required": True}]
            return {str(paths[0]): dishonest_actual}, ""
        def summary_dump(path, output):
            output.write_text(json.dumps(expected_summary))
            return True, ""
        with mock.patch.object(fingerprint, "current_envelope_version", return_value=1), \
             mock.patch.object(codec, "dump_fixture_descriptors", side_effect=descriptor_dump), \
             mock.patch.object(codec, "dump_canonical_summary", side_effect=summary_dump):
            violations = current.audit_record(record, root=root, registry=registry)
        expect(any("stale" in item for item in violations),
               f"expected stale current version to fail, got {violations}")
        expect(any("required" in item for item in violations),
               f"expected omitted current required registration to fail, got {violations}")
        expect(any("real descriptors differ" in item for item in violations),
               f"expected a descriptor oracle lie to fail, got {violations}")


def test_empty_and_duplicate_current_selections_are_rejected() -> None:
    print("current test selection rejects empty, duplicate, and missing registrations")
    import test_save_compat_audit as runner
    tests = list(runner.CURRENT_TESTS)
    expect(bool(tests), "current selection must be nonempty")
    expect(bool(runner.missing_current_tests([])),
           "empty current selection must be rejected")
    expect(bool(runner.duplicate_members(tests + tests)),
           "duplicate-member checker must identify duplicate current registrations")
    expect(bool(runner.missing_current_tests(tests[:-1])),
           "missing-current-test checker must identify a missing required registration")
    names = {fn.__name__ for fn in tests}
    expect(names == set(runner.REQUIRED_CURRENT_TEST_NAMES),
           "current registered tests must exactly match the required selection")
    expect(not any("reproducibility" in fn.__name__ for fn in runner.ALL_TESTS),
           "retired historical reproducibility tests must not be registered")


def test_current_seed_and_comparison_outcomes_use_current_inputs() -> None:
    print("current codec/helper and comparison outcomes: equal, mismatch, and current decode failure")
    record = current.load_record()
    source = common.REPO_ROOT / record["fixtures"][0]["path"]
    with tempfile.TemporaryDirectory(dir=common.REPO_ROOT) as temp:
        temp_path = Path(temp)
        equal_copy = temp_path / "equal.bin"
        changed = temp_path / "changed.bin"
        bad = temp_path / "bad-current.bin"
        spec = temp_path / "tags.json"
        spec.write_text('{"tiles":[{"page":"generated_page","x":1,"y":2,"z":7,"tags":["current-mode"]}]}')
        ok, tail = codec.seed_fixture_tags(source, spec, changed)
        expect(ok, f"expected current fixture tag-seeding to work, got {tail}")
        shutil.copyfile(source, equal_copy)
        outcome, report, detail = codec.compare_session_snapshots([source, equal_copy])
        expect(outcome == codec.COMPARE_OK,
               f"expected equal current saves, got {outcome}: {detail}")
        outcome, report, detail = codec.compare_session_snapshots([source, changed])
        expect(outcome == codec.COMPARE_MISMATCH,
               f"expected current tag edit to differ, got {outcome}: {detail}")
        bad.write_bytes(b"not a current envelope")
        outcome, report, detail = codec.compare_session_snapshots([source, bad])
        expect(outcome == codec.COMPARE_DECODE_FAILED,
               f"expected malformed current input to fail decode, got {outcome}: {detail}")
        descriptors, tail = codec.dump_fixture_descriptors([changed])
        expect(descriptors is not None, f"expected re-encoded current fixture to decode: {tail}")
        current_registry = {k: v for k, v in current.components.real_component_registry().items()
                            if k not in current.LEGACY_ONLY_COMPONENTS}
        expect(all(item["version"] == current_registry[item["id"]]["currentVersion"]
                   for item in descriptors[str(changed)]),
               "seed helper must encode every current component at its live version")


def test_corrupt_current_checksum_and_unsupported_framing_are_refused() -> None:
    print("current checksum corruption and unsupported framing stay distinct failures")
    base = current.load_record()
    with tempfile.TemporaryDirectory(dir=common.REPO_ROOT) as temp:
        root = Path(temp)
        original = base["fixtures"][0]
        original_bytes = (common.REPO_ROOT / original["path"]).read_bytes()
        unsupported = bytearray(original_bytes)
        unsupported[4:8] = (2).to_bytes(4, "big")
        fixture_path = root / "unsupported-version.bin"
        fixture_path.write_bytes(unsupported)
        summary_path = root / "summary.json"
        shutil.copyfile(common.REPO_ROOT / original["summary"], summary_path)
        fixture = json.loads(json.dumps(original))
        fixture.update(path="unsupported-version.bin", summary="summary.json",
                       sizeBytes=len(unsupported),
                       sha256=hashlib.sha256(unsupported).hexdigest())
        record = {"schemaVersion": 1, "framingVersion": 1,
                  "provenance": "synthetic", "fixtures": [fixture]}
        violations = current.audit_record(record, root=root)
        expect(any("compiled codec could not decode" in item for item in violations),
               f"expected compiled codec refusal for unsupported framing, got {violations}")
        decoded, detail = codec.dump_fixture_descriptors([fixture_path])
        expect(decoded is None and "UnsupportedEnvelopeVersion" in detail,
               f"expected explicit unsupported framing refusal, got {detail}")

        malformed = bytearray(original_bytes)
        malformed[0] ^= 0x7F
        malformed_path = root / "malformed.bin"
        malformed_path.write_bytes(malformed)
        decoded, detail = codec.dump_fixture_descriptors([malformed_path])
        expect(decoded is None and "BadMagic" in detail,
               f"expected malformed current framing refusal, got {detail}")
        fixture["sha256"] = "0" * 64
        violations = current.audit_record(record, root=root)
        expect(any("checksum mismatch" in item for item in violations),
               f"expected current checksum corruption to fail, got {violations}")


def test_compare_tool_error_is_not_a_fixture_verdict() -> None:
    print("current comparison helper failure remains distinct from fixture outcomes")
    record = current.load_record()
    source = common.REPO_ROOT / record["fixtures"][0]["path"]
    with mock.patch.dict(os.environ, {"SYNARCHY_SAVE_CODEC_EXE": "/missing/current-codec"}):
        outcome, report, detail = codec.compare_session_snapshots([source, source])
    expect(outcome == codec.COMPARE_ERROR,
           f"expected helper/tool failure, got {outcome}: {detail}")


TESTS = [
    test_current_record_checks_real_compiled_codec_and_records_identity,
    test_current_selection_does_not_read_historical_manifest_or_files,
    test_stale_or_dishonest_current_descriptors_fail_closed,
    test_empty_and_duplicate_current_selections_are_rejected,
    test_current_seed_and_comparison_outcomes_use_current_inputs,
    test_corrupt_current_checksum_and_unsupported_framing_are_refused,
    test_compare_tool_error_is_not_a_fixture_verdict,
]
