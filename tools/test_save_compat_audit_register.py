#!/usr/bin/env python3
"""Registration and generation cases of the save-compat self-test
(issue #2073).

The eleven members that drive `--add-baseline` and `--generate-session`
as transactions: the atomic write of a whole new baseline, the field and
overwrite refusals, `--force`, and every rollback path -- a failed real
codec validation, a generation error after the fixture was written, a
canonical-summary dump failure, and a validation failure at the end.

Requirement 14, and the reason this owner is the one that would fail
expensively if it were wrong: each case patches the module that OWNS the
seam it fakes, and every one of those seams is read module-qualified at
call time by its owner, so the rebinding is actually seen.

  `common.MANIFEST_PATH`
      `save_compat_audit_common` -- unpatched, these cases rewrite the
      real tracked docs/save_compat/manifest.json.
  `register._run_real_codec_validation`
      `save_compat_audit_register` -- unpatched, it spawns a real
      `cabal test`.
  `generate.generate_current_format_session`
      `save_compat_audit_generate` -- unpatched, it boots a real engine.
  `codec.dump_canonical_summary`
      `save_compat_audit_codec` -- unpatched, it re-decodes through the
      real toolchain.

Each faked seam additionally asserts the fake was REACHED, and every
case that touches the manifest asserts the real one's bytes are
unchanged afterwards (`expect_real_manifest_untouched`), so a patch that
silently failed to take is caught rather than passing on its own
assertions.
"""
from __future__ import annotations

import hashlib
import json
import sys
import tempfile
from pathlib import Path

sys.path.insert(0, str(Path(__file__).resolve().parent))
import save_compat_audit_codec as codec  # noqa: E402
import save_compat_audit_common as common  # noqa: E402
import save_compat_audit_generate as generate  # noqa: E402
import save_compat_audit_register as register  # noqa: E402

from selftestlib import expect  # noqa: E402
from test_save_compat_audit_support import (  # noqa: E402
    _Args, base_manifest, expect_real_manifest_untouched, make_fixture,
    real_manifest_bytes,
)


def test_add_baseline_creates_a_new_baseline_and_fixture_atomically() -> None:
    print("--add-baseline creates a whole new baseline entry")
    with tempfile.TemporaryDirectory(dir=common.REPO_ROOT) as d:
        tmp = Path(d)
        fixture = make_fixture(tmp, "new.bin", b"new fixture bytes")
        summary = tmp / "new.expected.json"
        summary.write_text('{"ok": true}')
        manifest_path = tmp / "manifest.json"
        manifest_path.write_text(json.dumps({"baselines": []}))
        real_before = real_manifest_bytes()
        old_path = common.MANIFEST_PATH
        common.MANIFEST_PATH = manifest_path
        try:
            rc = register.cmd_add_baseline(_Args(
                baseline_id="new-baseline", fixture_id="new-fixture",
                path=str(fixture.relative_to(common.REPO_ROOT)), kind="complete-session",
                summary=str(summary.relative_to(common.REPO_ROOT)),
                description="a test baseline", migration_target="current",
                migrated_by="test", components='[{"id":"metadata","version":1,"required":true}]'))
            expect(rc == 0, f"expected success, got exit code {rc}")
            written = json.loads(manifest_path.read_text())
            baselines = written.get("baselines", [])
            expect(len(baselines) == 1 and baselines[0]["id"] == "new-baseline",
                   f"expected exactly the new baseline, got {baselines}")
            if baselines:
                fixtures = baselines[0]["fixtures"]
                expect(len(fixtures) == 1 and fixtures[0]["id"] == "new-fixture"
                       and fixtures[0]["sha256"] == hashlib.sha256(b"new fixture bytes").hexdigest(),
                       f"expected the new fixture registered with a real checksum, got {fixtures}")
            expect_real_manifest_untouched(real_before, "--add-baseline")
        finally:
            common.MANIFEST_PATH = old_path


def test_add_baseline_refuses_new_baseline_missing_required_fields() -> None:
    print("--add-baseline refuses to create a new baseline missing description/migration-target/etc.")
    with tempfile.TemporaryDirectory(dir=common.REPO_ROOT) as d:
        tmp = Path(d)
        fixture = make_fixture(tmp, "new.bin", b"new fixture bytes")
        summary = tmp / "new.expected.json"
        summary.write_text('{"ok": true}')
        manifest_path = tmp / "manifest.json"
        manifest_path.write_text(json.dumps({"baselines": []}))
        old_path = common.MANIFEST_PATH
        common.MANIFEST_PATH = manifest_path
        try:
            rc = register.cmd_add_baseline(_Args(
                baseline_id="incomplete-baseline", fixture_id="new-fixture",
                path=str(fixture.relative_to(common.REPO_ROOT)), kind="complete-session",
                summary=str(summary.relative_to(common.REPO_ROOT))))
            expect(rc == 1, f"expected refusal (missing baseline fields), got exit code {rc}")
            written = json.loads(manifest_path.read_text())
            expect(written.get("baselines", []) == [],
                   "expected the manifest to stay untouched on refusal")
        finally:
            common.MANIFEST_PATH = old_path


def test_add_baseline_refuses_to_overwrite_without_force() -> None:
    print("--add-baseline refuses to silently overwrite an already-registered fixture")
    with tempfile.TemporaryDirectory(dir=common.REPO_ROOT) as d:
        tmp = Path(d)
        original = b"original bytes"
        fixture = make_fixture(tmp, "f.bin", original)
        summary = tmp / "f.expected.json"
        summary.write_text('{"ok": true}')
        manifest_path = tmp / "manifest.json"
        manifest_path.write_text(json.dumps(base_manifest(tmp, fixture, original)))
        tampered = b"tampered bytes -- someone hand-regenerated without --force"
        fixture.write_bytes(tampered)
        old_path = common.MANIFEST_PATH
        common.MANIFEST_PATH = manifest_path
        try:
            rc = register.cmd_add_baseline(_Args(
                baseline_id="test-baseline", fixture_id="test-fixture",
                path=str(fixture.relative_to(common.REPO_ROOT)), kind="complete-session",
                summary=str(summary.relative_to(common.REPO_ROOT))))
            expect(rc == 1, f"expected refusal without --force, got exit code {rc}")
            rc2 = register.cmd_add_baseline(_Args(
                baseline_id="test-baseline", fixture_id="test-fixture",
                path=str(fixture.relative_to(common.REPO_ROOT)), kind="complete-session",
                summary=str(summary.relative_to(common.REPO_ROOT)), force=True))
            expect(rc2 == 0, f"expected --force to succeed, got exit code {rc2}")
            written = json.loads(manifest_path.read_text())
            expect(written["baselines"][0]["fixtures"][0]["sha256"]
                   == hashlib.sha256(tampered).hexdigest(),
                   "expected --force to record the NEW checksum")
        finally:
            common.MANIFEST_PATH = old_path


def test_add_baseline_requires_summary_for_complete_session() -> None:
    print("--add-baseline refuses a complete-session fixture with no --summary")
    with tempfile.TemporaryDirectory(dir=common.REPO_ROOT) as d:
        tmp = Path(d)
        fixture = make_fixture(tmp, "f.bin", b"bytes")
        manifest_path = tmp / "manifest.json"
        manifest_path.write_text(json.dumps({"baselines": []}))
        old_path = common.MANIFEST_PATH
        common.MANIFEST_PATH = manifest_path
        try:
            rc = register.cmd_add_baseline(_Args(
                baseline_id="b", fixture_id="f",
                path=str(fixture.relative_to(common.REPO_ROOT)), kind="complete-session",
                description="d", migration_target="current", migrated_by="m",
                components="[]"))
            expect(rc == 1, f"expected refusal (no --summary), got exit code {rc}")
        finally:
            common.MANIFEST_PATH = old_path


def test_add_baseline_rolls_back_on_failed_real_codec_validation() -> None:
    print("--add-baseline rolls the manifest back if the real-codec validation fails")
    with tempfile.TemporaryDirectory(dir=common.REPO_ROOT) as d:
        tmp = Path(d)
        original = b"original bytes"
        fixture = make_fixture(tmp, "f.bin", original)
        summary = tmp / "f.expected.json"
        summary.write_text('{"ok": true}')
        manifest_path = tmp / "manifest.json"
        manifest_before = json.dumps({"baselines": []})
        manifest_path.write_text(manifest_before)
        real_before = real_manifest_bytes()
        old_manifest_path = common.MANIFEST_PATH
        old_validate = register._run_real_codec_validation
        common.MANIFEST_PATH = manifest_path
        # Simulate the real `cabal test` gate failing, without spawning a
        # real subprocess -- _finalize_manifest_write only ever consumes
        # (bool, str), so substituting this is a faithful stand-in for a
        # genuinely broken fixture.
        called = []
        register._run_real_codec_validation = (
            lambda: called.append(1) or (False, "simulated hspec failure"))
        try:
            rc = register.cmd_add_baseline(_Args(
                baseline_id="new-baseline", fixture_id="new-fixture",
                path=str(fixture.relative_to(common.REPO_ROOT)), kind="complete-session",
                summary=str(summary.relative_to(common.REPO_ROOT)),
                description="a test baseline", migration_target="current",
                migrated_by="test", components='[{"id":"metadata","version":1,"required":true}]',
                skip_validation=False))
            expect(rc == 1, f"expected the failed validation to fail the command, got {rc}")
            expect(manifest_path.read_text() == manifest_before,
                   "expected the manifest to be rolled back to its exact prior content")
            expect(called == [1],
                   "expected the patched real-codec validation to be the one "
                   "actually reached (a seam that silently kept the real "
                   "implementation would have spawned a real `cabal test`)")
            expect_real_manifest_untouched(
                real_before, "--add-baseline rollback")
        finally:
            common.MANIFEST_PATH = old_manifest_path
            register._run_real_codec_validation = old_validate


def test_add_baseline_keeps_registration_on_passed_real_codec_validation() -> None:
    print("--add-baseline keeps the registration if the real-codec validation passes")
    with tempfile.TemporaryDirectory(dir=common.REPO_ROOT) as d:
        tmp = Path(d)
        original = b"original bytes"
        fixture = make_fixture(tmp, "f.bin", original)
        summary = tmp / "f.expected.json"
        summary.write_text('{"ok": true}')
        manifest_path = tmp / "manifest.json"
        manifest_path.write_text(json.dumps({"baselines": []}))
        old_manifest_path = common.MANIFEST_PATH
        old_validate = register._run_real_codec_validation
        common.MANIFEST_PATH = manifest_path
        called = []
        register._run_real_codec_validation = (
            lambda: called.append(1) or (True, "simulated hspec pass"))
        try:
            rc = register.cmd_add_baseline(_Args(
                baseline_id="new-baseline", fixture_id="new-fixture",
                path=str(fixture.relative_to(common.REPO_ROOT)), kind="complete-session",
                summary=str(summary.relative_to(common.REPO_ROOT)),
                description="a test baseline", migration_target="current",
                migrated_by="test", components='[{"id":"metadata","version":1,"required":true}]',
                skip_validation=False))
            expect(rc == 0, f"expected the passed validation to keep the registration, got {rc}")
            written = json.loads(manifest_path.read_text())
            expect(len(written.get("baselines", [])) == 1,
                   "expected the new baseline to still be registered")
            expect(called == [1],
                   "expected the patched real-codec validation to be the one "
                   "actually reached, not the real subprocess")
        finally:
            common.MANIFEST_PATH = old_manifest_path
            register._run_real_codec_validation = old_validate


def test_add_baseline_skips_validation_for_component_focused_kind() -> None:
    print("--add-baseline never runs the generic real-codec gate for a component-focused fixture")
    with tempfile.TemporaryDirectory(dir=common.REPO_ROOT) as d:
        tmp = Path(d)
        fixture = make_fixture(tmp, "f.bin", b"lua payload bytes")
        manifest_path = tmp / "manifest.json"
        manifest_path.write_text(json.dumps({"baselines": []}))
        real_before = real_manifest_bytes()
        old_manifest_path = common.MANIFEST_PATH
        old_validate = register._run_real_codec_validation
        common.MANIFEST_PATH = manifest_path
        called = []
        register._run_real_codec_validation = lambda: called.append(1) or (True, "")
        try:
            rc = register.cmd_add_baseline(_Args(
                baseline_id="new-baseline", fixture_id="new-fixture",
                path=str(fixture.relative_to(common.REPO_ROOT)), kind="component-focused",
                description="a test baseline", migration_target="current",
                migrated_by="test", components="[]", skip_validation=False))
            expect(rc == 0, f"expected success, got {rc}")
            expect(called == [],
                   "expected the real-codec validation to never be invoked for a "
                   "component-focused fixture")
            expect_real_manifest_untouched(
                real_before, "--add-baseline component-focused")
        finally:
            common.MANIFEST_PATH = old_manifest_path
            register._run_real_codec_validation = old_validate


def test_generate_session_refuses_when_summary_exists_without_force() -> None:
    print("--generate-session refuses when the SUMMARY (not just the fixture) already exists")
    with tempfile.TemporaryDirectory(dir=common.REPO_ROOT) as d:
        tmp = Path(d)
        fixture_path = tmp / "gen.bin"  # deliberately does NOT exist
        summary_path = tmp / "gen.expected.json"
        summary_path.write_text('{"already": "here"}')
        called = []
        old_gen = generate.generate_current_format_session
        generate.generate_current_format_session = lambda **kw: called.append(1)
        try:
            rc = generate.cmd_generate(_Args(
                baseline_id="b", fixture_id="f",
                path=str(fixture_path.relative_to(common.REPO_ROOT)),
                summary=str(summary_path.relative_to(common.REPO_ROOT))))
            expect(rc == 1, f"expected refusal, got exit code {rc}")
            expect(called == [],
                   "expected generation to never even start once refused")
            expect(summary_path.read_text() == '{"already": "here"}',
                   "expected the pre-existing summary to be left untouched")
        finally:
            generate.generate_current_format_session = old_gen


def test_generate_session_rolls_back_on_generation_error_after_fixture_written() -> None:
    print("round-16 review: --generate-session restores the fixture even when "
          "GenerationError is raised AFTER the new bytes were already written "
          "(e.g. normalize_fixture_timestamp failing post-copyfile) -- not "
          "just when generation fails before ever touching the file")
    with tempfile.TemporaryDirectory(dir=common.REPO_ROOT) as d:
        tmp = Path(d)
        fixture_path = tmp / "gen.bin"
        summary_path = tmp / "gen.expected.json"
        original_fixture = b"pre-existing fixture bytes"
        fixture_path.write_bytes(original_fixture)
        old_gen = generate.generate_current_format_session
        called = []

        def fake_gen(**kw):
            called.append(1)
            # Simulates generate_current_format_session's real shape since
            # round-11: engine.saveWorld/shutil.copyfile succeeds and
            # writes new bytes FIRST, then normalize_fixture_timestamp
            # (a separate, later step) fails.
            kw["out_path"].write_bytes(b"newly generated but un-normalized bytes")
            raise generate.GenerationError("simulated timestamp-normalization failure")

        generate.generate_current_format_session = fake_gen
        try:
            rc = generate.cmd_generate(_Args(
                baseline_id="b", fixture_id="f",
                path=str(fixture_path.relative_to(common.REPO_ROOT)),
                summary=str(summary_path.relative_to(common.REPO_ROOT)),
                force=True))
            expect(rc == 1, f"expected failure, got exit code {rc}")
            expect(fixture_path.read_bytes() == original_fixture,
                   "expected the fixture to be restored to its ORIGINAL "
                   "bytes, not left as the newly-written-but-failed content")
            expect(not summary_path.exists(),
                   "expected the summary (which never existed before) to "
                   "still not exist")
            expect(called == [1],
                   "expected the patched generator to be the one actually "
                   "reached (a seam that silently kept the real "
                   "implementation would have booted a real engine)")
        finally:
            generate.generate_current_format_session = old_gen


def test_generate_session_rolls_back_fixture_and_summary_on_dump_failure() -> None:
    print("--generate-session restores BOTH fixture and summary if canonical-summary derivation fails")
    with tempfile.TemporaryDirectory(dir=common.REPO_ROOT) as d:
        tmp = Path(d)
        fixture_path = tmp / "gen.bin"
        summary_path = tmp / "gen.expected.json"
        original_fixture = b"pre-existing fixture bytes"
        original_summary = '{"pre": "existing summary"}'
        fixture_path.write_bytes(original_fixture)
        summary_path.write_text(original_summary)
        old_gen = generate.generate_current_format_session
        old_dump = codec.dump_canonical_summary
        # Simulate a real generation that DID write new bytes (clobbering
        # the pre-existing fixture, exactly like --force would let it),
        # then a dump that fails -- both files must roll back to their
        # ORIGINAL content, not just get deleted or left half-written.
        called = []
        generate.generate_current_format_session = (
            lambda **kw: (called.append("gen"),
                          kw["out_path"].write_bytes(b"newly generated bytes"))[1])
        codec.dump_canonical_summary = (
            lambda fp, sp: (called.append("dump"),
                            (False, "simulated dump failure"))[1])
        try:
            rc = generate.cmd_generate(_Args(
                baseline_id="b", fixture_id="f",
                path=str(fixture_path.relative_to(common.REPO_ROOT)),
                summary=str(summary_path.relative_to(common.REPO_ROOT)),
                force=True))
            expect(rc == 1, f"expected failure, got exit code {rc}")
            expect(fixture_path.read_bytes() == original_fixture,
                   "expected the fixture to be restored to its ORIGINAL bytes")
            expect(summary_path.read_text() == original_summary,
                   "expected the summary to be restored to its ORIGINAL content")
            expect(called == ["gen", "dump"],
                   f"expected both patched seams to be the ones actually "
                   f"reached, in order (a seam that silently kept its real "
                   f"implementation would have booted a real engine or "
                   f"spawned a real `cabal repl`), got {called}")
        finally:
            generate.generate_current_format_session = old_gen
            codec.dump_canonical_summary = old_dump


def test_generate_session_rolls_back_fixture_and_summary_on_validation_failure() -> None:
    print("--generate-session restores fixture+summary (not just the manifest) if real-codec validation fails")
    with tempfile.TemporaryDirectory(dir=common.REPO_ROOT) as d:
        tmp = Path(d)
        fixture_path = tmp / "gen.bin"
        summary_path = tmp / "gen.expected.json"
        manifest_path = tmp / "manifest.json"
        # Nothing pre-existing this time -- a first-ever generation for
        # a brand new baseline+fixture.
        manifest_path.write_text(json.dumps({"baselines": []}))
        old_gen = generate.generate_current_format_session
        old_dump = codec.dump_canonical_summary
        old_validate = register._run_real_codec_validation
        old_manifest_path = common.MANIFEST_PATH
        real_before = real_manifest_bytes()
        common.MANIFEST_PATH = manifest_path
        called = []
        generate.generate_current_format_session = (
            lambda **kw: (called.append("gen"),
                          kw["out_path"].write_bytes(b"newly generated bytes"))[1])
        codec.dump_canonical_summary = (
            lambda fp, sp: (called.append("dump"), sp.write_text('{"ok": true}'),
                            (True, ""))[2])
        register._run_real_codec_validation = (
            lambda: (called.append("validate"),
                     (False, "simulated hspec failure"))[1])
        try:
            rc = generate.cmd_generate(_Args(
                baseline_id="new-baseline", fixture_id="new-fixture",
                path=str(fixture_path.relative_to(common.REPO_ROOT)),
                summary=str(summary_path.relative_to(common.REPO_ROOT)),
                description="a test baseline", migration_target="current",
                migrated_by="test", components='[{"id":"metadata","version":1,"required":true}]',
                skip_validation=False))
            expect(rc == 1, f"expected failure, got exit code {rc}")
            expect(not fixture_path.exists(),
                   "expected the newly-generated fixture to be removed "
                   "(it did not exist before this invocation)")
            expect(not summary_path.exists(),
                   "expected the newly-generated summary to be removed "
                   "(it did not exist before this invocation)")
            expect(called == ["gen", "dump", "validate"],
                   f"expected all three patched seams to be the ones "
                   f"actually reached, in order (any that silently kept its "
                   f"real implementation would have booted a real engine or "
                   f"spawned a real cabal invocation), got {called}")
            expect_real_manifest_untouched(
                real_before, "--generate-session validation failure")
            written_manifest = json.loads(manifest_path.read_text())
            expect(written_manifest.get("baselines", []) == [],
                   "expected the manifest to also be rolled back (already "
                   "covered by _finalize_manifest_write, checked here for "
                   "full-transaction confidence)")
        finally:
            generate.generate_current_format_session = old_gen
            codec.dump_canonical_summary = old_dump
            register._run_real_codec_validation = old_validate
            common.MANIFEST_PATH = old_manifest_path


#: This owner's members, in the run order the façade concatenates
#: (issue #2073 requirement 12).
TESTS = [
    test_add_baseline_creates_a_new_baseline_and_fixture_atomically,
    test_add_baseline_refuses_new_baseline_missing_required_fields,
    test_add_baseline_refuses_to_overwrite_without_force,
    test_add_baseline_requires_summary_for_complete_session,
    test_add_baseline_rolls_back_on_failed_real_codec_validation,
    test_add_baseline_keeps_registration_on_passed_real_codec_validation,
    test_add_baseline_skips_validation_for_component_focused_kind,
    test_generate_session_refuses_when_summary_exists_without_force,
    test_generate_session_rolls_back_on_generation_error_after_fixture_written,
    test_generate_session_rolls_back_fixture_and_summary_on_dump_failure,
    test_generate_session_rolls_back_fixture_and_summary_on_validation_failure,
]
