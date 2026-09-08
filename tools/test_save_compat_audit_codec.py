#!/usr/bin/env python3
"""The save-compat self-test's codec-bridge owner (issue #2273).

`tools/save_compat_audit_codec.py` reaches the REAL Haskell save codec.
Until #2273 it did so by feeding three GHCi programs to
`cabal repl test:synarchy-test-headless`; it now execs the compiled
`exe:synarchy-save-codec` that `cabal build all` produces. That is a
change of how the production codec is REACHED, so what this owner covers
is exactly the two things a reader has to be able to trust about it:

  parity
      The compiled helper's output is the output the tracked corpus was
      built from. `test_helper_summary_matches_every_generated_expectation`
      compares the canonical summary BYTE-for-byte against every tracked
      `*.expected.json` the tool itself generated, and
      `test_normalizing_a_tracked_fixture_reproduces_its_tracked_bytes`
      does the same for the fixed-timestamp re-encode, which is the
      operation the tracked fixtures' own sha256s were computed after.

  diagnosis
      The Python side still distinguishes a decode failure from a helper
      that exited successfully having written nothing (#2273 requirement
      4), and a decode failure still names the fixture and the codec's
      own error text (requirement 5). Neither branch had a test before
      #2273 -- `tools/save_compat_audit_manifest.py`'s failure and
      missing-entry paths were reachable only through a `cabal repl`
      nobody was willing to spend a self-test on.

The third distinction requirement 4 names, manifest-versus-real-descriptor
mismatch, is NOT duplicated here: it belongs to the manifest audit that
emits those violations, and lives with the coverage owner as
`test_detects_manifest_version_claim_not_backed_by_real_fixture_bytes`.
What this owner adds beside it is the successful direction -- that the
real corpus decodes and that its descriptors back the manifest's claims.

Every member here needs the built helper, exactly as the real audit does
(`save_compat_audit.py`'s own run decodes the tracked corpus through it),
so this owner adds no build requirement the module did not already have.
It is deliberately NOT the reproducibility owner: these are ordinary
cheap members now that no member starts an interpreter, and putting one
there would make it expensive by construction.
"""
from __future__ import annotations

import json
import os
import shutil
import stat
import sys
import tempfile
from pathlib import Path

sys.path.insert(0, str(Path(__file__).resolve().parent))
import save_compat_audit_codec as codec  # noqa: E402
import save_compat_audit_common as common  # noqa: E402
import save_compat_audit_manifest as manifest_audit  # noqa: E402

from selftestlib import expect  # noqa: E402


#: A hand-authored expectation carries a `$comment` saying what it was
#: built to prove; one the tool GENERATED is exactly what
#: `Aeson.encode` wrote and is therefore byte-comparable. Only the
#: second kind can be a byte-parity oracle, and this is the property
#: that separates them -- not a list of names that would silently stop
#: covering a fixture added later.
_GENERATED_EXPECTATION_MARKER = "$comment"


def _tool_generated_expectations() -> list[tuple[Path, Path]]:
    """(fixture, expected-summary) for every tracked pair the tool wrote."""
    pairs = []
    for expected in sorted(common.FIXTURE_DATA_DIR.glob("*.expected.json")):
        fixture = expected.with_name(expected.name[:-len(".expected.json")] + ".bin")
        if not fixture.exists():
            continue
        try:
            document = json.loads(expected.read_text(encoding="utf-8"))
        except json.JSONDecodeError:
            continue
        if isinstance(document, dict) and _GENERATED_EXPECTATION_MARKER in document:
            continue
        pairs.append((fixture, expected))
    return pairs


def _fake_helper(tmp: Path, marker: str) -> Path:
    """A stand-in helper that prints `marker` and exits 0 writing nothing.

    The one thing a real `exe:synarchy-save-codec` cannot be made to do,
    and the exact shape of the failure requirement 4 asks the Python side
    to keep distinguishing.
    """
    script = tmp / "fake_codec_helper"
    script.write_text(f'#!/bin/sh\necho "{marker}"\nexit 0\n', encoding="utf-8")
    script.chmod(script.stat().st_mode | stat.S_IXUSR)
    return script


def _with_helper(path: Path):
    """Point the bridge's pre-resolved handoff at `path` for one block."""
    class _Scope:
        def __enter__(self) -> None:
            self.previous = os.environ.get(codec.ENV_CODEC_EXE)
            os.environ[codec.ENV_CODEC_EXE] = str(path)

        def __exit__(self, *_exc) -> None:
            if self.previous is None:
                os.environ.pop(codec.ENV_CODEC_EXE, None)
            else:
                os.environ[codec.ENV_CODEC_EXE] = self.previous
    return _Scope()


def test_helper_summary_matches_every_generated_expectation() -> None:
    print("issue #2273: the COMPILED helper's canonical summary is "
          "byte-identical to every tracked *.expected.json the tool itself "
          "generated -- the parity requirement the GHCi removal turns on")
    pairs = _tool_generated_expectations()
    expect(len(pairs) >= 20,
           f"expected the tracked corpus to still carry a substantial set of "
           f"tool-generated expectations to compare against, found "
           f"{len(pairs)} (a vacuous pass otherwise)")
    with tempfile.TemporaryDirectory(dir=common.REPO_ROOT) as d:
        out = Path(d) / "summary.json"
        for fixture, expected in pairs:
            ok, tail = codec.dump_canonical_summary(fixture, out)
            if not ok:
                expect(False, f"expected {fixture.name} to dump, got: {tail}")
                continue
            expect(out.read_bytes() == expected.read_bytes(),
                   f"expected the helper's summary for {fixture.name} to be "
                   f"byte-identical to its tracked {expected.name}")


def test_normalizing_a_tracked_fixture_reproduces_its_tracked_bytes() -> None:
    print("issue #2273: re-running the fixed-timestamp re-encode over an "
          "already-normalized tracked fixture reproduces its bytes exactly, "
          "so the operation the corpus's sha256s were taken after is both "
          "byte-idempotent and unchanged by the move off GHCi")
    fixture = common.FIXTURE_DATA_DIR / "u1-generated-world-identity.bin"
    if not fixture.exists():
        expect(False, f"expected the tracked fixture to exist at {fixture}")
        return
    tracked = fixture.read_bytes()
    with tempfile.TemporaryDirectory(dir=common.REPO_ROOT) as d:
        copy = Path(d) / "fixture.bin"
        shutil.copyfile(fixture, copy)
        ok, tail = codec.normalize_fixture_timestamp(copy)
        expect(ok, f"expected normalization to succeed, got: {tail}")
        expect(copy.read_bytes() == tracked,
               "expected one normalization pass over an already-normalized "
               "tracked fixture to reproduce its tracked bytes exactly")
        ok, tail = codec.normalize_fixture_timestamp(copy)
        expect(ok, f"expected a second normalization to succeed, got: {tail}")
        expect(copy.read_bytes() == tracked,
               "expected normalization to be byte-idempotent")
    expect(fixture.read_bytes() == tracked,
           "expected the TRACKED fixture itself to be untouched (this member "
           "normalizes a copy; writing through to the corpus would be the "
           "quiet failure the copy exists to prevent)")


def test_descriptor_dump_covers_every_requested_fixture_in_codec_order() -> None:
    print("issue #2273: the compiled helper decodes every tracked "
          "complete-session fixture, keys the dump by the exact path asked "
          "for, and lists each manifest's descriptors in the codec's own "
          "canonical component-id order")
    manifest = manifest_audit.load_manifest()
    requested = []
    for baseline in manifest.get("baselines", []):
        for fixture in baseline.get("fixtures", []):
            if fixture.get("kind") != "complete-session" or not fixture.get("sha256"):
                continue
            path = common.REPO_ROOT / fixture["path"]
            if path.exists():
                requested.append(path)
    expect(len(requested) >= 20,
           f"expected the manifest to still name a substantial tracked "
           f"complete-session corpus, found {len(requested)}")
    dumped, tail = codec.dump_fixture_descriptors(requested)
    if dumped is None:
        expect(False, f"expected the descriptor dump to succeed, got: {tail}")
        return
    missing = [p.name for p in requested if str(p) not in dumped]
    expect(not missing,
           f"expected every requested fixture to appear in the dump, missing "
           f"{missing}")
    for path in requested:
        descriptors = dumped.get(str(path), [])
        ids = [d["id"] for d in descriptors]
        expect(ids == sorted(ids),
               f"expected {path.name}'s descriptors in ascending component-id "
               f"order (encodeEnvelope's canonical layout), got {ids}")
        expect(all(isinstance(d["version"], int)
                   and isinstance(d["required"], bool) for d in descriptors),
               f"expected {path.name}'s descriptors to carry an integer "
               f"version and a boolean required flag, got {descriptors}")


def test_real_descriptors_back_the_manifests_declared_versions() -> None:
    print("issue #2273: the manifest-backed expectation itself -- every "
          "components[] version the real manifest declares is backed by a "
          "real tracked fixture's decoded descriptors, through the compiled "
          "helper rather than a cabal repl")
    manifest = manifest_audit.load_manifest()
    dumped, violations = manifest_audit.verify_fixture_descriptors(manifest)
    expect(dumped is not None,
           f"expected the real corpus to decode, got: {violations}")
    expect(violations == [],
           f"expected no manifest-versus-real-descriptor violations, got "
           f"{violations}")


def test_summary_failure_names_the_fixture_and_the_codec_error() -> None:
    print("issue #2273 requirement 5: a fixture that does not decode is "
          "diagnosable from the CI log alone -- the surfaced diagnostic "
          "carries the fixture path AND the production codec's own error")
    with tempfile.TemporaryDirectory(dir=common.REPO_ROOT) as d:
        broken = Path(d) / "broken-fixture.bin"
        broken.write_bytes(b"this is not an envelope at all")
        out = Path(d) / "summary.json"
        ok, tail = codec.dump_canonical_summary(broken, out)
        expect(not ok, "expected an undecodable fixture to fail the dump")
        expect(str(broken) in tail,
               f"expected the diagnostic to name the fixture path, got {tail!r}")
        expect("BadMagic" in tail,
               f"expected the diagnostic to carry the production codec's own "
               f"error text, got {tail!r}")
        expect(not out.exists(),
               "expected no canonical summary to be written for a fixture "
               "that did not decode")


def test_descriptor_dump_failure_names_the_fixture_and_the_codec_error() -> None:
    print("issue #2273 requirement 5, batch side: one undecodable fixture "
          "fails the whole descriptor dump rather than being silently "
          "omitted, and the diagnostic names it and the codec's error")
    with tempfile.TemporaryDirectory(dir=common.REPO_ROOT) as d:
        good = common.FIXTURE_DATA_DIR / "u1-generated-world-identity.bin"
        broken = Path(d) / "broken-fixture.bin"
        broken.write_bytes(b"this is not an envelope at all")
        dumped, tail = codec.dump_fixture_descriptors([good, broken])
        expect(dumped is None,
               "expected a batch containing an undecodable fixture to fail "
               "outright, not to answer a partial dump the caller would read "
               "as complete")
        expect(str(broken) in tail,
               f"expected the diagnostic to name the fixture path, got {tail!r}")
        expect("BadMagic" in tail,
               f"expected the diagnostic to carry the production codec's own "
               f"error text, got {tail!r}")


def test_summary_success_without_written_output_is_reported_as_failure() -> None:
    print("issue #2273 requirement 4: a helper that exits 0 printing DUMP_OK "
          "but writes no summary is its own diagnosis, not a silent success "
          "the caller then reads a missing file for")
    with tempfile.TemporaryDirectory(dir=common.REPO_ROOT) as d:
        tmp = Path(d)
        out = tmp / "summary.json"
        with _with_helper(_fake_helper(tmp, "DUMP_OK")):
            ok, tail = codec.dump_canonical_summary(
                common.FIXTURE_DATA_DIR / "u1-generated-world-identity.bin", out)
        expect(not ok,
               "expected a successful exit with no output file to be reported "
               "as a failure")
        expect("wrote no canonical summary" in tail,
               f"expected the missing-output diagnosis to say so in its own "
               f"words rather than borrowing a decode failure's, got {tail!r}")


def test_descriptor_success_without_written_output_is_reported_as_failure() -> None:
    print("issue #2273 requirement 4, batch side: the same distinction for "
          "the descriptor dump, whose caller reads the file back as JSON")
    with tempfile.TemporaryDirectory(dir=common.REPO_ROOT) as d:
        tmp = Path(d)
        with _with_helper(_fake_helper(tmp, "DESCRIPTOR_DUMP_OK")):
            dumped, tail = codec.dump_fixture_descriptors(
                [common.FIXTURE_DATA_DIR / "u1-generated-world-identity.bin"])
        expect(dumped is None,
               "expected a successful exit with no output file to be reported "
               "as a failure")
        expect("wrote no readable descriptor output" in tail,
               f"expected the missing-output diagnosis to say so in its own "
               f"words, got {tail!r}")


def test_pre_resolved_handoff_is_used_and_a_bad_one_is_refused() -> None:
    print("issue #2273 requirement 6: an exported absolute helper path is "
          "what a probe runner hands this bridge instead of a Cabal call, so "
          "it is honoured verbatim -- and an exported path that is not a file "
          "is an error rather than a silent fall-through to `cabal list-bin`, "
          "which would resolve a DIFFERENT build than the caller named")
    with tempfile.TemporaryDirectory(dir=common.REPO_ROOT) as d:
        tmp = Path(d)
        helper = _fake_helper(tmp, "DUMP_OK")
        with _with_helper(helper):
            resolved, why = codec.resolve_codec_exe()
        expect(resolved == str(helper),
               f"expected the exported path to be used verbatim, got "
               f"{resolved!r} ({why})")

        missing = tmp / "no-such-helper"
        with _with_helper(missing):
            resolved, why = codec.resolve_codec_exe()
        expect(resolved is None,
               "expected an exported path that is not a file to be refused")
        expect(str(missing) in why and codec.ENV_CODEC_EXE in why,
               f"expected the refusal to name both the variable and the path "
               f"it pointed at, got {why!r}")


#: This owner's members, in the run order the façade concatenates
#: (issue #2073 requirement 12).
TESTS = [
    test_helper_summary_matches_every_generated_expectation,
    test_normalizing_a_tracked_fixture_reproduces_its_tracked_bytes,
    test_descriptor_dump_covers_every_requested_fixture_in_codec_order,
    test_real_descriptors_back_the_manifests_declared_versions,
    test_summary_failure_names_the_fixture_and_the_codec_error,
    test_descriptor_dump_failure_names_the_fixture_and_the_codec_error,
    test_summary_success_without_written_output_is_reported_as_failure,
    test_descriptor_success_without_written_output_is_reported_as_failure,
    test_pre_resolved_handoff_is_used_and_a_bad_one_is_refused,
]
