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


#: Every tracked complete-session fixture's EXACT decoded envelope
#: manifest, in the codec's own canonical order, as
#: ``"<id>:<version>:<R|o>"`` tokens (``R`` = required).
#:
#: This is per FIXTURE, and that is the whole point of it. The manifest
#: declares components[] per BASELINE, and
#: `save_compat_audit_manifest.verify_fixture_descriptors` correspondingly
#: asks only that each declaration occur in SOME fixture of that
#: baseline -- so a wrong version or required flag in one of
#: `c3-raw-reference-v1`'s three fixtures is masked by a sibling that
#: happens to carry the declared value. Nothing in the tracked manifest
#: can express what ONE fixture's bytes decode to, which is why the
#: expectation is frozen here.
#:
#: It is an oracle for the HELPER, not for the corpus: the fixtures'
#: own bytes are already pinned by their manifest sha256, so a row that
#: stops matching means the decode changed, not the file. Adding a
#: tracked complete-session fixture means adding its row; the member
#: below fails loudly on a fixture with no row rather than skipping it.
_EXPECTED_DESCRIPTORS: dict[str, str] = {
    "b1-initial-session.bin":
        'metadata:1:R session:90:R',
    "b2-split-haskell-lua-state.bin":
        'buildings:1:R core-session:1:R craft-bills:1:R lua-state:1:R '
        'metadata:1:R power-nodes:1:R texture-palette:1:R unit-sim:1:R '
        'units:1:R world-activity:1:R world-edits:1:R world-pages:1:R',
    "b3-lua-versioned-session-v1.bin":
        'buildings:1:R core-session:1:R craft-bills:2:R '
        'lua.building_spawn:1:R lua.unit_ai:1:R metadata:1:R '
        'power-nodes:2:R texture-palette:1:R unit-sim:2:R units:1:R '
        'world-activity:1:R world-edits:1:R world-pages:1:R',
    "c3-typed-reference-v1-multipage.bin":
        'buildings:1:R core-session:1:R craft-bills:2:R '
        'lua.building_spawn:3:R lua.unit_ai:3:R metadata:1:R '
        'power-nodes:2:R texture-palette:1:R unit-sim:2:R units:1:R '
        'world-activity:1:R world-edits:1:R world-pages:1:R',
    "c3-typed-reference-v1-with-items.bin":
        'buildings:1:R core-session:1:R craft-bills:2:R '
        'lua.building_spawn:3:R lua.unit_ai:3:R metadata:1:R '
        'power-nodes:2:R texture-palette:1:R unit-sim:2:R units:1:R '
        'world-activity:1:R world-edits:1:R world-pages:1:R',
    "c3-typed-reference-v1.bin":
        'buildings:1:R core-session:1:R craft-bills:1:R '
        'lua.building_spawn:3:R lua.unit_ai:3:R metadata:1:R '
        'power-nodes:1:R texture-palette:1:R unit-sim:1:R units:1:R '
        'world-activity:1:R world-edits:1:R world-pages:1:R',
    "d1-location-instance-identity.bin":
        'buildings:1:R core-session:1:R craft-bills:2:R '
        'lua.building_spawn:3:R lua.unit_ai:3:R metadata:1:R '
        'power-nodes:2:R texture-palette:1:R unit-sim:2:R units:1:R '
        'world-activity:1:R world-edits:1:R world-pages:2:R',
    "e1-unit-location-knowledge.bin":
        'buildings:1:R core-session:1:R craft-bills:2:R '
        'lua.building_spawn:3:R lua.unit_ai:4:R metadata:1:R '
        'power-nodes:2:R texture-palette:1:R unit-sim:2:R units:1:R '
        'world-activity:1:R world-edits:1:R world-pages:2:R',
    "f1-autosave-classification.bin":
        'buildings:1:R core-session:1:R craft-bills:2:R '
        'lua.building_spawn:3:R lua.tutorial_progress:1:o lua.unit_ai:4:R '
        'metadata:2:R power-nodes:2:R texture-palette:1:R unit-sim:2:R '
        'units:1:R world-activity:1:R world-edits:1:R world-pages:2:R',
    "g1-language-provenance.bin":
        'buildings:1:R core-session:1:R craft-bills:2:R '
        'lua.building_spawn:3:R lua.tutorial_progress:1:o lua.unit_ai:4:R '
        'metadata:2:R power-nodes:2:R texture-palette:1:R unit-sim:2:R '
        'units:1:R world-activity:1:R world-edits:1:R world-pages:3:R',
    "h1-container-knowledge.bin":
        'buildings:1:R container-knowledge:1:o core-session:1:R '
        'craft-bills:2:R lua.building_spawn:3:R lua.tutorial_progress:1:o '
        'lua.unit_ai:4:R metadata:2:R power-nodes:2:R texture-palette:1:R '
        'unit-sim:2:R units:1:R world-activity:1:R world-edits:1:R '
        'world-pages:3:R',
    "i1-location-language-names.bin":
        'buildings:1:R container-knowledge:1:o core-session:1:R '
        'craft-bills:2:R lua.building_spawn:3:R lua.tutorial_progress:1:o '
        'lua.unit_ai:4:R metadata:2:R power-nodes:2:R texture-palette:1:R '
        'unit-sim:2:R units:1:R world-activity:1:R world-edits:1:R '
        'world-pages:4:R',
    "j1-river-language-names.bin":
        'buildings:1:R container-knowledge:1:o core-session:1:R '
        'craft-bills:2:R lua.building_spawn:3:R lua.tutorial_progress:1:o '
        'lua.unit_ai:4:R metadata:2:R power-nodes:2:R texture-palette:1:R '
        'unit-sim:2:R units:1:R world-activity:1:R world-edits:1:R '
        'world-pages:5:R',
    "k1-canonical-designation-frame.bin":
        'buildings:1:R container-knowledge:1:o core-session:1:R '
        'craft-bills:2:R lua.building_spawn:3:R lua.tutorial_progress:1:o '
        'lua.unit_ai:4:R metadata:2:R power-nodes:2:R texture-palette:1:R '
        'unit-sim:2:R units:1:R world-activity:2:R world-edits:1:R '
        'world-pages:5:R',
    "k1-name-etymology.bin":
        'buildings:1:R container-knowledge:1:o core-session:1:R '
        'craft-bills:2:R lua.building_spawn:3:R lua.tutorial_progress:1:o '
        'lua.unit_ai:4:R metadata:2:R power-nodes:2:R texture-palette:1:R '
        'unit-sim:2:R units:1:R world-activity:1:R world-edits:1:R '
        'world-pages:6:R',
    "l1-order-stall-budget.bin":
        'buildings:1:R container-knowledge:1:o core-session:1:R '
        'craft-bills:2:R lua.building_spawn:3:R lua.tutorial_progress:1:o '
        'lua.unit_ai:5:R metadata:2:R power-nodes:2:R texture-palette:1:R '
        'unit-sim:2:R units:1:R world-activity:2:R world-edits:1:R '
        'world-pages:6:R',
    "m1-item-bulk-storage.bin":
        'buildings:2:R container-knowledge:2:o core-session:1:R '
        'craft-bills:2:R lua.building_spawn:3:R lua.tutorial_progress:1:o '
        'lua.unit_ai:5:R metadata:2:R power-nodes:2:R texture-palette:1:R '
        'transfer-orders:1:o unit-sim:2:R units:2:R world-activity:3:R '
        'world-edits:1:R world-pages:6:R',
    "n1-location-sight-reveal.bin":
        'buildings:1:R container-knowledge:1:o core-session:1:R '
        'craft-bills:2:R lua.building_spawn:3:R lua.tutorial_progress:1:o '
        'lua.unit_ai:5:R metadata:2:R power-nodes:2:R texture-palette:1:R '
        'transfer-orders:1:o unit-sim:2:R units:1:R world-activity:2:R '
        'world-edits:1:R world-pages:7:R',
    "o1-wander-hazard-policy.bin":
        'buildings:2:R container-knowledge:2:o core-session:1:R '
        'craft-bills:2:R lua.building_spawn:3:R lua.tutorial_progress:1:o '
        'lua.unit_ai:5:R metadata:2:R power-nodes:2:R texture-palette:1:R '
        'transfer-orders:1:o unit-sim:3:R units:2:R world-activity:3:R '
        'world-edits:1:R world-pages:7:R',
    "p1-position-hold.bin":
        'buildings:2:R container-knowledge:2:o core-session:1:R '
        'craft-bills:2:R lua.building_spawn:3:R lua.tutorial_progress:1:o '
        'lua.unit_ai:6:R metadata:2:R power-nodes:2:R texture-palette:1:R '
        'transfer-orders:1:o unit-sim:3:R units:2:R world-activity:3:R '
        'world-edits:1:R world-pages:7:R',
    "q1-repair-ground-target.bin":
        'buildings:2:R container-knowledge:2:o core-session:1:R '
        'craft-bills:2:R lua.building_spawn:3:R lua.tutorial_progress:1:o '
        'lua.unit_ai:7:R metadata:2:R power-nodes:2:R texture-palette:1:R '
        'transfer-orders:1:o unit-sim:3:R units:2:R world-activity:3:R '
        'world-edits:1:R world-pages:7:R',
    "r1-ruin-nomad-encounter.bin":
        'buildings:2:R container-knowledge:2:o core-session:1:R '
        'craft-bills:2:R lua.building_spawn:3:R lua.tutorial_progress:1:o '
        'lua.unit_ai:7:R metadata:2:R power-nodes:2:R texture-palette:1:R '
        'transfer-orders:1:o unit-sim:3:R units:2:R world-activity:3:R '
        'world-edits:1:R world-pages:8:R',
    "s1-flora-instance-identity.bin":
        'buildings:2:R container-knowledge:2:o core-session:1:R '
        'craft-bills:2:R lua.building_spawn:3:R lua.tutorial_progress:1:o '
        'lua.unit_ai:7:R metadata:2:R power-nodes:2:R texture-palette:1:R '
        'transfer-orders:1:o unit-sim:3:R units:2:R world-activity:4:R '
        'world-edits:2:R world-pages:8:R',
    "t1-construct-attempt-receipt.bin":
        'buildings:2:R container-knowledge:2:o core-session:1:R '
        'craft-bills:2:R lua.building_spawn:3:R lua.tutorial_progress:1:o '
        'lua.unit_ai:8:R metadata:2:R power-nodes:2:R texture-palette:1:R '
        'transfer-orders:1:o unit-sim:3:R units:2:R world-activity:5:R '
        'world-edits:2:R world-pages:8:R',
    "u1-generated-world-identity.bin":
        'buildings:2:R container-knowledge:2:o core-session:1:R '
        'craft-bills:2:R lua.building_spawn:3:R lua.tutorial_progress:1:o '
        'lua.unit_ai:8:R metadata:3:R power-nodes:2:R texture-palette:1:R '
        'transfer-orders:1:o unit-sim:3:R units:2:R world-activity:5:R '
        'world-edits:2:R world-pages:9:R',
    "v1-location-significant-contents.bin":
        'buildings:2:R container-knowledge:2:o core-session:1:R '
        'craft-bills:2:R lua.building_spawn:3:R lua.tutorial_progress:1:o '
        'lua.unit_ai:8:R metadata:3:R power-nodes:2:R texture-palette:1:R '
        'transfer-orders:1:o unit-sim:3:R units:2:R world-activity:5:R '
        'world-edits:2:R world-pages:10:R',
    "w1-construct-stake-reference.bin":
        'buildings:2:R container-knowledge:2:o core-session:1:R '
        'craft-bills:2:R lua.building_spawn:3:R lua.tutorial_progress:1:o '
        'lua.unit_ai:9:R metadata:3:R power-nodes:2:R texture-palette:1:R '
        'transfer-orders:1:o unit-sim:3:R units:2:R world-activity:5:R '
        'world-edits:2:R world-pages:10:R',
    "x1-flora-species-names.bin":
        'buildings:2:R container-knowledge:2:o core-session:1:R '
        'craft-bills:2:R lua.building_spawn:3:R lua.tutorial_progress:1:o '
        'lua.unit_ai:9:R metadata:3:R power-nodes:2:R texture-palette:1:R '
        'transfer-orders:1:o unit-sim:3:R units:2:R world-activity:6:R '
        'world-edits:3:R world-pages:10:R',
}


def _stub_cabal(tmp: Path, *, binary: Path, build_creates: bool) -> Path:
    """A `cabal` on PATH that records its argv and answers `list-bin`.

    `build_creates` decides whether `cabal build` actually produces
    `binary`, which is what separates the two directions of the
    resolution's step 3: a helper that was merely absent, and one the
    build did not manage to produce.
    """
    bin_dir = tmp / "stub-bin"
    bin_dir.mkdir(exist_ok=True)
    log = tmp / "cabal.log"
    script = bin_dir / "cabal"
    # Built outside the f-strings below on purpose: a backslash inside an
    # f-string EXPRESSION is a SyntaxError before Python 3.12 (PEP 701
    # lifted it), and CI runs 3.10, so an interpolated `\"` here would
    # fail to parse there while compiling fine on a newer local one.
    build_action = 'touch "%s"' % binary if build_creates else ":"
    script.write_text(
        "#!/bin/sh\n"
        f'echo "$@" >> "{log}"\n'
        'case "$1" in\n'
        f'  list-bin) echo "{binary}" ;;\n'
        f'  build) {build_action} ;;\n'
        'esac\n'
        "exit 0\n", encoding="utf-8")
    script.chmod(script.stat().st_mode | stat.S_IXUSR)
    return bin_dir


def _with_path(bin_dir: Path):
    """Prepend `bin_dir` to PATH, and clear the resolution cache, for one
    block. The cache is cleared on the way OUT as well, so a stubbed
    answer can never leak into a later member's real resolution."""
    class _Scope:
        def __enter__(self) -> None:
            self.previous_path = os.environ.get("PATH", "")
            self.previous_env = os.environ.pop(codec.ENV_CODEC_EXE, None)
            self.previous_cache = codec._CACHED_CODEC_EXE
            os.environ["PATH"] = f"{bin_dir}{os.pathsep}{self.previous_path}"
            codec._CACHED_CODEC_EXE = None

        def __exit__(self, *_exc) -> None:
            os.environ["PATH"] = self.previous_path
            if self.previous_env is not None:
                os.environ[codec.ENV_CODEC_EXE] = self.previous_env
            codec._CACHED_CODEC_EXE = self.previous_cache
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


def _requested_complete_session_fixtures() -> list[Path]:
    """Exactly the corpus `verify_fixture_descriptors` feeds the helper."""
    manifest = manifest_audit.load_manifest()
    requested = []
    for baseline in manifest.get("baselines", []):
        for fixture in baseline.get("fixtures", []):
            if fixture.get("kind") != "complete-session" or not fixture.get("sha256"):
                continue
            path = common.REPO_ROOT / fixture["path"]
            if path.exists():
                requested.append(path)
    return requested


def _encode_descriptors(descriptors: list[dict]) -> str:
    """One fixture's decoded manifest in `_EXPECTED_DESCRIPTORS`' spelling."""
    return " ".join(
        f"{d['id']}:{d['version']}:{'R' if d['required'] else 'o'}"
        for d in descriptors)


def test_descriptor_dump_matches_every_fixtures_exact_ordered_manifest() -> None:
    print("issue #2273: the compiled helper's descriptor dump is compared "
          "EXACTLY at its observable boundary -- per requested path, the "
          "full ordered (id, version, required) list -- because the "
          "manifest's own per-baseline check lets a sibling fixture mask a "
          "wrong version or required flag in one of them")
    requested = _requested_complete_session_fixtures()
    expect(len(requested) >= 20,
           f"expected the manifest to still name a substantial tracked "
           f"complete-session corpus, found {len(requested)}")
    dumped, tail = codec.dump_fixture_descriptors(requested)
    if dumped is None:
        expect(False, f"expected the descriptor dump to succeed, got: {tail}")
        return
    expect(sorted(dumped) == sorted(str(p) for p in requested),
           f"expected the dump to be keyed by exactly the paths asked for, "
           f"got {sorted(Path(k).name for k in dumped)}")
    unlisted = [p.name for p in requested if p.name not in _EXPECTED_DESCRIPTORS]
    expect(not unlisted,
           f"expected every tracked complete-session fixture to carry a row "
           f"in _EXPECTED_DESCRIPTORS; add one for {unlisted} rather than "
           f"leaving it unchecked")
    for path in requested:
        if path.name not in _EXPECTED_DESCRIPTORS:
            continue
        actual = _encode_descriptors(dumped.get(str(path), []))
        expect(actual == _EXPECTED_DESCRIPTORS[path.name],
               f"expected {path.name}'s decoded manifest to be exactly\n"
               f"  {_EXPECTED_DESCRIPTORS[path.name]}\n"
               f"got\n  {actual}")
    stale = sorted(set(_EXPECTED_DESCRIPTORS) - {p.name for p in requested})
    expect(not stale,
           f"expected no _EXPECTED_DESCRIPTORS row for a fixture the "
           f"manifest no longer tracks, found {stale}")


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
        expect(not out.exists(),
               "expected nothing to be left at the caller's output path")


def test_a_pre_existing_summary_is_never_mistaken_for_this_runs_output() -> None:
    print("issue #2273: success is tied to bytes THIS invocation produced. "
          "`--generate-session --force` regenerates over a registered "
          "fixture's own *.expected.json, so the output path routinely "
          "already holds a valid summary; a helper that exited 0 having "
          "written nothing must not have that older content read back as "
          "this run's answer, and must not damage it either")
    with tempfile.TemporaryDirectory(dir=common.REPO_ROOT) as d:
        tmp = Path(d)
        out = tmp / "summary.json"
        stale = '{"metadata": {"seed": 1}, "pages": []}'
        out.write_text(stale, encoding="utf-8")
        with _with_helper(_fake_helper(tmp, "DUMP_OK")):
            ok, tail = codec.dump_canonical_summary(
                common.FIXTURE_DATA_DIR / "u1-generated-world-identity.bin", out)
        expect(not ok,
               "expected a helper that wrote nothing to fail even though the "
               "output path already held a non-empty, perfectly valid summary")
        expect("wrote no canonical summary" in tail,
               f"expected the missing-output diagnosis, got {tail!r}")
        expect(out.read_text(encoding="utf-8") == stale,
               "expected the pre-existing summary to be byte-untouched by a "
               "failed run, which is what the generation transaction's "
               "rollback relies on")

        # ... and the successful direction really does replace it, so the
        # staging above is not quietly swallowing the write.
        ok, tail = codec.dump_canonical_summary(
            common.FIXTURE_DATA_DIR / "u1-generated-world-identity.bin", out)
        expect(ok, f"expected the real helper to succeed, got: {tail}")
        expect(out.read_bytes() == (
                   common.FIXTURE_DATA_DIR
                   / "u1-generated-world-identity.expected.json").read_bytes(),
               "expected a successful run to replace the pre-existing content "
               "with this fixture's real canonical summary")


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


def test_resolution_builds_before_locating_even_when_the_helper_exists() -> None:
    print("issue #2273: an EXISTING helper is not evidence of a CURRENT one "
          "-- `cabal list-bin` answers a path either way -- so resolution "
          "builds first and locates second, every time. The `cabal repl` "
          "this replaced compiled from current sources on every call; "
          "accepting a stale binary would decode against yesterday's codec "
          "while reporting today's verdict, and the probe runner's own "
          "preflight (#1570) builds exe:synarchy, never this target")
    with tempfile.TemporaryDirectory(dir=common.REPO_ROOT) as d:
        tmp = Path(d)
        binary = tmp / "already-present-helper"
        binary.write_text("stale", encoding="utf-8")
        log = tmp / "cabal.log"

        with _with_path(_stub_cabal(tmp, binary=binary, build_creates=True)):
            resolved, why = codec.resolve_codec_exe()
        expect(resolved == str(binary),
               f"expected the helper to resolve, got {resolved!r} ({why})")
        calls = [line.split()[0] for line in
                 log.read_text(encoding="utf-8").splitlines() if line.split()]
        expect(calls == ["build", "list-bin"],
               f"expected one build THEN one locate, even for a helper that "
               f"was already on disk, got {calls}")

        # Once per process, not once per operation: the whole point of
        # caching the answer is that a batch of decodes pays this once.
        log.unlink()
        with _with_path(_stub_cabal(tmp, binary=binary, build_creates=True)):
            codec.resolve_codec_exe()
            codec.resolve_codec_exe()
            codec.resolve_codec_exe()
        calls = [line.split()[0] for line in
                 log.read_text(encoding="utf-8").splitlines() if line.split()]
        expect(calls == ["build", "list-bin"],
               f"expected the resolved answer to be cached for the process, "
               f"got {calls}")


def test_a_build_that_produces_no_helper_is_refused_not_retried() -> None:
    print("issue #2273: when the build reports success and the helper still "
          "is not there, resolution says exactly that rather than looping or "
          "handing a caller a path it is about to fail to exec")
    with tempfile.TemporaryDirectory(dir=common.REPO_ROOT) as d:
        tmp = Path(d)
        binary = tmp / "never-built-helper"
        log = tmp / "cabal.log"
        with _with_path(_stub_cabal(tmp, binary=binary, build_creates=False)):
            resolved, why = codec.resolve_codec_exe()
        expect(resolved is None,
               f"expected resolution to fail, got {resolved!r}")
        expect("does not exist" in why and str(binary) in why,
               f"expected the refusal to name the path the build did not "
               f"produce, got {why!r}")
        expect(log.read_text(encoding="utf-8").split().count("build") == 1,
               f"expected exactly ONE build attempt, not a retry loop, got "
               f"{log.read_text(encoding='utf-8')!r}")


#: This owner's members, in the run order the façade concatenates
#: (issue #2073 requirement 12).
TESTS = [
    test_helper_summary_matches_every_generated_expectation,
    test_normalizing_a_tracked_fixture_reproduces_its_tracked_bytes,
    test_descriptor_dump_matches_every_fixtures_exact_ordered_manifest,
    test_real_descriptors_back_the_manifests_declared_versions,
    test_summary_failure_names_the_fixture_and_the_codec_error,
    test_descriptor_dump_failure_names_the_fixture_and_the_codec_error,
    test_summary_success_without_written_output_is_reported_as_failure,
    test_descriptor_success_without_written_output_is_reported_as_failure,
    test_a_pre_existing_summary_is_never_mistaken_for_this_runs_output,
    test_pre_resolved_handoff_is_used_and_a_bad_one_is_refused,
    test_resolution_builds_before_locating_even_when_the_helper_exists,
    test_a_build_that_produces_no_helper_is_refused_not_retried,
]
