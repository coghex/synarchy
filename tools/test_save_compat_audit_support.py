#!/usr/bin/env python3
"""Shared fixtures and guards for the save-compat self-test (issue #2073).

The self-test's one owner with no test cases of its own. Issue #2073
requirement 4 names five categories of shared support, and every member
below is here because it falls into one of them:

  assertion state and failure reporting
      `expect_real_manifest_untouched` (with `REAL_MANIFEST_PATH` and
      `real_manifest_bytes`, which it reads). Assertion COUNTING and the
      failure list itself are `tools/selftestlib.py`'s since issue
      #1922; what stays here is the one module-local assertion, whose
      subject -- "a MANIFEST_PATH patch that did not take" -- is a
      property of this self-test rather than of self-tests generally.
  temporary manifest and fixture construction
      `make_fixture`, `base_manifest`, `_oldest_version_components`.
  shared subprocess/output doubles
      None. Every case that fakes a seam builds its own inline double
      and asserts the double was REACHED, so there is nothing common to
      hoist; this line records that the category was considered rather
      than that it was overlooked.
  common registration and generation fixtures
      `_Args`, the argparse stand-in both `--add-baseline` and
      `--generate-session` cases construct.
  helpers used by more than one test owner
      `make_fixture` and `base_manifest`, which four owners call, and
      `_oldest_version_components`, which `base_manifest` calls.

Requirement 16 keeps the dependency one-way: this module contains no
test case, imports no owner module and never imports the façade, so an
owner may import it freely and the façade sees it only transitively.
"""
from __future__ import annotations

import hashlib
import sys
from pathlib import Path

# Requirement 16 / the review's import-bootstrap addition: every module
# in this family runs the bootstrap itself, so a single owner can be
# imported on its own -- for the structural verification in issue
# #2073's acceptance, say -- without the façade having run first.
sys.path.insert(0, str(Path(__file__).resolve().parent))
import save_compat_audit_common as common  # noqa: E402
import save_compat_audit_components as components  # noqa: E402
import save_compat_audit_fingerprint as fingerprint  # noqa: E402

from selftestlib import expect  # noqa: E402


#: The real tracked manifest, resolved straight from the repository root
#: rather than through `common.MANIFEST_PATH`. The guard below exists to
#: catch a case whose patch of that attribute did NOT take, so it must
#: not consult the very attribute under suspicion (issue #2049).
REAL_MANIFEST_PATH = common.REPO_ROOT / "docs" / "save_compat" / "manifest.json"


def real_manifest_bytes() -> bytes:
    """The tracked manifest's exact bytes, or b"" when it is absent."""
    return (REAL_MANIFEST_PATH.read_bytes()
            if REAL_MANIFEST_PATH.exists() else b"")


def expect_real_manifest_untouched(before: bytes, where: str) -> None:
    """No registration or generation case may write the REAL manifest.

    Issue #2049: a `common.MANIFEST_PATH` patch that silently failed to
    take would leave the case passing on its own assertions while having
    rewritten tracked data. This is what catches that.
    """
    expect(real_manifest_bytes() == before,
           f"{where}: the real docs/save_compat/manifest.json is "
           f"byte-unchanged (a MANIFEST_PATH patch that did not take "
           f"would have rewritten tracked data here)")


def make_fixture(tmp: Path, name: str, content: bytes) -> Path:
    p = tmp / name
    p.write_bytes(content)
    return p


def _oldest_version_components() -> list[dict]:
    """A components[] list covering every REAL required component's
    oldest AND current accepted version (round-3 review: required
    components need coverage regardless of how many versions they
    accept, not just multi-version ones; round-10 review: a component's
    CURRENT version needs its own tracked coverage too, distinct from
    its oldest), satisfying audit_component_versions' full coverage
    check with entries that are trivially true of THIS repo's actual
    registry. audit_component_versions cross-checks against the real
    source unconditionally (there is no "test mode" -- that's the whole
    point), so a synthetic manifest aimed at ONE specific, unrelated
    violation class must still declare this or it would incidentally
    also fail on every real component's coverage check, which has
    nothing to do with what that test is exercising."""
    registry = components.real_component_registry()
    entries = []
    for cid, info in registry.items():
        if not info.get("required"):
            continue
        oldest = min(info["inputVersions"])
        current = info["currentVersion"]
        entries.append({"id": cid, "version": oldest, "required": True})
        if current != oldest:
            entries.append({"id": cid, "version": current, "required": True})
    return entries


def base_manifest(tmp: Path, fixture_path: Path, content: bytes) -> dict:
    return {
        "envelopeFramingVersion": fingerprint.current_envelope_version(),
        "frozenDtoFingerprint": fingerprint.frozen_dto_fingerprint(),
        "envelopeFramingFingerprint": fingerprint.envelope_framing_fingerprint(),
        "baselines": [
            {
                "id": "test-baseline",
                "migrationTarget": "current",
                "components": _oldest_version_components(),
                "fixtures": [
                    {
                        "id": "test-fixture",
                        "path": str(fixture_path.relative_to(common.REPO_ROOT))
                            if fixture_path.is_relative_to(common.REPO_ROOT)
                            else str(fixture_path),
                        "sha256": hashlib.sha256(content).hexdigest(),
                        "sizeBytes": len(content),
                    }
                ],
            }
        ],
    }


class _Args:
    """A minimal stand-in for argparse.Namespace -- only the attributes
    cmd_add_baseline/_build_fixture_entry actually read."""
    def __init__(self, **kwargs) -> None:
        defaults = dict(
            baseline_id=None, fixture_id=None, path=None, kind=None,
            summary=None, provenance=None, description=None,
            migration_target=None, migrated_by=None, components=None,
            declared_at=None, declared_by_issue=766, force=False,
            # Every EXISTING test below registers a fixture that was
            # never actually run through the real codec (they're plain
            # placeholder bytes) -- skip_validation defaults to True here
            # so they keep exercising the atomic bookkeeping in
            # isolation, without also needing a real cabal toolchain in
            # every environment this suite runs in. The validation path
            # itself is exercised separately below via a monkeypatched
            # _run_real_codec_validation, never a real subprocess.
            skip_validation=True,
            # --generate-session only -- cmd_generate reads these too;
            # every generate-session test below monkeypatches
            # generate_current_format_session itself, so the actual
            # values never reach a real engine boot.
            port=9999, page_id="test_page", seed=1, world_size=8,
            plate_count=3, spawn_building=None, spawn_unit=None,
            # #915: where to spawn --spawn-unit, how long to let ticks
            # run before saving, and a predicate that must hold before
            # the save -- for state a spawn verb never writes directly
            # (a per-unit location memory is INGESTED by the unit-AI
            # tick once the unit can SEE the location).
            spawn_unit_at="0,0", settle_seconds=0.0, setup_lua=None,
            require_lua=None,
            # #1101: the page's optional display identity and the
            # language provenance it was rendered from -- what makes a
            # generated fixture's placed locations carry real generated
            # names/glosses rather than definition labels.
            world_name=None, world_gloss=None, language_seed=None,
            language_version=None, name_expr=None)
        defaults.update(kwargs)
        self.__dict__.update(defaults)
