#!/usr/bin/env python3
"""The canonical persistence-state inspection/comparison surface (issue
#767, save-overhaul D1, requirements 1/2/5).

Rather than inventing a bespoke JSON schema for "every persistent field"
(a comprehensive re-serialization of `World.Save.Snapshot.SessionSnapshot`
and every type it embeds -- `WorldGenParams`, `GeoTimeline`, ...  -- would
be a large, narrow-purpose duplicate of the real wire codec), this reuses
the two facts that already make the REAL types comparable for free:

  - `World.Save.Snapshot.SessionSnapshot`/`PageSnapshot` derive `Eq`, and
    hold ONLY persistent gameplay state (contract requirement 1: no
    storage metadata, no runtime RNG/thread-schedule/GPU-handle state --
    see that module's own haddock for what is deliberately absent and
    why). Structural equality of two independently-produced,
    `decodeSessionEnvelope`-assembled snapshots IS the canonical,
    order-independent (every collection is `HashMap`-keyed) comparison
    every Haskell-owned save component needs.
  - `scripts/lib/data_codec.lua`'s canonical (sorted-key) encoding means
    two independently-produced encodings of the SAME logical Lua state
    are byte-identical -- so the raw `lua.unit_ai`/`lua.building_spawn`
    envelope payload bytes are exactly as strong a structural comparison,
    with no decode step (and no live Lua VM) required at all.

`compare_session_files` decodes N save files (independently produced --
e.g. an original save and a resave taken after a fresh-process load
published it) through the real `World.Save.Envelope.decodeSessionEnvelope`
and asserts every one is pairwise equal on both halves. This needs no
engine, no GPU, and no window.

Issue #2274: the last GHCi consumer in this family
--------------------------------------------------
That comparison was a `cabal repl test:synarchy-test-headless` program
of this module's own until #2274 -- the one remaining GHCi path after
#2273 converted `save_compat_audit.py`'s three codec operations to the
compiled `exe:synarchy-save-codec`. It cost far more than the decode it
performed:

  - a `cabal repl` recompiles into the shared inplace package database,
    so `persistence_contract` and `persistence_contract_sweep` had to
    hold `cabal-build` EXCLUSIVELY and the compact probe ran ALONE,
    after every other probe in a `--jobs 2` sweep had finished;
  - and it was the only reason the `behavior-probes` CI job built
    `synarchy-test-headless` at all, which on the 2026-09-02 runs
    recompiled 211 of 348 modules for a handful of library functions.

Both halves of the comparison are now `app-save-codec/Main.hs`'s
`compare` operation, reached through
`save_compat_audit.compare_session_snapshots`. It is the SAME program
against the SAME library function, compiled -- not a second decoder --
so the three outcomes below are unchanged.

On a mismatch this additionally calls
`save_compat_audit.dump_canonical_summary` (already covers
metadata/allocators/camera/every page's entities) on two files to give a
human-readable diagnostic of WHERE they diverge. The strict Eq/byte
check is the pass/fail gate; the summary dump is only for debugging a
failure. WHICH two files is taken from the comparison's own report
rather than assumed to be the first two: a run comparing four
generations can first diverge at `gen3`, and diffing `gen1` against
`gen2` would then report no difference at all beside a genuine mismatch.

Where the decoder comes from
----------------------------
`prepare_decoder` below is this surface's half of the two-binary
contract `tools/probe_engine.py` describes. Under `tools/run_probes.py`
the runner's preflight has already resolved the helper and exported
`SYNARCHY_SAVE_CODEC_EXE`, so it is a no-op -- which it must be, because
`persistence_contract` holds `cabal-build` only SHARED since #2274 and a
build under a shared hold is #1570's defect. Run BY HAND there is no
such export, so it performs the same preparation `probe_engine` performs
for the engine (#1913): one build and one `cabal list-bin` inside an
EXCLUSIVE hold, before any timed work starts.

Usage (as a library):
    from persistence_snapshot import compare_session_files, prepare_decoder
    prepare_decoder()          # once, before any timed work
    ok, detail = compare_session_files([path_a, path_b, path_c])
"""
from __future__ import annotations

import os
from pathlib import Path

import probe_engine  # noqa: E402 -- sibling module, tools/ on sys.path
import save_compat_audit_codec as codec  # noqa: E402
from save_compat_audit import (  # noqa: E402
    COMPARE_MISMATCH, COMPARE_OK, REPO_ROOT, compare_session_snapshots,
    dump_canonical_summary,
)

__all__ = ["REPO_ROOT", "compare_session_files", "prepare_decoder"]


def prepare_decoder(announce=print) -> str:
    """Resolve the compiled save decoder BEFORE any timed probe work.

    Returns its absolute path. A value already exported in
    `SYNARCHY_SAVE_CODEC_EXE` is taken verbatim and nothing is built --
    that is the aggregate runner's handoff, and the whole reason a probe
    the runner launched makes no Cabal contact. Otherwise this builds and
    locates the helper under an EXCLUSIVE `cabal-build` hold and exports
    the result into this process's environment, which is what also hands
    it down to a nested runner's own children.

    Raises `probe_engine.EnginePreparationError` when the helper cannot
    be prepared, the same refusal the engine's preparation raises: a
    probe that cannot decode its saves has nothing to assert, and
    discovering that after three engine boots is strictly worse than
    discovering it before the first.
    """
    exported = os.environ.get(codec.ENV_CODEC_EXE, "").strip()
    if exported:
        return exported
    resolved = probe_engine.prepare_executable(
        REPO_ROOT, announce=announce, target=codec.CODEC_TARGET,
        env_var=codec.ENV_CODEC_EXE)
    os.environ[codec.ENV_CODEC_EXE] = resolved
    return resolved


def _first_diff(actual, expected, path: str = "") -> str:
    """A short description of the first structural difference between two
    canonical-summary JSON values, recursing into nested dicts/lists -- or
    "" if they match. Diagnostic-only (never the pass/fail gate -- that's
    `compare_session_files`'s strict Eq/byte comparison via the real
    codec); mirrors `save_compat_migration_probe.py`'s identical helper."""
    if isinstance(expected, dict):
        if not isinstance(actual, dict):
            return f"{path}: expected an object, got {actual!r}"
        for k, v in expected.items():
            if k not in actual:
                return f"{path}.{k}: missing from the other summary"
            d = _first_diff(actual[k], v, f"{path}.{k}")
            if d:
                return d
        return ""
    if isinstance(expected, list):
        if not isinstance(actual, list):
            return f"{path}: expected an array, got {actual!r}"
        if len(actual) != len(expected):
            return f"{path}: {len(actual)} entries vs {len(expected)}"
        for i, (a, e) in enumerate(zip(actual, expected)):
            d = _first_diff(a, e, f"{path}[{i}]")
            if d:
                return d
        return ""
    if isinstance(expected, (int, float)) and isinstance(actual, (int, float)):
        if float(actual) != float(expected):
            return f"{path}: {actual!r} vs {expected!r}"
        return ""
    if actual != expected:
        return f"{path}: {actual!r} vs {expected!r}"
    return ""


# Keys that legitimately differ between two independently-saved files even
# when nothing gameplay-relevant changed (request metadata, contract
# requirement 5's own exclusion list) -- excluded from the diagnostic diff
# the same way `save_compat_migration_probe.py`'s comparison already does.
_DIAGNOSTIC_EXCLUDED_KEYS = frozenset({"$comment", "luaComponentCount", "isMigratedLegacyBaseline"})


def _canonicalize_for_diff(d: dict) -> dict:
    return {k: v for k, v in d.items() if k not in _DIAGNOSTIC_EXCLUDED_KEYS}


def diff_pair(paths: list[Path], report: dict | None) -> list[Path]:
    """The two files the canonical-summary diagnostic should compare.

    The comparison's reference (its first path) and the file that FIRST
    diverged from it, taken from the helper's own report. Both difference
    lists are consulted and the earlier entry in `paths` wins, so a run
    whose snapshots match while a `lua.<module>` payload does not still
    diffs the file that actually differs.

    Falls back to the first two paths when there is no usable report --
    which is what this always did before #2274, and is still the right
    answer for a decode failure, where nothing "first diverged".
    """
    if len(paths) < 2:
        return list(paths)
    fallback = list(paths[:2])
    if not isinstance(report, dict):
        return fallback
    reference = report.get("reference")
    differs = set()
    for key in ("snapshotDiffers", "luaComponentDiffers"):
        value = report.get(key)
        if isinstance(value, list):
            differs.update(str(entry) for entry in value)
    if not differs:
        return fallback
    first_divergent = next((p for p in paths if str(p) in differs), None)
    if first_divergent is None:
        return fallback
    anchor = next((p for p in paths if str(p) == str(reference)), paths[0])
    if str(anchor) == str(first_divergent):
        return fallback
    return [anchor, first_divergent]


def _summary_diff(paths: list[Path]) -> str:
    """A human-readable first structural difference between two saves.

    Best-effort and diagnostic-only: every failure here -- a helper that
    will not run, a summary that will not parse -- leaves the verdict and
    its marker line exactly as they were, because the pass/fail gate has
    already been decided by the real comparison.
    """
    import json
    import tempfile
    with tempfile.TemporaryDirectory() as td:
        summaries = []
        for p in paths:
            out = Path(td) / f"{p.stem}.summary.json"
            ok, _tail = dump_canonical_summary(p, out)
            summaries.append(out.read_text(encoding="utf-8") if ok else None)
        if not all(summaries) or len(summaries) < 2:
            return ""
        a = _canonicalize_for_diff(json.loads(summaries[0]))
        b = _canonicalize_for_diff(json.loads(summaries[1]))
        return _first_diff(a, b)


def compare_session_files(paths: list[Path]) -> tuple[bool, str]:
    """Decode every file in `paths` (at least 2) through the real save
    codec and assert every decoded `SessionSnapshot` plus every
    `lua.<module>` payload is pairwise structurally equal. Returns
    (all_equal, diagnostic) -- diagnostic is empty on success, or a
    human-readable explanation (including a per-file canonical-summary
    diff where available) on failure/decode error."""
    if len(paths) < 2:
        return True, ""
    outcome, report, detail = compare_session_snapshots(paths)
    if outcome == COMPARE_OK:
        return True, ""
    if outcome == COMPARE_MISMATCH:
        try:
            diff = _summary_diff(diff_pair(paths, report))
        except Exception:  # noqa: BLE001 -- diagnostic-only, never fatal
            diff = ""
        if diff:
            detail += (f"\nfirst structural difference (via canonical "
                       f"summary): {diff}")
    return False, detail
