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
engine, no GPU, and no window -- it runs as a `cabal repl` subprocess
against raw files on disk, mirroring `save_compat_audit.py`'s
`dump_canonical_summary` subprocess pattern exactly. On a mismatch, it
additionally calls `save_compat_audit.dump_canonical_summary` (already
covers metadata/allocators/camera/every page's entities) on each file to
give a human-readable diagnostic of WHERE the two diverge -- the strict
Eq/byte check is the pass/fail gate; the summary dump is only for
debugging a failure.

Usage (as a library):
    from persistence_snapshot import compare_session_files
    ok, detail = compare_session_files([path_a, path_b, path_c])
"""
from __future__ import annotations

import subprocess
from pathlib import Path

from save_compat_audit import (  # noqa: E402 -- sibling module, tools/ on sys.path
    REPO_ROOT, dump_canonical_summary,
)

# A permanent GHCi program (run via `cabal repl`, mirroring
# save_compat_audit.GHCI_DUMP_SUMMARY_TEMPLATE's exact subprocess
# pattern) that decodes every listed file and asserts every decoded
# `SessionSnapshot` -- and every `lua.<module>` component's raw payload
# bytes -- is pairwise structurally equal. `{paths_literal}` is a
# Haskell list-of-Strings literal built by `compare_session_files`.
_GHCI_COMPARE_TEMPLATE = r"""
:set -XOverloadedStrings -XTypeApplications -XScopedTypeVariables
import qualified Data.ByteString as BS
import qualified Data.HashSet as HS
import qualified Data.HashMap.Strict as HM
import World.Save.Envelope (decodeSessionEnvelope)

let paths = {paths_literal}
let luaNames = HS.fromList ["unit_ai", "building_spawn"]

:{{
decodedList <- mapM (\p -> do
    bytes <- BS.readFile p
    return (p, decodeSessionEnvelope luaNames luaNames bytes)) paths
:}}

:{{
case sequence [ either (const Nothing) Just d | (_, d) <- decodedList ] of
  Nothing ->
    putStrLn ("DECODE_FAILED: " ++ show
      [ (p, err) | (p, Left err) <- decodedList ])
  Just decoded -> do
    let snaps = [ (p, snap) | ((p, _), (_, snap, _, _)) <- zip decodedList decoded ]
        luaPayloads = [ (p, HM.fromList [ (name, bytes) | (name, _, _, bytes) <- comps ])
                      | ((p, _), (_, _, comps, _)) <- zip decodedList decoded ]
    case (snaps, luaPayloads) of
      ((_, firstSnap) : snapRest, (_, firstLua) : luaRest) -> do
        let snapMismatches = [ p | (p, s) <- snapRest, s /= firstSnap ]
            luaMismatches = [ p | (p, m) <- luaRest, m /= firstLua ]
        if null snapMismatches && null luaMismatches
          then putStrLn "COMPARE_OK"
          else putStrLn ("COMPARE_MISMATCH: snapshot-differs=" ++ show snapMismatches
                          ++ " lua-component-differs=" ++ show luaMismatches)
      _ -> putStrLn "COMPARE_EMPTY: fewer than one path decoded"
:}}
"""


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


def _haskell_string_list_literal(paths: list[Path]) -> str:
    escaped = [str(p).replace("\\", "\\\\").replace('"', '\\"') for p in paths]
    return "[" + ", ".join(f'"{p}"' for p in escaped) + "]"


def compare_session_files(paths: list[Path]) -> tuple[bool, str]:
    """Decode every file in `paths` (at least 2) through the real save
    codec and assert every decoded `SessionSnapshot` plus every
    `lua.<module>` payload is pairwise structurally equal. Returns
    (all_equal, diagnostic) -- diagnostic is empty on success, or a
    human-readable explanation (including a per-file canonical-summary
    diff where available) on failure/decode error."""
    if len(paths) < 2:
        return True, ""
    script = _GHCI_COMPARE_TEMPLATE.format(
        paths_literal=_haskell_string_list_literal(paths))
    try:
        proc = subprocess.run(
            ["cabal", "repl", "test:synarchy-test-headless"],
            input=script, cwd=REPO_ROOT, capture_output=True, text=True,
            timeout=1800)
    except FileNotFoundError:
        return False, "'cabal' was not found on PATH"
    output = (proc.stdout or "") + (proc.stderr or "")
    if "COMPARE_OK" in output:
        return True, ""
    if "COMPARE_MISMATCH" in output or "DECODE_FAILED" in output:
        detail_lines = [ln for ln in output.splitlines()
                         if "COMPARE_MISMATCH" in ln or "DECODE_FAILED" in ln]
        detail = "\n".join(detail_lines)
        # Best-effort human-readable diff between the first two files, via
        # the SAME canonical-summary dump save_compat_audit.py already
        # establishes -- only for debugging, never the pass/fail gate.
        try:
            import tempfile
            with tempfile.TemporaryDirectory() as td:
                summaries = []
                for p in paths[:2]:
                    out = Path(td) / f"{p.stem}.summary.json"
                    ok, _tail = dump_canonical_summary(p, out)
                    summaries.append(out.read_text(encoding="utf-8") if ok else None)
                if all(summaries):
                    import json
                    a = _canonicalize_for_diff(json.loads(summaries[0]))
                    b = _canonicalize_for_diff(json.loads(summaries[1]))
                    diff = _first_diff(a, b)
                    if diff:
                        detail += f"\nfirst structural difference (via canonical summary): {diff}"
        except Exception:  # noqa: BLE001 -- diagnostic-only, never fatal
            pass
        return False, detail
    return False, "\n".join(output.splitlines()[-60:])
