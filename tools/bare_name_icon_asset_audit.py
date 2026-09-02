"""Audit orchestration and reporting for the bare-name icon gate (#1740,
split by #2142 requirement 11).

`run_check(root, config, out=None)` is the whole audit: it collects every
configured Lua, YAML and Haskell reference, detects missing basenames,
validates each cross-family pin against its exact site, rows, row family
and supplying family, and writes the source / map / line /
searched-family / supplier diagnostics and the summary in the order the
live output has always had.

The root and configuration are PARAMETERS, never imported: that is what
lets the self-test drive this exact code against a temporary fixture tree
with its own configuration, and what keeps the module graph acyclic even
though the production configuration is a sibling leaf. `out` is the
injectable sink the self-test captures through.

Two orderings here are part of the public contract and are preserved
verbatim:

  * the interleaved write-then-raise sequence — family inventory,
    duplicate notes and per-site counts are written BEFORE the stale
    forwarding-allowlist entries are judged, and the `Extracted N ...`
    line before any missing-basename or pin failure; a self-pinning
    cross-family entry raises mid-stream;
  * forwarding-allowlist staleness is judged HERE, once, after BOTH the
    Lua and Haskell extractors have recorded their hits in the one
    `allow_hits` dict this module builds — an entry that matched nothing
    in either is the stale one, and neither extractor may decide that
    alone.

Consumes the shared leaf, the three extractors and the inventory owner.
Exit statuses are 0 (every reference resolves and every pin holds) or 1;
a `CheckError` propagates to the caller, which decides how to report it.
"""
from __future__ import annotations

import sys
from pathlib import Path

from bare_name_icon_asset_core import ICON_ROOT, CheckError
from bare_name_icon_asset_haskell import extract_haskell
from bare_name_icon_asset_inventory import build_index, loader_families, panel_families
from bare_name_icon_asset_lua import extract_lua
from bare_name_icon_asset_yaml import extract_yaml


def run_check(root: Path, config: dict, out=None) -> int:
    write = (out or sys.stdout).write
    failures = []

    panel = panel_families(root, config["panel_inventory"])
    loader = loader_families(root, config["loader_inventory"])
    write(f"Icon family inventory ({config['panel_inventory']['path']}): "
          f"{', '.join(panel)}\n")
    if set(panel) != set(loader):
        only_panel = sorted(set(panel) - set(loader))
        only_loader = sorted(set(loader) - set(panel))
        failures.append(
            "the two runtime icon-family inventories disagree: "
            f"{config['panel_inventory']['path']} has "
            f"{only_panel or 'nothing extra'}, "
            f"{config['loader_inventory']['path']} has "
            f"{only_loader or 'nothing extra'}. A family must be added to "
            "BOTH, and must own its <kind>_unknown.png fallback.")

    for family in panel:
        placeholder = root / ICON_ROOT / family / f"{family}_unknown.png"
        if not placeholder.is_file():
            failures.append(
                f"retained icon family {family!r} has no fallback placeholder "
                f"{ICON_ROOT}/{family}/{family}_unknown.png — every family in "
                f"the runtime inventory must own one, because it is what a row "
                f"of that kind draws when its basename misses the global index.")

    index, duplicates, missing_dirs = build_index(root, panel)
    for family in missing_dirs:
        failures.append(
            f"retained icon family {family!r} has no directory "
            f"{ICON_ROOT}/{family}/")
    for basename, first, last in duplicates:
        write(f"  note: basename {basename!r} exists in both {first!r} and "
              f"{last!r}; buildIconIndex's ordered last-wins rule resolves it "
              f"to {last!r}\n")

    allow_hits = {(entry["file"], entry["target"], entry["rhs"]): 0
                  for entry in config["forwarding_allowlist"]}
    allow_hits.update({(entry["file"], entry["pattern"]): 0
                       for entry in config.get("haskell_forwarding_allowlist", [])})

    references = []
    for spec in config["lua_sources"]:
        references.extend(extract_lua(root, spec, allow_hits))
    for spec in config.get("yaml_sources", []):
        references.extend(extract_yaml(root, spec))
    references.extend(extract_haskell(root, config, allow_hits))

    counts = {}
    for reference in references:
        counts[reference.site] = counts.get(reference.site, 0) + 1
    for site in sorted(counts):
        write(f"  {counts[site]:3d} reference(s) from {site}\n")

    for entry in config["forwarding_allowlist"]:
        key = (entry["file"], entry["target"], entry["rhs"])
        if allow_hits[key] == 0:
            raise CheckError(
                f"{entry['file']}: forwarding allowlist entry "
                f"`{entry['target']} = {entry['rhs']}` matched nothing; a stale "
                f"entry would silently permit a future computed assignment. "
                f"Remove it or correct it.")
    for entry in config.get("haskell_forwarding_allowlist", []):
        if allow_hits[(entry["file"], entry["pattern"])] == 0:
            raise CheckError(
                f"{entry['file']}: Haskell forwarding allowlist entry "
                f"{entry['pattern']!r} matched nothing; a stale entry would "
                f"silently permit a future engine-published basename. Remove "
                f"it or correct it.")

    source_count = (len(config["lua_sources"])
                    + len(config.get("yaml_sources", []))
                    + len({e["file"] for e in config.get("haskell_sites", [])}))
    write(f"Extracted {len(references)} authoritative bare-name references "
          f"from {source_count} sources\n")

    searched = ", ".join(panel)
    for reference in sorted(references, key=lambda r: (r.source, r.line, r.basename)):
        if reference.basename not in index:
            failures.append(
                f"missing icon basename {reference.basename!r}\n"
                f"      referenced by : {reference.source}:{reference.line}\n"
                f"      source map    : {reference.site}\n"
                f"      searched      : {ICON_ROOT}/<family>/ over families "
                f"{searched} (global index, exactly as "
                f"buildIconIndex resolves it)")

    for pin in config["cross_family_pins"]:
        basename, expected, row = pin["basename"], pin["family"], pin["row_family"]
        if expected == row:
            raise CheckError(
                f"cross-family pin {basename!r} declares the row's own fallback "
                f"family {row!r} as its supplier, so it pins nothing. A pin "
                f"exists to state that the asset comes from ANOTHER family; "
                f"drop it or correct it ({pin['reason']})")
        # Bind to the exact SITE and ROW, never to "the basename appears
        # somewhere". `agility` is used by both the physical-stat row and the
        # Dodge/Jumping SKILL rows in one file; a pin that accepted either
        # would keep asserting a cross-family use that had been removed.
        at_site = [r for r in references
                   if r.site == pin["site"] and r.basename == basename]
        missing_rows = [pin_row for pin_row in pin["rows"]
                        if not any(r.row == pin_row for r in at_site)]
        if missing_rows:
            failures.append(
                f"cross-family pin {basename!r} names row(s) "
                f"{', '.join(repr(r) for r in missing_rows)} of {pin['site']}, "
                f"but no reference there uses that basename on those rows "
                f"(present rows: "
                f"{', '.join(sorted(repr(r.row) for r in at_site)) or 'none'}). "
                f"The pinned reuse is gone or moved; re-decide it rather than "
                f"leaving a pin that asserts something untrue "
                f"({pin['reason']})")
            continue
        actual = index.get(basename)
        if actual is None:
            continue  # already reported as a missing basename
        if actual != expected:
            extra = (" — it now lives in the row's OWN fallback family, so the "
                     "reference is no longer cross-family"
                     if actual == row else "")
            failures.append(
                f"cross-family pin {basename!r} is supplied by {actual!r}, not "
                f"the pinned {expected!r}{extra}. That is a deliberate change "
                f"to the shipped global lookup and must be re-decided, not "
                f"absorbed ({pin['reason']})")
        else:
            named = [r for r in pin["rows"] if r is not None]
            where = (f"{pin['site']} row(s) {', '.join(repr(r) for r in named)}"
                     if named else pin["site"])
            write(f"  cross-family: {where} uses {basename!r} on a "
                  f"{row!r}-fallback row; supplied by family {actual!r}\n")

    if failures:
        write(f"\nFAIL — {len(failures)} problem(s):\n")
        for failure in failures:
            write(f"  - {failure}\n")
        return 1
    write("\nOK — every authoritative bare-name icon reference resolves "
          "through the global index.\n")
    return 0
