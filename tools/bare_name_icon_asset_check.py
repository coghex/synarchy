#!/usr/bin/env python3
"""Bare-name icon reference check (issue #1740).

Standalone: no engine boot, no GPU, no window. Reads the Lua sources, the
infection YAML and the tracked PNGs, and exits non-zero when an
authoritative bare-name icon reference does not resolve.

Why this exists
---------------
Unit-info panel icons are referenced by BARE BASENAME. At runtime
`scripts/unit_info_v2_panel_engine.lua` builds ONE global
`basename -> full path` index over `ICON_SUBDIRS` and consults the row's
`<kind>_unknown` placeholder only when the basename is absent from that
global index. So a deleted or misspelled basename does not error: it
silently degrades to the row family's placeholder, which looks like art
that has not landed yet rather than like a broken reference. Nothing
verified those references.

The lookup semantics mirrored here
----------------------------------
Deliberately the SHIPPED global ones, not family-local ownership:

  * the index is built from the retained ordered `ICON_SUBDIRS` families;
  * a reference is accepted when its basename resolves ANYWHERE in that
    global index — never required to live in the row's fallback family;
  * an explicit reference to a `<kind>_unknown` placeholder is accepted,
    because a deliberate fallback is a legal reference;
  * `buildIconIndex` assigns `iconIndex[basename] = path` while iterating
    `ICON_SUBDIRS` IN ORDER, so a basename present in two families
    resolves to the LAST family in that order. This check implements that
    same last-wins rule, so the family it names as the supplier is the
    family the runtime would actually load. Duplicates are reported in the
    summary rather than passed over silently; there are none today.

Intentional cross-family reuse is PINNED (`cross_family_pins`): each pin
names the reference SITE, the exact ROWS of it that reuse the asset, the
row's own fallback family and the family that actually supplies it, and
requires the last two to DIFFER. Binding to the site and row rather than
to "the basename appears somewhere" is what makes the pin an assertion
rather than a coincidence -- `agility` and `strength` are each used by
their own physical-stat row AND by a skill row in one file, so a
basename-only pin would still pass after the skill row's reuse was
deleted. Moving one of those assets into the row's own family, dropping a
pinned reuse, or reinterpreting the lookup as family-local therefore fails
this check or its self-test instead of quietly changing meaning.

Fail loudly, never narrow
-------------------------
The real hazard for a checker like this is an input shape its extractor
does not recognise: a missed reference reads as coverage that is not
there. So extraction refuses rather than shrugs. Each of these is an
error naming `file:line` (or the expected table) and never a quietly
smaller reference set:

  * an unsupported table shape inside an enumerated value table;
  * a Haskell `icon` publication in neither an enumerated site nor the
    Haskell forwarding allowlist;
  * an icon assignment whose value is COMPUTED rather than literal and
    which is not in the reason-carrying forwarding allowlist;
  * an icon assignment found OUTSIDE the enumerated reference sites of a
    source whose sites are named tables/functions;
  * an unterminated string or long comment;
  * an enumerated source, table, function, anchor or allowlist entry that
    yields zero matches.

The forwarding allowlist is the closed, reason-carrying enumeration that
keeps that rule satisfiable: `scripts/` really does contain live `icon =`
assignments that FORWARD an already-extracted value (an entry of a table
this check reads, or `M.icon`'s own return) rather than naming a new
basename. Those are listed one by one with their reason. Anything else,
literal or computed, in neither an enumerated site nor the allowlist is a
failure.

Authoritative reference sources
-------------------------------
  * `scripts/injuries.lua` — `KIND_ICON`, `INJURY_ICON`, and the literal
    icon strings inside `M.icon`, `M.list`, `M.infectionList` and
    `M.scarList`
  * `scripts/unit_info_v2_stat_defs.lua` — literal `icon = "..."` fields
  * `scripts/unit_info_v2_status.lua` — literal `icon = "..."` fields
  * `scripts/knowledge.lua` — the knowledge registry's icons and
    `M.UNKNOWN_ICON`
  * `data/infections/*.yaml` — `icon:` scalars, which reach the identical
    global index through Engine.Scripting.Lua.API.Infection ->
    `infectionIcon` -> `scripts/injuries.lua`'s infection rows
  * every `.hs` under `src/` and `app/` that names the Lua `icon` field —
    the ENGINE publishes bare names into it with no Lua map in between:
    `Units/Combat.hs` pushes the immunity row's literal, and
    `Asset/YamlInfection.hs` supplies the default an infection def with no
    `icon:` silently gets. Scope is the whole tree rather than a fixed file
    list, so a NEW publication site fails loudly instead of joining the
    index unchecked.

Deliberately NOT covered: the skill panel derives a basename from the
live skill name (`scripts/unit_info_v2_panels.lua` with
`panel_engine.lua`'s `def and def.icon or statKey`), which is genuinely
dynamic and outside any static extractor's reach; and
`assets/textures/icons/location/`, which is absent from `ICON_SUBDIRS`
and owned by `tools/location_map_icon_asset_check.py`.

Module structure (#2142)
------------------------
This file is the sole public executable: documentation, argument
parsing and dispatch. Each concern has exactly one owner beside it:

  * `bare_name_icon_asset_core.py`      — shared leaf: `CheckError`,
    `Reference`, `LineMap`, `blank_span`, `REPO_ROOT`, `ICON_ROOT`
  * `bare_name_icon_asset_lua.py`       — Lua lexing, region location,
    reference extraction
  * `bare_name_icon_asset_yaml.py`      — infection-YAML discovery, and
    the PyYAML import behind the dependency diagnostic
  * `bare_name_icon_asset_haskell.py`   — Haskell cleaning and
    publication discovery
  * `bare_name_icon_asset_inventory.py` — the two family inventories
    and the last-wins basename index
  * `bare_name_icon_asset_audit.py`     — `run_check`: orchestration,
    pins, diagnostics, summary
  * `bare_name_icon_asset_config.py`    — `REPO_CONFIG`, the one
    production authority
  * `test_bare_name_icon_asset_check.py` — the isolated fixture corpus
    and all 43 self-test checks, reached only via `--self-test`

Dependencies run one way: core and config are leaves; the extractors
and the inventory consume core (the inventory also takes `clean_lua`
from the Lua owner); the audit consumes the extractors and inventory
and receives configuration as an argument; the self-test consumes the
production owners; this façade dispatches to the audit or self-test.

Usage:
  python3 tools/bare_name_icon_asset_check.py
  python3 tools/bare_name_icon_asset_check.py --self-test
Exit codes: 0 = every authoritative reference resolves and every pinned
contract holds, 1 = it does not (or extraction refused, or a self-test
check failed).
"""
from __future__ import annotations

import argparse
import os
import sys

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))

# Importing the audit owner reaches bare_name_icon_asset_yaml at startup,
# so a missing PyYAML fails here, eagerly, on both public invocations —
# exactly as it did when the import sat in this file.
from bare_name_icon_asset_audit import run_check  # noqa: E402
from bare_name_icon_asset_config import REPO_CONFIG  # noqa: E402
from bare_name_icon_asset_core import REPO_ROOT, CheckError  # noqa: E402
from test_bare_name_icon_asset_check import self_test  # noqa: E402


def main() -> int:
    parser = argparse.ArgumentParser(
        description="Check every authoritative bare-name icon reference "
                    "against the runtime's global icon index (#1740).")
    parser.add_argument("--self-test", action="store_true",
                        help="run the isolated-fixture checks for this tool")
    args = parser.parse_args()
    if args.self_test:
        print("bare-name icon asset check — self-test\n")
        return self_test()
    try:
        return run_check(REPO_ROOT, REPO_CONFIG)
    except CheckError as error:
        print(f"\nFAIL — extraction refused: {error}")
        return 1


if __name__ == "__main__":
    sys.exit(main())
