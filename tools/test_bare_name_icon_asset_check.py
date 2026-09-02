"""Isolated fixture self-test for tools/bare_name_icon_asset_check.py and the
`tools/bare_name_icon_asset_<owner>.py` modules behind it (#1740, moved
out of the façade by #2142 requirements 14-18).

Reached ONLY through the public façade:

    python3 tools/bare_name_icon_asset_check.py --self-test

There is deliberately no separate command here — the façade is the sole
public executable, and CI and `make ci` invoke it twice and nothing else.

Every case builds a complete fixture repository in its own temporary
directory (Lua icon maps, a panel inventory, a startup loader, infection
YAML, three Haskell publication sites and an icon corpus of empty PNGs),
applies ONE mutation, and runs the production audit against that tree
with the fixture's own configuration. Nothing here reads, modifies or
deletes the shipped icon corpus, Lua sources, Haskell sources or
infection YAML.

`_run_case` catches the production `CheckError` and maps it to exit 2,
which is a self-test-internal convention: the façade's public statuses
stay 0 and 1. The 43 checks are 42 table-driven cases plus one inline
family-local resolver comparison that calls the inventory owner
directly, so the count printed is `len(checks) + 1`.

Consumes the production owners: the audit (`run_check`), the shared leaf
(`CheckError`, `ICON_ROOT`) and the inventory owner (`panel_families`,
`build_index`) — never a façade alias.
"""
from __future__ import annotations

import os
import sys
import tempfile
from pathlib import Path

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))

from bare_name_icon_asset_audit import run_check  # noqa: E402
from bare_name_icon_asset_core import ICON_ROOT, CheckError  # noqa: E402
from bare_name_icon_asset_inventory import build_index, panel_families  # noqa: E402


FIXTURE_FAMILIES = ("stat", "skill", "status")
FIXTURE_ASSETS = {
    "stat": ["stat_unknown", "agility"],
    "skill": ["skill_unknown"],
    "status": ["status_unknown", "pain", "broken_bone", "joint_injury",
               "rot_injury", "scar", "know_a", "bacterial_infection",
               "immunity"],
}

FIXTURE_PANEL = """-- fixture panel engine
local ICON_SUBDIRS = { "stat", "skill", "status" }
"""

FIXTURE_LOADER = """-- fixture startup loader
for _, sub in ipairs({ "stat", "skill", "status" }) do
    addTextureDir("assets/textures/icons/" .. sub, "Loading icons...")
end
"""

FIXTURE_INJ = """-- fixture injury maps
local KIND_ICON = {
    fracture = "broken_bone",
}

local INJURY_ICON = {
    ["fracture|foot"] = "joint_injury",
}

function M.icon(kind, part)
    kind = kind or "blunt"
    return INJURY_ICON[kind]
        or KIND_ICON[kind]
        or "pain"
end

function M.list(uid)
    local rowIcon = M.icon("blunt", "hand")
    if true then rowIcon = "rot_injury" end
    out[#out + 1] = {
        icon = rowIcon,
    }
end

function M.scarList(uid)
    out[#out + 1] = {
        icon     = "scar",
    }
end
"""

FIXTURE_DEFS = """-- fixture stat defs
local STAT_DEFS = {
    -- The physical-stat row: same-family, deliberately NOT pinned. It is the
    -- decoy that a basename-only pin would wrongly accept.
    agility = { icon = "agility", name = "Agility" },
    -- The skill row: the pinned cross-family reuse.
    dodge   = { icon = "agility", name = "Dodge" },
    hurt    = { icon = "pain",    name = "Pain" },
}
"""

FIXTURE_KNOW = """-- fixture knowledge registry
M.REGISTRY = {
    {
        id   = "a",
        icon = "know_a",
        desc = "one" .. "two",
    },
}

M.UNKNOWN_ICON = "stat_unknown"
"""

FIXTURE_YAML = """infections:
  - id: bug
    icon: bacterial_infection
"""

# A literal the engine pushes straight into the Lua `icon` field, a decoder
# default, a forwarding site, and -- deliberately -- the field name inside a
# comment, which must NOT read as a reference site.
FIXTURE_COMBAT = """module Fixture.Combat where

-- | Publishes the immunity row. The Lua side reads its "icon" field.
pushRow :: IO ()
pushRow = do
    Lua.pushstring (TE.encodeUtf8 "immunity")
    Lua.setfield (-2) "icon"
"""

FIXTURE_DECODER = """module Fixture.Decoder where

parseInfection v = Infection
    <$> v .: "id"
    <*> v .:? "icon" .!= "bacterial_infection"
"""

FIXTURE_FORWARD = """module Fixture.Forward where

publish d = putS "icon" (infIcon d)
"""


def fixture_config() -> dict:
    return {
        "panel_inventory": {"path": "scripts/panel.lua", "name": "ICON_SUBDIRS"},
        "loader_inventory": {
            "path": "scripts/loader.lua",
            "anchor": "addTextureDir(\"assets/textures/icons/\"",
        },
        "lua_sources": [
            {
                "path": "scripts/inj.lua",
                "targets": ["icon", "rowIcon"],
                "regions": [
                    {"kind": "value_table", "name": "KIND_ICON"},
                    {"kind": "value_table", "name": "INJURY_ICON"},
                    {"kind": "function", "name": "M.icon", "anchors": [{
                        "name": "M.icon last-resort fallback",
                        "pattern": r"^\s*or\s+\"(?P<name>[A-Za-z0-9_]+)\"\s*$",
                    }]},
                    {"kind": "function", "name": "M.list"},
                    {"kind": "function", "name": "M.scarList"},
                ],
            },
            {"path": "scripts/defs.lua", "whole_file_is_a_site": True,
             "regions": [{"kind": "file", "name": "literal `icon =` fields"}]},
            {
                "path": "scripts/know.lua",
                "targets": ["icon", "M.UNKNOWN_ICON"],
                "regions": [
                    {"kind": "table", "name": "M.REGISTRY"},
                    {"kind": "assignment", "name": "M.UNKNOWN_ICON"},
                ],
            },
        ],
        "yaml_sources": [{"dir": "data/inf", "key": "icon"}],
        "haskell_roots": ["src"],
        "haskell_field": "icon",
        "haskell_sites": [
            {"file": "src/Combat.hs", "name": "pushed `icon`",
             "pattern": r'pushstring\s*\([^\n"]*"(?P<name>[A-Za-z0-9_]+)"\s*\)'
                        r'\s*\n\s*Lua\.setfield\s*\(-2\)\s*"icon"'},
            {"file": "src/Decoder.hs", "name": "decoder default",
             "pattern": r'"icon"\s*\.!=\s*"(?P<name>[A-Za-z0-9_]+)"'},
        ],
        "haskell_forwarding_allowlist": [
            {"file": "src/Forward.hs", "pattern": r'putS\s+"icon"\s+\(infIcon d\)',
             "reason": "fixture: forwards an already-extracted value"},
        ],
        "forwarding_allowlist": [
            {"file": "scripts/inj.lua", "target": "rowIcon",
             "rhs": "M.icon(\"blunt\", \"hand\")",
             "reason": "forwards M.icon, whose literals are extracted"},
            {"file": "scripts/inj.lua", "target": "icon", "rhs": "rowIcon",
             "reason": "forwards the row's rowIcon"},
        ],
        "cross_family_pins": [
            {"basename": "agility", "family": "stat", "row_family": "skill",
             "site": "scripts/defs.lua", "rows": ["dodge"],
             "reason": "fixture: a skill-fallback row drawing a stat asset"},
        ],
    }


def build_fixture(base: Path) -> Path:
    root = base
    (root / "scripts").mkdir(parents=True, exist_ok=True)
    (root / "data" / "inf").mkdir(parents=True, exist_ok=True)
    (root / "src").mkdir(parents=True, exist_ok=True)
    (root / "src" / "Combat.hs").write_text(FIXTURE_COMBAT, encoding="utf-8")
    (root / "src" / "Decoder.hs").write_text(FIXTURE_DECODER, encoding="utf-8")
    (root / "src" / "Forward.hs").write_text(FIXTURE_FORWARD, encoding="utf-8")
    (root / "scripts" / "panel.lua").write_text(FIXTURE_PANEL, encoding="utf-8")
    (root / "scripts" / "loader.lua").write_text(FIXTURE_LOADER, encoding="utf-8")
    (root / "scripts" / "inj.lua").write_text(FIXTURE_INJ, encoding="utf-8")
    (root / "scripts" / "defs.lua").write_text(FIXTURE_DEFS, encoding="utf-8")
    (root / "scripts" / "know.lua").write_text(FIXTURE_KNOW, encoding="utf-8")
    (root / "data" / "inf" / "a.yaml").write_text(FIXTURE_YAML, encoding="utf-8")
    for family, names in FIXTURE_ASSETS.items():
        directory = root / ICON_ROOT / family
        directory.mkdir(parents=True, exist_ok=True)
        for name in names:
            (directory / f"{name}.png").write_bytes(b"")
    return root


def _edit(root: Path, relative: str, old: str, new: str) -> None:
    path = root / relative
    text = path.read_text(encoding="utf-8")
    if old not in text:
        raise AssertionError(f"fixture edit anchor not found in {relative}: {old!r}")
    path.write_text(text.replace(old, new, 1), encoding="utf-8")


def _run_case(root: Path, config: dict):
    import io
    buffer = io.StringIO()
    try:
        code = run_check(root, config, out=buffer)
        return code, buffer.getvalue()
    except CheckError as error:
        return 2, buffer.getvalue() + "\nEXTRACTION REFUSED: " + str(error)


def _drop_asset(root: Path, family: str, name: str) -> None:
    (root / ICON_ROOT / family / f"{name}.png").unlink()


def self_test() -> int:
    """Every case runs against its own isolated fixture tree."""
    checks = []

    def case(name, mutate, expect_code, expect_text=""):
        checks.append((name, mutate, expect_code, expect_text))

    # 1. Every supported map shape accepts a legal reference.
    case("baseline: every supported map shape resolves", lambda r, c: None, 0,
         "every authoritative bare-name icon reference resolves")

    # 2. Every supported map shape detects a deliberately missing reference.
    case("KIND_ICON value detects a missing asset",
         lambda r, c: _drop_asset(r, "status", "broken_bone"), 1, "'broken_bone'")
    case("INJURY_ICON value detects a missing asset",
         lambda r, c: _drop_asset(r, "status", "joint_injury"), 1, "'joint_injury'")
    case("M.icon last-resort anchor detects a missing asset",
         lambda r, c: _drop_asset(r, "status", "pain"), 1, "'pain'")
    case("M.list literal detects a missing asset",
         lambda r, c: _drop_asset(r, "status", "rot_injury"), 1, "'rot_injury'")
    case("M.scarList literal detects a missing asset",
         lambda r, c: _drop_asset(r, "status", "scar"), 1, "'scar'")
    case("stat-defs icon field detects a missing asset",
         lambda r, c: _drop_asset(r, "stat", "agility"), 1, "'agility'")
    case("knowledge registry icon detects a missing asset",
         lambda r, c: _drop_asset(r, "status", "know_a"), 1, "'know_a'")
    case("M.UNKNOWN_ICON detects a missing asset",
         lambda r, c: _drop_asset(r, "stat", "stat_unknown"), 1, "'stat_unknown'")
    case("infection YAML icon detects a missing asset",
         lambda r, c: _drop_asset(r, "status", "bacterial_infection"), 1,
         "'bacterial_infection'")
    # A YAML reference must carry its REAL line, like every Lua one.
    case("the YAML diagnostic names the real source line",
         lambda r, c: _drop_asset(r, "status", "bacterial_infection"), 1,
         "data/inf/a.yaml:3")
    case("a non-string YAML icon scalar is refused",
         lambda r, c: (r / "data" / "inf" / "a.yaml").write_text(
             "infections:\n  - id: bug\n    icon: 12\n", encoding="utf-8"),
         2, "a.yaml:3: `icon:` must be a non-empty string basename")

    # 2b. The missing-basename diagnostic names everything requirement 8 asks
    #     for: basename, file:line, source map, and the searched families.
    case("missing-basename diagnostic names source, map and searched families",
         lambda r, c: _drop_asset(r, "status", "scar"), 1, "source map    :")
    case("missing-basename diagnostic names the searched families",
         lambda r, c: _drop_asset(r, "status", "scar"), 1,
         "over families stat, skill, status")

    # 3. An explicit legal <kind>_unknown reference is accepted.
    case("an explicit <kind>_unknown reference is legal",
         lambda r, c: _edit(r, "scripts/know.lua",
                            'M.UNKNOWN_ICON = "stat_unknown"',
                            'M.UNKNOWN_ICON = "skill_unknown"'), 0,
         "every authoritative bare-name icon reference resolves")

    # 4. Global cross-family references resolve to the expected supplier.
    case("a cross-family reference names its real row and supplying family",
         lambda r, c: None, 0,
         "scripts/defs.lua row(s) 'dodge' uses 'agility' on a "
         "'skill'-fallback row; supplied by family 'stat'")
    case("a family-local move of a pinned asset is refused",
         lambda r, c: (_drop_asset(r, "stat", "agility"),
                       (r / ICON_ROOT / "skill" / "agility.png").write_bytes(b"")),
         1, "no longer cross-family")
    case("a pin naming the row's own family as its supplier is refused",
         lambda r, c: c["cross_family_pins"][0].update(family="skill"), 2,
         "pins nothing")
    # The pin must bind to its ROW, not merely to the basename: the untouched
    # physical-stat row keeps referencing 'agility' in the same file.
    case("a pin is not satisfied by an unrelated same-basename reference",
         lambda r, c: _edit(r, "scripts/defs.lua",
                            '    dodge   = { icon = "agility", name = "Dodge" },',
                            '    dodge   = { icon = "pain",    name = "Dodge" },'),
         1, "names row(s) 'dodge'")
    case("that same mutation leaves the decoy reference in place",
         lambda r, c: _edit(r, "scripts/defs.lua",
                            '    dodge   = { icon = "agility", name = "Dodge" },',
                            '    dodge   = { icon = "pain",    name = "Dodge" },'),
         1, "present rows: 'agility'")
    case("a pin naming a row of the wrong site is refused",
         lambda r, c: c["cross_family_pins"][0].update(site="scripts/inj.lua"),
         1, "present rows: none")

    # 6. An unsupported table shape fails loudly.
    case("an unsupported table shape is refused",
         lambda r, c: _edit(r, "scripts/inj.lua",
                            '    fracture = "broken_bone",',
                            '    fracture = "broken_bone",\n    "orphan",'),
         2, "unsupported table shape")
    case("a computed value inside an enumerated value table is refused",
         lambda r, c: _edit(r, "scripts/inj.lua",
                            '    fracture = "broken_bone",',
                            '    fracture = pickIcon(),'),
         2, "has a computed value")

    # 7. A computed icon expression outside the allowlist fails loudly.
    case("a NEW computed icon assignment outside the allowlist is refused",
         lambda r, c: _edit(r, "scripts/defs.lua",
                            '    hurt    = { icon = "pain",    name = "Pain" },',
                            '    hurt    = { icon = derived,   name = "Pain" },'),
         2, "computed rather than literal")

    # 7b. A literal icon string outside the enumerated sites fails loudly.
    case("a NEW literal icon string outside the enumerated sites is refused",
         lambda r, c: _edit(r, "scripts/inj.lua",
                            "-- fixture injury maps",
                            "-- fixture injury maps\nlocal icon = \"pain\""),
         2, "lies OUTSIDE every enumerated reference site")

    # 8. An unterminated string fails loudly.
    case("an unterminated string is refused",
         lambda r, c: _edit(r, "scripts/inj.lua",
                            '    fracture = "broken_bone",',
                            '    fracture = "broken_bone,'),
         2, "unterminated string literal")

    # 9. An expected source or table yielding zero references fails loudly.
    case("an emptied expected table is refused",
         lambda r, c: _edit(r, "scripts/inj.lua",
                            'local INJURY_ICON = {\n    ["fracture|foot"] '
                            '= "joint_injury",\n}',
                            'local INJURY_ICON = {\n}'),
         2, "produced zero references")
    case("a renamed expected table is refused",
         lambda r, c: _edit(r, "scripts/inj.lua", "local INJURY_ICON = {",
                            "local INJURY_ICONS = {"),
         2, "expected table `INJURY_ICON` was not found")
    case("a missing expected source is refused",
         lambda r, c: (r / "scripts" / "know.lua").unlink(), 2,
         "expected authoritative source is missing")
    case("a YAML source with no icon scalars is refused",
         lambda r, c: (r / "data" / "inf" / "a.yaml").write_text(
             "infections:\n  - id: bug\n", encoding="utf-8"),
         2, "refuses rather than silently narrowing")
    case("an emptied YAML source directory is refused",
         lambda r, c: (r / "data" / "inf" / "a.yaml").unlink(), 2,
         "produced no files")
    # The engine publishes bare names into the same Lua field without any Lua
    # map in between, so those sites are covered too -- and a mention of the
    # field in a COMMENT must not read as one.
    case("an engine-pushed icon literal detects a missing asset",
         lambda r, c: _drop_asset(r, "status", "immunity"), 1, "'immunity'")
    case("the engine-pushed diagnostic names its Haskell source and line",
         lambda r, c: _drop_asset(r, "status", "immunity"), 1, "src/Combat.hs:6")
    case("a decoder-default icon literal detects a missing asset",
         lambda r, c: _drop_asset(r, "status", "bacterial_infection"), 1,
         "src/Decoder.hs:5")
    case("the field name inside a Haskell comment is not a reference site",
         lambda r, c: None, 0,
         "every authoritative bare-name icon reference resolves")
    case("a NEW engine-published icon field outside the sites is refused",
         lambda r, c: (r / "src" / "Extra.hs").write_text(
             'module Fixture.Extra where\n\npush = Lua.setfield (-2) "icon"\n',
             encoding="utf-8"),
         2, "neither an enumerated Haskell reference site")
    case("a Haskell reference site that matches nothing is refused",
         lambda r, c: _edit(r, "src/Combat.hs",
                            '    Lua.setfield (-2) "icon"',
                            '    Lua.setfield (-2) "ikon"'),
         2, "matched nothing")
    case("a stale Haskell forwarding-allowlist entry is refused",
         lambda r, c: (r / "src" / "Forward.hs").write_text(
             "module Fixture.Forward where\n", encoding="utf-8"),
         2, "no longer names that field")
    case("a stale forwarding-allowlist entry is refused",
         lambda r, c: _edit(r, "scripts/inj.lua", "        icon = rowIcon,\n", ""),
         2, "matched nothing")

    # 10. A synthetic ICON_SUBDIRS family fails until BOTH runtime inventories
    #     and the fallback contract represent it.
    case("a synthetic family in one inventory only is refused",
         lambda r, c: _edit(r, "scripts/panel.lua",
                            '{ "stat", "skill", "status" }',
                            '{ "stat", "skill", "status", "synth" }'),
         1, "runtime icon-family inventories disagree")

    def both_inventories(root, config):
        _edit(root, "scripts/panel.lua", '{ "stat", "skill", "status" }',
              '{ "stat", "skill", "status", "synth" }')
        _edit(root, "scripts/loader.lua", '{ "stat", "skill", "status" }',
              '{ "stat", "skill", "status", "synth" }')

    case("a synthetic family in both inventories with no fallback is refused",
         both_inventories, 1, "no fallback placeholder")

    def both_plus_fallback(root, config):
        both_inventories(root, config)
        directory = root / ICON_ROOT / "synth"
        directory.mkdir(parents=True, exist_ok=True)
        (directory / "synth_unknown.png").write_bytes(b"")

    case("a synthetic family with both inventories and its fallback passes",
         both_plus_fallback, 0,
         "every authoritative bare-name icon reference resolves")

    failed = 0
    for name, mutate, expect_code, expect_text in checks:
        with tempfile.TemporaryDirectory() as tmp:
            root = build_fixture(Path(tmp))
            config = fixture_config()
            mutate(root, config)
            code, output = _run_case(root, config)
        ok = code == expect_code and expect_text in output
        print(f"  [{'PASS' if ok else 'FAIL'}] {name}")
        if not ok:
            failed += 1
            print(f"      expected exit {expect_code} containing "
                  f"{expect_text!r}, got exit {code}")
            for line in output.strip().splitlines():
                print(f"      | {line}")

    # 5. Rewriting the global lookup as FAMILY-LOCAL must fail. Proved
    #    directly against the resolver rather than through a source mutation:
    #    a family-local index restricted to the row's own fallback family can
    #    no longer supply the pinned cross-family basename.
    with tempfile.TemporaryDirectory() as tmp:
        root = build_fixture(Path(tmp))
        config = fixture_config()
        families = panel_families(root, config["panel_inventory"])
        pin = config["cross_family_pins"][0]
        global_index, _, _ = build_index(root, families)
        local_index, _, _ = build_index(root, families,
                                        restrict_to=pin["row_family"])
        ok = (global_index.get(pin["basename"]) == pin["family"]
              and pin["basename"] not in local_index)
        print(f"  [{'PASS' if ok else 'FAIL'}] a family-local rewrite of the "
              f"lookup loses the pinned cross-family reference")
        if not ok:
            failed += 1
            print(f"      global={global_index.get(pin['basename'])!r} "
                  f"local-has={pin['basename'] in local_index}")

    print()
    if failed:
        print(f"FAIL — {failed} self-test check(s) failed")
        return 1
    print(f"OK — {len(checks) + 1} self-test checks passed")
    return 0
