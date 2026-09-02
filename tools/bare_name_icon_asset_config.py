"""The repository's own authoritative configuration for the bare-name icon
gate (#1740, split by #2142 requirements 12 and 13).

The single source of: the authoritative Lua source paths and their
enumerated table / function / anchor / assignment regions, the infection
YAML glob, the Haskell roots and enumerated publication sites, the Lua
and Haskell forwarding allowlists with their reasons, the panel and
loader inventory locations, and the cross-family pins. Scanners receive
this as a parameter and the self-test uses its own isolated fixture
configuration; nothing copies it.

A LEAF: imports nothing from the other `bare_name_icon_asset_*` owners.
Read tools/bare_name_icon_asset_check.py's docstring for what each
section means and why the allowlists and pins are shaped the way they
are.
"""

COMBAT = "src/Engine/Scripting/Lua/API/Units/Combat.hs"
YAML_INFECTION = "src/Engine/Asset/YamlInfection.hs"
INFECTION_API = "src/Engine/Scripting/Lua/API/Infection.hs"
INJURIES = "scripts/injuries.lua"
STATUS = "scripts/unit_info_v2_status.lua"
STAT_DEFS = "scripts/unit_info_v2_stat_defs.lua"
KNOWLEDGE = "scripts/knowledge.lua"


REPO_CONFIG = {
    "panel_inventory": {
        "path": "scripts/unit_info_v2_panel_engine.lua",
        "name": "ICON_SUBDIRS",
    },
    "loader_inventory": {
        "path": "scripts/startup_loader.lua",
        "anchor": "addTextureDir(\"assets/textures/icons/\"",
    },
    "lua_sources": [
        {
            "path": INJURIES,
            # `rowIcon` is the injury row's own icon variable; it carries
            # literal basenames for the frostbite-rot swap.
            "targets": ["icon", "rowIcon"],
            "regions": [
                {"kind": "value_table", "name": "KIND_ICON"},
                {"kind": "value_table", "name": "INJURY_ICON"},
                {"kind": "function", "name": "M.icon", "anchors": [{
                    "name": "M.icon last-resort fallback",
                    # A continuation line that is ONLY `or "<literal>"`. Written
                    # this narrowly so `kind = kind or "blunt"` (a display name,
                    # not an icon) is not mistaken for one.
                    "pattern": r"^\s*or\s+\"(?P<name>[A-Za-z0-9_]+)\"\s*$",
                }]},
                {"kind": "function", "name": "M.list"},
                {"kind": "function", "name": "M.infectionList"},
                {"kind": "function", "name": "M.scarList"},
            ],
        },
        {"path": STAT_DEFS, "whole_file_is_a_site": True,
         "regions": [{"kind": "file", "name": "literal `icon =` fields"}]},
        {"path": STATUS, "whole_file_is_a_site": True,
         "regions": [{"kind": "file", "name": "literal `icon =` fields"}]},
        {
            "path": KNOWLEDGE,
            "targets": ["icon", "M.UNKNOWN_ICON"],
            "regions": [
                {"kind": "table", "name": "M.REGISTRY"},
                {"kind": "assignment", "name": "M.UNKNOWN_ICON"},
            ],
        },
    ],
    "yaml_sources": [
        # These reach the identical global index: Infection.hs publishes
        # `infIcon` to Lua, Combat.hs surfaces it as a wound's
        # `infectionIcon`, and injuries.lua's M.infectionList forwards it
        # into an infection row.
        {"dir": "data/infections", "key": "icon"},
    ],
    # Bare names the ENGINE publishes into the same Lua `icon` field without
    # passing through any Lua map. Scope is every .hs under these roots that
    # names the field, so a new publication site fails loudly.
    "haskell_roots": ["src", "app"],
    "haskell_field": "icon",
    "haskell_sites": [
        # Lua.pushstring (TE.encodeUtf8 "immunity")
        # Lua.setfield (-2) "icon"
        # -- unit.getImmunities' rows, rendered by
        # unit_info_v2_status.lua's immunity section.
        {"file": COMBAT, "name": "the immunity row's pushed `icon`",
         "pattern": r'pushstring\s*\([^\n"]*"(?P<name>[A-Za-z0-9_]+)"\s*\)'
                    r'\s*\n\s*Lua\.setfield\s*\(-2\)\s*"icon"'},
        # v .:? "icon" .!= "bacterial_infection" -- the basename an infection
        # def that declares no `icon:` silently gets, so it is authoritative
        # even though no YAML file spells it.
        {"file": YAML_INFECTION, "name": "the `icon:` decoder default",
         "pattern": r'"icon"\s*\.!=\s*"(?P<name>[A-Za-z0-9_]+)"'},
    ],
    "haskell_forwarding_allowlist": [
        {"file": INFECTION_API, "pattern": r'putS\s+"icon"\s+\(infIcon d\)',
         "reason": "forwards infIcon, extracted from data/infections/*.yaml "
                   "and from YamlInfection.hs's decoder default"},
    ],
    # Live `icon` assignments that FORWARD an already-extracted value rather
    # than naming a new basename. Keyed on the assignment text rather than a
    # line number so ordinary edits above them do not invalidate the list;
    # each entry must still match at least once.
    "forwarding_allowlist": [
        {"file": INJURIES, "target": "rowIcon", "rhs": "M.icon(w.kind, w.part)",
         "reason": "forwards M.icon, whose own literals are extracted"},
        {"file": INJURIES, "target": "icon", "rhs": "rowIcon",
         "reason": "forwards the row's rowIcon, extracted in M.list"},
        {"file": INJURIES, "target": "icon", "rhs": "w.infectionIcon",
         "reason": "forwards the engine-supplied infection icon, extracted "
                   "from data/infections/*.yaml"},
        {"file": INJURIES, "target": "icon", "rhs": "icon",
         "reason": "forwards the local `icon` chosen just above"},
        {"file": STATUS, "target": "icon", "rhs": "mc.icon",
         "reason": "forwards METER_CONDITIONS' literal, extracted in this file"},
        {"file": STATUS, "target": "icon", "rhs": "inj.icon",
         "reason": "forwards injuries.list's icon, extracted in injuries.lua"},
    ],
    # Intentional cross-family reuse. `row_family` is the fallback family the
    # row passes to buildIconStatPanel; `family` is the family that actually
    # supplies the asset. They must differ, or the global lookup has been
    # quietly reinterpreted as family-local.
    #
    # `site` + `rows` bind each pin to the EXACT references it describes. That
    # binding is load-bearing: `agility` and `strength` are each used both by
    # their own physical-stat row and by a SKILL row in the same file, and
    # `pain` is used by a Status row as well as by M.icon's injury-row last
    # resort. A pin satisfied by "the basename appears somewhere in the file"
    # would keep asserting a cross-family use after that use was deleted.
    "cross_family_pins": [
        {"basename": "agility", "family": "stat", "row_family": "skill",
         "site": STAT_DEFS, "rows": ["dodge", "jumping"],
         "reason": "the Dodge and Jumping SKILL rows draw the STAT-family "
                   "agility icon"},
        {"basename": "strength", "family": "stat", "row_family": "skill",
         "site": STAT_DEFS, "rows": ["grappling"],
         "reason": "the Grappling SKILL row draws the STAT-family strength icon"},
        {"basename": "weight", "family": "stat", "row_family": "status",
         "site": STAT_DEFS, "rows": ["carrying_capacity"],
         "reason": "the Status panel's Carry Load row draws the STAT-family "
                   "weight icon"},
        {"basename": "pain", "family": "status", "row_family": "injury",
         "site": f"{INJURIES} M.icon", "rows": [None],
         "reason": "M.icon's last-resort gives INJURY-kind rows the "
                   "STATUS-family pain icon"},
        {"basename": "nerve_injury", "family": "injury", "row_family": "status",
         "site": STATUS, "rows": ["neuro"],
         "reason": "the Brain-failing STATUS condition row draws an "
                   "INJURY-family icon"},
        {"basename": "festered_injury", "family": "injury",
         "row_family": "status", "site": STATUS, "rows": ["organ", "sepsis"],
         "reason": "the Organ-failure and Septic STATUS condition rows draw an "
                   "INJURY-family icon"},
        {"basename": "frostbite", "family": "injury", "row_family": "status",
         "site": STATUS, "rows": ["hypothermia", "hyperthermia"],
         "reason": "the Hypothermic and Overheating STATUS condition rows draw "
                   "an INJURY-family icon"},
    ],
}
