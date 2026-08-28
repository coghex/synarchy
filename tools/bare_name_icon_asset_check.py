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

Intentional cross-family reuse is PINNED (see `CROSS_FAMILY_PINS`): each
pin names the reference source, the row's own fallback family and the
family that actually supplies the asset, and requires the two to DIFFER.
Moving one of those assets into the row's own family, or reinterpreting
the lookup as family-local, therefore fails this check or its self-test
instead of quietly changing meaning.

Fail loudly, never narrow
-------------------------
The real hazard for a checker like this is an input shape its extractor
does not recognise: a missed reference reads as coverage that is not
there. So extraction refuses rather than shrugs. Each of these is an
error naming `file:line` (or the expected table) and never a quietly
smaller reference set:

  * an unsupported table shape inside an enumerated value table;
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

Deliberately NOT covered: the skill panel derives a basename from the
live skill name (`scripts/unit_info_v2_panels.lua` with
`panel_engine.lua`'s `def and def.icon or statKey`), which is genuinely
dynamic and outside any static extractor's reach; and
`assets/textures/icons/location/`, which is absent from `ICON_SUBDIRS`
and owned by `tools/location_map_icon_asset_check.py`.

Usage:
  python3 tools/bare_name_icon_asset_check.py
  python3 tools/bare_name_icon_asset_check.py --self-test
Exit codes: 0 = every authoritative reference resolves and every pinned
contract holds, 1 = it does not (or extraction refused, or a self-test
check failed).
"""
from __future__ import annotations

import argparse
import bisect
import re
import sys
import tempfile
from pathlib import Path

try:
    import yaml  # type: ignore
except ImportError:  # pragma: no cover - exercised only on a bare toolchain
    raise SystemExit(
        "bare_name_icon_asset_check.py needs PyYAML to read "
        "data/infections/*.yaml.\n"
        "Install the pinned toolchain:\n"
        "    python3 -m pip install --user -r tools/requirements-assets.txt\n"
        "(PyYAML is already required by tools/pack_atlas.py and "
        "tools/ci_parity_audit.py, which `make ci` and CI both run, so this "
        "adds no new dependency.)")

REPO_ROOT = Path(__file__).resolve().parent.parent
ICON_ROOT = "assets/textures/icons"


class CheckError(Exception):
    """A loud extraction failure: the check refuses rather than narrows."""


# --------------------------------------------------------------------------
# Lua lexing
# --------------------------------------------------------------------------

def _blank(span: str) -> str:
    return "".join("\n" if ch == "\n" else " " for ch in span)


def _long_bracket(text: str, i: int):
    """Return (level, index-after-open) if a Lua long bracket opens at i."""
    if i >= len(text) or text[i] != "[":
        return None
    j = i + 1
    level = 0
    while j < len(text) and text[j] == "=":
        level += 1
        j += 1
    if j < len(text) and text[j] == "[":
        return level, j + 1
    return None


def clean_lua(text: str, label: str) -> str:
    """Blank comments and long strings; keep short string literals verbatim.

    The result is the same length as the input (so indices and line numbers
    are shared with the original), which lets every later scan treat a
    quoted literal as a literal and everything else as code.
    """
    out = []
    i, n, line = 0, len(text), 1
    while i < n:
        ch = text[i]
        if ch == "\n":
            out.append("\n")
            line += 1
            i += 1
            continue
        if ch == "-" and text.startswith("--", i):
            opened = _long_bracket(text, i + 2)
            if opened:
                level, body = opened
                close = "]" + "=" * level + "]"
                end = text.find(close, body)
                if end < 0:
                    raise CheckError(
                        f"{label}:{line}: unterminated long comment")
                span = text[i:end + len(close)]
                out.append(_blank(span))
                line += span.count("\n")
                i = end + len(close)
                continue
            end = text.find("\n", i)
            if end < 0:
                end = n
            out.append(_blank(text[i:end]))
            i = end
            continue
        opened = _long_bracket(text, i)
        if opened:
            level, body = opened
            close = "]" + "=" * level + "]"
            end = text.find(close, body)
            if end < 0:
                raise CheckError(f"{label}:{line}: unterminated long string")
            span = text[i:end + len(close)]
            out.append(_blank(span))
            line += span.count("\n")
            i = end + len(close)
            continue
        if ch in "\"'":
            j = i + 1
            while j < n and text[j] != ch:
                if text[j] == "\\":
                    j += 2
                    continue
                if text[j] == "\n":
                    raise CheckError(
                        f"{label}:{line}: unterminated string literal")
                j += 1
            if j >= n:
                raise CheckError(f"{label}:{line}: unterminated string literal")
            out.append(text[i:j + 1])
            i = j + 1
            continue
        out.append(ch)
        i += 1
    return "".join(out)


def _skip_string(text: str, i: int) -> int:
    """If a short string starts at i, return the index just past it."""
    if i >= len(text) or text[i] not in "\"'":
        return i
    quote = text[i]
    j = i + 1
    while j < len(text) and text[j] != quote:
        j += 2 if text[j] == "\\" else 1
    return min(j + 1, len(text))


class LineMap:
    def __init__(self, text: str) -> None:
        self.starts = [0]
        for idx, ch in enumerate(text):
            if ch == "\n":
                self.starts.append(idx + 1)

    def line_of(self, index: int) -> int:
        return bisect.bisect_right(self.starts, index)


OPEN, CLOSE = "([{", ")]}"
BOUNDARY_KEYWORDS = re.compile(
    r"\b(?:then|do|else|elseif|repeat|until|return|end|function|while|if|for|in)\b")
IDENT_LIST = re.compile(r"^[A-Za-z_][\w.]*(?:\s*,\s*[A-Za-z_][\w.]*)*$")
LEADING_STRING = re.compile(r"^\s*(?P<q>[\"'])(?P<val>(?:[^\"'\\]|\\.)*)(?P=q)")
# What may legally trail a literal RHS before the statement really ends.
TRAILING_NOISE = re.compile(r"^[\s,;)}\]]*(?:\b(?:end|then|do|else|elseif)\b[\s;]*)*$")


class Assignment:
    __slots__ = ("line", "targets", "index", "rhs", "depth", "start")

    def __init__(self, line, targets, index, rhs, depth, start):
        self.line = line
        self.targets = targets
        self.index = index
        self.rhs = rhs
        self.depth = depth
        self.start = start

    @property
    def target(self) -> str:
        return self.targets[self.index]


def _split_top_level(text: str) -> list:
    """Split on commas that are not inside brackets or string literals."""
    parts, depth, buf, i = [], 0, [], 0
    while i < len(text):
        ch = text[i]
        if ch in "\"'":
            end = _skip_string(text, i)
            buf.append(text[i:end])
            i = end
            continue
        if ch in OPEN:
            depth += 1
        elif ch in CLOSE:
            depth -= 1
        elif ch == "," and depth == 0:
            parts.append("".join(buf))
            buf = []
            i += 1
            continue
        buf.append(ch)
        i += 1
    parts.append("".join(buf))
    return parts


def scan_assignments(cleaned: str, lines: LineMap) -> list:
    """Every simple assignment in the cleaned source, with its brace depth.

    Depth matters: at depth 0 a comma separates multi-assignment TARGETS
    (`icon, tag = "rot_injury", " - gangrenous"`), while inside a table
    constructor it separates FIELDS (`{ name = "Dead", icon = "death" }`).
    """
    found = []
    n = len(cleaned)
    depth = 0
    field_start = {0: 0}
    i = 0
    while i < n:
        ch = cleaned[i]
        if ch in "\"'":
            i = _skip_string(cleaned, i)
            continue
        if ch in OPEN:
            depth += 1
            field_start[depth] = i + 1
            i += 1
            continue
        if ch in CLOSE:
            # Deliberately does NOT reset field_start at the reopened depth: a
            # bracketed table key (`["fracture|foot"] = "joint_injury"`) is part
            # of the left-hand side, so the field must still start where the
            # enclosing `{` or `,` put it.
            depth = max(0, depth - 1)
            i += 1
            continue
        if ch in "\n;" or (ch == "," and depth > 0):
            field_start[depth] = i + 1
            i += 1
            continue
        if ch == "=":
            if cleaned[i + 1:i + 2] == "=" or cleaned[i - 1:i] in ("=", "<", ">", "~"):
                i += 2 if cleaned[i + 1:i + 2] == "=" else 1
                continue
            lhs_raw = cleaned[field_start.get(depth, 0):i]
            # A boundary keyword ends the previous statement: keep only what
            # follows it ("... then icon = " -> "icon ").
            keywords = list(BOUNDARY_KEYWORDS.finditer(lhs_raw))
            if keywords:
                lhs_raw = lhs_raw[keywords[-1].end():]
            lhs = re.sub(r"^\s*local\s+", "", lhs_raw).strip()
            # A bracketed table key (`["fracture|foot"] = "joint_injury"`) is a
            # single target and never a multi-assignment list.
            bracket_key = lhs.startswith("[") and lhs.endswith("]")
            if not lhs or not (bracket_key or IDENT_LIST.match(lhs)):
                i += 1
                continue
            targets = [lhs] if bracket_key else [t.strip() for t in lhs.split(",")]
            rhs_start = i + 1
            j, d = rhs_start, 0
            while j < n:
                cj = cleaned[j]
                if cj in "\"'":
                    j = _skip_string(cleaned, j)
                    continue
                if cj in OPEN:
                    d += 1
                elif cj in CLOSE:
                    if d == 0:
                        break
                    d -= 1
                elif cj in "\n;" and d == 0:
                    break
                elif cj == "," and d == 0 and depth > 0:
                    break
                j += 1
            rhs_all = cleaned[rhs_start:j]
            pieces = _split_top_level(rhs_all) if (depth == 0 and len(targets) > 1) \
                else [rhs_all]
            line = lines.line_of(i)
            for idx in range(len(targets)):
                piece = pieces[idx] if idx < len(pieces) else ""
                found.append(Assignment(line, targets, idx, piece, depth, i))
            # Resume INSIDE the right-hand side rather than past it: a table
            # constructor is itself full of field assignments, and skipping to
            # `j` would swallow every entry of `local KIND_ICON = { ... }`.
            i = rhs_start
            field_start[depth] = rhs_start
            continue
        i += 1
    return found


def literal_of(rhs: str):
    """The bare-name string an RHS denotes, or None when it is computed."""
    match = LEADING_STRING.match(rhs)
    if not match:
        return None
    if not TRAILING_NOISE.match(rhs[match.end():]):
        return None
    return match.group("val")


# --------------------------------------------------------------------------
# Region location
# --------------------------------------------------------------------------

def _brace_match(cleaned: str, open_index: int, label: str, line: int) -> int:
    depth, i = 0, open_index
    while i < len(cleaned):
        ch = cleaned[i]
        if ch in "\"'":
            i = _skip_string(cleaned, i)
            continue
        if ch == "{":
            depth += 1
        elif ch == "}":
            depth -= 1
            if depth == 0:
                return i + 1
        i += 1
    raise CheckError(f"{label}:{line}: table literal is never closed")


def locate_region(cleaned: str, lines: LineMap, label: str, region: dict):
    kind, name = region["kind"], region["name"]
    if kind == "file":
        return 0, len(cleaned)
    if kind in ("value_table", "table"):
        pattern = re.compile(
            r"(?:^|\n)[ \t]*(?:local[ \t]+)?" + re.escape(name) + r"[ \t]*=[ \t]*\{")
        match = pattern.search(cleaned)
        if not match:
            raise CheckError(
                f"{label}: expected table `{name}` was not found; the "
                f"extractor refuses rather than skipping a reference site")
        brace = cleaned.index("{", match.start())
        return match.start(), _brace_match(cleaned, brace, label,
                                           lines.line_of(brace))
    if kind == "function":
        pattern = re.compile(
            r"(?:^|\n)(?P<indent>[ \t]*)(?:local[ \t]+)?function[ \t]+"
            + re.escape(name) + r"[ \t]*\(")
        match = pattern.search(cleaned)
        if not match:
            raise CheckError(
                f"{label}: expected function `{name}` was not found; the "
                f"extractor refuses rather than skipping a reference site")
        indent = match.group("indent")
        start = match.start() + (1 if cleaned[match.start()] == "\n" else 0)
        terminator = re.compile(r"\n" + re.escape(indent) + r"end[ \t]*(?=\n|$)")
        closing = terminator.search(cleaned, match.end())
        if not closing:
            raise CheckError(
                f"{label}:{lines.line_of(match.start())}: function `{name}` "
                f"has no `end` at its own indentation; this extractor only "
                f"supports functions closed that way")
        return start, closing.end()
    if kind == "assignment":
        pattern = re.compile(
            r"(?:^|\n)[ \t]*(?:local[ \t]+)?" + re.escape(name) + r"[ \t]*=[^=]")
        match = pattern.search(cleaned)
        if not match:
            raise CheckError(
                f"{label}: expected assignment `{name}` was not found; the "
                f"extractor refuses rather than skipping a reference site")
        end = cleaned.find("\n", match.end())
        return match.start(), len(cleaned) if end < 0 else end
    raise CheckError(f"{label}: unknown region kind {kind!r}")


# --------------------------------------------------------------------------
# Reference extraction
# --------------------------------------------------------------------------

class Reference:
    __slots__ = ("basename", "source", "line", "site")

    def __init__(self, basename, source, line, site):
        self.basename = basename
        self.source = source
        self.line = line
        self.site = site

    def where(self) -> str:
        return f"{self.source}:{self.line} ({self.site})"


def _matches_target(lhs: str, target: str) -> bool:
    return lhs == target or lhs.split(".")[-1] == target.split(".")[-1]


def extract_lua(root: Path, spec: dict, allow_hits: dict) -> list:
    label = spec["path"]
    path = root / label
    if not path.is_file():
        raise CheckError(f"{label}: expected authoritative source is missing")
    cleaned = clean_lua(path.read_text(encoding="utf-8"), label)
    lines = LineMap(cleaned)
    assignments = scan_assignments(cleaned, lines)
    targets = spec.get("targets", ["icon"])

    regions = []
    for region in spec["regions"]:
        start, end = locate_region(cleaned, lines, label, region)
        regions.append((start, end, region))

    references = []
    covered = set()
    for start, end, region in regions:
        kind = region["kind"]
        site = f"{label} {region['name']}" if kind != "file" else label
        produced = 0
        inside = [a for a in assignments if start <= a.start < end]
        if kind == "value_table":
            body = cleaned[start:end]
            body_start_line = lines.line_of(start)
            for offset, raw in enumerate(body.split("\n")):
                stripped = raw.strip()
                if not stripped or stripped in ("{", "}", "},"):
                    continue
                if stripped.endswith("{") or stripped.startswith("}"):
                    continue
                if not re.match(r"^(?:\[[^\]]*\]|[A-Za-z_]\w*)\s*=\s*", stripped):
                    raise CheckError(
                        f"{label}:{body_start_line + offset}: unsupported table "
                        f"shape in `{region['name']}` — every entry must be a "
                        f"`key = \"basename\"` literal, got: {stripped}")
            for assignment in inside:
                if assignment.depth == 0:
                    continue
                covered.add(assignment.start)
                value = literal_of(assignment.rhs)
                if value is None:
                    raise CheckError(
                        f"{label}:{assignment.line}: `{region['name']}` entry "
                        f"`{assignment.target}` has a computed value "
                        f"({assignment.rhs.strip()!r}); this table must hold "
                        f"literal icon basenames only")
                references.append(Reference(value, label, assignment.line, site))
                produced += 1
        else:
            for assignment in inside:
                if not any(_matches_target(assignment.target, t) for t in targets):
                    continue
                covered.add(assignment.start)
                value = literal_of(assignment.rhs)
                if value is None:
                    key = (label, assignment.target.split(".")[-1],
                           assignment.rhs.strip())
                    if key not in allow_hits:
                        raise CheckError(
                            f"{label}:{assignment.line}: icon assignment "
                            f"`{assignment.target} = {assignment.rhs.strip()}` "
                            f"is computed rather than literal and is not in the "
                            f"forwarding allowlist; add a reason-carrying entry "
                            f"or make it a literal basename")
                    allow_hits[key] += 1
                    continue
                references.append(Reference(value, label, assignment.line, site))
                produced += 1
        for anchor in region.get("anchors", []):
            hits = 0
            body_start_line = lines.line_of(start)
            for offset, raw in enumerate(cleaned[start:end].split("\n")):
                match = re.match(anchor["pattern"], raw)
                if match:
                    references.append(
                        Reference(match.group("name"), label,
                                  body_start_line + offset, site))
                    hits += 1
                    produced += 1
            if hits == 0:
                raise CheckError(
                    f"{label}: anchor {anchor['name']!r} in `{region['name']}` "
                    f"matched nothing; the extractor refuses rather than "
                    f"silently dropping a reference site")
        if produced == 0:
            raise CheckError(
                f"{label}: reference site `{region['name']}` produced zero "
                f"references; the extractor refuses rather than silently "
                f"narrowing coverage")

    if not spec.get("whole_file_is_a_site", False):
        for assignment in assignments:
            if assignment.start in covered:
                continue
            if not any(_matches_target(assignment.target, t) for t in targets):
                continue
            raise CheckError(
                f"{label}:{assignment.line}: icon assignment "
                f"`{assignment.target} = {assignment.rhs.strip()}` lies OUTSIDE "
                f"every enumerated reference site "
                f"({', '.join(r['name'] for r in spec['regions'])}); enumerate "
                f"its site or allowlist it with a reason")
    return references


def extract_yaml(root: Path, spec: dict) -> list:
    directory = root / spec["dir"]
    if not directory.is_dir():
        raise CheckError(
            f"{spec['dir']}: expected authoritative YAML directory is missing")
    files = sorted(p for p in directory.iterdir() if p.suffix in (".yaml", ".yml"))
    if not files:
        raise CheckError(
            f"{spec['dir']}: expected authoritative YAML source produced no "
            f"files; the extractor refuses rather than silently narrowing")
    references = []
    for path in files:
        label = str(path.relative_to(root))
        try:
            document = yaml.safe_load(path.read_text(encoding="utf-8"))
        except yaml.YAMLError as error:
            raise CheckError(f"{label}: could not be parsed as YAML ({error})")
        found = []

        def walk(node):
            if isinstance(node, dict):
                for key, value in node.items():
                    if key == spec["key"]:
                        if not isinstance(value, str) or not value.strip():
                            raise CheckError(
                                f"{label}: `{spec['key']}:` must be a non-empty "
                                f"string basename, got {value!r}")
                        found.append(value.strip())
                    else:
                        walk(value)
            elif isinstance(node, list):
                for item in node:
                    walk(item)

        walk(document)
        for value in found:
            references.append(Reference(value, label, 0, f"{label} `{spec['key']}:`"))
    if not references:
        raise CheckError(
            f"{spec['dir']}: no `{spec['key']}:` scalars found; the extractor "
            f"refuses rather than silently narrowing coverage")
    return references


# --------------------------------------------------------------------------
# Family inventories and the asset index
# --------------------------------------------------------------------------

def _literal_list(body: str, label: str, what: str) -> list:
    names = re.findall(r"\"([^\"]*)\"|'([^']*)'", body)
    values = [a or b for a, b in names]
    residue = re.sub(r"\"[^\"]*\"|'[^']*'|[\s,]", "", body)
    if residue:
        raise CheckError(
            f"{label}: {what} is not a plain list of string literals "
            f"(unexpected {residue!r}); the extractor refuses rather than "
            f"guessing the family inventory")
    if not values:
        raise CheckError(f"{label}: {what} is empty")
    return values


def panel_families(root: Path, spec: dict) -> list:
    label = spec["path"]
    path = root / label
    if not path.is_file():
        raise CheckError(f"{label}: expected family inventory source is missing")
    cleaned = clean_lua(path.read_text(encoding="utf-8"), label)
    match = re.search(
        r"(?:local\s+)?" + re.escape(spec["name"]) + r"\s*=\s*\{(?P<body>[^}]*)\}",
        cleaned)
    if not match:
        raise CheckError(f"{label}: `{spec['name']}` was not found")
    return _literal_list(match.group("body"), label, f"`{spec['name']}`")


def loader_families(root: Path, spec: dict) -> list:
    label = spec["path"]
    path = root / label
    if not path.is_file():
        raise CheckError(f"{label}: expected family inventory source is missing")
    cleaned = clean_lua(path.read_text(encoding="utf-8"), label)
    calls = [m.start() for m in re.finditer(re.escape(spec["anchor"]), cleaned)]
    if len(calls) != 1:
        raise CheckError(
            f"{label}: expected exactly one `{spec['anchor']}` call, found "
            f"{len(calls)}; the extractor refuses rather than guessing which "
            f"one carries the icon family inventory")
    preceding = list(re.finditer(r"ipairs\(\s*\{(?P<body>[^}]*)\}\s*\)",
                                 cleaned[:calls[0]]))
    if not preceding:
        raise CheckError(
            f"{label}: no `ipairs({{...}})` family list precedes "
            f"`{spec['anchor']}`")
    return _literal_list(preceding[-1].group("body"), label,
                         "the icon preload family list")


def build_index(root: Path, families: list, restrict_to=None):
    """basename -> supplying family, mirroring buildIconIndex's last-wins order."""
    index, duplicates, missing_dirs = {}, [], []
    order = families if restrict_to is None else [restrict_to]
    for family in order:
        directory = root / ICON_ROOT / family
        if not directory.is_dir():
            missing_dirs.append(family)
            continue
        for entry in sorted(directory.iterdir()):
            if entry.suffix != ".png":
                continue
            basename = entry.stem
            if basename in index and index[basename] != family:
                duplicates.append((basename, index[basename], family))
            index[basename] = family
    return index, duplicates, missing_dirs


# --------------------------------------------------------------------------
# The check itself
# --------------------------------------------------------------------------

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

    references = []
    for spec in config["lua_sources"]:
        references.extend(extract_lua(root, spec, allow_hits))
    for spec in config.get("yaml_sources", []):
        references.extend(extract_yaml(root, spec))

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

    write(f"Extracted {len(references)} authoritative bare-name references "
          f"from {len(config['lua_sources']) + len(config.get('yaml_sources', []))} "
          f"sources\n")

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
        if not any(r.basename == basename for r in references):
            failures.append(
                f"cross-family pin {basename!r} matches no extracted reference; "
                f"the pin or the reference set is stale ({pin['reason']})")
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
            write(f"  cross-family: {pin['source']} uses {basename!r} on a "
                  f"{row!r}-fallback row; supplied by family {actual!r}\n")

    if failures:
        write(f"\nFAIL — {len(failures)} problem(s):\n")
        for failure in failures:
            write(f"  - {failure}\n")
        return 1
    write("\nOK — every authoritative bare-name icon reference resolves "
          "through the global index.\n")
    return 0


# --------------------------------------------------------------------------
# The repository's own configuration
# --------------------------------------------------------------------------

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
    "cross_family_pins": [
        {"basename": "agility", "family": "stat", "row_family": "skill",
         "source": STAT_DEFS,
         "reason": "the Dodge and Jumping SKILL rows draw the STAT-family "
                   "agility icon"},
        {"basename": "strength", "family": "stat", "row_family": "skill",
         "source": STAT_DEFS,
         "reason": "the Grappling SKILL row draws the STAT-family strength icon"},
        {"basename": "weight", "family": "stat", "row_family": "status",
         "source": STAT_DEFS,
         "reason": "the Status panel's Carry Load row draws the STAT-family "
                   "weight icon"},
        {"basename": "pain", "family": "status", "row_family": "injury",
         "source": INJURIES,
         "reason": "M.icon's last-resort gives INJURY-kind rows the "
                   "STATUS-family pain icon"},
        {"basename": "nerve_injury", "family": "injury", "row_family": "status",
         "source": STATUS,
         "reason": "the Brain-failing STATUS condition row draws an "
                   "INJURY-family icon"},
        {"basename": "festered_injury", "family": "injury",
         "row_family": "status", "source": STATUS,
         "reason": "the Organ-failure and Septic STATUS condition rows draw an "
                   "INJURY-family icon"},
        {"basename": "frostbite", "family": "injury", "row_family": "status",
         "source": STATUS,
         "reason": "the Hypothermic and Overheating STATUS condition rows draw "
                   "an INJURY-family icon"},
    ],
}


# --------------------------------------------------------------------------
# Self-test
# --------------------------------------------------------------------------

FIXTURE_FAMILIES = ("stat", "skill", "status")
FIXTURE_ASSETS = {
    "stat": ["stat_unknown", "agility"],
    "skill": ["skill_unknown"],
    "status": ["status_unknown", "pain", "broken_bone", "joint_injury",
               "rot_injury", "scar", "know_a", "bacterial_infection"],
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
    agility = { icon = "agility", name = "Agility" },
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
        "forwarding_allowlist": [
            {"file": "scripts/inj.lua", "target": "rowIcon",
             "rhs": "M.icon(\"blunt\", \"hand\")",
             "reason": "forwards M.icon, whose literals are extracted"},
            {"file": "scripts/inj.lua", "target": "icon", "rhs": "rowIcon",
             "reason": "forwards the row's rowIcon"},
        ],
        "cross_family_pins": [
            {"basename": "agility", "family": "stat", "row_family": "skill",
             "source": "scripts/defs.lua",
             "reason": "fixture: a skill-fallback row drawing a stat asset"},
        ],
    }


def build_fixture(base: Path) -> Path:
    root = base
    (root / "scripts").mkdir(parents=True, exist_ok=True)
    (root / "data" / "inf").mkdir(parents=True, exist_ok=True)
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
    case("a cross-family reference names its real supplying family",
         lambda r, c: None, 0,
         "scripts/defs.lua uses 'agility' on a 'skill'-fallback row; "
         "supplied by family 'stat'")
    case("a family-local move of a pinned asset is refused",
         lambda r, c: (_drop_asset(r, "stat", "agility"),
                       (r / ICON_ROOT / "skill" / "agility.png").write_bytes(b"")),
         1, "no longer cross-family")
    case("a pin naming the row's own family as its supplier is refused",
         lambda r, c: c["cross_family_pins"][0].update(family="skill"), 2,
         "pins nothing")

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
