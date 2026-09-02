"""Lua parsing and extraction for the bare-name icon gate (#1740, split by
#2142 requirement 5).

The ONE owner of everything that reads Lua source: short-string handling,
line and long-comment cleaning, long-string rejection, balanced assignment
scanning, table/function/anchor region location, enumerated value-table
extraction, literal `icon` field extraction, computed-value rejection,
out-of-site publication rejection and forwarding-allowlist accounting.

Fail closed, never narrow (requirement 6): an unsupported table shape, a
computed value outside the allowlist, an unterminated string or long
comment, a missing region, a zero-match site or anchor, and a literal
outside every enumerated site are each a `CheckError` naming `file:line`
(or the expected table), never a quietly smaller reference set.

Forwarding-allowlist HITS are recorded in the `allow_hits` dict the audit
owner passes in and validated for staleness there, after BOTH this
extractor and the Haskell one have run — this module never judges an
entry stale on its own.

Consumes only the shared leaf (`bare_name_icon_asset_core`). The runtime
inventory owner imports `clean_lua` from here rather than carrying a
second lexer (requirement 20).
"""
from __future__ import annotations

import re
from pathlib import Path

from bare_name_icon_asset_core import CheckError, LineMap, Reference, blank_span


# --------------------------------------------------------------------------
# Lua lexing
# --------------------------------------------------------------------------

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
                out.append(blank_span(span))
                line += span.count("\n")
                i = end + len(close)
                continue
            end = text.find("\n", i)
            if end < 0:
                end = n
            out.append(blank_span(text[i:end]))
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
            out.append(blank_span(span))
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


OPEN, CLOSE = "([{", ")]}"
BOUNDARY_KEYWORDS = re.compile(
    r"\b(?:then|do|else|elseif|repeat|until|return|end|function|while|if|for|in)\b")
IDENT_LIST = re.compile(r"^[A-Za-z_][\w.]*(?:\s*,\s*[A-Za-z_][\w.]*)*$")
LEADING_STRING = re.compile(r"^\s*(?P<q>[\"'])(?P<val>(?:[^\"'\\]|\\.)*)(?P=q)")
# What may legally trail a literal RHS before the statement really ends.
TRAILING_NOISE = re.compile(r"^[\s,;)}\]]*(?:\b(?:end|then|do|else|elseif)\b[\s;]*)*$")


class Assignment:
    __slots__ = ("line", "targets", "index", "rhs", "depth", "start",
                 "owner", "scope")

    def __init__(self, line, targets, index, rhs, depth, start, owner, scope):
        self.line = line
        self.targets = targets
        self.index = index
        self.rhs = rhs
        self.depth = depth
        self.start = start
        #: The table key whose constructor immediately encloses this
        #: assignment (`dodge` for `dodge = { icon = "agility" }`), or None
        #: for an anonymous entry or file scope.
        self.owner = owner
        #: Index of the innermost open bracket, so entries of the SAME table
        #: entry can be found as siblings (-1 at file scope).
        self.scope = scope

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
    owners = [None]          # owners[d] = the table key owning scope depth d
    scopes = []              # indices of the currently open brackets
    pending = {}             # depth -> (target, index just past its `=`)
    i = 0
    while i < n:
        ch = cleaned[i]
        if ch in "\"'":
            i = _skip_string(cleaned, i)
            continue
        if ch in OPEN:
            # A table constructor written as the right-hand side of `key = {`
            # makes `key` the owner of everything one level deeper; anything
            # else (a call's parens, an index) owns nothing.
            owner = None
            if ch == "{":
                recent = pending.get(depth)
                if recent and recent[1] <= i and not cleaned[recent[1]:i].strip():
                    owner = recent[0]
            owners.append(owner)
            scopes.append(i)
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
            if len(owners) > 1:
                owners.pop()
            if scopes:
                scopes.pop()
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
            owner = owners[depth] if depth < len(owners) else None
            scope = scopes[-1] if scopes else -1
            for idx in range(len(targets)):
                piece = pieces[idx] if idx < len(pieces) else ""
                found.append(Assignment(line, targets, idx, piece, depth, i,
                                        owner, scope))
            if len(targets) == 1:
                pending[depth] = (targets[0], rhs_start)
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

    row_key_fields = spec.get("row_key_fields", ["stat", "id", "name"])
    by_scope = {}
    for assignment in assignments:
        by_scope.setdefault(assignment.scope, []).append(assignment)

    def row_of(assignment):
        """Which row of the site this assignment belongs to.

        The enclosing table key when there is one (`dodge` in
        `dodge = { icon = "agility" }`); otherwise the entry's own
        identifying literal field, which is how an ANONYMOUS table entry
        (`{ stat = "neuro", icon = "nerve_injury" }`) names itself.
        """
        if assignment.owner is not None:
            return assignment.owner
        if assignment.scope < 0:
            return None
        siblings = by_scope.get(assignment.scope, ())
        for field in row_key_fields:
            for sibling in siblings:
                if sibling.target != field:
                    continue
                value = literal_of(sibling.rhs)
                if value:
                    return value
        return None

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
                references.append(Reference(value, label, assignment.line,
                                            site, assignment.target))
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
                references.append(Reference(value, label, assignment.line,
                                            site, row_of(assignment)))
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
