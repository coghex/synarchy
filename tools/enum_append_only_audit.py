#!/usr/bin/env python3
"""Append-only audit for the positionally-serialized enums (issue #1145).

A sum type whose `Serialize` instance is DERIVED THROUGH `Generic` is
encoded by cereal as a constructor INDEX followed by that constructor's
fields. The index is positional, so inserting, reordering, removing, or
renaming a constructor silently reinterprets every value already written
to disk: `Climbing` decodes as `Falling`, `DirNE` as `DirE`. Nothing
fails to compile and no ordinary test notices — which makes it the
highest-consequence silent-corruption rule in the project.

That rule was PARTIALLY enforced before this audit, not unenforced. The
manifest-driven compat gate (`Test.Headless.World.Save.Compat`) decodes
tracked binary fixtures and compares against canonical summaries that
record enum values BY NAME, so a reorder touching a constructor some
fixture happens to carry already failed. The gap was which constructors
the fixtures happen to carry: measured across every `*.expected.json`,
only `Standing`, `Idle`, `Walking`, `DirS`, `FixedCount` and
`PowerStorage` were covered, leaving a reorder confined to any other
constructor invisible. Extending the fixtures instead was considered and
rejected in the issue: they are ~300 KB binaries regenerated through a
real engine boot, one state per constructor would be costly, and they
still would not catch a RENAME. A golden constructor list is cheap and
catches reorder, insert, remove, and rename uniformly.

=== How the guarded set is determined (requirement 2)

A type is GUARDED iff all three hold:

  1. It is a `data` declaration under `src/` or `app/` (the shipped
     library + executable). A `newtype` cannot be a sum, and a `deriving
     newtype (Serialize)` instance delegates to the wrapped type rather
     than emitting a constructor tag at all.
  2. Its own deriving clauses name BOTH `Generic` (under any strategy)
     and `Serialize` under a strategy that goes through `Generic` — i.e.
     `stock`, `anyclass`, or unstrategied. A `deriving newtype
     (Serialize)` clause does NOT qualify it.
  3. It declares TWO OR MORE constructors, so cereal actually emits a
     positional constructor tag. A single-constructor record emits no
     tag; ITS hazard is field-order drift, which is the frozen-DTO
     boundary rule's and `tools/save_compat_audit.py`'s job, not this
     audit's.

This is deliberately a documented SUPERSET of "reachable from a
currently emitted component DTO or a still-accepted historical DTO" —
the option this issue's review explicitly permits. The superset is the
safer rule and, measured against this tree, barely larger: 29 of the 34
guarded types are genuinely reachable from a save-wire DTO today, and
the other 5 hold a `Generic`-derived `Serialize` instance precisely
because someone intended to serialize them. Guarding by the property
that CREATES the hazard, rather than by a reachability walk that must be
kept correct, is what makes requirement 2's "an enum that becomes
persisted later is not silently omitted" hold structurally: a type that
becomes persisted was already guarded the day its instance was derived.

Reachability is still computed, but only so the DIAGNOSTIC can name
every affected component and historical shape (see
`compute_wire_carriers`) — never to decide what is guarded. It is a
conservative over-approximation (declarations are keyed by bare type
name with their references unioned, so the same-named type pairs in this
tree over-reach rather than under-reach), which is the direction that
keeps a diagnostic from quietly missing an affected component.

=== What the baseline records, and why an append must ratchet it

`docs/save_compat/enum_baseline.json` records, per module-qualified type
identity, the ordered constructor list with each constructor's arity.
Arity is part of the record because a constructor that keeps its name
and index but gains or loses a field changes that alternative's payload
layout just as destructively as a reorder changes the tag.

Comparison runs in BOTH directions on module-qualified identities, so an
omitted type, a stale entry, a renamed type, and a same-named type in
another module all fail rather than pass silently.

An APPEND (a new trailing constructor, or a brand-new qualifying type)
is COMPATIBLE — but it still fails until the baseline is updated. That
ratchet is load-bearing: if an append did not have to land in the
baseline, the original list would remain a prefix of the live
declaration forever, and a LATER removal or rename of that appended
constructor would be invisible. `--update-baseline` performs exactly
that ratchet, and refuses to write anything when any incompatible change
is also present, so it can never double as a "make it pass" button.

What this audit does NOT cover: hand-written `Serialize` instances (the
`put`/`get` code IS the wire contract, in source, where review can see
it), field-order drift inside a single-constructor record (the frozen-DTO
boundary rule + `tools/save_compat_audit.py`), and whether a migration is
CORRECT (`Test.Headless.World.Save.Compat`'s real decode of tracked
fixtures).

Usage:
  python3 tools/enum_append_only_audit.py
  python3 tools/enum_append_only_audit.py --update-baseline
  python3 tools/enum_append_only_audit.py --self-test
Exit codes: 0 = the live declarations match the baseline exactly,
1 = they do not (append-compatible or not), or a self-test check failed.
"""
from __future__ import annotations

import argparse
import contextlib
import io
import json
import re
import tempfile
from dataclasses import dataclass, field
from pathlib import Path

REPO_ROOT = Path(__file__).resolve().parent.parent
BASELINE_REL = "docs/save_compat/enum_baseline.json"

# The shipped library + executable. Test-suite types are not the wire
# contract and are deliberately out of scope.
SOURCE_DIRS = ("src", "app")

# Where the save wire format's ROOT types live — used only to attribute a
# guarded type to the components and historical shapes that carry it (the
# diagnostic), never to decide what is guarded.
#
# The two globs are the point: a NEW component module or a new
# frozen-legacy module joins the root set automatically, so the roots
# cannot silently fall behind the code. Everything else is declared
# below and checked for liveness, so a stale entry fails too.
WIRE_ROOT_GLOBS = (
    "src/World/Save/Component/*.hs",
    "src/World/Save/Compat/*.hs",
)

# Modules matching those globs that are NOT DTO carriers. Each must still
# exist (a stale exclusion fails).
WIRE_ROOT_GLOB_EXCLUSIONS = {
    "World.Save.Component.Types":
        "content-free component machinery — declares the codec/registry "
        "contract, no component's DTO",
}

# Serialized root modules outside those globs. Each must still exist.
WIRE_ROOT_EXTRA = {
    "World.Save.Types":
        "the transitional in-memory load bridge and the `metadata` "
        "component's payload",
    "World.Save.Envelope.Types":
        "the checksummed envelope manifest framing",
    "World.Save.Reference":
        "the typed persistent references shared by every component",
}

# Haskell's symbol characters. A run of dashes followed by one of these
# is an operator (`-->`), not the start of a comment.
_SYMBOL_CHARS = set("!#$%&*+./<=>?@\\^|-~:")

# A top-level type declaration's opening line. Anchored at column 0,
# which is what keeps `where`/`let`-bound local declarations out.
_DECL_RE = re.compile(r"^(data|newtype|type)[ \t]+(.*)$")

# The declaration head's type name, allowing a leading `family`/`instance`
# keyword so those forms are RECOGNISED (and then rejected) rather than
# mistaken for an ordinary declaration.
_HEAD_NAME_RE = re.compile(r"(?:(family|instance)[ \t]+)?"
                           r"([A-Z][A-Za-z0-9_']*)")

_DERIVING_RE = re.compile(r"(?<![A-Za-z0-9_'])deriving(?![A-Za-z0-9_'])")
_STRATEGY_RE = re.compile(r"[ \t\r\n]*(stock|anyclass|newtype|via)"
                          r"(?![A-Za-z0-9_'])")
_VIA_RE = re.compile(r"(?<![A-Za-z0-9_'])via(?![A-Za-z0-9_'])")
_WHERE_RE = re.compile(r"(?<![A-Za-z0-9_'])where(?![A-Za-z0-9_'])")

# A (possibly qualified) type/constructor identifier. Only the LAST
# component is wanted: `HM.HashMap` is `HashMap`, and a fully qualified
# `Unit.Direction.Direction` is `Direction` — dropping the qualifier is
# what keeps the reference graph from missing an edge.
_QUALIFIED_RE = re.compile(
    r"(?<![A-Za-z0-9_'.])(?:[A-Z][A-Za-z0-9_']*\.)*([A-Z][A-Za-z0-9_']*)")

_CTOR_NAME_RE = re.compile(r"[ \t\r\n]*([A-Z][A-Za-z0-9_']*)")
_FIELD_NAME_RE = re.compile(r"[a-z_][A-Za-z0-9_']*")
_HAS_SIG_RE = re.compile(r"∷|::")

# `csComponent = <ident>` inside a component module, and the
# `<ident> = ComponentId "<text>"` definitions those resolve against.
_CS_COMPONENT_RE = re.compile(
    r"(?<![A-Za-z0-9_'])csComponent[ \t]*=[ \t]*([a-z][A-Za-z0-9_']*)")
_COMPONENT_ID_RE = re.compile(
    r"^([a-z][A-Za-z0-9_']*)[ \t]*=[ \t]*ComponentId[ \t]+\"([^\"]*)\"",
    re.MULTILINE)


class AuditError(Exception):
    """Something the reader could not classify.

    Every one of these is a HARD failure naming the source line. Silently
    skipping a declaration the parser does not understand is exactly how
    an unguarded enum would slip past, so an unrecognised declaration
    form, deriving strategy, or constructor shape reports instead."""


@dataclass(frozen=True)
class Constructor:
    """One alternative of a sum type, with the number of fields it
    carries. Both halves are the wire contract: the NAME pins what the
    positional tag means, the ARITY pins that alternative's payload."""
    name: str
    arity: int

    def render(self) -> str:
        return f"{self.name}/{self.arity}"


@dataclass
class Declaration:
    """One top-level `data`/`newtype`/`type` declaration."""
    kind: str                    # data | newtype | type
    name: str
    module: str                  # World.Save.Component.Page
    rel_path: str                # src/World/Save/Component/Page.hs
    line: int
    body: str                    # RHS, comments and deriving clauses gone
    # deriving strategy ("", "stock", "anyclass", "newtype") -> classes
    deriving_classes: dict[str, set[str]] = field(default_factory=dict)

    @property
    def qualified(self) -> str:
        return f"{self.module}.{self.name}"

    def where(self) -> str:
        return f"{self.rel_path}:{self.line}"


@dataclass
class GuardedType:
    """A discovered guarded sum: its identity plus its constructor list."""
    module: str
    name: str
    rel_path: str
    line: int
    constructors: list[Constructor]

    @property
    def qualified(self) -> str:
        return f"{self.module}.{self.name}"

    def where(self) -> str:
        return f"{self.rel_path}:{self.line}"


# ----- Haskell lexing ------------------------------------------------

def strip_haskell_comments(text: str) -> str:
    """Blank out Haskell comments, preserving every line break and column.

    Chasing comment forms in the declaration patterns themselves is a
    losing game — these modules are dense with `-- ^` haddock hanging off
    constructor fields, and the form that gets missed silently DROPS a
    constructor instead of reporting it. Removing comments up front
    collapses all of them into whitespace, so everything below only ever
    sees code. Line and column positions are preserved so reported line
    numbers still point at the real source.

    Nested `{- {- -} -}` blocks, string and character literals, and the
    identifier-trailing apostrophe (`DirNE'`, which must NOT open a
    character literal) are handled. A `{-#`-opened pragma is blanked the
    same way a block comment is, which is what the constructor reader
    wants: an `{-# UNPACK #-}` between a `!` and its type is noise.

    (Deliberately duplicated from `tools/material_id_audit.py` /
    `tools/unicode_operator_audit.py` rather than shared: every audit in
    `tools/` is a standalone, dependency-free script.)"""
    out: list[str] = []
    i, n = 0, len(text)
    depth = 0
    in_line_comment = False
    in_string = False
    in_char = False

    def blank(ch: str) -> str:
        return "\n" if ch == "\n" else " "

    while i < n:
        ch = text[i]
        pair = text[i:i + 2]
        if in_line_comment:
            if ch == "\n":
                in_line_comment = False
                out.append(ch)
            else:
                out.append(" ")
            i += 1
        elif depth:
            if pair == "{-":
                depth += 1
                out.append("  ")
                i += 2
            elif pair == "-}":
                depth -= 1
                out.append("  ")
                i += 2
            else:
                out.append(blank(ch))
                i += 1
        elif in_string or in_char:
            out.append(ch)
            if ch == "\\" and i + 1 < n:
                out.append(text[i + 1])
                i += 2
                continue
            if in_string and ch == '"':
                in_string = False
            elif in_char and ch == "'":
                in_char = False
            i += 1
        elif pair == "{-":
            depth = 1
            out.append("  ")
            i += 2
        elif pair == "--":
            run = i
            while run < n and text[run] == "-":
                run += 1
            following = text[run:run + 1]
            if following and following in _SYMBOL_CHARS:
                # A dash run followed by a symbol char is an operator.
                out.append(text[i:run])
                i = run
            else:
                in_line_comment = True
                out.append(" " * (run - i))
                i = run
        elif ch == '"':
            in_string = True
            out.append(ch)
            i += 1
        elif ch == "'" and not (i and (text[i - 1].isalnum()
                                       or text[i - 1] in "_'")):
            in_char = True
            out.append(ch)
            i += 1
        else:
            out.append(ch)
            i += 1
    return "".join(out)


def split_top_level(text: str, separators: str) -> list[str]:
    """Split on `separators` seen at bracket depth 0, outside literals.

    Used for the `|` between constructor alternatives and the `,` between
    a record's fields — both of which routinely appear NESTED
    (`!(Maybe (Int, Int))`, `![(Text, Int)]`) where they must not split."""
    out: list[str] = []
    cur: list[str] = []
    depth = 0
    in_string = False
    in_char = False
    i, n = 0, len(text)
    while i < n:
        ch = text[i]
        if in_string or in_char:
            cur.append(ch)
            if ch == "\\" and i + 1 < n:
                cur.append(text[i + 1])
                i += 2
                continue
            if (in_string and ch == '"') or (in_char and ch == "'"):
                in_string = in_char = False
            i += 1
            continue
        if ch == '"':
            in_string = True
        elif ch == "'" and not (i and (text[i - 1].isalnum()
                                       or text[i - 1] in "_'")):
            in_char = True
        elif ch in "([{":
            depth += 1
        elif ch in ")]}":
            depth -= 1
        elif depth == 0 and ch in separators:
            out.append("".join(cur))
            cur = []
            i += 1
            continue
        cur.append(ch)
        i += 1
    out.append("".join(cur))
    return out


def split_atoms(text: str) -> list[str]:
    """Split a positional constructor's argument list into its fields.

    Haskell requires every non-atomic field type to be parenthesised, so
    one bracket-aware whitespace-separated token IS one field:
    `!Int !Int !FloraId` is three, `!(Maybe WorldPageId) ![Text]` is two."""
    atoms: list[str] = []
    cur: list[str] = []
    depth = 0
    for ch in text:
        if ch in "([{":
            depth += 1
        elif ch in ")]}":
            depth -= 1
        if depth == 0 and ch in " \t\n\r":
            if cur:
                atoms.append("".join(cur))
                cur = []
            continue
        cur.append(ch)
    if cur:
        atoms.append("".join(cur))
    return atoms


def matching_bracket(text: str) -> int:
    """Index of the bracket closing the one at index 0, or -1."""
    depth = 0
    for i, ch in enumerate(text):
        if ch in "([{":
            depth += 1
        elif ch in ")]}":
            depth -= 1
            if depth == 0:
                return i
    return -1


# ----- Declaration extraction ----------------------------------------

def module_name_of(rel_path: str, text: str) -> str:
    """The module's name, cross-checked against its own header.

    The path is authoritative (it is what makes a baseline key stable),
    but a header that disagrees means the identity this audit keys on is
    not what it appears to be — so a mismatch fails rather than picking a
    winner."""
    stem = rel_path
    for prefix in SOURCE_DIRS:
        if stem.startswith(prefix + "/"):
            stem = stem[len(prefix) + 1:]
            break
    from_path = stem[:-3].replace("/", ".") if stem.endswith(".hs") else stem
    header = re.search(r"^module[ \t]+([A-Z][A-Za-z0-9_'.]*)", text, re.M)
    if header is None:
        raise AuditError(f"{rel_path}: no `module <Name> where` header")
    if header.group(1) != from_path:
        raise AuditError(
            f"{rel_path}: module header says `{header.group(1)}` but the "
            f"file path says `{from_path}` — the baseline keys on "
            f"module-qualified type identities, so these must agree")
    return from_path


def declaration_blocks(text: str) -> list[tuple[int, str]]:
    """Every top-level `data`/`newtype`/`type` block, as (1-based start
    line, block text). A block runs to the next line that starts a new
    top-level declaration — Haskell's layout rule means anything blank or
    indented still belongs to the one above."""
    lines = text.split("\n")
    blocks: list[tuple[int, str]] = []
    i, n = 0, len(lines)
    while i < n:
        if _DECL_RE.match(lines[i]):
            j = i + 1
            while j < n and (lines[j].strip() == ""
                             or lines[j][:1] in (" ", "\t")):
                j += 1
            blocks.append((i + 1, "\n".join(lines[i:j])))
            i = j
        else:
            i += 1
    return blocks


def parse_deriving(where: str, name: str,
                   block: str) -> tuple[str, dict[str, set[str]]]:
    """Split a declaration into (the part before its deriving clauses, the
    classes each clause derives keyed by strategy).

    A declaration may carry SEVERAL clauses — this codebase routinely
    writes `deriving stock (Show, Eq, Generic)` and `deriving anyclass
    (Hashable, Serialize)` as separate lines — so all of them are
    collected and the strategy kept, because `deriving newtype
    (Serialize)` means something completely different from `deriving
    anyclass (Serialize)`.

    `deriving via` is REFUSED outright rather than approximated: its two
    spellings put the class list on either side of the `via` type, and a
    reader that guessed wrong would silently drop a `Serialize` instance
    from the guarded set. There are none in this tree."""
    first = _DERIVING_RE.search(block)
    if first is None:
        return block, {}
    body = block[:first.start()]
    if _VIA_RE.search(block, first.start()):
        raise AuditError(
            f"{where}: `{name}` uses `deriving via`, whose wire layout "
            f"this audit cannot classify")
    clauses: dict[str, set[str]] = {}
    pos = first.start()
    while True:
        match = _DERIVING_RE.search(block, pos)
        if match is None:
            break
        cursor = match.end()
        strategy = ""
        strategy_match = _STRATEGY_RE.match(block, cursor)
        if strategy_match:
            strategy = strategy_match.group(1)
            cursor = strategy_match.end()
        while cursor < len(block) and block[cursor] in " \t\r\n":
            cursor += 1
        if cursor < len(block) and block[cursor] == "(":
            close = matching_bracket(block[cursor:])
            if close < 0:
                raise AuditError(
                    f"{where}: `{name}` has an unterminated `deriving (` "
                    f"clause")
            inner = block[cursor + 1:cursor + close]
            names = {n for part in split_top_level(inner, ",")
                     for n in _QUALIFIED_RE.findall(part)}
            cursor = cursor + close + 1
        else:
            bare = _QUALIFIED_RE.match(block, cursor)
            if bare is None:
                rest = " ".join(block[cursor:cursor + 60].split())
                raise AuditError(
                    f"{where}: `{name}` has an unreadable `deriving` "
                    f"clause: {rest!r}")
            names = {bare.group(1)}
            cursor = bare.end()
        clauses.setdefault(strategy, set()).update(names)
        pos = cursor
    return body, clauses


def parse_declaration(rel_path: str, module: str, line: int,
                      block: str) -> Declaration:
    """Turn one top-level declaration block into a `Declaration`.

    Rejects — loudly — every form this reader cannot classify: GADT
    syntax, data families and instances, `deriving via`. None exists in
    this tree today; one appearing later must report rather than be
    skipped, because a skipped `data ... where` is an unguarded enum."""
    where = f"{rel_path}:{line}"
    head_line = block.split("\n", 1)[0]
    kind_match = _DECL_RE.match(head_line)
    assert kind_match is not None
    kind = kind_match.group(1)
    head_match = _HEAD_NAME_RE.match(kind_match.group(2))
    if head_match is None:
        head = " ".join(head_line[:80].split())
        raise AuditError(f"{where}: cannot read the declaration head {head!r}")
    if head_match.group(1) is not None:
        raise AuditError(
            f"{where}: `{kind} {head_match.group(1)}` is not a form this "
            f"audit can classify — a data family/instance can declare "
            f"constructors this reader would never see")
    name = head_match.group(2)
    body, clauses = parse_deriving(where, name, block)
    if _WHERE_RE.search(body):
        raise AuditError(
            f"{where}: `{kind} {name}` uses GADT syntax (`where`), whose "
            f"constructors this reader cannot enumerate")
    # Everything after the FIRST top-level `=`. Bracket-aware so a `=`
    # nested in the head could never be mistaken for the one opening the
    # constructor list, and re-joined so a later one is not lost.
    parts = split_top_level(body, "=")
    return Declaration(kind=kind, name=name, module=module,
                       rel_path=rel_path, line=line,
                       body="=".join(parts[1:]) if len(parts) > 1 else "",
                       deriving_classes=clauses)


def parse_constructors(decl: Declaration) -> list[Constructor]:
    """The declaration's constructor list, in declared order.

    Anything that is not a plain prefix constructor — an infix
    constructor operator, an existential/context-carrying alternative, an
    empty alternative — fails rather than being silently miscounted."""
    alternatives = split_top_level(decl.body, "|")
    if len(alternatives) == 1 and not alternatives[0].strip():
        return []
    constructors: list[Constructor] = []
    for alt in alternatives:
        text = alt.strip()
        if not text:
            raise AuditError(
                f"{decl.where()}: `{decl.name}` has an empty constructor "
                f"alternative (a stray `|`?)")
        for keyword in ("forall", "∀"):
            if re.search(rf"(?<![A-Za-z0-9_']){re.escape(keyword)}"
                         rf"(?![A-Za-z0-9_'])", text):
                raise AuditError(
                    f"{decl.where()}: `{decl.name}` has a constructor using "
                    f"`{keyword}`, which this reader cannot classify")
        name_match = _CTOR_NAME_RE.match(text)
        if name_match is None:
            head = " ".join(text[:60].split())
            raise AuditError(
                f"{decl.where()}: cannot read a constructor name from "
                f"{head!r} — an infix constructor operator is not a form "
                f"this audit can classify")
        ctor = name_match.group(1)
        rest = text[name_match.end():]
        brace = rest.find("{")
        if brace >= 0 and not rest[:brace].strip():
            close = matching_bracket(rest[brace:])
            if close < 0:
                raise AuditError(
                    f"{decl.where()}: `{ctor}`'s record braces are "
                    f"unterminated")
            trailing = rest[brace + close + 1:].strip()
            if trailing:
                extra = " ".join(trailing[:40].split())
                raise AuditError(
                    f"{decl.where()}: `{ctor}` has unexpected text after "
                    f"its record braces: {extra!r}")
            arity = record_arity(decl, ctor, rest[brace + 1:brace + close])
        else:
            if _HAS_SIG_RE.search(rest):
                raise AuditError(
                    f"{decl.where()}: `{ctor}` carries a `∷` outside record "
                    f"braces, which this reader cannot classify")
            atoms = split_atoms(rest)
            for atom in atoms:
                if not atom.lstrip("!~"):
                    raise AuditError(
                        f"{decl.where()}: `{ctor}` has an unreadable field "
                        f"{atom!r}")
            arity = len(atoms)
        constructors.append(Constructor(ctor, arity))
    return constructors


def record_arity(decl: Declaration, ctor: str, inner: str) -> int:
    """How many fields a record constructor declares.

    Every depth-0 comma-separated group is exactly one field: both
    `{ a ∷ !Int, b ∷ !Int }` and the shared-signature `{ a, b ∷ !Int }`
    split into two groups, and both are two fields on the wire."""
    total = 0
    for group in split_top_level(inner, ","):
        text = group.strip()
        if not text:
            raise AuditError(
                f"{decl.where()}: `{ctor}` has an empty record field group")
        if _FIELD_NAME_RE.match(text) is None:
            head = " ".join(text[:40].split())
            raise AuditError(
                f"{decl.where()}: `{ctor}` has an unreadable record field "
                f"{head!r}")
        total += 1
    return total


def qualifies_as_guarded(decl: Declaration) -> bool:
    """Does this declaration meet guarded-set conditions 1 and 2?

    Condition 3 (two or more constructors) is checked by the caller,
    which needs the parsed list anyway."""
    if decl.kind != "data":
        return False
    clauses = decl.deriving_classes
    generic = any("Generic" in names for names in clauses.values())
    serialize = any("Serialize" in names
                    for strategy, names in clauses.items()
                    if strategy != "newtype")
    return generic and serialize


# ----- Repository scan -----------------------------------------------

@dataclass
class Scan:
    guarded: dict[str, GuardedType]           # qualified -> type
    declarations: list[Declaration]
    module_paths: dict[str, str]              # module -> rel path


def iter_source_files(root: Path) -> list[Path]:
    files: list[Path] = []
    for directory in SOURCE_DIRS:
        base = root / directory
        if base.is_dir():
            files.extend(sorted(base.rglob("*.hs")))
    return files


def scan_repository(root: Path) -> Scan:
    """Parse every shipped Haskell module, and pick out the guarded sums."""
    guarded: dict[str, GuardedType] = {}
    declarations: list[Declaration] = []
    module_paths: dict[str, str] = {}
    for path in iter_source_files(root):
        rel = path.relative_to(root).as_posix()
        text = strip_haskell_comments(path.read_text(encoding="utf-8"))
        module = module_name_of(rel, text)
        module_paths[module] = rel
        standalone = re.search(
            r"^deriving[ \t]+.*(?<![A-Za-z0-9_'])Serialize(?![A-Za-z0-9_'])",
            text, re.M)
        if standalone:
            raise AuditError(
                f"{rel}: standalone `deriving ... Serialize` is not a form "
                f"this audit can classify — it attaches an instance to a "
                f"type whose own declaration carries no evidence of it")
        for line, block in declaration_blocks(text):
            decl = parse_declaration(rel, module, line, block)
            declarations.append(decl)
            if not qualifies_as_guarded(decl):
                continue
            constructors = parse_constructors(decl)
            if len(constructors) < 2:
                continue
            if decl.qualified in guarded:
                raise AuditError(
                    f"{decl.where()}: `{decl.qualified}` is declared twice")
            guarded[decl.qualified] = GuardedType(
                module=decl.module, name=decl.name, rel_path=decl.rel_path,
                line=decl.line, constructors=constructors)
    return Scan(guarded=guarded, declarations=declarations,
                module_paths=module_paths)


# ----- Wire-carrier attribution (diagnostic only) --------------------
#
# None of this decides what is GUARDED — that is the three-condition rule
# in the module docstring. It exists so an incompatible change can name
# every component and historical shape that actually carries the type
# (issue #1145's review: "must refer to every affected component, not one
# singular owner"), and so the guidance can say so honestly when a
# guarded type is on no wire at all.

# `data`/`newtype` declarations inside a component module that are NOT
# wire shapes, so must not seed the reachability walk. Each must still
# exist (a stale entry fails), and any OTHER non-`DTO`-named declaration
# appearing in a component module fails too — which is what stops a real
# wire type from being left out of the roots silently.
NON_WIRE_COMPONENT_DECLS = {
    "World.Save.Component.Page.WorldPages":
        "the canonical decode TARGET `worldPagesCodec` migrates into, not "
        "a wire shape — its own `WorldPagesDTO*` are the wire",
}


@dataclass(frozen=True)
class Carrier:
    """One save-wire root a guarded type is reachable from."""
    label: str                    # '"unit-sim" — World.Save.Component.Entities'
    components: tuple[str, ...]   # ("unit-sim",) — empty for a bare module
    sort_key: tuple[str, str]
    path: tuple[str, ...]         # UnitSimDTO → PageSimDTO → … → Pose


def top_level_blocks(text: str) -> list[str]:
    """Every top-level declaration block (a column-0 line plus everything
    blank or indented under it). Haskell's layout rule makes this exact
    enough for the stylized codec definitions read below."""
    lines = text.split("\n")
    blocks: list[str] = []
    i, n = 0, len(lines)
    while i < n:
        if lines[i][:1] not in ("", " ", "\t"):
            j = i + 1
            while j < n and (lines[j].strip() == ""
                             or lines[j][:1] in (" ", "\t")):
                j += 1
            blocks.append("\n".join(lines[i:j]))
            i = j
        else:
            i += 1
    return blocks


def first_argument_types(signature_block: str) -> set[str]:
    """The type constructors in a signature's FIRST argument.

    `migrateUnitSimDTOv1 ∷ UnitSimDTOv1 → UnitSimDTO` resolves to
    `{UnitSimDTOv1}` — which is how a frozen historical DTO, named
    nowhere but in its migration function's inferred argument type, still
    becomes a reachability root."""
    parts = re.split(r"∷|::", signature_block, maxsplit=1)
    if len(parts) < 2:
        return set()
    body = parts[1].replace("->", "→").replace("=>", "⇒")
    contexts = split_top_level(body, "⇒")
    body = contexts[-1]
    first = split_top_level(body, "→")[0]
    return set(_QUALIFIED_RE.findall(first))


def wire_root_modules(root: Path, scan: Scan) -> dict[str, str]:
    """The save-wire root modules, as module -> why it is a root.

    Both the glob exclusions and the extras are checked for liveness, so
    a module that is renamed or deleted fails here rather than quietly
    shrinking the roots."""
    roots: dict[str, str] = {}
    for glob in WIRE_ROOT_GLOBS:
        for path in sorted(root.glob(glob)):
            rel = path.relative_to(root).as_posix()
            module = rel[len("src/"):-len(".hs")].replace("/", ".")
            if module in WIRE_ROOT_GLOB_EXCLUSIONS:
                continue
            roots[module] = f"matches {glob}"
    for module in sorted(WIRE_ROOT_GLOB_EXCLUSIONS):
        if module not in scan.module_paths:
            raise AuditError(
                f"stale WIRE_ROOT_GLOB_EXCLUSIONS entry: module `{module}` "
                f"no longer exists")
    for module, why in sorted(WIRE_ROOT_EXTRA.items()):
        if module not in scan.module_paths:
            raise AuditError(
                f"stale WIRE_ROOT_EXTRA entry: module `{module}` no longer "
                f"exists")
        roots[module] = why
    return roots


def wire_root_types(scan: Scan, roots: dict[str, str]) -> set[str]:
    """Every declaration name that may seed the reachability walk.

    In a `World.Save.Component.*` module only the `*DTO*`-named
    declarations are wire shapes — the module also declares the canonical
    types its codecs decode INTO, and seeding one of those walks the live
    session snapshot and attributes a type to components that never carry
    it (`world-pages` does not put a `Pose` on disk; `unit-sim` does).
    Every other non-`DTO` declaration there must be listed in
    `NON_WIRE_COMPONENT_DECLS`, so a genuinely new non-`DTO` wire type
    cannot be left out of the roots silently. The remaining root modules
    (the frozen legacy shapes, the legacy bridge, the envelope framing,
    the typed references) hold wire types only, so every declaration
    there is a root."""
    accounted: set[str] = set()
    names: set[str] = set()
    for decl in scan.declarations:
        if decl.module not in roots:
            continue
        if decl.module.startswith("World.Save.Component.") \
                and "DTO" not in decl.name:
            if decl.qualified not in NON_WIRE_COMPONENT_DECLS:
                raise AuditError(
                    f"{decl.where()}: `{decl.qualified}` is declared in a "
                    f"component module but is not named `*DTO*` — either it "
                    f"is a wire shape (name it so) or it is not (declare it "
                    f"in NON_WIRE_COMPONENT_DECLS, with why)")
            accounted.add(decl.qualified)
            continue
        names.add(decl.name)
    stale = sorted(set(NON_WIRE_COMPONENT_DECLS) - accounted)
    if stale:
        raise AuditError(
            f"stale NON_WIRE_COMPONENT_DECLS entr(y|ies): "
            f"{', '.join(stale)} no longer declared")
    return names


@dataclass(frozen=True)
class Codec:
    """One registered component's codec, and the wire types it seeds."""
    component: str                # "unit-sim"
    module: str                   # World.Save.Component.Entities
    seeds: tuple[str, ...]


def discover_codecs(root: Path, scan: Scan,
                    root_types: set[str]) -> list[Codec]:
    """Read every `componentCodec ComponentSpec {…}` definition.

    A codec's seeds are the `*DTO*` types named anywhere in its signature
    or its definition, plus — for every local helper the definition names
    — the `*DTO*` types in that helper's FIRST ARGUMENT. The second half
    picks up the frozen historical DTOs, which appear nowhere except as
    `atVersion 1 migrateFooDTOv1`'s inferred argument type. The `*DTO*`
    restriction is what keeps a `toFooDTO ∷ FooSnapshot → FooDTO`
    converter from seeding the LIVE snapshot side and printing a
    reachability path through state the component never encodes.

    A codec that resolves to no component id, or to no seed at all,
    FAILS: attributing nothing would UNDER-name an affected component,
    the one direction this diagnostic must never go."""
    types_rel = scan.module_paths.get("World.Save.Component.Types")
    if types_rel is None:
        raise AuditError(
            "World.Save.Component.Types is missing — the `ComponentId` "
            "definitions this audit resolves against live there")
    types_text = strip_haskell_comments(
        (root / types_rel).read_text(encoding="utf-8"))
    literals = dict(_COMPONENT_ID_RE.findall(types_text))
    if not literals:
        raise AuditError(
            f'{types_rel}: no `<name> = ComponentId "<id>"` definitions found')
    codecs: list[Codec] = []
    for module, rel in sorted(scan.module_paths.items()):
        if not module.startswith("World.Save.Component.") \
                or module == "World.Save.Component.Types":
            continue
        text = strip_haskell_comments(
            (root / rel).read_text(encoding="utf-8"))
        blocks = top_level_blocks(text)
        signatures: dict[str, str] = {}
        for block in blocks:
            sig = re.match(r"([a-z][A-Za-z0-9_']*)[ \t]*(?:∷|::)", block)
            if sig:
                signatures[sig.group(1)] = block
        for block in blocks:
            # The type SIGNATURE spells the class `ComponentCodec`; only
            # the definition calls the lower-case builder.
            if "componentCodec" not in block:
                continue
            binding = re.match(r"([a-z][A-Za-z0-9_']*)", block)
            if binding is None:
                continue
            name = binding.group(1)
            seed_text = block + "\n" + signatures.get(name, "")
            ids: set[str] = set()
            for ident in _CS_COMPONENT_RE.findall(seed_text):
                if ident not in literals:
                    raise AuditError(
                        f"{rel}: `csComponent = {ident}` does not resolve to "
                        f"a `ComponentId` definition in {types_rel}")
                ids.add(literals[ident])
            if not ids:
                raise AuditError(
                    f"{rel}: `{name}` builds a `componentCodec` but declares "
                    f"no `csComponent` this reader can find")
            def wire(names: set[str]) -> set[str]:
                return {n for n in names if n in root_types and "DTO" in n}

            seeds = wire(set(_QUALIFIED_RE.findall(seed_text)))
            for helper in set(re.findall(
                    r"(?<![A-Za-z0-9_'])([a-z][A-Za-z0-9_']*)", seed_text)):
                if helper in signatures:
                    seeds |= wire(first_argument_types(signatures[helper]))
            if not seeds:
                raise AuditError(
                    f"{rel}: `{name}`'s codec names no wire type this reader "
                    f"can resolve, so its component could never be named in "
                    f"a migration diagnostic")
            for component in sorted(ids):
                codecs.append(Codec(component=component, module=module,
                                    seeds=tuple(sorted(seeds))))
    if not codecs:
        raise AuditError("no component codecs discovered")
    return codecs


def reference_graph(scan: Scan) -> dict[str, set[str]]:
    """Type name -> the type names its declaration mentions.

    Keyed by BARE name with every same-named declaration's references
    unioned. That is a deliberate over-approximation: this tree has
    same-named type pairs in different modules, and modelling Haskell's
    import resolution to tell them apart would risk UNDER-reaching — the
    direction that produces a diagnostic quietly missing a component."""
    refs: dict[str, set[str]] = {}
    for decl in scan.declarations:
        refs.setdefault(decl.name, set()).update(
            _QUALIFIED_RE.findall(decl.body))
    return refs


def reachable_from(seeds: list[str],
                   refs: dict[str, set[str]]) -> dict[str, tuple[str, ...]]:
    """Breadth-first closure over the reference graph, keeping the
    shortest path to each type reached."""
    seen: dict[str, tuple[str, ...]] = {}
    queue: list[str] = []
    for seed in sorted(seeds):
        if seed in refs and seed not in seen:
            seen[seed] = (seed,)
            queue.append(seed)
    while queue:
        cur = queue.pop(0)
        for ref in sorted(refs.get(cur, ())):
            if ref in seen or ref not in refs:
                continue
            seen[ref] = seen[cur] + (ref,)
            queue.append(ref)
    return seen


def compute_wire_carriers(root: Path, scan: Scan) -> dict[str, list[Carrier]]:
    """For each guarded type, every component and historical shape that
    carries it — attributed per CODEC (so a module owning five components
    names only the ones whose own wire actually reaches the type) and per
    non-component root module (the frozen legacy shapes, the legacy
    bridge, the envelope framing, the typed references, none of which has
    a component id of its own)."""
    roots = wire_root_modules(root, scan)
    root_types = wire_root_types(scan, roots)
    refs = reference_graph(scan)
    guarded_by_name: dict[str, list[GuardedType]] = {}
    for entry in scan.guarded.values():
        guarded_by_name.setdefault(entry.name, []).append(entry)
    carriers: dict[str, list[Carrier]] = {}

    def record(label: str, components: tuple[str, ...],
               sort_key: tuple[str, str],
               reached: dict[str, tuple[str, ...]]) -> None:
        for name, entries in guarded_by_name.items():
            if name not in reached:
                continue
            for entry in entries:
                carriers.setdefault(entry.qualified, []).append(
                    Carrier(label, components, sort_key, reached[name]))

    for codec in discover_codecs(root, scan, root_types):
        record(f'"{codec.component}" — {codec.module}', (codec.component,),
               ("0", codec.component),
               reachable_from(list(codec.seeds), refs))
    for module in sorted(roots):
        if module.startswith("World.Save.Component."):
            continue
        seeds = [d.name for d in scan.declarations
                 if d.module == module and d.name in root_types]
        record(f"{module} — {roots[module]}", (), ("1", module),
               reachable_from(seeds, refs))
    return carriers


# ----- Baseline ------------------------------------------------------

def load_baseline(path: Path) -> dict[str, list[Constructor]]:
    """Read the checked-in constructor baseline.

    Strict: a malformed entry fails rather than being skipped, because a
    skipped entry is an unguarded enum."""
    if not path.exists():
        raise AuditError(f"{BASELINE_REL}: baseline file is missing")
    try:
        raw = json.loads(path.read_text(encoding="utf-8"))
    except json.JSONDecodeError as err:
        raise AuditError(f"{BASELINE_REL}: not valid JSON ({err})") from err
    if not isinstance(raw, dict) or not isinstance(raw.get("types"), dict):
        raise AuditError(
            f"{BASELINE_REL}: expected an object with a `types` object")
    out: dict[str, list[Constructor]] = {}
    for qualified, entry in raw["types"].items():
        if not isinstance(entry, dict) or "constructors" not in entry:
            raise AuditError(
                f"{BASELINE_REL}: `{qualified}` has no `constructors` list")
        raw_ctors = entry["constructors"]
        if not isinstance(raw_ctors, list) or len(raw_ctors) < 2:
            raise AuditError(
                f"{BASELINE_REL}: `{qualified}`'s `constructors` must be a "
                f"list of at least two entries (only sums are guarded)")
        ctors: list[Constructor] = []
        for item in raw_ctors:
            if not isinstance(item, dict) or "name" not in item \
                    or "arity" not in item:
                raise AuditError(
                    f"{BASELINE_REL}: `{qualified}` has a constructor entry "
                    f"without both `name` and `arity`")
            arity = item["arity"]
            if not isinstance(arity, int) or isinstance(arity, bool) \
                    or arity < 0:
                raise AuditError(
                    f"{BASELINE_REL}: `{qualified}`'s `{item['name']}` has a "
                    f"non-integer arity {arity!r}")
            ctors.append(Constructor(str(item["name"]), arity))
        out[qualified] = ctors
    if not out:
        raise AuditError(
            f"{BASELINE_REL}: declares no types — a vacuous baseline would "
            f"pass against anything")
    return out


def render_baseline(guarded: dict[str, GuardedType],
                    carriers: dict[str, list[Carrier]]) -> str:
    """Serialize the guarded set as the baseline file's content."""
    types: dict[str, object] = {}
    for qualified in sorted(guarded):
        entry = guarded[qualified]
        types[qualified] = {
            "source": entry.rel_path,
            "onSaveWire": qualified in carriers,
            "components": sorted({
                component
                for carrier in carriers.get(qualified, ())
                for component in carrier.components}),
            "constructors": [{"name": c.name, "arity": c.arity}
                             for c in entry.constructors],
        }
    document = {
        "_comment": (
            "Golden constructor lists for every positionally-serialized "
            "sum type (issue #1145). Generated and checked by "
            "tools/enum_append_only_audit.py -- do not hand-edit to make "
            "the audit pass: a change that is not a pure append is a "
            "save-format break, not a baseline update. `components` and "
            "`onSaveWire` are informational reachability attribution; the "
            "`constructors` list is the contract."),
        "types": types,
    }
    return json.dumps(document, indent=2, ensure_ascii=False) + "\n"


# ----- Comparison ----------------------------------------------------

@dataclass
class Finding:
    qualified: str
    compatible: bool       # True = append-compatible; baseline must ratchet
    lines: list[str]
    show_carriers: bool = True


def classify(qualified: str, entry: GuardedType,
             baseline: list[Constructor]) -> Finding | None:
    """Compare one type's live constructor list against its baseline."""
    live = entry.constructors
    if live == baseline:
        return None
    if live[:len(baseline)] == baseline:
        added = ", ".join(c.render() for c in live[len(baseline):])
        return Finding(qualified, True, [
            f"{qualified} ({entry.where()})",
            f"    appended {added} after {baseline[-1].render()} — "
            f"APPEND-COMPATIBLE.",
        ])
    lines = [f"{qualified} ({entry.where()})"]
    lines.extend(describe_incompatibility(baseline, live))
    return Finding(qualified, False, lines)


def describe_incompatibility(baseline: list[Constructor],
                             live: list[Constructor]) -> list[str]:
    """Say WHICH tag positions changed meaning, not merely that they did."""
    lines: list[str] = []
    for index in range(max(len(baseline), len(live))):
        was = baseline[index] if index < len(baseline) else None
        now = live[index] if index < len(live) else None
        if was == now:
            continue
        if was is None:
            lines.append(f"    tag {index}: added {now.render()} after an "
                         f"already-changed tag (not an append)")
        elif now is None:
            lines.append(f"    tag {index}: {was.render()} REMOVED — every "
                         f"saved value carrying this tag is unreadable")
        elif was.name != now.name:
            lines.append(f"    tag {index}: was {was.render()}, now "
                         f"{now.render()} — every saved {was.name} decodes "
                         f"as {now.name}")
        else:
            lines.append(f"    tag {index}: {was.name} carried {was.arity} "
                         f"field(s), now carries {now.arity} — the payload "
                         f"after this tag changes shape")
    return lines


def compare(guarded: dict[str, GuardedType],
            baseline: dict[str, list[Constructor]]) -> list[Finding]:
    """Cross-check the discovered set against the baseline BOTH ways."""
    findings: list[Finding] = []
    for qualified in sorted(guarded):
        if qualified not in baseline:
            entry = guarded[qualified]
            findings.append(Finding(qualified, True, [
                f"{qualified} ({entry.where()})",
                f"    newly qualifies for the guarded set with "
                f"{len(entry.constructors)} constructors and has no "
                f"baseline entry — APPEND-COMPATIBLE.",
            ]))
            continue
        finding = classify(qualified, guarded[qualified], baseline[qualified])
        if finding is not None:
            findings.append(finding)
    for qualified in sorted(baseline):
        if qualified in guarded:
            continue
        findings.append(Finding(qualified, False, [
            f"{qualified} (baseline only)",
            "    has a baseline entry but no longer qualifies for the "
            "guarded set — it was renamed, moved to another module, lost "
            "its `Generic`-derived `Serialize` instance, stopped being a "
            "sum, or was deleted.",
            "    Every one of those changes what already-saved bytes mean. "
            "Confirm no shipped payload carries it, then remove the entry "
            "in the SAME commit as the change.",
        ], show_carriers=False))
    return findings


# ----- Reporting -----------------------------------------------------

def carrier_lines(qualified: str,
                  carriers: dict[str, list[Carrier]]) -> list[str]:
    """The migration guidance for one incompatible change: every affected
    component and historical shape, and what to do instead."""
    entries = carriers.get(qualified, [])
    if not entries:
        return [
            "    Not reachable from any save-wire DTO in this tree, so no "
            "component version needs bumping today — but this type does "
            "derive positional `Serialize`, so confirm nothing outside "
            "src/World/Save writes it to disk before changing it.",
        ]
    lines = ["    On the wire in:"]
    for carrier in sorted(entries, key=lambda c: (c.sort_key, c.path)):
        lines.append(f"      {carrier.label}")
        lines.append(f"        via {' → '.join(carrier.path)}")
    lines.extend([
        "    Do this instead of editing the declaration in place:",
        "      1. Freeze the CURRENT shape as a versioned DTO that stays "
        "decodable — including a frozen copy of the OLD enum, since the "
        "historical bytes still carry the old tags.",
        "      2. Bump `ccVersion` on EVERY component listed above, and add "
        "the outgoing version to that component's `ccInputVers` (through "
        "`csOlderVersions`/`atVersion`) so the reader still accepts it.",
        "      3. Migrate from the frozen DTO into the changed type.",
        "      4. Then append the new constructor at the END, and ratchet "
        "this baseline.",
    ])
    return lines


def report(findings: list[Finding], carriers: dict[str, list[Carrier]],
           guarded_count: int, stale_attribution: bool = False) -> int:
    compatible = [f for f in findings if f.compatible]
    incompatible = [f for f in findings if not f.compatible]
    if not findings:
        if stale_attribution:
            print(f"{BASELINE_REL}: every constructor list matches, but its "
                  f"`source`/`onSaveWire`/`components` attribution no longer "
                  f"matches the code.")
            print("  Nothing is broken on the wire — but a diagnostic that "
                  "names the wrong components is worse than none, so the "
                  "file is kept regenerated rather than merely append-checked.")
            print("  Refresh it with: "
                  "python3 tools/enum_append_only_audit.py --update-baseline")
            return 1
        print(f"enum_append_only_audit.py: {guarded_count} guarded sum "
              f"type(s) match {BASELINE_REL}")
        return 0
    if incompatible:
        print(f"{len(incompatible)} INCOMPATIBLE constructor change(s) — "
              f"these silently reinterpret already-saved bytes:")
        for finding in incompatible:
            for line in finding.lines:
                print(f"  {line}")
            if finding.show_carriers:
                for line in carrier_lines(finding.qualified, carriers):
                    print(f"  {line}")
            print()
    if compatible:
        print(f"{len(compatible)} append-compatible change(s) — allowed, but "
              f"{BASELINE_REL} must record them:")
        for finding in compatible:
            for line in finding.lines:
                print(f"  {line}")
        print("  An append stays safe only while the baseline ratchets "
              "forward with it: leave the old list in place and a later "
              "removal or rename of the appended constructor would still "
              "look like a prefix match.")
        print(f"  The guarded set is every `data` under "
              f"{'/, '.join(SOURCE_DIRS)}/ that derives `Serialize` through "
              f"`Generic` and declares two or more constructors — see "
              f"tools/enum_append_only_audit.py's module docstring for why "
              f"that rule, and what it deliberately does not cover.")
        if not incompatible:
            print("  Record them with: "
                  "python3 tools/enum_append_only_audit.py --update-baseline")
    return 1


# ----- Entry points --------------------------------------------------

def run_repository_audit(root: Path = REPO_ROOT) -> int:
    try:
        scan = scan_repository(root)
        if not scan.guarded:
            print(f"no `Generic`-derived `Serialize` sum types found under "
                  f"{'/, '.join(SOURCE_DIRS)}/ — the audit would pass "
                  f"vacuously")
            return 1
        path = root / BASELINE_REL
        baseline = load_baseline(path)
        carriers = compute_wire_carriers(root, scan)
        stale = (path.read_text(encoding="utf-8")
                 != render_baseline(scan.guarded, carriers))
    except AuditError as err:
        print(f"enum_append_only_audit.py: {err}")
        return 1
    return report(compare(scan.guarded, baseline), carriers,
                  len(scan.guarded), stale)


def run_update_baseline(root: Path = REPO_ROOT) -> int:
    """Ratchet the baseline over append-compatible changes only."""
    path = root / BASELINE_REL
    try:
        scan = scan_repository(root)
        if not scan.guarded:
            print("refusing to write a vacuous baseline: no guarded sum "
                  "types found")
            return 1
        carriers = compute_wire_carriers(root, scan)
        existing = load_baseline(path) if path.exists() else {}
    except AuditError as err:
        print(f"enum_append_only_audit.py: {err}")
        return 1
    findings = compare(scan.guarded, existing)
    incompatible = [f for f in findings if not f.compatible]
    if incompatible:
        print(f"refusing to update {BASELINE_REL}: {len(incompatible)} "
              f"change(s) are NOT appends. Rewriting the baseline over them "
              f"would erase the evidence that saved bytes changed meaning.")
        for finding in incompatible:
            for line in finding.lines:
                print(f"  {line}")
            if finding.show_carriers:
                for line in carrier_lines(finding.qualified, carriers):
                    print(f"  {line}")
        return 1
    rendered = render_baseline(scan.guarded, carriers)
    before = path.read_text(encoding="utf-8") if path.exists() else ""
    path.parent.mkdir(parents=True, exist_ok=True)
    path.write_text(rendered, encoding="utf-8")
    if findings:
        print(f"{BASELINE_REL}: recorded {len(findings)} append-compatible "
              f"change(s)")
        for finding in findings:
            print(f"  {finding.lines[0]}")
    elif before != rendered:
        print(f"{BASELINE_REL}: refreshed the reachability attribution "
              f"({len(scan.guarded)} guarded sum types, no constructor list "
              f"changed)")
    else:
        print(f"{BASELINE_REL}: already up to date ({len(scan.guarded)} "
              f"guarded sum types)")
    return 0


# ----- Self-test -----------------------------------------------------
#
# Proves the audit can FAIL, rather than passing vacuously (requirement
# 4), on synthetic trees rather than the real one: each fixture is a
# miniature repository holding the modules the audit's root/liveness
# rules require, plus whatever enum the case is about.

_TYPES_HS = """\
module World.Save.Component.Types where

unitSimComponentId ∷ ComponentId
unitSimComponentId = ComponentId "unit-sim"
unitsComponentId ∷ ComponentId
unitsComponentId = ComponentId "units"
worldPagesComponentId ∷ ComponentId
worldPagesComponentId = ComponentId "world-pages"
"""

# A component module shaped like the real `World.Save.Component.Page`:
# its codec decodes INTO a canonical type (`WorldPages`) that reaches the
# whole live snapshot, while the bytes it actually writes are the
# `*DTO*`s. Seeding the canonical type instead would attribute every
# enum in the session to `"world-pages"`.
_PAGE_HS = """\
module World.Save.Component.Page where

data WorldPages = WorldPages ![PageSnapshot]

data PageCoreDTO = PageCoreDTO
    { pcPageId ∷ !WorldPageId
    } deriving (Show, Eq, Generic, Serialize)

newtype WorldPagesDTO = WorldPagesDTO [PageCoreDTO]
    deriving (Show, Eq, Generic, Serialize)

worldPagesCodec ∷ ComponentCodec WorldPages
worldPagesCodec = componentCodec ComponentSpec
    { csComponent = worldPagesComponentId
    , csVersion   = 1
    , csDecode    = basePageSnapshots
    }

basePageSnapshots ∷ WorldPagesDTO → WorldPages
basePageSnapshots = undefined
"""

# NOT a wire module: the canonical in-memory session shape, reachable
# from `WorldPages` but never encoded by `"world-pages"`.
_SNAPSHOT_HS = """\
module World.Save.Snapshot where

data PageSnapshot = PageSnapshot
    { pgsPose ∷ !Pose
    } deriving (Show, Eq, Generic)
"""

_ENTITIES_HS = """\
module World.Save.Component.Entities where

data UnitSimStateDTO = UnitSimStateDTO
    { usdPose ∷ !Pose
    } deriving (Show, Eq, Generic, Serialize)

unitSimCodec ∷ ComponentCodec UnitSimStateDTO
unitSimCodec = componentCodec ComponentSpec
    { csComponent = unitSimComponentId
    , csVersion   = 1
    }

data UnitInstanceDTO = UnitInstanceDTO
    { uidPose ∷ !Pose
    } deriving (Show, Eq, Generic, Serialize)

unitsCodec ∷ ComponentCodec UnitInstanceDTO
unitsCodec = componentCodec ComponentSpec
    { csComponent = unitsComponentId
    , csVersion   = 1
    , csEncode    = map toUnitInstanceDTO . unitsOf
    }

toUnitInstanceDTO ∷ UnitInstanceSnapshot → UnitInstanceDTO
toUnitInstanceDTO = undefined
"""

_POSE_HS = """\
module Unit.Sim.Types where

-- | APPEND-ONLY.
data Pose
    = Standing     -- ^ upright
    | Crouching    -- ^ ducked
    | Crawling
    deriving (Show, Eq, Generic, Serialize)
"""

# The modules WIRE_ROOT_EXTRA / WIRE_ROOT_GLOB_EXCLUSIONS require to
# exist. `World.Save.Types` carries a real shape so the fixtures cover
# the legacy bridge: `UnitInstanceSnapshot` is the LIVE side that
# `toUnitInstanceDTO` converts FROM, so a codec must not seed itself
# from it — but the bridge module is a wire root in its own right, so
# `ToolMode` is still reported as on the wire.
_STUB_MODULES = {
    "src/World/Save/Types.hs":
        "module World.Save.Types where\n\n"
        "data UnitInstanceSnapshot = UnitInstanceSnapshot\n"
        "    { uisTool ∷ !ToolMode\n"
        "    } deriving (Show, Eq, Generic, Serialize)\n",
    "src/World/Save/Envelope/Types.hs":
        "module World.Save.Envelope.Types where\n",
    "src/World/Save/Reference.hs": "module World.Save.Reference where\n",
    "src/World/Tool/Types.hs":
        "module World.Tool.Types where\n\n"
        "data ToolMode = DefaultTool | InfoTool | MineTool\n"
        "    deriving (Show, Eq, Generic, Serialize)\n",
}

_CLEAN_BASELINE_CACHE: str | None = None


def _source_tree() -> dict[str, str]:
    """The fixture repository, with no baseline file yet."""
    tree = dict(_STUB_MODULES)
    tree["src/World/Save/Component/Types.hs"] = _TYPES_HS
    tree["src/World/Save/Component/Entities.hs"] = _ENTITIES_HS
    tree["src/World/Save/Component/Page.hs"] = _PAGE_HS
    tree["src/World/Save/Snapshot.hs"] = _SNAPSHOT_HS
    tree["src/Unit/Sim/Types.hs"] = _POSE_HS
    return tree


def _clean_baseline_text() -> str:
    """The baseline the fixture tree captures to, produced by the audit's
    OWN writer rather than transcribed — which is what makes the
    `expect_clean` cases prove that capturing and checking agree."""
    global _CLEAN_BASELINE_CACHE
    if _CLEAN_BASELINE_CACHE is None:
        _, out = _run(_source_tree(), update=True)
        _CLEAN_BASELINE_CACHE = out.split("<<baseline>>\n", 1)[1]
    return _CLEAN_BASELINE_CACHE


def _clean_tree() -> dict[str, str]:
    tree = _source_tree()
    tree[BASELINE_REL] = _clean_baseline_text()
    return tree


def _rewrite_baseline(edit) -> str:
    """The clean baseline with `edit` applied to its parsed document."""
    document = json.loads(_clean_baseline_text())
    edit(document)
    return json.dumps(document, indent=2, ensure_ascii=False) + "\n"


def _materialize(root: Path, tree: dict[str, str]) -> None:
    for rel, content in tree.items():
        path = root / rel
        path.parent.mkdir(parents=True, exist_ok=True)
        path.write_text(content, encoding="utf-8")


def _run(tree: dict[str, str], update: bool = False) -> tuple[int, str]:
    """Run the audit against a synthetic tree, capturing its output."""
    with tempfile.TemporaryDirectory() as tmp:
        root = Path(tmp)
        _materialize(root, tree)
        buffer = io.StringIO()
        with contextlib.redirect_stdout(buffer):
            code = (run_update_baseline(root) if update
                    else run_repository_audit(root))
        text = buffer.getvalue()
        if update:
            written = root / BASELINE_REL
            text += "\n<<baseline>>\n"
            text += written.read_text(encoding="utf-8") if written.exists() \
                else "<<absent>>"
        return code, text


def _pose(*alternatives: str) -> str:
    """A `Pose` module declaring exactly these alternatives."""
    body = "\n    | ".join(alternatives)
    return ("module Unit.Sim.Types where\n\n"
            f"data Pose\n    = {body}\n"
            "    deriving (Show, Eq, Generic, Serialize)\n")


def _self_test() -> list[str]:
    failures: list[str] = []

    def expect_clean(label: str, tree: dict[str, str]) -> None:
        code, out = _run(tree)
        if code != 0:
            failures.append(f"{label}: expected a clean pass, got exit "
                            f"{code}:\n{out}")

    def expect_fail(label: str, tree: dict[str, str], *needles: str) -> None:
        code, out = _run(tree)
        if code == 0:
            failures.append(f"{label}: expected a failure, got a clean pass")
            return
        for needle in needles:
            if needle not in out:
                failures.append(
                    f"{label}: output did not mention {needle!r}:\n{out}")

    def with_pose(*alternatives: str) -> dict[str, str]:
        tree = _clean_tree()
        tree["src/Unit/Sim/Types.hs"] = _pose(*alternatives)
        return tree

    # 1. The tree the baseline was captured from passes — including with
    #    haddock comments hanging off the constructors, which must not
    #    drop one.
    expect_clean("clean tree", _clean_tree())

    # 2. Requirement 1: each of the four incompatible mutations fails,
    #    and names the tag whose meaning changed.
    expect_fail("reorder", with_pose("Crouching", "Standing", "Crawling"),
                "INCOMPATIBLE", "tag 0: was Standing/0, now Crouching/0",
                "every saved Standing decodes as Crouching")
    expect_fail("insertion",
                with_pose("Standing", "Sleeping", "Crouching", "Crawling"),
                "INCOMPATIBLE", "tag 1: was Crouching/0, now Sleeping/0")
    expect_fail("removal", with_pose("Standing", "Crawling"),
                "INCOMPATIBLE", "tag 1: was Crouching/0, now Crawling/0",
                "tag 2: Crawling/0 REMOVED")
    expect_fail("rename", with_pose("Standing", "Ducking", "Crawling"),
                "INCOMPATIBLE", "tag 1: was Crouching/0, now Ducking/0")
    expect_fail("arity change",
                with_pose("Standing", "Crouching !Int", "Crawling"),
                "INCOMPATIBLE",
                "Crouching carried 0 field(s), now carries 1")

    # 3. Requirement 6 + the review's multi-component correction: an
    #    incompatible change names EVERY affected component and the DTO
    #    path, and says what to do instead.
    _, reorder_out = _run(with_pose("Crouching", "Standing", "Crawling"))
    for needle in ('"unit-sim" — World.Save.Component.Entities',
                   '"units" — World.Save.Component.Entities',
                   "via UnitSimStateDTO → Pose",
                   "via UnitInstanceDTO → Pose", "Bump `ccVersion` on EVERY",
                   "`ccInputVers`", "Migrate from the frozen DTO"):
        if needle not in reorder_out:
            failures.append(f"migration guidance: missing {needle!r}:"
                            f"\n{reorder_out}")
    # ...and ONLY the affected ones. `"world-pages"` decodes into a
    # canonical type that reaches `Pose`, but the bytes it writes never
    # carry one — seeding a codec's decode TARGET instead of its DTOs
    # would name every component in the session and make the guidance
    # worthless.
    if '"world-pages"' in reorder_out:
        failures.append(
            "migration guidance: named `world-pages`, which carries no "
            f"`Pose` on the wire:\n{reorder_out}")
    # The same trap from the other side: `unitsCodec` names the LIVE
    # `UnitInstanceSnapshot` (as `toUnitInstanceDTO`'s argument), which
    # carries a `ToolMode` its DTO does not. Seeding a codec from the
    # live side would make `"units"` claim every enum in the snapshot.
    tool = _clean_tree()
    tool["src/World/Tool/Types.hs"] = (
        "module World.Tool.Types where\n\n"
        "data ToolMode = InfoTool | DefaultTool | MineTool\n"
        "    deriving (Show, Eq, Generic, Serialize)\n")
    _, tool_out = _run(tool)
    if "World.Save.Types — the transitional" not in tool_out:
        failures.append(f"migration guidance: `ToolMode` rides the legacy "
                        f"bridge and must say so:\n{tool_out}")
    if '"units"' in tool_out:
        failures.append(
            "migration guidance: `units` claimed a `ToolMode` that only its "
            f"codec's LIVE input carries, not its DTO:\n{tool_out}")

    # 4. Requirement 6: an append is classified as ALLOWED, distinctly
    #    from a failure, and still requires the baseline to ratchet.
    appended = with_pose("Standing", "Crouching", "Crawling", "Sleeping")
    code, out = _run(appended)
    if code == 0:
        failures.append("append: must still fail until the baseline ratchets")
    if "APPEND-COMPATIBLE" not in out or "--update-baseline" not in out:
        failures.append(f"append: not reported as append-compatible:\n{out}")
    if "INCOMPATIBLE" in out:
        failures.append(f"append: misreported as incompatible:\n{out}")

    # 5. The ratchet itself: --update-baseline records the append, the
    #    tree then passes, and REMOVING the appended constructor
    #    afterwards fails (the hole a non-ratcheting baseline leaves).
    code, out = _run(appended, update=True)
    if code != 0:
        failures.append(f"ratchet: --update-baseline refused an append:\n{out}")
    if '"name": "Sleeping"' not in out:
        failures.append(f"ratchet: baseline did not record the append:\n{out}")
    ratcheted = dict(appended)
    ratcheted[BASELINE_REL] = out.split("<<baseline>>\n", 1)[1]
    expect_clean("ratcheted tree", ratcheted)
    regressed = dict(ratcheted)
    regressed["src/Unit/Sim/Types.hs"] = _pose(
        "Standing", "Crouching", "Crawling")
    expect_fail("removal of a previously appended constructor", regressed,
                "INCOMPATIBLE", "tag 3: Sleeping/0 REMOVED")
    renamed_after_append = dict(ratcheted)
    renamed_after_append["src/Unit/Sim/Types.hs"] = _pose(
        "Standing", "Crouching", "Crawling", "Dozing")
    expect_fail("rename of a previously appended constructor",
                renamed_after_append, "INCOMPATIBLE",
                "tag 3: was Sleeping/0, now Dozing/0")

    # 6. --update-baseline must never double as a "make it pass" button.
    code, out = _run(with_pose("Crouching", "Standing", "Crawling"),
                     update=True)
    if code == 0:
        failures.append("--update-baseline accepted a reorder")
    if "refusing to update" not in out:
        failures.append(f"--update-baseline: no refusal message:\n{out}")
    if '"name": "Crouching"' in out.split("<<baseline>>\n", 1)[1] \
            and out.split("<<baseline>>\n", 1)[1].index('"Crouching"') \
            < out.split("<<baseline>>\n", 1)[1].index('"Standing"'):
        failures.append("--update-baseline rewrote the baseline anyway")

    # 7. Guarded-set completeness: a newly qualifying enum with no
    #    baseline entry fails (and is append-compatible).
    new_enum = _clean_tree()
    new_enum["src/Craft/Bills.hs"] = (
        "module Craft.Bills where\n\n"
        "data BillMode = FixedCount | RepeatForever\n"
        "    deriving stock (Show, Eq, Generic)\n"
        "    deriving anyclass (Serialize)\n")
    expect_fail("newly qualifying enum without a baseline entry", new_enum,
                "Craft.Bills.BillMode", "newly qualifies", "APPEND-COMPATIBLE")

    # 8. ...in BOTH directions: a baseline entry with no live type fails.
    def add_ghost(document) -> None:
        document["types"]["Unit.Sim.Types.UnitActivity"] = {
            "constructors": [{"name": "Idle", "arity": 0},
                             {"name": "Walking", "arity": 0}]}

    stale = _clean_tree()
    stale[BASELINE_REL] = _rewrite_baseline(add_ghost)
    expect_fail("stale baseline entry", stale,
                "Unit.Sim.Types.UnitActivity", "baseline only",
                "no longer qualifies")

    # 9. ...on MODULE-QUALIFIED identities: the same type name in another
    #    module is a different type, not a match.
    moved = _clean_tree()
    del moved["src/Unit/Sim/Types.hs"]
    moved["src/Unit/Pose.hs"] = _pose(
        "Standing", "Crouching", "Crawling").replace(
            "module Unit.Sim.Types", "module Unit.Pose")
    expect_fail("same type name in another module", moved,
                "Unit.Pose.Pose", "Unit.Sim.Types.Pose", "baseline only")

    # 10. The guarded-set rule itself: each of the three conditions
    #     genuinely excludes, and none of them excludes too much.
    for label, source in (
            ("newtype", "module Extra.Types where\n\n"
                        "newtype Wrap = Wrap Int\n"
                        "    deriving stock (Generic)\n"
                        "    deriving newtype (Serialize)\n"),
            ("single-constructor record",
             "module Extra.Types where\n\n"
             "data Only = Only { a ∷ !Int, b ∷ !Int }\n"
             "    deriving (Show, Eq, Generic, Serialize)\n"),
            ("sum with no Serialize instance",
             "module Extra.Types where\n\n"
             "data Plain = PlainA | PlainB\n"
             "    deriving (Show, Eq, Generic)\n"),
            ("sum whose Serialize is derived via newtype",
             "module Extra.Types where\n\n"
             "data Odd = OddA | OddB\n"
             "    deriving stock (Show, Generic)\n"
             "    deriving newtype (Serialize)\n"),
            ("type synonym", "module Extra.Types where\n\n"
                             "type Alias = Either Int Bool\n")):
        tree = _clean_tree()
        tree["src/Extra/Types.hs"] = source
        expect_clean(f"not guarded: {label}", tree)
    for label, source, ctors in (
            ("split stock/anyclass deriving clauses",
             "module Extra.Types where\n\n"
             "data Split = SplitA | SplitB\n"
             "    deriving stock (Show, Eq, Generic)\n"
             "    deriving anyclass (Hashable, Serialize)\n",
             "2 constructors"),
            ("unparenthesised single-class deriving clauses",
             "module Extra.Types where\n\n"
             "data Bare = BareA | BareB | BareC\n"
             "    deriving Generic\n"
             "    deriving Serialize\n",
             "3 constructors"),
            ("payload-carrying constructors",
             "module Extra.Types where\n\n"
             "data Payload\n"
             "    = PayA !Int !(Maybe (Int, Int))\n"
             "    | PayB ![(Text, Int)]\n"
             "    deriving (Show, Eq, Generic, Serialize)\n",
             "2 constructors")):
        tree = _clean_tree()
        tree["src/Extra/Types.hs"] = source
        expect_fail(f"guarded: {label}", tree, "Extra.Types.", ctors)

    # 11. Field counting: a record's shared-signature group and a
    #     positional constructor's bracketed types must each count once.
    tree = _clean_tree()
    tree["src/Extra/Types.hs"] = (
        "module Extra.Types where\n\n"
        "data Counted\n"
        "    = CountRec { x, y ∷ !Int, z ∷ !(Maybe (Int, Int)) }\n"
        "    | CountPos !Int !(Maybe Int) ![(Text, Int)]\n"
        "    deriving (Show, Eq, Generic, Serialize)\n")
    code, out = _run(tree, update=True)
    written = out.split("<<baseline>>\n", 1)[1]
    for needle in ('"name": "CountRec",\n          "arity": 3',
                   '"name": "CountPos",\n          "arity": 3'):
        if needle not in written:
            failures.append(f"field counting: missing {needle!r}:\n{written}")

    # 12. Fail-loud, not fail-quiet: a form the reader cannot classify
    #     must report rather than silently leave an enum unguarded.
    for label, rel, source, needle in (
            ("GADT syntax", "src/Extra/Types.hs",
             "module Extra.Types where\n\n"
             "data Gadt where\n"
             "    GA ∷ Gadt\n"
             "    deriving (Generic, Serialize)\n",
             "GADT syntax"),
            ("data family", "src/Extra/Types.hs",
             "module Extra.Types where\n\n"
             "data family Fam a\n",
             "data family/instance"),
            ("deriving via", "src/Extra/Types.hs",
             "module Extra.Types where\n\n"
             "data Viaed = ViaA | ViaB\n"
             "    deriving stock (Generic)\n"
             "    deriving (Serialize) via Wrapper\n",
             "deriving via"),
            ("standalone deriving", "src/Extra/Types.hs",
             "module Extra.Types where\n\n"
             "data Stand = StandA | StandB\n"
             "deriving instance Serialize Stand\n",
             "standalone `deriving ... Serialize`"),
            ("module header disagreeing with its path",
             "src/Extra/Types.hs",
             "module Extra.Other where\n", "module header says")):
        tree = _clean_tree()
        tree[rel] = source
        expect_fail(f"fail-loud: {label}", tree, needle)

    # 13. Vacuity: nothing discovered, or nothing declared, must fail.
    empty_src = {k: v for k, v in _clean_tree().items()
                 if k not in ("src/Unit/Sim/Types.hs",
                              "src/World/Tool/Types.hs")}
    expect_fail("no guarded types discovered", empty_src,
                "would pass vacuously")
    no_baseline = _clean_tree()
    no_baseline[BASELINE_REL] = json.dumps({"types": {}}, indent=2) + "\n"
    expect_fail("empty baseline", no_baseline, "declares no types")
    missing = {k: v for k, v in _clean_tree().items() if k != BASELINE_REL}
    expect_fail("missing baseline", missing, "baseline file is missing")
    for label, content, needle in (
            ("malformed JSON", "{ nope", "not valid JSON"),
            ("no types object", json.dumps({"nope": {}}),
             "expected an object with a `types` object"),
            ("entry without constructors",
             json.dumps({"types": {"Unit.Sim.Types.Pose": {}}}),
             "has no `constructors` list"),
            ("constructor without an arity",
             json.dumps({"types": {"Unit.Sim.Types.Pose": {"constructors": [
                 {"name": "Standing"}, {"name": "Crouching", "arity": 0}]}}}),
             "without both `name` and `arity`"),
            ("non-integer arity",
             json.dumps({"types": {"Unit.Sim.Types.Pose": {"constructors": [
                 {"name": "Standing", "arity": "0"},
                 {"name": "Crouching", "arity": 0}]}}}),
             "non-integer arity")):
        tree = _clean_tree()
        tree[BASELINE_REL] = content
        expect_fail(f"malformed baseline: {label}", tree, needle)

    # 14. Root liveness: the carrier attribution's own declared modules
    #     must still exist, so a rename cannot silently shrink the roots.
    for label, dropped, needle in (
            ("WIRE_ROOT_EXTRA", "src/World/Save/Reference.hs",
             "stale WIRE_ROOT_EXTRA entry"),
            ("WIRE_ROOT_GLOB_EXCLUSIONS",
             "src/World/Save/Component/Types.hs",
             "stale WIRE_ROOT_GLOB_EXCLUSIONS entry"),
            ("NON_WIRE_COMPONENT_DECLS", "src/World/Save/Component/Page.hs",
             "stale NON_WIRE_COMPONENT_DECLS")):
        tree = {k: v for k, v in _clean_tree().items() if k != dropped}
        # Force a finding so the carrier walk runs.
        tree["src/Unit/Sim/Types.hs"] = _pose("Crouching", "Standing",
                                              "Crawling")
        expect_fail(f"root liveness: {label}", tree, needle)

    # 14a. The informational attribution is REGENERATED, not merely
    #      append-checked: a `components` list that no longer matches the
    #      code fails, because a diagnostic naming the wrong components
    #      is worse than none. It is reported as its own thing, never as
    #      a constructor change.
    def misattribute(document) -> None:
        document["types"]["Unit.Sim.Types.Pose"]["components"] = \
            ["world-pages"]

    misattributed = _clean_tree()
    misattributed[BASELINE_REL] = _rewrite_baseline(misattribute)
    code, out = _run(misattributed)
    if code == 0:
        failures.append("stale attribution: expected a failure")
    if "attribution no longer matches" not in out:
        failures.append(f"stale attribution: not reported as such:\n{out}")
    if "INCOMPATIBLE" in out or "APPEND-COMPATIBLE" in out:
        failures.append(f"stale attribution: misreported as a constructor "
                        f"change:\n{out}")

    # 14b. ...and the flip side: a NEW non-`DTO` declaration in a
    #      component module must be classified deliberately, never
    #      dropped from the roots by naming convention alone.
    unnamed = with_pose("Crouching", "Standing", "Crawling")
    unnamed["src/World/Save/Component/Entities.hs"] = _ENTITIES_HS + (
        "\ndata UnitSimStateWire = UnitSimStateWire\n"
        "    { uswPose ∷ !Pose\n"
        "    } deriving (Show, Eq, Generic, Serialize)\n")
    expect_fail("non-`DTO` declaration in a component module", unnamed,
                "is not named `*DTO*`", "NON_WIRE_COMPONENT_DECLS")

    # 14c. A codec whose component id or wire type this reader cannot
    #      resolve fails rather than attributing nothing — under-naming
    #      an affected component is the one direction that is unsafe.
    for label, block, needle in (
            ("unresolvable component id",
             "unknownCodec ∷ ComponentCodec PageCoreDTO\n"
             "unknownCodec = componentCodec ComponentSpec\n"
             "    { csComponent = mysteryComponentId\n"
             "    }\n",
             "does not resolve to a `ComponentId` definition"),
            ("no csComponent at all",
             "namelessCodec ∷ ComponentCodec PageCoreDTO\n"
             "namelessCodec = componentCodec ComponentSpec\n"
             "    { csVersion = 1\n"
             "    }\n",
             "declares no `csComponent`"),
            ("no resolvable wire type",
             "vagueCodec ∷ ComponentCodec WorldPages\n"
             "vagueCodec = componentCodec ComponentSpec\n"
             "    { csComponent = worldPagesComponentId\n"
             "    }\n",
             "names no wire type this reader can resolve")):
        tree = with_pose("Crouching", "Standing", "Crawling")
        tree["src/World/Save/Component/Page.hs"] = _PAGE_HS + "\n" + block
        expect_fail(f"codec discovery: {label}", tree, needle)

    # 15. A guarded type no save-wire DTO reaches says so, rather than
    #     inventing a component to bump.
    orphan = with_pose("Crouching", "Standing", "Crawling")
    orphan["src/World/Save/Component/Entities.hs"] = \
        "module World.Save.Component.Entities where\n"
    expect_fail("unreachable guarded type", orphan,
                "Not reachable from any save-wire DTO")

    # 16. The comment stripper must not misread code AS a comment (which
    #     would blank a real constructor out of the compared set).
    commented = _clean_tree()
    commented["src/Unit/Sim/Types.hs"] = (
        "module Unit.Sim.Types where\n\n"
        "data Pose\n"
        "    = Standing   {- upright -}  -- ^ the default\n"
        "    | Crouching  -- ^ ducked {- not a real block -}\n"
        "    | Crawling\n"
        "    deriving (Show, Eq, Generic, Serialize)\n")
    expect_clean("comments around the constructors", commented)
    dashes = _clean_tree()
    dashes["src/Extra/Types.hs"] = (
        "module Extra.Types where\n\n"
        "step ∷ Int → Int\n"
        "step a = a --> a\n"
        "data Dashed = DashA | DashB\n"
        "    deriving (Show, Eq, Generic, Serialize)\n")
    expect_fail("a dash-run operator is not a comment", dashes,
                "Extra.Types.Dashed")

    # 17. The real repository must parse without a single unclassifiable
    #     declaration — the check that keeps the fixtures above honest.
    try:
        scan = scan_repository(REPO_ROOT)
    except AuditError as err:
        failures.append(f"real tree: {err}")
    else:
        if len(scan.guarded) < 2:
            failures.append(
                f"real tree: only {len(scan.guarded)} guarded sum type(s) "
                f"discovered — the discovery rule is not finding them")
        # Every type issue #1145 and its review named by hand: the five
        # in the issue body, the seven more its review found reused by
        # `Session`/`Page`/`WorldGen` DTOs, the `ToolMode` the frozen
        # `SessionV90` still carries, and the whole `GeoTimeline` closure
        # `WorldGenParamsDTO.gpGeoTimeline` drags in. Pinning them here
        # is what proves the discovery RULE covers the set someone
        # enumerated by reading the code, rather than merely finding
        # some types.
        for expected in (
                "Unit.Direction.Direction", "Unit.Sim.Types.Pose",
                "Unit.Sim.Types.UnitActivity", "Craft.Bills.BillMode",
                "Power.Types.PowerRole",
                "Engine.Graphics.Camera.CameraFacing",
                "World.Render.Zoom.Types.ZoomMapMode",
                "World.Construct.Types.ConstructStatus",
                "World.Fluid.Types.FluidType",
                "World.Weather.Types.PressureType",
                "World.Weather.Types.SurfaceType",
                "Location.Instance.LocationLifecycle",
                "World.Tool.Types.ToolMode",
                "World.Geology.Timeline.Types.GeoScale",
                "World.Geology.Timeline.Event.GeoEvent",
                "World.Geology.Timeline.Feature.FeatureShape",
                "World.Geology.Timeline.Feature.FeatureActivity",
                "World.Geology.Timeline.Feature.FeatureEvolution",
                "World.Geology.Timeline.Feature.VolcanicFeature",
                "World.Hydrology.Types.HydroFeature",
                "World.Hydrology.Types.HydroEvolution",
                "World.Hydrology.Types.LakeSource"):
            if expected not in scan.guarded:
                failures.append(
                    f"real tree: `{expected}` (named by issue #1145) is not "
                    f"in the discovered guarded set")
    return failures


def main_self_test() -> int:
    failures = _self_test()
    if failures:
        print(f"{len(failures)} self-test failure(s):")
        for failure in failures:
            print(f"  FAIL: {failure}")
        return 1
    print("enum_append_only_audit.py self-test: all checks passed")
    return 0


def main() -> int:
    parser = argparse.ArgumentParser(
        description="Audit that every positionally-serialized (Generic "
                    "Serialize) sum type's constructor list has only ever "
                    "grown at the end.")
    group = parser.add_mutually_exclusive_group()
    group.add_argument("--self-test", action="store_true",
                       help="run the audit's own fixture checks instead of "
                            "auditing the repository")
    group.add_argument("--update-baseline", action="store_true",
                       help=f"ratchet {BASELINE_REL} over append-compatible "
                            f"changes (refuses when any change is not an "
                            f"append)")
    args = parser.parse_args()
    if args.self_test:
        return main_self_test()
    if args.update_baseline:
        return run_update_baseline()
    return run_repository_audit()


if __name__ == "__main__":
    raise SystemExit(main())
