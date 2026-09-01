"""Haskell lexing and declaration parsing for the enum append-only audit.

The one owner of what a declaration MEANS: comment and literal handling,
top-level splitting, field signatures, wire-equivalent field-type
normalization, deriving strategies, constructor and record payloads, and
the guarded-set qualification rule's conditions 1 and 2.

Everything here is fail-loud by design. A declaration form this reader
cannot classify raises `AuditError` rather than being skipped, because a
skipped declaration is an unguarded enum.
"""
from __future__ import annotations

import re

from enum_append_only_audit_model import (
    QUALIFIED_RE,
    SOURCE_DIRS,
    AuditError,
    Constructor,
    Declaration,
)


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

# A constructor name, a record selector, and the field-signature
# marker in either of its two spellings.
_CTOR_NAME_RE = re.compile(r"[ \t\r\n]*([A-Z][A-Za-z0-9_']*)")
_FIELD_NAME_RE = re.compile(r"[a-z_][A-Za-z0-9_']*")
_HAS_SIG_RE = re.compile(r"∷|::")


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


def split_field_signature(text: str) -> tuple[str, str] | None:
    """Split one record field group at its depth-0 `∷`/`::`.

    Returns `(selectors, declared type)`, or `None` when the group
    carries no signature at all — which is not an error but the LEADING
    half of a shared signature (`{ x, y ∷ !Int }` splits on the comma
    into a bare `x` and a signed `y ∷ !Int`, and `x`'s type is `y`'s).
    Depth-aware so a kind signature nested inside brackets is not
    mistaken for the field's own."""
    depth = 0
    for i, ch in enumerate(text):
        if ch in "([{":
            depth += 1
        elif ch in ")]}":
            depth -= 1
        elif depth == 0:
            if ch == "∷":
                return text[:i], text[i + 1:]
            if text[i:i + 2] == "::":
                return text[:i], text[i + 2:]
    return None


def normalize_field_type(text: str) -> str:
    """One field's declared type, reduced to its wire-significant spelling.

    Requirement 2's whole content is in what this erases, and the erased
    set is deliberately CLOSED and small — everything not listed here
    stays significant, because a normalizer that is clever about type
    equivalence would start silently accepting real changes:

      - comments, including the `{-# UNPACK #-}` pragma (already blanked
        to spaces by `strip_haskell_comments`, which is why they never
        arrive here);
      - layout — newlines and runs of whitespace collapse to one space,
        and the padding a bracketed or tupled type may carry inside its
        brackets or around its commas is canonicalised, so `!(Int, Int)`
        and `~( Int ,Int )` are the same field;
      - `::` spelled as `∷`;
      - the strictness/laziness markers `!` and `~`, which are a
        code-generation directive rather than part of the type;
      - and, ONLY as a consequence of that last one, a redundant
        enclosing parenthesis pair. `!` forces parentheses a bare type
        does not need (`a ∷ !(Maybe Int)` versus `a ∷ Maybe Int`), so
        without this an added or removed `!` would still show up as a
        payload change and requirement 2 would not hold. A tuple's
        parentheses ARE its type and are kept.

    What stays significant: the field ORDER, the declared type's
    structure, and every identifier in it."""
    normalized = " ".join(text.replace("::", "∷").split())
    normalized = re.sub(r"([(\[])[ ]+", r"\1", normalized)
    normalized = re.sub(r"[ ]+([)\],])", r"\1", normalized)
    normalized = re.sub(r",(?=[^ ])", ", ", normalized)
    while normalized[:1] in ("!", "~"):
        normalized = normalized[1:].lstrip()
    while len(normalized) > 2 and normalized[0] == "(" \
            and matching_bracket(normalized) == len(normalized) - 1:
        inner = normalized[1:-1].strip()
        if not inner or len(split_top_level(inner, ",")) > 1:
            break
        normalized = inner
    return normalized


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
                     for n in QUALIFIED_RE.findall(part)}
            cursor = cursor + close + 1
        else:
            bare = QUALIFIED_RE.match(block, cursor)
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
            payload = record_slots(decl, ctor,
                                   rest[brace + 1:brace + close])
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
            payload = [normalize_field_type(atom) for atom in atoms]
        constructors.append(Constructor(ctor, tuple(payload)))
    return constructors


def record_slots(decl: Declaration, ctor: str, inner: str) -> list[str]:
    """A record constructor's field slots, in declared order.

    Every depth-0 comma-separated group is exactly one field: both
    `{ a ∷ !Int, b ∷ !Int }` and the shared-signature `{ a, b ∷ !Int }`
    split into two groups, and both are two fields on the wire. In the
    shared form only the LAST group of a run carries the signature, so
    the unsigned ones ahead of it take their type from it — which is
    also why an unsigned group is not an error until the run ends
    without one.

    Each slot records the selector as well as the type. That is not
    decoration: cereal writes a record's fields positionally and never
    the selectors, so swapping two SAME-TYPED fields (`{ x, y ∷ !Int }`
    → `{ y, x ∷ !Int }`) is a real reinterpretation of saved bytes that
    the types alone cannot see. Keeping the selector is the only handle
    on which slot is which — with the deliberate consequence that a
    pure selector RENAME reports too, exactly as a constructor rename
    already does, because nothing in the declaration distinguishes a
    rename from a rename-plus-reorder."""
    slots: list[str] = []
    pending: list[str] = []
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
        split = split_field_signature(text)
        if split is None:
            if _FIELD_NAME_RE.fullmatch(text) is None:
                head = " ".join(text[:40].split())
                raise AuditError(
                    f"{decl.where()}: `{ctor}` has an unreadable record "
                    f"field {head!r}")
            pending.append(text)
            continue
        selector = split[0].strip()
        if _FIELD_NAME_RE.fullmatch(selector) is None:
            raise AuditError(
                f"{decl.where()}: `{ctor}` has an unreadable record field "
                f"selector {selector!r}")
        declared = normalize_field_type(split[1])
        if not declared:
            raise AuditError(
                f"{decl.where()}: `{ctor}`'s field `{selector}` declares "
                f"no type")
        for name in [*pending, selector]:
            slots.append(f"{name} ∷ {declared}")
        pending = []
    if pending:
        raise AuditError(
            f"{decl.where()}: `{ctor}`'s record field(s) "
            f"{', '.join(pending)} carry no type signature")
    return slots


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
