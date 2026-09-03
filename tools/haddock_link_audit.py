#!/usr/bin/env python3
"""Guard: every QUALIFIED haddock link `'Module.function'` in a `src/` or
`app/` comment names a function the module actually exports (#2292,
HLR-1 of docs/haddock_link_resolution_design.md).

A haddock link renders as navigable only when the named module exports
the named symbol; otherwise it renders as plain text and sends a reader
to a module that hides the name. Module splits and the deliberate export
narrowing of #1083/#1154/#1156 left many such references behind, and
nothing caught them: `synarchy.cabal` passes `-haddock` to GHC, which
validates comment SYNTAX only -- link TARGETS are resolved by the
`haddock` tool, which no gate runs.

The detection rule
------------------
A qualified link `'M.f'` inside a Haskell comment is DEAD when all of:

* `M` is a module under `src/` or `app/`;
* `M` has an explicit export list (a module without one exports
  everything, so its links always resolve);
* that list neither names `f` nor carries a `module X` re-export or a
  `T(..)` subordinate group that supplies it; and
* `f` is a real top-level definition somewhere under `src/` or `app/` --
  a `name ∷` signature at column 0, or a record field of a `data`/
  `newtype` declaration.

A module linking its OWN unexported name is included: `src/World/Save/
Storage.hs` links `'World.Save.Storage.publishValidated'`, which its own
export list omits, and that is a real dead link.

The last clause is what keeps Lua binding names out. `'UI.setVisible'`
looks like a Haskell reference only because `src/UI.hs` exists;
`setVisible` is a Lua verb with no Haskell definition anywhere, so the
link is not a candidate.

What is deliberately NOT reported
---------------------------------
The SINGLE-QUOTE delimiter is itself the scoping rule D-1 asked for, and
it is the whole mechanism behind four of the exclusions: `@M.f@` code
spans (the accepted spelling for a name that is real but not exported,
established by PR #1407), backtick-quoted `` `M.f` `` prose (which
`src/Unit/Pathing/Cost.hs` writes for the very name
`src/Engine/Asset/YamlMaterials.hs` links), module links `"M.N"`, and
unqualified `'f'` links -- none of them is a qualified `'M.f'`, so none
is ever matched. Nothing masks a code span first: inside `@…@` Haddock
still resolves a quoted identifier, so a `'M.f'` written there is a real
link and belongs in the report, and a mask keyed on a stray second `@`
would instead swallow whatever sits between them.

Also never reported: modules outside this repository, modules without an
explicit export list, names supplied through a re-export, record fields
reached through `Type(..)`, and any text inside a string literal, a
character literal or a quasiquote. `test/`, `test-headless/`, `cbits/`
and Lua are outside the scanned trees entirely. Those are DETECTION
rules, not discretionary exceptions -- the only allowance for a genuine
failure is the generated baseline below.

The baseline ratchet
--------------------
Owner decision D-3 lands the guard before the cleanup, so this tool
ships with a checked-in baseline of every dead link present at its own
commit. A run exits zero only when the live findings match that baseline
exactly: a link not in it fails as NEW, and an entry no longer found
fails as STALE, so the file can only shrink. `--update-baseline`
regenerates it; the file is generated, never hand-edited, exactly like
`docs/save_compat/enum_baseline.json`.

The baseline is TEMPORARY, not a permanent exemption list. HLR-2 through
HLR-4 drain it in bounded comment-only sweeps, and HLR-5 deletes both
the file and `--update-baseline`, after which zero dead links is the
permanent state.

Comment awareness is not this module's own lexer: it reuses
`tools/unicode_operator_audit.py`'s scanner through the
`haskell_comment_spans` it exposes for that purpose, so nested block
comments, string literals and atomically-skipped char literals behave
identically on both sides. Quasiquotes are masked HERE rather than
there, because the shared scanner has no quasiquote state and the
unicode audit handles its one quasiquoting file by a separate,
file-scoped step.

Usage:
    python3 tools/haddock_link_audit.py
    python3 tools/haddock_link_audit.py --update-baseline
"""
from __future__ import annotations

import argparse
import json
import re
import sys
from collections import Counter
from dataclasses import dataclass
from pathlib import Path

REPO_ROOT = Path(__file__).resolve().parent.parent
sys.path.insert(0, str(Path(__file__).resolve().parent))
from unicode_operator_audit import (  # type: ignore  # noqa: E402
    haskell_code_only, haskell_comment_spans)

#: The generated ratchet. Deleted by HLR-5 together with
#: `--update-baseline`; see the module docstring.
BASELINE_PATH = REPO_ROOT / "tools" / "haddock_link_baseline.json"

#: The two trees a link may live in AND a definition may come from. The
#: test suites, `cbits/` and Lua are outside both by design.
SCANNED_TREES = ("src", "app")

# A qualified haddock link: `'Data.Some.Module.functionName'`. The module
# part is one or more capitalised components; the symbol is a
# lower-case-initial identifier, so a link to a CONSTRUCTOR (`'M.Ctor'`)
# is not a candidate -- D-1 scopes this arc to functions.
LINK_RE = re.compile(
    r"'([A-Z][A-Za-z0-9_']*(?:\.[A-Z][A-Za-z0-9_']*)*)"
    r"\.([a-z_][A-Za-z0-9_']*)'")

# A quasiquote opener. GHC lexes `[name|` as one with QuasiQuotes on --
# which synarchy.cabal enables globally -- so this matches exactly what
# the compiler treats as quasiquoted, list-comprehension lookalikes
# included. The quoter may be QUALIFIED (`[QQ.glsl| … |]`), which is
# how a quoter imported qualified is spelled; leaving that form
# unrecognised leaves its body unmasked, and a `--` line inside raw
# quoted source is then read as a real Haskell comment.
QUASIQUOTE_OPEN_RE = re.compile(
    r"\[(?:[A-Z][A-Za-z0-9_']*\.)*[a-z_][A-Za-z0-9_']*\|")

MODULE_HEADER_RE = re.compile(
    r"^module\s+([A-Z][A-Za-z0-9_'.]*)", re.MULTILINE)

# An import head, matched against a declaration BLOCK: the qualifier in
# either position (`import qualified M` and ImportQualifiedPost's
# `import M qualified`), the alias, and `hiding`. Whatever follows is
# the import list, read separately because it routinely spans lines.
IMPORT_HEAD_RE = re.compile(
    r"\Aimport\s+(qualified\s+)?([A-Z][A-Za-z0-9_'.]*)\s*(qualified\b)?"
    r"\s*(?:as\s+([A-Z][A-Za-z0-9_'.]*))?\s*(hiding\b)?\s*")

# A top-level signature, matched against a declaration BLOCK that starts
# at column 0. `\s*` spans newlines, so the continuation-line form
#
#     publishValidated
#         ∷ (FilePath → IO ()) → …
#
# -- which is how src/World/Save/Storage.hs declares the one same-module
# case the initial baseline must contain -- is recognised exactly like
# the same-line form. Both `∷` and `::` are accepted: the tree is
# UnicodeSyntax throughout, so a detector written against ASCII `::`
# alone would find no definitions at all and report a silent clean run.
SIGNATURE_RE = re.compile(
    r"\A([a-z_][A-Za-z0-9_']*(?:\s*,\s*[a-z_][A-Za-z0-9_']*)*)\s*(?:∷|::)",
    re.DOTALL)

DATA_HEADER_RE = re.compile(r"\A(?:data|newtype)\s+([A-Z][A-Za-z0-9_']*)")

# A record field group inside a `{ … }`, anchored to the `{` or `,` that
# opens it. The anchor is load-bearing: unanchored, the second field of
# `{ a ∷ Int, b ∷ Bool }` is found by scanning forward from the previous
# `∷`, which starts inside the TYPE and reads `Int, b` as the field
# group `nt, b` -- inventing a definition named after a type's tail.
RECORD_FIELD_RE = re.compile(
    r"[{,]\s*([a-z_][A-Za-z0-9_']*(?:\s*,\s*[a-z_][A-Za-z0-9_']*)*)\s*(?:∷|::)")


@dataclass(frozen=True)
class Finding:
    """One dead link occurrence."""
    path: str
    line: int
    module: str
    symbol: str
    defined_in: tuple[str, ...]

    @property
    def link(self) -> str:
        return f"'{self.module}.{self.symbol}'"

    def __str__(self) -> str:
        # `defined_in` is never empty: a symbol with no definition
        # anywhere is not a candidate in the first place, which is what
        # keeps Lua verbs out. Naming the real owner is what makes the
        # fix mechanical -- eight of the baselined links name a module
        # that never held the function.
        return (f"{self.path}:{self.line}: {self.link} — {self.module} "
                f"does not export {self.symbol} "
                f"(defined in {', '.join(self.defined_in)})")


@dataclass(frozen=True)
class ImportSpec:
    """One `import` declaration, as the export rules need to read it."""
    module: str
    qualified: bool
    alias: str | None
    hiding: bool
    #: The explicit import list flattened to names, or `None` when the
    #: import carries no list at all.
    names: frozenset[str] | None
    #: The types the list names as `T(..)`, whose subordinates it covers
    #: without spelling any of them.
    open_types: frozenset[str]

    @property
    def export_name(self) -> str:
        """The name a `module …` export entry must use to mean this
        import.

        `import Alpha as A` puts names in scope as `e` and `A.e`, never
        `Alpha.e`, so it is `module A` that re-exports them (Haskell
        2010 §5.2)."""
        return self.alias or self.module


@dataclass
class ModuleFacts:
    """What one module's source says about its exports and definitions."""
    name: str
    path: str
    has_export_list: bool
    exported_names: set[str]
    #: `T` for each `T(..)`, so its constructors and fields resolve.
    exported_open_types: set[str]
    #: Modules named by a `module X` re-export in the export list.
    reexported_modules: set[str]
    #: Every `import` this module declares, restrictions included.
    imports: tuple[ImportSpec, ...]
    #: Every top-level function this module defines.
    defined: set[str]
    #: `T -> {field, …}` for each record declared here.
    record_fields: dict[str, set[str]]


def mask_spans(text: str, spans: list[tuple[int, int]]) -> str:
    """Blank `[start, end)` spans to NULs, one character for one, so
    every other offset -- and so every line number -- stays valid."""
    out = list(text)
    for start, end in spans:
        for i in range(start, end):
            if out[i] != "\n":
                out[i] = "\x00"
    return "".join(out)


def quasiquote_spans(text: str) -> list[tuple[int, int]]:
    """The `[name| … |]` spans of `text`.

    The OPENER is located only inside a genuine code span, so a comment
    that merely contains `[frag|`-shaped text can never be mistaken for
    a real quasiquote boundary. The CLOSER is then found in the RAW
    text, because a `--` or `{-` inside the quasiquoted body opens a
    phantom comment state in the shared scanner (it has no quasiquote
    state), which would hide the `|]` from a code-span-only search.

    One code-span view is not enough, which is why this masks and
    RESCANS rather than walking a single scan. A raw quasiquote body may
    legally contain an unmatched `{-` (or a lone `"`), and that leaves
    the shared scanner in a comment or string state for the whole rest
    of the file -- so a LATER quasiquote's opener is not inside any code
    span and would never be found. Masking each quote as it is
    discovered ends that phantom state at its source and resynchronises
    the next scan. Only files that actually contain a quasiquote pay for
    the extra passes."""
    spans: list[tuple[int, int]] = []
    masked = text
    search_from = 0
    while True:
        match = QUASIQUOTE_OPEN_RE.search(haskell_code_only(masked),
                                          search_from)
        if match is None:
            return spans
        close = masked.find("|]", match.end())
        if close == -1:
            # An unterminated opener is a list comprehension or a stray
            # bracket, not a quasiquote. Skip past it rather than
            # masking the rest of the file.
            search_from = match.end()
            continue
        span = (match.start(), close + 2)
        spans.append(span)
        # Positions are preserved one for one, so the next iteration's
        # offsets stay valid in the original text.
        masked = mask_spans(masked, [span])
        search_from = close + 2


def comment_text_spans(text: str) -> list[tuple[int, int]]:
    """The comment spans of `text`, with quasiquotes masked first."""
    return haskell_comment_spans(mask_spans(text, quasiquote_spans(text)))


def _sanitize(text: str) -> str:
    """Turn `haskell_code_only`'s comment/string placeholder NULs back
    into whitespace.

    Positions matter while the file is being CUT into export lists and
    declaration blocks, which is why the masker preserves them one for
    one. Once a fragment has been cut, its NULs are just an interleaved
    comment, and a regex looking for whitespace must see whitespace there --
    the export entry `( publishStagedSession\n  -- * heading\n , …` is
    one entry whose name is followed by NULs, not a name that happens to
    end in them."""
    return text.replace("\x00", " ")


def _top_level_blocks(code: str) -> list[str]:
    """Every declaration block of `code` -- a column-0 line plus every
    line up to the next one.

    ONLY a column-0 declaration ends a block. Neither a comment nor a
    blank line may, because both sit INSIDE real declarations all over
    this tree: a documented record puts a `-- ^` line between two
    fields, and a long signature or record is routinely broken by a
    blank one. Cutting there truncates the declaration before its
    closing brace, which loses the fields after the comment and makes an
    unexported one look like a name with no definition at all -- a
    silent false NEGATIVE, the direction a ratchet must never fail in.

    A column-0 comment does not start a block either: `haskell_code_only`
    masks it to NULs, and a comment never declares anything."""
    blocks: list[str] = []
    current: list[str] | None = None
    for line in code.splitlines():
        if line[:1].strip("\x00").strip():
            if current is not None:
                blocks.append("\n".join(current))
            current = [line]
        elif current is not None:
            current.append(line)
    if current is not None:
        blocks.append("\n".join(current))
    return blocks


def _split_names(names: str) -> list[str]:
    return [part.strip() for part in names.split(",") if part.strip()]


def _export_list(code: str) -> tuple[bool, str]:
    """`(has an explicit export list, its text)` for module source
    `code` (comments and strings already blanked)."""
    header = MODULE_HEADER_RE.search(code)
    if header is None:
        return False, ""
    i, n = header.end(), len(code)
    while i < n and code[i] in " \t\n\r\x00":
        i += 1
    if i >= n or code[i] != "(":
        return False, ""
    depth, start = 0, i
    while i < n:
        if code[i] == "(":
            depth += 1
        elif code[i] == ")":
            depth -= 1
            if depth == 0:
                return True, code[start + 1:i]
        i += 1
    return False, ""


def _split_export_entries(body: str) -> list[str]:
    """`body` split on its TOP-LEVEL commas, so `T(..)`'s own commas and
    an operator export's parentheses stay with their entry."""
    entries, depth, current = [], 0, []
    for char in body:
        if char == "(":
            depth += 1
        elif char == ")":
            depth -= 1
        if char == "," and depth == 0:
            entries.append("".join(current))
            current = []
        else:
            current.append(char)
    entries.append("".join(current))
    return [entry.strip() for entry in entries if entry.strip()]


def _parse_name_entry(entry: str) -> tuple[str, set[str], bool]:
    """One export- or import-list entry as `(head, subordinates, open)`.

    `T(..)` yields `("T", set(), True)`; `T(a, b)` yields its two named
    subordinates; `(<+>)` and `type T` yield the bare name."""
    # `type Foo`, `pattern Bar`, `data Baz(..)`: the namespace keyword is
    # not part of the name.
    entry = re.sub(r"\A(?:type|pattern|data)\s+", "", entry.strip())
    subordinate_text = ""
    paren = entry.find("(")
    if paren != -1 and not entry.startswith("("):
        subordinate_text = entry[paren + 1:].rstrip().rstrip(")")
        entry = entry[:paren]
    head = entry.strip().strip("()").strip()
    if subordinate_text.strip() == "..":
        return head, set(), True
    return head, {sub.strip("()").strip()
                  for sub in _split_names(subordinate_text)}, False


def _parse_import(block: str, head: re.Match[str]) -> ImportSpec:
    """One `import` declaration block.

    The list is read from the block rather than the line, because import
    lists routinely span several indented lines -- and a list read as
    absent would silently widen the import to everything the module
    exports."""
    rest = block[head.end():].lstrip()
    names: frozenset[str] | None = None
    open_types: set[str] = set()
    if rest.startswith("("):
        depth = 0
        for i, char in enumerate(rest):
            if char == "(":
                depth += 1
            elif char == ")":
                depth -= 1
                if depth == 0:
                    flat: set[str] = set()
                    for entry in _split_export_entries(rest[1:i]):
                        name, subordinates, is_open = _parse_name_entry(entry)
                        if name:
                            flat.add(name)
                        flat.update(subordinates)
                        if is_open and name:
                            # `T(..)` covers every field of T without
                            # naming one. Recorded as the TYPE, so
                            # `import_supplies` can expand it against
                            # the type's real fields -- and so `hiding
                            # (T(..))` hides them too, which a flattened
                            # `{T}` could express neither way round.
                            open_types.add(name)
                    names = frozenset(flat)
                    break
    return ImportSpec(module=head.group(2),
                      qualified=bool(head.group(1) or head.group(3)),
                      alias=head.group(4),
                      hiding=bool(head.group(5)),
                      names=names,
                      open_types=frozenset(open_types))


def parse_module(rel_path: str, text: str) -> ModuleFacts | None:
    """Everything the audit needs to know about one Haskell source."""
    code = haskell_code_only(text)
    header = MODULE_HEADER_RE.search(code)
    if header is None:
        return None
    name = header.group(1)

    has_list, body = _export_list(code)
    exported: set[str] = set()
    open_types: set[str] = set()
    reexports: set[str] = set()
    if has_list:
        for entry in _split_export_entries(_sanitize(body)):
            if entry.startswith("module "):
                reexports.add(entry[len("module "):].strip())
                continue
            head, subordinates, is_open = _parse_name_entry(entry)
            if head:
                exported.add(head)
                # A qualified re-export (`GLFW.setWindowSize`) exports
                # the name under its own last component.
                exported.add(head.rsplit(".", 1)[-1])
            if is_open:
                open_types.add(head)
            exported.update(subordinates)

    imports: list[ImportSpec] = []
    defined: set[str] = set()
    record_fields: dict[str, set[str]] = {}
    for block in map(_sanitize, _top_level_blocks(code)):
        head_match = IMPORT_HEAD_RE.match(block)
        if head_match is not None:
            imports.append(_parse_import(block, head_match))
            continue
        signature = SIGNATURE_RE.match(block)
        if signature is not None:
            defined.update(_split_names(signature.group(1)))
            continue
        data = DATA_HEADER_RE.match(block)
        if data is not None:
            fields = record_fields.setdefault(data.group(1), set())
            # The braces stay IN the slice: `RECORD_FIELD_RE` anchors
            # the first field on the opening `{`.
            for group in re.findall(r"\{.*?\}", block, re.DOTALL):
                for field_match in RECORD_FIELD_RE.finditer(group):
                    fields.update(_split_names(field_match.group(1)))
    return ModuleFacts(name, rel_path, has_list, exported, open_types,
                       reexports, tuple(imports), defined, record_fields)


def _haskell_sources(repo_root: Path) -> list[Path]:
    files: list[Path] = []
    for tree in SCANNED_TREES:
        files.extend(sorted((repo_root / tree).glob("**/*.hs")))
    return files


@dataclass(frozen=True)
class Index:
    """The whole tree, as the detection rule needs to see it."""
    #: Module name -> what its source says.
    modules: dict[str, ModuleFacts]
    #: Symbol -> the modules that define it. Record fields are folded in
    #: here, because a field IS a linkable definition -- but only for
    #: the "is this a real name at all" question. No EXPORT decision
    #: reads a tree-wide field pool: two modules may declare the same
    #: type name (three pairs do here), and one's fields must never
    #: satisfy the other's `T(..)`. `open_type_fields` resolves that per
    #: module instead.
    definitions: dict[str, set[str]]


def build_index(repo_root: Path) -> Index:
    modules: dict[str, ModuleFacts] = {}
    for path in _haskell_sources(repo_root):
        rel = path.relative_to(repo_root).as_posix()
        facts = parse_module(rel, path.read_text(encoding="utf-8"))
        if facts is not None:
            modules[facts.name] = facts
    definitions: dict[str, set[str]] = {}
    for facts in modules.values():
        for symbol in facts.defined:
            definitions.setdefault(symbol, set()).add(facts.name)
        for fields in facts.record_fields.values():
            for field in fields:
                definitions.setdefault(field, set()).add(facts.name)
    return Index(modules, definitions)


def _names_type(index: Index, module: str, type_name: str,
                seen: frozenset[str] = frozenset()) -> bool:
    """Does `module` make `type_name` available to an importer?"""
    if module in seen:
        return False
    facts = index.modules.get(module)
    if facts is None or not facts.has_export_list:
        return True
    if type_name in facts.exported_names:
        return True
    return any(re_export != module
               and _names_type(index, re_export, type_name, seen | {module})
               for re_export in facts.reexported_modules)


def open_type_fields(index: Index, module: str, type_name: str,
                     seen: frozenset[str] = frozenset()) -> set[str]:
    """The fields `module`'s `T(..)` export actually stands for.

    Resolved by following how `T` reached `module`, never by the
    tree-wide union of every type with that name: `Alpha`'s
    `Config(..)` must not be satisfied by an unrelated `Other.Config`'s
    fields. `module` itself first, then the modules it imports that
    make `T` available, recursively -- which is exactly the path that
    put `T` in scope. `Unit.Types` exports `UnitManager(..)` for a
    record declared in `Unit.Types.Manager`, and every such export in
    this tree resolves within two hops."""
    if module in seen:
        return set()
    facts = index.modules.get(module)
    if facts is None:
        return set()
    local = facts.record_fields.get(type_name)
    if local:
        return set(local)
    fields: set[str] = set()
    for spec in facts.imports:
        if spec.module in index.modules and _names_type(
                index, spec.module, type_name):
            fields |= open_type_fields(index, spec.module, type_name,
                                       seen | {module})
    return fields


def import_supplies(index: Index, spec: ImportSpec, symbol: str) -> bool:
    """Does `spec` bring `symbol` into scope UNQUALIFIED?

    This is the restriction the import declaration applies; whether the
    imported module exports the name at all is the caller's half. A
    `T(..)` entry is expanded against T's REAL fields, so a selected
    import of a type carries its selectors and a `hiding (T(..))`
    withholds them."""
    if spec.qualified:
        return False
    if spec.names is None:
        return True
    listed = symbol in spec.names or any(
        symbol in open_type_fields(index, spec.module, type_name)
        for type_name in spec.open_types)
    return (not listed) if spec.hiding else listed


def exports_symbol(index: Index, module: str, symbol: str,
                   seen: frozenset[str] = frozenset()) -> bool:
    """Does `module` export `symbol`?

    A module outside this tree, or one with no explicit export list, is
    treated as supplying anything asked of it -- unknowable and
    everything-exported respectively both mean the link may resolve, and
    this guard never reports a link it cannot prove dead."""
    if module in seen:
        return False
    modules = index.modules
    facts = modules.get(module)
    if facts is None or not facts.has_export_list:
        return True
    if symbol in facts.exported_names:
        return True
    for type_name in facts.exported_open_types:
        if symbol in open_type_fields(index, module, type_name):
            return True
    for reexported in facts.reexported_modules:
        if reexported == module:
            # `module M` in M's OWN export list names the entities in
            # scope under BOTH `e` and `M.e` (Haskell 2010 §5.2), which
            # is exactly M's own top-level declarations -- functions and
            # record fields alike. A name M merely imports is in scope
            # as `e` and `N.e`, never `M.e`, so it is NOT carried here;
            # `module N` is the entry that carries those, and it is
            # handled below. Engine.Core.State is the tree's canonical
            # case (`module Engine.Core.State, module
            # Engine.Core.Lifecycle`): every link into it names one of
            # its own definitions or an EngineEnv field.
            if symbol in facts.defined:
                return True
            if any(symbol in fields
                   for fields in facts.record_fields.values()):
                return True
            continue
        # `module N` carries only what M's own imports of N actually
        # brought into scope. `import Alpha (other)` beside
        # `module Alpha` re-exports `other` and nothing else, so
        # following Alpha's whole export surface here would launder a
        # dead link clean. The entry is matched on the name the IMPORT
        # made available -- its alias when it has one, since
        # `import Alpha as A` is re-exported by `module A`, not by
        # `module Alpha`.
        specs = [spec for spec in facts.imports
                 if spec.export_name == reexported]
        if specs:
            if any(import_supplies(index, spec, symbol)
                   and exports_symbol(index, spec.module, symbol,
                                      seen | {module})
                   for spec in specs):
                return True
            continue
        # No import made that name available. A bare alias is then not a
        # module at all, and must not be mistaken for an unknown
        # external one that could supply anything; only a real module of
        # this tree is followed. No re-export in this tree takes this
        # branch.
        if reexported in modules and exports_symbol(
                index, reexported, symbol, seen | {module}):
            return True
    return False


def find_findings(rel_path: str, text: str, index: Index) -> list[Finding]:
    """Every dead qualified link in one source file."""
    findings: list[Finding] = []
    for start, end in comment_text_spans(text):
        comment = text[start:end]
        for match in LINK_RE.finditer(comment):
            module, symbol = match.group(1), match.group(2)
            facts = index.modules.get(module)
            if facts is None or not facts.has_export_list:
                continue
            if exports_symbol(index, module, symbol):
                continue
            defining = index.definitions.get(symbol)
            if not defining:
                continue
            line = text.count("\n", 0, start + match.start()) + 1
            findings.append(Finding(rel_path, line, module, symbol,
                                    tuple(sorted(defining))))
    return findings


def scan_tree(repo_root: Path) -> list[Finding]:
    index = build_index(repo_root)
    findings: list[Finding] = []
    for path in _haskell_sources(repo_root):
        rel = path.relative_to(repo_root).as_posix()
        findings.extend(find_findings(
            rel, path.read_text(encoding="utf-8"), index))
    return findings


def baseline_key(finding: Finding) -> tuple[str, str]:
    """What the baseline records: the file and the link, never the line.

    A comment edit that moves a still-dead link must not churn the
    baseline; only fixing the link may."""
    return (finding.path, finding.link)


def load_baseline(path: Path) -> Counter[tuple[str, str]]:
    if not path.exists():
        return Counter()
    entries = json.loads(path.read_text(encoding="utf-8"))["entries"]
    return Counter((entry["path"], entry["link"]) for entry in entries)


def write_baseline(path: Path, findings: list[Finding]) -> None:
    """Regenerate the baseline. Duplicate occurrences are preserved as
    repeated entries, so a file that links the same dead name twice must
    fix both before its count drops."""
    entries = sorted((baseline_key(finding) for finding in findings))
    document = {
        "_comment": (
            "Generated by tools/haddock_link_audit.py --update-baseline; "
            "never hand-edited. A TEMPORARY ratchet over the dead links "
            "present when the audit landed (#2292), not a permanent "
            "exemption list: it may only shrink, and HLR-5 deletes it "
            "along with --update-baseline."),
        "entries": [{"path": path_, "link": link} for path_, link in entries],
    }
    path.write_text(json.dumps(document, indent=2, ensure_ascii=False) + "\n",
                    encoding="utf-8")


def main(argv: list[str] | None = None) -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument(
        "--update-baseline", action="store_true",
        help="regenerate the checked-in baseline from the current tree")
    parser.add_argument(
        "--repo-root", type=Path, default=REPO_ROOT,
        help=argparse.SUPPRESS)
    parser.add_argument(
        "--baseline", type=Path, default=None, help=argparse.SUPPRESS)
    args = parser.parse_args(argv)
    baseline_path = args.baseline if args.baseline is not None else BASELINE_PATH

    findings = scan_tree(args.repo_root)
    if args.update_baseline:
        write_baseline(baseline_path, findings)
        print(f"Wrote {len(findings)} baselined dead haddock link(s) to "
              f"{baseline_path}.")
        return 0

    baseline = load_baseline(baseline_path)
    live = Counter(baseline_key(finding) for finding in findings)
    new_keys = live - baseline
    stale_keys = baseline - live

    if new_keys:
        remaining = Counter(new_keys)
        print(f"{sum(new_keys.values())} dead haddock link(s) not in the "
              f"baseline:")
        for finding in findings:
            key = baseline_key(finding)
            if remaining[key] > 0:
                remaining[key] -= 1
                print(f"  {finding}")
    if stale_keys:
        print(f"{sum(stale_keys.values())} baseline entry/entries no longer "
              f"found — shrink the baseline in the same change:")
        for (path_, link), count in sorted(stale_keys.items()):
            print(f"  {path_}: {link}" + (f" (x{count})" if count > 1 else ""))
    if new_keys or stale_keys:
        print("\nFix a dead link by demoting it to a code span (@M.f@) or by "
              "pointing it at an exported entry point; never widen an export "
              "list (docs/haddock_link_resolution_design.md D-2). Then run "
              "`python3 tools/haddock_link_audit.py --update-baseline`.")
        return 1

    print(f"No new dead qualified haddock links "
          f"({sum(baseline.values())} still baselined).")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
