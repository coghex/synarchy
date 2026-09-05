#!/usr/bin/env python3
"""Capability-record and projection discovery for the SS5
writing-module scanner (issue #2059's fail-closed accessor map;
extracted from tools/engine_env_capability_writers.py by issue #2230).

One discovery feeds both the accessor map the scan attributes writes
through and the completeness gate that blocks on a projection this
audit cannot read, which is why they live together: the original hole
was a map that silently described a smaller set of records than
anything checked. This owner recognizes a capability declaration by
its name and `data`/`newtype` keyword alone, reads its record block,
canonicalizes every projection's right-hand side onto a live
`EngineEnv` accessor, and FAILS -- by module, projection and field --
on any binding it cannot read.

It reads `CAPABILITY_MODULE_PREFIX` from
`engine_env_capability_writer_authority` and the projection parsing and
canonicalization helpers from `engine_env_capability_common`; it calls
no syntax function, which is what keeps syntax and projection
discovery free of a cycle, and it imports nothing from the facade.
`engine_env_capability_writer_scan` is the composition boundary
between the two.
"""
from __future__ import annotations

import re
from typing import NamedTuple

import engine_env_capability_writer_authority as authority  # type: ignore
from engine_env_capability_common import (  # type: ignore
    ALIAS_PRESERVING_WRAPPERS, _strip_haskell_comments,
    canonical_projection_accessor, extract_record_fields, module_identifier,
    parse_projection_binding_expressions, parse_projection_bindings,
)


# One discovery feeds BOTH the accessor map and the completeness gate
# below (issue #2059), so the map can never quietly describe a smaller
# set of records than the gate checks -- which is the shape the
# original hole took: `capability_accessor_map` was the only thing
# that read the projections, and anything it failed to read simply was
# not there.
#
# A capability type is recognized by its NAME and its `data`/`newtype`
# keyword alone, never by the shape of its body. GHC2024 enables
# `GADTs`, so `data XCapability where XCapability ∷ { ... } → XCapability`
# is a legal respelling of the same record, and a `newtype` is legal
# for a one-field one -- matching only `data X = X { ... }` left both
# undiscovered, which is the SAME silent omission one level up:
# neither the accessor map nor the completeness gate saw the record at
# all, so a direct write through its selector was filed as `other` and
# the audit exited 0. Recognizing the declaration is therefore
# separated from reading its fields: a capability type whose record
# block this audit cannot read is a violation, not a skip.
# Any layout COLUMN, because a module's body need not start at column
# zero, but only the two plain declaration keywords: a `data`/`newtype`
# `instance` or `family` naming a capability type is deliberately NOT
# read here. It is a form SS2.1's convention does not describe, so the
# backstop below reports it loudly instead -- "detect and fail" rather
# than a modelling branch nothing in the tree exercises.
_CAPABILITY_TYPE_DECL_PATTERN = (
    r"^[ \t]*(?:data|newtype)\s+%s(?![A-Za-z0-9_'])")
_CAPABILITY_TYPE_DECL_RE = re.compile(
    _CAPABILITY_TYPE_DECL_PATTERN
    % r"(?P<record>[A-Z][A-Za-z0-9_']*Capability)", re.MULTILINE)

# The fail-closed BACKSTOP for this whole discovery. Everything above
# recognizes the spellings this audit models; this recognizes that a
# capability type was DECLARED at all, by looking only for the
# `data`/`newtype` keyword and a `<Name>Capability` type name in the
# same declaration head. `[^\n=]` keeps it inside that head, so a field
# whose TYPE is a capability (`{ x ∷ RenderCapability }`, after the
# `=` or on a later line) is never mistaken for a declaration of one.
#
# Any loose match the strict pattern did not produce is a form this
# audit cannot read, and it is reported rather than skipped. That is
# what makes the discovery closed the way SS6.5's recognized write
# forms are closed: the NEXT unmodelled spelling -- whatever it turns
# out to be -- fails loudly instead of quietly taking a record out of
# the accessor map, so no legal respelling can leave a selector
# unenforced while the gate exits 0.
_LOOSE_CAPABILITY_DECL_RE = re.compile(
    r"(?<![A-Za-z0-9_'])(?:data|newtype)(?![A-Za-z0-9_'])[^\n=]{0,160}?"
    r"(?<![A-Za-z0-9_'])(?P<record>[A-Z][A-Za-z0-9_']*Capability)"
    r"(?![A-Za-z0-9_'])")


def _declaration_span(code: str, start: int) -> str:
    """`code` from the start of `start`'s LINE through the end of that
    declaration: its own line plus every following line that is blank
    or indented strictly PAST the declaration's own layout column,
    which is Haskell's layout rule for one item of a block.

    The column is read from the declaration rather than assumed to be
    zero, because a module whose body is uniformly indented puts every
    top-level declaration at the same non-zero column -- and treating
    column zero as the boundary there would run one declaration's span
    to the end of the file.

    Field extraction is bounded to this span so a declaration carrying
    no record block of its own cannot borrow the braces of a LATER
    declaration and report that one's fields as its own."""
    line_start = code.rfind("\n", 0, start) + 1
    lines = code[line_start:].split("\n")
    # The column is the declaration KEYWORD's, not the match's: the
    # pattern anchors before the leading whitespace, so measuring from
    # `start` would read every indented declaration as column zero and
    # run its span to the end of the file.
    column = len(lines[0]) - len(lines[0].lstrip())
    span = [lines[0]]
    for line in lines[1:]:
        if line.strip() and len(line) - len(line.lstrip()) <= column:
            break
        span.append(line)
    return "\n".join(span)


# Every field-carrying constructor's block is read, not just the
# first. A sum of records -- `data X = A { f ∷ … } | B { f ∷ …, g ∷ … }`,
# or a GADT declaring one record constructor per line -- puts EVERY
# constructor's selectors in one scope, so reading only the first block
# left `g` unenumerated and therefore unchecked: the completeness gate
# had nothing to say about it, and a projection binding it through
# anything this audit cannot read took it out of the accessor map
# silently, which is the exact failure mode #2059 exists to close.
#
# The shared field parser is CALLED per block rather than paraphrased,
# so grouped declarations (`{ a, b ∷ Int }`) and split
# name/signature lines keep behaving identically to every other record
# this tree parses.
_RECORD_BLOCK_HEAD = "data CapabilityRecordBlock = CapabilityRecordBlock "
_RECORD_BLOCK_PATTERN = (
    r"^data CapabilityRecordBlock = CapabilityRecordBlock\b")


def _record_blocks(span: str) -> list[str]:
    """Every top-level `{ ... }` block in one declaration's span, in
    source order -- one per field-carrying constructor."""
    blocks: list[str] = []
    depth = 0
    start = 0
    for index, character in enumerate(span):
        if character == "{":
            if depth == 0:
                start = index
            depth += 1
        elif character == "}" and depth > 0:
            depth -= 1
            if depth == 0:
                blocks.append(span[start:index + 1])
    return blocks


def capability_record_fields(source_text: str, record: str) -> list[str]:
    """The field names `record`'s own declaration brings into scope,
    whichever legal syntax declares it -- `data X = X { ... }`,
    `newtype X = X { ... }`, the GADT `data X where X ∷ { ... } → X`,
    or a SUM of record constructors. All of them put the same kind of
    selector in scope, so all of them must be read; a name declared by
    more than one constructor is one selector and is reported once, in
    first-declaration order.

    Raises `ValueError` when the declaration is absent or carries no
    record block at all, which the completeness audit reports rather
    than treating as a record with no fields."""
    code = _strip_haskell_comments(source_text)
    declaration = _CAPABILITY_TYPE_DECL_RE.search(code)
    while declaration is not None and declaration.group("record") != record:
        declaration = _CAPABILITY_TYPE_DECL_RE.search(code, declaration.end())
    if declaration is None:
        raise ValueError(
            f"no `data` or `newtype` declaration of `{record}` was found")
    blocks = _record_blocks(_declaration_span(code, declaration.start()))
    if not blocks:
        raise ValueError(
            f"`{record}`'s declaration carries no record block of its "
            f"own")
    fields: list[str] = []
    for block in blocks:
        for field in extract_record_fields(_RECORD_BLOCK_HEAD + block,
                                           _RECORD_BLOCK_PATTERN):
            if field not in fields:
                fields.append(field)
    return fields


def _capability_projection_re(record: str) -> re.Pattern[str]:
    """`to<Something> ∷ EngineEnv → <record>`, the SS2.1 projection
    signature, ASCII and Unicode arrows alike."""
    return re.compile(
        r"^[ \t]*(to[A-Za-z0-9_']*)\s*(?:∷|::)\s*"
        r"(?:[A-Z][A-Za-z0-9_']*\.)*EngineEnv\s*(?:→|->)\s*"
        rf"{re.escape(record)}(?![A-Za-z0-9_'])", re.MULTILINE)


class CapabilityRecord(NamedTuple):
    """One `Engine.Core.Capability.*` record declaration and the
    projection that builds it. `projection` is `None` when the module
    declares the record but no `EngineEnv → <record>` signature was
    found -- a state that is itself a violation, never a skip."""
    module: str
    relpath: str
    record: str
    projection: str | None


def discover_capability_records(sources: dict[str, str]
                                ) -> list[CapabilityRecord]:
    """Every `<Name>Capability` record declared under
    `Engine.Core.Capability.*`, paired with its projection, in module
    then declaration order.

    Comments are stripped first, so a Haddock example showing a record
    or a signature is not mistaken for the real declaration.

    A declaration this pattern cannot read is NOT here -- it is
    reported by `undiscovered_capability_declarations`, which the
    completeness audit fails on. Read the two together: this answers
    "what did we understand?", that one answers "did we understand
    everything?", and only the pair is fail-closed."""
    records: list[CapabilityRecord] = []
    for relpath, text in sorted(sources.items()):
        module = module_identifier(relpath)
        if not module.startswith(authority.CAPABILITY_MODULE_PREFIX):
            continue
        code = _strip_haskell_comments(text)
        for declaration in _CAPABILITY_TYPE_DECL_RE.finditer(code):
            record = declaration.group("record")
            signature = _capability_projection_re(record).search(code)
            records.append(CapabilityRecord(
                module, relpath, record,
                signature.group(1) if signature else None))
    return records


def undiscovered_capability_declarations(sources: dict[str, str]
                                         ) -> list[tuple[str, str, str]]:
    """`(module, relpath, record)` for every capability type a
    `data`/`newtype` declaration head names that
    `discover_capability_records` did NOT produce.

    This is the backstop that makes the discovery a CLOSED set rather
    than a list of spellings that happened to be thought of. Every hole
    #2059 has closed had the same shape -- a legal declaration the
    pattern did not match, so the record reached neither the accessor
    map nor the completeness gate and a direct write through its
    selector was filed as `other` while the audit exited 0. Naming the
    keyword and the type is enough to know a capability record is
    THERE; whether this audit can read its fields is a separate
    question, and the honest answer to "no" is to fail."""
    missed: list[tuple[str, str, str]] = []
    discovered = {(entry.relpath, entry.record)
                  for entry in discover_capability_records(sources)}
    for relpath, text in sorted(sources.items()):
        module = module_identifier(relpath)
        if not module.startswith(authority.CAPABILITY_MODULE_PREFIX):
            continue
        code = _strip_haskell_comments(text)
        seen: set[str] = set()
        for match in _LOOSE_CAPABILITY_DECL_RE.finditer(code):
            record = match.group("record")
            if record in seen or (relpath, record) in discovered:
                continue
            seen.add(record)
            missed.append((module, relpath, record))
    return missed


def capability_accessor_map(sources: dict[str, str], live_fields: list[str]
                            ) -> dict[str, tuple[tuple[str, str, str], ...]]:
    """`{capability accessor: ((field, defining module, record type), ...)}`
    for every `Engine.Core.Capability.*` record, derived from the LIVE
    projections (`parse_projection_bindings`) rather than a second
    checked-in list, so this canonicalization cannot drift from the
    records it describes.

    Each accessor maps to a TUPLE of candidates, not one, because a
    selector name is only unique within its own module: two capability
    records may both export `sharedRef`, and a consumer that imports one
    of them qualified is writing THAT one's field. Collapsing them would
    let the wrong owner win the scope test and drop a real write. The
    candidates are sorted by owner, so resolution is deterministic.

    Duplicate full/view accessors resolve independently: `Render`'s
    `rcVideoConfigRef` and `RenderView`'s `rvVideoConfigRef` are separate
    keys canonicalizing onto the same `videoConfigRef` field. A binding
    whose right-hand side is not a live `EngineEnv` field is skipped
    rather than invented -- `audit_save_load_projection` and the
    boundary checks are where a mis-bound projection is caught, and
    `audit_capability_projection_completeness` is what stops such a
    skip from being SILENT."""
    fields = set(live_fields)
    candidates: dict[str, set[tuple[str, str, str]]] = {}
    for entry in discover_capability_records(sources):
        if entry.projection is None:
            continue
        for capability_field, accessor in parse_projection_bindings(
                sources[entry.relpath], entry.projection).items():
            if accessor in fields:
                candidates.setdefault(capability_field, set()).add(
                    (accessor, entry.module, entry.record))
    return {name: tuple(sorted(owners, key=lambda entry: entry[1]))
            for name, owners in candidates.items()}


def audit_capability_projection_completeness(
    sources: dict[str, str], live_fields: list[str],
) -> list[str]:
    """Issue #2059's fail-closed half: every field of every live
    capability record must canonicalize onto a live `EngineEnv` field,
    or the audit STOPS and names the module, projection and field.

    `capability_accessor_map` is the whole ownership map behind
    `CAPABILITY_WRITER_MODULES` enforcement, the SS6.5 residue and
    requirement 6's closed-form safety check. Before this check, a
    field the parser could not read was simply absent from that map:
    every direct write through the selector resolved to no field, was
    filed as `other`, and disappeared from all three while the gate
    exited 0. A silent omission is therefore indistinguishable from a
    field nobody writes -- so there must be none, and each of the three
    ways one can arise is reported here:

    * the record's DECLARATION is in a form the discovery pattern
      cannot read, which loses the record entirely
      (`undiscovered_capability_declarations`);
    * the record's projection signature is not found at all, which
      loses every one of its fields at once;
    * a declared field has no binding the canonicalizer can read
      (`canonical_projection_accessor`) -- an unrecognized wrapper, an
      operator, a record update, or no binding in the construction;
    * a binding canonicalizes onto a name that is NOT a live
      `EngineEnv` field, which `capability_accessor_map` discards at
      the same cost (the reviewer's amendment to requirement 2).

    This does not widen what canonicalizes. Reading MORE spellings is
    `canonical_projection_accessor`'s job and stays deliberately
    bounded; this check only refuses to let an unread one pass as
    nothing."""
    fields = set(live_fields)
    violations: list[str] = []
    for module, relpath, record in undiscovered_capability_declarations(
            sources):
        violations.append(
            f"`{module}` declares `{record}` in a form this audit cannot "
            f"read ({relpath}) -- the `data`/`newtype` declaration is "
            f"there, but `discover_capability_records` did not produce "
            f"the record, so it reaches neither the capability accessor "
            f"map nor the checks below and every direct write through "
            f"one of its selectors would be filed as `other`. Teach "
            f"`_CAPABILITY_TYPE_DECL_PATTERN` the spelling, or restate "
            f"the declaration in one it reads; do NOT leave it "
            f"undiscovered, because an undiscovered record is an "
            f"unenforced one")
    for entry in discover_capability_records(sources):
        source = sources[entry.relpath]
        if entry.projection is None:
            violations.append(
                f"`{entry.module}` declares `{entry.record}` but no "
                f"`to... ∷ EngineEnv → {entry.record}` projection was "
                f"found ({entry.relpath}) -- without it EVERY selector of "
                f"the record is absent from the capability accessor map, "
                f"so every direct write through one is filed as `other` "
                f"and silently leaves SS5 writing-module enforcement. Give "
                f"the record its SS2.1 projection, or teach "
                f"`discover_capability_records` the spelling")
            continue
        try:
            declared = capability_record_fields(source, entry.record)
        except ValueError as error:
            violations.append(
                f"`{entry.module}`'s `{entry.record}` declares no record "
                f"block this audit can read ({entry.relpath}): {error} -- "
                f"SS2.1 requires a record whose every field projects an "
                f"`EngineEnv` handle, and a declaration whose selectors "
                f"cannot be enumerated puts every one of them outside "
                f"SS5's writing-module map")
            continue
        expressions = parse_projection_binding_expressions(
            source, entry.projection)
        for field in declared:
            expression = expressions.get(field)
            if expression is None:
                violations.append(
                    f"`{entry.module}`'s `{entry.projection}` binds no "
                    f"readable right-hand side for `{entry.record}`'s "
                    f"`{field}` ({entry.relpath}) -- a field the parser "
                    f"cannot pair with an `EngineEnv` accessor is missing "
                    f"from the capability accessor map, which silently "
                    f"exempts every direct write through `{field}` from "
                    f"SS5's writing-module map")
                continue
            accessor = canonical_projection_accessor(expression)
            if accessor is None:
                violations.append(
                    f"`{entry.module}`'s `{entry.projection}` binds "
                    f"`{field}` as `{expression}`, which this audit "
                    f"cannot canonicalize onto an `EngineEnv` accessor "
                    f"({entry.relpath}) -- SS2.1 requires every field to "
                    f"be the live handle an accessor names, spelled "
                    f"`accessor env` or `wrapper (accessor env)` for a "
                    f"named alias-preserving wrapper "
                    f"({', '.join(sorted(ALIAS_PRESERVING_WRAPPERS))}), "
                    f"with grouping optional. Restate the binding in a "
                    f"recognized form, or extend "
                    f"`canonical_projection_accessor` and SS2.1 together; "
                    f"do NOT leave it unread, because an unread binding "
                    f"is an unenforced field")
                continue
            if accessor not in fields:
                violations.append(
                    f"`{entry.module}`'s `{entry.projection}` binds "
                    f"`{field}` from `{accessor}`, which is not a live "
                    f"`EngineEnv` field ({entry.relpath}) -- "
                    f"`capability_accessor_map` drops a binding it cannot "
                    f"canonicalize onto the live record, so a renamed or "
                    f"mistyped accessor would take `{field}` out of SS5's "
                    f"writing-module map without failing anything")
    return violations
