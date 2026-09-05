#!/usr/bin/env python3
"""The whole-tree scan and its reports for the SS5 writing-module
scanner (issue #1892, capability mutation-authority epic #1890 --
CMA-1; extracted from tools/engine_env_capability_writers.py by issue
#2230).

`scan_capability_writes` is ONE pass over the immutable
`{relative path: source text}` map the aggregate audit hands it, and
that single pass is a contract: raw `EngineEnv` accessors, capability
accessors, mutation-site classification and the pass-on residue are
all established together, never by a second walk. Around it sit the
requirement-6 mutation-site check (`audit_mutation_sites`) and the
non-blocking residue report (`format_residue`), whose deterministic
ordering the aggregate's output depends on.

This owner is the composition boundary of the family: it reads
authority constants, the syntax owner's tokenizer, import resolver and
mutation-expression reader, and the projection owner's accessor map,
so those two never need to know about each other. It imports nothing
from the facade. Dependencies run one way: authority, then syntax and
projections over it, then scan over all three.
"""
from __future__ import annotations

from typing import NamedTuple

import engine_env_capability_writer_authority as authority  # type: ignore
import engine_env_capability_writer_projections as projections  # type: ignore
import engine_env_capability_writer_syntax as syntax  # type: ignore
from engine_env_capability_common import (  # type: ignore
    ENGINE_ENV_TYPE, INVENTORY_PATH, PERMANENT_DEFINER, PERMANENT_IMPORTERS,
    STATE_MODULE, module_identifier,
)


class Occurrence(NamedTuple):
    """One capability-accessor use the direct-write scan cannot
    attribute. Ordered path-first so the report is deterministic."""
    relpath: str
    line: int
    accessor: str
    field: str
    module: str


class WriteScan(NamedTuple):
    """Everything one pass over the production tree establishes."""
    writes: dict[str, set[str]]
    residue: list[Occurrence]
    sites: list[syntax.MutationSite]
    suppressed: frozenset[tuple[str, str]]


def scan_capability_writes(
    sources: dict[str, str], live_fields: list[str], *,
    permanent: frozenset[str] = PERMANENT_IMPORTERS,
    definer: str = PERMANENT_DEFINER,
    exemptions: dict[tuple[str, str], str] | None = None,
) -> WriteScan:
    """Pure core of the CMA-1 scan.

    Both a RAW `EngineEnv` accessor (a narrow-import consumer) and a
    CAPABILITY-record accessor canonicalize onto the same `EngineEnv`
    field, so the two consumer shapes are one boundary. Accessor AND
    mutation primitive are each recognized qualified (`State.fieldOne`,
    `Ref.writeIORef`) as readily as bare.

    Two rules decide an attribution, and neither models Haskell's
    binding forms: the identifier must be in scope in that module under
    the exact spelling used (`parse_imports`/`imports_name`), and it
    must head an APPLIED argument of the primitive -- the first argument
    of a prefix application (`_first_argument_head`), or the left
    operand of a backticked infix one (`_infix_left_operand_head`).
    `SHADOW_EXEMPTIONS` covers the residue of that: a module that binds
    a name matching an accessor AND applies it to a handle.

    EVERY mutation-primitive occurrence is classified exactly once
    (`classify_mutation_site`), and a site whose argument the scan
    cannot read is recorded as `unclassifiable` for `main` to fail on --
    requirement 6, and what keeps the recognized-form list closed.

    `permanent`/`definer` are SS6.1's cohort (D-4), excluded from the
    write map -- their authority is not what this boundary constrains.
    They are parameters, like `exemptions`, so the self-test can drive
    small synthetic fixtures instead of the real ~200-module tree.

    The residue is every remaining CAPABILITY-accessor use -- a helper
    argument, a context-record field, a queue/`TVar`/`MVar` handle, a
    point-free composition -- i.e. exactly what the write scan cannot
    attribute (D-5). A direct `readIORef` application to a known
    accessor is an inline READ, not a pass-on. Occurrences are counted
    individually, never deduplicated to field/module pairs. An
    accessor's own defining capability module is excluded, because its
    record declaration, export list and projection are declarations
    rather than uses."""
    exempt = set(permanent) | {definer}
    shadows = authority.SHADOW_EXEMPTIONS if exemptions is None else exemptions
    accessors = projections.capability_accessor_map(sources, live_fields)
    raw_fields = set(live_fields)

    writes: dict[str, set[str]] = {field: set() for field in live_fields}
    residue: list[Occurrence] = []
    sites: list[syntax.MutationSite] = []
    suppressed: set[tuple[str, str]] = set()

    for relpath, text in sorted(sources.items()):
        module = module_identifier(relpath)
        declarations = syntax.parse_imports(text)
        code = syntax.prepared_source(text)
        tokens = syntax.tokenize_haskell(code)
        indents = syntax.line_indents(code)

        def resolve(name: str) -> tuple[str, str, str] | None:
            """`(EngineEnv field, owning module, base accessor name)` for
            an occurrence spelled `name` here, or `None` when it names no
            accessor this module can reach under that exact spelling --
            `Other.fieldOne` is not this field, and neither is a bare
            `fieldOne` in a module that imports the owner `qualified` or
            `hiding` it."""
            qualifier, _, base = name.rpartition(".")
            if base in raw_fields:
                owners: tuple[tuple[str, str, str], ...] = (
                    (base, STATE_MODULE, ENGINE_ENV_TYPE),)
            else:
                owners = accessors.get(base, ())
                if not owners:
                    return None
            # One selector name can belong to several capability
            # records; the module's own imports say which one it means,
            # so every candidate is offered the scope test rather than
            # the first arbitrarily winning.
            for field, owner, record in owners:
                if not qualifier and module == owner:
                    return field, owner, base
                if syntax.imports_name(declarations, owner, base, qualifier, record):
                    return field, owner, base
            return None

        inline_heads: set[int] = set()
        for index, token in enumerate(tokens):
            if token.kind != "id":
                continue
            # A mutation primitive is just as much itself under a
            # qualifier (`Ref.writeIORef`, from
            # `import qualified Data.IORef as Ref`), and missing one
            # would be a SILENT hole in the gate -- but it must be the
            # `Data.IORef` one, resolved through this module's own
            # imports, or a local homonym would fabricate a write.
            primitive = syntax.resolve_primitive(declarations, token.text)
            if primitive is None:
                continue
            if not syntax.in_head_position(tokens, index, indents):
                # Being passed on, not applied: no inline use to record,
                # and the accessor beside it stays residue. Unless what
                # precedes it is an operator SECTION, which may well be
                # applying it -- unreadable either way, so it blocks.
                if primitive in authority.IOREF_WRITE_PRIMITIVES:
                    sites.append(syntax.MutationSite(
                        relpath, token.line, module,
                        "unclassifiable"
                        if syntax.after_operator_section(tokens, index) else "other",
                        None))
                continue
            head = syntax._first_argument_head(tokens, index)
            if head is None:
                head = syntax._infix_left_operand_head(tokens, index)
            if head is not None:
                inline_heads.add(head)
            if primitive not in authority.IOREF_WRITE_PRIMITIVES:
                continue

            kind, candidate = syntax.classify_mutation_site(tokens, index)
            if kind == "unclassifiable":
                sites.append(syntax.MutationSite(
                    relpath, token.line, module, "unclassifiable", None))
                continue
            field = None
            if kind == "applied" and candidate is not None:
                resolved = resolve(tokens[candidate].text)
                if resolved is not None:
                    field = resolved[0]
            if field is None or module in exempt:
                sites.append(
                    syntax.MutationSite(relpath, token.line, module, "other", field))
                continue
            if (module, field) in shadows:
                suppressed.add((module, field))
                sites.append(
                    syntax.MutationSite(relpath, token.line, module, "other", field))
                continue
            writes[field].add(module)
            sites.append(
                syntax.MutationSite(relpath, token.line, module, "write", field))

        for index, token in enumerate(tokens):
            if token.kind != "id":
                continue
            resolved = resolve(token.text)
            if resolved is None:
                continue
            field, owner, base = resolved
            if (not owner.startswith(authority.CAPABILITY_MODULE_PREFIX)
                    or module == owner or index in inline_heads):
                continue
            residue.append(
                Occurrence(relpath, token.line, base, field, module))

    residue.sort()
    sites.sort()
    return WriteScan(writes, residue, sites, frozenset(suppressed))


def audit_mutation_sites(sites: list[syntax.MutationSite]) -> list[str]:
    """Requirement 6: no mutation-primitive occurrence may go
    unclassified.

    This is what makes the recognized-form list in
    docs/engineenv_capability_inventory.md SS6.5 a CLOSED set. Without
    it, a spelling the scan does not model -- a new operator, an
    unfamiliar grouping -- silently drops the write and the map keeps
    claiming a guarantee it no longer provides. With it, the gate stops
    and names the site instead."""
    return [
        f"{site.relpath}:{site.line} mutates an `IORef` through an "
        f"expression this audit cannot read -- every mutation site must "
        f"classify (docs/{INVENTORY_PATH.name} SS6.5's recognized write "
        f"forms). Extend the scan and that list together, or restate the "
        f"site in a recognized form; do NOT leave it unread, because an "
        f"unread site is an unenforced field"
        for site in sites if site.kind == "unclassifiable"]


def format_residue(residue: list[Occurrence]) -> list[str]:
    """The non-blocking pass-on report (D-5), one line per SOURCE
    OCCURRENCE -- never deduplicated, never resolved to an originating
    module. This count is the evidence CMA-2's pilot and CMA-3's verdict
    both turn on: a small residue means a textual gate is nearly
    sufficient, a large one argues for a mechanism that travels with the
    handle. It is printed on EVERY run, ahead of every blocking check,
    so a failure elsewhere never costs the measurement."""
    lines = [
        f"capability-accessor pass-on residue: {len(residue)} use(s) the "
        f"direct-write scan cannot attribute (non-blocking, reported not "
        f"resolved -- design decision D-5):"
    ]
    lines.extend(
        f"  - {item.relpath}:{item.line} `{item.accessor}` "
        f"(-> `{item.field}`) in `{item.module}`"
        for item in residue)
    return lines
