#!/usr/bin/env python3
"""SS5 writing-module scanner for the EngineEnv capability audit
(issue #1892, capability mutation-authority epic #1890 -- CMA-1;
extracted from tools/engine_env_capability_audit.py by issue #2036;
split across four implementation owners by issue #2230).

`tools/engine_env_capability_audit.py` remains the gate CI and
`tools/ci-local.sh` run; this module is the half of it that pins each
`EngineEnv` field's direct WRITING MODULES, and it is reached only
through that aggregate. SS5's Writers cells are prose validated for
grammar and citation presence only, so until this check the doc could
claim a field has no writers at all and a change falsifying it passed
every gate -- the drift class #1669 closed for the field COUNT, still
open for the ownership CLAIMS. `CAPABILITY_WRITER_MODULES` is a
checked-in, both-directions map of the same shape as the
structural-boundary owner's `RENDER_MAIN_ONLY_MODULES`
(tools/engine_env_capability_boundaries.py since issue #2064): an
undeclared write fails, a stale entry fails, and the map's KEYS must
equal the live field set. It scans
DIRECT `IORef` mutation only, through the raw `EngineEnv` accessor and
through any capability-record accessor projecting it alike; SS6.1's
permanent cohort is exempt (design decision D-4); and every
capability-accessor use the scan cannot attribute -- a handle passed
to a helper, stored in a context record, or handed to a
queue/`TVar`/`MVar` -- is printed as the non-blocking pass-on residue
(D-5), which the aggregate prints ahead of every blocking check so a
failure elsewhere never costs the measurement. See SS6.5 of the
inventory doc and docs/capability_mutation_authority_design.md.

Since issue #2059 the ownership map behind that scan is also derived
FAIL-CLOSED. The map is built entirely from the live projections, so
anything the parser failed to read simply was not in it -- and a write
through the missing selector then resolved to no field, was filed as
`other`, and left the writing-module map, the residue and requirement
6's closed-form check while the gate still exited 0. Two changes close
that: projection right-hand sides are canonicalized STRUCTURALLY
(`canonical_projection_accessor`, in
tools/engine_env_capability_common.py), so semantically inert grouping
-- `(accessor env)`, `(accessor) env`, `wrapper ((accessor env))` --
canonicalizes exactly as its ungrouped spelling does; and
`audit_capability_projection_completeness` requires every field of
every live capability record to reach a live `EngineEnv` accessor,
naming the module, projection and field when one does not. Widening
what canonicalizes stays deliberately bounded (no dataflow, type or
scope analysis); what changed is that an unread binding now FAILS
instead of disappearing.

What this module is, since #2230
--------------------------------
Documentation and RE-EXPORTS. The scanner's whole surface stays
importable from this path, bound to the object its owner defines, and
this is still one production module from the aggregate's point of
view; the implementation lives with four one-way owners:

  `engine_env_capability_writer_authority.py`    the recognized
      mutation and read primitives and the modules they must come
      from, `CAPABILITY_MODULE_PREFIX`, and the two checked-in
      authorities `SHADOW_EXEMPTIONS` and `CAPABILITY_WRITER_MODULES`
      together with the two blocking checks that read them
      (`audit_shadow_exemptions`, `audit_writer_modules`);
  `engine_env_capability_writer_syntax.py`       the Haskell
      tokenizer, the import resolver (`tokenize_haskell`,
      `parse_imports`, `imports_name`, `resolve_primitive`) and the
      mutation-expression reader whose four-way outcome
      (`classify_mutation_site`) makes the recognized-form list a
      closed set;
  `engine_env_capability_writer_projections.py`  capability-record
      discovery, accessor canonicalization
      (`discover_capability_records`, `capability_accessor_map`) and
      #2059's fail-closed completeness audit;
  `engine_env_capability_writer_scan.py`         the single pass over
      the production tree (`scan_capability_writes`), the
      requirement-6 mutation-site check (`audit_mutation_sites`) and
      the pass-on residue report (`format_residue`).

Dependencies run one way and no owner imports this facade: authority
first, syntax and projections over it, scan over all three -- so the
scan owner, not an import edge, is the composition boundary between
syntax and projection discovery.

What the family READS from tools/engine_env_capability_common.py, and
does not own: the inventory-doc anchor, SS6.1's permanent set, comment
stripping, import chunking, module naming and the projection
canonicalizer. Nothing here imports the aggregate.

Every function is pure over the `{relpath: source_text}` map and the
ordered live-field list the aggregate hands it, so the focused
self-test (tools/test_engine_env_capability_writers.py, split across
case owners by issue #2228) drives it against synthetic trees through
this facade, and the production tree is scanned exactly once per audit
run, by the aggregate.

Not independently a gate: `python3 tools/engine_env_capability_audit.py`
is the one command that runs this, and adding a second invocation to
CI or `tools/ci-local.sh` would fail tools/ci_parity_audit.py's
command-set comparison for nothing.
"""
from __future__ import annotations

# ----- Re-exported scanner surface -----------------------------------------
#
# Every name the scanner exposed before the #2230 split stays importable
# from `engine_env_capability_writers`, bound to the same object its
# owner defines -- the public functions and result types, the checked-in
# authority data and primitive constants, and the underscored syntax
# helpers the focused self-test drives directly. Nothing outside this
# family imports an owner, and no owner imports this module.
from engine_env_capability_writer_authority import (  # noqa: F401
    ACCESS_PRIMITIVE_MODULES, CAPABILITY_MODULE_PREFIX,
    CAPABILITY_WRITER_MODULES, IOREF_ACCESS_PRIMITIVES, IOREF_MODULE,
    IOREF_READ_PRIMITIVES, IOREF_WRITE_PRIMITIVES, READ_ONLY_REF_MODULE,
    READ_ONLY_REF_READ_PRIMITIVES, SHADOW_EXEMPTIONS,
    WRITER_FACADE_FILENAME, audit_shadow_exemptions, audit_writer_modules,
)
from engine_env_capability_writer_projections import (  # noqa: F401
    CapabilityRecord, _RECORD_BLOCK_HEAD, _RECORD_BLOCK_PATTERN,
    _CAPABILITY_TYPE_DECL_PATTERN, _CAPABILITY_TYPE_DECL_RE,
    _LOOSE_CAPABILITY_DECL_RE, _capability_projection_re, _declaration_span,
    _record_blocks, audit_capability_projection_completeness,
    capability_accessor_map, capability_record_fields,
    discover_capability_records, undiscovered_capability_declarations,
)
from engine_env_capability_writer_scan import (  # noqa: F401
    Occurrence, WriteScan, audit_mutation_sites, format_residue,
    scan_capability_writes,
)
from engine_env_capability_writer_syntax import (  # noqa: F401
    _HASKELL_KEYWORDS, _HS_IDENT_RE, _IMPORT_DECL_RE, _IMPORT_HIDING_RE,
    _IMPORT_WILDCARD_RE, ImportDecl, MutationSite, Token, _applied_head,
    _first_argument_head, _infix_left_operand_head, _opens_record_dot,
    _operand_head, _past_primitive_parentheses, _skip_type_atom,
    after_operator_section, classify_mutation_site, first_argument_token,
    imports_name, in_head_position, line_indents, parse_imports,
    prepared_source, resolve_primitive, strip_import_declarations,
    tokenize_haskell,
)
