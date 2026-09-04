#!/usr/bin/env python3
"""Append-only audit for the positionally-serialized enums (issue #1145).

A sum type whose `Serialize` instance is DERIVED THROUGH `Generic` is
encoded by cereal as a constructor INDEX followed by that constructor's
fields. BOTH are positional. So inserting, reordering, removing, or
renaming a constructor silently reinterprets every value already written
to disk — `Climbing` decodes as `Falling`, `DirNE` as `DirE` — and so
does reordering or retyping the FIELDS of one constructor, which moves
no tag at all. Nothing fails to compile and no ordinary test notices —
which makes it the highest-consequence silent-corruption rule in the
project.

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
catches reorder, insert, remove, and rename uniformly — and, since
#1270, the same list's recorded payload signatures catch a
same-arity field reorder or retype inside one constructor, which a
fixture can only ever catch for the values it happens to carry.

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
safer rule and, measured against this tree, barely larger: 32 of the 37
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
identity, the ordered constructor list, and for each constructor its
ordered PAYLOAD — the field slots that follow its tag on the wire, with
`arity` their count. The payload is part of the record because cereal
writes a constructor's fields positionally too (issue #1270), so a
constructor that keeps its name and index while its fields are
reordered, or one field's serialized type is changed, reinterprets
already-saved bytes exactly as destructively as reordering the
constructors does — and neither the name nor the count moves. Recording
the count alone left that invisible.

A slot is `normalize_field_type`'s output for a positional alternative,
and `selector ∷ <type>` for a record one. Read those two functions for
the precise contract; the two limits worth knowing up front are:

  - It compares what the CONSTRUCTOR DECLARES. A field whose type is a
    synonym, or whose `Serialize` instance changes underneath it, is
    not discoverable here and no slot moves — that hazard belongs to
    the frozen-DTO boundary and to `Test.Headless.World.Save.Compat`'s
    real decode of tracked fixtures.
  - Two POSITIONAL fields of the same type, swapped, are the same
    declaration text; nothing static can see it. Record alternatives do
    not have that blind spot, which is why a slot keeps its selector.

Each entry also records WHERE that type sat on the save wire when it was
captured — its source file, and every component and historical shape
reachability found, with the path. That is not decoration: a type that
is renamed, moved, or DELETED can no longer be walked, and it is exactly
the change whose migration guidance matters most, since every tag it
defined stops decoding. The recorded attribution is what lets that case
still name the affected components. Because it would otherwise rot
silently, the whole file is kept REGENERATED: a baseline whose
attribution no longer matches the code fails on its own (distinctly from
any constructor change) rather than being quietly trusted.

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

A pure MODULE MOVE — the same type name, the same constructors slot for
slot, declared in a different module — is the one other change that
ratchets. It is recognised as a RELOCATION rather than reported as the
deletion it superficially resembles, because nothing on the wire
changed: the type is still guarded, still reached through the same
codec, and every tag still means what it meant. Only the baseline's
ownership metadata goes stale — the qualified key, the `source` path,
and (since #2135, where the codecs themselves moved into owner modules)
the module named inside a carrier LABEL — so it fails until
`--update-baseline` records the new owner. `relocations()` states every
clause of that recognition and why each one is narrow; the short version
is that a rename, an ambiguous pairing, a constructor change alongside
the move, a change to the type's save-wire attribution, and a genuine
deletion are all still INCOMPATIBLE, and the self-test proves each of
them. The attribution clause is the one that is not obvious: attribution
is walked by bare TYPE NAME, so without it a persisted enum could be
deleted from its DTO and an unrelated off-wire enum of the same name
introduced elsewhere, and the ratchet would rewrite the entry to
`onSaveWire: false` — erasing the component attribution that a later
deletion's diagnostic reads back. Only the label's MODULE is elided
there: the component set, the on-wire status and every `via` path are
still compared exactly.

=== Which gate owns what

This audit is the exhaustive authoritative gate for BOTH halves of a
guarded sum's wire contract: its constructor list, and each
constructor's payload signature. Nothing else owns the second half —
before #1270 nothing did, which is the gap that issue closed.

Other gates overlap it incidentally, and that overlap is welcome rather
than something to prune: `Test.Headless.World.Save.Compat` decodes
tracked binary fixtures and so re-proves whichever constructor VALUES
those fixtures happen to carry, and `tools/save_compat_audit.py`'s
frozen-DTO fingerprint moves when a frozen module is edited, which
catches some transitive sums (`WorldEditDTO`, `ConstructTargetDTO`)
from the other direction. Neither is exhaustive over the guarded set;
this audit is.

What this audit does NOT cover: hand-written `Serialize` instances (the
`put`/`get` code IS the wire contract, in source, where review can see
it), field-order drift inside a SINGLE-constructor record (which emits
no tag at all, and belongs to the frozen-DTO boundary rule +
`tools/save_compat_audit.py`), a change reachable only THROUGH a field's
declared type rather than visible in the declaration itself (a type
synonym redefined elsewhere, or a referenced type's own `Serialize`
implementation changing), and whether a migration is CORRECT
(`Test.Headless.World.Save.Compat`'s real decode of tracked fixtures).

=== Where the implementation lives

This file is the public façade: the contract above, the CLI, and mode
dispatch. Each correctness owner is a flat sibling in `tools/`, and the
dependencies run one way — every module below may import only the ones
above it (issue #2057):

  `_model`     shared discovery configuration, `AuditError`, and the
               records that cross an owner boundary; imports nothing
  `_parse`     Haskell lexing and declaration parsing — comments and
               literals, `normalize_field_type`, `record_slots`,
               deriving strategies, `qualifies_as_guarded`
  `_scan`      the repository walk that DECIDES the guarded set
  `_carrier`   `compute_wire_carriers` — diagnostic attribution only
  `_baseline`  loading and deterministic rendering of the baseline file
  `_report`    both-directions comparison, `relocations()`, and the
               migration guidance
  `_commands`  `run_repository_audit` / `run_update_baseline`
  `_selftest`  the synthetic mutation suite, loaded ONLY by `--self-test`
               and imported by no production module

Usage:
  python3 tools/enum_append_only_audit.py
  python3 tools/enum_append_only_audit.py --update-baseline
  python3 tools/enum_append_only_audit.py --self-test
Exit codes: 0 = the live declarations match the baseline exactly,
1 = they do not (append-compatible or not), or a self-test check failed.
"""
from __future__ import annotations

import argparse

from enum_append_only_audit_model import BASELINE_REL
from enum_append_only_audit_commands import (
    run_repository_audit,
    run_update_baseline,
)


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
        # Deferred so the production modes never load the
        # fixtures: the dependency runs one way (requirement 19).
        from enum_append_only_audit_selftest import main_self_test
        return main_self_test()
    if args.update_baseline:
        return run_update_baseline()
    return run_repository_audit()


if __name__ == "__main__":
    raise SystemExit(main())
