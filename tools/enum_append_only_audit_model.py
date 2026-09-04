"""Shared configuration and data models for the enum append-only audit.

The leaf of `tools/enum_append_only_audit.py`'s dependency graph: every
other owner imports from here and this module imports none of them, so
the discovery configuration, the error type, and the records that cross
owner boundaries each have exactly one definition. A per-owner copy of
`AuditError` would make the command layer's `except` clauses miss, and a
`Carrier` owned by the attribution walk would put a
baseline-serialization edge onto it.

Only patterns and records that cross an owner boundary live here. A
regex used by one owner alone (the deriving-clause patterns, the codec
`ComponentId` patterns) stays with that owner.

`REPO_ROOT` must keep resolving to the repository root, which is what a
flat sibling in `tools/` gives it. A module one directory deeper would
silently re-root discovery at `tools/`.
"""
from __future__ import annotations

import re
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


# `data`/`newtype` declarations inside a component module that are NOT
# wire shapes, so must not seed the reachability walk. Each must still
# exist (a stale entry fails), and any OTHER non-`DTO`-named declaration
# appearing in a component module fails too — which is what stops a real
# wire type from being left out of the roots silently.
NON_WIRE_COMPONENT_DECLS = {
    "World.Save.Component.PageCore.WorldPages":
        "the canonical decode TARGET `worldPagesCodec` migrates into, not "
        "a wire shape — its own `WorldPagesDTO*` are the wire",
}


# A (possibly qualified) type/constructor identifier. Only the LAST
# component is wanted: `HM.HashMap` is `HashMap`, and a fully qualified
# `Unit.Direction.Direction` is `Direction` — dropping the qualifier is
# what keeps the reference graph from missing an edge.
QUALIFIED_RE = re.compile(
    r"(?<![A-Za-z0-9_'.])(?:[A-Z][A-Za-z0-9_']*\.)*([A-Z][A-Za-z0-9_']*)")


class AuditError(Exception):
    """Something the reader could not classify.

    Every one of these is a HARD failure naming the source line. Silently
    skipping a declaration the parser does not understand is exactly how
    an unguarded enum would slip past, so an unrecognised declaration
    form, deriving strategy, or constructor shape reports instead."""


@dataclass(frozen=True)
class Constructor:
    """One alternative of a sum type: what its tag means, and what
    follows that tag on the wire.

    The NAME pins what the positional tag means. The PAYLOAD pins the
    fields that follow it, in declared order — cereal writes them
    POSITIONALLY too, so swapping two of a constructor's fields or
    changing one field's serialized type reinterprets already-saved
    bytes exactly as destructively as reordering the constructors does,
    while leaving the name and the field COUNT untouched (issue #1270).
    Recording the count alone was the gap: `arity` is now derived from
    the payload precisely so the two can never disagree.

    A slot's spelling is `normalize_field_type`'s output for a
    positional alternative, and `selector ∷ <type>` for a record one —
    see that function and `record_slots` for what the normalization
    deliberately erases and what it deliberately keeps."""
    name: str
    payload: tuple[str, ...]

    @property
    def arity(self) -> int:
        return len(self.payload)

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


@dataclass
class Scan:
    guarded: dict[str, GuardedType]           # qualified -> type
    declarations: list[Declaration]
    module_paths: dict[str, str]              # module -> rel path


@dataclass(frozen=True)
class Carrier:
    """One save-wire root a guarded type is reachable from."""
    label: str                    # '"unit-sim" — World.Save.Component.Entities'
    components: tuple[str, ...]   # ("unit-sim",) — empty for a bare module
    sort_key: tuple[str, str]
    path: tuple[str, ...]         # UnitSimDTO → PageSimDTO → … → Pose


@dataclass
class BaselineEntry:
    """One type's checked-in record: the constructor contract, plus the
    save-wire attribution captured WITH it.

    The attribution is recorded rather than merely recomputed because a
    type that is renamed, moved, or deleted can no longer be walked —
    and that is exactly the change whose migration guidance matters
    most. `recorded_carrier_lines` reads it back for that case."""
    constructors: list[Constructor]
    components: tuple[str, ...] = ()
    carriers: tuple[tuple[str, str], ...] = ()   # (label, via-path)
    source: str = ""
    # The captured `onSaveWire` flag, or None when the entry never
    # recorded one (a hand-added entry). `relocations()` needs the
    # captured value rather than a guess, because "was on the wire, now
    # is not" is exactly the change it must refuse to absorb.
    on_save_wire: bool | None = None
