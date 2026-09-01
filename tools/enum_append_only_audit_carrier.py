"""Wire-carrier attribution for the enum append-only audit.

None of this decides what is GUARDED — that is the three-condition rule
in the module docstring. It exists so an incompatible change can name
every component and historical shape that actually carries the type
(issue #1145's review: "must refer to every affected component, not one
singular owner"), and so the guidance can say so honestly when a
guarded type is on no wire at all.
"""
from __future__ import annotations

import re
from dataclasses import dataclass
from pathlib import Path

from enum_append_only_audit_model import (
    NON_WIRE_COMPONENT_DECLS,
    QUALIFIED_RE,
    WIRE_ROOT_EXTRA,
    WIRE_ROOT_GLOBS,
    WIRE_ROOT_GLOB_EXCLUSIONS,
    AuditError,
    Carrier,
    GuardedType,
    Scan,
)
from enum_append_only_audit_parse import (
    split_top_level,
    strip_haskell_comments,
)


# `csComponent = <ident>` inside a component module, and the
# `<ident> = ComponentId "<text>"` definitions those resolve against.
_CS_COMPONENT_RE = re.compile(
    r"(?<![A-Za-z0-9_'])csComponent[ \t]*=[ \t]*([a-z][A-Za-z0-9_']*)")
_COMPONENT_ID_RE = re.compile(
    r"^([a-z][A-Za-z0-9_']*)[ \t]*=[ \t]*ComponentId[ \t]+\"([^\"]*)\"",
    re.MULTILINE)


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
    return set(QUALIFIED_RE.findall(first))


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

            seeds = wire(set(QUALIFIED_RE.findall(seed_text)))
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
            QUALIFIED_RE.findall(decl.body))
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
