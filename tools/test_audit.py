#!/usr/bin/env python3
"""Unit tests for world_audit.py.

Constructs synthetic tile grids that exercise each check, verifies the
audit correctly identifies the issues.

Exit codes:
  0 = all tests passed
  1 = one or more tests failed
"""

from __future__ import annotations

import ast
import json
import sys
from pathlib import Path
from typing import Any

sys.path.insert(0, str(Path(__file__).resolve().parent))
import world_audit  # type: ignore
from world_audit import (  # type: ignore
    audit_dump, INT64_MIN, severity_of, classify_category,
    BUG_CATEGORIES, QUALITY_CATEGORIES, QUALITY_THRESHOLDS,
)
from world_check import (  # type: ignore
    CheckResult, check_issue_summary, check_determinism_status,
    PASS, FAIL, IMPROVED,
)


# ----- Emitted-category inventory ------------------------------------------
#
# The authoritative set of categories the audit can emit is the `category`
# argument of every `Issue(...)` construction in world_audit.py. It is NOT
# ALL_CHECKS, whose keys are check-function labels: TERRAIN_SPIKES_PITS is a
# key but not a category, and six real categories (the river/lake-under-terrain
# pair and the four floating-fluid variants) are categories but not keys.
#
# The inventory is therefore derived from the audit SOURCE by AST, so a
# category added to a check function but classified nowhere fails
# test_severity_classification instead of silently reaching world_check.py.
# Anything that cannot be resolved statically is reported as a failure — never
# skipped, since a skipped call site is exactly the fail-open hole this
# derivation exists to close.


def _resolve_category_expr(node: ast.expr) -> set[str] | None:
    """Return the category strings an expression can yield, or None.

    None means "not statically resolvable" and must be reported, not
    ignored. Three shapes resolve, covering every form world_audit.py uses:

      "LITERAL"                        a plain string constant
      "A" if cond else "B"             the river/lake-under-terrain dispatch
      {...}.get(key, "DEFAULT")        the floating-fluid family

    A resolution is never empty: an expression that yields no category at
    all is reported as unresolvable rather than contributing nothing, so a
    call site can never be dropped from the inventory silently.
    """
    if isinstance(node, ast.Constant):
        return {node.value} if isinstance(node.value, str) else None

    if isinstance(node, ast.IfExp):
        body = _resolve_category_expr(node.body)
        orelse = _resolve_category_expr(node.orelse)
        if body is None or orelse is None:
            return None
        return body | orelse

    if (isinstance(node, ast.Call)
            and isinstance(node.func, ast.Attribute)
            and node.func.attr == "get"
            and isinstance(node.func.value, ast.Dict)
            and not node.keywords
            and 1 <= len(node.args) <= 2):
        mapping = node.func.value
        # A `**other` entry has a None key and hides values we cannot see.
        if any(key is None for key in mapping.keys):
            return None
        out: set[str] = set()
        for value in mapping.values:
            resolved = _resolve_category_expr(value)
            if resolved is None:
                return None
            out |= resolved
        if len(node.args) == 2:
            default = _resolve_category_expr(node.args[1])
            if default is None:
                return None
            out |= default
        return out or None

    return None


def _target_names(target: ast.expr) -> list[str]:
    """Every bare name an assignment target actually binds.

    `a`, `a, b`, `*rest` and their nestings bind names; `obj.attr` and
    `obj[key]` bind an attribute or item, and the names they mention are
    read, not bound — reporting those would hide unrelated names.
    """
    if isinstance(target, ast.Name):
        return [target.id]
    if isinstance(target, ast.Starred):
        return _target_names(target.value)
    if isinstance(target, (ast.Tuple, ast.List)):
        names: list[str] = []
        for element in target.elts:
            names.extend(_target_names(element))
        return names
    return []


def _scope_nodes(scope: ast.AST) -> list[ast.AST]:
    """Nodes belonging to `scope`'s OWN lexical scope.

    Traversal stops at every nested function, class and lambda: their
    bindings are local to them. Adopting a nested scope's bindings would
    let an inner `cat = "ISOLATED_FLUID"` resolve an outer name whose real
    value is dynamic, and let one function's local resolve another's.
    A nested definition still contributes its own NAME, which IS bound
    here; the analysed scope's parameters arrive as its `arguments` child.
    """
    out: list[ast.AST] = []
    stack = list(ast.iter_child_nodes(scope))
    while stack:
        node = stack.pop()
        out.append(node)
        if isinstance(node, (ast.FunctionDef, ast.AsyncFunctionDef,
                             ast.ClassDef, ast.Lambda)):
            continue
        stack.extend(ast.iter_child_nodes(node))
    return out


def _assignments_in(scope: ast.AST) -> dict[str, list[ast.expr] | None]:
    """Map each name BOUND in `scope` to every value bound to it.

    A name bound anywhere by a form whose value cannot be read as an
    expression maps to None — "opaque" — instead of being described by
    whatever plain assignments happen to sit beside it. Recording only
    Assign/AnnAssign would read `cat = "ISOLATED_FLUID"` followed by
    `cat += suffix` as the bare literal, and a `for cat in ...` rebinding
    as nothing at all, which is exactly the silent acceptance this
    derivation exists to prevent.
    """
    nodes = _scope_nodes(scope)
    values: dict[str, list[ast.expr]] = {}
    opaque: set[str] = set()

    def hide(names: list[str]) -> None:
        opaque.update(names)

    def hide_arguments(args: ast.arguments) -> None:
        every = [*args.posonlyargs, *args.args, *args.kwonlyargs]
        if args.vararg is not None:
            every.append(args.vararg)
        if args.kwarg is not None:
            every.append(args.kwarg)
        hide([a.arg for a in every])

    for node in nodes:
        if isinstance(node, ast.Assign):
            for target in node.targets:
                if isinstance(target, ast.Name):
                    values.setdefault(target.id, []).append(node.value)
                else:
                    hide(_target_names(target))
        elif isinstance(node, (ast.AnnAssign, ast.NamedExpr)):
            if node.value is None:
                continue          # a bare annotation binds nothing
            if isinstance(node.target, ast.Name):
                values.setdefault(node.target.id, []).append(node.value)
            else:
                hide(_target_names(node.target))
        elif isinstance(node, ast.AugAssign):
            hide(_target_names(node.target))
        elif isinstance(node, (ast.For, ast.AsyncFor, ast.comprehension)):
            hide(_target_names(node.target))
        elif isinstance(node, ast.withitem):
            if node.optional_vars is not None:
                hide(_target_names(node.optional_vars))
        elif isinstance(node, ast.ExceptHandler):
            if node.name:
                hide([node.name])
        elif isinstance(node, (ast.Import, ast.ImportFrom)):
            hide([alias.asname or alias.name.split(".")[0]
                  for alias in node.names])
        elif isinstance(node, (ast.Global, ast.Nonlocal)):
            hide(node.names)
        elif isinstance(node, ast.Delete):
            for target in node.targets:
                hide(_target_names(target))
        elif isinstance(node, ast.arguments):
            # Only ever the analysed scope's own parameters: traversal stops
            # before a nested definition's argument list.
            hide_arguments(node)
        elif isinstance(node, (ast.FunctionDef, ast.AsyncFunctionDef,
                               ast.ClassDef)):
            hide([node.name])          # the definition's name, not its body
        elif isinstance(node, (ast.MatchAs, ast.MatchStar)):
            if node.name:
                hide([node.name])
        elif isinstance(node, ast.MatchMapping):
            if node.rest:
                hide([node.rest])

    bindings: dict[str, list[ast.expr] | None] = dict(values)
    for name in opaque:
        bindings[name] = None
    return bindings


def extract_issue_categories(source: str, filename: str
                             ) -> tuple[set[str], list[str]]:
    """Extract every category an `Issue(...)` call site can emit.

    Returns (categories, unresolved). `unresolved` lists the call sites whose
    category could not be determined statically, each as a
    `<filename>:<line>: <reason>` string; a caller must treat a non-empty
    list as a failure.
    """
    tree = ast.parse(source, filename=filename)

    parents: dict[int, ast.AST] = {}
    for parent in ast.walk(tree):
        for child in ast.iter_child_nodes(parent):
            parents[id(child)] = parent

    scope_cache: dict[int, dict[str, list[ast.expr] | None]] = {}

    def bindings_of(scope: ast.AST) -> dict[str, list[ast.expr] | None]:
        cached = scope_cache.get(id(scope))
        if cached is None:
            cached = _assignments_in(scope)
            scope_cache[id(scope)] = cached
        return cached

    def names_visible_at(node: ast.AST) -> dict[str, list[ast.expr] | None]:
        """The enclosing scope chain, innermost binding winning.

        Module, then each enclosing function from outermost inwards — the
        scopes a name here can actually be read from. A class body is a
        scope only for code directly inside it: a method never sees it.
        """
        chain: list[ast.AST] = []
        innermost = True
        current: ast.AST | None = parents.get(id(node))
        while current is not None:
            if isinstance(current, (ast.FunctionDef, ast.AsyncFunctionDef,
                                    ast.Lambda)):
                chain.append(current)
                innermost = False
            elif isinstance(current, ast.ClassDef):
                if innermost:
                    chain.append(current)
                innermost = False
            current = parents.get(id(current))

        visible = dict(bindings_of(tree))
        for scope in reversed(chain):
            visible.update(bindings_of(scope))
        return visible

    categories: set[str] = set()
    unresolved: list[str] = []

    for node in ast.walk(tree):
        if not isinstance(node, ast.Call):
            continue
        func = node.func
        called = (func.id if isinstance(func, ast.Name)
                  else func.attr if isinstance(func, ast.Attribute) else None)
        if called != "Issue":
            continue

        arg: ast.expr | None = node.args[0] if node.args else None
        if arg is None:
            for keyword in node.keywords:
                if keyword.arg == "category":
                    arg = keyword.value
                    break
        if arg is None:
            unresolved.append(
                f"{filename}:{node.lineno}: Issue(...) has no category argument"
            )
            continue

        detail = ""
        resolved = _resolve_category_expr(arg)
        if resolved is None and isinstance(arg, ast.Name):
            names = names_visible_at(node)
            bound = names.get(arg.id)
            if bound is None:
                detail = (
                    f": `{arg.id}` is bound by a form that cannot be read as "
                    f"a value (augmented assignment, a loop/with/except "
                    f"target, a parameter, an import alias or a match capture)"
                    if arg.id in names else
                    f": `{arg.id}` is never bound in this scope"
                )
            else:
                accumulated: set[str] = set()
                for value in bound:
                    one = _resolve_category_expr(value)
                    if one is None:
                        accumulated = set()
                        break
                    accumulated |= one
                resolved = accumulated or None

        if resolved is None:
            unresolved.append(
                f"{filename}:{node.lineno}: cannot statically resolve the "
                f"category of Issue({ast.unparse(arg)}){detail}"
            )
            continue
        categories |= resolved

    return categories, unresolved


def emitted_categories() -> tuple[set[str], list[str]]:
    """The audit's real emitted-category inventory, read from its source."""
    path = Path(world_audit.__file__).resolve()
    return extract_issue_categories(path.read_text(), str(path))


# ----- Helpers -------------------------------------------------------------

def tile(x: int, y: int, terrainZ: int = 1,
         fluidType: str | None = None, fluidSurf: int | None = None,
         matId: int = 56,  # loam — neutral; 62-64 trip WETLAND_ON_SLOPE
         glacierZone: bool = False, beyondGlacier: bool = False) -> dict[str, Any]:
    if fluidType is None:
        surfaceZ = terrainZ
    else:
        surfaceZ = max(terrainZ, fluidSurf if fluidSurf is not None else terrainZ)
    return {
        "x": x, "y": y, "v": x + y,
        "terrainZ": terrainZ,
        "surfaceZ": surfaceZ,
        "matId": matId,
        "fluidType": fluidType,
        "fluidSurf": fluidSurf,
        "iceSurf": None,
        "iceMode": None,
        "glacierZone": glacierZone,
        "beyondGlacier": beyondGlacier,
    }


def flat_grid(w: int, h: int, x0: int = 0, y0: int = 0,
              terrainZ: int = 1,
              fluidType: str | None = None,
              fluidSurf: int | None = None) -> list[dict[str, Any]]:
    return [
        tile(x0 + dx, y0 + dy, terrainZ=terrainZ,
             fluidType=fluidType, fluidSurf=fluidSurf)
        for dy in range(h) for dx in range(w)
    ]


def make_tiles(tiles: list[dict[str, Any]]) -> list[dict[str, Any]]:
    """Normalize: drop duplicates (later overrides earlier)."""
    seen = {}
    for t in tiles:
        seen[(t["x"], t["y"])] = t
    return list(seen.values())


def count_category(result_dict: dict[str, Any], cat: str) -> int:
    return result_dict["summary"].get(cat, 0)


# ----- Tests ---------------------------------------------------------------

FAILURES: list[str] = []


def expect(cond: bool, msg: str) -> None:
    if not cond:
        FAILURES.append(msg)
        print(f"  FAIL: {msg}")
    else:
        print(f"  OK:   {msg}")


def test_dry_below_sea() -> None:
    print("test_dry_below_sea")
    tiles = flat_grid(5, 5, -2, -2, terrainZ=-1, fluidType="ocean", fluidSurf=0)
    # Turn one tile into dry with terrainZ=-1
    tiles[12] = tile(0, 0, terrainZ=-1, fluidType=None)
    result = audit_dump(tiles).to_dict()
    expect(count_category(result, "DRY_BELOW_SEA") == 1,
           f"DRY_BELOW_SEA count: {count_category(result, 'DRY_BELOW_SEA')}, expected 1")


def test_ocean_on_land() -> None:
    print("test_ocean_on_land")
    # Cascade bug: ocean tile with terrainZ > 5
    tiles = flat_grid(3, 3, -1, -1, terrainZ=1, fluidType="ocean", fluidSurf=0)
    tiles[4] = tile(0, 0, terrainZ=100, fluidType="ocean", fluidSurf=0)
    result = audit_dump(tiles).to_dict()
    expect(count_category(result, "OCEAN_ON_LAND") == 1,
           f"OCEAN_ON_LAND count: {count_category(result, 'OCEAN_ON_LAND')}, expected 1")


def test_river_under_terrain() -> None:
    print("test_river_under_terrain")
    # River tile where fluidSurf < terrainZ
    tiles = flat_grid(3, 3, -1, -1, terrainZ=5, fluidType=None)
    tiles[4] = tile(0, 0, terrainZ=10, fluidType="river", fluidSurf=5)
    result = audit_dump(tiles).to_dict()
    expect(count_category(result, "RIVER_UNDER_TERRAIN") == 1,
           f"RIVER_UNDER_TERRAIN count: {count_category(result, 'RIVER_UNDER_TERRAIN')}, expected 1")


def test_lake_under_terrain() -> None:
    print("test_lake_under_terrain")
    tiles = flat_grid(3, 3, -1, -1, terrainZ=5, fluidType=None)
    tiles[4] = tile(0, 0, terrainZ=10, fluidType="lake", fluidSurf=5)
    result = audit_dump(tiles).to_dict()
    expect(count_category(result, "LAKE_UNDER_TERRAIN") == 1,
           f"LAKE_UNDER_TERRAIN count: {count_category(result, 'LAKE_UNDER_TERRAIN')}, expected 1")


def test_floating_fluid() -> None:
    print("test_floating_fluid")
    # Lava with fluidSurf - terrainZ > 15
    tiles = flat_grid(3, 3, -1, -1, terrainZ=-50, fluidType="lava", fluidSurf=5)
    result = audit_dump(tiles).to_dict()
    expect(count_category(result, "FLOATING_LAVA") == 9,
           f"FLOATING_LAVA count: {count_category(result, 'FLOATING_LAVA')}, expected 9")

    # River with high depth
    tiles = flat_grid(3, 3, -1, -1, terrainZ=-50, fluidType="river", fluidSurf=5)
    result = audit_dump(tiles).to_dict()
    expect(count_category(result, "FLOATING_RIVER") == 9,
           f"FLOATING_RIVER count: {count_category(result, 'FLOATING_RIVER')}, expected 9")

    # Ocean at any depth should NOT trigger floating
    tiles = flat_grid(3, 3, -1, -1, terrainZ=-100, fluidType="ocean", fluidSurf=0)
    result = audit_dump(tiles).to_dict()
    expect(count_category(result, "FLOATING_LAVA") == 0
           and count_category(result, "FLOATING_RIVER") == 0
           and count_category(result, "FLOATING_LAKE") == 0,
           "deep ocean should not trigger FLOATING_*")


def test_terrain_spike() -> None:
    print("test_terrain_spike")
    # Flat terrain with one tile spike
    tiles = flat_grid(5, 5, -2, -2, terrainZ=1, fluidType=None)
    # Replace center with a spike
    tiles = make_tiles(tiles)
    for i, t in enumerate(tiles):
        if t["x"] == 0 and t["y"] == 0:
            tiles[i] = tile(0, 0, terrainZ=100)
            break
    result = audit_dump(tiles).to_dict()
    expect(count_category(result, "TERRAIN_SPIKE") == 1,
           f"TERRAIN_SPIKE count: {count_category(result, 'TERRAIN_SPIKE')}, expected 1")


def test_terrain_pit() -> None:
    print("test_terrain_pit")
    tiles = flat_grid(5, 5, -2, -2, terrainZ=100, fluidType=None)
    tiles = make_tiles(tiles)
    for i, t in enumerate(tiles):
        if t["x"] == 0 and t["y"] == 0:
            tiles[i] = tile(0, 0, terrainZ=1)
            break
    result = audit_dump(tiles).to_dict()
    expect(count_category(result, "TERRAIN_PIT") == 1,
           f"TERRAIN_PIT count: {count_category(result, 'TERRAIN_PIT')}, expected 1")


def test_terrain_pit_submerged() -> None:
    print("test_terrain_pit_submerged")
    # A deep hole under a lake whose surface covers the lowest
    # neighbour: the water plane renders flat over it — concealed,
    # not an artifact. The despike pass only lowers, never raises,
    # so these are legitimate worldgen output.
    tiles = flat_grid(5, 5, -2, -2, terrainZ=100, fluidType=None)
    tiles = make_tiles(tiles)
    for i, t in enumerate(tiles):
        if t["x"] == 0 and t["y"] == 0:
            tiles[i] = tile(0, 0, terrainZ=1, fluidType="lake", fluidSurf=100)
            break
    result = audit_dump(tiles).to_dict()
    expect(count_category(result, "TERRAIN_PIT") == 0,
           f"submerged pit: TERRAIN_PIT count "
           f"{count_category(result, 'TERRAIN_PIT')}, expected 0 (concealed)")
    # But a pit whose fluid does NOT reach the lowest neighbour is
    # still a visible wall — must flag.
    for i, t in enumerate(tiles):
        if t["x"] == 0 and t["y"] == 0:
            tiles[i] = tile(0, 0, terrainZ=1, fluidType="lake", fluidSurf=3)
            break
    result = audit_dump(tiles).to_dict()
    expect(count_category(result, "TERRAIN_PIT") == 1,
           f"shallow-puddle pit: TERRAIN_PIT count "
           f"{count_category(result, 'TERRAIN_PIT')}, expected 1 (visible)")


def test_terrain_spike_submerged() -> None:
    print("test_terrain_spike_submerged")
    # An ocean seamount: spike fully under its own fluid surface
    # renders as flat water — concealed. (Real case: basalt seamount
    # from an underwater vent, seed 4 w64.)
    tiles = flat_grid(5, 5, -2, -2, terrainZ=-50, fluidType="ocean",
                      fluidSurf=0)
    tiles = make_tiles(tiles)
    for i, t in enumerate(tiles):
        if t["x"] == 0 and t["y"] == 0:
            tiles[i] = tile(0, 0, terrainZ=-20, fluidType="ocean", fluidSurf=0)
            break
    result = audit_dump(tiles).to_dict()
    expect(count_category(result, "TERRAIN_SPIKE") == 0,
           f"submerged spike: TERRAIN_SPIKE count "
           f"{count_category(result, 'TERRAIN_SPIKE')}, expected 0 (concealed)")
    # A dry spike poking above the surrounding water is an island /
    # pillar — still flags.
    for i, t in enumerate(tiles):
        if t["x"] == 0 and t["y"] == 0:
            tiles[i] = tile(0, 0, terrainZ=5, fluidType=None)
            break
    result = audit_dump(tiles).to_dict()
    expect(count_category(result, "TERRAIN_SPIKE") == 1,
           f"emergent spike: TERRAIN_SPIKE count "
           f"{count_category(result, 'TERRAIN_SPIKE')}, expected 1 (visible)")


def test_river_chunk_gap() -> None:
    print("test_river_chunk_gap")
    # River at x=15 (chunk edge), dry at x=16 (next chunk), terrain low
    tiles = [
        tile(15, 0, terrainZ=2, fluidType="river", fluidSurf=5),
        tile(16, 0, terrainZ=2, fluidType=None),
    ]
    result = audit_dump(tiles).to_dict()
    expect(count_category(result, "RIVER_CHUNK_GAP") == 1,
           f"RIVER_CHUNK_GAP count: {count_category(result, 'RIVER_CHUNK_GAP')}, expected 1")


def test_river_mouth_drop() -> None:
    print("test_river_mouth_drop")
    tiles = [
        tile(0, 0, terrainZ=5, fluidType="river", fluidSurf=15),
        tile(1, 0, terrainZ=-5, fluidType="ocean", fluidSurf=0),
    ]
    result = audit_dump(tiles).to_dict()
    expect(count_category(result, "RIVER_MOUTH_DROP") == 1,
           f"RIVER_MOUTH_DROP count: {count_category(result, 'RIVER_MOUTH_DROP')}, expected 1")


def test_island_1tile() -> None:
    print("test_island_1tile")
    # 1 dry tile surrounded by ocean
    tiles = [
        tile(-1, 0, terrainZ=-1, fluidType="ocean", fluidSurf=0),
        tile(1, 0, terrainZ=-1, fluidType="ocean", fluidSurf=0),
        tile(0, -1, terrainZ=-1, fluidType="ocean", fluidSurf=0),
        tile(0, 1, terrainZ=-1, fluidType="ocean", fluidSurf=0),
        tile(0, 0, terrainZ=-1, fluidType=None),
    ]
    result = audit_dump(tiles).to_dict()
    expect(count_category(result, "ISLAND_1TILE") == 1,
           f"ISLAND_1TILE count: {count_category(result, 'ISLAND_1TILE')}, expected 1")


def test_isolated_fluid() -> None:
    print("test_isolated_fluid")
    # 1 river tile surrounded by dry
    tiles = [
        tile(-1, 0, terrainZ=10, fluidType=None),
        tile(1, 0, terrainZ=10, fluidType=None),
        tile(0, -1, terrainZ=10, fluidType=None),
        tile(0, 1, terrainZ=10, fluidType=None),
        tile(0, 0, terrainZ=5, fluidType="river", fluidSurf=8),
    ]
    result = audit_dump(tiles).to_dict()
    expect(count_category(result, "ISOLATED_FLUID") == 1,
           f"ISOLATED_FLUID count: {count_category(result, 'ISOLATED_FLUID')}, expected 1")


def test_minbound_leak() -> None:
    print("test_minbound_leak")
    tiles = [
        tile(0, 0, terrainZ=INT64_MIN),  # beyondGlacier=False
    ]
    # Override surfaceZ since our helper doesn't handle minBound right
    tiles[0]["surfaceZ"] = INT64_MIN
    result = audit_dump(tiles).to_dict()
    expect(count_category(result, "MINBOUND_LEAK") == 1,
           f"MINBOUND_LEAK count: {count_category(result, 'MINBOUND_LEAK')}, expected 1")


def test_surface_inconsistent() -> None:
    print("test_surface_inconsistent")
    tiles = [
        {"x": 0, "y": 0, "v": 0,
         "terrainZ": 10, "surfaceZ": 5,  # wrong: should be max(10, None) = 10
         "matId": 64,
         "fluidType": None, "fluidSurf": None,
         "iceSurf": None, "iceMode": None,
         "glacierZone": False, "beyondGlacier": False},
    ]
    result = audit_dump(tiles).to_dict()
    expect(count_category(result, "SURFACE_INCONSISTENT") == 1,
           f"SURFACE_INCONSISTENT count: {count_category(result, 'SURFACE_INCONSISTENT')}, expected 1")


def test_river_surface_uses_fluid_not_max() -> None:
    """River tiles render surfaceZ = fluidSurf (not max(terr, fluid))
    because the engine hides terrain protrusions under the water plane.
    A river tile with surfaceZ = terrainZ when fluidSurf < terrainZ is
    INCONSISTENT (engine writes fluidSurf), and surfaceZ = fluidSurf is
    correct even though terrain pokes above."""
    print("test_river_surface_uses_fluid_not_max")
    # Correct: river with terr=10, water=8, surface=8 (water plane, NOT max).
    correct = [{
        "x": 0, "y": 0, "v": 0,
        "terrainZ": 10, "surfaceZ": 8,
        "matId": 64,
        "fluidType": "river", "fluidSurf": 8,
        "iceSurf": None, "iceMode": None,
        "glacierZone": False, "beyondGlacier": False,
    }]
    r = audit_dump(correct).to_dict()
    expect(count_category(r, "SURFACE_INCONSISTENT") == 0,
           f"river surface = fluidSurf should be consistent, "
           f"got {count_category(r, 'SURFACE_INCONSISTENT')}")

    # Incorrect: river where surfaceZ == max(terr, fluid). The engine
    # never writes this for River, so it indicates a real bug.
    wrong = [{
        "x": 0, "y": 0, "v": 0,
        "terrainZ": 10, "surfaceZ": 10,  # should be 8
        "matId": 64,
        "fluidType": "river", "fluidSurf": 8,
        "iceSurf": None, "iceMode": None,
        "glacierZone": False, "beyondGlacier": False,
    }]
    r = audit_dump(wrong).to_dict()
    expect(count_category(r, "SURFACE_INCONSISTENT") == 1,
           f"river with surface=max(terr,fluid) should flag, "
           f"got {count_category(r, 'SURFACE_INCONSISTENT')}")


def test_lake_surface_uses_max() -> None:
    """Lake/ocean/lava still use max(terrainZ, fluidSurf) — only rivers
    have the special flat rule."""
    print("test_lake_surface_uses_max")
    # Lake with terr=5, water=8, surface=8 (max) — correct
    tiles = [{
        "x": 0, "y": 0, "v": 0,
        "terrainZ": 5, "surfaceZ": 8,
        "matId": 64,
        "fluidType": "lake", "fluidSurf": 8,
        "iceSurf": None, "iceMode": None,
        "glacierZone": False, "beyondGlacier": False,
    }]
    r = audit_dump(tiles).to_dict()
    expect(count_category(r, "SURFACE_INCONSISTENT") == 0,
           f"lake surface = max(terr, fluid) should be consistent, "
           f"got {count_category(r, 'SURFACE_INCONSISTENT')}")


def test_dry_below_sea_inland_basin() -> None:
    """An inland basin below sea level that is NOT connected to the
    ocean (e.g. a sub-sea-level cave system or a closed depression)
    should not flag DRY_BELOW_SEA — those dry tiles are legitimate."""
    print("test_dry_below_sea_inland_basin")
    # Surround a sub-sea region with above-sea-level terrain — there's
    # no ocean path to the inner dry tile, so it should not flag.
    tiles = []
    for y in range(-3, 4):
        for x in range(-3, 4):
            # Outer ring above sea level
            if abs(x) == 3 or abs(y) == 3:
                tiles.append(tile(x, y, terrainZ=20))
            else:
                # Inner region below sea level, all dry
                tiles.append(tile(x, y, terrainZ=-5))
    result = audit_dump(tiles).to_dict()
    expect(count_category(result, "DRY_BELOW_SEA") == 0,
           f"inland basin should not flag DRY_BELOW_SEA, "
           f"got {count_category(result, 'DRY_BELOW_SEA')}")


def test_dry_below_sea_ocean_connected() -> None:
    """A dry tile directly adjacent to ocean tiles IS a bug and must
    flag — the ocean has clearly not reached a tile it should have."""
    print("test_dry_below_sea_ocean_connected")
    tiles = []
    # 5x5 ocean region
    for y in range(-2, 3):
        for x in range(-2, 3):
            tiles.append(tile(x, y, terrainZ=-3,
                              fluidType="ocean", fluidSurf=0))
    # Convert one to dry
    for i, t in enumerate(tiles):
        if t["x"] == 0 and t["y"] == 0:
            tiles[i] = tile(0, 0, terrainZ=-3, fluidType=None)
            break
    result = audit_dump(tiles).to_dict()
    expect(count_category(result, "DRY_BELOW_SEA") == 1,
           f"ocean-connected dry tile should flag, "
           f"got {count_category(result, 'DRY_BELOW_SEA')}")


def test_severity_classification() -> None:
    """Every category the audit can emit — the `category` argument of every
    `Issue(...)` call in world_audit.py, extracted from its source rather
    than restated here — must be classified as a BUG or a QUALITY metric,
    and every QUALITY category must carry an explicit threshold."""
    print("test_severity_classification")
    every_cat, unresolved = emitted_categories()

    # A call site we cannot read is a hole in the inventory, so it fails
    # rather than shrinking the set we go on to check.
    expect(not unresolved,
           f"Issue(...) call sites whose category is not statically "
           f"resolvable: {unresolved}")
    expect(every_cat,
           "no Issue(...) categories were extracted from world_audit.py — "
           "the derivation is broken, not the audit")

    # The two dynamic dispatches must keep resolving; losing them would
    # shrink the inventory silently and re-open the fail-open hole.
    dynamic = {"RIVER_UNDER_TERRAIN", "LAKE_UNDER_TERRAIN",
               "FLOATING_LAVA", "FLOATING_RIVER", "FLOATING_LAKE",
               "FLOATING_FLUID"}
    expect(dynamic <= every_cat,
           f"dynamically dispatched categories missing from the derived "
           f"inventory: {sorted(dynamic - every_cat)}")

    classified = BUG_CATEGORIES | QUALITY_CATEGORIES
    missing = every_cat - classified
    expect(not missing,
           f"unclassified categories (must be in BUG_CATEGORIES or "
           f"QUALITY_CATEGORIES): {sorted(missing)}")

    overlap = BUG_CATEGORIES & QUALITY_CATEGORIES
    expect(not overlap,
           f"categories in both BUG and QUALITY sets: {sorted(overlap)}")

    # Every QUALITY category should have a threshold
    missing_threshold = QUALITY_CATEGORIES - set(QUALITY_THRESHOLDS.keys())
    expect(not missing_threshold,
           f"QUALITY categories without thresholds: {sorted(missing_threshold)}")

    # severity_of() returns the right label for known cats
    expect(severity_of("OCEAN_ON_LAND") == "BUG",
           "OCEAN_ON_LAND should be BUG severity")
    expect(severity_of("ISOLATED_FLUID") == "QUALITY",
           "ISOLATED_FLUID should be QUALITY severity")
    expect(severity_of("RIVER_UNDER_TERRAIN") == "QUALITY",
           "RIVER_UNDER_TERRAIN should be QUALITY severity (underground "
           "water is legitimate)")

    # classify_category() is the closed variant the gate uses: it reports
    # the absence instead of bucketing an unknown category as QUALITY.
    expect(classify_category("OCEAN_ON_LAND") == "BUG",
           "classify_category should agree with severity_of on a BUG")
    expect(classify_category("ISOLATED_FLUID") == "QUALITY",
           "classify_category should agree with severity_of on a QUALITY")
    expect(classify_category("NEW_CORRUPTION") is None,
           "classify_category must return None for an unclassified category, "
           f"got {classify_category('NEW_CORRUPTION')!r}")


def test_category_extraction_resolves_and_fails_loudly() -> None:
    """The inventory derivation reads all three category shapes and reports
    — never skips — a call site it cannot resolve statically."""
    print("test_category_extraction_resolves_and_fails_loudly")

    def extract(body: str) -> tuple[set[str], list[str]]:
        return extract_issue_categories(
            "def check(grid, issues):\n" + body, "<synthetic>")

    cats, unresolved = extract('    issues.append(Issue("LITERAL", 0, 0, ""))\n')
    expect(cats == {"LITERAL"} and not unresolved,
           f"literal category should resolve, got {cats} / {unresolved}")

    cats, unresolved = extract(
        '    cat = "A" if ft == "river" else "B"\n'
        '    issues.append(Issue(cat, 0, 0, ""))\n')
    expect(cats == {"A", "B"} and not unresolved,
           f"conditional category should resolve to both arms, got "
           f"{cats} / {unresolved}")

    cats, unresolved = extract(
        '    cat = {"lava": "X", "river": "Y"}.get(ft, "Z")\n'
        '    issues.append(Issue(cat, 0, 0, ""))\n')
    expect(cats == {"X", "Y", "Z"} and not unresolved,
           f"dict-dispatch category should resolve to values plus default, "
           f"got {cats} / {unresolved}")

    cats, unresolved = extract(
        '    cat = compute_category(ft)\n'
        '    issues.append(Issue(cat, 0, 0, ""))\n')
    expect(not cats and len(unresolved) == 1,
           f"a computed category must be reported unresolved, got "
           f"{cats} / {unresolved}")

    cats, unresolved = extract('    issues.append(Issue(cat, 0, 0, ""))\n')
    expect(not cats and len(unresolved) == 1,
           f"an unbound category name must be reported unresolved, got "
           f"{cats} / {unresolved}")

    cats, unresolved = extract(
        '    cat = {}.get(ft)\n'
        '    issues.append(Issue(cat, 0, 0, ""))\n')
    expect(not cats and len(unresolved) == 1,
           f"a dispatch that yields no category at all must be reported "
           f"unresolved, got {cats} / {unresolved}")

    # A partially unresolvable dispatch must not contribute its readable half.
    cats, unresolved = extract(
        '    cat = "A" if ft == "river" else compute_category(ft)\n'
        '    issues.append(Issue(cat, 0, 0, ""))\n')
    expect(not cats and len(unresolved) == 1,
           f"a half-resolvable category must be reported unresolved rather "
           f"than contributing its readable arm, got {cats} / {unresolved}")

    # A name REBOUND by a form the derivation cannot read as a value must not
    # be described by whatever plain assignment sits beside it — the emitted
    # category is dynamic and may be unclassified.
    rebindings = {
        "augmented assignment":
            '    cat = "ISOLATED_FLUID"\n'
            '    cat += suffix\n',
        "loop target":
            '    cat = "ISOLATED_FLUID"\n'
            '    for cat in CATEGORIES:\n'
            '        pass\n',
        "tuple unpacking":
            '    cat = "ISOLATED_FLUID"\n'
            '    cat, extra = compute(ft)\n',
        "with target":
            '    cat = "ISOLATED_FLUID"\n'
            '    with opened(ft) as cat:\n'
            '        pass\n',
        "except target":
            '    cat = "ISOLATED_FLUID"\n'
            '    try:\n'
            '        pass\n'
            '    except ValueError as cat:\n'
            '        pass\n',
    }
    for label, prelude in rebindings.items():
        cats, unresolved = extract(
            prelude + '    issues.append(Issue(cat, 0, 0, ""))\n')
        expect(not cats and len(unresolved) == 1,
               f"a category name rebound by {label} must be reported "
               f"unresolved, got {cats} / {unresolved}")
        expect(unresolved and "ISOLATED_FLUID" not in unresolved[0],
               f"the {label} rebinding must not be read as the literal it "
               f"shadows, got {unresolved}")

    # A parameter is a binding whose value is a caller's choice.
    cats, unresolved = extract_issue_categories(
        'def check(grid, issues, cat):\n'
        '    issues.append(Issue(cat, 0, 0, ""))\n', "<synthetic>")
    expect(not cats and len(unresolved) == 1,
           f"a category taken as a parameter must be reported unresolved, "
           f"got {cats} / {unresolved}")

    # A module-level literal must not resolve another function's local name:
    # a function body is its own scope, so the module map never adopts it.
    cats, unresolved = extract_issue_categories(
        'def other(grid, issues):\n'
        '    cat = "ISOLATED_FLUID"\n'
        '    return cat\n'
        '\n'
        'def check(grid, issues):\n'
        '    issues.append(Issue(cat, 0, 0, ""))\n', "<synthetic>")
    expect(not cats and len(unresolved) == 1,
           f"another function's local must not resolve an unbound name, got "
           f"{cats} / {unresolved}")

    # A genuine module-level constant still resolves.
    cats, unresolved = extract_issue_categories(
        'CAT = "MODULE_LEVEL"\n'
        'def check(grid, issues):\n'
        '    issues.append(Issue(CAT, 0, 0, ""))\n', "<synthetic>")
    expect(cats == {"MODULE_LEVEL"} and not unresolved,
           f"a module-level category constant should resolve, got "
           f"{cats} / {unresolved}")

    # A NESTED scope's local must not resolve a name whose real binding is
    # dynamic: an inner literal is invisible at an enclosing call site.
    nested = {
        "an inner function":
            'def check(grid, issues):\n'
            '    def helper():\n'
            '        cat = "ISOLATED_FLUID"\n'
            '        return cat\n'
            '    issues.append(Issue(cat, 0, 0, ""))\n',
        "a sibling function":
            'def helper():\n'
            '    cat = "ISOLATED_FLUID"\n'
            'def check(grid, issues):\n'
            '    issues.append(Issue(cat, 0, 0, ""))\n',
        "a class body":
            'class Holder:\n'
            '    cat = "ISOLATED_FLUID"\n'
            'def check(grid, issues):\n'
            '    issues.append(Issue(cat, 0, 0, ""))\n',
        "a lambda":
            'def check(grid, issues):\n'
            '    make = lambda cat: cat\n'
            '    issues.append(Issue(cat, 0, 0, ""))\n',
    }
    for label, body in nested.items():
        cats, unresolved = extract_issue_categories(
            'cat = compute_category()\n' + body, "<synthetic>")
        expect(not cats and len(unresolved) == 1,
               f"a local in {label} must not resolve an enclosing dynamic "
               f"name, got {cats} / {unresolved}")

    # A real closure read is legitimate Python and must still resolve: the
    # enclosing function IS in scope, unlike a nested or sibling one.
    cats, unresolved = extract_issue_categories(
        'def outer():\n'
        '    cat = "CLOSURE_CAT"\n'
        '    def inner(issues):\n'
        '        issues.append(Issue(cat, 0, 0, ""))\n', "<synthetic>")
    expect(cats == {"CLOSURE_CAT"} and not unresolved,
           f"a closure read of an enclosing local should resolve, got "
           f"{cats} / {unresolved}")

    # A nested definition still binds its NAME in the enclosing scope.
    cats, unresolved = extract_issue_categories(
        'cat = "SHADOWED"\n'
        'def check(grid, issues):\n'
        '    def cat():\n'
        '        pass\n'
        '    issues.append(Issue(cat, 0, 0, ""))\n', "<synthetic>")
    expect(not cats and len(unresolved) == 1,
           f"a nested def shadowing the name must be reported unresolved, "
           f"got {cats} / {unresolved}")


def test_wetland_on_slope() -> None:
    """Wetland soil with ANY 4-neighbour delta > 2 is a BUG (the gate
    is border-aware since 2026-06-07, so cross-chunk counts too);
    flat wetland and submerged bed material are not."""
    print("test_wetland_on_slope")
    # Flat wetland: clean. Coords 1..5 stay inside chunk 0.
    tiles = [tile(x, y, terrainZ=10, matId=64)
             for y in range(1, 6) for x in range(1, 6)]
    r = audit_dump(make_tiles(tiles)).to_dict()
    expect(count_category(r, "WETLAND_ON_SLOPE") == 0,
           f"flat wetland should be clean, got "
           f"{count_category(r, 'WETLAND_ON_SLOPE')}")

    # Raise the centre tile (non-wetland) by 5: its 4 wetland
    # neighbours each see delta 5 > 2.
    spiked = make_tiles(tiles + [tile(3, 3, terrainZ=15, matId=56)])
    r = audit_dump(spiked).to_dict()
    expect(count_category(r, "WETLAND_ON_SLOPE") == 4,
           f"4 wetland neighbours of the spike should flag, got "
           f"{count_category(r, 'WETLAND_ON_SLOPE')}")

    # Cross-chunk delta now FLAGS: wetland at x=15 (chunk 0) next to
    # a +5 cliff at x=16 (chunk 1) — wetlandKeep reads the bordered
    # post-carve vector, so border tiles are gated like interior ones.
    border = make_tiles([
        tile(15, 3, terrainZ=10, matId=64),
        tile(16, 3, terrainZ=15, matId=56),
        tile(14, 3, terrainZ=10, matId=56),
        tile(15, 2, terrainZ=10, matId=56),
        tile(15, 4, terrainZ=10, matId=56),
    ])
    r = audit_dump(border).to_dict()
    expect(count_category(r, "WETLAND_ON_SLOPE") == 1,
           f"cross-chunk delta should flag (border-aware gate), got "
           f"{count_category(r, 'WETLAND_ON_SLOPE')}")

    # Submerged bed material is concealed by the water plane — a
    # steep lake-bed pillar wearing muck must NOT flag.
    sub = make_tiles(
        [tile(x, y, terrainZ=1, matId=50, fluidType="lake", fluidSurf=12)
         for y in range(1, 6) for x in range(1, 6)]
        + [tile(3, 3, terrainZ=10, matId=64, fluidType="lake",
                fluidSurf=12)])
    r = audit_dump(sub).to_dict()
    expect(count_category(r, "WETLAND_ON_SLOPE") == 0,
           f"submerged steep wetland should be exempt, got "
           f"{count_category(r, 'WETLAND_ON_SLOPE')}")


def test_desert_soil_on_slope() -> None:
    """Sand / salt flat on a same-chunk slope is a QUALITY issue."""
    print("test_desert_soil_on_slope")
    # Flat sand: clean.
    tiles = [tile(x, y, terrainZ=10, matId=55)
             for y in range(1, 6) for x in range(1, 6)]
    r = audit_dump(make_tiles(tiles)).to_dict()
    expect(count_category(r, "DESERT_SOIL_ON_SLOPE") == 0,
           f"flat sand should be clean, got "
           f"{count_category(r, 'DESERT_SOIL_ON_SLOPE')}")

    # Spike beside sand and a salt flat: both neighbours flag.
    spiked = make_tiles(tiles + [
        tile(3, 3, terrainZ=15, matId=56),
        tile(2, 3, terrainZ=10, matId=67),  # salt flat next to spike
    ])
    r = audit_dump(spiked).to_dict()
    expect(count_category(r, "DESERT_SOIL_ON_SLOPE") == 4,
           f"3 sand + 1 salt-flat neighbours should flag, got "
           f"{count_category(r, 'DESERT_SOIL_ON_SLOPE')}")


def test_clean_grid() -> None:
    """A clean grid with no issues should return zero bugs."""
    print("test_clean_grid")
    # All ocean below sea level, no issues
    tiles = flat_grid(5, 5, -2, -2, terrainZ=-5, fluidType="ocean", fluidSurf=0)
    result = audit_dump(tiles).to_dict()
    total = sum(result["summary"].values())
    expect(total == 0, f"clean grid should have 0 issues, got {total}: {result['summary']}")


def test_stats() -> None:
    """Fluid stats and elevation stats should be computed correctly."""
    print("test_stats")
    tiles = (
        flat_grid(3, 3, 0, 0, terrainZ=10, fluidType=None) +
        flat_grid(3, 3, 10, 0, terrainZ=-5, fluidType="ocean", fluidSurf=0) +
        flat_grid(3, 3, 20, 0, terrainZ=5, fluidType="river", fluidSurf=8)
    )
    result = audit_dump(tiles)
    d = result.to_dict()
    expect(d["tileCount"] == 27, f"tileCount: {d['tileCount']}, expected 27")
    expect(d["fluidStats"]["dry"] == 9, f"fluidStats.dry: {d['fluidStats']}")
    expect(d["fluidStats"]["ocean"] == 9, f"fluidStats.ocean: {d['fluidStats']}")
    expect(d["fluidStats"]["river"] == 9, f"fluidStats.river: {d['fluidStats']}")
    expect(d["elevationStats"]["min"] == -5, f"min: {d['elevationStats']}")
    expect(d["elevationStats"]["max"] == 10, f"max: {d['elevationStats']}")


def test_determinism_of_audit() -> None:
    """Same input must produce byte-identical audit output."""
    print("test_determinism_of_audit")
    tiles = flat_grid(10, 10, -5, -5, terrainZ=-5, fluidType="ocean", fluidSurf=0)
    a = json.dumps(audit_dump(tiles).to_dict(), sort_keys=True)
    b = json.dumps(audit_dump(tiles).to_dict(), sort_keys=True)
    c = json.dumps(audit_dump(tiles).to_dict(), sort_keys=True)
    expect(a == b and b == c, "audit output not deterministic")


# ----- world_check logic tests ---------------------------------------------

def _result() -> CheckResult:
    return CheckResult(seed=0, world_size=32, region=(0, 0, 0, 0), status=PASS)


def test_check_summary_strict_match() -> None:
    """Deterministic seed whose summary equals the baseline passes."""
    print("test_check_summary_strict_match")
    r = _result()
    base = {"LAKE_HOLE": 4, "FLOATING_LAKE": 300}
    check_issue_summary([dict(base)], base, {}, strict=True, result=r)
    expect(r.status == PASS, f"exact match should PASS, got {r.status}: {r.failures}")
    expect(not r.failures, f"no failures expected, got {r.failures}")


def test_check_summary_strict_regression() -> None:
    """Deterministic count above baseline (under threshold) is a regression."""
    print("test_check_summary_strict_regression")
    r = _result()
    base = {"LAKE_HOLE": 4}
    check_issue_summary([{"LAKE_HOLE": 5}], base, {}, strict=True, result=r)
    expect(r.status == FAIL, f"regression should FAIL, got {r.status}")
    expect(any("regressed above baseline" in f for f in r.failures),
           f"expected regression message, got {r.failures}")


def test_check_summary_strict_improvement() -> None:
    """Deterministic count below baseline is an improvement, not a failure."""
    print("test_check_summary_strict_improvement")
    r = _result()
    base = {"LAKE_HOLE": 4}
    check_issue_summary([{"LAKE_HOLE": 2}], base, {}, strict=True, result=r)
    expect(r.status == IMPROVED, f"improvement should be IMPROVED, got {r.status}")
    expect(not r.failures, f"no failures expected, got {r.failures}")
    expect(any("below baseline" in i for i in r.improvements),
           f"expected improvement message, got {r.improvements}")


def test_check_summary_strict_drop_to_zero() -> None:
    """A baseline category absent from the current summary counts as 0 (improvement)."""
    print("test_check_summary_strict_drop_to_zero")
    r = _result()
    base = {"LAKE_HOLE": 4}
    check_issue_summary([{}], base, {}, strict=True, result=r)
    expect(r.status == IMPROVED, f"drop-to-zero should be IMPROVED, got {r.status}")
    expect(any("LAKE_HOLE" in i for i in r.improvements),
           f"expected LAKE_HOLE improvement, got {r.improvements}")


def test_check_summary_bug_overrides_match() -> None:
    """A BUG category fails even when the deterministic count matches baseline."""
    print("test_check_summary_bug_overrides_match")
    r = _result()
    base = {"TERRAIN_SPIKE": 2}
    check_issue_summary([{"TERRAIN_SPIKE": 2}], base, {}, strict=True, result=r)
    expect(r.status == FAIL, f"nonzero BUG should FAIL despite match, got {r.status}")
    expect(any("must be 0" in f for f in r.failures),
           f"expected must-be-0 message, got {r.failures}")


def test_check_summary_threshold_overrides() -> None:
    """Exceeding the QUALITY threshold fails regardless of strict/baseline."""
    print("test_check_summary_threshold_overrides")
    over = QUALITY_THRESHOLDS["LAKE_HOLE"] + 1
    r = _result()
    # Baseline "matches" the over-threshold value, but the cap still fails.
    check_issue_summary([{"LAKE_HOLE": over}], {"LAKE_HOLE": over}, {},
                        strict=True, result=r)
    expect(r.status == FAIL, f"over-threshold should FAIL, got {r.status}")
    expect(any("exceeds threshold" in f for f in r.failures),
           f"expected threshold message, got {r.failures}")


def test_check_summary_unclassified_category_fails() -> None:
    """A category in neither BUG_CATEGORIES nor QUALITY_CATEGORIES fails the
    seed by name, on both paths and at any count — never tolerated under an
    implicit threshold."""
    print("test_check_summary_unclassified_category_fails")
    for strict, baseline in ((False, {}), (True, {"NEW_CORRUPTION": 1})):
        r = _result()
        check_issue_summary([{"NEW_CORRUPTION": 1}], baseline, {},
                            strict=strict, result=r)
        expect(r.status == FAIL,
               f"unclassified category should FAIL (strict={strict}), "
               f"got {r.status}")
        expect(any("NEW_CORRUPTION" in f and "UNCLASSIFIED" in f
                   for f in r.failures),
               f"failure should name the category as unclassified "
               f"(strict={strict}), got {r.failures}")

    # The old implicit 1000 tolerated a brand-new corruption class up to
    # that count in both modes; a single occurrence must now fail.
    r = _result()
    check_issue_summary([{"NEW_CORRUPTION": 999}], {}, {},
                        strict=False, result=r)
    expect(r.status == FAIL,
           f"a sub-1000 unclassified count should FAIL, got {r.status}")
    expect(not any("threshold 1000" in f for f in r.failures),
           f"no implicit 1000 threshold should survive, got {r.failures}")


def test_check_summary_unclassified_from_baseline_or_envelope() -> None:
    """An unclassified category reaching the check only through the baseline
    or the audit envelope fails too, even at a current count of zero."""
    print("test_check_summary_unclassified_from_baseline_or_envelope")
    r = _result()
    check_issue_summary([{}], {"BASELINE_ONLY": 3}, {}, strict=True, result=r)
    expect(r.status == FAIL,
           f"baseline-only unclassified category should FAIL, got {r.status}")
    expect(any("BASELINE_ONLY" in f for f in r.failures),
           f"failure should name BASELINE_ONLY, got {r.failures}")

    r = _result()
    check_issue_summary([{}], {}, {"ENVELOPE_ONLY": {"max": 7}},
                        strict=False, result=r)
    expect(r.status == FAIL,
           f"envelope-only unclassified category should FAIL, got {r.status}")
    expect(any("ENVELOPE_ONLY" in f for f in r.failures),
           f"failure should name ENVELOPE_ONLY, got {r.failures}")


def test_check_summary_quality_without_threshold_fails() -> None:
    """A QUALITY category with no QUALITY_THRESHOLDS entry fails by name
    rather than silently receiving an implicit default."""
    print("test_check_summary_quality_without_threshold_fails")
    cat = "QUALITY_NO_THRESHOLD"
    QUALITY_CATEGORIES.add(cat)
    try:
        for strict, baseline in ((False, {}), (True, {cat: 1})):
            r = _result()
            check_issue_summary([{cat: 1}], baseline, {},
                                strict=strict, result=r)
            expect(r.status == FAIL,
                   f"threshold-less QUALITY category should FAIL "
                   f"(strict={strict}), got {r.status}")
            expect(any(cat in f and "no explicit threshold" in f
                       for f in r.failures),
                   f"failure should name the category and the missing "
                   f"threshold (strict={strict}), got {r.failures}")

        # Baseline-only, current count zero: still a failure.
        r = _result()
        check_issue_summary([{}], {cat: 0}, {}, strict=True, result=r)
        expect(r.status == FAIL,
               f"threshold-less QUALITY category should FAIL at count 0, "
               f"got {r.status}")
    finally:
        QUALITY_CATEGORIES.discard(cat)

    # The mutation is undone, so the category is unclassified again.
    expect(classify_category(cat) is None,
           f"{cat} should be unclassified after the test restores the set")


def test_check_summary_racy_no_match_required() -> None:
    """Racy seeds don't require an exact match; under-threshold drift is a note."""
    print("test_check_summary_racy_no_match_required")
    r = _result()
    base = {"LAKE_HOLE": 2}
    env = {"LAKE_HOLE": {"min": 2, "max": 2}}
    # 5 != baseline 2, but it's under the threshold (25); racy mode must
    # not fail on the mismatch (the strict match rule does not apply).
    check_issue_summary([{"LAKE_HOLE": 5}], base, env, strict=False, result=r)
    expect(r.status == PASS, f"racy under-threshold mismatch should PASS, got {r.status}")
    expect(not r.failures, f"racy mode should not fail on mismatch, got {r.failures}")


def test_check_determinism_regression() -> None:
    """A seed that was deterministic and is now racy fails."""
    print("test_check_determinism_regression")
    r = _result()
    check_determinism_status(deterministic_baseline=True, deterministic_now=False,
                             n_distinct=3, runs=3, result=r)
    expect(r.status == FAIL, f"determinism regression should FAIL, got {r.status}")
    expect(any("determinism regression" in f for f in r.failures),
           f"expected determinism-regression message, got {r.failures}")


def test_check_determinism_improvement() -> None:
    """A seed that was racy and is now deterministic across runs>1 improves."""
    print("test_check_determinism_improvement")
    r = _result()
    check_determinism_status(deterministic_baseline=False, deterministic_now=True,
                             n_distinct=1, runs=3, result=r)
    expect(r.status == IMPROVED, f"racy->det should be IMPROVED, got {r.status}")


def test_check_determinism_single_run_safe() -> None:
    """With runs==1 a deterministic baseline can't trip a false regression."""
    print("test_check_determinism_single_run_safe")
    r = _result()
    check_determinism_status(deterministic_baseline=True, deterministic_now=True,
                             n_distinct=1, runs=1, result=r)
    expect(r.status == PASS, f"single-run det should stay PASS, got {r.status}")
    expect(not r.failures, f"no failures expected, got {r.failures}")


# ----- Runner --------------------------------------------------------------

def main() -> int:
    tests = [
        test_clean_grid,
        test_stats,
        test_determinism_of_audit,
        test_dry_below_sea,
        test_ocean_on_land,
        test_river_under_terrain,
        test_lake_under_terrain,
        test_floating_fluid,
        test_terrain_spike,
        test_terrain_pit,
        test_terrain_pit_submerged,
        test_terrain_spike_submerged,
        test_river_chunk_gap,
        test_river_mouth_drop,
        test_island_1tile,
        test_isolated_fluid,
        test_minbound_leak,
        test_surface_inconsistent,
        test_river_surface_uses_fluid_not_max,
        test_lake_surface_uses_max,
        test_dry_below_sea_inland_basin,
        test_dry_below_sea_ocean_connected,
        test_severity_classification,
        test_category_extraction_resolves_and_fails_loudly,
        test_wetland_on_slope,
        test_desert_soil_on_slope,
        test_check_summary_strict_match,
        test_check_summary_strict_regression,
        test_check_summary_strict_improvement,
        test_check_summary_strict_drop_to_zero,
        test_check_summary_bug_overrides_match,
        test_check_summary_threshold_overrides,
        test_check_summary_racy_no_match_required,
        test_check_summary_unclassified_category_fails,
        test_check_summary_unclassified_from_baseline_or_envelope,
        test_check_summary_quality_without_threshold_fails,
        test_check_determinism_regression,
        test_check_determinism_improvement,
        test_check_determinism_single_run_safe,
    ]

    for t in tests:
        t()
        print()

    if FAILURES:
        print(f"\n{len(FAILURES)} test failure(s):")
        for f in FAILURES:
            print(f"  {f}")
        return 1

    print(f"\nAll {len(tests)} test groups passed")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
