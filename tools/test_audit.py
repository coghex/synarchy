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
import contextlib
import io
import json
import sys
import tempfile
from pathlib import Path
from typing import Any

sys.path.insert(0, str(Path(__file__).resolve().parent))
import world_audit  # type: ignore
from world_audit import (  # type: ignore
    audit_dump, INT64_MIN, neighbors4, severity_of, classify_category,
    BUG_CATEGORIES, QUALITY_CATEGORIES, QUALITY_THRESHOLDS,
)
import world_baseline  # type: ignore
import world_check  # type: ignore
from world_check import (  # type: ignore
    CheckResult, check_issue_summary, check_determinism_status,
    check_seed, format_result, PASS, FAIL, IMPROVED, SKIP,
)

import selftest  # noqa: E402
from selftest import FAILURES, expect  # noqa: E402


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
    # Lava with fluidSurf - terrainZ > 15. The lava depth metric is named
    # DEEP_LAVA_COLUMN (#1876) because that is all it measures; rim
    # geometry is test_lava_rim_containment below.
    tiles = flat_grid(3, 3, -1, -1, terrainZ=-50, fluidType="lava", fluidSurf=5)
    result = audit_dump(tiles).to_dict()
    expect(count_category(result, "DEEP_LAVA_COLUMN") == 9,
           f"DEEP_LAVA_COLUMN count: "
           f"{count_category(result, 'DEEP_LAVA_COLUMN')}, expected 9")
    expect(count_category(result, "FLOATING_LAVA") == 0,
           "FLOATING_LAVA must no longer name a depth-only lava count")

    # River with high depth
    tiles = flat_grid(3, 3, -1, -1, terrainZ=-50, fluidType="river", fluidSurf=5)
    result = audit_dump(tiles).to_dict()
    expect(count_category(result, "FLOATING_RIVER") == 9,
           f"FLOATING_RIVER count: {count_category(result, 'FLOATING_RIVER')}, expected 9")

    # Ocean at any depth should NOT trigger floating
    tiles = flat_grid(3, 3, -1, -1, terrainZ=-100, fluidType="ocean", fluidSurf=0)
    result = audit_dump(tiles).to_dict()
    expect(count_category(result, "DEEP_LAVA_COLUMN") == 0
           and count_category(result, "FLOATING_RIVER") == 0
           and count_category(result, "FLOATING_LAKE") == 0,
           "deep ocean should not trigger the depth metrics")


def issue_coords(result_dict: dict[str, Any], cat: str) -> list[tuple[int, int]]:
    return sorted((i["x"], i["y"]) for i in result_dict["issues"].get(cat, []))


def test_lava_rim_containment() -> None:
    """Containment is rim geometry, not column depth (#1876).

    The old FLOATING_LAVA predicate was `fluidSurf - terrainZ > 15` and
    inspected no neighbour, so it counted every deep pool — including the
    fully contained ones `World.Magma.Pool` is supposed to produce. These
    cases drive the real audit and pin the contained-versus-breached
    distinction the threshold raise in PR #255 established by hand and
    never encoded.
    """
    print("test_lava_rim_containment")

    # (a) The five-cell fixture from the issue: a 20-deep pool whose whole
    #     dry rim sits AT the lava surface. Contained at any depth.
    contained = [
        tile(0, 0, terrainZ=-20, fluidType="lava", fluidSurf=0),
        tile(-1, 0, terrainZ=0), tile(1, 0, terrainZ=0),
        tile(0, -1, terrainZ=0), tile(0, 1, terrainZ=0),
    ]
    result = audit_dump(contained).to_dict()
    expect(count_category(result, "LAVA_RIM_BREACH") == 0,
           f"contained deep pool: LAVA_RIM_BREACH "
           f"{count_category(result, 'LAVA_RIM_BREACH')}, expected 0")
    expect(count_category(result, "LAVA_RIM_INCOMPLETE") == 0,
           f"contained deep pool has all four neighbours present: "
           f"LAVA_RIM_INCOMPLETE "
           f"{count_category(result, 'LAVA_RIM_INCOMPLETE')}, expected 0")
    # The depth metric survives the rename and still sees this column.
    expect(count_category(result, "DEEP_LAVA_COLUMN") == 1,
           f"contained deep pool is still a deep COLUMN: DEEP_LAVA_COLUMN "
           f"{count_category(result, 'DEEP_LAVA_COLUMN')}, expected 1")

    # (b) The same pool with ONE rim tile a single z lower is breached.
    breached = [
        tile(0, 0, terrainZ=-20, fluidType="lava", fluidSurf=0),
        tile(-1, 0, terrainZ=-1), tile(1, 0, terrainZ=0),
        tile(0, -1, terrainZ=0), tile(0, 1, terrainZ=0),
    ]
    result = audit_dump(breached).to_dict()
    expect(issue_coords(result, "LAVA_RIM_BREACH") == [(0, 0)],
           f"one dry rim tile below the surface breaches the pool: "
           f"{issue_coords(result, 'LAVA_RIM_BREACH')}, expected [(0, 0)]")

    # (c) Equal elevation is contained, strictly below is not — the exact
    #     boundary, checked from both sides on the same geometry.
    for rim_z, expected in ((0, 0), (-1, 1)):
        tiles = [
            tile(0, 0, terrainZ=-20, fluidType="lava", fluidSurf=0),
            tile(-1, 0, terrainZ=rim_z), tile(1, 0, terrainZ=rim_z),
            tile(0, -1, terrainZ=rim_z), tile(0, 1, terrainZ=rim_z),
        ]
        result = audit_dump(tiles).to_dict()
        expect(count_category(result, "LAVA_RIM_BREACH") == expected,
               f"rim at terrainZ={rim_z} vs fluidSurf=0: LAVA_RIM_BREACH "
               f"{count_category(result, 'LAVA_RIM_BREACH')}, "
               f"expected {expected}")

    # (d) One offending TILE, not one offending edge: all four rim tiles
    #     below the surface still emit a single occurrence.
    tiles = [
        tile(0, 0, terrainZ=-20, fluidType="lava", fluidSurf=0),
        tile(-1, 0, terrainZ=-5), tile(1, 0, terrainZ=-5),
        tile(0, -1, terrainZ=-5), tile(0, 1, terrainZ=-5),
    ]
    result = audit_dump(tiles).to_dict()
    expect(count_category(result, "LAVA_RIM_BREACH") == 1,
           f"four lower rim tiles are one offending lava tile: "
           f"LAVA_RIM_BREACH {count_category(result, 'LAVA_RIM_BREACH')}, "
           f"expected 1")

    # (e) Water is a barrier the pool stops against, not a rim breach —
    #     World.Magma.Pool.isWater treats ocean/lake/river the same way.
    tiles = [
        tile(0, 0, terrainZ=-20, fluidType="lava", fluidSurf=0),
        tile(-1, 0, terrainZ=-30, fluidType="ocean", fluidSurf=0),
        tile(1, 0, terrainZ=-10, fluidType="lake", fluidSurf=-5),
        tile(0, -1, terrainZ=-10, fluidType="river", fluidSurf=-5),
        tile(0, 1, terrainZ=0),
    ]
    result = audit_dump(tiles).to_dict()
    expect(count_category(result, "LAVA_RIM_BREACH") == 0,
           f"ocean/lake/river neighbours are barriers: LAVA_RIM_BREACH "
           f"{count_category(result, 'LAVA_RIM_BREACH')}, expected 0")

    # (f) The world-boundary sentinel is a barrier too, on both spellings.
    tiles = [
        tile(0, 0, terrainZ=-20, fluidType="lava", fluidSurf=0),
        tile(-1, 0, terrainZ=INT64_MIN, beyondGlacier=True),
        tile(1, 0, terrainZ=INT64_MIN),
        tile(0, -1, terrainZ=0), tile(0, 1, terrainZ=0),
    ]
    result = audit_dump(tiles).to_dict()
    expect(count_category(result, "LAVA_RIM_BREACH") == 0,
           f"sentinel neighbours are barriers: LAVA_RIM_BREACH "
           f"{count_category(result, 'LAVA_RIM_BREACH')}, expected 0")

    # (g) A neighbour outside the dumped region proves nothing either way:
    #     the tile records an incomplete judgement and no breach.
    edge = [tile(0, 0, terrainZ=-20, fluidType="lava", fluidSurf=0)]
    result = audit_dump(edge).to_dict()
    expect(count_category(result, "LAVA_RIM_BREACH") == 0,
           f"absent neighbours never emit a breach: LAVA_RIM_BREACH "
           f"{count_category(result, 'LAVA_RIM_BREACH')}, expected 0")
    expect(issue_coords(result, "LAVA_RIM_INCOMPLETE") == [(0, 0)],
           f"a region-edge lava tile records one incomplete judgement: "
           f"{issue_coords(result, 'LAVA_RIM_INCOMPLETE')}, "
           f"expected [(0, 0)]")

    # (h) The two records are independent: a tile can be provably breached
    #     on a present neighbour while another is off-window.
    tiles = [
        tile(0, 0, terrainZ=-20, fluidType="lava", fluidSurf=0),
        tile(1, 0, terrainZ=-5),
    ]
    result = audit_dump(tiles).to_dict()
    expect(issue_coords(result, "LAVA_RIM_BREACH") == [(0, 0)]
           and issue_coords(result, "LAVA_RIM_INCOMPLETE") == [(0, 0)],
           f"breach and incomplete coexist: breach "
           f"{issue_coords(result, 'LAVA_RIM_BREACH')}, incomplete "
           f"{issue_coords(result, 'LAVA_RIM_INCOMPLETE')}, "
           f"expected both [(0, 0)]")

    # (i) A higher pool draining into a lower one is perched. The
    #     occurrence is filed against the HIGHER tile, and the lower tile
    #     — which is supported everywhere — reports nothing.
    perched = flat_grid(4, 3, -1, -1, terrainZ=10, fluidType=None)
    perched = make_tiles(perched)
    for i, cell in enumerate(perched):
        if (cell["x"], cell["y"]) == (0, 0):
            perched[i] = tile(0, 0, terrainZ=0, fluidType="lava", fluidSurf=10)
        elif (cell["x"], cell["y"]) == (1, 0):
            perched[i] = tile(1, 0, terrainZ=0, fluidType="lava", fluidSurf=2)
    result = audit_dump(perched).to_dict()
    expect(issue_coords(result, "LAVA_RIM_BREACH") == [(0, 0)],
           f"a lava neighbour with a strictly lower surface breaches the "
           f"HIGHER tile: {issue_coords(result, 'LAVA_RIM_BREACH')}, "
           f"expected [(0, 0)]")
    expect(count_category(result, "LAVA_RIM_INCOMPLETE") == 0,
           f"both perched-pair tiles have four present neighbours: "
           f"LAVA_RIM_INCOMPLETE "
           f"{count_category(result, 'LAVA_RIM_INCOMPLETE')}, expected 0")


FIXTURE_DIR = Path(__file__).resolve().parent / "fixtures"

# A real `--dump` window, not a hand-built grid: seed 12321, worldSize 32,
# region -4,-4,4,4 (a tracked baseline seed), trimmed to the tiles within 3
# of its lava pool so every lava tile keeps all four real cardinal
# neighbours. Regenerate with:
#   cabal run -v0 exe:synarchy -- --dump --seed 12321 --worldSize 32 \
#       --region -4,-4,4,4
# then keep -12 <= x <= 4, 65 <= y <= 77.
REAL_LAVA_DUMP = FIXTURE_DIR / "dump_seed12321_lava_pool.json"


def real_lava_dump() -> list[dict[str, Any]]:
    return world_audit.load_dump_file(REAL_LAVA_DUMP)


def test_lava_rim_on_real_generated_output() -> None:
    """The containment check against REAL generator output, not a fixture.

    This is the case that keeps the predicate honest. On the shipped
    pipeline a pool's raw rim never reaches `--dump`: `poolRimCaps` raises
    every OUTERMOST pool tile to the pool surface as a basalt cap,
    `applyBasaltCaps` writes that terrain, and `applyLavaShell` then strips
    the zero-depth lava film off it (`src/World/Generate/Chunk.hs`). What
    the audit sees is the SEALED rim — a basalt wall flush with the lava —
    so a pool that `World.Magma.Pool.grow` truncated at its area or radius
    bound has already been repaired by the time the dump exists.

    So LAVA_RIM_BREACH is a live guard on that SEALING pipeline, and the
    zero it measures across the baselines is evidence the seal holds — not
    evidence that `grow` never truncates, which post-cap data cannot show
    either way. The mutation below is what proves the check is a guard
    rather than dead code: undo one real tile's cap and the breach appears.
    """
    print("test_lava_rim_on_real_generated_output")
    data = real_lava_dump()
    grid = {(t["x"], t["y"]): t for t in data}
    lava = [t for t in data if t["fluidType"] == "lava"]
    expect(len(lava) == 23,
           f"fixture holds the seed-12321 pool: {len(lava)} lava tiles, "
           f"expected 23")

    result = audit_dump(data).to_dict()
    # The same count seed 12321's tracked baseline records, so the fixture
    # is demonstrably that seed's real output and not a reduction of it.
    expect(count_category(result, "DEEP_LAVA_COLUMN") == 11,
           f"real dump reproduces the baseline's deep-column count: "
           f"{count_category(result, 'DEEP_LAVA_COLUMN')}, expected 11")
    expect(count_category(result, "LAVA_RIM_BREACH") == 0,
           f"the sealed rim is contained: LAVA_RIM_BREACH "
           f"{count_category(result, 'LAVA_RIM_BREACH')}, expected 0")
    expect(count_category(result, "LAVA_RIM_INCOMPLETE") == 0,
           f"every fixture lava tile has four present neighbours: "
           f"LAVA_RIM_INCOMPLETE "
           f"{count_category(result, 'LAVA_RIM_INCOMPLETE')}, expected 0")

    # WHY that zero holds, stated as an assertion rather than a comment:
    # every dry rim tile sits EXACTLY at the pool surface, which is what
    # poolRimCaps writes. A count of zero with no dry rim at all would be
    # vacuous; this shows the rim is really there and really flush.
    rim: list[tuple[tuple[int, int], tuple[int, int]]] = []
    off_surface = []
    for t in lava:
        for nbr in neighbors4(t["x"], t["y"]):
            n = grid.get(nbr)
            if n is None or n["fluidType"] is not None:
                continue
            rim.append((nbr, (t["x"], t["y"])))
            if n["terrainZ"] != t["fluidSurf"]:
                off_surface.append((nbr, n["terrainZ"], t["fluidSurf"]))
    expect(len(rim) == 36,
           f"the pool really has a dry rim to judge: {len(rim)} rim "
           f"adjacencies, expected 36")
    expect(not off_surface,
           f"every dry rim tile is flush with the lava surface "
           f"(poolRimCaps): off-surface {off_surface[:5]}")

    # Mutation 1 — undo one real tile's cap. This is what the rim looked
    # like before poolRimCaps raised it, and what a regression in that
    # sealing pass would leave behind.
    rim.sort()
    (rx, ry), (lx, ly) = rim[0]
    mutated = [dict(t) for t in data]
    for t in mutated:
        if (t["x"], t["y"]) == (rx, ry):
            t["terrainZ"] -= 1
            t["surfaceZ"] = t["terrainZ"]
    result = audit_dump(mutated).to_dict()
    expect(issue_coords(result, "LAVA_RIM_BREACH") == [(lx, ly)],
           f"lowering the real rim tile {(rx, ry)} by one z breaches the "
           f"lava tile beside it: {issue_coords(result, 'LAVA_RIM_BREACH')}, "
           f"expected [{(lx, ly)}]")

    # Mutation 2 — drop one real cardinal neighbour out of the window, the
    # way a dump region boundary would cut it.
    trimmed = [t for t in data if (t["x"], t["y"]) != (rx, ry)]
    result = audit_dump(trimmed).to_dict()
    expect((lx, ly) in issue_coords(result, "LAVA_RIM_INCOMPLETE"),
           f"a real lava tile whose neighbour left the window records an "
           f"incomplete judgement: "
           f"{issue_coords(result, 'LAVA_RIM_INCOMPLETE')}, expected to "
           f"contain {(lx, ly)}")
    expect(count_category(result, "LAVA_RIM_BREACH") == 0,
           f"and still reports no breach: LAVA_RIM_BREACH "
           f"{count_category(result, 'LAVA_RIM_BREACH')}, expected 0")


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
               "DEEP_LAVA_COLUMN", "FLOATING_RIVER", "FLOATING_LAKE",
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


def test_issue_ordering_is_canonical() -> None:
    """Serialized audit output is canonical in its ISSUE ORDER.

    `AuditResult.to_dict` sorts issues by (category, x, y) before grouping
    them. That sort is what makes an audit's issue arrays comparable
    between runs and keeps a re-captured baseline file's diff stable;
    `json.dumps(sort_keys=True)` cannot stand in for it, because it orders
    an object's KEYS and leaves its arrays exactly as built. Nothing else
    in this suite pins it: every per-category fixture asserts through
    `count_category`, which counts issues without looking at their order.

    Feeding ONE fixture through `audit_dump` in two different input orders
    and requiring byte-identical serializations tests exactly that. A
    same-input comparison cannot: `audit_dump` builds fresh state from its
    argument and reads no clock, randomness, or retained cross-call state,
    so two identical calls are equal however the sort is written — or
    whether it is written at all.

    LIMITATION, and why this fixture is built the way it is: the
    (category, x, y) key is not total. Two issues sharing all three and
    differing only in `details` keep their append order under Python's
    stable `sorted`, and append order follows the input list, so such a
    pair would make this test fail legitimately rather than reveal a
    regression. The fixture below therefore uses coordinates that are
    DISTINCT within each category, which the guards ahead of the
    comparison assert rather than assume; any fixture substituted here
    must hold the same property.
    """
    print("test_issue_ordering_is_canonical")
    # Two coordinate-disjoint clusters, each flagging four issues in one
    # category: wetland soil (matId 64) around a +5 spike in chunk 0, and
    # sand / salt flat (matId 55 / 67) around a second spike in chunk 1.
    wetland = [tile(x, y, terrainZ=10, matId=64)
               for y in range(1, 6) for x in range(1, 6)]
    wetland.append(tile(3, 3, terrainZ=15, matId=56))
    desert = [tile(x, y, terrainZ=10, matId=55)
              for y in range(1, 6) for x in range(17, 22)]
    desert.append(tile(19, 3, terrainZ=15, matId=56))
    desert.append(tile(18, 3, terrainZ=10, matId=67))

    # Normalize BEFORE permuting. `audit_dump` builds its grid with
    # last-write-wins duplicate-coordinate semantics, so reversing a list
    # still holding unresolved duplicates would change the INPUT, not just
    # its order, and the two runs would no longer be permutations.
    forward = make_tiles(wetland + desert)
    reverse = list(reversed(forward))

    result = audit_dump(forward)
    triples = [(i.category, i.x, i.y) for i in result.issues]
    per_category: dict[str, int] = {}
    for category, _x, _y in triples:
        per_category[category] = per_category.get(category, 0) + 1

    # Guards: without them a fixture that drifted to one issue, or to one
    # issue per category, would keep passing while pinning nothing —
    # sorting a singleton array is a no-op, so removing the sort would
    # stay invisible.
    expect(len(per_category) >= 2,
           f"fixture must produce at least two categories, got "
           f"{sorted(per_category)}")
    expect(any(n > 1 for n in per_category.values()),
           f"fixture must produce a category holding several issues, got "
           f"{per_category}")
    expect(len(triples) == len(set(triples)),
           f"fixture must not repeat a (category, x, y) triple — the sort "
           f"key is not total, so duplicates would order by append: "
           f"{sorted(triples)}")

    forward_json = json.dumps(result.to_dict(), sort_keys=True)
    reverse_json = json.dumps(audit_dump(reverse).to_dict(), sort_keys=True)
    expect(forward_json == reverse_json,
           "audit output is not canonical under input reordering: the same "
           "tiles fed in reverse serialized differently, so to_dict's "
           "(category, x, y) issue sort is missing or weakened")


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


# ----- Baseline content-hash gate (#1361) ----------------------------------
#
# These drive the real world_check.check_seed over synthetic dumps rather
# than re-implementing hash equality here: the baseline fixture is built
# by the real world_baseline.capture_seed, and every pass/fail decision
# below is made by production code. A test that compared hashes itself
# would still pass if check_seed stopped calling check_baseline_hash.

HASH_ENTRY = {"seed": 4242, "world_size": 32, "region": [-1, -1, 1, 1]}


def hash_tile(x: int, y: int, matId: int = 64) -> dict[str, Any]:
    """A dry, flat, audit-clean land tile."""
    return {
        "x": x, "y": y, "v": x + y,
        "terrainZ": 10, "surfaceZ": 10,
        "matId": matId,
        "fluidType": None, "fluidSurf": None,
        "iceSurf": None, "iceMode": None,
        "glacierZone": False, "beyondGlacier": False,
    }


def hash_dump_fixture(matId_at_index: tuple[int, int] | None = None
                      ) -> list[dict[str, Any]]:
    """A 6x6 flat dry grid, optionally with one tile's matId changed.

    matId is the point: 64 (muck) and 70 both sit on flat terrain, so
    neither trips WETLAND_ON_SLOPE or DESERT_SOIL_ON_SLOPE, and matId
    appears in no statistic world_check compares. A one-tile change is
    therefore invisible to tileCount, elevationStats, fluidStats and the
    audit summary alike — exactly the drift class the content hash
    exists to catch.
    """
    tiles = [hash_tile(x, y) for y in range(6) for x in range(6)]
    if matId_at_index is not None:
        index, matId = matId_at_index
        tiles[index] = dict(tiles[index], matId=matId)
    return tiles


def capture_hash_baseline(dumps: list[list[dict[str, Any]]]) -> dict[str, Any]:
    """Build a baseline the way world_baseline.py really builds one."""
    pending = list(dumps)
    original = world_baseline.run_dump
    world_baseline.run_dump = lambda *a, **k: pending.pop(0)
    try:
        return world_baseline.capture_seed(
            HASH_ENTRY["seed"], HASH_ENTRY["world_size"],
            tuple(HASH_ENTRY["region"]), len(dumps),
        )
    finally:
        world_baseline.run_dump = original


def run_check_seed(baseline: dict[str, Any],
                   current: list[list[dict[str, Any]]]) -> CheckResult:
    """Run the production check_seed against a real on-disk baseline."""
    pending = list(current)
    original_run = world_check.run_dump
    original_path = world_check.baseline_path
    with tempfile.TemporaryDirectory() as tmp:
        path = Path(tmp) / "baseline.json"
        path.write_text(json.dumps(baseline, indent=2) + "\n")
        world_check.run_dump = lambda *a, **k: pending.pop(0)
        world_check.baseline_path = lambda *a, **k: path
        try:
            return check_seed(HASH_ENTRY, runs=len(current))
        finally:
            world_check.run_dump = original_run
            world_check.baseline_path = original_path


def test_baseline_hash_match_passes() -> None:
    """A dump reproducing its baseline's recorded hash passes."""
    print("test_baseline_hash_match_passes")
    clean = hash_dump_fixture()
    baseline = capture_hash_baseline([clean, clean, clean])
    expect(baseline["determinism"]["distinctHashes"] == 1,
           f"fixture baseline should be deterministic, got "
           f"{baseline['determinism']['distinctHashes']} distinct hashes")

    r = run_check_seed(baseline, [hash_dump_fixture()])
    expect(r.status == PASS, f"matching hash should PASS, got {r.status}: {r.failures}")
    expect(not r.failures, f"no failures expected, got {r.failures}")
    expect(not r.banners,
           f"a gated deterministic seed needs no banner, got {r.banners}")


def test_baseline_hash_mismatch_fails_on_aggregate_preserving_drift() -> None:
    """A matId change no statistic models still fails, naming the seed.

    This is the false-pass class the gate was added for: every existing
    comparison sees identical values, so only the content hash can flag it.
    """
    print("test_baseline_hash_mismatch_fails_on_aggregate_preserving_drift")
    clean = hash_dump_fixture()
    baseline = capture_hash_baseline([clean, clean, clean])
    drifted = hash_dump_fixture(matId_at_index=(7, 70))

    # The drift really is invisible to every other comparison.
    a = audit_dump(clean, seed=HASH_ENTRY["seed"])
    b = audit_dump(drifted, seed=HASH_ENTRY["seed"])
    expect(a.tile_count == b.tile_count
           and a.elevation_stats == b.elevation_stats
           and a.fluid_stats == b.fluid_stats
           and a.summary() == b.summary(),
           "matId fixture must be aggregate-preserving for this test to mean "
           f"anything: {a.summary()} vs {b.summary()}")

    r = run_check_seed(baseline, [drifted])
    expect(r.status == FAIL, f"content drift should FAIL, got {r.status}")
    hash_failures = [f for f in r.failures if "content hash mismatch" in f]
    expect(len(hash_failures) == 1,
           f"expected exactly one content-hash failure, got {r.failures}")
    if hash_failures:
        message = hash_failures[0]
        expect(f"seed={HASH_ENTRY['seed']}" in message,
               f"failure must name the seed, got {message!r}")
        expect(baseline["determinism"]["hashes"][0] in message,
               f"failure must expose the expected hash, got {message!r}")
        expect(world_check.hash_dump(drifted) in message,
               f"failure must expose the actual hash, got {message!r}")


def test_baseline_hash_racy_baseline_is_announced_not_gated() -> None:
    """A multi-hash baseline is neither silently passed nor silently failed."""
    print("test_baseline_hash_racy_baseline_is_announced_not_gated")
    clean = hash_dump_fixture()
    variant = hash_dump_fixture(matId_at_index=(7, 70))
    baseline = capture_hash_baseline([clean, variant, clean])
    expect(baseline["determinism"]["distinctHashes"] == 2,
           f"fixture baseline should be racy, got "
           f"{baseline['determinism']['distinctHashes']} distinct hashes")

    # A current dump matching NEITHER recorded hash: three samples of a
    # race do not enumerate its outcomes, so this must not fail.
    unseen = hash_dump_fixture(matId_at_index=(11, 71))
    expect(world_check.hash_dump(unseen)
           not in baseline["determinism"]["hashes"],
           "the racy-case fixture must not accidentally match a recorded hash")

    r = run_check_seed(baseline, [unseen])
    expect(not any("content hash" in f for f in r.failures),
           f"a racy baseline must not gate content identity, got {r.failures}")
    banners = [b for b in r.banners if "racy baseline" in b]
    expect(len(banners) == 1,
           f"expected one racy-baseline banner, got {r.banners}")

    # ...and the banner must survive the normal, non-verbose output, which
    # prints format_result only. Notes are suppressed on a PASS, so a note
    # would be exactly the silent pass this rule forbids.
    line = format_result(r)
    expect(all(b in line for b in banners),
           f"banner must ride the seed's own output line: {line!r}")


def test_baseline_hash_inconsistent_baseline_fails() -> None:
    """A baseline whose deterministic flag contradicts its hashes fails."""
    print("test_baseline_hash_inconsistent_baseline_fails")
    clean = hash_dump_fixture()
    baseline = capture_hash_baseline([clean, clean, clean])
    baseline["determinism"]["deterministic"] = False

    r = run_check_seed(baseline, [hash_dump_fixture()])
    expect(r.status == FAIL,
           f"a self-contradictory baseline should FAIL, got {r.status}")
    expect(any("malformed baseline" in f for f in r.failures),
           f"expected a malformed-baseline failure, got {r.failures}")


def test_check_determinism_inactive_at_one_run() -> None:
    """At runs==1 the determinism rule records nothing, in either direction.

    The guard is explicit rather than incidental: one dump cannot be
    compared with itself, so this branch must not read as though it gates
    something. Content coverage at this setting comes from the baseline
    hash instead.
    """
    print("test_check_determinism_inactive_at_one_run")
    r = _result()
    check_determinism_status(deterministic_baseline=True, deterministic_now=False,
                             n_distinct=2, runs=1, result=r)
    expect(r.status == PASS,
           f"determinism status must be inactive at runs==1, got {r.status}")
    expect(not r.failures and not r.improvements,
           f"nothing should be recorded at runs==1, got "
           f"{r.failures} / {r.improvements}")


# ----- Strict capture invariants (#1598) -----------------------------------
#
# world_check.py compares tileCount and elevationStats min/max/median/count
# for exact equality with no envelope, so world_baseline.py must refuse to
# publish a seed whose capture runs disagreed on any of them rather than
# recording run 0. These drive the real world_baseline.capture_seed and the
# real world_baseline.main() over synthetic dumps — no world is generated.
#
# All five invariants are pinned individually. The two tools hard-code the
# strict set separately (Requirement 7 forbids touching world_check.py), so
# only per-invariant coverage catches the two drifting apart again.

STRICT_SEED = 5150
STRICT_ENTRY = {"seed": STRICT_SEED, "world_size": 32, "region": [-1, -1, 1, 1],
                "description": "strict invariant fixture", "quick": True}
STRICT_ENTRY_B = {"seed": 5151, "world_size": 32, "region": [-1, -1, 1, 1],
                  "description": "second strict fixture", "quick": True}


def strict_tile(x: int, y: int, terrainZ: int = 10,
                beyondGlacier: bool = False) -> dict[str, Any]:
    """A dry, flat, audit-clean tile at an explicit elevation."""
    return {
        "x": x, "y": y, "v": x + y,
        "terrainZ": terrainZ, "surfaceZ": terrainZ,
        "matId": 64,
        "fluidType": None, "fluidSurf": None,
        "iceSurf": None, "iceMode": None,
        "glacierZone": False, "beyondGlacier": beyondGlacier,
    }


def strict_dump(elevations: list[int],
                excluded: int = 0) -> list[dict[str, Any]]:
    """A dump whose real-terrain elevations are exactly `elevations`.

    `excluded` appends tiles marked beyondGlacier, which
    world_audit.compute_stats filters out of the elevation domain. That
    separation is what lets each invariant be varied on its own: tileCount
    can move while every elevation statistic holds still, and vice versa.
    """
    tiles = [strict_tile(i, 0, terrainZ=z) for i, z in enumerate(elevations)]
    tiles += [strict_tile(i, 1, beyondGlacier=True) for i in range(excluded)]
    return tiles


def capture_strict(dumps: list[list[dict[str, Any]]],
                   seed: int = STRICT_SEED) -> dict[str, Any]:
    """Run the real world_baseline.capture_seed over `dumps`."""
    pending = list(dumps)
    original = world_baseline.run_dump
    world_baseline.run_dump = lambda *a, **k: pending.pop(0)
    try:
        with contextlib.redirect_stderr(io.StringIO()):
            return world_baseline.capture_seed(
                seed, STRICT_ENTRY["world_size"],
                tuple(STRICT_ENTRY["region"]), len(dumps))
    finally:
        world_baseline.run_dump = original


def expect_strict_capture_failure(label: str,
                                  dumps: list[list[dict[str, Any]]],
                                  invariant: str,
                                  values: list[Any]) -> str:
    """Assert capture refuses `dumps`, naming seed, invariant and values."""
    try:
        capture_strict(dumps)
    except RuntimeError as exc:
        message = str(exc)
    else:
        expect(False, f"{label}: varying {invariant} must refuse the baseline")
        return ""

    expect(f"seed {STRICT_SEED}" in message,
           f"{label}: failure must name the seed, got {message!r}")
    expect(invariant in message,
           f"{label}: failure must name {invariant}, got {message!r}")
    for value in values:
        expect(repr(value) in message,
               f"{label}: failure must report observed {value!r}, "
               f"got {message!r}")
    return message


def test_strict_capture_accepts_a_stable_seed() -> None:
    """An unvarying capture is written exactly as before.

    The fixtures below differ from this one in a single invariant each, so
    this case is what proves a refusal is caused by that difference rather
    than by the new check rejecting everything.
    """
    print("test_strict_capture_accepts_a_stable_seed")
    stable = strict_dump([10, 10, 10, 20, 20, 20], excluded=2)
    baseline = capture_strict([stable, stable, stable])
    expect(baseline["tileCount"] == 8,
           f"stable capture records its tile count, got {baseline['tileCount']}")
    expect(baseline["elevationStats"] == {"min": 10, "max": 20,
                                          "median": 15, "count": 6},
           f"stable capture records its elevation stats, got "
           f"{baseline['elevationStats']}")


def test_strict_capture_refuses_varying_tile_count() -> None:
    """tileCount is the first field world_check.py compares exactly."""
    print("test_strict_capture_refuses_varying_tile_count")
    base = strict_dump([10] * 6)
    varied = strict_dump([10] * 6, excluded=1)

    # The extra tile really is invisible to every elevation statistic, so
    # the refusal below can only be attributable to tileCount.
    a = audit_dump(base, seed=STRICT_SEED)
    b = audit_dump(varied, seed=STRICT_SEED)
    expect(a.elevation_stats == b.elevation_stats,
           f"tileCount fixture must isolate tileCount: {a.elevation_stats} "
           f"vs {b.elevation_stats}")

    message = expect_strict_capture_failure(
        "tileCount", [base, varied, base], "tileCount", [6, 7])
    expect("elevationStats" not in message,
           f"only the varying invariant should be reported, got {message!r}")


def test_strict_capture_refuses_each_varying_elevation_statistic() -> None:
    """min, max, median and count each refuse on their own.

    Capture warned about min and max only before #1598; median and count
    reached the baseline as run 0's value with no warning at all. Each key
    is varied in isolation so a check covering three of the four still
    fails here.
    """
    print("test_strict_capture_refuses_each_varying_elevation_statistic")
    cases = [
        ("min", strict_dump([10] * 6), strict_dump([5] + [10] * 5), [5, 10]),
        ("max", strict_dump([10] * 6), strict_dump([15] + [10] * 5), [10, 15]),
        ("median", strict_dump([10, 10, 10, 20, 20, 20]),
         strict_dump([10, 10, 10, 10, 20, 20]), [10, 15]),
        ("count", strict_dump([10] * 6, excluded=1), strict_dump([10] * 7),
         [6, 7]),
    ]
    for key, base, varied, values in cases:
        # Each fixture pair differs in exactly this one statistic.
        a = audit_dump(base, seed=STRICT_SEED).elevation_stats
        b = audit_dump(varied, seed=STRICT_SEED).elevation_stats
        differing = sorted(k for k in ("min", "max", "median", "count")
                           if a.get(k) != b.get(k))
        expect(differing == [key],
               f"elevationStats.{key} fixture must isolate {key}, "
               f"differs in {differing} ({a} vs {b})")
        expect(audit_dump(base, seed=STRICT_SEED).tile_count
               == audit_dump(varied, seed=STRICT_SEED).tile_count,
               f"elevationStats.{key} fixture must hold tileCount still")

        expect_strict_capture_failure(
            key, [base, varied, base], f"elevationStats.{key}", values)


def test_strict_capture_reports_every_violated_invariant() -> None:
    """A capture varying in several invariants names all of them."""
    print("test_strict_capture_reports_every_violated_invariant")
    message = expect_strict_capture_failure(
        "all five", [strict_dump([10] * 6), strict_dump([5] * 7)],
        "tileCount", [6, 7])
    for key in ("min", "max", "median", "count"):
        expect(f"elevationStats.{key}" in message,
               f"every violated invariant must be named, {key} missing from "
               f"{message!r}")


def test_strict_capture_handles_an_absent_elevation_domain() -> None:
    """None and int observations report a failure, not a TypeError.

    world_audit.compute_stats returns None for min/max/median when a
    region holds no real terrain, and sorted() over a set mixing None with
    ints raises TypeError — which would replace the required failure with
    a crash on exactly the capture that most needs reporting.
    """
    print("test_strict_capture_handles_an_absent_elevation_domain")
    empty = strict_dump([], excluded=6)
    expect(audit_dump(empty, seed=STRICT_SEED).elevation_stats
           == {"min": None, "max": None, "median": None, "count": 0},
           "the empty-domain fixture must really produce None statistics")

    message = expect_strict_capture_failure(
        "absent domain", [strict_dump([10] * 6), empty],
        "elevationStats.min", [None, 10])
    expect("elevationStats.count" in message,
           f"count varies here too and must be named, got {message!r}")


def test_strict_capture_still_allows_a_hash_racy_seed() -> None:
    """Recorded-hash raciness is untouched when the strict fields hold.

    #1361's policy records the distinct hashes and lets world_check.py
    downgrade its content-identity gate; that is a different question from
    sampling a field the checker compares exactly.
    """
    print("test_strict_capture_still_allows_a_hash_racy_seed")
    clean = hash_dump_fixture()
    variant = hash_dump_fixture(matId_at_index=(7, 70))
    baseline = capture_strict([clean, variant, clean])
    expect(baseline["determinism"]["distinctHashes"] == 2,
           f"a hash-racy seed is still captured, got "
           f"{baseline['determinism']['distinctHashes']} distinct hashes")


def run_world_baseline_main(seeds: list[dict[str, Any]], argv: list[str],
                            dumps_by_seed: dict[int, list[list[dict[str, Any]]]],
                            baseline_dir: Path) -> tuple[int, str]:
    """Run world_baseline.main() against a temporary seeds file and dir.

    `dumps_by_seed` maps each seed to the dumps its capture runs return,
    so a seed can be made to vary without generating a world. An
    unqueued seed raises AssertionError rather than RuntimeError, which
    main() would otherwise absorb as an ordinary capture failure and hide
    a broken fixture.
    """
    pending = {seed: list(dumps) for seed, dumps in dumps_by_seed.items()}
    original_run = world_baseline.run_dump
    original_dir = world_baseline.BASELINE_DIR
    original_argv = sys.argv

    def fake_run_dump(seed: int, *a: Any, **k: Any) -> list[dict[str, Any]]:
        queue = pending.get(seed)
        if not queue:
            raise AssertionError(f"test: no dump queued for seed {seed}")
        return queue.pop(0)

    with tempfile.TemporaryDirectory() as tmp:
        seeds_file = Path(tmp) / "_seeds.json"
        seeds_file.write_text(json.dumps({"seeds": seeds}))
        world_baseline.run_dump = fake_run_dump
        world_baseline.BASELINE_DIR = baseline_dir
        sys.argv = (["world_baseline.py", "--seeds-file", str(seeds_file)]
                    + argv)
        captured = io.StringIO()
        try:
            with contextlib.redirect_stdout(captured), \
                    contextlib.redirect_stderr(captured):
                code = world_baseline.main()
        finally:
            world_baseline.run_dump = original_run
            world_baseline.BASELINE_DIR = original_dir
            sys.argv = original_argv
    return code, captured.getvalue()


def strict_baseline_file(baseline_dir: Path, entry: dict[str, Any]) -> Path:
    original_dir = world_baseline.BASELINE_DIR
    world_baseline.BASELINE_DIR = baseline_dir
    try:
        return world_baseline.baseline_path(
            entry["seed"], entry["world_size"], tuple(entry["region"]))
    finally:
        world_baseline.BASELINE_DIR = original_dir


def test_strict_capture_single_seed_writes_nothing_and_exits_nonzero() -> None:
    """--seed N on a varying seed leaves no file behind and fails."""
    print("test_strict_capture_single_seed_writes_nothing_and_exits_nonzero")
    with tempfile.TemporaryDirectory() as tmp:
        baseline_dir = Path(tmp)
        target = strict_baseline_file(baseline_dir, STRICT_ENTRY)
        code, output = run_world_baseline_main(
            [STRICT_ENTRY], ["--seed", str(STRICT_SEED), "--runs", "2"],
            {STRICT_SEED: [strict_dump([10] * 6), strict_dump([10] * 7)]},
            baseline_dir)
        expect_exit(code, 1, output, "a varying single seed fails")
        expect(not target.exists(),
               f"no baseline may be created for a refused seed, found {target}")
        expect_output_contains("1 failures", output, "the failure count")
        expect_output_contains(f"seed {STRICT_SEED}", output, "the seed")


def test_strict_capture_leaves_an_existing_baseline_byte_identical() -> None:
    """A refused seed's tracked baseline is not touched."""
    print("test_strict_capture_leaves_an_existing_baseline_byte_identical")
    with tempfile.TemporaryDirectory() as tmp:
        baseline_dir = Path(tmp)
        target = strict_baseline_file(baseline_dir, STRICT_ENTRY)
        sentinel = '{"tileCount": "do not touch"}\n'
        target.write_text(sentinel)

        code, output = run_world_baseline_main(
            [STRICT_ENTRY], ["--runs", "2"],
            {STRICT_SEED: [strict_dump([10] * 6), strict_dump([5] + [10] * 5)]},
            baseline_dir)
        expect_exit(code, 1, output, "a varying seed fails")
        expect(target.read_text() == sentinel,
               f"the existing baseline must be byte-identical, got "
               f"{target.read_text()!r}")


def test_strict_capture_failure_does_not_abort_the_other_seeds() -> None:
    """One refused seed still leaves the rest captured, and the run fails."""
    print("test_strict_capture_failure_does_not_abort_the_other_seeds")
    with tempfile.TemporaryDirectory() as tmp:
        baseline_dir = Path(tmp)
        refused = strict_baseline_file(baseline_dir, STRICT_ENTRY)
        written = strict_baseline_file(baseline_dir, STRICT_ENTRY_B)
        stable = strict_dump([10] * 6)

        code, output = run_world_baseline_main(
            [STRICT_ENTRY, STRICT_ENTRY_B], ["--runs", "2"],
            {STRICT_SEED: [stable, strict_dump([15] + [10] * 5)],
             STRICT_ENTRY_B["seed"]: [stable, stable]},
            baseline_dir)
        expect_exit(code, 1, output, "one refused seed fails the run")
        expect(not refused.exists(),
               f"the refused seed writes nothing, found {refused}")
        expect(written.exists(),
               f"a later seed is still captured, {written} missing")
        expect_output_contains("Captured 1 baselines, 1 failures", output,
                               "the per-run tally")


# ----- Missing-baseline exit policy (#1319) --------------------------------
#
# These drive the real world_check.main() over a temporary seeds file, so
# the assertion is about the process's own exit status and printed output
# rather than a re-implementation of the rule. No engine is booted: a
# selected seed with no baseline returns before run_dump, and run_dump is
# replaced by a recorder that fails loudly if it is reached at all.

MISSING_A = {"seed": 987654321, "world_size": 32, "region": [-4, -4, 4, 4],
             "description": "no baseline exists", "quick": True}
MISSING_B = {"seed": 987654322, "world_size": 64, "region": [-2, -2, 2, 2],
             "description": "also no baseline", "quick": True}
HASH_SEED_ENTRY = dict(HASH_ENTRY, description="drifts against its baseline",
                       quick=True)


def run_world_check_main(seeds: list[dict[str, Any]], argv: list[str],
                         baselines: dict[int, dict[str, Any]] | None = None,
                         dumps: list[list[dict[str, Any]]] | None = None
                         ) -> tuple[int, str, list[Any]]:
    """Run world_check.main() against a temporary seed selection.

    `baselines` maps a seed to a baseline document written to a temp file
    and served for that seed only; every other seed resolves through the
    real baseline_path, so a fake seed genuinely has no baseline on disk
    and its expected filename is the one the production naming rule
    generates. Returns (exit code, combined stdout+stderr, dump calls).
    """
    calls: list[Any] = []
    pending = list(dumps or [])
    original_run = world_check.run_dump
    original_path = world_check.baseline_path

    def fake_run_dump(*a: Any, **k: Any) -> list[dict[str, Any]]:
        calls.append(a)
        if not pending:
            raise RuntimeError("test: no world generation was expected here")
        return pending.pop(0)

    with tempfile.TemporaryDirectory() as tmp:
        seeds_file = Path(tmp) / "_seeds.json"
        seeds_file.write_text(json.dumps({"seeds": seeds}))
        overrides: dict[int, Path] = {}
        for seed, document in (baselines or {}).items():
            path = Path(tmp) / f"baseline_{seed}.json"
            path.write_text(json.dumps(document, indent=2) + "\n")
            overrides[seed] = path

        def fake_baseline_path(seed: int, world_size: int,
                               region: tuple[int, int, int, int]) -> Path:
            if seed in overrides:
                return overrides[seed]
            return original_path(seed, world_size, region)

        original_argv = sys.argv
        world_check.run_dump = fake_run_dump
        world_check.baseline_path = fake_baseline_path
        sys.argv = ["world_check.py", "--seeds-file", str(seeds_file)] + argv
        captured = io.StringIO()
        try:
            with contextlib.redirect_stdout(captured), \
                    contextlib.redirect_stderr(captured):
                code = world_check.main()
        finally:
            world_check.run_dump = original_run
            world_check.baseline_path = original_path
            sys.argv = original_argv
    return code, captured.getvalue(), calls


def expected_baseline_name(entry: dict[str, Any]) -> str:
    return world_baseline.baseline_path(
        entry["seed"], entry["world_size"], tuple(entry["region"])).name


# expect() prints its message on success too, so a captured run is attached
# to the message only when the assertion actually fails — otherwise these
# cases bury the rest of the suite's output.

def expect_exit(code: int, expected: int, output: str, label: str) -> None:
    if code == expected:
        expect(True, f"{label}: exit {expected}")
    else:
        expect(False, f"{label}: expected exit {expected}, got {code}. "
                      f"Run output: {output!r}")


def expect_output_contains(needle: str, output: str, label: str) -> None:
    if needle in output:
        expect(True, f"{label}: output names {needle}")
    else:
        expect(False, f"{label}: output must name {needle}. "
                      f"Run output: {output!r}")


def expect_missing_baseline_failure(entry: dict[str, Any], argv: list[str],
                                    label: str) -> None:
    code, output, calls = run_world_check_main([entry], argv)
    expect_exit(code, 1, output,
                f"{label}: a selected seed with no baseline fails")
    expect_output_contains(str(entry["seed"]), output, f"{label}: seed")
    expect_output_contains(expected_baseline_name(entry), output,
                           f"{label}: expected baseline file")
    expect(not calls,
           f"{label}: a seed with no baseline must not be generated, "
           f"run_dump was called {len(calls)} time(s)")


def test_missing_baseline_fails_every_selection_path() -> None:
    """Unfiltered, --quick and --seed N all fail on a missing baseline.

    The three paths are separate filters over the same seed list, so a
    fix applied to one of them would leave the other two green.
    """
    print("test_missing_baseline_fails_every_selection_path")
    expect_missing_baseline_failure(MISSING_A, [], "unfiltered")
    expect_missing_baseline_failure(MISSING_A, ["--quick"], "--quick")
    expect_missing_baseline_failure(
        MISSING_A, ["--seed", str(MISSING_A["seed"])], "--seed N")


def test_missing_baseline_reports_every_entry() -> None:
    """Two missing baselines are both named, not just the first."""
    print("test_missing_baseline_reports_every_entry")
    code, output, calls = run_world_check_main([MISSING_A, MISSING_B], [])
    expect_exit(code, 1, output, "two missing baselines fail")
    for entry in (MISSING_A, MISSING_B):
        expect_output_contains(expected_baseline_name(entry), output,
                               f"seed {entry['seed']} expected baseline")
    expect(not calls,
           f"no seed should have been generated, run_dump was called "
           f"{len(calls)} time(s)")


def test_allow_missing_baselines_tolerates_a_clean_skip_run() -> None:
    """The opt-in flag reports the skipped seeds and exits zero."""
    print("test_allow_missing_baselines_tolerates_a_clean_skip_run")
    code, output, calls = run_world_check_main(
        [MISSING_A, MISSING_B], ["--allow-missing-baselines"])
    expect_exit(code, 0, output,
                "--allow-missing-baselines tolerates an all-SKIP run")
    expect_output_contains("SKIP=2", output, "summary still counts the skips")
    for entry in (MISSING_A, MISSING_B):
        expect_output_contains(expected_baseline_name(entry), output,
                               f"tolerated seed {entry['seed']}")
    expect(not calls,
           f"the tolerant path still generates nothing, run_dump was called "
           f"{len(calls)} time(s)")


def test_allow_missing_baselines_does_not_mask_a_real_failure() -> None:
    """The flag narrows the missing-baseline cause and nothing else.

    A run holding both a missing baseline and an ordinary regression
    still exits 1 under the flag — otherwise the local-exploration
    escape hatch would be a way to pass a genuinely failing gate.
    """
    print("test_allow_missing_baselines_does_not_mask_a_real_failure")
    clean = hash_dump_fixture()
    baseline = capture_hash_baseline([clean, clean, clean])
    drifted = hash_dump_fixture((0, 70))

    code, output, _ = run_world_check_main(
        [HASH_SEED_ENTRY, MISSING_A], ["--allow-missing-baselines"],
        baselines={HASH_SEED_ENTRY["seed"]: baseline}, dumps=[drifted])
    expect_exit(code, 1, output,
                "--allow-missing-baselines does not mask an ordinary FAIL")
    expect_output_contains("FAIL=1 SKIP=1", output, "both dispositions")

    # Same seed, undrifted: the flag really does pass the rest of the run,
    # so the exit 1 above is the FAIL and not the flag failing to apply.
    code, output, _ = run_world_check_main(
        [HASH_SEED_ENTRY, MISSING_A], ["--allow-missing-baselines"],
        baselines={HASH_SEED_ENTRY["seed"]: baseline},
        dumps=[hash_dump_fixture()])
    expect_exit(code, 0, output,
                "a passing seed beside a tolerated missing baseline")


def test_clean_run_still_exits_zero() -> None:
    """A fully baselined, fully passing selection is unaffected."""
    print("test_clean_run_still_exits_zero")
    clean = hash_dump_fixture()
    baseline = capture_hash_baseline([clean, clean, clean])
    code, output, calls = run_world_check_main(
        [HASH_SEED_ENTRY], [], baselines={HASH_SEED_ENTRY["seed"]: baseline},
        dumps=[hash_dump_fixture()])
    expect_exit(code, 0, output, "a fully baselined passing run")
    expect_output_contains("PASS=1", output, "the seed passes")
    expect(len(calls) == 1,
           f"a baselined seed is generated exactly once at --runs 1, "
           f"run_dump was called {len(calls)} time(s)")


def test_bad_selections_keep_their_exit_two() -> None:
    """Empty selections are still invocation errors, not missing baselines.

    Both messages are pinned verbatim: the new exit policy sits after
    these returns, and folding an empty selection into it would turn a
    typo into a regression report.
    """
    print("test_bad_selections_keep_their_exit_two")
    code, output, _ = run_world_check_main(
        [dict(MISSING_A, quick=False)], ["--quick"])
    expect_exit(code, 2, output, "--quick with no quick-tagged seeds")
    expect_output_contains('error: no seeds tagged "quick": true in seeds file',
                           output, "unchanged empty-quick message")

    code, output, _ = run_world_check_main([MISSING_A], ["--seed", "1234567"])
    expect_exit(code, 2, output, "--seed for an absent seed")
    expect_output_contains("error: seed 1234567 not in seeds file", output,
                           "unchanged absent-seed message")


def test_missing_baseline_keeps_its_skip_disposition() -> None:
    """check_seed still reports SKIP and records the path it wanted.

    The exit policy names the recorded path rather than the SKIP string,
    so both halves have to hold for the run to fail for the right reason.
    """
    print("test_missing_baseline_keeps_its_skip_disposition")
    r = check_seed(MISSING_A, runs=1)
    expect(r.status == SKIP,
           f"a missing baseline stays externally visible as SKIP, got {r.status}")
    expect(r.missing_baseline is not None
           and r.missing_baseline.name == expected_baseline_name(MISSING_A),
           f"the expected baseline path must be recorded, got "
           f"{r.missing_baseline}")
    expect(world_check.exit_status([r], allow_missing_baselines=False) == 1,
           "a recorded missing baseline must fail by default")
    expect(world_check.exit_status([r], allow_missing_baselines=True) == 0,
           "a recorded missing baseline must be tolerated under the flag")


# ----- Runner --------------------------------------------------------------

def main() -> int:
    selftest.parse_verbose()
    tests = [
        test_clean_grid,
        test_stats,
        test_issue_ordering_is_canonical,
        test_dry_below_sea,
        test_ocean_on_land,
        test_river_under_terrain,
        test_lake_under_terrain,
        test_floating_fluid,
        test_lava_rim_containment,
        test_lava_rim_on_real_generated_output,
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
        test_check_determinism_inactive_at_one_run,
        test_baseline_hash_match_passes,
        test_baseline_hash_mismatch_fails_on_aggregate_preserving_drift,
        test_baseline_hash_racy_baseline_is_announced_not_gated,
        test_baseline_hash_inconsistent_baseline_fails,
        test_strict_capture_accepts_a_stable_seed,
        test_strict_capture_refuses_varying_tile_count,
        test_strict_capture_refuses_each_varying_elevation_statistic,
        test_strict_capture_reports_every_violated_invariant,
        test_strict_capture_handles_an_absent_elevation_domain,
        test_strict_capture_still_allows_a_hash_racy_seed,
        test_strict_capture_single_seed_writes_nothing_and_exits_nonzero,
        test_strict_capture_leaves_an_existing_baseline_byte_identical,
        test_strict_capture_failure_does_not_abort_the_other_seeds,
        test_missing_baseline_keeps_its_skip_disposition,
        test_missing_baseline_fails_every_selection_path,
        test_missing_baseline_reports_every_entry,
        test_allow_missing_baselines_tolerates_a_clean_skip_run,
        test_allow_missing_baselines_does_not_mask_a_real_failure,
        test_clean_run_still_exits_zero,
        test_bad_selections_keep_their_exit_two,
    ]

    for t in tests:
        t()
        print()

    if FAILURES:
        print(f"\n{len(FAILURES)} test failure(s):")
        for f in FAILURES:
            print(f"  {f}")
        return selftest.concluded(1)

    return selftest.concluded(0, f"\nAll {len(tests)} test groups passed")


if __name__ == "__main__":
    raise SystemExit(main())
