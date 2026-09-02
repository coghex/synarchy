#!/usr/bin/env python3
"""The emitted-category inventory of world_audit.py, and its two groups (#2070).

The authoritative set of categories the audit can emit is the `category`
argument of every `Issue(...)` construction in world_audit.py. It is NOT
ALL_CHECKS, whose keys are check-function labels: TERRAIN_SPIKES_PITS is a
key but not a category, and six real categories (the river/lake-under-terrain
pair and the four floating-fluid variants) are categories but not keys.

The inventory is therefore derived from the audit SOURCE by AST, so a
category added to a check function but classified nowhere fails
test_severity_classification instead of silently reaching world_check.py.
Anything that cannot be resolved statically is reported as a failure — never
skipped, since a skipped call site is exactly the fail-open hole this
derivation exists to close.

Extraction is pure AST over source text: nothing here executes a check
function, builds a grid, or drives a tool's `main()`. `world_audit` is
imported for two things only -- to locate its own source file, and for
the classification sets and functions `test_severity_classification`
holds the derived inventory against. Restating those sets here would
reopen the hole the derivation closes.

Not a gate of its own. Run through the aggregate:

  python3 tools/test_audit.py
"""
from __future__ import annotations

import ast
import sys
from pathlib import Path

sys.path.insert(0, str(Path(__file__).resolve().parent))
import world_audit  # type: ignore  # noqa: E402
from world_audit import (  # type: ignore  # noqa: E402
    severity_of, classify_category,
    BUG_CATEGORIES, QUALITY_CATEGORIES, QUALITY_THRESHOLDS,
)
from test_audit_support import expect  # noqa: E402


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


#: This owner's inventory, in the relative order these groups hold
#: within the aggregate's run sequence. `tools/test_audit.py` composes
#: that sequence from every owner's inventory; nothing here decides
#: when, or whether, it runs.
TESTS = (
    test_severity_classification,
    test_category_extraction_resolves_and_fails_loudly,
)
