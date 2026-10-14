#!/usr/bin/env python3
"""Physiology access and coverage audit (#2830, S5 of epic #2827).

The physiology stat-modes design (docs/designs/physiology_stat_modes_design.md,
D-7) makes the registry-owned keys -- every owned, derived and failure-meter
key in data/physiology/systems.yaml -- readable only through one accessor and
writable across systems only through one contribution seam, so that a system
running Nominal can substitute its healthy value without every caller
re-deriving "absent means Nominal". This audit enforces that rule and the
registry's own completeness.

What it checks (exit 1 on any finding, with path:line diagnostics):

  1. RAW ACCESS. ``unit.getStat``, ``unit.getStatBase``, ``unit.setStat`` and
     ``scripts.unit_stats`` ``.get`` of a registry key, including a
     caller-local ``or <default>`` fallback, outside the key's named system
     owner (``OWNERS``). Callers use ``scripts/physiology.lua`` (``get``,
     ``getBase``, ``fraction``) for reads and ``debit`` / ``credit`` for
     contributions between systems.
  2. MUTATING MODE VERBS. ``unit.setPhysioMode`` / ``unit.clearPhysioMode``
     anywhere but the facade. The five READ verbs (``getPhysioMode``,
     ``getPhysioOverride``, ``getPhysioDefault``, ``readPhysio``,
     ``readPhysioBase``) are explicitly allowed everywhere (D-7, SD-19).
  3. REGISTRY COVERAGE. Every system has a Lua owner file that exists and
     names one of its keys; every owned key has a healthy rule; every derived
     key a rule and a healthy value; every ``contributes`` entry (and every
     ``provides`` / ``effects`` entry) a Nominal substitute.
  4. TEMPLATING. No unit definition templates a registry key in ``stats:`` or
     ``skills:`` (the loader refuses it at run time; this is the static
     twin).

Scope: every ``scripts/**/*.lua`` file and the Lua embedded in
``tools/physiology_probe.py`` (extracted from its Python string literals).

Allowances, each tied to real ownership and named, never a directory:

  * ``OWNERS``: (file, system[, scope]) -- a file may touch the keys of the
    systems it actually owns. ``scope="meters"`` restricts the failure-meter
    engine to the meters of that system.
  * ``SURVIVAL_EXEMPTION``: ``neutralize`` in scripts/unit_survival_exempt.lua
    may ``setStat`` exactly six keys (the #2754 thermo/salt substitute tick).
    Removed by S4 when #2754 becomes the ``exempt`` profile (design F-7).
  * ``PROBE_FIXTURE_WRITES``: tools/physiology_probe.py may ``setStat`` the
    keys it forces as scenario fixtures (D-7 allows raw setStat debugging
    under Simulated) -- only from a STRAIGHT-LINE embedded chunk (no
    function, loop or branch, so never an aggregate or a helper) inside the
    named scenario scopes (``hunger_section``: calories, hunger, core_temp;
    ``starvation_bands_section``: hunger; its nested ``set_frac``: calories;
    a helper nested in a scenario, such as a measurement function, is a
    different scope). Reads in the probe are NOT allowed.
  * ``UI_EXCEPTIONS`` / ``UI_HELPER_EXCEPTIONS``: the TEMPORARY, enumerated
    legacy pool reads that stay raw until S6 (Nominal-aware display): the
    hydration, hunger and calories reads in ``formatStatus`` of
    scripts/unit_info_panel.lua, and the generic ``stats.get`` pool reads of
    ``fmtCurMax`` in scripts/unit_info_v2_stat_defs.lua. They are identified
    by file, enclosing function, verb and key (never by line number), each
    key admitted ONCE; any other raw UI read, in particular the
    ``salt_conc`` reads of unit_info_v2_status.lua, is flagged. S6 deletes
    this exception; no permanent UI exemption is authorized.

Supported forms and limits (the scanner is in tools/physiology_access_lua.py):

  * It understands a string-literal key; a loop variable over a literal list
    (``for _, k in ipairs({"a", "b"})`` or over a file-local list); ``v.field``
    for a loop variable over a local table of records
    (``for _, mc in ipairs(METER_CONDITIONS) ... mc.stat``); a file-local
    string constant; and a function parameter -- of the function itself or an
    enclosing closure -- which makes that function a GENERIC HELPER whose
    call sites with a literal registry key are reported at the call site.
  * Helper resolution is by bare function name, one level deep: a helper
    called through another helper's parameter, or through an alias, is not
    followed. Same-named functions in different files are treated as one.
  * A key it cannot resolve is an "unresolvable dynamic key": allowed inside
    an owner or a generic accessor (the facade, ``scripts/unit_stats.lua``),
    a finding anywhere else.
  * It does not follow data flow: ``unit.getAllStats`` / ``stats.getAll``
    snapshots, engine verbs that move registry state themselves
    (``unit.feed``, ``unit.getCalories``; S4's), Haskell code (covered by
    Haskell's own ``physioRead`` / ``physioConsumerView`` and the hspec
    group), and other probes and headless fixtures with raw access (for
    example tools/meal_waste_probe.py, canteen_instance_probe.py,
    state_of_mind_probe.py, foraging_probe.py): debugging code outside the
    gameplay-consumer rule.
  * Strings and comments are never mistaken for code; a registry name inside
    one is not an access.

Usage:
  python3 tools/physiology_access_audit.py
  python3 tools/physiology_access_audit.py --self-test
"""
from __future__ import annotations

import argparse
import ast
import sys
from collections import defaultdict
from dataclasses import dataclass, field
from pathlib import Path
from typing import Optional

sys.path.insert(0, str(Path(__file__).resolve().parent))

import physiology_access_lua as lua  # noqa: E402

REPO_ROOT = Path(__file__).resolve().parent.parent

REGISTRY_PATH = "data/physiology/systems.yaml"
UNIT_GLOB = "data/units/*.yaml"
SCRIPTS_GLOB = "scripts/**/*.lua"
PROBE_PATH = "tools/physiology_probe.py"
FACADE = "scripts/physiology.lua"

# Files whose dynamic raw accessors ARE the generic API: the facade, and the
# stats wrapper whose ``get`` the facade (and unowned callers) sit on.
GENERIC_ACCESSORS = frozenset({FACADE, "scripts/unit_stats.lua"})


@dataclass(frozen=True)
class Owner:
    system: str
    file: str
    scope: str = "all"          # "all" | "meters"


# system owner -> files that actually own its keys. Registry order is tick
# order (thermo, salt, hydration, nutrition).
OWNERS: tuple[Owner, ...] = (
    Owner("thermo", "scripts/thermo.lua"),
    Owner("salt", "scripts/salts.lua"),
    # The pool engine: per-resource tick, carry/integrate and revive gate.
    Owner("hydration", "scripts/unit_resource_tick.lua"),
    Owner("hydration", "scripts/unit_resource_carry.lua"),
    Owner("nutrition", "scripts/unit_resource_tick.lua"),
    Owner("nutrition", "scripts/unit_resource_carry.lua"),
    Owner("nutrition", "scripts/unit_resource_energy.lua"),
    Owner("nutrition", "scripts/unit_resource_regrowth.lua"),
    # The failure-meter engine: only the meters of the systems that declare
    # them, never their pools.
    Owner("thermo", "scripts/unit_resource_failure.lua", "meters"),
    Owner("salt", "scripts/unit_resource_failure.lua", "meters"),
)


@dataclass(frozen=True)
class SiteException:
    file: str
    function: str
    verb: str
    keys: frozenset[str]
    why: str


@dataclass(frozen=True)
class HelperException:
    """A generic helper whose literal-key call sites stay allowed. It
    applies only while EVERY definition of the helper is exactly the
    authorized one: `sites` raw accesses, all in `file`, all `verb` reads
    -- so a mutation or another verb added to the helper, or a further
    read, voids the exception -- and only for the pool `keys`."""
    file: str
    helper: str
    verb: str
    sites: int
    keys: frozenset[str]
    why: str


SURVIVAL_EXEMPTION = SiteException(
    "scripts/unit_survival_exempt.lua", "neutralize", "unit.setStat",
    frozenset({"core_temp", "salt", "salt_conc",
               "hypothermia", "hyperthermia", "salt_imbalance"}),
    "the #2754 thermo/salt substitute tick; S4 removes it (design F-7)")

@dataclass(frozen=True)
class FixtureWrites:
    """The probe's scenario setup writes: a STRAIGHT-LINE embedded chunk
    (no function, loop or branch -- so never an aggregate or a Lua helper)
    in exactly one of the named Python setup scopes, writing one of THAT
    scope's keys. A scope is the dotted chain of enclosing Python
    functions, so a measurement helper nested in a scenario function
    (``hunger_section.avg_hyd``) is a different scope from the scenario."""
    file: str
    verb: str
    scopes: dict[str, frozenset[str]]
    why: str


PROBE_FIXTURE_WRITES = FixtureWrites(
    PROBE_PATH, "unit.setStat",
    {"hunger_section": frozenset({"calories", "hunger", "core_temp"}),
     "starvation_bands_section": frozenset({"hunger"}),
     "starvation_bands_section.set_frac": frozenset({"calories"})},
    "scenario fixtures (D-7: raw setStat is allowed for debugging)")

# TEMPORARY (S5 -> S6). Each key is admitted once per function.
UI_EXCEPTIONS: tuple[SiteException, ...] = (
    SiteException("scripts/unit_info_panel.lua", "formatStatus", "unit.getStat",
                  frozenset({"hydration", "hunger", "calories"}),
                  "legacy pool display; S6 migrates it"),
)
UI_HELPER_EXCEPTIONS: tuple[HelperException, ...] = (
    HelperException("scripts/unit_info_v2_stat_defs.lua", "fmtCurMax",
                    "stats.get", 2, frozenset({"hydration", "hunger", "calories"}),
                    "generic cur/max pool display; S6 migrates it"),
)


@dataclass(frozen=True)
class Policy:
    owners: tuple[Owner, ...] = OWNERS
    facade: str = FACADE
    generic_accessors: frozenset[str] = GENERIC_ACCESSORS
    survival: SiteException = SURVIVAL_EXEMPTION
    probe_fixture: FixtureWrites = PROBE_FIXTURE_WRITES
    ui_exceptions: tuple[SiteException, ...] = UI_EXCEPTIONS
    ui_helpers: tuple[HelperException, ...] = UI_HELPER_EXCEPTIONS


# ---------------------------------------------------------------- registry

@dataclass
class Registry:
    systems: dict[str, dict]
    key_system: dict[str, str] = field(default_factory=dict)
    key_role: dict[str, str] = field(default_factory=dict)

    @property
    def keys(self) -> frozenset[str]:
        return frozenset(self.key_system)


def load_registry(doc: dict) -> Registry:
    systems = {}
    reg = Registry(systems)
    for s in doc.get("systems") or []:
        sid = s.get("id")
        systems[sid] = s
        for o in s.get("owns") or []:
            reg.key_system[o["key"]] = sid
            reg.key_role[o["key"]] = "owned"
        for d in s.get("derived") or []:
            reg.key_system[d["key"]] = sid
            reg.key_role[d["key"]] = "derived"
        for m in s.get("meters") or []:
            reg.key_system[m] = sid
            reg.key_role[m] = "meter"
    return reg


# ---------------------------------------------------------------- coverage

def check_coverage(doc: dict, files: dict[str, str], policy: Policy) -> list[str]:
    out: list[str] = []
    reg = load_registry(doc)
    for sid, s in reg.systems.items():
        for o in s.get("owns") or []:
            h = o.get("healthy")
            if not isinstance(h, dict) or not (("const" in h) ^ ("from" in h)):
                out.append(f"{REGISTRY_PATH}: system '{sid}' owned key "
                           f"'{o.get('key')}' has no healthy rule "
                           "(exactly one of const: / from:)")
        for d in s.get("derived") or []:
            if not d.get("rule") or not isinstance(d.get("healthy"), (int, float)):
                out.append(f"{REGISTRY_PATH}: system '{sid}' derived key "
                           f"'{d.get('key')}' needs a rule and a numeric healthy value")
        for section in ("contributes", "provides", "effects"):
            for e in s.get(section) or []:
                if e.get("substitute") is None:
                    out.append(f"{REGISTRY_PATH}: system '{sid}' {section} "
                               f"entry '{e.get('name')}' has no Nominal substitute")
        owners = [ow for ow in policy.owners if ow.system == sid]
        if not owners:
            out.append(f"{REGISTRY_PATH}: system '{sid}' has no Lua owner in "
                       "tools/physiology_access_audit.py OWNERS")
            continue
        keys = {o["key"] for o in s.get("owns") or []} \
            | {d["key"] for d in s.get("derived") or []} \
            | set(s.get("meters") or [])
        names_a_key = False
        for ow in owners:
            text = files.get(ow.file)
            if text is None:
                out.append(f"{ow.file}: named as the Lua owner of system "
                           f"'{sid}' but does not exist")
                continue
            strs = {t.text for t in lua.tokenize(text) if t.kind == "str"}
            names_a_key |= bool(strs & keys)
        if not names_a_key and all(ow.file in files for ow in owners):
            # Some owners are dynamic-key engine modules that never spell a
            # key; at least one owner must, or the mapping names nothing real.
            out.append(f"{REGISTRY_PATH}: no Lua owner of system '{sid}' names "
                       f"any of its keys ({', '.join(sorted(keys))})")
    for ow in policy.owners:
        if ow.system not in reg.systems:
            out.append(f"tools/physiology_access_audit.py OWNERS: '{ow.file}' "
                       f"owns unknown system '{ow.system}'")
    return out


def check_templating(doc: dict, unit_docs: dict[str, dict]) -> list[str]:
    reg = load_registry(doc)
    out = []
    for path, udoc in sorted(unit_docs.items()):
        for u in (udoc or {}).get("units") or []:
            for section in ("stats", "skills"):
                for k in (u.get(section) or {}):
                    if k in reg.keys:
                        out.append(f"{path}: unit '{u.get('name')}' templates "
                                   f"registry key '{k}' in {section}: -- an "
                                   "owned key is never a stat template (D-3)")
    return out


# ------------------------------------------------------------------ probe

def _string_text(node) -> Optional[str]:
    if isinstance(node, ast.Constant) and isinstance(node.value, str):
        return node.value
    if isinstance(node, ast.JoinedStr):
        return "".join(v.value if isinstance(v, ast.Constant)
                       and isinstance(v.value, str) else "0"
                       for v in node.values)
    return None


def embedded_lua(py_text: str) -> list[tuple[int, str, str]]:
    """(line, Lua text, scope) for every Python string literal that looks
    like Lua. `scope` is the dotted chain of Python functions holding it
    (``outer.inner``), or ``<module>``. An f-string's replacement fields become ``0``; implicitly
    concatenated literals arrive already joined, reported at their first
    line."""
    try:
        tree = ast.parse(py_text)
    except SyntaxError:
        return []
    parts = {id(v) for n in ast.walk(tree) if isinstance(n, ast.JoinedStr)
             for v in n.values}
    out = []

    def visit(node, scope):
        if isinstance(node, ast.FunctionDef):
            scope = node.name if scope == "<module>" else f"{scope}.{node.name}"
        if id(node) not in parts:
            text = _string_text(node)
            if text and ("unit." in text or "stats." in text
                         or "require(" in text):
                out.append((node.lineno, text, scope))
        for child in ast.iter_child_nodes(node):
            visit(child, scope)

    visit(tree, "<module>")
    return sorted(out)


# ------------------------------------------------------------------- scan

@dataclass
class Finding:
    path: str
    line: int
    message: str

    def render(self) -> str:
        return f"{self.path}:{self.line}: {self.message}"


def _owner_allows(policy: Policy, path: str, reg: Registry, key: str) -> bool:
    sid = reg.key_system.get(key)
    for ow in policy.owners:
        if ow.file == path and ow.system == sid:
            if ow.scope == "all" or reg.key_role.get(key) == "meter":
                return True
    return False


def scan_corpus(files: dict[str, str], probe: Optional[tuple[str, str]],
                reg: Registry, policy: Policy) -> list[Finding]:
    """Scan every Lua file and the probe's embedded chunks; apply the
    allowances; return the findings."""
    chunks: list[lua.ChunkScan] = []
    for path in sorted(files):
        chunks.append(lua.scan_chunk(path, files[path], reg.keys))
    if probe is not None:
        ppath, ptext = probe
        for ln, src, scope in embedded_lua(ptext):
            chunks.append(lua.scan_chunk(ppath, src, reg.keys,
                                         line_base=ln - 1, scope=scope))

    findings: list[Finding] = []

    # Mutating mode verbs.
    for ch in chunks:
        if ch.path == policy.facade:
            continue
        for mv in ch.mode_verbs:
            findings.append(Finding(mv.path, mv.line,
                f"mutating physiology verb {mv.verb} outside the facade "
                f"(only {policy.facade} may call it; the read verbs are "
                "allowed everywhere)"))

    # Helpers: functions that pass a parameter to a raw access. The facade
    # and the stats wrapper ARE the generic accessors, so they define none.
    helpers: dict[str, list[lua.HelperDef]] = defaultdict(list)
    for ch in chunks:
        if ch.path in policy.generic_accessors:
            continue
        for h in ch.helpers:
            helpers[h.name].append(h)

    accesses: list[lua.Access] = []
    for ch in chunks:
        accesses.extend(ch.accesses)
        accesses.extend(lua.helper_call_sites(ch, helpers))

    # Merge read+write of one site into a single diagnostic.
    sites: dict[tuple, dict] = {}
    for a in accesses:
        if a.key not in reg.keys:
            continue
        sk = (a.path, a.line, a.key, a.verb, a.function)
        s = sites.setdefault(sk, {"acc": a, "kinds": set(), "fallback": False})
        s["kinds"].add(a.access)
        s["fallback"] |= a.fallback

    ui_used: dict[tuple[str, str, str, str], int] = defaultdict(int)
    for sk in sorted(sites, key=lambda k: (k[0], k[1], k[2])):
        a, kinds, fb = sites[sk]["acc"], sites[sk]["kinds"], sites[sk]["fallback"]
        if a.path == policy.facade or _owner_allows(policy, a.path, reg, a.key):
            continue
        ex = policy.survival
        if a.path == ex.file and a.function == ex.function \
                and a.verb == ex.verb and a.key in ex.keys:
            continue
        fx = policy.probe_fixture
        if a.path == fx.file and a.verb == fx.verb and a.straight \
                and a.key in fx.scopes.get(a.scope, frozenset()):
            continue
        if a.via == "helper-call":
            name = a.verb.split(":", 1)[1]
            defs = helpers.get(name, [])
            if any(he.helper == name and a.key in he.keys
                   and len(defs) == he.sites
                   and all(d.path == he.file and d.verb == he.verb
                           and d.access == "read" for d in defs)
                   for he in policy.ui_helpers):
                continue
        else:
            hit = False
            for ex in policy.ui_exceptions:
                if a.path == ex.file and a.function == ex.function \
                        and a.verb == ex.verb and a.key in ex.keys:
                    k = (ex.file, ex.function, ex.verb, a.key)
                    ui_used[k] += 1
                    if ui_used[k] == 1:
                        hit = True
                    break
            if hit:
                continue
        system = reg.key_system[a.key]
        what = "write" if kinds == {"write"} else (
            "read/write" if len(kinds) > 1 else "read")
        advice = ("physiology.credit / physiology.debit for a contribution"
                  if "write" in kinds
                  else "physiology.get / getBase / fraction")
        via = (f"through {a.verb.split(':', 1)[1]}()"
               if a.via == "helper-call" else f"via {a.verb}")
        findings.append(Finding(a.path, a.line,
            f"raw {what} of {reg.key_role[a.key]} key '{a.key}' (system "
            f"'{system}') {via} in {a.function}() outside its owner"
            f"{' with a caller-local `or` fallback' if fb else ''}; use {advice}"))

    # Unresolvable dynamic keys outside an owner or generic accessor.
    owner_files = {o.file for o in policy.owners}
    for ch in chunks:
        if ch.path in policy.generic_accessors or ch.path in owner_files:
            continue
        for u in ch.unresolved:
            findings.append(Finding(u.path, u.line,
                f"{u.verb} with an unresolvable dynamic key '{u.text}' in "
                f"{u.function}(): name the key as a literal, a loop over a "
                "literal list or a table field the scanner can see, or move "
                "the access into the key's owner"))
    return findings


# ------------------------------------------------------------------- main

def collect(root: Path) -> tuple[dict[str, str], Optional[tuple[str, str]],
                                  dict, dict[str, dict]]:
    import yaml
    files = {p.relative_to(root).as_posix(): p.read_text(encoding="utf-8")
             for p in sorted(root.glob(SCRIPTS_GLOB))}
    probe_file = root / PROBE_PATH
    probe = (PROBE_PATH, probe_file.read_text(encoding="utf-8")) \
        if probe_file.is_file() else None
    doc = yaml.safe_load((root / REGISTRY_PATH).read_text(encoding="utf-8"))
    unit_docs = {p.relative_to(root).as_posix():
                 yaml.safe_load(p.read_text(encoding="utf-8"))
                 for p in sorted(root.glob(UNIT_GLOB))}
    return files, probe, doc, unit_docs


def run_audit(files: dict[str, str], probe: Optional[tuple[str, str]],
              doc: dict, unit_docs: dict[str, dict],
              policy: Policy = Policy()) -> list[str]:
    reg = load_registry(doc)
    findings = [f.render() for f in scan_corpus(files, probe, reg, policy)]
    findings += check_coverage(doc, files, policy)
    findings += check_templating(doc, unit_docs)
    return sorted(findings)


def main(argv: Optional[list[str]] = None) -> int:
    ap = argparse.ArgumentParser(description=__doc__.split("\n\n")[0])
    ap.add_argument("--self-test", action="store_true",
                    help="run the audit's own tests and exit")
    ap.add_argument("--root", type=Path, default=REPO_ROOT)
    args = ap.parse_args(argv)
    if args.self_test:
        import physiology_access_audit_selftest as st
        return st.run()
    try:
        files, probe, doc, unit_docs = collect(args.root)
    except (OSError, ValueError) as err:
        print(f"physiology access audit: cannot read the corpus: {err}")
        return 1
    if not files:
        print(f"physiology access audit: no files matched {SCRIPTS_GLOB} "
              "(an empty corpus must not pass)")
        return 1
    if probe is None:
        print(f"physiology access audit: {PROBE_PATH} is missing (its embedded "
              "Lua is in scope)")
        return 1
    findings = run_audit(files, probe, doc, unit_docs)
    if findings:
        print("physiology access audit: FAILED")
        for f in findings:
            print(f"  {f}")
        print(f"{len(findings)} finding(s). Reads go through scripts/physiology.lua; "
              "cross-system writes through physiology.credit/debit. See "
              "docs/physiology_consumer_facade.md.")
        return 1
    n_chunks = len(embedded_lua(probe[1]))
    print(f"physiology access audit: OK ({len(files)} Lua files, "
          f"{n_chunks} probe chunk(s), "
          f"{len(load_registry(doc).systems)} systems, "
          f"{len(load_registry(doc).keys)} registry keys, "
          f"{sum(len(e.keys) for e in UI_EXCEPTIONS) + len(UI_HELPER_EXCEPTIONS)} "
          "temporary UI exception site(s))")
    return 0


if __name__ == "__main__":
    sys.exit(main())
