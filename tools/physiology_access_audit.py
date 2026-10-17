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
     / ``unit.applyCombatEngagement`` (the SD-20 engagement, #2834)
     anywhere but the facade. The five READ verbs (``getPhysioMode``,
     ``getPhysioOverride``, ``getPhysioDefault``, ``readPhysio``,
     ``readPhysioBase``) and the pure maximum read ``unit.getMaxStamina``
     are explicitly allowed everywhere (D-7, SD-19). The ATOMIC STAMINA
     SEAM ``unit.commitStamina`` -- the one relative write to the pool the
     combat worker also spends -- may be called only by the facade (its
     ``debit`` / ``credit``) and the stamina pool engine
     (``scripts/unit_resource_tick.lua``); a script that wrote stamina any
     other way would reopen the #2470 / S8-F2 refund race.
  3. REGISTRY COVERAGE. Every system has a Lua owner file that exists and
     names one of its keys; every owned key has a healthy rule (exactly one of
     ``const:`` / ``from:`` / ``rule:``); every derived
     key a rule and a healthy value that is a number or ``live`` (#2836: a
     ``live`` key's Nominal answer is its rule, so it has no constant); every
     ``contributes`` entry (and every ``provides`` / ``effects`` entry) a
     Nominal substitute, where ``live`` is only valid on a ``provides`` entry
     naming a ``live`` derived key of its own system.
  4. TEMPLATING. No unit definition templates a registry key in ``stats:`` or
     ``skills:`` (the loader refuses it at run time; this is the static
     twin).

Scope: every ``scripts/**/*.lua`` file and the Lua embedded in
``tools/physiology_probe.py`` (extracted from its Python string literals).

Allowances, each tied to real ownership and named, never a directory:

  * ``OWNERS``: (file, system[, scope]) -- a file may touch the keys of the
    systems it actually owns. ``scope="meters"`` restricts the failure-meter
    engine to the meters of that system.
  * (There is no survival exemption any more: #2754's ``neutralize`` was
    removed by S4 (#2831) when it became the ``exempt`` profile, design F-7.)
  * ``PROBE_FIXTURE_WRITES``: tools/physiology_probe.py may ``setStat`` the
    keys it forces as scenario fixtures (D-7 allows raw setStat debugging
    under Simulated) -- only from the setup chunks pinned SITE BY SITE
    by (dotted Python scope, whole normalized Lua text): editing a pinned
    chunk, or putting a write into any other chunk (an observation, an
    aggregate, a helper, a nested measurement function), is flagged, and a
    pin the probe no longer contains is itself a finding. Reads in the probe
    are NOT allowed.
  * There is no UI exception (S6, #2832 deleted the temporary S5 allowance
    for the legacy hydration/hunger/calories reads of scripts/unit_info_panel.lua
    and the generic ``fmtCurMax`` pool reads of
    scripts/unit_info_v2_stat_defs.lua once both read through the facade):
    a raw UI read of a registry key, including the ``salt_conc`` status row
    and any pool read through a generic helper, is flagged like any other.
    No permanent UI exemption is authorized.

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
# order (thermo, salt, hydration, nutrition, stamina, exhaustion, sleep) except
# cardio (#2836), which is listed last but ticked FIRST by the orchestration.
OWNERS: tuple[Owner, ...] = (
    Owner("thermo", "scripts/thermo.lua"),
    # Cardio (#2836): the integrator of heart_rate and blood_oxygen. Circulation
    # is derived by the engine's pure rule, so scripts/circulation.lua is a
    # thin reader and an owner of nothing.
    Owner("cardio", "scripts/cardio.lua"),
    Owner("salt", "scripts/salts.lua"),
    # The pool engine: per-resource tick, carry/integrate and revive gate.
    Owner("hydration", "scripts/unit_resource_tick.lua"),
    Owner("hydration", "scripts/unit_resource_carry.lua"),
    Owner("nutrition", "scripts/unit_resource_tick.lua"),
    Owner("nutrition", "scripts/unit_resource_carry.lua"),
    Owner("nutrition", "scripts/unit_resource_energy.lua"),
    Owner("nutrition", "scripts/unit_resource_regrowth.lua"),
    # Stamina (#2834): the pool engine ticks and reads it; every other caller
    # reads through the facade, and every write goes through the atomic
    # commit (see ATOMIC_VERB_CALLERS).
    Owner("stamina", "scripts/unit_resource_tick.lua"),
    # Exhaustion and sleep (#2835): the same pool engine ticks them while
    # Simulated, and unit_resource_params.lua is where each pool is registered
    # to its system (it names the keys, and touches no state). Their consumers
    # (scripts/exhaustion.lua, unit_ai_sleep.lua, the Status row) read through
    # the facade and are NOT owners.
    Owner("exhaustion", "scripts/unit_resource_tick.lua"),
    Owner("exhaustion", "scripts/unit_resource_params.lua"),
    Owner("sleep", "scripts/unit_resource_tick.lua"),
    Owner("sleep", "scripts/unit_resource_params.lua"),
    # The failure-meter engine: only the meters of the systems that declare
    # them, never their pools.
    Owner("thermo", "scripts/unit_resource_failure.lua", "meters"),
    Owner("salt", "scripts/unit_resource_failure.lua", "meters"),
)


@dataclass(frozen=True)
class FixtureWrites:
    """The probe's scenario setup writes, pinned SITE BY SITE: a write is
    allowed only from an embedded chunk whose (Python scope, normalized Lua
    text) is in `sites`. The scope is the dotted chain of enclosing Python
    functions (a nested measurement helper is a different scope), and the
    text is the whole chunk, so editing a pinned chunk (another write, a
    different key, an extra statement) or putting a write into any other
    chunk -- an observation, an aggregate, a helper -- voids it. An
    f-string field reads as ``0``. A pinned site the probe no longer
    contains is itself a finding, so the pins cannot go stale silently."""
    file: str
    verb: str
    sites: frozenset[tuple[str, str]]
    why: str


PROBE_FIXTURE_WRITES = FixtureWrites(
    PROBE_PATH, "unit.setStat",
    frozenset({
        ("hunger_section",
         "local u=0; local mc=unit.getStat(u,'max_calories'); "
         "unit.setStat(u,'calories',mc*0.5); unit.setStat(u,'hunger',0); "
         "unit.removeItem(u,'rations'); unit.removeItem(u,'rations'); "
         "return 'ok'"),
        ("hunger_section",
         "local u=0; unit.setStat(u,'hunger',0); "
         "unit.setStat(u,'calories',unit.getStat(u,'max_calories')*0.5); "
         "return 'ok'"),
        ("hunger_section",
         "local u=0; unit.removeItem(u,'rations'); "
         "unit.removeItem(u,'rations'); unit.setStat(u,'hunger',0); "
         "return 'ok'"),
        ("hunger_section",
         "local u=0; unit.removeItem(u,'quinoa_sack'); "
         "unit.addItem(u,'quinoa_sack',0.02); unit.setStat(u,'hunger',0); "
         "return 'ok'"),
        ("hunger_section",
         "local u=0; unit.setStat(u,'calories',1); "
         "unit.setStat(u,'hunger',0); unit.setStat(u,'core_temp',34.0); "
         "return 'ok'"),
        ("hunger_section",
         "local u=0; unit.setStat(u,'calories',"
         "unit.getStat(u,'max_calories')); "
         "unit.setStat(u,'core_temp',34.0); return 'ok'"),
        ("starvation_bands_section",
         "local u=0; unit.setStat(u,'hunger',0); return 'ok'"),
        ("starvation_bands_section.set_frac",
         "local u=0; unit.setStat(u,'calories',"
         "unit.getStat(u,'max_calories')*0); return 'ok'"),
        # Exhaustion (#2835): the exhaustion section forces the pool to a
        # rested, a spent and a near-empty reading. Its READS go through
        # unit.readPhysio; these are the only raw writes, on Simulated units.
        ("exhaustion_section",
         "unit.setStat(0,'exhaustion',0); return 'ok'"),
        ("exhaustion_section",
         "local ms=require('scripts.unit_stats'); "
         "unit.setStat(0,'exhaustion',ms.get(0,'max_exhaustion')); "
         "return 'ok'"),
        ("exhaustion_section",
         "local ms=require('scripts.unit_stats'); "
         "unit.setStat(0,'exhaustion',ms.get(0,'max_exhaustion') * 0.05); "
         "return 'ok'"),
    }),
    "scenario fixtures (D-7: raw setStat is allowed for debugging)")

# The mutating verbs a file other than the facade may call, by verb. A verb
# absent here is facade-only. ``commitStamina`` is the atomic stamina commit
# (#2470): the pool engine drives the tick's regen/drain through it.
ATOMIC_VERB_CALLERS: dict[str, frozenset[str]] = {
    "unit.commitStamina": frozenset({"scripts/unit_resource_tick.lua"}),
}


@dataclass(frozen=True)
class Policy:
    owners: tuple[Owner, ...] = OWNERS
    facade: str = FACADE
    generic_accessors: frozenset[str] = GENERIC_ACCESSORS
    probe_fixture: FixtureWrites = PROBE_FIXTURE_WRITES
    verb_callers: dict[str, frozenset[str]] = field(
        default_factory=lambda: dict(ATOMIC_VERB_CALLERS))


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
            if not isinstance(h, dict) or sum(
                    k in h for k in ("const", "from", "rule")) != 1:
                out.append(f"{REGISTRY_PATH}: system '{sid}' owned key "
                           f"'{o.get('key')}' has no healthy rule "
                           "(exactly one of const: / from: / rule:)")
        live_derived = set()
        for d in s.get("derived") or []:
            h = d.get("healthy")
            numeric = isinstance(h, (int, float)) and not isinstance(h, bool)
            if not d.get("rule") or not (numeric or h == "live"):
                out.append(f"{REGISTRY_PATH}: system '{sid}' derived key "
                           f"'{d.get('key')}' needs a rule and a healthy value "
                           "(a number, or `live`)")
            if h == "live":
                live_derived.add(d.get("key"))
        for section in ("contributes", "provides", "effects"):
            for e in s.get(section) or []:
                if e.get("substitute") is None:
                    out.append(f"{REGISTRY_PATH}: system '{sid}' {section} "
                               f"entry '{e.get('name')}' has no Nominal substitute")
                elif e.get("substitute") == "live" and not (
                        section == "provides" and e.get("name") in live_derived):
                    out.append(f"{REGISTRY_PATH}: system '{sid}' {section} "
                               f"entry '{e.get('name')}' has a `live` substitute "
                               "but is not a provided live derived key of its "
                               "own system")
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
            if ch.path in policy.verb_callers.get(mv.verb, frozenset()):
                continue
            allowed = [policy.facade] + sorted(
                policy.verb_callers.get(mv.verb, frozenset()))
            findings.append(Finding(mv.path, mv.line,
                f"mutating physiology verb {mv.verb} outside the facade "
                f"(only {' and '.join(allowed)} may call it; the read "
                "verbs are allowed everywhere)"))

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
        # One entry per CALL: two calls on a line are two sites (the token
        # index tells them apart), a call's read and write are one.
        sk = (a.path, a.line, a.site, a.key, a.verb, a.function)
        s = sites.setdefault(sk, {"acc": a, "kinds": set(), "fallback": False})
        s["kinds"].add(a.access)
        s["fallback"] |= a.fallback

    for sk in sorted(sites, key=lambda k: (k[0], k[1], k[2], k[3])):
        a, kinds, fb = sites[sk]["acc"], sites[sk]["kinds"], sites[sk]["fallback"]
        if a.path == policy.facade or _owner_allows(policy, a.path, reg, a.key):
            continue
        fx = policy.probe_fixture
        if a.path == fx.file and a.verb == fx.verb and a.straight \
                and (a.scope, a.sig) in fx.sites:
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

    # A pinned fixture site the probe no longer contains is stale.
    if probe is not None:
        fx = policy.probe_fixture
        present = {(ch.scope, ch.sig) for ch in chunks if ch.path == fx.file}
        for scope, sig in sorted(fx.sites - present):
            findings.append(Finding(fx.file, 1,
                f"pinned fixture site in {scope}() is no longer in the probe "
                f"(update PROBE_FIXTURE_WRITES): {sig[:80]}..."))

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
          "no UI exception)")
    return 0


if __name__ == "__main__":
    sys.exit(main())
