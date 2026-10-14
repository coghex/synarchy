"""Self-test for tools/physiology_access_audit.py (#2830).

Every case builds a tiny in-memory corpus (path -> Lua text), runs the real
audit over it with the REAL policy, and asserts on the findings. The corpus
is never the repository: the audit's job is the rule, and the rule is tested
by what it admits and what it rejects.
"""
from __future__ import annotations

import sys
from pathlib import Path

sys.path.insert(0, str(Path(__file__).resolve().parent))

import physiology_access_audit as audit  # noqa: E402
import physiology_access_lua as lua  # noqa: E402

REGISTRY = {
    "systems": [
        {"id": "thermo", "requires": ["body"],
         "owns": [{"key": "core_temp", "healthy": {"const": "THERMO_BASELINE"}}],
         "meters": ["hypothermia", "hyperthermia"],
         "provides": [{"name": "core_temp", "substitute": {"value": 37.0}}],
         "contributes": [{"name": "sweat_debit", "substitute": "none"}]},
        {"id": "salt", "requires": ["body"],
         "owns": [{"key": "salt", "healthy": {"from": "body_mass"}},
                  {"key": "salt_store", "healthy": {"from": "body_mass"}}],
         "derived": [{"key": "salt_conc", "rule": "salt_concentration",
                      "healthy": 1.0}],
         "meters": ["salt_imbalance"]},
        {"id": "hydration", "requires": ["body"],
         "owns": [{"key": "hydration", "healthy": {"from": "max_hydration"}}]},
        {"id": "nutrition", "requires": ["body"],
         "owns": [{"key": "hunger", "healthy": {"from": "max_hunger"}},
                  {"key": "calories", "healthy": {"from": "max_calories"}}],
         "contributes": [{"name": "meal_salt_credit",
                          "substitute": "virtual_meal"}]},
    ]
}

# A corpus in which every owner file exists and names its keys and nothing
# violates a rule: the baseline every case perturbs.
BASE: dict[str, str] = {
    "scripts/physiology.lua":
        'local p = {}\nfunction p.get(uid, key) return unit.readPhysio(uid, key) end\n'
        'function p.credit(uid, key, a) unit.setStat(uid, key, a) end\nreturn p\n',
    "scripts/unit_stats.lua":
        'local stats = {}\nfunction stats.get(uid, name) return unit.getStat(uid, name) end\n'
        'return stats\n',
    "scripts/thermo.lua": 'local t = {}\nfunction t.tick(uid)\n'
        '  local c = unit.getStat(uid, "core_temp") or 37\n'
        '  unit.setStat(uid, "core_temp", c)\nend\nreturn t\n',
    "scripts/salts.lua": 'local s = {}\nfunction s.tick(uid)\n'
        '  unit.setStat(uid, "salt", unit.getStat(uid, "salt"))\n'
        '  unit.setStat(uid, "salt_store", 1)\n  unit.setStat(uid, "salt_conc", 1)\nend\n'
        'return s\n',
    "scripts/unit_resource_tick.lua":
        'local M = {}\nfunction M.tickResource(uid, resourceName)\n'
        '  local v = unit.getStat(uid, resourceName)\n'
        '  if resourceName == "hydration" then return v end\n'
        '  if resourceName == "hunger" then return v end\nend\nreturn M\n',
    "scripts/unit_resource_carry.lua":
        'local M = {}\nfunction M.integrate(uid, name, d)\n'
        '  unit.setStat(uid, name, (unit.getStat(uid, name) or 0) + d)\nend\nreturn M\n',
    "scripts/unit_resource_energy.lua":
        'local carry = require("scripts.unit_resource_carry")\nlocal M = {}\n'
        'function M.tickDigestion(uid)\n  carry.integrate(uid, "calories", 1)\n'
        '  unit.setStat(uid, "hunger", unit.getStat(uid, "calories"))\nend\nreturn M\n',
    "scripts/unit_resource_regrowth.lua":
        'local M = {}\nlocal function writeStat(uid, name, v)\n'
        '  unit.setStat(uid, name, v)\nend\nreturn M\n',
    "scripts/unit_resource_failure.lua":
        'local METERS = { { stat = "hypothermia" }, { stat = "hyperthermia" },\n'
        '  { stat = "salt_imbalance" } }\nlocal M = {}\nfunction M.tick(uid)\n'
        '  for _, m in ipairs(METERS) do\n'
        '    unit.setStat(uid, m.stat, unit.getStat(uid, m.stat) or 0)\n  end\nend\n'
        'return M\n',
    "scripts/unit_survival_exempt.lua":
        'local M = {}\nlocal SURVIVAL_METERS = { "hypothermia", "hyperthermia",\n'
        '  "salt_imbalance" }\nfunction M.neutralize(uid)\n'
        '  unit.setStat(uid, "core_temp", 37)\n  unit.setStat(uid, "salt", 70)\n'
        '  unit.setStat(uid, "salt_conc", 1.0)\n'
        '  for _, stat in ipairs(SURVIVAL_METERS) do\n    unit.setStat(uid, stat, 0)\n'
        '  end\nend\nreturn M\n',
    "scripts/unit_info_panel.lua":
        'local M = {}\nfunction M.formatStatus(uid)\n'
        '  local h = unit.getStat(uid, "hydration")\n'
        '  local g = unit.getStat(uid, "hunger")\n'
        '  local c = unit.getStat(uid, "calories")\n  return h, g, c\nend\nreturn M\n',
    "scripts/unit_info_v2_stat_defs.lua":
        'local M = {}\nfunction M.fmtCurMax(uid, curName, maxName)\n'
        '  local stats = require("scripts.unit_stats")\n'
        '  return stats.get(uid, curName), stats.get(uid, maxName)\nend\n'
        'function M.lowFrac(uid, cur, maxName, frac)\n'
        '  return require("scripts.physiology").fraction(uid, cur, maxName) < frac\n'
        'end\nreturn M\n',
    "scripts/unit_info_v2_status.lua":
        'local physiology = require("scripts.physiology")\n'
        'local statDefs = require("scripts.unit_info_v2_stat_defs")\n'
        'local fmtCurMax = statDefs.fmtCurMax\nlocal M = {}\n'
        'function M.salt(uid) return physiology.get(uid, "salt_conc") end\n'
        'function M.rows(u) return fmtCurMax(u, "hunger", "max_hunger"),\n'
        '  fmtCurMax(u, "hydration", "max_hydration") end\nreturn M\n',
}

PROBE_PY = (
    "AGG = (\n"
    '    "local function agg(s) local v=unit.readPhysio(u,s) or 0 return v end "\n'
    '    "return {core=agg(\'core_temp\')}"\n'
    ")\n"
    "def fixture(port, u):\n"
    '    send(port, f"unit.setStat({u},\'calories\',1); unit.setStat({u},\'hunger\',0); "\n'
    '               f"unit.setStat({u},\'core_temp\',34.0); return \'ok\'")\n'
    "def stomach(port, u):\n"
    '    return send(port, f"return unit.readPhysio({u},\'hunger\') or -1")\n'
    "def guard(port, b):\n"
    '    return send(port, f"return unit.getCalories({b}) and \'num\' or \'nil\'")\n'
)

UNITS = {"data/units/acolyte.yaml":
         {"units": [{"name": "acolyte",
                     "stats": {"strength": {"base": 1.0}},
                     "skills": {"balance": {"base": 40}}}]}}


def run_audit(files=None, probe=PROBE_PY, doc=REGISTRY, units=UNITS, **over):
    corpus = dict(BASE)
    corpus.update(files or {})
    for k in [k for k, v in corpus.items() if v is None]:
        del corpus[k]
    return audit.run_audit(corpus, ("tools/physiology_probe.py", probe),
                           doc, units)


def expect_clean(name, **kw):
    f = run_audit(**kw)
    assert not f, f"{name}: expected clean, got:\n  " + "\n  ".join(f)


def expect(name, needle, **kw):
    f = run_audit(**kw)
    assert any(needle in x for x in f), \
        f"{name}: expected a finding containing {needle!r}, got:\n  " + \
        ("\n  ".join(f) if f else "(nothing)")
    return f


def case_baseline_is_clean():
    expect_clean("baseline")


def case_owner_access_and_read_verbs_allowed():
    # Valid owner access is the baseline; add read verbs outside the facade.
    expect_clean("read verbs", files={"scripts/anywhere.lua":
        'local v, m = unit.readPhysio(1, "hydration")\n'
        'local b = unit.readPhysioBase(1, "salt")\n'
        'local x = unit.getPhysioMode(1, "thermo")\n'
        'local y = unit.getPhysioOverride(1, "thermo")\n'
        'local z = unit.getPhysioDefault(1, "thermo")\n'})


def case_unowned_caller_registry_access_rejected():
    expect("thermo reads calories", "raw read of owned key 'calories'",
           files={"scripts/thermo.lua": BASE["scripts/thermo.lua"].replace(
               'return t', 'local k = unit.getStat(1, "calories")\nreturn t')})
    expect("caller write", "raw write of owned key 'core_temp'",
           files={"scripts/other.lua": 'unit.setStat(1, "core_temp", 5)\n'})
    expect("caller read with fallback", "caller-local `or` fallback",
           files={"scripts/other.lua": 'local c = unit.getStat(1, "hydration") or 1\n'})
    expect("stats.get alias", "raw read of derived key 'salt_conc'",
           files={"scripts/other.lua":
                  'local st = require("scripts.unit_stats")\n'
                  'local c = st.get(1, "salt_conc")\n'})
    expect("inline require", "raw read of owned key 'hunger'",
           files={"scripts/other.lua":
                  'local c = require("scripts.unit_stats").get(1, "hunger")\n'})
    expect("getStatBase", "raw read of owned key 'hydration'",
           files={"scripts/other.lua": 'local c = unit.getStatBase(1, "hydration")\n'})


def case_owner_scope_is_ownership_not_directory():
    # The failure-meter engine owns the METERS of thermo/salt, not their pools.
    expect("meter engine pool read", "raw read of owned key 'core_temp'",
           files={"scripts/unit_resource_failure.lua":
                  BASE["scripts/unit_resource_failure.lua"].replace(
                      'return M', 'local c = unit.getStat(1, "core_temp")\nreturn M')})
    # A sibling in the same directory is no owner.
    expect("sibling", "raw write of owned key 'hunger'",
           files={"scripts/unit_resource_alerts.lua": 'unit.setStat(1, "hunger", 0)\n'})
    # An unrelated system's owner may not touch another system's keys.
    expect("salts reads hydration", "raw read of owned key 'hydration'",
           files={"scripts/salts.lua": BASE["scripts/salts.lua"].replace(
               'return s', 'local h = unit.getStat(1, "hydration")\nreturn s')})


def case_mutating_verb_leak_rejected():
    expect("setPhysioMode", "mutating physiology verb unit.setPhysioMode",
           files={"scripts/other.lua": 'unit.setPhysioMode(1, "salt", "nominal")\n'})
    expect("clearPhysioMode", "mutating physiology verb unit.clearPhysioMode",
           files={"scripts/other.lua": 'local f = unit.clearPhysioMode\n'})
    expect_clean("facade may mutate", files={"scripts/physiology.lua":
        BASE["scripts/physiology.lua"] + 'unit.setPhysioMode(1, "salt", "nominal")\n'})


def case_ui_exception_is_exact():
    expect_clean("ui exception baseline")
    # A new raw UI read in another UI file is flagged.
    expect("new UI read", "raw read of owned key 'hydration'",
           files={"scripts/unit_info_v2_panels.lua":
                  'local h = unit.getStat(1, "hydration")\n'})
    # The salt_conc status row is NOT excepted.
    expect("salt_conc status read", "raw read of derived key 'salt_conc'",
           files={"scripts/unit_info_v2_status.lua":
                  BASE["scripts/unit_info_v2_status.lua"].replace(
                      'physiology.get(uid, "salt_conc")',
                      'unit.getStat(uid, "salt_conc") or 1.0')})
    # A SECOND read of an excepted key in the excepted function is flagged.
    expect("second read", "raw read of owned key 'hydration'",
           files={"scripts/unit_info_panel.lua": BASE["scripts/unit_info_panel.lua"]
                  .replace('return h', 'local h2 = unit.getStat(uid, "hydration")\n  return h')})
    # An excepted key in another function of the excepted file is flagged.
    expect("other function", "raw read of owned key 'hunger'",
           files={"scripts/unit_info_panel.lua": BASE["scripts/unit_info_panel.lua"]
                  .replace('return M', 'function M.other(uid)\n'
                           '  return unit.getStat(uid, "hunger")\nend\nreturn M')})
    # A key outside the three enumerated is flagged even in formatStatus.
    expect("other key", "raw read of derived key 'salt_conc'",
           files={"scripts/unit_info_panel.lua": BASE["scripts/unit_info_panel.lua"]
                  .replace('return h', 'local s = unit.getStat(uid, "salt_conc")\n  return h')})


def case_ui_helper_exception_is_exactly_the_authorized_reads():
    base = BASE["scripts/unit_info_v2_stat_defs.lua"]
    anchor = "  return stats.get(uid, curName), stats.get(uid, maxName)\n"
    assert anchor in base
    # A mutation added to the helper voids it: every call site is flagged.
    expect("helper write", "through fmtCurMax()",
           files={"scripts/unit_info_v2_stat_defs.lua":
                  base.replace(anchor, "  unit.setStat(uid, curName, 0)\n" + anchor)})
    # So does another verb ...
    expect("helper getStatBase", "through fmtCurMax()",
           files={"scripts/unit_info_v2_stat_defs.lua":
                  base.replace(anchor, "  local b = unit.getStatBase(uid, curName)\n" + anchor)})
    # ... or a further raw read.
    expect("helper extra read", "through fmtCurMax()",
           files={"scripts/unit_info_v2_stat_defs.lua":
                  base.replace(anchor, "  local c = stats.get(uid, curName)\n" + anchor)})
    # A new call reading a key outside the authorized pool keys is flagged.
    expect("helper new key", "raw read of derived key 'salt_conc'",
           files={"scripts/unit_info_v2_status.lua":
                  BASE["scripts/unit_info_v2_status.lua"] +
                  'local y = fmtCurMax(1, "salt_conc", "max_salt")\n'})
    expect("helper meter key", "raw read of meter key 'hypothermia'",
           files={"scripts/unit_info_v2_status.lua":
                  BASE["scripts/unit_info_v2_status.lua"] +
                  'local y = fmtCurMax(1, "hypothermia", "max_x")\n'})
    # The authorized calls stay clean (the baseline).
    expect_clean("helper authorized")


def case_ui_exception_survives_line_shift_without_admitting_lowFrac():
    base = BASE["scripts/unit_info_v2_stat_defs.lua"]
    shifted = "-- shifted\n" * 40 + base
    expect_clean("fmtCurMax line shift", files={
        "scripts/unit_info_v2_stat_defs.lua": shifted})
    # lowFrac in the SAME file with a raw generic read is not admitted: its
    # call site with a literal registry key is flagged.
    raw_low = shifted.replace(
        'return require("scripts.physiology").fraction(uid, cur, maxName) < frac',
        'local stats = require("scripts.unit_stats")\n'
        '  return stats.get(uid, cur) < frac')
    f = expect("lowFrac raw helper", "through lowFrac()",
               files={"scripts/unit_info_v2_stat_defs.lua": raw_low,
                      "scripts/unit_info_v2_status.lua":
                      BASE["scripts/unit_info_v2_status.lua"] +
                      'local statDefs2 = require("scripts.unit_info_v2_stat_defs")\n'
                      'local lowFrac = statDefs2.lowFrac\n'
                      'local x = lowFrac(1, "hydration", "max_hydration", 0.5)\n'})
    assert all("fmtCurMax" not in x for x in f), f


def case_generic_helper_literal_at_call_site():
    # The lowFrac / fracColorFn shape: a closure over the key parameter.
    src = ('local function fracColorFn(cur, maxName, crit)\n'
           '  return function(uid)\n'
           '    local c = unit.getStat(uid, cur)\n'
           '    local m = unit.getStat(uid, maxName)\n'
           '    return c, m\n  end\nend\n'
           'local rows = { fracColorFn("hunger", "max_hunger", 0.1),\n'
           '               fracColorFn("stamina", "max_stamina", 0.1) }\n')
    f = expect("closure helper", "raw read of owned key 'hunger' (system "
               "'nutrition') through fracColorFn()",
               files={"scripts/other.lua": src})
    assert not any("stamina" in x or "max_hunger" in x for x in f), f
    # A helper never called with a registry key is not a finding.
    expect_clean("helper unused", files={"scripts/other.lua": src.replace(
        'fracColorFn("hunger", "max_hunger", 0.1)', 'fracColorFn("mood", "max_mood", 0.1)')})
    # The sanctioned facade call is not a raw read.
    expect_clean("facade call", files={"scripts/other.lua":
        'local physiology = require("scripts.physiology")\n'
        'local v = physiology.get(1, "hunger")\n'})


def case_dynamic_key_tables_and_loops():
    # The METER_CONDITIONS shape: a raw meter read through a table field.
    meters = ('local METER_CONDITIONS = {\n'
              '  { stat = "hypothermia", label = "H" },\n'
              '  { stat = "salt_imbalance", label = "S" },\n}\n'
              'local function conds(uid)\n'
              '  for _, mc in ipairs(METER_CONDITIONS) do\n'
              '    local v = stats.get(uid, mc.stat)\n  end\nend\n')
    f = expect("meter table", "raw read of meter key 'hypothermia'",
               files={"scripts/other.lua":
                      'local stats = require("scripts.unit_stats")\n' + meters})
    assert any("'salt_imbalance'" in x for x in f), f
    # A loop over an inline literal list.
    expect("inline list", "raw read of owned key 'calories'",
           files={"scripts/other.lua":
                  'for _, k in ipairs({"calories", "stamina"}) do\n'
                  '  local v = unit.getStat(1, k)\nend\n'})
    # A file-local constant.
    expect("local constant", "raw write of owned key 'hydration'",
           files={"scripts/other.lua": 'local K = "hydration"\nunit.setStat(1, K, 0)\n'})
    # A key the scanner cannot see, outside an owner, is itself a finding.
    expect("unresolvable", "unresolvable dynamic key",
           files={"scripts/other.lua":
                  'local function f(uid, t) return unit.getStat(uid, t.name) end\n'})
    # Inside an owner it is the engine's own dynamic access and is allowed.
    expect_clean("owner dynamic")


def case_strings_and_comments_are_not_code():
    expect_clean("comments", files={"scripts/other.lua":
        '-- unit.setStat(1, "hydration", 0)\n'
        '--[[ unit.getStat(1, "hunger") ]]\n'
        'local s = "unit.getStat(1, \\"salt\\")"\n'
        'local t = [[ unit.setPhysioMode(1) ]]\n'})


def case_survival_exemption_is_exactly_neutralize():
    expect_clean("neutralize six keys")
    expect("seventh key", "raw write of owned key 'hydration'",
           files={"scripts/unit_survival_exempt.lua":
                  BASE["scripts/unit_survival_exempt.lua"].replace(
                      '  unit.setStat(uid, "salt", 70)',
                      '  unit.setStat(uid, "salt", 70)\n  unit.setStat(uid, "hydration", 1)')})
    expect("other function", "raw write of owned key 'core_temp'",
           files={"scripts/unit_survival_exempt.lua":
                  BASE["scripts/unit_survival_exempt.lua"].replace(
                      'return M', 'function M.other(uid)\n'
                      '  unit.setStat(uid, "core_temp", 37)\nend\nreturn M')})
    expect("reads are not covered", "raw read of owned key 'core_temp'",
           files={"scripts/unit_survival_exempt.lua":
                  BASE["scripts/unit_survival_exempt.lua"].replace(
                      '  unit.setStat(uid, "salt", 70)',
                      '  local c = unit.getStat(uid, "core_temp")\n'
                      '  unit.setStat(uid, "salt", 70)')})
    expect("another file", "raw write of owned key 'salt'",
           files={"scripts/unit_survival_other.lua":
                  BASE["scripts/unit_survival_exempt.lua"]})


def case_probe_embedded_lua():
    # The baseline probe (fixture writes + readPhysio + getCalories guard)
    # is clean; the guard's `unit.getCalories` is not a stat read.
    expect_clean("probe baseline")
    expect("probe raw read", "tools/physiology_probe.py",
           probe=PROBE_PY.replace("unit.readPhysio({u},'hunger')",
                                  "unit.getStat({u},'hunger')"))
    expect("probe dynamic helper", "through agg()",
           probe=PROBE_PY.replace("unit.readPhysio(u,s)", "unit.getStat(u,s)"))
    # A fixture write of a key outside the enumerated three is flagged.
    expect("probe other write", "raw write of owned key 'hydration'",
           probe=PROBE_PY + 'def f(p,u):\n    send(p, f"unit.setStat({u},\'hydration\',1)")\n')
    # Reads of the fixture keys are still flagged (the rule is writes only).
    expect("probe read of fixture key", "raw read of owned key 'calories'",
           probe=PROBE_PY + 'def g(p,u):\n    send(p, f"return unit.getStat({u},\'calories\')")\n')
    # A NaN-style loop over a literal list.
    expect("probe loop", "raw read of owned key 'core_temp'",
           probe=PROBE_PY + 'NAN = ("for _,s in ipairs({\'core_temp\',\'circulation\'}) do "\n'
                            '       "local v=unit.getStat(u,s) end")\n')


def case_registry_coverage():
    import copy
    # Every system needs a Lua owner.
    doc = copy.deepcopy(REGISTRY)
    doc["systems"].append({"id": "mind", "requires": ["body"],
                           "owns": [{"key": "mood", "healthy": {"const": 1.0}}]})
    expect("no owner", "system 'mind' has no Lua owner", doc=doc)
    # An owned key needs a healthy rule.
    doc = copy.deepcopy(REGISTRY)
    del doc["systems"][0]["owns"][0]["healthy"]
    expect("no healthy", "has no healthy rule", doc=doc)
    doc = copy.deepcopy(REGISTRY)
    doc["systems"][0]["owns"][0]["healthy"] = {"const": 1, "from": "x"}
    expect("two healthy", "has no healthy rule", doc=doc)
    # A contribution needs a Nominal substitute.
    doc = copy.deepcopy(REGISTRY)
    doc["systems"][3]["contributes"][0]["substitute"] = None
    expect("no substitute", "meal_salt_credit has no Nominal substitute"
           if False else "entry 'meal_salt_credit' has no Nominal substitute",
           doc=doc)
    # A derived key needs a rule and a numeric healthy value.
    doc = copy.deepcopy(REGISTRY)
    del doc["systems"][1]["derived"][0]["rule"]
    expect("derived no rule", "derived key 'salt_conc' needs a rule", doc=doc)
    # An owner file must exist and must name a key.
    expect("owner missing", "scripts/thermo.lua: named as the Lua owner of "
           "system 'thermo' but does not exist", files={"scripts/thermo.lua": None})
    expect("owner names nothing", "no Lua owner of system 'thermo' names any",
           files={"scripts/thermo.lua": 'local t = {}\nreturn t\n',
                  "scripts/unit_resource_failure.lua": 'local M = {}\nreturn M\n'})
    # An empty corpus must not pass: no files -> every owner is missing.
    f = audit.run_audit({}, ("tools/physiology_probe.py", PROBE_PY), REGISTRY, UNITS)
    assert f, "an empty scripts corpus passed"


def case_templated_physiology_names_rejected():
    units = {"data/units/bad.yaml": {"units": [{"name": "bad",
             "stats": {"hydration": {"base": 1.0}, "strength": {"base": 1.0}},
             "skills": {"salt_conc": {"base": 1}}}]}}
    f = expect("templated stat", "unit 'bad' templates registry key 'hydration' "
               "in stats:", units=units)
    assert any("salt_conc" in x and "skills:" in x for x in f), f
    expect_clean("unrelated templates")


def case_tokenizer():
    toks = lua.tokenize('local a = "x" -- c\nlocal b = [==[ y ]==] --[[ z\n]] local c = 1')
    assert [t.text for t in toks if t.kind == "str"] == ["x", " y "], toks
    assert [t.line for t in toks if t.text == "c"] == [3], toks


CASES = [v for k, v in sorted(globals().items()) if k.startswith("case_")]


def run() -> int:
    failures = 0
    for fn in CASES:
        try:
            fn()
        except AssertionError as err:
            failures += 1
            print(f"FAIL {fn.__name__}: {err}")
    if failures:
        print(f"physiology access audit self-test: {failures} of {len(CASES)} failed")
        return 1
    print(f"physiology access audit self-test: OK ({len(CASES)} cases)")
    return 0


if __name__ == "__main__":
    sys.exit(run())
