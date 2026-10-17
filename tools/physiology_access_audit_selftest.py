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
        {"id": "stamina", "requires": ["body"],
         "owns": [{"key": "stamina", "healthy": {"rule": "max_stamina"}}],
         "provides": [{"name": "stamina_fraction",
                       "substitute": {"value": 1.0}}]},
        # S8b (#2835): exhaustion and sleep. The sleep system owns
        # sleep_pressure, so a system id is not always a key.
        {"id": "exhaustion", "requires": ["body"],
         "owns": [{"key": "exhaustion",
                   "healthy": {"from": "endurance", "scale": 10}}],
         "provides": [{"name": "restedness_fraction",
                       "substitute": {"value": 1.0}}]},
        {"id": "sleep", "requires": ["body"],
         "owns": [{"key": "sleep_pressure",
                   "healthy": {"from": "endurance", "scale": 10}}],
         "effects": [{"name": "sleep_need", "substitute": "none"}]},
        # S9 (#2836): cardio owns two integrators and derives circulation,
        # whose Nominal answer is the live rule, not a constant (`live`).
        {"id": "cardio", "requires": ["body"],
         "owns": [{"key": "heart_rate", "healthy": {"const": "RESTING_HR"}},
                  {"key": "blood_oxygen", "healthy": {"const": 1.0}}],
         "derived": [{"key": "circulation", "rule": "circulation",
                      "healthy": "live"}],
         "provides": [{"name": "heart_rate",
                       "substitute": {"value": "RESTING_HR"}},
                      {"name": "circulation", "substitute": "live"}],
         "effects": [{"name": "oxygen_path_hypoxia", "substitute": "none"}]},
        # S10 (#2837): blood stores its volume in the dedicated field uiBlood, so
        # it has a HASKELL owner (HASKELL_OWNERS) and no Lua one.
        {"id": "blood", "requires": ["body"],
         "owns": [{"key": "blood", "field": "uiBlood",
                   "healthy": {"rule": "blood_max"}}],
         "derived": [{"key": "blood_max", "rule": "blood_max", "healthy": "live"},
                     {"key": "blood_fraction", "rule": "blood_fraction",
                      "healthy": "live"}],
         "provides": [{"name": "blood_max", "substitute": "live"}],
         "contributes": [{"name": "visual_loss", "substitute": "wound_flow"}],
         "effects": [{"name": "volume_drain", "substitute": "none"}]},
        # S11a (#2838): consciousness owns nothing stored (two DERIVED values
        # whose Nominal answer is a constant); mind owns the durable keys and
        # derives state_of_mind live. Both have Lua owners (brain.lua,
        # mental_state.lua, thoughts.lua) that write only through the facade.
        {"id": "consciousness", "requires": ["body"],
         "derived": [{"key": "consciousness", "rule": "consciousness",
                      "healthy": 1.0},
                     {"key": "concentration", "rule": "concentration",
                      "healthy": 1.0}],
         "provides": [{"name": "consciousness", "substitute": {"value": 1.0}}],
         "effects": [{"name": "delirium", "substitute": "none"}]},
        {"id": "mind", "requires": ["body"],
         "owns": [{"key": "mood", "healthy": {"const": 1.0},
                   "seed": {"rule": "mood_target"}},
                  {"key": "caffeine", "healthy": {"const": 0.0}},
                  {"key": "mental_state", "healthy": {"const": 0.0}}],
         "derived": [{"key": "state_of_mind", "rule": "state_of_mind",
                      "healthy": "live"}],
         "provides": [{"name": "state_of_mind", "substitute": "live"}],
         "contributes": [{"name": "caffeine_credit", "substitute": "none"}],
         "effects": [{"name": "mental_breaks", "substitute": "none"}]},
    ]
}

# The #2754 survival-exemption writer, as it stood until S4 (#2831): kept as
# a fixture so the audit is shown to REJECT it now that no exemption names it.
LEGACY_NEUTRALIZE = (
        'local M = {}\nlocal SURVIVAL_METERS = { "hypothermia", "hyperthermia",\n'
        '  "salt_imbalance" }\nfunction M.neutralize(uid)\n'
        '  unit.setStat(uid, "core_temp", 37)\n  unit.setStat(uid, "salt", 70)\n'
        '  unit.setStat(uid, "salt_conc", 1.0)\n'
        '  for _, stat in ipairs(SURVIVAL_METERS) do\n    unit.setStat(uid, stat, 0)\n'
        '  end\nend\nreturn M\n')

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
        '  unit.setStat(uid, "salt_store", 1)\nend\n'
        'return s\n',
    "scripts/unit_resource_tick.lua":
        'local M = {}\nfunction M.tickResource(uid, resourceName)\n'
        '  local v = unit.getStat(uid, resourceName)\n'
        '  if resourceName == "hydration" then return v end\n'
        '  if resourceName == "hunger" then return v end\n'
        '  if resourceName == "stamina" then return unit.commitStamina(uid, 0) end\n'
        '  if resourceName == "exhaustion" then return v end\n'
        '  if resourceName == "sleep_pressure" then return v end\n'
        'end\nreturn M\n',
    # The cardio integrator (#2836): the owner of heart_rate / blood_oxygen.
    "scripts/cardio.lua": 'local c = {}\nfunction c.tick(uid)\n'
        '  local hr = unit.getStat(uid, "heart_rate") or 70\n'
        '  unit.setStat(uid, "heart_rate", hr)\n'
        '  unit.setStat(uid, "blood_oxygen", 1)\nend\nreturn c\n',
    # Where each pool is registered to its system: it names the keys and
    # touches no state (#2835).
    # The brain and the mental machine (#2838): they name their keys and touch
    # none raw; every read is the facade's and every write one batch.
    "scripts/brain.lua":
        'local physiology = require("scripts.physiology")\nlocal b = {}\n'
        'function b.consciousness(uid) return physiology.get(uid, "consciousness") end\n'
        'function b.tick(uid)\n'
        '  physiology.commit(uid, { mood = physiology.get(uid, "mood") })\nend\nreturn b\n',
    "scripts/mental_state.lua":
        'local physiology = require("scripts.physiology")\nlocal m = {}\n'
        'function m.tick(uid)\n'
        '  if physiology.get(uid, "mental_state") == 0 then\n'
        '    physiology.commit(uid, { mental_state = 1 })\n  end\nend\nreturn m\n',
    "scripts/thoughts.lua":
        'local physiology = require("scripts.physiology")\nlocal t = {}\n'
        'function t.tick(uid)\n'
        '  physiology.commit(uid, { caffeine = 0, mood = physiology.get(uid, "mood") })\nend\n'
        'return t\n',
    "scripts/unit_resource_params.lua":
        'local M = {}\nM.SYSTEM_OF = { exhaustion = "exhaustion",\n'
        '  sleep_pressure = "sleep" }\nreturn M\n',
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
    "scripts/unit_info_panel.lua":
        'local physiology = require("scripts.physiology")\n'
        'local M = {}\nfunction M.formatStatus(uid)\n'
        '  local h = physiology.get(uid, "hydration")\n'
        '  local g = physiology.get(uid, "hunger")\n'
        '  local c = physiology.get(uid, "calories")\n  return h, g, c\nend\nreturn M\n',
    "scripts/unit_info_v2_stat_defs.lua":
        'local M = {}\nfunction M.fmtCurMax(uid, curName, maxName)\n'
        '  local physiology = require("scripts.physiology")\n'
        '  return physiology.get(uid, curName), physiology.get(uid, maxName)\nend\n'
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

# The three pinned setup chunks (scope, text) the test policy names, spelled
# the way the Python source holds them (an f-string field reads as 0).
SITE_HUNGER = ("unit.setStat({u},'calories',1); unit.setStat({u},'hunger',0); "
               "unit.setStat({u},'core_temp',34.0); return 'ok'")
SITE_BANDS = "local u={u}; unit.setStat(u,'hunger',0); return 'ok'"
SITE_FRAC = "local u={u}; unit.setStat(u,'calories',1); return 'ok'"


def _sig(text):
    return " ".join(text.replace("{u}", "0").split())


TEST_POLICY = audit.Policy(probe_fixture=audit.FixtureWrites(
    audit.PROBE_PATH, "unit.setStat",
    frozenset({("hunger_section", _sig(SITE_HUNGER)),
               ("starvation_bands_section", _sig(SITE_BANDS)),
               ("starvation_bands_section.set_frac", _sig(SITE_FRAC))}),
    "test pins"))

PROBE_PY = (
    "AGG = (\n"
    '    "local function agg(s) local v=unit.readPhysio(u,s) or 0 return v end "\n'
    '    "return {core=agg(\'core_temp\')}"\n'
    ")\n"
    "def hunger_section(port, u):\n"
    f'    send(port, f"{SITE_HUNGER}")\n'
    "def stomach(port, u):\n"
    '    return send(port, f"return unit.readPhysio({u},\'hunger\') or -1")\n'
    "def guard(port, b):\n"
    '    return send(port, f"return unit.getCalories({b}) and \'num\' or \'nil\'")\n'
    "def starvation_bands_section(port, u):\n"
    f'    send(port, f"{SITE_BANDS}")\n'
    "    def set_frac(f):\n"
    f'        send(port, f"{SITE_FRAC}")\n'
)

UNITS = {"data/units/acolyte.yaml":
         {"units": [{"name": "acolyte",
                     "stats": {"strength": {"base": 1.0}},
                     "skills": {"balance": {"base": 40}}}]}}


# A Haskell corpus in which every inventoried file mentions the blood field and
# nothing else does: the baseline the blood cases perturb.
BASE_HS: dict[str, str] = {
    path: "-- " + reason + "\nuiBlood\n"
    for path, reason in audit.BLOOD_FIELD_INVENTORY.items()}
BASE_HS["src/Other/Unrelated.hs"] = "module Other.Unrelated where\n"


def run_audit(files=None, probe=PROBE_PY, doc=REGISTRY, units=UNITS,
              hs=None, policy=None, **over):
    corpus = dict(BASE)
    corpus.update(files or {})
    for k in [k for k, v in corpus.items() if v is None]:
        del corpus[k]
    hs_corpus = dict(BASE_HS)
    hs_corpus.update(hs or {})
    for k in [k for k, v in hs_corpus.items() if v is None]:
        del hs_corpus[k]
    return audit.run_audit(corpus, ("tools/physiology_probe.py", probe),
                           doc, units, policy or TEST_POLICY, hs_files=hs_corpus)


def expect_clean(name, **kw):
    f = run_audit(**kw)
    assert not f, f"{name}: expected clean, got:\n  " + "\n  ".join(f)


def expect(name, needle, **kw):
    f = run_audit(**kw)
    assert any(needle in x for x in f), \
        f"{name}: expected a finding containing {needle!r}, got:\n  " + \
        ("\n  ".join(f) if f else "(nothing)")
    return f


def _sys(doc, sid):
    return next(x for x in doc["systems"] if x["id"] == sid)


def case_blood_field_owner_and_inventory():
    # blood stores its volume in uiBlood: a HASKELL owner, no Lua one (#2837)
    expect_clean("blood baseline")
    import copy
    # a field-backed system with no Haskell owner is a coverage finding
    pol = audit.Policy(probe_fixture=TEST_POLICY.probe_fixture, haskell_owners={})
    r = run_audit(policy=pol)
    assert any("has no Haskell owner" in x for x in r), r
    # an owner file that is gone, or never mentions the field
    expect("owner missing", "named as the Haskell owner of system 'blood' but "
           "does not exist", hs={"src/Unit/Blood.hs": None})
    expect("owner silent", "never mentions uiBlood",
           hs={"src/Unit/Blood.hs": "module Unit.Blood where\n"})
    # a new reader of the field that is not inventoried is flagged
    expect("untracked reader", "touches the blood field or bloodView but is not "
           "in BLOOD_FIELD_INVENTORY",
           hs={"src/Sneaky/Reader.hs": "x = uiBlood inst\n"})
    expect("untracked view", "src/Sneaky/View.hs: touches the blood field",
           hs={"src/Sneaky/View.hs": "v = bloodViewOf s 1 0\n"})
    # an inventory entry that stopped touching the field is stale
    expect("stale entry", "no longer touches the blood field",
           hs={"src/Unit/Thread/Command/Motion.hs": "module M where\n"})
    # an unknown storage field
    doc = copy.deepcopy(REGISTRY)
    _sys(doc, "blood")["owns"][0]["field"] = "uiMystery"
    expect("unknown field", "unknown storage field 'uiMystery'", doc=doc)
    # a derived blood key must still be numeric or live
    doc = copy.deepcopy(REGISTRY)
    _sys(doc, "blood")["derived"][0]["healthy"] = "always"
    expect("blood derived word", "derived key 'blood_max' needs a rule", doc=doc)
    # the wound_flow substitute is a recognized word, never None
    doc = copy.deepcopy(REGISTRY)
    _sys(doc, "blood")["contributes"][0]["substitute"] = None
    expect("no wound flow", "entry 'visual_loss' has no Nominal substitute", doc=doc)


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


def case_stamina_engagement_and_atomic_commit_seams():
    # The SD-20 engagement application is facade-only (#2834).
    expect("applyCombatEngagement outside the facade",
           "mutating physiology verb unit.applyCombatEngagement",
           files={"scripts/other.lua": 'unit.applyCombatEngagement(1)\n'})
    expect_clean("facade applies the engagement", files={"scripts/physiology.lua":
        BASE["scripts/physiology.lua"] + 'unit.applyCombatEngagement(1)\n'})
    # The atomic stamina commit: the facade and the pool engine only.
    expect("commitStamina elsewhere", "mutating physiology verb unit.commitStamina",
           files={"scripts/thermo.lua": BASE["scripts/thermo.lua"].replace(
               'return t', 'unit.commitStamina(1, -1)\nreturn t')})
    expect("commitStamina names both callers", "only scripts/physiology.lua and "
           "scripts/unit_resource_tick.lua may call it",
           files={"scripts/other.lua": 'local r = unit.commitStamina(1, 1)\n'})
    expect_clean("the facade's debit/credit commit", files={"scripts/physiology.lua":
        BASE["scripts/physiology.lua"] + 'unit.commitStamina(1, -1)\n'})
    # The pure maximum read is allowed everywhere.
    expect_clean("getMaxStamina is a read", files={"scripts/other.lua":
        'local m = unit.getMaxStamina(1)\n'})
    # Stamina is owned: a raw read or write outside the pool engine is flagged,
    # the facade's get is not (the thermo shiver was the last raw writer).
    expect("raw stamina read", "raw read of owned key 'stamina'",
           files={"scripts/other.lua": 'local s = unit.getStat(1, "stamina")\n'})
    expect("raw stamina write", "raw write of owned key 'stamina'",
           files={"scripts/thermo.lua": BASE["scripts/thermo.lua"].replace(
               'return t', 'unit.setStat(1, "stamina", 0)\nreturn t')})
    expect_clean("the facade reads stamina", files={"scripts/other.lua":
        'local p = require("scripts.physiology")\n'
        'local f = p.fraction(1, "stamina", "max_stamina")\n'})


def case_brain_and_mind_ownership_and_atomic_batches():
    # The registry's brain keys are owned: a raw read or write outside the
    # facade is flagged against the right system, derived keys included.
    expect("raw mood read", "raw read of owned key 'mood' (system 'mind')",
           files={"scripts/other.lua": 'local m = unit.getStat(1, "mood")\n'})
    expect("raw mental_state write", "raw write of owned key 'mental_state'",
           files={"scripts/thermo.lua": BASE["scripts/thermo.lua"].replace(
               'return t', 'unit.setStat(1, "mental_state", 2)\nreturn t')})
    expect("raw concentration read", "raw read of derived key 'concentration' "
           "(system 'consciousness')",
           files={"scripts/other.lua": 'local c = unit.getStat(1, "concentration") or 1\n'})
    expect("raw state_of_mind read", "raw read of derived key 'state_of_mind'",
           files={"scripts/other.lua": 'local c = unit.getStat(1, "state_of_mind")\n'})
    expect_clean("the facade reads all of them", files={"scripts/other.lua":
        'local p = require("scripts.physiology")\n'
        'local a = p.get(1, "mood")\nlocal b = p.get(1, "concentration")\n'
        'local c = p.get(1, "state_of_mind")\nlocal d = p.get(1, "consciousness")\n'})
    # The atomic batch seam: unit.commitPhysio is the facade's alone, and
    # physiology.commit belongs to the three writers of the mind's keys.
    expect("commitPhysio outside the facade",
           "mutating physiology verb unit.commitPhysio",
           files={"scripts/other.lua": 'unit.commitPhysio(1, { mood = 1 })\n'})
    expect("commitPhysio in a writer", "mutating physiology verb unit.commitPhysio",
           files={"scripts/brain.lua": BASE["scripts/brain.lua"].replace(
               'physiology.commit(', 'unit.commitPhysio(')})
    expect_clean("the facade commits the batch", files={"scripts/physiology.lua":
        BASE["scripts/physiology.lua"] + 'unit.commitPhysio(1, { mood = 1 })\n'})
    expect("another script commits a mind batch",
           "mutating physiology verb physiology.commit",
           files={"scripts/other.lua": 'require("scripts.physiology").commit(1, { mood = 1 })\n'})
    expect("the commit callers are named", "only scripts/physiology.lua and "
           "scripts/brain.lua and scripts/mental_state.lua and scripts/thoughts.lua "
           "may call it",
           files={"scripts/other.lua": 'local physiology = require("scripts.physiology")\n'
                                       'physiology.commit(1, { mood = 1 })\n'})
    expect_clean("a contribution batch is allowed anywhere", files={"scripts/other.lua":
        'local physiology = require("scripts.physiology")\n'
        'physiology.creditBatch(1, { { key = "mood", amount = 0.1 } })\n'})
    # The three writer files each name their system's keys (coverage).
    expect("brain.lua names nothing", "no Lua owner of system 'consciousness' names any",
           files={"scripts/brain.lua": 'local b = {}\nreturn b\n'})
    expect("the mind owners name nothing", "no Lua owner of system 'mind' names any",
           files={"scripts/brain.lua": BASE["scripts/brain.lua"].replace(
                      '"mood"', '"x"').replace('physiology.get(uid, "consciousness")', '1'),
                  "scripts/mental_state.lua": 'local m = {}\nreturn m\n',
                  "scripts/thoughts.lua": 'local t = {}\nreturn t\n'})


def case_exhaustion_and_sleep_ownership():
    # Both pools are owned (#2835): a raw read or write outside the pool engine
    # is flagged against the OWNING system (the sleep pool belongs to `sleep`),
    # the facade's get is not.
    expect("raw exhaustion read", "raw read of owned key 'exhaustion' (system "
           "'exhaustion')",
           files={"scripts/other.lua": 'local e = unit.getStat(1, "exhaustion")\n'})
    expect("raw sleep read", "raw read of owned key 'sleep_pressure' (system "
           "'sleep')",
           files={"scripts/other.lua":
                  'local s = unit.getStat(1, "sleep_pressure")\n'})
    expect("raw sleep write", "raw write of owned key 'sleep_pressure'",
           files={"scripts/thermo.lua": BASE["scripts/thermo.lua"].replace(
               'return t', 'unit.setStat(1, "sleep_pressure", 0)\nreturn t')})
    expect_clean("the facade reads both", files={"scripts/other.lua":
        'local p = require("scripts.physiology")\n'
        'local a = p.get(1, "exhaustion")\n'
        'local b = p.fraction(1, "sleep_pressure", "max_sleep_pressure")\n'})
    # The derived maxima are unowned Lua stats: reading them is no access.
    expect_clean("the maxima are unowned", files={"scripts/other.lua":
        'local stats = require("scripts.unit_stats")\n'
        'local m = stats.get(1, "max_exhaustion")\n'
        'local n = stats.get(1, "max_sleep_pressure")\n'})
    # A system with no owner file naming its key is a coverage finding.
    expect("sleep owners name nothing", "no Lua owner of system 'sleep' names any",
           files={"scripts/unit_resource_tick.lua": 'local M = {}\nreturn M\n',
                  "scripts/unit_resource_params.lua": 'local M = {}\nreturn M\n'})


def case_cardio_ownership_and_derived_circulation():
    # heart_rate / blood_oxygen are owned (#2836): a raw read or write outside
    # cardio.lua is flagged against the cardio system; circulation is DERIVED,
    # so a raw read of it is flagged too (nothing stores it any more).
    expect("raw heart rate read", "raw read of owned key 'heart_rate' (system "
           "'cardio')",
           files={"scripts/other.lua": 'local h = unit.getStat(1, "heart_rate")\n'})
    expect("raw oxygen write", "raw write of owned key 'blood_oxygen'",
           files={"scripts/thermo.lua": BASE["scripts/thermo.lua"].replace(
               'return t', 'unit.setStat(1, "blood_oxygen", 0)\nreturn t')})
    expect("raw circulation read", "raw read of derived key 'circulation' (system "
           "'cardio')",
           files={"scripts/other.lua":
                  'local c = unit.getStat(1, "circulation") or 1.0\n'})
    expect_clean("the facade reads all three", files={"scripts/other.lua":
        'local p = require("scripts.physiology")\n'
        'local a = p.get(1, "heart_rate")\n'
        'local b = p.get(1, "blood_oxygen")\n'
        'local c = p.get(1, "circulation")\n'})
    # The cardio integrator writes its own keys; it is the only file that may.
    expect_clean("cardio.lua owns its integrators")
    # A cardio owner that names none of its keys is a coverage finding.
    expect("cardio owner names nothing", "no Lua owner of system 'cardio' names any",
           files={"scripts/cardio.lua": 'local c = {}\nreturn c\n'})


# The temporary S5 allowance for the legacy pool reads was deleted by S6
# (#2832): the migrated baseline is clean, and every FORM it used to admit
# is flagged like any other raw read.
RAW_PANEL = BASE["scripts/unit_info_panel.lua"].replace(
    'physiology.get(', 'unit.getStat(')
RAW_STAT_DEFS = BASE["scripts/unit_info_v2_stat_defs.lua"].replace(
    '  local physiology = require("scripts.physiology")\n'
    '  return physiology.get(uid, curName), physiology.get(uid, maxName)\n',
    '  local stats = require("scripts.unit_stats")\n'
    '  return stats.get(uid, curName), stats.get(uid, maxName)\n')


def case_former_ui_exception_forms_now_fail():
    expect_clean("migrated UI baseline")
    assert "unit.getStat(uid, \"hydration\")" in RAW_PANEL
    # The three raw pool reads formatStatus used to be excepted for.
    for key in ("hydration", "hunger", "calories"):
        expect(f"former formatStatus {key}", f"raw read of owned key '{key}'",
               files={"scripts/unit_info_panel.lua": RAW_PANEL})
    # The generic fmtCurMax helper's raw reads: its literal-key call sites
    # (the Status rows) are flagged through the helper.
    assert "stats.get(uid, curName)" in RAW_STAT_DEFS
    for key in ("hunger", "hydration"):
        expect(f"former fmtCurMax {key}", f"raw read of owned key '{key}'",
               files={"scripts/unit_info_v2_stat_defs.lua": RAW_STAT_DEFS})
    expect("former fmtCurMax names the helper", "through fmtCurMax()",
           files={"scripts/unit_info_v2_stat_defs.lua": RAW_STAT_DEFS})


def case_new_raw_ui_reads_are_flagged():
    # A new raw UI registry read in another UI file.
    expect("new UI read", "raw read of owned key 'hydration'",
           files={"scripts/unit_info_v2_panels.lua":
                  'local h = unit.getStat(1, "hydration")\n'})
    expect("new UI stats.get", "raw read of owned key 'calories'",
           files={"scripts/unit_info_v2_panels.lua":
                  'local stats = require("scripts.unit_stats")\n'
                  'local c = stats.get(1, "calories")\n'})
    # The salt_conc status row stays on the accessor path.
    expect("salt_conc status read", "raw read of derived key 'salt_conc'",
           files={"scripts/unit_info_v2_status.lua":
                  BASE["scripts/unit_info_v2_status.lua"].replace(
                      'physiology.get(uid, "salt_conc")',
                      'unit.getStat(uid, "salt_conc") or 1.0')})
    # Nor may a raw read hide in a second function or on one line with a
    # sanctioned one in the formerly excepted file.
    expect("other function", "raw read of owned key 'hunger'",
           files={"scripts/unit_info_panel.lua": BASE["scripts/unit_info_panel.lua"]
                  .replace('return M', 'function M.other(uid)\n'
                           '  return unit.getStat(uid, "hunger")\nend\nreturn M')})
    expect("same-line raw read", "raw read of owned key 'hydration'",
           files={"scripts/unit_info_panel.lua": BASE["scripts/unit_info_panel.lua"]
                  .replace('local h = physiology.get(uid, "hydration")',
                           'local h = physiology.get(uid, "hydration"); '
                           'local leaked = unit.getStat(uid, "hydration")')})
    # A key outside the three pools, in the formerly excepted function.
    expect("other key", "raw read of derived key 'salt_conc'",
           files={"scripts/unit_info_panel.lua": BASE["scripts/unit_info_panel.lua"]
                  .replace('return h', 'local s = unit.getStat(uid, "salt_conc")\n  return h')})
    # A new call of a raw helper with a registry key.
    expect("helper meter key", "raw read of meter key 'hypothermia'",
           files={"scripts/unit_info_v2_stat_defs.lua": RAW_STAT_DEFS,
                  "scripts/unit_info_v2_status.lua":
                  BASE["scripts/unit_info_v2_status.lua"] +
                  'local y = fmtCurMax(1, "hypothermia", "max_x")\n'})


def case_facade_ui_helper_survives_line_shift_and_mutation_stays_clean():
    base = BASE["scripts/unit_info_v2_stat_defs.lua"]
    shifted = "-- shifted\n" * 40 + base
    expect_clean("fmtCurMax line shift", files={
        "scripts/unit_info_v2_stat_defs.lua": shifted})
    # lowFrac in the SAME file with a raw generic read is flagged through
    # its call site.
    raw_low = shifted.replace(
        'return require("scripts.physiology").fraction(uid, cur, maxName) < frac',
        'local stats = require("scripts.unit_stats")\n'
        '  return stats.get(uid, cur) < frac')
    expect("lowFrac raw helper", "through lowFrac()",
           files={"scripts/unit_info_v2_stat_defs.lua": raw_low,
                  "scripts/unit_info_v2_status.lua":
                  BASE["scripts/unit_info_v2_status.lua"] +
                  'local statDefs2 = require("scripts.unit_info_v2_stat_defs")\n'
                  'local lowFrac = statDefs2.lowFrac\n'
                  'local x = lowFrac(1, "hydration", "max_hydration", 0.5)\n'})


def case_generic_helper_literal_at_call_site():
    # The lowFrac / fracColorFn shape: a closure over the key parameter.
    src = ('local function fracColorFn(cur, maxName, crit)\n'
           '  return function(uid)\n'
           '    local c = unit.getStat(uid, cur)\n'
           '    local m = unit.getStat(uid, maxName)\n'
           '    return c, m\n  end\nend\n'
           'local rows = { fracColorFn("hunger", "max_hunger", 0.1),\n'
           '               fracColorFn("morale", "max_morale", 0.1) }\n')
    f = expect("closure helper", "raw read of owned key 'hunger' (system "
               "'nutrition') through fracColorFn()",
               files={"scripts/other.lua": src})
    assert not any("morale" in x or "max_hunger" in x for x in f), f
    # A helper never called with a registry key is not a finding.
    expect_clean("helper unused", files={"scripts/other.lua": src.replace(
        'fracColorFn("hunger", "max_hunger", 0.1)', 'fracColorFn("morale", "max_morale", 0.1)')})
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


def case_no_survival_exemption_remains():
    # #2754's neutralize() was a named exemption until S4 (#2831) made it the
    # `exempt` profile. The same code is now an ordinary non-owner writer of
    # owned keys, and every one of its writes is flagged.
    expect("neutralize is no longer exempt", "raw write of owned key 'core_temp'",
           files={"scripts/unit_survival_exempt.lua":
                  LEGACY_NEUTRALIZE})
    expect("each of its keys", "raw write of derived key 'salt_conc'",
           files={"scripts/unit_survival_exempt.lua":
                  LEGACY_NEUTRALIZE})


def case_probe_embedded_lua():
    # The baseline probe (pinned setup chunks + readPhysio + getCalories
    # guard) is clean; the guard's `unit.getCalories` is not a stat read.
    expect_clean("probe baseline")
    expect("probe raw read", "tools/physiology_probe.py",
           probe=PROBE_PY.replace("unit.readPhysio({u},'hunger')",
                                  "unit.getStat({u},'hunger')"))
    expect("probe dynamic helper", "through agg()",
           probe=PROBE_PY.replace("unit.readPhysio(u,s)", "unit.getStat(u,s)"))
    # A write of a key outside the registry's fixtures, anywhere, is flagged.
    expect("probe other write", "raw write of owned key 'hydration'",
           probe=PROBE_PY + 'def f(p,u):\n    send(p, f"unit.setStat({u},\'hydration\',1)")\n')
    # Reads of the fixture keys are still flagged (the rule is writes only).
    expect("probe read of fixture key", "raw read of owned key 'calories'",
           probe=PROBE_PY + 'def g(p,u):\n    send(p, f"return unit.getStat({u},\'calories\')")\n')
    # The exception is the PINNED setup chunks, site by site, not a key set
    # or a scope. A write inserted into an OBSERVATION inside an authorized
    # scenario function is flagged ...
    expect("write in observation", "raw write of owned key 'hunger'",
           probe=PROBE_PY.replace(
               "def stomach(port, u):\n",
               "def stomach_obs(port, u):\n"
               "    return send(port, f\"unit.setStat({u},'hunger',0); "
               "return unit.getStat({u},'max_hunger') or -1\")\n"
               "def hunger_section(port, u):\n"
               "    return send(port, f\"unit.setStat({u},'hunger',0); "
               "return unit.getStat({u},'max_hunger') or -1\")\n"
               "def stomach(port, u):\n"))
    # ... as is an extra write inserted into the fixture chunk itself ...
    expect("edited pinned chunk", "raw write of owned key 'hunger'",
           probe=PROBE_PY.replace("unit.setStat({u},'hunger',0); ",
                                  "unit.setStat({u},'hunger',0); "
                                  "unit.setStat({u},'hunger',1); ", 1))
    # ... and a NEW straight-line chunk of an approved key in an authorized
    # scope that is not one of the pins.
    expect("unpinned chunk in scope", "raw write of owned key 'core_temp'",
           probe=PROBE_PY + 'def hunger_section(p):\n'
                 '    send(p, "unit.setStat(1,\'core_temp\',0); return \'ok\'")\n')
    # The aggregate (a function and loops), a Lua helper, a loop, a nested
    # measurement helper, and an unrelated function are all flagged.
    expect("probe write in aggregate", "raw write of owned key 'core_temp'",
           probe=PROBE_PY.replace("return v end ",
                                  "unit.setStat(u,'core_temp',0) return v end "))
    expect("probe write in helper", "raw write of owned key 'calories'",
           probe=PROBE_PY + 'def hunger_helper(p):\n'
                 '    send(p, "local function reset(u) unit.setStat(u,\'calories\',0) end")\n')
    expect("probe write in loop", "raw write of owned key 'hunger'",
           probe=PROBE_PY + 'def starvation_bands_section(p):\n'
                 '    send(p, "for _,u in ipairs(U) do unit.setStat(u,\'hunger\',0) end")\n')
    expect("probe write in nested helper", "raw write of owned key 'core_temp'",
           probe=PROBE_PY + 'def hunger_section(p):\n'
                 '    def avg_hyd(g):\n'
                 '        return send(p, "unit.setStat(1,\'core_temp\',0); '
                 'return unit.readPhysio(1,\'hydration\')")\n')
    expect("probe write elsewhere", "raw write of owned key 'core_temp'",
           probe=PROBE_PY + 'def unrelated(p):\n'
                 '    send(p, "unit.setStat(1,\'core_temp\',0); return \'ok\'")\n')
    # A pin the probe no longer contains is itself a finding.
    expect("stale pin", "pinned fixture site in starvation_bands_section.set_frac()",
           probe=PROBE_PY.replace(SITE_FRAC, "return 1"))
    # A NaN-style loop over a literal list.
    expect("probe loop", "raw read of owned key 'core_temp'",
           probe=PROBE_PY + 'NAN = ("for _,s in ipairs({\'core_temp\',\'circulation\'}) do "\n'
                            '       "local v=unit.getStat(u,s) end")\n')


def case_registry_coverage():
    import copy
    # Every system needs a Lua owner.
    doc = copy.deepcopy(REGISTRY)
    doc["systems"].append({"id": "psyche", "requires": ["body"],
                           "owns": [{"key": "morale", "healthy": {"const": 1.0}}]})
    expect("no owner", "system 'psyche' has no Lua owner", doc=doc)
    # An owned key needs a healthy rule.
    doc = copy.deepcopy(REGISTRY)
    del doc["systems"][0]["owns"][0]["healthy"]
    expect("no healthy", "has no healthy rule", doc=doc)
    doc = copy.deepcopy(REGISTRY)
    doc["systems"][0]["owns"][0]["healthy"] = {"const": 1, "from": "x"}
    expect("two healthy", "has no healthy rule", doc=doc)
    # ... exactly one of const / from / rule: a named rule is a rule.
    doc = copy.deepcopy(REGISTRY)
    doc["systems"][0]["owns"][0]["healthy"] = {"rule": "max_stamina", "const": 1}
    expect("rule and const", "has no healthy rule", doc=doc)
    expect_clean("a named healthy rule", doc=REGISTRY)
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
    # A derived key's healthy value is a number or `live` (#2836); anything
    # else, or none, is refused.
    doc = copy.deepcopy(REGISTRY)
    _sys(doc, "cardio")["derived"][0]["healthy"] = "always"
    expect("derived word", "derived key 'circulation' needs a rule and a healthy "
           "value", doc=doc)
    doc = copy.deepcopy(REGISTRY)
    del _sys(doc, "cardio")["derived"][0]["healthy"]
    expect("derived none", "derived key 'circulation' needs a rule and a healthy "
           "value", doc=doc)
    expect_clean("a live derived key", doc=REGISTRY)
    # A `live` substitute names a provided live derived key of its own system.
    doc = copy.deepcopy(REGISTRY)
    _sys(doc, "cardio")["provides"][1]["name"] = "oxygen_map"
    expect("live names no derived key", "entry 'oxygen_map' has a `live` "
           "substitute", doc=doc)
    doc = copy.deepcopy(REGISTRY)
    _sys(doc, "cardio")["derived"][0]["healthy"] = 1.0
    expect("live on a constant derived key", "entry 'circulation' has a `live` "
           "substitute", doc=doc)
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
