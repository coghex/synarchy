# Physiology consumer facade and access audit (#2830, S5)

S5 of the physiology stat-modes epic (#2827; design
`docs/designs/physiology_stat_modes_design.md`, D-1, D-2, D-5a, D-7, D-9,
SD-19 and the S5/S6 temporary UI exception). It moves every gameplay reader
and every cross-system writer of a registry-owned key behind one accessor,
adds the audit that keeps it there, and does so **without changing any
behavior**: production modes are inactive until S4, and this slice is the
seam S4 activates.

This page is the reference for the inactive path, the system owners, the
migrated interface and the temporary UI exception. The contract summary
lives in `docs/engine_contracts.md` §The physiology system registry and read
contract; the audit's own header documents the scanner.

## 1. What "inactive" means

The registry (`data/physiology/systems.yaml`) names the *registry-owned
keys*: the owned pools, the derived key and the failure meters of the four
v1 systems.

| System | Owned | Derived | Meters |
|---|---|---|---|
| `thermo` | `core_temp` | | `hypothermia`, `hyperthermia` |
| `salt` | `salt`, `salt_store` | `salt_conc` | `salt_imbalance` |
| `hydration` | `hydration` | | |
| `nutrition` | `hunger`, `calories` | | |

S2 (#2828) built the mode-aware read contract: under Nominal an owned key
reads its healthy rule, under Simulated its stored value. Production has no
way to choose a mode yet (the setters are S4's), so every system would
resolve Nominal and every stored pool would be ignored. **That must not
reach a consumer**, because today every unit simulates every pool.

The single switch is `PhysiologyRegistry.prModesActive` (a field of the
Rebuild-content registry, never serialized). The loader builds it `False`.
While it is `False`:

| Path | Inactive answer |
|---|---|
| `unit.readPhysio` / `unit.readPhysioBase` | the answer of `unit.getStat` / `unit.getStatBase`, mode `nil`, for **every** key |
| Haskell `physioRead` / `physioReadBase` | the same (`ReadUnowned`) |
| `Unit.Physiology.physioConsumerView` (wound tick) | the unit's stored stats map, unchanged |
| `scripts/physiology.lua` | today's `stats.get` / `getStatBase` / cur-over-max / pool write semantics, built on the verbs above |
| `unit.getPhysioMode` / `getPhysioOverride` / `getPhysioDefault` | unchanged: they report configuration (the resolver), not stat values |

`Unit.Physiology.physioReadPure` stays mode-aware whatever the flag says:
it is the pure resolver, and the boundary is applied by its callers. The
mode-aware answer is reachable in production **only** by setting the flag;
a controlled test does that with
`Test.Headless.Unit.PhysiologyFixture.activatePhysiologyModes`. S4 turns the
flag on (it reads it from the registry; `engine.getPhysiologyRegistry()`
publishes `modesActive`).

## 2. The migrated interface

### Lua: `scripts/physiology.lua`

| Function | Meaning |
|---|---|
| `get(uid, key)` → `value, mode` | effective value. Unowned keys read as `stats.get` (attribute, then the Lua-derived formulas such as `max_stamina`), mode `nil`. `nil` for an undefined or Unsupported key |
| `getBase(uid, key)` → `value, mode` | the stored base (`unit.getStatBase` for an unowned key) |
| `fraction(uid, key, maxKey)` | `get(key) / get(maxKey)`; `nil` when either is undefined **or the maximum is not positive**. The maximum is read first, so a unit with no usable maximum never has the current stat read. Unclamped |
| `credit(uid, key, amount)` | add to a pool, clamped to its capacity; returns the amount applied; `0` when nothing was written |
| `debit(uid, key, amount)` | remove from a pool, floored at 0; an absent pool is left absent; returns the amount applied |
| `isOwned(key)` | is `key` registry-owned |

`credit` / `debit` carry the per-key rules of the callers they replaced
(`RULES` in the module): `hydration` credits clamp to `max_hydration` and
write nothing without a positive maximum, an absent pool starting at 0;
`salt_store` credits clamp to `salts.storeCap` and an absent reserve
starts full (#2761); `salt` and `hydration` debits floor at 0. A key with no
rule answers `nil, reason` and writes nothing. The one place a Nominal
system will discard a contribution is `contributionApplies` — today it
returns `true`; S4 replaces its body (D-4).

Local system-owner updates stay with the owner (`salts.tick` writes its own
pools directly). The facade governs reads from, and contributions between,
systems.

### Haskell

* `Unit.Physiology.physioConsumerView keys capacities ctx` is the pure owned
  branch of `physioRead` for code that already holds the unit instance
  inside an atomic fold. Inactive it is `Right pcStats` unchanged; active it
  replaces each named key with its mode-aware effective value, removes a key
  whose system is Unsupported, and replaces each present `capacities` key
  (`max_calories`, `max_hydration`) with its EFFECTIVE value so a pool and
  its maximum agree on modifiers, as the Lua facade's `fraction` does. A
  violated invariant (a Simulated pool with no stored value, a healthy-rule
  input that is absent or not positive) is a `Left`, never a stored fallback.
* `Combat.Wounds.Tick.tickOneUnitWith registry …` is `tickOneUnit` under an
  explicit registry (`tickOneUnit` passes the empty one). `tickAllWounds`
  reads the registry once per tick through the `content-registries`
  capability. The wound tick's three reads — `calories` (healing,
  blood-recovery nutrition), `hydration` (blood-recovery nutrition) and
  `core_temp` (fever suppression) — go through the view. A `Left` makes the
  tick return the unit untouched as `PhysioInvariant` (logged by
  `tickAllWounds`) rather than tick it on a guessed pool. They remain
  **base** reads while inactive; D-2's switch to effective reads is a
  behavior change only when a modifier targets these keys, and none does.

## 3. Consumers migrated

| Reader or writer | Site | Through |
|---|---|---|
| AI drink / eat / forage / water-search utilities | `unit_ai_needs.lua`, `unit_ai_water.lua` | `fraction`, `get` |
| Autonomous canteen drink credit | `unit_ai_needs.lua` `drinkExecute` | `credit` |
| Meal salt | `salts.mealSalt` → `salts.deposit` (reached from `eatExecute`) | `credit("salt_store")` |
| Consumable (coffee) hydration | `consumable.lua` `applySip` | `credit("hydration")` |
| Thermo sweat | `thermo.lua` | `debit("hydration")`, `debit("salt")` |
| Thermo calorie throttle | `thermo.lua` | `fraction("calories")` |
| Salt's hydration term, concentration, cramps, sweat factor | `salts.lua` | `fraction`, `get("salt_conc")` |
| Brain consciousness inputs and hunger fraction | `brain.lua` (`fracOf` is a dynamic-key helper shared with stamina) | `get`, `fraction` |
| Cardio temperature input | `cardio.lua` | `get("core_temp")` |
| Starvation calorie fraction | `starvation.lua` | `fraction` |
| Status panel: `salt_conc` row text and colour, hydration conditions (`lowFrac`), pool row colours (`fracColorFn`), meter conditions | `unit_info_v2_status.lua`, `unit_info_v2_stat_defs.lua` | `get`, `fraction` |
| Wound healing, blood-recovery nutrition, fever suppression | `Healing.hs` / `Wounds/Tick.hs` | `physioConsumerView` |
| Physiology probe aggregate, stomach and hydration observations | `tools/physiology_probe.py` | `unit.readPhysio` (circulation remains an unowned pass-through read) |

Not migrated, by design: stamina, circulation, blood, injury and mental-state
redesigns; the engine's own `unit.feed` / `unit.getCalories` food primitives
(S4's virtual meals); S6's presentation.

## 4. System owners (the audit's `OWNERS`)

A file may touch the keys of the systems it actually owns, and nothing else.

| System | Lua owner | Notes |
|---|---|---|
| `thermo` | `scripts/thermo.lua` | `core_temp` |
| `salt` | `scripts/salts.lua` | `salt`, `salt_store`, `salt_conc` |
| `hydration` | `scripts/unit_resource_tick.lua`, `scripts/unit_resource_carry.lua` | the pool engine (dynamic keys) |
| `nutrition` | `scripts/unit_resource_tick.lua`, `unit_resource_carry.lua`, `unit_resource_energy.lua`, `unit_resource_regrowth.lua` | digestion, starvation, regrowth |
| `thermo` and `salt` **meters only** | `scripts/unit_resource_failure.lua` | the failure-meter engine; never their pools |

Two further allowances, each named and bounded:

* **Survival exemption (#2754).** `neutralize` in
  `scripts/unit_survival_exempt.lua` may `setStat` exactly `core_temp`,
  `salt`, `salt_conc`, `hypothermia`, `hyperthermia` and `salt_imbalance`.
  It is the substitute tick for exempt definitions (it replaces `thermo.tick`
  and `salts.tick`). It is not a UI exception and not a module-wide
  allowance. **S4 removes it** when #2754 becomes the `exempt` profile
  (design F-7, D-7 "the `survivalExempt` branch goes away").
* **Probe fixture writes.** `tools/physiology_probe.py` may `setStat`
  `calories`, `hunger` and `core_temp` as scenario fixtures (D-7 allows raw
  `setStat` for debugging under Simulated), only from a straight-line
  embedded chunk (no function, loop or branch, so never an aggregate or a
  Lua helper) in one of three named Python scopes, each with its own keys:
  `hunger_section` (`calories`, `hunger`, `core_temp`),
  `starvation_bands_section` (`hunger`) and its nested `set_frac`
  (`calories`). A measurement helper nested in a scenario (`avg_hyd`) is a
  different scope and is not allowed to write. Reads in the
  probe are not allowed; the numeric calorie observer reads through
  `unit.readPhysio`. Storage-absence guards (`unit.getCalories`, `unit.feed` refusal)
  keep their calls. S4 and S7 own their Nominal reinterpretation.

## 5. The temporary UI exception (S5 → S6)

Exactly these legacy pool reads stay raw until S6's Nominal-aware display,
identified by file, enclosing function, verb and key (never a line number),
each key admitted **once**:

* `scripts/unit_info_panel.lua`, `formatStatus`: the `hydration`, `hunger`
  and `calories` `unit.getStat` reads.
* `scripts/unit_info_v2_stat_defs.lua`, `fmtCurMax`: the two generic
  `stats.get` pool reads. Its call sites are admitted only for the pool keys
  `hydration`, `hunger` and `calories`, and only while the helper is exactly
  those two `stats.get` reads: a mutation, another verb or a further read
  added to it voids the exception and every call site is flagged.

Migrated here and **not** excepted: both `salt_conc` reads in
`unit_info_v2_status.lua`; `lowFrac` (hydration conditions, and stamina);
`fracColorFn` (hunger, calories and hydration row colours); and the
`METER_CONDITIONS` read of `hypothermia`, `hyperthermia` and
`salt_imbalance`. Any other raw UI read is flagged. S6 migrates the
excepted reads and deletes the exception; no permanent UI exemption is
authorized.

## 6. The access audit

`python3 tools/physiology_access_audit.py` (and `--self-test`), in the
`static-audits` job and `tools/ci-local.sh`. It checks:

1. **Raw access**: `unit.getStat`, `getStatBase`, `setStat` and the `get` of
   any alias of `scripts.unit_stats` on a registry key outside its owner,
   including a caller-local `or <default>`.
2. **Mutating verbs**: `unit.setPhysioMode` / `clearPhysioMode` anywhere but
   `scripts/physiology.lua`. The five read verbs are allowed everywhere.
3. **Registry coverage**: every system has an existing Lua owner naming one of
   its keys; every owned key a healthy rule; every derived key a rule and a
   healthy value; every `contributes` / `provides` / `effects` entry a
   Nominal substitute.
4. **Templating**: no unit definition templates a registry key in `stats:`
   or `skills:`.

Scope: all `scripts/**/*.lua` and the Lua embedded in
`tools/physiology_probe.py` (extracted from its Python string literals; an
f-string field becomes `0`).

**Supported forms and limits.** The scanner understands a string-literal
key; a loop variable over a literal list (inline or file-local); `v.field`
for a loop variable over a local table of records (the `METER_CONDITIONS`
shape); a file-local string constant; and a function parameter of the
function or of an enclosing closure, which makes the function a *generic
helper* whose call sites with a literal registry key are reported at the
call site (the `lowFrac` / `fracColorFn` shape). Helpers resolve by bare
name, one level deep. A key it cannot resolve is allowed inside an owner or
a generic accessor (the facade, `scripts/unit_stats.lua`) and a finding
elsewhere. It does **not** follow data flow (`unit.getAllStats` snapshots),
engine verbs that move registry state themselves (`unit.feed`), or Haskell
(covered by `physioConsumerView` and the hspec group). Other probes and
headless fixtures with raw access (for example `tools/meal_waste_probe.py`,
`canteen_instance_probe.py`, `state_of_mind_probe.py`, `foraging_probe.py`)
are debugging code outside the gameplay-consumer rule.

## 7. Gates

```sh
cabal test synarchy-test-headless --test-options='--match "physiology consumer migration"'
python3 tools/physiology_access_audit.py --self-test
python3 tools/physiology_access_audit.py
python3 tools/run_probes.py --only physiology,consumable_effects,state_of_mind --exact
```

`physiology consumer migration` compares the migrated modules with the paths
they replaced: twin runs against the verbatim pre-S5 statements for the
facade, scenario outputs recorded from the pre-S5 modules (`0482a98ef`) for
thermo, salts, brain, cardio, starvation, the AI needs, the consumable drink
and the status rows, and the wound tick against the pure formulas. A
definition that resolves every system to Nominal is registered with the
shipped registry loaded, and the verbs, the facade and the wound tick must
still return stored values; activating the registry is the contrast case.

## 8. Handoff

* **S4** sets the flag, replaces `contributionApplies`, adds the setters
  and the tick gates, removes the survival-exemption allowance, and
  re-derives each consumer's Nominal behavior through this seam, without
  migrating a caller.
* **S6** migrates the excepted UI reads and deletes
  `UI_EXCEPTIONS` / `UI_HELPER_EXCEPTIONS` from the audit; its acceptance
  includes the audit passing without them.
* **S7** moves the probe to Nominal assertions where profiles change.
