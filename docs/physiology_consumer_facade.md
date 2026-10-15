# Physiology consumer facade and access audit (#2830, S5; activated by #2831, S4)

S5 of the physiology stat-modes epic (#2827; design
`docs/designs/physiology_stat_modes_design.md`, D-1, D-2, D-5a, D-7, D-9,
SD-19 and the S5/S6 temporary UI exception) moved every gameplay reader and
every cross-system writer of a registry-owned key behind one accessor and added
the audit that keeps it there, without changing any behavior while production
modes were inactive. **S4 (#2831) activated that seam**: the facade now answers
the mode-aware contract, `setMode` / `clearMode` / `refresh` exist, and the
tick gates, the write gate and the load normalization stand on it.

This page is the reference for the facade, the system owners, the migrated
interface and the temporary UI exception. The contract summary lives in
`docs/engine_contracts.md` §The physiology system registry, read contract and
modes; the audit's own header documents the scanner.

## 1. The registry-owned keys, and what activation changed

The registry (`data/physiology/systems.yaml`) names the *registry-owned
keys*: the owned pools, the derived key and the failure meters of the four
v1 systems.

| System | Owned | Derived | Meters |
|---|---|---|---|
| `thermo` | `core_temp` | | `hypothermia`, `hyperthermia` |
| `salt` | `salt`, `salt_store` | `salt_conc` | `salt_imbalance` |
| `hydration` | `hydration` | | |
| `nutrition` | `hunger`, `calories` | | |

Until S4 the registry carried an inactive switch (`prModesActive`), because
production had no way to choose a mode and every unit simulated every pool;
the facade reproduced `stats.get` and the wound tick saw stored stats. S4
removed the switch with the thing it guarded. Now:

| Path | Active answer |
|---|---|
| `unit.readPhysio` / `unit.readPhysioBase` | the mode-aware value and mode: healthy for a Nominal system, stored for a Simulated one, `nil, nil` for an Unsupported one |
| Haskell `physioRead` / `physioReadBase` | the same (`ReadOwned` / `ReadUnsupported`) |
| `Unit.Physiology.physioConsumerView` (wound tick) | each named key replaced by its mode-aware effective value, an Unsupported key removed |
| `scripts/physiology.lua` | the mode-aware `get` / `getBase` / `fraction`, receiver-gated `credit` / `debit`, and the transition verbs below |
| `unit.setStat` | refuses a Nominal or Unsupported system's owned or meter key, and a derived key in every mode |

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
| `systemOf(key)`, `systemOrder()` | the owning system id of a key; the registry's system ids in tick order |
| `mode(uid, system)`, `modes(uid)`, `isSimulated(uid, system)` | the resolved mode (`"nominal"` / `"simulated"` / `nil` for Unsupported) of one system, of all of them read once, and the Simulated test the tick gates use |
| `setMode(uid, system, mode [, values])` → `true, changed` or `nil, reason` | the ONLY caller of `unit.setPhysioMode`: seeds or scrubs atomically, resets the carry / regrowth / alert-debounce caches, runs the change hooks on an EFFECTIVE change, refreshes the caches, and refuses atomically (see the engine contract) |
| `clearMode(uid, system)` | the same for `unit.clearPhysioMode` (removes the override) |
| `refresh(uid)` | recompute circulation, then the brain at dt 0 |
| `onModeChanged(id, system, fn)` | register a hook `fn(uid, system, from, to)`, replaced by id |

`credit` / `debit` carry the per-key rules of the callers they replaced
(`RULES` in the module): `hydration` credits clamp to `max_hydration` and
write nothing without a positive maximum, an absent pool starting at 0;
`salt_store` credits clamp to `salts.storeCap` and an absent reserve
starts full (#2761); `salt` and `hydration` debits floor at 0. A key with no
rule answers `nil, reason` and writes nothing. The one place a contribution
is discarded is `contributionApplies`: when the RECEIVING system is not
Simulated, `credit` / `debit` apply 0 (D-4), so a Nominal system keeps no key
and an Unsupported one has none.

Local system-owner updates stay with the owner (`salts.tick` writes its own
pools directly). The facade governs reads from, and contributions between,
systems.

### Haskell

* `Unit.Physiology.physioConsumerView keys capacities ctx` is the pure owned
  branch of `physioRead` for code that already holds the unit instance
  inside an atomic fold. It replaces each named key with its mode-aware
  effective value, removes a key whose system is Unsupported, and replaces
  each present `capacities` key
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
  `tickAllWounds`) rather than tick it on a guessed pool. The reads are
  EFFECTIVE (D-2), so a modifier on a pool or its maximum is honored.

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
(`unit.feed` refuses a unit with no `hunger`, which is every Nominal one);
S6's presentation.

## 4. System owners (the audit's `OWNERS`)

A file may touch the keys of the systems it actually owns, and nothing else.

| System | Lua owner | Notes |
|---|---|---|
| `thermo` | `scripts/thermo.lua` | `core_temp` |
| `salt` | `scripts/salts.lua` | `salt`, `salt_store`, `salt_conc` |
| `hydration` | `scripts/unit_resource_tick.lua`, `scripts/unit_resource_carry.lua` | the pool engine (dynamic keys) |
| `nutrition` | `scripts/unit_resource_tick.lua`, `unit_resource_carry.lua`, `unit_resource_energy.lua`, `unit_resource_regrowth.lua` | digestion, starvation, regrowth |
| `thermo` and `salt` **meters only** | `scripts/unit_resource_failure.lua` | the failure-meter engine; never their pools |

One further allowance, named and bounded (the survival exemption of #2754 that
stood beside it until S4 is gone: `scripts/unit_survival_exempt.lua` was removed
when #2754 became the `exempt` profile, design F-7):

* **Probe fixture writes.** `tools/physiology_probe.py` may `setStat`
  `calories`, `hunger` and `core_temp` as scenario fixtures (D-7 allows raw
  `setStat` for debugging under Simulated), but only from the eight setup
  chunks pinned **site by site** in the audit's `PROBE_FIXTURE_WRITES`: each
  is identified by its dotted Python scope (`hunger_section`,
  `starvation_bands_section`, `starvation_bands_section.set_frac`) and its
  whole normalized Lua text. Editing a pinned chunk, or putting a write into
  any other chunk (an observation, the aggregate, a Lua helper, a nested
  measurement function such as `avg_hyd`), is flagged, and a pin the probe no
  longer contains is itself a finding, so the pins cannot go stale silently.
  Changing a setup chunk therefore means updating its pin, deliberately.
  Reads in the probe are not allowed; the numeric calorie observer reads
  through `unit.readPhysio`. If this pinning proves a recurring burden, the
  owner's fallback is to remove the exception by moving the probe's setup
  writes onto the facade or a test setter.

## 5. The temporary UI exception (S5 → S6)

Until S6 a Nominal unit's hydration, hunger and calorie rows read `?`: the
excepted raw reads see an absent key. That is the documented temporary display.

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
cabal test synarchy-test-headless --test-options='--match "physiology stat modes"'
cabal test synarchy-test-headless --test-options='--match "physiology consumer migration"'
python3 tools/physiology_access_audit.py --self-test
python3 tools/physiology_access_audit.py
python3 tools/run_probes.py --only physiology,consumable_effects,state_of_mind --exact
```

`physiology stat modes` is S4's acceptance, run through the real shipped
consumers (see the engine contract). `physiology consumer migration` keeps the
pure half of the S5 gate: `tickOneUnitWith` against `tickOneUnit` and the
closed-form formulas for the wound tick. Its S5 twin runs against the
pre-migration statements proved the seam changed nothing while inactive, and
have no subject now that the switch is gone.

## 8. Handoff

* **S4 (done, #2831)** removed the switch, replaced `contributionApplies`,
  added the setters and the tick gates, removed the survival-exemption
  allowance, and re-derived each consumer's Nominal behavior through this
  seam without migrating a caller.
* **S6** migrates the excepted UI reads and deletes
  `UI_EXCEPTIONS` / `UI_HELPER_EXCEPTIONS` from the audit; its acceptance
  includes the audit passing without them.
* **S7** moves the probe to Nominal assertions where profiles change.
