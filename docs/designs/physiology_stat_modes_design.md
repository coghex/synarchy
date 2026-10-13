# Physiology stat modes design

Give each survival-physiology system an explicit mode, **Nominal** or
**Simulated**, so that "not simulated, healthy" stops being inferred from a
missing `uiStats` key.

Design state: `draft` (revision 3: all owner decisions settled 2026-10-13)

**Preconditions: [#2825](https://github.com/coghex/synarchy/issues/2825)
(mass-proportional sweat costs) and
[#2761](https://github.com/coghex/synarchy/issues/2761) (salt reserve, PR 2824)
are both merged.** This doc assumes both are implemented and is not processed
until both have landed. At processing time, check the reserve's merged
constant names and semantics against SD-4 and D-1.

Request `user-syn-physiology-design-20261013-1` (owner via laz), 2026-10-13.
This is a design draft for owner review. It is not an approval and not an
implementation specification, and it files nothing. Every claim about current
behavior comes from reading the source at `91f6a9d7a`. No engine, probe or
build was run, and no failing probe trace was examined. Proposals are kept
apart from evidence. Owner decisions are listed, in order of urgency, in
[Open decisions](#open-decisions-for-the-owner).

Status legend: `[ ]` unprocessed · `[#N]` linked to issue N ·
`[no-issue]` deliberately not tracked separately · `[deferred]` blocked on
a concrete precondition.

## Processing status

- [ ] EPIC. Explicit physiology stat modes
- [#2825] S1. Mass-proportional sweat costs. Owned by #2825, outside this epic, and a precondition for it
- [ ] S2. System registry, species defaults, resolver and read API (inactive: nothing consumes it)
- [ ] S3. Override storage and wire format (units v4, frozen v3 graph, decode policy); overrides always empty
- [ ] S4. Activation: tick gating, write gate, transitions, derived refresh, nutrition effect gating, dependency contributions, load normalization
- [ ] S5. Consumer migration to the accessor, access audit, probe read
- [ ] S6. UI display of Nominal
- [ ] S7. Species profile table (SD-10), probe update
- [ ] S8. Stamina, exhaustion, sleep pressure (design addendum first)
- [ ] S9. Cardio and circulation (design addendum first)
- [ ] S10. Blood volume and recovery (design addendum first)
- [ ] S11. Brain and mental state (design addendum first)
- [ ] S12. Owner survey of the remaining unit-specific data. Any further systems or stats it finds become slices here; the epic does not close before this survey

No slice has been selected or filed. Revision 2 redrew the slices (R8). The
earlier "behavior-preserving S2" claim was withdrawn, and persistence now
lands before any setter.

## Review round 1 disposition

Codex review: `~/.local/state/syn-physiology-design-review.md`
(syn-solver-21). I checked each claim against the source before applying it.

| Finding | Disposition | Where |
|---|---|---|
| R1 (major): squirrel trigger mis-stated | **Applied.** Recomputed with circulation ≈0.75, which gives an idle equilibrium of 36.8 °C. A meaningful share of idle rolls sweat. The probe disables unit AI, so its squirrels are idle (`tools/physiology_probe.py:50-53`). "Only while it moves" is withdrawn. F-2 is restated as a defect capable of producing the failure, not a reconstruction of a specific run | F-3, Testing T-1 |
| R2 (blocking): Nominal nutrition can still starve through stamina | **Applied.** Verified that the lean-floor kill comes before the calorie check (`scripts/unit_resource_energy.lua:248-252` vs `:255-256`), and that the fat-floor organ-failure drain lives in stamina (`scripts/unit_resource_tick.lua:187-200,259`). Nutrition now owns those effects | D-1, D-9, O-14, T-9 |
| R3 (major): accessor omits modifiers and read-induced writes | **Applied.** No modifier or stat template targets these keys today (grep of `scripts/`, `data/units/`, `data/equipment/`, `data/items/`), so current behavior agrees by accident. The contract now defines effective vs base reads, modifier handling under Nominal, template rejection, and meters in the write gate | D-2, D-7, O-16, T-6/T-7 |
| R4 (major): derived values lack initialization and transition rules | **Applied.** Derived values are a cache refreshed before any transition or load becomes observable. Circulation gets its own scheduling step. Salt seeding under low hydration is now a decision | D-5a, O-17, T-8 |
| R5 (major): I2 oracle unsatisfiable; I4 too broad | **Applied.** Dependencies are now interfaces of reads *and* contributions. I2's reference is a full healthy-interface stub. Its limits are stated, a feedback case is added, and I4 is restricted to salt and hydration debits | D-1, D-4, Testing |
| R6 (major): override presence ambiguous under live defaults | **Applied** with the reviewer's recommended semantics: set records intent, clear removes it | D-3, D-5, O-15, T-10 |
| R7 (major): unknown overrides are not like the immunity scrub | **Applied.** An unknown active system is now rejected by default, with declared retirement and rename migrations. Safe repairs are enumerated. Canonical round trip is kept separate from legacy normalization | D-6, O-8, T-11 |
| R8 (major): delivery order cannot meet its own contracts | **Applied.** Slices redrawn. Bodyless Unsupported is an intentional behavior change at activation | Delivery plan, O-11 |
| R9 (minor): no existing migration chain | **Applied.** Verified that v1 and v2 each migrate straight to the current DTO (`src/World/Save/Component/EntitySnapshots.hs:262-265,740-743,758-770`) and that the dispatcher picks one decoder per version (`src/World/Save/Component/Types.hs:476-478`) | D-6 |

None is disputed. Two points refine the reviewer's framing rather than
contradict it:

- On R3: the gap is in the contract, not in gameplay today.
- On R7: rejecting an unknown system makes retiring a system a content-change
  discipline. Removing one now requires a `retired:` registry entry, or saves
  carrying overrides for it will fail to load.

The reviewer's "Open decisions to add or change" items 1–6 are reflected in
O-1, O-4, O-6, O-8 and O-14 to O-17. Several of these were settled with
the owner in revision 3 (see the settled list).

## Summary

- A **system** is one simulated survival subsystem: `hydration`, `salt`,
  `thermo` or `nutrition`. The mode belongs to the system, not to each raw
  key. Each system declares an **interface**: the values it provides to
  others, the contributions it makes to others, the effects it owns, and a
  Nominal substitute for each.
- Each unit and system resolves to exactly one state: **Unsupported** (the
  species cannot have the system), **Nominal** (healthy semantics, no
  simulation, no contributions beyond the declared substitutes) or
  **Simulated** (live stored values). Whether a key is present in storage
  never decides which one applies.
- The effective mode is the unit's override if present, otherwise its species
  default, otherwise Nominal. An override records explicit intent, and only
  overrides are persisted. Values stay in `uiStats`. The storage invariant:
  under Nominal, owned keys, derived keys and meters are absent; under
  Simulated, owned keys are present.
- v1 covers four systems only. Stamina, exhaustion, sleep pressure, cardio,
  circulation, blood and the injury meters stay always-on, so this is not yet
  a mode for every physiology subsystem. The epic stays open until every
  system has one (SD-12).
- The small-creature salt failure: Nominal hydration's body-water fraction of
  1 is an intended simplification. The sweat costs being absolute rather than
  mass-proportional is a verified source defect, and it is capable of
  producing the failure. The fix keeps the probe's acceptance bounds.

## Current state and evidence

### Storage

- `UnitInstance.uiStats ∷ HM.HashMap Text Float` holds values, and
  `uiModifiers` holds the modifiers, keyed by stat name. They are separate
  maps (`src/Unit/Types/Instance.hs:67-75`). `uiStats` is sparse: "entries
  are filled on demand".
- `unit.getStat` is an **effective** read: base value plus active modifiers,
  clamped (`src/Engine/Scripting/Lua/API/Units/Stats.hs:291-303,628-641`).
  `unit.getStatBase` is the base read (`:315-327`). Both go through
  `getOrRollStat`, which **inserts** a lazily rolled value into `uiStats` when
  the definition has a template for the name (`:570-603`). `unit.getAllStats`
  rolls every authored template (`:608-623`). `unit.setStat` clamps to ≥ 0
  and inserts with no definition check (`:343-366`). No verb deletes a stat.
- The Haskell consumers read the **base** map directly, not effective values:
  `bloodRecoveryNutritionMultiplier` (`src/Combat/Wounds/Healing.hs:148-161`)
  and the fever check (`src/Combat/Wounds/Tick.hs:214`).
- Body recompute seeds the body-derived maxima for **every unit with a body**
  (`src/Unit/Thread/Command/Body.hs:170-212`).
- `uiStats` persists exactly through the `units` component, v3
  (`EntitySnapshots.hs:758-770`). The snapshot field is
  `World.Save.Types.uisStats` (`src/World/Save/Types.hs:759,829,980`).

### What "absent" means today

The same `nil` from `getStat` stands for four different things:

| Meaning | Example |
|---|---|
| Unsupported | `tiller`, `white_tailed_deer` and `unknown_unit` have no `body:` block, yet thermo and salts run on them via `body_mass or 70.0` (`scripts/thermo.lua:117`, `scripts/salts.lua:46`) |
| Not yet initialized | `tickResource`'s first tick: `current == nil` → fill to max (`scripts/unit_resource_tick.lua:165-172`) |
| Deliberately not simulated | red_squirrel, bear and technomule have no `hydration` entry in `scripts/unit_resource_config.lua` |
| Healthy by fallback | every reader's `or 1.0` / `or 37.0`, and Haskell's absent-pool-reads-1.0 |

The #2754 exemption (`scripts/unit_survival_exempt.lua`) writes homeostatic
values every tick for `nomad_primitive`, then on load refreshes circulation
and calls `brain.tick(uid, 0)` before the first, still-paused frame
(`:68-75`, called from `scripts/unit_resources.lua:223-230`). In effect it is
an ad hoc Nominal mode.

### Consumers and effects (by system)

| System | Owned state (writers) | Effects it performs | Readers |
|---|---|---|---|
| hydration | `hydration` (pool tick; thermo sweat `thermo.lua:206-210`; drink `unit_ai_needs.lua:174`; `unit_ai_water.lua`; `consumable.lua:171-176`) | collapse, `death_threshold` kill (`unit_resource_tick.lua:368-395`) | `salts.hydrationFrac`, AI drink utilities, status/debug panels, `Healing.hs` |
| salt | `salt`, `salt_conc` (`salts.lua:63-77`; thermo sweat `thermo.lua:211-214`; meal `unit_ai_needs.lua:279`) | `salt_imbalance` meter kill (`unit_resource_failure.lua`) | `speedMultiplier` → `movement_speed.lua:94`; `sweatFactor` → thermo; brain consciousness; status row |
| thermo | `core_temp`; writes `circulation` (`thermo.lua:110-113`) | sweat debits to hydration and salt; shiver stamina debit (`:201-205`); frostbite (`:217-226`); `hypothermia`/`hyperthermia` meters | cardio, brain, status row, `Wounds/Tick.hs` fever suppression |
| nutrition | `hunger`, `calories` | digestion; catabolism and the **lean-floor starvation kill** (`unit_resource_energy.lua:223-256`); surplus regrowth; the **fat-floor organ-failure drain** inside stamina (`unit_resource_tick.lua:187-200,259,292`); strength discount (`starvation.refreshStrength`); meal salt via eating | thermo `calFactor`, AI eat, `Healing.hs`, brain mood |

Tick order (`scripts/unit_resources.lua:129-199`): cardio → (exempt ?
neutralize : thermo, salts) → brain → thoughts → mental → injuries and
failure meters → per-def pools → digestion and starvation → strength →
revive. Pools, digestion and starvation run only for definitions with a
config entry.

### Species today

| Species | Body | Pools configured | thermo/salt run? | Salt input in normal play |
|---|---|---|---|---|
| acolyte | yes | stamina, hydration, hunger, calories, exhaustion, sleep_pressure | yes | meals (`eat_from_inventory`, acolytes only, `scripts/unit_ai.lua:185-195`) |
| bear_brown | yes | stamina, exhaustion, sleep_pressure | yes | none (`scripts/bear_ai.lua:295-317`) |
| red_squirrel | yes (≈0.3 kg) | stamina, exhaustion, sleep_pressure | yes | none (`scripts/red_squirrel_ai.lua:222-236`) |
| technomule | yes | none | yes | none |
| nomad_primitive | yes | none | no: pinned by #2754 | n/a |
| tiller, white_tailed_deer, unknown_unit | **no** | none | yes, if spawned, at a fictional 70 kg | none |

"None" means no normal gameplay replenishment. A debug or script call can
still add salt.

## Intended simplifications vs defects

A behavior is a **simplification** if it is what Nominal should mean for a
system that is not simulated. It is a **defect** if a Simulated system
produces wrong results, or if a non-simulated system leaks into one. All
findings come from reading source.

| # | Finding | Class | Evidence |
|---|---|---|---|
| F-1 | `salts.hydrationFrac` returns 1 with no live hydration pool, so `salt_conc = salt/maxSalt` | **Intended simplification**: Nominal hydration's substitute for a Simulated salt system | `scripts/salts.lua:49-54` |
| F-2 | Sweat costs are absolute: `SWEAT_HYDRATION_K = 0.05`/s and `SWEAT_SALT_K = 0.10`/s per unit of sweat, against capacities `0.6·body_mass` and `1.0·body_mass`. Per unit of sweat, salt drains 0.142857 %/s of capacity at 70 kg and 33.3333 %/s at 0.3 kg. Hydration drains 0.119048 %/s at 70 kg and 27.7778 %/s at 0.3 kg. Both ratios are 70/0.3 = 233.333×. The baseline terms were made proportional (`salts.lua:25-30`; hydration's `drain_constant_frac`); the sweat terms never were | **Defect** (mass scaling), verified at source level. **Fixed by #2825, which this design assumes merged.** The salt half is live for squirrels today. The hydration half is latent until squirrel hydration is simulated (for example by a scripted override). #2825's own probe diagnostics support F-3: the one squirrel whose core crossed 37.5 °C (37.20 → 37.62 °C over about 31 s) fell from salt_conc 1.00 to 0.42, while the squirrels below 37.5 °C lost almost nothing | `scripts/thermo.lua:68-69,206-214`; `Body.hs:171`; #2825 |
| F-3 | When squirrels sweat. A squirrel at mean rolls (constitution 0.5, fat fraction 0.10, `red_squirrel.yaml:26,31,53`) has fitness `(0.5 + 0.5·0.5) = 0.75` with no fat penalty, so circulation ≈ 0.75 (`circulation.lua:38-61`). `circLoss = 0.5 + 0.5·0.75 = 0.875` (`thermo.lua:180-181`), insulation is 1.2, and the loss coefficient at 22 °C is `0.1/1.2·0.875 ≈ 0.0729`/°C. **Idle, metabolism 1.2:** production 1.08 gives an equilibrium of ≈36.8 °C, inside the 36.5–37.5 band, so it neither shivers nor sweats. **Idle sweating** needs production > 1.130 at 37.5 °C, i.e. metabolism > ≈1.256 at mean fat and constitution. That is about a quarter of rolls if metabolism is normal with σ = range/4 (the YAML's stated convention, `red_squirrel.yaml:21`). Lower constitution or higher fat lowers the threshold. At metabolism 1.3 idle: core ≈37.55 °C, sweat ≈0.04, salt ≈1.25 %/s of `maxSalt`. Ignoring the kidney term and `sweatFactor`, `salt_conc` falls below 0.7 in under 30 s. **Moving:** production 1.68 gives core ≈38.2 °C, sweat ≈0.5, salt ≈17 %/s | **Not a defect in itself.** It shows F-2 can produce the probe's failure. The probe neutralizes `unitAi.update` (`physiology_probe.py:50-53`), which also stops `red_squirrel_ai` (dispatched through `unitAi`, `scripts/unit_ai.lua:246`), so probe squirrels are idle. A failing run is therefore consistent with a high-metabolism or low-circulation roll, not with movement. This is a source-level account; no particular run was reconstructed | arithmetic in this row; `thermo.lua:143-195` |
| F-4 | Salt is simulated for species with no salt input. Baseline excretion is `0.004/70` of capacity per second (`salts.lua:30,67`). Starting full, with no sweat and hydration absent, `salt_conc` reaches 0.75 after 4,375 s, ≈3.04 game days at `DAY_SECONDS` 1440 (`scripts/unit_stats.lua:37`). Sweat makes it sooner | **Defect**: a Simulated system with no supplier for one of its inputs. Predicted, not measured | above |
| F-5 | Bodyless definitions run thermo and salts on 70 kg fallbacks | **Simplification placeholder.** Becomes Unsupported at activation (SD-13) | `data/units/tiller.yaml` header |
| F-6 | `consumable.drink` credits hydration when `max_hydration` exists, using `getStat(...) or 0`, which can create a pool no system drains | **Latent defect** (write promotes absent to present). Whether play can reach it was not checked | `scripts/consumable.lua:171-176` |
| F-7 | #2754 pins healthy values by writing them every tick | **Simplification to subsume** as profile `exempt` | `scripts/unit_survival_exempt.lua` |
| F-8 | Haskell consumers read an absent pool as healthy, and read base values, not effective ones | **Consistent today** under the storage invariant, and because no modifiers target these keys. The contract must define it (D-2) | `Healing.hs:148-161`, `Wounds/Tick.hs:214` |
| F-9 | `physiology_probe.py`'s aggregate reads `unit.getStat(u,s) or 0`, so an absent Nominal `salt_conc` would read 0 | **Migration hazard** (S5/S7) | `tools/physiology_probe.py:57-71` |
| F-10 | Squirrels and bears register no drink or eat behavior, so a need enabled by override has no satisfier | **Implementation gap**, outside this design (owner, 2026-10-13) | `scripts/red_squirrel_ai.lua:222-236`, `scripts/bear_ai.lua:295-317` |

**Acceptance is not weakened.** The small-creature temperate bounds stay
(`0.7 ≤ salt_conc ≤ 1.3`, core 33.5–39 °C, no deaths). The regression is a
deterministic, parameterized case (T-1) that pins mass, constitution, fat,
metabolism, climate and activity. It does not depend on a random roll. If the
owner makes squirrel salt Nominal (SD-10), the probe asserts Nominal semantics
for squirrels, and the salt-only small-mass case under heat keeps the same
bounds in T-1.

## Goals and non-goals

Goals:

- Every v1 survival system has an explicit, queryable mode per unit.
- Nominal means healthy and not simulated, and it is distinct from
  Unsupported.
- Species defaults choose a fidelity tier. Scripts override per unit, so
  later gameplay (for example capture) can turn on any system for one
  unit.
- Every combination of modes is coherent. A Simulated system whose
  dependencies are Nominal stays healthy at healthy conditions.
- Modes, intent and values round-trip through save and load. Legacy saves
  normalize explicitly and idempotently.

Non-goals:

- In v1: modes for stamina, exhaustion, sleep pressure, cardio, circulation,
  blood or the injury meters. They are later slices of this same epic
  (SD-12), and the machinery is built to be reused for them (D-11).
- Capture, taming and captivity. They are out of scope, and a long way from
  implementation. The per-unit override API is everything they will need
  from this design. Recorded owner direction for that future work:
  capture will turn on hydration, nutrition, salt and other systems.
- Species need behaviors. Squirrels and bears register no drink or eat
  action (`scripts/red_squirrel_ai.lua:222-236`,
  `scripts/bear_ai.lua:295-317`). That is an implementation gap (F-10), not
  a mode decision.
- Retuning any physiology band. F-2's mass scaling belongs to #2825.

## Design decisions

### D-1. Systems and their interfaces

The registry is a content file, `data/physiology/systems.yaml`, loaded by
Haskell and exposed to Lua. Each system declares:

- `id`.
- `owns`: stored state keys, each with a **healthy rule**
  (`{ const: 37.0 }`, `{ from: max_hydration }`,
  `{ from: body_mass, scale: 1.0 }`).
- `derived`: cached keys the system computes, each with a healthy value
  (`salt_conc: 1.0`).
- `meters`: failure meters it owns.
- `requires`: species capabilities (`body` for all four in v1).
- `provides`: values other systems read.
- `contributes`: writes or events it makes into other systems.
- `effects`: deaths, collapses and penalties it performs.
- For every `provides`, `contributes` and `effects` entry, a **Nominal
  substitute**: what dependents see or receive while this system is Nominal.

| System | Owns (healthy) | Derived | Meters | Provides → Nominal substitute | Contributes → Nominal substitute | Effects (none while Nominal) |
|---|---|---|---|---|---|---|
| hydration | `hydration` (= effective `max_hydration`) | — | — | body-water fraction → 1.0 | — | collapse, `death_threshold` kill, thirst need |
| salt | `salt` (= `body_mass·SALT_PER_KG`), `salt_store` (#2761, assumed merged; = full) | `salt_conc` (1.0) | `salt_imbalance` | `salt_conc` → 1.0; `sweatFactor` → 1.0 | — | cramps, consciousness factor, meter kill |
| thermo | `core_temp` (37.0) | — | `hypothermia`, `hyperthermia` | `core_temp` → 37.0 | sweat debit to hydration and salt → none; shiver debit to stamina → none | frostbite, meter kills |
| nutrition | `hunger` (= `max_hunger`), `calories` (= `max_calories`) | — | — | `calFactor` → 1.0; strength discount → none | meal salt credit → **virtual meals** (settled SD-4); tissue maintenance → **recovery toward the healthy target** (settled SD-5) | digestion, catabolism, regrowth, lean-floor kill, fat-floor organ-failure drain in stamina, eat need |

Incoming contributions follow the receiver's mode. A debit or credit into a
Nominal system is discarded: `physiology.debit` and `credit` return 0. A
Simulated receiver applies it.

`circulation` is not owned by any system. It becomes its own always-on
orchestration step, after cardio and before thermo, computed from the
*effective* core read. That reproduces where thermo computes it today
(`thermo.lua:110-113`: before the core update) and what #2754 does for
nomads.

Constants that a healthy rule needs (`SALT_PER_KG`, `thermo.BASELINE`) live in
the registry, and the Lua modules read them from there.

### D-2. The read contract

`readPhysio(unit, key)` returns:

- `Unsupported`: the key is owned by a system the species cannot have. Lua
  gets `nil, nil`.
- `Nominal v`: the healthy rule, evaluated against **effective** inputs (for
  example effective `max_hydration`). Modifiers on the owned key itself are
  retained in `uiModifiers` but **not applied** while Nominal (SD-8).
- `Simulated v`: the **effective** value, stored base plus that key's active
  modifiers, through the same `effectiveStat` pipeline `getStat` uses today.
  This preserves current gameplay. `readPhysioBase` returns the base. A
  missing owned value under Simulated is an invariant violation and raises an
  error.
- Derived keys under Simulated return the cache, which D-5a guarantees is
  fresh before any observation. Meters of a Nominal system read 0.

Haskell gets one pure `physioRead` with the same semantics. `Healing.hs` and
`Wounds/Tick.hs` move to it (S5). Their switch from base to effective reads
is a behavior change only when a modifier targets these keys, and none does
today.

Every cross-system read goes through this accessor, and every cross-system
write goes through `debit` / `credit` (D-7). Together those two seams are the
whole dependency interface.

### D-3. Representation and override intent

- `uiPhysioOverrides ∷ HM.HashMap Text PhysioMode`, with
  `data PhysioMode = PhysioNominal | PhysioSimulated` (append-only
  `Generic Serialize` enum). The keys are system ids. **The presence of an
  override records explicit intent, independent of the effective mode.**
- `udPhysiology`: per-definition defaults from a `physiology:` YAML block,
  with optional `profile:` (D-10).
- Effective mode: Unsupported if `requires` fails; else the override if one
  is present; else the species default; else Nominal.
- Storage invariant: under Nominal, the system's owned keys, derived keys and
  meters are absent from `uiStats`. Under Simulated, its owned keys are
  present. One pure Haskell transition function maintains this; spawn,
  setters and load normalization all call it.
- Definition loading **rejects** any registry-owned, derived or meter name in
  a definition's `stats:` or `skills:` templates. `getOrRollStat` and
  `getAllStats` therefore cannot recreate a Nominal key. No shipped definition
  uses these names (checked by grep). `getAllStats` keeps its raw meaning.
  The facade `physiology.getAll` adds the mode-aware entries for display.

Alternatives considered and rejected: a single cell holding mode and value
per key (it can represent incoherent per-key modes and breaks every stat
path), reserved float keys (a type smuggled into floats), a Lua save module
(Haskell consumers and spawn need the mode), and divergence-only storage (it
loses pinning, see D-5). Defaults are live (SD-14).

### D-4. Dependency substitutes and the four salt × hydration combinations

A Nominal system supplies exactly its declared substitutes (D-1) and nothing
else. The salt input gap (F-4): with nutrition Nominal there are no meals.
**Virtual meals** substitute for them (settled SD-4). The salt tick reads
nutrition's mode. When nutrition is Nominal and the salt pool fraction
(`salt / maxSalt`) drops below `VIRTUAL_MEAL_THRESHOLD = 0.85`, the tick
credits one real meal's worth, `MEAL_SALT_FRAC` (0.30 of `maxSalt`), capped
at full, by the same path a real meal uses (`salts.mealSalt`).

- The threshold sits just above the 0.80 cramp onset (`CRAMP_START`), so
  a healthy unit never cramps.
- Baseline excretion, kidney excretion and sweat debits continue unchanged.
  The unit behaves as if it ate regularly, and its salt can sit anywhere in
  the normal band, not pinned at full.
- A virtual meal deposits into the #2761 salt reserve exactly as a real
  meal does. The trigger counts salt plus reserve against `maxSalt`, so a
  full reserve is not topped up every tick.
- Salt fraction differs from `salt_conc` only when hydration is Simulated.
  The trigger reads the pool fraction because a meal is a pool input.

In the expected use, salt and nutrition are switched Nominal together and
Simulated together (SD-4). The salt-Simulated, nutrition-Nominal combination is
an edge case, handled so that it stays healthy.

The four combinations, with thermo Simulated:

| Salt | Hydration | Behavior | Shipped by (SD-10) |
|---|---|---|---|
| Simulated | Simulated | `conc = saltFrac/hydrationFrac`; sweat debits both pools, each in proportion to its own capacity (#2825); water dilutes; real meals, or virtual meals when nutrition is Nominal, restore | `rich` (acolyte) |
| Simulated | Nominal | body-water fraction 1 (F-1); sweat debits salt only; virtual meals restore when nutrition is Nominal (SD-4) | `reduced` (SD-10) |
| Nominal | Simulated | `salt_conc` 1.0; sweat debits hydration; dehydration kills through hydration's own `death_threshold`; no hypernatremia, which belongs to salt | per-unit override (no shipped profile) |
| Nominal | Nominal | no pools; sweat still cools but its debits are discarded | `simple`, `exempt` |

There is one feedback loop: salt ↔ thermo. Depleted salt reduces sweating
(`sweatFactor`), which raises the core, which drives more sweat. It is covered
by T-4. With thermo Nominal there is no sweat or shiver at all.

### D-5. Transitions and override semantics

Verbs (D-7): `setPhysioMode(uid, system, mode [, values])`,
`clearPhysioMode(uid, system)`. Each is one atomic unit-manager update (the
`unit.commitStamina` discipline, #2470).

Override semantics:

- **Set** always records the override, even when it equals the current
  species default. The unit is then pinned against later default changes.
- **Clear** always removes the override, even when the effective mode does
  not change, so future defaults reach the unit again.
- Seeding or scrubbing, and the `onModeChanged` hooks, run only when the
  **effective** mode changes.

| Case | Override after | Effective change? | Seed/scrub, hooks |
|---|---|---|---|
| default Simulated, set Simulated | Simulated (pinned) | no | none |
| default Simulated, set Nominal | Nominal | yes | scrub, hooks |
| override Nominal, clear (default Simulated) | none | yes | seed, hooks |
| override Simulated, clear (default Simulated) | none | no | none; the unit now follows the default |
| pinned Simulated, content changes default to Nominal, save/load | Simulated | no | none |
| no override, content changes default to Nominal, save/load | none | yes, at load normalization (D-6) | scrub, logged |

Effective transitions:

- **Nominal → Simulated:** seed the owned keys from `values` or the healthy
  rules (danger-neutral seeding, SD-9). Meters start at 0.
- **Simulated → Nominal:** remove the owned keys, derived keys and meters. Any
  collapse this system caused clears through ordinary `checkRevive`.
- **Values without a mode change:** the system must be Simulated. Each key
  must be an owned key of that system; the value must be finite and ≥ 0, and
  ≤ its effective max where one exists. Otherwise the whole call is refused.
  A successful value write resets the `unit_resource_carry` remainder and the
  alert debounce for those keys. No `onModeChanged` hook runs.
- **Refused:** dead units (a corpse ticks nothing, `unit_resources.lua:129`),
  Unsupported systems, unknown ids, and Simulated without resource parameters
  (D-9).

`onModeChanged(uid, old, new)` hooks: clear carry remainders and alert
debounce. For nutrition, clear regrowth authorship. Cancel an in-progress
need sequence: `drink_from_source` returns `math.huge` while its latch is
active, before it checks thirst (`unit_ai_water.lua:224-225`).

Spawn: resolve the modes after the body-derived maxima are written, seed the
owned keys and derived values of Simulated systems from the healthy rules
(coherent: full salt with full hydration gives conc 1.0), and leave Nominal
keys absent. This replaces the nil-on-first-tick fill for mode-governed
pools. Stamina's commit-time init (#2470) is unaffected.

### D-5a. Derived state

Derived keys are a cache. `physiology.refresh(uid)` recomputes, from one
coherent read of the unit:

1. circulation, at the effective core;
2. each Simulated system's derived keys, in registry order (`salt_conc`
   from effective `salt`, `maxSalt` and the body-water fraction);
3. brain at `dt = 0`, the #2754 precedent (`unit_survival_exempt.lua:68-75`).
   It recomputes consciousness, concentration and state of mind without
   drifting mood or caffeine.

It runs synchronously:

- at the end of every facade transition or value write, before the call
  returns;
- in `unit_resources.onSaveLoaded` after `recomputeBody` and normalization,
  before the first paused frame (`unit_resources.lua:223-230`), replacing
  `neutralizeLoaded`;
- after spawn seeding, at the first observation.

**Invalidation.** A transition of system D refreshes every dependent of D in
the registry graph, so hydration changing while salt stays Simulated
recomputes `salt_conc`. `recomputeBody` during play can change `maxSalt`, and
the conc then lags by at most one resource tick. That is today's behavior and
is not observable while paused, because the load path refreshes. Only the
Lua facade calls the raw engine verbs. The access audit enforces this, so no
transition skips the refresh.

### D-6. Persistence and migration

Classification follows `docs/persistence_contract.md` §2.
`uiPhysioOverrides` is *Persist exactly*. `udPhysiology`, the registry and
the profiles are *Rebuild deterministically*. Simulated values stay in
`uiStats` (*Persist exactly*). Rows go into
`docs/persistence_state_inventory.md`, and the `umInstances` / `UnitsDTO`
narratives gain sentences.

**Wire format.** `units` goes from v3 to v4.

- Freeze the **whole outgoing v3 graph**: `UnitsDTOv3`, `PageUnitsDTOv3`,
  `UnitInstanceDTOv3`, and any nested type whose v4 shape differs. The
  nested shapes v4 leaves unchanged are referenced as-is; the enum audit
  guards their tags.
- Today v1 and v2 each migrate **straight to the current DTO**
  (`EntitySnapshots.hs:262-265,740-743`), and the dispatcher picks one
  decoder per version (`Component/Types.hs:476-478`). Make the dispatch
  explicit: `csOlderVersions = [atVersion 1 (migrateUnitsDTOv3 .
  migrateUnitsDTOv1), atVersion 2 (migrateUnitsDTOv3 . migrateUnitsDTOv2),
  atVersion 3 migrateUnitsDTOv3]`, with the v1/v2 migrations retargeted to
  produce the frozen v3. Each historical version is converted exactly once
  per step.
- Map the field through the live instance, the snapshot
  (`World.Save.Types`, alongside `uisStats` `:759,829,980`), the wire DTO
  and restore. Every old-version path, including the B1
  `World.Save.Compat.SessionV90` adapter that builds snapshots, yields
  empty overrides.
- Keep the historical fixtures and add a v3 fixture. `PhysioMode` enters the
  enum baseline as a pure append. Gates: `--match "persistence contract"`,
  the save-compat reproducibility check selected by
  `tools/ci_expensive_gates.py --gate save-compat`
  (`tools/test_save_compat_audit.py --only-reproducibility`),
  `persistence_inventory_audit.py` with its self-test, and
  `enum_append_only_audit.py`.

**Load policy.** The safe repairs are enumerated, and everything else
rejects. This replaces the revision-1 reasoning from the immunity scrub,
which applies to memories, not active intent (contract §4,
`docs/persistence_contract.md:263-290`).

| Case | Disposition |
|---|---|
| (a) Legacy v1–v3 payload | Empty overrides, then (b) |
| (b) Known system whose stored keys disagree with the effective mode (legacy save, or an intentional profile change) | **Normalize**: scrub or seed through the D-3 transition function, logged per unit and system. Non-blocking: the system and its healthy rule are known |
| (c) Override naming a system the registry declares `retired:` or `renamed_from:` | Apply the declared migration: remap a rename, or for a retirement drop the override and give orphaned owned keys the declared disposition (drop by default, or keep as plain stats). Logged |
| (d) Override naming an undeclared unknown system | **Reject the load**: an active gameplay reference to a missing definition, contract §4 |
| (e) Unknown `PhysioMode` tag | Decode failure: malformed |
| (f) Override Simulated for a system the unit's species no longer supports | **Reject** by default (active intent would be dropped). Settled SD-6 |
| (g) Seeding needs a healthy-rule input that is missing, zero or non-finite | **Reject**, naming the unit, key and input. Content-load validation ensures every rule's inputs are guaranteed for species meeting `requires`, so this arises only from corrupt saves |
| (h) A stored Simulated value that is non-finite | Reject: malformed. Finite out-of-range values are retained exactly, adding no validation beyond today's |

Exact round trip versus normalization: a session written by current code
loads with **zero** normalization actions and re-saves to identical
component bytes. A legacy session normalizes once. Saving it and loading
again performs **zero** further actions (idempotence). Normalization is
deterministic because the healthy rules are pure functions of stored stats.

No Lua save module is added. The `unit_resources` reset hook and teardown
registration also clear any hook caches.

### D-7. Script API

Engine verbs, which only the facade may call:

| Verb | Returns |
|---|---|
| `unit.getPhysioMode(uid, system)` | `"nominal"` / `"simulated"` / `nil` |
| `unit.getPhysioOverride(uid, system)` | override, or `nil` |
| `unit.getPhysioDefault(uid, system)` | species default |
| `unit.setPhysioMode(uid, system, mode [, values])` | `true` or `nil, reason` |
| `unit.clearPhysioMode(uid, system)` | `true` or `nil, reason` |
| `unit.readPhysio(uid, key)` / `unit.readPhysioBase(uid, key)` | `value, mode`, or `nil, nil` |

The write gate: `unit.setStat` on an owned, derived or **meter** key of a
**Nominal** system is refused (`false`). This closes F-6 and forces probes to
enable a system before forcing its pool (SD-16). Under Simulated, raw
`setStat` is still allowed for debugging. `unit.addModifier` on a Nominal
owned key is accepted and stored, but has no effect until the system is
Simulated (SD-8).

Facade `scripts/physiology.lua`:

- `get(uid, key)` → `value, mode` (effective)
- `getBase(uid, key)`
- `fraction(uid, key, maxKey)` → 1.0 under Nominal
- `getAll(uid)`
- `isSimulated(uid, system)`
- `debit(uid, key, amount)` / `credit(uid, key, amount)` → amount applied, 0
  under Nominal
- `setMode`, `clearMode` (wrapping the verbs, then hooks, then `refresh`)
- `refresh(uid)`
- `registerSystem(id, { tick, onModeChanged })`

Orchestration: `unit_resources.update` runs cardio → circulation → each
Simulated system's `tick` in registry order (thermo before salt, as today) →
brain → … Pool entries, digestion, starvation, regrowth and strength refresh
run only when their owning system is Simulated (D-9). The
`survivalExempt` branch goes away.

Access audit (new, or extending an existing audit; it has a self-test): it
flags raw `getStat` / `setStat` / `or <default>` reads on registry keys
outside their owner, raw verb calls outside the facade, registry coverage
(every system has a Lua owner, every owned key a healthy rule, every
`contributes` entry a substitute), and templated physiology names.

### D-8. UI display

Settled by the request (SD-2): a Nominal row shows `Nominal` in the neutral
value color, with the tooltip "Not simulated for this unit: always healthy."
and the healthy reading where it is meaningful. Pools show `Nominal`, never
`x / x`. The debug panel prints `Hydration: Nominal`. Unsupported rows are
hidden. Conditions and meter rows of Nominal systems never appear. The panel
rebuild signature (`panelShapeSig`) includes modes.

### D-9. Consumers: AI, penalties, death

- **AI.** Drink, eat and forage utilities read `physiology.fraction`, which
  is 1.0 under Nominal, so the utility is −∞. With the system Simulated, a
  need acts only if the species registers a behavior that satisfies it.
  Species without one are an implementation gap (F-10), outside this
  design. `onModeChanged` cancels latched need sequences.
- **Penalties.** Cramps, `sweatFactor`, `calFactor`, the strength discount
  and consciousness come through the accessor. A Nominal system contributes
  exactly its substitute.
- **Death and nutrition effects (R2).** Nutrition owns, and gates on its own
  Simulated mode: `tickDigestion`; the whole of `tickStarvation`, including
  catabolism and the lean-floor respiratory kill
  (`unit_resource_energy.lua:248-252`); `applyRegrowth`;
  `starvation.refreshStrength`'s discount (Nominal uses undiscounted
  strength); and the **organ-failure contribution inside the stamina tick**
  (`unit_resource_tick.lua:187-200,259`). With nutrition Nominal,
  `inOrganFailure` is false. What stays live under Nominal nutrition: ordinary
  stamina regen and drain, stamina's `kill_on_zero` from any other cause, and
  the injury `organ` failure meter (`unit_resource_failure.lua`), which is
  wound-driven and a different mechanism.
- **Tissue under Nominal nutrition recovers toward healthy** (settled SD-5).
  Body-composition upkeep reads nutrition's mode, and Nominal means
  "sufficient nutrition":
  - There is no catabolism, so no tissue is lost.
  - Each tissue below its **healthy target** regrows at the ordinary surplus
    regrowth rate, with the same activity split as `applyRegrowth`. It stops
    at the target, so a Nominal unit never fattens without bound the way an
    always-surplus Simulated unit can.
  - Tissue above its target is kept as-is. Sufficient nutrition does not
    slim a unit.
  - The target is the **species-mean body fat applied to the unit's own
    `frame_mass`**, through the spawn formula (`Body.hs:91-125`):
    `fat = max(minFat·1.7, frame·f̄)`,
    `lean = max(minLean·1.07, frame·(1−f̄)·0.5)`, where f̄ is the
    `bodyfat` mean in `udBodyTemplates` (`src/Unit/Types/Def.hs:139-144`).
  - No new persisted state is needed. `frame_mass` is already stored, and the
    rolled `bodyfat` is dropped at spawn (`Body.hs:123-124`), so an unusually
    lean or fat individual recovers to the species norm, not to its own spawn
    shape. That is accepted.
  - Mass changes call `recomputeBody` as regrowth does today. Strength and
    carrying capacity therefore recover with the tissue, and fat insulation
    in thermo follows.
  - In the expected use, nutrition goes Nominal together with salt, and the
    switch normally happens on a healthy unit. Recovering a wasted unit is a
    handled edge case.
- **Pools and meters.** A pool's `death_threshold`, `kill_on_zero` and
  collapse fire only in a Simulated pool's tick. Meters carry a `system`
  field and do not tick under Nominal. Injury meters always run.
  `checkRevive` evaluates only Simulated pools.
- **Parameters.** Species entry first, then the system's default parameters
  in the registry. The acolyte's mass-proportional values can serve as those
  defaults.

### D-10. Species profiles

| Profile | hydration | salt | thermo | nutrition | Species (SD-10) |
|---|---|---|---|---|---|
| `rich` | Sim | Sim | Sim | Sim | acolyte |
| `reduced` | Nominal | Sim | Sim | Nominal | bear_brown, technomule |
| `simple` | Nominal | Nominal | Sim | Nominal | red_squirrel |
| `exempt` | Nominal | Nominal | Nominal | Nominal | nomad_primitive |
| none | Unsupported | Unsupported | Unsupported | Unsupported | bodyless defs |

**Activation profile set (S4).** For bodied species, S4 reproduces today's
modes: acolyte `rich`; bear, squirrel and technomule `reduced` (they already
lack hydration and nutrition keys, so Nominal scrubs nothing); nomad `exempt`
(the same outcome as #2754). S4 makes three **intentional** behavior
changes, listed in its PR:

1. Bodyless definitions become Unsupported (SD-13).
2. `reduced` species gain virtual meals (SD-4), which fixes F-4.

(Mass-proportional sweat costs arrive earlier, through #2825.)

S7 applies the owner's table (SD-10).

Capture is out of scope (Non-goals). When it is designed, it uses
`physiology.setMode` per unit. The override semantics (SD-7) and
persistence (D-6) already cover it.

### D-11. Extensibility

- **A new system** needs a registry entry with keys, healthy rules, meters,
  requires, **provides, contributes, effects and their Nominal substitutes**,
  plus a Lua owner, profile columns and status metadata. The coverage audit
  fails until all of them exist.
- **New modes** are appended to `PhysioMode`. The read contract already
  returns the mode, but new modes are **not** free:
  - `Pinned(value)` needs a persisted pinned value, and an owner for it that
    neither the tick nor the write gate may change.
  - A `Coarse` (reduced-rate) tick may need persisted timing state.
  - Each needs its own persistence classification and a component version
    bump.
- **Remaining systems** (SD-12) are tracked by this epic as later slices.
  They reuse the resolver, invariant, transition, refresh and persistence
  machinery. Each needs a design addendum (interface, healthy rules,
  substitutes, effects) before it is filed.
- **Species thermoregulation variants** (panting, fur) are parameters, not
  modes.
- **Scenario authoring** can accept `physiology:` overrides per spawned unit.

## Testing strategy

The aim is linear coverage: one case per system, per dependency edge, or per
contract clause, plus the four salt × hydration cases and one feedback case.
There is no exponential mode × scenario × species suite. The Lua cases use
the `Test.Headless.Unit.SurvivalExemption` harness pattern: shipped modules
over the real `unit.*` API, fixed climate, no world. All inputs are pinned
or enumerated, with no entropy-seeded rolls.

### Invariants

- **I1 Nominal neutrality** (per system, in arctic and desert climates). Reads
  equal the substitutes. Meters read 0. Contributions into the system are
  discarded. No effect from it fires. Its keys stay absent after N ticks.
- **I2 Interface equivalence** (per dependency edge between systems; six in
  v1). Run S Simulated with D Nominal, and again with D replaced by a test
  **stub** that, at each tick boundary, supplies D's declared substitute
  interface: healthy reads, discarded contributions into D, and D's
  substitute contributions such as virtual meals. The trajectories
  must match within 1e-6 over 600 ticks. This proves that Nominal supplies
  exactly its declared interface and leaks nothing else. **Limitations:**
  these are sampled trajectories, not a proof of equivalence. Each edge is
  checked against one scenario. Feedback loops are covered only by T-4.
  Interface completeness depends on the coverage audit.
- **I3 Healthy steady state** (per system, alone Simulated, everything else
  Nominal). Temperate climate, idle and sustained exertion, over 2 game-hours.
  Each Simulated system stays within its healthy band. For salt with
  nutrition Nominal, virtual meals must keep `salt_conc` inside
  [`CRAMP_START`, `HYPER_START`] = [0.80, 1.35] throughout, with at least one
  virtual meal firing in the window under exertion.
- **I4 Mass invariance of salt and hydration debits** is owned by #2825's
  own acceptance, not by this epic. This epic only requires that the mode
  gating does not reintroduce absolute debits: `debit` is applied as a
  fraction of the receiver's capacity, and T-1 runs at 0.3 kg.
- **I5 Transition contract**, with an Haskell pure part: seed, scrub,
  refusals, override set/clear semantics (D-5 table), values validation,
  revive after Simulated → Nominal, and cancellation of a need latch.
- **I6 Persistence**: exact canonical round trip with zero normalization;
  legacy normalization is idempotent; and the D-6 table, as a pure
  table-driven spec over (a) to (h).

### Representative contract cases

- **T-1 Small-creature salt (4 combinations).** Mass 0.3 kg, constitution
  0.5, fat 0.10, at 22 °C and humidity 0.5, parameterized over (idle,
  metabolism 1.2), (idle, metabolism 1.3), and (forced moving, metabolism
  1.2), each in the four D-4 combinations, over 120 game-seconds. Asserts the
  probe's own bounds, no NaN, and Nominal keys healthy. The metabolism-1.3
  idle case is the deterministic stand-in for the probe's random rolls.
- **T-2 Acolyte reference heat.** Both systems Simulated at 70 kg in #2761's
  climate (39.6 °C, humidity 0.26), supplied. Guards #2825's rescale through
  the mode machinery.
  Coordinated with the "salt reserve (#2761)" group, not duplicating it.
- **T-3 `exempt`.** Reproduces #2754's outcomes: no survival delirium or
  death, injury meters live.
- **T-4 Feedback.** Salt and thermo Simulated, salt pre-depleted to conc
  0.7, desert heat. `sweatFactor` falls and the core rises faster than at
  conc 1.0. No NaN or oscillation beyond the clamps.
- **T-5 Per-unit override.** A squirrel switched to hydration Simulated by
  script uses the system's default parameters. It dehydrates over game-days,
  not seconds, and dies by `death_threshold`. Given a scripted water credit,
  it recovers. Autonomous drinking is not asserted (F-10).
- **T-6 Modifiers.** A modifier on `hydration` changes the Simulated read and
  has no effect under Nominal. A modifier on `max_hydration` moves the Nominal
  healthy value.
- **T-7 No write-through.** With hydration and salt Nominal, `getAllStats`,
  `getStat`, `getStatBase` and a failed `setStat` leave the owned, derived and
  meter keys absent. A definition templating an owned name fails to load.
- **T-8 Derived freshness.** Immediately after salt Nominal → Simulated, after
  hydration Nominal → Simulated with salt Simulated, and after a paused load,
  `salt_conc`, circulation and consciousness read coherent values before any
  tick.
- **T-9 Nutrition floors.** An acolyte at the fat floor, and one at the lean
  floor, are switched to nutrition Nominal.
  - Neither suffers organ-failure drain nor a starvation death over 1
    game-day.
  - Both tissues rise monotonically toward the species-mean target and stop
    at it.
  - A unit above target keeps its mass.
  - A wound-driven `organ` meter and a stamina drain from exertion still kill
    when driven.
- **T-12 Virtual meals.** Salt Simulated with nutrition Nominal at squirrel
  mass, idle and under exertion heat. A virtual meal fires each time the
  salt fraction crosses below 0.85 and never above it. `salt_conc` stays
  within [0.80, 1.35]. With nutrition Simulated, no virtual meal ever fires.
- **T-10 Override semantics.** Set-equal-default, clear-equal-default,
  changed-default with and without a pin, each across save and load (D-5
  table).
- **T-11 Load policy.** One case each for (b), (c), (d), (f) and (g).

### Static and probe

- The access audit with its self-test (D-7).
- `tools/physiology_probe.py` reads through `unit.readPhysio` (F-9). Its
  bounds are unchanged. F-2's primary gate is #2825's; T-1 covers it under
  the modes.
- Validation follows the root CLAUDE.md tiers: targeted `--match` groups,
  the D-6 persistence gates, and the matching self-tests. No full suites.

## Open decisions for the owner

None. Revision 3 settled every decision with the owner (2026-10-13). IDs
are stable, and each settled entry below names the decision it replaced.

**Settled:**

- **SD-1 (was O-5), by the request.** Nominal thermo reads a flat 37.0 °C. A
  fever-adjusted Nominal value would not be "healthy semantics". Consequently,
  fever's infection suppression (`Wounds/Tick.hs`) does not apply while
  thermo is Nominal. That is a recorded consequence, not a choice.
- **SD-2 (was O-7), by the request.** Nominal is displayed as a visible
  `Nominal` row (D-8). Hiding Nominal rows would not be a display of Nominal.
- **SD-3 (was O-13), owner 2026-10-13.** The sweat mass-scaling fix is
  [#2825](https://github.com/coghex/synarchy/issues/2825), already in the
  workflow pipeline. This design assumes it is implemented, and the doc is
  not processed until #2825 merges. S1 is removed from this epic.
- **SD-4 (was O-4), owner 2026-10-13. Virtual meals.** When nutrition is
  Nominal and salt is Simulated, the salt tick checks nutrition's mode. Below
  a salt fraction of 0.85 it credits one real meal's worth
  (`MEAL_SALT_FRAC` 0.30, capped at full), by the real meal path. Baseline
  excretion and sweat continue, so salt moves anywhere in the normal range,
  "as if getting regular meals". Details are in D-4. The expected use is
  that salt and nutrition switch modes together. This combination is a
  handled edge case.
- **SD-5 (was O-14), owner 2026-10-13. Slow recovery toward healthy.** Under
  Nominal nutrition, fat and muscle upkeep treats the unit as sufficiently
  nourished. There is no catabolism and no starvation death. Each tissue
  regrows at the ordinary regrowth rate up to the target, the species-mean
  body fat on the unit's own `frame_mass`, and nothing is added beyond it.
  Details are in D-9. Also an edge case: in expected use, both systems
  switch together.
- **SD-6 (was O-8), owner 2026-10-13.** Reject unless a migration is
  declared: the D-6 load-policy table as written. An undeclared unknown
  system, or a Simulated override for a system the species no longer
  supports, rejects the load. Removals and renames require `retired:` /
  `renamed_from:` registry entries with an orphan-key disposition.
  Known-system mismatches are normalized and logged.
- **SD-7 (was O-15), owner 2026-10-13. Set pins.** A set always records the
  override, even when it equals the species default. Clear always removes
  it. Seed, scrub and hooks run only on an effective change (D-5 table).
- **SD-8 (was O-16), owner 2026-10-13.** Modifiers on a Nominal system's owned
  keys are kept but ignored, and apply again when the system is Simulated.
  Modifiers on maxima still shift the healthy Nominal value (D-2, D-7).
- **SD-9 (was O-17), owner 2026-10-13. Danger-neutral seeding.** Enabling a
  system seeds it at the value that reads healthy given the current state of
  its dependencies. For salt: `salt = maxSalt × hydrationFrac`, so
  `salt_conc` starts at 1.0. A fidelity switch never creates new danger by
  itself (D-5).
- **SD-10 (was O-2), owner 2026-10-13. Profiles:** acolyte `rich`;
  bear_brown and technomule `reduced`; red_squirrel `simple`; nomad_primitive
  `exempt` (D-10). With squirrel salt Nominal, the probe's small-creature
  salt check asserts Nominal semantics for squirrels (S7). The salt-only
  combination at squirrel mass, under exertion heat, keeps the probe's bounds
  in T-1 and T-12.

- **SD-11 (was O-6), owner 2026-10-13. Capture is out of scope.** Captivity
  is far from implementation. Missing drink and eat behavior for squirrels
  and bears is an implementation gap (F-10), not a design decision here.
  Recorded direction for future capture: it will enable hydration,
  nutrition, salt and others through the per-unit override API.
- **SD-12 (was O-1), owner 2026-10-13.** The v1 scope of four systems is
  accepted, but the epic tracks the work until **every** physiology system
  has a mode. Slices S8 to S11 cover stamina, exhaustion and sleep; cardio
  and circulation; blood; brain and mental state. Slice S12 gates closure on
  the owner's survey of the remaining unit-specific data. That survey may
  find further systems or stats.
- **SD-13 (was O-11), owner 2026-10-13.** Bodyless definitions are
  Unsupported. This is an intentional change listed in S4's PR.
- **SD-14 (was O-3), owner 2026-10-13. Live defaults.** Units without a
  pinned override follow content changes on load, normalized and logged.
  Pinned units (SD-7) keep their mode.
- **SD-15 (was O-12), owner 2026-10-13.** #2761 is a precondition, like
  #2825. `salt_store` is an owned salt key, its healthy value is full, and
  virtual meals deposit into it.
- **SD-16 (was O-10), owner 2026-10-13. The engine refuses** raw `setStat`
  on a Nominal system's owned, derived or meter keys.
- **SD-17 (was O-9), owner 2026-10-13.** Species defaults are authored in a
  `physiology:` block in `data/units/*.yaml`. Profiles and systems live in
  `data/physiology/systems.yaml`. Haskell reads both at spawn and load, and
  definition loading validates them.

No open decisions remain. Implementation-time checks: #2761's merged
constants against SD-4 and D-1, and the S8 to S11 design addenda.

## Delivery plan (dependency-ordered)

Each activated intermediate state must satisfy its own advertised invariants
and round-trip contract.

0. **Precondition: #2825** (mass-proportional sweat debits) is merged. It is
   not a slice of this epic.
1. **S2.** Registry, `udPhysiology`, profiles, the pure resolver, read verbs,
   and template rejection. **Inactive:** no tick, consumer or save path uses
   it. Advertised invariants: resolver specs only.
2. **S3.** `uiPhysioOverrides` through the live instance, snapshot, wire and
   restore; units v4 with the frozen v3 graph and explicit dispatch;
   fixtures; decode-time rejections (d), (e) and (h). No setter exists, so
   overrides are always empty. Advertised: I6's exact round trip and the
   migrations. Load policy settled (SD-6).
3. **S4. Activation.** Tick gating; the write gate; transitions and setters;
   `refresh`; nutrition effect gating; the dependency contributions,
   including virtual meals (SD-4) and tissue recovery (SD-5); load
   normalization (b), (c), (f) and (g); and the activation profile set with
   its two intentional changes (D-10). Advertised: I1 to I3, I5, T-1 to T-4
   and T-6 to T-12. Needs S2, S3, SD-7 to SD-9.
4. **S5.** Consumers migrate to the accessor (status, debug, AI, brain,
   Haskell `physioRead`), plus the access audit and the probe read (F-9).
   S4's state stays coherent before S5 because unmigrated readers' fallbacks
   are healthy and Haskell reads absent as healthy. The exception is the
   probe's `or 0`, which reads no Nominal keys under the activation profile
   set.
5. **S6.** UI display of Nominal, with `--offscreen` evidence in
   `docs/pr-proofs/`.
6. **S7.** The owner's profile table (SD-10), and the probe's switch to Nominal
   assertions where squirrel modes change.
7. **S8 to S11. The remaining systems** (SD-12), one slice each, after S4:
   stamina, exhaustion and sleep pressure; cardio and circulation; blood
   volume and recovery; brain and mental state. Each starts with a design
   addendum to this doc covering its interface, healthy rules, substitutes
   and effects. Stamina's addendum must handle the combat thread's
   concurrent spend (#2470).
8. **S12. Owner survey** of the remaining unit-specific data, which may add
   systems or stats. The epic closes only after the survey, with every
   system it lists given a mode.

## Source notes

Read at `91f6a9d7a` (primary checkout, read-only): `scripts/salts.lua`,
`thermo.lua`, `circulation.lua`, `cardio.lua`, `brain.lua`,
`unit_resources.lua`, `unit_resource_tick.lua`, `unit_resource_config.lua`,
`unit_resource_energy.lua`, `unit_resource_failure.lua`,
`unit_survival_exempt.lua`, `unit_stats.lua`, `unit_ai_needs.lua`,
`unit_ai_water.lua`, `consumable.lua`, `unit_info_v2_status.lua`,
`unit_info_v2_stat_defs.lua`, `unit_info_panel.lua`, `unit_ai.lua`;
`src/Unit/Types/Instance.hs`, `src/Engine/Scripting/Lua/API/Units/Stats.hs`,
`src/Engine/Scripting/Lua/API/Register/Unit.hs`,
`src/Unit/Thread/Command/Body.hs`, `src/Combat/Wounds/Healing.hs`,
`src/Combat/Wounds/Tick.hs`, `src/World/Save/Types.hs`,
`src/World/Save/Component/EntitySnapshots.hs`,
`src/World/Save/Component/Types.hs`; `data/units/*.yaml`;
`tools/physiology_probe.py`;
`test-headless/Test/Headless/Unit/SurvivalExemption.hs`;
`docs/persistence_contract.md` §2, §4, §5;
`docs/persistence_state_inventory.md` rows `umInstances`, `UnitsDTO`,
`unit_resources`; `src/World/Save/CLAUDE.md`. Issue #2761 and the file list
of PR 2824 were read through `gh`; nothing in them was modified. Review round
1: `~/.local/state/syn-physiology-design-review.md` (read, not edited).
