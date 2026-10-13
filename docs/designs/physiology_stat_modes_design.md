# Physiology stat modes design

Give each survival-physiology system an explicit mode, **Nominal** or
**Simulated**, so that "not simulated, healthy" stops being inferred from a
missing `uiStats` key.

Design state: ready for issue processing

**Preconditions: both met.**

- **[#2825](https://github.com/coghex/synarchy/issues/2825)** (mass-proportional
  sweat costs) is merged as `8130d5e16` (PR 2826). `scripts/thermo.lua:99-115`
  defines `sweatMassScale = body_mass/70`, and `:224-235` applies it to both
  sweat debits. The 233× factor in F-2 is therefore historical.
- **[#2761](https://github.com/coghex/synarchy/issues/2761)** (salt reserve)
  is merged as `66e1a99cd` (PR 2824, final head `93b9dd1fa`) and closed.
  Master's `scripts/salts.lua` at `66e1a99cd` is **byte-identical** to the
  `0bf4bbfd5` reference this design was reviewed against
  (`git diff 0bf4bbfd5 66e1a99cd -- scripts/salts.lua` is empty), so every
  reserve citation below holds on master, line numbers included:
  - `salt_store` capacity `storeCap = maxSalt · STORE_CAP_FRAC`, with
    `STORE_CAP_FRAC = 2.0` (`scripts/salts.lua:68,98`);
  - release into the pool at no more than `STORE_RELEASE_FRAC = 0.002` of
    `maxSalt` per second (`:69`);
  - meals deposit into the reserve: `salts.mealSalt` → `salts.deposit`
    (`:146,153`), `MEAL_SALT_FRAC = 0.30` (`:35`).

  These match SD-4, D-1 and D-4, which closes the processing-time check of
  the merged constants.

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

Design review: **READY** (round 4, `bbed40dcd`; R4-1 applied). Both
preconditions are merged, and the merged reserve constants were checked
(header). Ready for issue processing.

- [x] EPIC. Explicit physiology stat modes — [#2827]
- [x] S1. Mass-proportional sweat costs (precondition, outside this epic) — [#2825]
- [x] S2. System registry, species defaults, resolver and read API (inactive: nothing consumes it) — [#2828]
- [x] S3. Override storage and wire format (units v4, frozen v3 graph, decode policy); overrides always empty — [#2829]
- [x] S5. Gameplay consumer migration to the accessor and facade, plus the access audit (inactive preparation; lands **before** S4) — [#2830]
- [ ] S4. Activation (revision 6: T-13 fixture pinned): tick gating, write gate, transitions, modifier-aware seeding, on-demand derived reads, nutrition effect gating and Nominal upkeep, dependency contributions, load normalization (tissue recovery per SD-18)
- [ ] S6. UI display of Nominal
- [ ] S7. Species profile table (SD-10), probe update
- [ ] S8. Stamina, exhaustion, sleep pressure (design addendum first)
- [ ] S9. Cardio and circulation (design addendum first)
- [ ] S10. Blood volume and recovery (design addendum first)
- [ ] S11. Brain and mental state (design addendum first)
- [ ] S12. Owner survey of the remaining unit-specific data. Any further systems or stats it finds become slices here; the epic does not close before this survey

No slice has been selected or filed. Revision 2 redrew the slices (R8).
Revision 4 moves gameplay consumer migration (S5) ahead of activation (S4),
following R2-8. Slice IDs are kept stable, so the list above is in delivery
order, not numeric order. Persistence still lands before any setter.

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

## Review round 2 disposition

Codex review: `~/.local/state/syn-physiology-design-review-2.md`
(syn-solver-21), of revision 3 at `718369741`, treating SD-1 to SD-17 as
settled. I checked each claim against the source at `718369741` and the
#2761 reference branch at `0bf4bbfd5` before applying it.

| Finding | Disposition | Where |
|---|---|---|
| R2-1: two virtual-meal predicates | **Applied.** Verified that revision 3 states both pool fraction and pool plus reserve. One predicate now: pool fraction < 0.85 (SD-4), with a reserve clause that only suppresses repeat deposits. Tick phase and capacity cap (`storeCap = 2·maxSalt` at `0bf4bbfd5`) stated. T-12 covers a low pool with a nonempty reserve and a crossing with an empty reserve | D-4, T-12 |
| R2-2: seeding ignores retained modifiers | **Applied.** Verified `effectiveStat` = `max 0 ((b+Σδ)(1+Σp))` (`src/Unit/Stats.hs:61-77`). The Nominal read value is now separate from the transition's base seed; inversion `b = t/(1+Σp) − Σδ`; atomic refusal when unrepresentable; per-key seed targets; caller values are effective targets. D-1's salt row reconciled with SD-9 | D-1, D-5, T-6 |
| R2-3: Nominal upkeep not separated from Simulated paths | **Applied, plus a new decision (O-18, since settled as SD-18).** Verified the strength discount (`scripts/starvation.lua:89-109`), the walking fat burn and calorie spend (`unit_resource_energy.lua:142-169`), the unbounded regrowth residue (`unit_resource_regrowth.lua:126-173`) and config-gated scheduling (`unit_resources.lua:167-190`). Added the `nominalUpkeep` step for every supported species: positive allocation only, a per-tissue clamp before residue, and synchronous strength restore. Tissue recovery is listed as S4 change 3. Applying it exposed that SD-5 also grows *healthy* below-mean individuals. That was raised as **O-18** rather than guessed, and the owner settled it as SD-18 | D-7, D-9, D-10, T-9, O-18 |
| R2-4: cache freshness over-promised | **Applied.** Verified direct publication (`Stats.hs:353-360`), separate brain writes (`brain.lua:254-269`), the combat `concentration` read (`Combat/Resolution/Common.hs:116-128`) and modifier expiry. v1 derived `salt_conc` is now computed on demand and never stored (no Haskell reader; grep of `src/`). Non-v1 caches keep a named one-pass steady-state lag, separate from the facade-transition and paused-load boundaries | D-1, D-2, D-3, D-5a, T-8 |
| R2-5: load recompute vs exact bytes | **Applied.** Verified the tick order (`unit_resources.lua:135-173`) leaves stored caches stale at a save. Caches are classified *Rebuild deterministically* (serialized-but-ignored after R3-5) and compared canonically. Durable values and overrides stay exact. Zero normalization is qualified by unchanged content. I6 gains a save-boundary case | D-6, I6 |
| R2-6: pre-frame saves | **Applied.** Verified the legacy fallbacks (`unit_resource_energy.lua:99-104,232-241`) and that `recomputeBodyDerivedStats` never creates `frame_mass`. New case (j): reconstruct `frame_mass = 22·h²`, the bulk-1.0 spawn frame, which keeps the legacy floors exactly. Case (g) is now limited to malformed input. A historical fixture is in T-11 | D-6, T-11 |
| R2-7: I3 demands an unreachable meal | **Applied.** Verified baseline loss is 0.004/70 per second, and that a full 2·`maxSalt` reserve keeps the pool full. I3 is steady-state only. T-12 uses explicit near-threshold initial values with an above-threshold control. New T-13 is the coupled thermo + salt sustained-exertion case. Thresholds and bands are unchanged | I3, T-12, T-13 |
| R2-8: API activated before consumers | **Applied.** Verified the raw Haskell readers (`Healing.hs:148-160`, `Wounds/Tick.hs:214`), the raw Lua writers, and the UI `?` fallback. Gameplay consumer migration and the access audit (S5) now land **before** S4 as behavior-preserving preparation. S4's tests run through the activated consumers. The temporary `?` display is documented until S6. Both preconditions are in step 0 | Delivery plan, Testing |
| R2-9 (minor): facade-only rule vs probe | **Applied.** Read verbs are exempt from the facade-only rule (their purity covers owned keys only, SD-19); mutating verbs stay facade-only. Unowned keys pass through with today's semantics in `get`, `getBase`, `getAll` and `fraction`. `getAllStats` is described as a storage enumeration of effective values (`Stats.hs:645-660`) | D-1, D-2, D-3, D-7 |

None is disputed. Revision 3's opening line read as if both prerequisites
were merged; the reviewer was right that they are a condition. The header now
says so, and step 0 lists both.

## Review round 4 disposition

Round 4 (`~/.local/state/syn-physiology-design-review-4.md`, of `bbed40dcd`)
returned **READY**: 0 blocking, 0 major, 1 minor. R4-1 is **applied**: S4's
§2 amendment now also qualifies #2761's per-unit stats paragraph and
cross-references the cache exceptions (D-6).

## Review round 3 disposition

Codex review: `~/.local/state/syn-physiology-design-review-3.md`
(syn-solver-21), of `484bfc952`, treating SD-1 to SD-18 as settled. Verdict
NOT READY: 0 blocking, 1 major, 4 minor. I checked each claim against
origin/master at `37dc3ebae` and the #2761 reference at `0bf4bbfd5`. Revision
6 is the next landed revision (revision 5 was the SD-18 settlement,
`484bfc952`).

| Finding | Disposition | Where |
|---|---|---|
| R3-1 (major): T-13 cannot require a meal from a full reserve | **Applied.** Verified the drain budget: with #2825's `sweatMassScale` (`thermo.lua:99-115,224-235`), a moving 0.3 kg squirrel loses about 0.000773·`maxSalt`/s, about 1.11 capacities per day, below the full 2·`maxSalt` reserve and its 0.002/s release. T-13 now pins body, metabolism, circulation inputs, climate, dt and initial pool, with an **empty** reserve. It requires at least two meals, so re-arming is exercised. The [0.80, 1.35] band and the 0.85 threshold are unchanged, and I3's full-reserve case is untouched | T-13 |
| R3-2 (minor): spawn seeding assumes no modifiers | **Applied.** Verified `spawnMods` from innate modifiers and starting accessories (`Spawn.hs:204-212`, installed `:250-253`). The parenthetical is removed. Seeding uses the completed spawn modifier map | D-5 |
| R3-3 (minor): stale "no new persisted state" | **Applied.** Now reads "no additional persisted target or original-shape value", and notes the SD-18 flag | D-9 |
| R3-4 (minor): T-12 fixtures miss `maxSalt` | **Applied.** (b) and (c) now read 0.80·`maxSalt` and 0.90·`maxSalt` | T-12 |
| R3-5 (minor): cache label vs taxonomy | **Applied.** Verified that the contract's *Rebuild deterministically* is never serialized (`persistence_contract.md:128-137`) and that only Reset names serialized-but-ignored (`:136`). The caches are documented as Rebuild deterministically with a serialized-but-ignored compatibility treatment. S4 amends §2 to admit it. Omitting them from payloads was rejected, as a wire change with no gain | D-6 |

None is disputed. No new owner decision was needed. The precondition text
is updated: #2825 merged as `8130d5e16`, and #2761's code has not merged yet.

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
  under Nominal, owned keys and meters are absent; under Simulated, owned
  keys are present. Derived keys are never stored; they are computed on
  demand.
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
  `{ from: body_mass, scale: 1.0 }`). The healthy rule is what a **Nominal
  read** returns. It is not by itself the base value a transition stores:
  seeding is a separate, modifier-aware rule (D-5, R2-2).
- `derived`: keys the system computes **on demand** from its owned state and
  its dependencies' reads, each with a healthy value (`salt_conc: 1.0`).
  Derived keys are never stored (D-5a, R2-4). Each derived rule is a named
  pure Haskell function, `saltConcentration` for v1, shared by `physioRead`
  and the Lua facade.
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
| salt | `salt` (Nominal read = `body_mass·SALT_PER_KG`; seed target per SD-9 = that × the effective body-water fraction), `salt_store` (#2761; Nominal read and seed target = full, `storeCap`) | `salt_conc` (1.0; on demand) | `salt_imbalance` | `salt_conc` → 1.0; `sweatFactor` → 1.0 | — | cramps, consciousness factor, meter kill |
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

Unowned keys pass through (R2-9). `stamina`, `circulation`, `heart_rate`,
`consciousness`, `strength` and every other key no v1 system owns are never
Unsupported. The read API returns them with mode `nil` and today's
`getStat` / `stats.get` semantics, **unchanged**: effective value, derived
formulas from `scripts/unit_stats.lua`, and `nil` only when undefined.

That includes today's **lazy roll** (SD-19). An authored but unrolled stat is
rolled on first read, which advances the stat RNG and caches the value in
`uiStats` (`src/Engine/Scripting/Lua/API/Units/Stats.hs:570-603`). An
unowned read is therefore *not* side-effect free, exactly as `getStat` is not
today. Only reads of **owned** keys are pure (D-2).

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
- Derived keys under Simulated are computed on read from the effective owned
  and dependency values (D-5a), so they cannot be stale. Meters of a Nominal
  system read 0.
- An unowned key: mode `nil`, with today's semantics (D-1), including the
  lazy roll.

**Read purity (SD-19).** Reads of physiology-**owned** keys (owned, derived
and meter keys of a registry system) are pure. `readPhysio`,
`readPhysioBase` and Haskell `physioRead` never initialize, roll or write for
them. Owned keys are never stat templates (D-3), so they have nothing to
roll. A missing Simulated value is an invariant violation (above), not a
cue to fill it. Unowned keys pass through to today's `getStat` semantics,
lazy roll included. The purity promise does not extend to them.

Haskell gets one `physioRead` with the same semantics. Its owned-key branch
is a pure function of the unit instance. Its unowned branch delegates to
today's `getEffectiveStat` (IO, which may roll). `Healing.hs` and
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
- Storage invariant: under Nominal, the system's owned keys and meters are
  absent from `uiStats`. Under Simulated, its owned keys are present.
  Derived keys are absent in **both** modes, because they are computed on
  demand (D-5a). One pure Haskell transition function maintains this; spawn,
  setters and load normalization all call it.
- Definition loading **rejects** any registry-owned, derived or meter name in
  a definition's `stats:` or `skills:` templates. `getOrRollStat` and
  `getAllStats` therefore cannot recreate a Nominal key. No shipped definition
  uses these names (checked by grep). `getAllStats` keeps its current meaning:
  a storage-oriented enumeration of the stored keys, returned as **effective**
  values (`src/Engine/Scripting/Lua/API/Units/Stats.hs:645-660` applies
  modifiers). The facade `physiology.getAll` adds the mode-aware and derived
  entries for display.

Alternatives considered and rejected: a single cell holding mode and value
per key (it can represent incoherent per-key modes and breaks every stat
path), reserved float keys (a type smuggled into floats), a Lua save module
(Haskell consumers and spawn need the mode), and divergence-only storage (it
loses pinning, see D-5). Defaults are live (SD-14).

### D-4. Dependency substitutes and the four salt × hydration combinations

A Nominal system supplies exactly its declared substitutes (D-1) and nothing
else. The salt input gap (F-4): with nutrition Nominal there are no meals.
**Virtual meals** substitute for them (settled SD-4). The salt tick reads
nutrition's mode. There is **one predicate**, used verbatim in D-4, SD-4,
I3 and T-12 (R2-1):

> A virtual meal fires when nutrition is Nominal, **and** the circulating
> pool fraction `salt / maxSalt < VIRTUAL_MEAL_THRESHOLD = 0.85`, **and**
> the reserve holds less than one meal's deposit
> (`salt_store < MEAL_SALT_FRAC · maxSalt`).

- **The predicate is the settled SD-4 pool-fraction threshold.** The reserve
  clause only *suppresses* repeat deposits. It never triggers on its own and
  never replaces the pool fraction with a pool-plus-reserve total. A full
  reserve is not a full pool (`scripts/salts.lua` at `0bf4bbfd5`: the
  reserve releases into the pool at no more than `STORE_RELEASE_FRAC` per
  second).
- With a low pool and a reserve holding at least one meal, no meal fires:
  the reserve is already refilling the pool. With a low pool and a reserve
  below one meal, exactly one meal fires, after which the reserve holds at
  least one meal and suppression applies.
- **Deposit:** `MEAL_SALT_FRAC · maxSalt` (0.30) into the reserve, by the
  real meal path (`salts.mealSalt` → `salts.deposit`), capped at the
  reserve's capacity `storeCap` (SD-15).
- **Tick phase:** the predicate is evaluated once per salt tick, *after*
  baseline excretion, kidney excretion and reserve release, and before the
  tick stores its values. A deposit made in tick *n* releases from tick
  *n+1*. Sweat debits arrive from thermo, which ticks before salt, so a tick
  sees that tick's sweat.
- The threshold sits just above the 0.80 cramp onset (`CRAMP_START`), so a
  healthy unit never cramps.
- Baseline excretion, kidney excretion and sweat debits continue unchanged.
  The unit behaves as if it ate regularly, and its salt can sit anywhere in
  the normal band, not pinned at full.
- The pool fraction differs from `salt_conc` only when hydration is
  Simulated. The predicate reads the pool fraction because a meal is a pool
  input.

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

- **Nominal → Simulated:** seed each owned key so that its **effective**
  value equals its seed target, using modifier-aware seeding (below). Meters
  start at 0.
- **Simulated → Nominal:** remove the owned keys and meters. Derived keys are
  never stored, so there is nothing to remove for them. Any
  collapse this system caused clears through ordinary `checkRevive`.
- **Values without a mode change:** the system must be Simulated. Each key
  must be an owned key of that system. The value is an **effective target**
  (the same convention as caller `values` at a transition). It must be
  finite and ≥ 0, and ≤ its effective max where one exists, and it is
  stored through the same modifier-aware seeding. Otherwise the whole call is
  refused.
  A successful value write resets the `unit_resource_carry` remainder and the
  alert debounce for those keys. No `onModeChanged` hook runs.
- **Refused:** dead units (a corpse ticks nothing, `unit_resources.lua:129`),
  Unsupported systems, unknown ids, and Simulated without resource parameters
  (D-9).

**Modifier-aware seeding (R2-2).** A transition stores a base value, but
retained modifiers (SD-8) apply again as soon as the system is Simulated
(`src/Unit/Stats.hs:61-77`: effective = `max 0 ((base + Σδ)·(1 + Σp))`). For
each owned key with effective target `t` and that key's active modifiers
(`Σδ`, `Σp` at the current game time):

- the base is `b = t / (1 + Σp) − Σδ`;
- if `1 + Σp ≤ 0`, `b < 0` or `b` is not finite, then `t` cannot be
  represented. **The whole transition is refused atomically** (`nil,
  "seed target unrepresentable: <key>"`), and no key, mode or override
  changes.

Seed targets, all computed from **effective** reads:

| Key | Seed target |
|---|---|
| `hydration` | effective `max_hydration` |
| `salt` | SD-9: `maxSalt ·` effective body-water fraction (1.0 when hydration is Nominal), so `salt_conc` starts at exactly 1.0 |
| `salt_store` | `storeCap` (full) |
| `hunger`, `calories` | effective `max_hunger`, `max_calories` |
| `core_temp` | 37.0 |

Caller-supplied `values` are effective targets too. They deliberately
override the automatic target for debugging and scripted fixtures, and go
through the same inversion and refusal. A modifier that later expires or
changes alters the effective value as any modifier does; seeding guarantees
the target only at the transition. Nominal reads are unaffected: they use the
healthy rule and ignore the owned key's modifiers (SD-8).

`onModeChanged(uid, old, new)` hooks: clear carry remainders and alert
debounce. For nutrition, clear regrowth authorship. Cancel an in-progress
need sequence: `drink_from_source` returns `math.huge` while its latch is
active, before it checks thirst (`unit_ai_water.lua:224-225`).

Spawn: resolve the modes after the body-derived maxima are written, seed
the owned keys of Simulated systems through modifier-aware seeding, and
leave Nominal keys absent. Seeding runs against the **completed spawn
modifier map**: the definition's innate modifiers plus the starting
accessories' buffs, built once as `spawnMods`
(`src/Unit/Thread/Command/Spawn.hs:204-212`, installed at `:250-253`). That
map also applies to the healthy-rule inputs and maxima the targets read.
Base equals target only where the relevant modifier sums are zero. Full salt
with full hydration gives conc 1.0. This replaces the nil-on-first-tick fill for mode-governed
pools. Stamina's commit-time init (#2470) is unaffected.

### D-5a. Derived state and observation boundaries

Revision 3 promised a derived cache that was fresh "before any observation".
The facade cannot keep that promise (R2-4). The engine publishes each stat
write to the unit manager directly
(`src/Engine/Scripting/Lua/API/Units/Stats.hs:353-360`), other threads read
unit snapshots (`src/Combat/Resolution/Common.hs:116-128` reads stored
`concentration`), and modifiers expire with game time, with no setter. So
there are two kinds of derived value, each with a named boundary.

**1. v1 derived keys are computed on demand, never cached.** `salt_conc` is
computed at every read from the effective `salt`, `maxSalt` and the effective
body-water fraction, by the registry's `saltConcentration` rule:

- inputs `(salt/maxSalt) / clamp(hydrationFrac, 0.05, 1)`, the formula of
  `scripts/salts.lua:49-58`;
- a Nominal hydration supplies 1.0;
- a Nominal salt makes it 1.0.

The salt tick's own kidney-excretion step calls the same function. No stored
copy exists, so nothing can be stale: not after a transition, a
`debit`/`credit`, a raw debugging `setStat` of a Simulated pool, a modifier
added or expiring, or a `recomputeBody` that moves `maxSalt`. A legacy stored
`salt_conc` is scrubbed at load (D-6 case (i)). Every reader of `salt_conc`
moves to the accessor in S5: brain, cramps, `sweatFactor`, the status row
and the probe. No Haskell code reads `salt_conc` (grep of `src/`).

**2. Non-v1 caches stay tick-lagged, as today.** These are `circulation` and
brain's outputs (`consciousness`, `concentration`, `state_of_mind`, `mood`).
No v1 system owns them, and they keep their current tick-lagged semantics.

- **Steady-state boundary:** at most one physiology pass of lag (the
  `unit_resources` tick interval). Other threads, including the combat
  resolver's `concentration` read, may observe a value up to one pass old.
  That is unchanged from today.
- **Transition boundary:** the facade's `physiology.refresh(uid)` runs
  synchronously after the engine commits a transition or value write, before
  the facade call returns. It recomputes circulation at the effective core,
  then brain at `dt = 0`, the #2754 precedent
  (`unit_survival_exempt.lua:68-75`). Lua code that sequences after the
  facade call therefore observes coherent values. A concurrent reader on
  another thread may observe, for at most one pass, the new pools beside
  pre-transition brain outputs. That is the steady-state boundary above.
  Brain's own writes are separate stat writes (`scripts/brain.lua:254-269`),
  as today. Making them atomic is S11's addendum.
- **Paused-load boundary:** `unit_resources.onSaveLoaded` runs the refresh
  for every surviving unit after `recomputeBody` and normalization, before
  the first paused frame (`unit_resources.lua:223-230`). It replaces
  `neutralizeLoaded`. Nothing observes the session between staging and that
  refresh except the paused UI, which the refresh precedes.

Only the facade calls the mutating engine verbs (D-7), and the access audit
enforces it, so no Lua transition skips the refresh.

### D-6. Persistence and migration

Classification follows `docs/persistence_contract.md` §2.
`uiPhysioOverrides` is *Persist exactly*. `udPhysiology`, the registry and
the profiles are *Rebuild deterministically*. Rows go into
`docs/persistence_state_inventory.md`, and the `umInstances` / `UnitsDTO`
narratives gain sentences.

Classification of the `uiStats` values this design touches (R2-5):

| Class | Keys | Classification | Round-trip oracle |
|---|---|---|---|
| Durable physiology state | owned keys of Simulated systems (`hydration`, `salt`, `salt_store`, `hunger`, `calories`, `core_temp`) and their meters | *Persist exactly* | exact value equality |
| Durable intent | `uiPhysioOverrides` | *Persist exactly* | exact equality |
| Durable body state | `tissue_recovering` (SD-18; unowned), alongside the existing `fat_mass` / `lean_mass` / `frame_mass` | *Persist exactly* | exact equality |
| v1 derived | `salt_conc` | **not stored** (computed on demand, D-5a) | absent from the payload; the on-demand value is compared after load |
| Tick-lagged caches | `circulation`, `consciousness`, `concentration`, `state_of_mind`, and the body-derived `strength` / `strength_body` / maxima | **Rebuild deterministically, serialized but ignored** (see below) | compared as the **canonical recomputation** from the loaded durable state, not by stored bytes |

**Taxonomy note (R3-5).** `docs/persistence_contract.md:128-137` defines
*Rebuild deterministically* as never serialized. Only *Reset to a documented
default* names a "serialized-but-ignored" variant (`:136`). These caches are
serialized, because the units DTO carries the whole stat map
(`src/World/Save/Component/EntitySnapshots.hs:470`). But their stored values
are never read: the load path overwrites every one of them before the first
frame. Circulation and brain are rebuilt in the D-5a refresh; strength and
the maxima by `recomputeBody` (`scripts/unit_resources.lua:223-225`). They are
rebuilt from durable state, not reset to a fixed default. So the honest
classification is *Rebuild deterministically* with a **serialized-but-ignored
compatibility treatment**, which the current taxonomy does not yet name.

The S4 slice therefore amends `docs/persistence_contract.md` §2 so the
*Rebuild deterministically* row also admits "(or serialized-but-ignored)",
mirroring the Reset row, and records these keys in the inventory with that
treatment.

The same amendment qualifies §2's **Per-unit stats (#2761)** paragraph from
`37dc3ebae` (`docs/persistence_contract.md:139-147`) (R4-1). That paragraph
gives two guarantees: exact preservation, with stored zero and partial
values returned verbatim, and absent-only defaulting by the owner. Both apply
to **durable** stats such as `salt_store`, unless an explicit per-key
classification says otherwise. The paragraph then names the exceptions and
cross-references this D-6 table:

- the serialized-but-ignored caches;
- on-demand `salt_conc`, which is never stored;
- the legacy normalizations (i) and (j).

#2761's exact treatment of a present zero or partial `salt_store` is
preserved.

Omitting the keys from new payloads was considered and rejected:
it would filter the stat map, a wire change in the `units` DTO, for no
behavioral gain.

The tick order makes the stored cache legitimately stale at a save boundary.
Salts and brain tick before the resource pools
(`scripts/unit_resources.lua:135-173`), so a hydration drain later in the
same pass leaves the stored values from earlier in that pass behind. The
load-time rebuild then changes them. That is why caches are compared
canonically and never by bytes. `docs/persistence_contract.md:76-79,128-136`
already separates exact durable fields from rebuilt state.

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
| (g) Seeding or a Nominal-upkeep target needs an input that is present but zero or non-finite, or missing when no legacy rule (j) covers it | **Reject**, naming the unit, key and input. Content-load validation guarantees the inputs for current spawns of species meeting `requires`, so this arises only from malformed saves. It is kept distinct from legitimate legacy absence, (j) |
| (h) A stored Simulated value that is non-finite | Reject: malformed. Finite out-of-range values are retained exactly, adding no validation beyond today's |
| (i) A stored `salt_conc` (legacy; any mode) | Scrub, logged. It is now computed on demand (D-5a) |
| (j) A bodied unit with `height` but no `frame_mass` (pre-frame save; the existing fallbacks support this, `scripts/unit_resource_energy.lua:99-104,232-241`) | **Legacy reconstruction**: `frame_mass = 22·height²`, logged. That is the spawn formula `22·h²·bulk` at bulk 1.0 (`Body.hs:96`). It is exactly the frame the legacy floors already assume: `0.44·h² = 0.02·22·h²` (`LEGACY_MIN_FAT_PER_H2`, `MIN_FAT_FRAC`, `unit_resource_energy.lua:83-84`) and `4.4·h² = 0.20·22·h²` (`:241`). The fat and lean floors therefore do not move for such a unit. Reconstruction is required because `recomputeBodyDerivedStats` never creates `frame_mass` (`Body.hs:158-181`), and load calls `recomputeBody`, not spawn seeding |

Exact round trip versus normalization, qualified by unchanged content and
canonical input. A session written by current code, loaded under **the same
content** (no profile, registry or definition change since it was saved),
loads with **zero** normalization actions. On re-save, every durable value
and override is bit-identical, and every cache equals its canonical
recomputation. A content change (SD-14) or a legacy payload normalizes once,
logged. Saving that session and loading it again performs **zero** further
actions (idempotence). The legacy frame reconstruction (j) is persisted, so it
is not repeated. Normalization and reconstruction are deterministic because
their rules are pure functions of stored stats.

No Lua save module is added. The `unit_resources` reset hook and teardown
registration also clear any hook caches.

### D-7. Script API

Engine verbs. **The mutating verbs** (`setPhysioMode`, `clearPhysioMode`)
**may be called only by the facade**, which runs hooks and the refresh. The
**read verbs** (`getPhysioMode`, `getPhysioOverride`, `getPhysioDefault`,
`readPhysio`, `readPhysioBase`) are an explicit exception (R2-9). Any code may
call them, including the console Lua that `tools/physiology_probe.py` sends.
They never change physiology state:

- modes, overrides and owned, derived and meter keys are untouched (SD-19);
- for an **unowned** key, `readPhysio` / `readPhysioBase` behave exactly as
  `getStat` / `getStatBase` do today, including the lazy roll of an authored
  but unrolled stat.

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
enable a system before forcing its pool (SD-16). A **derived** key
(`salt_conc`) is refused in **both** modes, since a stored copy would break the
on-demand rule (D-5a). Under Simulated, raw `setStat` of owned keys and
meters is still allowed for debugging. `unit.addModifier` on a Nominal
owned key is accepted and stored, but has no effect until the system is
Simulated (SD-8).

Facade `scripts/physiology.lua`:

- `get(uid, key)` → `value, mode` (effective). For an unowned key: `value,
  nil`, by today's `stats.get` semantics (D-1).
- `getBase(uid, key)`. For an unowned key: today's `getStatBase`.
- `fraction(uid, key, maxKey)` → 1.0 under Nominal. For unowned keys:
  `get(key) / get(maxKey)` by today's semantics, `nil` when either is
  undefined.
- `getAll(uid)` → today's `stats.getAll` enumeration of effective values,
  plus mode-aware and on-demand derived entries for owned systems.
- `isSimulated(uid, system)`
- `debit(uid, key, amount)` / `credit(uid, key, amount)` → amount applied, 0
  under Nominal
- `setMode`, `clearMode` (wrapping the verbs, then hooks, then `refresh`)
- `refresh(uid)`
- `registerSystem(id, { tick, onModeChanged })`

Orchestration: `unit_resources.update` runs cardio → circulation → each
Simulated system's `tick` in registry order (thermo before salt, as today) →
brain → … Pool entries, digestion, starvation, regrowth and the strength
*discount* run only when nutrition is Simulated. **Nominal upkeep** (D-9)
runs when nutrition is Nominal, for every supported species, including
species with no `unit_resource_config.lua` entry (R2-3). The
`survivalExempt` branch goes away.

Access audit (new, or extending an existing audit; it has a self-test): it
flags raw `getStat` / `setStat` / `or <default>` reads on registry keys
outside their owner, **mutating** verb calls outside the facade (read verbs
are exempt), registry coverage
(every system has a Lua owner, every owned key a healthy rule, every
`contributes` entry a substitute), and templated physiology names.

**Temporary UI exception (S5 to S6).** The audit carries one named,
enumerated, *temporary* exception, for the legacy UI pool reads that stay
raw until S6's Nominal-aware display. It lists exactly these sites, and any
other raw UI read is still flagged:

- `scripts/unit_info_panel.lua`: the `hydration`, `hunger` and `calories`
  reads (about lines 85, 93 and 101);
- `scripts/unit_info_v2_stat_defs.lua`: the generic `stats.get` pool reads
  in `fmtCurMax` (about lines 25-26).

The `salt_conc` status row (`scripts/unit_info_v2_status.lua`, about lines
60 and 63) is **not** excepted. It moves to the accessor in S5, as D-5a
requires. S6 deletes the exception.

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
  `starvation.refreshStrength`'s discount; and the **organ-failure
  contribution inside the stamina tick**
  (`unit_resource_tick.lua:187-200,259`). With nutrition Nominal,
  `inOrganFailure` is false. What stays live under Nominal nutrition: ordinary
  stamina regen and drain, stamina's `kill_on_zero` from any other cause, and
  the injury `organ` failure meter (`unit_resource_failure.lua`), which is
  wound-driven and a different mechanism.
- **Tissue under Nominal nutrition recovers toward healthy** (settled SD-5).
  Body-composition upkeep reads nutrition's mode, and Nominal means
  "sufficient nutrition". The mechanism (R2-3):
  - **Scheduling.** A new `nutrition.nominalUpkeep(uid, dt)` step runs in
    `unit_resources.update` for every living unit whose nutrition is Nominal
    and supported. It is scheduled **outside** the per-def config branch
    that gates digestion and starvation today
    (`scripts/unit_resources.lua:167-190`), so nomads and technomules, which
    have no config entry, are covered.
  - There is no catabolism, so no tissue is lost.
  - **Allocation.** It reuses only the *positive* allocation of
    `applyRegrowth` (`scripts/unit_resource_energy.lua:142-169`), at the
    same tissue rate and activity split (idle 90 % fat / 10 % muscle,
    walking 30 % / 70 %). It **excludes** the calorie spend, since there is
    no live store, and the walking fat burn (`0.5·rate`), which makes
    walking fat delta negative today. A Nominal walker therefore never loses
    fat, so recovery is monotonic.
  - **Clamp and residue.** Each tissue's increment is clamped so the tissue
    never exceeds its target, *before* the sub-binary32 residue is retained
    through `unit_resource_regrowth` (`:126-173`). Residue never carries a
    tissue past its target. A tissue already at target receives nothing,
    and its share is not redistributed.
  - Tissue above its target is kept as-is. Sufficient nutrition does not
    slim a unit.
  - The target is the **species-mean body fat applied to the unit's own
    `frame_mass`**, through the spawn formula (`Body.hs:91-125`):
    `fat = max(minFat·1.7, frame·f̄)`,
    `lean = max(minLean·1.07, frame·(1−f̄)·0.5)`, where f̄ is the
    `bodyfat` mean in `udBodyTemplates` (`src/Unit/Types/Def.hs:139-144`).
  - No additional persisted target or original-shape value is needed.
    `frame_mass` is already stored, and the rolled `bodyfat` is dropped at
    spawn (`Body.hs:123-124`), so an unusually lean or fat individual
    recovers to the species norm, not to its own spawn shape. That is
    accepted. SD-18 does add one persisted value, the `tissue_recovering`
    flag (below).
  - Mass changes call `recomputeBody` as regrowth does today. Carrying
    capacity and the maxima therefore recover with the tissue, and fat
    insulation in thermo follows.
  - **Strength.** On the Simulated → Nominal transition, nutrition's
    `onModeChanged` hook **synchronously** writes `strength =
    strength_body`, so a previous calorie discount is cleared even for a unit
    already at target, which has no mass change to force a recompute
    (`scripts/starvation.lua:89-109`). While Nominal, `nominalUpkeep`
    maintains `strength = strength_body` every pass, independently of the
    Simulated-only discount gate. On the Nominal → Simulated transition, the
    next `refreshStrength` reapplies the band for the seeded (full) calorie
    store, which leaves strength undiscounted.
  - **Which units recover: only depleted tissue (SD-18).** Upkeep regrows
    only while the unit's **`tissue_recovering`** flag is set. Healthy
    individuals keep their rolled shape, including those that rolled below
    the species mean.
    - **Set** the flag, under either nutrition mode, when fat falls below its
      spawn viability margin `minFat·1.7` or lean below `minLean·1.07`
      (`Body.hs:85-89,113-114`). Also set it when Simulated catabolism
      removes tissue (`tickStarvation`'s deficit branch,
      `unit_resource_energy.lua:255` onward). A healthy spawn starts at or
      above both margins, because spawn seeding clamps to them, so it never
      starts flagged.
    - **While set and nutrition is Nominal**, upkeep regrows each tissue
      toward its species-mean target, as above.
    - **Clear** the flag when both tissues reach or exceed their targets,
      under either mode. Under Simulated, surplus regrowth can clear it the
      same way.
    - **Storage:** `tissue_recovering` is a 0/1 body-composition stat in
      `uiStats`, beside `fat_mass` and `lean_mass`. It is **not owned by the
      nutrition system**, because the storage invariant removes owned keys
      under Nominal, which is exactly when the flag is needed. It is an
      unowned pass-through key (D-1) and *Persist exactly*. A legacy save
      without it reads 0. A legacy wasted unit below a margin is flagged on
      its first upkeep pass, so no migration is needed.
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
3. Nominal-nutrition units (bear, squirrel, technomule, nomad) gain Nominal
   upkeep (SD-5, D-9). Strength is undiscounted, and **depleted** tissue
   regrows to the species-mean target (SD-18). Healthy individuals do not
   change shape.

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
  Nominal). Temperate climate, idle and sustained exertion, over 600
  tick-seconds (10 game-hours at `DAY_SECONDS` 1440). Starting healthy, each
  Simulated system stays within its healthy band; for salt that is
  `salt_conc` in [`CRAMP_START`, `HYPER_START`] = [0.80, 1.35]. **No virtual
  meal is required here** (R2-7). With thermo Nominal there is no sweat, so a
  full pool loses only baseline excretion (`0.004/70` of capacity per second,
  `scripts/salts.lua:30,67`), about 3.4 % over the window, and the full
  reserve refills it. The trigger is exercised by T-12 and the sustained
  coupled case by T-13.
- **I4 Mass invariance of salt and hydration debits** is owned by #2825's
  own acceptance, not by this epic. This epic only requires that the mode
  gating does not reintroduce absolute debits: `debit` is applied as a
  fraction of the receiver's capacity, and T-1 runs at 0.3 kg.
- **I5 Transition contract**, with an Haskell pure part: seed, scrub,
  refusals, override set/clear semantics (D-5 table), values validation,
  revive after Simulated → Nominal, and cancellation of a need latch.
- **I6 Persistence**, on the D-6 classification boundary (R2-5):
  - Under unchanged content, a round trip performs zero normalizations.
    Durable values and overrides are bit-identical, and caches equal their
    canonical recomputation.
  - **Save-boundary case:** save immediately after a pass in which hydration
    drained after salts and brain ticked, and after a `recomputeBody` mass
    change. The stored caches are then legitimately stale. The durable
    oracle still holds exactly, and the caches still hold canonically.
  - A content-change case (SD-14) normalizes once, logged, and a second load
    makes no changes.
  - Legacy normalization is idempotent.
  - The D-6 table, as a pure table-driven spec over (a) to (j).

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
  healthy value. **Retained modifier across Nominal → Simulated (R2-2):**
  - With a +50 % percent modifier on `salt` and hydration at 0.5, the
    transition seeds base `b = 0.5·maxSalt / 1.5`, so effective salt is
    `0.5·maxSalt` and `salt_conc` reads exactly 1.0.
  - With a −100 % percent modifier on `salt`, the transition is refused
    atomically: mode, override and keys are unchanged.
  - A caller `values` target goes through the same inversion.
- **T-7 No write-through.** With hydration and salt Nominal, `getAllStats`,
  `getStat`, `getStatBase` and a failed `setStat` leave the owned, derived and
  meter keys absent. A definition templating an owned name fails to load.
- **T-8 Derived freshness** (R2-4):
  - `salt_conc` is coherent, read with no tick in between, after each of:
    salt Nominal → Simulated; hydration Nominal → Simulated with salt
    Simulated; a raw debugging `setStat` of Simulated `salt`; a `debit`; a
    `recomputeBody` that moves `maxSalt`; and the **expiry** of a modifier on
    `salt` (advance game time only, with no setter).
  - After a facade transition returns, its caller reads refreshed
    circulation and consciousness.
  - After a paused load, the same holds before the first frame.
  - Non-v1 caches observed from outside the facade are asserted only to
    within one pass (the D-5a steady-state boundary).
- **T-9 Nutrition floors and Nominal upkeep** (R2-3). An acolyte at the fat
  floor, and one at the lean floor, are switched to nutrition Nominal.
  - Neither suffers organ-failure drain nor a starvation death over 1
    game-day.
  - Both tissues rise monotonically toward the species-mean target, idle
    **and** walking (no walking fat burn), and stop exactly at it, with no
    residue overshoot.
  - A unit above target keeps its mass.
  - A unit **already at target with a stale calorie discount** on `strength`
    reads `strength = strength_body` immediately after the transition, with
    no mass change and no tick.
  - A `nomad_primitive`, which has no config entry, receives upkeep.
  - A wound-driven `organ` meter and a stamina drain from exertion still kill
    when driven.
  - **SD-18 cases:**
    - A healthy individual that is above its margins but below the mean is
      unflagged and does not grow.
    - A unit pushed below a margin is flagged, recovers to the target, and
      is then unflagged.
    - A unit that lost tissue to Simulated catabolism, while staying above
      its margins, is flagged at catabolism and recovers once Nominal.
    - A legacy save without the flag reads 0, and a below-margin legacy unit
      is flagged on its first pass.
- **T-12 Virtual meal trigger** (R2-1, R2-7). Salt Simulated, nutrition and
  hydration Nominal, thermo Nominal (so no sweat), squirrel mass, with
  **explicit initial values** set via `setPhysioMode` values:
  - (a) pool 0.8505·`maxSalt`, reserve 0. Baseline excretion crosses 0.85
    within about 9 s, and exactly one meal fires, depositing
    0.30·`maxSalt` into the reserve.
  - (b) pool 0.80·`maxSalt`, reserve 0.5·`maxSalt` (at least one meal). No meal fires,
    because the reserve is already releasing.
  - (c) pool 0.90·`maxSalt`, reserve 0, over 60 s. No meal fires: the above-threshold
    control.
  - (d) case (a) with nutrition Simulated. No virtual meal fires.

  The capacity cap cannot bind while suppression holds, because
  `storeCap = 2·maxSalt` exceeds two meals. A unit spec covers the cap.
- **T-13 Sustained coupled exertion** (R2-7, R3-1). Salt and thermo
  Simulated, hydration and nutrition Nominal. All inputs are pinned:
  - **Body:** `body_mass` 0.3 kg (so `maxSalt` = 0.3), fat fraction 0.10,
    constitution 0.5 (circulation ≈ 0.75, `scripts/circulation.lua:33-61`),
    metabolism 1.2, no clothing, no wounds or infection, full blood, and
    `heart_rate` at its activity target.
  - **Climate:** forced activity `walking`, 22 °C, humidity 0.5, fixed
    dt = 0.1 s, over 1,440 tick-seconds (1 game day).
  - **Initial values** (via `setPhysioMode` values): `core_temp` 37.0, pool
    1.00·`maxSalt`, and **reserve 0**. This is a deliberately depleted
    reserve, not the healthy full seed.

  **Verified drain budget** (algebra from source, R3-1):
  - Heat production 1.68 settles where
    `1.68 = (T − 22)·0.1·0.875/1.2 + 0.75·(T − 37.5)`, giving core ≈ 38.17 °C
    and sweat ≈ 0.501 (`scripts/thermo.lua:28-69,176-208`).
  - The #2825 sweat debit plus baseline excretion then removes about
    `(0.10·0.501 + 0.004)/70 ≈ 0.000773` of `maxSalt` per second
    (`thermo.lua:224-235`; `salts.lua:30`).
  - From 1.00, the pool crosses 0.85 after about 195 s, and the first virtual
    meal fires with the reserve empty.
  - The 0.30 deposit releases at up to 0.002/s, faster than the drain, so the
    pool returns toward ideal. The next trigger needs the pool at 0.85 again
    with the reserve below one meal. Each meal adds exactly the salt lost
    between triggers, so the meal-to-meal period is
    `0.30/0.000773 ≈ 388 s`. That predicts meals at roughly 195 s, 585 s,
    970 s and 1,360 s.
  - The overshoot below 0.85 at a trigger is at most one tick's drain
    (≈ 0.00008). The next tick's net release (≈ 0.00012) lifts the pool back
    above 0.85, so a meal cannot fire twice in a row.

  **Asserts:**
  - **at least two** virtual meals fire, so the predicate re-arms after a
    replenishment cycle;
  - after each meal, the pool returns above 0.85·`maxSalt`;
  - `salt_conc` stays in [0.80, 1.35] throughout;
  - no NaN.

  The band and SD-4's threshold are unchanged. The fixture's depleted
  reserve, not any constant, is what makes the trigger reachable. With the
  **healthy full reserve** (2·`maxSalt`), the same day loses about 1.11
  capacities, against a release faster than the loss. That reserve keeps the
  pool near ideal and fires no meal, which matches I3's steady-state case.
  The release rate and capacity are #2761's (`0bf4bbfd5`). Recheck them
  against the merged code.
- **T-10 Override semantics.** Set-equal-default, clear-equal-default,
  changed-default with and without a pin, each across save and load (D-5
  table).
- **T-11 Load policy.** One case each for (b), (c), (d), (f), (g), (i) and
  (j). (j) uses a historical pre-frame fixture: its fat and lean floors are
  unchanged after reconstruction, and a second load reconstructs nothing.
- **Activated paths, not the accessor alone** (R2-8). S4's run of I1, T-6
  and T-9 must go through the real activated consumers:
  - the Haskell `Healing.hs` nutrition multiplier with a negative `hydration`
    modifier;
  - `Wounds/Tick.hs` fever with thermo Nominal;
  - `unit_ai_needs` drink and eat;
  - `consumable.drink`;
  - brain consciousness.

### Static and probe

- The access audit with its self-test (D-7).
- `tools/physiology_probe.py` reads through `unit.readPhysio`, a read verb
  allowed outside the facade (D-7, R2-9). Its owned-key reads are pure
  (SD-19). It reads `circulation` as an
  unowned pass-through key and `salt_conc` on demand (F-9). Its bounds are
  unchanged. F-2's primary gate is #2825's; T-1 covers it under
  the modes.
- Validation follows the root CLAUDE.md tiers: targeted `--match` groups,
  the D-6 persistence gates, and the matching self-tests. No full suites.

## Open decisions for the owner

None. Every decision is settled: SD-1 to SD-17 in revision 3, and SD-18 (the
revision-4 decision O-18) on 2026-10-13. IDs are stable, and each settled
entry below names the decision it replaced.

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
- **SD-19 (raised during S2 processing), owner 2026-10-13. Owned-key read
  purity.** Only reads of physiology-**owned** keys are pure. `readPhysio`,
  `readPhysioBase` and Haskell `physioRead` never initialize, roll or write
  for owned keys.
  - Unowned keys pass through to today's `getStat` semantics unchanged. That
    includes the lazy roll of an authored but unrolled stat
    (`src/Engine/Scripting/Lua/API/Units/Stats.hs:570-603`), which advances
    the stat RNG and caches the value. Derived formulas from
    `scripts/unit_stats.lua` also keep today's behavior.
  - The read verbs stay callable by any code, and the facade-only rule for
    mutating verbs is unchanged (D-1, D-2, D-7).
  - Source: `~/.local/state/syn-process-physio-20261013-1/S2-read-contract-question.md`.
    Epic #2827's contract bullet is amended to match.
- **SD-18 (was O-18), owner 2026-10-13. Recover only after depletion.** This
  is option (b). Nominal tissue recovery acts only on a unit flagged
  `tissue_recovering`. The flag is set when a tissue falls below its spawn
  viability margin, or when Simulated catabolism removes tissue. Recovery then
  runs to SD-5's species-mean target and clears the flag. Healthy
  individuals, including below-mean rolls, keep their shape.
  - The flag is an unowned, persisted body-composition stat (D-9).
    Revision 4's O-18 text called it "an owned nutrition key". That was
    wrong, because the storage invariant removes owned keys under Nominal,
    and it is corrected here.
  - Rejected: (a) all below-target tissue regrows, so body diversity
    converges to the mean; (c) recovery only to the margin, which
    contradicts SD-5.

No open decisions remain. Implementation-time checks:

- #2761's merged constants (`storeCap`, `STORE_RELEASE_FRAC`,
  `salts.deposit`) against SD-4, D-1 and D-4;
- #2825's merged sweat constants against T-13;
- the S8 to S11 design addenda.

## Delivery plan (dependency-ordered)

Each activated intermediate state must satisfy its own advertised invariants
and round-trip contract.

0. **S1 and preconditions.** S1 is **#2825** (mass-proportional sweat
   debits), merged as `8130d5e16`. With **#2761** (salt reserve, PR 2824,
   merged as `66e1a99cd`), it is a precondition of this epic, not a slice to
   file (SD-3, SD-15).
1. **S2.** Registry, `udPhysiology`, profiles, the pure resolver, read verbs,
   and template rejection. **Inactive:** no tick, consumer or save path uses
   it. Advertised invariants: resolver specs only.
2. **S3.** `uiPhysioOverrides` through the live instance, snapshot, wire and
   restore; units v4 with the frozen v3 graph and explicit dispatch;
   fixtures; decode-time rejections (d), (e) and (h). No setter exists, so
   overrides are always empty. Advertised: I6's exact round trip and the
   migrations. Load policy settled (SD-6).
3. **S5. Gameplay consumer migration, inactive preparation** (R2-8; lands
   before S4). Every gameplay reader and writer of a registry key moves to
   the facade or to Haskell `physioRead`:
   - readers: AI needs, brain, cramps, `sweatFactor`, `Healing.hs`,
     `Wounds/Tick.hs`, and the probe aggregate;
   - writers: `unit_ai_needs.lua:174`, `consumable.lua:171-176`, thermo's
     sweat debits and `salts.mealSalt` move to `debit` / `credit`.

   The access audit lands with them. While inactive, the facade reports
   today's semantics for every key, so S5 is behavior-preserving. Advertised:
   the audit and its self-test, plus existing gates unchanged.
   The audit ships with the temporary UI exception (D-7) for exactly the
   enumerated legacy pool reads. The `salt_conc` status row is migrated here.
4. **S4. Activation.** Tick gating; the write gate; transitions and setters;
   modifier-aware seeding; on-demand `salt_conc`; `refresh`; nutrition
   effect gating and Nominal upkeep; the dependency contributions, including
   virtual meals (SD-4); load normalization (b), (c), (f), (g), (i) and (j);
   and the activation profile set with its three intentional changes (D-10).
   - Advertised: I1 to I3, I5, I6, T-1 to T-4 and T-6 to T-13. Each runs
     through the real activated Haskell and Lua consumers migrated in S5,
     not only the accessor.
   - Needs S2, S3, S5, SD-7 to SD-9 and SD-18.
   - **Temporary display, honestly documented in the S4 PR:** until S6, the
     status and debug panels show `?` for a Nominal pool, which is absent
     from storage (`scripts/unit_info_v2_stat_defs.lua:23-28`,
     `scripts/unit_info_panel.lua:84-88`). This is presentation only.
     Gameplay readers already use the facade from S5.
5. **S6.** UI display of Nominal, with `--offscreen` evidence in
   `docs/pr-proofs/`. S6 migrates the excepted legacy UI pool reads (D-7) to
   the Nominal-aware display and **deletes the temporary audit exception**.
   Acceptance includes the access audit passing with the exception removed.
6. **S7.** The owner's profile table (SD-10), and the probe's switch to Nominal
   assertions where squirrel modes change.
The remaining systems (SD-12) follow, one slice each, after S4. Each starts
with a design addendum to this doc covering its interface, healthy rules,
substitutes and effects.

7. **S8.** Stamina, exhaustion and sleep pressure. Stamina's addendum must
   handle the combat thread's concurrent spend (#2470).
8. **S9.** Cardio and circulation.
9. **S10.** Blood volume and recovery.
10. **S11.** Brain and mental state.
11. **S12. Owner survey** of the remaining unit-specific data, which may add
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
