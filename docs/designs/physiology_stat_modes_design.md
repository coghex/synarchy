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
- [x] S4. Activation (revision 6: T-13 fixture pinned): tick gating, write gate, transitions, modifier-aware seeding, on-demand derived reads, nutrition effect gating and Nominal upkeep, dependency contributions, load normalization (tissue recovery per SD-18) — [#2831]
- [x] S6. UI display of Nominal — [#2832]
- [x] S7. Species profile table (SD-10), probe update — [#2833]
- [x] S8a. Stamina: mode-aware combat cost and damage, effective-unit stamina commit, max resolution, SD-20 nomad engagement (addendum S8; depends on S4 #2831; independent) — [#2834]
- [x] S8b. Exhaustion and sleep pressure modes (addendum S8; depends on S4 #2831; independent) — [#2835]
- [x] S9. Cardio and circulation (addendum S9; depends on S4 #2831; independent) — [#2836]
- [x] S10. Blood volume and recovery (addendum S10; depends on S4 #2831; independent) — [#2837]
- [x] S11a. Brain and mental state: atomic writes and on-demand derived values, both systems registered all-Simulated (addendum S11; depends on S4 #2831; independent) — [#2838]
- [x] S11b. Mind and consciousness modes with the SD-23 profiles (addendum S11; depends on S11a) — [#2840]
- [ ] S12. Owner survey of the remaining unit-specific data. Any further systems or stats it finds become slices here; the epic does not close before this survey

S1 to S7 are filed as #2825 and #2827 to #2833. Revision 2 redrew the
slices (R8). Revision 4 moved gameplay consumer migration (S5) ahead of
activation (S4), following R2-8. Slice IDs are stable, so the list above is
in delivery order, not numeric order. Persistence still lands before any
setter.

The S8 to S11 addenda are owner-approved (2026-10-13; Codex A1 to A4
READY), so S8's former deferral is lifted. S8 and S11 are split into S8a,
S8b, S11a and S11b, following the approved slice plans. S9 and S10 stay
single.

- S8a, S8b, S9, S10 and S11a each depend only on S4 (#2831). They are
  **independent** of one another, so a block in one does not stall the
  others.
- S11b depends on S11a.
- S12, the owner survey, still gates epic closure.

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

*Amended by S9 (#2836): `circulation` is no longer an always-on step or a
cache. It is the on-demand derived key of the `cardio` system (S9.2, S9.9),
and the paragraph below describes the S4 state it replaced.*

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

**2. Non-v1 caches stay tick-lagged, as today.** *(S9, #2836: `circulation`
left this list; it is derived on demand and never cached. The facade's
`refresh` now recomputes only the brain.)* These were `circulation` and
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
| Tick-lagged caches | `consciousness`, `concentration`, `state_of_mind`, and the body-derived `strength` / `strength_body` / maxima (`circulation` left this row in S9, #2836: it is derived, **not stored**, and a legacy stored value is scrubbed at load in every mode, D-6 (i)) | **Rebuild deterministically, serialized but ignored** (see below) | compared as the **canonical recomputation** from the loaded durable state, not by stored bytes |

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

Orchestration: `unit_resources.update` runs cardio (S9: only while Simulated;
circulation is no longer a step) → each
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

**Former temporary UI exception (S5 to S6; deleted by S6, #2832).** The audit
carried one named, enumerated, *temporary* exception for the legacy UI pool
reads that stayed raw until S6's Nominal-aware display: the `hydration`,
`hunger` and `calories` reads in `scripts/unit_info_panel.lua` and the generic
`stats.get` pool reads in `fmtCurMax` of `scripts/unit_info_v2_stat_defs.lua`.
The `salt_conc` status row was never excepted (S5 moved it to the accessor, as
D-5a requires). S6 routed those sites through the facade and **deleted the
exception**, its types and its matching: no UI allowance remains, and the
audit's self-test now proves each former raw form is flagged like any other.

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

**Final species table, applied by S7 (#2833).** The shipped `data/units/*.yaml`
now carry exactly the SD-10 table above. Relative to S4's activation set, the
one **intentional effective change** is `red_squirrel`: `reduced` → `simple`,
so its salt goes Simulated → Nominal (hydration and nutrition were already
Nominal, thermo stays Simulated). A squirrel therefore holds no `salt` /
`salt_store` / `salt_conc` / `salt_imbalance` state, reads `salt_conc` 1.0
Nominal, has no cramps and no salt failure contribution, and its sweat factor
is 1.0; its thermo, circulation and every unowned system are untouched. Bodyless
definitions (`tiller`, `unknown_unit`, `white_tailed_deer`) stay Unsupported.
Nothing else is retuned. Saves follow D-6 (b): an unpinned squirrel saved under
`reduced` adopts `simple` on load (salt state scrubbed, one normalization note,
idempotent), while a squirrel pinned to salt Simulated keeps its pin and its
durable salt and reserve state exactly, a pin equal to the old default
included.

**Independent small-mass Simulated coverage.** The production squirrel's Nominal
salt must not stand in for the Simulated regression at 0.3 kg. That coverage
stays on explicit fixtures that pin their own modes: T-1's four salt x hydration
combinations and T-12's virtual-meal controls (`physiology stat modes`, all on
an explicit `reduced` fixture, unaffected by the shipped default), plus, in
`physiology species profiles`, the shipped squirrel with salt pinned Simulated
through the facade, held to the probe's 0.7 to 1.3 concentration and 33.5 to 39
core bounds under idle and exertion. No bound was relaxed. The physiology probe
asserts the squirrel's Nominal semantics through the mode API
(`tools/physiology_probe.py`, predicate proved engine-free by
`tools/test_physiology_probe.py`): thermo Simulated, salt, hydration and
nutrition Nominal as the species default (no probe-written pin), `salt_conc`
1.0 Nominal (never a zero standing in for an absent key), no Nominal state
stored, no imbalance, no cramps; it keeps the temperate core range, the
no-death check, the body-composition checks, the cold response and the
exhaustion-amble regression.

S7 applies the owner's table (SD-10).

**Stamina column, applied by S8a (#2834).** The table gains its fifth system.
`acolyte`, `bear_brown` and `red_squirrel` run stamina Simulated; `technomule`
runs it Nominal (`reduced` plus an explicit `stamina: nominal`, S8-F1: the mule
has no pool configuration, so a Simulated one would leave it permanently
winded); `nomad_primitive` is `exempt` with `stamina: nominal` and
`on_combat_engagement: { stamina: simulated }` (SD-20), so it spawns with no
pool and becomes an acolyte-like Simulated unit at its first admitted combat
attempt. All four profiles name stamina (`rich`, `reduced` and `simple`
Simulated, `exempt` Nominal), because the loader refuses a profile that does
not name every system. Bodyless definitions stay Unsupported.

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

None. Every decision is settled, all on 2026-10-13:

- SD-1 to SD-17 in revision 3;
- SD-18, the revision-4 decision O-18;
- SD-19 during S2 processing;
- SD-20 to SD-23 for the S8 to S11 addenda.

IDs are stable, and each settled entry below names the decision it
replaced.

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
- **SD-20 (S8-D1), owner 2026-10-13. Nomad stamina: Nominal until first
  combat engagement, then Simulated like an acolyte's.**
  - The switch includes exhaustion death (`kill_on_zero`).
  - It is the standard S4 transition: a full seed and a pinned, one-way
    override, which survives save/load.
  - The trigger is an admitted attack attempt, as attacker or target, hit or
    miss. It is detected in the combat commit, which sets the durable
    `combat_engaged` marker to pending, and applied by the Lua facade's
    `unit.applyCombatEngagement` on the next eligible pass, which seeds, pins
    and latches it as applied (S8.3, revised for A1-1).
  - Nomad stamina parameters are generous: +50 % `max_stamina` and 2.25×
    regen, calibrated by T-S8-5. Exhaustion death in an ordinary encounter
    is a tuning defect.
  - Out of scope, tracked separately: nomads' wider physical effectiveness
    (strength, damage).
  - The regen multiplier was revised from 1.5× to 2.25× in A1, because
    strike costs are a fraction of the maximum (S8.3). That serves the same
    owner intent.
- **SD-21 (S9-D1), owner 2026-10-13.** Option (a). Under Nominal cardio,
  circulation keeps its injury factors; only heart rate reads healthy.
- **SD-22 (S10-D1), owner 2026-10-13.** Option (a). No shipped profile uses
  Nominal blood, and Nominal is available by override. Performance gives
  no reason for it: an unwounded Simulated unit costs only per-unit
  arithmetic in the wound fold that runs anyway (`Tick.hs:185,540-553`).
- **SD-23 (S11-D1), owner 2026-10-13.** Option (b). Only acolytes keep a
  Simulated mind. Wildlife, the technomule and nomads are Nominal.
  Consciousness stays Simulated everywhere. Bears and squirrels stop having
  mental breaks.
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
   The audit shipped with the temporary UI exception (D-7) for exactly the
   enumerated legacy pool reads (deleted by S6). The `salt_conc` status row is
   migrated here.
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
     Gameplay readers already use the facade from S5. **Ended by S6 (#2832):**
     the panels now show `Nominal` (D-8), and the `?` for a Nominal pool is
     gone.
5. **S6.** UI display of Nominal, with `--offscreen` evidence in
   `docs/pr-proofs/`. S6 migrates the excepted legacy UI pool reads (D-7) to
   the Nominal-aware display and **deletes the temporary audit exception**.
   Acceptance includes the access audit passing with the exception removed.
   Delivered as #2832: the unit-info Status panel and the legacy debug text
   follow the resolved modes; the exception is deleted; the real offscreen
   evidence is `docs/pr-proofs/issue-2832-nominal-mode-ui.md`.
6. **S7.** The owner's profile table (SD-10), and the probe's switch to Nominal
   assertions where squirrel modes change.
   Delivered as #2833 (see D-10, "Final species table, applied by S7").
The remaining systems (SD-12) follow, with the owner-approved addenda as
their design (section "Addenda: remaining systems (S8 to S11)"). **S8a, S8b,
S9, S10 and S11a each depend only on S4 (#2831), and are independent of one
another**, so a block in one does not stall the others. S11b depends on
S11a.

7. **S8a. Stamina.** Addendum S8, slice plan S8.7 (S8a): mode-aware `Wear`
   and `Damage`, `staminaCommit`, the maximum resolution with
   `unit.getMaxStamina`, the shiver debit through `commitStamina`, and the
   SD-20 nomad engagement in full. Tests T-S8-1 to T-S8-8. Depends on S4;
   independent.
   Delivered as #2834 (see S8.9, "S8a delivered").
8. **S8b. Exhaustion and sleep pressure.** Addendum S8, slice plan S8.7
   (S8b). Lua-only. Depends on S4; independent of S8a.
9. **S9. Cardio and circulation.** Addendum S9 (S9.1 to S9.8), with SD-21.
   Depends on S4; independent.
10. **S10. Blood volume and recovery.** Addendum S10 (S10.1 to S10.8), with
    SD-22. Introduces `bloodView` if it lands before S9. Depends on S4;
    independent.
11. **S11a. Brain and mental state: rules and atomic writes.** Addendum S11,
    slice plan S11.7 (S11a). Both systems are registered all-Simulated,
    with the derived rules and `unit.commitPhysio`. Depends on S4;
    independent.
12. **S11b. Mind and consciousness modes.** Addendum S11, slice plan S11.7
    (S11b): the SD-23 profiles and their behavior change. Depends on S11a.
13. **S12. Owner survey** of the remaining unit-specific data, which may add
   systems or stats. The epic closes only after the survey, with every
   system it lists given a mode.

## Addenda: remaining systems (S8 to S11)

**Owner-approved 2026-10-13**, with Codex review A1 to A4 READY at SHA-256
`a9ed0915` (`~/.local/state/syn-physiology-addenda-review-1.md` to `-4.md`)
and decisions SD-20 to SD-23.

Status: **owner decisions settled** (SD-20 to SD-23, 2026-10-13). Revised
after Codex review A1, and awaiting re-review (request
`owner-syn-physio-addenda-20261013-1`).

#### Addenda review A1 disposition

Review: `~/.local/state/syn-physiology-addenda-review-1.md` (syn-solver-21),
of the uncommitted copy with SHA-256 `63db6242`. Verdict NOT READY: 0
blocking, 7 major, 2 minor. I checked every claim against `efe796837`. All
are applied, and none is disputed.

| Finding | Disposition | Where |
|---|---|---|
| A1-1: the resolver went Simulated before the facade seeded | **Applied.** Verified that the admitted commit does no physiology (`Admission.hs:198-205`), that Wear inserts 0 on an absent pool (`Wear.hs:166`), and that combat can drain admitted attacks while Lua is paused (`Combat/Thread.hs:117-131`, `unit_resources.lua:112-113`). There are now separate **pending** (1) and **applied** (2) marker states. The engagement default applies only when the marker is applied. A facade-only verb, `unit.applyCombatEngagement`, seeds, pins and latches in one commit, respects explicit overrides, and is refused on a corpse. Pending-save and paused-load handling are defined. The free-swing bound is now "until the next successful application", not a time | S8.3, S8.5, T-S8-4 |
| A1-2: the `max_stamina` modifier misses the derived maximum | **Applied.** Verified that `maxStaminaFor`'s fallback applies only `endurance` modifiers (`Common.hs:88-95`), and Lua's derivation does the same (`unit_stats.lua:75-88`). The shared rule now applies `max_stamina` modifiers to the derived base as well. Lua reads Haskell's single resolution, so no mirror remains. No shipped content modifies `max_stamina` (grep of `data/`), so the change preserves behavior. A direct case: an endurance-1 nomad reads 15 before engagement, after it and after load | S8.2, T-S8-6 |
| A1-3: `commitStamina` arithmetic vs modifier-aware seeding | **Applied.** Verified the base-value add, clamp and reports (`Stamina.hs:135-170`) and the effective formula (`Unit/Stats.hs:68-77`). There is now one effective-unit stamina commit function for ticks, combat cost, shivering and seeding, with defined base bounds, effective reports for consequences, and a defined behavior for a −100 % modifier. It is identical to today's arithmetic when no modifier applies | S8.2, T-S8-7 |
| A1-4: T-S8-5 not reproducible; trigger-row oracles | **Applied.** T-S8-5 is split into a fixed-cadence budget case (B) and a deterministic bounded encounter (E), with pinned inputs, timing, RNG, stop condition, samples and a comparison unit (stamina fraction). T-S8-4 has separate positive and negative counts, plus the A1-1 cases. T-S11-3 pins the roll path through `mental.TUNE` (`mental_state.lua:68-78`). **My own finding while fixing A1-2:** heavy strikes cost 25 % of the maximum (`Wear.hs:142-169`), so +50 % max with 1.5× regen gave the acolyte's *fractional* dynamics exactly, which misses SD-20's intent. Regen is now 2.25×, so fractional recovery is 1.5× the acolyte's | S8.3, S8.6, S11.6 |
| A1-5: S10's blood accessor boundary | **Applied.** Verified the structured `getBlood` (`Units/Combat.hs:389-420`), the unit-thread movement multiplier (`Body.hs:51-75`, applied at `Motion.hs:51,110`) and the base-mass maxima (`Tick.hs:456-458,542-553`). There is now a named `bloodView` read interface and storage adapter. The maximum keeps **base** `body_mass` everywhere, as today. Movement, the wound tick, `getBlood`, the UI and S9's rule are all migrated, and the unmoded fallback is defined for either landing order | S9.2, S10.2, S10.3, S10.6 |
| A1-6: decals depend on the actual drain | **Applied.** Verified the conserved-loss accumulator (`Tick.hs:413-455`, `Blood/Trail.hs:138-161`). Under Nominal, the wound-derived external flow feeds the trail accumulator as a visual-only contribution, with no `uiBlood` debit. That is an explicit exception to the conserved accounting. Clot, bandage and heal cleanup are unchanged | S10.2, T-S10-1 |
| A1-7: S11 registry incomplete | **Applied.** Verified the timer sentinels and lazy initialization (`mental_state.lua:105-115,168-198,216-221,253-291`, `thoughts.lua:142-152`), the consumable writes (`consumable.lua:178-191`), caffeine's stamina regen (`unit_resource_tick.lua:283-289`) and the shared mental effectiveness (`Common.hs:97-128`, `Units/Combat.hs:498-512`, `Craft/Execute.hs:259`). Every owned key now has a Nominal read and a seed. Legacy episodes are handled. Incoming credits and caffeine's substitute are listed. S11a registers both systems with all-Simulated profiles, so its derived rules are live before S11b. The writer inventory is complete | S11.2, S11.3, S11.5, S11.6, S11.7 |
| A1-8 (minor): wrong nutrition pool | **Applied.** Now reads calories and hydration (`Healing.hs:148-151`) | S10.1 |
| A1-9 (minor): S8 sub-slices called ordered | **Applied.** S8a and S8b are independent; only S11b depends on S11a | Dependency order |

No new owner decision was needed. Every fix implements SD-20 to SD-23 as
the owner settled them.

#### Addenda review A2 disposition

Review: `~/.local/state/syn-physiology-addenda-review-2.md` (syn-solver-21),
of SHA-256 `1292c4c5`. Verdict NOT READY: 0 blocking, 4 major, 2 minor. I
checked every claim against `efe796837`. All are applied, and none is
disputed.

| Finding | Disposition | Where |
|---|---|---|
| A2-1: pending engagement, live refusal and explicit-Simulated save | **Applied.** Verified that modifiers are unrestricted (`Units/Stats.hs:393-449`) and that the save window is real (`Combat/Thread.hs:117-131`). A live unit with an unattainable seed returns `seedUnrepresentable`: it stays pending and Nominal, retries each pass with one diagnostic per reason, and never bypasses the modifier. The pending-Nominal, free-strike and load claims are qualified by "no explicit override". An explicit-Simulated pending save keeps its pool and latches without reseeding. The stale "single writer" claim is fixed. New T-S8-4 cases (n) and (o) | S8.3, T-S8-4 |
| A2-2: false effective reports from the commit | **Applied.** Verified that `effectiveStat` clamps the product (`Unit/Stats.hs:68-77`). Reports are now always the effective read of what is stored. The `m > 0`, `m = 0` and `m < 0` cases are defined, as are the honest above-maximum floor (`floored`) and D-5 refusal at seeding. New T-S8-8 | S8.2, T-S8-8 |
| A2-3: T-S8-5 could pass without a strike | **Applied.** Verified the reaches (`nomad_primitive.yaml:29`, `acolyte.yaml:44,400-404`, `steel_dagger.yaml:25`) and that the cooldown lives in the AI (`unit_ai_combat_attack.lua:261-273`). E now places the units 0.5 tiles apart, keeps the shipped cooldown rule, and has a reproducible seeded spawn. A validity gate (admitted strikes, engagement applied) makes cap expiry a fixture failure. B uses thermo Nominal for no shiver, a stated charge and sample order, and non-asserted approximate budgets | T-S8-5 |
| A2-4: T-S11-3 stochastic and vacuous | **Applied.** Verified severity weighting and the delirium skip (`mental_state.lua:71-77,276,281-292`). The acolyte is lucid with severity exactly 1 and chance 1, so BREAK is certain. The bear is lucid too, and is suppressed by the mode: the write gate refuses its stress inputs. Hypoxia collapse is a separate case | T-S11-3 |
| A2-5 (minor): the bear's hydration in the consumable test | **Applied.** The bear's hydration is overridden to Simulated at 50 %, and the acolyte's is pinned at 50 % | T-S11-6 |
| A2-6 (minor): bandage vs zero bleed | **Applied.** Marks stop only at exactly zero external bleed (a sealed bandage or full clotting). A competent dressing at seep 0.05 keeps reduced marks (`Units/Medical.hs:48-50`, `Wounds/Bleed.hs:51-59`) | T-S10-1 |

No new owner decision was needed.

#### Addenda review A3 disposition

Review: `~/.local/state/syn-physiology-addenda-review-3.md` (syn-solver-21).
Verdict NOT READY: 1 major. I checked the claim against D-5 (`design:534-540`,
the guards `1 + Σp ≤ 0`, `b < 0`, non-finite `b`).

| Finding | Disposition | Where |
|---|---|---|
| A3-1: commit success used as transition eligibility | **Applied.** Confirmed that revision A2 let the ordinary solver's `ok` admit an `m < 0` seed, against D-5. Commit status and transition eligibility are now separate. D-5's explicit guards run before any seed, override or applied marker is published, in both the facade transition and `applyCombatEngagement`. Ordinary negative-multiplier arithmetic is kept for already-Simulated commits. The reviewer's concrete case is a new T-S8-8 row | S8.2, S8.3, T-S8-8 |

Evidence is from
reading the source at `efe796837`. No engine, probe or build was run.

Each addendum reuses the v1 machinery unchanged: the registry and resolver
(D-1, D-3), the three-valued read contract with SD-19 purity (D-2), the
modifier-aware seeding and transitions (D-5), on-demand derived keys and
observation boundaries (D-5a), the overrides field and load policy (D-6),
the facade and access audit (D-7), Nominal display (D-8), and the profiles
(D-10). An addendum states only what is new.

**Dependency order.** S8, S9, S10 and S11 each depend on S4 (#2831), which
activates the machinery, and on nothing else in this list. All four are
**independent** of one another:

- S9's cardio reads blood. S10's blood recovery reads nutrition and
  hydration. S11's rules read stamina, blood oxygen, core and salt.
- Every one of those cross-reads goes through the accessor. A not-yet-moded
  system reads as an unowned pass-through key with today's semantics
  (D-1). The same read returns the Nominal substitute once that system
  lands.
- So no addendum's correctness depends on another landing first, and
  processing can proceed past a deferral of any of them.
- Within the sub-slices, S8a and S8b each depend only on S4 and may land
  in either order. **S11b depends on S11a.**
- Where a system's state is not an ordinary `uiStats` key (blood's
  `uiBlood` field), or where its rules are introduced (S11a), the
  *introducing* slice also migrates the existing consumers it touches. The
  accessor alone does not carry those. S10.2 and S11.7 spell this out.

### S8. Stamina, exhaustion and sleep pressure

#### S8.1 Current state

| Key | Writers (thread) | Readers (thread) |
|---|---|---|
| `stamina` | Lua tick, through `unit.commitStamina`, which adds a relative delta in one `atomicModifyIORef'` and reports `before/after/maximum` (`src/Engine/Scripting/Lua/API/Units/Stamina.hs:1-99`; caller `scripts/unit_resource_tick.lua:302-324`) (Lua worker). Combat strike cost `staminaDrainStats` (`src/Combat/Resolution/Wear.hs:164-174`), committed in `commitIfAdmitted` (combat worker). **Thermo shiver debit, as a raw read-modify-write** (`scripts/thermo.lua:218-222`) (Lua worker) | Combat work scaling `staminaFrac` (`src/Combat/Resolution/Damage.hs:251-254`) (combat worker); `maxStaminaFor` (`Combat/Resolution/Common.hs:89-90`); brain mood and concentration (`scripts/brain.lua:251`); AI wander gate (`unit_ai_needs.lua:36-42`), pacing (`unit_ai_pace.lua:27-28`), lunge (`unit_ai_combat_lunge.lua:147-149`), attack (`unit_ai_combat_attack.lua:327-328`); UI row and "Collapsed (exhausted)" (`unit_info_v2_status.lua:128-129,329-330`) |
| `exhaustion` | Lua pool tick (`unit_resource_config.lua` `exhaustion` entries) | speed multiplier (`scripts/exhaustion.lua:37-55`, applied in `movement_speed.lua:95`); sleep utility term (`unit_ai_sleep.lua:201-205`). No UI row |
| `sleep_pressure` | Lua pool tick (drain while awake, regen while `sleeping`) | `go_to_sleep` utility and wake (`unit_ai_sleep.lua:165-205`), registered for acolytes (`unit_ai.lua:196`) and bears (`bear_ai.lua:315`), **not** squirrels; circadian shape (`circadian.lua`); UI row (`unit_info_v2_status.lua:292,337`) |
| `stance` | Combat spend (`Wear.hs:172-174`); Lua recovery through `unit.recoverStance`, relative and atomic (#2468, `Units/Stance.hs`; `unit_resource_injury.lua:39-52`) | combat |

Effects and deaths:

- Stamina collapses below 0.1 and revives at 0.5, and **`kill_on_zero`**
  applies to acolyte, bear and squirrel (`unit_resource_config.lua:40-49`,
  `:235-237`, `:282-284`).
- The fat-floor organ-failure drain inside the stamina tick belongs to
  nutrition (SD-5, D-9).
- Exhaustion and sleep pressure never kill or collapse.
- A sleeping pose speeds healing (`src/Combat/Wounds/Tick.hs:223`).

Defects and gaps found (source reading, not runs):

- **S8-F1. Species without a stamina pool are permanently winded after
  their first swing.** `nomad_primitive` and `technomule` have no stamina
  config. The strike cost reads an absent pool as **0**
  (`Wear.hs:166`, `HM.lookupDefault 0.0`), so the first swing inserts
  `stamina = 0`. Nothing regenerates it. Every later swing then works at
  the 0.3 floor (`Damage.hs:251-254`), while the first swing read the
  absent pool as full.
- **S8-F2. The thermo shiver debit is the #2470 race.**
  `thermo.lua:218-222` reads stamina and writes back an absolute value on
  the Lua worker while combat commits strikes on the combat worker, so a
  strike landing in between is refunded. S5's migration list (#2830) names
  thermo's *sweat* debits but not this one. S8 routes it through
  `commitStamina` (S8.3).

#### S8.2 Proposed registry entries

The three pools have disjoint consumers, so they are **three systems**:
`stamina`, `exhaustion` and `sleep`. `stance` stays unowned and always-on:
it is combat readiness, not survival, the same footing #2754 gave combat
physiology.

| System | Owns (Nominal read; seed target) | Meters | Requires | Provides → Nominal substitute | Contributes → substitute | Effects (none while Nominal) |
|---|---|---|---|---|---|---|
| stamina | `stamina` (= effective `max_stamina`) | — | body | stamina fraction → 1.0, to combat work, brain, AI gates and pacing | — | collapse below 0.1, `kill_on_zero`, combat strike cost |
| exhaustion | `exhaustion` (= effective `max_exhaustion`) | — | body | restedness fraction → 1.0, so speed multiplier 1.0 and sleep-utility term 0 | — | speed penalty |
| sleep | `sleep_pressure` (= effective `max_sleep_pressure`) | — | body | sleep deficit → 0, so `go_to_sleep` utility −∞ and no pressure wake | — | sleep need |

Incoming contributions:

- The **combat strike cost** into a Nominal stamina is **discarded**.
  `staminaDrainStats` consults the pure resolver for the attacker's stamina
  mode and writes no `stamina` key when Nominal. This keeps the storage
  invariant: no absent-as-0 insert.
- `Damage.hs` reads Nominal stamina as 1.0, through Haskell `physioRead`.
- Thermo's shiver debit into Nominal stamina is discarded.

Every stamina write keeps going through a relative, atomic engine verb:

- `physiology.debit` / `credit` on `stamina` call `unit.commitStamina` with
  the delta, never `setStat`.
- The Simulated → Nominal and Nominal → Simulated transitions for stamina
  run inside the same unit-manager `atomicModifyIORef'` that combat commits
  against (D-5 already requires one atomic update). A concurrent strike is
  therefore ordered entirely before or entirely after the transition. A
  strike before the switch to Nominal is scrubbed with the pool. A strike
  after the switch to Simulated spends from the freshly seeded pool.
- Under Nominal, `unit.commitStamina` returns its documented refusal with
  the reason "nominal", and mutates nothing (the "refusal mutates nothing"
  property, `Stamina.hs`). The tick never calls it then, because the pool
  entry ticks only when Simulated.
- `recoverStance` is unaffected.

**Maximum resolution (A1-2).** `maxStaminaFor` (`Common.hs:88-95`) has two
branches:

- An explicit `max_stamina` attribute: the effective value, as today.
- **No explicit attribute:** `effectiveStat(now, effective(endurance) × 10,
  mods on max_stamina)`. Today's fallback ignores `max_stamina` modifiers;
  the new one applies them to the derived base.

Lua's `stats.get(uid, "max_stamina")` calls a new pure read verb,
`unit.getMaxStamina(uid)`, which returns exactly `maxStaminaFor`. That
replaces the hand-kept Lua mirror (`unit_stats.lua:75-88`, "change in
lockstep"), so Lua reads, combat cost, commit bounds and seed targets share
one resolution. No shipped content targets `max_stamina` (grep of `data/`),
so nothing changes for existing units.

**Stamina write contract (A1-3).** One pure Haskell function,
`staminaCommit now inst Δ`, is used by:

- `unit.commitStamina` (pool ticks, and the facade's `debit` / `credit` on
  `stamina`, including the shiver debit);
- the combat strike cost in `Wear.hs`, as Δ = −cost;
- transition seeding.

It runs inside the caller's single `atomicModifyIORef'`, so strikes are
never refunded. The contract:

- **Units.** Δ, the reports and every consequence (collapse, `kill_on_zero`,
  alerts) are **effective** stamina, against the effective maximum
  `maxStaminaFor`.
- **Arithmetic (revised for A2-2).** It keeps the effective formula exactly
  (`src/Unit/Stats.hs:68-77`):
  - `eff(b) = max 0 ((b + Σδ)·m)`, where `m = 1 + Σp` and `Σδ`, `Σp` are
    stamina's active modifiers at `now`.
  - Modifiers carry no sign or range restriction
    (`Units/Stats.hs:393-449`), so `m` may be positive, zero or negative.
- **Every report is a true read.** `before = eff(b)`, and `after =
  eff(b')`, where `b'` is what is actually stored. Collapse, `kill_on_zero`
  and alerts (`unit_resource_tick.lua:302-324,357-394`) use exactly these.
  The commit never reports a value that differs from what an effective
  read of the stored state returns.
- **Target.** `e' = clamp (before + Δ) 0 max`. The commit then stores the
  admissible base whose effective value is nearest to `e'`:

  | Case | Behavior | Status |
  |---|---|---|
  | `m > 0` | `eff` is non-decreasing in `b`, so `b' = e'/m − Σδ`. If that is < 0, `b' = 0`. That happens when a positive flat floor `eff(0) = max 0 (Σδ·m)` exceeds `e'`. | `ok`, or `floored` |
  | `m = 0` | `eff ≡ 0`, so `b` is unchanged and the reports are 0 (true). | `zeroMultiplier` |
  | `m < 0` | `eff` is non-increasing in `b`. `b' = e'/m − Σδ` is valid when ≥ 0; otherwise `b' = 0`, the base nearest an attainable value. | `ok`, or `floored` |

  Every row stores a finite `b' ≥ 0` and reports `eff(b')`.
- **Above-maximum floors are reported honestly.** If a flat modifier floors
  the pool above `max` (for example `Σδ = 15` against a maximum of 10, with
  `m = 1`), the commit stores `b' = 0` and reports `after = 15`, status
  `floored`. Effective reads show 15 too. No collapse or death fires,
  because the true pool is high. This can arise after activation, from a
  modifier added to an already Simulated unit, and is handled per commit.
- **Refusals** are reserved for invalid calls: an unknown or dead unit, a
  non-finite Δ, or a Nominal stamina receiver. They mutate nothing and are
  named, as `Stamina.hs` does today.
- **Transition eligibility is separate from commit status (A3-1).** An
  ordinary commit's status (`ok`, `floored`, `zeroMultiplier`) only
  describes how an *already Simulated* pool was updated. It never decides
  whether a transition may seed. Before publishing **any** seed, override
  or applied engagement marker, both the facade transition
  (`setPhysioMode`, `clearPhysioMode`) and `unit.applyCombatEngagement`
  apply D-5's explicit guards to the seed target `t`:
  - `m = 1 + Σp ≤ 0` → refuse;
  - otherwise compute `b = t/m − Σδ`, and refuse if `b < 0` or `b` is not
    finite.

  A refusal is atomic: no key, mode, override or marker changes. Only when
  the guards pass is `b` stored as the seed. So a negative multiplier is
  **always** refused at a transition, even where the ordinary solver would
  reach the target (for example `t = 10`, `Σδ = −10`, `Σp = −2`, where `b'
  = 0` reads 10 and an ordinary commit would report `ok`). A flat floor
  above the target is refused by the `b < 0` guard. The ordinary
  negative-multiplier arithmetic above still applies, unchanged, to commits
  on a pool that is **already** Simulated.
- **No modifier** (`Σδ = Σp = 0`, which covers all shipped content): this is
  exactly today's base arithmetic, always status `ok`.

#### S8.3 Interactions

- **Nutrition (v1):** the organ-failure drain is already nutrition-owned
  and gated (D-9). With stamina Nominal and nutrition Simulated, a unit at
  its fat floor has no stamina pool to drain. Nutrition's lean-floor kill
  still applies, so starvation death survives.
- **Thermo (v1):** shiver costs stamina through `commitStamina` (fixes
  S8-F2). With thermo Nominal there is no shiver.
- **S11:** brain reads the stamina fraction (1.0 when Nominal), and the
  sleep utility reads exhaustion.
- **Tick order:** unchanged. Pools tick after injuries and failure meters
  (`unit_resources.lua:165-198`), and stance recovery first (`:130`).
- **Threads:** the only cross-thread key is `stamina`, and every writer uses
  an atomic relative commit. `exhaustion` and `sleep_pressure` are Lua-only.

**Combat engagement trigger (SD-20, revised for A1-1).** Nomads spawn with
stamina Nominal and switch to Simulated, one way, at their first combat
engagement.

- **Declaration.** The nomad's `physiology:` block declares
  `stamina: nominal` and `on_combat_engagement: { stamina: simulated }`.
  Definition loading checks that the named system exists and is Nominal by
  default. Only `nomad_primitive` declares it.
- **What counts as engagement.** An attack attempt **admitted** by
  `Combat.Resolution.Admission.commitIfAdmitted`
  (`src/Combat/Resolution/Admission.hs:183-205`), hit or miss. It counts
  for both units in that commit: the attacker and the target.
  - It does **not** count: an attack refused at admission, being in range,
    being chosen as an AI target, or an ally's strike on someone else.
  - A hit is a subset of an admitted attempt, so it needs no separate rule.
- **The marker has two states:** `combat_engaged = 1`, **pending**, and
  `= 2`, **applied**. It is an unowned, durable `uiStats` key, absent until
  engagement.
- **Detection, on the combat thread.** Inside that same atomic commit, for
  each unit whose species declares the trigger, the commit sets the marker
  to 1 if it is absent.
  - It never touches a marker that is already 1 or 2, so a burst of
    attempts is idempotent.
  - It creates no stamina and changes no mode.
  - Absent an explicit override, the effective mode is still Nominal, so
    the strike's stamina cost is discarded (S8.2) and no absent-as-0 insert
    happens.
  - With an **explicit Simulated override**, the strike is charged
    normally, because the effective mode is Simulated.
  - The marker's writers are this commit (absent → 1) and
    `applyCombatEngagement` (1 → 2). Both are atomic on the same reference,
    so no write is a cross-thread raw write.
- **The resolver** uses the engagement default only once the marker is
  applied:

  > effective = override, else (`combat_engaged == 2` ? engagement default
  > : species default)

  Absent an explicit override, a **pending** unit therefore stays Nominal.
  With an explicit override, the override decides, pending or not. The
  storage invariant holds throughout: no Simulated mode exists without its
  seeded pool.
- **Application, by the Lua facade.** At the start of each physiology pass
  (it runs only while unpaused and not torn down,
  `unit_resources.lua:112-113`), the facade calls a new facade-only
  mutating verb, `unit.applyCombatEngagement(uid)`, for each unit with
  `combat_engaged == 1`. In **one** `atomicModifyIORef'`, the verb:
  - **refuses** a dead unit (D-5) and leaves the marker pending. A corpse is
    never revisited, because the corpse branch of `unit_resources.update`
    skips it (`:129`). A target killed by the triggering strike therefore
    never transitions.
  - for an explicit stamina override, of either mode: sets the marker to
    applied **without** a transition. Explicit intent wins (SD-7).
  - otherwise performs the stamina transition from Nominal to Simulated:
    it seeds the full effective maximum through `staminaCommit`, records
    the **pinned** override, and sets the marker to applied, all together.
  - **a live unit whose seed fails D-5's guards**: `m ≤ 0`, as with a
    retained −100 % or more negative stamina modifier, or a computed base
    that is negative or non-finite (A3-1). The verb **refuses** atomically
    with `seedUnrepresentable`, before any seed, override or marker is
    written. The marker stays pending and the unit stays Nominal.
    - SD-8 ignores the modifier under Nominal, so the unit keeps fighting at
      full work. The refusal is retried on every eligible pass, because it
      is cheap and side-effect free. It succeeds on the first pass after the
      modifier no longer blocks the seed.
    - A diagnostic is logged **once per unit per refusal reason**, not every
      pass.
    - The facade never bypasses the modifier and never forces a partial
      seed.

  The verb returns `transitioned`, `latched`, `refusedDead` or
  `seedUnrepresentable`. On `transitioned`, the facade runs the hooks and
  refresh (D-5a).
- **The free-swing guarantee, scoped.** Without an explicit override, a
  strike admitted while the marker is pending is free: its cost is
  discarded, and `Damage.hs` reads full work. Pending ends at the next
  **successful** application. That is normally the next eligible pass. It
  is later only while a live unit's seed is unattainable, as above, which
  may be indefinitely if the blocking modifier is permanent. That is the
  intended consequence of SD-8 and D-5, not a leak. A dead unit's pending
  marker never resolves, and needs nothing further. With an explicit
  Simulated override, no strike is free.
- **One-way.** Once applied, `clearPhysioMode` returns to the engagement
  default, Simulated. If the effective mode does not change, it neither
  seeds nor runs hooks (D-5). An explicit scripted Nominal override still
  wins. Clearing that override later goes to Simulated, with a seed.
- **Save/load and paused load.**
  - A pending marker persists. On load, normalization (D-6 (b)) runs
    against the unit's **effective** mode:
    - with no explicit override, that is Nominal, so no stamina key is
      expected;
    - with an **explicit Simulated** override, it is Simulated, and the
      existing partial pool is **preserved** exactly.

    The first eligible pass after load then applies the marker. Without an
    override it transitions; with one it **latches** without reseeding.
  - Combat may admit attacks at a save boundary while Lua is paused
    (`Combat/Thread.hs:117-131`). That only creates pending markers, which
    is safe.
  - An applied marker and its pinned override both persist exactly.
- **Rejected alternative:** a new combat-to-Lua notice queue. It would add
  EngineEnv or capability state (§2.1, §6.4), it would be transient, so a
  save in between would lose it, and it would need ordering rules against
  the drained combat-log streams.

**Nomad stamina tuning (SD-20 owner intent, retuned for A1-4).** Nomads
should have a lot of stamina and never die of exhaustion in an ordinary
encounter. They get a `nomad_primitive` entry in `unit_resource_config.lua`,
with **acolyte semantics including `kill_on_zero`**, and these parameters:

- an innate `max_stamina` modifier of **+50 %** through the definition's
  `modifiers:` block, effective through the A1-2 resolution, so an
  endurance-1 nomad's maximum is 15;
- **every regen-side factor at 2.25× the acolyte's**: `regen_factor_idle`,
  `regen_factor_walking`, `regen_factor_collapsed`,
  `regen_factor_crouching` and `move_regen_factor`;
- the acolyte's collapse and revive thresholds.

The 2.25× follows from how strikes cost. Combat cost is a **fraction of the
maximum** (Quick 5 %, Heavy 25 %, `Wear.hs:142-169`), so a larger maximum
alone buys no fight endurance. Recovery must grow faster than the maximum.
2.25 = 1.5 × 1.5 gives 1.5× the acolyte's recovery *as a fraction of the
pool*, on top of the larger absolute pool. Under flat drains, such as
shivering, the larger pool also helps.

These are starting values, calibrated in S8a against T-S8-5. A nomad dying
of exhaustion in an ordinary encounter is a tuning defect.

Out of scope for S8, recorded as owner intent and to be tracked
separately: nomads should fight **physically more effectively than an
average human**. Strength, damage and the like are not stamina.

#### S8.4 Profile columns

| Species | stamina | exhaustion | sleep | Change from today |
|---|---|---|---|---|
| acolyte | Sim | Sim | Sim | none |
| bear_brown | Sim | Sim | Sim | none |
| red_squirrel | Sim | Sim | **Nominal** | none in play: its pool drains, but nothing reads it (no `go_to_sleep`); the status row shows Nominal |
| technomule | **Nominal** | Nominal | Nominal | fixes S8-F1 (never winded); it has no pools today |
| nomad_primitive | **Nominal → Sim on first engagement** (SD-20) | Nominal | Nominal | fixes S8-F1 before the first fight; afterwards an acolyte-like pool with generous parameters, including exhaustion death |
| bodyless | Unsupported | Unsupported | Unsupported | as SD-13 |

#### S8.5 Persistence

No wire change and no units v5. The owned keys already ride `uiStats`, and
the new system ids are override keys in the v4 `uiPhysioOverrides` map.
`PhysioMode` is unchanged.

- **`combat_engaged`** (SD-20) is an unowned, durable `uiStats` key, *Persist
  exactly*, with the values 1 (pending) and 2 (applied). Absent means never
  engaged. Its writers are the combat commit (absent → 1) and
  `applyCombatEngagement` (1 → 2). It needs an inventory row.
  - A legacy save lacks it.
  - A non-finite or out-of-set value is malformed, and rejects the load
    under D-6 (h).
- A legacy nomad whose stray `stamina = 0` came from S8-F1 has no marker.
  Its effective mode is Nominal, so normalization (b) scrubs the key. Its
  first engagement after load seeds a full pool.
- Other profile changes normalize as D-6 (b): a squirrel's
  `sleep_pressure` and a technomule's stray `stamina = 0` are scrubbed.
- The registry, profile and engagement declarations are content (*Rebuild
  deterministically*).

#### S8.6 Tests (linear)

- I1, I2 and I3 per new system: three neutrality cases, plus edges stamina →
  brain, exhaustion → sleep and nutrition → stamina.
- **T-S8-1 concurrency:** combat strikes interleaved with a stamina
  transition and with ordinary ticks.
  - The pool equals a serial order of spends and recoveries.
  - No strike is refunded, including the shiver debit (the S8-F2
    regression).
  - Under Nominal, no `stamina` key ever appears.
- **T-S8-2:** S8-F1. A Nominal technomule, or a nomad before engagement,
  swings repeatedly at full work, and no `stamina` key is created.
- **T-S8-3:** stance recovery unchanged across stamina modes.
- **T-S8-4, SD-20 trigger matrix** (A1-1, A1-4).

  **Positive rows**, each expecting marker 1 after the commit, then exactly
  one `transitioned` result at the next pass: a pinned Simulated override, a
  full effective seed, and marker 2.

  | Event | Unit |
  |---|---|
  | (a) a nomad's admitted attack hits | the attacker |
  | (a′) a nomad's admitted attack misses | the attacker |
  | (b) an admitted attack hits a nomad | the target |
  | (b′) an admitted attack misses a nomad | the target |

  **Negative rows**, each expecting **zero** markers and zero transitions:

  - (c) an attack refused at admission;
  - (d) being in range, or chosen as an AI target, with no admitted attempt;
  - (e) an admitted attempt between acolytes, or another non-declaring
    species.

  **State cases:**

  | Case | Expected |
  |---|---|
  | (f) five admitted attempts within one pass | one marker write, one transition |
  | (g) strikes admitted after the marker and before the next successful application | cost-free, with no `stamina` key; the first strike after application is charged |
  | (h) a pending save | after load: still pending, effective Nominal, no stamina key; normalization makes no stamina change; the first eligible pass transitions it |
  | (i) an explicit Nominal override before engagement | marker 1 → 2 with `latched` and no transition; the unit stays Nominal; clearing that override transitions to Simulated with a seed |
  | (j) an explicit Nominal override after engagement | wins |
  | (k) `clearPhysioMode` after engagement | stays Simulated, with no reseed and no hooks |
  | (l) the target is killed by the triggering strike | marker 1 on the corpse; application refused as dead; no transition and no repeated attempts |
  | (m) a second engagement after the marker is applied | no change |
  | (n) a **living** nomad with a retained −100 % stamina modifier is engaged (A2-1) | marker 1; each pass returns `seedUnrepresentable`, with one diagnostic, not one per pass; it stays Nominal at full work; once the modifier is removed, the next pass transitions with a full seed |
  | (o) an explicit Simulated override with a partial pool (stamina 4 of 10), marked pending, then saved and loaded before application (A2-1) | the pool is preserved at 4 through normalization; the first pass after load latches the marker to 2 without reseeding; the triggering strike was charged |
- **T-S8-5, SD-20 tuning acceptance** (A1-4, revised for A2-3). Two
  deterministic cases. The comparison unit is the **stamina fraction**
  (effective stamina ÷ effective maximum). Both cases run with **thermo
  Nominal by override** for both units, so there is no shiver at all
  (`thermo.lua:146-154,175-199` derive shivering from core, fever and
  heat balance, not from ambient alone).
  - **B, fixed-cadence budget.**
    - Fixture: an acolyte and an engaged nomad, both at endurance 1 with no
      other modifiers. Their maxima are 10 and 15.
    - Inputs: no wounds or pain; activity held `idle`; dt = 0.1 s.
    - At t = 1, 2, 3, … s, each unit is charged by the real
      `spendStrikeCost`: Heavy when t is a multiple of 4, Quick otherwise.
      That is 40 % of the maximum per 4 s.
    - **Ordering:** each charge is applied after that tick's physiology
      pass, and each sample is taken after the charge, at every whole
      second. Both pools start full, so regen is clipped until the first
      charge.
    - The window is 30 s.
    - Approximate steady-state budgets: the acolyte −0.20 per 4 s (idle
      regen 0.05/s), the nomad −0.10 per 4 s (0.075/s). The acolyte
      therefore crosses 0.1 at about 18 s, and the nomad stays near 0.25
      at 30 s. These figures are a guide, **not asserted**: start-up
      clipping and the Heavy phase shift the transient.
    - **Asserts:** the nomad's fraction stays ≥ 0.1 at every sample, and is
      at or above the acolyte's at every sample.
  - **E, bounded encounter.**
    - **Reproducible spawn:** before spawning, the fixture writes a fixed
      `StdGen` into the stat RNG and every other RNG ref combat or spawn
      draws from (`gotcha_headless_entropy_seeded_engine_rng`).
    - **Units:** one nomad from its definition with its default loadout
      (unarmed reach 0.75 tiles at base height, `nomad_primitive.yaml:29`),
      and one acolyte from the standard spawn profile with its dagger
      (reach about 0.97 tiles, `acolyte.yaml:44,400-404`,
      `steel_dagger.yaml:25`).
    - **Placement:** on a flat arena, both units face each other **0.5
      tiles apart**, inside both reaches, and are held there. There is no
      approach phase, and AI closing movement is disabled.
    - **Attack schedule:** both units' AI is replaced by a scripted
      schedule that keeps the shipped **attack cooldown rule**
      (`computeAttackCooldown`, and the `stance ≥ 0.25` gate,
      `unit_ai_combat_attack.lua:261-273`). Whenever that rule permits, a
      unit issues an attack, every fourth one Heavy and the rest Quick.
    - **Stop:** at the first collapse or death of either unit, or at 60 s.
    - **Validity gate, before the stamina oracle is read:**
      - each unit has at least one **admitted** strike;
      - the nomad's marker reached 2 by `transitioned`.

      Reaching 60 s with either unmet is a **fixture failure**, not a pass.
    - **Asserts**, given validity: the nomad's fraction never falls below
      0.1 at any 0.5 s sample, and the nomad never dies of exhaustion.

  Failing B or E (given a valid E) means the parameters are retuned. The
  acceptance is not relaxed.
- **T-S8-6, maximum resolution** (A1-2). An endurance-1, otherwise
  unmodified nomad reads `max_stamina = 15`:
  - from `stats.get`, from `maxStaminaFor` and from the commit bound;
  - before engagement, after engagement and after a save and load.
- **T-S8-8, commit boundary arithmetic** (A2-2):
  - **Negative multiplier and negative flat:** `b = 0`, `Σδ = −10`,
    `Σp = −2`, so `m = −1`. Reads give effective 10. A Heavy-sized
    `Δ = −2.5` stores `b' = 2.5` and reports 10 → 7.5, matching the
    effective read afterwards.
  - **Flat floor above the maximum:** maximum 10, `Σδ = 15`, `m = 1`. Any
    commit stores `b' = 0` and reports `after = 15`, status `floored`, with
    no collapse or death. A transition seed under it is refused.
  - **Zero multiplier:** `m = 0` reports 0, matching the read, status
    `zeroMultiplier`.
  - **Transition refused although the solver would succeed** (A3-1): target
    10, `Σδ = −10`, `Σp = −2` (`m = −1`).
    - Already Simulated at `b = 0`, which reads 10 (the highest value
      reachable with these modifiers), an ordinary commit targeting 10
      stores `b' = 0`, reads 10 and returns `ok`, which is correct.
    - A **Nominal → Simulated** `setPhysioMode` with the same modifiers is
      **refused** atomically: no seed, override or mode change.
    - So is an engagement application. `applyCombatEngagement` returns
      `seedUnrepresentable`, and the marker stays 1 with no pinned
      override.
- **T-S8-7, the effective stamina contract** (A1-3). With a retained −50 %
  stamina modifier:
  - the transition seeds base 20 for effective 10;
  - a zero commit leaves effective stamina at 10;
  - full-pool regen stays at effective 10;
  - a Heavy strike reports effective 10 → 7.5;
  - a shiver debit of 0.1 reports effective −0.1.

  With +50 %, regen never pushes effective stamina above the maximum. With
  −100 %, the transition is refused, and an existing Simulated pool reports
  0.

#### S8.7 Slice plan

- **S8a: stamina.**
  - Haskell `Wear` and `Damage` become mode-aware.
  - `staminaCommit` (A1-3) and the A1-2 maximum resolution, with
    `unit.getMaxStamina`.
  - The facade debit goes through `commitStamina`, and the shiver debit is
    migrated.
  - SD-20 in full: the declaration, the two-state marker in the combat
    commit, `unit.applyCombatEngagement`, the nomad config and the innate
    modifier.
  - The stamina profiles.
  - Tests T-S8-1 to T-S8-8.
- **S8b: exhaustion and sleep.** Lua-only.
- S8a and S8b are **independent** of each other. Both need S4.

#### S8.8 Decisions (settled)

- **SD-20 (S8-D1, nomad stamina): the owner's own design.**
  - Nomads spawn with stamina Nominal and switch to Simulated, like an
    acolyte's including exhaustion death, at their first combat engagement.
  - The switch is a standard, pinned, one-way S4 transition.
  - Parameters are generous, and the trigger and application are as in
    S8.3. None of the drafted options (a) to (c) was taken.

#### S8.9 S8a delivered (#2834)

What the slice built, as the code now states it. Where an implementation
choice fills a gap the text above leaves, it is named here.

- **Registry.** `stamina` is the fifth system (`data/physiology/systems.yaml`,
  last in registry order): requires `body`, owns `stamina`, no meters,
  provides `stamina_fraction` (1.0 under Nominal) and has the effects
  `collapse`, `kill_on_zero` and `strike_cost`, each absent under Nominal. Its
  healthy rule is the NAMED rule `{ rule: max_stamina }` (`HealthyMaxStamina`),
  because `max_stamina` is derived and never stored, so a `from:` rule could not
  read it, and the review forbade storing it (an explicit attribute would then
  switch every unit to the explicit branch). The healthy read and the seed
  target resolve through the same rule as `maxStaminaFor`.
- **One maximum.** `Unit.Stats.maxStaminaOf` is the rule: an explicit
  `max_stamina` wins (its own modifiers apply); otherwise
  `effectiveStat now (effective(endurance) x 10) (modifiers on max_stamina)`.
  `maxStaminaFor`, the registry's healthy value, `unit.getMaxStamina` (a pure
  read verb, `nil` for a unit with no endurance) and `stats.get(uid,
  "max_stamina")` all call it, so the hand-kept Lua mirror is gone.
- **One commit rule.** `Combat.Resolution.Stamina.staminaCore` is the pure
  arithmetic of S8.2 (the m > 0, m = 0 and m < 0 rows, floored and
  zeroMultiplier statuses). `unit.commitStamina` (pool tick, facade debit and
  credit, shiver) and the combat strike cost both use it; a transition's seed
  uses the same `t / m - d` solve through `invertEffectiveStatWhy` with D-5's
  guards. The verb now returns `{ before, after, maximum, status }` (all
  effective) and refuses by name: `nominal` and `unsupported` (distinct),
  `missing_pool` (a Simulated receiver without a pool is a broken invariant;
  the old first-observation fill is gone), `dead` (the combat lethality rule),
  `invalid_amount`, `invalid_stamina`, `invalid_maximum`, `no_such_unit`.
- **Combat.** `Wear` charges only a Simulated pool that exists (a Nominal or
  Unsupported attacker creates no key; stance is spent in every mode) and
  `Damage` reads `staminaWorkFraction` (1.0 for Nominal, Unsupported or an
  absent pool; the effective fraction with the 0.3 floor for Simulated).
  `Combat.Resolution.runResolution` reads the registry once per strike.
- **Engagement.** `UnitPhysiology` gains `upEngagement`; the YAML block is
  `on_combat_engagement: { stamina: simulated }`, validated at registration
  (the system exists, is stamina, is Nominal by default, mode `simulated`). The
  resolver takes a fourth input, the unit's marker: override, else the
  engagement default only when `combat_engaged == 2`, else the species default.
  `commitIfAdmitted` stamps the absent marker to 1 for attacker and target in
  the strike's own update; `unit.applyCombatEngagement` (facade-only) applies
  it: dead (the combat lethality rule, not the pose mirror) refuses and stays
  pending, an explicit override of either mode latches, otherwise it is
  `planSetMode` Simulated (so D-5's guards are a scripted `setMode`'s) with
  marker 2 in the same update, or `seedUnrepresentable` with a reason code
  (`nonpositive_multiplier`, `negative_base`). The facade logs a refusal once
  per unit and reason; the debounce is cleared with the other session caches.
  The marker is never writable by `unit.setStat` and never a definition
  template.
- **Nomad parameters.** `nomad_primitive` gets the acolyte's stamina entry with
  `regen_factor_idle`, `_walking`, `_collapsed`, `_crouching` and
  `move_regen_factor` scaled 2.25 (derived from the acolyte's table, so the two
  cannot drift) and an innate +50% `max_stamina` modifier. Side effect worth
  the owner's attention: `move_regen_factor` is also the speed-drain effort
  coefficient (`supply = move_regen_factor x endurance`, `drain = supply x
  ratio^2`), so an engaged nomad's fractional drain above comfort speed is 1.5x
  the acolyte's too; T-S8-5 holds both units stationary and cannot see it.
  Calibration (T-S8-5): B passes with the starting values, the nomad holding at
  or above both 0.1 and the acolyte's fraction at every sample (about 0.25 at
  30 s against the acolyte spent by 20 s); E is valid and passes, ending at the
  nomad's wound collapse (about 13.6 s) with its fraction never below 0.76. No
  parameter was retuned.
- **Persistence.** No wire change. `combat_engaged` is Persist exactly (absent,
  1, 2); a value outside the set or non-finite rejects the load (D-6 (h));
  stray stamina on a Nominal unit (a legacy nomad's `stamina = 0`, the mule's)
  is scrubbed (D-6 (b)); modifiers are not migrated, so a nomad saved before
  this slice keeps a maximum of 10. Combat may admit an attack at a paused save
  boundary, which only creates a pending marker.
- **Consumers.** Brain, AI wander, pacing, lunge and attack, `checkRevive` and
  the unit-info panels read stamina through the facade (a Nominal system reads
  full, an Unsupported one nothing). The Status row and the legacy debug text
  follow the S6 contract: Nominal shows the word and the neutral tooltip, an
  Unsupported unit hides the row. The access audit gains stamina ownership
  (the pool engine), the facade-only `applyCombatEngagement` and the atomic
  `commitStamina` seam (facade and pool engine only).

#### S8.10 S8b delivered (#2835)

What the slice built, as the code now states it. Lua and content only: no
Haskell source changed.

- **Registry.** `exhaustion` and `sleep` are the sixth and seventh systems
  (`data/physiology/systems.yaml`), each requiring `body` with no meters.
  `exhaustion` owns `exhaustion` and provides `restedness_fraction` and
  `movement_multiplier` (both the constant 1.0 under Nominal), contributes a
  zero `sleep_exhaustion_deficit` to the sleep utility, and has the effect
  `speed_penalty`. `sleep` owns `sleep_pressure` and provides `sleep_deficit`
  (0.0), with the effects `sleep_need` and `pressure_wake`. The system id and
  the key differ for sleep, so `SYSTEM_OF` maps `sleep_pressure` to `sleep`.
- **Healthy rule.** `{ from: endurance, scale: 10 }` for both pools. The
  maxima are Lua-only derived stats (`max_exhaustion`, `max_sleep_pressure` =
  effective endurance x 10) and never stored, so a literal
  `{ from: max_exhaustion }` would name an input no unit holds and refuse every
  spawn. A hspec case pins the seed target equal to `stats.get(max_*)` at
  binary32 for each shipped species over a spread of endurances. The one
  numerical trap: a Nominal substitute computed as `get(owned) / get(max)`
  reads 0.99999996 at endurance 0.3 (binary32 healthy 3.0 against a double
  3.0000001192), so every Nominal and Unsupported substitute is a constant.
- **Profiles.** `rich` S/S, `reduced` S/S, `simple` S/N, `exempt` N/N; the
  technomule overrides both to Nominal. The squirrel's sleep is Nominal
  because it has no `go_to_sleep`: its `sleep_pressure` config entry stays as
  the drain-only parameters of a pinned squirrel and the circadian shape.
- **Ticking.** `unit_resource_params.lua` maps `exhaustion` and
  `sleep_pressure` to their systems (acolyte entries are the registry
  defaults), so the pool tick runs each only while Simulated.
- **Consumers.** `exhaustion.fraction` and `speedMultiplier` answer 1.0
  unless exhaustion is Simulated, and compose into every gait through
  `movement_speed.bandMultiplier` as before (0.35 start, 0.55 floor). The sleep
  need is gated on sleep's mode before the latch, the pressure wake on
  Simulated sleep, and `unit_ai_sleep.lua` reads the pool through the facade.
  The Status row follows the S6 contract; exhaustion has no row.
- **Cancelling the latch.** The `go_to_sleep` hook (sleep only) and the
  utility's own check of a restored sequence abandon it: every sequence field
  is cleared and the unit is unwound to a pose it can act from. This is not
  just a -infinity utility, which would strand a Sleeping unit (Motion moves
  only Standing or Crawling units, revive acts only on Collapsed or Crawling,
  and the injury tick's crawling exemption keys on the phase). A hop in flight
  is waited out, because `unit.transitionTo` is a no-op mid-transition and
  `getPose` reports the pose being left.
- **Persistence.** No wire change; both pools Persist exactly while
  Simulated; the AI sequence is durable and untouched by the load and
  teardown resets. A legacy squirrel's stored `sleep_pressure` is scrubbed by
  D-6 (b), logged once.
- **Retained modifiers.** Seeds were already modifier-aware (D-5); S8b makes
  the other writers of the two pools so. `scripts/lib/stat_base.lua` solves
  the stored base for an effective target (`t / (1 + sum p) - sum d`, a
  modifier active while `now < expiry`); `credit` / `debit` for both pools and
  the resource tick's carry integrator use it, so a +50% or -50% modifier no
  longer turns an effective 10 into 13.5 on a debit or into 15 on the next
  tick. The integrator is shared, so a modified hydration or hunger pool gets
  the same correction; with no active modifier nothing changes bit for bit.
- **Facade and audit.** `credit` / `debit` gain rules for both pools (a
  Nominal receiver discards). The access audit gains the two owners, the
  probe's exhaustion fixture pins, and an updated self-test whose unowned
  example is `heart_rate`.

### S9. Cardio and circulation

#### S9.1 Current state

All of it is Lua and the Lua worker. No Haskell code reads `heart_rate`,
`blood_oxygen` or `circulation` (grep of `src/`).

- **`heart_rate` and `blood_oxygen`** are integrated by `cardio.tick`
  (`scripts/cardio.lua:44-76`). Its inputs are core temperature, blood
  fraction (`unit.getBlood`), activity, lung damage
  (`injuries.pulmonaryFailure`) and last tick's `circulation`. It ticks
  first in the pass (`unit_resources.lua:135`).
- **`circulation`** is computed by `circulation.compute`
  (`scripts/circulation.lua:31-62`) from blood fraction, constitution, fat,
  `sepsis`, core and heart rate. Thermo writes it (`thermo.lua:130-131`),
  and #2754 writes it for nomads (`unit_survival_exempt.lua:54`). v1 makes it
  an unowned always-on step (D-1) and a tick-lagged cache (D-5a).
- **Readers:**
  - thermo heat loss and frostbite (`thermo.lua`);
  - cardio's oxygen delivery;
  - brain consciousness via `blood_oxygen` (`brain.lua:176`);
  - the `hypoxia` failure meter driver, `max(o2 path, lung path)`
    (`cardio.lua:85-90`, `unit_resource_failure.lua:33`);
  - UI rows and the "Suffocating" condition
    (`unit_info_v2_status.lua:49-82,159,324-327`).
- **Death:** only through the `hypoxia` meter (suffocation).

#### S9.2 Proposed registry entry

One system, `cardio`.

| Owns (Nominal read; seed) | Derived (on demand) | Meters | Provides → Nominal substitute | Effects (none while Nominal) |
|---|---|---|---|---|
| `heart_rate` (70, `cardio.RESTING_HR`), `blood_oxygen` (1.0) | `circulation` (SD-21: injury factors always live) | — | heart rate → 70; blood oxygen → 1.0, to brain and to the meter's o2 path; circulation → computed with heart-rate factor 1.0 and every other factor live (SD-21) | o2 hypoxia |

- **Requires:** body.
- **Contributes:** none.

The **`hypoxia` meter is not owned by cardio.** Its lung-damage path is
injury physiology and must keep killing (#2754). Under Nominal cardio its
driver becomes `pulmonaryFailure` alone, because the o2 path reads the
substitute 1.0.

`circulation` becomes an **on-demand derived key**, `circulationRule`. It
is a pure Haskell port of `circulation.compute`, reading blood through
`bloodView` (S10.2). That removes the last stored v1-era cache this design inherited
(D-5a's tick-lagged list) and makes thermo's read coherent. A legacy stored
`circulation` is scrubbed at load, in the manner of D-6 (i). The raw writers
(`thermo.lua:130-131`, `unit_survival_exempt.lua:54`) are removed.

#### S9.3 Interactions

- **Thermo (v1):** reads circulation on demand. Under thermo Nominal,
  circulation still uses the core substitute 37.0.
- **S10:** circulation and heart rate read the blood fraction through the
  accessor (Nominal blood → 1.0).
- **S11:** consciousness reads `blood_oxygen`.
- **Tick order:** cardio stays first. With circulation on demand, the
  one-tick oxygen-delivery lag noted in `cardio.lua:41-43` disappears.
  That is a small, intended numeric change, listed in the slice PR.
- **Threads:** Lua-only today. The Haskell port of `circulationRule` is pure
  of the instance, so the combat thread may read it safely if it ever needs
  to.

#### S9.4 Proposed profile columns

`cardio` is Simulated for acolyte, bear_brown, technomule, red_squirrel
and nomad_primitive, and Unsupported for bodyless definitions. That is
today's behavior: #2754 kept cardio live for nomads. Nominal is available
per unit by override.

#### S9.5 Persistence

No v5. `heart_rate` and `blood_oxygen` are durable owned keys, already in
`uiStats`. `circulation` moves from a serialized-but-ignored cache (D-6) to
not stored: legacy values are scrubbed, logged and idempotent. The D-6
classification table and the planned §2 amendment drop `circulation` from
the cache row.

#### S9.6 Tests

- I1 and I3 for cardio; I2 edges cardio → brain and blood → cardio.
- **T-S9-1:** Nominal cardio with a punctured lung still suffocates through
  the meter's lung path.
- **T-S9-2:** on-demand circulation equals `circulation.compute` for a
  pinned grid of inputs. It is coherent immediately after a blood change
  or a modifier expiry on `constitution`.
- **T-S9-3:** frostbite drive under heavy blood loss still rises under
  Nominal cardio (SD-21).

#### S9.7 Slice plan

One issue, S9, **independent**; it needs S4.

#### S9.8 Decisions (settled)

- **SD-21 (S9-D1): option (a).** Under Nominal cardio, circulation keeps its
  injury factors (blood loss, sepsis, constitution, fat, cold
  vasoconstriction), and only heart rate reads healthy (factor 1.0). The
  `circulationRule` (S9.2) implements exactly that, and T-S9-3 asserts that
  frostbite drive still responds to heavy blood loss under Nominal cardio.

#### S9.9 Implementation record (#2836)

What shipped, where it differs from the S9.2 sketch, and why. The six
corrections of the approved issue review (issuecomment-6229566105) are applied
literally; each is noted.

- **Registry (S9.2).** `cardio` is the eighth system, appended after `sleep`
  in `data/physiology/systems.yaml`. It requires `body`, owns `heart_rate`
  (healthy `RESTING_HR`) and `blood_oxygen` (healthy 1.0), derives
  `circulation`, owns no meter, makes no outgoing contribution and has one
  effect, `oxygen_path_hypoxia` (absent while Nominal). `RESTING_HR: 70` is a
  registry constant (correction 6): `cardio.RESTING_HR` is a drift-tested
  copy, and circulation's divisor is the registry's, read by the Haskell
  rule. The `hypoxia` meter is owned by nobody.
- **Profile columns (correction 2).** The loader refuses a profile that does
  not name every system, so `cardio: simulated` is added to all four
  profiles (`rich`, `reduced`, `simple`, `exempt`). The existing columns are
  untouched. All five bodied species therefore resolve Simulated with no
  per-species entry, `exempt` keeps #2754's live cardio, and a definition
  with no `body:` resolves Unsupported.
- **A Nominal circulation is not a constant (correction 3).** The derived-key
  grammar gains `healthy: live`: a Nominal read evaluates the rule instead of
  returning a number. The interface-substitute grammar gains `live`, valid
  only on a `provides` entry naming a live derived key of its own system. The
  loader, the Lua registry export and the access audit's coverage check all
  accept it, and the loader refuses a `circulation` rule that is not live
  (it would erase the SD-21 factors) and a `salt_concentration` rule that is.
- **The rule.** `Unit.Physiology.Circulation.circulationRule` is a pure port
  of `circulation.compute` with the old constants, clamps and fallbacks. Its
  inputs are gathered by `Unit.Physiology.circulationFor`: the effective
  constitution, fat mass, body mass and sepsis (fallbacks 1.0, 0, 70, 0,
  never a lazy roll), the core temperature through its own mode-aware read
  (the thermo baseline under a Nominal or Unsupported thermo; a Simulated
  thermo with no stored core is an invariant violation), the effective heart
  rate while cardio is Simulated (a missing one is an invariant violation)
  and none under a Nominal cardio, whose heart-rate factor is exactly 1.0.
  `scripts/circulation.lua` is now a two-line reader over `unit.readPhysio`
  and holds no formula.
- **Blood (S9.2 requirement 2).** `Unit.Blood.bloodView` is the pure adapter
  in its unmoded form: the stored `uiBlood`, the maximum from BASE
  `body_mass` (70 kg fallback) times 0.075, the fraction clamped to 0..1 (1.0
  when the maximum is not positive) and the live wound bleed rate (lazy).
  `PhysioContext` carries it (`pcBlood`). Lua reaches it through the pure
  read verb `unit.readBloodView` (correction: cardio's Lua blood input);
  `unit.getBlood` is untouched until S10.
- **Writers removed (correction 1).** thermo no longer computes anything: it
  reads circulation on demand. `circulation.update` (the every-pass call in
  `unit_resources.lua` and the `physiology.refresh` call) is gone, so
  `refresh` keeps only the brain, and `circulation` leaves
  `rebuiltCacheStats` and the `onSaveLoaded` rebuild. (`unit_survival_exempt`
  no longer exists, S4 removed it.)
- **Tick order (correction 5).** Cardio keeps its dedicated call ahead of the
  registry-order loop, now gated on a Simulated cardio (it used to run for
  every unit, bodyless ones included). Its registry position is last; the
  comment in the registry says so. It integrates heart rate first and then
  reads circulation for the oxygen delivery, which removes the one-tick lag.
  That, and nothing else about the integration equations, drift factors or
  bands, is the intended numeric change.
- **Writers under modifiers.** The cardio integrators write the stored base
  whose EFFECTIVE value is the integrated one (`scripts/lib/stat_base.lua`,
  the S8b rule), so a retained modifier on `heart_rate` or `blood_oxygen`
  does not skew the trajectory.
- **Bodyless units (the intended behavior change).** A bodyless unit's cardio
  is Unsupported. Before, it integrated compensatory tachycardia and
  depressed oxygen and circulation from a 0 L blood seed against a 5.25 L
  fallback maximum, and brain consciousness read that. Now those keys are not
  stored (a stored value is scrubbed at load), cardio does not tick, brain
  reads oxygen 1.0, the hypoxia oxygen path is 0 and the lung path is
  unchanged. No consumer raises on the nil.
- **Status rows (correction 4).** The Nominal row text "Not simulated for this
  unit: always healthy." is false for circulation under a Nominal cardio.
  Heart rate and blood oxygen follow D-8 ("Nominal 70 bpm", "Nominal 100%";
  hidden when Unsupported). Circulation shows its live value in its usual
  colour bands under a Nominal cardio and is hidden only when Unsupported.
  The injury-driven Suffocating condition is unaffected.
- **Persistence (S9.5).** No wire change. `heart_rate` and `blood_oxygen` stay
  durable owned keys; a stored `circulation` is scrubbed in every mode, logged
  once, and a re-save/reload scrubs nothing more (D-6 (i) through the
  existing derived-key repair). Canonical circulation is computable before
  the first paused observation without being stored.

### S10. Blood volume and recovery

#### S10.1 Current state

- **Storage:** `uiBlood ∷ Float`, a dedicated `UnitInstance` field, not a
  `uiStats` key (`src/Unit/Types/Instance.hs:133-139`). It persists through
  `uisBlood` (`src/World/Save/Types.hs:841,1002`). The maximum is
  `body_mass × bloodMassRatio` (0.075, `src/Unit/Types/Wound.hs:134`),
  recomputed at each read.
- **Writers:** the wound tick, `tickAllWounds`, on the **combat thread**
  (`src/Combat/Thread.hs:137`), folding inside one `atomicModifyIORef'`
  (`src/Combat/Wounds/Tick.hs:105-135`).
  - It drains `bleedRate · dt` (`:373-422`).
  - It enqueues `UnconsciousNow` below `unconsciousFraction = 0.30`
    (`Combat/Wounds/Constants.hs:38`; `Tick.hs:458-479`).
  - It enqueues `DiedNow "exsanguination"` at 0 (`Tick.hs:475`), which the
    unit thread applies as collapse or kill (`:139-145`).
  - Recovery (#2639, `recoveredBloodVolume`, `Tick.hs:185,540-553`) runs at
    an aggregate bleed of zero and is scaled by
    `bloodRecoveryNutritionMultiplier` (`Healing.hs:148-161`), which reads
    calories and hydration (`:148-151`). S5 moves that read to `physioRead`.
- **Readers:**
  - Lua through `unit.getBlood`: cardio (`cardio.lua:46`), circulation
    (`circulation.lua:33`), AI (`unit_ai_core.lua:391`), the revive gate
    at 50 % (`unit_resource_tick.lua:472-475`), and the UI blood row,
    bleeding condition and "Unconscious (blood loss)"
    (`unit_info_v2_status.lua:26,121,273,297`);
  - **the unit thread's movement speed**: `injurySpeedMult` computes a
    blood multiplier, `0.5 + 0.5 × fraction`, from the raw `uiBlood` and
    base `body_mass` (`src/Unit/Thread/Command/Body.hs:51-75`), applied at
    `src/Unit/Thread/Command/Motion.hs:51,110`;
  - the Lua `unit.getBlood` record `{current, max, bleedRate}`, built from
    raw `uiBlood` and base `body_mass`
    (`src/Engine/Scripting/Lua/API/Units/Combat.hs:389-420`);
  - **bleed decals**, fed from the conserved actual drain: the external
    share of the volume really removed goes into `uiTrailState`
    (`Tick.hs:413-455`), and `Blood/Trail.hs:138-161` marks only from
    pending volume (`docs/blood_decals.md`). Movement-side decal code reads
    wounds (`Unit/Thread/Movement.hs:62`).
  - Every maximum today uses **base** `body_mass` (`Tick.hs:456-458,542-553`,
    `Body.hs:51-75`, `Units/Combat.hs:389-420`).
- **Death:** exsanguination at 0. **Collapse:** below 30 %, with recovery
  gated at 50 %.

#### S10.2 Proposed registry entry

One system, `blood`.

| Owns (Nominal read; seed) | Meters | Provides → Nominal substitute | Contributes → Nominal substitute | Effects (none while Nominal) |
|---|---|---|---|---|
| `blood` volume, stored in the `uiBlood` field (= `blood_max`) | — | volume → `blood_max`; fraction → 1.0, to cardio, circulation, the revive gate, AI, UI and **movement**; `bleedRate` → **live** from wounds | visual blood loss into the trail accumulator → **wound-derived flow, with no volume debit** (A1-6) | volume drain, collapse below 30 %, exsanguination death, recovery |

- **Derived:** `blood_max = base body_mass × bloodMassRatio`, and
  `blood_fraction`. They are on demand and never stored. **Base**, not
  effective, `body_mass` is kept deliberately: it is what every consumer
  uses today (S10.1), so no reader changes meaning. That retires the
  revision's earlier "effective max" wording.
- **Requires:** body.

Under Nominal, wounds are untouched: severity, infection, clotting,
bandages, decals, pain, gangrene and the wound-driven injury meters all
continue (`Tick.hs:474-480`). Only the volume and its own consequences are
skipped. Nominal blood is not injury immunity.

**`bloodView`, the read interface and storage adapter (A1-5).** One pure
Haskell function:

```
bloodView ∷ UnitDef → UnitInstance → BloodView
```

`BloodView` holds `{volume, max, fraction, bleedRate}`.

- **Simulated:** `volume = uiBlood`, `max = blood_max`, `fraction` clamped
  to 0..1, and `bleedRate` from the wounds, as today.
- **Nominal:** `volume = max = blood_max`, `fraction = 1.0`, and
  `bleedRate` still live from the wounds. The stored `uiBlood` is
  **ignored**, so a later body-mass change cannot turn a stale stored value
  into a partial fraction.
- **Unmoded, before S10 lands:** exactly today's raw values. That is the
  fallback S9 relies on.

Every consumer goes through it:

| Consumer | Thread |
|---|---|
| the wound tick's drain, thresholds and recovery | combat |
| `injurySpeedMult` (movement) | unit |
| `unit.getBlood`, which now builds its record from `bloodView` and keeps the same shape and `bleedRate`, so every Lua caller is mode-aware without per-call edits | Lua |
| S9's `circulationRule` and cardio's blood input | Lua |
| `physioRead` for `blood`, `blood_max` and `blood_fraction` | any |

**Landing order.** `bloodView` is introduced by whichever of S9 or S10
lands first, in its behavior-preserving unmoded form. S10 adds the mode
branch and migrates the remaining consumers: wound tick, movement and
`getBlood`. So S9 works before S10, and S10 works while cardio and
circulation are still Lua.

**Storage.** `uiBlood` is a Float field and cannot be absent. While
Nominal, it is **serialized-but-ignored** (D-6 taxonomy). The transition to
Nominal writes the current `blood_max` into it, so the stored value is
canonical. Nominal → Simulated seeds it at `blood_max`. The field carries no
modifiers, so modifier-aware seeding reduces to `base = target`.

**Visual bleeding under Nominal (A1-6).** The wound tick still computes the
wound-derived external flow, the sum of external `bleedRate · dt` with
clot and bandage applied, exactly as it does for drain.

- Under **Simulated**, the conserved rule is unchanged: the trail receives
  the external share of the volume actually removed (`Tick.hs:413-425`).
- Under **Nominal**, the trail accumulator receives that external flow
  **directly**, and `uiBlood` is not debited. This is an **intentional
  exception** to #882's conserved-loss accounting, recorded as blood's
  Nominal substitute for the visual-loss contribution.
- The zero-bleed clear (`Tick.hs:435-455`) and the clot, bandage and heal
  cleanup are unchanged, so marks stop as soon as external bleeding stops.

**Concurrency.** The wound tick on the combat thread consults the pure
resolver per unit, inside its atomic fold. A transition is its own atomic
update on the same `IORef`, so a tick sees the unit entirely before or
entirely after it. Movement, on the unit thread, reads `bloodView` from its
own snapshot, which is always a coherent instance.

#### S10.3 Interactions

- **Nutrition and hydration (v1):** recovery reads calories and hydration
  through `physioRead`, which gives 1.0 under Nominal, as S5 provides.
- **S9:** `circulationRule` and cardio read `bloodView`.
- **v1 death gating:** `checkRevive`'s blood gate reads `getBlood`, which is
  mode-aware.
- **Threads:** the volume is written only by the combat thread's fold and by
  transitions. Both are atomic on the same reference. Every other thread
  reads `bloodView` of a snapshot.

#### S10.4 Proposed profile columns

`blood` is Simulated for acolyte, bear_brown, technomule, red_squirrel
and nomad_primitive, and Unsupported for bodyless definitions. That is
today's behavior (SD-22).

#### S10.5 Persistence

No v5. `uisBlood` stays.

- Classification: *Persist exactly* when Simulated, *serialized-but-ignored*
  when Nominal. It is recorded in the `umInstances` row and the D-6 table.
- Normalization (b) rewrites the field to the canonical `blood_max` when a
  unit's effective blood mode is Nominal.
- `uiTrailState` keeps its existing classification.

#### S10.6 Tests

- I1 and I3 for blood; I2 edges blood → cardio and nutrition → blood.
- **T-S10-1, Nominal blood under a severe slash** (A1-6):
  - full volume, with no collapse and no exsanguination;
  - the wound, its pain and gangrene still progress;
  - the trail accumulator **grows**, and marks are produced, while the slash
    bleeds externally;
  - once a **fully sealing** bandage (seep 0) or complete clotting makes
    external bleeding exactly zero (`Units/Medical.hs:48-50`,
    `Wounds/Bleed.hs:51-59`), the accumulator clears and marks stop
    (`Tick.hs:435-455`);
  - a **competent dressing** (seep 0.05) keeps a reduced, non-zero flow,
    and marks continue at that reduced rate.
- **T-S10-2:** a transition interleaved with the combat thread's wound tick
  yields a serial order: no lost drain, and no drain under Nominal.
- **T-S10-3, Nominal → Simulated seed** (replaces the effective-max case):
  `uiBlood = blood_max` from **base** `body_mass`, matching `getBlood.max`,
  the movement maximum and the wound-tick maximum.
- **T-S10-4, movement** (A1-5): a Nominal-blood unit with a stored partial
  `uiBlood`, after a body-mass gain, moves at blood multiplier 1.0, and
  `getBlood` reports `current == max`.
- **T-S10-5, landing order:** with blood unmoded, `bloodView` equals today's
  raw values for a pinned grid of inputs, which is S9's fallback.

#### S10.7 Slice plan

One issue, S10, **independent**; it needs S4. If it lands before S9, it
introduces `bloodView` (S10.2).

#### S10.8 Decisions (settled)

- **SD-22 (S10-D1): option (a).** No shipped profile uses Nominal blood, and
  Nominal is available by override.
  - **Owner's reasoning:** they would have spawned wildlife and nomads
    Nominal for performance. But Simulated blood for an unwounded unit
    costs only per-unit arithmetic inside the wound fold, which runs for
    every unit anyway (`src/Combat/Wounds/Tick.hs:185`, recovery
    `:540-553`). Nominal would save nothing material.
  - Bleeding also stays combat physiology for nomads, as #2754 intended.

#### S10.9 Implementation record (#2837)

What shipped, where it differs from the S10.2 sketch, and why. The four
corrections and the spec additions of the approved issue review
(issuecomment-6231193340) are applied literally.

- **Registry.** `blood` is the ninth system, appended after `cardio` in
  `data/physiology/systems.yaml`. It requires `body`, owns the key `blood` with
  `field: uiBlood` (healthy: the new named rule `blood_max`), derives
  `blood_max` and `blood_fraction` (both `live`), owns no meter and contributes
  `visual_loss` with the new substitute `wound_flow`. Its effects (volume
  drain, blood-loss collapse, exsanguination, volume recovery) are absent
  while Nominal. `blood: simulated` is added to all four profiles (correction
  3); no `data/units/*.yaml` physiology block changes, so the five bodied
  species resolve Simulated (SD-22) and bodyless definitions Unsupported.
- **Healthy rule (correction 4).** `from:` reads the EFFECTIVE input, so a
  `body_mass` modifier would move the maximum. The `blood_max` rule is BASE
  `body_mass` x `bloodMassRatio` (0.075, the single source; no registry copy
  to drift) with the 70 kg fallback every blood reader has. The Nominal read,
  the transition seed and the caller-value cap all use it.
- **One adapter (correction 1).** `Unit.Blood` is extended, not duplicated:
  `bloodView` stays the RAW view (what a Simulated, Unsupported or undeclared
  blood reads, exactly the pre-S10 values), `nominalBlood` is the Nominal
  branch (volume = maximum, the stored `uiBlood` ignored, fraction 1.0, live
  bleed rate). `Unit.Physiology.instanceBloodView` / `unitBloodView` choose
  between them through the one pure resolver against the registry's own
  `blood` declaration; a registry with no `blood` system (the wound tick's
  `emptyPhysiologyRegistry`, hand-built fixtures) and an unregistered
  definition read raw. Every consumer reads through it: the wound fold, the
  unit-thread movement multiplier (`injurySpeedMult` now takes the view),
  `unit.getBlood` (record shape `{current, max, bleedRate}` unchanged),
  `unit.readBloodView`, circulation and cardio (consumed Nominal fraction 1.0
  without any change to their code).
- **Storage adapter (correction: no shadow pool).** The volume is never a
  `uiStats` key. A simulated owned read of a field-backed key returns the
  context's raw volume; the transition plans run over the stats with the field
  key materialized and split it back out (`Transition.trBlood`), so seeding,
  scrubbing, caller values and their caps are the generic code; the
  reconciliation used by spawn and the load gate skips field keys and scrubs a
  stored legacy `blood` stat in every mode; a raw `unit.setStat` of the
  volume or either derived key is refused. The access audit expresses the
  owner through `HASKELL_OWNERS` and inventories every Haskell file that
  touches the field or the view (`BLOOD_FIELD_INVENTORY`), with no Lua owner
  fabricated and no exemption.
- **Wound fold.** Inside the existing atomic update the resolver is consulted
  per unit. Under Nominal: no volume debit, no recovery (including the
  unwounded-unit branch), no blood-loss collapse and no exsanguination outcome;
  gangrene death, wound progression, infection, necrosis, pain, clotting,
  bandages and the bleed rate are unchanged. The trail accumulator is credited
  the wound-derived external flow directly; under Simulated it is credited the
  conserved external share of the volume actually removed, exactly as #882.
  Zero-external-bleed cleanup is identical in both modes.
- **Persistence (correction: D-6 zero normalization).** `uisBlood` and the
  units-v4 override map are unchanged. A Nominal unit's stored volume is
  serialized but ignored: the load gate rewrites it silently to the current
  canonical maximum, as the other rebuilt caches are, so a unit saved by
  current code whose body mass moved afterwards logs zero repairs and a second
  load finds it canonical. A stored legacy `blood` stat is logged once.
- **Presentation (correction).** The blood row follows the S6 contract: a
  Nominal blood reads `Nominal` in the neutral colour, its tooltip keeps the
  LIVE bleed rate, the wound-driven Bleeding condition stays with text that no
  longer claims exsanguination, the volume-derived blood-loss collapse
  condition is suppressed, and an Unsupported unit's row is hidden.

### S11. Brain and mental state

#### S11.1 Current state

`brain.tick` (`scripts/brain.lua:245-269`) makes **six separate**
`unit.setStat` writes per pass on the Lua worker:

- `consciousness`, the instantaneous minimum of temperature, oxygen and
  salt factors (`:172-196`);
- `mood` (drift);
- `emotional_pain` (drift);
- `caffeine` (decay);
- `concentration`, from consciousness, pain, stamina and caffeine;
- `state_of_mind`.

The rest of the mental state:

- **Thoughts:** `thoughts.tick` nudges `mood` and writes `thought_next_at`
  (`thoughts.lua:143-159`), drawing from the gameplay `math.random` stream.
- **Mental-state machine:** `mental.tick` (`mental_state.lua:216-280`)
  writes `mental_state`, `mental_since`, `mental_until`,
  `mental_next_roll_at`, `mental_cooldown_until`, `mental_high_since` and
  `mental_break_behavior`. Each state change is **three separate writes**
  (`:110-115`). While euphoric, it bumps the stored `concentration` on top
  of brain's value (`:240-246`).
- **Readers:**
  - **combat thread:** `mentalEffectiveness` reads `concentration` and
    `mental_state` from the instance (`src/Combat/Resolution/Common.hs:116-128`);
  - the collapse machine and revive gate (`unit_resource_injury.lua:141-154`,
    `unit_resource_tick.lua` `brain.canRise`);
  - the AI short-circuit for delirium and mental breaks, which applies to
    every species (`unit_ai_mental.lua`, `unit_ai.lua:299`). Lash-out
    excludes only the technomule as a target (`unit_ai_mental.lua:85`);
  - UI consciousness row and the Delirious, Confused and mental-state
    conditions (`unit_info_v2_status.lua`), and the thought log.
- **Effects:** unconscious collapse below 0.15, rise at 0.40; delirium and
  confusion; breaks (wander, flee, catatonia, lash-out, #717); euphoria's
  combat bonus. No death path.

Defect found:

- **S11-F1. Torn reads across threads.** Combat reads `concentration` and
  `mental_state` while the Lua worker is between brain's separate writes,
  or between `setState`'s three writes. It can also catch brain's
  concentration before or after the euphoria bump in the same pass. D-5a
  deferred this to S11.

#### S11.2 Proposed registry entries

**Two systems**, because the physiological and psychological halves have
different consumers and different natural profiles. Both require a body.

**`consciousness`** has no owned keys, no meters and no contributions.

- **Derived (on demand), as pure Haskell rules exposed through
  `physioRead` / `readPhysio`:**
  - `consciousness` (healthy 1.0): rule `consciousnessRule`, from effective
    core, `blood_oxygen` and `salt_conc`, read through the accessor;
  - `concentration` (healthy 1.0): rule `concentrationRule`, from
    consciousness, pain (`painFor`), the stamina fraction, `caffeine`, and
    the +0.10 euphoria lift when `mental_state` is EUPHORIC. The lift moves
    from `mental_state.lua:240-246` into the rule. Combat's separate ×1.10
    euphoric multiplier in `mentalEffectiveness` (`Common.hs:126-127`) is a
    distinct effect and stays.
- **Provides → Nominal substitute:**
  - consciousness → 1.0: never delirious, confused or unconscious *from
    physiology*. Concussion (`injuries.isUnconscious`) still knocks a unit
    out.
  - concentration → 1.0, to every consumer of `mentalEffectiveness`: combat
    hit and active-dodge rolls (`Common.hs:97-128`), the Lua
    `unit.getMentalEffectiveness` (`Units/Combat.hs:498-512`), and craft
    progress and quality (`Craft/Execute.hs:259`).
- **Effects:** physiological collapse, delirium, confusion.

**`mind`** owns the following keys. Each has its Nominal read and the seed
written at Nominal → Simulated and at spawn (A1-7). `now` is the
transition's game-time sample.

| Key | Nominal read | Seed |
|---|---|---|
| `mood` | 1.0 | the current drift target (as `driftMood`'s first tick does, `brain.lua:205`) |
| `emotional_pain` | 0 | 0 |
| `caffeine` | 0 | 0 |
| `mental_state` | STABLE (0) | STABLE |
| `mental_since` | 0 | `now` |
| `mental_until` | 0 | 0 (no episode) |
| `mental_next_roll_at` | 0 | `now + TUNE.CHECK_INTERVAL` (as `setState`, `mental_state.lua:115`) |
| `mental_cooldown_until` | 0 | 0 |
| `mental_high_since` | 0 | 0 |
| `mental_break_behavior` | WANDER (today's read default, `:106`) | WANDER |
| `thought_next_at` | 0 | `now`; `mind`'s `onModeChanged` hook then re-rolls it to `now + random · THOUGHT_INTERVAL_MIN` on the gameplay stream, as `thoughts.lua:143-147` does today |

- **Derived (on demand):** `state_of_mind` (Nominal = consciousness), rule
  `stateOfMindRule`.
- **Meters:** none.
- **Contributes → Nominal substitute:** caffeine's stamina regen bonus
  (`unit_resource_tick.lua:283-289`) → 0. It reads caffeine through the
  accessor, whether or not S8 has landed.
- **Incoming credits**, discarded when `mind` is Nominal: `consumable.drink`'s
  caffeine and mood (`consumable.lua:178-191`) move to `credit`. The rest of
  the drink still succeeds: fluid draw, hydration credit, and the result
  record. Thoughts' mood nudge (`thoughts.lua:159`) is `mind`'s own writer.
- **Provides → Nominal substitute:** no breaks, no euphoria, no thoughts.
- **Effects:** mental breaks, euphoria, thought mood nudges.

**Legacy and partial episodes.** Normalization (b) for a Simulated `mind`
seeds **only absent** keys and never overwrites a present durable key,
following the per-unit stats paragraph of persistence contract §2. A
partially populated episode is made consistent:

- `mental_state` BREAK or EUPHORIC with `mental_until` absent → seed
  `mental_until = now`, so the episode ends on its next tick through the
  normal `endEpisode`;
- a BREAK with `mental_break_behavior` absent → WANDER;
- every other absent key → its seed above.

Present values are kept, including stale deadlines, which the machine
already handles as expired.

**Atomic writes (the D-5a deferral).**

- `consciousness`, `concentration` and `state_of_mind` become on demand,
  and legacy stored values are scrubbed at load (as D-6 (i)).
- Every remaining multi-key write goes through a new engine verb,
  `unit.commitPhysio(uid, {key = value, …})`. It validates every key with the
  write gate and stores them in **one** `atomicModifyIORef'`, or none.
- The writers covered:

  | Writer | Batch |
  |---|---|
  | `brain.tick` | `mood`, `emotional_pain`, `caffeine` |
  | `thoughts.tick` | `mood`, `thought_next_at` |
  | `mental.tick` / `setState` / `enterBreak` / `enterEuphoria` / `endEpisode` / the high-since tracker (`mental_state.lua:105-198,253-291`) | each event's full key set |
  | `consumable.drink`'s mind credits | `caffeine`, `mood` |

- Combat's `mentalEffectiveness` reads `concentration` through the derived
  rule and `mental_state` from the same instance snapshot, which fixes
  S11-F1.

#### S11.3 Interactions

- **Reads:**
  - v1 thermo and salt through the accessor;
  - S9 `blood_oxygen` (unowned pass-through until S9 lands);
  - S8's stamina fraction (pass-through until S8; a pre-engagement nomad
    reads 1.0, SD-20);
  - pain, which is injury and always-on.
- **Incoming:** consumable credits, discarded under Nominal `mind`.
- **Outgoing:** caffeine → stamina regen bonus.
- **Tick order:** `brain → thoughts → mental` stays
  (`unit_resources.lua:150-162`). Each writes one atomic batch.
- **Threads:** combat and craft read the on-demand derived values and a
  batch-committed `mental_state`. Lua writes only atomic batches.
- **Persistence boundary:** D-5a's paused-load refresh no longer needs
  `brain.tick(uid, 0)` for the derived keys, because they are on demand. It
  still runs for the drift keys' canonical form.

#### S11.4 Profile columns (SD-23)

| Species | consciousness | mind | Change from today |
|---|---|---|---|
| acolyte | Sim | Sim | none |
| nomad_primitive | Sim | **Nominal** | no thoughts, breaks or euphoria |
| bear_brown | Sim | **Nominal** | **bears stop having mental breaks** |
| red_squirrel | Sim | **Nominal** | **squirrels stop having mental breaks** |
| technomule | Sim | **Nominal** | no mood, thoughts or breaks |
| bodyless | Unsupported | Unsupported | SD-13 |

`consciousness` stays Simulated everywhere. For nomads, its survival inputs
are already Nominal, so only combat-driven hypoxia can lower it, as #2754
intends. S11b lists the behavior change in its PR: wildlife, the technomule
and nomads stop having thoughts and mental breaks, so lash-out (#717) can
only come from acolytes.

#### S11.5 Persistence

No v5.

- The `mind` keys are durable (*Persist exactly*), with absent-only
  defaulting and the episode consistency rules above.
- `consciousness`, `concentration` and `state_of_mind` move from the D-6
  cache row to *not stored*, with legacy values scrubbed.
- The D-6 table, the §2 amendment and the inventory rows are updated. After
  S9 and S11, the cache row holds only the body-derived `strength` and
  maxima.

#### S11.6 Tests

- I1 and I3 for both systems; I2 edges thermo, salt and cardio →
  consciousness, stamina → concentration, and caffeine → stamina regen.
- **T-S11-1, torn-read regression:** with the combat thread sampling
  `mentalEffectiveness` while Lua runs brain, thoughts and a mental
  transition, every sample equals a serial state.
- **T-S11-2:** euphoria's +0.10 lift appears exactly once, in the derived
  concentration, and combat's ×1.10 is applied separately.
- **T-S11-3, Nominal mind, deterministic** (A1-4, revised for A2-4). The
  roll is `math.random() < BREAK_CHANCE_MAX · severity`, with severity =
  `(STRESSED_BELOW − state_of_mind) / STRESSED_BELOW`
  (`mental_state.lua:71-77,281-292`). Rolls are skipped for a delirious or
  unconscious unit (`:276`). The fixture therefore drives stress through
  the **psychological** inputs, keeping both units **lucid**.
  - **Shared setup:** temperate climate, full blood oxygen, no salt or
    thermal stress, so consciousness is 1.0 and the unit is not delirious.
    The fixture sets `mental.TUNE.BREAK_CHANCE_MAX = 1.0`.
  - **Acolyte, the positive control, with Simulated mind:**
    - Every pass, the fixture commits `mood = 0` and `emotional_pain = 1`
      through `unit.commitPhysio`. Wellbeing is then `clamp(0 − 0.5, 0, 1)
      = 0`, so state_of_mind is 0 and **severity is exactly 1**.
    - It starts at `mental_state` STRESSED, with `mental_since = now −
      SUSTAIN` and `mental_next_roll_at = now`.
    - The roll probability is exactly 1, since `math.random()` is in
      [0, 1), so the acolyte **must** enter BREAK on the next
      `mental.tick`.
    - No derived key is forced; only the owned inputs.
  - **Bear, with Nominal mind under SD-23**, in the same lucid setup:
    - the same `commitPhysio` of `mood` and `emotional_pain` is **refused**
      by the write gate;
    - `mental_state` reads STABLE throughout;
    - `mental.tick` makes no episode writes, and `mental.forceBreak` is a
      no-op;
    - no thoughts are recorded.

    Suppression here comes from the mode, not from delirium.
  - **Hypoxia collapse, a separate case:** a bear with blood oxygen driven
    below the consciousness floor, with its own inputs, collapses through
    `consciousnessRule`. Its oracle is the collapse, not episodes.
- **T-S11-4:** `commitPhysio` refuses the whole batch if any key fails the
  write gate.
- **T-S11-5, initialization and legacy episodes** (A1-7):
  - a fresh Simulated `mind` holds every seed in S11.2;
  - a legacy BREAK without `mental_until` ends on its next tick;
  - a legacy episode with every key present is preserved exactly.
- **T-S11-6, Nominal-mind consumable** (A1-7, revised for A2-5). The bear's
  default profile has Nominal hydration (D-10), so the fixture **overrides
  the bear's hydration to Simulated** with a pool at 50 % before drinking.
  Its mind stays Nominal. The acolyte's hydration is likewise pinned at
  50 %.
  - The bear gains hydration from the drink. Its caffeine and mood credits
    are discarded, and the keys stay absent.
  - The acolyte gains all three.

#### S11.7 Slice plan

- **S11a: the rules and atomic writes.**
  - Registers `consciousness` and `mind` with **all-Simulated** profiles,
    so the derived rules are live through `physioRead` before any mode
    changes.
  - Adds `consciousnessRule`, `concentrationRule` and `stateOfMindRule`,
    `unit.commitPhysio`, the seeds, legacy normalization and the writer
    migrations in S11.2. That includes consumable credits and caffeine's
    stamina read, whatever S8's state.
  - Fixes S11-F1, and is otherwise behavior-preserving.
- **S11b: the SD-23 profiles.** `mind` becomes Nominal for wildlife, the
  technomule and nomads, with the behavior change listed.
- S11b depends on S11a, and both need S4. S11a is **independent** of S8 to
  S10.

#### S11.8 Decisions (settled)

- **SD-23 (S11-D1): option (b).** Only acolytes keep a Simulated mind.
  Wildlife, the technomule and nomads are Nominal, with no thoughts or
  breaks. Consciousness stays Simulated everywhere. Behavior change: bears
  and squirrels stop having mental breaks.

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
