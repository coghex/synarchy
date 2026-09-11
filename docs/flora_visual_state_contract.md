# Flora visual state and texture fallback contract

This document is the single canonical statement of how a flora occurrence's
visual state is named, how sparse art is declared, how a texture is chosen from
whatever subset of art a species actually has, and how long a dead plant stays
visible. Epic #2526 (children EFM-2 through EFM-10) is written against it.

It is the authority for **visual semantics**. It is not the authority for what
any individual species ships: each family's authored YAML under `data/flora/`
remains authoritative for its own declared assets.

Design record:
[`environmental_flora_mortality_design.md`](environmental_flora_mortality_design.md)
(decisions D-1 through D-21). Contract statement and gates:
[`engine_contracts.md`](engine_contracts.md) §Flora visual state and fallback
(#2526). Art production: [`asset_generation.md`](asset_generation.md) §Flora
pipeline.

---

## 1. The five selector axes

The renderer asks for one semantic **selector**. Every axis below has a closed
vocabulary; anything outside it is rejected at load, never ignored.

| Axis | Vocabulary | Absent? |
|---|---|---|
| context | `wild`, `cultivated` | Never. Every request carries one. |
| phase | the `LifePhaseTag` YAML spellings: `sprout`, `seedling`, `vegetating`, `budding`, `flowering`, `ripening`, `matured`, `withering` — and `dead`, which is a **legacy authoring token only** (§1.1) | Only when the species declares no `phases` at all. |
| stage | the `AnnualStageTag` YAML spellings: `dormant`, `budding`, `flowering`, `fruiting`, `senescing` | Yes, when the species declares no `annualCycle`. |
| condition | `alive`, `dead` | Never. Every request carries one. |
| cause | `natural`, `drought`, `frost`, `fire`, `disease`, `damage`, `unknown` | Yes, and always absent when condition is `alive`. |

The phase and stage vocabularies are exactly the ones `parsePhaseTag` and
`parseCycleTag` already accept in `src/Engine/Asset/YamlFlora.hs`, backed by
`LifePhaseTag` and `AnnualStageTag` in `src/World/Flora/Types.hs`. This contract
adds no token to either.

**Three distinct things.** The runtime selector type, the YAML representation of
a declaration, and a texture filename are three separate artefacts and are never
assumed to coincide:

- the **selector** is an in-memory value the renderer constructs from an
  occurrence's semantic state;
- the **YAML declaration** is a sparse authored pattern that a selector may
  match;
- the **filename** is a readable convention (§4) with no runtime meaning at all.

A change to any one of them is not automatically a change to the other two.

### 1.1 `dead` is a legacy phase token, never a phase at death (D-21)

`dead` stays in the phase vocabulary because every mortal shipped species
authors it under `phases`, and `World.Flora.Growth` pins the age to that entry
for its dead window. Under the selector model that entry is **not** a life
phase, and `PhaseDead` is never used as a selector's phase:

- a legacy `phases` entry tagged `dead` normalizes to a declaration with
  `condition: dead` and every other axis wildcard — the species' Tier 1 generic
  dead art (§5);
- its `cycleOverrides` entries whose `phase` is `dead` normalize to
  stage-specific generic-dead declarations (`condition: dead` plus that
  `stage`), not to living art;
- every other legacy `phases`, `annualCycle`, and `cycleOverrides` entry
  normalizes to `context: wild`, `condition: alive`;
- a natural-lifespan death requests `condition: dead`, `cause: natural`, and the
  **last living** phase and stage frozen at death (§7.4), which resolves to the
  same `dead.png` the species shows today.

`PhaseDead` therefore remains in `LifePhaseTag` for decoding legacy YAML and
existing saves only. Natural death and hazard death share one fallback path.

---

## 2. Declaring variants: `textureVariants`

Optional art is declared, never discovered. The additive species-level key is a
list of selector patterns, each with a texture path relative to the species'
`texDir` exactly like `phases` and `annualCycle` entries:

```yaml
textureVariants:
  - context: cultivated
    phase: matured
    stage: flowering
    condition: alive
    texture: "cultivated_matured_flowering.png"
  - phase: sprout
    condition: dead
    texture: "sprout_dead.png"
  - phase: matured
    stage: flowering
    condition: dead
    cause: fire
    texture: "matured_flowering_charred.png"
  - condition: dead
    cause: fire
    texture: "charred.png"
```

Rules:

1. **An omitted axis is a wildcard**, deliberately declared. It is never a
   default inferred from the filename, the directory, or a sibling entry.
2. **Duplicate selectors are rejected.** Two entries whose five axes are
   pairwise identical — including "both omit `cause`" — are an authoring error
   and fail the whole file.
3. **Unknown vocabulary is rejected**, not ignored, at every axis position, the
   same way the existing closed vocabularies are (`Asset.FloraVocabularySchema`).
4. **`cause` requires `condition: dead`.** A declaration naming a cause
   alongside `condition: alive`, or alongside no condition at all, is rejected:
   nothing can ever request it.
5. **Every declared path joins the texture-subset audit**
   (`tools/texture_subset_audit.py`), so a variant pointing at a file that does
   not exist is caught the same way a missing phase texture is.
6. **No variant is ever discovered from the filesystem.** Art present on disk
   and absent from YAML changes nothing.
7. **An unreachable declaration is rejected, not silently dropped** (§2.1).

Rule 6 is live today, not hypothetical: `assets/textures/flora/wheat/wild/` and
`assets/textures/flora/wheat/cultivated/` exist (PR #2136) while
`data/flora/crops.yaml` still points wheat's `texDir` at `white_clover`. Those
twelve files render nothing until a declaration names them.

### 2.1 Matching, overlap, and determinism

A declaration **matches** a selector when, for each of the five axes, the
declaration either omits the axis or names exactly the selector's value.

- **An axis the declaration names cannot match a selector that lacks a value
  for it**, and a declaration no selector can ever reach is **rejected**. This
  follows the rule `cycleOverrides` already enforces (`requireDeclared`, #2315):
  a well-spelled override naming a phase the species never declares registers a
  texture no plant can select, which is a silent authoring dead end and is
  refused rather than dropped. Concretely, a `textureVariants` entry is rejected
  when its `phase` is absent from the species' own `phases`, or its `stage` is
  absent from its own `annualCycle`.
- **`phase: dead` is not a valid `textureVariants` selector.** `dead` is a
  legacy `phases[].tag` and `cycleOverrides[].phase` token (§1.1), never a
  phase at death, so a variant entry naming it is rejected. Death is declared
  with `condition: dead`.
- **Omitted context is not the same as `context: wild`.** An omitted context
  matches both contexts; `context: wild` matches wild requests only. The two are
  distinct selectors and therefore not duplicates under rule 2, even when every
  other axis agrees.
- **Overlap is normal and is resolved by the ladder, not by declaration order.**
  Several declarations may match one selector. The winner is decided solely by
  the ten-step order in §3 — the first step at which any declaration matches
  wins. File order, alphabetical order, and `HashMap` traversal order never
  affect the result.
- **Within one ladder step, an explicitly-declared axis beats a wildcard.** If
  both `{context: cultivated, condition: dead}` and `{condition: dead}` are
  declared and a cultivated dead plant is drawn, the cultivated one wins: it is
  the exact selector invariant 2 names. This is a total rule because two
  declarations that tie on explicitness at the same step are duplicates, which
  rule 2 already rejects.
- **Normalized legacy declarations participate in rule 2.** A species that
  authors `phases: [{tag: dead, …}]` *and* a `textureVariants` entry reading
  `condition: dead` with no other axis has declared the same selector twice, and
  the file is rejected. Authors migrating legacy art remove the legacy entry or
  make the new entry more specific.

**Worked examples.** For a species declaring exactly the four entries in the
block above:

| Request | Winner | Why |
|---|---|---|
| cultivated, matured, flowering, alive | `cultivated_matured_flowering.png` | exact declared selector (step 1) |
| wild, matured, flowering, alive | the existing `annualCycle`/`cycleOverrides` result | no dead or cultivated entry matches; step 9 |
| wild, sprout, dormant, dead, fire | `sprout_dead.png` | phase-preserving generic dead (step 5) outranks generic `charred.png` (step 6) |
| wild, matured, flowering, dead, fire | `matured_flowering_charred.png` | exact (step 1) |
| wild, matured, dormant, dead, fire | `charred.png` | the exact entry names `stage: flowering`, so it cannot match; no dead entry names `phase: matured` without a stage, so steps 4 and 5 are empty and step 6 wins |
| cultivated, sprout, dormant, dead, frost | `sprout_dead.png` | no frost art; the context-less `{phase: sprout, condition: dead}` entry is the shared default and satisfies the cultivated attempt of step 5 |

**Rejection examples.** `phase: seedlng` (misspelt token); `cause: lightning`
(outside the vocabulary); `condition: alive` with `cause: drought` (rule 4); two
entries both reading `{phase: sprout, condition: dead}` (rule 2); a
`textureVariants` entry reading `{condition: dead}` on a species that also
authors a legacy `dead` phase (rule 2 via §1.1); `stage: fruiting` on a species
whose `annualCycle` declares no `fruiting` stage, or `phase: ripening` on one
whose `phases` never reach `ripening` (rule 7); and `phase: dead` in any
`textureVariants` entry (rule 7 via §1.1).

---

## 3. The fallback contract

### 3.1 The nine invariants

1. Resolution is **pure, deterministic, finite, and independently testable**.
   The same selector and the same species always give the same answer.
2. **An exact declared selector wins.**
3. **Cultivated tries the same wild semantic state before discarding anything.**
   Context is dropped before phase, stage, condition, or cause.
4. **A dead request exhausts every valid dead candidate before falling back to
   living art.** Looking dead outranks preserving any other axis.
5. **Within dead candidates, phase-appropriate generic-dead art is preferred
   over an adult-shaped cause-specific asset** that would misrepresent a
   juvenile.
6. **Once phase cannot be preserved, cause-specific dead art is preferred over a
   fully generic dead asset.** Missing charred, drought, or frost art never
   makes a plant unrenderable.
7. **Annual-stage specificity is preserved when an exact asset permits it, but
   may be dropped before condition, phase, or cause.**
8. **The final semantic fallback is the species' own base texture** — the first
   `phases` entry's texture, or `matured.png` when a species declares no phases,
   as `registerFloraSpecies` already resolves it.
   `assets/textures/flora/unknown_flora.png` is **only the error fallback**, used
   when that declared base is itself missing or invalid. It is never reached by
   a semantic miss.
9. **Resolution never probes the filesystem at render time** and never changes
   because an undeclared file was added to a directory.

### 3.2 The ten-step shape

For a cultivated flowering sprout killed by fire, resolution attempts, in order:

1. exact cultivated sprout + flowering + fire-dead;
2. exact wild sprout + flowering + fire-dead;
3. cultivated, then wild, sprout + fire-dead **without** the annual stage;
4. cultivated, then wild, sprout + flowering + generic dead;
5. cultivated, then wild, sprout + generic dead;
6. cultivated, then wild, generic cause-specific dead;
7. cultivated, then wild, stage-specific generic dead;
8. cultivated, then wild, generic dead;
9. **the best corresponding living state** (§3.3);
10. the species base texture.

So a fire-killed sprout with no charred juvenile art shows ordinary dead-sprout
art; with no dead-sprout art either it may use generic charred art, then generic
dead art, and ultimately the base texture. It never stays visibly alive merely
to preserve the fire cause or the season.

A living request skips steps 1 through 8 — they are all dead candidates — and
enters at step 9 after its own exact and context-dropping attempts.

**What "matches at step N" means.** Each step preserves some axes and drops
others. A declaration satisfies a step when all three hold:

1. it **matches** the selector in the §2.1 sense;
2. it **names every axis the step preserves** — so a declaration omitting
   `stage` cannot satisfy a stage-preserving step, which is what keeps step 5
   distinct from step 4;
3. it **names no axis the step drops** — so a cause-specific declaration cannot
   satisfy a cause-dropping step, which is what keeps step 5 distinct from
   step 3.

**Context is the one exception**, by D-4: cultivated art overrides wild art
rather than defining a parallel lifecycle. Every step is attempted cultivated
first and then wild, and a declaration that omits `context` is a legitimate
candidate in both attempts — it is the shared default. When an explicit
`context: cultivated` declaration and a context-less one both match the
cultivated attempt, the explicit one wins by the explicitness rule in §2.1. No
other axis behaves this way.

Together these make the winner a function of the selector and the declared set
alone. Two declarations can only tie if they name the same axes with the same
values, which rule 2 already rejects.

### 3.3 "The best corresponding living state"

Step 9 is not a new mechanism. It is exactly today's living resolution in
`World.Flora.Render.resolveSpeciesTexture`, driven by the selector's frozen
phase and stage rather than by the current age and day of year:

1. the `cycleOverrides` entry for that exact `(phase, stage)` pair, if declared;
2. otherwise that `annualCycle` stage's texture, if the species has a cycle;
3. otherwise that life phase's texture;
4. otherwise the species base texture (which is also step 10).

A dead plant reaching step 9 therefore shows the living art of the state it died
in, which is the documented behaviour when a species ships no dead art at all.

---

## 4. The filename convention

Filenames follow a readable order — a context segment when present, then phase,
then stage, then a condition/cause suffix — and are **never authoritative**:

- `sprout.png`, `matured_flowering.png` — living art;
- `dead.png`, `sprout_dead.png` — generic and phase-aware death;
- `charred.png`, `matured_flowering_charred.png` — cause art;
- `wild/flowering.png`, `cultivated/flowering.png` — context as a **directory
  segment**, which is how the shipped wheat art is laid out;
- `cultivated_matured_flowering.png` — context as a **filename prefix**.

Both context spellings are acceptable because neither is read. Only a
`textureVariants` entry's `texture:` value, joined to the species' `texDir`,
registers anything. Renaming a file without editing YAML breaks the texture
audit; it never silently re-binds a selector.

---

## 5. The art maturity ladder

A species is renderable at every tier, and **no tier requires completing the
tier above across every combination**.

| Tier | Art | Effect |
|---|---|---|
| **Tier 0 — base** | one valid species texture | every state renders through step 10 |
| **Tier 1 — generic death** | `dead.png` or an equivalent `condition: dead` selector | dead plants look dead |
| **Tier 2 — phase-aware death** | dead-juvenile art such as `sprout_dead.png` | a dead sprout keeps its silhouette |
| **Tier 3 — generic cause** | `charred.png`, drought-, frost-, disease-, or damage-dead art shared across phases | cause is legible where it is acceptable to share one asset |
| **Tier 4 — exact state** | context-, phase-, stage-, and cause-specific combinations, e.g. cultivated flowering charred art | full fidelity |

Each texture family gets its own art issue and PR with a deliberate manifest,
and the owner signs off on every texture (D-8). Crops that reuse another
species' texture directory do not receive duplicate art issues until they gain
their own family.

---

## 6. Render context

Context is an explicit semantic input, decided where the occurrence is created:

- **naturally generated flora is `wild`**;
- **deliberately planted row flora is `cultivated`**;
- **groundcover `CropPlot`s are `cultivated`**;
- future deliberate transplantation must **choose** a context rather than infer
  one.

Context is **never inferred** from age, health, density, placement category, or
texture path. It may be persisted on the occurrence or in its authoritative
creation record, but it must survive replay and migration.

**A species that declares no cultivated variants renders exactly as it does
today in both contexts**, because every cultivated lookup falls through to the
wild state by invariant 3. Adding the context axis changes no existing visual.

---

## 7. Corpse retention: `corpsePolicy`

Corpse retention is authored semantic data, not a side effect of whether dead
art exists.

```yaml
corpsePolicy:
  visibility: transient       # transient | persistent
  durationDays: 60            # required for transient, refused for persistent
  successor: reseed           # reseed | absent — the WILD-context outcome
```

### 7.1 Field rules

- **`visibility`** is `transient` or `persistent`. `persistent` means visible
  until an explicit clearing, replacement, or revival action — not an immortal
  in-memory object; the record lives in page-owned persistence and is joined to
  regenerated flora.
- **`durationDays`** is **required for `transient` and refused for
  `persistent`.** It is an integer of at least 1. Zero and negative values are
  rejected: "no visible corpse" is not a retention window, and a corpse that
  should vanish rather than reseed is expressed with `successor: absent`.
- **`successor`** is `reseed` or `absent`, is **required for `transient` and
  refused for `persistent`**, and names the **wild-context outcome only**
  (D-19).

### 7.2 Cultivated occurrences never read `successor` (D-13)

At the end of its corpse window a dead cultivated row crop or groundcover plot
becomes **empty and awaits deliberate replanting**. Death never replants a field
for the player. That is a rule of the render context, not authored content.

`await_replanting` is therefore the documented **name of that cultivated
outcome** and is **not a species-level YAML token**: a definition declaring
`successor: await_replanting` is rejected like any other unknown vocabulary.

A crop species that also grows wild — wheat, tomato — declares only its wild
successor. The schema stays one scalar, and content cannot contradict D-13.

### 7.3 Overrides

`overrides` is an optional list of `phase`/`cause` selectors carrying the same
fields, added only where a real behaviour needs one:

```yaml
corpsePolicy:
  visibility: persistent
  overrides:
    - phase: sprout
      visibility: transient
      durationDays: 60
      successor: reseed
    - phase: sprout
      cause: fire
      visibility: transient
      durationDays: 10
      successor: reseed
```

- **An override declares a complete outcome. Nothing is inherited** from the
  species-level policy. The `transient`/`persistent` field coupling in §7.1 is
  validated per outcome, which only works if each outcome stands alone.
- **Only `phase` and `cause` select an override**, and each may be omitted as a
  wildcard. An override selecting neither is rejected: it restates the
  species-level policy.
- **Precedence, most specific first:** an override naming both `phase` and
  `cause` outranks a `phase`-only override, which outranks a `cause`-only
  override, which outranks the species-level policy. Phase beats cause because
  retention is structural: D-12 states that cause changes the preferred art but
  does not override the structural default.
- **Duplicate override selectors are rejected**, the same way duplicate texture
  selectors are.

The `cause: fire` entry above illustrates the schema; **no shipped species
declares a cause override today**, and D-12 deliberately keeps cause from
moving the structural default. The mechanism exists so a future species that
genuinely needs it can declare one rather than have retention inferred.

### 7.4 Freezing and snapshotting

A corpse **freezes its phase and annual stage at death** (D-10). A flowering
plant that dies stays semantically flowering-dead rather than continuing to
cycle with the calendar. Missing exact art changes only which texture is
displayed; it never changes the stored death state.

The **selected retention outcome is snapshotted when death occurs**, so a later
content-pack edit cannot silently reinterpret an existing save.

### 7.5 Class defaults

These are **authoring guidance** for new species, not runtime inference.
Nothing at runtime derives a policy from a placement category (D-14).

| Class | Default |
|---|---|
| mature trees | `persistent`, with a `transient` sprout override |
| cacti (their own structural class, D-20) | `persistent` mature, `transient` 60-day `reseed` sprout override |
| tree sprouts, wildflowers, grasses, ferns, crops | `transient`, 60 days |
| bushes | decided per species; bracken fern and red raspberry are both `transient` |

Mature trees persist regardless of what killed them. Saguaro — the only shipped
cactus and the arc's pilot species (D-16), demonstrated end to end by EFM-9 —
declares a persistent mature corpse with a transient 60-day reseeding sprout
override and keeps `lifecycle: perennial`. D-20 adds no `lifecycle:` token.

### 7.6 The legacy default

A species with **no `corpsePolicy` keeps today's behaviour**: `transient`
visibility for 60 days with the wild successor `reseed`, matching
`World.Flora.Growth.deadWindowDays`. A cultivated occurrence under that legacy
default still becomes empty and awaits replanting per §7.2. Repository-owned
species declare their policy explicitly; the omitted form exists so older
content keeps loading.

---

## 8. Compatibility promises

1. **Existing declarations keep loading and rendering unchanged.** `phases`,
   `annualCycle`, `cycleOverrides`, and `harvestable.harvested_texture` are
   untouched by this contract, and their normalization (§1.1) reproduces
   today's texture for today's requests.
2. **Undeclared cultivated variants change no visual** (§6).
3. **Persisted state records semantic tags only** — context, condition, cause,
   phase, stage, the retention outcome and its expiry — and **never a texture
   handle or a resolved path** (D-6). Every visual is re-resolved from tags, so
   art can be added, corrected, or renamed without migrating a save.
4. **No new `lifecycle:` vocabulary** is introduced (D-20), and no token is
   added to the phase or stage vocabularies (§1).

---

## 9. Scope of this document

This contract is documentation. It introduces no loader, resolver, persistence,
or retention behaviour. Its implementers are EFM-2 (loading and auditing
declarations), EFM-3 (the pure resolver), EFM-4 through EFM-6 (occurrence
identity, render context, persisted condition), EFM-7 and EFM-10 (the
mutation/query seam and retention), and EFM-8/EFM-9 (pilot art and the saguaro
demonstration).

Hazard producers — temperature exposure, drought, fire contact, disease, direct
damage — are out of scope for the whole epic. This contract fixes the cause
vocabulary they will use; it computes no exposure.
