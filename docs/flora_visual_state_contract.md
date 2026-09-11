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

- a legacy `phases` entry tagged `dead` **enters the variant order** as a
  declaration with `condition: dead` and every other axis wildcard — key
  `(1,0,0,0)`, the species' Tier 1 generic dead art (§5);
- its `cycleOverrides` entries whose `phase` is `dead` enter it as
  stage-specific generic-dead declarations (`condition: dead` plus that
  `stage`) — key `(1,0,0,1)` — not as living art. Because `(1,0,0,1)` outranks
  `(1,0,0,0)`, a species with both shows its stage-specific dead art, which is
  what it shows today;
- a natural-lifespan death requests `condition: dead`, `cause: natural`, and the
  **last living** phase and stage frozen at death (§7.4), and resolves through
  those two entries to the same `dead.png` the species shows today.

**Legacy LIVING entries are a different matter, and they are deliberately not
ranked in the variant order.** Every other `phases`, `annualCycle`, and
`cycleOverrides` entry describes a `wild` + `alive` state, but it keeps its own
existing precedence and is resolved as one unit at §3.3, after every
`textureVariants` candidate and before the base texture.

That is a compatibility requirement, not a simplification. Today's living
precedence puts the annual stage **above** the life phase: `resolveSpeciesTexture`
uses the active `annualCycle` stage's texture whenever the species has a cycle,
and falls back to the phase texture only when it has none. Ranking those
entries by the §3.2.1 key would invert that — phase outranks stage there — and
silently change what every shipped species draws. §3.3 preserves the existing
order exactly.

The two orders do not conflict, because they never compete: an author adding a
`textureVariants` entry is deliberately overriding the legacy result, and every
variant candidate is exhausted before §3.3 is consulted.

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

Matching is defined in two halves, because context does not behave like the
other four axes.

**Semantic match.** A declaration semantically matches a selector when, for each
of the four SEMANTIC axes — phase, stage, condition, cause — it either omits the
axis or names exactly the selector's value.

**Context attempt.** Context is not matched against the request directly.
Resolution runs a sequence of **context attempts**, and a declaration is
eligible in an attempt when it either omits `context` or names exactly that
attempt's context:

- a `wild` request has one attempt: `wild`;
- a `cultivated` request has two, **`cultivated` then `wild`**, and by
  invariant 3 both are made at each semantic key before moving to the next one
  (§3.2.1).

So an explicit `context: wild` declaration IS reachable from a cultivated
request — it is what the second attempt finds, which is exactly step 2 of the
ladder — while a `cultivated` declaration is never reachable from a wild
request, which is what makes cultivated art an override rather than a parallel
lifecycle (D-4).

A declaration **matches** a request in a given attempt when it both
semantically matches and is eligible in that attempt.

- **A SEMANTIC axis the declaration names cannot match a request that lacks a
  value for it**, and a declaration no request can ever reach is **rejected**. This
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
- **Omitted context is not the same as `context: wild`.** An omitted context is
  eligible in every attempt; `context: wild` is eligible only in the `wild`
  attempt. The two are distinct selectors and therefore not duplicates under
  rule 2, even when every other axis agrees. A cultivated request reaches both,
  the omitted one at its first attempt and the explicit wild one at its second.
- **Overlap is normal and is resolved by the order, not by declaration order.**
  Several declarations may match one request. The winner is decided solely by
  the candidate order in §3.2.1 — the first (key, attempt) pair at which any
  declaration matches wins. File order, alphabetical order, and `HashMap`
  traversal order never affect the result.
- **Within one (key, attempt) pair, an explicit `context:` beats a context-less
  declaration.** If both `{context: cultivated, condition: dead}` and
  `{condition: dead}` are declared and a cultivated dead plant is drawn, the
  cultivated one wins at the first attempt: it is the exact selector invariant 2
  names. This is a total rule because two declarations tying there are
  duplicates, which rule 2 already rejects.
- **Normalized legacy declarations participate in rule 2.** A species that
  authors `phases: [{tag: dead, …}]` *and* a `textureVariants` entry reading
  `condition: dead` with no other axis has declared the same selector twice, and
  the file is rejected. Authors migrating legacy art remove the legacy entry or
  make the new entry more specific.

**Worked examples.** For a species declaring exactly the four entries in the
block above:

| Request | Winner | Why |
|---|---|---|
| cultivated, matured, flowering, alive | `cultivated_matured_flowering.png` | exact declared selector: the first living key `(1,1,0,1)`, §3.2.2 |
| wild, matured, flowering, alive | the existing `annualCycle`/`cycleOverrides` result | the only living entry names `context: cultivated`, so no variant matches; §3.3 |
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

A living request never reaches steps 1 through 8: they are all dead
candidates. Its own order is §3.2.2.

### 3.2.1 The total order behind the ladder

Those ten steps are the **trace** of one general rule for one example request,
not an enumeration of every case. The rule itself is total: it orders every
declaration §2 permits, for every request.

For each declaration that **matches** the selector (§2.1), record which of the
four semantic axes it **names**:

```
key(d) = ( names condition, names phase, names cause, names stage )
```

Candidates are tried in **descending lexicographic order of that key**. The
axis order inside the key is exactly D-11's priority — death, then phase, then
cause, then annual stage — which is what makes invariants 4 through 7 hold by
construction.

**Each key is fully attempted in every context before the next key is
considered.** That interleave is invariant 3: a cultivated request tries
`cultivated` and then `wild` at one key, and only then weakens the key. Two full
passes would be wrong — they would let cultivated generic-dead art beat exact
wild art.

Within one (key, attempt) pair, an **explicit `context:` beats a context-less
declaration**, because a declaration omitting context is eligible in every
attempt and is the shared default (D-4).

Each numbered step in §3.2 is one value of that key. For the worked
fire-killed cultivated flowering sprout:

| Step | key | Reading |
|---|---|---|
| 1, 2 | `(1,1,1,1)` | exact: condition, phase, cause, stage |
| 3 | `(1,1,1,0)` | stage dropped |
| 4 | `(1,1,0,1)` | cause dropped, stage kept |
| 5 | `(1,1,0,0)` | phase-preserving generic dead |
| 6 | `(1,0,1,0)` | generic cause-specific dead |
| 7 | `(1,0,0,1)` | stage-specific generic dead |
| 8 | `(1,0,0,0)` | generic dead |
| 9 | — | the best corresponding living state (§3.3) |
| 10 | — | the species base texture |

Descending lexicographic order on those keys reproduces steps 1 through 8 in
exactly the decided sequence, so the ladder and the rule never disagree.

**Every permitted mask has a place.** The keys above are the ones the example
happens to declare; the order is defined over all sixteen, so a legal mask the
ten-step trace does not name is still ranked. A declaration reading
`{stage: flowering, condition: dead, cause: fire}`, for instance, has key
`(1,0,1,1)` and is tried after step 5 `(1,1,0,0)` and before step 6 `(1,0,1,0)`.
It is reachable and deterministic; rule 7 does not reject it.

Ties are impossible: two declarations matching in the same attempt at the same
key with the same context explicitness name the same axes with the same values,
which rule 2 already rejects. The winner is therefore a function of the selector and the
declared set alone — never of file, alphabetical, or `HashMap` order.

**Context is outside the key on purpose.** It is the attempt loop of §2.1, run
inside each key, never a reason to weaken a semantic axis — which is what
invariant 3 says.

### 3.2.2 Living requests

A living request carries `condition: alive` and no cause, so by rule 4 no
matching declaration can name a cause: its key is always `(c, p, 0, s)`. The
same descending order applies, giving eight candidate keys before §3.3:

`(1,1,0,1)` → `(1,1,0,0)` → `(1,0,0,1)` → `(1,0,0,0)` →
`(0,1,0,1)` → `(0,1,0,0)` → `(0,0,0,1)` → `(0,0,0,0)`

then the best corresponding living state (§3.3), then the base texture.

So for a living cultivated flowering sprout, `{phase: sprout, condition: alive}`
— key `(1,1,0,0)` — beats `{stage: flowering, condition: alive}` — key
`(1,0,0,1)`. Phase outranks annual stage for the living axes exactly as it does
for the dead ones (D-11, invariant 7).

**Numbering.** The step numbers in §3.2 name positions in the dead trace only.
A living request does not "enter at step 9": it runs its own eight keys above
and only then reaches §3.3 and the base texture, which are the two steps both
traces share.

### 3.3 "The best corresponding living state"

This is not a new mechanism, and it is not ranked by the §3.2.1 key. It is one
fixed step, reached by dead and living requests alike once every
`textureVariants` candidate is exhausted, and it is exactly today's living
resolution in `World.Flora.Render.resolveSpeciesTexture` — including its
stage-above-phase precedence (§1.1) — driven by the selector's frozen phase and
stage rather than by the current age and day of year:

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
- **Unreachable overrides are rejected**, by exactly the rules §2.1 applies to
  `textureVariants`, so EFM-2 has one validation to implement rather than two:
  a `phase` absent from the species' own `phases` is refused, and `phase: dead`
  is refused outright, because §1.1 makes `PhaseDead` a legacy authoring token
  that no runtime selector ever carries. A `cause` outside the §1 vocabulary is
  refused like any other unknown token. A corpse policy is selected by the
  frozen phase and the recorded cause (§7.4), which are the same values a
  texture selector carries, so an override no selector can carry is the same
  silent authoring dead end `requireDeclared` has refused since #2315.

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

A species with **no `corpsePolicy` takes today's values**: `transient`
visibility for 60 days with the wild successor `reseed`, matching
`World.Flora.Growth.deadWindowDays`. A cultivated occurrence under that legacy
default still becomes empty and awaits replanting per §7.2 — the legacy default
supplies today's window and today's WILD successor, not an exemption from D-13.
That is compatibility change (b) in §9, and it is deliberate. Repository-owned
species declare their policy explicitly; the omitted form exists so older
content keeps loading.

---

## 8. Harvest depletion is a sixth state, and death supersedes it

`harvestable.harvested_texture` is **not** one of the five axes, and this
contract does not fold it into them. Depletion is an orthogonal presentation
state driven by a regrowth timer, not by the occurrence's semantic condition.

Today it is a hard override that bypasses selection entirely:
`World.Render.FloraDraws` draws `fhHarvestedTexture` for any instance in the
harvest map and **never calls `resolveFloraTexture` at all**. Two rules keep
that from contradicting invariant 4:

1. **While the occurrence is alive, depletion still wins.** A harvested living
   plant draws its `harvested_texture` ahead of every living `textureVariants`
   candidate, exactly as it does today. Nothing about the common path changes.
2. **Once the occurrence is dead, condition supersedes depletion.** The dead
   candidate order in §3.2 runs, and `harvested_texture` is not consulted.

Rule 2 is a **deliberate, narrow behaviour change**, called out here rather than
left for a later child to discover. Today a plant that is inside its regrowth
window when it reaches its lifespan keeps drawing harvested stubble instead of
its `dead.png`; under this contract it looks dead. That is the change invariant
4 requires — a plant killed by fire while depleted must not draw the art of a
living, recently-picked plant — and it is also the behaviour a player expects,
since a dead plant does not regrow.

Scope: no producer can create a non-natural death until a later arc, so the only
occurrences this reaches today are harvestable species dying at their natural
lifespan. **EFM-7 owns this change** — it is the child that moves rendering onto
the condition seam — and owes the gate: a harvestable species inside its
regrowth window, killed, renders its dead candidate rather than
`harvested_texture`, and the same species alive and depleted still renders
`harvested_texture`.

---

## 9. Compatibility promises

1. **Existing declarations keep loading unchanged.** `phases`, `annualCycle`,
   `cycleOverrides`, `harvestable.harvested_texture`, and `lifecycle` are
   untouched as SCHEMA. Nothing an existing file authors is rejected, and the
   living precedence those entries resolve by is preserved exactly (§1.1, §3.3).
   A species that declares no `textureVariants` renders identically for every
   LIVING request.

   **Two deliberate behaviour changes are accepted, both listed here rather
   than left for a child to discover:**

   | # | Change | Scope today | Owner |
   |---|---|---|---|
   | a | A depleted plant that dies draws dead art rather than `harvested_texture` (§8 rule 2) | harvestable species reaching their natural lifespan | EFM-7 |
   | b | A **cultivated** occurrence whose corpse window expires becomes empty and awaits replanting instead of reseeding (§7.2) | row crops and groundcover `CropPlot`s, under both an explicit and an omitted `corpsePolicy` | EFM-10 |

   Change (b) is unavoidable, not incidental: `World.Flora.Growth.floraGrowth`
   today derives a generation arithmetically — `gen = floor(total / (lifespan +
   deadWindowDays))` — with no occurrence state and no notion of context, so
   every occurrence wraps to a fresh sprout. D-13 requires cultivated
   occurrences to stop doing that. §7.6's "keeps today's behaviour" therefore
   means the legacy default supplies today's 60-day window and today's WILD
   successor; it cannot preserve wild reseeding for a cultivated occurrence,
   because the two are the same code path today. EFM-10 owes the gate on both
   sides: a wild occurrence still reseeds at expiry, and a cultivated one
   becomes empty.
2. **Undeclared cultivated variants change no visual** (§6).
3. **Persisted state records semantic tags only** — context, condition, cause,
   phase, stage, the retention outcome and its expiry — and **never a texture
   handle or a resolved path** (D-6). Every visual is re-resolved from tags, so
   art can be added, corrected, or renamed without migrating a save.
4. **No new `lifecycle:` vocabulary** is introduced (D-20), and no token is
   added to the phase or stage vocabularies (§1).

---

## 10. Scope of this document

This contract is documentation. It introduces no loader, resolver, persistence,
or retention behaviour. Its implementers are EFM-2 (loading and auditing
declarations), EFM-3 (the pure resolver), EFM-4 through EFM-6 (occurrence
identity, render context, persisted condition), EFM-7 and EFM-10 (the
mutation/query seam and retention), and EFM-8/EFM-9 (pilot art and the saguaro
demonstration).

Hazard producers — temperature exposure, drought, fire contact, disease, direct
damage — are out of scope for the whole epic. This contract fixes the cause
vocabulary they will use; it computes no exposure.
