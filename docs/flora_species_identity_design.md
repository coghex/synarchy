# Flora species identity design

Flora species are identified at runtime by a sequential `FloraId` handed out in
the order `data/flora/*.yaml` happens to be enumerated, and that ordinal is
what the save persists for planted flora, crop plots, and plant designations,
and what salts generated placement. This design makes species identity a
property of the authored definition (its YAML `name`) rather than of the host
filesystem, so the same seed grows the same forest on every platform, and
adding, renaming, or reordering a species can never silently turn a player's
crop into a different plant. It benefits players carrying saves across builds
and platforms, and maintainers adding flora content. Origin: holistic audit
HPA-20 (`docs/holistic_project_audit_findings.md`).

Design state: `ready for issue processing`

Status legend: `[ ]` unprocessed · `[#N]` linked to issue N · `[no-issue]`
reviewed and deliberately not tracked separately · `[deferred]` blocked on a
concrete precondition

## Processing status

- [x] EPIC. Make flora species identity independent of file-enumeration order — [#2236]
- [x] FSI-1. Register and place flora independently of enumeration order — [#2241]
- [x] FSI-2. Persist flora species references by authored name — [#2243]

## Epic contract

- **Goal:** Generated flora for a given seed is identical on macOS and Linux
  and unchanged by the order in which species definitions are discovered, and
  every persisted species reference (planted flora edits, crop plots, plant
  designations) survives a catalog reorder, insertion, or rename with its
  meaning intact or fails load with a named reference — never a silent remap.
- **Done when:** a headless test registers the shipped catalog in two
  different orders and observes identical placement and identical
  `FloraInstanceId`s; a save written with species names reloads its crops and
  designations by name; a save naming a species this build lacks is refused
  with the name; and `world_check`-style cross-platform evidence exists for
  flora (a golden species layout for one seed asserted in CI).
- **Users and operators:** players loading saves across platforms and builds;
  maintainers adding or renaming flora species; the save-compatibility audit
  and its fixture corpus.
- **Arc label:** `save-load` (existing) proposed for the umbrella; FSI-1 also
  carries `worldgen`.

## Current state and evidence

Verified on master `5500e9771`.

- **Ids are load-order ordinals.** `nextFloraId` hands out sequential `Word16`
  ids as `engine.loadFloraYaml` encounters definitions
  (`src/Engine/Scripting/Lua/API/YamlTextures.hs:284-286`;
  `src/World/Flora/Types.hs:282-297`, `fcNextId` starts at 1).
  `scripts/startup_loader.lua` feeds `data/flora` through `addYamlDir`, i.e.
  raw `engine.listFiles` order; that verb's haddock says the order is
  OS-dependent and must *not* be sorted there because "flora IDs are allocated
  in load order and salt worldgen placement"
  (`src/Engine/Scripting/Lua/API/Core.hs:553-556`). #1232 gave items a
  canonical recursive order and deliberately left flora on the flat,
  OS-ordered walk for the same reason.
- **The order already differs by platform.** On the maintainer's macOS
  checkout `os.listdir('data/flora')` returns
  `temperate_deciduous, temperate_shrubs, saguaro, temperate_wildflowers,
  tropical, boreal_evergreen, crops, wetlands`; ext4 returns a different hash
  order. `tools/world_check.py` explicitly excludes flora from its baselines,
  so the "worldgen is bit-identical across macOS and Linux" claim has never
  covered flora.
- **Placement is salted by list position.** `worldGenSpecies` is
  `HM.toList (fcWorldGen cat)` (`src/World/Flora/Types.hs:320-322`) and
  `placeTileFlora` rolls with `floraHash seed gx gy (i + 100)` and salts
  `mkInstance` with `i * 10 + j`, `i` being that list index
  (`src/World/Flora/Placement.hs:118-146`). #1854 made the per-instance
  `FloraInstanceId` derive from the species `name` and per-tile ordinal `j`
  instead (`src/World/Flora/Identity.hs:146-157`), and left the rolls on `i`
  on purpose ("changing that would change which plants exist in every shipped
  world", `Placement.hs:103-110`).
- **Three durable references persist the ordinal.** `WePlaceFlora` and
  `WePlaceFloraWithId` in `world-edits` (v2), and `CropPlotDTO.cpiSpecies` and
  `PlantDesignationDTO.ptiCrop` in `world-activity` (v5, `padCropPlots` /
  `padPlant`) — `src/World/Save/Component/Page.hs:415-423,694-713,1954-1960`.
  The catalog that gave those numbers meaning is not in the save
  (`docs/persistence_state_inventory.md` classifies it as rebuilt at boot).
- **Validation is existence-only and incomplete.** `missingFloraReferences`
  (`src/World/Save/Types.hs:1290-1310`) checks that each numeric id resolves
  in the current catalog; it covers the edit log and crop plots but not plant
  designations. A reorder that keeps the species count passes and reinterprets
  every reference.
- **A name key already exists.** `FloraSpecies.fsName` is the YAML `name`,
  `findSpeciesByName` looks it up (`src/World/Flora/Types.hs:165,311-314`),
  and #1854 already treats the name as the stable species identity for
  instance ids. Nothing rejects two species sharing a `name`.
- **Lua sees numeric ids.** `engine.registerFloraSpecies` (`API/Flora.hs:64-69`,
  used by probes such as `flora_growth_probe`'s `probe_berry`) allocates the
  next id at runtime, and the plant/flora verbs take and return numeric ids
  (`API/Plant.hs:115,265`). No Lua save module persists a species id
  (`scripts/lib/save_modules.lua`, `unit_ai_save_refs.lua` searched).
- **Adjacent arcs.** #1854 (closed) owns instance identity and Chop keying by
  `FloraInstanceId`; `docs/designation_tools_design.md` and
  `docs/environmental_flora_mortality_design.md` restate its rule and plan no
  species-key persistence. Open epics #1837 (designation tools) and #1997
  (world streaming) do not touch species identity.

## Desired experience

- A player who creates a world on macOS and continues it on Linux (or after a
  flora species is added upstream) sees the same trees where they were and the
  same crops in their plots.
- A maintainer adds `data/flora/new_species.yaml` or renames a file, runs the
  game, and nothing about existing worlds or saves changes except that the new
  species can now appear where its placement rules say.
- A save that references a species this build no longer ships fails to load
  with a message naming the species and where it was referenced, exactly as a
  missing unit or item definition does today.
- Probes and tests that register species at runtime keep working; their ids
  remain session-local.

## Scope

### In scope

- Deterministic, platform-independent flora registration order.
- Placement rolls and instance salts that do not depend on a species' position
  in the catalog list.
- Persisting species by authored name in `world-edits` and `world-activity`,
  with frozen DTOs and explicit migrations.
- Name-based reference validation at save and load, including plant
  designations.
- Enforcing species-name uniqueness at content load.
- A cross-platform flora determinism gate.

### Out of scope

- Per-instance identity, Chop keying, and regrowth timers (#1854, landed).
- Environmental flora mortality (`docs/environmental_flora_mortality_design.md`).
- Designation-tool UX (#1837).
- Runtime material registrations lost at world init (HPA-27) and flora YAML
  enum validation (HPA-45), which are separate findings.
- Changing the Lua API's numeric species ids for the session; only persistence
  changes.
- Persisting the flora catalog itself in the save.

## Design

### Species key

The authored YAML `name` is the durable species key. It is already the input
to `generatedFloraInstanceId` (#1854), so nothing new is invented. Content
load refuses a file that would register a `name` already present in the
catalog (D-3), which is what makes the key a key.

### Runtime handle

`FloraId` remains the `Word16` runtime handle every hot path and the Lua API
use. It is assigned when the catalog is built and is never written to a save
by new code. Its assignment becomes deterministic: `startup_loader` enumerates
`data/flora` in `canonicalFileOrder` (the byte-wise order #1232 defined for
items), and within a file definitions register in document order. Runtime
registrations (`engine.registerFloraSpecies`) continue to take the next id
after the shipped catalog and are session-local by construction.

### Placement independence (FSI-1)

`placeTileFlora` stops using the species' list index. The roll hash and the
instance salt take a stable per-species value derived from the name (the same
`hashText` the identity module uses), and `worldGenSpecies` yields species in
canonical name order so per-tile ordinals `j` and instance counts are
reproducible regardless of HashMap traversal. Consequence: generated flora for
every existing seed changes once (D-1). After that, adding, removing, or
reordering a species cannot move any other species' plants, which is stronger
than today even on one platform.

### Persistence (FSI-2)

- `world-edits` v2 → v3: the two planting constructors carry the species name
  instead of `FloraId`; v1 and v2 DTOs are frozen verbatim and decode through
  a migration that resolves the numeric id against the *current* catalog.
- `world-activity` v5 → v6: `CropPlotDTO` and `PlantDesignationDTO` carry the
  species name; v5 is frozen and migrated the same way.
- Resolution at load: a name that `findSpeciesByName` cannot resolve is a
  `MissingFloraRef` naming the source, page, coordinate, and species name; the
  check now also walks plant designations.
- Legacy numeric ids can only be resolved against the catalog of the loading
  build, because the catalog that minted them was never saved. That is
  today's behaviour, applied one last time, and is recorded as a documented
  limitation (D-2).
- Every component bump registers a new fixture in the save-compatibility
  corpus per `tools/save_compat_audit.py --generate-session`.

### Lua boundary

Unchanged for the session: numeric ids in and out. The plant designation
verbs keep accepting a numeric id and resolve it to the name at the
persistence boundary, so no script changes are required.

## Proposals

- **P-1.** Adopted as D-1.
- **P-2.** Adopted as D-4.
- **P-3.** Adopted as D-2.
- **P-4.** Adopted as D-3.
- **P-5.** Adopted as D-5.

## Decisions

### D-1. Generated flora is relaid out once so placement no longer depends on catalog order

Placement rolls and instance salts are keyed by species name rather than list
position, and species are visited in canonical name order. Every existing seed
therefore grows a different generated layout once, after which adding,
removing, or reordering a species cannot move any other species' plants, and
macOS and Linux agree. Consequences: existing saves see generated trees move
on next chunk regeneration; Chop designations keyed to a `FloraInstanceId`
that no longer exists are dropped at reconciliation; the save version is
bumped per the worldgen-output convention (flora is outside `world_check`'s
baselines, so no recapture). Signed off 2026-09-01 (Q-1). Affects FSI-1.

### D-2. Legacy numeric species ids resolve against the loading build's catalog

A pre-name save's `FloraId` values in `world-edits` v1/v2 and
`world-activity` v5 are mapped to names through the catalog of the build that
loads them, which is today's semantics applied one last time. A save minted
under a different enumeration order may already carry the wrong species; that
limitation is documented rather than guarded, because the minting catalog was
never persisted and refusing pre-name saves would strand every existing one.
Signed off 2026-09-01 (Q-2). Affects FSI-2.

### D-3. A duplicate species name is refused at content load and at runtime registration

A flora file that would register a `name` already in the catalog is refused
whole, with the file and name logged, following `loadYamlList`'s
all-or-nothing rule; `engine.registerFloraSpecies` refuses a colliding name
the same way. This is what makes the authored name a key. Signed off
2026-09-01 (Q-3). Affects FSI-1 (where the check lands) and FSI-2 (which
relies on it).

### D-4. FSI-1 lands before FSI-2

The slices are independent, but the ledger and delivery order put FSI-1 first:
it is the smaller change, it establishes the cross-platform gate, and FSI-2's
name-based persistence is equally valid under either placement scheme. FSI-2
stays filed as `independent`, so both may be worked in parallel. Signed off
2026-09-01 (P-2).

### D-5. The cross-platform flora gate is a headless golden test, not a world_check baseline

For seed 42 at world size 64, the species name at a fixed sample of tiles is
asserted against a checked-in list in the headless suite, so a divergence
fails CI on either platform on every run. Flora stays outside
`tools/world_check.py`'s 21-seed baselines, so no tier-3 recapture is ever
owed for a flora change. Signed off 2026-09-01 (P-5). Affects FSI-1.

## Open questions

### Q-1. Is a one-time relayout of generated flora in existing saves acceptable?

Resolved by D-1.

### Q-2. How are legacy numeric species ids in existing saves resolved?

Resolved by D-2.

### Q-3. Does a duplicate species `name` become a load-time refusal?

Resolved by D-3.

## Verification strategy

- **Order independence:** a headless spec builds the shipped catalog twice in
  different registration orders and asserts identical `placeTileFlora` output
  (species per tile, instance counts, `FloraInstanceId`s) for a fixed chunk.
- **Cross-platform determinism:** the golden species-layout test (D-5) runs in
  the headless suite on every CI run; `tools/world_check.py` keeps excluding
  flora, so no baseline recapture is involved, but the save version is bumped
  per convention for a worldgen-output change.
- **Persistence:** `--match "persistence contract"` extended with named
  species references; new fixtures for `world-edits` v3 and `world-activity`
  v6 in `test-headless/data/save-compat/` registered in
  `docs/save_compat/manifest.json`; legacy v2/v5 fixtures still decode;
  `--match "persistence reference integrity"` covers a missing species name in
  each of the three reference sites, including plant designations.
- **Content load:** a duplicate-name fixture is refused with the file named.
- **Probes (opt-in):** `flora_growth_probe.py`, `till_probe.py`,
  `persistence_contract_probe.py`, `save_compat_audit.py` (no engine).
- **Documentation:** `docs/engine_contracts.md` records the species-key rule
  and the placement-salt rule; `docs/persistence_state_inventory.md` rows for
  the three references change from `FloraId` to species name.

## Delivery plan

### FSI-1. Register and place flora independently of enumeration order

- **Outcome:** the same catalog produces the same generated flora regardless
  of the order its files are discovered, on every platform.
- **Scope:** canonical `data/flora` enumeration in `startup_loader`;
  `worldGenSpecies` in canonical name order; placement rolls and instance
  salts keyed by species name instead of list index; duplicate-name refusal
  at content load and at `engine.registerFloraSpecies`; the two-order
  equivalence test and the golden layout test; save-version bump; Chop
  reconciliation drops designations whose instance no longer exists after a
  relayout.
- **Phase:** 1
- **Depends on:** `none`
- **Ordering:** `can land first`
- **Relevant decisions:** D-1, D-3, D-5
- **Acceptance signals:** two-order equivalence spec passes; golden layout
  spec passes on macOS and Linux CI; `engine.listFiles` haddock's "do not
  sort" caveat is retired; `flora_growth_probe.py` and `till_probe.py` pass.
- **Out of scope:** any persisted format change; legacy id resolution.
- **Open questions:** `None`

### FSI-2. Persist flora species references by authored name

- **Outcome:** planted flora edits, crop plots, and plant designations are
  saved and validated by species name; a missing species fails load by name.
- **Scope:** `world-edits` v3 and `world-activity` v6 with frozen v2/v5 DTOs
  and migrations; `missingFloraReferences` by name and over plant
  designations; Lua plant verbs resolve numeric ids at the persistence
  boundary; fixtures and manifest baselines; inventory and contracts doc rows.
- **Phase:** 2
- **Depends on:** `none` (independent of FSI-1; D-4 orders it second)
- **Ordering:** `independent`
- **Relevant decisions:** D-2, D-3, D-4
- **Acceptance signals:** persistence-contract and reference-integrity specs
  pass with named references; legacy fixtures decode; `save_compat_audit.py`
  passes with the new baselines; `persistence_contract_probe.py` passes.
- **Out of scope:** placement changes; persisting the catalog.
- **Open questions:** `None`
