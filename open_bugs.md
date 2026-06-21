# Open bugs — review of commit `8c4560e` ("huge dig/combat new work")

Found during a bug-finding pass over the combat / fall / injury / movement core
of the commit. File:line references are against the committed tree.

---

## Bug 1 — Acolyte's vital subparts aren't flagged vital; brain/neck hits never instakill

**Severity:** gameplay correctness (data bug)

The vital instant-death rule keys off the **subpart's own `bpVital` flag** once a
part has subparts:

- `src/Combat/Resolution.hs:511` —
  `lethalHit = [(pid,k) | (pid,k,s) ← dist, s ≥ 1.0, isVitalId pid]`, and
  `macroLethal` only fires when `null dist` (i.e. the body has *no* subparts).
- `scripts/injuries.lua:513` (`lethalCause`) uses `w.vital`, and `getWounds`
  (`src/Engine/Scripting/Lua/API/Units.hs:1092`) populates that from the engine
  `bpVital` flag. The `VITAL_PARTS` table fallback at `injuries.lua:514` only
  applies when `w.vital == nil` — but the engine always sends a concrete
  `true`/`false`, so the fallback is effectively dead code.

In `data/units/acolyte.yaml`, **only `heart` is `vital: true`**. `brain`
(`:96`), `cervical_spine` (`:118`), `thoracic_spine`, `lumbar_spine`, and
`aorta` are all `vital: false`. So destroying an acolyte's brain or breaking its
neck (severity ≥ 1.0) **never triggers instant death** — it only fills the slow
`neuro` failure meter (~45 game-seconds).

This contradicts the code's own stated contract:

- `src/Combat/Resolution.hs:504` — *"destroys a VITAL part or subpart … brain,
  spine, heart"*
- `scripts/injuries.lua:19` —
  `VITAL_PARTS = { head, neck, torso, brain, skull, spine, chest }`

The bear/technomule are unaffected: they mark the macro head/neck/torso vital and
have **no subparts**, so head wounds land on the vital macro (`lethalHit` fires).
The acolyte alone received the detailed subpart breakdown without the vital flags
being propagated to its lethal subparts.

**Fix:** set `vital: true` on the acolyte's `brain` (and almost certainly
`cervical_spine` / `thoracic_spine` / `lumbar_spine`, and `aorta`).

---

## Bug 2 — Failure-meter deaths re-fire their death alert every tick on the corpse

**Severity:** visible bug (death-message + combat-log spam)

In `scripts/unit_resources.lua:883`:

```lua
if not tickInjuries(uid, info, pose0)
   and not tickFailureMeters(uid, dt) then
```

`tickInjuries` returns `false` for an already-dead unit
(`scripts/unit_resources.lua:755`), which the `and` chain reads as *"didn't die —
continue"*, so **`tickFailureMeters` runs on corpses**. It has no dead-guard
(`:826`–`845`).

Because the combat wound tick **skips dead units**
(`src/Combat/Wounds.hs:205` — `| uiPose inst == "dead" = (inst, NoChange)`), a
unit that died via a failure meter (brain death / suffocation / shock / organ
failure) keeps the driving injury — and therefore that meter — pinned at ≥1.0
forever. So `scripts/unit_resources.lua:839` re-hits `emitDeathAlert` +
`unit.kill` **every tick (10 Hz)**. `emitDeathAlert`
(`scripts/unit_resources.lua:243`) has no dedupe, so the event feed and combat
log spam *"X died of brain death"* indefinitely.

(Corpses provably persist in `umInstances` — that's exactly why `Wounds.hs:205`
special-cases them. `unit.getAllIds` returns all instances, dead included.)

Note: `tickResource` / `tickStarvation` (the post-mortem hydration/stamina drain)
also run on corpses for the same reason; they are a secondary spam vector if a
resource drains past its kill threshold after death.

**Fix:** skip dead units at the top of the per-unit loop in
`unitResources.update` (`if pose0 == "dead" then` continue), or early-return from
`tickFailureMeters` when the pose is `"dead"`.

---

## Minor notes (low impact)

- **`allocateSubparts` blunt can return `[]`.**
  `src/Unit/Injury.hs:295` — for a blunt hit, if a part's subparts have no
  bone/cartilage (structural) and no nerve/organ (deepSoft), the function returns
  `[]`, contradicting its doc comment *"Always returns at least one subpart so a
  connecting hit never whiffs."* `runResolution`'s `null dist` fallback produces a
  single macro wound instead, so there's no crash — just a missing subpart
  distribution for that edge case.

- **`height_low` / `height_high` default to `0.0`.**
  `src/Engine/Asset/YamlUnits.hs` now defaults these to `0.0`, and Resolution's
  reach filter requires `bpHeightHigh ≥ reachLo` (≈ 0.18 for a 1.8 m attacker). A
  *targetable* part that omits these becomes unreachable except via the
  `null reachable` fallback. The acolyte sets them on every targetable part, so
  this is currently latent — a trap for future body plans.

- **`UnitMoveTo` can set `Running` activity on a crawling unit.**
  `src/Unit/Thread/Command.hs:299` computes `activity = if isRunning then Running
  else Walking` without consulting pose; a crawling unit told to move fast gets
  `Running` for one tick before the movement tick re-derives the gait via
  `gaitForPose` (Crawling → Walking). Cosmetic, self-correcting.

---

## Still unreviewed (as of this pass)

- `scripts/combat_log.lua` (+406)
- `scripts/unit_info_v2.lua` (+441)
- `scripts/unit_ai.lua` (+143)
- `src/Unit/Render.hs` (+70)
- `src/Unit/HitTest.hs`, `src/Building/HitTest.hs`
- `src/Engine/Scripting/Lua/API/Combat.hs` (`combat.emitDeath`)
- new test files (`Combat/Severing.hs`, `Unit/Fall.hs`, `Unit/Injury.hs`)
