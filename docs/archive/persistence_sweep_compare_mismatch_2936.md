# persistence_contract_sweep COMPARE_MISMATCH (#2936)

Verdict: **the comparison was at fault, not the save/load path.** The live
capture (`gen1`) and the re-saves (`gen2`-`gen4`) differ only in two playback
fields of a unit the Lua combat AI held in an animation-override stance, and
those fields are not something a load can restore. A load is a fixed point
everywhere else.

## Reproduction

`python3 tools/persistence_contract_sweep.py --skip-cross-probes
--keep-generations DIR` (size 64, seed 20260721, the sweep's default scenario)
reproduced `COMPARE_MISMATCH` on the first run, with `snapshot-differs` listing
`gen2`, `gen3` and `gen4` and `lua-component-differs` empty, as in the report.
The generations are kept at
`~/.local/state/synarchy/issue-2936/run1/` (`gen1`-`gen4`, `comparison.json`).
`--skip-cross-probes` reduces coverage by design, so that run exits non-zero
on its own "cross-referenced probes were skipped" check; the identity check is
reported separately.

## Which field

The envelope manifests of the four files (`gen1` is 10392756 bytes, the others
10392739) show two components differing:

- `metadata`: differs between every pair, as expected (save timestamps); the
  codec's `compare` does not look at it.
- `units`: 19696 bytes in `gen1`, 19679 in `gen2`/`gen3`/`gen4`, and
  `gen2` = `gen3` = `gen4` byte for byte.

The `units` difference is one string and nothing else. For the acolyte
"Jorin Kessler" (one of the sweep's two combatants: a player acolyte ordered
to attack a hostile one):

| generation | `uisCurrentAnim` | `uisAnimStart` |
|---|---|---|
| `gen1` (live capture) | `combat_idle_RH_dagger` | 47.16… |
| `gen2`, `gen3`, `gen4` | `idle` | 47.16… (same bytes) |

Every other unit field, every other component and every `lua.*` payload is
identical. The codec's own `compare` verdict on the unmodified code is the
`COMPARE_MISMATCH` above, because it compares the whole decoded
`SessionSnapshot` with `Eq`.

## Why a load cannot be a fixed point for it

- `scripts/unit_ai_combat_attack.lua` sets `unit.setAnimOverride(uid,
  "combat_idle_<class>")` on every AI tick a unit is in range and between
  swings, and an `attack_*` override for the swing itself.
- `Unit.Thread.publishToRender` resolves `uiCurrentAnim` every tick as
  `chooseAnim pose override stateAnim` and restarts `uiAnimStart` when the name
  changes.
- `uiAnimOverride` is runtime-only: `World.Save.Types.fromUnitInstanceSnapshot`
  resets it to `""` on every load (inventory row: "Reset to default - always
  cleared post-load").

So the saved `uisCurrentAnim` of a unit in a stance is a function of state the
load deliberately discards. The first tick after a load resolves the state
animation, the Lua AI re-sets the override only once the session runs again,
and the session is loaded paused. The re-save therefore records `idle`.

Why it is intermittent: whether the attacker is in a stance, mid-swing or still
walking at the instant the sweep pauses and saves `gen1` depends on real-time
scheduling between the AI loop and the sweep's console calls. The report's two
passing runs are consistent with the attacker's override being empty then; this
was not observed directly (their generations were not kept).

## Fix

`World.Save.Compare` (used by `synarchy-save-codec compare`) treats a chain of
saves as a live capture followed by re-saves taken across loads and asks two
questions:

- every re-save against the first re-save: exact, apart from the rebuilt
  physiology caches `withoutRebuiltCaches` already excuses. This is the
  fixed-point property a persistence defect breaks, and it covers the two
  playback fields.
- the first re-save against the live capture: the same, plus
  `uisCurrentAnim` and `uisAnimStart` excused. Facing, position, the sim
  mirrors (`uisActivity`, `uisPose`, `uisAnimStride`, `uisAnimReverse`) and all
  durable state stay compared.

The Hspec group `save generation comparison` pins the incident, the excused
fields, rejection of a durable difference against the live capture, exactness
of the playback fields between loaded generations, and the rebuilt-cache
exemption. The retained failing generations compare `COMPARE_OK` with the
fixed codec, and a fresh size-64 sweep run
(`~/.local/state/synarchy/issue-2936/run2/`) passes the four-generation
identity check with a `units` difference between `gen1` and `gen2` still
present (21498 vs 21463 bytes, 35 bytes here against 17 in run 1), so the fix was
exercised.

No serialized shape or meaning changed, so no component version moved.
