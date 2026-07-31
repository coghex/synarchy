# Location-placement frequency sweep (#997)

The bounded, reproducible measurement requirement #997 asks for before
the placement guarantee lands: how often does world generation place
**zero** locations?

This is a **measurement**, not a gate. The standing gates are the
`Location overlay (#89)` hspec group (always-blocking) and
`tools/location_overlay_probe.py`'s phase-9 placement matrix. The sweep
is deliberately not folded into either — it is a one-off record,
re-runnable via `tools/location_placement_sweep.py` when a placement
change makes a fresh number worth having.

## Inputs

| | |
|---|---|
| Repository revision | `c43fc272f49ca6472834585db5a25f415afc5de4` (`master`) |
| Working tree | Clean **for all source, scripts, data and config**; only `CLAUDE.md` and `docs/code_health_findings.md` were modified, so the binary under test is exactly that revision |
| Measured against | **Pre-change** code — the sweep was run from a checkout of the base revision, before any of this PR's edits |
| Tool | `tools/location_placement_sweep.py` (added by this PR; the sweep was driven by an identical copy at the base revision) |
| Generation config | `config/world_gen_default.yaml`, unchanged — loaded at boot by `Engine.Core.Init` in every boot mode, so headless generation uses the same configuration the GUI defaults to |
| Plate count | 10 for every run (the GUI/default) |
| Location definitions | `data/locations/ruin_small.yaml` — the only registered definition |
| Platform | macOS / aarch64. Worldgen output is bit-identical across macOS/aarch64 and Linux/x86_64, so the tuples reproduce anywhere |

Each world was generated in **its own engine process** (`--headless`),
so no run could influence the next and any single row reproduces on its
own:

```bash
python3 tools/location_placement_sweep.py --single --seed <seed> --size <size>
```

## Sweep shape

Requirement 1 asks for 1×size-256, 2×size-128 and 4×size-64 per base
seed over base seeds `{0, 1, 2}`. Generation is a pure function of the
complete tuple, so repeating a tuple regenerates an identical world
rather than sampling a new one; per the approved review, the repeats are
spread over distinct seeds instead. For base seed `s` and run index `r`
within a (base seed, size) cell, `seed = s + 3*r`:

- size 256 → seeds 0, 1, 2
- size 128 → seeds 0–5
- size 64 → seeds 0–11

21 runs, 21 distinct worlds.

## Results

**0 of 21 worlds placed zero locations — an observed zero-placement
frequency of 0.0%.**

| # | Seed | Size | Plates | `ruin_small` placed | Gen time |
|--:|-----:|-----:|-------:|--------------------:|---------:|
| 1 | 0 | 256 | 10 | 6 | 176.9 s |
| 2 | 1 | 256 | 10 | 6 | 238.0 s |
| 3 | 2 | 256 | 10 | 6 | 97.1 s |
| 4 | 0 | 128 | 10 | 6 | 14.5 s |
| 5 | 1 | 128 | 10 | 6 | 17.8 s |
| 6 | 2 | 128 | 10 | 6 | 21.9 s |
| 7 | 3 | 128 | 10 | 6 | 23.7 s |
| 8 | 4 | 128 | 10 | 6 | 19.0 s |
| 9 | 5 | 128 | 10 | 6 | 19.7 s |
| 10 | 0 | 64 | 10 | 6 | 4.2 s |
| 11 | 1 | 64 | 10 | 6 | 3.9 s |
| 12 | 2 | 64 | 10 | 6 | 6.1 s |
| 13 | 3 | 64 | 10 | 6 | 6.5 s |
| 14 | 4 | 64 | 10 | 6 | 5.2 s |
| 15 | 5 | 64 | 10 | **4** | 5.7 s |
| 16 | 6 | 64 | 10 | 6 | 5.5 s |
| 17 | 7 | 64 | 10 | 6 | 4.0 s |
| 18 | 8 | 64 | 10 | 6 | 7.3 s |
| 19 | 9 | 64 | 10 | 6 | 5.2 s |
| 20 | 10 | 64 | 10 | 6 | 6.8 s |
| 21 | 11 | 64 | 10 | 6 | 5.7 s |

Zero-placement tuples found: **none**.

## Reading this honestly

The sweep found no reproducer, so it does **not** establish a frequency
— it bounds one. Zero hits in 21 independent worlds puts the rate under
roughly 14% at 95% confidence (rule of three, 3/21), pooled across all
three sizes; that is consistent with both "rare" and "the reported world
was unlucky in a way these 21 seeds were not". #997 deliberately forbids
widening this into an open-ended seed search, so that is where the
measurement stops.

What it is **not** is evidence the defect is unreachable. Seed 5 at size
64 already falls short of `ruin_small`'s `max_count: 6`, which is the
same filter pressure that reaches zero when it goes far enough:
`Location.Overlay`'s strict pass keeps only land chunks satisfying
`anchorOk ∧ (wantsWater ∨ dryEnough)`, and `ruin_small` carries no
water opt-out, so a world whose land is all coast or all lake/river
frontage rejects every candidate. That path is reproduced directly and
deterministically by the `guaranteed placement (#997)` hspec fixture,
which needs no generated world: give every chunk an ocean distance of 1
and the strict pass returns empty on a world that plainly has land.

The guarantee is therefore a floor for a rare case, not a fix for a
common one — which is also why every fixed-seed location gate
(`location_content_probe.py`, `location_embark_probe.py`,
`expedition_loop_probe.py`) is expected to be completely unperturbed:
the guarantee only runs when the strict pass placed nothing at all.
