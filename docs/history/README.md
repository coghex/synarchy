# Historical audits (superseded — context only)

These are point-in-time investigation reports kept for provenance. They
predate later reworks and reference code that has since changed or been
removed. **Do not treat them as the current state of the system** — read
the live code and the agent memory notes instead.

- **`fluid_audit_2026-04.md`** — fluid generation/sim audit (2026-04-13).
  SUPERSEDED by the late-May 2026 fluid rewrite ("fluid rewrite 3").
  References functions that no longer exist (`drainOceanLakes`,
  `waterSideFaceQuads`). Its concrete bugs are obsolete; the durable
  fluid/water-table state lives in the agent memory notes
  (`plan_fluid_system`, `plan_water_table_rework`,
  `gotcha_ocean_chunk_boundary` — ocean chunk-boundary fix landed save
  v27) and the side-face subsystem now lives in `src/World/SideFace/`.

- **`pipeline_audit_2026-04.md`** — worldgen pipeline determinism
  investigation (2026-04-07). Conclusion (still useful as background):
  chunk generation is deterministic; the Sim thread (`src/Sim/`) is the
  source of dump-output non-determinism because it writes `wsTilesRef`
  independently. The "pure pipeline refactor" it scoped was deemed
  largely unnecessary.

- **`claude_md_2026-07-23_pretrim.md`** — verbatim snapshot of CLAUDE.md
  before its 2026-07-23 trim (3,927 → ~690 lines). The trim removed
  accumulated per-PR review-round narratives (mainly the #742-#750 UI
  responsive epic and the #756-#768 persistence overhaul) while keeping
  every durable contract in the live CLAUDE.md. Consult this snapshot,
  git history, or the referenced issues/PRs for the full rationale
  behind a contract the live file now states tersely.

- **`worldgen_timeline_profile_2026-07.md`** — GHC cost-centre profile of the
  worldgen setup/timeline phase (2026-07-03, issue #448). Not superseded —
  the dominant cost centre (`applyTimelineChunk`'s per-chunk replay loop)
  and the `-N1` profiling-crash workaround are still current; the
  cross-chunk border-recomputation angle it identifies is tracked as its
  own follow-up (#500).

The active river design brief (partially realised in
`src/World/River/Graph.hs`) was moved up to `docs/river_rework.md`, not
here — it is design reference, not a superseded audit.
