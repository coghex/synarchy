# Historical audits (superseded — context only)

These are point-in-time investigation reports kept for provenance. They
predate later reworks and reference code that has since changed or been
removed. **Do not treat them as the current state of the system** — read
the live code and the agent memory notes instead.

- **`fluid_audit_2026-04.md`** — fluid generation/sim audit (2026-04-13).
  SUPERSEDED by the late-May 2026 fluid rewrite ("fluid rewrite 3").
  References `drainOceanLakes`, which no longer exists; `waterSideFaceQuads`
  is still live (`src/World/Render/SideDecoQuads.hs`) — its chunk-boundary
  complaint was fixed by cross-chunk neighbor resolution (#26), and the
  residual U-wrap seam-alias case that fix left behind was closed
  separately (#1135). Its concrete bugs are obsolete; the durable
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

- **`savedata_version_changelog.md`** — the sparse per-bump changelog (v2
  through v91, with documented gaps) archived out of `currentSaveVersion`'s
  comment in `src/World/Save/Types.hs` (2026-07-30, issue #984). SUPERSEDED
  as a
  compatibility contract: it records the whole-file save-version scheme
  that governed rejection before the persistence overhaul (#756-#768); v90
  in the file is that overhaul's own switch to per-component versioning
  (`ccVersion`/`cdVersion`), which is what replaced it and is what actually
  gates a load today.

- **`river_rework.md`** — design brief for a river runtime model owning river
  geometry in a persistent graph with the simulator owning live water
  (2026-06-25, moved down from `docs/` on 2026-08-05). **NOT ADOPTED** —
  issue #1108 recorded the decision to archive rather than build it, and
  deleted the partial realisation it referred to (`src/World/River/Graph.hs`,
  which no production module ever imported). Rivers still work the way the
  brief proposed to replace; the river work that actually shipped on that
  model is #221 and #223.

- **`player_events.md`** — original player-event and notification design
  (accepted 2026-05-18, moved down from `docs/` on 2026-08-09 by #1161).
  **IMPLEMENTED WITH DIVERGENCE** — the event store, notification registry,
  settings, event log, pause routing, and popups shipped, while #37 retired
  the proposed per-category popup buttons in favor of clickable event lines
  and a single OK button. The archived brief preserves the original decisions
  and its own divergence note; it is not a current API reference.

- **`claude_md_2026-08-18_pretrim.md`** — verbatim snapshot of CLAUDE.md
  before its 2026-08-18 trim (151k → 99k chars). The trim removed
  accumulated verbosity, corrected three stale counts (`EngineEnv` field
  count, `src/` module count, probe-script count), and extracted the
  deepest as-built mechanics into the live `docs/engine_contracts.md`.
  Every contract survives in one of those two files — consult this
  snapshot only for wording the trim rephrased.

- **`expedition_survival_calibration_2026-07.md`** — the #919 expedition
  survival calibration run record (runs 2026-07-25 against revision
  `cca19b1101`, landed by PR #937; #998's fall-trauma addendum appended
  2026-07-31, archived 2026-09 by #2185). SUPERSEDED as the actionable
  record by `docs/expedition_survival_calibration.md`, which commit
  `9eda1412b` rewrote in place as the fully dispositioned SURV-1..SURV-10
  findings ledger. This snapshot is kept because live code and docs cite
  its observations E1-E7 and its fall-calibration section (#998), none of
  which survive in the ledger; its balance numbers, tunables, and open
  follow-ups are July 2026 state, not current.
