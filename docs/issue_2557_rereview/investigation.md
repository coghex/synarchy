# Issue 2557 rereview investigation

Status: complete. User approved the full revision, publication was exact-verified, and canonical Claude Fable 5.1 high rereview returned APPROVE. The backend applied reviewed:approve. Final spec: dcdde822a409029b92c94a7a73fbf46f6267e35841fd82c2f23ce778b007f660. Review: https://github.com/coghex/synarchy/issues/2557#issuecomment-5627103417. Published body, non-verdict labels, and canonical marker verified.

## Workflow and gate

- Skill read: `/Users/vincentcoghlan/.codex/plugins/cache/kanban/kanban/1.46.0/skills/issue-rereview/SKILL.md`.
- Only permitted timeline source: the same bundle's
  `skills/solve/scripts/trusted_issue_spec.py`.
- Canonical backend resolved using the skill's exact install-record
  precedence: `/Users/vincentcoghlan/Library/Application Support/kanban/issue-review/approve_issues.py`.
- `--check 2557 --legacy-policy dual --json`: CHANGES_REQUESTED, no pipeline
  incident. Next canonical reviewer: `claude-fable-5-1@high`. Rereview must
  not pin model, effort, or display name.
- Current spec: `63d077e1e82745b3702b3e3223a174e1290f4c18248cbdb972a928e9f059c07c`.
- Review: https://github.com/coghex/synarchy/issues/2557#issuecomment-5626895989
  (`gpt-6-astra@high`, initial, Claude origin).
- Baseline complete trusted payload: `baseline_2557.json`.
- Title: `[flora] Apply corpse retention and successor policy`.
- Preserve labels `flora`, `save-load`; canonical backend alone manages
  `reviewed:changes` / `reviewed:approve`.
- Preserve exact `<!-- issue-origin:claude -->` marker.
- Primary checkout was clean; research HEAD was
  `65ad8231916a0718877a942a63b27a7dd6316284` (review base was
  `293df10fb6e6e74b9cdc9dd6ad3786c932d91ad7`).

## Accepted product decision

Question already sent:

> For #2557, should transient corpses last the authored number of calendar
> days, regardless of plant health? I recommend calendar days. Today, a
> 60-day corpse lasts 96 calendar days at 50% health; the proposed rule
> would make it exactly 60, changing its reseeding date.

Options: `Use calendar days (recommended)` and
`Preserve legacy health-scaled timing`.

The user was told the exact skill requirement, "never silently choose
product scope or behavior," and why the compatibility change needs a
decision. The user answered "approved", accepting the recommended calendar-day rule and changed reseeding dates.

Verified from `World.Flora.Growth`: `growthRate h = 0.25 + 0.75 * clamp h`;
`floraGrowth` computes total age as baseline plus absolute day times that
rate, then wraps at lifespan + 60. For an annual with initial age 0 and
health 0.5, rate is 0.625, death starts on day 576, legacy reseed is day
672, and a 60-calendar-day policy instead reseeds on day 636. The numerical
example was checked with a short calculation, not an implementation test.

With calendar days chosen, explicitly revise the blanket promise that all
nonpersistent shipped species keep today's behavior. The design's legacy
missing-policy prose also calls its default "60-day/reseed"; spell out the
chosen duration semantics rather than hiding a second clock behind it.

## Feedback verified and required repairs

1. Separate growth age from the retention clock; add the half-health
   boundary test above according to the owner's choice. Persistent mature
   remains never wrap merely because the old 60-unit window passed.
2. Wild successors are `reseed` or `absent`. Both naturally dead cultivated
   rows and naturally dead plots must reach durable `await_replanting`
   without an external condition command. D-13/D-19 forbid automatic
   field replanting. `CropPlot.cropPlotInstance` feeds the same growth
   function with elapsed days since planting and `cpHealth`; it is not a
   separate naturally immortal crop lifecycle.
3. Expiry uses each owning page's calendar. `World.Thread.Time` advances
   only `wmVisible`; other pages must not borrow that clock. `world.setDate`
   currently canonicalizes the addressed page's date and bumps its quad
   cache, without rollover work. Add equivalent reconciliation for date
   assignments, including while paused, and multi-day jumps. Reseeding
   anchors to expiry, not observation time; later growth derives from that
   anchor. Repeated/backward assignments cannot undo durable successors.
4. Replace generation-number-only compaction with semantic equivalence,
   including age, phase, future death boundaries, and durable absence.
   Preserve markers/tombstones while their removal changes future
   behavior; require bounded current state per occurrence rather than
   eventual removal of every record.
5. Removal must be exact-identity and page-scoped, durable even while the
   row's chunk is evicted. Test place-A/remove-A/place-B/remove-A, same-tile
   co-tenants, seam aliases, allocator monotonicity, and cleanup of obsolete
   harvest/designation state. `appendEdit` preserves oldest-first replay;
   current `WePlaceFloraWithId` replays the same ID and age baseline.
   `forgetFloraInstances` removes harvest and designation entries;
   `forgetFloraDroppedSince` deliberately distinguishes eviction from
   actual removal. An old removal must never remove a replacement.
6. Natural death must snapshot selected retention/successor and frozen
   phase/stage, not reinterpret them after an authored policy edit. Cover
   save/load and regeneration, alongside explicit-condition persistence.
7. Keep migration explicit: current `world-edits` is v3, retains frozen
   v1/v2, and validates allocator cursors against planted IDs. Append the
   removal edit, freeze the outgoing complete v3 wire tree, migrate to v4
   (or the next version of the actual prerequisite result), retain all
   codecs, generate/register fixtures through the repository's tool.
   Condition vocabulary additions remain append-only under the prerequisite
   shape; verify enclosing historical records do not accidentally share a
   changed live layout. Runtime bridge changes follow Save/CLAUDE.md.
8. Extend `flora_condition_probe` with natural and explicit deaths across
   wild/row/plot forms, expiry boundaries, persistent retention, and row/
   plot replantability. Tests must distinguish real codec restoration from
   runtime save/load publication. Keep code, required docs, fixtures and
   evidence in the one implementation PR.

## Prerequisites and instructions already read

- Root instructions, `src/World/CLAUDE.md`, `src/World/Save/CLAUDE.md`.
- Persistence contract classification, content integrity, format versions;
  inventory flora/crop/edit owners; engine contracts tile seam, farming,
  and enum append-only rules. Save/load transaction contract was also read
  during the immediately preceding #2554 work.
- `docs/environmental_flora_mortality_design.md` retention section and
  D-5, D-6, D-9 through D-14, D-19. Missing
  `docs/flora_visual_state_contract.md` and its engine-contract heading are
  declared #2530 prerequisite deliverables, not current blockers.
- #2539 body and approved review: explicit/defaulted policy flag; finite
  positive `durationDays`, including valid fractional values; valid sparse
  phase/cause overrides; all 16 shipped species author a policy. Preserve
  supported fractional duration semantics when spelling out expiry checks.
- #2555 body and approved review: new commands own condition transitions,
  but Lua plot insertion/deletion and harvest mutations already happen
  directly. Expiry must not restore a replaced plot from a stale snapshot.
  Lua return true is queue admission; await action outcomes. Explicit clear
  removes only the addressed explicit condition and does not reset growth,
  generation, planting time, or harvest timers. Natural dead-window queries
  in that earlier slice create no records; EFM-10 adds natural mortality
  persistence here. Condition rendering is authoritative even over an
  existing depletion timer; preserve timer behavior and wood eligibility.
- #2549 body and approved review: at most one semantic record per occurrence;
  durable species-name and available planting/location provenance, because
  IDs encode neither species nor page/tile; missing species fails even when
  the chunk is unloaded. Equal planted counters on separate pages are valid.
  Frozen annual stage is optional; persistent has no expiry; transient
  stores exact expiry/successor; context/successor contradictions are invalid.
  Retained dangling keys are not rejected merely because nonresident.
  Capture path must be production WorldState -> WriteWorld -> PageSnapshot
  -> codecs -> adapter -> WorldPageSave -> staging.
- #2552's approved body is saved in the adjacent issue_2552_rereview folder;
  it adds shared-condition storage to plots, component migration, and
  persistence only. Its body/review explicitly leave growth, harvesting and
  expiry to #2555/#2557. Do not revive the earlier overlapping scope.

## Next actions

None for #2557 rereview. The earlier batch has no unprocessed remainder.
