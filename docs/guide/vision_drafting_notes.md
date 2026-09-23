# Synarchy vision drafting — research and owner questions

Started 2026-09-22 at the owner's request. This is an incomplete research
record, not the vision, accepted direction, an architectural review verdict,
or completed guide coverage. Do not use its questions or recommendations as
requirements. An incomplete `docs/vision.md` now records the owner's first
answers; the full vision has not been approved.

## Owner's drafting instruction

Read all project documentation before completing the vision. The vision will
be upstream of other project documents, so ask the owner about ambiguities,
oversights, and errors before deciding them. Stop at unresolved material
choices; do not infer acceptance from silence. Draft first; publication has
not been requested.

## Captured source context

- Primary checkout: `/Users/vincentcoghlan/work/synarchy`, local `master`
  `7adcfa31c1e007b7c5409e9e3876b6f79d32911b`. Clean at initial inventory.
- Documentation worktree resolved from branch `docs-wip`:
  `/Users/vincentcoghlan/work/synarchy-docs`, HEAD
  `720c7afe7d9f1414b408ae548488ed07d0a3dec4`.
- Initial inventory: primary `docs/` has 203 Markdown files (6,017,645 bytes);
  docs-worktree `docs/` has 220 (6,271,754 bytes). These are overlapping
  inventories, not 423 unique documents. Counts exclude this new record.
- The docs worktree has existing changes and untracked issue-review drafts.
  Preserve them. Its older unchanged files cannot replace newer contracts on
  master merely because this is the writing lane. Separately inspect local
  amendments and their recorded owner decisions.
- In particular, the unpublished structure-interaction design records
  September 16–17 multi-level-world decisions and final design readiness.
  Its expansion is materially newer than the primary copy. Design readiness
  does not constitute delivery, individual issue approval, or publication.
- Other existing local changes include chunk residency, expedition, gameplay
  timing, structure interaction, and several findings/coverage documents.
  The local gameplay-timing audit is deleted. Do not restore, disposition,
  reconcile, publish, or otherwise change these sources as part of drafting.
- No vision or prior guide reports were found in either documentation tree
  at initial inventory. No remote-head or live-tracker verification has yet
  been performed for this task.

## Reading completed so far

This is a partial list of actual reading, not a claim that the corpus has
been reviewed. Rerun the inventory and record full file hashes/versions when
resuming systematic reading; neither filenames nor headings count as reading.

- Owner-supplied repository instructions and global delivery agreements.
- Installed `guide/SKILL.md` and its `references/report-format.md` (read in
  the preceding feasibility discussion; guide is available globally).
- Root README introduction and project description, plus initial sections
  read in the feasibility discussion. Full README remains to be read.
- `docs/engine_contracts.md` introduction and contents only.
- Primary chunk-residency design opening and initial delivery map only.
- Docs-worktree expedition design lines 1–190; primary version lines 135–290.
  Its full decision history remains to be read and compared.
- Docs-worktree player manual lines 1–170 (the complete returned manual).
- Docs-worktree structure-interaction design lines 1–170 only.
- `docs/history/project_assessment_2026-07.md` lines 1–135 only.
- `docs/history/README.md` lines 1–100; its explicit supersession notes
  distinguish archival evidence from current authority.
- Project-local Claude memory entry point `MEMORY.md`, and complete memory
  notes `project_gameplay_pivot_2026_07.md`,
  `project_compute_allocation_2026_08.md`, and
  `project_focus_areas_2026_07.md`, under
  `/Users/vincentcoghlan/.claude/projects/-Users-vincentcoghlan-work-synarchy/memory/`.
  These are dated secondary summaries; stale statuses and superseded workflow
  instructions must not be promoted into the vision.

All remaining documents, unread portions, and relevant memory sources remain
pending. The owner requested all docs; a selective scan does not satisfy that.

### Second reading round, 2026-09-22

The machine-readable [reading inventory](vision_reading_inventory.json) now
records 240 unique Markdown paths and 251 selected source versions (primary
plus distinct unpublished local variants), with content hashes and a local
raw-text snapshot. Snapshot roots differ from the earlier inventory above:
the docs branch advanced to `156e874529228ee346e6bd49a64443b04708001c`
without this task changing it. Always retain source/version distinctions.

Fully read during this round:

- `docs/expedition_gameplay_loop.md` — all 1,467 lines of the unpublished
  version, plus the complete diff from primary (only processing-status and
  EXP-3 handoff amendments). Both versions are covered as document reading.
- `docs/unified_item_transfers.md` — complete primary version; a truncated
  initial read was supplemented with explicit reads of the missing decisions.
- `docs/nomad_primitive_encounter_design.md` — complete primary version.
- `docs/persistence_contract.md` — complete primary version.
- `docs/history/README.md` — complete primary version, 86 lines.
- The previously read full player manual is also recorded in the inventory.

The rest remains unread or partially read. No tests, game launches, tracker
mutations, review-coverage changes, or publication were performed.

Documentary implications to preserve, not new owner decisions:

- Expedition D-19 already explicitly names large underground dungeons as a
  long-term gameplay destination. Its first-slice exclusions are not global
  exclusions. The owner's new statement makes the intended priority clearer.
- Expedition D-7 specifies direct RTS retrieval, not the full Explore
  workflow. Its implemented-slice narrative cannot establish that the newly
  described expedition experience is complete.
- Expedition D-17 explicitly retires the radio-core map-reveal reward and
  removes the requirement that each prize unlock a new colony capability.
  Earlier example prose still mentions that reward. Preserve the explicit
  supersession; do not ask the owner to re-decide an already rejected idea.
- Expedition D-20's location-cleared flag latches when significant loot is
  first picked up by any faction. This is distinct from successful return and
  storage of expedition spoils. The vision must not silently redefine that
  runtime flag by using "expedition complete" interchangeably with it.
- The nomad design's opening explicitly makes approved #916 the implementation
  authority over its earlier alternatives. Its still-exploring body and
  provisional tracker plan are not a fresh approved backlog.
- Persistence preserves classified gameplay state at a coherent boundary;
  it expressly does not promise deterministic replay. The future-engine plan
  has not altered current save compatibility or established cross-engine
  compatibility. Do not infer either change.

## First owner questions — answered 2026-09-22

1. **Game and engine relationship.** README distinguishes Synarchy the engine
   from Ecce Homo the game, but the player manual calls the game Synarchy.
   Is the engine's purpose exclusively to serve this game, or is a reusable
   engine for other games an independently intended project outcome? This
   affects architectural tradeoffs, not merely naming.
2. **Enduring player experience.** The expedition design specifies
   discover → prepare → travel → confront risk → recover → return → improve
   the colony → reach farther. Is that the enduring organizing loop of the
   game, or the first proving slice within a broader colony simulation?
   What essential player experience or ambition is missing from those two
   descriptions? Do not derive the complete product vision from one arc's
   scope exclusions.
3. **Strategic priority.** The July memory says to finish the then-current
   infrastructure backlog and focus on the expedition game. Later docs carry
   substantial accepted simulation, physical-world, and world-scaling arcs.
   Should the vision still prioritize completing the expedition experience,
   or has the owner's priority changed? Distinguish current delivery order
   from timeless product principles; do not turn a dated queue into a ban on
   future infrastructure work.

### Owner answers (direct conversation, 2026-09-22)

These are explicit owner decisions, recorded separately from the still
incomplete documentary research. The owner has not approved a complete vision.

- **Project relationship:** Synarchy's engine serves this single game.
  The owner is developing a modular, reusable replacement engine separately
  at `~/work/hetoimasia`. Synarchy and Hetoimasia develop in parallel.
  Synarchy exists mainly to develop gameplay and assets. The later game
  integration and final product belong in a future separate Ecce Homo
  repository, which does not yet exist. Substantial work remains in both
  current repositories before that new repository can begin. Do not infer
  a migration schedule, compatibility contract, or feature freeze from this.
- **Game:** The discover/prepare/travel/risk/recovery/return/colony-improvement
  loop is the intended main loop within an open sandbox. Following it is
  not compulsory for gameplay.
- **First playable slice:** The player places the starting building,
  spawning units; prepares an expedition; selects units; right-clicks a
  marked map location; and chooses Explore. The party journeys while the
  player can work on the base. Arrival and enemy encounters produce alerts;
  clicking an alert focuses the camera on the relevant situation.
  The party clears the location, searches it for all containers before
  taking valuables, returns, stores the spoils, and resumes daily duties
  with rest before another expedition. The owner explicitly says much of
  this intended loop is absent or poorly implemented; do not describe the
  whole sequence as shipped or proven.
- **Longer horizon:** Lead with dungeoning gameplay. Later colony design
  slices add houses, workshops, and other colony systems. Eventually the
  owner wants giant procedurally generated underground dungeons that the
  player can watch units clear, with skill and equipment progression.
  Increasing difficulty with depth was expressed tentatively ("perhaps")
  and is not a settled requirement. Units should be able to retreat and
  finish more of a dungeon later with better skills and equipment; who
  decides retreat is being clarified.
- **Priority:** Expedition completion remains the main priority and a
  clear goal. Other design documents provide useful work that can proceed
  in parallel; they do not replace that priority.

### Follow-up Q4 — answered in part, 2026-09-22

After Explore, should units autonomously handle fighting, container searches,
loot selection, return and storage unless the player intervenes, or should
arrival/enemy alerts pause or hand control back to the player? For retreat,
should units decide themselves, should the player decide, or should both be
possible? Asked through the conversation's asynchronous question tool.
The owner clarified:

- Explore must become a goal in the AI script. Goal importance influences
  action utility so units pause routine duties and begin their expedition.
- Sending units back cancels that goal and follows the new player order.
- Local repositioning, such as moving between dungeon floors, must preserve
  the expedition goal.
- The AI assesses combat results and should end the expedition early after
  a disaster, such as severe injuries or deaths. Both player withdrawal and
  AI abandonment are intended.
- The owner is uncertain whether the existing implementation does this
  correctly and anticipates difficult behavioral testing. No implementation
  correctness or test success has been established by this discussion.

The answer did not settle pausing on alerts, party-versus-individual
withdrawal, or how to distinguish a replacement command from local direction.

### Follow-up questions Q5–Q6 — owner clarification received

- **Q5: command meaning and resumption.** Should an explicit Return/Cancel
  action end Explore while ordinary Move preserves it and resumes expedition
  activity on arrival, or should cancellation be inferred from where the
  player moves a unit? Is automatic resumption after local movement intended?
- **Q6: party coherence.** When one member judges an expedition too dangerous,
  does the assigned party withdraw together, or may members return
  independently while others continue?

**Owner clarification (next conversation reply):**

- An explicit player cancellation option is accepted, alongside each unit's
  ability to cancel its own personal mission.
- A goal is not necessarily the highest-priority action. Needs come first:
  a starving unit searches for food rather than completing its goal. Once
  those needs are met, the goal resumes.
- Feasibility and distance must influence utility. An expedition across the
  map may be assigned low enough utility that the unit keeps doing colony
  chores. Units attempt goals only when they judge them feasible.
- Retreat is per unit. Fear alone can make a unit run for its life; the
  rule is not limited to observed severe injury or death.
- Ideally units communicate their decision. Remaining units account for
  the loss of party strength and reassess their own chances. If insufficient,
  they cancel their own expedition; one retreat does not automatically cancel
  every member's mission.

This supersedes any unconditional wording earlier in these notes or the
draft that Explore makes units immediately abandon chores. It also settles
Q6 as individual decisions with reassessment, rather than group-wide control.
Q5's explicit cancellation is settled, but exact resumption after local Move
and its interaction with position hold remain a design question. No UI label,
distance cutoff, fear threshold, or communication channel has been selected.

**Q8 — answered:** does an initially infeasible Explore
goal stay pending and get reconsidered as circumstances change, or is it
rejected/cancelled and require a new order? Resumption after satisfying needs
is explicitly settled; do not assume it answers initial infeasibility too.

The owner chose immediate decline. Report a red player-log message identifying
the unit and cancellation reason, using the requested form:
`expidition canceled for unit <unit name>: <cancellation_reason>`.
The draft corrects the spelling to `Expedition` without changing its meaning.
This lets the player decide what to do next. An infeasible goal must not sit
pending and activate later. The owner wants goals to come and go quickly,
and rejected the assistant's proposed pending-goal behavior.

The earlier accepted-goal rule still stands: needs can temporarily interrupt
an accepted expedition and it resumes once those needs are met. This is
different from keeping a declined or canceled mission pending.

**Q9 — open, no decision:** the owner is uncertain whether multiple
simultaneous goals are desirable and is willing to consider arguments.
Do not promote the current single `activeGoal` implementation into product
intent, add a queue, or infer a permanent single-goal restriction. A bounded
initial design with one active mission and temporary need/action interrupts
may be proposed, but has not been approved.

**Q7 resolved by existing documentary authority, not a new owner choice:**
the initial pause question was asked before reading the notification policy.
Primary `docs/history/player_events.md` decisions 5–6 explicitly give players
independent per-category log/popup/pause controls. The current
`data/notification_categories.yaml` documents those same controls and their
defaults. There is no conflicting expedition-specific instruction requiring
a new pause policy, so preserve this already established control rather than
ask the owner to re-decide it. Do not invent defaults for new categories.
The history document's explicitly retired popup-button design remains retired.

### Third reading round and focused source check

- Fully read primary `scripts/CLAUDE.md`.
- Read primary `docs/history/player_events.md` lines 1–130 and the complete
  `data/notification_categories.yaml` for notification policy. The remainder
  of the historical document remains unread.
- Read primary `docs/engine_contracts.md` lines 2952–3020 and 3533–3630:
  position hold, beginning of player transfers, commanded-order stall budget,
  and the expedition control's evidence limits. Earlier docs-worktree search
  results had different line numbers and are not current contract evidence.
- Read `scripts/unit_ai_core.lua` lines 170–365 at primary
  `7adcfa31c1e007b7c5409e9e3876b6f79d32911b`. It has an `activeGoal` layer
  above action utility and a separate `commandedTask` for player moves.
  This is a focused context check, not a whole-AI audit or implementation.
- The current position-hold contract says player-move arrival creates a
  persistent hold, outscoring situational goals until explicitly cleared.
  That makes goal preservation and resumption separate choices for Q5.
  Do not silently change the existing hold contract through this draft.
- Suggested verification direction, not an owner-selected gate: exercise
  goal preservation/replacement and casualty-driven decisions with controlled
  inputs, then add integrated expedition evidence. Do not require one giant
  stochastic playthrough to prove every transition, and do not freeze numeric
  utility or casualty thresholds in the vision.

## Resume

### Sixth owner clarification — goals and automatic preparation

The owner accepted one active **goal** per unit initially, with needs and
immediate actions as interruptions. **Mission must not be used as a synonym**:
it will mean something else in future. Q9 is now resolved; multiple
simultaneous goals are deferred, not prohibited forever. Earlier uses of
"mission" in these research notes are historical assistant wording, not
product terminology.

The owner describes Find Water as the other existing goal: starting units
seek water until completing it, unless given a replacement goal. Explore
cancels Find Water and becomes the new goal; Find Water does not remain in
a queue. The general needs-first rule remains in force.

The owner identifies preparation as the main design gap from actual play.
Units should requisition goods automatically as part of Explore, based on
journey distance and their own perception of difficulty. An unknown location
calls for round-trip food and water plus a few minutes of exploration and a
buffer. A known dungeon implies weapons and a longer stay; a single building
requires less. Future location types can alter requirements. First-aid kits,
canteens, rations and weapons are relevant. Requisitioning must not
overencumber units. No exact buffer, timing unit, ratio, inventory policy,
stock-allocation rule, or group provisioning policy was chosen.

Preparation questions to resolve before turning proposals into requirements:

- **Q10: insufficient kit/capacity.** Decline when a minimum safe round-trip
  load cannot be supplied/carried, shorten the attempted visit, or permit a
  knowingly underprepared departure? Proposed recommendation: define a
  minimum safe load, declining with the accepted red reason if it cannot fit
  or be obtained. Allow adjustment of optional supplies only above that floor.
  Minimum essentials versus desirable equipment, and individual versus shared
  first aid, still require owner decisions.
- **Q11: loot headroom.** Reserve some outbound capacity for loot, or let
  supply consumption create capacity during travel? Full packs cannot also
  guarantee extraction; neither a reserve fraction nor an equipment-shedding
  policy is selected.
- **Q12: departure coordination.** Does the selected party wait for members
  to finish preparation and reassess after refusals, or may ready members
  leave independently? Individual retreat does not answer departure policy.
- **Q13: infeasible replacement goal.** If Explore is declined immediately,
  does it still cancel an existing Find Water goal? The owner specified
  replacement for assigning Explore and immediate refusal for infeasible
  expeditions, but has not explicitly settled their interaction.

Other preparation design work, not new owner-selected mechanisms: estimate
travel duration using routes, unit speed, carried load and needs rather than
only straight-line distance; prevent concurrent units counting the same last
supplies; use observed location/stock knowledge honestly; revalidate actual
loads and plan the return reserve; bound preparation failure so units cannot
wait indefinitely; keep supplies gathered before cancellation accounted for.
These concerns do not authorize a separate design document, issues, or code.

Focused source evidence:

- Read primary `docs/portable_loot_containers.md` lines 179–442: physical
  contents differ from remembered knowledge; exact recursive weight and
  ownership matter; random salvage does not guarantee repayment of supplies.
  The remainder of that large document is still pending.
- Rechecked `scripts/unit_ai_core.lua` lines 195–254. Current GOALS includes
  `find_water`, `notify_allies`, and `attack`; INITIAL_GOAL assigns Find Water
  to acolytes only. The code's vocabulary needs reconciliation with the
  owner's clarified goal concept. This is not approval to reclassify or
  remove those entries. Do not extend the starting-water goal to robots or
  every species merely by generalizing the owner's example.
- The previously read survival findings record weak acolytes with little
  supply headroom and deliberate body diversity. They support a real capacity
  decision, not an assumed minimum carrying-capacity increase.

The initial vision now includes automatic preparation and uses goal
consistently. The full corpus read remains incomplete.

### Fifth reading round

Fully read primary `docs/expedition_survival_calibration.md`,
`docs/architecture_conversation_audit_2026-09-05.md`, and
`docs/stance_recovery_design.md`. A truncated combined tool output was
supplemented by explicit reads of the architecture ending and stance opening.
Their full-read status is recorded against the captured source hashes.
The distinct unpublished architecture variant remains a separate pending
reading target; no tracker status or implementation claim was refreshed.

Carry forward these source distinctions:

- The survival findings have terminal dispositions. Their old observations
  are not fresh bugs or new balance decisions. SURV-4 records the owner's
  persistent-hold decision; survival interruptions remain allowed.
- The architecture conversation's follow-up records owner decisions that
  the home colony continues simulating during expeditions, gameplay-critical
  chunks should remain resident, and exact deterministic replay is unnecessary.
  Earlier recommendations in that report are explicitly superseded where
  the follow-up says so. Reconcile the later gameplay-timing and residency
  designs before completing the vision's corresponding principles.
- Stance recovery exemplifies an accepted bounded operation that preserves
  concurrent gameplay effects without a general engine rewrite. Its
  single-issue scope does not establish a universal implementation strategy.

Q8's immediate refusal is now in the vision draft, with the red unit-and-reason
message. Q9 remains open. Proposed recommendation for owner consideration:
one active mission per unit initially, with needs and local actions able to
interrupt it, and no queue of refused/canceled missions. Multiple simultaneous
missions could be reconsidered for a concrete later gameplay need. This is
not yet an accepted rule.

The fourth reading round completed `docs/history/player_events.md` through
line 787. The inventory now records the full historical document as read.
Its phase-specific non-goals and retired UI sketches are not global product
exclusions. This round also incorporated the owner's needs/feasibility and
individual-retreat clarification above. Q8 was subsequently answered with
immediate decline and a red reason-bearing log message; Q9 remains open.
Exact local-Move
resumption remains a later design question, without a selected hold exception.

The first three owner answers are recorded above. Obtain remaining answers
before settling dependent principles. Continue the complete corpus read,
keeping current contracts, accepted plans,
unresolved proposals, implementation evidence, and historical material
distinct. Record additional ambiguities with exact source passages and ask
before deciding them. Explicitly superseded passages can be identified as
such without asking the owner to re-decide their already recorded resolution.

The initial vision draft contains only the owner's newly clarified product
direction. Continue it after further source reading and owner clarification.
Preserve unresolved sections visibly if any remain;
do not make the draft authoritative or redirect downstream instructions to
it before the owner accepts it. Do not create tracker work, advance any
review ledger, or claim that documentation reading establishes code-review
coverage.

### Owner clarification: requisition ranges, loot valuation, and group missions

Recorded 2026-09-22. These explicit answers supersede earlier open Q10–Q12
where answered, and earlier wording reserving mission as an undefined future
concept. Earlier Q9-open notes are historical: the owner has since selected
one active goal per unit initially. No source-reading progress is implied by
this conversation update.

- Required requisitions that cannot fit cause individual expedition
  cancellation with the agreed red reason-bearing log warning. Requirements
  can be ranges: 1–2 rations means a minimum of one and a desired second if
  spare capacity allows.
- Preparation reserves 10–15% of inventory capacity for loot. Failure to
  preserve this reserve also cancels the unit's expedition. Optional extras
  cannot consume the required reserve. No exact percentage or capacity
  metric has been chosen.
- With a full inventory during an expedition, units weigh new loot against
  carried items and may drop the least valuable carried items. Value is
  personal and contextual, distinct from sale price, and sensitive to weight
  and usefulness. Hardware can outrank steel plate because it is compact and
  light; valuable treasure can outrank both, while a survival canteen normally
  outranks the treasure. The owner explicitly wants exceptional valuation
  strong enough for a unit to give up its canteen even at the cost of its
  life. Do not introduce an absolute protected-essentials rule or silently
  rewrite the previously accepted needs-first action policy.
- A goal belongs to an individual unit. A mission belongs to a whole
  player-controlled group. Selecting multiple units and choosing Explore
  creates the mission and assigns each unit the Explore goal.
- Units prepare but wait for mission readiness and explicit player dispatch.
  A ready mission starts when the player clicks its green check. The check
  becomes a yellow arrow indicating in progress. Green check/red X indicate
  readiness; the owner has not defined the red X as a cancel button.
- Mission boxes occupy the right side, beginning bottom-right; additional
  concurrent missions appear above. Clicking a box opens the expected roster
  and individual requisition requirements, with sensible automatic estimates
  and player-editable requirements for each inventory.
- This supersedes any assumption of automatic departure after assigning
  Explore. It does not revive declined goals or establish multiple concurrent
  goals per unit. Preparation and waiting for dispatch are accepted-goal
  stages. The owner acknowledges substantial implementation and new art;
  neither implementation nor asset generation is requested in this vision
  drafting task.

Material questions still requiring owner decisions:

1. Do freely editable requisitions allow an intentionally underprepared
   departure, or does a unit retain its own minimum feasibility veto? Capacity
   and the reserve have been explicitly required; no override of them has
   been granted. This also affects how broadly to interpret the exceptional
   willingness to sacrifice survival supplies when looting.
2. If a selected member refuses or later cancels during preparation, does the
   mission remove that member and reassess the remaining party, or stay
   blocked until the player resolves the roster? Individual withdrawal is
   settled; group membership/readiness after withdrawal is not.
3. Is readiness live while awaiting dispatch, including changes to supplies,
   needs, party strength, or edited requirements? Proposed recommendation:
   reevaluate and withdraw readiness when conditions no longer satisfy the
   agreed departure rules; revalidate at dispatch. This is not yet a settled
   mechanism.

Other later design questions: single-unit mission presentation; reassignment
from one mission to another with only one active goal; unavailable stock and
contention between missions; return of supplies after edits or cancellation;
shared supplies; eventual mission completion/empty-roster handling. Avoid
inventing answers through UI sketches or state names.

The vision draft captures these decisions provisionally. Full corpus reading
and owner approval remain pending; no downstream authority has been changed.

### Owner correction: configured loadouts govern preparation readiness

Recorded 2026-09-22 after the group-mission clarification. This supersedes
the preceding proposed feasibility veto, automatic preparation reassessment,
and any suggestion that falling below recommended provisioning blocks Start.

- Mission creation supplies suggested per-unit requisitions. The player can
  tab through units, see each actual inventory, and see red boxes for absent
  or insufficient required items, showing quantities held and needed.
  Clicking an unmet requirement allows editing. Other carried items can also
  be limited, including items acquired before mission creation.
- Player minimum/maximum settings control preparation. If two rations were
  suggested, setting minimum zero and maximum zero causes the unit to store
  its rations nearby at base. Once the configured loadout is satisfied and
  the mission is ready, the player can dispatch it with no rations. A unit's
  provisioning estimate cannot veto that choice.
- A unit dying or personally canceling during preparation is automatically
  removed with a player warning. Remaining units do not reassess the mission
  or change requisitions in response. Reassessment of the expedition belongs
  during the expedition. Personal goal cancellation remains possible during
  preparation; the owner explicitly preserves it.
- A mission warning icon with a tooltip explains unmet recommendations,
  such as a distance-dependent recommended ration minimum per unit. These
  warnings are advisory and do not prevent dispatch.
- Readiness must change immediately when configured requirements cease to
  be met. A preparing unit eating a ration below its minimum turns the
  mission red and works to replenish it. Readiness updates check compliance,
  not expedition safety or revised suggested quantities.
- The owner proposes callbacks rather than periodic polling to achieve
  responsiveness and asks for technical judgment. Proposed mechanism:
  notification after committed inventory/content changes invalidates the
  affected unit's readiness and schedules preparation work, also covering
  edits and membership changes. A single current-state validation at dispatch
  prevents a stale readiness indicator authorizing a noncompliant departure.
  Neither callback plumbing nor dispatch validation is implemented or newly
  established as an engine-wide contract by this recommendation.

Newly exposed questions, not settled through assumptions:

- Earlier impossible-loadout cancellation may occur before the player can
  correct a suggested loadout. Should impossible suggestions/settings remain
  red and editable instead, or when should individual cancellation occur?
  Retain the earlier capacity/reserve decision as historical explicit intent
  while flagging its interaction with the new flow in the vision.
- Earlier distance-based immediate refusal needs reconciliation with the
  new advisory assessment and no reassessment during preparation. Do not
  silently retain the old general feasibility veto.
- Are loadout quantity limits only preparation constraints? Carrying them
  into active expeditions could conflict with opportunistic looting. The
  current answer explicitly describes storing excess at base before departure.
- Missing required stock, no usable nearby storage for excess goods, and
  empty missions remain unspecified. They must not result in invented item
  deletion, forced ground dumping, or automatic lifecycle choices.

Focused local searches found engine-to-Lua callback dispatch infrastructure
in `src/Engine/Scripting/Lua/Thread/Dispatch.hs` and inventory mutation APIs
under `src/Engine/Scripting/Lua/API/Units/`. This is discovery only, not proof
that a complete inventory-change notification exists. No new source is
marked fully read, and no behavioral validation or implementation is claimed.

The vision was revised in place to remove superseded preparation claims;
the complete documentation review and owner approval are still pending.

### Owner clarification: persistent blocked missions and multiple goals

Recorded 2026-09-22. This supersedes immediate feasibility refusal,
capacity-based goal cancellation, and the earlier single-goal-only direction.

- Mission creation always creates the mission and assigns the participating
  units their goals, regardless of feasibility. Preparation blockers remain
  visible in red for the player to resolve; inability to meet requisitions
  does not cancel the goal. Distance affects recommendations, not acceptance.
- Before collecting anything for a requisition, a unit must know it has
  capacity for the required load. Units that cannot fit their requirements
  get an error exclamation icon in the mission screen. The mission remains
  red and editable. No partial collecting toward a known impossible loadout
  is authorized.
- The screen has a tab for each unit to edit individual requirements and a
  right-hand column for setting common requirements across all units. Applying
  those common requirements overrides existing per-unit custom requirements.
  This explicitly authorizes bulk replacement in the design; do not invent
  a confirmation requirement. Behavior for later-added units is not settled.
- The owner proposes multiple remembered goals per unit from the beginning
  so units can belong to queued missions, choosing the oldest-created one
  to requisition for. Missions may wait a long time while units handle other
  tasks or goals. Record this direction and distinguish it from unspecified
  queue scheduling, simultaneous preparation, or automatic dispatch.
- Actual voluntary/player cancellation and death remain distinct from a
  preparation blocker. Their previously agreed removal and warning behavior
  is not revoked. Explicit player dispatch of a ready mission is unchanged.

Questions to resolve before making scheduling authoritative:

1. If the oldest mission is blocked, does it retain the unit's sole mission
   preparation slot while ordinary work/other goals continue, or may a later
   mission prepare and depart first? Proposed recommendation: one mission
   loadout at a time, with clear waiting reasons. Strict oldest-first versus
   skipping is an owner choice, not an implementation detail.
2. Does Find Water now coexist with an Explore goal, or does Explore still
   explicitly replace it? More generally, multiple remembered goals need
   rules for retention and selection of non-mission goals; the previously
   settled Find Water replacement example cannot answer the changed model.
3. Does capacity validation consider the intended load after storing excess
   existing items, then store those items before collecting new ones? This
   would avoid blocking a feasible loadout because the unit currently carries
   items that the player has already instructed it to store. Exact sequencing
   remains unselected.

Later design must also specify when a unit is available for its next mission
(including return, unloading, recovery), shared-member readiness/dispatch,
and edits or cancellation of queued missions. No automatic transition or
reordering is inferred. The vision retains these as open choices, not a
completed mission-system specification. Full corpus reading remains pending.

### Owner clarification: mission menu, goal utility, and free-space thresholds

Recorded 2026-09-22. This entry supersedes the earlier Find Water replacement
example, the 10–15% reserve range, and the proposed stack of mission boxes.

- The owner proposes a dedicated compact mission menu: a bottom-right
  button showing the selected mission, initially the oldest, with a panel
  listing missions and their status. The player can select and reorder them,
  promoting an achievable mission over a blocked one. No automatic
  cancellation accompanies this change. Both blockers and achievable
  alternatives should be clear.
- The owner explicitly leaves concurrency open and requests feedback:
  choosing one current mission might unnecessarily restrict play to one
  mission at a time. Do not record that restriction as a settled decision.
  Proposed recommendation: distinguish the viewed mission, the player's
  priority order, and missions actually underway; independent groups can
  proceed concurrently. Shared units need an explicit commitment rule.
- Goal memory includes both mission-associated and independent goals. Each
  goal has a utility function expressing its importance. Find Water is not
  a mission and remains alongside mission goals. It should be valued highly
  enough to finish before the unit engages in a mission goal. This resolves
  coexistence and overrides earlier replacement semantics. Do not turn all
  goals into missions or silently apply oldest-first across all goal types.
- Free space of at least 10% is required for readiness. A unit below 25%
  free should try to store unneeded items until it reaches that target,
  retaining what it wants subject to configured quantity limits. The 25%
  target is not a second readiness gate. No compulsory full inventory purge
  or stripping of wanted supplies is authorized. The earlier 10–15% range
  is superseded.
- The answer does not define the exact sequence of checking intended versus
  actual capacity and storing/collecting items. It does establish that
  discretionary storing serves capacity needs and the desired reserve.

Recommendations awaiting owner acceptance:

1. Allow distinct parties to prepare and undertake missions concurrently.
   Apply mission priority to a unit's assigned missions, not as a global
   barrier to all later missions. That mission goal still competes with
   independent goals through utility. Clarify whether player ranking selects
   the eligible mission goal or merely weights several competing ones.
2. Let a shared unit prepare for one mission at a time. Reordering changes
   queued/preparing commitments; recommend that it not automatically recall
   a unit from an expedition already underway. This requires an owner choice,
   not an inferred change to return/cancel behavior.

The vision records confirmed answers and preserves scheduling as unresolved.
No full-corpus reading progress, implementation, publication, or validation
of the future mission system is claimed by this conversation update.

### Owner decision: master queue and unit-exclusive active missions

Recorded 2026-09-22. This supersedes per-unit mission ranking, the proposed
single-selected-mission HUD, and unresolved concurrent mission ownership.

- One unit can be assigned to several missions, but cannot belong to two
  active missions simultaneously. Concurrent active missions must have
  mutually exclusive unit sets. Independent goals remain governed by utility;
  the exclusivity rule concerns mission-associated goals, not all goals.
- The player freely sorts one master mission queue. Units do not sort their
  own mission goals; the active mission determines the eligible mission goal.
  When a mission clears, evaluate the next queued mission for overlap with
  all remaining active missions. Promote it if conflict-free. Otherwise leave
  the queue in place until another mission clears and reevaluate. Do not
  automatically skip a conflicting head. The player may reorder the queue.
- Multiple teams can be queued together, with different combinations of
  members in later missions. No fixed-team abstraction has been selected.
- Reordering active missions is cosmetic because they have disjoint units.
  It does not change their work, interrupt expeditions, or cancel goals.
  Activation is distinct from dispatch: preparation and ready-but-undispatched
  missions can be active, and the player still clicks the green check to
  begin travel.
- The bottom-right gameplay box shows the active mission list. Selecting it
  opens the full list in a tall central window, with active missions at the
  bottom and the separated queue extending upward. Drag/reorder is desired;
  move-up/move-down arrows are a proposed first-slice implementation. This
  replaces the previous selected-mission button proposal.
- The existing gameplay checklist must move to the top right. The owner
  proposes connecting it to actual mission milestones: creating Explore
  reveals "Prepare an exploration mission"; readiness/requisition completion
  checks it and reveals "Dispatch an expedition"; clicking the green check
  completes dispatch and reveals "Discover the location"; subsequent steps
  include "Explore the <location_name>" or location-type-specific objectives
  such as exploring ruins or clearing a dungeon. Location types have different
  completion requirements. Do not invent a universal clear predicate.
- Missions may eventually represent activities other than exploration.
  No additional mission types or detailed mechanics were selected.

Material lifecycle and guidance questions still open:

1. What does a mission clearing mean for releasing its units: location
   objective completion, return and storage, or also recovery/rest? Proposed
   recommendation for expeditions: retain the mission through return and
   storage, with personal recovery needs continuing to influence subsequent
   goal work. This is not yet owner-approved.
2. Is the checklist introductory guidance or per-mission tracking? If several
   missions are active, which progress is displayed? No automatic camera
   switching, tutorial gating, or repetition policy has been selected.
3. Does a successful queue promotion immediately evaluate the next entry
   too, stopping at the first conflict? Queue reevaluation on creation,
   reordering, cancellation, or individual membership changes also needs
   specification. Completion-triggered evaluation alone does not define all
   these cases. The no-automatic-skipping rule is already explicit and should
   not be repeatedly reopened as though unanswered.
4. Active mission membership edits must preserve exclusivity; behavior for
   attempts to add an already-committed unit remains unspecified. Likewise,
   moving a still-active blocked mission back into the queue has not been
   designed merely by allowing queue reordering.
5. Does a checked preparation milestone remain checked if readiness is later
   lost? Mission readiness itself must still update immediately as agreed.

The updated vision captures the owner decisions and marks lifecycle/UI
ambiguities. The broader source review is still incomplete; no implementation
or source-level verification of the existing checklist is claimed.

### Owner decision: loot return, full queue promotion, and one-time guidance

Recorded 2026-09-22. This explicitly supersedes the prior no-skipping rule
and resolves the successful expedition endpoint and checklist purpose.

- Returning and storing important loot remain part of the expedition mission.
  Only mission loot belongs to this deposit requirement: base materials and
  equipment are outside it. Do not require clearing all carried goods before
  completing an expedition, or equate every acquired item with mission loot.
- Location-specific completion need not require loot. For such missions,
  still confirm return home, using a reasonable home-base distance or,
  preferably in the owner's proposal, a callback when homeward pathing
  completes. Exact mechanism is not final; successful arrival must be
  distinguished from cancellation or failure. Party-level completion and
  partial-party loss/withdrawal need a later explicit contract.
- Completed missions move to an initially hidden completed-missions list,
  which the player can open to inspect past missions, encounters, and loot
  rewards. Retention limits and failed/aborted history are not specified.
- Queue promotion continues through the queue, including beyond conflicting
  entries. Every mission with an available unit set becomes active, with
  newly activated missions included in subsequent conflict checks. Player
  order determines precedence when queued missions share participants. Only
  missions containing a unit assigned to an active mission remain queued.
  This explicitly replaces the previously recorded stop-at-first-conflict
  behavior. A preparation blocker does not make an active mission queued.
- The checklist is tutorial/linear guidance, run once. The first completed
  mission finishes the "Complete a mission" sequence and it disappears.
  It is also a development device: the complete checklist should be
  achievable during each gameplay run. This is an intended validation
  scenario, not evidence of a passing current test, and does not make the
  sandbox tutorial mandatory.

Remaining important outcome question: if an expedition returns without
required loot after retreat, should it end as aborted/failed, free its units,
and retain its outcome in history? Do not infer success, permanent active
status, or automatic retry from the return rule. Earlier unit cancellation
rules do not by themselves define the aggregate mission outcome.

Other downstream details remain open: exact mission-loot classification,
party aggregation, unavailable storage, and tutorial step attribution if
different missions satisfy different milestones. These are recorded as
unresolved, not silently chosen. The full source review remains incomplete.

### Owner decision: Failed history and player cancellation

Recorded 2026-09-22 in response to the proposed unsuccessful-return outcome.

- Failed missions have a separate Failed tab next to Completed and Available.
  Active missions always remain at the bottom of the full mission window,
  regardless of the tab being viewed.
- The owner accepts recording genuinely unsuccessful missions as failed,
  including the discussed retreat/return without required loot, rather than
  successful completion. Ending the mission frees its units; failure does
  not complete the tutorial's successful-mission milestone.
- Player-canceled missions are excluded from Failed unless the player
  cancels after clearing the location and before the mission completes its
  normal return/home-storage lifecycle.
- That post-clear cancellation is a failed mission with loot and encounters
  recorded and the reason "player cancelled". Do not count location clearing
  alone as mission success or omit this failure because it was player-initiated.
- This answers history categorization; it does not define every possible
  failure trigger or equate one unit canceling its goal with the whole
  mission failing. Partial-party outcomes and location-specific clearing
  conditions still need their own design. Recorded loot must not silently be
  represented as delivered rewards if it was never brought home; exact
  recovered/carried/deposited presentation remains unspecified.

The vision was updated with these distinctions. It remains a provisional
draft pending the broader document review and resolution of remaining
design questions; no gameplay or UI implementation has been changed.

### Vision gap review requested by owner — 2026-09-22

Read the entire current vision draft and compared it with focused sections
of the player manual, expedition arc, timing, residency, faction, map, asset,
and persistence documentation. This is a review of the current draft with
targeted source checks, not completion of the requested full-corpus read.
New partial reading ranges were recorded only where current primary hashes
matched the captured inventory. No vision policy was added through inference.

Assessment: the expedition direction is substantial, but V-3 has grown into
a detailed mission-system specification while the overall product identity
and design tradeoffs remain thin. Eventually extract detailed mission UI,
state transitions, requisition quantities, callbacks, and test cases into a
linked design, preserving all owner decisions. Keep the vision authoritative
about intent, autonomy, planning, consequence, continuity, and priorities.
This review does not perform that extraction or create a new design workflow.

Missing or insufficiently expressed vision-level material:

- **World and player identity:** what Ecce Homo's setting is, who the player
  represents, who the acolytes are, and the intended tone. Present content
  includes portals, acolytes, robots, and nomads, but that is insufficient to
  invent lore, religion, genre boundaries, or thematic commitments.
- **Experience and design tradeoffs:** what makes this particular sandbox
  compelling; which decisions should occupy the player; when simulation
  detail earns its complexity; how agency, readability, and emergent behavior
  should be balanced. The existing expedition design's Desired experience
  names settlement simulation, RTS-style direction, procedural destinations,
  survival preparation, and settlement progress, but does not resolve all
  tradeoffs or the complete product vision.
- **World/colony/progression beyond expeditions:** how survival, production,
  settlement growth, technology, trade, factions, recruitment, and discovery
  are intended to contribute. Record durable purposes, not an exhaustive
  feature wishlist. The faction design separates control from culture and
  accommodates future player-controlled nomads; this is existing direction
  to reconcile, not evidence that diplomacy or multiplayer is a near-term
  product promise.
- **Consequences and campaign continuity:** mission failure is defined, but
  long-term loss/recovery and what happens when the colony loses everyone
  are not. The manual says there is no scripted victory; distinguish that
  current description from an owner-approved permanent endgame exclusion.
- **Art and presentation identity:** gameplay/assets are Synarchy's central
  output, but the vision has no short artistic intention. Asset generation
  documentation is production guidance, not a substitute for desired tone,
  readability, or how environment/characters should feel.
- **Scope and success boundaries:** distinguish the minimum end-to-end
  expedition milestone from the complete queued-mission ambition; identify
  deliberate current non-goals only where actually agreed. Network-ready
  identity shapes do not establish multiplayer scope. Platform facts and
  future product targets must not be conflated.

Existing accepted direction missing from the vision, to reconcile and
summarize rather than ask the owner to design from scratch:

- `docs/gameplay_timing_design.md` D-3, D-7–D-9: the home colony continues
  simulating while attention is elsewhere; fast-forward advances coupled
  gameplay together; 5–50 controlled units is the intended typical range;
  four CPU cores/8 GB total machine RAM is the minimum laptop class; under
  overload preserve responsive controls and readable presentation while
  gameplay slows coherently. The 10-by-10 chunk base is explicitly tentative.
- `docs/chunk_residency_streaming_design.md` opening and Arc A invariant:
  rendering/residency decisions must preserve durable gameplay consequences.
  The implementation arc is exploring; do not label the whole design approved.
- `docs/persistence_contract.md` section 1: save/load preserves a coherent
  session, not deterministic replay. Future mission goals, queues, loadouts,
  tutorial progress, and histories require explicit state classification;
  the vision should express player continuity without inventing a codec or
  silently demanding every UI selection persist.

Concrete unresolved mission behavior identified in the draft:

1. **Reprioritizing a blocked active mission:** only conflicting missions
   remain queued, and active-list reordering is cosmetic. If active A holds
   a unit while preparation is impossible, promoting queued B cannot free
   that unit. An explicit suspend/requeue/reassign operation or some other
   owner-selected policy is needed if preserving A while running B is desired.
   Do not reintroduce automatic refusal or cancellation to solve it.
2. **Withdrawal and release:** a unit that cancels its goal far from home
   must not silently be considered physically ready for another expedition.
   Goal membership, mission outcome, availability, and return behavior need
   separate definitions. Existing return/cancel intent alone does not settle
   partial parties or the empty-mission outcome.
3. **Find Water versus overrides:** high-utility Find Water is explicitly
   intended to finish before mission work. Consequently a fully provisioned
   loadout may still wait for that independent goal. Define how the UI exposes
   this and whether any player override exists; do not silently weaken it
   because player requisition edits override supply recommendations.
4. **Scope of mission loot:** distinguish carried/recovered/deposited loot
   and location-specific significant-item rules. The older expedition arc
   includes unique equipment among significant location items, while the
   new mission deposit rule excludes equipment; those can coexist because
   location clearing and home deposit are separate predicates. Do not claim
   a contradiction or unify the categories without owner direction.
5. **First-slice boundary:** V-3 bundles the playable expedition with multiple
   remembered goals, mission queue, concurrent parties, per-unit editing,
   history, and tutorial. The owner has not yet explicitly separated minimal
   acceptance from staged expansion; do not defer those features on their
   behalf merely because the scope is large.

Suggested next owner discussion: setting/player identity, desired experience
and simulation tradeoffs, then colony/campaign progression. Mission edge
cases should stay recorded for a dedicated design pass rather than consuming
the whole upstream vision conversation. The full source review remains a
requirement before final vision approval; this gap review does not satisfy it.

### Owner direction: story generation, setting, progression, and art

Recorded 2026-09-22 in response to the vision gap review. These are owner
decisions unless explicitly identified below as proposals or open questions.

- The setting is science fiction/fantasy, millions of years in the future,
  on a distant terraformed planet. The player controls a colony of acolytes,
  comparable in role to guiding a Dwarf Fortress expedition. Their purpose is
  exploration and recovery of religious artifacts; other factions have their
  own motives. Extensive lore exists in `~/work/synarchy-lore`; the vision
  should acknowledge that depth without reproducing all its details.
- Story generation is the central experience. Generate a setting and
  characters, give them goals and trials, and let interacting circumstances
  create unforeseen outcomes in a sandbox. The player enjoys watching and
  reading those stories, including combat logs, unit thoughts, and generated
  descriptions that make units feel like people. This does not specify LLMs,
  a director system, scripted disasters, or a particular prose generator.
- Realism serves storytelling. Detailed simulated combat and health are
  critical to causal chains such as brutal injury, survival, infection,
  delirium, and a fatal fall. This example expresses desired emergent behavior,
  not a verified current mechanic or a guaranteed authored event. Temperature
  and other less important simulations may be simplified. Enjoyable,
  understandable play takes priority over realism, with tuning based on the
  owner's experience rather than a universal numerical realism requirement.
- Hunger should drain slowly; missing meals for a few days should not itself
  kill a unit. Hydration is needed through the day, but a day without water
  should impair rather than kill by dehydration alone. Do not invent exact
  rates, convert this into a medical model, or silently retune production code.
- Dungeon combat and skill checks become harder with depth. When a floor
  exceeds a party's capabilities, better skills/equipment or more units allow
  progress later. This resolves the earlier tentative depth-difficulty idea.
- The owner proposes exploring only the next unexplored dungeon floor per
  Explore mission, with progress in the zoom-map location tooltip/popup.
  Preserve this for later design. Partial floor exploration, retries, floor
  completion versus whole-dungeon clearing, and the cancellation/failure
  boundary for floor missions still require precise contracts.
- The colony supports expeditions and other activities, including future
  mining/farming. Routine colony operation should mostly take care of itself.
  Manufacturing and research equip more units; loot supplies the best gear
  but not at the volume needed for many units. Beds and food production
  support population. No research tree, recruitment mechanism, or production
  balance is selected by these purposes.
- The only campaign failure is no living player-controlled units remaining.
  A failed mission or lost base alone is not campaign defeat while a unit
  survives. No separate ironman, resurrection, or campaign restart policy is
  inferred.
- A blocked preparing mission can be suspended by moving it out of the
  active list through the mission panel. This preserves the mission. Automatic
  reactivation/resumption remains to be reconciled with the previously chosen
  eager activation of every nonconflicting queued mission.
- World art should be original, bright, colorful, and contrasting, avoiding
  a generally dark or washed-out look. The owner's reference is Dead Cells
  sprites/animation, between its fantasy and Blasphemous realism, with the
  latter's more colorful palettes. These record the owner's desired aspects;
  no external comparison or factual palette assessment was conducted. The
  current project palette is neither final nor uniformly applied.
- UI direction: retro Mac OS 9, Diablo II, and dense 1990s/early-2000s Sid
  Meier game interfaces; many small boxes and controls organized intuitively.
  The three supplied examples are UI atmosphere only, to be translated into
  pixel art with intelligent layout. They are not instructions for world art.

External local sources actually inspected during this turn:

- `~/work/synarchy-lore/world.md`, all 75 lines,
  SHA-256 `6113a932c4b3341c519d58967400dee188a25aea4bc098a5c50caf5facba9bfe`.
- `~/work/synarchy-lore/factions/acolyte.md`, all 71 lines,
  SHA-256 `72980518e5528337fba981e13c09867921af5497aaac8fe737aae9209752bdcf`.
- `~/Desktop/examples/examplehud1.jpeg`, visually viewed,
  SHA-256 `ca875abf78ff182d774ed8733ad58b76a854b6641228f0579c89cd8bba0afd1e`:
  broad bottom dashboard, black panels with bright yellow double frames,
  portrait/information/icon areas, small floating text displays.
- `~/Desktop/examples/examplehud2.jpeg`, visually viewed,
  SHA-256 `36c1c8359478197f30b96e1f362d8b5c53fe821c09fe6e351bb1c3c1c4c57a9b`:
  bottom and right-edge panel banks, cream/gold beveled borders, mixed inset
  displays, icon grids, maps, and logs with saturated colored accents.
- `~/Desktop/examples/examplehud3.jpeg`, visually viewed,
  SHA-256 `9c1d91c6283e7bc06f8f312c294cc39086dcb695ad339e2a0dc0558f639a1801`:
  narrow upper control strips, wide lower instrument panels, illustrated
  buttons and dividers, cream/gold frames with red, black, and blue fields.

Image observations describe the references, not newly mandated colors or
panel arrangements. Their generated text and spatial clutter are not usable
specifications. No images were copied into the repo, modified, or generated.
The limited lore read does not complete a lore audit or the Synarchy corpus
review. The original Synarchy reading inventory is not expanded to imply
that every file in the separate lore repository now requires exhaustive review.

Two questions raised by these explicit answers and inspected sources:

1. Suspension must avoid automatic immediate reactivation. Does moving an
   active mission out create a manual pause until the player resumes it, or
   place it back into an automatically eligible queue with a selected ordering
   rule? The operation itself is accepted; the resumption policy is not.
2. The acolyte lore's "Gameplay and win condition" describes lore-path
   progression unlocking a final dungeon/enemy and a victory announcement,
   with the sandbox continuing afterward. Is this intended eventual gameplay
   or a provisional lore idea? The current no-units-alive defeat rule does
   not answer whether victory exists. The older player manual's no-scripted-
   victory statement is not authority to reject this lore without asking.

Lore also contains details about altars, return to Mont Gris, and alternate
beginnings. None are promoted into the current expedition return/deposit
contract or expanded player-faction scope through this limited context read.
Preserve the owner's request to keep the vision's lore summary concise.

### Owner resolution: utility-driven mission pause and provisional lore

Recorded 2026-09-22. This resolves the two questions immediately above.

- Moving a mission out of Active pauses it until the player explicitly
  resumes it. The mission's active flag governs whether its goal is eligible
  for unit work. At the next utility-function check, units following that
  goal notice the inactive mission, stop pursuing exploration, and choose
  another task. The mission and remembered goals are preserved.
- Resume operates through the same utility mechanism once the mission is
  active again. The existing unit-exclusivity rule and independent goal/need
  priorities remain. No new polling cadence, callback mechanism, transport
  cancellation implementation, or automatic return-home policy is selected.
- Automatic promotion must exclude manually paused missions, even when all
  their units are free. This is an explicit exception to the earlier rule
  that only unit conflicts could keep a mission inactive. A pause does not
  mean canceled, failed, or completed.
- The lore's victory sequence is definitely provisional. The owner explicitly
  directs that it remain in the lore and not be added to the vision. Removed
  both the open victory question and its source-section mention from the
  vision; the lore files are unchanged. This does not establish a permanent
  ban on future victory designs.

The vision now reflects these answers. It remains an incomplete working
draft pending the broader documentation review; no game code was changed.

### Follow-up vision gap review after foundational answers — 2026-09-22

The current vision now covers setting, player role, story generation,
simulation tradeoffs, colony purpose, dungeon progression, campaign defeat,
art direction, and mission suspension. Do not ask the owner to restate those
settled fundamentals. Remaining additions should strengthen the vision's
ability to decide tradeoffs, not turn it into an exhaustive feature catalog.

Recommended next topics, still proposals rather than accepted mechanics:

- **Lasting story records:** what survives saving/loading and character death,
  and what the player can later read about a unit, mission, or colony. The
  mission-history intention is established; retention and the relationship
  to transient notification streams are not. Current
  `docs/persistence_contract.md` section 3 explicitly resets notification
  queues and eventStoreRef, describing the event log as session-only. This
  does not prove every combat-log presentation or future biography is transient,
  nor justify silently changing the persistence contract. Distinguish facts,
  durable narrative records, and disposable notifications.
- **Character individuality:** which enduring differences should drive
  choices and stories beyond skills/equipment: values, temperament, beliefs,
  memories, relationships, or changes caused by experience. These examples
  are prompts for direction, not a newly required personality system.
- **Origin and pacing of events:** whether stories emerge solely from world
  actors and simulation, or an additional event-director system may introduce
  incidents to shape drama. Referencing both Dwarf Fortress and RimWorld does
  not answer this. No director, raid scaling, guaranteed rescue, or scripted
  disaster is implied by story generation.
- **Knowledge and narration:** what characters actually know, what the
  player can inspect, and how thoughts/beliefs differ from factual event
  accounts. Lore already distinguishes hidden history from participants'
  interpretations. Existing container knowledge is remembered observation,
  not a live global inventory (`docs/unified_item_transfers.md`, Container
  knowledge). Neither fact establishes a universal knowledge model. Suggest
  legible causal accounts without assuming omniscient characters.
- **World independence:** whether other groups pursue their own enduring
  activities between player encounters, and how much generated history or
  offscreen activity should shape stories. Lore supports distinct motives;
  simulation scope still needs explicit gameplay direction. Do not require
  every inhabitant of the entire planet to simulate in full detail.

Already established constraints still to summarize in the vision rather
than request again: continuing home-colony simulation, typical 5–50 controlled
units, the four-core/8-GB laptop class, coherent slowdown with responsive
controls, and whole-simulation fast-forward, as recorded by
`docs/gameplay_timing_design.md` D-3 and D-7–D-9. Those are intentions and
targets, not claims of achieved performance.

Suggested next questions are story retention across sessions/deaths and the
presence or absence of an event director. Keep character/world detail for
subsequent discussion so the owner is not asked to design every system at
once. Detailed UI layout and mission edge cases remain candidates for later
extraction into a linked design, preserving all approved decisions.

Read the current vision with an explicit supplementary read for the middle
sections truncated in combined output. Rechecked the cited existing timing,
persistence, and transfer passages. No full-corpus completion or new code
verification is claimed, and no new vision policy was selected by this review.

### Owner decision: history archives and configurable events

Recorded 2026-09-22. This answers the two questions from the latest vision
gap review without settling the other character/knowledge/world topics.

- Important history persists across save/load and character death. History
  grows too large to retain entirely in memory, so store it in files and let
  the history interface load the relevant file/records when the player wants
  to investigate an event. Dwarf Fortress is the owner's inspiration for
  this experience, not evidence of a verified external implementation.
- Event and character culling can be considered if archive growth becomes
  problematic. No retention cap, routine pruning, loss of deceased-character
  records, compression format, or index design is selected. This is durable
  history, not a silent change to the existing session-only event-log contract.
- An event system is desired and is not implemented yet according to the
  owner. It supplements the sandbox simulation with regular incidents to
  add variety. RimWorld's storytellers are a functional inspiration, but
  named storyteller personas do not fit the theme.
- Use simple world-creation settings to determine enabled event sets and
  their frequency/regularity. A mix of simulation-derived circumstances and
  scheduled/triggered events is intended. No adaptive difficulty director,
  intervention to force narrative outcomes, fixed recurrence interval,
  specific incident catalog, or later in-game editing of these settings is
  implied by this answer.

Later history design must reconcile archive ownership with save/load,
including loading earlier saves without silently presenting later events as
already happened. This is a design question to resolve under the existing
coherent-session contract, not a newly chosen branching or file-copy scheme.
The event-system design likewise needs explicit persistence and time semantics.

The vision now includes the approved history and event direction. Neither
history infrastructure nor event behavior was implemented by this update;
the full source review remains pending.

### Editorial extraction and focused reconciliation — 2026-09-22

The owner explicitly requested editorial cleanup, document reconciliation,
and a stop for clarification when uncertain. Applied the design-epic document
format to the extracted mission arc; repository ownership and master default
branch were verified before the new write. Broader vision/notes edits follow
the owner's expressly wider task, not the skill's usual single-file boundary.

- Moved the preceding V-3 text into
  `docs/designs/mission_system_design.md`, preserving its seven topic sections
  as D-1–D-7. The document remains exploring, with 12 question groups and no
  delivery slices. No epic or issue is proposed for immediate creation.
- Replaced V-3 in the vision with a concise overview and design link. Other
  owner-selected purpose, story, art, event, history, and survival directions
  remain. Added V-10 from previously documented approved timing/scale and
  continuity requirements, explicitly as targets rather than proven delivery.
- Read live #1229 and #2640 bodies and bounded open issue searches for overlap.
  These confirm the existing narrower integration work without granting a new
  mission-system readiness or changing its tracker scope.
- Read structure-interaction local lines 951–1188 and 1697–1805, primary
  portable-container lines 315–382, and primary faction-design lines 604–815
  and 942–976. Rechecked primary position-hold and discovery passages and
  structure-interaction D-9. Read ranges are evidence, not full-document
  completion; preserve the source hashes in the inventory.
- Created `docs/guide/vision_reconciliation.md` with authority distinctions,
  aligned contracts, intentionally deferred design choices, and RQ-1.

RQ-1 asks whether an explicit underground alert click selects the event's
vertical floor or preserves the current manual slice. The vision's alert
focus requirement and structure-interaction D-9 do not resolve that interaction
unambiguously. Do not choose on the owner's behalf. Reconciliation pauses for
this answer; the all-document read is still incomplete, not waived by cleanup.

### Owner clarification: explicit navigation controls vertical focus

Recorded 2026-09-22. The owner accepts changing visible slice when clicking
any camera-movement/navigation function, including an event. Use the alert's
z level, except "unless we are above ground", where the owner specifies the
default camera height the game starts with. Added mission-design D-8 and
resolved the original slice-changing-versus-horizontal-only choice.

One narrow semantic clarification remains before final reconciliation:
does above ground describe the destination, and does default height mean
the normal terrain-following mode rather than a fixed initial z? Source
reads of `cameraGotoTileFn` (Camera.hs 228–294), world_view.lua 477–498,
and default-camera declarations show that ordinary goToTile computes the
target surface plus headroom and enables tracking, Home enables tracking,
and the camera starts with tracking enabled. The current API accepts XY,
not an underground target z. These are source observations, not runtime
tests or a new implementation plan. No automatic following or zoom policy
is inferred from the owner's answer.

RQ-1a/Q-12 preserve the remaining interpretation explicitly. No rereading of
the corpus or completion of the broader review is implied by this update.

### Owner resolution: destination-based camera height

Recorded 2026-09-22. The owner confirms that "starting height" means normal
terrain-following height, not a fixed z. Restore it for above-ground
destinations regardless of the prior view. Underground notification jumps
set the visible slice to the notification's z to reveal the event through
terrain. RQ-1/RQ-1a and mission-design Q-12 are resolved by D-8; the broader
review resumes. No autonomous following, zoom policy, or runtime change is
implied by this documented navigation rule.

### Timing source reconciliation after camera clarification

Read the remaining primary `gameplay_timing_design.md` through line 633 and
every hunk of the distinct unpublished docs-wip version (654 lines). Both
hashes still match the captured inventory. The local D-12/D-13 record later
owner acceptance of the phase/wait graph, UI service taking precedence over
simulation throughput, and a scoped cooperative scheduler on the single
Lua owner. V-10 now preserves that stronger UI priority. These choices are
not unanswered questions and do not imply adopting Hetoimasia's runtime here.
Power credit, numerical response budgets, and missing-terrain consequences
retain their existing design gates. No scheduler implementation, event-clock
policy, history schema, or new performance target was selected in this pass.

Read all 1,277 lines of primary `chunk_residency_streaming_design.md` and the
complete diff against its distinct 1,272-line local version; both hashes still
match the inventory. The primary has newer CRS-2 evidence and its accepted
disposition, while the local variant retains extra delivery detail. The
accepted D-25 target is 4 GiB whole-process resident memory across gameplay,
generation, and loading, allowing slower generation/loading. V-10 carries
this forward as a target; the development-Mac measurements do not establish
minimum-machine compliance. Durable story-history files are separate from
deferred simulation hibernation and do not reopen that design's storage gate.
