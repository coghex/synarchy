# Unit goals and group missions design

Capture the owner's expedition and mission decisions in one place while
[the vision](../vision.md) states the game's purpose and priorities.

Design state: `exploring`

This is an editorial extraction of the owner's 2026-09-22 decisions, not a
new approval of unresolved behavior, an implementation specification ready
for delivery, or a replacement for existing engine contracts. Explicit
proposals and open questions remain distinguished below.

Status legend: `[ ]` unprocessed · `[#N]` linked to issue N ·
`[no-issue]` deliberately not tracked separately · `[deferred]` blocked on
a concrete precondition.

## Processing status

- [ ] EPIC. Coordinate individual goals and group missions

No delivery slices are selected. The EPIC entry is a workflow cursor, not
approval to create a new umbrella issue. Resolve overlap with existing
expedition work and the open scope questions before tracker processing.

## Epic contract

- **Goal:** The player can plan and dispatch expeditions through missions,
  while individual units pursue goals, satisfy needs, prepare supplies, and
  respond to danger.
- **Intended completion evidence:** An ordinary gameplay run demonstrates
  preparation, explicit dispatch, exploration, return and required loot
  storage, history, cancellation/failure, and the one-time tutorial.
  Queue, pause, and shared-unit behavior must preserve the decisions below.
  The first delivery boundary is unresolved (Q-10).
- **Users and operators:** Players managing colony expeditions and developers
  using the tutorial sequence as a repeatable gameplay walkthrough.
- **Arc label:** None proposed.

## Current state and evidence

The new mission model is intended behavior, not verified implemented behavior.

- [The earlier expedition arc](../expedition_gameplay_loop.md) owns its
  existing encounter, discovery, significant-item, tutorial, and integrated
  probe scope. [Epic #1229](https://github.com/coghex/synarchy/issues/1229)
  remains open; its remaining child
  [#2640](https://github.com/coghex/synarchy/issues/2640) extends the integrated
  confrontation/advancement gate. Both bodies were read on 2026-09-22.
  Finishing that narrow gate does not implement this mission model.
- [Engine contracts](../engine_contracts.md) §Position hold establishes that
  arrival from a player Move holds position until explicitly released.
  Goal preservation alone does not release that hold.
- [Portable containers](../portable_loot_containers.md) D-4 distinguishes
  weight from packing bulk; its unit inventories are weight-limited.
  The 10%/25% reserve does not implicitly introduce a new capacity metric.
- [Structure interaction](../structure_interaction_design.md) D-5/D-8/D-9
  records accepted stacked floors and manual camera slicing in the local,
  unpublished design. Those are plans, not evidence of implementation.
- [Faction identity and relations](../faction_tag_system_design.md) D-11
  permits several relationship-team tags per unit. The one-active-mission
  rule is a work commitment, not a ban on those memberships. D-17/D-19/D-35
  distinguish stopping an order from erasing informed hostility; mission
  cancellation must not implicitly redefine diplomatic state.
- [Persistence](../persistence_contract.md) remains the authority for state
  classification and migration. Existing transient notification streams are
  not a durable mission-history implementation.

Tracker searches found no open title/body match for `mission` and found the
related expedition issues above. That bounded check is not final per-slice
deduplication or a claim that every overlapping issue was examined.

## Scope and authority

This draft covers goals, mission scheduling, expedition preparation and
dispatch, individual interruption/withdrawal, outcomes and history, and the
tutorial connection. Future dungeon-floor missions are proposed in the
vision and require their own completion rules (Q-9).

This editorial task selects no delivery order, data schema, callback API,
new artwork, implementation, or tracker mutation. Other mission types,
procedural dungeon generation, character-history storage, event scheduling,
and broader colony systems retain their own future design work; this does
not exclude them from the product vision.

## Decisions

D-1 through D-7 preserve the owner's decisions previously embedded in V-3 of
the vision. Passages expressly marked proposed or open are not decisions
merely because they occur within these topics.

### D-1. Prove the expedition experience

The first playable slice should let the player:

1. Place the starting building and receive the starting units.
2. Select units, right-click a location marked on the map, and choose
   **Explore**. A group mission coordinates individual Explore goals. Units
   assess the trip and requisition supplies, with estimated requirements
   available for the player to review and edit in the mission panel.
3. Authorize departure once the mission is ready. Let units pursue the
   journey while continuing work on the base, subject
   to their needs and their judgment that the expedition is feasible.
4. Receive alerts when the party arrives or encounters enemies, and click
   an alert to focus the camera on the relevant situation.
5. Have the party clear the location, search it for all containers before
   taking valuables, and bring the spoils home.
6. Have the returning units store the spoils and resume their daily duties,
   with rest before another expedition.

This describes intended play, not a declaration that the sequence is already
implemented or verified. The owner identifies substantial missing or poorly
implemented behavior in this loop. Completion of an earlier, narrower issue
or expedition gate does not establish completion of this experience.

### D-2. Individual goals and group mission scheduling

Explore establishes a personal goal for each assigned unit. A goal influences
action utility; it is not an unconditional command or necessarily the
highest-priority action. Units' needs come first: a starving unit searches
for food instead of pursuing its expedition. Meeting those needs allows the
goal to resume, subject to feasibility.

An expedition goal can outweigh routine work. During preparation, estimated
supply needs advise the player; they do not give the unit a veto over the
player's configured requisitions. Units do not repeatedly reassess the
expedition during preparation. Mission creation always creates the mission
and assigns its participants their goals, regardless of expedition
feasibility. Preparation blockers keep the mission red and editable; they
do not cause outright refusal or automatic goal cancellation. Distance
informs recommendations rather than an assignment veto. During the
expedition, units can reassess feasibility and choose to abandon it. These
statements do not silently redefine every immediate player command.

Player intervention has two meanings that must remain distinct. An order to
send units back cancels the expedition goal and replaces it with the new
player order. The player must have an explicit cancellation option.
Local direction within the expedition, such as moving a unit between dungeon
floors, preserves the goal. The precise interaction between that temporary
direction and the existing position hold remains to be designed; retaining a
goal does not alone establish when its actions resume.

Each unit can also cancel its own goal. Retreat is an individual judgment,
not an automatic party-wide decision. A unit may flee from fear or abandon an
expedition after severe injuries or deaths. During an expedition, remaining
units should reassess
their chances using the party's reduced strength, and independently abandon
the expedition if they judge that strength insufficient. The intended
cooperation includes communicating withdrawals; the communication mechanism
and how units learn of an absent member remain for later design.

A unit's expedition cancellation produces a red player-log message
identifying the unit and reason:

> Expedition canceled for unit <unit name>: <cancellation_reason>

Cancellation during an expedition likewise reports who canceled and why.
The player decides what to do next. Actual cancellation removes that goal;
it does not leave it queued for automatic reactivation. A blocked preparation
goal remains assigned, as does a goal waiting for its mission's turn or the
player's dispatch authorization. These goals may persist for long periods.

A **goal** belongs to an individual unit; a **mission** coordinates a
player-controlled group of units. Units remember multiple goals from the
beginning, including goals associated with queued missions and independent
goals. Each goal specifies its importance through a utility function. Units
can handle other tasks or goals while missions wait; a blocked mission is
not automatically canceled.

Find Water is an independent goal, not a mission. It remains in goal memory
alongside mission goals and should have sufficiently high utility that a
unit completes it before engaging in a mission goal. This supersedes the
earlier rule that assigning Explore cancels Find Water. Underlying needs
still take precedence over goal work.

One master queue orders pending missions, initially oldest first and freely
reorderable by the player. Several missions can be active concurrently only
when their sets of participating units are mutually exclusive. A unit can
belong to several queued missions but at most one active mission. It does
not rank mission goals itself: only the goal associated with its active
mission is eligible for mission work. Independent goals such as Find Water
still compete through utility.

Promotion scans the master queue in player-defined order, skipping manually
paused missions and activating each eligible mission whose participants do
not overlap any currently active mission, including missions activated
earlier in that scan. A conflicting mission stays queued, but the scan
continues to later entries. Among missions not manually paused, only those
with at least one unit committed to an active mission remain queued. This
supersedes the earlier rule that a conflicting queue head stops promotion.
When a mission completes and frees its units, promotion runs again.
Reordering within the active list changes display order only, not execution.
The player can pause a mission by moving it out of the active list in the
mission menu, including when it is stuck in preparation. This clears its
active flag and preserves the mission and its associated goals. At their
next utility evaluation, units pursuing that mission see that it is inactive,
stop pursuing its exploration goal, and select another task. Pausing does
not require a separate cancellation of each unit's goal.

A manually paused mission stays inactive until the player resumes it; queue
promotion must not automatically undo the pause. Resumption remains subject
to the rule that active missions have mutually exclusive units. Once active
again, its goal becomes eligible through the same utility evaluation, with
independent goals and needs still applying. Pausing and resuming do not
themselves constitute mission failure or successful completion.

Activation commits units to a mission, including its preparation; it is
distinct from the player's later authorization to depart. Neither sorting
nor a preparation blocker automatically cancels a mission. The master queue
can assign different combinations of units to successive missions, without
requiring permanent teams. Successful completion includes the return and
mission-loot storage described below. Failed or aborted mission endings and
individual withdrawals still need precise release rules; preparation
blockers alone do not end an active mission.

Numerical utility, feasibility, fear, and casualty thresholds belong in later
designs; none are selected here.

### D-3. Player-configured preparation and carrying capacity

Units should requisition expedition goods for themselves. The normal flow
must not require the player to send each unit to storage and manually select
each supply. Preparation is part of acting on Explore, before departure.

Mission creation fills suggested per-unit requisitions from the journey
distance and perceived difficulty of the location. For an unknown location,
the estimate budgets
food and water for the outward and return journeys, a few minutes of
exploration, and an additional buffer. Once a location's type is known, the
estimate changes accordingly: a dungeon calls for weapons and a longer stay;
a simple single building needs less provisioning. Future location designs
can extend these expectations. First-aid kits, canteens, rations, and weapons
belong in the preparation assessment.

The player sets minimum and maximum quantities. A range of 1–2 rations means
at least one, with a second desired if capacity permits, and no more than two.
These settings govern items already carried before mission creation as well
as newly collected supplies. Setting rations to 0 minimum and 0 maximum makes
the unit store its existing rations nearby at base. An estimated need for two
rations does not prevent departure with that player-selected zero-ration load.

Preparation requires at least 10% of inventory capacity free for loot to be
ready. Units should aim for 25% free space: below that target they try to
store items they consider unneeded, retaining items they value within the
player's configured limits. Reaching 25% is desirable, not a readiness
requirement. This replaces the earlier 10–15% range. Units must not
overencumber themselves. Before collecting requisition goods, a unit must
establish that it has room for the required load. A unit unable to fit it retains its goal,
the mission stays red, and an error exclamation icon identifies that unit in
the mission screen. The player can edit requirements to resolve the blocker.
Storing discretionary items serves making room and pursuing the free-space
target; it is not an unconditional emptying of the inventory. The capacity
measure and exact sequencing of capacity checks, storing, and collection
remain for design; the draft does not select a new inventory model.

### D-4. Mission management and explicit dispatch

Selecting multiple units to Explore creates a mission and assigns each unit
the corresponding goal even when preparation is blocked. Units prepare for
the mission but do not depart until it is ready and the player explicitly
starts it. Queuing a mission does not authorize automatic departure.

The gameplay HUD shows the active mission list at the bottom right. Selecting
that box opens the complete mission list in a tall central window: active
missions at the bottom, visibly separated from the master queue extending
upward. Available, Completed, and Failed tabs select the upper list; active
missions remain visible at the bottom regardless of the selected tab.
Queue reordering controls scheduling; active-list reordering is
visual only. Drag reordering is the desired interaction, with move-up and
move-down arrows proposed for the first slice. Mission status, blockers, and
achievable alternatives must be apparent. The mission framework is intended
to accommodate activities beyond exploration; those activities are not yet
specified.

Opening a mission's details lets the player tab through the expected units
and see their existing inventories. Red boxes
show absent or insufficient required items, with quantities held and needed.
Clicking an unmet requirement allows its quantities to be edited. The player
can also limit other carried items and have excess items stored at base.
A column on the right applies common requirements to all participating
units, overriding existing individual custom requirements. Per-unit tabs
remain available for editing individual requirements. Whether these common
values also become defaults for subsequently added units is unspecified.
A green check and red X communicate readiness; clicking the ready mission's green
check starts it and changes the indicator to a yellow in-progress arrow.
Whether the red X also acts as a cancellation control remains unspecified.

During preparation, a unit that dies or cancels its goal is automatically
removed from the mission and the player receives a warning. This does not
trigger the remaining units to reassess the mission or change their
requisitions. Personal cancellation remains possible; it is distinct from
automatic party reassessment.

A mission warning icon and tooltip should explain departures from
recommendations, such as a distance-dependent recommended minimum number of
rations per unit. Falling below those recommendations warns the player but
does not block Start when the configured requisitions are satisfied.

Readiness reflects current compliance with configured requisitions. If a
waiting unit eats a ration and falls below its minimum, the mission immediately
turns red and the unit works to obtain a replacement. This updates compliance,
not the recommendation or the unit's judgment of the expedition. The owner
proposes change callbacks to achieve this responsiveness; the implementation
mechanism remains a design recommendation rather than an established engine
contract.

**Open preparation and mission choices:** recovery from unavailable required
stock or nearby storage; shared supplies and first-aid coverage; queue
scheduling and availability; and whether loadout limits apply after
departure. Single-unit presentation and empty missions also remain
unspecified. Detailed estimation, stock allocation, and resumption belong in
a later design.

### D-5. Completion, failure, and history

An expedition remains a mission through the return home and storage of its
loot items. Location-specific objectives determine whether loot is required
for completion. Base materials and equipment are outside the expedition's
loot-deposit requirement; completion does not require emptying every unit's
inventory. This distinction needs an explicit item/objective definition in
the later design rather than treating every acquired item as required loot.

For a location-specific mission with no loot requirement, completion still
requires return home. The owner proposes a successful homeward-path arrival
callback as the preferable completion signal, with proximity to the home
base as an alternative. The exact arrival mechanism and party-level
aggregation remain for design. Merely ending or canceling a path must not
be interpreted as confirmed arrival.

Completed missions leave the active list and enter a completed-mission
history, hidden by default but available for the player to open. That history
lets the player review past missions, encounters, and loot rewards. Missions
that fail have their own Failed tab, separate from Completed and Available.
An expedition that retreats and returns without required loot is a failed
outcome, not successful completion; ending the mission releases its units
for queue promotion.

Player cancellation before clearing the location is not recorded in Failed.
If the player cancels after clearing the location but before the mission
completes its return and loot-storage obligations, record it as failed,
including loot, encounters, and the failure reason "player cancelled".
Clearing the location is therefore distinct from successfully completing
the expedition. Location-specific clearing conditions, other failure
conditions, and treatment of partial-party outcomes remain for later design.

### D-6. One-time tutorial and gameplay walkthrough

Move the existing gameplay checklist to the top right; active missions
occupy the bottom right. The intended checklist sequence begins when the
player creates an exploration mission through Explore:

1. Show "Prepare an exploration mission" and check it when the mission is
   ready and its units have met their requisitions.
2. Show "Dispatch an expedition" and check it when the player clicks the
   green check to authorize departure.
3. Continue with "Discover the location", followed by an objective such as
   "Explore the <location_name>". Location type can determine the objective
   and its completion requirements, such as exploring ruins or clearing a
   dungeon.

The checklist is one-time tutorial guidance and a sequence of linear goals.
When the first mission is completed, its "Complete a mission" sequence is
finished and disappears; subsequent missions do not repeat the tutorial.
The checklist also serves development: progressing through the entire
checklist should be possible during each gameplay run. It does not make
following the tutorial compulsory in the sandbox.

These milestones describe intended behavior, not verified current
implementation. Attribution of earlier steps when missions overlap and
whether checked steps revert if readiness is lost remain for later design.

### D-7. Personal item valuation and loot replacement

During an expedition, a full inventory prompts a judgment about whether new
loot warrants replacing carried items. Units may drop their least valued
items to take something they value more. This valuation is distinct from
monetary price and accounts for usefulness and carrying cost: compact
hardware can be preferable to heavy steel plate, while life-sustaining water
normally matters more than either construction material or valuable treasure.

Survival supplies are not protected by an absolute prohibition on dropping
them. The design must allow a unit to value an item so strongly that it
sacrifices its water canteen, even at the cost of its life. How that exceptional
valuation arises, and how it interacts with needs taking priority in action
selection, remain to be designed; no universal personality or scoring formula
is selected here.

### D-8. Explicit camera focus updates the visible slice

The owner resolved camera-focus behavior on 2026-09-22: clicked navigation
actions, including events, must update vertical visibility as well as camera
position. Above-ground destinations restore terrain-following height, as
with Home; "starting height" does not mean a fixed initial z. Underground
notifications set the visible slice to the notification's z, cutting through
terrain to show the incident. The destination determines which rule applies,
not the camera's previous location or height.

This is explicit player navigation. It does not establish automatic slice
changes when a unit moves or is selected, or continuous camera following.
It is a scoped addition to structure-interaction D-9's manual-slicing policy.

## Proposals awaiting design

- Change notifications for inventory, item contents, requisition edits, and
  membership could keep readiness current without broad periodic scans.
  The owner proposed callbacks; their mechanism and a dispatch-time
  current-state validation remain design work.
- A successful homeward-arrival callback is the owner's preferred proposed
  completion signal for missions without loot requirements; the precise
  signal and aggregation rule are not selected.
- Explore could target the next unexplored dungeon floor, with progress in
  the zoom-map tooltip/popup. Partial floors, retry, and clearing need rules.
- Drag reordering is desired; up/down arrows are the proposed first-slice
  control. Neither establishes the complete first delivery boundary.

## Open questions

### Q-1. Local direction and persistent position holds

Moving within a dungeon preserves the expedition goal. Does mission work
resume automatically after that local Move, or only after an explicit
release? Existing position holds intentionally suppress autonomous work.
Do not select a new exception while implementing goal persistence.

### Q-2. Preparation capacity, quantities, and unavailable storage

Define the reserve's capacity measure against the existing weight/bulk model,
the sequence of storing excess goods and collecting requisitions, and
behavior when required stock or usable storage is absent. Shared supplies,
first-aid coverage, and nested/filled item accounting need explicit policy.
Inability to prepare retains the goal and reports a blocker; that is settled.

### Q-3. Party completion and partial-party outcomes

How do deaths, individual withdrawals, separation, or an empty roster affect
mission outcome and release? A unit canceling its own goal is not yet a
definition of the whole mission's failure. Preserve the accepted player-
cancellation history distinction and required return/storage duties.

### Q-4. Availability after pause, cancellation, or withdrawal

Pausing preserves goals and changes utility eligibility. If a unit is still
far from home, how does it become ready to prepare for a different mission?
Do not infer teleportation, immediate physical readiness, or automatic return
merely from freeing its active-mission membership.

### Q-5. Independent goals and dispatch readiness

Find Water can outrank mission work even when configured supplies are
satisfied. Define how readiness and blockers represent that situation.
Player edits override supply recommendations; no general override of
independent goals or survival needs has been selected.

### Q-6. Configuration and remaining controls

Clarify single-unit mission presentation, red-X interaction beyond status,
defaults for later-added members, and editing memberships without violating
active-mission exclusivity. New UI assets still need explicit inventories
and the repository's normal art/signoff process before implementation.

### Q-7. Tutorial attribution and reversible readiness

When missions overlap, which mission advances a pending tutorial stage?
Does a checked preparation stage stay checked if readiness later lapses?
Tutorial completion is one-time; live mission readiness must still update.

### Q-8. History, persistence, and event attribution

Classify goals, queues, loadouts, mission progress, outcomes, tutorial state,
and history under the existing persistence contract. Distinguish recovered,
carried, lost, and deposited loot in outcome records without assuming that
everything recorded was delivered. Plan archive/save ownership with the
future character/colony history system; choose no storage schema here.

### Q-9. Location, dungeon floor, and mission completion

Reconcile authored location clearance, a future floor objective, mission loot
requirements, and return completion without treating them as one predicate.
Unique equipment can be significant for location clearing while excluded
from a mission's home-deposit requirement; those are not inherently
contradictory. Define post-clear cancellation for future floor missions.

### Q-10. First delivery scope and relationship to existing work

Which parts form the initial player-verifiable delivery, and which come
later? Do not silently defer owner-requested queueing, several remembered
goals, concurrent missions, history, or UI. The existing #1229/#2640 scope
remains intact until an explicitly authorized change, and must not be
counted as proof of this larger design.

### Q-11. Loot judgment and need priorities

Define how exceptional willingness to sacrifice survival supplies interacts
with needs-first action selection. The owner explicitly wants both the
normal survival preference and exceptional self-endangering valuation.
Do not create an absolute essentials lock or invent a personality formula.

### Q-12. Alert focus and above-ground height — resolved by D-8

Use destination-based terrain following above ground and the notification's
z slice underground. Both the general navigation exception and the meaning
of default height are settled; do not reopen them as unanswered choices.

Current-source evidence: `cameraGotoTileFn` in
`src/Engine/Scripting/Lua/API/Camera.hs` accepts XY, derives target elevation
plus surface headroom, and enables z tracking; `world_view.lua`'s Home action
calls `camera.setZTracking(true)`. The default camera also starts with tracking
enabled. Thus current navigation already updates surface height, but cannot
name an underground event's z through those XY arguments. This was a narrow
source read, not runtime validation. Cross-page navigation remains unspecified.

## Verification direction

The owner wants the full tutorial to be achievable during a gameplay run.
Future verification must exercise real preparation, dispatch, exploration,
return, and outcomes, including shared-unit conflicts, pause/resume,
inventory changes, and save/load. Historical gate success proves only that
gate's scope. UI evidence must use rendered gameplay; no game was launched
and no behavioral tests were run for this editorial extraction.

These are verification subjects, not selected acceptance commands or
approved delivery slices. Unresolved semantics must be settled before their
tests can define expected behavior.

## Delivery plan

No slices have been chosen (0 slices). Keep this document exploring until
the owner resolves material questions and accepts a delivery boundary.
Do not create issues through this editorial task.

## Source notes

Primary owner source:
[vision drafting notes](../guide/vision_drafting_notes.md), including the
successive corrections made on 2026-09-22. The chronology preserves rejected
single-goal, immediate-refusal, automatic-preparation-reassessment, and
stop-at-first-conflict proposals without making them current requirements.

The extraction preserves the preceding V-3 text under stable topic decisions.
The [reconciliation record](../guide/vision_reconciliation.md) identifies
source authority, retained questions, and incomplete reading coverage.
