# Synarchy vision

**Status: incomplete working draft, 2026-09-22.** The owner decisions below
are recorded, but the full documentation review and approval of this document
are pending. This draft must not yet replace downstream authorities. Once
approved, it is intended to be the project's upstream statement of purpose
and direction. Unresolved choices remain explicitly unresolved.

## V-1. Develop the game here; build the replacement engine separately

Synarchy exists primarily to develop the gameplay and assets for Ecce Homo.
Its present engine serves this one game. Making Synarchy itself a reusable
engine for unrelated games is not an independent project goal.

The owner is developing the modular, reusable replacement engine in
`~/work/hetoimasia`. Synarchy and Hetoimasia develop in parallel. Later,
the game will be integrated with the new engine in a separate Ecce Homo
repository that does not yet exist. Substantial work remains in both current
projects before that integration project can begin.

This establishes the projects' purposes. It does not establish a migration
date, a porting or compatibility contract, or a freeze on Synarchy development.

## V-2. Generate stories in an expedition-led sandbox

Ecce Homo is a science-fiction/fantasy colony sandbox set millions of years
in the future on a distant terraformed planet. The player guides a colony
of acolytes, an expedition exploring the world and retrieving religious
artifacts. Other factions have their own motives. The setting has an
extensive developed history maintained separately in `~/work/synarchy-lore`;
this vision establishes its gameplay role without reproducing that lore.

The central experience is story generation, in the tradition of Dwarf
Fortress and RimWorld. Generate a setting and characters, give them goals,
expose them to trials, and let interacting circumstances produce unexpected
events. The player should enjoy watching those events unfold and reading
about them. Combat logs, unit thoughts, and procedurally generated character
descriptions are part of making units feel like people and their experiences
worth following. No particular text-generation technology is selected here.

The sandbox should permit varied outcomes and surprising causal chains.
Expeditions create opportunities for stories; the colony supports the people
who undertake them. This is the purpose against which additional systems
and simulation detail should be judged.

Stories arise from both the sandbox simulation and an event system that
regularly introduces incidents to add variety. The simulation remains the
foundation. Simple world-creation settings determine which sets of events
can trigger and how frequently or regularly they occur. Named
"storyteller" personas do not fit the intended theme. This event system is
future work, not an implemented capability; event catalogs, scheduling,
and any adaptive pacing remain for later design.

The main loop is:

> Discover → prepare → travel → confront risk → recover something → return →
> improve the colony → reach farther next time.

The game is a sandbox. This loop supplies opportunity and progression;
following it is not compulsory for gameplay. The colony and expeditions
belong to the same experience: the player can develop the base while a party
is away, and recovered equipment and resources support subsequent play.

## V-3. Prove the complete expedition experience first

The first playable experience starts with placing the colony's initial
building and receiving its units. The player assigns Explore at a marked
location, reviews preparation, and authorizes departure. Units travel and
handle the expedition while the player can attend to the base. Alerts make
arrival and danger visible. The party confronts the location, searches for
valuables, returns, deposits required mission loot, and resumes colony life.

This is intended behavior, not a claim that the current game or an older
integration gate already delivers the whole experience.

A **goal** belongs to an individual unit; a **mission** coordinates a
player-controlled group. Units remember several goals and select work by
utility, with needs and independent goals able to take precedence over
mission work. Player guidance and individual judgment both matter.

The player controls mission priority, requirements, dispatch, pause, and
cancellation. Units perform routine preparation themselves. Recommendations
advise the player, while configured requisitions govern readiness.
Preparation blockers remain visible and correctable rather than silently
discarding the mission. Several missions can operate concurrently with
mutually exclusive unit sets.

Units can abandon an expedition when circumstances warrant it. The player
must be able to understand interruptions and failures. Clearing a location
and completing a mission are distinct: the expedition includes returning
home and depositing its required loot. Completed and failed mission records
preserve outcomes for later inspection, subject to the accepted cancellation
distinction in the linked design.

The one-time gameplay checklist teaches the expedition loop and provides a
repeatable development walkthrough. Its whole sequence should be achievable
during ordinary play; following the tutorial is optional in the sandbox.

[Unit goals and group missions](designs/mission_system_design.md) preserves
the detailed owner decisions: goal selection, queue promotion, pause/resume,
requisition ranges and capacity reserves, loot valuation, interface layout,
tutorial stages, and outcome rules. It also records unresolved interactions
and the first delivery boundary. That document remains exploring; no delivery
slices or implementation readiness are implied by this vision.

## V-4. Keep the player informed while attention is elsewhere

The player should be able to attend to the colony while an expedition travels.
Arrival and enemy encounters must be brought to the player's attention.
Clicking their alerts must focus the camera on the relevant party or situation.
Explicit camera-focus actions also control the visible vertical slice; this
applies to navigation actions generally, not only expedition alerts. An
above-ground destination restores terrain-following height. An underground
notification sets the visible slice to the notification's z level so the
terrain is cut away to reveal it. The destination determines this behavior,
regardless of the previously viewed height. See the
[mission design](designs/mission_system_design.md#d-8-explicit-camera-focus-updates-the-visible-slice).

The existing notification design gives the player separate per-category
controls for logging, popups, and pausing. Retain that established control;
the expedition description does not create a new mandatory-pause rule.
Defaults for any new expedition notification categories remain for their
designs. Camera movement without a player navigation action has not been
selected.

### Preserve the stories for later reading

Important character and colony history should survive save/load and character
death. The player should be able to investigate past events through a history
interface, including the established mission records. The growing archive
must not require keeping the whole history in memory: save history to files
and load the relevant records when the player requests them.

Exact storage, indexing, and save integration belong in a later design.
Event or character culling may be considered if growth becomes problematic;
no retention limit or automatic deletion policy is selected now. Durable
history is a new requirement distinct from existing transient notifications;
this vision does not claim that current event logs already provide it or
change their persistence contract by implication.

## V-5. Lead toward substantial underground dungeons

Dungeoning leads the gameplay direction. The longer-term ambition is giant,
procedurally generated underground dungeons that the player can watch their
units clear. Units gain skills and equipment as they progress. They should be
able to retreat and tackle the rest later with better skills and equipment.
Both player-directed withdrawal and individual AI abandonment are intended,
including fear-driven retreat and reassessment as the party loses strength.

Combat and skill checks should become more demanding as units descend.
Reaching a floor the party cannot overcome creates a reason to improve its
equipment or skills, bring more units, and return later. Dungeon dimensions,
generation mechanisms, and balance remain for design.

The owner proposes that an Explore mission in a multilevel dungeon target
only the next unexplored floor, with dungeon progress visible in the zoom-map
location tooltip or popup. Preserve this direction for the dungeon design;
partially explored floors, failed attempts, and the exact definition of floor
completion still need rules. A floor objective must be distinguished from
clearing the entire dungeon and from completing the return-home mission.

The first ruin's limited scope must not be mistaken for a permanent exclusion
of this ambition. Conversely, the ambition does not make giant procedural
dungeons a prerequisite for completing the first expedition slice.

## V-6. Grow the colony through later design slices

The colony supports expeditions and is the base of operations for other
activities, including future mining and farming. It should largely take care
of routine work itself, allowing the player to develop its capabilities and
follow the stories of its people.

Manufacturing and research support better equipment at the quantities needed
to outfit more units. Loot provides the best equipment, but not enough of it
for a large force. Beds and food production support the population the player
wants to sustain. Skills, equipment, and party size together determine which
challenges the colony can undertake. Later slices develop houses, workshops,
and other colony features; dungeoning remains the immediate lead.

Detailed colony mechanics and their delivery order are not settled by this
summary.

## V-7. Preserve expedition priority alongside parallel work

Completing the expedition experience remains the primary development goal.
Other design documents provide useful work that can proceed in parallel;
their existence does not replace that priority.

Neither the July infrastructure backlog nor an old first-slice exclusion is a
permanent boundary on the game. Scheduling and dependencies belong in their
designs and tracker work, interpreted in light of this priority.

## V-8. Simulation serves stories and enjoyable play

Realism earns its place by supporting storytelling. Detailed combat and
health simulation are critical because interacting consequences can produce
events no single system scripted: a unit survives a brutal injury, develops
an infection, becomes delirious, and later falls from a cliff. This is an
illustration of the desired causal depth, not a claim that this complete
sequence is implemented or a requirement to force that outcome.

Enjoyable, understandable gameplay takes precedence over realism. Systems
with less storytelling value, such as temperature, can be simplified rather
than receiving equal simulation depth by default. Specific simplifications
still require design and do not authorize arbitrary removal of existing
mechanics.

Survival should not demand constant feeding. Hunger drains slowly, and
skipping a few days of meals should not by itself kill a unit. Hydration is
needed through the day, but one day without water should impair a unit rather
than kill it by dehydration alone. The owner will tune rates and consequences
through play to achieve the desired feel; no numerical metabolic model is
fixed here.

The campaign's only failure condition is that no player-controlled units
remain alive. A lost mission, destroyed base, or setback does not independently
end the campaign while a controlled unit survives.

## V-9. Distinctive, colorful pixel art and a rich retro interface

The game should have an original visual identity with bright, colorful,
contrasting artwork, avoiding a generally dark or washed-out palette. The
owner's sprite and animation reference is Dead Cells, with a direction
between its fantasy and Blasphemous's realism and the more colorful parts of
the latter's palettes. These are references for qualities to pursue, not a
request to reproduce their assets. The project's existing palette is open
to revision and is not yet applied uniformly.

The UI should evoke older, detailed interfaces: Mac OS 9, Diablo II, and
1990s/early-2000s Sid Meier strategy/simulation games. Numerous small boxes,
controls, and information panels are part of the desired character, arranged
intelligently and intuitively with readable text and clear grouping.

The three images in `~/Desktop/examples/` are UI atmosphere references only,
not world-art references or literal layout specifications. They use framed
panel groups, dense pictorial controls, inset displays, and strong contrasting
accents. The intended result translates that richness into pixel art with
usable organization. Their exact colors, panel counts, and positions are not
new mandatory design rules. Reference observations are retained in the
vision drafting notes; these local files are not yet durable project assets.

## V-10. Preserve a continuous world and responsive controls

The colony and other gameplay-critical locations continue simulating while
the player attends elsewhere. Viewing or retaining a location in memory must
not silently redefine its gameplay consequences. Save/load preserves a
coherent classified session rather than requiring deterministic replay.

Gameplay systems should advance coherently, including during fast-forward.
Under load, simulation may slow, but movement, health, work, and other coupled
systems must not receive inconsistent time credit. Controls and the UI should
remain responsive even when gameplay becomes slow; servicing the UI takes
precedence over simulation throughput.

Existing planning targets are a typical colony of 5–50 player-controlled
units on a laptop with four CPU cores and 8 GB total machine RAM. These are
targets, not hard population limits or claims of measured final capacity.
The accepted whole-process resident-memory target is 4 GiB during gameplay,
world generation, and save loading; slower generation or loading is acceptable
to stay within it. This target is not a claim of current compliance. The
tentative base footprint and other numerical performance budgets remain in
their owning designs.

These principles carry forward the established
[gameplay timing decisions](gameplay_timing_design.md) D-3, D-7–D-9, and D-12 and the
[persistence contract](persistence_contract.md), together with the
[residency design](chunk_residency_streaming_design.md) D-1–D-5 and D-25.
They select no additional
engine architecture or replacement-engine migration requirements.

## Sources and remaining review

V-1 through V-9 derive from the owner's direct answers on 2026-09-22,
recorded in [vision drafting notes](guide/vision_drafting_notes.md).
The [expedition design](expedition_gameplay_loop.md), especially its desired
experience and D-19, provides earlier context for the loop and dungeon
ambition. Its narrower direct-retrieval decision D-7 is not evidence of a
complete Explore workflow.

V-4's notification controls follow the existing
[player-events decisions](history/player_events.md) (decisions 5–6) and the
current `data/notification_categories.yaml` contract. The archived document's
retired per-category navigation buttons are not adopted.

The lore context was checked against `~/work/synarchy-lore/world.md` and
`factions/acolyte.md`. Their detailed reveals and mechanics are not promoted
to vision requirements by that limited read. All three local UI reference
images were visually inspected.

This vision is the intended upstream authority after owner approval.
Detailed designs own mechanics and delivery plans; engine contracts describe
existing guarantees and validation gates. A draft vision does not silently
rewrite those contracts. The lore repository owns setting detail, including
provisional ideas not selected for gameplay.

The [reconciliation record](guide/vision_reconciliation.md) distinguishes
confirmed alignment, explicit future changes, and unresolved source
interactions. Previously recorded owner decisions are not reopened merely
because they were documented elsewhere.

The rest of the project documentation is still being read. Existing subsystem
contracts, accepted designs, unresolved choices, and historical records must
be reconciled before this document is presented as a complete vision.
The [reading inventory](guide/vision_reading_inventory.json) records exact
source versions and actual reading progress; it is not a code-review ledger.

No architecture, persistence, art, platform, or migration policy has been
newly selected through silence or inferred from the replacement-engine plan.
Any proposed additions or contradictions found during the remaining reading
must be resolved with the owner before they become settled vision.
