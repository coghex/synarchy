# Vision editorial cleanup and reconciliation

Status: partial; source review continuing. Updated 2026-09-22.

## What changed

- [Vision](../vision.md) now separates product purpose and principles from
  detailed mission mechanics. V-3 is an overview with a link to the design.
- [Unit goals and group missions](../designs/mission_system_design.md)
  preserves all preceding V-3 material under D-1 through D-7, with explicit
  proposals and Q-1 through Q-12. It is exploring and has no delivery slices.
- V-10 summarizes previously documented owner decisions on background
  simulation, coherent time, responsive interaction, and target scale.
- The drafting notes retain conversational history and superseded choices.
  No existing source document, tracker status, or production behavior was
  changed. Nothing is published.

The user authorized editorial cleanup and document reconciliation, explicitly
requiring questions rather than assumptions when uncertain. Previously
deliberately deferred design questions stay deferred; this pass does not
silently resolve them or make the new design ready.

## Authority and version distinctions

The owning repository is `coghex/synarchy`; its default publication branch
was verified as `master` through GitHub on 2026-09-22. Edits remain uncommitted
in the branch-resolved `docs-wip` worktree.

Source versions and actual reading ranges are in
[the reading inventory](vision_reading_inventory.json). Primary-checkout
contracts and distinct unpublished owner-approved plans must be distinguished.
A clean older file in docs-wip does not override a newer primary contract.
Tracker queries below describe the read-time state, not a lasting status gate.

Once accepted, the vision supplies product direction, detailed designs own
mechanics and delivery, and current contracts continue to own implemented
invariants until deliberately revised. Lore is contextual authority for
setting, not automatic approval of every provisional gameplay idea in it.

## Reconciled relationships

| Topic | Evidence | Disposition |
| --- | --- | --- |
| Current and replacement engines | Owner's project-purpose answers | Synarchy develops this game's gameplay/assets; Hetoimasia develops the reusable engine. No inferred freeze, migration date, or compatibility promise. |
| Earlier expedition arc | `expedition_gameplay_loop.md`; live #1229 and #2640 bodies | The existing arc is narrower. Its existing outcomes and tests are not proof of the new goal/mission workflow. No existing issue scope changed. |
| Player direction and position holds | Primary engine contracts §Position hold | Preserve the contract; local-Move goal resumption is design Q-1, not an automatic new hold exception. |
| Units, mission groups, and faction teams | Faction design D-11, D-13–D-21, D-35 | Multiple relationship tags can coexist with one active mission commitment. Mission pause/cancel does not silently clear informed hostility. |
| Location clearing and loot return | Expedition D-20 and significant-item rules; new owner completion decisions | Location clearance and mission completion are distinct. Equipment can matter to a location predicate while being excluded from home-deposit requirements. |
| Capacity reserve | Portable-container D-4 and owner 10% minimum/25% target | Reserve thresholds are settled; the capacity metric and accounting remain design Q-2. No implicit new unit bulk limit. |
| Dungeon structures | Unpublished structure-interaction D-5/D-8 | Accepted stacked floors support future dungeons. An old arc's multi-level exclusion is not a permanent product prohibition. |
| UI artwork | Owner's art direction; three inspected local examples | References express atmosphere, not reusable project assets or already-approved generated art. Palette remains adjustable. |
| History | Persistence §3 and owner disk-history decision | Current transient notification policy remains intact; future durable story/mission archives need explicit state and save ownership. |
| Event direction | Owner's latest event-system answer | Simulation plus configurable regular incidents; no named storyteller personas or inferred adaptive director. |
| Performance and continuity | Gameplay timing D-3, D-7–D-9, plus unpublished accepted D-12/D-13; persistence §1 | Summarized as V-10, including UI service taking precedence over simulation throughput. The accepted Synarchy scheduler stays within its existing design; Hetoimasia's runtime is separate. Targets are not measurements; no actor cap, new clock design, or deterministic replay requirement. |
| Provisional lore victory | Owner's explicit response to lore read | Excluded from the vision. No further clarification required and no permanent anti-victory policy inferred. |
| Memory and off-camera continuity | Residency D-1–D-5, D-25–D-27, and newer primary CRS-2 results | Carry the accepted 4-GiB whole-process target into V-10 without claiming compliance. Residency must preserve gameplay consequences. History files do not authorize reviving deferred simulation hibernation; detailed-chunk storage remains measurement-gated. |

Tracker evidence:
[epic #1229](https://github.com/coghex/synarchy/issues/1229) and
[remaining integration child #2640](https://github.com/coghex/synarchy/issues/2640)
were open when read. The mission search returned no open matches; the
expedition search returned adjacent work. This is bounded overlap discovery,
not a comprehensive issue audit or permission to create another epic.

## RQ-1. Explicit alert focus and above-ground exception — resolved

The vision says clicking an arrival/enemy alert focuses the relevant party or
situation. The unpublished structure-interaction design's D-9 says floor
visibility uses only the camera's manual z level, and selecting a unit or
sending it to another floor does not automatically change that level.
Existing player-events navigation describes an XY pan.

The owner selected slice-changing navigation on 2026-09-22, applying to any
clicked camera movement function, including events. The slice should use the
alert's z, except above ground where it should return to the default camera
height at game start. This resolves the initial choice; see mission design
D-8. No autonomous camera following is introduced.

The owner's follow-up resolves RQ-1a: above-ground destinations always
restore normal terrain-following height. Underground notification jumps set
the slice to the notification's z level so the terrain reveals the event.
The previous camera height does not determine the destination behavior.
"Starting height" is not a fixed z. Mission design D-8 and vision V-4 now
record the complete rule. This reconciles explicit navigation with manual
slicing without introducing autonomous following.

The source review can continue. No implementation or source contract was
changed by recording the rule; the structure design and camera implementation
will need the explicit navigation exception when that behavior is delivered.

## Remaining review

The all-document review is not finished. The large inventory still contains
unread and partially read source versions, including historical reports and
design sections outside this focused pass. Filename searches, heading lists,
tracker searches, and this reconciliation table are not full reading.

Do not mark the vision authoritative, the mission design ready, or this
reconciliation complete until coverage is finished and material source
interactions are resolved. Other deliberately open product topics remain
character individuality, knowledge/narration, and world independence.
Mission implementation questions stay visible in the linked design.

## Validation for this editorial pass

Verified all seven extracted V-3 sections against the pre-edit content;
their bodies are preserved in the new design. After the camera clarification
and timing/residency reconciliation, checked 22 local Markdown links across
the vision, design, and this record; every target exists. Whitespace checks
passed. The vision is 310 lines; the exploring mission design is 540 lines
with eight decision topics, 12 question groups (Q-12 resolved), and zero
delivery slices.

The reading inventory currently has 16 read, six partial, and 229 unread
source targets. The primary checkout remains clean. No builds, runtime tests,
game launches, new assets, tracker changes, or publication were performed.
