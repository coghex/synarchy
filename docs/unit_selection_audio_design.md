# Unit selection audio: brown bear pilot

Selecting a unit should elicit a response belonging to that unit: a bear makes
a bear sound, and a speaking character can eventually acknowledge the player.
The brown bear is the first complete example. Its purpose is to establish a
reusable system that the owner can populate gradually while developing the game.

Design state: `exploring`

The [audio foundation design](audio_system_design.md) remains the authority for
the native mixer, worker, configuration, devices, and public playback API. This
document owns the proposed selection-response extension and its content pilot;
it does not silently revise the foundation's approved scope or readiness.

Status legend: `[ ]` unprocessed · `[#N]` linked to issue N · `[no-issue]`
reviewed and deliberately not tracked separately · `[deferred]` blocked on a
concrete precondition

## Processing status

- [ ] EPIC. Decide whether the selection pilot needs an umbrella tracker
- [ ] USA-1. Prove brown-bear selection responses with an approved sound set

The EPIC entry is an unresolved tracker decision, not authorization to create
an epic. The audio foundation already has its own AUD-1 through AUD-12 delivery
plan. D-4 includes that prerequisite work in this session.

## Epic contract

- **Goal:** the player hears a recognizable brown-bear acknowledgment when
  selecting a bear, with variation and controlled repetition.
- **Done when:** the agreed selection gestures trigger the approved recordings
  through the actual audio runtime; the owner accepts the sound and interaction
  in the game; automated tests cover selection, variation, and lifecycle rules;
  and the authoring instructions explain how to add another unit's responses.
- **Users and operators:** players, the owner authoring future unit sounds, and
  maintainers of selection and audio playback.
- **Arc label:** None proposed.

## Current state and evidence

Examined at `5b677136fdacb75a604da796e066c50710c128b7` on 2026-09-10.

- `synarchy.cabal`, `src/Engine/Core/Workers.hs`, and the `src/Engine`, `cbits`,
  `data`, and `config` inventories contain no audio runtime, audio worker,
  production audio catalog, or bear recordings. The foundation design has
  twelve unprocessed implementation slices.
- Foundation D-11 currently permits exactly one source per sound ID and
  explicitly excludes random variant selection. D-26 leaves the production
  sound library for a later content effort. Response sets therefore need a
  deliberate extension contract before implementation.
- The [approved menu reference](audio/menu_cues/README.md), MC-4, explicitly
  leaves unit-selection sounds for later. Those menu clicks do not settle
  this unit's sound or the selection trigger policy.
- `data/units/bear_brown.yaml` identifies the existing unit as `bear_brown`
  with display name `Brown Bear`.
- `scripts/init_mouse_entity.lua` handles direct selection and Shift-add;
  `scripts/unit_drag_select.lua` commits box selection on release;
  `scripts/init_context_menu.lua` can select a unit through Info. One raw
  mouse event or an unconditional wrapper around every selection mutation
  would not by itself establish the desired player-action semantics.
- The foreground discovery found no matching audio issue, open PR, other
  worktree, or overlapping active reservation. The approved menu-reference
  PR #2574 is already merged.

## Decisions

### D-1. Responses belong to the selected unit

On 2026-09-10 the owner described the Warcraft III style of distinct unit
response libraries as the desired reference: selecting a bear should make a
bear sound. A generic UI synth tone is not the chosen direction for this pilot.
This is a reference for interaction and personality, not a request to reuse
another game's recordings or dialogue.

### D-2. Start with the brown bear and grow the library gradually

The owner accepted a brown-bear pilot with three short selection responses,
avoiding immediate repeats, preventing rapid-click sound spam, and allowing
one responder for group selection. The remaining roster and order, combat,
injury, and ambient sound libraries are later content work. Exact playback
parameters and gesture behavior remain proposals until settled below.

### D-3. Complete the foreground work before opening a PR

The owner wants to develop the pilot together in this session and complete
the work and acceptance before making a PR. Do not open an early draft PR.
Keep implementation, content provenance, contracts, tests, and owner evidence
together in the implementation worktree. Publish only after that work is
complete. Tracker choice is still undecided; no issue or epic has been created.

### D-4. Implement the audio foundation in this foreground session

On 2026-09-10 the owner confirmed that the earlier synth was built in a separate
project and explicitly chose to implement Synarchy's sound here. Q-1 is resolved:
follow the foundation's AUD-1 through AUD-12 contracts locally, then prove the
bear selection extension. Complete the work and listening acceptance before
opening a PR. The separate Idou project is reference material and is unchanged.

### D-5. Build the preview audio player before the bear pilot

On 2026-09-10 the owner chose preview audio as the next foreground step:
`--preview audio`, a bottom-left Audio button in visual preview, Synth and Files
categories, the two existing engine-synth menu cues, and direct local-file
playback. Complete this player together before developing bear sounds. This
explicitly amends the foundation's earlier no-audio-in-preview scope; it does
not authorize a PR before the foreground work and owner acceptance are complete.

The owner will try the engine synth for the bear first, then supply WAV files
if synthesis does not achieve the desired result. Exact bear sounds and their
listening verdict remain to be developed after the player is usable.

### D-6. Publish the foundation and preview player before the bear pilot

On 2026-09-11 the owner accepted the preview's appearance and said it was ready
for a PR. Deliver the completed audio foundation and preview player as one
standalone PR. This replaces D-3/D-5's earlier requirement to finish the bear
pilot before opening a PR. Bear synthesis, owner-authored WAV alternatives,
response sets and selection integration remain follow-up foreground work.
The owner's statement is PR readiness feedback, not a recorded physical-device
listening or device-recovery verdict.

## Proposed design

### Content and response sets

Associate the bear's selection response with an authored set of short variants.
Try the engine synth first, with owner-authored WAV files as the fallback. The owner decides
the exact character from actual audio, including how it feels when repeated.
The owner selected that development order in D-5; no final bear assets exist yet.

Expose one semantic response identity to selection code. Put variant membership
and playback policy in authored data so adding another unit does not require
another species-specific code branch. Decide the response-set owner and its
relationship to foundation SoundIds before changing the existing schemas.

Use randomness isolated from gameplay. Avoid the immediately previous variant
when more than one exists. Define cooldown across the whole response set so
choosing a different clip cannot bypass suppression. Empty or absent content
must have an explicit policy; do not silently substitute the menu click.

### Selection intent and playback

Trigger a response from an accepted player selection action. Pin the exact
behavior for direct selection, repeated selection, Shift-add, box selection,
and Info before implementation. Selection restoration, HUD rebuilding, and
automated inspection must not accidentally become player acknowledgments.

Group selection yields at most one response. The responder rule must be stable
and should remain sensible when more species acquire sound sets. Decide how
an ongoing response interacts with a new selection and whether the sound is
spatially anchored to the bear or kept consistently audible as feedback.

### Runtime and state ownership

Actual in-game proof requires the foundation playback path. An offline audition
is content evidence and does not satisfy the in-game done condition. Follow
the foundation's real/null device rules, capability boundary, shutdown order,
and load reset rather than adding a separate process-based playback path.

Response history, cooldown, and active playback are transient. Classify any new
owners under the persistence inventory and reset stale selection/audio work
at session replacement. Do not consume gameplay random draws for variation.

## Open questions

### Q-1. Does this foreground session implement the audio foundation too?

Resolved by D-4. Foundation implementation belongs in this session. Offline
auditions remain content evidence; the pilot's acceptance also requires the
implemented in-game path.

### Q-2. How will bear sounds be supplied?

Resolved in D-5: try the engine synth after completing the preview player, then
owner-authored WAV files if needed. Preserve accepted definitions or files,
provenance appropriate to their source, and the owner's listening verdict.

### Q-3. What are the exact selection-response semantics?

Settle repeated selection, Shift-add, Info, group responder, spatial routing,
overlap/interruption, and cooldown behavior. The proposal above establishes
the questions; it does not invent numeric tuning or an accepted contract.

### Q-4. Where do response sets live, and what is the final delivery boundary?

Resolve the extension relative to foundation D-11, select the tracker artifact
independently of PR delivery, and keep each eventual delivery reviewable.
The owner's requirement to finish this foreground work before opening a PR
does not itself authorize an epic, issue, or publication of unfinished work.

## Verification strategy

- Audition each exact clip and a representative sequence; record the owner's
  accepted files and any rejected variants. Check decoding, duration, level,
  silence boundaries, and provenance appropriate to the chosen source.
- Exercise the actual response-set selector: membership, no immediate repeat,
  cooldown shared across variants, and isolation from gameplay randomness.
- Drive the real selection paths and assert one accepted acknowledgment,
  suppression of canceled/non-player actions, and one group responder.
- Use the foundation's offline/null output to verify actual mixed PCM and
  prove headless/offscreen never opens a physical audio device.
- Confirm reset/teardown drops stale work and that ordinary selection behavior
  still passes its relevant existing targeted tests.
- Conduct owner hands-on acceptance in the game only through an explicitly
  authorized visible launch. The normal no-window repository rule remains
  in effect until that concrete action is agreed.
- Include the applicable native/FFI, capability, persistence, registration,
  packaging, and configuration gates for the implementation actually selected.

## Delivery plan

### USA-1. Prove brown-bear selection responses with an approved sound set

- **Outcome:** an accepted bear selection pilot and an authoring path for
  gradually adding other unit response sets.
- **Scope:** the three approved clips, response-set contract and implementation,
  real selection integration, focused tests, authoring guidance, and evidence.
- **Phase:** first content pilot after its audio prerequisites.
- **Depends on:** foundation playback prerequisites; exact AUD slice boundary
  must be resolved under Q-1 and Q-4 before this becomes implementation-ready.
- **Ordering:** critical path for the pilot; sound audition preparation can
  proceed independently of runtime development after Q-2 is answered.
- **Relevant decisions:** D-1 through D-3.
- **Acceptance signals:** the verification strategy above and explicit owner
  acceptance of actual sound and selection behavior before any PR opens.
- **Out of scope:** the complete roster, spoken-unit production, order/combat
  cues, music, new bear animation, and unapproved sound generation.
- **Open questions:** Q-1 through Q-4.

## Session

Foreground reservation: `20260910T142328Z-audio-unit-selection-cue-8bd04e`.
The reservation remains active while decisions and acceptance are pending.
