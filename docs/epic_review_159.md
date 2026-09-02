# Epic Review Findings: Epic #159 — Locations feature

This report records the completed-arc review of epic #159 at
`master@5dc077ffd130`, against its reconciled 14-child scope. The epic has no
native GitHub sub-issues; its body declares foundation children #88, #89, #90,
#91, #414, #422, and #424, embark-and-discovery children #777, #778, #779,
#780, #781, and #782, and instance-identity child #911. The runtime arc is
coherent and its focused current gates pass; one new steering-document mistake
remains.

Status legend: `[ ]` unprocessed · `[#N]` filed as issue N · `[no-issue]`
reviewed and deliberately never to be filed · `[deferred]` blocked on a
concrete precondition

## Status

- [x] ER-1. The expedition design presents superseded location behavior as current — [#2183]

## 1. Location-arc handoff documentation

### [#2183] ER-1. The expedition design presents superseded location behavior as current

> **Captured note:** Correct `docs/expedition_gameplay_loop.md`'s as-built
> handoff for epic #159 while retaining its history: #1230 replaced #780's
> proximity halo and #781's paired icons, #1101 replaced #911's placeholder
> display names, and #916 now drives the instance lifecycle into `active` and
> `cleared`.

**Verification:** Verified. The document contains accurate later sections and
supersession notes for sight-based reveal and shared-unknown/type/dark-type
icons, but its section explicitly titled `Implementation status` still says
the old terminology “matches what shipped” and describes the removed
`discovery_margin` and icon pair as the live renderer contract. The same
current-status passage says no gameplay drives lifecycle states past
`discovered` and that display names still come from the definition label.
Current code instead uses sight, maps all six lifecycle states to the new icon
model, generates write-once names in the world's language, and clears
death-only encounters. The document therefore contradicts both the tree and
its own later design decisions.

**Evidence:**

- `docs/expedition_gameplay_loop.md:393` — the current `Implementation status`
  section lists paired icons, then lines 406–410 state that the removed
  expanded-bounds approach margin and icon pair are what the runtime renders.
- `docs/expedition_gameplay_loop.md:424` — the #911 handoff says nothing drives
  a location past `discovered`; `World.Thread.Discovery` now promotes and
  reports completed encounters.
- `docs/expedition_gameplay_loop.md:445` — the handoff still calls display names
  label-derived placeholders and leaves generated-language wiring as future
  work.
- `src/Location/Discovery.hs:13` — the live discovery contract records #1230's
  replacement of the halo with night-, facing-, and terrain-aware sight.
- `src/World/Render/Zoom/Icons.hs:74` — the renderer explicitly maps all six
  lifecycle states to shared unknown, normal type, or darkened type appearance.
- `src/Location/Instance.hs:41` — current instances receive write-once names in
  the world's generated language under #1101, falling back to `ldLabel` only
  when no language provenance exists.
- `src/World/Thread/Discovery.hs:105` — current encounter completion marks a
  location cleared and emits the corresponding event.

**Handoff context:**

- **Current behavior:** Runtime behavior and its focused tests are correct, but
  the durable expedition design offers mutually incompatible descriptions of
  the current location architecture.
- **Expected behavior:** Preserve #780/#781/#911's original delivered behavior
  as labeled history, while making the as-built handoff name #1230's sight and
  icon replacements, #1101's generated names, and #916's lifecycle consumer.
- **Scope and constraints:** Documentation-only correction. Do not change
  discovery, icon rendering, naming, encounters, persistence, the append-only
  lifecycle enum, or the still-open #917 significant-loot contract. Do not
  erase the historical account of what epic #159 originally shipped.
- **Verification target:** The `Implementation status` and #911 handoff no
  longer claim removed fields or superseded behavior are current; links to
  #780/#781 remain as historical provenance and current statements agree with
  `Location.Discovery`, `World.Render.Zoom.Icons`, `Location.Instance`, and
  `World.Thread.Discovery`.
- **Deduplication:** All-state tracker searches for the exact placeholder-name,
  paired-icon, approach-margin, and “nothing drives” claims found only the
  closed source features #1230/#1101/#916 and open follow-on #917, none of
  which owns this remaining documentation correction. The docs-worktree report
  corpus contains CH-137's older clean assessment of the design document, but
  no pending finding for this post-assessment drift.
- **Remaining uncertainty:** None.
