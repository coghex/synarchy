# Epic Review Findings: Epic #768 — Rebuild save/load around coherent snapshots and versioned components

This report records the current-HEAD review of epic #768 at
`dc9721904ed9abc7aae91c9fe60155c6d0e58a4b`. The epic declares eleven
implementation children, #756–#764, #766, and #767; all eleven are closed as
completed, and every implementation pull request received a final approval and
merged. The resulting architecture remains coherent at current HEAD: capture is
owner-acknowledged and immutable, the envelope and component schemas are
independently versioned, publication and whole-session load are transactional,
typed references share one integrity graph, compatibility fixtures are audited,
and repeated fresh-process cycles preserve the canonical session. The explicit
post-epic seam #900 has also landed. One new current steering mistake survives:
the closed epic's live body leaves every child unchecked and repeatedly assigns
the component envelope and compatibility baseline to historical version 83,
which was never that format.

Status legend: `[ ]` unprocessed · `[#N]` filed as issue N · `[no-issue]`
reviewed and deliberately never to be filed · `[deferred]` blocked on a
concrete precondition

## Status

- [x] ER-1. Epic #768 leaves its completed roadmap unchecked and pins compatibility to the wrong version — [#768]

## 1. Epic closure and compatibility steering

### [#768] ER-1. Epic #768 leaves its completed roadmap unchecked and pins compatibility to the wrong version

> **Captured note:** Epic #768 is closed and its owner closure comment confirms
> that all eleven children landed, but every child remains unchecked and the
> body still calls v83 the first component-envelope generation and compatibility
> baseline.

**Verification:** The contradiction is confined to the live tracker body. The
owner's closure comment enumerates all eleven completed children and identifies
#900 as a separate follow-on rather than unfinished epic scope. The current
contract and implementation agree with the completed children: B1 introduced
an envelope framing version of 1, component schemas evolve independently, and
the actual frozen B1 complete-session shape is migrated as `SessionV90`.
Historical v83 belongs to #785 and was not reassigned to the envelope. The
focused audits, Hspec selections, and four-generation fresh-process probe all
pass, so this is not evidence of a surviving implementation defect.

**Evidence:**

- [Epic #768's live body](https://github.com/coghex/synarchy/issues/768) — all
  eleven child boxes are unchecked, while the compatibility policy, B1 child,
  execution plan, testing strategy, and done criteria repeatedly describe the
  new format or baseline as v83.
- [The owner's closure comment](https://github.com/coghex/synarchy/issues/768#issuecomment-5076066633)
  — says every Phase A–D child landed and records #900 as the one deliberate,
  separately tracked seam.
- [Follow-on #900](https://github.com/coghex/synarchy/issues/900) — is now closed,
  confirming that the explicitly separated adapter work also has a durable
  tracker disposition.
- `docs/persistence_contract.md:277` — the authoritative format policy separates
  envelope framing, component schema versions, and the transitional global
  bridge version.
- `docs/persistence_contract.md:289` — preserves v83's real historical meaning
  as #785 rather than reassigning it to the save envelope.
- `docs/persistence_contract.md:296` — identifies the first completed B1 format
  as the compatibility baseline and its frozen complete-session migration as
  `World.Save.Compat.SessionV90`.
- `src/World/Save/Envelope.hs:37` and `src/World/Save/Envelope.hs:104` — define
  the independent envelope-framing contract; `currentEnvelopeVersion` remains
  B1's assigned value, 1.
- Current focused checks: the persistence inventory audit passes with 204
  root-owner fields, three Lua modules, eight typed-reference fields, and ten
  Lua reference kinds classified; its 168 self-test groups pass; the save
  compatibility audit passes with 21 baselines and 27 fixtures; its cheap-mode
  self-test and the CI-probe selector self-test pass; eight focused Hspec
  selections pass. After the freshness delta changed restored-page staging, a
  warning-clean headless rebuild plus the 26-example `canonical chunk identity`,
  8-example `persistence contract`, and 18-example `transactional load` groups
  also pass. `tools/persistence_contract_probe.py` completes three
  fresh-process save→load→save cycles with four structurally identical
  generations and zero failed checks.

**Handoff context:**

- **Current behavior:** A reader opening the closed epic sees an unfinished
  checklist and a specific compatibility-version policy that disagree with the
  issue state, closure comment, completed children, authoritative repository
  contract, frozen fixtures, and live codec.
- **Expected behavior:** Mark all eleven children complete and replace the v83
  claims with format-relative wording: compatibility begins at the actual B1
  tracked baseline, envelope framing is independently versioned (currently 1),
  and each gameplay component owns its schema version. Link the authoritative
  contract rather than copying another volatile global bridge version into the
  tracker.
- **Scope and constraints:** Tracker-body-only correction to epic #768. Do not
  change the codec, compatibility fixtures, current contract, historical version
  meanings, or child issues merely to make the stale prose true.
- **Verification target:** The closed epic marks #756–#764, #766, and #767
  complete; no longer labels the envelope or its baseline v83; and agrees with
  its closure comment and `docs/persistence_contract.md` §5.
- **Deduplication:** All-state tracker searches for #768's unchecked checklist,
  v83 envelope, and v83 baseline found no open corrective issue. The
  docs-worktree report corpus has no pending owner for this tracker correction.
  Open bug #2055 owns a distinct migrated unit-AI runtime-default failure and is
  not duplicated here; later save/load feature and structural-debt issues also
  do not own #768's stale body.
- **Remaining uncertainty:** None about the contradiction. The only editorial
  choice is how much historical pre-B1 context to retain after the volatile v83
  labels are removed.
