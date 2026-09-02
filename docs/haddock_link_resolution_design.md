# Haddock link resolution design

A haddock link of the form `'Module.function'` is the tree's cross-module
navigation: it names the function that actually does the thing a comment is
explaining. On 2026-09-02, 107 such links across 77 files under `src/` and
`app/` name a function the named module does not export, so they render as
plain text, cannot be jumped to, and send a reader to a module that hides the
symbol. The count was 67 when `docs/explore_report.md` EXPL-27 recorded it
two weeks earlier; it grows every time a module is split and the extracted
function is documented from outside. This arc adds the guard that makes a
haddock link resolve, and drains the existing debt behind it. It benefits
anyone reading a comment to find the code it describes, which is every
maintainer and every agent working in this tree.

Design state: `ready for issue processing`

Status legend: `[ ]` unprocessed · `[#N]` linked to issue N · `[no-issue]`
reviewed and deliberately not tracked separately · `[deferred]` blocked on a
concrete precondition

## Processing status

- [ ] EPIC. Make every cross-module haddock link resolve, and keep it so
- [ ] HLR-1. Add the haddock link audit with a checked-in baseline of today's dead links
- [ ] HLR-2. Drain the baseline across the save/load and scripting seam
- [ ] HLR-3. Drain the baseline across input dispatch and UI
- [ ] HLR-4. Drain the baseline across world generation, hydrology, geology and language
- [ ] HLR-5. Drain the remainder and delete the baseline

## Epic contract

- **Goal:** every `'Module.function'` haddock link in `src/` and `app/`
  names a function the named module exports, and a gate in CI and `make ci`
  fails the moment a new one does not.
- **Done when:** `tools/haddock_link_audit.py` runs in both gate sets with no
  baseline file, reports zero dead links on master, and its self-test proves
  it fires on a synthetic dead link.
- **Users and operators:** maintainers and agents reading comments; whoever
  splits a module next, who now gets told at CI time that the sibling's
  "reached only through `'X.f'`" no longer resolves.
- **Arc label:** `tech-debt` (existing); `documentation` on the sweep
  children.

## Current state and evidence

- **The sweep.** A 60-line detector (`haddock_links.py`, kept beside the
  processing scratch for EXPL-27) resolves every `'Module.function'` inside a
  comment under `src/` and `app/` against the named module's export list. A
  reference counts as dead only when: the module exists in this tree; it has
  an explicit export list (27 of 814 modules have none and export everything);
  the symbol is absent from that list; and the symbol is a real top-level
  function somewhere in the tree (it has a `name ∷` signature at column 0).
  The last rule is what excludes the Lua-binding false positives EXPL-27
  names (`'UI.setVisible'`, `'UI.setClickable'` and friends resolve as module
  references only because `src/UI.hs` exists) and record fields reached
  through `Type(..)`.
- **Result on 2026-09-02:** 107 dead links in 77 files. EXPL-27 counted 67
  (a floor it later corrected from 59). Two of the new hits were re-verified
  by hand: `'Engine.Asset.YamlItems.requirePositiveQuantity'` and
  `'Item.Types.defaultQualityTiers'` are both defined and both unexported.
- **They cluster on module-split seams.** The most-cited dead targets are
  `Engine.Scripting.Lua.Thread.Dispatch.handleLoadStaged` (8 sites),
  `World.Save.Integrity.luaEdgeResolves` (5),
  `World.Fluid.River.Identify.traceRivers` (4),
  `UI.InputOwnership.pagesInScope` (4), and
  `Engine.Input.Thread.Dispatch.dispatchInput` (4). All four of
  `Engine.Input.Thread.Keyboard`, `.Char`, `.Mouse` and `.Scroll` open by
  saying they are "reached only through `'Engine.Input.Thread.Dispatch.dispatchInput'`";
  #787 moved the router into a module that exports only its two public entry
  points. The export-narrowing issues #1083, #1154 and #1156 created most of
  the rest deliberately, and nothing told the comments.
- **One is same-module.** `src/World/Save/Storage.hs` links
  `'World.Save.Storage.publishValidated'`, which its own export list omits.
- **Nothing catches it.** `synarchy.cabal` passes `-haddock` to GHC, which
  only makes the parser accept and validate comment SYNTAX; link targets are
  resolved solely by the `haddock` tool, which no gate runs (`ci.yml`,
  `tools/ci-local.sh`, `Makefile` never invoke it). None of the existing
  audits inspect link targets.
- **Link forms in the tree** (2026-09-02): 1240 qualified `'M.f'` links,
  843 module links `"M.N"`, 4053 unqualified `'f'` links, and 79 code spans of
  the form `@M.f@`.
- **The precedent for the fix shape.** PR #1407 (#1083) rewrote 16 files'
  links to unexported names from `'x'` to monospace `@x@` when it narrowed
  those export lists; `docs/explore_report.md` EXPL-6 was closed as
  `[no-issue]` on exactly that ground. So the tree already treats "a code span
  names a real definition without claiming a link" as the accepted spelling.
- **The gate-set conventions this must follow.** Every audit under `tools/`
  ships with a self-test (`tools/test_<name>.py` or a `--self-test` flag),
  runs on both sides of `make ci` and `ci.yml` (`tools/ci_parity_audit.py`
  fails on any one-sided invocation without a reason-carrying exemption), and
  is listed in `docs/engine_contracts.md` §The `make ci` gate set.
  `tools/unicode_operator_audit.py` (437 lines, 533 of self-test) is the
  nearest shape: a comment/string-aware scan of `src/` + `app/` with a short
  explicit exemption list. `tools/enum_append_only_audit.py` is the ratchet
  shape: a checked-in generated baseline (`docs/save_compat/enum_baseline.json`)
  that a run compares against and that only an explicit `--update-baseline`
  may rewrite.
- **Tracker.** No open or closed issue proposes a haddock link audit. The
  related findings are `docs/explore_report.md` EXPL-6 (`[no-issue]`),
  EXPL-24 (`[deferred]` behind this arc; its four `pagesInScope` sites are in
  the list), and EXPL-39 (`World.Geology.Ore`'s `'World.Geology.Timeline.buildAge'`
  link, which carries a separate wrong-caller narrative and is in the list).
  The closed CH items on haddocks (CH-4, CH-20, CH-68, CH-86, CH-120) each fixed
  one stale comment and none added a guard.

## Desired experience

A maintainer reading `-- reached only through 'Engine.Input.Thread.Dispatch.dispatchInput'`
can follow that link to a definition, or sees `@Engine.Input.Thread.Dispatch.dispatchInput@`
and knows at a glance that the name is informational and not exported. A
maintainer who splits a module and leaves such a link behind gets a CI
failure naming the file, line, link, and the module that does not export the
symbol, in the same run that would otherwise have merged the split. Nobody
widens an export list to make a link resolve.

## Scope

### In scope

- A new audit, `tools/haddock_link_audit.py`, with a self-test, wired into
  `ci.yml` and `tools/ci-local.sh`, registered with `ci_parity_audit.py`, and
  listed in `docs/engine_contracts.md` §The `make ci` gate set and
  `tools/README.md`.
- Qualified cross-module links `'Module.function'` in every `--` and `{- -}`
  comment under `src/` and `app/`, including a module linking its own
  unexported name.
- A checked-in baseline of the dead links present when the audit lands, so
  the audit lands green as a ratchet, and the sweeps that drain it to empty.
- Fixing each dead link by demoting it to a code span or by pointing at the
  exported entry point, per D-2.

### Out of scope

- Building haddock documentation, or running the `haddock` tool, in any gate.
- Widening any export list. #1083, #1154 and #1156 narrowed them on purpose.
- Unqualified `'f'` links (4053) and module links `"M.N"` (843), per D-1.
- Rewriting the prose around a dead link. A sweep changes the link's spelling
  or target and nothing else; the narrative corrections `docs/explore_report.md`
  records separately (EXPL-24's `scopedPageOk` wording, EXPL-39's caller
  claim) stay with their own findings.
- `test-headless/`, `test/`, `cbits/`, and Lua.

## Design

**Detection rule.** A qualified link `'M.f'` inside a comment is dead when M
is a module in `src/` or `app/`, M has an explicit export list, that list
neither names `f` nor carries a `module X` re-export that could supply it,
and `f` is a top-level function defined somewhere in `src/` or `app/`. A
module without an export list exports everything and is skipped. A link to a
record field reached through `Type(..)`, to a Lua binding name, or to a module
outside the tree is not a candidate. The rule is stated in the tool's
docstring and every clause has a self-test fixture in both directions.

**Comment awareness.** The scan reads only comment text. The existing
comment/string-aware line scan in `tools/engine_env_capability_common.py`
and `tools/unicode_operator_audit.py` is the shape to reuse, so a `'M.f'`
inside a string literal or GLSL quasiquote is never a candidate.

**Baseline ratchet (HLR-1).** The audit compares its findings to a checked-in
baseline file listing dead links as `path`, `link`; a finding not in the
baseline fails, and a baseline entry no longer found is reported as stale
and fails too, so the file can only shrink. `--update-baseline` regenerates
it. The baseline is generated by the tool, never hand-edited, exactly like
`docs/save_compat/enum_baseline.json`. When the last sweep lands, the baseline
is deleted and the audit's baseline branch becomes unreachable (HLR-5 removes
the flag as well, so the tool cannot quietly grow a new allowlist).

**Fix policy (D-2).** For each dead link, the sweep chooses one of two
spellings and never a third:

1. **Demote to a code span** `@M.f@`, the default, as PR #1407 did. The name
   stays greppable and is visibly not a link. A same-module link to an
   unexported name becomes `@f@`.
2. **Point at the exported entry point** when an exported function reaches
   the same code, for example a sibling that says it is "reached only through
   `'X.dispatchInput'`" pointing at `'Engine.Input.Thread.processInput'`
   instead. Optional, never required.

Exporting `f` to satisfy the link is never chosen.

**Failure output.** One line per dead link: `path:line: 'M.f' — M does not
export f (defined in N)`, with the defining module named so the fix is
mechanical.

**Cost.** The detector reads ~814 files once and runs in well under a second
with no engine, so it is unconditional on both gate sides, like the texture
path check.

## Decisions

### D-1. The audit covers qualified `'Module.function'` links only

Owner decision, 2026-09-02. Module links `"M.N"` (843 in the tree) and
unqualified `'f'` links (4053) are out of scope for this arc. Consequence:
HLR-1's detection rule is exactly the one under **Design**, and the sweep
sizes in HLR-2 to HLR-5 are final for the current tree. Adding either other
class later is a new arc, not a widening of this one.

### D-2. A dead link is demoted to a code span by default, re-pointed only where an exported function reaches the same code, and never fixed by exporting

Owner decision, 2026-09-02. Each dead `'M.f'` becomes `@M.f@` (a same-module
one becomes `@f@`), which renders as monospace, is visibly not a link, and
keeps the exact name greppable; PR #1407 established the spelling. A sweep
author MAY instead re-point the link at an exported function that reaches the
same code (a sibling "reached only through `'X.dispatchInput'`" may point at
`'Engine.Input.Thread.processInput'`), but is never required to. Widening an
export list to satisfy a link is forbidden: #1083, #1154 and #1156 narrowed
those lists deliberately. Consequences: every sweep PR is a comment-only
edit a reviewer can check mechanically; no export list changes anywhere in
the arc; the audit's self-test does not need to model re-pointing, only
resolution. Rejected: "demote always" (loses a working link where one is
obvious) and "export when the function is the real subject" (reopens the
narrowing).

### D-3. The audit lands first as a ratchet over a generated baseline; four sweeps drain it; the last deletes it

Owner decision, 2026-09-02. HLR-1 ships the audit with a checked-in,
tool-generated baseline of every dead link present at that commit, so it
passes immediately and fails only on a link not in the baseline or a
baseline entry no longer found. HLR-2, HLR-3 and HLR-4 each delete their
entries and can land in any order; HLR-5 fixes the remainder, deletes the
baseline file, and removes `--update-baseline`, so the tool cannot quietly
grow a new allowlist. Consequences: the guard is live after one PR, which
matters because the count grew from 67 to 107 in two weeks; each sweep is a
12-to-23-file comment edit; the baseline compare is throwaway code that
exists only between HLR-1 and HLR-5. Rejected: one audit-plus-sweep PR across
77 files (a single large review, and conflicts with any in-flight PR touching
those comments).

## Open questions

### Q-1. Does the audit cover only qualified `'M.f'` links, or also module links and unqualified links?

Resolved by D-1: qualified links only.

### Q-2. Is the fix policy (point at the exported entry point, else demote to a code span; never export) the decision?

Resolved by D-2: demote by default, re-point where an exported function
reaches the same code, never export.

### Q-3. Ratchet with a checked-in baseline first, then sweeps, or one audit-plus-sweep PR?

Resolved by D-3: ratchet first, four sweeps, baseline deleted by the last.

## Verification strategy

- **The audit's own self-test** proves each clause of the detection rule
  fires and does not fire on fixtures: an exported target, an unexported
  target, a `module X` re-export, a `Type(..)` field, a module with no export
  list, a link inside a string literal, a link to a Lua binding name, and a
  same-module unexported link.
- **Gate parity:** `python3 tools/ci_parity_audit.py` passes with the new
  invocations on both sides.
- **Per sweep:** the audit's dead-link count drops by exactly the sites the
  sweep claims, the baseline diff in the PR is deletions only, and
  `cabal build all synarchy-test-headless` stays warning-clean (a demoted
  link is a comment edit and cannot change a warning; a re-pointed link is
  too).
- **Arc end:** the baseline file is gone, the audit runs without it, and a
  synthetic dead link added in the self-test fails the run.
- No headless suite, world check, or probe is affected: the arc changes
  comments and tooling only.

## Delivery plan

### HLR-1. Add the haddock link audit with a checked-in baseline of today's dead links

- **Outcome:** `tools/haddock_link_audit.py` and `tools/test_haddock_link_audit.py`
  exist, run in `ci.yml` and `tools/ci-local.sh`, pass `ci_parity_audit.py`,
  and land green because a generated baseline lists every dead link present
  at that commit. `docs/engine_contracts.md` §The `make ci` gate set and
  `tools/README.md` list the new member.
- **Scope:** the detection rule, comment awareness, the baseline compare and
  `--update-baseline`, failure output, self-test, gate wiring, and docs.
- **Phase:** 1
- **Depends on:** `none`
- **Ordering:** `critical path`
- **Relevant decisions:** D-1, D-3
- **Acceptance signals:** the audit reports zero new dead links on master
  and lists the baseline's count; the self-test fails on a synthetic dead
  link and on a stale baseline entry; parity passes.
- **Out of scope:** fixing any link.
- **Open questions:** `None`

### HLR-2. Drain the baseline across the save/load and scripting seam

- **Outcome:** the 32 dead links in 22 files under `src/Engine/Scripting`,
  `src/World/Save`, `src/World/Load`, `src/World/Thread` and
  `src/World/Command` are re-pointed or demoted per D-1; the baseline shrinks
  by exactly those entries.
- **Scope:** the `handleLoadStaged` (8 sites tree-wide, 5 here),
  `luaEdgeResolves` (5), `continueLoad`, `processAuthorizedSave`,
  `saveOwnerSet` and neighbouring links.
- **Phase:** 2
- **Depends on:** HLR-1
- **Ordering:** `independent` of HLR-3 and HLR-4
- **Relevant decisions:** D-1
- **Acceptance signals:** audit passes with the baseline reduced by these
  entries only; warning-clean build.
- **Out of scope:** any prose change beyond the link's spelling or target.
- **Open questions:** `None`

### HLR-3. Drain the baseline across input dispatch and UI

- **Outcome:** the 19 dead links in 12 files under `src/Engine/Input` and
  `src/UI` are fixed, including the four `pagesInScope` sites that
  `docs/explore_report.md` EXPL-24 records.
- **Scope:** `dispatchInput` (4), `pagesInScope` (4), `inputBoundaryPage` (3),
  `absolutePosition` (2) and the rest of that area.
- **Phase:** 2
- **Depends on:** HLR-1
- **Ordering:** `independent`
- **Relevant decisions:** D-2, D-3
- **Acceptance signals:** as HLR-2, for this area.
- **Out of scope:** EXPL-24's `scopedPageOk` wording point, which is not a
  link fix.
- **Open questions:** `None`

### HLR-4. Drain the baseline across world generation, hydrology, geology and language

- **Outcome:** the 27 dead links in 23 files under the rest of `src/World`,
  `src/Sim` and `src/Language` are fixed, including `traceRivers` (4) and the
  `buildAge` link `docs/explore_report.md` EXPL-39 records.
- **Scope:** the `World.Fluid.River.Identify.*` split, `World.Slope`,
  `World.Plate`, `World.Geology`, `Sim.Fluid`, and `Language.Generated`.
- **Phase:** 2
- **Depends on:** HLR-1
- **Ordering:** `independent`
- **Relevant decisions:** D-2, D-3
- **Acceptance signals:** as HLR-2, for this area.
- **Out of scope:** EXPL-39's caller-name correction beyond making the link
  resolve.
- **Open questions:** `None`

### HLR-5. Drain the remainder and delete the baseline

- **Outcome:** the remaining 29 dead links in 20 files (`src/Engine/Core`,
  `src/Engine/Graphics`, `src/Engine/Asset`, `src/Engine/PlayerEvent`,
  `src/Building`, `src/Location`, `src/Item`, `src/Unit`, `app/`) are fixed,
  the baseline file is deleted, `--update-baseline` is removed, and the audit
  fails on any dead link.
- **Scope:** the last sweep plus the ratchet's retirement.
- **Phase:** 3
- **Depends on:** HLR-2, HLR-3, HLR-4
- **Ordering:** `critical path`
- **Relevant decisions:** D-1
- **Acceptance signals:** audit passes with no baseline; the self-test's
  baseline fixtures are removed; the arc's done condition holds.
- **Out of scope:** any new link class (Q-1).
- **Open questions:** `None`
