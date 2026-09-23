# Issue 2559 rereview and approval-policy update

Status: complete. The user approved the full revised specification, it was
published and exact-verified, and the canonical rereview returned APPROVE.
The backend applied reviewed:approve. The personal approval skill and its
launcher now defer unset art-supply decisions to solve time; all three
launcher tests passed. No artwork was generated and no Synarchy code changed.

## Canonical gate

- Skill: /Users/vincentcoghlan/.codex/plugins/cache/kanban/kanban/1.46.0/skills/issue-rereview/SKILL.md
- Trusted timeline helper: the same bundle's skills/solve/scripts/trusted_issue_spec.py.
- Backend resolved through exact install-record precedence:
  /Users/vincentcoghlan/Library/Application Support/kanban/issue-review/approve_issues.py.
  It and ~/work/approve-issues.py resolve to ~/work/kanban/tools/approve_issues.py.
  The backend source was not modified.
- Original check: CHANGES_REQUESTED, no pipeline incident.
- Final gate: APPROVE by claude-fable-5-1@high, selected by the canonical
  rereview backend, triggered by the prior Codex dissent. No blockers.
- Final review: https://github.com/coghex/synarchy/issues/2559#issuecomment-5627415374
- Final spec: 2861dffb09b72168a3223add32cfdb003bcd0dfe9bfa9aec7dde416d2a4e3d51
- Review: https://github.com/coghex/synarchy/issues/2559#issuecomment-5627234469
- Baseline spec: aa6a4a191122371fc3fd9bedeae24deaedbfe37caea8f38402e1f725c78320a7
- Title: [flora] Create the saguaro dead-sprout texture.
- Preserve non-verdict labels art, flora and exact issue-origin:claude marker.
- baseline_2559.json contains the original trusted payload.
- approved_2559.json contains the verified final trusted payload and review.
- draft_metadata.json records approved-method decision, draft hash, and paths.

## Completed workflow change

User expressly authorized updating the approval skill. Changed personal files:

- ~/.codex/skills/approve-issue/SKILL.md: missing artwork or an undecided
  supply/generation method alone is not a readiness blocker. The solver
  settles an unset method/authorization when the asset is needed. Preserve
  an existing owner choice; retain concrete asset requirements, dependencies,
  completed-art signoff, and the prohibition on placeholders/scope deletion.
- scripts/review_with_conflicting_origin_fallback.py: adds the owner-approved
  policy to the canonical review_prompt, because the backend does not read
  this skill file. Keeps canonical model calls, locks, comments, labels,
  and returned verdicts untouched; the exact dual-origin fallback remains.
- scripts/test_review_with_conflicting_origin_fallback.py: 3 passing
  subprocess boundary tests with a fake backend (no model calls or GitHub
  mutations), checking policy delivery, exact data/argument forwarding,
  unchanged negative verdicts, exact origin fallback, and non-dual behavior.

Also imported the real backend and rendered initial/rereview prompts without
calling a model: both include the new policy while preserving canonical
dossier, dependency, and untrusted-content instructions. This change applies
to calls through the personal approve-issue launcher; installed canonical
backend source and plugin skills were not changed. Current #2559 rereview
uses its required direct backend route, with PixelLab explicitly in the body.

## Verified issue corrections

- Reviewed against master 36090b94a39a773f65a7e9dc236efd971b38269d. No relevant
  change since review base 65ad8231916a in asset_generation.md,
  engine_contracts.md, Preview/Discovery.hs, or the saguaro images.
- sprout.png: 48x48 RGBA, alpha bounds (18,5,30,47), last occupied row 46.
  Directory has only dead, matured, matured_flowering, matured_fruiting,
  sprout PNGs; no sprout_dead.png.
- Flora preview discovers files directly via Preview/Discovery.hs;
  temporary YAML declarations are unnecessary and removed from the draft.
- asset_generation.md flora steps 5-6 require color-shifted band restore
  preserving lower-band alpha silhouette. The solver chooses/documents its
  boundary from the existing reference before generation. Above-band slump
  or narrowing remains allowed; ground contact stays row 46.
- Draft adds a small evidence receipt with lower_band_start_row and source
  SHA-256, a concrete alpha-mask check, PixelLab provenance, and existing
  native/enlarged plus real-preview owner signoff. No new images beyond the
  single dead sprout; integration remains #2562 and charred art #2596.
- Acceptance Python snippet parses successfully. It was not executed against
  missing artwork. Primary checkout is clean.

## Final review clarifications

The trusted approval comment adds implementation requirements without new
owner decisions: match the reference's alpha convention (binary unless the
reference uses intermediate values); record the delivered image SHA-256 and
concrete generation inputs in the evidence file; and choose a non-vacuous
lower-band boundary inside reference alpha rows 5–46 by silhouette shape.
These additions are part of the canonical implementation specification.

## Next

This rereview is complete. Issues 2561, 2562, and 2563 remain unprocessed
from the prior approval batch; do not resume them as part of this rereview.
