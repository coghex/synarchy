# Issue 2563 rereview

Status: complete. The user approved the full revised specification. It was
published and exact-verified, and the canonical rereview returned APPROVE.
The backend applied reviewed:approve; the final trusted remote state matches
the approved title/body, non-verdict labels, and expected review marker.
No repository implementation changes were made.

Final review: https://github.com/coghex/synarchy/issues/2563#issuecomment-5627665936
Final spec: 57b9ace6610e700af0d7682ee77830e1cc874bdbdbe2b7010939aa680b20f18b
Route: prior Codex dissent -> Claude Fable 5.1 high.
Labels: enhancement, lua, units, reviewed:approve. Remaining blockers: none.

## Approved owner decision

The user answered "recommendation approved": the first successful report
delivered to a living teammate makes the whole minted group hostile,
including unreached members on other pages. Incident details remain limited
to observed/reached units. This resolves the shared-tag/per-recipient
contradiction; no new knowledge-aware relation authority is requested.
The complete subsequent body was separately approved by the user with "approved".

## Verified feedback and draft requirements

- src/Unit/Faction/Profile.hs:433 relationFromTo has only policy and two
  profiles as inputs. Live causes for matching ordered tag pairs outrank
  shared controllers/tags and the base table. It has no page or incident
  knowledge input. The cause-store prerequisite connects this authority.
- Design D-18 defines successful communication installing a team-wide
  reverse cause as escalation; Local awareness and team knowledge separates
  knowledge and relation. D-37 fixes report source to a minted roster or
  occupant group, not culture. The implementation PR must align this design
  prose with the owner's chosen scope; no separate later docs landing.
- unit.getLastAttacker returns only the latest attacker uid and timestamp,
  without expiry/consumption (API/Units/Combat.hs:423). Stable shared incident
  identity must distinguish victims/incidents and deduplicate repeated
  polling and reports across victim/witnesses and save/load. Keep historical
  identity/captured tags independent of live entity-reference resolution.
- scripts/CLAUDE.md:46 and unit_ai_encounter.lua require page qualification.
  Witnessing and walking delivery must use the acting unit's page and
  revalidate the recipient before transfer. Equal coordinates on distinct
  pages cannot grant visibility or proximity.
- unit_ai_notify.lua has delayed broadcasting and rank-split walk/transfer.
  Current water recipients are definition acolyte and must stay unchanged.
  Incident recipients use the explicit shared minted group tag; at least
  one eligible living recipient must actually receive the incident before
  installing reverse hostility. Cover no recipients, disappearing/dying
  recipients, and sender death during walking, transfer, or broadcasting.
- Persist incident history, reported state, pending phase and timing in the
  existing unit_ai Lua component. Preserve all supported older payloads with
  empty incident state; add a tracked version fixture and validate new fields.
  Existing state is replaced per entity on load and cleared on session exit.
- unit_ai_ref_schema.lua is the shared authority for all reference traversals:
  wire wrapping/unwrapping, references(), validation, and post-load reconcile.
  New resolvable references must participate. Scrubbing a stale attacker or
  delivery target must not rewrite incident identity/tags or erase a valid
  pending report. Historical identity is durable data, not a live pointer.
- Required docs accompany the implementation PR: faction design/contract
  clarification, persistence inventory classification and evidence/fixtures.
- Keep Haskell last-attacker fields runtime-only and preserve the ten-second
  retaliation, in-combat swap and lash-out; do not drain UI combat logs to
  source gameplay incidents (engine contracts Logging streams / persistence
  contract notification-only event queues).

## Completed draft

2563_body.md contains the full proposed body. Title and non-verdict labels
are preserved, as is the exact issue-origin:claude marker. It incorporates
the approved group-wide policy, stable shared incident identity, same-page
witness/delivery checks, actual living-recipient delivery, persistence and
reference reconciliation, and required documentation in the implementation
PR. Adds focused fresh-process save/load evidence because headless codec
tests alone cannot prove engine.saveWorld/load transaction behavior.
All existing acceptance tool paths were verified, and the shell acceptance
block passes bash -n. No implementation tests or probes were run for this
specification-only task. The proposed incident_reports probe and group are
to be added by the solver; Attack order transaction is supplied by #2558.

## Final canonical clarifications

The trusted approval comment adds requirements without new owner decisions:
- Reporters lacking a minted group tag create no pending report/cause;
  existing local retaliation stays unchanged.
- Walk rank pools are scoped to the same incident and reporter group,
  separate from water and other-incident reporters.
- No-recipient incidents remain pending without locking a reporter in an
  indefinite notify standstill or repeatedly broadcasting to an empty pool.
- Historical identity uses an explicitly classified retained typed reference
  or opaque identity; record the selected mechanism in the inventory.
- Witnesses use the existing real per-unit line-of-sight authority.
- The unwitnessed-kill fixture keeps survivors unable to see the victim even
  after death, so later corpse witnessing cannot invalidate its premise.

These additions belong to the canonical implementation specification; the
published body need not be edited again. approved_2563.json retains the full
trusted final review. published_2563.json retains the exact-verified update.

## Next

This issue rereview is complete. No further action is pending in this task.
