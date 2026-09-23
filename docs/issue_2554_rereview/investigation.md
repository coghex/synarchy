# Issue 2554 rereview investigation

Status: complete. User approved the full revision, publication was exact-verified, and canonical Claude Fable 5.1 high rereview returned APPROVE. The backend applied reviewed:approve. Final spec: fb29c8fdcb55545f5ee7f89d86d268fd60f6889fa3c6cc1d07c78dc52da36af7. Review: https://github.com/coghex/synarchy/issues/2554#issuecomment-5626778255. Published body, non-verdict labels, and canonical marker verified.

Skill: /Users/vincentcoghlan/.codex/plugins/cache/kanban/kanban/1.46.0/skills/issue-rereview/SKILL.md (already read in this conversation; currently installed, former 1.45.0 catalog path was stale).
Trusted helper (only permitted timeline source): /Users/vincentcoghlan/.codex/plugins/cache/kanban/kanban/1.46.0/skills/solve/scripts/trusted_issue_spec.py
Canonical backend resolved by the skill's install-record precedence: /Users/vincentcoghlan/Library/Application Support/kanban/issue-review/approve_issues.py
Gate: CHANGES_REQUESTED, no pipeline incident, next canonical reviewer claude-fable-5-1@high; do not pin models for rereview.
Spec: 53697c5ba0844b9cbd908106ab3eb0ca4c590f152e573c80a4f0d297dc88d4cb
Review: https://github.com/coghex/synarchy/issues/2554#issuecomment-5626088850
Title: Spawn with controllers, extra tags, and minted roster team tags
Labels: enhancement, locations, units, reviewed:changes. Preserve non-verdict labels and exact issue-origin:claude marker.

Canonical review decisions:
1. Cross-location nomads: owner accepted preserving shared-tag alliance. Replace requirement 4's neutral claim, which contradicts unchanged shared-tag authority. Preserve existing live-cause precedence, and test distinct report scopes despite the alliance.
2. Explicit player/debug overrides: preserve D-26 controller/capability mapping or intentionally force affiliation-only uncontrolled location spawns. Current unconditional no-controller wording contradicts player mapping and unchanged commandability/ownership promise. Existing both location helpers forward the faction string unchanged.

Verified evidence:
- Unit.Faction.Profile.relationFromTo: live directed causes first; then common non-null controller; then any shared tag -> ally; then base relations; otherwise neutral. Tests at test-headless/Test/Headless/Unit/FactionProfile.hs:335-344 prove shared tags outrank base/default. No separate culture/team precedence exists.
- docs/faction_tag_system_design.md D-29: acolyte/technomule default acolyte; nomad_primitive default nomad; animals wildlife. D-28 base table makes acolyte/nomad hostile and nomad/wildlife neutral. D-37 distinct group tags keep report scopes distinct even when cultures ally.
- Location.Instance header explicitly defines durable identity as (WorldPageId, LocationInstanceId); integer allocators restart at 1 per page. Mint occupant TAG and OWNER identities deterministically and collision-free from both components; include same integer on different pages in tests.
- scripts/locations.lua spawnUnitContent at ~448 and spawnEncounterUnitContent at ~495 independently default faction to hostile. ruin_small.yaml uses count_range and reaches the encounter helper via dedicated pass around 745. That path durably registers each successful roster prefix, retains exact ids/homes on retry, allocates only missing entries, and preserves original authored indices for loot draws. Remove only ruin_small's faction field, never reorder contents.
- The existing encounter Lua fixture stubs faction.relation via REL table. It can prove AI behavior, not catalogue/profile relation answers. Add production-authority coverage including different authored affiliations sharing one occupant group to isolate group-alliance behavior.
- Spawn.hs preserves faction slots 4/5, explicit nil Z, fixed page slot 6 and bindGen slot 7. Binding staleness is checked from the same WorldManager read as page resolution before ID allocation/queueing, returning nil plus page binding stale; ordinary unbound failures remain -1. Include malformed binding and active-page race regression alongside new profile request paths.
- building_spawn.lua passes nil, player, info.page, bindGen. init_mouse.lua passes nil, debug. Preserve page-binding handling and avoid spending portal roster/inventory on refusal.
- Restore reconciliation precedent is in World.Save.Types.fromUnitSnapshot ~876, consumed by World.Load.Stage ~1068. An empty src/World/Save diff cannot be a gate: allow non-wire restoration code while requiring unchanged serialized shapes, component versions, and retained codecs.

Legacy mapping authorities read:
- D-26 / FTS-3 (#2515): player -> local controller + definition defaults; wildlife/omitted -> defaults with wildlife fallback; hostile -> no controller + legacy_hostile; neutral -> no controller and no ordinary identity; debug -> no controller/no ordinary identity plus CapLocalCommandable and CapUnrestrictedCombat. legacyDebugProfile is mkProfile Nothing [] allFactionCapabilities.
- #2515 canonical approval narrows inverse-adapter claims: only use stated adapter precedence; wildlife/omitted on an acolyte may return neutral after defaults. Do not promise an exact legacy inverse universally.
- An additive occupant tag is itself relationship identity; when drafting the explicit debug/neutral override behavior, distinguish D-26 base mapping from the subsequently added group tag. Do not silently introduce a debug exception or alter relation authority. Clarify with the owner if full diplomatic inertness versus mandatory group membership remains ambiguous after the requested control/capability decision.

Remaining factual/spec additions for full draft:
- New profile request supports both faction positions with explicit nil Z/page/binding args. Mixed valid and invalid fields apply no partial requested controller or tags and report once; default fallback remains as the current issue promises.
- Canonical roster membership for new local-controller profile requests AND legacy player spawns; ingress owns it and ordinary runtime owners cannot remove it.
- Load backfill is idempotent, preserves all existing tags and provenance, logs once for each actual backfill, never fails solely for absence, no warning for already-correct units. Test existing membership under an additional owner without silently erasing provenance.
- Behavioral save -> restore -> capture evidence for minted roster and occupant tags and owners, including same location integer on different pages. No wire/schema/version changes.
- Keep required docs and evidence in this code PR before final review/merge. No parallel docs landing.
- Existing headless groups include Unit faction profile policy, Portal spawn page binding, persistent ruin encounter AI, Location discovery, Location instance identity. Unit faction profile wire and Lua faction profile API are declared prerequisite deliverables. New group remains exactly Faction spawn ingress.
- Existing issue says expedition_retrieval_probe is flaky and re-run first; do not turn that into unsolicited automatic retries. Listed acceptance probes are location_content_probe, portal_location_probe, expedition_loop_probe. Add appropriate persistence behavior gates in revision instead of blanket empty-diff gate.
- #2551's approved contract permits arbitrary syntactically valid minted tags and overlapping memberships, with removal scoped to one runtime owner. Preserve that behavior for ordinary owners while explicitly protecting ingress-owned roster membership. Do not erase an existing owner's membership when adding the canonical ingress owner. Document new profile request argument shapes and validate entire collections before applying them, consistent with the prerequisite.
- #2551's approved review distinguishes real-codec headless round trips from end-to-end engine.saveWorld probes. Require runtime save/load evidence for this issue's minted identities and backfill, in addition to codec coverage; do not describe a codec-only test as runtime persistence proof.

Next: none for #2554 rereview. Approval batch issues 2555/2556/2557 remain unprocessed; do not resume them without a new task instruction.
