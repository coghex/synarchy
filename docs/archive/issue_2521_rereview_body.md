## Background

FTS-5 of #2496, Phase 5, `critical path`, `depends on #2518` (FTS-4),
which must merge first. Port the alliance, hostility, combat-permission,
and AI consumers to FTS-1's directed controller/tag relation authority,
fed by FTS-2's catalogue. Deliver the code, tests, and accompanying
contract updates together in one PR closing this issue.

Verified on `master` `704d5ed347e5` (2026-09-10):

- The four production string-based relation callers are
  `scripts/unit_ai_medic.lua:92`, `scripts/unit_ai_core.lua:445`,
  `scripts/unit_ai_encounter.lua:80`, and
  `scripts/init_context_menu.lua:156`. No Haskell consumer outside the
  legacy authority and Lua binding uses those legacy relation queries.
- The medic helper currently passes patient before medic; group
  effectiveness already uses candidate toward subject. Symmetric
  fixtures cannot detect the medic argument reversal.
- Encounter acquisition and continuation separately require hostility.
  `disengageReason` rejects a non-hostile target, and the guard's return
  action outranks attack execution. Merely issuing `commandAttack`
  therefore does not prove that non-hostile self-defense works.
- Existing damage awareness provides attacker ID and time through
  `unit.getLastAttacker`. Initial retaliation uses a ten-second window;
  generic mid-fight target swapping has its own shorter window and
  eligibility rules. Last-attacker fields reset on load.
- Encounter acquisition currently announces a hostile faction relation
  even when the new policy would acquire through local retaliation.
- FTS-1's documented compatibility matrix intentionally changes eight
  ordered legacy relations to neutral. Its attack-permission matrix
  remains identical across all 25 ordered legacy pairs.

Owner decisions: FTS-5 supplies policy hooks; the live relation overlay is
empty at runtime and grandfathered combat authorization is fixture-only
until the later FTS-6 slices. For self-defense, a qualifying new hit permits
retaliation for ten seconds. Old hits cannot restart combat after diplomacy
clears or an attack goal ends; a genuinely later hit may permit a fresh
local response without making the team hostile.

## Requirements

1. **One directed relation authority.** All live-unit relation queries use
   FTS-1 over both profiles, the FTS-2 base table, and an explicit live-overlay
   input. The production overlay is empty in this slice. Production
   consumers no longer derive relations from the FTS-3 adapter,
   `unit.getFaction`, or legacy `Faction` values. Scripts neither
   reimplement precedence nor inspect tag sets to decide relations.

2. **Directed alliance.** A medic treats a patient when the medic regards
   the patient as `ally`. `groupEffectivenessVs` counts a candidate when
   that candidate regards the subject as `ally`, preserving existing
   range/commitment rules. The threat itself remains excluded, even
   when allied. Apply the approved FTS-1 matrix rather than preserving
   accidental alliances from the scalar model.

3. **Attack permission.** Permission is the attacker's directed hostility
   toward the target, or unrestricted combat on either profile. The
   context-menu Attack gate uses this authority. Debug staging remains
   permitted even across shared controllers/tags. The permission query
   and unrestricted-combat capability do not mutate relations, tags,
   ownership, discovery, or team knowledge. Permission alone does not
   make a non-hostile unit an autonomous acquisition target.

4. **Local self-defense.** Autonomous acquisition, pursuit, and
   continuation consult one consistent policy seam using the directed
   relation, local attacker identity/time, applicable incident cutoffs,
   and optional current-goal authorization. Ordinary acquisition needs
   hostility. A living, otherwise eligible recent attacker may instead
   be acquired through local awareness, including under an allied or
   neutral relation, without team-wide mutation. This permission must
   survive subsequent AI decisions while the same attacker remains
   recent: elapsed time at most ten seconds qualifies; greater than ten
   seconds does not. Each qualifying hit from that attacker refreshes
   the window. Visibility alone never refreshes local self-defense.
   Without hostility or valid grandfathering, expiry ends that
   local-retaliation goal. This rule does not revoke an explicit permitted
   player/debug attack merely because its faction relation is neutral.
   Existing page, leash, sight-memory, retreat, return-home, target
   eligibility, and player-order safeguards still apply; local awareness
   does not bypass them.

5. **Old damage cannot reopen combat.** An applicable diplomacy-clear
   boundary and termination of an attack goal each establish a cutoff
   for the affected unit's local response. A hit at or before the latest
   applicable cutoff cannot supply fresh self-defense, even if it remains
   within ten seconds. After diplomacy clears, pre-clear damage alone
   cannot sustain a non-hostile fight; continuation requires valid
   grandfathering or a qualifying later hit. After a goal ends, a hit
   must be strictly later than that termination to start another local
   attack. Thus a hit received during a grandfathered goal cannot be
   reused after that goal ends. A later hit may start fresh self-defense
   once existing retreat/return restrictions permit it; this is a new
   local response, not revival or transfer of the old authorization.
   Preserve the existing player-withdrawal cutoff and hold behavior.

6. **Grandfather only the current goal and target.** A supplied
   authorization permits a non-hostile attack to continue against the
   target assigned to that particular goal occurrence, including after
   local awareness expires. It ends on target death/disappearance,
   escape under the existing page/leash/sight-memory rules, attacker
   retreat, or any other goal completion/replacement. It cannot transfer
   to another target, another unit, or a replacement goal, even a new
   goal of the same name against the same target. While relying on this
   authorization, the assigned target stays fixed; a mid-fight
   retaliation swap cannot carry it to another attacker. It grants
   nobody else permission to join. Current hostility and qualifying new
   self-defense remain independent permissions, subject to requirements
   4–5; the authorization itself never permits a restart.

7. **Keep the runtime boundary explicit.** Production reads existing
   local damage awareness and enforces the local goal-termination cutoff.
   Minimal Lua decision bookkeeping needed to reject stale incidents is
   allowed, but only as transient state: it grants no combat permission,
   adds no saved payload, resets on successful load/session teardown,
   and cannot attach to a reused unit identity. Classify it as
   `Reset to default` in the persistence inventory and test that lifecycle.
   Existing saved withdrawal/hold state remains unchanged. Nonempty live
   overlays, diplomacy-clear cutoffs, and grandfathered authorizations
   are supplied by fixtures through the policy seam in this slice;
   FTS-6 owns their runtime production and persistence. Do not create
   runtime diplomacy state or runtime grandfathered authorization here.

8. **Lua compatibility.** Add unit-ID-based relation, alliance, and attack
   permission queries beside FTS-4's property queries and port all four
   production callers. The string-taking verbs retain their existing
   behavior for the compatibility window and gain no production callers.

9. **Exact compatibility matrix.** With the approved legacy profiles and
   base table, rows are actor and columns are target; `A` means ally,
   `H` hostile, and `N` neutral:

   | Actor → target | player | wildlife | hostile | neutral | debug |
   |---|---|---|---|---|---|
   | player | A | H | H | N | N |
   | wildlife | H | A | H | N | N |
   | hostile | H | H | A | N | N |
   | neutral | N | N | N | N | N |
   | debug | N | N | N | N | N |

   The eight changed ordered relations are player↔debug,
   wildlife↔debug, hostile↔debug, neutral→neutral, and debug→debug.
   Reflect their consequences in medic eligibility, group effectiveness,
   and autonomous hostility decisions. Attack permission remains
   compatible across all 25 pairs, including every debug direction.
   Preserve authored profiles, controller/tag precedence, and spawn
   mapping delivered by the prerequisites.

10. **Truthful encounter notices.** When acquisition is through local
    retaliation, describe self-defense/recent attack rather than claiming
    a hostile faction relation. Ordinary hostile acquisition retains its
    accurate explanation. Preserve discovery-gated visibility and the
    existing exactly-once aggression/disengagement notices per episode,
    including multiple guards deciding before queued writes are visible.

11. **Delivery boundaries and documentation.** No `UnitInstance` field,
    save encoding/component version, spawn argument, tag/relation mutation
    verb, communication path, or nomad-specific threat tuning changes.
    Update `docs/faction_tag_system_design.md` where local awareness,
    D-21/D-33, and FTS-5 describe the interaction, plus the relevant engine
    contract and persistence inventory. Record that new damage is a
    separate self-defense permission and that runtime diplomacy remains
    deferred. Include these documents and validation evidence in the same
    implementation PR and complete them before final review and merge.

## Acceptance

Extend `Lua faction model` with a nonempty
`directed relation consumers (FTS-5)` group using production unit-ID
bindings and the real consuming scripts. Do not replace the new relation
bindings with Boolean/relation stubs in these integration cases:

- Shared-controller units are allied and cannot attack each other unless
  unrestricted combat applies. Acolyte/nomad units without a higher
  precedence rule are mutually hostile.
- An injected `fight_team_A -> fight_team_B` hostile overlay permits A's
  attack while B's reverse relation remains allied and its attack is
  refused. Verify the constructed Attack menu item's enabled/disabled
  state through `scripts/init_context_menu.lua`, including debug staging.
- Asymmetric medic fixtures use medic→patient ally with the reverse
  neutral, and the converse refusal case. Asymmetric group fixtures use
  candidate→subject ally with the reverse neutral, and the converse
  exclusion case. Keep threat exclusion and range/commitment coverage.
- Cover all 25 compatibility relation and permission pairs, including
  the eight intentional changes and their medic/group consequences.
  Legacy string queries retain their original answers.

Put encounter sequences under `persistent ruin encounter AI`, and shared
combat-decision sequences under `directed relation consumers (FTS-5)`.
Exercise acquisition, subsequent guard/attack decisions, termination, and
attempted reacquisition through the real scripts. An assertion that
`commandAttack` was called is insufficient:

- Allied and neutral recent attackers permit local self-defense and
  continued attack without team-wide changes; uninjured peers do not
  join merely because another unit retaliated. Test time exactly ten
  seconds, just beyond ten, and refresh by a new qualifying hit.
- With hostility cleared by a fixture, pre-clear damage is insufficient
  without authorization. A hit strictly after clear permits local
  self-defense. Test a hit exactly at the cutoff as ineligible.
- A valid fixture authorization keeps only the original goal/target
  fighting after awareness expiry. Test its absence and mismatched
  unit, target, or goal occurrence as refusal cases.
- Exercise target death, disappearance, page departure, target and actor
  leash escape, sight-memory expiry, retreat, and goal completion or
  replacement. After each terminating boundary, the old authorization
  is unusable. Attempt reuse on another target and on a replacement
  goal against the same target, including the generic retaliation-swap
  path rather than only the encounter module.
- End a goal while its attacker is still recent, including a hit received
  after diplomacy cleared but before that goal ended. Old/equal-time hits
  cannot restart it; a hit strictly after termination may start a new
  eligible local response. Existing return-home, retreat, and player
  withdrawal restrictions must still win where applicable.
- Capture hostile-acquisition and non-hostile-retaliation notice text;
  preserve hidden encounters' privacy and exactly-once episode events.
- Exercise the transient cutoff through production lifecycle hooks:
  successful load and Exit to Menu clear it, reused IDs inherit nothing,
  and snapshots contain no new cutoff/authorization payload. Existing
  saved withdrawal/hold state retains its behavior.

Run these commands from the implementation worktree, in the production
profile. Run the three probes one at a time:

```bash
cabal build all
cabal build synarchy-test-headless
cabal test synarchy-test-headless --test-options='--match "Lua faction model"'
cabal test synarchy-test-headless --test-options='--match "persistent ruin encounter AI"'
cabal test synarchy-test-headless --test-options='--match "player move orders during combat"'
cabal test synarchy-test-headless --test-options='--match "Unit medical reach"'
cabal test synarchy-test-headless --test-options='--match "medical kit instance targeting"'
cabal test synarchy-test-headless --test-options='--match "Transfer context menu"'
cabal test synarchy-test-headless --test-options='--match "Unit faction profile policy"'
cabal test synarchy-test-headless --test-options='--match "Unit faction model"'
cabal test synarchy-test-headless --test-options='--match "unit AI load reset"'
python3 tools/lua_registration_audit.py
python3 tools/lua_duplicate_function_audit.py
python3 tools/lua_module_budget.py
python3 tools/unicode_operator_audit.py
python3 tools/persistence_inventory_audit.py
rg -n 'faction\.(relation|areAllies|canAttack)\s*\(' scripts
python3 tools/run_probes.py --only medic_coord_probe
python3 tools/run_probes.py --only retaliation_swap_probe
python3 tools/run_probes.py --only expedition_loop_probe
```

Expected: builds succeed, every selected spec group passes and is nonempty,
all five audits exit zero, and all three probes pass. The `rg` check prints
nothing and exits 1 for no matches. Record the commands/results in the PR.

Retain the original issue's gate exclusions: `combat_anim_probe` and
`lunge_probe` case B are not acceptance gates because their failures were
recorded on the base; `mental_state_probe` and `wander_hazard_probe` are
A/B evidence only. These exclusions do not excuse regressions introduced
by this change. No full suite or full local CI is required by this issue.

## Out of scope

- Runtime directed-relation causes, Attack/Hold/Cancel transactions,
  diplomacy resolution, grandfathered authorization, tag mutation, and
  profile getters: later FTS-6 slices.
- Damage/witness/report propagation, radio scope, reverse team-wide
  relations, and durable incident knowledge: FTS-6A.
- Removing legacy string-taking verbs, `unit.getFaction`, or the adapter:
  FTS-7.
- Nomad-specific threat range/leash tuning and multiplayer transport.

## Related

- Epic #2496; prerequisite #2518, following #2500, #2506, and #2515.
- #912: relation/group-effectiveness rules; #916: encounter and player
  combat orders; #717: lash-out target exclusions; #2328: strike
  revalidation; #1995: Lua verb declarations.
- `docs/engine_contracts.md` sections Position hold and The expedition
  loop; `scripts/CLAUDE.md`; `docs/persistence_contract.md`.
- Supporting design: `docs/faction_tag_system_design.md`, Directed
  relation precedence, Local awareness and team knowledge, Compatibility
  behavior to preserve, and D-13, D-15, D-18, D-19, D-21, D-23, D-24,
  D-33. This issue contains the normative implementation requirements.

<!-- issue-origin:claude -->
