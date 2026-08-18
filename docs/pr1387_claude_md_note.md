# PR #1387's CLAUDE.md note (#1254, UIT-5B) — extracted 2026-08-18

Removed from the PR branch so it could merge as code-only; the docs lane owns
re-homing it. The live text it patched no longer exists — CLAUDE.md's
**Player transfers + orders** bullet was compressed and its Mode A detail moved to
`docs/engine_contracts.md` §Player transfers, so this needs re-homing there rather
than re-applying.

Recoverable any time with:
```
git show 5706b8dc -- CLAUDE.md
```

## The diff as authored

```diff
@@ -1290,8 +1290,8 @@ before touching each area:
   when the unit is idle, so an in-flight approach to a now-meaningless
   endpoint would otherwise run to completion before the next session
   was ever considered — and every teardown is the same coupled,
-  idempotent one (panel close, replacement, `clear`, Exit to Menu, the
-  save-load reset). A REJECTED replacement leaves the running session
+  idempotent one (see **Mode A session failures** below for the whole
+  trigger list). A REJECTED replacement leaves the running session
   untouched; only a resize is exempt (see the container-window stack
   above). **A UNIT destination is held too** (#1251, UIT-4), because
   unit-to-unit is the one pairing where BOTH ends can walk away. Which
@@ -1312,22 +1312,60 @@ before touching each area:
   Scoping it per species would leave a legal target (a debug-spawned
   bear in the player faction) whose AI never evaluated the hold, walking
   away while an escort approached where it used to be. Release is
-  the one coupled teardown, extended to the pair: every path (panel
-  close, replacement — which must let go of the PRIOR target — Escape,
-  `clear`, Exit to Menu, the SUCCESSFUL-load reset) stops and nudges
-  both, and stopping is all it does to either, so a durable Mode B
-  order a load just restored onto a reused uid survives untouched. The
+  the one coupled teardown, extended to the pair: every path stops and
+  nudges both, and stopping is all it does to either, so a durable
+  Mode B order the unit is carrying survives untouched — the
+  SUCCESSFUL-load reset being the one path that stops neither, because
+  its recorded uids no longer name those units (below). The
   hold adds no refusal of its own: eligibility stays
   `isPlayerCommandable` of the live faction, never a def allowlist, and
   D-6 is unchanged and unreimplemented — a worn item is not in an
   endpoint's `contents` at all, so no pane can offer one, and the
   contract refuses it as `item_not_transferable` if anything names one
-  anyway. Session failure handling stays UIT-5B's. Gates: hspec
-  `--match "Unit transfer"`
+  anyway. **Every way a Mode A session can be interrupted ends it
+  through that ONE teardown** (#1254, UIT-5B), and the module's job is
+  to NOTICE each of them, which splits by phase: while the pair is open
+  the container window's own per-tick `stillThere` hook closes the level
+  (and with it the session) on an endpoint that vanished, but a session
+  spends its whole APPROACH with no window at all, so
+  `transfer_session.update` — a real 0.2 s script tick, the cadence the
+  container window already runs at — is the canonical liveness check and
+  covers BOTH phases. Its rule is `staleReason`: either endpoint gone,
+  the contract's own `eligible` gone (a demolished building, a unit that
+  left the player's factions), or a UNIT endpoint whose pose is `dead`
+  or `collapsed`. That last one cannot come from the contract —
+  `Unit.Transfer.endpointEligible` is `uevCommandable` alone, so a
+  corpse is a perfectly eligible endpoint by its lights — so it is
+  tested here rather than widened there, and the RECOVERABLE poses
+  (crawling, sleeping) are deliberately excluded: a session sits those
+  out. A **new player order to a held unit** ends the session and then
+  proceeds (signed off 2026-08-11 — player intent wins), through the one
+  shared `notePlayerOrder` boundary called from the player's own ingress
+  sites and NOWHERE else (`init_mouse.lua`'s right-click move order,
+  `init_context_menu.lua`'s Attack / Pick up / Move here) — never from
+  inside `unitAi.commandMove`/`commandAttack`/`commandPickup`, which
+  `building_spawn.lua` and `unit_ai_combat.lua` also call for scripted
+  and autonomous behaviour, and never from the escort's own approach.
+  It runs BEFORE the command, since the teardown stops every unit it
+  held. A zoom-band change or a HUD hide reaches the session through
+  `scripts/ui/view_teardown.lua` (#156) rather than a one-off call —
+  which is what covers the approach, where the container window's own
+  entry has no window to close — while `"resize"` stays exempt. Exit to
+  Menu keeps calling `clear` BEFORE `world.destroyAll`, so the release
+  still reaches live entities. The teardown itself is step-isolated: the
+  panels close first and each held unit is released independently, so a
+  missing FIRST endpoint costs neither the other endpoint its release
+  nor either panel its close. Requirement 7 is per-REQUEST atomicity and
+  nothing wider: a session owns no transaction, so ending one never
+  rolls back a commit that already succeeded and can only ever land
+  between two whole requests. Deliberately NOT added: a stall timer, and
+  any handling of an endpoint that is merely unreachable or drifted —
+  both are live and commandable, so neither is a session failure. Gates:
+  hspec `--match "Unit transfer"`
   (contract + both Lua surfaces + the cancel/prune verbs and the
   destroyed-carrier cleanup) and `--match "Transfer context menu"` (both
   gesture modes, the "Cancel transfer" entry, AND the escort session,
-  two-sided hold included),
+  two-sided hold and every #1254 failure trigger included),
   plus `--match "durable transfer orders survive"` for the post-prune
   save; `tools/transfer_order_probe.py` and
   `tools/item_list_widget_probe.py` (both manual-only; the latter owns
```
