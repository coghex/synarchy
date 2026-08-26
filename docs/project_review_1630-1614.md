# Project Review Findings: PRs #1630–#1614

These entries record focused evidence from the senior review of the next twelve merged PRs in merge-time order — #1630, #1632, #1629, #1628, #1627, #1626, #1623, #1625, #1622, #1619, #1615, and #1614 — for later one-at-a-time disposition. The same landing interval contains one direct first-parent commit, `04a6f55d` (`docs: add the CI stdout and logging hygiene findings report`), which is documentation-only and remains coherent with the report-processing lane.

Status legend: `[ ]` unprocessed · `[#N]` filed as issue N · `[no-issue]` reviewed and deliberately never to be filed · `[deferred]` blocked on a concrete precondition

PRs #1630, #1629, #1628, #1626, #1623, #1625, #1622, #1619, #1615, and #1614 retain their intended contracts in the current tree, and their focused tests, self-tests, or audits passed during this review. PR #1632 fixes the stale-save and shared-slot behavior recorded by NCT-22 / issue #1620, but its isolated roots still share mutable developer configuration. PR #1627 repairs the original non-default-speed loss from issue #1599, but its single pause epoch cannot preserve a pause imposed by another engine source during an autosave. No batch defect was found to have been repaired by a later merge, and tracker and pending-report searches found no existing owner for the two scopes recorded here.

## Status

- [x] PRR-1. Four isolated location probes still share the live config tree — [#1729]
- [x] PRR-2. An autosave restore can erase a notification pause that arrived while it ran — [#1730]

## 1. Location-probe configuration isolation

### [#1729] PRR-1. Four isolated location probes still share the live config tree

> **Captured note:** Isolate mutable configuration as well as save slots in the four location persistence probes. Their temporary resource roots symlink the checkout's real `config/`, so developer-local overrides affect probe behavior and engine initialization can write a new local override through the link into the checkout.

**Verification:** Verified structurally and with a synthetic source tree outside the checkout. Calling each distinct root builder produced a `config` symlink that was `samefile` with the synthetic source configuration directory; writing a representative `*.local.yaml` through the temporary root immediately created it in that source directory. `portal_ghost_probe.py` imports and uses the location-content builder, so it has the same alias. The production notification loader independently proves this is not a hypothetical write path: it materializes `config/notifications.local.yaml` whenever that file is absent.

**Evidence:**

- `tools/location_content_probe.py:118-136`, `tools/location_overlay_probe.py:86-104`, and `tools/location_stamp_idempotent_probe.py:140-158` — each helper symlinks all four families, including `config`, while describing them as read-only content safe to share.
- `tools/portal_ghost_probe.py:51-55,200-226` — the fourth probe imports `make_isolated_root` from location-content and gives both its headless writer and offscreen reader that root.
- `src/Engine/Asset/YamlNotifications.hs:159-177` — normal engine initialization creates and writes `notifications.local.yaml` under the selected resource root when no local file exists. Through these roots, that path resolves into the developer checkout. Existing local notification, keybinding, video, save, or other overrides are likewise inherited rather than excluded.
- `tools/run_probes.py:402-435` already records the underlying repository-wide fact for scheduling: engine initialization itself writes `config/` when local files are absent. The new helpers contradict that established ownership model by treating the same directory as immutable shared input.
- `tools/location_embark_probe.py:203-225` demonstrates the safe sibling pattern added immediately afterward: symlink only `scripts`, `assets`, and `data`; copy `config` while excluding `*.local.yaml`; make the private destination owner-writable; and keep all runtime writes inside the invocation root.
- NCT-22 in `docs/non_ci_test_audit_findings.md` and closed issue #1620 own stale fixed save fixtures, request-specific completion, and save-artifact cleanup. Neither records inherited local configuration or writes escaping through a configuration symlink. Pending project-review reports cover setup cleanup and copied-config permissions in other probe files, not these four aliases.
- All-state tracker searches for location-probe config symlinks, inherited local overrides, and mutable probe configuration found no open or closed owner beyond unrelated probe-runner and config-isolation work.

**Handoff context:**

- **Current behavior:** A developer's `*.local.yaml` files silently participate in all four probes. On a checkout where `notifications.local.yaml` is absent, the first supposedly isolated engine boot creates it in the real checkout through the symlink; teardown removes only the link and temporary save tree, leaving that newly created local state behind.
- **Expected behavior:** Every invocation receives tracked configuration defaults without developer-local overrides, and every configuration file created or changed by its engines remains inside the invocation-owned tree and is removed with it.
- **Scope and constraints:** Surfaced in PR #1632 / issue #1620. Preserve per-invocation resource roots and save names, request-specific save/load completion, the two-process scenarios, content-family sharing, symlink-safe teardown, and visible cleanup failure. Do not modify, remove, or chmod the source checkout's configuration.
- **Verification target:** Add pure root-builder coverage for all distinct helpers, or one shared helper contract if consolidated, that seeds a source `*.local.yaml`, asserts it is absent from the private root, writes a new local file through the root, and proves the source tree is unchanged. A boot-level case should also prove the notification override is materialized only inside the temporary root.
- **Deduplication:** Closed issue #1620 and its NCT-22 source report do not own configuration isolation. `docs/project_review_1655-1643.md` covers construction cleanup and copied-config permissions in different probes, while `location_embark_probe.py` fixed only itself. Tracker and pending-report searches found no owner for these four files.
- **Remaining uncertainty:** The review did not boot the four manual probes, one of which requires a GPU. The filesystem alias was reproduced directly and the production write path is unconditional when its local file is absent, so a live boot is not needed to establish the escape or local-override inheritance.

## 2. Pause-source ownership during autosave

### [#1730] PRR-2. An autosave restore can erase a notification pause that arrived while it ran

> **Captured note:** Make autosave release only the pause it imposed. A later `pause: true` notification that lands before the save finishes currently joins the same Boolean epoch without acquiring ownership, and the successful autosave then closes that epoch and resumes simulation.

**Verification:** Verified by a deterministic current-code transition trace. Starting unpaused, `acceptSaveRequest` records the player generation and opens the pause epoch. After capture, the world thread releases state owners before performing the disk write. A pause-configured event in that window calls `imposePause`, which sees the already-true flag and is a complete no-op; as an engine-imposed transition it also does not advance the player-intent generation. On successful write, `restoreAfterAutosave` therefore sees the original generation, writes the autosave's resume scale, consumes the sole resume slot, and clears the global flag. The notification's request to remain paused is lost.

**Evidence:**

- `src/World/Pause.hs:20-27,69-88` — the implementation represents every overlapping source as one epoch: only the false-to-true transition captures state, while every later `imposePause` does nothing at all.
- `src/Engine/PlayerEvent/Emit.hs:205-214` — any category whose configuration has `pause: true` calls that same `imposePause` after recording its event and popup.
- `src/Engine/Scripting/Lua/API/Save.hs:460-480` — autosave acceptance snapshots the current player-intent generation and imposes the pause while holding that generation's mutex.
- `src/World/Thread/Command/Save/WriteWorld.hs:361-380` — once the immutable snapshot is encoded, state owners resume before `writeSaveFiles` begins. This creates a real interval in which another thread can emit an event while the autosave transaction and its pause remain open.
- `src/Engine/Core/Capability/WorldSim.hs:152-188` and `docs/engineenv_capability_inventory.md:509` — only player pause or time-scale transitions advance `playerIntentGenRef`; notification pauses deliberately do not. Consequently the autosave's compare-and-restore gate cannot observe this later engine pause.
- `src/World/Thread/Command/Save/WriteWorld.hs:409-428,459-498` — the code protects the save's own later `save_load` event by restoring before emitting it, but the restore otherwise closes the shared epoch whenever the player generation still matches. It has no source token, count, or later-engine-pause generation to consult.
- `test-headless/Test/Headless/World/PauseSpeed.hs:159-365` covers repeated pause imposition, player-intent generation, and the save's own post-restore notification ordering. It does not cover the opposite order: autosave acceptance, unrelated pausing event, then autosave restore.
- All-state tracker searches for autosave erasing or closing a notification pause and overlapping pause-source ownership found no current owner. The earlier `docs/project_review_167-80.md` concern became issue #1599 and owned loss of the selected speed on an ordinary notification pause, not this autosave interleaving.

**Handoff context:**

- **Current behavior:** If a pause-configured event arrives after an unpaused autosave starts but before its successful restore, its popup and log entry remain, yet the autosave resumes simulation as though no later engine pause occurred. This can happen with any user-configured pausing category; the default `survival_critical` category is also configured to pause.
- **Expected behavior:** A successful autosave may undo its own pause only when no independent pause source has appeared since acceptance. A later notification pause remains authoritative until the player explicitly resumes, and that resume restores the speed captured at the start of the pause epoch.
- **Scope and constraints:** Surfaced in PR #1627 / issue #1599 at its interaction with issue #913's autosave restore. Preserve page-bound resume scales, the player's generation-based last-write-wins guarantee, load-publication policy, failed-save safety pause, and the ordering that lets the save's own success event pause after restoration. Engine-imposed pauses should not be mislabeled as player intent merely to make this case visible.
- **Verification target:** Extend the focused pause-speed suite with the exact order `acceptSaveRequest` for an autosave → emit an unrelated pause-configured event → `restoreAfterAutosave`. Assert the restore does not clear the pause, the page clock remains zero, and the subsequent player resume returns to the pre-autosave scale. Retain the existing save-success-event and player-intent cases.
- **Deduplication:** Closed issues #1599 and #913 establish the component contracts but do not track this overlap defect. Searches across open and closed issues and pending findings/project-review reports found no separate owner.
- **Remaining uncertainty:** The transition was established from the production functions rather than by delaying a real disk write. The exact frequency depends on which categories a player configures to pause and which event sources remain active during the write interval; the ownership failure is independent of that frequency.
