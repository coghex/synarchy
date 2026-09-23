# Issue 2552 rereview investigation

Status: owner approved the full draft; exact revision published and verified. Canonical Claude Fable 5.1 high rereview approved the revision and applied reviewed:approve. Review: https://github.com/coghex/synarchy/issues/2552#issuecomment-5625998295. No code changes have been made.

Skill: /Users/vincentcoghlan/.codex/plugins/cache/kanban/kanban/1.46.0/skills/issue-rereview/SKILL.md
The catalog's former 1.45.0 install path no longer exists; rg found the installed 1.46.0 bundle. Use its vendored trusted-comment helper only.
Canonical backend resolved according to this skill's install-record precedence: /Users/vincentcoghlan/Library/Application Support/kanban/issue-review/approve_issues.py
Current gate: CHANGES_REQUESTED; no pipeline incident; next model claude-fable-5-1@high (backend-selected, do not pin).
Spec: 4d7dc02c9a41c50adece149f121cf10d2544bf131d949e20266b1587c555e52e
Review: https://github.com/coghex/synarchy/issues/2552#issuecomment-5624521063

Verified current implementation:
- World.Thread.Command.Cursor.Plant inserts a PlantDesignation, not a CropPlot. world.plantCropAt in Engine.Scripting.Lua.API.Forage.Crop inserts newCropPlot only on eligible tilled, unoccupied groundcover tiles.
- Harvest.hs derives eligibility from floraGrowth and harvestOpen. It does not read a condition record. An open crop harvest deletes the entire plot. An occupied plot refuses replacement planting.
- CropPlotOf has species, planted day, health. cropPlotInstance uses floraInstanceIdNone and is only a growth adapter; crop conditions belong in the tile-keyed plot value, not an identity-keyed map.
- World.Save.Types nameCrop/resolveCrop record-update only cpSpecies, so additional semantic fields survive that conversion.
- PageActivity is currently v6; CropPlotDTO is three fields with FloraRef species. v1-v5 crop DTOs use frozen three-field CropPlotDTOv1 with FloraId. The approved prerequisite #2549 plans v7 by adding the occurrence-condition map. This issue then moves the component to v8.
- Every retained version must keep its transitive historical crop layout. Add nonempty v6/v7 payload tests plus earlier retained layouts; freezing the outgoing page wrapper alone cannot preserve nested wire bytes.

Owner decision recommendation:
Keep #2552 persistence-only. A newly stored dead condition does not affect existing growth/harvest/render/Lua behavior yet; a growth-derived dead plot still returns nil, but an otherwise open crop carrying a condition remains harvestable until #2555 (EFM-7). #2555 already explicitly integrates condition into growth and harvest eligibility (requirements 2 and 5). The acceptance test can mark an otherwise harvest-open plot, harvest it to prove discard-on-delete, then replant and prove fresh conditionless state. If owner selects immediate refusal, scope the eligibility integration explicitly and replace this acceptance scenario with refusal plus explicit deletion/replanting coverage.

Other necessary draft changes:
- Correct planting attribution.
- Cover pure clear/read, both absent and present condition lookup, and refused second mark preserving every original field plus unchanged plot data.
- Specify complete nonempty legacy migration fixtures for v6/v7 and earlier retained crop shapes; each migrates condition to absent while preserving species reference, planting day and health.
- Keep original title, flora/save-load labels, and exact issue-origin:claude marker. Canonical backend alone manages verdict labels/comments.
- Required persistence docs and fixtures land with the implementation PR before final review/merge per user working agreements.
- Existing groups: save migrations and Save.Snapshot; new group crop plot condition persistence and prerequisite flora condition persistence. GPU-free checks suffice for a persistence-only change.
- Existing run_probes.py accepts --only crop_probe,plant_probe and defaults --jobs 1; explicit --jobs 1 is clearer. No tests or probes have been run during issue-spec investigation.
- save_compat_audit.py --generate-session generates CURRENT format only and requires baseline-id, fixture-id, path, summary. Legacy payloads use frozen DTO encoders/retained fixture provenance; do not invent --generate-session support for historical v6/v7.

Next: await product decision, present full title/labels/body and blocking-review mapping, obtain explicit approval, guard against remote changes, publish via gh issue edit --body-file, re-fetch exact verification, then run resolved backend --path /Users/vincentcoghlan/work/synarchy --rereview 2552 --legacy-policy dual --json with no model overrides. Do not resume queued approvals 2553/2554 during this rereview.
