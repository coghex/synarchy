# Issue 2529 rereview investigation

Status: owner confirmed preservation of existing raised ice elevations. Full revised specification is drafted in 2529_body.md and awaits explicit approval before GitHub edits.

Canonical changes-requested review: https://github.com/coghex/synarchy/issues/2529#issuecomment-5620827565
Current spec: 73ee77e1f80cf0ca4d28bdfa651ade8ef2678d1edc0e63b367e2297a49fc02e3
Investigated checkout: 5b677136fdacb75a604da796e066c50710c128b7

Verified corrections to include after owner decision:
- World.Fluid.Ice computes max(terrain, fluid) as base; drape surface = base + 1, basin = min(fill level, base + 20). Rendering uses icSurface independently. Recommend preserving this geometry, converting the fluid contribution to the documented ceiling only when deriving the base.
- Binding includes startup preload, world_view.ensureStructuralTextures, world_manager initial binding, world_view.rebindStructural, Lua parseTextureType, World.Texture.Types, World.Render.Textures.Types, and handleWorldSetTextureCommand. Test intended level handles and late rebinding/cache invalidation.
- Level masks already include side alpha and lighting channels. Verify the combined rendered silhouette and separate side geometry, exact endpoints and no double coverage, including unequal left/right neighbors and enclosed equal-height cells. Quad counts alone cannot prove this.
- Production visibility uses documented ceiling for slice ownership; test negative absolute surfaces, zero as a wet exact-multiple plane over lower terrain, partial values around slice and effective-depth boundaries, all facings and ordinary/U seams.
- Update descriptions of current behavior only; retain historical evidence, migration explanations, earlier-phase descriptions, and findings status.
- Required docs, reproducible offscreen capture evidence and recorded owner verdict belong in the code PR before final review and merge.
- Preserve title, labels enhancement/fluid, and exact issue-origin:claude marker. Verdict labels stay backend-managed.

Acceptance drafting notes:
- Existing groups World.Render.SideFace and World.Slope.slopeBit are registered in test-headless/Spec.hs. Assign an exact new group name for fluid-level render/binding cases rather than leaving a placeholder.
- Add covered partial Lake/Ocean and dry drape/basin ice regressions if preservation chosen; whole-z ice output remains unchanged.
- Existing offscreen_probe.py is a broad UI probe, not the new fluid-scene gate. Use its boot pattern for a focused reproducible scene and record actual commands, revisions, inputs, captures and verdict in the implementation PR.
- Issue 2520 retains integer Lua queries and provides exact world-edit storage; do not assume a new public exact-unit Lua edit API exists. Use its exact representation through the appropriate fixture/edit path for scene setup.
- Prerequisites 2517,2520,2525 are declared dependencies, not expected implemented in the current checkout. Assets must be approved via 2525 before integration; this issue changes no masks.

Personal skill /Users/vincentcoghlan/.codex/skills/issue-rereview/SKILL.md requires product decision, then full standalone title/labels/body and explicit approval before gh issue edit. After publication and exact verification run /Users/vincentcoghlan/work/approve-issues.py --path /Users/vincentcoghlan/work/synarchy --rereview 2529 --legacy-policy dual --json. Current check says route claude-fable-5-1@high. Do not retry a selected-model failure or manually change review labels.
