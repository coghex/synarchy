## Background

Epic #2526, EFM-8B. Deliver one image in one art PR: generic fire-killed
saguaro remains. EFM-8A (#2559) separately owns the dead sprout. Both
must land before EFM-9 (#2562) integrates the pilot.

Verified on `master` `5b677136fdac` (2026-09-10): the saguaro directory
contains `sprout.png`, `matured.png`, `matured_flowering.png`,
`matured_fruiting.png`, and `dead.png` from #1688 / PR #1725; the required
image is absent. This extracts one asset from the former two-image EFM-8
manifest; it does not add a third pilot asset. `sprout_charred.png` remains
deliberately absent to exercise phase-before-cause fallback.

## Requirements

Create only `assets/textures/flora/saguaro/charred.png` as new artwork:
**128×128 RGBA**, with both fully transparent and opaque pixels.

Match the landed saguaro family: hard pixel edges, compact shading,
restrained palette, transparent background, and no soil, dirt mound,
painted shadow, or scenery. Preserve the living reference's ground-contact
row, canvas placement, and recognizable species silhouette.

The reference is `matured.png`: preserve its trunk, arm count,
branching, base row, and placement. Show blackened, charred trunk and arms
with ember-grey/ash highlights on rib edges; tips may be broken or burnt
short. Include no green tissue, flames, smoke, or glow. It must be the same
individual after fire and visibly distinct from the weathered tan skeleton
in `dead.png`. This generic cause asset depicts mature remains only.

## Art workflow

The image is missing and blocks EFM-9. Before creating it, the owner must
choose manual supply or generation through the documented PixelLab flora
pipeline. Follow `docs/asset_generation.md`, including its color-shifted
stage silhouette/band-restore rule. Do not use a placeholder.

Present this image at native size and enlarged with nearest-neighbour
scaling. Complete `cabal build all` in the isolated worktree and use the
real `--preview flora/saguaro` window for owner signoff. Ensure the proposed
image is actually selectable; a temporary uncommitted preview declaration
may be used and must be restored before commit. The new production
`textureVariants` declarations belong to EFM-9. Record explicit approval
of this image before opening its art PR; rejected art is revised and
presented again. Include required evidence in that same PR.

## Acceptance

```bash
python3 - <<'CHECK'
from PIL import Image
path = "assets/textures/flora/saguaro/charred.png"
im = Image.open(path)
assert im.format == "PNG" and im.mode == "RGBA"
assert im.size == (128, 128)
assert im.getchannel("A").getextrema() == (0, 255)
print("OK", path, im.size)
CHECK
cabal build all
python3 tools/texture_subset_audit.py
cabal run exe:synarchy -- --preview flora/saguaro
```

Expected: the image check prints `OK`, the build and audit pass, and the
owner approves the actual proposed image in native/enlarged and real
preview evidence. The PR contains this one new image and its associated
evidence only; temporary preview declarations are absent from the final
diff. Existing images remain unchanged. Audit success alone is not visual
signoff.

## Out of scope

Dead-sprout art, `sprout_charred.png`, other mortality/seasonal variants,
other species, YAML integration, and runtime changes.

## Related

- #2526: tracking epic; this is independently reviewed and delivered.
- #2530: prerequisite filename/selector contract. Depends on #2530.
- #2559: sibling dead-sprout asset delivery; it contains no charred image.
- #2562: blocked by both assets; owns production declarations and integration.
- #1688 / PR #1725: the living and dead saguaro references.
- `docs/environmental_flora_mortality_design.md`, D-8 and D-15–D-17.

<!-- issue-origin:codex -->
