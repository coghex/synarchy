# Approved saguaro juvenile living/dead pair — #2559

## Delivered artwork

- `assets/textures/flora/saguaro/sprout.png`: owner-approved replacement for the living placeholder.
- `assets/textures/flora/saguaro/sprout_dead.png`: upright dead companion, preserving the replacement's complete silhouette.

Both PNGs are 48×48 RGBA with binary alpha, bounds `(18, 5, 30, 47)`,
and ground contact at row 46. The living stem stays 12 pixels wide through
row 44, then becomes 10 pixels on row 45 and 8 on row 46. Row 47 remains
transparent. This gives the slight oval ground-contact arc requested by
the owner. The dead sprite changes colour and surface detail, not posture.

No adult artwork, YAML, runtime behaviour, or content identifiers change.
Production mortality declarations remain with #2562.

## Owner decisions and explicit approvals, 2026-09-11

The owner rejected drooping and expanded this task to replace the living
placeholder in the same PR:

> no, the drooping is not what cactii do. look how the matured dead is just
> the structure of the regular version with a few blemishes and a color
> change, that is what it should be. my sprout is not a great texture, i
> was really leaving it in as a placeholder, if you want to regenerate
> that too that would be fine, i can manually override the review to
> include it in the pr

The owner rejected both the original rounded foot and the first redesign's
flat bar, requesting a slight oval arc that blends into the ground in a
top-downish view. All owner visual verification used real Synarchy
`--preview flora/saguaro` windows, not contact-sheet approval.

Living approval after selecting the exact approved bytes under the
temporary review name `sprout_arc_review.png`:

> the arc version is best, now make the dead version

Dead approval after selecting the delivered `sprout_dead.png`:

> yes, that looks great

These decisions supersede the original issue's unchanged-placeholder and
collapsed-inward art direction. They do not represent a solver-applied
review override; the canonical tracker/review workflow remains separate.

When asked whether to update #2559 to match the approved direction and
rerun issue review, the owner explicitly chose:

> Keep the issue unchanged; I’ll handle the override

The issue is therefore intentionally unchanged. Any scope-related review
objection is for the owner to resolve; this implementation applies no
gate or verdict override.

## Generation and provenance

PixelLab was the owner-selected generation service.

The living arc was produced by `edit_image`, job
`0651c000-d5c4-4ff5-8535-da61096d6400`, seed `255974`.
[Living generation evidence](arc_living_evidence.json) contains the exact
prompt and hashes. The flat intermediate is retained only as the actual
generation input ([PNG](flat_living_candidate.png),
[provenance](flat_living_evidence.json)), not as accepted or production art.
[Original placeholder](original_sprout.png) records the pre-change input.
[Raw arc output](arc_living_raw.png) and
[approved delivery bytes](arc_living_candidate.png) are retained.

The dead companion was produced by `edit_image`, job
`c24b78a1-1e06-4b56-aeff-965877d9ed5f`, seed `255976`, with the approved
living arc as its sole image input. [The request](dead_request.json)
contains the exact prompt. [Raw output](dead_raw.png) was delivered
unchanged: the model already preserved the entire alpha silhouette, so
the colour-shifted band-restoration check required no pixel changes.
Rows 41–47 were designated as the lower band before generation.
No recolouring, hand retouching, or alpha repair was applied afterward.

[Canonical evidence](../saguaro_sprout_dead_evidence.json) records the
reference and delivered hashes, full generation inputs, band validation,
and both owner verdicts. [Detailed dead evidence](dead_evidence.json)
also links the preview and raw output.

## Validation and preview evidence

Run `python3 docs/art/saguaro_sprout_pair/verify.py` from the repository
to repeat the direct image checks. Adding `--sheet` recreates the
[native/nearest-enlarged comparison](approved_pair.png); this supplementary
sheet is not a substitute for the real-preview owner approvals above.

- Direct PNG checks: 48×48 RGBA, alpha values exactly 0/255, bottom row
  transparent, lowest opaque row 46.
- Complete living/dead alpha comparison: zero differing pixels; the
  recorded lower-band comparison also has zero differences.
- `cabal build all`: passed at code revision
  `13bd01bdeb3043e0087b93adc90d67352a253bf0` in this isolated worktree.
  Subsequent changes are artwork and evidence only.
- `python3 tools/texture_subset_audit.py`: passed, all 13 subsets.
  This validates declared references, not this undeclared new PNG.
- Living real-preview [capture](arc_living_preview.png) and
  [selection dump](arc_living_preview.json).
- Dead real-preview [capture](dead_preview.png) and
  [selection dump](dead_preview.json).

Preview used the successfully built production executable, Apple M3 Max
Vulkan, and nearest filtering. Captures include temporary flat/arc review
entries that were present during owner signoff; those exact-byte review
copies have since been removed from the production asset directory.
Rejected attempts remain recoverable outside the worktree and are not
part of the delivered art.
