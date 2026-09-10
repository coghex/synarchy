# Fluid-level mask review (#2525)

Generated on 2026-09-10 from `World.Slope.FaceMaps`, based on
`c847a9f078a741d9497fff8ce6cebc6d16520ac9`. The eight tracked masks are
`assets/textures/facemap/isoface_level_1.png` through `isoface_level_8.png`.
[File hashes and encoder versions](manifest.json) bind this record to the
reviewed files. The original `isoface.png` remains unchanged.

## Visual evidence and owner decision

[Contact sheet](contact_sheet.png): 512 native-resolution fluid swatches,
covering all eight levels, River/Lake/Ocean/Lava, sand/basalt, all four
camera facings, and local sun phases 0.125/0.75. These are schematic CPU
composites of the production material and lighting formula; gameplay
registration and integrated rendered evidence belong to #2529.

[Live preview capture](preview_browser.png) shows the existing browser
rendering the material comparison while the owner browsed the eight masks.

**Owner approval: APPROVED on 2026-09-10.** After viewing all eight raw
masks and the material comparison panels in the real Synarchy preview,
the owner answered the explicit approval question with:

> yes, i approve, these look good

This approves levels 1 through 8 and their appearance in the comparison
sheet. The masks and contact sheet are exactly the files bound by the
manifest. The preview ran after `cabal build all` in the isolated issue
worktree, with nearest filtering and byte-identical temporary copies in
`flora/fluid_level_review`. The preview is closed and all temporary copies
were verified and removed; no preview CLI or gameplay YAML change is in
this delivery.

## Validation

- `cabal build all`: passed (production profile).
- `cabal build synarchy-test-headless`: passed.
- Focused `World.Slope.FaceMaps`: 15 examples, 0 failures. Includes every
  tracked PNG compared with the production Haskell output, full-level
  equality, fixed corners, unchanged top, exact depth and channel laws.
- `fluid_level_masks.py`: all eight masks valid.
- `fluid_level_masks.py --self-test`: 12 structural mutations rejected,
  including an interior hole and a pure-colour channel swap; count checked.
- `fluid_level_masks.py --check-generated`: all eight production RGBA
  outputs and regenerated PNG bytes match; original files not rewritten.
- `fluid_level_contact_sheet.py --self-test`: tint, camera-dependent
  lighting, night ambient, and material/mask alpha checks passed.
- `preview_cli_probe.py`: passed; `facemap` remains unexposed.
- `texture_subset_audit.py`, `check_texture_paths.py`, and
  `unicode_operator_audit.py`: passed.

Regeneration and preview instructions:
[`docs/asset_generation.md`](../../asset_generation.md#fluid-level-masks-2525).
