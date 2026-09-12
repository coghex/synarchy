# Approved charred saguaro remains — #2596

## Delivered artwork

`assets/textures/flora/saguaro/charred.png` is the owner-approved generic
fire-killed texture for the mature saguaro. It is a 128×128 RGBA PNG with
binary alpha. It preserves `matured.png`'s upright posture, two arms, canvas
placement, lower trunk, and ground-contact row while replacing living green
with a blackened charcoal and ash-grey rib treatment.

The existing tan `dead.png` remains unchanged. `sprout_charred.png` is
deliberately absent: #2562 uses that sparse manifest to prove that
phase-appropriate `sprout_dead.png` outranks generic mature cause art. A
future exact sprout-charred texture can be added without changing the
fallback order.

## Owner approval, 2026-09-12

The owner inspected `charred.png` in a real, visible Synarchy
`--preview flora/saguaro` window with nearest-neighbour filtering and approved
the exact delivered bytes:

> it looks good, approved

The preview used port 9259 after `cabal build all` passed in the isolated
issue worktree. Its console dump reported `state: ready`, selected
`assets/textures/flora/saguaro/charred.png`, and loaded only that texture.
[The preview capture](preview.png) and
[the native/nearest-neighbour comparison](comparison.png) retain the visual
evidence.

## PixelLab provenance

The owner chose PixelLab and the landed `matured.png` as the base. PixelLab
`edit_image` job `609385be-8e2e-4ad0-aba8-efd57743c067`, seed `2596`, produced
one 128×128 transparent frame. [The exact request](request.json) records the
complete prompt and inputs. [The raw download](raw.png) and production
`charred.png` have the same SHA-256:

`c8ba06a360a106e251375c01f65b5e72a40ba48ef7434d5a3ee1c46947885374`.

No pixel post-processing was needed. Relative to `matured.png`, the output
adds no opaque pixels, removes only pixel `(79, 47)` (the permitted shortened
tip treatment), has an alpha-identical lower band from rows 97–127, and keeps
the same lowest opaque row, 124.

## Validation

Run from the repository root:

```bash
python3 docs/art/saguaro_charred/verify.py
python3 tools/texture_subset_audit.py
```

The verifier checks dimensions, RGBA mode, binary alpha, hashes, raw-output
identity, alpha subset, lower-band identity, and ground contact. The texture
subset audit passed all 13 subsets, but is a no-regression check only: the new
file remains intentionally undeclared until #2562 adds `textureVariants`.

`cabal build all` passed at
`e004672ae4986f77cad61e92c744bfa1545e411b`. No existing saguaro image or
`data/flora/saguaro.yaml` is changed.
