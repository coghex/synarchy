# Issue #2654: blueprint footprint offscreen evidence

Run on 2026-11-01 with `python3 tools/construction_blueprint_footprint_probe.py --port 9420` from the issue worktree. The real offscreen render path completed with exit 0 on the owner's GPU laptop. Fixture identity: seed `0x0000002A`, world size 128, plate count 10, 1x1 anchor `(-5, 0)`, 2x3 anchor `(0, -5)`.

The probe's thresholded meaningful-pixel boxes were:

- 1x1 designation: `(504, 409, 520, 437)`, 16x28 pixels.
- 2x3 designation: `(463, 356, 556, 442)`, 93x86 pixels.
- 2x3 after staking: `(463, 356, 556, 442)`, 93x86 pixels.

The designation and stake-step screenshot pairs are retained below. The production helpers count and bound pixels with an absolute luma delta of at least 8, which removes the observed whole-frame flicker through 7 luma levels. The run passed the existing visibility, box equality, no-visible-jump, smaller-control, one-job, and anchor-only checks.

## Captures

| Capture | SHA-256 |
|---|---|
| [1x1 baseline](before-1x1.png) | `5c4e45e1d14c57f35d4bf7732e739633d48e0ad54d59c28b68cdf688964833fa` |
| [1x1 designation](after-1x1.png) | `3551f666eec42e891b0ffb9fc39af77a951d0bb0352b7e29a334b7a53267966d` |
| [2x3 baseline](before-2x3.png) | `342d41b6ad2b397a84254ea3e12afc2b3e67b63b0ed0ec48a1488686b7ae530f` |
| [2x3 designation](after-2x3.png) | `2b9ac1b3e6a9434a02c2c0ce359b1009cb4b9c14535da59eddd9552d90f62fb2` |
| [2x3 staked](after-stake-2x3.png) | `6706b7a1ae2f246e5ee771957e588555a36aa0d5d125f187f4144b062f672107` |
