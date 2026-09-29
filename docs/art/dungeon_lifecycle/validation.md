# Validation — 2026-09-29

Validated in the isolated `foreground/issue-2513-dungeon-art` worktree on
base `a1a98d326e75891a5a6890a41774c2fbf60acbbc`, with the source images bound
by `manifest.json`. No engine or renderer implementation changed.

| Check | Result |
|---|---|
| `cabal build all` | Passed, production profile |
| `cabal build synarchy-test-headless` | Passed |
| Hspec `--match "structure construction frames"` | 91 examples, zero failures |
| Hspec `--match "Preview.StructurePack"` | 31 examples, zero failures |
| `python3 tools/dungeon_lifecycle_art.py` | All 14 selected states, six masks, YAML handoffs and final floor mask boundary pass |
| `python3 tools/texture_subset_audit.py` | All 13 subsets pass |
| `python3 tools/dungeon_lifecycle_capture.py` | All 12 pieces placed with expected texture/facemap palette paths; four distinct camera captures |
| `git diff --check` | Passed |

The shipped-pack Hspec exercises the real Lua loader and catalogue, resolves
every construction progress stage, verifies unique path identity for all six
new damage appearances, and checks that damage variants do not inherit
construction or receive a disappearing destruction clip. The preview group
checks the production manifest and its Lua boundary, including all 19
appearances and the two declared construction sequences.

After `cabal build all`, the required native window was launched from this
worktree with:

```sh
cabal run exe:synarchy -- --preview structures/dungeon_1 --port 9527
```

It reported `ready`, 19 appearances, zero missing textures, and zero missing
facemaps. The 36 undeclared lifecycle cells are intentional: 19 appearances
times two lifecycle types, minus the two new construction sequences.
The actual window's floor/post construction and ruined appearances were
selected through their reported UI bounds. `preview-state.json` records the
resulting paths, frame counts and alpha policy. Native captures:

- [Complete floor and appearance list](preview.png)
- [Floor construction](preview-floor-construction.png)
- [Post construction](preview-post.png)
- [Ruined floor](preview-floor-ruined.png)
- [Ruined post](preview-post-ruined.png)

The Vulkan offscreen arena captures were visually inspected at
[south](world/facesouth.png), [west](world/facewest.png),
[north](world/facenorth.png), and [east](world/faceeast.png). The floor remains
a recognizable tiled diamond at every damage level. Posts remain seated on
their floor corner; the broken and ruined post debris survives static
facemap clipping. Existing lighting and camera rotation apply normally.
The palette-path assertions are in `world/placed.json`; they establish
placement identity, not a new save/load or demolition behavior.

Owner approval was supplied for the exact source art in the requested HTML
report. The native and arena checks above are agent verification; no separate
owner verdict on those captures is claimed. The native preview is left open
for inspection. Full CI and unrelated probe suites were not run locally.
