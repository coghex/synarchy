# Wire art follows the camera: offscreen evidence (#2901)

Offscreen captures (`--offscreen`, no window) from
`python3 tools/wire_facing_capture.py --out docs/pr-proofs/wire-facing --port 9518`:
a three-tile straight run, an L corner and a tee, each at all four camera
facings (sun hour 12). Each `_x4.png` is a nearest-neighbour 4x crop centred on
the cable at that facing; `captures.json` lists every file.

**Result:** every scene reads as one continuous cable at every facing (open the
`_x4` crops). Before this change the same scenes drew disconnected segments at
west, north and east (#2516 checkpoint 3, finding 1).

The last two columns are read from the engine, not restated: the tile's STORED
world shape, and the facemap `structure.drawnFacemap` reports the render pass
lights it with, which names the variant DRAWN. The stored shape never changes;
the drawn one is the facing-rotated screen variant
(`Structure.Facing.screenWireShape`). The three-tile run is `end_e,
straight_ew, end_w` in the world and is drawn `end_n, straight_ns, end_s` at
`FaceWest`.

| scene | facing | files | stored (world) | drawn (screen) |
|---|---|---|---|---|
| straight | south | `straight_south.png` / `straight_south_x4.png` | `end_e`, `straight_ew`, `end_w` | `end_e`, `straight_ew`, `end_w` |
| straight | west | `straight_west.png` / `straight_west_x4.png` | `end_e`, `straight_ew`, `end_w` | `end_n`, `straight_ns`, `end_s` |
| straight | north | `straight_north.png` / `straight_north_x4.png` | `end_e`, `straight_ew`, `end_w` | `end_w`, `straight_ew`, `end_e` |
| straight | east | `straight_east.png` / `straight_east_x4.png` | `end_e`, `straight_ew`, `end_w` | `end_s`, `straight_ns`, `end_n` |
| corner | south | `corner_south.png` / `corner_south_x4.png` | `corner_se`, `end_w`, `end_n` | `corner_se`, `end_w`, `end_n` |
| corner | west | `corner_west.png` / `corner_west_x4.png` | `corner_se`, `end_w`, `end_n` | `corner_ne`, `end_s`, `end_w` |
| corner | north | `corner_north.png` / `corner_north_x4.png` | `corner_se`, `end_w`, `end_n` | `corner_nw`, `end_e`, `end_s` |
| corner | east | `corner_east.png` / `corner_east_x4.png` | `corner_se`, `end_w`, `end_n` | `corner_sw`, `end_n`, `end_e` |
| tee | south | `tee_south.png` / `tee_south_x4.png` | `tee_n`, `end_e`, `end_w`, `end_n` | `tee_n`, `end_e`, `end_w`, `end_n` |
| tee | west | `tee_west.png` / `tee_west_x4.png` | `tee_n`, `end_e`, `end_w`, `end_n` | `tee_w`, `end_n`, `end_s`, `end_w` |
| tee | north | `tee_north.png` / `tee_north_x4.png` | `tee_n`, `end_e`, `end_w`, `end_n` | `tee_s`, `end_w`, `end_e`, `end_s` |
| tee | east | `tee_east.png` / `tee_east_x4.png` | `tee_n`, `end_e`, `end_w`, `end_n` | `tee_e`, `end_s`, `end_n`, `end_e` |
