# Paged world-map artifact format

This is the storage contract for a generated world's paged map (issue #2693,
epic #2017, design slice WML-7). It covers the mandatory root/coarse coverage
published in the shared generated-world library, and the optional fine pages
kept as cache data. The implementation is `World.ZoomMap.PagedArtifact`
(`Types`, `Png`, `Format`, `Store`). It is not wired into anything yet:
publication during world creation is WML-8, load recovery is WML-9, and the
runtime fine-page cache and its quota are WML-10. The separate monolithic
reconstruction cache, `World.ZoomMap.Artifact`, is a different format and is
unchanged.

Design authority: [world_map_level_of_detail_design.md](world_map_level_of_detail_design.md)
(D-5, D-7, D-8, D-12, D-16, D-17, D-18). Codec and quota evidence:
[world_map_page_codec_measurement.md](world_map_page_codec_measurement.md).

## What is stored

| Data | Levels | Where | Status |
|---|---|---|---|
| Manifest | — | library entry, payload `map.manifest` | mandatory |
| Mandatory pages | coarse cutoff through root, inclusive | library entry, one payload per page | mandatory |
| Fine pages | below the coarse cutoff | caller-chosen directory; not listed in the entry record | optional cache |

Levels and pages use the existing pyramid addressing
(`World.ZoomMap.Pyramid.Address`). Level 0 is the finest. The root is the
smallest level whose longest axis is at most 2048 texels. The coarse cutoff
is the smallest level whose longest axis is at most 4096 texels. Required
coverage is every page of every level `mapPyramidMandatoryLevels` returns:
cutoff through root, as `mapPyramidInventory` derives them from the world
size. Both caps bound the cutoff and root levels, so mandatory coverage never
has more than 8 × 8 + 4 × 4 pages, whatever the world size. An 8192 world has
40 mandatory pages, and its manifest is about 3.7 KB. Coverage is computed
by page-count arithmetic. Neither the reader nor the writer enumerates fine
pages, and no finest-level world raster is ever materialised.

Every page is 514 × 514 texels of RGBA8, which is 1,056,784 decoded bytes. That
is a 512-texel payload plus a one-texel gutter on every side, exactly as
`mapPageImage` produces it. The gutter, transparent texels, and the
transparent padding of partial-edge pages are all stored and restored
byte-for-byte.

### Root representation

The root level is stored only as its mandatory pages. There is no separate
whole-root payload. `reassembleMapRoot` recovers the complete root raster
(`mpiRootPlan`'s width × height) in three steps. It strips each root-level
page's one-texel gutter, tiles the 512-texel payloads by `(pageU, pageV)`,
and crops the partial-edge padding to the plan's width and height.

## Compatibility

The artifact records four compatibility fields, separately from the world's
`GeneratedWorldId` and from byte integrity. A reader computes the expected
value with `currentMapCompatibility`. That value is pure and does not depend
on any world, so WML-9 can compute it before opening anything. Each field is
compared by exact equality. A mismatch is refused as
`MapArtifactIncompatible <field> <expected> <found>`, never as corruption.

| Field | Source | Bump when |
|---|---|---|
| generator | `mcGenerator` in `World.ZoomMap.PagedArtifact.Types` | A world-generation output change moves page pixels. This is the same change that rebaselines `tools/baselines/` or the #2298 finest-page goldens. |
| content | `mcContent` | Authored content read by the page pixels changes meaningfully: material or vegetation definitions, or the zoom-map and vegetation textures the palette samples. |
| palette | `mcPalette` | The colour derivation changes: `World.ZoomMap.ColorPalette` or the per-chunk tile colouring. |
| map schema | `mcMapSchema` | Page semantics change: addressing, page geometry, the gutter rule, or the reduction in `World.ZoomMap.Pyramid`. |

All four fields are hand-bumped integers. None of them is a digest of build
inputs, so an ordinary rebuild does not invalidate persisted artifacts. A
wrong compatibility is therefore a missed bump in a change that alters
pixels. Under D-8, the recovery path turns a bump into regeneration, or into
a refused load when regeneration is not possible. Bumping a field
invalidates every persisted mandatory artifact produced under its previous
value.

Compatibility metadata is not an identity. It never contains, and is never
derived from, the `GeneratedWorldId`, a save-slot name, a runtime page id, or
a filesystem path. World identity is only the `GeneratedWorldId`. It is
recorded in the manifest and in every page file, and compared by equality
(`MapArtifactWrongWorld`).

The byte layout has its own version, `mapArtifactFormatVersion` (currently 1).
It is shared by the manifest and page files. Bump it for any layout change.
A pixel-semantic change is a compatibility bump instead. A file with the
right magic and a different version is refused as
`MapArtifactUnknownVersion`. That is distinct from damage and from absence.

## Byte layouts

All integers are big-endian and fixed-width. Both files are framed the same
way: an 8-byte magic, a `Word32` format version, the fields, and then a
trailing SHA-256 over every preceding byte. Identical inputs therefore
produce identical bytes.

### Page file (`map-page-lLL-uUUUUU-vVVVVV.mappage`)

| Offset | Bytes | Field |
|---|---|---|
| 0 | 8 | magic `SYNMAPPG` |
| 8 | 4 | format version |
| 12 | 16 | `GeneratedWorldId` |
| 28 | 16 | compatibility: generator, content, palette, map schema |
| 44 | 12 | key: level, page-u, page-v |
| 56 | 8 | width, height (514, 514) |
| 64 | 1 | encoding (1 = PNG, RGBA8) |
| 65 | 8 | PNG length |
| 73 | n | the PNG |
| 73 + n | 32 | SHA-256 of bytes 0 … 72 + n |

The file name is derived from the key by `mapPageFileName`. It is zero-padded
so that names sort like keys, and it is never taken from input. The PNG is
JuicyPixels' `encodePng` of the exact RGBA8 bytes. JuicyPixels is already a
library dependency, and no codec package was added. Encoding is deterministic
under the pinned package set; the golden example pins it.

Each page file carries its world, compatibility, and key. Fine pages need
that binding because no manifest lists them. Mandatory pages have it too, so
a page file moved between worlds, producers, or keys is refused on its own.

### Manifest (`map.manifest`)

| Field | Bytes |
|---|---|
| magic `SYNMAPMF`, format version, total file length (`Word64`) | 20 |
| `GeneratedWorldId` | 16 |
| compatibility | 16 |
| world size | 4 |
| encoding | 1 |
| page edge, payload, gutter, cutoff level, root level, root width, root height | 28 |
| level count, then per mandatory level: level, width, height, pages-u, pages-v | 4 + 20 per level |
| page count, then per page: level, page-u, page-v, name length (`Word16`), name, page-file length (`Word64`), page-file SHA-256 | 4 + 88 per page |
| SHA-256 trailer | 32 |

Page entries are in `MapPageKey`'s derived order, `(level, pageU, pageV)`. The
writer sorts them, so the order does not depend on how the pages were
supplied. The reader refuses out-of-order and repeated keys. Everything in
the geometry block and the level table is derived from the recorded world
size, and the reader checks it against that derivation
(`MapArtifactGeometryMismatch`). Those fields make the file self-describing
without making it a second source of truth.

## Integrity and the library

| Value | Covers | Checked by |
|---|---|---|
| page-file trailer | page header + PNG | every page read |
| manifest trailer | whole manifest | every manifest read |
| manifest page entry: length + SHA-256 | whole page file | mandatory reads, before the page is parsed |
| library `PayloadDescriptor`: size + SHA-256 | each whole payload file | the library's deep check; cross-checked by `openMapArtifact` |

The manifest's per-page digest is the same SHA-256 of the whole page file that
the generated-world library records in that payload's `PayloadDescriptor`.
`openMapArtifact` requires the entry record to list the manifest and every
manifest page, with the same size and digest. It also refuses any map-owned
file (`map.manifest` or `map-page-*`) that the record lists but the manifest
does not. Files owned by other slices, such as D-18's later base chunks, are
not the map's to judge. A page swapped behind a manifest rewritten to match
is refused as `MapArtifactSubstituted`, or as `MapArtifactEntryIncomplete`
when its size change already fails the library's own check.

Integrity-byte accounting:

- A page file costs 105 bytes beyond its PNG: a 73-byte header and a 32-byte
  trailer. This replaces the experimental 32-byte allowance used in the
  codec measurement.
- A manifest costs 20 + 69 + 20 × levels + 4 + 88 × pages + 32 bytes.
- The library's entry record adds its own descriptor per file.
- WML-10's fine-page quota should count the whole page-file length of each
  cached page.

## Bounds, checked before allocation or decode

- **Manifest:** at most `mapManifestMaxBytes` (64 KiB), checked against the
  file size before reading. The declared total length must match the actual
  length. The level count must be at most 64 and the page count at most
  `mapManifestMaxPages` (256); both are checked against the bytes remaining
  before any entry is decoded.
- **Page file:** at most `mapPageFileMaxBytes`. The declared PNG length is
  refused above `mapPagePngMaxBytes` (2 MiB) using only the 73-byte header.
  An incompressible page's PNG is about 1.06 MB, so 2 MiB leaves a wide
  margin.
- **File reads:** the size is checked first. The read then asks for at most
  one byte more than the bound, so a file that grows in between still cannot
  force a larger allocation. Symlinks are refused and never followed.
- **PNG gate:** before the native decoder sees a byte, the signature and IHDR
  must declare exactly the page plan's width and height. That plan comes from
  `planMapImage MapImageRGBA8 (TiledImageSource 1 mapPageEdge)`, the same plan
  the inventory prices. The header must also declare bit depth 8, colour
  type 6 (RGBA), deflate, adaptive filtering, and no interlace. Other pixel
  types are refused, never converted.
- **Native decode:** JuicyPixels runs inside an exception boundary with its
  output fully forced, because its zlib inflate can throw on a corrupt
  stream. Any exception becomes `MapArtifactDecodeFailure`. The decoded
  length is then checked against the plan.

## Required versus optional

**Mandatory coverage** consists of the manifest and every page from the
cutoff level through the root. `publishMapArtifact` accepts exactly that set,
as decoded RGBA8. It checks that the keys are valid, complete, and not
repeated, before any encoding. It encodes and binds every page, then checks
that each page file decodes back to the exact input bytes. It encodes the
manifest and decodes it back. Only after all of that does it call
`publishEntryWith`. The library's staging, verification, atomic rename,
recovery, and path-safety guarantees apply unchanged, and the library stays
payload-neutral.

`openMapArtifact` fully verifies the manifest and every mandatory page, one
page's pixels at a time. `readMandatoryPage` re-verifies and decodes a
single page. Missing or corrupt required data makes the artifact unusable,
and the result says why. Regeneration and load-session recovery are WML-9's.

**Fine pages** are written by `writeFinePage` and read by `readFinePage`, in a
directory the caller passes. Only a valid key below the cutoff is accepted.
Anything else is a caller error (`MapArtifactNotFineLevel`,
`MapArtifactInvalidKey`). A fine page is never listed in the library's entry
record, so it can never make an entry incomplete. The library ignores
unlisted files in an entry directory: it neither trusts nor removes them.
Every problem reading a fine page is a `FinePageMiss`, either
`FinePageAbsent` or `FinePageInvalid <refusal>`, and never a mandatory
failure.

Republishing an entry replaces its whole directory, and cleanup removes it,
unlisted fine pages included. That is an accepted cache-miss outcome. WML-10
owns the final fine-page directory, the 3 GiB quota with its 95/5 split, and
eviction. This slice activates none of them.

## Diagnostics

`MapArtifactRefusal` keeps each class separate, so a caller can decide by
pattern instead of by message text:

| Class | Constructors |
|---|---|
| absence | `MapArtifactAbsent` (no entry), `MapArtifactMissingRequired RequiredManifest` / `(RequiredPage key)` |
| unknown format | `MapArtifactUnknownVersion`, `MapArtifactNotRecognised` |
| wrong identity | `MapArtifactWrongWorld`, `MapArtifactIncompatible <field>` |
| damage | `MapArtifactTruncated`, `MapArtifactLengthMismatch`, `MapArtifactChecksumMismatch`, `MapArtifactDecodeFailure` |
| declarations | `MapArtifactOversized`, `MapArtifactPngHeader`, `MapArtifactPngDimensions`, `MapArtifactGeometry`, `MapArtifactGeometryMismatch`, `MapArtifactMalformed` |
| keys | `MapArtifactInvalidKey`, `MapArtifactDuplicateKey`, `MapArtifactUnorderedKeys`, `MapArtifactConflictingName`, `MapArtifactUnexpectedPage`, `MapArtifactPageBinding`, `MapArtifactNotFineLevel` |
| library agreement | `MapArtifactSubstituted`, `MapArtifactEntryIncomplete`, `MapArtifactLibrary` |
| environment | `MapArtifactIO`, `MapArtifactImage` |

`mapArtifactRefusalText` renders any refusal for a log.

## Focused gates

```bash
cabal test synarchy-test-headless --test-options='--match "paged map artifact"'
cabal test synarchy-test-headless --test-options='--match "generated world library"'
cabal test synarchy-test-headless --test-options='--match "map pyramid"'
cabal test synarchy-test-headless --test-options='--match "map image plan"'
cabal test synarchy-test-headless --test-options='--match "World.ZoomMap.Artifact"'
```

The `paged map artifact` group needs no engine, GPU, or world generation. It
pins the golden page and manifest digests for a fixed input. A change to the
layout, the ordering, or the codec output fails that example, and it should
land with a format-version bump. The `map pyramid` gate also selects the
existing #2298 finest-page goldens, which generate a size-64 world.
