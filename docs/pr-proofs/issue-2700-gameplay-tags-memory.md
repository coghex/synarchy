# #2700 — gameplay tag registry: retained-memory measurement

Evidence for requirement 10 of #2700. The contract this measures is in
[gameplay_tags.md](../gameplay_tags.md).

## Reproduce

```sh
cabal build all synarchy-test-headless
SYNARCHY_TAG_MEMORY=1 cabal test synarchy-test-headless \
  --test-options='--match "Gameplay.Tags memory" +RTS -T -RTS'
```

The group is `Test.Headless.Gameplay.TagsMemory`. Without
`SYNARCHY_TAG_MEMORY=1` it registers no examples, so the ordinary
`--match "Gameplay.Tags"` gate does not run it. `+RTS -T` enables
`GHC.Stats`. The headless suite is built with `-rtsopts` so the flag is
accepted. Its baked RTS options (`-N -A128M`) still apply.

## Build and host

- GHC 9.12.2, cabal-install 3.16.1.0, production profile: default
  flags, which gives `-O2 -optc-O3` from the `build-policy` stanza.
- macOS on Apple Silicon (aarch64-osx), 64-bit words.
- Branch `issue-2700-gameplay-tag-registry`, based on master
  `8069ef007`. Measured 2026-09-30.

## Method

1. Call `performMajorGC` twice and read
   `gcdetails_live_bytes` (all live heap data after that GC).
2. Build the value, fully force it with `Control.DeepSeq.force`, and
   `evaluate` it.
3. Collect and read again. The value stays referenced past the second
   read, so it is live during both collections.

The difference between the two reads is the heap the value retains.
Anything built inside step 2 that the value does not keep, such as
input lists or intermediate maps, is garbage by step 3 and does not
count. Values that were already live before step 1 are also excluded:
the shared tag pool, the universe in the query rows, and the populated
registry in the clear row.

## Workload

- **Per category**, n targets numbered 1..n, each with two tags drawn
  from a pool of 32 shared `GameplayTag` values: tags
  `i mod 32` and `(7i + 3) mod 32`. That gives 2n memberships over
  32 distinct tags.
- **Page-scoped keys** (locations, plants, tiles) all use one shared
  `WorldPageId` value. Tiles are laid out on a 512-wide grid at
  `z = 0`, with the identity canonicalisation (arena frame).
- **Registry-owned text.** The unit row at n = 100 000 is repeated with
  a freshly built `Text` for every tag of every assignment. Nothing
  else holds that text, so the registry pays for it.
- **Untagged universe.** One million units, of which 1000 are tagged.
  The run makes 100 repeated `queryCount` and `queryExists` calls with
  a complement and an any/none filter, then materializes the results.
- **Clear.** 100 000 tagged units, then `clearTags` on every one.

## Results

Noise floor: 1080 bytes. This is what the harness reports when the
measured value is `()`.

| Row | Memberships | Retained bytes | Bytes / membership |
|---|---:|---:|---:|
| empty registry | 0 | 1 080 (= noise floor) | — |
| units, n = 1 000 | 2 000 | 243 984 | 122.0 |
| units, n = 10 000 | 20 000 | 2 403 992 | 120.2 |
| units, n = 100 000 | 200 000 | 24 003 864 | 120.0 |
| buildings, n = 1 000 | 2 000 | 243 952 | 122.0 |
| buildings, n = 10 000 | 20 000 | 2 404 504 | 120.2 |
| buildings, n = 100 000 | 200 000 | 24 004 024 | 120.0 |
| locations, n = 1 000 | 2 000 | 260 176 | 130.1 |
| locations, n = 10 000 | 20 000 | 2 564 408 | 128.2 |
| locations, n = 100 000 | 200 000 | 25 604 272 | 128.0 |
| plants, n = 1 000 | 2 000 | 260 120 | 130.1 |
| plants, n = 10 000 | 20 000 | 2 563 992 | 128.2 |
| plants, n = 100 000 | 200 000 | 25 604 120 | 128.0 |
| items, n = 1 000 | 2 000 | 243 952 | 122.0 |
| items, n = 10 000 | 20 000 | 2 404 616 | 120.2 |
| items, n = 100 000 | 200 000 | 24 004 168 | 120.0 |
| tiles, n = 1 000 | 2 000 | 292 520 | 146.3 |
| tiles, n = 10 000 | 20 000 | 2 884 784 | 144.2 |
| tiles, n = 100 000 | 200 000 | 28 806 224 | 144.0 |
| units, n = 100 000, registry-owned tag text | 200 000 | 35 203 976 | 176.0 |

Both indexes are included in every row. Retained size is linear in
memberships: about 120 bytes each for single-word keys (units,
buildings, items), 128 for (page, id) keys, and 144 for
(page, x, y, z) tiles. Tag text that nothing else owns adds about
56 bytes per assignment (35.2 MB against 24.0 MB). That covers one
`Text` per tag per assignment, because the registry does not intern.
The forward set and the reverse key share whichever copy they are
given.

### Untagged universe, queries, and clear

| Row | Retained bytes | Owner |
|---|---:|---|
| universe: 1 000 000 units (`Set UnitId`) | 56 033 016 | caller |
| registry: 1 000 of those units tagged | 243 720 | registry |
| live growth across 100 `count` + 100 `exists` queries | 1 216 (noise) | — |
| native result set: complement, 999 938 matches | 16 640 | caller (temporary) |
| list result: complement, 999 938 targets | 39 998 456 | caller (temporary) |
| native result set: one tag, 62 matches | 2 448 | caller (temporary) |
| 100 000 tagged units, then every target cleared | 1 368 (noise) | registry |

The group also asserts the following:

- The registry that ranges over the million-unit universe has exactly
  1000 forward entries, and it is structurally equal to a freshly
  built 1000-unit registry after all the queries. Its 243 720 bytes
  match the plain 1000-unit row (243 984) to within noise. The
  999 000 untagged units created no registry entries.
- Repeated queries retain nothing. Live growth across 200 queries is
  within the noise floor, and `registryCounts` is unchanged.
- Clearing every tag gives back `emptyTagRegistry`: `nullTagRegistry`
  holds and every category's counts are zero. What it retains is
  indistinguishable from the empty registry.

## Reading the numbers

- **Fixed overhead.** The empty registry is below this harness's
  resolution. It is a statically allocated closure, and the reading
  equals the 1080-byte noise floor. A fixed overhead exists (six empty
  index pairs), but this measurement does not put a byte figure on it
  and does not claim that it is zero.
- **Complement results share the caller's universe.**
  `Data.Set.difference` reuses the universe's subtrees, so while the
  caller holds the universe, a near-total complement adds only about
  16 KB. If the caller drops the universe, the result owns those
  nodes. Up to the universe's size is needed temporarily.
- **Lists cost more than sets.** Materializing the 999 938-element list
  costs about 40 bytes per target (cons cell plus `TargetUnit` box).
  `queryCount` and `queryExists` never build it.
- **Broad queries are the caller's cost.** They need temporary storage
  proportional to their result, and this row set shows how large that
  is at one million.

## Limitations

- These are live-heap bytes after a major GC on a 64-bit build. RTS
  block slack, nursery, and fragmentation are not included, and RSS
  will be larger.
- The workload shape (two tags per target, 32 distinct tags, dense
  numeric keys) sets the constants. More tags per target raise the
  forward sets' share, and more distinct tags raise the reverse map's
  node count. The linear-in-memberships shape holds either way.
- Sharing depends on the caller. In the main rows, page ids and tag
  text were live before measurement, so they appear only as references.
  The registry-owned row shows the unshared bound for tag text. Keys do
  not include page-id text.
- Readings vary by a few hundred bytes between runs (compare the
  1080-byte floor). Rows below about 2 KB are noise.
- The measurement runs inside the headless test process under hspec.
  Nothing else runs concurrently in a `--match "Gameplay.Tags memory"`
  run, but hspec's own small allocations fall inside the noise floor.
