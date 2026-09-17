# Chunk memory evidence — issue #2625

See [the report](../../chunk_memory_measurement.md) for methods, conclusions and
owner disposition. This directory accompanies the instrumentation in the same
PR. No profiling coordinator registry, local branch state or game saves are
published.

- [raw.tar.gz](raw.tar.gz): 61 retained files, 834,694 compressed bytes.
  Archive SHA256: `f0669fdbc7523e7bfbb2fcc8b9d51e68b1e8f550b7a2b1da5ab7addfc03e75e5`.
- [summary.txt](summary.txt): all 14 complete cells, phase maxima, variation,
  query times, sampling gaps, field groups and source JSON hashes.
- [summarize.py](summarize.py): independent matrix validation and recomputation.
- [SHA256SUMS](SHA256SUMS): hashes of archive members, excluding the manifest itself.

Extract only into a new scratch directory, then recompute:

```sh
MEMORY_EVIDENCE="$(mktemp -d)"
tar -xzf docs/evidence/chunk_memory_2625/raw.tar.gz -C "$MEMORY_EVIDENCE"
(cd "$MEMORY_EVIDENCE" && shasum -a 256 -c SHA256SUMS)
python3 docs/evidence/chunk_memory_2625/summarize.py "$MEMORY_EVIDENCE"
```

The root `w*` directories are the 12 completed headless and two completed
offscreen cells. Each contains `measurement.json` and `engine.log.gz`; offscreen
also contains `vmmap-summary.txt`. `environment.json` records toolchain, build,
environment and the overlapping development activity. The headless source is
`7650862c6b9f48102e5ae17e036615e678cec9f3`, offscreen source
`b279123fb684795d1e8d6a48a5ea439e2fd6a057`; the binary SHA256 is identical:
`9bf5a5905dc1121125eb387b8020ba2b34d0c310055f0c6f3e0e69fd1e15d491`.

`failed/` preserves the generation-query parser failure and the offscreen
numeric-ID failure. `initial-diagnostic/` preserves the earlier banner-framing
failure and all three successful base-revision staircase samples, plus their
exact drivers and metadata. Resource-root copies and derived zoom caches are
omitted; raw measurements and logs are retained unchanged.

`validation/` includes the production build, six new Hspec cases, four existing
canonical/lifecycle regression groups (26 + 7 + 21 + 10 cases), owner-park group
(11 cases), inventory self-test (173 groups/260 assertions) and save-codec
reproducibility test (4 assertions). `regressions.log` also preserves an initial
owner-park invocation that could not start because an overlapping Cabal
configuration disabled tests; the serial rerun in `owner-park.log` passed.
The six Python driver checks pass separately. Applicable inventory, capability,
Lua-registration and Unicode audits passed. No full local CI was requested.
