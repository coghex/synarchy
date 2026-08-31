# Project Review Findings: PRs #1547–#1535

This entry records the senior review of the next twelve merged PRs in merge-time order — #1547, #1546, #1536, #1545, #1544, #1543, #1542, #1541, #1540, #1538, #1537, and #1535 — plus direct first-parent commit `1657e834` (`docs: land pending design and findings updates`) in the same landing interval. The implementation, test-oracle, export-narrowing, CI, and playtest contracts remain coherent in the current tree and their focused checks passed. The direct commit's already-recorded shared-Cabal probe race was subsequently fixed by issue #1570 / PR #1630, so it is not a current finding. PR #1545's raw measurement corpus still reproduces exactly, but the tracked report draws a stronger failure-rate conclusion than its eight-attempt cells can establish; that current documentation defect is preserved below for later one-at-a-time disposition.

Status legend: `[ ]` unprocessed · `[#N]` filed as issue N · `[no-issue]` reviewed and deliberately never to be filed · `[deferred]` blocked on a concrete precondition

## Status

- [x] PRR-1. Probe concurrency report claims rate invariance its sample cannot establish — [#1994]

## 1. Probe-concurrency inference

### [#1994] PRR-1. Probe concurrency report claims rate invariance its sample cannot establish

> **Captured note:** Correct PR #1545's concurrency report so it distinguishes the observed absence of monotonic failure-rate degradation from proof that failure rates do not move. Eight attempts per cell and one repeated cell cannot establish rate invariance or a causal separation between probe flakiness and concurrency.

**Verification:** Verified against the current tracked report and its reproducible raw corpus. Re-running `aggregate.py` over the committed cohorts produced `summary.json` byte-identically. The report's own `-N4` table records `role` at 6/8, 7/8, 4/8, and 4/8 failures across C=1/2/4/8, while the sole repeated C=1 cell changes from 6/8 to 5/8. Wilson 95% intervals from those literal binomial counts are broad and overlapping: C1 0.409–0.929, C2 0.529–0.978, and C4/C8 0.215–0.785. That is consistent with no concurrency effect, but it does not prove equivalence; the single retest observes one difference rather than estimating a stable “±1 attempt” noise bound. The report nevertheless states rate invariance as a result and uses it as a premise that stability would permit concurrency 8.

**Evidence:**

- `docs/probe_concurrency_characterization.md:208-225` — the table visibly contains different observed rates, including `role`'s 7/8 at C=2 versus 4/8 at C=4/C=8, immediately followed by the categorical claim that rates “do not move with concurrency” and are properties of the probes.
- `docs/probe_concurrency_characterization.md:227-239` — one repeated C=1 cohort is said to “settle” the question and turn its one-attempt difference into a general ±1-attempt noise bound. One retest pair cannot estimate that bound, and even the observed C2→C4 difference is three attempts rather than one.
- `docs/probe_concurrency_characterization.md:468-471` — the recommendation treats “No failure rate moved” as established and concludes that stability alone permits C=8, making the overclaim part of the decision rationale rather than isolated wording.
- `docs/probe_concurrency_characterization.md:518-523` — the limitations section says n=8 is insufficient only below roughly 1/8. One attempt is the table's measurement granularity, not its statistical resolution; the current 95% intervals span roughly 0.38–0.57 probability points.
- `docs/measurements/probe_concurrency_1427/summary.json` — the canonical generated data retains the exact per-cell counts. `python3 docs/measurements/probe_concurrency_1427/aggregate.py docs/measurements/probe_concurrency_1427/cohorts | cmp -s - docs/measurements/probe_concurrency_1427/summary.json` exits 0, so the defect is in the interpretation, not transcription or aggregation.

**Handoff context:**

- **Current behavior:** The durable characterization says failure rates are invariant to concurrency and cites that assertion when allowing C=8 on stability grounds. Readers can reasonably treat that as measured equivalence even though the report collected only eight attempts per cell, scheduled the primary cells monotonically, and repeated just one cell once.
- **Expected behavior:** State the supported result narrowly: no monotonic concurrency-related degradation was observed in these three small cohorts, and the same recurring behavioral checks also failed at C=1. Treat failure-rate effects as unresolved unless a practical equivalence margin and enough randomized/repeated measurements are supplied. Keep observed counts, elapsed results, and raw evidence separate from that inference.
- **Scope and constraints:** Surfaced in PR #1545 / issue #1427. Preserve the committed raw corpus and byte-reproducible aggregation. No harness, probe, CI, RTS default, or game behavior change is implied. The recommended cap of 4 may remain supported by the report's independent elapsed-tail, throughput, and oversubscription evidence, but its stability premise must be weakened or re-measured.
- **Verification target:** Regenerate `summary.json` unchanged, then review the conclusion, recommendation rationale, and limits section together. They should no longer assert invariance or 1/8 inferential resolution. If a strong “rates do not move” result is retained, define a practically meaningful equivalence margin and add enough randomized/repeated cohorts for an equivalence analysis to support it.
- **Deduplication:** All-state tracker searches for probe-concurrency statistical power, invariant failure rates, and characterization overclaim found no owner. Open epic #1426 and closed issue #1427 own the lab and original measurement task, but neither tracks correction of this surviving inference.
- **Remaining uncertainty:** The review did not choose a domain-specific equivalence margin or rerun the three-hour experiment. The existing data are sufficient to prove the current categorical wording is unsupported and to support the weaker observational wording; stronger causal or equivalence claims require a newly specified measurement design.
