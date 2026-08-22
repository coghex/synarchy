# Raw measurement data — probe concurrency characterization (#1427)

The complete, unedited evidence behind
[`docs/probe_concurrency_characterization.md`](../../probe_concurrency_characterization.md).
Read the report first; this directory exists so every table in it can be
re-derived and every non-pass attempt inspected.

**Tested commit:** `caaed17e40c3f2e274b34f68bb863a63a95ab51d`, clean
tree. Each `cohorts/<cell>/cohort.txt` re-records that independently.

## Layout

| path | what it is |
|---|---|
| `summary.json` | the aggregated matrix — one entry per cell, every attempt classified. This is the report's machine-readable form. |
| `cohorts/<cell>/inv<i>.json` | the raw `probe-flake-result/v1` document for one harness invocation, exactly as `probe_flake.py --result` wrote it. One file per invocation, never merged. |
| `cohorts/<cell>/invocation_launches.txt` | that cohort's captured launch progress, one `===== inv<i> =====` section per invocation: which run index each invocation started, in what order, and the port it leased |
| `cohorts/<cell>/cohort.txt` | the cohort's own record: probe, `--rts-caps`, requested concurrency, runs per invocation, repository path, commit, tree-clean flag, start and end timestamps |
| `cohorts/<cell>/exit_codes.txt` | every invocation's exit status |
| `artifacts/<invocation>__run-NNN.txt` | retained artifacts for one **unsuccessful** attempt, bundled into one file with `=====`-delimited sections: `events.jsonl` (the protocol stream), `stdout.txt`, and every `engine/*.log`. `probe_flake.py` deletes a successful run's directory, so every file here is a non-pass attempt. |

### What was packaged, and what was left out

The launcher wrote these as many more, smaller files. Two reductions were
applied before checking them in, both to keep the pull request inside the
review payload limit, and both stated here so the omissions are visible
rather than implied. **No result document was touched**: every
invocation's `inv<i>.json` is verbatim, in its own file.

1. **Retained artifact directories are concatenated** into one `.txt`
   per attempt, section headers intact. Inside them the protocol event
   stream and stdout are verbatim; the engine log has two classes of
   deterministic boot noise elided — `[INFO] [Asset]` / `[INFO] [Lua]`
   loading chatter, and the `[Init:158]` / `[Init:185]` tectonic and
   climate banner — with the elided count printed under each log. Every
   other line, of every level and subsystem, is kept verbatim and in
   order. Those two classes are byte-identical across runs of a probe
   and carry nothing about why an attempt failed; for a behavioural
   failure the diagnosis is the event stream, which names the failing
   check and its detail payload.
2. **The launcher's captured stdout is omitted**, and only its stderr —
   the per-invocation launch progress — is kept, as
   `invocation_launches.txt`. That stdout was the rendered per-check
   table `probe_flake.py` prints, every value of which is in the
   `inv<i>.json` beside it (`render()` derives it from exactly that
   document). Starts, exits and curtailed cells stay auditable per
   invocation without it: `cohort.txt` records the cohort's start and
   end, `exit_codes.txt` every invocation's exit status, and each
   `inv<i>.json` its own `requested_runs` against `completed_runs`.
| `primary_matrix_launcher.log`, `rts_subset_launcher.log` | the launchers' own output — cohort start/end wall-clock and per-invocation exit codes |
| `cohort.sh`, `primary.sh`, `rts.sh` | the exact scripts that produced all of this |
| `aggregate.py` | the classifier and summarizer that produced `summary.json` |

Cell names are `n<caps>-c<concurrency>-<probe>`; `n4-c1-role-retest` is
the drift control, an identical repeat of `n4-c1-role` run 1 h 42 m
later.

## Reading a raw result document

`artifact_dir` and `retained_artifacts` inside `inv<i>.json` are the
**absolute paths on the measuring host** (`~/probe-flake-1427-artifacts/…`)
and are preserved verbatim rather than rewritten. To find one here, take
the last two path components and join them with `__`:
`.../<invocation-dir>/run-NNN` maps to
`artifacts/<invocation-dir>__run-NNN.txt`.

`peak_concurrency` is per-invocation and sampled at three instants when
`requested_runs` is 1, so a single invocation can report less than its
cohort's true peak. `summary.json`'s `mean_achieved_parallelism` (total
probe run time ÷ cohort wall time) is the independent cross-check.

## Regenerating

```bash
# One cohort: <probe> <rts-caps> <concurrency> <total-attempts> <outdir> [repo-root]
bash docs/measurements/probe_concurrency_1427/cohort.sh \
     role 4 8 8 /tmp/probe-1427/n4-c8-role

# Whole matrices (build the executable once first — see the report's §6).
# The output root is REQUIRED and has no default.
cabal build exe:synarchy
bash docs/measurements/probe_concurrency_1427/primary.sh /tmp/probe-1427
bash docs/measurements/probe_concurrency_1427/rts.sh     /tmp/probe-1427

# Re-derive summary.json from the retained documents
python3 docs/measurements/probe_concurrency_1427/aggregate.py \
        docs/measurements/probe_concurrency_1427/cohorts
```

All three scripts **refuse an output directory inside any git working
tree**, judged on the deepest existing ancestor before anything is
created — so a rerun cannot land on top of this checked-in dataset and
leave `summary.json` describing evidence that no longer exists, and a
refused path leaves no stray directory behind. `probe_flake.py` applies
the same rule to its own artifact root; these scripts extend it to the
cohort directory the launcher owns. Retained probe artifacts go to
`<output-root>/probe-flake-artifacts`, so a rerun is self-contained.

The repository the probes run from defaults to the checkout these scripts
live in; pass it as the trailing argument to measure a different one. The
original run predates that derivation and used
`~/probe-flake-1427-artifacts` as its artifact root, which is why the
`artifact_root` and `artifact_dir` values recorded in the checked-in
documents name it.

Re-running `aggregate.py` over `cohorts/` here reproduces the committed
`summary.json` **byte for byte** — verified. One field is the exception in
principle: `infrastructure_signatures` is derived by scanning each non-pass
attempt's retained artifacts through the host-absolute `artifact_dir`
recorded in the raw document, which does not resolve on another machine.
It was empty for all 168 attempts of this run, so the regeneration is
identical anyway; on a run that had matched a signature it would come
back empty instead. Re-derive that one field by grepping `artifacts/`
directly; the bundles carry the same text the scan reads.

These are **historical measurement artifacts**, not a maintained tool. No
gate runs them, and they are not expected to be re-run on every change —
a later characterization gets its own output root, which is now enforced
rather than merely asked for.
