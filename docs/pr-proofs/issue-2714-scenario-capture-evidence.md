# Issue #2714 (SCN-14): evidence for scenario capture

Validation record for [coghex/synarchy#2714](https://github.com/coghex/synarchy/issues/2714),
branch `issue-2714-scenario-capture`. The contract is in
[engine_contracts.md §Scenario capture](../engine_contracts.md#scenario-capture-the-coherent-snapshot-and-the-safe-file-write-2714) and
[scenario_format.md](../scenario_format.md#capturing-a-scenario-session-scn-14-2714).
Production profile, `cabal-3.16.1.0`, no GPU, no game launched (only `--headless`
engines, on non-8008 ports, under the project's probe-slot grant).

## What was decided

- **A capture is a save-barrier transaction.** It reuses the barrier's owner
  quiescence, parking, capture lock and encode-before-release discipline instead
  of a parallel mechanism; admission is #2713's shared boundary
  (`claimCapture`, one action under the admission lock). No `EngineEnv` field and
  no saved component changed; the request record rides inside `LoadStatusRef`
  (Exclude/transient).
- **The issue's three Corrections were applied literally.**
  1. No-overwrite publication is an exclusive atomic primitive added to
     `World.Save.Storage.Durable` (`publishExclusive`, a hard link the
     filesystem refuses when the name is taken; plus `writeUniqueDurably`, a
     temporary owned from creation). Explicit overwrite is a rename. A filesystem
     without hard links answers `FailPublication`; the residual window is not
     claimed closed because it does not exist.
  2. The hspec group acts as the barrier owners against the production service
     (it withholds and releases stand-in acknowledgements through the
     `Engine.Save.Barrier` API); live multi-thread coordination is the probe's.
     `tools/save_barrier_probe.py` is run as the ordinary-barrier regression gate.
  3. The fresh-process load spawns the compiled `exe:synarchy --headless
     --port <non-8008>` and drives #2713's load service through the debug
     console. #2713 exposed no entry reachable from a fresh process, so this PR
     adds ONE internal, non-public set of verbs on the `debug` table
     (`debug.loadScenario`, `debug.captureScenario`,
     `debug.getScenarioLoadStatus`, `debug.getScenarioCaptureStatus`),
     documented as superseded by SCN-15. The exact invocation and the retained
     comparison output are below.
- **Spec additions applied:** `engine.setPaused(false)` is refused for a capture
  in flight (decided under the player-intent lock, like the load gate); explicit
  overwrite uses the same verified-candidate-then-atomic-replace sequence and keeps
  no `.prev`; the candidate is decoded through the shared scenario boundary
  (#2699), not `World.Save.Envelope`; storage phases are a scenario-specific
  enumeration mapped onto Durable's `WriteStep`, not `StoragePhase`; no
  `EngineEnv`/capability/manager field was added, so the ratchet self-tests had
  nothing new to count (they were run anyway).

## Hspec groups (final head)

| Group | Examples | Result |
|---|---|---|
| `Scenario.Capture` | 59 | pass |
| `Scenario.Load` | 50 | pass |
| `Scenario` (every SCN group) | 398 | pass |
| `Save.Snapshot` | 25 | pass |
| `save snapshot barrier` | 30 | pass |
| `atomic save storage` | 84 | pass |
| `Lua.PauseGate` | 8 | pass (3 new: unpause refused for a capture in flight, an ordinary save does not hold it, decided under the pause lock) |
| `transactional load` | 34 | pass (9 new: the capture record and admission) |
| `generated world library` | 49 | pass (the other user of the shared publication helper) |
| all `Lua` groups | 773 | pass |
| headless lane `world` (`--lane world`) | 763 (1 pending) | pass |
| headless lane `rest` (`--lane rest`) | 11426 | pass |

`Scenario.Capture` covers: the document encoder as a property over 500 generated
scenarios (decode of an encode is the scenario; every optional field, both region
shapes, every fluid kind, both map kinds are generated); a loaded scenario of every
family captured as a current-format document with bounds, camera and tags across
categories, a fixed point under capture → load → capture, byte-identical for two
captures; the camera where it stands (live when the arena owns it, the page's
otherwise); an edited scenario recaptured as its CURRENT values (a healed wound is
`[]`, hydration and calories changed, a half-full canteen, a near-broken dagger, an
emptied and an overloaded nested container) and loaded back as those values; a
manually edited arena (no scenario identity anywhere) named structurally and
stably; terrain/fluid/flora/structures/power/location contents edited through the
real owners (a wet cell gone dry, a removed structure, current flora age, drawn-down
power, a location emptied of its content) with no old override or duplicate
returning; pending containers kept pending; orders left out and untouched; content
the document cannot state refused with the finding named and nothing written; an
ordinary gameplay session and a session with no arena refused; the busy matrix (a
second capture, a scenario load, an ordinary load, an ordinary save, an autosave and
an unpause) held at the snapshot boundary, during storage and while still waiting
for the Lua thread, with the active status, the generic status, the barrier, the
queue counters and the pause unchanged and nothing queued; a capture refused while
a scenario load, an ordinary load or a save holds the line; pause at acceptance
before any coordination; stale commands for an ended or a never-existing request
ignored; a thread cancellation passed through instead of reported as a failed
capture; an item transfer across two owners captured whole (the barrier waits for
the owner mid-mutation, tag and identity intact), a simulation writeback in
flight captured, every owner alive and parked from its final acknowledgement to the
release (mutations counted) and running again after it, a write made after the
bytes were fixed kept out of the file; five deferred failures inside the capture
window (a hidden exception in the projection, in one element, in the bytes, after
the read and with the document complete), each ending before storage with the line
free, the owners running and a later request succeeding, plus a document that
decodes into something else and one the schema refuses, never published; creation
order never shows in the document; a new file published complete, an existing one
refused without overwrite, a file that appears between preflight and publication
never overwritten, an explicit overwrite replacing in one step with no backup, Save
As preserving the original and the source, and an injected failure at every storage
step (create, write, flush, read, wrong bytes, link, replace, directory sync, a
temporary that cannot be removed) with the destination bytes, the directory's
contents, the publication flag, the terminal result and the paused state asserted at
each; cleanup removing only the request's own temporary; the destination checks
(missing directory, a directory, a symlink, no file name); the exclusive primitive
refusing a taken name however it was taken; the route back through an ordinary save
(captured from the restored arena it is byte-identical to the live capture).

## Mutation checks

27 single-source mutants, each applied alone, the matching group run, the file
restored. Every one is killed by an assertion (never by a compile error or a timeout):

| Mutant | Effect | Group | Result |
|---|---|---|---|
| `exclusive-publish-is-a-rename` | the no-overwrite path publishes by rename (a check-then-replace) | `Scenario.Capture` | killed |
| `candidate-bytes-unchecked` | the candidate is not compared with the bytes written | `Scenario.Capture` | killed |
| `temp-not-removed` | a failed request leaves its temporary file | `Scenario.Capture` | killed |
| `cleanup-removes-the-destination` | cleanup removes the destination instead of the temporary | `Scenario.Capture` | killed |
| `sync-failure-reported-unpublished` | a directory-sync failure is reported as not published | `Scenario.Capture` | killed |
| `overwrite-intent-ignored` | every capture replaces an existing file | `Scenario.Capture` | killed |
| `preflight-overwrite-check-removed` | an existing destination is not refused in preflight | `Scenario.Capture` | killed |
| `lock-never-released` | the capture lock is never released before storage | `Scenario.Capture` | killed |
| `lock-released-before-encoding` | the lock is released before the document is encoded | `Scenario.Capture` | killed |
| `findings-dropped` | projection findings (unstatable content, unregistered definitions) are dropped | `Scenario.Capture` | killed |
| `live-camera-ignored` | the live camera is never read | `Scenario.Capture` | killed |
| `directory-sync-ignored` | a failed directory sync is ignored (success reported) | `Scenario.Capture` | killed |
| `publish-not-recorded` | the publication is not recorded the moment it happens | `Scenario.Capture` | killed |
| `stale-commands-run` | a command that outlived its transaction runs | `Scenario.Capture` | killed |
| `failure-leaves-transaction-open` | a failed capture leaves the barrier transaction open | `Scenario.Capture` | killed |
| `equality-not-verified` | the decoded candidate is not compared with the captured scenario | `Scenario.Capture` | killed |
| `live-camera-always` | the live camera is read even when the page is not on screen | `Scenario.Capture` | killed |
| `fluid-plane-rounded` | a fluid plane is written off the exact eighth | `Scenario.Capture` | killed |
| `camera-not-encoded` | the camera x is not written | `Scenario.Capture` | killed |
| `flora-age-not-encoded` | a flora entry's age is not written | `Scenario.Capture` | killed |
| `tile-lists-truncated` | a tile-list region is truncated to its first tile | `Scenario.Capture` | killed |
| `exclusive-primitive-clobbers` | the exclusive publish primitive reports success on a taken name | `Scenario.Capture` | killed |
| `claim-ignores-load` | a capture is admitted while a load is active | `transactional load` | killed |
| `in-progress-ignores-barrier` | a record the barrier moved past still holds the pause | `transactional load` | killed |
| `termination-unguarded` | a termination naming another request ends the current one | `transactional load` | killed |
| `cancellation-swallowed` | a thread cancellation is reported as an ordinary exception | `Scenario.Capture` | killed |
| `unpause-not-gated-by-capture` | an unpause is allowed while a capture is in flight | `Lua.PauseGate` | killed |

## Real engines: `tools/scenario_capture_probe.py`

Exact invocation (isolated resource root, port 9231, three sequential engines):

```sh
python3 tools/scenario_capture_probe.py --port 9231 --retain docs/pr-proofs/issue-2714-scenario-capture
```

Result at the final head: PASS, 42 checks, three sequential engines, all stopped by the probe. The full output is
[probe-output.txt](issue-2714-scenario-capture/probe-output.txt); the retained
documents are [source.yaml](issue-2714-scenario-capture/source.yaml) (handwritten),
[capture-1.yaml](issue-2714-scenario-capture/capture-1.yaml) (the live arena after a
console edit), [capture-2-restored.yaml](issue-2714-scenario-capture/capture-2-restored.yaml)
(a FRESH process that restored an ordinary save with no scenario file) and
[capture-3-fresh.yaml](issue-2714-scenario-capture/capture-3-fresh.yaml) (a THIRD
fresh process that loaded `capture-2` and captured it again). All three are
byte-identical:

```
capture-1 sha256: c7378aa48b92ee5cf386801d1a539ec038b046905685c0cbf7f3603b599a92e3
capture-2 sha256: c7378aa48b92ee5cf386801d1a539ec038b046905685c0cbf7f3603b599a92e3
capture-3 sha256: c7378aa48b92ee5cf386801d1a539ec038b046905685c0cbf7f3603b599a92e3
```

It proves, against real engines with every owner thread running: one console chunk
requests a capture and then, before the Lua thread can take its turn, makes an
ordinary save, an autosave, an ordinary load, a scenario load, another capture and
an unpause — each refused, the arena paused, and the capture then completes under
the SAME id; the capture records the CURRENT hydration (3, not the 4.0 that was
written) and the console-spawned ground item; an existing destination without
overwrite is refused and changes no byte, with overwrite it is replaced, and a
different name preserves the original; no temporary is left and the source scenario
is untouched; the restored arena and the arena loaded from the captured file both
capture to the same bytes.

## Other gates

- `python3 tools/save_barrier_probe.py --port 9143`: PASS (save owners acknowledged, post-release mutation isolated, later save captured its own boundary, a disk write failure surfaced as `SaveFailed` without wedging the barrier, the loaded session stayed paused).
- Static audits: every engine-free command of the CI `static-audits` job (85, hlint
  excluded) passes locally, among them `persistence_inventory_audit.py`,
  `engine_env_capability_audit.py` and their self-tests, `haskell_module_budget.py`,
  `lua_module_budget.py`, `unicode_operator_audit.py`, `tshow_spelling_audit.py`,
  `lua_registration_audit.py`, `cabal_module_audit.py`, `haddock_link_audit.py` and
  `ci_parity_audit.py`.
- Ordinary-save compatibility: no saved component, migration or fixture changed;
  the compiled-codec-only tools (`tools/save_compat_audit.py`,
  `tools/test_save_compat_audit.py`) start no engine and use no port, so they needed
  no probe slot: `save_compat_audit.py`: 46 baselines, 52 fixtures intact; `test_save_compat_audit.py`: 103 of 103 members pass.
- `cabal build all synarchy-test-headless synarchy-test-graphical` builds with
  `-Werror`.

## Limits stated, not hidden

- A LIVING unit whose captured resources are already fatal is written; whether a
  unit survives paused readiness is decided by Lua at load, so the loader (not the
  capture) rejects it with its own diagnostic.
- Terrain the vocabulary cannot state (a column with no solid cell, a tag on a cell
  below the surface) and objects of an unregistered definition fail the capture
  with one diagnostic per finding instead of being dropped.
- A filesystem without hard links cannot publish without overwrite
  (`FailPublication`); an explicit overwrite uses rename and is unaffected.
- Durability is plain POSIX `fsync` of the file and the directory, the boundary
  ordinary saves document; nothing stronger is claimed.
