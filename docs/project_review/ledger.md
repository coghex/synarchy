# Project review ledger

Machine-owned state for the `project-review` workflow: one row per merged pull
request, per repository, with its title, when it merged, its status, the commit
a completed review verified it against, when that review completed, the report
it produced, and the evidence the row rests on. A checkmark means a clean review
against the commit beside it; `[legacy]` means coverage established by a
document that predates this ledger, with no date and no commit invented for it.
A title and a merge time are the listing's to supply, so a row that no merged-PR
listing has named yet carries neither rather than a guess.

Written by `project_review_ledger.py`. Edit it through that helper rather than
by hand: the payload below is parsed strictly, and an edit it cannot read stops
the next invocation instead of being ignored.

## coghex/synarchy

| PR | Title | Merged (UTC) | Status | Verified at | Completed (UTC) | Report | Evidence |
| ---: | --- | --- | --- | --- | --- | --- | --- |
| #2676 | Ignore treatment claims held by dead or collapsed medics (#2642) | 2026-09-21T13:57:41Z | ✓ clean | `7adcfa31c1e007b7c5409e9e3876b6f79d32911b` | 2026-09-22T15:04:04Z | — | — |
| #2675 | Add validated faction-tag definitions and legacy mappings (#2506) | 2026-09-21T11:56:00Z | never reviewed | — | — | — | — |
| #2674 | Add immediate structure teardown with transient destruction playback (#2491) | 2026-09-21T04:46:52Z | never reviewed | — | — | — | — |
| #2673 | Show portable containers in the container window (#2527) | 2026-09-21T02:11:53Z | never reviewed | — | — | — | — |
| #2672 | Make eighth-z fluid state exact, conserved, and durable (#2520) | 2026-09-20T20:29:47Z | never reviewed | — | — | — | — |
| #2671 | Add an identity-preserving ground-item move operation (#2486) | 2026-09-20T20:03:54Z | never reviewed | — | — | — | — |
| #2670 | Credit autonomous canteen drinking from the drain the engine actually applied (#2631) | 2026-09-20T15:07:13Z | never reviewed | — | — | — | — |
| #2669 | Decide starvation lean-floor death within a Float rounding tolerance | 2026-09-20T14:26:10Z | never reviewed | — | — | — | — |
| #2668 | Rebuild blood volume once bleeding is fully stabilized | 2026-09-20T01:33:20Z | never reviewed | — | — | — | — |
| #2667 | Commit a typed dropdown edit on focus loss instead of discarding it | 2026-09-19T13:25:26Z | never reviewed | — | — | — | — |
| #2666 | Rank only medics that can discover the patient they are ranked for | 2026-09-18T20:08:53Z | never reviewed | — | — | — | — |
| #2665 | Reject case-insensitive aliases of reserved generated-library file names | 2026-09-18T19:45:05Z | never reviewed | — | — | — | — |
| #2664 | Carry the resource tick's sub-binary32 remainder instead of dropping it | 2026-09-18T17:37:30Z | never reviewed | — | — | — | — |
| #2663 | Count only able-bodied workers toward building construction progress and recruitment | 2026-09-18T14:12:58Z | never reviewed | — | — | — | — |
| #2662 | Fetch antibiotics per medicine and bound the futile-cure loop | 2026-09-18T00:52:59Z | never reviewed | — | — | — | — |
| #2661 | Connect power networks across the cylindrical seam | 2026-09-18T00:29:23Z | never reviewed | — | — | — | — |
| #2660 | Give treatment a unique wound identity so same-time wounds are not mutated together | 2026-09-17T21:50:26Z | never reviewed | — | — | — | — |
| #2659 | Verify retained payload bytes before reusing a generated-library entry as unchanged | 2026-09-17T21:22:36Z | never reviewed | — | — | — | — |
| #2658 | Report failed saveLoaded teardown hooks in the load's reconciliation outcome | 2026-09-17T19:11:54Z | never reviewed | — | — | — | — |
| #2657 | Refuse pose transitions out of the terminal Dead pose at command execution | 2026-09-17T18:50:15Z | never reviewed | — | — | — | — |
| #2635 | Sync the list widget's scrollbar when its items are replaced | 2026-09-17T18:02:24Z | never reviewed | — | — | — | — |
| #2632 | Dismiss open dropdowns on Escape regardless of how many were ever created | 2026-09-17T17:28:15Z | never reviewed | — | — | — | — |
| #2626 | [CRS-2] Measure detailed-chunk memory and process high-water | 2026-09-17T16:31:15Z | never reviewed | — | — | — | — |
| #2624 | feat: spawn pending container shells from location content entries (#2505) | 2026-09-17T15:56:58Z | never reviewed | — | — | — | — |
| #2623 | feat: realize a loot profile deterministically into a container (#2502) | 2026-09-14T18:03:45Z | never reviewed | — | — | — | — |
| #2622 | Destroy units and ground items caught at a solidifying cell (#2490) | 2026-09-14T16:29:25Z | never reviewed | — | — | — | — |
| #2621 | [foraging] Preserve the edible target when harvesting a shared flora tile | 2026-09-13T16:15:52Z | never reviewed | — | — | — | — |
| #2620 | fix: bound and release the source-drink phase lock (#2545) | 2026-09-13T04:47:18Z | never reviewed | — | — | — | — |
| #2619 | docs: drop the obsolete arena save-test prohibition (#2569) | 2026-09-13T02:40:49Z | never reviewed | — | — | — | — |
| #2618 | farming: recheck proximity when resuming harvest-yield collection (#2550) | 2026-09-13T01:12:04Z | never reviewed | — | — | — | — |
| #2617 | survival: use the frame-based fat floor for organ failure (#2556) | 2026-09-12T21:23:02Z | never reviewed | — | — | — | — |
| #2616 | craft: plan ingredient sourcing per cycle, not per claim (#2524) | 2026-09-12T18:16:43Z | never reviewed | — | — | — | — |
| #2615 | survival: score water actions off the emptiest canteen, not the first (#2546) | 2026-09-12T17:56:08Z | never reviewed | — | — | — | — |
| #2614 | preview: resynchronize the audio pane on every catalog reload (#2611) | 2026-09-12T16:02:32Z | never reviewed | — | — | — | — |
| #2613 | mine: select the nearest WORKABLE designation (#2538) | 2026-09-12T15:29:23Z | never reviewed | — | — | — | — |
| #2612 | Add approved charred saguaro art | 2026-09-12T15:07:27Z | never reviewed | — | — | — | — |
| #2610 | chop: select the nearest CLAIMABLE designated tree (#2536) | 2026-09-12T13:25:12Z | never reviewed | — | — | — | — |
| #2609 | survival: restrict hydration recovery to actual source drinking (#2541) | 2026-09-12T05:14:12Z | never reviewed | — | — | — | — |
| #2608 | farm: select the nearest CLAIMABLE designation (#2534) | 2026-09-12T04:14:33Z | never reviewed | — | — | — | — |
| #2607 | Add approved saguaro juvenile living/dead pair | 2026-09-12T00:42:25Z | never reviewed | — | — | — | — |
| #2606 | Add engine audio and an interactive preview player | 2026-09-11T23:49:16Z | never reviewed | — | — | — | — |
| #2605 | Solidify the lava-water reaction product into durable stone through the world edit log | 2026-09-11T23:14:35Z | never reviewed | — | — | — | — |
| #2604 | Run the persistence-contract probe through the prebuilt decoder (#2274) | 2026-09-11T03:31:48Z | never reviewed | — | — | — | — |
| #2603 | Publish the canonical flora visual-state and fallback contract (#2530) | 2026-09-11T01:03:17Z | never reviewed | — | — | — | — |
| #2602 | Render flat fluid tops and give one-z drops a side face (#2517) | 2026-09-11T01:26:59Z | never reviewed | — | — | — | — |
| #2601 | Release ground-repair jobs when the worker no longer owns the target (#2531) | 2026-09-11T00:06:37Z | never reviewed | — | — | — | — |
| #2600 | Preserve combat stamina costs during physiology updates and evaluate exhaustion from committed values | 2026-09-10T23:31:42Z | never reviewed | — | — | — | — |
| #2599 | Retire page-owned units and buildings on single-page destroy and same-id re-init (#2476) | 2026-09-10T23:05:49Z | never reviewed | — | — | — | — |
| #2598 | Generate and approve eighth-level fluid masks (#2525) | 2026-09-10T16:46:39Z | never reviewed | — | — | — | — |
| #2597 | Honor station queue order when workers choose a bill (#2523) | 2026-09-10T15:56:28Z | never reviewed | — | — | — | — |
| #2595 | Measure world-map page codecs and bounded disk-cache tradeoffs | 2026-09-10T15:30:30Z | never reviewed | — | — | — | — |
| #2594 | Carry residual elapsed time across path waypoints (#2473) | 2026-09-10T14:35:05Z | never reviewed | — | — | — | — |
| #2593 | Fence in-flight fluid writebacks with the page's incarnation epoch | 2026-09-10T13:02:40Z | never reviewed | — | — | — | — |
| #2592 | Decide review-gate staleness by replaying the approved head (#2591) | 2026-09-09T22:29:01Z | never reviewed | — | — | — | — |
| #2590 | Make the building preview inspect every direction and lifecycle role (#2492) | 2026-09-09T18:21:33Z | never reviewed | — | — | — | — |
| #2589 | Measure Lua-to-Haskell calls with runtime-local telemetry | 2026-09-09T18:47:50Z | never reviewed | — | — | — | — |
| #2588 | Add the pure faction identity and relation policy model (#2500) | 2026-09-09T14:55:29Z | never reviewed | — | — | — | — |
| #2587 | Add the approved lantern item and sprite | 2026-09-09T14:29:01Z | never reviewed | — | — | — | — |
| #2586 | Render structure construction from authored progress frames (#2488) | 2026-09-09T11:57:00Z | never reviewed | — | — | — | — |
| #2585 | Persist player knowledge of portable containers (#2512) | 2026-09-09T04:33:51Z | never reviewed | — | — | — | — |
| #2584 | Load loot-profile definitions (#2499) | 2026-09-09T00:31:26Z | never reviewed | — | — | — | — |
| #2583 | Enforce capacity-safe, acyclic nested ownership moves | 2026-09-08T20:54:03Z | never reviewed | — | — | — | — |
| #2582 | Generate deterministic spatial map-pyramid pages from world-generation parameters | 2026-09-08T19:43:13Z | never reviewed | — | — | — | — |
| #2581 | Pilot a declarative registration contract on the UI namespace | 2026-09-08T17:30:45Z | never reviewed | — | — | — | — |
| #2580 | Retain sub-minute calendar progress across world ticks so the clock advances at default speed | 2026-09-08T13:23:44Z | never reviewed | — | — | — | — |
| #2579 | Resolve unlike-fluid contact by annihilation in every active-sim transfer path | 2026-09-08T14:43:30Z | never reviewed | — | — | — | — |
| #2578 | Decode save fixtures through a prebuilt executable instead of a cabal repl of the test suite | 2026-09-08T14:01:14Z | never reviewed | — | — | — | — |
| #2577 | Apply stance recovery atomically against the current stored value | 2026-09-08T06:19:32Z | never reviewed | — | — | — | — |
| #2576 | Fix instant-built seeding comment: SeedWhenBuilt, not SeedAtSpawn | 2026-09-08T06:01:51Z | never reviewed | — | — | — | — |
| #2575 | Fix circadian.getCircadianUrge doc comment call shape | 2026-09-08T05:46:24Z | never reviewed | — | — | — | — |
| #2574 | Preserve approved synthesized menu sound references | 2026-09-08T05:30:36Z | never reviewed | — | — | — | — |
| #2573 | Reduce normal startup delay with budgeted queue draining | 2026-09-08T05:06:45Z | never reviewed | — | — | — | — |
| #2572 | Migrate ten manual probes to the flake protocol | 2026-09-08T05:22:32Z | never reviewed | — | — | — | — |
| #2571 | docs: fix component-owner layering statement in Save.Component.Types | 2026-09-08T04:47:21Z | never reviewed | — | — | — | — |
| #2570 | Fix immediate-pause observation in orphan-prune probe | 2026-09-08T04:30:34Z | never reviewed | — | — | — | — |
| #2568 | docs: rewrite the Tier 3 damage derivation to the rotational swing and six-factor delivery | 2026-09-08T04:14:34Z | never reviewed | — | — | — | — |
| #2567 | docs: fix loadVegetationYamlFn call-site comment cardinality claim | 2026-09-08T03:59:27Z | never reviewed | — | — | — | — |
| #2566 | ci: retire the deleted fluid facade's three surviving references | 2026-09-08T03:43:07Z | never reviewed | — | — | — | — |
| #2565 | Relabel computeAmbientLight's curve to its own input convention | 2026-09-08T03:25:31Z | never reviewed | — | — | — | — |
| #2543 | docs: name every flattenItemInstances consumer without a stale count | 2026-09-08T03:06:45Z | [legacy] | — | — | [docs/project_review_2543-2119.md](../project_review_2543-2119.md) | cursor:docs/project_review_boundaries.md; report:docs/project_review_2543-2119.md (operator-confirmed) |
| #2542 | Fix inverted climateRegionSize comment, document minimumWorldSize divisibility | 2026-09-08T02:49:37Z | [legacy] | — | — | [docs/project_review_2542-2144.md](../project_review_2542-2144.md) | cursor:docs/project_review_boundaries.md; report:docs/project_review_2542-2144.md (operator-confirmed) |
| #2540 | docs: state text_wrap.lua's real surface in scripts/CLAUDE.md (#2306) | 2026-09-08T02:31:39Z | [legacy] | — | — | [docs/project_review_2542-2144.md](../project_review_2542-2144.md) | cursor:docs/project_review_boundaries.md; report:docs/project_review_2542-2144.md (operator-confirmed) |
| #2537 | Define the dormant coordinated simulation step protocol | 2026-09-08T02:15:48Z | [legacy] | — | — | [docs/project_review_2537-2196.md](../project_review_2537-2196.md) | cursor:docs/project_review_boundaries.md; report:docs/project_review_2537-2196.md (operator-confirmed) |
| #2532 | Extend the first-session tutorial through expedition completion | 2026-09-08T01:42:06Z | [legacy] | — | — | [docs/project_review_2532-2256.md](../project_review_2532-2256.md) | cursor:docs/project_review_boundaries.md; report:docs/project_review_2532-2256.md (operator-confirmed) |
| #2508 | preview: drop the dead `sortFrameFiles` re-export and name its one consumer | 2026-09-08T01:58:52Z | [legacy] | — | — | [docs/project_review_2508-2239.md](../project_review_2508-2239.md) | cursor:docs/project_review_boundaries.md; report:docs/project_review_2508-2239.md (operator-confirmed) |
| #2494 | Fix Blood.Pool's header to stop describing #884 as pending | 2026-09-08T00:51:18Z | [legacy] | — | — | [docs/project_review_2494-2313.md](../project_review_2494-2313.md) | cursor:docs/project_review_boundaries.md; report:docs/project_review_2494-2313.md (operator-confirmed) |
| #2493 | Correct stopWorkers's haddock: name the real invariant and all three callers | 2026-09-08T01:24:18Z | [legacy] | — | — | [docs/project_review_2493-2262.md](../project_review_2493-2262.md) | cursor:docs/project_review_boundaries.md; report:docs/project_review_2493-2262.md (operator-confirmed) |
| #2475 | Describe `Unit.Atlas.Digest`'s stream as the code hashes it: prefixed tag, then prefixed label and value per field | 2026-09-08T00:33:53Z | [legacy] | — | — | [docs/project_review_2475-2357.md](../project_review_2475-2357.md) | cursor:docs/project_review_boundaries.md; report:docs/project_review_2475-2357.md (operator-confirmed) |
| #2472 | Rewrite buildPreviewUnit's haddock to describe the atlas-first pipeline | 2026-09-08T00:04:45Z | [legacy] | — | — | [docs/project_review_2475-2357.md](../project_review_2475-2357.md) | cursor:docs/project_review_boundaries.md; report:docs/project_review_2475-2357.md (operator-confirmed) |
| #2469 | Correct `shutdownEngineWorkers`'s haddock: its non-fatal callers have a live logger | 2026-09-08T00:18:16Z | [legacy] | — | — | [docs/project_review_2475-2357.md](../project_review_2475-2357.md) | cursor:docs/project_review_boundaries.md; report:docs/project_review_2475-2357.md (operator-confirmed) |
| #2467 | Name all four `pickFrame` consumers in its haddock | 2026-09-07T22:43:29Z | [legacy] | — | — | — | cursor:docs/project_review_boundaries.md |
| #2466 | State `anySegmentIsSymlink`'s quantifier precisely: every level strictly below the root, never the root itself | 2026-09-07T22:18:20Z | [legacy] | — | — | [docs/project_review_2466-2453.md](../project_review_2466-2453.md) | cursor:docs/project_review_boundaries.md; report:docs/project_review_2466-2453.md (operator-confirmed) |
| #2465 | Fix Building.Render's stale sprite-height sort-key comment | 2026-09-07T22:00:34Z | [legacy] | — | — | [docs/project_review_2466-2453.md](../project_review_2466-2453.md) | cursor:docs/project_review_boundaries.md; report:docs/project_review_2466-2453.md (operator-confirmed) |
| #2464 | Point `isPointerSurfaceBlocked`'s two haddock references at `Engine.Input.Thread.Mouse` | 2026-09-07T23:38:00Z | [legacy] | — | — | — | cursor:docs/project_review_boundaries.md |
| #2463 | docs: correct Unit.HitTest's stale projection-mirroring claims | 2026-09-07T21:41:07Z | [legacy] | — | — | [docs/project_review_2466-2453.md](../project_review_2466-2453.md) | cursor:docs/project_review_boundaries.md; report:docs/project_review_2466-2453.md (operator-confirmed) |
| #2462 | docs: correct Engine.Core.Workers header's non-fatal caller description | 2026-09-07T23:22:37Z | [legacy] | — | — | [docs/project_review_2462-2405.md](../project_review_2462-2405.md) | cursor:docs/project_review_boundaries.md; report:docs/project_review_2462-2405.md (operator-confirmed) |
| #2461 | art: add approved umbrella thorn acacia textures | 2026-09-07T21:21:27Z | [legacy] | — | — | [docs/project_review_2466-2453.md](../project_review_2466-2453.md) | cursor:docs/project_review_boundaries.md; report:docs/project_review_2466-2453.md (operator-confirmed) |
| #2460 | refactor: make runPreview require its browsing state (#2208) | 2026-09-07T21:03:12Z | [legacy] | — | — | [docs/project_review_2466-2453.md](../project_review_2466-2453.md) | cursor:docs/project_review_boundaries.md; report:docs/project_review_2466-2453.md (operator-confirmed) |
| #2459 | docs: scope resolveTexture haddock to the T-pose mirror | 2026-09-07T20:42:32Z | [legacy] | — | — | [docs/project_review_2466-2453.md](../project_review_2466-2453.md) | cursor:docs/project_review_boundaries.md; report:docs/project_review_2466-2453.md (operator-confirmed) |
| #2458 | docs: state inputBoundaryPage's real (upLayer, upZIndex) tie-break | 2026-09-07T20:24:17Z | [legacy] | — | — | [docs/project_review_2466-2453.md](../project_review_2466-2453.md) | cursor:docs/project_review_boundaries.md; report:docs/project_review_2466-2453.md (operator-confirmed) |
| #2457 | docs: correct the buildings-viewer authority-split cross-reference | 2026-09-07T20:06:00Z | [legacy] | — | — | [docs/project_review_2466-2453.md](../project_review_2466-2453.md) | cursor:docs/project_review_boundaries.md; report:docs/project_review_2466-2453.md (operator-confirmed) |
| #2456 | docs: state the sim thread's real teardown dependency in `preRenderWorkers` | 2026-09-07T19:56:58Z | [legacy] | — | — | [docs/project_review_2466-2453.md](../project_review_2466-2453.md) | cursor:docs/project_review_boundaries.md; report:docs/project_review_2466-2453.md (operator-confirmed) |
| #2455 | docs: fix stale spriteRowSpan justification in unitToQuad's climb-occlusion comment | 2026-09-07T19:38:49Z | [legacy] | — | — | [docs/project_review_2466-2453.md](../project_review_2466-2453.md) | cursor:docs/project_review_boundaries.md; report:docs/project_review_2466-2453.md (operator-confirmed) |
| #2454 | refactor: stop logging a post-fork line for the four paired workers | 2026-09-07T19:17:01Z | [legacy] | — | — | [docs/project_review_2466-2453.md](../project_review_2466-2453.md) | cursor:docs/project_review_boundaries.md; report:docs/project_review_2466-2453.md (operator-confirmed) |
| #2453 | docs: point pose/activity mirror-field comments at their label functions | 2026-09-07T18:55:25Z | [legacy] | — | — | [docs/project_review_2466-2453.md](../project_review_2466-2453.md) | cursor:docs/project_review_boundaries.md; report:docs/project_review_2466-2453.md (operator-confirmed) |
| #2452 | fix: report worldgen parity diagnostic on failure instead of every run | 2026-09-07T18:33:01Z | [legacy] | — | — | — | cursor:docs/project_review_boundaries.md |
| #2451 | Give overdue Lua updates service during sustained message traffic | 2026-09-07T15:19:35Z | [legacy] | — | — | — | cursor:docs/project_review_boundaries.md |
| #2450 | docs: stop naming Combat among the always-ack save-barrier owners | 2026-09-07T14:45:50Z | [legacy] | — | — | — | cursor:docs/project_review_boundaries.md |
| #2449 | Remove dead AssetEvent type from Engine.Asset.Base | 2026-09-06T14:55:35Z | [legacy] | — | — | — | cursor:docs/project_review_boundaries.md |
| #2448 | Gate the claim owners' import direction, the command's seams and its entry point | 2026-09-06T15:17:13Z | [legacy] | — | — | — | cursor:docs/project_review_boundaries.md |
| #2447 | Correct the bindless-capacity comments that deny the effective limit the code enforces | 2026-09-06T13:17:32Z | [legacy] | — | — | — | cursor:docs/project_review_boundaries.md |
| #2446 | Remove the inert lcShowTimestamp and lcShowThreadId logger configuration fields | 2026-09-06T12:53:29Z | [legacy] | — | — | — | cursor:docs/project_review_boundaries.md |
| #2445 | Move Lua state-snapshot and per-action telemetry off the Info stream | 2026-09-06T09:21:30Z | [legacy] | — | — | — | cursor:docs/project_review_boundaries.md |
| #2444 | Split the de-flake deterministic gate along its three workflow owners | 2026-09-06T14:04:11Z | [legacy] | — | — | — | cursor:docs/project_review_boundaries.md |
| #2443 | Pin the treatment generator in the medical-kit spec's fixture reset | 2026-09-06T12:04:42Z | [legacy] | — | — | — | cursor:docs/project_review_boundaries.md |
| #2442 | Cover every Settings scale-change action with exact-once fan-out tests | 2026-09-06T12:28:16Z | [legacy] | — | — | — | cursor:docs/project_review_boundaries.md |
| #2441 | Document unit.getWounds's full 19-key schema and lock it with a key-set test | 2026-09-06T08:59:09Z | [legacy] | — | — | [docs/project_review_2441-2425.md](../project_review_2441-2425.md) | cursor:docs/project_review_boundaries.md; report:docs/project_review_2441-2425.md (operator-confirmed) |
| #2439 | Send the worldgen tectonic and climate banners to the generation log only | 2026-09-06T08:37:30Z | [legacy] | — | — | [docs/project_review_2441-2425.md](../project_review_2441-2425.md) | cursor:docs/project_review_boundaries.md; report:docs/project_review_2441-2425.md (operator-confirmed) |
| #2438 | Print only over-budget modules from the two module-budget guards | 2026-09-06T08:19:22Z | [legacy] | — | — | [docs/project_review_2441-2425.md](../project_review_2441-2425.md) | cursor:docs/project_review_boundaries.md; report:docs/project_review_2441-2425.md (operator-confirmed) |
| #2437 | Use a compact Hspec formatter for the headless suite in CI and make ci | 2026-09-06T08:03:16Z | [legacy] | — | — | [docs/project_review_2441-2425.md](../project_review_2441-2425.md) | cursor:docs/project_review_boundaries.md; report:docs/project_review_2441-2425.md (operator-confirmed) |
| #2436 | Extract the de-flake document contract behind the diagnosis façade | 2026-09-06T07:45:44Z | [legacy] | — | — | [docs/project_review_2441-2425.md](../project_review_2441-2425.md) | cursor:docs/project_review_boundaries.md; report:docs/project_review_2441-2425.md (operator-confirmed) |
| #2435 | Extract debug-console responsiveness behind the ResponsiveMenus façade | 2026-09-06T07:24:14Z | [legacy] | — | — | [docs/project_review_2441-2425.md](../project_review_2441-2425.md) | cursor:docs/project_review_boundaries.md; report:docs/project_review_2441-2425.md (operator-confirmed) |
| #2434 | Rebuild the etymology probe's HUD against its live render resources | 2026-09-06T07:06:20Z | [legacy] | — | — | [docs/project_review_2441-2425.md](../project_review_2441-2425.md) | cursor:docs/project_review_boundaries.md; report:docs/project_review_2441-2425.md (operator-confirmed) |
| #2433 | Give the chop probe a deterministic wood-harvestable target (#2058) | 2026-09-06T06:40:04Z | [legacy] | — | — | [docs/project_review_2441-2425.md](../project_review_2441-2425.md) | cursor:docs/project_review_boundaries.md; report:docs/project_review_2441-2425.md (operator-confirmed) |
| #2432 | Quiet the notification-registry success line, warn on an empty registry | 2026-09-06T06:25:20Z | [legacy] | — | — | [docs/project_review_2441-2425.md](../project_review_2441-2425.md) | cursor:docs/project_review_boundaries.md; report:docs/project_review_2441-2425.md (operator-confirmed) |
| #2431 | Keep the persistence sweep's failed checks inside the runner's retained output (#2060) | 2026-09-06T06:08:56Z | [legacy] | — | — | [docs/project_review_2441-2425.md](../project_review_2441-2425.md) | cursor:docs/project_review_boundaries.md; report:docs/project_review_2441-2425.md (operator-confirmed) |
| #2430 | Collapse routine Cabal build progress in CI and make ci (#1920) | 2026-09-06T05:45:53Z | [legacy] | — | — | [docs/project_review_2441-2425.md](../project_review_2441-2425.md) | cursor:docs/project_review_boundaries.md; report:docs/project_review_2441-2425.md (operator-confirmed) |
| #2429 | Correct the exhaustion and circadian comments that call #611/#612 future work | 2026-09-06T04:58:49Z | [legacy] | — | — | [docs/project_review_2429-2413.md](../project_review_2429-2413.md) | cursor:docs/project_review_boundaries.md; report:docs/project_review_2429-2413.md (operator-confirmed) |
| #2428 | Document brain.lua's real state_of_mind ownership instead of calling it read-only | 2026-09-06T03:40:11Z | [legacy] | — | — | [docs/project_review_2429-2413.md](../project_review_2429-2413.md) | cursor:docs/project_review_boundaries.md; report:docs/project_review_2429-2413.md (operator-confirmed) |
| #2427 | Enumerate all eleven transient unit-AI registries in the persistence inventory and reset-hook comment | 2026-09-06T02:56:28Z | [legacy] | — | — | [docs/project_review_2429-2413.md](../project_review_2429-2413.md) | cursor:docs/project_review_boundaries.md; report:docs/project_review_2429-2413.md (operator-confirmed) |
| #2426 | Correct Unit.Transfer's serializable-set count to seven | 2026-09-06T02:33:42Z | [legacy] | — | — | [docs/project_review_2429-2413.md](../project_review_2429-2413.md) | cursor:docs/project_review_boundaries.md; report:docs/project_review_2429-2413.md (operator-confirmed) |
| #2425 | Audit §2.1's capability-record sizes instead of hand-maintaining them | 2026-09-06T05:22:29Z | [legacy] | — | — | [docs/project_review_2441-2425.md](../project_review_2441-2425.md) | cursor:docs/project_review_boundaries.md; report:docs/project_review_2441-2425.md (operator-confirmed) |
| #2424 | Restore the #919 survival calibration run record as a linked docs/history archive | 2026-09-06T01:08:28Z | [legacy] | — | — | [docs/project_review_2429-2413.md](../project_review_2429-2413.md) | cursor:docs/project_review_boundaries.md; report:docs/project_review_2429-2413.md (operator-confirmed) |
| #2423 | Correct `World.River.Naming`'s write-once rationale to the post-#1868 append-only root placement | 2026-09-06T00:46:02Z | [legacy] | — | — | [docs/project_review_2429-2413.md](../project_review_2429-2413.md) | cursor:docs/project_review_boundaries.md; report:docs/project_review_2429-2413.md (operator-confirmed) |
| #2422 | Describe YamlVegetation's real input: a caller-enumerated data/vegetation/*.yaml directory | 2026-09-06T00:22:19Z | [legacy] | — | — | [docs/project_review_2429-2413.md](../project_review_2429-2413.md) | cursor:docs/project_review_boundaries.md; report:docs/project_review_2429-2413.md (operator-confirmed) |
| #2421 | Name all six publishGeneration inputs and the pre-write refusals in World.Save.Storage's header | 2026-09-05T23:00:54Z | [legacy] | — | — | [docs/project_review_2429-2413.md](../project_review_2429-2413.md) | cursor:docs/project_review_boundaries.md; report:docs/project_review_2429-2413.md (operator-confirmed) |
| #2420 | Print the enum audit's three coverage counts and stop engine_contracts.md hand-counting them | 2026-09-05T22:26:09Z | [legacy] | — | — | [docs/project_review_2429-2413.md](../project_review_2429-2413.md) | cursor:docs/project_review_boundaries.md; report:docs/project_review_2429-2413.md (operator-confirmed) |
| #2419 | Split the pure save-components gate along its four persistence owners (#2043) | 2026-09-05T22:11:26Z | [legacy] | — | — | [docs/project_review_2429-2413.md](../project_review_2429-2413.md) | cursor:docs/project_review_boundaries.md; report:docs/project_review_2429-2413.md (operator-confirmed) |
| #2418 | Describe the atlas budget as the generated-artifact check it is (#2217) | 2026-09-05T22:43:40Z | [legacy] | — | — | [docs/project_review_2429-2413.md](../project_review_2429-2413.md) | cursor:docs/project_review_boundaries.md; report:docs/project_review_2429-2413.md (operator-confirmed) |
| #2417 | Delete the two item-contents signature self-comparisons | 2026-09-05T21:26:11Z | [legacy] | — | — | [docs/project_review_2462-2405.md](../project_review_2462-2405.md) | cursor:docs/project_review_boundaries.md; report:docs/project_review_2462-2405.md (operator-confirmed) |
| #2416 | Split the item-list widget probe into one orchestrator and owner-scoped scenarios (#2046) | 2026-09-05T21:01:57Z | [legacy] | — | — | [docs/project_review_2462-2405.md](../project_review_2462-2405.md) | cursor:docs/project_review_boundaries.md; report:docs/project_review_2462-2405.md (operator-confirmed) |
| #2414 | Keep every lunge probe fixture on loaded arena terrain | 2026-09-05T20:13:24Z | [legacy] | — | — | [docs/project_review_2462-2405.md](../project_review_2462-2405.md) | cursor:docs/project_review_boundaries.md; report:docs/project_review_2462-2405.md (operator-confirmed) |
| #2413 | docs: streamline root agent guidance and retain critical context | 2026-09-05T21:45:44Z | [legacy] | — | — | [docs/project_review_2429-2413.md](../project_review_2429-2413.md) | cursor:docs/project_review_boundaries.md; report:docs/project_review_2429-2413.md (operator-confirmed) |
| #2412 | Tie the construction probe's progress oracle to the construct_job phase and capture state on every expired poll (#2172) | 2026-09-05T20:38:53Z | [legacy] | — | — | [docs/project_review_2462-2405.md](../project_review_2462-2405.md) | cursor:docs/project_review_boundaries.md; report:docs/project_review_2462-2405.md (operator-confirmed) |
| #2411 | Route every production show-to-Text wrapper through tshow, with a closed spelling guard (#2177) | 2026-09-05T19:48:28Z | [legacy] | — | — | [docs/project_review_2462-2405.md](../project_review_2462-2405.md) | cursor:docs/project_review_boundaries.md; report:docs/project_review_2462-2405.md (operator-confirmed) |
| #2410 | Move Lua module lifecycle and view narration from Info to Debug (#2174) | 2026-09-05T19:11:45Z | [legacy] | — | — | [docs/project_review_2462-2405.md](../project_review_2462-2405.md) | cursor:docs/project_review_boundaries.md; report:docs/project_review_2462-2405.md (operator-confirmed) |
| #2409 | Grade the replaced lunge's own cancellation, not the next launch (#2168) | 2026-09-05T16:21:05Z | [legacy] | — | — | [docs/project_review_2462-2405.md](../project_review_2462-2405.md) | cursor:docs/project_review_boundaries.md; report:docs/project_review_2462-2405.md (operator-confirmed) |
| #2408 | Dispatch parallel probes longest-expected-first (#2275) | 2026-09-05T15:57:39Z | [legacy] | — | — | [docs/project_review_2462-2405.md](../project_review_2462-2405.md) | cursor:docs/project_review_boundaries.md; report:docs/project_review_2462-2405.md (operator-confirmed) |
| #2407 | Seed the zoom coastal fill from ocean only, and make its extent independent of scan order (#2316) | 2026-09-05T15:34:06Z | [legacy] | — | — | [docs/project_review_2462-2405.md](../project_review_2462-2405.md) | cursor:docs/project_review_boundaries.md; report:docs/project_review_2462-2405.md (operator-confirmed) |
| #2406 | Pin the offscreen probe's fixture world and report every rejected portal candidate | 2026-09-05T13:26:45Z | [legacy] | — | — | [docs/project_review_2462-2405.md](../project_review_2462-2405.md) | cursor:docs/project_review_boundaries.md; report:docs/project_review_2462-2405.md (operator-confirmed) |
| #2405 | Give every headless temp fixture an invocation-owned directory through the harness primitive | 2026-09-05T13:06:21Z | [legacy] | — | — | [docs/project_review_2462-2405.md](../project_review_2462-2405.md) | cursor:docs/project_review_boundaries.md; report:docs/project_review_2462-2405.md (operator-confirmed) |
| #2403 | Author which harvest tags bypass the growth window and what a felled sprout or dead tree yields (#2212) | 2026-09-05T07:14:26Z | [legacy] | — | — | — | cursor:docs/project_review_boundaries.md |
| #2402 | Remove the write-only engine popup queue and assert popup pages on the delivery message (#2285) | 2026-09-05T12:42:56Z | [legacy] | — | — | — | cursor:docs/project_review_boundaries.md |
| #2401 | Split the capability-writer scanner along authority, syntax, projection, and scan owners (#2230) | 2026-09-05T06:45:47Z | [legacy] | — | — | — | cursor:docs/project_review_boundaries.md |
| #2400 | Ship every runtime and test resource in the source distribution and audit the manifest (#2175) | 2026-09-05T06:22:06Z | [legacy] | — | — | — | cursor:docs/project_review_boundaries.md |
| #2399 | Replace the false-green lua_strict_msg probe with a blocking strictness spec (#2161) | 2026-09-05T05:52:34Z | [legacy] | — | — | — | cursor:docs/project_review_boundaries.md |
| #2398 | Split the external-evidence self-test into identity, report, confinement and resilience owners (#2187) | 2026-09-05T04:29:48Z | [legacy] | — | — | — | cursor:docs/project_review_boundaries.md |
| #2397 | Split the manual gameplay runner into shared support and scenario owners (#2151) | 2026-09-05T03:27:21Z | [legacy] | — | — | — | cursor:docs/project_review_boundaries.md |
| #2396 | Split action-outcome coverage into an audit core, mutation corpus, and command façade | 2026-09-05T02:28:57Z | [legacy] | — | — | — | cursor:docs/project_review_boundaries.md |
| #2395 | Split the Tutorial HUD gate into lifecycle, presentation, scrolling, responsive, and caption owners | 2026-09-05T01:56:46Z | [legacy] | — | — | — | cursor:docs/project_review_boundaries.md |
| #2394 | Split the unit-atlas self-test along validation, compiler, and budget owners (#2061) | 2026-09-05T01:32:54Z | [legacy] | — | — | — | cursor:docs/project_review_boundaries.md |
| #2393 | Refuse a manual save whose name a legacy flat save occupies, and list one row per save name (#2335) | 2026-09-05T01:08:03Z | [legacy] | — | — | — | cursor:docs/project_review_boundaries.md |
| #2392 | Resolve medical kits by exact instance in supply discovery and fetch | 2026-09-05T00:43:19Z | [legacy] | — | — | — | cursor:docs/project_review_boundaries.md |
| #2391 | Canonicalize the world date at world.setDate and on load (#2339) | 2026-09-05T00:18:49Z | [legacy] | — | — | — | cursor:docs/project_review_boundaries.md |
| #2390 | Validate the ground item at item.select and gate the item Info callback's other-domain clears on its success | 2026-09-04T23:55:15Z | [legacy] | — | — | — | cursor:docs/project_review_boundaries.md |
| #2389 | Reject a malformed unit body_parts graph at the YAML boundary (#2348) | 2026-09-04T23:34:43Z | [legacy] | — | — | — | cursor:docs/project_review_boundaries.md |
| #2388 | Reject a flora file whose lifecycle, phase, cycle, or override token is present but unrecognized | 2026-09-04T23:07:55Z | [legacy] | — | — | — | cursor:docs/project_review_boundaries.md |
| #2387 | Require a finite positive fps — and an authorable domain for every other building number — at the YAML boundary | 2026-09-04T19:36:27Z | [legacy] | — | — | — | cursor:docs/project_review_boundaries.md |
| #2386 | Refuse non-finite camera coordinates at `camera.move`/`camera.setPosition`, and default a saved camera that carries one | 2026-09-04T17:45:45Z | [legacy] | — | — | — | cursor:docs/project_review_boundaries.md |
| #2385 | Reject an out-of-domain infection file at the authoring boundary | 2026-09-04T18:53:14Z | [legacy] | — | — | — | cursor:docs/project_review_boundaries.md |
| #2384 | Reconcile saved equipment slot keys against the current equipment class at staging | 2026-09-04T21:38:06Z | [legacy] | — | — | — | cursor:docs/project_review_boundaries.md |
| #2383 | Bound the craft and construct work clocks at every swallowed tick and unannounced gap | 2026-09-04T16:26:06Z | [legacy] | — | — | — | cursor:docs/project_review_boundaries.md |
| #2382 | Give a spillway tile shared by two lakes both source identities | 2026-09-04T15:18:27Z | [legacy] | — | — | — | cursor:docs/project_review_boundaries.md |
| #2380 | Refuse non-finite or out-of-domain arguments at world.digTile and keep a restored mine designation finite | 2026-09-04T15:54:28Z | [legacy] | — | — | — | cursor:docs/project_review_boundaries.md |
| #2379 | Contain a failing generation read to its own slot in listSaves | 2026-09-04T12:54:49Z | [legacy] | — | — | — | cursor:docs/project_review_boundaries.md |
| #2378 | Refuse non-finite coordinates and decal geometry at the three spawn boundaries | 2026-09-04T12:30:04Z | [legacy] | — | — | — | cursor:docs/project_review_boundaries.md |
| #2377 | Revalidate page, reach and stance in combat resolution before the strike commits | 2026-09-04T14:08:58Z | [legacy] | — | — | — | cursor:docs/project_review_boundaries.md |
| #2376 | Bound the dump's fast-settle wait and make the settle outcome-bearing | 2026-09-04T13:42:38Z | [legacy] | — | — | — | cursor:docs/project_review_boundaries.md |
| #2374 | Make the building commit the authority on footprint exclusivity (#2326) | 2026-09-04T13:15:19Z | [legacy] | — | — | — | cursor:docs/project_review_boundaries.md |
| #2373 | Resolve AI craft-bill mutations on the acting unit's own page, not the active one | 2026-09-04T05:59:20Z | [legacy] | — | — | — | cursor:docs/project_review_boundaries.md |
| #2372 | Split page-scoped persistence into world-pages, world-edits and world-activity owners (#2135) | 2026-09-04T05:09:08Z | [legacy] | — | — | — | cursor:docs/project_review_boundaries.md |
| #2370 | Split the per-probe claim command along storage, lease, and orchestration owners | 2026-09-04T04:43:44Z | [legacy] | — | — | — | cursor:docs/project_review_boundaries.md |
| #2369 | Scrub orphan acquired-immunity entries when a save is staged (#2305) | 2026-09-04T04:08:28Z | [legacy] | — | — | — | cursor:docs/project_review_boundaries.md |
| #2368 | Bind bulk chunk work to the page its caller chose | 2026-09-04T03:44:49Z | [legacy] | — | — | — | cursor:docs/project_review_boundaries.md |
| #2367 | Split the Lua persistence-component gate along its four contract owners | 2026-09-04T03:24:05Z | [legacy] | — | — | — | cursor:docs/project_review_boundaries.md |
| #2366 | Require the same page and the AI treatment range at the medical treatment verbs | 2026-09-04T05:36:25Z | [legacy] | — | — | — | cursor:docs/project_review_boundaries.md |
| #2365 | Split the flake self-test into harness and per-probe migration owners | 2026-09-04T02:59:50Z | [legacy] | — | — | [docs/project_review_2475-2357.md](../project_review_2475-2357.md) | cursor:docs/project_review_boundaries.md; report:docs/project_review_2475-2357.md (operator-confirmed) |
| #2364 | Extract embark-to-discovery phase owners behind its single probe façade | 2026-09-04T02:38:50Z | [legacy] | — | — | [docs/project_review_2475-2357.md](../project_review_2475-2357.md) | cursor:docs/project_review_boundaries.md; report:docs/project_review_2475-2357.md (operator-confirmed) |
| #2363 | Gate autonomous harvest pickup on carrying capacity (#2293) | 2026-09-04T02:17:50Z | [legacy] | — | — | [docs/project_review_2475-2357.md](../project_review_2475-2357.md) | cursor:docs/project_review_boundaries.md; report:docs/project_review_2475-2357.md (operator-confirmed) |
| #2362 | Split the Item List widget gate along model, row, invalidation, and tabbar owners | 2026-09-04T01:30:14Z | [legacy] | — | — | [docs/project_review_2475-2357.md](../project_review_2475-2357.md) | cursor:docs/project_review_boundaries.md; report:docs/project_review_2475-2357.md (operator-confirmed) |
| #2361 | Split production-defect issue publication along evidence, document, tracker, and census owners | 2026-09-04T00:02:17Z | [legacy] | — | — | [docs/project_review_2475-2357.md](../project_review_2475-2357.md) | cursor:docs/project_review_boundaries.md; report:docs/project_review_2475-2357.md (operator-confirmed) |
| #2360 | Reset the session-owned event store and game-clock epoch when Exit to Menu destroys every world | 2026-09-04T00:33:42Z | [legacy] | — | — | [docs/project_review_2475-2357.md](../project_review_2475-2357.md) | cursor:docs/project_review_boundaries.md; report:docs/project_review_2475-2357.md (operator-confirmed) |
| #2359 | Refuse non-finite or missing coordinates and speeds at the unit motion verbs, and a non-positive max_speed at the decoder | 2026-09-04T01:56:24Z | [legacy] | — | — | [docs/project_review_2475-2357.md](../project_review_2475-2357.md) | cursor:docs/project_review_boundaries.md; report:docs/project_review_2475-2357.md (operator-confirmed) |
| #2358 | Add a baseline-ratcheted audit for dead qualified Haddock links | 2026-09-03T23:39:49Z | [legacy] | — | — | [docs/project_review_2475-2357.md](../project_review_2475-2357.md) | cursor:docs/project_review_boundaries.md; report:docs/project_review_2475-2357.md (operator-confirmed) |
| #2357 | Split the container-window manager along endpoint, pane-rendering, and stack-lifecycle owners | 2026-09-03T23:19:00Z | [legacy] | — | — | [docs/project_review_2475-2357.md](../project_review_2475-2357.md) | cursor:docs/project_review_boundaries.md; report:docs/project_review_2475-2357.md (operator-confirmed) |
| #2356 | Extract tutorial progression and sticky-presentation owners behind its four-engine façade (#2145) | 2026-09-03T22:36:30Z | [legacy] | — | — | — | cursor:docs/project_review_boundaries.md |
| #2355 | Validate world-generation float settings at one shared domain (#2288) | 2026-09-03T22:57:44Z | [legacy] | — | — | — | cursor:docs/project_review_boundaries.md |
| #2354 | Cancel a timed-out debug command before it is claimed, and report an unknown outcome once it has started | 2026-09-03T21:16:40Z | [legacy] | — | — | — | cursor:docs/project_review_boundaries.md |
| #2353 | Report CI lane and step timings from one command (#2277) | 2026-09-03T20:58:38Z | [legacy] | — | — | — | cursor:docs/project_review_boundaries.md |
| #2352 | Write the fail-stop lifecycle transition before any crash reporting | 2026-09-03T20:37:31Z | [legacy] | — | — | — | cursor:docs/project_review_boundaries.md |
| #2351 | Merge live material registrations into normal world initialization instead of replacing the registry | 2026-09-03T20:15:03Z | [legacy] | — | — | — | cursor:docs/project_review_boundaries.md |
| #2350 | Cap the action-outcome ring and append through one shared helper | 2026-09-03T19:54:49Z | [legacy] | — | — | — | cursor:docs/project_review_boundaries.md |
| #2349 | Warn with the file and the decoder error when the worldgen config is malformed (#2286) | 2026-09-03T19:39:42Z | [legacy] | — | — | — | cursor:docs/project_review_boundaries.md |
| #2345 | Persist flora species references by authored name (#2243) | 2026-09-03T19:11:02Z | [legacy] | — | — | — | cursor:docs/project_review_boundaries.md |
| #2344 | Refuse unsafe time scales at world.setTimeScale and make the world clock total (#2280) | 2026-09-03T18:43:46Z | [legacy] | — | — | — | cursor:docs/project_review_boundaries.md |
| #2343 | Split the remaining probe-census core along contract, records, summary, storage, and CLI owners (#2131) | 2026-09-03T18:23:55Z | [legacy] | — | — | — | cursor:docs/project_review_boundaries.md |
| #2342 | Split the build-placement page-binding gate into four owners (#2173) | 2026-09-03T18:02:41Z | [legacy] | — | — | — | cursor:docs/project_review_boundaries.md |
| #2341 | Make preview probe families independently runnable behind one aggregate gate (#2089) | 2026-09-03T17:39:01Z | [legacy] | — | — | [docs/project_review_2494-2313.md](../project_review_2494-2313.md) | cursor:docs/project_review_boundaries.md; report:docs/project_review_2494-2313.md (operator-confirmed) |
| #2340 | Run the engine-free CI audits in a job that does not wait for the Cabal build (#2272) | 2026-09-03T17:16:37Z | [legacy] | — | — | [docs/project_review_2494-2313.md](../project_review_2494-2313.md) | cursor:docs/project_review_boundaries.md; report:docs/project_review_2494-2313.md (operator-confirmed) |
| #2331 | Split CI parity auditing into layer owners (#2159) | 2026-09-03T14:00:38Z | [legacy] | — | — | [docs/project_review_2494-2313.md](../project_review_2494-2313.md) | cursor:docs/project_review_boundaries.md; report:docs/project_review_2494-2313.md (operator-confirmed) |
| #2322 | Register and place flora independently of enumeration order | 2026-09-03T08:39:59Z | [legacy] | — | — | [docs/project_review_2494-2313.md](../project_review_2494-2313.md) | cursor:docs/project_review_boundaries.md; report:docs/project_review_2494-2313.md (operator-confirmed) |
| #2321 | Split the remaining probe-runner self-test along its five production owners | 2026-09-03T08:12:27Z | [legacy] | — | — | [docs/project_review_2494-2313.md](../project_review_2494-2313.md) | cursor:docs/project_review_boundaries.md; report:docs/project_review_2494-2313.md (operator-confirmed) |
| #2320 | Split the pure unit-atlas gate along index, freshness, and consumer owners | 2026-09-03T07:54:26Z | [legacy] | — | — | [docs/project_review_2494-2313.md](../project_review_2494-2313.md) | cursor:docs/project_review_boundaries.md; report:docs/project_review_2494-2313.md (operator-confirmed) |
| #2319 | Split the remaining EngineEnv audit implementation along inventory and boundary owners | 2026-09-03T07:32:17Z | [legacy] | — | — | [docs/project_review_2494-2313.md](../project_review_2494-2313.md) | cursor:docs/project_review_boundaries.md; report:docs/project_review_2494-2313.md (operator-confirmed) |
| #2318 | Split the de-flake handoff contract into four internal owners (#2180) | 2026-09-03T07:13:52Z | [legacy] | — | — | [docs/project_review_2494-2313.md](../project_review_2494-2313.md) | cursor:docs/project_review_boundaries.md; report:docs/project_review_2494-2313.md (operator-confirmed) |
| #2313 | Split the probe-census self-test along its five remaining owners (#2129) | 2026-09-03T06:57:05Z | [legacy] | — | — | [docs/project_review_2494-2313.md](../project_review_2494-2313.md) | cursor:docs/project_review_boundaries.md; report:docs/project_review_2494-2313.md (operator-confirmed) |
| #2312 | Sync the owning saves/ directory on slot creation and autosave rotation | 2026-09-03T13:01:54Z | [legacy] | — | — | [docs/project_review_2494-2313.md](../project_review_2494-2313.md) | cursor:docs/project_review_boundaries.md; report:docs/project_review_2494-2313.md (operator-confirmed) |
| #2309 | Park every save-barrier owner after its final-pass acknowledgement until the capture lock releases | 2026-09-03T13:31:44Z | [legacy] | — | — | [docs/project_review_2494-2313.md](../project_review_2494-2313.md) | cursor:docs/project_review_boundaries.md; report:docs/project_review_2494-2313.md (operator-confirmed) |
| #2296 | Split the capability-writer self-test along map, scanner, projection, and conformance owners | 2026-09-03T06:38:55Z | [legacy] | — | — | [docs/project_review_2493-2262.md](../project_review_2493-2262.md) | cursor:docs/project_review_boundaries.md; report:docs/project_review_2493-2262.md (operator-confirmed) |
| #2295 | Split the world-audit tool along column, boundary, region, soil, and policy owners (#2224) | 2026-09-03T06:20:00Z | [legacy] | — | — | [docs/project_review_2493-2262.md](../project_review_2493-2262.md) | cursor:docs/project_review_boundaries.md; report:docs/project_review_2493-2262.md (operator-confirmed) |
| #2289 | Rebase critic report image links onto the report's own directory (#2220) | 2026-09-03T05:54:51Z | [legacy] | — | — | [docs/project_review_2493-2262.md](../project_review_2493-2262.md) | cursor:docs/project_review_boundaries.md; report:docs/project_review_2493-2262.md (operator-confirmed) |
| #2287 | Admit the synchronously generated centre chunk and arena chunks to fluid simulation at init | 2026-09-03T05:28:38Z | [legacy] | — | — | [docs/project_review_2493-2262.md](../project_review_2493-2262.md) | cursor:docs/project_review_boundaries.md; report:docs/project_review_2493-2262.md (operator-confirmed) |
| #2281 | Fail the save publish preflight closed when an existing generation cannot be read (#2227) | 2026-09-03T04:58:47Z | [legacy] | — | — | [docs/project_review_2493-2262.md](../project_review_2493-2262.md) | cursor:docs/project_review_boundaries.md; report:docs/project_review_2493-2262.md (operator-confirmed) |
| #2279 | Split entity persistence along its three contract owners (#2150) | 2026-09-03T04:31:50Z | [legacy] | — | — | [docs/project_review_2493-2262.md](../project_review_2493-2262.md) | cursor:docs/project_review_boundaries.md; report:docs/project_review_2493-2262.md (operator-confirmed) |
| #2271 | Route every config/*.local.yaml write through one atomic-replace helper (#2202) | 2026-09-03T03:32:05Z | [legacy] | — | — | [docs/project_review_2493-2262.md](../project_review_2493-2262.md) | cursor:docs/project_review_boundaries.md; report:docs/project_review_2493-2262.md (operator-confirmed) |
| #2270 | Split the persistence-inventory self-test along Haskell, Lua, inventory, reference, and topology owners | 2026-09-03T03:09:44Z | [legacy] | — | — | [docs/project_review_2493-2262.md](../project_review_2493-2262.md) | cursor:docs/project_review_boundaries.md; report:docs/project_review_2493-2262.md (operator-confirmed) |
| #2268 | Split the save-compatibility self-test along its production owners (#2073) | 2026-09-03T02:50:22Z | [legacy] | — | — | [docs/project_review_2493-2262.md](../project_review_2493-2262.md) | cursor:docs/project_review_boundaries.md; report:docs/project_review_2493-2262.md (operator-confirmed) |
| #2267 | Give each Mode A transfer-session contract its own spec owner (#2090) | 2026-09-03T02:00:54Z | [legacy] | — | — | [docs/project_review_2532-2256.md](../project_review_2532-2256.md) | cursor:docs/project_review_boundaries.md; report:docs/project_review_2532-2256.md (operator-confirmed) |
| #2266 | Report a legacy-config copy failure as a write failure, not as a malformed legacy file (#2210) | 2026-09-03T01:39:37Z | [legacy] | — | — | [docs/project_review_2532-2256.md](../project_review_2532-2256.md) | cursor:docs/project_review_boundaries.md; report:docs/project_review_2532-2256.md (operator-confirmed) |
| #2265 | Split the probe-inflight self-test along its evidence-source owners (#2141) | 2026-09-03T01:18:20Z | [legacy] | — | — | [docs/project_review_2532-2256.md](../project_review_2532-2256.md) | cursor:docs/project_review_boundaries.md; report:docs/project_review_2532-2256.md (operator-confirmed) |
| #2264 | Reschedule due Lua scripts before their callbacks run (#2205) | 2026-09-03T00:36:12Z | [legacy] | — | — | [docs/project_review_2532-2256.md](../project_review_2532-2256.md) | cursor:docs/project_review_boundaries.md; report:docs/project_review_2532-2256.md (operator-confirmed) |
| #2263 | Reject generated-language profiles whose root space cannot cover the concept catalogue | 2026-09-03T00:05:08Z | [legacy] | — | — | [docs/project_review_2532-2256.md](../project_review_2532-2256.md) | cursor:docs/project_review_boundaries.md; report:docs/project_review_2532-2256.md (operator-confirmed) |
| #2262 | Fail startup visibly when a queued YAML family is empty or unparsable | 2026-09-03T02:23:09Z | [legacy] | — | — | [docs/project_review_2493-2262.md](../project_review_2493-2262.md) | cursor:docs/project_review_boundaries.md; report:docs/project_review_2493-2262.md (operator-confirmed) |
| #2261 | Drive every elapsed-time consumer from a monotonic clock with a bounded 0.25 s step (#2204) | 2026-09-02T22:50:20Z | [legacy] | — | — | [docs/project_review_2532-2256.md](../project_review_2532-2256.md) | cursor:docs/project_review_boundaries.md; report:docs/project_review_2532-2256.md (operator-confirmed) |
| #2260 | Restore the owning asset loader as the reported source location of shared YAML list logging | 2026-09-02T22:25:30Z | [legacy] | — | — | [docs/project_review_2532-2256.md](../project_review_2532-2256.md) | cursor:docs/project_review_boundaries.md; report:docs/project_review_2532-2256.md (operator-confirmed) |
| #2259 | Require a regular file for building preview static entries, not just a supported extension | 2026-09-02T22:00:36Z | [legacy] | — | — | [docs/project_review_2532-2256.md](../project_review_2532-2256.md) | cursor:docs/project_review_boundaries.md; report:docs/project_review_2532-2256.md (operator-confirmed) |
| #2258 | Add a pointer-only `hover` action to the playtest vocabulary | 2026-09-02T21:36:24Z | [legacy] | — | — | [docs/project_review_2532-2256.md](../project_review_2532-2256.md) | cursor:docs/project_review_boundaries.md; report:docs/project_review_2532-2256.md (operator-confirmed) |
| #2257 | Bound the combat and injury panels' grouped histories (#2189) | 2026-09-02T21:11:45Z | [legacy] | — | — | [docs/project_review_2532-2256.md](../project_review_2532-2256.md) | cursor:docs/project_review_boundaries.md; report:docs/project_review_2532-2256.md (operator-confirmed) |
| #2256 | Make Settings Back restore the persisted baseline for every video field | 2026-09-02T20:45:52Z | [legacy] | — | — | [docs/project_review_2532-2256.md](../project_review_2532-2256.md) | cursor:docs/project_review_boundaries.md; report:docs/project_review_2532-2256.md (operator-confirmed) |
| #2255 | Validate video config at one domain shared by YAML load and the Lua setters (#2198) | 2026-09-02T20:20:58Z | [legacy] | — | — | [docs/project_review_2508-2239.md](../project_review_2508-2239.md) | cursor:docs/project_review_boundaries.md; report:docs/project_review_2508-2239.md (operator-confirmed) |
| #2254 | Ghost planned buildings with their own art in both ghost states (#1845) | 2026-09-03T04:01:49Z | [legacy] | — | — | [docs/project_review_2493-2262.md](../project_review_2493-2262.md) | cursor:docs/project_review_boundaries.md; report:docs/project_review_2493-2262.md (operator-confirmed) |
| #2253 | Fail closed after the review gate's staleness decision (#2184) | 2026-09-02T19:59:55Z | [legacy] | — | — | [docs/project_review_2508-2239.md](../project_review_2508-2239.md) | cursor:docs/project_review_boundaries.md; report:docs/project_review_2508-2239.md (operator-confirmed) |
| #2252 | Stop wall rotation when either placed path is ambiguously owned | 2026-09-02T19:32:42Z | [legacy] | — | — | [docs/project_review_2508-2239.md](../project_review_2508-2239.md) | cursor:docs/project_review_boundaries.md; report:docs/project_review_2508-2239.md (operator-confirmed) |
| #2250 | Separate the README registry-count audit from the probe-runner process suite | 2026-09-02T19:05:45Z | [legacy] | — | — | [docs/project_review_2508-2239.md](../project_review_2508-2239.md) | cursor:docs/project_review_boundaries.md; report:docs/project_review_2508-2239.md (operator-confirmed) |
| #2249 | Add a non-looping building destruction presentation lifecycle (#2091) | 2026-09-02T18:41:11Z | [legacy] | — | — | [docs/project_review_2508-2239.md](../project_review_2508-2239.md) | cursor:docs/project_review_boundaries.md; report:docs/project_review_2508-2239.md (operator-confirmed) |
| #2248 | Draw scene text and UI-layer scene sprites in frame assembly at their declared layers | 2026-09-02T18:17:53Z | [legacy] | — | — | [docs/project_review_2508-2239.md](../project_review_2508-2239.md) | cursor:docs/project_review_boundaries.md; report:docs/project_review_2508-2239.md (operator-confirmed) |
| #2247 | docs: trim CLAUDE.md to session-wide rules; nested CLAUDE.md files + engine_contracts.md take the rest | 2026-09-02T17:30:07Z | [legacy] | — | — | [docs/project_review_2508-2239.md](../project_review_2508-2239.md) | cursor:docs/project_review_boundaries.md; report:docs/project_review_2508-2239.md (operator-confirmed) |
| #2246 | Supervise the debug-console listener and bound each client connection | 2026-09-02T17:52:39Z | [legacy] | — | — | [docs/project_review_2508-2239.md](../project_review_2508-2239.md) | cursor:docs/project_review_boundaries.md; report:docs/project_review_2508-2239.md (operator-confirmed) |
| #2245 | Split the EngineEnv audit self-test along inventory and boundary owners (#2062) | 2026-09-02T17:00:18Z | [legacy] | — | — | [docs/project_review_2508-2239.md](../project_review_2508-2239.md) | cursor:docs/project_review_boundaries.md; report:docs/project_review_2508-2239.md (operator-confirmed) |
| #2242 | Terminalize an accepted load whose Lua-thread half throws (#2162) | 2026-09-02T16:41:34Z | [legacy] | — | — | [docs/project_review_2508-2239.md](../project_review_2508-2239.md) | cursor:docs/project_review_boundaries.md; report:docs/project_review_2508-2239.md (operator-confirmed) |
| #2239 | Join the worker after a forced kill and stop reporting asynchronous termination as a crash | 2026-09-02T16:17:54Z | [legacy] | — | — | [docs/project_review_2508-2239.md](../project_review_2508-2239.md) | cursor:docs/project_review_boundaries.md; report:docs/project_review_2508-2239.md (operator-confirmed) |
| #2238 | Clear independently owned Lua surfaces and the locked tooltip when a load replaces the session | 2026-09-02T15:50:46Z | [legacy] | — | — | [docs/project_review_2537-2196.md](../project_review_2537-2196.md) | cursor:docs/project_review_boundaries.md; report:docs/project_review_2537-2196.md (operator-confirmed) |
| #2237 | Separate probe-promotion reporting from the census storage core | 2026-09-02T15:24:21Z | [legacy] | — | — | [docs/project_review_2537-2196.md](../project_review_2537-2196.md) | cursor:docs/project_review_boundaries.md; report:docs/project_review_2537-2196.md (operator-confirmed) |
| #2235 | Split the persistence-inventory audit along Haskell, Lua, and inventory-document owners | 2026-09-02T14:46:09Z | [legacy] | — | — | [docs/project_review_2537-2196.md](../project_review_2537-2196.md) | cursor:docs/project_review_boundaries.md; report:docs/project_review_2537-2196.md (operator-confirmed) |
| #2222 | Extract the expedition-loop probe's stage owners behind its facade (#2092) | 2026-09-02T09:10:46Z | [legacy] | — | — | [docs/project_review_2537-2196.md](../project_review_2537-2196.md) | cursor:docs/project_review_boundaries.md; report:docs/project_review_2537-2196.md (operator-confirmed) |
| #2207 | Split the responsive-gameplay gate into owner-scoped specs behind one shared engine | 2026-09-02T08:46:34Z | [legacy] | — | — | [docs/project_review_2537-2196.md](../project_review_2537-2196.md) | cursor:docs/project_review_boundaries.md; report:docs/project_review_2537-2196.md (operator-confirmed) |
| #2200 | Split the playtest critic along its ownership boundaries (#2069) | 2026-09-02T08:20:04Z | [legacy] | — | — | [docs/project_review_2537-2196.md](../project_review_2537-2196.md) | cursor:docs/project_review_boundaries.md; report:docs/project_review_2537-2196.md (operator-confirmed) |
| #2197 | Split the bare-name icon gate along language extractors, asset inventory, audit, and self-test owners (#2142) | 2026-09-02T07:56:50Z | [legacy] | — | — | [docs/project_review_2537-2196.md](../project_review_2537-2196.md) | cursor:docs/project_review_boundaries.md; report:docs/project_review_2537-2196.md (operator-confirmed) |
| #2196 | Split the /deflake self-test along its orchestration, handoff and preparation owners (#2093) | 2026-09-02T06:28:39Z | [legacy] | — | — | [docs/project_review_2537-2196.md](../project_review_2537-2196.md) | cursor:docs/project_review_boundaries.md; report:docs/project_review_2537-2196.md (operator-confirmed) |
| #2191 | Cache exact zoom reconstruction artifacts | 2026-09-02T05:54:36Z | [legacy] | — | — | [docs/project_review_2542-2144.md](../project_review_2542-2144.md) | cursor:docs/project_review_boundaries.md; report:docs/project_review_2542-2144.md (operator-confirmed) |
| #2190 | Migrate ten probes to structured flake results | 2026-09-02T14:19:39Z | [legacy] | — | — | [docs/project_review_2537-2196.md](../project_review_2537-2196.md) | cursor:docs/project_review_boundaries.md; report:docs/project_review_2537-2196.md (operator-confirmed) |
| #2181 | Ghost planned structure pieces with their own art in both ghost states (#1846) | 2026-09-02T07:33:12Z | [legacy] | — | — | [docs/project_review_2537-2196.md](../project_review_2537-2196.md) | cursor:docs/project_review_boundaries.md; report:docs/project_review_2537-2196.md (operator-confirmed) |
| #2178 | Split tools/pack_atlas.py along its inventory, compiler, index and budget owners (#2054) | 2026-09-02T04:21:32Z | [legacy] | — | — | [docs/project_review_2542-2144.md](../project_review_2542-2144.md) | cursor:docs/project_review_boundaries.md; report:docs/project_review_2542-2144.md (operator-confirmed) |
| #2158 | Extract the capability-writer scanner behind the EngineEnv audit facade | 2026-09-02T04:02:28Z | [legacy] | — | — | [docs/project_review_2542-2144.md](../project_review_2542-2144.md) | cursor:docs/project_review_boundaries.md; report:docs/project_review_2542-2144.md (operator-confirmed) |
| #2154 | Render and hit-test buildings from the active camera facing (#2088) | 2026-09-02T03:45:16Z | [legacy] | — | — | [docs/project_review_2542-2144.md](../project_review_2542-2144.md) | cursor:docs/project_review_boundaries.md; report:docs/project_review_2542-2144.md (operator-confirmed) |
| #2153 | Split the save-migrations gate along baseline, DTO-history, and legacy-envelope owners (#2094) | 2026-09-02T05:31:11Z | [legacy] | — | — | [docs/project_review_2542-2144.md](../project_review_2542-2144.md) | cursor:docs/project_review_boundaries.md; report:docs/project_review_2542-2144.md (operator-confirmed) |
| #2152 | Split the world-audit self-test along audit, check and baseline owners (#2070) | 2026-09-02T02:21:15Z | [legacy] | — | — | [docs/project_review_2542-2144.md](../project_review_2542-2144.md) | cursor:docs/project_review_boundaries.md; report:docs/project_review_2542-2144.md (operator-confirmed) |
| #2146 | Delete the two arena seed-contract self-comparisons and the fixture that feeds one of them | 2026-09-02T01:42:07Z | [legacy] | — | — | [docs/project_review_2542-2144.md](../project_review_2542-2144.md) | cursor:docs/project_review_boundaries.md; report:docs/project_review_2542-2144.md (operator-confirmed) |
| #2144 | Fail the unit-to-unit escort fixture at setup instead of grading a pair already in reach | 2026-09-02T01:17:24Z | [legacy] | — | — | [docs/project_review_2542-2144.md](../project_review_2542-2144.md) | cursor:docs/project_review_boundaries.md; report:docs/project_review_2542-2144.md (operator-confirmed) |
| #2143 | Give headless test fixtures a quiet log backend by default (#1925) | 2026-09-02T00:50:40Z | [legacy] | — | — | [docs/project_review_2543-2119.md](../project_review_2543-2119.md) | cursor:docs/project_review_boundaries.md; report:docs/project_review_2543-2119.md (operator-confirmed) |
| #2139 | Extract location-content scenario owners behind its eight-process façade | 2026-09-02T00:24:42Z | [legacy] | — | — | [docs/project_review_2543-2119.md](../project_review_2543-2119.md) | cursor:docs/project_review_boundaries.md; report:docs/project_review_2543-2119.md (operator-confirmed) |
| #2137 | Introduce camera-facing building asset declarations and distinct lifecycle roles | 2026-09-01T23:57:58Z | [legacy] | — | — | [docs/project_review_2543-2119.md](../project_review_2543-2119.md) | cursor:docs/project_review_boundaries.md; report:docs/project_review_2543-2119.md (operator-confirmed) |
| #2136 | [art] Add approved wild and cultivated wheat textures | 2026-09-01T23:31:03Z | [legacy] | — | — | [docs/project_review_2543-2119.md](../project_review_2543-2119.md) | cursor:docs/project_review_boundaries.md; report:docs/project_review_2543-2119.md (operator-confirmed) |
| #2134 | Split the save-compatibility tool along audit, codec, registration, and generation owners | 2026-09-01T23:05:39Z | [legacy] | — | — | [docs/project_review_2543-2119.md](../project_review_2543-2119.md) | cursor:docs/project_review_boundaries.md; report:docs/project_review_2543-2119.md (operator-confirmed) |
| #2133 | Establish the shared generated-world library and its reference-aware lifecycle (#2024) | 2026-09-02T07:03:37Z | [legacy] | — | — | [docs/project_review_2537-2196.md](../project_review_2537-2196.md) | cursor:docs/project_review_boundaries.md; report:docs/project_review_2537-2196.md (operator-confirmed) |
| #2132 | Migrate meal waste probe to probe-result/v1 | 2026-09-01T22:12:49Z | [legacy] | — | — | [docs/project_review_2543-2119.md](../project_review_2543-2119.md) | cursor:docs/project_review_boundaries.md; report:docs/project_review_2543-2119.md (operator-confirmed) |
| #2127 | Split the enum append-only gate along parser, carrier, baseline, and self-test owners (#2057) | 2026-09-01T22:38:13Z | [legacy] | — | — | [docs/project_review_2543-2119.md](../project_review_2543-2119.md) | cursor:docs/project_review_boundaries.md; report:docs/project_review_2543-2119.md (operator-confirmed) |
| #2125 | Gate location clearing on guaranteed significant loot (#917) | 2026-09-02T03:17:56Z | [legacy] | — | — | [docs/project_review_2542-2144.md](../project_review_2542-2144.md) | cursor:docs/project_review_boundaries.md; report:docs/project_review_2542-2144.md (operator-confirmed) |
| #2123 | Give each generated world an opaque persistent identity (#2021) | 2026-09-01T21:49:13Z | [legacy] | — | — | [docs/project_review_2543-2119.md](../project_review_2543-2119.md) | cursor:docs/project_review_boundaries.md; report:docs/project_review_2543-2119.md (operator-confirmed) |
| #2122 | Give both de-flake outcome consumers one shared handoff contract (#2097) | 2026-09-01T21:22:57Z | [legacy] | — | — | [docs/project_review_2543-2119.md](../project_review_2543-2119.md) | cursor:docs/project_review_boundaries.md; report:docs/project_review_2543-2119.md (operator-confirmed) |
| #2121 | Make Chop a drag-box tool with tree-anchored designation markers (#1856) | 2026-09-02T02:50:32Z | [legacy] | — | — | [docs/project_review_2542-2144.md](../project_review_2542-2144.md) | cursor:docs/project_review_boundaries.md; report:docs/project_review_2542-2144.md (operator-confirmed) |
| #2120 | One quiet-by-default assertion helper for the tools/ self-tests (#1922) | 2026-09-01T21:07:05Z | [legacy] | — | — | [docs/project_review_2543-2119.md](../project_review_2543-2119.md) | cursor:docs/project_review_boundaries.md; report:docs/project_review_2543-2119.md (operator-confirmed) |
| #2119 | Split the worldgen DTO graph into owner modules behind a façade (#2098) | 2026-09-01T20:39:31Z | [legacy] | — | — | [docs/project_review_2543-2119.md](../project_review_2543-2119.md) | cursor:docs/project_review_boundaries.md; report:docs/project_review_2543-2119.md (operator-confirmed) |
| #2118 | Split the atomic probe-claim gate along lease, census, and orchestration owners (#2100) | 2026-09-01T20:04:21Z | never reviewed | — | — | — | — |
| #2117 | Split the playtest runner's self-test along its module owners (#2040) | 2026-09-01T19:38:24Z | never reviewed | — | — | — | — |
| #2115 | Extract the unified-transfer probe's stage owners behind its single-session façade | 2026-09-01T11:15:23Z | never reviewed | — | — | — | — |
| #2114 | Make woundEffSeverity the only spelling of effective severity in the wound tick | 2026-09-01T10:45:22Z | never reviewed | — | — | — | — |
| #2113 | Route gameplay unit atlases through the player-selected sampler | 2026-09-01T10:22:24Z | never reviewed | — | — | — | — |
| #2112 | Split the generated-language headless spec along generator-version contracts | 2026-09-01T09:59:01Z | never reviewed | — | — | — | — |
| #2111 | Split the aggregate probe runner along registry, diagnostics, lifecycle, and scheduling owners | 2026-09-01T09:32:00Z | never reviewed | — | — | — | — |
| #2110 | Pin the encounter roll's id dependence, chunk independence and mapping | 2026-09-01T09:07:51Z | never reviewed | — | — | — | — |
| #2109 | Make structure drag planning authoritative and self-clearing (#1844) | 2026-09-01T18:21:36Z | never reviewed | — | — | — | — |
| #2108 | Give every flora instance stable identity and exact mutable state (#1854) | 2026-09-01T08:43:09Z | never reviewed | — | — | — | — |
| #2107 | Extrude one texel around every atlas cell so linear sampling cannot bleed across frames | 2026-09-01T08:13:13Z | never reviewed | — | — | — | — |
| #2106 | Carry signed 32-bit cylinder coordinates in every world vertex | 2026-09-01T07:47:55Z | never reviewed | — | — | — | — |
| #2105 | Delete the twelve subsumed repeated-projection examples and correct the seven inventory claims they back | 2026-09-01T07:22:20Z | never reviewed | — | — | — | — |
| #2104 | Pin UI textures to nearest instead of following the player's filter setting | 2026-09-01T06:58:04Z | never reviewed | — | — | — | — |
| #2103 | Keep input_check.py's diagnostic sequence alive after a missed fixture click | 2026-09-01T06:33:45Z | never reviewed | — | — | — | — |
| #2102 | Establish transient unit-AI runtime defaults before a migrated row goes live | 2026-09-01T06:11:29Z | never reviewed | — | — | — | — |
| #2101 | Require a crossed presentation boundary before acknowledging sticky tutorial rows | 2026-09-01T05:47:55Z | never reviewed | — | — | — | — |
| #2099 | Make the capability-writer audit fail closed on projection bindings it cannot read | 2026-09-01T04:59:42Z | never reviewed | — | — | — | — |
| #2096 | Migrate blood impact probe to probe-result/v1 | 2026-09-01T05:23:29Z | never reviewed | — | — | — | — |
| #2086 | Gate the durable location-stamp marker on world-thread commit, not on queuing | 2026-09-01T04:36:07Z | never reviewed | — | — | — | — |
| #2084 | Refuse an unsafe map image plan before allocating or uploading it (#2020) | 2026-09-01T03:41:07Z | never reviewed | — | — | — | — |
| #2083 | Gate Mode B transfer orders on the carrier's registered AI actions | 2026-09-01T03:17:45Z | never reviewed | — | — | — | — |
| #2082 | Exchange and activate runtime fluid across the cylindrical U seam | 2026-09-01T04:09:07Z | never reviewed | — | — | — | — |
| #2081 | Cap lateral fluid equalization at the source's remaining volume | 2026-09-01T01:48:14Z | never reviewed | — | — | — | — |
| #2079 | Aggregate active startup YAML logging once per registry family | 2026-09-01T01:21:38Z | never reviewed | — | — | — | — |
| #2077 | Add keyboard navigation to preview browser | 2026-09-01T01:03:01Z | never reviewed | — | — | — | — |
| #2066 | playtest: camera-relative zoom semantics and a bounded wheel delta for the scroll action | 2026-09-01T00:43:38Z | never reviewed | — | — | — | — |
| #2045 | Declare repair recipes instantaneous instead of advertising work the repair path never spends | 2026-09-01T00:27:39Z | never reviewed | — | — | — | — |
| #2039 | Gate the coastal-parallel threshold on the run length it names, in both river gates | 2026-09-01T00:11:23Z | never reviewed | — | — | — | — |
| #2038 | Bound breakthrough search scratch to its radius | 2026-08-31T23:47:22Z | never reviewed | — | — | — | — |
| #2032 | Migrate five targeted probes to probe-result/v1 | 2026-08-31T23:20:54Z | never reviewed | — | — | — | — |
| #2029 | Bound the debug console's retained scrollback and its layout measurement | 2026-08-31T22:58:45Z | never reviewed | — | — | — | — |
| #2028 | Make red raspberry fruiting art visibly ripe | 2026-08-31T22:35:31Z | never reviewed | — | — | — | — |
| #2025 | Skip location sight rasterization during clearance-only ticks | 2026-08-31T22:12:58Z | never reviewed | — | — | — | — |
| #2023 | Add Machine Shop construction progress art | 2026-08-31T21:39:47Z | never reviewed | — | — | — | — |
| #2022 | Centralize chunk demand behind one canonical chunk key and request owner | 2026-09-01T02:15:06Z | never reviewed | — | — | — | — |
| #2018 | Instrument World.Render scene assembly telemetry (#1921) | 2026-08-31T21:14:45Z | never reviewed | — | — | — | — |
| #2016 | Render crop Plant as a light-green flat tilled surface | 2026-08-31T20:33:13Z | never reviewed | — | — | — | — |
| #2015 | Keep a failing location/portal probe's failed check in the retained output | 2026-08-31T20:10:47Z | never reviewed | — | — | — | — |
| #2014 | Add common cattail wetland flora | 2026-08-31T19:47:41Z | never reviewed | — | — | — | — |
| #2013 | Measure fjord and glacial coast forms | 2026-08-31T19:28:42Z | never reviewed | — | — | — | — |
| #2012 | Gate every Lua call site against the engine's real registration set (#1996) | 2026-08-31T19:11:53Z | never reviewed | — | — | — | — |
| #2011 | Migrate lua_strict_msg probe to probe-result/v1 | 2026-08-31T18:42:22Z | never reviewed | — | — | — | — |
| #2010 | Prepare a directly-invoked probe's engine outside the READY deadline (#1913) | 2026-08-31T18:25:47Z | never reviewed | — | — | — | — |
| #2009 | Couple scene-text cache entries to their scene nodes' lifetimes | 2026-08-31T18:02:22Z | never reviewed | — | — | — | — |
| #2008 | Replace tomato placeholders with approved PixelLab art | 2026-08-31T17:43:22Z | [legacy] | — | — | — | cursor:docs/project_review_boundaries.md |
| #2007 | Add a read-only ContentRegistries view for every non-writer consumer | 2026-08-31T17:24:19Z | [legacy] | — | — | [docs/project_review_2007-1456.md](../project_review_2007-1456.md) | cursor:docs/project_review_boundaries.md; report:docs/project_review_2007-1456.md (operator-confirmed) |
| #2006 | Make the in-game console's completion candidates match its execution sandbox | 2026-08-31T17:06:31Z | [legacy] | — | — | — | cursor:docs/project_review_boundaries.md |
| #2005 | Retire camera.goToTile's obsolete glacier heap-overflow fence | 2026-08-31T16:46:54Z | [legacy] | — | — | — | cursor:docs/project_review_boundaries.md |
| #2004 | Seed the wake-boundary baseline when a unit enters the Sleeping phase | 2026-08-31T16:28:46Z | [legacy] | — | — | [docs/project_review_2004-1710.md](../project_review_2004-1710.md) | cursor:docs/project_review_boundaries.md; report:docs/project_review_2004-1710.md (operator-confirmed) |
| #2003 | Classify a Lua chunk source before shortening it for the log prefix | 2026-08-31T16:11:41Z | [legacy] | — | — | [docs/project_review_2003-1754.md](../project_review_2003-1754.md) | cursor:docs/project_review_boundaries.md; report:docs/project_review_2003-1754.md (operator-confirmed) |
| #2002 | Warn when swapchain format or present-mode selection falls back from the preferred capability | 2026-08-31T15:53:47Z | [legacy] | — | — | [docs/project_review_2002-1783.md](../project_review_2002-1783.md) | cursor:docs/project_review_boundaries.md; report:docs/project_review_2002-1783.md (operator-confirmed) |
| #2000 | Apply every whole-band speed multiplier to the ambient meander cap | 2026-08-31T03:58:09Z | [legacy] | — | — | [docs/project_review_2000-1827.md](../project_review_2000-1827.md) | cursor:docs/project_review_boundaries.md; report:docs/project_review_2000-1827.md (operator-confirmed) |
| #1999 | Fix Create World initial identity handoff | 2026-08-31T03:41:57Z | [legacy] | — | — | [docs/project_review_2000-1827.md](../project_review_2000-1827.md) | cursor:docs/project_review_boundaries.md; report:docs/project_review_2000-1827.md (operator-confirmed) |
| #1998 | Fit the debug console to the framebuffer width across the supported envelope | 2026-08-31T03:24:27Z | [legacy] | — | — | [docs/project_review_2000-1827.md](../project_review_2000-1827.md) | cursor:docs/project_review_boundaries.md; report:docs/project_review_2000-1827.md (operator-confirmed) |
| #1993 | Reject a non-positive or non-finite loot-table weight at its decoder | 2026-08-31T03:07:47Z | [legacy] | — | — | [docs/project_review_2000-1827.md](../project_review_2000-1827.md) | cursor:docs/project_review_boundaries.md; report:docs/project_review_2000-1827.md (operator-confirmed) |
| #1992 | Rank a unit's known locations in the page's cylindrical frame (#1944) | 2026-08-31T02:50:02Z | [legacy] | — | — | [docs/project_review_2000-1827.md](../project_review_2000-1827.md) | cursor:docs/project_review_boundaries.md; report:docs/project_review_2000-1827.md (operator-confirmed) |
| #1991 | Reject an explicitly empty --resource-root operand | 2026-08-31T02:33:22Z | [legacy] | — | — | [docs/project_review_2000-1827.md](../project_review_2000-1827.md) | cursor:docs/project_review_boundaries.md; report:docs/project_review_2000-1827.md (operator-confirmed) |
| #1989 | Retire an already-latched tutorial branch once it has been presented | 2026-08-31T02:15:30Z | [legacy] | — | — | [docs/project_review_1989-1834.md](../project_review_1989-1834.md) | cursor:docs/project_review_boundaries.md; report:docs/project_review_1989-1834.md (operator-confirmed) |
| #1988 | Stop the debug console collapsing distinct Lua table keys into one JSON member | 2026-08-31T01:59:45Z | [legacy] | — | — | — | cursor:docs/project_review_boundaries.md |
| #1987 | Resolve notification overrides per field, not per category (#1938) | 2026-08-31T01:41:10Z | [legacy] | — | — | [docs/project_review_1987-1893.md](../project_review_1987-1893.md) | cursor:docs/project_review_boundaries.md; report:docs/project_review_1987-1893.md (operator-confirmed) |
| #1986 | Derive the automatic sleep-wake boundary from the species circadian phase | 2026-08-31T01:26:41Z | [legacy] | — | — | [docs/project_review_1986-1908.md](../project_review_1986-1908.md) | cursor:docs/project_review_boundaries.md; report:docs/project_review_1986-1908.md (operator-confirmed) |
| #1985 | Place the unit hit box at the continuous Z the renderer draws | 2026-08-31T01:11:07Z | [legacy] | — | — | [docs/project_review_1986-1908.md](../project_review_1986-1908.md) | cursor:docs/project_review_boundaries.md; report:docs/project_review_1986-1908.md (operator-confirmed) |
| #1984 | Reject a non-positive recipe count instead of loading a free-output craft | 2026-08-31T00:52:04Z | [legacy] | — | — | [docs/project_review_1986-1908.md](../project_review_1986-1908.md) | cursor:docs/project_review_boundaries.md; report:docs/project_review_1986-1908.md (operator-confirmed) |
| #1981 | Stop the legacy config migration from promoting a neutral placeholder into durable local state | 2026-08-31T00:35:15Z | [legacy] | — | — | [docs/project_review_1981-1968.md](../project_review_1981-1968.md) | cursor:docs/project_review_boundaries.md; report:docs/project_review_1981-1968.md (operator-confirmed) |
| #1979 | Migrate text_encoding probe to probe-result/v1 | 2026-08-31T00:09:31Z | [legacy] | — | — | [docs/project_review_1981-1968.md](../project_review_1981-1968.md) | cursor:docs/project_review_boundaries.md; report:docs/project_review_1981-1968.md (operator-confirmed) |
| #1977 | Render Till as a flat level-ground surface | 2026-08-30T23:46:47Z | [legacy] | — | — | [docs/project_review_1981-1968.md](../project_review_1981-1968.md) | cursor:docs/project_review_boundaries.md; report:docs/project_review_1981-1968.md (operator-confirmed) |
| #1976 | Decode each save component once and derive the load phase structurally | 2026-08-30T23:22:21Z | [legacy] | — | — | [docs/project_review_1981-1968.md](../project_review_1981-1968.md) | cursor:docs/project_review_boundaries.md; report:docs/project_review_1981-1968.md (operator-confirmed) |
| #1975 | Give every inter-thread engine queue depth, high-water, and oldest-message-age telemetry | 2026-08-30T22:57:14Z | [legacy] | — | — | [docs/project_review_1981-1968.md](../project_review_1981-1968.md) | cursor:docs/project_review_boundaries.md; report:docs/project_review_1981-1968.md (operator-confirmed) |
| #1974 | Add Workbench construction progress art | 2026-08-30T22:34:30Z | [legacy] | — | — | [docs/project_review_1981-1968.md](../project_review_1981-1968.md) | cursor:docs/project_review_boundaries.md; report:docs/project_review_1981-1968.md (operator-confirmed) |
| #1973 | Gate hud.update's cursor hover on gameplay input ownership (#1931) | 2026-08-30T22:09:52Z | [legacy] | — | — | [docs/project_review_1981-1968.md](../project_review_1981-1968.md) | cursor:docs/project_review_boundaries.md; report:docs/project_review_1981-1968.md (operator-confirmed) |
| #1972 | Read category log thresholds from the documented ENGINE_LOG_<CATEGORY> name | 2026-08-30T21:16:55Z | [legacy] | — | — | [docs/project_review_1981-1968.md](../project_review_1981-1968.md) | cursor:docs/project_review_boundaries.md; report:docs/project_review_1981-1968.md (operator-confirmed) |
| #1971 | Redraw Till and Plant toolbar icons | 2026-08-30T20:54:18Z | [legacy] | — | — | [docs/project_review_1981-1968.md](../project_review_1981-1968.md) | cursor:docs/project_review_boundaries.md; report:docs/project_review_1981-1968.md (operator-confirmed) |
| #1970 | Preserve the engine's canonical save order through the main menu | 2026-08-30T20:34:39Z | [legacy] | — | — | [docs/project_review_1981-1968.md](../project_review_1981-1968.md) | cursor:docs/project_review_boundaries.md; report:docs/project_review_1981-1968.md (operator-confirmed) |
| #1968 | Give split input holds an ownership-safe modifier lifetime | 2026-08-30T20:11:29Z | [legacy] | — | — | [docs/project_review_1981-1968.md](../project_review_1981-1968.md) | cursor:docs/project_review_boundaries.md; report:docs/project_review_1981-1968.md (operator-confirmed) |
| #1964 | Gate the Unit Info row's selection cleanup on a successful unit selection | 2026-08-30T19:48:38Z | [legacy] | — | — | [docs/project_review_1986-1908.md](../project_review_1986-1908.md) | cursor:docs/project_review_boundaries.md; report:docs/project_review_1986-1908.md (operator-confirmed) |
| #1962 | Call the registered UI.setColor verb from bar.setFillColor | 2026-08-30T19:30:32Z | [legacy] | — | — | [docs/project_review_1986-1908.md](../project_review_1986-1908.md) | cursor:docs/project_review_boundaries.md; report:docs/project_review_1986-1908.md (operator-confirmed) |
| #1951 | Exclude deferred probes from the flake lab | 2026-08-30T19:13:47Z | [legacy] | — | — | [docs/project_review_1986-1908.md](../project_review_1986-1908.md) | cursor:docs/project_review_boundaries.md; report:docs/project_review_1986-1908.md (operator-confirmed) |
| #1943 | Derive every ENGINE_DEBUG category name and the "all" set from LogCategory | 2026-08-30T18:57:04Z | [legacy] | — | — | [docs/project_review_1986-1908.md](../project_review_1986-1908.md) | cursor:docs/project_review_boundaries.md; report:docs/project_review_1986-1908.md (operator-confirmed) |
| #1942 | Make each probe's copied config removable and stop item-instance passing over surviving residue | 2026-08-30T18:38:16Z | [legacy] | — | — | [docs/project_review_1986-1908.md](../project_review_1986-1908.md) | cursor:docs/project_review_boundaries.md; report:docs/project_review_1986-1908.md (operator-confirmed) |
| #1936 | Author real variants for five vegetation textures | 2026-08-30T18:21:42Z | [legacy] | — | — | [docs/project_review_1986-1908.md](../project_review_1986-1908.md) | cursor:docs/project_review_boundaries.md; report:docs/project_review_1986-1908.md (operator-confirmed) |
| #1923 | Add centered bounded zoom to --preview asset panes | 2026-08-30T18:05:26Z | [legacy] | — | — | [docs/project_review_1986-1908.md](../project_review_1986-1908.md) | cursor:docs/project_review_boundaries.md; report:docs/project_review_1986-1908.md (operator-confirmed) |
| #1908 | Add forgeable steel helmet (#1785) | 2026-08-30T17:34:51Z | [legacy] | — | — | [docs/project_review_1986-1908.md](../project_review_1986-1908.md) | cursor:docs/project_review_boundaries.md; report:docs/project_review_1986-1908.md (operator-confirmed) |
| #1906 | Give location_content_probe invocation-owned fixtures and log (#1884) | 2026-08-30T16:58:21Z | [legacy] | — | — | [docs/project_review_1987-1893.md](../project_review_1987-1893.md) | cursor:docs/project_review_boundaries.md; report:docs/project_review_1987-1893.md (operator-confirmed) |
| #1905 | Pin each capability field's writing modules in the EngineEnv capability audit | 2026-08-30T17:49:41Z | [legacy] | — | — | [docs/project_review_1986-1908.md](../project_review_1986-1908.md) | cursor:docs/project_review_boundaries.md; report:docs/project_review_1986-1908.md (operator-confirmed) |
| #1904 | Classify lava by rim containment instead of column depth in the world audit | 2026-08-30T15:43:41Z | [legacy] | — | — | [docs/project_review_1987-1893.md](../project_review_1987-1893.md) | cursor:docs/project_review_boundaries.md; report:docs/project_review_1987-1893.md (operator-confirmed) |
| #1903 | Seed the windowed-geometry cache from a fullscreen boot's decorated window | 2026-08-30T15:18:07Z | [legacy] | — | — | [docs/project_review_1987-1893.md](../project_review_1987-1893.md) | cursor:docs/project_review_boundaries.md; report:docs/project_review_1987-1893.md (operator-confirmed) |
| #1902 | Add primitive nomad locomotion and crawling animations | 2026-08-30T14:51:19Z | [legacy] | — | — | [docs/project_review_1987-1893.md](../project_review_1987-1893.md) | cursor:docs/project_review_boundaries.md; report:docs/project_review_1987-1893.md (operator-confirmed) |
| #1901 | Add primitive nomad combat animations | 2026-08-29T21:54:52Z | [legacy] | — | — | [docs/project_review_1987-1893.md](../project_review_1987-1893.md) | cursor:docs/project_review_boundaries.md; report:docs/project_review_1987-1893.md (operator-confirmed) |
| #1900 | Add persistent nomad encounters to small ruins | 2026-08-30T21:45:05Z | [legacy] | — | — | [docs/project_review_1981-1968.md](../project_review_1981-1968.md) | cursor:docs/project_review_boundaries.md; report:docs/project_review_1981-1968.md (operator-confirmed) |
| #1899 | Resolve structure-pack piece art engine-side for unplaced pieces (#1842) | 2026-08-29T21:31:55Z | [legacy] | — | — | [docs/project_review_1987-1893.md](../project_review_1987-1893.md) | cursor:docs/project_review_boundaries.md; report:docs/project_review_1987-1893.md (operator-confirmed) |
| #1898 | docs: describe power_workshop_probe by the #590 recipe-draw model it gates | 2026-08-29T21:05:28Z | [legacy] | — | — | [docs/project_review_1987-1893.md](../project_review_1987-1893.md) | cursor:docs/project_review_boundaries.md; report:docs/project_review_1987-1893.md (operator-confirmed) |
| #1897 | Light each visible world page from its own clock and circumference | 2026-08-29T20:43:35Z | [legacy] | — | — | [docs/project_review_1987-1893.md](../project_review_1987-1893.md) | cursor:docs/project_review_boundaries.md; report:docs/project_review_1987-1893.md (operator-confirmed) |
| #1895 | fix(playtest): reject conflicting verdicts on one friction candidate | 2026-08-29T20:12:56Z | [legacy] | — | — | [docs/project_review_1987-1893.md](../project_review_1987-1893.md) | cursor:docs/project_review_boundaries.md; report:docs/project_review_1987-1893.md (operator-confirmed) |
| #1894 | Keep an added concept id from re-rooting an existing concept | 2026-08-29T19:50:51Z | [legacy] | — | — | [docs/project_review_1987-1893.md](../project_review_1987-1893.md) | cursor:docs/project_review_boundaries.md; report:docs/project_review_1987-1893.md (operator-confirmed) |
| #1893 | Gate world-entity clicks on the zoomed-in view (#1875) | 2026-08-29T19:08:07Z | [legacy] | — | — | [docs/project_review_1987-1893.md](../project_review_1987-1893.md) | cursor:docs/project_review_boundaries.md; report:docs/project_review_1987-1893.md (operator-confirmed) |
| #1891 | Record a non-object player reply as a wait instead of crashing the turn | 2026-08-29T18:51:22Z | [legacy] | — | — | — | cursor:docs/project_review_boundaries.md |
| #1889 | Pin and record sight conditions in the tutorial probe's pre-latched reveal | 2026-08-29T18:30:51Z | [legacy] | — | — | — | cursor:docs/project_review_boundaries.md |
| #1888 | Replay preview animations continuously, whatever their authored loop (#1833) | 2026-08-29T18:15:01Z | [legacy] | — | — | — | cursor:docs/project_review_boundaries.md |
| #1887 | Stock the technomule's field toolbox with starter hand tools | 2026-08-29T17:59:22Z | [legacy] | — | — | — | cursor:docs/project_review_boundaries.md |
| #1886 | Make exact roster-unit selection a prerequisite before the embark probe orders a move | 2026-08-29T17:43:43Z | [legacy] | — | — | — | cursor:docs/project_review_boundaries.md |
| #1885 | repair_ai_probe: judge phase 8's repair ordering from one timeline (#1767) | 2026-08-29T17:04:41Z | [legacy] | — | — | — | cursor:docs/project_review_boundaries.md |
| #1883 | Give the foraging probe a deterministic harvestable target instead of natural placement | 2026-08-29T16:48:02Z | [legacy] | — | — | — | cursor:docs/project_review_boundaries.md |
| #1881 | Tie the embark probe's two saves to their own requests (#1746) | 2026-08-29T16:14:25Z | [legacy] | — | — | — | cursor:docs/project_review_boundaries.md |
| #1880 | Give the flora-growth probe invocation-owned fixtures and log (#1682) | 2026-08-29T16:31:41Z | [legacy] | — | — | — | cursor:docs/project_review_boundaries.md |
| #1879 | Capture the worker and network state when the power-workshop AI polls time out | 2026-08-29T15:57:41Z | [legacy] | — | — | — | cursor:docs/project_review_boundaries.md |
| #1878 | Require a positive preferred-soil score so plant_probe's granite zero proves soil gating | 2026-08-29T15:43:22Z | [legacy] | — | — | [docs/project_review_1878-1859.md](../project_review_1878-1859.md) | cursor:docs/project_review_boundaries.md; report:docs/project_review_1878-1859.md (operator-confirmed) |
| #1877 | Give each preview and offscreen engine boot its own retained log (#1763) | 2026-08-29T15:27:41Z | [legacy] | — | — | [docs/project_review_1878-1859.md](../project_review_1878-1859.md) | cursor:docs/project_review_boundaries.md; report:docs/project_review_1878-1859.md (operator-confirmed) |
| #1872 | Assert the cold thought's own identity in thought_probe phase 4 (#1759) | 2026-08-29T14:37:40Z | [legacy] | — | — | [docs/project_review_1878-1859.md](../project_review_1878-1859.md) | cursor:docs/project_review_boundaries.md; report:docs/project_review_1878-1859.md (operator-confirmed) |
| #1870 | File an issue instead of a PR when the bug is in the engine (#1438) | 2026-08-29T14:53:26Z | [legacy] | — | — | [docs/project_review_1878-1859.md](../project_review_1878-1859.md) | cursor:docs/project_review_boundaries.md; report:docs/project_review_1878-1859.md (operator-confirmed) |
| #1867 | Isolate the offscreen type-icon assertion from the discovery popup that covers it | 2026-08-29T14:22:28Z | [legacy] | — | — | [docs/project_review_1878-1859.md](../project_review_1878-1859.md) | cursor:docs/project_review_boundaries.md; report:docs/project_review_1878-1859.md (operator-confirmed) |
| #1866 | Establish an admissible pickup order in the follow-command probe before judging arbitration | 2026-08-29T14:07:06Z | [legacy] | — | — | [docs/project_review_1878-1859.md](../project_review_1878-1859.md) | cursor:docs/project_review_boundaries.md; report:docs/project_review_1878-1859.md (operator-confirmed) |
| #1865 | Exercise the state-of-mind guard in all three consciousness bands | 2026-08-29T13:51:42Z | [legacy] | — | — | [docs/project_review_1878-1859.md](../project_review_1878-1859.md) | cursor:docs/project_review_boundaries.md; report:docs/project_review_1878-1859.md (operator-confirmed) |
| #1864 | Stop tools/README.md from carrying a hand-maintained probe count | 2026-08-29T17:21:07Z | [legacy] | — | — | — | cursor:docs/project_review_boundaries.md |
| #1863 | Require the etymology probe's forced scroll configuration to actually overflow | 2026-08-29T13:36:15Z | [legacy] | — | — | [docs/project_review_1878-1859.md](../project_review_1878-1859.md) | cursor:docs/project_review_boundaries.md; report:docs/project_review_1878-1859.md (operator-confirmed) |
| #1862 | Follow the harvested yield's identity in farm_ai_probe phase 9 | 2026-08-29T04:08:45Z | [legacy] | — | — | [docs/project_review_1878-1859.md](../project_review_1878-1859.md) | cursor:docs/project_review_boundaries.md; report:docs/project_review_1878-1859.md (operator-confirmed) |
| #1861 | Name the phase and nested probes active when a sweep times out | 2026-08-29T03:06:18Z | [legacy] | — | — | [docs/project_review_1878-1859.md](../project_review_1878-1859.md) | cursor:docs/project_review_boundaries.md; report:docs/project_review_1878-1859.md (operator-confirmed) |
| #1860 | Generate the thermo probe's dump world with the same plate count as its live world | 2026-08-29T02:12:09Z | [legacy] | — | — | [docs/project_review_1878-1859.md](../project_review_1878-1859.md) | cursor:docs/project_review_boundaries.md; report:docs/project_review_1878-1859.md (operator-confirmed) |
| #1859 | Give the location probes a private config tree instead of a symlink to the checkout | 2026-08-29T00:50:44Z | [legacy] | — | — | [docs/project_review_1878-1859.md](../project_review_1878-1859.md) | cursor:docs/project_review_boundaries.md; report:docs/project_review_1878-1859.md (operator-confirmed) |
| #1852 | Source repair targets from the ground, completing the repair sourcing ladder | 2026-08-28T19:45:15Z | [legacy] | — | — | [docs/project_review_1989-1834.md](../project_review_1989-1834.md) | cursor:docs/project_review_boundaries.md; report:docs/project_review_1989-1834.md (operator-confirmed) |
| #1851 | Generate ci-local.sh's step labels instead of hand-numbering them | 2026-08-28T19:28:30Z | [legacy] | — | — | [docs/project_review_1989-1834.md](../project_review_1989-1834.md) | cursor:docs/project_review_boundaries.md; report:docs/project_review_1989-1834.md (operator-confirmed) |
| #1847 | Report CI-promotion candidates from the probe census | 2026-08-28T19:11:54Z | [legacy] | — | — | [docs/project_review_1989-1834.md](../project_review_1989-1834.md) | cursor:docs/project_review_boundaries.md; report:docs/project_review_1989-1834.md (operator-confirmed) |
| #1843 | Add scavenged field toolbox | 2026-08-28T18:50:45Z | [legacy] | — | — | [docs/project_review_1989-1834.md](../project_review_1989-1834.md) | cursor:docs/project_review_boundaries.md; report:docs/project_review_1989-1834.md (operator-confirmed) |
| #1841 | Stage each probe's isolated root inside its own cleanup guard | 2026-08-28T18:29:20Z | [legacy] | — | — | [docs/project_review_1989-1834.md](../project_review_1989-1834.md) | cursor:docs/project_review_boundaries.md; report:docs/project_review_1989-1834.md (operator-confirmed) |
| #1840 | Rename the two water diagnostics out of the test_* namespace | 2026-08-28T17:30:01Z | [legacy] | — | — | [docs/project_review_1989-1834.md](../project_review_1989-1834.md) | cursor:docs/project_review_boundaries.md; report:docs/project_review_1989-1834.md (operator-confirmed) |
| #1839 | Correct iiWeight's stale carried-weight comment to defer to itemTotalWeight | 2026-08-28T16:46:02Z | [legacy] | — | — | [docs/project_review_1989-1834.md](../project_review_1989-1834.md) | cursor:docs/project_review_boundaries.md; report:docs/project_review_1989-1834.md (operator-confirmed) |
| #1838 | Define the non-success outcomes of a de-flake attempt (#1439) | 2026-08-29T00:03:15Z | [legacy] | — | — | [docs/project_review_1989-1834.md](../project_review_1989-1834.md) | cursor:docs/project_review_boundaries.md; report:docs/project_review_1989-1834.md (operator-confirmed) |
| #1836 | Gate bare-name panel icon maps against the global runtime index and repair fallback contracts | 2026-08-28T17:09:07Z | [legacy] | — | — | [docs/project_review_1989-1834.md](../project_review_1989-1834.md) | cursor:docs/project_review_boundaries.md; report:docs/project_review_1989-1834.md (operator-confirmed) |
| #1835 | Re-point the F4 Layer A coverage checker at the split input modules | 2026-08-28T18:07:10Z | [legacy] | — | — | [docs/project_review_1989-1834.md](../project_review_1989-1834.md) | cursor:docs/project_review_boundaries.md; report:docs/project_review_1989-1834.md (operator-confirmed) |
| #1834 | Make the EngineEnv capability inventory's field total mechanically checked | 2026-08-28T15:55:23Z | [legacy] | — | — | [docs/project_review_1989-1834.md](../project_review_1989-1834.md) | cursor:docs/project_review_boundaries.md; report:docs/project_review_1989-1834.md (operator-confirmed) |
| #1832 | Gate the world_determinism content-identity self-test (#1724) | 2026-08-28T15:34:37Z | [legacy] | — | — | [docs/project_review_2000-1827.md](../project_review_2000-1827.md) | cursor:docs/project_review_boundaries.md; report:docs/project_review_2000-1827.md (operator-confirmed) |
| #1831 | Close the location anchor vocabulary into one type (#1681) | 2026-08-28T15:12:59Z | [legacy] | — | — | [docs/project_review_2000-1827.md](../project_review_2000-1827.md) | cursor:docs/project_review_boundaries.md; report:docs/project_review_2000-1827.md (operator-confirmed) |
| #1830 | Bound the ntfy notification job and its curl calls in elapsed time | 2026-08-28T14:50:33Z | [legacy] | — | — | [docs/project_review_2000-1827.md](../project_review_2000-1827.md) | cursor:docs/project_review_boundaries.md; report:docs/project_review_2000-1827.md (operator-confirmed) |
| #1829 | Decode world.getFluidAt by its arity contract in the probes that read it as a table | 2026-08-28T14:19:11Z | [legacy] | — | — | [docs/project_review_2000-1827.md](../project_review_2000-1827.md) | cursor:docs/project_review_boundaries.md; report:docs/project_review_2000-1827.md (operator-confirmed) |
| #1828 | Make the texture-path checker comment-aware and run it as a blocking gate | 2026-08-28T14:37:04Z | [legacy] | — | — | [docs/project_review_2000-1827.md](../project_review_2000-1827.md) | cursor:docs/project_review_boundaries.md; report:docs/project_review_2000-1827.md (operator-confirmed) |
| #1827 | Clear the global unit selection unconditionally on the gameplay Escape | 2026-08-28T14:01:30Z | [legacy] | — | — | [docs/project_review_2000-1827.md](../project_review_2000-1827.md) | cursor:docs/project_review_boundaries.md; report:docs/project_review_2000-1827.md (operator-confirmed) |
| #1826 | Pin the shipped concept catalogue's id inventory against removal, rename and unratcheted addition | 2026-08-28T13:46:29Z | [legacy] | — | — | — | cursor:docs/project_review_boundaries.md |
| #1825 | Scan the unit/building managers and UnitThreadState in the persistence inventory audit | 2026-08-28T13:30:15Z | [legacy] | — | — | — | cursor:docs/project_review_boundaries.md |
| #1824 | Preserve keyboard control focus across the Defaults and preview-arrival rebuilds | 2026-08-28T13:15:06Z | [legacy] | — | — | — | cursor:docs/project_review_boundaries.md |
| #1823 | Reject unusable LLM flavor output instead of recording it as a flavored persona | 2026-08-28T13:00:05Z | [legacy] | — | — | — | cursor:docs/project_review_boundaries.md |
| #1822 | Bound the #418 front-wall billboard lift to the slice the renderer draws | 2026-08-28T02:11:52Z | [legacy] | — | — | — | cursor:docs/project_review_boundaries.md |
| #1821 | Reject a non-positive location-content count or rolls instead of spawning nothing silently | 2026-08-28T12:46:25Z | [legacy] | — | — | — | cursor:docs/project_review_boundaries.md |
| #1820 | Pin UI.Clipping's UI scale and isolate its resource root (#1747) | 2026-08-28T01:56:09Z | [legacy] | — | — | — | cursor:docs/project_review_boundaries.md |
| #1819 | Reject a non-positive flora regrowth_time instead of accepting an infinite harvest loop | 2026-08-28T01:42:36Z | [legacy] | — | — | — | cursor:docs/project_review_boundaries.md |
| #1818 | Clear both deferred selection arms on a direct tile selection | 2026-08-28T01:20:49Z | [legacy] | — | — | — | cursor:docs/project_review_boundaries.md |
| #1817 | Prevent authored location bounds from overflowing during instance anchoring (#1796) | 2026-08-28T01:00:30Z | [legacy] | — | — | — | cursor:docs/project_review_boundaries.md |
| #1816 | Report a commanded move order abandoned by its stall budget (#1769) | 2026-08-28T00:39:08Z | [legacy] | — | — | — | cursor:docs/project_review_boundaries.md |
| #1815 | Apply the persisted borderless window mode at graphical startup | 2026-08-28T00:21:01Z | [legacy] | — | — | — | cursor:docs/project_review_boundaries.md |
| #1814 | Reject an out-of-domain explicit condition in item.spawnGround | 2026-08-27T23:58:23Z | [legacy] | — | — | — | cursor:docs/project_review_boundaries.md |
| #1813 | Retain a turn's drained post-step evidence when its screenshot fails | 2026-08-27T23:43:10Z | [legacy] | — | — | — | cursor:docs/project_review_boundaries.md |
| #1812 | Close the location content-kind vocabulary, removing nested structures (#1708) | 2026-08-27T21:55:14Z | [legacy] | — | — | — | cursor:docs/project_review_boundaries.md |
| #1811 | Make pending auto-harvest collection eligible for arbitration (#1743) | 2026-08-27T21:41:00Z | [legacy] | — | — | — | cursor:docs/project_review_boundaries.md |
| #1810 | Make the forage flora query and lookup seam-aware at the cylindrical U wrap | 2026-08-27T21:26:34Z | [legacy] | — | — | — | cursor:docs/project_review_boundaries.md |
| #1809 | Reject a quality_tiers override that cannot label every quality | 2026-08-27T21:10:42Z | [legacy] | — | — | — | cursor:docs/project_review_boundaries.md |
| #1808 | Invalidate pending activations on a real visible exclusivity change (#1748) | 2026-08-27T20:54:35Z | [legacy] | — | — | — | cursor:docs/project_review_boundaries.md |
| #1807 | Resolve combat's max_stamina through effective stats so equipped and innate modifiers apply | 2026-08-27T20:39:28Z | [legacy] | — | — | — | cursor:docs/project_review_boundaries.md |
| #1806 | Validate the structure placement target before interning its texture paths | 2026-08-27T20:22:20Z | [legacy] | — | — | — | cursor:docs/project_review_boundaries.md |
| #1805 | Reject non-finite unit.repairItem deltas instead of silently breaking or free-repairing an item | 2026-08-27T20:05:30Z | [legacy] | — | — | — | cursor:docs/project_review_boundaries.md |
| #1804 | Apply the dry-bank slope rule to wet neighbours across chunk seams | 2026-08-27T19:49:13Z | [legacy] | — | — | — | cursor:docs/project_review_boundaries.md |
| #1803 | Apply the facing-aware wrap offset to structure quads at the cylindrical seam | 2026-08-27T19:34:25Z | [legacy] | — | — | — | cursor:docs/project_review_boundaries.md |
| #1802 | Derive a consumable sip's effects from the drain the engine actually applied | 2026-08-27T19:18:07Z | [legacy] | — | — | [docs/project_review_2002-1783.md](../project_review_2002-1783.md) | cursor:docs/project_review_boundaries.md; report:docs/project_review_2002-1783.md (operator-confirmed) |
| #1801 | Give Lua tick intervals a finite-value policy the scheduler can honour | 2026-08-27T18:58:23Z | [legacy] | — | — | [docs/project_review_2002-1783.md](../project_review_2002-1783.md) | cursor:docs/project_review_boundaries.md; report:docs/project_review_2002-1783.md (operator-confirmed) |
| #1800 | Release loaded texture atlases at shutdown before the Vulkan device is destroyed | 2026-08-27T18:34:15Z | [legacy] | — | — | [docs/project_review_2002-1783.md](../project_review_2002-1783.md) | cursor:docs/project_review_boundaries.md; report:docs/project_review_2002-1783.md (operator-confirmed) |
| #1799 | Make F3 click correlation honour modal scope and pointer-blocking occlusion | 2026-08-27T18:13:14Z | [legacy] | — | — | [docs/project_review_2002-1783.md](../project_review_2002-1783.md) | cursor:docs/project_review_boundaries.md; report:docs/project_review_2002-1783.md (operator-confirmed) |
| #1798 | Generate an arena base from the seed it records (#1718) | 2026-08-27T17:53:10Z | [legacy] | — | — | [docs/project_review_2002-1783.md](../project_review_2002-1783.md) | cursor:docs/project_review_boundaries.md; report:docs/project_review_2002-1783.md (operator-confirmed) |
| #1797 | Establish a finite positive domain for material move_cost (#1734) | 2026-08-27T17:30:24Z | [legacy] | — | — | [docs/project_review_2002-1783.md](../project_review_2002-1783.md) | cursor:docs/project_review_boundaries.md; report:docs/project_review_2002-1783.md (operator-confirmed) |
| #1795 | Stop the craft-bill probe racing the auto-haul, and pin the craft identity contract | 2026-08-27T17:06:45Z | [legacy] | — | — | [docs/project_review_2002-1783.md](../project_review_2002-1783.md) | cursor:docs/project_review_boundaries.md; report:docs/project_review_2002-1783.md (operator-confirmed) |
| #1794 | Rotate structure wall identity with the camera (#1712) | 2026-08-27T16:47:08Z | [legacy] | — | — | [docs/project_review_2002-1783.md](../project_review_2002-1783.md) | cursor:docs/project_review_boundaries.md; report:docs/project_review_2002-1783.md (operator-confirmed) |
| #1792 | Give long save migration probe its own timeout | 2026-08-27T16:23:16Z | [legacy] | — | — | [docs/project_review_2002-1783.md](../project_review_2002-1783.md) | cursor:docs/project_review_boundaries.md; report:docs/project_review_2002-1783.md (operator-confirmed) |
| #1784 | Refuse a portal spawn whose page stopped being active after the tick's snapshot | 2026-08-27T16:08:03Z | [legacy] | — | — | [docs/project_review_2002-1783.md](../project_review_2002-1783.md) | cursor:docs/project_review_boundaries.md; report:docs/project_review_2002-1783.md (operator-confirmed) |
| #1783 | Keep the preview probe from stealing focus | 2026-08-27T15:43:15Z | [legacy] | — | — | [docs/project_review_2002-1783.md](../project_review_2002-1783.md) | cursor:docs/project_review_boundaries.md; report:docs/project_review_2002-1783.md (operator-confirmed) |
| #1779 | Mark a location stamped only when its geometry actually materialized | 2026-08-27T15:18:00Z | [legacy] | — | — | [docs/project_review_2003-1754.md](../project_review_2003-1754.md) | cursor:docs/project_review_boundaries.md; report:docs/project_review_2003-1754.md (operator-confirmed) |
| #1778 | Stop a successful autosave from clearing a pause another engine source imposed while it ran | 2026-08-27T13:30:18Z | [legacy] | — | — | [docs/project_review_2003-1754.md](../project_review_2003-1754.md) | cursor:docs/project_review_boundaries.md; report:docs/project_review_2003-1754.md (operator-confirmed) |
| #1777 | fix: refuse a texture handle the shader cannot resolve instead of reporting it loaded | 2026-08-27T14:32:42Z | [legacy] | — | — | [docs/project_review_2003-1754.md](../project_review_2003-1754.md) | cursor:docs/project_review_boundaries.md; report:docs/project_review_2003-1754.md (operator-confirmed) |
| #1776 | Dedup and count chunk-queue requests under one canonical seam identity | 2026-08-27T13:03:36Z | [legacy] | — | — | [docs/project_review_2003-1754.md](../project_review_2003-1754.md) | cursor:docs/project_review_boundaries.md; report:docs/project_review_2003-1754.md (operator-confirmed) |
| #1775 | Require the whole fixed bindless descriptor binding from every accepted device | 2026-08-27T14:54:06Z | [legacy] | — | — | [docs/project_review_2003-1754.md](../project_review_2003-1754.md) | cursor:docs/project_review_boundaries.md; report:docs/project_review_2003-1754.md (operator-confirmed) |
| #1774 | Give every attached UI element exactly one structural owner | 2026-08-27T13:54:52Z | [legacy] | — | — | [docs/project_review_2003-1754.md](../project_review_2003-1754.md) | cursor:docs/project_review_boundaries.md; report:docs/project_review_2003-1754.md (operator-confirmed) |
| #1773 | Keep mental effectiveness finite so NaN XP cannot bias combat or persist as item quality | 2026-08-27T12:30:26Z | [legacy] | — | — | [docs/project_review_2003-1754.md](../project_review_2003-1754.md) | cursor:docs/project_review_boundaries.md; report:docs/project_review_2003-1754.md (operator-confirmed) |
| #1764 | Let an in-flight pose transition finish a stop instead of erasing it | 2026-08-27T04:53:49Z | [legacy] | — | — | [docs/project_review_2003-1754.md](../project_review_2003-1754.md) | cursor:docs/project_review_boundaries.md; report:docs/project_review_2003-1754.md (operator-confirmed) |
| #1756 | Validate each decoded allocator's own floor, not only the ids beneath it | 2026-08-27T02:51:22Z | [legacy] | — | — | [docs/project_review_2003-1754.md](../project_review_2003-1754.md) | cursor:docs/project_review_boundaries.md; report:docs/project_review_2003-1754.md (operator-confirmed) |
| #1755 | Request swapchain recreation when the framebuffer size changes | 2026-08-27T02:11:28Z | [legacy] | — | — | [docs/project_review_2003-1754.md](../project_review_2003-1754.md) | cursor:docs/project_review_boundaries.md; report:docs/project_review_2003-1754.md (operator-confirmed) |
| #1754 | Enforce the one-positive-nutrition-mode invariant on food item definitions | 2026-08-27T00:30:07Z | [legacy] | — | — | [docs/project_review_2003-1754.md](../project_review_2003-1754.md) | cursor:docs/project_review_boundaries.md; report:docs/project_review_2003-1754.md (operator-confirmed) |
| #1753 | Capture a deferred mouse gesture's framebuffer press position at press | 2026-08-26T22:29:02Z | [legacy] | — | — | [docs/project_review_2004-1710.md](../project_review_2004-1710.md) | cursor:docs/project_review_boundaries.md; report:docs/project_review_2004-1710.md (operator-confirmed) |
| #1751 | Build the cached world-quad pass from the snapshot it is stamped with | 2026-08-26T20:37:28Z | [legacy] | — | — | [docs/project_review_2004-1710.md](../project_review_2004-1710.md) | cursor:docs/project_review_boundaries.md; report:docs/project_review_2004-1710.md (operator-confirmed) |
| #1749 | Retract the staged structure piece when the world thread declines its placement | 2026-08-26T19:31:52Z | [legacy] | — | — | [docs/project_review_2004-1710.md](../project_review_2004-1710.md) | cursor:docs/project_review_boundaries.md; report:docs/project_review_2004-1710.md (operator-confirmed) |
| #1742 | Give player events a stable sequence so the playtest oracle cannot silently lose rows | 2026-08-26T18:30:05Z | [legacy] | — | — | [docs/project_review_2004-1710.md](../project_review_2004-1710.md) | cursor:docs/project_review_boundaries.md; report:docs/project_review_2004-1710.md (operator-confirmed) |
| #1741 | flora: integrate saguaro into world generation | 2026-08-26T18:08:06Z | [legacy] | — | — | [docs/project_review_2004-1710.md](../project_review_2004-1710.md) | cursor:docs/project_review_boundaries.md; report:docs/project_review_2004-1710.md (operator-confirmed) |
| #1728 | Let a lunge observe its airborne phase so the landing strike can fire | 2026-08-26T13:37:19Z | [legacy] | — | — | [docs/project_review_2004-1710.md](../project_review_2004-1710.md) | cursor:docs/project_review_boundaries.md; report:docs/project_review_2004-1710.md (operator-confirmed) |
| #1727 | fix: stop publishing a failed bindless registration as a loaded texture | 2026-08-26T14:18:04Z | [legacy] | — | — | [docs/project_review_2004-1710.md](../project_review_2004-1710.md) | cursor:docs/project_review_boundaries.md; report:docs/project_review_2004-1710.md (operator-confirmed) |
| #1726 | fix: reserve texture handle zero as the missing-texture sentinel | 2026-08-26T13:19:28Z | [legacy] | — | — | [docs/project_review_2004-1710.md](../project_review_2004-1710.md) | cursor:docs/project_review_boundaries.md; report:docs/project_review_2004-1710.md (operator-confirmed) |
| #1725 | [flora] Create the saguaro texture set | 2026-08-26T05:22:06Z | [legacy] | — | — | [docs/project_review_2004-1710.md](../project_review_2004-1710.md) | cursor:docs/project_review_boundaries.md; report:docs/project_review_2004-1710.md (operator-confirmed) |
| #1710 | fix(craft): clear a dead claimant's craft bill independently of claim eligibility | 2026-08-25T18:58:47Z | [legacy] | — | — | [docs/project_review_2004-1710.md](../project_review_2004-1710.md) | cursor:docs/project_review_boundaries.md; report:docs/project_review_2004-1710.md (operator-confirmed) |
| #1700 | Refuse cross-page endpoint pairs in the four lax unit item verbs | 2026-08-26T04:05:01Z | [legacy] | — | — | [docs/project_review_2004-1710.md](../project_review_2004-1710.md) | cursor:docs/project_review_boundaries.md; report:docs/project_review_2004-1710.md (operator-confirmed) |
| #1698 | review-gate: decide staleness by the PR's own patch, not its file set | 2026-08-25T18:20:36Z | [legacy] | — | — | — | cursor:docs/project_review_boundaries.md |
| #1697 | building_spawn: enter the portal's failure path on unit.spawn's -1 sentinel | 2026-08-25T18:04:15Z | [legacy] | — | — | — | cursor:docs/project_review_boundaries.md |
| #1684 | Refuse diagnosis records outside a closed producer-provenance contract | 2026-08-25T16:39:36Z | [legacy] | — | — | [docs/project_review_1684-1656.md](../project_review_1684-1656.md) | report:docs/project_review_1684-1656.md (operator-confirmed) |
| #1683 | Resolve every phase of unitAi.commandPickup on the carrier's own page | 2026-08-25T16:24:54Z | [legacy] | — | — | [docs/project_review_1684-1656.md](../project_review_1684-1656.md) | cursor:docs/project_review_boundaries.md; report:docs/project_review_1684-1656.md (operator-confirmed) |
| #1678 | fix(save): reject a decoded location instance whose stored bounds are inverted | 2026-08-25T16:04:37Z | [legacy] | — | — | [docs/project_review_1684-1656.md](../project_review_1684-1656.md) | cursor:docs/project_review_boundaries.md; report:docs/project_review_1684-1656.md (operator-confirmed) |
| #1677 | fix: attach a loaded zoom atlas only to the page whose cache produced it | 2026-08-25T15:42:29Z | [legacy] | — | — | [docs/project_review_1684-1656.md](../project_review_1684-1656.md) | cursor:docs/project_review_boundaries.md; report:docs/project_review_1684-1656.md (operator-confirmed) |
| #1665 | Tie the foraging probe's round trip to its own save, in its own root | 2026-08-25T13:50:42Z | [legacy] | — | — | [docs/project_review_1684-1656.md](../project_review_1684-1656.md) | cursor:docs/project_review_boundaries.md; report:docs/project_review_1684-1656.md (operator-confirmed) |
| #1664 | Refuse to pass the till probe with its fluid rule unexercised | 2026-08-25T13:25:29Z | [legacy] | — | — | [docs/project_review_1684-1656.md](../project_review_1684-1656.md) | cursor:docs/project_review_boundaries.md; report:docs/project_review_1684-1656.md (operator-confirmed) |
| #1663 | Prove unit.injure attributes its event to the unit it wounded | 2026-08-25T13:11:33Z | [legacy] | — | — | [docs/project_review_1684-1656.md](../project_review_1684-1656.md) | cursor:docs/project_review_boundaries.md; report:docs/project_review_1684-1656.md (operator-confirmed) |
| #1662 | Emit the handoff from the process that measured | 2026-08-24T16:52:49Z | [legacy] | — | — | [docs/project_review_1684-1656.md](../project_review_1684-1656.md) | cursor:docs/project_review_boundaries.md; report:docs/project_review_1684-1656.md (operator-confirmed) |
| #1658 | Bound tutorial objective-row glyphs against the shipped labels, and fit the rows that overran (#1581) | 2026-08-24T14:11:57Z | [legacy] | — | — | [docs/project_review_1684-1656.md](../project_review_1684-1656.md) | cursor:docs/project_review_boundaries.md; report:docs/project_review_1684-1656.md (operator-confirmed) |
| #1657 | Give each retaliation-swap window its own staged fixture (#1578) | 2026-08-24T13:57:18Z | [legacy] | — | — | [docs/project_review_1684-1656.md](../project_review_1684-1656.md) | cursor:docs/project_review_boundaries.md; report:docs/project_review_1684-1656.md (operator-confirmed) |
| #1656 | List movement-probe courses without booting an engine | 2026-08-24T13:43:19Z | [legacy] | — | — | [docs/project_review_1684-1656.md](../project_review_1684-1656.md) | report:docs/project_review_1684-1656.md (operator-confirmed) |
| #1655 | Derive item_temp_probe's rate fixture from the observed ambient (#1611) | 2026-08-24T13:29:46Z | [legacy] | — | — | [docs/project_review_1655-1643.md](../project_review_1655-1643.md) | report:docs/project_review_1655-1643.md (operator-confirmed) |
| #1654 | Correct the position-hold probe's unit count in its docstring, inventory and classifier | 2026-08-24T13:13:12Z | [legacy] | — | — | [docs/project_review_1655-1643.md](../project_review_1655-1643.md) | cursor:docs/project_review_boundaries.md; report:docs/project_review_1655-1643.md (operator-confirmed) |
| #1653 | Gate the injury-log probe on a real fall's injury event | 2026-08-24T04:22:29Z | [legacy] | — | — | [docs/project_review_1655-1643.md](../project_review_1655-1643.md) | cursor:docs/project_review_boundaries.md; report:docs/project_review_1655-1643.md (operator-confirmed) |
| #1652 | Decide mechanically whether a measured flake is the probe's fault | 2026-08-25T00:52:55Z | [legacy] | — | — | [docs/project_review_1684-1656.md](../project_review_1684-1656.md) | cursor:docs/project_review_boundaries.md; report:docs/project_review_1684-1656.md (operator-confirmed) |
| #1651 | Fail the etymology probe when a required entity is absent | 2026-08-24T04:08:17Z | [legacy] | — | — | [docs/project_review_1655-1643.md](../project_review_1655-1643.md) | cursor:docs/project_review_boundaries.md; report:docs/project_review_1655-1643.md (operator-confirmed) |
| #1650 | Give the construction footprint probe a site it can actually render on | 2026-08-24T02:24:02Z | [legacy] | — | — | [docs/project_review_1655-1643.md](../project_review_1655-1643.md) | cursor:docs/project_review_boundaries.md; report:docs/project_review_1655-1643.md (operator-confirmed) |
| #1649 | Give the embark probe an isolated resource root and remove every artifact it creates | 2026-08-24T02:10:37Z | [legacy] | — | — | [docs/project_review_1655-1643.md](../project_review_1655-1643.md) | cursor:docs/project_review_boundaries.md; report:docs/project_review_1655-1643.md (operator-confirmed) |
| #1648 | Tie the item-temperature probe's save/load round trip to its own request and an isolated root | 2026-08-24T01:55:57Z | [legacy] | — | — | [docs/project_review_1655-1643.md](../project_review_1655-1643.md) | cursor:docs/project_review_boundaries.md; report:docs/project_review_1655-1643.md (operator-confirmed) |
| #1647 | Tie the farm-AI and flora-growth save/load round trips to their own requests and isolated roots | 2026-08-24T00:07:39Z | [legacy] | — | — | [docs/project_review_1655-1643.md](../project_review_1655-1643.md) | cursor:docs/project_review_boundaries.md; report:docs/project_review_1655-1643.md (operator-confirmed) |
| #1646 | Give the graphical GLFW spec project-owned window coverage and label its environment checks | 2026-08-23T23:45:16Z | [legacy] | — | — | [docs/project_review_1655-1643.md](../project_review_1655-1643.md) | cursor:docs/project_review_boundaries.md; report:docs/project_review_1655-1643.md (operator-confirmed) |
| #1645 | Make the location-stamp probe prove its footprint materialized (#1575) | 2026-08-23T23:30:42Z | [legacy] | — | — | [docs/project_review_1655-1643.md](../project_review_1655-1643.md) | cursor:docs/project_review_boundaries.md; report:docs/project_review_1655-1643.md (operator-confirmed) |
| #1644 | Judge the blood lifecycle probe's save-load path by ownership | 2026-08-23T23:15:52Z | [legacy] | — | — | [docs/project_review_1655-1643.md](../project_review_1655-1643.md) | cursor:docs/project_review_boundaries.md; report:docs/project_review_1655-1643.md (operator-confirmed) |
| #1643 | Tie the item-instance probe's save/load round trip to its own request and an isolated root | 2026-08-23T23:01:15Z | [legacy] | — | — | [docs/project_review_1655-1643.md](../project_review_1655-1643.md) | report:docs/project_review_1655-1643.md (operator-confirmed) |
| #1642 | Fix the save-pause probe's vacuous resumed-speed oracle | 2026-08-23T22:47:50Z | [legacy] | — | — | [docs/project_review_1642-1631.md](../project_review_1642-1631.md) | report:docs/project_review_1642-1631.md (operator-confirmed) |
| #1641 | Correct the freshwater slope flatten's documented condition and cover every case it catches | 2026-08-23T22:33:10Z | [legacy] | — | — | [docs/project_review_1642-1631.md](../project_review_1642-1631.md) | cursor:docs/project_review_boundaries.md; report:docs/project_review_1642-1631.md (operator-confirmed) |
| #1640 | Select the graphical device spec's adapter with pickPhysicalDevice instead of enumeration order | 2026-08-23T21:38:07Z | [legacy] | — | — | [docs/project_review_1642-1631.md](../project_review_1642-1631.md) | cursor:docs/project_review_boundaries.md; report:docs/project_review_1642-1631.md (operator-confirmed) |
| #1639 | Scale auto-harvest by the farming skill (#1582) | 2026-08-23T21:17:40Z | [legacy] | — | — | [docs/project_review_1642-1631.md](../project_review_1642-1631.md) | cursor:docs/project_review_boundaries.md; report:docs/project_review_1642-1631.md (operator-confirmed) |
| #1638 | Re-evidence the CI probe-gate demotions inherited from direct commit b09c1518 | 2026-08-23T20:57:57Z | [legacy] | — | — | [docs/project_review_1642-1631.md](../project_review_1642-1631.md) | cursor:docs/project_review_boundaries.md; report:docs/project_review_1642-1631.md (operator-confirmed) |
| #1637 | Add an exact-instance player drink gesture for coffee consumables | 2026-08-23T20:39:12Z | [legacy] | — | — | [docs/project_review_1642-1631.md](../project_review_1642-1631.md) | cursor:docs/project_review_boundaries.md; report:docs/project_review_1642-1631.md (operator-confirmed) |
| #1636 | Restore executable coverage for the settings Revert contract and retire its stale offline harness | 2026-08-23T20:18:06Z | [legacy] | — | — | [docs/project_review_1642-1631.md](../project_review_1642-1631.md) | cursor:docs/project_review_boundaries.md; report:docs/project_review_1642-1631.md (operator-confirmed) |
| #1635 | Cover Engine.Core.Queue's blocking read and timeout behavior | 2026-08-23T19:57:12Z | [legacy] | — | — | [docs/project_review_1642-1631.md](../project_review_1642-1631.md) | cursor:docs/project_review_boundaries.md; report:docs/project_review_1642-1631.md (operator-confirmed) |
| #1634 | Fail baseline capture instead of recording an arbitrary sample of a varying strict invariant | 2026-08-23T17:32:36Z | [legacy] | — | — | [docs/project_review_1642-1631.md](../project_review_1642-1631.md) | cursor:docs/project_review_boundaries.md; report:docs/project_review_1642-1631.md (operator-confirmed) |
| #1633 | Clear session-owned Lua entity tables at one declared Exit-to-Menu boundary | 2026-08-23T19:31:36Z | [legacy] | — | — | [docs/project_review_1642-1631.md](../project_review_1642-1631.md) | cursor:docs/project_review_boundaries.md; report:docs/project_review_1642-1631.md (operator-confirmed) |
| #1632 | Tie the four location probes' save/load fixtures to their own requests and isolated roots | 2026-08-23T15:58:48Z | [legacy] | — | — | [docs/project_review_1630-1614.md](../project_review_1630-1614.md) | cursor:docs/project_review_boundaries.md; report:docs/project_review_1630-1614.md (operator-confirmed) |
| #1631 | Reconcile every persisted unit-AI reference family at the post-load boundary | 2026-08-23T17:10:57Z | [legacy] | — | — | [docs/project_review_1642-1631.md](../project_review_1642-1631.md) | report:docs/project_review_1642-1631.md (operator-confirmed) |
| #1630 | Isolate concurrent probe launches from the shared Cabal build directory | 2026-08-23T16:19:04Z | [legacy] | — | — | [docs/project_review_1630-1614.md](../project_review_1630-1614.md) | report:docs/project_review_1630-1614.md (operator-confirmed) |
| #1629 | Refuse a construction designation on an already-designated tile | 2026-08-23T15:23:38Z | [legacy] | — | — | [docs/project_review_1630-1614.md](../project_review_1630-1614.md) | cursor:docs/project_review_boundaries.md; report:docs/project_review_1630-1614.md (operator-confirmed) |
| #1628 | fix: clear the pending popup queue on teardown even when no card is active | 2026-08-23T15:05:12Z | [legacy] | — | — | [docs/project_review_1630-1614.md](../project_review_1630-1614.md) | cursor:docs/project_review_boundaries.md; report:docs/project_review_1630-1614.md (operator-confirmed) |
| #1627 | Restore the player's chosen world speed after any pause (#1599) | 2026-08-23T14:43:48Z | [legacy] | — | — | [docs/project_review_1630-1614.md](../project_review_1630-1614.md) | cursor:docs/project_review_boundaries.md; report:docs/project_review_1630-1614.md (operator-confirmed) |
| #1626 | fix: map negative-infinite step costs to the ceiling instead of a free step | 2026-08-23T14:22:05Z | [legacy] | — | — | [docs/project_review_1630-1614.md](../project_review_1630-1614.md) | cursor:docs/project_review_boundaries.md; report:docs/project_review_1630-1614.md (operator-confirmed) |
| #1625 | fix: key the nested item-contents signature on child quality and weight | 2026-08-23T13:14:27Z | [legacy] | — | — | [docs/project_review_1630-1614.md](../project_review_1630-1614.md) | cursor:docs/project_review_boundaries.md; report:docs/project_review_1630-1614.md (operator-confirmed) |
| #1624 | Bind one build placement to the page its click hit-tested (#1602) | 2026-08-23T19:02:13Z | [legacy] | — | — | [docs/project_review_1642-1631.md](../project_review_1642-1631.md) | cursor:docs/project_review_boundaries.md; report:docs/project_review_1642-1631.md (operator-confirmed) |
| #1623 | Restore the lenient-UTF-8 sweep in unit.moveTo and gate it in CI | 2026-08-23T14:04:14Z | [legacy] | — | — | [docs/project_review_1630-1614.md](../project_review_1630-1614.md) | cursor:docs/project_review_boundaries.md; report:docs/project_review_1630-1614.md (operator-confirmed) |
| #1622 | fix: reject a fluid writeback computed before the live edit it would overwrite | 2026-08-23T11:05:38Z | [legacy] | — | — | [docs/project_review_1630-1614.md](../project_review_1630-1614.md) | cursor:docs/project_review_boundaries.md; report:docs/project_review_1630-1614.md (operator-confirmed) |
| #1619 | tools: reserve each parallel probe's full port span and honour --port with --jobs | 2026-08-23T10:42:25Z | [legacy] | — | — | [docs/project_review_1630-1614.md](../project_review_1630-1614.md) | cursor:docs/project_review_boundaries.md; report:docs/project_review_1630-1614.md (operator-confirmed) |
| #1615 | fix(worldgen): exclude indestructible neighbours from final-age soil shed credit | 2026-08-23T10:23:43Z | [legacy] | — | — | [docs/project_review_1630-1614.md](../project_review_1630-1614.md) | cursor:docs/project_review_boundaries.md; report:docs/project_review_1630-1614.md (operator-confirmed) |
| #1614 | Resolve unit movement, re-ground, and wound infection from the unit's own world page | 2026-08-23T10:01:52Z | [legacy] | — | — | [docs/project_review_1630-1614.md](../project_review_1630-1614.md) | report:docs/project_review_1630-1614.md (operator-confirmed) |
| #1606 | Carry the source world page through popup coordinates so a replayed event cannot pan the wrong world | 2026-08-23T09:38:07Z | [legacy] | — | — | [docs/project_review_2007-1456.md](../project_review_2007-1456.md) | cursor:docs/project_review_boundaries.md; report:docs/project_review_2007-1456.md (operator-confirmed) |
| #1601 | tools: rename the five river diagnostics off the test_* prefix | 2026-08-23T08:42:09Z | [legacy] | — | — | [docs/project_review_2007-1456.md](../project_review_2007-1456.md) | cursor:docs/project_review_boundaries.md; report:docs/project_review_2007-1456.md (operator-confirmed) |
| #1574 | /deflake: select, claim, measure, record, release (#1436) | 2026-08-22T22:46:18Z | [legacy] | — | — | [docs/project_review_2007-1456.md](../project_review_2007-1456.md) | cursor:docs/project_review_boundaries.md; report:docs/project_review_2007-1456.md (operator-confirmed) |
| #1568 | Define tshow once in UPrelude and replace the hand-written T.pack (show x) sites | 2026-08-22T22:23:50Z | [legacy] | — | — | [docs/project_review_2007-1456.md](../project_review_2007-1456.md) | cursor:docs/project_review_boundaries.md; report:docs/project_review_2007-1456.md (operator-confirmed) |
| #1567 | Correct consonantOnly's stale claim that bound-form legality shares its cluster scope | 2026-08-22T21:59:14Z | [legacy] | — | — | [docs/project_review_2007-1456.md](../project_review_2007-1456.md) | cursor:docs/project_review_boundaries.md; report:docs/project_review_2007-1456.md (operator-confirmed) |
| #1566 | Delete the river-identity self-comparison and its unachievable stability comment | 2026-08-22T21:42:07Z | [legacy] | — | — | [docs/project_review_2007-1456.md](../project_review_2007-1456.md) | cursor:docs/project_review_boundaries.md; report:docs/project_review_2007-1456.md (operator-confirmed) |
| #1565 | Select the next probe by the priority ladder (#1435) | 2026-08-22T19:59:23Z | [legacy] | — | — | [docs/project_review_2007-1456.md](../project_review_2007-1456.md) | cursor:docs/project_review_boundaries.md; report:docs/project_review_2007-1456.md (operator-confirmed) |
| #1564 | Narrow the over-wide src/Blood, src/Language, and src/Item export lists | 2026-08-22T19:45:04Z | [legacy] | — | — | [docs/project_review_2007-1456.md](../project_review_2007-1456.md) | cursor:docs/project_review_boundaries.md; report:docs/project_review_2007-1456.md (operator-confirmed) |
| #1563 | Pin one location-name vector and delete the naming self-comparison | 2026-08-22T19:24:13Z | [legacy] | — | — | [docs/project_review_2007-1456.md](../project_review_2007-1456.md) | cursor:docs/project_review_boundaries.md; report:docs/project_review_2007-1456.md (operator-confirmed) |
| #1562 | Align the three stale hydrology source comments with the authoritative pipeline map | 2026-08-22T19:07:29Z | [legacy] | — | — | [docs/project_review_2007-1456.md](../project_review_2007-1456.md) | cursor:docs/project_review_boundaries.md; report:docs/project_review_2007-1456.md (operator-confirmed) |
| #1561 | Point the enum-reorder mitigation at per-component migration | 2026-08-22T18:48:17Z | [legacy] | — | — | [docs/project_review_2007-1456.md](../project_review_2007-1456.md) | cursor:docs/project_review_boundaries.md; report:docs/project_review_2007-1456.md (operator-confirmed) |
| #1560 | Correct logEntryWith's Haddock reference to the removed call-site skip list | 2026-08-22T18:32:11Z | [legacy] | — | — | [docs/project_review_2007-1456.md](../project_review_2007-1456.md) | cursor:docs/project_review_boundaries.md; report:docs/project_review_2007-1456.md (operator-confirmed) |
| #1559 | Replace the two tautological determinism assertions in the visual-helper suites | 2026-08-22T18:03:35Z | [legacy] | — | — | [docs/project_review_2007-1456.md](../project_review_2007-1456.md) | cursor:docs/project_review_boundaries.md; report:docs/project_review_2007-1456.md (operator-confirmed) |
| #1558 | Delete the guaranteed-placement self-comparison and its false parity comment | 2026-08-22T17:50:24Z | [legacy] | — | — | [docs/project_review_2007-1456.md](../project_review_2007-1456.md) | cursor:docs/project_review_boundaries.md; report:docs/project_review_2007-1456.md (operator-confirmed) |
| #1557 | Delete the building spawn/preview self-comparison and correct its overstated comment | 2026-08-22T17:24:07Z | [legacy] | — | — | [docs/project_review_2007-1456.md](../project_review_2007-1456.md) | cursor:docs/project_review_boundaries.md; report:docs/project_review_2007-1456.md (operator-confirmed) |
| #1556 | Pin the negative-context loot draw and delete the return-type tautology | 2026-08-22T17:11:29Z | [legacy] | — | — | [docs/project_review_2007-1456.md](../project_review_2007-1456.md) | cursor:docs/project_review_boundaries.md; report:docs/project_review_2007-1456.md (operator-confirmed) |
| #1555 | Correct shutdownEngine's stale Vulkan-safety comment about worker shutdown order | 2026-08-22T16:58:44Z | [legacy] | — | — | [docs/project_review_2007-1456.md](../project_review_2007-1456.md) | cursor:docs/project_review_boundaries.md; report:docs/project_review_2007-1456.md (operator-confirmed) |
| #1554 | ci: rotate project cache every eight changes | 2026-08-22T16:37:55Z | [legacy] | — | — | [docs/project_review_2007-1456.md](../project_review_2007-1456.md) | cursor:docs/project_review_boundaries.md; report:docs/project_review_2007-1456.md (operator-confirmed) |
| #1553 | test: replace the three tautological language-suite assertions | 2026-08-22T16:15:28Z | [legacy] | — | — | [docs/project_review_2007-1456.md](../project_review_2007-1456.md) | cursor:docs/project_review_boundaries.md; report:docs/project_review_2007-1456.md (operator-confirmed) |
| #1552 | test: replace the four tautological determinism assertions in the unit and blood helper suites | 2026-08-22T14:24:33Z | [legacy] | — | — | [docs/project_review_2007-1456.md](../project_review_2007-1456.md) | cursor:docs/project_review_boundaries.md; report:docs/project_review_2007-1456.md (operator-confirmed) |
| #1551 | refactor: move baseTileW/baseTileH into World.Grid | 2026-08-22T14:07:52Z | [legacy] | — | — | [docs/project_review_2007-1456.md](../project_review_2007-1456.md) | cursor:docs/project_review_boundaries.md; report:docs/project_review_2007-1456.md (operator-confirmed) |
| #1550 | Claim a probe atomically so parallel deflake agents do not collide | 2026-08-22T17:37:52Z | [legacy] | — | — | [docs/project_review_2007-1456.md](../project_review_2007-1456.md) | cursor:docs/project_review_boundaries.md; report:docs/project_review_2007-1456.md (operator-confirmed) |
| #1549 | tools: generate and audit the manual-only probe census page (#1431) | 2026-08-22T13:46:12Z | [legacy] | — | — | [docs/project_review_2007-1456.md](../project_review_2007-1456.md) | cursor:docs/project_review_boundaries.md; report:docs/project_review_2007-1456.md (operator-confirmed) |
| #1548 | refactor(world): route chunk derivation through globalToChunk (#1113) | 2026-08-22T13:26:25Z | [legacy] | — | — | [docs/project_review_2007-1456.md](../project_review_2007-1456.md) | cursor:docs/project_review_boundaries.md; report:docs/project_review_2007-1456.md (operator-confirmed) |
| #1547 | locations: give the bounds validity rule one home, drop three unused exports | 2026-08-22T13:02:42Z | [legacy] | — | — | [docs/project_review_1547-1535.md](../project_review_1547-1535.md) | report:docs/project_review_1547-1535.md (operator-confirmed) |
| #1546 | ci: keep docs-only checks out of Cabal | 2026-08-22T12:43:22Z | [legacy] | — | — | [docs/project_review_1547-1535.md](../project_review_1547-1535.md) | cursor:docs/project_review_boundaries.md; report:docs/project_review_1547-1535.md (operator-confirmed) |
| #1545 | Characterize probe measurement under concurrency and RTS overrides (#1427) | 2026-08-22T08:15:13Z | [legacy] | — | — | [docs/project_review_1547-1535.md](../project_review_1547-1535.md) | cursor:docs/project_review_boundaries.md; report:docs/project_review_1547-1535.md (operator-confirmed) |
| #1544 | Test the flora lifespan mixer by field sensitivity, not against itself | 2026-08-22T07:55:24Z | [legacy] | — | — | [docs/project_review_1547-1535.md](../project_review_1547-1535.md) | cursor:docs/project_review_boundaries.md; report:docs/project_review_1547-1535.md (operator-confirmed) |
| #1543 | Name the three focus systems in all five focus modules and rename the two shell-focus ones | 2026-08-22T07:38:30Z | [legacy] | — | — | [docs/project_review_1547-1535.md](../project_review_1547-1535.md) | cursor:docs/project_review_boundaries.md; report:docs/project_review_1547-1535.md (operator-confirmed) |
| #1542 | Start playtest budgets after the first player-ready frame (#1539) | 2026-08-22T07:09:23Z | [legacy] | — | — | [docs/project_review_1547-1535.md](../project_review_1547-1535.md) | cursor:docs/project_review_boundaries.md; report:docs/project_review_1547-1535.md (operator-confirmed) |
| #1541 | Replace the climate self-comparison with a finiteness check and one absolute anchor | 2026-08-22T06:49:59Z | [legacy] | — | — | [docs/project_review_1547-1535.md](../project_review_1547-1535.md) | cursor:docs/project_review_boundaries.md; report:docs/project_review_1547-1535.md (operator-confirmed) |
| #1540 | Move the shared physics constants out of the injury model and narrow the Unit/Combat export lists | 2026-08-22T06:30:22Z | [legacy] | — | — | [docs/project_review_1547-1535.md](../project_review_1547-1535.md) | cursor:docs/project_review_boundaries.md; report:docs/project_review_1547-1535.md (operator-confirmed) |
| #1538 | Narrow the over-wide src/World/ export lists | 2026-08-22T06:07:03Z | [legacy] | — | — | [docs/project_review_1547-1535.md](../project_review_1547-1535.md) | cursor:docs/project_review_boundaries.md; report:docs/project_review_1547-1535.md (operator-confirmed) |
| #1537 | Pin the location-instance identity mapping instead of comparing it to itself | 2026-08-22T05:41:11Z | [legacy] | — | — | [docs/project_review_1547-1535.md](../project_review_1547-1535.md) | cursor:docs/project_review_boundaries.md; report:docs/project_review_1547-1535.md (operator-confirmed) |
| #1536 | ci: run behavior probes in parallel with tests | 2026-08-22T11:38:42Z | [legacy] | — | — | [docs/project_review_1547-1535.md](../project_review_1547-1535.md) | cursor:docs/project_review_boundaries.md; report:docs/project_review_1547-1535.md (operator-confirmed) |
| #1535 | Pin blood texture pixels and pool placement instead of comparing calls to themselves | 2026-08-22T05:24:49Z | [legacy] | — | — | [docs/project_review_1547-1535.md](../project_review_1547-1535.md) | report:docs/project_review_1547-1535.md (operator-confirmed) |
| #1534 | Give the eight world-render quad sites one vertex-construction helper | 2026-08-22T05:07:30Z | [legacy] | — | — | [docs/project_review_2007-1456.md](../project_review_2007-1456.md) | cursor:docs/project_review_boundaries.md; report:docs/project_review_2007-1456.md (operator-confirmed) |
| #1533 | Replace the eleven tautological determinism assertions in Language.Generated | 2026-08-22T04:16:59Z | [legacy] | — | — | [docs/project_review_2007-1456.md](../project_review_2007-1456.md) | cursor:docs/project_review_boundaries.md; report:docs/project_review_2007-1456.md (operator-confirmed) |
| #1532 | Declare the power node role and rating in the building YAML | 2026-08-22T04:42:14Z | [legacy] | — | — | [docs/project_review_2007-1456.md](../project_review_2007-1456.md) | cursor:docs/project_review_boundaries.md; report:docs/project_review_2007-1456.md (operator-confirmed) |
| #1531 | Prove world init really wires the location overlay (#1375) | 2026-08-22T03:59:00Z | [legacy] | — | — | [docs/project_review_2007-1456.md](../project_review_2007-1456.md) | cursor:docs/project_review_boundaries.md; report:docs/project_review_2007-1456.md (operator-confirmed) |
| #1530 | Run the three GPU-free specs in test-headless; record test/ as build-only | 2026-08-22T03:37:52Z | [legacy] | — | — | [docs/project_review_2007-1456.md](../project_review_2007-1456.md) | cursor:docs/project_review_boundaries.md; report:docs/project_review_2007-1456.md (operator-confirmed) |
| #1529 | Set the per-probe acceptable-failure policy (X) | 2026-08-22T01:42:52Z | [legacy] | — | — | [docs/project_review_2007-1456.md](../project_review_2007-1456.md) | cursor:docs/project_review_boundaries.md; report:docs/project_review_2007-1456.md (operator-confirmed) |
| #1528 | Delete the two prune functions no production path can correctly call | 2026-08-22T01:16:23Z | [legacy] | — | — | [docs/project_review_2007-1456.md](../project_review_2007-1456.md) | cursor:docs/project_review_boundaries.md; report:docs/project_review_2007-1456.md (operator-confirmed) |
| #1527 | Assert the value GLFW's clock setter was given | 2026-08-22T00:52:11Z | [legacy] | — | — | [docs/project_review_2007-1456.md](../project_review_2007-1456.md) | cursor:docs/project_review_boundaries.md; report:docs/project_review_2007-1456.md (operator-confirmed) |
| #1526 | Give WorldPageId a field accessor and delete the eleven hand-written unwrappers | 2026-08-21T23:05:14Z | [legacy] | — | — | [docs/project_review_2007-1456.md](../project_review_2007-1456.md) | cursor:docs/project_review_boundaries.md; report:docs/project_review_2007-1456.md (operator-confirmed) |
| #1523 | Reconcile the probe census's cross-field invariants | 2026-08-21T22:37:14Z | [legacy] | — | — | [docs/project_review_2007-1456.md](../project_review_2007-1456.md) | cursor:docs/project_review_boundaries.md; report:docs/project_review_2007-1456.md (operator-confirmed) |
| #1522 | Define cohort and staleness semantics for census records | 2026-08-21T21:24:29Z | [legacy] | — | — | [docs/project_review_2007-1456.md](../project_review_2007-1456.md) | cursor:docs/project_review_boundaries.md; report:docs/project_review_2007-1456.md (operator-confirmed) |
| #1521 | Run the w128 volcano exposure regression on worldgen-selected CI runs | 2026-08-21T21:53:59Z | [legacy] | — | — | [docs/project_review_2007-1456.md](../project_review_2007-1456.md) | cursor:docs/project_review_boundaries.md; report:docs/project_review_2007-1456.md (operator-confirmed) |
| #1520 | Detect probes with related work already in flight | 2026-08-21T19:31:12Z | [legacy] | — | — | [docs/project_review_2007-1456.md](../project_review_2007-1456.md) | cursor:docs/project_review_boundaries.md; report:docs/project_review_2007-1456.md (operator-confirmed) |
| #1519 | Validate the probe census against a declared schema, not hand-rolled checks | 2026-08-21T19:08:21Z | [legacy] | — | — | [docs/project_review_2007-1456.md](../project_review_2007-1456.md) | cursor:docs/project_review_boundaries.md; report:docs/project_review_2007-1456.md (operator-confirmed) |
| #1518 | Select the save-compat repl reproducibility test by changed paths | 2026-08-21T18:39:19Z | [legacy] | — | — | [docs/project_review_2007-1456.md](../project_review_2007-1456.md) | cursor:docs/project_review_boundaries.md; report:docs/project_review_2007-1456.md (operator-confirmed) |
| #1517 | Select only the boot smoke for UI widget-kit changes (#1365) | 2026-08-21T18:12:28Z | [legacy] | — | — | [docs/project_review_2007-1456.md](../project_review_2007-1456.md) | cursor:docs/project_review_boundaries.md; report:docs/project_review_2007-1456.md (operator-confirmed) |
| #1516 | Add bounded dual-provider playtest usage tracking | 2026-08-21T17:44:13Z | [legacy] | — | — | [docs/project_review_2007-1456.md](../project_review_2007-1456.md) | cursor:docs/project_review_boundaries.md; report:docs/project_review_2007-1456.md (operator-confirmed) |
| #1515 | Give clamp and formatGameTimeHMS one definition each in scripts/lib | 2026-08-21T17:27:29Z | [legacy] | — | — | [docs/project_review_2007-1456.md](../project_review_2007-1456.md) | cursor:docs/project_review_boundaries.md; report:docs/project_review_2007-1456.md (operator-confirmed) |
| #1514 | Follow first-aid treatment to a stable-or-terminal outcome (#1221) | 2026-08-21T17:04:23Z | [legacy] | — | — | [docs/project_review_2007-1456.md](../project_review_2007-1456.md) | cursor:docs/project_review_boundaries.md; report:docs/project_review_2007-1456.md (operator-confirmed) |
| #1513 | Migrate the thermo_altitude probe to probe-result/v1 | 2026-08-21T16:37:48Z | [legacy] | — | — | [docs/project_review_2007-1456.md](../project_review_2007-1456.md) | cursor:docs/project_review_boundaries.md; report:docs/project_review_2007-1456.md (operator-confirmed) |
| #1512 | Read the Codex $test record for a probe, read-only | 2026-08-21T15:37:20Z | [legacy] | — | — | [docs/project_review_2007-1456.md](../project_review_2007-1456.md) | cursor:docs/project_review_boundaries.md; report:docs/project_review_2007-1456.md (operator-confirmed) |
| #1511 | Report which restore outcome each CI cache got (#1358) | 2026-08-21T16:08:33Z | [legacy] | — | — | [docs/project_review_2007-1456.md](../project_review_2007-1456.md) | cursor:docs/project_review_boundaries.md; report:docs/project_review_2007-1456.md (operator-confirmed) |
| #1510 | Stop a save-migration fixture after its load prerequisite fails | 2026-08-21T14:43:54Z | [legacy] | — | — | [docs/project_review_2007-1456.md](../project_review_2007-1456.md) | cursor:docs/project_review_boundaries.md; report:docs/project_review_2007-1456.md (operator-confirmed) |
| #1509 | Run the probe-protocol self-test in CI and make ci | 2026-08-21T15:10:32Z | [legacy] | — | — | [docs/project_review_2007-1456.md](../project_review_2007-1456.md) | cursor:docs/project_review_boundaries.md; report:docs/project_review_2007-1456.md (operator-confirmed) |
| #1508 | Pin each fall-survival example to its own measured value (#1412) | 2026-08-21T14:13:57Z | [legacy] | — | — | [docs/project_review_2007-1456.md](../project_review_2007-1456.md) | cursor:docs/project_review_boundaries.md; report:docs/project_review_2007-1456.md (operator-confirmed) |
| #1507 | Stop probe_census --set-acceptable-failures from clearing the stored justification | 2026-08-21T13:56:39Z | [legacy] | — | — | [docs/project_review_2007-1456.md](../project_review_2007-1456.md) | cursor:docs/project_review_boundaries.md; report:docs/project_review_2007-1456.md (operator-confirmed) |
| #1506 | Treat ordinary test-only paths as probe-neutral in the CI probe selector | 2026-08-21T08:39:00Z | [legacy] | — | — | [docs/project_review_2007-1456.md](../project_review_2007-1456.md) | cursor:docs/project_review_boundaries.md; report:docs/project_review_2007-1456.md (operator-confirmed) |
| #1505 | Fail the unified-transfer probe on unexpected persistence integrity diagnostics | 2026-08-21T08:14:32Z | [legacy] | — | — | [docs/project_review_2007-1456.md](../project_review_2007-1456.md) | cursor:docs/project_review_boundaries.md; report:docs/project_review_2007-1456.md (operator-confirmed) |
| #1504 | Reject a malformed --region instead of dumping the default region | 2026-08-21T07:52:19Z | [legacy] | — | — | [docs/project_review_2007-1456.md](../project_review_2007-1456.md) | cursor:docs/project_review_boundaries.md; report:docs/project_review_2007-1456.md (operator-confirmed) |
| #1503 | Define the probe census record and its atomic write path | 2026-08-21T07:23:02Z | [legacy] | — | — | [docs/project_review_2007-1456.md](../project_review_2007-1456.md) | cursor:docs/project_review_boundaries.md; report:docs/project_review_2007-1456.md (operator-confirmed) |
| #1502 | Select the worldgen gate for the simulation and world-thread stages the dump reads | 2026-08-21T07:00:01Z | [legacy] | — | — | [docs/project_review_2007-1456.md](../project_review_2007-1456.md) | cursor:docs/project_review_boundaries.md; report:docs/project_review_2007-1456.md (operator-confirmed) |
| #1501 | Pin every third-party GitHub Action by commit SHA, bumping the three docker actions first | 2026-08-21T06:30:09Z | [legacy] | — | — | [docs/project_review_2007-1456.md](../project_review_2007-1456.md) | cursor:docs/project_review_boundaries.md; report:docs/project_review_2007-1456.md (operator-confirmed) |
| #1500 | Provision every load-validated registry in the save-migration probe bootstrap | 2026-08-21T06:04:17Z | [legacy] | — | — | [docs/project_review_2007-1456.md](../project_review_2007-1456.md) | cursor:docs/project_review_boundaries.md; report:docs/project_review_2007-1456.md (operator-confirmed) |
| #1499 | Register the already-migrated position_hold probe as protocol-compatible | 2026-08-21T05:38:26Z | [legacy] | — | — | [docs/project_review_2007-1456.md](../project_review_2007-1456.md) | cursor:docs/project_review_boundaries.md; report:docs/project_review_2007-1456.md (operator-confirmed) |
| #1498 | Give the retaliation swap its window back, and a gate for it | 2026-08-21T05:11:45Z | [legacy] | — | — | [docs/project_review_2007-1456.md](../project_review_2007-1456.md) | cursor:docs/project_review_boundaries.md; report:docs/project_review_2007-1456.md (operator-confirmed) |
| #1497 | Clear the cached build and store targets when no target resolves | 2026-08-21T04:49:52Z | [legacy] | — | — | [docs/project_review_2007-1456.md](../project_review_2007-1456.md) | cursor:docs/project_review_boundaries.md; report:docs/project_review_2007-1456.md (operator-confirmed) |
| #1496 | Describe the real fresh-world arrival in the manual and the portal objective | 2026-08-21T04:22:02Z | [legacy] | — | — | [docs/project_review_2007-1456.md](../project_review_2007-1456.md) | cursor:docs/project_review_boundaries.md; report:docs/project_review_2007-1456.md (operator-confirmed) |
| #1495 | Normalise inequality to ≢ in src/+app/ and extend the operator audit to catch ≠ | 2026-08-21T03:53:10Z | [legacy] | — | — | [docs/project_review_2007-1456.md](../project_review_2007-1456.md) | cursor:docs/project_review_boundaries.md; report:docs/project_review_2007-1456.md (operator-confirmed) |
| #1491 | Give master pushes their own CI run, and docs-only pushes a fast path (#1490) | 2026-08-20T23:16:11Z | [legacy] | — | — | [docs/project_review_2007-1456.md](../project_review_2007-1456.md) | cursor:docs/project_review_boundaries.md; report:docs/project_review_2007-1456.md (operator-confirmed) |
| #1489 | Stop asserting the real findings report still has a `[deferred]` heading | 2026-08-20T21:09:42Z | [legacy] | — | — | [docs/project_review_2007-1456.md](../project_review_2007-1456.md) | cursor:docs/project_review_boundaries.md; report:docs/project_review_2007-1456.md (operator-confirmed) |
| #1480 | Replace the 22 local jget copies with probelib.send_json (#1160) | 2026-08-21T01:43:04Z | [legacy] | — | — | [docs/project_review_2007-1456.md](../project_review_2007-1456.md) | cursor:docs/project_review_boundaries.md; report:docs/project_review_2007-1456.md (operator-confirmed) |
| #1478 | Bracket every test Vulkan instance with its destroy (#1401) | 2026-08-21T00:55:42Z | [legacy] | — | — | [docs/project_review_2007-1456.md](../project_review_2007-1456.md) | cursor:docs/project_review_boundaries.md; report:docs/project_review_2007-1456.md (operator-confirmed) |
| #1477 | Refuse a cyclic default_contents graph instead of hanging at item creation | 2026-08-21T00:34:59Z | [legacy] | — | — | [docs/project_review_2007-1456.md](../project_review_2007-1456.md) | cursor:docs/project_review_boundaries.md; report:docs/project_review_2007-1456.md (operator-confirmed) |
| #1473 | Move the zoom cache's output types out of the render tree and state the cache/render boundary | 2026-08-20T21:43:54Z | [legacy] | — | — | [docs/project_review_2007-1456.md](../project_review_2007-1456.md) | cursor:docs/project_review_boundaries.md; report:docs/project_review_2007-1456.md (operator-confirmed) |
| #1472 | Fail the worldgen regression gate when a selected seed has no baseline | 2026-08-20T15:14:53Z | [legacy] | — | — | [docs/project_review_2007-1456.md](../project_review_2007-1456.md) | cursor:docs/project_review_boundaries.md; report:docs/project_review_2007-1456.md (operator-confirmed) |
| #1470 | Move the deprecated GitHub Actions off their Node 20 majors | 2026-08-20T14:48:00Z | [legacy] | — | — | [docs/project_review_2007-1456.md](../project_review_2007-1456.md) | cursor:docs/project_review_boundaries.md; report:docs/project_review_2007-1456.md (operator-confirmed) |
| #1469 | Hold position after a completed player move order (#1216) | 2026-08-20T14:00:12Z | [legacy] | — | — | [docs/project_review_2007-1456.md](../project_review_2007-1456.md) | cursor:docs/project_review_boundaries.md; report:docs/project_review_2007-1456.md (operator-confirmed) |
| #1468 | Make make ci a true mirror of CI's gate set, and gate the two against drift | 2026-08-20T08:14:21Z | [legacy] | — | — | [docs/project_review_2007-1456.md](../project_review_2007-1456.md) | cursor:docs/project_review_boundaries.md; report:docs/project_review_2007-1456.md (operator-confirmed) |
| #1467 | Pin the river-name vector and delete the naming self-comparison | 2026-08-20T07:45:56Z | [legacy] | — | — | [docs/project_review_2007-1456.md](../project_review_2007-1456.md) | cursor:docs/project_review_boundaries.md; report:docs/project_review_2007-1456.md (operator-confirmed) |
| #1466 | Test the world-audit output's canonicalizing sort instead of comparing calls to themselves | 2026-08-20T07:17:08Z | [legacy] | — | — | [docs/project_review_2007-1456.md](../project_review_2007-1456.md) | cursor:docs/project_review_boundaries.md; report:docs/project_review_2007-1456.md (operator-confirmed) |
| #1465 | Decide Vulkan instance extensions in a pure function and pin it without a driver | 2026-08-20T05:23:19Z | [legacy] | — | — | [docs/project_review_2007-1456.md](../project_review_2007-1456.md) | cursor:docs/project_review_boundaries.md; report:docs/project_review_2007-1456.md (operator-confirmed) |
| #1464 | Make the autosave player-intent race test deterministic (#1372) | 2026-08-20T04:55:13Z | [legacy] | — | — | [docs/project_review_2007-1456.md](../project_review_2007-1456.md) | cursor:docs/project_review_boundaries.md; report:docs/project_review_2007-1456.md (operator-confirmed) |
| #1463 | Stop the envelope framing fingerprint reacting to redundant LANGUAGE pragma edits | 2026-08-20T04:33:32Z | [legacy] | — | — | [docs/project_review_2007-1456.md](../project_review_2007-1456.md) | cursor:docs/project_review_boundaries.md; report:docs/project_review_2007-1456.md (operator-confirmed) |
| #1462 | Report NoLand for a landless world even when no definition is placeable | 2026-08-20T04:02:05Z | [legacy] | — | — | [docs/project_review_2007-1456.md](../project_review_2007-1456.md) | cursor:docs/project_review_boundaries.md; report:docs/project_review_2007-1456.md (operator-confirmed) |
| #1461 | Keep ambient wander from routing over damaging drops (#1217) | 2026-08-20T03:39:22Z | [legacy] | — | — | [docs/project_review_2007-1456.md](../project_review_2007-1456.md) | cursor:docs/project_review_boundaries.md; report:docs/project_review_2007-1456.md (operator-confirmed) |
| #1460 | Converge every item-creation path on one materializer | 2026-08-20T03:13:03Z | [legacy] | — | — | [docs/project_review_2007-1456.md](../project_review_2007-1456.md) | cursor:docs/project_review_boundaries.md; report:docs/project_review_2007-1456.md (operator-confirmed) |
| #1459 | Wait for the requested page in the multiworld-save probe | 2026-08-20T02:47:48Z | [legacy] | — | — | [docs/project_review_2007-1456.md](../project_review_2007-1456.md) | cursor:docs/project_review_boundaries.md; report:docs/project_review_2007-1456.md (operator-confirmed) |
| #1458 | Gate the combat-animation probe's death contract on the unit's actual pose | 2026-08-20T02:27:38Z | [legacy] | — | — | [docs/project_review_2007-1456.md](../project_review_2007-1456.md) | cursor:docs/project_review_boundaries.md; report:docs/project_review_2007-1456.md (operator-confirmed) |
| #1457 | Serialize the config probes against every engine-booting probe (#1444) | 2026-08-20T02:07:41Z | [legacy] | — | — | [docs/project_review_2007-1456.md](../project_review_2007-1456.md) | cursor:docs/project_review_boundaries.md; report:docs/project_review_2007-1456.md (operator-confirmed) |
| #1456 | Re-validate the transfer session's source at its reusable creation boundary | 2026-08-20T01:43:02Z | [legacy] | — | — | [docs/project_review_2007-1456.md](../project_review_2007-1456.md) | cursor:docs/project_review_boundaries.md; report:docs/project_review_2007-1456.md (operator-confirmed) |
| #1455 | Compare each worldgen dump against its baseline's recorded content hash | 2026-08-20T01:20:58Z | [legacy] | — | — | [docs/project_review_1455-1424.md](../project_review_1455-1424.md) | report:docs/project_review_1455-1424.md (operator-confirmed) |
| #1454 | Fit the tutorial toggle caption inside its box and assert its rendered bounds | 2026-08-19T23:22:39Z | [legacy] | — | — | [docs/project_review_1455-1424.md](../project_review_1455-1424.md) | report:docs/project_review_1455-1424.md (operator-confirmed) |
| #1453 | Establish combat preconditions in the combat-animation probe before sampling | 2026-08-20T00:43:02Z | [legacy] | — | — | [docs/project_review_1455-1424.md](../project_review_1455-1424.md) | report:docs/project_review_1455-1424.md (operator-confirmed) |
| #1452 | Require regular files for building preview animation frames | 2026-08-19T22:59:45Z | [legacy] | — | — | [docs/project_review_1455-1424.md](../project_review_1455-1424.md) | report:docs/project_review_1455-1424.md (operator-confirmed) |
| #1451 | Let a probe record more than one manual-only reason (#1440) | 2026-08-19T22:34:33Z | [legacy] | — | — | [docs/project_review_1455-1424.md](../project_review_1455-1424.md) | report:docs/project_review_1455-1424.md (operator-confirmed) |
| #1450 | Keep the autosave staging slots out of the player-facing save list | 2026-08-19T22:06:34Z | [legacy] | — | — | [docs/project_review_1455-1424.md](../project_review_1455-1424.md) | report:docs/project_review_1455-1424.md (operator-confirmed) |
| #1449 | Start items at full condition; ground spawn is the salvage exception (#1421) | 2026-08-19T21:41:39Z | [legacy] | — | — | [docs/project_review_1455-1424.md](../project_review_1455-1424.md) | report:docs/project_review_1455-1424.md (operator-confirmed) |
| #1448 | Define a probe result protocol and add the repeat-run flakiness harness | 2026-08-19T21:17:02Z | [legacy] | — | — | [docs/project_review_1455-1424.md](../project_review_1455-1424.md) | report:docs/project_review_1455-1424.md (operator-confirmed) |
| #1446 | Give the per-tile fluid-surface fold one definition in Chunk/Fluid.hs | 2026-08-19T20:15:43Z | [legacy] | — | — | [docs/project_review_1455-1424.md](../project_review_1455-1424.md) | report:docs/project_review_1455-1424.md (operator-confirmed) |
| #1445 | Keep every bleeding-trail probe spawn on loaded arena terrain | 2026-08-19T19:53:43Z | [legacy] | — | — | [docs/project_review_1455-1424.md](../project_review_1455-1424.md) | report:docs/project_review_1455-1424.md (operator-confirmed) |
| #1443 | Stop a meal before opening a mostly-wasted discrete food item (#1219) | 2026-08-19T19:29:46Z | [legacy] | — | — | [docs/project_review_1455-1424.md](../project_review_1455-1424.md) | report:docs/project_review_1455-1424.md (operator-confirmed) |
| #1442 | Give the six worker threads one startup definition in Engine.Core.Thread | 2026-08-19T19:08:33Z | [legacy] | — | — | [docs/project_review_1455-1424.md](../project_review_1455-1424.md) | report:docs/project_review_1455-1424.md (operator-confirmed) |
| #1424 | Write an engine's ledger line before the handshake that releases its probe | 2026-08-19T18:31:28Z | [legacy] | — | — | [docs/project_review_1455-1424.md](../project_review_1455-1424.md) | report:docs/project_review_1455-1424.md (operator-confirmed) |
| #1423 | Reject world-audit categories that were never classified or given a threshold | 2026-08-19T17:39:39Z | [legacy] | — | — | [docs/project_review_1423-1297.md](../project_review_1423-1297.md) | cursor:docs/project_review_boundaries.md; report:docs/project_review_1423-1297.md (operator-confirmed) |
| #1422 | Serialize probes that mutate the same repository resource (#1322) | 2026-08-19T17:14:35Z | [legacy] | — | — | [docs/project_review_1423-1297.md](../project_review_1423-1297.md) | report:docs/project_review_1423-1297.md (operator-confirmed) |
| #1411 | Repair the action-outcome probe's portal and chop fixtures | 2026-08-19T18:04:12Z | [legacy] | — | — | [docs/project_review_1423-1297.md](../project_review_1423-1297.md) | cursor:docs/project_review_boundaries.md; report:docs/project_review_1423-1297.md (operator-confirmed) |
| #1409 | Pin the Hackage index-state so a dependency plan needs a commit to move | 2026-08-19T16:49:15Z | [legacy] | — | — | [docs/project_review_1423-1297.md](../project_review_1423-1297.md) | report:docs/project_review_1423-1297.md (operator-confirmed) |
| #1407 | Narrow the remaining over-wide src/Engine/ export lists and remove the superseded Lua log registrar | 2026-08-19T15:46:20Z | [legacy] | — | — | [docs/project_review_1423-1297.md](../project_review_1423-1297.md) | report:docs/project_review_1423-1297.md (operator-confirmed) |
| #1406 | Load item definitions from logical subdirectories | 2026-08-19T15:21:04Z | [legacy] | — | — | [docs/project_review_1423-1297.md](../project_review_1423-1297.md) | report:docs/project_review_1423-1297.md (operator-confirmed) |
| #1405 | Resolve the boot mode and debug port once in Main, and fix two app/ nits | 2026-08-19T14:05:46Z | [legacy] | — | — | [docs/project_review_1423-1297.md](../project_review_1423-1297.md) | report:docs/project_review_1423-1297.md (operator-confirmed) |
| #1404 | Place the remote-settlement modal's title and message inside the panel's content area | 2026-08-19T13:38:07Z | [legacy] | — | — | [docs/project_review_1423-1297.md](../project_review_1423-1297.md) | report:docs/project_review_1423-1297.md (operator-confirmed) |
| #1403 | Gate the unified transfer system end to end | 2026-08-19T12:52:53Z | [legacy] | — | — | [docs/project_review_1423-1297.md](../project_review_1423-1297.md) | report:docs/project_review_1423-1297.md (operator-confirmed) |
| #1393 | Give the dump's generation parameters and chunk region named types | 2026-08-19T03:29:31Z | [legacy] | — | — | [docs/project_review_1423-1297.md](../project_review_1423-1297.md) | report:docs/project_review_1423-1297.md (operator-confirmed) |
| #1392 | Drop the headless harness's redundant post-shutdown sleep | 2026-08-19T00:53:38Z | [legacy] | — | — | [docs/project_review_1423-1297.md](../project_review_1423-1297.md) | report:docs/project_review_1423-1297.md (operator-confirmed) |
| #1391 | Stop HUD layout fixtures from emitting 948 missing-world warnings | 2026-08-18T23:34:04Z | [legacy] | — | — | [docs/project_review_1423-1297.md](../project_review_1423-1297.md) | report:docs/project_review_1423-1297.md (operator-confirmed) |
| #1390 | Fail the headless example when its world worker has already died | 2026-08-18T23:05:26Z | [legacy] | — | — | [docs/project_review_1423-1297.md](../project_review_1423-1297.md) | report:docs/project_review_1423-1297.md (operator-confirmed) |
| #1389 | Run the etymology page-scope gate without a world thread | 2026-08-18T19:51:11Z | [legacy] | — | — | [docs/project_review_1423-1297.md](../project_review_1423-1297.md) | report:docs/project_review_1423-1297.md (operator-confirmed) |
| #1387 | Handle Mode A session failures (#1254, UIT-5B) | 2026-08-18T18:46:02Z | [legacy] | — | — | [docs/project_review_1423-1297.md](../project_review_1423-1297.md) | report:docs/project_review_1423-1297.md (operator-confirmed) |
| #1373 | Isolate headless config writes from the developer's checkout (#1357) | 2026-08-18T18:23:19Z | [legacy] | — | — | [docs/project_review_1423-1297.md](../project_review_1423-1297.md) | report:docs/project_review_1423-1297.md (operator-confirmed) |
| #1371 | Reject unknown probe keys in exact selection instead of silently dropping them | 2026-08-18T17:08:32Z | [legacy] | — | — | [docs/project_review_1423-1297.md](../project_review_1423-1297.md) | report:docs/project_review_1423-1297.md (operator-confirmed) |
| #1353 | Extend escort transfers to unit-to-unit two-sided holds | 2026-08-18T15:50:32Z | [legacy] | — | — | [docs/project_review_1423-1297.md](../project_review_1423-1297.md) | report:docs/project_review_1423-1297.md (operator-confirmed) |
| #1352 | Terminate a probe's engine descendants on ordinary failure, not only on timeout | 2026-08-18T15:26:57Z | [legacy] | — | — | [docs/project_review_1423-1297.md](../project_review_1423-1297.md) | report:docs/project_review_1423-1297.md (operator-confirmed) |
| #1351 | Fail the Lua duplicate-function audit when its scan scope or module grammar goes unmatched | 2026-08-18T14:50:39Z | [legacy] | — | — | [docs/project_review_1423-1297.md](../project_review_1423-1297.md) | report:docs/project_review_1423-1297.md (operator-confirmed) |
| #1350 | Add the escort transfer session (Mode A) | 2026-08-17T03:49:50Z | [legacy] | — | — | [docs/project_review_1423-1297.md](../project_review_1423-1297.md) | report:docs/project_review_1423-1297.md (operator-confirmed) |
| #1349 | Handle Mode B transfer-order failures (#1253) | 2026-08-17T02:05:52Z | [legacy] | — | — | [docs/project_review_1423-1297.md](../project_review_1423-1297.md) | report:docs/project_review_1423-1297.md (operator-confirmed) |
| #1348 | Report simultaneously severed subparts in encounter order (#1331) | 2026-08-17T01:44:47Z | [legacy] | — | — | [docs/project_review_1423-1297.md](../project_review_1423-1297.md) | report:docs/project_review_1423-1297.md (operator-confirmed) |
| #1347 | Reject craft and construction jobs whose input load cannot fit before claiming them | 2026-08-16T22:25:56Z | [legacy] | — | — | [docs/project_review_1423-1297.md](../project_review_1423-1297.md) | report:docs/project_review_1423-1297.md (operator-confirmed) |
| #1346 | Clear the unit-AI claim and repair-priority tables when a save load replaces the session | 2026-08-16T19:51:36Z | [legacy] | — | — | [docs/project_review_1423-1297.md](../project_review_1423-1297.md) | report:docs/project_review_1423-1297.md (operator-confirmed) |
| #1345 | Give scripts/shell.lua one module identity so settings rescale reaches the live console | 2026-08-16T19:26:24Z | [legacy] | — | — | [docs/project_review_1423-1297.md](../project_review_1423-1297.md) | report:docs/project_review_1423-1297.md (operator-confirmed) |
| #1344 | Restore transfer_order_probe.py's item fixture against the current item schema | 2026-08-16T16:21:54Z | [legacy] | — | — | [docs/project_review_1423-1297.md](../project_review_1423-1297.md) | report:docs/project_review_1423-1297.md (operator-confirmed) |
| #1343 | Fail a probe at setup when its inline fixture registers nothing | 2026-08-16T15:58:28Z | [legacy] | — | — | [docs/project_review_1423-1297.md](../project_review_1423-1297.md) | report:docs/project_review_1423-1297.md (operator-confirmed) |
| #1340 | Promote "Store in cargo" to a queued order-at-a-distance | 2026-08-16T14:54:32Z | [legacy] | — | — | [docs/project_review_1423-1297.md](../project_review_1423-1297.md) | report:docs/project_review_1423-1297.md (operator-confirmed) |
| #1339 | Close the unit-atlas pipeline with documentation and focused regression gates (#1262) | 2026-08-16T14:28:15Z | [legacy] | — | — | [docs/project_review_1423-1297.md](../project_review_1423-1297.md) | report:docs/project_review_1423-1297.md (operator-confirmed) |
| #1338 | Add the nested container-window stack | 2026-08-16T13:28:11Z | [legacy] | — | — | [docs/project_review_1423-1297.md](../project_review_1423-1297.md) | report:docs/project_review_1423-1297.md (operator-confirmed) |
| #1336 | Compile the six remaining unit trees and retire per-frame unit-animation loading (#1261) | 2026-08-16T05:42:10Z | [legacy] | — | — | [docs/project_review_1423-1297.md](../project_review_1423-1297.md) | report:docs/project_review_1423-1297.md (operator-confirmed) |
| #1335 | Show tracked temperature summaries in unit and cargo item-list rows | 2026-08-16T05:20:30Z | [legacy] | — | — | [docs/project_review_1423-1297.md](../project_review_1423-1297.md) | report:docs/project_review_1423-1297.md (operator-confirmed) |
| #1334 | Take gameplay's random stream back from a UI widget (#1330) | 2026-08-16T04:52:43Z | [legacy] | — | — | [docs/project_review_1423-1297.md](../project_review_1423-1297.md) | report:docs/project_review_1423-1297.md (operator-confirmed) |
| #1333 | Migrate acolyte as the end-to-end atlas pilot | 2026-08-16T02:36:16Z | [legacy] | — | — | [docs/project_review_1423-1297.md](../project_review_1423-1297.md) | report:docs/project_review_1423-1297.md (operator-confirmed) |
| #1332 | Sign every supplied row field the item list can display | 2026-08-16T02:12:32Z | [legacy] | — | — | [docs/project_review_1423-1297.md](../project_review_1423-1297.md) | report:docs/project_review_1423-1297.md (operator-confirmed) |
| #1328 | Mark every location unknown on the zoom map and reveal its type by unit sight | 2026-08-16T00:24:48Z | [legacy] | — | — | [docs/project_review_1423-1297.md](../project_review_1423-1297.md) | report:docs/project_review_1423-1297.md (operator-confirmed) |
| #1327 | Add physical bulk and portable-storage capacity data | 2026-08-15T21:06:50Z | [legacy] | — | — | [docs/project_review_1423-1297.md](../project_review_1423-1297.md) | report:docs/project_review_1423-1297.md (operator-confirmed) |
| #1317 | Add atlas-frame storage and sampling to the unit runtime | 2026-08-15T15:57:15Z | [legacy] | — | — | [docs/project_review_1423-1297.md](../project_review_1423-1297.md) | report:docs/project_review_1423-1297.md (operator-confirmed) |
| #1316 | Give the five divergent truncateToWidth copies one shared implementation | 2026-08-15T15:18:40Z | [legacy] | — | — | [docs/project_review_1423-1297.md](../project_review_1423-1297.md) | report:docs/project_review_1423-1297.md (operator-confirmed) |
| #1315 | Validate unit-animation frame contents in the asset inventory gate | 2026-08-15T14:54:49Z | [legacy] | — | — | [docs/project_review_1423-1297.md](../project_review_1423-1297.md) | report:docs/project_review_1423-1297.md (operator-confirmed) |
| #1314 | Execute a transfer order as a unit job that commits on arrival | 2026-08-15T11:42:53Z | [legacy] | — | — | [docs/project_review_1423-1297.md](../project_review_1423-1297.md) | report:docs/project_review_1423-1297.md (operator-confirmed) |
| #1313 | Build the deterministic per-animation atlas compiler and index | 2026-08-14T23:13:03Z | [legacy] | — | — | [docs/project_review_1423-1297.md](../project_review_1423-1297.md) | report:docs/project_review_1423-1297.md (operator-confirmed) |
| #1312 | Render last-known container contents with an age indicator (#1237) | 2026-08-14T22:49:02Z | [legacy] | — | — | [docs/project_review_1423-1297.md](../project_review_1423-1297.md) | report:docs/project_review_1423-1297.md (operator-confirmed) |
| #1310 | Give transfer orders durable state and persistence (#1246) | 2026-08-14T20:54:46Z | [legacy] | — | — | [docs/project_review_1423-1297.md](../project_review_1423-1297.md) | report:docs/project_review_1423-1297.md (operator-confirmed) |
| #1309 | Make the unit asset inventory authoritative and enforceable | 2026-08-14T15:57:15Z | [legacy] | — | — | [docs/project_review_1423-1297.md](../project_review_1423-1297.md) | report:docs/project_review_1423-1297.md (operator-confirmed) |
| #1308 | Fail the save-wire gates when a guarded constructor's same-arity payload shape changes | 2026-08-14T15:35:59Z | [legacy] | — | — | [docs/project_review_1423-1297.md](../project_review_1423-1297.md) | report:docs/project_review_1423-1297.md (operator-confirmed) |
| #1307 | Reject duplicate, current, and future entries in a component's csOlderVersions table | 2026-08-14T04:10:15Z | [legacy] | — | — | [docs/project_review_1423-1297.md](../project_review_1423-1297.md) | report:docs/project_review_1423-1297.md (operator-confirmed) |
| #1306 | Add container-knowledge to Lua's Haskell-component mirror and make the mirror drift-proof | 2026-08-14T03:47:28Z | [legacy] | — | — | [docs/project_review_1423-1297.md](../project_review_1423-1297.md) | report:docs/project_review_1423-1297.md (operator-confirmed) |
| #1305 | Fail the cabal inventory audit on duplicate or path-contradicting modules | 2026-08-14T03:25:40Z | [legacy] | — | — | [docs/project_review_1423-1297.md](../project_review_1423-1297.md) | report:docs/project_review_1423-1297.md (operator-confirmed) |
| #1304 | Make the restored-entity apply context immutable across Lua save components | 2026-08-14T00:55:22Z | [legacy] | — | — | [docs/project_review_1423-1297.md](../project_review_1423-1297.md) | report:docs/project_review_1423-1297.md (operator-confirmed) |
| #1303 | Validate defName before classifying a held instance as non-transferable | 2026-08-14T00:33:48Z | [legacy] | — | — | [docs/project_review_1423-1297.md](../project_review_1423-1297.md) | report:docs/project_review_1423-1297.md (operator-confirmed) |
| #1302 | Report source_missing when a cross-manager rollback cannot restore | 2026-08-14T00:15:10Z | [legacy] | — | — | [docs/project_review_1423-1297.md](../project_review_1423-1297.md) | report:docs/project_review_1423-1297.md (operator-confirmed) |
| #1301 | Keep etymology recurrence anchored to the active page under an explicit pageId | 2026-08-13T21:47:49Z | [legacy] | — | — | [docs/project_review_1423-1297.md](../project_review_1423-1297.md) | report:docs/project_review_1423-1297.md (operator-confirmed) |
| #1300 | Keep the calibration runner's provisioning transfers within carrying capacity | 2026-08-13T21:26:20Z | [legacy] | — | — | [docs/project_review_1423-1297.md](../project_review_1423-1297.md) | report:docs/project_review_1423-1297.md (operator-confirmed) |
| #1299 | Bound world.suggestName's reroll ordinal instead of accepting caller-sized synchronous work | 2026-08-13T21:04:26Z | [legacy] | — | — | [docs/project_review_1423-1297.md](../project_review_1423-1297.md) | report:docs/project_review_1423-1297.md (operator-confirmed) |
| #1298 | Invalidate every alias handle sharing an atlas before its bindless slot is freed | 2026-08-13T18:04:00Z | [legacy] | — | — | [docs/project_review_1423-1297.md](../project_review_1423-1297.md) | report:docs/project_review_1423-1297.md (operator-confirmed) |
| #1297 | Make the name plate's location containment seam-aware | 2026-08-13T17:42:28Z | [legacy] | — | — | [docs/project_review_1423-1297.md](../project_review_1423-1297.md) | report:docs/project_review_1423-1297.md (operator-confirmed) |
| #1296 | Make the headless UI specs independent of the developer's saved UI scale | 2026-08-13T17:20:29Z | [legacy] | — | — | [docs/project_review_1296.md](../project_review_1296.md) | report:docs/project_review_1296.md (operator-confirmed) |
| #1295 | Charge a commanded order's stall budget in eligible time only | 2026-08-13T16:23:36Z | [legacy] | — | — | [docs/project_review_1296.md](../project_review_1296.md) | report:docs/project_review_1296.md (operator-confirmed) |
| #1294 | Capture the first-aid scenario's pre-fall baseline under a stopped simulation | 2026-08-13T13:45:34Z | [legacy] | — | — | [docs/project_review_1296.md](../project_review_1296.md) | report:docs/project_review_1296.md (operator-confirmed) |
| #1293 | Select the nearest of several units for Transfer instead of requiring exactly one | 2026-08-13T13:26:55Z | [legacy] | — | — | [docs/project_review_1296.md](../project_review_1296.md) | report:docs/project_review_1296.md (operator-confirmed) |
| #1292 | Use Codex Luna for naive playtests | 2026-08-13T12:45:24Z | [legacy] | — | — | [docs/project_review_1296.md](../project_review_1296.md) | report:docs/project_review_1296.md (operator-confirmed) |
| #1290 | Reject non-finite remembered weights and reveal times in container-knowledge validation | 2026-08-13T01:45:00Z | [legacy] | — | — | [docs/project_review_1296.md](../project_review_1296.md) | report:docs/project_review_1296.md (operator-confirmed) |
| #1289 | Query and enable the exact descriptor-indexing features the bindless layout uses | 2026-08-13T00:53:48Z | [legacy] | — | — | [docs/project_review_1296.md](../project_review_1296.md) | report:docs/project_review_1296.md (operator-confirmed) |
| #1288 | Announce "Engine running" only when the startup promotion commits | 2026-08-12T23:43:58Z | [legacy] | — | — | [docs/project_review_1296.md](../project_review_1296.md) | report:docs/project_review_1296.md (operator-confirmed) |
| #1287 | Restore deterministic offscreen building targeting in interaction probes | 2026-08-12T20:39:19Z | [legacy] | — | — | [docs/project_review_1296.md](../project_review_1296.md) | report:docs/project_review_1296.md (operator-confirmed) |
| #1285 | Prevent docs_land.sh from committing unrelated pre-staged files | 2026-08-12T18:35:22Z | [legacy] | — | — | [docs/project_review_1296.md](../project_review_1296.md) | report:docs/project_review_1296.md (operator-confirmed) |
| #1284 | Keep a shutdown requested during engine startup | 2026-08-12T15:12:07Z | [legacy] | — | — | [docs/project_review_1296.md](../project_review_1296.md) | report:docs/project_review_1296.md (operator-confirmed) |
| #1271 | Make the debug-console boot probe's successful-bind check load-tolerant | 2026-08-12T14:21:37Z | [legacy] | — | — | [docs/project_review_1296.md](../project_review_1296.md) | report:docs/project_review_1296.md (operator-confirmed) |
| #1248 | Generalize the container window to any endpoint kind | 2026-08-12T13:25:43Z | never reviewed | — | — | — | — |
| #1245 | Make the transactional-load probe deterministically test mutual exclusion | 2026-08-12T05:09:44Z | never reviewed | — | — | — | — |
| #1244 | Cut the two live UI truncators at code-point boundaries | 2026-08-12T04:27:59Z | never reviewed | — | — | — | — |
| #1243 | Repair the Lua save API smoke test against the current asynchronous contracts | 2026-08-12T02:59:49Z | never reviewed | — | — | — | — |
| #1242 | Retire a power building's node when the building is destroyed | 2026-08-12T01:57:20Z | never reviewed | — | — | — | — |
| #1241 | Re-derive worn-accessory buffs when an accessory is unequipped | 2026-08-12T02:37:22Z | never reviewed | — | — | — | — |
| #1240 | Judge spawn-time loadout shedding against effective carrying capacity | 2026-08-12T01:35:21Z | never reviewed | — | — | — | — |
| #1236 | Resolve ground pickup and unit drops on the unit's owning page | 2026-08-11T22:03:00Z | never reviewed | — | — | — | — |
| #1235 | Require power.placeNode's supplying unit to belong to the destination page | 2026-08-11T21:31:36Z | never reviewed | — | — | — | — |
| #1228 | Replace the transposable positional runs in the two 14-parameter quad producers | 2026-08-11T17:26:58Z | never reviewed | — | — | — | — |
| #1227 | Keep persistent wire networks electrically connected across chunk eviction | 2026-08-11T16:31:50Z | never reviewed | — | — | — | — |
| #1226 | Report a distinct terminal disposition when post-load reconciliation callbacks fail | 2026-08-11T14:45:23Z | never reviewed | — | — | — | — |
| #1225 | State who owns the findings report's status fields, and audit that they agree | 2026-08-11T03:06:54Z | never reviewed | — | — | — | — |
| #1224 | Narrow the src/UI export lists, delete the dead submitBuffer, and drop the UI.Focus TextBuffer re-export | 2026-08-11T02:45:22Z | never reviewed | — | — | — | — |
| #1223 | Drain and refill the exact canteen instance the AI selected | 2026-08-11T02:24:37Z | never reviewed | — | — | — | — |
| #1215 | Surface Lua load rollback failures instead of reporting a cleanly aborted load | 2026-08-10T20:35:20Z | never reviewed | — | — | — | — |
| #1214 | Fix the invalid regex escape sequence in action_outcome_coverage.py's docstring | 2026-08-10T19:56:59Z | never reviewed | — | — | — | — |
| #1211 | Keep a selectable generation on disk when saving over a corrupt-authoritative recovery slot | 2026-08-10T18:42:37Z | never reviewed | — | — | — | — |
| #1210 | Reconcile the code-health report against the stranded autostash state | 2026-08-10T01:00:03Z | [legacy] | — | — | [docs/project_review_1210-1183.md](../project_review_1210-1183.md) | report:docs/project_review_1210-1183.md (operator-confirmed) |
| #1202 | Make the debug console's input editing UTF-8 code-point safe (#1187) | 2026-08-09T23:08:40Z | [legacy] | — | — | [docs/project_review_1210-1183.md](../project_review_1210-1183.md) | report:docs/project_review_1210-1183.md (operator-confirmed) |
| #1201 | State the hydrology pipeline's stage boundaries in a tracked document (#1109) | 2026-08-09T22:45:03Z | [legacy] | — | — | [docs/project_review_1210-1183.md](../project_review_1210-1183.md) | report:docs/project_review_1210-1183.md (operator-confirmed) |
| #1199 | Reject present-but-malformed CLI values instead of silently defaulting (#1191) | 2026-08-09T21:57:18Z | [legacy] | — | — | [docs/project_review_1210-1183.md](../project_review_1210-1183.md) | report:docs/project_review_1210-1183.md (operator-confirmed) |
| #1198 | Fail --headless/--offscreen boot when the debug listener can't start (#1190) | 2026-08-09T18:23:20Z | [legacy] | — | — | [docs/project_review_1210-1183.md](../project_review_1210-1183.md) | report:docs/project_review_1210-1183.md (operator-confirmed) |
| #1194 | L5: Show a generated name's etymology—decompose it into roots and meanings (#1104) | 2026-08-08T22:58:12Z | [legacy] | — | — | [docs/project_review_1210-1183.md](../project_review_1210-1183.md) | report:docs/project_review_1210-1183.md (operator-confirmed) |
| #1193 | Normalise the pick and designation coordinate frame at the U seam (#1175) | 2026-08-08T20:57:09Z | [legacy] | — | — | [docs/project_review_1210-1183.md](../project_review_1210-1183.md) | report:docs/project_review_1210-1183.md (operator-confirmed) |
| #1188 | Show world identity in the save browser and the loading flow (#1107) | 2026-08-08T07:45:40Z | [legacy] | — | — | [docs/project_review_1210-1183.md](../project_review_1210-1183.md) | report:docs/project_review_1210-1183.md (operator-confirmed) |
| #1186 | Correct Unit.Pathing.Cost's module comment about future modifiers | 2026-08-08T07:21:22Z | [legacy] | — | — | [docs/project_review_1210-1183.md](../project_review_1210-1183.md) | report:docs/project_review_1210-1183.md (operator-confirmed) |
| #1185 | Replace stale boot-mode enumerations in three module comments | 2026-08-08T06:56:07Z | [legacy] | — | — | [docs/project_review_1210-1183.md](../project_review_1210-1183.md) | report:docs/project_review_1210-1183.md (operator-confirmed) |
| #1184 | Make the u-wrap render offset facing-aware (#1176) | 2026-08-08T06:25:51Z | [legacy] | — | — | [docs/project_review_1210-1183.md](../project_review_1210-1183.md) | report:docs/project_review_1210-1183.md (operator-confirmed) |
| #1183 | Remove dead underscore-prefixed bindings | 2026-08-08T06:02:09Z | [legacy] | — | — | [docs/project_review_1210-1183.md](../project_review_1210-1183.md) | report:docs/project_review_1210-1183.md (operator-confirmed) |
| #1182 | Fix fluid-audit archive note's false waterSideFaceQuads claim | 2026-08-08T05:39:51Z | [legacy] | — | — | [docs/project_review_1182-1167.md](../project_review_1182-1167.md) | report:docs/project_review_1182-1167.md (operator-confirmed) |
| #1180 | Dedup applyFacingF: import from World.Grid instead of redefining | 2026-08-08T05:23:45Z | [legacy] | — | — | [docs/project_review_1182-1167.md](../project_review_1182-1167.md) | report:docs/project_review_1182-1167.md (operator-confirmed) |
| #1179 | Delete the World.Fluids facade and import World.Fluid.Ocean directly | 2026-08-08T05:02:07Z | [legacy] | — | — | [docs/project_review_1182-1167.md](../project_review_1182-1167.md) | report:docs/project_review_1182-1167.md (operator-confirmed) |
| #1178 | Move composeFluidMap's stale water-table haddock to lcWaterTableMap | 2026-08-08T04:38:32Z | [legacy] | — | — | [docs/project_review_1182-1167.md](../project_review_1182-1167.md) | report:docs/project_review_1182-1167.md (operator-confirmed) |
| #1174 | Name rivers in their world's language and expose river identity to Lua (#1102) | 2026-08-08T02:24:28Z | [legacy] | — | — | [docs/project_review_1182-1167.md](../project_review_1182-1167.md) | report:docs/project_review_1182-1167.md (operator-confirmed) |
| #1173 | Suggest world names from the generated-language system (#1106) | 2026-08-08T00:36:59Z | [legacy] | — | — | [docs/project_review_1182-1167.md](../project_review_1182-1167.md) | report:docs/project_review_1182-1167.md (operator-confirmed) |
| #1172 | Add an append-only audit for the positionally-serialized enums | 2026-08-08T00:12:19Z | [legacy] | — | — | [docs/project_review_1182-1167.md](../project_review_1182-1167.md) | report:docs/project_review_1182-1167.md (operator-confirmed) |
| #1171 | Canonicalise the render-path chunk lookups so water side faces and slopes draw at the U seam | 2026-08-07T23:50:25Z | [legacy] | — | — | [docs/project_review_1182-1167.md](../project_review_1182-1167.md) | report:docs/project_review_1182-1167.md (operator-confirmed) |
| #1170 | Wrap display text by code point, not by byte (#1159) | 2026-08-07T23:28:11Z | [legacy] | — | — | [docs/project_review_1182-1167.md](../project_review_1182-1167.md) | report:docs/project_review_1182-1167.md (operator-confirmed) |
| #1169 | C0: Extract one shared item-list widget from the three duplicated inventory panels | 2026-08-07T17:47:15Z | [legacy] | — | — | [docs/project_review_1182-1167.md](../project_review_1182-1167.md) | report:docs/project_review_1182-1167.md (operator-confirmed) |
| #1168 | Name placed locations in their world's own language (#1101) | 2026-08-07T17:26:35Z | [legacy] | — | — | [docs/project_review_1182-1167.md](../project_review_1182-1167.md) | report:docs/project_review_1182-1167.md (operator-confirmed) |
| #1167 | L3: Give generated languages per-language orthographic conventions beyond ASCII | 2026-08-07T17:03:32Z | [legacy] | — | — | [docs/project_review_1182-1167.md](../project_review_1182-1167.md) | report:docs/project_review_1182-1167.md (operator-confirmed) |
| #1166 | Give the save command's reference-edge and Lua-component payloads named records | 2026-08-07T16:19:26Z | [legacy] | — | — | [docs/project_review_1165-1128.md](../project_review_1165-1128.md) | report:docs/project_review_1165-1128.md (operator-confirmed) |
| #1165 | Remove stray -fprof-auto pragmas from 43 worldgen modules | 2026-08-07T16:42:07Z | [legacy] | — | — | [docs/project_review_1165-1128.md](../project_review_1165-1128.md) | report:docs/project_review_1165-1128.md (operator-confirmed) |
| #1164 | Remove the World.ZoomMap facade and the dead background renderer | 2026-08-07T15:55:41Z | [legacy] | — | — | [docs/project_review_1165-1128.md](../project_review_1165-1128.md) | report:docs/project_review_1165-1128.md (operator-confirmed) |
| #1163 | Remove five underscore-silenced dead bindings in BuildPixels | 2026-08-07T15:34:39Z | [legacy] | — | — | [docs/project_review_1165-1128.md](../project_review_1165-1128.md) | report:docs/project_review_1165-1128.md (operator-confirmed) |
| #1143 | Give the component codec helper real multi-version decode and named arguments | 2026-08-06T23:59:26Z | [legacy] | — | — | [docs/project_review_1165-1128.md](../project_review_1165-1128.md) | report:docs/project_review_1165-1128.md (operator-confirmed) |
| #1142 | L1d: give a small deterministic set of roots bound forms for dependent compound slots | 2026-08-07T00:40:01Z | [legacy] | — | — | [docs/project_review_1165-1128.md](../project_review_1165-1128.md) | report:docs/project_review_1165-1128.md (operator-confirmed) |
| #1141 | Give the save system's item enumeration one implementation | 2026-08-07T00:20:27Z | [legacy] | — | — | [docs/project_review_1165-1128.md](../project_review_1165-1128.md) | report:docs/project_review_1165-1128.md (operator-confirmed) |
| #1140 | Fix three minor worldgen defects in the fluid identify and chunk-smoothing paths | 2026-08-06T16:55:08Z | [legacy] | — | — | [docs/project_review_1165-1128.md](../project_review_1165-1128.md) | report:docs/project_review_1165-1128.md (operator-confirmed) |
| #1134 | L1c: boundary phonology at every morpheme join, no triple-letter runs (generator version 3) | 2026-08-06T14:14:50Z | [legacy] | — | — | [docs/project_review_1165-1128.md](../project_review_1165-1128.md) | report:docs/project_review_1165-1128.md (operator-confirmed) |
| #1130 | Archive the abandoned river redesign and delete its unused module | 2026-08-05T16:20:06Z | [legacy] | — | — | [docs/project_review_1165-1128.md](../project_review_1165-1128.md) | report:docs/project_review_1165-1128.md (operator-confirmed) |
| #1129 | A2: generalize the transfer contract to bidirectional endpoints, instance sets, and partial batches | 2026-08-05T16:00:28Z | [legacy] | — | — | [docs/project_review_1165-1128.md](../project_review_1165-1128.md) | report:docs/project_review_1165-1128.md (operator-confirmed) |
| #1128 | Add a CI audit that the Haskell material constants match data/materials YAML | 2026-08-05T15:37:25Z | [legacy] | — | — | [docs/project_review_1165-1128.md](../project_review_1165-1128.md) | report:docs/project_review_1165-1128.md (operator-confirmed) |
| #1127 | Delete the always-empty moSurface overlay field | 2026-08-05T15:17:10Z | [legacy] | — | — | [docs/project_review_1127-1079.md](../project_review_1127-1079.md) | report:docs/project_review_1127-1079.md (operator-confirmed) |
| #1126 | A3: give containers a persisted player-knowledge layer with stale contents | 2026-08-05T14:51:03Z | [legacy] | — | — | [docs/project_review_1127-1079.md](../project_review_1127-1079.md) | report:docs/project_review_1127-1079.md (operator-confirmed) |
| #1125 | Widen the shipped SDF font atlases past printable ASCII (#1098) | 2026-08-05T05:55:19Z | [legacy] | — | — | [docs/project_review_1127-1079.md](../project_review_1127-1079.md) | report:docs/project_review_1127-1079.md (operator-confirmed) |
| #1124 | L1b: Constrain CCV onsets and let 'y' serve as a vowel (generator version 2) | 2026-08-05T05:33:19Z | [legacy] | — | — | [docs/project_review_1127-1079.md](../project_review_1127-1079.md) | report:docs/project_review_1127-1079.md (operator-confirmed) |
| #1123 | Persist a generated world's language provenance (#1092) | 2026-08-05T01:58:43Z | [legacy] | — | — | [docs/project_review_1127-1079.md](../project_review_1127-1079.md) | report:docs/project_review_1127-1079.md (operator-confirmed) |
| #1122 | L2a: Render a visible fallback for missing glyphs instead of silently dropping them | 2026-08-05T01:37:29Z | [legacy] | — | — | [docs/project_review_1127-1079.md](../project_review_1127-1079.md) | report:docs/project_review_1127-1079.md (operator-confirmed) |
| #1121 | Give the river-flat surface rule one definition and apply it on the dig path | 2026-08-05T02:22:29Z | [legacy] | — | — | [docs/project_review_1127-1079.md](../project_review_1127-1079.md) | report:docs/project_review_1127-1079.md (operator-confirmed) |
| #1120 | P2a: Stop discarding the Create World world name before it reaches world.init | 2026-08-05T01:14:25Z | [legacy] | — | — | [docs/project_review_1127-1079.md](../project_review_1127-1079.md) | report:docs/project_review_1127-1079.md (operator-confirmed) |
| #1089 | Apply Lua save components per entity instead of clobbering singletons | 2026-08-03T22:18:26Z | [legacy] | — | — | [docs/project_review_1127-1079.md](../project_review_1127-1079.md) | report:docs/project_review_1127-1079.md (operator-confirmed) |
| #1082 | Restate the capture-lock gate's rationale as a present-tense invariant | 2026-08-03T20:37:58Z | [legacy] | — | — | [docs/project_review_1127-1079.md](../project_review_1127-1079.md) | report:docs/project_review_1127-1079.md (operator-confirmed) |
| #1080 | Give the uniform buffer object one layout definition | 2026-08-03T17:43:07Z | [legacy] | — | — | [docs/project_review_1127-1079.md](../project_review_1127-1079.md) | report:docs/project_review_1127-1079.md (operator-confirmed) |
| #1079 | Seal ErrorContext and drop the throwEngineException/catchEngine aliases | 2026-08-03T17:21:20Z | [legacy] | — | — | [docs/project_review_1127-1079.md](../project_review_1127-1079.md) | report:docs/project_review_1127-1079.md (operator-confirmed) |
| #1076 | Remove the verified dead logging helpers and wrapper chains | 2026-08-03T15:49:57Z | [legacy] | — | — | [docs/project_review_1076-1064.md](../project_review_1076-1064.md) | report:docs/project_review_1076-1064.md (operator-confirmed) |
| #1075 | Remove review-round provenance from production Haskell comments | 2026-08-03T15:25:11Z | [legacy] | — | — | [docs/project_review_1076-1064.md](../project_review_1076-1064.md) | report:docs/project_review_1076-1064.md (operator-confirmed) |
| #1074 | Remove the test-only Engine.Core.Var wrapper | 2026-08-03T15:00:27Z | [legacy] | — | — | [docs/project_review_1076-1064.md](../project_review_1076-1064.md) | report:docs/project_review_1076-1064.md (operator-confirmed) |
| #1073 | Remove the unused LogToFile and LogMulti logging backends | 2026-08-03T14:35:36Z | [legacy] | — | — | [docs/project_review_1076-1064.md](../project_review_1076-1064.md) | report:docs/project_review_1076-1064.md (operator-confirmed) |
| #1071 | Remove dead TimingState fields and clarify its FPS accumulators | 2026-08-03T04:23:48Z | [legacy] | — | — | [docs/project_review_1076-1064.md](../project_review_1076-1064.md) | report:docs/project_review_1076-1064.md (operator-confirmed) |
| #1070 | Remove the engine's unconstructed exception domains | 2026-08-03T04:01:13Z | [legacy] | — | — | [docs/project_review_1076-1064.md](../project_review_1076-1064.md) | report:docs/project_review_1076-1064.md (operator-confirmed) |
| #1069 | Remove the unused AssetConfig from EngineState | 2026-08-03T03:42:04Z | [legacy] | — | — | [docs/project_review_1076-1064.md](../project_review_1076-1064.md) | report:docs/project_review_1076-1064.md (operator-confirmed) |
| #1068 | Correct Engine.Input.Thread's facade Haddock | 2026-08-03T03:20:24Z | [legacy] | — | — | [docs/project_review_1076-1064.md](../project_review_1076-1064.md) | report:docs/project_review_1076-1064.md (operator-confirmed) |
| #1067 | Fix three minor defects in the Lua scripting tree | 2026-08-02T23:54:54Z | [legacy] | — | — | [docs/project_review_1076-1064.md](../project_review_1076-1064.md) | report:docs/project_review_1076-1064.md (operator-confirmed) |
| #1066 | Remove unlisted dead source modules and audit the Cabal library inventory | 2026-08-02T23:32:19Z | [legacy] | — | — | [docs/project_review_1076-1064.md](../project_review_1076-1064.md) | report:docs/project_review_1076-1064.md (operator-confirmed) |
| #1065 | Remove the dead legacy Vulkan state types | 2026-08-02T23:09:04Z | [legacy] | — | — | [docs/project_review_1076-1064.md](../project_review_1076-1064.md) | report:docs/project_review_1076-1064.md (operator-confirmed) |
| #1064 | Recurse Haskell module-budget guard into nested split directories | 2026-08-02T22:47:35Z | [legacy] | — | — | [docs/project_review_1076-1064.md](../project_review_1076-1064.md) | report:docs/project_review_1076-1064.md (operator-confirmed) |
| #1063 | Encode the dump with aeson and give its two wait loops one definition | 2026-08-02T20:27:00Z | [legacy] | — | — | [docs/project_review_1063-1049.md](../project_review_1063-1049.md) | report:docs/project_review_1063-1049.md (operator-confirmed) |
| #1062 | Attach the game-clock Haddock to gameTimeRef | 2026-08-02T20:03:51Z | [legacy] | — | — | [docs/project_review_1063-1049.md](../project_review_1063-1049.md) | report:docs/project_review_1063-1049.md (operator-confirmed) |
| #1061 | Give the bindless texture-array and handle-table sizes a single definition | 2026-08-02T20:48:24Z | [legacy] | — | — | [docs/project_review_1063-1049.md](../project_review_1063-1049.md) | report:docs/project_review_1063-1049.md (operator-confirmed) |
| #1060 | Make -Werror part of the checked-in Cabal warning policy | 2026-08-02T19:44:20Z | [legacy] | — | — | [docs/project_review_1063-1049.md](../project_review_1063-1049.md) | report:docs/project_review_1063-1049.md (operator-confirmed) |
| #1056 | Replace repeated nested GraphicsState updates with one modifyGraphicsState helper | 2026-08-02T13:25:15Z | [legacy] | — | — | [docs/project_review_1063-1049.md](../project_review_1063-1049.md) | report:docs/project_review_1063-1049.md (operator-confirmed) |
| #1055 | Remove the phantom legacy path from the Vulkan texture system | 2026-08-02T14:05:20Z | [legacy] | — | — | [docs/project_review_1063-1049.md](../project_review_1063-1049.md) | report:docs/project_review_1063-1049.md (operator-confirmed) |
| #1054 | Fix six minor defects in the bindless texture and vertex modules | 2026-08-02T12:58:50Z | [legacy] | — | — | [docs/project_review_1063-1049.md](../project_review_1063-1049.md) | report:docs/project_review_1063-1049.md (operator-confirmed) |
| #1053 | Prefix the DevQueues and SwapchainSupportDetails field names | 2026-08-02T12:34:46Z | [legacy] | — | — | [docs/project_review_1063-1049.md](../project_review_1063-1049.md) | report:docs/project_review_1063-1049.md (operator-confirmed) |
| #1052 | Split Engine.Asset.YamlTextures into materials, vegetation, and the texture-name registry | 2026-08-02T05:01:41Z | [legacy] | — | — | [docs/project_review_1063-1049.md](../project_review_1063-1049.md) | report:docs/project_review_1063-1049.md (operator-confirmed) |
| #1051 | Delete the unconsumed half of Engine.Asset.Manager | 2026-08-02T14:39:59Z | [legacy] | — | — | [docs/project_review_1063-1049.md](../project_review_1063-1049.md) | report:docs/project_review_1063-1049.md (operator-confirmed) |
| #1050 | Remove eleven dead window wrappers from Engine.Graphics.Window.GLFW | 2026-08-02T16:57:03Z | [legacy] | — | — | [docs/project_review_1063-1049.md](../project_review_1063-1049.md) | report:docs/project_review_1063-1049.md (operator-confirmed) |
| #1049 | Remove the never-constructed ScriptFunction script value | 2026-08-02T04:36:30Z | [legacy] | — | — | [docs/project_review_1063-1049.md](../project_review_1063-1049.md) | report:docs/project_review_1063-1049.md (operator-confirmed) |
| #1048 | Remove the dead Engine.Graphics.Transform module | 2026-08-02T03:49:19Z | [legacy] | — | — | [docs/project_review_1048-1034.md](../project_review_1048-1034.md) | report:docs/project_review_1048-1034.md (operator-confirmed) |
| #1047 | Narrow five over-wide Engine export lists, delete unused withSceneGraph | 2026-08-02T03:23:17Z | [legacy] | — | — | [docs/project_review_1048-1034.md](../project_review_1048-1034.md) | report:docs/project_review_1048-1034.md (operator-confirmed) |
| #1046 | Document the three water-table fields the terrain dump layer emits | 2026-08-02T02:54:43Z | [legacy] | — | — | [docs/project_review_1048-1034.md](../project_review_1048-1034.md) | report:docs/project_review_1048-1034.md (operator-confirmed) |
| #1045 | Replace shutdownEngine's positional thread parameters with the named worker record | 2026-08-02T01:16:56Z | [legacy] | — | — | [docs/project_review_1048-1034.md](../project_review_1048-1034.md) | report:docs/project_review_1048-1034.md (operator-confirmed) |
| #1044 | Normalise the enforced Unicode operators and guard them against regression | 2026-08-02T00:54:17Z | [legacy] | — | — | [docs/project_review_1048-1034.md](../project_review_1048-1034.md) | report:docs/project_review_1048-1034.md (operator-confirmed) |
| #1043 | Unify normal and thread logging entry-construction and formatting | 2026-08-02T00:30:50Z | [legacy] | — | — | [docs/project_review_1048-1034.md](../project_review_1048-1034.md) | report:docs/project_review_1048-1034.md (operator-confirmed) |
| #1042 | Make logging source attribution independent of wrapper names | 2026-08-02T00:12:18Z | [legacy] | — | — | [docs/project_review_1048-1034.md](../project_review_1048-1034.md) | report:docs/project_review_1048-1034.md (operator-confirmed) |
| #1041 | Extract the EngineEnv-free helpers out of Engine.Scripting.Lua.API.Save | 2026-08-01T19:46:53Z | [legacy] | — | — | [docs/project_review_1048-1034.md](../project_review_1048-1034.md) | report:docs/project_review_1048-1034.md (operator-confirmed) |
| #1039 | Give the capability-record convention one documented home | 2026-08-01T19:23:50Z | [legacy] | — | — | [docs/project_review_1048-1034.md](../project_review_1048-1034.md) | report:docs/project_review_1048-1034.md (operator-confirmed) |
| #1038 | Rename allLayers to defaultLayers and build it with field syntax | 2026-08-01T20:12:11Z | [legacy] | — | — | [docs/project_review_1048-1034.md](../project_review_1048-1034.md) | report:docs/project_review_1048-1034.md (operator-confirmed) |
| #1037 | Derive the unknown-preview-category message from App.Cli's category lists | 2026-08-01T19:00:53Z | [legacy] | — | — | [docs/project_review_1048-1034.md](../project_review_1048-1034.md) | report:docs/project_review_1048-1034.md (operator-confirmed) |
| #1035 | Finalize blood decal documentation and gate the bleeding arc for epic closure | 2026-08-01T16:26:52Z | [legacy] | — | — | [docs/project_review_1035-1020.md](../project_review_1035-1020.md) | report:docs/project_review_1035-1020.md (operator-confirmed) |
| #1034 | Collapse the eleven identical asset YAML list loaders into one helper | 2026-08-01T16:49:34Z | [legacy] | — | — | [docs/project_review_1048-1034.md](../project_review_1048-1034.md) | report:docs/project_review_1048-1034.md (operator-confirmed) |
| #1033 | Remove EngineM's vestigial environment type parameter | 2026-08-01T15:59:17Z | [legacy] | — | — | [docs/project_review_1035-1020.md](../project_review_1035-1020.md) | report:docs/project_review_1035-1020.md (operator-confirmed) |
| #1032 | Identify Vulkan instances as Synarchy and remove unused graphics dimensions | 2026-08-01T15:34:13Z | [legacy] | — | — | [docs/project_review_1035-1020.md](../project_review_1035-1020.md) | report:docs/project_review_1035-1020.md (operator-confirmed) |
| #1030 | B1: Add the selected-unit -> eligible-container Transfer interaction | 2026-08-01T02:55:16Z | [legacy] | — | — | [docs/project_review_1035-1020.md](../project_review_1035-1020.md) | report:docs/project_review_1035-1020.md (operator-confirmed) |
| #1029 | Add configurable interval autosave, off by default (#913) | 2026-08-01T13:13:04Z | [legacy] | — | — | [docs/project_review_1035-1020.md](../project_review_1035-1020.md) | report:docs/project_review_1035-1020.md (operator-confirmed) |
| #1028 | --preview Phase 4: buildings viewer, flora/structures reuse, epic acceptance gate | 2026-08-01T02:32:55Z | [legacy] | — | — | [docs/project_review_1035-1020.md](../project_review_1035-1020.md) | report:docs/project_review_1035-1020.md (operator-confirmed) |
| #1027 | EngineEnv capability split E8: SaveLoadCapability, the permanent-only access flip, and the epic gate | 2026-08-01T02:07:39Z | [legacy] | — | — | [docs/project_review_1035-1020.md](../project_review_1035-1020.md) | report:docs/project_review_1035-1020.md (operator-confirmed) |
| #1026 | Give the three engine main loops one definition of the save-barrier drain and startup handshake | 2026-07-31T20:48:37Z | [legacy] | — | — | [docs/project_review_1035-1020.md](../project_review_1035-1020.md) | report:docs/project_review_1035-1020.md (operator-confirmed) |
| #1025 | Extract the shared boot config patch and error-path worker teardown into App.Boot | 2026-07-31T21:11:23Z | [legacy] | — | — | [docs/project_review_1035-1020.md](../project_review_1035-1020.md) | report:docs/project_review_1035-1020.md (operator-confirmed) |
| #1024 | Reject boot-mode flags that the selected mode ignores | 2026-07-31T20:26:28Z | [legacy] | — | — | [docs/project_review_1035-1020.md](../project_review_1035-1020.md) | report:docs/project_review_1035-1020.md (operator-confirmed) |
| #1023 | Downshift follow_command's pace with stamina instead of collapsing (#999) | 2026-07-31T16:43:32Z | [legacy] | — | — | [docs/project_review_1035-1020.md](../project_review_1035-1020.md) | report:docs/project_review_1035-1020.md (operator-confirmed) |
| #1020 | Keep already-latched tutorial branches visible | 2026-07-31T16:22:47Z | [legacy] | — | — | [docs/project_review_1035-1020.md](../project_review_1035-1020.md) | report:docs/project_review_1035-1020.md (operator-confirmed) |
| #1018 | Fix pathologically lethal shallow falls in Unit.Fall (#998) | 2026-07-31T16:01:30Z | [legacy] | — | — | [docs/project_review_1018-991.md](../project_review_1018-991.md) | report:docs/project_review_1018-991.md (operator-confirmed) |
| #1017 | Define the unit-to-container transfer contract and queued transaction foundation (#1000) | 2026-07-31T15:39:21Z | [legacy] | — | — | [docs/project_review_1018-991.md](../project_review_1018-991.md) | report:docs/project_review_1018-991.md (operator-confirmed) |
| #1015 | Guarantee at least one location in every generated world with land (#997) | 2026-07-31T12:51:08Z | [legacy] | — | — | [docs/project_review_1018-991.md](../project_review_1018-991.md) | report:docs/project_review_1018-991.md (operator-confirmed) |
| #1004 | EngineEnv capability split E5b: migrate the world→render handoff slots to a RenderHandoffCapability record | 2026-07-31T05:25:35Z | [legacy] | — | — | [docs/project_review_1018-991.md](../project_review_1018-991.md) | report:docs/project_review_1018-991.md (operator-confirmed) |
| #1003 | Remove redundant LANGUAGE pragmas outside src/World and src/Engine (3 of 3) | 2026-07-31T05:47:11Z | [legacy] | — | — | [docs/project_review_1018-991.md](../project_review_1018-991.md) | report:docs/project_review_1018-991.md (operator-confirmed) |
| #1002 | Remove redundant LANGUAGE pragmas under src/Engine (2 of 3) | 2026-07-31T05:05:32Z | [legacy] | — | — | [docs/project_review_1018-991.md](../project_review_1018-991.md) | report:docs/project_review_1018-991.md (operator-confirmed) |
| #1001 | Remove redundant LANGUAGE pragmas under src/World/ (1 of 3) | 2026-07-31T04:43:50Z | [legacy] | — | — | [docs/project_review_1018-991.md](../project_review_1018-991.md) | report:docs/project_review_1018-991.md (operator-confirmed) |
| #995 | Gate the first expedition end to end (#923) | 2026-07-31T03:09:46Z | [legacy] | — | — | [docs/project_review_1018-991.md](../project_review_1018-991.md) | report:docs/project_review_1018-991.md (operator-confirmed) |
| #994 | Archive the currentSaveVersion changelog out of World.Save.Types | 2026-07-31T03:36:09Z | [legacy] | — | — | [docs/project_review_1018-991.md](../project_review_1018-991.md) | report:docs/project_review_1018-991.md (operator-confirmed) |
| #993 | Remove unused legacy fields from EngineConfig | 2026-07-31T02:26:58Z | [legacy] | — | — | [docs/project_review_1018-991.md](../project_review_1018-991.md) | report:docs/project_review_1018-991.md (operator-confirmed) |
| #991 | Gate the first-session tutorial foundation (#922) | 2026-07-30T16:56:00Z | [legacy] | — | — | [docs/project_review_1018-991.md](../project_review_1018-991.md) | report:docs/project_review_1018-991.md (operator-confirmed) |
| #990 | Remove the never-drawn demo quad vertex buffer | 2026-07-30T17:24:39Z | [legacy] | — | — | [docs/project_review_1018-991.md](../project_review_1018-991.md) | report:docs/project_review_1018-991.md (operator-confirmed) |
| #989 | Remove the dead legacy font fragment shader | 2026-07-30T16:33:26Z | [legacy] | — | — | [docs/project_review_989-939.md](../project_review_989-939.md) | report:docs/project_review_989-939.md (operator-confirmed) |
| #988 | Render the tutorial HUD checklist (#960) | 2026-07-30T14:54:46Z | [legacy] | — | — | [docs/project_review_989-939.md](../project_review_989-939.md) | report:docs/project_review_989-939.md (operator-confirmed) |
| #987 | Remove the dead bindless texture-system teardown | 2026-07-30T15:16:31Z | [legacy] | — | — | [docs/project_review_989-939.md](../project_review_989-939.md) | report:docs/project_review_989-939.md (operator-confirmed) |
| #986 | Correct the bindless texture module header's slot-capacity claim | 2026-07-30T14:32:15Z | [legacy] | — | — | [docs/project_review_989-939.md](../project_review_989-939.md) | report:docs/project_review_989-939.md (operator-confirmed) |
| #966 | Close both cleanup branches in Font's Show instance | 2026-07-30T13:17:02Z | [legacy] | — | — | [docs/project_review_989-939.md](../project_review_989-939.md) | report:docs/project_review_989-939.md (operator-confirmed) |
| #963 | Evaluate the first-session tutorial objectives (#959) | 2026-07-27T17:21:22Z | [legacy] | — | — | [docs/project_review_989-939.md](../project_review_989-939.md) | report:docs/project_review_989-939.md (operator-confirmed) |
| #962 | Persist tutorial objective progress (#958) | 2026-07-27T16:11:28Z | [legacy] | — | — | [docs/project_review_989-939.md](../project_review_989-939.md) | report:docs/project_review_989-939.md (operator-confirmed) |
| #961 | Define and load the first-session tutorial tree (#957) | 2026-07-27T14:46:45Z | [legacy] | — | — | [docs/project_review_989-939.md](../project_review_989-939.md) | report:docs/project_review_989-939.md (operator-confirmed) |
| #955 | Remove fixed spawn-only items from ruin loot (#921) | 2026-07-27T03:45:10Z | [legacy] | — | — | [docs/project_review_989-939.md](../project_review_989-939.md) | report:docs/project_review_989-939.md (operator-confirmed) |
| #954 | Give units their own knowledge of discovered locations, alongside global player discovery | 2026-07-27T00:51:04Z | [legacy] | — | — | [docs/project_review_989-939.md](../project_review_989-939.md) | report:docs/project_review_989-939.md (operator-confirmed) |
| #953 | Make location loot-table rolls seed-stable per instance | 2026-07-26T22:06:47Z | [legacy] | — | — | [docs/project_review_989-939.md](../project_review_989-939.md) | report:docs/project_review_989-939.md (operator-confirmed) |
| #941 | Add BuildingCapability and narrow the last 14 units-buildings-combat consumers (#896) | 2026-07-26T14:29:02Z | [legacy] | — | — | [docs/project_review_938-910.md](../project_review_938-910.md) | report:docs/project_review_938-910.md (operator-confirmed) |
| #940 | Stationary and collapsed-unit blood pooling via layered bounded spawns (#883) | 2026-07-26T14:07:10Z | [legacy] | — | — | [docs/project_review_938-910.md](../project_review_938-910.md) | report:docs/project_review_938-910.md (operator-confirmed) |
| #939 | EngineEnv capability split E7b: EventsCapability for player-event, notification, and popup consumers | 2026-07-26T16:50:54Z | [legacy] | — | — | [docs/project_review_989-939.md](../project_review_989-939.md) | report:docs/project_review_989-939.md (operator-confirmed) |
| #938 | --preview units/<name>: unit animation and direction viewer (#887) | 2026-07-26T14:54:32Z | [legacy] | — | — | [docs/project_review_938-910.md](../project_review_938-910.md) | report:docs/project_review_938-910.md (operator-confirmed) |
| #937 | Record first-expedition survival calibration observations (#919) | 2026-07-26T13:44:57Z | [legacy] | — | — | [docs/project_review_938-910.md](../project_review_938-910.md) | report:docs/project_review_938-910.md (operator-confirmed) |
| #935 | Add UiCapability and narrow 15 UI/focus/HUD consumers (#897) | 2026-07-26T02:43:55Z | [legacy] | — | — | [docs/project_review_938-910.md](../project_review_938-910.md) | report:docs/project_review_938-910.md (operator-confirmed) |
| #930 | EngineEnv capability split E6a: UnitCombatCapability + 35 narrowed consumers | 2026-07-26T01:45:35Z | [legacy] | — | — | [docs/project_review_938-910.md](../project_review_938-910.md) | report:docs/project_review_938-910.md (operator-confirmed) |
| #929 | Prove expedition retrieval and return end to end (#920) | 2026-07-26T03:04:28Z | [legacy] | — | — | [docs/project_review_938-910.md](../project_review_938-910.md) | report:docs/project_review_938-910.md (operator-confirmed) |
| #928 | Replace ad-hoc faction string comparison with a typed relation model | 2026-07-25T23:12:42Z | [legacy] | — | — | [docs/project_review_938-910.md](../project_review_938-910.md) | report:docs/project_review_938-910.md (operator-confirmed) |
| #927 | Add manual first-expedition gameplay scenarios (#925) | 2026-07-25T19:27:06Z | [legacy] | — | — | [docs/project_review_938-910.md](../project_review_938-910.md) | report:docs/project_review_938-910.md (operator-confirmed) |
| #926 | Give placed locations a stable instance identity and gameplay lifecycle (#911) | 2026-07-25T19:48:32Z | [legacy] | — | — | [docs/project_review_938-910.md](../project_review_938-910.md) | report:docs/project_review_938-910.md (operator-confirmed) |
| #924 | Declare the expedition-arc scope rule in CLAUDE.md (#914) | 2026-07-25T14:44:21Z | [legacy] | — | — | [docs/project_review_938-910.md](../project_review_938-910.md) | report:docs/project_review_938-910.md (operator-confirmed) |
| #910 | Migrate input and Lua-transport consumers to an InputCapability record (#892) | 2026-07-25T01:24:06Z | [legacy] | — | — | [docs/project_review_938-910.md](../project_review_938-910.md) | report:docs/project_review_938-910.md (operator-confirmed) |
| #909 | EngineEnv capability split E5a: WorldSimCapability for the world/sim consumers | 2026-07-25T00:42:34Z | [legacy] | — | — | [docs/project_review_909-874.md](../project_review_909-874.md) | report:docs/project_review_909-874.md (operator-confirmed) |
| #908 | Cache windowed geometry on the transition, not on vcWindowMode (#907) | 2026-07-24T22:57:32Z | [legacy] | — | — | [docs/project_review_909-874.md](../project_review_909-874.md) | report:docs/project_review_909-874.md (operator-confirmed) |
| #906 | EngineEnv capability split E3: migrate render/window/Vulkan/asset consumers to a RenderCapability record | 2026-07-24T21:33:17Z | [legacy] | — | — | [docs/project_review_909-874.md](../project_review_909-874.md) | report:docs/project_review_909-874.md (operator-confirmed) |
| #905 | EngineEnv capability split E2: ContentRegistries capability record (#890) | 2026-07-24T19:56:59Z | [legacy] | — | — | [docs/project_review_909-874.md](../project_review_909-874.md) | report:docs/project_review_909-874.md (operator-confirmed) |
| #904 | Bleeding trails: bounded blood-mark emission from moving units (#882) | 2026-07-24T18:17:42Z | [legacy] | — | — | [docs/project_review_909-874.md](../project_review_909-874.md) | report:docs/project_review_909-874.md (operator-confirmed) |
| #903 | Preview browser Phase 2: canonical categories + simple-category browsing (#886) | 2026-07-24T17:11:01Z | [legacy] | — | — | [docs/project_review_909-874.md](../project_review_909-874.md) | report:docs/project_review_909-874.md (operator-confirmed) |
| #902 | EngineEnv capability split E1: CoreCapability record + full-access ratchet | 2026-07-24T14:12:02Z | [legacy] | — | — | [docs/project_review_909-874.md](../project_review_909-874.md) | report:docs/project_review_909-874.md (operator-confirmed) |
| #881 | Add EngineEnv capability inventory and its CI audit | 2026-07-24T00:13:23Z | [legacy] | — | — | [docs/project_review_909-874.md](../project_review_909-874.md) | report:docs/project_review_909-874.md (operator-confirmed) |
| #880 | Split Unit.Types into focused submodules | 2026-07-23T19:29:14Z | [legacy] | — | — | [docs/project_review_909-874.md](../project_review_909-874.md) | report:docs/project_review_909-874.md (operator-confirmed) |
| #879 | Isolate craft_probe's base-quality checks from mental effectiveness | 2026-07-23T17:46:32Z | [legacy] | — | — | [docs/project_review_909-874.md](../project_review_909-874.md) | report:docs/project_review_909-874.md (operator-confirmed) |
| #875 | Split World.Generate.Timeline: extract spike removal, drop stale comment | 2026-07-23T17:28:13Z | [legacy] | — | — | [docs/project_review_909-874.md](../project_review_909-874.md) | report:docs/project_review_909-874.md (operator-confirmed) |
| #874 | Add mental-effectiveness combat/craft tie-ins from state of mind (#353) | 2026-07-22T16:39:04Z | [legacy] | — | — | [docs/project_review_909-874.md](../project_review_909-874.md) | report:docs/project_review_909-874.md (operator-confirmed) |
| #873 | Fix data_codec integer/float round-trip above 2^53 (#865) | 2026-07-22T13:20:40Z | [legacy] | — | — | [docs/project_review_873-860.md](../project_review_873-860.md) | report:docs/project_review_873-860.md (operator-confirmed) |
| #872 | [save-overhaul D1] Add the end-to-end persistence contract suite | 2026-07-22T12:43:37Z | [legacy] | — | — | [docs/project_review_873-860.md](../project_review_873-860.md) | report:docs/project_review_873-860.md (operator-confirmed) |
| #871 | [UI hardening C3] Align rendered box overflow with interactive bounds (#749) | 2026-07-22T02:33:15Z | [legacy] | — | — | [docs/project_review_873-860.md](../project_review_873-860.md) | report:docs/project_review_873-860.md (operator-confirmed) |
| #870 | Fix save_modules.applyAll's no-prepared-load diagnostic and prove crash recovery (#864) | 2026-07-21T18:55:08Z | [legacy] | — | — | [docs/project_review_873-860.md](../project_review_873-860.md) | report:docs/project_review_873-860.md (operator-confirmed) |
| #869 | [save-overhaul C4] Establish component migrations and tracked compatibility fixtures | 2026-07-21T17:46:03Z | [legacy] | — | — | [docs/project_review_873-860.md](../project_review_873-860.md) | report:docs/project_review_873-860.md (operator-confirmed) |
| #868 | [save-overhaul C3] Add typed persistent references and a shared integrity graph | 2026-07-20T14:32:17Z | [legacy] | — | — | [docs/project_review_873-860.md](../project_review_873-860.md) | report:docs/project_review_873-860.md (operator-confirmed) |
| #867 | [UI hardening C4] Migrate gameplay HUD/overlays onto the responsive resize contract | 2026-07-20T20:12:23Z | [legacy] | — | — | [docs/project_review_873-860.md](../project_review_873-860.md) | report:docs/project_review_873-860.md (operator-confirmed) |
| #866 | [UI hardening C2] Add responsive lifecycle contract for menu screens | 2026-07-20T04:11:04Z | [legacy] | — | — | [docs/project_review_873-860.md](../project_review_873-860.md) | report:docs/project_review_873-860.md (operator-confirmed) |
| #863 | [save-overhaul C2] Stage and atomically publish whole-session loads | 2026-07-20T03:54:54Z | [legacy] | — | — | [docs/project_review_873-860.md](../project_review_873-860.md) | report:docs/project_review_873-860.md (operator-confirmed) |
| #862 | [save-overhaul B3] Make Lua persistence versioned, scoped, and fail-fast | 2026-07-19T03:11:34Z | [legacy] | — | — | [docs/project_review_873-860.md](../project_review_873-860.md) | report:docs/project_review_873-860.md (operator-confirmed) |
| #861 | Publish saves atomically with lossless previous-generation recovery (#762) | 2026-07-18T19:35:53Z | [legacy] | — | — | [docs/project_review_873-860.md](../project_review_873-860.md) | report:docs/project_review_873-860.md (operator-confirmed) |
| #860 | [save-overhaul B2] Split Haskell persistence into independently versioned components (#760) | 2026-07-18T04:18:18Z | [legacy] | — | — | [docs/project_review_873-860.md](../project_review_873-860.md) | report:docs/project_review_873-860.md (operator-confirmed) |
| #859 | Add embark-to-discovery integration probe (#782) | 2026-07-17T18:49:04Z | [legacy] | — | — | [docs/project_review_859-848.md](../project_review_859-848.md) | report:docs/project_review_859-848.md (operator-confirmed) |
| #858 | Guard remote-warning establishHere() against an active-world switch (#844) | 2026-07-17T18:33:43Z | [legacy] | — | — | [docs/project_review_859-848.md](../project_review_859-848.md) | report:docs/project_review_859-848.md (operator-confirmed) |
| #857 | [UI hardening C1] Add opt-in clipping and viewport-aware popup placement | 2026-07-16T22:19:50Z | [legacy] | — | — | [docs/project_review_859-848.md](../project_review_859-848.md) | report:docs/project_review_859-848.md (operator-confirmed) |
| #856 | Add discrete-control release-activation and keyboard control focus (#745) | 2026-07-17T01:57:42Z | [legacy] | — | — | [docs/project_review_859-848.md](../project_review_859-848.md) | report:docs/project_review_859-848.md (operator-confirmed) |
| #855 | [locations] Render paired discovery-state icons on the zoom map | 2026-07-16T17:36:50Z | [legacy] | — | — | [docs/project_review_859-848.md](../project_review_859-848.md) | report:docs/project_review_859-848.md (operator-confirmed) |
| #854 | [save-overhaul B1] Introduce the tagged checksummed v83 save envelope | 2026-07-16T17:23:34Z | [legacy] | — | — | [docs/project_review_859-848.md](../project_review_859-848.md) | report:docs/project_review_859-848.md (operator-confirmed) |
| #853 | Persist location discovery when player units approach (#780) | 2026-07-16T15:02:28Z | [legacy] | — | — | [docs/project_review_859-848.md](../project_review_859-848.md) | report:docs/project_review_859-848.md (operator-confirmed) |
| #852 | Capture an immutable validated session snapshot (#758) | 2026-07-16T16:09:51Z | [legacy] | — | — | [docs/project_review_859-848.md](../project_review_859-848.md) | report:docs/project_review_859-848.md (operator-confirmed) |
| #851 | Add persisted until-stock craft-bill mode (#795) | 2026-07-16T02:09:34Z | [legacy] | — | — | [docs/project_review_859-848.md](../project_review_859-848.md) | report:docs/project_review_859-848.md (operator-confirmed) |
| #850 | Make solar generation follow longitude-local daylight (#794) | 2026-07-15T22:15:10Z | [legacy] | — | — | [docs/project_review_859-848.md](../project_review_859-848.md) | report:docs/project_review_859-848.md (operator-confirmed) |
| #849 | Make F3 click correlation identify actual input controls | 2026-07-15T21:20:53Z | [legacy] | — | — | [docs/project_review_859-848.md](../project_review_859-848.md) | report:docs/project_review_859-848.md (operator-confirmed) |
| #848 | Attribute playtest oracle evidence to the action that produced it (#775) | 2026-07-15T20:31:45Z | [legacy] | — | — | [docs/project_review_859-848.md](../project_review_859-848.md) | report:docs/project_review_859-848.md (operator-confirmed) |
| #847 | Convert F4 Layer-A click/drag/scroll locations to framebuffer space | 2026-07-15T19:41:40Z | [legacy] | — | — | [docs/project_review_847-834.md](../project_review_847-834.md) | report:docs/project_review_847-834.md (operator-confirmed) |
| #846 | Fix false-accepted outcome for unbound gameplay keys | 2026-07-15T18:47:10Z | [legacy] | — | — | [docs/project_review_847-834.md](../project_review_847-834.md) | report:docs/project_review_847-834.md (operator-confirmed) |
| #845 | Route all wheel input through the active UI policy (#744) | 2026-07-15T17:48:51Z | [legacy] | — | — | [docs/project_review_847-834.md](../project_review_847-834.md) | report:docs/project_review_847-834.md (operator-confirmed) |
| #843 | [locations] Warn before establishing a portal remotely from all locations | 2026-07-15T16:29:14Z | [legacy] | — | — | [docs/project_review_847-834.md](../project_review_847-834.md) | report:docs/project_review_847-834.md (operator-confirmed) |
| #842 | review-gate: don't strip reviewed:approve for no-op branch-update pushes | 2026-07-15T13:37:37Z | [legacy] | — | — | [docs/project_review_847-834.md](../project_review_847-834.md) | report:docs/project_review_847-834.md (operator-confirmed) |
| #841 | Stop paused craft bills after their current cycle | 2026-07-15T03:45:15Z | [legacy] | — | — | [docs/project_review_847-834.md](../project_review_847-834.md) | report:docs/project_review_847-834.md (operator-confirmed) |
| #840 | Preserve structure material payment when a construction claimant dies (#799) | 2026-07-15T16:44:29Z | [legacy] | — | — | [docs/project_review_847-834.md](../project_review_847-834.md) | report:docs/project_review_847-834.md (operator-confirmed) |
| #839 | Reject unsupported location anchor tags instead of dropping their constraints | 2026-07-15T02:44:55Z | [legacy] | — | — | [docs/project_review_847-834.md](../project_review_847-834.md) | report:docs/project_review_847-834.md (operator-confirmed) |
| #838 | Resolve unit vision and combat awareness from the unit's own world page | 2026-07-15T03:00:53Z | [legacy] | — | — | [docs/project_review_847-834.md](../project_review_847-834.md) | report:docs/project_review_847-834.md (operator-confirmed) |
| #837 | [locations] Block portal placement inside location bounds and verify ghost tint | 2026-07-15T02:24:19Z | [legacy] | — | — | [docs/project_review_847-834.md](../project_review_847-834.md) | report:docs/project_review_847-834.md (operator-confirmed) |
| #836 | Make flora_growth_probe's year-round-harvest fixture probe-owned | 2026-07-15T02:08:54Z | [legacy] | — | — | [docs/project_review_847-834.md](../project_review_847-834.md) | report:docs/project_review_847-834.md (operator-confirmed) |
| #835 | Redistribute shed mountain soil to lower terrain (#812) | 2026-07-15T01:36:37Z | [legacy] | — | — | [docs/project_review_835-822.md](../project_review_835-822.md) | report:docs/project_review_835-822.md (operator-confirmed) |
| #834 | Implement calorie-store hungry/starving threshold effects | 2026-07-15T01:53:19Z | [legacy] | — | — | [docs/project_review_847-834.md](../project_review_847-834.md) | report:docs/project_review_847-834.md (operator-confirmed) |
| #833 | Make the location-content probe validate the live loot registry | 2026-07-15T01:06:04Z | [legacy] | — | — | [docs/project_review_835-822.md](../project_review_835-822.md) | report:docs/project_review_835-822.md (operator-confirmed) |
| #832 | Render committed building blueprints across their full footprint | 2026-07-15T01:20:35Z | [legacy] | — | — | [docs/project_review_835-822.md](../project_review_835-822.md) | report:docs/project_review_835-822.md (operator-confirmed) |
| #831 | Tile-Z regression coverage bypasses the UI wiring that lost Z | 2026-07-15T00:34:45Z | [legacy] | — | — | [docs/project_review_835-822.md](../project_review_835-822.md) | report:docs/project_review_835-822.md (operator-confirmed) |
| #830 | Remove duplicate slider element lookup and preserve its full handle contract | 2026-07-15T00:09:31Z | [legacy] | — | — | [docs/project_review_835-822.md](../project_review_835-822.md) | report:docs/project_review_835-822.md (operator-confirmed) |
| #829 | Reject construction jobs for already-occupied structure slots | 2026-07-14T23:54:21Z | [legacy] | — | — | [docs/project_review_835-822.md](../project_review_835-822.md) | report:docs/project_review_835-822.md (operator-confirmed) |
| #828 | Fix #816: relax freshwater renderer's exact-one-drop slope rule | 2026-07-14T21:22:58Z | [legacy] | — | — | [docs/project_review_835-822.md](../project_review_835-822.md) | report:docs/project_review_835-822.md (operator-confirmed) |
| #827 | [save-overhaul A2] Add coordinated paused snapshot barrier | 2026-07-14T21:08:50Z | [legacy] | — | — | [docs/project_review_835-822.md](../project_review_835-822.md) | report:docs/project_review_835-822.md (operator-confirmed) |
| #826 | Give Tiny/Small worlds inland-origin rivers without breaching calderas (#811) | 2026-07-14T00:53:55Z | [legacy] | — | — | [docs/project_review_835-822.md](../project_review_835-822.md) | report:docs/project_review_835-822.md (operator-confirmed) |
| #825 | Complete the remaining Lua decodeUtf8Lenient sweep | 2026-07-13T23:45:15Z | [legacy] | — | — | [docs/project_review_835-822.md](../project_review_835-822.md) | report:docs/project_review_835-822.md (operator-confirmed) |
| #824 | Fix: input.* primary-timeout ack is indeterminate, not retry-safe | 2026-07-13T23:59:20Z | [legacy] | — | — | [docs/project_review_835-822.md](../project_review_835-822.md) | report:docs/project_review_835-822.md (operator-confirmed) |
| #823 | Snapshot the clicked chunk for zoom-map selection | 2026-07-13T21:32:06Z | [legacy] | — | — | [docs/project_review_823-789.md](../project_review_823-789.md) | report:docs/project_review_823-789.md (operator-confirmed) |
| #822 | Keep pathing costs finite under extreme configuration and terrain | 2026-07-13T21:45:13Z | [legacy] | — | — | [docs/project_review_835-822.md](../project_review_835-822.md) | report:docs/project_review_835-822.md (operator-confirmed) |
| #821 | Re-split Engine.Input.Thread and guard its reviewability boundary | 2026-07-13T21:14:26Z | [legacy] | — | — | [docs/project_review_823-789.md](../project_review_823-789.md) | report:docs/project_review_823-789.md (operator-confirmed) |
| #820 | Eliminate the mutable CI-image publication race | 2026-07-13T20:59:22Z | [legacy] | — | — | [docs/project_review_823-789.md](../project_review_823-789.md) | report:docs/project_review_823-789.md (operator-confirmed) |
| #819 | Define authoritative spatial bounds for placed locations | 2026-07-13T17:35:31Z | [legacy] | — | — | [docs/project_review_823-789.md](../project_review_823-789.md) | report:docs/project_review_823-789.md (operator-confirmed) |
| #818 | Demote movement probe from CI_ELIGIBLE to manual-only targeted | 2026-07-13T17:09:04Z | [legacy] | — | — | [docs/project_review_823-789.md](../project_review_823-789.md) | report:docs/project_review_823-789.md (operator-confirmed) |
| #817 | Bound CI build-cache churn to one snapshot per dependency plan (#790) | 2026-07-13T16:56:40Z | [legacy] | — | — | [docs/project_review_823-789.md](../project_review_823-789.md) | report:docs/project_review_823-789.md (operator-confirmed) |
| #810 | Dispose blood GPU textures when world pages are replaced or destroyed (#788) | 2026-07-13T17:22:25Z | [legacy] | — | — | [docs/project_review_823-789.md](../project_review_823-789.md) | report:docs/project_review_823-789.md (operator-confirmed) |
| #809 | Make editable Lua widgets Unicode-safe | 2026-07-13T16:43:45Z | [legacy] | — | — | [docs/project_review_823-789.md](../project_review_823-789.md) | report:docs/project_review_823-789.md (operator-confirmed) |
| #808 | Isolate periodic thoughts from the state-of-mind probe | 2026-07-13T16:29:41Z | [legacy] | — | — | [docs/project_review_823-789.md](../project_review_823-789.md) | report:docs/project_review_823-789.md (operator-confirmed) |
| #804 | Migrate legacy runtime config to new local paths | 2026-07-13T16:14:14Z | [legacy] | — | — | [docs/project_review_823-789.md](../project_review_823-789.md) | report:docs/project_review_823-789.md (operator-confirmed) |
| #803 | Clarify starvation in player manual | 2026-07-13T15:58:39Z | [legacy] | — | — | [docs/project_review_823-789.md](../project_review_823-789.md) | report:docs/project_review_823-789.md (operator-confirmed) |
| #792 | [tooling] Re-verify the seven base-failing probes and record accurate failure classifications | 2026-07-12T21:02:26Z | [legacy] | — | — | [docs/project_review_792-740.md](../project_review_792-740.md) | report:docs/project_review_792-740.md (operator-confirmed) |
| #791 | Rebuild final regional climate from the timeline's evolved forcing | 2026-07-12T20:50:00Z | [legacy] | — | — | [docs/project_review_792-740.md](../project_review_792-740.md) | report:docs/project_review_792-740.md (operator-confirmed) |
| #789 | Add generated-language profile generator and native proper-name renderer (#710) | 2026-07-12T21:14:25Z | [legacy] | — | — | [docs/project_review_823-789.md](../project_review_823-789.md) | report:docs/project_review_823-789.md (operator-confirmed) |
| #770 | [UI hardening A2] Separate pointer blocking, click handling, and scroll capture | 2026-07-12T19:05:31Z | [legacy] | — | — | [docs/project_review_792-740.md](../project_review_792-740.md) | report:docs/project_review_792-740.md (operator-confirmed) |
| #769 | [save-overhaul A1] Define the persistence contract and audited state inventory | 2026-07-12T19:51:23Z | [legacy] | — | — | [docs/project_review_792-740.md](../project_review_792-740.md) | report:docs/project_review_792-740.md (operator-confirmed) |
| #765 | Give LayerModal pages a real input-exclusive boundary (#742) | 2026-07-12T15:40:18Z | [legacy] | — | — | [docs/project_review_792-740.md](../project_review_792-740.md) | report:docs/project_review_792-740.md (operator-confirmed) |
| #755 | Split World.Geology.Timeline.River into focused submodules | 2026-07-12T15:28:21Z | [legacy] | — | — | [docs/project_review_792-740.md](../project_review_792-740.md) | report:docs/project_review_792-740.md (operator-confirmed) |
| #754 | Review the 5 unclassified behavior probes: promote 3, classify 2 | 2026-07-12T15:16:00Z | [legacy] | — | — | [docs/project_review_792-740.md](../project_review_792-740.md) | report:docs/project_review_792-740.md (operator-confirmed) |
| #753 | Give the Create World preview pane real pre-generation art | 2026-07-12T15:04:15Z | [legacy] | — | — | [docs/project_review_792-740.md](../project_review_792-740.md) | report:docs/project_review_792-740.md (operator-confirmed) |
| #752 | [content] Backfill the 8 missing textures for shipped power + cooking content | 2026-07-12T14:53:21Z | [legacy] | — | — | [docs/project_review_792-740.md](../project_review_792-740.md) | report:docs/project_review_792-740.md (operator-confirmed) |
| #751 | Split World.Geology.Timeline.RiverTrace into focused submodules | 2026-07-12T05:46:01Z | [legacy] | — | — | [docs/project_review_792-740.md](../project_review_792-740.md) | report:docs/project_review_792-740.md (operator-confirmed) |
| #740 | Split World.Geology.Timeline.Types into focused submodules | 2026-07-12T04:28:16Z | [legacy] | — | — | [docs/project_review_792-740.md](../project_review_792-740.md) | report:docs/project_review_792-740.md (operator-confirmed) |
| #739 | Grow the semantic concept catalogue to 150 entries | 2026-07-12T03:55:02Z | [legacy] | — | — | [docs/project_review_739-716.md](../project_review_739-716.md) | report:docs/project_review_739-716.md (operator-confirmed) |
| #738 | Instrument F4 Layer A for keyboard, text, scroll, and drag routing | 2026-07-12T03:18:39Z | [legacy] | — | — | [docs/project_review_739-716.md](../project_review_739-716.md) | report:docs/project_review_739-716.md (operator-confirmed) |
| #737 | [psychology] Mental states slice 2: catatonia and lash-out break behaviours | 2026-07-12T05:00:30Z | [legacy] | — | — | [docs/project_review_792-740.md](../project_review_792-740.md) | report:docs/project_review_792-740.md (operator-confirmed) |
| #736 | Distinguish interrupted playtest steps from never-started ones | 2026-07-12T00:48:31Z | [legacy] | — | — | [docs/project_review_739-716.md](../project_review_739-716.md) | report:docs/project_review_739-716.md (operator-confirmed) |
| #735 | Fix playtest critic dropping live F4 action outcomes | 2026-07-11T14:36:20Z | [legacy] | — | — | [docs/project_review_739-716.md](../project_review_739-716.md) | report:docs/project_review_739-716.md (operator-confirmed) |
| #734 | Register the 12 orphaned behavior probes and sync probe docs | 2026-07-11T14:08:16Z | [legacy] | — | — | [docs/project_review_739-716.md](../project_review_739-716.md) | report:docs/project_review_739-716.md (operator-confirmed) |
| #733 | Give every behavior probe a --port flag, retire fixed-port special cases | 2026-07-10T23:09:20Z | [legacy] | — | — | [docs/project_review_739-716.md](../project_review_739-716.md) | report:docs/project_review_739-716.md (operator-confirmed) |
| #732 | Make sequential input.* acks a real modifier-lifetime boundary | 2026-07-11T14:19:22Z | [legacy] | — | — | [docs/project_review_739-716.md](../project_review_739-716.md) | report:docs/project_review_739-716.md (operator-confirmed) |
| #731 | Persist player-facing world identity separately from page and save names | 2026-07-10T20:34:19Z | [legacy] | — | — | [docs/project_review_739-716.md](../project_review_739-716.md) | report:docs/project_review_739-716.md (operator-confirmed) |
| #720 | Fix playtest event-log delta tracking | 2026-07-10T17:19:01Z | [legacy] | — | — | [docs/project_review_739-716.md](../project_review_739-716.md) | report:docs/project_review_739-716.md (operator-confirmed) |
| #719 | Remove inert Create World controls | 2026-07-10T18:21:22Z | [legacy] | — | — | [docs/project_review_739-716.md](../project_review_739-716.md) | report:docs/project_review_739-716.md (operator-confirmed) |
| #718 | Record playtest trace phases truthfully; replay only executed phases | 2026-07-10T15:22:15Z | [legacy] | — | — | [docs/project_review_739-716.md](../project_review_739-716.md) | report:docs/project_review_739-716.md (operator-confirmed) |
| #716 | Mental states: stressed → break + euphoria over state_of_mind (#352) | 2026-07-10T14:52:02Z | [legacy] | — | — | [docs/project_review_739-716.md](../project_review_739-716.md) | report:docs/project_review_739-716.md (operator-confirmed) |
| #715 | C2: Seed-driven persona generation for the playtest harness | 2026-07-10T14:25:56Z | [legacy] | — | — | [docs/project_review_715-694.md](../project_review_715-694.md) | report:docs/project_review_715-694.md (operator-confirmed) |
| #714 | Fence synthetic modifier releases behind their action's Lua callbacks | 2026-07-10T13:14:52Z | [legacy] | — | — | [docs/project_review_715-694.md](../project_review_715-694.md) | report:docs/project_review_715-694.md (operator-confirmed) |
| #712 | Model semantic proper names and render English glosses | 2026-07-10T04:45:30Z | [legacy] | — | — | [docs/project_review_715-694.md](../project_review_715-694.md) | report:docs/project_review_715-694.md (operator-confirmed) |
| #711 | Offscreen GPU render mode: --offscreen, window off, render on (#650) | 2026-07-10T04:33:33Z | [legacy] | — | — | [docs/project_review_715-694.md](../project_review_715-694.md) | report:docs/project_review_715-694.md (operator-confirmed) |
| #705 | Gate swapchain TRANSFER_SRC usage on surface capabilities | 2026-07-10T00:53:13Z | [legacy] | — | — | [docs/project_review_715-694.md](../project_review_715-694.md) | report:docs/project_review_715-694.md (operator-confirmed) |
| #704 | F4 — Rejected-action / silent-failure oracle tap (debug.drainActionOutcomes) | 2026-07-10T04:13:01Z | [legacy] | — | — | [docs/project_review_715-694.md](../project_review_715-694.md) | report:docs/project_review_715-694.md (operator-confirmed) |
| #703 | [tech-debt] Split World.ZoomMap.Cache into smaller modules | 2026-07-10T00:00:34Z | [legacy] | — | — | [docs/project_review_715-694.md](../project_review_715-694.md) | report:docs/project_review_715-694.md (operator-confirmed) |
| #702 | [tech-debt] Split World.Thread.Command.Cursor into smaller modules | 2026-07-09T23:45:53Z | [legacy] | — | — | [docs/project_review_715-694.md](../project_review_715-694.md) | report:docs/project_review_715-694.md (operator-confirmed) |
| #701 | H2: Critic — oracle-grounded friction triage + UX report | 2026-07-10T00:24:22Z | [legacy] | — | — | [docs/project_review_715-694.md](../project_review_715-694.md) | report:docs/project_review_715-694.md (operator-confirmed) |
| #696 | H1: Player harness — lockstep runner + naive player agent + session trace | 2026-07-09T22:25:05Z | [legacy] | — | — | [docs/project_review_715-694.md](../project_review_715-694.md) | report:docs/project_review_715-694.md (operator-confirmed) |
| #695 | Split World.Plate into smaller modules | 2026-07-09T22:12:22Z | [legacy] | — | — | [docs/project_review_715-694.md](../project_review_715-694.md) | report:docs/project_review_715-694.md (operator-confirmed) |
| #694 | [tech-debt] Split World.Geology.Erosion into smaller modules | 2026-07-09T22:00:40Z | [legacy] | — | — | [docs/project_review_715-694.md](../project_review_715-694.md) | report:docs/project_review_715-694.md (operator-confirmed) |
| #693 | [tech-debt] Split World.Fluid.Lake.Identify into smaller modules | 2026-07-09T21:48:59Z | [legacy] | — | — | [docs/project_review_693-682.md](../project_review_693-682.md) | report:docs/project_review_693-682.md (operator-confirmed) |
| #692 | Split World.Hydrology.Simulation into focused submodules | 2026-07-09T21:37:19Z | [legacy] | — | — | [docs/project_review_693-682.md](../project_review_693-682.md) | report:docs/project_review_693-682.md (operator-confirmed) |
| #691 | Split World.Weather.Generate into smaller modules | 2026-07-09T21:27:33Z | [legacy] | — | — | [docs/project_review_693-682.md](../project_review_693-682.md) | report:docs/project_review_693-682.md (operator-confirmed) |
| #690 | F2: Synthetic input injection verbs (input.click / moveMouse / key / scroll / type) | 2026-07-09T21:05:07Z | [legacy] | — | — | [docs/project_review_693-682.md](../project_review_693-682.md) | report:docs/project_review_693-682.md (operator-confirmed) |
| #689 | Split Engine.Scripting.Lua.Message into smaller modules | 2026-07-09T20:33:16Z | [legacy] | — | — | [docs/project_review_693-682.md](../project_review_693-682.md) | report:docs/project_review_693-682.md (operator-confirmed) |
| #688 | [tech-debt] Split Engine.Scripting.Lua.Thread into smaller modules | 2026-07-09T20:20:33Z | [legacy] | — | — | [docs/project_review_693-682.md](../project_review_693-682.md) | report:docs/project_review_693-682.md (operator-confirmed) |
| #687 | F1: Vulkan framebuffer screenshot verb (debug.captureScreenshot) | 2026-07-09T20:06:48Z | [legacy] | — | — | [docs/project_review_693-682.md](../project_review_693-682.md) | report:docs/project_review_693-682.md (operator-confirmed) |
| #686 | [tech-debt] Split Engine.Scripting.Lua.API.Items into smaller modules | 2026-07-09T19:55:05Z | [legacy] | — | — | [docs/project_review_693-682.md](../project_review_693-682.md) | report:docs/project_review_693-682.md (operator-confirmed) |
| #685 | [tech-debt] Split Engine.Scripting.Lua.API.Forage into submodules | 2026-07-09T19:43:23Z | [legacy] | — | — | [docs/project_review_693-682.md](../project_review_693-682.md) | report:docs/project_review_693-682.md (operator-confirmed) |
| #684 | [tech-debt] Split Combat.Resolution into smaller modules | 2026-07-09T19:31:42Z | [legacy] | — | — | [docs/project_review_693-682.md](../project_review_693-682.md) | report:docs/project_review_693-682.md (operator-confirmed) |
| #683 | C1: Player-facing minimal manual (docs/player_manual.md) | 2026-07-09T19:20:01Z | [legacy] | — | — | [docs/project_review_693-682.md](../project_review_693-682.md) | report:docs/project_review_693-682.md (operator-confirmed) |
| #682 | [tech-debt] Split World.Generate.Chunk into smaller modules | 2026-07-09T19:01:55Z | [legacy] | — | — | [docs/project_review_693-682.md](../project_review_693-682.md) | report:docs/project_review_693-682.md (operator-confirmed) |
| #681 | Split World.Fluid.River.Identify into smaller modules | 2026-07-09T18:50:14Z | [legacy] | — | — | [docs/project_review_681-669.md](../project_review_681-669.md) | report:docs/project_review_681-669.md (operator-confirmed) |
| #680 | [tech-debt] Split World.Render.Quads into smaller modules | 2026-07-09T18:38:31Z | [legacy] | — | — | [docs/project_review_681-669.md](../project_review_681-669.md) | report:docs/project_review_681-669.md (operator-confirmed) |
| #679 | Split Engine.Scripting.Lua.API.World into focused submodules | 2026-07-09T18:26:48Z | [legacy] | — | — | [docs/project_review_681-669.md](../project_review_681-669.md) | report:docs/project_review_681-669.md (operator-confirmed) |
| #678 | [tech-debt] Split Engine.Scripting.Lua.API.WorldQuery into submodules | 2026-07-09T18:15:07Z | [legacy] | — | — | [docs/project_review_681-669.md](../project_review_681-669.md) | report:docs/project_review_681-669.md (operator-confirmed) |
| #677 | [tech-debt] Split Engine.Scripting.Lua.API.Equipment into smaller modules | 2026-07-09T18:03:24Z | [legacy] | — | — | [docs/project_review_681-669.md](../project_review_681-669.md) | report:docs/project_review_681-669.md (operator-confirmed) |
| #676 | [tech-debt] Update CLAUDE.md tilling section + Wounds/Tick restMult comment | 2026-07-09T17:44:19Z | [legacy] | — | — | [docs/project_review_681-669.md](../project_review_681-669.md) | report:docs/project_review_681-669.md (operator-confirmed) |
| #675 | [tech-debt] Split World.Geology.Timeline into smaller modules | 2026-07-09T16:51:37Z | [legacy] | — | — | [docs/project_review_681-669.md](../project_review_681-669.md) | report:docs/project_review_681-669.md (operator-confirmed) |
| #674 | Split World.Thread.Command.Save into smaller modules | 2026-07-09T16:39:54Z | [legacy] | — | — | [docs/project_review_681-669.md](../project_review_681-669.md) | report:docs/project_review_681-669.md (operator-confirmed) |
| #673 | CI: bake GHC toolchain into the CI image (ci-v2); probe gate --jobs 2 | 2026-07-09T16:12:28Z | [legacy] | — | — | [docs/project_review_681-669.md](../project_review_681-669.md) | report:docs/project_review_681-669.md (operator-confirmed) |
| #672 | F3 — UI widget introspection oracle (ui.dumpWidgets) | 2026-07-09T15:45:07Z | [legacy] | — | — | [docs/project_review_681-669.md](../project_review_681-669.md) | report:docs/project_review_681-669.md (operator-confirmed) |
| #670 | Support runtime resource loading outside the repo working directory | 2026-07-09T15:30:20Z | [legacy] | — | — | [docs/project_review_681-669.md](../project_review_681-669.md) | report:docs/project_review_681-669.md (operator-confirmed) |
| #669 | Split Engine.Scripting.Lua.API.Units into focused submodules | 2026-07-09T15:13:55Z | [legacy] | — | — | [docs/project_review_681-669.md](../project_review_681-669.md) | report:docs/project_review_681-669.md (operator-confirmed) |
| #668 | [tech-debt] Split ui_manager.lua into UI lifecycle and event routing modules | 2026-07-09T14:59:07Z | [legacy] | — | — | [docs/project_review_668-655.md](../project_review_668-655.md) | report:docs/project_review_668-655.md (operator-confirmed) |
| #667 | [tech-debt] Split Engine.Scripting.Lua.API.Buildings into smaller modules | 2026-07-09T14:41:52Z | [legacy] | — | — | [docs/project_review_668-655.md](../project_review_668-655.md) | report:docs/project_review_668-655.md (operator-confirmed) |
| #666 | Make Cabal metadata and source distribution checks pass | 2026-07-09T14:16:17Z | [legacy] | — | — | [docs/project_review_668-655.md](../project_review_668-655.md) | report:docs/project_review_668-655.md (operator-confirmed) |
| #663 | --preview Phase 1: boot skeleton — CLI dispatch + minimal graphical exec path | 2026-07-09T08:10:26Z | [legacy] | — | — | [docs/project_review_668-655.md](../project_review_668-655.md) | report:docs/project_review_668-655.md (operator-confirmed) |
| #662 | Split scripts/init.lua into lifecycle and gameplay input routers | 2026-07-09T07:40:08Z | [legacy] | — | — | [docs/project_review_668-655.md](../project_review_668-655.md) | report:docs/project_review_668-655.md (operator-confirmed) |
| #661 | Separate versioned config defaults from local runtime config state | 2026-07-09T07:12:06Z | [legacy] | — | — | [docs/project_review_668-655.md](../project_review_668-655.md) | report:docs/project_review_668-655.md (operator-confirmed) |
| #660 | [tech-debt] Split Engine.Scripting.Lua.API registration into smaller modules | 2026-07-09T02:15:58Z | [legacy] | — | — | [docs/project_review_668-655.md](../project_review_668-655.md) | report:docs/project_review_668-655.md (operator-confirmed) |
| #659 | Give bear_brown a dawn-centered (nocturnal) circadian curve | 2026-07-09T01:50:30Z | [legacy] | — | — | [docs/project_review_668-655.md](../project_review_668-655.md) | report:docs/project_review_668-655.md (operator-confirmed) |
| #658 | Fix decodeUtf8 crash in Lua text API + byte-unsafe truncation trigger | 2026-07-09T01:05:13Z | [legacy] | — | — | [docs/project_review_668-655.md](../project_review_668-655.md) | report:docs/project_review_668-655.md (operator-confirmed) |
| #657 | [tech-debt] Split UI.Manager into smaller modules | 2026-07-09T00:39:43Z | [legacy] | — | — | [docs/project_review_668-655.md](../project_review_668-655.md) | report:docs/project_review_668-655.md (operator-confirmed) |
| #656 | Split Engine.Input.Thread into smaller modules | 2026-07-09T00:14:13Z | [legacy] | — | — | [docs/project_review_668-655.md](../project_review_668-655.md) | report:docs/project_review_668-655.md (operator-confirmed) |
| #655 | Split World.Thread.Command.Edit into focused submodules | 2026-07-08T23:48:44Z | [legacy] | — | — | [docs/project_review_668-655.md](../project_review_668-655.md) | report:docs/project_review_668-655.md (operator-confirmed) |
| #654 | Split World.Generate.Config into smaller modules | 2026-07-08T23:23:11Z | [legacy] | — | — | [docs/project_review_654-628.md](../project_review_654-628.md) | report:docs/project_review_654-628.md (operator-confirmed) |
| #653 | Split UI.Tooltip into smaller modules | 2026-07-08T23:02:39Z | [legacy] | — | — | [docs/project_review_654-628.md](../project_review_654-628.md) | report:docs/project_review_654-628.md (operator-confirmed) |
| #651 | Fix CI: revert GHCUP_INSTALL_BASE_PREFIX (breaks GHC version selection) | 2026-07-08T20:50:56Z | [legacy] | — | — | [docs/project_review_654-628.md](../project_review_654-628.md) | report:docs/project_review_654-628.md (operator-confirmed) |
| #640 | Add Sleeping pose and go_to_sleep AI goal (#612) | 2026-07-08T21:05:53Z | [legacy] | — | — | [docs/project_review_654-628.md](../project_review_654-628.md) | report:docs/project_review_654-628.md (operator-confirmed) |
| #639 | Tilling: dedicated push animation (#517) | 2026-07-08T20:53:04Z | [legacy] | — | — | [docs/project_review_654-628.md](../project_review_654-628.md) | report:docs/project_review_654-628.md (operator-confirmed) |
| #637 | Split unit_info_v2.lua into smaller unit-info panel modules | 2026-07-08T17:16:23Z | [legacy] | — | — | [docs/project_review_654-628.md](../project_review_654-628.md) | report:docs/project_review_654-628.md (operator-confirmed) |
| #634 | [tech-debt] Split World.Geology.Coastal into smaller modules | 2026-07-08T16:50:49Z | [legacy] | — | — | [docs/project_review_654-628.md](../project_review_654-628.md) | report:docs/project_review_654-628.md (operator-confirmed) |
| #633 | Split Combat.Wounds into smaller modules | 2026-07-08T15:59:49Z | [legacy] | — | — | [docs/project_review_654-628.md](../project_review_654-628.md) | report:docs/project_review_654-628.md (operator-confirmed) |
| #631 | [circadian] Sleep pressure + circadian urge signal | 2026-07-08T15:34:16Z | [legacy] | — | — | [docs/project_review_654-628.md](../project_review_654-628.md) | report:docs/project_review_654-628.md (operator-confirmed) |
| #630 | Add Strict/StrictData to Engine.Scripting.Lua.Types | 2026-07-08T15:08:43Z | [legacy] | — | — | [docs/project_review_654-628.md](../project_review_654-628.md) | report:docs/project_review_654-628.md (operator-confirmed) |
| #629 | Split Engine.Graphics.Font.Load into smaller modules | 2026-07-08T14:50:11Z | [legacy] | — | — | [docs/project_review_654-628.md](../project_review_654-628.md) | report:docs/project_review_654-628.md (operator-confirmed) |
| #628 | [blood] Spawn impact blood from new wounds | 2026-07-08T09:09:24Z | [legacy] | — | — | [docs/project_review_654-628.md](../project_review_654-628.md) | report:docs/project_review_654-628.md (operator-confirmed) |
| #627 | [tech-debt] Split World.Slope into smaller modules | 2026-07-08T08:03:47Z | [legacy] | — | — | [docs/project_review_627-614.md](../project_review_627-614.md) | report:docs/project_review_627-614.md (operator-confirmed) |
| #626 | [blood] Generate and render procedural blood decal textures | 2026-07-08T07:38:15Z | [legacy] | — | — | [docs/project_review_627-614.md](../project_review_627-614.md) | report:docs/project_review_627-614.md (operator-confirmed) |
| #625 | Split Unit.Thread.Movement into smaller modules | 2026-07-08T07:12:46Z | [legacy] | — | — | [docs/project_review_627-614.md](../project_review_627-614.md) | report:docs/project_review_627-614.md (operator-confirmed) |
| #624 | Split unit_ai.lua into smaller AI behavior modules | 2026-07-08T06:47:15Z | [legacy] | — | — | [docs/project_review_627-614.md](../project_review_627-614.md) | report:docs/project_review_627-614.md (operator-confirmed) |
| #623 | Split Unit.Thread.Command into smaller modules | 2026-07-08T06:21:43Z | [legacy] | — | — | [docs/project_review_627-614.md](../project_review_627-614.md) | report:docs/project_review_627-614.md (operator-confirmed) |
| #621 | Add blood decal model and debug surface | 2026-07-08T05:56:16Z | [legacy] | — | — | [docs/project_review_627-614.md](../project_review_627-614.md) | report:docs/project_review_627-614.md (operator-confirmed) |
| #620 | Calibrate scroll-to-zoom by scroll amount, not callback count | 2026-07-08T05:25:49Z | [legacy] | — | — | [docs/project_review_627-614.md](../project_review_627-614.md) | report:docs/project_review_627-614.md (operator-confirmed) |
| #619 | Split Engine.Core.Log into Types/Env/Format submodules | 2026-07-08T05:05:19Z | [legacy] | — | — | [docs/project_review_627-614.md](../project_review_627-614.md) | report:docs/project_review_627-614.md (operator-confirmed) |
| #617 | Split app/Main.hs into boot and dump modules | 2026-07-08T04:40:06Z | [legacy] | — | — | [docs/project_review_627-614.md](../project_review_627-614.md) | report:docs/project_review_627-614.md (operator-confirmed) |
| #616 | Add exhaustion meter: exertion-driven fatigue, endurance-scaled recovery | 2026-07-08T04:19:35Z | [legacy] | — | — | [docs/project_review_627-614.md](../project_review_627-614.md) | report:docs/project_review_627-614.md (operator-confirmed) |
| #615 | Split unit_resources.lua into smaller physiology modules | 2026-07-08T03:50:29Z | [legacy] | — | — | [docs/project_review_627-614.md](../project_review_627-614.md) | report:docs/project_review_627-614.md (operator-confirmed) |
| #614 | Split scripts/debug.lua into overlay mode modules | 2026-07-08T03:10:14Z | [legacy] | — | — | [docs/project_review_627-614.md](../project_review_627-614.md) | report:docs/project_review_627-614.md (operator-confirmed) |
| #609 | Wire the furnace into the power grid; add machine shop content | 2026-07-08T02:24:58Z | [legacy] | — | — | [docs/project_review_609-535.md](../project_review_609-535.md) | report:docs/project_review_609-535.md (operator-confirmed) |
| #608 | Make craft recipe power draw job-dependent | 2026-07-08T01:08:42Z | [legacy] | — | — | [docs/project_review_609-535.md](../project_review_609-535.md) | report:docs/project_review_609-535.md (operator-confirmed) |
| #605 | Add furnace default/construction/destruction textures | 2026-07-07T19:22:46Z | [legacy] | — | — | [docs/project_review_609-535.md](../project_review_609-535.md) | report:docs/project_review_609-535.md (operator-confirmed) |
| #602 | Stabilize medic_coord_probe and promote it to CI eligibility (#589) | 2026-07-07T19:07:30Z | [legacy] | — | — | [docs/project_review_609-535.md](../project_review_609-535.md) | report:docs/project_review_609-535.md (operator-confirmed) |
| #601 | Stabilize disarm_probe and promote it to CI eligibility | 2026-07-07T18:33:17Z | [legacy] | — | — | [docs/project_review_609-535.md](../project_review_609-535.md) | report:docs/project_review_609-535.md (operator-confirmed) |
| #600 | Make infection_probe self-contained and CI-eligible | 2026-07-07T16:35:48Z | [legacy] | — | — | [docs/project_review_609-535.md](../project_review_609-535.md) | report:docs/project_review_609-535.md (operator-confirmed) |
| #598 | Split Engine.Scripting.Lua.API.Craft into Recipe/Execute/Bill submodules | 2026-07-07T15:56:53Z | [legacy] | — | — | [docs/project_review_609-535.md](../project_review_609-535.md) | report:docs/project_review_609-535.md (operator-confirmed) |
| #597 | Add whetstone texture | 2026-07-07T15:19:31Z | [legacy] | — | — | [docs/project_review_609-535.md](../project_review_609-535.md) | report:docs/project_review_609-535.md (operator-confirmed) |
| #595 | Show behavior-probe CI eligibility from the tooling | 2026-07-07T15:06:58Z | [legacy] | — | — | [docs/project_review_609-535.md](../project_review_609-535.md) | report:docs/project_review_609-535.md (operator-confirmed) |
| #594 | [tooling] Refresh tools/README.md for current probe runner behavior | 2026-07-07T14:43:10Z | [legacy] | — | — | [docs/project_review_609-535.md](../project_review_609-535.md) | report:docs/project_review_609-535.md (operator-confirmed) |
| #536 | Parallel probe dispatch via run_probes.py --jobs (rescoped #531) | 2026-07-07T13:38:39Z | [legacy] | — | — | [docs/project_review_609-535.md](../project_review_609-535.md) | report:docs/project_review_609-535.md (operator-confirmed) |
| #535 | Gate features in CI: path-selective, blocking behavior-probe job (#530) | 2026-07-07T03:59:35Z | [legacy] | — | — | [docs/project_review_609-535.md](../project_review_609-535.md) | report:docs/project_review_609-535.md (operator-confirmed) |
| #534 | Extract shared probe harness (probelib) and migrate all probes (#529) | 2026-07-07T00:07:57Z | [legacy] | — | — | [docs/project_review_534-518.md](../project_review_534-518.md) | report:docs/project_review_534-518.md (operator-confirmed) |
| #533 | Add `make ci` local pre-push gate mirroring CI (#527) | 2026-07-06T18:10:31Z | [legacy] | — | — | [docs/project_review_534-518.md](../project_review_534-518.md) | report:docs/project_review_534-518.md (operator-confirmed) |
| #532 | CI: incremental builds via dist-newstyle + toolchain caching (#526) | 2026-07-06T17:04:49Z | [legacy] | — | — | [docs/project_review_534-518.md](../project_review_534-518.md) | report:docs/project_review_534-518.md (operator-confirmed) |
| #528 | Consumable drink effects scaled by quality + temperature (#347) | 2026-07-06T17:40:40Z | [legacy] | — | — | [docs/project_review_534-518.md](../project_review_534-518.md) | report:docs/project_review_534-518.md (operator-confirmed) |
| #525 | Glacier evolution: remove dead retreat/melt terrain branches, fix stale TODOs | 2026-07-06T16:21:21Z | [legacy] | — | — | [docs/project_review_534-518.md](../project_review_534-518.md) | report:docs/project_review_534-518.md (operator-confirmed) |
| #524 | Thought system: periodic per-unit thoughts driven by mood/pain/environment | 2026-07-06T16:05:27Z | [legacy] | — | — | [docs/project_review_534-518.md](../project_review_534-518.md) | report:docs/project_review_534-518.md (operator-confirmed) |
| #523 | Unify consciousness + mood into a state-of-mind model (#350) | 2026-07-06T14:44:21Z | [legacy] | — | — | [docs/project_review_534-518.md](../project_review_534-518.md) | report:docs/project_review_534-518.md (operator-confirmed) |
| #522 | Make locomotion injury penalty data-driven (#393) | 2026-07-06T05:30:27Z | [legacy] | — | — | [docs/project_review_534-518.md](../project_review_534-518.md) | report:docs/project_review_534-518.md (operator-confirmed) |
| #521 | Add farm AI: plant + skill-gated auto-harvest + rot (#336) | 2026-07-06T03:32:25Z | [legacy] | — | — | [docs/project_review_534-518.md](../project_review_534-518.md) | report:docs/project_review_534-518.md (operator-confirmed) |
| #520 | Remove dead scsGenFluid field (unused save-diff baseline) | 2026-07-06T00:23:39Z | [legacy] | — | — | [docs/project_review_534-518.md](../project_review_534-518.md) | report:docs/project_review_534-518.md (operator-confirmed) |
| #519 | Add planting tool + suitability screen (#335) | 2026-07-05T23:31:45Z | [legacy] | — | — | [docs/project_review_534-518.md](../project_review_534-518.md) | report:docs/project_review_534-518.md (operator-confirmed) |
| #518 | Add crop content + two growth forms (#334) | 2026-07-05T20:53:04Z | [legacy] | — | — | [docs/project_review_534-518.md](../project_review_534-518.md) | report:docs/project_review_534-518.md (operator-confirmed) |
| #516 | Add tilling designation tool + AI mechanism (partial #333) | 2026-07-05T18:29:27Z | [legacy] | — | — | [docs/project_review_516-505.md](../project_review_516-505.md) | report:docs/project_review_516-505.md (operator-confirmed) |
| #515 | [power] Powered workshops + consumers (requires_power + drain) | 2026-07-05T13:41:39Z | [legacy] | — | — | [docs/project_review_516-505.md](../project_review_516-505.md) | report:docs/project_review_516-505.md (operator-confirmed) |
| #514 | Power-network simulation: connected components + energy balance (#360) | 2026-07-05T00:03:34Z | [legacy] | — | — | [docs/project_review_516-505.md](../project_review_516-505.md) | report:docs/project_review_516-505.md (operator-confirmed) |
| #513 | Add power-grid wire structure piece + connection-aware autotile (#359) | 2026-07-04T21:41:16Z | [legacy] | — | — | [docs/project_review_516-505.md](../project_review_516-505.md) | report:docs/project_review_516-505.md (operator-confirmed) |
| #512 | Longitude-local day/night: solar time varies around the world cylinder | 2026-07-04T18:35:24Z | [legacy] | — | — | [docs/project_review_516-505.md](../project_review_516-505.md) | report:docs/project_review_516-505.md (operator-confirmed) |
| #511 | Repair UI: player priority flag + condition/sharpness surfacing (#303) | 2026-07-04T13:37:18Z | [legacy] | — | — | [docs/project_review_516-505.md](../project_review_516-505.md) | report:docs/project_review_516-505.md (operator-confirmed) |
| #510 | Kitchen workshop + cooking skill/knowledge + coffee recipe (#346) | 2026-07-04T05:48:07Z | [legacy] | — | — | [docs/project_review_516-505.md](../project_review_516-505.md) | report:docs/project_review_516-505.md (operator-confirmed) |
| #509 | Animate the unknown-unit fallback with idle/walk cycles (#485) | 2026-07-04T05:13:50Z | [legacy] | — | — | [docs/project_review_516-505.md](../project_review_516-505.md) | report:docs/project_review_516-505.md (operator-confirmed) |
| #508 | Fix #500: share plate-base terrain across chunk-window borders | 2026-07-04T05:00:38Z | [legacy] | — | — | [docs/project_review_516-505.md](../project_review_516-505.md) | report:docs/project_review_516-505.md (operator-confirmed) |
| #507 | Crafting UI: bill/order queue + station panel (#330) | 2026-07-04T04:33:33Z | [legacy] | — | — | [docs/project_review_516-505.md](../project_review_516-505.md) | report:docs/project_review_516-505.md (operator-confirmed) |
| #506 | Power items + placeable solar panel & battery (#358) | 2026-07-04T03:36:14Z | [legacy] | — | — | [docs/project_review_516-505.md](../project_review_516-505.md) | report:docs/project_review_516-505.md (operator-confirmed) |
| #505 | Fold construction designation into the build tool (#403) | 2026-07-04T03:17:14Z | [legacy] | — | — | [docs/project_review_516-505.md](../project_review_516-505.md) | report:docs/project_review_516-505.md (operator-confirmed) |
| #504 | #302 Repair AI: utility + designation + go-to-station-and-repair | 2026-07-04T03:03:28Z | [legacy] | — | — | [docs/project_review_504-492.md](../project_review_504-492.md) | report:docs/project_review_504-492.md (operator-confirmed) |
| #503 | Quality tiers + tooltip descriptions (#345) | 2026-07-04T01:44:03Z | [legacy] | — | — | [docs/project_review_504-492.md](../project_review_504-492.md) | report:docs/project_review_504-492.md (operator-confirmed) |
| #502 | Profile the worldgen setup/timeline phase and document findings | 2026-07-04T01:25:27Z | [legacy] | — | — | [docs/project_review_504-492.md](../project_review_504-492.md) | report:docs/project_review_504-492.md (operator-confirmed) |
| #501 | Fabrication tier: steel_bar -> tools, weapons, construction stock (#328) | 2026-07-04T01:12:35Z | [legacy] | — | — | [docs/project_review_504-492.md](../project_review_504-492.md) | report:docs/project_review_504-492.md (operator-confirmed) |
| #499 | Craft AI + bill backend: per-station orders drive production end to end (#329) | 2026-07-04T01:02:15Z | [legacy] | — | — | [docs/project_review_504-492.md](../project_review_504-492.md) | report:docs/project_review_504-492.md (operator-confirmed) |
| #498 | Make uphill travel cost move speed and stamina, not only routing weight (#375) | 2026-07-03T23:25:08Z | [legacy] | — | — | [docs/project_review_504-492.md](../project_review_504-492.md) | report:docs/project_review_504-492.md (operator-confirmed) |
| #497 | Fix location stamp idempotency after anchor floor is cleared (#424) | 2026-07-03T22:59:18Z | [legacy] | — | — | [docs/project_review_504-492.md](../project_review_504-492.md) | report:docs/project_review_504-492.md (operator-confirmed) |
| #496 | Remove stale TODO on the severed-wound icon (#372) | 2026-07-03T22:46:24Z | [legacy] | — | — | [docs/project_review_504-492.md](../project_review_504-492.md) | report:docs/project_review_504-492.md (operator-confirmed) |
| #495 | Make the front-wall vegetation lift seam-aware (#423) | 2026-07-03T22:15:34Z | [legacy] | — | — | [docs/project_review_504-492.md](../project_review_504-492.md) | report:docs/project_review_504-492.md (operator-confirmed) |
| #494 | Unify duplicated magic constants (blood ratio, UI layer, max_stamina, climb height, deco base, aware range) | 2026-07-03T21:07:51Z | [legacy] | — | — | [docs/project_review_504-492.md](../project_review_504-492.md) | report:docs/project_review_504-492.md (operator-confirmed) |
| #493 | Enforce location min spacing across the cylindrical U seam | 2026-07-03T18:55:08Z | [legacy] | — | — | [docs/project_review_504-492.md](../project_review_504-492.md) | report:docs/project_review_504-492.md (operator-confirmed) |
| #492 | Harden decodeUtf8 trust boundaries | 2026-07-03T18:42:06Z | [legacy] | — | — | [docs/project_review_504-492.md](../project_review_504-492.md) | report:docs/project_review_504-492.md (operator-confirmed) |
| #491 | Strictness uniformity: modifyTVar' + Data.Map.Strict on long-lived state | 2026-07-03T18:28:42Z | [legacy] | — | — | [docs/project_review_491-475.md](../project_review_491-475.md) | report:docs/project_review_491-475.md (operator-confirmed) |
| #490 | Document headless behavior probes + add opt-in aggregate runner | 2026-07-03T17:36:42Z | [legacy] | — | — | [docs/project_review_491-475.md](../project_review_491-475.md) | report:docs/project_review_491-475.md (operator-confirmed) |
| #488 | Per-subset unknown-texture fallback + audit tool (#478) | 2026-07-03T17:59:03Z | [legacy] | — | — | [docs/project_review_491-475.md](../project_review_491-475.md) | report:docs/project_review_491-475.md (operator-confirmed) |
| #487 | Replace unsafePerformIO fresh-buffer idiom with unsafeCreate / runST | 2026-07-03T17:11:06Z | [legacy] | — | — | [docs/project_review_491-475.md](../project_review_491-475.md) | report:docs/project_review_491-475.md (operator-confirmed) |
| #486 | [cbits] Harden lua_debug.c reentrancy trap + font_stb.c size checks | 2026-07-03T16:47:11Z | [legacy] | — | — | [docs/project_review_491-475.md](../project_review_491-475.md) | report:docs/project_review_491-475.md (operator-confirmed) |
| #484 | Refresh README to describe Synarchy as a full game | 2026-07-03T16:32:52Z | [legacy] | — | — | [docs/project_review_491-475.md](../project_review_491-475.md) | report:docs/project_review_491-475.md (operator-confirmed) |
| #482 | #301 Repair model + stations/items (condition/sharpness as station-gated recipes) | 2026-07-03T16:17:48Z | [legacy] | — | — | [docs/project_review_491-475.md](../project_review_491-475.md) | report:docs/project_review_491-475.md (operator-confirmed) |
| #481 | [tech-debt] Name UI/Sim magic offsets + operator/logic nits | 2026-07-03T15:52:27Z | [legacy] | — | — | [docs/project_review_491-475.md](../project_review_491-475.md) | report:docs/project_review_491-475.md (operator-confirmed) |
| #480 | Wire night perception into combat awareness (#315) | 2026-07-03T15:47:53Z | [legacy] | — | — | [docs/project_review_491-475.md](../project_review_491-475.md) | report:docs/project_review_491-475.md (operator-confirmed) |
| #477 | #471 Replace cabal jsem semaphore with per-package ghc -j (concurrent worktree builds can't deadlock) | 2026-07-03T15:09:21Z | [legacy] | — | — | [docs/project_review_491-475.md](../project_review_491-475.md) | report:docs/project_review_491-475.md (operator-confirmed) |
| #476 | #441 Drop fromJust from the UPrelude re-export list | 2026-07-03T14:49:45Z | [legacy] | — | — | [docs/project_review_491-475.md](../project_review_491-475.md) | report:docs/project_review_491-475.md (operator-confirmed) |
| #475 | Purge stale phase/skeleton/slice milestone comments | 2026-07-03T14:48:43Z | [legacy] | — | — | [docs/project_review_491-475.md](../project_review_491-475.md) | report:docs/project_review_491-475.md (operator-confirmed) |
| #474 | #265 Derived unit roles: emergent skill-derived labels weight work selection | 2026-07-03T14:27:59Z | [legacy] | — | — | [docs/project_review_474-461.md](../project_review_474-461.md) | report:docs/project_review_474-461.md (operator-confirmed) |
| #473 | #223 River/lake bed depth: canyon rivers + graben rift lakes | 2026-07-03T13:54:20Z | [legacy] | — | — | [docs/project_review_474-461.md](../project_review_474-461.md) | report:docs/project_review_474-461.md (operator-confirmed) |
| #472 | #220 Coastline variety: tectonic steepness field drives coast profiles (PR 1 of 2) | 2026-07-03T03:21:45Z | [legacy] | — | — | [docs/project_review_474-461.md](../project_review_474-461.md) | report:docs/project_review_474-461.md (operator-confirmed) |
| #470 | #327 Smelting tier: ore + fuel → metal bars at the furnace | 2026-07-03T02:44:17Z | [legacy] | — | — | [docs/project_review_474-461.md](../project_review_474-461.md) | report:docs/project_review_474-461.md (operator-confirmed) |
| #469 | #392 Item buffs gain a percent axis (ibPercent → smPercent) | 2026-07-03T01:19:43Z | [legacy] | — | — | [docs/project_review_474-461.md](../project_review_474-461.md) | report:docs/project_review_474-461.md (operator-confirmed) |
| #468 | #156 shared view-transition teardown registry | 2026-07-02T22:44:26Z | [legacy] | — | — | [docs/project_review_474-461.md](../project_review_474-461.md) | report:docs/project_review_474-461.md (operator-confirmed) |
| #467 | #343 Crafted-output quality from crafter skill + knowledge-gated recipes | 2026-07-02T22:42:54Z | [legacy] | — | — | [docs/project_review_474-461.md](../project_review_474-461.md) | report:docs/project_review_474-461.md (operator-confirmed) |
| #465 | #326 Work stations: furnace + workbench with operations, findStation, craft.executeAt | 2026-07-02T20:39:11Z | [legacy] | — | — | [docs/project_review_474-461.md](../project_review_474-461.md) | report:docs/project_review_474-461.md (operator-confirmed) |
| #464 | #344: item temperature + cooling to ambient (iiTemp, save v68) | 2026-07-02T20:38:04Z | [legacy] | — | — | [docs/project_review_474-461.md](../project_review_474-461.md) | report:docs/project_review_474-461.md (operator-confirmed) |
| #463 | #332: flora growth runtime — derived age/phase/season on an advancing calendar | 2026-07-02T20:08:01Z | [legacy] | — | — | [docs/project_review_474-461.md](../project_review_474-461.md) | report:docs/project_review_474-461.md (operator-confirmed) |
| #462 | #325: crafting recipe data model + craft.* Lua API | 2026-07-02T18:59:49Z | [legacy] | — | — | [docs/project_review_474-461.md](../project_review_474-461.md) | report:docs/project_review_474-461.md (operator-confirmed) |
| #461 | #97: wood resource — chop designation + tree felling AI + wood_log | 2026-07-02T15:21:43Z | [legacy] | — | — | [docs/project_review_474-461.md](../project_review_474-461.md) | report:docs/project_review_474-461.md (operator-confirmed) |
| #460 | #94: foraging AI + interactive flora backend | 2026-07-02T13:19:51Z | [legacy] | — | — | [docs/project_review_459-450.md](../project_review_459-450.md) | report:docs/project_review_459-450.md (operator-confirmed) |
| #459 | #96: build job AI — acolytes execute construction designations | 2026-07-02T13:46:37Z | [legacy] | — | — | [docs/project_review_459-450.md](../project_review_459-450.md) | report:docs/project_review_459-450.md (operator-confirmed) |
| #458 | #93: two-layer hunger/digestion model + bulk food (quinoa sack) | 2026-07-02T04:08:13Z | [legacy] | — | — | [docs/project_review_459-450.md](../project_review_459-450.md) | report:docs/project_review_459-450.md (operator-confirmed) |
| #457 | #91: Ruins location type — structure tile variants + room_small_damaged | 2026-07-02T03:54:42Z | [legacy] | — | — | [docs/project_review_459-450.md](../project_review_459-450.md) | report:docs/project_review_459-450.md (operator-confirmed) |
| #456 | #434: init.lua — kill location_stamper + item_info_panel on shutdown; declare all script-id locals | 2026-07-02T03:04:07Z | [legacy] | — | — | [docs/project_review_459-450.md](../project_review_459-450.md) | report:docs/project_review_459-450.md (operator-confirmed) |
| #455 | #433: legacy fullscreen fallback uses .:? — missing key no longer resets the video config | 2026-07-02T03:00:41Z | [legacy] | — | — | [docs/project_review_459-450.md](../project_review_459-450.md) | report:docs/project_review_459-450.md (operator-confirmed) |
| #454 | #365: Arena pages survive save/load — rebuild via the flat builder instead of the generator | 2026-07-02T02:39:16Z | [legacy] | — | — | [docs/project_review_459-450.md](../project_review_459-450.md) | report:docs/project_review_459-450.md (operator-confirmed) |
| #453 | Pan margin + parallel chunk build for quad-cache rebuilds (#447) | 2026-07-01T23:51:57Z | [legacy] | — | — | [docs/project_review_459-450.md](../project_review_459-450.md) | report:docs/project_review_459-450.md (operator-confirmed) |
| #452 | Add ntfy.sh push notifications for PR lifecycle | 2026-07-01T23:43:02Z | [legacy] | — | — | [docs/project_review_459-450.md](../project_review_459-450.md) | report:docs/project_review_459-450.md (operator-confirmed) |
| #451 | Cache sorted per-layer world batches; frame loop linear-merges (#446) | 2026-07-01T23:28:43Z | [legacy] | — | — | [docs/project_review_459-450.md](../project_review_459-450.md) | report:docs/project_review_459-450.md (operator-confirmed) |
| #450 | Storable vectors for sprite-batch vertices (#445) | 2026-07-01T23:08:48Z | [legacy] | — | — | [docs/project_review_459-450.md](../project_review_459-450.md) | report:docs/project_review_459-450.md (operator-confirmed) |
| #449 | #436: CI — Linux build + headless test gate; track world_check baselines (#421) | 2026-07-02T00:33:39Z | [legacy] | — | — | [docs/project_review_459-450.md](../project_review_459-450.md) | report:docs/project_review_459-450.md (operator-confirmed) |
| #444 | Parallelize module compilation + unlock RTS tuning (#443) | 2026-07-01T22:17:57Z | [legacy] | — | — | [docs/project_review_432-412.md](../project_review_432-412.md) | report:docs/project_review_432-412.md (operator-confirmed) |
| #442 | #435: Enable -Wall + incomplete-pattern warnings in the production profile | 2026-07-01T22:37:59Z | [legacy] | — | — | [docs/project_review_432-412.md](../project_review_432-412.md) | report:docs/project_review_432-412.md (operator-confirmed) |
| #432 | Gate settings debug logInfo spam behind engine.logDebug | 2026-07-01T23:04:13Z | [legacy] | — | — | [docs/project_review_432-412.md](../project_review_432-412.md) | report:docs/project_review_432-412.md (operator-confirmed) |
| #431 | Location content spawning: units, items, buildings, structures, loot tables (#90) | 2026-07-01T21:08:18Z | [legacy] | — | — | [docs/project_review_432-412.md](../project_review_432-412.md) | report:docs/project_review_432-412.md (operator-confirmed) |
| #430 | Reorganize assets/textures/ by role (#428) | 2026-07-01T19:27:37Z | [legacy] | — | — | [docs/project_review_432-412.md](../project_review_432-412.md) | report:docs/project_review_432-412.md (operator-confirmed) |
| #429 | [docs] Fix stale facts in CLAUDE.md and tools/README.md | 2026-07-01T18:13:16Z | [legacy] | — | — | [docs/project_review_432-412.md](../project_review_432-412.md) | report:docs/project_review_432-412.md (operator-confirmed) |
| #425 | [docs] Fix misnumbered algorithm-walkthrough comments | 2026-07-01T16:53:32Z | [legacy] | — | — | [docs/project_review_432-412.md](../project_review_432-412.md) | report:docs/project_review_432-412.md (operator-confirmed) |
| #420 | [docs] Fix stale `.claude/scripts` references in CLAUDE.md | 2026-07-01T14:31:06Z | [legacy] | — | — | [docs/project_review_432-412.md](../project_review_432-412.md) | report:docs/project_review_432-412.md (operator-confirmed) |
| #419 | [locations][render] lift flora/veg in front of a structure wall over the whole wall (#418) | 2026-07-01T04:08:07Z | [legacy] | — | — | [docs/project_review_432-412.md](../project_review_432-412.md) | report:docs/project_review_432-412.md (operator-confirmed) |
| #417 | [locations][render] z-aware front-wall sort so a sunken room's rim occludes its walls (#415) | 2026-07-01T00:34:50Z | [legacy] | — | — | [docs/project_review_432-412.md](../project_review_432-412.md) | report:docs/project_review_432-412.md (operator-confirmed) |
| #416 | Keep location placements clear of water (#414) | 2026-06-30T22:36:04Z | [legacy] | — | — | [docs/project_review_432-412.md](../project_review_432-412.md) | report:docs/project_review_432-412.md (operator-confirmed) |
| #413 | Flatten location room footprint to the lowest ground level | 2026-06-30T19:56:09Z | [legacy] | — | — | [docs/project_review_432-412.md](../project_review_432-412.md) | report:docs/project_review_432-412.md (operator-confirmed) |
| #412 | World-gen location overlay (#89) | 2026-06-30T19:26:55Z | [legacy] | — | — | [docs/project_review_432-412.md](../project_review_432-412.md) | report:docs/project_review_432-412.md (operator-confirmed) |
| #411 | Fix #374: add diagnostic context to bare error partials on worker-thread paths | 2026-06-30T15:49:56Z | [legacy] | — | — | [docs/project_review_411-399.md](../project_review_411-399.md) | report:docs/project_review_411-399.md (operator-confirmed) |
| #410 | Fix #385: worldgen dead-code sweep (BoundarySide, plateAt, foldlM, wrapChunkX/Y, meander seed, dup isRiverCarveEvent) | 2026-06-30T15:43:43Z | [legacy] | — | — | [docs/project_review_411-399.md](../project_review_411-399.md) | report:docs/project_review_411-399.md (operator-confirmed) |
| #409 | Fix #380: correct inaccurate Lua API docstrings | 2026-06-30T15:31:03Z | [legacy] | — | — | [docs/project_review_411-399.md](../project_review_411-399.md) | report:docs/project_review_411-399.md (operator-confirmed) |
| #408 | Fix #387: Lua-bridge + UI dead code sweep | 2026-06-30T15:08:24Z | [legacy] | — | — | [docs/project_review_411-399.md](../project_review_411-399.md) | report:docs/project_review_411-399.md (operator-confirmed) |
| #407 | Fix #396: dedupe broken-equipment overlay into scripts/ui/broken_overlay | 2026-06-30T15:04:05Z | [legacy] | — | — | [docs/project_review_411-399.md](../project_review_411-399.md) | report:docs/project_review_411-399.md (operator-confirmed) |
| #406 | Fix #389: dedup diverged helper copies (resolveTextureH hit-box bug + 3 cleanups) | 2026-06-30T14:42:02Z | [legacy] | — | — | [docs/project_review_411-399.md](../project_review_411-399.md) | report:docs/project_review_411-399.md (operator-confirmed) |
| #405 | Fix #384: remove dead channel-mask pipeline + stale composeFluidMap parameters | 2026-06-30T14:42:05Z | [legacy] | — | — | [docs/project_review_411-399.md](../project_review_411-399.md) | report:docs/project_review_411-399.md (operator-confirmed) |
| #404 | Fix #383: engine dead-export/field sweep | 2026-06-30T14:48:02Z | [legacy] | — | — | [docs/project_review_411-399.md](../project_review_411-399.md) | report:docs/project_review_411-399.md (operator-confirmed) |
| #402 | Fix #382: remove the never-wired Event subsystem | 2026-06-30T14:10:07Z | [legacy] | — | — | [docs/project_review_411-399.md](../project_review_411-399.md) | report:docs/project_review_411-399.md (operator-confirmed) |
| #401 | Fix #376: single shared woundEffSeverity helper (medic targeting, bleed display, injured-anim) | 2026-06-30T15:28:24Z | [legacy] | — | — | [docs/project_review_411-399.md](../project_review_411-399.md) | report:docs/project_review_411-399.md (operator-confirmed) |
| #400 | #386: Remove vestigial usFallImpact / computeFallImpact scalar | 2026-06-30T14:05:21Z | [legacy] | — | — | [docs/project_review_411-399.md](../project_review_411-399.md) | report:docs/project_review_411-399.md (operator-confirmed) |
| #399 | Fix #377: unit.list() scopes to the active world page | 2026-06-30T13:57:35Z | [legacy] | — | — | [docs/project_review_411-399.md](../project_review_411-399.md) | report:docs/project_review_411-399.md (operator-confirmed) |
| #398 | Fix #378: correct misnamed colors in Math.colorToVec4 (opaque, green/lime) | 2026-06-30T13:56:56Z | [legacy] | — | — | [docs/project_review_398-348.md](../project_review_398-348.md) | report:docs/project_review_398-348.md (operator-confirmed) |
| #368 | Fix #367: tile select honours the picked z, not the column surface | 2026-06-30T03:27:13Z | [legacy] | — | — | [docs/project_review_398-348.md](../project_review_398-348.md) | report:docs/project_review_398-348.md (operator-confirmed) |
| #366 | Add #219 headless multi-world save/load regression test | 2026-06-30T02:10:33Z | [legacy] | — | — | [docs/project_review_398-348.md](../project_review_398-348.md) | report:docs/project_review_398-348.md (operator-confirmed) |
| #364 | Fix #300: unit.repairItem engine primitive + Lua API | 2026-06-30T02:41:15Z | [legacy] | — | — | [docs/project_review_398-348.md](../project_review_398-348.md) | report:docs/project_review_398-348.md (operator-confirmed) |
| #363 | Fix #298: world-thread heap overflow loading glacier-rim chunks | 2026-06-29T22:47:45Z | [legacy] | — | — | [docs/project_review_398-348.md](../project_review_398-348.md) | report:docs/project_review_398-348.md (operator-confirmed) |
| #362 | Fix #312: surface material affects unit movement (sand slower than rock) | 2026-06-30T00:12:48Z | [legacy] | — | — | [docs/project_review_398-348.md](../project_review_398-348.md) | report:docs/project_review_398-348.md (operator-confirmed) |
| #356 | Fix #306: re-derive AI utility ladder against FOLLOW_COMMAND_UTILITY=7.0 | 2026-06-29T21:38:03Z | [legacy] | — | — | [docs/project_review_398-348.md](../project_review_398-348.md) | report:docs/project_review_398-348.md (operator-confirmed) |
| #355 | Fix #337: give exposed rock jagged slopes directional variety | 2026-06-30T00:12:17Z | [legacy] | — | — | [docs/project_review_398-348.md](../project_review_398-348.md) | report:docs/project_review_398-348.md (operator-confirmed) |
| #354 | #305: movement speed accounts for encumbrance (carried load ÷ capacity), eased by endurance | 2026-06-29T18:37:10Z | [legacy] | — | — | [docs/project_review_398-348.md](../project_review_398-348.md) | report:docs/project_review_398-348.md (operator-confirmed) |
| #348 | Fix #313: rename dangling item sprites (antibiotics, raw_fluorite) | 2026-06-29T17:51:09Z | [legacy] | — | — | [docs/project_review_398-348.md](../project_review_398-348.md) | report:docs/project_review_398-348.md (operator-confirmed) |
| #341 | Fix #321: round-trip non-finite numbers in the Lua save serializer | 2026-06-29T17:48:08Z | [legacy] | — | — | [docs/project_review_341-296.md](../project_review_341-296.md) | report:docs/project_review_341-296.md (operator-confirmed) |
| #340 | Fix #323: correct building.getStorageWeight docstring | 2026-06-29T17:52:51Z | [legacy] | — | — | [docs/project_review_398-348.md](../project_review_398-348.md) | report:docs/project_review_398-348.md (operator-confirmed) |
| #339 | Fix #317: clamp chemical-erosion intensity to [0,1] | 2026-06-29T17:37:17Z | [legacy] | — | — | [docs/project_review_341-296.md](../project_review_341-296.md) | report:docs/project_review_341-296.md (operator-confirmed) |
| #338 | Dedupe wrapChunkCoordU to a single canonical source (#316) | 2026-06-29T17:51:28Z | [legacy] | — | — | [docs/project_review_398-348.md](../project_review_398-348.md) | report:docs/project_review_398-348.md (operator-confirmed) |
| #322 | Fix #319: debug-console JSON serializer emits valid JSON for inf/nan + control chars | 2026-06-29T16:25:43Z | [legacy] | — | — | [docs/project_review_341-296.md](../project_review_341-296.md) | report:docs/project_review_341-296.md (operator-confirmed) |
| #320 | Fix #304: hysteresis on the collapse↔crawl locomotor boundary | 2026-06-29T16:41:26Z | [legacy] | — | — | [docs/project_review_341-296.md](../project_review_341-296.md) | report:docs/project_review_341-296.md (operator-confirmed) |
| #318 | Fix #308: elevation-correct unit thermo ambient (altitude lapse rate) | 2026-06-29T16:25:58Z | [legacy] | — | — | [docs/project_review_341-296.md](../project_review_341-296.md) | report:docs/project_review_341-296.md (operator-confirmed) |
| #314 | Fix #309: throttle failed building spawns (no per-frame retry/log flood) | 2026-06-29T15:46:57Z | [legacy] | — | — | [docs/project_review_341-296.md](../project_review_341-296.md) | report:docs/project_review_341-296.md (operator-confirmed) |
| #311 | Fix #307: route lunge stamina gate through stats.get(max_stamina) | 2026-06-29T15:21:28Z | [legacy] | — | — | [docs/project_review_341-296.md](../project_review_341-296.md) | report:docs/project_review_341-296.md (operator-confirmed) |
| #310 | Fix #297: clamp camera.gotoTile inside the glacier rim | 2026-06-29T16:41:04Z | [legacy] | — | — | [docs/project_review_341-296.md](../project_review_341-296.md) | report:docs/project_review_341-296.md (operator-confirmed) |
| #296 | Finish #92: activity-scaled hunger/hydration drain + calorie↔thermo/heal coupling + feed API | 2026-06-29T00:52:32Z | [legacy] | — | — | [docs/project_review_341-296.md](../project_review_341-296.md) | report:docs/project_review_341-296.md (operator-confirmed) |
| #295 | Construction designation tool (#95) | 2026-06-29T02:51:32Z | [legacy] | — | — | [docs/project_review_341-296.md](../project_review_341-296.md) | report:docs/project_review_341-296.md (operator-confirmed) |
| #294 | Input settings tab keybind editor; remove System tab (#277) | 2026-06-29T13:21:35Z | [legacy] | — | — | [docs/project_review_341-296.md](../project_review_341-296.md) | report:docs/project_review_341-296.md (operator-confirmed) |
| #293 | All-worlds save: persist & restore every world page (#216/#217/#218) | 2026-06-29T14:25:33Z | [legacy] | — | — | [docs/project_review_341-296.md](../project_review_341-296.md) | report:docs/project_review_341-296.md (operator-confirmed) |
| #292 | Fix #286: GPU handle→slot indirection for the world render cache | 2026-06-28T21:18:30Z | [legacy] | — | — | [docs/project_review_292-281.md](../project_review_292-281.md) | report:docs/project_review_292-281.md (operator-confirmed) |
| #291 | Fix #88: data-driven location definitions | 2026-06-28T21:01:20Z | [legacy] | — | — | [docs/project_review_292-281.md](../project_review_292-281.md) | report:docs/project_review_292-281.md (operator-confirmed) |
| #290 | Fix #275: route camera pan + Q/E + Home through the binding system | 2026-06-28T20:07:30Z | [legacy] | — | — | [docs/project_review_292-281.md](../project_review_292-281.md) | report:docs/project_review_292-281.md (operator-confirmed) |
| #289 | Fix #136: stop unit_info_v2 hiding the shared HUD info panel | 2026-06-28T19:12:36Z | [legacy] | — | — | [docs/project_review_292-281.md](../project_review_292-281.md) | report:docs/project_review_292-281.md (operator-confirmed) |
| #288 | Fix #221: extend river sources inland to their catchment divides | 2026-06-28T19:56:30Z | [legacy] | — | — | [docs/project_review_292-281.md](../project_review_292-281.md) | report:docs/project_review_292-281.md (operator-confirmed) |
| #287 | Restructure SaveData into per-world WorldPageSave + globals (#215) | 2026-06-28T19:33:07Z | [legacy] | — | — | [docs/project_review_292-281.md](../project_review_292-281.md) | report:docs/project_review_292-281.md (operator-confirmed) |
| #285 | Restore phase-3 structural rebind — fix magenta interior regression from #282 | 2026-06-28T17:30:15Z | [legacy] | — | — | [docs/project_review_292-281.md](../project_review_292-281.md) | report:docs/project_review_292-281.md (operator-confirmed) |
| #284 | Fix #224: exposed hard rock slopes & reads as jagged peaks | 2026-06-28T18:54:48Z | [legacy] | — | — | [docs/project_review_292-281.md](../project_review_292-281.md) | report:docs/project_review_292-281.md (operator-confirmed) |
| #283 | [keybinds] C — Keybind Lua API (#276) | 2026-06-28T17:50:51Z | [legacy] | — | — | [docs/project_review_292-281.md](../project_review_292-281.md) | report:docs/project_review_292-281.md (operator-confirmed) |
| #282 | Race-safe, late-texture-robust world render-cache invalidation (#35) | 2026-06-28T17:08:00Z | [legacy] | — | — | [docs/project_review_292-281.md](../project_review_292-281.md) | report:docs/project_review_292-281.md (operator-confirmed) |
| #281 | Remove obsolete structure_test.lua harness (#70) | 2026-06-28T16:48:04Z | [legacy] | — | — | [docs/project_review_292-281.md](../project_review_292-281.md) | report:docs/project_review_292-281.md (operator-confirmed) |
| #280 | Multi-key KeyBindings model + array config format (#274) | 2026-06-28T16:56:41Z | [legacy] | — | — | [docs/project_review_292-281.md](../project_review_292-281.md) | report:docs/project_review_292-281.md (operator-confirmed) |
| #279 | Fix #225: steep mountain faces shed soil to bare rock | 2026-06-28T16:47:21Z | [legacy] | — | — | [docs/project_review_279-261.md](../project_review_279-261.md) | report:docs/project_review_279-261.md (operator-confirmed) |
| #278 | Use YAML portrait assets in unit_info_v2 with live-frame fallback (#36) | 2026-06-28T16:35:58Z | [legacy] | — | — | [docs/project_review_279-261.md](../project_review_279-261.md) | report:docs/project_review_279-261.md (operator-confirmed) |
| #272 | Per-unit generated names (#264) | 2026-06-28T16:31:51Z | [legacy] | — | — | [docs/project_review_279-261.md](../project_review_279-261.md) | report:docs/project_review_279-261.md (operator-confirmed) |
| #271 | Fix #64: arm the gen-complete structural rebind on the save-load path | 2026-06-28T16:05:46Z | [legacy] | — | — | [docs/project_review_279-261.md](../project_review_279-261.md) | report:docs/project_review_279-261.md (operator-confirmed) |
| #270 | Fix #37: retire dead popup button plumbing for the line-click model | 2026-06-28T07:02:45Z | [legacy] | — | — | [docs/project_review_279-261.md](../project_review_279-261.md) | report:docs/project_review_279-261.md (operator-confirmed) |
| #269 | Fix #21: refine lake-hole / water-water-cliff audit classification | 2026-06-28T07:27:47Z | [legacy] | — | — | [docs/project_review_279-261.md](../project_review_279-261.md) | report:docs/project_review_279-261.md (operator-confirmed) |
| #268 | Fix #183: clear tile/chunk cursor selection on world hide | 2026-06-28T07:27:18Z | [legacy] | — | — | [docs/project_review_279-261.md](../project_review_279-261.md) | report:docs/project_review_279-261.md (operator-confirmed) |
| #267 | Fix #176: clear building selection on zoom/menu transitions | 2026-06-28T00:46:43Z | [legacy] | — | — | [docs/project_review_279-261.md](../project_review_279-261.md) | report:docs/project_review_279-261.md (operator-confirmed) |
| #266 | Fix #148: gate armed debug spawn/edit clicks on the gameplay view | 2026-06-28T00:47:24Z | [legacy] | — | — | [docs/project_review_279-261.md](../project_review_279-261.md) | report:docs/project_review_279-261.md (operator-confirmed) |
| #263 | Fix #123: live pick on click instead of stale cached hover | 2026-06-28T00:36:07Z | [legacy] | — | — | [docs/project_review_279-261.md](../project_review_279-261.md) | report:docs/project_review_279-261.md (operator-confirmed) |
| #262 | Fix #132: clear chunk/tile selection on zoom-band transitions | 2026-06-28T00:23:18Z | [legacy] | — | — | [docs/project_review_262-248.md](../project_review_262-248.md) | report:docs/project_review_262-248.md (operator-confirmed) |
| #261 | Fix #254: converge despike on residual peak pillars (TERRAIN_SPIKE) | 2026-06-28T00:23:35Z | [legacy] | — | — | [docs/project_review_279-261.md](../project_review_279-261.md) | report:docs/project_review_279-261.md (operator-confirmed) |
| #260 | Fix #67: per-instance item identity so UI actions hit the clicked item | 2026-06-28T15:11:40Z | [legacy] | — | — | [docs/project_review_279-261.md](../project_review_279-261.md) | report:docs/project_review_279-261.md (operator-confirmed) |
| #259 | Fix #31: add hover feedback for slider, toggle, and randbox widgets | 2026-06-28T00:01:34Z | [legacy] | — | — | [docs/project_review_262-248.md](../project_review_262-248.md) | report:docs/project_review_262-248.md (operator-confirmed) |
| #258 | Fix #154: gate game.onMouseDown gameplay actions on active world | 2026-06-28T00:01:28Z | [legacy] | — | — | [docs/project_review_262-248.md](../project_review_262-248.md) | report:docs/project_review_262-248.md (operator-confirmed) |
| #257 | Fix #138: tear down arena tile-editor popup on zoom-band transitions | 2026-06-27T23:45:03Z | [legacy] | — | — | [docs/project_review_262-248.md](../project_review_262-248.md) | report:docs/project_review_262-248.md (operator-confirmed) |
| #256 | Fix #140: cancel build placement on zoom-band transitions | 2026-06-27T23:41:54Z | [legacy] | — | — | [docs/project_review_262-248.md](../project_review_262-248.md) | report:docs/project_review_262-248.md (operator-confirmed) |
| #255 | Fix #20: recalibrate FLOATING_LAVA threshold for deep contained pools | 2026-06-27T23:38:54Z | [legacy] | — | — | [docs/project_review_262-248.md](../project_review_262-248.md) | report:docs/project_review_262-248.md (operator-confirmed) |
| #253 | Fix #143: tear down build picker on zoom-band transitions | 2026-06-27T23:33:41Z | [legacy] | — | — | [docs/project_review_262-248.md](../project_review_262-248.md) | report:docs/project_review_262-248.md (operator-confirmed) |
| #252 | Fix #22: enforce deterministic baseline summaries in world_check | 2026-06-27T23:35:08Z | [legacy] | — | — | [docs/project_review_262-248.md](../project_review_262-248.md) | report:docs/project_review_262-248.md (operator-confirmed) |
| #251 | Fix #151: gate debug-overlay click claim on the current view | 2026-06-27T23:31:12Z | [legacy] | — | — | [docs/project_review_262-248.md](../project_review_262-248.md) | report:docs/project_review_262-248.md (operator-confirmed) |
| #250 | Fix #125: scope drag-select hit-test to the active world | 2026-06-27T23:24:21Z | [legacy] | — | — | [docs/project_review_262-248.md](../project_review_262-248.md) | report:docs/project_review_262-248.md (operator-confirmed) |
| #249 | Fix #144: tear down mine-designation anchor on zoom transitions | 2026-06-27T22:48:13Z | [legacy] | — | — | [docs/project_review_249-236.md](../project_review_249-236.md) | report:docs/project_review_249-236.md (operator-confirmed) |
| #248 | Fix #146: tear down active drag-select on view transitions | 2026-06-27T22:49:53Z | [legacy] | — | — | [docs/project_review_262-248.md](../project_review_262-248.md) | report:docs/project_review_262-248.md (operator-confirmed) |
| #247 | Fix #145: gate debug_anim_panel on the gameplay zoomed-in view | 2026-06-27T22:39:36Z | [legacy] | — | — | [docs/project_review_249-236.md](../project_review_249-236.md) | report:docs/project_review_249-236.md (operator-confirmed) |
| #246 | Fix #141: tear down cargo inventory popup on zoom transitions | 2026-06-27T22:23:54Z | [legacy] | — | — | [docs/project_review_249-236.md](../project_review_249-236.md) | report:docs/project_review_249-236.md (operator-confirmed) |
| #245 | Fix #38: load unit pathing cost tunables from config/pathing.yaml | 2026-06-27T22:37:16Z | [legacy] | — | — | [docs/project_review_249-236.md](../project_review_249-236.md) | report:docs/project_review_249-236.md (operator-confirmed) |
| #244 | Fix #103: represent build tool in engine ToolMode | 2026-06-27T23:10:44Z | [legacy] | — | — | [docs/project_review_262-248.md](../project_review_262-248.md) | report:docs/project_review_262-248.md (operator-confirmed) |
| #243 | Fix #137: gate unit_info_v2 pane on zoom view + HUD visibility | 2026-06-27T22:12:25Z | [legacy] | — | — | [docs/project_review_249-236.md](../project_review_249-236.md) | report:docs/project_review_249-236.md (operator-confirmed) |
| #242 | Fix #152: gate debug anim panel click-claim on the gameplay view | 2026-06-27T21:33:29Z | [legacy] | — | — | [docs/project_review_249-236.md](../project_review_249-236.md) | report:docs/project_review_249-236.md (operator-confirmed) |
| #241 | Fix #135: zoom-chunk and zoomed-in tile selections can no longer coexist | 2026-06-27T22:29:02Z | [legacy] | — | — | [docs/project_review_249-236.md](../project_review_249-236.md) | report:docs/project_review_249-236.md (operator-confirmed) |
| #240 | Fix #114: drag-select no longer arms on tool/overlay-claimed clicks | 2026-06-27T21:36:47Z | [legacy] | — | — | [docs/project_review_249-236.md](../project_review_249-236.md) | report:docs/project_review_249-236.md (operator-confirmed) |
| #239 | Fix #134: keep HUD info panel visibility in sync with its page | 2026-06-27T21:38:41Z | [legacy] | — | — | [docs/project_review_249-236.md](../project_review_249-236.md) | report:docs/project_review_249-236.md (operator-confirmed) |
| #238 | Fix #139: dismiss context menu on zoom-band transitions | 2026-06-27T21:26:15Z | [legacy] | — | — | [docs/project_review_249-236.md](../project_review_249-236.md) | report:docs/project_review_249-236.md (operator-confirmed) |
| #237 | Fix #133: tag HUD info pushes so zoom-map chunk selection keeps entity selection | 2026-06-27T19:50:59Z | [legacy] | — | — | [docs/project_review_237-208.md](../project_review_237-208.md) | report:docs/project_review_237-208.md (operator-confirmed) |
| #236 | Fix #175: clear ground-item selection on hud.hide() and zoom transitions | 2026-06-27T19:54:59Z | [legacy] | — | — | [docs/project_review_249-236.md](../project_review_249-236.md) | report:docs/project_review_249-236.md (operator-confirmed) |
| #235 | Fix #147: hide main debug overlay on zoom and menu transitions | 2026-06-27T19:15:25Z | [legacy] | — | — | [docs/project_review_237-208.md](../project_review_237-208.md) | report:docs/project_review_237-208.md (operator-confirmed) |
| #234 | Fix #142: close item-contents popup on zoom transitions | 2026-06-27T18:43:25Z | [legacy] | — | — | [docs/project_review_237-208.md](../project_review_237-208.md) | report:docs/project_review_237-208.md (operator-confirmed) |
| #233 | Fix #99: close cargo inventory popup on hud.hide() | 2026-06-27T18:47:13Z | [legacy] | — | — | [docs/project_review_237-208.md](../project_review_237-208.md) | report:docs/project_review_237-208.md (operator-confirmed) |
| #232 | Fix #222: slope water tiles toward exposed-air edges (waterfall lips) | 2026-06-27T19:48:34Z | [legacy] | — | — | [docs/project_review_237-208.md](../project_review_237-208.md) | report:docs/project_review_237-208.md (operator-confirmed) |
| #231 | Fix #104: close unit_log overlay on hud.hide() | 2026-06-27T18:22:57Z | [legacy] | — | — | [docs/project_review_237-208.md](../project_review_237-208.md) | report:docs/project_review_237-208.md (operator-confirmed) |
| #230 | Fix #100: item-contents popup guards missing container + closes on hud.hide | 2026-06-27T18:05:16Z | [legacy] | — | — | [docs/project_review_237-208.md](../project_review_237-208.md) | report:docs/project_review_237-208.md (operator-confirmed) |
| #229 | Fix #195: prune orphaned per-id Lua state after save load | 2026-06-27T20:59:59Z | [legacy] | — | — | [docs/project_review_249-236.md](../project_review_249-236.md) | report:docs/project_review_249-236.md (operator-confirmed) |
| #228 | Fix #129: only the active world drives the HUD info panel | 2026-06-27T18:06:02Z | [legacy] | — | — | [docs/project_review_237-208.md](../project_review_237-208.md) | report:docs/project_review_237-208.md (operator-confirmed) |
| #227 | Fix #213: remove dead arm in soilFromClimate hot+dry desert branch | 2026-06-27T17:59:02Z | [legacy] | — | — | [docs/project_review_237-208.md](../project_review_237-208.md) | report:docs/project_review_237-208.md (operator-confirmed) |
| #226 | Fix #191: merge restored page into global managers on load | 2026-06-27T17:22:15Z | [legacy] | — | — | [docs/project_review_237-208.md](../project_review_237-208.md) | report:docs/project_review_237-208.md (operator-confirmed) |
| #212 | Fix #128: clear stale Weather/Resources tabs on tile selection | 2026-06-27T16:58:58Z | [legacy] | — | — | [docs/project_review_237-208.md](../project_review_237-208.md) | report:docs/project_review_237-208.md (operator-confirmed) |
| #211 | Fix #172: enforce selection mutual-exclusivity on Shift-clicks | 2026-06-27T16:05:14Z | [legacy] | — | — | [docs/project_review_211-200.md](../project_review_211-200.md) | report:docs/project_review_211-200.md (operator-confirmed) |
| #210 | Fix #182: gate gameplay key handlers when a menu/overlay is open | 2026-06-27T15:58:51Z | [legacy] | — | — | [docs/project_review_211-200.md](../project_review_211-200.md) | report:docs/project_review_211-200.md (operator-confirmed) |
| #209 | Fix #197: scope unit_ai building target finders to the active world | 2026-06-27T15:53:27Z | [legacy] | — | — | [docs/project_review_211-200.md](../project_review_211-200.md) | report:docs/project_review_211-200.md (operator-confirmed) |
| #208 | Fix #196: scope building_spawn to the active world page | 2026-06-27T16:05:41Z | [legacy] | — | — | [docs/project_review_237-208.md](../project_review_237-208.md) | report:docs/project_review_237-208.md (operator-confirmed) |
| #207 | Fix #177: clear building and ground-item selections on Escape | 2026-06-27T15:44:00Z | [legacy] | — | — | [docs/project_review_211-200.md](../project_review_211-200.md) | report:docs/project_review_211-200.md (operator-confirmed) |
| #206 | Fix #180: clear tile/chunk cursor selection on Escape | 2026-06-27T15:38:16Z | [legacy] | — | — | [docs/project_review_211-200.md](../project_review_211-200.md) | report:docs/project_review_211-200.md (operator-confirmed) |
| #205 | Fix #185: hit-test UI on middle-click so it can't reach gameplay | 2026-06-27T15:24:28Z | [legacy] | — | — | [docs/project_review_211-200.md](../project_review_211-200.md) | report:docs/project_review_211-200.md (operator-confirmed) |
| #204 | Fix #198: scope build-tool visibility to the active world | 2026-06-27T15:23:39Z | [legacy] | — | — | [docs/project_review_211-200.md](../project_review_211-200.md) | report:docs/project_review_211-200.md (operator-confirmed) |
| #203 | Fix #186: clear UI focus on right-click miss path | 2026-06-27T15:14:12Z | [legacy] | — | — | [docs/project_review_211-200.md](../project_review_211-200.md) | report:docs/project_review_211-200.md (operator-confirmed) |
| #202 | Re-drop weapons re-equipped into a disabled hand (#193) | 2026-06-27T15:04:47Z | [legacy] | — | — | [docs/project_review_211-200.md](../project_review_211-200.md) | report:docs/project_review_211-200.md (operator-confirmed) |
| #201 | Fix #194: clear unit_resources per-uid caches on save-load | 2026-06-27T14:58:54Z | [legacy] | — | — | [docs/project_review_211-200.md](../project_review_211-200.md) | report:docs/project_review_211-200.md (operator-confirmed) |
| #200 | Fix #190: roll back popped item on cross-manager transfer failure | 2026-06-27T14:56:15Z | [legacy] | — | — | [docs/project_review_211-200.md](../project_review_211-200.md) | report:docs/project_review_211-200.md (operator-confirmed) |
| #199 | Fix #189: weigh actual ItemInstance in depositToCargo capacity check | 2026-06-27T14:48:39Z | [legacy] | — | — | [docs/project_review_199-166.md](../project_review_199-166.md) | report:docs/project_review_199-166.md (operator-confirmed) |
| #192 | Consume right-clicks on ordinary clickable UI controls (#184) | 2026-06-27T14:41:37Z | [legacy] | — | — | [docs/project_review_199-166.md](../project_review_199-166.md) | report:docs/project_review_199-166.md (operator-confirmed) |
| #188 | Make structure.* APIs authoritative against lcStructures (#68) | 2026-06-27T15:45:48Z | [legacy] | — | — | [docs/project_review_211-200.md](../project_review_211-200.md) | report:docs/project_review_211-200.md (operator-confirmed) |
| #187 | Fix #178: clear building/item selection on unit context-menu Info | 2026-06-27T14:32:41Z | [legacy] | — | — | [docs/project_review_199-166.md](../project_review_199-166.md) | report:docs/project_review_199-166.md (operator-confirmed) |
| #181 | Consume ground-item clicks in info mode so tile-info doesn't run (#173) | 2026-06-27T14:23:25Z | [legacy] | — | — | [docs/project_review_199-166.md](../project_review_199-166.md) | report:docs/project_review_199-166.md (operator-confirmed) |
| #179 | Guard projection math against zero-size window/framebuffer (#118) | 2026-06-27T14:45:38Z | [legacy] | — | — | [docs/project_review_199-166.md](../project_review_199-166.md) | report:docs/project_review_199-166.md (operator-confirmed) |
| #174 | Stop hidden HUD from pushing world hover state (#153) | 2026-06-27T14:17:16Z | [legacy] | — | — | [docs/project_review_199-166.md](../project_review_199-166.md) | report:docs/project_review_199-166.md (operator-confirmed) |
| #171 | Fix #116: clear held input state on focus loss and minimize | 2026-06-27T14:25:38Z | [legacy] | — | — | [docs/project_review_199-166.md](../project_review_199-166.md) | report:docs/project_review_199-166.md (operator-confirmed) |
| #170 | Clear mine-tool anchor on Exit to Menu (#102) | 2026-06-27T13:59:40Z | [legacy] | — | — | [docs/project_review_199-166.md](../project_review_199-166.md) | report:docs/project_review_199-166.md (operator-confirmed) |
| #169 | Gate debug anim panel on the debug overlay being active | 2026-06-27T14:01:13Z | [legacy] | — | — | [docs/project_review_199-166.md](../project_review_199-166.md) | report:docs/project_review_199-166.md (operator-confirmed) |
| #168 | Fix #110: clear armed preview on debug_anim_panel shutdown | 2026-06-27T13:38:56Z | [legacy] | — | — | [docs/project_review_199-166.md](../project_review_199-166.md) | report:docs/project_review_199-166.md (operator-confirmed) |
| #167 | Fix #107: shell quit/exit actually quit the game | 2026-06-27T13:36:35Z | [legacy] | — | — | [docs/project_review_167-80.md](../project_review_167-80.md) | report:docs/project_review_167-80.md (operator-confirmed) |
| #166 | Fix #117: locked boxless tooltips swallow clicks on their panel | 2026-06-27T13:37:32Z | [legacy] | — | — | [docs/project_review_199-166.md](../project_review_199-166.md) | report:docs/project_review_199-166.md (operator-confirmed) |
| #165 | Fix #106: reset registered modules to fresh state when absent from save | 2026-06-27T13:20:56Z | [legacy] | — | — | [docs/project_review_167-80.md](../project_review_167-80.md) | report:docs/project_review_167-80.md (operator-confirmed) |
| #164 | Fix #108: settings Revert restores saved tooltip dwell/hint values | 2026-06-27T13:13:50Z | [legacy] | — | — | [docs/project_review_167-80.md](../project_review_167-80.md) | report:docs/project_review_167-80.md (operator-confirmed) |
| #163 | Fix #98: millisecond-precision save timestamps for correct recency ordering | 2026-06-27T13:40:51Z | [legacy] | — | — | [docs/project_review_199-166.md](../project_review_199-166.md) | report:docs/project_review_199-166.md (operator-confirmed) |
| #162 | Fix #105: item tooltips show per-instance sharpness, not def base | 2026-06-27T13:07:02Z | [legacy] | — | — | [docs/project_review_167-80.md](../project_review_167-80.md) | report:docs/project_review_167-80.md (operator-confirmed) |
| #155 | Fix #115: ground-item info shows instance weight, not def weight | 2026-06-26T23:44:05Z | [legacy] | — | — | [docs/project_review_167-80.md](../project_review_167-80.md) | report:docs/project_review_167-80.md (operator-confirmed) |
| #121 | Group D (epic #101): lifecycle / teardown leaks | 2026-06-26T21:11:48Z | [legacy] | — | — | [docs/project_review_167-80.md](../project_review_167-80.md) | report:docs/project_review_167-80.md (operator-confirmed) |
| #113 | Group C (epic #101): per-world units and buildings | 2026-06-26T21:11:26Z | [legacy] | — | — | [docs/project_review_167-80.md](../project_review_167-80.md) | report:docs/project_review_167-80.md (operator-confirmed) |
| #112 | Group B (epic #101): per-world simulation state | 2026-06-26T21:11:04Z | [legacy] | — | — | [docs/project_review_167-80.md](../project_review_167-80.md) | report:docs/project_review_167-80.md (operator-confirmed) |
| #109 | Group A (epic #101): canonical active-world resolver | 2026-06-26T21:09:42Z | [legacy] | — | — | [docs/project_review_167-80.md](../project_review_167-80.md) | report:docs/project_review_167-80.md (operator-confirmed) |
| #83 | Freeze world timescale on save/load auto-pause (#42) | 2026-06-26T17:41:23Z | [legacy] | — | — | [docs/project_review_167-80.md](../project_review_167-80.md) | report:docs/project_review_167-80.md (operator-confirmed) |
| #80 | Fail dump mode hard on init/chunk-load timeout (#45) | 2026-06-26T17:27:45Z | [legacy] | — | — | [docs/project_review_167-80.md](../project_review_167-80.md) | report:docs/project_review_167-80.md (operator-confirmed) |
| #77 | Clear stale build-tool hover tile on off-world cursor (#66) | 2026-06-26T17:59:44Z | [legacy] | — | — | [docs/project_review_167-80.md](../project_review_167-80.md) | report:docs/project_review_167-80.md (operator-confirmed) |
| #71 | Fix pause-menu Save calling nonexistent engine.save API (#54) | 2026-06-26T15:00:59Z | [legacy] | — | — | [docs/project_review_71-33.md](../project_review_71-33.md) | report:docs/project_review_71-33.md (operator-confirmed) |
| #63 | Collapse over-length stride transitions to instant (#56) | 2026-06-26T14:45:46Z | [legacy] | — | — | [docs/project_review_71-33.md](../project_review_71-33.md) | report:docs/project_review_71-33.md (operator-confirmed) |
| #62 | Register engine.logError onto the Lua engine table (#53) | 2026-06-26T14:19:42Z | [legacy] | — | — | [docs/project_review_71-33.md](../project_review_71-33.md) | report:docs/project_review_71-33.md (operator-confirmed) |
| #57 | Don't start a TCP listener in dump mode (#46) | 2026-06-26T13:52:44Z | [legacy] | — | — | [docs/project_review_71-33.md](../project_review_71-33.md) | report:docs/project_review_71-33.md (operator-confirmed) |
| #52 | Treat dump-mode wait timeouts as seconds (#44) | 2026-06-26T13:45:41Z | [legacy] | — | — | [docs/project_review_71-33.md](../project_review_71-33.md) | report:docs/project_review_71-33.md (operator-confirmed) |
| #51 | Reject nonexistent page IDs in world.show (#48) | 2026-06-26T13:34:19Z | [legacy] | — | — | [docs/project_review_71-33.md](../project_review_71-33.md) | report:docs/project_review_71-33.md (operator-confirmed) |
| #50 | Dedup already-pending chunks in world.loadChunksInRegion (#43) | 2026-06-26T14:42:47Z | [legacy] | — | — | [docs/project_review_71-33.md](../project_review_71-33.md) | report:docs/project_review_71-33.md (operator-confirmed) |
| #47 | Remove the dead zoom-map river/lake preview flags (#28) | 2026-06-26T13:06:45Z | [legacy] | — | — | [docs/project_review_71-33.md](../project_review_71-33.md) | report:docs/project_review_71-33.md (operator-confirmed) |
| #41 | Remove the no-op passive fluid simulation pass (#39) | 2026-06-26T12:40:32Z | [legacy] | — | — | [docs/project_review_71-33.md](../project_review_71-33.md) | report:docs/project_review_71-33.md (operator-confirmed) |
| #40 | Render water side faces across chunk boundaries (#26) | 2026-06-26T12:32:11Z | [legacy] | — | — | [docs/project_review_71-33.md](../project_review_71-33.md) | report:docs/project_review_71-33.md (operator-confirmed) |
| #34 | Document world_determinism as a content-identity check (#23) | 2026-06-25T14:29:15Z | [legacy] | — | — | [docs/project_review_71-33.md](../project_review_71-33.md) | report:docs/project_review_71-33.md (operator-confirmed) |
| #33 | Fix flora annual-cycle rendering to use day-of-year, not day-of-month (#25) | 2026-06-25T14:25:28Z | [legacy] | — | — | [docs/project_review_71-33.md](../project_review_71-33.md) | report:docs/project_review_71-33.md (operator-confirmed) |
| #32 | Unify river dry-gap threshold across river tooling (#24) | 2026-06-25T14:03:33Z | [legacy] | — | — | [docs/project_review_32-14.md](../project_review_32-14.md) | report:docs/project_review_32-14.md (operator-confirmed) |
| #17 | Normalize codebase to Unicode syntax | 2026-02-12T06:15:23Z | [legacy] | — | — | [docs/project_review_32-14.md](../project_review_32-14.md) | report:docs/project_review_32-14.md (operator-confirmed) |
| #16 | Refactor World.Geology into focused submodules and remove dead code | 2026-02-12T04:26:32Z | [legacy] | — | — | [docs/project_review_32-14.md](../project_review_32-14.md) | report:docs/project_review_32-14.md (operator-confirmed) |
| #14 | Add comprehensive logging to core engine subsystems | 2026-02-02T20:38:17Z | [legacy] | — | — | [docs/project_review_32-14.md](../project_review_32-14.md) | report:docs/project_review_32-14.md (operator-confirmed) |

- Migrated from the cursor-v2 record. Its exclusive boundary #1423 is kept as provenance only and schedules nothing.

<!-- project-review:ledger:v1 -->

```json
{
  "repositories": {
    "coghex/synarchy": {
      "direct": {
        "adopted": null,
        "endpoint": null,
        "reports": [],
        "reviewed": []
      },
      "excluded": {
        "commits": [],
        "prs": []
      },
      "lease_defaults": null,
      "migration": {
        "boundary": {
          "merged_at": "2026-08-19T17:39:39Z",
          "number": 1423
        },
        "source": "cursor-v2",
        "withheld_boundary": null
      },
      "rows": {
        "1001": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_1018-991.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-07-31T04:43:50Z",
          "report": "docs/project_review_1018-991.md",
          "status": "legacy",
          "title": "Remove redundant LANGUAGE pragmas under src/World/ (1 of 3)"
        },
        "1002": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_1018-991.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-07-31T05:05:32Z",
          "report": "docs/project_review_1018-991.md",
          "status": "legacy",
          "title": "Remove redundant LANGUAGE pragmas under src/Engine (2 of 3)"
        },
        "1003": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_1018-991.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-07-31T05:47:11Z",
          "report": "docs/project_review_1018-991.md",
          "status": "legacy",
          "title": "Remove redundant LANGUAGE pragmas outside src/World and src/Engine (3 of 3)"
        },
        "1004": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_1018-991.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-07-31T05:25:35Z",
          "report": "docs/project_review_1018-991.md",
          "status": "legacy",
          "title": "EngineEnv capability split E5b: migrate the world\u2192render handoff slots to a RenderHandoffCapability record"
        },
        "1015": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_1018-991.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-07-31T12:51:08Z",
          "report": "docs/project_review_1018-991.md",
          "status": "legacy",
          "title": "Guarantee at least one location in every generated world with land (#997)"
        },
        "1017": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_1018-991.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-07-31T15:39:21Z",
          "report": "docs/project_review_1018-991.md",
          "status": "legacy",
          "title": "Define the unit-to-container transfer contract and queued transaction foundation (#1000)"
        },
        "1018": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_1018-991.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-07-31T16:01:30Z",
          "report": "docs/project_review_1018-991.md",
          "status": "legacy",
          "title": "Fix pathologically lethal shallow falls in Unit.Fall (#998)"
        },
        "1020": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_1035-1020.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-07-31T16:22:47Z",
          "report": "docs/project_review_1035-1020.md",
          "status": "legacy",
          "title": "Keep already-latched tutorial branches visible"
        },
        "1023": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_1035-1020.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-07-31T16:43:32Z",
          "report": "docs/project_review_1035-1020.md",
          "status": "legacy",
          "title": "Downshift follow_command's pace with stamina instead of collapsing (#999)"
        },
        "1024": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_1035-1020.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-07-31T20:26:28Z",
          "report": "docs/project_review_1035-1020.md",
          "status": "legacy",
          "title": "Reject boot-mode flags that the selected mode ignores"
        },
        "1025": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_1035-1020.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-07-31T21:11:23Z",
          "report": "docs/project_review_1035-1020.md",
          "status": "legacy",
          "title": "Extract the shared boot config patch and error-path worker teardown into App.Boot"
        },
        "1026": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_1035-1020.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-07-31T20:48:37Z",
          "report": "docs/project_review_1035-1020.md",
          "status": "legacy",
          "title": "Give the three engine main loops one definition of the save-barrier drain and startup handshake"
        },
        "1027": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_1035-1020.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-08-01T02:07:39Z",
          "report": "docs/project_review_1035-1020.md",
          "status": "legacy",
          "title": "EngineEnv capability split E8: SaveLoadCapability, the permanent-only access flip, and the epic gate"
        },
        "1028": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_1035-1020.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-08-01T02:32:55Z",
          "report": "docs/project_review_1035-1020.md",
          "status": "legacy",
          "title": "--preview Phase 4: buildings viewer, flora/structures reuse, epic acceptance gate"
        },
        "1029": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_1035-1020.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-08-01T13:13:04Z",
          "report": "docs/project_review_1035-1020.md",
          "status": "legacy",
          "title": "Add configurable interval autosave, off by default (#913)"
        },
        "1030": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_1035-1020.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-08-01T02:55:16Z",
          "report": "docs/project_review_1035-1020.md",
          "status": "legacy",
          "title": "B1: Add the selected-unit -> eligible-container Transfer interaction"
        },
        "1032": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_1035-1020.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-08-01T15:34:13Z",
          "report": "docs/project_review_1035-1020.md",
          "status": "legacy",
          "title": "Identify Vulkan instances as Synarchy and remove unused graphics dimensions"
        },
        "1033": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_1035-1020.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-08-01T15:59:17Z",
          "report": "docs/project_review_1035-1020.md",
          "status": "legacy",
          "title": "Remove EngineM's vestigial environment type parameter"
        },
        "1034": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_1048-1034.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-08-01T16:49:34Z",
          "report": "docs/project_review_1048-1034.md",
          "status": "legacy",
          "title": "Collapse the eleven identical asset YAML list loaders into one helper"
        },
        "1035": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_1035-1020.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-08-01T16:26:52Z",
          "report": "docs/project_review_1035-1020.md",
          "status": "legacy",
          "title": "Finalize blood decal documentation and gate the bleeding arc for epic closure"
        },
        "1037": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_1048-1034.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-08-01T19:00:53Z",
          "report": "docs/project_review_1048-1034.md",
          "status": "legacy",
          "title": "Derive the unknown-preview-category message from App.Cli's category lists"
        },
        "1038": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_1048-1034.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-08-01T20:12:11Z",
          "report": "docs/project_review_1048-1034.md",
          "status": "legacy",
          "title": "Rename allLayers to defaultLayers and build it with field syntax"
        },
        "1039": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_1048-1034.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-08-01T19:23:50Z",
          "report": "docs/project_review_1048-1034.md",
          "status": "legacy",
          "title": "Give the capability-record convention one documented home"
        },
        "1041": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_1048-1034.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-08-01T19:46:53Z",
          "report": "docs/project_review_1048-1034.md",
          "status": "legacy",
          "title": "Extract the EngineEnv-free helpers out of Engine.Scripting.Lua.API.Save"
        },
        "1042": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_1048-1034.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-08-02T00:12:18Z",
          "report": "docs/project_review_1048-1034.md",
          "status": "legacy",
          "title": "Make logging source attribution independent of wrapper names"
        },
        "1043": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_1048-1034.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-08-02T00:30:50Z",
          "report": "docs/project_review_1048-1034.md",
          "status": "legacy",
          "title": "Unify normal and thread logging entry-construction and formatting"
        },
        "1044": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_1048-1034.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-08-02T00:54:17Z",
          "report": "docs/project_review_1048-1034.md",
          "status": "legacy",
          "title": "Normalise the enforced Unicode operators and guard them against regression"
        },
        "1045": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_1048-1034.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-08-02T01:16:56Z",
          "report": "docs/project_review_1048-1034.md",
          "status": "legacy",
          "title": "Replace shutdownEngine's positional thread parameters with the named worker record"
        },
        "1046": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_1048-1034.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-08-02T02:54:43Z",
          "report": "docs/project_review_1048-1034.md",
          "status": "legacy",
          "title": "Document the three water-table fields the terrain dump layer emits"
        },
        "1047": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_1048-1034.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-08-02T03:23:17Z",
          "report": "docs/project_review_1048-1034.md",
          "status": "legacy",
          "title": "Narrow five over-wide Engine export lists, delete unused withSceneGraph"
        },
        "1048": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_1048-1034.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-08-02T03:49:19Z",
          "report": "docs/project_review_1048-1034.md",
          "status": "legacy",
          "title": "Remove the dead Engine.Graphics.Transform module"
        },
        "1049": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_1063-1049.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-08-02T04:36:30Z",
          "report": "docs/project_review_1063-1049.md",
          "status": "legacy",
          "title": "Remove the never-constructed ScriptFunction script value"
        },
        "1050": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_1063-1049.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-08-02T16:57:03Z",
          "report": "docs/project_review_1063-1049.md",
          "status": "legacy",
          "title": "Remove eleven dead window wrappers from Engine.Graphics.Window.GLFW"
        },
        "1051": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_1063-1049.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-08-02T14:39:59Z",
          "report": "docs/project_review_1063-1049.md",
          "status": "legacy",
          "title": "Delete the unconsumed half of Engine.Asset.Manager"
        },
        "1052": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_1063-1049.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-08-02T05:01:41Z",
          "report": "docs/project_review_1063-1049.md",
          "status": "legacy",
          "title": "Split Engine.Asset.YamlTextures into materials, vegetation, and the texture-name registry"
        },
        "1053": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_1063-1049.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-08-02T12:34:46Z",
          "report": "docs/project_review_1063-1049.md",
          "status": "legacy",
          "title": "Prefix the DevQueues and SwapchainSupportDetails field names"
        },
        "1054": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_1063-1049.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-08-02T12:58:50Z",
          "report": "docs/project_review_1063-1049.md",
          "status": "legacy",
          "title": "Fix six minor defects in the bindless texture and vertex modules"
        },
        "1055": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_1063-1049.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-08-02T14:05:20Z",
          "report": "docs/project_review_1063-1049.md",
          "status": "legacy",
          "title": "Remove the phantom legacy path from the Vulkan texture system"
        },
        "1056": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_1063-1049.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-08-02T13:25:15Z",
          "report": "docs/project_review_1063-1049.md",
          "status": "legacy",
          "title": "Replace repeated nested GraphicsState updates with one modifyGraphicsState helper"
        },
        "1060": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_1063-1049.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-08-02T19:44:20Z",
          "report": "docs/project_review_1063-1049.md",
          "status": "legacy",
          "title": "Make -Werror part of the checked-in Cabal warning policy"
        },
        "1061": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_1063-1049.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-08-02T20:48:24Z",
          "report": "docs/project_review_1063-1049.md",
          "status": "legacy",
          "title": "Give the bindless texture-array and handle-table sizes a single definition"
        },
        "1062": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_1063-1049.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-08-02T20:03:51Z",
          "report": "docs/project_review_1063-1049.md",
          "status": "legacy",
          "title": "Attach the game-clock Haddock to gameTimeRef"
        },
        "1063": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_1063-1049.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-08-02T20:27:00Z",
          "report": "docs/project_review_1063-1049.md",
          "status": "legacy",
          "title": "Encode the dump with aeson and give its two wait loops one definition"
        },
        "1064": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_1076-1064.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-08-02T22:47:35Z",
          "report": "docs/project_review_1076-1064.md",
          "status": "legacy",
          "title": "Recurse Haskell module-budget guard into nested split directories"
        },
        "1065": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_1076-1064.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-08-02T23:09:04Z",
          "report": "docs/project_review_1076-1064.md",
          "status": "legacy",
          "title": "Remove the dead legacy Vulkan state types"
        },
        "1066": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_1076-1064.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-08-02T23:32:19Z",
          "report": "docs/project_review_1076-1064.md",
          "status": "legacy",
          "title": "Remove unlisted dead source modules and audit the Cabal library inventory"
        },
        "1067": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_1076-1064.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-08-02T23:54:54Z",
          "report": "docs/project_review_1076-1064.md",
          "status": "legacy",
          "title": "Fix three minor defects in the Lua scripting tree"
        },
        "1068": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_1076-1064.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-08-03T03:20:24Z",
          "report": "docs/project_review_1076-1064.md",
          "status": "legacy",
          "title": "Correct Engine.Input.Thread's facade Haddock"
        },
        "1069": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_1076-1064.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-08-03T03:42:04Z",
          "report": "docs/project_review_1076-1064.md",
          "status": "legacy",
          "title": "Remove the unused AssetConfig from EngineState"
        },
        "1070": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_1076-1064.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-08-03T04:01:13Z",
          "report": "docs/project_review_1076-1064.md",
          "status": "legacy",
          "title": "Remove the engine's unconstructed exception domains"
        },
        "1071": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_1076-1064.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-08-03T04:23:48Z",
          "report": "docs/project_review_1076-1064.md",
          "status": "legacy",
          "title": "Remove dead TimingState fields and clarify its FPS accumulators"
        },
        "1073": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_1076-1064.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-08-03T14:35:36Z",
          "report": "docs/project_review_1076-1064.md",
          "status": "legacy",
          "title": "Remove the unused LogToFile and LogMulti logging backends"
        },
        "1074": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_1076-1064.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-08-03T15:00:27Z",
          "report": "docs/project_review_1076-1064.md",
          "status": "legacy",
          "title": "Remove the test-only Engine.Core.Var wrapper"
        },
        "1075": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_1076-1064.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-08-03T15:25:11Z",
          "report": "docs/project_review_1076-1064.md",
          "status": "legacy",
          "title": "Remove review-round provenance from production Haskell comments"
        },
        "1076": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_1076-1064.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-08-03T15:49:57Z",
          "report": "docs/project_review_1076-1064.md",
          "status": "legacy",
          "title": "Remove the verified dead logging helpers and wrapper chains"
        },
        "1079": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_1127-1079.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-08-03T17:21:20Z",
          "report": "docs/project_review_1127-1079.md",
          "status": "legacy",
          "title": "Seal ErrorContext and drop the throwEngineException/catchEngine aliases"
        },
        "1080": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_1127-1079.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-08-03T17:43:07Z",
          "report": "docs/project_review_1127-1079.md",
          "status": "legacy",
          "title": "Give the uniform buffer object one layout definition"
        },
        "1082": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_1127-1079.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-08-03T20:37:58Z",
          "report": "docs/project_review_1127-1079.md",
          "status": "legacy",
          "title": "Restate the capture-lock gate's rationale as a present-tense invariant"
        },
        "1089": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_1127-1079.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-08-03T22:18:26Z",
          "report": "docs/project_review_1127-1079.md",
          "status": "legacy",
          "title": "Apply Lua save components per entity instead of clobbering singletons"
        },
        "109": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_167-80.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-06-26T21:09:42Z",
          "report": "docs/project_review_167-80.md",
          "status": "legacy",
          "title": "Group A (epic #101): canonical active-world resolver"
        },
        "112": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_167-80.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-06-26T21:11:04Z",
          "report": "docs/project_review_167-80.md",
          "status": "legacy",
          "title": "Group B (epic #101): per-world simulation state"
        },
        "1120": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_1127-1079.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-08-05T01:14:25Z",
          "report": "docs/project_review_1127-1079.md",
          "status": "legacy",
          "title": "P2a: Stop discarding the Create World world name before it reaches world.init"
        },
        "1121": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_1127-1079.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-08-05T02:22:29Z",
          "report": "docs/project_review_1127-1079.md",
          "status": "legacy",
          "title": "Give the river-flat surface rule one definition and apply it on the dig path"
        },
        "1122": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_1127-1079.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-08-05T01:37:29Z",
          "report": "docs/project_review_1127-1079.md",
          "status": "legacy",
          "title": "L2a: Render a visible fallback for missing glyphs instead of silently dropping them"
        },
        "1123": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_1127-1079.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-08-05T01:58:43Z",
          "report": "docs/project_review_1127-1079.md",
          "status": "legacy",
          "title": "Persist a generated world's language provenance (#1092)"
        },
        "1124": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_1127-1079.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-08-05T05:33:19Z",
          "report": "docs/project_review_1127-1079.md",
          "status": "legacy",
          "title": "L1b: Constrain CCV onsets and let 'y' serve as a vowel (generator version 2)"
        },
        "1125": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_1127-1079.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-08-05T05:55:19Z",
          "report": "docs/project_review_1127-1079.md",
          "status": "legacy",
          "title": "Widen the shipped SDF font atlases past printable ASCII (#1098)"
        },
        "1126": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_1127-1079.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-08-05T14:51:03Z",
          "report": "docs/project_review_1127-1079.md",
          "status": "legacy",
          "title": "A3: give containers a persisted player-knowledge layer with stale contents"
        },
        "1127": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_1127-1079.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-08-05T15:17:10Z",
          "report": "docs/project_review_1127-1079.md",
          "status": "legacy",
          "title": "Delete the always-empty moSurface overlay field"
        },
        "1128": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_1165-1128.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-08-05T15:37:25Z",
          "report": "docs/project_review_1165-1128.md",
          "status": "legacy",
          "title": "Add a CI audit that the Haskell material constants match data/materials YAML"
        },
        "1129": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_1165-1128.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-08-05T16:00:28Z",
          "report": "docs/project_review_1165-1128.md",
          "status": "legacy",
          "title": "A2: generalize the transfer contract to bidirectional endpoints, instance sets, and partial batches"
        },
        "113": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_167-80.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-06-26T21:11:26Z",
          "report": "docs/project_review_167-80.md",
          "status": "legacy",
          "title": "Group C (epic #101): per-world units and buildings"
        },
        "1130": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_1165-1128.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-08-05T16:20:06Z",
          "report": "docs/project_review_1165-1128.md",
          "status": "legacy",
          "title": "Archive the abandoned river redesign and delete its unused module"
        },
        "1134": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_1165-1128.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-08-06T14:14:50Z",
          "report": "docs/project_review_1165-1128.md",
          "status": "legacy",
          "title": "L1c: boundary phonology at every morpheme join, no triple-letter runs (generator version 3)"
        },
        "1140": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_1165-1128.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-08-06T16:55:08Z",
          "report": "docs/project_review_1165-1128.md",
          "status": "legacy",
          "title": "Fix three minor worldgen defects in the fluid identify and chunk-smoothing paths"
        },
        "1141": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_1165-1128.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-08-07T00:20:27Z",
          "report": "docs/project_review_1165-1128.md",
          "status": "legacy",
          "title": "Give the save system's item enumeration one implementation"
        },
        "1142": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_1165-1128.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-08-07T00:40:01Z",
          "report": "docs/project_review_1165-1128.md",
          "status": "legacy",
          "title": "L1d: give a small deterministic set of roots bound forms for dependent compound slots"
        },
        "1143": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_1165-1128.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-08-06T23:59:26Z",
          "report": "docs/project_review_1165-1128.md",
          "status": "legacy",
          "title": "Give the component codec helper real multi-version decode and named arguments"
        },
        "1163": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_1165-1128.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-08-07T15:34:39Z",
          "report": "docs/project_review_1165-1128.md",
          "status": "legacy",
          "title": "Remove five underscore-silenced dead bindings in BuildPixels"
        },
        "1164": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_1165-1128.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-08-07T15:55:41Z",
          "report": "docs/project_review_1165-1128.md",
          "status": "legacy",
          "title": "Remove the World.ZoomMap facade and the dead background renderer"
        },
        "1165": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_1165-1128.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-08-07T16:42:07Z",
          "report": "docs/project_review_1165-1128.md",
          "status": "legacy",
          "title": "Remove stray -fprof-auto pragmas from 43 worldgen modules"
        },
        "1166": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_1165-1128.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-08-07T16:19:26Z",
          "report": "docs/project_review_1165-1128.md",
          "status": "legacy",
          "title": "Give the save command's reference-edge and Lua-component payloads named records"
        },
        "1167": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_1182-1167.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-08-07T17:03:32Z",
          "report": "docs/project_review_1182-1167.md",
          "status": "legacy",
          "title": "L3: Give generated languages per-language orthographic conventions beyond ASCII"
        },
        "1168": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_1182-1167.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-08-07T17:26:35Z",
          "report": "docs/project_review_1182-1167.md",
          "status": "legacy",
          "title": "Name placed locations in their world's own language (#1101)"
        },
        "1169": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_1182-1167.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-08-07T17:47:15Z",
          "report": "docs/project_review_1182-1167.md",
          "status": "legacy",
          "title": "C0: Extract one shared item-list widget from the three duplicated inventory panels"
        },
        "1170": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_1182-1167.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-08-07T23:28:11Z",
          "report": "docs/project_review_1182-1167.md",
          "status": "legacy",
          "title": "Wrap display text by code point, not by byte (#1159)"
        },
        "1171": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_1182-1167.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-08-07T23:50:25Z",
          "report": "docs/project_review_1182-1167.md",
          "status": "legacy",
          "title": "Canonicalise the render-path chunk lookups so water side faces and slopes draw at the U seam"
        },
        "1172": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_1182-1167.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-08-08T00:12:19Z",
          "report": "docs/project_review_1182-1167.md",
          "status": "legacy",
          "title": "Add an append-only audit for the positionally-serialized enums"
        },
        "1173": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_1182-1167.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-08-08T00:36:59Z",
          "report": "docs/project_review_1182-1167.md",
          "status": "legacy",
          "title": "Suggest world names from the generated-language system (#1106)"
        },
        "1174": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_1182-1167.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-08-08T02:24:28Z",
          "report": "docs/project_review_1182-1167.md",
          "status": "legacy",
          "title": "Name rivers in their world's language and expose river identity to Lua (#1102)"
        },
        "1178": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_1182-1167.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-08-08T04:38:32Z",
          "report": "docs/project_review_1182-1167.md",
          "status": "legacy",
          "title": "Move composeFluidMap's stale water-table haddock to lcWaterTableMap"
        },
        "1179": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_1182-1167.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-08-08T05:02:07Z",
          "report": "docs/project_review_1182-1167.md",
          "status": "legacy",
          "title": "Delete the World.Fluids facade and import World.Fluid.Ocean directly"
        },
        "1180": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_1182-1167.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-08-08T05:23:45Z",
          "report": "docs/project_review_1182-1167.md",
          "status": "legacy",
          "title": "Dedup applyFacingF: import from World.Grid instead of redefining"
        },
        "1182": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_1182-1167.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-08-08T05:39:51Z",
          "report": "docs/project_review_1182-1167.md",
          "status": "legacy",
          "title": "Fix fluid-audit archive note's false waterSideFaceQuads claim"
        },
        "1183": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_1210-1183.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-08-08T06:02:09Z",
          "report": "docs/project_review_1210-1183.md",
          "status": "legacy",
          "title": "Remove dead underscore-prefixed bindings"
        },
        "1184": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_1210-1183.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-08-08T06:25:51Z",
          "report": "docs/project_review_1210-1183.md",
          "status": "legacy",
          "title": "Make the u-wrap render offset facing-aware (#1176)"
        },
        "1185": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_1210-1183.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-08-08T06:56:07Z",
          "report": "docs/project_review_1210-1183.md",
          "status": "legacy",
          "title": "Replace stale boot-mode enumerations in three module comments"
        },
        "1186": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_1210-1183.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-08-08T07:21:22Z",
          "report": "docs/project_review_1210-1183.md",
          "status": "legacy",
          "title": "Correct Unit.Pathing.Cost's module comment about future modifiers"
        },
        "1188": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_1210-1183.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-08-08T07:45:40Z",
          "report": "docs/project_review_1210-1183.md",
          "status": "legacy",
          "title": "Show world identity in the save browser and the loading flow (#1107)"
        },
        "1193": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_1210-1183.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-08-08T20:57:09Z",
          "report": "docs/project_review_1210-1183.md",
          "status": "legacy",
          "title": "Normalise the pick and designation coordinate frame at the U seam (#1175)"
        },
        "1194": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_1210-1183.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-08-08T22:58:12Z",
          "report": "docs/project_review_1210-1183.md",
          "status": "legacy",
          "title": "L5: Show a generated name's etymology\u2014decompose it into roots and meanings (#1104)"
        },
        "1198": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_1210-1183.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-08-09T18:23:20Z",
          "report": "docs/project_review_1210-1183.md",
          "status": "legacy",
          "title": "Fail --headless/--offscreen boot when the debug listener can't start (#1190)"
        },
        "1199": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_1210-1183.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-08-09T21:57:18Z",
          "report": "docs/project_review_1210-1183.md",
          "status": "legacy",
          "title": "Reject present-but-malformed CLI values instead of silently defaulting (#1191)"
        },
        "1201": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_1210-1183.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-08-09T22:45:03Z",
          "report": "docs/project_review_1210-1183.md",
          "status": "legacy",
          "title": "State the hydrology pipeline's stage boundaries in a tracked document (#1109)"
        },
        "1202": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_1210-1183.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-08-09T23:08:40Z",
          "report": "docs/project_review_1210-1183.md",
          "status": "legacy",
          "title": "Make the debug console's input editing UTF-8 code-point safe (#1187)"
        },
        "121": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_167-80.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-06-26T21:11:48Z",
          "report": "docs/project_review_167-80.md",
          "status": "legacy",
          "title": "Group D (epic #101): lifecycle / teardown leaks"
        },
        "1210": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_1210-1183.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-08-10T01:00:03Z",
          "report": "docs/project_review_1210-1183.md",
          "status": "legacy",
          "title": "Reconcile the code-health report against the stranded autostash state"
        },
        "1211": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [],
          "history": [],
          "merged_at": "2026-08-10T18:42:37Z",
          "report": null,
          "status": "never-reviewed",
          "title": "Keep a selectable generation on disk when saving over a corrupt-authoritative recovery slot"
        },
        "1214": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [],
          "history": [],
          "merged_at": "2026-08-10T19:56:59Z",
          "report": null,
          "status": "never-reviewed",
          "title": "Fix the invalid regex escape sequence in action_outcome_coverage.py's docstring"
        },
        "1215": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [],
          "history": [],
          "merged_at": "2026-08-10T20:35:20Z",
          "report": null,
          "status": "never-reviewed",
          "title": "Surface Lua load rollback failures instead of reporting a cleanly aborted load"
        },
        "1223": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [],
          "history": [],
          "merged_at": "2026-08-11T02:24:37Z",
          "report": null,
          "status": "never-reviewed",
          "title": "Drain and refill the exact canteen instance the AI selected"
        },
        "1224": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [],
          "history": [],
          "merged_at": "2026-08-11T02:45:22Z",
          "report": null,
          "status": "never-reviewed",
          "title": "Narrow the src/UI export lists, delete the dead submitBuffer, and drop the UI.Focus TextBuffer re-export"
        },
        "1225": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [],
          "history": [],
          "merged_at": "2026-08-11T03:06:54Z",
          "report": null,
          "status": "never-reviewed",
          "title": "State who owns the findings report's status fields, and audit that they agree"
        },
        "1226": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [],
          "history": [],
          "merged_at": "2026-08-11T14:45:23Z",
          "report": null,
          "status": "never-reviewed",
          "title": "Report a distinct terminal disposition when post-load reconciliation callbacks fail"
        },
        "1227": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [],
          "history": [],
          "merged_at": "2026-08-11T16:31:50Z",
          "report": null,
          "status": "never-reviewed",
          "title": "Keep persistent wire networks electrically connected across chunk eviction"
        },
        "1228": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [],
          "history": [],
          "merged_at": "2026-08-11T17:26:58Z",
          "report": null,
          "status": "never-reviewed",
          "title": "Replace the transposable positional runs in the two 14-parameter quad producers"
        },
        "1235": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [],
          "history": [],
          "merged_at": "2026-08-11T21:31:36Z",
          "report": null,
          "status": "never-reviewed",
          "title": "Require power.placeNode's supplying unit to belong to the destination page"
        },
        "1236": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [],
          "history": [],
          "merged_at": "2026-08-11T22:03:00Z",
          "report": null,
          "status": "never-reviewed",
          "title": "Resolve ground pickup and unit drops on the unit's owning page"
        },
        "1240": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [],
          "history": [],
          "merged_at": "2026-08-12T01:35:21Z",
          "report": null,
          "status": "never-reviewed",
          "title": "Judge spawn-time loadout shedding against effective carrying capacity"
        },
        "1241": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [],
          "history": [],
          "merged_at": "2026-08-12T02:37:22Z",
          "report": null,
          "status": "never-reviewed",
          "title": "Re-derive worn-accessory buffs when an accessory is unequipped"
        },
        "1242": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [],
          "history": [],
          "merged_at": "2026-08-12T01:57:20Z",
          "report": null,
          "status": "never-reviewed",
          "title": "Retire a power building's node when the building is destroyed"
        },
        "1243": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [],
          "history": [],
          "merged_at": "2026-08-12T02:59:49Z",
          "report": null,
          "status": "never-reviewed",
          "title": "Repair the Lua save API smoke test against the current asynchronous contracts"
        },
        "1244": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [],
          "history": [],
          "merged_at": "2026-08-12T04:27:59Z",
          "report": null,
          "status": "never-reviewed",
          "title": "Cut the two live UI truncators at code-point boundaries"
        },
        "1245": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [],
          "history": [],
          "merged_at": "2026-08-12T05:09:44Z",
          "report": null,
          "status": "never-reviewed",
          "title": "Make the transactional-load probe deterministically test mutual exclusion"
        },
        "1248": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [],
          "history": [],
          "merged_at": "2026-08-12T13:25:43Z",
          "report": null,
          "status": "never-reviewed",
          "title": "Generalize the container window to any endpoint kind"
        },
        "1271": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_1296.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-08-12T14:21:37Z",
          "report": "docs/project_review_1296.md",
          "status": "legacy",
          "title": "Make the debug-console boot probe's successful-bind check load-tolerant"
        },
        "1284": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_1296.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-08-12T15:12:07Z",
          "report": "docs/project_review_1296.md",
          "status": "legacy",
          "title": "Keep a shutdown requested during engine startup"
        },
        "1285": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_1296.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-08-12T18:35:22Z",
          "report": "docs/project_review_1296.md",
          "status": "legacy",
          "title": "Prevent docs_land.sh from committing unrelated pre-staged files"
        },
        "1287": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_1296.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-08-12T20:39:19Z",
          "report": "docs/project_review_1296.md",
          "status": "legacy",
          "title": "Restore deterministic offscreen building targeting in interaction probes"
        },
        "1288": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_1296.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-08-12T23:43:58Z",
          "report": "docs/project_review_1296.md",
          "status": "legacy",
          "title": "Announce \"Engine running\" only when the startup promotion commits"
        },
        "1289": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_1296.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-08-13T00:53:48Z",
          "report": "docs/project_review_1296.md",
          "status": "legacy",
          "title": "Query and enable the exact descriptor-indexing features the bindless layout uses"
        },
        "1290": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_1296.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-08-13T01:45:00Z",
          "report": "docs/project_review_1296.md",
          "status": "legacy",
          "title": "Reject non-finite remembered weights and reveal times in container-knowledge validation"
        },
        "1292": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_1296.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-08-13T12:45:24Z",
          "report": "docs/project_review_1296.md",
          "status": "legacy",
          "title": "Use Codex Luna for naive playtests"
        },
        "1293": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_1296.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-08-13T13:26:55Z",
          "report": "docs/project_review_1296.md",
          "status": "legacy",
          "title": "Select the nearest of several units for Transfer instead of requiring exactly one"
        },
        "1294": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_1296.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-08-13T13:45:34Z",
          "report": "docs/project_review_1296.md",
          "status": "legacy",
          "title": "Capture the first-aid scenario's pre-fall baseline under a stopped simulation"
        },
        "1295": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_1296.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-08-13T16:23:36Z",
          "report": "docs/project_review_1296.md",
          "status": "legacy",
          "title": "Charge a commanded order's stall budget in eligible time only"
        },
        "1296": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_1296.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-08-13T17:20:29Z",
          "report": "docs/project_review_1296.md",
          "status": "legacy",
          "title": "Make the headless UI specs independent of the developer's saved UI scale"
        },
        "1297": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_1423-1297.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-08-13T17:42:28Z",
          "report": "docs/project_review_1423-1297.md",
          "status": "legacy",
          "title": "Make the name plate's location containment seam-aware"
        },
        "1298": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_1423-1297.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-08-13T18:04:00Z",
          "report": "docs/project_review_1423-1297.md",
          "status": "legacy",
          "title": "Invalidate every alias handle sharing an atlas before its bindless slot is freed"
        },
        "1299": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_1423-1297.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-08-13T21:04:26Z",
          "report": "docs/project_review_1423-1297.md",
          "status": "legacy",
          "title": "Bound world.suggestName's reroll ordinal instead of accepting caller-sized synchronous work"
        },
        "1300": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_1423-1297.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-08-13T21:26:20Z",
          "report": "docs/project_review_1423-1297.md",
          "status": "legacy",
          "title": "Keep the calibration runner's provisioning transfers within carrying capacity"
        },
        "1301": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_1423-1297.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-08-13T21:47:49Z",
          "report": "docs/project_review_1423-1297.md",
          "status": "legacy",
          "title": "Keep etymology recurrence anchored to the active page under an explicit pageId"
        },
        "1302": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_1423-1297.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-08-14T00:15:10Z",
          "report": "docs/project_review_1423-1297.md",
          "status": "legacy",
          "title": "Report source_missing when a cross-manager rollback cannot restore"
        },
        "1303": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_1423-1297.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-08-14T00:33:48Z",
          "report": "docs/project_review_1423-1297.md",
          "status": "legacy",
          "title": "Validate defName before classifying a held instance as non-transferable"
        },
        "1304": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_1423-1297.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-08-14T00:55:22Z",
          "report": "docs/project_review_1423-1297.md",
          "status": "legacy",
          "title": "Make the restored-entity apply context immutable across Lua save components"
        },
        "1305": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_1423-1297.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-08-14T03:25:40Z",
          "report": "docs/project_review_1423-1297.md",
          "status": "legacy",
          "title": "Fail the cabal inventory audit on duplicate or path-contradicting modules"
        },
        "1306": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_1423-1297.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-08-14T03:47:28Z",
          "report": "docs/project_review_1423-1297.md",
          "status": "legacy",
          "title": "Add container-knowledge to Lua's Haskell-component mirror and make the mirror drift-proof"
        },
        "1307": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_1423-1297.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-08-14T04:10:15Z",
          "report": "docs/project_review_1423-1297.md",
          "status": "legacy",
          "title": "Reject duplicate, current, and future entries in a component's csOlderVersions table"
        },
        "1308": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_1423-1297.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-08-14T15:35:59Z",
          "report": "docs/project_review_1423-1297.md",
          "status": "legacy",
          "title": "Fail the save-wire gates when a guarded constructor's same-arity payload shape changes"
        },
        "1309": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_1423-1297.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-08-14T15:57:15Z",
          "report": "docs/project_review_1423-1297.md",
          "status": "legacy",
          "title": "Make the unit asset inventory authoritative and enforceable"
        },
        "1310": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_1423-1297.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-08-14T20:54:46Z",
          "report": "docs/project_review_1423-1297.md",
          "status": "legacy",
          "title": "Give transfer orders durable state and persistence (#1246)"
        },
        "1312": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_1423-1297.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-08-14T22:49:02Z",
          "report": "docs/project_review_1423-1297.md",
          "status": "legacy",
          "title": "Render last-known container contents with an age indicator (#1237)"
        },
        "1313": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_1423-1297.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-08-14T23:13:03Z",
          "report": "docs/project_review_1423-1297.md",
          "status": "legacy",
          "title": "Build the deterministic per-animation atlas compiler and index"
        },
        "1314": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_1423-1297.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-08-15T11:42:53Z",
          "report": "docs/project_review_1423-1297.md",
          "status": "legacy",
          "title": "Execute a transfer order as a unit job that commits on arrival"
        },
        "1315": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_1423-1297.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-08-15T14:54:49Z",
          "report": "docs/project_review_1423-1297.md",
          "status": "legacy",
          "title": "Validate unit-animation frame contents in the asset inventory gate"
        },
        "1316": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_1423-1297.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-08-15T15:18:40Z",
          "report": "docs/project_review_1423-1297.md",
          "status": "legacy",
          "title": "Give the five divergent truncateToWidth copies one shared implementation"
        },
        "1317": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_1423-1297.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-08-15T15:57:15Z",
          "report": "docs/project_review_1423-1297.md",
          "status": "legacy",
          "title": "Add atlas-frame storage and sampling to the unit runtime"
        },
        "1327": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_1423-1297.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-08-15T21:06:50Z",
          "report": "docs/project_review_1423-1297.md",
          "status": "legacy",
          "title": "Add physical bulk and portable-storage capacity data"
        },
        "1328": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_1423-1297.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-08-16T00:24:48Z",
          "report": "docs/project_review_1423-1297.md",
          "status": "legacy",
          "title": "Mark every location unknown on the zoom map and reveal its type by unit sight"
        },
        "1332": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_1423-1297.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-08-16T02:12:32Z",
          "report": "docs/project_review_1423-1297.md",
          "status": "legacy",
          "title": "Sign every supplied row field the item list can display"
        },
        "1333": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_1423-1297.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-08-16T02:36:16Z",
          "report": "docs/project_review_1423-1297.md",
          "status": "legacy",
          "title": "Migrate acolyte as the end-to-end atlas pilot"
        },
        "1334": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_1423-1297.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-08-16T04:52:43Z",
          "report": "docs/project_review_1423-1297.md",
          "status": "legacy",
          "title": "Take gameplay's random stream back from a UI widget (#1330)"
        },
        "1335": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_1423-1297.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-08-16T05:20:30Z",
          "report": "docs/project_review_1423-1297.md",
          "status": "legacy",
          "title": "Show tracked temperature summaries in unit and cargo item-list rows"
        },
        "1336": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_1423-1297.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-08-16T05:42:10Z",
          "report": "docs/project_review_1423-1297.md",
          "status": "legacy",
          "title": "Compile the six remaining unit trees and retire per-frame unit-animation loading (#1261)"
        },
        "1338": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_1423-1297.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-08-16T13:28:11Z",
          "report": "docs/project_review_1423-1297.md",
          "status": "legacy",
          "title": "Add the nested container-window stack"
        },
        "1339": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_1423-1297.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-08-16T14:28:15Z",
          "report": "docs/project_review_1423-1297.md",
          "status": "legacy",
          "title": "Close the unit-atlas pipeline with documentation and focused regression gates (#1262)"
        },
        "1340": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_1423-1297.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-08-16T14:54:32Z",
          "report": "docs/project_review_1423-1297.md",
          "status": "legacy",
          "title": "Promote \"Store in cargo\" to a queued order-at-a-distance"
        },
        "1343": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_1423-1297.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-08-16T15:58:28Z",
          "report": "docs/project_review_1423-1297.md",
          "status": "legacy",
          "title": "Fail a probe at setup when its inline fixture registers nothing"
        },
        "1344": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_1423-1297.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-08-16T16:21:54Z",
          "report": "docs/project_review_1423-1297.md",
          "status": "legacy",
          "title": "Restore transfer_order_probe.py's item fixture against the current item schema"
        },
        "1345": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_1423-1297.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-08-16T19:26:24Z",
          "report": "docs/project_review_1423-1297.md",
          "status": "legacy",
          "title": "Give scripts/shell.lua one module identity so settings rescale reaches the live console"
        },
        "1346": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_1423-1297.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-08-16T19:51:36Z",
          "report": "docs/project_review_1423-1297.md",
          "status": "legacy",
          "title": "Clear the unit-AI claim and repair-priority tables when a save load replaces the session"
        },
        "1347": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_1423-1297.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-08-16T22:25:56Z",
          "report": "docs/project_review_1423-1297.md",
          "status": "legacy",
          "title": "Reject craft and construction jobs whose input load cannot fit before claiming them"
        },
        "1348": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_1423-1297.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-08-17T01:44:47Z",
          "report": "docs/project_review_1423-1297.md",
          "status": "legacy",
          "title": "Report simultaneously severed subparts in encounter order (#1331)"
        },
        "1349": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_1423-1297.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-08-17T02:05:52Z",
          "report": "docs/project_review_1423-1297.md",
          "status": "legacy",
          "title": "Handle Mode B transfer-order failures (#1253)"
        },
        "1350": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_1423-1297.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-08-17T03:49:50Z",
          "report": "docs/project_review_1423-1297.md",
          "status": "legacy",
          "title": "Add the escort transfer session (Mode A)"
        },
        "1351": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_1423-1297.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-08-18T14:50:39Z",
          "report": "docs/project_review_1423-1297.md",
          "status": "legacy",
          "title": "Fail the Lua duplicate-function audit when its scan scope or module grammar goes unmatched"
        },
        "1352": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_1423-1297.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-08-18T15:26:57Z",
          "report": "docs/project_review_1423-1297.md",
          "status": "legacy",
          "title": "Terminate a probe's engine descendants on ordinary failure, not only on timeout"
        },
        "1353": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_1423-1297.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-08-18T15:50:32Z",
          "report": "docs/project_review_1423-1297.md",
          "status": "legacy",
          "title": "Extend escort transfers to unit-to-unit two-sided holds"
        },
        "1371": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_1423-1297.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-08-18T17:08:32Z",
          "report": "docs/project_review_1423-1297.md",
          "status": "legacy",
          "title": "Reject unknown probe keys in exact selection instead of silently dropping them"
        },
        "1373": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_1423-1297.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-08-18T18:23:19Z",
          "report": "docs/project_review_1423-1297.md",
          "status": "legacy",
          "title": "Isolate headless config writes from the developer's checkout (#1357)"
        },
        "1387": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_1423-1297.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-08-18T18:46:02Z",
          "report": "docs/project_review_1423-1297.md",
          "status": "legacy",
          "title": "Handle Mode A session failures (#1254, UIT-5B)"
        },
        "1389": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_1423-1297.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-08-18T19:51:11Z",
          "report": "docs/project_review_1423-1297.md",
          "status": "legacy",
          "title": "Run the etymology page-scope gate without a world thread"
        },
        "1390": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_1423-1297.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-08-18T23:05:26Z",
          "report": "docs/project_review_1423-1297.md",
          "status": "legacy",
          "title": "Fail the headless example when its world worker has already died"
        },
        "1391": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_1423-1297.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-08-18T23:34:04Z",
          "report": "docs/project_review_1423-1297.md",
          "status": "legacy",
          "title": "Stop HUD layout fixtures from emitting 948 missing-world warnings"
        },
        "1392": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_1423-1297.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-08-19T00:53:38Z",
          "report": "docs/project_review_1423-1297.md",
          "status": "legacy",
          "title": "Drop the headless harness's redundant post-shutdown sleep"
        },
        "1393": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_1423-1297.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-08-19T03:29:31Z",
          "report": "docs/project_review_1423-1297.md",
          "status": "legacy",
          "title": "Give the dump's generation parameters and chunk region named types"
        },
        "14": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_32-14.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-02-02T20:38:17Z",
          "report": "docs/project_review_32-14.md",
          "status": "legacy",
          "title": "Add comprehensive logging to core engine subsystems"
        },
        "1403": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_1423-1297.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-08-19T12:52:53Z",
          "report": "docs/project_review_1423-1297.md",
          "status": "legacy",
          "title": "Gate the unified transfer system end to end"
        },
        "1404": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_1423-1297.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-08-19T13:38:07Z",
          "report": "docs/project_review_1423-1297.md",
          "status": "legacy",
          "title": "Place the remote-settlement modal's title and message inside the panel's content area"
        },
        "1405": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_1423-1297.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-08-19T14:05:46Z",
          "report": "docs/project_review_1423-1297.md",
          "status": "legacy",
          "title": "Resolve the boot mode and debug port once in Main, and fix two app/ nits"
        },
        "1406": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_1423-1297.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-08-19T15:21:04Z",
          "report": "docs/project_review_1423-1297.md",
          "status": "legacy",
          "title": "Load item definitions from logical subdirectories"
        },
        "1407": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_1423-1297.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-08-19T15:46:20Z",
          "report": "docs/project_review_1423-1297.md",
          "status": "legacy",
          "title": "Narrow the remaining over-wide src/Engine/ export lists and remove the superseded Lua log registrar"
        },
        "1409": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_1423-1297.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-08-19T16:49:15Z",
          "report": "docs/project_review_1423-1297.md",
          "status": "legacy",
          "title": "Pin the Hackage index-state so a dependency plan needs a commit to move"
        },
        "1411": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "cursor:docs/project_review_boundaries.md",
            "report:docs/project_review_1423-1297.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-08-19T18:04:12Z",
          "report": "docs/project_review_1423-1297.md",
          "status": "legacy",
          "title": "Repair the action-outcome probe's portal and chop fixtures"
        },
        "1422": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_1423-1297.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-08-19T17:14:35Z",
          "report": "docs/project_review_1423-1297.md",
          "status": "legacy",
          "title": "Serialize probes that mutate the same repository resource (#1322)"
        },
        "1423": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "cursor:docs/project_review_boundaries.md",
            "report:docs/project_review_1423-1297.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-08-19T17:39:39Z",
          "report": "docs/project_review_1423-1297.md",
          "status": "legacy",
          "title": "Reject world-audit categories that were never classified or given a threshold"
        },
        "1424": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_1455-1424.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-08-19T18:31:28Z",
          "report": "docs/project_review_1455-1424.md",
          "status": "legacy",
          "title": "Write an engine's ledger line before the handshake that releases its probe"
        },
        "1442": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_1455-1424.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-08-19T19:08:33Z",
          "report": "docs/project_review_1455-1424.md",
          "status": "legacy",
          "title": "Give the six worker threads one startup definition in Engine.Core.Thread"
        },
        "1443": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_1455-1424.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-08-19T19:29:46Z",
          "report": "docs/project_review_1455-1424.md",
          "status": "legacy",
          "title": "Stop a meal before opening a mostly-wasted discrete food item (#1219)"
        },
        "1445": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_1455-1424.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-08-19T19:53:43Z",
          "report": "docs/project_review_1455-1424.md",
          "status": "legacy",
          "title": "Keep every bleeding-trail probe spawn on loaded arena terrain"
        },
        "1446": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_1455-1424.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-08-19T20:15:43Z",
          "report": "docs/project_review_1455-1424.md",
          "status": "legacy",
          "title": "Give the per-tile fluid-surface fold one definition in Chunk/Fluid.hs"
        },
        "1448": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_1455-1424.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-08-19T21:17:02Z",
          "report": "docs/project_review_1455-1424.md",
          "status": "legacy",
          "title": "Define a probe result protocol and add the repeat-run flakiness harness"
        },
        "1449": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_1455-1424.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-08-19T21:41:39Z",
          "report": "docs/project_review_1455-1424.md",
          "status": "legacy",
          "title": "Start items at full condition; ground spawn is the salvage exception (#1421)"
        },
        "1450": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_1455-1424.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-08-19T22:06:34Z",
          "report": "docs/project_review_1455-1424.md",
          "status": "legacy",
          "title": "Keep the autosave staging slots out of the player-facing save list"
        },
        "1451": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_1455-1424.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-08-19T22:34:33Z",
          "report": "docs/project_review_1455-1424.md",
          "status": "legacy",
          "title": "Let a probe record more than one manual-only reason (#1440)"
        },
        "1452": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_1455-1424.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-08-19T22:59:45Z",
          "report": "docs/project_review_1455-1424.md",
          "status": "legacy",
          "title": "Require regular files for building preview animation frames"
        },
        "1453": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_1455-1424.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-08-20T00:43:02Z",
          "report": "docs/project_review_1455-1424.md",
          "status": "legacy",
          "title": "Establish combat preconditions in the combat-animation probe before sampling"
        },
        "1454": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_1455-1424.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-08-19T23:22:39Z",
          "report": "docs/project_review_1455-1424.md",
          "status": "legacy",
          "title": "Fit the tutorial toggle caption inside its box and assert its rendered bounds"
        },
        "1455": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_1455-1424.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-08-20T01:20:58Z",
          "report": "docs/project_review_1455-1424.md",
          "status": "legacy",
          "title": "Compare each worldgen dump against its baseline's recorded content hash"
        },
        "1456": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "cursor:docs/project_review_boundaries.md",
            "report:docs/project_review_2007-1456.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-08-20T01:43:02Z",
          "report": "docs/project_review_2007-1456.md",
          "status": "legacy",
          "title": "Re-validate the transfer session's source at its reusable creation boundary"
        },
        "1457": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "cursor:docs/project_review_boundaries.md",
            "report:docs/project_review_2007-1456.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-08-20T02:07:41Z",
          "report": "docs/project_review_2007-1456.md",
          "status": "legacy",
          "title": "Serialize the config probes against every engine-booting probe (#1444)"
        },
        "1458": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "cursor:docs/project_review_boundaries.md",
            "report:docs/project_review_2007-1456.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-08-20T02:27:38Z",
          "report": "docs/project_review_2007-1456.md",
          "status": "legacy",
          "title": "Gate the combat-animation probe's death contract on the unit's actual pose"
        },
        "1459": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "cursor:docs/project_review_boundaries.md",
            "report:docs/project_review_2007-1456.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-08-20T02:47:48Z",
          "report": "docs/project_review_2007-1456.md",
          "status": "legacy",
          "title": "Wait for the requested page in the multiworld-save probe"
        },
        "1460": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "cursor:docs/project_review_boundaries.md",
            "report:docs/project_review_2007-1456.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-08-20T03:13:03Z",
          "report": "docs/project_review_2007-1456.md",
          "status": "legacy",
          "title": "Converge every item-creation path on one materializer"
        },
        "1461": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "cursor:docs/project_review_boundaries.md",
            "report:docs/project_review_2007-1456.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-08-20T03:39:22Z",
          "report": "docs/project_review_2007-1456.md",
          "status": "legacy",
          "title": "Keep ambient wander from routing over damaging drops (#1217)"
        },
        "1462": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "cursor:docs/project_review_boundaries.md",
            "report:docs/project_review_2007-1456.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-08-20T04:02:05Z",
          "report": "docs/project_review_2007-1456.md",
          "status": "legacy",
          "title": "Report NoLand for a landless world even when no definition is placeable"
        },
        "1463": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "cursor:docs/project_review_boundaries.md",
            "report:docs/project_review_2007-1456.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-08-20T04:33:32Z",
          "report": "docs/project_review_2007-1456.md",
          "status": "legacy",
          "title": "Stop the envelope framing fingerprint reacting to redundant LANGUAGE pragma edits"
        },
        "1464": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "cursor:docs/project_review_boundaries.md",
            "report:docs/project_review_2007-1456.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-08-20T04:55:13Z",
          "report": "docs/project_review_2007-1456.md",
          "status": "legacy",
          "title": "Make the autosave player-intent race test deterministic (#1372)"
        },
        "1465": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "cursor:docs/project_review_boundaries.md",
            "report:docs/project_review_2007-1456.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-08-20T05:23:19Z",
          "report": "docs/project_review_2007-1456.md",
          "status": "legacy",
          "title": "Decide Vulkan instance extensions in a pure function and pin it without a driver"
        },
        "1466": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "cursor:docs/project_review_boundaries.md",
            "report:docs/project_review_2007-1456.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-08-20T07:17:08Z",
          "report": "docs/project_review_2007-1456.md",
          "status": "legacy",
          "title": "Test the world-audit output's canonicalizing sort instead of comparing calls to themselves"
        },
        "1467": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "cursor:docs/project_review_boundaries.md",
            "report:docs/project_review_2007-1456.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-08-20T07:45:56Z",
          "report": "docs/project_review_2007-1456.md",
          "status": "legacy",
          "title": "Pin the river-name vector and delete the naming self-comparison"
        },
        "1468": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "cursor:docs/project_review_boundaries.md",
            "report:docs/project_review_2007-1456.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-08-20T08:14:21Z",
          "report": "docs/project_review_2007-1456.md",
          "status": "legacy",
          "title": "Make make ci a true mirror of CI's gate set, and gate the two against drift"
        },
        "1469": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "cursor:docs/project_review_boundaries.md",
            "report:docs/project_review_2007-1456.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-08-20T14:00:12Z",
          "report": "docs/project_review_2007-1456.md",
          "status": "legacy",
          "title": "Hold position after a completed player move order (#1216)"
        },
        "1470": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "cursor:docs/project_review_boundaries.md",
            "report:docs/project_review_2007-1456.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-08-20T14:48:00Z",
          "report": "docs/project_review_2007-1456.md",
          "status": "legacy",
          "title": "Move the deprecated GitHub Actions off their Node 20 majors"
        },
        "1472": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "cursor:docs/project_review_boundaries.md",
            "report:docs/project_review_2007-1456.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-08-20T15:14:53Z",
          "report": "docs/project_review_2007-1456.md",
          "status": "legacy",
          "title": "Fail the worldgen regression gate when a selected seed has no baseline"
        },
        "1473": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "cursor:docs/project_review_boundaries.md",
            "report:docs/project_review_2007-1456.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-08-20T21:43:54Z",
          "report": "docs/project_review_2007-1456.md",
          "status": "legacy",
          "title": "Move the zoom cache's output types out of the render tree and state the cache/render boundary"
        },
        "1477": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "cursor:docs/project_review_boundaries.md",
            "report:docs/project_review_2007-1456.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-08-21T00:34:59Z",
          "report": "docs/project_review_2007-1456.md",
          "status": "legacy",
          "title": "Refuse a cyclic default_contents graph instead of hanging at item creation"
        },
        "1478": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "cursor:docs/project_review_boundaries.md",
            "report:docs/project_review_2007-1456.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-08-21T00:55:42Z",
          "report": "docs/project_review_2007-1456.md",
          "status": "legacy",
          "title": "Bracket every test Vulkan instance with its destroy (#1401)"
        },
        "1480": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "cursor:docs/project_review_boundaries.md",
            "report:docs/project_review_2007-1456.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-08-21T01:43:04Z",
          "report": "docs/project_review_2007-1456.md",
          "status": "legacy",
          "title": "Replace the 22 local jget copies with probelib.send_json (#1160)"
        },
        "1489": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "cursor:docs/project_review_boundaries.md",
            "report:docs/project_review_2007-1456.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-08-20T21:09:42Z",
          "report": "docs/project_review_2007-1456.md",
          "status": "legacy",
          "title": "Stop asserting the real findings report still has a `[deferred]` heading"
        },
        "1491": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "cursor:docs/project_review_boundaries.md",
            "report:docs/project_review_2007-1456.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-08-20T23:16:11Z",
          "report": "docs/project_review_2007-1456.md",
          "status": "legacy",
          "title": "Give master pushes their own CI run, and docs-only pushes a fast path (#1490)"
        },
        "1495": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "cursor:docs/project_review_boundaries.md",
            "report:docs/project_review_2007-1456.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-08-21T03:53:10Z",
          "report": "docs/project_review_2007-1456.md",
          "status": "legacy",
          "title": "Normalise inequality to \u2262 in src/+app/ and extend the operator audit to catch \u2260"
        },
        "1496": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "cursor:docs/project_review_boundaries.md",
            "report:docs/project_review_2007-1456.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-08-21T04:22:02Z",
          "report": "docs/project_review_2007-1456.md",
          "status": "legacy",
          "title": "Describe the real fresh-world arrival in the manual and the portal objective"
        },
        "1497": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "cursor:docs/project_review_boundaries.md",
            "report:docs/project_review_2007-1456.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-08-21T04:49:52Z",
          "report": "docs/project_review_2007-1456.md",
          "status": "legacy",
          "title": "Clear the cached build and store targets when no target resolves"
        },
        "1498": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "cursor:docs/project_review_boundaries.md",
            "report:docs/project_review_2007-1456.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-08-21T05:11:45Z",
          "report": "docs/project_review_2007-1456.md",
          "status": "legacy",
          "title": "Give the retaliation swap its window back, and a gate for it"
        },
        "1499": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "cursor:docs/project_review_boundaries.md",
            "report:docs/project_review_2007-1456.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-08-21T05:38:26Z",
          "report": "docs/project_review_2007-1456.md",
          "status": "legacy",
          "title": "Register the already-migrated position_hold probe as protocol-compatible"
        },
        "1500": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "cursor:docs/project_review_boundaries.md",
            "report:docs/project_review_2007-1456.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-08-21T06:04:17Z",
          "report": "docs/project_review_2007-1456.md",
          "status": "legacy",
          "title": "Provision every load-validated registry in the save-migration probe bootstrap"
        },
        "1501": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "cursor:docs/project_review_boundaries.md",
            "report:docs/project_review_2007-1456.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-08-21T06:30:09Z",
          "report": "docs/project_review_2007-1456.md",
          "status": "legacy",
          "title": "Pin every third-party GitHub Action by commit SHA, bumping the three docker actions first"
        },
        "1502": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "cursor:docs/project_review_boundaries.md",
            "report:docs/project_review_2007-1456.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-08-21T07:00:01Z",
          "report": "docs/project_review_2007-1456.md",
          "status": "legacy",
          "title": "Select the worldgen gate for the simulation and world-thread stages the dump reads"
        },
        "1503": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "cursor:docs/project_review_boundaries.md",
            "report:docs/project_review_2007-1456.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-08-21T07:23:02Z",
          "report": "docs/project_review_2007-1456.md",
          "status": "legacy",
          "title": "Define the probe census record and its atomic write path"
        },
        "1504": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "cursor:docs/project_review_boundaries.md",
            "report:docs/project_review_2007-1456.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-08-21T07:52:19Z",
          "report": "docs/project_review_2007-1456.md",
          "status": "legacy",
          "title": "Reject a malformed --region instead of dumping the default region"
        },
        "1505": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "cursor:docs/project_review_boundaries.md",
            "report:docs/project_review_2007-1456.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-08-21T08:14:32Z",
          "report": "docs/project_review_2007-1456.md",
          "status": "legacy",
          "title": "Fail the unified-transfer probe on unexpected persistence integrity diagnostics"
        },
        "1506": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "cursor:docs/project_review_boundaries.md",
            "report:docs/project_review_2007-1456.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-08-21T08:39:00Z",
          "report": "docs/project_review_2007-1456.md",
          "status": "legacy",
          "title": "Treat ordinary test-only paths as probe-neutral in the CI probe selector"
        },
        "1507": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "cursor:docs/project_review_boundaries.md",
            "report:docs/project_review_2007-1456.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-08-21T13:56:39Z",
          "report": "docs/project_review_2007-1456.md",
          "status": "legacy",
          "title": "Stop probe_census --set-acceptable-failures from clearing the stored justification"
        },
        "1508": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "cursor:docs/project_review_boundaries.md",
            "report:docs/project_review_2007-1456.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-08-21T14:13:57Z",
          "report": "docs/project_review_2007-1456.md",
          "status": "legacy",
          "title": "Pin each fall-survival example to its own measured value (#1412)"
        },
        "1509": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "cursor:docs/project_review_boundaries.md",
            "report:docs/project_review_2007-1456.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-08-21T15:10:32Z",
          "report": "docs/project_review_2007-1456.md",
          "status": "legacy",
          "title": "Run the probe-protocol self-test in CI and make ci"
        },
        "1510": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "cursor:docs/project_review_boundaries.md",
            "report:docs/project_review_2007-1456.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-08-21T14:43:54Z",
          "report": "docs/project_review_2007-1456.md",
          "status": "legacy",
          "title": "Stop a save-migration fixture after its load prerequisite fails"
        },
        "1511": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "cursor:docs/project_review_boundaries.md",
            "report:docs/project_review_2007-1456.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-08-21T16:08:33Z",
          "report": "docs/project_review_2007-1456.md",
          "status": "legacy",
          "title": "Report which restore outcome each CI cache got (#1358)"
        },
        "1512": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "cursor:docs/project_review_boundaries.md",
            "report:docs/project_review_2007-1456.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-08-21T15:37:20Z",
          "report": "docs/project_review_2007-1456.md",
          "status": "legacy",
          "title": "Read the Codex $test record for a probe, read-only"
        },
        "1513": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "cursor:docs/project_review_boundaries.md",
            "report:docs/project_review_2007-1456.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-08-21T16:37:48Z",
          "report": "docs/project_review_2007-1456.md",
          "status": "legacy",
          "title": "Migrate the thermo_altitude probe to probe-result/v1"
        },
        "1514": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "cursor:docs/project_review_boundaries.md",
            "report:docs/project_review_2007-1456.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-08-21T17:04:23Z",
          "report": "docs/project_review_2007-1456.md",
          "status": "legacy",
          "title": "Follow first-aid treatment to a stable-or-terminal outcome (#1221)"
        },
        "1515": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "cursor:docs/project_review_boundaries.md",
            "report:docs/project_review_2007-1456.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-08-21T17:27:29Z",
          "report": "docs/project_review_2007-1456.md",
          "status": "legacy",
          "title": "Give clamp and formatGameTimeHMS one definition each in scripts/lib"
        },
        "1516": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "cursor:docs/project_review_boundaries.md",
            "report:docs/project_review_2007-1456.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-08-21T17:44:13Z",
          "report": "docs/project_review_2007-1456.md",
          "status": "legacy",
          "title": "Add bounded dual-provider playtest usage tracking"
        },
        "1517": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "cursor:docs/project_review_boundaries.md",
            "report:docs/project_review_2007-1456.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-08-21T18:12:28Z",
          "report": "docs/project_review_2007-1456.md",
          "status": "legacy",
          "title": "Select only the boot smoke for UI widget-kit changes (#1365)"
        },
        "1518": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "cursor:docs/project_review_boundaries.md",
            "report:docs/project_review_2007-1456.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-08-21T18:39:19Z",
          "report": "docs/project_review_2007-1456.md",
          "status": "legacy",
          "title": "Select the save-compat repl reproducibility test by changed paths"
        },
        "1519": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "cursor:docs/project_review_boundaries.md",
            "report:docs/project_review_2007-1456.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-08-21T19:08:21Z",
          "report": "docs/project_review_2007-1456.md",
          "status": "legacy",
          "title": "Validate the probe census against a declared schema, not hand-rolled checks"
        },
        "1520": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "cursor:docs/project_review_boundaries.md",
            "report:docs/project_review_2007-1456.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-08-21T19:31:12Z",
          "report": "docs/project_review_2007-1456.md",
          "status": "legacy",
          "title": "Detect probes with related work already in flight"
        },
        "1521": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "cursor:docs/project_review_boundaries.md",
            "report:docs/project_review_2007-1456.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-08-21T21:53:59Z",
          "report": "docs/project_review_2007-1456.md",
          "status": "legacy",
          "title": "Run the w128 volcano exposure regression on worldgen-selected CI runs"
        },
        "1522": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "cursor:docs/project_review_boundaries.md",
            "report:docs/project_review_2007-1456.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-08-21T21:24:29Z",
          "report": "docs/project_review_2007-1456.md",
          "status": "legacy",
          "title": "Define cohort and staleness semantics for census records"
        },
        "1523": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "cursor:docs/project_review_boundaries.md",
            "report:docs/project_review_2007-1456.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-08-21T22:37:14Z",
          "report": "docs/project_review_2007-1456.md",
          "status": "legacy",
          "title": "Reconcile the probe census's cross-field invariants"
        },
        "1526": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "cursor:docs/project_review_boundaries.md",
            "report:docs/project_review_2007-1456.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-08-21T23:05:14Z",
          "report": "docs/project_review_2007-1456.md",
          "status": "legacy",
          "title": "Give WorldPageId a field accessor and delete the eleven hand-written unwrappers"
        },
        "1527": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "cursor:docs/project_review_boundaries.md",
            "report:docs/project_review_2007-1456.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-08-22T00:52:11Z",
          "report": "docs/project_review_2007-1456.md",
          "status": "legacy",
          "title": "Assert the value GLFW's clock setter was given"
        },
        "1528": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "cursor:docs/project_review_boundaries.md",
            "report:docs/project_review_2007-1456.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-08-22T01:16:23Z",
          "report": "docs/project_review_2007-1456.md",
          "status": "legacy",
          "title": "Delete the two prune functions no production path can correctly call"
        },
        "1529": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "cursor:docs/project_review_boundaries.md",
            "report:docs/project_review_2007-1456.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-08-22T01:42:52Z",
          "report": "docs/project_review_2007-1456.md",
          "status": "legacy",
          "title": "Set the per-probe acceptable-failure policy (X)"
        },
        "1530": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "cursor:docs/project_review_boundaries.md",
            "report:docs/project_review_2007-1456.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-08-22T03:37:52Z",
          "report": "docs/project_review_2007-1456.md",
          "status": "legacy",
          "title": "Run the three GPU-free specs in test-headless; record test/ as build-only"
        },
        "1531": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "cursor:docs/project_review_boundaries.md",
            "report:docs/project_review_2007-1456.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-08-22T03:59:00Z",
          "report": "docs/project_review_2007-1456.md",
          "status": "legacy",
          "title": "Prove world init really wires the location overlay (#1375)"
        },
        "1532": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "cursor:docs/project_review_boundaries.md",
            "report:docs/project_review_2007-1456.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-08-22T04:42:14Z",
          "report": "docs/project_review_2007-1456.md",
          "status": "legacy",
          "title": "Declare the power node role and rating in the building YAML"
        },
        "1533": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "cursor:docs/project_review_boundaries.md",
            "report:docs/project_review_2007-1456.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-08-22T04:16:59Z",
          "report": "docs/project_review_2007-1456.md",
          "status": "legacy",
          "title": "Replace the eleven tautological determinism assertions in Language.Generated"
        },
        "1534": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "cursor:docs/project_review_boundaries.md",
            "report:docs/project_review_2007-1456.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-08-22T05:07:30Z",
          "report": "docs/project_review_2007-1456.md",
          "status": "legacy",
          "title": "Give the eight world-render quad sites one vertex-construction helper"
        },
        "1535": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_1547-1535.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-08-22T05:24:49Z",
          "report": "docs/project_review_1547-1535.md",
          "status": "legacy",
          "title": "Pin blood texture pixels and pool placement instead of comparing calls to themselves"
        },
        "1536": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "cursor:docs/project_review_boundaries.md",
            "report:docs/project_review_1547-1535.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-08-22T11:38:42Z",
          "report": "docs/project_review_1547-1535.md",
          "status": "legacy",
          "title": "ci: run behavior probes in parallel with tests"
        },
        "1537": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "cursor:docs/project_review_boundaries.md",
            "report:docs/project_review_1547-1535.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-08-22T05:41:11Z",
          "report": "docs/project_review_1547-1535.md",
          "status": "legacy",
          "title": "Pin the location-instance identity mapping instead of comparing it to itself"
        },
        "1538": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "cursor:docs/project_review_boundaries.md",
            "report:docs/project_review_1547-1535.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-08-22T06:07:03Z",
          "report": "docs/project_review_1547-1535.md",
          "status": "legacy",
          "title": "Narrow the over-wide src/World/ export lists"
        },
        "1540": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "cursor:docs/project_review_boundaries.md",
            "report:docs/project_review_1547-1535.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-08-22T06:30:22Z",
          "report": "docs/project_review_1547-1535.md",
          "status": "legacy",
          "title": "Move the shared physics constants out of the injury model and narrow the Unit/Combat export lists"
        },
        "1541": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "cursor:docs/project_review_boundaries.md",
            "report:docs/project_review_1547-1535.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-08-22T06:49:59Z",
          "report": "docs/project_review_1547-1535.md",
          "status": "legacy",
          "title": "Replace the climate self-comparison with a finiteness check and one absolute anchor"
        },
        "1542": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "cursor:docs/project_review_boundaries.md",
            "report:docs/project_review_1547-1535.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-08-22T07:09:23Z",
          "report": "docs/project_review_1547-1535.md",
          "status": "legacy",
          "title": "Start playtest budgets after the first player-ready frame (#1539)"
        },
        "1543": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "cursor:docs/project_review_boundaries.md",
            "report:docs/project_review_1547-1535.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-08-22T07:38:30Z",
          "report": "docs/project_review_1547-1535.md",
          "status": "legacy",
          "title": "Name the three focus systems in all five focus modules and rename the two shell-focus ones"
        },
        "1544": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "cursor:docs/project_review_boundaries.md",
            "report:docs/project_review_1547-1535.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-08-22T07:55:24Z",
          "report": "docs/project_review_1547-1535.md",
          "status": "legacy",
          "title": "Test the flora lifespan mixer by field sensitivity, not against itself"
        },
        "1545": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "cursor:docs/project_review_boundaries.md",
            "report:docs/project_review_1547-1535.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-08-22T08:15:13Z",
          "report": "docs/project_review_1547-1535.md",
          "status": "legacy",
          "title": "Characterize probe measurement under concurrency and RTS overrides (#1427)"
        },
        "1546": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "cursor:docs/project_review_boundaries.md",
            "report:docs/project_review_1547-1535.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-08-22T12:43:22Z",
          "report": "docs/project_review_1547-1535.md",
          "status": "legacy",
          "title": "ci: keep docs-only checks out of Cabal"
        },
        "1547": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_1547-1535.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-08-22T13:02:42Z",
          "report": "docs/project_review_1547-1535.md",
          "status": "legacy",
          "title": "locations: give the bounds validity rule one home, drop three unused exports"
        },
        "1548": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "cursor:docs/project_review_boundaries.md",
            "report:docs/project_review_2007-1456.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-08-22T13:26:25Z",
          "report": "docs/project_review_2007-1456.md",
          "status": "legacy",
          "title": "refactor(world): route chunk derivation through globalToChunk (#1113)"
        },
        "1549": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "cursor:docs/project_review_boundaries.md",
            "report:docs/project_review_2007-1456.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-08-22T13:46:12Z",
          "report": "docs/project_review_2007-1456.md",
          "status": "legacy",
          "title": "tools: generate and audit the manual-only probe census page (#1431)"
        },
        "155": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_167-80.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-06-26T23:44:05Z",
          "report": "docs/project_review_167-80.md",
          "status": "legacy",
          "title": "Fix #115: ground-item info shows instance weight, not def weight"
        },
        "1550": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "cursor:docs/project_review_boundaries.md",
            "report:docs/project_review_2007-1456.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-08-22T17:37:52Z",
          "report": "docs/project_review_2007-1456.md",
          "status": "legacy",
          "title": "Claim a probe atomically so parallel deflake agents do not collide"
        },
        "1551": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "cursor:docs/project_review_boundaries.md",
            "report:docs/project_review_2007-1456.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-08-22T14:07:52Z",
          "report": "docs/project_review_2007-1456.md",
          "status": "legacy",
          "title": "refactor: move baseTileW/baseTileH into World.Grid"
        },
        "1552": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "cursor:docs/project_review_boundaries.md",
            "report:docs/project_review_2007-1456.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-08-22T14:24:33Z",
          "report": "docs/project_review_2007-1456.md",
          "status": "legacy",
          "title": "test: replace the four tautological determinism assertions in the unit and blood helper suites"
        },
        "1553": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "cursor:docs/project_review_boundaries.md",
            "report:docs/project_review_2007-1456.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-08-22T16:15:28Z",
          "report": "docs/project_review_2007-1456.md",
          "status": "legacy",
          "title": "test: replace the three tautological language-suite assertions"
        },
        "1554": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "cursor:docs/project_review_boundaries.md",
            "report:docs/project_review_2007-1456.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-08-22T16:37:55Z",
          "report": "docs/project_review_2007-1456.md",
          "status": "legacy",
          "title": "ci: rotate project cache every eight changes"
        },
        "1555": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "cursor:docs/project_review_boundaries.md",
            "report:docs/project_review_2007-1456.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-08-22T16:58:44Z",
          "report": "docs/project_review_2007-1456.md",
          "status": "legacy",
          "title": "Correct shutdownEngine's stale Vulkan-safety comment about worker shutdown order"
        },
        "1556": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "cursor:docs/project_review_boundaries.md",
            "report:docs/project_review_2007-1456.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-08-22T17:11:29Z",
          "report": "docs/project_review_2007-1456.md",
          "status": "legacy",
          "title": "Pin the negative-context loot draw and delete the return-type tautology"
        },
        "1557": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "cursor:docs/project_review_boundaries.md",
            "report:docs/project_review_2007-1456.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-08-22T17:24:07Z",
          "report": "docs/project_review_2007-1456.md",
          "status": "legacy",
          "title": "Delete the building spawn/preview self-comparison and correct its overstated comment"
        },
        "1558": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "cursor:docs/project_review_boundaries.md",
            "report:docs/project_review_2007-1456.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-08-22T17:50:24Z",
          "report": "docs/project_review_2007-1456.md",
          "status": "legacy",
          "title": "Delete the guaranteed-placement self-comparison and its false parity comment"
        },
        "1559": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "cursor:docs/project_review_boundaries.md",
            "report:docs/project_review_2007-1456.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-08-22T18:03:35Z",
          "report": "docs/project_review_2007-1456.md",
          "status": "legacy",
          "title": "Replace the two tautological determinism assertions in the visual-helper suites"
        },
        "1560": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "cursor:docs/project_review_boundaries.md",
            "report:docs/project_review_2007-1456.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-08-22T18:32:11Z",
          "report": "docs/project_review_2007-1456.md",
          "status": "legacy",
          "title": "Correct logEntryWith's Haddock reference to the removed call-site skip list"
        },
        "1561": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "cursor:docs/project_review_boundaries.md",
            "report:docs/project_review_2007-1456.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-08-22T18:48:17Z",
          "report": "docs/project_review_2007-1456.md",
          "status": "legacy",
          "title": "Point the enum-reorder mitigation at per-component migration"
        },
        "1562": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "cursor:docs/project_review_boundaries.md",
            "report:docs/project_review_2007-1456.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-08-22T19:07:29Z",
          "report": "docs/project_review_2007-1456.md",
          "status": "legacy",
          "title": "Align the three stale hydrology source comments with the authoritative pipeline map"
        },
        "1563": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "cursor:docs/project_review_boundaries.md",
            "report:docs/project_review_2007-1456.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-08-22T19:24:13Z",
          "report": "docs/project_review_2007-1456.md",
          "status": "legacy",
          "title": "Pin one location-name vector and delete the naming self-comparison"
        },
        "1564": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "cursor:docs/project_review_boundaries.md",
            "report:docs/project_review_2007-1456.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-08-22T19:45:04Z",
          "report": "docs/project_review_2007-1456.md",
          "status": "legacy",
          "title": "Narrow the over-wide src/Blood, src/Language, and src/Item export lists"
        },
        "1565": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "cursor:docs/project_review_boundaries.md",
            "report:docs/project_review_2007-1456.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-08-22T19:59:23Z",
          "report": "docs/project_review_2007-1456.md",
          "status": "legacy",
          "title": "Select the next probe by the priority ladder (#1435)"
        },
        "1566": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "cursor:docs/project_review_boundaries.md",
            "report:docs/project_review_2007-1456.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-08-22T21:42:07Z",
          "report": "docs/project_review_2007-1456.md",
          "status": "legacy",
          "title": "Delete the river-identity self-comparison and its unachievable stability comment"
        },
        "1567": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "cursor:docs/project_review_boundaries.md",
            "report:docs/project_review_2007-1456.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-08-22T21:59:14Z",
          "report": "docs/project_review_2007-1456.md",
          "status": "legacy",
          "title": "Correct consonantOnly's stale claim that bound-form legality shares its cluster scope"
        },
        "1568": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "cursor:docs/project_review_boundaries.md",
            "report:docs/project_review_2007-1456.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-08-22T22:23:50Z",
          "report": "docs/project_review_2007-1456.md",
          "status": "legacy",
          "title": "Define tshow once in UPrelude and replace the hand-written T.pack (show x) sites"
        },
        "1574": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "cursor:docs/project_review_boundaries.md",
            "report:docs/project_review_2007-1456.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-08-22T22:46:18Z",
          "report": "docs/project_review_2007-1456.md",
          "status": "legacy",
          "title": "/deflake: select, claim, measure, record, release (#1436)"
        },
        "16": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_32-14.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-02-12T04:26:32Z",
          "report": "docs/project_review_32-14.md",
          "status": "legacy",
          "title": "Refactor World.Geology into focused submodules and remove dead code"
        },
        "1601": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "cursor:docs/project_review_boundaries.md",
            "report:docs/project_review_2007-1456.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-08-23T08:42:09Z",
          "report": "docs/project_review_2007-1456.md",
          "status": "legacy",
          "title": "tools: rename the five river diagnostics off the test_* prefix"
        },
        "1606": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "cursor:docs/project_review_boundaries.md",
            "report:docs/project_review_2007-1456.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-08-23T09:38:07Z",
          "report": "docs/project_review_2007-1456.md",
          "status": "legacy",
          "title": "Carry the source world page through popup coordinates so a replayed event cannot pan the wrong world"
        },
        "1614": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_1630-1614.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-08-23T10:01:52Z",
          "report": "docs/project_review_1630-1614.md",
          "status": "legacy",
          "title": "Resolve unit movement, re-ground, and wound infection from the unit's own world page"
        },
        "1615": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "cursor:docs/project_review_boundaries.md",
            "report:docs/project_review_1630-1614.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-08-23T10:23:43Z",
          "report": "docs/project_review_1630-1614.md",
          "status": "legacy",
          "title": "fix(worldgen): exclude indestructible neighbours from final-age soil shed credit"
        },
        "1619": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "cursor:docs/project_review_boundaries.md",
            "report:docs/project_review_1630-1614.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-08-23T10:42:25Z",
          "report": "docs/project_review_1630-1614.md",
          "status": "legacy",
          "title": "tools: reserve each parallel probe's full port span and honour --port with --jobs"
        },
        "162": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_167-80.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-06-27T13:07:02Z",
          "report": "docs/project_review_167-80.md",
          "status": "legacy",
          "title": "Fix #105: item tooltips show per-instance sharpness, not def base"
        },
        "1622": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "cursor:docs/project_review_boundaries.md",
            "report:docs/project_review_1630-1614.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-08-23T11:05:38Z",
          "report": "docs/project_review_1630-1614.md",
          "status": "legacy",
          "title": "fix: reject a fluid writeback computed before the live edit it would overwrite"
        },
        "1623": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "cursor:docs/project_review_boundaries.md",
            "report:docs/project_review_1630-1614.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-08-23T14:04:14Z",
          "report": "docs/project_review_1630-1614.md",
          "status": "legacy",
          "title": "Restore the lenient-UTF-8 sweep in unit.moveTo and gate it in CI"
        },
        "1624": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "cursor:docs/project_review_boundaries.md",
            "report:docs/project_review_1642-1631.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-08-23T19:02:13Z",
          "report": "docs/project_review_1642-1631.md",
          "status": "legacy",
          "title": "Bind one build placement to the page its click hit-tested (#1602)"
        },
        "1625": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "cursor:docs/project_review_boundaries.md",
            "report:docs/project_review_1630-1614.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-08-23T13:14:27Z",
          "report": "docs/project_review_1630-1614.md",
          "status": "legacy",
          "title": "fix: key the nested item-contents signature on child quality and weight"
        },
        "1626": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "cursor:docs/project_review_boundaries.md",
            "report:docs/project_review_1630-1614.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-08-23T14:22:05Z",
          "report": "docs/project_review_1630-1614.md",
          "status": "legacy",
          "title": "fix: map negative-infinite step costs to the ceiling instead of a free step"
        },
        "1627": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "cursor:docs/project_review_boundaries.md",
            "report:docs/project_review_1630-1614.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-08-23T14:43:48Z",
          "report": "docs/project_review_1630-1614.md",
          "status": "legacy",
          "title": "Restore the player's chosen world speed after any pause (#1599)"
        },
        "1628": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "cursor:docs/project_review_boundaries.md",
            "report:docs/project_review_1630-1614.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-08-23T15:05:12Z",
          "report": "docs/project_review_1630-1614.md",
          "status": "legacy",
          "title": "fix: clear the pending popup queue on teardown even when no card is active"
        },
        "1629": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "cursor:docs/project_review_boundaries.md",
            "report:docs/project_review_1630-1614.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-08-23T15:23:38Z",
          "report": "docs/project_review_1630-1614.md",
          "status": "legacy",
          "title": "Refuse a construction designation on an already-designated tile"
        },
        "163": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_199-166.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-06-27T13:40:51Z",
          "report": "docs/project_review_199-166.md",
          "status": "legacy",
          "title": "Fix #98: millisecond-precision save timestamps for correct recency ordering"
        },
        "1630": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_1630-1614.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-08-23T16:19:04Z",
          "report": "docs/project_review_1630-1614.md",
          "status": "legacy",
          "title": "Isolate concurrent probe launches from the shared Cabal build directory"
        },
        "1631": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_1642-1631.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-08-23T17:10:57Z",
          "report": "docs/project_review_1642-1631.md",
          "status": "legacy",
          "title": "Reconcile every persisted unit-AI reference family at the post-load boundary"
        },
        "1632": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "cursor:docs/project_review_boundaries.md",
            "report:docs/project_review_1630-1614.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-08-23T15:58:48Z",
          "report": "docs/project_review_1630-1614.md",
          "status": "legacy",
          "title": "Tie the four location probes' save/load fixtures to their own requests and isolated roots"
        },
        "1633": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "cursor:docs/project_review_boundaries.md",
            "report:docs/project_review_1642-1631.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-08-23T19:31:36Z",
          "report": "docs/project_review_1642-1631.md",
          "status": "legacy",
          "title": "Clear session-owned Lua entity tables at one declared Exit-to-Menu boundary"
        },
        "1634": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "cursor:docs/project_review_boundaries.md",
            "report:docs/project_review_1642-1631.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-08-23T17:32:36Z",
          "report": "docs/project_review_1642-1631.md",
          "status": "legacy",
          "title": "Fail baseline capture instead of recording an arbitrary sample of a varying strict invariant"
        },
        "1635": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "cursor:docs/project_review_boundaries.md",
            "report:docs/project_review_1642-1631.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-08-23T19:57:12Z",
          "report": "docs/project_review_1642-1631.md",
          "status": "legacy",
          "title": "Cover Engine.Core.Queue's blocking read and timeout behavior"
        },
        "1636": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "cursor:docs/project_review_boundaries.md",
            "report:docs/project_review_1642-1631.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-08-23T20:18:06Z",
          "report": "docs/project_review_1642-1631.md",
          "status": "legacy",
          "title": "Restore executable coverage for the settings Revert contract and retire its stale offline harness"
        },
        "1637": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "cursor:docs/project_review_boundaries.md",
            "report:docs/project_review_1642-1631.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-08-23T20:39:12Z",
          "report": "docs/project_review_1642-1631.md",
          "status": "legacy",
          "title": "Add an exact-instance player drink gesture for coffee consumables"
        },
        "1638": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "cursor:docs/project_review_boundaries.md",
            "report:docs/project_review_1642-1631.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-08-23T20:57:57Z",
          "report": "docs/project_review_1642-1631.md",
          "status": "legacy",
          "title": "Re-evidence the CI probe-gate demotions inherited from direct commit b09c1518"
        },
        "1639": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "cursor:docs/project_review_boundaries.md",
            "report:docs/project_review_1642-1631.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-08-23T21:17:40Z",
          "report": "docs/project_review_1642-1631.md",
          "status": "legacy",
          "title": "Scale auto-harvest by the farming skill (#1582)"
        },
        "164": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_167-80.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-06-27T13:13:50Z",
          "report": "docs/project_review_167-80.md",
          "status": "legacy",
          "title": "Fix #108: settings Revert restores saved tooltip dwell/hint values"
        },
        "1640": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "cursor:docs/project_review_boundaries.md",
            "report:docs/project_review_1642-1631.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-08-23T21:38:07Z",
          "report": "docs/project_review_1642-1631.md",
          "status": "legacy",
          "title": "Select the graphical device spec's adapter with pickPhysicalDevice instead of enumeration order"
        },
        "1641": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "cursor:docs/project_review_boundaries.md",
            "report:docs/project_review_1642-1631.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-08-23T22:33:10Z",
          "report": "docs/project_review_1642-1631.md",
          "status": "legacy",
          "title": "Correct the freshwater slope flatten's documented condition and cover every case it catches"
        },
        "1642": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_1642-1631.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-08-23T22:47:50Z",
          "report": "docs/project_review_1642-1631.md",
          "status": "legacy",
          "title": "Fix the save-pause probe's vacuous resumed-speed oracle"
        },
        "1643": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_1655-1643.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-08-23T23:01:15Z",
          "report": "docs/project_review_1655-1643.md",
          "status": "legacy",
          "title": "Tie the item-instance probe's save/load round trip to its own request and an isolated root"
        },
        "1644": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "cursor:docs/project_review_boundaries.md",
            "report:docs/project_review_1655-1643.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-08-23T23:15:52Z",
          "report": "docs/project_review_1655-1643.md",
          "status": "legacy",
          "title": "Judge the blood lifecycle probe's save-load path by ownership"
        },
        "1645": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "cursor:docs/project_review_boundaries.md",
            "report:docs/project_review_1655-1643.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-08-23T23:30:42Z",
          "report": "docs/project_review_1655-1643.md",
          "status": "legacy",
          "title": "Make the location-stamp probe prove its footprint materialized (#1575)"
        },
        "1646": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "cursor:docs/project_review_boundaries.md",
            "report:docs/project_review_1655-1643.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-08-23T23:45:16Z",
          "report": "docs/project_review_1655-1643.md",
          "status": "legacy",
          "title": "Give the graphical GLFW spec project-owned window coverage and label its environment checks"
        },
        "1647": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "cursor:docs/project_review_boundaries.md",
            "report:docs/project_review_1655-1643.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-08-24T00:07:39Z",
          "report": "docs/project_review_1655-1643.md",
          "status": "legacy",
          "title": "Tie the farm-AI and flora-growth save/load round trips to their own requests and isolated roots"
        },
        "1648": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "cursor:docs/project_review_boundaries.md",
            "report:docs/project_review_1655-1643.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-08-24T01:55:57Z",
          "report": "docs/project_review_1655-1643.md",
          "status": "legacy",
          "title": "Tie the item-temperature probe's save/load round trip to its own request and an isolated root"
        },
        "1649": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "cursor:docs/project_review_boundaries.md",
            "report:docs/project_review_1655-1643.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-08-24T02:10:37Z",
          "report": "docs/project_review_1655-1643.md",
          "status": "legacy",
          "title": "Give the embark probe an isolated resource root and remove every artifact it creates"
        },
        "165": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_167-80.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-06-27T13:20:56Z",
          "report": "docs/project_review_167-80.md",
          "status": "legacy",
          "title": "Fix #106: reset registered modules to fresh state when absent from save"
        },
        "1650": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "cursor:docs/project_review_boundaries.md",
            "report:docs/project_review_1655-1643.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-08-24T02:24:02Z",
          "report": "docs/project_review_1655-1643.md",
          "status": "legacy",
          "title": "Give the construction footprint probe a site it can actually render on"
        },
        "1651": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "cursor:docs/project_review_boundaries.md",
            "report:docs/project_review_1655-1643.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-08-24T04:08:17Z",
          "report": "docs/project_review_1655-1643.md",
          "status": "legacy",
          "title": "Fail the etymology probe when a required entity is absent"
        },
        "1652": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "cursor:docs/project_review_boundaries.md",
            "report:docs/project_review_1684-1656.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-08-25T00:52:55Z",
          "report": "docs/project_review_1684-1656.md",
          "status": "legacy",
          "title": "Decide mechanically whether a measured flake is the probe's fault"
        },
        "1653": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "cursor:docs/project_review_boundaries.md",
            "report:docs/project_review_1655-1643.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-08-24T04:22:29Z",
          "report": "docs/project_review_1655-1643.md",
          "status": "legacy",
          "title": "Gate the injury-log probe on a real fall's injury event"
        },
        "1654": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "cursor:docs/project_review_boundaries.md",
            "report:docs/project_review_1655-1643.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-08-24T13:13:12Z",
          "report": "docs/project_review_1655-1643.md",
          "status": "legacy",
          "title": "Correct the position-hold probe's unit count in its docstring, inventory and classifier"
        },
        "1655": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_1655-1643.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-08-24T13:29:46Z",
          "report": "docs/project_review_1655-1643.md",
          "status": "legacy",
          "title": "Derive item_temp_probe's rate fixture from the observed ambient (#1611)"
        },
        "1656": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_1684-1656.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-08-24T13:43:19Z",
          "report": "docs/project_review_1684-1656.md",
          "status": "legacy",
          "title": "List movement-probe courses without booting an engine"
        },
        "1657": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "cursor:docs/project_review_boundaries.md",
            "report:docs/project_review_1684-1656.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-08-24T13:57:18Z",
          "report": "docs/project_review_1684-1656.md",
          "status": "legacy",
          "title": "Give each retaliation-swap window its own staged fixture (#1578)"
        },
        "1658": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "cursor:docs/project_review_boundaries.md",
            "report:docs/project_review_1684-1656.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-08-24T14:11:57Z",
          "report": "docs/project_review_1684-1656.md",
          "status": "legacy",
          "title": "Bound tutorial objective-row glyphs against the shipped labels, and fit the rows that overran (#1581)"
        },
        "166": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_199-166.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-06-27T13:37:32Z",
          "report": "docs/project_review_199-166.md",
          "status": "legacy",
          "title": "Fix #117: locked boxless tooltips swallow clicks on their panel"
        },
        "1662": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "cursor:docs/project_review_boundaries.md",
            "report:docs/project_review_1684-1656.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-08-24T16:52:49Z",
          "report": "docs/project_review_1684-1656.md",
          "status": "legacy",
          "title": "Emit the handoff from the process that measured"
        },
        "1663": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "cursor:docs/project_review_boundaries.md",
            "report:docs/project_review_1684-1656.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-08-25T13:11:33Z",
          "report": "docs/project_review_1684-1656.md",
          "status": "legacy",
          "title": "Prove unit.injure attributes its event to the unit it wounded"
        },
        "1664": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "cursor:docs/project_review_boundaries.md",
            "report:docs/project_review_1684-1656.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-08-25T13:25:29Z",
          "report": "docs/project_review_1684-1656.md",
          "status": "legacy",
          "title": "Refuse to pass the till probe with its fluid rule unexercised"
        },
        "1665": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "cursor:docs/project_review_boundaries.md",
            "report:docs/project_review_1684-1656.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-08-25T13:50:42Z",
          "report": "docs/project_review_1684-1656.md",
          "status": "legacy",
          "title": "Tie the foraging probe's round trip to its own save, in its own root"
        },
        "167": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_167-80.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-06-27T13:36:35Z",
          "report": "docs/project_review_167-80.md",
          "status": "legacy",
          "title": "Fix #107: shell quit/exit actually quit the game"
        },
        "1677": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "cursor:docs/project_review_boundaries.md",
            "report:docs/project_review_1684-1656.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-08-25T15:42:29Z",
          "report": "docs/project_review_1684-1656.md",
          "status": "legacy",
          "title": "fix: attach a loaded zoom atlas only to the page whose cache produced it"
        },
        "1678": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "cursor:docs/project_review_boundaries.md",
            "report:docs/project_review_1684-1656.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-08-25T16:04:37Z",
          "report": "docs/project_review_1684-1656.md",
          "status": "legacy",
          "title": "fix(save): reject a decoded location instance whose stored bounds are inverted"
        },
        "168": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_199-166.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-06-27T13:38:56Z",
          "report": "docs/project_review_199-166.md",
          "status": "legacy",
          "title": "Fix #110: clear armed preview on debug_anim_panel shutdown"
        },
        "1683": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "cursor:docs/project_review_boundaries.md",
            "report:docs/project_review_1684-1656.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-08-25T16:24:54Z",
          "report": "docs/project_review_1684-1656.md",
          "status": "legacy",
          "title": "Resolve every phase of unitAi.commandPickup on the carrier's own page"
        },
        "1684": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_1684-1656.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-08-25T16:39:36Z",
          "report": "docs/project_review_1684-1656.md",
          "status": "legacy",
          "title": "Refuse diagnosis records outside a closed producer-provenance contract"
        },
        "169": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_199-166.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-06-27T14:01:13Z",
          "report": "docs/project_review_199-166.md",
          "status": "legacy",
          "title": "Gate debug anim panel on the debug overlay being active"
        },
        "1697": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "cursor:docs/project_review_boundaries.md"
          ],
          "history": [],
          "merged_at": "2026-08-25T18:04:15Z",
          "report": null,
          "status": "legacy",
          "title": "building_spawn: enter the portal's failure path on unit.spawn's -1 sentinel"
        },
        "1698": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "cursor:docs/project_review_boundaries.md"
          ],
          "history": [],
          "merged_at": "2026-08-25T18:20:36Z",
          "report": null,
          "status": "legacy",
          "title": "review-gate: decide staleness by the PR's own patch, not its file set"
        },
        "17": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_32-14.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-02-12T06:15:23Z",
          "report": "docs/project_review_32-14.md",
          "status": "legacy",
          "title": "Normalize codebase to Unicode syntax"
        },
        "170": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_199-166.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-06-27T13:59:40Z",
          "report": "docs/project_review_199-166.md",
          "status": "legacy",
          "title": "Clear mine-tool anchor on Exit to Menu (#102)"
        },
        "1700": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "cursor:docs/project_review_boundaries.md",
            "report:docs/project_review_2004-1710.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-08-26T04:05:01Z",
          "report": "docs/project_review_2004-1710.md",
          "status": "legacy",
          "title": "Refuse cross-page endpoint pairs in the four lax unit item verbs"
        },
        "171": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_199-166.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-06-27T14:25:38Z",
          "report": "docs/project_review_199-166.md",
          "status": "legacy",
          "title": "Fix #116: clear held input state on focus loss and minimize"
        },
        "1710": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "cursor:docs/project_review_boundaries.md",
            "report:docs/project_review_2004-1710.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-08-25T18:58:47Z",
          "report": "docs/project_review_2004-1710.md",
          "status": "legacy",
          "title": "fix(craft): clear a dead claimant's craft bill independently of claim eligibility"
        },
        "1725": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "cursor:docs/project_review_boundaries.md",
            "report:docs/project_review_2004-1710.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-08-26T05:22:06Z",
          "report": "docs/project_review_2004-1710.md",
          "status": "legacy",
          "title": "[flora] Create the saguaro texture set"
        },
        "1726": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "cursor:docs/project_review_boundaries.md",
            "report:docs/project_review_2004-1710.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-08-26T13:19:28Z",
          "report": "docs/project_review_2004-1710.md",
          "status": "legacy",
          "title": "fix: reserve texture handle zero as the missing-texture sentinel"
        },
        "1727": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "cursor:docs/project_review_boundaries.md",
            "report:docs/project_review_2004-1710.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-08-26T14:18:04Z",
          "report": "docs/project_review_2004-1710.md",
          "status": "legacy",
          "title": "fix: stop publishing a failed bindless registration as a loaded texture"
        },
        "1728": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "cursor:docs/project_review_boundaries.md",
            "report:docs/project_review_2004-1710.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-08-26T13:37:19Z",
          "report": "docs/project_review_2004-1710.md",
          "status": "legacy",
          "title": "Let a lunge observe its airborne phase so the landing strike can fire"
        },
        "174": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_199-166.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-06-27T14:17:16Z",
          "report": "docs/project_review_199-166.md",
          "status": "legacy",
          "title": "Stop hidden HUD from pushing world hover state (#153)"
        },
        "1741": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "cursor:docs/project_review_boundaries.md",
            "report:docs/project_review_2004-1710.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-08-26T18:08:06Z",
          "report": "docs/project_review_2004-1710.md",
          "status": "legacy",
          "title": "flora: integrate saguaro into world generation"
        },
        "1742": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "cursor:docs/project_review_boundaries.md",
            "report:docs/project_review_2004-1710.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-08-26T18:30:05Z",
          "report": "docs/project_review_2004-1710.md",
          "status": "legacy",
          "title": "Give player events a stable sequence so the playtest oracle cannot silently lose rows"
        },
        "1749": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "cursor:docs/project_review_boundaries.md",
            "report:docs/project_review_2004-1710.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-08-26T19:31:52Z",
          "report": "docs/project_review_2004-1710.md",
          "status": "legacy",
          "title": "Retract the staged structure piece when the world thread declines its placement"
        },
        "1751": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "cursor:docs/project_review_boundaries.md",
            "report:docs/project_review_2004-1710.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-08-26T20:37:28Z",
          "report": "docs/project_review_2004-1710.md",
          "status": "legacy",
          "title": "Build the cached world-quad pass from the snapshot it is stamped with"
        },
        "1753": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "cursor:docs/project_review_boundaries.md",
            "report:docs/project_review_2004-1710.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-08-26T22:29:02Z",
          "report": "docs/project_review_2004-1710.md",
          "status": "legacy",
          "title": "Capture a deferred mouse gesture's framebuffer press position at press"
        },
        "1754": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "cursor:docs/project_review_boundaries.md",
            "report:docs/project_review_2003-1754.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-08-27T00:30:07Z",
          "report": "docs/project_review_2003-1754.md",
          "status": "legacy",
          "title": "Enforce the one-positive-nutrition-mode invariant on food item definitions"
        },
        "1755": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "cursor:docs/project_review_boundaries.md",
            "report:docs/project_review_2003-1754.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-08-27T02:11:28Z",
          "report": "docs/project_review_2003-1754.md",
          "status": "legacy",
          "title": "Request swapchain recreation when the framebuffer size changes"
        },
        "1756": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "cursor:docs/project_review_boundaries.md",
            "report:docs/project_review_2003-1754.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-08-27T02:51:22Z",
          "report": "docs/project_review_2003-1754.md",
          "status": "legacy",
          "title": "Validate each decoded allocator's own floor, not only the ids beneath it"
        },
        "1764": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "cursor:docs/project_review_boundaries.md",
            "report:docs/project_review_2003-1754.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-08-27T04:53:49Z",
          "report": "docs/project_review_2003-1754.md",
          "status": "legacy",
          "title": "Let an in-flight pose transition finish a stop instead of erasing it"
        },
        "1773": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "cursor:docs/project_review_boundaries.md",
            "report:docs/project_review_2003-1754.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-08-27T12:30:26Z",
          "report": "docs/project_review_2003-1754.md",
          "status": "legacy",
          "title": "Keep mental effectiveness finite so NaN XP cannot bias combat or persist as item quality"
        },
        "1774": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "cursor:docs/project_review_boundaries.md",
            "report:docs/project_review_2003-1754.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-08-27T13:54:52Z",
          "report": "docs/project_review_2003-1754.md",
          "status": "legacy",
          "title": "Give every attached UI element exactly one structural owner"
        },
        "1775": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "cursor:docs/project_review_boundaries.md",
            "report:docs/project_review_2003-1754.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-08-27T14:54:06Z",
          "report": "docs/project_review_2003-1754.md",
          "status": "legacy",
          "title": "Require the whole fixed bindless descriptor binding from every accepted device"
        },
        "1776": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "cursor:docs/project_review_boundaries.md",
            "report:docs/project_review_2003-1754.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-08-27T13:03:36Z",
          "report": "docs/project_review_2003-1754.md",
          "status": "legacy",
          "title": "Dedup and count chunk-queue requests under one canonical seam identity"
        },
        "1777": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "cursor:docs/project_review_boundaries.md",
            "report:docs/project_review_2003-1754.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-08-27T14:32:42Z",
          "report": "docs/project_review_2003-1754.md",
          "status": "legacy",
          "title": "fix: refuse a texture handle the shader cannot resolve instead of reporting it loaded"
        },
        "1778": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "cursor:docs/project_review_boundaries.md",
            "report:docs/project_review_2003-1754.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-08-27T13:30:18Z",
          "report": "docs/project_review_2003-1754.md",
          "status": "legacy",
          "title": "Stop a successful autosave from clearing a pause another engine source imposed while it ran"
        },
        "1779": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "cursor:docs/project_review_boundaries.md",
            "report:docs/project_review_2003-1754.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-08-27T15:18:00Z",
          "report": "docs/project_review_2003-1754.md",
          "status": "legacy",
          "title": "Mark a location stamped only when its geometry actually materialized"
        },
        "1783": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "cursor:docs/project_review_boundaries.md",
            "report:docs/project_review_2002-1783.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-08-27T15:43:15Z",
          "report": "docs/project_review_2002-1783.md",
          "status": "legacy",
          "title": "Keep the preview probe from stealing focus"
        },
        "1784": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "cursor:docs/project_review_boundaries.md",
            "report:docs/project_review_2002-1783.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-08-27T16:08:03Z",
          "report": "docs/project_review_2002-1783.md",
          "status": "legacy",
          "title": "Refuse a portal spawn whose page stopped being active after the tick's snapshot"
        },
        "179": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_199-166.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-06-27T14:45:38Z",
          "report": "docs/project_review_199-166.md",
          "status": "legacy",
          "title": "Guard projection math against zero-size window/framebuffer (#118)"
        },
        "1792": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "cursor:docs/project_review_boundaries.md",
            "report:docs/project_review_2002-1783.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-08-27T16:23:16Z",
          "report": "docs/project_review_2002-1783.md",
          "status": "legacy",
          "title": "Give long save migration probe its own timeout"
        },
        "1794": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "cursor:docs/project_review_boundaries.md",
            "report:docs/project_review_2002-1783.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-08-27T16:47:08Z",
          "report": "docs/project_review_2002-1783.md",
          "status": "legacy",
          "title": "Rotate structure wall identity with the camera (#1712)"
        },
        "1795": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "cursor:docs/project_review_boundaries.md",
            "report:docs/project_review_2002-1783.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-08-27T17:06:45Z",
          "report": "docs/project_review_2002-1783.md",
          "status": "legacy",
          "title": "Stop the craft-bill probe racing the auto-haul, and pin the craft identity contract"
        },
        "1797": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "cursor:docs/project_review_boundaries.md",
            "report:docs/project_review_2002-1783.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-08-27T17:30:24Z",
          "report": "docs/project_review_2002-1783.md",
          "status": "legacy",
          "title": "Establish a finite positive domain for material move_cost (#1734)"
        },
        "1798": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "cursor:docs/project_review_boundaries.md",
            "report:docs/project_review_2002-1783.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-08-27T17:53:10Z",
          "report": "docs/project_review_2002-1783.md",
          "status": "legacy",
          "title": "Generate an arena base from the seed it records (#1718)"
        },
        "1799": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "cursor:docs/project_review_boundaries.md",
            "report:docs/project_review_2002-1783.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-08-27T18:13:14Z",
          "report": "docs/project_review_2002-1783.md",
          "status": "legacy",
          "title": "Make F3 click correlation honour modal scope and pointer-blocking occlusion"
        },
        "1800": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "cursor:docs/project_review_boundaries.md",
            "report:docs/project_review_2002-1783.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-08-27T18:34:15Z",
          "report": "docs/project_review_2002-1783.md",
          "status": "legacy",
          "title": "Release loaded texture atlases at shutdown before the Vulkan device is destroyed"
        },
        "1801": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "cursor:docs/project_review_boundaries.md",
            "report:docs/project_review_2002-1783.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-08-27T18:58:23Z",
          "report": "docs/project_review_2002-1783.md",
          "status": "legacy",
          "title": "Give Lua tick intervals a finite-value policy the scheduler can honour"
        },
        "1802": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "cursor:docs/project_review_boundaries.md",
            "report:docs/project_review_2002-1783.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-08-27T19:18:07Z",
          "report": "docs/project_review_2002-1783.md",
          "status": "legacy",
          "title": "Derive a consumable sip's effects from the drain the engine actually applied"
        },
        "1803": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "cursor:docs/project_review_boundaries.md"
          ],
          "history": [],
          "merged_at": "2026-08-27T19:34:25Z",
          "report": null,
          "status": "legacy",
          "title": "Apply the facing-aware wrap offset to structure quads at the cylindrical seam"
        },
        "1804": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "cursor:docs/project_review_boundaries.md"
          ],
          "history": [],
          "merged_at": "2026-08-27T19:49:13Z",
          "report": null,
          "status": "legacy",
          "title": "Apply the dry-bank slope rule to wet neighbours across chunk seams"
        },
        "1805": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "cursor:docs/project_review_boundaries.md"
          ],
          "history": [],
          "merged_at": "2026-08-27T20:05:30Z",
          "report": null,
          "status": "legacy",
          "title": "Reject non-finite unit.repairItem deltas instead of silently breaking or free-repairing an item"
        },
        "1806": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "cursor:docs/project_review_boundaries.md"
          ],
          "history": [],
          "merged_at": "2026-08-27T20:22:20Z",
          "report": null,
          "status": "legacy",
          "title": "Validate the structure placement target before interning its texture paths"
        },
        "1807": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "cursor:docs/project_review_boundaries.md"
          ],
          "history": [],
          "merged_at": "2026-08-27T20:39:28Z",
          "report": null,
          "status": "legacy",
          "title": "Resolve combat's max_stamina through effective stats so equipped and innate modifiers apply"
        },
        "1808": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "cursor:docs/project_review_boundaries.md"
          ],
          "history": [],
          "merged_at": "2026-08-27T20:54:35Z",
          "report": null,
          "status": "legacy",
          "title": "Invalidate pending activations on a real visible exclusivity change (#1748)"
        },
        "1809": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "cursor:docs/project_review_boundaries.md"
          ],
          "history": [],
          "merged_at": "2026-08-27T21:10:42Z",
          "report": null,
          "status": "legacy",
          "title": "Reject a quality_tiers override that cannot label every quality"
        },
        "181": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_199-166.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-06-27T14:23:25Z",
          "report": "docs/project_review_199-166.md",
          "status": "legacy",
          "title": "Consume ground-item clicks in info mode so tile-info doesn't run (#173)"
        },
        "1810": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "cursor:docs/project_review_boundaries.md"
          ],
          "history": [],
          "merged_at": "2026-08-27T21:26:34Z",
          "report": null,
          "status": "legacy",
          "title": "Make the forage flora query and lookup seam-aware at the cylindrical U wrap"
        },
        "1811": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "cursor:docs/project_review_boundaries.md"
          ],
          "history": [],
          "merged_at": "2026-08-27T21:41:00Z",
          "report": null,
          "status": "legacy",
          "title": "Make pending auto-harvest collection eligible for arbitration (#1743)"
        },
        "1812": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "cursor:docs/project_review_boundaries.md"
          ],
          "history": [],
          "merged_at": "2026-08-27T21:55:14Z",
          "report": null,
          "status": "legacy",
          "title": "Close the location content-kind vocabulary, removing nested structures (#1708)"
        },
        "1813": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "cursor:docs/project_review_boundaries.md"
          ],
          "history": [],
          "merged_at": "2026-08-27T23:43:10Z",
          "report": null,
          "status": "legacy",
          "title": "Retain a turn's drained post-step evidence when its screenshot fails"
        },
        "1814": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "cursor:docs/project_review_boundaries.md"
          ],
          "history": [],
          "merged_at": "2026-08-27T23:58:23Z",
          "report": null,
          "status": "legacy",
          "title": "Reject an out-of-domain explicit condition in item.spawnGround"
        },
        "1815": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "cursor:docs/project_review_boundaries.md"
          ],
          "history": [],
          "merged_at": "2026-08-28T00:21:01Z",
          "report": null,
          "status": "legacy",
          "title": "Apply the persisted borderless window mode at graphical startup"
        },
        "1816": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "cursor:docs/project_review_boundaries.md"
          ],
          "history": [],
          "merged_at": "2026-08-28T00:39:08Z",
          "report": null,
          "status": "legacy",
          "title": "Report a commanded move order abandoned by its stall budget (#1769)"
        },
        "1817": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "cursor:docs/project_review_boundaries.md"
          ],
          "history": [],
          "merged_at": "2026-08-28T01:00:30Z",
          "report": null,
          "status": "legacy",
          "title": "Prevent authored location bounds from overflowing during instance anchoring (#1796)"
        },
        "1818": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "cursor:docs/project_review_boundaries.md"
          ],
          "history": [],
          "merged_at": "2026-08-28T01:20:49Z",
          "report": null,
          "status": "legacy",
          "title": "Clear both deferred selection arms on a direct tile selection"
        },
        "1819": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "cursor:docs/project_review_boundaries.md"
          ],
          "history": [],
          "merged_at": "2026-08-28T01:42:36Z",
          "report": null,
          "status": "legacy",
          "title": "Reject a non-positive flora regrowth_time instead of accepting an infinite harvest loop"
        },
        "1820": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "cursor:docs/project_review_boundaries.md"
          ],
          "history": [],
          "merged_at": "2026-08-28T01:56:09Z",
          "report": null,
          "status": "legacy",
          "title": "Pin UI.Clipping's UI scale and isolate its resource root (#1747)"
        },
        "1821": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "cursor:docs/project_review_boundaries.md"
          ],
          "history": [],
          "merged_at": "2026-08-28T12:46:25Z",
          "report": null,
          "status": "legacy",
          "title": "Reject a non-positive location-content count or rolls instead of spawning nothing silently"
        },
        "1822": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "cursor:docs/project_review_boundaries.md"
          ],
          "history": [],
          "merged_at": "2026-08-28T02:11:52Z",
          "report": null,
          "status": "legacy",
          "title": "Bound the #418 front-wall billboard lift to the slice the renderer draws"
        },
        "1823": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "cursor:docs/project_review_boundaries.md"
          ],
          "history": [],
          "merged_at": "2026-08-28T13:00:05Z",
          "report": null,
          "status": "legacy",
          "title": "Reject unusable LLM flavor output instead of recording it as a flavored persona"
        },
        "1824": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "cursor:docs/project_review_boundaries.md"
          ],
          "history": [],
          "merged_at": "2026-08-28T13:15:06Z",
          "report": null,
          "status": "legacy",
          "title": "Preserve keyboard control focus across the Defaults and preview-arrival rebuilds"
        },
        "1825": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "cursor:docs/project_review_boundaries.md"
          ],
          "history": [],
          "merged_at": "2026-08-28T13:30:15Z",
          "report": null,
          "status": "legacy",
          "title": "Scan the unit/building managers and UnitThreadState in the persistence inventory audit"
        },
        "1826": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "cursor:docs/project_review_boundaries.md"
          ],
          "history": [],
          "merged_at": "2026-08-28T13:46:29Z",
          "report": null,
          "status": "legacy",
          "title": "Pin the shipped concept catalogue's id inventory against removal, rename and unratcheted addition"
        },
        "1827": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "cursor:docs/project_review_boundaries.md",
            "report:docs/project_review_2000-1827.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-08-28T14:01:30Z",
          "report": "docs/project_review_2000-1827.md",
          "status": "legacy",
          "title": "Clear the global unit selection unconditionally on the gameplay Escape"
        },
        "1828": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "cursor:docs/project_review_boundaries.md",
            "report:docs/project_review_2000-1827.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-08-28T14:37:04Z",
          "report": "docs/project_review_2000-1827.md",
          "status": "legacy",
          "title": "Make the texture-path checker comment-aware and run it as a blocking gate"
        },
        "1829": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "cursor:docs/project_review_boundaries.md",
            "report:docs/project_review_2000-1827.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-08-28T14:19:11Z",
          "report": "docs/project_review_2000-1827.md",
          "status": "legacy",
          "title": "Decode world.getFluidAt by its arity contract in the probes that read it as a table"
        },
        "1830": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "cursor:docs/project_review_boundaries.md",
            "report:docs/project_review_2000-1827.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-08-28T14:50:33Z",
          "report": "docs/project_review_2000-1827.md",
          "status": "legacy",
          "title": "Bound the ntfy notification job and its curl calls in elapsed time"
        },
        "1831": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "cursor:docs/project_review_boundaries.md",
            "report:docs/project_review_2000-1827.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-08-28T15:12:59Z",
          "report": "docs/project_review_2000-1827.md",
          "status": "legacy",
          "title": "Close the location anchor vocabulary into one type (#1681)"
        },
        "1832": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "cursor:docs/project_review_boundaries.md",
            "report:docs/project_review_2000-1827.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-08-28T15:34:37Z",
          "report": "docs/project_review_2000-1827.md",
          "status": "legacy",
          "title": "Gate the world_determinism content-identity self-test (#1724)"
        },
        "1834": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "cursor:docs/project_review_boundaries.md",
            "report:docs/project_review_1989-1834.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-08-28T15:55:23Z",
          "report": "docs/project_review_1989-1834.md",
          "status": "legacy",
          "title": "Make the EngineEnv capability inventory's field total mechanically checked"
        },
        "1835": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "cursor:docs/project_review_boundaries.md",
            "report:docs/project_review_1989-1834.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-08-28T18:07:10Z",
          "report": "docs/project_review_1989-1834.md",
          "status": "legacy",
          "title": "Re-point the F4 Layer A coverage checker at the split input modules"
        },
        "1836": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "cursor:docs/project_review_boundaries.md",
            "report:docs/project_review_1989-1834.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-08-28T17:09:07Z",
          "report": "docs/project_review_1989-1834.md",
          "status": "legacy",
          "title": "Gate bare-name panel icon maps against the global runtime index and repair fallback contracts"
        },
        "1838": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "cursor:docs/project_review_boundaries.md",
            "report:docs/project_review_1989-1834.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-08-29T00:03:15Z",
          "report": "docs/project_review_1989-1834.md",
          "status": "legacy",
          "title": "Define the non-success outcomes of a de-flake attempt (#1439)"
        },
        "1839": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "cursor:docs/project_review_boundaries.md",
            "report:docs/project_review_1989-1834.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-08-28T16:46:02Z",
          "report": "docs/project_review_1989-1834.md",
          "status": "legacy",
          "title": "Correct iiWeight's stale carried-weight comment to defer to itemTotalWeight"
        },
        "1840": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "cursor:docs/project_review_boundaries.md",
            "report:docs/project_review_1989-1834.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-08-28T17:30:01Z",
          "report": "docs/project_review_1989-1834.md",
          "status": "legacy",
          "title": "Rename the two water diagnostics out of the test_* namespace"
        },
        "1841": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "cursor:docs/project_review_boundaries.md",
            "report:docs/project_review_1989-1834.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-08-28T18:29:20Z",
          "report": "docs/project_review_1989-1834.md",
          "status": "legacy",
          "title": "Stage each probe's isolated root inside its own cleanup guard"
        },
        "1843": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "cursor:docs/project_review_boundaries.md",
            "report:docs/project_review_1989-1834.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-08-28T18:50:45Z",
          "report": "docs/project_review_1989-1834.md",
          "status": "legacy",
          "title": "Add scavenged field toolbox"
        },
        "1847": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "cursor:docs/project_review_boundaries.md",
            "report:docs/project_review_1989-1834.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-08-28T19:11:54Z",
          "report": "docs/project_review_1989-1834.md",
          "status": "legacy",
          "title": "Report CI-promotion candidates from the probe census"
        },
        "1851": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "cursor:docs/project_review_boundaries.md",
            "report:docs/project_review_1989-1834.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-08-28T19:28:30Z",
          "report": "docs/project_review_1989-1834.md",
          "status": "legacy",
          "title": "Generate ci-local.sh's step labels instead of hand-numbering them"
        },
        "1852": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "cursor:docs/project_review_boundaries.md",
            "report:docs/project_review_1989-1834.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-08-28T19:45:15Z",
          "report": "docs/project_review_1989-1834.md",
          "status": "legacy",
          "title": "Source repair targets from the ground, completing the repair sourcing ladder"
        },
        "1859": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "cursor:docs/project_review_boundaries.md",
            "report:docs/project_review_1878-1859.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-08-29T00:50:44Z",
          "report": "docs/project_review_1878-1859.md",
          "status": "legacy",
          "title": "Give the location probes a private config tree instead of a symlink to the checkout"
        },
        "1860": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "cursor:docs/project_review_boundaries.md",
            "report:docs/project_review_1878-1859.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-08-29T02:12:09Z",
          "report": "docs/project_review_1878-1859.md",
          "status": "legacy",
          "title": "Generate the thermo probe's dump world with the same plate count as its live world"
        },
        "1861": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "cursor:docs/project_review_boundaries.md",
            "report:docs/project_review_1878-1859.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-08-29T03:06:18Z",
          "report": "docs/project_review_1878-1859.md",
          "status": "legacy",
          "title": "Name the phase and nested probes active when a sweep times out"
        },
        "1862": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "cursor:docs/project_review_boundaries.md",
            "report:docs/project_review_1878-1859.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-08-29T04:08:45Z",
          "report": "docs/project_review_1878-1859.md",
          "status": "legacy",
          "title": "Follow the harvested yield's identity in farm_ai_probe phase 9"
        },
        "1863": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "cursor:docs/project_review_boundaries.md",
            "report:docs/project_review_1878-1859.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-08-29T13:36:15Z",
          "report": "docs/project_review_1878-1859.md",
          "status": "legacy",
          "title": "Require the etymology probe's forced scroll configuration to actually overflow"
        },
        "1864": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "cursor:docs/project_review_boundaries.md"
          ],
          "history": [],
          "merged_at": "2026-08-29T17:21:07Z",
          "report": null,
          "status": "legacy",
          "title": "Stop tools/README.md from carrying a hand-maintained probe count"
        },
        "1865": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "cursor:docs/project_review_boundaries.md",
            "report:docs/project_review_1878-1859.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-08-29T13:51:42Z",
          "report": "docs/project_review_1878-1859.md",
          "status": "legacy",
          "title": "Exercise the state-of-mind guard in all three consciousness bands"
        },
        "1866": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "cursor:docs/project_review_boundaries.md",
            "report:docs/project_review_1878-1859.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-08-29T14:07:06Z",
          "report": "docs/project_review_1878-1859.md",
          "status": "legacy",
          "title": "Establish an admissible pickup order in the follow-command probe before judging arbitration"
        },
        "1867": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "cursor:docs/project_review_boundaries.md",
            "report:docs/project_review_1878-1859.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-08-29T14:22:28Z",
          "report": "docs/project_review_1878-1859.md",
          "status": "legacy",
          "title": "Isolate the offscreen type-icon assertion from the discovery popup that covers it"
        },
        "187": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_199-166.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-06-27T14:32:41Z",
          "report": "docs/project_review_199-166.md",
          "status": "legacy",
          "title": "Fix #178: clear building/item selection on unit context-menu Info"
        },
        "1870": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "cursor:docs/project_review_boundaries.md",
            "report:docs/project_review_1878-1859.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-08-29T14:53:26Z",
          "report": "docs/project_review_1878-1859.md",
          "status": "legacy",
          "title": "File an issue instead of a PR when the bug is in the engine (#1438)"
        },
        "1872": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "cursor:docs/project_review_boundaries.md",
            "report:docs/project_review_1878-1859.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-08-29T14:37:40Z",
          "report": "docs/project_review_1878-1859.md",
          "status": "legacy",
          "title": "Assert the cold thought's own identity in thought_probe phase 4 (#1759)"
        },
        "1877": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "cursor:docs/project_review_boundaries.md",
            "report:docs/project_review_1878-1859.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-08-29T15:27:41Z",
          "report": "docs/project_review_1878-1859.md",
          "status": "legacy",
          "title": "Give each preview and offscreen engine boot its own retained log (#1763)"
        },
        "1878": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "cursor:docs/project_review_boundaries.md",
            "report:docs/project_review_1878-1859.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-08-29T15:43:22Z",
          "report": "docs/project_review_1878-1859.md",
          "status": "legacy",
          "title": "Require a positive preferred-soil score so plant_probe's granite zero proves soil gating"
        },
        "1879": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "cursor:docs/project_review_boundaries.md"
          ],
          "history": [],
          "merged_at": "2026-08-29T15:57:41Z",
          "report": null,
          "status": "legacy",
          "title": "Capture the worker and network state when the power-workshop AI polls time out"
        },
        "188": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_211-200.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-06-27T15:45:48Z",
          "report": "docs/project_review_211-200.md",
          "status": "legacy",
          "title": "Make structure.* APIs authoritative against lcStructures (#68)"
        },
        "1880": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "cursor:docs/project_review_boundaries.md"
          ],
          "history": [],
          "merged_at": "2026-08-29T16:31:41Z",
          "report": null,
          "status": "legacy",
          "title": "Give the flora-growth probe invocation-owned fixtures and log (#1682)"
        },
        "1881": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "cursor:docs/project_review_boundaries.md"
          ],
          "history": [],
          "merged_at": "2026-08-29T16:14:25Z",
          "report": null,
          "status": "legacy",
          "title": "Tie the embark probe's two saves to their own requests (#1746)"
        },
        "1883": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "cursor:docs/project_review_boundaries.md"
          ],
          "history": [],
          "merged_at": "2026-08-29T16:48:02Z",
          "report": null,
          "status": "legacy",
          "title": "Give the foraging probe a deterministic harvestable target instead of natural placement"
        },
        "1885": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "cursor:docs/project_review_boundaries.md"
          ],
          "history": [],
          "merged_at": "2026-08-29T17:04:41Z",
          "report": null,
          "status": "legacy",
          "title": "repair_ai_probe: judge phase 8's repair ordering from one timeline (#1767)"
        },
        "1886": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "cursor:docs/project_review_boundaries.md"
          ],
          "history": [],
          "merged_at": "2026-08-29T17:43:43Z",
          "report": null,
          "status": "legacy",
          "title": "Make exact roster-unit selection a prerequisite before the embark probe orders a move"
        },
        "1887": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "cursor:docs/project_review_boundaries.md"
          ],
          "history": [],
          "merged_at": "2026-08-29T17:59:22Z",
          "report": null,
          "status": "legacy",
          "title": "Stock the technomule's field toolbox with starter hand tools"
        },
        "1888": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "cursor:docs/project_review_boundaries.md"
          ],
          "history": [],
          "merged_at": "2026-08-29T18:15:01Z",
          "report": null,
          "status": "legacy",
          "title": "Replay preview animations continuously, whatever their authored loop (#1833)"
        },
        "1889": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "cursor:docs/project_review_boundaries.md"
          ],
          "history": [],
          "merged_at": "2026-08-29T18:30:51Z",
          "report": null,
          "status": "legacy",
          "title": "Pin and record sight conditions in the tutorial probe's pre-latched reveal"
        },
        "1891": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "cursor:docs/project_review_boundaries.md"
          ],
          "history": [],
          "merged_at": "2026-08-29T18:51:22Z",
          "report": null,
          "status": "legacy",
          "title": "Record a non-object player reply as a wait instead of crashing the turn"
        },
        "1893": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "cursor:docs/project_review_boundaries.md",
            "report:docs/project_review_1987-1893.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-08-29T19:08:07Z",
          "report": "docs/project_review_1987-1893.md",
          "status": "legacy",
          "title": "Gate world-entity clicks on the zoomed-in view (#1875)"
        },
        "1894": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "cursor:docs/project_review_boundaries.md",
            "report:docs/project_review_1987-1893.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-08-29T19:50:51Z",
          "report": "docs/project_review_1987-1893.md",
          "status": "legacy",
          "title": "Keep an added concept id from re-rooting an existing concept"
        },
        "1895": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "cursor:docs/project_review_boundaries.md",
            "report:docs/project_review_1987-1893.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-08-29T20:12:56Z",
          "report": "docs/project_review_1987-1893.md",
          "status": "legacy",
          "title": "fix(playtest): reject conflicting verdicts on one friction candidate"
        },
        "1897": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "cursor:docs/project_review_boundaries.md",
            "report:docs/project_review_1987-1893.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-08-29T20:43:35Z",
          "report": "docs/project_review_1987-1893.md",
          "status": "legacy",
          "title": "Light each visible world page from its own clock and circumference"
        },
        "1898": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "cursor:docs/project_review_boundaries.md",
            "report:docs/project_review_1987-1893.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-08-29T21:05:28Z",
          "report": "docs/project_review_1987-1893.md",
          "status": "legacy",
          "title": "docs: describe power_workshop_probe by the #590 recipe-draw model it gates"
        },
        "1899": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "cursor:docs/project_review_boundaries.md",
            "report:docs/project_review_1987-1893.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-08-29T21:31:55Z",
          "report": "docs/project_review_1987-1893.md",
          "status": "legacy",
          "title": "Resolve structure-pack piece art engine-side for unplaced pieces (#1842)"
        },
        "1900": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "cursor:docs/project_review_boundaries.md",
            "report:docs/project_review_1981-1968.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-08-30T21:45:05Z",
          "report": "docs/project_review_1981-1968.md",
          "status": "legacy",
          "title": "Add persistent nomad encounters to small ruins"
        },
        "1901": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "cursor:docs/project_review_boundaries.md",
            "report:docs/project_review_1987-1893.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-08-29T21:54:52Z",
          "report": "docs/project_review_1987-1893.md",
          "status": "legacy",
          "title": "Add primitive nomad combat animations"
        },
        "1902": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "cursor:docs/project_review_boundaries.md",
            "report:docs/project_review_1987-1893.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-08-30T14:51:19Z",
          "report": "docs/project_review_1987-1893.md",
          "status": "legacy",
          "title": "Add primitive nomad locomotion and crawling animations"
        },
        "1903": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "cursor:docs/project_review_boundaries.md",
            "report:docs/project_review_1987-1893.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-08-30T15:18:07Z",
          "report": "docs/project_review_1987-1893.md",
          "status": "legacy",
          "title": "Seed the windowed-geometry cache from a fullscreen boot's decorated window"
        },
        "1904": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "cursor:docs/project_review_boundaries.md",
            "report:docs/project_review_1987-1893.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-08-30T15:43:41Z",
          "report": "docs/project_review_1987-1893.md",
          "status": "legacy",
          "title": "Classify lava by rim containment instead of column depth in the world audit"
        },
        "1905": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "cursor:docs/project_review_boundaries.md",
            "report:docs/project_review_1986-1908.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-08-30T17:49:41Z",
          "report": "docs/project_review_1986-1908.md",
          "status": "legacy",
          "title": "Pin each capability field's writing modules in the EngineEnv capability audit"
        },
        "1906": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "cursor:docs/project_review_boundaries.md",
            "report:docs/project_review_1987-1893.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-08-30T16:58:21Z",
          "report": "docs/project_review_1987-1893.md",
          "status": "legacy",
          "title": "Give location_content_probe invocation-owned fixtures and log (#1884)"
        },
        "1908": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "cursor:docs/project_review_boundaries.md",
            "report:docs/project_review_1986-1908.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-08-30T17:34:51Z",
          "report": "docs/project_review_1986-1908.md",
          "status": "legacy",
          "title": "Add forgeable steel helmet (#1785)"
        },
        "192": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_199-166.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-06-27T14:41:37Z",
          "report": "docs/project_review_199-166.md",
          "status": "legacy",
          "title": "Consume right-clicks on ordinary clickable UI controls (#184)"
        },
        "1923": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "cursor:docs/project_review_boundaries.md",
            "report:docs/project_review_1986-1908.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-08-30T18:05:26Z",
          "report": "docs/project_review_1986-1908.md",
          "status": "legacy",
          "title": "Add centered bounded zoom to --preview asset panes"
        },
        "1936": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "cursor:docs/project_review_boundaries.md",
            "report:docs/project_review_1986-1908.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-08-30T18:21:42Z",
          "report": "docs/project_review_1986-1908.md",
          "status": "legacy",
          "title": "Author real variants for five vegetation textures"
        },
        "1942": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "cursor:docs/project_review_boundaries.md",
            "report:docs/project_review_1986-1908.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-08-30T18:38:16Z",
          "report": "docs/project_review_1986-1908.md",
          "status": "legacy",
          "title": "Make each probe's copied config removable and stop item-instance passing over surviving residue"
        },
        "1943": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "cursor:docs/project_review_boundaries.md",
            "report:docs/project_review_1986-1908.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-08-30T18:57:04Z",
          "report": "docs/project_review_1986-1908.md",
          "status": "legacy",
          "title": "Derive every ENGINE_DEBUG category name and the \"all\" set from LogCategory"
        },
        "1951": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "cursor:docs/project_review_boundaries.md",
            "report:docs/project_review_1986-1908.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-08-30T19:13:47Z",
          "report": "docs/project_review_1986-1908.md",
          "status": "legacy",
          "title": "Exclude deferred probes from the flake lab"
        },
        "1962": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "cursor:docs/project_review_boundaries.md",
            "report:docs/project_review_1986-1908.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-08-30T19:30:32Z",
          "report": "docs/project_review_1986-1908.md",
          "status": "legacy",
          "title": "Call the registered UI.setColor verb from bar.setFillColor"
        },
        "1964": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "cursor:docs/project_review_boundaries.md",
            "report:docs/project_review_1986-1908.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-08-30T19:48:38Z",
          "report": "docs/project_review_1986-1908.md",
          "status": "legacy",
          "title": "Gate the Unit Info row's selection cleanup on a successful unit selection"
        },
        "1968": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "cursor:docs/project_review_boundaries.md",
            "report:docs/project_review_1981-1968.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-08-30T20:11:29Z",
          "report": "docs/project_review_1981-1968.md",
          "status": "legacy",
          "title": "Give split input holds an ownership-safe modifier lifetime"
        },
        "1970": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "cursor:docs/project_review_boundaries.md",
            "report:docs/project_review_1981-1968.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-08-30T20:34:39Z",
          "report": "docs/project_review_1981-1968.md",
          "status": "legacy",
          "title": "Preserve the engine's canonical save order through the main menu"
        },
        "1971": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "cursor:docs/project_review_boundaries.md",
            "report:docs/project_review_1981-1968.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-08-30T20:54:18Z",
          "report": "docs/project_review_1981-1968.md",
          "status": "legacy",
          "title": "Redraw Till and Plant toolbar icons"
        },
        "1972": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "cursor:docs/project_review_boundaries.md",
            "report:docs/project_review_1981-1968.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-08-30T21:16:55Z",
          "report": "docs/project_review_1981-1968.md",
          "status": "legacy",
          "title": "Read category log thresholds from the documented ENGINE_LOG_<CATEGORY> name"
        },
        "1973": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "cursor:docs/project_review_boundaries.md",
            "report:docs/project_review_1981-1968.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-08-30T22:09:52Z",
          "report": "docs/project_review_1981-1968.md",
          "status": "legacy",
          "title": "Gate hud.update's cursor hover on gameplay input ownership (#1931)"
        },
        "1974": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "cursor:docs/project_review_boundaries.md",
            "report:docs/project_review_1981-1968.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-08-30T22:34:30Z",
          "report": "docs/project_review_1981-1968.md",
          "status": "legacy",
          "title": "Add Workbench construction progress art"
        },
        "1975": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "cursor:docs/project_review_boundaries.md",
            "report:docs/project_review_1981-1968.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-08-30T22:57:14Z",
          "report": "docs/project_review_1981-1968.md",
          "status": "legacy",
          "title": "Give every inter-thread engine queue depth, high-water, and oldest-message-age telemetry"
        },
        "1976": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "cursor:docs/project_review_boundaries.md",
            "report:docs/project_review_1981-1968.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-08-30T23:22:21Z",
          "report": "docs/project_review_1981-1968.md",
          "status": "legacy",
          "title": "Decode each save component once and derive the load phase structurally"
        },
        "1977": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "cursor:docs/project_review_boundaries.md",
            "report:docs/project_review_1981-1968.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-08-30T23:46:47Z",
          "report": "docs/project_review_1981-1968.md",
          "status": "legacy",
          "title": "Render Till as a flat level-ground surface"
        },
        "1979": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "cursor:docs/project_review_boundaries.md",
            "report:docs/project_review_1981-1968.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-08-31T00:09:31Z",
          "report": "docs/project_review_1981-1968.md",
          "status": "legacy",
          "title": "Migrate text_encoding probe to probe-result/v1"
        },
        "1981": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "cursor:docs/project_review_boundaries.md",
            "report:docs/project_review_1981-1968.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-08-31T00:35:15Z",
          "report": "docs/project_review_1981-1968.md",
          "status": "legacy",
          "title": "Stop the legacy config migration from promoting a neutral placeholder into durable local state"
        },
        "1984": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "cursor:docs/project_review_boundaries.md",
            "report:docs/project_review_1986-1908.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-08-31T00:52:04Z",
          "report": "docs/project_review_1986-1908.md",
          "status": "legacy",
          "title": "Reject a non-positive recipe count instead of loading a free-output craft"
        },
        "1985": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "cursor:docs/project_review_boundaries.md",
            "report:docs/project_review_1986-1908.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-08-31T01:11:07Z",
          "report": "docs/project_review_1986-1908.md",
          "status": "legacy",
          "title": "Place the unit hit box at the continuous Z the renderer draws"
        },
        "1986": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "cursor:docs/project_review_boundaries.md",
            "report:docs/project_review_1986-1908.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-08-31T01:26:41Z",
          "report": "docs/project_review_1986-1908.md",
          "status": "legacy",
          "title": "Derive the automatic sleep-wake boundary from the species circadian phase"
        },
        "1987": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "cursor:docs/project_review_boundaries.md",
            "report:docs/project_review_1987-1893.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-08-31T01:41:10Z",
          "report": "docs/project_review_1987-1893.md",
          "status": "legacy",
          "title": "Resolve notification overrides per field, not per category (#1938)"
        },
        "1988": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "cursor:docs/project_review_boundaries.md"
          ],
          "history": [],
          "merged_at": "2026-08-31T01:59:45Z",
          "report": null,
          "status": "legacy",
          "title": "Stop the debug console collapsing distinct Lua table keys into one JSON member"
        },
        "1989": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "cursor:docs/project_review_boundaries.md",
            "report:docs/project_review_1989-1834.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-08-31T02:15:30Z",
          "report": "docs/project_review_1989-1834.md",
          "status": "legacy",
          "title": "Retire an already-latched tutorial branch once it has been presented"
        },
        "199": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_199-166.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-06-27T14:48:39Z",
          "report": "docs/project_review_199-166.md",
          "status": "legacy",
          "title": "Fix #189: weigh actual ItemInstance in depositToCargo capacity check"
        },
        "1991": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "cursor:docs/project_review_boundaries.md",
            "report:docs/project_review_2000-1827.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-08-31T02:33:22Z",
          "report": "docs/project_review_2000-1827.md",
          "status": "legacy",
          "title": "Reject an explicitly empty --resource-root operand"
        },
        "1992": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "cursor:docs/project_review_boundaries.md",
            "report:docs/project_review_2000-1827.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-08-31T02:50:02Z",
          "report": "docs/project_review_2000-1827.md",
          "status": "legacy",
          "title": "Rank a unit's known locations in the page's cylindrical frame (#1944)"
        },
        "1993": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "cursor:docs/project_review_boundaries.md",
            "report:docs/project_review_2000-1827.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-08-31T03:07:47Z",
          "report": "docs/project_review_2000-1827.md",
          "status": "legacy",
          "title": "Reject a non-positive or non-finite loot-table weight at its decoder"
        },
        "1998": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "cursor:docs/project_review_boundaries.md",
            "report:docs/project_review_2000-1827.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-08-31T03:24:27Z",
          "report": "docs/project_review_2000-1827.md",
          "status": "legacy",
          "title": "Fit the debug console to the framebuffer width across the supported envelope"
        },
        "1999": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "cursor:docs/project_review_boundaries.md",
            "report:docs/project_review_2000-1827.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-08-31T03:41:57Z",
          "report": "docs/project_review_2000-1827.md",
          "status": "legacy",
          "title": "Fix Create World initial identity handoff"
        },
        "200": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_211-200.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-06-27T14:56:15Z",
          "report": "docs/project_review_211-200.md",
          "status": "legacy",
          "title": "Fix #190: roll back popped item on cross-manager transfer failure"
        },
        "2000": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "cursor:docs/project_review_boundaries.md",
            "report:docs/project_review_2000-1827.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-08-31T03:58:09Z",
          "report": "docs/project_review_2000-1827.md",
          "status": "legacy",
          "title": "Apply every whole-band speed multiplier to the ambient meander cap"
        },
        "2002": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "cursor:docs/project_review_boundaries.md",
            "report:docs/project_review_2002-1783.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-08-31T15:53:47Z",
          "report": "docs/project_review_2002-1783.md",
          "status": "legacy",
          "title": "Warn when swapchain format or present-mode selection falls back from the preferred capability"
        },
        "2003": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "cursor:docs/project_review_boundaries.md",
            "report:docs/project_review_2003-1754.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-08-31T16:11:41Z",
          "report": "docs/project_review_2003-1754.md",
          "status": "legacy",
          "title": "Classify a Lua chunk source before shortening it for the log prefix"
        },
        "2004": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "cursor:docs/project_review_boundaries.md",
            "report:docs/project_review_2004-1710.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-08-31T16:28:46Z",
          "report": "docs/project_review_2004-1710.md",
          "status": "legacy",
          "title": "Seed the wake-boundary baseline when a unit enters the Sleeping phase"
        },
        "2005": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "cursor:docs/project_review_boundaries.md"
          ],
          "history": [],
          "merged_at": "2026-08-31T16:46:54Z",
          "report": null,
          "status": "legacy",
          "title": "Retire camera.goToTile's obsolete glacier heap-overflow fence"
        },
        "2006": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "cursor:docs/project_review_boundaries.md"
          ],
          "history": [],
          "merged_at": "2026-08-31T17:06:31Z",
          "report": null,
          "status": "legacy",
          "title": "Make the in-game console's completion candidates match its execution sandbox"
        },
        "2007": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "cursor:docs/project_review_boundaries.md",
            "report:docs/project_review_2007-1456.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-08-31T17:24:19Z",
          "report": "docs/project_review_2007-1456.md",
          "status": "legacy",
          "title": "Add a read-only ContentRegistries view for every non-writer consumer"
        },
        "2008": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "cursor:docs/project_review_boundaries.md"
          ],
          "history": [],
          "merged_at": "2026-08-31T17:43:22Z",
          "report": null,
          "status": "legacy",
          "title": "Replace tomato placeholders with approved PixelLab art"
        },
        "2009": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [],
          "history": [],
          "merged_at": "2026-08-31T18:02:22Z",
          "report": null,
          "status": "never-reviewed",
          "title": "Couple scene-text cache entries to their scene nodes' lifetimes"
        },
        "201": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_211-200.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-06-27T14:58:54Z",
          "report": "docs/project_review_211-200.md",
          "status": "legacy",
          "title": "Fix #194: clear unit_resources per-uid caches on save-load"
        },
        "2010": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [],
          "history": [],
          "merged_at": "2026-08-31T18:25:47Z",
          "report": null,
          "status": "never-reviewed",
          "title": "Prepare a directly-invoked probe's engine outside the READY deadline (#1913)"
        },
        "2011": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [],
          "history": [],
          "merged_at": "2026-08-31T18:42:22Z",
          "report": null,
          "status": "never-reviewed",
          "title": "Migrate lua_strict_msg probe to probe-result/v1"
        },
        "2012": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [],
          "history": [],
          "merged_at": "2026-08-31T19:11:53Z",
          "report": null,
          "status": "never-reviewed",
          "title": "Gate every Lua call site against the engine's real registration set (#1996)"
        },
        "2013": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [],
          "history": [],
          "merged_at": "2026-08-31T19:28:42Z",
          "report": null,
          "status": "never-reviewed",
          "title": "Measure fjord and glacial coast forms"
        },
        "2014": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [],
          "history": [],
          "merged_at": "2026-08-31T19:47:41Z",
          "report": null,
          "status": "never-reviewed",
          "title": "Add common cattail wetland flora"
        },
        "2015": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [],
          "history": [],
          "merged_at": "2026-08-31T20:10:47Z",
          "report": null,
          "status": "never-reviewed",
          "title": "Keep a failing location/portal probe's failed check in the retained output"
        },
        "2016": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [],
          "history": [],
          "merged_at": "2026-08-31T20:33:13Z",
          "report": null,
          "status": "never-reviewed",
          "title": "Render crop Plant as a light-green flat tilled surface"
        },
        "2018": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [],
          "history": [],
          "merged_at": "2026-08-31T21:14:45Z",
          "report": null,
          "status": "never-reviewed",
          "title": "Instrument World.Render scene assembly telemetry (#1921)"
        },
        "202": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_211-200.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-06-27T15:04:47Z",
          "report": "docs/project_review_211-200.md",
          "status": "legacy",
          "title": "Re-drop weapons re-equipped into a disabled hand (#193)"
        },
        "2022": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [],
          "history": [],
          "merged_at": "2026-09-01T02:15:06Z",
          "report": null,
          "status": "never-reviewed",
          "title": "Centralize chunk demand behind one canonical chunk key and request owner"
        },
        "2023": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [],
          "history": [],
          "merged_at": "2026-08-31T21:39:47Z",
          "report": null,
          "status": "never-reviewed",
          "title": "Add Machine Shop construction progress art"
        },
        "2025": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [],
          "history": [],
          "merged_at": "2026-08-31T22:12:58Z",
          "report": null,
          "status": "never-reviewed",
          "title": "Skip location sight rasterization during clearance-only ticks"
        },
        "2028": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [],
          "history": [],
          "merged_at": "2026-08-31T22:35:31Z",
          "report": null,
          "status": "never-reviewed",
          "title": "Make red raspberry fruiting art visibly ripe"
        },
        "2029": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [],
          "history": [],
          "merged_at": "2026-08-31T22:58:45Z",
          "report": null,
          "status": "never-reviewed",
          "title": "Bound the debug console's retained scrollback and its layout measurement"
        },
        "203": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_211-200.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-06-27T15:14:12Z",
          "report": "docs/project_review_211-200.md",
          "status": "legacy",
          "title": "Fix #186: clear UI focus on right-click miss path"
        },
        "2032": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [],
          "history": [],
          "merged_at": "2026-08-31T23:20:54Z",
          "report": null,
          "status": "never-reviewed",
          "title": "Migrate five targeted probes to probe-result/v1"
        },
        "2038": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [],
          "history": [],
          "merged_at": "2026-08-31T23:47:22Z",
          "report": null,
          "status": "never-reviewed",
          "title": "Bound breakthrough search scratch to its radius"
        },
        "2039": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [],
          "history": [],
          "merged_at": "2026-09-01T00:11:23Z",
          "report": null,
          "status": "never-reviewed",
          "title": "Gate the coastal-parallel threshold on the run length it names, in both river gates"
        },
        "204": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_211-200.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-06-27T15:23:39Z",
          "report": "docs/project_review_211-200.md",
          "status": "legacy",
          "title": "Fix #198: scope build-tool visibility to the active world"
        },
        "2045": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [],
          "history": [],
          "merged_at": "2026-09-01T00:27:39Z",
          "report": null,
          "status": "never-reviewed",
          "title": "Declare repair recipes instantaneous instead of advertising work the repair path never spends"
        },
        "205": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_211-200.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-06-27T15:24:28Z",
          "report": "docs/project_review_211-200.md",
          "status": "legacy",
          "title": "Fix #185: hit-test UI on middle-click so it can't reach gameplay"
        },
        "206": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_211-200.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-06-27T15:38:16Z",
          "report": "docs/project_review_211-200.md",
          "status": "legacy",
          "title": "Fix #180: clear tile/chunk cursor selection on Escape"
        },
        "2066": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [],
          "history": [],
          "merged_at": "2026-09-01T00:43:38Z",
          "report": null,
          "status": "never-reviewed",
          "title": "playtest: camera-relative zoom semantics and a bounded wheel delta for the scroll action"
        },
        "207": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_211-200.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-06-27T15:44:00Z",
          "report": "docs/project_review_211-200.md",
          "status": "legacy",
          "title": "Fix #177: clear building and ground-item selections on Escape"
        },
        "2077": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [],
          "history": [],
          "merged_at": "2026-09-01T01:03:01Z",
          "report": null,
          "status": "never-reviewed",
          "title": "Add keyboard navigation to preview browser"
        },
        "2079": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [],
          "history": [],
          "merged_at": "2026-09-01T01:21:38Z",
          "report": null,
          "status": "never-reviewed",
          "title": "Aggregate active startup YAML logging once per registry family"
        },
        "208": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_237-208.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-06-27T16:05:41Z",
          "report": "docs/project_review_237-208.md",
          "status": "legacy",
          "title": "Fix #196: scope building_spawn to the active world page"
        },
        "2081": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [],
          "history": [],
          "merged_at": "2026-09-01T01:48:14Z",
          "report": null,
          "status": "never-reviewed",
          "title": "Cap lateral fluid equalization at the source's remaining volume"
        },
        "2082": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [],
          "history": [],
          "merged_at": "2026-09-01T04:09:07Z",
          "report": null,
          "status": "never-reviewed",
          "title": "Exchange and activate runtime fluid across the cylindrical U seam"
        },
        "2083": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [],
          "history": [],
          "merged_at": "2026-09-01T03:17:45Z",
          "report": null,
          "status": "never-reviewed",
          "title": "Gate Mode B transfer orders on the carrier's registered AI actions"
        },
        "2084": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [],
          "history": [],
          "merged_at": "2026-09-01T03:41:07Z",
          "report": null,
          "status": "never-reviewed",
          "title": "Refuse an unsafe map image plan before allocating or uploading it (#2020)"
        },
        "2086": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [],
          "history": [],
          "merged_at": "2026-09-01T04:36:07Z",
          "report": null,
          "status": "never-reviewed",
          "title": "Gate the durable location-stamp marker on world-thread commit, not on queuing"
        },
        "209": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_211-200.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-06-27T15:53:27Z",
          "report": "docs/project_review_211-200.md",
          "status": "legacy",
          "title": "Fix #197: scope unit_ai building target finders to the active world"
        },
        "2096": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [],
          "history": [],
          "merged_at": "2026-09-01T05:23:29Z",
          "report": null,
          "status": "never-reviewed",
          "title": "Migrate blood impact probe to probe-result/v1"
        },
        "2099": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [],
          "history": [],
          "merged_at": "2026-09-01T04:59:42Z",
          "report": null,
          "status": "never-reviewed",
          "title": "Make the capability-writer audit fail closed on projection bindings it cannot read"
        },
        "210": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_211-200.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-06-27T15:58:51Z",
          "report": "docs/project_review_211-200.md",
          "status": "legacy",
          "title": "Fix #182: gate gameplay key handlers when a menu/overlay is open"
        },
        "2101": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [],
          "history": [],
          "merged_at": "2026-09-01T05:47:55Z",
          "report": null,
          "status": "never-reviewed",
          "title": "Require a crossed presentation boundary before acknowledging sticky tutorial rows"
        },
        "2102": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [],
          "history": [],
          "merged_at": "2026-09-01T06:11:29Z",
          "report": null,
          "status": "never-reviewed",
          "title": "Establish transient unit-AI runtime defaults before a migrated row goes live"
        },
        "2103": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [],
          "history": [],
          "merged_at": "2026-09-01T06:33:45Z",
          "report": null,
          "status": "never-reviewed",
          "title": "Keep input_check.py's diagnostic sequence alive after a missed fixture click"
        },
        "2104": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [],
          "history": [],
          "merged_at": "2026-09-01T06:58:04Z",
          "report": null,
          "status": "never-reviewed",
          "title": "Pin UI textures to nearest instead of following the player's filter setting"
        },
        "2105": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [],
          "history": [],
          "merged_at": "2026-09-01T07:22:20Z",
          "report": null,
          "status": "never-reviewed",
          "title": "Delete the twelve subsumed repeated-projection examples and correct the seven inventory claims they back"
        },
        "2106": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [],
          "history": [],
          "merged_at": "2026-09-01T07:47:55Z",
          "report": null,
          "status": "never-reviewed",
          "title": "Carry signed 32-bit cylinder coordinates in every world vertex"
        },
        "2107": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [],
          "history": [],
          "merged_at": "2026-09-01T08:13:13Z",
          "report": null,
          "status": "never-reviewed",
          "title": "Extrude one texel around every atlas cell so linear sampling cannot bleed across frames"
        },
        "2108": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [],
          "history": [],
          "merged_at": "2026-09-01T08:43:09Z",
          "report": null,
          "status": "never-reviewed",
          "title": "Give every flora instance stable identity and exact mutable state (#1854)"
        },
        "2109": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [],
          "history": [],
          "merged_at": "2026-09-01T18:21:36Z",
          "report": null,
          "status": "never-reviewed",
          "title": "Make structure drag planning authoritative and self-clearing (#1844)"
        },
        "211": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_211-200.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-06-27T16:05:14Z",
          "report": "docs/project_review_211-200.md",
          "status": "legacy",
          "title": "Fix #172: enforce selection mutual-exclusivity on Shift-clicks"
        },
        "2110": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [],
          "history": [],
          "merged_at": "2026-09-01T09:07:51Z",
          "report": null,
          "status": "never-reviewed",
          "title": "Pin the encounter roll's id dependence, chunk independence and mapping"
        },
        "2111": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [],
          "history": [],
          "merged_at": "2026-09-01T09:32:00Z",
          "report": null,
          "status": "never-reviewed",
          "title": "Split the aggregate probe runner along registry, diagnostics, lifecycle, and scheduling owners"
        },
        "2112": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [],
          "history": [],
          "merged_at": "2026-09-01T09:59:01Z",
          "report": null,
          "status": "never-reviewed",
          "title": "Split the generated-language headless spec along generator-version contracts"
        },
        "2113": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [],
          "history": [],
          "merged_at": "2026-09-01T10:22:24Z",
          "report": null,
          "status": "never-reviewed",
          "title": "Route gameplay unit atlases through the player-selected sampler"
        },
        "2114": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [],
          "history": [],
          "merged_at": "2026-09-01T10:45:22Z",
          "report": null,
          "status": "never-reviewed",
          "title": "Make woundEffSeverity the only spelling of effective severity in the wound tick"
        },
        "2115": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [],
          "history": [],
          "merged_at": "2026-09-01T11:15:23Z",
          "report": null,
          "status": "never-reviewed",
          "title": "Extract the unified-transfer probe's stage owners behind its single-session fa\u00e7ade"
        },
        "2117": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [],
          "history": [],
          "merged_at": "2026-09-01T19:38:24Z",
          "report": null,
          "status": "never-reviewed",
          "title": "Split the playtest runner's self-test along its module owners (#2040)"
        },
        "2118": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [],
          "history": [],
          "merged_at": "2026-09-01T20:04:21Z",
          "report": null,
          "status": "never-reviewed",
          "title": "Split the atomic probe-claim gate along lease, census, and orchestration owners (#2100)"
        },
        "2119": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "cursor:docs/project_review_boundaries.md",
            "report:docs/project_review_2543-2119.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-09-01T20:39:31Z",
          "report": "docs/project_review_2543-2119.md",
          "status": "legacy",
          "title": "Split the worldgen DTO graph into owner modules behind a fa\u00e7ade (#2098)"
        },
        "212": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_237-208.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-06-27T16:58:58Z",
          "report": "docs/project_review_237-208.md",
          "status": "legacy",
          "title": "Fix #128: clear stale Weather/Resources tabs on tile selection"
        },
        "2120": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "cursor:docs/project_review_boundaries.md",
            "report:docs/project_review_2543-2119.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-09-01T21:07:05Z",
          "report": "docs/project_review_2543-2119.md",
          "status": "legacy",
          "title": "One quiet-by-default assertion helper for the tools/ self-tests (#1922)"
        },
        "2121": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "cursor:docs/project_review_boundaries.md",
            "report:docs/project_review_2542-2144.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-09-02T02:50:32Z",
          "report": "docs/project_review_2542-2144.md",
          "status": "legacy",
          "title": "Make Chop a drag-box tool with tree-anchored designation markers (#1856)"
        },
        "2122": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "cursor:docs/project_review_boundaries.md",
            "report:docs/project_review_2543-2119.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-09-01T21:22:57Z",
          "report": "docs/project_review_2543-2119.md",
          "status": "legacy",
          "title": "Give both de-flake outcome consumers one shared handoff contract (#2097)"
        },
        "2123": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "cursor:docs/project_review_boundaries.md",
            "report:docs/project_review_2543-2119.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-09-01T21:49:13Z",
          "report": "docs/project_review_2543-2119.md",
          "status": "legacy",
          "title": "Give each generated world an opaque persistent identity (#2021)"
        },
        "2125": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "cursor:docs/project_review_boundaries.md",
            "report:docs/project_review_2542-2144.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-09-02T03:17:56Z",
          "report": "docs/project_review_2542-2144.md",
          "status": "legacy",
          "title": "Gate location clearing on guaranteed significant loot (#917)"
        },
        "2127": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "cursor:docs/project_review_boundaries.md",
            "report:docs/project_review_2543-2119.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-09-01T22:38:13Z",
          "report": "docs/project_review_2543-2119.md",
          "status": "legacy",
          "title": "Split the enum append-only gate along parser, carrier, baseline, and self-test owners (#2057)"
        },
        "2132": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "cursor:docs/project_review_boundaries.md",
            "report:docs/project_review_2543-2119.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-09-01T22:12:49Z",
          "report": "docs/project_review_2543-2119.md",
          "status": "legacy",
          "title": "Migrate meal waste probe to probe-result/v1"
        },
        "2133": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "cursor:docs/project_review_boundaries.md",
            "report:docs/project_review_2537-2196.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-09-02T07:03:37Z",
          "report": "docs/project_review_2537-2196.md",
          "status": "legacy",
          "title": "Establish the shared generated-world library and its reference-aware lifecycle (#2024)"
        },
        "2134": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "cursor:docs/project_review_boundaries.md",
            "report:docs/project_review_2543-2119.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-09-01T23:05:39Z",
          "report": "docs/project_review_2543-2119.md",
          "status": "legacy",
          "title": "Split the save-compatibility tool along audit, codec, registration, and generation owners"
        },
        "2136": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "cursor:docs/project_review_boundaries.md",
            "report:docs/project_review_2543-2119.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-09-01T23:31:03Z",
          "report": "docs/project_review_2543-2119.md",
          "status": "legacy",
          "title": "[art] Add approved wild and cultivated wheat textures"
        },
        "2137": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "cursor:docs/project_review_boundaries.md",
            "report:docs/project_review_2543-2119.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-09-01T23:57:58Z",
          "report": "docs/project_review_2543-2119.md",
          "status": "legacy",
          "title": "Introduce camera-facing building asset declarations and distinct lifecycle roles"
        },
        "2139": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "cursor:docs/project_review_boundaries.md",
            "report:docs/project_review_2543-2119.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-09-02T00:24:42Z",
          "report": "docs/project_review_2543-2119.md",
          "status": "legacy",
          "title": "Extract location-content scenario owners behind its eight-process fa\u00e7ade"
        },
        "2143": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "cursor:docs/project_review_boundaries.md",
            "report:docs/project_review_2543-2119.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-09-02T00:50:40Z",
          "report": "docs/project_review_2543-2119.md",
          "status": "legacy",
          "title": "Give headless test fixtures a quiet log backend by default (#1925)"
        },
        "2144": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "cursor:docs/project_review_boundaries.md",
            "report:docs/project_review_2542-2144.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-09-02T01:17:24Z",
          "report": "docs/project_review_2542-2144.md",
          "status": "legacy",
          "title": "Fail the unit-to-unit escort fixture at setup instead of grading a pair already in reach"
        },
        "2146": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "cursor:docs/project_review_boundaries.md",
            "report:docs/project_review_2542-2144.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-09-02T01:42:07Z",
          "report": "docs/project_review_2542-2144.md",
          "status": "legacy",
          "title": "Delete the two arena seed-contract self-comparisons and the fixture that feeds one of them"
        },
        "2152": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "cursor:docs/project_review_boundaries.md",
            "report:docs/project_review_2542-2144.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-09-02T02:21:15Z",
          "report": "docs/project_review_2542-2144.md",
          "status": "legacy",
          "title": "Split the world-audit self-test along audit, check and baseline owners (#2070)"
        },
        "2153": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "cursor:docs/project_review_boundaries.md",
            "report:docs/project_review_2542-2144.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-09-02T05:31:11Z",
          "report": "docs/project_review_2542-2144.md",
          "status": "legacy",
          "title": "Split the save-migrations gate along baseline, DTO-history, and legacy-envelope owners (#2094)"
        },
        "2154": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "cursor:docs/project_review_boundaries.md",
            "report:docs/project_review_2542-2144.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-09-02T03:45:16Z",
          "report": "docs/project_review_2542-2144.md",
          "status": "legacy",
          "title": "Render and hit-test buildings from the active camera facing (#2088)"
        },
        "2158": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "cursor:docs/project_review_boundaries.md",
            "report:docs/project_review_2542-2144.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-09-02T04:02:28Z",
          "report": "docs/project_review_2542-2144.md",
          "status": "legacy",
          "title": "Extract the capability-writer scanner behind the EngineEnv audit facade"
        },
        "2178": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "cursor:docs/project_review_boundaries.md",
            "report:docs/project_review_2542-2144.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-09-02T04:21:32Z",
          "report": "docs/project_review_2542-2144.md",
          "status": "legacy",
          "title": "Split tools/pack_atlas.py along its inventory, compiler, index and budget owners (#2054)"
        },
        "2181": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "cursor:docs/project_review_boundaries.md",
            "report:docs/project_review_2537-2196.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-09-02T07:33:12Z",
          "report": "docs/project_review_2537-2196.md",
          "status": "legacy",
          "title": "Ghost planned structure pieces with their own art in both ghost states (#1846)"
        },
        "2190": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "cursor:docs/project_review_boundaries.md",
            "report:docs/project_review_2537-2196.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-09-02T14:19:39Z",
          "report": "docs/project_review_2537-2196.md",
          "status": "legacy",
          "title": "Migrate ten probes to structured flake results"
        },
        "2191": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "cursor:docs/project_review_boundaries.md",
            "report:docs/project_review_2542-2144.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-09-02T05:54:36Z",
          "report": "docs/project_review_2542-2144.md",
          "status": "legacy",
          "title": "Cache exact zoom reconstruction artifacts"
        },
        "2196": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "cursor:docs/project_review_boundaries.md",
            "report:docs/project_review_2537-2196.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-09-02T06:28:39Z",
          "report": "docs/project_review_2537-2196.md",
          "status": "legacy",
          "title": "Split the /deflake self-test along its orchestration, handoff and preparation owners (#2093)"
        },
        "2197": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "cursor:docs/project_review_boundaries.md",
            "report:docs/project_review_2537-2196.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-09-02T07:56:50Z",
          "report": "docs/project_review_2537-2196.md",
          "status": "legacy",
          "title": "Split the bare-name icon gate along language extractors, asset inventory, audit, and self-test owners (#2142)"
        },
        "2200": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "cursor:docs/project_review_boundaries.md",
            "report:docs/project_review_2537-2196.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-09-02T08:20:04Z",
          "report": "docs/project_review_2537-2196.md",
          "status": "legacy",
          "title": "Split the playtest critic along its ownership boundaries (#2069)"
        },
        "2207": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "cursor:docs/project_review_boundaries.md",
            "report:docs/project_review_2537-2196.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-09-02T08:46:34Z",
          "report": "docs/project_review_2537-2196.md",
          "status": "legacy",
          "title": "Split the responsive-gameplay gate into owner-scoped specs behind one shared engine"
        },
        "2222": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "cursor:docs/project_review_boundaries.md",
            "report:docs/project_review_2537-2196.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-09-02T09:10:46Z",
          "report": "docs/project_review_2537-2196.md",
          "status": "legacy",
          "title": "Extract the expedition-loop probe's stage owners behind its facade (#2092)"
        },
        "2235": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "cursor:docs/project_review_boundaries.md",
            "report:docs/project_review_2537-2196.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-09-02T14:46:09Z",
          "report": "docs/project_review_2537-2196.md",
          "status": "legacy",
          "title": "Split the persistence-inventory audit along Haskell, Lua, and inventory-document owners"
        },
        "2237": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "cursor:docs/project_review_boundaries.md",
            "report:docs/project_review_2537-2196.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-09-02T15:24:21Z",
          "report": "docs/project_review_2537-2196.md",
          "status": "legacy",
          "title": "Separate probe-promotion reporting from the census storage core"
        },
        "2238": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "cursor:docs/project_review_boundaries.md",
            "report:docs/project_review_2537-2196.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-09-02T15:50:46Z",
          "report": "docs/project_review_2537-2196.md",
          "status": "legacy",
          "title": "Clear independently owned Lua surfaces and the locked tooltip when a load replaces the session"
        },
        "2239": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "cursor:docs/project_review_boundaries.md",
            "report:docs/project_review_2508-2239.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-09-02T16:17:54Z",
          "report": "docs/project_review_2508-2239.md",
          "status": "legacy",
          "title": "Join the worker after a forced kill and stop reporting asynchronous termination as a crash"
        },
        "2242": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "cursor:docs/project_review_boundaries.md",
            "report:docs/project_review_2508-2239.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-09-02T16:41:34Z",
          "report": "docs/project_review_2508-2239.md",
          "status": "legacy",
          "title": "Terminalize an accepted load whose Lua-thread half throws (#2162)"
        },
        "2245": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "cursor:docs/project_review_boundaries.md",
            "report:docs/project_review_2508-2239.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-09-02T17:00:18Z",
          "report": "docs/project_review_2508-2239.md",
          "status": "legacy",
          "title": "Split the EngineEnv audit self-test along inventory and boundary owners (#2062)"
        },
        "2246": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "cursor:docs/project_review_boundaries.md",
            "report:docs/project_review_2508-2239.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-09-02T17:52:39Z",
          "report": "docs/project_review_2508-2239.md",
          "status": "legacy",
          "title": "Supervise the debug-console listener and bound each client connection"
        },
        "2247": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "cursor:docs/project_review_boundaries.md",
            "report:docs/project_review_2508-2239.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-09-02T17:30:07Z",
          "report": "docs/project_review_2508-2239.md",
          "status": "legacy",
          "title": "docs: trim CLAUDE.md to session-wide rules; nested CLAUDE.md files + engine_contracts.md take the rest"
        },
        "2248": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "cursor:docs/project_review_boundaries.md",
            "report:docs/project_review_2508-2239.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-09-02T18:17:53Z",
          "report": "docs/project_review_2508-2239.md",
          "status": "legacy",
          "title": "Draw scene text and UI-layer scene sprites in frame assembly at their declared layers"
        },
        "2249": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "cursor:docs/project_review_boundaries.md",
            "report:docs/project_review_2508-2239.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-09-02T18:41:11Z",
          "report": "docs/project_review_2508-2239.md",
          "status": "legacy",
          "title": "Add a non-looping building destruction presentation lifecycle (#2091)"
        },
        "2250": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "cursor:docs/project_review_boundaries.md",
            "report:docs/project_review_2508-2239.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-09-02T19:05:45Z",
          "report": "docs/project_review_2508-2239.md",
          "status": "legacy",
          "title": "Separate the README registry-count audit from the probe-runner process suite"
        },
        "2252": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "cursor:docs/project_review_boundaries.md",
            "report:docs/project_review_2508-2239.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-09-02T19:32:42Z",
          "report": "docs/project_review_2508-2239.md",
          "status": "legacy",
          "title": "Stop wall rotation when either placed path is ambiguously owned"
        },
        "2253": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "cursor:docs/project_review_boundaries.md",
            "report:docs/project_review_2508-2239.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-09-02T19:59:55Z",
          "report": "docs/project_review_2508-2239.md",
          "status": "legacy",
          "title": "Fail closed after the review gate's staleness decision (#2184)"
        },
        "2254": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "cursor:docs/project_review_boundaries.md",
            "report:docs/project_review_2493-2262.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-09-03T04:01:49Z",
          "report": "docs/project_review_2493-2262.md",
          "status": "legacy",
          "title": "Ghost planned buildings with their own art in both ghost states (#1845)"
        },
        "2255": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "cursor:docs/project_review_boundaries.md",
            "report:docs/project_review_2508-2239.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-09-02T20:20:58Z",
          "report": "docs/project_review_2508-2239.md",
          "status": "legacy",
          "title": "Validate video config at one domain shared by YAML load and the Lua setters (#2198)"
        },
        "2256": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "cursor:docs/project_review_boundaries.md",
            "report:docs/project_review_2532-2256.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-09-02T20:45:52Z",
          "report": "docs/project_review_2532-2256.md",
          "status": "legacy",
          "title": "Make Settings Back restore the persisted baseline for every video field"
        },
        "2257": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "cursor:docs/project_review_boundaries.md",
            "report:docs/project_review_2532-2256.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-09-02T21:11:45Z",
          "report": "docs/project_review_2532-2256.md",
          "status": "legacy",
          "title": "Bound the combat and injury panels' grouped histories (#2189)"
        },
        "2258": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "cursor:docs/project_review_boundaries.md",
            "report:docs/project_review_2532-2256.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-09-02T21:36:24Z",
          "report": "docs/project_review_2532-2256.md",
          "status": "legacy",
          "title": "Add a pointer-only `hover` action to the playtest vocabulary"
        },
        "2259": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "cursor:docs/project_review_boundaries.md",
            "report:docs/project_review_2532-2256.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-09-02T22:00:36Z",
          "report": "docs/project_review_2532-2256.md",
          "status": "legacy",
          "title": "Require a regular file for building preview static entries, not just a supported extension"
        },
        "226": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_237-208.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-06-27T17:22:15Z",
          "report": "docs/project_review_237-208.md",
          "status": "legacy",
          "title": "Fix #191: merge restored page into global managers on load"
        },
        "2260": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "cursor:docs/project_review_boundaries.md",
            "report:docs/project_review_2532-2256.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-09-02T22:25:30Z",
          "report": "docs/project_review_2532-2256.md",
          "status": "legacy",
          "title": "Restore the owning asset loader as the reported source location of shared YAML list logging"
        },
        "2261": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "cursor:docs/project_review_boundaries.md",
            "report:docs/project_review_2532-2256.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-09-02T22:50:20Z",
          "report": "docs/project_review_2532-2256.md",
          "status": "legacy",
          "title": "Drive every elapsed-time consumer from a monotonic clock with a bounded 0.25 s step (#2204)"
        },
        "2262": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "cursor:docs/project_review_boundaries.md",
            "report:docs/project_review_2493-2262.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-09-03T02:23:09Z",
          "report": "docs/project_review_2493-2262.md",
          "status": "legacy",
          "title": "Fail startup visibly when a queued YAML family is empty or unparsable"
        },
        "2263": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "cursor:docs/project_review_boundaries.md",
            "report:docs/project_review_2532-2256.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-09-03T00:05:08Z",
          "report": "docs/project_review_2532-2256.md",
          "status": "legacy",
          "title": "Reject generated-language profiles whose root space cannot cover the concept catalogue"
        },
        "2264": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "cursor:docs/project_review_boundaries.md",
            "report:docs/project_review_2532-2256.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-09-03T00:36:12Z",
          "report": "docs/project_review_2532-2256.md",
          "status": "legacy",
          "title": "Reschedule due Lua scripts before their callbacks run (#2205)"
        },
        "2265": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "cursor:docs/project_review_boundaries.md",
            "report:docs/project_review_2532-2256.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-09-03T01:18:20Z",
          "report": "docs/project_review_2532-2256.md",
          "status": "legacy",
          "title": "Split the probe-inflight self-test along its evidence-source owners (#2141)"
        },
        "2266": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "cursor:docs/project_review_boundaries.md",
            "report:docs/project_review_2532-2256.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-09-03T01:39:37Z",
          "report": "docs/project_review_2532-2256.md",
          "status": "legacy",
          "title": "Report a legacy-config copy failure as a write failure, not as a malformed legacy file (#2210)"
        },
        "2267": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "cursor:docs/project_review_boundaries.md",
            "report:docs/project_review_2532-2256.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-09-03T02:00:54Z",
          "report": "docs/project_review_2532-2256.md",
          "status": "legacy",
          "title": "Give each Mode A transfer-session contract its own spec owner (#2090)"
        },
        "2268": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "cursor:docs/project_review_boundaries.md",
            "report:docs/project_review_2493-2262.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-09-03T02:50:22Z",
          "report": "docs/project_review_2493-2262.md",
          "status": "legacy",
          "title": "Split the save-compatibility self-test along its production owners (#2073)"
        },
        "227": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_237-208.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-06-27T17:59:02Z",
          "report": "docs/project_review_237-208.md",
          "status": "legacy",
          "title": "Fix #213: remove dead arm in soilFromClimate hot+dry desert branch"
        },
        "2270": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "cursor:docs/project_review_boundaries.md",
            "report:docs/project_review_2493-2262.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-09-03T03:09:44Z",
          "report": "docs/project_review_2493-2262.md",
          "status": "legacy",
          "title": "Split the persistence-inventory self-test along Haskell, Lua, inventory, reference, and topology owners"
        },
        "2271": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "cursor:docs/project_review_boundaries.md",
            "report:docs/project_review_2493-2262.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-09-03T03:32:05Z",
          "report": "docs/project_review_2493-2262.md",
          "status": "legacy",
          "title": "Route every config/*.local.yaml write through one atomic-replace helper (#2202)"
        },
        "2279": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "cursor:docs/project_review_boundaries.md",
            "report:docs/project_review_2493-2262.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-09-03T04:31:50Z",
          "report": "docs/project_review_2493-2262.md",
          "status": "legacy",
          "title": "Split entity persistence along its three contract owners (#2150)"
        },
        "228": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_237-208.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-06-27T18:06:02Z",
          "report": "docs/project_review_237-208.md",
          "status": "legacy",
          "title": "Fix #129: only the active world drives the HUD info panel"
        },
        "2281": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "cursor:docs/project_review_boundaries.md",
            "report:docs/project_review_2493-2262.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-09-03T04:58:47Z",
          "report": "docs/project_review_2493-2262.md",
          "status": "legacy",
          "title": "Fail the save publish preflight closed when an existing generation cannot be read (#2227)"
        },
        "2287": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "cursor:docs/project_review_boundaries.md",
            "report:docs/project_review_2493-2262.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-09-03T05:28:38Z",
          "report": "docs/project_review_2493-2262.md",
          "status": "legacy",
          "title": "Admit the synchronously generated centre chunk and arena chunks to fluid simulation at init"
        },
        "2289": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "cursor:docs/project_review_boundaries.md",
            "report:docs/project_review_2493-2262.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-09-03T05:54:51Z",
          "report": "docs/project_review_2493-2262.md",
          "status": "legacy",
          "title": "Rebase critic report image links onto the report's own directory (#2220)"
        },
        "229": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_249-236.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-06-27T20:59:59Z",
          "report": "docs/project_review_249-236.md",
          "status": "legacy",
          "title": "Fix #195: prune orphaned per-id Lua state after save load"
        },
        "2295": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "cursor:docs/project_review_boundaries.md",
            "report:docs/project_review_2493-2262.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-09-03T06:20:00Z",
          "report": "docs/project_review_2493-2262.md",
          "status": "legacy",
          "title": "Split the world-audit tool along column, boundary, region, soil, and policy owners (#2224)"
        },
        "2296": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "cursor:docs/project_review_boundaries.md",
            "report:docs/project_review_2493-2262.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-09-03T06:38:55Z",
          "report": "docs/project_review_2493-2262.md",
          "status": "legacy",
          "title": "Split the capability-writer self-test along map, scanner, projection, and conformance owners"
        },
        "230": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_237-208.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-06-27T18:05:16Z",
          "report": "docs/project_review_237-208.md",
          "status": "legacy",
          "title": "Fix #100: item-contents popup guards missing container + closes on hud.hide"
        },
        "2309": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "cursor:docs/project_review_boundaries.md",
            "report:docs/project_review_2494-2313.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-09-03T13:31:44Z",
          "report": "docs/project_review_2494-2313.md",
          "status": "legacy",
          "title": "Park every save-barrier owner after its final-pass acknowledgement until the capture lock releases"
        },
        "231": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_237-208.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-06-27T18:22:57Z",
          "report": "docs/project_review_237-208.md",
          "status": "legacy",
          "title": "Fix #104: close unit_log overlay on hud.hide()"
        },
        "2312": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "cursor:docs/project_review_boundaries.md",
            "report:docs/project_review_2494-2313.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-09-03T13:01:54Z",
          "report": "docs/project_review_2494-2313.md",
          "status": "legacy",
          "title": "Sync the owning saves/ directory on slot creation and autosave rotation"
        },
        "2313": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "cursor:docs/project_review_boundaries.md",
            "report:docs/project_review_2494-2313.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-09-03T06:57:05Z",
          "report": "docs/project_review_2494-2313.md",
          "status": "legacy",
          "title": "Split the probe-census self-test along its five remaining owners (#2129)"
        },
        "2318": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "cursor:docs/project_review_boundaries.md",
            "report:docs/project_review_2494-2313.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-09-03T07:13:52Z",
          "report": "docs/project_review_2494-2313.md",
          "status": "legacy",
          "title": "Split the de-flake handoff contract into four internal owners (#2180)"
        },
        "2319": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "cursor:docs/project_review_boundaries.md",
            "report:docs/project_review_2494-2313.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-09-03T07:32:17Z",
          "report": "docs/project_review_2494-2313.md",
          "status": "legacy",
          "title": "Split the remaining EngineEnv audit implementation along inventory and boundary owners"
        },
        "232": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_237-208.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-06-27T19:48:34Z",
          "report": "docs/project_review_237-208.md",
          "status": "legacy",
          "title": "Fix #222: slope water tiles toward exposed-air edges (waterfall lips)"
        },
        "2320": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "cursor:docs/project_review_boundaries.md",
            "report:docs/project_review_2494-2313.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-09-03T07:54:26Z",
          "report": "docs/project_review_2494-2313.md",
          "status": "legacy",
          "title": "Split the pure unit-atlas gate along index, freshness, and consumer owners"
        },
        "2321": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "cursor:docs/project_review_boundaries.md",
            "report:docs/project_review_2494-2313.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-09-03T08:12:27Z",
          "report": "docs/project_review_2494-2313.md",
          "status": "legacy",
          "title": "Split the remaining probe-runner self-test along its five production owners"
        },
        "2322": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "cursor:docs/project_review_boundaries.md",
            "report:docs/project_review_2494-2313.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-09-03T08:39:59Z",
          "report": "docs/project_review_2494-2313.md",
          "status": "legacy",
          "title": "Register and place flora independently of enumeration order"
        },
        "233": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_237-208.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-06-27T18:47:13Z",
          "report": "docs/project_review_237-208.md",
          "status": "legacy",
          "title": "Fix #99: close cargo inventory popup on hud.hide()"
        },
        "2331": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "cursor:docs/project_review_boundaries.md",
            "report:docs/project_review_2494-2313.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-09-03T14:00:38Z",
          "report": "docs/project_review_2494-2313.md",
          "status": "legacy",
          "title": "Split CI parity auditing into layer owners (#2159)"
        },
        "234": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_237-208.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-06-27T18:43:25Z",
          "report": "docs/project_review_237-208.md",
          "status": "legacy",
          "title": "Fix #142: close item-contents popup on zoom transitions"
        },
        "2340": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "cursor:docs/project_review_boundaries.md",
            "report:docs/project_review_2494-2313.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-09-03T17:16:37Z",
          "report": "docs/project_review_2494-2313.md",
          "status": "legacy",
          "title": "Run the engine-free CI audits in a job that does not wait for the Cabal build (#2272)"
        },
        "2341": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "cursor:docs/project_review_boundaries.md",
            "report:docs/project_review_2494-2313.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-09-03T17:39:01Z",
          "report": "docs/project_review_2494-2313.md",
          "status": "legacy",
          "title": "Make preview probe families independently runnable behind one aggregate gate (#2089)"
        },
        "2342": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "cursor:docs/project_review_boundaries.md"
          ],
          "history": [],
          "merged_at": "2026-09-03T18:02:41Z",
          "report": null,
          "status": "legacy",
          "title": "Split the build-placement page-binding gate into four owners (#2173)"
        },
        "2343": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "cursor:docs/project_review_boundaries.md"
          ],
          "history": [],
          "merged_at": "2026-09-03T18:23:55Z",
          "report": null,
          "status": "legacy",
          "title": "Split the remaining probe-census core along contract, records, summary, storage, and CLI owners (#2131)"
        },
        "2344": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "cursor:docs/project_review_boundaries.md"
          ],
          "history": [],
          "merged_at": "2026-09-03T18:43:46Z",
          "report": null,
          "status": "legacy",
          "title": "Refuse unsafe time scales at world.setTimeScale and make the world clock total (#2280)"
        },
        "2345": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "cursor:docs/project_review_boundaries.md"
          ],
          "history": [],
          "merged_at": "2026-09-03T19:11:02Z",
          "report": null,
          "status": "legacy",
          "title": "Persist flora species references by authored name (#2243)"
        },
        "2349": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "cursor:docs/project_review_boundaries.md"
          ],
          "history": [],
          "merged_at": "2026-09-03T19:39:42Z",
          "report": null,
          "status": "legacy",
          "title": "Warn with the file and the decoder error when the worldgen config is malformed (#2286)"
        },
        "235": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_237-208.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-06-27T19:15:25Z",
          "report": "docs/project_review_237-208.md",
          "status": "legacy",
          "title": "Fix #147: hide main debug overlay on zoom and menu transitions"
        },
        "2350": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "cursor:docs/project_review_boundaries.md"
          ],
          "history": [],
          "merged_at": "2026-09-03T19:54:49Z",
          "report": null,
          "status": "legacy",
          "title": "Cap the action-outcome ring and append through one shared helper"
        },
        "2351": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "cursor:docs/project_review_boundaries.md"
          ],
          "history": [],
          "merged_at": "2026-09-03T20:15:03Z",
          "report": null,
          "status": "legacy",
          "title": "Merge live material registrations into normal world initialization instead of replacing the registry"
        },
        "2352": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "cursor:docs/project_review_boundaries.md"
          ],
          "history": [],
          "merged_at": "2026-09-03T20:37:31Z",
          "report": null,
          "status": "legacy",
          "title": "Write the fail-stop lifecycle transition before any crash reporting"
        },
        "2353": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "cursor:docs/project_review_boundaries.md"
          ],
          "history": [],
          "merged_at": "2026-09-03T20:58:38Z",
          "report": null,
          "status": "legacy",
          "title": "Report CI lane and step timings from one command (#2277)"
        },
        "2354": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "cursor:docs/project_review_boundaries.md"
          ],
          "history": [],
          "merged_at": "2026-09-03T21:16:40Z",
          "report": null,
          "status": "legacy",
          "title": "Cancel a timed-out debug command before it is claimed, and report an unknown outcome once it has started"
        },
        "2355": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "cursor:docs/project_review_boundaries.md"
          ],
          "history": [],
          "merged_at": "2026-09-03T22:57:44Z",
          "report": null,
          "status": "legacy",
          "title": "Validate world-generation float settings at one shared domain (#2288)"
        },
        "2356": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "cursor:docs/project_review_boundaries.md"
          ],
          "history": [],
          "merged_at": "2026-09-03T22:36:30Z",
          "report": null,
          "status": "legacy",
          "title": "Extract tutorial progression and sticky-presentation owners behind its four-engine fa\u00e7ade (#2145)"
        },
        "2357": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "cursor:docs/project_review_boundaries.md",
            "report:docs/project_review_2475-2357.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-09-03T23:19:00Z",
          "report": "docs/project_review_2475-2357.md",
          "status": "legacy",
          "title": "Split the container-window manager along endpoint, pane-rendering, and stack-lifecycle owners"
        },
        "2358": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "cursor:docs/project_review_boundaries.md",
            "report:docs/project_review_2475-2357.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-09-03T23:39:49Z",
          "report": "docs/project_review_2475-2357.md",
          "status": "legacy",
          "title": "Add a baseline-ratcheted audit for dead qualified Haddock links"
        },
        "2359": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "cursor:docs/project_review_boundaries.md",
            "report:docs/project_review_2475-2357.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-09-04T01:56:24Z",
          "report": "docs/project_review_2475-2357.md",
          "status": "legacy",
          "title": "Refuse non-finite or missing coordinates and speeds at the unit motion verbs, and a non-positive max_speed at the decoder"
        },
        "236": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_249-236.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-06-27T19:54:59Z",
          "report": "docs/project_review_249-236.md",
          "status": "legacy",
          "title": "Fix #175: clear ground-item selection on hud.hide() and zoom transitions"
        },
        "2360": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "cursor:docs/project_review_boundaries.md",
            "report:docs/project_review_2475-2357.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-09-04T00:33:42Z",
          "report": "docs/project_review_2475-2357.md",
          "status": "legacy",
          "title": "Reset the session-owned event store and game-clock epoch when Exit to Menu destroys every world"
        },
        "2361": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "cursor:docs/project_review_boundaries.md",
            "report:docs/project_review_2475-2357.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-09-04T00:02:17Z",
          "report": "docs/project_review_2475-2357.md",
          "status": "legacy",
          "title": "Split production-defect issue publication along evidence, document, tracker, and census owners"
        },
        "2362": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "cursor:docs/project_review_boundaries.md",
            "report:docs/project_review_2475-2357.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-09-04T01:30:14Z",
          "report": "docs/project_review_2475-2357.md",
          "status": "legacy",
          "title": "Split the Item List widget gate along model, row, invalidation, and tabbar owners"
        },
        "2363": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "cursor:docs/project_review_boundaries.md",
            "report:docs/project_review_2475-2357.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-09-04T02:17:50Z",
          "report": "docs/project_review_2475-2357.md",
          "status": "legacy",
          "title": "Gate autonomous harvest pickup on carrying capacity (#2293)"
        },
        "2364": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "cursor:docs/project_review_boundaries.md",
            "report:docs/project_review_2475-2357.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-09-04T02:38:50Z",
          "report": "docs/project_review_2475-2357.md",
          "status": "legacy",
          "title": "Extract embark-to-discovery phase owners behind its single probe fa\u00e7ade"
        },
        "2365": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "cursor:docs/project_review_boundaries.md",
            "report:docs/project_review_2475-2357.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-09-04T02:59:50Z",
          "report": "docs/project_review_2475-2357.md",
          "status": "legacy",
          "title": "Split the flake self-test into harness and per-probe migration owners"
        },
        "2366": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "cursor:docs/project_review_boundaries.md"
          ],
          "history": [],
          "merged_at": "2026-09-04T05:36:25Z",
          "report": null,
          "status": "legacy",
          "title": "Require the same page and the AI treatment range at the medical treatment verbs"
        },
        "2367": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "cursor:docs/project_review_boundaries.md"
          ],
          "history": [],
          "merged_at": "2026-09-04T03:24:05Z",
          "report": null,
          "status": "legacy",
          "title": "Split the Lua persistence-component gate along its four contract owners"
        },
        "2368": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "cursor:docs/project_review_boundaries.md"
          ],
          "history": [],
          "merged_at": "2026-09-04T03:44:49Z",
          "report": null,
          "status": "legacy",
          "title": "Bind bulk chunk work to the page its caller chose"
        },
        "2369": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "cursor:docs/project_review_boundaries.md"
          ],
          "history": [],
          "merged_at": "2026-09-04T04:08:28Z",
          "report": null,
          "status": "legacy",
          "title": "Scrub orphan acquired-immunity entries when a save is staged (#2305)"
        },
        "237": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_237-208.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-06-27T19:50:59Z",
          "report": "docs/project_review_237-208.md",
          "status": "legacy",
          "title": "Fix #133: tag HUD info pushes so zoom-map chunk selection keeps entity selection"
        },
        "2370": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "cursor:docs/project_review_boundaries.md"
          ],
          "history": [],
          "merged_at": "2026-09-04T04:43:44Z",
          "report": null,
          "status": "legacy",
          "title": "Split the per-probe claim command along storage, lease, and orchestration owners"
        },
        "2372": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "cursor:docs/project_review_boundaries.md"
          ],
          "history": [],
          "merged_at": "2026-09-04T05:09:08Z",
          "report": null,
          "status": "legacy",
          "title": "Split page-scoped persistence into world-pages, world-edits and world-activity owners (#2135)"
        },
        "2373": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "cursor:docs/project_review_boundaries.md"
          ],
          "history": [],
          "merged_at": "2026-09-04T05:59:20Z",
          "report": null,
          "status": "legacy",
          "title": "Resolve AI craft-bill mutations on the acting unit's own page, not the active one"
        },
        "2374": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "cursor:docs/project_review_boundaries.md"
          ],
          "history": [],
          "merged_at": "2026-09-04T13:15:19Z",
          "report": null,
          "status": "legacy",
          "title": "Make the building commit the authority on footprint exclusivity (#2326)"
        },
        "2376": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "cursor:docs/project_review_boundaries.md"
          ],
          "history": [],
          "merged_at": "2026-09-04T13:42:38Z",
          "report": null,
          "status": "legacy",
          "title": "Bound the dump's fast-settle wait and make the settle outcome-bearing"
        },
        "2377": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "cursor:docs/project_review_boundaries.md"
          ],
          "history": [],
          "merged_at": "2026-09-04T14:08:58Z",
          "report": null,
          "status": "legacy",
          "title": "Revalidate page, reach and stance in combat resolution before the strike commits"
        },
        "2378": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "cursor:docs/project_review_boundaries.md"
          ],
          "history": [],
          "merged_at": "2026-09-04T12:30:04Z",
          "report": null,
          "status": "legacy",
          "title": "Refuse non-finite coordinates and decal geometry at the three spawn boundaries"
        },
        "2379": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "cursor:docs/project_review_boundaries.md"
          ],
          "history": [],
          "merged_at": "2026-09-04T12:54:49Z",
          "report": null,
          "status": "legacy",
          "title": "Contain a failing generation read to its own slot in listSaves"
        },
        "238": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_249-236.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-06-27T21:26:15Z",
          "report": "docs/project_review_249-236.md",
          "status": "legacy",
          "title": "Fix #139: dismiss context menu on zoom-band transitions"
        },
        "2380": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "cursor:docs/project_review_boundaries.md"
          ],
          "history": [],
          "merged_at": "2026-09-04T15:54:28Z",
          "report": null,
          "status": "legacy",
          "title": "Refuse non-finite or out-of-domain arguments at world.digTile and keep a restored mine designation finite"
        },
        "2382": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "cursor:docs/project_review_boundaries.md"
          ],
          "history": [],
          "merged_at": "2026-09-04T15:18:27Z",
          "report": null,
          "status": "legacy",
          "title": "Give a spillway tile shared by two lakes both source identities"
        },
        "2383": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "cursor:docs/project_review_boundaries.md"
          ],
          "history": [],
          "merged_at": "2026-09-04T16:26:06Z",
          "report": null,
          "status": "legacy",
          "title": "Bound the craft and construct work clocks at every swallowed tick and unannounced gap"
        },
        "2384": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "cursor:docs/project_review_boundaries.md"
          ],
          "history": [],
          "merged_at": "2026-09-04T21:38:06Z",
          "report": null,
          "status": "legacy",
          "title": "Reconcile saved equipment slot keys against the current equipment class at staging"
        },
        "2385": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "cursor:docs/project_review_boundaries.md"
          ],
          "history": [],
          "merged_at": "2026-09-04T18:53:14Z",
          "report": null,
          "status": "legacy",
          "title": "Reject an out-of-domain infection file at the authoring boundary"
        },
        "2386": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "cursor:docs/project_review_boundaries.md"
          ],
          "history": [],
          "merged_at": "2026-09-04T17:45:45Z",
          "report": null,
          "status": "legacy",
          "title": "Refuse non-finite camera coordinates at `camera.move`/`camera.setPosition`, and default a saved camera that carries one"
        },
        "2387": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "cursor:docs/project_review_boundaries.md"
          ],
          "history": [],
          "merged_at": "2026-09-04T19:36:27Z",
          "report": null,
          "status": "legacy",
          "title": "Require a finite positive fps \u2014 and an authorable domain for every other building number \u2014 at the YAML boundary"
        },
        "2388": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "cursor:docs/project_review_boundaries.md"
          ],
          "history": [],
          "merged_at": "2026-09-04T23:07:55Z",
          "report": null,
          "status": "legacy",
          "title": "Reject a flora file whose lifecycle, phase, cycle, or override token is present but unrecognized"
        },
        "2389": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "cursor:docs/project_review_boundaries.md"
          ],
          "history": [],
          "merged_at": "2026-09-04T23:34:43Z",
          "report": null,
          "status": "legacy",
          "title": "Reject a malformed unit body_parts graph at the YAML boundary (#2348)"
        },
        "239": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_249-236.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-06-27T21:38:41Z",
          "report": "docs/project_review_249-236.md",
          "status": "legacy",
          "title": "Fix #134: keep HUD info panel visibility in sync with its page"
        },
        "2390": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "cursor:docs/project_review_boundaries.md"
          ],
          "history": [],
          "merged_at": "2026-09-04T23:55:15Z",
          "report": null,
          "status": "legacy",
          "title": "Validate the ground item at item.select and gate the item Info callback's other-domain clears on its success"
        },
        "2391": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "cursor:docs/project_review_boundaries.md"
          ],
          "history": [],
          "merged_at": "2026-09-05T00:18:49Z",
          "report": null,
          "status": "legacy",
          "title": "Canonicalize the world date at world.setDate and on load (#2339)"
        },
        "2392": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "cursor:docs/project_review_boundaries.md"
          ],
          "history": [],
          "merged_at": "2026-09-05T00:43:19Z",
          "report": null,
          "status": "legacy",
          "title": "Resolve medical kits by exact instance in supply discovery and fetch"
        },
        "2393": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "cursor:docs/project_review_boundaries.md"
          ],
          "history": [],
          "merged_at": "2026-09-05T01:08:03Z",
          "report": null,
          "status": "legacy",
          "title": "Refuse a manual save whose name a legacy flat save occupies, and list one row per save name (#2335)"
        },
        "2394": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "cursor:docs/project_review_boundaries.md"
          ],
          "history": [],
          "merged_at": "2026-09-05T01:32:54Z",
          "report": null,
          "status": "legacy",
          "title": "Split the unit-atlas self-test along validation, compiler, and budget owners (#2061)"
        },
        "2395": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "cursor:docs/project_review_boundaries.md"
          ],
          "history": [],
          "merged_at": "2026-09-05T01:56:46Z",
          "report": null,
          "status": "legacy",
          "title": "Split the Tutorial HUD gate into lifecycle, presentation, scrolling, responsive, and caption owners"
        },
        "2396": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "cursor:docs/project_review_boundaries.md"
          ],
          "history": [],
          "merged_at": "2026-09-05T02:28:57Z",
          "report": null,
          "status": "legacy",
          "title": "Split action-outcome coverage into an audit core, mutation corpus, and command fa\u00e7ade"
        },
        "2397": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "cursor:docs/project_review_boundaries.md"
          ],
          "history": [],
          "merged_at": "2026-09-05T03:27:21Z",
          "report": null,
          "status": "legacy",
          "title": "Split the manual gameplay runner into shared support and scenario owners (#2151)"
        },
        "2398": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "cursor:docs/project_review_boundaries.md"
          ],
          "history": [],
          "merged_at": "2026-09-05T04:29:48Z",
          "report": null,
          "status": "legacy",
          "title": "Split the external-evidence self-test into identity, report, confinement and resilience owners (#2187)"
        },
        "2399": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "cursor:docs/project_review_boundaries.md"
          ],
          "history": [],
          "merged_at": "2026-09-05T05:52:34Z",
          "report": null,
          "status": "legacy",
          "title": "Replace the false-green lua_strict_msg probe with a blocking strictness spec (#2161)"
        },
        "240": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_249-236.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-06-27T21:36:47Z",
          "report": "docs/project_review_249-236.md",
          "status": "legacy",
          "title": "Fix #114: drag-select no longer arms on tool/overlay-claimed clicks"
        },
        "2400": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "cursor:docs/project_review_boundaries.md"
          ],
          "history": [],
          "merged_at": "2026-09-05T06:22:06Z",
          "report": null,
          "status": "legacy",
          "title": "Ship every runtime and test resource in the source distribution and audit the manifest (#2175)"
        },
        "2401": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "cursor:docs/project_review_boundaries.md"
          ],
          "history": [],
          "merged_at": "2026-09-05T06:45:47Z",
          "report": null,
          "status": "legacy",
          "title": "Split the capability-writer scanner along authority, syntax, projection, and scan owners (#2230)"
        },
        "2402": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "cursor:docs/project_review_boundaries.md"
          ],
          "history": [],
          "merged_at": "2026-09-05T12:42:56Z",
          "report": null,
          "status": "legacy",
          "title": "Remove the write-only engine popup queue and assert popup pages on the delivery message (#2285)"
        },
        "2403": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "cursor:docs/project_review_boundaries.md"
          ],
          "history": [],
          "merged_at": "2026-09-05T07:14:26Z",
          "report": null,
          "status": "legacy",
          "title": "Author which harvest tags bypass the growth window and what a felled sprout or dead tree yields (#2212)"
        },
        "2405": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "cursor:docs/project_review_boundaries.md",
            "report:docs/project_review_2462-2405.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-09-05T13:06:21Z",
          "report": "docs/project_review_2462-2405.md",
          "status": "legacy",
          "title": "Give every headless temp fixture an invocation-owned directory through the harness primitive"
        },
        "2406": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "cursor:docs/project_review_boundaries.md",
            "report:docs/project_review_2462-2405.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-09-05T13:26:45Z",
          "report": "docs/project_review_2462-2405.md",
          "status": "legacy",
          "title": "Pin the offscreen probe's fixture world and report every rejected portal candidate"
        },
        "2407": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "cursor:docs/project_review_boundaries.md",
            "report:docs/project_review_2462-2405.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-09-05T15:34:06Z",
          "report": "docs/project_review_2462-2405.md",
          "status": "legacy",
          "title": "Seed the zoom coastal fill from ocean only, and make its extent independent of scan order (#2316)"
        },
        "2408": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "cursor:docs/project_review_boundaries.md",
            "report:docs/project_review_2462-2405.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-09-05T15:57:39Z",
          "report": "docs/project_review_2462-2405.md",
          "status": "legacy",
          "title": "Dispatch parallel probes longest-expected-first (#2275)"
        },
        "2409": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "cursor:docs/project_review_boundaries.md",
            "report:docs/project_review_2462-2405.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-09-05T16:21:05Z",
          "report": "docs/project_review_2462-2405.md",
          "status": "legacy",
          "title": "Grade the replaced lunge's own cancellation, not the next launch (#2168)"
        },
        "241": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_249-236.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-06-27T22:29:02Z",
          "report": "docs/project_review_249-236.md",
          "status": "legacy",
          "title": "Fix #135: zoom-chunk and zoomed-in tile selections can no longer coexist"
        },
        "2410": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "cursor:docs/project_review_boundaries.md",
            "report:docs/project_review_2462-2405.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-09-05T19:11:45Z",
          "report": "docs/project_review_2462-2405.md",
          "status": "legacy",
          "title": "Move Lua module lifecycle and view narration from Info to Debug (#2174)"
        },
        "2411": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "cursor:docs/project_review_boundaries.md",
            "report:docs/project_review_2462-2405.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-09-05T19:48:28Z",
          "report": "docs/project_review_2462-2405.md",
          "status": "legacy",
          "title": "Route every production show-to-Text wrapper through tshow, with a closed spelling guard (#2177)"
        },
        "2412": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "cursor:docs/project_review_boundaries.md",
            "report:docs/project_review_2462-2405.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-09-05T20:38:53Z",
          "report": "docs/project_review_2462-2405.md",
          "status": "legacy",
          "title": "Tie the construction probe's progress oracle to the construct_job phase and capture state on every expired poll (#2172)"
        },
        "2413": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "cursor:docs/project_review_boundaries.md",
            "report:docs/project_review_2429-2413.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-09-05T21:45:44Z",
          "report": "docs/project_review_2429-2413.md",
          "status": "legacy",
          "title": "docs: streamline root agent guidance and retain critical context"
        },
        "2414": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "cursor:docs/project_review_boundaries.md",
            "report:docs/project_review_2462-2405.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-09-05T20:13:24Z",
          "report": "docs/project_review_2462-2405.md",
          "status": "legacy",
          "title": "Keep every lunge probe fixture on loaded arena terrain"
        },
        "2416": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "cursor:docs/project_review_boundaries.md",
            "report:docs/project_review_2462-2405.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-09-05T21:01:57Z",
          "report": "docs/project_review_2462-2405.md",
          "status": "legacy",
          "title": "Split the item-list widget probe into one orchestrator and owner-scoped scenarios (#2046)"
        },
        "2417": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "cursor:docs/project_review_boundaries.md",
            "report:docs/project_review_2462-2405.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-09-05T21:26:11Z",
          "report": "docs/project_review_2462-2405.md",
          "status": "legacy",
          "title": "Delete the two item-contents signature self-comparisons"
        },
        "2418": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "cursor:docs/project_review_boundaries.md",
            "report:docs/project_review_2429-2413.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-09-05T22:43:40Z",
          "report": "docs/project_review_2429-2413.md",
          "status": "legacy",
          "title": "Describe the atlas budget as the generated-artifact check it is (#2217)"
        },
        "2419": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "cursor:docs/project_review_boundaries.md",
            "report:docs/project_review_2429-2413.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-09-05T22:11:26Z",
          "report": "docs/project_review_2429-2413.md",
          "status": "legacy",
          "title": "Split the pure save-components gate along its four persistence owners (#2043)"
        },
        "242": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_249-236.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-06-27T21:33:29Z",
          "report": "docs/project_review_249-236.md",
          "status": "legacy",
          "title": "Fix #152: gate debug anim panel click-claim on the gameplay view"
        },
        "2420": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "cursor:docs/project_review_boundaries.md",
            "report:docs/project_review_2429-2413.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-09-05T22:26:09Z",
          "report": "docs/project_review_2429-2413.md",
          "status": "legacy",
          "title": "Print the enum audit's three coverage counts and stop engine_contracts.md hand-counting them"
        },
        "2421": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "cursor:docs/project_review_boundaries.md",
            "report:docs/project_review_2429-2413.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-09-05T23:00:54Z",
          "report": "docs/project_review_2429-2413.md",
          "status": "legacy",
          "title": "Name all six publishGeneration inputs and the pre-write refusals in World.Save.Storage's header"
        },
        "2422": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "cursor:docs/project_review_boundaries.md",
            "report:docs/project_review_2429-2413.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-09-06T00:22:19Z",
          "report": "docs/project_review_2429-2413.md",
          "status": "legacy",
          "title": "Describe YamlVegetation's real input: a caller-enumerated data/vegetation/*.yaml directory"
        },
        "2423": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "cursor:docs/project_review_boundaries.md",
            "report:docs/project_review_2429-2413.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-09-06T00:46:02Z",
          "report": "docs/project_review_2429-2413.md",
          "status": "legacy",
          "title": "Correct `World.River.Naming`'s write-once rationale to the post-#1868 append-only root placement"
        },
        "2424": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "cursor:docs/project_review_boundaries.md",
            "report:docs/project_review_2429-2413.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-09-06T01:08:28Z",
          "report": "docs/project_review_2429-2413.md",
          "status": "legacy",
          "title": "Restore the #919 survival calibration run record as a linked docs/history archive"
        },
        "2425": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "cursor:docs/project_review_boundaries.md",
            "report:docs/project_review_2441-2425.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-09-06T05:22:29Z",
          "report": "docs/project_review_2441-2425.md",
          "status": "legacy",
          "title": "Audit \u00a72.1's capability-record sizes instead of hand-maintaining them"
        },
        "2426": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "cursor:docs/project_review_boundaries.md",
            "report:docs/project_review_2429-2413.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-09-06T02:33:42Z",
          "report": "docs/project_review_2429-2413.md",
          "status": "legacy",
          "title": "Correct Unit.Transfer's serializable-set count to seven"
        },
        "2427": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "cursor:docs/project_review_boundaries.md",
            "report:docs/project_review_2429-2413.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-09-06T02:56:28Z",
          "report": "docs/project_review_2429-2413.md",
          "status": "legacy",
          "title": "Enumerate all eleven transient unit-AI registries in the persistence inventory and reset-hook comment"
        },
        "2428": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "cursor:docs/project_review_boundaries.md",
            "report:docs/project_review_2429-2413.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-09-06T03:40:11Z",
          "report": "docs/project_review_2429-2413.md",
          "status": "legacy",
          "title": "Document brain.lua's real state_of_mind ownership instead of calling it read-only"
        },
        "2429": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "cursor:docs/project_review_boundaries.md",
            "report:docs/project_review_2429-2413.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-09-06T04:58:49Z",
          "report": "docs/project_review_2429-2413.md",
          "status": "legacy",
          "title": "Correct the exhaustion and circadian comments that call #611/#612 future work"
        },
        "243": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_249-236.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-06-27T22:12:25Z",
          "report": "docs/project_review_249-236.md",
          "status": "legacy",
          "title": "Fix #137: gate unit_info_v2 pane on zoom view + HUD visibility"
        },
        "2430": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "cursor:docs/project_review_boundaries.md",
            "report:docs/project_review_2441-2425.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-09-06T05:45:53Z",
          "report": "docs/project_review_2441-2425.md",
          "status": "legacy",
          "title": "Collapse routine Cabal build progress in CI and make ci (#1920)"
        },
        "2431": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "cursor:docs/project_review_boundaries.md",
            "report:docs/project_review_2441-2425.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-09-06T06:08:56Z",
          "report": "docs/project_review_2441-2425.md",
          "status": "legacy",
          "title": "Keep the persistence sweep's failed checks inside the runner's retained output (#2060)"
        },
        "2432": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "cursor:docs/project_review_boundaries.md",
            "report:docs/project_review_2441-2425.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-09-06T06:25:20Z",
          "report": "docs/project_review_2441-2425.md",
          "status": "legacy",
          "title": "Quiet the notification-registry success line, warn on an empty registry"
        },
        "2433": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "cursor:docs/project_review_boundaries.md",
            "report:docs/project_review_2441-2425.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-09-06T06:40:04Z",
          "report": "docs/project_review_2441-2425.md",
          "status": "legacy",
          "title": "Give the chop probe a deterministic wood-harvestable target (#2058)"
        },
        "2434": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "cursor:docs/project_review_boundaries.md",
            "report:docs/project_review_2441-2425.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-09-06T07:06:20Z",
          "report": "docs/project_review_2441-2425.md",
          "status": "legacy",
          "title": "Rebuild the etymology probe's HUD against its live render resources"
        },
        "2435": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "cursor:docs/project_review_boundaries.md",
            "report:docs/project_review_2441-2425.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-09-06T07:24:14Z",
          "report": "docs/project_review_2441-2425.md",
          "status": "legacy",
          "title": "Extract debug-console responsiveness behind the ResponsiveMenus fa\u00e7ade"
        },
        "2436": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "cursor:docs/project_review_boundaries.md",
            "report:docs/project_review_2441-2425.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-09-06T07:45:44Z",
          "report": "docs/project_review_2441-2425.md",
          "status": "legacy",
          "title": "Extract the de-flake document contract behind the diagnosis fa\u00e7ade"
        },
        "2437": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "cursor:docs/project_review_boundaries.md",
            "report:docs/project_review_2441-2425.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-09-06T08:03:16Z",
          "report": "docs/project_review_2441-2425.md",
          "status": "legacy",
          "title": "Use a compact Hspec formatter for the headless suite in CI and make ci"
        },
        "2438": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "cursor:docs/project_review_boundaries.md",
            "report:docs/project_review_2441-2425.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-09-06T08:19:22Z",
          "report": "docs/project_review_2441-2425.md",
          "status": "legacy",
          "title": "Print only over-budget modules from the two module-budget guards"
        },
        "2439": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "cursor:docs/project_review_boundaries.md",
            "report:docs/project_review_2441-2425.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-09-06T08:37:30Z",
          "report": "docs/project_review_2441-2425.md",
          "status": "legacy",
          "title": "Send the worldgen tectonic and climate banners to the generation log only"
        },
        "244": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_262-248.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-06-27T23:10:44Z",
          "report": "docs/project_review_262-248.md",
          "status": "legacy",
          "title": "Fix #103: represent build tool in engine ToolMode"
        },
        "2441": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "cursor:docs/project_review_boundaries.md",
            "report:docs/project_review_2441-2425.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-09-06T08:59:09Z",
          "report": "docs/project_review_2441-2425.md",
          "status": "legacy",
          "title": "Document unit.getWounds's full 19-key schema and lock it with a key-set test"
        },
        "2442": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "cursor:docs/project_review_boundaries.md"
          ],
          "history": [],
          "merged_at": "2026-09-06T12:28:16Z",
          "report": null,
          "status": "legacy",
          "title": "Cover every Settings scale-change action with exact-once fan-out tests"
        },
        "2443": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "cursor:docs/project_review_boundaries.md"
          ],
          "history": [],
          "merged_at": "2026-09-06T12:04:42Z",
          "report": null,
          "status": "legacy",
          "title": "Pin the treatment generator in the medical-kit spec's fixture reset"
        },
        "2444": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "cursor:docs/project_review_boundaries.md"
          ],
          "history": [],
          "merged_at": "2026-09-06T14:04:11Z",
          "report": null,
          "status": "legacy",
          "title": "Split the de-flake deterministic gate along its three workflow owners"
        },
        "2445": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "cursor:docs/project_review_boundaries.md"
          ],
          "history": [],
          "merged_at": "2026-09-06T09:21:30Z",
          "report": null,
          "status": "legacy",
          "title": "Move Lua state-snapshot and per-action telemetry off the Info stream"
        },
        "2446": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "cursor:docs/project_review_boundaries.md"
          ],
          "history": [],
          "merged_at": "2026-09-06T12:53:29Z",
          "report": null,
          "status": "legacy",
          "title": "Remove the inert lcShowTimestamp and lcShowThreadId logger configuration fields"
        },
        "2447": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "cursor:docs/project_review_boundaries.md"
          ],
          "history": [],
          "merged_at": "2026-09-06T13:17:32Z",
          "report": null,
          "status": "legacy",
          "title": "Correct the bindless-capacity comments that deny the effective limit the code enforces"
        },
        "2448": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "cursor:docs/project_review_boundaries.md"
          ],
          "history": [],
          "merged_at": "2026-09-06T15:17:13Z",
          "report": null,
          "status": "legacy",
          "title": "Gate the claim owners' import direction, the command's seams and its entry point"
        },
        "2449": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "cursor:docs/project_review_boundaries.md"
          ],
          "history": [],
          "merged_at": "2026-09-06T14:55:35Z",
          "report": null,
          "status": "legacy",
          "title": "Remove dead AssetEvent type from Engine.Asset.Base"
        },
        "245": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_249-236.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-06-27T22:37:16Z",
          "report": "docs/project_review_249-236.md",
          "status": "legacy",
          "title": "Fix #38: load unit pathing cost tunables from config/pathing.yaml"
        },
        "2450": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "cursor:docs/project_review_boundaries.md"
          ],
          "history": [],
          "merged_at": "2026-09-07T14:45:50Z",
          "report": null,
          "status": "legacy",
          "title": "docs: stop naming Combat among the always-ack save-barrier owners"
        },
        "2451": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "cursor:docs/project_review_boundaries.md"
          ],
          "history": [],
          "merged_at": "2026-09-07T15:19:35Z",
          "report": null,
          "status": "legacy",
          "title": "Give overdue Lua updates service during sustained message traffic"
        },
        "2452": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "cursor:docs/project_review_boundaries.md"
          ],
          "history": [],
          "merged_at": "2026-09-07T18:33:01Z",
          "report": null,
          "status": "legacy",
          "title": "fix: report worldgen parity diagnostic on failure instead of every run"
        },
        "2453": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "cursor:docs/project_review_boundaries.md",
            "report:docs/project_review_2466-2453.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-09-07T18:55:25Z",
          "report": "docs/project_review_2466-2453.md",
          "status": "legacy",
          "title": "docs: point pose/activity mirror-field comments at their label functions"
        },
        "2454": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "cursor:docs/project_review_boundaries.md",
            "report:docs/project_review_2466-2453.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-09-07T19:17:01Z",
          "report": "docs/project_review_2466-2453.md",
          "status": "legacy",
          "title": "refactor: stop logging a post-fork line for the four paired workers"
        },
        "2455": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "cursor:docs/project_review_boundaries.md",
            "report:docs/project_review_2466-2453.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-09-07T19:38:49Z",
          "report": "docs/project_review_2466-2453.md",
          "status": "legacy",
          "title": "docs: fix stale spriteRowSpan justification in unitToQuad's climb-occlusion comment"
        },
        "2456": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "cursor:docs/project_review_boundaries.md",
            "report:docs/project_review_2466-2453.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-09-07T19:56:58Z",
          "report": "docs/project_review_2466-2453.md",
          "status": "legacy",
          "title": "docs: state the sim thread's real teardown dependency in `preRenderWorkers`"
        },
        "2457": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "cursor:docs/project_review_boundaries.md",
            "report:docs/project_review_2466-2453.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-09-07T20:06:00Z",
          "report": "docs/project_review_2466-2453.md",
          "status": "legacy",
          "title": "docs: correct the buildings-viewer authority-split cross-reference"
        },
        "2458": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "cursor:docs/project_review_boundaries.md",
            "report:docs/project_review_2466-2453.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-09-07T20:24:17Z",
          "report": "docs/project_review_2466-2453.md",
          "status": "legacy",
          "title": "docs: state inputBoundaryPage's real (upLayer, upZIndex) tie-break"
        },
        "2459": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "cursor:docs/project_review_boundaries.md",
            "report:docs/project_review_2466-2453.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-09-07T20:42:32Z",
          "report": "docs/project_review_2466-2453.md",
          "status": "legacy",
          "title": "docs: scope resolveTexture haddock to the T-pose mirror"
        },
        "246": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_249-236.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-06-27T22:23:54Z",
          "report": "docs/project_review_249-236.md",
          "status": "legacy",
          "title": "Fix #141: tear down cargo inventory popup on zoom transitions"
        },
        "2460": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "cursor:docs/project_review_boundaries.md",
            "report:docs/project_review_2466-2453.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-09-07T21:03:12Z",
          "report": "docs/project_review_2466-2453.md",
          "status": "legacy",
          "title": "refactor: make runPreview require its browsing state (#2208)"
        },
        "2461": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "cursor:docs/project_review_boundaries.md",
            "report:docs/project_review_2466-2453.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-09-07T21:21:27Z",
          "report": "docs/project_review_2466-2453.md",
          "status": "legacy",
          "title": "art: add approved umbrella thorn acacia textures"
        },
        "2462": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "cursor:docs/project_review_boundaries.md",
            "report:docs/project_review_2462-2405.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-09-07T23:22:37Z",
          "report": "docs/project_review_2462-2405.md",
          "status": "legacy",
          "title": "docs: correct Engine.Core.Workers header's non-fatal caller description"
        },
        "2463": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "cursor:docs/project_review_boundaries.md",
            "report:docs/project_review_2466-2453.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-09-07T21:41:07Z",
          "report": "docs/project_review_2466-2453.md",
          "status": "legacy",
          "title": "docs: correct Unit.HitTest's stale projection-mirroring claims"
        },
        "2464": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "cursor:docs/project_review_boundaries.md"
          ],
          "history": [],
          "merged_at": "2026-09-07T23:38:00Z",
          "report": null,
          "status": "legacy",
          "title": "Point `isPointerSurfaceBlocked`'s two haddock references at `Engine.Input.Thread.Mouse`"
        },
        "2465": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "cursor:docs/project_review_boundaries.md",
            "report:docs/project_review_2466-2453.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-09-07T22:00:34Z",
          "report": "docs/project_review_2466-2453.md",
          "status": "legacy",
          "title": "Fix Building.Render's stale sprite-height sort-key comment"
        },
        "2466": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "cursor:docs/project_review_boundaries.md",
            "report:docs/project_review_2466-2453.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-09-07T22:18:20Z",
          "report": "docs/project_review_2466-2453.md",
          "status": "legacy",
          "title": "State `anySegmentIsSymlink`'s quantifier precisely: every level strictly below the root, never the root itself"
        },
        "2467": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "cursor:docs/project_review_boundaries.md"
          ],
          "history": [],
          "merged_at": "2026-09-07T22:43:29Z",
          "report": null,
          "status": "legacy",
          "title": "Name all four `pickFrame` consumers in its haddock"
        },
        "2469": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "cursor:docs/project_review_boundaries.md",
            "report:docs/project_review_2475-2357.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-09-08T00:18:16Z",
          "report": "docs/project_review_2475-2357.md",
          "status": "legacy",
          "title": "Correct `shutdownEngineWorkers`'s haddock: its non-fatal callers have a live logger"
        },
        "247": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_249-236.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-06-27T22:39:36Z",
          "report": "docs/project_review_249-236.md",
          "status": "legacy",
          "title": "Fix #145: gate debug_anim_panel on the gameplay zoomed-in view"
        },
        "2472": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "cursor:docs/project_review_boundaries.md",
            "report:docs/project_review_2475-2357.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-09-08T00:04:45Z",
          "report": "docs/project_review_2475-2357.md",
          "status": "legacy",
          "title": "Rewrite buildPreviewUnit's haddock to describe the atlas-first pipeline"
        },
        "2475": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "cursor:docs/project_review_boundaries.md",
            "report:docs/project_review_2475-2357.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-09-08T00:33:53Z",
          "report": "docs/project_review_2475-2357.md",
          "status": "legacy",
          "title": "Describe `Unit.Atlas.Digest`'s stream as the code hashes it: prefixed tag, then prefixed label and value per field"
        },
        "248": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_262-248.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-06-27T22:49:53Z",
          "report": "docs/project_review_262-248.md",
          "status": "legacy",
          "title": "Fix #146: tear down active drag-select on view transitions"
        },
        "249": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_249-236.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-06-27T22:48:13Z",
          "report": "docs/project_review_249-236.md",
          "status": "legacy",
          "title": "Fix #144: tear down mine-designation anchor on zoom transitions"
        },
        "2493": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "cursor:docs/project_review_boundaries.md",
            "report:docs/project_review_2493-2262.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-09-08T01:24:18Z",
          "report": "docs/project_review_2493-2262.md",
          "status": "legacy",
          "title": "Correct stopWorkers's haddock: name the real invariant and all three callers"
        },
        "2494": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "cursor:docs/project_review_boundaries.md",
            "report:docs/project_review_2494-2313.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-09-08T00:51:18Z",
          "report": "docs/project_review_2494-2313.md",
          "status": "legacy",
          "title": "Fix Blood.Pool's header to stop describing #884 as pending"
        },
        "250": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_262-248.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-06-27T23:24:21Z",
          "report": "docs/project_review_262-248.md",
          "status": "legacy",
          "title": "Fix #125: scope drag-select hit-test to the active world"
        },
        "2508": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "cursor:docs/project_review_boundaries.md",
            "report:docs/project_review_2508-2239.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-09-08T01:58:52Z",
          "report": "docs/project_review_2508-2239.md",
          "status": "legacy",
          "title": "preview: drop the dead `sortFrameFiles` re-export and name its one consumer"
        },
        "251": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_262-248.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-06-27T23:31:12Z",
          "report": "docs/project_review_262-248.md",
          "status": "legacy",
          "title": "Fix #151: gate debug-overlay click claim on the current view"
        },
        "252": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_262-248.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-06-27T23:35:08Z",
          "report": "docs/project_review_262-248.md",
          "status": "legacy",
          "title": "Fix #22: enforce deterministic baseline summaries in world_check"
        },
        "253": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_262-248.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-06-27T23:33:41Z",
          "report": "docs/project_review_262-248.md",
          "status": "legacy",
          "title": "Fix #143: tear down build picker on zoom-band transitions"
        },
        "2532": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "cursor:docs/project_review_boundaries.md",
            "report:docs/project_review_2532-2256.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-09-08T01:42:06Z",
          "report": "docs/project_review_2532-2256.md",
          "status": "legacy",
          "title": "Extend the first-session tutorial through expedition completion"
        },
        "2537": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "cursor:docs/project_review_boundaries.md",
            "report:docs/project_review_2537-2196.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-09-08T02:15:48Z",
          "report": "docs/project_review_2537-2196.md",
          "status": "legacy",
          "title": "Define the dormant coordinated simulation step protocol"
        },
        "2540": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "cursor:docs/project_review_boundaries.md",
            "report:docs/project_review_2542-2144.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-09-08T02:31:39Z",
          "report": "docs/project_review_2542-2144.md",
          "status": "legacy",
          "title": "docs: state text_wrap.lua's real surface in scripts/CLAUDE.md (#2306)"
        },
        "2542": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "cursor:docs/project_review_boundaries.md",
            "report:docs/project_review_2542-2144.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-09-08T02:49:37Z",
          "report": "docs/project_review_2542-2144.md",
          "status": "legacy",
          "title": "Fix inverted climateRegionSize comment, document minimumWorldSize divisibility"
        },
        "2543": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "cursor:docs/project_review_boundaries.md",
            "report:docs/project_review_2543-2119.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-09-08T03:06:45Z",
          "report": "docs/project_review_2543-2119.md",
          "status": "legacy",
          "title": "docs: name every flattenItemInstances consumer without a stale count"
        },
        "255": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_262-248.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-06-27T23:38:54Z",
          "report": "docs/project_review_262-248.md",
          "status": "legacy",
          "title": "Fix #20: recalibrate FLOATING_LAVA threshold for deep contained pools"
        },
        "256": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_262-248.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-06-27T23:41:54Z",
          "report": "docs/project_review_262-248.md",
          "status": "legacy",
          "title": "Fix #140: cancel build placement on zoom-band transitions"
        },
        "2565": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [],
          "history": [],
          "merged_at": "2026-09-08T03:25:31Z",
          "report": null,
          "status": "never-reviewed",
          "title": "Relabel computeAmbientLight's curve to its own input convention"
        },
        "2566": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [],
          "history": [],
          "merged_at": "2026-09-08T03:43:07Z",
          "report": null,
          "status": "never-reviewed",
          "title": "ci: retire the deleted fluid facade's three surviving references"
        },
        "2567": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [],
          "history": [],
          "merged_at": "2026-09-08T03:59:27Z",
          "report": null,
          "status": "never-reviewed",
          "title": "docs: fix loadVegetationYamlFn call-site comment cardinality claim"
        },
        "2568": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [],
          "history": [],
          "merged_at": "2026-09-08T04:14:34Z",
          "report": null,
          "status": "never-reviewed",
          "title": "docs: rewrite the Tier 3 damage derivation to the rotational swing and six-factor delivery"
        },
        "257": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_262-248.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-06-27T23:45:03Z",
          "report": "docs/project_review_262-248.md",
          "status": "legacy",
          "title": "Fix #138: tear down arena tile-editor popup on zoom-band transitions"
        },
        "2570": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [],
          "history": [],
          "merged_at": "2026-09-08T04:30:34Z",
          "report": null,
          "status": "never-reviewed",
          "title": "Fix immediate-pause observation in orphan-prune probe"
        },
        "2571": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [],
          "history": [],
          "merged_at": "2026-09-08T04:47:21Z",
          "report": null,
          "status": "never-reviewed",
          "title": "docs: fix component-owner layering statement in Save.Component.Types"
        },
        "2572": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [],
          "history": [],
          "merged_at": "2026-09-08T05:22:32Z",
          "report": null,
          "status": "never-reviewed",
          "title": "Migrate ten manual probes to the flake protocol"
        },
        "2573": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [],
          "history": [],
          "merged_at": "2026-09-08T05:06:45Z",
          "report": null,
          "status": "never-reviewed",
          "title": "Reduce normal startup delay with budgeted queue draining"
        },
        "2574": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [],
          "history": [],
          "merged_at": "2026-09-08T05:30:36Z",
          "report": null,
          "status": "never-reviewed",
          "title": "Preserve approved synthesized menu sound references"
        },
        "2575": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [],
          "history": [],
          "merged_at": "2026-09-08T05:46:24Z",
          "report": null,
          "status": "never-reviewed",
          "title": "Fix circadian.getCircadianUrge doc comment call shape"
        },
        "2576": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [],
          "history": [],
          "merged_at": "2026-09-08T06:01:51Z",
          "report": null,
          "status": "never-reviewed",
          "title": "Fix instant-built seeding comment: SeedWhenBuilt, not SeedAtSpawn"
        },
        "2577": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [],
          "history": [],
          "merged_at": "2026-09-08T06:19:32Z",
          "report": null,
          "status": "never-reviewed",
          "title": "Apply stance recovery atomically against the current stored value"
        },
        "2578": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [],
          "history": [],
          "merged_at": "2026-09-08T14:01:14Z",
          "report": null,
          "status": "never-reviewed",
          "title": "Decode save fixtures through a prebuilt executable instead of a cabal repl of the test suite"
        },
        "2579": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [],
          "history": [],
          "merged_at": "2026-09-08T14:43:30Z",
          "report": null,
          "status": "never-reviewed",
          "title": "Resolve unlike-fluid contact by annihilation in every active-sim transfer path"
        },
        "258": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_262-248.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-06-28T00:01:28Z",
          "report": "docs/project_review_262-248.md",
          "status": "legacy",
          "title": "Fix #154: gate game.onMouseDown gameplay actions on active world"
        },
        "2580": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [],
          "history": [],
          "merged_at": "2026-09-08T13:23:44Z",
          "report": null,
          "status": "never-reviewed",
          "title": "Retain sub-minute calendar progress across world ticks so the clock advances at default speed"
        },
        "2581": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [],
          "history": [],
          "merged_at": "2026-09-08T17:30:45Z",
          "report": null,
          "status": "never-reviewed",
          "title": "Pilot a declarative registration contract on the UI namespace"
        },
        "2582": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [],
          "history": [],
          "merged_at": "2026-09-08T19:43:13Z",
          "report": null,
          "status": "never-reviewed",
          "title": "Generate deterministic spatial map-pyramid pages from world-generation parameters"
        },
        "2583": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [],
          "history": [],
          "merged_at": "2026-09-08T20:54:03Z",
          "report": null,
          "status": "never-reviewed",
          "title": "Enforce capacity-safe, acyclic nested ownership moves"
        },
        "2584": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [],
          "history": [],
          "merged_at": "2026-09-09T00:31:26Z",
          "report": null,
          "status": "never-reviewed",
          "title": "Load loot-profile definitions (#2499)"
        },
        "2585": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [],
          "history": [],
          "merged_at": "2026-09-09T04:33:51Z",
          "report": null,
          "status": "never-reviewed",
          "title": "Persist player knowledge of portable containers (#2512)"
        },
        "2586": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [],
          "history": [],
          "merged_at": "2026-09-09T11:57:00Z",
          "report": null,
          "status": "never-reviewed",
          "title": "Render structure construction from authored progress frames (#2488)"
        },
        "2587": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [],
          "history": [],
          "merged_at": "2026-09-09T14:29:01Z",
          "report": null,
          "status": "never-reviewed",
          "title": "Add the approved lantern item and sprite"
        },
        "2588": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [],
          "history": [],
          "merged_at": "2026-09-09T14:55:29Z",
          "report": null,
          "status": "never-reviewed",
          "title": "Add the pure faction identity and relation policy model (#2500)"
        },
        "2589": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [],
          "history": [],
          "merged_at": "2026-09-09T18:47:50Z",
          "report": null,
          "status": "never-reviewed",
          "title": "Measure Lua-to-Haskell calls with runtime-local telemetry"
        },
        "259": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_262-248.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-06-28T00:01:34Z",
          "report": "docs/project_review_262-248.md",
          "status": "legacy",
          "title": "Fix #31: add hover feedback for slider, toggle, and randbox widgets"
        },
        "2590": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [],
          "history": [],
          "merged_at": "2026-09-09T18:21:33Z",
          "report": null,
          "status": "never-reviewed",
          "title": "Make the building preview inspect every direction and lifecycle role (#2492)"
        },
        "2592": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [],
          "history": [],
          "merged_at": "2026-09-09T22:29:01Z",
          "report": null,
          "status": "never-reviewed",
          "title": "Decide review-gate staleness by replaying the approved head (#2591)"
        },
        "2593": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [],
          "history": [],
          "merged_at": "2026-09-10T13:02:40Z",
          "report": null,
          "status": "never-reviewed",
          "title": "Fence in-flight fluid writebacks with the page's incarnation epoch"
        },
        "2594": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [],
          "history": [],
          "merged_at": "2026-09-10T14:35:05Z",
          "report": null,
          "status": "never-reviewed",
          "title": "Carry residual elapsed time across path waypoints (#2473)"
        },
        "2595": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [],
          "history": [],
          "merged_at": "2026-09-10T15:30:30Z",
          "report": null,
          "status": "never-reviewed",
          "title": "Measure world-map page codecs and bounded disk-cache tradeoffs"
        },
        "2597": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [],
          "history": [],
          "merged_at": "2026-09-10T15:56:28Z",
          "report": null,
          "status": "never-reviewed",
          "title": "Honor station queue order when workers choose a bill (#2523)"
        },
        "2598": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [],
          "history": [],
          "merged_at": "2026-09-10T16:46:39Z",
          "report": null,
          "status": "never-reviewed",
          "title": "Generate and approve eighth-level fluid masks (#2525)"
        },
        "2599": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [],
          "history": [],
          "merged_at": "2026-09-10T23:05:49Z",
          "report": null,
          "status": "never-reviewed",
          "title": "Retire page-owned units and buildings on single-page destroy and same-id re-init (#2476)"
        },
        "260": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_279-261.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-06-28T15:11:40Z",
          "report": "docs/project_review_279-261.md",
          "status": "legacy",
          "title": "Fix #67: per-instance item identity so UI actions hit the clicked item"
        },
        "2600": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [],
          "history": [],
          "merged_at": "2026-09-10T23:31:42Z",
          "report": null,
          "status": "never-reviewed",
          "title": "Preserve combat stamina costs during physiology updates and evaluate exhaustion from committed values"
        },
        "2601": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [],
          "history": [],
          "merged_at": "2026-09-11T00:06:37Z",
          "report": null,
          "status": "never-reviewed",
          "title": "Release ground-repair jobs when the worker no longer owns the target (#2531)"
        },
        "2602": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [],
          "history": [],
          "merged_at": "2026-09-11T01:26:59Z",
          "report": null,
          "status": "never-reviewed",
          "title": "Render flat fluid tops and give one-z drops a side face (#2517)"
        },
        "2603": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [],
          "history": [],
          "merged_at": "2026-09-11T01:03:17Z",
          "report": null,
          "status": "never-reviewed",
          "title": "Publish the canonical flora visual-state and fallback contract (#2530)"
        },
        "2604": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [],
          "history": [],
          "merged_at": "2026-09-11T03:31:48Z",
          "report": null,
          "status": "never-reviewed",
          "title": "Run the persistence-contract probe through the prebuilt decoder (#2274)"
        },
        "2605": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [],
          "history": [],
          "merged_at": "2026-09-11T23:14:35Z",
          "report": null,
          "status": "never-reviewed",
          "title": "Solidify the lava-water reaction product into durable stone through the world edit log"
        },
        "2606": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [],
          "history": [],
          "merged_at": "2026-09-11T23:49:16Z",
          "report": null,
          "status": "never-reviewed",
          "title": "Add engine audio and an interactive preview player"
        },
        "2607": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [],
          "history": [],
          "merged_at": "2026-09-12T00:42:25Z",
          "report": null,
          "status": "never-reviewed",
          "title": "Add approved saguaro juvenile living/dead pair"
        },
        "2608": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [],
          "history": [],
          "merged_at": "2026-09-12T04:14:33Z",
          "report": null,
          "status": "never-reviewed",
          "title": "farm: select the nearest CLAIMABLE designation (#2534)"
        },
        "2609": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [],
          "history": [],
          "merged_at": "2026-09-12T05:14:12Z",
          "report": null,
          "status": "never-reviewed",
          "title": "survival: restrict hydration recovery to actual source drinking (#2541)"
        },
        "261": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_279-261.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-06-28T00:23:35Z",
          "report": "docs/project_review_279-261.md",
          "status": "legacy",
          "title": "Fix #254: converge despike on residual peak pillars (TERRAIN_SPIKE)"
        },
        "2610": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [],
          "history": [],
          "merged_at": "2026-09-12T13:25:12Z",
          "report": null,
          "status": "never-reviewed",
          "title": "chop: select the nearest CLAIMABLE designated tree (#2536)"
        },
        "2612": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [],
          "history": [],
          "merged_at": "2026-09-12T15:07:27Z",
          "report": null,
          "status": "never-reviewed",
          "title": "Add approved charred saguaro art"
        },
        "2613": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [],
          "history": [],
          "merged_at": "2026-09-12T15:29:23Z",
          "report": null,
          "status": "never-reviewed",
          "title": "mine: select the nearest WORKABLE designation (#2538)"
        },
        "2614": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [],
          "history": [],
          "merged_at": "2026-09-12T16:02:32Z",
          "report": null,
          "status": "never-reviewed",
          "title": "preview: resynchronize the audio pane on every catalog reload (#2611)"
        },
        "2615": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [],
          "history": [],
          "merged_at": "2026-09-12T17:56:08Z",
          "report": null,
          "status": "never-reviewed",
          "title": "survival: score water actions off the emptiest canteen, not the first (#2546)"
        },
        "2616": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [],
          "history": [],
          "merged_at": "2026-09-12T18:16:43Z",
          "report": null,
          "status": "never-reviewed",
          "title": "craft: plan ingredient sourcing per cycle, not per claim (#2524)"
        },
        "2617": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [],
          "history": [],
          "merged_at": "2026-09-12T21:23:02Z",
          "report": null,
          "status": "never-reviewed",
          "title": "survival: use the frame-based fat floor for organ failure (#2556)"
        },
        "2618": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [],
          "history": [],
          "merged_at": "2026-09-13T01:12:04Z",
          "report": null,
          "status": "never-reviewed",
          "title": "farming: recheck proximity when resuming harvest-yield collection (#2550)"
        },
        "2619": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [],
          "history": [],
          "merged_at": "2026-09-13T02:40:49Z",
          "report": null,
          "status": "never-reviewed",
          "title": "docs: drop the obsolete arena save-test prohibition (#2569)"
        },
        "262": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_262-248.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-06-28T00:23:18Z",
          "report": "docs/project_review_262-248.md",
          "status": "legacy",
          "title": "Fix #132: clear chunk/tile selection on zoom-band transitions"
        },
        "2620": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [],
          "history": [],
          "merged_at": "2026-09-13T04:47:18Z",
          "report": null,
          "status": "never-reviewed",
          "title": "fix: bound and release the source-drink phase lock (#2545)"
        },
        "2621": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [],
          "history": [],
          "merged_at": "2026-09-13T16:15:52Z",
          "report": null,
          "status": "never-reviewed",
          "title": "[foraging] Preserve the edible target when harvesting a shared flora tile"
        },
        "2622": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [],
          "history": [],
          "merged_at": "2026-09-14T16:29:25Z",
          "report": null,
          "status": "never-reviewed",
          "title": "Destroy units and ground items caught at a solidifying cell (#2490)"
        },
        "2623": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [],
          "history": [],
          "merged_at": "2026-09-14T18:03:45Z",
          "report": null,
          "status": "never-reviewed",
          "title": "feat: realize a loot profile deterministically into a container (#2502)"
        },
        "2624": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [],
          "history": [],
          "merged_at": "2026-09-17T15:56:58Z",
          "report": null,
          "status": "never-reviewed",
          "title": "feat: spawn pending container shells from location content entries (#2505)"
        },
        "2626": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [],
          "history": [],
          "merged_at": "2026-09-17T16:31:15Z",
          "report": null,
          "status": "never-reviewed",
          "title": "[CRS-2] Measure detailed-chunk memory and process high-water"
        },
        "263": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_279-261.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-06-28T00:36:07Z",
          "report": "docs/project_review_279-261.md",
          "status": "legacy",
          "title": "Fix #123: live pick on click instead of stale cached hover"
        },
        "2632": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [],
          "history": [],
          "merged_at": "2026-09-17T17:28:15Z",
          "report": null,
          "status": "never-reviewed",
          "title": "Dismiss open dropdowns on Escape regardless of how many were ever created"
        },
        "2635": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [],
          "history": [],
          "merged_at": "2026-09-17T18:02:24Z",
          "report": null,
          "status": "never-reviewed",
          "title": "Sync the list widget's scrollbar when its items are replaced"
        },
        "2657": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [],
          "history": [],
          "merged_at": "2026-09-17T18:50:15Z",
          "report": null,
          "status": "never-reviewed",
          "title": "Refuse pose transitions out of the terminal Dead pose at command execution"
        },
        "2658": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [],
          "history": [],
          "merged_at": "2026-09-17T19:11:54Z",
          "report": null,
          "status": "never-reviewed",
          "title": "Report failed saveLoaded teardown hooks in the load's reconciliation outcome"
        },
        "2659": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [],
          "history": [],
          "merged_at": "2026-09-17T21:22:36Z",
          "report": null,
          "status": "never-reviewed",
          "title": "Verify retained payload bytes before reusing a generated-library entry as unchanged"
        },
        "266": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_279-261.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-06-28T00:47:24Z",
          "report": "docs/project_review_279-261.md",
          "status": "legacy",
          "title": "Fix #148: gate armed debug spawn/edit clicks on the gameplay view"
        },
        "2660": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [],
          "history": [],
          "merged_at": "2026-09-17T21:50:26Z",
          "report": null,
          "status": "never-reviewed",
          "title": "Give treatment a unique wound identity so same-time wounds are not mutated together"
        },
        "2661": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [],
          "history": [],
          "merged_at": "2026-09-18T00:29:23Z",
          "report": null,
          "status": "never-reviewed",
          "title": "Connect power networks across the cylindrical seam"
        },
        "2662": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [],
          "history": [],
          "merged_at": "2026-09-18T00:52:59Z",
          "report": null,
          "status": "never-reviewed",
          "title": "Fetch antibiotics per medicine and bound the futile-cure loop"
        },
        "2663": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [],
          "history": [],
          "merged_at": "2026-09-18T14:12:58Z",
          "report": null,
          "status": "never-reviewed",
          "title": "Count only able-bodied workers toward building construction progress and recruitment"
        },
        "2664": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [],
          "history": [],
          "merged_at": "2026-09-18T17:37:30Z",
          "report": null,
          "status": "never-reviewed",
          "title": "Carry the resource tick's sub-binary32 remainder instead of dropping it"
        },
        "2665": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [],
          "history": [],
          "merged_at": "2026-09-18T19:45:05Z",
          "report": null,
          "status": "never-reviewed",
          "title": "Reject case-insensitive aliases of reserved generated-library file names"
        },
        "2666": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [],
          "history": [],
          "merged_at": "2026-09-18T20:08:53Z",
          "report": null,
          "status": "never-reviewed",
          "title": "Rank only medics that can discover the patient they are ranked for"
        },
        "2667": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [],
          "history": [],
          "merged_at": "2026-09-19T13:25:26Z",
          "report": null,
          "status": "never-reviewed",
          "title": "Commit a typed dropdown edit on focus loss instead of discarding it"
        },
        "2668": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [],
          "history": [],
          "merged_at": "2026-09-20T01:33:20Z",
          "report": null,
          "status": "never-reviewed",
          "title": "Rebuild blood volume once bleeding is fully stabilized"
        },
        "2669": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [],
          "history": [],
          "merged_at": "2026-09-20T14:26:10Z",
          "report": null,
          "status": "never-reviewed",
          "title": "Decide starvation lean-floor death within a Float rounding tolerance"
        },
        "267": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_279-261.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-06-28T00:46:43Z",
          "report": "docs/project_review_279-261.md",
          "status": "legacy",
          "title": "Fix #176: clear building selection on zoom/menu transitions"
        },
        "2670": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [],
          "history": [],
          "merged_at": "2026-09-20T15:07:13Z",
          "report": null,
          "status": "never-reviewed",
          "title": "Credit autonomous canteen drinking from the drain the engine actually applied (#2631)"
        },
        "2671": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [],
          "history": [],
          "merged_at": "2026-09-20T20:03:54Z",
          "report": null,
          "status": "never-reviewed",
          "title": "Add an identity-preserving ground-item move operation (#2486)"
        },
        "2672": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [],
          "history": [],
          "merged_at": "2026-09-20T20:29:47Z",
          "report": null,
          "status": "never-reviewed",
          "title": "Make eighth-z fluid state exact, conserved, and durable (#2520)"
        },
        "2673": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [],
          "history": [],
          "merged_at": "2026-09-21T02:11:53Z",
          "report": null,
          "status": "never-reviewed",
          "title": "Show portable containers in the container window (#2527)"
        },
        "2674": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [],
          "history": [],
          "merged_at": "2026-09-21T04:46:52Z",
          "report": null,
          "status": "never-reviewed",
          "title": "Add immediate structure teardown with transient destruction playback (#2491)"
        },
        "2675": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [],
          "history": [],
          "merged_at": "2026-09-21T11:56:00Z",
          "report": null,
          "status": "never-reviewed",
          "title": "Add validated faction-tag definitions and legacy mappings (#2506)"
        },
        "2676": {
          "claim": null,
          "commit": "7adcfa31c1e007b7c5409e9e3876b6f79d32911b",
          "completed_at": "2026-09-22T15:04:04Z",
          "evidence": [],
          "history": [
            {
              "commit": "7adcfa31c1e007b7c5409e9e3876b6f79d32911b",
              "completed_at": "2026-09-22T15:04:04Z",
              "fixes": [],
              "kind": "attempt",
              "outcome": "clean",
              "recurrences": [],
              "repeats": [],
              "report": null,
              "token": "9c012895f70b95de1bb4d87f54929b66"
            }
          ],
          "merged_at": "2026-09-21T13:57:41Z",
          "report": null,
          "status": "clean",
          "title": "Ignore treatment claims held by dead or collapsed medics (#2642)"
        },
        "268": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_279-261.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-06-28T07:27:18Z",
          "report": "docs/project_review_279-261.md",
          "status": "legacy",
          "title": "Fix #183: clear tile/chunk cursor selection on world hide"
        },
        "269": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_279-261.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-06-28T07:27:47Z",
          "report": "docs/project_review_279-261.md",
          "status": "legacy",
          "title": "Fix #21: refine lake-hole / water-water-cliff audit classification"
        },
        "270": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_279-261.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-06-28T07:02:45Z",
          "report": "docs/project_review_279-261.md",
          "status": "legacy",
          "title": "Fix #37: retire dead popup button plumbing for the line-click model"
        },
        "271": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_279-261.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-06-28T16:05:46Z",
          "report": "docs/project_review_279-261.md",
          "status": "legacy",
          "title": "Fix #64: arm the gen-complete structural rebind on the save-load path"
        },
        "272": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_279-261.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-06-28T16:31:51Z",
          "report": "docs/project_review_279-261.md",
          "status": "legacy",
          "title": "Per-unit generated names (#264)"
        },
        "278": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_279-261.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-06-28T16:35:58Z",
          "report": "docs/project_review_279-261.md",
          "status": "legacy",
          "title": "Use YAML portrait assets in unit_info_v2 with live-frame fallback (#36)"
        },
        "279": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_279-261.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-06-28T16:47:21Z",
          "report": "docs/project_review_279-261.md",
          "status": "legacy",
          "title": "Fix #225: steep mountain faces shed soil to bare rock"
        },
        "280": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_292-281.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-06-28T16:56:41Z",
          "report": "docs/project_review_292-281.md",
          "status": "legacy",
          "title": "Multi-key KeyBindings model + array config format (#274)"
        },
        "281": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_292-281.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-06-28T16:48:04Z",
          "report": "docs/project_review_292-281.md",
          "status": "legacy",
          "title": "Remove obsolete structure_test.lua harness (#70)"
        },
        "282": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_292-281.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-06-28T17:08:00Z",
          "report": "docs/project_review_292-281.md",
          "status": "legacy",
          "title": "Race-safe, late-texture-robust world render-cache invalidation (#35)"
        },
        "283": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_292-281.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-06-28T17:50:51Z",
          "report": "docs/project_review_292-281.md",
          "status": "legacy",
          "title": "[keybinds] C \u2014 Keybind Lua API (#276)"
        },
        "284": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_292-281.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-06-28T18:54:48Z",
          "report": "docs/project_review_292-281.md",
          "status": "legacy",
          "title": "Fix #224: exposed hard rock slopes & reads as jagged peaks"
        },
        "285": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_292-281.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-06-28T17:30:15Z",
          "report": "docs/project_review_292-281.md",
          "status": "legacy",
          "title": "Restore phase-3 structural rebind \u2014 fix magenta interior regression from #282"
        },
        "287": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_292-281.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-06-28T19:33:07Z",
          "report": "docs/project_review_292-281.md",
          "status": "legacy",
          "title": "Restructure SaveData into per-world WorldPageSave + globals (#215)"
        },
        "288": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_292-281.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-06-28T19:56:30Z",
          "report": "docs/project_review_292-281.md",
          "status": "legacy",
          "title": "Fix #221: extend river sources inland to their catchment divides"
        },
        "289": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_292-281.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-06-28T19:12:36Z",
          "report": "docs/project_review_292-281.md",
          "status": "legacy",
          "title": "Fix #136: stop unit_info_v2 hiding the shared HUD info panel"
        },
        "290": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_292-281.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-06-28T20:07:30Z",
          "report": "docs/project_review_292-281.md",
          "status": "legacy",
          "title": "Fix #275: route camera pan + Q/E + Home through the binding system"
        },
        "291": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_292-281.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-06-28T21:01:20Z",
          "report": "docs/project_review_292-281.md",
          "status": "legacy",
          "title": "Fix #88: data-driven location definitions"
        },
        "292": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_292-281.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-06-28T21:18:30Z",
          "report": "docs/project_review_292-281.md",
          "status": "legacy",
          "title": "Fix #286: GPU handle\u2192slot indirection for the world render cache"
        },
        "293": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_341-296.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-06-29T14:25:33Z",
          "report": "docs/project_review_341-296.md",
          "status": "legacy",
          "title": "All-worlds save: persist & restore every world page (#216/#217/#218)"
        },
        "294": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_341-296.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-06-29T13:21:35Z",
          "report": "docs/project_review_341-296.md",
          "status": "legacy",
          "title": "Input settings tab keybind editor; remove System tab (#277)"
        },
        "295": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_341-296.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-06-29T02:51:32Z",
          "report": "docs/project_review_341-296.md",
          "status": "legacy",
          "title": "Construction designation tool (#95)"
        },
        "296": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_341-296.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-06-29T00:52:32Z",
          "report": "docs/project_review_341-296.md",
          "status": "legacy",
          "title": "Finish #92: activity-scaled hunger/hydration drain + calorie\u2194thermo/heal coupling + feed API"
        },
        "310": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_341-296.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-06-29T16:41:04Z",
          "report": "docs/project_review_341-296.md",
          "status": "legacy",
          "title": "Fix #297: clamp camera.gotoTile inside the glacier rim"
        },
        "311": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_341-296.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-06-29T15:21:28Z",
          "report": "docs/project_review_341-296.md",
          "status": "legacy",
          "title": "Fix #307: route lunge stamina gate through stats.get(max_stamina)"
        },
        "314": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_341-296.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-06-29T15:46:57Z",
          "report": "docs/project_review_341-296.md",
          "status": "legacy",
          "title": "Fix #309: throttle failed building spawns (no per-frame retry/log flood)"
        },
        "318": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_341-296.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-06-29T16:25:58Z",
          "report": "docs/project_review_341-296.md",
          "status": "legacy",
          "title": "Fix #308: elevation-correct unit thermo ambient (altitude lapse rate)"
        },
        "32": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_32-14.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-06-25T14:03:33Z",
          "report": "docs/project_review_32-14.md",
          "status": "legacy",
          "title": "Unify river dry-gap threshold across river tooling (#24)"
        },
        "320": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_341-296.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-06-29T16:41:26Z",
          "report": "docs/project_review_341-296.md",
          "status": "legacy",
          "title": "Fix #304: hysteresis on the collapse\u2194crawl locomotor boundary"
        },
        "322": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_341-296.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-06-29T16:25:43Z",
          "report": "docs/project_review_341-296.md",
          "status": "legacy",
          "title": "Fix #319: debug-console JSON serializer emits valid JSON for inf/nan + control chars"
        },
        "33": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_71-33.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-06-25T14:25:28Z",
          "report": "docs/project_review_71-33.md",
          "status": "legacy",
          "title": "Fix flora annual-cycle rendering to use day-of-year, not day-of-month (#25)"
        },
        "338": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_398-348.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-06-29T17:51:28Z",
          "report": "docs/project_review_398-348.md",
          "status": "legacy",
          "title": "Dedupe wrapChunkCoordU to a single canonical source (#316)"
        },
        "339": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_341-296.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-06-29T17:37:17Z",
          "report": "docs/project_review_341-296.md",
          "status": "legacy",
          "title": "Fix #317: clamp chemical-erosion intensity to [0,1]"
        },
        "34": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_71-33.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-06-25T14:29:15Z",
          "report": "docs/project_review_71-33.md",
          "status": "legacy",
          "title": "Document world_determinism as a content-identity check (#23)"
        },
        "340": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_398-348.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-06-29T17:52:51Z",
          "report": "docs/project_review_398-348.md",
          "status": "legacy",
          "title": "Fix #323: correct building.getStorageWeight docstring"
        },
        "341": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_341-296.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-06-29T17:48:08Z",
          "report": "docs/project_review_341-296.md",
          "status": "legacy",
          "title": "Fix #321: round-trip non-finite numbers in the Lua save serializer"
        },
        "348": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_398-348.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-06-29T17:51:09Z",
          "report": "docs/project_review_398-348.md",
          "status": "legacy",
          "title": "Fix #313: rename dangling item sprites (antibiotics, raw_fluorite)"
        },
        "354": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_398-348.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-06-29T18:37:10Z",
          "report": "docs/project_review_398-348.md",
          "status": "legacy",
          "title": "#305: movement speed accounts for encumbrance (carried load \u00f7 capacity), eased by endurance"
        },
        "355": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_398-348.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-06-30T00:12:17Z",
          "report": "docs/project_review_398-348.md",
          "status": "legacy",
          "title": "Fix #337: give exposed rock jagged slopes directional variety"
        },
        "356": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_398-348.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-06-29T21:38:03Z",
          "report": "docs/project_review_398-348.md",
          "status": "legacy",
          "title": "Fix #306: re-derive AI utility ladder against FOLLOW_COMMAND_UTILITY=7.0"
        },
        "362": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_398-348.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-06-30T00:12:48Z",
          "report": "docs/project_review_398-348.md",
          "status": "legacy",
          "title": "Fix #312: surface material affects unit movement (sand slower than rock)"
        },
        "363": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_398-348.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-06-29T22:47:45Z",
          "report": "docs/project_review_398-348.md",
          "status": "legacy",
          "title": "Fix #298: world-thread heap overflow loading glacier-rim chunks"
        },
        "364": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_398-348.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-06-30T02:41:15Z",
          "report": "docs/project_review_398-348.md",
          "status": "legacy",
          "title": "Fix #300: unit.repairItem engine primitive + Lua API"
        },
        "366": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_398-348.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-06-30T02:10:33Z",
          "report": "docs/project_review_398-348.md",
          "status": "legacy",
          "title": "Add #219 headless multi-world save/load regression test"
        },
        "368": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_398-348.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-06-30T03:27:13Z",
          "report": "docs/project_review_398-348.md",
          "status": "legacy",
          "title": "Fix #367: tile select honours the picked z, not the column surface"
        },
        "398": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_398-348.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-06-30T13:56:56Z",
          "report": "docs/project_review_398-348.md",
          "status": "legacy",
          "title": "Fix #378: correct misnamed colors in Math.colorToVec4 (opaque, green/lime)"
        },
        "399": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_411-399.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-06-30T13:57:35Z",
          "report": "docs/project_review_411-399.md",
          "status": "legacy",
          "title": "Fix #377: unit.list() scopes to the active world page"
        },
        "40": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_71-33.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-06-26T12:32:11Z",
          "report": "docs/project_review_71-33.md",
          "status": "legacy",
          "title": "Render water side faces across chunk boundaries (#26)"
        },
        "400": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_411-399.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-06-30T14:05:21Z",
          "report": "docs/project_review_411-399.md",
          "status": "legacy",
          "title": "#386: Remove vestigial usFallImpact / computeFallImpact scalar"
        },
        "401": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_411-399.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-06-30T15:28:24Z",
          "report": "docs/project_review_411-399.md",
          "status": "legacy",
          "title": "Fix #376: single shared woundEffSeverity helper (medic targeting, bleed display, injured-anim)"
        },
        "402": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_411-399.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-06-30T14:10:07Z",
          "report": "docs/project_review_411-399.md",
          "status": "legacy",
          "title": "Fix #382: remove the never-wired Event subsystem"
        },
        "404": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_411-399.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-06-30T14:48:02Z",
          "report": "docs/project_review_411-399.md",
          "status": "legacy",
          "title": "Fix #383: engine dead-export/field sweep"
        },
        "405": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_411-399.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-06-30T14:42:05Z",
          "report": "docs/project_review_411-399.md",
          "status": "legacy",
          "title": "Fix #384: remove dead channel-mask pipeline + stale composeFluidMap parameters"
        },
        "406": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_411-399.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-06-30T14:42:02Z",
          "report": "docs/project_review_411-399.md",
          "status": "legacy",
          "title": "Fix #389: dedup diverged helper copies (resolveTextureH hit-box bug + 3 cleanups)"
        },
        "407": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_411-399.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-06-30T15:04:05Z",
          "report": "docs/project_review_411-399.md",
          "status": "legacy",
          "title": "Fix #396: dedupe broken-equipment overlay into scripts/ui/broken_overlay"
        },
        "408": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_411-399.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-06-30T15:08:24Z",
          "report": "docs/project_review_411-399.md",
          "status": "legacy",
          "title": "Fix #387: Lua-bridge + UI dead code sweep"
        },
        "409": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_411-399.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-06-30T15:31:03Z",
          "report": "docs/project_review_411-399.md",
          "status": "legacy",
          "title": "Fix #380: correct inaccurate Lua API docstrings"
        },
        "41": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_71-33.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-06-26T12:40:32Z",
          "report": "docs/project_review_71-33.md",
          "status": "legacy",
          "title": "Remove the no-op passive fluid simulation pass (#39)"
        },
        "410": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_411-399.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-06-30T15:43:43Z",
          "report": "docs/project_review_411-399.md",
          "status": "legacy",
          "title": "Fix #385: worldgen dead-code sweep (BoundarySide, plateAt, foldlM, wrapChunkX/Y, meander seed, dup isRiverCarveEvent)"
        },
        "411": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_411-399.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-06-30T15:49:56Z",
          "report": "docs/project_review_411-399.md",
          "status": "legacy",
          "title": "Fix #374: add diagnostic context to bare error partials on worker-thread paths"
        },
        "412": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_432-412.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-06-30T19:26:55Z",
          "report": "docs/project_review_432-412.md",
          "status": "legacy",
          "title": "World-gen location overlay (#89)"
        },
        "413": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_432-412.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-06-30T19:56:09Z",
          "report": "docs/project_review_432-412.md",
          "status": "legacy",
          "title": "Flatten location room footprint to the lowest ground level"
        },
        "416": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_432-412.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-06-30T22:36:04Z",
          "report": "docs/project_review_432-412.md",
          "status": "legacy",
          "title": "Keep location placements clear of water (#414)"
        },
        "417": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_432-412.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-07-01T00:34:50Z",
          "report": "docs/project_review_432-412.md",
          "status": "legacy",
          "title": "[locations][render] z-aware front-wall sort so a sunken room's rim occludes its walls (#415)"
        },
        "419": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_432-412.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-07-01T04:08:07Z",
          "report": "docs/project_review_432-412.md",
          "status": "legacy",
          "title": "[locations][render] lift flora/veg in front of a structure wall over the whole wall (#418)"
        },
        "420": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_432-412.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-07-01T14:31:06Z",
          "report": "docs/project_review_432-412.md",
          "status": "legacy",
          "title": "[docs] Fix stale `.claude/scripts` references in CLAUDE.md"
        },
        "425": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_432-412.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-07-01T16:53:32Z",
          "report": "docs/project_review_432-412.md",
          "status": "legacy",
          "title": "[docs] Fix misnumbered algorithm-walkthrough comments"
        },
        "429": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_432-412.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-07-01T18:13:16Z",
          "report": "docs/project_review_432-412.md",
          "status": "legacy",
          "title": "[docs] Fix stale facts in CLAUDE.md and tools/README.md"
        },
        "430": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_432-412.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-07-01T19:27:37Z",
          "report": "docs/project_review_432-412.md",
          "status": "legacy",
          "title": "Reorganize assets/textures/ by role (#428)"
        },
        "431": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_432-412.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-07-01T21:08:18Z",
          "report": "docs/project_review_432-412.md",
          "status": "legacy",
          "title": "Location content spawning: units, items, buildings, structures, loot tables (#90)"
        },
        "432": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_432-412.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-07-01T23:04:13Z",
          "report": "docs/project_review_432-412.md",
          "status": "legacy",
          "title": "Gate settings debug logInfo spam behind engine.logDebug"
        },
        "442": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_432-412.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-07-01T22:37:59Z",
          "report": "docs/project_review_432-412.md",
          "status": "legacy",
          "title": "#435: Enable -Wall + incomplete-pattern warnings in the production profile"
        },
        "444": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_432-412.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-07-01T22:17:57Z",
          "report": "docs/project_review_432-412.md",
          "status": "legacy",
          "title": "Parallelize module compilation + unlock RTS tuning (#443)"
        },
        "449": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_459-450.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-07-02T00:33:39Z",
          "report": "docs/project_review_459-450.md",
          "status": "legacy",
          "title": "#436: CI \u2014 Linux build + headless test gate; track world_check baselines (#421)"
        },
        "450": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_459-450.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-07-01T23:08:48Z",
          "report": "docs/project_review_459-450.md",
          "status": "legacy",
          "title": "Storable vectors for sprite-batch vertices (#445)"
        },
        "451": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_459-450.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-07-01T23:28:43Z",
          "report": "docs/project_review_459-450.md",
          "status": "legacy",
          "title": "Cache sorted per-layer world batches; frame loop linear-merges (#446)"
        },
        "452": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_459-450.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-07-01T23:43:02Z",
          "report": "docs/project_review_459-450.md",
          "status": "legacy",
          "title": "Add ntfy.sh push notifications for PR lifecycle"
        },
        "453": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_459-450.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-07-01T23:51:57Z",
          "report": "docs/project_review_459-450.md",
          "status": "legacy",
          "title": "Pan margin + parallel chunk build for quad-cache rebuilds (#447)"
        },
        "454": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_459-450.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-07-02T02:39:16Z",
          "report": "docs/project_review_459-450.md",
          "status": "legacy",
          "title": "#365: Arena pages survive save/load \u2014 rebuild via the flat builder instead of the generator"
        },
        "455": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_459-450.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-07-02T03:00:41Z",
          "report": "docs/project_review_459-450.md",
          "status": "legacy",
          "title": "#433: legacy fullscreen fallback uses .:? \u2014 missing key no longer resets the video config"
        },
        "456": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_459-450.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-07-02T03:04:07Z",
          "report": "docs/project_review_459-450.md",
          "status": "legacy",
          "title": "#434: init.lua \u2014 kill location_stamper + item_info_panel on shutdown; declare all script-id locals"
        },
        "457": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_459-450.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-07-02T03:54:42Z",
          "report": "docs/project_review_459-450.md",
          "status": "legacy",
          "title": "#91: Ruins location type \u2014 structure tile variants + room_small_damaged"
        },
        "458": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_459-450.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-07-02T04:08:13Z",
          "report": "docs/project_review_459-450.md",
          "status": "legacy",
          "title": "#93: two-layer hunger/digestion model + bulk food (quinoa sack)"
        },
        "459": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_459-450.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-07-02T13:46:37Z",
          "report": "docs/project_review_459-450.md",
          "status": "legacy",
          "title": "#96: build job AI \u2014 acolytes execute construction designations"
        },
        "460": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_459-450.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-07-02T13:19:51Z",
          "report": "docs/project_review_459-450.md",
          "status": "legacy",
          "title": "#94: foraging AI + interactive flora backend"
        },
        "461": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_474-461.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-07-02T15:21:43Z",
          "report": "docs/project_review_474-461.md",
          "status": "legacy",
          "title": "#97: wood resource \u2014 chop designation + tree felling AI + wood_log"
        },
        "462": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_474-461.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-07-02T18:59:49Z",
          "report": "docs/project_review_474-461.md",
          "status": "legacy",
          "title": "#325: crafting recipe data model + craft.* Lua API"
        },
        "463": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_474-461.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-07-02T20:08:01Z",
          "report": "docs/project_review_474-461.md",
          "status": "legacy",
          "title": "#332: flora growth runtime \u2014 derived age/phase/season on an advancing calendar"
        },
        "464": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_474-461.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-07-02T20:38:04Z",
          "report": "docs/project_review_474-461.md",
          "status": "legacy",
          "title": "#344: item temperature + cooling to ambient (iiTemp, save v68)"
        },
        "465": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_474-461.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-07-02T20:39:11Z",
          "report": "docs/project_review_474-461.md",
          "status": "legacy",
          "title": "#326 Work stations: furnace + workbench with operations, findStation, craft.executeAt"
        },
        "467": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_474-461.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-07-02T22:42:54Z",
          "report": "docs/project_review_474-461.md",
          "status": "legacy",
          "title": "#343 Crafted-output quality from crafter skill + knowledge-gated recipes"
        },
        "468": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_474-461.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-07-02T22:44:26Z",
          "report": "docs/project_review_474-461.md",
          "status": "legacy",
          "title": "#156 shared view-transition teardown registry"
        },
        "469": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_474-461.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-07-03T01:19:43Z",
          "report": "docs/project_review_474-461.md",
          "status": "legacy",
          "title": "#392 Item buffs gain a percent axis (ibPercent \u2192 smPercent)"
        },
        "47": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_71-33.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-06-26T13:06:45Z",
          "report": "docs/project_review_71-33.md",
          "status": "legacy",
          "title": "Remove the dead zoom-map river/lake preview flags (#28)"
        },
        "470": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_474-461.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-07-03T02:44:17Z",
          "report": "docs/project_review_474-461.md",
          "status": "legacy",
          "title": "#327 Smelting tier: ore + fuel \u2192 metal bars at the furnace"
        },
        "472": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_474-461.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-07-03T03:21:45Z",
          "report": "docs/project_review_474-461.md",
          "status": "legacy",
          "title": "#220 Coastline variety: tectonic steepness field drives coast profiles (PR 1 of 2)"
        },
        "473": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_474-461.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-07-03T13:54:20Z",
          "report": "docs/project_review_474-461.md",
          "status": "legacy",
          "title": "#223 River/lake bed depth: canyon rivers + graben rift lakes"
        },
        "474": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_474-461.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-07-03T14:27:59Z",
          "report": "docs/project_review_474-461.md",
          "status": "legacy",
          "title": "#265 Derived unit roles: emergent skill-derived labels weight work selection"
        },
        "475": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_491-475.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-07-03T14:48:43Z",
          "report": "docs/project_review_491-475.md",
          "status": "legacy",
          "title": "Purge stale phase/skeleton/slice milestone comments"
        },
        "476": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_491-475.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-07-03T14:49:45Z",
          "report": "docs/project_review_491-475.md",
          "status": "legacy",
          "title": "#441 Drop fromJust from the UPrelude re-export list"
        },
        "477": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_491-475.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-07-03T15:09:21Z",
          "report": "docs/project_review_491-475.md",
          "status": "legacy",
          "title": "#471 Replace cabal jsem semaphore with per-package ghc -j (concurrent worktree builds can't deadlock)"
        },
        "480": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_491-475.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-07-03T15:47:53Z",
          "report": "docs/project_review_491-475.md",
          "status": "legacy",
          "title": "Wire night perception into combat awareness (#315)"
        },
        "481": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_491-475.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-07-03T15:52:27Z",
          "report": "docs/project_review_491-475.md",
          "status": "legacy",
          "title": "[tech-debt] Name UI/Sim magic offsets + operator/logic nits"
        },
        "482": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_491-475.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-07-03T16:17:48Z",
          "report": "docs/project_review_491-475.md",
          "status": "legacy",
          "title": "#301 Repair model + stations/items (condition/sharpness as station-gated recipes)"
        },
        "484": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_491-475.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-07-03T16:32:52Z",
          "report": "docs/project_review_491-475.md",
          "status": "legacy",
          "title": "Refresh README to describe Synarchy as a full game"
        },
        "486": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_491-475.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-07-03T16:47:11Z",
          "report": "docs/project_review_491-475.md",
          "status": "legacy",
          "title": "[cbits] Harden lua_debug.c reentrancy trap + font_stb.c size checks"
        },
        "487": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_491-475.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-07-03T17:11:06Z",
          "report": "docs/project_review_491-475.md",
          "status": "legacy",
          "title": "Replace unsafePerformIO fresh-buffer idiom with unsafeCreate / runST"
        },
        "488": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_491-475.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-07-03T17:59:03Z",
          "report": "docs/project_review_491-475.md",
          "status": "legacy",
          "title": "Per-subset unknown-texture fallback + audit tool (#478)"
        },
        "490": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_491-475.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-07-03T17:36:42Z",
          "report": "docs/project_review_491-475.md",
          "status": "legacy",
          "title": "Document headless behavior probes + add opt-in aggregate runner"
        },
        "491": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_491-475.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-07-03T18:28:42Z",
          "report": "docs/project_review_491-475.md",
          "status": "legacy",
          "title": "Strictness uniformity: modifyTVar' + Data.Map.Strict on long-lived state"
        },
        "492": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_504-492.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-07-03T18:42:06Z",
          "report": "docs/project_review_504-492.md",
          "status": "legacy",
          "title": "Harden decodeUtf8 trust boundaries"
        },
        "493": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_504-492.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-07-03T18:55:08Z",
          "report": "docs/project_review_504-492.md",
          "status": "legacy",
          "title": "Enforce location min spacing across the cylindrical U seam"
        },
        "494": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_504-492.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-07-03T21:07:51Z",
          "report": "docs/project_review_504-492.md",
          "status": "legacy",
          "title": "Unify duplicated magic constants (blood ratio, UI layer, max_stamina, climb height, deco base, aware range)"
        },
        "495": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_504-492.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-07-03T22:15:34Z",
          "report": "docs/project_review_504-492.md",
          "status": "legacy",
          "title": "Make the front-wall vegetation lift seam-aware (#423)"
        },
        "496": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_504-492.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-07-03T22:46:24Z",
          "report": "docs/project_review_504-492.md",
          "status": "legacy",
          "title": "Remove stale TODO on the severed-wound icon (#372)"
        },
        "497": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_504-492.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-07-03T22:59:18Z",
          "report": "docs/project_review_504-492.md",
          "status": "legacy",
          "title": "Fix location stamp idempotency after anchor floor is cleared (#424)"
        },
        "498": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_504-492.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-07-03T23:25:08Z",
          "report": "docs/project_review_504-492.md",
          "status": "legacy",
          "title": "Make uphill travel cost move speed and stamina, not only routing weight (#375)"
        },
        "499": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_504-492.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-07-04T01:02:15Z",
          "report": "docs/project_review_504-492.md",
          "status": "legacy",
          "title": "Craft AI + bill backend: per-station orders drive production end to end (#329)"
        },
        "50": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_71-33.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-06-26T14:42:47Z",
          "report": "docs/project_review_71-33.md",
          "status": "legacy",
          "title": "Dedup already-pending chunks in world.loadChunksInRegion (#43)"
        },
        "501": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_504-492.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-07-04T01:12:35Z",
          "report": "docs/project_review_504-492.md",
          "status": "legacy",
          "title": "Fabrication tier: steel_bar -> tools, weapons, construction stock (#328)"
        },
        "502": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_504-492.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-07-04T01:25:27Z",
          "report": "docs/project_review_504-492.md",
          "status": "legacy",
          "title": "Profile the worldgen setup/timeline phase and document findings"
        },
        "503": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_504-492.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-07-04T01:44:03Z",
          "report": "docs/project_review_504-492.md",
          "status": "legacy",
          "title": "Quality tiers + tooltip descriptions (#345)"
        },
        "504": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_504-492.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-07-04T03:03:28Z",
          "report": "docs/project_review_504-492.md",
          "status": "legacy",
          "title": "#302 Repair AI: utility + designation + go-to-station-and-repair"
        },
        "505": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_516-505.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-07-04T03:17:14Z",
          "report": "docs/project_review_516-505.md",
          "status": "legacy",
          "title": "Fold construction designation into the build tool (#403)"
        },
        "506": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_516-505.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-07-04T03:36:14Z",
          "report": "docs/project_review_516-505.md",
          "status": "legacy",
          "title": "Power items + placeable solar panel & battery (#358)"
        },
        "507": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_516-505.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-07-04T04:33:33Z",
          "report": "docs/project_review_516-505.md",
          "status": "legacy",
          "title": "Crafting UI: bill/order queue + station panel (#330)"
        },
        "508": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_516-505.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-07-04T05:00:38Z",
          "report": "docs/project_review_516-505.md",
          "status": "legacy",
          "title": "Fix #500: share plate-base terrain across chunk-window borders"
        },
        "509": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_516-505.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-07-04T05:13:50Z",
          "report": "docs/project_review_516-505.md",
          "status": "legacy",
          "title": "Animate the unknown-unit fallback with idle/walk cycles (#485)"
        },
        "51": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_71-33.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-06-26T13:34:19Z",
          "report": "docs/project_review_71-33.md",
          "status": "legacy",
          "title": "Reject nonexistent page IDs in world.show (#48)"
        },
        "510": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_516-505.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-07-04T05:48:07Z",
          "report": "docs/project_review_516-505.md",
          "status": "legacy",
          "title": "Kitchen workshop + cooking skill/knowledge + coffee recipe (#346)"
        },
        "511": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_516-505.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-07-04T13:37:18Z",
          "report": "docs/project_review_516-505.md",
          "status": "legacy",
          "title": "Repair UI: player priority flag + condition/sharpness surfacing (#303)"
        },
        "512": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_516-505.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-07-04T18:35:24Z",
          "report": "docs/project_review_516-505.md",
          "status": "legacy",
          "title": "Longitude-local day/night: solar time varies around the world cylinder"
        },
        "513": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_516-505.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-07-04T21:41:16Z",
          "report": "docs/project_review_516-505.md",
          "status": "legacy",
          "title": "Add power-grid wire structure piece + connection-aware autotile (#359)"
        },
        "514": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_516-505.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-07-05T00:03:34Z",
          "report": "docs/project_review_516-505.md",
          "status": "legacy",
          "title": "Power-network simulation: connected components + energy balance (#360)"
        },
        "515": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_516-505.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-07-05T13:41:39Z",
          "report": "docs/project_review_516-505.md",
          "status": "legacy",
          "title": "[power] Powered workshops + consumers (requires_power + drain)"
        },
        "516": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_516-505.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-07-05T18:29:27Z",
          "report": "docs/project_review_516-505.md",
          "status": "legacy",
          "title": "Add tilling designation tool + AI mechanism (partial #333)"
        },
        "518": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_534-518.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-07-05T20:53:04Z",
          "report": "docs/project_review_534-518.md",
          "status": "legacy",
          "title": "Add crop content + two growth forms (#334)"
        },
        "519": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_534-518.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-07-05T23:31:45Z",
          "report": "docs/project_review_534-518.md",
          "status": "legacy",
          "title": "Add planting tool + suitability screen (#335)"
        },
        "52": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_71-33.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-06-26T13:45:41Z",
          "report": "docs/project_review_71-33.md",
          "status": "legacy",
          "title": "Treat dump-mode wait timeouts as seconds (#44)"
        },
        "520": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_534-518.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-07-06T00:23:39Z",
          "report": "docs/project_review_534-518.md",
          "status": "legacy",
          "title": "Remove dead scsGenFluid field (unused save-diff baseline)"
        },
        "521": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_534-518.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-07-06T03:32:25Z",
          "report": "docs/project_review_534-518.md",
          "status": "legacy",
          "title": "Add farm AI: plant + skill-gated auto-harvest + rot (#336)"
        },
        "522": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_534-518.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-07-06T05:30:27Z",
          "report": "docs/project_review_534-518.md",
          "status": "legacy",
          "title": "Make locomotion injury penalty data-driven (#393)"
        },
        "523": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_534-518.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-07-06T14:44:21Z",
          "report": "docs/project_review_534-518.md",
          "status": "legacy",
          "title": "Unify consciousness + mood into a state-of-mind model (#350)"
        },
        "524": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_534-518.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-07-06T16:05:27Z",
          "report": "docs/project_review_534-518.md",
          "status": "legacy",
          "title": "Thought system: periodic per-unit thoughts driven by mood/pain/environment"
        },
        "525": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_534-518.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-07-06T16:21:21Z",
          "report": "docs/project_review_534-518.md",
          "status": "legacy",
          "title": "Glacier evolution: remove dead retreat/melt terrain branches, fix stale TODOs"
        },
        "528": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_534-518.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-07-06T17:40:40Z",
          "report": "docs/project_review_534-518.md",
          "status": "legacy",
          "title": "Consumable drink effects scaled by quality + temperature (#347)"
        },
        "532": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_534-518.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-07-06T17:04:49Z",
          "report": "docs/project_review_534-518.md",
          "status": "legacy",
          "title": "CI: incremental builds via dist-newstyle + toolchain caching (#526)"
        },
        "533": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_534-518.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-07-06T18:10:31Z",
          "report": "docs/project_review_534-518.md",
          "status": "legacy",
          "title": "Add `make ci` local pre-push gate mirroring CI (#527)"
        },
        "534": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_534-518.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-07-07T00:07:57Z",
          "report": "docs/project_review_534-518.md",
          "status": "legacy",
          "title": "Extract shared probe harness (probelib) and migrate all probes (#529)"
        },
        "535": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_609-535.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-07-07T03:59:35Z",
          "report": "docs/project_review_609-535.md",
          "status": "legacy",
          "title": "Gate features in CI: path-selective, blocking behavior-probe job (#530)"
        },
        "536": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_609-535.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-07-07T13:38:39Z",
          "report": "docs/project_review_609-535.md",
          "status": "legacy",
          "title": "Parallel probe dispatch via run_probes.py --jobs (rescoped #531)"
        },
        "57": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_71-33.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-06-26T13:52:44Z",
          "report": "docs/project_review_71-33.md",
          "status": "legacy",
          "title": "Don't start a TCP listener in dump mode (#46)"
        },
        "594": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_609-535.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-07-07T14:43:10Z",
          "report": "docs/project_review_609-535.md",
          "status": "legacy",
          "title": "[tooling] Refresh tools/README.md for current probe runner behavior"
        },
        "595": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_609-535.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-07-07T15:06:58Z",
          "report": "docs/project_review_609-535.md",
          "status": "legacy",
          "title": "Show behavior-probe CI eligibility from the tooling"
        },
        "597": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_609-535.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-07-07T15:19:31Z",
          "report": "docs/project_review_609-535.md",
          "status": "legacy",
          "title": "Add whetstone texture"
        },
        "598": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_609-535.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-07-07T15:56:53Z",
          "report": "docs/project_review_609-535.md",
          "status": "legacy",
          "title": "Split Engine.Scripting.Lua.API.Craft into Recipe/Execute/Bill submodules"
        },
        "600": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_609-535.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-07-07T16:35:48Z",
          "report": "docs/project_review_609-535.md",
          "status": "legacy",
          "title": "Make infection_probe self-contained and CI-eligible"
        },
        "601": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_609-535.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-07-07T18:33:17Z",
          "report": "docs/project_review_609-535.md",
          "status": "legacy",
          "title": "Stabilize disarm_probe and promote it to CI eligibility"
        },
        "602": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_609-535.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-07-07T19:07:30Z",
          "report": "docs/project_review_609-535.md",
          "status": "legacy",
          "title": "Stabilize medic_coord_probe and promote it to CI eligibility (#589)"
        },
        "605": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_609-535.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-07-07T19:22:46Z",
          "report": "docs/project_review_609-535.md",
          "status": "legacy",
          "title": "Add furnace default/construction/destruction textures"
        },
        "608": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_609-535.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-07-08T01:08:42Z",
          "report": "docs/project_review_609-535.md",
          "status": "legacy",
          "title": "Make craft recipe power draw job-dependent"
        },
        "609": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_609-535.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-07-08T02:24:58Z",
          "report": "docs/project_review_609-535.md",
          "status": "legacy",
          "title": "Wire the furnace into the power grid; add machine shop content"
        },
        "614": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_627-614.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-07-08T03:10:14Z",
          "report": "docs/project_review_627-614.md",
          "status": "legacy",
          "title": "Split scripts/debug.lua into overlay mode modules"
        },
        "615": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_627-614.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-07-08T03:50:29Z",
          "report": "docs/project_review_627-614.md",
          "status": "legacy",
          "title": "Split unit_resources.lua into smaller physiology modules"
        },
        "616": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_627-614.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-07-08T04:19:35Z",
          "report": "docs/project_review_627-614.md",
          "status": "legacy",
          "title": "Add exhaustion meter: exertion-driven fatigue, endurance-scaled recovery"
        },
        "617": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_627-614.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-07-08T04:40:06Z",
          "report": "docs/project_review_627-614.md",
          "status": "legacy",
          "title": "Split app/Main.hs into boot and dump modules"
        },
        "619": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_627-614.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-07-08T05:05:19Z",
          "report": "docs/project_review_627-614.md",
          "status": "legacy",
          "title": "Split Engine.Core.Log into Types/Env/Format submodules"
        },
        "62": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_71-33.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-06-26T14:19:42Z",
          "report": "docs/project_review_71-33.md",
          "status": "legacy",
          "title": "Register engine.logError onto the Lua engine table (#53)"
        },
        "620": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_627-614.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-07-08T05:25:49Z",
          "report": "docs/project_review_627-614.md",
          "status": "legacy",
          "title": "Calibrate scroll-to-zoom by scroll amount, not callback count"
        },
        "621": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_627-614.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-07-08T05:56:16Z",
          "report": "docs/project_review_627-614.md",
          "status": "legacy",
          "title": "Add blood decal model and debug surface"
        },
        "623": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_627-614.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-07-08T06:21:43Z",
          "report": "docs/project_review_627-614.md",
          "status": "legacy",
          "title": "Split Unit.Thread.Command into smaller modules"
        },
        "624": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_627-614.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-07-08T06:47:15Z",
          "report": "docs/project_review_627-614.md",
          "status": "legacy",
          "title": "Split unit_ai.lua into smaller AI behavior modules"
        },
        "625": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_627-614.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-07-08T07:12:46Z",
          "report": "docs/project_review_627-614.md",
          "status": "legacy",
          "title": "Split Unit.Thread.Movement into smaller modules"
        },
        "626": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_627-614.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-07-08T07:38:15Z",
          "report": "docs/project_review_627-614.md",
          "status": "legacy",
          "title": "[blood] Generate and render procedural blood decal textures"
        },
        "627": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_627-614.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-07-08T08:03:47Z",
          "report": "docs/project_review_627-614.md",
          "status": "legacy",
          "title": "[tech-debt] Split World.Slope into smaller modules"
        },
        "628": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_654-628.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-07-08T09:09:24Z",
          "report": "docs/project_review_654-628.md",
          "status": "legacy",
          "title": "[blood] Spawn impact blood from new wounds"
        },
        "629": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_654-628.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-07-08T14:50:11Z",
          "report": "docs/project_review_654-628.md",
          "status": "legacy",
          "title": "Split Engine.Graphics.Font.Load into smaller modules"
        },
        "63": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_71-33.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-06-26T14:45:46Z",
          "report": "docs/project_review_71-33.md",
          "status": "legacy",
          "title": "Collapse over-length stride transitions to instant (#56)"
        },
        "630": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_654-628.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-07-08T15:08:43Z",
          "report": "docs/project_review_654-628.md",
          "status": "legacy",
          "title": "Add Strict/StrictData to Engine.Scripting.Lua.Types"
        },
        "631": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_654-628.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-07-08T15:34:16Z",
          "report": "docs/project_review_654-628.md",
          "status": "legacy",
          "title": "[circadian] Sleep pressure + circadian urge signal"
        },
        "633": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_654-628.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-07-08T15:59:49Z",
          "report": "docs/project_review_654-628.md",
          "status": "legacy",
          "title": "Split Combat.Wounds into smaller modules"
        },
        "634": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_654-628.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-07-08T16:50:49Z",
          "report": "docs/project_review_654-628.md",
          "status": "legacy",
          "title": "[tech-debt] Split World.Geology.Coastal into smaller modules"
        },
        "637": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_654-628.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-07-08T17:16:23Z",
          "report": "docs/project_review_654-628.md",
          "status": "legacy",
          "title": "Split unit_info_v2.lua into smaller unit-info panel modules"
        },
        "639": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_654-628.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-07-08T20:53:04Z",
          "report": "docs/project_review_654-628.md",
          "status": "legacy",
          "title": "Tilling: dedicated push animation (#517)"
        },
        "640": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_654-628.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-07-08T21:05:53Z",
          "report": "docs/project_review_654-628.md",
          "status": "legacy",
          "title": "Add Sleeping pose and go_to_sleep AI goal (#612)"
        },
        "651": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_654-628.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-07-08T20:50:56Z",
          "report": "docs/project_review_654-628.md",
          "status": "legacy",
          "title": "Fix CI: revert GHCUP_INSTALL_BASE_PREFIX (breaks GHC version selection)"
        },
        "653": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_654-628.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-07-08T23:02:39Z",
          "report": "docs/project_review_654-628.md",
          "status": "legacy",
          "title": "Split UI.Tooltip into smaller modules"
        },
        "654": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_654-628.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-07-08T23:23:11Z",
          "report": "docs/project_review_654-628.md",
          "status": "legacy",
          "title": "Split World.Generate.Config into smaller modules"
        },
        "655": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_668-655.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-07-08T23:48:44Z",
          "report": "docs/project_review_668-655.md",
          "status": "legacy",
          "title": "Split World.Thread.Command.Edit into focused submodules"
        },
        "656": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_668-655.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-07-09T00:14:13Z",
          "report": "docs/project_review_668-655.md",
          "status": "legacy",
          "title": "Split Engine.Input.Thread into smaller modules"
        },
        "657": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_668-655.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-07-09T00:39:43Z",
          "report": "docs/project_review_668-655.md",
          "status": "legacy",
          "title": "[tech-debt] Split UI.Manager into smaller modules"
        },
        "658": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_668-655.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-07-09T01:05:13Z",
          "report": "docs/project_review_668-655.md",
          "status": "legacy",
          "title": "Fix decodeUtf8 crash in Lua text API + byte-unsafe truncation trigger"
        },
        "659": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_668-655.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-07-09T01:50:30Z",
          "report": "docs/project_review_668-655.md",
          "status": "legacy",
          "title": "Give bear_brown a dawn-centered (nocturnal) circadian curve"
        },
        "660": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_668-655.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-07-09T02:15:58Z",
          "report": "docs/project_review_668-655.md",
          "status": "legacy",
          "title": "[tech-debt] Split Engine.Scripting.Lua.API registration into smaller modules"
        },
        "661": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_668-655.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-07-09T07:12:06Z",
          "report": "docs/project_review_668-655.md",
          "status": "legacy",
          "title": "Separate versioned config defaults from local runtime config state"
        },
        "662": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_668-655.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-07-09T07:40:08Z",
          "report": "docs/project_review_668-655.md",
          "status": "legacy",
          "title": "Split scripts/init.lua into lifecycle and gameplay input routers"
        },
        "663": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_668-655.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-07-09T08:10:26Z",
          "report": "docs/project_review_668-655.md",
          "status": "legacy",
          "title": "--preview Phase 1: boot skeleton \u2014 CLI dispatch + minimal graphical exec path"
        },
        "666": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_668-655.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-07-09T14:16:17Z",
          "report": "docs/project_review_668-655.md",
          "status": "legacy",
          "title": "Make Cabal metadata and source distribution checks pass"
        },
        "667": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_668-655.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-07-09T14:41:52Z",
          "report": "docs/project_review_668-655.md",
          "status": "legacy",
          "title": "[tech-debt] Split Engine.Scripting.Lua.API.Buildings into smaller modules"
        },
        "668": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_668-655.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-07-09T14:59:07Z",
          "report": "docs/project_review_668-655.md",
          "status": "legacy",
          "title": "[tech-debt] Split ui_manager.lua into UI lifecycle and event routing modules"
        },
        "669": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_681-669.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-07-09T15:13:55Z",
          "report": "docs/project_review_681-669.md",
          "status": "legacy",
          "title": "Split Engine.Scripting.Lua.API.Units into focused submodules"
        },
        "670": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_681-669.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-07-09T15:30:20Z",
          "report": "docs/project_review_681-669.md",
          "status": "legacy",
          "title": "Support runtime resource loading outside the repo working directory"
        },
        "672": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_681-669.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-07-09T15:45:07Z",
          "report": "docs/project_review_681-669.md",
          "status": "legacy",
          "title": "F3 \u2014 UI widget introspection oracle (ui.dumpWidgets)"
        },
        "673": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_681-669.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-07-09T16:12:28Z",
          "report": "docs/project_review_681-669.md",
          "status": "legacy",
          "title": "CI: bake GHC toolchain into the CI image (ci-v2); probe gate --jobs 2"
        },
        "674": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_681-669.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-07-09T16:39:54Z",
          "report": "docs/project_review_681-669.md",
          "status": "legacy",
          "title": "Split World.Thread.Command.Save into smaller modules"
        },
        "675": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_681-669.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-07-09T16:51:37Z",
          "report": "docs/project_review_681-669.md",
          "status": "legacy",
          "title": "[tech-debt] Split World.Geology.Timeline into smaller modules"
        },
        "676": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_681-669.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-07-09T17:44:19Z",
          "report": "docs/project_review_681-669.md",
          "status": "legacy",
          "title": "[tech-debt] Update CLAUDE.md tilling section + Wounds/Tick restMult comment"
        },
        "677": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_681-669.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-07-09T18:03:24Z",
          "report": "docs/project_review_681-669.md",
          "status": "legacy",
          "title": "[tech-debt] Split Engine.Scripting.Lua.API.Equipment into smaller modules"
        },
        "678": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_681-669.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-07-09T18:15:07Z",
          "report": "docs/project_review_681-669.md",
          "status": "legacy",
          "title": "[tech-debt] Split Engine.Scripting.Lua.API.WorldQuery into submodules"
        },
        "679": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_681-669.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-07-09T18:26:48Z",
          "report": "docs/project_review_681-669.md",
          "status": "legacy",
          "title": "Split Engine.Scripting.Lua.API.World into focused submodules"
        },
        "680": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_681-669.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-07-09T18:38:31Z",
          "report": "docs/project_review_681-669.md",
          "status": "legacy",
          "title": "[tech-debt] Split World.Render.Quads into smaller modules"
        },
        "681": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_681-669.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-07-09T18:50:14Z",
          "report": "docs/project_review_681-669.md",
          "status": "legacy",
          "title": "Split World.Fluid.River.Identify into smaller modules"
        },
        "682": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_693-682.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-07-09T19:01:55Z",
          "report": "docs/project_review_693-682.md",
          "status": "legacy",
          "title": "[tech-debt] Split World.Generate.Chunk into smaller modules"
        },
        "683": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_693-682.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-07-09T19:20:01Z",
          "report": "docs/project_review_693-682.md",
          "status": "legacy",
          "title": "C1: Player-facing minimal manual (docs/player_manual.md)"
        },
        "684": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_693-682.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-07-09T19:31:42Z",
          "report": "docs/project_review_693-682.md",
          "status": "legacy",
          "title": "[tech-debt] Split Combat.Resolution into smaller modules"
        },
        "685": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_693-682.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-07-09T19:43:23Z",
          "report": "docs/project_review_693-682.md",
          "status": "legacy",
          "title": "[tech-debt] Split Engine.Scripting.Lua.API.Forage into submodules"
        },
        "686": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_693-682.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-07-09T19:55:05Z",
          "report": "docs/project_review_693-682.md",
          "status": "legacy",
          "title": "[tech-debt] Split Engine.Scripting.Lua.API.Items into smaller modules"
        },
        "687": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_693-682.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-07-09T20:06:48Z",
          "report": "docs/project_review_693-682.md",
          "status": "legacy",
          "title": "F1: Vulkan framebuffer screenshot verb (debug.captureScreenshot)"
        },
        "688": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_693-682.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-07-09T20:20:33Z",
          "report": "docs/project_review_693-682.md",
          "status": "legacy",
          "title": "[tech-debt] Split Engine.Scripting.Lua.Thread into smaller modules"
        },
        "689": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_693-682.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-07-09T20:33:16Z",
          "report": "docs/project_review_693-682.md",
          "status": "legacy",
          "title": "Split Engine.Scripting.Lua.Message into smaller modules"
        },
        "690": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_693-682.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-07-09T21:05:07Z",
          "report": "docs/project_review_693-682.md",
          "status": "legacy",
          "title": "F2: Synthetic input injection verbs (input.click / moveMouse / key / scroll / type)"
        },
        "691": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_693-682.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-07-09T21:27:33Z",
          "report": "docs/project_review_693-682.md",
          "status": "legacy",
          "title": "Split World.Weather.Generate into smaller modules"
        },
        "692": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_693-682.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-07-09T21:37:19Z",
          "report": "docs/project_review_693-682.md",
          "status": "legacy",
          "title": "Split World.Hydrology.Simulation into focused submodules"
        },
        "693": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_693-682.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-07-09T21:48:59Z",
          "report": "docs/project_review_693-682.md",
          "status": "legacy",
          "title": "[tech-debt] Split World.Fluid.Lake.Identify into smaller modules"
        },
        "694": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_715-694.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-07-09T22:00:40Z",
          "report": "docs/project_review_715-694.md",
          "status": "legacy",
          "title": "[tech-debt] Split World.Geology.Erosion into smaller modules"
        },
        "695": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_715-694.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-07-09T22:12:22Z",
          "report": "docs/project_review_715-694.md",
          "status": "legacy",
          "title": "Split World.Plate into smaller modules"
        },
        "696": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_715-694.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-07-09T22:25:05Z",
          "report": "docs/project_review_715-694.md",
          "status": "legacy",
          "title": "H1: Player harness \u2014 lockstep runner + naive player agent + session trace"
        },
        "701": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_715-694.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-07-10T00:24:22Z",
          "report": "docs/project_review_715-694.md",
          "status": "legacy",
          "title": "H2: Critic \u2014 oracle-grounded friction triage + UX report"
        },
        "702": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_715-694.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-07-09T23:45:53Z",
          "report": "docs/project_review_715-694.md",
          "status": "legacy",
          "title": "[tech-debt] Split World.Thread.Command.Cursor into smaller modules"
        },
        "703": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_715-694.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-07-10T00:00:34Z",
          "report": "docs/project_review_715-694.md",
          "status": "legacy",
          "title": "[tech-debt] Split World.ZoomMap.Cache into smaller modules"
        },
        "704": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_715-694.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-07-10T04:13:01Z",
          "report": "docs/project_review_715-694.md",
          "status": "legacy",
          "title": "F4 \u2014 Rejected-action / silent-failure oracle tap (debug.drainActionOutcomes)"
        },
        "705": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_715-694.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-07-10T00:53:13Z",
          "report": "docs/project_review_715-694.md",
          "status": "legacy",
          "title": "Gate swapchain TRANSFER_SRC usage on surface capabilities"
        },
        "71": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_71-33.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-06-26T15:00:59Z",
          "report": "docs/project_review_71-33.md",
          "status": "legacy",
          "title": "Fix pause-menu Save calling nonexistent engine.save API (#54)"
        },
        "711": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_715-694.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-07-10T04:33:33Z",
          "report": "docs/project_review_715-694.md",
          "status": "legacy",
          "title": "Offscreen GPU render mode: --offscreen, window off, render on (#650)"
        },
        "712": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_715-694.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-07-10T04:45:30Z",
          "report": "docs/project_review_715-694.md",
          "status": "legacy",
          "title": "Model semantic proper names and render English glosses"
        },
        "714": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_715-694.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-07-10T13:14:52Z",
          "report": "docs/project_review_715-694.md",
          "status": "legacy",
          "title": "Fence synthetic modifier releases behind their action's Lua callbacks"
        },
        "715": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_715-694.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-07-10T14:25:56Z",
          "report": "docs/project_review_715-694.md",
          "status": "legacy",
          "title": "C2: Seed-driven persona generation for the playtest harness"
        },
        "716": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_739-716.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-07-10T14:52:02Z",
          "report": "docs/project_review_739-716.md",
          "status": "legacy",
          "title": "Mental states: stressed \u2192 break + euphoria over state_of_mind (#352)"
        },
        "718": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_739-716.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-07-10T15:22:15Z",
          "report": "docs/project_review_739-716.md",
          "status": "legacy",
          "title": "Record playtest trace phases truthfully; replay only executed phases"
        },
        "719": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_739-716.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-07-10T18:21:22Z",
          "report": "docs/project_review_739-716.md",
          "status": "legacy",
          "title": "Remove inert Create World controls"
        },
        "720": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_739-716.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-07-10T17:19:01Z",
          "report": "docs/project_review_739-716.md",
          "status": "legacy",
          "title": "Fix playtest event-log delta tracking"
        },
        "731": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_739-716.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-07-10T20:34:19Z",
          "report": "docs/project_review_739-716.md",
          "status": "legacy",
          "title": "Persist player-facing world identity separately from page and save names"
        },
        "732": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_739-716.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-07-11T14:19:22Z",
          "report": "docs/project_review_739-716.md",
          "status": "legacy",
          "title": "Make sequential input.* acks a real modifier-lifetime boundary"
        },
        "733": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_739-716.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-07-10T23:09:20Z",
          "report": "docs/project_review_739-716.md",
          "status": "legacy",
          "title": "Give every behavior probe a --port flag, retire fixed-port special cases"
        },
        "734": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_739-716.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-07-11T14:08:16Z",
          "report": "docs/project_review_739-716.md",
          "status": "legacy",
          "title": "Register the 12 orphaned behavior probes and sync probe docs"
        },
        "735": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_739-716.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-07-11T14:36:20Z",
          "report": "docs/project_review_739-716.md",
          "status": "legacy",
          "title": "Fix playtest critic dropping live F4 action outcomes"
        },
        "736": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_739-716.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-07-12T00:48:31Z",
          "report": "docs/project_review_739-716.md",
          "status": "legacy",
          "title": "Distinguish interrupted playtest steps from never-started ones"
        },
        "737": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_792-740.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-07-12T05:00:30Z",
          "report": "docs/project_review_792-740.md",
          "status": "legacy",
          "title": "[psychology] Mental states slice 2: catatonia and lash-out break behaviours"
        },
        "738": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_739-716.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-07-12T03:18:39Z",
          "report": "docs/project_review_739-716.md",
          "status": "legacy",
          "title": "Instrument F4 Layer A for keyboard, text, scroll, and drag routing"
        },
        "739": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_739-716.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-07-12T03:55:02Z",
          "report": "docs/project_review_739-716.md",
          "status": "legacy",
          "title": "Grow the semantic concept catalogue to 150 entries"
        },
        "740": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_792-740.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-07-12T04:28:16Z",
          "report": "docs/project_review_792-740.md",
          "status": "legacy",
          "title": "Split World.Geology.Timeline.Types into focused submodules"
        },
        "751": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_792-740.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-07-12T05:46:01Z",
          "report": "docs/project_review_792-740.md",
          "status": "legacy",
          "title": "Split World.Geology.Timeline.RiverTrace into focused submodules"
        },
        "752": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_792-740.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-07-12T14:53:21Z",
          "report": "docs/project_review_792-740.md",
          "status": "legacy",
          "title": "[content] Backfill the 8 missing textures for shipped power + cooking content"
        },
        "753": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_792-740.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-07-12T15:04:15Z",
          "report": "docs/project_review_792-740.md",
          "status": "legacy",
          "title": "Give the Create World preview pane real pre-generation art"
        },
        "754": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_792-740.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-07-12T15:16:00Z",
          "report": "docs/project_review_792-740.md",
          "status": "legacy",
          "title": "Review the 5 unclassified behavior probes: promote 3, classify 2"
        },
        "755": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_792-740.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-07-12T15:28:21Z",
          "report": "docs/project_review_792-740.md",
          "status": "legacy",
          "title": "Split World.Geology.Timeline.River into focused submodules"
        },
        "765": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_792-740.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-07-12T15:40:18Z",
          "report": "docs/project_review_792-740.md",
          "status": "legacy",
          "title": "Give LayerModal pages a real input-exclusive boundary (#742)"
        },
        "769": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_792-740.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-07-12T19:51:23Z",
          "report": "docs/project_review_792-740.md",
          "status": "legacy",
          "title": "[save-overhaul A1] Define the persistence contract and audited state inventory"
        },
        "77": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_167-80.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-06-26T17:59:44Z",
          "report": "docs/project_review_167-80.md",
          "status": "legacy",
          "title": "Clear stale build-tool hover tile on off-world cursor (#66)"
        },
        "770": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_792-740.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-07-12T19:05:31Z",
          "report": "docs/project_review_792-740.md",
          "status": "legacy",
          "title": "[UI hardening A2] Separate pointer blocking, click handling, and scroll capture"
        },
        "789": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_823-789.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-07-12T21:14:25Z",
          "report": "docs/project_review_823-789.md",
          "status": "legacy",
          "title": "Add generated-language profile generator and native proper-name renderer (#710)"
        },
        "791": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_792-740.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-07-12T20:50:00Z",
          "report": "docs/project_review_792-740.md",
          "status": "legacy",
          "title": "Rebuild final regional climate from the timeline's evolved forcing"
        },
        "792": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_792-740.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-07-12T21:02:26Z",
          "report": "docs/project_review_792-740.md",
          "status": "legacy",
          "title": "[tooling] Re-verify the seven base-failing probes and record accurate failure classifications"
        },
        "80": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_167-80.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-06-26T17:27:45Z",
          "report": "docs/project_review_167-80.md",
          "status": "legacy",
          "title": "Fail dump mode hard on init/chunk-load timeout (#45)"
        },
        "803": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_823-789.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-07-13T15:58:39Z",
          "report": "docs/project_review_823-789.md",
          "status": "legacy",
          "title": "Clarify starvation in player manual"
        },
        "804": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_823-789.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-07-13T16:14:14Z",
          "report": "docs/project_review_823-789.md",
          "status": "legacy",
          "title": "Migrate legacy runtime config to new local paths"
        },
        "808": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_823-789.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-07-13T16:29:41Z",
          "report": "docs/project_review_823-789.md",
          "status": "legacy",
          "title": "Isolate periodic thoughts from the state-of-mind probe"
        },
        "809": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_823-789.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-07-13T16:43:45Z",
          "report": "docs/project_review_823-789.md",
          "status": "legacy",
          "title": "Make editable Lua widgets Unicode-safe"
        },
        "810": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_823-789.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-07-13T17:22:25Z",
          "report": "docs/project_review_823-789.md",
          "status": "legacy",
          "title": "Dispose blood GPU textures when world pages are replaced or destroyed (#788)"
        },
        "817": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_823-789.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-07-13T16:56:40Z",
          "report": "docs/project_review_823-789.md",
          "status": "legacy",
          "title": "Bound CI build-cache churn to one snapshot per dependency plan (#790)"
        },
        "818": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_823-789.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-07-13T17:09:04Z",
          "report": "docs/project_review_823-789.md",
          "status": "legacy",
          "title": "Demote movement probe from CI_ELIGIBLE to manual-only targeted"
        },
        "819": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_823-789.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-07-13T17:35:31Z",
          "report": "docs/project_review_823-789.md",
          "status": "legacy",
          "title": "Define authoritative spatial bounds for placed locations"
        },
        "820": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_823-789.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-07-13T20:59:22Z",
          "report": "docs/project_review_823-789.md",
          "status": "legacy",
          "title": "Eliminate the mutable CI-image publication race"
        },
        "821": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_823-789.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-07-13T21:14:26Z",
          "report": "docs/project_review_823-789.md",
          "status": "legacy",
          "title": "Re-split Engine.Input.Thread and guard its reviewability boundary"
        },
        "822": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_835-822.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-07-13T21:45:13Z",
          "report": "docs/project_review_835-822.md",
          "status": "legacy",
          "title": "Keep pathing costs finite under extreme configuration and terrain"
        },
        "823": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_823-789.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-07-13T21:32:06Z",
          "report": "docs/project_review_823-789.md",
          "status": "legacy",
          "title": "Snapshot the clicked chunk for zoom-map selection"
        },
        "824": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_835-822.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-07-13T23:59:20Z",
          "report": "docs/project_review_835-822.md",
          "status": "legacy",
          "title": "Fix: input.* primary-timeout ack is indeterminate, not retry-safe"
        },
        "825": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_835-822.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-07-13T23:45:15Z",
          "report": "docs/project_review_835-822.md",
          "status": "legacy",
          "title": "Complete the remaining Lua decodeUtf8Lenient sweep"
        },
        "826": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_835-822.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-07-14T00:53:55Z",
          "report": "docs/project_review_835-822.md",
          "status": "legacy",
          "title": "Give Tiny/Small worlds inland-origin rivers without breaching calderas (#811)"
        },
        "827": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_835-822.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-07-14T21:08:50Z",
          "report": "docs/project_review_835-822.md",
          "status": "legacy",
          "title": "[save-overhaul A2] Add coordinated paused snapshot barrier"
        },
        "828": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_835-822.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-07-14T21:22:58Z",
          "report": "docs/project_review_835-822.md",
          "status": "legacy",
          "title": "Fix #816: relax freshwater renderer's exact-one-drop slope rule"
        },
        "829": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_835-822.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-07-14T23:54:21Z",
          "report": "docs/project_review_835-822.md",
          "status": "legacy",
          "title": "Reject construction jobs for already-occupied structure slots"
        },
        "83": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_167-80.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-06-26T17:41:23Z",
          "report": "docs/project_review_167-80.md",
          "status": "legacy",
          "title": "Freeze world timescale on save/load auto-pause (#42)"
        },
        "830": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_835-822.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-07-15T00:09:31Z",
          "report": "docs/project_review_835-822.md",
          "status": "legacy",
          "title": "Remove duplicate slider element lookup and preserve its full handle contract"
        },
        "831": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_835-822.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-07-15T00:34:45Z",
          "report": "docs/project_review_835-822.md",
          "status": "legacy",
          "title": "Tile-Z regression coverage bypasses the UI wiring that lost Z"
        },
        "832": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_835-822.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-07-15T01:20:35Z",
          "report": "docs/project_review_835-822.md",
          "status": "legacy",
          "title": "Render committed building blueprints across their full footprint"
        },
        "833": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_835-822.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-07-15T01:06:04Z",
          "report": "docs/project_review_835-822.md",
          "status": "legacy",
          "title": "Make the location-content probe validate the live loot registry"
        },
        "834": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_847-834.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-07-15T01:53:19Z",
          "report": "docs/project_review_847-834.md",
          "status": "legacy",
          "title": "Implement calorie-store hungry/starving threshold effects"
        },
        "835": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_835-822.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-07-15T01:36:37Z",
          "report": "docs/project_review_835-822.md",
          "status": "legacy",
          "title": "Redistribute shed mountain soil to lower terrain (#812)"
        },
        "836": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_847-834.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-07-15T02:08:54Z",
          "report": "docs/project_review_847-834.md",
          "status": "legacy",
          "title": "Make flora_growth_probe's year-round-harvest fixture probe-owned"
        },
        "837": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_847-834.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-07-15T02:24:19Z",
          "report": "docs/project_review_847-834.md",
          "status": "legacy",
          "title": "[locations] Block portal placement inside location bounds and verify ghost tint"
        },
        "838": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_847-834.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-07-15T03:00:53Z",
          "report": "docs/project_review_847-834.md",
          "status": "legacy",
          "title": "Resolve unit vision and combat awareness from the unit's own world page"
        },
        "839": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_847-834.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-07-15T02:44:55Z",
          "report": "docs/project_review_847-834.md",
          "status": "legacy",
          "title": "Reject unsupported location anchor tags instead of dropping their constraints"
        },
        "840": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_847-834.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-07-15T16:44:29Z",
          "report": "docs/project_review_847-834.md",
          "status": "legacy",
          "title": "Preserve structure material payment when a construction claimant dies (#799)"
        },
        "841": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_847-834.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-07-15T03:45:15Z",
          "report": "docs/project_review_847-834.md",
          "status": "legacy",
          "title": "Stop paused craft bills after their current cycle"
        },
        "842": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_847-834.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-07-15T13:37:37Z",
          "report": "docs/project_review_847-834.md",
          "status": "legacy",
          "title": "review-gate: don't strip reviewed:approve for no-op branch-update pushes"
        },
        "843": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_847-834.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-07-15T16:29:14Z",
          "report": "docs/project_review_847-834.md",
          "status": "legacy",
          "title": "[locations] Warn before establishing a portal remotely from all locations"
        },
        "845": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_847-834.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-07-15T17:48:51Z",
          "report": "docs/project_review_847-834.md",
          "status": "legacy",
          "title": "Route all wheel input through the active UI policy (#744)"
        },
        "846": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_847-834.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-07-15T18:47:10Z",
          "report": "docs/project_review_847-834.md",
          "status": "legacy",
          "title": "Fix false-accepted outcome for unbound gameplay keys"
        },
        "847": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_847-834.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-07-15T19:41:40Z",
          "report": "docs/project_review_847-834.md",
          "status": "legacy",
          "title": "Convert F4 Layer-A click/drag/scroll locations to framebuffer space"
        },
        "848": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_859-848.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-07-15T20:31:45Z",
          "report": "docs/project_review_859-848.md",
          "status": "legacy",
          "title": "Attribute playtest oracle evidence to the action that produced it (#775)"
        },
        "849": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_859-848.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-07-15T21:20:53Z",
          "report": "docs/project_review_859-848.md",
          "status": "legacy",
          "title": "Make F3 click correlation identify actual input controls"
        },
        "850": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_859-848.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-07-15T22:15:10Z",
          "report": "docs/project_review_859-848.md",
          "status": "legacy",
          "title": "Make solar generation follow longitude-local daylight (#794)"
        },
        "851": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_859-848.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-07-16T02:09:34Z",
          "report": "docs/project_review_859-848.md",
          "status": "legacy",
          "title": "Add persisted until-stock craft-bill mode (#795)"
        },
        "852": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_859-848.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-07-16T16:09:51Z",
          "report": "docs/project_review_859-848.md",
          "status": "legacy",
          "title": "Capture an immutable validated session snapshot (#758)"
        },
        "853": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_859-848.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-07-16T15:02:28Z",
          "report": "docs/project_review_859-848.md",
          "status": "legacy",
          "title": "Persist location discovery when player units approach (#780)"
        },
        "854": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_859-848.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-07-16T17:23:34Z",
          "report": "docs/project_review_859-848.md",
          "status": "legacy",
          "title": "[save-overhaul B1] Introduce the tagged checksummed v83 save envelope"
        },
        "855": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_859-848.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-07-16T17:36:50Z",
          "report": "docs/project_review_859-848.md",
          "status": "legacy",
          "title": "[locations] Render paired discovery-state icons on the zoom map"
        },
        "856": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_859-848.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-07-17T01:57:42Z",
          "report": "docs/project_review_859-848.md",
          "status": "legacy",
          "title": "Add discrete-control release-activation and keyboard control focus (#745)"
        },
        "857": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_859-848.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-07-16T22:19:50Z",
          "report": "docs/project_review_859-848.md",
          "status": "legacy",
          "title": "[UI hardening C1] Add opt-in clipping and viewport-aware popup placement"
        },
        "858": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_859-848.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-07-17T18:33:43Z",
          "report": "docs/project_review_859-848.md",
          "status": "legacy",
          "title": "Guard remote-warning establishHere() against an active-world switch (#844)"
        },
        "859": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_859-848.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-07-17T18:49:04Z",
          "report": "docs/project_review_859-848.md",
          "status": "legacy",
          "title": "Add embark-to-discovery integration probe (#782)"
        },
        "860": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_873-860.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-07-18T04:18:18Z",
          "report": "docs/project_review_873-860.md",
          "status": "legacy",
          "title": "[save-overhaul B2] Split Haskell persistence into independently versioned components (#760)"
        },
        "861": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_873-860.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-07-18T19:35:53Z",
          "report": "docs/project_review_873-860.md",
          "status": "legacy",
          "title": "Publish saves atomically with lossless previous-generation recovery (#762)"
        },
        "862": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_873-860.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-07-19T03:11:34Z",
          "report": "docs/project_review_873-860.md",
          "status": "legacy",
          "title": "[save-overhaul B3] Make Lua persistence versioned, scoped, and fail-fast"
        },
        "863": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_873-860.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-07-20T03:54:54Z",
          "report": "docs/project_review_873-860.md",
          "status": "legacy",
          "title": "[save-overhaul C2] Stage and atomically publish whole-session loads"
        },
        "866": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_873-860.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-07-20T04:11:04Z",
          "report": "docs/project_review_873-860.md",
          "status": "legacy",
          "title": "[UI hardening C2] Add responsive lifecycle contract for menu screens"
        },
        "867": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_873-860.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-07-20T20:12:23Z",
          "report": "docs/project_review_873-860.md",
          "status": "legacy",
          "title": "[UI hardening C4] Migrate gameplay HUD/overlays onto the responsive resize contract"
        },
        "868": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_873-860.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-07-20T14:32:17Z",
          "report": "docs/project_review_873-860.md",
          "status": "legacy",
          "title": "[save-overhaul C3] Add typed persistent references and a shared integrity graph"
        },
        "869": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_873-860.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-07-21T17:46:03Z",
          "report": "docs/project_review_873-860.md",
          "status": "legacy",
          "title": "[save-overhaul C4] Establish component migrations and tracked compatibility fixtures"
        },
        "870": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_873-860.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-07-21T18:55:08Z",
          "report": "docs/project_review_873-860.md",
          "status": "legacy",
          "title": "Fix save_modules.applyAll's no-prepared-load diagnostic and prove crash recovery (#864)"
        },
        "871": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_873-860.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-07-22T02:33:15Z",
          "report": "docs/project_review_873-860.md",
          "status": "legacy",
          "title": "[UI hardening C3] Align rendered box overflow with interactive bounds (#749)"
        },
        "872": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_873-860.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-07-22T12:43:37Z",
          "report": "docs/project_review_873-860.md",
          "status": "legacy",
          "title": "[save-overhaul D1] Add the end-to-end persistence contract suite"
        },
        "873": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_873-860.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-07-22T13:20:40Z",
          "report": "docs/project_review_873-860.md",
          "status": "legacy",
          "title": "Fix data_codec integer/float round-trip above 2^53 (#865)"
        },
        "874": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_909-874.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-07-22T16:39:04Z",
          "report": "docs/project_review_909-874.md",
          "status": "legacy",
          "title": "Add mental-effectiveness combat/craft tie-ins from state of mind (#353)"
        },
        "875": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_909-874.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-07-23T17:28:13Z",
          "report": "docs/project_review_909-874.md",
          "status": "legacy",
          "title": "Split World.Generate.Timeline: extract spike removal, drop stale comment"
        },
        "879": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_909-874.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-07-23T17:46:32Z",
          "report": "docs/project_review_909-874.md",
          "status": "legacy",
          "title": "Isolate craft_probe's base-quality checks from mental effectiveness"
        },
        "880": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_909-874.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-07-23T19:29:14Z",
          "report": "docs/project_review_909-874.md",
          "status": "legacy",
          "title": "Split Unit.Types into focused submodules"
        },
        "881": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_909-874.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-07-24T00:13:23Z",
          "report": "docs/project_review_909-874.md",
          "status": "legacy",
          "title": "Add EngineEnv capability inventory and its CI audit"
        },
        "902": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_909-874.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-07-24T14:12:02Z",
          "report": "docs/project_review_909-874.md",
          "status": "legacy",
          "title": "EngineEnv capability split E1: CoreCapability record + full-access ratchet"
        },
        "903": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_909-874.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-07-24T17:11:01Z",
          "report": "docs/project_review_909-874.md",
          "status": "legacy",
          "title": "Preview browser Phase 2: canonical categories + simple-category browsing (#886)"
        },
        "904": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_909-874.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-07-24T18:17:42Z",
          "report": "docs/project_review_909-874.md",
          "status": "legacy",
          "title": "Bleeding trails: bounded blood-mark emission from moving units (#882)"
        },
        "905": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_909-874.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-07-24T19:56:59Z",
          "report": "docs/project_review_909-874.md",
          "status": "legacy",
          "title": "EngineEnv capability split E2: ContentRegistries capability record (#890)"
        },
        "906": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_909-874.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-07-24T21:33:17Z",
          "report": "docs/project_review_909-874.md",
          "status": "legacy",
          "title": "EngineEnv capability split E3: migrate render/window/Vulkan/asset consumers to a RenderCapability record"
        },
        "908": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_909-874.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-07-24T22:57:32Z",
          "report": "docs/project_review_909-874.md",
          "status": "legacy",
          "title": "Cache windowed geometry on the transition, not on vcWindowMode (#907)"
        },
        "909": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_909-874.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-07-25T00:42:34Z",
          "report": "docs/project_review_909-874.md",
          "status": "legacy",
          "title": "EngineEnv capability split E5a: WorldSimCapability for the world/sim consumers"
        },
        "910": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_938-910.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-07-25T01:24:06Z",
          "report": "docs/project_review_938-910.md",
          "status": "legacy",
          "title": "Migrate input and Lua-transport consumers to an InputCapability record (#892)"
        },
        "924": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_938-910.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-07-25T14:44:21Z",
          "report": "docs/project_review_938-910.md",
          "status": "legacy",
          "title": "Declare the expedition-arc scope rule in CLAUDE.md (#914)"
        },
        "926": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_938-910.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-07-25T19:48:32Z",
          "report": "docs/project_review_938-910.md",
          "status": "legacy",
          "title": "Give placed locations a stable instance identity and gameplay lifecycle (#911)"
        },
        "927": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_938-910.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-07-25T19:27:06Z",
          "report": "docs/project_review_938-910.md",
          "status": "legacy",
          "title": "Add manual first-expedition gameplay scenarios (#925)"
        },
        "928": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_938-910.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-07-25T23:12:42Z",
          "report": "docs/project_review_938-910.md",
          "status": "legacy",
          "title": "Replace ad-hoc faction string comparison with a typed relation model"
        },
        "929": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_938-910.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-07-26T03:04:28Z",
          "report": "docs/project_review_938-910.md",
          "status": "legacy",
          "title": "Prove expedition retrieval and return end to end (#920)"
        },
        "930": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_938-910.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-07-26T01:45:35Z",
          "report": "docs/project_review_938-910.md",
          "status": "legacy",
          "title": "EngineEnv capability split E6a: UnitCombatCapability + 35 narrowed consumers"
        },
        "935": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_938-910.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-07-26T02:43:55Z",
          "report": "docs/project_review_938-910.md",
          "status": "legacy",
          "title": "Add UiCapability and narrow 15 UI/focus/HUD consumers (#897)"
        },
        "937": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_938-910.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-07-26T13:44:57Z",
          "report": "docs/project_review_938-910.md",
          "status": "legacy",
          "title": "Record first-expedition survival calibration observations (#919)"
        },
        "938": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_938-910.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-07-26T14:54:32Z",
          "report": "docs/project_review_938-910.md",
          "status": "legacy",
          "title": "--preview units/<name>: unit animation and direction viewer (#887)"
        },
        "939": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_989-939.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-07-26T16:50:54Z",
          "report": "docs/project_review_989-939.md",
          "status": "legacy",
          "title": "EngineEnv capability split E7b: EventsCapability for player-event, notification, and popup consumers"
        },
        "940": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_938-910.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-07-26T14:07:10Z",
          "report": "docs/project_review_938-910.md",
          "status": "legacy",
          "title": "Stationary and collapsed-unit blood pooling via layered bounded spawns (#883)"
        },
        "941": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_938-910.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-07-26T14:29:02Z",
          "report": "docs/project_review_938-910.md",
          "status": "legacy",
          "title": "Add BuildingCapability and narrow the last 14 units-buildings-combat consumers (#896)"
        },
        "953": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_989-939.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-07-26T22:06:47Z",
          "report": "docs/project_review_989-939.md",
          "status": "legacy",
          "title": "Make location loot-table rolls seed-stable per instance"
        },
        "954": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_989-939.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-07-27T00:51:04Z",
          "report": "docs/project_review_989-939.md",
          "status": "legacy",
          "title": "Give units their own knowledge of discovered locations, alongside global player discovery"
        },
        "955": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_989-939.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-07-27T03:45:10Z",
          "report": "docs/project_review_989-939.md",
          "status": "legacy",
          "title": "Remove fixed spawn-only items from ruin loot (#921)"
        },
        "961": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_989-939.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-07-27T14:46:45Z",
          "report": "docs/project_review_989-939.md",
          "status": "legacy",
          "title": "Define and load the first-session tutorial tree (#957)"
        },
        "962": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_989-939.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-07-27T16:11:28Z",
          "report": "docs/project_review_989-939.md",
          "status": "legacy",
          "title": "Persist tutorial objective progress (#958)"
        },
        "963": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_989-939.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-07-27T17:21:22Z",
          "report": "docs/project_review_989-939.md",
          "status": "legacy",
          "title": "Evaluate the first-session tutorial objectives (#959)"
        },
        "966": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_989-939.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-07-30T13:17:02Z",
          "report": "docs/project_review_989-939.md",
          "status": "legacy",
          "title": "Close both cleanup branches in Font's Show instance"
        },
        "986": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_989-939.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-07-30T14:32:15Z",
          "report": "docs/project_review_989-939.md",
          "status": "legacy",
          "title": "Correct the bindless texture module header's slot-capacity claim"
        },
        "987": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_989-939.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-07-30T15:16:31Z",
          "report": "docs/project_review_989-939.md",
          "status": "legacy",
          "title": "Remove the dead bindless texture-system teardown"
        },
        "988": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_989-939.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-07-30T14:54:46Z",
          "report": "docs/project_review_989-939.md",
          "status": "legacy",
          "title": "Render the tutorial HUD checklist (#960)"
        },
        "989": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_989-939.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-07-30T16:33:26Z",
          "report": "docs/project_review_989-939.md",
          "status": "legacy",
          "title": "Remove the dead legacy font fragment shader"
        },
        "990": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_1018-991.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-07-30T17:24:39Z",
          "report": "docs/project_review_1018-991.md",
          "status": "legacy",
          "title": "Remove the never-drawn demo quad vertex buffer"
        },
        "991": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_1018-991.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-07-30T16:56:00Z",
          "report": "docs/project_review_1018-991.md",
          "status": "legacy",
          "title": "Gate the first-session tutorial foundation (#922)"
        },
        "993": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_1018-991.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-07-31T02:26:58Z",
          "report": "docs/project_review_1018-991.md",
          "status": "legacy",
          "title": "Remove unused legacy fields from EngineConfig"
        },
        "994": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_1018-991.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-07-31T03:36:09Z",
          "report": "docs/project_review_1018-991.md",
          "status": "legacy",
          "title": "Archive the currentSaveVersion changelog out of World.Save.Types"
        },
        "995": {
          "claim": null,
          "commit": null,
          "completed_at": null,
          "evidence": [
            "report:docs/project_review_1018-991.md (operator-confirmed)"
          ],
          "history": [],
          "merged_at": "2026-07-31T03:09:46Z",
          "report": "docs/project_review_1018-991.md",
          "status": "legacy",
          "title": "Gate the first expedition end to end (#923)"
        }
      }
    }
  },
  "version": 4
}
```
