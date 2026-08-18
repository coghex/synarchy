# Suggested issue-hunt priorities

This is a working priority list following the CI and non-CI test audits.  It
is intended to guide focused issue hunts, not to replace the individual
evidence-backed findings reports.

## Recommended order

1. **Persistence transactions and probe integrity**
   - Hunt save acceptance/completion, stale-slot loads, restart boundaries,
     artifact cleanup/isolation, and multiworld save identity.
   - Repeated test-oracle flaws across independent probes make this the most
     promising cross-cutting area.

2. **Multiworld ownership**
   - Look for code that ambiguously uses an active world instead of requiring
     an explicit page/world: Lua dispatch, worker queues, save/load,
     UI-backed world lookups, and resource lifecycle.
   - One-world gameplay does not remove the need for explicit ownership at
     boundaries; this is a known recurring source of defects.

3. **Real UI input paths**
   - Use GPU/offscreen verification for modal boundaries, scroll capture,
     control activation, keyboard focus restoration, resize/rebuild state, and
     world-backed panel retargeting.
   - Keep issue hunts split by behavior region rather than treating UI as one
     undifferentiated surface.

4. **AI scenario contracts**
   - Hunt claim/release, interruption, save/load during work, stale targets,
     priority arbitration, and setup paths that silently select a different
     behavior than the test intends to observe.
   - Prefer deliberately controlled fixtures so a failure names the behavior,
     not worldgen or scheduler accident.

5. **Worldgen-to-gameplay boundaries**
   - Probe generated locations, rivers, fluid, and climate as they reach
     gameplay APIs and UI, not only as worldgen statistics.
   - Look especially for absent/invalid fixture handling and identity drift
     across generation, save/load, and regeneration.

6. **Graphics lifecycle**
   - Hunt Vulkan resource ownership, optional-capability fallbacks, live GPU
     selection, offscreen teardown, and project-owned window behavior.

7. **Test harness integrity**
   - Hunt worker crashes that are not surfaced, needless engine/bootstrap
     startup, fixture leakage, and successful skips of required phases.
   - These defects amplify false confidence in every subsystem.

## First focused hunt

Start with **persistence transactions plus multiworld ownership**.  They have
the broadest blast radius, already show repeated evidence, and can invalidate
otherwise convincing behavior tests.
