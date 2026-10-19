# persistence_contract_probe blood-leg flake: diagnosis

Diagnosed 2026-10-18 against master fa7b5c20f, with an instrumented reproduction of the probe's
engine N (harness_bl1.py). The raw traces (trace.json per run), the engine logs and the harness are
kept locally under ~/.local/state/synarchy-evidence/flake-persistence-blood/ on the owner's machine.
The run names below (run1 to run9) refer to those directories.

## Verdict

This is a **probe-fixture bug**, not a regression in the save, load or blood code.

Engine N of the blood leg runs with the full autonomous AI stack live. It puts its
acolytes on a mountain summit, then leaves the session unpaused for a wall-clock-dependent
time. The acolyte AI's fall-permitted actions can walk the bled acolyte off the summit:
the find_water water search (survival) and treat_ally (the medic response to another
fixture unit's fall). The fall stamps dozens of fracture, blunt and internal wounds. Those
drain the unit before the probe pauses, so it is saved either dead with cur 0.0, or
crawling and partly drained (about 7% left). After the reload it cannot drain 0.02 more,
so "the Simulated unit drains after the reload and a fresh cut" fails.

## Mechanism, with evidence

1. **The AI is live, although the probe assumes it is not.**
   - scripts/init_loader.lua:112 loads scripts/unit_ai.lua, and :107 loads
     unit_resources.lua, in every headless session.
   - persistence_contract_probe.py's comment says "headless boots skip the loading screen
     entirely", so its engagement_boot loads only unit_stats and unit_resources.
   - Evidence: in run3_modules/trace.json, package.loaded includes scripts.unit_ai,
     bear_ai and red_squirrel_ai, and the bled acolyte's AI action is search_for_water.

2. **The fixture site is a summit.**
   - Seed 424242 and world size 8 give terrain z at the spawn tiles (gx, gy) that falls
     about 28 levels per diagonal step: (4,4)=172, (5,5)=144, (6,6)=116, (7,7)=89,
     (8,8)=61, (9,9)=34.
   - The bled acolyte at (4,4) stands on a small summit: (4,3)=174, (4,4)=172,
     (4,5)=172, (5,3)=170, (5,2)=171. Every way off it drops 14 to 28 levels.
   - Evidence: the full map is in run5_load16/trace.json under "terrain-z".
   - ensure_chunks is a no-op here: loadChunksInRegion returns 0 and waitForChunks returns
     0 because world init already loaded the chunks. So it is not a z=0 spawn problem.

3. **The AI's fall-permitted moves cause the fall.**
   - scripts/ambient_movement.lua is explicit: survival actions (water and food seeking)
     and medic, work and fetch pathing "keep the fall-permitted default".
   - unit_ai_water.lua:436 calls unit.moveTo with no avoid_falls.
   - Reproduced in run6_dwell300. The bled acolyte (unit 1) was on search_for_water. Its
     first fall came about 47 s after spawn, then 172 to 170 to 144. At 99 s it was dead
     with blood 0.0 and 44 wounds (lungs and intestines internal, skull, spine, leg
     fractures).
   - Reproduced again in run7_dwell120_3: it fell about 97 s after spawn and died with
     blood 0.0 and 43 wounds.
   - With find_water cleared through probelib.clear_find_water (run8_fix_dwell300 and
     run9_fix_actions), the acolyte wanders fall-safe on its summit. Then the other acolyte
     (unit 5) falls. Unit 1 switches to treat_ally, walks to it off the summit
     (174, 172, 170, 144) and dies at about 94 s with blood 0.0. So **clearing find_water
     alone is NOT a fix**.

4. **Why it is a flake, and worse on CI.**
   - The hazard is per second of unpaused time, and its timing is random. The AI uses
     math.random (search angle offset, decisions). Climb slips use the stat RNG.
   - Locally the probe's engine N is unpaused for about 9 to 13 s before "settled" and
     about 6 s in the bleed window (run1, run2, run5).
   - The first fatal fall arrived 47 to 97 s into the exposure in 2 of 5 long dwells. The
     other 3 survived 60 s, 120 s and 120 s.
   - On CI every console round trip is slower. The settle loop also stays unpaused while
     any fixture animal is knocked down; the squirrel and bear fall and slip on this slope
     in every run. Its cap is 40 s, and the bleed window adds up to 30 s more. Exposure
     therefore runs several times longer, and the failure becomes likely.
   - Local CPU load alone did not lengthen it: run5 used 16 burners on 16 cores, with
     11 s unpaused.

5. **The CI signatures match.**
   - #2861 run 39170659952 and #2853 run 39059424742 attempt 1 saved the bled unit with
     cur 0.0 and pose dead.
   - #2859 run 39125782913 saved it with cur 0.359 of 5.346 and pose crawling.
     injuries.cannotWalk needs a leg fracture of severity at least 0.85, or two of at least
     0.5. That can only come from a fall: the probe stamps one 0.1 torso slash, which
     clots and bleeds at about 0.02 L/s.
   - The "2 checks failed" in #2861 is consistent with the pre-save "holds a partial,
     non-zero volume" check also failing on cur 0. The CI tail is truncated, so this is
     inferred.

## Proposed minimal fix (probe fixture)

**Make the AI unable to move the blood engines' fixture units.** The blood leg tests save
and load of blood and mind state; it needs no autonomous behaviour. Two deterministic
ways:

- **(A) Preferred: give the probe a handle on the init_loader-owned AI script, and pause
  it.**
  - Files: scripts/init_loader.lua (export its loaded script ids, for example
    M.scriptId(name)) and a probelib helper pause_ai(port), which calls
    engine.pauseScript(id) on unit_ai.
  - Call the helper from persistence_contract_probe.engagement_boot for the blood engines
    N, O and P.
  - Deterministic because unit_ai.update never runs. No moveTo is ever issued, so no walk,
    climb or fall can occur. The units then move only by gravity at spawn, which already
    settles in the first second.
  - Caveat: init_loader.lua is code, so this goes through a PR.
- **(B) Probe-only: empty the fixture species' action lists.**
  - For acolyte, nomad_primitive, red_squirrel, bear_brown and white_tailed_deer, call
    require('scripts.unit_ai').registerActions(def, {}, {excludeUniversal = every
    universal action}).
  - tickOne then has no candidate and issues no movement.
  - Only tools/persistence_contract_probe.py changes.
  - Before claiming determinism for B, check that an empty list idles cleanly.

**Not sufficient:**
- clear_find_water, demonstrated by run8 and run9.
- Re-siting to flat ground: the 8-wide world has no flat area in 0..10 by 0..9.
- Shorter waits: they lower the probability without bounding it.

Either fix needs no threshold change, and the probe's checks stay as they are. The other
engines of this probe that spawn acolytes unpaused on the same seed (the
exsanguination/sleep/cardio engines at :750 and :909) are exposed to the same hazard and
should take the same fix.

## Secondary observations (not the flake's cause; for the owner)

- **Ambient wander still allows a risky climb.** In run9, unit 5's action was wander,
  which asks for avoid_falls. It started climbing a 27-level cliff (realZ 62 to 68.7),
  slipped (Unit/Thread/Movement/Climb.hs, a stat-RNG slip roll) and fell to its death.
  avoid_falls blocks drops, not climbs with slip risk. That may fall short of #1217's
  stated aim that "aimless, zero-stakes movement should never choose preventable injury".
  This is a product question.
- **unit_resources may be loaded twice.** engagement_boot loads unit_resources.lua again
  on top of init_loader's copy, so it may tick twice per pass. Not verified; worth
  checking alongside the fix.
