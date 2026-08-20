#!/usr/bin/env python3
"""Content-registry probe (#890, EngineEnv capability split E2).

The focused functional gate for the `content-registries` capability
(`docs/engineenv_capability_inventory.md` SS5/SS7.6): every one of the
seven registries is exercised through BOTH its public Lua writer and a
public Lua reader, in one headless boot, so the #890 narrowing
(`Engine.Core.Capability.ContentRegistries`) can't silently break a
registry that no other CI-eligible probe touches.

`tools/craft_probe.py` only loads substance / item / equipment / recipe
data, and the infection + location probes are manual-only — so before
this probe, three of the seven writer/reader pairs (infection, location
defs, loot tables) had no automated gate at all.

Phases:

  1. Writers — all seven `load*Yaml` verbs over the shipped data files:
     substance, item, equipment, infection, recipe, location, loot
     table. Each file must report a positive count of its OWN (a
     rejected file is invisible in a summed total, #1233), and the item
     registry additionally has to load exactly as many definitions as
     the `data/items/` TREE authors — recursively, at any depth, since
     item definitions may live in logical subdirectories (#1232) — the
     completeness check that makes this the gate proving the shipped
     item corpus is fully valid.
  2. Readers — one public query per registry: substance.get/getNames,
     item.listDefs, equipment.getClass/getClassNames,
     infection.get/getNames, craft.get/getNames (+ repair.get/getNames,
     the repair-axis-filtered view of the same registry),
     engine.listLocationDefs, and both loot draws — loot.roll (the
     shared-RNG compatibility surface, including the unknown-table nil
     path) and loot.rollFor (#948: the seed-stable draw the placed-
     location content-spawn path uses — repeatable for one context,
     independent across contexts, nil for an unknown table or an
     incomplete context).
  3. Reload — the loaders are PUBLIC verbs callable at any time, not a
     one-shot boot step (`Engine.Scripting.Lua.API.Register.Engine`), so
     a probe-authored substance file is loaded twice with a changed
     property: the second load must REPLACE by name (registry stays
     queryable, name count unchanged, new value visible), and a
     re-load of a shipped file must leave its defs queryable too.
  4. World join — with a real generated world, world.listPlacedLocations
     joins each placement against the location-def registry. The phase
     POLLS for at least one placement whose id resolves against a
     registered def (an empty list would satisfy a "all known entries
     are well-formed" check vacuously, and `world.show` is fire-and-
     forget onto the world thread), then requires every such placement
     to carry the def's `bounds` and to carry NO `discovery_margin`
     (#1230 removed the field from both tables) — the read that goes
     through the capability record in `API.WorldQuery.Location` — and
     requires the argument-less active-world form to agree with the
     page-targeted one.

Usage: python3 tools/content_registry_probe.py [--port 9341]
"""
import argparse
import glob
import re
import sys

from probelib import boot, init_world, poll_until, quit_engine, send, send_json

# This probe's console default. Its registry queries decode whole
# catalogues, so they get longer than probelib.send_json's 10 s --
# the same 15 s the local jget this file used to define had, and the
# same one `num` below still uses.
QUERY_TIMEOUT = 15.0

PROBE_SUBSTANCE_YAML = "/tmp/content_registry_probe_substances.yaml"

# One probe-authored substance, written twice with a different density,
# to witness the loaders' insert/replace-by-id behaviour (phase 3).
PROBE_SUBSTANCE_TMPL = """\
substances:
  - name: probe_alloy
    density: {density}
    tensile_strength: 400.0
    yield_strength: 250.0
    shear_strength: 230.0
    fracture_toughness: 50.0
    hardness: 150.0
    stab_resistance: 1.0
    slash_resistance: 1.0
    blunt_resistance: 1.0
"""


def num(port, lua, timeout=15.0):
    raw = send(port, lua, timeout).strip().strip('"')
    try:
        return float(raw)
    except (TypeError, ValueError):
        return None


# The one recursive family: item definitions may sit in logical
# subdirectories (#1232). `**` with recursive=True also matches zero
# directories, so this covers the top level too.
ITEM_YAML_GLOB = "data/items/**/*.yaml"


def check(passed, ok, label, detail=""):
    print(f"  {'PASS' if ok else 'FAIL'}  {label}"
          + (f"   [{detail}]" if detail and not ok else ""))
    return passed and ok


def load_all(port, fn, pattern, recursive=False):
    """Run `fn` over every file matching `pattern`.

    `recursive` turns on `**` expansion, which only the item tree needs:
    item definitions may be organized into subdirectories (#1232), and
    every other family here is flat by design.

    Returns `(total, rejected)`: the summed count the loader reported, and
    the list of files that reported no answer at all or a count of zero.

    Per-file accounting is the point. Summing alone made this probe blind
    to exactly the failure it looks like it covers (#1233 review): one
    invalid file returns 0 while its siblings still push the total above
    zero, so a `total > 0` check passes on a partially-broken corpus. A
    file in any of these seven directories that contributes no
    definitions is a broken file, never a legitimate empty one.
    """
    total, rejected = 0, []
    for path in sorted(glob.glob(pattern, recursive=recursive)):
        n = num(port, f"return {fn}('{path}')")
        if n is None or int(n) <= 0:
            rejected.append(path)
        else:
            total += int(n)
    return total, rejected


def authored_item_defs():
    """How many item definitions the shipped YAML actually declares.

    Derived from the files on disk rather than pinned, so this probe
    proves the ENGINE loaded everything that is authored without also
    becoming a second place to update when an item is added. Walks the
    whole `data/items/` tree, at any depth, for the same reason startup
    discovery does (#1232). The exact inventory (35 files / 61
    definitions) is pinned separately, in the `Item.BulkStorage` hspec
    group.
    """
    total = 0
    for path in sorted(glob.glob(ITEM_YAML_GLOB, recursive=True)):
        with open(path, encoding="utf-8") as fh:
            for line in fh:
                if re.match(r"^\s*-\s+name:", line):
                    total += 1
    return total


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--port", type=int, default=9341)
    args = ap.parse_args()
    port = args.port
    passed = True

    proc = boot(port)
    try:
        # --- Phase 1: every writer path -------------------------------
        print("\n-- phase 1: registry writers (all seven load*Yaml verbs) --")
        writers = [
            ("substance", "engine.loadSubstanceYaml", "data/substances/*.yaml"),
            ("item", "engine.loadItemYaml", ITEM_YAML_GLOB),
            ("equipment", "engine.loadEquipmentYaml", "data/equipment/*.yaml"),
            ("infection", "engine.loadInfectionYaml", "data/infections/*.yaml"),
            ("recipe", "engine.loadRecipeYaml", "data/recipes/*.yaml"),
            ("location", "engine.loadLocationYaml", "data/locations/*.yaml"),
            ("loot table", "engine.loadLootTableYaml", "data/loot_tables/*.yaml"),
        ]
        item_total = None
        for label, fn, pattern in writers:
            n, rejected = load_all(port, fn, pattern,
                                   recursive=pattern == ITEM_YAML_GLOB)
            passed = check(passed, n > 0 and not rejected,
                           f"{label} writer loaded defs",
                           f"{fn} over {pattern} reported {n}; "
                           f"files contributing nothing: {rejected}")
            if label == "item":
                item_total = n

        # #1233: every authored item definition must have loaded, not
        # merely "some". A definition the loader REJECTS (a missing or
        # non-finite `bulk`, say) is silently absent from the registry
        # otherwise, since its file still returns the count of whatever
        # else parsed.
        expected_items = authored_item_defs()
        passed = check(passed,
                       item_total is not None and item_total == expected_items,
                       "every authored item definition loaded",
                       f"engine loaded {item_total}, the data/items/ tree "
                       f"declares {expected_items}")

        # --- Phase 2: one public reader per registry -------------------
        print("\n-- phase 2: registry readers --")

        names = send_json(port, "return substance.getNames()",
                          timeout=QUERY_TIMEOUT)
        steel = send_json(port, "return substance.get('steel')",
                          timeout=QUERY_TIMEOUT)
        ok = (isinstance(names, list) and "steel" in names
              and isinstance(steel, dict) and steel.get("density", 0) > 0)
        passed = check(passed, ok, "substance.getNames/get",
                       f"names={names} steel={steel}")

        item_defs = send_json(port, "return item.listDefs()",
                              timeout=QUERY_TIMEOUT)
        ok = (isinstance(item_defs, list) and len(item_defs) > 0
              and any(d.get("name") == "steel_bar" for d in item_defs))
        passed = check(passed, ok, "item.listDefs",
                       f"count={len(item_defs) if isinstance(item_defs, list) else item_defs}")

        cls_names = send_json(port, "return equipment.getClassNames()",
                              timeout=QUERY_TIMEOUT)
        humanoid = send_json(port, "return equipment.getClass('humanoid')",
                             timeout=QUERY_TIMEOUT)
        ok = (isinstance(cls_names, list) and "humanoid" in cls_names
              and isinstance(humanoid, dict)
              and len(humanoid.get("slots", [])) > 0)
        passed = check(passed, ok, "equipment.getClassNames/getClass",
                       f"names={cls_names} slots={humanoid}")

        inf_names = send_json(port, "return infection.getNames()",
                              timeout=QUERY_TIMEOUT)
        staph = send_json(port, "return infection.get('staph')",
                          timeout=QUERY_TIMEOUT)
        ok = (isinstance(inf_names, list) and "staph" in inf_names
              and isinstance(staph, dict) and staph.get("id") == "staph")
        passed = check(passed, ok, "infection.getNames/get",
                       f"count={len(inf_names) if isinstance(inf_names, list) else inf_names} staph={staph}")

        recipe_names = send_json(port, "return craft.getNames()",
                                 timeout=QUERY_TIMEOUT)
        ok = isinstance(recipe_names, list) and len(recipe_names) > 0
        passed = check(passed, ok, "craft.getNames", f"names={recipe_names}")
        if ok:
            one = recipe_names[0]
            rec = send_json(port, f"return craft.get('{one}')",
                            timeout=QUERY_TIMEOUT)
            ok2 = (isinstance(rec, dict) and rec.get("id") == one
                   and isinstance(rec.get("station"), str))
            passed = check(passed, ok2, "craft.get returns the full shape",
                           f"{one} -> {rec}")

        # repair.* is the repair-axis-filtered view of the SAME recipe
        # registry, and the one reader that also reaches the item
        # registry (repair.repairAt) — so it gates a second field of the
        # capability record from a second module.
        repair_names = send_json(port, "return repair.getNames()",
                                 timeout=QUERY_TIMEOUT)
        ok = isinstance(repair_names, list) and len(repair_names) > 0
        passed = check(passed, ok, "repair.getNames (repair-tagged recipes)",
                       f"names={repair_names}")
        if ok:
            rp = send_json(port, f"return repair.get('{repair_names[0]}')",
                           timeout=QUERY_TIMEOUT)
            ok2 = isinstance(rp, dict) and rp.get("repairAxis") in (
                "condition", "sharpness")
            passed = check(passed, ok2, "repair.get carries a repair axis",
                           f"{repair_names[0]} -> {rp}")

        loc_defs = send_json(port, "return engine.listLocationDefs()",
                             timeout=QUERY_TIMEOUT)
        ok = (isinstance(loc_defs, list) and len(loc_defs) > 0
              and all(isinstance(d.get("bounds"), dict) for d in loc_defs))
        passed = check(passed, ok, "engine.listLocationDefs",
                       f"defs={loc_defs}")
        # #1230: the field is GONE from the public def table. Asserting
        # its absence is the inversion of the check this line replaced,
        # which required it — bounds stays the authoritative footprint.
        stale = ([d for d in loc_defs if "discovery_margin" in d]
                 if isinstance(loc_defs, list) else [])
        passed = check(passed, not stale,
                       "engine.listLocationDefs reports no discovery_margin "
                       "(#1230)", f"stale={stale}")
        def_ids = ({d["id"] for d in loc_defs}
                   if isinstance(loc_defs, list) else set())

        # loot.roll draws from the loot-table registry (and, unlike every
        # other reader here, also needs the shared stat RNG — the one
        # field #890 had to pass alongside the capability record).
        entry_ids = set()
        for _ in range(12):
            pick = send_json(port, "return loot.roll('ruin_common')",
                             timeout=QUERY_TIMEOUT)
            if isinstance(pick, str) and pick not in ("null", "nil"):
                entry_ids.add(pick)
        ok = len(entry_ids) > 0
        passed = check(passed, ok, "loot.roll draws from the loaded table",
                       f"picks={sorted(entry_ids)}")
        unknown = send(port, "return loot.roll('no_such_table')").strip().strip('"')
        ok = unknown in ("null", "nil", "")
        passed = check(passed, ok, "loot.roll on an unknown table is nil",
                       f"got={unknown!r}")

        # loot.rollFor (#948) is the seed-stable draw the placed-location
        # content-spawn path uses: pure in (tableId, worldSeed,
        # instanceId, entryIndex, rollIndex), reading no generator. Same
        # process, repeated calls, identical context -> identical pick.
        repeats = {send(port, "return loot.rollFor('ruin_common',42,1,3,1)")
                   .strip().strip('"') for _ in range(8)}
        ok = len(repeats) == 1 and repeats != {"null"} and repeats != {"nil"}
        passed = check(passed, ok,
                       "loot.rollFor repeats one pick for one context",
                       f"picks={sorted(repeats)}")

        # Distinct contexts are independent streams — vary each
        # discriminator in turn and require the set of picks to move at
        # all. (Not an all-distinct claim: chance may legitimately
        # reselect the same entry.)
        varied = {send(port, f"return loot.rollFor('ruin_common',{s},{i},{e},{r})")
                  .strip().strip('"')
                  for s, i, e, r in ((42, 1, 3, 1), (42, 2, 3, 1),
                                     (42, 1, 1, 1), (42, 1, 3, 2),
                                     (43, 1, 3, 1))}
        ok = len(varied) > 1
        passed = check(passed, ok,
                       "loot.rollFor separates seed/instance/entry/roll",
                       f"picks={sorted(varied)}")

        # Unknown table -> nil, same as loot.roll. Incomplete context ->
        # nil too, rather than silently falling back to the entropy path.
        for lua, label in (
                ("return loot.rollFor('no_such_table',42,1,3,1)",
                 "loot.rollFor on an unknown table is nil"),
                ("return loot.rollFor('ruin_common')",
                 "loot.rollFor without a context is nil"),
                ("return loot.rollFor('ruin_common',42,1)",
                 "loot.rollFor with a partial context is nil")):
            got = send(port, lua).strip().strip('"')
            ok = got in ("null", "nil", "")
            passed = check(passed, ok, label, f"got={got!r}")

        # --- Phase 3: loaders stay callable (insert/replace, not frozen)
        print("\n-- phase 3: reload semantics --")
        with open(PROBE_SUBSTANCE_YAML, "w") as f:
            f.write(PROBE_SUBSTANCE_TMPL.format(density=1111.0))
        n1 = num(port, f"return engine.loadSubstanceYaml('{PROBE_SUBSTANCE_YAML}')")
        after1 = send_json(port, "return substance.getNames()",
                           timeout=QUERY_TIMEOUT)
        d1 = send_json(port, "return substance.get('probe_alloy')",
                       timeout=QUERY_TIMEOUT)
        ok = (n1 == 1 and isinstance(after1, list) and "probe_alloy" in after1
              and isinstance(d1, dict) and abs(d1.get("density", 0) - 1111.0) < 0.5)
        passed = check(passed, ok, "post-boot load registers a new substance",
                       f"n={n1} def={d1}")

        with open(PROBE_SUBSTANCE_YAML, "w") as f:
            f.write(PROBE_SUBSTANCE_TMPL.format(density=2222.0))
        n2 = num(port, f"return engine.loadSubstanceYaml('{PROBE_SUBSTANCE_YAML}')")
        after2 = send_json(port, "return substance.getNames()",
                           timeout=QUERY_TIMEOUT)
        d2 = send_json(port, "return substance.get('probe_alloy')",
                       timeout=QUERY_TIMEOUT)
        ok = (n2 == 1
              and isinstance(after1, list) and isinstance(after2, list)
              and len(after2) == len(after1)          # replaced, not duplicated
              and isinstance(d2, dict)
              and abs(d2.get("density", 0) - 2222.0) < 0.5)
        passed = check(passed, ok,
                       "re-loading replaces by name and stays queryable",
                       f"n={n2} before={len(after1) if isinstance(after1, list) else after1} "
                       f"after={len(after2) if isinstance(after2, list) else after2} def={d2}")

        # A shipped file re-loaded on top of itself must leave its defs
        # intact too (the registries are not one-shot / frozen).
        num(port, "return engine.loadSubstanceYaml('data/substances/metals.yaml')")
        steel2 = send_json(port, "return substance.get('steel')",
                           timeout=QUERY_TIMEOUT)
        after3 = send_json(port, "return substance.getNames()",
                           timeout=QUERY_TIMEOUT)
        ok = (isinstance(steel2, dict) and steel2.get("density", 0) > 0
              and isinstance(after3, list) and len(after3) == len(after2))
        passed = check(passed, ok, "re-loading a shipped file keeps it queryable",
                       f"steel={steel2} count={len(after3) if isinstance(after3, list) else after3}")

        n_recipes_before = send_json(port, "return craft.getNames()",
                                     timeout=QUERY_TIMEOUT)
        load_all(port, "engine.loadRecipeYaml", "data/recipes/*.yaml")  # noqa: F841
        n_recipes_after = send_json(port, "return craft.getNames()",
                                    timeout=QUERY_TIMEOUT)
        ok = (isinstance(n_recipes_before, list) and isinstance(n_recipes_after, list)
              and len(n_recipes_after) == len(n_recipes_before))
        passed = check(passed, ok, "re-loading the recipe set does not duplicate ids",
                       f"before={len(n_recipes_before) if isinstance(n_recipes_before, list) else n_recipes_before} "
                       f"after={len(n_recipes_after) if isinstance(n_recipes_after, list) else n_recipes_after}")

        # --- Phase 4: placed-location join against the def registry ----
        print("\n-- phase 4: world.listPlacedLocations def join --")
        page = "content_registry_probe"
        init_world(port, name=page, seed=42, size=64, plates=3)
        # `init_world`'s world.show is fire-and-forget onto the world
        # thread, so the page is NOT necessarily active the instant it
        # returns — an argument-less world.listPlacedLocations() issued
        # right here can still read a nonexistent active world and come
        # back empty. An empty list would satisfy a naive
        # "is it a list?" + "are all known entries well-formed?" pair
        # VACUOUSLY, so poll for the real thing instead: at least one
        # placement whose id resolves against the def registry. That is
        # the join this phase exists to gate, and a w64/seed-42 world
        # placing >= 1 ruin_small is the same expectation
        # tools/location_overlay_probe.py's check 1 already relies on.
        def placements():
            entries = send_json(port, f"return world.listPlacedLocations('{page}')",
                                timeout=30.0)
            if not isinstance(entries, list):
                return None
            known = [p for p in entries if p.get("id") in def_ids]
            return (entries, known) if known else None

        found = poll_until(90, placements, interval=0.5)
        detail = ""
        if found is None:
            last = send_json(port, f"return world.listPlacedLocations('{page}')",
                             timeout=30.0)
            detail = f"def_ids={sorted(def_ids)} last={last!r}"
        passed = check(passed, found is not None,
                       "world.listPlacedLocations reports >= 1 placement "
                       "resolving against a registered def", detail)
        if found is not None:
            placed, known = found
            bad = [p for p in known if not isinstance(p.get("bounds"), dict)]
            passed = check(passed, not bad,
                           "every placement with a registered def carries "
                           "its bounds",
                           f"placed={len(placed)} known={len(known)} bad={bad}")
            # #1230: and none of them still reports the removed margin.
            stale_p = [p for p in known if "discovery_margin" in p]
            passed = check(passed, not stale_p,
                           "no placement reports discovery_margin (#1230)",
                           f"stale={stale_p}")
            print(f"        (placements: {len(placed)}, "
                  f"with a registered def: {len(known)})")
            # The argument-less form (active world) must agree with the
            # page-targeted one — same registry read, same join, and the
            # form the Lua `locations` module actually calls.
            active = poll_until(
                30,
                lambda: (lambda e: e if isinstance(e, list) and e else None)(
                    send_json(port, "return world.listPlacedLocations()",
                              timeout=30.0)),
                interval=0.5)
            passed = check(passed, active is not None
                           and len(active) == len(placed),
                           "the active-world form agrees with the "
                           "page-targeted form",
                           f"active={len(active) if isinstance(active, list) else active} "
                           f"page={len(placed)}")

        print("\n" + ("ALL CONTENT-REGISTRY CHECKS PASSED"
                      if passed else "SOME FAILED"))
        return 0 if passed else 1
    finally:
        quit_engine(port, proc)


if __name__ == "__main__":
    sys.exit(main())
