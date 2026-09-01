#!/usr/bin/env python3
"""Location naming: a placed location's name in its world's own
generated language (#2095).

One owner for #1101's four contracts -- a generated native name plus a
non-empty English gloss on a provenance-bearing world, the `ldLabel`
fallback with the `gloss` key ABSENT on the same terrain seed without
one, both surviving save -> fresh process -> load byte-exact, and a
fresh-process regeneration of the same seed + language reproducing them
exactly.

The assertions are `tools/location_content_probe.py`'s own, moved rather
than rewritten. Nothing here boots an engine.
"""
from __future__ import annotations

import time

from probelib import send

from .engine_queries import gen_world, placed
from .invocation import ScenarioState

#: The language seed the #1101 phase names its world from. An arbitrary
#: but fixed unsigned 64-bit value, deliberately above 2^63-1 so the
#: decimal-string carrier world.init/world.getLanguageProvenance use is
#: exercised over a range a Lua integer could not hold.
NAMED_WORLD_LANG_SEED = "12345678901234567890"


def gen_named_world(port: int, page: str, seed: int, size: int) -> None:
    """A world with a #1092 language provenance, so its placed
    locations get generated names (#1101) instead of ldLabel."""
    send(port, f"world.init('{page}', {seed}, {size}, 3, 'Vashenkoro', "
               f"'Ashen Land', '{NAMED_WORLD_LANG_SEED}'); return 'ok'")
    send(port, "return world.waitForInit(240)", timeout=250)
    send(port, f"world.show('{page}'); return 'ok'")
    send(port, "return world.loadChunksInRegion(-1,-1,1,1)")
    send(port, "return world.waitForChunks(60)", timeout=65)

def ruins_ready(port: int, page: str, tries: int = 40) -> list[dict]:
    """The `ruin_small` rows on `page`, polled until non-empty (#1101).

    A second world.init in a session already holding a live page cannot
    be waited on with world.waitForInit -- it reads the ACTIVE world's
    phase (worldWaitForInitFn), which is already `done` -- so the page's
    gen params, and with them its overlay, become readable only some
    time after the call returns. Same reason `loc_at` polls.
    """
    last: list[dict] = []
    for _ in range(tries):
        last = [e for e in placed(port, page) if e["id"] == "ruin_small"]
        if last:
            return last
        time.sleep(0.5)
    return last



def check_generated_names(args, state: ScenarioState,
                          failures: list[str]) -> bool:
    """Generate the named world and the unnamed one on the same terrain
    seed, and check both readings.

    Answers True when the provenance-bearing world really placed ruins
    -- the condition under which the facade takes the save that the
    reload phase reads.
    """
    gen_named_world(args.port, "ln", args.seed, args.size)
    prov = send(args.port, "return world.getLanguageProvenance('ln')")
    if NAMED_WORLD_LANG_SEED not in prov:
        failures.append(
            f"phase 5 (#1101): the page recorded no language provenance "
            f"— every assertion below would only be testing the "
            f"no-language fallback; got {prov!r}")
    rows = ruins_ready(args.port, "ln")
    if not rows:
        failures.append("phase 5 (#1101): no ruin_small placed on the "
                        "named world")
        return False
    named = {e["instance_id"]: (e["name"], e.get("gloss"))
             for e in rows}
    bad = [(i, n, g) for i, (n, g) in named.items()
           if not n or n == "Small Ruin" or not g]
    if bad:
        failures.append(
            f"phase 5 (#1101): expected a generated name + gloss on "
            f"every ruin, got {bad}")
    else:
        print(f"PASS: {len(named)} ruin(s) named in the world's own "
              f"language: "
              + ", ".join(f"{n} ({g})" for n, g in named.values()))

    # The SAME terrain seed with no language: the fallback, and
    # the gloss key absent rather than empty.
    gen_world(args.port, "lp", args.seed, args.size)
    plain = ruins_ready(args.port, "lp")
    wrong = [e for e in plain
             if e["name"] != "Small Ruin" or "gloss" in e]
    if not plain:
        failures.append("phase 5 (#1101): no ruin_small placed on "
                        "the unnamed world")
    elif wrong:
        failures.append(
            f"phase 5 (#1101): an unnamed world must fall back to "
            f"ldLabel with NO gloss key, got "
            f"{[(e['name'], e.get('gloss')) for e in wrong]}")
    else:
        print(f"PASS: the same seed with no language falls back to "
              f"'Small Ruin' on all {len(plain)} ruin(s), no gloss")
    state.named = named
    return True


def check_names_survived_reload(args, state: ScenarioState,
                                failures: list[str]) -> None:
    """The fresh process: every name and gloss came back byte-exact
    from the save, and regenerating the same seed + language from
    scratch reproduces them -- write-once storage would otherwise hide a
    nondeterministic namer."""
    after = {e["instance_id"]: (e["name"], e.get("gloss"))
             for e in ruins_ready(args.port, "ln")}
    if after == state.named:
        print("PASS: every location name AND gloss survived "
              "save -> fresh process -> load byte-exact")
    else:
        failures.append(
            f"phase 5 (#1101): names/glosses changed across "
            f"save/load: before={state.named} after={after}")

    # Same seed, same language, fresh process: identical
    # names. Write-once storage would hide a nondeterministic
    # namer, so this regenerates rather than reloading.
    gen_named_world(args.port, "ln2", args.seed, args.size)
    regen = {e["instance_id"]: (e["name"], e.get("gloss"))
             for e in ruins_ready(args.port, "ln2")}
    if regen == state.named:
        print("PASS: regenerating the same seed + language in a "
              "fresh process reproduces every name and gloss")
    else:
        failures.append(
            f"phase 5 (#1101): regeneration is not deterministic: "
            f"first={state.named} regenerated={regen}")
