#!/usr/bin/env python3
"""Headless probe for issue #604: blood decal model + debug surface.

Boots headless, builds a flat arena (no worldgen/AI needed — blood.* is a
pure registry op, not tied to terrain or units), and drives the blood.*
debug Lua surface (Engine.Scripting.Lua.API.Blood / Blood.Types) end to
end.

Checks:
  1. same/near-same requests reuse a texture descriptor.
  2. different styles or severity buckets create distinct descriptors.
  3. exceeding the texture cap evicts the oldest descriptor.
  4. evicting a descriptor removes associated decals.
  5. clear leaves both descriptor and decal lists empty.

PASS  = all checks hold.
FAIL  = any check violated (bug in the model/debug surface).
"""
from __future__ import annotations
import sys
from probelib import quit_engine, boot, init_arena, send, send_json

PORT = 9009
LOG = "/tmp/blood_decal_probe_engine.log"


def lua_props(props: dict | None) -> str:
    if not props:
        return "nil"
    parts = []
    for k, v in props.items():
        parts.append(f"{k}='{v}'" if isinstance(v, str) else f"{k}={v}")
    return "{" + ", ".join(parts) + "}"


def spawn(gx: float, gy: float, wound: str, severity: str, props: dict | None = None):
    lua = (f"local d,t,n = blood.spawn({gx}, {gy}, '{wound}', '{severity}', "
           f"{lua_props(props)}); return {{decalId=d, textureId=t, isNew=n}}")
    result = send_json(PORT, lua)
    if not isinstance(result, dict) or "decalId" not in result:
        print(f"FAIL (setup): blood.spawn({gx},{gy},{wound!r},{severity!r},"
              f"{props}) -> {result!r}")
        sys.exit(2)
    return result


def list_textures() -> list:
    return send_json(PORT, "return blood.listTextures()") or []


def list_decals() -> list:
    return send_json(PORT, "return blood.listDecals()") or []


def get_texture(tid: int):
    return send_json(PORT, f"return blood.getTexture({tid})")


def get_texture_cap() -> int:
    return int(float(send(PORT, "return blood.getTextureCap()")))


def main() -> int:
    proc = boot(PORT, log=LOG)
    try:
        init_arena(PORT)

        cap = get_texture_cap()
        if cap <= 0:
            print(f"FAIL (setup): blood.getTextureCap() = {cap}")
            return 2
        print(f"texture cap = {cap}")

        # --- 1. same/near-same requests reuse a texture descriptor -----
        s1 = spawn(10, 10, "stab", "moderate", {"footprint": "medium"})
        s2 = spawn(11, 11, "stab", "moderate", {"footprint": "medium"})
        if s2["textureId"] != s1["textureId"] or s2["isNew"]:
            print(f"FAIL: identical request minted a new texture "
                  f"({s1['textureId']} vs {s2['textureId']})")
            return 1
        s3 = spawn(12, 12, "stab", "moderate", {"footprint": "large"})
        if s3["textureId"] != s1["textureId"] or s3["isNew"]:
            print("FAIL: a one-bucket-step-away request did not reuse "
                  f"the near-matching texture ({s1['textureId']} vs "
                  f"{s3['textureId']})")
            return 1
        print(f"PASS: same/near-same requests reused texture {s1['textureId']} "
              f"(decals {s1['decalId']}, {s2['decalId']}, {s3['decalId']})")

        # --- 2. different styles/severities create distinct descriptors --
        s4 = spawn(13, 13, "stab", "moderate",
                    {"style": "streak", "footprint": "medium"})
        if s4["textureId"] == s1["textureId"] or not s4["isNew"]:
            print("FAIL: a different style reused the same texture "
                  f"({s4['textureId']})")
            return 1
        s5 = spawn(14, 14, "stab", "severe", {"footprint": "medium"})
        if s5["textureId"] in (s1["textureId"], s4["textureId"]) or not s5["isNew"]:
            print("FAIL: a different severity bucket reused an existing "
                  f"texture ({s5['textureId']})")
            return 1
        print(f"PASS: different style ({s4['textureId']}) and severity "
              f"({s5['textureId']}) each minted a distinct texture")

        listed = list_textures()
        textures_so_far = len(listed)
        if textures_so_far != 3:
            print(f"FAIL (setup): expected 3 distinct textures so far, "
                  f"got {textures_so_far}")
            return 2

        # blood.getTexture's reported FIFO order must match its actual
        # rank in listTextures() (oldest = 0), not a hardcoded value.
        expected_order = {t["id"]: i for i, t in enumerate(listed)}
        for tid in (s1["textureId"], s4["textureId"], s5["textureId"]):
            got = get_texture(tid)
            if not got or got.get("order") != expected_order[tid]:
                print(f"FAIL: blood.getTexture({tid}) order={got and got.get('order')!r}, "
                      f"expected {expected_order[tid]}")
                return 1
        print("PASS: blood.getTexture reports the correct FIFO order")

        # --- 3/4. exceeding the cap evicts the oldest descriptor, and --
        #          cascades to every decal that referenced it ----------
        # s1's texture (tex A) is the OLDEST (order 0) — s2 and s3 also
        # reference it via near-match reuse. Fill the pool up to exactly
        # `cap` with mutually-distinct filler textures (a fixed style/
        # severity combo disjoint from A/B/C, varied only by wound kind
        # text — a wound-kind-only difference is worth more than the
        # near-match threshold on its own, so every filler is guaranteed
        # distinct from A/B/C and from every other filler), then spawn
        # ONE more distinct texture to push the pool over cap.
        fillers_needed = cap - textures_so_far
        filler_decal_ids = []
        for i in range(fillers_needed):
            f = spawn(20 + i, 20 + i, f"fillerkind{i}", "minor",
                       {"style": "drops"})
            if not f["isNew"]:
                print(f"FAIL (setup): filler {i} unexpectedly reused a "
                      f"texture ({f['textureId']})")
                return 2
            filler_decal_ids.append(f["decalId"])

        at_cap = list_textures()
        if len(at_cap) != cap:
            print(f"FAIL (setup): expected exactly {cap} textures at "
                  f"capacity, got {len(at_cap)}")
            return 2

        overflow = spawn(99, 99, "overflowkind", "catastrophic",
                          {"style": "smear"})
        if not overflow["isNew"]:
            print("FAIL (setup): overflow request unexpectedly reused a "
                  "texture")
            return 2

        after_evict = list_textures()
        after_ids = {t["id"] for t in after_evict}
        if len(after_evict) != cap:
            print(f"FAIL: pool size after overflow is {len(after_evict)}, "
                  f"expected it to stay capped at {cap}")
            return 1
        if s1["textureId"] in after_ids:
            print(f"FAIL: the oldest texture ({s1['textureId']}) was not "
                  "evicted despite exceeding the cap")
            return 1
        if get_texture(s1["textureId"]) is not None:
            print(f"FAIL: blood.getTexture({s1['textureId']}) still "
                  "resolves after eviction")
            return 1
        print(f"PASS: exceeding the cap ({cap}) evicted the oldest "
              f"texture ({s1['textureId']}); pool holds {len(after_evict)}")

        decal_ids_after = {d["id"] for d in list_decals()}
        evicted_decal_ids = {s1["decalId"], s2["decalId"], s3["decalId"]}
        still_present = evicted_decal_ids & decal_ids_after
        if still_present:
            print(f"FAIL: decal(s) {still_present} referencing the evicted "
                  "texture are still listed")
            return 1
        survivors = {s4["decalId"], s5["decalId"], overflow["decalId"],
                     *filler_decal_ids}
        missing_survivors = survivors - decal_ids_after
        if missing_survivors:
            print(f"FAIL: decal(s) {missing_survivors} on a non-evicted "
                  "texture were wrongly removed")
            return 1
        print("PASS: evicting the oldest texture cascade-removed exactly "
              f"the {len(evicted_decal_ids)} decal(s) that referenced it")

        # --- 5. clear leaves both lists empty ---------------------------
        cleared = send(PORT, "return blood.clear()")
        if cleared.lower() != "true":
            print(f"FAIL: blood.clear() returned {cleared!r}")
            return 1
        remaining_textures = list_textures()
        remaining_decals = list_decals()
        if remaining_textures or remaining_decals:
            print(f"FAIL: after clear, textures={remaining_textures!r} "
                  f"decals={remaining_decals!r}")
            return 1
        print("PASS: clear left both the texture and decal lists empty")

        print("\nPASS: all blood decal model + debug surface checks held")
        return 0
    finally:
        quit_engine(PORT, proc)


if __name__ == "__main__":
    sys.exit(main())
