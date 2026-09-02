#!/usr/bin/env python3
"""Headless probe for issue #604/#606: blood decal model, procedural
texture generation, and world-render records.

Boots headless, builds a flat arena (no worldgen/AI needed — blood.* is a
pure registry op, not tied to terrain or units), and drives the blood.*
debug Lua surface (Engine.Scripting.Lua.API.Blood / Blood.Types /
Blood.Texture / Blood.Render) end to end.

Checks:
  1. same/near-same requests reuse a texture descriptor.
  2. different styles or severity buckets create distinct descriptors.
  3. exceeding the texture cap evicts the oldest descriptor.
  4. evicting a descriptor removes associated decals.
  5. every listed texture reports generated pixel data (width/height/hash).
  6. a spawned decal produces a renderable record (blood.getRenderQuads).
  7. an evicted texture's decals never appear in getRenderQuads either.
  8. a decal spawned already-dry reports a darker, fainter render tint
     than a decal spawned fresh.
  9. clear leaves both descriptor and decal lists empty.

blood.getRenderQuads() deliberately doesn't touch the GPU (no headless
device exists to upload to) — it reports the same resolved data
(Blood.Render.bloodRenderRecords) the real renderer
(World.Render.BloodQuads) turns into world-space quads once
uploadBloodTextures has run on a graphical session.

PASS  = all checks hold.
FAIL  = any check violated (bug in the model/debug surface).

This probe implements the shared `probe-result/v1` contract: `--describe`
prints its ordered stable checks without booting an engine, and a harnessed
run writes structured events while a standalone run keeps its human-readable
per-check output.
"""
from __future__ import annotations
import argparse
import sys
import probe_protocol
from probelib import quit_engine, boot, init_arena, send, send_json

PORT = 9011
LOG = "/tmp/blood_decal_probe_engine.log"
LOG_NAME = "blood_decal_probe_engine.log"
PROBE_KEY = "blood_decal"
CHECKS = [
    ("near_requests_reuse", "same and near-same requests reuse one texture"),
    ("distinct_requests_mint", "different style and severity requests mint distinct textures"),
    ("fifo_order_reported", "texture lookup reports the correct FIFO order"),
    ("oldest_texture_evicted", "exceeding the cap evicts the oldest texture"),
    ("eviction_removes_decals", "texture eviction removes exactly its associated decals"),
    ("pixel_data_bounded", "live textures expose generated bounded pixel data"),
    ("render_quads_live_only", "render quads contain every live decal and no evicted decal"),
    ("dry_tint_ages", "an already-dry decal is darker and fainter than a fresh decal"),
    ("clear_empties_registry", "clear empties the texture and decal registries"),
]
DESCRIPTOR = probe_protocol.build_descriptor(PROBE_KEY, CHECKS)


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
        raise RuntimeError(
            f"blood.spawn({gx},{gy},{wound!r},{severity!r},{props}) -> {result!r}")
    return result


def list_textures() -> list:
    return send_json(PORT, "return blood.listTextures()") or []


def list_decals() -> list:
    return send_json(PORT, "return blood.listDecals()") or []


def get_texture(tid: int):
    return send_json(PORT, f"return blood.getTexture({tid})")


def get_texture_cap() -> int:
    return int(float(send(PORT, "return blood.getTextureCap()")))


def get_render_quads() -> list:
    return send_json(PORT, "return blood.getRenderQuads()") or []


def main() -> int:
    ap = argparse.ArgumentParser(description=__doc__)
    ap.add_argument("--port", type=int, default=9011)
    ap.add_argument("--describe", action="store_true")
    args = ap.parse_args()
    if args.describe:
        print(DESCRIPTOR.to_json())
        return 0
    rep = probe_protocol.reporter_from_env(DESCRIPTOR)
    try:
        return _run(args.port, rep)
    finally:
        rep.close()


def _run(port: int, rep: probe_protocol.Reporter) -> int:
    global PORT
    PORT = port

    proc = boot(PORT, log=rep.engine_log_path(LOG_NAME, LOG),
                args=rep.engine_args())
    try:
        init_arena(PORT)

        cap = get_texture_cap()
        if cap <= 0:
            rep.abort(f"blood.getTextureCap() = {cap}", {"cap": cap})
            return 2
        rep.note(f"texture cap = {cap}")

        # --- 1. same/near-same requests reuse a texture descriptor -----
        s1 = spawn(10, 10, "stab", "moderate", {"footprint": "medium"})
        s2 = spawn(11, 11, "stab", "moderate", {"footprint": "medium"})
        if s2["textureId"] != s1["textureId"] or s2["isNew"]:
            rep.check("near_requests_reuse", False,
                      "identical request minted a new texture",
                      {"first": s1["textureId"], "second": s2["textureId"]})
            return 1
        s3 = spawn(12, 12, "stab", "moderate", {"footprint": "large"})
        if s3["textureId"] != s1["textureId"] or s3["isNew"]:
            rep.check("near_requests_reuse", False,
                      "a one-bucket-step-away request did not reuse the near-matching texture",
                      {"first": s1["textureId"], "near": s3["textureId"]})
            return 1
        rep.check("near_requests_reuse", True,
                  f"same/near-same requests reused texture {s1['textureId']}",
                  {"texture": s1["textureId"],
                   "decals": [s1["decalId"], s2["decalId"], s3["decalId"]]})

        # --- 2. different styles/severities create distinct descriptors --
        s4 = spawn(13, 13, "stab", "moderate",
                    {"style": "streak", "footprint": "medium"})
        if s4["textureId"] == s1["textureId"] or not s4["isNew"]:
            rep.check("distinct_requests_mint", False,
                      "a different style reused the same texture",
                      {"texture": s4["textureId"]})
            return 1
        s5 = spawn(14, 14, "stab", "severe", {"footprint": "medium"})
        if s5["textureId"] in (s1["textureId"], s4["textureId"]) or not s5["isNew"]:
            rep.check("distinct_requests_mint", False,
                      "a different severity bucket reused an existing texture",
                      {"texture": s5["textureId"]})
            return 1
        rep.check("distinct_requests_mint", True,
                  "different style and severity each minted a distinct texture",
                  {"style_texture": s4["textureId"],
                   "severity_texture": s5["textureId"]})

        listed = list_textures()
        textures_so_far = len(listed)
        if textures_so_far != 3:
            rep.abort("expected three distinct textures before the FIFO check",
                      {"textures": textures_so_far})
            return 2

        # blood.getTexture's reported FIFO order must match its actual
        # rank in listTextures() (oldest = 0), not a hardcoded value.
        expected_order = {t["id"]: i for i, t in enumerate(listed)}
        for tid in (s1["textureId"], s4["textureId"], s5["textureId"]):
            got = get_texture(tid)
            if not got or got.get("order") != expected_order[tid]:
                rep.check("fifo_order_reported", False,
                          "blood.getTexture reported the wrong FIFO order",
                          {"texture": tid,
                           "actual": got and got.get("order"),
                           "expected": expected_order[tid]})
                return 1
        rep.check("fifo_order_reported", True,
                  "blood.getTexture reports the correct FIFO order")

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
                rep.abort("a capacity filler unexpectedly reused a texture",
                          {"filler": i, "texture": f["textureId"]})
                return 2
            filler_decal_ids.append(f["decalId"])

        at_cap = list_textures()
        if len(at_cap) != cap:
            rep.abort("texture pool did not reach its reported capacity",
                      {"expected": cap, "actual": len(at_cap)})
            return 2

        overflow = spawn(99, 99, "overflowkind", "catastrophic",
                          {"style": "smear"})
        if not overflow["isNew"]:
            rep.abort("overflow request unexpectedly reused a texture")
            return 2

        after_evict = list_textures()
        after_ids = {t["id"] for t in after_evict}
        if len(after_evict) != cap:
            rep.check("oldest_texture_evicted", False,
                      "pool size changed after overflow",
                      {"expected": cap, "actual": len(after_evict)})
            return 1
        if s1["textureId"] in after_ids:
            rep.check("oldest_texture_evicted", False,
                      "the oldest texture was not evicted",
                      {"texture": s1["textureId"], "cap": cap})
            return 1
        if get_texture(s1["textureId"]) is not None:
            rep.check("oldest_texture_evicted", False,
                      "the evicted texture still resolves by id",
                      {"texture": s1["textureId"]})
            return 1
        rep.check("oldest_texture_evicted", True,
                  f"exceeding the cap ({cap}) evicted the oldest texture",
                  {"texture": s1["textureId"], "pool_size": len(after_evict)})

        decal_ids_after = {d["id"] for d in list_decals()}
        evicted_decal_ids = {s1["decalId"], s2["decalId"], s3["decalId"]}
        still_present = evicted_decal_ids & decal_ids_after
        if still_present:
            rep.check("eviction_removes_decals", False,
                      "decals referencing the evicted texture are still listed",
                      {"decals": sorted(still_present)})
            return 1
        survivors = {s4["decalId"], s5["decalId"], overflow["decalId"],
                     *filler_decal_ids}
        missing_survivors = survivors - decal_ids_after
        if missing_survivors:
            rep.check("eviction_removes_decals", False,
                      "decals on a live texture were wrongly removed",
                      {"decals": sorted(missing_survivors)})
            return 1
        rep.check("eviction_removes_decals", True,
                  "evicting the oldest texture removed exactly its decals",
                  {"removed": len(evicted_decal_ids)})

        # --- 5. every listed texture reports generated pixel data ------
        for t in after_evict:
            w, h, ph = t.get("width"), t.get("height"), t.get("pixelHash")
            if not (isinstance(w, (int, float)) and w > 0
                    and isinstance(h, (int, float)) and h > 0
                    and ph is not None):
                rep.check("pixel_data_bounded", False,
                          "a live texture is missing generated pixel data",
                          {"texture": t.get("id"), "width": w,
                           "height": h, "pixel_hash": ph})
                return 1
            if w > 32 or h > 32:
                rep.check("pixel_data_bounded", False,
                          "a live texture exceeds the bounded maximum size",
                          {"texture": t.get("id"), "width": w, "height": h})
                return 1
        rep.check("pixel_data_bounded", True,
                  "all live textures report generated bounded pixel data",
                  {"textures": len(after_evict)})

        # --- 6/7. renderable records exist for live decals, never for --
        #          evicted ones ------------------------------------------
        quads = get_render_quads()
        quad_decal_ids = {q["decal"] for q in quads}
        if not survivors.issubset(quad_decal_ids):
            rep.check("render_quads_live_only", False,
                      "render quads are missing live decals",
                      {"decals": sorted(survivors - quad_decal_ids)})
            return 1
        if evicted_decal_ids & quad_decal_ids:
            rep.check("render_quads_live_only", False,
                      "render quads still contain evicted decals",
                      {"decals": sorted(evicted_decal_ids & quad_decal_ids)})
            return 1
        for q in quads:
            for key in ("texture", "x", "y", "tintR", "tintG", "tintB", "alpha"):
                if key not in q:
                    rep.check("render_quads_live_only", False,
                              "a render record is missing required data",
                              {"decal": q.get("decal"), "field": key})
                    return 1
        rep.check("render_quads_live_only", True,
                  "getRenderQuads reports all live decals and none evicted",
                  {"decals": len(quad_decal_ids)})

        # --- 8. an already-dry decal reads darker/fainter than a fresh --
        #        one spawned at (about) the same time ---------------------
        fresh = spawn(50, 50, "agingcheck", "moderate",
                      {"style": "pool", "wetness": 1})
        old = spawn(51, 51, "agingcheck", "moderate",
                    {"style": "pool", "wetness": 0.02})
        if old["textureId"] != fresh["textureId"]:
            rep.abort("fresh and old aging fixtures minted different textures",
                      {"fresh": fresh["textureId"], "old": old["textureId"]})
            return 2
        by_decal = {q["decal"]: q for q in get_render_quads()}
        freshQ, oldQ = by_decal.get(fresh["decalId"]), by_decal.get(old["decalId"])
        if freshQ is None or oldQ is None:
            rep.check("dry_tint_ages", False,
                      "aging fixtures are missing from render quads",
                      {"fresh": freshQ, "old": oldQ})
            return 1
        if not (oldQ["alpha"] < freshQ["alpha"] and oldQ["tintR"] < freshQ["tintR"]):
            rep.check("dry_tint_ages", False,
                      "an already-dry decal is not darker and fainter than fresh",
                      {"fresh": freshQ, "old": oldQ})
            return 1
        rep.check("dry_tint_ages", True,
                  "aged decal tint is darker and fainter than fresh",
                  {"old_alpha": oldQ["alpha"], "old_tint": oldQ["tintR"],
                   "fresh_alpha": freshQ["alpha"], "fresh_tint": freshQ["tintR"]})

        # --- 9. clear leaves both lists empty ---------------------------
        cleared = send(PORT, "return blood.clear()")
        if cleared.lower() != "true":
            rep.check("clear_empties_registry", False,
                      "blood.clear returned false", {"result": cleared})
            return 1
        remaining_textures = list_textures()
        remaining_decals = list_decals()
        if remaining_textures or remaining_decals:
            rep.check("clear_empties_registry", False,
                      "clear left textures or decals behind",
                      {"textures": remaining_textures, "decals": remaining_decals})
            return 1
        rep.check("clear_empties_registry", True,
                  "clear left both the texture and decal lists empty")

        rep.note("\nPASS: all blood decal model + debug surface checks held")
        return 0
    except RuntimeError as error:
        rep.abort(str(error))
        return 2
    finally:
        quit_engine(PORT, proc)


if __name__ == "__main__":
    sys.exit(main())
