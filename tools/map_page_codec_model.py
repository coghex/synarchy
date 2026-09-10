"""Engine-free corpus checks and explicitly synthetic cache arithmetic (#2303)."""
from collections import OrderedDict
from fractions import Fraction
import re

DECODED_BYTES = 514 * 514 * 4
CATEGORIES = {"ocean", "varied-land", "ice", "lava", "transparency",
              "latitude-edge", "longitude-seam", "parity"}
CODECS = ("raw", "png")


def ratio(encoded, decoded=DECODED_BYTES):
    if encoded < 0 or decoded < 0:
        raise ValueError("negative byte count")
    return Fraction(encoded, decoded) if decoded else None


def level_shape(size, level):
    if type(size) is not int or size < 8 or size % 8:
        raise ValueError("world size must be normalized (>=8, divisible by 8)")
    root = 0
    while size * 32 // (2 ** root) > 2048:
        root += 1
    if type(level) is not int or not 0 <= level <= root:
        raise ValueError("invalid level")
    width, height = size * 16 // (2 ** level), size * 32 // (2 ** level)
    return (width + 511) // 512, (height + 511) // 512


def validate_corpus(worlds):
    ids, covered = set(), set()
    if not worlds:
        raise ValueError("empty corpus")
    for world in worlds:
        if type(world["seed"]) is not int or not 0 <= world["seed"] < 2 ** 64:
            raise ValueError("invalid seed")
        if type(world["plates"]) is not int or world["plates"] < 1:
            raise ValueError("invalid plate count")
        if not world["pages"]:
            raise ValueError("empty world")
        keys = set()
        for page in world["pages"]:
            ident = page["id"]
            if not re.fullmatch(r"[a-z0-9-]+", ident) or ident in ids:
                raise ValueError("unsafe or duplicate page id")
            ids.add(ident)
            u, v, level = page["u"], page["v"], page["level"]
            nu, nv = level_shape(world["size"], level)
            if type(u) is not int or type(v) is not int or not (0 <= u < nu and 0 <= v < nv):
                raise ValueError("page outside level inventory")
            if (level, u, v) in keys:
                raise ValueError("duplicate page address")
            keys.add((level, u, v))
            # Every finest cell on both staggered rows maps to an even
            # physical u+v. These are PAGE coords, not raw lattice coords.
            for row in (0, 1):
                cu, cv = u * 16 * 2 ** level, v * 16 * 2 ** level + row
                lattice_u = 2 * cu + cv % 2 - world["size"] // 2
                lattice_v = cv - world["size"] // 2
                if (lattice_u + lattice_v) % 2:
                    raise ValueError("invalid cell parity")
            categories = set(page["categories"])
            if not categories or not categories <= CATEGORIES:
                raise ValueError("invalid categories")
            covered |= categories
    if covered != CATEGORIES:
        raise ValueError(f"missing categories: {sorted(CATEGORIES - covered)}")


def verify_observation(page, observed):
    """Labels are claims. Missing real terrain is a fixture FAILURE."""
    checks = {
        "ocean": observed["ocean_source_tiles"] > 0,
        "lava": observed["lava_source_tiles"] > 0,
        "ice": observed["ice_source_tiles"] > 0,
        "varied-land": len(observed["dry_material_ids"]) >= 3,
        "transparency": observed["transparent_pixels"] > 0,
        "longitude-seam": observed["longitude_seam"],
        "latitude-edge": observed["latitude_edge"],
        "parity": observed["parity_round_trip"],
    }
    return [c for c in page["categories"] if not checks[c]]


def damage(pristine, decoded):
    if decoded is None:
        return {"different_bytes": None, "different_pixels": None}
    if len(pristine) != len(decoded) or len(pristine) % 4:
        raise ValueError("damage requires matching RGBA dimensions")
    return {"different_bytes": sum(a != b for a, b in zip(pristine, decoded)),
            "different_pixels": sum(pristine[i:i+4] != decoded[i:i+4]
                                    for i in range(0, len(pristine), 4))}


def corruption_offsets(size):
    if size < 3:
        raise ValueError("encoded page too short")
    return [0, size // 2, size - 1]


def lru(requests, sizes, quota):
    """Cold byte-bounded LRU. Oversized requests miss and bypass storage."""
    if quota < 0 or any(n <= 0 for n in sizes.values()):
        raise ValueError("invalid quota or page size")
    cache, distinct = OrderedDict(), set()
    used = peak = hits = count = 0
    for key in requests:
        count += 1
        distinct.add(key)
        if key in cache:
            hits += 1
            cache.move_to_end(key)
        elif sizes[key] <= quota:
            while used + sizes[key] > quota:
                _, old = cache.popitem(last=False)
                used -= old
            cache[key] = sizes[key]
            used += sizes[key]
            peak = max(peak, used)
    repeats = count - len(distinct)
    return {"requests": count, "hits": hits, "misses": count - hits,
            "first_requests": len(distinct), "repeat_requests": repeats,
            "repeat_misses": repeats - hits,
            "repeat_hit_rate": hits / repeats if repeats else None,
            "unbounded_hit_rate": repeats / count if count else None,
            "hit_rate": float(Fraction(hits, count)) if count else None,
            "peak_resident_bytes": peak,
            "distinct_working_set_bytes": sum(sizes[k] for k in distinct),
            "distinct_pages": len(distinct)}


def camera_trace(kind, size=8192):
    """Declared synthetic cameras, NOT observed player telemetry.

    A 4K planning viewport: 8x5 pages plus one-page ring -> 10x7.
    Move by one page at a time. Each camera requests row-major pages,
    wrapping longitude, clipping latitude. Zoom steps use levels 0,1,2.
    """
    if kind == "home-expeditions":
        cameras = []
        for expedition in range(8):
            route = [(16 + i, 24 + expedition * 6 + i // 3, expedition % 3)
                     for i in range(40)]
            cameras += [(16, 24, 0)] * 3 + route + route[::-1] + [(16, 24, 0)] * 3
    elif kind == "frontier":
        cameras = [(i, 30 + i // 8, (i // 80) % 3) for i in range(240)]
    elif kind == "distant-inspection":
        cameras = [(i * 37, i * 53, i % 3) for i in range(120)]
        cameras += cameras[::-1]
    else:
        raise ValueError("unknown camera trace")
    requests = []
    for cu, cv, level in cameras:
        nu, nv = level_shape(size, level)
        center_u, center_v = cu % nu, cv % nv
        requests.extend((level, (center_u + du) % nu, center_v + dv)
                        for dv in range(-3, 4) if 0 <= center_v + dv < nv
                        for du in range(-4, 6))
    return requests


def compare_checks(rows):
    """Every codec stays visible, including failures and missing results."""
    return {codec: {"pages": sum(r["codec"] == codec for r in rows),
                    "passed": bool([r for r in rows if r["codec"] == codec]) and
                    all(r["deterministic"] and r["corruption_detected"]
                        and r["round_trip"] for r in rows if r["codec"] == codec)}
            for codec in CODECS}
