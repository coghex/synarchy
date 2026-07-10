#!/usr/bin/env python3
"""Persona interface (#647) + persona generation (C2, #649).

THE INTERFACE — the contract H1 consumes. A persona is a small
structured blob handed to H1:

    name:        str  — short identifier (also the trace's label)
    temperament: str  — who this player is (the persona blurb)
    goal:        str  — what they're trying to accomplish this session
    tendencies:  [str] — behavioral leanings the player role-plays

Optional: prose (str) — extra freeform flavor appended to the prompt.

Files are YAML (or JSON — a JSON file is valid YAML) in this package's
personas/ directory; generated personas conform to the same schema and
are passed by path (`run.py --persona path/to/file.yaml`). H1 ships
three hardcoded placeholders so it runs standalone.

THE GENERATOR — `generate_persona(seed)` is a pure function of a seed:
it samples one value per behavioral axis (six orthogonal axes; the
sixth, `goal`, becomes H1's goal text) from `personas/axes.yaml`, maps
each sampled value to a behavioral tendency and a temperament fragment,
and assembles a spec that is a strict superset of the H1 schema (extra
fields: `seed`, `axes`, `sampling` — provenance H1 ignores but the
trace records for replay). Same seed -> identical spec, always.

`coverage_personas(count, seed)` is the campaign-spread alternative:
a balanced Latin-hypercube-style sample across the axis space (each
axis value appears equally often, combos deduplicated) so a batch
deliberately spans combinations instead of clustering. Reproducible
from (seed, count).

`llm_flavor(spec)` optionally rewrites the name + temperament with a
cheap model. The prose is FROZEN into the returned spec at generation
time — downstream (H1 trace, replay) always reuses the stored text and
never regenerates, so the LLM's non-determinism can't leak into
replays. Axes/goal/tendencies stay seed-deterministic regardless.

CLI (preview + file emission):

    python3 tools/playtest/personas.py --seed 42
    python3 tools/playtest/personas.py --seed 42 --count 5
    python3 tools/playtest/personas.py --coverage --count 12
    python3 tools/playtest/personas.py --seed 42 --llm [--model M]
    python3 tools/playtest/personas.py --seed 42 --count 5 --out DIR
    python3 tools/playtest/personas.py --selftest   # offline, no API
"""
from __future__ import annotations

import argparse
import itertools
import json
import math
import os
import random
import re
import sys

PERSONA_DIR = os.path.join(os.path.dirname(os.path.abspath(__file__)), "personas")
AXES_FILENAME = "axes.yaml"
AXES_PATH = os.path.join(PERSONA_DIR, AXES_FILENAME)
REQUIRED_FIELDS = ("name", "temperament", "goal", "tendencies")

# The five behavioral axes the temperament template weaves into prose.
# axes.yaml must define at least these; any EXTRA axis added there is
# picked up generically (tendency + an extra template sentence).
TEMPLATE_AXES = ("experience", "patience", "reads_guidance", "play_style",
                 "persistence")

# Cheap + configurable — the blurb is one paragraph of prose (#649
# design call; claude-sonnet-5 is the other suggested choice).
DEFAULT_FLAVOR_MODEL = "claude-haiku-4-5"

FLAVOR_SCHEMA = {
    "type": "object",
    "properties": {
        "name": {"type": "string",
                 "description": "short snake_case handle, adjective_firstname style"},
        "blurb": {"type": "string",
                  "description": "one-paragraph third-person persona voice"},
    },
    "required": ["name", "blurb"],
    "additionalProperties": False,
}


def _yaml():
    try:
        import yaml
        return yaml
    except ImportError as e:
        raise SystemExit(
            "persona generation needs PyYAML: pip install pyyaml") from e


# ---------------------------------------------------------------------------
# Loading (the H1 interface — #647)
# ---------------------------------------------------------------------------

def _load_file(path: str) -> dict:
    with open(path, encoding="utf-8") as f:
        text = f.read()
    try:
        import yaml  # PyYAML — present in the dev env; JSON fallback below
        data = yaml.safe_load(text)
    except ImportError:
        data = json.loads(text)
    if not isinstance(data, dict):
        raise ValueError(f"{path}: persona must be a mapping")
    missing = [k for k in REQUIRED_FIELDS if not data.get(k)]
    if missing:
        raise ValueError(f"{path}: persona missing fields: {', '.join(missing)}")
    if not isinstance(data["tendencies"], list):
        raise ValueError(f"{path}: tendencies must be a list of strings")
    return data


def list_personas() -> list[str]:
    names = []
    for fn in sorted(os.listdir(PERSONA_DIR)):
        if fn == AXES_FILENAME:  # generator data, not a persona
            continue
        if fn.endswith((".yaml", ".yml", ".json")):
            names.append(os.path.splitext(fn)[0])
    return names


def load_persona(name_or_path: str) -> dict:
    """Load by bundled name ('curious_carl') or by file path (the C2
    hand-off shape)."""
    if os.path.sep in name_or_path or os.path.isfile(name_or_path):
        return _load_file(name_or_path)
    for ext in (".yaml", ".yml", ".json"):
        candidate = os.path.join(PERSONA_DIR, name_or_path + ext)
        if os.path.isfile(candidate):
            return _load_file(candidate)
    raise FileNotFoundError(
        f"no persona named {name_or_path!r}; bundled: {', '.join(list_personas())}")


# ---------------------------------------------------------------------------
# Generation (C2 — #649)
# ---------------------------------------------------------------------------

def load_axes(path: str = AXES_PATH) -> dict:
    """Load + validate the axis data. Axis order in the file IS the
    sampling order — reordering changes what a given seed produces."""
    yaml = _yaml()
    with open(path, encoding="utf-8") as f:
        data = yaml.safe_load(f)

    def bad(msg):
        raise ValueError(f"{path}: {msg}")

    if not isinstance(data, dict):
        bad("top level must be a mapping")
    axes = data.get("axes")
    if not isinstance(axes, list) or not axes:
        bad("'axes' must be a non-empty list")
    seen_axes = set()
    for axis in axes:
        name = axis.get("name") if isinstance(axis, dict) else None
        if not name or not isinstance(name, str):
            bad("every axis needs a string 'name'")
        if name in seen_axes:
            bad(f"duplicate axis {name!r}")
        seen_axes.add(name)
        values = axis.get("values")
        if not isinstance(values, list) or len(values) < 2:
            bad(f"axis {name!r} needs >= 2 values")
        seen_vals = set()
        for v in values:
            for field in ("value", "adjective", "tendency", "blurb"):
                if not isinstance(v, dict) or not v.get(field) \
                        or not isinstance(v[field], str):
                    bad(f"axis {name!r}: every value needs a string {field!r}")
            if v["value"] in seen_vals:
                bad(f"axis {name!r}: duplicate value {v['value']!r}")
            seen_vals.add(v["value"])
    for name in TEMPLATE_AXES:
        if name not in seen_axes:
            bad(f"missing required axis {name!r}")
    goals = data.get("goals")
    if not isinstance(goals, list) or len(goals) < 2:
        bad("'goals' must be a list with >= 2 entries")
    for g in goals:
        for field in ("value", "goal", "blurb"):
            if not isinstance(g, dict) or not g.get(field) \
                    or not isinstance(g[field], str):
                bad(f"every goal needs a string {field!r}")
    names = data.get("names")
    if not isinstance(names, list) or not names \
            or not all(isinstance(n, str) and n for n in names):
        bad("'names' must be a non-empty list of strings")
    return data


def _assemble(seed: int, chosen: dict, goal: dict, rng: random.Random,
              data: dict, sampling: dict) -> dict:
    """Build one persona spec from sampled axis values. Consumes exactly
    two rng draws (adjective, first name) — the draw order is part of
    the determinism contract."""
    adjective = rng.choice([entry["adjective"] for entry in chosen.values()])
    first = rng.choice(data["names"])
    display = first.capitalize()
    extra = "".join(f" They {chosen[name]['blurb']}."
                    for name in chosen if name not in TEMPLATE_AXES)
    temperament = (
        f"{display} {chosen['experience']['blurb']}. "
        f"They {chosen['patience']['blurb']}, and they "
        f"{chosen['reads_guidance']['blurb']}. "
        f"They {chosen['play_style']['blurb']}; when something goes wrong, "
        f"they {chosen['persistence']['blurb']}.{extra} "
        f"This session, {display} {goal['blurb']}.")
    return {
        "seed": seed,
        "name": f"{adjective}_{first}",
        "axes": {name: entry["value"] for name, entry in chosen.items()},
        "goal": goal["goal"].strip(),
        "temperament": temperament,
        "tendencies": [entry["tendency"] for entry in chosen.values()],
        "sampling": sampling,
    }


def generate_persona(seed: int, data: dict | None = None) -> dict:
    """Pure function of `seed` -> persona spec (H1 schema superset).
    Same seed, same axes.yaml -> byte-identical spec."""
    data = data or load_axes()
    rng = random.Random(seed)
    chosen = {axis["name"]: rng.choice(axis["values"]) for axis in data["axes"]}
    goal = rng.choice(data["goals"])
    return _assemble(seed, chosen, goal, rng, data, sampling={"mode": "random"})


def coverage_personas(count: int, seed: int = 0,
                      data: dict | None = None) -> list[dict]:
    """A balanced spread of `count` personas across the axis space:
    per-axis columns each cycle every value equally often, then get an
    independent seeded shuffle (Latin-hypercube style), so marginals are
    balanced and axes are decorrelated; duplicate combos are re-rolled.
    Reproducible from (seed, count). count >= the full grid size just
    cycles the grid enumeration."""
    data = data or load_axes()
    dims = [(a["name"], a["values"]) for a in data["axes"]] \
        + [("goal", data["goals"])]
    total = math.prod(len(vals) for _, vals in dims)
    rng = random.Random(seed)
    if count >= total:
        grid = list(itertools.product(*[vals for _, vals in dims]))
        rows = [grid[i % total] for i in range(count)]
    else:
        cols = []
        for _, vals in dims:
            col = [vals[i % len(vals)] for i in range(count)]
            rng.shuffle(col)
            cols.append(col)
        rows = [tuple(col[i] for col in cols) for i in range(count)]
        seen = set()
        for i, row in enumerate(rows):
            key = tuple(v["value"] for v in row)
            tries = 0
            while key in seen and tries < 1000:  # rare: birthday collision
                row = tuple(rng.choice(vals) for _, vals in dims)
                key = tuple(v["value"] for v in row)
                tries += 1
            seen.add(key)
            rows[i] = row
    specs = []
    for i, row in enumerate(rows):
        chosen = {dims[j][0]: row[j] for j in range(len(dims) - 1)}
        specs.append(_assemble(
            seed, chosen, row[-1], rng, data,
            sampling={"mode": "coverage", "count": count, "index": i}))
    return specs


# ---------------------------------------------------------------------------
# Optional LLM flavor (frozen into the spec — #649 reproducibility call)
# ---------------------------------------------------------------------------

def _slug(text: str) -> str:
    return re.sub(r"[^a-z0-9]+", "_", str(text).lower()).strip("_")


def _flavor_prompt(spec: dict) -> str:
    tendencies = "\n".join(f"- {t}" for t in spec["tendencies"])
    return (
        "Invent a playtest persona for a colony-sim game. The behavioral "
        "traits below are FIXED — the name and description you write must "
        "fit every one of them and contradict none.\n\n"
        f"Traits: {json.dumps(spec['axes'], sort_keys=True)}\n"
        f"Session goal: {spec['goal']}\n"
        f"Tendencies:\n{tendencies}\n\n"
        f"A plain template description, for reference: {spec['temperament']}\n\n"
        "Write a richer version: a short snake_case handle in "
        "adjective_firstname style, and a one-paragraph (3-5 sentence) "
        "third-person description of who this player is and how they "
        'behave. Reply with one JSON object: {"name": ..., "blurb": ...}')


def _anthropic_complete(prompt: str, model: str) -> dict:
    try:
        import anthropic
    except ImportError as e:
        raise SystemExit(
            "--llm needs the Anthropic SDK: pip install anthropic\n"
            "(template personas don't)") from e
    client = anthropic.Anthropic()  # ANTHROPIC_API_KEY / ant auth profile
    kwargs = dict(model=model, max_tokens=512,
                  thinking={"type": "disabled"},
                  messages=[{"role": "user", "content": prompt}])
    try:
        response = client.messages.create(
            output_config={"format": {"type": "json_schema",
                                      "schema": FLAVOR_SCHEMA}},
            **kwargs)
    except anthropic.BadRequestError:
        # Model/config without structured-output support: lenient parse.
        response = client.messages.create(**kwargs)
    except TypeError as e:
        if "authentication" not in str(e).lower():
            raise
        raise SystemExit(
            "--llm needs Anthropic credentials: set ANTHROPIC_API_KEY or "
            "run `ant auth login` (template personas don't)") from e
    text = next((b.text for b in response.content if b.type == "text"), "")
    try:
        return json.loads(text)
    except (ValueError, TypeError):
        m = re.search(r"\{.*\}", text or "", re.DOTALL)
        if not m:
            raise ValueError(f"flavor model returned no JSON: {text[:200]!r}")
        return json.loads(m.group(0))


def llm_flavor(spec: dict, model: str = DEFAULT_FLAVOR_MODEL,
               _complete=None) -> dict:
    """Rewrite name + temperament with an LLM, FREEZING the result into
    the returned spec: this is the only point prose is ever generated —
    everything downstream (persona files, the H1 trace, replay) reads
    the stored text. Axes/goal/tendencies are never touched, so they
    stay seed-deterministic. `_complete` is injectable for tests."""
    complete = _complete or _anthropic_complete
    data = complete(_flavor_prompt(spec), model)
    out = dict(spec)
    name = _slug(data.get("name", ""))
    blurb = str(data.get("blurb", "")).strip()
    if name:
        out["name"] = name
    if blurb:
        out["temperament"] = blurb
    out["llm"] = {"model": model, "flavored": True}
    return out


# ---------------------------------------------------------------------------
# CLI
# ---------------------------------------------------------------------------

def _unique_path(out_dir: str, name: str, used: set) -> str:
    """Distinct personas can roll the same adjective_firstname — keep
    the spec's name (it's seed-deterministic) but never overwrite a
    file: suffix the FILENAME instead."""
    candidate = os.path.join(out_dir, f"{name}.yaml")
    n = 2
    while candidate in used or os.path.exists(candidate):
        candidate = os.path.join(out_dir, f"{name}_{n}.yaml")
        n += 1
    used.add(candidate)
    return candidate


def _dump(spec: dict) -> str:
    return _yaml().safe_dump(spec, sort_keys=False, allow_unicode=True)


def selftest() -> int:
    """Offline check — no engine, no API key, no window."""
    import tempfile
    failures = []

    def check(name, ok, detail=""):
        print(f"  [{'ok' if ok else 'FAIL'}] {name}"
              + (f" — {detail}" if detail else ""))
        if not ok:
            failures.append(name)

    data = load_axes()
    check("axes.yaml loads and validates",
          len(data["axes"]) >= 5 and len(data["goals"]) >= 2)

    # 1. determinism: same seed -> identical spec; nearby seeds differ
    a, b = generate_persona(42, data), generate_persona(42, data)
    check("same seed -> identical spec", a == b)
    check("different seeds -> different specs",
          any(generate_persona(s, data) != a for s in (43, 44, 45)))

    # 2. H1 schema conformance: required fields + file round-trip through
    # the same loader run.py uses
    check("spec carries every H1 required field",
          all(a.get(k) for k in REQUIRED_FIELDS)
          and isinstance(a["tendencies"], list)
          and all(isinstance(t, str) for t in a["tendencies"]))
    check("one tendency per axis", len(a["tendencies"]) == len(data["axes"]))
    with tempfile.TemporaryDirectory() as tmp:
        path = os.path.join(tmp, f"{a['name']}.yaml")
        with open(path, "w", encoding="utf-8") as f:
            f.write(_dump(a))
        loaded = load_persona(path)
        check("generated file loads via load_persona", loaded == a)
        # the prompt H1 actually builds accepts the generated persona
        here = os.path.dirname(os.path.abspath(__file__))
        sys.path.insert(0, here)
        sys.path.insert(0, os.path.dirname(here))
        import agent as agent_mod
        prompt = agent_mod.build_system_prompt(loaded, "MANUAL", (1280, 720))
        check("H1 prompt assembly accepts a generated persona",
              loaded["name"] in prompt and loaded["goal"] in prompt
              and all(t in prompt for t in loaded["tendencies"]))

    # 3. axis coverage over many seeds: every value of every axis (and
    # every goal) is reachable
    seen = {axis["name"]: set() for axis in data["axes"]}
    seen_goals = set()
    for s in range(200):
        spec = generate_persona(s, data)
        for name, val in spec["axes"].items():
            seen[name].add(val)
        seen_goals.add(spec["goal"])
    check("every axis value appears across 200 seeds",
          all(seen[axis["name"]] == {v["value"] for v in axis["values"]}
              for axis in data["axes"]))
    check("every goal appears across 200 seeds",
          len(seen_goals) == len(data["goals"]))

    # 4. coverage mode: count, uniqueness, balance, reproducibility
    cov = coverage_personas(12, seed=7, data=data)
    combos = [tuple(sorted(s["axes"].items())) + (s["goal"],) for s in cov]
    check("coverage returns the requested count", len(cov) == 12)
    check("coverage combos are unique", len(set(combos)) == 12)
    balanced = True
    for axis in data["axes"]:
        counts = {v["value"]: 0 for v in axis["values"]}
        for s in cov:
            counts[s["axes"][axis["name"]]] += 1
        floor = 12 // len(axis["values"])
        if any(c < floor - 1 for c in counts.values()):
            balanced = False
    check("coverage batch is near-balanced per axis", balanced)
    check("coverage reproducible from (seed, count)",
          cov == coverage_personas(12, seed=7, data=data))
    check("coverage varies with seed",
          cov != coverage_personas(12, seed=8, data=data))
    check("coverage specs record provenance",
          all(s["sampling"] == {"mode": "coverage", "count": 12, "index": i}
              for i, s in enumerate(cov)))

    # 5. LLM flavor freeze: prose lands in the spec once; axes/goal/
    # tendencies untouched; the saved file replays the stored prose
    calls = []

    def fake_complete(prompt, model):
        calls.append(model)
        return {"name": "Groovy Greta!!", "blurb": "  A test blurb. "}

    flavored = llm_flavor(a, model="fake-model", _complete=fake_complete)
    check("flavored name is slugged", flavored["name"] == "groovy_greta")
    check("flavored blurb frozen into temperament",
          flavored["temperament"] == "A test blurb.")
    check("flavor marks provenance",
          flavored["llm"] == {"model": "fake-model", "flavored": True})
    check("flavor leaves the deterministic core untouched",
          all(flavored[k] == a[k]
              for k in ("seed", "axes", "goal", "tendencies", "sampling")))
    check("flavor is a single generation call", calls == ["fake-model"])
    with tempfile.TemporaryDirectory() as tmp:
        path = os.path.join(tmp, "flavored.yaml")
        with open(path, "w", encoding="utf-8") as f:
            f.write(_dump(flavored))
        reloaded = load_persona(path)
        check("saved flavored spec replays stored prose verbatim",
              reloaded == flavored and calls == ["fake-model"])

    # 6. the axes data file is not offered as a persona
    bundled = list_personas()
    check("axes.yaml hidden from persona listing",
          "axes" not in bundled and "curious_carl" in bundled)

    if failures:
        print(f"selftest: FAILED ({len(failures)}): {', '.join(failures)}")
        return 1
    print("selftest: all checks passed")
    return 0


def main(argv=None) -> int:
    ap = argparse.ArgumentParser(
        description="Generate playtest personas (C2, #649). Prints YAML "
                    "specs conforming to the H1 persona schema; --out "
                    "writes one file per persona for run.py --persona.")
    ap.add_argument("--seed", type=int, default=0,
                    help="base seed (random mode: persona i uses seed+i)")
    ap.add_argument("--count", type=int, default=None,
                    help="how many personas (default 1; coverage default 12)")
    ap.add_argument("--coverage", action="store_true",
                    help="balanced spread across the axis space instead of "
                         "seeded-random sampling")
    ap.add_argument("--llm", action="store_true",
                    help="rewrite name+blurb with a cheap model (frozen "
                         "into the spec; needs an Anthropic key)")
    ap.add_argument("--model", default=DEFAULT_FLAVOR_MODEL,
                    help="flavor model (with --llm)")
    ap.add_argument("--out", metavar="DIR", default=None,
                    help="write <name>.yaml per persona instead of printing")
    ap.add_argument("--selftest", action="store_true",
                    help="offline generator check (no API key)")
    args = ap.parse_args(argv)

    if args.selftest:
        return selftest()

    data = load_axes()
    if args.coverage:
        specs = coverage_personas(args.count or 12, seed=args.seed, data=data)
    else:
        specs = [generate_persona(args.seed + i, data=data)
                 for i in range(args.count or 1)]
    if args.llm:
        specs = [llm_flavor(spec, model=args.model) for spec in specs]

    if args.out:
        os.makedirs(args.out, exist_ok=True)
        used: set = set()
        for spec in specs:
            path = _unique_path(args.out, spec["name"], used)
            with open(path, "w", encoding="utf-8") as f:
                f.write(_dump(spec))
            print(path)
    else:
        for i, spec in enumerate(specs):
            if i:
                print("---")
            print(_dump(spec), end="")
    return 0


if __name__ == "__main__":
    sys.exit(main())
