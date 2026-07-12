#!/usr/bin/env python3
"""Generated-language report (#710) — quality/regression tool for the
native-name generator (`Language.Generated.*`), not a bug-gating probe.

Drives the production Haskell generator directly through the engine's
`--language-report` dispatch mode (`cabal run exe:synarchy --
--language-report --seeds LO:HI`, a pure-computation boot path that
never touches the graphical engine, headless simulation, or world
generation) and reports on/validates its JSON output: profile
diversity, canonical native-name renderings alongside their English
glosses, root collisions, duplicate names, output-length distribution,
and contract (ASCII/length/capitalization/punctuation) violations. No
generation logic is reimplemented here — only inspection of the
Haskell generator's real output.

Usage:
  python3 tools/language_report.py --seeds 0:255
  python3 tools/language_report.py --seeds 0:255 --check

Exit codes: 0 pass, 1 check failure, 2 bad invocation.
"""

import argparse
import json
import math
import re
import subprocess
import sys
from collections import Counter

# One canonical native word: an uppercase ASCII letter, then lowercase
# ASCII letters, with optional internal '-'/''' runs of letters — never
# leading, trailing, or a repeated mark (#710 requirement 6).
CONTRACT_RE = re.compile(r"^[A-Z][a-z]*(?:['-][a-z]+)*$")

REPRESENTATIVE_COUNT = 5
SIGNATURE_RATIO = 240 / 256
DISTINCT_NAME_RATIO = 0.95
WORD64_MAX = 2 ** 64 - 1


def run_report(lo, hi):
    cmd = ["cabal", "run", "-v0", "exe:synarchy", "--",
           "--language-report", "--seeds", f"{lo}:{hi}"]
    try:
        out = subprocess.run(cmd, stdout=subprocess.PIPE, stderr=subprocess.PIPE,
                              check=True)
    except subprocess.CalledProcessError as exc:
        stderr = exc.stderr.decode(errors="replace").strip() if exc.stderr else ""
        print(f"language-report generator failed (exit {exc.returncode}): {stderr}",
              file=sys.stderr)
        sys.exit(2)
    return json.loads(out.stdout)


def contract_violations(name):
    if name is None:
        return ["missing"]
    reasons = []
    if not (3 <= len(name) <= 32):
        reasons.append("length")
    if "--" in name or "''" in name:
        reasons.append("repeated-punctuation")
    if name[:1] in "'-" or name[-1:] in "'-":
        reasons.append("leading-or-trailing-punctuation")
    if not CONTRACT_RE.match(name):
        reasons.append("character-or-capitalization")
    return reasons


def parse_seeds(raw):
    parts = raw.split(":")
    if len(parts) != 2:
        return None
    try:
        lo, hi = int(parts[0]), int(parts[1])
    except ValueError:
        return None
    if lo < 0 or hi < lo or hi > WORD64_MAX:
        return None
    return lo, hi


def main():
    ap = argparse.ArgumentParser(description=__doc__,
                                  formatter_class=argparse.RawDescriptionHelpFormatter)
    ap.add_argument("--seeds", required=True, help="inclusive LO:HI seed range")
    ap.add_argument("--check", action="store_true",
                     help="validate collisions/contract/diversity, exit nonzero on failure")
    args = ap.parse_args()

    seeds_range = parse_seeds(args.seeds)
    if seeds_range is None:
        print(f"invalid --seeds {args.seeds!r}, expected LO:HI with "
              f"0 <= LO <= HI <= {WORD64_MAX}", file=sys.stderr)
        return 2
    lo, hi = seeds_range

    data = run_report(lo, hi)
    seeds = data["seeds"]

    print(f"generator version: {data['generatorVersion']}")
    print(f"concept catalogue: version {data['catalogueVersion']}, "
          f"{data['conceptCount']} concepts")
    print(f"seeds: {lo}..{hi} ({len(seeds)} total)")
    print()

    representative = seeds[:REPRESENTATIVE_COUNT]
    print(f"profile summary ({len(representative)} representative seeds):")
    for s in representative:
        p = s["profile"]
        print(f"  seed {s['seed']}: consonants={p['consonants']} vowels={p['vowels']}")
        print(f"    shapes={','.join(p['syllableShapes'])} "
              f"syllables={p['minSyllables']}-{p['maxSyllables']} "
              f"compoundOrder={p['compoundOrder']} genitiveOrder={p['genitiveOrder']}")
        print(f"    pluralAffix=+{p['pluralAffix']!r} "
              f"possessiveAffix=+{p['possessiveAffix']!r} joinStyle={p['joinStyle']}")
    print()

    print("canonical renderings (representative seeds), native (English gloss):")
    for s in representative:
        print(f"  seed {s['seed']}:")
        for r in s["renderings"]:
            native = r["native"] if r["native"] is not None else f"ERROR({r['nativeError']})"
            gloss = r["gloss"] if r["gloss"] is not None else f"ERROR({r['glossError']})"
            print(f"    {r['form']:<12} {native:<24} ({gloss})")
    print()

    # Root-collision count WITHIN EACH language (requirement 18), not
    # just an aggregate — one line per seed in the sample.
    print(f"root collisions per language ({len(seeds)} seeds):")
    for s in seeds:
        print(f"  seed {s['seed']}: {s['rootCollisions']} collision(s)")
    print()

    # Aggregate stats over every requested seed, not just the
    # representative ones above.
    signatures = set(s["profileSignature"] for s in seeds)
    total_collisions = sum(s["rootCollisions"] for s in seeds)

    all_names = []
    violations = []
    lengths = []
    for s in seeds:
        for r in s["renderings"]:
            name = r["native"]
            all_names.append(name)
            if name is not None:
                lengths.append(len(name))
            reasons = contract_violations(name)
            if reasons:
                violations.append((s["seed"], r["form"], name, reasons))

    name_counts = Counter(n for n in all_names if n is not None)
    distinct_names = set(name_counts)
    total_names = len(all_names)
    distinct_frac = (len(distinct_names) / total_names) if total_names else 0.0
    duplicated = {n: c for n, c in name_counts.items() if c > 1}
    # Extra occurrences beyond each name's first — the count that
    # reconciles with distinct/total above (total - distinct).
    duplicate_name_count = sum(c - 1 for c in duplicated.values())

    print(f"distinct profile signatures: {len(signatures)} / {len(seeds)}")
    print(f"root collisions (post-resolution, summed over all seeds): {total_collisions}")
    print(f"distinct native names: {len(distinct_names)} / {total_names} "
          f"({distinct_frac * 100:.1f}%)")
    print(f"duplicate native names across the sample: {duplicate_name_count} "
          f"({len(duplicated)} distinct string(s) repeated)")
    for name, count in list(duplicated.items())[:20]:
        print(f"  {name!r} appears {count} times")
    if lengths:
        print(f"output length distribution: min={min(lengths)} max={max(lengths)} "
              f"avg={sum(lengths) / len(lengths):.1f}")
    print(f"contract violations: {len(violations)}")
    for (seed, form, name, reasons) in violations[:20]:
        print(f"  seed={seed} form={form} name={name!r} reasons={','.join(reasons)}")

    if not args.check:
        return 0

    print()
    ok = True

    if total_collisions != 0:
        print(f"CHECK FAIL: {total_collisions} root collision(s) remain after resolution",
              file=sys.stderr)
        ok = False

    if violations:
        print(f"CHECK FAIL: {len(violations)} name(s) violate the output contract",
              file=sys.stderr)
        ok = False

    sig_threshold = math.ceil(len(seeds) * SIGNATURE_RATIO)
    if len(signatures) < sig_threshold:
        print(f"CHECK FAIL: only {len(signatures)} distinct profile signatures across "
              f"{len(seeds)} seeds (need >= {sig_threshold})", file=sys.stderr)
        ok = False

    if distinct_frac < DISTINCT_NAME_RATIO:
        print(f"CHECK FAIL: only {distinct_frac * 100:.1f}% of canonical seed/meaning "
              f"combinations produce distinct native names "
              f"(need >= {DISTINCT_NAME_RATIO * 100:.0f}%)", file=sys.stderr)
        ok = False

    if ok:
        print("CHECK OK")
        return 0
    return 1


if __name__ == "__main__":
    sys.exit(main())
