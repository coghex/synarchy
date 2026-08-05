#!/usr/bin/env python3
"""Generated-language report (#710, #1094) — quality/regression tool for
the native-name generator (`Language.Generated.*`), not a bug-gating
probe.

Drives the production Haskell generator directly through the engine's
`--language-report` dispatch mode (`cabal run exe:synarchy --
--language-report --seeds LO:HI`, a pure-computation boot path that
never touches the graphical engine, headless simulation, or world
generation) and reports on/validates its JSON output: profile
diversity, canonical native-name renderings alongside their English
glosses, root collisions, duplicate names, output-length distribution,
contract (ASCII/length/capitalization/punctuation) violations, and
#1094's two-consonant-onset and `y`-role contracts. No generation logic
is reimplemented here — only inspection of the Haskell generator's real
output.

`--check` is the enforced 256-seed quality gate (#1094 requirement 10).
It splits into two kinds of assertion, and the distinction matters:

* Structural gates hold for ANY seed range and are always enforced —
  zero root collisions, zero contract violations, every profile's
  admissible-onset density inside the 25-45% band, every word-initial
  two-consonant onset admissible under that profile's own exported
  relation, no identical-consonant onset, no duplicate name within a
  single language, and the 3-character minimum.
* Pinned gates are REGRESSION PINS measured from the current generator
  at the canonical `--seeds 0:255` sample: exact distinct-signature and
  distinct-name counts, the maximum and average name length, the
  cross-seed onset-diversity ratio, and the presence of all three `y`
  roles. Nothing forbids two independently generated languages from
  coincidentally sharing a short string, so these are pins to be
  updated deliberately alongside a generator change, not invariants.
  They are skipped (loudly) for any other seed range.

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
WORD64_MAX = 2 ** 64 - 1

# --- Structural bounds (enforced for every seed range) ----------------

# #1094 requirement 4's inclusive admissible-pair density band, as
# integer percentages of n*(n-1). Evaluated in integers so the bound is
# exact rather than dependent on a rounded percentage.
DENSITY_LO_PCT = 25
DENSITY_HI_PCT = 45

# #710 requirement 6's floor, structurally guaranteed by
# Language.Generated.Root.ensureMinLength.
MIN_LENGTH_FLOOR = 3

# The architecture's structural maximum: a root is at most 3 syllables
# of a 3-segment shape (9), plus a possessive affix of at most 3, plus
# one join character, plus a second root of at most 9.
STRUCTURAL_MAX_LENGTH = 22

# --- Pinned gates (the canonical 0:255 sample only) -------------------

PINNED_RANGE = (0, 255)
PINNED_VERSION = 2

PIN_DISTINCT_SIGNATURES = 256
PIN_DISTINCT_NAMES = 1280
PIN_MAX_LENGTH = 19
PIN_AVG_LENGTH = 9.7375
AVG_LENGTH_TOLERANCE = 0.5

# Hard floors, kept no weaker than the ratios this checker enforced
# before #1094 tightened it to exact pins.
SIGNATURE_RATIO = 240 / 256
DISTINCT_NAME_RATIO = 0.95

# #1094 requirement 4's cross-seed diversity rule: an ordered pair
# qualifies when BOTH characters are in that profile's consonant
# inventory for at least this many sampled profiles, and at least half
# of the qualifying pairs must be admissible in some sampled profiles
# and inadmissible in others.
SHARED_PAIR_MIN_PROFILES = 8

REQUIRED_Y_ROLES = ("consonant", "vowel", "both")


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


def density_bounds(total):
    """#1094 requirement 4's inclusive admissible-pair count band for an
    ordered-pair total, in integer arithmetic — the same computation
    Language.Generated.Onset.onsetDensityBounds performs."""
    lo = -(-DENSITY_LO_PCT * total // 100)   # ceiling
    hi = DENSITY_HI_PCT * total // 100       # floor
    return lo, max(lo, hi)


def word_initial_onsets(name, profile):
    """Every two-consonant onset in `name` that a `CCV` syllable
    provably produced.

    Roots are flat text with no per-character slot provenance (#1094
    requirement 7), so this is scoped to positions that can only be a
    syllable onset at the start of a rendered root: the name's first two
    glyphs, and the first two glyphs after each '-' join. Interior
    adjacencies come from syllable and compound concatenation, which
    #1094 assigns to L1c, not here.

    A position qualifies only when BOTH glyphs are consonant-capable and
    NEITHER is vowel-capable in this profile. That exclusion is what
    makes the check well defined under a dual-role `y` (requirement 6):
    a `CV` syllable whose vowel slot drew a dual-role `y` renders "by…"
    and a `VC` syllable renders "yz…", both word-initial pairs of
    consonant-capable glyphs the `CCV` path never selected.

    The leading glyph is case-folded first — rendering capitalizes it.
    """
    consonants = set(profile["consonants"])
    vowels = set(profile["vowels"])
    folded = name[:1].lower() + name[1:]
    starts = [0] + [i + 1 for i, ch in enumerate(folded) if ch == "-"]
    found = []
    for i in starts:
        if i + 1 >= len(folded):
            continue
        a, b = folded[i], folded[i + 1]
        if a in consonants and b in consonants and a not in vowels and b not in vowels:
            found.append((i, a, b))
    return found


def main():
    ap = argparse.ArgumentParser(description=__doc__,
                                  formatter_class=argparse.RawDescriptionHelpFormatter)
    ap.add_argument("--seeds", required=True, help="inclusive LO:HI seed range")
    ap.add_argument("--check", action="store_true",
                     help="enforce the #1094 quality gate, exit nonzero on failure")
    args = ap.parse_args()

    seeds_range = parse_seeds(args.seeds)
    if seeds_range is None:
        print(f"invalid --seeds {args.seeds!r}, expected LO:HI with "
              f"0 <= LO <= HI <= {WORD64_MAX}", file=sys.stderr)
        return 2
    lo, hi = seeds_range

    data = run_report(lo, hi)
    seeds = data["seeds"]
    generator_version = data["generatorVersion"]

    print(f"generator version: {generator_version}")
    print(f"concept catalogue: version {data['catalogueVersion']}, "
          f"{data['conceptCount']} concepts")
    print(f"seeds: {lo}..{hi} ({len(seeds)} total)")
    print()

    representative = seeds[:REPRESENTATIVE_COUNT]
    print(f"profile summary ({len(representative)} representative seeds):")
    for s in representative:
        p = s["profile"]
        print(f"  seed {s['seed']}: consonants={p['consonants']} vowels={p['vowels']} "
              f"yRole={p['yRole']}")
        print(f"    shapes={','.join(p['syllableShapes'])} "
              f"syllables={p['minSyllables']}-{p['maxSyllables']} "
              f"compoundOrder={p['compoundOrder']} genitiveOrder={p['genitiveOrder']}")
        print(f"    pluralAffix=+{p['pluralAffix']!r} "
              f"possessiveAffix=+{p['possessiveAffix']!r} joinStyle={p['joinStyle']}")
        pct = (100 * p["onsetAdmissible"] / p["onsetTotal"]) if p["onsetTotal"] else 0.0
        print(f"    admissible onsets={p['onsetAdmissible']}/{p['onsetTotal']} "
              f"({pct:.1f}%): {' '.join(p['onsetPairs'][:12])}"
              f"{' ...' if len(p['onsetPairs']) > 12 else ''}")
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
    name_seeds = {}
    within_seed_duplicates = []
    for s in seeds:
        this_seed = Counter()
        for r in s["renderings"]:
            name = r["native"]
            all_names.append(name)
            if name is not None:
                lengths.append(len(name))
                name_seeds.setdefault(name, []).append(s["seed"])
                this_seed[name] += 1
            reasons = contract_violations(name)
            if reasons:
                violations.append((s["seed"], r["form"], name, reasons))
        for name, count in this_seed.items():
            if count > 1:
                within_seed_duplicates.append((s["seed"], name, count))

    name_counts = Counter(n for n in all_names if n is not None)
    distinct_names = set(name_counts)
    total_names = len(all_names)
    distinct_frac = (len(distinct_names) / total_names) if total_names else 0.0
    duplicated = {n: c for n, c in name_counts.items() if c > 1}
    # Extra occurrences beyond each name's first — the count that
    # reconciles with distinct/total above (total - distinct).
    duplicate_name_count = sum(c - 1 for c in duplicated.values())

    # --- #1094: per-profile onset density and y-role distribution -----
    density_failures = []
    empty_relations = []
    y_role_counts = Counter()
    version_counts = Counter()
    for s in seeds:
        p = s["profile"]
        version_counts[p["version"]] += 1
        y_role_counts[p["yRole"]] += 1
        admissible, total_pairs = p["onsetAdmissible"], p["onsetTotal"]
        # Version 1 deliberately constrains nothing (#1094 requirement
        # 1): its relation is empty and the density band does not apply.
        if p["version"] == 1:
            continue
        if admissible == 0:
            empty_relations.append(s["seed"])
        band_lo, band_hi = density_bounds(total_pairs)
        if not (band_lo <= admissible <= band_hi):
            density_failures.append((s["seed"], admissible, total_pairs,
                                      band_lo, band_hi))

    # --- #1094: word-initial onsets against the exported relation -----
    # Version 2 only: version 1 renders CCV unconstrained by design and
    # its pinned goldens include an identical-consonant onset.
    onsets_checked = 0
    identical_onsets = []
    inadmissible_onsets = []
    for s in seeds:
        p = s["profile"]
        if p["version"] == 1:
            continue
        admissible_set = set(p["onsetPairs"])
        for r in s["renderings"]:
            name = r["native"]
            if name is None:
                continue
            for (pos, a, b) in word_initial_onsets(name, p):
                onsets_checked += 1
                if a == b:
                    identical_onsets.append((s["seed"], r["form"], name, a + b))
                if a + b not in admissible_set:
                    inadmissible_onsets.append((s["seed"], r["form"], name,
                                                 a + b, pos))

    # --- #1094: cross-seed shared-pair diversity ----------------------
    shared_counts = Counter()
    admissible_counts = Counter()
    for s in seeds:
        p = s["profile"]
        if p["version"] == 1:
            continue
        inventory = sorted(set(p["consonants"]))
        admissible_set = set(p["onsetPairs"])
        for a in inventory:
            for b in inventory:
                if a == b:
                    continue
                shared_counts[(a, b)] += 1
                if a + b in admissible_set:
                    admissible_counts[(a, b)] += 1
    qualifying = [pr for pr, c in shared_counts.items()
                  if c >= SHARED_PAIR_MIN_PROFILES]
    disagreeing = [pr for pr in qualifying
                   if 0 < admissible_counts[pr] < shared_counts[pr]]
    diversity_ratio = (len(disagreeing) / len(qualifying)) if qualifying else 0.0

    print(f"distinct profile signatures: {len(signatures)} / {len(seeds)}")
    print(f"root collisions (post-resolution, summed over all seeds): {total_collisions}")
    print(f"distinct native names: {len(distinct_names)} / {total_names} "
          f"({distinct_frac * 100:.1f}%)")
    print(f"duplicate native names across the sample: {duplicate_name_count} "
          f"({len(duplicated)} distinct string(s) repeated)")
    for name, count in list(duplicated.items())[:20]:
        print(f"  {name!r} appears {count} times, seeds "
              f"{','.join(str(x) for x in name_seeds[name])}")
    print(f"duplicate native names within a single language: "
          f"{len(within_seed_duplicates)}")
    for (seed, name, count) in within_seed_duplicates[:20]:
        print(f"  seed={seed} {name!r} appears {count} times")
    if lengths:
        print(f"output length distribution: min={min(lengths)} max={max(lengths)} "
              f"avg={sum(lengths) / len(lengths):.4f}")
    print(f"contract violations: {len(violations)}")
    for (seed, form, name, reasons) in violations[:20]:
        print(f"  seed={seed} form={form} name={name!r} reasons={','.join(reasons)}")
    print(f"profiles by generator version: "
          f"{', '.join(f'v{v}={c}' for v, c in sorted(version_counts.items()))}")
    print(f"y roles: {', '.join(f'{k}={y_role_counts[k]}' for k in REQUIRED_Y_ROLES)}"
          f"{', none=%d' % y_role_counts['none'] if y_role_counts['none'] else ''}")
    print(f"admissible-onset density violations "
          f"({DENSITY_LO_PCT}-{DENSITY_HI_PCT}% band): {len(density_failures)}")
    for (seed, k, total_pairs, band_lo, band_hi) in density_failures[:20]:
        print(f"  seed={seed} admissible={k} of {total_pairs} "
              f"(need {band_lo}..{band_hi})")
    print(f"word-initial two-consonant onsets checked: {onsets_checked} "
          f"(identical={len(identical_onsets)}, "
          f"inadmissible={len(inadmissible_onsets)})")
    for (seed, form, name, pair) in identical_onsets[:20]:
        print(f"  IDENTICAL seed={seed} form={form} name={name!r} onset={pair!r}")
    for (seed, form, name, pair, pos) in inadmissible_onsets[:20]:
        print(f"  INADMISSIBLE seed={seed} form={form} name={name!r} "
              f"onset={pair!r} at {pos}")
    print(f"shared ordered pairs (in >= {SHARED_PAIR_MIN_PROFILES} profiles' "
          f"inventories): {len(qualifying)}, disagreeing across profiles: "
          f"{len(disagreeing)} ({diversity_ratio * 100:.1f}%)")

    if not args.check:
        return 0

    print()
    ok = True

    def fail(msg):
        nonlocal ok
        print(f"CHECK FAIL: {msg}", file=sys.stderr)
        ok = False

    # --- Structural gates: enforced for every seed range --------------

    if total_collisions != 0:
        fail(f"{total_collisions} root collision(s) remain after resolution")

    if violations:
        fail(f"{len(violations)} name(s) violate the output contract")

    if within_seed_duplicates:
        fail(f"{len(within_seed_duplicates)} canonical name(s) duplicated within a "
             f"single language (roots are unique per language, so this cannot "
             f"be coincidence)")

    if empty_relations:
        fail(f"{len(empty_relations)} version-2 profile(s) have an empty "
             f"admissible-onset relation (first: seed {empty_relations[0]})")

    if density_failures:
        fail(f"{len(density_failures)} version-2 profile(s) fall outside the "
             f"{DENSITY_LO_PCT}-{DENSITY_HI_PCT}% admissible-onset density band")

    if identical_onsets:
        fail(f"{len(identical_onsets)} identical-consonant word-initial onset(s)")

    if inadmissible_onsets:
        fail(f"{len(inadmissible_onsets)} word-initial onset(s) rejected by their "
             f"own profile's exported admissibility relation")

    if lengths:
        if min(lengths) < MIN_LENGTH_FLOOR:
            fail(f"minimum canonical name length {min(lengths)} is below the "
                 f"{MIN_LENGTH_FLOOR}-character floor")
        if max(lengths) > STRUCTURAL_MAX_LENGTH:
            fail(f"maximum canonical name length {max(lengths)} exceeds the "
                 f"architecture's structural maximum of {STRUCTURAL_MAX_LENGTH}")
    else:
        fail("no canonical names were rendered")

    mismatched_versions = [v for v in version_counts if v != generator_version]
    if mismatched_versions:
        fail(f"profiles built at version(s) {mismatched_versions} while the report "
             f"header claims version {generator_version}")

    # --- Pinned gates: the canonical 0:255 sample only ----------------

    # The pins are measured at one seed range and one generator version.
    # A different RANGE legitimately skips them; a different VERSION at
    # the canonical range does not — that is the generator regressing
    # under the very command the acceptance criteria run, and silently
    # skipping the pins would report CHECK OK for it.
    if (lo, hi) != PINNED_RANGE:
        print(f"pinned regression gates SKIPPED: they are measured for "
              f"--seeds {PINNED_RANGE[0]}:{PINNED_RANGE[1]}, this run is "
              f"--seeds {lo}:{hi}")
    elif generator_version != PINNED_VERSION:
        fail(f"the canonical --seeds {PINNED_RANGE[0]}:{PINNED_RANGE[1]} sample "
             f"reports generator version {generator_version}, but the pins are "
             f"measured at version {PINNED_VERSION}")
    else:
        sig_floor = math.ceil(len(seeds) * SIGNATURE_RATIO)
        if len(signatures) < sig_floor:
            fail(f"only {len(signatures)} distinct profile signatures across "
                 f"{len(seeds)} seeds (floor {sig_floor})")
        elif len(signatures) != PIN_DISTINCT_SIGNATURES:
            fail(f"distinct profile signatures {len(signatures)} != pinned "
                 f"{PIN_DISTINCT_SIGNATURES}")

        name_floor = math.ceil(total_names * DISTINCT_NAME_RATIO)
        if len(distinct_names) < name_floor:
            fail(f"only {len(distinct_names)} distinct native names out of "
                 f"{total_names} (floor {name_floor})")
        elif len(distinct_names) != PIN_DISTINCT_NAMES:
            fail(f"distinct native names {len(distinct_names)} != pinned "
                 f"{PIN_DISTINCT_NAMES}")

        if max(lengths) != PIN_MAX_LENGTH:
            fail(f"maximum canonical name length {max(lengths)} != pinned "
                 f"{PIN_MAX_LENGTH}")

        avg_length = sum(lengths) / len(lengths)
        if abs(avg_length - PIN_AVG_LENGTH) > AVG_LENGTH_TOLERANCE:
            fail(f"average canonical name length {avg_length:.4f} is more than "
                 f"{AVG_LENGTH_TOLERANCE} from the pinned {PIN_AVG_LENGTH}")

        if not qualifying:
            fail(f"no ordered pair appears in at least "
                 f"{SHARED_PAIR_MIN_PROFILES} profiles' consonant inventories, so "
                 f"the cross-seed diversity rule cannot be evaluated")
        elif 2 * len(disagreeing) < len(qualifying):
            fail(f"only {len(disagreeing)} of {len(qualifying)} shared ordered "
                 f"pairs disagree across profiles (need at least half)")

        missing_roles = [r for r in REQUIRED_Y_ROLES if y_role_counts[r] == 0]
        if missing_roles:
            fail(f"no profile assigns 'y' the role(s) {', '.join(missing_roles)}")

    if ok:
        print("CHECK OK")
        return 0
    return 1


if __name__ == "__main__":
    sys.exit(main())
