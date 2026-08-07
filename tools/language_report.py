#!/usr/bin/env python3
"""Generated-language report (#710, #1094, #1095, #1096, #1100) —
quality/regression tool for the native-name generator
(`Language.Generated.*`), not a bug-gating probe.

Drives the production Haskell generator directly through the engine's
`--language-report` dispatch mode (`cabal run exe:synarchy --
--language-report --seeds LO:HI`, a pure-computation boot path that
never touches the graphical engine, headless simulation, or world
generation) and reports on/validates its JSON output: profile
diversity, canonical native-name renderings alongside their English
glosses, root collisions, duplicate names, output-length distribution,
contract (repertoire/length/capitalization/punctuation) violations,
#1094's two-consonant-onset and `y`-role contracts, #1095's
triple-letter-run guarantee and per-language boundary phonology, #1096's
bound morphemes, and #1100's per-language extended orthography. No
generation logic is reimplemented here — only inspection of the Haskell
generator's real output.

#1100's output repertoire is the one thing this tool holds an
independent copy of, in `EXTENDED_LOWER`/`EXTENDED_UPPER` below. That is
deliberate: the contract regex is the ENFORCED statement of what a name
may contain, so deriving it from the generator's own claim would make it
follow any widening automatically and enforce nothing. The generator
emits its `outputInventory` in the report header and `--check` fails
when the two disagree, so a repertoire change has to be made in both
places — here, where it is reviewed as a contract, and there, where it
is generated — and neither side can drift alone.

That last clause is why #1096's admissibility verdict arrives as a
per-record `admissible` BOOLEAN computed by the Haskell side rather than
being recomputed here: the admissibility relation is generation logic.
The prefix rule and both collision totals ARE directly checkable from
the exposed strings and counts, so only that one signal crosses as a
verdict.

`--check` is the enforced 256-seed quality gate (#1094 requirement 10,
#1095 and #1096 acceptance). It splits into two kinds of assertion, and
the distinction matters:

* Structural gates hold for ANY seed range and are always enforced —
  zero root collisions, zero contract violations, zero triple-letter
  runs, every profile's admissible-onset density inside the 25-45% band,
  every word-initial two-consonant onset admissible under that profile's
  own exported relation, no identical-consonant onset, every
  boundary-phonology-era profile declaring a real boundary rule, no
  duplicate name within a single language, the 3-character minimum,
  #1096's bound-form rules (at most eight per language, every stored
  form a nonempty strictly-shorter prefix retaining a visible letter,
  zero inadmissible forms, zero bound-related collisions, at least one
  visible free-to-bound shortening across the sample, and no bound form
  at all below the version that introduced them), and #1100's
  orthography rules (the generator's declared output repertoire equals
  this tool's own, every extended character in a rendered name belongs
  to the inventory of the language that rendered it, and no extended
  character at all below the version that introduced them).
* Pinned gates are REGRESSION PINS measured from the current generator
  at the canonical `--seeds 0:255` sample: exact distinct-signature,
  total-name and distinct-name counts, the maximum and average name
  length, the cross-seed onset-diversity ratio, the presence of all
  three `y` roles, the presence of both compound and both genitive
  orderings, and #1100's requirement that the sample contain languages
  WITH extended orthography and languages without. Nothing forbids two
  independently generated languages from coincidentally sharing a short
  string, so these are pins to be updated deliberately alongside a
  generator change, not invariants. They are skipped (loudly) for any
  other seed range.

#1096's bound-slot renderings are accumulated SEPARATELY from the
canonical `renderings` array and never enter the distinct-name,
profile-signature, or pinned length-distribution populations. Those
gates are ratios and exact counts measured against the canonical
sample; folding tens of thousands of extra names into their
denominators would let a real regression hide behind added volume. The
new names are still subject to every zero-gated structural check
(output contract, 3-32 length, triple runs, word-initial onsets), where
extra population can only ever find more defects, never mask one.

Doubled letters are REPORTED, never gated. #1095 requires them to remain
legal at a comparable rate rather than to hit a threshold, and the
enforceable form of that lives in the hspec suite as a fixture whose
in-morpheme double survives every join — a population percentage here
would be an arbitrary number nobody could justify moving.

Usage:
  python3 tools/language_report.py --seeds 0:255
  python3 tools/language_report.py --seeds 0:255 --check
  python3 tools/language_report.py --self-test

`--self-test` boots no generator: it exercises the detectors themselves
against known-good and known-bad strings, so "zero triple-letter runs"
is evidence that the gate FIRES on a triple rather than evidence that it
cannot see one.

Exit codes: 0 pass, 1 check failure, 2 bad invocation.
"""

import argparse
import json
import math
import re
import subprocess
import sys
from collections import Counter

# --- #1100: the output repertoire ------------------------------------
#
# This tool's INDEPENDENT copy of
# Language.Generated.Orthography.extendedLetterTable's two case columns,
# ascending by code point. Written as escapes so a review reads code
# points rather than trusting a terminal font, with the glyphs alongside.
# `--check` compares the assembled inventory against the generator's own
# `outputInventory`, so these cannot silently fall behind it.
EXTENDED_LOWER = (
    "\u00E0\u00E1\u00E2\u00E4\u00E5\u00E7\u00E8\u00E9"  # à á â ä å ç è é
    "\u00EA\u00EB\u00EC\u00ED\u00EE\u00EF\u00F2\u00F3"  # ê ë ì í î ï ò ó
    "\u00F4\u00F6\u00F8\u00F9\u00FA\u00FB\u00FC\u0101"  # ô ö ø ù ú û ü ā
    "\u0103\u0105\u0107\u0109\u010D\u010F\u0111\u0113"  # ă ą ć ĉ č ď đ ē
    "\u0119\u011B\u011D\u011F\u0125\u012B\u0135\u013A"  # ę ě ĝ ğ ĥ ī ĵ ĺ
    "\u013E\u0142\u0144\u0148\u014D\u0151\u0155\u0159"  # ľ ł ń ň ō ő ŕ ř
    "\u015B\u015D\u015F\u0161\u0163\u0165\u016B\u016D"  # ś ŝ ş š ţ ť ū ŭ
    "\u016F\u0171\u0175\u017A\u017E"                    # ů ű ŵ ź ž
)
EXTENDED_UPPER = (
    "\u00C0\u00C1\u00C2\u00C4\u00C5\u00C7\u00C8\u00C9"  # À Á Â Ä Å Ç È É
    "\u00CA\u00CB\u00CC\u00CD\u00CE\u00CF\u00D2\u00D3"  # Ê Ë Ì Í Î Ï Ò Ó
    "\u00D4\u00D6\u00D8\u00D9\u00DA\u00DB\u00DC\u0100"  # Ô Ö Ø Ù Ú Û Ü Ā
    "\u0102\u0104\u0106\u0108\u010C\u010E\u0110\u0112"  # Ă Ą Ć Ĉ Č Ď Đ Ē
    "\u0118\u011A\u011C\u011E\u0124\u012A\u0134\u0139"  # Ę Ě Ĝ Ğ Ĥ Ī Ĵ Ĺ
    "\u013D\u0141\u0143\u0147\u014C\u0150\u0154\u0158"  # Ľ Ł Ń Ň Ō Ő Ŕ Ř
    "\u015A\u015C\u015E\u0160\u0162\u0164\u016A\u016C"  # Ś Ŝ Ş Š Ţ Ť Ū Ŭ
    "\u016E\u0170\u0174\u0179\u017D"                    # Ů Ű Ŵ Ź Ž
)

# The two non-letter characters a name may contain: the possessive
# apostrophe and the JoinHyphen separator, both pre-existing. #1100
# requirement 8 permits more only with an orthographic justification and
# adjacency rules; none was added.
NAME_MARKS = "'-"

LOWER_LETTERS = "abcdefghijklmnopqrstuvwxyz" + EXTENDED_LOWER
UPPER_LETTERS = "ABCDEFGHIJKLMNOPQRSTUVWXYZ" + EXTENDED_UPPER
LETTERS = frozenset(LOWER_LETTERS + UPPER_LETTERS)
EXTENDED_SET = frozenset(EXTENDED_LOWER + EXTENDED_UPPER)

# The whole ASCII alphabet is admitted even though `q` and `x` are in
# neither phoneme pool: this is the historical contract's letter class,
# and narrowing it would tighten the contract for no gain.
OUTPUT_INVENTORY = "".join(sorted(LOWER_LETTERS + UPPER_LETTERS + NAME_MARKS))

# One canonical native word: an uppercase letter, then lowercase letters,
# with optional internal '-'/''' runs of letters — never leading,
# trailing, or a repeated mark (#710 requirement 6, repertoire widened by
# #1100). Built from the repertoire above so the regex and the
# cross-check can never describe different character sets.
CONTRACT_RE = re.compile(
    r"^[{u}][{l}]*(?:['-][{l}]+)*$".format(
        u=re.escape(UPPER_LETTERS), l=re.escape(LOWER_LETTERS)))

# #1100: the first generator version whose languages draw extended
# letters. Below it a language is pure ASCII and must stay that way, or a
# historical world's names would re-render.
EXTENDED_ORTHOGRAPHY_VERSION = 5

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
# of a 3-segment shape (9) plus one #1095 boundary segment at each of its
# 2 syllable joins (11); an affixed root adds one boundary segment and a
# possessive affix of at most 3 (15); a compound adds one join character
# and a second root (27). Well inside the 3-32 output contract, so
# epenthesis can never push the tail past the ceiling.
STRUCTURAL_MAX_LENGTH = 27

# #1096 requirement 2's per-language cap on bound morphemes.
MAX_BOUND_FORMS = 8

# #1096: the first generator version whose languages form bound
# morphemes. Below it a language has none at all and every dependent
# slot renders with the free form, which is what keeps versions 1-3's
# pinned goldens byte-identical.
BOUND_FORM_VERSION = 4

# --- Pinned gates (the canonical 0:255 sample only) -------------------

PINNED_RANGE = (0, 255)
PINNED_VERSION = 5

PIN_DISTINCT_SIGNATURES = 256
PIN_TOTAL_NAMES = 1280
PIN_DISTINCT_NAMES = 1280
PIN_MAX_LENGTH = 22
# Measured at generator version 5. The version-4 figure was 9.7969 and
# the version-3 one 9.9422. Observational — the hard gate is the 3-32
# output contract.
PIN_AVG_LENGTH = 9.7234
AVG_LENGTH_TOLERANCE = 0.5

# #1100 acceptance: "some languages draw extended characters and others
# draw none". Floors rather than exact counts — the population split is
# a property of the draw, and pinning it exactly would turn an unrelated
# generator tweak into a failure with nothing wrong behind it.
PIN_MIN_MARKED_LANGUAGES = 100
PIN_MIN_PLAIN_LANGUAGES = 20

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

# #1095: the first generator version whose profiles mediate morpheme
# boundaries. Versions below it join raw and are frozen that way, so
# "unmediated" is correct for them and a defect at or above it.
BOUNDARY_PHONOLOGY_VERSION = 3
UNMEDIATED_BOUNDARY = "unmediated"


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


def letter_runs(name, length):
    """Every run of `length` contiguous letters in `name` that are the
    same letter ignoring case (#1095 requirement 3).

    Case is folded because rendering capitalizes the first letter last,
    so `Aaa` is a triple; punctuation is not a letter, so a hyphen join's
    `a-a` and an apostrophe affix's `h'h` interrupt a run rather than
    forming one.

    "Letter" spans #1100's extended repertoire, matching
    Language.Generated.Boundary.hasTripleRun: `ááá` is exactly as much a
    triple as `aaa`, and the old `isascii()` guard would have skipped
    straight past it. Distinct letters never form a run whatever their
    marks — `á` and `a` are different code points and different
    phonemes — so `aáa` is not one.
    """
    folded = name.lower()
    return [folded[i:i + length]
            for i in range(len(folded) - length + 1)
            if folded[i] in LETTERS and len(set(folded[i:i + length])) == 1]


def foreign_extended_chars(name, profile):
    """Every extended character in `name` that does NOT belong to the
    inventory of the language that rendered it (#1100 requirement 1).

    This is the check that separates a convention from decoration. An
    accent is only a signature if it came out of the language's own
    phoneme inventory; a mark applied to finished output would show up
    here as a character the profile never held. Compared case-folded,
    because rendering capitalizes the initial and inventories are
    lowercase.
    """
    own = set(profile["consonants"]) | set(profile["vowels"])
    return sorted({c for c in name
                   if c in EXTENDED_SET and c.lower() not in own})


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


def bound_form_violations(free, bound):
    """Why a stored bound form breaks #1096 requirement 3's shape rules,
    or [] when it does not.

    "Differs from the free form only by deleting terminal characters" is
    exactly "nonempty strict prefix, strictly shorter", so that is what
    is checked rather than a separate edit-distance notion. Admissibility
    is NOT checked here — it is generation logic, and arrives as the
    Haskell-computed `admissible` flag instead.
    """
    if not isinstance(free, str) or not isinstance(bound, str):
        return ["missing"]
    reasons = []
    if not bound:
        reasons.append("empty")
    if len(bound) >= len(free):
        reasons.append("not-shorter")
    if not free.startswith(bound):
        reasons.append("not-a-prefix")
    if not any(c in LETTERS for c in bound):
        reasons.append("no-visible-letter")
    return reasons


def self_test():
    """Prove the detectors this tool gates on actually fire. Runs no
    generator, so it stays a fast, dependency-free check that a green
    `--check` means "no defect found", not "no defect detectable"."""
    failures = []

    def expect(label, got, want):
        if got != want:
            failures.append(f"{label}: got {got!r}, want {want!r}")

    # #1095 triples: found case-insensitively, interrupted by punctuation.
    for name in ("aaa", "Aaa", "aAa", "kaaan", "Zoccce", "wwwi"):
        expect(f"triple in {name!r}", bool(letter_runs(name, 3)), True)
    for name in ("a-aa", "aa-a", "h'hh", "abba", "Kobbha", "ab", ""):
        expect(f"triple in {name!r}", bool(letter_runs(name, 3)), False)
    # A quadruple contains two overlapping triples; the count is what the
    # gate reports, so pin it rather than only its truthiness.
    expect("runs in 'aaaa'", len(letter_runs("aaaa", 3)), 2)
    # Doubles are reported, never gated — but the same helper finds them.
    for name in ("abba", "Kobbha", "a-aa"):
        expect(f"double in {name!r}", bool(letter_runs(name, 2)), True)
    expect("double in 'Kaved'", bool(letter_runs("Kaved", 2)), False)

    # The pre-existing output contract, so a rewrite here cannot quietly
    # stop rejecting what it always rejected.
    expect("contract 'Kara'", contract_violations("Kara"), [])
    expect("contract \"Kara'b\"", contract_violations("Kara'b"), [])
    expect("contract 'Kara-bo'", contract_violations("Kara-bo"), [])
    for bad in ("kara", "Ka", "Kara--bo", "-Kara", "Kara-", "Kar3"):
        if not contract_violations(bad):
            failures.append(f"contract {bad!r}: accepted, want rejected")
    expect("contract None", contract_violations(None), ["missing"])

    # The #1094 density band, in the integer arithmetic the Haskell side
    # uses (Language.Generated.Onset.onsetDensityBounds).
    expect("density_bounds(30)", density_bounds(30), (8, 13))
    expect("density_bounds(132)", density_bounds(132), (33, 59))

    # #1096's bound-form shape rules, so "zero prefix violations" is
    # evidence the detector FIRES on a bad form rather than evidence it
    # cannot see one.
    expect("bound 'kara'/'kar'", bound_form_violations("kara", "kar"), [])
    expect("bound 'kara'/'k'", bound_form_violations("kara", "k"), [])
    expect("bound 'kara'/''", bound_form_violations("kara", ""),
           ["empty", "no-visible-letter"])
    expect("bound 'kara'/'kara'", bound_form_violations("kara", "kara"),
           ["not-shorter"])
    expect("bound 'kara'/'karas'", bound_form_violations("kara", "karas"),
           ["not-shorter", "not-a-prefix"])
    # Stem substitution and internal deletion are exactly what the
    # strict-prefix rule exists to exclude.
    expect("bound 'kara'/'kor'", bound_form_violations("kara", "kor"),
           ["not-a-prefix"])
    expect("bound 'karad'/'krd'", bound_form_violations("karad", "krd"),
           ["not-a-prefix"])
    expect("bound 'kara'/'-'", bound_form_violations("kara", "-"),
           ["not-a-prefix", "no-visible-letter"])
    expect("bound None", bound_form_violations("kara", None), ["missing"])

    # --- #1100: the widened repertoire --------------------------------

    # Every literal below is written as escapes. Accented text in a
    # source file has two spellings that look identical, and one of the
    # cases here exists precisely to tell them apart.

    # The assembled inventory must be exactly what the contract regex's
    # two classes plus the marks describe, or a character could be
    # admitted by one and not the other.
    expect("inventory size", len(OUTPUT_INVENTORY), 26 * 2 + 61 * 2 + 2)
    expect("inventory is sorted and unique",
           OUTPUT_INVENTORY, "".join(sorted(set(OUTPUT_INVENTORY))))
    expect("extended cases pair up", len(EXTENDED_LOWER), len(EXTENDED_UPPER))
    # Each lowercase member uppercases to the member at the same index,
    # which is what makes a name starting with one capitalizable at all.
    for lo_c, up_c in zip(EXTENDED_LOWER, EXTENDED_UPPER):
        expect("upper(U+%04X)" % ord(lo_c), lo_c.upper(), up_c)
        expect("lower(U+%04X)" % ord(up_c), up_c.lower(), lo_c)

    # The contract over extended names: a marked initial and marked
    # interior letters are accepted, and the pre-existing structural
    # rules still hold around them.
    expect("contract 'K\u00E1r\u00F3'",
           contract_violations("K\u00E1r\u00F3"), [])
    expect("contract '\u00C1r\u00F3-b\u00E1'",
           contract_violations("\u00C1r\u00F3-b\u00E1"), [])
    expect("contract 'K\u00E1ra\u2019b'",   # curly quote is not the mark
           contract_violations("K\u00E1ra\u2019b"),
           ["character-or-capitalization"])
    # A combining sequence is deliberately NOT the same thing as a
    # precomposed letter: #1100 restricts the repertoire to single code
    # points, so "A" + U+0301 is rejected even though it renders
    # identically to the accepted U+00C1.
    expect("contract combining A+U+0301",
           contract_violations("A\u0301ra\u0301"),
           ["character-or-capitalization"])
    for label, bad in (("lowercase extended initial", "\u00E1ra"),
                       ("uppercase in the interior",  "K\u00C1ra"),
                       ("letter outside the repertoire", "Kar\u00E6"),
                       ("trailing mark",  "K\u00E1-"),
                       ("leading mark",   "-K\u00E1ra"),
                       # Two DIFFERENT marks side by side. The "''"/"--"
                       # substring test above cannot see these, so they
                       # rest entirely on the regex — and are mirrored in
                       # the Hspec predicate's own table for the same
                       # reason.
                       ("hyphen then apostrophe", "K-'ara"),
                       ("apostrophe then hyphen", "K'-ara")):
        if not contract_violations(bad):
            failures.append(f"contract {label} {bad!r}: accepted, "
                            f"want rejected")

    # A triple of an extended letter is a triple; a marked and unmarked
    # pair of the same base are DIFFERENT letters and form no run.
    expect("triple U+00E1 x3", bool(letter_runs("\u00E1\u00E1\u00E1", 3)), True)
    expect("triple U+00C1+U+00E1 x2",
           bool(letter_runs("\u00C1\u00E1\u00E1", 3)), True)
    expect("triple a-U+00E1-a", bool(letter_runs("a\u00E1a", 3)), False)
    expect("double U+00E1 x2", bool(letter_runs("\u00E1\u00E1", 2)), True)
    expect("double a+U+00E1", bool(letter_runs("a\u00E1", 2)), False)

    # An extended letter IS a visible letter, so a bound form made of
    # one has no shape violation.
    expect("bound 'k\u0105ra'/'k\u0105'",
           bound_form_violations("k\u0105ra", "k\u0105"), [])
    expect("bound '\u0105ra'/'\u0105'",
           bound_form_violations("\u0105ra", "\u0105"), [])

    # The membership detector, so "zero foreign extended characters" is
    # evidence the gate FIRES on a sprinkled accent rather than evidence
    # it cannot see one.
    marked = {"consonants": "kr", "vowels": "a\u00E1"}
    plain = {"consonants": "kr", "vowels": "a"}
    expect("own mark in its own language",
           foreign_extended_chars("K\u00E1ra", marked), [])
    expect("own mark as the initial",
           foreign_extended_chars("\u00C1ra", marked), [])
    expect("foreign mark in a plain language",
           foreign_extended_chars("K\u00E1ra", plain), ["\u00E1"])
    expect("foreign mark as the initial",
           foreign_extended_chars("\u00C1ra", plain), ["\u00C1"])
    expect("an ascii name is never foreign",
           foreign_extended_chars("Kara", plain), [])

    for f in failures:
        print(f"SELF-TEST FAIL: {f}", file=sys.stderr)
    if failures:
        return 1
    print("SELF-TEST OK")
    return 0


def main():
    ap = argparse.ArgumentParser(description=__doc__,
                                  formatter_class=argparse.RawDescriptionHelpFormatter)
    ap.add_argument("--seeds", help="inclusive LO:HI seed range")
    ap.add_argument("--check", action="store_true",
                     help="enforce the #1094/#1095 quality gate, exit nonzero on failure")
    ap.add_argument("--self-test", action="store_true",
                     help="check this tool's own detectors and exit; boots no generator")
    args = ap.parse_args()

    if args.self_test:
        return self_test()
    if args.seeds is None:
        print("--seeds LO:HI is required (or --self-test)", file=sys.stderr)
        return 2

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
        print(f"    boundaryRule={p['boundaryRule']} "
              f"segments={p['boundarySegments']!r}")
        print(f"    diacritic={p['diacritic']} "
              f"extendedChars={p['extendedChars']!r}")
        bfs = s["boundForms"]
        print(f"    bound forms ({len(bfs)}/{MAX_BOUND_FORMS}): "
              + (", ".join(f"{b['concept']} {b['free']}->{b['bound']}"
                            for b in bfs) or "none"))
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

    print("bound-slot renderings (representative seeds), "
          "the same concept bare and in each dependent slot:")
    for s in representative:
        print(f"  seed {s['seed']}:")
        by_concept = {}
        for r in s["boundRenderings"]:
            by_concept.setdefault(r["concept"], []).append(r)
        for concept, rows in list(by_concept.items())[:3]:
            rendered = "  ".join(
                f"{r['slot']}={r['native']}" + ("*" if r["shortened"] else "")
                for r in rows)
            print(f"    {concept:<12} {rendered}")
    print("  (* = the completed name is visibly different from the same "
          "expression rendered with free forms only)")
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
    triple_runs = []
    names_with_double = 0
    # --- #1100: per-language orthography ------------------------------
    # An extended character is only a convention if it came out of the
    # inventory of the language that rendered it, so membership is
    # accumulated over BOTH name populations (canonical here, bound-slot
    # below) — the check is zero-gated, so extra population can only
    # find more defects.
    foreign_extended = []
    extended_before_version = []
    marked_languages = 0
    plain_languages = 0
    diacritic_counts = Counter()
    names_with_extended = 0

    for s in seeds:
        this_seed = Counter()
        p = s["profile"]
        if p["extendedChars"]:
            marked_languages += 1
        else:
            plain_languages += 1
        diacritic_counts[p["diacritic"]] += 1
        # A version predating extended orthography must have none at
        # all: that is what keeps its pinned goldens byte-identical.
        if p["version"] < EXTENDED_ORTHOGRAPHY_VERSION and p["extendedChars"]:
            extended_before_version.append(
                (s["seed"], p["version"], p["extendedChars"]))
        for r in s["renderings"]:
            name = r["native"]
            all_names.append(name)
            if name is not None:
                lengths.append(len(name))
                name_seeds.setdefault(name, []).append(s["seed"])
                this_seed[name] += 1
                for run in letter_runs(name, 3):
                    triple_runs.append((s["seed"], r["form"], name, run))
                if letter_runs(name, 2):
                    names_with_double += 1
                if any(c in EXTENDED_SET for c in name):
                    names_with_extended += 1
                foreign = foreign_extended_chars(name, p)
                if foreign:
                    foreign_extended.append(
                        (s["seed"], r["form"], name, foreign))
            reasons = contract_violations(name)
            if reasons:
                violations.append((s["seed"], r["form"], name, reasons))
        for name, count in this_seed.items():
            if count > 1:
                within_seed_duplicates.append((s["seed"], name, count))

    # --- #1096: bound morphemes ---------------------------------------
    # Accumulated in their OWN containers. The canonical arrays above
    # feed ratio and exact-count pins; these feed zero-gated structural
    # checks only, so the two populations must not be mixed.
    bound_over_cap = []
    bound_shape_failures = []
    bound_inadmissible = []
    bound_before_version = []
    bound_collisions_total = 0
    bound_form_total = 0
    bound_shortenings = 0
    bound_violations = []
    bound_triple_runs = []
    bare_slot_mismatches = []
    bound_lengths = []
    for s in seeds:
        p = s["profile"]
        forms = s["boundForms"]
        bound_form_total += len(forms)
        bound_collisions_total += s["boundCollisions"]
        if len(forms) > MAX_BOUND_FORMS:
            bound_over_cap.append((s["seed"], len(forms)))
        # A version predating bound morphology must have none at all:
        # that is what keeps its pinned goldens byte-identical.
        if p["version"] < BOUND_FORM_VERSION and forms:
            bound_before_version.append((s["seed"], p["version"], len(forms)))
        free_of = {}
        for b in forms:
            free_of[b["concept"]] = b["free"]
            reasons = bound_form_violations(b["free"], b["bound"])
            if reasons:
                bound_shape_failures.append(
                    (s["seed"], b["concept"], b["free"], b["bound"], reasons))
            if not b["admissible"]:
                bound_inadmissible.append(
                    (s["seed"], b["concept"], b["bound"]))
        for r in s["boundRenderings"]:
            name = r["native"]
            if r["shortened"]:
                bound_shortenings += 1
            reasons = contract_violations(name)
            if reasons:
                bound_violations.append(
                    (s["seed"], r["concept"], r["slot"], name, reasons))
            if name is None:
                continue
            bound_lengths.append(len(name))
            for run in letter_runs(name, 3):
                bound_triple_runs.append(
                    (s["seed"], r["concept"], r["slot"], name, run))
            foreign = foreign_extended_chars(name, p)
            if foreign:
                foreign_extended.append(
                    (s["seed"], f"{r['concept']}/{r['slot']}", name, foreign))
            # Requirement 6's first row: Bare has no dependent slot, so
            # it is always the free form. Compared case-insensitively
            # because rendering capitalizes the initial — that is the
            # only difference this tool asserts, and it reimplements no
            # generation logic to do it.
            if r["slot"] == "bare":
                free = free_of.get(r["concept"])
                if free is not None and name.lower() != free.lower():
                    bare_slot_mismatches.append(
                        (s["seed"], r["concept"], name, free))

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
    boundary_rule_counts = Counter()
    # #1096 requirement 6: profile-specific compound/genitive ordering
    # changes only the final display order, never which slot is
    # dependent — so the sample has to actually contain both directions
    # for that claim to have been exercised at all.
    compound_orders = Counter()
    genitive_orders = Counter()
    unmediated_profiles = []
    for s in seeds:
        p = s["profile"]
        version_counts[p["version"]] += 1
        y_role_counts[p["yRole"]] += 1
        boundary_rule_counts[p["boundaryRule"]] += 1
        compound_orders[p["compoundOrder"]] += 1
        genitive_orders[p["genitiveOrder"]] += 1
        # #1095: a version that has boundary phonology must actually
        # carry a policy. An "unmediated" profile there would silently
        # render every join raw while every other gate still passed.
        if (p["version"] >= BOUNDARY_PHONOLOGY_VERSION
                and p["boundaryRule"] == UNMEDIATED_BOUNDARY):
            unmediated_profiles.append(s["seed"])
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
    # Version 2 onward: version 1 renders CCV unconstrained by design and
    # its pinned goldens include an identical-consonant onset.
    #
    # #1096's bound-slot names are included here. Both gates below are
    # zero-gated rather than ratios, so extra population can only find
    # more defects, and the property genuinely must hold for them: a
    # bound form is a PREFIX of its free root, so a name beginning with
    # one begins with exactly the two glyphs the free root began with.
    onsets_checked = 0
    identical_onsets = []
    inadmissible_onsets = []
    for s in seeds:
        p = s["profile"]
        if p["version"] == 1:
            continue
        admissible_set = set(p["onsetPairs"])
        labelled = ([(r["form"], r["native"]) for r in s["renderings"]]
                    + [(f"{r['concept']}/{r['slot']}", r["native"])
                       for r in s["boundRenderings"]])
        for (label, name) in labelled:
            if name is None:
                continue
            for (pos, a, b) in word_initial_onsets(name, p):
                onsets_checked += 1
                if a == b:
                    identical_onsets.append((s["seed"], label, name, a + b))
                if a + b not in admissible_set:
                    inadmissible_onsets.append((s["seed"], label, name,
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
    # #1095: triples are gated to zero; doubles are evidence only — they
    # must remain legal at a comparable rate, never be suppressed.
    print(f"triple-letter runs: {len(triple_runs)}")
    for (seed, form, name, run) in triple_runs[:20]:
        print(f"  seed={seed} form={form} name={name!r} run={run!r}")
    if all_names:
        print(f"names containing a doubled letter: {names_with_double} / "
              f"{len(all_names)} "
              f"({100 * names_with_double / len(all_names):.1f}%)")
    print(f"boundary rules: "
          f"{', '.join(f'{k}={v}' for k, v in sorted(boundary_rule_counts.items()))}")
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

    # --- #1096 summary -------------------------------------------------
    bound_counts = Counter(len(s["boundForms"]) for s in seeds)
    print(f"bound forms per language (cap {MAX_BOUND_FORMS}): "
          f"{', '.join(f'{k} form(s)={v} language(s)' for k, v in sorted(bound_counts.items()))}"
          f"; {bound_form_total} total")
    print(f"free/free root collisions: {total_collisions}")
    print(f"bound-related collisions (bound vs any free form or another "
          f"bound form): {bound_collisions_total}")
    print(f"stored bound forms failing the prefix/shape rules: "
          f"{len(bound_shape_failures)}")
    for (seed, concept, free, bound, reasons) in bound_shape_failures[:20]:
        print(f"  seed={seed} {concept} {free!r}->{bound!r} "
              f"reasons={','.join(reasons)}")
    print(f"stored bound forms rejected by their own profile's "
          f"admissibility relation: {len(bound_inadmissible)}")
    for (seed, concept, bound) in bound_inadmissible[:20]:
        print(f"  seed={seed} {concept} {bound!r}")
    print(f"visible free-to-bound shortenings in completed output: "
          f"{bound_shortenings} / {len(bound_lengths)} bound-slot rendering(s)")
    if bound_lengths:
        print(f"bound-slot output length distribution: min={min(bound_lengths)} "
              f"max={max(bound_lengths)} "
              f"avg={sum(bound_lengths) / len(bound_lengths):.4f}")
    print(f"bound-slot contract violations: {len(bound_violations)}")
    for (seed, concept, slot, name, reasons) in bound_violations[:20]:
        print(f"  seed={seed} {concept}/{slot} name={name!r} "
              f"reasons={','.join(reasons)}")
    print(f"bound-slot triple-letter runs: {len(bound_triple_runs)}")
    for (seed, concept, slot, name, run) in bound_triple_runs[:20]:
        print(f"  seed={seed} {concept}/{slot} name={name!r} run={run!r}")
    print(f"Bare renderings disagreeing with the concept's free form: "
          f"{len(bare_slot_mismatches)}")
    for (seed, concept, name, free) in bare_slot_mismatches[:20]:
        print(f"  seed={seed} {concept} bare={name!r} free={free!r}")
    print(f"compound orders: "
          f"{', '.join(f'{k}={v}' for k, v in sorted(compound_orders.items()))}")
    print(f"genitive orders: "
          f"{', '.join(f'{k}={v}' for k, v in sorted(genitive_orders.items()))}")

    # --- #1100 summary -------------------------------------------------
    print(f"languages with extended orthography: {marked_languages} / "
          f"{len(seeds)} (plain: {plain_languages})")
    print(f"diacritic families: "
          f"{', '.join(f'{k}={v}' for k, v in sorted(diacritic_counts.items()))}")
    if all_names:
        print(f"canonical names containing an extended character: "
              f"{names_with_extended} / {len(all_names)} "
              f"({100 * names_with_extended / len(all_names):.1f}%)")
    print(f"extended characters not in the rendering language's own "
          f"inventory: {len(foreign_extended)}")
    for (seed, form, name, chars) in foreign_extended[:20]:
        print(f"  seed={seed} form={form} name={name!r} "
              f"foreign={''.join(chars)!r}")

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

    if triple_runs:
        first = triple_runs[0]
        fail(f"{len(triple_runs)} triple-letter run(s) in rendered output "
             f"(first: seed {first[0]} {first[2]!r} run {first[3]!r})")

    if unmediated_profiles:
        fail(f"{len(unmediated_profiles)} profile(s) at generator version "
             f">= {BOUNDARY_PHONOLOGY_VERSION} declare no boundary rule "
             f"(first: seed {unmediated_profiles[0]})")

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

    # --- #1096: bound morphemes (structural, every seed range) --------

    if bound_over_cap:
        fail(f"{len(bound_over_cap)} language(s) hold more than "
             f"{MAX_BOUND_FORMS} bound forms (first: seed "
             f"{bound_over_cap[0][0]} with {bound_over_cap[0][1]})")

    if bound_before_version:
        first = bound_before_version[0]
        fail(f"{len(bound_before_version)} profile(s) below generator version "
             f"{BOUND_FORM_VERSION} carry bound forms, which would re-render "
             f"a historical language's dependent slots (first: seed "
             f"{first[0]} at version {first[1]})")

    if bound_shape_failures:
        first = bound_shape_failures[0]
        fail(f"{len(bound_shape_failures)} stored bound form(s) are not a "
             f"nonempty strictly-shorter prefix retaining a visible letter "
             f"(first: seed {first[0]} {first[1]} {first[2]!r}->{first[3]!r} "
             f"{','.join(first[4])})")

    if bound_inadmissible:
        first = bound_inadmissible[0]
        fail(f"{len(bound_inadmissible)} stored bound form(s) are rejected by "
             f"their own profile's admissibility relation (first: seed "
             f"{first[0]} {first[1]} {first[2]!r})")

    if bound_collisions_total != 0:
        fail(f"{bound_collisions_total} bound-related collision(s): a bound "
             f"form equals another concept's free form or another accepted "
             f"bound form")

    if bound_violations:
        fail(f"{len(bound_violations)} bound-slot name(s) violate the output "
             f"contract")

    if bound_triple_runs:
        first = bound_triple_runs[0]
        fail(f"{len(bound_triple_runs)} triple-letter run(s) in bound-slot "
             f"output (first: seed {first[0]} {first[3]!r} run {first[4]!r})")

    if bare_slot_mismatches:
        first = bare_slot_mismatches[0]
        fail(f"{len(bare_slot_mismatches)} Bare rendering(s) do not use the "
             f"concept's free form (first: seed {first[0]} {first[1]} "
             f"bare={first[2]!r} free={first[3]!r})")

    # A generator that quietly stopped shortening anything would still
    # satisfy every rule above — they are all "no bad form exists".
    if generator_version >= BOUND_FORM_VERSION:
        if bound_form_total == 0:
            fail(f"generator version {generator_version} produced no bound "
                 f"forms at all across {len(seeds)} language(s)")
        if bound_shortenings == 0:
            fail("no visible free-to-bound shortening occurs anywhere in the "
                 "sample, so bound forms change no completed name")
        if not bound_lengths:
            fail("no bound-slot names were rendered")

    # --- #1100: extended orthography (structural, every seed range) ---

    # The generator's declared repertoire against this tool's own. A
    # mismatch means the contract regex below is enforcing a different
    # character set from the one names are drawn from, which makes every
    # "zero contract violations" result meaningless in one direction or
    # the other.
    declared = data.get("outputInventory")
    if declared is None:
        fail("the report carries no outputInventory, so the enforced "
             "contract cannot be checked against the generator's own "
             "repertoire")
    elif declared != OUTPUT_INVENTORY:
        only_gen = "".join(sorted(set(declared) - set(OUTPUT_INVENTORY)))
        only_here = "".join(sorted(set(OUTPUT_INVENTORY) - set(declared)))
        fail(f"the generator's output repertoire ({len(declared)} chars) "
             f"differs from this tool's ({len(OUTPUT_INVENTORY)}): "
             f"generator-only {only_gen!r}, checker-only {only_here!r}")

    if extended_before_version:
        first = extended_before_version[0]
        fail(f"{len(extended_before_version)} profile(s) below generator "
             f"version {EXTENDED_ORTHOGRAPHY_VERSION} carry extended "
             f"characters, which would re-render a historical language's "
             f"names (first: seed {first[0]} at version {first[1]} with "
             f"{first[2]!r})")

    if foreign_extended:
        first = foreign_extended[0]
        fail(f"{len(foreign_extended)} name(s) contain an extended "
             f"character that is not in the inventory of the language "
             f"that rendered it, so it was applied to output rather than "
             f"drawn as a phoneme (first: seed {first[0]} {first[2]!r} "
             f"foreign {''.join(first[3])!r})")

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

        # The issue's "1280/1280 distinct names" pins BOTH halves of the
        # ratio: a generator that rendered fewer names would otherwise
        # satisfy a distinct-count pin by shrinking the sample.
        if total_names != PIN_TOTAL_NAMES:
            fail(f"total canonical names {total_names} != pinned "
                 f"{PIN_TOTAL_NAMES}")

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

        # #1096 requirement 6's "both available ordering directions":
        # with only one direction present, the claim that ordering
        # changes display order and not slot assignment is untested.
        if len(compound_orders) < 2:
            fail(f"the canonical sample uses only compound ordering(s) "
                 f"{sorted(compound_orders)}, so the bound-form slot matrix "
                 f"is exercised in one direction only")
        if len(genitive_orders) < 2:
            fail(f"the canonical sample uses only genitive ordering(s) "
                 f"{sorted(genitive_orders)}, so the possessive slot is "
                 f"exercised in one direction only")

        # #1100: the choice has to VARY by seed. One-sided in either
        # direction defeats the design — every world accented the same
        # way is no more a per-language signature than none of them
        # being accented at all.
        if marked_languages < PIN_MIN_MARKED_LANGUAGES:
            fail(f"only {marked_languages} of {len(seeds)} languages draw "
                 f"extended characters (floor {PIN_MIN_MARKED_LANGUAGES})")
        if plain_languages < PIN_MIN_PLAIN_LANGUAGES:
            fail(f"only {plain_languages} of {len(seeds)} languages draw "
                 f"none (floor {PIN_MIN_PLAIN_LANGUAGES}) — the extended "
                 f"repertoire must be a per-language choice, not a "
                 f"universal one")
        if names_with_extended == 0:
            fail("no canonical name contains an extended character, so the "
                 "widened repertoire changes no completed name")
        # More than one family, for the same reason both compound and
        # both genitive orderings are required above: with a single
        # family every accented world reads the same, and "difference
        # across worlds" is untested.
        marked_families = [f for f in diacritic_counts if f != "none"]
        if len(marked_families) < 2:
            fail(f"the canonical sample uses only the diacritic "
                 f"{sorted(marked_families)}, so an accent identifies no "
                 f"language in particular")

    if ok:
        print("CHECK OK")
        return 0
    return 1


if __name__ == "__main__":
    sys.exit(main())
