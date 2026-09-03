#!/usr/bin/env python3
"""The `tools/README.md` registry-count audit and its mutations (#1584).

`tools/README.md` used to state a probe-registry total in prose beside
the very sentence calling `run_probes.py --list` authoritative. The
number drifted three times (#539, #721, #1584), so it is gone, and this
audit keeps it gone: the `run_probes.py` section may point at `--list`
for the listing and `ci_probes.py --status` for the derived counts, but
it may not display a total of its own.

This owner is the audit itself -- its section parser, Markdown
normalization, lexical rules, diagnostics and exception contract -- and
the mutation matrix that proves each rule fires. Every rule in
`README_TOTAL_RULES` is checked against a crafted violating section AND
against the shipped file, because a rule with no proven failing case is
not a rule (#704, #1128, #1309); the per-rule coverage assertion is a
set comparison against that declaration, so the matrix cannot fall
behind the rules and no count of them is written down here to go stale.

Split out of `tools/test_run_probes.py` by #2035: that suite's process
groups, engines, schedulers, ports, preflights, retries and resource
locks have nothing to do with reading a Markdown document, and this
audit needs none of them -- only `re` and `html`. The dependency runs
one way. `tools/test_run_probes.py` imports this module and calls
`test_the_readme_states_no_registry_total` as one entry of its own
aggregate gate, so the unconditional `python3 tools/test_run_probes.py`
that CI and `tools/ci-local.sh` run still fails when this audit fails --
which that suite proves rather than assumes, driving this module against
`VIOLATING_DOCUMENT` through its own `main`. Nothing here imports back.

Usage:
  python3 tools/test_readme_registry_count.py
Exit codes: 0 = all tests passed, 1 = one or more failed.
"""
from __future__ import annotations

import html
import re
from pathlib import Path

import selftestlib
from selftestlib import FAILURES, expect


# --------------------------------------------------------------------------
# The README's run_probes section states no registry total (issue #1584)
# --------------------------------------------------------------------------
# `tools/README.md` used to name a probe total in prose ("low 30s", then
# "mid-50s") beside the very sentence calling `--list` authoritative. It
# drifted three times (#539, #721, #1584), so the count is gone and this
# guard keeps it gone: the reader is sent to `run_probes.py --list` for the
# listing and `ci_probes.py --status` for the derived counts, and any total
# the document displays must be obtained mechanically instead.
#
# Scope is lexical, and covers a total stated in either direction, without
# enumerating the verb that joins the two halves -- an enumeration is a list
# of the phrasings someone already thought of, and review found two more of
# them for every one added. So the two structural rules are: a registry
# SUBJECT and a quantity inside one clause, however joined ("the probe
# registry totals 90", "the registry consists of 93 probes", "the registry
# has a current total of 93", "Probe count: 90"); and a quantity directly
# counting a plain probe noun ("93 probes", "contains 90 probes"). A vague
# decade band, an approximated number and a number pinned to
# "currently"/"as of" are rejected on their own. Quantities count as such
# spelled out at registry scale ("ninety") as well as in digits.
#
# Nothing the reader sees is exempt: inline code is scanned with its
# backticks removed, so "There are `93 registered probes`." is caught like
# the unformatted sentence, and fenced blocks are scanned as written.
#
# A total whose noun is only an antecedent ("The suite registers 93 of
# them.") is covered too, by the partitive that carries the reference --
# adjacency is required there, so the section's own "declares 2 for each of
# those two" is untouched.
#
# What stays legal is what this section legitimately says: SMALL spelled-out
# subset counts ("two registered probes derive a second listener", "Three
# registered probes still drive Cabal"), operational quantities (`--jobs 4`,
# the 900-second default, GUI port 8008) and flag examples (`--port 9500`).
# Every rule is mutation-tested against a crafted violating section AND
# against the real file, because a rule with no proven failing case is not a
# rule (#704, #1128, #1309).

README_PATH = Path(__file__).resolve().parent / "README.md"


def use_readme(path: Path) -> None:
    """Point this audit at a document other than the shipped README.

    The one supported way to redirect it, and it exists for exactly one
    caller: `tools/test_run_probes.py` proves that a FAILING audit is a
    failing aggregate gate (#2035) by running its own `main` against
    `VIOLATING_DOCUMENT`, and needs a seam this module owns rather than
    reaching into the constant above from outside. Ordinary runs -- direct
    or composed -- never call it and read the shipped file.
    """
    global README_PATH
    README_PATH = path

# The section this guard governs. Matched EXACTLY (after stripping) so a
# renamed heading fails loudly rather than scanning nothing.
RUN_PROBES_SECTION_HEADING = "### `run_probes.py` — opt-in aggregate runner"

# A vague magnitude band -- the shape both historical drifts took. Bare
# "tens" is excluded on purpose: "low tens of minutes" is real text here.
_MAGNITUDE_BAND = (
    r"(?:low|mid|high)[-\s]"
    r"(?:\d0s|twenties|thirties|forties|fifties|sixties|seventies|eighties"
    r"|nineties)")

# A number word only a REGISTRY total would reach, written as one token
# whether it is hyphenated or spaced ("ninety-three", "ninety three"). A
# tens word is required to open it, so the small words this section
# legitimately uses for subset counts stay unreachable: "two registered
# probes" and "drifted three times" match nothing here, and neither does
# "tens" ("low tens of minutes").
_LARGE_NUMBER_WORD = (
    r"(?:(?:twenty|thirty|forty|fifty|sixty|seventy|eighty|ninety)"
    r"(?:[-\s](?:one|two|three|four|five|six|seven|eight|nine))?"
    r"|hundreds?)")

# Approximation written as a SUFFIX rather than a leading word: "90+",
# "90-plus", "90 or more", "90-ish". These say the same thing as "around 90"
# from the other side of the number, so the number token absorbs them --
# which is also what lets "90-plus" be seen at all, since a bare number
# refuses to be glued to a hyphenated word.
_APPROX_SUFFIX = (
    r"(?:\s*\+|[-\s]plus\b|-?ish\b"
    r"|\s+or\s+(?:more|so|thereabouts)\b)")

# The head of an attributive compound that counts the registry itself:
# "a 93-PROBE registry", "a 93-ENTRY list". Naming the head is what keeps
# "900-second" out -- a second is not something the registry holds.
_COUNTED_HEAD = r"(?:probe|entry|entries|script|member|row|slot)s?"

# A number standing on its OWN, which is what a count is. The lookarounds
# drop numbers glued to something else: an identifier or hyphenated compound
# ("900-second default", "utf-8") and a term of an expression ("`--port + 1`",
# "base + N - 1"). Those are the two shapes this section's real numbers take
# beside a registry noun. An explicit approximate suffix is the exception --
# it is punctuation ABOUT the count, not a different thing being counted.
_STANDALONE_NUMBER = (
    r"(?<![\w+*/-])(?:"
    r"(?:\d+|" + _LARGE_NUMBER_WORD + r")" + _APPROX_SUFFIX +
    r"|\d+(?=-" + _COUNTED_HEAD + r"\b)"
    r"|\d+(?![\w-])"
    r"|" + _LARGE_NUMBER_WORD + r"\b)")

# Any quantity a total can be stated as.
_QUANTITY = r"(?:" + _STANDALONE_NUMBER + r"|" + _MAGNITUDE_BAND + r")"

# A noun phrase naming the registry as a whole. Plain "probes" is NOT one:
# "Run up to 4 probes concurrently" counts a concurrency limit, not the
# registry.
_REGISTRY_NOUN = (
    r"(?:registered\s+probes?|probes?\s+(?:\w+\s+){0,2}registered"
    r"|probe\s+registry"
    r"|probes?\s+in\s+the\s+registry|total\s+probes?"
    r"|probes?\s+in\s+total)")

# The SUBJECT half of a stated total, wider than the noun above: a bare
# "registry", and the count nouns a total gets attached to, name the whole
# thing too.
# A noun that IS a count. Enumerated once, then reused in every shape the
# subject can take, so a synonym is added in one place instead of in three
# -- "tally" was the round-8 bypass, and "probe tally", "tally of probes"
# and "registry tally" all became reachable together.
_COUNT_NOUN = (
    r"(?:counts?|totals?|tall(?:y|ies)|numbers?|sizes?|lengths?|census(?:es)?"
    r"|head\s?counts?|inventor(?:y|ies)|rosters?|lists?|tables?"
    r"|registr(?:y|ies)|populations?|amounts?"
    # The container is a count noun too: what "the probe collection" has 93
    # of is the registry. Three words this section uses for a SUBSET are
    # deliberately absent -- "a selection of the probes above", "a `--jobs`
    # batch", and the process "group" a probe is reaped in.
    r"|collections?|sets?|suites?|famil(?:y|ies)|catalogues?|catalogs?"
    r"|indexe?s?|directories|directory|pools?|arrays?|bundles?|corpus(?:es)?"
    r"|complements?|manifests?|listings?|ledgers?|enumerations?|line-?ups?"
    r"|series|rolls?|sheets?)")

_REGISTRY_SUBJECT = (
    r"(?:" + _REGISTRY_NOUN + r"|registry"
    # "probe tally", "the probes' count", "the registry's size", and with
    # what is being counted named in between: "probe script count",
    # "probe-script count", "registry entry total". The middle word must be
    # something the registry HOLDS, which is what keeps the section's own "a
    # probe's PORT count" out -- a port is not a member of the registry.
    r"|(?:probes?|registry)(?:'s|')?(?:\s+|-)(?:" + _COUNTED_HEAD
    + r"(?:\s+|-))?" + _COUNT_NOUN +
    # "tally of the registered probes", "count of all probes", "total
    # number of every registered probe" -- the qualifiers between "of" and
    # the noun are counted, not spelled, so a new one needs no edit here.
    r"|" + _COUNT_NOUN + r"\s+of\s+(?:\w+\s+){0,3}?probes?\b"
    # "the list", "the table" -- in this section the DEFINITE one is the
    # registry, which is why "--list" is called authoritative two sentences
    # earlier. The article must sit directly on the noun, so the section's
    # own "A declared count `N`" (indefinite, and qualified) is not one.
    r"|the\s+" + _COUNT_NOUN + r"\b)")

# "all probes", "every registered probe" -- a universal quantifier names the
# whole registry as surely as "the registry" does. It is a LEADING subject
# only: trailing, "every probe" is just how this section refers to probes in
# general ("exits 130 after terminating every probe still running"), and
# "every OTHER probe" is a genuine subset, so neither may be reached
# backwards from a number.
_UNIVERSAL_PROBE_SUBJECT = r"(?:all|every)\s+(?:the\s+)?(?:registered\s+)?probes?"

# The registry's own object, which names the whole thing as directly as any
# English phrase does. It has to be matched CASE-SENSITIVELY: `PROBES` is
# the identifier, and case-insensitively it is the word "probes", which this
# section says twenty times about individual probes.
# The module qualifier is part of the OBJECT, not of the words between a
# quantity and it: the gap the inverted branch below walks forbids `.`, so
# an unlisted qualifier would put a dot in that gap and silently stop the
# rule from firing. `probe_runner_registry` is the owner since #2074;
# `run_probes` is kept because older prose still spells it that way.
_REGISTRY_OBJECT = (
    r"(?:(?:run_probes|probe_runner_registry)\.)?PROBES"
    r"(?:\s+(?:list|dict|table|map|registry|roster))?")

# "~90" has no word boundary before the tilde, so the two spellings need
# separate anchoring; both end immediately before the quantity.
_APPROXIMATOR = (
    r"(?:~\s*"
    r"|\b(?:about|around|roughly|approximately|nearly|almost|some"
    r"|close\s+to|more\s+than|over|upwards\s+of)\s+)")

_AS_OF_NOW = (
    r"(?:currently|now|today|at\s+present|as\s+of|right\s+now"
    r"|at\s+the\s+moment|these\s+days)")

# Phrases that make a number a BOUND rather than a count -- the one shape
# this section legitimately writes as "<number> probes".
_BOUND_PREFIX = (r"(?<!up to )(?<!at most )(?<!as many as )"
                 r"(?<!no more than )(?<!only )")

README_TOTAL_RULES: tuple[tuple[str, "re.Pattern[str]"], ...] = (
    # "currently in the mid-50s", "low 30s", "high 80s"
    ("magnitude-band", re.compile(_MAGNITUDE_BAND, re.I)),
    # "around 90", "roughly 90", "~90", "about ninety"
    ("approximate-quantity",
     re.compile(_APPROXIMATOR + r"(?:\d+|" + _LARGE_NUMBER_WORD + r"\b)",
                re.I)),
    # "currently 90", "as of today, 90", "now in the mid-50s"
    ("dated-quantity",
     re.compile(r"\b" + _AS_OF_NOW + r"\b[^.\n]{0,40}?" + _QUANTITY, re.I)),
    # A registry SUBJECT and a quantity in the same clause, in EITHER order
    # and however they are joined: "the probe registry totals 90", "the
    # registry consists of 93 probes", "Probe count: 90", and inverted,
    # "there are 93 entries in the probe registry". Deliberately NOT a list
    # of verbs -- an enumeration is a list of the phrasings someone already
    # thought of, and word order is just one more phrasing. The clause ends
    # at "." or ";" or a line break, and the reach is capped, so a number
    # elsewhere in the same sentence is not swept in.
    ("registry-quantity-clause",
     re.compile(r"(?:(?:" + _REGISTRY_SUBJECT + r"|"
                + _UNIVERSAL_PROBE_SUBJECT + r")[^.;\n]{0,80}?"
                + _APPROXIMATOR + r"?" + _QUANTITY
                + r"|" + _APPROXIMATOR + r"?" + _QUANTITY
                + r"[^.;\n]{0,80}?" + _REGISTRY_SUBJECT + r")", re.I)),
    # A quantity directly counting a plain member of the registry: "93
    # probes", "contains 90 probes", "93 registered scripts", "ninety
    # entries". The noun is the same class the compounds use -- `PROBES` is
    # a registry of probe SCRIPTS, so counting its scripts counts it. Small
    # spelled-out numbers are not quantities here, so "two registered
    # probes" stays legal, and an explicit BOUND ("up to 4 probes") is
    # exempt.
    ("quantified-registry-noun",
     re.compile(_BOUND_PREFIX + _APPROXIMATOR + r"?" + _STANDALONE_NUMBER
                + r"(?:\s+\w+){0,2}(?:\s+|-)" + _COUNTED_HEAD + r"\b",
                re.I)),
    # A quantity whose noun is only an ANTECEDENT: "the suite registers 93 of
    # them". The partitive must follow the number DIRECTLY -- that is what
    # separates it from "declares 2 for each of those two", where the number
    # counts something else entirely.
    ("partitive-quantity",
     re.compile(_APPROXIMATOR + r"?" + _STANDALONE_NUMBER
                + r"\s+of\s+(?:them|these|those|the\s+(?:probes|registry"
                r"|scripts|lot))\b", re.I)),
    # A quantity reached DIRECTLY by a verb of having or listing, with no
    # subject or noun to anchor it: "the suite registers 93". Adjacency is
    # again what keeps the section's own numbers out -- "uses 3600 seconds",
    # "exits 130", "reaches 8008" and "used stride 1" all say what their
    # number is, and none of these verbs is how they say it.
    ("counting-verb-quantity",
     re.compile(r"\b(?:registers?|registered|lists?|ships?|holds?|counts?"
                r"|totals?|totall?ed|numbers?|numbered|contains?|comprises?"
                r"|includes?|consists?\s+of|carries|carry)\s+"
                + _APPROXIMATOR + r"?" + _STANDALONE_NUMBER, re.I)),
    # The registry OBJECT and a quantity in one clause, in either order:
    # "the PROBES list has 93 entries", "93 entries in PROBES". Same shape as
    # `registry-quantity-clause`, but the object is anchored
    # case-sensitively, so only the surrounding prose is folded.
    ("registry-object-quantity",
     re.compile(r"(?:" + _REGISTRY_OBJECT + r"(?i:[^.;\n]{0,80}?"
                + _APPROXIMATOR + r"?" + _QUANTITY + r")"
                + r"|(?i:" + _APPROXIMATOR + r"?" + _QUANTITY
                + r"[^.;\n]{0,80}?)" + _REGISTRY_OBJECT + r")")),
)


class ReadmeSectionError(Exception):
    """The guard could not locate exactly one target section.

    Raised rather than returning an empty section: a guard that silently
    scanned nothing would report a clean file forever.
    """


# Markdown has TWO code fences, and a tilde one hid a total from an earlier
# revision of this guard: `###` inside it read as the next heading, ending
# the section early. A fence closes only on its OWN character, and only on a
# run at least as long as the one that opened it, so a shorter or
# different-character run inside a block stays content.
_FENCE_LINE = re.compile(r"^\s*(?P<fence>`{3,}|~{3,})(?P<info>.*)$")

# A Markdown link or image renders as its LABEL; the destination is not
# displayed at all. Both forms need "](" or "][" with nothing between, which
# is what leaves the progress block's "[1/1] persistence_contract_sweep.py"
# and "[timeout 120s] TIMEOUT (120.0s)" alone -- a bracket followed by a
# space is not a link.
_MD_LINK = re.compile(
    r"!?\[([^\]\n]*)\](?:\([^)\n]*\)|\[[^\]\n]*\])")

# Inline HTML, which Markdown renders: the TAGS are markup and go, while
# everything between them is displayed text and stays. An HTML comment's
# delimiters go the same way and its content is deliberately KEPT -- scanning
# more than the reader sees is the safe direction here. The tag pattern needs
# a letter straight after "<", so a bare comparison ("`--jobs > 1`") is not
# mistaken for one, and its attribute run is quote-aware so a ">" inside an
# attribute value does not truncate it.
# The attribute run is quote-AWARE: a ">" inside a quoted attribute value
# does not end the tag, so `<em title="x > y">` is one tag rather than a
# truncated one that leaves "y\">" behind between a number and its noun.
_HTML_MARKUP = re.compile(
    r"""<!--|-->|</?[A-Za-z][A-Za-z0-9-]*(?:"[^"]*"|'[^']*'|[^>"'])*>""")

# Quotation marks around displayed text. Same non-intra-word rule as the
# formatting runs, so a possessive apostrophe survives ("a probe's port
# count") while the quotes in `There are "93" probes.` do not -- a quote is
# no more part of the number than a star is.
_QUOTE_RUN = re.compile(
    r"(?<![A-Za-z0-9])[\"'\u201c\u201d\u2018\u2019\u00ab\u00bb]+"
    r"|[\"'\u201c\u201d\u2018\u2019\u00ab\u00bb]+(?![A-Za-z0-9])")

# A formatting run: one to three "*" or "_" not flanked by alphanumerics on
# BOTH sides. In prose that is Markdown emphasis; in a fenced comment it is
# someone writing emphasis where it does not render. Either way it is not
# part of the number beside it. Intra-word runs are left alone, which keeps
# `PROBE_PORT_SPANS` one token and "2*3" an expression.
_FORMATTING_RUN = re.compile(r"(?<![A-Za-z0-9])[*_]{1,3}|[*_]{1,3}(?![A-Za-z0-9])")


def _mark_fences(lines: list[str]) -> list[tuple[str, bool]]:
    """Pair each line with whether it is fenced code (the fences included)."""
    marked: list[tuple[str, bool]] = []
    open_char: str | None = None
    open_length = 0
    for line in lines:
        match = _FENCE_LINE.match(line)
        if match:
            fence = match.group("fence")
            char, length = fence[0], len(fence)
            if open_char is None:
                # A backtick fence's info string may not contain a backtick;
                # that shape is inline code, not an opening fence.
                if not (char == "`" and "`" in match.group("info")):
                    marked.append((line, True))
                    open_char, open_length = char, length
                    continue
            elif (char == open_char and length >= open_length
                  and not match.group("info").strip()):
                marked.append((line, True))
                open_char = None
                continue
        marked.append((line, open_char is not None))
    return marked


def readme_section(text: str,
                   heading: str = RUN_PROBES_SECTION_HEADING) -> str:
    """Return the body of `heading`'s section, its fenced code included.

    The section ends at the next heading of the SAME OR HIGHER level, so a
    child heading ("#### Details") stays INSIDE it rather than truncating
    the scan. Only headings outside a fence count -- the bash examples are
    full of `# comment` lines that would otherwise look like one. A heading
    that is absent, or present more than once, raises.
    """
    marked = _mark_fences(text.splitlines())
    starts = [index for index, (line, fenced) in enumerate(marked)
              if not fenced and line.strip() == heading]
    if not starts:
        raise ReadmeSectionError(f"section heading not found: {heading!r}")
    if len(starts) > 1:
        raise ReadmeSectionError(
            f"section heading is ambiguous: {heading!r} appears "
            f"{len(starts)} times (lines "
            f"{', '.join(str(index + 1) for index in starts)})")
    start = starts[0]
    level = len(heading) - len(heading.lstrip("#"))
    end = len(marked)
    for index in range(start + 1, len(marked)):
        line, fenced = marked[index]
        if fenced:
            continue
        following = re.match(r"(#{1,6}) \S", line)
        if following and len(following.group(1)) <= level:
            end = index
            break
    return "\n".join(line for line, _ in marked[start + 1:end])


def _scannable(section: str) -> str:
    """Return the section's DISPLAYED text: nothing is exempt from the rules.

    Inline code spans keep their content and lose only their backticks --
    "There are `93 registered probes`." is a total the reader sees, so it is
    a total the rules must see. The backticks become spaces so a span's edge
    cannot fuse two tokens, and the substitution is done per prose RUN rather
    than per line because a span here wraps across lines. Fenced blocks are
    passed through unchanged (their backticks are the fences), so a total
    smuggled into a shell comment is scanned too.

    A link or image is reduced to its label, since that is the whole of what
    the reader sees -- "There are [93](https://example.test) registered
    probes." states the total as plainly as the unlinked sentence.

    Inline HTML is reduced to what it renders: entities are decoded, tags
    and comment delimiters are dropped, and the text between them is kept --
    "There are 93 <em>registered</em> probes." states the same total the
    plain sentence does. That pass and the formatting one run over FENCED
    content too, since a shell comment can carry a total as easily as a
    paragraph can, and "**93**" hides its number in either.

    Quotation marks go too, by the same non-intra-word rule that spares a
    possessive apostrophe: `There are "93" probes.` displays a total, and
    the quotes are no more part of the number than a star is.

    Emphasis delimiters go the same way as the backticks, for the same
    reason: "There are **93 registered probes**." displays a total, and the
    stars are not part of it. A run is only a delimiter when it is NOT
    flanked by alphanumerics on both sides, which is what leaves an
    identifier's underscores (`PROBE_PORT_SPANS`) and an arithmetic "2*3"
    intact -- the latter deliberately, since a term of an expression is not
    a count.

    Prose is also UNWRAPPED. This file is hard-wrapped near 75 columns, so
    every paragraph in it is soft line breaks the reader never sees; a rule
    that stopped at one would be defeated by where a sentence happened to
    fold. A BLANK line is a real break and is kept, which is what still
    bounds a clause -- along with the "." and ";" the rules stop at, and
    their own character cap.
    """
    chunks: list[str] = []
    run: list[str] = []
    run_fenced: bool | None = None

    def flush() -> None:
        if not run:
            return
        # Decoration, entities and tags are noise in BOTH runs: inside a
        # fence "**93**" is not emphasis, but it is not arithmetic either,
        # and the stars would hide the number just the same.
        blob = html.unescape("\n".join(run))
        blob = _MD_LINK.sub(r" \1 ", blob)
        blob = _FORMATTING_RUN.sub(" ", _HTML_MARKUP.sub(" ", blob))
        blob = _QUOTE_RUN.sub(" ", blob)
        if run_fenced:
            chunks.append(blob)
            return
        # Prose only: a backtick delimits inline code here rather than a
        # block, and a line break inside a paragraph is soft.
        blob = re.sub(r"`([^`]*)`", r" \1 ", blob, flags=re.S)
        blob = re.sub(r"(?<!\n)\n(?![\n])", " ", blob)
        chunks.append(blob)

    for line, fenced in _mark_fences(section.splitlines()):
        if run_fenced is None or fenced == run_fenced:
            run_fenced = fenced
            run.append(line)
        else:
            flush()
            run = [line]
            run_fenced = fenced
    flush()
    return "\n".join(chunks)


def readme_total_claim_problems(
        text: str,
        heading: str = RUN_PROBES_SECTION_HEADING) -> list[str]:
    """Report every registry-total claim in `heading`'s section.

    Raises ReadmeSectionError if that section cannot be located exactly once.
    """
    scannable = _scannable(readme_section(text, heading))
    problems: list[str] = []
    for rule, pattern in README_TOTAL_RULES:
        for match in pattern.finditer(scannable):
            problems.append(f"{rule}: {match.group(0)!r}")
    return problems


# The one crafted total the boundary cases below reuse, and the smallest
# document that displays it inside the guarded section. `VIOLATING_DOCUMENT`
# is this module's export to `tools/test_run_probes.py`, which drives this
# audit against it to prove a failing audit is a failing aggregate gate
# (#2035) -- so the phrasing is written once, here, rather than retyped in
# the aggregate where it could drift into something the rules accept.
TOTAL_CLAIM_EXAMPLE = "The registry holds 90 registered probes.\n"

VIOLATING_DOCUMENT = ("# Tools\n\n" + RUN_PROBES_SECTION_HEADING + "\n\n"
                      + TOTAL_CLAIM_EXAMPLE)


def test_the_readme_states_no_registry_total() -> None:
    print("\n-- tools/README.md's run_probes section states no registry total")

    # The shipped file, through the same helper the mutations use.
    shipped = README_PATH.read_text(encoding="utf-8")
    expect(readme_total_claim_problems(shipped) == [],
           f"the shipped README section claims no registry total "
           f"({readme_total_claim_problems(shipped)})")

    # The document the aggregate gate's composition proof drives this audit
    # against has to really violate, or that proof would pass on a clean run.
    expect(readme_total_claim_problems(VIOLATING_DOCUMENT) != [],
           "the shared violating document states a registry total")

    section = readme_section(shipped)
    expect("run_probes.py\n--list`" in section
           and "`python3\ntools/ci_probes.py --status`" in section,
           "it points at --list for the listing and ci_probes.py --status "
           "for the counts")

    # --- the section boundary itself -------------------------------------
    # A guard that cannot find its target must fail, never scan "".
    for broken, why in (
            (shipped.replace(RUN_PROBES_SECTION_HEADING,
                             "### `run_probes.py` — aggregate runner", 1),
             "a renamed heading"),
            (shipped.replace(RUN_PROBES_SECTION_HEADING, "", 1),
             "a deleted heading")):
        try:
            readme_total_claim_problems(broken)
            expect(False, f"{why} is refused")
        except ReadmeSectionError as error:
            expect("not found" in str(error), f"{why} is refused ({error})")
    duplicated = shipped.replace(
        RUN_PROBES_SECTION_HEADING,
        f"{RUN_PROBES_SECTION_HEADING}\n\nstub\n\n"
        f"{RUN_PROBES_SECTION_HEADING}", 1)
    try:
        readme_total_claim_problems(duplicated)
        expect(False, "a duplicated heading is refused")
    except ReadmeSectionError as error:
        expect("ambiguous" in str(error),
               f"a duplicated heading is refused ({error})")

    # The scan stops at the next heading: an identical claim one section
    # later is out of scope, and the same claim inside is caught.
    heading_line = f"{RUN_PROBES_SECTION_HEADING}\n"
    claim = TOTAL_CLAIM_EXAMPLE
    outside = ("# Tools\n\n" + heading_line + "\nBody.\n\n"
               "### `other.py`\n\n" + claim)
    inside = ("# Tools\n\n" + heading_line + "\nBody. " + claim
              + "\n### `other.py`\n\nUnrelated.\n")
    expect(readme_total_claim_problems(outside) == [],
           "a total claim in the NEXT section is out of scope")
    expect(readme_total_claim_problems(inside) != [],
           "the same claim inside the section is caught")
    # A CHILD heading does not end the section -- a total parked under one
    # is still inside it -- while a sibling or parent heading does.
    child = ("# Tools\n\n" + heading_line + "\nBody.\n\n#### Details\n\n"
             + claim + "\n### `other.py`\n\nUnrelated.\n")
    expect(readme_total_claim_problems(child) != [],
           "a total under a CHILD heading is still in scope")
    for level, where in (("###", "a sibling heading"),
                         ("##", "a parent heading"),
                         ("#", "a top-level heading")):
        beyond = ("# Tools\n\n" + heading_line + "\nBody.\n\n"
                  + f"{level} Elsewhere\n\n" + claim)
        expect(readme_total_claim_problems(beyond) == [],
               f"{where} ends the section")

    # A `# comment` line inside a fenced block is not a heading, so a claim
    # after one is still inside the section.
    fenced = ("# Tools\n\n" + heading_line + "\n```bash\n"
              "# Run up to 4 probes concurrently\nrun\n```\n\n" + claim)
    expect(readme_total_claim_problems(fenced) != [],
           "a fenced `# comment` does not end the section early")

    # Markdown's OTHER fence. A tilde block whose content opens with `###`
    # ended the section early in an earlier revision, hiding the total the
    # same block displayed.
    for opener, closer, why in (
            ("~~~bash", "~~~", "a tilde fence"),
            ("````bash", "````", "a four-backtick fence"),
            ("~~~~", "~~~~", "a four-tilde fence")):
        block = ("# Tools\n\n" + heading_line + f"\n{opener}\n###\n"
                 "# There are 93 registered probes.\nrun\n"
                 f"{closer}\n\n### `other.py`\n\nUnrelated.\n")
        expect(readme_total_claim_problems(block) != [],
               f"{why} keeps its content in scope")
    # A fence closes only on its OWN character, and only on a run at least
    # as long as the opener -- otherwise the rest of the block escapes.
    for inner, why in (("```", "a shorter backtick run"),
                       ("~~~", "a different fence character")):
        block = ("# Tools\n\n" + heading_line + "\n````bash\n"
                 f"{inner}\n###\n# There are 93 registered probes.\n"
                 "````\n\n### `other.py`\n\nUnrelated.\n")
        expect(readme_total_claim_problems(block) != [],
               f"{why} does not close the block ({why})")
    # A line of inline code is not an opening fence, so the prose after it
    # is still prose and still scanned.
    inline_run = ("# Tools\n\n" + heading_line + "\n```a``` is inline.\n\n"
                  + claim)
    expect(readme_total_claim_problems(inline_run) != [],
           "a backtick run with code in its info string is not a fence")

    # --- one crafted violation per rule ----------------------------------
    def rules_fired(body: str) -> set[str]:
        doc = "# Tools\n\n" + heading_line + "\n" + body + "\n"
        return {problem.split(":", 1)[0]
                for problem in readme_total_claim_problems(doc)}

    per_rule = {
        "magnitude-band": (
            "It sits in the mid-50s.",
            "It sits in the low 30s.",
            "It sits in the high 80s.",
            "It sits in the mid 50s.",
        ),
        "approximate-quantity": (
            "It has grown to around 90 since then.",
            "It has grown to roughly 90 since then.",
            "It has grown to ~90 since then.",
            "It has grown to about 90 since then.",
            "It has grown to more than 80 since then.",
            "It has grown to about ninety since then.",
        ),
        "dated-quantity": (
            "Today that number is 90.",
            "It stood, at present, at 90.",
            "It stands now in the mid-50s.",
        ),
        # A registry subject and a quantity in one clause, joined by whatever
        # the writer chose. The first five are verbatim the forms rounds 1
        # and 3 of review found the narrower, verb-enumerating rules let
        # through.
        "registry-quantity-clause": (
            "Probe count: 90.",
            "The probe registry totals 90.",
            "The number of registered probes is 90.",
            "The probe registry consists of 93 probes.",
            "The registry has a current total of 93 probes.",
            "The registry has a current total of 93.",
            "The registry holds 90.",
            "The probe registry = 90.",
            "The registry stands at 90.",
            "The registry is 93 strong.",
            "The registry grew to ninety.",
            "The registry, at the time of writing, holds 93.",
            "The probe total is 93.",
            "The total number of registered probes is 93.",
            "Registered probes: 90.",
            "The registry: 93.",
            "The probe list has 93 entries.",
            "The list of registered probes has 93 entries.",
            "The probe table has 93 rows.",
            "The probe tally is 93.",
            "The probe census is 93.",
            "The registry size is 93.",
            "The registry's total is 93.",
            "The tally of registered probes is 93.",
            "The count of all probes is 93.",
            "The tally of every registered probe is 93.",
            "The number of all the probes is 93.",
            "All probes, 93 in total, are registered.",
            "All registered probes: 93.",
            "Every registered probe is one of 93.",
            "All the probes number ninety-three.",
            # A possessive survives the quote strip; a TRAILING one is
            # removed, which the widened separator absorbs.
            "The probes' count is 93.",
            "The probe headcount is 93.",
            "The full probe collection has 93 members.",
            "The probe suite has 93 members.",
            "The collection of registered probes is 93 strong.",
            "The probe catalogue holds 93.",
            "The registry index is 93 long.",
            "A 93-entry probe list is current.",
            "The probe manifest has 93 entries.",
            "The probe ledger has 93 entries.",
            "The listing of registered probes runs to 93.",
            "The probe line-up is 93 strong.",
            "The [probe registry](https://example.test) totals 93.",
            "There are 93 entries in the list.",
            "The list has 93 entries.",
            "The table has 93 rows.",
            "The count is 93.",
            "The probe script count is 93.",
            "The probe-script count is 93.",
            "The registry entry total is 93.",
            "The probe member count is ninety-three.",
            "The probe tally is ninety-three.",
            "The registry holds ninety-three.",
            "The probe tally is 90+.",
            # The same totals folded across a soft line break, which is how
            # this hard-wrapped file would actually carry one.
            "Probe count:\n93.",
            "The probe registry\ntotals 90.",
            "The probe registry totals **93**.",
            "*The registry holds 93.*",
            "The probe tally is **ninety-three**.",
            "The probe registry totals <b>93</b>.",
            "<!-- The probe registry totals 93. -->",
            "The registry holds 93.&nbsp;",
            "The registry holds ninety-plus.",
            # Inverted: the quantity leads and the subject follows.
            "There are 93 entries in the probe registry.",
            "There are 93 scripts in the registry.",
            "It ships 93 in the probe list.",
            "Ninety sit in the registry today.",
        ),
        # A quantity carried by an antecedent rather than a noun. The first
        # is verbatim the form round 4 of review found.
        "partitive-quantity": (
            "93 of them are registered.",
            "The suite registers 93 of them.",
            "It ships ninety of them.",
            "Around 93 of these are registered.",
            "It lists 93 of the probes.",
            "The runner knows 93 of the registry.",
        ),
        # The registry's own object as the subject. The first is verbatim the
        # form round 6 of review found.
        "registry-object-quantity": (
            # First case fires this rule ALONE: "PROBES" with no count noun
            # after it, so the case-insensitive subject cannot reach it, and
            # no verb, probe noun or partitive to reach either.
            "PROBES: 93.",
            "The PROBES list has 93 entries.",
            "`probe_runner_registry.PROBES` has 93 entries.",
            "PROBES holds 93.",
            "The PROBES dict is 93 long.",
            # Inverted, the same way.
            "There are 93 entries in PROBES.",
            "93 rows sit in `probe_runner_registry.PROBES`.",
        ),
        # A quantity reached directly by a verb of having or listing, with
        # neither a registry subject nor a noun.
        "counting-verb-quantity": (
            # First case fires this rule ALONE: the subject is a pronoun, so
            # no count noun, probe noun or partitive can reach the number.
            "It registers 93.",
            "The suite registers 93.",
            "The runner holds 93.",
            "It contains ninety.",
            "The suite lists around 93.",
        ),
        # A quantity counting a plain registry member, with no registry
        # subject and no counting verb needed.
        "quantified-registry-noun": (
            "There are 90 probes.",
            "There are ninety-three probes.",
            "There are ninety three probes.",
            "About ninety-three probes are registered.",
            "There are 90+ probes.",
            "There are 90-plus probes.",
            "There are 90 or more probes.",
            "There are 90-ish probes.",
            "There are 90 or so probes.",
            "There are 93 registered\nprobes.",
            # Emphasis is display, not content.
            "There are **93 registered probes**.",
            "There are __93 registered probes__.",
            # Inline HTML renders to the same sentence.
            "There are 93 <em>registered</em> probes.",
            "There are <span class=\"x\">93 registered probes</span>.",
            # A ">" inside a quoted attribute value does not end the tag.
            "There are 93 <em title=\"x > y\">registered</em> scripts.",
            "There are <span data-n='a > b'>93 probes</span>.",
            "There are &#57;&#51; registered probes.",
            # The attributive compound: the number counts the head noun.
            "A 93-probe registry is current.",
            "A 93-probe suite ships today.",
            # A link renders as its label.
            "There are [93](https://example.test) registered probes.",
            "There are [93][count] registered probes.",
            "There are ![93](x.png) registered probes.",
            # `PROBES` is a registry of probe scripts.
            "There are 93 registered scripts.",
            "There are 93 scripts.",
            "There are ninety entries.",
            "It holds 93 rows.",
            # Quoted, straight and curly, digits and words alike.
            "There are \"93\" probes.",
            "There are '93' registered scripts.",
            "There are \u201c93\u201d probes.",
            "There are \"ninety-three\" probes.",
            "It is a ninety-three-probe registry.",
            "90 registered probes ship today.",
            "It registers 90 probes today.",
            "It lists ninety probes.",
            "This doc tracks 93 probes.",
            "We ship 93 probes.",
        ),
    }

    expect(set(per_rule) == {rule for rule, _ in README_TOTAL_RULES},
           f"every rule has crafted violations "
           f"(missing {sorted({r for r, _ in README_TOTAL_RULES} - set(per_rule))}, "
           f"unknown {sorted(set(per_rule) - {r for r, _ in README_TOTAL_RULES})})")
    for rule, bodies in per_rule.items():
        for body in bodies:
            fired = rules_fired(body)
            expect(rule in fired,
                   f"[{rule}] rejects {body!r} (fired {sorted(fired)})")
    # Each rule is independently load-bearing: the first case of each fires
    # that rule ALONE, so no rule is carried by another.
    for rule, bodies in per_rule.items():
        fired = rules_fired(bodies[0])
        expect(fired == {rule},
               f"[{rule}] is the only rule {bodies[0]!r} needs "
               f"(fired {sorted(fired)})")

    # Requirement 3 lists the normalization categories by name, so the
    # matrix names them too: one representative prohibited total per
    # category, proving no formatting path reaches the rules unnormalized.
    # A plain-text mutation cannot prove any of these.
    by_normalization = {
        "inline-code": "The probe registry totals `93`.",
        "emphasis": "There are **93 registered probes**.",
        "link": "There are [93](https://example.test) registered probes.",
        "inline-html": "There are 93 <em>registered</em> probes.",
        "inline-html-attribute":
            "There are 93 <em title=\"x > y\">registered</em> scripts.",
        "entity": "There are &#57;&#51; registered probes.",
        "quotation": "There are \"93\" probes.",
        "soft-wrap": "Probe count:\n93.",
        "fenced-comment":
            "```bash\n# There are **93 registered probes**.\nrun\n```",
    }
    expect(set(by_normalization) == {
        "inline-code", "emphasis", "link", "inline-html",
        "inline-html-attribute", "entity", "quotation", "soft-wrap",
        "fenced-comment"},
        "every normalization category requirement 3 names has a case")
    for category, body in by_normalization.items():
        fired = rules_fired(body)
        expect(fired != set(),
               f"[{category}] a total survives normalization "
               f"({body[:44]!r})")

    # The historical sentences, verbatim, as they actually shipped.
    for historical in (
            "`python3 tools/run_probes.py --list` is the authoritative count "
            "and listing of registered probes — it's grown over time "
            "(currently in the low 30s) and this doc doesn't try to track "
            "the exact number.",
            "`python3 tools/run_probes.py --list` is the authoritative count "
            "and listing of registered probes — it's grown over time "
            "(currently in the mid-50s) and this doc doesn't try to track "
            "the exact number.",
            "`python3 tools/run_probes.py --list` is the authoritative count "
            "and listing of registered probes — it's grown over time "
            "(currently in the high 80s) and this doc doesn't try to track "
            "the exact number.",
            "`python3 tools/ci_probes.py --status` reports 11 CI-eligible, "
            "79 manual-only, 90 total registered probes.",
            # The round-1 review's three bypasses, verbatim.
            "The probe registry totals 90.",
            "The registry contains 90 probes.",
            "The number of registered probes is 90.",
            # The round-3 review's two bypasses, verbatim.
            "The probe registry consists of 93 probes.",
            "The registry has a current total of 93 probes.",
            # The round-4 review's bypass, verbatim, and the same total with
            # the antecedent left implicit.
            "The suite registers 93 of them.",
            "The suite registers 93.",
            # The round-6 review's bypass, verbatim.
            "The PROBES list has 93 entries.",
            # The round-7 review's bypass, verbatim.
            "There are 93 entries in the probe registry.",
            # The round-8 review's bypass, verbatim.
            "The probe tally is 93.",
            # The round-9 review's bypass, verbatim.
            "There are ninety-three probes.",
            # The round-10 review's bypass, verbatim.
            "The count of all probes is 93.",
            # The round-12 review's bypass, verbatim.
            "All probes, 93 in total, are registered.",
            # The round-13 review's two bypasses, verbatim.
            "There are 90+ probes.",
            "There are 90-plus probes.",
            # The round-14 review's bypass, verbatim.
            "Probe count:\n93.",
            # The round-15 review's two bypasses, verbatim.
            "There are **93 registered probes**.",
            "The probe registry totals **93**.",
            # The round-16 review's bypass, verbatim.
            "There are 93 <em>registered</em> probes.",
            # The round-18 review's bypass, verbatim.
            "The full probe collection has 93 members.",
            # The round-19 review's bypass, verbatim.
            "A 93-probe registry is current.",
            # The round-20 review's bypass, verbatim.
            "The probe manifest has 93 entries.",
            # The round-21 review's bypass, verbatim.
            "There are [93](https://example.test) registered probes.",
            # The round-22 review's two bypasses, verbatim.
            "There are 93 entries in the list.",
            "The list has 93 entries.",
            # The round-23 review's two bypasses, verbatim.
            "The probe script count is 93.",
            "The probe-script count is 93.",
            # The round-24 review's two bypasses, verbatim.
            "There are 93 registered scripts.",
            "There are 93 scripts.",
            # The round-25 review's bypass, verbatim.
            "There are \"93\" probes.",
            # The round-26 review's bypass, verbatim.
            "There are 93 <em title=\"x > y\">registered</em> scripts.",
            # And the original drift sentence as it really shipped, wrapped.
            "`python3 tools/run_probes.py --list` is the authoritative count\n"
            "and listing of registered probes — it's grown over time\n"
            "(currently in the mid-50s) and this doc doesn't try to\n"
            "track the exact number.",
            # The round-2 review's bypass: the same totals, formatted.
            "There are `93 registered probes`.",
            "The probe registry totals `93`.",
            "`90 registered probes` are listed here.",
            "The registry holds `ninety`.",
            "It is `currently in the mid-50s`.",
            "```bash\n# There are 93 registered probes\nrun\n```",
            # The round-17 review's bypass, verbatim: formatting markers in
            # a fenced comment.
            "```bash\n# There are **93 registered probes**.\nrun\n```",
            "```bash\n# The probe registry totals <b>93</b>.\nrun\n```"):
        expect(rules_fired(historical) != set(),
               f"the drift sentence is rejected: {historical[:60]!r}...")

    # --- the accepts the rules must not break ----------------------------
    for benign in (
            "```bash\npython3 tools/run_probes.py --jobs 4\n```",
            "```bash\npython3 tools/run_probes.py --port 9500\n```",
            "```bash\n# Run up to 4 probes concurrently\n"
            "python3 tools/run_probes.py --jobs 4\n```",
            "Two registered probes derive a second listener from `--port`.",
            "Three registered probes legitimately still drive Cabal.",
            # The compound head is named, so an ordinary hyphenated unit is
            # still not a count: a second is not something the registry
            # holds, and neither is a process.
            "Most registered probes use the ordinary 900-second default.",
            "Its manifest-wide 2-process path is slow, and 120-second "
            "timeouts stay available.",
            "`save_compat_migration` uses 3600 seconds because its measured "
            "runtime is above 2300 seconds.",
            "Run everything, sequentially (slow — low tens of minutes).",
            "A span that covers the user's GUI port 8008 is refused.",
            "Concurrency cuts wall-time to roughly `total / N`.",
            "`tools/test_run_probes.py` validates every row against the live "
            "registry.",
            "An overlap-free parallel allocation over the whole registry.",
            "Every registered probe holds `repo-config` in a shared interest.",
            "The runner now resolves the executable ONCE, before a single "
            "probe process exists.",
            "`--retries N` re-runs a failed probe SOLO up to `N` more times.",
            # These two pin the universal subject's LEADING-only position:
            # both put "every probe" downstream of a number.
            "Ctrl-C exits 130 after terminating every probe still running.",
            "`probe_runner_registry.PROBE_PORT_SPANS` declares 2 for each of those two, "
            "and every other probe reserves its base alone.",
            "The runner reaps each probe's process group on EVERY completion "
            "path (#1323) — success, ordinary nonzero exit, timeout.",
            "This is the mode CI's selective gate (`tools/ci_probes.py`, "
            "#530) relies on.",
            "**Shared repository resources (#1322, #1444, #1570).** "
            "`run_probes` declares two tables, which EVERY registered probe "
            "holds in a shared interest.",
            "Before #1571 the allocator used stride 1, so selecting "
            "`debug_console_boot` immediately before `transactional_load` "
            "under `--jobs 2` put both on 9401.",
            # An INDEFINITE, qualified count noun is not the registry: this
            # is the sentence that decides the definite article must sit
            # directly on the noun.
            "A declared count `N` reserves the contiguous span "
            "`base … base + N - 1`, and `--jobs` lays the selected probes' "
            "spans end to end.",
            "Adding a future multi-port probe is one row in that table: "
            "nothing in the allocator knows any probe by name, and "
            "`--jobs 2` still works.",
            "A `--port` that reaches 8008 stays build-free.",
            # A PORT is not something the registry holds, so this stays a
            # sentence about ports even with a count noun two words from a
            # probe and a number fifty characters on.
            "So a probe's port count is DATA -- `probe_runner_registry.PROBE_PORT_SPANS` "
            "declares 2 for each of those two.",
            "`probe_runner_registry.PROBE_TIMEOUT_OVERRIDES` declares 3600 for one key.",
            # An identifier's underscores and an expression's star are not
            # emphasis, so neither dissolves into a fake number.
            "`probe_runner_registry.PROBE_TIMEOUT_OVERRIDES` and "
            "`save_compat_migration` name 3600 between them.",
            "The span is 2*3 wide.",
            # The three subset words the count-noun class must NOT swallow.
            # Each is placed exactly where a count noun would make it a
            # registry subject, with a number in reach.
            "Runs a selection of the probes above, 4 at a time, and prints "
            "a per-probe PASS/FAIL summary.",
            "The runner records every attempt it dispatches in a probe "
            "batch of 4.",
            "It reaps the probe group after 2 seconds.",
            # A possessive survives the quote strip. "own" is not something
            # the registry holds, so this stays a sentence about one probe.
            "The probe's own boot time is 2 seconds.",
            # "sum" is not a count noun either: the section's own sentence
            # puts it two words from a probe and sixty characters from a
            # number, and must stay clean.
            "Total cost is roughly the sum of each probe's own boot + "
            "scenario time, and CI's selective gate (#530) relies on it.",
            # A hyphenated word that merely BEGINS with a count noun is not
            # one -- "manifest-wide" describes a path, not a total.
            "`save_compat_migration` uses 3600 seconds because its "
            "manifest-wide two-process path is slow.",
            # A comparison is not an opening tag, so nothing downstream of
            # it is swallowed as markup.
            "With `--jobs > 1` the spans are laid out from it instead of "
            "from the default `9400`, and a declared count `N` reserves "
            "`base … base + N - 1`.",
            "Adding a future multi-port probe is one row in that table, and "
            "`tools/test_run_probes.py` validates every row against the live "
            "registry.",
            # "1) or so" -- an approximate SUFFIX that does not touch its
            # number, and a number that is not the count of anything.
            "Cap `N` at (cores − 1) or so — each probe is a full engine "
            "process.",
            # A bracket followed by a space is not a link, so nothing after
            # one is swallowed as a destination.
            "```\n#probe-progress# 19:25:04 +0.0s   | phase | engine A\n"
            "#probe-progress# 19:29:11 +247.2s | end   | chop attempt 1/2\n"
            "[1/1] persistence_contract_sweep.py ... [timeout 120s] "
            "TIMEOUT (120.0s)\n```",
            # Fenced identifiers keep their underscores through the same
            # normalization that strips a fenced "**".
            "```bash\npython3 tools/persistence_contract_sweep.py "
            "--jobs 4\npython3 tools/run_probes.py --port 9500\n```",
            "Total cost is roughly the sum of each probe's own boot + "
            "scenario time, and CI's selective gate (#530) relies on it.",
            "`--list` shows the full probe registry but not CI status.",
            "With `--jobs > 1` the spans are laid out from it instead of "
            "from the default `9400`.",
            "`debug_console_boot_probe.py` boots its checks on `--port + 1`.",
            "`save_compat_migration` uses `PROBE_TIMEOUT_OVERRIDES`' 3600 s.",
            "Run up to 4 probes concurrently, each its own engine.",
            "Up to `N` probes run at once, each on its own port span.",
            "This doc states no total of its own: a hand-written one drifted "
            "three times (#539, #721, #1584).",
            "Each of those two probes reserves a second port.",
            # The section's own hardest accepts, in their REAL wrapping: a
            # registry noun and a number in one unwrapped sentence, kept
            # apart only by the rules' character cap and by what the number
            # is glued to.
            "A probe is handed one `--port`, but two\n"
            "registered probes derive a second, concurrently live listener "
            "from it:\n`debug_console_boot_probe.py` boots its "
            "successful-bind and\nwidget-module checks on `--port + 1`, and "
            "`offscreen_probe.py` starts a\nsecond offscreen engine on "
            "`--port + 1` while the first is still up.",
            "- **`--jobs N`, concurrent:** up to `N` probes run at once, "
            "each its own\n  engine on its own reserved port span (#531, "
            "#1571), cutting wall-time to\n  roughly `total / N`, bounded "
            "by the slowest single probe.",
            "**Timeouts are per probe.** Most registered probes use the "
            "ordinary 900-second\ndefault. A scenario whose complete "
            "expected workload structurally exceeds that\nclass declares a "
            "validated key-specific default in\n"
            "`probe_runner_registry.PROBE_TIMEOUT_OVERRIDES`.",
            "The whole plan is computed before the first subprocess exists, "
            "and a span that reaches 8008 is refused (exit 2)."):
        fired = rules_fired(benign)
        expect(fired == set(),
               f"an operational quantity is accepted: {benign[:56]!r} "
               f"(fired {sorted(fired)})")


#: This module's ordered inventory of test groups, in the shape every
#: owner under `tools/probe_runner_tests/` declares (#2130). The
#: `tools/test_run_probes.py` facade places these in its aggregate order
#: and serves `--family readme` from them, so the audit keeps exactly one
#: owner and no caller names its group a second time.
TEST_GROUPS = (
    test_the_readme_states_no_registry_total,
)


def main() -> int:
    selftestlib.parse_verbose()
    test_the_readme_states_no_registry_total()
    if FAILURES:
        print(f"\n{len(FAILURES)} test(s) failed:")
        for failure in FAILURES:
            print(f"  {failure}")
        return selftestlib.concluded(1)
    return selftestlib.concluded(
        0, "\nThe tools/README.md registry-count audit passed")


if __name__ == "__main__":
    raise SystemExit(main())
