#!/usr/bin/env python3
"""Unit tests for run_probes.py's process-group teardown (issue #1323).

Deterministic and GPU-free: every "probe" here is a synthetic script in a
throwaway tree, and every "engine" a synthetic descendant that outlives
it. No Vulkan, no worldgen, no registered probe is ever run. The real
`tools/run_probes.py` is imported and driven, with `REPO_ROOT` and
`PROBES` pointed at the temp tree -- so this exercises the shipped code
paths (`run_one`, `run_with_retry`, `main`) rather than a copy.

The synthetic descendant reproduces exactly what makes the defect
invisible to `communicate()`: `probelib.boot` starts the engine WITHOUT
`start_new_session`, so it inherits the probe's process group, and
redirects its output to a log file rather than the runner's inherited
pipe. The pipe therefore reaches EOF the moment the probe itself exits,
whatever the descendant is still doing.

Covered: ordinary success, ordinary nonzero failure with its output tail
intact, an engine that ignores SIGTERM after an ORDINARY exit (the only
case that reaches the reap's own SIGKILL escalation, and the one that
proves the grace is not charged to the probe's elapsed time), timeout
with the SIGTERM-to-SIGKILL escalation, a shutting-down runner refusing
to launch one more probe, runner interruption (a real SIGINT to a real
runner process), parallel cancellation, an interrupt landing DURING
future submission, a probe launched into an already-starting shutdown,
an interrupt landing in the launch window itself, and a retry rebinding
the port a SIGKILLed engine had held. Also covers `select`/`main`'s
`--exact` unknown-key rejection (issue #1321): a mixed valid/invalid
request, the pre-existing all-invalid empty-selection diagnostic, a
wholly valid request's registry-order and duplicate-collapse behavior,
and substring selection's unchanged permissiveness. And the reserved port
spans (issue #1571): the shipped `PROBE_PORT_SPANS` declaration validated
against the live registry, an overlap-free parallel allocation over the
whole registry and over the exact pair that broke, the GUI-port refusal
covering a span that only REACHES 8008, `--port` basing the parallel
allocation, and a synthetic two-port probe binding real sockets alongside
its neighbour -- with the undeclared-span control that proves the same two
probes collide without the declaration. And the parallel
scheduler's reader/writer serialization (issues #1322 and #1444): probes
stamp their own occupancy windows, so a probe declaring an exclusive
interest is proved never to overlap ANY other probe -- the other config
declaration or an ordinary engine-booting one, in either dispatch order --
while two ordinary probes still overlap each other, across a conflicting
probe that passes, one that fails, and one that times out. And the
`tools/README.md` guard (issue #1584): the `run_probes.py` section states
no probe-registry total, each of the four lexical rules is shown to reject
a crafted violating section and to leave the real file (and its
operational quantities) alone, and a missing or ambiguous target section
fails rather than scanning nothing.

Usage:
  python3 tools/test_run_probes.py
Exit codes: 0 = all tests passed, 1 = one or more failed.
"""
from __future__ import annotations

import ast
import html
import os
import random
import re
import shutil
import signal
import socket
import subprocess
import sys
import tempfile
import textwrap
import threading
import time
import uuid
from pathlib import Path

sys.path.insert(0, str(Path(__file__).resolve().parent))
import probe_engine  # type: ignore
import probe_resource_lock  # type: ignore
import probe_runner_diagnostics  # type: ignore
import probe_runner_lifecycle  # type: ignore
import probe_runner_registry  # type: ignore
import probe_runner_resources  # type: ignore
import probe_runner_scheduler  # type: ignore
import run_probes  # type: ignore

TOOLS_DIR = str(Path(__file__).resolve().parent)
FAILURES: list[str] = []

# Short enough to keep the suite quick, long enough that a correct
# SIGTERM-then-poll escalation is genuinely exercised rather than skipped.
TEST_GRACE = 1.5


def expect(cond: bool, msg: str) -> None:
    if not cond:
        FAILURES.append(msg)
        print(f"  FAIL: {msg}")
    else:
        print(f"  OK:   {msg}")


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


# --------------------------------------------------------------------------
# Synthetic tree
# --------------------------------------------------------------------------
DESCENDANT_SRC = textwrap.dedent("""\
    # Stands in for the engine a probe boots: records its pid so the test
    # can check it is gone, then outlives its parent unless signalled.
    #
    # Given a port it also BINDS it, the way a real engine's debug console
    # does, and appends whether the bind succeeded to a shared log. That
    # log is how a retry proves the port was genuinely released rather
    # than merely signalled -- #1190 aborts a boot that cannot bind.
    import os, signal, socket, sys, time
    pidfile, term_policy = sys.argv[1], sys.argv[2]
    port = int(sys.argv[3]) if len(sys.argv) > 3 else 0
    bindlog = sys.argv[4] if len(sys.argv) > 4 else None
    # Seconds to spend between the ledger write and the handshake write.
    # Zero for every ordinary probe; a test that wants the ORDER above
    # proven rather than asserted sets it (see
    # test_engine_ledger_is_durable_before_the_handshake).
    handshake_delay = float(sys.argv[5]) if len(sys.argv) > 5 else 0.0
    if term_policy == "ignore-term":
        signal.signal(signal.SIGTERM, signal.SIG_IGN)
    held = None
    if port:
        held = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
        try:
            held.bind(("127.0.0.1", port))
            held.listen(4)
            outcome = "bound"
        except OSError:
            outcome = "inuse"
        if bindlog:
            with open(bindlog, "a") as fh:
                print(outcome, file=fh)
                fh.flush()
                os.fsync(fh.fileno())
    # ORDER IS LOAD-BEARING: every record this engine leaves must be
    # durable BEFORE the handshake file, because the handshake is what
    # releases the probe -- and the probe exiting is what fires
    # `run_one`'s `finally: reap_group(pgid)`, which SIGTERMs this
    # process. `.all` (the per-attempt ledger `engine_pids` counts) is
    # therefore written first, and `pidfile` (the single-slot handshake
    # `probe_src` polls) last. Written the other way round the probe can
    # observe pidfile's CONTENT as soon as the write is flushed -- the
    # fsync that follows is not what makes it readable -- and reap this
    # process before it ever reaches `.all`, losing that attempt's line
    # and failing `test_retry_reaps_between_attempts` with one pid where
    # the retry really did boot two. That is not hypothetical: it is a
    # real Linux CI failure, and it stayed invisible on a fast local
    # filesystem where the window is microseconds. The bind log above is
    # already on the safe side of the handshake for the same reason --
    # which is why it stayed intact in that failure while `.all` did not.
    with open(pidfile + ".all", "a") as fh:
        print(os.getpid(), file=fh)
        fh.flush()
        os.fsync(fh.fileno())
    if handshake_delay:
        time.sleep(handshake_delay)
    with open(pidfile, "w") as fh:
        fh.write(str(os.getpid()))
        fh.flush()
        os.fsync(fh.fileno())
    time.sleep(600)
    """)


def probe_src(root: Path, name: str, *, exit_code: int = 0,
              tail_lines: int = 0, hang: bool = False,
              ignore_term: bool = False, engine_ignores_term: bool = False,
              descendant: bool = True, hold_port: int = 0,
              dwell: float = 0.0,
              handshake_delay: float = 0.0,
              bind_span: int = 0, bind_delay: float = 0.0,
              progress: "tuple[tuple[str, str, str], ...]" = (),
              failures: "tuple[tuple[str, str, str], ...]" = (),
              sentinel: str = "") -> str:
    """A synthetic probe that boots a synthetic 'engine' the way probelib does.

    ``ignore_term`` and ``engine_ignores_term`` are independent on purpose:
    an engine that survives SIGTERM while its probe exits NORMALLY is the
    only thing that reaches `reap_group`'s own SIGKILL escalation, which
    the timeout path's separate escalation would otherwise mask.

    ``dwell`` holds the probe alive for that many seconds, and every probe
    stamps ``start``/``end`` wall-clock times into ``<name>.interval`` — a
    measurable occupancy window, which is what lets the scheduler tests
    (#1322) prove which probes overlapped rather than merely that each ran.
    A probe killed before it finishes records only its ``start``.

    ``bind_span`` makes the probe bind its OWN assigned span in-process --
    ``--port`` through ``--port + bind_span - 1`` -- appending "bound" or
    "inuse" per port to ``<name>.binds`` and exiting nonzero if any member
    was taken. That is what a real multi-port probe does
    (`debug_console_boot_probe.py` and `offscreen_probe.py` each keep a
    second listener on ``--port + 1``), and it is what makes an
    overlapping allocation observable rather than merely asserted (#1571).
    ``bind_delay`` holds that bind back, so a test can order two probes'
    binds deterministically instead of racing them.

    Every probe records the ``--port`` it was handed to ``<name>.port``,
    one line per attempt.

    ``progress`` is a sequence of ``(kind, identity, detail)`` triples the
    probe emits, in order, BEFORE its ``tail_lines`` -- through the real
    ``probe_runner_diagnostics.ProgressEmitter``, never a hand-copied string, so these
    cases exercise the shipped convention on both sides (#1768). Emitting
    them first is what lets a case bury them under more than ``--tail``
    ordinary lines and still require the failure presentation to surface
    them.

    ``failures`` is the same for #1982's durable failure records --
    ``(kind, identity, detail)`` triples emitted through the real
    ``probe_runner_diagnostics.FailureEmitter``. They are emitted FLUSHED, before the
    block-buffered ``tail_lines``, which is exactly the displacement the
    real probes suffer: their ``FAIL:`` lines go to an unbuffered stderr
    the runner merges into a piped, block-buffered stdout, so they
    overtake the buffered check output and land at the TOP of the merged
    capture, above whatever the ``--tail`` retains.

    ``sentinel`` is one ordinary line printed FIRST -- a non-diagnostic
    marker a case can require to stay OMITTED, which is what proves the
    failure presentation did not simply dump the complete capture.
    """
    pidfile = root / f"{name}.enginepid"
    startedfile = root / f"{name}.started"
    logfile = root / f"{name}.enginelog"
    interval = root / f"{name}.interval"
    term_policy = "ignore-term" if engine_ignores_term else "obey-term"
    lines = [
        "import argparse, os, signal, socket, subprocess, sys, time",
        # run_probes always appends --port in the parallel path; accept it
        # the way every registered probe does (#723).
        "ap = argparse.ArgumentParser()",
        "ap.add_argument('--port', type=int, default=0)",
        "_args = ap.parse_args()",
        # Appended, not truncated: `--retries` reuses this path, so the
        # port of every attempt is readable, in order.
        f"_pf = open({str(root / (name + '.port'))!r}, 'a')",
        "print(_args.port, file=_pf)",
        "_pf.flush()",
        "_pf.close()",
        # What the runner handed this attempt in the environment (#1570):
        # the resolved engine executable, and the resources an ancestor
        # already holds exclusively on its behalf. One line per attempt,
        # empty when the variable was absent -- which is itself the
        # assertion for a probe that must be left on the direct-invocation
        # fallback.
        f"_ef = open({str(root / (name + '.env'))!r}, 'a')",
        "print(os.environ.get('SYNARCHY_PROBE_ENGINE_EXE', ''),"
        " os.environ.get('SYNARCHY_PROBE_HELD_EXCLUSIVE', ''),"
        " os.environ.get('SYNARCHY_PROBE_HELD_NAMESPACE', ''),"
        " sep='|', file=_ef)",
        "_ef.flush()",
        "_ef.close()",
        f"open({str(startedfile)!r}, 'w').write('1')",
        # Appended, not truncated: --retries reuses these paths, so an
        # attempt count is readable from the file too.
        f"_iv = open({str(interval)!r}, 'a')",
        "def _stamp(kind):",
        "    print(kind, repr(time.time()), file=_iv)",
        "    _iv.flush()",
        "    os.fsync(_iv.fileno())",
        "_stamp('start')",
    ]
    if sentinel:
        lines.append(f"print({sentinel!r})")
    if ignore_term:
        lines.append("signal.signal(signal.SIGTERM, signal.SIG_IGN)")
    if descendant:
        lines += [
            # probelib.boot's exact shape: NO new session (so it lands in
            # this probe's group) and output to a log file, NOT the pipe
            # run_probes is reading -- which is why communicate() returns
            # as soon as this probe exits.
            # Clear the previous attempt's marker first: `--retries` reuses
            # these paths, and waiting on a STALE pidfile would let this
            # probe exit before its own engine had started.
            f"try:\n    os.unlink({str(pidfile)!r})\nexcept OSError:\n    pass",
            f"log = open({str(logfile)!r}, 'w')",
            f"subprocess.Popen([sys.executable, {str(root / '_descendant.py')!r},"
            f" {str(pidfile)!r}, {term_policy!r}, {str(hold_port)!r},"
            f" {str(root / (name + '.binds'))!r}, {str(handshake_delay)!r}],"
            " stdout=log, stderr=subprocess.STDOUT)",
            # Do not exit before the descendant has recorded its pid, or
            # the test would have nothing to look for.
            "deadline = time.time() + 30",
            "while time.time() < deadline:",
            f"    try:",
            f"        if open({str(pidfile)!r}).read().strip():",
            "            break",
            "    except OSError:",
            "        pass",
            "    time.sleep(0.02)",
        ]
    if bind_span:
        # The probe's own span, bound the way a real multi-port probe
        # binds it: base first, then each derived port, every socket kept
        # open for the probe's whole life.
        if bind_delay:
            lines.append(f"time.sleep({bind_delay!r})")
        lines += [
            "_held = []",
            "_clash = False",
            f"_blog = open({str(root / (name + '.binds'))!r}, 'a')",
            f"for _p in range(_args.port, _args.port + {bind_span}):",
            "    _s = socket.socket(socket.AF_INET, socket.SOCK_STREAM)",
            "    try:",
            "        _s.bind(('127.0.0.1', _p))",
            "        _s.listen(4)",
            "        _held.append(_s)",
            "        _outcome = 'bound'",
            "    except OSError:",
            "        _s.close()",
            "        _clash = True",
            "        _outcome = 'inuse'",
            "    print(_outcome, file=_blog)",
            "    _blog.flush()",
            "    os.fsync(_blog.fileno())",
        ]
    if progress:
        # The real module, imported from the REAL tools directory: a
        # synthetic copy of the record format would let the producer and
        # the consumer drift apart without a test noticing.
        lines += [
            f"sys.path.insert(0, {str(Path(__file__).resolve().parent)!r})",
            "import probe_runner_diagnostics as _diag",
            "_progress = _diag.ProgressEmitter()",
        ]
        for kind, identity, detail in progress:
            lines.append(
                f"_progress.emit({kind!r}, {identity!r}, {detail!r})")
    if failures:
        # The real module again, for the same reason: a synthetic copy of
        # the record format would let producer and consumer drift apart
        # without a test noticing (#1982).
        lines += [
            f"sys.path.insert(0, {str(Path(__file__).resolve().parent)!r})",
            "import probe_runner_diagnostics as _diag",
            "_failure = _diag.FailureEmitter('synthetic')",
        ]
        for kind, identity, detail in failures:
            lines.append(
                f"_failure.emit({kind!r}, {identity!r}, {detail!r})")
    for i in range(tail_lines):
        lines.append(f"print('diagnostic line {i}')")
    lines.append("sys.stdout.flush()")
    if dwell:
        lines.append(f"time.sleep({dwell!r})")
    if hang:
        lines.append("time.sleep(600)")
    lines.append("_stamp('end')")
    if bind_span:
        # A clash is the probe's OWN failure, exactly as a real engine's
        # "Address already in use" is; the declared exit code still wins
        # when every port was free.
        lines.append(f"sys.exit(1 if _clash else {exit_code})")
    else:
        lines.append(f"sys.exit({exit_code})")
    return "\n".join(lines) + "\n"


class Tree:
    """A throwaway REPO_ROOT holding synthetic probes under tools/."""

    def __init__(self) -> None:
        self.root = Path(tempfile.mkdtemp(prefix="test_run_probes_"))
        (self.root / "tools").mkdir()
        (self.root / "_descendant.py").write_text(DESCENDANT_SRC)
        self.probes: list[tuple[str, str, str]] = []
        # What the preflight double answers with (#1570). A real file,
        # executable, at an absolute path, so `probe_engine`'s validation
        # runs for real rather than being bypassed.
        self.executable = self.root / "synthetic-synarchy"
        self.executable.write_text("#!/bin/sh\nexit 0\n")
        self.executable.chmod(0o755)

    def env_lines(self, name: str) -> list[tuple[str, str, str]]:
        """`(engine exe, held-exclusive, held-namespace)` per attempt."""
        try:
            raw = (self.root / f"{name}.env").read_text()
        except OSError:
            return []
        out = []
        for line in raw.splitlines():
            parts = line.split("|")
            if len(parts) == 3:
                out.append((parts[0], parts[1], parts[2]))
        return out

    def engine_exes(self, name: str) -> list[str]:
        """The engine executable each attempt of this probe was handed."""
        return [exe for exe, _, _ in self.env_lines(name)]

    def add(self, name: str, **kw) -> str:
        script = f"{name}_probe.py"
        (self.root / "tools" / script).write_text(
            probe_src(self.root, name, **kw))
        self.probes.append((name, script, f"synthetic {name}"))
        return script

    def engine_pid(self, name: str) -> int | None:
        try:
            raw = (self.root / f"{name}.enginepid").read_text().strip()
        except OSError:
            return None
        return int(raw) if raw else None

    def started(self, name: str) -> bool:
        return (self.root / f"{name}.started").exists()

    def engine_pids(self, name: str) -> list[int]:
        """Every engine pid this probe booted, one per attempt."""
        try:
            raw = (self.root / f"{name}.enginepid.all").read_text()
        except OSError:
            return []
        return [int(tok) for tok in raw.split() if tok.isdigit()]

    def intervals(self, name: str) -> list[tuple[float, float | None]]:
        """This probe's occupancy windows, one per attempt, in start order.

        The end is None for an attempt killed before it could stamp one
        (a timeout, or an interrupt) — the window is still open-ended, not
        absent, so a caller can still order it against another probe.
        """
        try:
            raw = (self.root / f"{name}.interval").read_text()
        except OSError:
            return []
        windows: list[list[float | None]] = []
        for line in raw.splitlines():
            parts = line.split()
            if len(parts) != 2:
                continue
            kind, stamp = parts[0], float(parts[1])
            if kind == "start":
                windows.append([stamp, None])
            elif kind == "end" and windows:
                windows[-1][1] = stamp
        return [(w[0], w[1]) for w in windows]

    def window(self, name: str) -> tuple[float, float | None]:
        """This probe's single occupancy window; raises if it did not run once."""
        got = self.intervals(name)
        assert len(got) == 1, f"{name} recorded {len(got)} window(s), expected 1"
        return got[0]

    def ports(self, name: str) -> list[int]:
        """The `--port` this probe was handed, one per attempt, in order."""
        try:
            raw = (self.root / f"{name}.port").read_text()
        except OSError:
            return []
        return [int(tok) for tok in raw.split() if tok.lstrip("-").isdigit()]

    def binds(self, name: str) -> list[str]:
        """One entry per attempt: "bound" or "inuse"."""
        try:
            raw = (self.root / f"{name}.binds").read_text()
        except OSError:
            return []
        return raw.split()

    def cleanup(self) -> None:
        # Belt and braces: never leave a synthetic descendant behind even
        # if an assertion failed before the runner reaped it.
        for name, _, _ in self.probes:
            for pid in self.engine_pids(name) or ([self.engine_pid(name)]
                                                  if self.engine_pid(name) else []):
                try:
                    os.kill(pid, signal.SIGKILL)
                except OSError:
                    pass
        shutil.rmtree(self.root, ignore_errors=True)


def process_state(pid: int) -> str | None:
    """The one-letter process state, or None when it cannot be read.

    Linux publishes it in /proc/<pid>/stat. That field follows the comm,
    which is parenthesized and may itself contain spaces and ')', so it is
    read from the LAST ')' rather than by splitting the whole line. macOS
    has no /proc, so `ps` answers there.
    """
    try:
        with open(f"/proc/{pid}/stat") as fh:
            raw = fh.read()
        return raw[raw.rindex(")") + 1:].split()[0]
    except (OSError, ValueError, IndexError):
        pass
    try:
        done = subprocess.run(["ps", "-o", "state=", "-p", str(pid)],
                              capture_output=True, text=True, timeout=15)
    except (OSError, subprocess.SubprocessError):
        return None
    state = done.stdout.strip()
    return state[0] if state else None


def pid_alive(pid: int) -> bool:
    """True while `pid` names a process that has not yet exited.

    A ZOMBIE does not count. It has already died and is only waiting for a
    parent that will never wait() for it -- which is precisely the state a
    correctly killed synthetic engine ends up in here, because it is
    orphaned the moment its probe exits. `os.kill(pid, 0)` cannot tell the
    two apart and reports a zombie as living, so on its own it fails every
    "the engine is gone" assertion in this suite wherever orphans are not
    reaped promptly: green on macOS, where launchd reaps at once, and red
    in a CI container whose PID 1 does not reap at all.

    An unreadable state is treated as alive, so a broken `process_state`
    fails these assertions rather than passing them vacuously.
    """
    try:
        os.kill(pid, 0)
    except ProcessLookupError:
        return False
    except PermissionError:
        return True
    return process_state(pid) != "Z"


def wait_pid_gone(pid: int, seconds: float = 15.0) -> bool:
    deadline = time.monotonic() + seconds
    while time.monotonic() < deadline:
        if not pid_alive(pid):
            return True
        time.sleep(0.05)
    return not pid_alive(pid)


def wait_file(path: Path, seconds: float = 60.0) -> bool:
    deadline = time.monotonic() + seconds
    while time.monotonic() < deadline:
        if path.exists() and path.read_text().strip():
            return True
        time.sleep(0.05)
    return False


class PreflightRecorder:
    """A deterministic stand-in for the preflight's subprocess entry point.

    `probe_runner_resources.engine_preflight` makes the ONE Cabal contact an aggregate
    run is allowed (#1570): a freshness `cabal build` and then a read-only
    `cabal list-bin`. This answers both without a toolchain — the build
    succeeds silently, the query prints the synthetic tree's own
    executable — and records every argv it was handed, with the wall-clock
    instant of each call.

    The RECORD is the point. Counting the calls is what proves the
    preflight happened exactly once for a whole sweep, and timing them
    against the probes' own `<name>.interval` stamps is what proves it
    finished before any probe process existed, rather than merely that it
    happened.

    `fail` makes the named step exit nonzero, which is how the
    failure-before-anything-spawns case is driven.
    """

    def __init__(self, executable, *, fail: str | None = None,
                 message: str = "synthetic preflight failure") -> None:
        self.executable = str(executable)
        self.fail = fail
        self.message = message
        self.calls: list[tuple[tuple[str, ...], float]] = []

    def __call__(self, argv, cwd=None, capture_output=False, text=False):
        argv = tuple(argv)
        self.calls.append((argv, time.time()))
        step = "build" if "build" in argv else "locate"
        if self.fail == step:
            return subprocess.CompletedProcess(argv, 1, "", self.message)
        stdout = "" if step == "build" else f"{self.executable}\n"
        return subprocess.CompletedProcess(argv, 0, stdout, "")

    @property
    def argvs(self) -> list[tuple[str, ...]]:
        return [argv for argv, _ in self.calls]

    @property
    def finished_at(self) -> float:
        """When the LAST preflight call was made; 0.0 if none was."""
        return max((when for _, when in self.calls), default=0.0)


class patched:
    """Point the real runner at the synthetic tree for one test.

    The cross-process resource namespace (#1436) is redirected to a
    SYNTHETIC token as well, and that is not cosmetic: the synthetic
    probes are named `config_migration` and `config_state`, so under the
    real namespace this suite would take the repository's real EXCLUSIVE
    `repo-config` lock and stall — or be stalled by — a genuine probe
    sweep or `/deflake` measurement running beside it. A token per test
    also keeps the suite's own cases from coordinating with each other.
    """

    def __init__(self, tree: Tree, grace: float = TEST_GRACE,
                 namespace: str | None = None,
                 spans: dict[str, int] | None = None,
                 timeouts: dict[str, float] | None = None,
                 preflight: "PreflightRecorder | None" = None) -> None:
        self.tree, self.grace = tree, grace
        # The engine-executable preflight (#1570) is doubled for the same
        # reason the namespace is: a synthetic tree is no Cabal project,
        # and a real `cabal build` here would make every main()-driven
        # case depend on a toolchain and a warm build directory.
        self.preflight = (PreflightRecorder(tree.executable)
                          if preflight is None else preflight)
        self.namespace = namespace or f"selftest{uuid.uuid4().hex[:12]}"
        # The synthetic probes have synthetic keys, so the shipped
        # `PROBE_PORT_SPANS` says nothing about them. A test declaring a
        # multi-port synthetic probe substitutes its own table (#1571);
        # passing none leaves every synthetic probe at the default one
        # port, which is what the shipped table would give them anyway.
        self.spans = {} if spans is None else dict(spans)
        # The shipped timeout table names shipped keys. Synthetic registries
        # start with no exceptions unless a case supplies its own declarations.
        self.timeouts = {} if timeouts is None else dict(timeouts)

    def __enter__(self):
        self._saved = (probe_engine.REPO_ROOT, probe_runner_registry.PROBES,
                       probe_runner_lifecycle.GROUP_GRACE, probe_runner_resources.RESOURCE_NAMESPACE,
                       probe_runner_registry.PROBE_PORT_SPANS,
                       probe_runner_registry.PROBE_TIMEOUT_OVERRIDES,
                       probe_runner_resources.ENGINE_EXECUTABLE,
                       probe_runner_resources.ENGINE_PREFLIGHT_RUNNER)
        # An operator's own export of the runner's variables must not
        # decide what a case here observes; `main` re-derives all of them.
        self._saved_env = {name: os.environ.pop(name, None)
                           for name in probe_runner_resources.RUNNER_ENV_VARS}
        probe_engine.REPO_ROOT = str(self.tree.root)
        probe_runner_registry.PROBES = list(self.tree.probes)
        probe_runner_lifecycle.GROUP_GRACE = self.grace
        probe_runner_resources.RESOURCE_NAMESPACE = self.namespace
        probe_runner_registry.PROBE_PORT_SPANS = self.spans
        probe_runner_registry.PROBE_TIMEOUT_OVERRIDES = self.timeouts
        probe_runner_resources.ENGINE_EXECUTABLE = None
        probe_runner_resources.ENGINE_PREFLIGHT_RUNNER = self.preflight
        return self

    def __exit__(self, *exc):
        (probe_engine.REPO_ROOT, probe_runner_registry.PROBES, probe_runner_lifecycle.GROUP_GRACE,
         probe_runner_resources.RESOURCE_NAMESPACE,
         probe_runner_registry.PROBE_PORT_SPANS,
         probe_runner_registry.PROBE_TIMEOUT_OVERRIDES,
         probe_runner_resources.ENGINE_EXECUTABLE,
         probe_runner_resources.ENGINE_PREFLIGHT_RUNNER) = self._saved
        for name, value in self._saved_env.items():
            if value is None:
                os.environ.pop(name, None)
            else:
                os.environ[name] = value
        clear_namespace(self.namespace)
        return False


def clear_namespace(namespace: str) -> None:
    """Remove one synthetic namespace's lock and note files from /tmp.

    Scoped to the literal `synarchy-probe-resource-<namespace>-` prefix of
    a token this suite minted, and run only once every hold it took has
    been released, so nothing another process could be holding is touched.
    The real runner never unlinks a lock file — see
    `probe_resource_lock`'s module docstring for why — but the tokens here
    are per-test and would otherwise accumulate in /tmp forever.
    """
    prefix = f"{probe_resource_lock.SHARED_PREFIX}-{namespace}-"
    try:
        entries = list(probe_resource_lock.LOCK_ROOT.glob(f"{prefix}*"))
    except OSError:
        return
    for entry in entries:
        if not entry.name.startswith(prefix):
            continue
        try:
            entry.unlink()
        except OSError:
            pass


# --------------------------------------------------------------------------
# run_one: every completion path reaps the group
# --------------------------------------------------------------------------
def test_success_reaps_the_engine() -> None:
    print("\n-- an ordinary SUCCESS still reaps the probe's engine")
    tree = Tree()
    try:
        script = tree.add("success", exit_code=0)
        with patched(tree):
            ok, timed_out, elapsed, out = probe_runner_lifecycle.run_one(script, None, 120.0)
        pid = tree.engine_pid("success")
        expect(ok is True, "a zero-exit probe is still reported ok")
        expect(timed_out is False, "a zero-exit probe is not reported as timed out")
        expect(pid is not None, "the synthetic engine recorded its pid")
        expect(pid is not None and wait_pid_gone(pid),
               "the engine a passing probe left running is gone once run_one returns")
    finally:
        tree.cleanup()


def test_failure_reaps_the_engine_and_keeps_the_tail() -> None:
    print("\n-- an ordinary NONZERO exit reaps the engine, tail intact")
    tree = Tree()
    try:
        script = tree.add("failure", exit_code=1, tail_lines=30)
        with patched(tree):
            ok, timed_out, elapsed, out = probe_runner_lifecycle.run_one(script, None, 120.0)
        pid = tree.engine_pid("failure")
        expect(ok is False, "a nonzero-exit probe is still reported as failed")
        expect(timed_out is False, "a plain failure is not reported as a timeout")
        expect("diagnostic line 0" in out and "diagnostic line 29" in out,
               "the whole captured output survives teardown, first line to last")
        expect(pid is not None and wait_pid_gone(pid),
               "the engine a FAILING probe left running is gone once run_one returns")
    finally:
        tree.cleanup()


def test_clean_probe_is_unaffected() -> None:
    print("\n-- reaping an already-empty group changes nothing")
    tree = Tree()
    try:
        script = tree.add("clean", exit_code=0, tail_lines=3, descendant=False)
        with patched(tree):
            ok, timed_out, elapsed, out = probe_runner_lifecycle.run_one(script, None, 120.0)
        expect(ok is True, "a probe that tore down its own engine still passes")
        expect("diagnostic line 2" in out, "its output is untouched")
        # The grace is only ever spent on a group that is still alive, so a
        # clean probe must not pay it.
        expect(elapsed < TEST_GRACE,
               f"its elapsed time excludes the teardown grace (got {elapsed:.2f}s)")
    finally:
        tree.cleanup()


def test_timeout_escalates_to_sigkill() -> None:
    print("\n-- a TIMEOUT escalates SIGTERM -> SIGKILL across the whole group")
    tree = Tree()
    try:
        # Both the probe and its engine ignore SIGTERM, so only the
        # escalation can end them.
        script = tree.add("stuck", hang=True, ignore_term=True,
                          engine_ignores_term=True, exit_code=0)
        with patched(tree):
            ok, timed_out, elapsed, out = probe_runner_lifecycle.run_one(script, None, 3.0)
        pid = tree.engine_pid("stuck")
        expect(timed_out is True, "the hung probe is reported as a timeout")
        expect(ok is False, "a timed-out probe is not reported as ok")
        expect(pid is not None and wait_pid_gone(pid),
               "a SIGTERM-ignoring engine is SIGKILLed rather than left running")
    finally:
        tree.cleanup()


def test_stubborn_engine_is_killed_without_charging_the_probe() -> None:
    print("\n-- an engine that ignores SIGTERM is SIGKILLed, off the probe's clock")
    tree = Tree()
    try:
        # The probe exits NORMALLY, so the timeout path never runs: only
        # the after-every-completion reap can end this engine, and only by
        # escalating past the SIGTERM it ignores.
        script = tree.add("stubborn", exit_code=0, engine_ignores_term=True)
        with patched(tree):
            ok, timed_out, elapsed, out = probe_runner_lifecycle.run_one(script, None, 120.0)
        pid = tree.engine_pid("stubborn")
        expect(ok is True, "the probe itself is still reported as passing")
        expect(timed_out is False, "and not as a timeout")
        expect(pid is not None and wait_pid_gone(pid),
               "the SIGTERM-ignoring engine is SIGKILLed after the grace")
        # The correction to requirement 5: teardown may spend the grace,
        # but the probe's recorded elapsed time is the probe's own.
        expect(elapsed < TEST_GRACE,
               f"the grace spent killing it is NOT charged to the probe's "
               f"elapsed time (got {elapsed:.2f}s, grace {TEST_GRACE}s)")
    finally:
        tree.cleanup()


def test_a_stopping_runner_launches_no_further_probe() -> None:
    print("\n-- a shutting-down runner does not launch one more probe")
    # `fut.cancel()` cannot stop a work item a free worker has ALREADY
    # picked up, so the stop flag has to be checked where the process is
    # actually spawned. Asserted here directly: through the executor it
    # would depend on winning a race against cancel().
    tree = Tree()
    try:
        script = tree.add("never_run", exit_code=0)
        groups = probe_runner_lifecycle.ProbeGroups()
        groups.stopping.set()
        with patched(tree):
            ok, timed_out, elapsed, out = probe_runner_lifecycle.run_one(
                script, 9401, 120.0, groups)
        expect(tree.started("never_run") is False,
               "no probe process was spawned at all")
        expect(tree.engine_pid("never_run") is None,
               "and therefore no engine was booted")
        expect(ok is False and timed_out is False,
               "the un-run probe is reported as neither ok nor timed out")
        # NEVER LAUNCHED, not launched-and-killed: the shutdown branch
        # inside run_one would also end this probe, a moment later and
        # after a real Popen, which these two distinguish.
        expect(elapsed == 0.0,
               f"no probe was timed because none ran (got {elapsed!r})")
        expect("not started" in out,
               f"and it reports itself as never started (got {out!r})")
    finally:
        tree.cleanup()


def test_reap_group_on_a_dead_group_is_a_noop() -> None:
    print("\n-- reaping a group that is already gone is success, not an error")
    # A pid nothing can be running under: our own group with every member
    # already reaped is impossible to construct portably, so use a fresh
    # child we have already waited on.
    proc = subprocess.Popen([sys.executable, "-c", "pass"], start_new_session=True)
    proc.wait()
    started = time.monotonic()
    try:
        probe_runner_lifecycle.reap_group(proc.pid, grace=TEST_GRACE)
        raised = None
    except Exception as exc:  # pragma: no cover - the assertion reports it
        raised = exc
    expect(raised is None, f"reap_group on an empty group raises nothing (got {raised!r})")
    expect(time.monotonic() - started < TEST_GRACE,
           "and returns immediately rather than spending the grace period")
    expect(probe_runner_lifecycle._group_alive(proc.pid) is False,
           "_group_alive reports an empty group as gone")


# --------------------------------------------------------------------------
# Aggregate behaviour: statuses and the aggregate exit code are unchanged
# --------------------------------------------------------------------------
def _main_with_open(tree: Tree, argv: list[str]) -> tuple[int, str]:
    """`_main_with`'s body WITHOUT entering `patched`.

    The cross-process cases hold `patched` open across a background
    thread, so they cannot have it entered a second time underneath them:
    the namespace would be restored by the inner exit while the sweep was
    still running.
    """
    import io
    import contextlib as _contextlib
    buf = io.StringIO()
    saved = sys.argv
    sys.argv = ["run_probes.py", *argv]
    try:
        with _contextlib.redirect_stdout(buf), _contextlib.redirect_stderr(buf):
            rc = run_probes.main()
    finally:
        sys.argv = saved
    return rc, buf.getvalue()


def _main_with(tree: Tree, argv: list[str],
               spans: dict[str, int] | None = None,
               timeouts: dict[str, float] | None = None) -> tuple[int, str]:
    import io
    import contextlib
    buf = io.StringIO()
    saved_argv = sys.argv
    sys.argv = ["run_probes.py"] + argv
    try:
        with patched(tree, spans=spans, timeouts=timeouts), \
             contextlib.redirect_stdout(buf), \
             contextlib.redirect_stderr(buf):
            rc = run_probes.main()
    finally:
        sys.argv = saved_argv
    return rc, buf.getvalue()


def _main_refusal(tree: Tree, argv: list[str],
                  spans: dict[str, int] | None = None) -> tuple[int, str]:
    """`_main_with`, but reporting a `sys.exit` refusal as its own code.

    `main` refuses a bad port plan two ways: `sys.exit(message)` for the
    base-is-the-GUI-port case, which predates #1571, and `return 2` for
    the span-aware plan check. A test asserting the refusal happened at
    all should not have to know which.
    """
    try:
        return _main_with(tree, argv, spans=spans)
    except SystemExit as leaving:
        code = leaving.code
        return (1 if isinstance(code, str) else (code or 0),
                code if isinstance(code, str) else "")


def test_aggregate_exit_codes_unchanged() -> None:
    print("\n-- PASS/FAIL reporting and the aggregate exit code are unchanged")
    tree = Tree()
    try:
        tree.add("good", exit_code=0)
        rc, out = _main_with(tree, ["--only", "good", "--exact"])
        expect(rc == 0, f"an all-passing selection still exits 0 (got {rc})")
        expect("PASS" in out, "and reports PASS")
    finally:
        tree.cleanup()

    tree = Tree()
    try:
        tree.add("bad", exit_code=1, tail_lines=5)
        rc, out = _main_with(tree, ["--only", "bad", "--exact"])
        expect(rc == 1, f"a failing selection still exits 1 (got {rc})")
        expect("FAIL" in out, "and reports FAIL")
        expect("diagnostic line 4" in out,
               "and still prints the failing probe's output tail")
    finally:
        tree.cleanup()


def test_timeout_overrides_are_validated_registry_data() -> None:
    print("\n-- per-probe timeout defaults are validated registry data")
    expect(probe_runner_registry.timeout_override_problems() == [],
           "the shipped timeout declarations are valid")
    expect(probe_runner_registry.effective_timeout("save_compat_migration") == 3600.0,
           "save_compat_migration receives its declared 3600s default")
    expect(probe_runner_registry.effective_timeout("movement")
           == probe_runner_registry.DEFAULT_TIMEOUT,
           "an ordinary registered probe keeps the shared default")
    expect(probe_runner_registry.effective_timeout("save_compat_migration", 17.0) == 17.0,
           "an explicit CLI value wins over the key-specific default")

    unknown = probe_runner_registry.timeout_override_problems(
        overrides={"not_registered": 1.0})
    expect(any("unknown probe key" in problem for problem in unknown),
           f"an unknown declaration is rejected ({unknown})")
    for bad in (0, -1, float("inf"), float("nan"), True, "900"):
        problems = probe_runner_registry.timeout_override_problems(
            overrides={"movement": bad})
        expect(any("finite and positive" in problem for problem in problems),
               f"an unusable timeout {bad!r} is rejected ({problems})")

    tree = Tree()
    try:
        tree.add("ordinary", exit_code=0)
        for bad in ("0", "-1", "nan", "inf"):
            rc, out = _main_with(tree, ["--timeout", bad])
            expect(rc == 2 and "finite and positive" in out,
                   f"CLI --timeout {bad!r} is rejected before execution ({out!r})")
        expect(not tree.started("ordinary"),
               "no probe starts for an invalid explicit timeout")
    finally:
        tree.cleanup()


def test_key_specific_timeout_and_explicit_override_reach_execution() -> None:
    print("\n-- key-specific defaults reach execution and explicit CLI wins")
    tree = Tree()
    try:
        tree.add("slow", dwell=0.25, descendant=False)
        rc, out = _main_with(
            tree, ["--only", "slow", "--exact"],
            timeouts={"slow": 0.05})
        expect(rc == 1 and "TIMEOUT" in out,
               f"the short key-specific default terminates the probe ({out!r})")
        expect("timeout 0.05s" in out,
               f"the effective key-specific budget is reported ({out!r})")
    finally:
        tree.cleanup()

    tree = Tree()
    try:
        tree.add("slow", dwell=0.25, descendant=False)
        rc, out = _main_with(
            tree, ["--only", "slow", "--exact", "--timeout", "2"],
            timeouts={"slow": 0.05})
        expect(rc == 0 and "PASS" in out,
               f"an explicit larger budget overrides the default ({out!r})")
        expect("timeout 2s" in out,
               f"the explicit effective budget is reported ({out!r})")
    finally:
        tree.cleanup()


def test_parallel_retry_reuses_the_key_specific_timeout() -> None:
    print("\n-- a parallel attempt and its solo retry share the key budget")
    tree = Tree()
    try:
        tree.add("slow", dwell=0.25, descendant=False)
        rc, out = _main_with(
            tree,
            ["--only", "slow", "--exact", "--jobs", "2", "--retries", "1"],
            timeouts={"slow": 0.05})
        expect(rc == 1 and out.count("timeout 0.05s") >= 2,
               f"both attempts report the same key-specific budget ({out!r})")
        expect("solo retry 1/1" in out and "TIMEOUT" in out,
               f"the failed parallel attempt reached its solo retry ({out!r})")
    finally:
        tree.cleanup()


# --------------------------------------------------------------------------
# Parallel scheduling honours the reader/writer resource model (#1322, #1444)
#
# These drive the SHIPPED `probe_runner_resources.EXCLUSIVE_RESOURCES` and
# `IMPLICIT_SHARED_RESOURCES` -- only `PROBES` and `REPO_ROOT` are redirected
# at the synthetic tree, and the synthetic probes are deliberately named
# `config_migration` and `config_state` so the real declaration is what
# decides. A test-local conflict table would prove the mechanism while
# leaving the two probes the issue is about unguarded.
#
# `unrelated_a`/`unrelated_b` stand in for the ~85 probes that declare
# nothing and simply boot an engine in the shared checkout. Under #1444
# those hold `repo-config` SHARED, so they overlap each other freely and
# neither of them may overlap a config probe.
# --------------------------------------------------------------------------
def overlaps(a: tuple[float, float | None],
             b: tuple[float, float | None]) -> bool:
    """True when two occupancy windows share any instant.

    An open-ended window (a probe killed before it stamped its end) is
    treated as running until now, which is the conservative reading: it
    can only make a missed overlap MORE likely to be reported, never less.
    """
    a_end = a[1] if a[1] is not None else time.time()
    b_end = b[1] if b[1] is not None else time.time()
    return a[0] < b_end and b[0] < a_end


def progress_lines(out: str, script: str) -> int:
    """How many times the runner announced a verdict for `script`."""
    return sum(1 for line in out.splitlines()
               if line.lstrip().startswith("[") and f" {script} ... " in line)


def test_declared_conflicts_never_overlap() -> None:
    print("\n-- --jobs runs an exclusive probe alone, and does not serialize "
          "the probes that declare nothing")
    tree = Tree()
    try:
        # No descendant: this is about SCHEDULING, and an extra process per
        # probe only adds teardown jitter to the windows being compared.
        #
        # DISPATCH ORDER A: both exclusive probes are dispatched before any
        # ordinary one, so this is the direction where the barrier must hold
        # NEW work back. Three jobs against four probes means a scheduler
        # that only serialized the two config declarations against each
        # other would happily run `unrelated_a` and `unrelated_b` alongside
        # `config_migration` -- which is exactly #1444's defect.
        tree.add("config_migration", dwell=0.8, descendant=False)
        tree.add("config_state", dwell=0.8, descendant=False)
        tree.add("unrelated_a", dwell=0.8, descendant=False)
        tree.add("unrelated_b", dwell=0.8, descendant=False)
        rc, out = _main_with(tree, ["--jobs", "3"])
        expect(rc == 0, f"every probe still passes (exit {rc})")
        expect("4/4 passed" in out, "and the aggregate summary counts all four")

        # Exactly once each: a naive lock inside the worker would still run
        # them all, so this is the floor the overlap checks build on.
        names = ("config_migration", "config_state", "unrelated_a", "unrelated_b")
        for name in names:
            got = tree.intervals(name)
            expect(len(got) == 1 and got[0][1] is not None,
                   f"{name} ran exactly once and to completion "
                   f"(windows: {got})")
            expect(progress_lines(out, f"{name}_probe.py") == 1,
                   f"and the runner reported {name}'s verdict exactly once")

        migration = tree.window("config_migration")
        state = tree.window("config_state")
        first = tree.window("unrelated_a")
        second = tree.window("unrelated_b")
        expect(not overlaps(migration, state),
               f"the two declared-conflicting probes never overlap "
               f"(migration {migration}, state {state})")
        # Requirement 1: no OTHER probe's engine may boot while either
        # config probe runs, whether or not it declares anything.
        for label, solo in (("config_migration", migration),
                            ("config_state", state)):
            for other_label, other in (("unrelated_a", first),
                                       ("unrelated_b", second)):
                expect(not overlaps(solo, other),
                       f"{other_label} never overlaps {label} "
                       f"({other_label} {other}, {label} {solo})")
        # Requirement 3: the declaration must not cost the undeclared
        # probes their concurrency -- the suite is not serialized wholesale.
        expect(overlaps(first, second),
               f"two probes declaring nothing still run concurrently "
               f"(unrelated_a {first}, unrelated_b {second})")
    finally:
        tree.cleanup()


def test_a_solo_probe_waits_for_work_already_running() -> None:
    print("\n-- an exclusive probe waits for running work, without parking "
          "in a worker slot")
    tree = Tree()
    try:
        # DISPATCH ORDER B, the mirror of the test above: ordinary work is
        # already running when the exclusive probe becomes dispatchable, so
        # this is the direction where the barrier must hold the CONFIG probe
        # back. A scheduler that only blocked new work during a solo probe
        # would let `config_migration` start alongside `unrelated_a` here.
        #
        # Putting it in the MIDDLE of the registry order also proves it
        # yields its worker slot: `unrelated_b` is dispatched behind it,
        # into a two-job run, which can only happen if the blocked probe was
        # skipped rather than submitted and parked on a lock.
        tree.add("unrelated_a", dwell=1.0, descendant=False)
        tree.add("config_migration", dwell=0.5, descendant=False)
        tree.add("unrelated_b", dwell=1.0, descendant=False)
        rc, out = _main_with(tree, ["--jobs", "2"])
        expect(rc == 0, f"every probe still passes (exit {rc})")
        expect("3/3 passed" in out, "and the aggregate summary counts all three")

        first = tree.window("unrelated_a")
        migration = tree.window("config_migration")
        second = tree.window("unrelated_b")
        expect(first[1] is not None and second[1] is not None,
               f"both undeclared probes ran to completion "
               f"(unrelated_a {first}, unrelated_b {second})")
        expect(overlaps(first, second),
               f"the blocked probe yielded its slot: the two undeclared "
               f"probes ran concurrently (unrelated_a {first}, "
               f"unrelated_b {second})")
        expect(not overlaps(migration, first) and not overlaps(migration, second),
               f"and the exclusive probe overlapped neither "
               f"(migration {migration}, unrelated_a {first}, "
               f"unrelated_b {second})")
        expect(migration[0] >= max(first[1], second[1]),
               f"it started only after both were reaped "
               f"(migration start {migration[0]}, latest end "
               f"{max(first[1], second[1])})")
    finally:
        tree.cleanup()


def test_conflict_is_released_after_a_failure() -> None:
    print("\n-- a FAILING exclusive probe still releases both interests")
    tree = Tree()
    try:
        tree.add("config_migration", exit_code=1, tail_lines=3,
                 dwell=0.6, descendant=False)
        tree.add("config_state", dwell=0.6, descendant=False)
        # The undeclared probe is here because the barrier it waits on is
        # the SHARED half of the ledger, released by the same code path but
        # counted separately -- a release that dropped only the exclusive
        # set would still pass the two-config-probe check below.
        tree.add("unrelated", dwell=0.6, descendant=False)
        rc, out = _main_with(tree, ["--jobs", "3"])
        expect(rc == 1, f"the failing selection still exits 1 (got {rc})")
        expect("FAIL" in out and "PASS" in out,
               "and every probe reports its own verdict")
        expect("2/3 passed" in out,
               "with the aggregate counting the two that passed")
        migration = tree.window("config_migration")
        state = tree.window("config_state")
        other = tree.window("unrelated")
        expect(state[0] >= migration[0] and other[0] >= migration[0],
               "the other probes waited for the failing one")
        expect(not overlaps(migration, state) and not overlaps(migration, other),
               f"and neither overlapped it (migration {migration}, "
               f"state {state}, unrelated {other})")
    finally:
        tree.cleanup()


def test_conflict_is_released_after_a_timeout() -> None:
    print("\n-- a TIMED-OUT exclusive probe still releases both interests")
    tree = Tree()
    try:
        # Hangs until the runner's own --timeout kills it; the other probes
        # can only start after that, which is what the gaps below measure.
        tree.add("config_migration", hang=True, descendant=False)
        tree.add("config_state", dwell=0.3, descendant=False)
        tree.add("unrelated", dwell=0.3, descendant=False)
        rc, out = _main_with(tree, ["--jobs", "3", "--timeout", "2"])
        expect(rc == 1, f"the timed-out selection exits 1 (got {rc})")
        expect("TIMEOUT" in out, "the hanging probe is reported as a TIMEOUT")
        expect(progress_lines(out, "config_state_probe.py") == 1
               and progress_lines(out, "unrelated_probe.py") == 1
               and "PASS" in out,
               "and both waiting probes still ran and reported PASS")
        migration = tree.window("config_migration")
        for name in ("config_state", "unrelated"):
            waited = tree.window(name)
            expect(waited[0] - migration[0] >= 1.5,
                   f"{name} started only after the timeout fired, not "
                   f"alongside it (gap {waited[0] - migration[0]:.2f}s of a "
                   f"2s timeout)")
        expect(migration[1] is None,
               "and the hanging probe never completed on its own")
    finally:
        tree.cleanup()


def test_exclusive_resource_declaration_is_data_about_real_probes() -> None:
    print("\n-- the shipped EXCLUSIVE_RESOURCES table names registered probes")
    known = {p[0] for p in probe_runner_registry.PROBES}
    unknown = sorted(k for k in probe_runner_resources.EXCLUSIVE_RESOURCES if k not in known)
    expect(not unknown,
           f"every declared key names a registered probe (unknown: {unknown})")
    empty = sorted(k for k, v in probe_runner_resources.EXCLUSIVE_RESOURCES.items() if not v)
    expect(not empty,
           f"and every declaration names at least one resource (empty: {empty})")
    both = (probe_runner_resources.exclusive_resources("config_migration")
            & probe_runner_resources.exclusive_resources("config_state"))
    expect(bool(both),
           f"the two config probes still declare an intersecting resource "
           f"(shared: {sorted(both)})")
    expect(not probe_runner_resources.exclusive_resources("combat_anim"),
           "an undeclared probe needs nothing exclusively")


def test_every_probe_declares_what_an_exclusive_holder_takes() -> None:
    print("\n-- the shipped declaration serializes EVERY exclusive holder "
          "against the whole registry (#1444, #1570)")
    expect(bool(probe_runner_resources.IMPLICIT_SHARED_RESOURCES),
           "there is an implicit shared interest at all "
           f"(got {probe_runner_resources.IMPLICIT_SHARED_RESOURCES!r})")
    declared = set(probe_runner_resources.EXCLUSIVE_RESOURCES)
    config_probes = {"config_migration", "config_state"}
    expect(config_probes <= declared,
           f"both config probes are still exclusive holders (#1322/#1444) "
           f"(declared: {sorted(declared)})")
    for key in sorted(config_probes):
        expect("repo-config" in probe_runner_resources.exclusive_resources(key),
               f"{key} still takes repo-config exclusively")
    # The three probes that still drive Cabal themselves -- a `cabal repl`
    # through persistence_snapshot / save_compat_audit, which is NOT an
    # engine boot and has no prebuilt equivalent (#1570).
    ghci = {"persistence_contract", "persistence_contract_sweep",
            "save_compat_migration"}
    expect(ghci <= declared,
           f"every GHCi consumer is an exclusive holder too "
           f"(missing: {sorted(ghci - declared)})")
    for key in sorted(ghci):
        expect("cabal-build" in probe_runner_resources.exclusive_resources(key),
               f"{key} takes the shared Cabal build state exclusively")
    for key in sorted(declared):
        # An interest is one or the other, never both, or a release would
        # drop the exclusive half and leave the shared count behind.
        overlap = (probe_runner_resources.exclusive_resources(key)
                   & probe_runner_resources.shared_resources(key))
        expect(not overlap,
               f"{key} holds no resource in both interests "
               f"(both: {sorted(overlap)})")
    # The whole registry, per holder, not a sample: this is what makes
    # "nothing else may run beside an exclusive holder" a property of the
    # shipped data rather than of the synthetic probes the scheduling
    # tests above drive. An interest in EITHER direction counts -- two
    # exclusive holders of one resource exclude each other too, which is
    # how the three GHCi consumers stay off each other's `cabal repl`.
    for key in sorted(declared):
        taken = probe_runner_resources.exclusive_resources(key)
        unguarded = sorted(
            other for other, _, _ in probe_runner_registry.PROBES
            if other != key
            and not taken <= (probe_runner_resources.shared_resources(other)
                              | probe_runner_resources.exclusive_resources(other)))
        expect(not unguarded,
               f"every other registered probe declares an interest in "
               f"everything {key} takes, so none can be scheduled beside it "
               f"(unguarded: {unguarded})")


# --------------------------------------------------------------------------
# The engine executable is resolved ONCE, before any probe (#1570)
#
# These drive the REAL `run_probes.main` against the synthetic tree, with
# only the preflight's subprocess entry point doubled — so the ordering,
# the call count, the refusal path and the environment handed to each
# probe are the shipped code's, not a restatement of it.
# --------------------------------------------------------------------------
def preflight_argvs(recorder: PreflightRecorder) -> list[list[str]]:
    return [list(argv) for argv in recorder.argvs]


def first_start(tree: Tree, name: str) -> float | None:
    """When this probe's FIRST attempt began, or None if it never ran."""
    windows = tree.intervals(name)
    return windows[0][0] if windows else None


def test_one_preflight_precedes_every_parallel_probe() -> None:
    print("\n-- a --jobs sweep makes ONE Cabal contact, before any probe starts")
    tree = Tree()
    try:
        for name in ("alpha", "beta", "gamma"):
            tree.add(name, exit_code=0)
        recorder = PreflightRecorder(tree.executable)
        with patched(tree, preflight=recorder):
            rc, out = _main_with_open(
                tree, ["--only", "alpha,beta,gamma", "--exact", "--jobs", "3"])
        expect(rc == 0, f"every probe still passes (got {rc})\n{out}")
        expect(preflight_argvs(recorder) == [
                   ["cabal", "build", "exe:synarchy"],
                   ["cabal", "list-bin", "exe:synarchy"]],
               f"exactly one freshness build and one read-only query, in that "
               f"order (got {preflight_argvs(recorder)})")
        starts = [first_start(tree, name) for name in ("alpha", "beta", "gamma")]
        expect(all(when is not None for when in starts),
               f"all three probes really ran (starts: {starts})")
        expect(all(when is not None and recorder.finished_at <= when
                   for when in starts),
               f"and the preflight finished before the earliest of them "
               f"(preflight {recorder.finished_at}, starts {starts})")
    finally:
        tree.cleanup()


def test_one_preflight_precedes_every_sequential_probe() -> None:
    print("\n-- and a sequential sweep makes the same one contact, first")
    tree = Tree()
    try:
        tree.add("alpha", exit_code=0)
        tree.add("beta", exit_code=0)
        recorder = PreflightRecorder(tree.executable)
        with patched(tree, preflight=recorder):
            rc, out = _main_with_open(tree, ["--only", "alpha,beta", "--exact"])
        expect(rc == 0, f"both probes still pass (got {rc})\n{out}")
        expect(len(recorder.calls) == 2,
               f"the sequential path preflights once too, not per probe "
               f"(calls: {preflight_argvs(recorder)})")
        starts = [first_start(tree, name) for name in ("alpha", "beta")]
        expect(all(when is not None and recorder.finished_at <= when
                   for when in starts),
               f"before either of them (preflight {recorder.finished_at}, "
               f"starts {starts})")
    finally:
        tree.cleanup()


def test_a_failed_preflight_spawns_nothing() -> None:
    print("\n-- a preflight that fails starts no probe, allocates no retry")
    for failing_step in ("build", "locate"):
        tree = Tree()
        try:
            tree.add("alpha", exit_code=0)
            recorder = PreflightRecorder(tree.executable, fail=failing_step,
                                          message="no such package")
            with patched(tree, preflight=recorder):
                rc, out = _main_with_open(
                    tree, ["--only", "alpha", "--exact", "--retries", "2"])
            expect(rc == 2,
                   f"the {failing_step} step failing exits 2 (got {rc})")
            expect("cabal" in out and "no such package" in out,
                   f"and says which command failed, and why (got {out!r})")
            expect(not tree.started("alpha"),
                   "no probe process was spawned")
            expect(tree.intervals("alpha") == [],
                   f"so no attempt was recorded either "
                   f"(got {tree.intervals('alpha')})")
            expect("PASS" not in out and "FAIL" not in out,
                   f"and no probe verdict was reported (got {out!r})")
        finally:
            tree.cleanup()


def test_an_unusable_resolved_path_is_refused_not_ignored() -> None:
    print("\n-- an executable that cannot be run is a refusal, not a fallback")
    tree = Tree()
    try:
        tree.add("alpha", exit_code=0)
        missing = tree.root / "not-built-yet"
        recorder = PreflightRecorder(missing)
        with patched(tree, preflight=recorder):
            rc, out = _main_with_open(tree, ["--only", "alpha", "--exact"])
        expect(rc == 2, f"a list-bin answer naming no file exits 2 (got {rc})")
        expect(str(missing) in out,
               f"and names the path it could not use (got {out!r})")
        expect(not tree.started("alpha"), "and no probe was spawned")
    finally:
        tree.cleanup()


def test_list_and_rejected_selections_stay_build_free() -> None:
    print("\n-- --list and a selection that runs nothing never reach Cabal")
    tree = Tree()
    try:
        tree.add("alpha", exit_code=0)
        for argv, why in (
                (["--list"], "--list"),
                (["--only", "nosuchprobe", "--exact"], "an all-invalid --exact"),
                (["--only", "alpha,nosuchprobe", "--exact"],
                 "a MIXED --exact selection"),
                (["--only", "nosuchsubstring"], "a substring matching nothing")):
            recorder = PreflightRecorder(tree.executable)
            with patched(tree, preflight=recorder):
                _rc, _out = _main_with_open(tree, argv)
            expect(not recorder.calls,
                   f"{why} builds nothing (calls: {preflight_argvs(recorder)})")
        expect(not tree.started("alpha"),
               "and the mixed selection still ran no probe")
    finally:
        tree.cleanup()


def test_gui_port_refusal_still_precedes_the_build() -> None:
    print("\n-- a refused port plan is refused before anything is built")
    tree = Tree()
    try:
        tree.add("alpha", exit_code=0)
        recorder = PreflightRecorder(tree.executable)
        with patched(tree, preflight=recorder):
            rc, _out = _main_refusal(
                tree, ["--only", "alpha", "--exact", "--port",
                       str(probe_runner_registry.GUI_PORT)])
        expect(rc != 0, f"the GUI port is still refused (got {rc})")
        expect(not recorder.calls,
               f"and nothing was built first "
               f"(calls: {preflight_argvs(recorder)})")
    finally:
        tree.cleanup()


def test_the_resolved_executable_reaches_every_attempt() -> None:
    print("\n-- every probe process is handed the one resolved executable")
    tree = Tree()
    try:
        tree.add("alpha", exit_code=0)
        # Fails once, so the parallel batch's SOLO retry is a second
        # attempt that must be handed the same executable.
        tree.add("flaky", exit_code=1)
        recorder = PreflightRecorder(tree.executable)
        with patched(tree, preflight=recorder):
            _rc, out = _main_with_open(
                tree, ["--only", "alpha,flaky", "--exact", "--jobs", "2",
                       "--retries", "1"])
        want = str(tree.executable)
        expect(tree.engine_exes("alpha") == [want],
               f"the parallel attempt got it (got {tree.engine_exes('alpha')})")
        expect(tree.engine_exes("flaky") == [want, want],
               f"and so did BOTH the parallel attempt and its solo retry "
               f"(got {tree.engine_exes('flaky')})\n{out}")
        expect(len(recorder.calls) == 2,
               f"the retry built nothing further "
               f"(calls: {preflight_argvs(recorder)})")
    finally:
        tree.cleanup()

    tree = Tree()
    try:
        tree.add("solo", exit_code=1)
        recorder = PreflightRecorder(tree.executable)
        with patched(tree, preflight=recorder):
            _rc, _out = _main_with_open(
                tree, ["--only", "solo", "--exact", "--retries", "2"])
        want = str(tree.executable)
        expect(tree.engine_exes("solo") == [want, want, want],
               f"the sequential path's inline retries got it too "
               f"(got {tree.engine_exes('solo')})")
        expect(len(recorder.calls) == 2,
               f"still one preflight (calls: {preflight_argvs(recorder)})")
    finally:
        tree.cleanup()


def test_a_direct_run_one_leaves_the_child_on_the_fallback() -> None:
    print("\n-- run_one without a resolved executable hands the child nothing")
    tree = Tree()
    try:
        script = tree.add("bare", exit_code=0)
        with patched(tree):
            # `patched` clears the resolved executable; nothing calls the
            # preflight here. What matters is the STRIPPING: an operator's
            # stale export must not decide which binary the child runs, so
            # a child that was handed nothing is left to prepare its own
            # (#1913) rather than inheriting one nobody resolved.
            os.environ[probe_engine.ENV_ENGINE_EXE] = str(tree.executable)
            try:
                ok, _t, _e, _out = probe_runner_lifecycle.run_one(script, None, 120.0)
            finally:
                os.environ.pop(probe_engine.ENV_ENGINE_EXE, None)
        expect(ok, "the probe still ran")
        expect(tree.engine_exes("bare") == [""],
               f"and saw no runner-supplied executable, so probelib "
               f"prepares its own (got {tree.engine_exes('bare')})")
    finally:
        tree.cleanup()


def test_a_nested_runner_adopts_the_executable_without_rebuilding() -> None:
    print("\n-- a nested runner reuses what its ancestor already resolved")
    tree = Tree()
    try:
        recorder = PreflightRecorder(tree.executable)
        with patched(tree, preflight=recorder):
            adopted = probe_runner_resources.engine_preflight(
                environ={probe_engine.ENV_ENGINE_EXE: str(tree.executable)})
        expect(adopted == str(tree.executable),
               f"the inherited executable is adopted verbatim (got {adopted})")
        expect(not recorder.calls,
               f"with no second build (calls: {preflight_argvs(recorder)})")

        recorder = PreflightRecorder(tree.executable)
        with patched(tree, preflight=recorder):
            resolved = probe_runner_resources.engine_preflight(environ={})
        expect(resolved == str(tree.executable),
               "and with nothing inherited it resolves one itself")
        expect(len(recorder.calls) == 2,
               f"through exactly one build and one query "
               f"(calls: {preflight_argvs(recorder)})")
    finally:
        tree.cleanup()


def test_an_ancestors_exclusive_hold_is_not_waited_on() -> None:
    print("\n-- a nested runner never waits on its own ancestor's hold")
    namespace = f"selftest{uuid.uuid4().hex[:12]}"
    env = {probe_runner_resources.ENV_HELD_NAMESPACE: namespace,
           probe_runner_resources.ENV_HELD_EXCLUSIVE: "cabal-build"}
    lock_exclusive, lock_shared = probe_runner_resources.cross_process_interests(
        "chop", namespace, env)
    expect("cabal-build" not in lock_shared,
           f"an inherited exclusive drops out of a shared request "
           f"(got {sorted(lock_shared)})")
    expect("repo-config" in lock_shared,
           f"while everything else is still requested "
           f"(got {sorted(lock_shared)})")
    nested_exclusive, _ = probe_runner_resources.cross_process_interests(
        "save_compat_migration", namespace, env)
    expect(not nested_exclusive,
           f"and out of an exclusive request too "
           f"(got {sorted(nested_exclusive)})")

    # The in-process ledger keeps the FULL declarations, so a nested sweep
    # still serializes its own probes against each other.
    expect("cabal-build" in probe_runner_resources.exclusive_resources(
               "save_compat_migration"),
           "the declaration itself is untouched")

    # A DIFFERENT namespace inherits nothing: a resource name means
    # nothing outside the repository its lock was taken in.
    foreign = dict(env, **{probe_runner_resources.ENV_HELD_NAMESPACE: "somewhere-else"})
    _fx, foreign_shared = probe_runner_resources.cross_process_interests(
        "chop", namespace, foreign)
    expect("cabal-build" in foreign_shared,
           f"a hold from another namespace is ignored "
           f"(got {sorted(foreign_shared)})")

    # And the hold really is grantable: take `cabal-build` exclusively
    # here, then acquire the nested request against the same namespace.
    ancestor = probe_resource_lock.acquire(
        exclusive={"cabal-build"}, namespace=namespace,
        purpose="selftest ancestor")
    try:
        try:
            nested = probe_resource_lock.acquire(
                exclusive=lock_exclusive, shared=lock_shared,
                namespace=namespace, purpose="selftest nested")
        except probe_resource_lock.ResourceBusy as busy:
            expect(False, f"the nested request still blocked on {busy.resource!r}")
        else:
            expect(True, "the nested request is granted under the ancestor's "
                         "exclusive hold")
            nested.release()
    finally:
        ancestor.release()
        clear_namespace(namespace)


FOREIGN_TRY_SRC = textwrap.dedent("""\
    # Try ONCE, without waiting, to take one resource in one interest.
    # Prints "busy" when a live holder refuses it and "free" when it is
    # granted (released again immediately). A SEPARATE process, because
    # an flock conflict between two open file descriptions is exactly
    # what the cross-process layer is made of, and asking from inside
    # the holding process would prove nothing about another runner.
    import sys
    sys.path.insert(0, sys.argv[1])
    import probe_resource_lock
    namespace, resource, interest = sys.argv[2], sys.argv[3], sys.argv[4]
    want = {resource}
    kwargs = ({"exclusive": want} if interest == "exclusive"
              else {"shared": want})
    try:
        hold = probe_resource_lock.acquire(namespace=namespace,
                                           purpose="selftest probe", **kwargs)
    except probe_resource_lock.ResourceBusy:
        print("busy")
    else:
        hold.release()
        print("free")
    """)


def foreign_interest(namespace: str, resource: str, interest: str) -> str:
    """"busy" or "free": whether ANOTHER process could take it right now."""
    with tempfile.TemporaryDirectory() as tmp:
        script = Path(tmp) / "try_acquire.py"
        script.write_text(FOREIGN_TRY_SRC)
        done = subprocess.run(
            [sys.executable, str(script), TOOLS_DIR, namespace, resource,
             interest],
            capture_output=True, text=True, timeout=60)
        return done.stdout.strip() or f"error: {done.stderr.strip()[:200]}"


def test_the_preflight_build_excludes_a_foreign_runner() -> None:
    print("\n-- while the preflight builds, no other runner is in the tree")
    tree = Tree()
    try:
        observed: list[str] = []
        namespace = f"selftest{uuid.uuid4().hex[:12]}"
        recorder = PreflightRecorder(tree.executable)

        def watching(argv, cwd=None, capture_output=False, text=False):
            # Asked from INSIDE the build, which is the only instant that
            # answers the question the concern is about.
            observed.append(foreign_interest(namespace,
                                              probe_runner_resources.BUILD_RESOURCE,
                                              "shared"))
            observed.append(foreign_interest(namespace,
                                              probe_runner_resources.BUILD_RESOURCE,
                                              "exclusive"))
            return recorder(argv, cwd=cwd, capture_output=capture_output,
                            text=text)

        tree.add("alpha", exit_code=0)
        with patched(tree, namespace=namespace, preflight=watching):
            rc, out = _main_with_open(tree, ["--only", "alpha", "--exact"])
        expect(rc == 0, f"the sweep still passes (got {rc})\n{out}")
        expect(observed and all(answer == "busy" for answer in observed),
               f"every foreign interest in the build state was refused for "
               f"the whole preflight (got {observed})")
        expect(foreign_interest(namespace, probe_runner_resources.BUILD_RESOURCE,
                                 "exclusive") == "free",
               "and the hold is released once the preflight is done, so the "
               "sweep's own probes are never queued behind it")
    finally:
        tree.cleanup()
        clear_namespace(namespace)


def test_the_preflight_build_waits_for_a_foreign_runner() -> None:
    print("\n-- and it waits for a foreign holder rather than building beside it")
    tree = Tree()
    namespace = f"selftest{uuid.uuid4().hex[:12]}"
    holder = None
    try:
        recorder = PreflightRecorder(tree.executable)
        resolved: list[str] = []
        failed: list[BaseException] = []
        holder = ForeignHolder(namespace, "exclusive",
                               probe_runner_resources.BUILD_RESOURCE)
        expect(holder.wait_until_held(), "the foreign runner holds the "
                                          "build state exclusively")

        def resolve() -> None:
            try:
                with patched(tree, namespace=namespace, preflight=recorder):
                    resolved.append(probe_runner_resources.engine_preflight(namespace,
                                                                 environ={}))
            except BaseException as error:      # reported, never swallowed
                failed.append(error)

        worker = threading.Thread(target=resolve, daemon=True)
        worker.start()
        worker.join(timeout=4.0)
        expect(worker.is_alive(),
               "the preflight is still waiting, not building")
        expect(not recorder.calls,
               f"so no Cabal command ran beside the foreign holder "
               f"(calls: {preflight_argvs(recorder)})")
        holder.stop()
        holder = None
        worker.join(timeout=90.0)
        expect(not worker.is_alive(), "and it proceeds once that holder lets go")
        expect(not failed, f"without raising ({failed})")
        expect(resolved == [str(tree.executable)],
               f"resolving the executable it was going to resolve "
               f"(got {resolved})")
        expect(len(recorder.calls) == 2,
               f"through the same one build and one query "
               f"(calls: {preflight_argvs(recorder)})")
    finally:
        if holder is not None:
            holder.stop()
        tree.cleanup()
        clear_namespace(namespace)


def test_a_nested_preflight_does_not_wait_on_its_ancestor() -> None:
    print("\n-- a nested runner's preflight is inside its ancestor's hold")
    tree = Tree()
    namespace = f"selftest{uuid.uuid4().hex[:12]}"
    ancestor = None
    try:
        recorder = PreflightRecorder(tree.executable)
        ancestor = probe_resource_lock.acquire(
            exclusive={probe_runner_resources.BUILD_RESOURCE}, namespace=namespace,
            purpose="selftest ancestor")
        # The environment a nested runner is handed: no executable (so it
        # really does build), but its ancestor's exclusive hold declared.
        env = {probe_runner_resources.ENV_HELD_NAMESPACE: namespace,
               probe_runner_resources.ENV_HELD_EXCLUSIVE: probe_runner_resources.BUILD_RESOURCE}
        with patched(tree, namespace=namespace, preflight=recorder):
            resolved = probe_runner_resources.engine_preflight(namespace, environ=env)
        expect(resolved == str(tree.executable),
               f"it resolved without waiting on its ancestor (got {resolved})")
        expect(len(recorder.calls) == 2,
               f"having really built (calls: {preflight_argvs(recorder)})")
    finally:
        if ancestor is not None:
            ancestor.release()
        tree.cleanup()
        clear_namespace(namespace)


def test_the_hold_environment_names_what_a_probe_holds() -> None:
    print("\n-- a probe is told what its runner holds exclusively for it")
    namespace = "selftest-hold-env"
    env = probe_runner_resources.descendant_hold_env("save_compat_migration", namespace)
    expect(env.get(probe_runner_resources.ENV_HELD_EXCLUSIVE) == "cabal-build",
           f"an exclusive holder exports its resource (got {env!r})")
    expect(env.get(probe_runner_resources.ENV_HELD_NAMESPACE) == namespace,
           f"qualified by the namespace it was taken in (got {env!r})")
    expect(probe_runner_resources.descendant_hold_env("chop", namespace) == {},
           "a probe holding nothing exclusively exports nothing")
    expect(probe_runner_resources.descendant_hold_env("save_compat_migration", None) == {},
           "and without a namespace there is nothing to export")


def test_a_probe_is_handed_its_runners_exclusive_holds() -> None:
    print("\n-- and that environment really reaches the probe process")
    tree = Tree()
    try:
        # Named for a shipped EXCLUSIVE holder, so the real declaration is
        # what decides — the same trick the scheduling tests above use.
        tree.add("config_state", exit_code=0)
        with patched(tree) as fixture:
            rc, out = _main_with_open(
                tree, ["--only", "config_state", "--exact"])
        expect(rc == 0, f"the probe passed (got {rc})\n{out}")
        lines = tree.env_lines("config_state")
        expect(len(lines) == 1, f"it ran once (got {lines})")
        if lines:
            _exe, held, held_ns = lines[0]
            expect(held == "repo-config",
                   f"and was told what its runner holds for it (got {held!r})")
            expect(held_ns == fixture.namespace,
                   f"in the runner's own namespace (got {held_ns!r})")
    finally:
        tree.cleanup()


def registered_probe_sources() -> dict[str, str]:
    """Every registered probe script's source text, keyed by probe key."""
    tools = Path(TOOLS_DIR)
    out = {}
    for key, script, _ in probe_runner_registry.PROBES:
        path = tools / script
        if path.is_file():
            out[key] = path.read_text(encoding="utf-8")
    return out


def cabal_run_launchers(source: str) -> list[str]:
    """Sequence literals in `source` that spell a `cabal run` launch.

    Structural, over the parsed tree rather than the text: a list or
    tuple whose first element is the string "cabal" and whose second is
    "run". That is exactly the engine launch #1570 removed from every
    probe, and it stays out of reach of a probe that merely MENTIONS
    cabal in prose or runs a different cabal subcommand behind the
    runner-supplied-executable check (`resource_root_probe.py`).
    """
    found = []
    for node in ast.walk(ast.parse(source)):
        if not isinstance(node, (ast.List, ast.Tuple)):
            continue
        head = [element.value for element in node.elts[:2]
                if isinstance(element, ast.Constant)
                and isinstance(element.value, str)]
        if head[:2] == ["cabal", "run"]:
            found.append(ast.unparse(node))
    return found


def test_no_registered_probe_spells_a_cabal_engine_launch() -> None:
    print("\n-- no registered probe launches its engine through `cabal run`")
    offenders = {key: launchers
                 for key, source in registered_probe_sources().items()
                 if (launchers := cabal_run_launchers(source))}
    expect(not offenders,
           f"every engine launch goes through probe_engine.engine_command "
           f"(offenders: {offenders})")
    for shared in ("probelib.py", "probe_engine.py"):
        source = (Path(TOOLS_DIR) / shared).read_text(encoding="utf-8")
        launchers = cabal_run_launchers(source)
        if shared == "probelib.py":
            expect(not launchers,
                   f"probelib no longer spells one either (got {launchers})")
        else:
            expect(len(launchers) == 1,
                   f"probe_engine owns the ONE remaining fallback spelling "
                   f"(got {launchers})")

    # Mutation: the guard has to FIRE on a reintroduced launcher, not
    # merely agree that today's tree is clean.
    reintroduced = ('cmd = ["cabal", "run", "-v0", "exe:synarchy", "--", '
                    '"--headless"]\n')
    expect(cabal_run_launchers(reintroduced),
           "a reintroduced `cabal run` launcher is caught")
    expect(not cabal_run_launchers('r = ["cabal", "list-bin", "exe:synarchy"]\n'),
           "while a non-launching cabal subcommand is not mistaken for one")
    expect(not cabal_run_launchers('note = "run this with cabal run"\n'),
           "and neither is prose that merely mentions it")


def test_resource_ledger_is_a_reader_writer_lock() -> None:
    print("\n-- the ledger grants many readers at once and a writer only alone")
    ledger = probe_runner_resources.ResourceLedger()
    shared, exclusive = {"repo-config"}, {"repo-config"}
    expect(ledger.idle(), "a fresh ledger holds nothing")
    expect(not ledger.blocked(exclusive, set()),
           "so an exclusive interest is grantable")

    # Three readers, then a writer: the shared side must COUNT, not merely
    # remember that someone held it, or the writer starts after the first
    # release with two engines still up.
    for _ in range(3):
        expect(not ledger.blocked(set(), shared),
               "another reader may join while readers hold it")
        ledger.acquire(set(), shared)
    for held in (2, 1):
        expect(ledger.blocked(exclusive, set()),
               f"a writer is blocked while {held + 1} reader(s) hold it")
        ledger.release(set(), shared)
    expect(ledger.blocked(exclusive, set()),
           "still blocked with the last reader holding it")
    ledger.release(set(), shared)
    expect(ledger.idle() and not ledger.blocked(exclusive, set()),
           "and grantable only once every reader has released")

    # And the mirror: a writer excludes readers and writers alike.
    ledger.acquire(exclusive, set())
    expect(ledger.blocked(set(), shared),
           "a reader is blocked while a writer holds it")
    expect(ledger.blocked(exclusive, set()),
           "and so is a second writer")
    ledger.release(exclusive, set())
    expect(ledger.idle(), "releasing the writer empties the ledger")

    # A probe that declares nothing shared is never blocked by anything,
    # which is what keeps an unrelated resource out of this decision.
    ledger.acquire(exclusive, set())
    expect(not ledger.blocked(set(), {"some-other-resource"}),
           "an interest in a different resource is unaffected")
    ledger.release(exclusive, set())


# --------------------------------------------------------------------------
# Exact selection: unknown keys are rejected rather than silently dropped
# (#1321)
# --------------------------------------------------------------------------
# --------------------------------------------------------------------------
# The cross-process half of the same reader/writer model (#1436)
# --------------------------------------------------------------------------
FOREIGN_HOLDER_SRC = textwrap.dedent("""\
    # A separate PROCESS holding one probe resource, because that is the
    # thing the in-process ledger cannot see. Signals readiness by
    # creating a file, then holds until a release file appears.
    import sys, time
    from pathlib import Path
    sys.path.insert(0, sys.argv[1])
    import probe_resource_lock as lock
    namespace, interest, resource, ready, release = sys.argv[2:7]
    kwargs = ({"exclusive": {resource}} if interest == "exclusive"
              else {"shared": {resource}})
    hold = lock.acquire(namespace=namespace, purpose="foreign holder", **kwargs)
    Path(ready).write_text("held")
    deadline = time.time() + 120
    while not Path(release).exists() and time.time() < deadline:
        time.sleep(0.05)
    hold.release()
""")


class ForeignHolder:
    """A real second process holding one resource for the duration."""

    def __init__(self, namespace: str, interest: str,
                 resource: str = "repo-config") -> None:
        self.dir = Path(tempfile.mkdtemp(prefix="foreign_holder_"))
        script = self.dir / "holder.py"
        script.write_text(FOREIGN_HOLDER_SRC)
        self.ready = self.dir / "ready"
        self.release_flag = self.dir / "release"
        self.proc = subprocess.Popen(
            [sys.executable, str(script), TOOLS_DIR, namespace, interest,
             resource, str(self.ready), str(self.release_flag)])

    def wait_until_held(self, seconds: float = 30.0) -> bool:
        return wait_file(self.ready, seconds)

    def stop(self) -> None:
        try:
            self.release_flag.write_text("go")
        except OSError:
            pass
        try:
            self.proc.wait(timeout=30)
        except subprocess.TimeoutExpired:
            self.proc.kill()
            self.proc.wait(timeout=10)
        shutil.rmtree(self.dir, ignore_errors=True)


def test_a_foreign_exclusive_holder_makes_the_sweep_wait() -> None:
    print("\n-- a foreign EXCLUSIVE holder stalls every probe without "
          "crashing the scheduler")
    tree = Tree()
    namespace = f"selftest{uuid.uuid4().hex[:12]}"
    holder = ForeignHolder(namespace, "exclusive")
    seen: dict = {}
    try:
        seen["took_lock"] = holder.wait_until_held()
        # Every registered probe holds `repo-config` SHARED, so ONE foreign
        # exclusive holder conflicts with the whole roster. Before #1436 the
        # scheduler's "nothing running, work pending" guard raised
        # RuntimeError here and took the sweep down.
        tree.add("unrelated_a", dwell=0.2, descendant=False)
        tree.add("unrelated_b", dwell=0.2, descendant=False)
        saved_poll = probe_runner_resources.RESOURCE_WAIT_POLL
        probe_runner_resources.RESOURCE_WAIT_POLL = 0.2
        result: dict = {}

        def sweep() -> None:
            with patched(tree, namespace=namespace):
                result["rc"], result["out"] = _main_with_open(tree, ["--jobs", "2"])

        thread = threading.Thread(target=sweep, daemon=True)
        thread.start()
        # Nothing is asserted while the sweep runs: it has redirected
        # stdout, so a message printed here would land in its buffer.
        # Observations are recorded and judged after the join.
        time.sleep(2.0)
        seen["still_waiting"] = thread.is_alive()
        seen["nothing_started"] = (not tree.started("unrelated_a")
                                   and not tree.started("unrelated_b"))
        holder.stop()
        thread.join(timeout=90)
        probe_runner_resources.RESOURCE_WAIT_POLL = saved_poll

        expect(seen["took_lock"], "the foreign process took the lock")
        expect(seen["still_waiting"],
               "the sweep is still waiting rather than having crashed or "
               "finished")
        expect(seen["nothing_started"],
               "and no probe started while the foreign holder was in the way")
        expect(not thread.is_alive(), "the sweep finishes once the lock frees")
        expect(result.get("rc") == 0,
               f"and every probe then passes (exit {result.get('rc')})")
        expect("waiting for 'repo-config'" in (result.get("out") or "")
               and "exclusive" in (result.get("out") or ""),
               "the runner said WHICH resource it was waiting on and in which "
               "interest")
        for name in ("unrelated_a", "unrelated_b"):
            got = tree.intervals(name)
            expect(len(got) == 1 and got[0][1] is not None,
                   f"{name} ran exactly once, after the wait (windows: {got})")
    finally:
        holder.stop()
        clear_namespace(namespace)
        tree.cleanup()


def test_waiting_for_a_foreign_holder_is_not_charged_to_the_probe() -> None:
    print("\n-- a queued probe's elapsed time and timeout cover execution "
          "only, never the wait")
    tree = Tree()
    namespace = f"selftest{uuid.uuid4().hex[:12]}"
    holder = ForeignHolder(namespace, "exclusive")
    seen: dict = {}
    try:
        seen["took_lock"] = holder.wait_until_held()
        tree.add("unrelated_a", dwell=0.2, descendant=False)
        saved_poll = probe_runner_resources.RESOURCE_WAIT_POLL
        probe_runner_resources.RESOURCE_WAIT_POLL = 0.2
        result: dict = {}

        def sweep() -> None:
            with patched(tree, namespace=namespace):
                # SEQUENTIAL, and with a timeout far shorter than the wait
                # below: if the wait were inside the probe's own clock this
                # would be reported TIMEOUT instead of PASS.
                result["rc"], result["out"] = _main_with_open(
                    tree, ["--jobs", "1", "--timeout", "5"])

        thread = threading.Thread(target=sweep, daemon=True)
        thread.start()
        time.sleep(8.0)
        seen["still_waiting"] = thread.is_alive()
        holder.stop()
        thread.join(timeout=90)
        probe_runner_resources.RESOURCE_WAIT_POLL = saved_poll
        out = result.get("out") or ""

        expect(seen["took_lock"], "the foreign process took the lock")
        expect(seen["still_waiting"], "the sweep waited rather than running")
        expect(result.get("rc") == 0,
               f"the probe passes after an 8s wait against a 5s timeout "
               f"(exit {result.get('rc')})")
        expect("TIMEOUT" not in out,
               "and the wait is never reported as a TIMEOUT")
        window = tree.window("unrelated_a")
        expect(window[1] is not None and (window[1] - window[0]) < 5.0,
               f"the probe's own occupancy window is its execution alone "
               f"({window})")
    finally:
        holder.stop()
        clear_namespace(namespace)
        tree.cleanup()


def test_a_foreign_shared_holder_never_blocks_a_shared_probe() -> None:
    print("\n-- a foreign SHARED holder does not serialize ordinary probes")
    tree = Tree()
    namespace = f"selftest{uuid.uuid4().hex[:12]}"
    holder = ForeignHolder(namespace, "shared")
    try:
        took = holder.wait_until_held()
        tree.add("unrelated_a", dwell=0.8, descendant=False)
        tree.add("unrelated_b", dwell=0.8, descendant=False)
        with patched(tree, namespace=namespace):
            rc, out = _main_with_open(tree, ["--jobs", "2"])
        expect(took, "the foreign process took the lock")
        expect(rc == 0, f"both probes pass beside the shared holder (exit {rc})")
        first, second = tree.window("unrelated_a"), tree.window("unrelated_b")
        expect(overlaps(first, second),
               f"and they still run concurrently: shared holders coexist "
               f"(unrelated_a {first}, unrelated_b {second})")
        expect("waiting for" not in out, "nothing waited on anything")
    finally:
        holder.stop()
        clear_namespace(namespace)
        tree.cleanup()


def test_a_run_probes_exclusive_probe_blocks_a_foreign_shared_acquirer() -> None:
    print("\n-- the conflict is detected in the other direction too: a "
          "run_probes exclusive probe blocks a foreign shared acquirer")
    tree = Tree()
    namespace = f"selftest{uuid.uuid4().hex[:12]}"
    seen: dict = {}
    try:
        tree.add("config_state", dwell=3.0, descendant=False)
        result: dict = {}

        def sweep() -> None:
            with patched(tree, namespace=namespace):
                result["rc"], result["out"] = _main_with_open(tree, ["--jobs", "1"])

        thread = threading.Thread(target=sweep, daemon=True)
        thread.start()
        seen["started"] = wait_file(tree.root / "config_state.started", 60.0)
        # While it runs, a /deflake-shaped acquirer must be refused even for
        # a SHARED interest -- the direction the in-process ledger could
        # never enforce, because the acquirer is not in its process.
        try:
            spurious = probe_resource_lock.acquire(
                shared={"repo-config"}, namespace=namespace,
                purpose="foreign shared acquirer")
        except probe_resource_lock.ResourceBusy as busy:
            seen["busy"] = busy
        else:
            spurious.release()
        thread.join(timeout=120)

        expect(seen.get("started") is True, "the exclusive probe started")
        expect("busy" in seen,
               "a foreign SHARED acquirer is refused while run_probes holds "
               "the resource exclusively")
        busy = seen.get("busy")
        if busy is not None:
            expect(busy.resource == "repo-config" and busy.interest == "shared",
                   f"and the refusal names the resource and the interest "
                   f"({busy.resource!r}, {busy.interest})")
            expect(any(holder.get("interest") == "exclusive"
                       for holder in busy.holders),
                   f"and reports the exclusive holder ({busy.holders})")
        expect(result.get("rc") == 0,
               f"the sweep itself is unaffected (exit {result.get('rc')})")
        # And once it is over, the same acquisition succeeds.
        after = probe_resource_lock.acquire(shared={"repo-config"},
                                            namespace=namespace)
        after.release()
        expect(True, "the same acquisition succeeds once the probe is done")
    finally:
        clear_namespace(namespace)
        tree.cleanup()


def test_exact_mixed_selection_is_rejected_before_listing() -> None:
    print("\n-- --exact + --list with unknown keys alongside a valid one is "
          "rejected, not partially listed")
    tree = Tree()
    try:
        tree.add("good", exit_code=0)
        rc, out = _main_with(
            tree, ["--only", "good,not_a_probe,also_bad", "--exact", "--list"])
        expect(rc != 0,
               f"a mixed valid/invalid --exact selection must fail (got {rc})")
        expect("not_a_probe" in out and "also_bad" in out,
               f"the diagnostic names every unknown key, got: {out!r}")
        expect("good_probe.py" not in out,
               f"no partial listing of the valid probe leaks through, got: {out!r}")
    finally:
        tree.cleanup()


def test_exact_mixed_selection_never_runs_the_valid_probe() -> None:
    print("\n-- the same rejection happens before RUNNING anything, not just listing")
    tree = Tree()
    try:
        tree.add("good", exit_code=0)
        rc, out = _main_with(tree, ["--only", "good,not_a_probe", "--exact"])
        expect(rc != 0, f"the mixed selection is rejected (got {rc})")
        expect(not tree.started("good"), "the valid probe never actually started")
    finally:
        tree.cleanup()


def test_exact_all_invalid_selection_keeps_existing_diagnostic() -> None:
    print("\n-- an all-invalid --exact selection keeps the pre-existing "
          "empty-selection error and exit code")
    tree = Tree()
    try:
        tree.add("good", exit_code=0)
        rc, out = _main_with(tree, ["--only", "not_a_probe", "--exact", "--list"])
        expect(rc == 2, f"an all-invalid --exact selection still exits 2 (got {rc})")
        expect("matched no probes" in out,
               f"and keeps the existing 'matched no probes' diagnostic, got: {out!r}")
    finally:
        tree.cleanup()


def test_exact_all_valid_selection_is_unaffected() -> None:
    print("\n-- a wholly valid --exact selection lists in registry order, unchanged")
    tree = Tree()
    try:
        tree.add("alpha", exit_code=0)
        tree.add("beta", exit_code=0)
        rc, out = _main_with(tree, ["--only", "beta,alpha", "--exact", "--list"])
        expect(rc == 0, f"a wholly valid --exact selection still exits 0 (got {rc})")
        expect(out.index("alpha_probe.py") < out.index("beta_probe.py"),
               f"registry order survives regardless of request order, got: {out!r}")
    finally:
        tree.cleanup()


def test_exact_duplicate_valid_keys_still_collapse() -> None:
    print("\n-- a wholly valid --exact selection with a duplicated key lists it once")
    tree = Tree()
    try:
        tree.add("alpha", exit_code=0)
        rc, out = _main_with(tree, ["--only", "alpha,alpha", "--exact", "--list"])
        expect(rc == 0, f"still exits 0 (got {rc})")
        expect(out.count("alpha_probe.py") == 1,
               f"a duplicated valid key is listed exactly once, got: {out!r}")
    finally:
        tree.cleanup()


def test_substring_selection_stays_permissive() -> None:
    print("\n-- substring (non --exact) selection still ignores an unmatched needle")
    tree = Tree()
    try:
        tree.add("craft", exit_code=0)
        rc, out = _main_with(tree, ["--only", "craft,not_a_probe", "--list"])
        expect(rc == 0,
               f"substring selection with one unmatched needle still succeeds (got {rc})")
        expect("craft_probe.py" in out, "the matching probe is still listed")
    finally:
        tree.cleanup()


def test_retry_reaps_between_attempts() -> None:
    print("\n-- a retry never starts before the previous attempt's group is reaped")
    tree = Tree()
    try:
        tree.add("flaky", exit_code=1)
        rc, out = _main_with(tree, ["--only", "flaky", "--exact", "--retries", "1"])
        pids = tree.engine_pids("flaky")
        expect(rc == 1, f"the probe still fails after its retry (got {rc})")
        expect(out.count("retrying solo") == 1, "exactly one retry was announced")
        expect(len(pids) == 2,
               f"both attempts really booted an engine (got {pids})")
        alive = [pid for pid in pids if not wait_pid_gone(pid)]
        expect(not alive,
               f"no engine from EITHER attempt is left running (alive: {alive})")
    finally:
        tree.cleanup()


# --------------------------------------------------------------------------
# Interruption: a real SIGINT to a real runner process
# --------------------------------------------------------------------------
DRIVER_SRC = textwrap.dedent("""\
    import sys, time
    sys.path.insert(0, {tools!r})
    import probe_engine
    import probe_runner_lifecycle
    import probe_runner_registry
    import probe_runner_resources
    import run_probes
    probe_engine.REPO_ROOT = {root!r}
    probe_runner_registry.PROBES = {probes!r}
    # Like the in-process fixture, a synthetic registry starts with no
    # shipped key-specific timeout declarations.
    probe_runner_registry.PROBE_TIMEOUT_OVERRIDES = {{}}
    probe_runner_lifecycle.GROUP_GRACE = {grace!r}
    # The synthetic tree is not a git checkout, so the cross-process
    # resource namespace (#1436) has to be supplied the same way the
    # in-process `patched` fixture supplies it -- otherwise the runner
    # refuses to start and the interrupt below has nothing to interrupt.
    probe_runner_resources.RESOURCE_NAMESPACE = {namespace!r}
    # ... and the engine-executable preflight (#1570) the same way, for
    # the same reason: the synthetic tree is no Cabal project, so a real
    # freshness build would refuse the run before the interrupt could
    # reach it. One build, one list-bin, both answered here.
    _synthetic_exe = {executable!r}

    def _preflight(argv, cwd=None, capture_output=False, text=False):
        import subprocess as _sp
        out = "" if "build" in tuple(argv) else _synthetic_exe + chr(10)
        return _sp.CompletedProcess(tuple(argv), 0, out, "")

    probe_runner_resources.ENGINE_PREFLIGHT_RUNNER = _preflight
    submit_delay = {submit_delay!r}
    if submit_delay:
        # Widen the SUBMISSION window so an interrupt can land inside it.
        # This slows submission down; it does not change what the runner
        # does with the futures, which is what the test is about.
        import concurrent.futures as _cf
        _real = _cf.ThreadPoolExecutor.submit

        def _slow_submit(self, fn, *a, **kw):
            time.sleep(submit_delay)
            return _real(self, fn, *a, **kw)

        _cf.ThreadPoolExecutor.submit = _slow_submit
    sys.argv = ["run_probes.py"] + {argv!r}
    sys.exit(run_probes.main())
    """)


def _run_driver(tree: Tree, argv: list[str], wait_for: list[str],
                grace: float = TEST_GRACE, exit_budget: float = 60.0,
                submit_delay: float = 0.0):
    """Start the real runner in its own session and SIGINT it mid-run."""
    driver = tree.root / "driver.py"
    namespace = f"selftest{uuid.uuid4().hex[:12]}"
    driver.write_text(DRIVER_SRC.format(
        tools=TOOLS_DIR, root=str(tree.root), probes=list(tree.probes),
        grace=grace, argv=argv, submit_delay=submit_delay,
        namespace=namespace, executable=str(tree.executable)))
    proc = subprocess.Popen(
        [sys.executable, str(driver)],
        stdout=subprocess.PIPE, stderr=subprocess.STDOUT, text=True,
        start_new_session=True)
    # Only interrupt once the probes we mean to catch have really booted
    # their engines -- otherwise the test proves nothing.
    ready = all(wait_file(tree.root / f"{name}.enginepid") for name in wait_for)
    # SIGINT the RUNNER's group only. Its probes are in their own sessions
    # (start_new_session=True), exactly as under a terminal Ctrl-C, so this
    # signal cannot reach them: the runner has to do it itself.
    os.killpg(os.getpgid(proc.pid), signal.SIGINT)
    try:
        out, _ = proc.communicate(timeout=exit_budget)
        rc = proc.returncode
    except subprocess.TimeoutExpired:
        # A runner that does not return from an interrupt is a FAILURE to
        # report, not a suite that hangs: the synthetic probes sleep for
        # ten minutes, so waiting it out proves nothing. rc None then
        # fails the exit-code expectation below.
        os.killpg(os.getpgid(proc.pid), signal.SIGKILL)
        out, _ = proc.communicate(timeout=30)
        rc = None
    # The runner process is gone by here, so nothing it held is still held.
    clear_namespace(namespace)
    return ready, rc, out


def test_engine_ledger_is_durable_before_the_handshake() -> None:
    print("\n-- an engine's ledger line is durable before it releases its probe")
    tree = Tree()
    try:
        # The reap fires the instant the probe exits, and the probe exits
        # the instant the handshake file has content -- so anything the
        # engine writes AFTER the handshake races a SIGTERM. This spends
        # three quarters of a second on the safe side of that handshake:
        # with the ledger written first it changes nothing, and if the two
        # writes are ever swapped back the reap lands squarely in this
        # sleep and the ledger loses the line. That is the real Linux CI
        # failure, made deterministic instead of left to a filesystem
        # whose fsync happens to be slow that day.
        tree.add("ledger", exit_code=0, handshake_delay=0.75)
        rc, _ = _main_with(tree, ["--only", "ledger", "--exact"])
        pids = tree.engine_pids("ledger")
        expect(rc == 0, f"the probe still passes (got {rc})")
        expect(len(pids) == 1,
               f"its engine recorded itself despite the delayed handshake "
               f"(got {pids})")
        expect(pids == [tree.engine_pid("ledger")],
               f"and the ledger names the same engine the handshake did "
               f"(ledger {pids}, handshake {tree.engine_pid('ledger')})")
        expect(all(wait_pid_gone(pid) for pid in pids),
               "and that engine is still gone once run_one returns")
    finally:
        tree.cleanup()


def test_ctrl_c_leaves_no_engine_behind() -> None:
    print("\n-- Ctrl-C mid-run terminates the running probe AND its engine")
    tree = Tree()
    try:
        tree.add("interrupted", hang=True)
        ready, rc, out = _run_driver(
            tree, ["--only", "interrupted", "--exact"], ["interrupted"])
        pid = tree.engine_pid("interrupted")
        expect(ready, "the probe booted its engine before the interrupt")
        expect(rc == 130, f"the interrupted runner exits 130 (got {rc})")
        expect(pid is not None and wait_pid_gone(pid),
               "the engine it had booted is gone once the runner exits")
    finally:
        tree.cleanup()


def test_ctrl_c_cancels_queued_parallel_work() -> None:
    print("\n-- Ctrl-C in --jobs mode reaps the running probes and starts no more")
    tree = Tree()
    try:
        for name in ("par_a", "par_b", "par_c", "par_d"):
            tree.add(name, hang=True)
        # jobs=2 with four probes: exactly two occupy the workers (they
        # hang), so the other two can only start if the interrupt fails to
        # stop the queue.
        ready, rc, out = _run_driver(
            tree, ["--jobs", "2"], ["par_a", "par_b"])
        expect(ready, "both concurrent probes booted their engines")
        expect(rc == 130, f"the interrupted parallel runner exits 130 (got {rc})")
        for name in ("par_a", "par_b"):
            pid = tree.engine_pid(name)
            expect(pid is not None and wait_pid_gone(pid),
                   f"{name}'s engine is gone once the runner exits")
        not_started = [n for n in ("par_c", "par_d") if not tree.started(n)]
        expect(len(not_started) == 2,
               f"neither queued probe was launched after the interrupt "
               f"(never started: {not_started})")
        for name in ("par_c", "par_d"):
            pid = tree.engine_pid(name)
            expect(pid is None, f"{name} booted no engine at all")
    finally:
        tree.cleanup()


def test_ctrl_c_during_submission_starts_nothing_more() -> None:
    print("\n-- Ctrl-C DURING future submission launches no further probe")
    tree = Tree()
    names = [f"sub_{i}" for i in range(8)]
    try:
        for name in names:
            tree.add(name, hang=True)
        # Submission is normally instantaneous, so it is slowed here to make
        # the window reachable. Interrupting inside it used to leave the
        # executor's own shutdown(wait=True) to run every future submitted
        # so far -- booting engines after the interrupt.
        ready, rc, out = _run_driver(
            tree, ["--jobs", "2"], ["sub_0"], submit_delay=0.35)
        expect(ready, "the first probe booted its engine while submission continued")
        expect(rc == 130, f"the runner still exits 130 (got {rc})")
        started = [n for n in names if tree.started(n)]
        expect(len(started) < len(names),
               f"submission really was interrupted partway "
               f"(started {len(started)}/{len(names)})")
        leaked = []
        for name in names:
            pid = tree.engine_pid(name)
            if pid is not None and not wait_pid_gone(pid):
                leaked.append(name)
        expect(not leaked,
               f"no engine survives the interrupt (still running: {leaked})")
    finally:
        tree.cleanup()


class StopOnAdd(probe_runner_lifecycle.ProbeGroups):
    """Forces the interleaving a natural run only hits by chance.

    Shutdown begins AFTER run_one's pre-Popen stop check and before
    `reap_all` could have snapshotted the group this call just created --
    so that group is reachable from nothing but run_one itself.

    ``ready_marker`` widens that window rather than changing its shape:
    production constrains only WHERE the flag may be set relative to the
    runner, never how far the probe has got by then, and a descheduled
    worker thread can leave it arbitrarily far along. Waiting until the
    probe is genuinely up is what makes the case discriminating -- signal
    it at t=0 instead and it dies to the default SIGTERM action before it
    has installed the handler under test, so a runner with no escalation
    at all would pass.
    """

    def __init__(self, ready_marker: Path | None = None) -> None:
        super().__init__()
        self._ready = ready_marker

    def add(self, pgid: int) -> None:
        super().add(pgid)
        if self._ready is not None:
            wait_file(self._ready)
        self.stopping.set()


def test_probe_launched_into_shutdown_is_killed_promptly() -> None:
    print("\n-- a probe launched into a starting shutdown is ended, not left to run")
    tree = Tree()
    try:
        # Ignores SIGTERM, so only a real escalation ends it. Without one it
        # would sit in communicate() for the whole --timeout (900 s in
        # production) while the interrupted runner waited on this worker.
        script = tree.add("late", hang=True, ignore_term=True,
                          engine_ignores_term=True)
        started = time.monotonic()
        with patched(tree):
            ok, timed_out, elapsed, out = probe_runner_lifecycle.run_one(
                script, None, 30.0,
                StopOnAdd(tree.root / "late.enginepid"))
        wall = time.monotonic() - started
        pid = tree.engine_pid("late")
        expect(ok is False, "the probe is not reported as ok")
        expect(timed_out is False, "and not as a timeout -- it was cut short")
        expect(wall < 12.0,
               f"run_one returns on the teardown grace, not the --timeout "
               f"(took {wall:.1f}s of a 30s timeout)")
        expect(pid is not None and wait_pid_gone(pid),
               f"the engine it had already booted is gone too (pid {pid})")
    finally:
        tree.cleanup()


def test_ctrl_c_in_the_launch_window_leaves_nothing() -> None:
    print("\n-- a Ctrl-C in the launch window still reaps the probe it spawned")
    tree = Tree()
    try:
        script = tree.add("launch", hang=True)
        launched: dict[str, int] = {}
        real_popen = subprocess.Popen

        def popen_then_interrupt(*a, **kw):
            # Deliver a REAL SIGINT in the exact window the concern names:
            # the probe is spawned, and run_one has not yet recorded its
            # pgid. Undeferred, the KeyboardInterrupt lands here and the
            # Popen object never reaches run_one at all, so nothing can
            # reap the probe or the engine it goes on to boot.
            #
            # ONCE, and only for the probe launch. This patches the shared
            # subprocess module, and the reap itself shells out to `ps`;
            # firing again there would interrupt the teardown being tested
            # and look exactly like the leak this case is checking for.
            proc = real_popen(*a, **kw)
            if not launched:
                launched["pgid"] = proc.pid
                os.kill(os.getpid(), signal.SIGINT)
            return proc

        raised = None
        probe_runner_lifecycle.subprocess.Popen = popen_then_interrupt
        try:
            with patched(tree):
                probe_runner_lifecycle.run_one(script, None, 120.0, probe_runner_lifecycle.ProbeGroups())
        except KeyboardInterrupt:
            raised = "KeyboardInterrupt"
        except BaseException as exc:  # pragma: no cover - reported below
            raised = repr(exc)
        finally:
            probe_runner_lifecycle.subprocess.Popen = real_popen
        expect(raised == "KeyboardInterrupt",
               f"the interrupt still reaches the caller (got {raised})")
        expect("pgid" in launched, "the probe really was spawned")
        pid = launched.get("pgid")
        expect(pid is not None and wait_pid_gone(pid),
               f"the probe process itself is gone (pid {pid})")
        engine = tree.engine_pid("launch")
        expect(engine is None or wait_pid_gone(engine),
               f"and so is anything it managed to boot (pid {engine})")
    finally:
        tree.cleanup()


def test_liveness_check_does_not_count_a_zombie_as_running() -> None:
    print("\n-- this suite's own liveness check reads a zombie as gone, not running")
    # Every "the engine is gone" assertion below rests on pid_alive, and a
    # pid_alive that answered "running" for a dead-but-unreaped process
    # would fail them all (CI, 2026-08-17), while one that answered "gone"
    # for a LIVE process would pass them all vacuously. Both directions are
    # pinned here so the predicate itself is gated rather than trusted.
    zombie = subprocess.Popen([sys.executable, "-c", "pass"])
    try:
        deadline = time.monotonic() + 15.0
        while time.monotonic() < deadline:
            if process_state(zombie.pid) == "Z":
                break
            time.sleep(0.05)
        expect(process_state(zombie.pid) == "Z",
               f"a real zombie was produced and its state is readable "
               f"(got {process_state(zombie.pid)!r})")
        naive_says_alive = True
        try:
            os.kill(zombie.pid, 0)
        except ProcessLookupError:
            naive_says_alive = False
        expect(naive_says_alive,
               "os.kill(pid, 0) still reports it -- so it alone cannot be "
               "the check, which is the CI failure this pins")
        expect(pid_alive(zombie.pid) is False,
               "pid_alive reports the zombie as NOT running")
    finally:
        zombie.wait()
    live = subprocess.Popen([sys.executable, "-c", "import time; time.sleep(60)"])
    try:
        expect(pid_alive(live.pid) is True,
               "and it still reports a genuinely running process as alive")
    finally:
        live.kill()
        live.wait()


def free_port() -> int:
    """A port nothing is listening on right now."""
    with socket.socket(socket.AF_INET, socket.SOCK_STREAM) as probe:
        probe.bind(("127.0.0.1", 0))
        return probe.getsockname()[1]


def test_reap_returns_only_once_the_group_is_observed_gone() -> None:
    print("\n-- the reap does not return on the SIGKILL, but on the group going")
    # Sending SIGKILL is not the group being gone: delivery and teardown
    # are asynchronous. Asserted on the reap's OWN last observation rather
    # than by racing it, so the case is deterministic.
    tree = Tree()
    try:
        script = tree.add("kill_wait", exit_code=1, engine_ignores_term=True)
        seen: list[bool] = []
        real_running = probe_runner_lifecycle._group_running

        def watched(pgid: int) -> bool:
            answer = real_running(pgid)
            seen.append(answer)
            return answer

        # _group_running is the predicate the reap gates its return on: a
        # zombie is not a running member, so this is what "the group is
        # gone" actually means for a port about to be reused.
        probe_runner_lifecycle._group_running = watched
        try:
            with patched(tree):
                probe_runner_lifecycle.run_one(script, None, 120.0)
        finally:
            probe_runner_lifecycle._group_running = real_running
        expect(bool(seen), "the reap really did inspect the group")
        expect(True in seen,
               "it saw a live member first -- otherwise it reaped nothing and "
               "this case proves nothing")
        expect(seen and seen[-1] is False,
               f"its LAST look saw the group empty before returning "
               f"(observations ended {seen[-3:]})")
    finally:
        tree.cleanup()


def test_retry_can_rebind_the_port_a_killed_engine_held() -> None:
    print("\n-- a retry can bind the port a SIGTERM-ignoring engine held")
    tree = Tree()
    try:
        port = free_port()
        # Attempt 1 leaks an engine that ignores SIGTERM and owns `port`.
        # Only a reap that SIGKILLs it AND waits for the port to be
        # released lets attempt 2's engine bind the same port; otherwise
        # the retry hits exactly the #1190 abort this PR is about.
        tree.add("rebind", exit_code=1, engine_ignores_term=True,
                 hold_port=port)
        rc, out = _main_with(tree, ["--only", "rebind", "--exact",
                                    "--retries", "1", "--port", str(port)])
        binds = tree.binds("rebind")
        expect(rc == 1, f"the probe still fails on both attempts (got {rc})")
        expect(len(binds) == 2,
               f"both attempts really booted an engine that tried to bind "
               f"(got {binds})")
        expect(binds == ["bound", "bound"],
               f"the retry's engine bound the same port the first one held "
               f"(got {binds})")
    finally:
        tree.cleanup()


# --------------------------------------------------------------------------
# Reserved port spans (#1571)
# --------------------------------------------------------------------------
def free_port_span(width: int) -> int:
    """A base with `width` consecutive ports all bindable right now.

    An ephemeral port is one port; a multi-port probe needs a run of
    them, and nothing hands those out. So candidate bases are tried until
    the whole run binds at once — which is also the only way to know the
    run is really free rather than merely starting at a free port.
    """
    for _ in range(400):
        base = random.randint(20000, 59000)
        held: list[socket.socket] = []
        try:
            for port in range(base, base + width):
                sock = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
                sock.bind(("127.0.0.1", port))
                held.append(sock)
        except OSError:
            continue
        finally:
            for sock in held:
                sock.close()
        if len(held) == width:
            return base
    raise AssertionError(f"no free {width}-port span found")


def test_port_span_declaration_is_data_about_real_probes() -> None:
    print("\n-- the shipped PROBE_PORT_SPANS table names registered probes")
    known = {p[0] for p in probe_runner_registry.PROBES}
    unknown = sorted(k for k in probe_runner_registry.PROBE_PORT_SPANS if k not in known)
    expect(not unknown,
           f"every declared key names a registered probe (unknown: {unknown})")
    bad = sorted(k for k, v in probe_runner_registry.PROBE_PORT_SPANS.items()
                 if not isinstance(v, int) or isinstance(v, bool) or v < 1)
    expect(not bad,
           f"every declaration is a positive port COUNT (bad: {bad})")
    expect(probe_runner_registry.port_span("debug_console_boot") == 2,
           "debug_console_boot declares two ports -- it binds base and base+1")
    expect(probe_runner_registry.port_span("offscreen") == 2,
           "offscreen declares two ports -- its second engine runs alongside "
           "the first")
    expect(probe_runner_registry.port_span("combat_anim") == probe_runner_registry.DEFAULT_PORT_SPAN
           == 1,
           "an undeclared probe reserves its base alone")
    expect(list(probe_runner_registry.reserved_ports("debug_console_boot", 9400))
           == [9400, 9401],
           "a declared count N reserves base .. base+N-1, contiguously")
    expect(list(probe_runner_registry.reserved_ports("combat_anim", 9400)) == [9400],
           "and an undeclared probe reserves exactly its base")


def test_parallel_allocation_never_overlaps() -> None:
    print("\n-- the parallel allocation lays declared spans end to end")
    # Every registered probe at once: whatever the table says, no two
    # selected probes may be handed a port the other may bind.
    ports = probe_runner_registry.allocate_parallel_ports(probe_runner_registry.PROBES)
    expect(len(ports) == len(probe_runner_registry.PROBES),
           "every selected probe gets exactly one base")
    claimed: dict[int, str] = {}
    overlaps: list[str] = []
    for (key, _, _), base in zip(probe_runner_registry.PROBES, ports):
        for port in probe_runner_registry.reserved_ports(key, base):
            if port in claimed:
                overlaps.append(f"{key} and {claimed[port]} both reserve {port}")
            claimed[port] = key
    expect(not overlaps,
           f"no two probes reserve the same port (overlaps: {overlaps[:3]})")
    expect(ports == sorted(ports) and len(set(ports)) == len(ports),
           "bases are handed out in registry order, each strictly after the last")
    expect(ports[0] == probe_runner_registry.PARALLEL_PORT_BASE,
           f"the default origin is still {probe_runner_registry.PARALLEL_PORT_BASE} "
           f"(got {ports[0]})")

    # The exact pair from #1571, in the order that broke: a two-port
    # probe immediately before a one-port one.
    pair = [("debug_console_boot", "a.py", ""), ("transactional_load", "b.py", "")]
    got = probe_runner_registry.allocate_parallel_ports(pair, 9400)
    expect(got == [9400, 9402],
           f"debug_console_boot's neighbour starts past its span (got {got})")
    expect(probe_runner_registry.allocate_parallel_ports(pair, 9500) == [9500, 9502],
           "and the layout follows a caller-supplied origin")


def test_gui_port_refusal_covers_the_whole_span() -> None:
    print("\n-- a span that REACHES the GUI port is refused, not just a base "
          "that equals it")
    expect(probe_runner_registry.GUI_PORT == 8008,
           f"the GUI port is still 8008 (got {probe_runner_registry.GUI_PORT})")
    spans = {"wide": 2}
    saved = probe_runner_registry.PROBE_PORT_SPANS
    probe_runner_registry.PROBE_PORT_SPANS = spans
    try:
        conflicts = probe_runner_registry.gui_port_conflicts(
            [("wide", 8007), ("narrow", 8007), ("wide", 9400)])
        expect(conflicts == [("wide", 8007)],
               f"only the span that actually covers 8008 conflicts "
               f"(got {conflicts})")
        text = probe_runner_registry.describe_gui_conflicts(conflicts)
        expect("8007-8008" in text and "wide" in text and "8008" in text,
               f"the refusal names the probe and the span (got {text!r})")
        expect(probe_runner_registry.gui_port_conflicts(
                   [("wide", 8007), ("wide", 8007)]) == [("wide", 8007)],
               "the same probe at the same base is one conflict, not two -- a "
               "parallel plan lists it twice (allocation and solo-retry origin)")
    finally:
        probe_runner_registry.PROBE_PORT_SPANS = saved

    for jobs in ("1", "2"):
        tree = Tree()
        try:
            tree.add("wide", dwell=0.0)
            tree.add("narrow", dwell=0.0)
            rc, out = _main_refusal(
                tree, ["--only", "wide,narrow", "--exact", "--jobs", jobs,
                       "--port", "8007"],
                spans={"wide": 2})
            expect(rc == 2,
                   f"--jobs {jobs} --port 8007 with a two-port probe is a bad "
                   f"invocation (got {rc})")
            expect("8008" in out and "wide" in out,
                   f"and says which probe reaches the GUI port (got {out!r})")
            expect(not tree.started("wide") and not tree.started("narrow"),
                   "nothing was launched -- the plan is validated before any "
                   "subprocess exists")
        finally:
            tree.cleanup()

    # The pre-#1571 exact-base refusal is unchanged.
    tree = Tree()
    try:
        tree.add("narrow")
        rc, out = _main_refusal(tree, ["--only", "narrow", "--exact",
                                       "--port", "8008"])
        expect(rc != 0 and "8008" in out,
               f"--port 8008 itself is still refused (got {rc}, {out!r})")
        expect(not tree.started("narrow"), "and still starts nothing")
    finally:
        tree.cleanup()


def test_port_with_jobs_bases_the_parallel_allocation() -> None:
    print("\n-- --port is HONOURED with --jobs: it is the allocation origin")
    tree = Tree()
    try:
        base = free_port_span(4)
        tree.add("wide", bind_span=2)
        tree.add("narrow", bind_span=1)
        rc, out = _main_with(tree, ["--only", "wide,narrow", "--exact",
                                    "--jobs", "2", "--retries", "0",
                                    "--port", str(base)],
                             spans={"wide": 2})
        expect(rc == 0, f"both probes passed (got {rc})\n{out}")
        expect(tree.ports("wide") == [base],
               f"the first probe is based at --port itself "
               f"(got {tree.ports('wide')}, wanted [{base}])")
        expect(tree.ports("narrow") == [base + 2],
               f"the second starts past the first's TWO-port span "
               f"(got {tree.ports('narrow')}, wanted [{base + 2}])")
    finally:
        tree.cleanup()

    # Sequential is unchanged: one base, handed to every probe.
    tree = Tree()
    try:
        base = free_port_span(2)
        tree.add("wide", bind_span=2)
        tree.add("narrow", bind_span=1)
        rc, out = _main_with(tree, ["--only", "wide,narrow", "--exact",
                                    "--port", str(base)],
                             spans={"wide": 2})
        expect(rc == 0, f"sequentially both still pass (got {rc})\n{out}")
        expect(tree.ports("wide") == [base] and tree.ports("narrow") == [base],
               f"and both still get the same base "
               f"(wide {tree.ports('wide')}, narrow {tree.ports('narrow')})")
    finally:
        tree.cleanup()

    # Unset, the parallel allocation still starts at the default origin.
    tree = Tree()
    try:
        tree.add("alpha")
        tree.add("beta")
        rc, _ = _main_with(tree, ["--only", "alpha,beta", "--exact",
                                  "--jobs", "2"])
        expect(rc == 0, f"the default-origin run still passes (got {rc})")
        expect(tree.ports("alpha") == [probe_runner_registry.PARALLEL_PORT_BASE]
               and tree.ports("beta") == [probe_runner_registry.PARALLEL_PORT_BASE + 1],
               f"unset --port keeps the 9400 origin "
               f"(alpha {tree.ports('alpha')}, beta {tree.ports('beta')})")
    finally:
        tree.cleanup()


def test_a_two_port_probe_never_takes_its_neighbours_base() -> None:
    print("\n-- a two-port probe and its neighbour both bind, concurrently")
    # The #1571 defect, reproduced against real sockets rather than
    # asserted: `wide` binds base and base+1 and holds both; `narrow`
    # binds whatever base it was handed, AFTER `wide` has bound (the
    # delay makes the order deterministic instead of a race).
    tree = Tree()
    try:
        base = free_port_span(4)
        tree.add("wide", bind_span=2, dwell=2.0)
        tree.add("narrow", bind_span=1, bind_delay=0.7)
        rc, out = _main_with(tree, ["--only", "wide,narrow", "--exact",
                                    "--jobs", "2", "--retries", "0",
                                    "--port", str(base)],
                             spans={"wide": 2})
        expect(rc == 0, f"both probes passed together (got {rc})\n{out}")
        expect(tree.binds("wide") == ["bound", "bound"],
               f"the two-port probe bound BOTH its ports "
               f"(got {tree.binds('wide')})")
        expect(tree.binds("narrow") == ["bound"],
               f"and its neighbour bound its own, uncontested "
               f"(got {tree.binds('narrow')})")
    finally:
        tree.cleanup()

    # The control: with the span UNDECLARED the allocator is back to
    # stride 1 and the same two probes collide. Without this the test
    # above could pass on a layout that never overlapped anyway.
    tree = Tree()
    try:
        base = free_port_span(4)
        tree.add("wide", bind_span=2, dwell=2.0)
        tree.add("narrow", bind_span=1, bind_delay=0.7)
        rc, out = _main_with(tree, ["--only", "wide,narrow", "--exact",
                                    "--jobs", "2", "--retries", "0",
                                    "--port", str(base)],
                             spans={})
        expect(rc == 1,
               f"an undeclared two-port probe really does collide (got {rc})")
        expect(tree.ports("narrow") == [base + 1],
               f"because stride 1 hands the neighbour base+1 "
               f"(got {tree.ports('narrow')})")
        expect(tree.binds("narrow") == ["inuse"],
               f"which the two-port probe is already holding "
               f"(got {tree.binds('narrow')})")
    finally:
        tree.cleanup()


def test_the_synthetic_fixtures_are_valid_python() -> None:
    print("\n-- the synthetic probe and engine sources are valid Python")
    # These are generated source strings, and a mistake in one does NOT
    # announce itself. An unescaped newline inside DESCENDANT_SRC once
    # defeated textwrap.dedent, leaving every line indented and the engine
    # unable to start at all: nothing booted, so nothing needed reaping,
    # and the suite reported sixteen "the engine is gone" failures instead
    # of one broken fixture. Compiling them here names that mistake.
    tree = Tree()
    try:
        problems = []
        try:
            compile(DESCENDANT_SRC, "<descendant>", "exec")
        except SyntaxError as exc:
            problems.append(f"DESCENDANT_SRC: {exc}")
        expect(DESCENDANT_SRC.splitlines()[0].startswith("#"),
               "DESCENDANT_SRC really was dedented (first line is flush left)")
        # One of every shape the cases below actually generate.
        variants = {
            "plain": {},
            "failing": {"exit_code": 1, "tail_lines": 3},
            "hanging": {"hang": True, "ignore_term": True},
            "stubborn engine": {"engine_ignores_term": True},
            "no engine": {"descendant": False},
            "port holder": {"hold_port": 9999},
            "dwelling": {"dwell": 0.25, "descendant": False},
            "progress": {"progress": (("phase", "engine A", "build it"),),
                         "tail_lines": 2, "descendant": False},
        }
        for label, kw in variants.items():
            try:
                compile(probe_src(tree.root, "fixture", **kw),
                        f"<probe {label}>", "exec")
            except SyntaxError as exc:
                problems.append(f"probe_src({label}): {exc}")
        expect(not problems, f"every generated source compiles ({problems})")
    finally:
        tree.cleanup()


def test_group_running_ignores_a_zombie_only_group() -> None:
    print("\n-- a group holding only a zombie does not count as running")
    # This is what lets the reap's waits finish promptly instead of
    # spending the whole grace, and then the post-SIGKILL settle, on an
    # engine that has already exited and released its port. Signals cannot
    # see the difference: a zombie-only group answers EPERM on macOS and
    # succeeds on Linux, so killpg alone calls it alive either way.
    #
    # Tested directly, because the platform decides whether it is even
    # reachable end to end: macOS reaps orphans through launchd almost at
    # once, while a container's PID 1 may never reap at all.
    child = subprocess.Popen([sys.executable, "-c", "pass"],
                             start_new_session=True)
    pgid = child.pid
    try:
        deadline = time.monotonic() + 15.0
        while time.monotonic() < deadline:
            if process_state(pgid) == "Z":
                break
            time.sleep(0.05)
        expect(process_state(pgid) == "Z",
               f"the child is a zombie and nothing has reaped it "
               f"(state {process_state(pgid)!r})")
        expect(probe_runner_lifecycle._group_alive(pgid) is True,
               "signals still report its group -- so the distinction below is "
               "real here, not an artifact of it having already gone")
        expect(probe_runner_lifecycle._group_running(pgid) is False,
               "but nothing in it is RUNNING")
    finally:
        child.wait()
    live = subprocess.Popen([sys.executable, "-c", "import time; time.sleep(60)"],
                            start_new_session=True)
    try:
        expect(probe_runner_lifecycle._group_running(live.pid) is True,
               "while a group with a live member still counts as running")
    finally:
        live.kill()
        live.wait()



# --------------------------------------------------------------------------
# Durable progress records and timeout attribution (#1768)
#
# The loss these cases are about is invisible to every other test here: a
# probe's phase output is block-buffered in the child, so a `--timeout`
# SIGKILL discards it and the failure artifact names no phase at all. The
# pure cases below pin the ONE shared convention (`run_probes.py` defines
# it; `persistence_contract_sweep.py` and the runner's own nested-attempt
# records both use it), and the subprocess cases prove it survives a real
# forced termination and reaches the DEFAULT failure presentation on both
# of that presentation's two paths.
# --------------------------------------------------------------------------
class ProgressStub:
    """Stands in for "this line is not a progress record" in a filter."""
    kind = ""


def progress_records(out: str) -> list[probe_runner_diagnostics.ProgressRecord]:
    """Every progress record in some output, in order."""
    return [record for record in
            (probe_runner_diagnostics.parse_progress(line) for line in out.splitlines())
            if record is not None]


def record_pairs(out: str) -> list[tuple[str, str]]:
    """`(kind, identity)` for every progress record, in order."""
    return [(record.kind, record.identity) for record in progress_records(out)]


def test_progress_records_round_trip_and_stay_out_of_the_verdict_shape() -> None:
    print("\n-- a progress record round-trips, and can never be miscounted "
          "as a verdict announcement")
    now = time.time()
    line = probe_runner_diagnostics.format_progress(
        "phase", "engine A", "build the scenario, save 'gen1'",
        elapsed=12.34, now=now)
    record = probe_runner_diagnostics.parse_progress(line)
    expect(record is not None, f"the rendered record parses back ({line!r})")
    expect(record.kind == "phase" and record.identity == "engine A",
           f"with its kind and identity intact (got {record!r})")
    expect(record.detail == "build the scenario, save 'gen1'",
           f"and its detail intact (got {record.detail!r})")
    expect("+12.3s" in record.stamp,
           f"the stamp carries the elapsed offset (got {record.stamp!r})")
    expect(":" in record.stamp,
           f"and a wall-clock time, so two producers sharing one pipe can "
           f"be ordered against each other (got {record.stamp!r})")

    # Free text may contain the field separator; only the first three
    # fields are structural.
    awkward = probe_runner_diagnostics.format_progress(
        "end", "chop (chop_probe.py) attempt 1/2", "FAIL | exit 1",
        elapsed=1.0, now=now)
    expect(probe_runner_diagnostics.parse_progress(awkward).detail == "FAIL | exit 1",
           "a detail containing the separator survives the round trip")
    expect(probe_runner_diagnostics.parse_progress(awkward).identity
           == "chop (chop_probe.py) attempt 1/2",
           "and the identity is not confused by it")

    expect(probe_runner_diagnostics.parse_progress("[3/12] chop_probe.py ... PASS (4.0s)")
           is None,
           "an ordinary verdict announcement is not a progress record")
    expect(probe_runner_diagnostics.parse_progress("diagnostic line 7") is None,
           "and neither is ordinary probe output")

    # The other direction, which is the one that could break a shipped
    # test: `progress_lines` counts a verdict by "starts with [ and
    # contains ' <script> ... '". A progress record naming a script must
    # not match that shape.
    dispatch = probe_runner_diagnostics.format_progress(
        "begin", probe_runner_diagnostics.attempt_identity("chop", "chop_probe.py", 1, 2),
        "dispatched", elapsed=0.1, now=now)
    expect(progress_lines(dispatch, "chop_probe.py") == 0,
           f"a dispatch record naming a script is not counted as that "
           f"probe's verdict ({dispatch!r})")

    try:
        probe_runner_diagnostics.format_progress("nonsense", "x", "y", elapsed=0.0, now=now)
    except ValueError:
        expect(True, "an unknown record kind is refused at the source")
    else:
        expect(False, "an unknown record kind should be refused at the source")


def test_progress_attribution_derives_the_in_flight_set() -> None:
    print("\n-- attribution names the latest phase and every attempt "
          "started without finishing")
    now = time.time()

    def line(kind, identity, detail, elapsed):
        return probe_runner_diagnostics.format_progress(kind, identity, detail,
                                          elapsed=elapsed, now=now)

    expect(probe_runner_diagnostics.progress_attribution("") == [],
           "a capture with no progress records yields no attribution at all")
    expect(probe_runner_diagnostics.progress_attribution("just some probe output\n") == [],
           "and neither does ordinary probe output")

    alpha = probe_runner_diagnostics.attempt_identity("chop", "chop_probe.py", 1, 2)
    beta = probe_runner_diagnostics.attempt_identity("till", "till_probe.py", 1, 2)
    gamma = probe_runner_diagnostics.attempt_identity("till", "till_probe.py", 2, 2)
    capture = "\n".join([
        line("phase", "engine A", "build the scenario", 0.1),
        "some ordinary output",
        line("phase", "engine C", "load 'gen2', save 'gen3'", 300.0),
        line("phase", "cross-probes", "running 11 probe(s)", 500.0),
        line("begin", alpha, "dispatched", 500.1),
        line("begin", beta, "dispatched", 500.2),
        line("end", beta, "FAIL (10.0s)", 510.2),
        line("begin", gamma, "solo retry", 510.3),
    ]) + "\n"
    got = probe_runner_diagnostics.progress_attribution(capture)
    text = "\n".join(got)
    expect(any("cross-probes" in ln for ln in got),
           f"the LATEST phase is named, not the first (got {got!r})")
    expect("engine A" not in text and "engine C" not in text,
           f"and the superseded phases are not (got {got!r})")
    expect("+500.0s" in text,
           f"with the offset that quantifies how long it occupied "
           f"(got {got!r})")
    expect(any(alpha in ln for ln in got) and any(gamma in ln for ln in got),
           f"both attempts started without an end are named (got {got!r})")
    expect(beta not in text,
           f"and the attempt that completed is NOT reported in flight "
           f"(got {got!r})")
    expect(text.index(alpha) < text.index(gamma),
           "the in-flight attempts are listed in dispatch order")

    # A retry that finishes clears only its own attempt.
    finished = capture + line("end", gamma, "PASS (5.0s)", 515.0) + "\n"
    remaining = "\n".join(probe_runner_diagnostics.progress_attribution(finished))
    expect(alpha in remaining and gamma not in remaining,
           f"completing the retry leaves only the still-running attempt "
           f"(got {remaining!r})")


def test_progress_survives_a_forced_timeout_outside_the_ordinary_tail() -> None:
    print("\n-- a phase record emitted before a SIGKILL timeout reaches the "
          "default failure report, even buried under more than --tail lines")
    tree = Tree()
    try:
        # 40 ordinary lines after the record, against the default
        # `--tail 25`: the record is provably outside the tail, so only
        # the attribution can surface it. The probe and its engine both
        # ignore SIGTERM, so this is the real escalate-to-SIGKILL path.
        tree.add("slow", progress=(("phase", "engine C",
                                    "fresh process, load 'gen2', save 'gen3'"),),
                 tail_lines=40, hang=True, ignore_term=True,
                 engine_ignores_term=True)
        rc, out = _main_with(tree, ["--timeout", "3"])
        expect(rc == 1, f"the timed-out run still fails (exit {rc})")
        expect("TIMEOUT" in out, "and is reported as a TIMEOUT")
        expect("progress: latest phase entered at" in out,
               f"the attribution line is printed:\n{out}")
        expect("engine C" in out and "load 'gen2', save 'gen3'" in out,
               f"naming the phase that was active at the kill:\n{out}")
        expect(probe_runner_diagnostics.PROGRESS_MARKER not in out,
               f"the raw record is NOT reprinted -- it fell outside the "
               f"25-line tail:\n{out}")
        expect("diagnostic line 39" in out,
               "the ordinary tail is preserved as context")
        expect("diagnostic line 0" not in out,
               f"and the complete capture is NOT dumped:\n{out}")
    finally:
        tree.cleanup()


def test_in_flight_attempts_are_derivable_from_the_default_report() -> None:
    print("\n-- a timeout names every nested attempt started without a "
          "completion, and no completed one")
    tree = Tree()
    try:
        # A probe standing in for the sweep: it enters a phase, then its
        # nested runner dispatches two attempts into the SAME pipe and
        # completes one before everything is killed. The records are the
        # real ones -- `probe_src` emits them through
        # `probe_runner_diagnostics.ProgressEmitter` -- so this is the shipped
        # convention crossing a real process boundary and a real SIGKILL.
        finished = probe_runner_diagnostics.attempt_identity("chop", "chop_probe.py", 1, 2)
        running = probe_runner_diagnostics.attempt_identity("till", "till_probe.py", 1, 2)
        retrying = probe_runner_diagnostics.attempt_identity("chop", "chop_probe.py", 2, 2)
        tree.add("nested",
                 progress=(("phase", "cross-probes", "running 2 probe(s)"),
                           ("begin", finished, "dispatched"),
                           ("begin", running, "dispatched"),
                           ("end", finished, "FAIL (1.0s)"),
                           ("begin", retrying, "solo retry")),
                 tail_lines=40, hang=True, ignore_term=True,
                 engine_ignores_term=True)
        rc, out = _main_with(tree, ["--timeout", "3"])
        expect(rc == 1, f"the run fails (exit {rc})")
        expect("2 nested probe attempt(s) still in flight" in out,
               f"the in-flight count is reported:\n{out}")
        expect(running in out,
               f"the attempt still running is named ({running!r}):\n{out}")
        expect(retrying in out,
               f"and so is the solo retry in flight ({retrying!r}):\n{out}")
        expect(out.count(finished) == 0,
               f"the attempt that completed is not reported in flight:\n{out}")
        expect("cross-probes" in out,
               f"and the phase it happened in is still named:\n{out}")
        expect("diagnostic line 0" not in out,
               "without dumping the complete capture")
    finally:
        tree.cleanup()


def test_parallel_dispatch_and_retry_records_name_every_attempt() -> None:
    print("\n-- the parallel path records every attempt before it begins, "
          "and the solo retry too, without disturbing the verdict lines")
    tree = Tree()
    try:
        tree.add("alpha", dwell=0.3, descendant=False)
        tree.add("beta", exit_code=1, tail_lines=40, descendant=False,
                 progress=(("phase", "engine A", "build the scenario"),))
        rc, out = _main_with(tree, ["--jobs", "2", "--retries", "1"])
        expect(rc == 1, f"the failing probe still fails the run (exit {rc})")

        pairs = record_pairs(out)
        alpha_1 = probe_runner_diagnostics.attempt_identity("alpha", "alpha_probe.py", 1, 2)
        beta_1 = probe_runner_diagnostics.attempt_identity("beta", "beta_probe.py", 1, 2)
        beta_2 = probe_runner_diagnostics.attempt_identity("beta", "beta_probe.py", 2, 2)
        for kind, identity in (("begin", alpha_1), ("end", alpha_1),
                               ("begin", beta_1), ("end", beta_1),
                               ("begin", beta_2), ("end", beta_2)):
            expect((kind, identity) in pairs,
                   f"the runner emitted a {kind} record for {identity!r} "
                   f"(got {pairs!r})")
        expect(pairs.index(("begin", beta_1)) < pairs.index(("end", beta_1))
               < pairs.index(("begin", beta_2)),
               f"the batch attempt is recorded before it begins and the "
               f"retry only after it ended (got {pairs!r})")
        expect(pairs.count(("begin", beta_1)) == 1,
               f"exactly one dispatch record per attempt (got {pairs!r})")

        # The shipped concurrency tests count verdict announcements by
        # shape; the new records must not join that count.
        expect(progress_lines(out, "alpha_probe.py") == 1
               and progress_lines(out, "beta_probe.py") == 1,
               "each probe still announces exactly one verdict")

        # Requirement 4 on the OTHER default failure path: the parallel
        # end-of-run tail block.
        block = out.split("--- beta_probe.py (FAIL) ---")[-1]
        expect("progress: latest phase entered at" in block
               and "engine A" in block,
               f"the parallel failure block carries the attribution too:\n{out}")
        expect("diagnostic line 0" not in block,
               "and still does not dump the complete capture")

        # Close the loop: the records the runner ACTUALLY printed, read
        # back by the real consumer. Dropping the completions is what a
        # kill mid-batch leaves behind, and both dispatches must then be
        # reported in flight.
        mid_batch = "\n".join(
            line for line in out.splitlines()
            if (probe_runner_diagnostics.parse_progress(line) or ProgressStub).kind != "end")
        derived = "\n".join(probe_runner_diagnostics.progress_attribution(mid_batch))
        expect(alpha_1 in derived and beta_1 in derived and beta_2 in derived,
               f"the runner's own records derive the in-flight set when the "
               f"completions are missing (got {derived!r})")
    finally:
        tree.cleanup()


# --------------------------------------------------------------------------
# Durable failure records and the retained failed check (#1982)
#
# #1768's cases above are about a probe that never finished. These are
# about one that finished and FAILED: it printed its per-check verdicts
# to a block-buffered stdout pipe and its terminal `FAIL:` summary to an
# unbuffered stderr the runner merges into that same pipe, so the
# `FAIL:` lines OVERTOOK the buffered output and landed at the top of the
# capture while `--tail 25` printed only the bottom. A real coordinated
# run spent 279.5 s to report "1 check(s) FAILED" and name the check
# nowhere.
#
# The synthetic probes below reproduce that displacement exactly -- real
# `FailureEmitter` records flushed ahead of more than `--tail` buffered
# lines -- and require the DEFAULT presentation, on both of its paths, to
# surface every one of them without dumping the capture.
# --------------------------------------------------------------------------
def failure_records(out: str) -> list[probe_runner_diagnostics.FailureRecord]:
    """Every failure record in some output, in order."""
    return [record for record in
            (probe_runner_diagnostics.parse_failure(line) for line in out.splitlines())
            if record is not None]


def test_failure_records_round_trip_and_stay_off_the_progress_channel() -> None:
    print("\n-- a failure record round-trips, and is not a progress record")
    now = time.time()
    line = probe_runner_diagnostics.format_failure(
        "check", "location_embark_probe",
        "the discovered icon never appeared at (12,7)",
        elapsed=279.4, now=now)
    record = probe_runner_diagnostics.parse_failure(line)
    expect(record is not None, f"the rendered record parses back ({line!r})")
    expect(record.kind == "check"
           and record.identity == "location_embark_probe",
           f"with its kind and identity intact (got {record!r})")
    expect(record.detail == "the discovered icon never appeared at (12,7)",
           f"and its detail intact (got {record.detail!r})")
    expect("+279.4s" in record.stamp,
           f"the stamp carries the elapsed offset, so 'at the very end of "
           f"a 279.5 s run' is readable (got {record.stamp!r})")

    awkward = probe_runner_diagnostics.format_failure(
        "setup", "probe", "no [flat] site | tried 6 seeds",
        elapsed=1.0, now=now)
    expect(probe_runner_diagnostics.parse_failure(awkward).detail
           == "no [flat] site | tried 6 seeds",
           "a detail containing the separator survives the round trip")

    # A detail spanning lines would split into a marked line and an
    # unmarked orphan the parser could only drop; one record is one line.
    multi = probe_runner_diagnostics.format_failure(
        "check", "probe", "first\nsecond\n   third", elapsed=1.0, now=now)
    expect("\n" not in multi,
           f"a multi-line detail is collapsed to one line ({multi!r})")
    expect(probe_runner_diagnostics.parse_failure(multi).detail == "first second third",
           f"keeping every word (got {probe_runner_diagnostics.parse_failure(multi)!r})")

    # The two conventions must not read each other's records: #1768's
    # promise is that a capture with no PROGRESS records yields no
    # progress attribution at all, and a failing probe emitting only
    # failure records must not break it.
    expect(probe_runner_diagnostics.parse_progress(line) is None,
           "a failure record is not a progress record")
    expect(probe_runner_diagnostics.progress_attribution(line + "\n") == [],
           "and yields no progress attribution")
    progress = probe_runner_diagnostics.format_progress("phase", "engine A", "build",
                                          elapsed=1.0, now=now)
    expect(probe_runner_diagnostics.parse_failure(progress) is None,
           "and a progress record is not a failure record")
    expect(probe_runner_diagnostics.failure_attribution(progress + "\n") == [],
           "nor does it yield failure attribution")

    expect(probe_runner_diagnostics.parse_failure("FAIL: something broke") is None,
           "an ordinary printed FAIL line is not a record")
    expect(progress_lines(line, "location_embark_probe.py") == 0,
           f"and a record naming a probe is not counted as its verdict "
           f"({line!r})")

    try:
        probe_runner_diagnostics.format_failure("nonsense", "x", "y", elapsed=0.0, now=now)
    except ValueError:
        expect(True, "an unknown record kind is refused at the source")
    else:
        expect(False, "an unknown record kind should be refused at the source")


def test_failure_attribution_names_every_recorded_failure_once() -> None:
    print("\n-- attribution names every recorded failure exactly once, "
          "keeps the two vocabularies apart, and carries the context")
    now = time.time()

    def line(kind, identity, detail, elapsed):
        return probe_runner_diagnostics.format_failure(kind, identity, detail,
                                         elapsed=elapsed, now=now)

    expect(probe_runner_diagnostics.failure_attribution("") == [],
           "a capture with no failure records yields no attribution at all")
    expect(probe_runner_diagnostics.failure_attribution("just some probe output\n") == [],
           "and neither does ordinary probe output")

    capture = "\n".join([
        line("setup", "stamp_probe", "no conforming [flat] site", 4.0),
        "some ordinary output",
        line("check", "stamp_probe", "room at (12,7) never stamped", 9.0),
        line("check", "stamp_probe", "structure.clear left the floor", 11.0),
        line("context", "engine log", "/tmp/x/engine.log", 12.0),
        line("context", "engine log tail", "vulkan: device lost", 12.0),
    ]) + "\n"
    got = probe_runner_diagnostics.failure_attribution(capture)
    text = "\n".join(got)
    expect("3 recorded failure(s)" in text,
           f"the count covers both vocabularies (got {got!r})")
    for detail in ("no conforming [flat] site",
                   "room at (12,7) never stamped",
                   "structure.clear left the floor"):
        expect(text.count(detail) == 1,
               f"{detail!r} is named exactly once (got {got!r})")
    expect("SETUP FAILURE: no conforming [flat] site" in text,
           f"a setup failure keeps its own vocabulary (got {got!r})")
    expect("FAIL: room at (12,7) never stamped" in text,
           f"and an ordinary failure keeps its own (got {got!r})")
    expect(text.index("room at (12,7)") < text.index("structure.clear"),
           "recorded failures are listed in the order they happened")
    expect("engine log: /tmp/x/engine.log" in text
           and "vulkan: device lost" in text,
           f"and the bounded context is carried beside them (got {got!r})")
    expect(text.index("structure.clear") < text.index("/tmp/x/engine.log"),
           "with the failures first and the context after them")

    # The tail is printed BESIDE the attribution, so the records
    # themselves must be withheld from it or every failure appears twice.
    stripped = probe_runner_diagnostics.without_failure_records(capture)
    expect(probe_runner_diagnostics.FAILURE_MARKER not in stripped,
           f"the records are withheld from the ordinary tail ({stripped!r})")
    expect("some ordinary output" in stripped,
           "while everything else survives it")


def test_failed_checks_survive_outside_the_ordinary_tail() -> None:
    print("\n-- a completed failing probe's failed checks reach the default "
          "report, though its records sit above the 25-line tail")
    tree = Tree()
    try:
        # The observed shape: several failure records flushed into the
        # merged pipe, then 40 block-buffered ordinary lines against the
        # default `--tail 25`. Every record is provably outside the tail,
        # so only the attribution can surface it. A phase record rides
        # along, because #1982 requirement 4 wants the failure CLASS and
        # the phase both readable without rerunning the probe.
        tree.add("stamp", exit_code=1, tail_lines=40,
                 sentinel="sentinel: the very first line of this run",
                 progress=(("phase", "engine C", "fresh process, load 'gen2'"),),
                 failures=(("setup", "stamp_probe",
                            "no conforming [flat] site in 6 seeds"),
                           ("check", "stamp_probe",
                            "room at (12,7) never stamped on first load"),
                           ("check", "stamp_probe",
                            "structure.clear did not remove the anchor floor"),
                           ("context", "engine log", "/tmp/stamp/engine.log"),
                           ("context", "engine log tail",
                            "vulkan: swapchain out of date")))
        rc, out = _main_with(tree, [])
        expect(rc == 1, f"the failing run still fails (exit {rc})")
        expect("FAIL" in out, "and is reported as a FAIL")

        # Requirement 1 and 2: every failed check and its detail, named.
        expect("3 recorded failure(s)" in out,
               f"the recorded count is reported:\n{out}")
        for detail in ("no conforming [flat] site in 6 seeds",
                       "room at (12,7) never stamped on first load",
                       "structure.clear did not remove the anchor floor"):
            expect(out.count(detail) == 1,
                   f"{detail!r} is named exactly once:\n{out}")
        expect("SETUP FAILURE: no conforming [flat] site" in out,
               f"the setup vocabulary survives distinctly:\n{out}")

        # Requirement 4: the phase and the invocation context.
        expect("progress: latest phase entered at" in out and "engine C" in out,
               f"the phase the run was in is named too:\n{out}")
        expect("engine log: /tmp/stamp/engine.log" in out
               and "vulkan: swapchain out of date" in out,
               f"and the bounded engine-log context:\n{out}")

        # Requirement 6: bounded, not a dump. The sentinel is the very
        # first line of the run and stays omitted; the tail is the last
        # 25 ordinary lines and nothing more.
        expect("sentinel: the very first line" not in out,
               f"an early non-diagnostic line stays omitted:\n{out}")
        expect("diagnostic line 0" not in out,
               f"and the complete capture is NOT dumped:\n{out}")
        expect("diagnostic line 39" in out and "diagnostic line 15" in out,
               f"while the ordinary tail is preserved as context:\n{out}")
        expect("diagnostic line 14" not in out,
               f"bounded at exactly --tail lines:\n{out}")
        expect(probe_runner_diagnostics.FAILURE_MARKER not in out,
               f"and the raw records are not reprinted beside the "
               f"attribution that already carries them:\n{out}")
    finally:
        tree.cleanup()


def test_failed_checks_survive_the_parallel_presentation() -> None:
    print("\n-- the same guarantee holds in the --jobs failure block, and "
          "only the FINAL attempt's capture is the one it is read from")
    tree = Tree()
    try:
        tree.add("alpha", dwell=0.3, descendant=False)
        tree.add("beta", exit_code=1, tail_lines=40, descendant=False,
                 sentinel="sentinel: the very first line of this run",
                 progress=(("phase", "engine A", "build the scenario"),),
                 failures=(("check", "beta_probe",
                            "the overlay lost a ruin across save-load"),
                           ("check", "beta_probe",
                            "only 2/5 ruin(s) materialized after load"),
                           ("context", "engine log", "/tmp/beta/engine.log")))
        # `--retries 1` makes this the reviewer's case: `run_with_retry`
        # keeps only the FINAL attempt's capture, and the guarantee is
        # about the completed nonzero attempt that decided the verdict.
        rc, out = _main_with(tree, ["--jobs", "2", "--retries", "1"])
        expect(rc == 1, f"the failing probe still fails the run (exit {rc})")

        block = out.split("--- beta_probe.py (FAIL) ---")[-1]
        expect("2 recorded failure(s)" in block,
               f"the parallel failure block carries the attribution:\n{out}")
        for detail in ("the overlay lost a ruin across save-load",
                       "only 2/5 ruin(s) materialized after load"):
            expect(block.count(detail) == 1,
                   f"{detail!r} is named exactly once in the block:\n{out}")
        expect("engine log: /tmp/beta/engine.log" in block,
               f"with its context:\n{out}")
        expect("progress: latest phase entered at" in block
               and "engine A" in block,
               f"and the phase attribution beside it:\n{out}")
        expect("sentinel: the very first line" not in block,
               f"the early non-diagnostic line stays omitted:\n{out}")
        expect("diagnostic line 0" not in block,
               f"and the complete capture is NOT dumped:\n{out}")
        expect("diagnostic line 39" in block,
               f"while the ordinary tail is preserved:\n{out}")
        expect(probe_runner_diagnostics.FAILURE_MARKER not in out,
               f"and no raw record is reprinted:\n{out}")

        # A passing probe's block does not exist at all, so nothing of
        # the mechanism reaches a green run.
        expect("alpha_probe.py (FAIL)" not in out,
               f"the passing probe gets no failure block:\n{out}")

        # The retention guarantee is about the FINAL completed nonzero
        # attempt -- the one that decided the verdict. `run_with_retry`
        # keeps only that attempt's capture, so proving the retry really
        # happened is what makes the assertions above about the second
        # attempt rather than a single-run coincidence.
        expect(len(tree.intervals("beta")) == 2,
               f"the failing probe really was retried "
               f"({tree.intervals('beta')!r})")
        expect(failure_records(block) == [],
               f"and no raw record reaches the block that already renders "
               f"them (got {failure_records(block)!r})")
    finally:
        tree.cleanup()

#: The six probes #1982 repaired. Every terminal failure any of them
#: reports must reach the runner as a durable record, so the structural
#: guard below can name them as one set.
REPAIRED_PROBES = (
    "location_embark_probe.py",
    "location_stamp_idempotent_probe.py",
    "location_content_probe.py",
    "location_overlay_probe.py",
    "portal_location_probe.py",
    "portal_ghost_probe.py",
)


def test_no_repaired_probe_still_reports_a_failure_to_stderr() -> None:
    print("\n-- no repaired probe reports a terminal failure on the "
          "unbuffered stderr the runner's tail cannot retain")

    # This is the mechanism of the whole bug, stated as a guard: the
    # runner launches each probe with `stderr=subprocess.STDOUT`, and
    # Python leaves stderr unbuffered while block-buffering the piped
    # stdout. ANY failure written to stderr therefore overtakes the
    # buffered output and lands above the retained `--tail`, whatever
    # else the probe does correctly. One such path survived the first
    # pass of this repair -- portal_ghost's phase-1 setup exit, which
    # returned without reaching `report` at all -- so the guard is over
    # the whole set rather than the paths that were noticed.
    tools = Path(__file__).resolve().parent
    for script in REPAIRED_PROBES:
        source = (tools / script).read_text(encoding="utf-8")
        expect("file=sys.stderr" not in source,
               f"{script} writes nothing to stderr; a failure there is "
               f"exactly what the runner's tail cannot keep")
        expect("FailureEmitter" in source,
               f"{script} produces durable failure records instead")


def test_a_probes_setup_exit_is_recorded_and_recoverable() -> None:
    print("\n-- portal_ghost's phase-1 setup exit records a durable setup "
          "failure the runner can recover from the complete capture")

    # The engine-free half of a needs-GPU probe: this exit is reached
    # when the fixture never materialised, BEFORE any GPU work, and it is
    # the one terminal exit that does not go through `report`. Driving it
    # directly is what proves it emits at all -- the review that found it
    # found it by reading, and nothing failed.
    import io
    import contextlib
    sys.path.insert(0, str(Path(__file__).resolve().parent))
    import portal_ghost_probe

    buf = io.StringIO()
    with contextlib.redirect_stdout(buf):
        rc = portal_ghost_probe.report_prep_setup_failure()
    out = buf.getvalue()
    expect(rc == 1, f"the setup exit still fails the probe (got {rc})")

    records = failure_records(out)
    kinds = [record.kind for record in records]
    expect("setup" in kinds,
           f"it records a SETUP failure, not an ordinary one (got {kinds!r})")
    setup = [record for record in records if record.kind == "setup"]
    expect(len(setup) == 1,
           f"exactly one, so the runner names it once (got {setup!r})")
    expect("no ruin_small with resolvable bounds" in setup[0].detail,
           f"carrying the diagnosis (got {setup[0].detail!r})")
    expect(setup[0].identity == "portal_ghost_probe",
           f"and naming its producer (got {setup[0].identity!r})")
    expect(any(record.kind == "context" for record in records),
           f"with the prep engine log as context (got {records!r})")

    # Nothing reaches stderr on this path any more, and the runner's own
    # consumer recovers the whole thing from the capture.
    derived = "\n".join(probe_runner_diagnostics.failure_attribution(out))
    expect("SETUP FAILURE: phase 1 (headless prep)" in derived,
           f"the runner's presentation recovers it (got {derived!r})")
    expect("no ruin_small with resolvable bounds" in derived,
           f"with its detail (got {derived!r})")

def test_the_readme_states_no_registry_total() -> None:
    print("\n-- tools/README.md's run_probes section states no registry total")

    # The shipped file, through the same helper the mutations use.
    shipped = README_PATH.read_text(encoding="utf-8")
    expect(readme_total_claim_problems(shipped) == [],
           f"the shipped README section claims no registry total "
           f"({readme_total_claim_problems(shipped)})")

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
    claim = "The registry holds 90 registered probes.\n"
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



def main() -> int:
    test_the_synthetic_fixtures_are_valid_python()
    test_liveness_check_does_not_count_a_zombie_as_running()
    test_group_running_ignores_a_zombie_only_group()
    test_success_reaps_the_engine()
    test_failure_reaps_the_engine_and_keeps_the_tail()
    test_clean_probe_is_unaffected()
    test_timeout_escalates_to_sigkill()
    test_stubborn_engine_is_killed_without_charging_the_probe()
    test_a_stopping_runner_launches_no_further_probe()
    test_reap_group_on_a_dead_group_is_a_noop()
    test_aggregate_exit_codes_unchanged()
    test_the_readme_states_no_registry_total()
    test_timeout_overrides_are_validated_registry_data()
    test_key_specific_timeout_and_explicit_override_reach_execution()
    test_parallel_retry_reuses_the_key_specific_timeout()
    test_exclusive_resource_declaration_is_data_about_real_probes()
    test_every_probe_declares_what_an_exclusive_holder_takes()
    test_one_preflight_precedes_every_parallel_probe()
    test_one_preflight_precedes_every_sequential_probe()
    test_a_failed_preflight_spawns_nothing()
    test_an_unusable_resolved_path_is_refused_not_ignored()
    test_list_and_rejected_selections_stay_build_free()
    test_gui_port_refusal_still_precedes_the_build()
    test_the_resolved_executable_reaches_every_attempt()
    test_a_direct_run_one_leaves_the_child_on_the_fallback()
    test_a_nested_runner_adopts_the_executable_without_rebuilding()
    test_an_ancestors_exclusive_hold_is_not_waited_on()
    test_the_preflight_build_excludes_a_foreign_runner()
    test_the_preflight_build_waits_for_a_foreign_runner()
    test_a_nested_preflight_does_not_wait_on_its_ancestor()
    test_the_hold_environment_names_what_a_probe_holds()
    test_a_probe_is_handed_its_runners_exclusive_holds()
    test_no_registered_probe_spells_a_cabal_engine_launch()
    test_resource_ledger_is_a_reader_writer_lock()
    test_declared_conflicts_never_overlap()
    test_a_solo_probe_waits_for_work_already_running()
    test_conflict_is_released_after_a_failure()
    test_conflict_is_released_after_a_timeout()
    test_a_foreign_exclusive_holder_makes_the_sweep_wait()
    test_waiting_for_a_foreign_holder_is_not_charged_to_the_probe()
    test_a_foreign_shared_holder_never_blocks_a_shared_probe()
    test_a_run_probes_exclusive_probe_blocks_a_foreign_shared_acquirer()
    test_exact_mixed_selection_is_rejected_before_listing()
    test_exact_mixed_selection_never_runs_the_valid_probe()
    test_exact_all_invalid_selection_keeps_existing_diagnostic()
    test_exact_all_valid_selection_is_unaffected()
    test_exact_duplicate_valid_keys_still_collapse()
    test_substring_selection_stays_permissive()
    test_retry_reaps_between_attempts()
    test_engine_ledger_is_durable_before_the_handshake()
    test_ctrl_c_leaves_no_engine_behind()
    test_ctrl_c_cancels_queued_parallel_work()
    test_ctrl_c_during_submission_starts_nothing_more()
    test_probe_launched_into_shutdown_is_killed_promptly()
    test_ctrl_c_in_the_launch_window_leaves_nothing()
    test_reap_returns_only_once_the_group_is_observed_gone()
    test_retry_can_rebind_the_port_a_killed_engine_held()
    test_port_span_declaration_is_data_about_real_probes()
    test_parallel_allocation_never_overlaps()
    test_gui_port_refusal_covers_the_whole_span()
    test_port_with_jobs_bases_the_parallel_allocation()
    test_a_two_port_probe_never_takes_its_neighbours_base()
    test_progress_records_round_trip_and_stay_out_of_the_verdict_shape()
    test_progress_attribution_derives_the_in_flight_set()
    test_progress_survives_a_forced_timeout_outside_the_ordinary_tail()
    test_in_flight_attempts_are_derivable_from_the_default_report()
    test_parallel_dispatch_and_retry_records_name_every_attempt()
    test_failure_records_round_trip_and_stay_off_the_progress_channel()
    test_failure_attribution_names_every_recorded_failure_once()
    test_failed_checks_survive_outside_the_ordinary_tail()
    test_failed_checks_survive_the_parallel_presentation()
    test_no_repaired_probe_still_reports_a_failure_to_stderr()
    test_a_probes_setup_exit_is_recorded_and_recoverable()
    if FAILURES:
        print(f"\n{len(FAILURES)} test(s) failed:")
        for failure in FAILURES:
            print(f"  {failure}")
        return 1
    print("\nAll run_probes teardown tests passed")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
