#!/usr/bin/env python3
"""Haskell record parsing and its end-to-end audit mutations (#2138).

Twenty-four groups in three fragments, matching the aggregate's order:

  `TESTS_RECORD_PARSING`   13 direct `extract_record_fields` cases --
                           brace blocks, nested and unbalanced comments,
                           grouped and multiline fields, string and char
                           literals in types, and the missing-record raise;
  `TESTS_FIELD_MUTATIONS`  7 end-to-end mutations that hide a field from
                           the parser or expose one, each asserting the
                           audit notices;
  `TESTS_POINTER_REACHED`  4 groups binding #1703's pointer-reached
                           gameplay managers to the production
                           `ROOT_RECORDS`, parsed from their real sources.

The parser cases exercise `persistence_inventory_audit_haskell`, which
owns extraction since #2124; the mutation and manager groups call the
composed `audit()` because what they assert is the whole pipeline's
verdict. The three real-source groups read the tree and mutate only
in-memory copies -- nothing here writes a repository file.
"""
from __future__ import annotations

import re

from .support import expect
from .fixtures_haskell import (
    FAKE_ROOT_RECORDS,
    SYNTHETIC_ENGINE_ENV,
)
from .fixtures_inventory import SYNTHETIC_INVENTORY_MISSING_ONE
from persistence_inventory_audit import (  # type: ignore
    ROOT_RECORDS,
    audit,
)
from persistence_inventory_audit_haskell import extract_record_fields  # type: ignore


# A record with an UNBALANCED brace inside a haddock comment (a lone
# `}`) -- if comments aren't stripped before brace-depth tracking, this
# closes the record block right after fieldOne and fieldTwo/fieldThree
# are never seen. This is the exact false-negative the audit must not
# have.
SYNTHETIC_ENGINE_ENV_UNBALANCED_COMMENT = """\
module Fake where

data EngineEnv = EngineEnv
  { fieldOne   ∷ IORef Int
    -- ^ refers to an unrelated closing brace from other prose: cheese}
  , fieldTwo   ∷ IORef Text
  , fieldThree ∷ Q.Queue Int
  } deriving (Eq)
"""

# A record whose haddock comment contains a legally NESTED Haskell block
# comment (`{- outer {- inner -} still outer -}`) with an unmatched `}`
# left over after the inner comment's own close. A non-nesting stripper
# removes only up to the FIRST `-}` (the inner one), leaving " with a
# stray } here -}" in the text -- and that stray `}` would close the
# record block early, exactly like the unbalanced-comment case above,
# but only reachable via a legally nested comment.
SYNTHETIC_ENGINE_ENV_NESTED_COMMENT = """\
module Fake where

data EngineEnv = EngineEnv
  { fieldOne   ∷ IORef Int
    {- outer comment {- inner -} with a stray } here -}
  , fieldTwo   ∷ IORef Text
  , fieldThree ∷ Q.Queue Int
  } deriving (Eq)
"""

# fieldTwo's name and its `∷`/type are on DIFFERENT physical lines --
# legal Haskell layout. A field-name matcher anchored to "same line as
# the arrow" never sees it.
SYNTHETIC_ENGINE_ENV_MULTILINE_FIELD = """\
module Fake where

data EngineEnv = EngineEnv
  { fieldOne   ∷ IORef Int
  , fieldTwo
      ∷ IORef Text
  , fieldThree ∷ Q.Queue Int
  } deriving (Eq)
"""

# A GROUPED field declaration -- several names sharing one trailing
# type signature (`name1, name2 :: Type`), legal Haskell. `unclassified`
# has no arrow of its own; it borrows `classified`'s.
SYNTHETIC_ENGINE_ENV_GROUPED_FIELD = """\
module Fake where

data EngineEnv = EngineEnv
  { fieldOne ∷ IORef Int
  , unclassified, fieldTwo ∷ IORef Text
  , fieldThree ∷ Q.Queue Int
  } deriving (Eq)
"""

# A record field typed with a DataKinds/GHC.TypeLits promoted string
# literal containing a `}` -- legal Haskell. A brace-counter that isn't
# string-aware treats this as the record's OWN closing brace, hiding
# `unclassified` (and every field after it) from extraction entirely.
SYNTHETIC_ENGINE_ENV_STRING_LITERAL_BRACE = """\
module Fake where

data EngineEnv = EngineEnv
  { fieldOne ∷ IORef Int
  , classified ∷ Proxy "}"
  , unclassified ∷ Int
  } deriving (Eq)
"""

# Same hazard, but with `--` inside the promoted string literal instead
# of `}` -- must not be mistaken for a line-comment start either.
SYNTHETIC_ENGINE_ENV_STRING_LITERAL_DASH = """\
module Fake where

data EngineEnv = EngineEnv
  { fieldOne ∷ IORef Int
  , classified ∷ Proxy "--"
  , unclassified ∷ Int
  } deriving (Eq)
"""

# A promoted Char literal (DataKinds `'}'`) containing a `}` in a
# field's own type -- the char-literal sibling of the string-literal
# brace hazard above.
SYNTHETIC_ENGINE_ENV_CHAR_LITERAL_BRACE = """\
module Fake where

data EngineEnv = EngineEnv
  { fieldOne ∷ IORef Int
  , classified ∷ Proxy '}'
  , unclassified ∷ Int
  } deriving (Eq)
"""

# Ordinary Haskell identifiers ending in one or more trailing "primes"
# (`foo'`, `bar''`) -- must NOT be mistaken for char-literal openers.
SYNTHETIC_ENGINE_ENV_TRAILING_PRIMES = """\
module Fake where

data EngineEnv = EngineEnv
  { fieldOne' ∷ IORef Int
  , fieldTwo'' ∷ Int
  } deriving (Eq)
"""

# Two DIFFERENT fields whose promoted string-literal TYPES happen to
# spell out `{-`/`-}` -- a block-comment stripper that isn't
# literal-aware sees the first as "opening" a comment and the second
# (in a LATER field) as "closing" it, silently swallowing everything
# (including real field declarations) in between, including
# `unclassified` itself.
SYNTHETIC_ENGINE_ENV_FAKE_BLOCK_COMMENT_STRINGS = """\
module Fake where

data EngineEnv = EngineEnv
  { fieldOne ∷ IORef Int
  , documented ∷ Proxy "{-"
  , unclassified ∷ Proxy "-}"
  } deriving (Eq)
"""


# ----- Tests -------------------------------------------------------------

def test_extract_fields_from_brace_block():
    fields = extract_record_fields(SYNTHETIC_ENGINE_ENV, r"^data EngineEnv = EngineEnv\b")
    expect(fields == ["fieldOne", "fieldTwo", "fieldThree"],
           f"extracts exactly the three EngineEnv fields, got {fields}")


def test_extract_fields_stray_brace_in_comment_is_harmless():
    # The haddock comment under fieldOne contains a literal `{...}` — the
    # depth tracker must not let prose braces close the block early.
    fields = extract_record_fields(SYNTHETIC_ENGINE_ENV, r"^data EngineEnv = EngineEnv\b")
    expect("fieldTwo" in fields and "fieldThree" in fields,
           "a brace inside a haddock comment doesn't truncate field extraction")


def test_extract_fields_unbalanced_brace_in_comment_does_not_truncate():
    fields = extract_record_fields(SYNTHETIC_ENGINE_ENV_UNBALANCED_COMMENT,
                                    r"^data EngineEnv = EngineEnv\b")
    expect(fields == ["fieldOne", "fieldTwo", "fieldThree"],
           f"an UNBALANCED brace inside a haddock comment (a lone '}}') does not "
           f"prematurely close the record and drop later fields, got {fields}")


def test_extract_fields_nested_block_comment_does_not_truncate():
    fields = extract_record_fields(SYNTHETIC_ENGINE_ENV_NESTED_COMMENT,
                                    r"^data EngineEnv = EngineEnv\b")
    expect(fields == ["fieldOne", "fieldTwo", "fieldThree"],
           f"a legally NESTED {{- -}} block comment (with a stray '}}' left "
           f"over from a non-nesting strip) does not truncate extraction, "
           f"got {fields}")


def test_extract_fields_name_and_arrow_on_different_lines():
    fields = extract_record_fields(SYNTHETIC_ENGINE_ENV_MULTILINE_FIELD,
                                    r"^data EngineEnv = EngineEnv\b")
    expect(fields == ["fieldOne", "fieldTwo", "fieldThree"],
           f"a field whose name and `∷`/type are on DIFFERENT physical "
           f"lines is still extracted, got {fields}")


def test_extract_fields_grouped_declaration():
    fields = extract_record_fields(SYNTHETIC_ENGINE_ENV_GROUPED_FIELD,
                                    r"^data EngineEnv = EngineEnv\b")
    expect(fields == ["fieldOne", "unclassified", "fieldTwo", "fieldThree"],
           f"a grouped declaration (`unclassified, fieldTwo ∷ IORef Text`) "
           f"extracts BOTH names sharing the trailing type, got {fields}")


def test_extract_fields_survives_brace_in_string_literal_type():
    fields = extract_record_fields(SYNTHETIC_ENGINE_ENV_STRING_LITERAL_BRACE,
                                    r"^data EngineEnv = EngineEnv\b")
    expect(fields == ["fieldOne", "classified", "unclassified"],
           f"a DataKinds promoted string literal type containing '}}' "
           f"(`Proxy \"}}\"`) does not prematurely close the record and "
           f"drop later fields, got {fields}")


def test_extract_fields_survives_dash_in_string_literal_type():
    fields = extract_record_fields(SYNTHETIC_ENGINE_ENV_STRING_LITERAL_DASH,
                                    r"^data EngineEnv = EngineEnv\b")
    expect(fields == ["fieldOne", "classified", "unclassified"],
           f"a DataKinds promoted string literal type containing '--' "
           f"(`Proxy \"--\"`) is not mistaken for a line comment, "
           f"got {fields}")


def test_extract_fields_survives_brace_in_char_literal_type():
    fields = extract_record_fields(SYNTHETIC_ENGINE_ENV_CHAR_LITERAL_BRACE,
                                    r"^data EngineEnv = EngineEnv\b")
    expect(fields == ["fieldOne", "classified", "unclassified"],
           f"a DataKinds promoted CHAR literal type containing '}}' "
           f"(`Proxy '}}'`) does not prematurely close the record and "
           f"drop later fields, got {fields}")


def test_extract_fields_trailing_primes_are_not_char_literals():
    fields = extract_record_fields(SYNTHETIC_ENGINE_ENV_TRAILING_PRIMES,
                                    r"^data EngineEnv = EngineEnv\b")
    expect(fields == ["fieldOne'", "fieldTwo''"],
           f"ordinary identifiers ending in trailing primes are not "
           f"mistaken for char-literal openers, got {fields}")


def test_extract_fields_survives_fake_block_comment_delimiters_in_strings():
    fields = extract_record_fields(SYNTHETIC_ENGINE_ENV_FAKE_BLOCK_COMMENT_STRINGS,
                                    r"^data EngineEnv = EngineEnv\b")
    expect(fields == ["fieldOne", "documented", "unclassified"],
           f"two string literals spelling out '{{-' and '-}}' in "
           f"DIFFERENT fields' types do not get mistaken for a block "
           f"comment's open/close, swallowing the field between them, "
           f"got {fields}")


def test_extract_fields_ignores_other_records():
    fields = extract_record_fields(SYNTHETIC_ENGINE_ENV, r"^data EngineEnv = EngineEnv\b")
    expect("unrelated" not in fields,
           "fields from a different record in the same file are not picked up")


def test_extract_fields_missing_record_raises():
    raised = False
    try:
        extract_record_fields(SYNTHETIC_ENGINE_ENV, r"^data NoSuchRecord = NoSuchRecord\b")
    except ValueError:
        raised = True
    expect(raised, "a record-start pattern that matches nothing raises ValueError")


def test_audit_detects_field_hidden_behind_unbalanced_comment_brace():
    """Regression for the false-negative the naive brace counter had: a
    lone unbalanced `}` in a haddock comment used to close the record
    block early, so fieldThree was never extracted and its absence from
    the inventory went unreported. It must be reported now."""
    violations = audit(
        {"Fake.hs": SYNTHETIC_ENGINE_ENV_UNBALANCED_COMMENT},
        {},
        SYNTHETIC_INVENTORY_MISSING_ONE,  # missing fieldTwo, not fieldThree
        root_records=FAKE_ROOT_RECORDS,
    )
    expect(any("fieldTwo" in v for v in violations),
           f"fieldTwo (dropped from the fixture inventory) is still reported "
           f"even with an unbalanced brace earlier in the same record, got {violations}")


def test_audit_detects_field_hidden_behind_nested_comment():
    """Regression for the nesting-unaware stripper: a legally nested
    {- -} comment used to leave a stray `}` behind that closed the
    record early, hiding fieldTwo's absence from the inventory."""
    violations = audit(
        {"Fake.hs": SYNTHETIC_ENGINE_ENV_NESTED_COMMENT},
        {},
        SYNTHETIC_INVENTORY_MISSING_ONE,  # missing fieldTwo
        root_records=FAKE_ROOT_RECORDS,
    )
    expect(any("fieldTwo" in v for v in violations),
           f"fieldTwo is still reported even with a nested block comment "
           f"earlier in the same record, got {violations}")


def test_audit_detects_field_with_name_and_arrow_on_different_lines():
    """Regression for the multiline-field false-negative: a field
    whose name and `∷`/type are on different physical lines used to
    never be extracted, so its absence from the inventory went
    unreported."""
    violations = audit(
        {"Fake.hs": SYNTHETIC_ENGINE_ENV_MULTILINE_FIELD},
        {},
        SYNTHETIC_INVENTORY_MISSING_ONE,  # missing fieldTwo
        root_records=FAKE_ROOT_RECORDS,
    )
    expect(any("fieldTwo" in v for v in violations),
           f"fieldTwo (whose name and arrow are on different lines in this "
           f"fixture) is still reported when unclassified, got {violations}")


def test_audit_detects_grouped_field_declaration():
    """Regression for the grouped-declaration false-negative: only the
    LAST name in `name1, name2 :: Type` used to be extracted, so an
    unclassified name earlier in the group went unreported."""
    violations = audit(
        {"Fake.hs": SYNTHETIC_ENGINE_ENV_GROUPED_FIELD},
        {},
        SYNTHETIC_INVENTORY_MISSING_ONE,  # missing fieldTwo; unclassified is never classified either
        root_records=FAKE_ROOT_RECORDS,
    )
    expect(any("unclassified" in v for v in violations),
           f"the bare-named first field in a grouped declaration is "
           f"reported when unclassified, got {violations}")
    expect(any("EngineEnv.fieldTwo" in v for v in violations),
           f"the arrow-bearing second field in the group is also reported "
           f"when unclassified, got {violations}")


def test_audit_detects_field_hidden_behind_brace_in_string_literal():
    """Regression: a DataKinds promoted string literal containing '}'
    in a field's own type used to prematurely close the record,
    hiding every field after it from the audit entirely."""
    violations = audit(
        {"Fake.hs": SYNTHETIC_ENGINE_ENV_STRING_LITERAL_BRACE},
        {},
        SYNTHETIC_INVENTORY_MISSING_ONE,  # classifies fieldOne/fieldThree; classified/unclassified aren't real names here
        root_records=FAKE_ROOT_RECORDS,
    )
    expect(any("unclassified" in v for v in violations),
           f"the field after the brace-containing string literal type is "
           f"still extracted and reported when unclassified, got {violations}")


def test_audit_detects_field_hidden_behind_brace_in_char_literal():
    """Regression: a DataKinds promoted CHAR literal containing '}'
    used to prematurely close the record the same way a string literal
    did (test above) -- the char-literal-specific code path."""
    violations = audit(
        {"Fake.hs": SYNTHETIC_ENGINE_ENV_CHAR_LITERAL_BRACE},
        {},
        SYNTHETIC_INVENTORY_MISSING_ONE,
        root_records=FAKE_ROOT_RECORDS,
    )
    expect(any("unclassified" in v for v in violations),
           f"the field after the brace-containing char literal type is "
           f"still extracted and reported when unclassified, got {violations}")


def test_audit_detects_field_hidden_behind_fake_block_comment_strings():
    """Regression: two DIFFERENT fields' string-literal types spelling
    out '{-' and '-}' used to be read as a real block comment's
    open/close (block-comment stripping ran BEFORE string-awareness),
    silently swallowing the field between them from the audit
    entirely."""
    violations = audit(
        {"Fake.hs": SYNTHETIC_ENGINE_ENV_FAKE_BLOCK_COMMENT_STRINGS},
        {},
        SYNTHETIC_INVENTORY_MISSING_ONE,  # missing fieldTwo; documented/unclassified aren't real names here
        root_records=FAKE_ROOT_RECORDS,
    )
    expect(any("unclassified" in v for v in violations),
           f"the field after the fake block-comment-shaped string "
           f"literals is still extracted and reported when "
           f"unclassified, got {violations}")


# ----- #1703 pointer-reached gameplay managers ---------------------------

# The three records EngineEnv reaches only through a bare IORef pointer.
# Before #1703 the audit scanned the POINTER field (classified `Rebuild`,
# delegating onward) but never the record it points at, so a field added
# inside an already-reachable manager passed every gate with no
# persistence decision recorded. Each entry is the exact production
# ROOT_RECORDS triple these tests bind against -- asserting the label
# alone would pass even if the path or pattern silently stopped matching
# the live record, and a synthetic fixture would prove nothing about the
# production allowlist at all.
POINTER_REACHED_MANAGERS = [
    ("UnitManager", "src/Unit/Types/Manager.hs",
     r"^data UnitManager = UnitManager\b"),
    ("BuildingManager", "src/Building/Types.hs",
     r"^data BuildingManager = BuildingManager\b"),
    ("UnitThreadState", "src/Unit/Sim/Types.hs",
     r"^data UnitThreadState = UnitThreadState\b"),
]

# The field name injected into a REAL record's source below. Deliberately
# one no inventory table can plausibly classify, so its violation can be
# attributed unambiguously.
PROBE_FIELD = "injectedUnclassifiedProbeField"


def _inject_field_into_real_record(source: str, pattern: str,
                                   field_name: str) -> str:
    """Add one unclassified field to a REAL checked-out record's text.

    Finds the record's `data X = X` line via its own production regex,
    then the opening `{` of its brace block, and splices a new field in
    right after the first one. Everything else in the file is untouched,
    so the audit sees the actual tree with exactly one field added."""
    lines = source.split("\n")
    header = re.compile(pattern)
    for index, line in enumerate(lines):
        if not header.match(line):
            continue
        for brace in range(index, len(lines)):
            if lines[brace].lstrip().startswith("{"):
                lines.insert(brace + 1, f"    , {field_name} ∷ Int")
                return "\n".join(lines)
        raise AssertionError(
            f"no opening brace found after the record header {pattern!r}")
    raise AssertionError(f"no line matched the record header {pattern!r}")


def test_production_roots_include_the_pointer_reached_managers():
    """The three records must be in the PRODUCTION allowlist, at their
    real source paths, with patterns that match the live declarations --
    not merely reachable through some fixture."""
    for label, relpath, pattern in POINTER_REACHED_MANAGERS:
        entries = [e for e in ROOT_RECORDS if e[0] == label]
        expect(entries == [(label, relpath, pattern)],
               f"ROOT_RECORDS carries exactly one {label} entry at "
               f"{relpath} with the expected pattern, got {entries}")


def test_pointer_reached_managers_parse_from_their_real_sources():
    """A pattern that stopped matching (a record moved file, or the
    parser desynced) would make the audit report a hard violation rather
    than silently scanning nothing -- but only if the source is actually
    reachable and parseable, which this pins directly."""
    from persistence_inventory_audit import _load_repo_state  # type: ignore
    record_sources, _, _, _, _ = _load_repo_state()
    # BuildingManager: bmDefs, bmInstances, bmNextId, bmSelected, plus
    # #2091's session-transient bmDestructions.
    expected_counts = {"UnitManager": 4, "BuildingManager": 5,
                       "UnitThreadState": 1}
    for label, relpath, pattern in POINTER_REACHED_MANAGERS:
        expect(relpath in record_sources,
               f"{label}'s source {relpath} is loaded by _load_repo_state")
        fields = extract_record_fields(record_sources[relpath], pattern)
        expect(len(fields) == expected_counts[label],
               f"{label} parses to {expected_counts[label]} fields from its "
               f"real source, got {len(fields)}: {fields}")


def test_audit_detects_a_new_field_on_each_pointer_reached_manager():
    """The regression #1703 exists to prevent, proven against the REAL
    tree through the DEFAULT (production) root records: add one field to
    each of the three managers in turn and the audit must name it.

    `audit()` is called with no `root_records=` argument on purpose --
    that is what makes this bind to the production allowlist rather than
    to a fixture list the test itself supplies."""
    from persistence_inventory_audit import _load_repo_state  # type: ignore
    record_sources, scripts_text_by_file, inventory_text, registered_ids, \
        component_sources = _load_repo_state()
    for label, relpath, pattern in POINTER_REACHED_MANAGERS:
        mutated = dict(record_sources)
        mutated[relpath] = _inject_field_into_real_record(
            record_sources[relpath], pattern, PROBE_FIELD)
        expect(mutated[relpath] != record_sources[relpath],
               f"the {label} fixture mutation actually changed {relpath}")
        violations = audit(mutated, scripts_text_by_file, inventory_text,
                           registered_ids=registered_ids,
                           component_sources=component_sources)
        expect(any(f"{label}.{PROBE_FIELD}" in v for v in violations),
               f"a field added to {label} ({relpath}) is reported by the "
               f"audit under its production root entry, got {violations}")
        expect(all(PROBE_FIELD in v for v in violations),
               f"injecting one field into {label} produces no OTHER "
               f"violation -- the rest of the real tree stays clean, got "
               f"{violations}")


def test_pointer_reached_manager_fields_are_classified_per_owner():
    """The three managers' own headings are load-bearing: the audit
    scopes classification PER owner heading, so a row moved out from
    under `### UnitThreadState` (or either manager's heading) stops
    counting. Stripping the heading must therefore fail the audit."""
    from persistence_inventory_audit import _load_repo_state  # type: ignore
    record_sources, scripts_text_by_file, inventory_text, registered_ids, \
        component_sources = _load_repo_state()
    for label, relpath, pattern in POINTER_REACHED_MANAGERS:
        renamed = inventory_text.replace(f"### {label}\n",
                                         f"### Not{label}\n")
        expect(renamed != inventory_text,
               f"the inventory really carries a '### {label}' owner heading")
        violations = audit(record_sources, scripts_text_by_file, renamed,
                           registered_ids=registered_ids,
                           component_sources=component_sources)
        fields = extract_record_fields(record_sources[relpath], pattern)
        for field in fields:
            expect(any(f"{label}.{field}" in v for v in violations),
                   f"{label}.{field} is reported once its owner heading is "
                   f"renamed away, got {violations}")


# ----- Registry ----------------------------------------------------------
#: The 24 groups this family owns, in the aggregate's order. Three
#: fragments, because the aggregate interleaves them with other families:
#: the parser cases lead the whole run, the field mutations sit after the
#: inventory's classification-parsing block, and the pointer-reached
#: managers come last, after the reference groups. `tests.py` composes
#: them; nothing here knows where in the run they land.

#: 13 direct `extract_record_fields` cases: what the parser must see
#: through comments, literals and multi-line declarations.
TESTS_RECORD_PARSING = (
    test_extract_fields_from_brace_block,
    test_extract_fields_stray_brace_in_comment_is_harmless,
    test_extract_fields_unbalanced_brace_in_comment_does_not_truncate,
    test_extract_fields_nested_block_comment_does_not_truncate,
    test_extract_fields_name_and_arrow_on_different_lines,
    test_extract_fields_grouped_declaration,
    test_extract_fields_survives_brace_in_string_literal_type,
    test_extract_fields_survives_dash_in_string_literal_type,
    test_extract_fields_survives_brace_in_char_literal_type,
    test_extract_fields_trailing_primes_are_not_char_literals,
    test_extract_fields_survives_fake_block_comment_delimiters_in_strings,
    test_extract_fields_ignores_other_records,
    test_extract_fields_missing_record_raises,
)

#: 7 end-to-end mutations, each hiding a field behind a construct the
#: parser once truncated on, or spreading one across lines. The audit must
#: still report it unclassified -- a parser regression shows up here as a
#: SILENT pass, which is why each has a direct counterpart above.
TESTS_FIELD_MUTATIONS = (
    test_audit_detects_field_hidden_behind_unbalanced_comment_brace,
    test_audit_detects_field_hidden_behind_nested_comment,
    test_audit_detects_field_with_name_and_arrow_on_different_lines,
    test_audit_detects_grouped_field_declaration,
    test_audit_detects_field_hidden_behind_brace_in_string_literal,
    test_audit_detects_field_hidden_behind_brace_in_char_literal,
    test_audit_detects_field_hidden_behind_fake_block_comment_strings,
)

#: 4 groups over #1703's pointer-reached gameplay managers, read from
#: their real checked-out sources and mutated only in memory.
TESTS_POINTER_REACHED = (
    test_production_roots_include_the_pointer_reached_managers,
    test_pointer_reached_managers_parse_from_their_real_sources,
    test_audit_detects_a_new_field_on_each_pointer_reached_manager,
    test_pointer_reached_manager_fields_are_classified_per_owner,
)

#: The family's complete inventory: its fragments, in their own order.
TESTS = TESTS_RECORD_PARSING + TESTS_FIELD_MUTATIONS + TESTS_POINTER_REACHED
