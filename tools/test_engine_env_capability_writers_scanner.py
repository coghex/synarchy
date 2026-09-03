#!/usr/bin/env python3
"""The scanner MECHANICS of engine_env_capability_writers.py -- lexical
scope, tokenization and mutation-expression classification (issue
#1892, CMA-1; extracted from
tools/test_engine_env_capability_writers.py by issue #2228).

Twenty-eight groups over what `scan_capability_writes` must read out of
Haskell text before any policy applies: which names an import brings
into scope (explicit lists, `hiding`, `qualified`, aliases, wildcards,
a bare import, a type-only import), what the tokenizer does with
comments, strings, literals and line numbers, and when an expression is
a write, residue, or unreadable (infix and strict application, visible
type applications, redundant parentheses, operator sections,
record-dot access, a primitive used as a value, a locally shadowed
name, a multiline expression, sibling statements).

The map policy those reads feed belongs to
`test_engine_env_capability_writers_map`; capability-record and
projection discovery to `..._projections`; the real repository and
`resolve_primitive`'s provenance rule to `..._conformance`.

Not a gate of its own. Run through the focused façade or the aggregate:

  python3 tools/test_engine_env_capability_writers.py --only scanner
  python3 tools/test_engine_env_capability_audit.py
"""
from __future__ import annotations

import sys
from pathlib import Path

sys.path.insert(0, str(Path(__file__).resolve().parent))
from engine_env_capability_common import (  # type: ignore  # noqa: E402
    _strip_haskell_comments,
)
from engine_env_capability_writers import (  # type: ignore  # noqa: E402
    _applied_head, _first_argument_head, _infix_left_operand_head,
    audit_mutation_sites, audit_writer_modules, imports_name, parse_imports,
    tokenize_haskell,
)
from test_engine_env_capability_writers_support import (  # noqa: E402
    TRAP_MODULE as _TRAP_MODULE,
    WRITER_FIELDS as _WRITER_FIELDS,
    expect,
    full_scan as _full_scan,
    scan as _scan,
    writer_sources,
)


# ----- This owner's fixtures -------------------------------------------

# `Engine.Core.State` is imported for the TYPE only, so an identically
# named local is not the field -- the live shape of
# `src/Unit/Thread/Movement.hs`'s `utsRef` parameter.
_TYPE_ONLY_IMPORTER = """\
module Narrow.Mod where

import Data.IORef

import Engine.Core.State (EngineEnv)

tick ∷ EngineEnv → IORef Int → IO ()
tick _ fieldOne = writeIORef fieldOne 5
"""

# A module-local helper that shares an accessor's name and is APPLIED
# exactly like the real thing, so nothing about the write's SHAPE
# distinguishes it. `Engine.Core.State` is imported for the `EngineEnv`
# type alone, which is the only reason this is not the field -- the
# import-scope gate on its own.
_LOCAL_HOMONYM = """\
module Homonym.Mod where

import Data.IORef

import Engine.Core.State (EngineEnv)

fieldOne ∷ EngineEnv → IORef Int
fieldOne _ = error "this module's own helper, not the accessor"

tick ∷ EngineEnv → IO ()
tick env = writeIORef (fieldOne env) 5
"""

# Qualified spellings, through the module's own name and through an
# `as` alias. Both name the field exactly as the bare spelling does.
_QUALIFIED_WRITER = """\
module Qualified.Mod where

import Data.IORef

import qualified Engine.Core.State as State
import qualified Engine.Core.Capability.Fake as Cap

bumpRaw ∷ State.EngineEnv → IO ()
bumpRaw env = writeIORef (State.fieldTwo env) 4

bumpCapability ∷ State.EngineEnv → IO ()
bumpCapability env =
    writeIORef (Cap.fkFieldOne (Cap.toFakeCapability env)) 5
"""

# The two ways a qualified spelling must NOT resolve: a prefix this
# module establishes for a different module, and the aliased module's
# own name, which the alias replaces.
_MISQUALIFIED = """\
module Misqualified.Mod where

import Data.IORef

import qualified Engine.Core.State as State
import qualified Data.Map as Other

wrongModule ∷ State.EngineEnv → IO ()
wrongModule env = writeIORef (Other.fieldTwo env) 6

replacedName ∷ State.EngineEnv → IO ()
replacedName env = writeIORef (Engine.Core.State.fieldTwo env) 7
"""

# A mutation primitive is itself under a qualifier too. Missing this
# spelling would let an undeclared writer through in silence.
_QUALIFIED_PRIMITIVE = """\
module QualPrim.Mod where

import Data.IORef

import qualified Data.IORef as Ref
import Engine.Core.State (EngineEnv, fieldOne)

bump ∷ EngineEnv → IO ()
bump env = Ref.writeIORef (fieldOne env) 1
"""

# `qualified` removes the UNQUALIFIED spelling from scope, so this
# module's own `fieldOne` helper is not the field even though the owner
# is imported -- while `State.fieldTwo` in the same module is.
_QUALIFIED_ONLY = """\
module QualOnly.Mod where

import Data.IORef

import qualified Engine.Core.State as State

fieldOne ∷ State.EngineEnv → IORef Int
fieldOne _ = error "this module's own helper, not the accessor"

viaHomonym ∷ State.EngineEnv → IO ()
viaHomonym env = writeIORef (fieldOne env) 2

viaQualifier ∷ State.EngineEnv → IO ()
viaQualifier env = writeIORef (State.fieldTwo env) 3
"""

# A bare first argument: never the accessor (it projects out of a
# handle, so it cannot BE the `IORef`), and for a capability accessor
# it surfaces in the residue rather than being silently dropped.
_BARE_ARGUMENT = """\
module Bare.Mod where

import Data.IORef

import Engine.Core.State (EngineEnv, fieldTwo)
import Engine.Core.Capability.Fake (FakeCapability(..))

viaWildcard ∷ FakeCapability → Int → IO ()
viaWildcard FakeCapability{..} newValue = writeIORef fkFieldOne newValue

viaParenthesizedLocal ∷ IORef Text → IO ()
viaParenthesizedLocal fieldTwo = writeIORef (fieldTwo) 9
"""

# `hiding` brings in everything EXCEPT the listed names, which is how a
# module legally defines its own `fieldOne` while importing the rest.
_HIDING_IMPORTER = """\
module Hiding.Mod where

import Data.IORef

import Engine.Core.State hiding (fieldOne)

fieldOne ∷ EngineEnv → IORef Int
fieldOne _ = error "this module's own helper, not the accessor"

shadowed ∷ EngineEnv → IO ()
shadowed env = writeIORef (fieldOne env) 1

visible ∷ EngineEnv → IO ()
visible env = writeIORef (fieldTwo env) 2
"""

# Any two-argument function may be written infix, so a backticked
# primitive is the same direct write with its arguments swapped --
# qualified spelling included.
_INFIX_WRITER = """\
module Infix.Mod where

import Data.IORef

import qualified Data.IORef as Ref
import Engine.Core.State (EngineEnv, fieldOne)
import Engine.Core.Capability.Fake (FakeCapability(..), toFakeCapability)

raw ∷ EngineEnv → IO ()
raw env = (fieldOne env) `writeIORef` 1

viaCapability ∷ EngineEnv → IO ()
viaCapability env = (fkFieldTwo (toFakeCapability env)) `Ref.writeIORef` 2
"""

# A backtick operator binds looser than application, so an infix
# operand needs no parentheses at all.
_BARE_OPERAND = """\
module BareOperand.Mod where

import Data.IORef

import qualified Data.IORef as Ref
import Engine.Core.State (EngineEnv, fieldThree)
import Engine.Core.Capability.Fake (FakeCapability(..), toFakeCapability)

raw ∷ EngineEnv → IO ()
raw env = fieldThree env `writeIORef` 1

viaCapability ∷ EngineEnv → IO ()
viaCapability env = fkFieldOne (toFakeCapability env) `Ref.writeIORef` 2
"""

# Redundant parentheses change nothing -- around the primitive in a
# prefix application, and around an infix operand. But a parenthesized
# primitive that something else is APPLYING to is an argument being
# passed on, not a write here.
_PARENTHESIZED = """\
module Parens.Mod where

import Data.IORef

import Engine.Core.State (EngineEnv, fieldOne, fieldTwo, fieldThree)

aroundThePrimitive ∷ EngineEnv → IO ()
aroundThePrimitive env = (writeIORef) (fieldOne env) 1

aroundTheOperand ∷ EngineEnv → IO ()
aroundTheOperand env = ((fieldTwo env)) `writeIORef` 2

passedOnward ∷ EngineEnv → IO ()
passedOnward env = withLogging (writeIORef) (fieldThree env) 3
"""

# Parentheses around the ACCESSOR itself, prefix and infix.
_PARENTHESIZED_ACCESSOR = """\
module ParenAccessor.Mod where

import Data.IORef

import Engine.Core.State (EngineEnv, fieldOne, fieldTwo, fieldThree)

prefix ∷ EngineEnv → IO ()
prefix env = writeIORef ((fieldOne) env) 1

infixForm ∷ EngineEnv → IO ()
infixForm env = ((fieldTwo) env) `writeIORef` 2

unapplied ∷ IORef Int → IO ()
unapplied _ = writeIORef (fieldThree) 3
"""

# A visible type application is not the value argument. Legal under
# GHC2024's default `TypeApplications`, and invisible to a scan that
# expects the accessor immediately after the primitive.
_TYPE_APPLICATION = """\
module TypeApp.Mod where

import Data.IORef

import Engine.Core.State (EngineEnv, fieldOne, fieldTwo, fieldThree)

simple ∷ EngineEnv → IO ()
simple env = writeIORef @Int (fieldOne env) 1

grouped ∷ EngineEnv → IO ()
grouped env = writeIORef @(IORef Text) (fieldTwo env) 2

-- The type application sits INSIDE parentheses around the primitive.
insideParentheses ∷ EngineEnv → IO ()
insideParentheses env = (writeIORef @Int) (fieldThree env) 3
"""

# `$!` is the strict sibling of `$` and groups its argument the same
# way; the tokenizer splits it into two punctuation tokens.
_STRICT_APPLICATION = """\
module Strict.Mod where

import Data.IORef

import Engine.Core.State (EngineEnv, fieldOne, fieldTwo)

strict ∷ EngineEnv → IO ()
strict env = (writeIORef $! fieldOne env) 1

lazyControl ∷ EngineEnv → IO ()
lazyControl env = (writeIORef $ fieldTwo env) 2
"""

# All six recognized mutation primitives, so no spelling can leave the
# closed set unnoticed.
_ALL_PRIMITIVES = """\
module AllPrims.Mod where

import Data.IORef

import Engine.Core.State (EngineEnv, fieldOne)

a, b, c, d, e, f ∷ EngineEnv → IO ()
a env = writeIORef (fieldOne env) 1
b env = atomicWriteIORef (fieldOne env) 2
c env = modifyIORef (fieldOne env) (+ 1)
d env = modifyIORef' (fieldOne env) (+ 1)
e env = atomicModifyIORef (fieldOne env) (\\n → (n, ()))
f env = atomicModifyIORef' (fieldOne env) (\\n → (n, ()))
"""

# A BARE import grants everything the module exports; the remaining
# import shape the scan must honour at scan level.
_BARE_IMPORTER = """\
module BareImport.Mod where

import Data.IORef

import Engine.Core.State

bump ∷ EngineEnv → IO ()
bump env = writeIORef (fieldOne env) 1
"""

# An argument is plainly being formed and its head is not an
# identifier: requirement 6's blocking case. Beside it, two shapes that
# form NO argument here and are therefore ordinary non-writes.
_UNREADABLE = """\
module Unreadable.Mod where

import Data.IORef

import Engine.Core.State (EngineEnv, fieldOne)

unboxed ∷ EngineEnv → IO ()
unboxed env = writeIORef (# fieldOne env #) 1
"""

# `OverloadedRecordDot` field access. The scan cannot read it as an
# accessor application, and taking `env` as the argument head would
# quietly make this a non-write -- so it is unclassifiable, and
# requirement 6 reports it. Spaced composition is ordinary code and
# must NOT be swept up with it.
_RECORD_DOT = """\
module RecordDot.Mod where

import Data.IORef

import Engine.Core.State (EngineEnv, fieldOne)

viaDot ∷ EngineEnv → IO ()
viaDot env = modifyIORef' (env.fieldOne) id
"""

_COMPOSED_ARGUMENT = """\
module Composed.Mod where

import Data.IORef

import Engine.Core.State (EngineEnv, fieldOne)

viaComposition ∷ EngineEnv → IO ()
viaComposition env = modifyIORef' (chooseRef . pick $ env) id
"""

# A primitive handed to another function UNPARENTHESIZED is still
# being handed on: the tokens after it are that function's arguments,
# not its own. The capability half must also keep its residue entry,
# which a phantom inline use would swallow.
_UNPARENTHESIZED_VALUE = """\
module PassedOn.Mod where

import Data.IORef

import Engine.Core.State (EngineEnv, fieldOne)
import Engine.Core.Capability.Fake (FakeCapability(..), toFakeCapability)

raw ∷ EngineEnv → IO ()
raw env = withLogging writeIORef (fieldOne env) 1

viaCapability ∷ EngineEnv → IO ()
viaCapability env =
    withLogging writeIORef (fkFieldTwo (toFakeCapability env)) 2
"""

# A keyword lexes as an identifier but applies to nothing, so a
# primitive after one IS in head position -- the shape at
# `src/Unit/Thread/Movement/Climb.hs:86`.
# The same hand-off spread over lines. A newline does not end an
# application; the continuation is indented past the line that opened
# it, and that is what distinguishes it from a sibling statement.
_MULTILINE_VALUE = """\
module MultiPassed.Mod where

import Data.IORef

import Engine.Core.State (EngineEnv, fieldOne)
import Engine.Core.Capability.Fake (FakeCapability(..), toFakeCapability)

raw ∷ EngineEnv → IO ()
raw env = withLogging
    writeIORef
    (fieldOne env)
    1

viaCapability ∷ EngineEnv → IO ()
viaCapability env = withLogging
    writeIORef
    (fkFieldTwo (toFakeCapability env))
    2
"""

# Sibling statements at the same column are NOT continuations, however
# the previous one ended -- and it very often ends in `)`.
_SIBLING_STATEMENTS = """\
module Siblings.Mod where

import Data.IORef

import Engine.Core.State (EngineEnv, fieldThree)

run ∷ EngineEnv → IO ()
run env = do
    pure ()
    writeIORef (fieldThree env) 1
"""

# An operator SECTION applied prefix. `($)` applies its arguments and
# `(.)` composes them -- opposite consequences for whether a write
# happens here, and a textual scan cannot tell which. Unreadable, so
# it blocks rather than passing silently.
_OPERATOR_SECTION = """\
module Section.Mod where

import Data.IORef

import Engine.Core.State (EngineEnv, fieldOne)

applied ∷ EngineEnv → IO ()
applied env = ($) writeIORef (fieldOne env) 1
"""

# A parenthesized group holding a real expression is not a section, and
# is the ordinary passed-on case.
_PARENTHESIZED_CALLEE = """\
module Callee.Mod where

import Data.IORef

import Engine.Core.State (EngineEnv, fieldTwo)

handedOn ∷ EngineEnv → IO ()
handedOn env = (chooseLogger env) writeIORef (fieldTwo env) 2
"""

_AFTER_KEYWORD = """\
module AfterKeyword.Mod where

import Data.IORef

import Engine.Core.State (EngineEnv, fieldThree)

pick ∷ EngineEnv → Bool → IO ()
pick env done =
    if done
        then pure ()
        else writeIORef (fieldThree env) 1
"""

_PRIMITIVE_AS_VALUE = """\
module AsValue.Mod where

import Data.IORef

import Engine.Core.State (EngineEnv, fieldOne)

handedOn ∷ [IORef Int] → IO ()
handedOn refs = mapM_ (writeIORef) refs
"""

# The same two writes, but importing the accessors BY NAME rather than
# through `FakeCapability(..)`: the import list itself then contains the
# accessor tokens, so it is what proves an import declaration is not a
# use. `fkFieldTwo` is imported and never used.
_EXPLICIT_IMPORTER = """\
module Explicit.Mod where

import Data.IORef

import Engine.Core.State (EngineEnv)
import Engine.Core.Capability.Fake
  ( fkFieldOne
  , fkFieldTwo
  , toFakeCapability
  )

bump ∷ EngineEnv → IO ()
bump env = writeIORef (fkFieldOne (toFakeCapability env)) 1
"""

# One expression, four lines: nothing here is findable by a line-wise
# scan, matching the real
# `Engine.Scripting.Lua.API.StructureArt`/`Engine.Input.Thread.Dispatch`
# multiline mutations.
_MULTILINE_WRITER = """\
module Multi.Mod where

import Data.IORef

import Engine.Core.State (EngineEnv)
import Engine.Core.Capability.Fake (FakeCapability(..), toFakeCapability)

bump ∷ EngineEnv → IO ()
bump env =
    atomicModifyIORef'
        (fkFieldOne
            (toFakeCapability env))
        (\\n → (n + 1, ()))
"""

# A comment marker inside a STRING is text. `src/Engine/Scripting/Lua/
# Thread/Dispatch.hs:257` carries a real one -- `<> " -- " <> reason` --
# and truncating there also removes the string's closing quote, which
# desynchronises everything after it.
_STRING_COMMENT_MARKER = """\
module StringMarker.Mod where

import Data.IORef

import Engine.Core.State (EngineEnv, fieldOne, fieldTwo, fieldThree)

marked ∷ EngineEnv → IO ()
marked env = let marker = "--" in writeIORef (fieldOne env) 1

afterwards ∷ EngineEnv → IO ()
afterwards env = writeIORef (fieldTwo env) 2

nested ∷ EngineEnv → IO ()
nested env = {- outer {- inner -} still a comment -}
    writeIORef (fieldThree env) 3
"""

# `T(..)` grants `T`'s selectors and nobody else's, so a wildcard on
# some OTHER type in the same module puts no `EngineEnv` field in scope.
_FOREIGN_WILDCARD = """\
module ForeignWildcard.Mod where

import Data.IORef

import Engine.Core.State (EngineEnv, WindowState(..))

fieldOne ∷ EngineEnv → IORef Int
fieldOne _ = error "this module's own helper, not the accessor"

use ∷ EngineEnv → IO ()
use env = writeIORef (fieldOne env) 1
"""

# The control: the wildcard that DOES own the field.
_OWNING_WILDCARD = """\
module OwningWildcard.Mod where

import Data.IORef

import Engine.Core.State (EngineEnv(..))

use ∷ EngineEnv → IO ()
use env = writeIORef (fieldTwo env) 2
"""

#: The paths this owner's own fixtures occupy in the synthetic tree.
#: The trap module's lives in the support module's `SHARED_PATHS`,
#: beside the fixture itself, because the map owner drives it too.
_PATHS = {
    "narrow": "src/Narrow/Mod.hs",
    "homonym": "src/Homonym/Mod.hs",
    "qualified": "src/Qualified/Mod.hs",
    "misqualified": "src/Misqualified/Mod.hs",
    "qualPrim": "src/QualPrim/Mod.hs",
    "qualOnly": "src/QualOnly/Mod.hs",
    "bare": "src/Bare/Mod.hs",
    "hiding": "src/Hiding/Mod.hs",
    "infix": "src/Infix/Mod.hs",
    "bareOperand": "src/BareOperand/Mod.hs",
    "parens": "src/Parens/Mod.hs",
    "parenAccessor": "src/ParenAccessor/Mod.hs",
    "typeApp": "src/TypeApp/Mod.hs",
    "strict": "src/Strict/Mod.hs",
    "allPrims": "src/AllPrims/Mod.hs",
    "bareImport": "src/BareImport/Mod.hs",
    "unreadable": "src/Unreadable/Mod.hs",
    "recordDot": "src/RecordDot/Mod.hs",
    "composed": "src/Composed/Mod.hs",
    "passedOn": "src/PassedOn/Mod.hs",
    "multiPassed": "src/MultiPassed/Mod.hs",
    "siblings": "src/Siblings/Mod.hs",
    "section": "src/Section/Mod.hs",
    "callee": "src/Callee/Mod.hs",
    "afterKeyword": "src/AfterKeyword/Mod.hs",
    "asValue": "src/AsValue/Mod.hs",
    "explicit": "src/Explicit/Mod.hs",
    "multiline": "src/Multi/Mod.hs",
    "stringMarker": "src/StringMarker/Mod.hs",
    "foreignWildcard": "src/ForeignWildcard/Mod.hs",
    "owningWildcard": "src/OwningWildcard/Mod.hs",
}


def _writer_sources(**modules: str) -> dict[str, str]:
    """This owner's synthetic tree, over its own paths."""
    return writer_sources(_PATHS, modules)


# ----- This owner's cases ----------------------------------------------

def test_out_of_scope_names_are_not_writes():
    """The import-scope gate, on its own. `Homonym.Mod` defines its own
    `fieldOne` helper and APPLIES it exactly the way a real write
    applies the accessor, so the write's shape says nothing; the only
    thing that distinguishes it from the field is that
    `Engine.Core.State` is imported for the `EngineEnv` TYPE alone. The
    two gates are independent on purpose: neither is asked to be
    complete by itself."""
    writes, residue = _scan(_writer_sources(homonym=_LOCAL_HOMONYM))
    expect(writes["fieldOne"] == set(),
           f"a name the module never imported cannot be the accessor, "
           f"got: {sorted(writes['fieldOne'])}")
    expect(residue == [],
           f"the fixture names no capability accessor, so it contributes "
           f"no residue, got: {residue}")


def test_comments_and_bare_arguments_are_not_writes():
    """The two remaining false-positive gates, in the same fixture as
    the residue case so one module proves all three: commentary that
    NAMES a write does not perform one, and a BARE first argument is
    never the accessor (`shadowed`'s parameter here) because an accessor
    projects out of a handle and so cannot itself be the `IORef`."""
    writes, residue = _scan(_writer_sources(trap=_TRAP_MODULE,
                                            narrow=_TYPE_ONLY_IMPORTER))
    expect(writes["fieldOne"] == set() and writes["fieldTwo"] == set(),
           f"neither a commented-out write, a bare local argument, nor a "
           f"type-only import may produce a write, got: fieldOne="
           f"{sorted(writes['fieldOne'])}, fieldTwo="
           f"{sorted(writes['fieldTwo'])}")
    expect(all(item.module != "Narrow.Mod" for item in residue),
           f"a module that never names a capability accessor contributes "
           f"no residue, got: {residue}")


def test_backticked_infix_mutations_are_writes():
    """Any two-argument function may be written infix, so
    ``(fieldOne env) `writeIORef` 1`` is the same direct write with its
    arguments swapped. A scan that only looked to the RIGHT of the
    primitive would miss it in silence."""
    writes, _ = _scan(_writer_sources(infix=_INFIX_WRITER))
    expect(writes["fieldOne"] == {"Infix.Mod"},
           f"a backticked raw-accessor write must be attributed, got: "
           f"{sorted(writes['fieldOne'])}")
    expect(writes["fieldTwo"] == {"Infix.Mod"},
           f"a backticked, qualified, capability-accessor write must be "
           f"attributed too, got: {sorted(writes['fieldTwo'])}")

    bare, _ = _scan(_writer_sources(bareOperand=_BARE_OPERAND))
    expect(bare["fieldThree"] == {"BareOperand.Mod"},
           f"an UNPARENTHESIZED left operand is the same write -- a "
           f"backtick binds looser than application, got: "
           f"{sorted(bare['fieldThree'])}")
    expect(bare["fieldOne"] == {"BareOperand.Mod"},
           f"and the same holds for a bare capability-accessor operand "
           f"under a qualified primitive, got: "
           f"{sorted(bare['fieldOne'])}")


def test_redundant_parentheses_change_nothing():
    """Parentheses around a primitive in a prefix application, and
    around an infix operand, are the same write. A primitive that
    something else is APPLYING to is not: it is being passed onward,
    which D-5 reports rather than attributes."""
    writes, _ = _scan(_writer_sources(parens=_PARENTHESIZED))
    expect(writes["fieldOne"] == {"Parens.Mod"},
           f"`(writeIORef) (fieldOne env) 1` is a write, got: "
           f"{sorted(writes['fieldOne'])}")
    expect(writes["fieldTwo"] == {"Parens.Mod"},
           f"a doubly parenthesized infix operand is a write, got: "
           f"{sorted(writes['fieldTwo'])}")
    expect(writes["fieldThree"] == set(),
           f"a primitive handed to another function is not this module's "
           f"write, got: {sorted(writes['fieldThree'])}")


def test_parentheses_around_the_accessor_change_nothing():
    """`writeIORef ((fieldOne) env) 1` applies exactly what
    `writeIORef (fieldOne env) 1` does, prefix or infix. Only the
    closers balancing the openers stepped over are consumed, so a
    genuinely unapplied `(fieldThree)` still is not a write."""
    writes, _ = _scan(_writer_sources(parenAccessor=_PARENTHESIZED_ACCESSOR))
    expect(writes["fieldOne"] == {"ParenAccessor.Mod"},
           f"a parenthesized prefix accessor head is a write, got: "
           f"{sorted(writes['fieldOne'])}")
    expect(writes["fieldTwo"] == {"ParenAccessor.Mod"},
           f"a parenthesized infix accessor head is a write, got: "
           f"{sorted(writes['fieldTwo'])}")
    expect(writes["fieldThree"] == set(),
           f"an unapplied accessor is still not a write, got: "
           f"{sorted(writes['fieldThree'])}")


def test_visible_type_applications_are_skipped():
    """`writeIORef @Int (fieldOne env) 1` is a direct write. A scan that
    expects the accessor immediately after the primitive stops at the
    `@` and lets an undeclared writer through in silence."""
    writes, _ = _scan(_writer_sources(typeApp=_TYPE_APPLICATION))
    expect(writes["fieldOne"] == {"TypeApp.Mod"},
           f"a type application by name must be stepped over, got: "
           f"{sorted(writes['fieldOne'])}")
    expect(writes["fieldTwo"] == {"TypeApp.Mod"},
           f"a parenthesized type application must be stepped over "
           f"whole, got: {sorted(writes['fieldTwo'])}")
    expect(writes["fieldThree"] == {"TypeApp.Mod"},
           f"a type application INSIDE parentheses around the primitive "
           f"must be stepped over before the closer, got: "
           f"{sorted(writes['fieldThree'])}")


def test_strict_application_groups_like_lazy():
    """`$!` is `$` with a `seq`, and groups its argument identically. The
    tokenizer splits it into two punctuation tokens, so its `!` has to
    be stepped over or the write disappears."""
    writes, _ = _scan(_writer_sources(strict=_STRICT_APPLICATION))
    expect(writes["fieldOne"] == {"Strict.Mod"},
           f"`writeIORef $! fieldOne env` is a write, got: "
           f"{sorted(writes['fieldOne'])}")
    expect(writes["fieldTwo"] == {"Strict.Mod"},
           f"and so is the lazy control, got: "
           f"{sorted(writes['fieldTwo'])}")


def test_a_first_argument_must_be_applied():
    """`_first_argument_head` directly, on the two halves of the rule:
    the argument must be GROUPED (parenthesized, or reached through
    `$`), and within that group the accessor must be APPLIED. A bare
    `prim ref v` and a parenthesized-but-unapplied `prim (ref) v` are
    both non-answers, and only the second distinguishes the halves."""
    def head_of(text):
        tokens = tokenize_haskell(text)
        return _first_argument_head(tokens, 0)

    expect(head_of("writeIORef (fieldOne env) 1") == 2,
           "an applied accessor inside parens is the first-argument head")
    expect(head_of("writeIORef $ fieldOne env") == 2,
           "`$` groups the first argument just as parentheses do")
    expect(head_of("writeIORef $! fieldOne env") == 3,
           "and `$!` groups it identically")
    expect(head_of("writeIORef (fieldOne) 1") is None,
           "a parenthesized but unapplied name is not an accessor "
           "application")
    expect(head_of("writeIORef ((fieldOne) env) 1") == 3,
           "parentheses around the accessor itself change nothing")
    expect(head_of("writeIORef fieldOne 1") is None,
           "a bare first argument is never an accessor application")
    expect(head_of("writeIORef @Int (fieldOne env) 1") == 4,
           "a visible type application is stepped over, not treated as "
           "the value argument")
    expect(head_of("writeIORef @Int fieldOne 1") is None,
           "stepping over a type application must not turn a bare "
           "argument into an application")

    def infix_head_of(text):
        tokens = tokenize_haskell(text)
        index = next(i for i, token in enumerate(tokens)
                     if token.text.endswith("writeIORef"))
        return _infix_left_operand_head(tokens, index)

    expect(infix_head_of("(fieldOne env) `writeIORef` 1") == 1,
           "a backticked primitive's left operand is its accessor")
    expect(infix_head_of("(fieldOne) `writeIORef` 1") is None,
           "an unapplied left operand is not an accessor application")
    expect(infix_head_of("writeIORef (fieldOne env) 1") is None,
           "a prefix application has no infix left operand")
    expect(infix_head_of("(fieldOne env) `writeIORef 1") is None,
           "an unterminated backtick is not an infix application")
    expect(infix_head_of("fieldOne env `writeIORef` 1") == 0,
           "an unparenthesized applied operand is read back to its head")
    expect(infix_head_of("fieldOne `writeIORef` 1") is None,
           "an unapplied bare operand is not an accessor application")
    expect(infix_head_of("x >> fieldOne env `writeIORef` 1") == 3,
           "the walk stops at the operator, not at the start of the line")
    expect(infix_head_of("fkFieldOne (cap env) `writeIORef` 1") == 0,
           "a trailing `)` closing an ARGUMENT is not the operand's own")
    expect(infix_head_of("((fieldOne) env) `writeIORef` 1") == 2,
           "and the same holds for an infix operand's head")
    expect(infix_head_of("(pick cfg) fieldOne `writeIORef` 1") == 1,
           "a group to the LEFT of an identifier is the application's "
           "head, so the identifier is its argument, not the accessor")

    # `_applied_head` consumes exactly the closers that balance the
    # openers written directly before the accessor. Reading past them
    # would let an unapplied accessor borrow whatever follows the group
    # it sits in.
    nested = tokenize_haskell("f ((fieldOne)) env")
    head = next(i for i, token in enumerate(nested)
                if token.text == "fieldOne")
    expect(_applied_head(nested, head) == head,
           "two openers before the accessor balance two closers, after "
           "which `env` applies it")
    trailing = tokenize_haskell("f (fieldOne)) env")
    head = next(i for i, token in enumerate(trailing)
                if token.text == "fieldOne")
    expect(_applied_head(trailing, head) is None,
           "one opener balances one closer, and the next token is "
           "another closer, not an argument")


def test_qualified_accessors_are_resolved():
    """A qualified spelling names the field exactly as the bare one
    does, through the module's own name or an `as` alias, so it must be
    attributed rather than silently missed -- otherwise
    `import qualified Engine.Core.State as State` is a hole in the
    gate."""
    writes, _ = _scan(_writer_sources(qualified=_QUALIFIED_WRITER))
    expect(writes["fieldTwo"] == {"Qualified.Mod"},
           f"`State.fieldTwo` must resolve to the raw field, got: "
           f"{sorted(writes['fieldTwo'])}")
    expect(writes["fieldOne"] == {"Qualified.Mod"},
           f"`Cap.fkFieldOne` must resolve through the capability "
           f"projection, got: {sorted(writes['fieldOne'])}")

    violations = audit_writer_modules(
        writes, _WRITER_FIELDS,
        declared={f: frozenset() for f in _WRITER_FIELDS})
    expect(len(violations) == 2 and all("Qualified.Mod" in v
                                        for v in violations),
           f"an undeclared qualified write must be a violation like any "
           f"other, got: {violations}")


def test_a_qualifier_must_name_the_owning_module():
    """The other half of qualified resolution: a prefix bound to a
    DIFFERENT module does not name this field, and an `as` alias
    REPLACES the module's own name as a qualifier rather than joining
    it. Neither line may be attributed."""
    writes, _ = _scan(_writer_sources(misqualified=_MISQUALIFIED))
    expect(writes["fieldTwo"] == set(),
           f"neither a foreign qualifier nor an alias-replaced module "
           f"name may resolve to the field, got: "
           f"{sorted(writes['fieldTwo'])}")


def test_qualified_mutation_primitives_are_recognized():
    """A mutation primitive under a qualifier -- `Ref.writeIORef`, from
    `import qualified Data.IORef as Ref` -- is the same write, and
    missing it would be a silent hole rather than a conservative
    miss."""
    writes, _ = _scan(_writer_sources(qualPrim=_QUALIFIED_PRIMITIVE))
    expect(writes["fieldOne"] == {"QualPrim.Mod"},
           f"`Ref.writeIORef` must be recognized as a mutation "
           f"primitive, got: {sorted(writes['fieldOne'])}")


def test_a_hiding_clause_removes_a_name_from_scope():
    """`hiding` brings in everything EXCEPT the listed names, so a
    module that hides `fieldOne` and defines its own is not writing the
    field -- while everything it did NOT hide stays in scope. Treating
    a `hiding` import as simply unrestricted loses that."""
    writes, _ = _scan(_writer_sources(hiding=_HIDING_IMPORTER))
    expect(writes["fieldOne"] == set(),
           f"a hidden name is out of scope, so the module's own helper "
           f"is not the accessor, got: {sorted(writes['fieldOne'])}")
    expect(writes["fieldTwo"] == {"Hiding.Mod"},
           f"a `hiding` clause must not remove anything it did not "
           f"name, got: {sorted(writes['fieldTwo'])}")

    declarations = parse_imports(
        "import Engine.Core.State hiding (fieldOne)\n")
    expect(not imports_name(declarations, "Engine.Core.State",
                            "fieldOne", ""),
           "the hidden name is not in scope")
    expect(imports_name(declarations, "Engine.Core.State",
                        "fieldTwo", ""),
           "everything else still is")


def test_a_qualified_only_import_excludes_the_bare_spelling():
    """`qualified` removes the UNQUALIFIED spelling from scope entirely,
    so a module-local homonym is not the field even though the owner is
    imported -- while the qualified spelling in the same module still
    is. Merging every import of a module into one scope answer loses
    exactly this distinction."""
    writes, _ = _scan(_writer_sources(qualOnly=_QUALIFIED_ONLY))
    expect(writes["fieldOne"] == set(),
           f"a bare `fieldOne` is out of scope under a qualified-only "
           f"import, got: {sorted(writes['fieldOne'])}")
    expect(writes["fieldTwo"] == {"QualOnly.Mod"},
           f"`State.fieldTwo` in the same module must still be "
           f"attributed, got: {sorted(writes['fieldTwo'])}")


def test_import_declarations_record_qualification_and_alias():
    """`parse_imports` keeps each declaration separate, because one
    module is legitimately imported twice on different terms and each
    declaration carries its own answer."""
    declarations = parse_imports(
        "import Engine.Core.State (EngineEnv, fieldOne)\n"
        "import qualified Engine.Core.State as State\n"
        "import Data.IORef qualified as Ref\n"
        "import Data.Map hiding (lookup)\n"
        "import Engine.Core.Capability.Fake (FakeCapability(..))\n"
        "import Engine.Core.Defaults\n")
    shape = [(d.module, d.qualified, d.qualifier,
              None if d.names is None else sorted(d.names),
              sorted(d.wildcards))
             for d in declarations]
    expect(shape == [
        ("Engine.Core.State", False, "Engine.Core.State",
         ["EngineEnv", "fieldOne"], []),
        ("Engine.Core.State", True, "State", None, []),
        ("Data.IORef", True, "Ref", None, []),
        ("Data.Map", False, "Data.Map", None, []),
        ("Engine.Core.Capability.Fake", False,
         "Engine.Core.Capability.Fake", [], ["FakeCapability"]),
        ("Engine.Core.Defaults", False, "Engine.Core.Defaults", None, []),
    ], f"all six import shapes -- explicit list, qualified-with-alias, "
       f"`ImportQualifiedPost`, `hiding`, a `(..)` wildcard and a bare "
       f"import -- must each be recorded with its own qualification, "
       f"qualifier and name list, got: {shape}")

    expect(imports_name(declarations, "Engine.Core.State", "fieldOne", ""),
           "the unqualified declaration puts the bare name in scope")
    expect(imports_name(declarations, "Engine.Core.State", "fieldOne",
                        "State"),
           "the qualified declaration puts `State.fieldOne` in scope")
    expect(not imports_name(declarations[1:], "Engine.Core.State",
                            "fieldOne", ""),
           "a qualified-only import puts NO bare spelling in scope")
    expect(not imports_name(declarations, "Engine.Core.State", "fieldOne",
                            "Ref"),
           "a qualifier bound to another module resolves nothing here")
    expect(not imports_name(declarations[:1], "Engine.Core.State",
                            "fieldTwo", ""),
           "an explicit list brings in the names it enumerates and no "
           "others")


def test_a_bare_argument_surfaces_as_residue():
    """A bare accessor name in a mutation primitive's first argument is
    never attributed -- and when it is a CAPABILITY accessor (the record
    wildcard the rule's one blind spot needs) it is not silently
    dropped either: with no application to consume it inline, it lands
    in the pass-on residue where D-5 can count it."""
    writes, residue = _scan(_writer_sources(bare=_BARE_ARGUMENT))
    expect(writes["fieldOne"] == set() and writes["fieldTwo"] == set(),
           f"a bare first argument must never be attributed, got: "
           f"fieldOne={sorted(writes['fieldOne'])}, "
           f"fieldTwo={sorted(writes['fieldTwo'])}")
    bare = [item for item in residue if item.module == "Bare.Mod"]
    # `(fieldTwo)` is parenthesized but never applied -- the grouping
    # test alone would let it through, so both halves of the rule are
    # exercised here.
    expect(len(bare) == 1 and bare[0].accessor == "fkFieldOne"
           and bare[0].field == "fieldOne",
           f"the wildcard-bound capability accessor must be reported as "
           f"residue rather than dropped, got: {bare}")


def test_import_declarations_are_not_uses():
    """An import list names the accessor; naming one is not using one.
    `Explicit.Mod` imports `fkFieldOne` and `fkFieldTwo` by name across
    four lines and writes only the first, so the import declaration --
    the one place both tokens appear together -- must register as
    neither a write nor a residue use. It also drives
    `parse_imports`' explicit-name path, which `FakeCapability(..)`
    never reaches."""
    writes, residue = _scan(_writer_sources(explicit=_EXPLICIT_IMPORTER))
    expect(writes["fieldOne"] == {"Explicit.Mod"},
           f"an accessor imported by name must still be in scope at its "
           f"write site, got: {sorted(writes['fieldOne'])}")
    expect(writes["fieldTwo"] == set(),
           f"`fkFieldTwo` appears only in the import list, so nothing may "
           f"be attributed to `fieldTwo`, got: {sorted(writes['fieldTwo'])}")
    named = [item for item in residue if item.module == "Explicit.Mod"]
    expect(named == [],
           f"an import declaration must not register as a use, got: {named}")


def test_multiline_expressions_are_scanned():
    """The scan reads complete EXPRESSIONS: a mutation whose accessor
    argument sits three lines below the primitive is one token
    sequence, exactly like the real `rhStructureArtCatalogRef` and
    `rvFramebufferMinimizeGenRef` sites."""
    writes, _ = _scan(_writer_sources(multiline=_MULTILINE_WRITER))
    expect(writes["fieldOne"] == {"Multi.Mod"},
           f"a four-line `atomicModifyIORef'` must be detected, got: "
           f"{sorted(writes['fieldOne'])}")


def test_tokenizer_skips_literals_and_keeps_line_numbers():
    """String and character literals are consumed whole, so an accessor
    name inside one is not a token; identifier primes stay part of the
    identifier; and every token carries its own 1-based line, which is
    what makes the residue report citable."""
    tokens = tokenize_haskell(
        'a = "fieldOne"\nb = \'x\'\nmodifyIORef\' c\n')
    texts = [t.text for t in tokens if t.kind == "id"]
    expect("fieldOne" not in texts,
           f"an accessor name inside a string literal must not tokenize as "
           f"an identifier, got: {texts}")
    expect("modifyIORef'" in texts,
           f"a primed identifier must tokenize whole, got: {texts}")
    line = next(t.line for t in tokens if t.text == "modifyIORef'")
    expect(line == 3,
           f"`modifyIORef'` sits on line 3, got: {line}")


def test_every_recognized_primitive_is_read():
    """All six mutation primitives in the closed set, each on the same
    field, so a spelling that stopped being recognized shows up as a
    missing write SITE rather than a silently smaller map."""
    scan = _full_scan(_writer_sources(allPrims=_ALL_PRIMITIVES))
    expect(scan.writes["fieldOne"] == {"AllPrims.Mod"},
           f"every primitive must attribute to the field, got: "
           f"{sorted(scan.writes['fieldOne'])}")
    attributed = [site for site in scan.sites
                  if site.module == "AllPrims.Mod" and site.kind == "write"]
    expect(len(attributed) == 6,
           f"all six spellings must be read as writes, got "
           f"{len(attributed)}")


def test_a_bare_import_brings_the_accessor_into_scope():
    """The last import shape: a bare import grants everything the target
    exports, so the accessor is in scope without being named."""
    writes, _ = _scan(_writer_sources(bareImport=_BARE_IMPORTER))
    expect(writes["fieldOne"] == {"BareImport.Mod"},
           f"a bare import puts the accessor in scope, got: "
           f"{sorted(writes['fieldOne'])}")


def test_an_unreadable_mutation_site_blocks():
    """Requirement 6. An argument is plainly being formed and its head
    is not an identifier, so the scan says so and the audit fails --
    which is how a spelling outside the recognized set stops the gate
    instead of silently dropping a write."""
    scan = _full_scan(_writer_sources(unreadable=_UNREADABLE))
    kinds = [site.kind for site in scan.sites
             if site.module == "Unreadable.Mod"]
    expect(kinds == ["unclassifiable"],
           f"the site must be recorded as unclassifiable, got: {kinds}")
    violations = audit_mutation_sites(scan.sites)
    expect(len(violations) == 1 and "Unreadable" in violations[0],
           f"and that must be a blocking violation, got: {violations}")


def test_a_primitive_must_be_in_head_position():
    """`withLogging writeIORef (fieldOne env) 1` hands the primitive to
    `withLogging`. Reading the tokens after it as its own arguments
    invents a write — and, with a capability accessor, hides that
    accessor's pass-on residue entry behind a phantom inline use.

    A KEYWORD before the primitive applies to nothing, so `else
    writeIORef (...) ...` is head position; layout ends a statement
    with no token at all, so a preceding identifier or bracket only
    counts on the SAME line."""
    scan = _full_scan(_writer_sources(passedOn=_UNPARENTHESIZED_VALUE))
    expect(scan.writes["fieldOne"] == set()
           and scan.writes["fieldTwo"] == set(),
           f"a primitive being passed on writes nothing, got: "
           f"fieldOne={sorted(scan.writes['fieldOne'])}, "
           f"fieldTwo={sorted(scan.writes['fieldTwo'])}")
    expect([site.kind for site in scan.sites
            if site.module == "PassedOn.Mod"] == ["other", "other"],
           "both sites classify as ordinary non-writes")
    residue = [item for item in scan.residue
               if item.module == "PassedOn.Mod"]
    expect(len(residue) == 1 and residue[0].accessor == "fkFieldTwo",
           f"and the capability accessor keeps its residue entry, got: "
           f"{residue}")

    writes, _ = _scan(_writer_sources(afterKeyword=_AFTER_KEYWORD))
    expect(writes["fieldThree"] == {"AfterKeyword.Mod"},
           f"while a primitive after a keyword is in head position, "
           f"got: {sorted(writes['fieldThree'])}")

    # A newline does not end an application: the continuation is
    # indented past the line that opened it.
    scan = _full_scan(_writer_sources(multiPassed=_MULTILINE_VALUE))
    expect(scan.writes["fieldOne"] == set()
           and scan.writes["fieldTwo"] == set(),
           f"a multiline hand-off writes nothing either, got: "
           f"fieldOne={sorted(scan.writes['fieldOne'])}, "
           f"fieldTwo={sorted(scan.writes['fieldTwo'])}")
    expect(len([item for item in scan.residue
                if item.module == "MultiPassed.Mod"]) == 1,
           "and the capability accessor keeps its residue entry")

    # …while a sibling statement at the same column is a new statement,
    # however the previous one ended.
    writes, _ = _scan(_writer_sources(siblings=_SIBLING_STATEMENTS))
    expect(writes["fieldThree"] == {"Siblings.Mod"},
           f"a statement following `pure ()` is not its continuation, "
           f"got: {sorted(writes['fieldThree'])}")


def test_an_operator_section_applying_a_primitive_blocks():
    """`($) writeIORef (fieldOne env) 1` is a direct write and
    `(.) writeIORef f` is not, and nothing textual separates them. The
    site is therefore unreadable rather than silently `other` --
    recognizing each operator individually is the open-ended path this
    arc rejects.

    A parenthesized group holding a real expression is not a section,
    and stays the ordinary passed-on case."""
    scan = _full_scan(_writer_sources(section=_OPERATOR_SECTION))
    expect([site.kind for site in scan.sites
            if site.module == "Section.Mod"] == ["unclassifiable"],
           f"an applied operator section must block, got: "
           f"{[s.kind for s in scan.sites if s.module == 'Section.Mod']}")
    expect(len(audit_mutation_sites(scan.sites)) == 1, "and be reported")

    scan = _full_scan(_writer_sources(callee=_PARENTHESIZED_CALLEE))
    expect([site.kind for site in scan.sites
            if site.module == "Callee.Mod"] == ["other"],
           "a parenthesized expression callee is not a section")
    expect(scan.writes["fieldTwo"] == set()
           and audit_mutation_sites(scan.sites) == [],
           "so it is an ordinary hand-off: no write, and no block")


def test_record_dot_access_is_unclassifiable():
    """`modifyIORef' (env.fieldOne) id` is a direct mutation the scan
    cannot read. Taking `env` as the argument head would make it a
    silent non-write, which is exactly what requirement 6 exists to
    prevent, so the site blocks instead.

    Spaced composition tokenizes identically and is ordinary code —
    only the ABSENCE of a gap distinguishes them — so it must not be
    swept up with it."""
    scan = _full_scan(_writer_sources(recordDot=_RECORD_DOT))
    kinds = [site.kind for site in scan.sites
             if site.module == "RecordDot.Mod"]
    expect(kinds == ["unclassifiable"],
           f"record-dot access must block, got: {kinds}")
    expect(len(audit_mutation_sites(scan.sites)) == 1,
           "and be reported")

    scan = _full_scan(_writer_sources(composed=_COMPOSED_ARGUMENT))
    kinds = [site.kind for site in scan.sites
             if site.module == "Composed.Mod"]
    expect(kinds == ["other"],
           f"spaced composition is ordinary code, got: {kinds}")
    expect(audit_mutation_sites(scan.sites) == [],
           "and blocks nothing")


def test_a_primitive_used_as_a_value_is_not_unreadable():
    """The other side of requirement 6: a primitive that is not applied
    to anything HERE is being handed onward, which is an ordinary
    non-write, not an unreadable site. Confusing the two would make the
    guard fire on correct code."""
    scan = _full_scan(_writer_sources(asValue=_PRIMITIVE_AS_VALUE))
    kinds = [site.kind for site in scan.sites
             if site.module == "AsValue.Mod"]
    expect(kinds == ["other"],
           f"a primitive passed as a value classifies as `other`, got: "
           f"{kinds}")
    expect(audit_mutation_sites(scan.sites) == [],
           "and blocks nothing")


def test_a_comment_marker_inside_a_string_is_text():
    """`let marker = "--" in writeIORef (fieldOne env) 1` is a real
    write. Stripping at that `--` would drop it AND remove the string's
    closing quote, desynchronising every literal after it -- which is
    how three genuine mutation sites in
    `Engine.Scripting.Lua.Thread.Dispatch` were invisible until this
    was fixed. Block comments nest, too."""
    writes, _ = _scan(_writer_sources(stringMarker=_STRING_COMMENT_MARKER))
    expect(writes["fieldOne"] == {"StringMarker.Mod"},
           f"the write after a string containing `--` must survive, got: "
           f"{sorted(writes['fieldOne'])}")
    expect(writes["fieldTwo"] == {"StringMarker.Mod"},
           f"and so must everything after it, got: "
           f"{sorted(writes['fieldTwo'])}")
    expect(writes["fieldThree"] == {"StringMarker.Mod"},
           f"a NESTED block comment must close where Haskell closes it, "
           f"got: {sorted(writes['fieldThree'])}")

    stripped = _strip_haskell_comments(
        'a = "-- not a comment" -- but this is\n'
        'b = x --> y\n'
        'c = {- {- nested -} still -} kept\n')
    expect('"-- not a comment"' in stripped and "but this is" not in stripped,
           f"only the real comment is blanked, got: {stripped!r}")
    expect("x --> y" in stripped,
           f"a dash run continuing into a symbol is an operator, got: "
           f"{stripped!r}")
    expect("kept" in stripped and "nested" not in stripped
           and "still" not in stripped,
           f"a nested block comment closes at its own end, got: "
           f"{stripped!r}")
    expect(len(stripped.split("\n")) == 4,
           "and every line position is preserved")

    # A prime CONTINUES an identifier. Reading `x'` as opening a
    # character literal consumes `' '` and leaves the following quote
    # looking like a string opener, which swallows the rest of the file.
    primed = _strip_haskell_comments("f x' '\"' = 1 -- gone\ng = 2\n")
    expect("gone" not in primed and "g = 2" in primed,
           f"a primed identifier must not open a character literal, got: "
           f"{primed!r}")


def test_token_lines_survive_a_string_gap():
    """A Haskell string gap is a backslash, whitespace including
    NEWLINES, and another backslash. Skipping the escaped character
    without counting that newline reports every later token a line
    early -- and a residue entry or a blocking site then names the
    wrong source line, which is the one thing those reports exist to
    give."""
    tokens = tokenize_haskell('a = "start\\\n   \\end"\nb = 1\n')
    lines = {token.text: token.line for token in tokens if token.kind == "id"}
    expect(lines.get("a") == 1 and lines.get("b") == 3,
           f"the gap spans one newline, so `b` sits on line 3, got: "
           f"{lines}")

    plain = tokenize_haskell('a = "one\\ntwo"\nb = 1\n')
    expect({t.text: t.line for t in plain if t.kind == "id"}.get("b") == 2,
           "while an escaped `\\n` inside a literal spans no newline "
           "at all")


def test_a_wildcard_grants_only_its_own_type_s_selectors():
    """`import Engine.Core.State (WindowState(..))` brings in
    `WindowState`'s selectors, not `EngineEnv`'s -- so a module-local
    `fieldOne` used beside it is not the accessor. Treating every
    `(..)` as unrestricted access would make that a false writer, and
    then an undeclared-writer failure over code that touches no field."""
    writes, _ = _scan(_writer_sources(foreignWildcard=_FOREIGN_WILDCARD))
    expect(writes["fieldOne"] == set(),
           f"another type's wildcard puts no `EngineEnv` field in scope, "
           f"got: {sorted(writes['fieldOne'])}")

    writes, _ = _scan(_writer_sources(owningWildcard=_OWNING_WILDCARD))
    expect(writes["fieldTwo"] == {"OwningWildcard.Mod"},
           f"but the owning type's wildcard does, got: "
           f"{sorted(writes['fieldTwo'])}")

    declarations = parse_imports(
        "import Engine.Core.State (EngineEnv, WindowState(..))\n")
    expect(not imports_name(declarations, "Engine.Core.State", "fieldOne",
                            "", "EngineEnv"),
           "a foreign wildcard grants nothing here")
    expect(imports_name(parse_imports(
        "import Engine.Core.State (EngineEnv(..))\n"),
        "Engine.Core.State", "fieldOne", "", "EngineEnv"),
           "and the owning one grants everything it declares")


#: This owner's inventory of the scanner's lexical mechanics, in the relative order
#: these groups hold within the façade's run sequence.
#: `tools/test_engine_env_capability_writers.py` composes that
#: sequence from every owner's inventory; nothing here decides when,
#: or whether, it runs.
TESTS = (
    test_out_of_scope_names_are_not_writes,
    test_comments_and_bare_arguments_are_not_writes,
    test_backticked_infix_mutations_are_writes,
    test_redundant_parentheses_change_nothing,
    test_parentheses_around_the_accessor_change_nothing,
    test_visible_type_applications_are_skipped,
    test_strict_application_groups_like_lazy,
    test_a_first_argument_must_be_applied,
    test_qualified_accessors_are_resolved,
    test_a_qualifier_must_name_the_owning_module,
    test_qualified_mutation_primitives_are_recognized,
    test_a_hiding_clause_removes_a_name_from_scope,
    test_a_qualified_only_import_excludes_the_bare_spelling,
    test_import_declarations_record_qualification_and_alias,
    test_a_bare_argument_surfaces_as_residue,
    test_import_declarations_are_not_uses,
    test_multiline_expressions_are_scanned,
    test_tokenizer_skips_literals_and_keeps_line_numbers,
    test_every_recognized_primitive_is_read,
    test_a_bare_import_brings_the_accessor_into_scope,
    test_an_unreadable_mutation_site_blocks,
    test_a_primitive_must_be_in_head_position,
    test_an_operator_section_applying_a_primitive_blocks,
    test_record_dot_access_is_unclassifiable,
    test_a_primitive_used_as_a_value_is_not_unreadable,
    test_a_comment_marker_inside_a_string_is_text,
    test_token_lines_survive_a_string_gap,
    test_a_wildcard_grants_only_its_own_type_s_selectors,
)
