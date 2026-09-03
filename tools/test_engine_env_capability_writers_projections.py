#!/usr/bin/env python3
"""Capability-record and PROJECTION discovery in
engine_env_capability_writers.py -- #2059's fail-closed accessor map
(extracted from tools/test_engine_env_capability_writers.py by issue
#2228).

Eighteen groups over how a capability selector is canonicalized back to
the `EngineEnv` field it projects, and what must fail closed when it
cannot be. The binding side: a qualified accessor, a named alias
wrapper, semantically inert grouping (bare and wrapped), an unreadable
binding, a record with no discoverable projection, a binding onto a
dead accessor, and `parse_projection_binding_expressions` keeping the
unreadable ones. The declaration side: `data`, `newtype`, GADT, sum,
and uniformly indented capability modules, a declaration form the
audit does not model, a capability-TYPED field that is not a
declaration, a type with no record block, a read-only ref read that is
an inline use rather than a pass-on, and one selector belonging to two
capabilities. Two of the three real-repository assertions live here
because they are projection questions: projection completeness, and
that no live capability record is unreadable.

The map policy belongs to `test_engine_env_capability_writers_map`,
the scanner's lexical mechanics to `..._scanner`, and the writer-map
conformance case and the mutation-primitive provenance rule to
`..._conformance`.

Not a gate of its own. Run through the focused façade or the aggregate:

  python3 tools/test_engine_env_capability_writers.py --only projections
  python3 tools/test_engine_env_capability_audit.py
"""
from __future__ import annotations

import sys
from pathlib import Path

sys.path.insert(0, str(Path(__file__).resolve().parent))
from engine_env_capability_common import (  # type: ignore  # noqa: E402
    ENGINE_ENV_FILE, ENGINE_ENV_PATTERN, REPO_ROOT,
    canonical_projection_accessor, extract_record_fields,
    parse_projection_binding_expressions, scan_production_sources,
)
from engine_env_capability_writers import (  # type: ignore  # noqa: E402
    audit_capability_projection_completeness, audit_writer_modules,
    capability_accessor_map, capability_record_fields,
    discover_capability_records, parse_imports, resolve_primitive,
    undiscovered_capability_declarations,
)
from test_engine_env_capability_writers_support import (  # noqa: E402
    WRITER_FIELDS as _WRITER_FIELDS,
    expect,
    scan as _scan,
    writer_sources,
)


# ----- This owner's fixtures -------------------------------------------

# Two capability records exporting the SAME selector, projecting
# different fields. A consumer imports one of them qualified, so its
# imports are what say which `sharedRef` it means.
_ALPHA_CAPABILITY = """\
module Engine.Core.Capability.Alpha
  ( AlphaCapability(..)
  , toAlphaCapability
  ) where

import Engine.Core.State (EngineEnv, fieldOne)

data AlphaCapability = AlphaCapability
  { sharedRef ∷ IORef Int
  }

toAlphaCapability ∷ EngineEnv → AlphaCapability
toAlphaCapability env = AlphaCapability
  { sharedRef = fieldOne env
  }
"""

_BETA_CAPABILITY = """\
module Engine.Core.Capability.Beta
  ( BetaCapability(..)
  , toBetaCapability
  ) where

import Engine.Core.State (EngineEnv, fieldTwo)

data BetaCapability = BetaCapability
  { sharedRef ∷ IORef Text
  }

toBetaCapability ∷ EngineEnv → BetaCapability
toBetaCapability env = BetaCapability
  { sharedRef = fieldTwo env
  }
"""

# A capability module may import `Engine.Core.State` under an alias and
# project through the QUALIFIED accessor. Missing that spelling drops
# the record's accessors from the map entirely, and every write made
# through them with it.
_QUALIFIED_PROJECTION = """\
module Engine.Core.Capability.Gamma
  ( GammaCapability(..)
  , toGammaCapability
  ) where

import qualified Engine.Core.State as State

data GammaCapability = GammaCapability
  { gmFieldThree ∷ IORef Int
  }

toGammaCapability ∷ State.EngineEnv → GammaCapability
toGammaCapability env = GammaCapability
  { gmFieldThree = State.fieldThree env
  }
"""

# SS2.1's abstract-wrapper extension (issue #1896): a view field is
# `field = toReadOnlyRef (accessor env)`. It aliases the very same live
# handle, so it must canonicalize exactly as the bare form does.
_WRAPPED_PROJECTION = """\
module Engine.Core.Capability.DeltaView
  ( DeltaViewCapability(..)
  , toDeltaViewCapability
  ) where

import Engine.Core.ReadOnlyRef (ReadOnlyRef, toReadOnlyRef)
import Engine.Core.State (EngineEnv, fieldOne, fieldTwo)

data DeltaViewCapability = DeltaViewCapability
  { dvFieldOne ∷ ReadOnlyRef Int
  , dvFieldTwo ∷ ReadOnlyRef Text
  }

toDeltaViewCapability ∷ EngineEnv → DeltaViewCapability
toDeltaViewCapability env = DeltaViewCapability
  { dvFieldOne = toReadOnlyRef (fieldOne env)
  , dvFieldTwo = snapshotOf (fieldTwo env)
  }
"""

# Issue #2059: the SAME two bindings, spelled with semantically inert
# grouping. Haskell reads `(fieldOne env)` and `(fieldTwo) env` exactly
# as their ungrouped forms; before #2059 the surface regexes read
# NEITHER, so both selectors were absent from the accessor map and the
# consumer write below was filed as `other` while the gate exited 0.
_GROUPED_PROJECTION = """\
module Engine.Core.Capability.Epsilon
  ( EpsilonCapability(..)
  , toEpsilonCapability
  ) where

import Engine.Core.State (EngineEnv, fieldOne, fieldTwo)

data EpsilonCapability = EpsilonCapability
  { epFieldOne ∷ IORef Int
  , epFieldTwo ∷ IORef Text
  }

toEpsilonCapability ∷ EngineEnv → EpsilonCapability
toEpsilonCapability env = EpsilonCapability
  { epFieldOne = (fieldOne env)
  , epFieldTwo = (fieldTwo) env
  }
"""

# The write that must be attributed through the grouped projection --
# the module is in no field's writing-module map, so it must fail.
_GROUPED_CONSUMER = """\
module Grouped.Mod where

import Data.IORef

import Engine.Core.State (EngineEnv)
import Engine.Core.Capability.Epsilon
  (EpsilonCapability(..), toEpsilonCapability)

sneak ∷ EngineEnv → IO ()
sneak env = writeIORef (epFieldOne (toEpsilonCapability env)) 1
"""

# SS2.1's wrapped form carries the same grouping freedom, and it is
# parsed by its own path -- inside the wrapper's argument and around
# the whole application.
_GROUPED_WRAPPED_PROJECTION = """\
module Engine.Core.Capability.ZetaView
  ( ZetaViewCapability(..)
  , toZetaViewCapability
  ) where

import Engine.Core.ReadOnlyRef (ReadOnlyRef, toReadOnlyRef)
import Engine.Core.State (EngineEnv, fieldOne, fieldTwo)

data ZetaViewCapability = ZetaViewCapability
  { ztFieldOne ∷ ReadOnlyRef Int
  , ztFieldTwo ∷ ReadOnlyRef Text
  }

toZetaViewCapability ∷ EngineEnv → ZetaViewCapability
toZetaViewCapability env = ZetaViewCapability
  { ztFieldOne = toReadOnlyRef ((fieldOne env))
  , ztFieldTwo = (toReadOnlyRef (fieldTwo env))
  }
"""

# Two bindings the canonicalizer genuinely cannot read: an
# unrecognized wrapper (which might copy) and an operator expression.
# Widening the canonicalizer to guess at either is exactly what #2059
# forbids -- the requirement is that they FAIL, not that they parse.
_UNREADABLE_PROJECTION = """\
module Engine.Core.Capability.Eta
  ( EtaCapability(..)
  , toEtaCapability
  ) where

import Engine.Core.State (EngineEnv, fieldOne, fieldTwo, fieldThree)

data EtaCapability = EtaCapability
  { etFieldOne   ∷ IORef Int
  , etFieldTwo   ∷ IORef Text
  , etFieldThree ∷ Q.Queue Int
  }

toEtaCapability ∷ EngineEnv → EtaCapability
toEtaCapability env = EtaCapability
  { etFieldOne   = fieldOne env
  , etFieldTwo   = snapshotOf (fieldTwo env)
  , etFieldThree = chooseRef . pick $ env
  }
"""

# A record whose projection is not named `to<Name>Capability`, so no
# SS2.1 signature is discoverable: legal Haskell that loses EVERY
# selector of the record at once.
_UNPROJECTED_CAPABILITY = """\
module Engine.Core.Capability.Theta
  ( ThetaCapability(..)
  ) where

import Engine.Core.State (EngineEnv, fieldThree)

data ThetaCapability = ThetaCapability
  { thFieldThree ∷ Q.Queue Int
  }

thetaFrom ∷ EngineEnv → ThetaCapability
thetaFrom env = ThetaCapability
  { thFieldThree = fieldThree env
  }
"""

# A binding that canonicalizes onto a name that is not a live
# `EngineEnv` field. `capability_accessor_map` discards it at exactly
# the same cost as an unreadable one, so it must fail the same way.
_DEAD_ACCESSOR_PROJECTION = """\
module Engine.Core.Capability.Iota
  ( IotaCapability(..)
  , toIotaCapability
  ) where

import Engine.Core.State (EngineEnv, fieldRenamed)

data IotaCapability = IotaCapability
  { ioFieldRenamed ∷ IORef Int
  }

toIotaCapability ∷ EngineEnv → IotaCapability
toIotaCapability env = IotaCapability
  { ioFieldRenamed = fieldRenamed env
  }
"""

# GHC2024 enables `GADTs`, so this declares exactly the record
# `data KappaCapability = KappaCapability { ... }` declares -- the same
# two selectors, in the same scope. Recognizing only the ordinary form
# left the whole record undiscovered, which is a strictly worse silent
# omission than an unreadable field: nothing about it reached the map
# or the completeness gate.
_GADT_PROJECTION = """\
module Engine.Core.Capability.Kappa
  ( KappaCapability(..)
  , toKappaCapability
  ) where

import Engine.Core.State (EngineEnv, fieldOne, fieldTwo)

data KappaCapability where
  KappaCapability ∷ { kaFieldOne ∷ IORef Int
                    , kaFieldTwo ∷ IORef Text } → KappaCapability

toKappaCapability ∷ EngineEnv → KappaCapability
toKappaCapability env = KappaCapability
  { kaFieldOne = fieldOne env
  , kaFieldTwo = (fieldTwo env)
  }
"""

_GADT_CONSUMER = """\
module Kappa.Mod where

import Data.IORef

import Engine.Core.State (EngineEnv)
import Engine.Core.Capability.Kappa (KappaCapability(..), toKappaCapability)

sneak ∷ EngineEnv → IO ()
sneak env = writeIORef (kaFieldOne (toKappaCapability env)) 1
"""

# The third legal spelling: a one-field record may be a `newtype`.
_NEWTYPE_PROJECTION = """\
module Engine.Core.Capability.Lambda
  ( LambdaCapability(..)
  , toLambdaCapability
  ) where

import Engine.Core.State (EngineEnv, fieldThree)

newtype LambdaCapability = LambdaCapability
  { laFieldThree ∷ Q.Queue Int
  }

toLambdaCapability ∷ EngineEnv → LambdaCapability
toLambdaCapability env = LambdaCapability
  { laFieldThree = fieldThree env
  }
"""

# A capability type with no record block at all, followed by an
# unrelated record that HAS one. Reading the declaration by name means
# the audit must also refuse to borrow the later declaration's braces
# and report `borrowed` as this record's field.
_BLOCKLESS_CAPABILITY = """\
module Engine.Core.Capability.Nu
  ( NuCapability(..)
  , toNuCapability
  ) where

import Engine.Core.State (EngineEnv)

data NuCapability = NuAlpha | NuBeta

toNuCapability ∷ EngineEnv → NuCapability
toNuCapability env = NuAlpha

data Unrelated = Unrelated
  { borrowed ∷ Int
  }
"""

# A SUM of record constructors. Every constructor's selectors live in
# ONE scope, so `omFieldTwo` is as reachable as `omFieldOne` -- reading
# only the first constructor's block left it unenumerated, and then the
# completeness gate had nothing to say about however it was bound.
_SUM_PROJECTION = """\
module Engine.Core.Capability.Omega
  ( OmegaCapability(..)
  , toOmegaCapability
  ) where

import Engine.Core.State (EngineEnv, fieldOne, fieldTwo)

data OmegaCapability
  = OmegaFirst { omFieldOne ∷ IORef Int }
  | OmegaSecond { omFieldOne ∷ IORef Int
                , omFieldTwo ∷ IORef Text }

toOmegaCapability ∷ EngineEnv → OmegaCapability
toOmegaCapability env = OmegaSecond
  { omFieldOne = fieldOne env
  , omFieldTwo = fieldTwo env
  }
"""

# The same declaration, with the second constructor's field bound
# through a `where`-bound helper. The accessor map cannot see through
# that either, so this is the shape in which the unenumerated field
# went completely untracked: no binding, no map entry, no violation.
_SUM_HIDDEN_PROJECTION = _SUM_PROJECTION.replace(
    "  , omFieldTwo = fieldTwo env\n  }\n",
    "  , omFieldTwo = hidden\n  }\n  where hidden = fieldTwo env\n")

_SUM_CONSUMER = """\
module Omega.Mod where

import Data.IORef

import Engine.Core.State (EngineEnv)
import Engine.Core.Capability.Omega (OmegaCapability(..), toOmegaCapability)

sneak ∷ EngineEnv → IO ()
sneak env = writeIORef (omFieldTwo (toOmegaCapability env)) 1
"""

# A GADT declaring one record constructor per line -- the same sum,
# spelled the other legal way.
_GADT_SUM_PROJECTION = """\
module Engine.Core.Capability.Psi
  ( PsiCapability(..)
  , toPsiCapability
  ) where

import Engine.Core.State (EngineEnv, fieldOne)

data PsiCapability where
  PsiA ∷ { psFieldOne ∷ IORef Int } → PsiCapability
  PsiB ∷ { psFieldTwo ∷ IORef Text } → PsiCapability

toPsiCapability ∷ EngineEnv → PsiCapability
toPsiCapability env = PsiA
  { psFieldOne = fieldOne env
  }
"""

# A module whose body is uniformly indented. Legal Haskell -- the
# layout column is set by the first token after `where`, and nothing
# requires it to be zero -- and every top-level declaration then sits
# at that column. The trailing unrelated record is the trap: the
# declaration span must stop at the next declaration in the SAME
# column, not run to the end of an all-indented file.
_INDENTED_MODULE = """\
module Engine.Core.Capability.Rho
  ( RhoCapability(..)
  , toRhoCapability
  ) where

  import Engine.Core.State (EngineEnv, fieldOne)

  data RhoCapability = RhoCapability
    { rhFieldOne ∷ IORef Int
    }

  toRhoCapability ∷ EngineEnv → RhoCapability
  toRhoCapability env = RhoCapability
    { rhFieldOne = fieldOne env
    }

  data Unrelated = Unrelated
    { borrowed ∷ Int
    }
"""

_INDENTED_CONSUMER = """\
module Rho.Mod where

import Data.IORef

import Engine.Core.State (EngineEnv)
import Engine.Core.Capability.Rho (RhoCapability(..), toRhoCapability)

sneak ∷ EngineEnv → IO ()
sneak env = writeIORef (rhFieldOne (toRhoCapability env)) 1
"""

# A declaration form this audit deliberately does not model. The
# backstop must still see that a capability record was declared, so
# the record fails loudly instead of vanishing.
_UNMODELLED_DECLARATION = """\
module Engine.Core.Capability.Sigma
  ( SigmaCapability(..)
  ) where

import Engine.Core.State (EngineEnv, fieldOne)

data instance Envelope SigmaCapability = SigmaCapability
  { sgFieldOne ∷ IORef Int
  }
"""

# The backstop's false-positive trap: a field whose TYPE is a
# capability record is not a DECLARATION of one, and neither is a
# GADT constructor's record field.
_CAPABILITY_TYPED_FIELDS = """\
module Engine.Core.Capability.Tau
  ( TauCapability(..)
  , toTauCapability
  ) where

import Engine.Core.State (EngineEnv, fieldOne)

data TauCapability = TauCapability
  { tuFieldOne ∷ IORef Int
  }

data Context = Context
  { ctxRender ∷ RenderCapability
  , ctxInput  ∷ InputCapability
  }

data Envelope where
  Envelope ∷ { evRender ∷ RenderCapability } → Envelope

toTauCapability ∷ EngineEnv → TauCapability
toTauCapability env = TauCapability
  { tuFieldOne = fieldOne env
  }
"""

# The migrated reader: it CONSUMES the wrapped handle inline, exactly as
# a `readIORef` consumer does, so it must not be counted as a pass-on.
_WRAPPED_READER = """\
module WrappedReader.Mod where

import Engine.Core.ReadOnlyRef (readReadOnlyRef)

import Engine.Core.State (EngineEnv)
import Engine.Core.Capability.DeltaView
  (DeltaViewCapability(..), toDeltaViewCapability)

peek ∷ EngineEnv → IO Int
peek env = readReadOnlyRef (dvFieldOne (toDeltaViewCapability env))
"""

# The pass-on this whole arc exists to catch: the wrapped handle is
# stored in a context record instead of being read here.
_WRAPPED_PASS_ON = """\
module WrappedPassOn.Mod where

import Engine.Core.State (EngineEnv)
import Engine.Core.Capability.DeltaView
  (DeltaViewCapability(..), toDeltaViewCapability)

observer ∷ EngineEnv → Observer
observer env = Observer { obField = dvFieldOne (toDeltaViewCapability env) }
"""

# `readReadOnlyRef` is held to the same scope rule every primitive is:
# a module-local one of that name is a different function.
_LOCAL_READONLY_PRIMITIVE = """\
module LocalReadOnly.Mod where

import Engine.Core.State (EngineEnv)
import Engine.Core.Capability.DeltaView
  (DeltaViewCapability(..), toDeltaViewCapability)

readReadOnlyRef ∷ α → IO Int
readReadOnlyRef _ = pure 0

peek ∷ EngineEnv → IO Int
peek env = readReadOnlyRef (dvFieldOne (toDeltaViewCapability env))
"""

_GAMMA_CONSUMER = """\
module Gamma.Mod where

import Data.IORef

import Engine.Core.State (EngineEnv)
import Engine.Core.Capability.Gamma (GammaCapability(..), toGammaCapability)

bump ∷ EngineEnv → IO ()
bump env = writeIORef (gmFieldThree (toGammaCapability env)) 1
"""

_ALPHA_CONSUMER = """\
module CollideA.Mod where

import Data.IORef

import Engine.Core.State (EngineEnv)
import qualified Engine.Core.Capability.Alpha as A

bump ∷ EngineEnv → IO ()
bump env = writeIORef (A.sharedRef (A.toAlphaCapability env)) 1
"""

_BETA_CONSUMER = """\
module CollideB.Mod where

import Data.IORef

import Engine.Core.State (EngineEnv)
import qualified Engine.Core.Capability.Beta as B

bump ∷ EngineEnv → IO ()
bump env = writeIORef (B.sharedRef (B.toBetaCapability env)) 2
"""

#: The paths this owner's own fixtures occupy in the synthetic tree.
_PATHS = {
    "alpha": "src/Engine/Core/Capability/Alpha.hs",
    "beta": "src/Engine/Core/Capability/Beta.hs",
    "gamma": "src/Engine/Core/Capability/Gamma.hs",
    "deltaView": "src/Engine/Core/Capability/DeltaView.hs",
    "epsilon": "src/Engine/Core/Capability/Epsilon.hs",
    "zetaView": "src/Engine/Core/Capability/ZetaView.hs",
    "eta": "src/Engine/Core/Capability/Eta.hs",
    "theta": "src/Engine/Core/Capability/Theta.hs",
    "iota": "src/Engine/Core/Capability/Iota.hs",
    "kappa": "src/Engine/Core/Capability/Kappa.hs",
    "lambda": "src/Engine/Core/Capability/Lambda.hs",
    "nu": "src/Engine/Core/Capability/Nu.hs",
    "omega": "src/Engine/Core/Capability/Omega.hs",
    "psi": "src/Engine/Core/Capability/Psi.hs",
    "rho": "src/Engine/Core/Capability/Rho.hs",
    "sigma": "src/Engine/Core/Capability/Sigma.hs",
    "tau": "src/Engine/Core/Capability/Tau.hs",
    "grouped": "src/Grouped/Mod.hs",
    "gammaConsumer": "src/Gamma/Mod.hs",
    "kappaConsumer": "src/Kappa/Mod.hs",
    "omegaConsumer": "src/Omega/Mod.hs",
    "rhoConsumer": "src/Rho/Mod.hs",
    "collideA": "src/CollideA/Mod.hs",
    "collideB": "src/CollideB/Mod.hs",
    "wrappedReader": "src/WrappedReader/Mod.hs",
    "wrappedPassOn": "src/WrappedPassOn/Mod.hs",
    "localReadOnly": "src/LocalReadOnly/Mod.hs",
}


def _writer_sources(**modules: str) -> dict[str, str]:
    """This owner's synthetic tree, over its own paths."""
    return writer_sources(_PATHS, modules)


# ----- This owner's cases ----------------------------------------------

def test_a_projection_may_name_its_accessor_qualified():
    """A capability module may import `Engine.Core.State` under an
    alias and project `gmFieldThree = State.fieldThree env`. If that
    spelling is not parsed, the record's accessors never enter the map
    and every write through them is classified as somebody else's."""
    sources = _writer_sources(gamma=_QUALIFIED_PROJECTION,
                              gammaConsumer=_GAMMA_CONSUMER)
    accessors = capability_accessor_map(sources, _WRITER_FIELDS)
    expect(accessors.get("gmFieldThree") == (
        ("fieldThree", "Engine.Core.Capability.Gamma", "GammaCapability"),),
           f"the qualified projection must canonicalize to the bare "
           f"field, got: {accessors.get('gmFieldThree')}")

    writes, _ = _scan(sources)
    expect(writes["fieldThree"] == {"Gamma.Mod"},
           f"and the write through it must be attributed, got: "
           f"{sorted(writes['fieldThree'])}")


def test_a_view_field_wrapped_by_a_named_alias_wrapper_canonicalizes():
    """SS2.1's abstract-wrapper extension (issue #1896). A reader-facing
    view projects `dvFieldOne = toReadOnlyRef (fieldOne env)` -- the same
    live handle, denied a write by its type. If that spelling is not
    parsed, the accessor never enters the map, and then EVERY use of it
    is invisible: the write scan cannot attribute one, and the pass-on
    residue CMA-3 weighs silently loses the context-record sites the
    wrapper was introduced to protect.

    The wrapper set is CLOSED, so an unrecognized function around the
    accessor does not canonicalize -- `snapshotOf` might copy, and
    inventing an alias for it would claim a guarantee nothing gives."""
    sources = _writer_sources(deltaView=_WRAPPED_PROJECTION)
    accessors = capability_accessor_map(sources, _WRITER_FIELDS)
    expect(accessors.get("dvFieldOne") == (
        ("fieldOne", "Engine.Core.Capability.DeltaView",
         "DeltaViewCapability"),),
           f"the wrapped projection must canonicalize to the bare field, "
           f"got: {accessors.get('dvFieldOne')}")
    expect("dvFieldTwo" not in accessors,
           f"but an unrecognized wrapper must NOT be treated as an alias, "
           f"got: {accessors.get('dvFieldTwo')}")


def test_a_redundantly_grouped_projection_canonicalizes_and_is_enforced():
    """Issue #2059's requirement 1, bare form. `(fieldOne env)` and
    `(fieldTwo) env` are the ungrouped bindings with semantically inert
    parentheses, so they must canonicalize identically -- and a direct
    write through the selector must fail the writing-module map exactly
    as the ungrouped spelling does.

    Before the fix both bindings were unreadable: the accessor map
    omitted both selectors, `writeIORef (epFieldOne ...)` resolved to no
    field and was recorded as `other`, and `audit_writer_modules` had
    nothing to reject while `audit_mutation_sites` saw nothing
    unclassifiable. The gate exited 0 on an unenforced write."""
    sources = _writer_sources(epsilon=_GROUPED_PROJECTION,
                              grouped=_GROUPED_CONSUMER)
    accessors = capability_accessor_map(sources, _WRITER_FIELDS)
    expect(accessors.get("epFieldOne") == (
        ("fieldOne", "Engine.Core.Capability.Epsilon",
         "EpsilonCapability"),),
           f"a binding grouped as `(accessor env)` must canonicalize to "
           f"the bare field, got: {accessors.get('epFieldOne')}")
    expect(accessors.get("epFieldTwo") == (
        ("fieldTwo", "Engine.Core.Capability.Epsilon",
         "EpsilonCapability"),),
           f"and so must one grouped as `(accessor) env`, got: "
           f"{accessors.get('epFieldTwo')}")

    expect(canonical_projection_accessor("((fieldOne env))") == "fieldOne",
           "nested inert grouping must canonicalize too")
    expect(canonical_projection_accessor("State.fieldOne env") == "fieldOne",
           "a qualified accessor must still report bare")

    violations = audit_capability_projection_completeness(
        sources, _WRITER_FIELDS)
    expect(violations == [],
           f"a fully readable grouped projection must raise no "
           f"completeness violation, got: {violations}")

    writes, _ = _scan(sources)
    expect(writes["fieldOne"] == {"Grouped.Mod"},
           f"the write through the grouped selector must be attributed, "
           f"got: {sorted(writes['fieldOne'])}")

    declared = {"fieldOne": frozenset(), "fieldTwo": frozenset(),
                "fieldThree": frozenset()}
    rejected = audit_writer_modules(writes, _WRITER_FIELDS, declared=declared)
    expect(len(rejected) == 1 and "Grouped.Mod" in rejected[0],
           f"and the undeclared write through it must be rejected, got: "
           f"{rejected}")


def test_a_redundantly_grouped_wrapped_projection_canonicalizes():
    """Requirement 1's other half. The wrapped form
    (`wrapper (accessor env)`) is read by its own path, so its grouping
    freedom needs its own case: parentheses INSIDE the wrapper's
    argument and parentheses AROUND the whole application both leave
    the same live handle, and both must reach the same field."""
    sources = _writer_sources(zetaView=_GROUPED_WRAPPED_PROJECTION)
    accessors = capability_accessor_map(sources, _WRITER_FIELDS)
    expect(accessors.get("ztFieldOne") == (
        ("fieldOne", "Engine.Core.Capability.ZetaView",
         "ZetaViewCapability"),),
           f"`toReadOnlyRef ((accessor env))` must canonicalize, got: "
           f"{accessors.get('ztFieldOne')}")
    expect(accessors.get("ztFieldTwo") == (
        ("fieldTwo", "Engine.Core.Capability.ZetaView",
         "ZetaViewCapability"),),
           f"and so must `(toReadOnlyRef (accessor env))`, got: "
           f"{accessors.get('ztFieldTwo')}")

    violations = audit_capability_projection_completeness(
        sources, _WRITER_FIELDS)
    expect(violations == [],
           f"a fully readable grouped WRAPPED projection must raise no "
           f"completeness violation, got: {violations}")


def test_an_unreadable_projection_binding_fails_closed():
    """Requirement 2, and the reason requirement 1 is not enough on its
    own: widening the canonicalizer can never be finished, so the
    spellings it does NOT read must fail loudly instead of vanishing.

    An unrecognized wrapper might copy and an operator expression might
    be anything, so neither canonicalizes -- and both are named, with
    their module, projection and field, rather than leaving the
    selector quietly out of the accessor map."""
    sources = _writer_sources(eta=_UNREADABLE_PROJECTION)

    accessors = capability_accessor_map(sources, _WRITER_FIELDS)
    expect("etFieldOne" in accessors
           and "etFieldTwo" not in accessors
           and "etFieldThree" not in accessors,
           f"the map must still refuse to INVENT an alias for an "
           f"unreadable binding, got: {sorted(accessors)}")

    violations = audit_capability_projection_completeness(
        sources, _WRITER_FIELDS)
    expect(len(violations) == 2,
           f"exactly the two unreadable bindings must be reported, got: "
           f"{violations}")
    for field in ("etFieldTwo", "etFieldThree"):
        expect(any(field in v
                   and "Engine.Core.Capability.Eta" in v
                   and "toEtaCapability" in v
                   for v in violations),
               f"the violation for `{field}` must name the capability "
               f"module, projection and field, got: {violations}")
    expect(not any("etFieldOne" in v for v in violations),
           f"the readable binding beside them must not be reported, got: "
           f"{violations}")

    # The refusal itself, pinned directly. Dropping unrecognized
    # characters instead of refusing on them would leave the two
    # bindings above unreadable by accident -- their operators happen
    # to sit beside a THIRD identifier -- while quietly canonicalizing
    # a two-identifier operator expression that shares no handle at
    # all. Each of these applies SOMETHING to `env` that is not the
    # accessor, and none may reach a field.
    for expression in ("pickRef <$> env",
                       "toReadOnlyRef $ fieldOne env",
                       "fieldOne <$> pure env",
                       "either fieldOne fieldTwo env",
                       "fieldOne @Int env",
                       "(fieldOne env",
                       "fieldOne env (",
                       "fieldOne env)"):
        expect(canonical_projection_accessor(expression) is None,
               f"`{expression}` names no accessor this audit can read, so "
               f"it must not canonicalize; got: "
               f"{canonical_projection_accessor(expression)}")


def test_a_capability_record_with_no_discoverable_projection_fails_closed():
    """The same hole one level up. A record whose projection the audit
    cannot find loses EVERY selector at once, which is strictly worse
    than one unreadable field -- so an undiscoverable projection is a
    violation naming the record, never a module quietly skipped."""
    sources = _writer_sources(theta=_UNPROJECTED_CAPABILITY)
    records = {entry.record: entry.projection
               for entry in discover_capability_records(sources)}
    expect(records.get("ThetaCapability", "missing") is None,
           f"the record must be discovered WITHOUT a projection rather "
           f"than not discovered at all, got: {records}")

    violations = audit_capability_projection_completeness(
        sources, _WRITER_FIELDS)
    expect(len(violations) == 1
           and "ThetaCapability" in violations[0]
           and "Engine.Core.Capability.Theta" in violations[0],
           f"an undiscoverable projection must be reported by record and "
           f"module, got: {violations}")


def test_a_projection_binding_onto_a_dead_accessor_fails_closed():
    """The reviewer's amendment to requirement 2.
    `capability_accessor_map` drops a parsed binding whose accessor is
    not a live `EngineEnv` field, at exactly the same cost as an
    unreadable one -- so a renamed or mistyped accessor must fail here
    too, naming the accessor it could not find."""
    sources = _writer_sources(iota=_DEAD_ACCESSOR_PROJECTION)
    expect("ioFieldRenamed" not in capability_accessor_map(
               sources, _WRITER_FIELDS),
           "the map must not carry a selector bound from a dead accessor")

    violations = audit_capability_projection_completeness(
        sources, _WRITER_FIELDS)
    expect(len(violations) == 1
           and "ioFieldRenamed" in violations[0]
           and "fieldRenamed" in violations[0],
           f"a binding onto a name that is not a live EngineEnv field "
           f"must be reported by field and accessor, got: {violations}")


def test_projection_binding_expressions_keep_the_unreadable_ones():
    """The two parsers are deliberately different. The accessor map
    reads only what canonicalizes, but the completeness gate must see
    every binding the construction WRITES -- otherwise an unreadable
    one would be indistinguishable from an absent one and could not be
    quoted back in the failure message."""
    expressions = parse_projection_binding_expressions(
        _UNREADABLE_PROJECTION, "toEtaCapability")
    expect(expressions == {"etFieldOne": "fieldOne env",
                           "etFieldTwo": "snapshotOf (fieldTwo env)",
                           "etFieldThree": "chooseRef . pick $ env"},
           f"every binding must be returned verbatim, readable or not, "
           f"got: {expressions}")
    expect(parse_projection_binding_expressions(
               _UNREADABLE_PROJECTION, "toNothingCapability") == {},
           "and a projection with no equation returns nothing at all")


def test_projection_completeness_against_the_real_repo():
    """Requirement 3: the live tree passes. Every capability record has
    a discoverable projection, and every field it declares canonicalizes
    onto a live `EngineEnv` accessor -- so the gate added here is a
    ratchet on the real code, not a rule only fixtures satisfy."""
    sources = scan_production_sources(REPO_ROOT)
    live_fields = extract_record_fields(
        (REPO_ROOT / ENGINE_ENV_FILE).read_text(encoding="utf-8"),
        ENGINE_ENV_PATTERN)
    records = discover_capability_records(sources)
    expect(len(records) >= 14,
           f"every capability module must contribute a record, got: "
           f"{len(records)}")
    expect([entry.module for entry in records
            if entry.projection is None] == [],
           "every live capability record must have a discoverable "
           "projection")
    violations = audit_capability_projection_completeness(
        sources, live_fields)
    expect(violations == [],
           f"the real repository must raise no capability projection "
           f"completeness violation, got: {violations}")


def test_a_capability_record_is_found_whatever_syntax_declares_it():
    """A capability type is recognized by its NAME and its
    `data`/`newtype` keyword, never by the shape of its body.

    GHC2024 enables `GADTs`, so
    `data X where X ∷ { ... } → X` declares the very same record --
    same selectors, same scope -- as `data X = X { ... }`, and a
    one-field record may be a `newtype`. Matching only the ordinary
    form left both records undiscovered, which is the same silent
    omission #2059 closes but one level up and strictly worse: the
    record reached neither the accessor map nor the completeness gate,
    so a direct write through its selector was filed as `other` and the
    audit exited 0 with nothing to report."""
    sources = _writer_sources(kappa=_GADT_PROJECTION,
                              kappaConsumer=_GADT_CONSUMER)
    expect(capability_record_fields(_GADT_PROJECTION, "KappaCapability")
           == ["kaFieldOne", "kaFieldTwo"],
           f"a GADT record's selectors must be enumerated, got: "
           f"{capability_record_fields(_GADT_PROJECTION, 'KappaCapability')}")

    records = {entry.record: entry.projection
               for entry in discover_capability_records(sources)}
    expect(records.get("KappaCapability") == "toKappaCapability",
           f"the GADT record and its projection must be discovered, got: "
           f"{records}")

    accessors = capability_accessor_map(sources, _WRITER_FIELDS)
    expect(accessors.get("kaFieldOne") == (
        ("fieldOne", "Engine.Core.Capability.Kappa", "KappaCapability"),)
           and accessors.get("kaFieldTwo") == (
        ("fieldTwo", "Engine.Core.Capability.Kappa", "KappaCapability"),),
           f"both GADT selectors must canonicalize, got: "
           f"{accessors.get('kaFieldOne')}, {accessors.get('kaFieldTwo')}")
    expect(audit_capability_projection_completeness(
               sources, _WRITER_FIELDS) == [],
           "a fully readable GADT projection must raise no completeness "
           "violation")

    writes, _ = _scan(sources)
    expect(writes["fieldOne"] == {"Kappa.Mod"},
           f"and the write through the GADT selector must be attributed, "
           f"got: {sorted(writes['fieldOne'])}")
    declared = {"fieldOne": frozenset(), "fieldTwo": frozenset(),
                "fieldThree": frozenset()}
    rejected = audit_writer_modules(writes, _WRITER_FIELDS, declared=declared)
    expect(len(rejected) == 1 and "Kappa.Mod" in rejected[0],
           f"and the undeclared write must be rejected, got: {rejected}")

    newtype_sources = _writer_sources(**{"lambda": _NEWTYPE_PROJECTION})
    expect(capability_accessor_map(newtype_sources, _WRITER_FIELDS).get(
               "laFieldThree") == (
        ("fieldThree", "Engine.Core.Capability.Lambda",
         "LambdaCapability"),),
           "a `newtype` capability record must canonicalize the same way")
    expect(audit_capability_projection_completeness(
               newtype_sources, _WRITER_FIELDS) == [],
           "and raise no completeness violation")


def test_an_indented_capability_module_is_fully_enforced():
    """A module's layout column is set by the first token after
    `where` and need not be zero, so every top-level declaration of a
    uniformly indented module sits at a non-zero column. Anchoring
    discovery at column zero made such a module invisible end to end --
    no record, no accessor map entry, no completeness violation, and a
    consumer's `writeIORef` through its selector filed as `other`.

    The declaration SPAN has to follow the same column, or the fix
    trades one silent failure for a false one: measured from column
    zero, the record's span would run to the end of an all-indented
    file and report the next declaration's fields as its own."""
    sources = _writer_sources(rho=_INDENTED_MODULE,
                              rhoConsumer=_INDENTED_CONSUMER)
    records = {entry.record: entry.projection
               for entry in discover_capability_records(sources)}
    expect(records.get("RhoCapability") == "toRhoCapability",
           f"an indented declaration and its indented projection must "
           f"both be found, got: {records}")
    expect(capability_record_fields(_INDENTED_MODULE, "RhoCapability")
           == ["rhFieldOne"],
           f"and the span must stop at the next declaration in the same "
           f"column, got: "
           f"{capability_record_fields(_INDENTED_MODULE, 'RhoCapability')}")
    expect(capability_accessor_map(sources, _WRITER_FIELDS).get(
               "rhFieldOne") == (
        ("fieldOne", "Engine.Core.Capability.Rho", "RhoCapability"),),
           "the indented projection must canonicalize")
    expect(audit_capability_projection_completeness(
               sources, _WRITER_FIELDS) == [],
           "and raise no completeness violation")

    writes, _ = _scan(sources)
    expect(writes["fieldOne"] == {"Rho.Mod"},
           f"and the write through its selector must be attributed, got: "
           f"{sorted(writes['fieldOne'])}")
    declared = {"fieldOne": frozenset(), "fieldTwo": frozenset(),
                "fieldThree": frozenset()}
    rejected = audit_writer_modules(writes, _WRITER_FIELDS, declared=declared)
    expect(len(rejected) == 1 and "Rho.Mod" in rejected[0],
           f"and the undeclared write must be rejected, got: {rejected}")


def test_an_unmodelled_capability_declaration_fails_closed():
    """The backstop, and the reason this discovery is a CLOSED set
    rather than a list of spellings someone happened to think of.

    Every hole closed here had one shape: a legal declaration the
    pattern did not match, so the record reached neither the accessor
    map nor the completeness gate and a write through its selector was
    filed as `other` while the audit exited 0. Naming the
    `data`/`newtype` keyword and a `<Name>Capability` type is enough to
    know a capability record is THERE; whether this audit can read its
    fields is a separate question, and the honest answer to "no" is to
    fail. So the NEXT unmodelled spelling -- whatever it is -- stops the
    gate instead of quietly disarming it."""
    sources = _writer_sources(sigma=_UNMODELLED_DECLARATION)
    missed = undiscovered_capability_declarations(sources)
    expect([record for _, _, record in missed] == ["SigmaCapability"],
           f"a declaration the pattern cannot read must still be seen, "
           f"got: {missed}")
    violations = audit_capability_projection_completeness(
        sources, _WRITER_FIELDS)
    expect(len(violations) == 1
           and "SigmaCapability" in violations[0]
           and "cannot read" in violations[0],
           f"and must be reported by module and record, got: {violations}")

    # The named forms SS2.1 does not describe, each reported rather
    # than modelled: this is the "detect and fail" half, and it is what
    # lets the strict pattern stay small without leaving a hole.
    header = "module Engine.Core.Capability.Sigma where\n\n"
    body = "  { sgFieldOne ∷ IORef Int\n  }\n"
    for head in ("data instance SigmaCapability Int = SigmaCapability\n",
                 "newtype instance SigmaCapability Int = SigmaCapability\n",
                 "data instance Envelope SigmaCapability = SigmaCapability\n"):
        reported = audit_capability_projection_completeness(
            _writer_sources(sigma=header + head + body), _WRITER_FIELDS)
        expect(len(reported) == 1 and "SigmaCapability" in reported[0],
               f"`{head.strip()}` must be reported, got: {reported}")
    family = audit_capability_projection_completeness(
        _writer_sources(
            sigma=header + "data family SigmaCapability ∷ Type → Type\n"),
        _WRITER_FIELDS)
    expect(len(family) == 1 and "SigmaCapability" in family[0],
           f"and so must a `data family` naming one, got: {family}")


def test_a_capability_typed_field_is_not_a_declaration():
    """The backstop's own false-positive trap. Naming a capability
    RECORD as a field's type -- a context record holding
    `RenderCapability`, which is exactly D-7's shipped pass-on shape --
    declares nothing, and reporting it would make the gate cry wolf on
    the very pattern the residue exists to measure."""
    sources = _writer_sources(tau=_CAPABILITY_TYPED_FIELDS)
    expect(undiscovered_capability_declarations(sources) == [],
           f"a capability-typed FIELD must not read as a declaration, "
           f"got: {undiscovered_capability_declarations(sources)}")
    expect(audit_capability_projection_completeness(
               sources, _WRITER_FIELDS) == [],
           "and must raise no violation")


def test_the_real_repo_declares_no_unreadable_capability_record():
    """The backstop against the live tree: every capability record it
    declares is one this audit actually reads, so the ratchet is on the
    real code rather than only on fixtures."""
    expect(undiscovered_capability_declarations(
               scan_production_sources(REPO_ROOT)) == [],
           "the real repository must declare no capability record this "
           "audit cannot read")


def test_every_record_constructor_s_selectors_are_enumerated():
    """A capability type may declare more than one record constructor,
    and every constructor's selectors live in ONE scope -- so reading
    only the first block left the rest unenumerated and therefore
    unchecked.

    That is #2059's own failure mode one level up: the completeness
    gate had nothing to say about a field it never knew existed, so a
    projection binding it through anything the canonicalizer cannot
    read took the selector out of the accessor map silently, and an
    undeclared write through it produced no violation at all. Both
    directions are pinned here: the field must be ENFORCED when its
    binding is readable, and must FAIL LOUDLY when it is not."""
    expect(capability_record_fields(_SUM_PROJECTION, "OmegaCapability")
           == ["omFieldOne", "omFieldTwo"],
           f"every constructor's selectors must be enumerated, once each "
           f"in first-declaration order, got: "
           f"{capability_record_fields(_SUM_PROJECTION, 'OmegaCapability')}")
    expect(capability_record_fields(_GADT_SUM_PROJECTION, "PsiCapability")
           == ["psFieldOne", "psFieldTwo"],
           f"and the same for a GADT declaring one record constructor "
           f"per line, got: "
           f"{capability_record_fields(_GADT_SUM_PROJECTION, 'PsiCapability')}")

    # Readable binding: the later constructor's selector is enforced.
    readable = _writer_sources(omega=_SUM_PROJECTION,
                               omegaConsumer=_SUM_CONSUMER)
    expect(capability_accessor_map(readable, _WRITER_FIELDS).get(
               "omFieldTwo") == (
        ("fieldTwo", "Engine.Core.Capability.Omega", "OmegaCapability"),),
           "a later constructor's selector must canonicalize")
    expect(audit_capability_projection_completeness(
               readable, _WRITER_FIELDS) == [],
           "and raise no completeness violation when its binding reads")
    writes, _ = _scan(readable)
    expect(writes["fieldTwo"] == {"Omega.Mod"},
           f"and the write through it must be attributed, got: "
           f"{sorted(writes['fieldTwo'])}")
    declared = {"fieldOne": frozenset(), "fieldTwo": frozenset(),
                "fieldThree": frozenset()}
    rejected = audit_writer_modules(writes, _WRITER_FIELDS, declared=declared)
    expect(len(rejected) == 1 and "Omega.Mod" in rejected[0],
           f"and the undeclared write must be rejected, got: {rejected}")

    # Unreadable binding: the selector leaves the map, so the gate must
    # be the thing that stops -- otherwise the write below is untracked.
    hidden = _writer_sources(omega=_SUM_HIDDEN_PROJECTION,
                             omegaConsumer=_SUM_CONSUMER)
    expect("omFieldTwo" not in capability_accessor_map(
               hidden, _WRITER_FIELDS),
           "a binding through a `where`-bound helper must not be guessed "
           "at")
    hidden_writes, _ = _scan(hidden)
    expect(hidden_writes["fieldTwo"] == set(),
           f"so the write through it is genuinely unattributed, got: "
           f"{sorted(hidden_writes['fieldTwo'])} -- which is exactly why "
           f"the completeness gate must fail")
    violations = audit_capability_projection_completeness(
        hidden, _WRITER_FIELDS)
    expect(len(violations) == 1 and "omFieldTwo" in violations[0],
           f"and the completeness gate must report `omFieldTwo` by name, "
           f"got: {violations}")

    # The unprojected constructor's field is reported the same way.
    gadt_violations = audit_capability_projection_completeness(
        _writer_sources(psi=_GADT_SUM_PROJECTION), _WRITER_FIELDS)
    expect(len(gadt_violations) == 1 and "psFieldTwo" in gadt_violations[0],
           f"a selector no binding covers must be reported by name, got: "
           f"{gadt_violations}")


def test_a_capability_type_with_no_record_block_fails_closed():
    """Recognizing a declaration by name is separated from reading its
    fields, so a `<Name>Capability` whose declaration carries no record
    block is a violation rather than a skip -- and the audit must not
    borrow the braces of a LATER declaration and report ITS field as
    this record's."""
    sources = _writer_sources(nu=_BLOCKLESS_CAPABILITY)
    violations = audit_capability_projection_completeness(
        sources, _WRITER_FIELDS)
    expect(len(violations) == 1
           and "NuCapability" in violations[0]
           and "record block" in violations[0],
           f"a capability type with no readable record block must be "
           f"reported, got: {violations}")
    expect(not any("borrowed" in v for v in violations),
           f"and the unrelated record's field must not be read as its "
           f"own, got: {violations}")


def test_a_read_only_ref_read_is_an_inline_use_not_a_pass_on():
    """`readReadOnlyRef` consumes the handle exactly as `readIORef`
    does, so a migrated reader is an inline use. Without that, every
    reader moved onto a wrapped view would be recounted as a pass-on and
    the residue would inflate by the size of the migration -- reporting
    the OPPOSITE of what the migration did."""
    sources = _writer_sources(deltaView=_WRAPPED_PROJECTION,
                              wrappedReader=_WRAPPED_READER)
    _, residue = _scan(sources)
    expect([r for r in residue if r.module == "WrappedReader.Mod"] == [],
           f"an inline read of a wrapped field is not residue, got: "
           f"{[r for r in residue if r.module == 'WrappedReader.Mod']}")

    # ...and the pass-on it is contrasted with still IS residue, or the
    # rule above would have been achieved by simply going blind.
    sources = _writer_sources(deltaView=_WRAPPED_PROJECTION,
                              wrappedPassOn=_WRAPPED_PASS_ON)
    _, residue = _scan(sources)
    passed = [(r.accessor, r.field) for r in residue
              if r.module == "WrappedPassOn.Mod"]
    expect(passed == [("dvFieldOne", "fieldOne")],
           f"storing a wrapped handle in a context record must stay "
           f"residue, got: {passed}")

    # The primitive is held to the scope rule too: a module-local
    # `readReadOnlyRef` is a different function, so the accessor beside
    # it was not consumed here and stays residue.
    sources = _writer_sources(deltaView=_WRAPPED_PROJECTION,
                              localReadOnly=_LOCAL_READONLY_PRIMITIVE)
    _, residue = _scan(sources)
    expect([r.accessor for r in residue if r.module == "LocalReadOnly.Mod"]
           == ["dvFieldOne"],
           f"a module-local `readReadOnlyRef` is not the primitive, got: "
           f"{[r.accessor for r in residue if r.module == 'LocalReadOnly.Mod']}")

    expect(resolve_primitive(
        parse_imports("import Engine.Core.ReadOnlyRef (readReadOnlyRef)\n"),
        "readReadOnlyRef") == "readReadOnlyRef",
           "the read-only read resolves through its own defining module")
    expect(resolve_primitive(parse_imports("import Data.IORef\n"),
                             "readReadOnlyRef") is None,
           "and `Data.IORef` does not put it in scope")


def test_one_selector_may_belong_to_two_capabilities():
    """A selector name is only unique within its own record. Two
    capability modules may both export `sharedRef`, and the consumer's
    own imports say which one it means -- so every candidate owner is
    offered the scope test rather than one arbitrarily winning and the
    write being dropped as somebody else's."""
    sources = _writer_sources(alpha=_ALPHA_CAPABILITY,
                              beta=_BETA_CAPABILITY,
                              collideA=_ALPHA_CONSUMER,
                              collideB=_BETA_CONSUMER)
    accessors = capability_accessor_map(sources, _WRITER_FIELDS)
    expect(accessors["sharedRef"] == (
        ("fieldOne", "Engine.Core.Capability.Alpha", "AlphaCapability"),
        ("fieldTwo", "Engine.Core.Capability.Beta", "BetaCapability"),
    ), f"both owners must survive, sorted, got: "
       f"{accessors.get('sharedRef')}")

    # One consumer per record, so neither candidate order can be right
    # by luck: each write must land on the field of the capability that
    # consumer actually imported.
    writes, _ = _scan(sources)
    expect(writes["fieldOne"] == {"CollideA.Mod"},
           f"the `Alpha` consumer writes `Alpha`'s field, got: "
           f"{sorted(writes['fieldOne'])}")
    expect(writes["fieldTwo"] == {"CollideB.Mod"},
           f"and the `Beta` consumer writes `Beta`'s, got: "
           f"{sorted(writes['fieldTwo'])}")


#: This owner's inventory of capability-record and projection discovery, in the relative order
#: these groups hold within the façade's run sequence.
#: `tools/test_engine_env_capability_writers.py` composes that
#: sequence from every owner's inventory; nothing here decides when,
#: or whether, it runs.
TESTS = (
    test_a_projection_may_name_its_accessor_qualified,
    test_a_view_field_wrapped_by_a_named_alias_wrapper_canonicalizes,
    test_a_redundantly_grouped_projection_canonicalizes_and_is_enforced,
    test_a_redundantly_grouped_wrapped_projection_canonicalizes,
    test_an_unreadable_projection_binding_fails_closed,
    test_a_capability_record_with_no_discoverable_projection_fails_closed,
    test_a_projection_binding_onto_a_dead_accessor_fails_closed,
    test_projection_binding_expressions_keep_the_unreadable_ones,
    test_projection_completeness_against_the_real_repo,
    test_a_capability_record_is_found_whatever_syntax_declares_it,
    test_an_indented_capability_module_is_fully_enforced,
    test_an_unmodelled_capability_declaration_fails_closed,
    test_a_capability_typed_field_is_not_a_declaration,
    test_the_real_repo_declares_no_unreadable_capability_record,
    test_every_record_constructor_s_selectors_are_enumerated,
    test_a_capability_type_with_no_record_block_fails_closed,
    test_a_read_only_ref_read_is_an_inline_use_not_a_pass_on,
    test_one_selector_may_belong_to_two_capabilities,
)
