-- | A structural read-only alias of a live 'IORef' (issue #1896, CMA-2
--   of the capability mutation-authority epic #1890; design decision
--   D-7 in @docs\/capability_mutation_authority_design.md@).
--
--   The @EngineEnv@ capability split (#537, #889–#899) narrows __which
--   fields__ a module can name, not __what it may do__ with one it can
--   name: every capability record aliases the same live handles
--   'Engine.Core.State.EngineEnv' holds, so a consumer that legitimately
--   reads a field can also write it. A boundary drawn at the RECORD also
--   ends the moment the 'IORef' is extracted — and 35% of capability
--   accessor uses in this tree pass the raw handle onward, into helper
--   parameters and into context records mixing several capabilities.
--
--   'ReadOnlyRef' is the boundary that TRAVELS with the handle instead.
--   It is a @newtype@ over the very same 'IORef' — no copy, no snapshot,
--   no synchronization of its own — whose constructor this module does
--   not export, so the only thing a holder can do with one is
--   'readReadOnlyRef' it. Passing it into a helper or packing it into a
--   context record carries that restriction along
--   ('Building.Knowledge.Live.ContainerObserver' is the production
--   example).
--
--   __It is a boundary, not a capability, and deliberately forgeable.__
--   'toReadOnlyRef' is public: anyone already holding the raw 'IORef'
--   can wrap it. That is the point — the guarantee is "a module handed
--   only the wrapped form cannot write", which is exactly what the
--   narrowed consumer's capability record delivers by handing it nothing
--   else. Making construction private would buy no authority (the raw
--   handle is what confers it) while blocking the legitimate wrappers:
--   the view projection itself, and test fixtures building a context
--   record by hand.
--
--   __Nothing here is an escape hatch, and nothing may add one.__ There
--   is deliberately no unwrap, no @modify@, no @Internal@ companion
--   module, and no record field — @tools\/test_read_only_ref_compile.py@
--   pins the resulting compile failures against the PUBLIC interface
--   alone.
--
--   The 'Eq' instance is 'IORef''s own pointer equality lifted through
--   the wrapper. It compares two aliases; it reveals no handle and
--   writes nothing, and it is what lets the projection-aliasing hspec
--   coverage assert "the same live container" the way every other
--   capability record's does.
module Engine.Core.ReadOnlyRef
  ( ReadOnlyRef
  , toReadOnlyRef
  , readReadOnlyRef
  ) where

import UPrelude
import Data.IORef (IORef, readIORef)

-- | A read-only alias of a live 'IORef'. The constructor is NOT
--   exported: 'toReadOnlyRef' wraps, 'readReadOnlyRef' reads, and there
--   is no third operation.
--
--   Mutation through a legitimate raw writer handle is immediately
--   visible here — this aliases the caller's container rather than
--   copying it, exactly as a capability projection does.
newtype ReadOnlyRef α = ReadOnlyRef (IORef α)
  deriving (Eq)

-- | Wrap a live handle. The result aliases @ref@; it does not snapshot
--   it.
toReadOnlyRef ∷ IORef α → ReadOnlyRef α
toReadOnlyRef = ReadOnlyRef

-- | Read the current value. The only operation a 'ReadOnlyRef' holder
--   has.
readReadOnlyRef ∷ ReadOnlyRef α → IO α
readReadOnlyRef (ReadOnlyRef ref) = readIORef ref
