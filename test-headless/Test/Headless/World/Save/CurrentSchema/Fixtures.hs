{-# LANGUAGE DeriveAnyClass #-}
{-# OPTIONS_GHC -Wno-orphans #-}
-- | Compiled shape fixtures for the current-schema guard (issue #2916).
--
--   Each fixture is a REAL Haskell type compiled into the test target and
--   walked by the real extractor; nothing here is a hand-entered
--   dictionary. A \"variant\" of a type is a separate compiled type named
--   @Name__Variant@; 'canon' strips the @__Variant@ suffix from every
--   recorded string, so a base and a candidate variant carry the same
--   type key exactly as an edited declaration would.
--
--   The catalogue pairs a base record with a candidate record and the
--   verdict the comparison must reach. The hspec group asserts what the
--   extractor itself can decide (a changed shape changes the record, an
--   undeclared encoder fails extraction); @tools/current_save_schema_audit.py
--   --self-test@ replays the same catalogue through the Python comparator.
module Test.Headless.World.Save.CurrentSchema.Fixtures
    ( Case(..)
    , Verdict(..)
    , catalogue
    , canon
    , FxHandUndeclared__A(..)
    ) where

import UPrelude
import Data.Aeson (Value(..), object, (.=))
import qualified Data.Aeson.Key as K
import qualified Data.Aeson.KeyMap as KM
import qualified Data.HashMap.Strict as HM
import qualified Data.Serialize as S
import qualified Data.Text as T
import qualified Data.Vector as V
import Data.Proxy (Proxy(..))
import GHC.Generics (Generic)

import World.Save.Wire
import Test.Headless.World.Save.CurrentSchema.Record

-- Fixture types -------------------------------------------------------

data FxRec__Base = FxRec__Base { fxA ∷ !Int, fxB ∷ !Text }
    deriving (Show, Generic, S.Serialize)
data FxRec__Added = FxRec__Added { fxA ∷ !Int, fxB ∷ !Text, fxC ∷ !Int }
    deriving (Show, Generic, S.Serialize)
data FxRec__Removed = FxRec__Removed { fxA ∷ !Int }
    deriving (Show, Generic, S.Serialize)
data FxRec__Reordered = FxRec__Reordered { fxB ∷ !Text, fxA ∷ !Int }
    deriving (Show, Generic, S.Serialize)
data FxRec__Retyped = FxRec__Retyped { fxA ∷ !Int, fxB ∷ !Int }
    deriving (Show, Generic, S.Serialize)

data FxSwap__Base = FxSwap__Base { fxX ∷ !Int, fxY ∷ !Int }
    deriving (Show, Generic, S.Serialize)
data FxSwap__Swapped = FxSwap__Swapped { fxY ∷ !Int, fxX ∷ !Int }
    deriving (Show, Generic, S.Serialize)

data FxEnum__Base = FxP__Base | FxQ__Base
    deriving (Show, Generic, S.Serialize)
data FxEnum__Reordered = FxQ__Reordered | FxP__Reordered
    deriving (Show, Generic, S.Serialize)
data FxEnum__Appended = FxP__Appended | FxQ__Appended | FxR__Appended
    deriving (Show, Generic, S.Serialize)

type FxAlias a b = HM.HashMap a b
data FxAliasUse__Base = FxAliasUse__Base { fxM ∷ !(FxAlias Int Text) }
    deriving (Show, Generic, S.Serialize)
data FxAliasUse__Permuted = FxAliasUse__Permuted { fxM ∷ !(FxAlias Text Int) }
    deriving (Show, Generic, S.Serialize)

newtype FxNew__Base = FxNew__Base Int
    deriving stock (Show, Generic)
    deriving anyclass S.Serialize
newtype FxNew__Retyped = FxNew__Retyped Text
    deriving stock (Show, Generic)
    deriving anyclass S.Serialize

data FxNested__Inner = FxNested__Inner { fxN ∷ !Int }
    deriving (Show, Generic, S.Serialize)
data FxNested__Outer = FxNested__Outer { fxIn ∷ ![FxNested__Inner], fxMb ∷ !(Maybe FxNested__Inner) }
    deriving (Show, Generic, S.Serialize)

data FxShared__Base = FxShared__Base { fxS ∷ !Int }
    deriving (Show, Generic, S.Serialize)
data FxShared__Changed = FxShared__Changed { fxS ∷ !Int, fxT ∷ !Int }
    deriving (Show, Generic, S.Serialize)
data FxUserA__Base = FxUserA__Base { fxShared ∷ !FxShared__Base }
    deriving (Show, Generic, S.Serialize)
data FxUserA__Changed = FxUserA__Changed { fxShared ∷ !FxShared__Changed }
    deriving (Show, Generic, S.Serialize)
data FxUserB__Base = FxUserB__Base { fxShared ∷ !FxShared__Base }
    deriving (Show, Generic, S.Serialize)
data FxUserB__Changed = FxUserB__Changed { fxShared ∷ !FxShared__Changed }
    deriving (Show, Generic, S.Serialize)

-- | A hand-written encoder that writes its fields in the opposite order
--   to the generic encoding, with no declaration: extraction must fail.
data FxHandUndeclared__A = FxHandUndeclared__A !Int !Int
    deriving (Show, Generic)
instance S.Serialize FxHandUndeclared__A where
    put (FxHandUndeclared__A a b) = S.put b >> S.put a
    get = do b ← S.get; a ← S.get; pure (FxHandUndeclared__A a b)

-- | Four equal-typed fields whose hand-written serializer swaps the
--   MIDDLE two on both write and read: it round-trips, and only a sample
--   that gives those two fields different values exposes it.
data FxSwapMid__A = FxSwapMid__A !Int !Int !Int !Int
    deriving (Show, Generic)
instance S.Serialize FxSwapMid__A where
    put (FxSwapMid__A a b c d) = S.put a >> S.put c >> S.put b >> S.put d
    get = do a ← S.get; c ← S.get; b ← S.get; d ← S.get
             pure (FxSwapMid__A a b c d)

-- | Seven nested single-field wrappers; the OUTER one writes a prefix
--   byte yet round-trips. Sampling must still reach it however deep the
--   wrapping goes.
data FxD1__A = FxD1__A !Int deriving (Show, Generic, S.Serialize)
data FxD2__A = FxD2__A !FxD1__A deriving (Show, Generic, S.Serialize)
data FxD3__A = FxD3__A !FxD2__A deriving (Show, Generic, S.Serialize)
data FxD4__A = FxD4__A !FxD3__A deriving (Show, Generic, S.Serialize)
data FxD5__A = FxD5__A !FxD4__A deriving (Show, Generic, S.Serialize)
data FxD6__A = FxD6__A !FxD5__A deriving (Show, Generic, S.Serialize)
data FxD7__A = FxD7__A !FxD6__A deriving (Show, Generic)
instance S.Serialize FxD7__A where
    put (FxD7__A x) = S.putWord8 0xAA >> S.put x
    get = S.getWord8 >> (FxD7__A ⊚ S.get)

-- | A type with no constructible value at all.
data FxLoop__A = FxLoop__A !FxLoop__A deriving (Show, Generic, S.Serialize)

-- | A newtype whose encoder ROUND-TRIPS but is not its field's own
--   encoding (a prefix byte): same recorded shape, wrong declaration.
newtype FxNtOpaque__A = FxNtOpaque__A Int
    deriving stock (Show, Generic)
instance S.Serialize FxNtOpaque__A where
    put (FxNtOpaque__A n) = S.putWord8 0xAA >> S.put n
    get = S.getWord8 >> (FxNtOpaque__A ⊚ S.get)
instance WireSchema FxNtOpaque__A where
    wireNode = newtypeNode "FxNtOpaque" "" FxNtOpaque__A
    wireSamples = newtypeSamples FxNtOpaque__A

-- | The same shape of encoder, declared: extraction passes.
data FxHandDeclared__A = FxHandDeclared__A !Int !Int
    deriving (Show, Generic)
instance S.Serialize FxHandDeclared__A where
    put (FxHandDeclared__A a b) = S.put b >> S.put a
    get = do b ← S.get; a ← S.get; pure (FxHandDeclared__A a b)
instance WireSchema FxHandDeclared__A where
    wireNode = customNodeOpaque
        (CustomDecl "fixture" "second field, then first field"
                    "current save schema: fixture layout")
        [Child (Proxy ∷ Proxy Int)]
    wireSamples _ = [FxHandDeclared__A 1 2, FxHandDeclared__A 3 4]

-- Records --------------------------------------------------------------

-- | Strip @__Variant@ from every string, so variants share keys.
canon ∷ Value → Value
canon v = case v of
    String t → String (strip t)
    Array xs → Array (V.map canon xs)
    Object o → Object (KM.fromList
        [ (K.fromText (strip (K.toText k)), canon x) | (k, x) ← KM.toList o ])
    _ → v
  where
    strip = T.pack . go . T.unpack
    go ('_' : '_' : rest) | (name, rest') ← span (`elem` ['A' .. 'Z'] ⧺ ['a' .. 'z'] ⧺ ['0' .. '9']) rest
                          , not (null name) = go rest'
    go (c : rest) = c : go rest
    go [] = []

-- | One carrier over a compiled root type.
car ∷ ∀ d. (S.Serialize d, WireSchema d)
    ⇒ Text → Word32 → [Word32] → Proxy d → CarrierIn
car i v acc p = CarrierIn
    { ciId = i, ciClass = "component", ciVersion = v, ciAccepted = acc
    , ciRequired = True, ciRoot = Just (wireRoot p), ciOwners = []
    , ciDetail = Null }

record' ∷ [CarrierIn] → [RouteIn] → Either [Text] Value
record' cs rs = canon ⊚ buildRecord cs rs Null

data Verdict = Passes | Fails Text
    deriving (Show, Eq)

data Case = Case
    { caseName    ∷ Text
    , caseBase    ∷ Either [Text] Value
    , caseCand    ∷ Either [Text] Value
    , caseVerdict ∷ Verdict
    }

-- | Every fixture case: (name, base, candidate, expected verdict).
catalogue ∷ [Case]
catalogue =
    [ -- shape changes need a strictly higher real version
      c "field added, same version" (rec1 1 [1]) (recA 1 [1]) (Fails "same-version drift")
    , c "field added, bumped, current-only" (rec1 1 [1]) (recA 2 [2]) Passes
    , c "field added, bumped, older version kept" (rec1 1 [1]) (recA 2 [1, 2]) (Fails "still accepts")
    , c "field removed, same version" (rec1 1 [1]) (recR 1 [1]) (Fails "same-version drift")
    , c "field reordered, same version" (rec1 1 [1]) (recO 1 [1]) (Fails "same-version drift")
    , c "field retyped, same version" (rec1 1 [1]) (recT 1 [1]) (Fails "same-version drift")
    , c "equal-typed named-field swap, same version"
        (one "x" 1 [1] (Proxy ∷ Proxy FxSwap__Base))
        (one "x" 1 [1] (Proxy ∷ Proxy FxSwap__Swapped)) (Fails "same-version drift")
    , c "equal-typed named-field swap, bumped"
        (one "x" 1 [1] (Proxy ∷ Proxy FxSwap__Base))
        (one "x" 2 [2] (Proxy ∷ Proxy FxSwap__Swapped)) Passes
    , c "enum reorder, same version"
        (one "e" 1 [1] (Proxy ∷ Proxy FxEnum__Base))
        (one "e" 1 [1] (Proxy ∷ Proxy FxEnum__Reordered)) (Fails "same-version drift")
    , c "enum append, same version"
        (one "e" 1 [1] (Proxy ∷ Proxy FxEnum__Base))
        (one "e" 1 [1] (Proxy ∷ Proxy FxEnum__Appended)) (Fails "same-version drift")
    , c "enum append, bumped"
        (one "e" 1 [1] (Proxy ∷ Proxy FxEnum__Base))
        (one "e" 2 [2] (Proxy ∷ Proxy FxEnum__Appended)) Passes
    , c "alias parameter permutation, same version"
        (one "a" 1 [1] (Proxy ∷ Proxy FxAliasUse__Base))
        (one "a" 1 [1] (Proxy ∷ Proxy FxAliasUse__Permuted)) (Fails "same-version drift")
    , c "newtype payload retyped, same version"
        (one "n" 1 [1] (Proxy ∷ Proxy FxNew__Base))
        (one "n" 1 [1] (Proxy ∷ Proxy FxNew__Retyped)) (Fails "same-version drift")
    , c "nested type reached through list and Maybe is unchanged"
        (one "o" 1 [1] (Proxy ∷ Proxy FxNested__Outer))
        (one "o" 1 [1] (Proxy ∷ Proxy FxNested__Outer)) Passes
      -- a shared type owned by two carriers
    , c "shared type changed, only one owner bumped"
        (sharedBase 1 1 [1] [1]) (sharedChanged 2 1 [2] [1])
        (Fails "same-version drift")
    , c "shared type changed, both owners bumped"
        (sharedBase 1 1 [1] [1]) (sharedChanged 2 2 [2] [2]) Passes
      -- custom encoders
    , Case "undeclared hand-written encoder fails extraction"
        (Right Null) (record' [car "h" 1 [1] (Proxy ∷ Proxy FxHandUndeclared__A)] [])
        (Fails "unclassified encoder")
    , Case "round-tripping non-transparent newtype fails extraction"
        (Right Null) (record' [car "nt" 1 [1] (Proxy ∷ Proxy FxNtOpaque__A)] [])
        (Fails "transparent newtype")
    , Case "deeply wrapped type with a round-tripping prefix serializer fails extraction"
        (Right Null) (record' [car "deep" 1 [1] (Proxy ∷ Proxy FxD7__A)] [])
        (Fails "unclassified encoder")
    , Case "middle-field swapping serializer fails extraction"
        (Right Null) (record' [car "swap" 1 [1] (Proxy ∷ Proxy FxSwapMid__A)] [])
        (Fails "unclassified encoder")
    , Case "type with no constructible sample fails extraction"
        (Right Null) (record' [car "loop" 1 [1] (Proxy ∷ Proxy FxLoop__A)] [])
        (Fails "unchecked boundary")
    , c "declared hand-written encoder extracts"
        (one "h" 1 [1] (Proxy ∷ Proxy FxHandDeclared__A))
        (one "h" 1 [1] (Proxy ∷ Proxy FxHandDeclared__A)) Passes
      -- registrations, retained versions and legacy routes
    , c "new carrier accepting only its current version"
        (rec1 1 [1]) (both' (recCar 1 [1]) (car "new" 1 [1] (Proxy ∷ Proxy FxNew__Base))) Passes
    , c "new carrier accepting an older version"
        (rec1 1 [1]) (both' (recCar 1 [1]) (car "new" 2 [1, 2] (Proxy ∷ Proxy FxNew__Base)))
        (Fails "new carrier")
    , c "carrier removed with its record entry"
        (both' (recCar 1 [1]) (car "new" 1 [1] (Proxy ∷ Proxy FxNew__Base))) (rec1 1 [1]) Passes
    , c "unchanged carrier keeps its older versions"
        (rec1 3 [1, 2, 3]) (rec1 3 [1, 2, 3]) Passes
    , c "removing one older registration alone"
        (rec1 3 [1, 2, 3]) (rec1 3 [2, 3]) Passes
    , c "version decreased"
        (rec1 2 [2]) (rec1 1 [1]) (Fails "version went down")
    , c "bump without a shape change, older version kept"
        (rec1 1 [1]) (rec1 2 [1, 2]) (Fails "still accepts")
    , c "bump without a shape change, current only"
        (rec1 1 [1]) (rec1 2 [2]) Passes
    , c "legacy route stays enabled over an affected owner"
        (withRoute False (recCar 1 [1]) "r")
        (withRoute True (carA 2 [2]) "r") (Fails "legacy route")
    , c "legacy route disabled over an affected owner"
        (withRoute True (recCar 1 [1]) "r")
        (withRoute False (carA 2 [2]) "r") Passes
    , c "legacy route stays enabled over unchanged owners"
        (withRoute True (recCar 1 [1]) "r")
        (withRoute True (recCar 1 [1]) "r") Passes
    ]
  where
    c n b ca v = Case n b ca v
    one ∷ ∀ d. (S.Serialize d, WireSchema d) ⇒ Text → Word32 → [Word32] → Proxy d → Either [Text] Value
    one i v acc p = record' [car i v acc p] []
    rec1 v acc = one "r" v acc (Proxy ∷ Proxy FxRec__Base)
    recA v acc = one "r" v acc (Proxy ∷ Proxy FxRec__Added)
    recR v acc = one "r" v acc (Proxy ∷ Proxy FxRec__Removed)
    recO v acc = one "r" v acc (Proxy ∷ Proxy FxRec__Reordered)
    recT v acc = one "r" v acc (Proxy ∷ Proxy FxRec__Retyped)
    recCar v acc = car "r" v acc (Proxy ∷ Proxy FxRec__Base)
    carA v acc = car "r" v acc (Proxy ∷ Proxy FxRec__Added)
    both' a b = record' [a, b] []
    sharedBase va vb accA accB = record'
        [ car "ua" va accA (Proxy ∷ Proxy FxUserA__Base)
        , car "ub" vb accB (Proxy ∷ Proxy FxUserB__Base) ] []
    sharedChanged va vb accA accB = record'
        [ car "ua" va accA (Proxy ∷ Proxy FxUserA__Changed)
        , car "ub" vb accB (Proxy ∷ Proxy FxUserB__Changed) ] []
    withRoute enabled cr rid =
        record' [cr] [RouteIn rid [ciId cr] (object [ "publicFullSession" .= enabled ])]
