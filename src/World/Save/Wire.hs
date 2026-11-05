{-# LANGUAGE UndecidableInstances, ExistentialQuantification #-}
-- | Compiled wire-shape evidence for the current-schema save guard
--   (issue #2916, PSP-2 of #2914).
--
--   The save registry erases each component's wire DTO type
--   ('World.Save.Component.Types.RegisteredComponent' keeps only ids and
--   versions), but 'World.Save.Component.Types.componentCodec' still
--   knows the type through @ComponentSpec d a@. It therefore demands a
--   'WireSchema' instance for @d@ and keeps the resulting 'WireRoot' on
--   the codec, so the guard reads shapes from the SAME type the codec
--   encodes, never from a parallel list of names.
--
--   === What the walk is, and is not
--
--   'WireSchema' walks GHC's own 'Generic' and 'Typeable' representations:
--   constructors in declaration order, record selectors, newtypes, and
--   every instantiated field type transitively, with type synonyms
--   already resolved by GHC. It describes REPRESENTATION, not arbitrary
--   'S.Serialize' behaviour, so every reachable type lands in exactly one
--   class of boundary and an unlisted boundary is a compile error or a
--   failed behavioural check, never a silent blessing:
--
--   * 'WKGeneric' -- the default 'Generic'-driven instance. A 'Generic'
--     instance cannot tell whether the type's 'S.Serialize' instance uses
--     cereal's generic defaults, so 'wnChecks' pairs the REAL
--     'S.encode' of distinguishing sample values with cereal's own
--     'GSerializePut' encoding of the same value. A hand-written encoder
--     that disagrees fails the check unless it is declared below. The
--     check is evidence on samples, not a proof.
--   * 'WKPrimitive' / 'WKLibrary' -- cereal's base-type and container
--     encoders, declared by an explicit instance here.
--   * 'WKCustom' -- a hand-written encoder, declared by an explicit
--     instance in "World.Save.Wire.Custom" with an owner, a hand-written
--     description of the bytes, and the hspec example that exercises it.
--     Those contracts are DECLARED and TESTED, not mechanically derived.
--
--   Nothing here is on the save/load path: samples and checks are lazy
--   thunks that only the guard's extractor ever forces.
module World.Save.Wire
    ( WireSchema(..)
    , WireNode(..)
    , WireKind(..)
    , WireCon(..)
    , WireField(..)
    , WireCheck(..)
    , Child(..)
    , CustomDecl(..)
    , WireRoot(..)
    , wireRoot
    , emptyWireRoot
    , typeKey
    , nodeEdges
      -- * Building nodes
    , primitiveNode
    , libraryNode
    , customNode
    , customNodeOpaque
    , newtypeNode
    , newtypeSamples
      -- * Generic machinery (for instances elsewhere)
    , GShape(..)
    , GFields(..)
    , GSample(..)
    ) where

import UPrelude
import qualified Data.ByteString as BS
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import qualified Data.IntMap.Strict as IM
import qualified Data.List as L
import qualified Data.Map.Strict as Map
import qualified Data.Sequence as Seq
import qualified Data.Serialize as S
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.Vector as V
import Data.Char (isSpace)
import Data.Hashable (Hashable)
import Data.Int (Int8, Int16, Int32, Int64)
import Data.Kind (Type)
import Data.Proxy (Proxy(..))
import Data.Typeable
    (Typeable, TypeRep, typeRep, splitTyConApp, tyConModule, tyConName)
import GHC.Generics

-- | How one type's bytes are produced (see the module header).
data WireKind
    = WKGeneric
    | WKPrimitive !Text
    | WKLibrary !Text
    | WKNewtype
      -- ^ a newtype whose 'S.Serialize' instance is newtype-derived: its
      --   bytes are its field's. 'wnChecks' compares the real encoding
      --   with the field's on sample values.
    | WKCustom !CustomDecl
    deriving (Show, Eq)

-- | A hand-written encoder's declaration. The description and the test
--   name are written by a human next to the instance; nothing derives
--   them, which is why the record states them as declared.
data CustomDecl = CustomDecl
    { cdOwner    ∷ !Text
      -- ^ The module that owns the hand-written 'S.Serialize' instance.
    , cdContract ∷ !Text
      -- ^ The bytes the instance writes, in words.
    , cdTest     ∷ !Text
      -- ^ The @current save schema@ hspec example that exercises it.
    } deriving (Show, Eq)

data WireField = WireField
    { wfSelector ∷ !Text   -- ^ empty for a positional field
    , wfType     ∷ !Text   -- ^ the field type's 'typeKey'
    } deriving (Show, Eq)

data WireCon = WireCon
    { wcName   ∷ !Text
    , wcRecord ∷ !Bool
    , wcFields ∷ ![WireField]
    } deriving (Show, Eq)

-- | One behavioural observation: what 'S.encode' really wrote for a
--   sample value, and what cereal's generic encoding of that value is.
data WireCheck = WireCheck
    { wkSample  ∷ !Text
    , wkActual  ∷ !BS.ByteString
    , wkGeneric ∷ !BS.ByteString
    , wkRoundTrip ∷ !Bool
      -- ^ decoding the real bytes and re-encoding reproduces them
    }

-- | An existentially packed field type, so a node can name the types it
--   reaches without the walk knowing them statically.
data Child = ∀ b. WireSchema b ⇒ Child (Proxy b)

-- | One type in a persisted shape.
data WireNode = WireNode
    { wnKey      ∷ !Text
    , wnDataType ∷ !Text
      -- ^ GHC's @module.datatype@ for a 'Generic' type, otherwise empty.
    , wnNewtype  ∷ !Bool
    , wnKind     ∷ !WireKind
    , wnCons     ∷ ![WireCon]
    , wnArgs     ∷ ![Text]
      -- ^ element types of a library container, in order
    , wnChildren ∷ ![Child]
    , wnChecks   ∷ [WireCheck]
      -- ^ lazy: only the guard's extractor forces it
    }

nodeEdges ∷ WireNode → [Text]
nodeEdges n = L.nub (concatMap (map wfType . wcFields) (wnCons n) ⧺ wnArgs n)

-- | Everything reachable from one root, keyed by 'typeKey'.
data WireRoot = WireRoot
    { wrKey   ∷ Text
    , wrNodes ∷ Map.Map Text WireNode
    }

-- | A root that reaches nothing, for stub codecs that never describe a
--   real wire type.
emptyWireRoot ∷ WireRoot
emptyWireRoot = WireRoot "" Map.empty

-- | The instantiated, fully module-qualified rendering of a type. GHC has
--   already resolved synonyms, so a parameter permutation of an alias
--   changes the key exactly when it changes the type.
typeKey ∷ ∀ a. Typeable a ⇒ Proxy a → Text
typeKey _ = renderRep (typeRep (Proxy ∷ Proxy a))

renderRep ∷ TypeRep → Text
renderRep r =
    let (tc, args) = splitTyConApp r
        name = T.pack (tyConModule tc) <> "." <> T.pack (tyConName tc)
    in T.unwords (name : map (wrap . renderRep) args)
  where wrap t = if T.any isSpace t then "(" <> t <> ")" else t

-- | Walk every type reachable from the root, once each.
wireRoot ∷ ∀ a. WireSchema a ⇒ Proxy a → WireRoot
wireRoot p = WireRoot (typeKey p) (go Map.empty [Child p])
  where
    go seen [] = seen
    go seen (Child q : rest)
        | Map.member k seen = go seen rest
        | otherwise = go (Map.insert k n seen) (wnChildren n ⧺ rest)
      where n = wireNode q
            k = typeKey q

-- | A type that can appear in a persisted shape.
class Typeable a ⇒ WireSchema a where
    wireNode    ∷ Proxy a → WireNode
    -- | Small, distinguishing sample values, used only by the guard's
    --   behavioural checks. Empty when none can be built.
    wireSamples ∷ Proxy a → [a]
    wireSamples _ = []
    -- | 'wireSamples' under a nesting budget: a generic type yields no
    --   sample once the budget is spent, so a recursive or very deep
    --   type cannot make the walk diverge. Leaves ignore the budget.
    wireSamplesD ∷ Int → Proxy a → [a]
    wireSamplesD _ = wireSamples

-- Generic shape ------------------------------------------------------

class GShape (f ∷ Type → Type) where
    gcons ∷ Proxy f → [WireCon]
    gchildren ∷ Proxy f → [Child]
    -- | @(module.datatype, isNewtype)@, answered by the 'D1' wrapper.
    gmeta ∷ Proxy f → (Text, Bool)
    gmeta _ = ("", False)

instance GShape V1 where
    gcons _ = []
    gchildren _ = []

instance (GShape f, GShape g) ⇒ GShape (f :+: g) where
    gcons _ = gcons (Proxy ∷ Proxy f) ⧺ gcons (Proxy ∷ Proxy g)
    gchildren _ = gchildren (Proxy ∷ Proxy f) ⧺ gchildren (Proxy ∷ Proxy g)

instance (Datatype c, GShape f) ⇒ GShape (D1 c f) where
    gcons _ = gcons (Proxy ∷ Proxy f)
    gchildren _ = gchildren (Proxy ∷ Proxy f)
    gmeta _ = ( T.pack (moduleName d) <> "." <> T.pack (datatypeName d)
              , isNewtype d )
      where d = undefined ∷ D1 c f ()

instance (Constructor c, GFields f) ⇒ GShape (C1 c f) where
    gcons _ = [ WireCon
                  { wcName = T.pack (conName (undefined ∷ C1 c f ()))
                  , wcRecord = conIsRecord (undefined ∷ C1 c f ())
                  , wcFields = gfields (Proxy ∷ Proxy f) } ]
    gchildren _ = gfchildren (Proxy ∷ Proxy f)

class GFields (f ∷ Type → Type) where
    gfields ∷ Proxy f → [WireField]
    gfchildren ∷ Proxy f → [Child]

instance GFields U1 where
    gfields _ = []
    gfchildren _ = []

instance (GFields f, GFields g) ⇒ GFields (f :*: g) where
    gfields _ = gfields (Proxy ∷ Proxy f) ⧺ gfields (Proxy ∷ Proxy g)
    gfchildren _ = gfchildren (Proxy ∷ Proxy f) ⧺ gfchildren (Proxy ∷ Proxy g)

instance (Selector s, WireSchema c) ⇒ GFields (S1 s (K1 i c)) where
    gfields _ = [ WireField (T.pack (selName (undefined ∷ S1 s (K1 i c) ())))
                            (typeKey (Proxy ∷ Proxy c)) ]
    gfchildren _ = [Child (Proxy ∷ Proxy c)]

-- Generic samples ----------------------------------------------------

-- | Sample values of a representation. Variant @k@ picks the @k@-th
--   sample of the first field and the @(k+1)@-th of the next (wrapping),
--   so adjacent equal-typed fields get DIFFERENT values and a swapped
--   write order is visible in the bytes.
class GSample (f ∷ Type → Type) where
    gsample ∷ Int → Int → [f p]

instance GSample V1 where gsample _ _ = []
instance GSample U1 where gsample _ _ = [U1]
instance (GSample f, GSample g) ⇒ GSample (f :+: g) where
    gsample d k = map L1 (gsample d k) ⧺ map R1 (gsample d k)
instance GSample f ⇒ GSample (M1 i c f) where
    gsample d k = map M1 (gsample d k)
instance (GSample f, GSample g) ⇒ GSample (f :*: g) where
    gsample d k = [ a :*: b | a ← take 1 (gsample d k), b ← take 1 (gsample d (k + 1)) ]
instance WireSchema c ⇒ GSample (K1 i c) where
    -- Only the first two samples are ever forced, so the cost stays
    -- linear in depth however many constructors a field type has.
    gsample d k = case take 2 (wireSamplesD (d - 1) (Proxy ∷ Proxy c)) of
        []      → []
        [x]     → [K1 x]
        (x : y : _) → [K1 (if even k then x else y)]

-- | The nesting budget a node's own checks start with.
sampleDepth ∷ Int
sampleDepth = 5

-- The default instance -----------------------------------------------

-- | Every 'Generic' type is walked by default; its 'S.Serialize'
--   instance is then CHECKED against cereal's generic encoding (see the
--   module header). A type whose encoder is hand-written must be
--   declared in "World.Save.Wire.Custom" or it fails that check.
instance {-# OVERLAPPABLE #-}
    ( Generic a, Typeable a, S.Serialize a
    , GShape (Rep a), S.GSerializePut (Rep a), GSample (Rep a) )
    ⇒ WireSchema a where
    wireNode p = (genericNode p)
        { wnKind = WKGeneric
        , wnChecks = [ check ("variant " <> tshow k) x
                     | k ← [0, 1 ∷ Int], x ← variant k ] }
      where
        variant ∷ Int → [a]
        variant k = map to (gsample sampleDepth k ∷ [Rep a ()])
        check lbl x =
            let real = S.encode x
            in WireCheck
                { wkSample = lbl
                , wkActual = real
                , wkGeneric = S.runPut (S.gPut (from x))
                , wkRoundTrip = case S.decode real of
                    Right y → S.encode (y `asTypeOf` x) ≡ real
                    Left _  → False }
    wireSamples = wireSamplesD sampleDepth
    wireSamplesD d _
        | d ≤ 0     = []
        | otherwise = concat [ map to (gsample d k ∷ [Rep a ()]) | k ← [0, 1 ∷ Int] ]

genericNode
    ∷ ∀ a. (Generic a, Typeable a, GShape (Rep a)) ⇒ Proxy a → WireNode
genericNode p = WireNode
    { wnKey = typeKey p
    , wnDataType = fst meta
    , wnNewtype = snd meta
    , wnKind = WKGeneric
    , wnCons = gcons (Proxy ∷ Proxy (Rep a))
    , wnArgs = []
    , wnChildren = gchildren (Proxy ∷ Proxy (Rep a))
    , wnChecks = [] }
  where meta = gmeta (Proxy ∷ Proxy (Rep a))

-- | A type 'S.Serialize' encodes with a base-type encoder.
primitiveNode ∷ ∀ a. Typeable a ⇒ Text → Proxy a → WireNode
primitiveNode what p = bare p (WKPrimitive what) []

-- | A container or library type whose encoder is cereal's, over the
--   given element types.
libraryNode ∷ ∀ a. Typeable a ⇒ Text → [Child] → Proxy a → WireNode
libraryNode what kids p = (bare p (WKLibrary what) kids)
    { wnArgs = [ childKey c | c ← kids ] }

childKey ∷ Child → Text
childKey (Child q) = typeKey q

bare ∷ ∀ a. Typeable a ⇒ Proxy a → WireKind → [Child] → WireNode
bare p kind kids = WireNode
    { wnKey = typeKey p, wnDataType = "", wnNewtype = False, wnKind = kind
    , wnCons = [], wnArgs = [], wnChildren = kids, wnChecks = [] }

-- | A hand-written encoder over a 'Generic' type: the representation is
--   still recorded (a field edit is a recorded change), and the
--   declaration says what the real encoder does with it.
customNode
    ∷ ∀ a. (Generic a, Typeable a, GShape (Rep a), S.Serialize a, WireSchema a)
    ⇒ CustomDecl → Proxy a → WireNode
customNode decl p = (genericNode p)
    { wnKind = WKCustom decl, wnChecks = roundTripChecks p }

-- | Round-trip observations for the declared samples of a hand-written
--   encoder: its own bytes decode and re-encode identically. The
--   "generic" side is the actual bytes again, since a hand-written
--   encoder has no generic encoding to agree with.
roundTripChecks ∷ ∀ a. (S.Serialize a, WireSchema a) ⇒ Proxy a → [WireCheck]
roundTripChecks p =
    [ WireCheck
        { wkSample = "sample " <> tshow i, wkActual = real, wkGeneric = real
        , wkRoundTrip = case S.decode real of
            Right y → S.encode (y `asTypeOf` x) ≡ real
            Left _  → False }
    | (i, x) ← zip [0 ∷ Int ..] (wireSamples p), let real = S.encode x ]

-- | A hand-written encoder over a type with no 'Generic' instance: only
--   the declaration and the types it names.
customNodeOpaque
    ∷ ∀ a. (S.Serialize a, WireSchema a)
    ⇒ CustomDecl → [Child] → Proxy a → WireNode
customNodeOpaque decl kids p = (bare p (WKCustom decl) kids)
    { wnArgs = [ childKey c | c ← kids ], wnChecks = roundTripChecks p }

-- | A newtype with a newtype-derived 'S.Serialize' instance, named by its
--   constructor and (empty for none) field selector, over its field type.
--   Records a one-constructor shape, and checks that the real encoding of
--   a wrapped sample is the field's own encoding.
newtypeNode
    ∷ ∀ a b. (Typeable a, S.Serialize a, WireSchema b, S.Serialize b)
    ⇒ Text → Text → (b → a) → Proxy a → WireNode
newtypeNode con sel wrap p = WireNode
    { wnKey = typeKey p, wnDataType = "", wnNewtype = True, wnKind = WKNewtype
    , wnCons = [WireCon con (not (T.null sel))
                        [WireField sel (typeKey (Proxy ∷ Proxy b))]]
    , wnArgs = [], wnChildren = [Child (Proxy ∷ Proxy b)]
    , wnChecks =
        [ WireCheck
            { wkSample = "newtype sample", wkActual = real
            , wkGeneric = S.encode x
            , wkRoundTrip = case S.decode real of
                Right y → S.encode (y `asTypeOf` wrapped) ≡ real
                Left _  → False }
        | x ← wireSamples (Proxy ∷ Proxy b)
        , let wrapped = wrap x, let real = S.encode wrapped ] }

-- | Sample values of a newtype, from its field's samples.
newtypeSamples ∷ ∀ a b. WireSchema b ⇒ (b → a) → Proxy a → [a]
newtypeSamples wrap _ = map wrap (wireSamples (Proxy ∷ Proxy b))

-- Base types ---------------------------------------------------------

instance {-# OVERLAPPING #-} WireSchema Int where
    wireNode = primitiveNode "cereal Int (8 bytes, big endian)"
    wireSamples _ = [0, 1]
instance {-# OVERLAPPING #-} WireSchema Integer where
    wireNode = primitiveNode "cereal Integer"
    wireSamples _ = [0, 1]
instance {-# OVERLAPPING #-} WireSchema Int8 where
    wireNode = primitiveNode "cereal Int8"
    wireSamples _ = [0, 1]
instance {-# OVERLAPPING #-} WireSchema Int16 where
    wireNode = primitiveNode "cereal Int16 (big endian)"
    wireSamples _ = [0, 1]
instance {-# OVERLAPPING #-} WireSchema Int32 where
    wireNode = primitiveNode "cereal Int32 (big endian)"
    wireSamples _ = [0, 1]
instance {-# OVERLAPPING #-} WireSchema Int64 where
    wireNode = primitiveNode "cereal Int64 (big endian)"
    wireSamples _ = [0, 1]
instance {-# OVERLAPPING #-} WireSchema Word where
    wireNode = primitiveNode "cereal Word (8 bytes, big endian)"
    wireSamples _ = [0, 1]
instance {-# OVERLAPPING #-} WireSchema Word8 where
    wireNode = primitiveNode "cereal Word8"
    wireSamples _ = [0, 1]
instance {-# OVERLAPPING #-} WireSchema Word16 where
    wireNode = primitiveNode "cereal Word16 (big endian)"
    wireSamples _ = [0, 1]
instance {-# OVERLAPPING #-} WireSchema Word32 where
    wireNode = primitiveNode "cereal Word32 (big endian)"
    wireSamples _ = [0, 1]
instance {-# OVERLAPPING #-} WireSchema Word64 where
    wireNode = primitiveNode "cereal Word64 (big endian)"
    wireSamples _ = [0, 1]
instance {-# OVERLAPPING #-} WireSchema Double where
    wireNode = primitiveNode "cereal Double (IEEE 754 bits, big endian)"
    wireSamples _ = [0, 1.5]
instance {-# OVERLAPPING #-} WireSchema Float where
    wireNode = primitiveNode "cereal Float (IEEE 754 bits, big endian)"
    wireSamples _ = [0, 1.5]
instance {-# OVERLAPPING #-} WireSchema Char where
    wireNode = primitiveNode "cereal Char (UTF-8)"
    wireSamples _ = ['a', 'b']
instance {-# OVERLAPPING #-} WireSchema Text where
    wireNode = primitiveNode "UPrelude orphan Serialize Text (UTF-8, length prefixed)"
    wireSamples _ = ["", "ab"]
instance {-# OVERLAPPING #-} WireSchema BS.ByteString where
    wireNode = primitiveNode "cereal ByteString (Int64 length prefix)"
    wireSamples _ = ["", "ab"]

-- Containers ---------------------------------------------------------

instance {-# OVERLAPPING #-} WireSchema a ⇒ WireSchema [a] where
    wireNode = libraryNode "cereal list (Int64 length prefix, then elements)"
                           [Child (Proxy ∷ Proxy a)]
    wireSamples = wireSamplesD sampleDepth
    wireSamplesD d _ = [[], take 2 (wireSamplesD (d - 1) (Proxy ∷ Proxy a))]

instance {-# OVERLAPPING #-} WireSchema a ⇒ WireSchema (V.Vector a) where
    wireNode = libraryNode "World.Hydrology.Types orphan Serialize Vector (as a list)"
                           [Child (Proxy ∷ Proxy a)]
    wireSamples = wireSamplesD sampleDepth
    wireSamplesD d _ = [V.empty, V.fromList (take 2 (wireSamplesD (d - 1) (Proxy ∷ Proxy a)))]

instance {-# OVERLAPPING #-} WireSchema a ⇒ WireSchema (Seq.Seq a) where
    wireNode = libraryNode "cereal Seq (Int64 length prefix, then elements)"
                           [Child (Proxy ∷ Proxy a)]
    wireSamples = wireSamplesD sampleDepth
    wireSamplesD d _ = [Seq.empty, Seq.fromList (take 2 (wireSamplesD (d - 1) (Proxy ∷ Proxy a)))]

instance {-# OVERLAPPING #-} (WireSchema a, Ord a) ⇒ WireSchema (Set.Set a) where
    wireNode = libraryNode "cereal Set (ascending elements)"
                           [Child (Proxy ∷ Proxy a)]
    wireSamples = wireSamplesD sampleDepth
    wireSamplesD d _ = [Set.empty, Set.fromList (take 2 (wireSamplesD (d - 1) (Proxy ∷ Proxy a)))]

instance {-# OVERLAPPING #-} (WireSchema a, Hashable a)
    ⇒ WireSchema (HS.HashSet a) where
    wireNode = libraryNode "cereal HashSet (via list of elements)"
                           [Child (Proxy ∷ Proxy a)]
    wireSamples = wireSamplesD sampleDepth
    wireSamplesD d _ = [HS.empty, HS.fromList (take 2 (wireSamplesD (d - 1) (Proxy ∷ Proxy a)))]

instance {-# OVERLAPPING #-} (WireSchema k, WireSchema v, Ord k)
    ⇒ WireSchema (Map.Map k v) where
    wireNode = libraryNode "cereal Map (ascending key/value pairs)"
                           [Child (Proxy ∷ Proxy k), Child (Proxy ∷ Proxy v)]
    wireSamples = wireSamplesD sampleDepth
    wireSamplesD d _ = [Map.empty, Map.fromList (pairs d (Proxy ∷ Proxy k) (Proxy ∷ Proxy v))]

instance {-# OVERLAPPING #-} WireSchema v ⇒ WireSchema (IM.IntMap v) where
    wireNode = libraryNode "cereal IntMap (ascending key/value pairs)"
                           [Child (Proxy ∷ Proxy v)]
    wireSamples = wireSamplesD sampleDepth
    wireSamplesD d _ = [IM.empty, IM.fromList (zip [0, 1] (wireSamplesD (d - 1) (Proxy ∷ Proxy v)))]

instance {-# OVERLAPPING #-} (WireSchema k, WireSchema v, Hashable k)
    ⇒ WireSchema (HM.HashMap k v) where
    wireNode = libraryNode "UPrelude orphan Serialize HashMap (list of key/value pairs)"
                           [Child (Proxy ∷ Proxy k), Child (Proxy ∷ Proxy v)]
    wireSamples = wireSamplesD sampleDepth
    wireSamplesD d _ = [HM.empty, HM.fromList (pairs d (Proxy ∷ Proxy k) (Proxy ∷ Proxy v))]

pairs ∷ (WireSchema k, WireSchema v) ⇒ Int → Proxy k → Proxy v → [(k, v)]
pairs d pk pv = take 2 (zip (wireSamplesD (d - 1) pk) (wireSamplesD (d - 1) pv))
