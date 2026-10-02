{-# LANGUAGE Strict #-}
-- | The tolerant reading machinery behind "Scenario.Decode" (#2699).
--
--   A scenario document that PARSED as YAML is never rejected wholesale
--   for malformed content (D-16): every field is read on its own, and a
--   bad one becomes a 'ScenarioDiagnostic' naming its document path, the
--   reason, and what the rejection cost. This module owns that per-field
--   discipline; "Scenario.Decode" owns the v1 vocabulary built on it.
module Scenario.Decode.Monad
    ( -- * The decoder
      Dec
    , DecEnv(..)
    , Node(..)
    , NodeKind(..)
    , runDec
    , askEnv
    , diag
    , emitNode
    , isFinalPass
    , isRejectedPath
      -- * Paths
    , keyPath
    , indexPath
      -- * Objects
    , asObject
    , checkKeys
      -- * Fields
    , Parser
    , optField
    , optNullable
    , reqField
    , listField
    , mapField
      -- * Scalar parsers
    , pInt
    , pIntIn
    , pFloat
    , pFloatIn
    , pFloatMin
    , pText
    , pNonEmptyText
    , pBool
    , pOneOf
      -- * Identity and tags
    , explicitIdOk
    , readId
    , rawExplicitId
    , readTags
    ) where

import UPrelude
import qualified Data.Text as T
import qualified Data.HashSet as HS
import Data.Foldable (toList)
import Data.List (sortOn)
import qualified Data.Aeson as A
import qualified Data.Aeson.Key as K
import qualified Data.Aeson.KeyMap as KM
import Control.Monad.State.Strict (State, runState)
import Gameplay.Tags.Types (GameplayTag, mkGameplayTag)
import Scenario.Bounds (TileBounds)
import Scenario.Types

-- * The decoder

-- | What an entry contributes to cross-entry analysis: identity,
--   ownership and references (see "Scenario.Decode").
data NodeKind
    = NodeTerrain | NodeFluid | NodeFlora | NodeStructure | NodeBuilding
    | NodeLocation | NodeUnit | NodeItem
    deriving (Show, Eq)

data Node = Node
    { nPath     ∷ !Text
    , nKind     ∷ !NodeKind
    , nId       ∷ !(Maybe Text)          -- ^ valid explicit id, if any
    , nOwner    ∷ !(Maybe Text)          -- ^ owning entry's path
    , nAlive    ∷ !Bool                  -- ^ survived its own local checks
    , nRequired ∷ ![(Text, Text, NodeKind)]
      -- ^ required references: field path, target id, expected kind
    , nOptional ∷ ![(Text, Text, NodeKind)]
      -- ^ optional references (bindings), same shape
    } deriving (Show, Eq)

data DecEnv = DecEnv
    { envCatalog ∷ !ScenarioCatalog
    , envBounds  ∷ !(Maybe TileBounds)
    , envFinal   ∷ !Bool
      -- ^ the second, building pass: diagnostics are not re-emitted and
      --   the analysis results below are applied
    , envRejected ∷ !(HS.HashSet Text)
      -- ^ entry paths rejected by cross-entry analysis
    , envDropped  ∷ !(HS.HashSet Text)
      -- ^ optional-reference field paths dropped by analysis
    }

data Acc = Acc ![ScenarioDiagnostic] ![Node]

-- | The decoder: an environment plus an accumulator of diagnostics and
--   nodes, both kept newest-first and reversed by 'runDec'.
type Dec = ReaderT DecEnv (State Acc)

runDec ∷ DecEnv → Dec α → (α, [ScenarioDiagnostic], [Node])
runDec env m =
    let (a, Acc ds ns) = runState (runReaderT m env) (Acc [] [])
    in (a, reverse ds, reverse ns)

askEnv ∷ Dec DecEnv
askEnv = ask

isFinalPass ∷ Dec Bool
isFinalPass = envFinal <$> askEnv

isRejectedPath ∷ Text → Dec Bool
isRejectedPath p = HS.member p ∘ envRejected <$> askEnv

-- | Record a diagnostic. The building pass re-reads the same document
--   and would only repeat the first pass's findings, so it records none.
diag ∷ Text → DiagnosticReason → DiagnosticEffect → Dec ()
diag p r e = do
    final ← isFinalPass
    unless final $
        modify (\(Acc ds ns) → Acc (ScenarioDiagnostic p r e : ds) ns)

emitNode ∷ Node → Dec ()
emitNode n = do
    final ← isFinalPass
    unless final $ modify (\(Acc ds ns) → Acc ds (n : ns))

-- * Paths

keyPath ∷ Text → Text → Text
keyPath p k
    | T.null p  = k
    | otherwise = p <> "." <> k

indexPath ∷ Text → Int → Text
indexPath p i = p <> "[" <> tshow i <> "]"

-- * Objects

asObject ∷ A.Value → Maybe A.Object
asObject (A.Object o) = Just o
asObject _            = Nothing

-- | Reject (field-level) every key outside the known vocabulary.
checkKeys ∷ Text → [Text] → A.Object → Dec ()
checkKeys p known o =
    forM_ (KM.keys o) $ \k →
        let kt = K.toText k
        in unless (kt `elem` known) $ diag (keyPath p kt) UnknownField FieldRejected

-- * Fields

-- | A scalar parser: 'Left' carries what was expected.
type Parser α = A.Value → Either Text α

lookupKey ∷ Text → A.Object → Maybe A.Value
lookupKey k = KM.lookup (K.fromText k)

-- | An optional field: omitted → 'Omitted'; malformed → a field-level
--   rejection and 'Omitted' (the field falls back to its default).
optField ∷ Text → Text → Parser α → A.Object → Dec (Authored α)
optField p k parse o = case lookupKey k o of
    Nothing → pure Omitted
    Just v  → case parse v of
        Right x  → pure (Authored x)
        Left why → do
            diag (keyPath p k) (InvalidValue why) FieldRejected
            pure Omitted

-- | 'optField' where an explicit YAML @null@ is a value of its own: the
--   runtime's honest absence, distinct from omission.
optNullable ∷ Text → Text → Parser α → A.Object → Dec (Authored (Maybe α))
optNullable p k parse = optField p k parse'
  where
    parse' A.Null = Right Nothing
    parse' v      = Just <$> parse v

-- | A required field: omitted or malformed rejects the entry.
reqField ∷ Text → Text → Parser α → A.Object → Dec (Maybe α)
reqField p k parse o = case lookupKey k o of
    Nothing → do
        diag (keyPath p k) MissingRequired EntryRejected
        pure Nothing
    Just v → case parse v of
        Right x  → pure (Just x)
        Left why → do
            diag (keyPath p k) (InvalidValue why) EntryRejected
            pure Nothing

-- | An optional list field read element by element. Omitted →
--   'Omitted'; not a list → a field-level rejection and 'Omitted'; each
--   element is handed to @elemDec@ with its own path.
listField ∷ Text → Text → A.Object → (Text → A.Value → Dec (Maybe α))
          → Dec (Authored [α])
listField p k o elemDec = case lookupKey k o of
    Nothing → pure Omitted
    Just (A.Array xs) → do
        let fp = keyPath p k
        ys ← forM (zip [0 ..] (toList xs)) $ \(i, v) → elemDec (indexPath fp i) v
        pure (Authored (catMaybes ys))
    Just _ → do
        diag (keyPath p k) (InvalidValue "a list") FieldRejected
        pure Omitted

-- | An optional mapping field read entry by entry, keys in sorted order
--   so the result never depends on the YAML library's key order.
mapField ∷ Text → Text → A.Object → (Text → Text → A.Value → Dec (Maybe β))
         → Dec (Authored [(Text, β)])
mapField p k o entryDec = case lookupKey k o of
    Nothing → pure Omitted
    Just (A.Object m) → do
        let fp = keyPath p k
            entries = sortOn fst [ (K.toText key, v) | (key, v) ← KM.toList m ]
        ys ← forM entries $ \(key, v) →
            fmap (\b → (key, b)) <$> entryDec (keyPath fp key) key v
        pure (Authored (catMaybes ys))
    Just _ → do
        diag (keyPath p k) (InvalidValue "a mapping") FieldRejected
        pure Omitted

-- * Scalar parsers

pInt ∷ Parser Int
pInt v@(A.Number _) = case A.fromJSON v of
    A.Success n → Right n
    A.Error _   → Left "an integer"
pInt _ = Left "an integer"

pIntIn ∷ Int → Int → Parser Int
pIntIn lo hi v = do
    n ← pInt v
    if n < lo ∨ n > hi
        then Left ("an integer in [" <> tshow lo <> ", " <> tshow hi <> "]")
        else Right n

-- | A finite number. YAML's @.nan@/@.inf@, and values that overflow
--   'Float', are not.
pFloat ∷ Parser Float
pFloat v@(A.Number _) = case A.fromJSON v of
    A.Success (d ∷ Double) →
        let f = realToFrac d ∷ Float
        in if isNaN f ∨ isInfinite f then Left "a finite number" else Right f
    A.Error _ → Left "a finite number"
pFloat _ = Left "a finite number"

pFloatIn ∷ Float → Float → Parser Float
pFloatIn lo hi v = do
    f ← pFloat v
    if f < lo ∨ f > hi
        then Left ("a number in [" <> tshow lo <> ", " <> tshow hi <> "]")
        else Right f

pFloatMin ∷ Float → Parser Float
pFloatMin lo v = do
    f ← pFloat v
    if f < lo then Left ("a number ≥ " <> tshow lo) else Right f

pText ∷ Parser Text
pText (A.String t) = Right t
pText _            = Left "a string"

pNonEmptyText ∷ Parser Text
pNonEmptyText v = do
    t ← pText v
    if T.null t then Left "a non-empty string" else Right t

pBool ∷ Parser Bool
pBool (A.Bool b) = Right b
pBool _          = Left "true or false"

pOneOf ∷ [(Text, α)] → Parser α
pOneOf table v = do
    t ← pText v
    case lookup t table of
        Just x  → Right x
        Nothing → Left ("one of " <> T.intercalate ", " (map fst table))

-- * Identity and tags

-- | An explicit id is non-empty and drawn from letters, digits and
--   @_ - : .@ — never @[@, so it cannot collide with an automatic
--   (path-shaped) id.
explicitIdOk ∷ Text → Bool
explicitIdOk t = not (T.null t) ∧ T.all ok t
  where ok c = (c ≥ 'a' ∧ c ≤ 'z') ∨ (c ≥ 'A' ∧ c ≤ 'Z')
             ∨ (c ≥ '0' ∧ c ≤ '9') ∨ c `elem` ("_-:." ∷ String)

-- | The entry's identity: its valid explicit @id@, else the automatic
--   path id. A malformed @id@ is a field-level rejection.
readId ∷ Text → A.Object → Dec (ScenarioId, Maybe Text)
readId p o = do
    r ← optField p "id" parseId o
    pure $ case r of
        Authored t → (ExplicitId t, Just t)
        Omitted    → (AutoId p, Nothing)
  where
    parseId v = do
        t ← pText v
        if explicitIdOk t
            then Right t
            else Left "an id of letters, digits, '_', '-', ':' or '.'"

-- | The valid explicit id of a raw entry value, if it has one — for
--   entries rejected before their own fields are read, whose identity
--   must still count as declared.
rawExplicitId ∷ A.Value → Maybe Text
rawExplicitId v = case asObject v of
    Just o | Just (A.String t) ← KM.lookup "id" o, explicitIdOk t → Just t
    _ → Nothing

-- | Optional gameplay tags: any non-empty string is a tag
--   ('Gameplay.Tags.Types.mkGameplayTag'); an empty one is dropped with
--   a warning and the entry kept. No case, spelling or uniqueness rule.
readTags ∷ Text → A.Object → Dec [GameplayTag]
readTags p o = do
    r ← listField p "tags" o $ \ip v → case v of
        A.String t → case mkGameplayTag t of
            Just tag → pure (Just tag)
            Nothing  → do
                diag ip EmptyTag FieldRejected
                pure Nothing
        _ → do
            diag ip (InvalidValue "a string") FieldRejected
            pure Nothing
    pure (authoredOr [] r)
