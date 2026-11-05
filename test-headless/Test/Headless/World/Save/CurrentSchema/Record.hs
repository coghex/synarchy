-- | Turns compiled carrier evidence into the canonical current-schema
--   record (issue #2916). Pure: the live registry, the metadata and
--   framing values, the Lua registry and the compiled fixtures all hand
--   this module the SAME input shape, so the real extraction and the
--   fixture catalogue cannot diverge in how a record is built.
module Test.Headless.World.Save.CurrentSchema.Record
    ( CarrierIn(..)
    , RouteIn(..)
    , buildRecord
    , renderNode
    , wireProblems
    ) where

import UPrelude
import Data.Aeson (Value(..), object, toJSON, (.=))
import qualified Data.Aeson.Key as K
import qualified Data.List as L
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import World.Save.Wire

-- | One persisted carrier: a Haskell component, the metadata component,
--   the framing, a Lua module, or a retired legacy carrier.
data CarrierIn = CarrierIn
    { ciId       ∷ Text
    , ciClass    ∷ Text
    , ciVersion  ∷ Word32
    , ciAccepted ∷ [Word32]
      -- ^ the versions the REAL decode entry point accepts, probed
    , ciRequired ∷ Bool
    , ciRoot     ∷ Maybe WireRoot
    , ciOwners   ∷ [Text]
    , ciDetail   ∷ Value
    }

-- | A retired legacy route and the carriers it decodes through.
data RouteIn = RouteIn
    { riId      ∷ Text
    , riOwners  ∷ [Text]
    , riEnabled ∷ Value
    }

keyOf ∷ Text → K.Key
keyOf = K.fromText

jsonObj ∷ [(Text, Value)] → Value
jsonObj kvs = object [ keyOf k .= v | (k, v) ← kvs ]

-- | A node's recorded form. @shape@ is what the comparison treats as the
--   wire shape; @declared@ is the human description of a hand-written
--   encoder, which is recorded but never authoritative.
renderNode ∷ WireNode → Value
renderNode n = jsonObj
    [ ("shape", jsonObj
        [ ("kind", String kindName)
        , ("detail", String detail)
        , ("dataType", String (wnDataType n))
        , ("newtype", Bool (wnNewtype n))
        , ("constructors", toJSON [ renderCon c | c ← wnCons n ])
        , ("args", toJSON (wnArgs n)) ])
    , ("declared", declared) ]
  where
    (kindName, detail, declared) = case wnKind n of
        WKGeneric      → ("generic", "", Null)
        WKPrimitive t  → ("primitive", t, Null)
        WKLibrary t    → ("library", t, Null)
        WKNewtype      → ("newtype", "", Null)
        WKCustom d     → ( "custom", cdOwner d
                         , jsonObj [ ("contract", String (cdContract d))
                                   , ("test", String (cdTest d))
                                   , ("sampled", Bool (not (null (wnChecks n)))) ] )
    renderCon c = jsonObj
        [ ("name", String (wcName c))
        , ("record", Bool (wcRecord c))
        , ("fields", toJSON [ jsonObj [ ("selector", String (wfSelector f))
                                      , ("type", String (wfType f)) ]
                            | f ← wcFields c ]) ]

-- | Problems the walk itself can report: a generic node whose real
--   encoding disagrees with cereal's generic encoding (an undeclared
--   hand-written encoder), a failed round trip, and a field type the
--   walk did not reach.
wireProblems ∷ WireRoot → [Text]
wireProblems root = concat
    [ [ "unclassified encoder: " <> wnKey n <> " is walked as " <> what (wnKind n)
        <> " but its real Serialize encoding differs from "
        <> against (wnKind n) <> " on " <> wkSample c
        <> " (declare it in World.Save.Wire.Custom)"
      | n ← nodes, checksEncoding (wnKind n), c ← wnChecks n
      , wkActual c ≢ wkGeneric c ]
    , [ "round trip failed: " <> wnKey n <> " on " <> wkSample c
      | n ← nodes, c ← wnChecks n, not (wkRoundTrip c) ]
    , [ "unchecked boundary: " <> wnKey n <> " is classified "
        <> what (wnKind n) <> " (or declared custom) but no sample value could "
        <> "be built for it, so its encoding was never compared"
      | n ← nodes, needsEvidence (wnKind n), null (wnChecks n) ]
    , [ "unreached type: " <> wnKey n <> " names " <> e <> ", which the walk did not reach"
      | n ← nodes, e ← nodeEdges n, not (Map.member e (wrNodes root)) ]
    ]
  where
    nodes = Map.elems (wrNodes root)
    checksEncoding k = case k of
        WKGeneric → True
        WKNewtype → True
        _         → False
    needsEvidence k = case k of
        WKGeneric  → True
        WKNewtype  → True
        WKCustom _ → True
        _          → False
    what k = case k of WKNewtype → "a transparent newtype"; _ → "generic"
    against k = case k of
        WKNewtype → "its field's encoding"
        _         → "cereal's generic encoding"

-- | Build the record, or every reason it cannot be built.
buildRecord ∷ [CarrierIn] → [RouteIn] → Value → Either [Text] Value
buildRecord carriers routes extras
    | not (null problems) = Left problems
    | otherwise = Right $ jsonObj
        [ ("format", Number 1)
        , ("extractor", String "current-save-schema/1")
        , ("types", jsonObj [ (k, renderNode n) | (k, n) ← Map.toList types ])
        , ("carriers", jsonObj [ (ciId c, renderCarrier c) | c ← carriers ])
        , ("routes", jsonObj [ (riId r, jsonObj [ ("owners", toJSON (L.sort (riOwners r)))
                                               , ("enabled", riEnabled r) ])
                             | r ← routes ])
        , ("extras", extras) ]
  where
    ids = map ciId carriers
    types = Map.unions [ wrNodes r | Just r ← map ciRoot carriers ]
    problems = concat
        [ [ "duplicate carrier coverage: " <> i
          | (i, n) ← Map.toList (Map.fromListWith (+) [ (x, 1 ∷ Int) | x ← ids ]), n > 1 ]
        , [ "ambiguous type: " <> k <> " renders differently under two carriers"
          | (k, vs) ← Map.toList (Map.fromListWith (⧺)
                [ (k', [renderNode n]) | Just r ← map ciRoot carriers
                , (k', n) ← Map.toList (wrNodes r) ])
          , length (L.nub vs) > 1 ]
        , [ "route " <> riId r <> " names unknown carrier " <> o
          | r ← routes, o ← riOwners r, o `notElem` ids ]
        , [ "carrier " <> ciId c <> ": current version " <> tshow (ciVersion c)
            <> " is not in its accepted versions " <> tshow (ciAccepted c)
          | c ← carriers, ciClass c ≢ "legacy", ciVersion c `notElem` ciAccepted c ]
        , concat [ map ((ciId c <> ": ") <>) (wireProblems r)
                 | c ← carriers, Just r ← [ciRoot c] ]
        ]
    renderCarrier c = jsonObj
        [ ("class", String (ciClass c))
        , ("version", toJSON (ciVersion c))
        , ("accepted", toJSON (L.sort (ciAccepted c)))
        , ("required", Bool (ciRequired c))
        , ("root", maybe Null (String . wrKey) (ciRoot c))
        , ("reaches", toJSON (maybe [] (Set.toList . Map.keysSet . wrNodes) (ciRoot c)))
        , ("owners", toJSON (ciOwners c))
        , ("detail", ciDetail c) ]
