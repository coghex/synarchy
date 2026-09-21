{-# LANGUAGE Strict, DeriveGeneric #-}
-- | The @data\/factions\/*.yaml@ authoring boundary (#2506, FTS-2 of the
--   faction-tag arc #2496).
--
--   __Two stages, and the split is load-bearing.__ 'loadFactionYaml'
--   DECODES — shape, types, key spelling — and answers 'Nothing' for a
--   file that is not a faction catalogue at all, which
--   @scripts\/startup_loader.lua@ turns into the terminal parse failure
--   it turns every family's parse failure into (#2203).
--   'factionCatalogueRefusal' then applies the five SEMANTIC rules of
--   requirement 2 against the catalogue registered so far, and a
--   violation refuses the whole file the way a duplicate flora name
--   does (#2241): nothing is registered, and the diagnostic names the
--   offending id.
--
--   __Whole-file, because a catalogue is one document.__ A half-admitted
--   catalogue is a relation table nobody authored: the tags a file
--   declares are what its own relations are checked against, and
--   dropping one bad row would leave the rest silently meaning something
--   else. It is also what makes the registry's accumulate-across-files
--   behaviour safe — a refused file leaves everything already registered
--   exactly as it was.
--
--   __The relation VALUE is decoded as text and judged here__, not by a
--   'FromJSON' instance for 'FactionRelation'. An Aeson failure could
--   only reach for a JSON path like @$.relations[2].relation@ — an index
--   nobody can map back to a row without counting — whereas requirement
--   2 wants the offending id named. The same reasoning is why
--   'Engine.Asset.YamlLootProfiles' parses its own fields by hand.
module Engine.Asset.YamlFactions
    ( -- * The authored shapes
      FactionYamlTag(..)
    , FactionYamlRelation(..)
    , FactionYamlDoc(..)
      -- * Loading
    , loadFactionYaml
    , loadFactionYamlOutcome
      -- * Post-decode validation
    , CatalogueRefusal(..)
    , refusalReason
    , refusalDetail
    , factionCatalogueRefusal
    , admitFactionYamlDoc
    ) where

import UPrelude
import Control.Monad (foldM)
import GHC.Generics (Generic)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.Yaml as Yaml
import Data.Aeson (FromJSON(..), withObject, (.:), (.:?), (.!=))
import qualified Data.Aeson.Types as Aeson (Parser)
import Engine.Core.Log (LoggerState, logDebug, logWarn, LogCategory(..))
import Unit.Faction (FactionRelation(..))
import Unit.Faction.Catalogue
    ( FactionCatalogue, FactionTagDecl(..), catalogueDeclaredPairs
    , declaresTag, extendFactionCatalogue, renderTagPair )
import Unit.Faction.Profile
    (BaseRelationEntry(..), mkFactionTag)

-- * The authored shapes

-- | One @faction_tags:@ entry. Only @id@ is required; @description@ is
--   author-facing documentation the engine never reads.
data FactionYamlTag = FactionYamlTag
    { fytId          ∷ !Text
    , fytDescription ∷ !(Maybe Text)
    } deriving (Show, Eq, Generic)

instance FromJSON FactionYamlTag where
    parseJSON = withObject "FactionYamlTag" $ \v → FactionYamlTag
        ⊚ v .:  "id"
        ⊛ v .:? "description"

-- | One @relations:@ entry, in exactly one of its two forms.
--
--   @pair: [a, b]@ is the SYMMETRIC shorthand — ordinary mutual
--   hostility is the common case and spelling it twice invites a
--   half-declared pair. @from:@ + @to:@ is the directed form, which
--   declares that one direction and leaves the reverse to the rest of
--   the table and the precedence.
--
--   Exactly one form per entry, enforced at DECODE: an entry carrying
--   both, or neither, is not a shape this schema has a meaning for, so
--   it is a parse failure rather than one of requirement 2's semantic
--   refusals (which all name an offending id).
data FactionYamlRelation = FactionYamlRelation
    { fyrFrom     ∷ !Text
    , fyrTo       ∷ !Text
    , fyrSymmetric ∷ !Bool
    , fyrRelation ∷ !Text
      -- ^ kept as the author's own text; judged by
      --   'factionCatalogueRefusal', never by a 'FromJSON' instance.
    } deriving (Show, Eq, Generic)

instance FromJSON FactionYamlRelation where
    parseJSON = withObject "FactionYamlRelation" $ \v → do
        mPair ← v .:? "pair" ∷ Aeson.Parser (Maybe [Text])
        mFrom ← v .:? "from"
        mTo   ← v .:? "to"
        rel   ← v .:  "relation"
        case (mPair, mFrom, mTo) of
            (Just [a, b], Nothing, Nothing) →
                pure (FactionYamlRelation a b True rel)
            (Just _, Nothing, Nothing) →
                fail "a relation's `pair` must name exactly two tags"
            (Nothing, Just f, Just t) →
                pure (FactionYamlRelation f t False rel)
            (Nothing, _, _) →
                fail "a relation needs either `pair: [a, b]` or both \
                     \`from:` and `to:`"
            _ → fail "a relation declares either `pair:` or `from:`/`to:`, \
                     \never both"

-- | One catalogue file. Both blocks are optional: a file may declare
--   only tags, only relations, or (uselessly but legally) neither.
data FactionYamlDoc = FactionYamlDoc
    { fydTags      ∷ ![FactionYamlTag]
    , fydRelations ∷ ![FactionYamlRelation]
    } deriving (Show, Eq, Generic)

instance FromJSON FactionYamlDoc where
    parseJSON = withObject "FactionYamlDoc" $ \v → FactionYamlDoc
        ⊚ v .:? "faction_tags" .!= []
        ⊛ v .:? "relations"    .!= []

-- * Loading

-- | Decode one catalogue file. 'Nothing' is a DECODE failure and
--   nothing else; every semantic rule lives in
--   'factionCatalogueRefusal'.
loadFactionYamlOutcome ∷ LoggerState → FilePath → IO (Maybe FactionYamlDoc)
loadFactionYamlOutcome logger path = do
    result ← Yaml.decodeFileEither path
    case result of
        Left err → do
            logWarn logger CatAsset $ "Failed to parse faction YAML "
                <> T.pack path <> ": " <> tshow err
            return Nothing
        Right doc → do
            logDebug logger CatAsset $ "Parsed faction catalogue "
                <> T.pack path <> ": " <> tshow (length (fydTags doc))
                <> " tag(s), " <> tshow (length (fydRelations doc))
                <> " relation(s)"
            return (Just doc)

-- | 'loadFactionYamlOutcome' with the decode outcome discarded, for a
--   caller that only wants the document.
loadFactionYaml ∷ LoggerState → FilePath → IO FactionYamlDoc
loadFactionYaml logger path =
    fromMaybe (FactionYamlDoc [] []) <$> loadFactionYamlOutcome logger path

-- * Post-decode validation

-- | Why a decoded catalogue file was refused entire. Each constructor
--   carries the offending id — a relation pair rendered by
--   'renderTagPair' — because requirement 2 wants the diagnostic to name
--   it.
data CatalogueRefusal
    = MalformedTagId !Text
      -- ^ a declared tag id, or a relation endpoint, that fails
      --   'Unit.Faction.Profile.isValidFactionTagText'.
    | DuplicateTagDecl !Text
      -- ^ a tag declared twice, in this file or against one already
      --   registered.
    | UndeclaredEndpoint !Text
      -- ^ a relation naming a tag no catalogue file declares (D-30).
    | SelfRelation !Text
      -- ^ a relation whose two endpoints are the same tag. Same-tag
      --   alliance is an engine rule and never a row (requirement 3), so
      --   such a row could only ever restate or contradict it.
    | InvalidRelationValue !Text
      -- ^ a @relation:@ outside @ally@ \/ @neutral@ \/ @hostile@.
    | DuplicateRelationPair !Text
      -- ^ an ordered pair declared more than once, by any mix of
      --   directed and symmetric entries, whether or not the two values
      --   agree.
    deriving (Show, Eq)

refusalReason ∷ CatalogueRefusal → Text
refusalReason r = case r of
    MalformedTagId       _ → "malformed faction tag id"
    DuplicateTagDecl     _ → "duplicate faction tag declaration"
    UndeclaredEndpoint   _ → "undeclared faction tag"
    SelfRelation         _ → "self-paired faction relation"
    InvalidRelationValue _ → "invalid faction relation value"
    DuplicateRelationPair _ → "duplicate faction relation"

refusalDetail ∷ CatalogueRefusal → Text
refusalDetail r = case r of
    MalformedTagId       t → t
    DuplicateTagDecl     t → t
    UndeclaredEndpoint   t → t
    SelfRelation         t → t
    InvalidRelationValue t → t
    DuplicateRelationPair t → t

-- | Every relation value the catalogue accepts, and nothing else.
parseRelationValue ∷ Text → Maybe FactionRelation
parseRelationValue t = case t of
    "ally"    → Just RelAlly
    "neutral" → Just RelNeutral
    "hostile" → Just RelHostile
    _         → Nothing

-- | Requirement 2's five rules plus requirement 3's self-relation rule,
--   applied against @cat@ — the catalogue as already registered — in the
--   author's own document order.
--
--   Tags are settled BEFORE relations because a relation's endpoints are
--   checked against the tags this same file declares, which is what lets
--   one file be self-contained. 'Nothing' means the whole document may
--   be admitted.
factionCatalogueRefusal ∷ FactionCatalogue → FactionYamlDoc
                        → Maybe CatalogueRefusal
factionCatalogueRefusal cat doc =
    either Just (const Nothing) (admitFactionYamlDoc cat doc)

-- | The validated document, ready for
--   'Unit.Faction.Catalogue.extendFactionCatalogue', or the first
--   refusal in the author's own document order.
--
--   One function decides both because the tag declarations have to be
--   resolved into 'FactionTag's to be checked at all, and resolving them
--   twice — once to judge, once to register — is how the two drift.
--
--   Tags are settled BEFORE relations because a relation's endpoints are
--   checked against the tags this same file declares, which is what lets
--   one file be self-contained.
admitFactionYamlDoc ∷ FactionCatalogue → FactionYamlDoc
                    → Either CatalogueRefusal
                             ([FactionTagDecl], [BaseRelationEntry])
admitFactionYamlDoc cat doc = do
    declared ← reverse <$> foldM addDecl [] (fydTags doc)
    -- A THROWAWAY catalogue, never stored and never returned: it exists
    -- only so `declaresTag` below sees this file's own declarations
    -- beside everything already registered. Its source path is a label
    -- nothing keys off, because this value does not outlive the call.
    let known = extendFactionCatalogue "<the document being admitted>"
                                       declared [] cat
    (entries, _) ← foldM (addRelation known)
                         ([], Map.keysSet (catalogueDeclaredPairs cat))
                         (fydRelations doc)
    pure (declared, reverse entries)
  where
    -- Declarations accumulate in reverse. A tag is a duplicate when it
    -- is already REGISTERED or already declared earlier in this same
    -- file; both are the same authoring mistake and get the same answer.
    addDecl acc t = case mkFactionTag (fytId t) of
        Nothing  → Left (MalformedTagId (fytId t))
        Just tag
            | declaresTag cat tag ∨ any ((≡ tag) ∘ ftdTag) acc →
                Left (DuplicateTagDecl (fytId t))
            | otherwise →
                Right (FactionTagDecl tag (fytDescription t) : acc)

    -- `seen` is every ordered pair declared so far, by any file and by
    -- any mix of forms — which is what makes a symmetric entry collide
    -- with a directed one, and a directed pair collide with the reverse
    -- half of an earlier symmetric entry, whether or not the values
    -- agree.
    addRelation known (acc, seen) r = do
        src ← endpoint known (fyrFrom r)
        tgt ← endpoint known (fyrTo r)
        when (src ≡ tgt) $ Left (SelfRelation (fyrFrom r))
        rel ← maybe (Left (InvalidRelationValue (fyrRelation r))) Right
                    (parseRelationValue (fyrRelation r))
        let entry | fyrSymmetric r = SymmetricBase src tgt rel
                  | otherwise      = DirectedBase src tgt rel
            added | fyrSymmetric r = [(src, tgt), (tgt, src)]
                  | otherwise      = [(src, tgt)]
        case filter (`Set.member` seen) added of
            (p : _) → Left (DuplicateRelationPair (renderTagPair p))
            []      → Right ( entry : acc
                            , foldr Set.insert seen added )

    endpoint known raw = case mkFactionTag raw of
        Nothing  → Left (MalformedTagId raw)
        Just tag
            | declaresTag known tag → Right tag
            | otherwise             → Left (UndeclaredEndpoint raw)
