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
    , scanFactionTagVocabulary
      -- * Post-decode validation
    , CatalogueRefusal(..)
    , refusalReason
    , refusalDetail
    , factionCatalogueRefusal
    , admitFactionYamlDoc
    , catalogueIntegrityRefusal
    ) where

import UPrelude
import Control.Monad (foldM)
import Data.List (sort)
import Data.Set (Set)
import GHC.Generics (Generic)
import System.Directory (doesDirectoryExist, listDirectory)
import System.FilePath (takeDirectory, takeExtension, (</>))
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
    , catalogueEntries, catalogueTags, declaresTag, renderTagPair )
import Unit.Faction.Profile
    (BaseRelationEntry(..), FactionTag, factionTagText, mkFactionTag)

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

-- | Every faction tag declared ANYWHERE in the directory @path@ sits
--   in, as a vocabulary and nothing more.
--
--   __Why a per-file loader reads its siblings.__ A catalogue family is
--   a DIRECTORY, so a relation's endpoints may legitimately be declared
--   in another file — a new culture's file relating it to @acolyte@ is
--   the obvious case. Resolving endpoints against only what is already
--   REGISTERED would make that file's admission depend on the order the
--   family's files happened to be enumerated in: the same tree would
--   boot on one machine and refuse on another. Staging the vocabulary
--   ahead of admission is what removes that, and it is the same reason
--   @engine.loadTutorialDir@ enumerates its own directory — the
--   question being asked simply is not answerable from inside one file.
--
--   Strictly a VOCABULARY: nothing here registers, refuses, or
--   validates. Each sibling is still decoded, judged and registered by
--   its own queue entry. A sibling that fails to decode contributes no
--   tags (its own entry reports that parse failure terminally), and a
--   malformed id contributes nothing (its own entry refuses it).
--
--   Read in sorted order purely so the work is deterministic; the
--   result is a 'Set' and order cannot reach an answer. It takes no
--   logger for the same reason it warns about nothing.
scanFactionTagVocabulary ∷ FilePath → IO (Set FactionTag)
scanFactionTagVocabulary path = do
    let dir = takeDirectory path
    present ← doesDirectoryExist dir
    if not present then pure Set.empty else do
        names ← sort ∘ filter ((≡ ".yaml") ∘ takeExtension) <$> listDirectory dir
        docs ← mapM (siblingDoc ∘ (dir </>)) names
        pure (Set.fromList
                  [ tag
                  | doc ← catMaybes docs
                  , t ← fydTags doc
                  , Just tag ← [mkFactionTag (fytId t)] ])
  where
    -- Quiet on failure: the sibling's OWN queue entry is what reports a
    -- parse failure, and warning twice for one broken file would put a
    -- second diagnostic in front of the one that names the family.
    siblingDoc p = do
        result ← Yaml.decodeFileEither p
        pure $ case result ∷ Either Yaml.ParseException FactionYamlDoc of
            Left _    → Nothing
            Right doc → Just doc

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

-- | Requirement 2's rules plus requirement 3's self-relation rule,
--   applied against @cat@ — the catalogue as already registered — with
--   endpoints resolved against @vocab@, the whole directory's declared
--   tag vocabulary ('scanFactionTagVocabulary').
--
--   'Nothing' means the whole document may be admitted.
factionCatalogueRefusal ∷ Set FactionTag → FactionCatalogue → FactionYamlDoc
                        → Maybe CatalogueRefusal
factionCatalogueRefusal vocab cat doc =
    either Just (const Nothing) (admitFactionYamlDoc vocab cat doc)

-- | The validated document, ready for
--   'Unit.Faction.Catalogue.extendFactionCatalogue', or the first
--   refusal in the author's own document order.
--
--   One function decides both because the tag declarations have to be
--   resolved into 'FactionTag's to be checked at all, and resolving them
--   twice — once to judge, once to register — is how the two drift.
--
--   __Two different sets, deliberately.__ A DUPLICATE declaration is
--   judged against what is REGISTERED plus what this document has said
--   so far, because that is what "declared twice" means and because a
--   file must not collide with its own vocabulary entry. A relation
--   ENDPOINT is judged against @vocab@ — every tag the directory
--   declares, loaded or not — plus those same two, because a relation
--   may legitimately name a tag a sibling file declares and a rule that
--   said otherwise would depend on enumeration order.
admitFactionYamlDoc ∷ Set FactionTag → FactionCatalogue → FactionYamlDoc
                    → Either CatalogueRefusal
                             ([FactionTagDecl], [BaseRelationEntry])
admitFactionYamlDoc vocab cat doc = do
    declared ← reverse <$> foldM addDecl [] (fydTags doc)
    let known = Set.unions [ vocab, catalogueTags cat
                           , Set.fromList (map ftdTag declared) ]
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
            | Set.member tag known → Right tag
            | otherwise            → Left (UndeclaredEndpoint raw)

-- | Does the WHOLE proposed catalogue still hold together?
--
--   Admitting one document proves that document is well formed against
--   everything else; it does not prove everything else is still well
--   formed against IT. Re-reading a file that dropped a tag another
--   file's relation names is exactly that case: the replacement
--   document is faultless and the catalogue it would produce is not.
--
--   So a write is gated on the complete proposed catalogue, and the
--   refusal names the endpoint that no longer resolves. Endpoints are
--   resolved against @vocab@ for the same reason they are during
--   admission — a relation may name a tag a sibling declares that has
--   not been registered yet, and mid-family that is the normal state.
--
--   Only the endpoint rule is re-run here. Duplicate declarations and
--   duplicate ordered pairs are decided pairwise at admission and no
--   removal can create one.
catalogueIntegrityRefusal ∷ Set FactionTag → FactionCatalogue
                          → Maybe CatalogueRefusal
catalogueIntegrityRefusal vocab cat =
    case [ t | t ← concatMap endpoints (catalogueEntries cat)
             , not (Set.member t known) ] of
        (t : _) → Just (UndeclaredEndpoint (factionTagText t))
        []      → Nothing
  where
    known = Set.union vocab (catalogueTags cat)
    endpoints (DirectedBase s t _)  = [s, t]
    endpoints (SymmetricBase a b _) = [a, b]
