{-# LANGUAGE Strict #-}
-- | Loading + validation of the versioned English concept catalogue
--   (#709). The production catalogue lives at 'conceptCataloguePath';
--   it is authored as a YAML LIST of entries (not a map — YAML maps
--   silently swallow duplicate keys, and duplicate concept ids must be
--   a hard, descriptive 'CatalogueError').
--
--   Validation is fail-loud by design: duplicate or malformed ids,
--   unknown domains, and empty/whitespace lexical forms all reject the
--   whole catalogue rather than degrading into fallback text. Parsing
--   ('parseCatalogue') is pure; only 'loadCatalogue' touches IO.
--
--   A catalogue is TWO files (#1868). Beside the authored YAML sits the
--   generated placement-order artifact at 'conceptOrdinalPath', which
--   records the append-only ordinal 'Language.Generated.Root' places
--   each concept at. Both are read here and validated against each
--   other, so root assignment stays pure and can never be reached with
--   a catalogue whose placement order was reconstructed from ascending
--   ids, from authored YAML order, or from a caller's traversal order —
--   a missing, malformed or disagreeing artifact rejects the catalogue
--   outright instead.
module Language.Semantic.Catalogue
    ( conceptCataloguePath
    , conceptOrdinalPath
    , parseConceptOrdinals
    , parseCatalogue
    , loadCatalogue
    ) where

import UPrelude
import Control.Monad (foldM)
import qualified Data.Aeson as A
import Data.Aeson.Types (parseEither)
import qualified Data.ByteString as BS
import Data.Char (isDigit, isSpace)
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified Data.Text as T
import Data.Yaml ( FromJSON(..), withObject, (.:), (.:?)
                 , decodeEither', prettyPrintParseException )
import Language.Semantic.Types

-- | The production catalogue, relative to the resource root (#636) —
--   the same cwd-relative convention every other @data/@ family uses.
conceptCataloguePath ∷ FilePath
conceptCataloguePath = "data" ⊘ "language" ⊘ "concepts.yaml"

-- | The generated placement-order artifact, beside the catalogue it
--   orders. Under @data/@ rather than @docs/@ precisely because it is
--   loaded at run time through the same resource root.
conceptOrdinalPath ∷ FilePath
conceptOrdinalPath = "data" ⊘ "language" ⊘ "concept_id_baseline.json"

-- | The artifact schema this build reads. Bumped in lockstep with
--   @tools/concept_id_inventory_audit.py@\'s @BASELINE_VERSION@; an
--   older or newer file is a descriptive rejection, never a guess at
--   what it meant.
conceptOrdinalVersion ∷ Int
conceptOrdinalVersion = 2

-- | Raw shapes as they appear in YAML, before validation. Kept internal
--   so nothing downstream can hold an unvalidated catalogue.
data RawConcept = RawConcept
    { rcId         ∷ !Text
    , rcDomain     ∷ !Text
    , rcSingular   ∷ !Text
    , rcPlural     ∷ !(Maybe Text)
    , rcModifier   ∷ !(Maybe Text)
    , rcPossessive ∷ !(Maybe Text)
    }

instance FromJSON RawConcept where
    parseJSON = withObject "concept" $ \v → RawConcept
        ⊚ v .:  "id"
        ⊛ v .:  "domain"
        ⊛ v .:  "singular"
        ⊛ v .:? "plural"
        ⊛ v .:? "modifier"
        ⊛ v .:? "possessive"

-- Positional on purpose: the only consumer is validateCatalogue's
-- pattern match, so named selectors would be unused top-level binds
-- (a -Werror failure).
data RawCatalogue = RawCatalogue !Int ![RawConcept]

-- The placement-order artifact's raw shapes. Positional for the same
-- reason 'RawCatalogue' is. The @ordinal@ field decodes through aeson's
-- 'Int' instance, which rejects a string, a boolean and a fractional
-- number, so a non-integer ordinal is an 'OrdinalJsonError' naming the
-- offending path rather than a silently truncated position.
data RawOrdinalEntry = RawOrdinalEntry !Text !Int

instance FromJSON RawOrdinalEntry where
    parseJSON = withObject "concept ordinal" $ \v → RawOrdinalEntry
        ⊚ v .: "id"
        ⊛ v .: "ordinal"

instance FromJSON RawCatalogue where
    parseJSON = withObject "concept catalogue" $ \v → RawCatalogue
        ⊚ v .: "version"
        ⊛ v .: "concepts"

-- | Parse and validate the generated placement-order artifact.
--
--   Every failure is descriptive and total: malformed JSON, an
--   unreadable schema version, a non-integer or missing ordinal, a
--   repeated id and a repeated ordinal are each their own
--   'CatalogueError'. Nothing here reconstructs an order — an artifact
--   this rejects rejects the catalogue with it.
parseConceptOrdinals ∷ BS.ByteString → Either CatalogueError ConceptOrdinals
parseConceptOrdinals bytes = case A.eitherDecodeStrict' bytes of
    Left err → Left $ OrdinalJsonError $ T.pack err
    Right value → do
        -- The version is read and checked BEFORE the entries, so a
        -- superseded artifact (schema 1 recorded a bare `ids` array,
        -- and lived under docs/) reports the version it actually
        -- declares rather than a confusing missing-key error about the
        -- key that schema never had.
        ver ← field "version" value
        when (ver ≢ conceptOrdinalVersion) $
            Left (UnsupportedOrdinalVersion ver)
        entries ← field "concepts" value
        pairs ← traverse validateEntry entries
        mkConceptOrdinals pairs
  where
    field ∷ FromJSON α ⇒ A.Key → A.Value → Either CatalogueError α
    field name value =
        case parseEither (withObject "concept placement order" (.: name))
                         value of
            Left err → Left (OrdinalJsonError (T.pack err))
            Right x  → Right x

    validateEntry (RawOrdinalEntry raw n) =
        (\cid → (cid, n)) ⊚ validateId raw

-- | Parse and validate a concept catalogue from YAML bytes, against the
--   already-validated placement order it must agree with.
--
--   The YAML is validated FIRST, so a malformed catalogue reports its
--   own defect rather than an id-set mismatch downstream of it.
parseCatalogue ∷ ConceptOrdinals → BS.ByteString
               → Either CatalogueError Catalogue
parseCatalogue ords bytes = case decodeEither' bytes of
    Left yamlErr →
        Left $ CatalogueYamlError $ T.pack $ prettyPrintParseException yamlErr
    Right raw → validateCatalogue ords raw

-- | Read and validate the catalogue at @catPath@ together with the
--   placement-order artifact at @ordPath@.
loadCatalogue ∷ FilePath → FilePath → IO (Either CatalogueError Catalogue)
loadCatalogue catPath ordPath = do
    ordBytes ← BS.readFile ordPath
    catBytes ← BS.readFile catPath
    pure $ do
        ords ← parseConceptOrdinals ordBytes
        parseCatalogue ords catBytes

validateCatalogue ∷ ConceptOrdinals → RawCatalogue
                  → Either CatalogueError Catalogue
validateCatalogue ords (RawCatalogue ver raws)
    | ver < 1   = Left (InvalidVersion ver)
    | otherwise = mkCatalogue ⌫ foldM step (M.empty, M.empty) raws
  where
    -- The two files must name exactly the same concepts. An id the
    -- artifact has never recorded has no placement position, and a
    -- recorded id the catalogue dropped is a removal the ratchet exists
    -- to refuse; either way the pair is rejected rather than repaired.
    mkCatalogue (concepts, _)
        | null unrecorded ∧ null unknown = Right (Catalogue ver concepts ords)
        | otherwise = Left (OrdinalCatalogueMismatch unrecorded unknown)
      where
        recorded   = S.fromList (ordinalIds ords)
        authored   = M.keysSet concepts
        unrecorded = S.toList (S.difference authored recorded)
        unknown    = S.toList (S.difference recorded authored)

    -- Accumulator: the validated catalogue map, plus every singular form
    -- seen so far (lowercased) keyed to the concept that first authored
    -- it, so a later collision can name both offending ids.
    step ∷ (M.Map ConceptId ConceptEntry, M.Map Text ConceptId) → RawConcept
         → Either CatalogueError (M.Map ConceptId ConceptEntry, M.Map Text ConceptId)
    step (acc, singulars) rc = do
        cid ← validateId (rcId rc)
        when (M.member cid acc) $ Left (DuplicateConceptId cid)
        dom ← case domainFromText (rcDomain rc) of
            Nothing → Left (UnknownDomain cid (rcDomain rc))
            Just d  → Right d
        sing ← validateForm cid FormSingular (rcSingular rc)
        let singKey = T.toLower sing
        case M.lookup singKey singulars of
            Just first → Left (DuplicateSingularForm sing first cid)
            Nothing    → pure ()
        plu  ← traverse (validateForm cid FormPlural)     (rcPlural rc)
        modi ← traverse (validateForm cid FormModifier)   (rcModifier rc)
        poss ← traverse (validateForm cid FormPossessive) (rcPossessive rc)
        pure ( M.insert cid (ConceptEntry dom sing plu modi poss) acc
             , M.insert singKey cid singulars )

-- | Concept ids are #710's root-derivation input, so their shape is
--   pinned: nonempty, starting with an uppercase ASCII letter, made of
--   uppercase ASCII letters, digits, and underscores only.
validateId ∷ Text → Either CatalogueError ConceptId
validateId raw
    | T.null raw =
        Left $ InvalidConceptId raw "concept id is empty"
    | not (leadChar (T.head raw)) =
        Left $ InvalidConceptId raw
             "concept id must start with an uppercase ASCII letter"
    | not (T.all idChar raw) =
        Left $ InvalidConceptId raw
             "concept id may contain only uppercase ASCII letters, digits, and underscores"
    | otherwise = Right (ConceptId raw)
  where
    leadChar c = c ≥ 'A' ∧ c ≤ 'Z'
    idChar c   = leadChar c ∨ isDigit c ∨ c ≡ '_'

-- | Authored forms must be real words: nonempty and whitespace-free
--   (the gloss renderer owns word spacing and capitalizes only the
--   first character of each form, so an internal space would silently
--   break proper-name capitalization).
validateForm ∷ ConceptId → FormKind → Text → Either CatalogueError Text
validateForm cid k w
    | T.null w =
        Left $ InvalidLexicalForm cid k "form is empty"
    | T.any isSpace w =
        Left $ InvalidLexicalForm cid k "form contains whitespace"
    | otherwise = Right w
