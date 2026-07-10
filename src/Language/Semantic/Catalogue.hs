{-# LANGUAGE Strict, UnicodeSyntax #-}
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
module Language.Semantic.Catalogue
    ( conceptCataloguePath
    , parseCatalogue
    , loadCatalogue
    ) where

import UPrelude
import Control.Monad (foldM)
import qualified Data.ByteString as BS
import Data.Char (isDigit, isSpace)
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import Data.Yaml ( FromJSON(..), withObject, (.:), (.:?)
                 , decodeEither', prettyPrintParseException )
import Language.Semantic.Types

-- | The production catalogue, relative to the resource root (#636) —
--   the same cwd-relative convention every other @data/@ family uses.
conceptCataloguePath ∷ FilePath
conceptCataloguePath = "data" ⊘ "language" ⊘ "concepts.yaml"

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

instance FromJSON RawCatalogue where
    parseJSON = withObject "concept catalogue" $ \v → RawCatalogue
        ⊚ v .: "version"
        ⊛ v .: "concepts"

-- | Parse and validate a concept catalogue from YAML bytes.
parseCatalogue ∷ BS.ByteString → Either CatalogueError Catalogue
parseCatalogue bytes = case decodeEither' bytes of
    Left yamlErr →
        Left $ CatalogueYamlError $ T.pack $ prettyPrintParseException yamlErr
    Right raw → validateCatalogue raw

-- | Read and validate the catalogue file at @path@.
loadCatalogue ∷ FilePath → IO (Either CatalogueError Catalogue)
loadCatalogue path = parseCatalogue ⊚ BS.readFile path

validateCatalogue ∷ RawCatalogue → Either CatalogueError Catalogue
validateCatalogue (RawCatalogue ver raws)
    | ver < 1   = Left (InvalidVersion ver)
    | otherwise = Catalogue ver ⊚ foldM step M.empty raws
  where
    step ∷ M.Map ConceptId ConceptEntry → RawConcept
         → Either CatalogueError (M.Map ConceptId ConceptEntry)
    step acc rc = do
        cid ← validateId (rcId rc)
        when (M.member cid acc) $ Left (DuplicateConceptId cid)
        dom ← case domainFromText (rcDomain rc) of
            Nothing → Left (UnknownDomain cid (rcDomain rc))
            Just d  → Right d
        sing ← validateForm cid FormSingular (rcSingular rc)
        plu  ← traverse (validateForm cid FormPlural)     (rcPlural rc)
        modi ← traverse (validateForm cid FormModifier)   (rcModifier rc)
        poss ← traverse (validateForm cid FormPossessive) (rcPossessive rc)
        pure $ M.insert cid (ConceptEntry dom sing plu modi poss) acc

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
