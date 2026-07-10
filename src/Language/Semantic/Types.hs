{-# LANGUAGE Strict, UnicodeSyntax #-}
-- | Semantic proper names (#709): the language-independent middle layer
--   of the world-naming arc (#708). A proper name is a structured
--   'NameExpr' over stable 'ConceptId's — never an English source
--   string — so the same meaning can render both as an English gloss
--   ("Ashen Land", 'Language.Semantic.English') and, later, as a native
--   name in a generated language (#710).
--
--   Concept ids are LOAD-BEARING for #710: a generated language derives
--   each concept's native root deterministically from the id string, so
--   renaming an id silently re-roots every language that uses it.
--   Ids may be added, never renamed or reused.
--
--   English lexical forms are AUTHORED, not derived from spelling
--   (@memory@ → @memories@, @oath@ → modifier @sworn@); a form a name
--   expression needs but the catalogue doesn't supply is a descriptive
--   'RenderError', never a fallback to the raw id or fabricated English.
--
--   This layer is pure: no engine, world, Lua, or random state.
module Language.Semantic.Types
    ( ConceptId(..)
    , GramNumber(..)
    , NameExpr(..)
    , ConceptDomain(..)
    , FormKind(..)
    , ConceptEntry(..)
    , Catalogue(..)
    , CatalogueError(..)
    , RenderError(..)
    , lookupConcept
    , conceptCount
    , conceptIds
    , formOf
    , numberFormKind
    , formKindText
    , domainText
    , domainFromText
    , catalogueErrorText
    , renderErrorText
    ) where

import UPrelude
import qualified Data.Map.Strict as M
import qualified Data.Text as T

-- | Stable identifier of a semantic concept (e.g. @ASH@, @LAND@).
--   Uppercase ASCII letters, digits, and underscores; independent of —
--   and never derived from — the concept's English wording.
newtype ConceptId = ConceptId { conceptIdText ∷ Text }
    deriving (Show, Eq, Ord)

-- | Explicit grammatical number. The renderer never infers number (or
--   any other lexical role) from an English string.
data GramNumber = Singular | Plural
    deriving (Show, Eq)

-- | A proper name as structured meaning. The four supported forms are
--   the #709 contract that #710's generated-language renderer must also
--   cover — extend only alongside that issue's form list.
data NameExpr
    = Bare !ConceptId
      -- ^ @Bare(SILENCE)@ → \"Silence\"
    | Modifier !ConceptId !ConceptId
      -- ^ modifier, then head: @Modifier(ASH, LAND)@ → \"Ashen Land\"
    | Of !ConceptId !GramNumber !ConceptId
      -- ^ head, then explicitly-numbered complement:
      --   @Of(EYE, plural STORM)@ → \"Eye of Storms\"
    | Possessive !ConceptId !ConceptId
      -- ^ owner, then possessed head:
      --   @Possessive(WOLF, HEART)@ → \"Wolf's Heart\"
    deriving (Show, Eq)

-- | The naming domains the starter vocabulary spans (#709 req 7).
data ConceptDomain
    = DomainPlace      -- ^ world/place heads (land, isle, gate, ...)
    | DomainElement    -- ^ elements and materials (ash, iron, frost, ...)
    | DomainCelestial  -- ^ celestial and temporal (moon, dawn, winter, ...)
    | DomainCreature   -- ^ animals, body parts, symbols (wolf, eye, crown, ...)
    | DomainEmotion    -- ^ emotions, virtues, calamities (sorrow, mercy, ruin, ...)
    | DomainMythic     -- ^ mythic and social abstractions (god, oath, memory, ...)
    deriving (Show, Eq, Ord, Enum, Bounded)

-- | Which authored lexical form of a concept a rendering needs.
data FormKind = FormSingular | FormPlural | FormModifier | FormPossessive
    deriving (Show, Eq)

-- | One concept's authored English lexical forms. Only the singular is
--   mandatory; a missing optional form is a rendering error when (and
--   only when) a name expression demands it.
data ConceptEntry = ConceptEntry
    { ceDomain     ∷ !ConceptDomain
    , ceSingular   ∷ !Text
    , cePlural     ∷ !(Maybe Text)
    , ceModifier   ∷ !(Maybe Text)
    , cePossessive ∷ !(Maybe Text)
    } deriving (Show, Eq)

-- | A validated concept catalogue. Keyed by an ordered 'M.Map' so every
--   traversal downstream (#710 root assignment in particular) is
--   deterministic and independent of insertion order.
data Catalogue = Catalogue
    { catVersion  ∷ !Int
    , catConcepts ∷ !(M.Map ConceptId ConceptEntry)
    } deriving (Show, Eq)

-- | Why a concept catalogue was rejected at load/validation time.
data CatalogueError
    = CatalogueYamlError !Text
      -- ^ the YAML itself failed to parse or decode
    | InvalidVersion !Int
    | InvalidConceptId !Text !Text
      -- ^ offending raw id, reason
    | DuplicateConceptId !ConceptId
    | UnknownDomain !ConceptId !Text
      -- ^ concept, offending raw domain
    | InvalidLexicalForm !ConceptId !FormKind !Text
      -- ^ concept, form, reason
    deriving (Show, Eq)

-- | Why a semantically well-typed 'NameExpr' could not be rendered
--   against a given catalogue.
data RenderError
    = UnknownConcept !ConceptId
    | MissingForm !ConceptId !FormKind
    deriving (Show, Eq)

lookupConcept ∷ ConceptId → Catalogue → Maybe ConceptEntry
lookupConcept cid = M.lookup cid ∘ catConcepts

conceptCount ∷ Catalogue → Int
conceptCount = M.size ∘ catConcepts

-- | All concept ids, in stable ascending order.
conceptIds ∷ Catalogue → [ConceptId]
conceptIds = M.keys ∘ catConcepts

-- | The authored text of one lexical form, if the author supplied it.
formOf ∷ FormKind → ConceptEntry → Maybe Text
formOf FormSingular   = Just ∘ ceSingular
formOf FormPlural     = cePlural
formOf FormModifier   = ceModifier
formOf FormPossessive = cePossessive

-- | The noun form an explicit grammatical number selects.
numberFormKind ∷ GramNumber → FormKind
numberFormKind Singular = FormSingular
numberFormKind Plural   = FormPlural

formKindText ∷ FormKind → Text
formKindText FormSingular   = "singular"
formKindText FormPlural     = "plural"
formKindText FormModifier   = "modifier"
formKindText FormPossessive = "possessive"

domainText ∷ ConceptDomain → Text
domainText DomainPlace     = "place"
domainText DomainElement   = "element"
domainText DomainCelestial = "celestial"
domainText DomainCreature  = "creature"
domainText DomainEmotion   = "emotion"
domainText DomainMythic    = "mythic"

domainFromText ∷ Text → Maybe ConceptDomain
domainFromText "place"     = Just DomainPlace
domainFromText "element"   = Just DomainElement
domainFromText "celestial" = Just DomainCelestial
domainFromText "creature"  = Just DomainCreature
domainFromText "emotion"   = Just DomainEmotion
domainFromText "mythic"    = Just DomainMythic
domainFromText _           = Nothing

catalogueErrorText ∷ CatalogueError → Text
catalogueErrorText err = case err of
    CatalogueYamlError msg →
        "concept catalogue YAML is malformed: " <> msg
    InvalidVersion v →
        "concept catalogue version must be a positive integer, got "
        <> T.pack (show v)
    InvalidConceptId raw why →
        "invalid concept id " <> T.pack (show raw) <> ": " <> why
    DuplicateConceptId (ConceptId cid) →
        "duplicate concept id " <> cid
    UnknownDomain (ConceptId cid) raw →
        "concept " <> cid <> " has unknown domain " <> T.pack (show raw)
        <> " (expected one of: place, element, celestial, creature,"
        <> " emotion, mythic)"
    InvalidLexicalForm (ConceptId cid) k why →
        "concept " <> cid <> " has an invalid " <> formKindText k
        <> " form: " <> why

renderErrorText ∷ RenderError → Text
renderErrorText err = case err of
    UnknownConcept (ConceptId cid) →
        "unknown concept id " <> cid <> " in name expression"
    MissingForm (ConceptId cid) k →
        "concept " <> cid <> " has no authored " <> formKindText k
        <> " form, which this name expression requires"
