{-# LANGUAGE Strict, DeriveGeneric, DeriveAnyClass, DerivingStrategies #-}
-- | Semantic proper names (#709): the language-independent middle layer
--   of the world-naming arc (#708). A proper name is a structured
--   'NameExpr' over stable 'ConceptId's — never an English source
--   string — so the same meaning can render both as an English gloss
--   ("Ashen Land", 'Language.Semantic.English') and, later, as a native
--   name in a generated language (#710).
--
--   Concept ids are LOAD-BEARING for #710: a generated language derives
--   each concept's native root deterministically from the id string, so
--   renaming an id silently re-roots every language that uses it — and
--   'Language.Etymology' reports an id the catalogue no longer carries
--   as @EtyInvalidConcept@, so a removal costs every already-persisted
--   'EtymologySource' naming it (#1104) its etymology.
--   Ids may be added, never renamed or reused.
--
--   PART of that rule is enforced (#1717) — not the whole of it.
--   @tools\/concept_id_inventory_audit.py@ (CI + @make ci@, with its own
--   @--self-test@) pins every shipped id against
--   @data\/language\/concept_id_baseline.json@, which records an id's
--   PRESENCE, exact STRING and 'ConceptOrdinals' position and nothing
--   else: a REMOVAL fails, a RENAME fails as both a removal and an
--   addition, and an ADDITION passes only through that audit's
--   @--update-baseline@ ratchet. Same-string REUSE — keeping an id
--   while repurposing what it means — is deliberately NOT enforced and
--   stays a review policy: the audit leaves the authored English forms
--   and the 'ConceptDomain' editable (they are display data, free to
--   improve), so a repurposed entry is indistinguishable there from an
--   ordinary copy-edit.
--
--   That artifact lives under @data\/@ rather than @docs\/@ because it
--   is READ at run time, through the resource root (#636), beside
--   @concepts.yaml@: 'Catalogue' carries the ordinals it records, and
--   'Language.Generated.Root' places concepts in that order.
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
    , ConceptOrdinals
    , CatalogueError(..)
    , RenderError(..)
    , mkConceptOrdinals
    , conceptOrdinal
    , ordinalIds
    , ordinalCount
    , placementOrder
    , lookupConcept
    , conceptCount
    , conceptIds
    , formOf
    , numberFormKind
    , formKindText
    , domainFromText
    , catalogueErrorText
    , renderErrorText
    ) where

import UPrelude
import GHC.Generics (Generic)
import Control.DeepSeq (NFData)
import Data.List (sortOn)
import Data.Serialize (Serialize)
import qualified Data.Map.Strict as M
import qualified Data.Text as T

-- | Stable identifier of a semantic concept (e.g. @ASH@, @LAND@).
--   Uppercase ASCII letters, digits, and underscores; independent of —
--   and never derived from — the concept's English wording.
--
--   'Serialize'\/'NFData' are derived from the underlying 'Text' so a
--   name's originating expression can ride into a save as part of
--   #1104's optional 'Language.Etymology.Source.EtymologySource'.
newtype ConceptId = ConceptId { conceptIdText ∷ Text }
    deriving stock (Generic)
    deriving newtype (Show, Eq, Ord, NFData, Serialize)

-- | Explicit grammatical number. The renderer never infers number (or
--   any other lexical role) from an English string.
--
--   Serialized positionally by constructor tag through @Generic
--   Serialize@ (#1104) — APPEND-ONLY, like every other enum reachable
--   from a save (see the enum schema policy in @CLAUDE.md@ and
--   @tools\/enum_append_only_audit.py@).
data GramNumber = Singular | Plural
    deriving (Show, Eq, Generic, NFData, Serialize)

-- | A proper name as structured meaning. The four supported forms are
--   the #709 contract that #710's generated-language renderer must also
--   cover — extend only alongside that issue's form list.
--
--   Serialized positionally by constructor tag through @Generic
--   Serialize@ (#1104, which persists a generated name's originating
--   expression) — APPEND-ONLY, exactly like 'GramNumber' above.
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
    deriving (Show, Eq, Generic, NFData, Serialize)

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
--   traversal downstream is deterministic and independent of insertion
--   order.
--
--   'catOrdinals' is the separately-recorded APPEND-ONLY placement order
--   #710 root assignment folds in (#1868), validated at load time to
--   name exactly the same ids as 'catConcepts'. It is a field of the
--   catalogue rather than a parameter threaded past it so that holding a
--   validated catalogue is the same thing as holding the order its
--   concepts are placed in — there is no way to reach root assignment
--   with one but not the other.
data Catalogue = Catalogue
    { catVersion  ∷ !Int
    , catConcepts ∷ !(M.Map ConceptId ConceptEntry)
    , catOrdinals ∷ !ConceptOrdinals
    } deriving (Show, Eq)

-- | Each concept's position in the catalogue's append-only placement
--   order, recorded in @data\/language\/concept_id_baseline.json@ and
--   ratcheted by @tools\/concept_id_inventory_audit.py@.
--
--   The ordinal exists because root assignment used to place concepts in
--   ascending id order, which made an ADDITION able to move an existing
--   concept's root: a new id sorting before an incumbent could take the
--   root that incumbent would have had, forcing it to reroll to an
--   entirely different one and silently costing every persisted
--   'Language.Etymology.Source.EtymologySource' naming it its etymology
--   (#1868). An appended ordinal cannot displace anything already
--   placed.
--
--   This is a recorded fact, not a derivable one: the seeded ordinals
--   happen to equal ascending-id rank (that identity is what let the
--   ordinal be introduced without changing any existing language), but
--   every later addition breaks that coincidence, so nothing may
--   reconstruct these from the catalogue.
newtype ConceptOrdinals = ConceptOrdinals (M.Map ConceptId Int)
    deriving (Show, Eq)

-- | Build the placement order from recorded @(id, ordinal)@ pairs,
--   rejecting a repeated id or a repeated ordinal — either would make
--   placement ambiguous, so neither is papered over.
--
--   This is the ONLY constructor: 'ConceptOrdinals' is exported
--   abstractly so no caller can conjure an order from ascending ids,
--   authored YAML order, or its own traversal order.
mkConceptOrdinals ∷ [(ConceptId, Int)] → Either CatalogueError ConceptOrdinals
mkConceptOrdinals = go M.empty M.empty
  where
    go byId _ [] = Right (ConceptOrdinals byId)
    go byId byOrdinal ((cid, n) : rest)
        | M.member cid byId = Left (DuplicateOrdinalId cid)
        | Just other ← M.lookup n byOrdinal = Left (DuplicateOrdinal n other cid)
        | otherwise = go (M.insert cid n byId) (M.insert n cid byOrdinal) rest

conceptOrdinal ∷ ConceptId → ConceptOrdinals → Maybe Int
conceptOrdinal cid (ConceptOrdinals m) = M.lookup cid m

-- | Every id the placement order records, in ascending id order.
ordinalIds ∷ ConceptOrdinals → [ConceptId]
ordinalIds (ConceptOrdinals m) = M.keys m

ordinalCount ∷ ConceptOrdinals → Int
ordinalCount (ConceptOrdinals m) = M.size m

-- | @ids@ in placement order: recorded concepts by ascending ordinal.
--
--   An id the order does not record is placed AFTER every recorded one,
--   in ascending id order. That tail is unreachable through the
--   production path — loading validates that the catalogue and the
--   artifact name the same ids, and refuses the pair otherwise — and it
--   is deliberately not a fallback to the old ascending-id placement:
--   it is the only tail rule that cannot move a recorded concept's root,
--   which is exactly the property an unrecorded id must not be able to
--   break.
placementOrder ∷ ConceptOrdinals → [ConceptId] → [ConceptId]
placementOrder ords = sortOn key
  where
    -- 'Left' sorts before 'Right', so every recorded concept is placed
    -- (by ordinal) ahead of every unrecorded one (by id).
    key cid = maybe (Right cid) Left (conceptOrdinal cid ords)
                ∷ Either Int ConceptId

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
    | DuplicateSingularForm !Text !ConceptId !ConceptId
      -- ^ colliding singular (as authored on the first concept, compared
      --   case-insensitively), the first concept to author it, and the
      --   later concept that repeats it
    | OrdinalJsonError !Text
      -- ^ the placement-order artifact's JSON failed to parse or decode
    | UnsupportedOrdinalVersion !Int
      -- ^ the artifact declares a schema version this build cannot read
    | DuplicateOrdinalId !ConceptId
      -- ^ the artifact records one concept twice
    | DuplicateOrdinal !Int !ConceptId !ConceptId
      -- ^ one ordinal, and the two concepts claiming it
    | OrdinalCatalogueMismatch ![ConceptId] ![ConceptId]
      -- ^ ids the catalogue carries but the artifact does not record,
      --   and ids the artifact records but the catalogue does not carry
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
        <> tshow v
    InvalidConceptId raw why →
        "invalid concept id " <> tshow raw <> ": " <> why
    DuplicateConceptId (ConceptId cid) →
        "duplicate concept id " <> cid
    UnknownDomain (ConceptId cid) raw →
        "concept " <> cid <> " has unknown domain " <> tshow raw
        <> " (expected one of: place, element, celestial, creature,"
        <> " emotion, mythic)"
    InvalidLexicalForm (ConceptId cid) k why →
        "concept " <> cid <> " has an invalid " <> formKindText k
        <> " form: " <> why
    DuplicateSingularForm sing (ConceptId first) (ConceptId second) →
        "concepts " <> first <> " and " <> second
        <> " share the same singular form " <> tshow sing
    OrdinalJsonError msg →
        "concept placement-order artifact is malformed: " <> msg
    UnsupportedOrdinalVersion v →
        "concept placement-order artifact declares schema version "
        <> tshow v <> ", which this build cannot read; regenerate it with"
        <> " `python3 tools/concept_id_inventory_audit.py --update-baseline`"
    DuplicateOrdinalId (ConceptId cid) →
        "concept placement-order artifact records " <> cid
        <> " more than once"
    DuplicateOrdinal n (ConceptId first) (ConceptId second) →
        "concept placement-order artifact gives ordinal " <> tshow n
        <> " to both " <> first <> " and " <> second
        <> "; an ordinal is a placement position, so it identifies"
        <> " exactly one concept"
    OrdinalCatalogueMismatch unrecorded unknown →
        "the concept catalogue and its placement-order artifact name"
        <> " different concepts"
        <> listing " unrecorded in the artifact: " unrecorded
        <> listing " recorded but absent from the catalogue: " unknown
        <> "; run `python3 tools/concept_id_inventory_audit.py"
        <> " --update-baseline` to record a deliberate addition"
      where
        listing _ []   = ""
        listing lbl cs = lbl <> T.intercalate ", " [ c | ConceptId c ← cs ]

renderErrorText ∷ RenderError → Text
renderErrorText err = case err of
    UnknownConcept (ConceptId cid) →
        "unknown concept id " <> cid <> " in name expression"
    MissingForm (ConceptId cid) k →
        "concept " <> cid <> " has no authored " <> formKindText k
        <> " form, which this name expression requires"
