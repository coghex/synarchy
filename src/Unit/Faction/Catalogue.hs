{-# LANGUAGE Strict #-}
-- | The loaded faction-tag catalogue (#2506, FTS-2 of the faction-tag
--   arc #2496): the registry behind @data\/factions\/*.yaml@ and the two
--   validation rules that read it.
--
--   __What this module owns and what it deliberately does not.__ It
--   holds the DECLARED tags and the authored base relations, and it
--   answers the one question authored data is allowed to ask of them —
--   "is this id declared?" (D-30). It owns no YAML: the decoding and
--   the per-document rules live in "Engine.Asset.YamlFactions", which
--   merges its result in here. It owns no precedence either:
--   'cataloguePolicy' hands the base table to
--   "Unit.Faction.Profile"'s 'FactionPolicy' and the five-tier
--   precedence stays exactly where FTS-1 put it.
--
--   __Declaring a tag does not close the namespace__ (requirement 3,
--   D-12). The catalogue is what AUTHORED data may name; runtime
--   systems still mint team and conflict tags nobody declared, and
--   those evaluate under the same precedence — neutral unless shared or
--   related. The only thing an undeclared tag cannot do is appear in a
--   shipped YAML file.
--
--   __Same-tag alliance and the neutral default are engine rules, never
--   rows__ (requirement 3). 'Unit.Faction.Profile.relationFromTo'
--   answers @RelAlly@ for a shared tag and @RelNeutral@ for an
--   unrelated pair before this table is consulted at all, so a catalogue
--   that tried to declare either would be stating something the engine
--   already guarantees — which is why a self-paired relation row is
--   REFUSED rather than merged (see 'Engine.Asset.YamlFactions').
--
--   __Accumulating across files.__ A family is a directory, so the
--   registry grows one file at a time and the uniqueness rules span
--   everything registered so far, not just the document in hand: a
--   second file re-declaring @nomad@, or re-declaring the ordered pair
--   @acolyte → nomad@, is refused exactly as a file that repeats itself
--   internally is. That is the same whole-file rule
--   'Engine.Asset.YamlFlora' applies to a duplicate species name.
--
--   __Re-loading ONE file is not a collision__, though. The registry
--   keeps a contribution per source path, so a second
--   @engine.loadFactionYaml@ on the same file replaces what that file
--   said instead of colliding with it — the insert\/replace semantics
--   every other @engine.load*Yaml@ verb has, expressed at the one
--   granularity a whole-file rule can have.
module Unit.Faction.Catalogue
    ( -- * Declarations
      FactionTagDecl(..)
      -- * The registry
    , FactionCatalogue
    , FactionSource(..)
    , emptyFactionCatalogue
    , catalogueDeclarations
    , catalogueTags
    , catalogueEntries
    , cataloguePolicy
    , catalogueDeclaredPairs
    , declaresTag
    , withoutSource
    , extendFactionCatalogue
      -- * Resolving authored tag references
    , FactionTagRejection(..)
    , rejectionReason
    , rejectionDetail
    , resolveDeclaredTags
    , renderTagPair
    ) where

import UPrelude
import Data.Map.Strict (Map)
import Data.Set (Set)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Unit.Faction (FactionRelation(..))
import Unit.Faction.Profile
    ( BaseRelationEntry(..), FactionPolicy, FactionTag, factionTagText
    , mkFactionPolicy, mkFactionTag )

-- * Declarations

-- | One @faction_tags:@ entry, as the catalogue keeps it.
--
--   The @description@ is documentation for the author and nothing else —
--   no engine rule reads it — but it is kept rather than discarded so a
--   future authoring tool can show what a tag MEANS without reparsing
--   the file.
data FactionTagDecl = FactionTagDecl
    { ftdTag         ∷ !FactionTag
    , ftdDescription ∷ !(Maybe Text)
    } deriving (Show, Eq)

-- * The registry

-- | Everything @data\/factions\/@ has declared so far, kept as one
--   CONTRIBUTION per source file.
--
--   Per file rather than as one merged pile, because a catalogue file is
--   a document: re-loading the same path replaces what that path said
--   ('withoutSource' first, then 'extendFactionCatalogue'), which is
--   what makes @engine.loadFactionYaml@ idempotent the way every other
--   @engine.load*Yaml@ verb is, while two DIFFERENT files colliding on a
--   tag or an ordered pair stays the refusal it should be.
--
--   Contributions keep load order, and within one contribution the
--   author's own order, because that is the order a diagnostic should
--   report in. Neither order changes an answer: 'cataloguePolicy'
--   reduces the entries with a commutative max
--   ('Unit.Faction.Profile.mkFactionPolicy'), and membership is a 'Set'.
data FactionCatalogue = FactionCatalogue
    { fcSources ∷ ![FactionSource] }
    deriving (Show, Eq)

-- | One file's admitted declarations.
data FactionSource = FactionSource
    { fsPath    ∷ !FilePath
    , fsDecls   ∷ ![FactionTagDecl]
    , fsEntries ∷ ![BaseRelationEntry]
    } deriving (Show, Eq)

emptyFactionCatalogue ∷ FactionCatalogue
emptyFactionCatalogue = FactionCatalogue []

catalogueDeclarations ∷ FactionCatalogue → [FactionTagDecl]
catalogueDeclarations = concatMap fsDecls ∘ fcSources

catalogueTags ∷ FactionCatalogue → Set FactionTag
catalogueTags = Set.fromList ∘ map ftdTag ∘ catalogueDeclarations

catalogueEntries ∷ FactionCatalogue → [BaseRelationEntry]
catalogueEntries = concatMap fsEntries ∘ fcSources

-- | Every ordered @(source, target)@ pair the authored entries declare,
--   with its relation. A symmetric entry contributes both directions; a
--   directed one contributes exactly its own.
--
--   This is what makes a re-declaration detectable at all:
--   'mkFactionPolicy' merges a repeated pair by severity and so cannot
--   see one.
catalogueDeclaredPairs ∷ FactionCatalogue
                       → Map (FactionTag, FactionTag) FactionRelation
catalogueDeclaredPairs = foldl' addPair Map.empty ∘ catalogueEntries
  where
    addPair m (DirectedBase s t r)  = Map.insert (s, t) r m
    addPair m (SymmetricBase a b r) = Map.insert (b, a) r
                                          (Map.insert (a, b) r m)

-- | The base table FTS-1's policy evaluates against. Built from the
--   authored entries rather than from 'catalogueDeclaredPairs' so there
--   is one expansion rule and it is 'mkFactionPolicy'\'s.
cataloguePolicy ∷ FactionCatalogue → FactionPolicy
cataloguePolicy = mkFactionPolicy ∘ catalogueEntries

declaresTag ∷ FactionCatalogue → FactionTag → Bool
declaresTag cat t = Set.member t (catalogueTags cat)

-- | Drop whatever @path@ contributed, leaving every other file's
--   contribution untouched.
--
--   A loader calls this BEFORE validating a re-read of that same path,
--   so the file is judged against the rest of the directory rather than
--   against its own previous self — which would otherwise refuse every
--   reload as a duplicate declaration.
withoutSource ∷ FilePath → FactionCatalogue → FactionCatalogue
withoutSource path (FactionCatalogue sources) =
    FactionCatalogue [ s | s ← sources, fsPath s ≢ path ]

-- | Append one file's already-validated declarations and entries.
--
--   Total and unconditional: every uniqueness rule has been decided by
--   the caller ("Engine.Asset.YamlFactions") against the catalogue this
--   is being applied to, because a rule that fired here would already be
--   too late — half the document would be registered. A second call for
--   a path already present appends beside it rather than replacing it;
--   'withoutSource' is what a reload goes through.
extendFactionCatalogue ∷ FilePath → [FactionTagDecl] → [BaseRelationEntry]
                       → FactionCatalogue → FactionCatalogue
extendFactionCatalogue path decls entries (FactionCatalogue sources) =
    FactionCatalogue (sources ⧺ [FactionSource path decls entries])

-- * Resolving authored tag references

-- | Why an authored tag reference was refused. Carries the offending id
--   so the diagnostic can name it — "some tag in data\/units is wrong"
--   is not something an author can act on.
data FactionTagRejection
    = MalformedFactionTag !Text
      -- ^ fails 'Unit.Faction.Profile.isValidFactionTagText': empty, or
      --   carrying whitespace.
    | UndeclaredFactionTag !Text
      -- ^ syntactically fine, but @data\/factions\/@ never declared it
      --   (D-30). Legal at runtime, illegal in authored data.
    | DuplicateFactionTag !Text
      -- ^ named twice in the same list.
    deriving (Show, Eq)

-- | The short reason phrase a terminal startup diagnostic leads with.
rejectionReason ∷ FactionTagRejection → Text
rejectionReason r = case r of
    MalformedFactionTag  _ → "malformed faction tag id"
    UndeclaredFactionTag _ → "undeclared faction tag"
    DuplicateFactionTag  _ → "duplicate faction tag"

-- | The offending id itself.
rejectionDetail ∷ FactionTagRejection → Text
rejectionDetail r = case r of
    MalformedFactionTag  t → t
    UndeclaredFactionTag t → t
    DuplicateFactionTag  t → t

-- | Resolve one authored @faction_tags:@ list against the catalogue.
--
--   Left on the FIRST offending id in the author's own order, so the
--   reported id is the first one they would find reading the file.
--   Right preserves that order; an empty list resolves to an empty list
--   rather than being rejected, which is what "omitted means no
--   authored defaults" needs (requirement 4).
resolveDeclaredTags ∷ FactionCatalogue → [Text]
                    → Either FactionTagRejection [FactionTag]
resolveDeclaredTags cat = go []
  where
    go acc [] = Right (reverse acc)
    go acc (raw : rest) = case mkFactionTag raw of
        Nothing  → Left (MalformedFactionTag raw)
        Just tag
            | not (declaresTag cat tag) → Left (UndeclaredFactionTag raw)
            | tag `elem` acc            → Left (DuplicateFactionTag raw)
            | otherwise                 → go (tag : acc) rest

-- | Render one ordered pair the way every diagnostic in this family
--   spells it. It lives here, beside the pair map, so the refusal
--   detail and any future authoring tool print a pair identically.
renderTagPair ∷ (FactionTag, FactionTag) → Text
renderTagPair (s, t) = factionTagText s <> " → " <> factionTagText t
