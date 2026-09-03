{-# LANGUAGE Strict, DeriveGeneric, DeriveAnyClass, DerivingStrategies #-}
-- | Generated-language profiles (#710): the native-rendering layer of
--   the world-naming arc (#708), built on top of #709's language-
--   independent semantic meanings. A 'Profile' is a small, deterministic
--   description of one language's naming style — enough to render
--   recognizable, culturally-flavored proper names, never enough for
--   arbitrary sentence grammar (no tense, person, agreement, or clauses).
--
--   Ordering design: the four #709 'Language.Semantic.Types.NameExpr'
--   forms split into two ordering families. @Modifier@ and @Of@ are both
--   descriptive compounds (a modifying element next to a head), so both
--   are governed by 'profCompoundOrder'. @Possessive@ expresses a
--   belongs-to relation instead, with its own independent
--   'profPossessive' order and affix — #710 requirement 11 explicitly
--   allows genitive/possessive constructions to differ from modifier
--   compounds, which this split makes possible.
--
--   This layer is pure: no engine, world, Lua, IO, wall-clock, or
--   floating-point arithmetic (#710 requirement 12) — every derived
--   value comes from integer hashing over an explicit seed, so the same
--   inputs produce byte-identical output on any supported platform.
module Language.Generated.Types
    ( LangSeed(..)
    , langSeedText
    , GeneratorVersion(..)
    , currentGeneratorVersion
    , supportedGeneratorVersions
    , LanguageProvenance(..)
    , CompoundOrder(..)
    , GenitiveOrder(..)
    , Segment(..)
    , SyllableShape(..)
    , PluralMarking(..)
    , PossessiveMarking(..)
    , JoinStyle(..)
    , OnsetRelation(..)
    , emptyOnsetRelation
    , onsetPairs
    , onsetPairCount
    , onsetPairText
    , YRole(..)
    , profileYRole
    , yRoleText
    , BoundaryRule(..)
    , BoundaryRepair(..)
    , BoundaryPolicy(..)
    , boundaryRuleText
    , boundarySegmentText
    , Profile(..)
    , GeneratorError(..)
    , generatorErrorText
    ) where

import UPrelude
import qualified Data.Set as S
import qualified Data.Text as T
import Control.DeepSeq (NFData)
import Data.Serialize (Serialize)
import GHC.Generics (Generic)

-- | The explicit 64-bit seed a language is generated from (#710
--   requirement 1). Still a value of its OWN — nothing here reads a
--   terrain, plate, or gem seed, and no world-generation code reads
--   this one — but no longer unrelated to world seeds in practice:
--   'Language.Suggest.worldLanguageSeed' derives a world's language
--   seed injectively from its normalized numeric world seed (#1106), so
--   changing the seed on the Create World screen changes the language
--   the suggested name is drawn from.
--
--   'Serialize' is derived from the underlying 'Word64' so a world's
--   language provenance can ride into a save (#1092).
newtype LangSeed = LangSeed { langSeedWord ∷ Word64 }
    deriving stock (Show, Eq, Generic)
    deriving newtype (NFData, Serialize)

-- | A seed's decimal text, for surfaces that cannot carry an unsigned
--   64-bit integer losslessly (#1092): a Lua integer is SIGNED 64-bit
--   and a Lua number is a double, so a seed above @2^63-1@ would come
--   back negative (or rounded) through either. Text is exact for the
--   whole 'Word64' range.
langSeedText ∷ LangSeed → Text
langSeedText (LangSeed s) = tshow s

-- | The generator algorithm's version. A future breaking change to
--   profile/root/rendering behavior adds a new version rather than
--   silently changing an existing version's output (#710 requirements
--   2 and 15) — which is what lets a world named by an older generator
--   still be explained after the current version advances (#1092).
newtype GeneratorVersion = GeneratorVersion { generatorVersionInt ∷ Int }
    deriving stock (Show, Eq, Generic)
    deriving newtype (NFData, Serialize)

-- | The version new languages are generated at. Distinct from
--   'supportedGeneratorVersions': advancing this must never make an
--   older world's recorded version unconstructible.
currentGeneratorVersion ∷ GeneratorVersion
currentGeneratorVersion = GeneratorVersion 5

-- | Every version 'Language.Generated.Profile.generateProfile' can
--   build a profile for — historical versions included, since a save
--   may carry any of them (#1092 requirement 4). The dispatcher itself
--   is the implementation authority; this list is what error text and
--   callers enumerate from, and
--   "Test.Headless.Language.Generated" pins the two together (every
--   entry builds, and no other version does).
supportedGeneratorVersions ∷ [GeneratorVersion]
supportedGeneratorVersions =
    [ GeneratorVersion 1, GeneratorVersion 2, GeneratorVersion 3
    , GeneratorVersion 4, GeneratorVersion 5 ]

-- | Which generated language produced a piece of rendered text, and
--   under which generator (#1092). Seed and version are ONE value, so
--   they can never be half-present: a profile is reconstructible from
--   both together or from neither.
data LanguageProvenance = LanguageProvenance
    { lpSeed    ∷ !LangSeed          -- ^ The language's seed.
    , lpVersion ∷ !GeneratorVersion  -- ^ The generator that rendered it.
    } deriving (Show, Eq, Generic, NFData, Serialize)

-- | Ordering for the two compound forms (@Modifier@, @Of@): which side
--   is written first.
data CompoundOrder = ModifierFirst | HeadFirst
    deriving (Show, Eq)

-- | Ordering for the possessive form: whether the marked owner or the
--   possessed head comes first.
data GenitiveOrder = OwnerFirst | HeadFirstGenitive
    deriving (Show, Eq)

-- | One slot in a syllable template.
data Segment = ConsonantSlot | VowelSlot
    deriving (Show, Eq)

newtype SyllableShape = SyllableShape { shapeSegments ∷ [Segment] }
    deriving (Show, Eq)

-- | Explicit-plural marking: a lowercase-letter suffix appended
--   directly to a root (#710 requirement 9 — grammatical marking must
--   never replace the root, only affix it).
newtype PluralMarking = PluralMarking { plmAffix ∷ Text }
    deriving (Show, Eq)

-- | Possessive marking: an order plus an affix appended to the owner's
--   root. The affix is letters only, or a single leading apostrophe
--   followed by letters — never a trailing or repeated mark.
data PossessiveMarking = PossessiveMarking
    { pmOrder ∷ !GenitiveOrder
    , pmAffix ∷ !Text
    } deriving (Show, Eq)

-- | How two independently-generated roots are joined into one
--   compound word. Compact glues them directly; Hyphen always inserts
--   a single separator at the boundary. This is a whole-profile style
--   choice, not a per-word clash-avoidance heuristic — it is one of
--   the things that makes two languages sound different (#710
--   requirement 14).
data JoinStyle = JoinCompact | JoinHyphen
    deriving (Show, Eq)

-- | Which ORDERED consonant pairs a profile admits as a two-consonant
--   syllable onset (#1094 requirement 3). Carried by the profile
--   itself rather than recomputed from 'profSeed' at query time, so
--   the relation is ordinary style state: it participates in
--   'Language.Generated.Signature.profileSignature' and in report
--   introspection exactly like the inventories do.
--
--   Version 1 carries 'emptyOnsetRelation' — its @CCV@ rendering draws
--   both consonants independently and must stay byte-identical
--   (#1094 requirement 1) — so an empty relation is what "this profile
--   constrains nothing" means, NOT "this profile has no answer". The
--   query ('Language.Generated.Onset.admissibleOnset') is total for
--   every constructible profile of every version.
newtype OnsetRelation = OnsetRelation { onsetPairSet ∷ S.Set (Char, Char) }
    deriving (Show, Eq)

emptyOnsetRelation ∷ OnsetRelation
emptyOnsetRelation = OnsetRelation S.empty

-- | The admitted ordered pairs in a canonical ascending order — the
--   representation the renderer indexes into and the signature hashes,
--   so neither depends on construction order.
onsetPairs ∷ OnsetRelation → [(Char, Char)]
onsetPairs = S.toAscList ∘ onsetPairSet

onsetPairCount ∷ OnsetRelation → Int
onsetPairCount = S.size ∘ onsetPairSet

-- | The admitted pairs as one canonical text, each pair contributing
--   exactly two characters. Fixed-width entries make the concatenation
--   unambiguous, so it can be hashed directly without a separator.
onsetPairText ∷ OnsetRelation → Text
onsetPairText r = T.pack (concat [ [a, b] | (a, b) ← onsetPairs r ])

-- | The role a profile gives the letter @y@ (#1094 requirement 6).
--   Exactly three states — a version-2 profile always places @y@ in at
--   least one inventory, so "neither" is deliberately unrepresentable
--   here and surfaces as 'Nothing' for profiles (version 1) that never
--   made the choice.
data YRole = YConsonantOnly | YVowelOnly | YBothRoles
    deriving (Show, Eq)

-- | Read a profile's @y@ role back off its inventories, which are the
--   authority: requirement 6's table IS inventory membership, so the
--   role is derived rather than stored and the two can never disagree.
--   'Nothing' means @y@ is in neither inventory, which only a version-1
--   profile (whose consonant subset may simply not have drawn it) can
--   produce.
profileYRole ∷ Profile → Maybe YRole
profileYRole p = case (isCons, isVow) of
    (True,  True)  → Just YBothRoles
    (True,  False) → Just YConsonantOnly
    (False, True)  → Just YVowelOnly
    (False, False) → Nothing
  where
    isCons = 'y' `elem` profConsonants p
    isVow  = 'y' `elem` profVowels p

-- | A profile's @y@ role as report text.
yRoleText ∷ Profile → Text
yRoleText p = case profileYRole p of
    Just YConsonantOnly → "consonant"
    Just YVowelOnly     → "vowel"
    Just YBothRoles     → "both"
    Nothing             → "none"

-- | Which repair a language applies when a morpheme boundary is not
--   admissible (#1095 requirement 1). Every repair is an INSERTION or a
--   right-morpheme edit — never a change to the left morpheme — which is
--   what makes #710 requirement 9's "the bare root is always a prefix"
--   guarantee survive boundary phonology at the affix sites.
data BoundaryRule
    = BoundaryEpenthetic
      -- ^ Insert the language's own fixed epenthetic segment.
    | BoundaryHarmonic
      -- ^ Assimilation across the boundary: the inserted vowel copies
      --   the left morpheme's own final nucleus (the segment before its
      --   closing consonant) when that is a vowel, and falls back to the
      --   fixed epenthetic vowel when it is not.
    | BoundarySimplifying
      -- ^ Cluster simplification: drop the right morpheme's initial
      --   segment when that both leaves it nonempty and resolves the
      --   boundary, else fall back to epenthesis.
    deriving (Show, Eq)

-- | One language's boundary-repair parameters. The two linking
--   consonants are always DISTINCT, which is what makes a linker
--   guaranteed to differ from the segment it is separating (see
--   'Language.Generated.Boundary.joinMorphemes').
data BoundaryRepair = BoundaryRepair
    { brRule       ∷ !BoundaryRule
    , brEpenthetic ∷ !Char  -- ^ Epenthetic vowel, from 'profVowels'.
    , brLinker     ∷ !Char  -- ^ Linking consonant, from 'profConsonants'.
    , brLinkerAlt  ∷ !Char  -- ^ A second, distinct linking consonant.
    } deriving (Show, Eq)

-- | How a generated language mediates a morpheme boundary (#1095).
--
--   'BoundaryUnmediated' is what "this version predates boundary
--   phonology" means — versions 1 and 2 concatenate morphemes raw and
--   must stay byte-identical (#710 requirement 15, #1092 requirement 4),
--   exactly as 'emptyOnsetRelation' means "this version constrains no
--   onset". The mediation query
--   ('Language.Generated.Boundary.joinMorphemes') is total for every
--   constructible profile of every version.
data BoundaryPolicy
    = BoundaryUnmediated
    | BoundaryMediated !BoundaryRepair
    deriving (Show, Eq)

-- | A policy's rule as report text.
boundaryRuleText ∷ BoundaryPolicy → Text
boundaryRuleText BoundaryUnmediated = "unmediated"
boundaryRuleText (BoundaryMediated rep) = case brRule rep of
    BoundaryEpenthetic   → "epenthetic"
    BoundaryHarmonic     → "harmonic"
    BoundarySimplifying  → "simplifying"

-- | A policy's chosen segments as one canonical text, so the signature
--   can hash them and the report can print them. Empty for an
--   unmediated policy, which carries no segments at all.
boundarySegmentText ∷ BoundaryPolicy → Text
boundarySegmentText BoundaryUnmediated = ""
boundarySegmentText (BoundaryMediated rep) =
    T.pack [brEpenthetic rep, brLinker rep, brLinkerAlt rep]

-- | A generated language's naming style — bounded for proper-name
--   rendering only (#710 requirement 4): enough to fix a phonology and
--   a handful of compounding/marking rules, nothing resembling general
--   sentence grammar.
data Profile = Profile
    { profVersion        ∷ !GeneratorVersion
    , profSeed           ∷ !LangSeed
    , profConsonants     ∷ ![Char]
    , profVowels         ∷ ![Char]
    , profSyllableShapes ∷ ![SyllableShape]
    , profMinSyllables   ∷ !Int
    , profMaxSyllables   ∷ !Int
    , profCompoundOrder  ∷ !CompoundOrder
    , profPossessive     ∷ !PossessiveMarking
    , profPlural         ∷ !PluralMarking
    , profJoin           ∷ !JoinStyle
    , profOnset          ∷ !OnsetRelation
    , profBoundary       ∷ !BoundaryPolicy
    } deriving (Show, Eq)

-- | Why a generated language could not be produced for a requested
--   (version, seed).
--
--   Two failures, and they fail at different moments. A version this
--   build never defined is rejected by
--   'Language.Generated.Profile.generateProfile' before a profile
--   exists at all. A profile that DOES build can still be unusable:
--   'Language.Generated.Root.assignRoots' needs one distinct root per
--   catalogue concept, and a small enough phonology simply cannot
--   render that many (#2206), which is rejected before assignment
--   begins rather than discovered as an endless collision reroll.
--
--   No 'Generic'\/'Serialize' instance, deliberately: this type never
--   rides into a save, so adding a constructor is outside
--   @tools/enum_append_only_audit.py@'s append-only rule.
data GeneratorError
    = UnsupportedGeneratorVersion Int
    | InsufficientRootSpace !GeneratorVersion !LangSeed !Int !Int
      -- ^ The version and seed of the offending profile, the EXACT
      --   number of distinct case-insensitive roots its own production
      --   rules can render, and the number of concepts needing one.
    deriving (Show, Eq)

generatorErrorText ∷ GeneratorError → Text
generatorErrorText (InsufficientRootSpace ver seed capacity required) =
    "language-generator version " <> tshow (generatorVersionInt ver)
    <> " seed " <> langSeedText seed
    <> " cannot name the concept catalogue: its root space holds "
    <> tshow capacity <> " distinct roots for "
    <> tshow required <> " concepts (shortfall "
    <> tshow (required - capacity) <> ")"
generatorErrorText (UnsupportedGeneratorVersion v) =
    "unsupported language-generator version " <> tshow v
    <> " (supported: " <> supported <> ")"
  where
    -- Enumerates every DEFINED version, not just the current one: once
    -- historical versions coexist (#1092 requirement 4), naming only
    -- the current one would mislabel a perfectly constructible version
    -- as the sole supported one.
    supported = T.intercalate ", "
        [ tshow (generatorVersionInt g) | g ← supportedGeneratorVersions ]
