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
    , shapeLength
    , PluralMarking(..)
    , PossessiveMarking(..)
    , JoinStyle(..)
    , Profile(..)
    , GeneratorError(..)
    , generatorErrorText
    ) where

import UPrelude
import qualified Data.Text as T
import Data.Serialize (Serialize)
import GHC.Generics (Generic)

-- | The explicit 64-bit seed a language is generated from (#710
--   requirement 1). Distinct from any world-generation seed — a
--   language seed has no relationship to terrain/plate seeds.
--
--   'Serialize' is derived from the underlying 'Word64' so a world's
--   language provenance can ride into a save (#1092).
newtype LangSeed = LangSeed { langSeedWord ∷ Word64 }
    deriving (Show, Eq)
    deriving newtype (Serialize)

-- | A seed's decimal text, for surfaces that cannot carry an unsigned
--   64-bit integer losslessly (#1092): a Lua integer is SIGNED 64-bit
--   and a Lua number is a double, so a seed above @2^63-1@ would come
--   back negative (or rounded) through either. Text is exact for the
--   whole 'Word64' range.
langSeedText ∷ LangSeed → Text
langSeedText (LangSeed s) = T.pack (show s)

-- | The generator algorithm's version. A future breaking change to
--   profile/root/rendering behavior adds a new version rather than
--   silently changing an existing version's output (#710 requirements
--   2 and 15) — which is what lets a world named by an older generator
--   still be explained after the current version advances (#1092).
newtype GeneratorVersion = GeneratorVersion { generatorVersionInt ∷ Int }
    deriving (Show, Eq)
    deriving newtype (Serialize)

-- | The version new languages are generated at. Distinct from
--   'supportedGeneratorVersions': advancing this must never make an
--   older world's recorded version unconstructible.
currentGeneratorVersion ∷ GeneratorVersion
currentGeneratorVersion = GeneratorVersion 1

-- | Every version 'Language.Generated.Profile.generateProfile' can
--   build a profile for — historical versions included, since a save
--   may carry any of them (#1092 requirement 4). The dispatcher itself
--   is the implementation authority; this list is what error text and
--   callers enumerate from, and
--   "Test.Headless.Language.Generated" pins the two together (every
--   entry builds, and no other version does).
supportedGeneratorVersions ∷ [GeneratorVersion]
supportedGeneratorVersions = [GeneratorVersion 1]

-- | Which generated language produced a piece of rendered text, and
--   under which generator (#1092). Seed and version are ONE value, so
--   they can never be half-present: a profile is reconstructible from
--   both together or from neither.
data LanguageProvenance = LanguageProvenance
    { lpSeed    ∷ !LangSeed          -- ^ The language's seed.
    , lpVersion ∷ !GeneratorVersion  -- ^ The generator that rendered it.
    } deriving (Show, Eq, Generic, Serialize)

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

shapeLength ∷ SyllableShape → Int
shapeLength = length ∘ shapeSegments

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
    } deriving (Show, Eq)

-- | Why a profile could not be generated for a requested version.
data GeneratorError = UnsupportedGeneratorVersion Int
    deriving (Show, Eq)

generatorErrorText ∷ GeneratorError → Text
generatorErrorText (UnsupportedGeneratorVersion v) =
    "unsupported language-generator version " <> T.pack (show v)
    <> " (supported: " <> supported <> ")"
  where
    -- Enumerates every DEFINED version, not just the current one: once
    -- historical versions coexist (#1092 requirement 4), naming only
    -- the current one would mislabel a perfectly constructible version
    -- as the sole supported one.
    supported = T.intercalate ", "
        [ T.pack (show (generatorVersionInt g)) | g ← supportedGeneratorVersions ]
