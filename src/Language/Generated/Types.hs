{-# LANGUAGE Strict, UnicodeSyntax #-}
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
    , GeneratorVersion(..)
    , currentGeneratorVersion
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

-- | The explicit 64-bit seed a language is generated from (#710
--   requirement 1). Distinct from any world-generation seed — a
--   language seed has no relationship to terrain/plate seeds.
newtype LangSeed = LangSeed { langSeedWord ∷ Word64 }
    deriving (Show, Eq)

-- | The generator algorithm's version. Only version 1 is defined; a
--   future breaking change to profile/root/rendering behavior adds a
--   new version rather than silently changing version 1's output
--   (#710 requirements 2 and 15).
newtype GeneratorVersion = GeneratorVersion { generatorVersionInt ∷ Int }
    deriving (Show, Eq)

currentGeneratorVersion ∷ GeneratorVersion
currentGeneratorVersion = GeneratorVersion 1

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
    <> " (only version " <> T.pack (show (generatorVersionInt currentGeneratorVersion))
    <> " is supported)"
