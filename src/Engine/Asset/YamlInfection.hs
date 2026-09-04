{-# LANGUAGE Strict, DeriveGeneric #-}
-- | YAML loader for data/infections/*.yaml. Mirrors Engine.Asset.YamlSubstance.
--   The on-disk schema is documented in data/infections/bacteria.yaml.
--
--   Every governed field is checked HERE, at the authoring boundary, and
--   nowhere else (#2346). The consumers all assume the domains that file
--   header documents and none of them can defend itself:
--
--     * 'Combat.Wounds.Infection.selectInfectionType' sums
--       @base_weight × climateMatchWeight@, draws @randomR (0, total)@
--       and subtracts cumulative weights, so a NEGATIVE weight biases
--       the draw toward its neighbour and a NaN one makes @total@ NaN,
--       @total ≤ 0@ false, and the whole pick degenerate.
--     * @bandMatch@ treats a climate band as ORDERED; an inverted band
--       matches nothing and falls off in both directions at once.
--     * @aggressiveness@ and @infectability@ multiply Ticker-A growth,
--       and @cure_rate@ multiplies the antibiotic reduction AFTER the
--       capability clamp — so a negative cure rate makes a dose worsen
--       the infection it was meant to treat.
--     * @site@ and @curable_by@ are matched by EXACT token, so one
--       misspelling silently drops a definition out of every selection
--       pool, or makes it permanently incurable, with no diagnostic —
--       the same silent drop #2315 rejects for flora.
--
--   A rejection fails the DECODE, which is what makes it whole-FILE:
--   'Engine.Asset.YamlList.loadYamlListOutcome' hands back 'Nothing',
--   'Engine.Scripting.Lua.API.Infection.loadInfectionYamlFn' registers
--   nothing from that file, and the startup loader treats it exactly as
--   it treats a parse failure (#2203). Per-definition skipping is
--   deliberately NOT an option: it would leave a file half-registered
--   with no way for an author to tell.
module Engine.Asset.YamlInfection
    ( InfectionYamlDef(..)
    , InfectionYamlFile(..)
    , loadInfectionYaml
    , loadInfectionYamlOutcome
    , categoryVocabulary
    , siteVocabulary
    , treatmentVocabulary
    ) where

import UPrelude
import GHC.Generics (Generic)
import qualified Data.Text as T
import Data.Aeson (FromJSON(..), (.:), withObject)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KM
import qualified Data.Aeson.Types as Aeson (Parser)
import Engine.Core.Log (LoggerState)
import Engine.Core.Yaml.Scalar (nonFiniteSpelling)
import Engine.Asset.YamlList (loadYamlListOutcome)

-- | YAML shape for one infection entry. Only `id` + `name` + `category`
--   are required; everything else has a sensible default so a terse entry
--   still loads.
data InfectionYamlDef = InfectionYamlDef
    { iyId            ∷ !Text
    , iyName          ∷ !Text
    , iyIcon          ∷ !Text
    , iyCategory      ∷ !Text
    , iySites         ∷ ![Text]
    , iyBaseWeight    ∷ !Float
    , iyTempMin       ∷ !Float
    , iyTempMax       ∷ !Float
    , iyMoistMin      ∷ !Float
    , iyMoistMax      ∷ !Float
    , iyAggressiveness ∷ !Float
    , iyInfectability  ∷ !Float
    , iyCurableBy     ∷ ![Text]
    , iyCureRate      ∷ !Float
    , iyWoundInfectable ∷ !Bool
    , iyEffects       ∷ ![Text]
    , iyTransmissibility ∷ !Float
    , iyTransmission  ∷ ![Text]
    } deriving (Show, Eq, Generic)

-- * Vocabularies
--
--   Exported so a consumer or a spec can name the same closed set this
--   boundary enforces rather than restating it.

-- | The five categories @data/infections/bacteria.yaml@'s header
--   documents. Only @bacterial@ is wired to a cure today; the rest are
--   on record so the schema is exercised, which is why the whole set is
--   accepted rather than just the wired one.
categoryVocabulary ∷ [Text]
categoryVocabulary = ["bacterial", "parasitic", "fungal", "viral", "prion"]

-- | The two wound-site classes 'Combat.Wounds.Infection.woundSiteClass'
--   can ever produce, and therefore the only two tokens
--   @infectionsForSite@ can ever match. Anything else silently removes
--   the definition from every pool.
siteVocabulary ∷ [Text]
siteVocabulary = ["surface", "deep"]

-- | Every treatment the engine actually implements. Today that is the
--   single item name 'Engine.Scripting.Lua.API.Units.Medical' matches
--   @curable_by@ against; an unrecognised token makes the infection
--   incurable with no diagnostic, so it is rejected here instead.
treatmentVocabulary ∷ [Text]
treatmentVocabulary = ["antibiotics"]

-- | The band a definition that authors no @climate:@ block inherits:
--   the full plausible range, unchanged from before this domain check.
defaultTempBand, defaultMoistBand ∷ (Float, Float)
defaultTempBand  = (-50, 50)
defaultMoistBand = (0, 1)

-- * The authoring boundary

instance FromJSON InfectionYamlDef where
    parseJSON = withObject "InfectionYamlDef" $ \v → do
        -- `id` is read FIRST, monadically, because every rejection
        -- below names the DEFINITION it rejects and the applicative
        -- chain cannot pass an already-parsed field to a later one.
        -- Naming it is the whole point of the named parsers: the
        -- loader's own warning supplies the FILE, but aeson alone only
        -- reaches for a path like `$.infections[4].cure_rate` — an
        -- index nobody can map back to a definition without counting
        -- entries (the shape Engine.Asset.YamlFlora reads `name` in).
        ident ← v .: "id"
        (tMin, tMax, mMin, mMax) ← requireClimate ident v
        InfectionYamlDef ident
            ⊚ v .: "name"
            ⊛ optionalText ident "icon" "bacterial_infection" v
            ⊛ requireCategory ident v
            ⊛ requireTokens ident "site" siteVocabulary v
            ⊛ requireNonNegative ident "base_weight" 1.0 v
            ⊛ pure tMin
            ⊛ pure tMax
            ⊛ pure mMin
            ⊛ pure mMax
            ⊛ requireNonNegative ident "aggressiveness" 1.0 v
            ⊛ requireNonNegative ident "infectability" 1.0 v
            ⊛ requireTokens ident "curable_by" treatmentVocabulary v
            ⊛ requireNonNegative ident "cure_rate" 1.0 v
            ⊛ optionalBool ident "wound_infectable" True v
            ⊛ freeFormTokens ident "effects" v
            ⊛ requireNonNegative ident "transmissibility" 0.0 v
            ⊛ freeFormTokens ident "transmission" v

-- | The presence rule every optional key below is read through: a key
--   that is ABSENT takes its documented default, and a key that is
--   PRESENT is parsed, whatever it holds.
--
--   Aeson's @.:?@ collapses those two — it reads @cure_rate: null@ as
--   an omission and quietly substitutes @1.0@ — which is exactly the
--   present-versus-absent confusion #1191 rejects. An author who wrote
--   the key meant something by it, so a null there is malformed, not a
--   default.
presentKey ∷ Text → Aeson.Object → Maybe Aeson.Value
presentKey field v = KM.lookup (Key.fromText field) v

-- | Fail the whole file's decode, naming the definition. The loader
--   wrapper supplies the file path around this; together they name the
--   file, the id, the field, and the offending value (requirement 4).
--
--   The message is deliberately ASCII: 'Data.Yaml.ParseException'
--   renders an aeson failure through 'show', which escapes a non-ASCII
--   character into a numeric escape an author then has to decode.
reject ∷ Text → Text → Aeson.Parser α
reject ident why = fail ∘ T.unpack $ "infection '" <> ident <> "': " <> why

-- | How a rejected value is quoted back. A number is rendered from the
--   'Scientific' the document actually held rather than from whatever
--   it narrowed to, so an overflowing literal is reported as written.
renderValue ∷ Aeson.Value → Text
renderValue val = case val of
    Aeson.Number s → tshow s
    Aeson.String t → t
    Aeson.Bool b   → if b then "true" else "false"
    Aeson.Null     → "null"
    Aeson.Array _  → "a list"
    Aeson.Object _ → "a block"

-- | One finite number narrowed to the engine's 32-bit 'Float'. @what@
--   names the thing being read (a field, or one bound of a band).
--
--   Two spellings have to be caught that a plain @Float@ leaf cannot
--   diagnose. YAML's scalar resolver hands @.nan@ and @.inf@ over as
--   STRINGS (see 'Engine.Core.Yaml.Scalar'), so decoding to 'Float'
--   first would surface them as a type error naming neither the
--   definition nor what was wrong. And the finiteness check still has
--   to run AFTER narrowing, because a perfectly ordinary @1.0e100@ is a
--   valid 'Scientific' that becomes @Infinity@ in the 'Float' field the
--   engine multiplies with.
finiteNumber ∷ Text → Text → Aeson.Value → Aeson.Parser Float
finiteNumber ident what val = case val of
    Aeson.Number s →
        let f = realToFrac s ∷ Float
        in if isNaN f ∨ isInfinite f
             then reject ident (what <> " must be finite, got " <> tshow s)
             else pure f
    Aeson.String t
        | isJust (nonFiniteSpelling t) →
            reject ident (what <> " must be finite, got " <> t)
    _ → reject ident (what <> " must be a number, got " <> renderValue val)

-- | A governed multiplier or weight: finite and @>= 0@ after narrowing.
--   Absent keeps @def@; present and out of domain fails the file.
requireNonNegative ∷ Text → Text → Float → Aeson.Object → Aeson.Parser Float
requireNonNegative ident field def v = case presentKey field v of
    Nothing  → pure def
    Just val → do
        f ← finiteNumber ident field val
        if f < 0
          then reject ident (field <> " must be >= 0, got " <> tshow f)
          else pure f

-- | The optional @climate:@ block, flattened to the four bounds the
--   definition record carries. An absent block leaves both bands at
--   their documented full range.
requireClimate
    ∷ Text → Aeson.Object → Aeson.Parser (Float, Float, Float, Float)
requireClimate ident v = case presentKey "climate" v of
    Nothing → pure (tLo, tHi, mLo, mHi)
    Just (Aeson.Object c) → do
        (t0, t1) ← band ident "climate temp" "temp" defaultTempBand Nothing c
        (m0, m1) ← band ident "climate moisture" "moisture" defaultMoistBand
                        (Just defaultMoistBand) c
        pure (t0, t1, m0, m1)
    Just other → reject ident
        ("climate must be a block of temp and moisture bands, got "
          <> renderValue other)
  where
    (tLo, tHi) = defaultTempBand
    (mLo, mHi) = defaultMoistBand

-- | One climate band: EXACTLY two finite bounds, in order, optionally
--   confined to @mBounds@.
--
--   The shape check is the point. The @pair@ helper this replaces
--   RESHAPED whatever it was given — @[a, b, c]@ silently truncated,
--   @[a]@ silently borrowed the default upper bound, @[]@ silently
--   became the full range — so a band an author got wrong loaded as a
--   band they never wrote.
band ∷ Text → Text → Text → (Float, Float) → Maybe (Float, Float)
     → Aeson.Object → Aeson.Parser (Float, Float)
band ident what key def mBounds c = case presentKey key c of
    Nothing  → pure def
    Just val → case val of
        Aeson.Array _ → do
            xs ← parseJSON val ∷ Aeson.Parser [Aeson.Value]
            case xs of
                [loV, hiV] → do
                    lo ← finiteNumber ident (what <> " min") loV
                    hi ← finiteNumber ident (what <> " max") hiV
                    when (lo > hi) $ reject ident
                        (what <> " must be ordered min <= max, got ["
                          <> tshow lo <> ", " <> tshow hi <> "]")
                    forM_ mBounds $ \(blo, bhi) →
                        forM_ [("min" ∷ Text, lo), ("max", hi)] $ \(nm, x) →
                            when (x < blo ∨ x > bhi) $ reject ident
                                (what <> " " <> nm <> " must lie within ["
                                  <> tshow blo <> ", " <> tshow bhi
                                  <> "], got " <> tshow x)
                    pure (lo, hi)
                _ → reject ident
                        (what <> " must be exactly two numbers [min, max], \
                         \got " <> tshow (length xs) <> " entries")
        _ → reject ident
                (what <> " must be exactly two numbers [min, max], got "
                  <> renderValue val)

-- | A list of tokens drawn from a CLOSED vocabulary. Absent is the
--   empty list, exactly as before; a present list is checked token by
--   token.
requireTokens ∷ Text → Text → [Text] → Aeson.Object → Aeson.Parser [Text]
requireTokens ident field vocab v = case presentKey field v of
    Nothing  → pure []
    Just val → do
        ts ← textList ident field val
        forM ts $ \t →
            if t `elem` vocab
              then pure t
              else reject ident (field <> " must be one of "
                     <> T.intercalate ", " vocab <> "; got " <> tshow t)

-- | A list of tokens from an OPEN vocabulary (@effects@,
--   @transmission@), which the file header documents as free-form and
--   this change deliberately leaves free-form. Only the SHAPE is
--   checked, so @effects: null@ is still the malformed presence
--   'presentKey' exists to catch.
freeFormTokens ∷ Text → Text → Aeson.Object → Aeson.Parser [Text]
freeFormTokens ident field v = case presentKey field v of
    Nothing  → pure []
    Just val → textList ident field val

textList ∷ Text → Text → Aeson.Value → Aeson.Parser [Text]
textList ident field val = case val of
    Aeson.Array _ → do
        vs ← parseJSON val ∷ Aeson.Parser [Aeson.Value]
        forM vs $ \x → case x of
            Aeson.String t → pure t
            _ → reject ident (field <> " entries must be tokens, got "
                               <> renderValue x)
    _ → reject ident (field <> " must be a list of tokens, got "
                       <> renderValue val)

-- | The required @category@, checked against its closed vocabulary.
--   Read through 'presentKey' rather than @.:@ so a missing one is
--   diagnosed by definition id like everything else here.
requireCategory ∷ Text → Aeson.Object → Aeson.Parser Text
requireCategory ident v = case presentKey "category" v of
    Nothing → reject ident "category is required and has no default"
    Just (Aeson.String t)
        | t `elem` categoryVocabulary → pure t
    Just other → reject ident
        ("category must be one of " <> T.intercalate ", " categoryVocabulary
          <> "; got " <> quoted other)
  where
    quoted x = case x of
        Aeson.String t → tshow t
        _              → renderValue x

optionalText ∷ Text → Text → Text → Aeson.Object → Aeson.Parser Text
optionalText ident field def v = case presentKey field v of
    Nothing               → pure def
    Just (Aeson.String t) → pure t
    Just other            → reject ident
        (field <> " must be text, got " <> renderValue other)

optionalBool ∷ Text → Text → Bool → Aeson.Object → Aeson.Parser Bool
optionalBool ident field def v = case presentKey field v of
    Nothing             → pure def
    Just (Aeson.Bool b) → pure b
    Just other          → reject ident
        (field <> " must be true or false, got " <> renderValue other)

newtype InfectionYamlFile = InfectionYamlFile
    { iyfInfections ∷ [InfectionYamlDef]
    } deriving (Show, Eq, Generic)

instance FromJSON InfectionYamlFile where
    parseJSON = withObject "InfectionYamlFile" $ \v → InfectionYamlFile
        ⊚ v .: "infections"

-- | 'loadInfectionYaml' with the decode OUTCOME kept (#2203):
--   'Nothing' is a parse failure, @Just xs@ a file that decoded
--   (possibly to an empty list). The startup loader needs the two
--   apart; every other caller reads 'loadInfectionYaml'.
loadInfectionYamlOutcome
    ∷ LoggerState → FilePath → IO (Maybe [InfectionYamlDef])
loadInfectionYamlOutcome logger =
    loadYamlListOutcome logger "infection" "infections" iyfInfections

loadInfectionYaml ∷ LoggerState → FilePath → IO [InfectionYamlDef]
loadInfectionYaml logger path =
    fromMaybe [] ⊚ loadInfectionYamlOutcome logger path
