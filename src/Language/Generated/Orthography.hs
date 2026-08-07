{-# LANGUAGE Strict #-}
-- | Per-language orthographic conventions beyond ASCII (#1100): the ONE
--   canonical statement of which extended letters a generated language
--   may draw, what their uppercase forms are, and therefore exactly
--   which characters can ever appear in a generated name.
--
--   The design principle the issue sets, and the reason this is an
--   inventory rather than a substitution pass: an accent must be part of
--   a language's phoneme inventory, never sprinkled onto finished
--   output. A profile that has @á@ draws it exactly as it draws @a@ —
--   through the same syllable shapes, the same onset relation (#1094),
--   the same boundary repair (#1095), and the same bound-form ladder
--   (#1096). Nothing anywhere edits a rendered name to add a mark, which
--   is what makes one world's names carry a consistent signature a
--   player can actually perceive.
--
--   Three rules keep the repertoire honest, and each one excludes real
--   characters that would otherwise be tempting:
--
--   * __Precomposed single code points only.__ Every extended letter is
--     one 'Char' with a one-code-point uppercase, because 'Profile'
--     stores @[Char]@ inventories and
--     'Language.Generated.Render.capitalizeWord' uppercases one 'Char'.
--     Combining sequences are out of scope, so @a@ + @U+0301@ is not a
--     member and a name containing one is a contract violation.
--   * __A diacritic marks a base sound the language already has.__ Every
--     member is an ASCII letter plus one mark, and a language may only
--     draw it when that ASCII letter is already in the matching
--     inventory. So the separate letters @þ ð æ œ ĳ@ — which are not
--     any ASCII letter with a mark — are deliberately absent.
--   * __@y@ is never a base.__ From version 2 on, @y@'s role is drawn
--     explicitly and it can sit in BOTH inventories (#1094 requirement
--     6). A marked @ý@ would inherit that ambiguity through a completely
--     different mechanism, giving a second dual-role letter nothing in
--     'Language.Generated.Onset.consonantOnly' was designed around. It
--     costs three characters to exclude and removes the whole question.
--
--   Font safety (#1100 requirement 3) is settled by 'nameFontPolicy'
--   over in "Engine.Graphics.Font.Repertoire": generated names are
--   displayed in the extended-Latin fonts only, and every character
--   'outputInventory' lists is proved to rasterize from each of them by
--   "Test.Headless.Language.Generated"'s font-coverage group. Adding a
--   member here without that proof passing is what the test exists to
--   stop.
module Language.Generated.Orthography
    ( -- * Diacritic families
      DiacriticFamily(..)
    , diacriticFamilies
    , diacriticFamilyText
    , familyMarks
      -- * The canonical repertoire
    , extendedLetterTable
    , extendedLetters
    , extendedUppercaseOf
    , isExtendedLetter
    , isNameLetter
    , isNameInitial
    , nameMarks
    , outputInventory
      -- * Per-language selection
    , maxMarksPerInventory
    , extendedInventory
    , extendedOrthographyVersion
      -- * Reading a profile back
    , profileExtendedChars
    , profileDiacritic
    , profileDiacriticText
    ) where

import UPrelude
import Data.Char (isAsciiUpper, isAsciiLower)
import Data.List (sort)
import Language.Generated.Types
import Language.Generated.Hash (draw, pickIndex, shuffleBy, wordInRange)

-- * Diacritic families

-- | The mark a language's extended letters all carry.
--
--   A language draws ONE family, which is what turns its accented
--   letters into a convention rather than a scattering: a world whose
--   names show @č@ also shows @š@ and @ř@, never @č@ beside @ő@. The
--   family is not stored on the 'Profile' — it is read back off the
--   inventories by 'profileDiacritic', exactly as
--   'Language.Generated.Types.profileYRole' reads @y@'s role back,
--   because inventory membership IS the fact and a stored copy could
--   disagree with it.
data DiacriticFamily
    = DiaAcute        -- ^ @á é í ó ú ć ĺ ń ŕ ś ź@
    | DiaGrave        -- ^ @à è ì ò ù@
    | DiaCircumflex   -- ^ @â ê î ô û ĉ ĝ ĥ ĵ ŝ ŵ@
    | DiaDiaeresis    -- ^ @ä ë ï ö ü@
    | DiaCaron        -- ^ @ě č ď ľ ň ř š ť ž@
    | DiaMacron       -- ^ @ā ē ī ō ū@
    | DiaBreve        -- ^ @ă ğ ŭ@
    | DiaOgonek       -- ^ @ą ę@
    | DiaCedilla      -- ^ @ç ş ţ@
    | DiaRing         -- ^ @å ů@
    | DiaDoubleAcute  -- ^ @ő ű@
    | DiaStroke       -- ^ @ø đ ł@
    deriving (Show, Eq, Ord, Enum, Bounded)

-- | Every family, in a fixed order. The order is load bearing: a
--   version-5 profile selects its family by index into this list, so
--   reordering it re-renders every existing version-5 language.
diacriticFamilies ∷ [DiacriticFamily]
diacriticFamilies = [minBound .. maxBound]

diacriticFamilyText ∷ DiacriticFamily → Text
diacriticFamilyText f = case f of
    DiaAcute       → "acute"
    DiaGrave       → "grave"
    DiaCircumflex  → "circumflex"
    DiaDiaeresis   → "diaeresis"
    DiaCaron       → "caron"
    DiaMacron      → "macron"
    DiaBreve       → "breve"
    DiaOgonek      → "ogonek"
    DiaCedilla     → "cedilla"
    DiaRing        → "ring"
    DiaDoubleAcute → "double-acute"
    DiaStroke      → "stroke"

-- * The canonical repertoire

-- | THE repertoire, as @(family, slot, base, lowercase, uppercase)@.
--
--   Written out with explicit code points rather than literal glyphs so
--   a reviewer can check a row without trusting their terminal's font,
--   and so a homoglyph cannot be smuggled in. @slot@ says which
--   inventory the letter joins — it follows from the base letter (the
--   ASCII vowel pool is @aeiou@ and the consonant pool holds the rest),
--   and stating it here keeps the classification out of a second module
--   that could disagree.
--
--   Every row is present in BOTH extended-Latin fonts' real cmaps with a
--   non-empty outline; the font-coverage test re-derives that from the
--   shipped @.ttf@ files rather than trusting this comment.
extendedLetterTable ∷ [(DiacriticFamily, Segment, Char, Char, Char)]
extendedLetterTable =
    [ (DiaAcute, VowelSlot,     'a', '\x00E1', '\x00C1')  -- á Á
    , (DiaAcute, VowelSlot,     'e', '\x00E9', '\x00C9')  -- é É
    , (DiaAcute, VowelSlot,     'i', '\x00ED', '\x00CD')  -- í Í
    , (DiaAcute, VowelSlot,     'o', '\x00F3', '\x00D3')  -- ó Ó
    , (DiaAcute, VowelSlot,     'u', '\x00FA', '\x00DA')  -- ú Ú
    , (DiaAcute, ConsonantSlot, 'c', '\x0107', '\x0106')  -- ć Ć
    , (DiaAcute, ConsonantSlot, 'l', '\x013A', '\x0139')  -- ĺ Ĺ
    , (DiaAcute, ConsonantSlot, 'n', '\x0144', '\x0143')  -- ń Ń
    , (DiaAcute, ConsonantSlot, 'r', '\x0155', '\x0154')  -- ŕ Ŕ
    , (DiaAcute, ConsonantSlot, 's', '\x015B', '\x015A')  -- ś Ś
    , (DiaAcute, ConsonantSlot, 'z', '\x017A', '\x0179')  -- ź Ź

    , (DiaGrave, VowelSlot,     'a', '\x00E0', '\x00C0')  -- à À
    , (DiaGrave, VowelSlot,     'e', '\x00E8', '\x00C8')  -- è È
    , (DiaGrave, VowelSlot,     'i', '\x00EC', '\x00CC')  -- ì Ì
    , (DiaGrave, VowelSlot,     'o', '\x00F2', '\x00D2')  -- ò Ò
    , (DiaGrave, VowelSlot,     'u', '\x00F9', '\x00D9')  -- ù Ù

    , (DiaCircumflex, VowelSlot,     'a', '\x00E2', '\x00C2')  -- â Â
    , (DiaCircumflex, VowelSlot,     'e', '\x00EA', '\x00CA')  -- ê Ê
    , (DiaCircumflex, VowelSlot,     'i', '\x00EE', '\x00CE')  -- î Î
    , (DiaCircumflex, VowelSlot,     'o', '\x00F4', '\x00D4')  -- ô Ô
    , (DiaCircumflex, VowelSlot,     'u', '\x00FB', '\x00DB')  -- û Û
    , (DiaCircumflex, ConsonantSlot, 'c', '\x0109', '\x0108')  -- ĉ Ĉ
    , (DiaCircumflex, ConsonantSlot, 'g', '\x011D', '\x011C')  -- ĝ Ĝ
    , (DiaCircumflex, ConsonantSlot, 'h', '\x0125', '\x0124')  -- ĥ Ĥ
    , (DiaCircumflex, ConsonantSlot, 'j', '\x0135', '\x0134')  -- ĵ Ĵ
    , (DiaCircumflex, ConsonantSlot, 's', '\x015D', '\x015C')  -- ŝ Ŝ
    , (DiaCircumflex, ConsonantSlot, 'w', '\x0175', '\x0174')  -- ŵ Ŵ

    , (DiaDiaeresis, VowelSlot, 'a', '\x00E4', '\x00C4')  -- ä Ä
    , (DiaDiaeresis, VowelSlot, 'e', '\x00EB', '\x00CB')  -- ë Ë
    , (DiaDiaeresis, VowelSlot, 'i', '\x00EF', '\x00CF')  -- ï Ï
    , (DiaDiaeresis, VowelSlot, 'o', '\x00F6', '\x00D6')  -- ö Ö
    , (DiaDiaeresis, VowelSlot, 'u', '\x00FC', '\x00DC')  -- ü Ü

    , (DiaCaron, VowelSlot,     'e', '\x011B', '\x011A')  -- ě Ě
    , (DiaCaron, ConsonantSlot, 'c', '\x010D', '\x010C')  -- č Č
    , (DiaCaron, ConsonantSlot, 'd', '\x010F', '\x010E')  -- ď Ď
    , (DiaCaron, ConsonantSlot, 'l', '\x013E', '\x013D')  -- ľ Ľ
    , (DiaCaron, ConsonantSlot, 'n', '\x0148', '\x0147')  -- ň Ň
    , (DiaCaron, ConsonantSlot, 'r', '\x0159', '\x0158')  -- ř Ř
    , (DiaCaron, ConsonantSlot, 's', '\x0161', '\x0160')  -- š Š
    , (DiaCaron, ConsonantSlot, 't', '\x0165', '\x0164')  -- ť Ť
    , (DiaCaron, ConsonantSlot, 'z', '\x017E', '\x017D')  -- ž Ž

    , (DiaMacron, VowelSlot, 'a', '\x0101', '\x0100')  -- ā Ā
    , (DiaMacron, VowelSlot, 'e', '\x0113', '\x0112')  -- ē Ē
    , (DiaMacron, VowelSlot, 'i', '\x012B', '\x012A')  -- ī Ī
    , (DiaMacron, VowelSlot, 'o', '\x014D', '\x014C')  -- ō Ō
    , (DiaMacron, VowelSlot, 'u', '\x016B', '\x016A')  -- ū Ū

    , (DiaBreve, VowelSlot,     'a', '\x0103', '\x0102')  -- ă Ă
    , (DiaBreve, VowelSlot,     'u', '\x016D', '\x016C')  -- ŭ Ŭ
    , (DiaBreve, ConsonantSlot, 'g', '\x011F', '\x011E')  -- ğ Ğ

    , (DiaOgonek, VowelSlot, 'a', '\x0105', '\x0104')  -- ą Ą
    , (DiaOgonek, VowelSlot, 'e', '\x0119', '\x0118')  -- ę Ę

    , (DiaCedilla, ConsonantSlot, 'c', '\x00E7', '\x00C7')  -- ç Ç
    , (DiaCedilla, ConsonantSlot, 's', '\x015F', '\x015E')  -- ş Ş
    , (DiaCedilla, ConsonantSlot, 't', '\x0163', '\x0162')  -- ţ Ţ

    , (DiaRing, VowelSlot, 'a', '\x00E5', '\x00C5')  -- å Å
    , (DiaRing, VowelSlot, 'u', '\x016F', '\x016E')  -- ů Ů

    , (DiaDoubleAcute, VowelSlot, 'o', '\x0151', '\x0150')  -- ő Ő
    , (DiaDoubleAcute, VowelSlot, 'u', '\x0171', '\x0170')  -- ű Ű

    , (DiaStroke, VowelSlot,     'o', '\x00F8', '\x00D8')  -- ø Ø
    , (DiaStroke, ConsonantSlot, 'd', '\x0111', '\x0110')  -- đ Đ
    , (DiaStroke, ConsonantSlot, 'l', '\x0142', '\x0141')  -- ł Ł
    ]

-- | One family's @(base, marked)@ pairs for one inventory, in the
--   table's own order — the order a profile's mark selection shuffles,
--   so it is load bearing the same way 'diacriticFamilies' is.
familyMarks ∷ DiacriticFamily → Segment → [(Char, Char)]
familyMarks fam slot =
    [ (base, lower)
    | (f, s, base, lower, _) ← extendedLetterTable, f ≡ fam, s ≡ slot ]

-- | Every extended lowercase letter, ascending. A language draws a
--   subset of this; no generated name may contain a lowercase letter
--   outside it and ASCII.
extendedLetters ∷ [Char]
extendedLetters = sort [ lower | (_, _, _, lower, _) ← extendedLetterTable ]

-- | The single-code-point uppercase of an extended letter.
--
--   The table is the authority rather than 'Data.Char.toUpper': a
--   rendered name's initial goes through @toUpper@, and the test suite
--   pins the two to agree, so the repertoire cannot drift from what
--   rendering actually produces.
extendedUppercaseOf ∷ Char → Maybe Char
extendedUppercaseOf c =
    case [ upper | (_, _, _, lower, upper) ← extendedLetterTable, lower ≡ c ] of
        (u : _) → Just u
        []      → Nothing

-- | Whether a character is an extended letter in either case.
isExtendedLetter ∷ Char → Bool
isExtendedLetter c = any match extendedLetterTable
  where match (_, _, _, lower, upper) = c ≡ lower ∨ c ≡ upper

-- | Whether a character is a LETTER for the purposes of every rule that
--   used to say "ASCII letter" — #1095's triple-run detection and
--   #1096's "a bound form retains a visible letter".
--
--   Widening those predicates cannot disturb versions 1-4: their
--   inventories are ASCII, so no text they produce contains a character
--   this admits and 'Data.Char.isAsciiUpper'/'isAsciiLower' did not.
isNameLetter ∷ Char → Bool
isNameLetter c = isAsciiUpper c ∨ isAsciiLower c ∨ isExtendedLetter c

-- | Whether a character may be a rendered name's INITIAL: an uppercase
--   letter, ASCII or extended.
isNameInitial ∷ Char → Bool
isNameInitial c =
    isAsciiUpper c ∨ any isUpper' extendedLetterTable
  where isUpper' (_, _, _, _, upper) = c ≡ upper

-- | The only non-letter characters a generated name may contain.
--
--   #1100 requirement 8 permits further orthographic symbols and
--   requires each to be justified as a device rather than as
--   decoration. None is added: the length mark a generated language
--   might want already exists as the macron family above, carried ON the
--   vowel where a phoneme inventory can own it, and a bare symbol would
--   need its own position and adjacency rules in every rule #1094-#1096
--   states. The apostrophe (possessive affixes) and hyphen (the
--   'JoinHyphen' style) are the pre-existing two and are unchanged.
nameMarks ∷ [Char]
nameMarks = ['\'', '-']

-- | THE complete set of characters a generated name can ever contain,
--   ascending: every ASCII letter in both cases, every extended letter
--   in both cases, and 'nameMarks'.
--
--   Deliberately the whole ASCII alphabet rather than only the pooled
--   letters (@q@ and @x@ are in neither pool), because this is the set
--   the output contract admits and the set font coverage is proved
--   against — narrowing it would tighten the historical contract for no
--   gain. Shared verbatim with @tools/language_report.py@, which carries
--   its own literal copy and cross-checks it against the value the
--   generator emits, so neither side can drift alone.
outputInventory ∷ [Char]
outputInventory = sort $ concat
    [ ['a' .. 'z'], ['A' .. 'Z']
    , [ lower | (_, _, _, lower, _) ← extendedLetterTable ]
    , [ upper | (_, _, _, _, upper) ← extendedLetterTable ]
    , nameMarks
    ]

-- * Per-language selection

-- | The most marked letters one language adds to one inventory.
--
--   Small on purpose. Three accented vowels beside three to five plain
--   ones is already a strong signature; a language that marked
--   everything would read as noise, which is the failure mode the whole
--   design principle exists to avoid.
maxMarksPerInventory ∷ Int
maxMarksPerInventory = 3

-- | The first generator version whose languages have extended
--   orthography. A fixed literal, never a comparison against
--   'currentGeneratorVersion' (#1092 requirement 4): a world records the
--   version that named it, and versions 1-4 must keep rendering pure
--   ASCII after the current version advances.
extendedOrthographyVersion ∷ Int
extendedOrthographyVersion = 5

-- | One language's extended letters, as @(vowels, consonants)@ to
--   append to its ASCII inventories.
--
--   Deterministic in @(seed, inventories)@ alone. Four draws, in order:
--
--   1. whether this language marks anything at all — one in four do not,
--      which is what makes "some languages draw extended characters and
--      others draw none" a property of the sample rather than a hope;
--   2. which family it uses;
--   3-4. how many vowel and consonant marks it takes, then which.
--
--   A marked language always gains at least one letter WHEN ITS FAMILY
--   HAS ONE TO GIVE: candidates are restricted to marks whose base
--   sound this language already has, so a cedilla language holding none
--   of @c s t@ has nothing to mark and stays plain. That is a real
--   outcome, not a gap — you cannot mark a sound you do not have — and
--   it simply adds to the unmarked population.
extendedInventory ∷ Word64 → [Char] → [Char] → ([Char], [Char])
extendedInventory seed vowels consonants
    | wordInRange (draw seed 0) 0 3 ≡ 0 = ([], [])
    | otherwise                          = (pickedV, pickedC)
  where
    fam = diacriticFamilies
            !! pickIndex (draw seed 1) (length diacriticFamilies)

    availV = [ m | (base, m) ← familyMarks fam VowelSlot,     base `elem` vowels ]
    availC = [ m | (base, m) ← familyMarks fam ConsonantSlot, base `elem` consonants ]

    capV = min maxMarksPerInventory (length availV)
    capC = min maxMarksPerInventory (length availC)

    -- The zero floor lifts to one on whichever side is the language's
    -- only option, so a family with candidates always contributes.
    kV | capV ≡ 0  = 0
       | otherwise = wordInRange (draw seed 2) (if capC ≡ 0 then 1 else 0) capV
    kC | capC ≡ 0  = 0
       | otherwise = wordInRange (draw seed 3) (if kV ≡ 0 then 1 else 0) capC

    -- Sorted, so the appended tail depends on WHICH marks were chosen
    -- and not on the order the shuffle happened to hand them back.
    pickedV = sort (take kV (shuffleBy (draw seed 4) 1 availV))
    pickedC = sort (take kC (shuffleBy (draw seed 5) 1 availC))

-- * Reading a profile back

-- | Every extended letter in a profile's inventories, ascending.
--   Empty for versions 1-4 and for a version-5 language that drew none.
profileExtendedChars ∷ Profile → [Char]
profileExtendedChars p =
    sort (filter isExtendedLetter (profVowels p <> profConsonants p))

-- | The diacritic family a profile's letters carry, derived from
--   inventory membership rather than stored.
--
--   'Nothing' for a language with no extended letters. A language whose
--   letters somehow spanned two families would be a generator bug — the
--   draw takes one family — so this reports the family of the FIRST
--   extended letter and the test suite separately asserts they all
--   agree, rather than this quietly picking a winner.
profileDiacritic ∷ Profile → Maybe DiacriticFamily
profileDiacritic p = case profileExtendedChars p of
    []      → Nothing
    (c : _) → familyOf c
  where
    familyOf c =
        case [ f | (f, _, _, lower, _) ← extendedLetterTable, lower ≡ c ] of
            (f : _) → Just f
            []      → Nothing

-- | A profile's diacritic family as report text.
profileDiacriticText ∷ Profile → Text
profileDiacriticText = maybe "none" diacriticFamilyText ∘ profileDiacritic
