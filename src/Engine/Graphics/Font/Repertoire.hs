-- | Which characters a font atlas is asked to bake, and the identity
--   that follows from that choice (#1098).
--
--   Character selection is internal font policy: Lua asks for a font
--   path, not a character set, so the repertoire is resolved here from
--   the path alone. Everything a repertoire is used for — the request
--   that gets intersected with the font's cmap, the coverage report, the
--   atlas cache key — needs it in ONE canonical form, so 'Repertoire' is
--   an abstract type whose only constructor sorts and deduplicates.
--
--   Identity lives here rather than beside the cache because two
--   independent sites build the key ('Engine.Graphics.Font.Load' on the
--   engine thread, "Engine.Scripting.Lua.API.Text" on the Lua thread to
--   short-circuit before the load is even queued). If they derived it
--   separately, the Lua-side dedup could hand back a handle whose atlas
--   was generated for a different repertoire.
module Engine.Graphics.Font.Repertoire
  ( -- * Canonical repertoires
    Repertoire
  , canonicalRepertoire
  , repertoireChars
  , repertoireSize
    -- * The shipped policy
  , repertoireForFont
  , printableAscii
  , extendedLatin
  , asciiWithCurlyQuotes
    -- * Atlas identity
  , FontKey(..)
  , sdfSizeSentinel
  , sdfFontKey
  , bitmapFontKey
  ) where

import UPrelude
import qualified Data.Set as Set
import System.FilePath (takeFileName)

-- * Canonical repertoires

-- | A set of requested characters in canonical form: deduplicated and
--   ascending by codepoint. Abstract, so no caller can construct one
--   that is ordered differently — the packing result and the cache key
--   both depend on the order being a function of the set alone.
newtype Repertoire = Repertoire { repertoireChars ∷ [Char] }
  deriving (Eq, Ord, Show)

-- | The only way to build one. Any input order or duplication yields
--   the same value.
canonicalRepertoire ∷ [Char] → Repertoire
canonicalRepertoire = Repertoire . Set.toAscList . Set.fromList

repertoireSize ∷ Repertoire → Int
repertoireSize = length . repertoireChars

-- * The shipped policy

-- | @U+0020@–@U+007E@: what every atlas baked before #1098, and what a
--   font with no explicit policy still gets.
printableAscii ∷ Repertoire
printableAscii = canonicalRepertoire asciiChars

asciiChars ∷ [Char]
asciiChars = [' '..'~']

-- | The punctuation generated names and UI copy actually reach for,
--   none of which has an ASCII spelling that survives round-tripping.
commonPunctuation ∷ [Char]
commonPunctuation =
  [ '\x2010'  -- HYPHEN
  , '\x2013'  -- EN DASH
  , '\x2014'  -- EM DASH
  , '\x2018'  -- LEFT SINGLE QUOTATION MARK
  , '\x2019'  -- RIGHT SINGLE QUOTATION MARK
  , '\x201C'  -- LEFT DOUBLE QUOTATION MARK
  , '\x201D'  -- RIGHT DOUBLE QUOTATION MARK
  , '\x2020'  -- DAGGER
  , '\x2021'  -- DOUBLE DAGGER
  , '\x2022'  -- BULLET
  , '\x2026'  -- HORIZONTAL ELLIPSIS
  , '\x2032'  -- PRIME
  , '\x2033'  -- DOUBLE PRIME
  ]

-- | The four curly quotation marks, the one non-ASCII group the title
--   font is asked for.
curlyQuotes ∷ [Char]
curlyQuotes = ['\x2018', '\x2019', '\x201C', '\x201D']

-- | Printable ASCII plus Latin-1 Supplement, Latin Extended-A and
--   'commonPunctuation' — enough accented Latin for generated names.
--
--   Soft hyphen @U+00AD@ is excluded deliberately: it is a line-break
--   opportunity rather than a mark, so baking it would spend a cell on
--   something no layout pass should ever draw.
extendedLatin ∷ Repertoire
extendedLatin = canonicalRepertoire $ concat
    [ asciiChars
    , filter (≢ '\x00AD') ['\x00A1'..'\x00FF']
    , ['\x0100'..'\x017F']
    , commonPunctuation
    ]

-- | Printable ASCII plus 'curlyQuotes'.
asciiWithCurlyQuotes ∷ Repertoire
asciiWithCurlyQuotes = canonicalRepertoire (asciiChars ⧺ curlyQuotes)

-- | The internal registry. Keyed by file name rather than the whole
--   path so the policy survives a resource-root-prefixed or absolute
--   path; the three tracked fonts are the only names with a policy of
--   their own, and every other font keeps 'printableAscii'.
--
--   @gothic.ttf@ is the outlier: its cmap carries 84 codepoints, so
--   asking it for extended Latin would report an enormous miss list
--   without gaining a single glyph.
repertoireForFont ∷ FilePath → Repertoire
repertoireForFont path = case takeFileName path of
    "arcade.ttf" → extendedLatin
    "shell.ttf"  → extendedLatin
    "gothic.ttf" → asciiWithCurlyQuotes
    _            → printableAscii

-- * Atlas identity

-- | The size an SDF atlas records in its cache key. SDF atlases are
--   generated once at 'Engine.Graphics.Font.SDF.sdfBaseSize' and scaled
--   at draw time, so the requested size is not part of their identity.
sdfSizeSentinel ∷ Int
sdfSizeSentinel = -1

-- | What makes one cached atlas a valid substitute for another.
--
--   The repertoire is carried whole rather than hashed: two
--   configurations for one font path must never collide, and a few
--   hundred characters per shipped font is not a size worth trading
--   that guarantee for.
data FontKey = FontKey
  { fkPath       ∷ FilePath
  , fkSize       ∷ Int         -- ^ 'sdfSizeSentinel' for SDF atlases
  , fkRepertoire ∷ Repertoire
  } deriving (Eq, Ord, Show)

-- | The key for the SDF atlas of a font path, policy resolved.
sdfFontKey ∷ FilePath → FontKey
sdfFontKey path = FontKey path sdfSizeSentinel (repertoireForFont path)

-- | The key for a size-specific bitmap atlas, which still bakes
--   printable ASCII only.
bitmapFontKey ∷ FilePath → Int → FontKey
bitmapFontKey path size = FontKey path size printableAscii
