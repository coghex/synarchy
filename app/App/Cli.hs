-- | Command-line argument parsing shared across every boot mode: dump-layer
--   selection, generic @--flag value@/region parsing, plus preview-target,
--   image-size, language-report, and seed-range parsing.
--
--   Absence and malformed presence are DIFFERENT answers (#1191). A
--   parser here returns @'Either' 'CliError' ('Maybe' a)@: 'Right'
--   'Nothing' means the flag never appeared and the caller's documented
--   default applies, while 'Left' means the user typed the flag with a
--   value that cannot mean what they wrote. Collapsing the two — which
--   is what every one of these did before — turns a typo into a silent
--   substitution: @--seed not-a-number@ generated seed 42 and exited 0
--   with a full, valid, wrong dump.
module App.Cli
  ( DumpLayers(..)
  , defaultLayers
  , dumpLayerNames
  , CliError(..)
  , cliErrorMessage
  , parseDump
  , parseArg
  , lookupFlagValue
  , parseStrArg
  , ChunkRegion(..)
  , defaultChunkRegion
  , parseRegion
  , chunkRegionCoords
  , parseSize
  , parsePreview
  , PreviewCategoryKind(..)
  , classifyPreviewCategory
  , simplePreviewCategories
  , groupedPreviewCategories
  , parseLanguageReport
  , parseSeeds
  ) where

import UPrelude
import Data.Char (toLower)
import Data.List (intercalate, isPrefixOf)

-- | Which layers to include in dump output.
data DumpLayers = DumpLayers
    { dlTerrain  ∷ !Bool
    , dlMaterial ∷ !Bool
    , dlFluid    ∷ !Bool
    , dlIce      ∷ !Bool
    , dlOre      ∷ !Bool
    , dlSlope    ∷ !Bool
    } deriving (Show)

-- | Default layers (when --dump has no =value): the original five. The
--   slope layer is OPT-IN only (--dump=...,slope) so a bare --dump stays
--   byte-identical to historical output — the worldgen baselines and the
--   determinism/audit tools all drive a bare --dump and must not see new
--   fields.
defaultLayers ∷ DumpLayers
defaultLayers = DumpLayers
    { dlTerrain  = True
    , dlMaterial = True
    , dlFluid    = True
    , dlIce      = True
    , dlOre      = True
    , dlSlope    = False
    }

-- | Every layer name @--dump=@ accepts, in the order an error message
--   lists them. @elevation@ is a historical alias for @terrain@ and is
--   deliberately part of the accepted set, not a separate layer.
dumpLayerNames ∷ [String]
dumpLayerNames =
    ["terrain", "elevation", "material", "fluid", "ice", "ore", "slope"]

-- | A flag the user actually typed, carrying a value that cannot mean
--   what they wrote (#1191). Absence is never one of these — it is
--   'Right' 'Nothing' from the parser, and the caller's default applies.
data CliError
    = MissingFlagValue !String
      -- ^ The flag was the LAST token in argv, with no operand after
      --   it. Distinct from absence: the user asked for something.
    | BadNumericValue !String !String
      -- ^ Flag, then the offending token verbatim (as typed, so the
      --   message can quote what the user will recognize).
    | BadSizeValue !String
      -- ^ @--size@'s own @WxH@ syntax, which additionally requires both
      --   dimensions positive. Its own constructor rather than a shared
      --   one carrying an expectation string: there is exactly one flag
      --   with this shape, and the message differs entirely.
    | EmptyDumpSelection
      -- ^ @--dump=@ with nothing after the @=@ at all.
    | EmptyDumpLayerName !Int
      -- ^ A 1-based empty segment INSIDE a non-empty selection
      --   (@--dump=terrain,@ or @--dump=terrain,,fluid@). Reported as
      --   empty explicitly rather than as an unknown layer @""@.
    | UnknownDumpLayer !String
      -- ^ A non-empty token naming no layer in 'dumpLayerNames'.
    deriving (Eq, Show)

-- | The stderr line a 'CliError' is reported with. Always names the
--   flag, and — where there is one — the offending token exactly as
--   typed, since a value that survived the shell is what the user has
--   to go looking for in their command line.
cliErrorMessage ∷ CliError → String
cliErrorMessage (MissingFlagValue flag) =
    flag ⧺ " requires a value, but no value followed it"
cliErrorMessage (BadNumericValue flag raw) =
    flag ⧺ ": invalid value " ⧺ show raw ⧺ " (expected a whole number)"
cliErrorMessage (BadSizeValue raw) =
    "--size: invalid value " ⧺ show raw
        ⧺ " (expected WxH with a positive width and height, e.g. 1280x720)"
cliErrorMessage EmptyDumpSelection =
    "--dump= requires at least one layer name" ⧺ expectedLayers
cliErrorMessage (EmptyDumpLayerName i) =
    "--dump=: layer " ⧺ show i ⧺ " is empty -- an empty layer name "
        ⧺ "selects nothing" ⧺ expectedLayers
cliErrorMessage (UnknownDumpLayer tok) =
    "--dump=: unknown layer " ⧺ show tok ⧺ expectedLayers

expectedLayers ∷ String
expectedLayers = " (expected one or more of: "
    ⧺ intercalate ", " dumpLayerNames ⧺ ")"

-- | Parse @--dump@ or @--dump=layer1,layer2,...@ from args.
--   'Right' 'Nothing' if @--dump@ is not present; 'Left' if it is
--   present with a selection naming no layer, an unknown layer, or an
--   empty segment — each of which used to be accepted silently,
--   producing FEWER layers than asked for (a bare @--dump=@ produced
--   none at all, and still exited 0 with tile records carrying nothing
--   but coordinates).
parseDump ∷ [String] → Either CliError (Maybe DumpLayers)
parseDump [] = Right Nothing
parseDump (a:rest)
    | a ≡ "--dump" = Right (Just defaultLayers)
    | "--dump=" `isPrefixOf` a = Just ⊚ parseDumpSelection (drop 7 a)
    | otherwise = parseDump rest

-- | Validate every token of a @--dump=@ selection, then build the
--   layer set from it. Matching stays case-insensitive and keeps the
--   @elevation@ alias; only the silent acceptance of unmatched tokens
--   is gone.
parseDumpSelection ∷ String → Either CliError DumpLayers
parseDumpSelection "" = Left EmptyDumpSelection
parseDumpSelection sel = do
    -- Left to right, so a command line with several bad tokens always
    -- names the first one.
    forM_ (zip [1 ∷ Int ..] raw) $ \(i, tok) →
        if null tok
            then Left (EmptyDumpLayerName i)
            else when (map toLower tok `notElem` dumpLayerNames) $
                     Left (UnknownDumpLayer tok)
    pure DumpLayers
        { dlTerrain  = "terrain"  `elem` flags ∨ "elevation" `elem` flags
        , dlMaterial = "material" `elem` flags
        , dlFluid    = "fluid"    `elem` flags
        , dlIce      = "ice"      `elem` flags
        , dlOre      = "ore"      `elem` flags
        , dlSlope    = "slope"    `elem` flags
        }
  where
    raw   = splitOn ',' sel
    flags = map (map toLower) raw

-- | The one raw @--flag VALUE@ lookup every value parser here is built
--   on. 'Right' 'Nothing' = the flag never appeared; 'Left' = it
--   appeared as the last token, with nothing to be its value.
lookupFlagValue ∷ String → [String] → Either CliError (Maybe String)
lookupFlagValue _ [] = Right Nothing
lookupFlagValue flag [f]
    | f ≡ flag  = Left (MissingFlagValue flag)
    | otherwise = Right Nothing
lookupFlagValue flag (f:v:rest)
    | f ≡ flag  = Right (Just v)
    | otherwise = lookupFlagValue flag (v:rest)

-- | Parse @--flag N@ from args. The FIRST occurrence decides: a
--   malformed one is an error rather than something to skip past in
--   search of a later well-formed occurrence (which is how a typo used
--   to become 'Nothing', indistinguishable from absence).
parseArg ∷ Read a ⇒ String → [String] → Either CliError (Maybe a)
parseArg flag args = lookupFlagValue flag args ⌦ \case
    Nothing  → Right Nothing
    Just raw → case reads raw of
        [(v, "")] → Right (Just v)
        _         → Left (BadNumericValue flag raw)

-- | Parse @--flag VALUE@ from args, returning the raw token. Unlike
--   'parseArg' there's no 'reads' round-trip — a filepath would need
--   Haskell string quoting to survive one.
--
--   Deliberately keeps the pre-#1191 lenient shape, in which a trailing
--   flag with no operand is indistinguishable from absence: its only
--   caller is 'parseSeeds', whose own 'Nothing' already covers absence,
--   a malformed range and an out-of-range bound alike, and whose caller
--   reports one error for all of them.
parseStrArg ∷ String → [String] → Maybe String
parseStrArg flag = either (const Nothing) id ∘ lookupFlagValue flag

-- | The chunk region a dump covers: @--region@'s four coordinates,
--   each named, in the order the flag writes them. It replaces the
--   bare four-'Int' tuple this used to be (#1081), in which
--   @(cx1, cy1, cx2, cy2)@ and @(cx1, cx2, cy1, cy2)@ were the same
--   type and the corner convention lived only in whichever
--   destructuring pattern happened to be read.
--
--   The two corners are kept exactly as parsed and are NOT sorted,
--   normalized, or reinterpreted: 'chunkRegionCoords' walks the
--   DIRECTED inclusive ranges @[x1..x2]@ and @[y1..y2]@, so a region
--   whose second corner precedes its first covers no chunks at all.
--   That is the behaviour a reversed @--region@ has always had, and
--   changing it here would silently redefine what such a command line
--   dumps.
data ChunkRegion = ChunkRegion
    { crX1 ∷ !Int
    , crY1 ∷ !Int
    , crX2 ∷ !Int
    , crY2 ∷ !Int
    } deriving (Eq, Show)

-- | The region a dump covers when @--region@ is absent — and, today,
--   when it is present but malformed (see 'parseRegion').
defaultChunkRegion ∷ ChunkRegion
defaultChunkRegion = ChunkRegion
    { crX1 = -8
    , crY1 = -8
    , crX2 = 8
    , crY2 = 8
    }

-- | Parse @--region cx1,cy1,cx2,cy2@ from args.
--
--   Deliberately still collapses absence and malformed presence onto
--   'defaultChunkRegion' — that is @docs\/code_health_findings.md@
--   CH-67, sequenced after this type and explicitly out of scope for
--   #1081. Separating the two is a change of the RESULT type
--   ('Either' 'CliError' ('Maybe' 'ChunkRegion'), as every #1191
--   parser above already returns) around the same 'ChunkRegion', with
--   'defaultChunkRegion' moving to the caller.
parseRegion ∷ [String] → ChunkRegion
parseRegion [] = defaultChunkRegion
parseRegion ("--region":s:_) =
    case map reads (splitOn ',' s) of
        [[(cx1,"")],[(cy1,"")],[(cx2,"")],[(cy2,"")]] →
            ChunkRegion { crX1 = cx1, crY1 = cy1, crX2 = cx2, crY2 = cy2 }
        _ → defaultChunkRegion
parseRegion (_:rest) = parseRegion rest

-- | Every chunk coordinate a region covers, as @(x, y)@ pairs, in the
--   order the dump emits them: x outer, y inner, both ranges directed
--   and inclusive. The one place that enumeration lives — the dump
--   walks it twice (queueing chunks, then encoding them), and the two
--   walks must agree tile for tile or the JSON stops matching the
--   chunks that were loaded for it.
chunkRegionCoords ∷ ChunkRegion → [(Int, Int)]
chunkRegionCoords r =
    [ (x, y) | x ← [crX1 r .. crX2 r], y ← [crY1 r .. crY2 r] ]

-- | Parse @--size WxH@ from args (offscreen render size, #650).
--   'Right' 'Nothing' ONLY on absence — the caller still falls back to
--   the video-config resolution there. A malformed or non-positive
--   value is a 'Left' (#1191): it used to be the same 'Nothing', so
--   @--size not-a-size@ rendered at the local config's resolution and
--   silently defeated the whole point of pinning a size.
parseSize ∷ [String] → Either CliError (Maybe (Int, Int))
parseSize args = lookupFlagValue "--size" args ⌦ \case
    Nothing  → Right Nothing
    Just raw → case splitOn 'x' (map toLower raw) of
        [ws, hs] → case (reads ws, reads hs) of
            ([(w, "")], [(h, "")]) | w > 0 ∧ h > 0 → Right (Just (w, h))
            _ → Left (BadSizeValue raw)
        _ → Left (BadSizeValue raw)

-- | Parse --preview category[/item] from args.
--   Nothing = --preview not present at all (normal dispatch continues).
--   Just Nothing = --preview given with no value following it — an
--   error, NOT "not present": the caller must not silently fall through
--   to normal headless/graphical dispatch here.
--   Just (Just (category, mItem)) = --preview <value> parsed. Only the
--   FIRST slash splits category from item — everything after it
--   (internal slashes included) is the item path verbatim, so a nested
--   simple-category target (@--preview items/tools/hammer.png@) keeps
--   its full "tools/hammer.png" item rather than being truncated to
--   just "tools" (#886). A trailing slash with nothing after it
--   (@--preview units/@) is treated the same as a bare category
--   (mItem = Nothing), not an empty item name.
parsePreview ∷ [String] → Maybe (Maybe (String, Maybe String))
parsePreview [] = Nothing
parsePreview ["--preview"] = Just Nothing
parsePreview ("--preview":s:_) = Just $ Just $
    case break (≡ '/') s of
        (cat, '/':rest) → (cat, if null rest then Nothing else Just rest)
        (cat, _)        → (cat, Nothing)
parsePreview (_:rest) = parsePreview rest

-- | Which of the epic's canonical --preview categories 'cat' names, if
--   any. Simple categories preview a single flat asset folder; grouped
--   categories require a specific --preview <category>/<item> (the
--   folder holds many named entries, e.g. one per unit). This is the
--   epic-level reconciliation (#886): @equipment@/@hud@ (no longer
--   top-level asset directories since #428's reorganization — HUD
--   assets live under @ui/hud@) are ordinary unknown categories now,
--   with no compatibility aliases, and @structures@ (a real top-level
--   directory) is grouped. @facemap@, @utility@, and @vegetation@ stay
--   unexposed.
data PreviewCategoryKind
    = SimplePreviewCategory
    | GroupedPreviewCategory
    | UnknownPreviewCategory
    deriving (Eq, Show)

classifyPreviewCategory ∷ String → PreviewCategoryKind
classifyPreviewCategory cat
    | cat `elem` simplePreviewCategories  = SimplePreviewCategory
    | cat `elem` groupedPreviewCategories = GroupedPreviewCategory
    | otherwise                           = UnknownPreviewCategory

-- | Preview categories backed by a single flat, recursively-browsable
--   asset folder. The canonical set from #886 — see 'classifyPreviewCategory'.
simplePreviewCategories ∷ [String]
simplePreviewCategories = ["icons", "items", "ui", "world"]

-- | Preview categories requiring a specific --preview <category>/<item>
--   target. The canonical set from #886 — see 'classifyPreviewCategory'.
groupedPreviewCategories ∷ [String]
groupedPreviewCategories = ["units", "flora", "buildings", "structures"]

-- | Whether @--language-report@ (#710) is present at all. It never
--   boots the engine/world (unlike --dump/--headless/--offscreen), so
--   Main only needs a presence check plus 'parseSeeds' for its value.
parseLanguageReport ∷ [String] → Bool
parseLanguageReport = elem "--language-report"

-- | Parse @--seeds LO:HI@ (an inclusive 'Word64' range) from args.
--   Nothing on absence, a malformed range, LO > HI, or either bound
--   outside @[0, 2^64-1]@ — 'reads' alone is not enough here since
--   GHC's 'Word64' 'Read' instance silently wraps a negative literal
--   via 'fromInteger' rather than rejecting it, so bounds are parsed as
--   'Integer' first and range-checked before narrowing.
parseSeeds ∷ [String] → Maybe (Word64, Word64)
parseSeeds args = do
    s ← parseStrArg "--seeds" args
    case splitOn ':' s of
        [loS, hiS] → do
            lo ← parseWord64Bound loS
            hi ← parseWord64Bound hiS
            if lo ≤ hi then Just (lo, hi) else Nothing
        _ → Nothing

parseWord64Bound ∷ String → Maybe Word64
parseWord64Bound s = case reads s ∷ [(Integer, String)] of
    [(v, "")] | v ≥ 0 ∧ v ≤ toInteger (maxBound ∷ Word64) → Just (fromInteger v)
    _ → Nothing

splitOn ∷ Char → String → [String]
splitOn _ [] = [""]
splitOn d (c:cs)
    | c ≡ d    = "" : splitOn d cs
    | otherwise = case splitOn d cs of
        (w:ws) → (c:w) : ws
        []     → [[c]]
