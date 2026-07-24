-- | Command-line argument parsing shared by every boot mode (graphical,
--   headless, dump): dump-layer selection, generic @--flag value@/region
--   parsing.
module App.Cli
  ( DumpLayers(..)
  , allLayers
  , parseDump
  , parseArg
  , parseStrArg
  , parseRegion
  , parseSize
  , parsePreview
  , PreviewCategoryKind(..)
  , classifyPreviewCategory
  , parseLanguageReport
  , parseSeeds
  ) where

import UPrelude
import Data.Char (toLower)
import Data.List (isPrefixOf)

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
allLayers ∷ DumpLayers
allLayers = DumpLayers True True True True True False

-- | Parse --dump or --dump=layer1,layer2,... from args.
--   Returns Nothing if --dump not present, Just layers otherwise.
parseDump ∷ [String] → Maybe DumpLayers
parseDump [] = Nothing
parseDump (a:rest)
    | a ≡ "--dump" = Just allLayers
    | "--dump=" `isPrefixOf` a =
        let flags = map (map toLower) $ splitOn ',' (drop 7 a)
        in Just DumpLayers
            { dlTerrain  = "terrain"  `elem` flags ∨ "elevation" `elem` flags
            , dlMaterial = "material" `elem` flags
            , dlFluid    = "fluid"    `elem` flags
            , dlIce      = "ice"      `elem` flags
            , dlOre      = "ore"      `elem` flags
            , dlSlope    = "slope"    `elem` flags
            }
    | otherwise = parseDump rest

-- | Parse --flag N from args
parseArg ∷ Read a ⇒ String → [String] → Maybe a
parseArg _ [] = Nothing
parseArg flag (f:n:rest)
    | f ≡ flag  = case reads n of
        [(v, "")] → Just v
        _         → parseArg flag rest
    | otherwise = parseArg flag (n:rest)
parseArg _ [_] = Nothing

-- | Parse --flag VALUE from args, returning the raw token. Unlike
--   'parseArg' there's no 'reads' round-trip — a filepath would need
--   Haskell string quoting to survive one.
parseStrArg ∷ String → [String] → Maybe String
parseStrArg _ [] = Nothing
parseStrArg flag (f:v:rest)
    | f ≡ flag  = Just v
    | otherwise = parseStrArg flag (v:rest)
parseStrArg _ [_] = Nothing

-- | Parse --region cx1,cy1,cx2,cy2 from args
parseRegion ∷ [String] → (Int, Int, Int, Int)
parseRegion [] = (-8, -8, 8, 8)
parseRegion ("--region":s:_) =
    case map reads (splitOn ',' s) of
        [[(cx1,"")],[(cy1,"")],[(cx2,"")],[(cy2,"")]] →
            (cx1, cy1, cx2, cy2)
        _ → (-8, -8, 8, 8)
parseRegion (_:rest) = parseRegion rest

-- | Parse --size WxH from args (offscreen render size, #650).
--   Nothing on absence or malformed/non-positive values — the caller
--   falls back to the video-config resolution.
parseSize ∷ [String] → Maybe (Int, Int)
parseSize args = do
    s ← parseStrArg "--size" args
    case splitOn 'x' (map toLower s) of
        [ws, hs] → case (reads ws, reads hs) of
            ([(w, "")], [(h, "")]) | w > 0 ∧ h > 0 → Just (w, h)
            _ → Nothing
        _ → Nothing

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
    | cat `elem` simpleCategories  = SimplePreviewCategory
    | cat `elem` groupedCategories = GroupedPreviewCategory
    | otherwise                    = UnknownPreviewCategory
  where
    simpleCategories  = ["icons", "items", "ui", "world"]
    groupedCategories = ["units", "flora", "buildings", "structures"]

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
