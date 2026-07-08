-- | Command-line argument parsing shared by every boot mode (graphical,
--   headless, dump): dump-layer selection, generic @--flag value@/region
--   parsing.
module App.Cli
  ( DumpLayers(..)
  , allLayers
  , parseDump
  , parseArg
  , parseRegion
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

-- | Parse --region cx1,cy1,cx2,cy2 from args
parseRegion ∷ [String] → (Int, Int, Int, Int)
parseRegion [] = (-8, -8, 8, 8)
parseRegion ("--region":s:_) =
    case map reads (splitOn ',' s) of
        [[(cx1,"")],[(cy1,"")],[(cx2,"")],[(cy2,"")]] →
            (cx1, cy1, cx2, cy2)
        _ → (-8, -8, 8, 8)
parseRegion (_:rest) = parseRegion rest

splitOn ∷ Char → String → [String]
splitOn _ [] = [""]
splitOn d (c:cs)
    | c ≡ d    = "" : splitOn d cs
    | otherwise = case splitOn d cs of
        (w:ws) → (c:w) : ws
        []     → [[c]]
