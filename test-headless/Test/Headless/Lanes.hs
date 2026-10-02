-- | Selectable lanes for the headless suite (#2744, CIR-15 of epic #2742).
--
--   The suite is an ordered list of named lanes, each one contiguous
--   block of @Spec.hs@'s registrations. One lane is the DEFAULT: it is
--   the block every unassigned top-level group is registered in, so a
--   group added without touching any lane definition runs in exactly that
--   lane. The lanes are disjoint by construction, because each
--   registration appears in exactly one block, and together they are the
--   whole suite.
--
--   The executable takes two flags of its own, stripped before Hspec
--   parses the rest:
--
--     * @--lane NAME@ (or @--lane=NAME@) runs exactly that lane. An
--       unknown name fails before anything runs, naming the known lanes.
--     * @--list-lanes@ prints each lane name, one per line, in suite
--       order, the default marked @(default)@, and exits.
--     * @--lane-self-test@ runs the lane machinery's own regression
--       ("Test.Headless.LaneSelection", on synthetic lanes) INSTEAD of the
--       suite. It is deliberately in no lane, so the suite's examples stay
--       exactly the ones the single block registered; @tools/headless_lanes.py@
--       runs it.
--
--   With no @--lane@ the executable runs every lane in order: the same
--   examples, in the same order, with the same descriptions as the single
--   @hspec $ do@ block it replaced.
--
--   It also offers Hspec one extra formatter, @--format=inventory@, for
--   @tools/headless_lanes.py@: one line per example it would run (its
--   full path as a JSON list of strings), then one total line.
module Test.Headless.Lanes
    ( Lanes(..)
    , LaneRequest(..)
    , laneNames
    , parseLaneArgs
    , lanesSpec
    , runLanes
      -- * Inventory output
    , inventoryItemMarker
    , inventoryTotalMarker
    , inventoryLine
    ) where

import UPrelude
import Data.Char (ord)
import Data.List (intercalate)
import Numeric (showHex)
import System.Environment (getArgs, withArgs)
import System.Exit (die)
import Test.Hspec (Spec)
import Test.Hspec.Core.Runner (Config(..), defaultConfig, hspecWith)
import Test.Hspec.Core.Formatters.V2
    (Formatter(..), formatterToFormat, writeLine, getTotalCount)

-- | The suite's lanes, in suite order, and the name of the default lane.
data Lanes = Lanes
    { lanesInOrder ∷ [(String, Spec)]
    , lanesDefault ∷ String
    }

data LaneRequest
    = AllLanes
    | OneLane String
    | ListLanes
    | SelfTest
    deriving (Show, Eq)

laneNames ∷ Lanes → [String]
laneNames = map fst ∘ lanesInOrder

-- | Split the lane flags off the command line; everything else is left,
--   in order, for Hspec.
parseLaneArgs ∷ [String] → Either String (LaneRequest, [String])
parseLaneArgs = go AllLanes []
  where
    go req acc [] = Right (req, reverse acc)
    go AllLanes acc ("--list-lanes" : rest) = go ListLanes acc rest
    go _ _ ("--list-lanes" : _) = Left "--list-lanes may not be combined with --lane"
    go AllLanes acc ("--lane-self-test" : rest) = go SelfTest acc rest
    go _ _ ("--lane-self-test" : _) = Left "--lane-self-test may not be combined with --lane"
    go req acc ("--lane" : rest) = case rest of
        (name : rest') → setLane req name acc rest'
        [] → Left "--lane needs a lane name"
    go req acc (arg : rest)
        | Just name ← stripLaneEq arg = setLane req name acc rest
        | otherwise = go req (arg : acc) rest
    setLane AllLanes name acc rest
        | null name = Left "--lane needs a lane name"
        | otherwise = go (OneLane name) acc rest
    setLane _ _ _ _ = Left "--lane may be given once, and not with --list-lanes or --lane-self-test"
    stripLaneEq arg = case splitAt 7 arg of
        ("--lane=", name) → Just name
        _                 → Nothing

-- | The spec a request runs: one lane, or every lane in order.
lanesSpec ∷ Lanes → LaneRequest → Either String Spec
lanesSpec lanes req = case req of
    AllLanes → Right (mapM_ snd (lanesInOrder lanes))
    ListLanes → Right (pure ())
    SelfTest → Right (pure ())
    OneLane name → case lookup name (lanesInOrder lanes) of
        Just s  → Right s
        Nothing → Left ("unknown headless lane " ⧺ show name
                        ⧺ "; the lanes are: " ⧺ intercalate ", " (described lanes))
  where
    described ls = [ n ⧺ (if n ≡ lanesDefault ls then " (default)" else "")
                   | n ← laneNames ls ]

-- | The test executable's @main@. The second argument is the lane
--   machinery's own regression, run only by @--lane-self-test@.
runLanes ∷ Lanes → Spec → IO ()
runLanes lanes selfTest = do
    args ← getArgs
    (req, rest) ← either die pure (parseLaneArgs args)
    case req of
        ListLanes → forM_ (laneNames lanes) $ \n →
            putStrLn (n ⧺ (if n ≡ lanesDefault lanes then " (default)" else ""))
        SelfTest → withArgs rest (hspecWith config selfTest)
        _ → do
            spec ← either die pure (lanesSpec lanes req)
            withArgs rest (hspecWith config spec)
  where
    config = defaultConfig
        { configAvailableFormatters =
            configAvailableFormatters defaultConfig
            ⧺ [("inventory", formatterToFormat inventoryFormatter)] }

-- * Inventory output

inventoryItemMarker, inventoryTotalMarker ∷ String
inventoryItemMarker  = "headless-inventory-item "
inventoryTotalMarker = "headless-inventory-total "

-- | One example: its marker, then its groups and description as a JSON
--   list of strings, ASCII-only so no locale can mangle it.
inventoryLine ∷ [String] → String → String
inventoryLine groups desc =
    inventoryItemMarker ⧺ "[" ⧺ intercalate "," (map jsonString (groups ⧺ [desc])) ⧺ "]"

jsonString ∷ String → String
jsonString s = "\"" ⧺ concatMap esc s ⧺ "\""
  where
    esc '"'  = "\\\""
    esc '\\' = "\\\\"
    esc c
        | ord c < 0x20 ∨ ord c > 0x7e = unicodeEscape (ord c)
        | otherwise = [c]
    unicodeEscape n
        | n > 0xFFFF = let m = n - 0x10000
                       in u4 (0xD800 + m `div` 0x400) ⧺ u4 (0xDC00 + m `mod` 0x400)
        | otherwise = u4 n
    u4 n = "\\u" ⧺ pad (showHex n "")
    pad h = replicate (4 - length h) '0' ⧺ h

inventoryFormatter ∷ Formatter
inventoryFormatter = Formatter
    { formatterStarted      = pure ()
    , formatterGroupStarted = \_ → pure ()
    , formatterGroupDone    = \_ → pure ()
    , formatterProgress     = \_ _ → pure ()
    , formatterItemStarted  = \_ → pure ()
    , formatterItemDone     = \(groups, desc) _ → writeLine (inventoryLine groups desc)
    , formatterDone         = do
        n ← getTotalCount
        writeLine (inventoryTotalMarker ⧺ show n)
    }
