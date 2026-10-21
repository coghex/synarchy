-- | @synarchy --help@'s pure half (#2863).
--
--   @--help@ used to be an unrecognized argument, so it selected no
--   mode and booted the graphical game. These cases pin what the fix
--   rests on without a process: which argv tokens request help, and
--   that the usage text names every boot mode, every ancillary flag in
--   'incompatibleFlagTable', @--resource-root@ and the help flags
--   themselves. They are checked against the definitions @app\/Main.hs@
--   and 'App.ResourceRoot' really consume ('selectBootMode',
--   'incompatibleFlagTable', 'decideResourceRoot'), so a flag or mode
--   added there without usage text fails here.
--
--   That the real executable prints this text, exits 0 and writes
--   nothing, from any directory and with any other argv, is
--   @tools\/preview_cli_probe.py@'s check 11.
module Test.Headless.App.Usage (spec) where

import UPrelude
import Test.Hspec
import Data.Char (isAscii)
import Data.List (intercalate, isPrefixOf, isInfixOf, isSuffixOf, findIndex)
import App.Cli (BootModeSelection(..), selectBootMode, bootModeSelectionName)
import App.ResourceRoot (RootDecision(..), decideResourceRoot
                        , resourceRootFlag)
import App.Usage (helpFlags, helpRequested, incompatibleFlagTable
                 , modeSelector, usageText)

-- | Whether the usage text has an entry line for @token@: the token at
--   the start of an indented line, followed by a space, a comma or
--   nothing — so @--seed@ does not match an entry for @--seeds@.
hasEntry ∷ String → Bool
hasEntry token = any isEntry (lines usageText)
  where
    isEntry l = case stripLead l of
        Just rest → token `isPrefixOf` rest
            ∧ take 1 (drop (length token) rest) `elem` ["", " ", ",", "["]
        Nothing → False
    stripLead l = if "  " `isPrefixOf` l ∧ take 1 (drop 2 l) ≢ " "
        then Just (drop 2 l) else Nothing

-- | The index of the entry line for @token@, for ordering checks.
entryIndex ∷ String → Maybe Int
entryIndex token = findIndex (("  " ⧺ token) `isPrefixOf`) (lines usageText)

-- | The text with its line wrapping undone, so a phrase that the
--   wrapper split across lines can still be found.
flat ∷ String
flat = unwords (words usageText)

-- | Every mode that has a selector token, in precedence order.
selected ∷ [(BootModeSelection, String)]
selected = [ (m, tok) | m ← [minBound .. maxBound], Just tok ← [modeSelector m] ]

spec ∷ Spec
spec = describe "App.Usage --help (#2863)" $ do

  describe "helpRequested" $ do
    it "accepts both exact spellings on their own" $ do
      helpRequested ["--help"] `shouldBe` True
      helpRequested ["-h"] `shouldBe` True
      helpFlags `shouldMatchList` ["-h", "--help"]

    it "wins wherever it appears, whatever else argv carries" $ do
      helpRequested ["--headless", "--port", "9008", "--help"] `shouldBe` True
      helpRequested ["-h", "--dump=bogus", "--seed", "x"] `shouldBe` True
      helpRequested ["--resource-root", "--help"] `shouldBe` True
      helpRequested ["--language-report", "--seeds", "-h"] `shouldBe` True

    it "matches exact tokens only, so every other argv keeps its \
       \existing behaviour" $
      forM_ [ [], ["--headless"], ["--helpme"], ["--help=x"], ["-help"]
            , ["--HELP"], ["-H"], ["-hh"], ["help"], ["--dump=help"] ] $ \args →
        (args, helpRequested args) `shouldBe` (args, False)

  describe "usageText" $ do
    it "lists every boot mode by name, graphical included" $
      forM_ [minBound .. maxBound] $ \m →
        (bootModeSelectionName m ⧺ ":") `shouldSatisfy` (`isInfixOf` usageText)

    it "has an entry for each mode selector, and that token really \
       \selects that mode" $
      forM_ selected $ \(m, tok) → do
        selectBootMode [tok] `shouldBe` m
        tok `shouldSatisfy` hasEntry

    it "gives graphical no selector: it is what argv naming none selects" $ do
      modeSelector SelectGraphical `shouldBe` Nothing
      selectBootMode [] `shouldBe` SelectGraphical
      "(no mode flag)" `shouldSatisfy` hasEntry

    it "lists the modes in precedence order, as its header claims" $
      forM_ [ (a, b) | (i, a) ← zip [0 ∷ Int ..] selected
                     , (j, b) ← zip [0 ..] selected, i < j ] $
        \((ma, ta), (_, tb)) → do
          selectBootMode [tb, ta] `shouldBe` ma
          (entryIndex ta < entryIndex tb) `shouldBe` True

    it "names every flag in incompatibleFlagTable, with the modes that \
       \honour it" $
      forM_ incompatibleFlagTable $ \(flag, honoured) → do
        flag `shouldSatisfy` hasEntry
        let modes = "[" ⧺ intercalate ", " (map bootModeSelectionName honoured)
                    ⧺ "]"
        (flag, modes `isInfixOf` flat) `shouldBe` (flag, True)

    it "names --resource-root, the token decideResourceRoot parses" $ do
      resourceRootFlag `shouldBe` "--resource-root"
      decideResourceRoot [resourceRootFlag, "/x"] Nothing
        `shouldBe` RootExplicit "/x" resourceRootFlag
      (resourceRootFlag ⧺ " PATH") `shouldSatisfy` hasEntry
      "SYNARCHY_ROOT" `shouldSatisfy` (`isInfixOf` usageText)

    it "names both help spellings" $
      forM_ helpFlags $ \f → f `shouldSatisfy` (`isInfixOf` usageText)

    it "is plain ASCII within 80 columns, ending in a newline" $ do
      usageText `shouldSatisfy` all isAscii
      filter ((> 80) ∘ length) (lines usageText) `shouldBe` []
      usageText `shouldSatisfy` ("\n" `isSuffixOf`)

  describe "incompatibleFlagTable" $
    it "keeps every CH-58 row unchanged by its move out of Main" $
      incompatibleFlagTable `shouldBe`
        [ ("--seed",      [SelectDump])
        , ("--worldSize", [SelectDump])
        , ("--plates",    [SelectDump])
        , ("--ages",      [SelectDump])
        , ("--region",    [SelectDump])
        , ("--size",      [SelectOffscreen])
        , ("--seeds",     [SelectLanguageReport])
        , ("--arena",     [SelectHeadless, SelectGraphical, SelectOffscreen])
          -- #2715: honoured exactly where --arena is (it selects that profile)
        , ("--scenario",  [SelectHeadless, SelectGraphical, SelectOffscreen])
        , ("--port",      [SelectHeadless, SelectGraphical, SelectOffscreen
                          , SelectPreview])
        ]
