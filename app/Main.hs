{-# LANGUAGE CPP #-}
module Main where

import UPrelude
import System.Environment (setEnv, getArgs)
import System.Exit (exitSuccess, exitWith, ExitCode(..))
import System.IO (hPutStrLn, stderr)
import Data.List (intercalate)
import qualified Data.Text as T
import World.Generate.Config (minimumWorldSize, normalizeWorldSize
                             , normalizePlateCount)
import Engine.Core.Types (BootProfile(..), PreviewBrowse(..))
import Engine.Preview.Discovery (discoverEntries, resolveFocusedEntry
                                , focusErrorMessage, textureCategoryRoot
                                , resolveItemDir, itemDirErrorMessage)
import Engine.Preview.Unit (buildPreviewUnit, unitFocusErrorMessage
                           , unitsCategoryRoot)
import Engine.Preview.Building (buildPreviewBuilding)
import World.Plate (defaultPlatesFor)
import App.Cli (parseDump, parseArg, parseRegion, parseSize, parsePreview
               , PreviewCategoryKind(..), classifyPreviewCategory
               , parseLanguageReport, parseSeeds)
import App.ResourceRoot (applyResourceRoot)
import App.Graphical (runGraphical)
import App.Headless (runHeadless)
import App.Offscreen (runOffscreen)
import App.Dump (runDump)
import App.Preview (runPreview)
import App.LanguageReport (runLanguageReport)

main ∷ IO ()
main = do
  setEnv "NSLog_Disabled" "YES"
  setEnv "MVK_CONFIG_USE_METAL_ARGUMENT_BUFFERS" "2"
#ifdef DEVELOPMENT
  setEnv "VK_LOADER_DEBUG" "none"
  setEnv "VK_LOADER_MESSAGE_LEVEL" "error"
  setEnv "VK_LOADER_LOG_LEVEL" "0"
#endif

  args ← getArgs
  -- Resolve + chdir into the runtime resource root before ANY dispatch
  -- (#636): scripts/, assets/, data/, config/ are all loaded by
  -- cwd-relative paths from here on.
  applyResourceRoot args
  -- Reject a mode-specific flag given to a boot mode that ignores it
  -- (CH-58) before any normalization warning or boot dispatch, so an
  -- ignored value can never appear to have taken effect.
  rejectIncompatibleFlags args
  let headless = "--headless" `elem` args
      offscreen = "--offscreen" `elem` args
      bootProfile = if "--arena" `elem` args then BootArena else BootNormal
      mDump    = parseDump args
      mPreview = parsePreview args
      port = parseArg "--port" args
      seed = parseArg "--seed" args
      worldSz = parseArg "--worldSize" args
      -- `--plates` is the canonical flag; `--ages` is a legacy alias
      -- (its original name was misleading — the value is the plate
      -- count, not number of geological ages, which is rolled
      -- randomly inside buildTimeline).
      plates  = parseArg "--plates" args
      agesLeg = parseArg "--ages" args
      region = parseRegion args
      rawWorldSize = fromMaybe 256 worldSz
      worldSize = normalizeWorldSize rawWorldSize
      rawPlateCount = case plates of
          Just p  → p
          Nothing → fromMaybe (defaultPlatesFor worldSize) agesLeg
      plateCount = normalizePlateCount rawPlateCount

  -- worldSize/plateCount only ever reach a non-default value in --dump
  -- mode: rejectIncompatibleFlags above already exits before this point
  -- for any other mode that was given --worldSize/--plates/--ages, so
  -- gating these on 'isJust mDump' would be redundant.
  when (worldSize /= rawWorldSize) $
    hPutStrLn stderr $ "worldSize " ⧺ show rawWorldSize
        ⧺ " normalized to " ⧺ show worldSize
        ⧺ " (minimum/multiple " ⧺ show minimumWorldSize ⧺ ")."
  when (plateCount /= rawPlateCount) $
    hPutStrLn stderr $ "plateCount " ⧺ show rawPlateCount
        ⧺ " normalized to " ⧺ show plateCount
        ⧺ " (minimum 1)."

  if parseLanguageReport args
    then case parseSeeds args of
      Just seeds → runLanguageReport seeds
      Nothing → do
          hPutStrLn stderr $ "--language-report requires --seeds LO:HI "
              ⧺ "(an inclusive range within 0.." ⧺ show (maxBound ∷ Word64) ⧺ ")"
          exitWith (ExitFailure 1)
    else case mDump of
      Just layers → runDump layers (fromMaybe 42 seed) worldSize
                                   plateCount region
      Nothing → case mPreview of
        -- --preview wins over headless/graphical dispatch, same as --dump
        -- above: a bare `--preview ...` shouldn't also stand up the normal
        -- boot path.
        Just Nothing → do
            hPutStrLn stderr $ "--preview requires a target, e.g. "
                ⧺ "--preview icons or --preview units/acolyte"
            exitWith (ExitFailure 1)
        Just (Just (cat, mItem)) → case classifyPreviewCategory cat of
          UnknownPreviewCategory → do
              hPutStrLn stderr $ "Unrecognized preview category: " ⧺ cat
                  ⧺ " (expected one of: icons, items, ui, world, units, "
                  ⧺ "flora, buildings, structures)"
              exitWith (ExitFailure 1)
          GroupedPreviewCategory → case mItem of
              Nothing → do
                  putStrLn $ "select a specific " ⧺ cat
                      ⧺ ", e.g. --preview units/acolyte"
                  exitSuccess
              Just item → runGroupedPreview cat item (fromMaybe 8008 port)
          SimplePreviewCategory → case mItem of
            -- Bare simple category: recursively discover every texture
            -- under its root (#886 Requirement 3) — always succeeds
            -- (an empty/missing root just yields no entries, not a
            -- pre-boot error; every canonical simple category is a
            -- real, populated directory in this repo).
            Nothing → do
                entries ← discoverEntries (textureCategoryRoot cat)
                runPreview (T.pack cat, Nothing) (Just (PreviewList entries))
                           (Just (fromMaybe 8008 port))
            -- Focused item: resolve + validate BEFORE ever creating a
            -- window (#886 Requirement 4) — absolute paths, ".."
            -- traversal, symlink escapes, directories, unsupported
            -- extensions, and plain nonexistence all reject here.
            Just item → resolveFocusedEntry (textureCategoryRoot cat) item ⌦ \case
                Left err → do
                    hPutStrLn stderr $ "--preview " ⧺ cat ⧺ "/" ⧺ item
                        ⧺ ": " ⧺ T.unpack (focusErrorMessage err)
                    exitWith (ExitFailure 1)
                Right entry →
                    runPreview (T.pack cat, Just (T.pack item))
                               (Just (PreviewItem entry))
                               (Just (fromMaybe 8008 port))
        Nothing
          -- Offscreen (#650) wins over --headless if both are given:
          -- it is the strictly more capable mode (GPU on, window off).
          | offscreen → runOffscreen bootProfile (Just (fromMaybe 8008 port))
                                     (parseSize args)
          | headless  → runHeadless bootProfile (Just (fromMaybe 8008 port))
          | otherwise → runGraphical bootProfile (Just (fromMaybe 8008 port))

-- | Dispatch a @--preview \<grouped category\>/\<item\>@ target (#888
--   completes the set). Every branch resolves and validates the item
--   BEFORE ever creating a window, exactly like a focused
--   simple-category item — an unknown item, a name carrying path
--   structure, a symlinked directory, and a file where a directory was
--   expected all reject here.
--
--   @flora@ and @structures@ item folders are flat sets of static PNGs
--   — the exact shape #886's simple-category browser already handles —
--   so they are deliberately ROUTED into it (rooted at the item's own
--   folder) rather than given viewers of their own (#888 Requirement
--   2). The @otherwise@ branch is therefore the general grouped-item
--   rule, not a fallback: a future flat grouped category needs no code
--   here at all.
runGroupedPreview ∷ String → String → Int → IO ()
runGroupedPreview cat item port
    | cat ≡ "units" =
        buildPreviewUnit unitsCategoryRoot item ⌦ \case
            Left err → rejectItem (unitFocusErrorMessage err)
            Right unit → runPreview target (Just (PreviewUnitAnims unit))
                                    (Just port)
    | cat ≡ "buildings" =
        buildPreviewBuilding (textureCategoryRoot cat) item ⌦ \case
            Left err → rejectItem (itemDirErrorMessage err)
            Right building →
                runPreview target (Just (PreviewBuildingAssets building))
                           (Just port)
    | otherwise =
        resolveItemDir (textureCategoryRoot cat) item ⌦ \case
            Left err → rejectItem (itemDirErrorMessage err)
            Right dir → do
                entries ← discoverEntries dir
                runPreview target (Just (PreviewList entries)) (Just port)
  where
    target = (T.pack cat, Just (T.pack item))
    rejectItem msg = do
        hPutStrLn stderr $ "--preview " ⧺ cat ⧺ "/" ⧺ item ⧺ ": "
            ⧺ T.unpack msg
        exitWith (ExitFailure 1)

-- | The boot mode argv selects, by the SAME precedence as the dispatch
--   above (language-report, then dump, then preview, then offscreen,
--   then headless, else graphical) — used only to name the selected mode
--   in a 'rejectIncompatibleFlags' error.
selectedBootModeName ∷ [String] → String
selectedBootModeName args
    | parseLanguageReport args   = "language-report"
    | isJust (parseDump args)    = "dump"
    | isJust (parsePreview args) = "preview"
    | "--offscreen" `elem` args  = "offscreen"
    | "--headless" `elem` args   = "headless"
    | otherwise                  = "graphical"

-- | Every ancillary (non-mode-selecting) flag Main parses, paired with
--   the boot mode(s) that actually honour it (CH-58) — everything else
--   silently discards it today. Detected by syntactic occurrence of the
--   flag token in argv, not by whether the flag's own value parses: a
--   malformed @--seed nonsense@ given to headless must still be rejected
--   here rather than quietly vanishing into 'parseArg's @Nothing@.
incompatibleFlagTable ∷ [(String, [String])]
incompatibleFlagTable =
    [ ("--seed",      ["dump"])
    , ("--worldSize", ["dump"])
    , ("--plates",    ["dump"])
    , ("--ages",      ["dump"])
    , ("--region",    ["dump"])
    , ("--size",      ["offscreen"])
    , ("--seeds",     ["language-report"])
    , ("--arena",     ["headless", "graphical", "offscreen"])
    , ("--port",      ["headless", "graphical", "offscreen", "preview"])
    ]

-- | Exit 1 before any normalization warning or boot dispatch if argv
--   carries a flag from 'incompatibleFlagTable' that the selected boot
--   mode does not honour (CH-58) — the same pre-boot-rejection shape
--   this file already uses for a bare/unknown @--preview@ target.
rejectIncompatibleFlags ∷ [String] → IO ()
rejectIncompatibleFlags args = case violations of
    []               → pure ()
    (flag, honoured) : _ → do
        hPutStrLn stderr $ flag ⧺ " is not supported in " ⧺ mode
            ⧺ " mode (only honoured in " ⧺ intercalate ", " honoured ⧺ ")"
        exitWith (ExitFailure 1)
  where
    mode = selectedBootModeName args
    violations =
        [ (flag, honoured)
        | (flag, honoured) ← incompatibleFlagTable
        , flag `elem` args
        , mode `notElem` honoured
        ]
