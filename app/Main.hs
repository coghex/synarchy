{-# LANGUAGE CPP #-}
module Main where

import UPrelude
import System.Environment (setEnv, getArgs)
import System.Exit (exitSuccess, exitWith, ExitCode(..))
import System.IO (hPutStrLn, stderr)
import qualified Data.Text as T
import World.Generate.Config (minimumWorldSize, normalizeWorldSize
                             , normalizePlateCount)
import Engine.Core.Types (BootProfile(..), PreviewBrowse(..))
import Engine.Preview.Discovery (discoverEntries, resolveFocusedEntry
                                , focusErrorMessage, textureCategoryRoot)
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

  when (isJust mDump ∧ worldSize /= rawWorldSize) $
    hPutStrLn stderr $ "worldSize " ⧺ show rawWorldSize
        ⧺ " normalized to " ⧺ show worldSize
        ⧺ " (minimum/multiple " ⧺ show minimumWorldSize ⧺ ")."
  when (isJust mDump ∧ plateCount /= rawPlateCount) $
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
          GroupedPreviewCategory | isNothing mItem → do
              putStrLn $ "select a specific " ⧺ cat
                  ⧺ ", e.g. --preview units/acolyte"
              exitSuccess
          -- Grouped category/item: classification is canonical as of
          -- this issue, but the actual browsing implementation is
          -- #887 (units) / #888 (the rest) — keep Phase 1's (#632)
          -- placeholder-label boot, no 'PreviewBrowse'.
          GroupedPreviewCategory → runPreview (T.pack cat, T.pack ⊚ mItem)
                                    Nothing (Just (fromMaybe 8008 port))
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
