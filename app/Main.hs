{-# LANGUAGE CPP #-}
module Main where

import UPrelude
import System.Environment (setEnv, getArgs)
import System.Exit (exitSuccess, exitWith, ExitCode(..))
import System.IO (hPutStrLn, stderr)
import qualified Data.Text as T
import World.Generate.Config (minimumWorldSize, normalizeWorldSize
                             , normalizePlateCount)
import Engine.Core.Types (BootProfile(..))
import World.Plate (defaultPlatesFor)
import App.Cli (parseDump, parseArg, parseRegion, parsePreview
               , PreviewCategoryKind(..), classifyPreviewCategory)
import App.Graphical (runGraphical)
import App.Headless (runHeadless)
import App.Dump (runDump)
import App.Preview (runPreview)

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
  let headless = "--headless" `elem` args
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

  case mDump of
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
                ⧺ " (expected one of: icons, equipment, hud, items, ui, "
                ⧺ "world, units, flora, buildings)"
            exitWith (ExitFailure 1)
        GroupedPreviewCategory | isNothing mItem → do
            putStrLn $ "select a specific " ⧺ cat
                ⧺ ", e.g. --preview units/acolyte"
            exitSuccess
        _ → runPreview (T.pack cat, T.pack ⊚ mItem)
                        (Just (fromMaybe 8008 port))
      Nothing
        | headless  → runHeadless bootProfile (Just (fromMaybe 8008 port))
        | otherwise → runGraphical bootProfile (Just (fromMaybe 8008 port))
