{-# LANGUAGE CPP #-}
module Main where

import UPrelude
import System.Environment (setEnv, getArgs)
import System.IO (hPutStrLn, stderr)
import World.Generate.Config (minimumWorldSize, normalizeWorldSize
                             , normalizePlateCount)
import Engine.Core.Types (BootProfile(..))
import World.Plate (defaultPlatesFor)
import App.Cli (parseDump, parseArg, parseRegion)
import App.Graphical (runGraphical)
import App.Headless (runHeadless)
import App.Dump (runDump)

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
    Nothing
      | headless  → runHeadless bootProfile (Just (fromMaybe 8008 port))
      | otherwise → runGraphical bootProfile (Just (fromMaybe 8008 port))
