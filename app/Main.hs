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
import App.Cli (parseDump, defaultLayers, parseArg, parseRegion
               , parseSize, parsePreview
               , PreviewCategoryKind(..), classifyPreviewCategory
               , simplePreviewCategories, groupedPreviewCategories
               , parseSeeds
               , BootModeSelection(..), selectBootMode, bootModeSelectionName
               , CliError, cliErrorMessage)
import App.ResourceRoot (applyResourceRoot)
import App.Graphical (runGraphical)
import App.Headless (runHeadless)
import App.Offscreen (runOffscreen)
import App.Dump (runDump, DumpGenParams(..))
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
  -- The ONE boot-mode resolution (#1086). 'rejectIncompatibleFlags'
  -- just below and the dispatch at the bottom both consume THIS value,
  -- so the mode a rejection names can never be a different mode from
  -- the one that would have run; the precedence itself, and why it runs
  -- in that order, live in 'App.Cli.selectBootMode'.
  let bootMode = selectBootMode args
  -- Reject a mode-specific flag given to a boot mode that ignores it
  -- (CH-58) before any normalization warning or boot dispatch, so an
  -- ignored value can never appear to have taken effect.
  rejectIncompatibleFlags bootMode args
  -- Every handled value is parsed and validated HERE (#1191), after the
  -- mode-compatibility rejection above and before any mode-specific
  -- early exit, boot, or normalization warning. Two consequences are
  -- deliberate:
  --
  --   * Validation does not depend on whether the selected mode would
  --     go on to CONSUME the value. A malformed --port fails even for a
  --     bare grouped `--preview units`, which exits before ever using a
  --     port; a malformed --ages fails even when a valid --plates takes
  --     precedence over it below.
  --   * 'rejectIncompatibleFlags' keeps its priority. A flag the
  --     selected mode does not honour is still reported as unsupported
  --     in that mode — naming the mode, not the value — because it
  --     already exited above. So anything reaching this point is a flag
  --     the mode really does honour.
  --
  -- Order here is the order errors are reported in; only the first is
  -- shown, exactly like 'rejectIncompatibleFlags'.
  mDump   ← orExitCli (parseDump args)
  port    ← orExitCli (parseArg "--port" args)
  seed    ← orExitCli (parseArg "--seed" args)
  worldSz ← orExitCli (parseArg "--worldSize" args)
  -- `--plates` is the canonical flag; `--ages` is a legacy alias
  -- (its original name was misleading — the value is the plate
  -- count, not number of geological ages, which is rolled
  -- randomly inside buildTimeline).
  plates  ← orExitCli (parseArg "--plates" args)
  agesLeg ← orExitCli (parseArg "--ages" args)
  mSize   ← orExitCli (parseSize args)

  let bootProfile = if "--arena" `elem` args then BootArena else BootNormal
      mPreview = parsePreview args
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
  when (worldSize ≢ rawWorldSize) $
    hPutStrLn stderr $ "worldSize " ⧺ show rawWorldSize
        ⧺ " normalized to " ⧺ show worldSize
        ⧺ " (minimum/multiple " ⧺ show minimumWorldSize ⧺ ")."
  when (plateCount ≢ rawPlateCount) $
    hPutStrLn stderr $ "plateCount " ⧺ show rawPlateCount
        ⧺ " normalized to " ⧺ show plateCount
        ⧺ " (minimum 1)."

  case bootMode of
    SelectLanguageReport → case parseSeeds args of
      Just seeds → runLanguageReport seeds
      Nothing → do
          hPutStrLn stderr $ "--language-report requires --seeds LO:HI "
              ⧺ "(an inclusive range within 0.." ⧺ show (maxBound ∷ Word64) ⧺ ")"
          exitWith (ExitFailure 1)
    -- Field syntax, not three positional 'Int's: #1081. The seed's own
    -- default lives here because 'parseArg' answers absence with
    -- 'Nothing' and a malformed value with a 'Left' that already
    -- exited above. 'mDump' is necessarily 'Just' under 'SelectDump'
    -- (that is what selected it, and a malformed selection exited
    -- above); 'defaultLayers' is the bare @--dump@ answer either way.
    SelectDump → runDump (fromMaybe defaultLayers mDump)
        DumpGenParams { dgpSeed       = fromMaybe 42 seed
                      , dgpWorldSize  = worldSize
                      , dgpPlateCount = plateCount }
        region
    SelectPreview → case mPreview of
        Just (Just (cat, mItem)) → runPreviewTarget cat mItem port
        -- @--preview@ with no target at all. Plain 'Nothing' cannot
        -- occur here — 'parsePreview' answering 'Just' is what selected
        -- this mode — and would be the same user error if it did.
        _ → do
            hPutStrLn stderr $ "--preview requires a target, e.g. "
                ⧺ "--preview icons or --preview units/acolyte"
            exitWith (ExitFailure 1)
    -- Every port-taking mode passes the CLI's own 'Maybe' 'Int'
    -- through untouched (#1086): absence is 'Nothing', and
    -- 'App.Boot.patchBootConfig' resolves it against the one
    -- library-owned 'Engine.Core.Defaults.defaultDebugPort'.
    SelectOffscreen → runOffscreen bootProfile port mSize
    SelectHeadless  → runHeadless bootProfile port
    SelectGraphical → runGraphical bootProfile port

-- | Dispatch a resolved @--preview \<category\>[\/\<item\>]@ target.
--   Lifted out of 'main' by #1086's single boot-mode resolution; the
--   branches are exactly the ones that were nested inside it.
runPreviewTarget ∷ String → Maybe String → Maybe Int → IO ()
runPreviewTarget cat mItem port = case classifyPreviewCategory cat of
    UnknownPreviewCategory → do
        hPutStrLn stderr $ "Unrecognized preview category: " ⧺ cat
            ⧺ " (expected one of: " ⧺ intercalate ", "
                (simplePreviewCategories ⧺ groupedPreviewCategories)
            ⧺ ")"
        exitWith (ExitFailure 1)
    GroupedPreviewCategory → case mItem of
        Nothing → do
            putStrLn $ "select a specific " ⧺ cat
                ⧺ ", e.g. --preview units/acolyte"
            exitSuccess
        Just item → runGroupedPreview cat item port
    SimplePreviewCategory → case mItem of
        -- Bare simple category: recursively discover every texture
        -- under its root (#886 Requirement 3) — always succeeds (an
        -- empty/missing root just yields no entries, not a pre-boot
        -- error; every canonical simple category is a real, populated
        -- directory in this repo).
        Nothing → do
            entries ← discoverEntries (textureCategoryRoot cat)
            runPreview (T.pack cat, Nothing) (Just (PreviewList entries)) port
        -- Focused item: resolve + validate BEFORE ever creating a
        -- window (#886 Requirement 4) — absolute paths, ".." traversal,
        -- symlink escapes, directories, unsupported extensions, and
        -- plain nonexistence all reject here.
        Just item → resolveFocusedEntry (textureCategoryRoot cat) item ⌦ \case
            Left err → do
                hPutStrLn stderr $ "--preview " ⧺ cat ⧺ "/" ⧺ item
                    ⧺ ": " ⧺ T.unpack (focusErrorMessage err)
                exitWith (ExitFailure 1)
            Right entry →
                runPreview (T.pack cat, Just (T.pack item))
                           (Just (PreviewItem entry)) port

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
runGroupedPreview ∷ String → String → Maybe Int → IO ()
runGroupedPreview cat item port
    | cat ≡ "units" =
        buildPreviewUnit unitsCategoryRoot item ⌦ \case
            Left err → rejectItem (unitFocusErrorMessage err)
            Right unit → runPreview target (Just (PreviewUnitAnims unit)) port
    | cat ≡ "buildings" =
        buildPreviewBuilding (textureCategoryRoot cat) item ⌦ \case
            Left err → rejectItem (itemDirErrorMessage err)
            Right building →
                runPreview target (Just (PreviewBuildingAssets building)) port
    | otherwise =
        resolveItemDir (textureCategoryRoot cat) item ⌦ \case
            Left err → rejectItem (itemDirErrorMessage err)
            Right dir → do
                entries ← discoverEntries dir
                runPreview target (Just (PreviewList entries)) port
  where
    target = (T.pack cat, Just (T.pack item))
    rejectItem msg = do
        hPutStrLn stderr $ "--preview " ⧺ cat ⧺ "/" ⧺ item ⧺ ": "
            ⧺ T.unpack msg
        exitWith (ExitFailure 1)

-- | Take a parsed CLI value, or exit 1 reporting what the user actually
--   typed (#1191). The same pre-boot-rejection shape
--   'rejectIncompatibleFlags' and the @--preview@ target errors already
--   use: stderr, exit 1, nothing started.
orExitCli ∷ Either CliError a → IO a
orExitCli = either report pure
  where
    report err = do
        hPutStrLn stderr (cliErrorMessage err)
        exitWith (ExitFailure 1)

-- | Every ancillary (non-mode-selecting) flag Main parses, paired with
--   the boot mode(s) that actually honour it (CH-58) — everything else
--   silently discards it today. Detected by syntactic occurrence of the
--   flag token in argv, not by whether the flag's own value parses: a
--   malformed @--seed nonsense@ given to headless must still be rejected
--   here rather than quietly vanishing into 'parseArg's @Nothing@.
incompatibleFlagTable ∷ [(String, [BootModeSelection])]
incompatibleFlagTable =
    [ ("--seed",      [SelectDump])
    , ("--worldSize", [SelectDump])
    , ("--plates",    [SelectDump])
    , ("--ages",      [SelectDump])
    , ("--region",    [SelectDump])
    , ("--size",      [SelectOffscreen])
    , ("--seeds",     [SelectLanguageReport])
    , ("--arena",     [SelectHeadless, SelectGraphical, SelectOffscreen])
    , ("--port",      [SelectHeadless, SelectGraphical, SelectOffscreen
                      , SelectPreview])
    ]

-- | Exit 1 before any normalization warning or boot dispatch if argv
--   carries a flag from 'incompatibleFlagTable' that the selected boot
--   mode does not honour (CH-58) — the same pre-boot-rejection shape
--   this file already uses for a bare/unknown @--preview@ target.
rejectIncompatibleFlags ∷ BootModeSelection → [String] → IO ()
rejectIncompatibleFlags bootMode args = case violations of
    []               → pure ()
    (flag, honoured) : _ → do
        hPutStrLn stderr $ flag ⧺ " is not supported in "
            ⧺ bootModeSelectionName bootMode ⧺ " mode (only honoured in "
            ⧺ intercalate ", " (map bootModeSelectionName honoured) ⧺ ")"
        exitWith (ExitFailure 1)
  where
    violations =
        [ (flag, honoured)
        | (flag, honoured) ← incompatibleFlagTable
        , flag `elem` args
        , bootMode `notElem` honoured
        ]
