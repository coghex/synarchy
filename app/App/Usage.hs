-- | @synarchy --help@ / @-h@ (#2863): the usage text, and the one
--   definition of every ancillary flag it describes.
--
--   Before this module an agent asking the binary what it accepted got
--   a graphical boot instead: @--help@ selected no mode, so it fell
--   through to the default graphical mode, which opened a window (or,
--   with no display, failed after engine initialization had already
--   written @config\/*.local.yaml@).
--
--   Drift is prevented by construction rather than by a comment:
--
--   * 'flagSpecs' IS the ancillary-flag table. @app\/Main.hs@'s
--     mode-compatibility rejection reads 'incompatibleFlagTable', a
--     projection of it, so a flag cannot be added to the rejection
--     without a value format and summary that 'usageText' prints.
--   * The boot modes are listed by enumerating 'BootModeSelection', and
--     'modeUsage' is a total match on it, so a new mode is a compile
--     error here until it is described.
--   * @--resource-root@ is spelled by 'App.ResourceRoot.resourceRootFlag',
--     the token 'App.ResourceRoot.decideResourceRoot' itself parses.
--
--   The hspec group @"App.Usage"@ pins the rest (each selector really
--   selects its mode, every flag appears in the text), and
--   @tools\/preview_cli_probe.py@ runs the real executable.
module App.Usage
  ( helpFlags
  , helpRequested
  , FlagSpec(..)
  , flagSpecs
  , incompatibleFlagTable
  , modeSelector
  , usageText
  ) where

import UPrelude
import Data.List (intercalate)
import App.Cli (BootModeSelection(..), bootModeSelectionName
               , dumpLayerNames, simplePreviewCategories
               , groupedPreviewCategories
               , ChunkRegion(..), defaultChunkRegion)
import App.ResourceRoot (resourceRootFlag)
import Engine.Core.Defaults (defaultDebugPort)

-- | The exact argv tokens that request help. Exact matches only:
--   @--helpme@ or @--help=x@ is still an unrecognized argument and is
--   ignored, as every unrecognized argument is.
helpFlags ∷ [String]
helpFlags = ["-h", "--help"]

-- | Whether argv asks for help. Any position wins, including the
--   operand position of another flag, because @app\/Main.hs@ answers it
--   before resource-root resolution, mode selection and value
--   validation: help never depends on the rest of argv being valid.
helpRequested ∷ [String] → Bool
helpRequested = any (`elem` helpFlags)

-- | One ancillary (non-mode-selecting) flag: its token, the operand it
--   takes (empty for a bare switch), the boot modes that honour it
--   (CH-58), and the one-line summary @--help@ prints.
data FlagSpec = FlagSpec
    { fsFlag     ∷ !String
    , fsValue    ∷ !String
    , fsHonoured ∷ ![BootModeSelection]
    , fsSummary  ∷ !String
    }

-- | Every ancillary flag @app\/Main.hs@ parses, in the order @--help@
--   lists them. See 'incompatibleFlagTable' for what the honoured
--   lists enforce.
flagSpecs ∷ [FlagSpec]
flagSpecs =
    [ FlagSpec "--seed" "N" [SelectDump] "world seed"
    , FlagSpec "--worldSize" "N" [SelectDump]
        "world side in chunks (rounded up to a whole region)"
    , FlagSpec "--plates" "N" [SelectDump]
        "tectonic plate count (default derived from the world size)"
    , FlagSpec "--ages" "N" [SelectDump]
        "legacy alias for --plates (--plates wins when both are given)"
    , FlagSpec "--region" "CX1,CY1,CX2,CY2" [SelectDump]
        ("inclusive chunk range to dump (default " ⧺ regionText ⧺ ")")
    , FlagSpec "--size" "WxH" [SelectOffscreen]
        "render size in pixels (default: the video config's resolution)"
    , FlagSpec "--seeds" "LO:HI" [SelectLanguageReport]
        "inclusive seed range to report (required)"
    , FlagSpec "--arena" "" [SelectHeadless, SelectGraphical, SelectOffscreen]
        "boot the arena profile instead of the normal game"
    , FlagSpec "--port" "N"
        [SelectHeadless, SelectGraphical, SelectOffscreen, SelectPreview]
        ("debug-console TCP port (default " ⧺ show defaultDebugPort ⧺ ")")
    ]
  where
    r = defaultChunkRegion
    regionText = intercalate "," (map show [crX1 r, crY1 r, crX2 r, crY2 r])

-- | Every ancillary flag, paired with the boot mode(s) that actually
--   honour it (CH-58). @app\/Main.hs@'s @rejectIncompatibleFlags@ exits
--   1 before any boot when argv carries one of these flags and the
--   selected mode is not in its list. Detected by syntactic occurrence
--   of the flag token in argv, not by whether the flag's own value
--   parses: a malformed @--seed nonsense@ given to headless must still
--   be rejected rather than quietly vanishing into 'App.Cli.parseArg's
--   @Nothing@.
incompatibleFlagTable ∷ [(String, [BootModeSelection])]
incompatibleFlagTable = [ (fsFlag f, fsHonoured f) | f ← flagSpecs ]

-- | The argv token that selects a mode in 'App.Cli.selectBootMode';
--   'Nothing' for graphical, which is what argv naming no selector
--   resolves to.
modeSelector ∷ BootModeSelection → Maybe String
modeSelector SelectLanguageReport = Just "--language-report"
modeSelector SelectDump           = Just "--dump"
modeSelector SelectPreview        = Just "--preview"
modeSelector SelectOffscreen      = Just "--offscreen"
modeSelector SelectHeadless       = Just "--headless"
modeSelector SelectGraphical      = Nothing

-- | How @--help@ writes a mode: its argv form and what it does.
modeUsage ∷ BootModeSelection → (String, String)
modeUsage m = case m of
    SelectLanguageReport → ( selector ⧺ " --seeds LO:HI"
        , "print the generated-language report for the seed range as "
          ⧺ "JSON, then exit (no engine)" )
    SelectDump → ( selector ⧺ "[=LAYERS]"
        , "generate a world with no window or GPU, print its tiles as "
          ⧺ "JSON on stdout, then exit. LAYERS is a comma-separated "
          ⧺ "subset of " ⧺ intercalate ", " dumpLayerNames
          ⧺ " (bare --dump: every layer but slope)" )
    SelectPreview → ( selector ⧺ " TARGET"
        , "open an asset preview window. TARGET is CATEGORY[/ITEM], "
          ⧺ "CATEGORY one of " ⧺ intercalate ", " previewCategories
          ⧺ ", or a WAV/FLAC/MP3 file path" )
    SelectOffscreen → ( selector
        , "run the game with Vulkan rendering and no window; debug "
          ⧺ "console on --port" )
    SelectHeadless → ( selector
        , "run the game with no window and no GPU; debug console on "
          ⧺ "--port" )
    SelectGraphical → ( "(no mode flag)"
        , "run the game in a window (the default)" )
  where
    selector = fromMaybe "" (modeSelector m)
    previewCategories =
        simplePreviewCategories ⧺ groupedPreviewCategories ⧺ ["audio"]

-- | The complete @--help@ text, newline-terminated. Pure, so the hspec
--   group can check it without a process.
usageText ∷ String
usageText = unlines $
    [ "Usage: synarchy [MODE] [OPTIONS]"
    , ""
    , "Modes (at most one; if several are given, the first listed here wins):"
    ]
    ⧺ concat [ entry form (bootModeSelectionName m ⧺ ": " ⧺ what)
             | m ← [minBound .. maxBound]
             , let (form, what) = modeUsage m ]
    ⧺ [ ""
      , "Options (each is accepted only by the modes in brackets; giving"
      , "one to any other mode exits 1 before anything starts):"
      ]
    ⧺ concat [ entry (spec f) (fsSummary f ⧺ " [" ⧺ modes f ⧺ "]")
             | f ← flagSpecs ]
    ⧺ [ ""
      , "Global options:"
      ]
    ⧺ entry (resourceRootFlag ⧺ " PATH")
        ("run against the synarchy checkout at PATH (default: "
         ⧺ "$SYNARCHY_ROOT, else the current directory) [all modes]")
    ⧺ entry (intercalate ", " helpFlags)
        "print this help and exit, ignoring every other argument"
    ⧺ [ ""
      , "Arguments not listed here are ignored."
      ]
  where
    spec f | null (fsValue f) = fsFlag f
           | otherwise        = fsFlag f ⧺ " " ⧺ fsValue f
    modes = intercalate ", " ∘ map bootModeSelectionName ∘ fsHonoured
    -- The form on its own line, the description wrapped beneath it:
    -- forms vary too much in width for a readable two-column table.
    entry form what = ("  " ⧺ form) : map ("      " ⧺) (wrap 72 what)

-- | Greedy word wrap at @width@ columns; a single over-long word gets a
--   line of its own rather than being split.
wrap ∷ Int → String → [String]
wrap width = go [] 0 ∘ words
  where
    go acc _ [] = [unwords (reverse acc) | not (null acc)]
    go [] _ (w:ws) = go [w] (length w) ws
    go acc n (w:ws)
        | n + 1 + length w ≤ width = go (w : acc) (n + 1 + length w) ws
        | otherwise = unwords (reverse acc) : go [w] (length w) ws
