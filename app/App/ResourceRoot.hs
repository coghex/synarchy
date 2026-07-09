-- | Runtime resource-root resolution (#636).
--
--   Every runtime resource family — @scripts/@, @assets/@, @data/@,
--   @config/@ — is loaded by paths relative to the process working
--   directory, from hundreds of call sites across Haskell and Lua.
--   Rather than thread an explicit root through all of them, the
--   executable resolves ONE resource root at startup and chdirs into
--   it: every existing relative path then works unchanged from any
--   launch directory.
--
--   Precedence: @--resource-root PATH@ flag, then the @SYNARCHY_ROOT@
--   environment variable, then the current working directory (the
--   repo-root development default — no flags or environment needed
--   there). The root is validated up front either way, so a launch
--   from the wrong directory fails with an error naming the missing
--   paths and the root in use instead of a scatter of downstream
--   file-not-found failures.
module App.ResourceRoot (applyResourceRoot) where

import UPrelude
import Control.Monad (filterM)
import Data.List (intercalate)
import System.Directory (doesDirectoryExist, makeAbsolute
                        , setCurrentDirectory)
import System.Environment (lookupEnv)
import System.Exit (exitWith, ExitCode(..))
import System.IO (hPutStrLn, stderr)
import App.Cli (parseStrArg)

-- | The directories a valid resource root must contain — the four
--   runtime resource families the engine loads by relative path.
resourceFamilies ∷ [FilePath]
resourceFamilies = ["scripts", "assets", "data", "config"]

-- | Resolve the resource root from args/environment, validate it, and
--   chdir into it. Exits with an actionable error (naming the missing
--   paths and the root in use) when the root doesn't exist or lacks a
--   resource family.
applyResourceRoot ∷ [String] → IO ()
applyResourceRoot args = do
  -- A bare `--resource-root` (no path following it) is an error, NOT
  -- "not present" — it must not silently fall back to the cwd default
  -- (same rule as bare `--preview` in Main).
  when ("--resource-root" `elem` args
        ∧ isNothing (parseStrArg "--resource-root" args)) $ do
    hPutStrLn stderr "synarchy: --resource-root requires a path argument"
    exitWith (ExitFailure 1)
  mEnv ← lookupEnv "SYNARCHY_ROOT"
  let mExplicit = case parseStrArg "--resource-root" args of
        Just p  → Just (p, "--resource-root")
        Nothing → case mEnv of
          Just p | not (null p) → Just (p, "SYNARCHY_ROOT")
          _                     → Nothing
      (rawRoot, source) = fromMaybe (".", "current directory") mExplicit
  root ← makeAbsolute rawRoot
  rootExists ← doesDirectoryExist root
  unless rootExists $
    dieInvalidRoot source root ["no such directory"]
  missing ← filterM (fmap not ∘ doesDirectoryExist ∘ (root ⊘))
                    resourceFamilies
  unless (null missing) $
    dieInvalidRoot source root ["missing " ⧺ (root ⊘ d) | d ← missing]
  case mExplicit of
    Nothing → pure ()  -- already running in the root; leave cwd alone
    Just _  → do
      setCurrentDirectory root
      -- stderr, not stdout: --dump's stdout must stay pure JSON and
      -- --headless's stdout carries the READY line.
      hPutStrLn stderr $ "resource root: " ⧺ root ⧺ " (from " ⧺ source ⧺ ")"

-- | Report an unusable resource root and exit. Names the root, where it
--   came from, and each missing path, so a bad launch directory is
--   diagnosable from the message alone.
dieInvalidRoot ∷ String → FilePath → [String] → IO ()
dieInvalidRoot source root problems = do
  hPutStrLn stderr $ "synarchy: invalid resource root " ⧺ root
      ⧺ " (from " ⧺ source ⧺ ")"
  mapM_ (hPutStrLn stderr ∘ ("  " ⧺)) problems
  hPutStrLn stderr $ "expected a directory containing "
      ⧺ intercalate ", " (map (⧺ "/") resourceFamilies)
      ⧺ " (a synarchy checkout). Launch from the repo root, or point"
      ⧺ " --resource-root (or SYNARCHY_ROOT) at one."
  exitWith (ExitFailure 1)
