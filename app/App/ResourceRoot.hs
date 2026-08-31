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
--
--   That precedence is decided PURELY, by 'decideResourceRoot', from
--   argv and the environment value alone (#1949): every way the flag
--   can be wrong is an outcome of that one function rather than a
--   filesystem accident, so an operand the user typed can never be
--   answered by whichever directory the process happens to be sitting
--   in.
module App.ResourceRoot
  ( applyResourceRoot
  , RootDecision(..)
  , decideResourceRoot
  ) where

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

-- | What argv and @SYNARCHY_ROOT@ between them select, before any
--   filesystem is consulted. The two error constructors are the two
--   distinct ways @--resource-root@ can be present and unusable, and
--   they stay distinct: a bare flag and an empty operand are different
--   mistakes and get different messages.
data RootDecision
  = RootFlagMissingValue
    -- ^ A bare trailing @--resource-root@, with no operand after it.
  | RootFlagEmptyValue
    -- ^ @--resource-root ""@ — an operand the user typed, which names
    --   no path. It is NOT an alias for the current directory
    --   (@makeAbsolute ""@ resolves to one) and does NOT fall through
    --   to @SYNARCHY_ROOT@: the caller asked for an explicit root and
    --   supplied nothing to be one.
  | RootExplicit !FilePath !String
    -- ^ A root to chdir into: the raw operand, and the source label the
    --   diagnostics name it by.
  | RootCurrentDirectory
    -- ^ Nothing explicit was selected — validate the cwd in place and
    --   leave it alone.
  deriving (Eq, Show)

-- | The resource-root precedence, as a pure function of argv and the
--   @SYNARCHY_ROOT@ value. An empty environment variable is absence
--   (it always was); an empty FLAG operand is an error rather than
--   absence, because the flag being present at all is the user asking
--   for a specific root (#1949, the same present-but-malformed policy
--   #1191 gave the numeric and selector flags).
decideResourceRoot ∷ [String] → Maybe String → RootDecision
decideResourceRoot args mEnv = case parseStrArg "--resource-root" args of
    -- 'parseStrArg' is deliberately lenient — it answers a bare
    -- trailing flag with the same 'Nothing' as absence — so the
    -- presence of the token in argv is what separates the two here.
    Nothing | "--resource-root" `elem` args → RootFlagMissingValue
    Just "" → RootFlagEmptyValue
    Just p  → RootExplicit p "--resource-root"
    Nothing → case mEnv of
      Just p | not (null p) → RootExplicit p "SYNARCHY_ROOT"
      _                     → RootCurrentDirectory

-- | Resolve the resource root from args/environment, validate it, and
--   chdir into it. Exits with an actionable error (naming the missing
--   paths and the root in use) when the root doesn't exist or lacks a
--   resource family.
applyResourceRoot ∷ [String] → IO ()
applyResourceRoot args = do
  mEnv ← lookupEnv "SYNARCHY_ROOT"
  -- A bare `--resource-root` (no path following it) is an error, NOT
  -- "not present" — it must not silently fall back to the cwd default
  -- (same rule as bare `--preview` in Main). An empty operand is the
  -- other half of that rule and gets its own message.
  mExplicit ← case decideResourceRoot args mEnv of
    RootFlagMissingValue → dieFlag "--resource-root requires a path argument"
    RootFlagEmptyValue   →
      dieFlag $ "--resource-root: invalid value \"\" (the operand is"
          ⧺ " empty; expected a path to a synarchy checkout)"
    RootExplicit p src   → pure (Just (p, src))
    RootCurrentDirectory → pure Nothing
  let (rawRoot, source) = fromMaybe (".", "current directory") mExplicit
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

-- | Report a malformed @--resource-root@ operand and exit. Separate
--   from 'dieInvalidRoot': nothing here has been resolved to a path
--   yet, so there is no root to name — only the flag and what is wrong
--   with what followed it.
dieFlag ∷ String → IO a
dieFlag message = do
  hPutStrLn stderr $ "synarchy: " ⧺ message
  exitWith (ExitFailure 1)

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
