{-# LANGUAGE Strict #-}
-- | The scenario path policy (#2715, SCN-15 of epic #2698): the ONE place
--   that turns what a caller typed into the file the load and capture
--   services (#2713, #2714) are handed. Decisions D-49 and D-50 of
--   @docs/designs/predefined_arena_worlds_design.md@ are the contract; the
--   owner's decision of 2026-10-21 names the scenario folder. The policy
--   is documented once, in @docs/engine_contracts.md@ (§Scenario entry
--   points).
--
--   == The rules
--
--   * __Command line__ ('EntryCli'): a relative path resolves from the
--     directory that LAUNCHED Synarchy (captured before the resource root
--     changed the working directory); an absolute path stands as given.
--     Either way the file name carries its extension: @ambush.yaml@ is a
--     launch-relative file, @ambush@ is invalid, never a scenario name.
--   * __Lua and in-game__ ('EntryGame'): an absolute path stands as given
--     and carries its extension. Anything else is a NAME inside the
--     scenario folder (@scenarios\/@ under the selected resource root):
--     @ambush@ and @ambush.yaml@ both mean @scenarios\/ambush.yaml@, a name
--     may descend into subfolders, and it can never leave the folder —
--     not by @..@, not by a symbolic link. A missing name is NOT retried
--     against the launch directory or invented as an absolute path.
--   * __One extension__: @.yaml@, lower case. @.yml@ and every other
--     suffix are refused with the reason, never rewritten (D-50 names only
--     @.yaml@; the services read one format).
--   * __Loading__ also requires the resolved file to exist and be a
--     regular file, so a typo is refused before anything is admitted.
--   * __Capturing__ applies the same rules to the destination. The
--     scenario folder is created on the first capture that names a
--     folder-relative destination (and only then); loading never creates
--     it. The services keep deciding overwrite and the destination's
--     writability.
--
--   Everything the policy refuses carries the typed text and, where it
--   got that far, the resolved path, so the diagnostic is actionable
--   wherever it is shown.
module Scenario.Path
    ( -- * The policy's vocabulary
      scenarioFolderName
    , scenarioExtension
    , ScenarioEntry(..)
    , ScenarioUse(..)
    , ScenarioRoots(..)
    , scenarioFolder
      -- * Resolution
    , PathProblem(..)
    , pathProblemText
    , pathProblemReason
    , pathProblemResolved
    , resolveScenarioPath
      -- * The pure half, exposed for its tests
    , Lexical(..)
    , lexicalPath
    ) where

import UPrelude
import Control.Exception (IOException, try)
import Data.List (isPrefixOf)
import qualified Data.Text as T
import System.Directory
    ( canonicalizePath, createDirectoryIfMissing, doesDirectoryExist
    , doesFileExist, doesPathExist )
import System.FilePath
    ( isAbsolute, joinPath, normalise, splitDirectories, takeBaseName
    , takeExtension, takeFileName, (</>) )

-- | The scenario folder's name, directly under the resource root.
scenarioFolderName ∷ FilePath
scenarioFolderName = "scenarios"

-- | The one extension a scenario file carries.
scenarioExtension ∷ String
scenarioExtension = ".yaml"

-- | Which entry point the caller came in by.
data ScenarioEntry
    = EntryCli
      -- ^ @--scenario \<path\>@
    | EntryGame
      -- ^ @engine.loadScenario@ \/ @engine.saveScenario@, and anything in
      --   game that offers the same names
    deriving (Show, Eq)

-- | What the resolved path is for.
data ScenarioUse
    = UseLoad
    | UseCapture
    deriving (Show, Eq)

-- | The two directories the policy distinguishes. The launch directory is
--   captured BEFORE the resource root changes the working directory; the
--   resource root is the directory the process then runs in.
data ScenarioRoots = ScenarioRoots
    { srLaunchDir    ∷ !FilePath
    , srResourceRoot ∷ !FilePath
    } deriving (Show, Eq)

-- | The scenario folder of a resource root.
scenarioFolder ∷ ScenarioRoots → FilePath
scenarioFolder roots = srResourceRoot roots </> scenarioFolderName

-- | Why a path was refused.
data PathProblem
    = PathEmpty
      -- ^ nothing, or only white space
    | PathBadCharacter
      -- ^ a NUL, which no file name can hold
    | PathMissingExtension !FilePath
      -- ^ an extensionless command-line or absolute path
    | PathUnsupportedExtension !FilePath !String
      -- ^ the typed text and its suffix
    | PathNamesNoFile !FilePath
      -- ^ a trailing separator, or an extension with no name
    | PathTraversal !FilePath
      -- ^ an in-game name with a @..@ component
    | PathEscapesFolder !FilePath !FilePath
      -- ^ the typed text and the real location it reached (a symbolic
      --   link that leaves the scenario folder)
    | PathFolderUnavailable !FilePath !Text
      -- ^ the scenario folder could not be created, or is not a folder
    | PathNotFound !FilePath
      -- ^ the resolved file does not exist
    | PathNotAFile !FilePath
      -- ^ the resolved path exists and is not a regular file
    deriving (Show, Eq)

-- | One actionable sentence.
pathProblemText ∷ PathProblem → Text
pathProblemText = \case
    PathEmpty → "the scenario path is empty"
    PathBadCharacter → "the scenario path contains a NUL character"
    PathMissingExtension p →
        "the scenario path " <> quoted p <> " has no extension; the file name must end in "
        <> T.pack scenarioExtension
    PathUnsupportedExtension p ext →
        "the scenario path " <> quoted p <> " has the unsupported extension " <> quoted ext
        <> "; the only scenario extension is " <> T.pack scenarioExtension
    PathNamesNoFile p → "the scenario path " <> quoted p <> " names no file"
    PathTraversal p →
        "the scenario name " <> quoted p <> " leaves the scenario folder ('..'); use a name inside it, \
        \or an absolute path"
    PathEscapesFolder p real →
        "the scenario name " <> quoted p <> " resolves to " <> quoted real
        <> ", outside the scenario folder; use an absolute path to reach it"
    PathFolderUnavailable folder why →
        "the scenario folder " <> quoted folder <> " is unavailable: " <> why
    PathNotFound p → "no scenario file exists at " <> quoted p
    PathNotAFile p → "the scenario path " <> quoted p <> " is not a regular file"
  where quoted = tshow

-- | The machine-readable class of a refusal: @not_found@ for a file that
--   is not there, @invalid_path@ for everything else.
pathProblemReason ∷ PathProblem → Text
pathProblemReason = \case
    PathNotFound _ → "not_found"
    _ → "invalid_path"

-- | The resolved path a refusal got as far as naming, if any.
pathProblemResolved ∷ PathProblem → Maybe FilePath
pathProblemResolved = \case
    PathEscapesFolder _ real → Just real
    PathNotFound p → Just p
    PathNotAFile p → Just p
    _ → Nothing

-- | What the pure half decides: the path the text names, before the file
--   system is consulted.
data Lexical = Lexical
    { lxPath   ∷ !FilePath
      -- ^ the resolved path (absolute unless a root was relative)
    , lxInside ∷ !Bool
      -- ^ it is a name inside the scenario folder, so containment applies
    } deriving (Show, Eq)

-- | The pure half of the policy: extension, shape and root rules.
lexicalPath ∷ ScenarioEntry → ScenarioRoots → String → Either PathProblem Lexical
lexicalPath entry roots raw
    | T.null (T.strip (T.pack raw)) = Left PathEmpty
    | '\NUL' `elem` raw = Left PathBadCharacter
    | isAbsolute raw = withExtension raw ⌦ \p → Right (Lexical (normalise p) False)
    | entry ≡ EntryCli =
        withExtension raw ⌦ \p → Right (Lexical (normalise (srLaunchDir roots </> p)) False)
    | otherwise = do
        let comps = [ c | c ← splitDirectories raw, c ≢ "." ]
        when (".." `elem` comps) $ Left (PathTraversal raw)
        when (null comps) $ Left (PathNamesNoFile raw)
        named ← case takeExtension raw of
            "" | null (takeFileName raw) → Left (PathNamesNoFile raw)
               | otherwise → Right (joinPath comps ⧺ scenarioExtension)
            _ → withExtension (joinPath comps)
        Right (Lexical (normalise (scenarioFolder roots </> named)) True)
  where
    -- a path that must already carry the one supported extension
    withExtension p
        | null (takeFileName p) = Left (PathNamesNoFile raw)
        | otherwise = case takeExtension p of
            "" → Left (PathMissingExtension raw)
            ext | ext ≢ scenarioExtension → Left (PathUnsupportedExtension raw ext)
                | null (takeBaseName p) → Left (PathNamesNoFile raw)
                | otherwise → Right p

-- | Resolve what a caller typed into the file a service is handed, or the
--   reason it cannot be. Consults the file system for containment (real
--   locations, so a symbolic link cannot carry a name out of the folder),
--   for the existence of a file to load, and — for the first capture into
--   the scenario folder — to create that one folder.
resolveScenarioPath ∷ ScenarioEntry → ScenarioUse → ScenarioRoots → String
                    → IO (Either PathProblem FilePath)
resolveScenarioPath entry use roots raw = case lexicalPath entry roots raw of
    Left problem → pure (Left problem)
    Right (Lexical path inside) → do
        contained ← if inside then containment path else pure (Right ())
        case contained of
            Left problem → pure (Left problem)
            Right () → case use of
                UseCapture → pure (Right path)
                UseLoad → do
                    present ← doesPathExist path
                    isFile ← doesFileExist path
                    pure $ if not present then Left (PathNotFound path)
                           else if not isFile then Left (PathNotAFile path)
                           else Right path
  where
    folder = scenarioFolder roots
    -- the destination of a capture creates the folder; a load never does
    containment path = do
        made ← case use of
            UseCapture → ensureFolder
            UseLoad → pure (Right ())
        case made of
            Left problem → pure (Left problem)
            Right () → do
                present ← doesDirectoryExist folder
                if not present
                    then pure (Right ())
                    else do
                        realFolder ← canonicalizePath folder
                        real ← canonicalizePath path
                        pure $ if splitDirectories realFolder `isPrefixOf` splitDirectories real
                                  ∧ real ≢ realFolder
                            then Right ()
                            else Left (PathEscapesFolder raw real)
    ensureFolder = do
        made ← try (createDirectoryIfMissing False folder) ∷ IO (Either IOException ())
        isDir ← doesDirectoryExist folder
        pure $ case made of
            Left e → Left (PathFolderUnavailable folder (tshow e))
            Right ()
                | isDir → Right ()
                | otherwise → Left (PathFolderUnavailable folder "it is not a directory")
