{-# LANGUAGE OverloadedStrings #-}
-- | "Scenario.Paths" (#2715, SCN-15 of epic #2698): the scenario path
--   policy ("Scenario.Path"), D-49 and D-50 and the owner's choice of the
--   scenario folder, against REAL directories — a launch directory and a
--   resource root that are different places, files that exist in one and
--   not the other, symbolic links in and out of the scenario folder — for
--   loading AND for capture destinations.
--
--   The properties pin the invariants the examples sample: an in-game name
--   never leaves the folder, a command-line path never leaves the launch
--   directory unless it is absolute, an accepted path always carries the
--   one extension, and resolving a resolved path changes nothing.
--
--   Run just this gate: @cabal test synarchy-test-headless
--   --test-options='--match "Scenario.Paths"'@.
module Test.Headless.Scenario.Paths (spec) where

import UPrelude
import Test.Hspec
import Test.QuickCheck
import Control.Exception (bracket)
import Control.Monad ((>=>))
import Data.List (isInfixOf, isPrefixOf)
import qualified Data.Text as T
import System.Directory
import System.FilePath
import System.Random (randomRIO)
import Scenario.Path

-- | The directories one example works in.
data Tree = Tree
    { tLaunch   ∷ FilePath
    , tResource ∷ FilePath
    , tFolder   ∷ FilePath
    , tOutside  ∷ FilePath
    }

roots ∷ Tree → ScenarioRoots
roots t = ScenarioRoots { srLaunchDir = tLaunch t, srResourceRoot = tResource t }

-- | A scratch directory under the system's, removed afterwards. Its name
--   has a space in it: the policy must not care.
withScratch ∷ String → (FilePath → IO α) → IO α
withScratch label act = bracket make removePathForcibly (canonicalizePath >=> act)
  where
    make = do
        tmp ← getTemporaryDirectory
        n ← randomRIO (0 ∷ Int, 10 ^ (9 ∷ Int))
        let dir = tmp </> (label ⧺ " " ⧺ show n)
        createDirectoryIfMissing True dir
        pure dir

withTree ∷ (Tree → IO α) → IO α
withTree act = withScratch "scenario paths" $ \base → do
    let launch = base </> "launch dir"
        resource = base </> "resource root"
        folder = resource </> "scenarios"
        outside = base </> "elsewhere"
    forM_ [launch, folder, outside, folder </> "sub", launch </> "sub"] $
        createDirectoryIfMissing True
    -- the same name exists in BOTH places, so a fallback would be visible
    forM_ [ folder </> "ambush.yaml", launch </> "ambush.yaml", launch </> "rel.yaml"
          , folder </> "sub" </> "deep.yaml", launch </> "launch-only.yaml"
          , folder </> "ünï cødé \"q\" 'x'.yaml", outside </> "out.yaml"
          , folder </> "sub" </> "twin.yaml" ] $ \f → writeFile f "version: 3\n"
    createDirectory (folder </> "adir.yaml")
    -- a link out of the folder, a link to a folder outside, and a link that
    -- stays inside
    createFileLink (outside </> "out.yaml") (folder </> "link.yaml")
    createDirectoryLink outside (folder </> "dirlink")
    createFileLink (folder </> "ambush.yaml") (folder </> "inside-link.yaml")
    act (Tree launch resource folder outside)

-- | A tree whose resource root has NO scenario folder yet.
withBareTree ∷ (Tree → IO α) → IO α
withBareTree act = withScratch "scenario bare" $ \base → do
    let launch = base </> "launch"
        resource = base </> "resource"
    mapM_ (createDirectoryIfMissing True) [launch, resource]
    act (Tree launch resource (resource </> "scenarios") (base </> "elsewhere"))

resolveLoad ∷ ScenarioEntry → Tree → String → IO (Either PathProblem FilePath)
resolveLoad entry t = resolveScenarioPath entry UseLoad (roots t)

resolveSave ∷ ScenarioEntry → Tree → String → IO (Either PathProblem FilePath)
resolveSave entry t = resolveScenarioPath entry UseCapture (roots t)

spec ∷ Spec
spec = describe "Scenario.Paths" $ do
    cliSpec
    gameSpec
    absoluteSpec
    captureSpec
    folderSpec
    propertySpec

-- * Command line

cliSpec ∷ Spec
cliSpec = around withTree $ describe "command-line paths (D-49, D-50)" $ do

    it "resolves a relative path from the LAUNCH directory, not the resource root" $ \t → do
        resolveLoad EntryCli t "rel.yaml" `shouldReturn` Right (tLaunch t </> "rel.yaml")

    it "treats ambush.yaml as a launch-relative file even when the scenario folder holds one too" $ \t → do
        resolveLoad EntryCli t "ambush.yaml" `shouldReturn` Right (tLaunch t </> "ambush.yaml")

    it "resolves nested relative paths and ./ prefixes against the launch directory" $ \t → do
        -- lexical: the file system, not the policy, folds the '..'
        resolveLoad EntryCli t "sub/../rel.yaml"
            `shouldReturn` Right (normalise (tLaunch t </> "sub/../rel.yaml"))
        resolveLoad EntryCli t "./rel.yaml" `shouldReturn` Right (tLaunch t </> "rel.yaml")

    it "requires the extension: an extensionless name is invalid, never a scenario-folder lookup" $ \t → do
        resolveLoad EntryCli t "ambush" `shouldReturn` Left (PathMissingExtension "ambush")
        resolveLoad EntryCli t "sub/ambush" `shouldReturn` Left (PathMissingExtension "sub/ambush")

    it "does not fall back to the scenario folder for a missing launch-relative file" $ \t → do
        -- the folder holds sub/deep.yaml; the launch directory only has sub/ with no such file
        r ← resolveLoad EntryCli t "sub/deep.yaml"
        r `shouldBe` Left (PathNotFound (tLaunch t </> "sub" </> "deep.yaml"))

    it "reports a missing file with its resolved path" $ \t → do
        resolveLoad EntryCli t "nope.yaml" `shouldReturn` Left (PathNotFound (tLaunch t </> "nope.yaml"))

    it "lets a command-line path leave the launch directory (it is the user's own path)" $ \t → do
        resolveLoad EntryCli t "../elsewhere/out.yaml"
            `shouldReturn` Right (normalise (tLaunch t </> "../elsewhere/out.yaml"))

    it "refuses an empty, blank or NUL-bearing path" $ \t → do
        resolveLoad EntryCli t "" `shouldReturn` Left PathEmpty
        resolveLoad EntryCli t "   " `shouldReturn` Left PathEmpty
        resolveLoad EntryCli t "a\NULb.yaml" `shouldReturn` Left PathBadCharacter

    it "refuses a final '.' component, which names a directory whatever precedes it" $ \t → do
        forM_ ["rel.yaml/.", "./rel.yaml/.", "sub/./.", ".", "./"] $ \raw →
            resolveLoad EntryCli t raw `shouldReturn` Left (PathNamesNoFile raw)

    it "refuses a directory and a name with no stem" $ \t → do
        resolveLoad EntryCli t "sub/" `shouldReturn` Left (PathNamesNoFile "sub/")
        resolveLoad EntryCli t ".yaml" `shouldReturn` Left (PathNamesNoFile ".yaml")

-- * In game

gameSpec ∷ Spec
gameSpec = around withTree $ describe "in-game names (D-49, D-50)" $ do

    it "resolves a bare name and the same name with its extension to the same file in the scenario folder" $ \t → do
        resolveLoad EntryGame t "ambush" `shouldReturn` Right (tFolder t </> "ambush.yaml")
        resolveLoad EntryGame t "ambush.yaml" `shouldReturn` Right (tFolder t </> "ambush.yaml")

    it "resolves nested names inside the folder" $ \t → do
        resolveLoad EntryGame t "sub/deep" `shouldReturn` Right (tFolder t </> "sub" </> "deep.yaml")
        resolveLoad EntryGame t "./sub/deep.yaml" `shouldReturn` Right (tFolder t </> "sub" </> "deep.yaml")

    it "carries spaces, quotes and Unicode through unchanged" $ \t → do
        resolveLoad EntryGame t "ünï cødé \"q\" 'x'"
            `shouldReturn` Right (tFolder t </> "ünï cødé \"q\" 'x'.yaml")

    it "never falls back to the launch directory for a missing name" $ \t → do
        -- launch-only.yaml exists only in the launch directory
        resolveLoad EntryGame t "launch-only"
            `shouldReturn` Left (PathNotFound (tFolder t </> "launch-only.yaml"))
        resolveLoad EntryGame t "rel.yaml"
            `shouldReturn` Left (PathNotFound (tFolder t </> "rel.yaml"))

    it "refuses traversal out of the folder, however it is spelled" $ \t → do
        forM_ ["../rel", "../rel.yaml", "sub/../../rel", "sub/..", "a/../../b.yaml"] $ \name →
            resolveLoad EntryGame t name `shouldReturn` Left (PathTraversal name)

    it "refuses a symbolic link that leaves the folder, to a file and to a folder" $ \t → do
        r1 ← resolveLoad EntryGame t "link"
        r1 `shouldSatisfy` escapes
        r2 ← resolveLoad EntryGame t "dirlink/out"
        r2 `shouldSatisfy` escapes

    it "accepts a symbolic link that stays inside the folder" $ \t → do
        resolveLoad EntryGame t "inside-link" `shouldReturn` Right (tFolder t </> "inside-link.yaml")

    it "refuses every extension but .yaml, naming it" $ \t → do
        resolveLoad EntryGame t "ambush.yml" `shouldReturn` Left (PathUnsupportedExtension "ambush.yml" ".yml")
        resolveLoad EntryGame t "ambush.YAML" `shouldReturn` Left (PathUnsupportedExtension "ambush.YAML" ".YAML")
        resolveLoad EntryGame t "ambush.txt" `shouldReturn` Left (PathUnsupportedExtension "ambush.txt" ".txt")
        resolveLoad EntryGame t "v1.2" `shouldReturn` Left (PathUnsupportedExtension "v1.2" ".2")

    it "refuses a final '.' component rather than folding it into the file before it" $ \t → do
        -- 'ambush.yaml/.' must not load (or overwrite) scenarios/ambush.yaml
        forM_ ["ambush.yaml/.", "ambush/.", "sub/deep.yaml/./.", ".", "./", "sub/."] $ \raw → do
            resolveLoad EntryGame t raw `shouldReturn` Left (PathNamesNoFile raw)
            resolveSave EntryGame t raw `shouldReturn` Left (PathNamesNoFile raw)

    it "refuses an empty name, a trailing separator and a directory" $ \t → do
        resolveLoad EntryGame t "" `shouldReturn` Left PathEmpty
        resolveLoad EntryGame t "sub/" `shouldReturn` Left (PathNamesNoFile "sub/")
        resolveLoad EntryGame t "adir" `shouldReturn` Left (PathNotAFile (tFolder t </> "adir.yaml"))

    it "reports the typed text and the resolved path in its diagnostic" $ \t → do
        Left problem ← resolveLoad EntryGame t "nothing-here"
        T.unpack (pathProblemText problem)
            `shouldSatisfy` isInfixOf (tFolder t </> "nothing-here.yaml")
        pathProblemReason problem `shouldBe` "not_found"
        Left traversal ← resolveLoad EntryGame t "../x"
        pathProblemReason traversal `shouldBe` "invalid_path"
        T.unpack (pathProblemText traversal) `shouldSatisfy` ("../x" `isInfixOf`)
  where
    escapes = \case
        Left (PathEscapesFolder _ _) → True
        _ → False

-- * Absolute paths

absoluteSpec ∷ Spec
absoluteSpec = around withTree $ describe "absolute paths" $ do

    it "work through both entry points, unchanged, with different launch and resource roots" $ \t → do
        let p = tOutside t </> "out.yaml"
        resolveLoad EntryCli t p `shouldReturn` Right p
        resolveLoad EntryGame t p `shouldReturn` Right p

    it "need the extension through both entry points" $ \t → do
        let p = tOutside t </> "out"
        resolveLoad EntryCli t p `shouldReturn` Left (PathMissingExtension p)
        resolveLoad EntryGame t p `shouldReturn` Left (PathMissingExtension p)

    it "are not confined to the scenario folder in game, and symlinks are followed" $ \t → do
        let p = tFolder t </> "link.yaml"
        resolveLoad EntryGame t p `shouldReturn` Right p

    it "refuse a final '.' component through both entry points" $ \t → do
        let p = tOutside t </> "out.yaml" ++ "/."
        resolveLoad EntryCli t p `shouldReturn` Left (PathNamesNoFile p)
        resolveLoad EntryGame t p `shouldReturn` Left (PathNamesNoFile p)

    it "are never invented from a bare name" $ \t → do
        resolveLoad EntryGame t "out" `shouldReturn` Left (PathNotFound (tFolder t </> "out.yaml"))
        resolveLoad EntryCli t "out" `shouldReturn` Left (PathMissingExtension "out")

    it "report a missing absolute file by its own path" $ \t → do
        let p = tOutside t </> "gone.yaml"
        resolveLoad EntryGame t p `shouldReturn` Left (PathNotFound p)

-- * Capture destinations

captureSpec ∷ Spec
captureSpec = describe "capture destinations follow the same rules" $ do
    around withTree $ do
        it "resolve a bare name into the scenario folder, existing file or not" $ \t → do
            resolveSave EntryGame t "fresh" `shouldReturn` Right (tFolder t </> "fresh.yaml")
            resolveSave EntryGame t "ambush" `shouldReturn` Right (tFolder t </> "ambush.yaml")
            resolveSave EntryGame t "fresh.yaml" `shouldReturn` Right (tFolder t </> "fresh.yaml")

        it "refuse traversal, a foreign extension and a symlink out of the folder" $ \t → do
            resolveSave EntryGame t "../fresh" `shouldReturn` Left (PathTraversal "../fresh")
            resolveSave EntryGame t "fresh.yml" `shouldReturn` Left (PathUnsupportedExtension "fresh.yml" ".yml")
            r ← resolveSave EntryGame t "link"
            r `shouldSatisfy` either (\case PathEscapesFolder _ _ → True; _ → False) (const False)
            r2 ← resolveSave EntryGame t "dirlink/new"
            r2 `shouldSatisfy` either (\case PathEscapesFolder _ _ → True; _ → False) (const False)

        it "take an absolute destination as given, and still require the extension" $ \t → do
            let p = tOutside t </> "new.yaml"
            resolveSave EntryGame t p `shouldReturn` Right p
            resolveSave EntryGame t (tOutside t </> "new") `shouldReturn` Left (PathMissingExtension (tOutside t </> "new"))

        it "leave a missing parent folder for the capture service to report" $ \t → do
            resolveSave EntryGame t "nodir/new" `shouldReturn` Right (tFolder t </> "nodir" </> "new.yaml")

    around withBareTree $ do
        it "create the scenario folder on the first folder-relative capture, once" $ \t → do
            doesDirectoryExist (tFolder t) `shouldReturn` False
            resolveSave EntryGame t "first" `shouldReturn` Right (tFolder t </> "first.yaml")
            doesDirectoryExist (tFolder t) `shouldReturn` True
            -- a second capture finds it
            resolveSave EntryGame t "second" `shouldReturn` Right (tFolder t </> "second.yaml")

        it "never create it for a load, which reports the missing file instead" $ \t → do
            resolveLoad EntryGame t "first" `shouldReturn` Left (PathNotFound (tFolder t </> "first.yaml"))
            doesDirectoryExist (tFolder t) `shouldReturn` False

        it "never create it for a refused capture" $ \t → do
            _ ← resolveSave EntryGame t "../x"
            _ ← resolveSave EntryGame t "x.txt"
            _ ← resolveSave EntryGame t ""
            doesDirectoryExist (tFolder t) `shouldReturn` False

        it "never create it for a destination ending in a '.' component" $ \t → do
            _ ← resolveSave EntryGame t "first.yaml/."
            doesDirectoryExist (tFolder t) `shouldReturn` False

        it "never create it for a command-line or absolute destination" $ \t → do
            _ ← resolveSave EntryCli t "x.yaml"
            _ ← resolveSave EntryGame t (tLaunch t </> "x.yaml")
            doesDirectoryExist (tFolder t) `shouldReturn` False

-- * The folder itself

folderSpec ∷ Spec
folderSpec = describe "the scenario folder" $ do
    it "is the top-level scenarios/ directory under the resource root" $ do
        scenarioFolderName `shouldBe` "scenarios"
        scenarioFolder (ScenarioRoots "/launch" "/res") `shouldBe` "/res/scenarios"
        scenarioExtension `shouldBe` ".yaml"

    around withBareTree $
        it "refuses a capture when the scenario 'folder' is a file" $ \t → do
            writeFile (tFolder t) "not a folder"
            r ← resolveSave EntryGame t "x"
            r `shouldSatisfy` either (\case PathFolderUnavailable _ _ → True; _ → False) (const False)

-- * Properties

propertySpec ∷ Spec
propertySpec = describe "invariants" $ do
    let rs = ScenarioRoots { srLaunchDir = "/launch/dir", srResourceRoot = "/resource/root" }
        folder = scenarioFolder rs

    it "an in-game name is accepted only as a file INSIDE the scenario folder, carrying the extension" $
        property $ forAll (nameOf nameAlphabet) $ \name →
            not (isAbsolute name) ==> case lexicalPath EntryGame rs name of
                Left _ → True
                Right lx →
                    lxInside lx
                    ∧ (splitDirectories folder `isPrefixOf` splitDirectories (lxPath lx))
                    ∧ (".." `notElem` splitDirectories (lxPath lx))
                    ∧ takeExtension (lxPath lx) ≡ ".yaml"

    it "a name with a '..' component is never accepted in game" $
        property $ forAll (nameOf nameAlphabet) $ \name →
            (".." `elem` splitDirectories name ∧ not (isAbsolute name)) ==>
                either (const True) (const False) (lexicalPath EntryGame rs name)

    it "a relative command-line path is accepted only under the launch directory's resolution and with the extension" $
        property $ forAll (nameOf cliAlphabet) $ \name →
            not (isAbsolute name) ==> case lexicalPath EntryCli rs name of
                Left _ → True
                Right lx →
                    not (lxInside lx)
                    ∧ takeExtension (lxPath lx) ≡ ".yaml"
                    ∧ lxPath lx ≡ normalise ("/launch/dir" </> name)

    it "a path whose final component is '.' is never accepted, by any entry point or shape" $
        property $ forAll (nameOf (nameAlphabet ⧺ ["/abs"])) $ \name →
            let dotted = name ⧺ "/."
            in all (\e → either (const True) (const False) (lexicalPath e rs dotted))
                   [EntryGame, EntryCli]

    it "an extensionless command-line path is never accepted" $
        property $ forAll (nameOf cliAlphabet) $ \name →
            null (takeExtension name) ==>
                either (const True) (const False) (lexicalPath EntryCli rs name)

    it "resolving an accepted path as an absolute path through either entry point changes nothing" $
        property $ forAll (nameOf nameAlphabet) $ \name →
            case lexicalPath EntryGame rs name of
                Left _ → True
                Right lx → all (\e → fmap lxPath (lexicalPath e rs (lxPath lx)) ≡ Right (lxPath lx))
                               [EntryGame, EntryCli]

    it "no accepted path, from either entry point, holds a NUL or ends without the extension" $
        property $ forAll (nameOf (nameAlphabet ⧺ ["\NUL", " "])) $ \name →
            all (\e → case lexicalPath e rs name of
                        Left _ → True
                        Right lx → '\NUL' `notElem` lxPath lx ∧ ".yaml" `isSuffixOf'` lxPath lx)
                [EntryGame, EntryCli]
  where
    isSuffixOf' suffix s = reverse suffix `isPrefixOf` reverse s
    nameAlphabet = ["a", "b", "..", ".", "sub", "ünï", "x y", "q\"q", "n.yaml", "n.yml", "n.", ".yaml", ""]
    cliAlphabet = nameAlphabet ⧺ ["/abs", "/", "n.yaml"]
    -- a path assembled from separator-joined components
    nameOf alphabet = do
        k ← choose (0, 5)
        parts ← vectorOf k (elements alphabet)
        trailing ← elements ["", "/"]
        pure (foldr1' (\a b → a ⧺ "/" ⧺ b) parts ⧺ trailing)
    foldr1' _ [] = ""
    foldr1' f xs = foldr1 f xs
