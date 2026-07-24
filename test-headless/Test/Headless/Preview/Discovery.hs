{-# LANGUAGE UnicodeSyntax, OverloadedStrings #-}
-- | Pure(ish) tests for 'Engine.Preview.Discovery' (#886, Phase 2 of the
--   @--preview@ texture browser epic #427): recursive discovery,
--   category-relative labeling, deterministic ordering, non-texture
--   exclusion, nested focused-path resolution, and containment
--   rejection. No engine needed — every function here is filesystem-only.
module Test.Headless.Preview.Discovery (spec) where

import UPrelude
import Test.Hspec
import Control.Exception (finally)
import Data.List (sort)
import qualified Data.Text as T
import System.Directory
    ( getTemporaryDirectory, createDirectoryIfMissing, removeDirectoryRecursive
    , doesDirectoryExist )
import System.FilePath ((</>))
import Engine.Core.Types (PreviewEntry(..))
import Engine.Preview.Discovery

-- A real, always-present repo asset (mirrors Test.Headless.Asset.TextureFallback's
-- 'realAsset' convention) — proves 'textureCategoryRoot' + resolution
-- against the ACTUAL canonical layout, not just a synthetic fixture.
realCategoryRoot ∷ FilePath
realCategoryRoot = textureCategoryRoot "icons"

realItem ∷ String
realItem = "skill/climbing.png"

-- A private fixture directory for the cases that need control over
-- non-.png files / nested depth / an unsupported extension — none of
-- which exist under any real simple-category root in this repo (every
-- asset is already a .png).
withFixture ∷ (FilePath → IO ()) → IO ()
withFixture action = do
    tmp ← getTemporaryDirectory
    let root = tmp </> "synarchy-preview-discovery-spec"
    createDirectoryIfMissing True (root </> "sub")
    writeFile (root </> "a.png") ""
    writeFile (root </> "sub" </> "b.png") ""
    writeFile (root </> "sub" </> "c.PNG") ""        -- extension case-insensitivity
    writeFile (root </> "notes.txt") ""              -- must be excluded
    writeFile (root </> "sub" </> "readme.md") ""    -- must be excluded
    (`finally` removeDirectoryRecursive root) (action root)

spec ∷ Spec
spec = do
    describe "textureCategoryRoot" $
        it "joins assets/textures/<category>" $
            textureCategoryRoot "icons" `shouldBe` ("assets" </> "textures" </> "icons")

    describe "isSupportedTextureFile" $ do
        it "accepts .png" $
            isSupportedTextureFile "a/b/c.png" `shouldBe` True
        it "is case-insensitive on the extension" $
            isSupportedTextureFile "a/b/c.PNG" `shouldBe` True
        it "rejects other extensions" $
            isSupportedTextureFile "a/b/c.jpg" `shouldBe` False
        it "rejects an extensionless name" $
            isSupportedTextureFile "a/b/c" `shouldBe` False

    describe "sortEntries" $
        it "orders lexicographically by label, case-sensitive" $ do
            let mk l = PreviewEntry { peLabel = l, pePath = l }
                entries = map mk ["z.png", "A.png", "a.png", "m/x.png"]
            map peLabel (sortEntries entries)
                `shouldBe` ["A.png", "a.png", "m/x.png", "z.png"]

    describe "discoverEntries" $ do
        it "is empty for a nonexistent root (never an error)" $ do
            entries ← discoverEntries "assets/textures/does-not-exist-886"
            entries `shouldBe` []

        it "recursively discovers real repo textures with / -separated, \
           \category-relative labels, sorted, extension included" $ do
            entries ← discoverEntries realCategoryRoot
            map (T.unpack . peLabel) entries `shouldContain` [realItem]
            map peLabel entries `shouldBe` sort (map peLabel entries)
            entries `shouldSatisfy` all (\e → T.isSuffixOf ".png" (peLabel e))
            entries `shouldSatisfy`
                all (\e → pePath e ≡ T.pack realCategoryRoot <> "/" <> peLabel e)

        it "excludes non-texture files and includes nested entries \
           \(case-insensitive extension), sorted" $
            withFixture $ \root → do
                entries ← discoverEntries root
                map peLabel entries `shouldBe`
                    ["a.png", "sub/b.png", "sub/c.PNG"]

    describe "resolveFocusedEntry" $ do
        it "resolves a real nested item to the same path discoverEntries reports" $ do
            result ← resolveFocusedEntry realCategoryRoot realItem
            case result of
                Left err → expectationFailure (T.unpack (focusErrorMessage err))
                Right entry → do
                    peLabel entry `shouldBe` T.pack realItem
                    pePath entry `shouldBe` T.pack (realCategoryRoot </> realItem)

        it "a displayed discoverEntries label resolves back to the identical entry" $
            withFixture $ \root → do
                discovered ← discoverEntries root
                forM_ discovered $ \d → do
                    result ← resolveFocusedEntry root (T.unpack (peLabel d))
                    result `shouldBe` Right d

        it "rejects a nonexistent item" $ do
            result ← resolveFocusedEntry realCategoryRoot "skill/does_not_exist.png"
            result `shouldBe` Left FocusNotFound

        it "rejects a directory given as the item" $ do
            result ← resolveFocusedEntry realCategoryRoot "skill"
            result `shouldBe` Left FocusNotAFile

        it "rejects an absolute item path before touching the filesystem" $ do
            result ← resolveFocusedEntry realCategoryRoot "/etc/passwd"
            result `shouldBe` Left FocusEscapesRoot

        it "rejects a .. traversal component" $ do
            result ← resolveFocusedEntry realCategoryRoot "../../../etc/passwd"
            result `shouldBe` Left FocusEscapesRoot

        it "rejects a .. traversal component in the middle of the path" $ do
            result ← resolveFocusedEntry realCategoryRoot "skill/../../ui/box"
            result `shouldBe` Left FocusEscapesRoot

        it "rejects an unsupported extension on an existing file" $
            withFixture $ \root → do
                result ← resolveFocusedEntry root "notes.txt"
                result `shouldBe` Left FocusUnsupportedExtension

        it "reports FocusNotFound when the category root itself doesn't exist" $ do
            result ← resolveFocusedEntry "assets/textures/does-not-exist-886" "x.png"
            result `shouldBe` Left FocusNotFound

    -- Sanity: 'realCategoryRoot' really exists in this checkout, or every
    -- test above proves nothing (an accidental repo reorganization would
    -- silently vacuous-pass otherwise).
    describe "fixture sanity" $
        it "the real icons category root exists" $ do
            exists ← doesDirectoryExist realCategoryRoot
            exists `shouldBe` True
