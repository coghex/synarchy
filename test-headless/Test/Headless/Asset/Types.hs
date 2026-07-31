-- | Pure regression tests for the hand-written 'Show' instance on
--   'Engine.Asset.Types.Font' (#951). The record carries a
--   @Maybe (IO ())@ cleanup action, so the instance is written by hand
--   and redacts that field to @\<present\>@\/@\<absent\>@ rather than
--   showing it. The closing @" }"@ used to live inside the @else@
--   branch alone, so a font with @fCleanup = Just _@ rendered without
--   its terminating brace.
--
--   Nothing in this build constructs 'Font' (the live font pipeline
--   stores 'FontAtlas' values in @FontCache.fcFonts@), so this is the
--   instance's only coverage. It asserts on the rendered string alone
--   and never runs the cleanup action it holds.
module Test.Headless.Asset.Types (spec) where

import UPrelude
import Test.Hspec
import Data.List (isInfixOf, isSuffixOf)
import qualified Data.Map.Strict as Map
import Engine.Asset.Base (AssetId(..), AssetStatus(..))
import Engine.Asset.Types (Font(..), GlyphInfo(..))

-- | A fully-populated 'Font' apart from the cleanup action under test.
fontWith ∷ Maybe (IO ()) → Font
fontWith cleanup = Font
    { fId       = AssetId 7
    , fName     = "test_font"
    , fPath     = "assets/fonts/test.ttf"
    , fSize     = 16
    , fStatus   = AssetLoaded
    , fAtlasId  = Just (AssetId 3)
    , fGlyphMap = Map.fromList [ ('a', glyph), ('b', glyph) ]
    , fRefCount = 2
    , fCleanup  = cleanup
    }

glyph ∷ GlyphInfo
glyph = GlyphInfo
    { giUVRect  = (0, 0, 1, 1)
    , giSize    = (8, 12)
    , giBearing = (0, 10)
    , giAdvance = 9
    }

-- | Every field ahead of the redacted cleanup value, in the order the
--   instance emits them — shared by both cleanup states.
sharedPrefix ∷ String
sharedPrefix =
    "Font { fId = AssetId 7\
    \, fName = \"test_font\"\
    \, fPath = \"assets/fonts/test.ttf\"\
    \, fSize = 16\
    \, fStatus = AssetLoaded\
    \, fAtlasId = Just (AssetId 3)\
    \, fGlyphMap = <2 glyphs>\
    \, fRefCount = 2\
    \, fCleanup = "

spec ∷ Spec
spec = describe "Show Font" $ do
    it "closes the record when fCleanup is Nothing" $
        show (fontWith Nothing) `shouldSatisfy` isSuffixOf "<absent> }"

    -- The regression: this branch used to stop at "<present>", with no
    -- closing brace at all.
    it "closes the record when fCleanup is Just an action" $
        show (fontWith (Just (pure ()))) `shouldSatisfy` isSuffixOf "<present> }"

    it "keeps the field order and cleanup redaction in both states" $ do
        show (fontWith Nothing) `shouldBe` sharedPrefix <> "<absent> }"
        show (fontWith (Just (pure ()))) `shouldBe` sharedPrefix <> "<present> }"

    it "redacts rather than renders the cleanup action" $ do
        let rendered = show (fontWith (Just (pure ())))
        rendered `shouldSatisfy` isInfixOf "fCleanup = <present>"
        -- no `Just`/`Nothing` leaking out of the cleanup field itself
        drop (length sharedPrefix) rendered `shouldBe` "<present> }"
