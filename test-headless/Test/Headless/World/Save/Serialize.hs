{-# LANGUAGE UnicodeSyntax, OverloadedStrings #-}
-- | Tests for the orphan @Serialize Text@ instance in UPrelude — the
--   #437 hardening that replaced an impure 'T.decodeUtf8' with
--   'T.decodeUtf8'' so a corrupted/truncated save with invalid UTF-8
--   inside a text field reports a clean parse error instead of
--   throwing a 'UnicodeException' out of 'S.runGet'.
module Test.Headless.World.Save.Serialize (spec) where

import UPrelude
import Test.Hspec
import qualified Data.ByteString as BS
import qualified Data.Serialize as S
import Data.Either (isLeft)

spec ∷ Spec
spec = describe "Serialize Text" $ do
    it "round-trips valid UTF-8 text" $
        (S.decode (S.encode ("hello, world 你好" ∷ Text)) ∷ Either String Text)
            `shouldBe` Right "hello, world 你好"

    it "reports a clean parse error for invalid UTF-8, not a thrown exception" $
        -- A lone continuation byte (0x80) is never valid UTF-8 on its
        -- own. Encode it the same way 'Serialize Text' encodes any
        -- ByteString payload, then decode as Text: this must return
        -- 'Left', mirroring exactly how a corrupted save's Text field
        -- would fail inside 'World.Save.Serialize.loadWorld'.
        let corrupt = S.encode (BS.pack [0x80])
        in (S.runGet (S.get ∷ S.Get Text) corrupt) `shouldSatisfy` isLeft
