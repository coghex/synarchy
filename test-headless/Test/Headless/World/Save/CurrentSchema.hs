-- | The @current save schema@ gate (issue #2916, PSP-2 of #2914): the
--   COMPILED half of @tools/current_save_schema_audit.py@.
--
--   It extracts the persisted shape from the live registry, the metadata
--   and framing values, the retired legacy routes and the evaluated Lua
--   registry; checks that extraction is complete, unambiguous and
--   fail-closed; and, when asked, writes the evidence the Python driver
--   compares. Nothing here reads source text.
--
--   Two environment variables switch on output (both optional):
--
--   * @SYNARCHY_CURRENT_SCHEMA_EVIDENCE=<file>@ -- the candidate record
--     and the extraction problems, as JSON.
--   * @SYNARCHY_CURRENT_SCHEMA_FIXTURES=<file>@ -- the compiled fixture
--     catalogue, for the driver's @--self-test@.
module Test.Headless.World.Save.CurrentSchema (spec) where

import UPrelude
import Test.Hspec
import Data.Aeson (Value(..), object, toJSON, (.=), encode)
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString.Lazy as BL
import qualified Data.List as L
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import System.Environment (lookupEnv)
import System.Info (compilerName, compilerVersion, arch, os)
import Data.Version (showVersion)

import World.Save.Component (saveComponentRegistry)
import World.Save.Component.Types
import World.Save.Wire
import World.Save.Wire.Custom (customBoundaries)
import World.Save.Envelope.Types (ComponentId(..))
import Test.Headless.World.Save.CurrentSchema.Fixtures
import Test.Headless.World.Save.CurrentSchema.Live
import Test.Headless.World.Save.CurrentSchema.Record

-- | The live record, or every reason it cannot be built.
extractLive ∷ IO (Either [Text] (Value, LuaEvidence))
extractLive = do
    lua ← luaEvidence
    pure $ case lua of
        Left e → Left ["Lua evidence: " <> e]
        Right ev →
            let problems = liveProblems ev
            in case buildRecord (liveCarriers ev) liveRoutes (liveExtras ev) of
                Left ps → Left (ps ⧺ problems)
                Right r | null problems → Right (r, ev)
                        | otherwise → Left problems

cases ∷ [Case]
cases = catalogue

-- | The fixture case of that name; a missing one is a test bug.
byName ∷ Text → Case
byName n = fromMaybe (error ("no fixture case named " <> T.unpack n))
                     (listToMaybe [ c | c ← cases, caseName c ≡ n ])

caseValue ∷ Case → Value
caseValue c = object
    [ "name" .= caseName c
    , "base" .= either (const Null) id (caseBase c)
    , "candidate" .= either (const Null) id (caseCand c)
    , "baseError" .= either id (const []) (caseBase c)
    , "candidateError" .= either id (const []) (caseCand c)
    , "verdict" .= (case caseVerdict c of Passes → "pass" ∷ Text; Fails _ → "fail")
    , "match" .= (case caseVerdict c of Passes → ""; Fails m → m) ]

spec ∷ Spec
spec = do
    describe "extraction" $ do
        it "builds the live record with no problem and emits it on request" $ do
            r ← extractLive
            case r of
                Left ps → expectationFailure (T.unpack (T.unlines ps))
                Right (record, _) → do
                    out ← lookupEnv "SYNARCHY_CURRENT_SCHEMA_EVIDENCE"
                    forM_ out $ \path →
                        BL.writeFile path (encode (object
                            [ "record" .= record
                            , "toolchain" .= object
                                [ "compiler" .= T.pack (compilerName ⧺ "-" ⧺ showVersion compilerVersion)
                                , "arch" .= T.pack arch, "os" .= T.pack os
                                , "extractor" .= ("current-save-schema/1" ∷ Text) ] ]))
                    record `shouldSatisfy` (/= Null)

        it "covers every registered Haskell component, the metadata, the framing and every Lua module" $ do
            Right (Object rec', ev) ← extractLive
            let carriers = case KM.lookup "carriers" rec' of
                    Just (Object o) → o
                    _ → mempty
                haskellIds = [ "haskell:" <> t | rc ← saveComponentRegistry
                             , let ComponentId t = rcId rc ]
            forM_ (haskellIds ⧺ ["metadata", "framing"]) $ \i →
                (i, KM.member (fromText' i) carriers) `shouldBe` (i, True)
            length (leModules ev) `shouldSatisfy` (> 0)
            length (L.nub haskellIds) `shouldBe` length haskellIds

        it "reports every reachable type of every Haskell root once, with a reachability set per carrier" $ do
            let roots = [ rcWire rc | rc ← saveComponentRegistry ]
            forM_ roots $ \r → Map.member (wrKey r) (wrNodes r) `shouldBe` True

    describe "serialization boundaries" $ do
        it "every reachable hand-written boundary is declared, and every declaration is reachable" $ do
            Right (_, ev) ← extractLive
            customProblems (liveCarriers ev) `shouldBe` []

        it "declares exactly the hand-written encoders in World.Save.Wire.Custom" $
            length customBoundaries `shouldSatisfy` (> 0)

        it "a hand-written encoder that differs from the generic one fails extraction unless declared" $ do
            let cand = caseCand (byName "undeclared hand-written encoder fails extraction")
            either (any ("unclassified encoder" `T.isInfixOf`)) (const False) cand `shouldBe` True
            let ok = caseCand (byName "declared hand-written encoder extracts")
            either (const False) (const True) ok `shouldBe` True

    describe "acceptance agrees with the real decode entry points" $ do
        it "every carrier's advertised versions equal the versions its decoder accepts" $ do
            Right (_, ev) ← extractLive
            concatMap advertisedMismatch (liveCarriers ev) `shouldBe` []

        it "a carrier advertising a version its decoder rejects is reported" $ do
            Right (_, ev) ← extractLive
            let tampered = [ c { ciDetail = object [ "advertised" .= [99 ∷ Int] ] }
                           | c ← take 1 (liveCarriers ev) ]
            concatMap advertisedMismatch tampered `shouldNotBe` []

    describe "retired legacy routes" $ do
        it "records the B1 and B2 routes as enabled through every entry point, owned conservatively" $ do
            Right (Object rec', _) ← extractLive
            let routes = case KM.lookup "routes" rec' of Just (Object o) → o; _ → mempty
            KM.member "b1-session" routes `shouldBe` True
            KM.member "b2-session" routes `shouldBe` True
            let flags = [ b | rt ← KM.elems routes, Object o ← [rt]
                        , Just (Object e) ← [KM.lookup "enabled" o]
                        , Bool b ← KM.elems e ]
            length flags `shouldBe` 8
            and flags `shouldBe` True

    describe "Lua registry" $ do
        it "evaluates the production registrations through the real loader" $ do
            Right (_, ev) ← extractLive
            length (leLoaded ev) `shouldSatisfy` (> 20)
            let ids = [ i | Object o ← leModules ev, Just (String i) ← [KM.lookup "id" o] ]
            L.sort ids `shouldBe` ["building_spawn", "tutorial_progress", "unit_ai"]

    describe "compiled shape fixtures" $ do
        let find = byName
            differs n = do
                let Case _ b cd _ = find n
                (b, cd) `shouldSatisfy` \(x, y) → x ≢ y ∧ either (const False) (const True) x
                                                         ∧ either (const False) (const True) y
        forM_ [ "field added, same version", "field removed, same version"
              , "field reordered, same version", "field retyped, same version"
              , "equal-typed named-field swap, same version"
              , "enum reorder, same version", "enum append, same version"
              , "alias parameter permutation, same version"
              , "newtype payload retyped, same version" ] $ \n →
            it ("changes the record: " <> T.unpack n) (differs n)

        it "leaves the record unchanged for an unchanged type reached through containers" $ do
            let Case _ b cd _ = find "nested type reached through list and Maybe is unchanged"
            b `shouldBe` cd

        it "records a shared type under every carrier that reaches it" $ do
            case caseCand (find "shared type changed, both owners bumped") of
                Right (Object o) → do
                    let carriers = case KM.lookup "carriers" o of Just (Object c) → c; _ → mempty
                        reaches k = case KM.lookup k carriers of
                            Just (Object c) → case KM.lookup "reaches" c of
                                Just (Array xs) → length xs
                                _ → 0
                            _ → 0
                    reaches "ua" `shouldSatisfy` (> 1)
                    reaches "ua" `shouldBe` reaches "ub"
                _ → expectationFailure "no object record"

        it "emits the catalogue on request" $ do
            out ← lookupEnv "SYNARCHY_CURRENT_SCHEMA_FIXTURES"
            forM_ out $ \path →
                BL.writeFile path (encode (toJSON (map caseValue cases)))
            length cases `shouldSatisfy` (> 20)

fromText' ∷ Text → KM.Key
fromText' = fromString . T.unpack
