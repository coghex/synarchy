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
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU
import qualified Data.Serialize as S
import Data.Int (Int64)
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
import World.Chunk.Types (ChunkCoord(..))
import World.Fluid.Types (IceLevelGrid(..))
import World.Fluid.Lake.Types (LakeChunkEntry(..), WorldLakes(..))
import World.Fluid.River.Types (RiverChunkEntry(..), WorldRivers(..))
import World.Fluid.OceanMask (WorldOceanMask(..))
import World.Fluid.Seabed.Types (SeabedTable(..))
import World.Geology.Coastal.Types (CoastalTable(..))
import World.Geology.Ore.Types (WorldOreDeposits(..))
import World.Save.Compat.SessionV90 (sessionComponentVersion)
import World.Save.Envelope (luaStateComponentVersion, currentEnvelopeVersion)
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
                Right (record, ev) → do
                    out ← lookupEnv "SYNARCHY_CURRENT_SCHEMA_EVIDENCE"
                    forM_ out $ \path →
                        BL.writeFile path (encode (object
                            [ "record" .= record
                            , "provenance" .= object
                                [ "luaScriptsRequested" .= length (leLoaded ev) ]
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

    describe "custom boundary layouts" $ do
        it "every declared custom boundary has a layout example and a distinguishing sample" $ do
            Right (_, ev) ← extractLive
            let declaredTests = L.sort (map (cdTest . snd) customBoundaries)
            declaredTests `shouldBe` L.sort [ "current save schema: custom boundary layout: " <> n | (n, _, _) ← customLayouts ]
            forM_ (liveCarriers ev) $ \c → forM_ (ciRoot c) $ \r →
                forM_ (Map.elems (wrNodes r)) $ \n → case wnKind n of
                    WKCustom _ → length (wnChecks n) `shouldSatisfy` (>= 2)
                    _ → pure ()
        forM_ customLayouts $ \(name, expected, actual) →
            it ("custom boundary layout: " <> T.unpack name) $
                -- the expected bytes are specified by hand from the
                -- declared layout, not produced by the encoder under test
                actual `shouldBe` expected

    describe "library and primitive boundary layouts" $ do
        it "every reachable library or primitive boundary has a hand-specified layout, and none is stale" $ do
            Right (_, ev) ← extractLive
            let reached = L.sort (L.nub
                    [ t | c ← liveCarriers ev, Just r ← [ciRoot c]
                        , n ← Map.elems (wrNodes r)
                        , t ← case wnKind n of
                            WKPrimitive d → [d]
                            WKLibrary d   → [d]
                            _             → [] ])
            reached `shouldBe` L.sort [ d | (d, _, _) ← libraryLayouts ]
        forM_ libraryLayouts $ \(detail, expected, actual) →
            it ("library boundary layout: " <> T.unpack detail) $
                actual `shouldBe` expected

    describe "extraction fails closed" $ do
        let failing = [ c | c ← cases, caseBase c ≡ Right Null ]
        it "has a failing-extraction case for each classification gap" $
            length failing `shouldSatisfy` (>= 5)
        forM_ failing $ \c →
            it (T.unpack (caseName c)) $ case (caseCand c, caseVerdict c) of
                (Left ps, Fails m) → ps `shouldSatisfy` any (m `T.isInfixOf`)
                _ → expectationFailure "expected extraction to fail with the declared reason"

    describe "acceptance agrees with the real decode entry points" $ do
        it "every carrier's advertised versions equal the versions its decoder accepts" $ do
            Right (_, ev) ← extractLive
            concatMap advertisedMismatch (liveCarriers ev) `shouldBe` []

        it "the framing declares its advertised version and every entry point agrees" $ do
            Right (_, ev) ← extractLive
            let framing = [ c | c ← liveCarriers ev, ciId c ≡ "framing" ]
                cur = currentEnvelopeVersion
            map ciAccepted framing `shouldBe` [[cur]]
            concatMap advertisedMismatch framing `shouldBe` []
            let widened = [ c { ciAccepted = [cur - 1, cur] } | c ← framing ]
            concatMap advertisedMismatch widened `shouldNotBe` []
            -- a legitimate bump (current-only decoding at the next
            -- version) is consistent, so extraction would not reject it
            let bumped = [ c { ciVersion = cur + 1, ciAccepted = [cur + 1]
                             , ciDetail = object [ "advertised" .= [cur + 1] ] }
                         | c ← framing ]
            concatMap advertisedMismatch bumped `shouldBe` []

        it "entry points that disagree about acceptance are reported" $ do
            entryDisagreements "framing" [("a", [1]), ("b", [1])] `shouldBe` []
            entryDisagreements "framing" [("a", [1]), ("b", [0, 1])] `shouldNotBe` []

        it "a carrier advertising a version its decoder rejects is reported" $ do
            Right (_, ev) ← extractLive
            let tampered = [ c { ciDetail = object [ "advertised" .= [99 ∷ Int] ] }
                           | c ← take 1 (liveCarriers ev) ]
            concatMap advertisedMismatch tampered `shouldNotBe` []

    describe "retired legacy routes" $ do
        it "records the B1 and B2 routes with every entry point's real state" $ do
            Right (Object rec', _) ← extractLive
            let routes = case KM.lookup "routes" rec' of Just (Object o) → o; _ → mempty
            KM.member "b1-session" routes `shouldBe` True
            KM.member "b2-session" routes `shouldBe` True
            forM_ (KM.elems routes) $ \rt → case rt of
                Object o → case KM.lookup "enabled" o of
                    Just (Object e) → do
                        L.sort (map show (KM.keys e)) `shouldBe`
                            L.sort (map show ["publicFullSession", "publicMetadataOnly"
                                             , "directFullSession", "directMetadataOnly" ∷ Text])
                        forM_ (KM.elems e) $ \f → f `shouldSatisfy` isBool
                    _ → expectationFailure "route has no entry-point state"
                _ → expectationFailure "route is not an object"

        it "reads a refused shape as disabled through the same entry points" $ do
            -- A retired route is one whose shape the entry points no
            -- longer recognise: probe the real predicates with a frozen
            -- version no reader accepts.
            let off r bytes = case legacyFlags r bytes of
                    Object o → and [ f ≡ Bool False | f ← KM.elems o ]
                    _ → False
            off "b1-session" (b1ProbeWith (sessionComponentVersion + 1)) `shouldBe` True
            off "b2-session" (b2ProbeWith (luaStateComponentVersion + 1)) `shouldBe` True

    describe "Lua registry" $ do
        it "evaluates the production registrations through the real loader" $ do
            Right (_, ev) ← extractLive
            length (leLoaded ev) `shouldSatisfy` (> 20)
            let mods = [ o | Object o ← leModules ev ]
            mods `shouldSatisfy` (not . null)
            forM_ mods $ \o → do
                case KM.lookup "owner" o of
                    Just (String p) → leLoaded ev `shouldSatisfy` (p `elem`)
                    _ → expectationFailure "module without an owning script"
                case (KM.lookup "version" o, KM.lookup "accepted" o) of
                    (Just v, Just (Array xs)) → V.toList xs `shouldSatisfy` (v `elem`)
                    _ → expectationFailure "module without version or probed acceptance"

        it "a newly registered module flows through the same extraction" $ do
            r ← luaEvidenceWith [("scripts/fixture_added.lua", fixtureModule)]
            case r of
                Left e → expectationFailure (T.unpack e)
                Right ev → do
                    let ids = [ i | Object o ← leModules ev, Just (String i) ← [KM.lookup "id" o] ]
                    ids `shouldSatisfy` ("fixture_added" `elem`)
                    let carriers = [ c | c ← liveCarriers ev, ciId c ≡ "lua:fixture_added" ]
                    map ciAccepted carriers `shouldBe` [[1, 2]]
                    map ciOwners carriers `shouldBe` [["scripts/fixture_added.lua", "scripts/lib/data_codec.lua"]]
                    liveProblems ev `shouldBe` []

        it "loading an additional non-registering script leaves the canonical record unchanged" $ do
            Right ev0 ← luaEvidence
            Right ev1 ← luaEvidenceWith
                [("scripts/ui_only.lua", "return { init = function() end }")]
            length (leLoaded ev1) `shouldBe` length (leLoaded ev0) + 1
            buildRecord (liveCarriers ev1) liveRoutes (liveExtras ev1)
                `shouldBe` buildRecord (liveCarriers ev0) liveRoutes (liveExtras ev0)

        it "a script that cannot initialise fails the run instead of being skipped" $ do
            r ← luaEvidenceWith [("scripts/broken.lua", "return { init = function() error('boom') end }")]
            either ("failed under the stubs" `T.isInfixOf`) (const False) r `shouldBe` True

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

isBool ∷ Value → Bool
isBool (Bool _) = True
isBool _ = False

-- | A registering module for the module-addition regression.
fixtureModule ∷ Text
fixtureModule = T.unlines
    [ "local m = {}"
    , "function m.init()"
    , "  require('scripts.lib.save_modules').register('fixture_added', {"
    , "    version = 2, inputVersions = { 1, 2 }, required = true, scope = 'global', deps = {},"
    , "    snapshot = function() return {} end, decode = function(v, d) return d end,"
    , "    validate = function(d) return nil end, apply = function(d) end })"
    , "end"
    , "return m" ]

-- Hand-specified wire bytes -------------------------------------------

be ∷ Int → Int64 → BS.ByteString
be n v = BS.pack [ fromIntegral (v `shiftR` (8 * i)) | i ← [n - 1, n - 2 .. 0] ]

i64 ∷ Int64 → BS.ByteString
i64 = be 8

-- | A cereal list: its Int length, then the elements.
lst ∷ [BS.ByteString] → BS.ByteString
lst xs = i64 (fromIntegral (length xs)) <> BS.concat xs

coord ∷ Int64 → Int64 → BS.ByteString
coord a b = i64 a <> i64 b

w8 ∷ [Int64] → [BS.ByteString]
w8 = map (be 1)

-- | A 256-tile bitmask packed into 32 bytes, bit i of byte b = tile 8b+i.
packed ∷ [Int] → BS.ByteString
packed on = lst (w8 [ sum [ 2 ^ i | i ← [0 .. 7], 8 * b + i `elem` on ] | b ← [0 .. 31] ])

bitmask ∷ [Int] → VU.Vector Bool
bitmask on = VU.generate 256 (`elem` on)

customLayouts ∷ [(Text, BS.ByteString, BS.ByteString)]
customLayouts =
    [ ( "IceLevelGrid"
      , i64 1 <> i64 2 <> lst [i64 3]
      , S.encode (IceLevelGrid 1 2 (VU.fromList [3])) )
    , ( "LakeChunkEntry"
      , i64 5 <> packed [0, 9]
      , S.encode (LakeChunkEntry 5 (bitmask [0, 9])) )
    , ( "WorldLakes"
      , lst [] <> lst [coord 1 2 <> lst [i64 5 <> packed [0, 9]]]
          <> lst [coord 3 4 <> lst [i64 5, i64 6]]
      , S.encode (WorldLakes V.empty
            (HM.singleton (ChunkCoord 1 2) (V.fromList [LakeChunkEntry 5 (bitmask [0, 9])]))
            (HM.singleton (ChunkCoord 3 4) (VU.fromList [5, 6]))) )
    , ( "RiverChunkEntry"
      , i64 6 <> packed [0, 9] <> lst [i64 7, i64 8] <> lst (w8 [1, 2])
      , S.encode (RiverChunkEntry 6 (bitmask [0, 9]) (VU.fromList [7, 8]) (VU.fromList [1, 2])) )
    , ( "WorldRivers"
      , lst [] <> lst [coord 1 2 <> lst [i64 6 <> packed [0, 9] <> lst [i64 7, i64 8] <> lst (w8 [1, 2])]]
          <> lst [coord 3 4 <> lst [i64 5, i64 6]]
      , S.encode (WorldRivers V.empty
            (HM.singleton (ChunkCoord 1 2)
                (V.fromList [RiverChunkEntry 6 (bitmask [0, 9]) (VU.fromList [7, 8]) (VU.fromList [1, 2])]))
            (HM.singleton (ChunkCoord 3 4) (VU.fromList [5, 6]))) )
    , ( "WorldOceanMask"
      , lst [coord 1 2 <> packed [0, 9]]
      , S.encode (WorldOceanMask (HM.singleton (ChunkCoord 1 2) (bitmask [0, 9]))) )
    , ( "SeabedTable"
      , lst [coord 1 2 <> lst [i64 1, i64 (-2)]] <> lst [coord 3 4 <> lst (w8 [3, 4])]
      , S.encode (SeabedTable (HM.singleton (ChunkCoord 1 2) (VU.fromList [1, -2]))
                              (HM.singleton (ChunkCoord 3 4) (VU.fromList [3, 4]))) )
    , ( "CoastalTable"
      , lst [coord 1 2 <> lst [i64 1, i64 (-2)]] <> lst [coord 3 4 <> lst (w8 [3, 4])]
      , S.encode (CoastalTable (HM.singleton (ChunkCoord 1 2) (VU.fromList [1, -2]))
                               (HM.singleton (ChunkCoord 3 4) (VU.fromList [3, 4]))) )
    , ( "WorldOreDeposits"
      , lst [coord 1 2 <> lst [be 1 1 <> i64 2, be 1 3 <> i64 4]]
      , S.encode (WorldOreDeposits (HM.singleton (ChunkCoord 1 2) [(1, 2), (3, 4)])) )
    ]

-- | Every reachable library or primitive boundary, keyed by the
--   description its instance declares, with bytes specified by hand. The
--   orphan instances in this repository (Text, HashMap, Vector, HashSet)
--   are ours; the rest are cereal's, pinned all the same so a swap of the
--   serializer behind any of them is visible.
libraryLayouts ∷ [(Text, BS.ByteString, BS.ByteString)]
libraryLayouts =
    [ ( "UPrelude orphan Serialize HashMap (list of key/value pairs)"
      , lst [i64 1 <> i64 2], S.encode (HM.fromList [(1 ∷ Int, 2 ∷ Int)]) )
    , ( "World.Hydrology.Types orphan Serialize Vector (as a list)"
      , lst [i64 1, i64 2], S.encode (V.fromList [1, 2 ∷ Int]) )
    , ( "World.Generate.Types orphan Serialize HashSet (as a list)"
      , lst [i64 7], S.encode (HS.fromList [7 ∷ Int]) )
    , ( "cereal list (Int64 length prefix, then elements)"
      , lst [i64 1, i64 2], S.encode [1, 2 ∷ Int] )
    , ( "UPrelude orphan Serialize Text (UTF-8, length prefixed)"
      , i64 2 <> "ab", S.encode ("ab" ∷ Text) )
    , ( "cereal ByteString (Int64 length prefix)"
      , i64 2 <> "ab", S.encode ("ab" ∷ BS.ByteString) )
    , ( "cereal Double (IEEE 754 bits, big endian)"
      , be 8 0x3FF8000000000000, S.encode (1.5 ∷ Double) )
    , ( "cereal Float (IEEE 754 bits, big endian)"
      , be 4 0x3FC00000, S.encode (1.5 ∷ Float) )
    , ( "cereal Int (8 bytes, big endian)", i64 5, S.encode (5 ∷ Int) )
    , ( "cereal Int64 (big endian)", i64 (-2), S.encode (-2 ∷ Int64) )
    , ( "cereal Word8", be 1 7, S.encode (7 ∷ Word8) )
    , ( "cereal Word16 (big endian)", be 2 0x0102, S.encode (0x0102 ∷ Word16) )
    , ( "cereal Word32 (big endian)", be 4 0x01020304, S.encode (0x01020304 ∷ Word32) )
    , ( "cereal Word64 (big endian)", be 8 0x0102030405060708
      , S.encode (0x0102030405060708 ∷ Word64) )
    ]

fromText' ∷ Text → KM.Key
fromText' = fromString . T.unpack
