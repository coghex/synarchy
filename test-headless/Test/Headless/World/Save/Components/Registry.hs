{-# LANGUAGE ScopedTypeVariables #-}
-- | The registry-and-codec owner of the "save components" gate
--   (issue #760, split out under #2043): registry structure and
--   dependency order, per-component codecs, location-instance stored
--   bounds, shared codec construction, and @csOlderVersions@ validity.
--   Pure -- no engine, no IO; every 'World.Save.Component.Session.SessionSnapshot'
--   here is a synthetic literal.
--
--   Composed by the facade 'Test.Headless.World.Save.Components', which
--   is the only module @test-headless/Spec.hs@ registers.
module Test.Headless.World.Save.Components.Registry
    (spec) where

import UPrelude
import Test.Hspec
import Control.Exception (ErrorCall(..), evaluate)
import qualified Data.ByteString as BS
import qualified Data.HashMap.Strict as HM
import qualified Data.Serialize as S
import qualified Data.Text as T
import Numeric (showHex)

import World.Save.Envelope.Types (ComponentId(..))
import World.Save.Component
import World.Save.Component.Types
import World.Save.Component.Session
import World.Save.Component.Page
import World.Save.Component.Entities
import World.Save.Component.Knowledge (containerKnowledgeCodec)
import World.Save.Component.Transfer (transferOrdersCodec)
import World.Save.Reference (SamePageRef(..))
import World.Save.Snapshot
import Location.Bounds (AbsBounds(..))
import Location.Instance
    ( LocationInstance(..), LocationInstances(..), LocationInstanceId(..)
    , LocationLifecycle(..), LocationSignificantItem(..) )
import World.Generate.Types (WorldGenParams(..), defaultWorldGenParams)
import World.River.Naming (RiverNames(..))
import World.Page.Types (WorldPageId(..), WorldIdentity(..))
import World.Render.Zoom.Types (ZoomMapMode(..))
import Item.Ground (GroundItem(..))
import World.Flora.Harvest (emptyFloraHarvests)
import World.Chunk.Types (ChunkCoord(..))
import World.Construct.Attempt (firstConstructAttemptId)
import Craft.Bills (BillId(..), BillMode(..))
import Power.Types (PowerNodeId(..), PowerRole(..))
import Building.Types (BuildingId(..))
import Test.Headless.Harness.GeneratedIds (fixtureGeneratedWorldIdForPage)
import Test.Headless.World.Save.Components.Fixture

spec ∷ Spec
spec = do
    describe "registry contract" $ do
        it "the authoritative registry is structurally well-formed \
           \(no duplicate ids, deps resolve, no cycles)" $
            registryStaticErrors `shouldBe` []

        it "topologically orders every component after its dependencies" $
            case dependencyOrder saveComponentRegistry of
                Left cyc → expectationFailure ("unexpected cycle: " <> show cyc)
                Right ordered → do
                    let ids = map rcId ordered
                        before a b = case (elemIndex' a ids, elemIndex' b ids) of
                            (Just i, Just j) → i < j
                            _                → False
                    -- world-pages precedes everything that depends on it
                    before worldPagesComponentId coreSessionComponentId
                        `shouldBe` True
                    before unitsComponentId unitSimComponentId `shouldBe` True
                    before buildingsComponentId craftBillsComponentId
                        `shouldBe` True

        it "rejects a dependency cycle in the registry" $ do
            let a = stubComponent (ComponentId "a") [ComponentId "b"]
                b = stubComponent (ComponentId "b") [ComponentId "a"]
            isLeft (dependencyOrder [a, b]) `shouldBe` True

        it "every gameplay component is required EXCEPT the two \
           \deliberately-optional ones -- requirement 7's rule, plus its \
           \documented exceptions: #1087's container-knowledge (absence \
           \means no container has ever been inspected) and #1246's \
           \transfer-orders (absence means no order is queued). Both \
           \post-date every tracked compatibility baseline and both \
           \absences are TRUE of such a session rather than invented. A \
           \THIRD optional component has to be justified here rather \
           \than slip in unnoticed" $
            [ rcId c | c ← saveComponentRegistry, not (rcRequired c) ]
                `shouldBe` [ containerKnowledgeComponentId
                           , transferOrdersComponentId ]

    describe "per-component codecs" $ do
        it "each component round-trips its own slice of the snapshot at \
           \its OWN CURRENT version" $ do
            -- The decode version is read from the codec rather than
            -- written out, so this cannot drift out of step with the
            -- encoder the way a literal does. It did: these were
            -- hard-coded, and once world-pages went to v7 (#1230) the
            -- v7 bytes were still being dispatched through the v6
            -- migration. It PASSED, because these fixtures carry an
            -- empty location table and the two shapes differ only in a
            -- per-instance field — so the round trip claimed here was
            -- silently never exercising the current decoder at all.
            -- #1233's buildings/units/world-activity bumps left the
            -- same three literals stale for the same reason.
            --
            -- Genuine frozen-shape coverage is not lost by this: every
            -- historical version has real frozen bytes behind it in
            -- "Test.Headless.World.Save.Compat", which encodes each
            -- vN DTO explicitly instead of hoping the current encoder
            -- still happens to emit that layout.
            let check c = case ccDecode c (ccVersion c) (ccEncode c richSnapshot) of
                    Right _  → pure () ∷ IO ()
                    Left e   → expectationFailure (T.unpack (renderComponentError e))
            check coreSessionCodec
            check worldPagesCodec
            check buildingsCodec
            check unitsCodec
            check unitSimCodec
            check craftBillsCodec
            check powerNodesCodec
            check worldEditsCodec
            check worldActivityCodec
            check texPaletteCodec

        it "declares a stable id and current version of 1" $ do
            ccId coreSessionCodec `shouldBe` coreSessionComponentId
            ccVersion coreSessionCodec `shouldBe` 1
            ccVersion worldPagesCodec `shouldBe` 10

        it "rejects a NEWER unsupported version, naming the phase" $
            case ccDecode worldPagesCodec 999 (ccEncode worldPagesCodec richSnapshot) of
                Left e  → do
                    cePhase e `shouldBe` DecodePhase
                    ceVersion e `shouldBe` 999
                Right _ → expectationFailure "expected version rejection"

        it "rejects an OLDER unsupported version" $
            ccDecode buildingsCodec 0 (ccEncode buildingsCodec richSnapshot)
                `shouldSatisfy` isLeftC

        it "rejects a truncated / malformed payload" $
            case ccDecode coreSessionCodec 1 (BS.pack [1,2,3]) of
                Left e  → cePhase e `shouldBe` DecodePhase
                Right _ → expectationFailure "expected malformed-payload rejection"

        it "world-pages self-validates a duplicate page id (component-local \
           \invariant)" $ do
            let dup = basePageSnapshots
                        (WorldPagesDTO [pageCore page1, pageCore page1])
            ccValidate worldPagesCodec dup `shouldSatisfy` (not . null)

        it "world-pages self-validates an empty page set" $
            ccValidate worldPagesCodec (basePageSnapshots (WorldPagesDTO []))
                `shouldSatisfy` (not . null)

        -- #760 round 8: per-page allocator validation for the three
        -- per-page (not global) id counters — craft bills, power nodes,
        -- ground items — mirroring world-pages' own component-local
        -- @ccValidate@ precedent above.
        it "craft-bills self-validates a bill id at/above the page's own \
           \allocator" $ do
            let badQueue = BillQueueDTO
                    { bqBills = HM.singleton (BillId 5) CraftBillDTO
                        { bilId = BillId 5, bilStation = SamePageRef (BuildingId 1)
                        , bilRecipe = "r", bilRemaining = -1, bilClaimant = Nothing
                        , bilClaimedAt = 0, bilProgress = 0, bilSeq = 5
                        , bilPaused = False, bilWorking = False
                        , bilMode = RepeatForever, bilTarget = 0
                        , bilOutputItem = "" }
                    , bqNextId = 5 }
                bad = CraftBillsDTO [ PageCraftBillsDTO page1 badQueue ]
            ccValidate craftBillsCodec bad `shouldSatisfy` (not . null)

        it "craft-bills accepts a queue whose every bill id sits below the \
           \allocator" $
            ccValidate craftBillsCodec
                (CraftBillsDTO [ PageCraftBillsDTO page1
                                    (toBillQueueDTO richBills) ])
                `shouldBe` []

        -- #760 round 9 (still-open item 1): the allocator check alone
        -- doesn't catch a map key that disagrees with the bill's OWN
        -- embedded id -- a hand-crafted envelope could carry
        -- @bqBills = {#1 -> bill{bilId=#2}}@ and slip past the allocator
        -- check (both #1 and #2 sit below it).
        it "craft-bills rejects a bill whose map key disagrees with its \
           \own embedded id" $ do
            let mismatched = BillQueueDTO
                    { bqBills = HM.singleton (BillId 1) CraftBillDTO
                        { bilId = BillId 2, bilStation = SamePageRef (BuildingId 1)
                        , bilRecipe = "r", bilRemaining = -1, bilClaimant = Nothing
                        , bilClaimedAt = 0, bilProgress = 0, bilSeq = 1
                        , bilPaused = False, bilWorking = False
                        , bilMode = RepeatForever, bilTarget = 0
                        , bilOutputItem = "" }
                    , bqNextId = 5 }
                bad = CraftBillsDTO [ PageCraftBillsDTO page1 mismatched ]
            ccValidate craftBillsCodec bad `shouldSatisfy` (not . null)

        it "power-nodes self-validates a node id at/above the page's own \
           \allocator" $ do
            let badReg = NodeRegistryDTO
                    { regNodes = HM.singleton (PowerNodeId 3) PowerNodeDTO
                        { nodId = PowerNodeId 3, nodBuilding = SamePageRef (BuildingId 1)
                        , nodRole = PowerSource, nodPeakWatts = 400
                        , nodCapacityWh = 0, nodStoredWh = 0 }
                    , regNextId = 3 }
                bad = PowerNodesDTO [ PagePowerNodesDTO page1 badReg ]
            ccValidate powerNodesCodec bad `shouldSatisfy` (not . null)

        it "power-nodes accepts a registry whose every node id sits below \
           \the allocator" $
            ccValidate powerNodesCodec
                (PowerNodesDTO [ PagePowerNodesDTO page1
                                    (toNodeRegistryDTO richNodes) ])
                `shouldBe` []

        -- #760 round 9 (still-open item 1): same key/value identity gap
        -- as craft-bills above, for power nodes.
        it "power-nodes rejects a node whose map key disagrees with its \
           \own embedded id" $ do
            let mismatched = NodeRegistryDTO
                    { regNodes = HM.singleton (PowerNodeId 1) PowerNodeDTO
                        { nodId = PowerNodeId 2, nodBuilding = SamePageRef (BuildingId 1)
                        , nodRole = PowerSource, nodPeakWatts = 400
                        , nodCapacityWh = 0, nodStoredWh = 0 }
                    , regNextId = 5 }
                bad = PowerNodesDTO [ PagePowerNodesDTO page1 mismatched ]
            ccValidate powerNodesCodec bad `shouldSatisfy` (not . null)

        it "world-activity self-validates a ground-item id at/above the \
           \page's own allocator" $ do
            let badGround = GroundItemsDTO
                    { gisiNextId = 1
                    , gisiItems = HM.singleton 1
                        (toGroundItemDTO (GroundItem richItem 0 0)) }
                bad = WorldActivityDTO
                    [ PageActivityDTO page1 HM.empty HM.empty HM.empty
                        HM.empty HM.empty emptyFloraHarvests HM.empty
                        badGround HM.empty HM.empty HM.empty
                        firstConstructAttemptId ]
            ccValidate worldActivityCodec bad `shouldSatisfy` (not . null)

        it "world-activity accepts ground items whose ids all sit below \
           \the allocator" $
            ccValidate worldActivityCodec
                (WorldActivityDTO
                    [ PageActivityDTO page1 HM.empty HM.empty HM.empty
                        HM.empty HM.empty emptyFloraHarvests HM.empty
                        (GroundItemsDTO 2
                            (HM.singleton 1 (toGroundItemDTO
                                (GroundItem richItem 0 0))))
                        HM.empty HM.empty HM.empty
                        firstConstructAttemptId ])
                `shouldBe` []

    -- #1668: the stored footprint of a persisted location instance is
    -- durable spatial authority (#911/#777), and the save decode path
    -- is the ONE 'AbsBounds' construction site that does not sit
    -- downstream of the YAML loader's inverted-bounds gate --
    -- 'fromAbsBoundsDTO' copies four unrestricted 'Int's off the wire.
    -- These cases drive the real decode+validate boundary
    -- ('decodeComponentValue' 's own @ccDecode@ then @ccValidate@
    -- sequence) at EVERY carrier shape, so no historical version
    -- routes around the check: the current 'LocationInstanceDTO' rides
    -- @world-pages@ v10, frozen 'LocationInstanceDTOv5' rides v8/v9,
    -- 'LocationInstanceDTOv4' rides v7, 'LocationInstanceDTOv3' rides v6,
    -- 'LocationInstanceDTOv2' rides v4/v5 and 'LocationInstanceDTOv1'
    -- rides v2/v3 (one version per identical carrier shape suffices).
    -- @world-pages@ v1 predates persisted instances and carries no
    -- 'AbsBoundsDTO' at all.
    describe "location-instance stored bounds (#1668)" $ do
        let gpWith b = defaultGP
                { wgpLocationInstances = LocationInstances
                    { lisNextId        = 2
                    , lisById          = HM.singleton (LocationInstanceId 1)
                        LocationInstance
                            { liId              = LocationInstanceId 1
                            , liDefId           = "ruin"
                            , liChunk           = ChunkCoord 0 0
                            , liAnchor          = (8, 8)
                            , liBounds          = b
                            , liDisplayName     = "Small Ruin"
                            , liGloss           = Nothing
                            , liEtymology       = Nothing
                            , liLifecycle       = LifecycleUnknown
                            , liContentsSpawned = False
                            , liEncounter       = Nothing
                            , liSignificant     = []
                            , liClearEventEmitted = False }
                    , lisPendingLegacy = Nothing } }
            -- One box per carrier, all inverted on x, so a failure names
            -- which version leaked rather than which coordinate did.
            invertedX = AbsBounds 10 6 6 10
            invertedY = AbsBounds 6 10 10 6
            invertedXY = AbsBounds 10 10 6 6
            degenerate = AbsBounds 6 6 6 6

            bytesAt ∷ Word32 → AbsBounds → BS.ByteString
            bytesAt 10 b = S.encode (WorldPagesDTO
                [ (pageCore page1) { pcGenParams = toWorldGenParamsDTO (gpWith b) } ])
            bytesAt 9 b = S.encode (WorldPagesDTOv9
                [ PageCoreDTOv9
                    { pc9PageId = page1
                    , pc9GenParams = toWorldGenParamsDTOv7 (gpWith b)
                    , pc9CameraX = 0, pc9CameraY = 0
                    , pc9TimeHour = 0, pc9TimeMinute = 0
                    , pc9DateYear = 1, pc9DateMonth = 1, pc9DateDay = 1
                    , pc9MapMode = ZMDefault, pc9Identity = Nothing
                    , pc9GeneratedId =
                        Just (fixtureGeneratedWorldIdForPage page1) } ])
            bytesAt 8 b = S.encode (WorldPagesDTOv8
                [ PageCoreDTOv8
                    { pc8PageId = page1
                    , pc8GenParams = toWorldGenParamsDTOv7 (gpWith b)
                    , pc8CameraX = 0, pc8CameraY = 0
                    , pc8TimeHour = 0, pc8TimeMinute = 0
                    , pc8DateYear = 1, pc8DateMonth = 1, pc8DateDay = 1
                    , pc8MapMode = ZMDefault, pc8Identity = Nothing } ])
            bytesAt 7 b = S.encode (WorldPagesDTOv7
                [ PageCoreDTOv7
                    { pc7PageId = page1
                    , pc7GenParams = toWorldGenParamsDTOv6 (gpWith b)
                    , pc7CameraX = 0, pc7CameraY = 0
                    , pc7TimeHour = 0, pc7TimeMinute = 0
                    , pc7DateYear = 1, pc7DateMonth = 1, pc7DateDay = 1
                    , pc7MapMode = ZMDefault, pc7Identity = Nothing } ])
            bytesAt 6 b = S.encode (WorldPagesDTOv6
                [ PageCoreDTOv6
                    { pc6PageId = page1
                    , pc6GenParams = toWorldGenParamsDTOv5 (gpWith b)
                    , pc6CameraX = 0, pc6CameraY = 0
                    , pc6TimeHour = 0, pc6TimeMinute = 0
                    , pc6DateYear = 1, pc6DateMonth = 1, pc6DateDay = 1
                    , pc6MapMode = ZMDefault, pc6Identity = Nothing } ])
            bytesAt 5 b = S.encode (WorldPagesDTOv5
                [ PageCoreDTOv5
                    { pc5PageId = page1
                    , pc5GenParams = toWorldGenParamsDTOv4 (gpWith b)
                    , pc5CameraX = 0, pc5CameraY = 0
                    , pc5TimeHour = 0, pc5TimeMinute = 0
                    , pc5DateYear = 1, pc5DateMonth = 1, pc5DateDay = 1
                    , pc5MapMode = ZMDefault, pc5Identity = Nothing } ])
            bytesAt 3 b = S.encode (WorldPagesDTOv3
                [ (pageCoreV3 page1)
                    { pc3GenParams = toWorldGenParamsDTOv2 (gpWith b) } ])
            bytesAt v _ = error ("bytesAt: unsupported version " <> show v)

            -- Exactly what 'decodeComponentValue' does: decode at the
            -- descriptor's version, then validate the canonical value.
            decodeThenValidate v b =
                case ccDecode worldPagesCodec v (bytesAt v b) of
                    Left e   → Left e
                    Right wp → Right (ccValidate worldPagesCodec wp)

            carriers ∷ [(String, Word32)]
            carriers = [ ("v10 / LocationInstanceDTO",  10)
                       , ("v9 / LocationInstanceDTOv5",  9)
                       , ("v8 / LocationInstanceDTOv5",  8)
                       , ("v7 / LocationInstanceDTOv4", 7)
                       , ("v6 / LocationInstanceDTOv3", 6)
                       , ("v5 / LocationInstanceDTOv2", 5)
                       , ("v3 / LocationInstanceDTOv1", 3) ]

            expectErrors label v b check =
                case decodeThenValidate v b of
                    Left e → expectationFailure
                        (label <> ": decode failed -- "
                         <> T.unpack (renderComponentError e))
                    Right [] → expectationFailure
                        (label <> ": an inverted stored box was ACCEPTED")
                    Right es → check es

        it "rejects an x-inverted stored box at EVERY carrier shape, in \
           \ValidatePhase, naming the component, the page, the instance \
           \and the axis" $
            forM_ carriers $ \(label, v) →
                expectErrors label v invertedX $ \es → do
                    map cePhase es `shouldBe` [ValidatePhase]
                    map ceComponent es `shouldBe` [worldPagesComponentId]
                    es `shouldSatisfy` mentions "page1"
                    es `shouldSatisfy` mentions "location instance #1"
                    es `shouldSatisfy` mentions "x axis"
                    es `shouldSatisfy` mentions "minX 10"
                    es `shouldSatisfy` mentions "maxX 6"

        it "rejects a y-inverted stored box at every carrier shape" $
            forM_ carriers $ \(label, v) →
                expectErrors label v invertedY $ \es → do
                    es `shouldSatisfy` mentions "y axis"
                    es `shouldNotSatisfy` mentions "x axis"

        it "names BOTH axes when a stored box is inverted on both -- a \
           \single unspecified inversion would not say what is wrong" $
            forM_ carriers $ \(label, v) →
                expectErrors label v invertedXY $ \es → do
                    length es `shouldBe` 2
                    es `shouldSatisfy` mentions "x axis"
                    es `shouldSatisfy` mentions "y axis"

        it "ACCEPTS a degenerate single-tile stored box at every carrier \
           \shape -- inclusive bounds make min ≡ max a real 1x1 \
           \footprint, not corruption" $
            forM_ carriers $ \(label, v) →
                case decodeThenValidate v degenerate of
                    Left e → expectationFailure
                        (label <> ": decode failed -- "
                         <> T.unpack (renderComponentError e))
                    Right es → (label, es) `shouldBe` (label, [])

        it "leaves an accepted stored box AUTHORITATIVE -- the decoded \
           \footprint is the one on the wire, never rederived (#911)" $
            forM_ carriers $ \(label, v) →
                case ccDecode worldPagesCodec v (bytesAt v degenerate) of
                    Left e → expectationFailure
                        (label <> ": decode failed -- "
                         <> T.unpack (renderComponentError e))
                    Right wp →
                        ( label
                        , map liBounds
                            (concatMap (HM.elems ∘ lisById
                                          ∘ wgpLocationInstances
                                          ∘ pgsGenParams)
                                       (HM.elems (wpBase wp))) )
                            `shouldBe` (label, [degenerate])

        it "converts snapshot ↔ DTO with no live-state reads: the world \
           \seed survives the round trip (a meaningful seed stays present, \
           \requirement 10)" $
            case ccDecode worldPagesCodec (ccVersion worldPagesCodec)
                          (ccEncode worldPagesCodec richSnapshot) of
                Right wp →
                    [ wgpSeed (pgsGenParams p)
                    | p ← maybeToList (HM.lookup page1 (wpBase wp)) ]
                        `shouldBe` [123456]
                Left e → expectationFailure (T.unpack (renderComponentError e))

    -- Issue #1093: every codec is now built through ONE shared
    -- construction that takes named arguments and can decode more than
    -- one encoded version, each through its own frozen DTO. These are the
    -- contracts that refactor had to keep exactly: the bytes it writes,
    -- the errors it reports, and the fact that its advertised
    -- accepted-version set IS what it dispatches on.
    describe "shared codec construction (issue #1093)" $ do
        it "encodes every registered gameplay component to byte-identical \
           \payloads (pinned length + fingerprint captured from the code \
           \BEFORE the construction changed)" $ do
            encodedPayloadDigests richSnapshot `shouldBe` goldenRichPayloads
            encodedPayloadDigests fullSnapshot `shouldBe` goldenFullPayloads

        -- The golden rows above are pinned on pages with an EMPTY
        -- location table, so they cannot witness #917's v10 layout at
        -- all — which is exactly why an unmoved world-pages row reads
        -- as a stale pin. This is the observation that settles it: the
        -- obligation fields DO reach the wire, and the golden fixtures
        -- simply have no location to carry them.
        it "a page owing significant contents encodes MORE than the same \
           \page owing none -- the v10 obligation fields reach the wire, \
           \which the golden rows above cannot show because their \
           \location tables are empty" $ do
            let withInstance entries = (minimalPage page1)
                    { pgsGenParams = canon (defaultWorldGenParams
                        { wgpLocationInstances = LocationInstances
                            { lisNextId        = 2
                            , lisById          = HM.singleton
                                (LocationInstanceId 1) (significantOwner entries)
                            , lisPendingLegacy = Nothing } }) }
                sizeOf page = case captureSessionSnapshot minimalGlobals [page] of
                    Left errs → error ("fixture invalid: " <> show errs)
                    Right s   → sum [ BS.length (rcEncode c s)
                                    | c ← saveComponentRegistry
                                    , rcId c ≡ worldPagesComponentId ]
                owing = sizeOf (withInstance
                            [LocationSignificantItem 1 "processing_unit"
                                (Just 7) True])
                empty' = sizeOf (withInstance [])
            owing `shouldSatisfy` (> empty')

        it "probes EVERY registered component -- a new codec cannot escape \
           \the dispatch invariants below by simply not being listed" $
            map cpId codecProbes `shouldBe` map rcId saveComponentRegistry

        it "advertises exactly the versions it dispatches on: ccInputVers is \
           \strictly ascending, ends at ccVersion, every listed version \
           \reaches a real decoder, and nothing outside it does" $
            forM_ codecProbes $ \p → do
                let vers = cpInputVers p
                    label extra = T.unpack (componentIdText (cpId p)) <> ": " <> extra
                vers `shouldSatisfy` \vs → and (zipWith (<) vs (drop 1 vs))
                unless (not (null vers) ∧ last vers ≡ cpVersion p) $
                    expectationFailure
                        (label "ccInputVers must end at ccVersion, got "
                         <> show vers <> " for v" <> show (cpVersion p))
                -- An ACCEPTED version reaches its own cereal decoder, so
                -- empty bytes fail as a malformed payload…
                forM_ vers $ \v → case cpDecodeErr p v BS.empty of
                    Just e → do
                        cePhase e `shouldBe` DecodePhase
                        ceVersion e `shouldBe` v
                        ceComponent e `shouldBe` cpId p
                        unless ("malformed payload: " `T.isPrefixOf` ceMessage e) $
                            expectationFailure
                                (label ("v" <> show v <> " is advertised as \
                                        \accepted but did not reach a decoder: ")
                                 <> T.unpack (ceMessage e))
                    Nothing → expectationFailure
                        (label ("v" <> show v <> " decoded EMPTY bytes"))
                -- …while anything outside the set is rejected as an
                -- unsupported version, naming every version that IS
                -- accepted.
                let expected = "unsupported schema version (reader supports "
                             <> T.intercalate ", " [ "v" <> T.pack (show v)
                                                   | v ← vers ] <> ")"
                forM_ [0, cpVersion p + 1] $ \v →
                    cpDecodeErr p v BS.empty
                        `shouldBe` Just (ComponentError (cpId p) v DecodePhase expected)

        it "reports an unsupported version identically for a SINGLETON \
           \reader (component, version, phase, and the full message)" $
            decodeErrorOf coreSessionCodec 2
                    (ccEncode coreSessionCodec richSnapshot)
                `shouldBe` Just (ComponentError coreSessionComponentId 2
                    DecodePhase
                    "unsupported schema version (reader supports v1)")

        it "reports an unsupported version identically for a TWO-version \
           \reader -- the existing 'reader supports v1, v2' rendering" $ do
            decodeErrorOf craftBillsCodec 3
                    (ccEncode craftBillsCodec richSnapshot)
                `shouldBe` Just (ComponentError craftBillsComponentId 3
                    DecodePhase
                    "unsupported schema version (reader supports v1, v2)")
            decodeErrorOf powerNodesCodec 7 BS.empty
                `shouldBe` Just (ComponentError powerNodesComponentId 7
                    DecodePhase
                    "unsupported schema version (reader supports v1, v2)")

        -- unit-sim gained a third version with #1217's per-request hazard
        -- policy; it is the reader that exercises the rendering between
        -- the two- and nine-version cases either side of it.
        it "reports an unsupported version identically for a THREE-version \
           \reader" $
            decodeErrorOf unitSimCodec 0 BS.empty
                `shouldBe` Just (ComponentError unitSimComponentId 0
                    DecodePhase
                    "unsupported schema version (reader supports v1, v2, v3)")

        it "reports an unsupported version identically for a TEN-version \
           \reader" $
            decodeErrorOf worldPagesCodec 11 BS.empty
                `shouldBe` Just (ComponentError worldPagesComponentId 11
                    DecodePhase
                    "unsupported schema version \
                    \(reader supports v1, v2, v3, v4, v5, v6, v7, v8, v9, \
                    \v10)")

        it "reports a malformed payload identically -- same component, \
           \supplied version, DecodePhase, and cereal-derived message -- at \
           \a singleton reader's only version and at BOTH a multi-version \
           \reader's current and historical versions" $ do
            let truncated = BS.pack [1, 2, 3]
                cerealMsg = "malformed payload: too few bytes\n\
                            \From:\tdemandInput\n\n"
            decodeErrorOf coreSessionCodec 1 truncated
                `shouldBe` Just (ComponentError coreSessionComponentId 1
                                   DecodePhase cerealMsg)
            decodeErrorOf craftBillsCodec 2 truncated
                `shouldBe` Just (ComponentError craftBillsComponentId 2
                                   DecodePhase cerealMsg)
            decodeErrorOf craftBillsCodec 1 truncated
                `shouldBe` Just (ComponentError craftBillsComponentId 1
                                   DecodePhase cerealMsg)

        -- The point a widened ccInputVers alone could never reach: ONE
        -- byte string means different things at different versions,
        -- because each version owns a different frozen DTO type. A
        -- pre-#1092 world-pages page core ends in the two-field
        -- 'WorldIdentityDTOv1', where v3's ends in the three-field
        -- 'WorldIdentityDTO' — so a v2 payload carrying an identity is
        -- genuinely shorter than any v3 payload, and reading it with the
        -- current DTO must fail rather than half-parse.
        it "reads each accepted version through its OWN frozen DTO -- the \
           \same v2 world-pages bytes decode at v2 and are REJECTED at v3" $ do
            let v2Bytes = S.encode (WorldPagesDTOv2 [pageCoreV2 page1])
            case ccDecode worldPagesCodec 2 v2Bytes of
                Right wp → map (fmap wiName . pgsIdentity)
                               (HM.elems (wpBase wp))
                    `shouldBe` [Just "Old World"]
                Left e → expectationFailure (T.unpack (renderComponentError e))
            decodeErrorOf worldPagesCodec 3 v2Bytes
                `shouldSatisfy` maybe False ((≡ DecodePhase) . cePhase)
            decodeErrorOf worldPagesCodec 4 v2Bytes
                `shouldSatisfy` maybe False ((≡ DecodePhase) . cePhase)
            decodeErrorOf worldPagesCodec 5 v2Bytes
                `shouldSatisfy` maybe False ((≡ DecodePhase) . cePhase)

        it "a v3 world-pages payload reaches the v3 decoder (#1101) rather \
           \than the current one" $ do
            let v3Bytes = S.encode (WorldPagesDTOv3 [pageCoreV3 page1])
            case ccDecode worldPagesCodec 3 v3Bytes of
                Right wp → map (fmap wiName . pgsIdentity)
                               (HM.elems (wpBase wp))
                    `shouldBe` [Just "Old World"]
                Left e → expectationFailure (T.unpack (renderComponentError e))

        it "a v4 world-pages payload reaches the v4 decoder (#1102) rather \
           \than the current one, and its page comes back with NO river \
           \names -- a save written before rivers were named never \
           \acquires them" $ do
            let v4Bytes = S.encode (WorldPagesDTOv4 [pageCoreV4 page1])
            case ccDecode worldPagesCodec 4 v4Bytes of
                Right wp → do
                    map (fmap wiName . pgsIdentity) (HM.elems (wpBase wp))
                        `shouldBe` [Just "Old World"]
                    map (rvnById . wgpRiverNames . pgsGenParams)
                        (HM.elems (wpBase wp))
                        `shouldBe` [HM.empty]
                Left e → expectationFailure (T.unpack (renderComponentError e))

        it "a v1 craft-bills payload reaches the v1 decoder and comes back \
           \MIGRATED (bare ids wrapped as same-page references), not \
           \reinterpreted as the current DTO" $ do
            let v1Bytes = S.encode (CraftBillsDTOv1
                    [ PageCraftBillsDTOv1 page1 (BillQueueDTOv1
                        { bq1NextId = 2
                        , bq1Bills  = HM.singleton (BillId 1) CraftBillDTOv1
                            { bil1Id         = BillId 1
                            , bil1Station    = BuildingId 1
                            , bil1Recipe     = "forge_steel_dagger"
                            , bil1Remaining  = 1
                            , bil1Claimant   = Nothing
                            , bil1ClaimedAt  = 0
                            , bil1Progress   = 0
                            , bil1Seq        = 1
                            , bil1Paused     = False
                            , bil1Working    = False
                            , bil1Mode       = FixedCount
                            , bil1Target     = 0
                            , bil1OutputItem = "steel_dagger"
                            } }) ])
            case ccDecode craftBillsCodec 1 v1Bytes of
                Right (CraftBillsDTO [slice]) →
                    map bilStation (HM.elems (bqBills (pcbBills slice)))
                        `shouldBe` [SamePageRef (BuildingId 1)]
                Right other → expectationFailure
                    ("expected exactly one migrated page slice, got "
                     <> show other)
                Left e → expectationFailure (T.unpack (renderComponentError e))

    -- Issue #1275: 'csOlderVersions' promised every entry was OLDER than
    -- the current version, and nothing enforced it. Because
    -- 'componentCodec' sorts the current version together with the
    -- declared older ones and dispatches by first-match 'lookup', a
    -- malformed table degrades SILENTLY rather than failing: a repeated
    -- version leaves its second decoder unreachable, the current version
    -- listed as older is shadowed by the real current decoder, and a
    -- future version is advertised and accepted as though it were
    -- history. The version is an ordinary 'Word32' argument, so the type
    -- checker sees nothing wrong with any of them.
    --
    -- 'componentCodec' is the AUTHORITATIVE boundary for that contract:
    -- it rejects the table BEFORE a 'ComponentCodec' exists, so a
    -- malformed declaration cannot reach a live dispatch table at all.
    -- The registered-codec invariants above ("advertises exactly the
    -- versions it dispatches on") and 'tools/save_compat_audit.py' both
    -- still observe the same rule over the real components — deliberately
    -- kept as defense-in-depth, since each catches it through a
    -- different mechanism (runtime probing / source parsing).
    describe "csOlderVersions table validity (issue #1275)" $ do
        it "rejects a version declared TWICE, naming the component and the \
           \repeated version -- the dispatch table's lookup would only ever \
           \reach the first of the two decoders" $
            evaluate (componentCodec (versionTableProbe 4 [3, 2, 3]))
                `shouldThrow` errorMentioning
                    ["version-table-probe", "v3", "more than once"]

        it "rejects the CURRENT version declared as older -- sortOn is \
           \stable and the current decoder is prepended, so the entry's own \
           \frozen DTO would never be reached" $
            evaluate (componentCodec (versionTableProbe 4 [4, 1]))
                `shouldThrow` errorMentioning
                    ["version-table-probe", "v4", "CURRENT version"]

        it "rejects a version NEWER than csVersion -- the reader would \
           \advertise and accept a version no writer has ever produced" $
            evaluate (componentCodec (versionTableProbe 4 [5, 1]))
                `shouldThrow` errorMentioning
                    ["version-table-probe", "v5", "NEWER"]

        it "reports the FIRST offending entry in declaration order, so the \
           \diagnostic points at a real line rather than at whichever entry \
           \a sort happened to surface" $ do
            evaluate (componentCodec (versionTableProbe 4 [9, 2, 2]))
                `shouldThrow` errorMentioning ["v9"]
            evaluate (componentCodec (versionTableProbe 4 [2, 2, 9]))
                `shouldThrow` errorMentioning ["v2"]

        it "leaves every WELL-FORMED table alone: descending, ascending, \
           \single-entry, and empty declarations all build and advertise \
           \the same ascending accepted set they always did" $ do
            let versOf = ccInputVers . componentCodec
            versOf (versionTableProbe 4 [3, 2, 1]) `shouldBe` [1, 2, 3, 4]
            versOf (versionTableProbe 4 [1, 2, 3]) `shouldBe` [1, 2, 3, 4]
            versOf (versionTableProbe 4 [1])       `shouldBe` [1, 4]
            versOf (versionTableProbe 4 [])        `shouldBe` [4]

        it "the pure check agrees exactly with what construction does -- \
           \Nothing for a well-formed table, and a message naming the \
           \component and the offending version otherwise" $ do
            olderVersionTableError probeComponentId 4 [3, 2, 1]
                `shouldBe` Nothing
            olderVersionTableError probeComponentId 4 []
                `shouldBe` Nothing
            olderVersionTableError probeComponentId 4 [2, 2]
                `shouldSatisfy` maybe False
                    (\m → "version-table-probe" `T.isInfixOf` m
                          ∧ "v2" `T.isInfixOf` m)

        it "every REAL registered codec still constructs -- forcing the \
           \whole authoritative registry runs this check over every shipped \
           \declaration, so a malformed one would fail HERE rather than \
           \being reported after the fact" $
            map (T.null . componentIdText . rcId) saveComponentRegistry
                `shouldSatisfy` all not

-- Helpers -----------------------------------------------------------

-- | The same page core in the frozen pre-#1092 v2 shape, WITH an identity
--   — the field whose DTO actually differs between v2 and v3, so a v2
--   payload built here is genuinely not a v3 payload.
pageCoreV2 ∷ WorldPageId → PageCoreDTOv2
pageCoreV2 pid = PageCoreDTOv2
    { pc2PageId = pid, pc2GenParams = toWorldGenParamsDTOv2 defaultGP
    , pc2CameraX = 0, pc2CameraY = 0, pc2TimeHour = 0, pc2TimeMinute = 0
    , pc2DateYear = 1, pc2DateMonth = 1, pc2DateDay = 1, pc2MapMode = ZMDefault
    , pc2Identity = Just (WorldIdentityDTOv1 "Old World" Nothing) }

-- | The same page core in the frozen pre-#1101 v3 shape: #1092's
--   three-field identity over the frozen pre-#1101 gen params, whose
--   location-instance table carries no gloss.
pageCoreV4 ∷ WorldPageId → PageCoreDTOv4
pageCoreV4 pid = PageCoreDTOv4
    { pc4PageId = pid, pc4GenParams = toWorldGenParamsDTOv3 defaultGP
    , pc4CameraX = 0, pc4CameraY = 0, pc4TimeHour = 0, pc4TimeMinute = 0
    , pc4DateYear = 1, pc4DateMonth = 1, pc4DateDay = 1, pc4MapMode = ZMDefault
    , pc4Identity = Just (WorldIdentityDTOv2 "Old World" Nothing Nothing) }

pageCoreV3 ∷ WorldPageId → PageCoreDTOv3
pageCoreV3 pid = PageCoreDTOv3
    { pc3PageId = pid, pc3GenParams = toWorldGenParamsDTOv2 defaultGP
    , pc3CameraX = 0, pc3CameraY = 0, pc3TimeHour = 0, pc3TimeMinute = 0
    , pc3DateYear = 1, pc3DateMonth = 1, pc3DateDay = 1, pc3MapMode = ZMDefault
    , pc3Identity = Just (WorldIdentityDTOv2 "Old World" Nothing Nothing) }

componentIdText ∷ ComponentId → Text
componentIdText (ComponentId t) = t

-- | Issue #1275: a synthetic spec whose ONLY interesting content is its
--   declared version table. The DTO is a bare 'Word32' so nothing about
--   any real component's shape can influence what the construction-time
--   check does with the declarations.
versionTableProbe ∷ Word32 → [Word32] → ComponentSpec Word32 Word32
versionTableProbe current older = ComponentSpec
    { csComponent     = probeComponentId
    , csVersion       = current
    , csRequired      = True
    , csDeps          = []
    , csEncode        = const 0
    , csDecode        = id
    , csOlderVersions = [ atVersion v (id ∷ Word32 → Word32) | v ← older ]
    , csValidate      = const []
    }

probeComponentId ∷ ComponentId
probeComponentId = ComponentId "version-table-probe"

-- | An 'ErrorCall' whose message contains every given fragment — used to
--   prove a construction-time rejection actually NAMES the component and
--   the offending version, not merely that something crashed.
errorMentioning ∷ [Text] → Selector ErrorCall
errorMentioning needles (ErrorCall msg) =
    all (`T.isInfixOf` T.pack msg) needles

-- | A payload's byte length plus an FNV-1a-64 fingerprint of its bytes —
--   a compact stand-in for pinning whole encoded payloads inline
--   (issue #1093's byte-identical-encoding requirement). Deliberately its
--   OWN hash rather than the envelope's manifest checksum: this gate must
--   keep meaning exactly "these component bytes are unchanged" even if the
--   envelope's framing checksum is ever changed.
payloadDigest ∷ BS.ByteString → (Int, Text)
payloadDigest bytes = (BS.length bytes, hex16 (BS.foldl' step 0xcbf29ce484222325 bytes))
  where
    step ∷ Word64 → Word8 → Word64
    step h b = (h `xor` fromIntegral b) * 0x100000001b3
    hex16 w = let s = showHex w "" in T.pack (replicate (16 - length s) '0' <> s)

-- | The encoded payload of EVERY registered gameplay component, captured
--   from the code as it stood BEFORE issue #1093 changed how codecs are
--   constructed. That change is entirely about how a 'ComponentCodec' is
--   BUILT, never about what it writes, so every entry here had to survive
--   it untouched — the round-trip and manifest-fixture gates prove
--   decodability and canonical equivalence, but neither would notice a
--   re-encoding that merely round-trips.
--
--   Pinned against BOTH shared snapshots: 'richSnapshot' (two pages,
--   populated entities/edits/climate) and 'fullSnapshot' (the one that
--   also populates craft bills, power nodes, ground items, designations),
--   so no component's row is the degenerate encoding of an empty slice.
--
--   A deliberate schema bump (a new 'csVersion' plus its frozen
--   predecessor in 'csOlderVersions') legitimately moves the affected
--   component's rows — update them in the same commit as the bump, with
--   that component's own compatibility fixture. Any OTHER movement means
--   encoded bytes changed by accident.
goldenRichPayloads ∷ [(Text, (Int, Text))]
goldenRichPayloads =
    [ ("core-session",        (85,   "74d3010096cbbe2b"))
    , ("texture-palette",     (16,   "88201fb960ff6465"))
      -- #2021 re-pinned: @world-pages@ v9 appended each page's optional
      -- generated-world id (17 bytes per page here — a present tag plus
      -- 128 opaque bits). Only this component's rows moved; no other
      -- component carries the id.
      --
      -- #917 took the component to v10 and did NOT move this row, which
      -- is correct rather than an oversight: its two obligation fields
      -- hang off a LOCATION INSTANCE, and these fixture pages carry
      -- 'defaultWorldGenParams' — an EMPTY instance table — so there is
      -- no record for them to append to. The v10 layout is pinned where
      -- it can actually be observed, by
      -- "a page owing significant contents encodes MORE than the same
      -- page owing none" below.
    , ("world-pages",         (1340, "70b209601aaa96d0"))
      -- #1854 re-pinned: @world-edits@ v2 appends the page's
      -- planted-flora allocator cursor to every page slice (and a
      -- FloraInstanceId to every WePlaceFlora entry, of which this
      -- fixture has none), and @world-activity@ v4 appends the two
      -- deferred legacy-migration maps and re-keys Chop/harvest state
      -- onto FloraInstanceId. Every other row is unchanged.
    , ("world-edits",         (66,   "5f4fc96e8f002516"))
      -- #1844 re-pinned again: world-activity v5 appends each
      -- designation's attempt identity and payment record, and the
      -- page's own attempt allocator.
    , ("world-activity",      (242,  "d5f6a72687031136"))
    , ("buildings",           (151,  "3dafc93879ea3b82"))
    , ("units",               (249,  "fc6ed2ffd1c79265"))
    , ("unit-sim",            (123,  "81797b8874157310"))
    , ("craft-bills",         (58,   "beec8f6ff4c58c26"))
    , ("power-nodes",         (58,   "beec8f6ff4c58c26"))
    , ("container-knowledge", (50,   "1ed7627acac89064"))
    , ("transfer-orders",     (58,   "beec8f6ff4c58c26"))
    ]

goldenFullPayloads ∷ [(Text, (Int, Text))]
goldenFullPayloads =
    [ ("core-session",        (85,  "0641eeed95100f9a"))
    , ("texture-palette",     (16,  "88201fb960ff6465"))
      -- #2021 re-pinned, same reason as goldenRichPayloads (one page
      -- here, so 17 bytes rather than 34). #917 left it unmoved for the
      -- same reason too — this page's location table is empty.
    , ("world-pages",         (700, "e99c20c10976e8b9"))
      -- #1854 re-pinned, same two components as goldenRichPayloads.
    , ("world-edits",         (78,  "d70f14ce21048a09"))
      -- #1233 re-pinned: this fixture's page carries a ground item, and
      -- world-activity v3 appended the item tree's physical values (an
      -- absent Maybe pair per item, ×3 nesting levels). #1854 re-pinned
      -- it again for v4's two deferred-migration maps. Every other row
      -- is unchanged, because no other fixture slice holds an item.
      -- #1844 re-pinned again for world-activity v5, exactly as
      -- goldenRichPayloads is.
    , ("world-activity",      (378, "401b1ef21412a4ee"))
    , ("buildings",           (130, "2b6c80ab8c216329"))
    , ("units",               (228, "4b3dd9531385aafc"))
    , ("unit-sim",            (102, "2977ea9721e11313"))
    , ("craft-bills",         (125, "687f006dbc839e32"))
    , ("power-nodes",         (58,  "0cadd98f962a6b12"))
    , ("container-knowledge", (29,  "1a075ce50a1643b1"))
    , ("transfer-orders",     (87,  "952016d6f5458b43"))
    ]

encodedPayloadDigests ∷ SessionSnapshot → [(Text, (Int, Text))]
encodedPayloadDigests snap =
    [ (componentIdText (rcId c), payloadDigest (rcEncode c snap))
    | c ← saveComponentRegistry ]

-- | One type-erased view of a concrete codec, enough to probe its
--   version dispatch without knowing what it decodes into.
data CodecProbe = CodecProbe
    { cpId        ∷ ComponentId
    , cpVersion   ∷ Word32
    , cpInputVers ∷ [Word32]
    , cpDecodeErr ∷ Word32 → BS.ByteString → Maybe ComponentError
    }

probeOf ∷ ComponentCodec a → CodecProbe
probeOf cc = CodecProbe
    { cpId        = ccId cc
    , cpVersion   = ccVersion cc
    , cpInputVers = ccInputVers cc
    , cpDecodeErr = decodeErrorOf cc
    }

-- | Every registered gameplay codec, as probes. Kept in
--   'saveComponentRegistry' order and cross-checked against it below, so
--   a component added to the registry without a probe here fails rather
--   than silently escaping the dispatch invariants.
codecProbes ∷ [CodecProbe]
codecProbes =
    [ probeOf coreSessionCodec, probeOf texPaletteCodec
    , probeOf worldPagesCodec, probeOf worldEditsCodec
    , probeOf worldActivityCodec, probeOf buildingsCodec
    , probeOf unitsCodec, probeOf unitSimCodec
    , probeOf craftBillsCodec, probeOf powerNodesCodec
    , probeOf containerKnowledgeCodec
    , probeOf transferOrdersCodec
    ]

decodeErrorOf ∷ ComponentCodec a → Word32 → BS.ByteString → Maybe ComponentError
decodeErrorOf cc v bytes = either Just (const Nothing) (ccDecode cc v bytes)

isLeftC ∷ Either ComponentError a → Bool
isLeftC (Left _) = True
isLeftC _        = False

elemIndex' ∷ Eq a ⇒ a → [a] → Maybe Int
elemIndex' x = go 0
  where go _ [] = Nothing
        go i (y:ys) | x ≡ y = Just i
                    | otherwise = go (i+1) ys

-- | A dummy registered component for cycle testing (its codec bodies are
--   never exercised — dependencyOrder only reads id + deps).
stubComponent ∷ ComponentId → [ComponentId] → RegisteredComponent
stubComponent cid deps = RegisteredComponent
    { rcId = cid, rcVersion = 1, rcInputVers = [1], rcRequired = True
    , rcDeps = deps, rcEncode = const BS.empty
    , rcPrepare = const (Right (\s → Right s)) }
