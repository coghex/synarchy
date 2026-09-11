-- | The save-compatibility tooling's compiled codec helper (issue
--   #2273).
--
--   Four operations the Python save tooling needs from the REAL
--   Haskell save codec, and nothing else:
--
--     * @summary@         — derive a fixture's canonical-summary JSON
--                           from its decoded @SessionSnapshot@\/
--                           @SaveMetadata@.
--     * @set-timestamp@   — rewrite ONLY a fixture's @metadata@
--                           component's @smTimestamp@, re-encoding
--                           every other component's bytes verbatim.
--     * @descriptors@     — dump a batch of fixtures' RAW envelope
--                           manifests (id\/version\/required) as one
--                           JSON object keyed by fixture path.
--     * @compare@         — decode N independently-produced session
--                           saves and report whether every one is
--                           structurally identical, on both the decoded
--                           @SessionSnapshot@ and every @lua.\<module\>@
--                           component's raw payload bytes.
--
--   Until #2273 the first three were GHCi programs fed to
--   @cabal repl test:synarchy-test-headless@ from
--   @tools\/save_compat_audit_codec.py@, so every invocation loaded the
--   348-module test suite into the interpreter to reach a handful of
--   library functions — 2.6–3.7 minutes per CI run. They are compiled
--   here instead, against exactly the same library functions, and
--   @cabal build all@ produces the binary.
--
--   @compare@ (#2274) is the fourth and last of that family:
--   @tools\/persistence_snapshot.py@ kept a GHCi program of its own for
--   the @persistence_contract@ probe's structural comparison, which is
--   why that probe had to hold the shared Cabal build state EXCLUSIVELY
--   and why the @behavior-probes@ CI job built @synarchy-test-headless@
--   at all. It is the same program, against the same library function,
--   compiled.
--
--   Deliberately a SEPARATE executable rather than a mode of
--   @exe:synarchy@ (requirement 6 leaves the choice to the solver):
--   this program must not boot, must not resolve or @chdir@ into a
--   runtime resource root, and must not link the renderer, so keeping
--   it out of @app\/Main.hs@'s boot-mode precedence is what makes
--   \"decode these bytes\" a pure, millisecond operation. The Python
--   side resolves it by absolute path — see
--   @tools\/save_compat_audit_codec.py@'s @resolve_codec_exe@ — so a
--   caller holding a pre-resolved path (the probe runner's
--   one-resolved-binary contract, #1570) invokes it with no Cabal
--   contact at all.
--
--   Every operation reports success with the SAME stdout marker its
--   GHCi predecessor printed (@DUMP_OK@ \/ @NORMALIZE_OK@ \/
--   @DESCRIPTOR_DUMP_OK@ \/ @COMPARE_OK@) and, on failure, exits
--   non-zero after naming the offending fixture path and the codec's own
--   error text on stderr (requirement 5), so a broken fixture is
--   diagnosable straight from a CI log.
--
--   @compare@ is the one operation whose non-@OK@ outcomes are ANSWERS
--   rather than malfunctions: @COMPARE_MISMATCH@ and @DECODE_FAILED@
--   both describe the fixtures, not this program. It still exits
--   non-zero for them — a caller judging only the status must not read
--   "these saves differ" as success — and additionally writes a
--   machine-readable report to @--output@ so the caller can name WHICH
--   generation first diverged instead of guessing at the first two.
module Main where

import UPrelude
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as AK
import Data.Aeson ((.=))
import qualified Data.Serialize as S
import qualified Data.Text as T
import Data.List (sortOn)
import System.Environment (getArgs, getProgName)
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)
import Control.Exception (IOException, try)

import World.Save.Envelope (decodeSessionEnvelope, currentEnvelopeVersion
                           , metadataComponentId, LuaComponentSpec(..))
import World.Save.Envelope.Codec (DecodedEnvelope(..), decodeEnvelope
                                 , encodeEnvelope)
import World.Save.Envelope.Types (ComponentId(..), ComponentDescriptor(..)
                                 , EnvelopeManifest(..)
                                 , defaultEnvelopeLimits)
import World.Save.Component (componentKnownIds)
import World.Save.Compat.SessionV90 (sessionComponentId)
import World.Save.Snapshot
import World.Save.Types
import World.Page.Types (WorldPageId(..))
import Building.Types (BuildingId(..))
import Unit.Types (UnitId(..))
import Unit.Sim.Types (UnitSimState(..))
import Craft.Bills (CraftBills(..), CraftBill(..), BillId(..))
import Power.Types (PowerNodes(..), PowerNode(..), PowerNodeId(..))
import Item.Ground (GroundItems(..))
import Item.Types (ItemInstance(..))
import Item.Knowledge

-- | The live Lua persistent-component names @decodeSessionEnvelope@ is
--   given as BOTH its known and its required set when deriving a
--   canonical summary — the same pair the GHCi summary program passed.
luaComponentNames ∷ HS.HashSet Text
luaComponentNames = HS.fromList ["unit_ai", "building_spawn"]

-- | The known-id set used for a STRUCTURAL re-encode (@set-timestamp@):
--   every Haskell-owned component plus the two live Lua ones.
--
--   It widens what may APPEAR; the reader-required set stays empty.
--   Reusing it for both would demand that whatever fixture is being
--   rewritten carry every component the current build knows about —
--   including any OPTIONAL one added after the fixture was captured
--   (#1087's container-knowledge), which by definition it need not.
structuralKnownIds ∷ HS.HashSet ComponentId
structuralKnownIds =
    HS.insert metadataComponentId
      (HS.insert (ComponentId "lua.unit_ai")
        (HS.insert (ComponentId "lua.building_spawn") componentKnownIds))

-- | The known-id set used for a raw DESCRIPTOR dump: 'structuralKnownIds'
--   widened with BOTH retired legacy ids (@session@ and @lua-state@).
--
--   One universal set serves every fixture regardless of which shape it
--   actually is, because a descriptor dump needs only the envelope's
--   STRUCTURAL manifest to decode — no application-level decode or
--   migration — which succeeds for any of B1\/B2\/B3\/C3's tracked
--   shapes.
universalKnownIds ∷ HS.HashSet ComponentId
universalKnownIds =
    HS.insert sessionComponentId
      (HS.insert (ComponentId "lua-state") structuralKnownIds)

main ∷ IO ()
main = getArgs ≫= \case
    ("summary" : rest)       → runSummary rest
    ("set-timestamp" : rest) → runSetTimestamp rest
    ("descriptors" : rest)   → runDescriptors rest
    ("compare" : rest)       → runCompare rest
    args                     → usageFailure args

usageFailure ∷ ∀ α. [String] → IO α
usageFailure args = do
    prog ← getProgName
    hPutStrLn stderr $ case args of
        []       → "no operation given"
        (op : _) → "unknown operation: " ⧺ op
    hPutStrLn stderr $ unlines
        [ "usage:"
        , "  " ⧺ prog ⧺ " summary --fixture PATH --output PATH"
        , "  " ⧺ prog ⧺ " set-timestamp --fixture PATH --timestamp TS "
              ⧺ "[--output PATH]"
        , "  " ⧺ prog ⧺ " descriptors --output PATH FIXTURE [FIXTURE ...]"
        , "  " ⧺ prog ⧺ " compare --output PATH FIXTURE FIXTURE [FIXTURE ...]"
        ]
    exitFailure

-- | Pull @--flag value@ out of an argument list, answering the value and
--   the arguments that remain. A flag given twice keeps the LAST value,
--   matching how an ordinary command line behaves; a flag given with no
--   value is a usage error, not a silently absent one.
takeFlag ∷ String → [String] → Either String (Maybe String, [String])
takeFlag _ [] = Right (Nothing, [])
takeFlag flag (a : rest)
    | a ≡ flag = case rest of
        (v : more) → do
            (later, leftover) ← takeFlag flag more
            pure (Just (fromMaybe v later), leftover)
        [] → Left (flag ⧺ " requires a value")
    | otherwise = do
        (v, leftover) ← takeFlag flag rest
        pure (v, a : leftover)

-- | Resolve a REQUIRED @--flag value@, failing with the flag's own name.
requireFlag ∷ String → [String] → Either String (String, [String])
requireFlag flag args = do
    (v, leftover) ← takeFlag flag args
    case v of
        Just value → Right (value, leftover)
        Nothing    → Left (flag ⧺ " is required")

-- | Exit 1 naming the argument problem, rather than proceeding with a
--   half-resolved command line.
orUsageFailure ∷ ∀ α. Either String α → IO α
orUsageFailure = either report pure
  where
    report message = do
        hPutStrLn stderr message
        exitFailure

-- | Report an operation's failure on stderr — naming the fixture path
--   and the codec's own error text (requirement 5) — and exit 1.
failWith ∷ ∀ α. String → FilePath → String → IO α
failWith marker fixturePath detail = do
    hPutStrLn stderr (marker ⧺ ": " ⧺ fixturePath ⧺ ": " ⧺ detail)
    exitFailure

-- * summary

-- | Derive a fixture's canonical-summary JSON DIRECTLY from its real,
--   decoded @SessionSnapshot@\/@SaveMetadata@ — not from live engine
--   queries, several of which (hour\/minute of day, in particular) have
--   no debug-console verb to read at all.
--
--   The object below mirrors EXACTLY the schema
--   @test-headless\/Test\/Headless\/World\/Save\/Compat\/Baselines.hs@'s
--   @ExpectedSummary@\/@ExpectedPage@\/@Expected*@ Aeson types parse —
--   the two must be kept in sync by hand if that schema ever grows a
--   field.
runSummary ∷ [String] → IO ()
runSummary args = do
    (fixturePath, rest0) ← orUsageFailure (requireFlag "--fixture" args)
    (outputPath, rest1)  ← orUsageFailure (requireFlag "--output" rest0)
    unless (null rest1) (orUsageFailure (Left ("unexpected arguments: "
                                               ⧺ unwords rest1)))
    bytes ← BS.readFile fixturePath
    case decodeSessionEnvelope luaComponentNames luaComponentNames bytes of
        Left err → failWith "DUMP_FAILED" fixturePath
                            ("decode: " ⧺ T.unpack err)
        Right (meta, snap, luaComponents, isMigrated) → do
            BSL.writeFile outputPath
                (Aeson.encode (canonicalSummary meta snap
                                                (length luaComponents)
                                                isMigrated))
            putStrLn "DUMP_OK"

canonicalSummary
    ∷ SaveMetadata → SessionSnapshot → Int → Bool → Aeson.Value
canonicalSummary meta snap luaComponentCount isMigrated = Aeson.object
    [ "metadata" .= Aeson.object
        [ "seed" .= smSeed meta, "worldSize" .= smWorldSize meta
        , "plateCount" .= smPlateCount meta, "worldName" .= smWorldName meta
        , "worldGloss" .= smWorldGloss meta ]
    , "gameTime" .= snapGameTime snap
    , "nextItemId" .= snapNextItemId snap
    , "nextBuildingId" .= snapNextBuildingId snap
    , "nextUnitId" .= snapNextUnitId snap
    , "camera" .= Aeson.object
        [ "ownerPage" .= fmap (\(WorldPageId p) → p) (lcsOwnerPage cam)
        , "x" .= lcsX cam, "y" .= lcsY cam, "zoom" .= lcsZoom cam
        , "facing" .= T.pack (show (lcsFacing cam)) ]
    , "activePage" .= activePageText
    , "visiblePages" .= map (\(WorldPageId p) → p) (snapVisiblePages snap)
    , "pages" .= map dumpPage
        (sortOn (\(WorldPageId p, _) → p) (HM.toList (snapPages snap)))
    , "luaComponentCount" .= luaComponentCount
      -- #2512: SESSION-scoped, so it sits beside the other whole-session
      -- values rather than inside 'dumpPage'. Absent from every expected
      -- summary generated before this component existed, which is
      -- exactly right for those fixtures -- the Baselines reader
      -- defaults a missing key to the empty list, the value a save
      -- carrying no such component really restores.
    , "portableKnowledge" .= map dumpPortableRecord
        (sortOn fst (HM.toList (pkRecords (snapPortableKnowledge snap))))
    , "isMigratedLegacyBaseline" .= isMigrated
    ]
  where
    cam = snapLiveCamera snap
    WorldPageId activePageText = snapActivePage snap

-- | One remembered PORTABLE container (#2512). Both observations are
--   independently optional and are emitted as such — a @null@ weight or
--   reveal time means "never learned", which the reader must be able to
--   tell from a zero.
dumpPortableRecord ∷ (Word64, PortableRecord) → Aeson.Value
dumpPortableRecord (iid, r) = Aeson.object
    [ "instanceId" .= iid
    , "state" .= portableKnowledgeStateId (portableRecordState (Just r))
    , "storedWeight" .= fmap woWeight (prWeight r)
    , "weighedAt" .= fmap woAt (prWeight r)
    , "revealedAt" .= fmap coAt (prContents r)
    , "items" .= fmap (map dumpItem ∘ coItems) (prContents r)
    ]

dumpItem ∷ ItemInstance → Aeson.Value
dumpItem i = Aeson.object
    [ "defName" .= iiDefName i, "instanceId" .= iiInstanceId i
    , "currentFill" .= iiCurrentFill i, "quality" .= iiQuality i
    , "condition" .= iiCondition i, "weight" .= iiWeight i
    , "contents" .= map dumpItem (iiContents i) ]

dumpBuilding ∷ (BuildingId, BuildingInstanceSnapshot) → Aeson.Value
dumpBuilding (bid, b) = Aeson.object
    [ "id" .= unBuildingId bid, "defName" .= bisDefName b
    , "anchorX" .= bisAnchorX b, "anchorY" .= bisAnchorY b
    , "gridZ" .= bisGridZ b, "buildProgress" .= bisBuildProgress b ]

dumpUnit ∷ (UnitId, UnitInstanceSnapshot) → Aeson.Value
dumpUnit (uid, u) = Aeson.object
    [ "id" .= unUnitId uid, "defName" .= uisDefName u
    , "gridX" .= uisGridX u, "gridY" .= uisGridY u
    , "gridZ" .= uisGridZ u, "facing" .= T.pack (show (uisFacing u))
    , "activity" .= uisActivity u, "pose" .= uisPose u
    , "inventory" .= map dumpItem (uisInventory u) ]

dumpSim ∷ (UnitId, UnitSimState) → Aeson.Value
dumpSim (uid, s) = Aeson.object
    [ "unitId" .= unUnitId uid, "realX" .= usRealX s, "realY" .= usRealY s
    , "gridZ" .= usGridZ s, "pose" .= T.pack (show (usPose s))
    , "state" .= T.pack (show (usState s))
    , "facing" .= T.pack (show (usFacing s)) ]

dumpBill ∷ CraftBill → Aeson.Value
dumpBill b = Aeson.object
    [ "id" .= unBillId (cbId b), "station" .= unBuildingId (cbStation b)
    , "recipe" .= cbRecipe b, "remaining" .= cbRemaining b
    , "claimant" .= fmap unUnitId (cbClaimant b)
    , "mode" .= T.pack (show (cbMode b)) ]

dumpNode ∷ PowerNode → Aeson.Value
dumpNode n = Aeson.object
    [ "id" .= unPowerNodeId (pnId n), "building" .= unBuildingId (pnBuilding n)
    , "role" .= T.pack (show (pnRole n)), "peakWatts" .= pnPeakWatts n
    , "capacityWh" .= pnCapacityWh n, "storedWh" .= pnStoredWh n ]

dumpPage ∷ (WorldPageId, PageSnapshot) → Aeson.Value
dumpPage (WorldPageId pid, page) = Aeson.object
    [ "pageId" .= pid
    , "buildingCount" .= HM.size (bsnInstances (pgsBuildings page))
    , "unitCount" .= HM.size (usnInstances (pgsUnits page))
    , "unitSimStateCount" .= HM.size (pgsUnitSimStates page)
    , "craftBillCount" .= HM.size (cbsBills (pgsCraftBills page))
    , "powerNodeCount" .= HM.size (pnsNodes (pgsPowerNodes page))
    , "groundItemCount" .= HM.size (gisItems (pgsGroundItems page))
    , "timeHour" .= pgsTimeHour page, "timeMinute" .= pgsTimeMinute page
    -- #2471: the sub-minute calendar progress beside the whole
    -- minutes. Absent from every expected summary generated before
    -- world-pages v11, which is exactly right for those fixtures --
    -- the Baselines reader defaults a missing key to 0, the value a
    -- migrated pre-v11 payload really carries.
    , "timeRemainder" .= pgsTimeRemainder page
    , "dateYear" .= pgsDateYear page, "dateMonth" .= pgsDateMonth page
    , "dateDay" .= pgsDateDay page
    , "mapMode" .= T.pack (show (pgsMapMode page))
    , "buildings" .= map dumpBuilding
        (sortOn (unBuildingId ∘ fst)
           (HM.toList (bsnInstances (pgsBuildings page))))
    , "units" .= map dumpUnit
        (sortOn (unUnitId ∘ fst) (HM.toList (usnInstances (pgsUnits page))))
    , "unitSimStates" .= map dumpSim
        (sortOn (unUnitId ∘ fst) (HM.toList (pgsUnitSimStates page)))
    , "craftBills" .= map dumpBill
        (sortOn cbId (HM.elems (cbsBills (pgsCraftBills page))))
    , "powerNodes" .= map dumpNode
        (sortOn pnId (HM.elems (pnsNodes (pgsPowerNodes page))))
    ]

-- * set-timestamp

-- | Overwrite ONLY the @metadata@ component's @smTimestamp@, leaving
--   every other component's version\/required\/payload bytes completely
--   untouched, and write the result to @--output@ (defaulting to the
--   input, i.e. in place).
--
--   @engine.saveWorld@ — the real production save path
--   @--generate-session@ deliberately reuses — always stamps the
--   CURRENT wall-clock time into @smTimestamp@ (by design: an ordinary
--   player save needs each save to carry a distinct real timestamp).
--   That means two @--generate-session@ runs over IDENTICAL
--   seed\/world-size\/plate-count\/spawn arguments produce DIFFERENT
--   envelope bytes and sha256s purely from wall-clock drift, defeating
--   the reproducibility a tracked fixture's checksum depends on. This
--   normalizes that ONE field post-generation, via the real envelope
--   codec (decode the raw manifest\/payloads, rebuild every component's
--   spec verbatim except metadata's, re-encode) rather than a
--   hand-rolled binary patch — so the fix stays correct through any
--   future envelope framing change.
--
--   The @--output@ form is what lets the save-compat self-test build two
--   envelopes differing ONLY in that field, and is the same code path
--   normalization itself takes.
runSetTimestamp ∷ [String] → IO ()
runSetTimestamp args = do
    (fixturePath, rest0) ← orUsageFailure (requireFlag "--fixture" args)
    (timestamp, rest1)   ← orUsageFailure (requireFlag "--timestamp" rest0)
    (mOutput, rest2)     ← orUsageFailure (takeFlag "--output" rest1)
    unless (null rest2) (orUsageFailure (Left ("unexpected arguments: "
                                               ⧺ unwords rest2)))
    let outputPath = fromMaybe fixturePath mOutput
    bytes ← BS.readFile fixturePath
    case decodeEnvelope defaultEnvelopeLimits currentEnvelopeVersion
                        structuralKnownIds HS.empty bytes of
        Left e → failWith "NORMALIZE_FAILED" fixturePath
                          ("decode: " ⧺ show e)
        Right decoded → case decodeMetadata decoded of
            Left e → failWith "NORMALIZE_FAILED" fixturePath
                              ("metadata decode: " ⧺ e)
            Right meta →
                let fixedMeta = meta { smTimestamp = T.pack timestamp }
                    newSpecs =
                        [ ( cdId d, cdVersion d, cdRequired d
                          , if cdId d ≡ metadataComponentId
                               then S.encode fixedMeta
                               else HM.lookupDefault BS.empty (cdId d)
                                        (dePayloads decoded) )
                        | d ← emComponents (deManifest decoded) ]
                in case encodeEnvelope defaultEnvelopeLimits
                                       currentEnvelopeVersion newSpecs of
                    Left e → failWith "NORMALIZE_FAILED" fixturePath
                                      ("encode: " ⧺ show e)
                    Right outBytes → do
                        BS.writeFile outputPath outBytes
                        putStrLn "NORMALIZE_OK"

decodeMetadata ∷ DecodedEnvelope → Either String SaveMetadata
decodeMetadata decoded =
    S.decode (HM.lookupDefault BS.empty metadataComponentId
                               (dePayloads decoded))

-- * descriptors

-- | Decode a batch of fixtures' RAW envelope manifests — their actual
--   on-disk @(id, version, required)@ descriptors, exactly as the real
--   codec sees them — and write them all out as one JSON object keyed
--   by fixture path.
--
--   The save-compat audit's version-coverage checks previously trusted a
--   baseline's declared @components[]@ versions as-is, entirely from the
--   manifest JSON, never cross-checked against what a fixture's OWN
--   bytes contain: bumping only the manifest's declared version (with no
--   fixture change at all) satisfied every coverage check while
--   validating nothing. This dump is what grinds that claim against
--   real, decoded descriptors.
--
--   EVERY fixture must decode: one failure fails the whole batch, so a
--   corrupt fixture can never be silently omitted from an object the
--   caller then reads as complete.
runDescriptors ∷ [String] → IO ()
runDescriptors args = do
    (outputPath, fixturePaths) ← orUsageFailure (requireFlag "--output" args)
    when (null fixturePaths)
         (orUsageFailure (Left "descriptors requires at least one fixture"))
    results ← mapM describeOne fixturePaths
    case [ (p, e) | (p, Left e) ← results ] of
        ((p, e) : _) → failWith "DESCRIPTOR_DUMP_FAILED" p e
        [] → do
            BSL.writeFile outputPath (Aeson.encode (Aeson.object
                [ AK.fromString p .= descs | (p, Right descs) ← results ]))
            putStrLn "DESCRIPTOR_DUMP_OK"

describeOne ∷ FilePath → IO (FilePath, Either String [Aeson.Value])
describeOne path = do
    bytes ← BS.readFile path
    pure $ case decodeEnvelope defaultEnvelopeLimits currentEnvelopeVersion
                               universalKnownIds HS.empty bytes of
        Left e → (path, Left (show e))
        Right decoded → (path, Right
            [ Aeson.object
                [ "id" .= componentIdText (cdId d), "version" .= cdVersion d
                , "required" .= cdRequired d ]
            | d ← emComponents (deManifest decoded) ])

componentIdText ∷ ComponentId → Text
componentIdText (ComponentId t) = t

-- * compare

-- | Decode N independently-produced session saves and report whether
--   every one is structurally IDENTICAL to the first.
--
--   This is @tools\/persistence_snapshot.py@'s
--   @compare_session_files@ (#767, save-overhaul D1 requirement 1) with
--   the GHCi program it used to feed @cabal repl
--   test:synarchy-test-headless@ compiled in place (#2274). The
--   comparison itself is unchanged, and is deliberately NOT a bespoke
--   field-by-field schema:
--
--     * @SessionSnapshot@ derives @Eq@ and holds only persistent
--       gameplay state, so structural equality of two
--       @decodeSessionEnvelope@-assembled snapshots IS the canonical,
--       order-independent comparison (every collection is
--       @HashMap@-keyed).
--     * @scripts\/lib\/data_codec.lua@'s canonical (sorted-key) encoding
--       makes two independently-produced encodings of the SAME logical
--       Lua state byte-identical, so the raw @lua.\<module\>@ payload
--       bytes are as strong a comparison with no decode step and no live
--       Lua VM at all.
--
--   Both halves are checked, and both are reported separately: a
--   snapshot that matches while a Lua component's bytes do not is a
--   different defect from the reverse, and collapsing them into one
--   \"differs\" answer would hide which.
--
--   Every path is compared against the FIRST, which is what makes the
--   report able to name the generation that first diverged. Pairwise
--   equality against one reference is transitive equality across the
--   whole set: if every later file equals the first, they equal each
--   other.
runCompare ∷ [String] → IO ()
runCompare args = do
    (outputPath, fixturePaths) ← orUsageFailure (requireFlag "--output" args)
    when (length fixturePaths < 2)
         (orUsageFailure (Left ("compare requires at least two fixtures, got "
                                ⧺ show (length fixturePaths))))
    decoded ← mapM decodeOne fixturePaths
    case [ (p, e) | (p, Left e) ← decoded ] of
        errs@(_ : _) → do
            writeCompareReport outputPath (Aeson.object
                [ "outcome" .= ("decode_failed" ∷ Text)
                , "decodeErrors" .= [ Aeson.object [ "path" .= T.pack p
                                                   , "error" .= T.pack e ]
                                    | (p, e) ← errs ] ])
            putStrLn ("DECODE_FAILED: " ⧺ show errs)
            exitFailure
        [] → do
            let compared = [ (p, v) | (p, Right v) ← decoded ]
            case compared of
                [] → usageFailure ["compare"]
                ((refPath, refValue) : rest) → do
                    let snapDiffs = [ p | (p, v) ← rest
                                        , comparedSnapshot v
                                            ≢ comparedSnapshot refValue ]
                        luaDiffs  = [ p | (p, v) ← rest
                                        , comparedLua v ≢ comparedLua refValue ]
                        report outcome = Aeson.object
                            [ "outcome" .= (outcome ∷ Text)
                            , "reference" .= T.pack refPath
                            , "snapshotDiffers" .= map T.pack snapDiffs
                            , "luaComponentDiffers" .= map T.pack luaDiffs ]
                    if null snapDiffs ∧ null luaDiffs
                      then do
                        writeCompareReport outputPath (report "ok")
                        putStrLn "COMPARE_OK"
                      else do
                        writeCompareReport outputPath (report "mismatch")
                        putStrLn ("COMPARE_MISMATCH: snapshot-differs="
                                  ⧺ show snapDiffs
                                  ⧺ " lua-component-differs=" ⧺ show luaDiffs)
                        exitFailure

-- | The two halves one save contributes to the comparison: its decoded
--   session snapshot, and every live @lua.\<module\>@ component's raw
--   payload bytes keyed by component id.
data ComparedSave = ComparedSave
    { comparedSnapshot ∷ SessionSnapshot
    , comparedLua      ∷ HM.HashMap Text BS.ByteString
    }

-- | Decode one save, reporting an unreadable FILE the same way an
--   undecodable one is reported.
--
--   @BS.readFile@ throws, and @compare@'s inputs are paths a probe
--   produced rather than tracked fixtures: a generation that was never
--   written, or was cleaned up early, is a plausible thing to be handed.
--   Letting that escape would end the program with an uncaught
--   @IOException@ and no marker at all, which the Python side can only
--   classify as "the toolchain broke" — when what actually happened is
--   a named save this run could not read, which is exactly what
--   @DECODE_FAILED@ is for.
decodeOne ∷ FilePath → IO (FilePath, Either String ComparedSave)
decodeOne path = do
    readResult ← try (BS.readFile path)
    pure $ case readResult of
        Left e → (path, Left ("read: " ⧺ show (e ∷ IOException)))
        Right bytes → case decodeSessionEnvelope luaComponentNames
                                                 luaComponentNames bytes of
            Left err → (path, Left (T.unpack err))
            Right (_, snap, comps, _) → (path, Right (ComparedSave snap
                (HM.fromList [ (lcsId c, lcsPayload c) | c ← comps ])))

-- | Write the machine-readable comparison report.
--
--   Written for EVERY outcome, including the two that exit non-zero:
--   the caller's whole reason for asking is to learn which generation
--   diverged, and a report withheld on the failing path would leave it
--   with nothing but the stdout line to parse.
writeCompareReport ∷ FilePath → Aeson.Value → IO ()
writeCompareReport outputPath = BSL.writeFile outputPath ∘ Aeson.encode
