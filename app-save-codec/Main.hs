-- | The save-compatibility tooling's compiled codec helper (issue
--   #2273).
--
--   Five operations the Python save tooling needs from the REAL
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
import qualified Data.Text.Encoding as TE
import Text.Printf (printf)
import Data.List (sortOn)
import System.Environment (getArgs, getProgName)
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)
import Control.Exception (IOException, try)

import World.Save.Envelope (decodeSessionEnvelope, currentEnvelopeVersion
                           , metadataComponentId, LuaComponentSpec(..)
                           , encodeSessionSnapshot)
import World.Save.Envelope.Codec (DecodedEnvelope(..), decodeEnvelope
                                 , encodeEnvelope)
import World.Save.Envelope.Types (ComponentId(..), ComponentDescriptor(..), fnv1a64
                                 , EnvelopeManifest(..)
                                 , defaultEnvelopeLimits)
import World.Save.Component (componentKnownIds)
import World.Save.Compat.SessionV90 (sessionComponentId)
import World.Save.Snapshot
import World.Save.Physiology (withoutRebuiltCaches)
import World.Save.Types
import World.Page.Types (WorldPageId(..))
import World.Edit.Types (WorldEdit(..))
import World.Fluid.Exact (fluidUnitsPerZ, exactTopLevel)
import World.Fluid.Types (FluidType(..))
import Building.Types (BuildingId(..))
import Unit.Types (UnitId(..))
import Unit.Physiology.Types (physioModeText)
import Unit.Sim.Types (UnitSimState(..))
import Craft.Bills (CraftBills(..), CraftBill(..), BillId(..))
import Power.Types (PowerNodes(..), PowerNode(..), PowerNodeId(..))
import Item.Ground (GroundItems(..))
import Item.Types (ItemInstance(..))
import Item.Knowledge
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Aeson ((.:), (.:?), (.!=))
import Control.Monad (foldM)
import Gameplay.Tags.Index (categoryForward, setCategoryTags)
import Gameplay.Tags.Page (PageTagsSnapshot(..))
import Data.Int (Int64)
import World.Flora.Identity (floraInstanceIdToLua, floraInstanceIdFromLua)
import Gameplay.Tags.Tag (GameplayTag, gameplayTagText, mkGameplayTag, itemTargetId, mkItemTarget)
import Scenario.Items.Refs (itemRefsFromList, itemRefsToAscList, restrictItemRefs)
import Location.Instance (LocationInstances(..), LocationInstanceId(..))
import World.Generate.Types (WorldGenParams(..))
import World.Save.Envelope.Types (EnvelopeError(..))
import World.Save.Component.Types (luaComponentPrefix)

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
    ("seed-tags" : rest)     → runSeedTags rest
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
        , "  " ⧺ prog ⧺ " seed-tags --fixture PATH --spec PATH --output PATH"
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
      -- #2703: the session's physical-item gameplay tag assignments,
      -- keyed by item instance id, SESSION-scoped like the item
      -- allocator. Absent before core-session v2, which the Baselines
      -- reader reads as "no assignments".
    , "itemTags" .=
        [ Aeson.object [ "id" .= itemTargetId t
                       , "tags" .= map gameplayTagText (Set.toAscList ts) ]
        | (t, ts) ← Map.toAscList (snapItemTags snap) ]
      -- #2709: the items' scenario references, keyed by item instance
      -- id. Absent before core-session v3, which the Baselines reader
      -- reads as "no references".
    , "itemRefs" .=
        [ Aeson.object [ "id" .= i, "ref" .= r ]
        | (i, r) ← itemRefsToAscList (snapItemRefs snap) ]
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
    , "inventory" .= map dumpItem (uisInventory u)
      -- #2829: the explicit physiology mode intent, so omitting or losing
      -- a NONEMPTY map is observable; and the one durable stat the
      -- units-v3 fixture (z7) exists to pin, #2761's salt reserve, so a
      -- present zero or partial value cannot silently become absent.
    , "physioOverrides" .= Aeson.object
        [ AK.fromText sid .= physioModeText m
        | (sid, m) ← sortOn fst (HM.toList (uisPhysioOverrides u)) ]
    , "durableStats" .= Aeson.object
        [ AK.fromText k .= v
        | k ← ["salt_store"], Just v ← [HM.lookup k (uisStats u)] ] ]

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
    -- #2520: the EXACT fluid plane. Neither the whole-z Lua queries nor
    -- the dump's `fluidSurf` can express a remainder, so neither can
    -- serve as this format's precision oracle; these values can.
    --
    -- Three layers, because each closes a hole the one above it
    -- leaves open:
    --
    --   * the page totals catch a rescale of the whole plane, but a
    --     TYPE SWAP leaves all three unchanged, and so does any pair of
    --     compensating remainder corruptions that keeps the sum;
    --   * `byType` adds, per fluid type, a histogram of how many cells
    --     sit at each top fill level 1..8 — so a swap moves a row and a
    --     one-unit cell is a level-1 row under its own type. But a
    --     histogram is still a MULTISET: two same-type cells moved by
    --     +8 and -8 keep their levels AND the sum, and a permutation of
    --     two cells' surfaces is invisible to it;
    --   * `digest` closes both. It is an fnv1a64 over EVERY snapshot in
    --     canonical coordinate order, carrying each cell's own
    --     coordinate, type and exact surface, so any change to any cell
    --     — value, type, position, or ordering — moves it.
    --
    -- `samples` then makes the digest legible: a deterministic,
    -- coordinate-ordered witness per (type, level), so a mismatch names
    -- real cells with their real exact surfaces instead of only a
    -- changed hash. Those are the mandated one-unit, seven-unit and
    -- partial-Ocean cells, pinned individually and by coordinate.
    --
    -- What the save records is the exact SURFACE, not volume-over-
    -- terrain: terrain is regenerated from the page's own gen params
    -- rather than stored, so it is not available here. Surface is the
    -- durable quantity, and against the deterministic terrain a given
    -- seed regenerates it fixes the volume too; the volume-over-terrain
    -- identity itself is proved at a KNOWN terrain by the hspec round
    -- trip ("Sim.Fluid.Exact" / "save migrations").
    --
    -- Absent from every summary generated before world-edits v4, which
    -- the Baselines reader treats as "this fixture pins nothing here"
    -- rather than as zero.
    , "fluidSnapshots" .= dumpFluidSnapshots page
      -- #2701: the page's authoritative general gameplay tag
      -- assignments, per category, with their target ids -- so losing,
      -- leaking or re-targeting one is observable. Absent from every
      -- summary generated before units v5 / buildings v3 / world-pages
      -- v14, which the Baselines reader reads as "no assignments": the
      -- value every migrated older payload really restores.
    , "gameplayTags" .= Aeson.object
        [ "units" .= dumpTagged unUnitId (usnTags (pgsUnits page))
        , "buildings" .= dumpTagged unBuildingId (bsnTags (pgsBuildings page))
        , "locations" .= dumpTagged unLocationInstanceId
            (categoryForward (lisTags (wgpLocationInstances (pgsGenParams page))))
          -- #2705: cells, flora occurrences (with their recorded tile)
          -- and groundcover plots, by target identity. Absent before
          -- world-edits v5 and read as "no assignments".
        , "tiles" .=
            [ Aeson.object [ "x" .= x, "y" .= y, "z" .= z
                           , "tags" .= map gameplayTagText (Set.toAscList ts) ]
            | ((x, y, z), ts) ← Map.toAscList (ptsTiles (pgsPageTags page)) ]
        , "plants" .=
            [ Aeson.object [ "id" .= floraInstanceIdToLua fid
                           , "tile" .= Map.lookup fid (ptsOccurrenceTiles (pgsPageTags page))
                           , "tags" .= map gameplayTagText (Set.toAscList ts) ]
            | (fid, ts) ← Map.toAscList (ptsOccurrences (pgsPageTags page)) ]
        , "plots" .=
            [ Aeson.object [ "x" .= x, "y" .= y
                           , "tags" .= map gameplayTagText (Set.toAscList ts) ]
            | ((x, y), ts) ← Map.toAscList (ptsPlots (pgsPageTags page)) ]
        ]
    ]
  where
    dumpTagged ∷ Aeson.ToJSON i ⇒ (k → i) → Map.Map k (Set.Set GameplayTag) → [Aeson.Value]
    dumpTagged key m =
        [ Aeson.object [ "id" .= key k, "tags" .= map gameplayTagText (Set.toAscList ts) ]
        | (k, ts) ← Map.toAscList m ]
    dumpFluidSnapshots pg =
        let ordered = fluidSnapshotCells pg
            cells = [ (ft, z) | (_, _, ft, z) ← ordered ]
            surfaces = [ z | (_, _, _, z) ← ordered ]
        in Aeson.object
            [ "count" .= length surfaces
              -- Every cell whose TOP z is only partly filled: the
              -- state the pre-#2520 whole-z plane could not express.
            , "partialCount" .= length
                [ () | z ← surfaces, exactTopLevel z ≢ fluidUnitsPerZ ]
              -- Exact to the unit, so a rescale of even one cell shows.
            , "exactSum" .= sum surfaces
            , "byType" .= map (dumpFluidType cells) fluidTypeNames
            , "digest" .= fluidSnapshotDigest ordered
            , "samples" .= map dumpFluidSample (fluidSnapshotSamples ordered) ]

    -- Every type is emitted, present or not, so a type that VANISHES
    -- from a resave is a row going to zero rather than a row going
    -- missing — which a reader comparing lists would have to special-
    -- case, and a reader comparing sets would not see at all.
    dumpFluidType cells (ft, name) =
        let mine = [ z | (t, z) ← cells, t ≡ ft ]
        in Aeson.object
            [ "type" .= name
            , "count" .= length mine
            , "exactSum" .= sum mine
              -- levels !! (k - 1) is how many of this type's cells have
              -- top fill level k. Index 7 (level 8) is the full-level
              -- population; every other index is a remainder class.
            , "levels" .= [ length [ () | z ← mine, exactTopLevel z ≡ k ]
                          | k ← [1 .. fluidUnitsPerZ] ] ]

-- | Every fluid snapshot a page carries, in CANONICAL coordinate order
--   (@gx@ then @gy@) rather than hashmap order, so the sequence below
--   is a function of the page's content alone.
fluidSnapshotCells ∷ PageSnapshot → [(Int, Int, FluidType, Int)]
fluidSnapshotCells pg = sortOn (\(gx, gy, _, _) → (gx, gy))
    [ (gx, gy, ft, z)
    | edits ← HM.elems (pgsEdits pg)
    , WeSetFluidSnapshot gx gy ft z ← edits ]

-- | fnv1a64 over the whole coordinate-ordered sequence, each cell
--   contributing its coordinate, its type and its EXACT surface.
--
--   This is the layer that makes the plane's per-cell values durable
--   evidence rather than a multiset: two cells whose surfaces moved by
--   @+8@ and @-8@, or whose surfaces were swapped between them, leave
--   every count, sum and histogram entry identical and move this.
--
--   Hashed from the decimal rendering of the tuple, which is total for
--   every 'Int' including negatives — the plane is signed, and a
--   fixed-width encoder would have to pick a width this does not need.
fluidSnapshotDigest ∷ [(Int, Int, FluidType, Int)] → T.Text
fluidSnapshotDigest ordered =
    T.pack (printf "%016x" (fnv1a64 (TE.encodeUtf8 rendered)))
  where
    rendered = T.intercalate ";"
        [ T.pack (show gx) <> "," <> T.pack (show gy) <> ","
          <> fluidTypeName ft <> "," <> T.pack (show z)
        | (gx, gy, ft, z) ← ordered ]

-- | A deterministic, legible WITNESS for each (type, top fill level)
--   the page actually carries: the coordinate-FIRST cell of that class.
--
--   Bounded by construction — four types times eight levels — and
--   ordered, so the list is a function of the page's content. Its job
--   is to make a digest mismatch readable and to pin the mandated
--   cases (a one-unit cell, a seven-unit cell, a partial Ocean cell)
--   by coordinate and exact value, not to enumerate the plane: the
--   digest above is what covers every cell.
fluidSnapshotSamples
    ∷ [(Int, Int, FluidType, Int)] → [(Int, Int, FluidType, Int)]
fluidSnapshotSamples ordered =
    [ cell
    | (ft, _) ← fluidTypeNames
    , level ← [1 .. fluidUnitsPerZ]
    , cell ← take 1 [ c | c@(_, _, t, z) ← ordered
                        , t ≡ ft, exactTopLevel z ≡ level ] ]

dumpFluidSample ∷ (Int, Int, FluidType, Int) → Aeson.Value
dumpFluidSample (gx, gy, ft, z) = Aeson.object
    [ "gx" .= gx, "gy" .= gy
    , "type" .= fluidTypeName ft
    , "exactSurface" .= z
      -- Derivable from exactSurface, emitted anyway so a mismatch reads
      -- as "this cell's TOP LEVEL changed" without the reader doing
      -- modular arithmetic in its head.
    , "topLevel" .= exactTopLevel z ]

fluidTypeName ∷ FluidType → T.Text
fluidTypeName Ocean = "ocean"
fluidTypeName Lake  = "lake"
fluidTypeName River = "river"
fluidTypeName Lava  = "lava"

fluidTypeNames ∷ [(FluidType, T.Text)]
fluidTypeNames = [ (ft, fluidTypeName ft)
                 | ft ← [Ocean, Lake, River, Lava] ]

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
--   The ONE thing it does not compare is the rebuilt caches
--   ('World.Save.Physiology.rebuiltCacheStats', #2831): stats a load
--   recomputes rather than reads back, which a save taken a tick before
--   the recompute legitimately holds stale. They are classified
--   serialized-but-ignored, not durable, and compared canonically by the
--   hspec group @physiology stat modes@ instead.
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
            Right (_, snap, comps, _) → (path, Right (ComparedSave
                (withoutRebuiltCaches snap)
                (HM.fromList [ (lcsId c, lcsPayload c) | c ← comps ])))

-- | Write the machine-readable comparison report.
--
--   Written for EVERY outcome, including the two that exit non-zero:
--   the caller's whole reason for asking is to learn which generation
--   diverged, and a report withheld on the failing path would leave it
--   with nothing but the stdout line to parse.
writeCompareReport ∷ FilePath → Aeson.Value → IO ()
writeCompareReport outputPath = BSL.writeFile outputPath ∘ Aeson.encode

-- * seed-tags

-- | Write general gameplay tag assignments (#2701) into a save and
--   write the result to @--output@: decode the whole session through
--   the real decoder ('decodeSessionEnvelope'), set each named target's
--   COMPLETE tag set on its owning page, and re-encode through the very
--   encoder an ordinary save uses ('encodeSessionSnapshot'), carrying
--   every Lua component's payload across verbatim.
--
--   This is fixture support for the compatibility corpus and
--   @tools\/gameplay_tag_persistence_probe.py@, nothing more: there is
--   no gameplay verb for tags yet (SCN-06), and this program is not
--   reachable from Lua. It deliberately does NOT check that a target
--   exists, so a probe can seed a dangling assignment and prove the
--   load's integrity rules drop it; a malformed tag (the empty string)
--   is refused, because the codec itself could never carry one.
--
--   @--spec@ is JSON:
--
--   > {"units":     [{"page": "main_world", "id": 1, "tags": ["a", "b"]}],
--   >  "buildings": [...], "locations": [...],
--   >  "items":     [{"id": 42, "tags": ["c"]}]}
--
--   Items (#2703) carry no page: their assignments are session-scoped
--   and keyed by item instance id, which must not be 0. An empty @tags@
--   list clears the target. Each category is optional.
runSeedTags ∷ [String] → IO ()
runSeedTags args = do
    (fixturePath, rest0) ← orUsageFailure (requireFlag "--fixture" args)
    (specPath, rest1)    ← orUsageFailure (requireFlag "--spec" rest0)
    (outputPath, rest2)  ← orUsageFailure (requireFlag "--output" rest1)
    unless (null rest2) (orUsageFailure (Left ("unexpected arguments: "
                                               ⧺ unwords rest2)))
    specBytes ← BSL.readFile specPath
    seed ← case Aeson.eitherDecode specBytes of
        Left e  → failWith "SEED_FAILED" specPath ("spec: " ⧺ e)
        Right v → pure v
    bytes ← BS.readFile fixturePath
    luaNames ← case luaComponentNamesIn bytes of
        Left e  → failWith "SEED_FAILED" fixturePath ("envelope: " ⧺ e)
        Right n → pure n
    case decodeSessionEnvelope luaNames HS.empty bytes of
        Left err → failWith "SEED_FAILED" fixturePath ("decode: " ⧺ T.unpack err)
        Right (_, _, _, True) → failWith "SEED_FAILED" fixturePath
            "a migrated legacy baseline cannot be re-encoded as a seeded save"
        Right (meta, snap, luaComponents, False) →
            case applyTagSeed seed snap of
                Left e → failWith "SEED_FAILED" fixturePath e
                Right snap' → do
                    BS.writeFile outputPath
                        (encodeSessionSnapshot meta snap' luaComponents)
                    putStrLn "SEED_OK"

-- | One target's complete tag set.
data SeedEntry = SeedEntry
    { sePage ∷ !Text, seId ∷ !Int, seTags ∷ ![Text] }

instance Aeson.FromJSON SeedEntry where
    parseJSON = Aeson.withObject "seed entry" $ \o →
        SeedEntry <$> o .: "page" <*> o .: "id" <*> o .: "tags"

-- | One item's complete tag set (#2703): session-scoped, so no page.
--   An entry may also carry the item's scenario reference (#2709): a
--   @"ref"@ sets it (the empty string clears it), and an entry that
--   names only a @"ref"@ leaves the item's tags alone.
data ItemSeedEntry = ItemSeedEntry
    { iseId ∷ !Word64, iseTags ∷ !(Maybe [Text]), iseRef ∷ !(Maybe Text) }

instance Aeson.FromJSON ItemSeedEntry where
    parseJSON = Aeson.withObject "item seed entry" $ \o →
        ItemSeedEntry <$> o .: "id" <*> o .:? "tags" <*> o .:? "ref"

-- | One page-local cell, occurrence or plot seed (#2705). Cells and
--   plots name a canonical tile (@z@ only for a cell); an occurrence
--   names its 'floraInstanceIdToLua' id and the tile it stands on.
data PlaceSeedEntry = PlaceSeedEntry
    { pseePage ∷ !Text, pseeX ∷ !Int, pseeY ∷ !Int, pseeZ ∷ !(Maybe Int)
    , pseeId ∷ !(Maybe Int64), pseeTags ∷ ![Text] }

instance Aeson.FromJSON PlaceSeedEntry where
    parseJSON = Aeson.withObject "page seed entry" $ \o → PlaceSeedEntry
        <$> o .: "page" <*> o .: "x" <*> o .: "y" <*> o .:? "z" <*> o .:? "id"
        <*> o .: "tags"

data TagSeed = TagSeed
    { tsUnitSeeds ∷ ![SeedEntry], tsBuildingSeeds ∷ ![SeedEntry]
    , tsLocationSeeds ∷ ![SeedEntry], tsItemSeeds ∷ ![ItemSeedEntry]
    , tsTileSeeds ∷ ![PlaceSeedEntry], tsPlantSeeds ∷ ![PlaceSeedEntry]
    , tsPlotSeeds ∷ ![PlaceSeedEntry] }

instance Aeson.FromJSON TagSeed where
    parseJSON = Aeson.withObject "tag seed" $ \o → TagSeed
        <$> o .:? "units" .!= [] <*> o .:? "buildings" .!= []
        <*> o .:? "locations" .!= [] <*> o .:? "items" .!= []
        <*> o .:? "tiles" .!= [] <*> o .:? "plants" .!= [] <*> o .:? "plots" .!= []

applyTagSeed ∷ TagSeed → SessionSnapshot → Either String SessionSnapshot
applyTagSeed seed snap0 =
    foldM (entry onUnit) snap0 (tsUnitSeeds seed)
        ≫= \s1 → foldM (entry onBuilding) s1 (tsBuildingSeeds seed)
        ≫= \s2 → foldM (entry onLocation) s2 (tsLocationSeeds seed)
        ≫= \s3 → foldM itemEntry s3 (tsItemSeeds seed)
        ≫= \s4 → foldM (placeEntry onTile) s4 (tsTileSeeds seed)
        ≫= \s5 → foldM (placeEntry onPlant) s5 (tsPlantSeeds seed)
        ≫= \s6 → foldM (placeEntry onPlot) s6 (tsPlotSeeds seed)
  where
    placeEntry f snap e = do
        tags ← maybe (Left ("empty tag for " ⧺ T.unpack (pseePage e)))
                     (Right . Set.fromList) (traverse mkGameplayTag (pseeTags e))
        let pid = WorldPageId (pseePage e)
        page ← maybe (Left ("no page '" ⧺ T.unpack (pseePage e) ⧺ "' in the save"))
                     Right (HM.lookup pid (snapPages snap))
        page' ← f e tags page
        pure snap { snapPages = HM.insert pid page' (snapPages snap) }
    withPageTags page g = page { pgsPageTags = g (pgsPageTags page) }
    onTile e ts page = case pseeZ e of
        Nothing → Left "a tile seed needs z"
        Just z → Right $ withPageTags page $ \pt →
            pt { ptsTiles = setOrClear (pseeX e, pseeY e, z) ts (ptsTiles pt) }
    onPlant e ts page = case pseeId e ⌦ floraInstanceIdFromLua of
        Nothing → Left "a plant seed needs a real flora occurrence id"
        Just fid → Right $ withPageTags page $ \pt →
            pt { ptsOccurrences = setOrClear fid ts (ptsOccurrences pt)
               , ptsOccurrenceTiles =
                   if Set.null ts then Map.delete fid (ptsOccurrenceTiles pt)
                   else Map.insert fid (pseeX e, pseeY e) (ptsOccurrenceTiles pt) }
    onPlot e ts page = Right $ withPageTags page $ \pt →
        pt { ptsPlots = setOrClear (pseeX e, pseeY e) ts (ptsPlots pt) }
    itemEntry snap e = do
        t ← maybe (Left "item instance id 0 names no item") Right
                  (mkItemTarget (iseId e))
        withTags ← case iseTags e of
            Nothing → pure snap
            Just names → do
                tags ← maybe (Left ("empty tag for item " ⧺ show (iseId e)))
                             (Right . Set.fromList) (traverse mkGameplayTag names)
                pure snap { snapItemTags = setOrClear t tags (snapItemTags snap) }
        pure $ case iseRef e of
            Nothing → withTags
            Just r → withTags { snapItemRefs = setRef (iseId e) r (snapItemRefs withTags) }
    entry f snap e = do
        tags ← maybe (Left ("empty tag for " ⧺ T.unpack (sePage e) ⧺ "/"
                            ⧺ show (seId e)))
                     (Right . Set.fromList) (traverse mkGameplayTag (seTags e))
        let pid = WorldPageId (sePage e)
        page ← maybe (Left ("no page '" ⧺ T.unpack (sePage e) ⧺ "' in the save"))
                     Right (HM.lookup pid (snapPages snap))
        pure snap { snapPages = HM.insert pid (f (seId e) tags page) (snapPages snap) }
    setRef i r refs
        | T.null r = restrictItemRefs (≢ i) refs
        | otherwise = itemRefsFromList (itemRefsToAscList (restrictItemRefs (≢ i) refs) ⧺ [(i, r)])
    setOrClear k ts m | Set.null ts = Map.delete k m
                      | otherwise   = Map.insert k ts m
    onUnit i ts page = page { pgsUnits = (pgsUnits page)
        { usnTags = setOrClear (UnitId (fromIntegral i)) ts (usnTags (pgsUnits page)) } }
    onBuilding i ts page = page { pgsBuildings = (pgsBuildings page)
        { bsnTags = setOrClear (BuildingId (fromIntegral i)) ts
                               (bsnTags (pgsBuildings page)) } }
    onLocation i ts page =
        let params = pgsGenParams page
            lis    = wgpLocationInstances params
        in page { pgsGenParams = params { wgpLocationInstances = lis
                { lisTags = setCategoryTags (LocationInstanceId i) ts (lisTags lis) } } }

-- | Every @lua.\<name\>@ component the envelope carries, by bare name,
--   found by asking the real envelope decoder: each Lua component it
--   reports as an unknown REQUIRED id is added to the known set and the
--   decode retried, so the answer is the manifest's own, validated.
luaComponentNamesIn ∷ BS.ByteString → Either String (HS.HashSet Text)
luaComponentNamesIn bytes = go (64 ∷ Int) structuralKnownIds
  where
    go 0 _ = Left "too many unknown Lua components"
    go n known = case decodeEnvelope defaultEnvelopeLimits currentEnvelopeVersion
                                     known HS.empty bytes of
        Left (UnknownRequiredComponent cid@(ComponentId t))
            | luaComponentPrefix `T.isPrefixOf` t → go (n - 1) (HS.insert cid known)
        Left e → Left (show e)
        Right decoded → Right $ HS.fromList
            [ name | d ← emComponents (deManifest decoded)
                   , Just name ← [T.stripPrefix luaComponentPrefix
                                                (componentIdText (cdId d))] ]
