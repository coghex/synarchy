{-# LANGUAGE OverloadedStrings #-}
-- | Tag assignments in the tracked save-compat fixtures name targets
--   that exist (#2894), and the v5 world-edits shape the corpus keeps
--   them in can be written at all.
--
--   @synarchy-save-codec seed-tags@ writes a page's terrain-cell and
--   flora-occurrence tags through the codec, which checks neither: a
--   save carries no chunk, so a cell with no stored terrain and an
--   occurrence the chunk does not hold both encode cleanly. A load
--   discards them, though — 'reconcileAdmittedPageTags' runs on every
--   admission and is the intended stale-reference cleanup — so a fixture
--   shipping such a target describes a page the engine cannot restore,
--   and its canonical summary then disagrees with every real load. That
--   is how z11–z13 shipped three cells and two occurrence ids that the
--   seed-42 page never held.
--
--   'tagTargetsSpec' closes that for every manifest-declared complete-
--   session fixture, whoever seeded it: the fixture goes through the
--   REAL load staging ('stageSession' — chunk generation against the
--   shipped materials and flora, edit replay, flora admission and the
--   reconciliation itself), and the tags the staged page holds must be
--   exactly the tags the fixture decodes to. It also asserts that the
--   chunk owning every target is resident in the staged page.
--   Reconciliation leaves an assignment in an absent chunk untouched
--   ('Gameplay.Tags.Page.reconcileAdmittedChunks'), so without that a
--   target in a chunk this staging never builds would pass vacuously.
--
--   The codec's ability to seed a dangling target stays: negative probes
--   (@tools\/gameplay_tag_persistence_probe.py@) use it on live saves.
--   This group is about the TRACKED corpus only.
--
--   'worldEditsDowngradeSpec' pins the one operation regenerating
--   z11–z13 needs that no save path does: writing a world-edits payload
--   in its frozen v5 shape ('downgradeWorldEditsV5'), which is what
--   keeps those fixtures the corpus's historical v5 payloads that carry
--   cell and occurrence assignments.
--
--   Run just this gate: @cabal test synarchy-test-headless
--   --test-options='--match "save migrations"'@.
module Test.Headless.World.Save.Compat.TagTargets
    ( tagTargetsSpec
    , withShippedContentEngine
    , worldEditsDowngradeSpec
    ) where

import UPrelude
import Test.Hspec
import qualified Data.Aeson as Aeson
import Data.Aeson ((.:), (.:?), (.!=))
import qualified Data.ByteString as BS
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import qualified Data.List as L
import qualified Data.Map.Strict as Map
import qualified Data.Serialize as S
import qualified Data.Text as T
import Data.IORef (newIORef, readIORef)
import System.Directory (listDirectory)
import System.FilePath (takeExtension)

import Engine.Core.Init (EngineInitResult(..))
import Engine.Core.State (EngineEnv(..))
import Engine.Core.Thread (ThreadControl(..))
import Engine.Scripting.Lua.API (registerLuaAPI)
import Engine.Scripting.Lua.Thread (createLuaBackendState)
import Engine.Scripting.Lua.Thread.Console (executeDebugLua)
import Engine.Scripting.Lua.Types (LuaBackendState(..))
import Gameplay.Tags.Page
    (PageTagsSnapshot(..), RestoredPageTags(..), reconcileAdmittedChunks, restorePageTags)
import Gameplay.Tags.Types (tileTargetX, tileTargetY, tileTargetZ)
import Test.Headless.Harness.Isolation (withIsolatedResourceRoot)
import Test.Headless.Harness.Log (initializeEngineHeadlessQuiet)
import World.Chunk.Residency (canonicalChunkCoord)
import World.Chunk.Types (ChunkCoord(..), LoadedChunk(..))
import World.Construct.Apply (applyConstructSlopes)
import World.Edit.Apply (replayEdits)
import World.Edit.Types (WorldEdit(..), appendEdit, canonicalizeWorldEdits)
import World.Generate.Arena (clipArenaChunk)
import World.Generate.Types (WorldGenParams(..))
import World.Mine.Apply (applyDigSlopes)
import Gameplay.Tags.Tag (mkGameplayTag)
import qualified Data.Set as Set
import World.Material (MaterialRegistry)
import World.Save.Types (renderMissingFloraRef, resolveFloraReferences)
import World.Thread.ChunkLoading (generateDemandedChunks)
import World.Fluid.Types (FluidType(..))
import World.Flora.Identity (FloraInstanceId, floraInstanceIdToLua, plantedFloraInstanceId)
import World.Flora.Reference (FloraRef(..))
import World.Flora.Types (FloraCatalog, FloraId(..))
import World.Generate.Coordinates (globalToChunk)
import World.Material.Id (MaterialId(..))
import World.Page.Types (WorldPageId(..))
import World.Save.Component.PageEdits
    ( PageEditsDTO(..), PageEditsDTOv4(..), PageEditsDTOv5(..)
    , WorldEditDTO(..), WorldEditDTOv4(..), WorldEditsDTO(..)
    , WorldEditsDTOv4(..), WorldEditsDTOv5(..)
    , downgradeWorldEditDTOv4, downgradeWorldEditsV5
    , migrateWorldEditsV5, worldEditsCodec )
import World.Save.Component.Types (ComponentCodec(..))
import World.Save.Envelope (decodeSessionEnvelope)
import World.Save.Snapshot (PageSnapshot(..), SessionSnapshot(..))

-- * The shipped content

-- | A private headless engine with the shipped @data\/materials@ and
--   @data\/flora@ loaded through the real @engine.load*Yaml@ bindings, in
--   the order @scripts\/startup_loader.lua@ uses (materials first: a
--   species' soils resolve against the registry at registration), and
--   the isolated resource root every boot of the engine needs
--   (@Test.Headless.World.FloraOrder@ records why on both counts). The
--   chunk a fixture's page regenerates from its seed depends on exactly
--   this content, so a fixture target is only meaningful against it.
withShippedContentEngine ∷ (EngineEnv → IO α) → IO α
withShippedContentEngine act = withIsolatedResourceRoot $ do
    EngineInitResult env ← initializeEngineHeadlessQuiet
    ls ← createLuaBackendState (luaToEngineQueue env) (luaQueue env)
                               (assetPoolRef env) (nextObjectIdRef env)
                               (inputStateRef env) (loggerRef env)
    stateRef ← newIORef ThreadRunning
    registerLuaAPI (lbsLuaState ls) env ls stateRef
    loadFamily ls "data/materials" "loadMaterialYaml"
    loadFamily ls "data/flora"     "loadFloraYaml"
    act env
  where
    loadFamily ls dir verb = do
        files ← L.sort ∘ filter ((≡ ".yaml") ∘ takeExtension) <$> listDirectory dir
        forM_ files $ \f → do
            _ ← executeDebugLua (lbsLuaState ls)
                    ("return engine." <> verb <> "('"
                     <> T.pack dir <> "/" <> T.pack f <> "')")
            pure ()

-- * Admission

-- | Every complete-session fixture the manifest declares.
newtype FixturePaths = FixturePaths [FilePath]

instance Aeson.FromJSON FixturePaths where
    parseJSON = Aeson.withObject "manifest" $ \o → do
        baselines ← o .: "baselines"
        paths ← forM (baselines ∷ [Aeson.Value]) $ Aeson.withObject "baseline" $ \b → do
            fixtures ← b .:? "fixtures" .!= []
            forM (fixtures ∷ [Aeson.Value]) $ Aeson.withObject "fixture" $ \f → do
                kind ← f .: "kind"
                path ← f .: "path"
                pure [ path | kind ≡ ("complete-session" ∷ Text) ]
        pure (FixturePaths (concat (concat paths)))

trackedFixturePaths ∷ IO [FilePath]
trackedFixturePaths = do
    parsed ← Aeson.eitherDecodeFileStrict' "docs/save_compat/manifest.json"
    case parsed of
        Left err → fail ("manifest: " <> err)
        Right (FixturePaths ps) → pure ps

decodeFixture ∷ FilePath → IO SessionSnapshot
decodeFixture path = do
    bytes ← BS.readFile path
    let names = HS.fromList ["unit_ai", "building_spawn"]
    either (\e → fail (path <> ": " <> T.unpack e)) (\(_, s, _, _) → pure s)
           (decodeSessionEnvelope names names bytes)

-- | A page's cell and occurrence assignments, the part this group owns.
hasCellOrOccurrenceTags ∷ PageTagsSnapshot → Bool
hasCellOrOccurrenceTags pt =
    not (Map.null (ptsTiles pt)) ∨ not (Map.null (ptsOccurrences pt))

-- | The canonical tile every cell and occurrence assignment names.
targetTiles ∷ PageTagsSnapshot → [(Int, Int)]
targetTiles pt = L.nub $
    [ (x, y) | (x, y, _) ← Map.keys (ptsTiles pt) ]
    ⧺ Map.elems (ptsOccurrenceTiles pt)

-- | What the admission of every chunk that owns a target does to a page's
--   assignments, as readable lines (none when every target survives).
--
--   The owning chunks are RECONSTRUCTED from the page the fixture holds,
--   by the production steps in the order a load applies them: the
--   generator ('generateDemandedChunks', which is also the arena's own
--   authority), the page's flora references resolved against the shipped
--   catalog and its edit log put in canonical frame and replayed
--   ('replayEdits'), the passes the load stage then applies to a replayed
--   chunk (an arena's clip to its bounds, 'applyDigSlopes' and
--   'applyConstructSlopes' over the page's designations — each can remove
--   a cell or a rooted plant), and the restored assignments reconciled
--   against the result ('reconcileAdmittedChunks', the pure core of
--   'reconcileAdmittedPageTags'). Reconstructing them here, rather than
--   reading whatever a particular load happens to make resident, is what
--   puts a target in ANY chunk under test: a load builds the camera
--   chunk and its neighbours and leaves every other assignment untouched
--   until its chunk is admitted, and an absent chunk reconciles nothing.
admissionProblems ∷ MaterialRegistry → FloraCatalog → WorldPageId → PageSnapshot
                  → [Text]
admissionProblems registry catalog pid page = case resolved of
    Left errs → [ "its flora references cannot be resolved: "
                  <> T.intercalate "; " (map renderMissingFloraRef errs) ]
    Right (edits, _, _, _) →
        let canon = canonicalChunkCoord params
            admitted = [ applyConstructSlopes (pgsConstructDesignations page)
                           ( applyDigSlopes (pgsMineDesignations page)
                           ( clipArenaChunk (wgpArenaBounds params)
                           ( replayEdits (canonicalizeWorldEdits canon edits) lc)))
                       | lc ← generated ]
            restored = rptPageTags (restorePageTags pid HM.empty want)
            (_, deadCells, deadOccs) = reconcileAdmittedChunks admitted restored
        in [ "chunk " <> tshow c <> " owns a target but is never generated"
           | c ← owners, c ∉ map lcCoord generated ]
           ⧺ [ "cell " <> tshow (tileTargetX t, tileTargetY t, tileTargetZ t)
               <> " is not a stored non-air cell once its chunk is admitted"
             | t ← deadCells ]
           ⧺ [ "occurrence " <> tshow (floraInstanceIdToLua o)
               <> " is not held by its chunk once it is admitted"
             | o ← deadOccs ]
  where
    params = pgsGenParams page
    want = pgsPageTags page
    owners = L.nub [ canonicalChunkCoord params (fst (globalToChunk x y))
                   | (x, y) ← targetTiles want ]
    generated = generateDemandedChunks registry catalog pid params owners
    resolved = resolveFloraReferences catalog pid (pgsEdits page) HM.empty
                                      HM.empty HM.empty

tagTargetsSpec ∷ SpecWith EngineEnv
tagTargetsSpec = describe "tracked fixtures' tag targets survive admission (#2894)" $ do
    paths ← runIO trackedFixturePaths

    it "the corpus really exercises a nonempty cell and occurrence round trip" $ \_ → do
        pages ← concatMap (HM.elems . snapPages) <$> mapM decodeFixture paths
        let pts = map pgsPageTags pages
        any (not . Map.null . ptsTiles) pts `shouldBe` True
        any (not . Map.null . ptsOccurrences) pts `shouldBe` True

    -- The gate must be able to fail. Each case takes a TRACKED fixture's
    -- page and seeds one target admission removes, the way a careless
    -- codec seed would.
    describe "refuses a fixture whose seeded target admission removes" $ do
        let seeded name path mutate expected = it name $ \env → do
                snap ← decodeFixture path
                registry ← readIORef (materialRegistryRef env)
                catalog ← readIORef (floraCatalogRef env)
                (pid, page) ← case HM.toList (snapPages snap) of
                    [(p, pg)] → pure (p, mutate pg)
                    _ → fail (path <> ": expected exactly one page")
                admissionProblems registry catalog pid page
                    `shouldSatisfy` any (expected `T.isInfixOf`)
            tagged = Set.fromList (maybe [] pure (mkGameplayTag "probe"))
            withCell cell pg = pg { pgsPageTags = (pgsPageTags pg)
                { ptsTiles = Map.insert cell tagged (ptsTiles (pgsPageTags pg)) } }
            -- an occurrence tag, with a planting edit placing it when asked
            withPlant planting fid tile pg =
                let pt = pgsPageTags pg
                    withTag = pg { pgsPageTags = pt
                        { ptsOccurrences = Map.insert fid tagged (ptsOccurrences pt)
                        , ptsOccurrenceTiles = Map.insert fid tile (ptsOccurrenceTiles pt) } }
                in if planting
                   then withTag { pgsEdits = appendEdit
                            (fst (globalToChunk (fst tile) (snd tile)))
                            (WePlaceFloraRef (fst tile) (snd tile) (FloraByName "tomato_plant")
                                             0 12 fid)
                            (pgsEdits withTag)
                        , pgsPlantedFloraCursor = 100 }
                   else withTag
        seeded "a cell above the terrain" "test-headless/data/save-compat/z10-item-tags.bin"
            (withCell (3, 4, 10)) "cell (3,4,10)"
        seeded "an occurrence no chunk generates" "test-headless/data/save-compat/z10-item-tags.bin"
            (withPlant False (plantedFloraInstanceId 99) (6, 3)) "occurrence"
        -- (10, 0) lies outside z17's 20x10 arena but inside the chunk that
        -- intersects it: replay places the plant, the load's arena clip
        -- removes it
        seeded "a planting outside an arena's bounds" "test-headless/data/save-compat/z17-scenario-placement.bin"
            (withPlant True (plantedFloraInstanceId 99) (10, 0)) "occurrence"

    forM_ paths $ \path →
        it (path <> " keeps every cell and occurrence assignment it carries") $ \env → do
            snap ← decodeFixture path
            registry ← readIORef (materialRegistryRef env)
            catalog ← readIORef (floraCatalogRef env)
            let problems =
                    [ unWorldPageId pid <> ": " <> line
                    | (pid, page) ← HM.toList (snapPages snap)
                    , hasCellOrOccurrenceTags (pgsPageTags page)
                    , line ← admissionProblems registry catalog pid page ]
            problems `shouldBe` []

-- * The v5 shape

-- | One edit of every constructor of the frozen v4\/v5 sum, in
--   declaration order.
frozenEdits ∷ [WorldEditDTOv4]
frozenEdits =
    [ WeDeleteTileDv4 1 2
    , WeSetFluidTileDv4 1 2 Lake
    , WeAddTileDv4 1 2 (MaterialId 3)
    , WeSetSlopeDv4 1 2 3 4
    , WeSetCellDv4 1 2 3 (MaterialId 4)
    , WeSetStructureDv4 1 2 3 4 5 6
    , WeClearStructureDv4 1 2 3
    , WeSetVegDv4 1 2 3 4
    , WePlaceFloraDv4 1 2 (FloraId 3) 4 5.5
    , WeSetFluidSnapshotDv4 1 2 Lava 3
    , WeClearFluidSnapshotDv4 1 2
    , WePlaceFloraWithIdDv4 1 2 (FloraId 3) 4 5.5 planted
    , WePlaceFloraRefDv4 1 2 (FloraByName "oak") 4 5.5 planted
    ]

-- | Declaration index of a frozen constructor. Exhaustive on purpose: a
--   constructor added to the frozen sum stops this compiling instead of
--   silently falling outside 'frozenEdits'.
frozenIndex ∷ WorldEditDTOv4 → Int
frozenIndex e = case e of
    WeDeleteTileDv4 {} → 0
    WeSetFluidTileDv4 {} → 1
    WeAddTileDv4 {} → 2
    WeSetSlopeDv4 {} → 3
    WeSetCellDv4 {} → 4
    WeSetStructureDv4 {} → 5
    WeClearStructureDv4 {} → 6
    WeSetVegDv4 {} → 7
    WePlaceFloraDv4 {} → 8
    WeSetFluidSnapshotDv4 {} → 9
    WeClearFluidSnapshotDv4 {} → 10
    WePlaceFloraWithIdDv4 {} → 11
    WePlaceFloraRefDv4 {} → 12

planted ∷ FloraInstanceId
planted = plantedFloraInstanceId 7

pageP ∷ WorldPageId
pageP = WorldPageId "downgrade_page"

-- | A v5 payload whose page holds every frozen edit and nonempty tags.
sampleV5 ∷ WorldEditsDTOv5
sampleV5 = WorldEditsDTOv5
    [ PageEditsDTOv5
        { ped5PageId = pageP
        , ped5Edits = HM.fromList [(ChunkCoord 0 0, frozenEdits)]
        , ped5PlantedFloraCursor = 8
        , ped5TileTags = [((3, 4, 10), ["quarry", "shared"]), ((3, 4, 8), ["deep"])]
        , ped5OccurrenceTags = [(planted, ["sown"])]
        , ped5OccurrenceTiles = [(planted, (6, 3))]
        , ped5PlotTags = [((1, 1), ["plot"])]
        } ]

worldEditsDowngradeSpec ∷ Spec
worldEditsDowngradeSpec = describe "world-edits v5 downgrade (#2894)" $ do
    it "the sample holds one edit of every frozen constructor" $
        map frozenIndex frozenEdits `shouldBe` [0 .. 12]

    it "undoes the v5 migration for every frozen edit constructor" $
        forM_ frozenEdits $ \e → downgradeOne e `shouldBe` Just e

    it "writes bytes the v5 reader accepts and migrates to the same payload" $ do
        let live = migrateWorldEditsV5 sampleV5
        v5 ← maybe (expectationFailure "no v5 spelling" >> fail "unreachable") pure
                   (downgradeWorldEditsV5 live)
        S.encode v5 `shouldBe` S.encode sampleV5
        case ccDecode worldEditsCodec 5 (S.encode v5) of
            Left e → expectationFailure (show e)
            Right back → S.encode back `shouldBe` S.encode live

    it "refuses a payload carrying an edit no v5 payload can" $ do
        let withState = WorldEditsDTO
                [ page { pedEdits = fmap (⧺ [WeSetFloraStateD 1 2 planted 3 0 0 0 1 1])
                                         (pedEdits page) }
                | page ← wedPages (migrateWorldEditsV5 sampleV5) ]
        isNothing (downgradeWorldEditsV5 withState) `shouldBe` True
        isNothing (downgradeWorldEditDTOv4 (WeSetFloraStateD 1 2 planted 3 0 0 0 1 1))
            `shouldBe` True

    it "keeps a v4 payload v4: it still migrates through v5" $ do
        let v4 = WorldEditsDTOv4 [PageEditsDTOv4 pageP HM.empty 1]
        case ccDecode worldEditsCodec 4 (S.encode v4) of
            Left e → expectationFailure (show e)
            Right (WorldEditsDTO slices) → map pedPageId slices `shouldBe` [pageP]
  where
    downgradeOne e =
        let single = WorldEditsDTOv5
                [PageEditsDTOv5 pageP (HM.singleton (ChunkCoord 0 0) [e]) 1 [] [] [] []]
        in case downgradeWorldEditsV5 (migrateWorldEditsV5 single) of
            Just (WorldEditsDTOv5 [s]) → HM.lookup (ChunkCoord 0 0) (ped5Edits s) ⌦ listToMaybe
            _ → Nothing
