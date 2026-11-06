{-# LANGUAGE OverloadedStrings #-}
-- | Synthetic current-save tag admission assertions. This module does not
-- read the retired historical fixture corpus.
module Test.Headless.World.Save.Compat.TagTargets
    ( currentTagAdmissionSpec
    , withShippedContentEngine
    ) where

import UPrelude
import Test.Hspec
import qualified Data.HashMap.Strict as HM
import qualified Data.List as L
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.Vector.Unboxed as VU
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
import Gameplay.Tags.Page (PageTagsSnapshot(..), RestoredPageTags(..), reconcileAdmittedChunks, restorePageTags)
import Gameplay.Tags.Tag (mkGameplayTag)
import Gameplay.Tags.Types (tileTargetX, tileTargetY, tileTargetZ)
import Test.Headless.Harness.Isolation (withIsolatedResourceRoot)
import Test.Headless.Harness.Log (initializeEngineHeadlessQuiet)
import Test.Headless.World.Save.Components.Fixture (minimalPage)
import World.Arena.Bounds (mkArenaBounds)
import World.Chunk.Residency (canonicalChunkCoord)
import World.Chunk.Types (LoadedChunk(..), columnIndex)
import World.Construct.Apply (applyConstructSlopes)
import World.Edit.Apply (replayEdits)
import World.Edit.Types (WorldEdit(..), appendEdit, canonicalizeWorldEdits)
import World.Flora.Identity (floraInstanceIdToLua, plantedFloraInstanceId)
import World.Flora.Reference (FloraRef(..))
import World.Flora.Types (FloraCatalog)
import World.Generate.Arena (clipArenaChunk)
import World.Generate.Coordinates (globalToChunk)
import World.Generate.Types (WorldGenParams(..))
import World.Material (MaterialRegistry)
import World.Mine.Apply (applyDigSlopes)
import World.Page.Types (WorldPageId(..))
import World.Save.Snapshot (PageSnapshot(..))
import World.Save.Types (renderMissingFloraRef, resolveFloraReferences)
import World.Thread.ChunkLoading (generateDemandedChunks)

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


currentTagAdmissionSpec ∷ SpecWith EngineEnv
currentTagAdmissionSpec = describe "current tag admission" $ do
    it "keeps nonempty cell and occurrence assignments through staging" $ \env → do
        registry ← readIORef (materialRegistryRef env)
        catalog ← readIORef (floraCatalogRef env)
        let pid = WorldPageId "current-tag-admission"
            params = (pgsGenParams (minimalPage pid))
                { wgpSeed = 0, wgpWorldSize = 100000, wgpArenaBounds = Nothing }
            base = (minimalPage pid) { pgsGenParams = params }
            x = 5
            y = 5
            chunk = canonicalChunkCoord params (fst (globalToChunk x y))
            generated = generateDemandedChunks registry catalog pid params [chunk]
            z = case filter ((≡ chunk) ∘ lcCoord) generated of
                g : _ → lcTerrainSurfaceMap g VU.! columnIndex 5 5
                [] → error "current tag staging fixture generated no owning chunk"
            tag = Set.fromList (maybe [] pure (mkGameplayTag "current"))
            fid = plantedFloraInstanceId 99
            tagged = base
                { pgsPageTags = (pgsPageTags base)
                    { ptsTiles = Map.singleton (x, y, z) tag
                    , ptsOccurrences = Map.singleton fid tag
                    , ptsOccurrenceTiles = Map.singleton fid (6, 3) }
                , pgsEdits = appendEdit (fst (globalToChunk 6 3))
                    (WePlaceFloraRef 6 3 (FloraByName "tomato_plant")
                        0 12 fid) (pgsEdits base)
                , pgsPlantedFloraCursor = 100
                }
        hasCellOrOccurrenceTags (pgsPageTags tagged) `shouldBe` True
        admissionProblems registry catalog pid tagged `shouldBe` []

    it "detects invalid cells, absent occurrences, and arena plantings out of bounds" $ \env → do
        registry ← readIORef (materialRegistryRef env)
        catalog ← readIORef (floraCatalogRef env)
        let pid = WorldPageId "current-tag-admission-invalid"
            params = (pgsGenParams (minimalPage pid))
                { wgpSeed = 0, wgpWorldSize = 100000, wgpArenaBounds = Nothing }
            base = (minimalPage pid) { pgsGenParams = params }
            x = 5
            y = 5
            chunk = canonicalChunkCoord params (fst (globalToChunk x y))
            generated = generateDemandedChunks registry catalog pid params [chunk]
            z = case filter ((≡ chunk) ∘ lcCoord) generated of
                g : _ → lcTerrainSurfaceMap g VU.! columnIndex 5 5
                [] → error "current invalid tag fixture generated no owning chunk"
            tag = Set.fromList (maybe [] pure (mkGameplayTag "current"))
            absent = plantedFloraInstanceId 98
            bad = base { pgsPageTags = (pgsPageTags base)
                { ptsTiles = Map.singleton (x, y, z + 100) tag
                , ptsOccurrences = Map.singleton absent tag
                , ptsOccurrenceTiles = Map.singleton absent (6, 3) } }
            problems = admissionProblems registry catalog pid bad
            bounds = either (error ∘ show) id (mkArenaBounds 20 10)
            arenaParams = params
                { wgpSeed = 0, wgpWorldSize = 100000
                , wgpArenaBounds = Just bounds }
            arenaPage = base
                { pgsGenParams = arenaParams
                , pgsPageTags = (pgsPageTags base)
                    { ptsOccurrences = Map.singleton absent tag
                    , ptsOccurrenceTiles = Map.singleton absent (10, 0) }
                , pgsEdits = appendEdit (fst (globalToChunk 10 0))
                    (WePlaceFloraRef 10 0 (FloraByName "tomato_plant")
                        0 12 absent) (pgsEdits base)
                , pgsPlantedFloraCursor = 100
                }
            arenaPageWithoutBounds = arenaPage
                { pgsGenParams = arenaParams { wgpArenaBounds = Nothing } }
            unboundedProblems = admissionProblems registry catalog pid arenaPageWithoutBounds
            arenaProblems = admissionProblems registry catalog pid arenaPage
        problems `shouldSatisfy` any (T.isInfixOf "not a stored non-air cell")
        problems `shouldSatisfy` any (T.isInfixOf "not held by its chunk")
        unboundedProblems `shouldBe` []
        arenaProblems `shouldSatisfy` any (T.isInfixOf "not held by its chunk")
