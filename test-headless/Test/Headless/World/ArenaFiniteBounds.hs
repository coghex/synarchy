{-# LANGUAGE OverloadedStrings #-}
-- | "arena finite bounds" (#2708, SCN-08 of epic #2698).
--
--   An arena page may carry exact, centred, nonwrapping dimensions. They
--   are PAGE state: created with the page, enforced at every consumer of
--   terrain, persisted with the page, and restored without any scenario
--   file. This group covers the consumers, each driven through its
--   production entry point with expectations written as literals from the
--   issue's own formulae (20×10 is X −10…9, Y −4…5; 1×1 is only (0, 0)):
--
--   * the pure rectangle, dimension domain, tiles and positions, partial
--     chunks, clipping and footprint results ("…Pure");
--   * closed fluid edges, inside a chunk and at seams ("…Sim");
--   * units, the command handlers and the movement tick ("…Movement");
--   * and here, the live engine: startup, camera demand, explicit queued
--     demand, eviction and readmission, staged restoration, the real
--     component codec, authoritative mutations, building placement and
--     ground objects — on a finite page that is HIDDEN while another page
--     is on screen, so the page's own rectangle is what decides.
--
--   Run just this gate: @cabal test synarchy-test-headless
--   --test-options='--match "arena finite bounds"'@.
module Test.Headless.World.ArenaFiniteBounds (spec) where

import UPrelude
import Test.Hspec
import Control.Concurrent (threadDelay)
import Control.Exception (SomeException, finally, throwIO, try)
import Data.IORef (atomicModifyIORef', readIORef, writeIORef)
import qualified Data.ByteString as BS
import qualified Data.Text.Encoding as TE
import qualified HsLua as Lua
import Engine.Scripting.Lua.API.World.Lifecycle (worldInitArenaFn)
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import qualified Data.Text as T
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU
import Data.Either (isRight)
import Data.List (sort)
import GHC.Clock (getMonotonicTimeNSec)
import Building.Thread.Command (applyBuildingSpawn)
import Building.Placement
    (PlacementResult(..), canPlaceAt, footprintOutsideArenaReason)
import Building.Reservation (reserveFootprint)
import Building.Types
    ( BuildingDef(..), BuildingId(..), BuildingManager(..), emptyBuildingManager )
import Engine.Core.Capability.Building (toBuildingCapability)
import Engine.Core.Capability.ContentRegistriesView (toContentRegistriesViewCapability)
import Engine.Core.Capability.WorldSim (toWorldSimCapability)
import Engine.Core.Log (LoggerState, LogConfig(..), LogBackend(..), initLogger, defaultLogConfig)
import Engine.Core.State (EngineEnv(..))
import qualified Engine.Core.Queue as Q
import Engine.Graphics.Camera (Camera2D(..), CameraFacing(..))
import Item.Ground (GroundItems(..))
import Item.Knowledge (emptyPortableKnowledge)
import Location.Instance (emptyLocationInstances)
import Sim.Command.Types (SimCommand(..))
import Sim.Topology (SimTopology(..))
import Structure.Palette (emptyTexPalette)
import Test.Headless.Building.Placement (mkDef)
import Test.Headless.Harness
    ( sendWorldCommand, waitForChunksAt, waitForWorldInit, withHeadlessEngine )
import Test.Headless.Harness.GeneratedIds (fixtureGeneratedWorldIdForPage)
import qualified Test.Headless.World.ArenaFiniteBounds.Movement as Movement
import qualified Test.Headless.World.ArenaFiniteBounds.Pure as Pure
import qualified Test.Headless.World.ArenaFiniteBounds.Sim as Sim
import Test.Headless.Location.ContainerShells (containerItemDefs, crateItemDef)
import World.Arena.Bounds (ArenaBounds, mkArenaBounds)
import World.Chunk.Admit (pageIncarnation)
import World.Chunk.Queue (enqueueChunkRequest)
import World.Chunk.Residency (canonicalChunkCoord)
import World.Edit.Types (WorldEdit(..), WorldEdits, appendEdit, emptyWorldEdits)
import World.Generate.Arena (arenaFootprint, generateArenaChunk, generateArenaChunkIn)
import World.GroundItems (groundRestShift)
import World.Grid (gridToWorld)
import World.Load.Publish (publishStagedSession)
import World.Load.Stage (renderStageError, stageSession)
import World.Load.Types (StagedPage(..), StagedSession(..))
import World.Material (MaterialId(..), matGranite)
import World.Save.Component.PageCore
    ( ArenaBoundsDTO(..), PageCoreDTO(..), WorldPagesDTO(..)
    , basePageSnapshots, validatePages )
import World.Save.Component.Page (blankPageSnapshot)
import World.Save.Component.WorldGen (toWorldGenParamsDTO)
import World.Save.Component.Types (renderComponentError)
import World.Save.Envelope (decodeSessionEnvelope, encodeSessionSnapshot)
import World.Save.Snapshot (LiveCameraSnapshot(..), PageSnapshot(..), SessionSnapshot(..))
import World.Save.Snapshot.Adapter (SaveRequestMeta(..), snapshotSaveMetadata, snapshotToSaveData)
import World.Types
import Engine.Scripting.Lua.API.Items.Ground (spawnSalvageOnPage)

-- | The full group. Its engine half shares one headless engine; the
--   pure halves need none.
spec ∷ Spec
spec = do
    Pure.spec
    Sim.spec
    Movement.pureSpec
    aroundAll withHeadlessEngine $ do
        productionSpec
        Movement.engineSpec

bounds ∷ Int → Int → ArenaBounds
bounds w h = either (error . show) id (mkArenaBounds w h)

-- * Harness

waitUntil ∷ Int → IO Bool → IO Bool
waitUntil secs cond = go (secs * 20)
  where go k | k ≤ 0 = pure False
             | otherwise = cond ≫= \ok → if ok then pure True
                                         else threadDelay 50000 ≫ go (k - 1 ∷ Int)

uniquePage ∷ T.Text → IO WorldPageId
uniquePage tag = do
    n ← getMonotonicTimeNSec
    pure (WorldPageId (tag <> "_" <> T.pack (show n)))

-- | Point the camera at the middle of a chunk (camera positions are in
--   world space, converted with the camera's own facing).
goToChunk ∷ EngineEnv → ChunkCoord → IO ()
goToChunk env (ChunkCoord cx cy) = do
    facing ← camFacing ⊚ readIORef (cameraRef env)
    let (wx, wy) = gridToWorld facing (cx * 16 + 8) (cy * 16 + 8)
    atomicModifyIORef' (cameraRef env) $ \cam →
        (cam { camPosition = (wx, wy), camZoom = 0.5 }, ())

residentKeys ∷ WorldState → IO [ChunkCoord]
residentKeys ws = sort ∘ HM.keys ∘ wtdChunks ⊚ readIORef (wsTilesRef ws)

-- | A readable account of how two chunks differ, field by field and
--   column by column — an equality failure on a whole chunk prints
--   hundreds of columns and names none of them.
chunkDiff ∷ LoadedChunk → LoadedChunk → [String]
chunkDiff got want = concat
    [ [ "coord" | lcCoord got ≢ lcCoord want ]
    , [ "column " ⧺ show i ⧺ ": " ⧺ show (lcTiles got V.! i) ⧺ " /= " ⧺ show (lcTiles want V.! i)
      | i ← [0 .. min (V.length (lcTiles got)) (V.length (lcTiles want)) - 1]
      , lcTiles got V.! i ≢ lcTiles want V.! i ]
    , [ "surface map" | lcSurfaceMap got ≢ lcSurfaceMap want ]
    , [ "terrain surface map" | lcTerrainSurfaceMap got ≢ lcTerrainSurfaceMap want ]
    , [ "fluid map" | lcFluidMap got ≢ lcFluidMap want ]
    , [ "flora" | lcFlora got ≢ lcFlora want ]
    , [ "side deco at " ⧺ show [ i | i ← [0 .. 255], lcSideDeco got VU.! i ≢ lcSideDeco want VU.! i ]
      | lcSideDeco got ≢ lcSideDeco want ]
    , [ "water table" | lcWaterTableMap got ≢ lcWaterTableMap want ]
    , [ "structures" | lcStructures got ≢ lcStructures want ]
    ]

residentChunk ∷ WorldState → ChunkCoord → IO (Maybe LoadedChunk)
residentChunk ws c = HM.lookup c ∘ wtdChunks ⊚ readIORef (wsTilesRef ws)

awaitChunk ∷ WorldState → ChunkCoord → IO LoadedChunk
awaitChunk ws c = do
    ok ← waitForChunksAt ws c 60
    unless ok $ expectationFailure ("chunk never became resident: " ⧺ show c)
    residentChunk ws c ≫= maybe (expectationFailure "chunk vanished" ≫ error "unreachable") pure

-- | A finite arena page that is VISIBLE (the camera loader serves
--   visible pages). Hands back the sim seeds init published.
withVisibleFinite
    ∷ EngineEnv → Int → Int
    → (WorldPageId → WorldState → ArenaBounds → [SimCommand] → IO α) → IO α
withVisibleFinite env w h act = do
    pid ← uniquePage "finite_visible"
    let ab = bounds w h
    _ ← Q.flushQueue (simQueue env)
    sendWorldCommand env (WorldInitArena pid (Just ab))
    ws ← waitForWorldInit env pid 60
    seeds ← Q.flushQueue (simQueue env)
    sendWorldCommand env (WorldInitArenaDone pid)
    visible ← waitUntil 30 (elem pid ∘ wmVisible ⊚ readIORef (worldManagerRef env))
    unless visible $ expectationFailure "the finite arena never became visible"
    goToChunk env (ChunkCoord 0 0)
    act pid ws ab seeds `finally` do
        sendWorldCommand env (WorldDestroy pid)
        goToChunk env (ChunkCoord 0 0)

-- | A finite arena page that stays HIDDEN while an EXPANDABLE page is
--   shown.
withHiddenFinite ∷ EngineEnv → Int → Int → (WorldPageId → WorldState → ArenaBounds → IO α) → IO α
withHiddenFinite env w h act = do
    pid ← uniquePage "finite_hidden"
    other ← uniquePage "open_visible"
    let ab = bounds w h
    sendWorldCommand env (WorldInitArena pid (Just ab))
    ws ← waitForWorldInit env pid 60
    sendWorldCommand env (WorldInitArena other Nothing)
    _ ← waitForWorldInit env other 60
    sendWorldCommand env (WorldInitArenaDone other)
    shown ← waitUntil 30 (elem other ∘ wmVisible ⊚ readIORef (worldManagerRef env))
    unless shown $ expectationFailure "the other arena never became visible"
    act pid ws ab `finally` do
        sendWorldCommand env (WorldDestroy pid)
        sendWorldCommand env (WorldDestroy other)

-- | The chunk set a size starts with, as literals.
startupExpectation ∷ [((Int, Int), [(Int, Int)])]
startupExpectation =
    [ ((1, 1),    [(0, 0)])
    , ((5, 5),    [(-1, -1), (-1, 0), (0, -1), (0, 0)])
    , ((20, 10),  [(-1, -1), (-1, 0), (0, -1), (0, 0)])
    , ((21, 11),  [(-1, -1), (-1, 0), (0, -1), (0, 0)])
    , ((100, 100), [ (cx, cy) | cx ← [-2 .. 2], cy ← [-2 .. 2] ]) ]

productionSpec ∷ SpecWith EngineEnv
productionSpec = describe "through the production paths" $ do
    luaSpec
    startupSpec
    demandSpec
    evictionSpec
    restoreSpec
    codecSpec
    mutationSpec
    objectSpec

-- * The Lua entry

describeArg ∷ LuaArg → String
describeArg a = case a of
    ArgText t → "text " <> T.unpack t
    ArgNum n  → "num " <> show n
    ArgNil    → "nil"
    ArgBool b → "bool " <> show b

-- | One argument of a @world.initArena@ call.
data LuaArg = ArgText T.Text | ArgNum Double | ArgNil | ArgBool Bool

-- | Call the REAL binding @world.initArena@ with these arguments;
--   'Left' carries the Lua error it raised.
callInitArena ∷ EngineEnv → [LuaArg] → IO (Either SomeException ())
callInitArena env args = try $ Lua.run $ do
    Lua.openlibs
    Lua.pushHaskellFunction (worldInitArenaFn env)
    forM_ args $ \a → case a of
        ArgText t → Lua.pushstring (TE.encodeUtf8 t)
        ArgNum n  → Lua.pushnumber (Lua.Number n)
        ArgNil    → Lua.pushnil
        ArgBool b → Lua.pushboolean b
    -- a protected call, as a script's own call is: a raised Lua error
    -- comes back as a status, never as a crashed state
    st ← Lua.pcall (Lua.NumArgs (fromIntegral (length args))) (Lua.NumResults 0) Nothing
    when (st /= Lua.OK) $ Lua.liftIO (throwIO (userError "lua error"))

luaSpec ∷ SpecWith EngineEnv
luaSpec = describe "world.initArena" $ do

    it "creates a finite arena from two integer dimensions and an expandable one from none" $ \env → do
        pid ← uniquePage "lua_finite"
        open ← uniquePage "lua_open"
        callInitArena env [ArgText (unWorldPageId pid), ArgNum 20, ArgNum 10] ≫= (`shouldSatisfy` isRight)
        callInitArena env [ArgText (unWorldPageId open)] ≫= (`shouldSatisfy` isRight)
        ws ← waitForWorldInit env pid 60
        wo ← waitForWorldInit env open 60
        ((wgpArenaBounds =≪) ⊚ readIORef (wsGenParamsRef ws)) `shouldReturn` Just (bounds 20 10)
        ((wgpArenaBounds =≪) ⊚ readIORef (wsGenParamsRef wo)) `shouldReturn` Nothing
        sendWorldCommand env (WorldDestroy pid)
        sendWorldCommand env (WorldDestroy open)

    it "refuses a size outside the domain, half a size, a non-integer and a non-number, creating nothing" $ \env → do
        pid ← uniquePage "lua_refused"
        let name = ArgText (unWorldPageId pid)
            bad = [ [name, ArgNum 20]                       -- width alone
                  , [name, ArgNil, ArgNum 10]               -- height alone
                  , [name, ArgNum 0, ArgNum 5]
                  , [name, ArgNum 5, ArgNum (-1)]
                  , [name, ArgNum 100001, ArgNum 1]
                  , [name, ArgNum 1, ArgNum 100001]
                  , [name, ArgNum 1.5, ArgNum 3]
                  , [name, ArgNum (0 / 0), ArgNum 3]
                  , [name, ArgNum (1 / 0), ArgNum 3]
                  , [name, ArgText "20", ArgNum 10]
                  , [name, ArgBool True, ArgNum 10] ]
        forM_ bad $ \args → do
            r ← callInitArena env args
            (either (const True) (const False) r, length args) `shouldBe` (True, length args)
            unless (either (const True) (const False) r) $
                expectationFailure ("accepted: " <> show (map describeArg args))
        -- nothing was enqueued, so the page never appears
        threadDelay 800000
        mgr ← readIORef (worldManagerRef env)
        isNothing (lookup pid (wmWorlds mgr)) `shouldBe` True

-- * Startup

startupSpec ∷ SpecWith EngineEnv
startupSpec = describe "creation" $ do

    it "starts each size with exactly the chunks that intersect its rectangle, built by the authority" $ \env →
        forM_ startupExpectation $ \((w, h), coords) →
            withVisibleFinite env w h $ \_ ws ab _ → do
                residentKeys ws `shouldReturn` sort [ ChunkCoord x y | (x, y) ← coords ]
                tiles ← wtdChunks ⊚ readIORef (wsTilesRef ws)
                forM_ (HM.toList tiles) $ \(c, lc) →
                    lc `shouldBe` generateArenaChunkIn (Just ab) 0 c

    it "gives a 1×1 arena one resident chunk with exactly one playable column" $ \env →
        withVisibleFinite env 1 1 $ \_ ws _ _ → do
            residentKeys ws `shouldReturn` [ChunkCoord 0 0]
            Just lc ← residentChunk ws (ChunkCoord 0 0)
            [ i | (i, col) ← zip [0 ∷ Int ..] (V.toList (lcTiles lc))
                , VU.length (ctMats col) > 0 ] `shouldBe` [0]
            VU.length (ctMats (lcTiles lc V.! 0)) `shouldBe` 16

    it "records the bounds in the page's own params and tile data, and stays an arena" $ \env →
        withVisibleFinite env 20 10 $ \_ ws ab _ → do
            Just params ← readIORef (wsGenParamsRef ws)
            wgpArenaBounds params `shouldBe` Just ab
            isArenaParams params `shouldBe` True
            wgpWorldSize params `shouldBe` 100000
            canonicalChunkCoord params (ChunkCoord 7 (-7)) `shouldBe` ChunkCoord 7 (-7)
            wtdBounds ⊚ readIORef (wsTilesRef ws) `shouldReturn` Just ab

    it "keeps an expandable arena exactly as it was: no bounds anywhere, the whole footprint" $ \env → do
        pid ← uniquePage "open_arena"
        sendWorldCommand env (WorldInitArena pid Nothing)
        ws ← waitForWorldInit env pid 60
        Just params ← readIORef (wsGenParamsRef ws)
        wgpArenaBounds params `shouldBe` Nothing
        wtdBounds ⊚ readIORef (wsTilesRef ws) `shouldReturn` Nothing
        residentKeys ws `shouldReturn` sort arenaFootprint
        sendWorldCommand env (WorldDestroy pid)

    it "admits exactly the startup chunks to the simulation, each seeded with the finite topology" $ \env →
        withVisibleFinite env 20 10 $ \pid _ ab seeds → do
            let loaded = [ (c, topo) | SimChunkLoaded p _ topo c _ _ ← seeds, p ≡ pid ]
            sort (map fst loaded) `shouldBe`
                sort [ ChunkCoord x y | (x, y) ← [(-1, -1), (-1, 0), (0, -1), (0, 0)] ]
            map snd loaded `shouldBe` replicate 4 (SimFiniteFlat ab)

-- * Demand

demandSpec ∷ SpecWith EngineEnv
demandSpec = describe "demand beyond the startup set" $ do

    it "leaves a finite map where it is when the camera goes off it" $ \env →
        withVisibleFinite env 20 10 $ \_ ws ab _ → do
            before ← residentKeys ws
            forM_ [ChunkCoord 50 50, ChunkCoord 2 0, ChunkCoord (-3) (-3), ChunkCoord 0 40] $ \c → do
                goToChunk env c
                threadDelay 600000
                residentKeys ws `shouldReturn` before
            wtdBounds ⊚ readIORef (wsTilesRef ws) `shouldReturn` Just ab
            Just params ← readIORef (wsGenParamsRef ws)
            wgpArenaBounds params `shouldBe` Just ab

    it "loads the remaining valid chunks of an arena larger than the startup set, and only those" $ \env →
        withVisibleFinite env 200 200 $ \_ ws ab _ → do      -- X, Y = −100…99: chunks −7…6
            goToChunk env (ChunkCoord 7 0)
            -- the window around chunk (7,0) reaches chunks 5 and 6, which exist
            _ ← awaitChunk ws (ChunkCoord 6 0)
            partial ← awaitChunk ws (ChunkCoord 6 0)
            chunkDiff partial (generateArenaChunkIn (Just ab) 0 (ChunkCoord 6 0)) `shouldBe` []
            threadDelay 600000
            keys ← residentKeys ws
            -- nothing at or beyond chunk 7, and no chunk past −7 either
            [ c | c@(ChunkCoord x y) ← keys, x > 6 ∨ y > 6 ∨ x < -7 ∨ y < -7 ] `shouldBe` []
            ChunkCoord 7 0 `elem` keys `shouldBe` False

    it "refuses an outside explicit request at once: nothing queued, claimed or waited for" $ \env →
        withVisibleFinite env 20 10 $ \pid ws _ _ → do
            n ← enqueueChunkRequest pid ws
                    [ ChunkCoord 1 0, ChunkCoord (-2) 0, ChunkCoord 0 5, ChunkCoord 60000 (-60000) ]
            n `shouldBe` 0
            readIORef (wsInitQueueRef ws) `shouldReturn` []
            readIORef (wsLoadPhaseRef ws) `shouldReturn` LoadDone
            -- asking again changes nothing and is just as fast
            enqueueChunkRequest pid ws [ChunkCoord 1 0] `shouldReturn` 0
            readIORef (wsLoadPhaseRef ws) `shouldReturn` LoadDone

    it "accepts the valid part of a mixed request once, dedups a repeat, and drains to done" $ \env →
        withVisibleFinite env 200 200 $ \pid ws ab _ → do
            let valid = [ChunkCoord 4 0, ChunkCoord 6 0]
                mixed = valid ⧺ [ChunkCoord 7 0, ChunkCoord 50 50, ChunkCoord 60000 (-60000)]
            enqueueChunkRequest pid ws mixed `shouldReturn` 2
            enqueueChunkRequest pid ws mixed `shouldReturn` 0
            forM_ valid $ \c → do
                lc ← awaitChunk ws c
                chunkDiff lc (generateArenaChunkIn (Just ab) 0 c) `shouldBe` []
            done ← waitUntil 30 ((≡ LoadDone) ⊚ readIORef (wsLoadPhaseRef ws))
            done `shouldBe` True
            readIORef (wsInitQueueRef ws) `shouldReturn` []
            residentChunk ws (ChunkCoord 7 0) `shouldReturn` Nothing

-- * Eviction

evictionSpec ∷ SpecWith EngineEnv
evictionSpec = describe "eviction and readmission" $

    it "readmits a partial chunk identical to what was evicted, with an edge edit replayed" $ \env →
        withVisibleFinite env 200 200 $ \pid ws ab _ → do
            let partial = ChunkCoord 6 0
                -- a full 5×5 window of valid chunks (−7…−3), so the budget of 25
                -- is spent on it alone and everything else is evicted
                away = ChunkCoord (-5) (-5)
                edge = (99, 0)                       -- the last tile of the map in x
                idx = 0 * 16 + (99 - 96)
                MaterialId graniteId = matGranite
            atomicModifyIORef' (wsTilesRef ws) $ \td → (td { wtdMaxChunks = 25 }, ())
            goToChunk env partial
            _ ← awaitChunk ws partial
            sendWorldCommand env (WorldAddTile pid (fst edge) (snd edge) matGranite)
            edited ← waitUntil 30 $ maybe False (\lc → lcTerrainSurfaceMap lc VU.! idx ≢ lcTerrainSurfaceMap
                (generateArenaChunkIn (Just ab) 0 partial) VU.! idx) ⊚ residentChunk ws partial
            edited `shouldBe` True
            goToChunk env away
            _ ← awaitChunk ws away
            gone ← waitUntil 30 (isNothing ⊚ residentChunk ws partial)
            gone `shouldBe` True
            goToChunk env partial
            back ← awaitChunk ws partial
            VU.last (ctMats (lcTiles back V.! idx)) `shouldBe` graniteId
            -- every other column, padding included, is the generator's own
            let base = generateArenaChunkIn (Just ab) 0 partial
            [ i | i ← [0 .. 255], i ≢ idx, lcTiles back V.! i ≢ lcTiles base V.! i ] `shouldBe` []

-- * Staged restoration

stagedName ∷ T.Text
stagedName = "finite_staged"

quietLog ∷ IO LoggerState
quietLog = initLogger defaultLogConfig { lcBackend = LogToCallback (\_ → pure ()) }

-- | A one-page snapshot of an arena with (or without) bounds.
snapshotOf ∷ Maybe ArenaBounds → WorldEdits → SessionSnapshot
snapshotOf mb edits =
    let pid = WorldPageId stagedName
        params = defaultWorldGenParams
            { wgpSeed = 0, wgpWorldSize = 100000, wgpArenaBounds = mb }
        page = (blankPageSnapshot pid params)
            { pgsEdits = edits
            , pgsGeneratedId = Just (fixtureGeneratedWorldIdForPage pid) }
    in SessionSnapshot
        { snapGameTime = 0, snapTexPalette = emptyTexPalette
        , snapNextItemId = 1, snapNextBuildingId = 1, snapNextUnitId = 1
        , snapActivePage = pid, snapVisiblePages = [pid]
        , snapLiveCamera = LiveCameraSnapshot (Just pid) 0 0 1 FaceSouth
        , snapPages = HM.singleton pid page
        , snapItemTags = mempty, snapItemRefs = mempty
        , snapPortableKnowledge = emptyPortableKnowledge }

saveDataOf ∷ SessionSnapshot → SaveData
saveDataOf = snapshotToSaveData
    (SaveRequestMeta "finite" "2026-10-18T00:00:00.000000Z" False)

stage ∷ EngineEnv → SaveData → IO (StagedPage, StagedSession)
stage env saveData = do
    logger ← quietLog
    matReg ← readIORef (materialRegistryRef env)
    staged ← stageSession env logger saveData matReg ≫= \r → case r of
        Left e → expectationFailure (T.unpack (renderStageError e)) ≫ error "unreachable"
        Right s → pure s
    case filter ((≡ WorldPageId stagedName) ∘ spPageId) (ssPages staged) of
        (p : _) → pure (p, staged)
        [] → expectationFailure "the staged page is missing" ≫ error "unreachable"

restoreSpec ∷ SpecWith EngineEnv
restoreSpec = describe "staged restoration" $ do

    it "rebuilds the same chunk set as creation, with the rectangle in place before any terrain" $ \env →
        forM_ startupExpectation $ \((w, h), coords) → do
            let ab = bounds w h
            (page, _) ← stage env (saveDataOf (snapshotOf (Just ab) emptyWorldEdits))
            let ws = spWorldState page
            residentKeys ws `shouldReturn` sort [ ChunkCoord x y | (x, y) ← coords ]
            tiles ← wtdChunks ⊚ readIORef (wsTilesRef ws)
            forM_ (HM.toList tiles) $ \(c, lc) → lc `shouldBe` generateArenaChunkIn (Just ab) 0 c
            wtdBounds ⊚ readIORef (wsTilesRef ws) `shouldReturn` Just ab
            (wgpArenaBounds =≪) ⊚ readIORef (wsGenParamsRef ws) `shouldReturn` Just ab

    it "replays an edge edit and clips an overlay edit that lies outside the rectangle" $ \env → do
        let ab = bounds 20 10
            overlay = appendEdit (ChunkCoord 0 0) (WeAddTile 9 5 matGranite)
                    $ appendEdit (ChunkCoord 0 0) (WeAddTile 12 0 matGranite) emptyWorldEdits
        (page, _) ← stage env (saveDataOf (snapshotOf (Just ab) overlay))
        Just lc ← residentChunk (spWorldState page) (ChunkCoord 0 0)
        -- the in-bounds corner was raised by one
        lcTerrainSurfaceMap lc VU.! (5 * 16 + 9) `shouldBe` lcTerrainSurfaceMap
            (generateArenaChunkIn (Just ab) 0 (ChunkCoord 0 0)) VU.! (5 * 16 + 9) + 1
        -- the damaged edit at x = 12 made no terrain: that column is still absent
        lcTiles lc V.! (0 * 16 + 12) `shouldBe` lcTiles (generateArenaChunkIn (Just ab) 0 (ChunkCoord 0 0)) V.! 12
        VU.length (ctMats (lcTiles lc V.! 12)) `shouldBe` 0

    it "restores an expandable arena as expandable" $ \env → do
        (page, _) ← stage env (saveDataOf (snapshotOf Nothing emptyWorldEdits))
        let ws = spWorldState page
        wtdBounds ⊚ readIORef (wsTilesRef ws) `shouldReturn` Nothing
        residentKeys ws `shouldReturn` sort arenaFootprint

    it "restores a LEGACY arena save (no bounds) as expandable, on the corrected foundation" $ \env → do
        bytes ← BS.readFile legacyFixture
        snap ← case decodeSessionEnvelope (HS.fromList ["unit_ai", "building_spawn"])
                       (HS.fromList ["unit_ai", "building_spawn"]) bytes of
            Left err → expectationFailure (T.unpack err) ≫ error "unreachable"
            Right (_, s, _, _) → pure s
        let pid = WorldPageId "legacy_arena"
            page0 = snapPages snap HM.! pid
        wgpArenaBounds (pgsGenParams page0) `shouldBe` Nothing
        logger ← quietLog
        matReg ← readIORef (materialRegistryRef env)
        staged ← stageSession env logger
                    (snapshotToSaveData (SaveRequestMeta "legacy" "2026-10-18T00:00:00.000000Z" False) snap)
                    matReg
        case staged of
            Left e → expectationFailure (T.unpack (renderStageError e))
            Right ss → case filter ((≡ pid) ∘ spPageId) (ssPages ss) of
                [] → expectationFailure "the legacy page is missing"
                (p : _) → do
                    wtdBounds ⊚ readIORef (wsTilesRef (spWorldState p)) `shouldReturn` Nothing
                    residentKeys (spWorldState p) `shouldReturn` sort arenaFootprint
        -- reading it did not rewrite it
        BS.readFile legacyFixture `shouldReturn` bytes

    it "lets a restored finite arena load its remaining valid chunks and refuse the outside" $ \env →
        (`finally` writeIORef (enginePausedRef env) False) $ do
            let ab = bounds 200 200
            (page, ss) ← stage env (saveDataOf (snapshotOf (Just ab) emptyWorldEdits))
            logger ← quietLog
            _ ← Q.flushQueue (simQueue env)
            publishStagedSession env logger 2708 ss
            let ws = spWorldState page
                pid = WorldPageId stagedName
            -- restored simulation state: every startup chunk is seeded to the
            -- sim with the FINITE topology (so the closed edge survives a load)
            seeds ← Q.flushQueue (simQueue env)
            let loaded = [ (c, topo) | SimChunkLoaded p _ topo c _ _ ← seeds, p ≡ pid ]
            sort (map fst loaded) `shouldBe` sort arenaFootprint
            map snd loaded `shouldBe` replicate (length arenaFootprint) (SimFiniteFlat ab)
            enqueueChunkRequest pid ws [ChunkCoord 5 0, ChunkCoord 7 0] `shouldReturn` 1
            lc ← awaitChunk ws (ChunkCoord 5 0)
            lc `shouldBe` generateArenaChunkIn (Just ab) 0 (ChunkCoord 5 0)
            sendWorldCommand env (WorldDestroy pid)

legacyFixture ∷ FilePath
legacyFixture = "test-headless/data/save-compat/z14-arena-legacy-shallow.bin"

-- * The real codec

codecSpec ∷ SpecWith EngineEnv
codecSpec = describe "the real component codec" $ do

    it "round-trips finite and expandable arenas, bounds exactly, through an envelope" $ \_ →
        forM_ [Nothing, Just (bounds 1 1), Just (bounds 20 10), Just (bounds 21 11)
              , Just (bounds 100000 100000)] $ \mb → do
            let snap = snapshotOf mb emptyWorldEdits
                bytes = encodeSessionSnapshot
                    (snapshotSaveMetadata (SaveRequestMeta "finite" "ts" False) snap) snap []
            case decodeSessionEnvelope HS.empty HS.empty bytes of
                Left err → expectationFailure (T.unpack err)
                Right (_, back, _, _) →
                    (wgpArenaBounds ∘ pgsGenParams ⊚ HM.lookup (WorldPageId stagedName) (snapPages back))
                        `shouldBe` Just mb

    it "refuses a stored size outside the dimension domain, naming the page" $ \_ → do
        let snap = snapshotOf (Just (bounds 20 10)) emptyWorldEdits
            page = snapPages snap HM.! WorldPageId stagedName
            core size = PageCoreDTO
                { pcPageId = WorldPageId stagedName
                , pcGenParams = toWorldGenParamsDTO (pgsGenParams page)
                , pcCameraX = 0, pcCameraY = 0, pcTimeHour = 0, pcTimeMinute = 0
                , pcTimeRemainder = 0, pcDateYear = 1, pcDateMonth = 1, pcDateDay = 1
                , pcMapMode = pgsMapMode page, pcIdentity = Nothing
                , pcGeneratedId = Just (fixtureGeneratedWorldIdForPage (WorldPageId stagedName))
                , pcLocationTags = [], pcArenaBounds = Just size }
            errorsFor size = map renderComponentError
                (validatePages (basePageSnapshots (WorldPagesDTO [core size])))
        errorsFor (ArenaBoundsDTO 20 10) `shouldBe` []
        forM_ [ArenaBoundsDTO 0 5, ArenaBoundsDTO 5 (-1), ArenaBoundsDTO 100001 1] $ \bad →
            errorsFor bad `shouldSatisfy` any (T.isInfixOf "dimension domain")

    it "refuses bounds on a page that is not an arena" $ \_ → do
        let pid = WorldPageId "generated_with_bounds"
            params = defaultWorldGenParams
                { wgpSeed = 42, wgpWorldSize = 64, wgpArenaBounds = Just (bounds 20 10) }
            page = (blankPageSnapshot pid params)
                { pgsGeneratedId = Just (fixtureGeneratedWorldIdForPage pid) }
            core = PageCoreDTO
                { pcPageId = pid, pcGenParams = toWorldGenParamsDTO params
                , pcCameraX = 0, pcCameraY = 0, pcTimeHour = 0, pcTimeMinute = 0
                , pcTimeRemainder = 0, pcDateYear = 1, pcDateMonth = 1, pcDateDay = 1
                , pcMapMode = pgsMapMode page, pcIdentity = Nothing
                , pcGeneratedId = pgsGeneratedId page, pcLocationTags = []
                , pcArenaBounds = Just (ArenaBoundsDTO 20 10) }
        map renderComponentError (validatePages (basePageSnapshots (WorldPagesDTO [core])))
            `shouldSatisfy` any (T.isInfixOf "not an arena page")

-- * Authoritative mutations

mutationSpec ∷ SpecWith EngineEnv
mutationSpec = describe "authoritative mutations on a hidden finite page" $ do

    it "applies edge and corner edits and refuses every one outside, leaving no edit and no terrain" $ \env →
        withHiddenFinite env 20 10 $ \pid ws ab → do
            let granite = matGranite
                marker = (0, 0)
                outside = [(10, 0), (-11, 0), (0, 6), (0, -5), (10, 6), (-11, -5), (12, 2)]
                inside = [(9, 5), (-10, -4), (9, -4), (-10, 5)]
            -- everything outside first, then the in-bounds edits, then a marker
            forM_ outside $ \(x, y) → do
                sendWorldCommand env (WorldAddTile pid x y granite)
                sendWorldCommand env (WorldDeleteTile pid x y)
                sendWorldCommand env (WorldSetCell pid x y 0 granite)
                sendWorldCommand env (WorldSetSlope pid x y 0 1)
                sendWorldCommand env (WorldSetFluidTile pid x y Lake)
                sendWorldCommand env (WorldDebugSetFluidSurface pid x y Lake 3)
                sendWorldCommand env (WorldSetVeg pid x y 0 1)
                sendWorldCommand env (WorldClearStructure pid x y 0)
            forM_ inside $ \(x, y) → sendWorldCommand env (WorldAddTile pid x y granite)
            sendWorldCommand env (WorldAddTile pid (fst marker) (snd marker) granite)
            let done = do
                    lcs ← mapM (residentChunk ws) [ChunkCoord (-1) (-1), ChunkCoord 0 (-1), ChunkCoord (-1) 0, ChunkCoord 0 0]
                    pure (all (maybe False (const True)) lcs)
            _ ← waitUntil 30 done
            applied ← waitUntil 30 $ do
                m ← residentChunk ws (ChunkCoord 0 0)
                pure (maybe False (\lc → lcTerrainSurfaceMap lc VU.! 0 ≡ lcTerrainSurfaceMap
                        (generateArenaChunkIn (Just ab) 0 (ChunkCoord 0 0)) VU.! 0 + 1) m)
            applied `shouldBe` True
            -- the edit log holds exactly the five in-bounds edits
            edits ← readIORef (wsEditsRef ws)
            sort [ (c, length es) | (c, es) ← HM.toList edits ]
                `shouldBe` sort [ (ChunkCoord (-1) (-1), 1), (ChunkCoord 0 (-1), 1)
                                , (ChunkCoord (-1) 0, 1), (ChunkCoord 0 0, 2) ]
            -- and every padding column is still absent, outside tiles still no terrain
            forM_ [ChunkCoord (-1) (-1), ChunkCoord 0 (-1), ChunkCoord (-1) 0, ChunkCoord 0 0] $ \c → do
                Just lc ← residentChunk ws c
                let want = generateArenaChunkIn (Just ab) 0 c
                    changed = [ i | i ← [0 .. 255], lcTiles lc V.! i ≢ lcTiles want V.! i ]
                    inBounds i = let gx = fst (cxy c) * 16 + i `mod` 16
                                     gy = snd (cxy c) * 16 + i `div` 16
                                 in gx ≥ -10 ∧ gx ≤ 9 ∧ gy ≥ -4 ∧ gy ≤ 5
                filter (not ∘ inBounds) changed `shouldBe` []

    it "refuses an outside edit on a hidden page while the visible page's own rectangle would admit it" $ \env →
        withHiddenFinite env 20 10 $ \pid ws _ → do
            -- (50, 50) is an ordinary tile of the visible, expandable page
            sendWorldCommand env (WorldAddTile pid 50 50 matGranite)
            sendWorldCommand env (WorldAddTile pid 0 0 matGranite)
            _ ← waitUntil 30 $ maybe False (const True) ⊚ residentChunk ws (ChunkCoord 0 0)
            applied ← waitUntil 30 $ do
                m ← residentChunk ws (ChunkCoord 0 0)
                pure (maybe False (\lc → lcTerrainSurfaceMap lc VU.! 0 ≡ lcTerrainSurfaceMap
                        (generateArenaChunk 0 (ChunkCoord 0 0)) VU.! 0 + 1) m)
            applied `shouldBe` True
            edits ← readIORef (wsEditsRef ws)
            HM.keys edits `shouldBe` [ChunkCoord 0 0]
            residentChunk ws (ChunkCoord 3 3) `shouldReturn` Nothing
  where cxy (ChunkCoord x y) = (x, y)

-- * Buildings and ground objects

objectSpec ∷ SpecWith EngineEnv
objectSpec = describe "buildings and ground objects" $ do

    it "judges a footprint whole: placeable only when every tile is inside the rectangle" $ \env →
        withHiddenFinite env 20 10 $ \_ ws _ → do
            td ← readIORef (wsTilesRef ws)
            let bm = emptyBuildingManager
                place w h x y = canPlaceAt bm td emptyLocationInstances 0 (mkDef False w h) x y
                crosses = NotPlaceable footprintOutsideArenaReason
            place 1 1 9 5 `shouldBe` Placeable
            place 1 1 (-10) (-4) `shouldBe` Placeable
            place 2 2 8 4 `shouldBe` Placeable
            place 2 2 9 5 `shouldBe` crosses
            place 2 2 (-11) (-4) `shouldBe` crosses
            place 2 1 9 0 `shouldBe` crosses
            place 1 2 0 5 `shouldBe` crosses
            place 1 1 10 0 `shouldBe` crosses
            place 3 3 50 50 `shouldBe` crosses

    it "inserts a fitting building and drops a crossing one, releasing its claim and leaving no instance" $ \env →
        withHiddenFinite env 20 10 $ \pid ws _ → do
            logger ← quietLog
            epoch ← pageIncarnation ws
            let defFor w h = (mkDef False w h) { bdName = "finite_box" }
                spawn bid w h gx gy = do
                    writeIORef (buildingManagerRef env) emptyBuildingManager
                        { bmDefs = HM.singleton "finite_box" (defFor w h) }
                    -- the admission, as building.spawn takes it (it would have
                    -- passed canPlaceAt in a place the page did not bound)
                    claimed ← atomicModifyIORef' (buildingManagerRef env) $ \bm →
                        reserveFootprint 0 pid (defFor w h) gx gy bm
                    case claimed of
                        Left why → expectationFailure (T.unpack why)
                        Right _ → pure ()
                    applyBuildingSpawn logger (toWorldSimCapability env)
                        (toContentRegistriesViewCapability env) (toBuildingCapability env)
                        bid "finite_box" gx gy 0 pid epoch
                    bm ← readIORef (buildingManagerRef env)
                    pure (HM.member bid (bmInstances bm), HM.size (bmReservations bm))
            -- the first reservation takes id 1
            spawn (BuildingId 1) 1 1 9 5 `shouldReturn` (True, 0)
            spawn (BuildingId 1) 2 2 9 5 `shouldReturn` (False, 0)
            spawn (BuildingId 1) 3 1 8 0 `shouldReturn` (False, 0)

    it "rests a ground object only inside the map, and refuses to create one outside" $ \env →
        withHiddenFinite env 20 10 $ \_ ws _ → do
            td ← readIORef (wsTilesRef ws)
            params ← readIORef (wsGenParamsRef ws)
            groundRestShift params td 9 5 `shouldBe` Just (0, 0)
            groundRestShift params td (-10) (-4) `shouldBe` Just (0, 0)
            forM_ [(10, 5), (9, 6), (-11, -4), (-10, -5), (50, 50)] $ \(x, y) →
                groundRestShift params td x y `shouldBe` Nothing
            writeIORef (itemManagerRef env) containerItemDefs
            let spawnAt x y = spawnSalvageOnPage env ws crateItemDef "fixture_crate"
                                  x y Nothing Nothing Nothing Nothing
                groundCount = HM.size ∘ gisItems ⊚ readIORef (wsGroundItemsRef ws)
            forM_ [(10.5, 0.5), (0.5, 6.5), (-10.5, 0.5), (0.5, -4.5), (1.0e9, 0.5)] $ \(x, y) →
                spawnAt x y >>= \r → isJust r `shouldBe` False
            groundCount `shouldReturn` 0
            spawnAt 9.5 5.5 >>= \r → isJust r `shouldBe` True
            groundCount `shouldReturn` 1
