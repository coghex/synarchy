module Test.Headless.World.ChunkMemory (spec) where

import UPrelude
import Test.Hspec
import Control.Exception (finally)
import Data.IORef (newIORef, readIORef, writeIORef)
import qualified Data.ByteString.Char8 as BS
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU
import qualified HsLua as Lua
import Engine.Core.State (EngineEnv(..))
import Engine.Core.Thread (ThreadControl(..))
import qualified Engine.Core.Queue as Q
import Engine.Scripting.Lua.API (registerLuaAPI)
import Engine.Scripting.Lua.API.ChunkMemory
import Engine.Scripting.Lua.Thread (createLuaBackendState)
import Engine.Scripting.Lua.Types (LuaBackendState(..))
import World.Chunk.Memory
import World.Chunk.Types
import World.Chunk.Admit
import World.Chunk.Residency (canonicalChunkCoord)
import World.Generate.Arena (generateFlatChunk)
import World.Generate.Types (WorldGenParams(..), defaultWorldGenParams)
import World.Page.Types (WorldPageId(..))
import World.State.Types (WorldState(..), WorldManager(..), emptyWorldState, emptyWorldManager)
import World.Tile.Types (WorldTileData(..), emptyWorldTileData)
import World.Fluid.Types (FluidCell(..), FluidType(..), IceCell(..), IceMode(..))
import World.Flora.Types (FloraChunkData(..), FloraInstance(..), FloraId(..))
import World.Flora.Identity (plantedFloraInstanceId)
import World.Magma.Overlay (MagmaOverlay(..))
import Structure.Types (StructurePieceData(..))
import Sim.State.Types
import Sim.Chunk (loadedChunkState)
import Sim.Thread (handleSimCommand)
import Sim.Memory (captureSimMemory, SimMemory(..), SimPageMemory(..))
import World.Load.Publish (discardStaleQueues)
import Test.Headless.Harness (withHeadlessEngineNoWorld)
import Test.Headless.Harness.Isolation (withIsolatedResourceRoot)

pid ∷ WorldPageId
pid = WorldPageId "memory"
coord ∷ ChunkCoord
coord = ChunkCoord 0 0
params ∷ WorldGenParams
params = defaultWorldGenParams { wgpWorldSize = 8, wgpSeed = 1 }

depthChunk ∷ Int → LoadedChunk
depthChunk n = (generateFlatChunk coord)
    { lcTiles = V.replicate 256 (ColumnTiles 0 (VU.replicate n 1)
                        (VU.replicate n 0) (VU.replicate n 0)) }

page ∷ IO WorldState
page = do
    ws ← emptyWorldState
    writeIORef (wsGenParamsRef ws) (Just params)
    pure ws

seed ∷ WorldState → ChunkCoord → LoadedChunk → IO ()
seed ws cc chunk = do
    claims ← claimChunkGeneration ws pid params [cc]
    publishSeedChunks ws claims emptyWorldTileData { wtdChunks = HM.singleton cc chunk }

check ∷ Lua.State → BS.ByteString → IO ()
check ls code = Lua.runWith ls $ do
    result ← Lua.dostring code
    when (result ≢ Lua.OK) $ do
        err ← Lua.tostring (-1)
        Lua.liftIO $ expectationFailure (show err)

withBackend ∷ EngineEnv → (Lua.State → IO α) → IO α
withBackend env action = do
    backend ← createLuaBackendState (luaToEngineQueue env) (luaQueue env)
        (assetPoolRef env) (nextObjectIdRef env) (inputStateRef env) (loggerRef env)
    state ← newIORef ThreadRunning
    let ls = lbsLuaState backend
    (registerLuaAPI ls env backend state ≫ action ls) `finally` Lua.close ls

spec ∷ Spec
spec = describe "chunk residency accounting" $ do
    it "charges stored interior air and longer columns without changing count" $ do
        let shallow = depthChunk 8
            deep = depthChunk 80
            air = deep { lcTiles = V.map (\c → c { ctMats = VU.replicate 80 0 }) (lcTiles deep) }
        cbColumns (chunkBytes deep) - cbColumns (chunkBytes shallow) `shouldBe` 256*72*3
        chunkBytes air `shouldBe` chunkBytes deep
        cbMaps (chunkBytes deep) `shouldBe` cbMaps (chunkBytes shallow)

    it "includes fluid, ice, flora, structures and sparse magma overlays" $ do
        let base = depthChunk 8
            wet = base { lcFluidMap = V.replicate 256 (Just (FluidCell Lake 12)) }
            plant = FloraInstance (FloraId 1) 0 0 0 0 8 1 1 0 16
                        (plantedFloraInstanceId 1) False
            rich = wet { lcIceMap = V.replicate 256 (Just (IceCell 13 BasinIce))
                       , lcFlora = FloraChunkData [plant]
                       , lcStructures = HM.singleton (0,0,0) (StructurePieceData 1 2 8)
                       , lcMagma = Just (MagmaOverlay (HM.singleton (0,0) 8) HM.empty) }
        cbMaps (chunkBytes wet) `shouldSatisfy` (> cbMaps (chunkBytes base))
        cbOverlays (chunkBytes rich) `shouldSatisfy` (> cbOverlays (chunkBytes wet))
        cbColumns (chunkBytes rich) `shouldBe` cbColumns (chunkBytes base)

    it "separates aliases, requests, transient claims, admissions, edits and eviction" $ do
        ws ← page
        let alias = ChunkCoord 4 0
            canon = canonicalChunkCoord params alias
        registered ← registerChunkDemand ws pid params [alias,canon]
        length registered `shouldBe` 1
        pending ← readPageMemory (pid,ws)
        pmRequested pending `shouldBe` 1
        HS.size (pmKeys pending) `shouldBe` 0
        withTransientChunkClaim ws pid params alias $ do
            transient ← readPageMemory (pid,ws)
            pmInFlight transient `shouldBe` 1
            HS.size (pmKeys transient) `shouldBe` 0
        claims ← claimChunkGeneration ws pid params [alias]
        admitResidentChunks ws claims
        mixed ← readPageMemory (pid,ws)
        pmOwnerAgrees mixed `shouldBe` False
        publishSeedChunks ws claims emptyWorldTileData
            { wtdChunks = HM.singleton canon (depthChunk 8) }
        first ← readPageMemory (pid,ws)
        pmOwnerAgrees first `shouldBe` True
        pmRequested first `shouldBe` 0
        pmInFlight first `shouldBe` 0
        writeIORef (wsTilesRef ws) emptyWorldTileData
            { wtdChunks = HM.singleton canon (depthChunk 80) }
        edited ← readPageMemory (pid,ws)
        pmKeys edited `shouldBe` pmKeys first
        cbColumns (pmBytes edited) `shouldSatisfy` (> cbColumns (pmBytes first))
        writeIORef (wsTilesRef ws) emptyWorldTileData
        releaseEvictedChunks ws pid params [canon]
        gone ← readPageMemory (pid,ws)
        pmOwnerResident gone `shouldBe` 0
        pmBytes gone `shouldBe` mempty
        pmOwnerAgrees gone `shouldBe` True

    it "reports the simulation owner's retained chunks and their incarnation" $ do
        ws ← page
        epoch ← pageIncarnation ws
        let lc = depthChunk 8
            sc = loadedChunkState (lcFluidMap lc) (lcTerrainSurfaceMap lc)
            sw = emptySimWorldState { swsIncarnation = Just epoch, swsChunks = HM.singleton coord sc }
        snap ← captureSimMemory emptySimState { ssWorlds = HM.singleton pid sw }
        map spmEpoch (smPages snap) `shouldBe` [Just epoch]
        map (map fst . spmChunks) (smPages snap) `shouldBe` [[coord]]
        sum [sum (map snd (spmChunks p)) | p ← smPages snap] `shouldSatisfy` (> 0)

    it "wires real queries, sampled windows, bounded pending requests and replacement isolation" $
        withIsolatedResourceRoot $ withHeadlessEngineNoWorld $ \env → withBackend env $ \ls → do
            ws ← page
            seed ws coord (depthChunk 8)
            let install w = writeIORef (worldManagerRef env) emptyWorldManager { wmWorlds = [(pid,w)] }
            install ws
            check ls "local s=world.getChunkMemory(); assert(s.resident==1 and s.pages[1].ownerKeysAgree); assert(not s.simulation.available); assert(s.highWaterKind=='sampled')"
            check ls "for i=1,20 do world.getChunkMemory(); world.resetChunkMemoryWindow() end"
            cmds ← Q.flushQueue (simQueue env)
            length cmds `shouldBe` 1
            epoch ← pageIncarnation ws
            let lc = depthChunk 8
                sc = loadedChunkState (lcFluidMap lc) (lcTerrainSurfaceMap lc)
                sw = emptySimWorldState { swsIncarnation = Just epoch
                     , swsChunks = HM.fromList [(coord,sc),(ChunkCoord 1 0,sc)] }
            sim ← newIORef emptySimState { ssWorlds = HM.singleton pid sw }
            logger ← readIORef (loggerRef env)
            mapM_ (handleSimCommand env logger sim) cmds
            check ls "local s=world.getChunkMemory(); assert(s.simulation.available); local p=s.simulation.pages[1]; assert(p.currentIncarnation and p.chunks==2 and p.notInTileCache==1); assert(s.samples==1)"
            writeIORef (wsTilesRef ws) emptyWorldTileData
            releaseEvictedChunks ws pid params [coord]
            check ls "local s=world.getChunkMemory(); assert(s.resident==0 and s.peakResident==1); assert(s.simulation.pages[1].notInTileCache==2)"
            replacement ← page
            install replacement
            check ls "local s=world.getChunkMemory(); assert(s.pageHighWater[1].resident==0); assert(not s.simulation.pages[1].currentIncarnation); assert(s.simulation.pages[1].notInTileCache==nil)"
            check ls "world.resetChunkMemoryWindow(); local s=world.getChunkMemory(); assert(s.peakResident==0 and s.samples==1 and not s.simulation.available)"
            discardStaleQueues env logger
            check ls "assert(not world.getChunkMemory().simulation.available)"
            afterLoad ← Q.flushQueue (simQueue env)
            length afterLoad `shouldBe` 1
            mapM_ (handleSimCommand env logger sim) afterLoad
            check ls "assert(world.getChunkMemory().simulation.available)"
