-- | Explicitly sampled diagnostics. World payloads and owner states are
-- independent snapshots; simulation replies are timestamped and epoch-bound.
-- Nothing here claims a globally atomic snapshot of the three owners.
module Engine.Scripting.Lua.API.ChunkMemory
    ( ChunkMemoryWindow, newChunkMemoryWindow
    , getChunkMemoryFn, resetChunkMemoryWindowFn
    , PageMemory(..), readPageMemory, pageMemoryValue
    ) where

import UPrelude
import qualified Data.Aeson as A
import Data.Aeson ((.=))
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import qualified Data.Map.Strict as Map
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.List (find)
import GHC.Conc (getNumCapabilities)
import Control.Concurrent.MVar (MVar, newEmptyMVar, tryTakeMVar)
import Control.Exception (mask_)
import qualified HsLua as Lua
import Engine.Core.Clock (monotonicSeconds)
import Engine.Core.Capability.WorldSim (WorldSimCapability(..))
import qualified Engine.Core.Queue as Q
import Engine.Scripting.Lua.API.Yaml (pushAeson)
import World.State.Types (WorldState(..), WorldManager(..))
import World.Page.Types (WorldPageId(..))
import World.Tile.Types (WorldTileData(..))
import World.Chunk.Types (ChunkCoord)
import World.Chunk.Memory
import World.Chunk.Residency
import Sim.Command.Types (SimCommand(..))
import Sim.Memory

data PageMemory = PageMemory
    { pmPage ∷ !WorldPageId
    , pmEpoch ∷ !ChunkGeneration
    , pmTime ∷ !Double
    , pmKeys ∷ !(HS.HashSet ChunkCoord)
    , pmBytes ∷ !ChunkBytes
    , pmMinBytes ∷ !Integer
    , pmMaxBytes ∷ !Integer
    , pmRequested ∷ !Int
    , pmInFlight ∷ !Int
    , pmOwnerResident ∷ !Int
    , pmOwnerAgrees ∷ !Bool
    }

-- Only scalar high-water rows survive calls; prune rows for retired pages.
data MemoryWindow = MemoryWindow
    { mwStart ∷ !Double
    , mwSamples ∷ !Integer
    , mwRows ∷ !(Map.Map (WorldPageId, Word64) (Int, Integer))
    , mwPeakCount ∷ !Int
    , mwPeakBytes ∷ !Integer
    , mwPending ∷ !(Maybe (MVar (Maybe SimMemory)))
    , mwSim ∷ !(Maybe SimMemory)
    }
newtype ChunkMemoryWindow = ChunkMemoryWindow (IORef MemoryWindow)

newChunkMemoryWindow ∷ IO ChunkMemoryWindow
newChunkMemoryWindow = do
    now ← monotonicSeconds
    ChunkMemoryWindow ⊚ newIORef (MemoryWindow now 0 Map.empty 0 0 Nothing Nothing)

readPageMemory ∷ (WorldPageId, WorldState) → IO PageMemory
readPageMemory (pid, ws) = do
    now ← monotonicSeconds
    td ← readIORef (wsTilesRef ws)
    owner ← readIORef (wsChunkResidencyRef ws)
    let states = chunkOwnerStates owner
        resident = HS.fromList [ckCoord k | (k, ChunkResident) ← states]
        keys = HM.keysSet (wtdChunks td)
        costs = map chunkBytes (HM.elems (wtdChunks td))
        totals = map totalChunkBytes costs
    pure PageMemory
        { pmPage = pid, pmEpoch = chunkOwnerGeneration owner, pmTime = now
        , pmKeys = keys, pmBytes = mconcat costs
        , pmMinBytes = minimum (if null totals then [0] else totals)
        , pmMaxBytes = maximum (0:totals)
        , pmRequested = length [() | (_, ChunkRequested) ← states]
        , pmInFlight = length [() | (_, ChunkInFlight) ← states]
        , pmOwnerResident = HS.size resident
        , pmOwnerAgrees = resident ≡ keys
        }

bytesValue ∷ ChunkBytes → A.Value
bytesValue b = A.object
    [ "columns" .= cbColumns b, "derivedMaps" .= cbMaps b
    , "overlays" .= cbOverlays b, "containers" .= cbContainers b
    , "total" .= totalChunkBytes b ]

pageMemoryValue ∷ PageMemory → A.Value
pageMemoryValue p = A.object
    [ "page" .= unWorldPageId (pmPage p)
    , "incarnation" .= chunkGenerationValue (pmEpoch p)
    , "sampleTimeSeconds" .= pmTime p
    , "resident" .= HS.size (pmKeys p)
    , "requested" .= pmRequested p, "inFlight" .= pmInFlight p
    , "ownerResident" .= pmOwnerResident p
    , "ownerKeysAgree" .= pmOwnerAgrees p
    , "logicalEstimatedBytes" .= bytesValue (pmBytes p)
    , "minChunkBytes" .= pmMinBytes p, "maxChunkBytes" .= pmMaxBytes p
    ]

simValue ∷ Double → [PageMemory] → Maybe SimMemory → A.Value
simValue _ _ Nothing = A.object ["available" .= False]
simValue now pages (Just sm) = A.object
    [ "available" .= True, "sampleTimeSeconds" .= smTime sm
    , "ageSeconds" .= max 0 (now - smTime sm)
    , "logicalEstimatedBytes" .= sum [sum (map snd (spmChunks s)) | s ← smPages sm]
    , "pages" .= map row (smPages sm) ]
  where
    row s =
        let current = find (\p → pmPage p ≡ spmPage s ∧ Just (pmEpoch p) ≡ spmEpoch s) pages
            absent = (\p → length [() | (cc, _) ← spmChunks s, not (HS.member cc (pmKeys p))]) ⊚ current
        in A.object
            [ "page" .= unWorldPageId (spmPage s)
            , "incarnation" .= (chunkGenerationValue ⊚ spmEpoch s)
            , "currentIncarnation" .= isJust current
            , "chunks" .= length (spmChunks s)
            , "notInTileCache" .= absent
            , "logicalEstimatedBytes" .= sum (map snd (spmChunks s)) ]

getChunkMemoryFn ∷ ChunkMemoryWindow → WorldSimCapability → Lua.LuaE Lua.Exception Lua.NumResults
getChunkMemoryFn (ChunkMemoryWindow ref) wsc = do
    value ← Lua.liftIO $ do
        mgr ← readIORef (wsWorldManagerRef wsc)
        pages ← traverse readPageMemory (wmWorlds mgr)
        now ← monotonicSeconds
        capabilities ← getNumCapabilities
        -- Single Lua-thread owner; masking prevents enqueue-without-recording.
        mask_ $ do
            old ← readIORef ref
            ready ← maybe (pure Nothing) tryTakeMVar (mwPending old)
            pending ← case (mwPending old, ready) of
                (Just p, Nothing) → pure p
                _ → do
                    p ← newEmptyMVar
                    Q.writeQueue (wsSimQueue wsc) (SimReadMemory p)
                    pure p
            let latest = case ready of
                    Just (Just sm) | smTime sm ≥ mwStart old → Just sm
                    Just Nothing → Nothing
                    _ → mwSim old
                rows = Map.fromList
                    [ let key = (pmPage p, chunkGenerationValue (pmEpoch p))
                          (n,b) = Map.findWithDefault (0,0) key (mwRows old)
                          !peakN = max n (HS.size (pmKeys p))
                          !peakB = max b (totalChunkBytes (pmBytes p))
                      in (key, (peakN, peakB))
                    | p ← pages ]
                count = sum (map (HS.size . pmKeys) pages)
                bytes = totalChunkBytes (mconcat (map pmBytes pages))
                next = old { mwRows = rows, mwSamples = mwSamples old + 1
                           , mwPeakCount = max count (mwPeakCount old)
                           , mwPeakBytes = max bytes (mwPeakBytes old)
                           , mwPending = Just pending, mwSim = latest }
            writeIORef ref next
            pure $ A.object
                [ "schema" .= (1 ∷ Int), "model" .= ("logical-unshared-v1" ∷ Text)
                , "capabilities" .= capabilities
                , "wordBytes" .= wordBytes
                , "consistency" .= ("independent owner samples; not a transaction" ∷ Text)
                , "windowStartSeconds" .= mwStart next, "sampleTimeSeconds" .= now
                , "samples" .= mwSamples next, "highWaterKind" .= ("sampled" ∷ Text)
                , "resident" .= count, "logicalEstimatedBytes" .= bytes
                , "peakResident" .= mwPeakCount next, "peakLogicalEstimatedBytes" .= mwPeakBytes next
                , "pageHighWater" .= [A.object ["page" .= unWorldPageId pid, "incarnation" .= epoch
                                      , "resident" .= n, "logicalEstimatedBytes" .= b]
                                    | ((pid,epoch),(n,b)) ← Map.toAscList rows]
                , "pages" .= map pageMemoryValue pages, "simulation" .= simValue now pages latest ]
    pushAeson value
    pure 1

resetChunkMemoryWindowFn ∷ ChunkMemoryWindow → Lua.LuaE Lua.Exception Lua.NumResults
resetChunkMemoryWindowFn (ChunkMemoryWindow ref) = do
    Lua.liftIO $ do
        now ← monotonicSeconds
        old ← readIORef ref
        -- Keep the pending reply slot: resetting must not enqueue duplicates
        -- while the sim is parked behind a save/load barrier.
        writeIORef ref (MemoryWindow now 0 Map.empty 0 0 (mwPending old) Nothing)
    pure 0
