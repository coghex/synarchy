-- | The causal fence between a live world edit and the sim's fluid
--   writebacks (#1596).
--
--   'World.Command.Types.WorldApplyFluids' and every edit command share
--   the WORLD queue; the re-seed an edit sends the sim
--   ('World.Thread.Command.Edit.Sync.syncEditToSim') rides the
--   INDEPENDENT sim queue with no acknowledgement. Nothing orders the
--   two, so a batch the sim computed from the PRE-edit chunk can land
--   behind the edit, and 'World.Thread.Command.applyOneWriteback'
--   replaces that chunk's whole fluid / terrain-surface /
--   rendered-surface / side-decoration set. Pausing right after the edit
--   made the damage durable, because the writeback that would have
--   repaired it is only produced by the sim's tick branch — and a save
--   in that window persisted the pre-edit fluid as trailing
--   'World.Edit.Types.WeSetFluidSnapshot' edits that replay AFTER the
--   player's own.
--
--   These specs drive the real world thread, so every step is the
--   production path: real 'World.Command.Types.WorldSetFluidTile' /
--   'World.Command.Types.WorldAddTile' handlers, the real
--   'World.Thread.Command.handleApplyFluidsCommand', the real
--   'World.Thread.Command.Save.WriteWorld' save transaction, and the
--   real 'World.Edit.Apply.replayEdits' replay. The one thing they do
--   NOT use is a live sim thread: the whole point is to force ONE
--   interleaving that a running sim only reaches by chance, so the batch
--   is constructed from a snapshot taken BEFORE the edits and delivered
--   AFTER them, exactly as the losing race would.
--
--   No sim thread also means the sim queue is undrained, which is what
--   lets 'editGenFromSimQueue' read the generation the production edit
--   handler actually published — the sim's own half of the handoff is
--   to keep that number ('Sim.State.Types.scsEditGen') and stamp it onto
--   the writebacks it later emits.
--
--   Run:
--   @cabal test synarchy-test-headless --test-options='--match "fluid writeback staleness"'@
module Test.Headless.World.FluidWritebackStaleness (spec, saveSpec) where

import UPrelude
import Test.Hspec
import Control.Concurrent (threadDelay)
import Control.Concurrent.MVar (MVar, newEmptyMVar, takeMVar)
import Control.Exception (finally)
import Data.IORef (readIORef)
import Data.List (find)
import Data.Maybe (mapMaybe)
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import qualified Data.Text as T
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU
import System.Directory (doesFileExist, removePathForcibly)
import System.Timeout (timeout)

import Engine.Core.State (EngineEnv(..))
import qualified Engine.Core.Queue as Q
import Sim.Command.Types (SimCommand(..))
import Test.Headless.Harness (sendWorldCommand, waitForWorldInit)
import World.Edit.Apply (replayEdits)
import World.Edit.Types (WorldEdit(..))
import World.Generate.Coordinates (chunkToGlobal)
import World.Material (matLoam)
import World.Save.Serialize (loadWorld)
import World.Types

-- | Every page here is its own private w8 world: these specs EDIT the
--   page and hand-deliver writebacks to it, so they must not share one.
stalePageId, recoveryPageId, ackPageId, savePageId ∷ WorldPageId
stalePageId    = WorldPageId "fwb_stale_w8"
recoveryPageId = WorldPageId "fwb_recovery_w8"
ackPageId      = WorldPageId "fwb_ack_w8"
savePageId     = WorldPageId "fwb_save_w8"

saveSlotName ∷ Text
saveSlotName = "fwb_stale_window_spec"

chunkCells ∷ Int
chunkCells = chunkSize * chunkSize

-- | Everything one scenario needs after its page is generated and its
--   two live edits have provably committed.
data EditedPage = EditedPage
    { epState     ∷ WorldState
    , epEdited    ∷ ChunkCoord   -- ^ The chunk both edits landed in.
    , epUntouched ∷ ChunkCoord   -- ^ A different loaded chunk, never edited.
    , epFluidTile ∷ (Int, Int)   -- ^ Global tile the fluid edit targeted.
    , epFluidIdx  ∷ Int          -- ^ …its column index within 'epEdited'.
    , epTerrIdx   ∷ Int          -- ^ Column index the terrain edit targeted.
    , epBefore    ∷ LoadedChunk  -- ^ 'epEdited' as it stood BEFORE the edits.
    , epAfter     ∷ LoadedChunk  -- ^ …and immediately after them.
    , epUntBefore ∷ LoadedChunk  -- ^ 'epUntouched' before anything was sent.
    }

spec ∷ SpecWith EngineEnv
spec =
    describe "fluid writeback staleness (#1596)" $ do

      it "drops a writeback derived from the pre-edit chunk, keeping all \
         \four sim-owned fields at the edited values, while an untouched \
         \chunk in the SAME batch is applied in full" $ \env → do
        ep ← withEditedPage env stalePageId

        -- The losing race, forced: this payload was captured before
        -- either edit was sent. Only lcSideDeco carries a synthetic
        -- value — it is sim-owned and no live edit touches it, so its
        -- pre-edit and post-edit contents are identical and a real
        -- pre-edit copy could not distinguish "dropped" from "applied".
        let stale = FluidWriteback
                { fwCoord    = epEdited ep
                , fwEditGen  = 0
                , fwFluid    = lcFluidMap (epBefore ep)
                , fwTerrain  = lcTerrainSurfaceMap (epBefore ep)
                , fwSurf     = lcSurfaceMap (epBefore ep)
                , fwSideDeco = VU.replicate chunkCells 0xAB
                }
            fresh = perturbed (epUntouched ep) 0 (epUntBefore ep)

        deliverBatch env stalePageId [stale, fresh]

        edited ← chunkAt (epState ep) (epEdited ep)
        -- Requirements 1 and 7: every field 'applyOneWriteback' writes
        -- still holds what the edits left there, and nothing about the
        -- rest of the chunk moved either.
        lcFluidMap edited          `shouldBe` lcFluidMap (epAfter ep)
        lcTerrainSurfaceMap edited `shouldBe` lcTerrainSurfaceMap (epAfter ep)
        lcSurfaceMap edited        `shouldBe` lcSurfaceMap (epAfter ep)
        lcSideDeco edited          `shouldBe` lcSideDeco (epAfter ep)

        -- Requirement 2: the drop is per chunk, so the untouched chunk's
        -- writeback in the same batch lands whole.
        untouched ← chunkAt (epState ep) (epUntouched ep)
        lcFluidMap untouched          `shouldBe` fwFluid fresh
        lcTerrainSurfaceMap untouched `shouldBe` fwTerrain fresh
        lcSurfaceMap untouched        `shouldBe` fwSurf fresh
        lcSideDeco untouched          `shouldBe` fwSideDeco fresh

      it "accepts the next writeback for the edited chunk once it carries \
         \the generation the production SimChunkEdited re-seed published" $
        \env → do
        ep ← withEditedPage env recoveryPageId

        -- The generation the world thread minted for this chunk and put
        -- on the sim wire. Two edits landed, so it is the second one's:
        -- taking the LAST message is what a sim that drained the queue
        -- in order would have kept.
        gen ← editGenFromSimQueue env recoveryPageId (epEdited ep)
        gen `shouldSatisfy` (> 0)

        -- Requirement 4: stamped with that generation, a writeback is
        -- accepted again — so fluid keeps flowing from the edited state
        -- instead of the chunk being frozen out forever.
        let resumed = perturbed (epEdited ep) gen (epAfter ep)
        deliverBatch env recoveryPageId [resumed]

        edited ← chunkAt (epState ep) (epEdited ep)
        lcFluidMap edited          `shouldBe` fwFluid resumed
        lcTerrainSurfaceMap edited `shouldBe` fwTerrain resumed
        lcSurfaceMap edited        `shouldBe` fwSurf resumed
        lcSideDeco edited          `shouldBe` fwSideDeco resumed

      it "fills the batch's ack MVar even when every writeback in it is \
         \dropped, so fast settle cannot hang" $ \env → do
        ep ← withEditedPage env ackPageId

        -- Both stale: the edited chunk's generation has moved on, and
        -- the untouched chunk's has not — so stamping the untouched one
        -- with a generation the page never issued makes it stale too.
        -- 'SimFastSettleAll' and the --dump fast-settle path block on
        -- this MVar (Sim.Thread's takeMVar), so requirement 5 is a
        -- deadlock guard, not a nicety.
        let allStale = [ perturbed (epEdited ep) 0 (epAfter ep)
                       , perturbed (epUntouched ep) 99 (epUntBefore ep)
                       ]
        ack ← newEmptyMVar
        sendWorldCommand env
            (WorldApplyFluids (FluidWritebackBatch ackPageId allStale (Just ack)))
        acked ← timeout ackTimeoutMicros (takeMVar ack)
        acked `shouldBe` Just ()

        -- …and nothing in either chunk moved on the way.
        edited ← chunkAt (epState ep) (epEdited ep)
        lcFluidMap edited          `shouldBe` lcFluidMap (epAfter ep)
        lcTerrainSurfaceMap edited `shouldBe` lcTerrainSurfaceMap (epAfter ep)
        lcSurfaceMap edited        `shouldBe` lcSurfaceMap (epAfter ep)
        lcSideDeco edited          `shouldBe` lcSideDeco (epAfter ep)
        untouched ← chunkAt (epState ep) (epUntouched ep)
        lcFluidMap untouched `shouldBe` lcFluidMap (epUntBefore ep)
        lcSideDeco untouched `shouldBe` lcSideDeco (epUntBefore ep)

-- | Requirement 6, registered under the @persistence contract@ describe
--   (see @Spec.hs@) so that gate's @--match@ covers it.
--
--   This is the consequence the issue was actually reported for: the
--   overwrite alone is transient, but a save taken while it stands makes
--   it permanent, because 'World.Thread.Command.Save.WriteWorld.appendFluidSnapshot'
--   reads @lcFluidMap@ straight out of @wsTilesRef@ and APPENDS the
--   result behind the player's own edits, which replay
--   ('World.Edit.Apply.replayEdits', a left fold) then applies last.
--
--   So the window is forced exactly as above and then a REAL
--   'World.Command.Types.WorldSave' is taken inside it — no snapshot
--   literal, no hand-built edit list.
saveSpec ∷ SpecWith EngineEnv
saveSpec =
    describe "a save taken in the stale-writeback window (#1596)" $
      it "records the EDITED fluid in its trailing appendFluidSnapshot \
         \edits, and replaying that page's log reproduces the edit" $ \env →
        let cleanup = removePathForcibly ("saves/" <> T.unpack saveSlotName)
        in (`finally` cleanup) $ do
            cleanup
            ep ← withEditedPage env savePageId

            let stale = FluidWriteback
                    { fwCoord    = epEdited ep
                    , fwEditGen  = 0
                    , fwFluid    = lcFluidMap (epBefore ep)
                    , fwTerrain  = lcTerrainSurfaceMap (epBefore ep)
                    , fwSurf     = lcSurfaceMap (epBefore ep)
                    , fwSideDeco = lcSideDeco (epBefore ep)
                    }
            deliverBatch env savePageId [stale]

            sendWorldCommand env
                (WorldSave savePageId saveSlotName
                           "2026-08-23T00:00:00.000000Z" [] [] Nothing)
            waitForFile ("saves/" <> T.unpack saveSlotName <> "/world.synworld")

            logger ← readIORef (loggerRef env)
            (sd, _, _) ← loadWorld logger saveSlotName HS.empty HS.empty
                ⌦ either (\(_, e) → expectationFailure (T.unpack e)
                                 ≫ error "unreachable") pure

            wps ← case find ((≡ savePageId) ∘ wpsPageId) (sdWorlds sd) of
                Nothing → expectationFailure "saved page missing from SaveData"
                            ≫ error "unreachable"
                Just w  → pure w

            let (fgx, fgy) = epFluidTile ep
                chunkLog   = HM.lookupDefault [] (epEdited ep) (wpsEdits wps)
                snapshots  = [ ft | WeSetFluidSnapshot sx sy ft _ ← chunkLog
                                  , (sx, sy) ≡ (fgx, fgy) ]
                cleared    = [ () | WeClearFluidSnapshot sx sy ← chunkLog
                                  , (sx, sy) ≡ (fgx, fgy) ]

            -- The player's own edit is still in the log, and the
            -- trailing snapshot for that tile agrees with it rather than
            -- reinstating the pre-edit dry column.
            chunkLog `shouldSatisfy`
                elem (WeSetFluidTile fgx fgy River)
            snapshots `shouldBe` [River]
            cleared   `shouldBe` []

            -- …and the whole log, replayed onto the freshly generated
            -- chunk exactly as chunk loading does it, reproduces the
            -- edit rather than the pre-edit state.
            let replayed = replayEdits (wpsEdits wps) (epBefore ep)
            fmap fcType (lcFluidMap replayed V.! epFluidIdx ep)
                `shouldBe` Just River
            lcTerrainSurfaceMap replayed VU.! epTerrIdx ep
                `shouldBe` lcTerrainSurfaceMap (epAfter ep) VU.! epTerrIdx ep

-- Fixture ---------------------------------------------------------

-- | Generate a private w8 page, choose two loaded chunks and two tiles,
--   commit one live fluid edit and one live terrain edit, and return the
--   chunk states either side of them.
--
--   The barrier between "sent" and "after" is an EMPTY acked writeback
--   batch: the world queue is FIFO, so its ack proves both edit commands
--   have been handled without asserting anything about their result
--   (which is what the examples are for). Reading @wsTilesRef@ before
--   the edits are provably in would make every later comparison
--   circular.
withEditedPage ∷ EngineEnv → WorldPageId → IO EditedPage
withEditedPage env pageId = do
    sendWorldCommand env (WorldInit pageId 45 8 3 Nothing)
    ws ← waitForWorldInit env pageId 120
    td ← readIORef (wsTilesRef ws)

    (editedCoord, before, fluidLocal) ← case dryTileChunk td of
        Just found → pure found
        Nothing    → expectationFailure
                        "fixture: no loaded chunk with a dry tile"
                     ≫ error "unreachable"
    untouchedCoord ← case find (≢ editedCoord) (HM.keys (wtdChunks td)) of
        Just c  → pure c
        Nothing → expectationFailure "fixture: page has only one chunk"
                  ≫ error "unreachable"
    untBefore ← chunkAt ws untouchedCoord

    -- A second tile in the same chunk, so the terrain edit cannot be
    -- confused with the fluid one.
    let terrLocal  = (fst fluidLocal + 1, snd fluidLocal)
        fluidIdx   = columnIndex (fst fluidLocal) (snd fluidLocal)
        terrIdx    = columnIndex (fst terrLocal) (snd terrLocal)
        fluidTile  = chunkToGlobal editedCoord (fst fluidLocal) (snd fluidLocal)
        terrTile   = chunkToGlobal editedCoord (fst terrLocal) (snd terrLocal)

    sendWorldCommand env
        (WorldSetFluidTile pageId (fst fluidTile) (snd fluidTile) River)
    sendWorldCommand env
        (WorldAddTile pageId (fst terrTile) (snd terrTile) matLoam)
    -- FIFO barrier: an empty batch applies nothing and still acks.
    barrier ← newEmptyMVar
    sendWorldCommand env
        (WorldApplyFluids (FluidWritebackBatch pageId [] (Just barrier)))
    awaitAck barrier
    after ← chunkAt ws editedCoord

    -- The edits really landed and really changed the fields a writeback
    -- overwrites — otherwise "the stale batch was dropped" would be
    -- indistinguishable from "the batch was applied and changed nothing".
    fmap fcType (lcFluidMap after V.! fluidIdx) `shouldBe` Just River
    lcFluidMap before V.! fluidIdx `shouldBe` Nothing
    lcTerrainSurfaceMap after VU.! terrIdx
        `shouldBe` lcTerrainSurfaceMap before VU.! terrIdx + 1

    pure EditedPage
        { epState     = ws
        , epEdited    = editedCoord
        , epUntouched = untouchedCoord
        , epFluidTile = fluidTile
        , epFluidIdx  = fluidIdx
        , epTerrIdx   = terrIdx
        , epBefore    = before
        , epAfter     = after
        , epUntBefore = untBefore
        }

-- | First loaded chunk holding a dry column away from the chunk's east
--   edge. Dry, because the fluid edit has to CHANGE the column for
--   "dropped" and "applied" to be distinguishable; away from the east
--   edge, because the terrain edit takes the column one step east and
--   both edits must land in the SAME chunk for the per-chunk contract to
--   be the thing under test.
dryTileChunk ∷ WorldTileData → Maybe (ChunkCoord, LoadedChunk, (Int, Int))
dryTileChunk td = case mapMaybe dryIn (HM.elems (wtdChunks td)) of
    (found : _) → Just found
    []          → Nothing
  where
    dryIn lc = case filter (isDry lc) interiorLocals of
        (local : _) → Just (lcCoord lc, lc, local)
        []          → Nothing
    isDry lc (lx, ly) = case lcFluidMap lc V.! columnIndex lx ly of
        Nothing → True
        Just _  → False

-- | Every local column except the east edge, so @lx + 1@ stays inside
--   the chunk.
interiorLocals ∷ [(Int, Int)]
interiorLocals =
    [ (lx, ly) | ly ← [0 .. chunkSize - 1], lx ← [0 .. chunkSize - 2] ]

-- | A writeback that differs from @lc@ in ALL FOUR fields
--   'World.Thread.Command.applyOneWriteback' writes, so "was it applied?"
--   has an unambiguous answer either way.
perturbed ∷ ChunkCoord → Word64 → LoadedChunk → FluidWriteback
perturbed coord gen lc = FluidWriteback
    { fwCoord    = coord
    , fwEditGen  = gen
    , fwFluid    = V.replicate chunkCells (Just (FluidCell Lava 7))
    , fwTerrain  = VU.map (+ 11) (lcTerrainSurfaceMap lc)
    , fwSurf     = VU.map (+ 13) (lcSurfaceMap lc)
    , fwSideDeco = VU.replicate chunkCells 5
    }

-- | Send one batch and block until the world thread has handled it.
deliverBatch ∷ EngineEnv → WorldPageId → [FluidWriteback] → IO ()
deliverBatch env pageId writebacks = do
    ack ← newEmptyMVar
    sendWorldCommand env
        (WorldApplyFluids (FluidWritebackBatch pageId writebacks (Just ack)))
    awaitAck ack

ackTimeoutMicros ∷ Int
ackTimeoutMicros = 30 * 1000 * 1000

-- | Wait for a batch ack, failing the example rather than hanging the
--   suite if the world thread never fulfils it.
awaitAck ∷ MVar () → IO ()
awaitAck ack = do
    got ← timeout ackTimeoutMicros (takeMVar ack)
    case got of
        Just () → pure ()
        Nothing → expectationFailure
            "world thread never acked a WorldApplyFluids batch"

chunkAt ∷ WorldState → ChunkCoord → IO LoadedChunk
chunkAt ws coord = do
    td ← readIORef (wsTilesRef ws)
    case lookupChunk coord td of
        Just lc → pure lc
        Nothing → expectationFailure ("chunk not loaded: " ⧺ show coord)
                  ≫ error "unreachable"

-- | The live-edit generation the production edit handlers published for
--   this chunk, read off the REAL sim queue (nothing drains it here, so
--   every 'SimChunkEdited' the world thread wrote is still queued). The
--   last one wins: a sim draining in order would end holding that value
--   as 'Sim.State.Types.scsEditGen'.
editGenFromSimQueue ∷ EngineEnv → WorldPageId → ChunkCoord → IO Word64
editGenFromSimQueue env pageId coord = do
    cmds ← Q.flushQueue (simQueue env)
    let gens = [ g | SimChunkEdited p _ c g _ _ ← cmds
                   , p ≡ pageId, c ≡ coord ]
    case gens of
        [] → expectationFailure
                ("no SimChunkEdited on the sim queue for " ⧺ show coord)
             ≫ error "unreachable"
        _  → pure (foldl' max 0 gens)

waitForFile ∷ FilePath → IO ()
waitForFile path = go (300 ∷ Int)
  where
    go 0 = expectationFailure $ "save file never appeared: " ⧺ path
    go n = do
        exists ← doesFileExist path
        if exists then pure () else threadDelay 100000 ≫ go (n - 1)
