{-# LANGUAGE Strict #-}
-- | What the solidifying cell does to whatever was standing on it
--   (#2490, FR-3 of epic #2480).
--
--   #2485's own group proves the STONE. This one drives the same
--   hand-delivered commit against the same kind of real generated page
--   and grades what happened to the occupants: the unit that was
--   standing there, the corpse that already was, the item lying on the
--   tile, and — just as load-bearing — the neighbours and the
--   same-coordinate rows on another page that all of it must leave
--   alone.
--
--   Both production threads are real. The world thread is the live one
--   'Test.Headless.World.Solidification.deliver' hands a batch to; the
--   unit thread is not started by this harness, so its queue is drained
--   here by calling the production 'processAllUnitCommands' directly.
--   That is not a shortcut but the point of one example: draining BY
--   HAND opens the exact window a running unit thread only reaches by
--   chance — a unit walking onto the solidified tile after the stone
--   landed but before the kill was handled.
--
--   Run:
--   @cabal test synarchy-test-headless --test-options='--match "solidification occupants"'@
module Test.Headless.World.SolidificationOccupants (spec, pureSpec) where

import UPrelude
import Test.Hspec
import qualified Data.HashMap.Strict as HM
import qualified Data.Map.Strict as Map
import qualified Data.Sequence as Seq
import qualified Data.Vector.Unboxed as VU
import Data.Foldable (toList)
import Data.IORef (atomicModifyIORef', newIORef, readIORef, writeIORef)
import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.MVar
    (MVar, newEmptyMVar, putMVar, takeMVar, tryTakeMVar)
import System.Timeout (timeout)
import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TVar (writeTVar)

import qualified Engine.Core.Queue as Q
import Engine.Asset.Handle (TextureHandle(..))
import Engine.Core.Capability.WorldSim
    (WorldSimCapability(..), toWorldSimCapability, withPlayerIntentHeld)
import Engine.Core.State (EngineEnv(..))
import Engine.PlayerEvent
    (CategoryCfg(..), PlayerEvent(..), StoredEvent(..), emptyEventStore)
import Engine.PlayerEvent.Emit (readEventLog)
import Combat.Types (CombatEvent(..))
import Item.Ground (GroundItems(..), spawnGroundItem)
import Item.Types (ItemInstance(..))
import Sim.Fluid.Reaction (ReactionResult(..), SolidProduct(..))
import Unit.Faction (Faction(..))
import Unit.Command.Types (UnitCommand(..))
import Unit.Sim.Types
import Unit.Thread.Command (processAllUnitCommands)
import Unit.Thread.Command.Spawn (handleUnitSpawnCommand)
import Unit.Thread.Command.Solidify
    ( SolidifySeams(..), handleUnitSolidifyOccupantsCommandWith
    , productionSolidifySeams, solidificationDeathCause
    , solidificationEventCategory )
import Unit.Transfer (TransferBatch(..), TransferEndpoint(..))
import Unit.Transfer.Orders
    (TransferOrders(..), addTransferOrder, emptyTransferOrders)
import Unit.Types
import World.Chunk.Admit (pageIncarnation)
import World.Material (MaterialRegistry, matLoam)
import World.Thread.Command.Reaction
    ( ReactionAdmission(..), ReactionCommitSeams(..), admitReaction
    , commitReactionsWith, productionReactionCommitSeams )
import World.Edit.Apply (applyEdit)
import World.Edit.Types (WorldEdit(..))
import World.Flora.Designation (replaceChunkForgettingFlora)
import World.Generate.Coordinates (canonicalTile)
import World.Plate.Wrap (worldWidthTiles)
import World.Reaction.Occupants (occupiesTile)
import World.Types
import Test.Headless.Harness (getWorldState, sendWorldCommand)
import Test.Headless.World.Solidification
    ( LivePage(..), ackTimeoutMicros, chunkAt, deliver, livePage
    , liveEvent )

-- * Fixtures ---------------------------------------------------------

-- | Every example gets its OWN generated page, for the same reason
--   #2485's do: a page carries the live-edit generations admission
--   compares against, so sharing one would let an earlier example's
--   commit decide a later one's. (Re-initialising an id would not help
--   — that is a same-id re-incarnation, #2477, which retires the units
--   the example just spawned.)
victimPageId, corpsePageId, moverPageId, otherPageId ∷ WorldPageId
reactingPageId, crossingPageId, aliasPageId, stalePageId ∷ WorldPageId
replayPageId, delayPageId, selectionPageId, emptyTilePageId ∷ WorldPageId
mirrorPageId, floodedPageId ∷ WorldPageId
seamPageId, movedPageId, retiredPageId, orphanPageId ∷ WorldPageId
racedPageId, evictedPageId, climbingPageId ∷ WorldPageId
pausedPageId, coherentPageId, lateItemPageId ∷ WorldPageId
victimPageId    = WorldPageId "occupants_victim_w8"
corpsePageId    = WorldPageId "occupants_corpse_w8"
moverPageId     = WorldPageId "occupants_corpseheight_w8"
otherPageId     = WorldPageId "occupants_other_w8"
reactingPageId  = WorldPageId "occupants_reacting_w8"
crossingPageId  = WorldPageId "occupants_crossing_w8"
aliasPageId     = WorldPageId "occupants_alias_w8"
stalePageId     = WorldPageId "occupants_stale_w8"
replayPageId    = WorldPageId "occupants_replay_w8"
delayPageId     = WorldPageId "occupants_delay_w8"
selectionPageId = WorldPageId "occupants_selection_w8"
emptyTilePageId = WorldPageId "occupants_emptytile_w8"
mirrorPageId    = WorldPageId "occupants_mirror_w8"
floodedPageId   = WorldPageId "occupants_flooded_w8"
seamPageId      = WorldPageId "occupants_seam_w8"
movedPageId     = WorldPageId "occupants_moved_w8"
retiredPageId   = WorldPageId "occupants_retired_w8"
orphanPageId    = WorldPageId "occupants_orphan_w8"
racedPageId     = WorldPageId "occupants_raced_w8"
evictedPageId   = WorldPageId "occupants_evicted_w8"
climbingPageId  = WorldPageId "occupants_climbing_w8"
pausedPageId    = WorldPageId "occupants_paused_w8"
coherentPageId  = WorldPageId "occupants_coherent_w8"
lateItemPageId  = WorldPageId "occupants_lateitem_w8"

-- | The world size every 'livePage' here generates at, and therefore
--   the one its u-aliases are computed against.
fixtureWorldSize ∷ Int
fixtureWorldSize = 8

-- | The local cell every example reacts on, and one beside it.
reactCell, nextCell ∷ (Int, Int)
reactCell = (4, 4)
nextCell  = (5, 4)

-- | A body-less def: nothing here rolls stats, equips anything, or
--   needs an item registry, because none of that decides whether a unit
--   standing on molten rock dies.
occupantDef ∷ UnitDef
occupantDef = UnitDef
    { udName = occupantDefName, udNamePool = Nothing
    , udDisplayName = Nothing
    , udTexture = TextureHandle 0, udPortrait = Nothing
    , udDirSprites = Map.empty
    , udBaseWidth = 0, udMaxSpeed = 1.0, udRunThreshold = 0.6
    , udAnimations = HM.empty, udStateAnims = HM.empty, udEagerStats = False
    , udStatTemplates = HM.empty, udBodyTemplates = HM.empty
    , udSkillTemplates = HM.empty, udKnowledgeTemplates = HM.empty
    , udStartingInventory = []
    , udEquipmentClass = Nothing, udStartingEquipment = HM.empty
    , udStartingAccessories = []
    , udBodyParts = []
    , udNaturalResistance = defaultNaturalResistance
    , udNaturalWeapon = Nothing, udModifiers = [] }

occupantDefName ∷ Text
occupantDefName = "solidification_dummy"

-- | Register the def and clear both event streams, so every assertion
--   below is about what THIS example produced. The engine is shared by
--   the whole group ('aroundAll'), so the streams are not empty just
--   because nothing has happened yet.
prepare ∷ EngineEnv → IO ()
prepare env = do
    atomicModifyIORef' (unitManagerRef env) $ \um →
        (um { umDefs = HM.insert occupantDefName occupantDef (umDefs um) }, ())
    clearStreams env

clearStreams ∷ EngineEnv → IO ()
clearStreams env = do
    writeIORef (injuryEventsRef env) Seq.empty
    atomically $ writeTVar (eventStoreRef env) emptyEventStore

-- | Spawn one unit through the REAL 'UnitSpawn' handler and drain it in.
spawnAt ∷ EngineEnv → WorldState → WorldPageId → Word32 → (Int, Int) → Int
        → IO UnitId
spawnAt env ws pageId raw (gx, gy) z = do
    epoch ← pageIncarnation ws
    let uid = UnitId raw
    Q.writeQueue (unitQueue env)
        (UnitSpawn uid occupantDefName
                   (fromIntegral gx + 0.5) (fromIntegral gy + 0.5) z
                   FactionPlayer pageId epoch)
    drainUnits env
    pure uid

drainUnits ∷ EngineEnv → IO ()
drainUnits env = void (processAllUnitCommands env (utsRef env))

-- | Put a ground item at a tile's centre, through the production
--   inserter, and answer its page-local id.
dropItemAt ∷ WorldState → Text → (Int, Int) → IO Int
dropItemAt ws name (gx, gy) =
    atomicModifyIORef' (wsGroundItemsRef ws) $
        spawnGroundItem (fixtureItem name)
            (fromIntegral gx + 0.5) (fromIntegral gy + 0.5)

fixtureItem ∷ Text → ItemInstance
fixtureItem name = ItemInstance
    { iiDefName = name, iiCurrentFill = 0, iiQuality = 100
    , iiCondition = 100, iiWeight = 1.0, iiSharpness = 100
    , iiContents = [], iiInstanceId = 0, iiTemp = Nothing
    , iiBulk = Just 1, iiStorage = Nothing }

holdsItem ∷ WorldState → Int → IO Bool
holdsItem ws gid = HM.member gid ∘ gisItems <$> readIORef (wsGroundItemsRef ws)

selectItem ∷ WorldState → Int → IO ()
selectItem ws gid = atomicModifyIORef' (wsCursorRef ws) $ \cs →
    (cs { selectedGroundItem = Just gid }, ())

selectedItem ∷ WorldState → IO (Maybe Int)
selectedItem ws = selectedGroundItem <$> readIORef (wsCursorRef ws)

-- | The global tile one local cell of a page's lava chunk names.
tileOf ∷ LivePage → (Int, Int) → (Int, Int)
tileOf lp (lx, ly) =
    let ChunkCoord cx cy = lpLava lp
    in (cx * chunkSize + lx, cy * chunkSize + ly)

-- | Deliver a commit that solidifies @cell@ of this page's lava chunk,
--   claiming the generation that chunk ACTUALLY stands at — so it is
--   admitted, and so a second reaction on a page that has already
--   committed one is not refused for a reason the example is not about.
react ∷ EngineEnv → LivePage → WorldPageId → (Int, Int) → IO ()
react env lp pageId cell =
    currentGeneration lp ≫= reactFrom env lp pageId cell

-- | The live-edit generation this page's lava chunk stands at, which is
--   what a sim half computed against it would have been stamped with.
currentGeneration ∷ LivePage → IO Word64
currentGeneration lp =
    HM.lookupDefault 0 (lpLava lp)
        <$> readIORef (wsChunkEditGenRef (lpState lp))

-- | 'react' with an explicit claimed generation, so an example can
--   deliver a STALE result on purpose.
reactFrom ∷ EngineEnv → LivePage → WorldPageId → (Int, Int) → Word64 → IO ()
reactFrom env lp pageId cell gen =
    deliver env (lpState lp) pageId []
        [ ReactionResult [(lpLava lp, gen)]
                         [liveEvent (lpLava lp) cell (lpLava lp) SolidBasalt] ]

-- * Read-back helpers -------------------------------------------------

-- | @-Wincomplete-uni-patterns@ is on, so an absent row fails the
--   example by name instead of pattern-matching.
require ∷ String → Maybe a → IO a
require label = maybe (expectationFailure label ≫ error "unreachable") pure

simStateOf ∷ EngineEnv → UnitId → IO UnitSimState
simStateOf env uid = do
    uts ← readIORef (utsRef env)
    require ("no sim state for " ⧺ show uid) (HM.lookup uid (utsSimStates uts))

instanceOf ∷ EngineEnv → UnitId → IO UnitInstance
instanceOf env uid = do
    um ← readIORef (unitManagerRef env)
    require ("no instance for " ⧺ show uid) (HM.lookup uid (umInstances um))

poseOf ∷ EngineEnv → UnitId → IO Pose
poseOf env uid = usPose <$> simStateOf env uid

tileUnderUnit ∷ UnitSimState → (Int, Int)
tileUnderUnit ss = (floor (usRealX ss), floor (usRealY ss))

-- | The injury stream's @"death"@ rows for one unit.
deathsFor ∷ EngineEnv → UnitId → IO [CombatEvent]
deathsFor env (UnitId raw) = do
    evs ← readIORef (injuryEventsRef env)
    pure [ e | e ← toList evs, ceKind e ≡ "death", ceTarget e ≡ Just raw ]

deathCausesFor ∷ EngineEnv → UnitId → IO [Maybe Text]
deathCausesFor env uid = map (HM.lookup "cause" ∘ cePayload) <$> deathsFor env uid

-- | The event-log rows attributed to one unit.
logRowsFor ∷ EngineEnv → UnitId → IO [PlayerEvent]
logRowsFor env (UnitId raw) = do
    rows ← readEventLog env
    pure [ seEvent r | r ← rows, peUid (seEvent r) ≡ Just raw ]

-- | Raise one column by @n@ z, through the production add-tile edit —
--   the same 'World.Edit.Apply.applyEdit' a player's own add-tile
--   makes, so the terrain and surface maps move together.
--
--   Written straight into the page for 'floodTileDeep'\'s reason: the
--   world thread is idle here, and this is fixture setup rather than
--   anything under test.
raiseCellBy ∷ WorldState → ChunkCoord → (Int, Int) → (Int, Int) → Int
            → IO ()
raiseCellBy ws coord (gx, gy) cell n = forM_ [1 .. n] $ \_ → do
    lc ← chunkAt ws coord
    let lc' = applyEdit (WeAddTile gx gy matLoam) lc
    when (terrainTopAt lc' cell ≡ terrainTopAt lc cell) $
        expectationFailure "fixture: the add-tile did not raise the column"
    replaceChunkForgettingFlora ws lc lc'

terrainTopAt ∷ LoadedChunk → (Int, Int) → Int
terrainTopAt lc (lx, ly) = lcTerrainSurfaceMap lc VU.! columnIndex lx ly

-- | Move one unit's authoritative position to a tile's centre, WITHOUT
--   going through a command — this is the unit thread's own state, and
--   an example that wants a unit somewhere mid-drain has no queue left
--   to ask.
placeAt ∷ EngineEnv → UnitId → (Int, Int) → IO ()
placeAt env uid (gx, gy) = atomicModifyIORef' (utsRef env) $ \uts →
    ( uts { utsSimStates = HM.adjust
              (\ss → ss { usRealX = fromIntegral gx + 0.5
                        , usRealY = fromIntegral gy + 0.5 })
              uid (utsSimStates uts) }, () )

-- | Move one unit's RENDER-FACING position without touching its
--   authoritative one — the state the unit thread's own
--   @publishToRender@ would reconcile on its next tick.
--
--   No production path writes these two out of step on purpose; a
--   fixture does, because being up to one tick apart is exactly what
--   the mirror IS between publishes, and a selection reading the wrong
--   one of the pair is the defect this pins.
placeMirrorAt ∷ EngineEnv → UnitId → (Int, Int) → IO ()
placeMirrorAt env uid (gx, gy) =
    atomicModifyIORef' (unitManagerRef env) $ \um →
        ( um { umInstances = HM.adjust
                 (\inst → inst { uiGridX = fromIntegral gx + 0.5
                               , uiGridY = fromIntegral gy + 0.5 })
                 uid (umInstances um) }, () )

-- | Stand DEEP fluid over a column, so the page's resolved surface is
--   above its terrain top BOTH before the reaction and after it.
--
--   Deep on purpose. @world.setFluidTile@ places exactly one level, and
--   'World.Edit.Apply.applyEdit' displaces a cell the new stone reaches
--   — so a one-level flood would be gone by the time the correction
--   runs, and the case could not tell a terrain-top lookup from a
--   surface one. Three levels survive the single z the stone adds,
--   which is the retained-fluid state engine contracts §Fluid reaction
--   describes for an active chunk's solidified cell.
--
--   Applied through the production 'World.Edit.Apply.applyEdit' and the
--   production 'World.Flora.Designation.replaceChunkForgettingFlora',
--   so the fluid map and the surface map move together exactly as a
--   real edit moves them. Written straight into the page rather than
--   queued because there is no world command for an explicit fluid
--   SURFACE; the world thread is idle here (the page's init has been
--   waited for and no batch is in flight).
floodTileDeep ∷ WorldState → ChunkCoord → (Int, Int) → (Int, Int) → IO ()
floodTileDeep ws coord (gx, gy) cell = do
    lc ← chunkAt ws coord
    let top = terrainTopAt lc cell
        lc' = applyEdit (WeSetFluidSnapshot gx gy Lake (top + 3)) lc
    replaceChunkForgettingFlora ws lc lc'

surfaceAt ∷ LoadedChunk → (Int, Int) → Int
surfaceAt lc (lx, ly) = lcSurfaceMap lc VU.! columnIndex lx ly

-- | Drive the REAL commit for one solidification, running @between@
--   after the occupant snapshot and before the first stone.
--
--   The production path is 'World.Thread.Command.Reaction.commitReactions',
--   which is 'commitReactionsWith' with a no-op seam; this is the same
--   body with the hook filled in, not a reimplementation of it. It also
--   uses the production 'admitReaction' to decide the result, so the
--   case cannot pass by committing something the world thread would
--   have refused.
--
--   Called from the example's own thread rather than through the world
--   queue, because the point is to interleave a unit-position change
--   with the commit's internals. That is safe here for the reason
--   'floodTileDeep' is: the page's init has been waited for, no batch
--   is in flight, and the world thread touches nothing in between.
reactWithSeam ∷ EngineEnv → LivePage → WorldPageId → (Int, Int)
              → (ReactionCommitSeams → ReactionCommitSeams) → IO ()
reactWithSeam env lp pageId cell withSeam = do
    let ws = lpState lp
    registry ← readIORef (wsMaterialRegistryRef (toWorldSimCapability env))
    gens ← readIORef (wsChunkEditGenRef ws)
    td ← readIORef (wsTilesRef ws)
    logger ← readIORef (loggerRef env)
    let rr = ReactionResult
                 [(lpLava lp, HM.lookupDefault 0 (lpLava lp) gens)]
                 [liveEvent (lpLava lp) cell (lpLava lp) SolidBasalt]
    case admitReaction (registry ∷ MaterialRegistry) gens td rr of
        ReactionRefused why →
            expectationFailure ("fixture: the result was refused: " ⧺ show why)
        ReactionAdmitted evs →
            commitReactionsWith (withSeam productionReactionCommitSeams)
                env logger pageId ws [(rr, evs)]

-- | Re-initialise a page under the SAME id, which is what retires the
--   previous incarnation's rows (#2476/#2477).
--
--   'waitForWorldInit' alone is not enough here: a page already stands
--   under this id, so it would answer with the OUTGOING one the instant
--   it was asked. The wait is therefore for a genuinely different
--   'WorldState' object to be registered, which is what the lifecycle
--   transition installs — and the transition is also what retires the
--   previous incarnation's unit rows, so that is exactly the edge the
--   caller needs to have passed.
reinitPage ∷ EngineEnv → WorldPageId → WorldState → IO WorldState
reinitPage env pageId old = do
    sendWorldCommand env (WorldInit pageId 45 8 3 Nothing)
    awaitReplacement (3000 ∷ Int)
  where
    awaitReplacement 0 =
        expectationFailure "fixture: the page was never re-initialised"
            ≫ error "unreachable"
    awaitReplacement n = do
        mWs ← getWorldState env pageId
        case mWs of
            Just ws | not (sameWorldState ws old) → do
                phase ← readIORef (wsLoadPhaseRef ws)
                if phase ≡ LoadDone
                    then pure ws
                    else threadDelay 10000 ≫ awaitReplacement (n - 1)
            _ → threadDelay 10000 ≫ awaitReplacement (n - 1)

-- | Are these two handles the same live page object? Compared by one of
--   its own 'IORef's, since a 'WorldState' is a record of refs with no
--   identity of its own.
sameWorldState ∷ WorldState → WorldState → Bool
sameWorldState a b = wsTilesRef a ≡ wsTilesRef b

-- | Drain the unit queue by hand, running the real solidification
--   handler with a seam for every 'UnitSolidifyOccupants' it holds.
--
--   The same recipe 'Test.Headless.World.PageIncarnation' uses for
--   'Unit.Thread.Command.Spawn.SpawnSeams': flush the queue and call
--   the production handler with one hook filled in, so what runs is its
--   real body and not a restatement of it.
drainUnitsWithSolidifySeam ∷ EngineEnv → IO () → IO ()
drainUnitsWithSolidifySeam env between = do
    held ← Q.flushQueue (unitQueue env)
    let seams = productionSolidifySeams { seamAfterEpochCheck = between }
    forM_ held $ \cmd → case cmd of
        UnitSolidifyOccupants pageId epoch gx gy top victims →
            handleUnitSolidifyOccupantsCommandWith seams env (utsRef env)
                pageId epoch gx gy top victims
        other → handleOther other
  where
    handleOther cmd = do
        Q.writeQueue (unitQueue env) cmd
        drainUnits env

-- | Drop one chunk from a page's tiles, as an eviction does.
--
--   Written straight into the page for 'floodTileDeep'\'s reason: the
--   world thread is idle here, and this is fixture setup standing in
--   for @updateChunkLoading@ rather than anything under test. What
--   matters to the case is only that the lookup the handler would make
--   can no longer be answered.
evictChunkFrom ∷ WorldState → ChunkCoord → IO ()
evictChunkFrom ws coord = atomicModifyIORef' (wsTilesRef ws) $ \td →
    (td { wtdChunks = HM.delete coord (wtdChunks td) }, ())

-- | Turn the @pause@ flag of one notification category on or off, the
--   way a player's own @config\/notifications.local.yaml@ override does,
--   answering the previous setting so an example can restore it.
setCategoryPause ∷ EngineEnv → Text → Bool → IO (Maybe Bool)
setCategoryPause env category wanted =
    atomicModifyIORef' (notificationCfgRef env) $ \cfg →
        case HM.lookup category cfg of
            Nothing  → (cfg, Nothing)
            Just cat → ( HM.insert category cat { ccPause = wanted } cfg
                       , Just (ccPause cat) )

-- | Is @lock@ free right now? Takes and immediately returns it, so a
--   probe never steals it from a real holder for longer than the check.
lockIsFree ∷ MVar () → IO Bool
lockIsFree lock = do
    got ← tryTakeMVar lock
    case got of
        Nothing → pure False
        Just () → putMVar lock () ≫ pure True

-- | Poll @p@ every 10 ms until it holds or the attempts run out.
pollUntil ∷ Int → IO Bool → IO Bool
pollUntil 0 _ = pure False
pollUntil n p = do
    ok ← p
    if ok then pure True else threadDelay 10000 ≫ pollUntil (n - 1) p

-- * The live group ----------------------------------------------------

spec ∷ SpecWith EngineEnv
spec = describe "solidification occupants (#2490)" $ do

    it "kills the unit standing on the solidifying tile, files it on \
       \both streams, destroys the ground item there, and leaves the \
       \neighbours of both alone" $ \env → do
        prepare env
        lp ← livePage env victimPageId
        let ws     = lpState lp
            doomed = tileOf lp reactCell
            beside = tileOf lp nextCell
        before ← chunkAt ws (lpLava lp)
        victim ← spawnAt env ws victimPageId 9001 doomed
                         (terrainTopAt before reactCell)
        bystander ← spawnAt env ws victimPageId 9002 beside
                            (terrainTopAt before nextCell)
        doomedItem ← dropItemAt ws "doomed_item" doomed
        keptItem   ← dropItemAt ws "kept_item" beside

        react env lp victimPageId reactCell
        drainUnits env

        -- Requirement 1: the terminal state, and the SAME one every
        -- other death produces.
        poseOf env victim `shouldReturn` Dead
        ss ← simStateOf env victim
        usTarget ss `shouldBe` Nothing
        usLocalPath ss `shouldBe` []
        usTransitionUntil ss `shouldBe` Nothing
        usState ss `shouldBe` Idle
        inst ← instanceOf env victim
        uiPose inst `shouldBe` "dead"

        -- Requirement 2, first half: the injury stream, with a cause
        -- naming the reaction and the tile.
        deathCausesFor env victim
            `shouldReturn` [Just (uncurry solidificationDeathCause doomed)]
        -- …and second half: the player event log, attributed to the
        -- unit, at the reaction PAGE's coordinates and under a category
        -- whose shipped default logs it.
        rows ← logRowsFor env victim
        map peCategory rows `shouldBe` [solidificationEventCategory]
        map peCoords rows `shouldBe` [Just doomed]
        map peSourcePage rows `shouldBe` [Just (unWorldPageId victimPageId)]

        -- Requirement 3: the item at the tile is gone, and only it.
        holdsItem ws doomedItem `shouldReturn` False
        holdsItem ws keptItem `shouldReturn` True

        -- Requirement 6: a unit one tile over is untouched — not dead,
        -- and not moved.
        poseOf env bystander `shouldReturn` Standing
        bys ← simStateOf env bystander
        tileUnderUnit bys `shouldBe` beside
        (length <$> deathsFor env bystander) `shouldReturn` 0
        logRowsFor env bystander `shouldReturn` []

    it "lifts the corpse clear of the new stone in BOTH the simulation \
       \and the render-facing state, instead of leaving it buried" $
      \env → do
        prepare env
        lp ← livePage env moverPageId
        let ws     = lpState lp
            doomed = tileOf lp reactCell
        before ← chunkAt ws (lpLava lp)
        let baseZ = terrainTopAt before reactCell
        victim ← spawnAt env ws moverPageId 9101 doomed baseZ

        react env lp moverPageId reactCell
        drainUnits env

        after ← chunkAt ws (lpLava lp)
        let newTop = terrainTopAt after reactCell
        newTop `shouldBe` baseZ + 1
        ss   ← simStateOf env victim
        inst ← instanceOf env victim
        -- Requirement 4: at or above the new terrain top, on both
        -- surfaces, so neither presentation shows a body inside stone.
        (usGridZ ss ≥ newTop) `shouldBe` True
        usRealZ ss `shouldBe` fromIntegral (usGridZ ss)
        uiGridZ inst `shouldBe` usGridZ ss
        uiRealZ inst `shouldBe` usRealZ ss
        -- …and it is a height correction, not a teleport: the body
        -- stays where it fell.
        tileUnderUnit ss `shouldBe` doomed

    it "kills a unit that is MID-CROSSING the tile it is currently \
       \over, which the ordinary re-ground lift would have skipped" $
      \env → do
        prepare env
        lp ← livePage env crossingPageId
        let ws     = lpState lp
            doomed = tileOf lp reactCell
            goal   = tileOf lp nextCell
        before ← chunkAt ws (lpLava lp)
        victim ← spawnAt env ws crossingPageId 9201 doomed
                         (terrainTopAt before reactCell)
        -- A walking unit, still over the doomed tile and heading off it.
        -- 'Unit.Thread.Command.Lifecycle.handleUnitReGroundCommand'
        -- requires @usState ≡ Idle@ and so would ignore exactly this
        -- unit; occupancy here is the floor of the position, nothing
        -- else.
        atomicModifyIORef' (utsRef env) $ \uts →
            ( uts { utsSimStates = HM.adjust
                      (\ss → ss { usState = Walking
                                , usTarget = Just MoveTarget
                                    { mtTargetX = fromIntegral (fst goal)
                                    , mtTargetY = fromIntegral (snd goal)
                                    , mtSpeed   = 1.0
                                    , mtHazard  = FallProhibited } })
                      victim (utsSimStates uts) }, () )

        react env lp crossingPageId reactCell
        drainUnits env
        poseOf env victim `shouldReturn` Dead
        (length <$> deathsFor env victim) `shouldReturn` 1

    it "leaves an already-dead occupant terminal and files no second \
       \death, while still lifting it clear of the stone" $ \env → do
        prepare env
        lp ← livePage env corpsePageId
        let ws     = lpState lp
            doomed = tileOf lp reactCell
        before ← chunkAt ws (lpLava lp)
        corpse ← spawnAt env ws corpsePageId 9301 doomed
                         (terrainTopAt before reactCell)
        Q.writeQueue (unitQueue env) (UnitKill corpse)
        drainUnits env
        -- Everything the FIRST death produced is discarded, so nothing
        -- below can pass on its rows.
        clearStreams env

        react env lp corpsePageId reactCell
        drainUnits env

        poseOf env corpse `shouldReturn` Dead
        (length <$> deathsFor env corpse) `shouldReturn` 0
        logRowsFor env corpse `shouldReturn` []
        after ← chunkAt ws (lpLava lp)
        ss ← simStateOf env corpse
        (usGridZ ss ≥ terrainTopAt after reactCell) `shouldBe` True

    it "retires the dying carrier's transfer orders, keeps a selection \
       \of an item it did not remove, and clears one of an item it did" $
      \env → do
        prepare env
        lp ← livePage env selectionPageId
        let ws     = lpState lp
            doomed = tileOf lp reactCell
            beside = tileOf lp nextCell
        before ← chunkAt ws (lpLava lp)
        victim ← spawnAt env ws selectionPageId 9401 doomed
                         (terrainTopAt before reactCell)
        doomedItem ← dropItemAt ws "doomed_item" doomed
        keptItem   ← dropItemAt ws "kept_item" beside
        -- A durable order this carrier is walking (#1253): a corpse
        -- never ticks again, so nothing else would ever retire it.
        orders0 ← require "the fixture could not mint a transfer order" $
            addTransferOrder victim
                (TransferBatch (EndpointUnit victim)
                               (EndpointUnit (UnitId 9402)) [])
                emptyTransferOrders
        writeIORef (wsTransferOrdersRef ws) (fst orders0)
        -- The selection names the SURVIVING item, so the first commit
        -- has to leave it standing.
        selectItem ws keptItem

        react env lp selectionPageId reactCell
        drainUnits env

        holdsItem ws doomedItem `shouldReturn` False
        selectedItem ws `shouldReturn` Just keptItem
        orders ← readIORef (wsTransferOrdersRef ws)
        HM.size (trosOrders orders) `shouldBe` 0

        -- Now solidify the tile the SELECTED item is on: that removal
        -- has to take the selection with it.
        react env lp selectionPageId nextCell
        drainUnits env
        holdsItem ws keptItem `shouldReturn` False
        selectedItem ws `shouldReturn` Nothing

    it "destroys a unit and an item whose coordinates are a u-ALIAS of \
       \the solidified tile" $ \env → do
        prepare env
        lp ← livePage env aliasPageId
        let ws     = lpState lp
            doomed = tileOf lp reactCell
            step   = worldWidthTiles fixtureWorldSize `div` 2
            alias  = (fst doomed + step, snd doomed - step)
        -- The fixture is only meaningful if that really is an alias of
        -- the same physical tile, and not the tile itself.
        canonicalTile fixtureWorldSize (fst alias) (snd alias)
            `shouldBe` doomed
        (alias ≢ doomed) `shouldBe` True
        before ← chunkAt ws (lpLava lp)
        victim ← spawnAt env ws aliasPageId 9501 alias
                         (terrainTopAt before reactCell)
        aliasItem ← dropItemAt ws "aliased_item" alias

        react env lp aliasPageId reactCell
        drainUnits env

        poseOf env victim `shouldReturn` Dead
        holdsItem ws aliasItem `shouldReturn` False

    it "leaves a unit and an item at the SAME coordinates on another \
       \page completely alone" $ \env → do
        prepare env
        reacting ← livePage env reactingPageId
        other    ← livePage env otherPageId
        let doomed = tileOf reacting reactCell
        before ← chunkAt (lpState reacting) (lpLava reacting)
        -- Same global tile, different page. Nothing about this row is
        -- standing on the edited tile (#1593).
        elsewhere ← spawnAt env (lpState other) otherPageId 9601 doomed
                            (terrainTopAt before reactCell)
        otherItem ← dropItemAt (lpState other) "other_page_item" doomed

        react env reacting reactingPageId reactCell
        drainUnits env

        poseOf env elsewhere `shouldReturn` Standing
        (length <$> deathsFor env elsewhere) `shouldReturn` 0
        holdsItem (lpState other) otherItem `shouldReturn` True

    it "destroys nothing when the result is REFUSED as stale" $ \env → do
        prepare env
        lp ← livePage env stalePageId
        let ws     = lpState lp
            doomed = tileOf lp reactCell
        before ← chunkAt ws (lpLava lp)
        survivor ← spawnAt env ws stalePageId 9701 doomed
                           (terrainTopAt before reactCell)
        item ← dropItemAt ws "survivor_item" doomed

        -- Generation 7 is one this page never issued, so admission
        -- refuses the whole result before anything is applied.
        reactFrom env lp stalePageId reactCell 7
        drainUnits env

        after ← chunkAt ws (lpLava lp)
        terrainTopAt after reactCell `shouldBe` terrainTopAt before reactCell
        poseOf env survivor `shouldReturn` Standing
        (length <$> deathsFor env survivor) `shouldReturn` 0
        logRowsFor env survivor `shouldReturn` []
        holdsItem ws item `shouldReturn` True

    it "destroys nothing on a REPLAY of an event the page already \
       \committed, so a newcomer to the stone is not caught by it" $
      \env → do
        prepare env
        lp ← livePage env replayPageId
        let ws     = lpState lp
            doomed = tileOf lp reactCell
        before ← chunkAt ws (lpLava lp)
        first ← spawnAt env ws replayPageId 9801 doomed
                        (terrainTopAt before reactCell)
        -- The generation the delivered result was computed against,
        -- captured BEFORE the commit that consumes it.
        computedAt ← currentGeneration lp
        react env lp replayPageId reactCell
        drainUnits env
        poseOf env first `shouldReturn` Dead

        -- Someone walks onto the fresh stone, and the SAME result is
        -- delivered again. Its claimed generation is the pre-commit
        -- one, which the commit itself advanced, so admission refuses
        -- it and nothing about the newcomer changes.
        after ← chunkAt ws (lpLava lp)
        newcomer ← spawnAt env ws replayPageId 9802 doomed
                           (terrainTopAt after reactCell)
        newItem ← dropItemAt ws "newcomer_item" doomed
        clearStreams env
        reactFrom env lp replayPageId reactCell computedAt
        drainUnits env

        poseOf env newcomer `shouldReturn` Standing
        (length <$> deathsFor env newcomer) `shouldReturn` 0
        holdsItem ws newItem `shouldReturn` True

    it "kills the EDIT-TIME occupant across unit-queue delay, and not \
       \whoever is standing there when the kill is finally handled" $
      \env → do
        prepare env
        lp ← livePage env delayPageId
        let ws     = lpState lp
            doomed = tileOf lp reactCell
            beside = tileOf lp nextCell
        before ← chunkAt ws (lpLava lp)
        caught ← spawnAt env ws delayPageId 9901 doomed
                         (terrainTopAt before reactCell)
        latecomer ← spawnAt env ws delayPageId 9902 beside
                            (terrainTopAt before nextCell)

        -- The stone lands. The unit queue is deliberately NOT drained.
        react env lp delayPageId reactCell

        -- Now the two swap tiles, exactly as they could while the unit
        -- thread was still busy with an earlier tick.
        placeAt env caught beside
        placeAt env latecomer doomed

        drainUnits env

        -- The one that was there when the stone formed died even though
        -- it has since left; the one that arrived afterwards is
        -- untouched.
        poseOf env caught `shouldReturn` Dead
        (length <$> deathsFor env caught) `shouldReturn` 1
        poseOf env latecomer `shouldReturn` Standing
        (length <$> deathsFor env latecomer) `shouldReturn` 0

        -- …but "untouched" means NOT KILLED, not "left inside the
        -- rock". This path replaced the ordinary re-ground, so if the
        -- handler settled only its victims the survivor would stand a
        -- level below the stone it is standing on, for ever.
        after ← chunkAt ws (lpLava lp)
        let stoneTop = terrainTopAt after reactCell
        stoneTop `shouldBe` terrainTopAt before reactCell + 1
        lateSs ← simStateOf env latecomer
        lateInst ← instanceOf env latecomer
        tileUnderUnit lateSs `shouldBe` doomed
        (usGridZ lateSs ≥ stoneTop) `shouldBe` True
        (usRealZ lateSs ≥ fromIntegral stoneTop) `shouldBe` True
        (uiGridZ lateInst ≥ stoneTop) `shouldBe` True
        (uiRealZ lateInst ≥ fromIntegral stoneTop) `shouldBe` True

    it "commits a solidification with no occupants at all without \
       \killing anything or filing an event" $ \env → do
        prepare env
        lp ← livePage env emptyTilePageId
        let ws     = lpState lp
            beside = tileOf lp nextCell
        before ← chunkAt ws (lpLava lp)
        bystander ← spawnAt env ws emptyTilePageId 9911 beside
                            (terrainTopAt before nextCell)

        react env lp emptyTilePageId reactCell
        drainUnits env

        after ← chunkAt ws (lpLava lp)
        terrainTopAt after reactCell
            `shouldBe` terrainTopAt before reactCell + 1
        poseOf env bystander `shouldReturn` Standing
        readEventLog env `shouldReturn` []
        (Seq.length <$> readIORef (injuryEventsRef env)) `shouldReturn` 0

    it "judges occupancy on the AUTHORITATIVE sim position, not on the \
       \render mirror that lags it by up to a tick" $ \env → do
        prepare env
        lp ← livePage env mirrorPageId
        let ws     = lpState lp
            doomed = tileOf lp reactCell
            beside = tileOf lp nextCell
        before ← chunkAt ws (lpLava lp)
        -- `caught` has really crossed onto the doomed tile; its mirror
        -- still shows the neighbour it came from.
        caught ← spawnAt env ws mirrorPageId 9921 beside
                         (terrainTopAt before nextCell)
        placeAt env caught doomed
        placeMirrorAt env caught beside
        -- `spared` has really crossed OFF it; its mirror still shows the
        -- doomed tile.
        spared ← spawnAt env ws mirrorPageId 9922 doomed
                         (terrainTopAt before reactCell)
        placeAt env spared beside
        placeMirrorAt env spared doomed
        -- The fixture is only meaningful while the two genuinely
        -- disagree, which is what a mid-tick mover looks like.
        caughtInst ← instanceOf env caught
        (floor (uiGridX caughtInst), floor (uiGridY caughtInst))
            `shouldBe` beside
        sparedInst ← instanceOf env spared
        (floor (uiGridX sparedInst), floor (uiGridY sparedInst))
            `shouldBe` doomed

        react env lp mirrorPageId reactCell
        drainUnits env

        poseOf env caught `shouldReturn` Dead
        (length <$> deathsFor env caught) `shouldReturn` 1
        poseOf env spared `shouldReturn` Standing
        (length <$> deathsFor env spared) `shouldReturn` 0

    it "rests the corpse on the STONE, not on fluid the solidified cell \
       \still holds above it" $ \env → do
        prepare env
        lp ← livePage env floodedPageId
        let ws     = lpState lp
            doomed = tileOf lp reactCell
        floodTileDeep ws (lpLava lp) doomed reactCell
        before ← chunkAt ws (lpLava lp)
        let baseTop = terrainTopAt before reactCell
        -- The whole point of the case: the page's RESOLVED surface here
        -- stands above its terrain top, so a correction that read the
        -- surface would float the body.
        (surfaceAt before reactCell > baseTop) `shouldBe` True
        victim ← spawnAt env ws floodedPageId 9931 doomed baseTop

        react env lp floodedPageId reactCell
        drainUnits env

        after ← chunkAt ws (lpLava lp)
        let stoneTop = terrainTopAt after reactCell
        stoneTop `shouldBe` baseTop + 1
        (surfaceAt after reactCell > stoneTop) `shouldBe` True
        ss ← simStateOf env victim
        inst ← instanceOf env victim
        poseOf env victim `shouldReturn` Dead
        -- EXACTLY the stone top: at it, so it is not buried, and no
        -- higher, so it is not floating on the water above it.
        usGridZ ss `shouldBe` stoneTop
        uiGridZ inst `shouldBe` stoneTop

    it "judges the victim set on the snapshot taken BEFORE the stone, \
       \so a unit that moves while the commit runs cannot change it" $
      \env → do
        prepare env
        lp ← livePage env seamPageId
        let ws     = lpState lp
            doomed = tileOf lp reactCell
            beside = tileOf lp nextCell
        before ← chunkAt ws (lpLava lp)
        caught ← spawnAt env ws seamPageId 9941 doomed
                         (terrainTopAt before reactCell)
        latecomer ← spawnAt env ws seamPageId 9942 beside
                            (terrainTopAt before nextCell)

        -- The interleaving a running unit thread could produce, made
        -- deterministic: the two swap tiles after the occupants have
        -- been read and before a single stone has landed. Everything
        -- the commit does afterwards — the edits, the item removals,
        -- the generation advance, both refreshes — happens with the
        -- units in their SWAPPED positions.
        reactWithSeam env lp seamPageId reactCell $ \seams →
            seams { seamAfterOccupantSnapshot = do
                        placeAt env caught beside
                        placeAt env latecomer doomed }
        drainUnits env

        poseOf env caught `shouldReturn` Dead
        (length <$> deathsFor env caught) `shouldReturn` 1
        poseOf env latecomer `shouldReturn` Standing
        (length <$> deathsFor env latecomer) `shouldReturn` 0

    it "settles a victim that moved before the drain against ITS OWN \
       \column, not against the column the stone went into" $ \env → do
        prepare env
        lp ← livePage env movedPageId
        let ws     = lpState lp
            doomed = tileOf lp reactCell
        -- A neighbour raised HIGHER than the stone will leave the
        -- reacting column, built rather than searched for: a generated
        -- page is flat enough around any given cell that looking for a
        -- natural step makes the case depend on the seed.
        raiseCellBy ws (lpLava lp) (tileOf lp nextCell) nextCell 3
        before ← chunkAt ws (lpLava lp)
        victim ← spawnAt env ws movedPageId 9951 doomed
                         (terrainTopAt before reactCell)

        react env lp movedPageId reactCell
        -- Moves after the commit and before the drain — the window the
        -- carried victim set exists to survive, and the one the
        -- correction has to notice.
        placeAt env victim (tileOf lp nextCell)
        drainUnits env

        after ← chunkAt ws (lpLava lp)
        let stoneTop = terrainTopAt after reactCell
            ownTop   = terrainTopAt after nextCell
        -- The fixture is only meaningful while the two columns differ,
        -- and differ in the direction that BURIES rather than floats.
        (ownTop > stoneTop) `shouldBe` True
        poseOf env victim `shouldReturn` Dead
        ss ← simStateOf env victim
        inst ← instanceOf env victim
        -- Clear of ITS OWN ground. Correcting against the stone column
        -- would have left it at `stoneTop`, two z inside the ground it
        -- is actually lying on.
        usGridZ ss `shouldBe` ownTop
        uiGridZ inst `shouldBe` ownTop

    it "skips a victim whose page was re-initialised under the same id \
       \before the kill was drained, killing and reporting nothing" $
      \env → do
        prepare env
        lp ← livePage env retiredPageId
        let ws     = lpState lp
            doomed = tileOf lp reactCell
        before ← chunkAt ws (lpLava lp)
        doomedUnit ← spawnAt env ws retiredPageId 9961 doomed
                             (terrainTopAt before reactCell)

        react env lp retiredPageId reactCell
        -- The page is REPLACED while the kill sits queued. #2476 retires
        -- the previous incarnation's instances at once and leaves their
        -- sim rows to a UnitClearPage that is queued BEHIND our message,
        -- so a handler checking only the sim state would kill and report
        -- an orphan — at coordinates that now mean the new world.
        _ ← reinitPage env retiredPageId ws
        clearStreams env
        drainUnits env

        (length <$> deathsFor env doomedUnit) `shouldReturn` 0
        logRowsFor env doomedUnit `shouldReturn` []

    it "skips a victim the ROSTER no longer holds, even while the page \
       \itself is still the one the commit ran against" $ \env → do
        prepare env
        lp ← livePage env orphanPageId
        let ws     = lpState lp
            doomed = tileOf lp reactCell
        before ← chunkAt ws (lpLava lp)
        orphan ← spawnAt env ws orphanPageId 9971 doomed
                         (terrainTopAt before reactCell)

        react env lp orphanPageId reactCell
        -- EXACTLY the window `registerPageIncarnation` opens: its
        -- `retirePageUnits` drops the instances immediately, under the
        -- lifecycle lock, and leaves the sim rows to a `UnitClearPage`
        -- that can still be queued BEHIND this kill. Reproduced by hand
        -- so the roster half of the check is what decides the outcome,
        -- with the page — and therefore its incarnation — unchanged.
        atomicModifyIORef' (unitManagerRef env) $ \um →
            (um { umInstances = HM.delete orphan (umInstances um) }, ())
        clearStreams env
        drainUnits env

        -- Nothing was killed and nothing was reported about a row that
        -- no longer exists.
        (length <$> deathsFor env orphan) `shouldReturn` 0
        logRowsFor env orphan `shouldReturn` []
        uts ← readIORef (utsRef env)
        map usPose (maybeToList (HM.lookup orphan (utsSimStates uts)))
            `shouldBe` [Standing]

    it "refuses the kill when the page is replaced AFTER the handler's \
       \first epoch check, not only before the drain" $ \env → do
        prepare env
        lp ← livePage env racedPageId
        let ws     = lpState lp
            doomed = tileOf lp reactCell
        before ← chunkAt ws (lpLava lp)
        victim ← spawnAt env ws racedPageId 9981 doomed
                         (terrainTopAt before reactCell)

        react env lp racedPageId reactCell
        clearStreams env
        -- The schedule the FIRST check cannot cover: the replacement
        -- lands after the handler has accepted the epoch and before it
        -- acts. Only the revalidation inside the lifecycle lock refuses
        -- it. (Run on this thread, as PageIncarnation's own spawn cases
        -- run theirs — the world thread is idle and the seam is the
        -- interleaving under test.)
        replaced ← newIORef False
        drainUnitsWithSolidifySeam env $ do
            _ ← reinitPage env racedPageId ws
            writeIORef replaced True
        readIORef replaced `shouldReturn` True

        (length <$> deathsFor env victim) `shouldReturn` 0
        logRowsFor env victim `shouldReturn` []

    it "still lifts the corpse clear of the stone when the reaction's \
       \chunk is EVICTED before the kill is drained" $ \env → do
        prepare env
        lp ← livePage env evictedPageId
        let ws     = lpState lp
            doomed = tileOf lp reactCell
        before ← chunkAt ws (lpLava lp)
        let baseZ = terrainTopAt before reactCell
        victim ← spawnAt env ws evictedPageId 9991 doomed baseZ

        react env lp evictedPageId reactCell
        after ← chunkAt ws (lpLava lp)
        let stoneTop = terrainTopAt after reactCell
        stoneTop `shouldBe` baseZ + 1
        -- The world thread's own tick can evict the reaction chunk
        -- inside this queue's unbounded delay. The handler's live
        -- lookup then answers nothing, and only the height the commit
        -- CARRIED can keep the body out of the stone the durable edit
        -- will replay.
        evictChunkFrom ws (lpLava lp)
        drainUnits env

        poseOf env victim `shouldReturn` Dead
        ss ← simStateOf env victim
        inst ← instanceOf env victim
        usGridZ ss `shouldBe` stoneTop
        uiGridZ inst `shouldBe` stoneTop

    it "lifts a victim killed MID-PULL-UP by its continuous height too, \
       \not only by the grid z its climb had already committed" $
      \env → do
        prepare env
        lp ← livePage env climbingPageId
        let ws     = lpState lp
            doomed = tileOf lp reactCell
        before ← chunkAt ws (lpLava lp)
        let baseZ = terrainTopAt before reactCell
        victim ← spawnAt env ws climbingPageId 9995 doomed baseZ
        -- A one-level pull-up in flight: `usGridZ` has already committed
        -- to the destination, while `usRealZ` is still lerping up from
        -- the start and stands BELOW it. That is the shape
        -- `Unit.Thread.Movement` leaves during a climb, and the reason
        -- the two fields exist separately.
        atomicModifyIORef' (utsRef env) $ \uts →
            ( uts { utsSimStates = HM.adjust
                      (\ss → ss { usGridZ  = baseZ + 1
                                , usRealZ  = fromIntegral baseZ
                                , usState  = TransitioningTo Standing
                                , usClimbToTile =
                                    Just ( fromIntegral (fst doomed)
                                         , fromIntegral (snd doomed)
                                         , baseZ + 1 ) })
                      victim (utsSimStates uts) }, () )
        atomicModifyIORef' (unitManagerRef env) $ \um →
            ( um { umInstances = HM.adjust
                     (\inst → inst { uiGridZ = baseZ + 1
                                   , uiRealZ = fromIntegral baseZ })
                     victim (umInstances um) }, () )

        react env lp climbingPageId reactCell
        drainUnits env

        after ← chunkAt ws (lpLava lp)
        let stoneTop = terrainTopAt after reactCell
        stoneTop `shouldBe` baseZ + 1
        poseOf env victim `shouldReturn` Dead
        ss ← simStateOf env victim
        inst ← instanceOf env victim
        -- The grid z was ALREADY at the new top, so a correction that
        -- only looked at it would have declined to touch anything — and
        -- the kill has just cleared the climb endpoints and the
        -- transition timer, so no later tick would ever finish the lerp.
        usGridZ ss `shouldBe` stoneTop
        (usRealZ ss ≥ fromIntegral stoneTop) `shouldBe` True
        (uiRealZ inst ≥ fromIntegral stoneTop) `shouldBe` True
        uiGridZ inst `shouldBe` stoneTop

    it "files the deaths with the lifecycle lock RELEASED, so a category \
       \the player set to pause cannot take a second lock under it" $
      \env → do
        prepare env
        let worldSim = toWorldSimCapability env
        -- The shipped unit_warning settings do not pause, but they are
        -- player-editable, and a handler may not depend on that.
        restore ← setCategoryPause env solidificationEventCategory True
        lp ← livePage env pausedPageId
        let ws     = lpState lp
            doomed = tileOf lp reactCell
        before ← chunkAt ws (lpLava lp)
        victim ← spawnAt env ws pausedPageId 9997 doomed
                         (terrainTopAt before reactCell)
        react env lp pausedPageId reactCell

        -- Another actor holds the pause epoch — the mutex
        -- `World.Pause.imposePause` needs — for as long as this example
        -- wants it. Anything that reaches for it now blocks.
        epochHeld ← newEmptyMVar
        releaseEpoch ← newEmptyMVar
        _ ← forkIO $ withPlayerIntentHeld worldSim $ \_ → do
                putMVar epochHeld ()
                takeMVar releaseEpoch
        takeMVar epochHeld

        drained ← newEmptyMVar
        _ ← forkIO (drainUnits env ≫ putMVar drained ())

        -- The kill happens INSIDE the lifecycle lock, so a dead pose
        -- means the handler has reached the point where the old
        -- arrangement would emit — still holding that lock — and block
        -- on the epoch the thread above owns.
        killed ← pollUntil 500 ((≡ Dead) <$> poseOf env victim)
        killed `shouldBe` True
        -- So the lifecycle lock must be free while the epoch is still
        -- held. Emitting under it would keep it held for exactly as
        -- long as this example chooses to hold the epoch.
        free ← pollUntil 200 (lockIsFree (wsPageLifecycleLock worldSim))
        free `shouldBe` True

        putMVar releaseEpoch ()
        finished ← timeout ackTimeoutMicros (takeMVar drained)
        finished `shouldBe` Just ()
        -- …and the report still happened, pause and all.
        (length <$> deathsFor env victim) `shouldReturn` 1
        (length <$> logRowsFor env victim) `shouldReturn` 1
        readIORef (enginePausedRef env) `shouldReturn` True

        -- Leave the shared engine as it was found.
        _ ← setCategoryPause env solidificationEventCategory
                (fromMaybe False restore)
        writeIORef (enginePausedRef env) False

    it "reads the roster and the positions under ONE lock, so a spawn \
       \commit cannot land between them" $ \env → do
        prepare env
        lp ← livePage env coherentPageId
        let ws     = lpState lp
            doomed = tileOf lp reactCell
        before ← chunkAt ws (lpLava lp)
        let baseZ = terrainTopAt before reactCell
        occupant ← spawnAt env ws coherentPageId 9985 doomed baseZ
        epoch ← pageIncarnation ws

        -- The interleaving the two-read snapshot used to admit: a spawn
        -- commit lands between the roster read and the position read,
        -- so the newcomer is missing from the roster already taken —
        -- and if the mover is also stepped off the cell in that window,
        -- a tile occupied throughout yields no victims at all.
        --
        -- The spawn rides the REAL commit handler, which takes the same
        -- page lifecycle lock the snapshot now holds across both reads,
        -- so it must NOT be able to complete while that section is open.
        landedInside ← newIORef True
        spawnDone ← newEmptyMVar
        reactWithSeam env lp coherentPageId reactCell $ \seams →
            seams { seamInsideOccupantSnapshot = do
                      _ ← forkIO $ do
                            handleUnitSpawnCommand env (utsRef env)
                                (UnitId 9986) occupantDefName
                                (fromIntegral (fst doomed) + 0.5)
                                (fromIntegral (snd doomed) + 0.5)
                                baseZ FactionPlayer coherentPageId epoch
                            putMVar spawnDone ()
                      -- Long enough for an unlocked spawn to have
                      -- finished several times over.
                      threadDelay 300000
                      um ← readIORef (unitManagerRef env)
                      writeIORef landedInside
                          (HM.member (UnitId 9986) (umInstances um))
                      -- …and the mover leaves, which is the other half
                      -- of the interleaving.
                      placeAt env occupant (tileOf lp nextCell) }

        -- The spawn was still blocked on the lock when the roster had
        -- already been read, so no ADDITION straddled the two reads.
        -- (That is the whole of what the lock buys here. A concurrent
        -- REMOVAL still can straddle them — `UnitDestroy` retires the
        -- two stores in two separate writes, in this same order — and
        -- what filters the stale name it leaves behind is the consumer:
        -- the kill handler's own roster recheck, covered by the "skips
        -- a victim the ROSTER no longer holds" example above.)
        readIORef landedInside `shouldReturn` False
        finished ← timeout ackTimeoutMicros (takeMVar spawnDone)
        finished `shouldBe` Just ()

        drainUnits env
        -- The newcomer was not an occupant when the stone was decided,
        -- and it is not killed for one.
        poseOf env (UnitId 9986) `shouldReturn` Standing
        (length <$> deathsFor env (UnitId 9986)) `shouldReturn` 0

    it "destroys the ground items captured at the victim cutoff, and \
       \leaves one dropped onto the cell after it" $ \env → do
        prepare env
        lp ← livePage env lateItemPageId
        let ws     = lpState lp
            doomed = tileOf lp reactCell
        caught ← dropItemAt ws "caught_item" doomed
        lateGid ← newIORef (-1)

        -- The item set is captured with the units, BEFORE the first
        -- stone. A drop landing after that cutoff — here, between the
        -- snapshot and the edits, but equally any time before the
        -- removal runs — was never on the cell this reaction caught.
        -- Scanning the live map at removal time instead would take it.
        reactWithSeam env lp lateItemPageId reactCell $ \seams →
            seams { seamAfterOccupantSnapshot = do
                      gid ← dropItemAt ws "late_item" doomed
                      writeIORef lateGid gid }
        drainUnits env

        late ← readIORef lateGid
        (late ≢ caught) `shouldBe` True
        holdsItem ws caught `shouldReturn` False
        holdsItem ws late `shouldReturn` True
        -- …and the stone really did land on that tile, so the survival
        -- is about the cutoff and not about nothing having happened.
        after ← chunkAt ws (lpLava lp)
        before ← pure (lpBefore lp)
        (terrainTopAt after reactCell > terrainTopAt before reactCell)
            `shouldBe` True

-- * The occupancy predicate itself ------------------------------------

-- | Pinned directly as well as through the live commits above, because
--   those can only reach it with the positions a real generated page
--   happens to produce — and the rule it encodes (floor, canonical
--   frame, page already narrowed) has cases those do not cover.
pureSpec ∷ Spec
pureSpec = describe "solidification occupants (#2490) — occupancy" $ do
    let w = fixtureWorldSize
        step = worldWidthTiles w `div` 2
    it "reads a sub-tile position as the tile it is the floor of" $ do
        occupiesTile w (3, 4) 3.9 4.1 `shouldBe` True
        occupiesTile w (3, 4) 4.0 4.1 `shouldBe` False
    it "uses floor, not truncation, on a negative coordinate" $ do
        occupiesTile w (-1, 0) (-0.5) 0.5 `shouldBe` True
        occupiesTile w (0, 0) (-0.5) 0.5 `shouldBe` False
    it "matches a u-alias of the tile, named from either side" $ do
        occupiesTile w (3, 4) (fromIntegral (3 + step) + 0.5)
                              (fromIntegral (4 - step) + 0.5)
            `shouldBe` True
        occupiesTile w (3 + step, 4 - step) 3.5 4.5 `shouldBe` True
    it "does not match a genuinely different tile" $
        occupiesTile w (3, 4) 5.5 4.5 `shouldBe` False
