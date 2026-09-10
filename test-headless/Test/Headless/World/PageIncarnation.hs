{-# LANGUAGE Strict #-}
{-# LANGUAGE OverloadedStrings #-}
-- | "Page incarnation entity teardown" (#2476): destroying ONE world
--   page, or re-initialising a live page id, retires everything that
--   incarnation owned in the global unit and building managers — and
--   retires nothing the replacement owns.
--
--   Before #2476 neither path touched those managers at all. A page id
--   is a reusable logical NAME, so the replacement inherited the
--   previous incarnation's units, unit selection and sim state, its
--   buildings, building selection, destruction effects and outstanding
--   footprint reservations, all still keyed to the name it had just
--   taken over.
--
--   The fix is not a bare page filter, and these examples are shaped
--   around why. Teardown stays QUEUE-ordered (#58), so a clear drains
--   after admissions that were already in flight — and, on the init
--   paths, after admissions made against the REPLACEMENT, which reuse
--   the same page name. A page-only filter erases those too: a
--   replacement's reservation would be deleted before its spawn could
--   consume it, and 'Building.Reservation.commitFootprint' would then
--   refuse the very placement the claim was taken for. So the boundary
--   is drawn by ID instead: one shared lifecycle lock
--   ('Engine.Core.State.pageLifecycleLock') linearises every entity
--   admission against every page lifecycle transition, and each queued
--   clear carries the allocator reading that transition took as an
--   EXCLUSIVE cutoff.
--
--   Fixture choices that carry weight:
--
--   * __Every admission goes through the production Lua verb.__
--     @unit.spawn@, @building.spawn@ and @power.placeNode@ are the only
--     three sites that allocate an entity id, and they are what the
--     cutoff has to classify. An id invented by this module would
--     describe an admission no production path ever made.
--   * __Every transition goes through its production handler.__
--     'handleWorldDestroyCommand', 'handleWorldInitCommand' and
--     'handleWorldInitArenaCommand' — never a hand-written
--     @UnitClearPage@ except in the two deliberate no-op cases, which
--     are about the HANDLER's tolerance rather than the enqueue.
--   * __The drain is held.__ The engine here runs no worker threads, so
--     a queued clear stays queued until an example drains it
--     deliberately. That is what makes "admit a replacement while the
--     clear is still outstanding" a controlled state instead of a race.
--   * __The lock is proven to be taken, not merely present.__ The
--     'lockSpec' group holds the mutex on this thread and shows each
--     admission and each transition blocking on it, then completing
--     once it is released. Nothing single-threaded can otherwise tell a
--     locked transition from an unlocked one.
--
--   Run just this gate: @cabal test synarchy-test-headless
--   --test-options='--match "Page incarnation entity teardown"'@.
module Test.Headless.World.PageIncarnation (spec) where

import UPrelude
import Test.Hspec
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU
import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.MVar
    (MVar, newEmptyMVar, putMVar, takeMVar, tryTakeMVar)
import Control.Exception (SomeException, finally, try)
import Data.IORef (atomicModifyIORef', newIORef, readIORef, writeIORef)
import Data.List (sort)

import Building.Schema
import Building.Command.Types (BuildingCommand(..))
import Building.Thread.Command (processAllBuildingCommands)
import Building.Types
    ( BuildingDef(..), BuildingId(..), BuildingInstance(..)
    , BuildingManager(..), DestructionClip(..), DestructionEffect(..)
    , FootprintReservation(..), emptyBuildingManager )
import Engine.Asset.Handle (TextureHandle(..))
import Engine.Core.Capability.Building (toBuildingCapability)
import Engine.Core.Capability.ContentRegistriesView
    (toContentRegistriesViewCapability)
import Engine.Core.Capability.UnitCombat
    (UnitCombatCapability(..), toUnitCombatCapability)
import Engine.Core.Capability.WorldSim (toWorldSimCapability)
import Engine.Core.Init (EngineInitResult(..))
import Engine.Core.State (EngineEnv(..))
import Engine.Core.Thread (ThreadControl(..))
import qualified Engine.Core.Queue as Q
import Engine.Scripting.Lua.API (registerLuaAPI)
import Engine.Scripting.Lua.Thread (createLuaBackendState)
import Engine.Scripting.Lua.Thread.Console (executeDebugLua)
import Engine.Scripting.Lua.Types (LuaBackendState(..))
import Item.Types (ItemInstance(..))
import Power.Base (PowerNodeSpec(..))
import Power.Types (PowerNodes(..), emptyPowerNodes)
import Structure.Types (emptyChunkStructures)
import Test.Headless.Harness.Isolation (withIsolatedResourceRoot)
import Test.Headless.Harness.Log (initializeEngineHeadlessQuiet)
import Unit.Command.Types (UnitCommand(..))
import Unit.Direction (Direction(..))
import Unit.Faction (Faction(..))
import Unit.Sim.Types (UnitThreadState(..))
import Unit.Thread.Command (processAllUnitCommands)
import Unit.Types
    ( BodyPart(..), UnitDef(..), UnitId(..), UnitInstance(..)
    , UnitManager(..), defaultNaturalResistance, emptyUnitManager )
import World.Chunk.Types
    (ChunkCoord(..), ColumnTiles(..), LoadedChunk(..), chunkSize)
import World.Flora.Types (emptyFloraChunkData)
import World.Fluid.Types (emptyIceMap)
import World.Generate.Types (WorldGenParams(..), defaultWorldGenParams)
import World.Page.Types (WorldPageId(..))
import World.State.Types
    ( WorldManager(..), WorldState(..), emptyWorldManager, emptyWorldState
    , settleSelectionProjection )
import World.Thread.Command (handleWorldCommand)
import World.Thread.Command.Basic
    (handleWorldDestroyAllCommand, handleWorldDestroyCommand)
import World.Thread.Command.Init
    (handleWorldInitArenaCommand, handleWorldInitCommand)
import World.Thread.Command.UI (handleWorldShowCommand)
import World.Tile.Types (WorldTileData(..))

-- * Fixture identity

-- | The page under test: destroyed and re-initialised over and over.
incPage ∷ WorldPageId
incPage = WorldPageId "inc_page"

-- | A second live page that no example ever tears down. Everything it
--   owns must survive every transition applied to 'incPage' — the
--   page-scoping half of the filter, which a cutoff alone would not
--   provide.
keepPage ∷ WorldPageId
keepPage = WorldPageId "inc_keep"

-- | An id no page ever holds, for the "clear that matches nothing"
--   no-op case.
absentPage ∷ WorldPageId
absentPage = WorldPageId "inc_absent"

-- | The u-wrap world size both fixture pages use. Identical on purpose,
--   so an anchor names the same canonical tile on either one.
worldSize ∷ Int
worldSize = 8

-- | Anchors far enough apart that no two 1x1 placements below ever
--   contend for a tile, and all well inside the loaded chunk.
oldTile, claimTile, effectTile, lateTile, boundTile, powerTile ∷ (Int, Int)
oldTile    = (2, 2)
claimTile  = (4, 2)
effectTile = (6, 2)
lateTile   = (2, 4)
boundTile  = (4, 4)
powerTile  = (6, 4)

unitTile ∷ (Int, Int)
unitTile = (3, 3)

-- * Definitions

unitDefName, buildingDefName, panelDefName ∷ Text
unitDefName     = "inc_unit"
buildingDefName = "inc_hut"
panelDefName    = "inc_panel"

minimalUnitDef ∷ UnitDef
minimalUnitDef = UnitDef
    { udName = unitDefName, udNamePool = Nothing, udDisplayName = Nothing
    , udTexture = TextureHandle 0, udPortrait = Nothing
    , udDirSprites = Map.empty
    , udBaseWidth = 0, udMaxSpeed = 1.0, udRunThreshold = 0.6
    , udAnimations = HM.empty, udStateAnims = HM.empty, udEagerStats = False
    , udStatTemplates = HM.empty, udBodyTemplates = HM.empty
    , udSkillTemplates = HM.empty, udKnowledgeTemplates = HM.empty
    , udStartingInventory = []
    , udEquipmentClass = Nothing, udStartingEquipment = HM.empty
    , udStartingAccessories = []
    , udBodyParts =
        [ BodyPart
            { bpId = "torso", bpName = "torso", bpParent = Nothing
            , bpVital = False, bpAreaWeight = 1.0, bpTacticalValue = 0.5
            , bpBleedFactor = 1.0, bpHeightLow = 0, bpHeightHigh = 1
            , bpLayers = [], bpTargetable = True, bpDepth = 0.0
            , bpAffectsLocomotion = False, bpAffectsBalance = False } ]
    , udNaturalResistance = defaultNaturalResistance
    , udNaturalWeapon = Nothing, udModifiers = [] }

hutDef ∷ BuildingDef
hutDef = BuildingDef
    { bdName            = buildingDefName
    , bdDisplayName     = buildingDefName
    , bdCategory        = "Test"
    , bdDescription     = ""
    , bdTextures        = legacyAssets (TextureHandle 0)
    , bdIconTexture     = TextureHandle 0
    , bdTileW           = 1
    , bdTileH           = 1
    , bdPlacement       = "flat_ground"
    , bdIsStarting      = False
    , bdRace            = "acolyte"
    , bdSpriteAnchor    = "diamond_bottom"
    , bdBuildWork       = 0
    , bdMaterials       = HM.empty
    , bdStorageCapacity = 0
    , bdOperations      = []
    , bdAnimations      = HM.empty
    , bdRoleAnims       = Map.empty
    , bdVisualClass     = FreestandingInstallation
    , bdPowerDrain      = 0
    , bdPowerNode       = Nothing
    }

-- | The power-item building. @power.placeNode@ is the third admission
--   site, and it only reaches the lifecycle lock for a def that
--   actually declares a node.
panelDef ∷ BuildingDef
panelDef = hutDef { bdName = panelDefName, bdDisplayName = panelDefName
                  , bdPowerNode = Just (PowerNodeSource 400) }

-- | The unit that supplies 'panelDefName' to @power.placeNode@. Seeded
--   directly rather than spawned, because it must exist BEFORE any
--   example's own admissions so its id can never be confused with one.
supplierUid ∷ UnitId
supplierUid = UnitId 1

supplierUnit ∷ UnitInstance
supplierUnit = (mkUnitInstance incPage)
    { uiInventory = [ panelItem 1, panelItem 2, panelItem 3, panelItem 4 ] }

panelItem ∷ Word64 → ItemInstance
panelItem iid = ItemInstance
    { iiDefName = panelDefName, iiCurrentFill = 0, iiQuality = 100
    , iiCondition = 100, iiWeight = 1.0, iiSharpness = 100
    , iiContents = [], iiInstanceId = iid, iiTemp = Nothing
    , iiBulk = Just 1, iiStorage = Nothing }

mkUnitInstance ∷ WorldPageId → UnitInstance
mkUnitInstance page = UnitInstance
    { uiDefName = unitDefName, uiName = "", uiPage = page
    , uiTexture = TextureHandle 0, uiDirSprites = Map.empty
    , uiBaseWidth = 0, uiGridX = 3, uiGridY = 3, uiGridZ = 0
    , uiRealZ = 0, uiFacing = DirS
    , uiCurrentAnim = "", uiAnimStart = 0, uiAnimReverse = False
    , uiActivity = "idle", uiPose = "standing", uiAnimStride = 1
    , uiStats = HM.singleton "carrying_capacity" 100
    , uiModifiers = HM.empty, uiSkills = HM.empty
    , uiKnowledge = HM.empty, uiInventory = [], uiEquipment = HM.empty
    , uiAccessories = [], uiFactionId = FactionPlayer, uiWounds = []
    , uiScars = [], uiImmuneResponse = 0, uiImmunities = HM.empty
    , uiBlood = 5.0, uiLastAttackerUid = Nothing, uiLastAttackerAt = 0
    , uiAnimOverride = "", uiFrozen = False, uiForceLoop = False
    , uiClimbDest = Nothing, uiTrailState = Nothing
    }

-- * Terrain

flatChunkAt ∷ ChunkCoord → LoadedChunk
flatChunkAt coord =
    let area = chunkSize * chunkSize
        col  = ColumnTiles
            { ctStartZ = 0, ctMats = VU.singleton 1
            , ctSlopes = VU.singleton 0, ctVeg = VU.singleton 0 }
    in LoadedChunk
        { lcCoord             = coord
        , lcTiles             = V.replicate area col
        , lcSurfaceMap        = VU.replicate area 0
        , lcTerrainSurfaceMap = VU.replicate area 0
        , lcFluidMap          = V.replicate area Nothing
        , lcIceMap            = emptyIceMap
        , lcFlora             = emptyFloraChunkData
        , lcSideDeco          = VU.replicate area 0
        , lcWaterTableMap     = VU.replicate area 0
        , lcMagma             = Nothing
        , lcStructures        = emptyChunkStructures
        }

flatTiles ∷ WorldTileData
flatTiles =
    let chunks = [flatChunkAt (ChunkCoord 0 0), flatChunkAt (ChunkCoord 1 0)]
    in WorldTileData
        { wtdChunks    = HM.fromList [(lcCoord c, c) | c ← chunks]
        , wtdMaxChunks = length chunks }

-- * Scene

-- | Both pages live, 'incPage' the visible HEAD, both carrying the same
--   flat terrain; both managers reset to defs plus the one supplier
--   unit. Every other row an example reads is one that example itself
--   created through a production path.
resetScene ∷ EngineEnv → IO ()
resetScene env = do
    wsInc  ← emptyWorldState
    wsKeep ← emptyWorldState
    forM_ [wsInc, wsKeep] $ \ws → do
        writeIORef (wsTilesRef ws) flatTiles
        writeIORef (wsGenParamsRef ws) $ Just defaultWorldGenParams
            { wgpWorldSize = worldSize }
        writeIORef (wsPowerNodesRef ws) emptyPowerNodes
    writeIORef (worldManagerRef env) emptyWorldManager
        { wmWorlds  = [(incPage, wsInc), (keepPage, wsKeep)]
        , wmVisible = [incPage] }
    writeIORef (unitManagerRef env) emptyUnitManager
        { umDefs      = HM.singleton unitDefName minimalUnitDef
        , umInstances = HM.singleton supplierUid supplierUnit
        , umNextId    = 2 }
    writeIORef (buildingManagerRef env) emptyBuildingManager
        { bmDefs = HM.fromList [ (buildingDefName, hutDef)
                               , (panelDefName, panelDef) ] }
    writeIORef (ucUtsRef (toUnitCombatCapability env))
               UnitThreadState { utsSimStates = HM.empty }
    drainQueues env

-- | Empty every queue an example might inspect, discarding what was on
--   it. Used only between scenes.
drainQueues ∷ EngineEnv → IO ()
drainQueues env = do
    _ ← Q.flushQueue (unitQueue env)
    _ ← Q.flushQueue (buildingQueue env)
    _ ← Q.flushQueue (worldQueue env)
    _ ← Q.flushQueue (simQueue env)
    pure ()

-- * Seeding one incarnation

-- | Every category of row a page incarnation can own, on @page@:
--
--   1. a unit instance (spawned through @unit.spawn@ and drained),
--   2. that unit in 'umSelected',
--   3. that unit's 'utsSimStates' entry (the spawn handler's own),
--   4. a committed building (through @building.spawn@ and drained),
--   5. that building in 'bmSelected',
--   6. a destruction effect,
--   7. an outstanding footprint reservation whose spawn is never
--      drained, so it is still holding tiles when the transition runs.
--
--   Items 6 and 7 are installed against ids taken from the REAL
--   allocators, so they sit in the same monotonic sequence every other
--   row does and a cutoff classifies them the same way.
seedIncarnation ∷ EngineEnv → LuaBackendState → WorldPageId → IO ()
seedIncarnation env ls page = do
    ids ← admitIncarnation ls page
    drainEntities env
    finishIncarnation env ls page ids

-- | Seed BOTH fixture pages with one drain between the admissions and
--   the finishing touches.
--
--   Seeding them one after the other would not do: the second page's
--   drain would consume the first page's deliberately-outstanding
--   claim, and the scoping example below would then be comparing a
--   category of row that is not actually present. 'keepPage' is
--   finished FIRST so the single-slot building selection ends up naming
--   'incPage'\'s building — the row a teardown of the OTHER page must
--   leave alone.
seedBothPages ∷ EngineEnv → LuaBackendState → IO ()
seedBothPages env ls = do
    incIds  ← admitIncarnation ls incPage
    keepIds ← admitIncarnation ls keepPage
    drainEntities env
    finishIncarnation env ls keepPage keepIds
    finishIncarnation env ls incPage incIds

-- | The half that needs a drain: one unit and one building, both
--   admitted through their production verb.
admitIncarnation ∷ LuaBackendState → WorldPageId → IO (Word32, Word32)
admitIncarnation ls page = do
    uid ← spawnUnitOn ls page unitTile
    bid ← spawnBuildingOn ls buildingDefName oldTile page Nothing
    pure (uid, bid)

-- | The half that must NOT be drained afterwards: the two selections,
--   a destruction effect, and one claim left outstanding on purpose.
finishIncarnation ∷ EngineEnv → LuaBackendState → WorldPageId
                  → (Word32, Word32) → IO ()
finishIncarnation env ls page (uid, bid) = do
    atomicModifyIORef' (unitManagerRef env) $ \um →
        (um { umSelected = HS.insert (UnitId uid) (umSelected um) }, ())
    atomicModifyIORef' (buildingManagerRef env) $ \bm →
        (bm { bmSelected = Just (BuildingId bid) }, ())
    -- A destruction effect, keyed by an id this page really allocated.
    effectId ← allocBuildingId env
    atomicModifyIORef' (buildingManagerRef env) $ \bm →
        (bm { bmDestructions =
                HM.insert effectId (effectFor effectId page) (bmDestructions bm) }
        , ())
    -- An outstanding claim: admitted, never drained, still holding tiles.
    _ ← spawnBuildingOn ls buildingDefName claimTile page Nothing
    pure ()

-- | One 'BuildingId' straight from the production allocator.
allocBuildingId ∷ EngineEnv → IO BuildingId
allocBuildingId env = atomicModifyIORef' (buildingManagerRef env) $ \bm →
    (bm { bmNextId = bmNextId bm + 1 }, BuildingId (bmNextId bm))

effectFor ∷ BuildingId → WorldPageId → DestructionEffect
effectFor bid page = DestructionEffect
    { deBuildingId = bid, deDefName = buildingDefName, dePage = page
    , deAnchorX = fst effectTile, deAnchorY = snd effectTile, deGridZ = 0
    , deAnchorOffset = 0
    , deClip = DestructionClip
        { dcFps = 1, dcFrameCount = 1
        , dcFrames = legacyAssets (V.singleton (TextureHandle 0)) }
    -- Far enough ahead of the fixture clock that the drain's own
    -- expiry pass can never be what removed it.
    , deStartedAt = 1.0e9 }

-- * Reading the live state back

-- | Every row on one page, by raw id, sorted. One value so an
--   assertion states the whole page at once rather than six separately.
data Rows = Rows
    { rUnits        ∷ [Word32]
    , rUnitSelected ∷ [Word32]
    , rBuildings    ∷ [Word32]
    , rEffects      ∷ [Word32]
    , rClaims       ∷ [Word32]
    , rBuildingSel  ∷ Maybe Word32
    } deriving (Eq, Show)

noRows ∷ Rows
noRows = Rows [] [] [] [] [] Nothing

rowsOn ∷ EngineEnv → WorldPageId → IO Rows
rowsOn env page = do
    um ← readIORef (unitManagerRef env)
    bm ← readIORef (buildingManagerRef env)
    let onPage = HM.filter ((≡ page) . uiPage) (umInstances um)
        mine   = HM.keysSet onPage
        blds   = HM.filter ((≡ page) . biPage) (bmInstances bm)
    pure Rows
        { rUnits        = sort (map unUnitId (HM.keys onPage))
        , rUnitSelected = sort [ unUnitId u | u ← HS.toList (umSelected um)
                                            , HS.member u mine ]
        , rBuildings    = sort (map unBuildingId (HM.keys blds))
        , rEffects      = sort [ unBuildingId b
                              | (b, e) ← HM.toList (bmDestructions bm)
                              , dePage e ≡ page ]
        , rClaims       = sort [ unBuildingId b
                              | (b, r) ← HM.toList (bmReservations bm)
                              , frPage r ≡ page ]
        , rBuildingSel  = case bmSelected bm of
            Just b | HM.member b blds → Just (unBuildingId b)
            _                         → Nothing
        }

-- | Every sim state the unit thread holds, globally. Sim states carry
--   no page of their own, so "the retired unit's sim state went with
--   it" is a claim about this whole set.
simStateIds ∷ EngineEnv → IO [Word32]
simStateIds env =
    sort . map unUnitId . HM.keys . utsSimStates
        <$> readIORef (ucUtsRef (toUnitCombatCapability env))

-- | The two selection sets exactly as the managers hold them, with no
--   page filter applied.
--
--   'Rows' reports selection page-scoped, which is the right view for
--   "what does this page still own" but is VACUOUS as a gate on the
--   clear's own selection handling: it resolves a selected id through
--   the page's instance map, so an id left selected after its instance
--   was removed reads as absent either way. These two readers are what
--   an assertion about the selection itself must use.
selectionsRaw ∷ EngineEnv → IO ([Word32], Maybe Word32)
selectionsRaw env = do
    um ← readIORef (unitManagerRef env)
    bm ← readIORef (buildingManagerRef env)
    pure ( sort (map unUnitId (HS.toList (umSelected um)))
         , unBuildingId <$> bmSelected bm )

-- | How many item instances a unit is carrying — zero for a unit the
--   manager no longer holds.
unitInventorySize ∷ EngineEnv → UnitId → IO Int
unitInventorySize env uid = do
    um ← readIORef (unitManagerRef env)
    pure (maybe 0 (length . uiInventory) (HM.lookup uid (umInstances um)))

allocators ∷ EngineEnv → IO (Word32, Word32)
allocators env = do
    um ← readIORef (unitManagerRef env)
    bm ← readIORef (buildingManagerRef env)
    pure (umNextId um, bmNextId bm)

selectionGen ∷ EngineEnv → IO Word64
selectionGen env = wmSelectionGen <$> readIORef (worldManagerRef env)

teardownsPending ∷ EngineEnv → IO Int
teardownsPending env = wmTeardownsPending <$> readIORef (worldManagerRef env)

-- | The unit and building queues' contents, in order, put straight
--   back. Constructor tags, because the claim is about WHICH messages a
--   transition enqueued.
queuedEntityTags ∷ EngineEnv → IO ([Text], [Text])
queuedEntityTags env = do
    us ← peek (unitQueue env)
    bs ← peek (buildingQueue env)
    pure (map tag us, map tag bs)
  where
    peek q = do
        pending ← Q.flushQueue q
        mapM_ (Q.writeQueue q) pending
        pure pending
    tag ∷ Show α ⇒ α → Text
    tag = T.takeWhile (≢ ' ') . tshow

-- * Driving the production paths

-- | The REAL entity drains — the same two passes the unit thread runs
--   in one tick, units first. Nothing here is reimplemented, so what a
--   clear does is what the engine would do.
-- | The unit drain alone, and the building drain alone. Each fence
--   example drives only its own, so one cannot pass because the other
--   half of 'drainEntities' happened to block.
drainUnitsOnly ∷ EngineEnv → IO Bool
drainUnitsOnly env =
    processAllUnitCommands env (ucUtsRef (toUnitCombatCapability env))

drainBuildingsOnly ∷ EngineEnv → IO ()
drainBuildingsOnly env =
    processAllBuildingCommands (loggerRef env) (toWorldSimCapability env)
        (toContentRegistriesViewCapability env) (toBuildingCapability env)

drainEntities ∷ EngineEnv → IO ()
drainEntities env = do
    _ ← processAllUnitCommands env (ucUtsRef (toUnitCombatCapability env))
    processAllBuildingCommands (loggerRef env) (toWorldSimCapability env)
        (toContentRegistriesViewCapability env) (toBuildingCapability env)

-- | The REAL world-thread dispatcher, which is where a page-BOUND
--   placement is both validated and inserted (#1602).
runWorldQueue ∷ EngineEnv → IO ()
runWorldQueue env = do
    cmds ← Q.flushQueue (worldQueue env)
    logger ← readIORef (loggerRef env)
    forM_ cmds $ \cmd → do
        handleWorldCommand env logger cmd
        atomicModifyIORef' (worldManagerRef env) $ \mgr →
            (settleSelectionProjection mgr, ())

destroyPage ∷ EngineEnv → WorldPageId → IO ()
destroyPage env page = do
    logger ← readIORef (loggerRef env)
    handleWorldDestroyCommand env logger page

initArenaPage ∷ EngineEnv → WorldPageId → IO ()
initArenaPage env page = do
    logger ← readIORef (loggerRef env)
    handleWorldInitArenaCommand env logger page

-- | A real, cheap @w8@ world init — the codebase's own convention for a
--   headless page that must come from the production generator rather
--   than a hand-built 'WorldState'.
initWorldPage ∷ EngineEnv → WorldPageId → IO ()
initWorldPage env page = do
    logger ← readIORef (loggerRef env)
    handleWorldInitCommand env logger page 42 8 3 Nothing

showPage ∷ EngineEnv → WorldPageId → IO ()
showPage env page = do
    logger ← readIORef (loggerRef env)
    handleWorldShowCommand (toWorldSimCapability env) logger page

-- | The three lifecycle transitions requirement 3 names, each leaving
--   'incPage' registered and visible so a replacement admission can be
--   made against it.
data Path = Path { pathName ∷ String, pathRun ∷ EngineEnv → IO () }

paths ∷ [Path]
paths =
    [ Path "destroy then recreate the same id" $ \env → settled env $ do
        destroyPage env incPage
        -- The recreate replaces nothing (destroy already removed the
        -- id), so it enqueues no clear of its own — the destroy's is
        -- the only one outstanding.
        initArenaPage env incPage
        showPage env incPage
    , Path "world.init replacing a live id" $ \env →
        settled env (initWorldPage env incPage)
    , Path "world.initArena replacing a live id" $ \env →
        settled env (initArenaPage env incPage)
    ]

-- | Run one transition and leave the scene in the state the world
--   thread would leave it in, WITHOUT re-entering any of the entity
--   queues the examples are reading.
--
--   Three things happen after the handler returns:
--
--   * The selection projection is settled, exactly as
--     @World.Thread@\'s drain does after every command, so a binding
--     captured afterwards is genuinely fresh rather than merely
--     un-applied.
--   * The world and sim queues are DISCARDED. What a transition
--     enqueues for later — chunk loading, an arena's @InitArenaDone@ —
--     is downstream of the lifecycle boundary under test, and running
--     it here would pay for worldgen and could move page selection
--     underneath a binding an example is about to capture. The unit
--     and building queues are deliberately left alone: the queued
--     clears are the whole subject.
--   * The page's terrain is replaced with the fixture's flat chunks, so
--     a later placement is decided by the boundary rather than by
--     whatever ground a seed happened to generate. The transition
--     itself ran in full; only the ground it left is standardised.
settled ∷ EngineEnv → IO () → IO ()
settled env act = do
    act
    atomicModifyIORef' (worldManagerRef env) $ \mgr →
        (settleSelectionProjection mgr, ())
    _ ← Q.flushQueue (worldQueue env)
    _ ← Q.flushQueue (simQueue env)
    flattenPage env incPage

-- | Install the fixture's flat terrain and u-wrap size on whichever
--   'WorldState' currently stands under @page@.
flattenPage ∷ EngineEnv → WorldPageId → IO ()
flattenPage env page = do
    wm ← readIORef (worldManagerRef env)
    forM_ (lookup page (wmWorlds wm)) $ \ws → do
        writeIORef (wsTilesRef ws) flatTiles
        writeIORef (wsGenParamsRef ws) $ Just defaultWorldGenParams
            { wgpWorldSize = worldSize }
        writeIORef (wsPowerNodesRef ws) emptyPowerNodes

-- * Lua plumbing

newBareLuaBackend ∷ EngineEnv → IO LuaBackendState
newBareLuaBackend env = do
    ls ← createLuaBackendState (luaToEngineQueue env) (luaQueue env)
                               (assetPoolRef env) (nextObjectIdRef env)
                               (inputStateRef env) (loggerRef env)
    stateRef ← newIORef ThreadRunning
    registerLuaAPI (lbsLuaState ls) env ls stateRef
    loaded ← executeDebugLua (lbsLuaState ls) formatterLua
    loaded `shouldNotSatisfy` isLuaError
    pure ls

isLuaError ∷ Text → Bool
isLuaError t = "error:" `T.isPrefixOf` t ∨ "syntax error:" `T.isPrefixOf` t

-- | Folds each verb's multi-value return into ONE string; the debug
--   console reports only the first value.
formatterLua ∷ Text
formatterLua = T.concat
    [ "_G.__bs = function(a, b) "
    , "  if a == nil then return 'nil|' .. tostring(b) end; "
    , "  return 'id|' .. tostring(a); end; "
    , "_G.__pn = function(a, b) "
    , "  if a == nil then return 'nil|' .. tostring(b) end; "
    , "  return 'id|' .. tostring(b); end; "
    , "_G.__to = function(a, b) "
    , "  if a == nil then return 'nil|' .. tostring(b) end; "
    , "  return 'ok|' .. tostring(#a); end; "
    , "return 'ok'" ]

q ∷ Text → Text
q t = "\"" <> t <> "\""

-- | @unit.spawn@ through the registered production API, answering the
--   allocated raw id. Z is supplied explicitly so nothing here depends
--   on a surface lookup.
spawnUnitOn ∷ LuaBackendState → WorldPageId → (Int, Int) → IO Word32
spawnUnitOn ls (WorldPageId pg) (gx, gy) = do
    raw ← executeDebugLua (lbsLuaState ls) $ T.concat
        [ "return unit.spawn('", unitDefName, "', "
        , tshow gx, ", ", tshow gy, ", 0, 'player', '", pg, "')" ]
    -- The verb pushes a Lua NUMBER, so the console may render it with
    -- or without a fractional part; parse as a Double and round.
    case reads (T.unpack raw) ∷ [(Double, String)] of
        [(n, "")] | n > 0 → pure (round n)
        _ → fail ("unit.spawn refused or unparsable: " <> T.unpack raw)

-- | @building.spawn@, answering the allocated raw id. @mGen@ present
--   makes it a page-BOUND placement, routed to the world thread.
spawnBuildingOn ∷ LuaBackendState → Text → (Int, Int) → WorldPageId
                → Maybe Word64 → IO Word32
spawnBuildingOn ls defName tile page mGen = do
    raw ← tryBuildingSpawn ls defName tile page mGen
    let parsed = do
            n ← T.stripPrefix "id|" (unquote raw)
            case reads (T.unpack n) ∷ [(Int, String)] of
                [(v, "")] → Just (fromIntegral v ∷ Word32)
                _         → Nothing
    case parsed of
        Just v  → pure v
        Nothing → fail ("building.spawn refused: " <> T.unpack raw)

-- | The same call, reporting the raw folded answer instead of failing —
--   for the refusal cases.
tryBuildingSpawn ∷ LuaBackendState → Text → (Int, Int) → WorldPageId
                 → Maybe Word64 → IO Text
tryBuildingSpawn ls defName (gx, gy) (WorldPageId pg) mGen =
    executeDebugLua (lbsLuaState ls) $ T.concat
        [ "return _G.__bs(building.spawn('", defName, "', "
        , tshow gx, ", ", tshow gy, ", '", pg, "'"
        , maybe "" (\g → ", " <> tshow g) mGen
        , "))" ]

-- | @power.placeNode@, answering the folded string.
placeNodeOnPage ∷ LuaBackendState → Word32 → WorldPageId → (Int, Int) → IO Text
placeNodeOnPage ls supplier (WorldPageId pg) (gx, gy) =
    executeDebugLua (lbsLuaState ls) $ T.concat
        [ "return _G.__pn(power.placeNode(", tshow supplier, ", '"
        , panelDefName, "', "
        , tshow gx, ", ", tshow gy, ", '", pg, "'))" ]

-- | @unit.exists(uid)@ / @building.getInfo(bid)@ through the registered
--   production API, folded to a plain @true@\/@false@. Between them
--   they answer the question every one of the five surfaces round 2
--   named depends on: can a verb still RESOLVE an old incarnation's
--   entity under the replacement's reused page name?
unitExists ∷ LuaBackendState → Word32 → IO Text
unitExists ls uid = executeDebugLua (lbsLuaState ls) $ T.concat
    [ "return unit.exists(", tshow uid, ") and true or false" ]

buildingResolves ∷ LuaBackendState → Word32 → IO Text
buildingResolves ls bid = executeDebugLua (lbsLuaState ls) $ T.concat
    [ "return building.getInfo(", tshow bid, ") ~= nil" ]

-- | @unit.getTransferOrders(uid)@ — the READ side of the one store
--   resolution `unit.createTransferOrder` writes through
--   ('unitOrderStore'), folded to @ok|<count>@ or @nil|reason@. Reading
--   is enough to gate the resolution itself, which is what decides
--   whose page store a unit's durable orders may reach.
getOrders ∷ LuaBackendState → Word32 → IO Text
getOrders ls uid = executeDebugLua (lbsLuaState ls) $ T.concat
    [ "return _G.__to(unit.getTransferOrders(", tshow uid, "))" ]

-- | Give a unit the power items `power.placeNode` consumes. Fixture
--   state, not the thing under test: `unit.spawn` grants no inventory,
--   and a supplier admitted AFTER a transition is exactly what these
--   examples need to hand the verb.
givePanels ∷ EngineEnv → UnitId → IO ()
givePanels env uid = atomicModifyIORef' (unitManagerRef env) $ \um →
    case HM.lookup uid (umInstances um) of
        Nothing → (um, ())
        Just u  →
            let u' = u { uiInventory = [panelItem 11, panelItem 12] }
            in (um { umInstances = HM.insert uid u' (umInstances um) }, ())

unquote ∷ Text → Text
unquote t = case T.stripPrefix "\"" t of
    Just rest → T.dropWhileEnd (≡ '"') rest
    Nothing   → t

-- * Spec

spec ∷ Spec
spec = describe "Page incarnation entity teardown" $ aroundAll setup $ do
    teardownSpec
    survivalSpec
    bindingSpec
    cutoffSpec
    scopeSpec
    lockSpec
  where
    -- Isolation wraps the boot (#1357): engine init is itself a config
    -- writer, so a scratch root established afterwards is already late.
    setup act = withIsolatedResourceRoot $ do
        EngineInitResult env ← initializeEngineHeadlessQuiet
        ls ← newBareLuaBackend env
        act (env, ls)

-- | Requirement 7: after each transition drains, none of the old
--   incarnation's rows remains.
teardownSpec ∷ SpecWith (EngineEnv, LuaBackendState)
teardownSpec = describe "the old incarnation's rows are retired" $ do

    it "the fixture really seeds every category of row, so an empty \
       \result below can only be teardown" $ \(env, ls) → do
        resetScene env
        seedIncarnation env ls incPage
        rows ← rowsOn env incPage
        rUnits rows        `shouldSatisfy` ((≡ 2) . length)  -- supplier + spawn
        rUnitSelected rows `shouldSatisfy` ((≡ 1) . length)
        rBuildings rows    `shouldSatisfy` ((≡ 1) . length)
        rEffects rows      `shouldSatisfy` ((≡ 1) . length)
        rClaims rows       `shouldSatisfy` ((≡ 1) . length)
        rBuildingSel rows  `shouldSatisfy` isJust
        ids ← simStateIds env
        length ids `shouldBe` 1

    forM_ paths $ \p →
        it ("retires every row after " <> pathName p) $ \(env, ls) → do
            resetScene env
            seedIncarnation env ls incPage
            (u0, b0) ← allocators env
            pathRun p env
            -- #2476 round 2: the transition retires the manager rows
            -- ITSELF, before it returns and before anything drains.
            -- That is what stops every verb that resolves an entity's
            -- page from finding an old row under the replacement's
            -- reused name and spending it into durable state that
            -- outlives the row — an item drop, a transfer, a
            -- construction payment, a container reveal, a power
            -- placement. Asserting it here, on the manager state
            -- immediately after the handler returns, gates the whole
            -- class at its one structural cause rather than verb by
            -- verb.
            rowsOn env incPage `shouldReturn` noRows
            selectionsRaw env  `shouldReturn` ([], Nothing)
            -- The sim states, though, ARE still queued: they belong to
            -- the unit thread, so only its own drain may remove them.
            simsStillQueued ← simStateIds env
            simsStillQueued `shouldNotBe` []
            drainEntities env
            rowsOn env incPage `shouldReturn` noRows
            simStateIds env    `shouldReturn` []
            -- Read RAW, not through the page filter: an id left in
            -- 'umSelected'/'bmSelected' after its instance was retired
            -- is exactly the leak a page-scoped view cannot see.
            selectionsRaw env  `shouldReturn` ([], Nothing)
            -- Requirement 4: the allocators are never rewound.
            (u1, b1) ← allocators env
            u1 `shouldSatisfy` (≥ u0)
            b1 `shouldSatisfy` (≥ b0)
            -- Requirement 9: no session fence, no session marker.
            teardownsPending env `shouldReturn` 0
            (us, bs) ← queuedEntityTags env
            us `shouldNotSatisfy` elem "UnitEndSession"
            bs `shouldNotSatisfy` elem "BuildingEndSession"

    it "registering an id no page holds enqueues no clear at all" $
        \(env, _) → do
            resetScene env
            destroyPage env incPage
            drainEntities env
            drainQueues env
            -- incPage is now unregistered; re-creating it replaces
            -- nothing, so requirement 3's exception applies.
            initArenaPage env incPage
            queuedEntityTags env `shouldReturn` ([], [])

    it "a single-page destroy enqueues exactly one clear on each entity \
       \queue and no session marker" $ \(env, ls) → do
        resetScene env
        seedIncarnation env ls incPage
        drainQueues env
        destroyPage env incPage
        queuedEntityTags env
            `shouldReturn` (["UnitClearPage"], ["BuildingClearPage"])

-- | Requirement 7 and the acceptance's interleaving bullets: the
--   REPLACEMENT's admissions survive the clear that is still queued.
survivalSpec ∷ SpecWith (EngineEnv, LuaBackendState)
survivalSpec = describe "the replacement's admissions survive" $ do

    forM_ paths $ \p →
        it ("an unbound building admitted after " <> pathName p
            <> " keeps its claim and commits") $ \(env, ls) → do
            resetScene env
            seedIncarnation env ls incPage
            pathRun p env
            -- Admitted while BOTH clears are still queued.
            bid ← spawnBuildingOn ls buildingDefName lateTile incPage Nothing
            claimsNow ← rClaims <$> rowsOn env incPage
            -- The claim is outstanding: it must survive until its own
            -- spawn consumes it, not be swept by the queued clear.
            claimsNow `shouldSatisfy` elem bid
            drainEntities env
            rows ← rowsOn env incPage
            rBuildings rows `shouldBe` [bid]
            -- Consumed by the commit, not by the clear.
            rClaims rows    `shouldBe` []

    forM_ paths $ \p →
        it ("a unit admitted after " <> pathName p <> " survives")
            $ \(env, ls) → do
            resetScene env
            seedIncarnation env ls incPage
            pathRun p env
            uid ← spawnUnitOn ls incPage lateTile
            drainEntities env
            (rUnits <$> rowsOn env incPage) `shouldReturn` [uid]
            simStateIds env `shouldReturn` [uid]

    it "a power node placed by a REPLACEMENT-incarnation supplier commits \
       \its building and registers into the page that replaced it" $
        \(env, ls) → do
            resetScene env
            seedIncarnation env ls incPage
            initArenaPage env incPage
            -- Admitted AFTER the transition, so at or above the floor.
            supplier ← spawnUnitOn ls incPage unitTile
            drainEntities env
            givePanels env (UnitId supplier)
            answer ← placeNodeOnPage ls supplier incPage powerTile
            answer `shouldSatisfy` T.isPrefixOf "\"id|"
            drainEntities env
            rows ← rowsOn env incPage
            length (rBuildings rows) `shouldBe` 1
            -- The node landed in the REPLACEMENT's own registry, which
            -- is the state 'placeNodeOn' resolved under the lock.
            wm ← readIORef (worldManagerRef env)
            case lookup incPage (wmWorlds wm) of
                Nothing → expectationFailure "the replacement page vanished"
                Just ws → do
                    nodes ← readIORef (wsPowerNodesRef ws)
                    HM.size (pnsNodes nodes) `shouldBe` 1

    forM_ paths $ \p →
        it ("a supplier from the incarnation " <> pathName p
            <> " replaced cannot place a node onto the replacement")
            $ \(env, ls) → do
            resetScene env
            seedIncarnation env ls incPage
            -- The fixture supplier is installed by 'resetScene', so it
            -- is a PRE-cutoff unit — and it is still in @umInstances@
            -- carrying this page's reused name until the queued
            -- 'UnitClearPage' drains. That window is the whole point:
            -- without the incarnation floor its item would become a
            -- building AND a power node on the replacement, both at or
            -- above the cutoff, so both outliving the supplier itself.
            pathRun p env
            answer ← placeNodeOnPage ls (unUnitId supplierUid) incPage
                                     powerTile
            -- The supplier is not "on the wrong page" — it is GONE, the
            -- same answer the verb gives for a demolished or destroyed
            -- unit, because the transition retired it in the same
            -- locked step that replaced the page.
            answer `shouldBe` q ("nil|unit has no " <> panelDefName)
            -- Nothing was popped, no id spent, nothing enqueued.
            inv ← unitInventorySize env supplierUid
            inv `shouldBe` 0
            drainEntities env
            rBuildings <$> rowsOn env incPage `shouldReturn` []
            wm ← readIORef (worldManagerRef env)
            forM_ (lookup incPage (wmWorlds wm)) $ \ws → do
                nodes ← readIORef (wsPowerNodesRef ws)
                HM.size (pnsNodes nodes) `shouldBe` 0

    forM_ paths $ \p →
        it ("a carrier from the incarnation " <> pathName p
            <> " replaced cannot reach the replacement's transfer-order \
               \store, and a replacement's carrier can") $ \(env, ls) → do
            resetScene env
            seedIncarnation env ls incPage
            -- Resolvable before the transition, so the refusal below
            -- is the teardown's doing and not a fixture accident.
            getOrders ls (unUnitId supplierUid) `shouldReturn` q "ok|0"
            pathRun p env
            -- A durable order stored here would name a carrier the
            -- teardown removes, and nothing would ever retire it: it
            -- would ride every later save as a dangling acting-unit
            -- reference. The carrier is already gone, so the store it
            -- would have been written into cannot even be resolved.
            getOrders ls (unUnitId supplierUid)
                `shouldReturn` q "nil|unit.getTransferOrders: no such \
                                 \unit, or its world page is not loaded"
            replacement ← spawnUnitOn ls incPage lateTile
            drainEntities env
            getOrders ls replacement `shouldReturn` q "ok|0"

    forM_ paths $ \p →
        it ("no production verb can resolve an old entity once "
            <> pathName p <> " has returned") $ \(env, ls) → do
            resetScene env
            (uid, bid) ← admitIncarnation ls incPage
            drainEntities env
            -- Both resolve BEFORE the transition, so the answers after
            -- it are the teardown's doing and not a fixture accident.
            unitExists ls uid       `shouldReturn` "true"
            buildingResolves ls bid `shouldReturn` "true"
            pathRun p env
            -- …and neither afterwards, WITHOUT any drain. This is the
            -- premise every one of round 2's five surfaces rests on: an
            -- item drop, a strict or lax transfer, a construction
            -- payment and a container reveal all begin by resolving an
            -- entity's page, and none of them can reach one that is no
            -- longer in its manager. Gating the cause rather than each
            -- verb is what keeps the guarantee from depending on an
            -- enumeration of callers that can grow.
            unitExists ls uid       `shouldReturn` "false"
            buildingResolves ls bid `shouldReturn` "false"

    forM_ paths $ \p →
        it ("a spawn already dequeued when " <> pathName p
            <> " ran cannot insert afterwards") $ \(env, ls) → do
            resetScene env
            _ ← spawnUnitOn ls incPage unitTile
            bid ← spawnBuildingOn ls buildingDefName oldTile incPage Nothing
            -- Take the commands OFF their queues first. That is the
            -- schedule neither half of the teardown can catch: the
            -- transition's immediate retirement finds no instance to
            -- remove because the spawn has not run, and its queued
            -- clear is enqueued BEHIND nothing, so re-queuing the spawn
            -- afterwards puts it after the clear. Without the
            -- page-incarnation epoch on the command, the insertion
            -- below would make an old incarnation's entity live under
            -- the replacement's name — externally visible to every
            -- verb until some later clear, and to Lua in between.
            heldUnits ← Q.flushQueue (unitQueue env)
            heldBuildings ← Q.flushQueue (buildingQueue env)
            length heldUnits     `shouldBe` 1
            length heldBuildings `shouldBe` 1
            pathRun p env
            mapM_ (Q.writeQueue (unitQueue env)) heldUnits
            mapM_ (Q.writeQueue (buildingQueue env)) heldBuildings
            drainEntities env
            -- Refused at its own commit, by the epoch it carries.
            rows ← rowsOn env incPage
            rUnits rows     `shouldBe` []
            rBuildings rows `shouldBe` []
            simStateIds env `shouldReturn` []
            -- And the refused building's claim is retired with it, so
            -- its tiles are not held against the replacement forever.
            rClaims rows `shouldNotSatisfy` elem bid

    it "a spawn admitted before a destroy and drained after it is \
       \dropped by the absent-page guard, leaking no claim" $
        \(env, ls) → do
            resetScene env
            _ ← spawnUnitOn ls incPage lateTile
            bid ← spawnBuildingOn ls buildingDefName lateTile incPage Nothing
            claims0 ← rClaims <$> rowsOn env incPage
            claims0 `shouldBe` [bid]
            destroyPage env incPage
            drainEntities env
            -- The page is gone, so neither spawn inserts; the building
            -- spawn's own drop path retires its claim.
            bm ← readIORef (buildingManagerRef env)
            HM.size (bmInstances bm)    `shouldBe` 0
            HM.size (bmReservations bm) `shouldBe` 0
            -- The supplier goes too, and must: it was seeded ON the
            -- destroyed page, so it is an old-incarnation row like any
            -- other and its id is below the captured cutoff.
            um ← readIORef (unitManagerRef env)
            HM.null (umInstances um) `shouldBe` True

-- | Requirement 6: the direct world-thread bound commit.
bindingSpec ∷ SpecWith (EngineEnv, LuaBackendState)
bindingSpec = describe "page-bound placement across the boundary" $ do

    forM_ paths $ \p →
        it ("a valid bound placement admitted after " <> pathName p
            <> " commits on the world thread and survives the delayed \
               \clear") $ \(env, ls) → do
            resetScene env
            seedIncarnation env ls incPage
            pathRun p env
            gen ← selectionGen env
            bid ← spawnBuildingOn ls buildingDefName boundTile incPage
                                  (Just gen)
            -- The world thread commits it NOW — ahead of the building
            -- clear, which is still sitting on the building queue.
            runWorldQueue env
            committed ← rBuildings <$> rowsOn env incPage
            -- Committed already — ahead of the clear, and beside the
            -- old incarnation's rows, which are still waiting for it.
            committed `shouldSatisfy` elem bid
            drainEntities env
            -- Its id is at or above the cutoff, so the clear leaves it.
            rows ← rowsOn env incPage
            rBuildings rows `shouldBe` [bid]
            rClaims rows    `shouldBe` []

    forM_ paths $ \p →
        it ("a binding captured before " <> pathName p
            <> " is refused and leaks no claim") $ \(env, ls) → do
            resetScene env
            seedIncarnation env ls incPage
            gen ← selectionGen env
            (_, b0) ← allocators env
            pathRun p env
            tryBuildingSpawn ls buildingDefName boundTile incPage (Just gen)
                `shouldReturn` q "nil|page binding stale"
            -- Refused ahead of the reservation transaction: no claim
            -- taken, no id consumed. The page carries no claim at all
            -- by now — the transition retired the old incarnation's —
            -- so the untouched id counter is what proves the refusal
            -- was free rather than merely tidy.
            (rClaims <$> rowsOn env incPage) `shouldReturn` []
            (_, b1) ← allocators env
            b1 `shouldBe` b0

    it "a bound placement naming a page that is NOT the visible head is \
       \refused even with a fresh generation" $ \(env, ls) → do
        resetScene env
        gen ← selectionGen env
        -- keepPage is live but hidden, so replacing it would bump no
        -- generation and the binding could not detect the change.
        tryBuildingSpawn ls buildingDefName boundTile keepPage (Just gen)
            `shouldReturn` q "nil|page binding stale"
        -- The identical placement UNBOUND is still accepted: the
        -- refusal is the binding's, not the page's.
        _ ← spawnBuildingOn ls buildingDefName boundTile keepPage Nothing
        pure ()

-- | Requirement 5: the exact cutoff classification, with real
--   admissions on both sides of the transition.
cutoffSpec ∷ SpecWith (EngineEnv, LuaBackendState)
cutoffSpec = describe "the cutoff classifies every admitted id" $
    forM_ paths $ \p →
        it ("across " <> pathName p) $ \(env, ls) → do
            resetScene env
            -- BEFORE: two real admissions, left undrained.
            earlyU ← spawnUnitOn ls incPage unitTile
            earlyB ← spawnBuildingOn ls buildingDefName oldTile incPage Nothing
            -- Nothing else allocates between here and the transition,
            -- so these ARE the cutoffs it captures.
            (cutoffU, cutoffB) ← allocators env
            pathRun p env
            -- AFTER: two more, against the replacement.
            lateU ← spawnUnitOn ls incPage lateTile
            lateB ← spawnBuildingOn ls buildingDefName lateTile incPage Nothing
            earlyU `shouldSatisfy` (< cutoffU)
            earlyB `shouldSatisfy` (< cutoffB)
            lateU  `shouldSatisfy` (≥ cutoffU)
            lateB  `shouldSatisfy` (≥ cutoffB)
            drainEntities env
            rows ← rowsOn env incPage
            -- Exactly the post-cutoff pair survives; the pre-cutoff
            -- pair was either inserted and then cleared (the init
            -- paths, where the page is registered throughout) or
            -- dropped outright by the absent-page guard.
            rUnits rows     `shouldBe` [lateU]
            rBuildings rows `shouldBe` [lateB]
            simStateIds env `shouldReturn` [lateU]

-- | Requirements 4 and 8: scoping, and the two no-op cases.
scopeSpec ∷ SpecWith (EngineEnv, LuaBackendState)
scopeSpec = describe "scoping and no-ops" $ do

    it "destroying a hidden page leaves every row of the visible page \
       \untouched" $ \(env, ls) → do
        resetScene env
        seedBothPages env ls
        visibleBefore ← rowsOn env incPage
        -- The selection names the VISIBLE page's building, so "the
        -- clear did not touch it" is a real claim rather than a
        -- vacuous one.
        rBuildingSel visibleBefore `shouldSatisfy` isJust
        (selUnitsBefore, selBuildingBefore) ← selectionsRaw env
        simsBefore    ← simStateIds env
        destroyPage env keepPage
        drainEntities env
        rowsOn env keepPage `shouldReturn` noRows
        -- The visible page is untouched by the clear. Its own pending
        -- spawn commits in the same drain, exactly as it would have
        -- with no destroy at all, so that ONE claim-to-instance move is
        -- the whole expected difference — and it is the difference a
        -- page-blind clear would have erased instead.
        after ← rowsOn env incPage
        after `shouldBe` visibleBefore
            { rBuildings = sort (rClaims visibleBefore ++ rBuildings visibleBefore)
            , rClaims    = [] }
        -- Read raw: the destroyed page's unit leaves the selection set
        -- and the visible page's building keeps the single-slot
        -- building selection, which a page-filtered view could not
        -- distinguish from "cleared".
        (selUnitsAfter, selBuildingAfter) ← selectionsRaw env
        selBuildingAfter `shouldBe` selBuildingBefore
        selUnitsAfter `shouldSatisfy` (\a → length a < length selUnitsBefore)
        forM_ selUnitsAfter $ \u →
            rUnits visibleBefore `shouldSatisfy` elem u
        -- The visible page's units keep their sim states; only the
        -- hidden page's went.
        simsAfter ← simStateIds env
        simsAfter `shouldSatisfy` (\a → length a < length simsBefore)
        simsAfter `shouldNotBe` []
        -- Every surviving sim state still belongs to a unit the visible
        -- page holds, so the clear orphaned none and kept none it
        -- should have taken. (Only spawned units have one at all — the
        -- fixture's directly-installed supplier never went through the
        -- spawn handler.)
        incUnits ← rUnits <$> rowsOn env incPage
        forM_ simsAfter $ \u → incUnits `shouldSatisfy` elem u

    it "a page clear that matches no row is a no-op" $ \(env, ls) → do
        resetScene env
        seedIncarnation env ls incPage
        -- Settle first, so the only queued work left is the two clears
        -- and the one spawn admitted just below. Anything else pending
        -- would make "nothing changed" a claim about the drain rather
        -- than about the clears.
        drainEntities env
        extra ← spawnBuildingOn ls buildingDefName lateTile incPage Nothing
        before ← rowsOn env incPage
        rClaims before `shouldBe` [extra]
        sims0  ← simStateIds env
        (u0, b0) ← allocators env
        Q.writeQueue (unitQueue env) (UnitClearPage absentPage (UnitId u0))
        Q.writeQueue (buildingQueue env)
                     (BuildingClearPage absentPage (BuildingId b0))
        drainEntities env
        after ← rowsOn env incPage
        -- The ONLY difference is the one the extra spawn made: its
        -- claim became an instance. Nothing the clears named was here,
        -- so they removed nothing and moved neither allocator.
        after `shouldBe` before { rBuildings = sort (extra : rBuildings before)
                                , rClaims    = [] }
        simStateIds env `shouldReturn` sims0
        allocators env  `shouldReturn` (u0, b0)

    it "a page clear arriving after destroy-all's whole-manager clear \
       \is a no-op" $ \(env, ls) → do
        resetScene env
        seedIncarnation env ls incPage
        (u0, b0) ← allocators env
        logger ← readIORef (loggerRef env)
        handleWorldDestroyAllCommand env logger
        -- Queued BEHIND destroy-all's own clears, so it runs against
        -- already-emptied managers. (Both markers stop their drain, so
        -- two passes are needed to reach it.)
        Q.writeQueue (unitQueue env) (UnitClearPage incPage (UnitId u0))
        Q.writeQueue (buildingQueue env)
                     (BuildingClearPage incPage (BuildingId b0))
        drainEntities env
        drainEntities env
        um ← readIORef (unitManagerRef env)
        bm ← readIORef (buildingManagerRef env)
        HM.null (umInstances um)    `shouldBe` True
        HM.null (bmInstances bm)    `shouldBe` True
        HM.null (bmReservations bm) `shouldBe` True
        -- Nothing rewound the allocators on the way through.
        (umNextId um, bmNextId bm) `shouldBe` (u0, b0)

    it "a clear preserves rows at or above its cutoff on its own page" $
        \(env, ls) → do
            resetScene env
            bid ← spawnBuildingOn ls buildingDefName oldTile incPage Nothing
            drainEntities env
            -- A cutoff BELOW the committed id: the row is on the page
            -- but out of range, so the filter must leave it.
            Q.writeQueue (buildingQueue env)
                         (BuildingClearPage incPage (BuildingId bid))
            drainEntities env
            (rBuildings <$> rowsOn env incPage) `shouldReturn` [bid]

-- | The lock itself. Nothing single-threaded can distinguish a locked
--   transition from an unlocked one, so each example HOLDS the mutex on
--   this thread, runs the production path on another, and shows it make
--   no progress until the mutex is released.
lockSpec ∷ SpecWith (EngineEnv, LuaBackendState)
lockSpec = describe "the lifecycle lock is actually taken" $ do

    it "the unit spawn COMMIT blocks on it, so a transition cannot land \
       \between the handler's revalidation and its insertion" $
        \(env, ls) → do
            resetScene env
            _ ← spawnUnitOn ls incPage unitTile
            -- Draining the spawn must take the lock: the epoch it
            -- verifies is checked at the top of the handler and the
            -- insertion happens after a great deal of work, so only a
            -- fence around the revalidation AND the write can stop a
            -- re-init landing in between and leaving a departed
            -- incarnation's unit visible under the replacement.
            --
            -- The UNIT drain alone, so the building commit's own fence
            -- cannot be what blocks and make this pass for the wrong
            -- reason.
            blockedUntilReleased env (void (drainUnitsOnly env))

    it "the building spawn COMMIT blocks on it too, on the shared body \
       \both routes use" $ \(env, ls) → do
        resetScene env
        _ ← spawnBuildingOn ls buildingDefName oldTile incPage Nothing
        blockedUntilReleased env (drainBuildingsOnly env)

    it "unit.spawn blocks on it" $ \(env, _) → withOwnBackend env $ \ls → do
        resetScene env
        blockedUntilReleased env (void (spawnUnitOn ls incPage unitTile))

    it "building.spawn blocks on it" $ \(env, _) →
        withOwnBackend env $ \ls → do
            resetScene env
            blockedUntilReleased env
                (void (spawnBuildingOn ls buildingDefName oldTile incPage
                                       Nothing))

    it "power.placeNode blocks on it" $ \(env, _) →
        withOwnBackend env $ \ls → do
            resetScene env
            blockedUntilReleased env
                (void (placeNodeOnPage ls (unUnitId supplierUid) incPage
                                       powerTile))

    it "single-page destroy blocks on it" $ \(env, _) → do
        resetScene env
        blockedUntilReleased env (destroyPage env incPage)

    it "world.initArena blocks on it" $ \(env, _) → do
        resetScene env
        blockedUntilReleased env (initArenaPage env incPage)

    it "world.init blocks on it" $ \(env, _) → do
        resetScene env
        blockedUntilReleased env (initWorldPage env incPage)

-- | A Lua state of this example's own, so the forked call below never
--   shares one with the main thread.
withOwnBackend ∷ EngineEnv → (LuaBackendState → IO α) → IO α
withOwnBackend env act = newBareLuaBackend env ≫= act

-- | Hold 'pageLifecycleLock', start @act@ elsewhere, and assert that it
--   makes no progress while the mutex is held and completes once it is
--   released.
--
--   The negative half is what a missing 'withPageLifecycle' fails:
--   without it the action runs straight through and 'finished' is
--   already full. The wait is one-sided — a slower machine only makes
--   "did not finish" more true — and the positive half is an ordinary
--   blocking 'takeMVar', not a timeout.
blockedUntilReleased ∷ EngineEnv → IO () → IO ()
blockedUntilReleased env act = do
    finished ← newEmptyMVar ∷ IO (MVar (Either SomeException ()))
    () ← takeMVar (pageLifecycleLock env)
    -- 'try' so a failing action still fills the box: a deadlock here
    -- would strand every later example behind the held mutex.
    (do _ ← forkIO ((try act ∷ IO (Either SomeException ()))
                        ≫= \outcome → putMVar finished outcome)
        threadDelay 250000
        early ← tryTakeMVar finished
        early `shouldSatisfy` isNothing)
      `finally` putMVar (pageLifecycleLock env) ()
    outcome ← takeMVar finished
    case outcome of
        Right () → pure ()
        Left err → expectationFailure
            ("the locked action failed once released: " <> show err)
