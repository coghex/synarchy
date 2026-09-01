{-# LANGUAGE Strict #-}
{-# LANGUAGE OverloadedStrings #-}
-- | "Portal spawn page binding" (#1686): a portal that was in
--   @buildingSpawn.update@'s active-page snapshot but whose page stopped
--   being the active page before the tick reached its commit spawns
--   nothing.
--
--   #196 closed the DESTINATION half of this race — the tick names the
--   building's own page, so a mid-tick switch can no longer route the
--   unit into the wrong world. It left the ELIGIBILITY half open:
--   @unit.spawn@ honours an explicit page for ANY live page, hidden
--   included, so the enumerated portal still spent a roster entry and
--   put an acolyte into a page the player is no longer looking at.
--
--   Everything below the stub seam is REAL: the registered Lua API, the
--   shipped @scripts/building_spawn.lua@, the production
--   'handleWorldShowCommand' \/ 'handleWorldHideCommand' handlers, and
--   the production @world.show@ \/ @world.hide@ enqueue path. The one
--   stub is a wrapper around @building.getActiveIds@ that lands a
--   selection change immediately AFTER the real snapshot and before the
--   real commit — the interleaving the race needs, made deterministic.
--   'installPageSwitch' is 'Test.Headless.Building.PageBinding'\'s seam,
--   reused for the same reason.
--
--   The engine runs NO worker threads, so a queued 'UnitSpawn' stays in
--   its queue: "no unit was created" is asserted on the queue and on the
--   id allocator rather than raced against a drainer. @scripts.unit_ai@
--   is pre-seeded into @package.loaded@ so the real AI singleton never
--   boots and @commandMove@ — which ONLY the success path reaches — is
--   an observable spy. That spy is what makes the no-items assertion
--   non-vacuous: the shipped portal's @starting_items@ is empty, so
--   @unit.addItem@ alone could never distinguish "refused" from
--   "succeeded", while @commandMove@ sits in the SAME success-only block
--   and is proven below to fire exactly once when the spawn is accepted.
--
--   Run just this gate: @cabal test synarchy-test-headless
--   --test-options='--match "Portal spawn page binding"'@.
module Test.Headless.Building.PortalSpawnBinding (spec) where

import UPrelude
import Test.Hspec
import Test.Headless.Harness.Isolation (withIsolatedResourceRoot)
import Data.IORef (atomicModifyIORef', newIORef, readIORef, writeIORef)
import qualified Data.HashMap.Strict as HM
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU
import qualified HsLua as Lua

import Building.Types
    ( BuildingDef(..), BuildingId(..), BuildingInstance(..)
    , BuildingManager(..), emptyBuildingManager )
import Engine.Asset.Handle (TextureHandle(..))
import Engine.Core.Init (initializeEngineHeadless, EngineInitResult(..))
import Engine.Core.State (EngineEnv(..))
import Engine.Core.Thread (ThreadControl(..))
import Engine.Core.Capability.WorldSim (toWorldSimCapability)
import qualified Engine.Core.Queue as Q
import Engine.Scripting.Lua.API (registerLuaAPI)
import Engine.Scripting.Lua.Thread (createLuaBackendState)
import Engine.Scripting.Lua.Thread.Console (executeDebugLua)
import Engine.Scripting.Lua.Types (LuaBackendState(..))
import Structure.Types (emptyChunkStructures)
import Unit.Command.Types (UnitCommand(..))
import Unit.Faction (Faction(..))
import Unit.Types
    ( UnitDef(..), UnitManager(..), emptyUnitManager
    , defaultNaturalResistance )
import World.Chunk.Types
    (ChunkCoord(..), ColumnTiles(..), LoadedChunk(..))
import World.Flora.Types (emptyFloraChunkData)
import World.Fluid.Types (emptyIceMap)
import World.Generate.Types (WorldGenParams(..), defaultWorldGenParams)
import World.Page.Types (WorldPageId(..))
import World.State.Types
    ( WorldManager(..), WorldState(..), emptyWorldManager, emptyWorldState
    , settleSelectionProjection )
import World.Command.Types (WorldCommand(..))
import World.Thread.Command (handleWorldCommand)
import World.Thread.Command.UI
    (handleWorldHideCommand, handleWorldShowCommand)
import World.Tile.Types (WorldTileData(..))

-- * Fixture identity

-- | The page the portal stands on, and the page a mid-tick selection
--   change switches to.
pageA, pageB ∷ WorldPageId
pageA = WorldPageId "portal_bind_a"
pageB = WorldPageId "portal_bind_b"

-- | Surface elevations. They differ so the accepted spawn's Z proves
--   WHICH page the height came from (#196's other half): a Z of
--   'terrainZB' would mean the commit read the page that became active,
--   not the portal's own.
terrainZA, terrainZB ∷ Int
terrainZA = 5
terrainZB = 9

-- | The portal's anchor tile. Inside chunk (0,0) on both pages, so the
--   only thing that can vary between them is the elevation above.
portalTile ∷ (Int, Int)
portalTile = (2, 3)

-- | The one portal, and the roster it starts with. SEEDED — a positive
--   countdown, so @ensureState@'s first-sight seeding (the @-1@ sentinel
--   becoming the configured count) cannot be mistaken for the roster
--   consumption this gate is asserting the absence of.
portalBid ∷ BuildingId
portalBid = BuildingId 1

seededRoster ∷ Int
seededRoster = 6

-- | The shipped portal def name and its roster's first entry. The gate
--   drives @scripts/building_spawn.lua@'s REAL config table, which is
--   keyed by def name, so these are not free choices.
portalDefName, rosterFirstUnit ∷ Text
portalDefName   = "acolyte_portal"
rosterFirstUnit = "acolyte"

-- | The portal's own @spawn_offset@ and @walk_to_offset@, restated so
--   the accepted-spawn assertions name exact coordinates.
spawnPos, walkPos ∷ (Double, Double)
spawnPos = (fromIntegral (fst portalTile) + 0.5, fromIntegral (snd portalTile) + 0.5)
walkPos  = (fromIntegral (fst portalTile) + 0.5, fromIntegral (snd portalTile) + 2.5)

-- * Terrain fixtures

flatChunkAt ∷ ChunkCoord → Int → LoadedChunk
flatChunkAt coord z =
    let area = 16 * 16
        col  = ColumnTiles
            { ctStartZ = z
            , ctMats   = VU.singleton 1
            , ctSlopes = VU.singleton 0
            , ctVeg    = VU.singleton 0
            }
    in LoadedChunk
        { lcCoord             = coord
        , lcTiles             = V.replicate area col
        , lcSurfaceMap        = VU.replicate area z
        , lcTerrainSurfaceMap = VU.replicate area z
        , lcFluidMap          = V.replicate area Nothing
        , lcIceMap            = emptyIceMap
        , lcFlora             = emptyFloraChunkData
        , lcSideDeco          = VU.replicate area 0
        , lcWaterTableMap     = VU.replicate area 0
        , lcMagma             = Nothing
        , lcStructures        = emptyChunkStructures
        }

tilesAt ∷ Int → WorldTileData
tilesAt z = WorldTileData
    { wtdChunks    = HM.singleton (ChunkCoord 0 0) (flatChunkAt (ChunkCoord 0 0) z)
    , wtdMaxChunks = 1
    }

-- * Definitions

-- | @bdBuildWork = 0@ and no @appearing@ state animation, so
--   'Building.Types.currentActivity' reports @built@ immediately and the
--   tick is not held at its appear gate.
portalDef ∷ BuildingDef
portalDef = BuildingDef
    { bdName            = portalDefName
    , bdDisplayName     = portalDefName
    , bdCategory        = "Test"
    , bdDescription     = ""
    , bdTexture         = TextureHandle 0, bdIconTexture         = TextureHandle 0
    , bdTileW           = 1
    , bdTileH           = 1
    , bdPlacement       = "flat_ground"
    , bdIsStarting      = True
    , bdRace            = "acolyte"
    , bdSpriteAnchor    = "diamond_bottom"
    , bdBuildWork       = 0
    , bdMaterials       = HM.empty
    , bdStorageCapacity = 0
    , bdOperations      = []
    , bdAnimations      = HM.empty
    , bdStateAnims      = HM.empty
    , bdPowerDrain      = 0
    , bdPowerNode       = Nothing
    }

portalInstance ∷ BuildingInstance
portalInstance = BuildingInstance
    { biDefName            = portalDefName
    , biPage               = pageA
    , biTexture            = TextureHandle 0
    , biAnchorX            = fst portalTile
    , biAnchorY            = snd portalTile
    , biGridZ              = terrainZA
    , biSpawnedAt          = 0
    , biTileW              = 1
    , biTileH              = 1
    , biSpawnRemaining     = seededRoster
    , biBuildProgress      = 0
    , biMaterialsDelivered = HM.empty
    , biStorage            = []
    }

acolyteDef ∷ UnitDef
acolyteDef = UnitDef
    { udName = rosterFirstUnit, udNamePool = Nothing
    , udDisplayName = Just "Acolyte"
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
    , udNaturalWeapon = Nothing, udModifiers = []
    }

-- * Scene

-- | Both pages live, page A visible and holding the seeded portal, the
--   unit world completely empty. Every queue is drained so an example
--   never inherits the previous one's traffic.
resetScene ∷ EngineEnv → IO ()
resetScene env = do
    wsA ← emptyWorldState
    wsB ← emptyWorldState
    writeIORef (wsTilesRef wsA) (tilesAt terrainZA)
    writeIORef (wsTilesRef wsB) (tilesAt terrainZB)
    writeIORef (wsGenParamsRef wsA) (Just defaultWorldGenParams { wgpWorldSize = 8 })
    writeIORef (wsGenParamsRef wsB) (Just defaultWorldGenParams { wgpWorldSize = 8 })
    writeIORef (worldManagerRef env) emptyWorldManager
        { wmWorlds  = [(pageA, wsA), (pageB, wsB)]
        , wmVisible = [pageA] }
    writeIORef (buildingManagerRef env) emptyBuildingManager
        { bmDefs      = HM.singleton portalDefName portalDef
        , bmInstances = HM.singleton portalBid portalInstance
        , bmNextId    = 2 }
    writeIORef (unitManagerRef env) emptyUnitManager
        { umDefs = HM.singleton rosterFirstUnit acolyteDef }
    writeIORef (gameTimeRef env) 0
    writeIORef (enginePausedRef env) False
    _ ← drainUnitQueue env
    _ ← drainWorldQueue env
    pure ()

-- * Queue readers

drainUnitQueue ∷ EngineEnv → IO [UnitCommand]
drainUnitQueue env = go []
  where
    go acc = do
        mCmd ← Q.tryReadQueue (unitQueue env)
        case mCmd of
            Nothing  → pure (reverse acc)
            Just cmd → go (cmd : acc)

drainWorldQueue ∷ EngineEnv → IO [WorldCommand]
drainWorldQueue env = go []
  where
    go acc = do
        mCmd ← Q.tryReadQueue (worldQueue env)
        case mCmd of
            Nothing  → pure (reverse acc)
            Just cmd → go (cmd : acc)

-- | Only the spawns, as @(defName, gx, gy, gz, faction, page)@.
spawnedUnits ∷ EngineEnv
             → IO [(Text, Double, Double, Int, Faction, WorldPageId)]
spawnedUnits env = do
    cmds ← drainUnitQueue env
    pure [ (n, realToFrac gx, realToFrac gy, gz, f, p)
         | UnitSpawn _ n gx gy gz f p ← cmds ]

-- | The portal's remaining roster count, read straight off the manager
--   the Lua verbs mutate.
rosterRemaining ∷ EngineEnv → IO Int
rosterRemaining env = do
    bm ← readIORef (buildingManagerRef env)
    pure $ maybe (-1) biSpawnRemaining (HM.lookup portalBid (bmInstances bm))

-- | The id allocator's next value and the live instance count — "no unit
--   was created" has to mean both.
unitAllocation ∷ EngineEnv → IO (Word32, Int)
unitAllocation env = do
    um ← readIORef (unitManagerRef env)
    pure (umNextId um, HM.size (umInstances um))

-- * Lua plumbing

newBareLuaBackend ∷ EngineEnv → IO LuaBackendState
newBareLuaBackend env = do
    ls ← createLuaBackendState (luaToEngineQueue env) (luaQueue env)
                                (assetPoolRef env) (nextObjectIdRef env)
                                (inputStateRef env) (loggerRef env)
    stateRef ← newIORef ThreadRunning
    registerLuaAPI (lbsLuaState ls) env ls stateRef
    pure ls

evalDebug ∷ LuaBackendState → Text → IO Text
evalDebug ls src = T.dropAround (≡ '"') <$> executeDebugLua (lbsLuaState ls) src

-- | @__pageSwitch(mode)@ — a SYNCHRONOUS page-selection change driven by
--   the REAL production handlers, callable from inside a Lua stub. This
--   is 'Test.Headless.Building.PageBinding'\'s seam: it is what lets a
--   scenario land a change at an exact point INSIDE one
--   @buildingSpawn.update@ call rather than only before or after it.
--
--   @"toB"@ hides A and shows B. @"aba"@ additionally returns to A, so
--   the final active page is the portal's own and ONLY the selection
--   generation can tell the snapshot is stale.
installPageSwitch ∷ EngineEnv → LuaBackendState → IO ()
installPageSwitch env ls = Lua.runWith (lbsLuaState ls) $ do
    Lua.pushHaskellFunction switchFn
    Lua.setglobal (Lua.Name "__pageSwitch")
  where
    switchFn ∷ Lua.LuaE Lua.Exception Lua.NumResults
    switchFn = do
        modeArg ← Lua.tostring 1
        Lua.liftIO $ do
            logger ← readIORef (loggerRef env)
            let wsc = toWorldSimCapability env
                hide = handleWorldHideCommand wsc logger
                show' = handleWorldShowCommand wsc logger
            case modeArg of
                Just "toB" → hide pageA >> show' pageB
                Just "aba" → do
                    hide pageA
                    show' pageB
                    hide pageB
                    show' pageA
                _ → pure ()
            atomicModifyIORef' (worldManagerRef env) $ \mgr →
                (settleSelectionProjection mgr, ())
        pure 0

-- | Remember the real verb once, before any example wraps it.
rememberRealVerbs ∷ LuaBackendState → IO Text
rememberRealVerbs ls = evalDebug ls
    "_G.__realActiveIds = building.getActiveIds; return 'remembered'"

-- | The AI spy plus the counters every assertion below reads. Installed
--   once; 'resetLua' zeroes it between examples.
installSpies ∷ LuaBackendState → IO Text
installSpies ls = evalDebug ls $ T.intercalate " "
    [ "_G.CALLS = { move = 0, addItem = 0 };"
    , "_G.MOVE_ARGS = {};"
    , "package.loaded['scripts.unit_ai'] = {"
    , "  commandMove = function(uid, x, y, speed, internal)"
    , "    CALLS.move = CALLS.move + 1;"
    , "    MOVE_ARGS = { uid = uid, x = x, y = y,"
    , "                  speed = speed, internal = internal }"
    , "  end,"
    , "};"
    , "_G.__realAddItem = unit.addItem;"
    , "unit.addItem = function(...)"
    , "  CALLS.addItem = CALLS.addItem + 1;"
    , "  return _G.__realAddItem(...)"
    , "end;"
    , "return 'spied'" ]

-- | Put the VM back on the real @building.getActiveIds@, clear the
--   portal's per-spawn Lua state (the module keeps it across a
--   re-@require@ by design) and zero the spy counters.
resetLua ∷ LuaBackendState → IO Text
resetLua ls = evalDebug ls $ T.intercalate " "
    [ "building.getActiveIds = _G.__realActiveIds;"
    , "local BS = require('scripts.building_spawn');"
    , "for k in pairs(BS.state) do BS.state[k] = nil end;"
    , "CALLS.move, CALLS.addItem = 0, 0;"
    , "MOVE_ARGS = {};"
    , "return 'reset'" ]

-- | Wrap @building.getActiveIds@ so a selection change lands between the
--   REAL snapshot and the REAL commit. The ids and generation handed on
--   are the engine's own — the stub only chooses WHEN the change
--   happens, never what the snapshot says.
--
--   @applied:@ modes drive the production world-thread handlers
--   synchronously, so the change is already APPLIED at commit time.
--   @enqueued:@ modes call the production @world.show@ \/ @world.hide@
--   and leave the command sitting in the world queue, which is the
--   in-flight case: @hide@ is an EFFECTIVE change not yet applied,
--   @showA@ a redundant request that changes nothing.
stubSnapshotThenSwitch ∷ LuaBackendState → Text → IO Text
stubSnapshotThenSwitch ls mode = evalDebug ls $ T.concat
    [ "building.getActiveIds = function() "
    , "  local ids, gen = _G.__realActiveIds(); "
    , case mode of
        "applied:toB"   → "__pageSwitch('toB'); "
        "applied:aba"   → "__pageSwitch('aba'); "
        "enqueued:hide" → T.concat ["world.hide('", unWorldPageId pageA, "'); "]
        "enqueued:showA" → T.concat ["world.show('", unWorldPageId pageA, "'); "]
        _               → ""
    , "  return ids, gen "
    , "end; return 'stubbed'" ]

-- | One @buildingSpawn.update@ through the real module.
portalTick ∷ LuaBackendState → IO Text
portalTick ls = evalDebug ls
    "require('scripts.building_spawn').update(0.016); return 'ticked'"

-- | The spy counters as @move|addItem@.
spyCounts ∷ LuaBackendState → IO Text
spyCounts ls = evalDebug ls
    "return tostring(CALLS.move) .. '|' .. tostring(CALLS.addItem)"

-- | The portal's Lua per-spawn state, folded to
--   @lastUid|spawnFailures|lastSpawnedAt@. A refused tick must leave all
--   three untouched: it is not a spawn that FAILED, so none of the retry
--   bookkeeping may fire either.
portalLuaState ∷ LuaBackendState → IO Text
portalLuaState ls = evalDebug ls $ T.concat
    [ "local s = require('scripts.building_spawn').state[", tshow (unBuildingId portalBid), "]; "
    , "if not s then return 'absent' end; "
    , "return tostring(s.lastUid) .. '|' .. tostring(s.spawnFailures) "
    , "  .. '|' .. tostring(s.lastSpawnedAt)" ]

-- | The move the success path commands, folded to @uid|x|y|internal@.
moveArgs ∷ LuaBackendState → IO Text
moveArgs ls = evalDebug ls $ T.concat
    [ "local m = MOVE_ARGS; if m.uid == nil then return 'none' end; "
    , "return tostring(m.uid) .. '|' .. tostring(m.x) "
    , "  .. '|' .. tostring(m.y) .. '|' .. tostring(m.internal)" ]

-- | Drain the world queue through the REAL dispatcher, so an example can
--   prove what an in-flight command does once it lands.
runWorldQueue ∷ EngineEnv → IO ()
runWorldQueue env = do
    logger ← readIORef (loggerRef env)
    cmds ← drainWorldQueue env
    forM_ cmds $ \cmd → do
        handleWorldCommand env logger cmd
        atomicModifyIORef' (worldManagerRef env) $ \mgr →
            (settleSelectionProjection mgr, ())

-- | The active page id, so a scenario can state what the switch actually
--   did rather than assume it.
activePageId ∷ EngineEnv → IO (Maybe WorldPageId)
activePageId env = do
    wm ← readIORef (worldManagerRef env)
    pure $ case wmVisible wm of
        (p:_) → Just p
        []    → Nothing

-- * Spec

spec ∷ Spec
spec = describe "Portal spawn page binding (#1686)" $ aroundAll setup $ do
    fixtureSpec
    staleSpec
    acceptedSpec
  where
    -- Isolation wraps the boot, not the other way round (#1357): engine
    -- init is itself a config writer. It stays open for the whole group
    -- because the engine booted inside it — @scripts/@ is symlinked
    -- there, so the real building_spawn Lua still loads.
    setup act = withIsolatedResourceRoot $ do
        EngineInitResult env ← initializeEngineHeadless
        ls ← newBareLuaBackend env
        installPageSwitch env ls
        _ ← rememberRealVerbs ls
        _ ← installSpies ls
        act (env, ls)

-- | The seam really is a seam, and the fixture really is roster-ready.
--   Without this, a "nothing happened" assertion could pass against a
--   tick that was never eligible in the first place.
fixtureSpec ∷ SpecWith (EngineEnv, LuaBackendState)
fixtureSpec = describe "the fixture is a live, roster-ready portal" $ do

    it "enumerates the portal on page A with a generation, from the real \
       \active-page snapshot" $ \(env, ls) → do
        resetScene env
        _ ← resetLua ls
        raw ← evalDebug ls
            "local ids, gen = building.getActiveIds(); \
            \return tostring(#ids) .. '|' .. tostring(ids[1]) \
            \  .. '|' .. tostring(gen)"
        raw `shouldBe` "1|1|0"

    it "starts with a positive countdown already seeded, so a later \
       \unchanged reading is not ensureState's first-sight seeding" $
        \(env, _) → do
            resetScene env
            rosterRemaining env `shouldReturn` seededRoster

    it "moves the selection generation when the seam switches pages, and \
       \leaves it alone when the request is ineffective" $ \(env, ls) → do
        resetScene env
        _ ← resetLua ls
        before' ← evalDebug ls "local _, g = building.getActiveIds(); return tostring(g)"
        _ ← evalDebug ls "__pageSwitch('toB'); return 'ok'"
        afterSwitch ← evalDebug ls "local _, g = building.getActiveIds(); return tostring(g)"
        before' `shouldBe` "0"
        afterSwitch `shouldNotBe` before'

staleSpec ∷ SpecWith (EngineEnv, LuaBackendState)
staleSpec = describe "a tick whose page stopped being active" $ do

    it "spawns nothing when the page was hidden after the snapshot: no \
       \UnitSpawn queued, no id allocated, no unit on either page, no \
       \roster entry consumed, and no walk commanded" $ \(env, ls) → do
        resetScene env
        _ ← resetLua ls
        _ ← stubSnapshotThenSwitch ls "applied:toB"
        _ ← portalTick ls
        activePageId env `shouldReturn` Just pageB
        spawnedUnits env `shouldReturn` []
        unitAllocation env `shouldReturn` (1, 0)
        rosterRemaining env `shouldReturn` seededRoster
        spyCounts ls `shouldReturn` "0|0"

    it "refuses an A->B->A round trip too, where the active page id is \
       \the portal's own again and only the generation can tell" $
        \(env, ls) → do
            resetScene env
            _ ← resetLua ls
            _ ← stubSnapshotThenSwitch ls "applied:aba"
            _ ← portalTick ls
            -- The page id is back to the one the snapshot named: an
            -- id comparison alone would accept this tick.
            activePageId env `shouldReturn` Just pageA
            spawnedUnits env `shouldReturn` []
            unitAllocation env `shouldReturn` (1, 0)
            rosterRemaining env `shouldReturn` seededRoster
            spyCounts ls `shouldReturn` "0|0"

    it "refuses a change that is only ENQUEUED, so a tick racing an \
       \in-flight world.hide is not optimistically accepted" $
        \(env, ls) → do
            resetScene env
            _ ← resetLua ls
            _ ← stubSnapshotThenSwitch ls "enqueued:hide"
            _ ← portalTick ls
            -- Nothing has been applied yet: page A is still the visible
            -- head, and the hide is still sitting in the world queue.
            activePageId env `shouldReturn` Just pageA
            spawnedUnits env `shouldReturn` []
            unitAllocation env `shouldReturn` (1, 0)
            rosterRemaining env `shouldReturn` seededRoster
            spyCounts ls `shouldReturn` "0|0"
            -- and the queued command really was the hide.
            runWorldQueue env
            activePageId env `shouldReturn` Nothing

    it "leaves the tick's own retry bookkeeping untouched: a refusal is \
       \not a spawn that failed, so nothing is stamped and the next tick \
       \behaves as if this one never ran" $ \(env, ls) → do
        resetScene env
        _ ← resetLua ls
        _ ← stubSnapshotThenSwitch ls "applied:toB"
        _ ← portalTick ls
        portalLuaState ls `shouldReturn` "nil|nil|nil"

acceptedSpec ∷ SpecWith (EngineEnv, LuaBackendState)
acceptedSpec = describe "a tick whose page is still active" $ do

    it "spawns on the portal's own page with that page's elevation, \
       \commands the internal walk-out, and consumes exactly one roster \
       \entry" $ \(env, ls) → do
        resetScene env
        _ ← resetLua ls
        _ ← portalTick ls
        spawns ← spawnedUnits env
        spawns `shouldBe`
            [ ( rosterFirstUnit, fst spawnPos, snd spawnPos
              , terrainZA, FactionPlayer, pageA ) ]
        unitAllocation env `shouldReturn` (2, 0)
        rosterRemaining env `shouldReturn` (seededRoster - 1)
        -- commandMove fires exactly once, from the success-only block
        -- the stale cases above proved is never entered.
        spyCounts ls `shouldReturn` "1|0"
        -- The walk-out target is the portal's own walk_to_offset,
        -- and it is INTERNAL (#1216) so the fresh acolyte does not end
        -- up holding the tile the roster picked.
        moveArgs ls `shouldReturn` T.concat
            [ "1.0|", tshow (fst walkPos), "|", tshow (snd walkPos), "|true" ]

    it "is not rejected by an INEFFECTIVE selection request landing \
       \mid-tick: a redundant world.show moves neither generation" $
        \(env, ls) → do
            resetScene env
            _ ← resetLua ls
            _ ← stubSnapshotThenSwitch ls "enqueued:showA"
            _ ← portalTick ls
            spawns ← spawnedUnits env
            spawns `shouldBe`
                [ ( rosterFirstUnit, fst spawnPos, snd spawnPos
                  , terrainZA, FactionPlayer, pageA ) ]
            rosterRemaining env `shouldReturn` (seededRoster - 1)
            spyCounts ls `shouldReturn` "1|0"

    it "leaves every UNBOUND unit.spawn caller alone — no binding, no \
       \refusal, even with the page hidden underneath it" $
        \(env, ls) → do
            resetScene env
            _ ← resetLua ls
            _ ← evalDebug ls "__pageSwitch('toB'); return 'ok'"
            uid ← evalDebug ls $ T.concat
                [ "return tostring(unit.spawn('", rosterFirstUnit
                , "', 2.5, 3.5, nil, 'player', '", unWorldPageId pageA, "'))" ]
            uid `shouldBe` "1.0"
            spawns ← spawnedUnits env
            map (\(n, _, _, _, _, p) → (n, p)) spawns
                `shouldBe` [(rosterFirstUnit, pageA)]
