{-# LANGUAGE OverloadedStrings #-}
-- | The scene and plumbing of the "Lua gameplay tags" group (#2706).
--
--   One session with two pages. Page P holds every target form: four
--   units (one carrying a satchel ⊃ pouch ⊃ ration tree, an equipped
--   dagger and a worn habit), a storage building (a stored crate ⊃ bar,
--   and a delivered bar), a ground item whose id is above 2^53, a
--   location, and three resident chunks of hand-built terrain (the
--   "Gameplay.Tags.Residency" layout: columns store z 5 … 10 with an AIR
--   cell at z 7) carrying wild co-tenant plants, a planted row crop and
--   a groundcover plot. Page Q repeats the terrain and a location with
--   the same ids, so equal keys on two pages are exercised.
--
--   Ids overlap across categories on purpose: unit 1, building 1, item 1
--   and location 1 are four different targets.
--
--   A pump thread plays the world thread: it drains the world queue
--   through the REAL dispatcher ('handleWorldCommand'), so a tile or
--   plant verb's queued edit is applied exactly as in the engine. A
--   one-shot hook runs just before the pump handles a tag edit, which is
--   how the stale-work cases change the page underneath a queued edit.
module Test.Headless.Lua.GameplayTags.Scene
    ( -- * Fixture
      Fx(..)
    , withFixture
    , resetScene
    , newPage
    , armTagEditHook
      -- * Running Lua
    , run
    , runRaw
      -- * Target literals
    , pageP, pageQ, pageText
    , unitT, buildingT, itemT, locationT, plantT, plotT, tileT
      -- * Rendering expected targets
    , render, renderList
      -- * Scene identities
    , worldSize
    , colTile, otherTile, plotTile, farTile
    , oakOcc, berryOcc, rowCrop, farOak
    , satchel, pouch, ration, dagger, habit, crate, crateBar, deliveredBar, bigItem, otherItem
    , chunkA, chunkC
    , cell, plotOf, occOf
      -- * Owner views
    , view, assignments, assignmentOf
      -- * Residency
    , evictAAndC, readmitC, editChunkA
    ) where

import UPrelude
import Test.Hspec
import qualified Data.HashMap.Strict as HM
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU
import Control.Concurrent (forkIO, killThread)
import Control.Exception (SomeException, bracket, try)
import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef, writeIORef)

import Building.Types (BuildingId(..), BuildingInstance(..), BuildingManager(..), emptyBuildingManager)
import Engine.Asset.Handle (TextureHandle(..))
import Engine.Core.Init (EngineInitResult(..))
import Engine.Core.Log (LoggerState)
import Engine.Core.State (EngineEnv(..))
import qualified Engine.Core.Queue as Q
import Engine.Scripting.Lua.Thread.Console (executeDebugLua)
import Engine.Scripting.Lua.Types (LuaBackendState(..))
import Gameplay.Tags.Live
import Gameplay.Tags.Page (plotStoreFromPlots)
import Gameplay.Tags.PageLive (reconcileAdmittedPageTags)
import Gameplay.Tags.Types
import Item.Ground (spawnGroundItem)
import Item.Types (ItemInstance(..), ItemStorage(..))
import Language.Semantic.Types (ConceptId(..))
import Location.Bounds (RelBounds(..))
import Location.Instance (LocationInstanceId(..), LocationInstances(..), allocateLocationInstance)
import Location.Types (LocationDef(..), LocationNaming(..))
import Test.Headless.Harness.Isolation (withIsolatedResourceRoot)
import Test.Headless.Harness.Log (initializeEngineHeadlessQuiet)
import Test.Headless.Unit.TransferApi
    (mkBuilding, mkItem, mkUnit, minimalDef, newBareLuaBackend, storageDef)
import Unit.Faction (Faction(..))
import Unit.Types (UnitId(..), UnitInstance(..), UnitManager(..), emptyUnitManager)
import World.Chunk.Admit (releaseEvictedChunks)
import World.Chunk.Residency (canonicalChunkCoord)
import World.Chunk.Types (ChunkCoord(..), ColumnTiles(..), LoadedChunk(..), chunkSize)
import World.Command.Types (WorldCommand(..))
import World.Edit.Apply (applyEdit)
import World.Edit.Types (WorldEdit)
import World.Flora.CropPlot (CropPlot, CropPlotOf(..))
import World.Flora.Designation (replaceChunkForgettingFlora)
import World.Flora.Identity
    (FloraInstanceId, floraInstanceIdToLua, generatedFloraInstanceId, plantedFloraInstanceId)
import World.Flora.Types
    ( FloraCatalog(..), FloraChunkData(..), FloraContext(..), FloraId(..)
    , FloraInstance(..)
    , emptyFloraCatalog, newFloraSpecies )
import World.Fluid.Types (emptyIceMap)
import Structure.Types (emptyChunkStructures)
import World.Generate.Types (WorldGenParams(..), defaultWorldGenParams)
import World.Page.Types (WorldPageId(..))
import World.State.Types (WorldManager(..), WorldState(..), emptyWorldState, emptyWorldManager)
import World.Thread.Command (handleWorldCommand)
import World.Tile.Types (WorldTileData(..), evictDistantChunksWithReport, insertChunk, lookupChunk)

-- * Fixture

data Fx = Fx
    { fxEnv    ∷ EngineEnv
    , fxLs     ∷ LuaBackendState
    , fxHook   ∷ IORef (Maybe (IO ()))
    , fxLogger ∷ LoggerState
    }

-- | An isolated engine, the production Lua registration, the Lua
--   helpers below, and the world-thread pump for the group's lifetime.
withFixture ∷ (Fx → IO ()) → IO ()
withFixture act = withIsolatedResourceRoot $ do
    EngineInitResult env ← initializeEngineHeadlessQuiet
    ls ← newBareLuaBackend env
    loaded ← executeDebugLua (lbsLuaState ls) helpersLua
    loaded `shouldBe` "\"ok\""
    hook ← newIORef Nothing
    logger ← readIORef (loggerRef env)
    bracket (forkIO (pump env logger hook)) killThread $ \_ →
        act (Fx env ls hook logger)

-- | The world thread, for this group: the real dispatcher over the
--   real queue.
pump ∷ EngineEnv → LoggerState → IORef (Maybe (IO ())) → IO ()
pump env logger hook = forever $ do
    next ← Q.readQueueTimeout 20000 (worldQueue env)
    forM_ next $ \cmd → do
        case cmd of
            WorldEditPageTags{} → atomicModifyIORef' hook (Nothing,) ≫= sequence_
            _ → pure ()
        void (try (handleWorldCommand env logger cmd) ∷ IO (Either SomeException ()))

-- | Run this once, on the pump, just before it handles the next tag
--   edit.
armTagEditHook ∷ Fx → IO () → IO ()
armTagEditHook fx h = writeIORef (fxHook fx) (Just h)

-- | @__r(...)@ renders a verb's answer as one string: @nil|<message>@
--   for a refusal, the rendered value otherwise (with @|<n>@ appended
--   when a success returns anything but exactly one value). Tables of
--   targets render each target's fields sorted, and a float is marked,
--   so an integer that came back as a float cannot pass.
helpersLua ∷ Text
helpersLua = T.unlines
    [ "_G.__s = function(v)"
    , "  if type(v) ~= 'table' then"
    , "    if math.type(v) == 'float' then return 'float:' .. tostring(v) end"
    , "    return tostring(v)"
    , "  end"
    , "  if rawget(v, 'kind') ~= nil then"
    , "    local keys = {}"
    , "    for k in pairs(v) do keys[#keys + 1] = k end"
    , "    table.sort(keys)"
    , "    local parts = {}"
    , "    for _, k in ipairs(keys) do parts[#parts + 1] = k .. '=' .. __s(v[k]) end"
    , "    return '{' .. table.concat(parts, ' ') .. '}'"
    , "  end"
    , "  local parts = {}"
    , "  for i = 1, #v do parts[i] = __s(v[i]) end"
    , "  return '[' .. table.concat(parts, ',') .. ']'"
    , "end"
    , "_G.__r = function(...)"
    , "  local n = select('#', ...)"
    , "  local a, b = ..."
    , "  if a == nil then return 'nil|' .. tostring(b) end"
    , "  if n ~= 1 then return __s(a) .. '|' .. n end"
    , "  return __s(a)"
    , "end"
    , "return 'ok'" ]

-- | Evaluate a Lua chunk and return its single string result, unquoted.
runRaw ∷ Fx → Text → IO Text
runRaw fx src = do
    out ← executeDebugLua (lbsLuaState (fxLs fx)) src
    out `shouldNotSatisfy` (\t → "error:" `T.isPrefixOf` t ∨ "syntax error:" `T.isPrefixOf` t)
    pure (unquote out)
  where
    unquote t = fromMaybe t (T.stripPrefix "\"" t ≫= T.stripSuffix "\"")

-- | @run fx "gameplayTags.add(…)"@: one call, rendered by @__r@.
run ∷ Fx → Text → IO Text
run fx call = runRaw fx ("return __r(" <> call <> ")")

-- * Target literals

pageP, pageQ ∷ WorldPageId
pageP = WorldPageId "lua_tags_page"
pageQ = WorldPageId "lua_tags_other"

pageText ∷ WorldPageId → Text
pageText = unWorldPageId

unitT, buildingT, itemT ∷ Integer → Text
unitT n = "{kind='unit', id=" <> tshow n <> "}"
buildingT n = "{kind='building', id=" <> tshow n <> "}"
itemT n = "{kind='item', id=" <> tshow n <> "}"

locationT ∷ WorldPageId → Int → Text
locationT p n = "{kind='location', page='" <> pageText p <> "', id=" <> tshow n <> "}"

plantT ∷ WorldPageId → FloraInstanceId → Text
plantT p fid = "{kind='plant', page='" <> pageText p <> "', id="
    <> tshow (floraInstanceIdToLua fid) <> "}"

plotT ∷ WorldPageId → (Int, Int) → Text
plotT p (x, y) = "{kind='plot', page='" <> pageText p <> "', x=" <> tshow x <> ", y=" <> tshow y <> "}"

tileT ∷ WorldPageId → (Int, Int) → Int → Text
tileT p (x, y) z = "{kind='tile', page='" <> pageText p <> "', x=" <> tshow x
    <> ", y=" <> tshow y <> ", z=" <> tshow z <> "}"

-- * Rendering expected targets

-- | A target as @__s@ renders it.
render ∷ TagTarget → Text
render = \case
    TargetUnit (UnitId n) → "{id=" <> tshow n <> " kind=unit}"
    TargetBuilding (BuildingId n) → "{id=" <> tshow n <> " kind=building}"
    TargetItem i → "{id=" <> tshow (itemTargetId i) <> " kind=item}"
    TargetLocation (LocationTarget p (LocationInstanceId n)) →
        "{id=" <> tshow n <> " kind=location page=" <> pageText p <> "}"
    TargetPlant pt → case (plantTargetId pt, plantTargetPlotTile pt) of
        (Just fid, _) → "{id=" <> tshow (floraInstanceIdToLua fid) <> " kind=plant page="
                          <> pageText (plantTargetPage pt) <> "}"
        (_, Just (x, y)) → "{kind=plot page=" <> pageText (plantTargetPage pt)
                             <> " x=" <> tshow x <> " y=" <> tshow y <> "}"
        _ → "{?}"
    TargetTile t → "{kind=tile page=" <> pageText (tileTargetPage t) <> " x=" <> tshow (tileTargetX t)
        <> " y=" <> tshow (tileTargetY t) <> " z=" <> tshow (tileTargetZ t) <> "}"

renderList ∷ [TagTarget] → Text
renderList ts = "[" <> T.intercalate "," (map render ts) <> "]"

-- * The scene

worldSize ∷ Int
worldSize = 8

params ∷ WorldGenParams
params = defaultWorldGenParams { wgpWorldSize = worldSize }

chunkA, chunkB, chunkC ∷ ChunkCoord
chunkA = ChunkCoord 0 0
chunkB = ChunkCoord 1 0
chunkC = ChunkCoord 0 3

colTile, otherTile, plotTile, farTile ∷ (Int, Int)
colTile   = (3, 4)
otherTile = (6, 9)
plotTile  = (10, 2)
farTile   = (3, 3 * 16 + 4)

cell ∷ WorldPageId → (Int, Int) → Int → TagTarget
cell page (x, y) z = TargetTile (mkTileTarget (canonicalChunkCoord params) page x y z)

plotOf ∷ WorldPageId → (Int, Int) → TagTarget
plotOf page (x, y) = TargetPlant (mkPlotTarget worldSize page x y)

occOf ∷ WorldPageId → FloraInstanceId → TagTarget
occOf page fid = TargetPlant (fromMaybe (error "none") (mkPlantTarget page fid))

column ∷ ColumnTiles
column = ColumnTiles
    { ctStartZ = 5, ctMats = VU.fromList [1, 1, 0, 1, 1, 1]
    , ctSlopes = VU.replicate 6 0, ctVeg = VU.replicate 6 0 }

oakId, berryId, cropId ∷ FloraId
oakId = FloraId 1; berryId = FloraId 2; cropId = FloraId 3

catalog ∷ FloraCatalog
catalog = emptyFloraCatalog
    { fcSpecies = HM.fromList
        [ (1, newFloraSpecies "lua_oak" (TextureHandle 0))
        , (2, newFloraSpecies "lua_berry" (TextureHandle 0))
        , (3, newFloraSpecies "lua_wheat" (TextureHandle 0)) ]
    , fcNextId = 4 }

oakOcc, berryOcc, rowCrop, farOak ∷ FloraInstanceId
oakOcc   = generatedFloraInstanceId (pageText pageP) 3 4 "lua_oak" 0
berryOcc = generatedFloraInstanceId (pageText pageP) 3 4 "lua_berry" 0
rowCrop  = plantedFloraInstanceId 7
farOak   = generatedFloraInstanceId (pageText pageP) 3 52 "lua_oak" 0

plant ∷ (Int, Int) → FloraId → FloraInstanceId → FloraInstance
plant (gx, gy) fid iid = FloraInstance
    { fiSpecies = fid, fiTileX = fromIntegral (gx `mod` chunkSize)
    , fiTileY = fromIntegral (gy `mod` chunkSize)
    , fiOffU = 0, fiOffV = 0, fiZ = 10, fiAge = 1, fiHealth = 1
    , fiVariant = 0, fiBaseWidth = 8, fiInstanceId = iid, fiChopDesignated = False, fiContext = ContextWild }

chunkWith ∷ ChunkCoord → [FloraInstance] → LoadedChunk
chunkWith coord flora =
    let area = chunkSize * chunkSize
    in LoadedChunk
        { lcCoord = coord, lcTiles = V.replicate area column
        , lcSurfaceMap = VU.replicate area 10
        , lcTerrainSurfaceMap = VU.replicate area 10
        , lcFluidMap = V.replicate area Nothing
        , lcIceMap = emptyIceMap, lcFlora = FloraChunkData flora
        , lcSideDeco = VU.empty, lcWaterTableMap = VU.replicate area (-100)
        , lcMagma = Nothing, lcStructures = emptyChunkStructures }

freshChunkA, freshChunkB, freshChunkC ∷ LoadedChunk
freshChunkA = chunkWith chunkA
    [ plant colTile oakId oakOcc, plant colTile berryId berryOcc
    , plant (7, 9) cropId rowCrop ]
freshChunkB = chunkWith chunkB []
freshChunkC = chunkWith chunkC [plant farTile oakId farOak]

wheatPlot ∷ CropPlot
wheatPlot = CropPlot cropId 0 1.0

ruinDef ∷ LocationDef
ruinDef = LocationDef
    { ldId = "lua_tag_ruin", ldLabel = "Tagged Ruin", ldType = "ruin"
    , ldBuilder = "room_small", ldAnchor = [], ldMaxCount = 0
    , ldMinSpacing = 0, ldContents = [], ldBounds = RelBounds (-1) (-1) 1 1
    , ldMapIcon = Nothing
    , ldNaming = LocationNaming
        { lnHeads = [ConceptId "KEEP"], lnModifiers = [ConceptId "ASH"] } }

-- | One page: three resident chunks, one plot, location 1, and (with
--   @True@) a ground item. Also a replacement page whose terrain is live,
--   so an edit that wrongly reached it would land.
newPage ∷ Bool → IO WorldState
newPage withGround = do
    ws ← emptyWorldState
    let lis0 = (wgpLocationInstances params) { lisNextId = 1 }
        lis = case allocateLocationInstance Nothing chunkA ruinDef lis0 of
            Left e → error ("allocateLocationInstance: " <> show e)
            Right (_, l) → l
    writeIORef (wsGenParamsRef ws) (Just params { wgpLocationInstances = lis })
    writeIORef (wsTilesRef ws) WorldTileData
        { wtdChunks = HM.fromList [ (chunkA, freshChunkA), (chunkB, freshChunkB)
                                  , (chunkC, freshChunkC) ]
        , wtdMaxChunks = 200, wtdBounds = Nothing }
    writeIORef (wsCropPlotsRef ws) (plotStoreFromPlots (HM.fromList [(plotTile, wheatPlot)]))
    when withGround $ atomicModifyIORef' (wsGroundItemsRef ws) $ \gis →
        (fst (spawnGroundItem (mkItem "ration" bigItem 0.5) 5.5 5.5 gis), ())
    pure ws

-- | Item ids by role. 'bigItem' is 2^53 + 1, which no float can name.
satchel, pouch, ration, dagger, habit, crate, crateBar, deliveredBar, bigItem, otherItem ∷ Word64
satchel = 1; pouch = 2; ration = 3; dagger = 4; habit = 5; crate = 6; crateBar = 7
deliveredBar = 8; bigItem = 9007199254740993; otherItem = 20

roomy ∷ ItemStorage
roomy = ItemStorage { isWeightCapacity = 50, isBulkCapacity = 50 }

container ∷ Text → Word64 → [ItemInstance] → ItemInstance
container name iid kids = (mkItem name iid 0.5) { iiContents = kids, iiStorage = Just roomy }

-- | Rebuild both pages and every manager; nothing is tagged.
resetScene ∷ Fx → IO ()
resetScene fx = do
    let env = fxEnv fx
    writeIORef (fxHook fx) Nothing
    wsP ← newPage True
    wsQ ← newPage False
    epoch ← wmSessionEpoch ⊚ readIORef (worldManagerRef env)
    writeIORef (worldManagerRef env) emptyWorldManager
        { wmWorlds = [(pageP, wsP), (pageQ, wsQ)], wmVisible = [pageP]
        , wmSessionEpoch = epoch }
    writeIORef (floraCatalogRef env) catalog
    let soldier tile inv worn = (mkUnit "acolyte" FactionPlayer tile 100 inv worn)
            { uiPage = pageP }
        acolyte = (soldier (10, 10)
                     [container "satchel" satchel [container "pouch" pouch [mkItem "ration" ration 0.5]]]
                     [mkItem "acolyte_habit" habit 1.0])
            { uiEquipment = HM.singleton "right_hand" (mkItem "steel_dagger" dagger 1.0) }
        stranger = (mkUnit "acolyte" FactionPlayer (10, 10) 100
                           [mkItem "steel_bar" otherItem 1.0] []) { uiPage = pageQ }
    writeIORef (unitManagerRef env) emptyUnitManager
        { umDefs = HM.fromList [("acolyte", minimalDef "acolyte" "Acolyte")]
        , umInstances = HM.fromList
            [ (UnitId 1, acolyte)
            , (UnitId 2, soldier (11, 10) [] [])
            , (UnitId 3, stranger)
            , (UnitId 5, soldier (12, 10) [] [])
            , (UnitId 6, soldier (13, 10) [] []) ]
        , umNextId = 7 }
    writeIORef (buildingManagerRef env) emptyBuildingManager
        { bmDefs = HM.fromList [("cargo_hold", storageDef "cargo_hold" "Cargo Hold" (1, 1) 0 200)]
        , bmInstances = HM.fromList
            [ (BuildingId 1, (mkBuilding "cargo_hold" (11, 9) (1, 1)
                                [container "crate" crate [mkItem "steel_bar" crateBar 1.0]])
                                { biPage = pageP
                                , biMaterialsDelivered =
                                    HM.singleton "steel_bar" [mkItem "steel_bar" deliveredBar 1.0] }) ]
        , bmNextId = 2 }
    writeIORef (nextItemInstanceIdRef env) 100

-- * Owner views

view ∷ Fx → IO LiveTagView
view fx = let env = fxEnv fx
          in captureLiveTagView (unitManagerRef env) (buildingManagerRef env) (worldManagerRef env)

-- | Every assignment the owners hold, retained ones included: what a
--   refused request must leave unchanged.
assignments ∷ Fx → IO [(TagTarget, Set.Set GameplayTag)]
assignments fx = do
    -- The registry is built from the owners' whole indexes, so retained
    -- assignments of non-resident targets are included.
    liveTagAssignments ⊚ view fx

assignmentOf ∷ Fx → TagTarget → IO (Maybe (Set.Set GameplayTag))
assignmentOf fx t = lookup t ⊚ assignments fx

-- * Residency, through the pipeline's own functions

pageState ∷ Fx → WorldPageId → IO WorldState
pageState fx page = do
    wm ← readIORef (worldManagerRef (fxEnv fx))
    maybe (fail ("no live page " <> show page)) pure (lookup page (wmWorlds wm))

-- | Evict chunks A and C of page P (the camera on B, room for one).
evictAAndC ∷ Fx → IO ()
evictAAndC fx = do
    ws ← pageState fx pageP
    td ← readIORef (wsTilesRef ws)
    let (td', evicted) = evictDistantChunksWithReport chunkB 0 td { wtdMaxChunks = 1 }
    releaseEvictedChunks ws pageP params evicted
    writeIORef (wsTilesRef ws) td' { wtdMaxChunks = 200 }
    lookupChunk chunkC td' `shouldSatisfy` isNothing

-- | Readmit chunk C through the admission hook.
readmitC ∷ Fx → IO ()
readmitC fx = do
    ws ← pageState fx pageP
    atomicModifyIORef' (wsTilesRef ws) $ \td → (insertChunk freshChunkC td, ())
    readIORef (wsTilesRef ws) ≫= reconcileAdmittedPageTags (fxLogger fx) ws pageP [freshChunkC]

-- | An in-place edit of page P's chunk A, as every edit handler applies
--   it.
editChunkA ∷ Fx → WorldEdit → IO ()
editChunkA fx e = do
    ws ← pageState fx pageP
    td ← readIORef (wsTilesRef ws)
    lc ← maybe (fail "chunk A not resident") pure (lookupChunk chunkA td)
    replaceChunkForgettingFlora ws lc (applyEdit e lc)

