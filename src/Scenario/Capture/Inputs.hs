{-# LANGUAGE Strict #-}
-- | Everything a scenario capture reads, taken from ONE instant (#2714,
--   SCN-14 of epic #2698).
--
--   The capture is a pure function of 'CaptureInputs' ("Scenario.Capture.Project"):
--   the unit and building managers, the page's tile store, edit log, ground
--   items, power nodes, tags, crop plots, regrowth timers and generation
--   parameters, the session's item identities and tags, and the registries
--   the adapters read definitions from. Reading them is the only step that
--   touches live state, and it runs on the WORLD thread inside the save
--   barrier's capture window — after every other owner has acknowledged the
--   final quiescence pass and parked — so the values below are one coherent
--   state and nothing mutates them while the projection is evaluated.
--
--   'readSessionInputs' takes the session's managers as arguments rather
--   than reading them from the engine, so the same projection captures a
--   live session and one restored from an ordinary save.
module Scenario.Capture.Inputs
    ( CaptureInputs(..)
    , SessionParts(..)
    , readSessionInputs
    , readLiveInputs
    , liveTestingPage
    ) where

import UPrelude
import Data.IORef (readIORef)
import Building.Types (BuildingDef, BuildingManager(..))
import Engine.Core.Capability.Building (BuildingCapability(..), toBuildingCapability)
import Engine.Core.Capability.ContentRegistries
    (ContentRegistriesCapability(..), toContentRegistriesCapability)
import Engine.Core.Capability.ContentRegistriesView
    (ContentRegistriesViewCapability(..), toContentRegistriesViewCapability)
import Engine.Core.Capability.RenderHandoff (RenderHandoffCapability(..), toRenderHandoffCapability)
import Engine.Core.Capability.RenderView (RenderViewCapability(..), toRenderViewCapability)
import Engine.Core.Capability.UnitCombat (UnitCombatCapability(..), toUnitCombatCapability)
import Engine.Core.Capability.WorldSim (WorldSimCapability(..), toWorldSimCapability)
import Engine.Core.ReadOnlyRef (readReadOnlyRef)
import Engine.Core.State (EngineEnv)
import Engine.Graphics.Camera (Camera2D(..))
import qualified Data.HashMap.Strict as HM
import Gameplay.Tags.Index (CategoryIndex)
import Gameplay.Tags.Tag (ItemTarget)
import Gameplay.Tags.Page (CropPlotStore(..), PageTagsSnapshot(..), capturePageTags)
import Item.Ground (GroundItems)
import Item.Types (ItemManager)
import Location.Instance (LocationInstances, emptyLocationInstances)
import Location.Types (LocationRegistry)
import Power.Types (PowerNodes)
import Scenario.Items.Refs (ItemRefs)
import Scenario.World.Capture (WorldSnapshot(..))
import Structure.Palette (TexPalette)
import Structure.ArtCatalog (StructureArtCatalog)
import Unit.Physiology.Types (PhysiologyRegistry)
import Unit.Types (UnitManager(..))
import World.Arena.Bounds (ArenaBounds)
import World.Chunk.Types (LoadedChunk)
import World.Generate.Types (WorldGenParams(..), isArenaParams)
import World.Page.Types (WorldPageId)
import World.Render.Camera.Types (WorldCamera(..))
import World.State.Types
import World.Tile.Types (WorldTileData(..))
import World.Time.Types (defaultCalendarConfig, worldAbsoluteDay)

-- | The session a capture reads, as the caller holds it.
data SessionParts = SessionParts
    { spPage       ∷ !WorldPageId
    , spWorld      ∷ !WorldState
    , spUnits      ∷ !UnitManager
    , spBuildings  ∷ !BuildingManager
    , spItemRefs   ∷ !ItemRefs
    , spItemTags   ∷ !(CategoryIndex ItemTarget)
    , spPalette    ∷ !TexPalette
    , spLiveCamera ∷ !(Maybe (Float, Float))
      -- ^ the live camera when this page owns it; 'Nothing' reads the
      --   page's own stored camera
    }

-- | One instant of the session, ready to project.
data CaptureInputs = CaptureInputs
    { ciPage       ∷ !WorldPageId
    , ciBounds     ∷ !(Maybe ArenaBounds)
    , ciSeed       ∷ !Word64
    , ciCamera     ∷ !(Float, Float)
    , ciUnits      ∷ !UnitManager
    , ciBuildings  ∷ !BuildingManager
    , ciBuildingDefs ∷ !(HM.HashMap Text BuildingDef)
    , ciItems      ∷ !ItemManager
    , ciItemRefs   ∷ !ItemRefs
    , ciItemTags   ∷ !(CategoryIndex ItemTarget)
    , ciGround     ∷ !GroundItems
    , ciPowerNodes ∷ !PowerNodes
    , ciLocations  ∷ !LocationInstances
    , ciLocationDefs ∷ !LocationRegistry
    , ciPhysiology ∷ !PhysiologyRegistry
    , ciNow        ∷ !Double
    , ciStructureArt ∷ !StructureArtCatalog
    , ciPalette    ∷ !TexPalette
    , ciWorld      ∷ !WorldSnapshot
    , ciChunks     ∷ ![LoadedChunk]
    }

-- | Read the session's parts and the registries beside them. Reads, never
--   writes; call it only at the snapshot boundary.
readSessionInputs ∷ EngineEnv → SessionParts → IO CaptureInputs
readSessionInputs env parts = do
    let wsc = toWorldSimCapability env
        regs = toContentRegistriesCapability env
        view = toContentRegistriesViewCapability env
        render = toRenderHandoffCapability env
        ws = spWorld parts
        um = spUnits parts
        bm = spBuildings parts
    params ← readIORef (wsGenParamsRef ws)
    ground ← readIORef (wsGroundItemsRef ws)
    nodes ← readIORef (wsPowerNodesRef ws)
    td ← readIORef (wsTilesRef ws)
    edits ← readIORef (wsEditsRef ws)
    plots ← readIORef (wsCropPlotsRef ws)
    harvests ← readIORef (wsFloraHarvestsRef ws)
    tags ← readIORef (wsPageTagsRef ws)
    date ← readIORef (wsDateRef ws)
    WorldCamera pcx pcy ← readIORef (wsCameraRef ws)
    items ← readReadOnlyRef (crvItemManagerRef view)
    locs ← readIORef (crLocationDefsRef regs)
    reg ← readIORef (crPhysiologyRegistryRef regs)
    now ← readIORef (wsGameTimeRef wsc)
    art ← readIORef (rhStructureArtCatalogRef render)
    materials ← readIORef (wsMaterialRegistryRef wsc)
    floraCat ← readIORef (wsFloraCatalogRef wsc)
    let bounds = params ⌦ wgpArenaBounds
        calendar = maybe defaultCalendarConfig wgpCalender params
        resident = HM.elems (wtdChunks td)
        pageTags = capturePageTags tags plots
    pure CaptureInputs
        { ciPage = spPage parts, ciBounds = bounds
        , ciSeed = maybe 0 wgpSeed params
        , ciCamera = fromMaybe (pcx, pcy) (spLiveCamera parts)
        , ciUnits = um, ciBuildings = bm, ciBuildingDefs = bmDefs bm
        , ciItems = items, ciItemRefs = spItemRefs parts, ciItemTags = spItemTags parts
        , ciGround = ground, ciPowerNodes = nodes
        , ciLocations = maybe emptyLocationInstances wgpLocationInstances params
        , ciLocationDefs = locs, ciPhysiology = reg, ciNow = now
        , ciStructureArt = art, ciPalette = spPalette parts
        , ciChunks = resident
        , ciWorld = WorldSnapshot
            { snapBounds = bounds, snapSeed = maybe 0 wgpSeed params
            , snapEdits = edits, snapResident = resident
            , snapMaterials = materials, snapFlora = floraCat
            , snapAbsDay = worldAbsoluteDay calendar date
            , snapCropPlots = cpsPlots plots, snapHarvests = harvests
            , snapTileTags = ptsTiles pageTags, snapOccTags = ptsOccurrences pageTags
            , snapPlotTags = ptsPlots pageTags } }

-- | The page a capture reads: the developer arena, when the live session is
--   a testing one. 'Left' says why not.
liveTestingPage ∷ EngineEnv → WorldPageId → IO (Either Text WorldState)
liveTestingPage env pid = do
    mgr ← readIORef (wsWorldManagerRef (toWorldSimCapability env))
    verdicts ← forM (wmWorlds mgr) $ \(p, ws) → do
        params ← readIORef (wsGenParamsRef ws)
        pure (p, ws, maybe False isArenaParams params)
    pure $ case [ p | (p, _, False) ← verdicts ] of
        [] → case [ ws | (p, ws, True) ← verdicts, p ≡ pid ] of
            (ws : _) → Right ws
            [] → Left ("the testing arena page '" <> tshow pid <> "' is not live")
        bad → Left ("the live session is not a testing session: it holds the ordinary gameplay page(s) "
                    <> tshow bad <> "; only the developer arena can be captured")

-- | 'readSessionInputs' over the LIVE session: the managers and the item
--   identities as they stand now, and the live camera when the arena owns it.
readLiveInputs ∷ EngineEnv → WorldPageId → WorldState → IO CaptureInputs
readLiveInputs env pid ws = do
    let wsc = toWorldSimCapability env
    um ← readIORef (ucUnitManagerRef (toUnitCombatCapability env))
    bm ← readIORef (bcBuildingManagerRef (toBuildingCapability env))
    wm ← readIORef (wsWorldManagerRef wsc)
    palette ← readIORef (rhTexPaletteRef (toRenderHandoffCapability env))
    cam ← readIORef (rvCameraRef (toRenderViewCapability env))
    let visibleHere = case wmVisible wm of
            (v : _) → v ≡ pid
            [] → False
    readSessionInputs env SessionParts
        { spPage = pid, spWorld = ws, spUnits = um, spBuildings = bm
        , spItemRefs = wmItemScenarioRefs wm, spItemTags = wmItemTags wm
        , spPalette = palette
        , spLiveCamera = if visibleHere then Just (camPosition cam) else Nothing }
