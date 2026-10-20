{-# LANGUAGE Strict #-}
-- | Constructing, installing and capturing scenario buildings (#2712,
--   SCN-12 of epic #2698).
--
--   __Construction is pure up to its item ids__ and installs nothing. A
--   validated 'Scenario.Types.BuildingEntry' becomes a complete
--   'Building.Types.BuildingInstance' — placement, physical construction
--   state, delivered materials and storage — plus the power node it mints,
--   its tags and the identity mapping a later commit installs.
--
--   __The whole footprint is judged first.__ Placement goes through the
--   ONE placement authority ('Building.Placement.canPlaceAt': a chunk the
--   page holds, flat ground, the exact finite bounds, occupancy — here
--   including the buildings this scenario has already accepted — and, for
--   a starting building, the exclusion of a placed location's bounds)
--   BEFORE anything the building owns is minted. A footprint that cannot
--   stand rejects the building whole, with its storage and delivered
--   materials as 'CascadeRejected' (D-47): no item id is consumed, nothing
--   spills, nothing is relocated.
--
--   __Explicit values are authoritative; only omissions default.__ An
--   omitted @build_progress@ is complete, an omitted @storage@ or
--   @materials_delivered@ is nothing, an omitted @power_charge@ is the
--   placement default (empty). An explicit value, @0@ and @[]@ included, is
--   installed as stated, with no gameplay capacity check on the stored
--   tree (D-57). The building's roster countdown is NOT scenario state (the
--   owner's decision): every scenario building is stored with
--   @biSpawnRemaining = 0@, so the Lua spawn sequencer neither seeds a
--   definition roster nor adds a unit — its units are exactly the ones the
--   scenario's @units[]@ states.
--
--   __The commit is one owner transition under the page-incarnation
--   fence.__ 'installBuiltBuilding' re-checks the incarnation inside the
--   page-lifecycle lock, then reserves the footprint and commits it in the
--   SAME manager modify that inserts the instance, its tags and its
--   scenario reference ('Building.Reservation.reserveFootprint' and
--   'commitFootprint': the transition that inserts is itself the authority
--   on whether the tiles are free), registers the power node against the
--   committed id, and only then attaches the item trees' references and
--   tags all-or-nothing. A building whose items cannot take their
--   identities is retired again in one step. 'BuildingInstalled' is
--   returned only after all of that; accepting a request or reserving a
--   footprint is never reported as completion.
--
--   __Capture__ reads the live manager, the power registry and the item
--   trees as one coherent snapshot and writes every supported current
--   value explicitly (an emptied storage stays @[]@, a spent battery
--   @power_charge: 0@). A building built from reference @R@ is captured as
--   @R@; any other is named structurally from its definition and anchor
--   (an anchor is unique on a page), never from an allocation id.
module Scenario.Placement.Building
    ( BuildingBuildEnv(..)
    , BuiltBuilding(..)
    , BuildingBuild(..)
    , buildBuildings
    , buildingFootprintTiles
      -- * Install
    , BuildingInstallRefs(..)
    , BuildingInstallFailure(..)
    , BuildingCompletion(..)
    , installBuiltBuilding
    , installBuildingBuilds
      -- * Capture
    , BuildingCaptureCtx(..)
    , BuildingsCapture(..)
    , captureBuildings
    ) where

import UPrelude
import Data.IORef (IORef, readIORef, atomicModifyIORef')
import Data.List (sortOn)
import qualified Data.HashMap.Strict as HM
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Building.Placement
    ( PlacementResult(..), buildingAnchorZ, canPlaceAt, footprintOutsideArenaReason )
import Building.Reservation (commitFootprint, releaseReservation, reserveFootprint)
import Building.Types
import Gameplay.Tags.Index (setCategoryTags)
import Gameplay.Tags.ItemLifecycle (retiredBuildingItemIds, withDestroyedItems)
import Gameplay.Tags.Live (commitItemTreeAssignments)
import Gameplay.Tags.Types (GameplayTag)
import Location.Instance (LocationInstances)
import Power.Base (PowerNodeSpec(..))
import Power.Live (retirePowerNodeEverywhere)
import Power.Types (PowerNode(..), PowerRole(..), addPowerNodeCharged)
import Scenario.Decode.Monad (indexPath, keyPath)
import Scenario.Items.Build (ItemBuild(..), ItemBuildEnv, itemRefTargets, itemTagTargets)
import Scenario.Items.Capture
    ( BuildingCapture(..), CaptureCtx, CaptureState, Captured(..), captureBuildingItems
    , reserveDocumentId )
import Scenario.Items.Owners (BuildingItems(..), buildBuildingItems)
import Scenario.Types
import Unit.Types.Manager (UnitManager)
import World.Arena.Bounds (ArenaBounds)
import World.Chunk.Admit (pageIncarnation)
import World.Chunk.Residency (ChunkGeneration)
import World.Generate.Coordinates (canonicalTile)
import World.Page.Types (WorldPageId(..))
import World.State.Types (WorldManager(..), WorldState(..))
import World.Tile.Types (WorldTileData)

-- * Construction

-- | What construction reads from the engine and the page. Plain values
--   and the item allocator.
data BuildingBuildEnv = BuildingBuildEnv
    { bbeDefs      ∷ !(HM.HashMap Text BuildingDef)
    , bbeItems     ∷ !ItemBuildEnv
    , bbeBounds    ∷ !(Maybe ArenaBounds)
    , bbeWorldSize ∷ !Int
      -- ^ the page's wrap size, as placement reads it
    , bbeTiles     ∷ !WorldTileData
      -- ^ the terrain every footprint stands on: the chunks the entries'
      --   footprints touch, resident or rebuilt from the page's durable
      --   log, carrying the page's bounds
    , bbeExisting  ∷ !BuildingManager
      -- ^ the page's buildings (occupancy); other pages' are excluded by
      --   the caller
    , bbeLocations ∷ !LocationInstances
      -- ^ the page's placed locations (a starting building may not stand
      --   inside one)
    , bbeNow       ∷ !Double
      -- ^ game time at materialization
    , bbePage      ∷ !WorldPageId
    }

-- | One constructed building, not yet installed.
data BuiltBuilding = BuiltBuilding
    { bbRef      ∷ !Text
      -- ^ the scenario reference ('scenarioIdText')
    , bbPath     ∷ !Text
    , bbDef      ∷ !BuildingDef
    , bbInstance ∷ !BuildingInstance
    , bbTags     ∷ !(Set.Set GameplayTag)
    , bbItems    ∷ !ItemBuild
      -- ^ storage and delivered materials: the reference mapping and tags
    , bbPower    ∷ !(Maybe (PowerRole, Float, Float))
      -- ^ the node the building mints: role, rating, initial charge
    }

data BuildingBuild = BuildingBuild
    { bbuBuilding    ∷ !(Maybe BuiltBuilding)   -- ^ 'Nothing': the building was rejected
    , bbuDiagnostics ∷ ![ScenarioDiagnostic]
    }

-- | Every tile the entries' footprints cover, for the definitions that
--   exist: what 'bbeTiles' must hold terrain for
--   ('Scenario.World.Capture.placementTiles'). An entry of an unknown
--   definition covers nothing here; construction rejects it.
buildingFootprintTiles ∷ HM.HashMap Text BuildingDef → [BuildingEntry] → [(Int, Int)]
buildingFootprintTiles defs entries =
    [ (beX e + dx, beY e + dy)
    | e ← entries, Just d ← [HM.lookup (beDefinition e) defs]
    , dx ← [0 .. bdTileW d - 1], dy ← [0 .. bdTileH d - 1] ]

-- | Construct @entries@ in order. A rejected building leaves no trace in
--   the occupancy later entries are judged against; an accepted one holds
--   its footprint for them.
buildBuildings ∷ BuildingBuildEnv → [(Text, BuildingEntry)] → IO [BuildingBuild]
buildBuildings env entries = go (bbeExisting env) entries
  where
    go _ [] = pure []
    go bm ((path, e) : rest) = do
        (out, bm') ← buildOne env bm path e
        (out :) <$> go bm' rest

buildOne ∷ BuildingBuildEnv → BuildingManager → Text → BuildingEntry
         → IO (BuildingBuild, BuildingManager)
buildOne env bm path e = case HM.lookup (beDefinition e) (bbeDefs env) of
    Nothing → reject [ ScenarioDiagnostic (keyPath path "definition")
                         (UnknownDefinition (beDefinition e)) EntryRejected ]
    Just def → case canPlaceAt bm (bbeTiles env) (bbeLocations env) (bbeWorldSize env)
                                def (beX e) (beY e) of
        NotPlaceable why →
            reject [ ScenarioDiagnostic path (placementReason why) EntryRejected ]
        Placeable → do
            let terrainZ = buildingAnchorZ (bbeWorldSize env) (bbeTiles env) (beX e) (beY e)
            case terrainZ of
                Nothing → reject [ ScenarioDiagnostic path
                                     (InvalidValue "a chunk the page holds") EntryRejected ]
                Just z0 → accept def z0
  where
    reject ds = pure ( BuildingBuild Nothing (ds ⧺ cascade path e), bm )
    placementReason why
        | why ≡ footprintOutsideArenaReason = FootprintOutsideBounds
        | otherwise = InvalidValue ("a placeable footprint: " <> why)

    accept def terrainZ = do
        (bItems, built) ← buildBuildingItems (bbeItems env) path e
        let zDiag = case beZ e of
                Authored z | z ≢ terrainZ →
                    [ ScenarioDiagnostic (keyPath path "z")
                        (InvalidValue ("the terrain surface at the anchor (" <> tshow terrainZ
                                       <> "): a building stands on the ground"))
                        FieldRejected ]
                _ → []
            progress = authoredOr (bdBuildWork def) (beBuildProgress e)
            storage = authoredOr [] (bitStorage bItems)
            delivered = authoredOr HM.empty (bitDelivered bItems)
            power = case bdPowerNode def of
                Just (PowerNodeSource w)  → Just (PowerSource, w, 0)
                Just (PowerNodeStorage c) → Just (PowerStorage, c, authoredOr 0 (bePowerCharge e))
                Nothing → Nothing
            now = bbeNow env
            instBase = BuildingInstance
                { biDefName = beDefinition e
                , biPage = bbePage env
                , biTexture = bdSouthTexture def
                , biAnchorX = beX e, biAnchorY = beY e
                , biGridZ = terrainZ
                  -- A complete zero-work building is stamped as placed long
                  -- enough ago that its appearance has finished playing; a
                  -- worker-built one has no clock.
                , biSpawnedAt = if bdBuildWork def > 0 then now
                                else now - buildingAppearDuration def - 1
                , biTileW = bdTileW def, biTileH = bdTileH def
                  -- not scenario state: never the -1 "seed me" sentinel
                , biSpawnRemaining = 0
                , biBuildProgress = progress
                , biMaterialsDelivered = delivered
                , biStorage = storage
                }
            bb = BuiltBuilding
                { bbRef = scenarioIdText (beId e), bbPath = path, bbDef = def
                , bbInstance = instBase
                , bbTags = Set.fromList (beTags e)
                , bbItems = built, bbPower = power }
            -- hold the footprint for the entries after this one
            (lid, bm1) = nextBuildingId bm
            bm' = bm1 { bmInstances = HM.insert lid instBase (bmInstances bm1) }
        pure (BuildingBuild (Just bb) (zDiag ⧺ ibDiagnostics built), bm')

-- | Every item entry below a building, owned by the rejected building
--   (D-47: nothing spills, nothing is promoted).
cascade ∷ Text → BuildingEntry → [ScenarioDiagnostic]
cascade path e =
    concat [ owned (indexPath (keyPath path field) i) x
           | (field, xs) ← [ ("storage", authoredOr [] (beStorage e))
                           , ("materials_delivered", authoredOr [] (beMaterialsDelivered e)) ]
           , (i, x) ← zip [0 ..] xs ]
  where
    owned p x = ScenarioDiagnostic p (OwnerRejected path) CascadeRejected
        : concat [ owned (indexPath (keyPath p "contents") j) c
                 | (j, c) ← zip [0 ..] (authoredOr [] (ieContents x)) ]

-- * Install

-- | The live state a commit writes, and the lock that orders it against
--   page transitions ('Engine.Core.Capability.WorldSim.withPageLifecycle').
data BuildingInstallRefs = BuildingInstallRefs
    { birBuildings ∷ IORef BuildingManager
    , birUnits     ∷ IORef UnitManager
    , birWorld     ∷ IORef WorldManager
    , birLifecycle ∷ ∀ α. IO α → IO α
    , birWorldSize ∷ !Int
    }

data BuildingInstallFailure
    = BuildingPageNotAdmitting
      -- ^ the page is gone, or is no longer the incarnation the building
      --   was built for
    | BuildingReferenceHeld !Text
      -- ^ another live building already holds this scenario reference
    | BuildingFootprintRefused !Text
      -- ^ the footprint was no longer free at the commit, or its
      --   reservation was refused
    | BuildingItemsRefused ![Text]
      -- ^ the item trees could not take their references and tags; the
      --   building was retired again
    deriving (Show, Eq)

-- | The terminal result for one building entry.
data BuildingCompletion
    = BuildingInstalled !BuildingId !Text
      -- ^ live and complete, with its scenario reference
    | BuildingRejected ![ScenarioDiagnostic]
      -- ^ construction rejected the entry; nothing was installed
    | BuildingNotInstalled !Text !BuildingInstallFailure
      -- ^ construction succeeded but the commit did not; nothing remains
    deriving (Show)

-- | Commit one constructed building onto @page@, admitted for incarnation
--   @epoch@.
installBuiltBuilding ∷ BuildingInstallRefs → WorldPageId → ChunkGeneration → BuiltBuilding
                     → IO (Either BuildingInstallFailure BuildingId)
installBuiltBuilding refs page epoch b = do
    admitted ← birLifecycle refs $ do
        wm ← readIORef (birWorld refs)
        case lookup page (wmWorlds wm) of
            Nothing → pure (Left BuildingPageNotAdmitting)
            Just ws → do
                ours ← (≡ epoch) ⊚ pageIncarnation ws
                if not ours then pure (Left BuildingPageNotAdmitting) else do
                    r ← atomicModifyIORef' (birBuildings refs) commitOne
                    forM_ r $ \bid → forM_ (bbPower b) $ \(role, param, charge) →
                        atomicModifyIORef' (wsPowerNodesRef ws) $ \ns →
                            (fst (addPowerNodeCharged bid role param charge ns), ())
                    pure r
    case admitted of
        Left why → pure (Left why)
        Right bid → do
            attached ← commitItemTreeAssignments (birUnits refs) (birBuildings refs) (birWorld refs)
                           (itemTagTargets (bbItems b)) (itemRefTargets (bbItems b))
            case attached of
                Left targets → do
                    retire bid
                    pure (Left (BuildingItemsRefused (map tshow targets)))
                Right () → pure (Right bid)
  where
    inst = bbInstance b
    def = bbDef b
    (cx, cy) = canonicalTile (birWorldSize refs) (biAnchorX inst) (biAnchorY inst)
    commitOne bm
        | bbRef b `elem` Map.elems (bmScenarioRefs bm) = (bm, Left (BuildingReferenceHeld (bbRef b)))
        | otherwise = case reserveFootprint (birWorldSize refs) page def cx cy bm of
            (bm1, Left why) → (bm1, Left (BuildingFootprintRefused why))
            (bm1, Right bid) →
                -- the transition that inserts is itself the authority on
                -- whether the tiles are free
                let (bm2, accepted) = commitFootprint (birWorldSize refs) page bid cx cy
                                          (bdTileW def) (bdTileH def) bm1
                in if not accepted
                    then (releaseReservation bid bm2, Left (BuildingFootprintRefused "the footprint is no longer free"))
                    else ( bm2 { bmInstances = HM.insert bid inst (bmInstances bm2)
                               , bmTags = if Set.null (bbTags b) then bmTags bm2
                                          else setCategoryTags bid (bbTags b) (bmTags bm2)
                               , bmScenarioRefs = Map.insert bid (bbRef b) (bmScenarioRefs bm2) }
                         , Right bid )
    -- the building and what it owns, gone in one step: the instance, its
    -- tags and reference, its power node, and every item it held forgotten
    -- by the epoch-guarded item-destruction cleanup
    retire bid = birLifecycle refs $ do
        withDestroyedItems (birWorld refs) $
            atomicModifyIORef' (birBuildings refs) $ \bm →
                let bm' = removeBuildingQuietly bid bm
                in (bm', ((), retiredBuildingItemIds bm bm'))
        retirePowerNodeEverywhere (birWorld refs) bid

-- | Commit every build of one owner list in order, one completion per
--   entry.
installBuildingBuilds ∷ BuildingInstallRefs → WorldPageId → ChunkGeneration → [BuildingBuild]
                      → IO [BuildingCompletion]
installBuildingBuilds refs page epoch = mapM one
  where
    one bb = case bbuBuilding bb of
        Nothing → pure (BuildingRejected (bbuDiagnostics bb))
        Just b → installBuiltBuilding refs page epoch b ≫= \case
            Right bid → pure (BuildingInstalled bid (bbRef b))
            Left why → pure (BuildingNotInstalled (bbRef b) why)

-- * Capture

-- | What capture reads: one coherent snapshot of the manager's buildings
--   of the page, the power registry, the live reference and tag lookups
--   and the item capture context, all from one instant.
data BuildingCaptureCtx = BuildingCaptureCtx
    { bccDefs     ∷ !(HM.HashMap Text BuildingDef)
    , bccNodes    ∷ BuildingId → Maybe PowerNode
    , bccNameOf   ∷ BuildingId → Maybe Text
      -- ^ the FINAL explicit id of a referenced building, decided for the
      --   whole document by 'Scenario.Items.Capture.nameDocumentRefs' over
      --   'Building.Types.bmScenarioRefs' (a reference is an automatic path
      --   such as @buildings[0]@ until it is named: it is never written
      --   as an id verbatim); a building with none is named here
    , bccTags     ∷ BuildingId → Set.Set GameplayTag
      -- ^ 'Building.Types.bmTags'
    , bccItemCtx  ∷ !CaptureCtx
    }

data BuildingsCapture = BuildingsCapture
    { bcapEntries ∷ ![BuildingEntry]
      -- ^ by (anchor)
    , bcapRefs    ∷ ![(BuildingId, Text)]
      -- ^ building id → the explicit id its entry carries
    , bcapItems   ∷ ![(Word64, Text)]
      -- ^ item instance id → the explicit id its entry carries
    , bcapSkipped ∷ ![BuildingId]
      -- ^ buildings of an unknown definition, not captured
    , bcapState   ∷ !CaptureState
    }

-- | Capture the page's buildings, given the ids of every OTHER entry of
--   the document (units, locations, …: scenario ids share one namespace)
--   already reserved in the capture state.
captureBuildings ∷ BuildingCaptureCtx → CaptureState → [(BuildingId, BuildingInstance)]
                 → BuildingsCapture
captureBuildings ctx st0 buildings0 = BuildingsCapture
    { bcapEntries = map (\(_, _, e, _) → e) done
    , bcapRefs = [ (bid, scenarioIdText (beId e)) | (bid, _, e, _) ← done ]
    , bcapItems = concat [ capRefs (bcCaptured c) | (_, _, _, c) ← done ]
    , bcapSkipped = [ bid | (bid, _) ← buildings0, bid `notElem` [ b | (b, _, _) ← known ] ]
    , bcapState = stEnd }
  where
    known = [ (bid, b, d) | (bid, b) ← buildings0, Just d ← [HM.lookup (biDefName b) (bccDefs ctx)] ]
    -- the anchor of a building is unique on a page, so it is a total
    -- structural order; referenced buildings keep their reference
    ordered = sortOn (\(_, b, _) → (biAnchorX b, biAnchorY b, biDefName b)) known
    (stEnd, doneRev) = foldl' one (st0, []) ordered
    done = reverse doneRev
    one (st, acc) (bid, b, _) =
        let (name, st1) = case bccNameOf ctx bid of
                Just r → (r, st)
                Nothing → reserveDocumentId
                    ("buildings." <> biDefName b <> "." <> tshow (biAnchorX b) <> "." <> tshow (biAnchorY b)) st
            (st2, c) = captureBuildingItems (bccItemCtx ctx) st1 name b
            node = bccNodes ctx bid
            charge = case node of
                Just n | pnRole n ≡ PowerStorage → Authored (pnStoredWh n)
                _ → Omitted
            entry = BuildingEntry
                { beId = ExplicitId name
                , beTags = Set.toAscList (bccTags ctx bid)
                , beDefinition = biDefName b
                , beX = biAnchorX b, beY = biAnchorY b
                , beZ = Authored (biGridZ b)
                , beStorage = Authored (bcStorage c)
                , beBuildProgress = Authored (biBuildProgress b)
                , beMaterialsDelivered = Authored (bcDelivered c)
                , bePowerCharge = charge }
        in (st2, (bid, b, entry, c) : acc)
