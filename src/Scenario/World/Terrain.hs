{-# LANGUAGE Strict #-}
-- | Turning validated scenario terrain and fluid patches into durable
--   world edits (#2711, SCN-11 of epic #2698): the pure construction
--   half of the terrain and fluid adapters. It installs nothing — see
--   "Scenario.World.Install" — and reads nothing but its arguments.
--
--   __What a terrain patch means.__ Every admitted tile of the patch gets
--   a column whose top solid cell is @surface_z@, whose top cell is the
--   named material, and whose top slope is the authored ramp bitmask.
--   Relative to the arena's unedited column ("World.Generate.Arena": sixteen
--   cells, the top at 'seaLevel', loam over granite) that is: a raised
--   pad of the named material above it, a column dug down to it, or the
--   top cell replaced. Nothing below the top is invented: the cells the
--   base column already has stay as they are. A column that already
--   equals the stated spec produces NO edit, so it keeps the base's
--   surface vegetation; any column an edit changes carries none (surface
--   vegetation is derived, not authorable — docs\/scenario_format.md).
--
--   __Overlap.__ Patches apply in authored order, and a later patch
--   REPLACES the column state of every tile it covers. The edits are built
--   from the FINAL state of each tile, once, so the log never replays an
--   earlier patch's relative raise or dig underneath a later one.
--
--   __Fluid.__ A fluid patch states an absolute surface on the exact
--   eighth-z plane ("World.Fluid.Exact"). A tile is wet when that plane is
--   above the tile's FINAL terrain top, and then records an exact fluid
--   snapshot ('WeSetFluidSnapshot'); a tile whose terrain reaches the
--   plane is dry and records nothing. Fluid is not rounded to a z level
--   or a full cell, and a fluid patch carries no tags (fluid is not a
--   tag target). A tile the terrain changed that the fluid leaves dry is
--   cleared EXPLICITLY ('WeClearFluidSnapshot'): the replay of a dug
--   column at or below the water table would otherwise reveal
--   groundwater there, and an authored absence is authoritative.
--
--   __Bounds and budgets.__ A region is clipped tile by tile against the
--   page's own bounds ('World.Arena.Bounds.clipTileRegion'): the
--   admitted tiles are kept with one 'TilesClipped' diagnostic, and a
--   patch with none is rejected. Safety limits bound the work an
--   authored file can ask for — 'maxMaterializedTiles' per family and a
--   'surface_z' window, plus a cap on the cells (tiles × the height changed)
--   the terrain may change — and a patch beyond any is rejected whole with
--   an actionable diagnostic rather than half-applied.
--
--   Recoverable problems are data ('ScenarioDiagnostic'), reported at
--   the document path the caller supplies for each entry.
module Scenario.World.Terrain
    ( -- * Environment
      TerrainEnv(..)
      -- * The unedited column
    , baseTopZ
    , baseFloorZ
    , baseTopMaterial
    , maxSurfaceZ
    , maxMaterializedTiles
    , maxMaterializedCells
      -- * Terrain
    , TileSpec(..)
    , BuiltTerrainPatch(..)
    , TerrainBuild(..)
    , buildTerrain
    , tileSpecEdits
    , baseTileSpec
      -- * Fluid
    , BuiltFluidPatch(..)
    , FluidBuild(..)
    , buildFluids
    , maxFluidDepthUnits
    , fluidTypeOfKind
    , fluidKindOfType
    ) where

import UPrelude
import Data.List (sortOn)
import qualified Data.Map.Strict as M
import qualified Data.Set as Set
import Gameplay.Tags.Types (GameplayTag)
import Scenario.Bounds (regionTileCount)
import Scenario.Types
import World.Arena.Bounds (ArenaBounds, clipTileRegion)
import World.Constants (seaLevel)
import World.Edit.Types (WorldEdit(..))
import World.Fluid.Exact (exactSurfaceOfZ)
import Sim.Fluid.Types (ActiveFluidCell(..), fluidCellToActive)
import World.Fluid.Types (FluidCell(..), FluidType(..))
import World.Generate.Arena (arenaGraniteLayers, arenaLoamLayers)
import World.Material (matAir, matLoam)
import World.Material.Id (MaterialId(..))

-- * Environment

-- | What construction needs from the engine: the page's own bounds and
--   the material registry, as a plain name lookup.
data TerrainEnv = TerrainEnv
    { teBounds   ∷ !(Maybe ArenaBounds)
    , teMaterial ∷ !(Text → Maybe MaterialId)
    }

-- * The unedited column

-- | The terrain top of an unedited arena column.
baseTopZ ∷ Int
baseTopZ = seaLevel

-- | The lowest cell an unedited arena column has: no edit can grow a
--   column downward, so a surface below it cannot be represented.
baseFloorZ ∷ Int
baseFloorZ = seaLevel - (arenaLoamLayers + arenaGraniteLayers) + 1

-- | The material of the unedited column's top cell.
baseTopMaterial ∷ MaterialId
baseTopMaterial = matLoam

-- | How far above the base a column may be raised: a safety limit, not a
--   physical one (the column grows by one cell per z).
maxSurfaceZ ∷ Int
maxSurfaceZ = baseTopZ + 256

-- | The most tiles one family (terrain, fluid) may ask to materialize.
--   A rectangle over an expandable arena has no map to clip it, and a
--   file naming a billion tiles must be refused, not attempted.
maxMaterializedTiles ∷ Integer
maxMaterializedTiles = 1048576

-- | The most cells (one edit each) the terrain of one scenario may ask to
--   change: a tile costs one plus its distance from the base top, so a tall
--   raise over a wide region is bounded too, not only the tile count.
maxMaterializedCells ∷ Integer
maxMaterializedCells = 4194304

-- * Terrain

-- | The stated shape of one column.
data TileSpec = TileSpec
    { tsSurfaceZ ∷ !Int
    , tsMaterial ∷ !MaterialId
    , tsSlope    ∷ !Word8
    } deriving (Show, Eq)

-- | The unedited column's own spec.
baseTileSpec ∷ TileSpec
baseTileSpec = TileSpec baseTopZ baseTopMaterial 0

-- | One accepted terrain patch: its admitted tiles and the cell its tags
--   label on each of them.
data BuiltTerrainPatch = BuiltTerrainPatch
    { btpPath      ∷ !Text
    , btpId        ∷ !ScenarioId
    , btpTags      ∷ ![GameplayTag]
    , btpSurfaceZ  ∷ !Int
    , btpTiles     ∷ ![(Int, Int)]
      -- ^ admitted tiles, authored order, duplicates removed
    , btpLiveCells ∷ ![(Int, Int, Int)]
      -- ^ the cells @(x, y, surface_z)@ that are still solid after every
      --   later patch: where this patch's tags go
    } deriving (Show, Eq)

data TerrainBuild = TerrainBuild
    { tbSpecs       ∷ !(M.Map (Int, Int) TileSpec)
      -- ^ the FINAL spec of every touched tile
    , tbPatches     ∷ ![BuiltTerrainPatch]
    , tbEdits       ∷ ![WorldEdit]
      -- ^ in apply order: by tile, each tile's edits together
    , tbDiagnostics ∷ ![ScenarioDiagnostic]
    } deriving (Show, Eq)

diagAt ∷ Text → DiagnosticReason → DiagnosticEffect → ScenarioDiagnostic
diagAt = ScenarioDiagnostic

-- | Build the terrain of @patches@, each with its document path.
buildTerrain ∷ TerrainEnv → [(Text, TerrainPatch)] → TerrainBuild
buildTerrain env patches =
    let (final, accepted, diags, _, _) = foldl' step (M.empty, [], [], 0, 0) patches
        built = reverse accepted
        edits = concat [ tileSpecEdits x y spec | ((x, y), spec) ← M.toAscList final ]
        patchesLive = map (live final) built
        liveDiags = [ diagAt (btpPath p) (InvalidValue
                        (tshow (length (btpTiles p) - length (btpLiveCells p))
                         <> " tagged cell(s) no longer exist: a later patch lowered the column"))
                        FieldRejected
                    | p ← patchesLive, not (null (btpTags p))
                    , length (btpLiveCells p) < length (btpTiles p) ]
    in TerrainBuild
        { tbSpecs = final
        , tbPatches = patchesLive
        , tbEdits = edits
        , tbDiagnostics = sortOn sdPath (reverse diags ⧺ liveDiags)
        }
  where
    live final p = p { btpLiveCells =
        [ (x, y, btpSurfaceZ p)
        | (x, y) ← btpTiles p
        , Just spec ← [M.lookup (x, y) final]
        , tsSurfaceZ spec ≥ btpSurfaceZ p ] }
    step (final, acc, diags, used, cells) (path, p) =
        case judge path p used cells of
            Left ds → (final, acc, reverse ds ⧺ diags, used, cells)
            Right (mat, tiles, ds, n) →
                let spec = TileSpec (tpSurfaceZ p) mat (authoredOr 0 (tpSlope p))
                    final' = foldl' (\m t → M.insert t spec m) final tiles
                    bp = BuiltTerrainPatch path (tpId p) (tpTags p) (tpSurfaceZ p) tiles []
                in ( final', bp : acc, reverse ds ⧺ diags, used + n
                   , cells + n * cellCost (tpSurfaceZ p) )
    -- the cells one tile of a patch changes: its own plus every z it raises
    -- or digs between the base top and the stated surface
    cellCost z = 1 + toInteger (abs (z - baseTopZ))
    judge path p used cells = do
        mat ← case teMaterial env (tpMaterial p) of
            Nothing → Left [diagAt path (UnknownDefinition (tpMaterial p)) EntryRejected]
            Just m | m ≡ matAir →
                Left [diagAt path (InvalidValue "a solid terrain material, not air") EntryRejected]
            Just m → Right m
        when (tpSurfaceZ p < baseFloorZ ∨ tpSurfaceZ p > maxSurfaceZ) $
            Left [diagAt path (InvalidValue
                    ("surface_z in [" <> tshow baseFloorZ <> ", " <> tshow maxSurfaceZ
                     <> "]: a column has " <> tshow (negate baseFloorZ + 1)
                     <> " cells below sea level and cannot grow downward"))
                    EntryRejected]
        (tiles, ds, n) ← admit path (teBounds env) (tpRegion p) used
        when (cells + n * cellCost (tpSurfaceZ p) > maxMaterializedCells) $
            Left [diagAt path (InvalidValue
                    ("a patch within the " <> tshow maxMaterializedCells
                     <> "-cell limit (tiles × height changed): this one would reach "
                     <> tshow (cells + n * cellCost (tpSurfaceZ p))))
                    EntryRejected]
        pure (mat, tiles, ds, n)

-- | Clip a region to the page and to the family's tile budget.
admit ∷ Text → Maybe ArenaBounds → TileRegion → Integer
      → Either [ScenarioDiagnostic] ([(Int, Int)], [ScenarioDiagnostic], Integer)
admit path bounds region used =
    case clipTileRegion bounds region of
        (Nothing, _) → Left [diagAt path OutsideBounds EntryRejected]
        (Just kept, dropped) →
            let n = regionTileCount kept
                note = [ diagAt path (ClippedTiles dropped) TilesClipped | dropped > 0 ]
            in if used + n > maxMaterializedTiles
                then Left [diagAt path (InvalidValue
                        ("a region within the " <> tshow maxMaterializedTiles
                         <> "-tile limit per family: this one would reach "
                         <> tshow (used + n)))
                        EntryRejected]
                else Right (regionTiles kept, note, n)

-- | The tiles of a clipped region, authored order for a list, row-major
--   for a rectangle; duplicates removed.
regionTiles ∷ TileRegion → [(Int, Int)]
regionTiles (RegionRect x0 y0 x1 y1) = [ (x, y) | x ← [x0 .. x1], y ← [y0 .. y1] ]
regionTiles (RegionTiles ts) = dedupe ts
  where dedupe = go Set.empty
        go _ [] = []
        go seen (t : rest)
            | Set.member t seen = go seen rest
            | otherwise         = t : go (Set.insert t seen) rest

-- | The edits that take an unedited column to @spec@ (none when it
--   already is that column).
tileSpecEdits ∷ Int → Int → TileSpec → [WorldEdit]
tileSpecEdits gx gy spec@(TileSpec s m slope)
    | spec ≡ baseTileSpec = []
    | otherwise =
        shape ⧺ [ WeSetSlope gx gy s slope | slope ≢ 0 ]
  where
    shape
        | s > baseTopZ = replicate (s - baseTopZ) (WeAddTile gx gy m)
        | s ≡ baseTopZ = [WeSetCell gx gy s m]
        | otherwise    = replicate (baseTopZ - s) (WeDeleteTile gx gy)
                         ⧺ [WeSetCell gx gy s m]

-- * Fluid

data BuiltFluidPatch = BuiltFluidPatch
    { bfpPath    ∷ !Text
    , bfpId      ∷ !ScenarioId
    , bfpKind    ∷ !FluidKind
    , bfpSurface ∷ !Int
    , bfpTiles   ∷ ![(Int, Int)]
    } deriving (Show, Eq)

data FluidBuild = FluidBuild
    { fbFinal       ∷ !(M.Map (Int, Int) (FluidType, Int))
      -- ^ the final fluid state of every WET tile
    , fbPatches     ∷ ![BuiltFluidPatch]
    , fbEdits       ∷ ![WorldEdit]
    , fbDiagnostics ∷ ![ScenarioDiagnostic]
    } deriving (Show, Eq)

fluidTypeOfKind ∷ FluidKind → FluidType
fluidTypeOfKind k = case k of
    FluidOcean → Ocean
    FluidLake  → Lake
    FluidRiver → River
    FluidLava  → Lava

fluidKindOfType ∷ FluidType → FluidKind
fluidKindOfType t = case t of
    Ocean → FluidOcean
    Lake  → FluidLake
    River → FluidRiver
    Lava  → FluidLava

-- | The deepest fluid column the active simulation can hold, in exact
--   units over the terrain top — READ OFF the owner, not restated: the
--   volume 'Sim.Fluid.Types.fluidCellToActive' keeps for a cell as deep as
--   an Int can say, which is where its 'Word16' clamp bites, so a deeper
--   authored plane would be accepted and then silently lose fluid.
maxFluidDepthUnits ∷ Integer
maxFluidDepthUnits =
    maybe 0 (toInteger ∘ afcVolume) (fluidCellToActive 0 (FluidCell Lake maxBound))

-- | Build the fluid of @patches@ over the terrain whose tile specs are
--   @terrain@ (an untouched tile is the base column).
buildFluids ∷ Maybe ArenaBounds → M.Map (Int, Int) TileSpec
            → [(Text, FluidPatch)] → FluidBuild
buildFluids bounds terrain patches =
    let (final, accepted, diags, _) = foldl' step (M.empty, [], [], 0) patches
        topOf t = maybe baseTopZ tsSurfaceZ (M.lookup t terrain)
        wet = M.mapMaybeWithKey
                (\t (ft, e) → if e > exactSurfaceOfZ (topOf t) then Just (ft, e) else Nothing)
                final
        -- An authored absence is authoritative: digging at or below the
        -- water table makes the REPLAY reveal groundwater
        -- ('World.Edit.Apply.applyEdit' of 'WeDeleteTile'), so every tile
        -- the terrain changed and the fluid does not wet is cleared
        -- explicitly rather than left to that reveal.
        clears = [ WeClearFluidSnapshot x y
                 | (t@(x, y), spec) ← M.toAscList terrain
                 , spec ≢ baseTileSpec, not (M.member t wet) ]
        edits = clears
            ⧺ [ WeSetFluidSnapshot x y ft e | ((x, y), (ft, e)) ← M.toAscList wet ]
        tagDiags = [ diagAt (keyedPath path "tags")
                        (InvalidValue "a fluid patch has no tag owner: tag the terrain cell")
                        FieldRejected
                   | (path, p) ← patches, not (null (fpTags p)) ]
    in FluidBuild
        { fbFinal = wet
        , fbPatches = reverse accepted
        , fbEdits = edits
        , fbDiagnostics = sortOn sdPath (reverse diags ⧺ tagDiags)
        }
  where
    keyedPath p k = p <> "." <> k
    -- the deepest admitted tile decides: Integer arithmetic, so a plane near
    -- the top of the Int range cannot overflow the comparison
    depthOk path p r@(tiles, _, _) =
        let depthAt t = toInteger (fpSurfaceEighths p)
                          - toInteger (exactSurfaceOfZ (maybe baseTopZ tsSurfaceZ (M.lookup t terrain)))
            deepest = maximum (0 : map depthAt tiles)
        in if deepest > maxFluidDepthUnits
            then Left [diagAt path (InvalidValue
                    ("a fluid at most " <> tshow maxFluidDepthUnits <> " units deep (the "
                     <> "simulation's volume range): this one reaches " <> tshow deepest))
                    EntryRejected]
            else Right r
    step (final, acc, diags, used) (path, p) =
        case admit path bounds (fpRegion p) used ⌦ depthOk path p of
            Left ds → (final, acc, reverse ds ⧺ diags, used)
            Right (tiles, ds, n) →
                let ft = fluidTypeOfKind (fpKind p)
                    final' = foldl' (\m t → M.insert t (ft, fpSurfaceEighths p) m) final tiles
                    bp = BuiltFluidPatch path (fpId p) (fpKind p) (fpSurfaceEighths p) tiles
                in (final', bp : acc, reverse ds ⧺ diags, used + n)
