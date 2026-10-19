{-# LANGUAGE Strict #-}
-- | Reading the page's physical baseline back into scenario entries
--   (#2711, SCN-11 of epic #2698): the capture half of the terrain, fluid
--   and flora adapters.
--
--   Capture is a pure read of a snapshot the caller already holds — the
--   page's live edit log, its resident chunks, its crop plots, regrowth
--   timers and tag assignments, taken at one instant after the save
--   barrier reconciled the simulation with the world. It has no engine
--   handle, consumes no gameplay RNG and mints no runtime identity.
--
--   __What is exported.__ Every supported CURRENT value is written
--   explicitly, so a recapture REPLACES the old overrides: a column's top
--   (surface z, top material, slope), each wet tile's fluid kind and exact
--   eighth-z surface, and each plant's species, tile, z, derived current
--   age (never the stored baseline), health, offsets, variant and
--   regrowth (an explicit @0@ for a harvestable plant whose timer ran
--   out). Nothing reconstructible is duplicated: a column equal to the
--   arena's unedited column is not a patch (unless it carries tags), and
--   an occurrence outside the planted id namespace is generated flora
--   the world regenerates, not an authored one.
--
--   __No residency assumption.__ A chunk that is not resident is read
--   from the durable log: the arena base is regenerated and the chunk's
--   edits replayed ('World.Edit.Apply.replayEdits'), the same
--   reconstruction an admission performs. Non-residency is never
--   deletion, and no unbounded world is enumerated: only chunks the log
--   or the resident set names are visited, and a tagged cell outside both
--   is read as the unedited column.
--
--   __What the v2 schema cannot say__ is reported, not dropped silently:
--   a tag on a cell below the surface, and a column with no solid cell,
--   come back as diagnostics. Interior cells below the top are not part
--   of the terrain vocabulary.
--
--   __Names.__ Entries are written under generated explicit ids that
--   depend only on the content (the family, the values, the tile and an
--   ordinal among equal siblings, ranked by the raw values with the
--   derived 'Ord' — never a rendered string, an allocation number or the
--   order entries were visited in) and are made unique against
--   @reserved@: the ids of every OTHER entry of the document being
--   written, which are never taken.
module Scenario.World.Capture
    ( WorldSnapshot(..)
    , CapturedWorld(..)
    , captureWorldContent
    ) where

import UPrelude
import Data.List (find, sortOn)
import qualified Data.HashMap.Strict as HM
import qualified Data.Map.Strict as M
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU
import Gameplay.Tags.Types (GameplayTag)
import Scenario.Items.Refs (maxItemRefLength, stableHash64)
import Scenario.Types
import Scenario.World.Terrain
import World.Arena.Bounds (ArenaBounds, arenaAdmitsTile)
import World.Chunk.Types
    ( ChunkCoord(..), ColumnTiles(..), LoadedChunk(..), chunkSize, columnIndex )
import World.Edit.Apply (replayEdits)
import World.Edit.Types (WorldEdits)
import World.Flora.CropPlot (CropPlots, CropPlotOf(..))
import World.Flora.Harvest (FloraHarvests)
import World.Flora.Identity (FloraInstanceId, isPlantedFloraInstanceId)
import World.Flora.Types
import World.Generate.Arena (clipArenaChunk, generateArenaChunkIn)
import World.Generate.Coordinates (chunkToGlobal, globalToChunk)
import World.Material (MaterialProps(..), MaterialRegistry, getMaterialProps, isKnownMaterial)
import World.Material.Id (MaterialId(..))
import World.Fluid.Exact (exactSurfaceOfZ)
import World.Fluid.Types (FluidCell(..))
import Scenario.World.Flora (derivedOccurrenceAge, derivedPlotAge)

-- | One coherent view of a page, as the caller read it.
data WorldSnapshot = WorldSnapshot
    { snapBounds     ∷ !(Maybe ArenaBounds)
    , snapSeed       ∷ !Word64
    , snapEdits      ∷ !WorldEdits
      -- ^ the page's edit log in its LIVE form (planting edits carry
      --   species handles), fluid snapshots included for chunks that are
      --   not resident
    , snapResident   ∷ ![LoadedChunk]
      -- ^ the resident chunks, authoritative for their own columns
    , snapMaterials  ∷ !MaterialRegistry
    , snapFlora      ∷ !FloraCatalog
    , snapAbsDay     ∷ !Int
    , snapCropPlots  ∷ !CropPlots
    , snapHarvests   ∷ !FloraHarvests
    , snapTileTags   ∷ !(M.Map (Int, Int, Int) (Set.Set GameplayTag))
    , snapOccTags    ∷ !(M.Map FloraInstanceId (Set.Set GameplayTag))
    , snapPlotTags   ∷ !(M.Map (Int, Int) (Set.Set GameplayTag))
    }

data CapturedWorld = CapturedWorld
    { cwTerrain     ∷ ![TerrainPatch]
    , cwFluids      ∷ ![FluidPatch]
    , cwFlora       ∷ ![FloraEntry]
    , cwDiagnostics ∷ ![ScenarioDiagnostic]
    } deriving (Show, Eq)

-- | Capture the snapshot. @reserved@ are the ids of the document's other
--   entries.
captureWorldContent ∷ Set.Set Text → WorldSnapshot → CapturedWorld
captureWorldContent reserved snap =
    let chunks = chunkStates snap
        tiles = columnsOf snap chunks
        -- terrain ------------------------------------------------------
        tagsOf t = M.findWithDefault Set.empty t (snapTileTags snap)
        namedTiles = M.filter (isKnownMaterial (snapMaterials snap) ∘ tsMaterial ∘ tcSpec) tiles
        terrainGroups = M.fromListWith (⧺)
            [ ((tsSurfaceZ spec, unMaterialId (tsMaterial spec), tsSlope spec
               , Set.toAscList tags), [(x, y)])
            | ((x, y), c) ← M.toAscList namedTiles
            , let spec = tcSpec c
            , let tags = tagsOf (x, y, tsSurfaceZ spec)
            , spec ≢ baseTileSpec ∨ not (Set.null tags) ]
        unnamed = M.size tiles - M.size namedTiles
        hollow = length (hollowTiles snap chunks)
        (terrainEntries, names1) = foldl' nameTerrain ([], reserved)
            (M.toAscList terrainGroups)
        nameTerrain (acc, used) ((z, mid, slope, tags), ts) =
            let sorted = sortOn id ts
                matName = materialName (snapMaterials snap) (MaterialId mid)
                base = "terrain." <> tshow z <> "." <> matName <> ".s" <> tshow slope
                       <> "." <> firstTile sorted
                (name, used') = uniqueName used base
                p = TerrainPatch
                    { tpId = ExplicitId name
                    , tpTags = tags
                    , tpRegion = regionOf sorted
                    , tpMaterial = matName
                    , tpSurfaceZ = z
                    , tpSlope = Authored slope }
            in (p : acc, used')
        -- tags below the surface cannot be said ------------------------
        belowSurface =
            [ k | k@(x, y, z) ← M.keys (snapTileTags snap)
                , maybe True (\c → tsSurfaceZ (tcSpec c) ≢ z) (M.lookup (x, y) tiles)
                , arenaAdmitsTile (snapBounds snap) x y ]
        columnDiags =
            [ ScenarioDiagnostic "terrain.material"
                (InvalidValue (tshow unnamed <> " column(s) have a top material with no "
                    <> "registered name and cannot be written"))
                FieldRejected
            | unnamed > 0 ]
            ⧺ [ ScenarioDiagnostic "terrain.columns"
                (InvalidValue (tshow hollow <> " column(s) have no solid cell and cannot "
                    <> "be written: a patch states a surface"))
                FieldRejected
              | hollow > 0 ]
        tagDiags = [ ScenarioDiagnostic "terrain.tags"
                        (InvalidValue (tshow (length belowSurface)
                            <> " tagged cell(s) are not a column's top cell and cannot be "
                            <> "written: the terrain vocabulary names only the surface"))
                        FieldRejected
                   | not (null belowSurface) ]
        -- fluids -------------------------------------------------------
        fluidGroups = M.fromListWith (⧺)
            [ ((fromEnum (fluidKindOfType (fcType f)), fcExactSurface f), [(x, y)])
            | ((x, y), c) ← M.toAscList tiles, Just f ← [tcFluid c]
            -- a cell whose plane does not rise above its terrain top holds
            -- no volume: it is dry (a dug column's groundwater film)
            , fcExactSurface f > exactSurfaceOfZ (tsSurfaceZ (tcSpec c)) ]
        (fluidEntries, names2) = foldl' nameFluid ([], names1) (M.toAscList fluidGroups)
        nameFluid (acc, used) ((kindIx, e), ts) =
            let sorted = sortOn id ts
                kind = toEnum kindIx
                base = "fluid." <> fluidKindName kind <> "." <> tshow e
                       <> "." <> firstTile sorted
                (name, used') = uniqueName used base
            in (FluidPatch (ExplicitId name) [] (regionOf sorted) kind e : acc, used')
        -- flora --------------------------------------------------------
        floraEntries = captureFlora snap chunks names2
    in CapturedWorld
        { cwTerrain = reverse terrainEntries
        , cwFluids = reverse fluidEntries
        , cwFlora = floraEntries
        , cwDiagnostics = sortOn sdPath (tagDiags ⧺ columnDiags)
        }

-- | The first tile of a sorted, non-empty group, as @x.y@.
firstTile ∷ [(Int, Int)] → Text
firstTile ts = case ts of
    (x, y) : _ → tshow x <> "." <> tshow y
    []         → "0.0"

-- * Chunk states

-- | What capture reads of a column.
data TileCol = TileCol
    { tcSpec  ∷ !TileSpec
    , tcFluid ∷ !(Maybe FluidCell)
    }

-- | Every chunk the snapshot names, as it IS: resident chunks as held,
--   the rest rebuilt from the base and the durable log.
chunkStates ∷ WorldSnapshot → M.Map ChunkCoord LoadedChunk
chunkStates snap =
    let resident = M.fromList [ (lcCoord lc, lc) | lc ← snapResident snap ]
        logged = [ c | c ← HM.keys (snapEdits snap), not (M.member c resident) ]
        rebuilt = M.fromList
            [ (c, clipArenaChunk (snapBounds snap)
                      (replayEdits (snapEdits snap)
                          (generateArenaChunkIn (snapBounds snap) (snapSeed snap) c)))
            | c ← logged ]
    in M.union resident rebuilt

-- | The shape of every PRESENT column of every visited chunk, plus the
--   unedited column for a tagged tile no visited chunk holds.
columnsOf ∷ WorldSnapshot → M.Map ChunkCoord LoadedChunk → M.Map (Int, Int) TileCol
columnsOf snap chunks =
    let visited = M.fromList
            [ ((gx, gy), c)
            | (coord, lc) ← M.toAscList chunks
            , let authored = authoredChunk snap coord
            , ly ← [0 .. chunkSize - 1], lx ← [0 .. chunkSize - 1]
            , let (gx, gy) = chunkToGlobal coord lx ly
            , arenaAdmitsTile (snapBounds snap) gx gy
            , Just c ← [columnOf lc authored (columnIndex lx ly)] ]
        extra = M.fromList
            [ ((x, y), TileCol baseTileSpec Nothing)
            | (x, y, _) ← M.keys (snapTileTags snap)
            , arenaAdmitsTile (snapBounds snap) x y
            , not (M.member (fst (globalToChunk x y)) chunks) ]
    in M.union visited extra

-- | The tiles inside the map whose column has no solid cell at all (fully
--   dug through), which a patch — a stated surface — cannot say.
hollowTiles ∷ WorldSnapshot → M.Map ChunkCoord LoadedChunk → [(Int, Int)]
hollowTiles snap chunks =
    [ (gx, gy)
    | (coord, lc) ← M.toAscList chunks
    , ly ← [0 .. chunkSize - 1], lx ← [0 .. chunkSize - 1]
    , let (gx, gy) = chunkToGlobal coord lx ly
    , arenaAdmitsTile (snapBounds snap) gx gy
    , isNothing (columnOf lc Nothing (columnIndex lx ly)) ]

-- | The chunk as the DURABLE LOG alone builds it — the arena base with the
--   edits replayed, no derived pass — or 'Nothing' for a chunk the log never
--   names (the unedited base). It is where a column's AUTHORED slope is read.
authoredChunk ∷ WorldSnapshot → ChunkCoord → Maybe LoadedChunk
authoredChunk snap c
    | HM.member c (snapEdits snap) =
        Just (clipArenaChunk (snapBounds snap)
                (replayEdits (snapEdits snap) (generateArenaChunkIn (snapBounds snap) (snapSeed snap) c)))
    | otherwise = Nothing

-- | One column. Its slope is the AUTHORED one (the recorded @WeSetSlope@ of
--   its top cell, else flat): the slope a live chunk holds also carries
--   DERIVED bits — the border-strip recompute after an admission, mid-dig
--   masks — which are not scenario state, and which depend on which
--   neighbours happen to be resident.
columnOf ∷ LoadedChunk → Maybe LoadedChunk → Int → Maybe TileCol
columnOf lc authored idx =
    let col = lcTiles lc V.! idx
        top = lcTerrainSurfaceMap lc VU.! idx
        i = top - ctStartZ col
        authoredSlope = case authored of
            Nothing → 0
            Just a →
                let acol = lcTiles a V.! idx
                    ai = top - ctStartZ acol
                in if ai ≥ 0 ∧ ai < VU.length (ctSlopes acol) then ctSlopes acol VU.! ai else 0
    in if i < 0 ∨ i ≥ VU.length (ctMats col) ∨ ctMats col VU.! i ≡ 0
        then Nothing
        else Just TileCol
            { tcSpec = TileSpec top (MaterialId (ctMats col VU.! i)) authoredSlope
            , tcFluid = lcFluidMap lc V.! idx }

-- * Flora

captureFlora ∷ WorldSnapshot → M.Map ChunkCoord LoadedChunk → Set.Set Text
             → [FloraEntry]
captureFlora snap chunks used0 =
    let cat = snapFlora snap
        day = snapAbsDay snap
        occurrences =
            [ ((x, y), fi, sp)
            | (coord, lc) ← M.toAscList chunks
            , fi ← fcdInstances (lcFlora lc)
            , isPlantedFloraInstanceId (fiInstanceId fi)
            , let (x, y) = chunkToGlobal coord (fromIntegral (fiTileX fi)) (fromIntegral (fiTileY fi))
            , arenaAdmitsTile (snapBounds snap) x y
            , Just sp ← [lookupSpecies (fiSpecies fi) cat] ]
        occEntry ((x, y), fi, sp) =
            ( (fsName sp, (x, y), rankKey fi)
            , \name → FloraEntry
                { feId = ExplicitId name
                , feTags = Set.toAscList (M.findWithDefault Set.empty (fiInstanceId fi)
                                                                       (snapOccTags snap))
                , feSpecies = fsName sp, feX = x, feY = y
                , feZ = Authored (fiZ fi)
                , feAge = Authored (derivedOccurrenceAge day fi)
                , feHealth = Authored (fiHealth fi)
                , feOffsetU = Authored (fiOffU fi), feOffsetV = Authored (fiOffV fi)
                , feVariant = Authored (fiVariant fi)
                , feRegrowth = if isJust (fsHarvest sp)
                    then Authored (HM.lookupDefault 0 (fiInstanceId fi) (snapHarvests snap))
                    else Omitted } )
        plots =
            [ ((x, y), cp, sp)
            | ((x, y), cp) ← M.toAscList (M.fromList (HM.toList (snapCropPlots snap)))
            , arenaAdmitsTile (snapBounds snap) x y
            , Just sp ← [lookupSpecies (cpSpecies cp) cat] ]
        plotEntry ((x, y), cp, sp) =
            ( (fsName sp, (x, y), ((0, 0, 0, 0, 0, 0) ∷ RankKey))
            , \name → FloraEntry
                { feId = ExplicitId name
                , feTags = Set.toAscList (M.findWithDefault Set.empty (x, y) (snapPlotTags snap))
                , feSpecies = fsName sp, feX = x, feY = y
                , feZ = Omitted
                , feAge = Authored (derivedPlotAge day cp)
                , feHealth = Authored (cpHealth cp)
                , feOffsetU = Omitted, feOffsetV = Omitted, feVariant = Omitted
                , feRegrowth = Omitted } )
        all' = sortOn fst (map occEntry occurrences ⧺ map plotEntry plots)
        (entries, _) = foldl' step ([], used0) (zip [0 ∷ Int ..] all')
        -- an ordinal among entries equal in (species, tile): their rank
        -- in the sorted order of the group
        ordinals = M.fromListWith (flip (⧺)) [ (key3 k, [i]) | (i, (k, _)) ← zip [0 ∷ Int ..] all' ]
        key3 (a, b, _) = (a, b)
        ordinalOf i k = case M.lookup (key3 k) ordinals of
            Just is | length is > 1 → Just (length (takeWhile (< i) is) + 1)
            _ → Nothing
        step (acc, usedNames) (i, (k@(spName, (x, y), _), mk)) =
            let base = "flora." <> spName <> "." <> tshow x <> "." <> tshow y
                       <> maybe "" (\n → "." <> tshow n) (ordinalOf i k)
                (name, used') = uniqueName usedNames base
            in (mk name : acc, used')
    in reverse entries

-- | The ranking of occurrences that share a species and a tile: raw
--   values under the derived 'Ord', never a rendered form.
type RankKey = (Int, Float, Float, Word8, Float, Float)

rankKey ∷ FloraInstance → RankKey
rankKey fi = (fiZ fi, fiOffU fi, fiOffV fi, fiVariant fi, fiHealth fi, fiAge fi)

-- * Names and regions

-- | The scenario id vocabulary: letters, digits and @_ - : .@.
sanitize ∷ Text → Text
sanitize = T.map (\c → if ok c then c else '_')
  where ok c = (c ≥ 'a' ∧ c ≤ 'z') ∨ (c ≥ 'A' ∧ c ≤ 'Z') ∨ (c ≥ '0' ∧ c ≤ '9')
               ∨ c ≡ '_' ∨ c ≡ '-' ∨ c ≡ ':' ∨ c ≡ '.'

-- | The first spelling of @base@ nothing has taken: @base@, @base-2@,
--   @base-3@ …, shortened with a stable hash when it would pass the
--   save's length limit.
uniqueName ∷ Set.Set Text → Text → (Text, Set.Set Text)
uniqueName used base0 =
    let base = sanitize base0
        bounded t | T.length t ≤ maxItemRefLength = t
                  | otherwise = T.take (maxItemRefLength - 17) t <> "-" <> hex64 (stableHash64 t)
        candidates = bounded base : [ bounded (base <> "-" <> tshow n) | n ← [2 ∷ Int ..] ]
        name = fromMaybe base (find (not ∘ (`Set.member` used)) candidates)
    in (name, Set.insert name used)
  where hex64 w = T.pack (showHexWord w)

showHexWord ∷ Word64 → String
showHexWord w = go (16 ∷ Int) w ""
  where go 0 _ acc = acc
        go n x acc = go (n - 1) (x `shiftR` 4) ("0123456789abcdef" !! fromIntegral (x ⌃ 15) : acc)

materialName ∷ MaterialRegistry → MaterialId → Text
materialName reg m = mpName (getMaterialProps reg m)

-- | A set of tiles as a region: a rectangle when they exactly fill their
--   bounding box, otherwise the sorted list.
regionOf ∷ [(Int, Int)] → TileRegion
regionOf ts =
    let xs = map fst ts
        ys = map snd ts
        x0 = minimum xs; x1 = maximum xs; y0 = minimum ys; y1 = maximum ys
        area = (toInteger x1 - toInteger x0 + 1) * (toInteger y1 - toInteger y0 + 1)
    in if area ≡ toInteger (length ts)
        then RegionRect x0 y0 x1 y1
        else RegionTiles ts
