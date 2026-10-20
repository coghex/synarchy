{-# LANGUAGE Strict #-}
-- | Turning validated scenario flora entries into the durable plant
--   state the existing owners reconstruct (#2711, SCN-11 of epic #2698):
--   the pure construction half of the flora adapter.
--
--   __Representation follows the species.__ A groundcover crop species
--   ('World.Flora.Types.isPlantableCropCategory' @"groundcover_crop"@)
--   becomes a tile-keyed 'World.Flora.CropPlot.CropPlot'; every other
--   species becomes an occurrence (a wild plant, or a planted row crop)
--   with its own 'World.Flora.Identity.FloraInstanceId'. The schema has
--   no selector: the species decides, exactly as it does for the
--   planting verbs.
--
--   __Identity.__ Every occurrence takes its id from the page's
--   EXISTING planted allocator ('nextPlantedFloraCursor'), in authored
--   order of the accepted entries, and the advanced cursor travels with
--   the build so the page can never reissue it. No id is manufactured
--   and no third namespace exists. A crop plot has no id — its owner
--   keys it by tile — and never carries the reserved non-identity.
--
--   __The time basis (the schema's @age@ is CURRENT age).__ Growth
--   derives: @age(day) = fiAge + day · growthRate(health)@
--   ('World.Flora.Growth.floraGrowth'). The scenario states the age the
--   plant has AT the initialized clock, so construction stores the
--   BASELINE that derives it — @fiAge = age − day · rate@, searched to the
--   last bit so the derived value is the authored one whenever the float
--   grid at that clock can say it (always at day 0), and otherwise its
--   nearest neighbour on that grid — and capture
--   ("Scenario.World.Capture") exports the derived value, never the
--   stored baseline. A derived value is on the grid, so it round-trips
--   exactly: capture, materialize and capture again is the identity. A crop plot stores a planted DAY instead and
--   derives @age = elapsedDays · rate@, so its authored age lands on
--   the nearest whole elapsed day (the plot owner's own resolution).
--
--   __Age is TOTAL growth.__ The schema's @age@ is the growth the plant has
--   accumulated at the initialized clock — @fiAge + day · rate@ — before
--   the lifecycle wraps it into a generation and a dead window
--   ("World.Flora.Growth"): the value is the plant's, not a display age,
--   so a dead plant exports as dead and a reseeded one as its generation.
--
--   __Where a plant stands.__ On the column's top solid cell. The render
--   and pick resolvers place a plant there whatever its @fiZ@ says, so an
--   authored @z@ other than that top has no representation: it is a field
--   rejection and the plant takes the top. An authored @z@ equal to it is
--   accepted.
--
--   __Defaults.__ An omitted age, offset or variant is what the world
--   generator's own producer ('World.Flora.Placement.mkInstance') makes
--   from a private hash of the scenario id — never gameplay randomness,
--   never an allocation number. An omitted health is full health; an
--   omitted regrowth is "harvestable now". An authored value is
--   authoritative, zero included.
--
--   __Regrowth__ is GAME SECONDS remaining ('World.Flora.Harvest'), and
--   needs no clock conversion. It is valid only for a harvestable
--   occurrence; a crop plot has no harvest timer (its harvest deletes
--   the plot).
--
--   Recoverable problems are data ('ScenarioDiagnostic'). An unknown
--   species, an entry outside the page's bounds, or a second crop plot on
--   one tile rejects the ENTRY (never relocating it); an invalid or
--   inapplicable optional field rejects only that field.
module Scenario.World.Flora
    ( FloraEnv(..)
    , BuiltOccurrence(..)
    , BuiltPlot(..)
    , FloraBuild(..)
    , buildFlora
    , baselineForAge
    , derivedOccurrenceAge
    , plotPlantedDay
    , derivedPlotAge
    , isGroundcoverSpecies
    ) where

import UPrelude
import Data.List (find)
import qualified Data.HashMap.Strict as HM
import qualified Data.Set as Set
import GHC.Float (castFloatToWord32, castWord32ToFloat)
import Gameplay.Tags.Types (GameplayTag)
import Scenario.Items.Refs (stableHash64)
import Scenario.Types
import World.Arena.Bounds (ArenaBounds, arenaAdmitsTile)
import World.Edit.Types (WorldEdit(..))
import World.Flora.CropPlot
    (CropPlot, newCropPlot, cropPlotElapsedDays, cropPlotInstance)
import World.Flora.Growth (floraTotalAge, floraTotalAgeAt, growthRate)
import World.Flora.Identity
    (FloraInstanceId, floraInstanceIdNone, floraInstanceSalt, nextPlantedFloraCursor)
import World.Flora.Placement (mkInstance, speciesMaxAge)
import World.Flora.Types
import World.Generate.Coordinates (globalToChunk)

-- | What construction needs from the engine.
data FloraEnv = FloraEnv
    { feBounds     ∷ !(Maybe ArenaBounds)
    , feCatalog    ∷ !FloraCatalog
    , feAbsDay     ∷ !Int
      -- ^ the initialized clock: 'World.Time.Types.worldAbsoluteDay' of the
      --   page's date
    , feTerrainTop ∷ !((Int, Int) → Int)
      -- ^ the FINAL terrain top of a tile, so a plant's default z is the
      --   surface it stands on
    , feCursor     ∷ !Word64
      -- ^ the page's planted-flora allocator cursor
    }

-- | One accepted occurrence.
data BuiltOccurrence = BuiltOccurrence
    { boPath     ∷ !Text
    , boId       ∷ !ScenarioId
    , boTags     ∷ ![GameplayTag]
    , boInstance ∷ !FloraInstanceId
    , boTile     ∷ !(Int, Int)
    , boSpecies  ∷ !FloraId
    , boState    ∷ !FloraInstance
      -- ^ the complete physical state the durable edits replay
    } deriving (Show, Eq)

-- | One accepted crop plot.
data BuiltPlot = BuiltPlot
    { bpPath ∷ !Text
    , bpId   ∷ !ScenarioId
    , bpTags ∷ ![GameplayTag]
    , bpTile ∷ !(Int, Int)
    , bpPlot ∷ !CropPlot
    } deriving (Show, Eq)

data FloraBuild = FloraBuild
    { flbOccurrences ∷ ![BuiltOccurrence]
    , flbPlots       ∷ ![BuiltPlot]
    , flbEdits       ∷ ![WorldEdit]
      -- ^ per occurrence, in authored order: the planting edit, then the
      --   state edit
    , flbHarvests    ∷ ![(FloraInstanceId, Float)]
      -- ^ regrowth GAME SECONDS remaining, only where a timer is running
    , flbCursor      ∷ !Word64
      -- ^ the allocator cursor after this build
    , flbDiagnostics ∷ ![ScenarioDiagnostic]
    } deriving (Show, Eq)

diagAt ∷ Text → DiagnosticReason → DiagnosticEffect → ScenarioDiagnostic
diagAt = ScenarioDiagnostic

fieldPath ∷ Text → Text → Text
fieldPath p k = p <> "." <> k

-- | Whether a species is a groundcover crop (a crop plot).
isGroundcoverSpecies ∷ FloraCatalog → FloraId → Bool
isGroundcoverSpecies cat fid = case HM.lookup (unFloraId fid) (fcWorldGen cat) of
    Just wg → fwCategory wg ≡ "groundcover_crop"
    Nothing → False

data Acc = Acc
    { accOcc    ∷ ![BuiltOccurrence]
    , accPlots  ∷ ![BuiltPlot]
    , accHarv   ∷ ![(FloraInstanceId, Float)]
    , accCursor ∷ !Word64
    , accTiles  ∷ !(Set.Set (Int, Int))   -- ^ tiles already holding a plot
    , accDiags  ∷ ![ScenarioDiagnostic]
    }

-- | Build the flora of @entries@, each with its document path.
buildFlora ∷ FloraEnv → [(Text, FloraEntry)] → FloraBuild
buildFlora env entries =
    let acc = foldl' step (Acc [] [] [] (feCursor env) Set.empty []) entries
        occs = reverse (accOcc acc)
        edits = concat
            [ [ WePlaceFloraWithId gx gy (boSpecies o) 0 (fiBaseWidth fi) (boInstance o)
              , WeSetFloraState gx gy (boInstance o) (fiZ fi) (fiOffU fi) (fiOffV fi)
                                (fiVariant fi) (fiHealth fi) (fiAge fi) ]
            | o ← occs, let (gx, gy) = boTile o, let fi = boState o ]
    in FloraBuild
        { flbOccurrences = occs
        , flbPlots = reverse (accPlots acc)
        , flbEdits = edits
        , flbHarvests = reverse (accHarv acc)
        , flbCursor = accCursor acc
        , flbDiagnostics = reverse (accDiags acc)
        }
  where
    cat = feCatalog env
    day = feAbsDay env
    step acc (path, e) = case judge acc path e of
        Left ds → acc { accDiags = reverse ds ⧺ accDiags acc }
        Right r → r

    judge acc path e = do
        (fid, sp) ← maybe (Left [diagAt path (UnknownDefinition (feSpecies e)) EntryRejected])
                          Right (findSpeciesByName (feSpecies e) cat)
        when (not (arenaAdmitsTile (feBounds env) (feX e) (feY e))) $
            Left [diagAt path OutsideBounds EntryRejected]
        let tile = (feX e, feY e)
            groundcover = isGroundcoverSpecies cat fid
            health = authoredOr 1.0 (feHealth e)
            rate = growthRate health
            top = feTerrainTop env tile
            wg = HM.lookup (unFloraId fid) (fcWorldGen cat)
            baseWidth = maybe 0 fwFootprint wg
            category = maybe "" fwCategory wg
            seed = stableHash64 (scenarioIdText (feId e))
            salt = floraInstanceSalt (fsName sp) 0
            -- the generator's own producer, keyed by the scenario id
            proto = mkInstance category fid 0 0 top seed (feX e) (feY e) salt
                        0 1 baseWidth (speciesMaxAge fid cat) health floraInstanceIdNone
            age = authoredOr (fiAge proto) (feAge e)
            note k why = diagAt (fieldPath path k) (InvalidValue why) FieldRejected
        if groundcover
          then do
            when (Set.member tile (accTiles acc)) $
                Left [diagAt path (InvalidValue "one crop plot per tile: this tile already has one")
                        EntryRejected]
            let notes = [ note k "not applicable to a groundcover crop plot"
                        | (k, authored) ← [ ("offset_u", isAuthored (feOffsetU e))
                                          , ("offset_v", isAuthored (feOffsetV e))
                                          , ("variant", isAuthored (feVariant e))
                                          , ("regrowth", isAuthored (feRegrowth e)) ]
                        , authored ]
                        ⧺ [ note "z" "a crop plot stands on the column surface"
                          | Authored z ← [feZ e], z ≢ top ]
                plot = newCropPlot fid (plotPlantedDay day rate age) health
            Right acc
                { accPlots = BuiltPlot path (feId e) (feTags e) tile plot : accPlots acc
                , accTiles = Set.insert tile (accTiles acc)
                , accDiags = reverse notes ⧺ accDiags acc }
          else do
            let harvestable = isJust (fsHarvest sp)
                regrowthNote = [ note "regrowth" "only a harvestable species has a regrowth timer"
                               | isAuthored (feRegrowth e), not harvestable ]
                    -- a plant stands on the column's top solid cell: the render
                    -- and pick resolvers place it there whatever fiZ says, so a
                    -- different z has no representation
                    ⧺ [ note "z" ("a plant stands on the column's top solid cell (z "
                                  <> tshow top <> ")")
                      | Authored z ← [feZ e], z ≢ top ]
                regrowth = case feRegrowth e of
                    Authored r | harvestable → r
                    _                        → 0
                (iid, cursor') = nextPlantedFloraCursor (accCursor acc)
                (_, (lx, ly)) = globalToChunk (fst tile) (snd tile)
                state = FloraInstance
                    { fiSpecies = fid
                    , fiTileX = fromIntegral lx
                    , fiTileY = fromIntegral ly
                    , fiOffU = authoredOr (fiOffU proto) (feOffsetU e)
                    , fiOffV = authoredOr (fiOffV proto) (feOffsetV e)
                    , fiZ = top
                    , fiAge = baselineForAge health day age
                    , fiHealth = health
                    , fiVariant = authoredOr (fiVariant proto) (feVariant e)
                    , fiBaseWidth = baseWidth
                    , fiInstanceId = iid
                    , fiChopDesignated = False
                      -- A scenario occurrence is planted through the
                      -- planted allocator (above) and replayed as a
                      -- planting edit, so it is cultivated (#2547).
                    , fiContext = ContextCultivated
                    }
                built = BuiltOccurrence path (feId e) (feTags e) iid tile fid state
            Right acc
                { accOcc = built : accOcc acc
                , accHarv = [ (iid, regrowth) | regrowth > 0 ] ⧺ accHarv acc
                , accCursor = cursor'
                , accDiags = reverse regrowthNote ⧺ accDiags acc }

-- | The stored baseline @b@ such that @b + day · rate@ — the EXACT
--   expression "World.Flora.Growth".'floraGrowth' evaluates — equals the
--   authored current @age@ when any float baseline can make it so. The
--   plain subtraction can be a unit in the last place off, so the
--   neighbours are tried and the first exact one wins. A sum at a large
--   clock can only land on that magnitude's float grid, so an @age@ off
--   the grid has no exact baseline and the plain subtraction (the
--   nearest) is the answer; the value it derives IS on the grid, and
--   asking for that value again is exact.
baselineForAge ∷ Float → Int → Float → Float
baselineForAge health day age =
    let dr = fromIntegral (max 0 day) * growthRate health
        b0 = age - dr
        up = take 8 (iterate nextUp b0)
        down = take 8 (iterate nextDown b0)
    in fromMaybe b0 (find (\b → floraTotalAgeAt b health day ≡ age) (up ⧺ down))

nextUp ∷ Float → Float
nextUp f
    | isNaN f ∨ isInfinite f = f
    | f ≡ 0     = castWord32ToFloat 1
    | f > 0     = castWord32ToFloat (castFloatToWord32 f + 1)
    | otherwise = castWord32ToFloat (castFloatToWord32 f - 1)

nextDown ∷ Float → Float
nextDown = negate ∘ nextUp ∘ negate

-- | The age (game days) a stored occurrence has at @day@ — what capture
--   exports: the TOTAL growth @fiAge + day · rate@, the exact expression
--   "World.Flora.Growth".'floraGrowth' starts from, BEFORE the lifecycle
--   wraps it into a generation and a dead window. Exporting the wrapped
--   display age instead would lose the lifecycle state — a dead plant
--   would come back as a living one — whereas the total, run back through
--   'baselineForAge' and the same hash-derived lifespan, reproduces the
--   generation, the dead window and the phase exactly.
derivedOccurrenceAge ∷ Int → FloraInstance → Float
derivedOccurrenceAge = floraTotalAge

-- | The planted day that gives a crop plot the authored age: its age is
--   @elapsedDays · rate@, so the elapsed days are @age / rate@ rounded
--   to a whole day (never negative).
plotPlantedDay ∷ Int → Float → Float → Int
plotPlantedDay day rate age = day - max 0 (round (age / rate))

-- | The age a crop plot has at @day@ — total growth, before the lifecycle
--   wrap, through the plot owner's own growth adapter
--   ('cropPlotInstance', 'cropPlotElapsedDays'): @elapsedDays · rate@.
derivedPlotAge ∷ Int → CropPlot → Float
derivedPlotAge day plot = floraTotalAge (cropPlotElapsedDays day plot) (cropPlotInstance plot)
