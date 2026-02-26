{-# LANGUAGE Strict, UnicodeSyntax, DeriveGeneric, DeriveAnyClass #-}
module World.Weather.Types
    ( -- * Climate Region Grid
      ClimateCoord(..)
    , ClimateGrid(..)
    , emptyClimateGrid
    , climateRegionSize
      -- * Regional Climate (equilibrium state)
    , RegionClimate(..)
    , defaultRegionClimate
    , SeasonalClimate(..)
      -- * Ocean Simulation
    , OceanCell(..)
    , OceanCurrent(..)
    , OceanGrid(..)
    , emptyOceanGrid
    , WaterMass(..)
    , ThermohalineCell(..)
      -- * Atmospheric Circulation
    , WindCell(..)
    , PressureSystem(..)
    , PressureType(..)
    , AtmoGrid(..)
    , emptyAtmoGrid
    , MoistureCell(..)
      -- * Surface Properties
    , SurfaceType(..)
    , SurfaceBudget(..)
      -- * Simulation State
    , ClimateState(..)
    , initClimateState
    , ClimateParams(..)
    , defaultClimateParams
    ) where

import UPrelude hiding (get)
import Control.DeepSeq (NFData(..))
import GHC.Generics (Generic)
import Data.Serialize (Serialize(..))
import Data.Hashable (Hashable(..))
import qualified Data.HashMap.Strict as HM
import Data.Text.Encoding (encodeUtf8, decodeUtf8)

-----------------------------------------------------------
-- Climate Region Coordinate
-----------------------------------------------------------

-- | Climate regions are 4×4 chunks = 64×64 tiles.
--   This is coarser than your geological RegionCoord (8×8 chunks)
--   but you could unify them if you prefer.
climateRegionSize ∷ Int
climateRegionSize = 4  -- in chunks (so 64 tiles per side)

data ClimateCoord = ClimateCoord !Int !Int
    deriving (Show, Eq, Ord, Generic, Serialize)

instance NFData ClimateCoord where
    rnf (ClimateCoord x y) = rnf x `seq` rnf y

instance Hashable ClimateCoord where
    hashWithSalt s (ClimateCoord x y) =
        s `hashWithSalt` x `hashWithSalt` y

-----------------------------------------------------------
-- Seasonal Climate (summer / winter extremes)
-----------------------------------------------------------

-- | Two-season snapshot. For geological timescales this
--   captures the range; actual monthly interpolation
--   happens at runtime after the game starts.
data SeasonalClimate = SeasonalClimate
    { scSummer ∷ !Float
    , scWinter ∷ !Float
    } deriving (Show, Eq, Generic, Serialize)

instance NFData SeasonalClimate where
    rnf (SeasonalClimate s w) = rnf s `seq` rnf w

-----------------------------------------------------------
-- Regional Climate (equilibrium per geological era)
-----------------------------------------------------------

-- | Climate data for a single 4×4-chunk region.
--   This is the main output of the climate simulation.
--   Each field represents a long-term equilibrium value
--   (averaged over an entire geological age, ~10-50 Myr).
--
--   At game start, these become the *baseline* that the
--   real-time weather system perturbs around.
data RegionClimate = RegionClimate
    { -- Temperature
      rcAirTemp        ∷ !SeasonalClimate  -- ^ Mean air temp (°C), summer/winter
      -- Moisture
    , rcHumidity       ∷ !Float            -- ^ Relative humidity 0.0-1.0
    , rcPrecipitation  ∷ !SeasonalClimate  -- ^ Precipitation (mm/year equivalent), seasonal
    , rcPrecipType     ∷ !Float            -- ^ Fraction that falls as snow (0.0-1.0)
    , rcEvaporation    ∷ !Float            -- ^ Evaporation rate (mm/year equiv)
    , rcCloudCover     ∷ !Float            -- ^ Mean cloud cover 0.0-1.0
      -- Pressure & Wind
    , rcPressure       ∷ !Float            -- ^ Mean sea-level pressure (relative, 1.0 = baseline)
    , rcWindDir        ∷ !Float            -- ^ Prevailing wind direction (radians)
    , rcWindSpeed      ∷ !Float            -- ^ Mean wind speed 0.0-1.0
      -- Derived / terrain interaction
    , rcOrographicLift ∷ !Float            -- ^ How much terrain forces air up (0.0-1.0)
                                           --   High values = windward mountain slopes
    , rcContinentality ∷ !Float            -- ^ Effective distance from ocean moisture
                                           --   (wind-direction-weighted, not Euclidean)
    , rcAlbedo         ∷ !Float            -- ^ Surface reflectivity 0.0-1.0
                                           --   (ice ~0.8, forest ~0.15, desert ~0.35)
    , rcElevAvg        ∷ !Int              -- ^ Mean elevation across region
    } deriving (Show, Eq, Generic, Serialize)

instance NFData RegionClimate where
    rnf (RegionClimate at h p pt e cc pr wd ws ol co al el) =
        rnf at `seq` rnf h `seq` rnf p `seq` rnf pt `seq`
        rnf e `seq` rnf cc `seq` rnf pr `seq` rnf wd `seq`
        rnf ws `seq` rnf ol `seq` rnf co `seq` rnf al `seq` rnf el

defaultRegionClimate ∷ RegionClimate
defaultRegionClimate = RegionClimate
    { rcAirTemp        = SeasonalClimate 15.0 15.0
    , rcHumidity       = 0.5
    , rcPrecipitation  = SeasonalClimate 0.5 0.5
    , rcPrecipType     = 0.0
    , rcEvaporation    = 0.5
    , rcCloudCover     = 0.3
    , rcPressure       = 1.0
    , rcWindDir        = 0.0
    , rcWindSpeed      = 0.3
    , rcOrographicLift = 0.0
    , rcContinentality = 0.5
    , rcAlbedo         = 0.3
    , rcElevAvg        = 0
    }

-----------------------------------------------------------
-- Climate Grid (all regions)
-----------------------------------------------------------

data ClimateGrid = ClimateGrid
    { cgRegions ∷ !(HM.HashMap ClimateCoord RegionClimate)
    , cgSize    ∷ !Int   -- ^ Number of climate regions per side
    } deriving (Show, Eq, Generic, Serialize, NFData)

emptyClimateGrid ∷ Int → ClimateGrid
emptyClimateGrid size = ClimateGrid HM.empty size

-----------------------------------------------------------
-- Ocean Cell
-----------------------------------------------------------

-- | Per-region ocean state. Only exists for ocean regions.
--   The simulation iterates these to convergence each era.
--
--   Currents emerge from:
--   1. Wind stress (surface currents follow prevailing winds)
--   2. Coriolis deflection (latitude-dependent)
--   3. Continental blocking (coastlines redirect flow)
--   4. Thermohaline circulation (density-driven deep currents)
data OceanCell = OceanCell
    { ocTemperature ∷ !SeasonalClimate  -- ^ Sea surface temperature (°C)
    , ocSalinity    ∷ !Float            -- ^ Parts per thousand (global avg ~35)
                                        --   Higher near evaporative subtropical gyres
                                        --   Lower near river mouths and polar ice melt
    , ocDepth       ∷ !Int              -- ^ Ocean depth at this cell (tiles)
                                        --   Drives deep water formation thresholds
    , ocCurrentDir  ∷ !Float            -- ^ Surface current direction (radians)
    , ocCurrentSpd  ∷ !Float            -- ^ Surface current speed (0.0-1.0)
    , ocUpwelling   ∷ !Float            -- ^ Deep water upwelling intensity (0.0-1.0)
                                        --   High near western continental shelves
                                        --   Brings nutrients + cold water to surface
    , ocIceCover    ∷ !Float            -- ^ Fraction covered by sea ice (0.0-1.0)
                                        --   Affects albedo and heat exchange
    } deriving (Show, Eq, Generic, Serialize)

instance NFData OceanCell where
    rnf (OceanCell t s d cd cs u i) =
        rnf t `seq` rnf s `seq` rnf d `seq` rnf cd `seq`
        rnf cs `seq` rnf u `seq` rnf i

-----------------------------------------------------------
-- Ocean Currents (emergent from simulation)
-----------------------------------------------------------

-- | A named ocean current, extracted after simulation converges.
--   These are for bookkeeping/display, not simulation input.
--   The actual simulation works on OceanCell current vectors.
data OceanCurrent = OceanCurrent
    { ocName       ∷ !Text             -- ^ Generated name (e.g. "Northern Gyre")
    , ocPath       ∷ ![(ClimateCoord)] -- ^ Cells this current passes through
    , ocWarm       ∷ !Bool             -- ^ Warm current (affects coastal climate)
    , ocStrength   ∷ !Float            -- ^ Relative strength 0.0-1.0
    } deriving (Show, Eq, Generic, Serialize, NFData)

-- | Deep water mass for thermohaline circulation.
--   Dense cold salty water sinks at polar regions,
--   flows along the bottom, upwells in the tropics.
data WaterMass = WaterMass
    { wmDensity     ∷ !Float   -- ^ Derived from temperature + salinity
    , wmTemperature ∷ !Float   -- ^ Deep water temp (typically 1-4°C)
    , wmSalinity    ∷ !Float   -- ^ Deep water salinity
    , wmFlowDir     ∷ !Float   -- ^ Deep current direction
    , wmFlowSpd     ∷ !Float   -- ^ Deep current speed (much slower than surface)
    } deriving (Show, Eq, Generic, Serialize, NFData)

-- | Thermohaline circulation cell: connects sinking zones
--   (polar, high salinity) to upwelling zones (tropical, divergent).
data ThermohalineCell = ThermohalineCell
    { thcSinkRegion    ∷ !ClimateCoord   -- ^ Where dense water forms and sinks
    , thcUpwellRegion  ∷ !ClimateCoord   -- ^ Where deep water returns to surface
    , thcStrength      ∷ !Float          -- ^ Circulation intensity (0.0-1.0)
    , thcDeepTemp      ∷ !Float          -- ^ Temperature of the deep current
    } deriving (Show, Eq, Generic, Serialize, NFData)

data OceanGrid = OceanGrid
    { ogCells      ∷ !(HM.HashMap ClimateCoord OceanCell)
    , ogDeepWater  ∷ !(HM.HashMap ClimateCoord WaterMass)
    , ogCurrents   ∷ ![OceanCurrent]        -- ^ Named currents (extracted post-sim)
    , ogThcCells   ∷ ![ThermohalineCell]     -- ^ Major thermohaline loops
    } deriving (Show, Eq, Generic, Serialize, NFData)

emptyOceanGrid ∷ OceanGrid
emptyOceanGrid = OceanGrid HM.empty HM.empty [] []

-----------------------------------------------------------
-- Atmospheric Circulation
-----------------------------------------------------------

-- | Per-region atmospheric cell. Wind emerges from
--   pressure gradients + Coriolis + terrain blocking.
--
--   Simulation approach (iterated to convergence per era):
--   1. Compute pressure from temperature gradients
--      (warm air rises → low pressure, cold air sinks → high pressure)
--   2. Wind blows from high to low pressure
--   3. Coriolis deflects wind (right in NH, left in SH)
--   4. Terrain blocks/redirects wind
--   5. Wind over ocean picks up moisture
--   6. Moisture precipitates on orographic lift or convergence
--   7. Temperature adjusts from cloud cover + albedo feedback
--   8. Repeat until stable
data WindCell = WindCell
    { wcDir      ∷ !Float   -- ^ Wind direction (radians)
    , wcSpeed    ∷ !Float   -- ^ Wind speed (0.0-1.0)
    , wcPressure ∷ !Float   -- ^ Sea-level pressure (relative)
    } deriving (Show, Eq, Generic, Serialize)

instance NFData WindCell where
    rnf (WindCell d s p) = rnf d `seq` rnf s `seq` rnf p

-- | Moisture transport cell. Separate from wind because
--   moisture depletes as it precipitates — a wind cell
--   might have strong flow but no moisture left if it
--   already rained on a mountain range upwind.
data MoistureCell = MoistureCell
    { mcFlux     ∷ !Float   -- ^ Moisture flux (0.0-1.0), how much water vapor
                             --   is being carried through this cell
    , mcSource   ∷ !Float   -- ^ Local moisture source (evaporation from ocean/lakes)
    , mcSink     ∷ !Float   -- ^ Local moisture sink (precipitation)
    } deriving (Show, Eq, Generic, Serialize)

instance NFData MoistureCell where
    rnf (MoistureCell f s sk) = rnf f `seq` rnf s `seq` rnf sk

-- | Large-scale pressure systems that emerge from simulation.
--   Bookkeeping only (like OceanCurrent).
data PressureSystem = PressureSystem
    { psCenter   ∷ !ClimateCoord
    , psType     ∷ !PressureType
    , psRadius   ∷ !Int          -- ^ Approximate radius in climate regions
    , psStrength ∷ !Float        -- ^ How far from baseline pressure
    } deriving (Show, Eq, Generic, Serialize, NFData)

data PressureType
    = HighPressure    -- ^ Subtropical high, polar high (clear skies, dry)
    | LowPressure     -- ^ Subpolar low, equatorial trough (storms, rain)
    deriving (Show, Eq, Generic, Serialize, NFData)

data AtmoGrid = AtmoGrid
    { agWind     ∷ !(HM.HashMap ClimateCoord WindCell)
    , agMoisture ∷ !(HM.HashMap ClimateCoord MoistureCell)
    , agSystems  ∷ ![PressureSystem]    -- ^ Major pressure systems (extracted)
    } deriving (Show, Eq, Generic, Serialize, NFData)

emptyAtmoGrid ∷ AtmoGrid
emptyAtmoGrid = AtmoGrid HM.empty HM.empty []

-----------------------------------------------------------
-- Surface Type (drives albedo + evaporation)
-----------------------------------------------------------

-- | Coarse surface classification per climate region.
--   Determined by temperature + precipitation equilibrium.
--   Feeds back into albedo → temperature loop.
data SurfaceType
    = SurfIceSheet         -- ^ Permanent ice (albedo ~0.8)
    | SurfTundra           -- ^ Permafrost + sparse vegetation (albedo ~0.5)
    | SurfBoreal           -- ^ Coniferous forest (albedo ~0.15)
    | SurfTempForest       -- ^ Deciduous/mixed forest (albedo ~0.18)
    | SurfGrassland        -- ^ Steppe/prairie (albedo ~0.25)
    | SurfDesert           -- ^ Arid, sand/rock (albedo ~0.35)
    | SurfTropical         -- ^ Dense tropical forest (albedo ~0.12)
    | SurfSavanna          -- ^ Tropical grassland with scattered trees (albedo ~0.20)
    | SurfWetland          -- ^ Swamp/marsh (albedo ~0.12, high evaporation)
    | SurfOcean            -- ^ Open water (albedo ~0.06)
    deriving (Show, Eq, Enum, Bounded, Generic, Serialize, NFData)

-- | Energy/moisture budget summary for a region.
--   Computed from all the other fields; useful for
--   biome classification and river source identification.
data SurfaceBudget = SurfaceBudget
    { sbSurfaceType ∷ !SurfaceType
    , sbAlbedo      ∷ !Float        -- ^ Effective albedo (from surface type)
    , sbNetMoisture ∷ !Float        -- ^ Precipitation - Evaporation (>0 = wet, river source)
    , sbRunoff      ∷ !Float        -- ^ Fraction of precip that becomes runoff (0.0-1.0)
                                    --   High for mountains, low for flat desert
    , sbSnowpack    ∷ !Float        -- ^ Annual snowpack accumulation
                                    --   Drives spring melt → seasonal river peaks
    } deriving (Show, Eq, Generic, Serialize, NFData)

-----------------------------------------------------------
-- Full Climate Simulation State
-----------------------------------------------------------

-- | The complete climate state, threaded through geological
--   timeline alongside GeoState. Updated each era after
--   plate movements change continent shapes and elevations.
data ClimateState = ClimateState
    { csClimate   ∷ !ClimateGrid     -- ^ Per-region equilibrium climate
    , csOcean     ∷ !OceanGrid       -- ^ Ocean state + currents
    , csAtmo      ∷ !AtmoGrid        -- ^ Atmospheric circulation
    , csSurface   ∷ !(HM.HashMap ClimateCoord SurfaceBudget) -- ^ Surface budgets
    , csGlobalCO2 ∷ !Float           -- ^ Global CO2 level (from GeoState)
    , csGlobalTemp ∷ !Float          -- ^ Global mean temperature offset
    , csSolarConst ∷ !Float          -- ^ Solar constant (can vary over deep time)
    } deriving (Show, Eq, Generic, Serialize, NFData)
instance (Serialize k, Serialize v, Eq k, Hashable k)
    ⇒ Serialize (HM.HashMap k v) where
    put = put . HM.toList
    get = HM.fromList <$> get

initClimateState ∷ Int → ClimateState
initClimateState worldSize =
    let regSize = worldSize `div` climateRegionSize
    in ClimateState
        { csClimate    = emptyClimateGrid regSize
        , csOcean      = emptyOceanGrid
        , csAtmo       = emptyAtmoGrid
        , csSurface    = HM.empty
        , csGlobalCO2  = 1.0
        , csGlobalTemp = 0.0
        , csSolarConst = 1.0
        }

-----------------------------------------------------------
-- Simulation Parameters (tuning knobs)
-----------------------------------------------------------

-- | Knobs for the climate simulation.
--   These control convergence speed and physical constants.
data ClimateParams = ClimateParams
    { cpIterations      ∷ !Int     -- ^ Relaxation iterations per era
    , cpCoriolisScale   ∷ !Float   -- ^ Coriolis strength multiplier
    , cpWindDrag        ∷ !Float   -- ^ How much wind stress drives ocean currents
    , cpThermalInertia  ∷ !Float   -- ^ Ocean thermal lag (0.0-1.0, higher = slower response)
    , cpOrographicScale ∷ !Float   -- ^ How aggressively mountains force rain
    , cpEvapScale       ∷ !Float   -- ^ Evaporation rate multiplier
    , cpAlbedoFeedback  ∷ !Float   -- ^ How much ice-albedo feedback amplifies cooling
    , cpThcThreshold    ∷ !Float   -- ^ Density threshold for deep water formation
    } deriving (Show, Eq, Generic, Serialize, NFData)

defaultClimateParams ∷ ClimateParams
defaultClimateParams = ClimateParams
    { cpIterations      = 50    -- 50 iterations usually converges
    , cpCoriolisScale   = 1.0
    , cpWindDrag        = 0.3
    , cpThermalInertia  = 0.7
    , cpOrographicScale = 1.5
    , cpEvapScale       = 1.0
    , cpAlbedoFeedback  = 0.5
    , cpThcThreshold    = 1.025  -- ~seawater density at 2°C, 35 ppt
    }
