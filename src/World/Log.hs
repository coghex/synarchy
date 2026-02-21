{-# LANGUAGE Strict, UnicodeSyntax #-}
module World.Log
    ( -- * The unified world logger
      WorldLogger(..)
    , WorldLogDest(..)
    , WorldVerbosity(..)
    , makeWorldLogger
      -- * Subsystem log dispatch
    , logWorldGen
    , logGeoTimeline
    , logHydrology
    , logWeather
    ) where

import UPrelude
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.IORef (IORef, readIORef)
import qualified Data.Text as T
import qualified Engine.Core.Queue as Q
import Engine.Core.Log ( LoggerState, LogCategory(..), LogLevel(..)
                       , logInfo, logDebug, logThreadInfo, logThreadDebug )
import Engine.Core.State (EngineEnv(..))
import Engine.Scripting.Lua.Types (LuaMsg(..))
import World.Types
import World.Geology.Types
import World.Hydrology.Types
import World.Weather.Types
import World.Plate (TectonicPlate(..))
import qualified World.Geology.Log   as GeoLog
import qualified World.Hydrology.Log as HydroLog
import qualified World.Weather.Log   as WeatherLog

-----------------------------------------------------------
-- Verbosity Control
-----------------------------------------------------------

-- | Per-subsystem verbosity levels.
--   Silent  = no output at all
--   Summary = one-liner totals only
--   Normal  = section headers + key stats (default)
--   Verbose = full expanded event log
data WorldVerbosity
    = Silent
    | Summary
    | Normal
    | Verbose
    deriving (Show, Eq, Ord, Enum, Bounded)

-----------------------------------------------------------
-- Log Destination
-----------------------------------------------------------

-- | Where a log line should be sent.
data WorldLogDest
    = DestEngine   -- ^ Engine file/console logger only (logInfo / logDebug)
    | DestInGame   -- ^ In-game Lua panel only (LuaWorldGenLog)
    | DestBoth     -- ^ Both destinations (typical for world-gen progress)
    deriving (Show, Eq)

-----------------------------------------------------------
-- WorldLogger
-----------------------------------------------------------

-- | A bundle of everything needed to emit world-gen log lines.
--   Pass this down instead of bare LoggerState / EngineEnv.
data WorldLogger = WorldLogger
    { wlLogger      ∷ !LoggerState  -- ^ Engine logger (file / console)
    , wlLuaQueue    ∷ !(Q.Queue LuaMsg) -- ^ Queue for in-game panel
    , wlDest        ∷ !WorldLogDest     -- ^ Default destination
    , wlGeoV        ∷ !WorldVerbosity   -- ^ Geology verbosity
    , wlHydroV      ∷ !WorldVerbosity   -- ^ Hydrology verbosity
    , wlWeatherV    ∷ !WorldVerbosity   -- ^ Weather verbosity
    }

-- | Construct a WorldLogger from the EngineEnv.
--   Verbosities default to Normal; callers may override the fields.
makeWorldLogger ∷ EngineEnv → IORef LoggerState → WorldLogDest → IO WorldLogger
makeWorldLogger env logRef dest = do
    logger ← readIORef logRef
    return WorldLogger
        { wlLogger   = logger
        , wlLuaQueue = luaQueue env
        , wlDest     = dest
        , wlGeoV     = Normal
        , wlHydroV   = Normal
        , wlWeatherV = Normal
        }

-----------------------------------------------------------
-- Core emit helper
-----------------------------------------------------------

-- | Emit a single line respecting the destination setting.
emitLine ∷ MonadIO m ⇒ WorldLogger → WorldLogDest → Text → m ()
emitLine wl dest msg = liftIO $ case dest of
    DestEngine → logThreadInfo (wlLogger wl) CatWorld msg
    DestInGame → Q.writeQueue (wlLuaQueue wl) (LuaWorldGenLog msg)
    DestBoth   → do
        logThreadInfo (wlLogger wl) CatWorld msg
        Q.writeQueue (wlLuaQueue wl) (LuaWorldGenLog msg)

-- | Emit a debug-level line (engine only, never in-game).
emitDebug ∷ MonadIO m ⇒ WorldLogger → Text → m ()
emitDebug wl msg = liftIO $ logThreadDebug (wlLogger wl) CatWorld msg

-- | Emit lines from a [Text] list using the logger's default destination.
emitLines ∷ MonadIO m ⇒ WorldLogger → [Text] → m ()
emitLines wl = mapM_ (emitLine wl (wlDest wl))

-----------------------------------------------------------
-- Generic world-gen progress message
-----------------------------------------------------------

-- | Emit a free-form world-gen progress message (both destinations).
logWorldGen ∷ MonadIO m ⇒ WorldLogger → Text → m ()
logWorldGen wl = emitLine wl DestBoth

-----------------------------------------------------------
-- Geology dispatch
-----------------------------------------------------------

-- | Log the geological timeline, respecting wlGeoV.
logGeoTimeline ∷ MonadIO m ⇒ WorldLogger → GeoTimeline → Word64 → Int → Int → m ()
logGeoTimeline wl tl seed worldSize plateCount =
    case wlGeoV wl of
        Silent  → return ()
        Summary → emitLines wl (GeoLog.formatTimelineSummaryLines tl)
        Normal  → do
            emitLines wl (GeoLog.formatTimelineSummaryLines tl)
            emitLines wl (GeoLog.formatPlatesSummary seed worldSize plateCount)
        Verbose → do
            emitLines wl (GeoLog.formatTimeline tl)
            emitLines wl (GeoLog.formatPlatesSummary seed worldSize plateCount)

-----------------------------------------------------------
-- Hydrology dispatch
-----------------------------------------------------------

-- | Log the hydrological features, respecting wlHydroV.
logHydrology ∷ MonadIO m ⇒ WorldLogger → [PersistentFeature] → m ()
logHydrology wl features =
    case wlHydroV wl of
        Silent  → return ()
        Summary → emitLines wl (HydroLog.formatHydroSummary features)
        Normal  → emitLines wl (HydroLog.formatHydroNormal features)
        Verbose → emitLines wl (HydroLog.formatHydroVerbose features)

-----------------------------------------------------------
-- Weather dispatch
-----------------------------------------------------------

-- | Log the climate state, respecting wlWeatherV.
logWeather ∷ MonadIO m ⇒ WorldLogger → ClimateState → m ()
logWeather wl cs =
    case wlWeatherV wl of
        Silent  → return ()
        Summary → emitLines wl (WeatherLog.formatWeatherSummary cs)
        Normal  → emitLines wl (WeatherLog.formatWeatherNormal cs)
        Verbose → emitLines wl (WeatherLog.formatWeatherVerbose cs)
