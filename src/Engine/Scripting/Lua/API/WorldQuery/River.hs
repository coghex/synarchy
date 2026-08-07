{-# LANGUAGE Strict #-}
-- | River-network query: world.getRivers.
module Engine.Scripting.Lua.API.WorldQuery.River
    ( worldGetRiversFn
    ) where

import UPrelude
import qualified HsLua as Lua
import qualified Data.Text.Encoding as TE
import qualified Data.Vector as V
import Engine.Core.Capability.WorldSim
    (WorldSimCapability(..))
import World.Types
import World.River.Identity (timelineRivers)
import World.River.Naming (RiverName(..), lookupRiverName)
import Engine.Scripting.Lua.API.WorldQuery.Lookup (getWorldGenParams)

-- | world.getRivers() → array of river tables
--   Each river: { id=N, source={x,y}, mouth={x,y}, flowRate=N,
--                 segmentCount=N, segments={...} [, name=S, gloss=S] }
--   Each segment: { sx,sy, ex,ey, width, valleyWidth, depth, flowRate,
--                   startElev, endElev }
--
--   Water surface elevation is no longer carried on the segment — it
--   is derived per-tile from the water-table compute at chunk gen.
--   Scripts that need surface heights should call world.getSurfaceAt.
--
--   @id@ (#1102) is the river's underlying
--   'World.Base.GeoFeatureId' as a plain integer — stable across calls,
--   across save/load, and across a regeneration of the same seed. It is
--   page-local: feature ids restart at zero for every timeline, and this
--   query only ever reads the ACTIVE page, so an id is meaningful only
--   against the page it came from. It is absent only if the timeline's
--   compacted river events cannot be matched to its river features
--   ("World.River.Identity"), which the compaction pass makes
--   unreachable — a missing id beats a wrong one.
--
--   @name@ and @gloss@ (#1102) are the river's name in this page's own
--   generated language and its English reading. Both are ABSENT — so
--   ordinary Lua access yields nil — for a page with no #1092 language
--   provenance and for every save written before #1102, mirroring the
--   optional-field convention @world.getIdentity@ already uses. Every
--   other field is unchanged, and so are the order and the number of
--   rivers returned.
worldGetRiversFn ∷ WorldSimCapability → Lua.LuaE Lua.Exception Lua.NumResults
worldGetRiversFn wsc = do
    mParams ← Lua.liftIO $ getWorldGenParams wsc
    case mParams of
        Nothing → do
            Lua.pushnil
            return 1
        Just params → do
            let timeline = wgpGeoTimeline params
                riverNames = wgpRiverNames params
                -- Every compacted river event, paired with the id of
                -- the persistent feature it was emitted from.
                rivers = timelineRivers timeline
            Lua.newtable
            mapM_ (\(rIdx, (mFid, river)) → do
                Lua.newtable
                -- Identity + name (#1102). Absent keys read as nil.
                forM_ mFid $ \fid@(GeoFeatureId rawFid) → do
                    Lua.pushinteger (fromIntegral rawFid)
                    Lua.setfield (Lua.nth 2) "id"
                    forM_ (lookupRiverName fid riverNames) $ \nm → do
                        Lua.pushstring (TE.encodeUtf8 (rvnDisplayName nm))
                        Lua.setfield (Lua.nth 2) "name"
                        forM_ (rvnGloss nm) $ \g → do
                            Lua.pushstring (TE.encodeUtf8 g)
                            Lua.setfield (Lua.nth 2) "gloss"
                -- Source
                let GeoCoord srcX srcY = rpSourceRegion river
                Lua.newtable
                Lua.pushinteger (fromIntegral srcX)
                Lua.setfield (Lua.nth 2) "x"
                Lua.pushinteger (fromIntegral srcY)
                Lua.setfield (Lua.nth 2) "y"
                Lua.setfield (Lua.nth 2) "source"
                -- Mouth
                let GeoCoord mthX mthY = rpMouthRegion river
                Lua.newtable
                Lua.pushinteger (fromIntegral mthX)
                Lua.setfield (Lua.nth 2) "x"
                Lua.pushinteger (fromIntegral mthY)
                Lua.setfield (Lua.nth 2) "y"
                Lua.setfield (Lua.nth 2) "mouth"
                -- Flow rate
                Lua.pushnumber (Lua.Number (realToFrac (rpFlowRate river)))
                Lua.setfield (Lua.nth 2) "flowRate"
                -- Segment count
                Lua.pushinteger (fromIntegral (V.length (rpSegments river)))
                Lua.setfield (Lua.nth 2) "segmentCount"
                -- Segments
                Lua.newtable
                V.iforM_ (rpSegments river) $ \sIdx seg → do
                    let GeoCoord sx sy = rsStart seg
                        GeoCoord ex ey = rsEnd seg
                    Lua.newtable
                    Lua.pushinteger (fromIntegral sx)
                    Lua.setfield (Lua.nth 2) "sx"
                    Lua.pushinteger (fromIntegral sy)
                    Lua.setfield (Lua.nth 2) "sy"
                    Lua.pushinteger (fromIntegral ex)
                    Lua.setfield (Lua.nth 2) "ex"
                    Lua.pushinteger (fromIntegral ey)
                    Lua.setfield (Lua.nth 2) "ey"
                    Lua.pushinteger (fromIntegral (rsWidth seg))
                    Lua.setfield (Lua.nth 2) "width"
                    Lua.pushinteger (fromIntegral (rsValleyWidth seg))
                    Lua.setfield (Lua.nth 2) "valleyWidth"
                    Lua.pushinteger (fromIntegral (rsDepth seg))
                    Lua.setfield (Lua.nth 2) "depth"
                    Lua.pushnumber (Lua.Number (realToFrac (rsFlowRate seg)))
                    Lua.setfield (Lua.nth 2) "flowRate"
                    Lua.pushinteger (fromIntegral (rsStartElev seg))
                    Lua.setfield (Lua.nth 2) "startElev"
                    Lua.pushinteger (fromIntegral (rsEndElev seg))
                    Lua.setfield (Lua.nth 2) "endElev"
                    Lua.rawseti (Lua.nth 2) (fromIntegral sIdx + 1)
                Lua.setfield (Lua.nth 2) "segments"
                Lua.rawseti (Lua.nth 2) rIdx
                ) (zip [1..] rivers)
            return 1
