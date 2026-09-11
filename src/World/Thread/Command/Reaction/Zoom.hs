{-# LANGUAGE Strict #-}

-- | Publishing a live zoom-map terrain refresh for the chunks an
--   accepted solidification changed (#2485, requirement 9).
--
--   The detailed tile render needs nothing more than the quad-cache
--   invalidation every terrain edit already does: it rebuilds its quads
--   from the chunk in 'World.State.Types.wsTilesRef', which the edit has
--   already replaced. The ZOOM map does not. Its renderer
--   ('World.Render.Zoom.Quads.renderFromBaked') samples a precomputed
--   atlas texture, and 'World.Render.Zoom.Bake.ensureBakedAtlas' only
--   re-derives quads from it — the terrain pixels themselves were
--   produced once at page initialization and nothing regenerates them.
--   Dropping 'World.State.Types.wsZoomAtlasRef' to force per-material
--   baking is not a repair either: that path colours a whole chunk by
--   its majority material ('World.ZoomMap.Types.zceTexIndex'), in which
--   one new stone tile cannot appear at all.
--
--   So this regenerates the affected chunks' pixels from the live
--   post-edit tiles, patches them into the retained atlas, and
--   republishes the whole image through the SAME
--   'Engine.Core.State.zoomAtlasDataRef' handoff a fresh init and a load
--   publish use — targeted at the exact 'World.State.Types.WorldState'
--   that accepted the edit, never "every visible page" (#763, #1670).
--   The upload assigns a new texture handle, which is what
--   'ensureBakedAtlas' notices to drop the baked entries built against
--   the old one.
module World.Thread.Command.Reaction.Zoom
    ( refreshZoomTerrain
    , zoomTileOverrideFor
    , atlasTileIndexFor
    ) where

import UPrelude
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU
import Control.Monad (foldM)
import Data.IORef (readIORef, writeIORef)
import Engine.Core.Capability.WorldSim
    (WorldSimCapability(..), toWorldSimCapability)
import Engine.Core.Log (logDebug, logWarn, LogCategory(..), LoggerState)
import Engine.Core.State (EngineEnv, zoomAtlasDataRef)
import World.Material (MaterialRegistry)
import World.Types
import World.ZoomMap.Live
    (ZoomTileOverride(..), liveChunkPixels, patchAtlasTile)
import World.ZoomMap.Live.Types (ZoomLiveAtlas(..))

-- | Regenerate and republish the zoom-map tiles of every chunk in
--   @touched@, each with the local cell indices the commit changed.
--
--   Best effort and loud: a page with no atlas of its own to patch, or a
--   chunk the page's zoom cache does not name, is reported and skipped
--   rather than aborting the commit — the stone is already durable in
--   the edit log at this point, and refusing to draw it is not a reason
--   to fail a transaction that has already succeeded.
refreshZoomTerrain ∷ EngineEnv → LoggerState → WorldState
                   → [(ChunkCoord, [Int])] → IO ()
refreshZoomTerrain env logger ws touched
    | null touched = pure ()
    | otherwise = do
        mLive ← readIORef (wsZoomLiveRef ws)
        mParams ← readIORef (wsGenParamsRef ws)
        case (mLive, mParams) of
            (Nothing, _) → logDebug logger CatWorld
                "Zoom refresh skipped: this page publishes no atlas of \
                \its own, so its zoom map renders per material"
            (_, Nothing) → logWarn logger CatWorld
                "Zoom refresh skipped: page has no generation parameters"
            (Just live, Just params) → do
                registry ← readIORef
                    (wsMaterialRegistryRef (toWorldSimCapability env))
                cache ← readIORef (wsZoomCacheRef ws)
                td ← readIORef (wsTilesRef ws)
                patched ← foldM (patchOne logger params registry cache td)
                                live touched
                writeIORef (wsZoomLiveRef ws) (Just patched)
                -- Target the page that accepted the edit and nothing
                -- else: this is the same per-PAGE association #1670
                -- established, and re-reading the world manager when the
                -- upload finally runs would race a load publish.
                writeIORef (zoomAtlasDataRef env) $
                    Just ( zlaWidth patched, zlaHeight patched
                         , zlaPixels patched, [ws] )
                logDebug logger CatWorld $
                    "Zoom refresh: republished atlas for "
                    <> tshow (length touched) <> " chunk(s)"

-- | Patch one chunk's tile into the atlas, or leave the atlas untouched
--   and say why.
patchOne ∷ LoggerState → WorldGenParams → MaterialRegistry
         → V.Vector ZoomChunkEntry → WorldTileData
         → ZoomLiveAtlas → (ChunkCoord, [Int]) → IO ZoomLiveAtlas
patchOne logger params registry cache td live (coord, indices) =
    case (atlasTileIndexFor cache coord, lookupChunk coord td) of
        (Nothing, _) → skip $
            "chunk " <> tshow coord <> " is not in this page's zoom cache"
        (_, Nothing) → skip $
            "chunk " <> tshow coord <> " is no longer loaded"
        (Just idx, Just lc) → do
            let overrides = [ o | i ← indices
                                , Just o ← [zoomTileOverrideFor lc i] ]
                tile = liveChunkPixels params registry (zlaPalette live)
                                       coord overrides
            case patchAtlasTile (zlaWidth live) (zlaChunksPerRow live)
                                idx tile (zlaPixels live) of
                Left why → skip why
                Right pixels → pure live { zlaPixels = pixels }
  where
    skip why = do
        logWarn logger CatWorld ("Zoom refresh skipped: " <> why)
        pure live

-- | Where a chunk's tile sits in the atlas: its position in the page's
--   OWN zoom cache.
--
--   The cache vector and the atlas blocks are packed in the same order
--   by construction ('World.ZoomMap.Cache.BuildPixels.buildZoomCacheWithPixels'
--   emits both from one traversal, and
--   'World.ZoomMap.ChunkTexture.buildZoomAtlas' lays the blocks out by
--   index), so reading the index off the cache is what keeps this from
--   re-deriving — and eventually disagreeing with — that ordering.
atlasTileIndexFor ∷ V.Vector ZoomChunkEntry → ChunkCoord → Maybe Int
atlasTileIndexFor cache (ChunkCoord ccx ccy) =
    V.findIndex (\e → zceChunkX e ≡ ccx ∧ zceChunkY e ≡ ccy) cache

-- | What the LIVE chunk holds at one local cell, as a zoom-tile
--   override. 'Nothing' when the index or the column is out of range,
--   which a refresh reports rather than painting a guess.
zoomTileOverrideFor ∷ LoadedChunk → Int → Maybe ZoomTileOverride
zoomTileOverrideFor lc idx
    | idx < 0 ∨ idx ≥ VU.length (lcTerrainSurfaceMap lc) = Nothing
    | relZ < 0 ∨ relZ ≥ VU.length (ctMats col)           = Nothing
    | otherwise = Just ZoomTileOverride
        { ztoIndex    = idx
        , ztoElev     = topZ
        , ztoMaterial = ctMats col VU.! relZ
        , ztoVeg      = ctVeg  col VU.! relZ
        , ztoFluid    = lcFluidMap lc V.! idx
        }
  where
    topZ = lcTerrainSurfaceMap lc VU.! idx
    col  = lcTiles lc V.! idx
    relZ = topZ - ctStartZ col
