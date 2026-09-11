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
    , atlasTileIndexFor
    ) where

import UPrelude
import qualified Data.ByteString as BS
import qualified Data.HashMap.Strict as HM
import qualified Data.Vector as V
import Control.Monad (foldM)
import Data.IORef (atomicModifyIORef', readIORef, writeIORef)
import Engine.Core.Capability.WorldSim
    (WorldSimCapability(..), toWorldSimCapability)
import Engine.Core.Log (logDebug, logWarn, LogCategory(..), LoggerState)
import Engine.Core.State
    (EngineEnv, ZoomAtlasUpload(..), queueZoomAtlasUpload, zoomAtlasDataRef)
import Engine.Graphics.Camera (CameraFacing(..))
import World.Types
import World.ZoomMap.Live (liveChunkZoom, patchAtlasTile)
import World.ZoomMap.Live.Types (ZoomLiveAtlas(..))

-- | Regenerate and republish the zoom-map terrain of every chunk in
--   @touched@.
--
--   Two products, because the zoom map has two renderers. The per-chunk
--   SUMMARY entry in 'wsZoomCacheRef' is refreshed unconditionally: a
--   page with no atlas of its own bakes one texture per chunk from that
--   entry's majority material and elevation
--   ('World.Render.Zoom.Bake.bakeEntries'), so a refresh that updated
--   only the atlas would leave such a page reading generation-time data
--   forever. The atlas BLOCK is regenerated and republished on top of
--   that, for the page that has one.
--
--   Loud where it cannot do the second half. A page whose zoom map
--   renders per material cannot show one changed tile at all — that path
--   colours a whole chunk by one material — so the skip is reported with
--   what it means rather than filed as a debug line. The commit is not
--   failed for it: the stone is already durable in the edit log by this
--   point, and refusing to draw it is not a reason to fail a transaction
--   that has already succeeded.
refreshZoomTerrain ∷ EngineEnv → LoggerState → WorldPageId → WorldState
                   → [ChunkCoord] → IO ()
refreshZoomTerrain env logger pageId ws touched
    | null touched = pure ()
    | otherwise = do
        mParams ← readIORef (wsGenParamsRef ws)
        case mParams of
            Nothing → logWarn logger CatWorld
                "Zoom refresh skipped: page has no generation parameters"
            Just params → do
                registry ← readIORef
                    (wsMaterialRegistryRef (toWorldSimCapability env))
                mLive ← readIORef (wsZoomLiveRef ws)
                case mLive of
                  -- No atlas means no zoom map at all: a page whose
                  -- atlas was refused does not keep a zoom cache either,
                  -- and an arena never had one. The invariant this rests
                  -- on is "a page with a zoom cache has an atlas", which
                  -- is what makes every zoom map in the engine per-tile
                  -- refreshable rather than some of them silently frozen
                  -- at generation time.
                  Nothing → logDebug logger CatWorld
                      "Zoom refresh skipped: this page has no zoom map"
                  Just live → do
                    td ← readIORef (wsTilesRef ws)
                    cache ← readIORef (wsZoomCacheRef ws)
                    -- The chunk's whole edit log, not this delivery's
                    -- cells: the block is regenerated from
                    -- generation-time data, so an override set scoped to
                    -- one commit would repaint every earlier edit in the
                    -- chunk back to its generated appearance.
                    edits ← readIORef (wsEditsRef ws)
                    let regenerated =
                            [ (cc, liveChunkZoom params registry
                                       (Just (zlaPalette live)) cc lc
                                       (HM.lookupDefault [] cc edits))
                            | cc ← touched
                            , Just lc ← [lookupChunk cc td] ]
                    -- ONE vector threaded through every chunk, not one
                    -- write per chunk from the same starting vector: a
                    -- delivery that touches two chunks would otherwise
                    -- have the second write store the first chunk's
                    -- ORIGINAL entry beside the second chunk's new one,
                    -- losing the first refresh entirely.
                    refreshed ← foldM (refreshCacheEntry logger) cache
                        [ (cc, entry) | (cc, (entry, _)) ← regenerated ]
                    writeIORef (wsZoomCacheRef ws) refreshed
                    patched ← foldM (patchOne logger cache) live
                                    [ (cc, block)
                                    | (cc, (_, Just block)) ← regenerated ]
                    writeIORef (wsZoomLiveRef ws) (Just patched)
                    publishAtlas env pageId ws patched
                    logDebug logger CatWorld $
                        "Zoom refresh: republished atlas for "
                        <> tshow (length regenerated) <> " chunk(s)"
                    -- The baked entries are derived from the cache
                    -- vector, and 'ensureBakedAtlas' notices only a
                    -- changed atlas HANDLE — which this republication
                    -- will eventually supply, but not before the next
                    -- frame or two. Dropping them is what makes the
                    -- refreshed summary visible on the very next bake.
                    writeIORef (wsBakedZoomRef ws)
                        (V.empty, defaultWorldTextures, FaceSouth)

-- | Write one chunk's refreshed summary entry back into the page's zoom
--   cache, IN PLACE.
--
--   In place because the vector's ORDER is the atlas layout: the tile a
--   chunk's pixels occupy is its index here
--   ('World.ZoomMap.ChunkTexture.buildZoomAtlas' lays the blocks out by
--   index), so anything that reordered or resized it would repoint every
--   baked quad.
refreshCacheEntry ∷ LoggerState → V.Vector ZoomChunkEntry
                  → (ChunkCoord, ZoomChunkEntry) → IO (V.Vector ZoomChunkEntry)
refreshCacheEntry logger cache (coord, entry) =
    case atlasTileIndexFor cache coord of
        Nothing → do
            logWarn logger CatWorld $
                "Zoom refresh skipped: chunk " <> tshow coord
                <> " is not in this page's zoom cache"
            pure cache
        Just idx → pure (cache V.// [(idx, entry)])

-- | Hand the patched image to the render thread's upload, targeted at
--   the exact page that accepted the edit and nothing else (#763,
--   #1670).
publishAtlas ∷ EngineEnv → WorldPageId → WorldState → ZoomLiveAtlas
             → IO ()
publishAtlas env pageId ws patched =
    -- Queued, never written over a slot: two pages can commit between
    -- render frames, and overwriting would leave the loser's retained
    -- pixels disagreeing with the texture on screen. A SECOND refresh of
    -- this same page before the render thread drains replaces its own
    -- pending entry, so a busy page cannot queue without bound.
    atomicModifyIORef' (zoomAtlasDataRef env) $ \queued →
        ( queueZoomAtlasUpload
            (ZoomAtlasUpload (zlaWidth patched) (zlaHeight patched)
                             (zlaPixels patched) pageId [ws])
            queued
        , () )

-- | Patch one chunk's regenerated block into the atlas, or leave the
--   atlas untouched and say why.
patchOne ∷ LoggerState → V.Vector ZoomChunkEntry → ZoomLiveAtlas
         → (ChunkCoord, BS.ByteString) → IO ZoomLiveAtlas
patchOne logger cache live (coord, block) =
    case atlasTileIndexFor cache coord of
        Nothing → skip $
            "chunk " <> tshow coord <> " is not in this page's zoom cache"
        Just idx →
            case patchAtlasTile (zlaWidth live) (zlaChunksPerRow live)
                                idx block (zlaPixels live) of
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
