{-# LANGUAGE Strict #-}

-- | What a page has to RETAIN to refresh one chunk of its zoom map
--   while the page is live (#2485).
--
--   A leaf on purpose. The generator that uses this ("World.ZoomMap.Live")
--   reaches the worldgen pipeline, which reaches "World.Types" and
--   therefore "World.State.Types" — so the record the page HOLDS cannot
--   live beside the generator without a cycle. Only the palette and the
--   atlas bytes are here; the two 'World.ZoomMap.ColorPalette' and
--   'Data.ByteString' dependencies are both leaves.
module World.ZoomMap.Live.Types
    ( ZoomLiveAtlas(..)
    ) where

import UPrelude
import qualified Data.ByteString as BS
import World.ZoomMap.ColorPalette (ZoomColorPalette)

-- | The zoom atlas a page is currently SHOWING, kept so an accepted
--   terrain edit can regenerate one chunk's tile and republish the
--   whole image.
--
--   Retaining the bytes is the cost of live zoom-map correctness. The
--   renderer samples ONE atlas texture
--   ('World.Render.Zoom.Bake.bakeEntriesAtlas'); there is no per-chunk
--   texture to replace and no partial-upload path, so a changed tile
--   can only reach the screen as a whole re-uploaded image, and the
--   only way to assemble that image without regenerating every chunk
--   is to still have the one that was uploaded.
--
--   'Nothing' whenever this page has no atlas to refresh: a page whose
--   zoom atlas was refused ('Engine.Map.ImageAdmission.admitWorldZoomAtlas'),
--   an arena, or a loaded page that is not the session's atlas owner
--   (#1670). Those pages render the zoom map per material and a live
--   refresh has nothing to patch, which is reported rather than
--   silently skipped.
data ZoomLiveAtlas = ZoomLiveAtlas
    { zlaPalette      ∷ !ZoomColorPalette
      -- ^ The palette this page's pixels were generated with. Rebuilding
      --   it per refresh would re-read @data\/materials@ and
      --   @data\/vegetation@ off disk on the world worker, and a palette
      --   that had drifted since init would repaint the patched tile in
      --   colours its neighbours do not use.
    , zlaWidth        ∷ !Int
    , zlaHeight       ∷ !Int
    , zlaChunksPerRow ∷ !Int
      -- ^ Atlas tiles per row, the same value
      --   'World.Render.Zoom.Types.zaiChunksPerRow' bakes UVs against.
    , zlaPixels       ∷ !BS.ByteString
      -- ^ RGBA8, @zlaWidth * zlaHeight * 4@ bytes.
    }
