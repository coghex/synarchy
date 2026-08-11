{-# LANGUAGE Strict #-}
-- | The render inputs a world-quad producer shares with every other one,
--   plus the semantically distinct coordinate wrappers that keep a tile's
--   world position from being transposed (#1138, @docs/code_health_findings.md@
--   CH-99).
--
--   'World.Render.TileQuads.tileToQuad' and
--   'World.Render.SideDecoQuads.waterSideFaceQuads' each took fourteen
--   positional parameters, and within them runs of same-typed arguments
--   ('worldX'\/'worldY'\/'worldZ', 'zSlice'\/'effDepth') that only a
--   trailing comment distinguished. Exchanging two compiled cleanly and
--   silently misplaced every tile.
--
--   Two things fix that here, and both matter:
--
--   * 'QuadContext' names the shared head (slot lookups, textures,
--     facing) and the per-pass tail (slice, depth, alpha, wrap offset)
--     ONCE, so a call site forwards one value instead of re-listing
--     eight positionally.
--   * 'WorldX' \/ 'WorldY' \/ 'WorldZ' \/ 'ZSlice' \/ 'EffectiveDepth'
--     are separate types rather than record fields of a common 'Int', so
--     a transposition is a type error at the call site AND inside the
--     record. A record of bare 'Int's would only have moved the hazard
--     into 'QuadContext''s construction.
--
--   The wrappers are deliberately thin: producers unwrap once at the top
--   of their @let@ and do the arithmetic on plain 'Int' exactly as
--   before, so this is a signature change and not a behavioural one.
module World.Render.QuadContext
    ( QuadContext(..)
    , WorldX(..)
    , WorldY(..)
    , WorldZ(..)
    , ZSlice(..)
    , EffectiveDepth(..)
    ) where

import UPrelude
import Engine.Asset.Handle (TextureHandle(..))
import Engine.Graphics.Camera (CameraFacing(..))
import World.Render.Textures.Types (WorldTextures(..))

-- | A tile's global X coordinate.
newtype WorldX = WorldX { unWorldX ∷ Int }

-- | A tile's global Y coordinate.
newtype WorldY = WorldY { unWorldY ∷ Int }

-- | A tile's global Z (elevation) coordinate.
newtype WorldZ = WorldZ { unWorldZ ∷ Int }

-- | The camera's current z-slice: the elevation the pass draws at, and
--   the origin every quad's screen height offset is measured from.
newtype ZSlice = ZSlice { unZSlice ∷ Int }

-- | How many z-levels below 'ZSlice' the pass renders, which is also the
--   range the depth fade/haze is normalised over.
newtype EffectiveDepth = EffectiveDepth { unEffectiveDepth ∷ Int }

-- | Everything a quad producer needs that is fixed for one chunk's pass.
--
--   Constant for the whole frame: the two bindless slot lookups, the
--   texture table, the camera facing, the z-slice, the effective depth
--   and the zoom alpha. Per-chunk: 'qcWrapOffset', the screen shift the
--   cylindrical wrap puts on the chunk being drawn (#1176 — BOTH axes,
--   because at east\/west facings the u-wrap displaces screen Y and not
--   screen X at all). That is why the context is built inside the
--   per-chunk loop rather than once for the frame.
data QuadContext = QuadContext
    { qcLookupSlot     ∷ TextureHandle → Int
      -- ^ handle → bindless tile-texture slot id
    , qcLookupFmSlot   ∷ TextureHandle → Float
      -- ^ handle → bindless face-map slot id
    , qcTextures       ∷ WorldTextures
    , qcFacing         ∷ CameraFacing
    , qcZSlice         ∷ ZSlice
    , qcEffectiveDepth ∷ EffectiveDepth
    , qcTileAlpha      ∷ Float
      -- ^ zoom cross-fade alpha applied to every quad in the pass
    , qcWrapOffset     ∷ (Float, Float)
      -- ^ screen-space (x, y) shift for this chunk's wrap image. Kept as
      --   the pair #1176 produces rather than split into two 'Float'
      --   fields: the value travels whole from
      --   'World.Render.ChunkCulling.isChunkVisibleWrapped' to the
      --   producer that destructures it, and is never rebuilt from two
      --   separate floats, so there is no site at which its axes could
      --   be transposed. Two adjacent bare 'Float' fields would have
      --   created one.
    }
