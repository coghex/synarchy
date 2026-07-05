module World.Cursor.Types
    ( CursorState(..)
    , emptyCursorState
    ) where

import UPrelude
import Engine.Asset.Handle (TextureHandle)

data CursorState = CursorState
    { zoomCursorPos  ∷ Maybe (Int, Int)
    , zoomCursorTexture ∷ Maybe TextureHandle
    , zoomHoverTexture ∷ Maybe TextureHandle
    , zoomSelectedPos ∷ Maybe (Int, Int)
    , zoomSelectNow   ∷ Bool
    -- | Pixel-space mouse position from input events.
    , worldCursorPos ∷ Maybe (Int, Int)
    -- | Tile-space hover result resolved by the render-thread hit test
    --   (accounts for isometric tilt, camera facing, elevation, and the
    --   u-wrap boundary). Written every frame by renderWorldCursorQuads;
    --   read from Lua via world.getHoverTile().
    , worldHoverTile ∷ Maybe (Int, Int)
    -- | Fractional grid position of the same hover point (item/unit
    --   convention: tile k spans [k, k+1), center at k+0.5). Lets
    --   sub-tile placements (ground-item spawn) land exactly under
    --   the cursor instead of snapping to the tile center. Read from
    --   Lua via world.getHoverPos().
    , worldHoverPos ∷ Maybe (Float, Float)
    , worldCursorTexture ∷ Maybe TextureHandle
    , worldCursorBgTexture ∷ Maybe TextureHandle
    , worldHoverTexture ∷ Maybe TextureHandle
    , worldHoverBgTexture ∷ Maybe TextureHandle
    , worldSelectedTile ∷ Maybe (Int, Int, Int)
    , worldSelectNow ∷ Bool
    -- | Mine-designation tool: first-click anchor tile. While set,
    --   the render pass previews the anchor→hover rectangle; the
    --   second click commits it (WorldDesignateMine) and clears this.
    , mineAnchor ∷ Maybe (Int, Int)
    -- | Texture for committed mine-designation markers (set from Lua
    --   like the cursor textures; rendered over designated tiles).
    , mineDesignTexture ∷ Maybe TextureHandle
    -- | Construction-designation tool (#95): first-click anchor tile.
    --   Mirrors 'mineAnchor' — the render pass previews the
    --   anchor→hover rectangle until the second click commits it.
    , constructAnchor ∷ Maybe (Int, Int)
    -- | Ghost textures for committed construction designations, one per
    --   target category (structure pieces vs buildings) so the blueprint
    --   reads differently for each. Set from Lua like the cursor textures.
    , constructStructTexture ∷ Maybe TextureHandle
    , constructBuildingTexture ∷ Maybe TextureHandle
    -- | Wire path tool (#359): while true, the anchor→hover preview (and
    --   the build tool's commit, which snaps the same way in Lua) is
    --   constrained to a straight 1-wide LINE along whichever axis has
    --   the larger extent from the anchor, instead of the filled
    --   rectangle every other structure piece designates. Set from Lua
    --   (construction.setLineMode) when entering/leaving wire placement.
    , constructLineMode ∷ Bool
    -- | Chop-designation tool (#97): first-click anchor tile. Mirrors
    --   'mineAnchor' — the render pass previews the anchor→hover
    --   rectangle until the second click commits it.
    , chopAnchor ∷ Maybe (Int, Int)
    -- | Texture for committed chop-designation markers (set from Lua
    --   like the cursor textures; rendered over designated trees).
    , chopDesignTexture ∷ Maybe TextureHandle
    -- | Till-designation tool (#333): first-click anchor tile. Mirrors
    --   'mineAnchor' — the render pass previews the anchor→hover
    --   rectangle until the second click commits it.
    , tillAnchor ∷ Maybe (Int, Int)
    -- | Texture for committed till-designation markers (set from Lua
    --   like the cursor textures; rendered over designated fields).
    , tillDesignTexture ∷ Maybe TextureHandle
    -- | Texture for committed plant-designation markers (#335). No
    --   anchor field — the plant tool is single-tile, no pending
    --   rectangle to preview.
    , plantDesignTexture ∷ Maybe TextureHandle
    -- | Ground item selected in the world view (white outline +
    --   info panel). Mutually exclusive with unit/building selection
    --   (enforced by the Lua click routing).
    , selectedGroundItem ∷ Maybe Int
    }

emptyCursorState ∷ CursorState
emptyCursorState =
    CursorState
        { zoomCursorPos  = Nothing
        , zoomCursorTexture = Nothing
        , zoomHoverTexture = Nothing
        , zoomSelectedPos = Nothing
        , zoomSelectNow = False
        , worldCursorPos = Nothing
        , worldHoverTile = Nothing
        , worldHoverPos = Nothing
        , worldCursorTexture = Nothing
        , worldCursorBgTexture = Nothing
        , worldHoverTexture = Nothing
        , worldHoverBgTexture = Nothing
        , worldSelectedTile = Nothing
        , worldSelectNow = False
        , mineAnchor = Nothing
        , mineDesignTexture = Nothing
        , constructAnchor = Nothing
        , constructStructTexture = Nothing
        , constructBuildingTexture = Nothing
        , constructLineMode = False
        , chopAnchor = Nothing
        , chopDesignTexture = Nothing
        , tillAnchor = Nothing
        , tillDesignTexture = Nothing
        , plantDesignTexture = Nothing
        , selectedGroundItem = Nothing
        }
