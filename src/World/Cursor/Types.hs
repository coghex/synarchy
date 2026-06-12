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
        , worldCursorTexture = Nothing
        , worldCursorBgTexture = Nothing
        , worldHoverTexture = Nothing
        , worldHoverBgTexture = Nothing
        , worldSelectedTile = Nothing
        , worldSelectNow = False
        , mineAnchor = Nothing
        , mineDesignTexture = Nothing
        , selectedGroundItem = Nothing
        }
