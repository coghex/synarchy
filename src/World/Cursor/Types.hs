module World.Cursor.Types
    ( CursorState(..)
    , emptyCursorState
    ) where

import UPrelude
import qualified Data.HashSet as HS
import Engine.Asset.Handle (TextureHandle)
import World.Construct.Types (StructurePiece)

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
    --
    --   Unlike 'mineAnchor' this is only HALF of what a structure
    --   preview needs (#1846). With no anchor and a piece armed in
    --   'constructStructureTarget', the render pass previews the single
    --   hovered tile; with both, the whole extent. With no armed piece
    --   it previews nothing, anchor or not.
    , constructAnchor ∷ Maybe (Int, Int)
    -- | Ghost texture for committed BUILDING construction designations.
    --   Set from Lua like the cursor textures.
    --
    --   Structures no longer have one: #1846 replaced the category
    --   placeholder with the piece's own art, resolved through #1842's
    --   catalogue and drawn by the same geometry the placer builds with.
    --   DTV-10 (#1845) retires this last one and the mechanism with it.
    , constructBuildingTexture ∷ Maybe TextureHandle
    -- | The structure piece the build tool currently has ARMED, or
    --   'Nothing' when it is off or holding a building target (#1846).
    --
    --   The engine cannot ask Lua what the picker chose, and the ghost
    --   has to draw the piece's OWN art from the first hover — before any
    --   anchor exists and therefore before any designation carries the
    --   descriptor. So the tool states it on entering placement and
    --   clears it on leaving (@construction.setStructureTarget@ /
    --   @clearStructureTarget@), exactly as it already does for
    --   'constructLineMode'.
    --
    --   It is a PREVIEW input only. A committed designation carries its
    --   own target and never reads this, so a stale value can at worst
    --   preview the wrong piece under the cursor, never designate one.
    , constructStructureTarget ∷ Maybe StructurePiece
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
    -- | Building def names already warned about after a committed
    --   CtBuilding designation referenced one missing from bmDefs
    --   (#807). renderWorldCursorQuads runs every frame, so this dedups
    --   the log to one warning per distinct missing name per session
    --   instead of flooding it.
    , constructMissingDefsWarned ∷ HS.HashSet Text
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
        , constructBuildingTexture = Nothing
        , constructStructureTarget = Nothing
        , constructLineMode = False
        , chopAnchor = Nothing
        , chopDesignTexture = Nothing
        , tillAnchor = Nothing
        , tillDesignTexture = Nothing
        , plantDesignTexture = Nothing
        , selectedGroundItem = Nothing
        , constructMissingDefsWarned = HS.empty
        }
