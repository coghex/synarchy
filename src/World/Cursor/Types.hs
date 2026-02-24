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
    , worldCursorPos ∷ Maybe (Int, Int)
    , worldCursorTexture ∷ Maybe TextureHandle
    , worldCursorBgTexture ∷ Maybe TextureHandle
    , worldHoverTexture ∷ Maybe TextureHandle
    , worldHoverBgTexture ∷ Maybe TextureHandle
    , worldSelectedTile ∷ Maybe (Int, Int, Int)
    , worldSelectNow ∷ Bool
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
        , worldCursorTexture = Nothing
        , worldCursorBgTexture = Nothing
        , worldHoverTexture = Nothing
        , worldHoverBgTexture = Nothing
        , worldSelectedTile = Nothing
        , worldSelectNow = False
        }
