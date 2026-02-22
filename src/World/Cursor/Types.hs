module World.Cursor.Types
    ( CursorState(..)
    , emptyCursorState
    ) where

import UPrelude
import Engine.Asset.Handle (TextureHandle)

data CursorState = CursorState
    { zoomCursorPos  ∷ Maybe (Int, Int)
    , worldCursorPos ∷ Maybe (Int, Int)
    , zoomCursorTexture ∷ Maybe TextureHandle
    , zoomHoverTexture ∷ Maybe TextureHandle
    }

emptyCursorState ∷ CursorState
emptyCursorState =
    CursorState
        { zoomCursorPos  = Nothing
        , worldCursorPos = Nothing
        , zoomCursorTexture = Nothing
        , zoomHoverTexture = Nothing
        }
