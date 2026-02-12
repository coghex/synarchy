{-# LANGUAGE Strict, UnicodeSyntax #-}
module UI.Types
  ( -- Handles
    PageHandle(..)
  , ElementHandle(..)
  , BoxTextureHandle(..)
    -- Layers
  , UILayer(..)
  , TextBuffer(..)
  , emptyBuffer
    -- Page
  , UIPage(..)
    -- Element
  , UIElement(..)
  , UIRenderData(..)
  , UIBoxStyle(..)
  , UITextStyle(..)
  , UISpriteStyle(..)
  -- Box Textures
  , BoxTextureSet(..)
    -- Manager
  , UIPageManager(..)
  , emptyUIPageManager
  ) where

import UPrelude
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import qualified Data.Set as Set
import Engine.Asset.Handle (TextureHandle(..), FontHandle(..))

-----------------------------------------------------------
-- Handles
-----------------------------------------------------------

newtype PageHandle = PageHandle { unPageHandle ∷ Word32 }
  deriving (Eq, Ord, Show)

newtype ElementHandle = ElementHandle { unElementHandle ∷ Word32 }
  deriving (Eq, Ord, Show)

newtype BoxTextureHandle = BoxTextureHandle { unBoxTextureHandle ∷ Word32 }
  deriving (Eq, Ord, Show)

-----------------------------------------------------------
-- Layers and UI Types
-----------------------------------------------------------

data UILayer
  = LayerHUD
  | LayerMenu
  | LayerModal
  | LayerTooltip
  | LayerDebug
  deriving (Eq, Ord, Enum, Bounded, Show)

data TextBuffer = TextBuffer
  { tbContent ∷ T.Text  -- ^ The actual text
  , tbCursor  ∷ Int     -- ^ Cursor position (character index)
  } deriving (Show, Eq)

emptyBuffer ∷ TextBuffer
emptyBuffer = TextBuffer
  { tbContent = T.empty
  , tbCursor  = 0
  }

-----------------------------------------------------------
-- Page and Element Types
-----------------------------------------------------------

data UIPage = UIPage
  { upHandle       ∷ PageHandle
  , upName         ∷ Text
  , upLayer        ∷ UILayer
  , upZIndex       ∷ Int
  , upVisible      ∷ Bool
  , upRootElements ∷ [ElementHandle]
  , upFocusedElement ∷ Maybe ElementHandle
  } deriving (Show)

data UIElement = UIElement
  { ueHandle     ∷ ElementHandle
  , uePage       ∷ PageHandle
  , ueParent     ∷ Maybe ElementHandle
  , ueName       ∷ Text
  , uePosition   ∷ (Float, Float)
  , ueSize       ∷ (Float, Float)
  , ueZIndex     ∷ Int
  , ueVisible    ∷ Bool
  , ueClickable  ∷ Bool
  , ueChildren   ∷ [ElementHandle]
  , ueRenderData ∷ UIRenderData
  , ueOnClick    ∷ Maybe Text
  , ueTextBuffer  ∷ Maybe TextBuffer
  } deriving (Show)

-----------------------------------------------------------
-- Render Data
-----------------------------------------------------------

data UIRenderData
  = RenderNone
  | RenderBox UIBoxStyle
  | RenderText UITextStyle
  | RenderSprite UISpriteStyle
  deriving (Show)

-----------------------------------------------------------
-- Box Textures
-----------------------------------------------------------

data BoxTextureSet = BoxTextureSet
  { btsCenter ∷ TextureHandle
  , btsN      ∷ TextureHandle
  , btsS      ∷ TextureHandle
  , btsE      ∷ TextureHandle
  , btsW      ∷ TextureHandle
  , btsNE     ∷ TextureHandle
  , btsNW     ∷ TextureHandle
  , btsSE     ∷ TextureHandle
  , btsSW     ∷ TextureHandle
  } deriving (Show, Eq)

data UIBoxStyle = UIBoxStyle
  { ubsTextures ∷ BoxTextureHandle
  , ubsTileSize ∷ Float
  , ubsColor    ∷ (Float, Float, Float, Float)
  , ubsOverflow ∷ Float
  } deriving (Show)

data UITextStyle = UITextStyle
  { utsText  ∷ Text
  , utsFont  ∷ FontHandle
  , utsSize  ∷ Float
  , utsColor ∷ (Float, Float, Float, Float)
  } deriving (Show)

data UISpriteStyle = UISpriteStyle
  { ussTexture ∷ TextureHandle
  , ussColor   ∷ (Float, Float, Float, Float)
  } deriving (Show)

-----------------------------------------------------------
-- UI Manager
-----------------------------------------------------------

data UIPageManager = UIPageManager
  { upmPages         ∷ Map.Map PageHandle UIPage
  , upmElements      ∷ Map.Map ElementHandle UIElement
  , upmVisiblePages  ∷ Set.Set PageHandle
  , upmNextPageId    ∷ Word32
  , upmNextElemId    ∷ Word32
  , upmHovered       ∷ Maybe ElementHandle
  , upmBoxTextures   ∷ Map.Map BoxTextureHandle BoxTextureSet
  , upmNextBoxTexId  ∷ Word32
  , upmGlobalFocus ∷ Maybe ElementHandle       
  } deriving (Show)

emptyUIPageManager ∷ UIPageManager
emptyUIPageManager = UIPageManager
  { upmPages        = Map.empty
  , upmElements     = Map.empty
  , upmVisiblePages = Set.empty
  , upmNextPageId   = 1
  , upmNextElemId   = 1
  , upmHovered      = Nothing
  , upmBoxTextures  = Map.empty
  , upmNextBoxTexId = 1
  , upmGlobalFocus = Nothing
  }
