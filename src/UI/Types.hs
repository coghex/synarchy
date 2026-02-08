{-# LANGUAGE Strict #-}
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

-- | Handle to a UI page
newtype PageHandle = PageHandle { unPageHandle :: Word32 }
  deriving (Eq, Ord, Show)

-- | Handle to a UI element
newtype ElementHandle = ElementHandle { unElementHandle :: Word32 }
  deriving (Eq, Ord, Show)

-- | UI layers (rendered bottom to top)
data UILayer
  = LayerHUD
  | LayerMenu
  | LayerModal
  | LayerTooltip
  | LayerDebug
  deriving (Eq, Ord, Enum, Bounded, Show)

-- | A UI page
data UIPage = UIPage
  { upHandle       :: PageHandle
  , upName         :: Text
  , upLayer        :: UILayer
  , upZIndex       :: Int
  , upVisible      :: Bool
  , upRootElements :: [ElementHandle]
  , upFocusedElement :: Maybe ElementHandle
  } deriving (Show)

-- | A UI element
data UIElement = UIElement
  { ueHandle     :: ElementHandle
  , uePage       :: PageHandle
  , ueParent     :: Maybe ElementHandle
  , ueName       :: Text
  , uePosition   :: (Float, Float)
  , ueSize       :: (Float, Float)
  , ueZIndex     :: Int
  , ueVisible    :: Bool
  , ueClickable  :: Bool
  , ueChildren   :: [ElementHandle]
  , ueRenderData :: UIRenderData
  , ueOnClick    :: Maybe Text
  , ueTextBuffer  :: Maybe TextBuffer
  } deriving (Show)

-- | What an element renders as
data UIRenderData
  = RenderNone
  | RenderBox UIBoxStyle
  | RenderText UITextStyle
  | RenderSprite UISpriteStyle
  deriving (Show)

-- | Text buffer with cursor state (for text input elements)
data TextBuffer = TextBuffer
  { tbContent :: T.Text  -- ^ The actual text
  , tbCursor  :: Int     -- ^ Cursor position (character index)
  } deriving (Show, Eq)

-- | Empty text buffer
emptyBuffer :: TextBuffer
emptyBuffer = TextBuffer
  { tbContent = T.empty
  , tbCursor  = 0
  }

-- A handle to a group of 9 textures
newtype BoxTextureHandle = BoxTextureHandle { unBoxTextureHandle :: Word32 }
  deriving (Eq, Ord, Show)

-- The 9 textures for a box
data BoxTextureSet = BoxTextureSet
  { btsCenter :: TextureHandle
  , btsN      :: TextureHandle
  , btsS      :: TextureHandle
  , btsE      :: TextureHandle
  , btsW      :: TextureHandle
  , btsNE     :: TextureHandle
  , btsNW     :: TextureHandle
  , btsSE     :: TextureHandle
  , btsSW     :: TextureHandle
  } deriving (Show, Eq)

data UIBoxStyle = UIBoxStyle
  { ubsTextures :: BoxTextureHandle
  , ubsTileSize :: Float
  , ubsColor    :: (Float, Float, Float, Float)
  , ubsOverflow :: Float
  } deriving (Show)

data UITextStyle = UITextStyle
  { utsText  :: Text
  , utsFont  :: FontHandle
  , utsSize  :: Float
  , utsColor :: (Float, Float, Float, Float)
  } deriving (Show)

data UISpriteStyle = UISpriteStyle
  { ussTexture :: TextureHandle
  , ussColor   :: (Float, Float, Float, Float)
  } deriving (Show)

-- | Manager for all UI pages and elements
data UIPageManager = UIPageManager
  { upmPages         :: Map.Map PageHandle UIPage
  , upmElements      :: Map.Map ElementHandle UIElement
  , upmVisiblePages  :: Set.Set PageHandle
  , upmNextPageId    :: Word32
  , upmNextElemId    :: Word32
  , upmHovered       :: Maybe ElementHandle
  , upmBoxTextures   :: Map.Map BoxTextureHandle BoxTextureSet
  , upmNextBoxTexId  :: Word32
  , upmGlobalFocus :: Maybe ElementHandle       
  } deriving (Show)

emptyUIPageManager :: UIPageManager
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
