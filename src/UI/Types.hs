{-# LANGUAGE Strict #-}
module UI.Types
  ( -- Handles
    PageHandle(..)
  , ElementHandle(..)
    -- Layers
  , UILayer(..)
    -- Page
  , UIPage(..)
    -- Element
  , UIElement(..)
  , UIRenderData(..)
  , UIBoxStyle(..)
  , UITextStyle(..)
  , UISpriteStyle(..)
    -- Manager
  , UIPageManager(..)
  , emptyUIPageManager
  ) where

import UPrelude
import qualified Data.Map.Strict as Map
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
  } deriving (Show)

-- | A UI element
data UIElement = UIElement
  { ueHandle       :: ElementHandle
  , uePage         :: PageHandle
  , ueParent       :: Maybe ElementHandle
  , ueName         :: Text
  , uePosition     :: (Float, Float)
  , ueSize         :: (Float, Float)
  , ueZIndex       :: Int
  , ueVisible      :: Bool
  , ueClickable    :: Bool
  , ueChildren     :: [ElementHandle]
  , ueRenderData   :: UIRenderData
  } deriving (Show)

-- | What an element renders as
data UIRenderData
  = RenderNone
  | RenderBox UIBoxStyle
  | RenderText UITextStyle
  | RenderSprite UISpriteStyle
  deriving (Show)

data UIBoxStyle = UIBoxStyle
  { ubsColor :: (Float, Float, Float, Float)
  } deriving (Show)

data UITextStyle = UITextStyle
  { utsText  :: Text
  , utsFont  :: FontHandle
  , utsColor :: (Float, Float, Float, Float)
  } deriving (Show)

data UISpriteStyle = UISpriteStyle
  { ussTexture :: TextureHandle
  , ussColor   :: (Float, Float, Float, Float)
  } deriving (Show)

-- | Manager for all UI pages and elements
data UIPageManager = UIPageManager
  { upmPages        :: Map.Map PageHandle UIPage
  , upmElements     :: Map.Map ElementHandle UIElement
  , upmVisiblePages :: Set.Set PageHandle
  , upmNextPageId   :: Word32
  , upmNextElemId   :: Word32
  , upmHovered      :: Maybe ElementHandle
  , upmDefaultBoxTex:: Maybe TextureHandle
  } deriving (Show)

emptyUIPageManager :: UIPageManager
emptyUIPageManager = UIPageManager
  { upmPages        = Map.empty
  , upmElements     = Map.empty
  , upmVisiblePages = Set.empty
  , upmNextPageId   = 1
  , upmNextElemId   = 1
  , upmHovered      = Nothing
  , upmDefaultBoxTex= Nothing
  }
