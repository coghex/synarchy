{-# LANGUAGE Strict, UnicodeSyntax #-}
module UI.Types
  ( -- * Handles
    PageHandle(..)
  , ElementHandle(..)
  , BoxTextureHandle(..)
    -- * Layers
  , UILayer(..)
  , TextBuffer(..)
  , emptyBuffer
    -- * Page
  , UIPage(..)
    -- * Element
  , UIElement(..)
  , UIRenderData(..)
  , UIBoxStyle(..)
  , UITextStyle(..)
  , UISpriteStyle(..)
    -- * Box Textures
  , BoxTextureSet(..)
    -- * Tooltips
  , TooltipContent(..)
  , TooltipSprite(..)
  , TooltipStyle(..)
  , TooltipState(..)
  , defaultTooltipStyle
  , emptyTooltipState
    -- * Manager
  , UIPageManager(..)
  , emptyUIPageManager
  ) where

import UPrelude
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import qualified Data.Set as Set
import qualified Data.Vector as V
import Engine.Asset.Handle (TextureHandle(..), FontHandle(..))

-- | Handle to a UI page
newtype PageHandle = PageHandle { unPageHandle ∷ Word32 }
  deriving (Eq, Ord, Show)

-- | Handle to a UI element
newtype ElementHandle = ElementHandle { unElementHandle ∷ Word32 }
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
  { upHandle       ∷ PageHandle
  , upName         ∷ Text
  , upLayer        ∷ UILayer
  , upZIndex       ∷ Int
  , upVisible      ∷ Bool
  , upRootElements ∷ [ElementHandle]
  , upFocusedElement ∷ Maybe ElementHandle
  } deriving (Show)

-- | A UI element
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
  , ueOnRightClick ∷ Maybe Text
  , ueTextBuffer  ∷ Maybe TextBuffer
  , ueTooltip     ∷ Maybe TooltipContent
    -- ^ Optional hover tooltip. When set and the cursor lingers over
    --   the element for the configured dwell time, a small floating
    --   panel appears next to the cursor showing this content.
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
  { tbContent ∷ T.Text  -- ^ The actual text
  , tbCursor  ∷ Int     -- ^ Cursor position (character index)
  } deriving (Show, Eq)

-- | Empty text buffer
emptyBuffer ∷ TextBuffer
emptyBuffer = TextBuffer
  { tbContent = T.empty
  , tbCursor  = 0
  }

-- | Handle to a group of 9 textures (3x3 box corners/edges/center)
newtype BoxTextureHandle = BoxTextureHandle { unBoxTextureHandle ∷ Word32 }
  deriving (Eq, Ord, Show)

-- | The 9 textures for a 3x3 box
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

-- | Visual content for a tooltip. A tooltip can carry any combination
--   of an optional text line plus zero or more sprites; each sprite
--   can be static (one frame) or animated (multiple frames cycled at
--   'tsFrameDurMs').
data TooltipContent = TooltipContent
  { ttText     ∷ Maybe Text
  , ttSprites  ∷ [TooltipSprite]
  , ttMaxWidth ∷ Maybe Float
    -- ^ Optional max content width in pixels (sprite row is laid out
    --   left-to-right beneath the text; values below the natural
    --   width are ignored).
  } deriving (Show, Eq)

data TooltipSprite = TooltipSprite
  { tsFrames     ∷ V.Vector TextureHandle
    -- ^ One entry = static sprite; more than one = animated.
  , tsFrameDurMs ∷ Int
    -- ^ Per-frame duration in milliseconds (ignored when one frame).
  , tsSize       ∷ (Float, Float)
  } deriving (Show, Eq)

-- | Visual style for tooltips. Configurable from Lua via
--   UI.setTooltipStyle so games can pick a font / box look that
--   matches the rest of their UI. Handles default to 0 (unset);
--   the runtime degrades gracefully when a resource is missing
--   (text won't render without a font, box backdrop won't render
--   without a box-texture set).
data TooltipStyle = TooltipStyle
  { tsFont          ∷ FontHandle
  , tsFontSize      ∷ Float
  , tsTextColor     ∷ (Float, Float, Float, Float)
  , tsBgColor       ∷ (Float, Float, Float, Float)
  , tsPadding       ∷ Float
  , tsBoxTextures   ∷ BoxTextureHandle
  , tsBoxTileSize   ∷ Float
  , tsMouseOffsetX  ∷ Float
  , tsMouseOffsetY  ∷ Float
  , tsDwellMs       ∷ Float
    -- ^ How long the cursor must rest before the tooltip appears.
  , tsSpriteGap     ∷ Float
    -- ^ Horizontal gap between sprites in the sprite row.
  } deriving (Show)

defaultTooltipStyle ∷ TooltipStyle
defaultTooltipStyle = TooltipStyle
  { tsFont         = FontHandle 0
  , tsFontSize     = 16
  , tsTextColor    = (1.0, 1.0, 1.0, 1.0)
  , tsBgColor      = (1.0, 1.0, 1.0, 1.0)
  , tsPadding      = 8
  , tsBoxTextures  = BoxTextureHandle 0
  , tsBoxTileSize  = 32
  , tsMouseOffsetX = 14
  , tsMouseOffsetY = 18
  , tsDwellMs      = 400
  , tsSpriteGap    = 4
  }

-- | Per-frame runtime state for the tooltip subsystem. Owned by the
--   UI page manager so it lives behind the existing uiManagerRef
--   without needing a new top-level IORef.
data TooltipState = TooltipState
  { ttsStyle           ∷ TooltipStyle
  , ttsHoveredElem     ∷ Maybe ElementHandle
    -- ^ Element the cursor is currently resting on (may not yet have
    --   triggered a show; see 'ttsDwellRemaining').
  , ttsDwellRemaining  ∷ Float
    -- ^ Milliseconds remaining before the tooltip appears. Reset to
    --   the style's dwell every time the hover target changes.
  , ttsActivePage      ∷ Maybe PageHandle
    -- ^ Transient tooltip page (lazily created on first show; reused
    --   across subsequent shows).
  , ttsActiveContent   ∷ Maybe TooltipContent
    -- ^ Snapshot of the content currently being rendered. Used to
    --   detect when the content changed and the visuals need rebuilding.
  , ttsActiveElem      ∷ Maybe ElementHandle
    -- ^ The element whose content is currently being shown. Distinct
    --   from 'ttsHoveredElem' because the content can outlive the
    --   hover for an animation frame.
  , ttsAnimTimeMs      ∷ Float
    -- ^ Monotonic ms counter used to derive animation frame indices
    --   for animated sprites.
  , ttsSpriteHandles   ∷ [ElementHandle]
    -- ^ Sprite element handles for the current shown tooltip, in the
    --   same order as 'ttSprites' on the active content. Tracked so
    --   the per-frame tick can cheaply swap textures without rebuilding.
  , ttsBoxHandle       ∷ Maybe ElementHandle
  , ttsTextHandle      ∷ Maybe ElementHandle
  } deriving (Show)

emptyTooltipState ∷ TooltipState
emptyTooltipState = TooltipState
  { ttsStyle          = defaultTooltipStyle
  , ttsHoveredElem    = Nothing
  , ttsDwellRemaining = 0
  , ttsActivePage     = Nothing
  , ttsActiveContent  = Nothing
  , ttsActiveElem     = Nothing
  , ttsAnimTimeMs     = 0
  , ttsSpriteHandles  = []
  , ttsBoxHandle      = Nothing
  , ttsTextHandle     = Nothing
  }

-- | Manager for all UI pages and elements
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
  , upmTooltip     ∷ TooltipState
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
  , upmTooltip     = emptyTooltipState
  }
