{-# LANGUAGE Strict, UnicodeSyntax #-}
module UI.Types
  ( -- * Handles
    PageHandle(..)
  , ElementHandle(..)
  , BoxTextureHandle(..)
    -- * Layers
  , UILayer(..)
  , uiLayerBand
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

-- | UI layers (rendered bottom to top). Layer alone only decides paint
--   order — whether a page actually BLOCKS input reaching whatever
--   paints below it is the separate, per-page 'upInputExclusive' flag
--   (see 'UI.InputOwnership'). 'LayerModal' pages default exclusive
--   (a real modal boundary); every other layer, including 'LayerDebug'
--   and 'LayerTooltip', defaults pass-through — a miss on them simply
--   continues the search to whatever paints below.
data UILayer
  = LayerHUD      -- ^ in-world hud (tile cursor, selections)
  | LayerOverlay  -- ^ hud chrome over the world view (toolbar, panels)
  | LayerMenu
  | LayerModal
  | LayerTooltip
  | LayerDebug    -- ^ shell + debug overlay: pass-through by design,
                  --   always painted (and hit-tested) above any modal
  deriving (Eq, Ord, Enum, Bounded, Show)

-- | Paint-order band for a page — the SINGLE source of truth for UI
--   stacking. The renderer ('UI.Render.uiLayerToLayerId') draws at
--   @band + accumulated element zIndex@; hit-testing ('UI.Manager')
--   reproduces the same key, so the element you SEE on top is the
--   element the cursor interacts with. Bands are spaced so one
--   element's accumulated zIndex cannot climb into the next band
--   (popups use up to ~1003; HUD text/cursor children use 1-2 — the
--   old bases were 1 apart, which let HUD elements paint over menu
--   pages).
uiLayerBand ∷ UILayer → Int → Int
uiLayerBand layer pageZ = case layer of
    LayerHUD     → 0
    LayerOverlay → 5000
    LayerMenu    → 10000
    LayerModal   → 20000 + pageZ
    LayerTooltip → 100000 + pageZ
    LayerDebug   → 200000 + pageZ

-- | A UI page
data UIPage = UIPage
  { upHandle       ∷ PageHandle
  , upName         ∷ Text
  , upLayer        ∷ UILayer
  , upZIndex       ∷ Int
  , upVisible      ∷ Bool
  , upRootElements ∷ [ElementHandle]
  , upFocusedElement ∷ Maybe ElementHandle
  , upInputExclusive ∷ Bool
    -- ^ #742: when visible, this page establishes a modal input
    --   boundary — pointer input that misses every owned control on
    --   or above it cannot reach a lower page or the game world (see
    --   'UI.InputOwnership.routePointer'). Defaults to
    --   @upLayer ≡ LayerModal@ at creation ('UI.Manager.Page.createPage');
    --   override with 'UI.Manager.Page.setPageInputExclusive' for a
    --   modal-layer page that is only visual stacking, not a real
    --   input-exclusive dialog (e.g. 'scripts/popup.lua's notification
    --   cards).
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
  , ueBlocksPointer ∷ Bool
    -- ^ #743: explicit opt-in that this element blocks pointer input
    --   (left/right/middle) even with no click callback of its own —
    --   independent of 'ueClickable'/'ueOnClick'/'ueOnRightClick'. The
    --   EFFECTIVE predicate a click/middle-click route actually
    --   consults is 'UI.Manager.Query.elementBlocksPointer', which
    --   ORs this flag with the pre-existing rule that a clickable
    --   control with a registered left- or right-click callback
    --   blocks by default (so 'UI.setClickable' + 'UI.setOnClick'
    --   keeps working exactly as before with this left at its default
    --   'False'). Defaults to 'False' — a purely visual element stays
    --   pointer-pass-through.
  , ueCapturesScroll ∷ Bool
    -- ^ #743: explicit opt-in that this element captures wheel/scroll
    --   input, independent of click/pointer-blocking policy — see
    --   'UI.InputOwnership.routeScroll'. Unlike 'ueBlocksPointer' this
    --   has no callback-derived fallback: nothing about registering a
    --   click callback implies scroll capture. Defaults to 'False'.
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
  , ttHint     ∷ Maybe Text
    -- ^ Optional secondary line rendered beneath a thin horizontal
    --   separator in a smaller, dimmer font. Used for "Right click
    --   to expand"-style affordance hints under the main title.
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
  , tsHintDelayMs   ∷ Float
    -- ^ Additional delay (cumulative with 'tsDwellMs') before the
    --   hint + separator section transitions in. The tooltip first
    --   appears with title only; after the cursor lingers another
    --   'tsHintDelayMs', the rich form fades in.
  , tsSpriteGap     ∷ Float
    -- ^ Horizontal gap between sprites in the sprite row.
  , tsHintFontSize  ∷ Float
    -- ^ Font size for the hint line beneath the separator.
  , tsHintColor     ∷ (Float, Float, Float, Float)
  , tsSeparatorColor ∷ (Float, Float, Float, Float)
  , tsSeparatorThickness ∷ Float
    -- ^ Height of the horizontal separator between title and hint.
  , tsSeparatorTexture ∷ TextureHandle
    -- ^ Texture used for the separator strip. Set this to a 1×1 white
    --   pixel (or any flat fill) so the colour tint above produces the
    --   exact requested colour. When unset (handle 0), the renderer
    --   falls back to the centre tile of the configured box-texture
    --   set, which makes the separator look like a sliver of the box
    --   fill — usually too low-contrast to read as a divider.
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
  , tsHintDelayMs  = 400
  , tsSpriteGap    = 4
  , tsHintFontSize = 11
  , tsHintColor    = (0.7, 0.7, 0.7, 1.0)
  , tsSeparatorColor = (1.0, 1.0, 1.0, 1.0)
  , tsSeparatorThickness = 1.0
  , tsSeparatorTexture = TextureHandle 0
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
  , ttsHintRemainingMs ∷ Float
    -- ^ Countdown (ms) until the hint + separator stage of the
    --   tooltip transitions in. Reset to 'tsHintDelayMs' when the
    --   tooltip first appears for a new element; decremented while
    --   the same tooltip remains showing. Once it hits 0, the
    --   displayed content swaps to the rich (text + separator + hint)
    --   form on the next frame.
  , ttsSpriteHandles   ∷ [ElementHandle]
    -- ^ Sprite element handles for the current shown tooltip, in the
    --   same order as 'ttSprites' on the active content. Tracked so
    --   the per-frame tick can cheaply swap textures without rebuilding.
  , ttsBoxHandle       ∷ Maybe ElementHandle
  , ttsTextHandle      ∷ Maybe ElementHandle
  , ttsHintHandles     ∷ [ElementHandle]
    -- ^ One text element per line in the hint (split on '\n'). Empty
    --   list when there's no hint. Stacked vertically by
    --   repositionVisuals at a small interline gap.
  , ttsSeparatorHandle ∷ Maybe ElementHandle
  , ttsLocked          ∷ Bool
    -- ^ When True, the tooltip is frozen in place: hover changes are
    --   ignored, dwell timing is paused, and the tooltip stays visible
    --   until something explicitly unlocks it. Animation frames still
    --   advance so animated sprites keep playing while locked.
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
  , ttsHintRemainingMs = 0
  , ttsSpriteHandles  = []
  , ttsBoxHandle      = Nothing
  , ttsTextHandle     = Nothing
  , ttsHintHandles    = []
  , ttsSeparatorHandle = Nothing
  , ttsLocked         = False
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
