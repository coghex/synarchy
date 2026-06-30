module UI.Focus
  ( -- * Core Types
    FocusId(..)
  , TextBuffer(..)
  , FocusTarget(..)
  , FocusManager(..)
  , InputMode(..)
    -- * Constructors
  , emptyBuffer
  , createFocusManager
  -- * focus operation
  , getInputMode
  , registerFocusTarget
  , setFocus
  , clearFocus
  ) where

import UPrelude
import qualified Data.Map.Strict as Map
import UI.Types (TextBuffer(..), emptyBuffer)

-- | Unique identifier for focusable elements
newtype FocusId = FocusId { unFocusId ∷ Word32 }
  deriving (Eq, Ord, Show)

-- | A focusable UI element
data FocusTarget = FocusTarget
  { ftId          ∷ FocusId   -- ^ Unique identifier
  , ftAcceptsText ∷ Bool       -- ^ Does this target accept text input?
  , ftTabIndex    ∷ Int        -- ^ Order for Tab navigation
  } deriving (Show)

-- | The focus manager tracks all focusable elements
data FocusManager = FocusManager
  { fmCurrentFocus ∷ Maybe FocusId              -- ^ Currently focused element
  , fmTargets      ∷ Map.Map FocusId FocusTarget -- ^ All registered targets
  , fmNextId       ∷ Word32                      -- ^ Next ID to assign
  } deriving (Show)

-- | Input mode derived from focus state
data InputMode
  = GameInputMode           -- ^ No text field focused, keys go to game
  | TextInputMode FocusId   -- ^ This text field has focus
  deriving (Show, Eq)

-- | Create empty focus manager
createFocusManager ∷ FocusManager
createFocusManager = FocusManager
  { fmCurrentFocus = Nothing
  , fmTargets      = Map.empty
  , fmNextId       = 1
  }

-- | Get current input mode from focus manager
getInputMode ∷ FocusManager → InputMode
getInputMode fm = case fmCurrentFocus fm of
  Nothing  → GameInputMode
  Just fid → case Map.lookup fid (fmTargets fm) of
    Just target | ftAcceptsText target → TextInputMode fid
    _ → GameInputMode

-- | Register a new focusable element, returns its ID and updated manager
registerFocusTarget ∷ Bool → Int → FocusManager → (FocusId, FocusManager)
registerFocusTarget acceptsText tabIndex fm =
  let fid = FocusId (fmNextId fm)
      target = FocusTarget
        { ftId          = fid
        , ftAcceptsText = acceptsText
        , ftTabIndex    = tabIndex
        }
  in ( fid
     , fm { fmTargets = Map.insert fid target (fmTargets fm)
          , fmNextId  = fmNextId fm + 1 }
     )

-- | Set focus to a target
setFocus ∷ FocusId → FocusManager → FocusManager
setFocus fid fm = fm { fmCurrentFocus = Just fid }

-- | Clear focus (return to game input mode)
clearFocus ∷ FocusManager → FocusManager
clearFocus fm = fm { fmCurrentFocus = Nothing }
