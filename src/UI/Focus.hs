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
  , unregisterFocusTarget
  , setFocus
  , clearFocus
  , focusNext
  , getFocusTarget
  , modifyFocusTargetBuffer
  ) where

import UPrelude
import Data.List (sortOn)
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import UI.Types (TextBuffer(..), emptyBuffer)

-- | Unique identifier for focusable elements
newtype FocusId = FocusId { unFocusId ∷ Word32 }
  deriving (Eq, Ord, Show)

-- | A focusable UI element
data FocusTarget = FocusTarget
  { ftId          ∷ FocusId   -- ^ Unique identifier
  , ftBuffer      ∷ TextBuffer -- ^ Text buffer for this element
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
        , ftBuffer      = emptyBuffer
        , ftAcceptsText = acceptsText
        , ftTabIndex    = tabIndex
        }
  in ( fid
     , fm { fmTargets = Map.insert fid target (fmTargets fm)
          , fmNextId  = fmNextId fm + 1 }
     )

-- | Remove a focusable element
unregisterFocusTarget ∷ FocusId → FocusManager → FocusManager
unregisterFocusTarget fid fm = fm
  { fmTargets      = Map.delete fid (fmTargets fm)
  , fmCurrentFocus = if fmCurrentFocus fm ≡ Just fid
                     then Nothing
                     else fmCurrentFocus fm
  }

-- | Set focus to a target
setFocus ∷ FocusId → FocusManager → FocusManager
setFocus fid fm = fm { fmCurrentFocus = Just fid }

-- | Clear focus (return to game input mode)
clearFocus ∷ FocusManager → FocusManager
clearFocus fm = fm { fmCurrentFocus = Nothing }

-- | Navigate to next focusable element (Tab)
focusNext ∷ FocusManager → FocusManager
focusNext fm =
  let textTargets = sortOn ftTabIndex
                  $ filter ftAcceptsText
                  $ Map.elems (fmTargets fm)
      nextFocus = case fmCurrentFocus fm of
        Nothing → ftId <$> listToMaybe textTargets
        Just current →
          let after = dropWhile (\t → ftId t ≢ current) textTargets
          in case drop 1 after of
               (next:_) → Just (ftId next)
               []       → ftId <$> listToMaybe textTargets  -- wrap around
  in fm { fmCurrentFocus = nextFocus }

-- | Get a focus target by ID
getFocusTarget ∷ FocusId → FocusManager → Maybe FocusTarget
getFocusTarget fid fm = Map.lookup fid (fmTargets fm)

-- | Modify the buffer of a focus target
modifyFocusTargetBuffer ∷ FocusId → (TextBuffer → TextBuffer) → FocusManager → FocusManager
modifyFocusTargetBuffer fid f fm = fm
  { fmTargets = Map.adjust (\t → t { ftBuffer = f (ftBuffer t) }) fid (fmTargets fm) }
