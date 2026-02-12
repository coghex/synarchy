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

-----------------------------------------------------------
-- Core Types
-----------------------------------------------------------

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

-----------------------------------------------------------
-- Constructors
-----------------------------------------------------------

createFocusManager ∷ FocusManager
createFocusManager = FocusManager
  { fmCurrentFocus = Nothing
  , fmTargets      = Map.empty
  , fmNextId       = 1
  }

-----------------------------------------------------------
-- Focus Operations
-----------------------------------------------------------

getInputMode ∷ FocusManager → InputMode
getInputMode fm = case fmCurrentFocus fm of
  Nothing  → GameInputMode
  Just fid → case Map.lookup fid (fmTargets fm) of
    Just target | ftAcceptsText target → TextInputMode fid
    _ → GameInputMode

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

clearFocus ∷ FocusManager → FocusManager
clearFocus fm = fm { fmCurrentFocus = Nothing }

focusNext ∷ FocusManager → FocusManager
focusNext fm =
  let textTargets = sortOn ftTabIndex
                  $ filter ftAcceptsText
                  $ Map.elems (fmTargets fm)
      nextFocus = case fmCurrentFocus fm of
        Nothing → ftId ⊚ listToMaybe textTargets
        Just current →
          let after = dropWhile (\t → ftId t ≢ current) textTargets
          in case drop 1 after of
               (next:_) → Just (ftId next)
               []       → ftId ⊚ listToMaybe textTargets
  in fm { fmCurrentFocus = nextFocus }

getFocusTarget ∷ FocusId → FocusManager → Maybe FocusTarget
getFocusTarget fid fm = Map.lookup fid (fmTargets fm)

modifyFocusTargetBuffer ∷ FocusId → (TextBuffer → TextBuffer) → FocusManager → FocusManager
modifyFocusTargetBuffer fid f fm = fm
  { fmTargets = Map.adjust (\t → t { ftBuffer = f (ftBuffer t) }) fid (fmTargets fm) }
