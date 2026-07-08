{-# LANGUAGE Strict, UnicodeSyntax #-}
module UI.Manager.Text
  ( enableTextInput
  , getTextBuffer
  , setTextBuffer
  , modifyTextBuffer
  ) where

import UPrelude
import qualified Data.Map.Strict as Map
import UI.Types
import UI.Manager.Core (modifyElement)

-- * Text Buffer Operations

-- | Enable text input on an element (initializes empty buffer)
enableTextInput ∷ ElementHandle → UIPageManager → UIPageManager
enableTextInput handle = modifyElement handle `flip` \elem →
    elem { ueTextBuffer = Just emptyBuffer }

-- | Get an element's text buffer
getTextBuffer ∷ ElementHandle → UIPageManager → Maybe TextBuffer
getTextBuffer handle mgr =
    case Map.lookup handle (upmElements mgr) of
        Nothing → Nothing
        Just elem → ueTextBuffer elem

-- | Set an element's text buffer
setTextBuffer ∷ ElementHandle → TextBuffer → UIPageManager → UIPageManager
setTextBuffer handle buffer = modifyElement handle `flip` \elem →
    elem { ueTextBuffer = Just buffer }

-- | Modify an element's text buffer with a function
modifyTextBuffer ∷ ElementHandle → (TextBuffer → TextBuffer) → UIPageManager → UIPageManager
modifyTextBuffer handle f = modifyElement handle `flip` \elem →
    case ueTextBuffer elem of
        Nothing → elem
        Just buf → elem { ueTextBuffer = Just (f buf) }
