module UI.TextBuffer
  ( -- * Character Operations
    insertChar
  , deleteBackward
  , deleteForward
    -- * Cursor Movement
  , cursorLeft
  , cursorRight
  , cursorHome
  , cursorEnd
    -- * Buffer Management
  , clearBuffer
  , submitBuffer
  ) where

import UPrelude
import qualified Data.Text as T
import UI.Focus (TextBuffer(..))

-- | Insert character at cursor position
insertChar ∷ Char → TextBuffer → TextBuffer
insertChar c buf =
  let (before, after) = T.splitAt (tbCursor buf) (tbContent buf)
      newContent = before <> T.singleton c <> after
  in buf { tbContent = newContent
         , tbCursor  = tbCursor buf + 1 }

-- | Delete character before cursor (Backspace)
deleteBackward ∷ TextBuffer → TextBuffer
deleteBackward buf
  | tbCursor buf ≤ 0 = buf
  | otherwise =
      let (before, after) = T.splitAt (tbCursor buf) (tbContent buf)
      in buf { tbContent = T.dropEnd 1 before <> after
             , tbCursor  = tbCursor buf - 1 }

-- | Delete character at cursor (Delete key)
deleteForward ∷ TextBuffer → TextBuffer
deleteForward buf =
  let (before, after) = T.splitAt (tbCursor buf) (tbContent buf)
  in buf { tbContent = before <> T.drop 1 after }

-- | Move cursor left
cursorLeft ∷ TextBuffer → TextBuffer
cursorLeft buf = buf { tbCursor = max 0 (tbCursor buf - 1) }

-- | Move cursor right
cursorRight ∷ TextBuffer → TextBuffer
cursorRight buf = buf { tbCursor = min (T.length $ tbContent buf) (tbCursor buf + 1) }

-- | Move cursor to start
cursorHome ∷ TextBuffer → TextBuffer
cursorHome buf = buf { tbCursor = 0 }

-- | Move cursor to end
cursorEnd ∷ TextBuffer → TextBuffer
cursorEnd buf = buf { tbCursor = T.length (tbContent buf) }

-- | Clear the buffer
clearBuffer ∷ TextBuffer → TextBuffer
clearBuffer buf = buf { tbContent = T.empty, tbCursor = 0 }

-- | Get content and clear (for submission)
submitBuffer ∷ TextBuffer → (T.Text, TextBuffer)
submitBuffer buf = (tbContent buf, clearBuffer buf)
