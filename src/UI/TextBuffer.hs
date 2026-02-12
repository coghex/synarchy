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

-----------------------------------------------------------
-- Character Operations
-----------------------------------------------------------

insertChar ∷ Char → TextBuffer → TextBuffer
insertChar c buf =
  let (before, after) = T.splitAt (tbCursor buf) (tbContent buf)
      newContent = before <> T.singleton c <> after
  in buf { tbContent = newContent
         , tbCursor  = tbCursor buf + 1 }

deleteBackward ∷ TextBuffer → TextBuffer
deleteBackward buf
  | tbCursor buf ≤ 0 = buf
  | otherwise =
      let (before, after) = T.splitAt (tbCursor buf) (tbContent buf)
      in buf { tbContent = T.dropEnd 1 before <> after
             , tbCursor  = tbCursor buf - 1 }

deleteForward ∷ TextBuffer → TextBuffer
deleteForward buf =
  let (before, after) = T.splitAt (tbCursor buf) (tbContent buf)
  in buf { tbContent = before <> T.drop 1 after }

-----------------------------------------------------------
-- Cursor Movement
-----------------------------------------------------------

cursorLeft ∷ TextBuffer → TextBuffer
cursorLeft buf = buf { tbCursor = max 0 (tbCursor buf - 1) }

cursorRight ∷ TextBuffer → TextBuffer
cursorRight buf = buf { tbCursor = min (T.length $ tbContent buf) (tbCursor buf + 1) }

cursorHome ∷ TextBuffer → TextBuffer
cursorHome buf = buf { tbCursor = 0 }

cursorEnd ∷ TextBuffer → TextBuffer
cursorEnd buf = buf { tbCursor = T.length (tbContent buf) }

-----------------------------------------------------------
-- Buffer Management
-----------------------------------------------------------

clearBuffer ∷ TextBuffer → TextBuffer
clearBuffer buf = buf { tbContent = T.empty, tbCursor = 0 }

submitBuffer ∷ TextBuffer → (T.Text, TextBuffer)
submitBuffer buf = (tbContent buf, clearBuffer buf)
