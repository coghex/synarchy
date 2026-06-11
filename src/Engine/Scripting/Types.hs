module Engine.Scripting.Types where

import UPrelude
import qualified Data.Text as T
import Data.Dynamic (Dynamic)

data ScriptValue
  = ScriptNumber Double
  | ScriptString T.Text
  | ScriptBool Bool
  | ScriptNil
  | ScriptTable [(ScriptValue, ScriptValue)]
  | ScriptFunction Dynamic
  deriving (Show)
