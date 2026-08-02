module Engine.Scripting.Types where

import UPrelude
import qualified Data.Text as T

data ScriptValue
  = ScriptNumber Double
  | ScriptString T.Text
  | ScriptBool Bool
  | ScriptNil
  | ScriptTable [(ScriptValue, ScriptValue)]
  deriving (Show)
