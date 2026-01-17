module Engine.Scripting.Types where

import UPrelude
import qualified Data.Text as T
import Data.Dynamic (Dynamic)

-- | Abstract script value (can hold any script type)
data ScriptValue
  = ScriptNumber Double
  | ScriptString T.Text
  | ScriptBool Bool
  | ScriptNil
  | ScriptTable [(ScriptValue, ScriptValue)]
  | ScriptFunction Dynamic
  deriving (Show)

-- | Script execution result
data ScriptResult
  = ScriptSuccess [ScriptValue]
  | ScriptError T.Text
  deriving (Show)

-- | Script context (opaque handle to interpreter state)
newtype ScriptContext = ScriptContext Dynamic
  deriving (Show)
