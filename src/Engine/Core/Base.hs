module Engine.Core.Base where

import UPrelude
import Engine.Core.Var
import qualified Control.Monad.Logger.CallStack as Logger


type LoggingFunc = Logger.Loc → Logger.LogSource → Logger.LogLevel
                              → Logger.LogStr → IO ()
