module Engine.Event.Base where
import UPrelude

data LogLevel = LogDebug Int | LogInfo | LogWarn | LogError
  deriving (Show, Eq, Ord)

data SystemAction
  = SysRecreateWindow
  | SysReloadResources
  | SysToggleFullscreen
  | SysResizeWindow Int Int
  | SysExit
  deriving (Show, Eq)
