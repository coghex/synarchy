module Engine.Event.Base where
import UPrelude
-- Move basic event types here, without dependencies on Asset types
data SystemAction
  = SysRecreateWindow
  | SysReloadResources
  | SysToggleFullscreen
  | SysResizeWindow Int Int
  | SysExit
  deriving (Show, Eq)
