-- | Pure preview-window policy. The real preview browser normally opens
--   exactly like a graphical game window. Automated preview probes can opt
--   into a hidden, non-activating GLFW window while retaining the real
--   surface/swapchain/resize path that an offscreen boot would bypass.
module App.Preview.Config
  ( previewHiddenWindowEnvVar
  , previewWindowConfig
  ) where

import UPrelude
import Engine.Core.Defaults (defaultWindowConfig)
import Engine.Graphics.Config (VideoConfig)
import Engine.Graphics.Window.Types (WindowConfig(..))

previewHiddenWindowEnvVar ∷ String
previewHiddenWindowEnvVar = "SYNARCHY_PREVIEW_HIDDEN"

-- | Presence of 'previewHiddenWindowEnvVar' selects the automated-probe
--   presentation. Its value is deliberately irrelevant, matching the
--   project's presence-based @SYNARCHY_FULL_TESTS@ convention.
previewWindowConfig ∷ Bool → VideoConfig → WindowConfig
previewWindowConfig hidden videoConfig =
  let config = defaultWindowConfig videoConfig
  in if hidden
       then config
         { wcFullscreen = False
         , wcVisible = False
         , wcFocused = False
         }
       else config
