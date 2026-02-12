module Engine.Graphics.Base where

import UPrelude

-----------------------------------------------------------
-- Basic Graphics Configuration
-----------------------------------------------------------

data GraphicsConfig = GraphicsConfig
  { gcAppName    ∷ Text   -- ^ Application name for Vulkan
  , gcWidth      ∷ Int    -- ^ Initial window width
  , gcHeight     ∷ Int    -- ^ Initial window height
  , gcDebugMode  ∷ Bool   -- ^ Enable Vulkan validation layers
  , gcMaxFrames  ∷ Int    -- ^ Maximum frames in flight
  }

