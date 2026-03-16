{-# LANGUAGE Strict #-}
module Engine.Graphics.Vulkan.Types.Cleanup
  ( Cleanup(..)
  , emptyCleanup
  , runAllCleanups
  ) where

import UPrelude

-- | Cleanup actions for Vulkan resources
-- All fields are IO () actions that destroy the corresponding resource
data Cleanup = Cleanup
  { cleanupFontUI       ∷ IO ()
  , cleanupFont         ∷ IO ()
  , cleanupBindlessUI   ∷ IO ()
  , cleanupBindless     ∷ IO ()
  , cleanupFramebuffers ∷ IO ()
  , cleanupMSAAImage    ∷ IO ()
  , cleanupImageViews   ∷ IO ()
  , cleanupRenderPass   ∷ IO ()
  , cleanupSwapchain    ∷ IO ()
  }

-- | No-op cleanup (all fields are @pure ()@)
emptyCleanup ∷ Cleanup
emptyCleanup = Cleanup
  { cleanupFontUI       = pure ()
  , cleanupFont         = pure ()
  , cleanupBindlessUI   = pure ()
  , cleanupBindless     = pure ()
  , cleanupFramebuffers = pure ()
  , cleanupMSAAImage    = pure ()
  , cleanupImageViews   = pure ()
  , cleanupRenderPass   = pure ()
  , cleanupSwapchain    = pure ()
  }

-- | Run all cleanup actions
runAllCleanups ∷ Cleanup → IO ()
runAllCleanups Cleanup{..} = do
  cleanupFontUI
  cleanupFont
  cleanupBindlessUI
  cleanupBindless
  cleanupFramebuffers
  cleanupMSAAImage
  cleanupImageViews
  cleanupRenderPass
  cleanupSwapchain
