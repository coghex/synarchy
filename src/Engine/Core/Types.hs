module Engine.Core.Types
  ( BootProfile(..)
  , bootProfileTag
  , EngineConfig(..)
  , PreviewEntry(..)
  , PreviewBrowse(..)
  ) where

import UPrelude

data BootProfile
  = BootNormal
  | BootArena
  | BootPreview
  deriving (Eq, Show)

bootProfileTag ∷ BootProfile → Text
bootProfileTag BootNormal  = "normal"
bootProfileTag BootArena   = "arena"
bootProfileTag BootPreview = "preview"

-- | One discovered/resolved texture entry for the @--preview@ simple-
--   category browser (#886). 'peLabel' is the category-relative path
--   (forward-slash separated, extension included — the SAME string a
--   displayed entry can be supplied back as an @--preview cat/item@
--   target) shown in the list; 'pePath' is the actual loadable asset
--   path (@assets/textures/\<category\>/...@).
data PreviewEntry = PreviewEntry
  { peLabel ∷ !Text
  , pePath  ∷ !Text
  } deriving (Eq, Show)

-- | Resolved simple-category browsing state, computed once in @Main@
--   before boot so the discovery/containment logic
--   ('Engine.Preview.Discovery') never has to run again from the Lua
--   thread. 'PreviewList' backs a bare @--preview \<simple category\>@
--   (Requirement 3); 'PreviewItem' backs a validated
--   @--preview \<simple category\>/\<item\>@ (Requirement 4). Grouped
--   categories (and anything outside 'BootPreview') carry no
--   'PreviewBrowse' at all — 'ecPreviewBrowse' stays 'Nothing' and the
--   Phase 1 (#632) placeholder-label boot is unaffected.
data PreviewBrowse
  = PreviewList ![PreviewEntry]
  | PreviewItem !PreviewEntry
  deriving (Eq, Show)

data EngineConfig = EngineConfig
  { windowWidth     ∷ Int
  , windowHeight    ∷ Int
  , enableVSync     ∷ Bool
  , enableDebug     ∷ Bool
  , ecHeadless      ∷ Bool
  , ecDebugPort     ∷ Int
  , ecBootProfile   ∷ BootProfile
  -- | (category, item) requested via @--preview category[/item]@.
  --   'Nothing' outside 'BootPreview'; item is 'Nothing' for a bare
  --   category (e.g. @--preview icons@).
  , ecPreviewTarget ∷ Maybe (Text, Maybe Text)
  -- | Resolved simple-category browsing state; see 'PreviewBrowse'.
  , ecPreviewBrowse ∷ Maybe PreviewBrowse
  } deriving (Eq, Show)
