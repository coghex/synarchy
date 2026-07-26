module Engine.Core.Types
  ( BootProfile(..)
  , bootProfileTag
  , EngineConfig(..)
  , PreviewEntry(..)
  , PreviewFrameDir(..)
  , PreviewAnim(..)
  , PreviewUnit(..)
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

-- | One displayable direction cell of a previewed unit animation (#887,
--   Phase 3). Directions are the LONG folder-name spellings
--   (@"south"@, @"south-west"@, …) so the Lua viewer and the
--   introspection dump speak the same vocabulary as the on-disk asset
--   layout. 'pfdSource' is the direction the frames were actually
--   authored under: equal to 'pfdDirection' for a directly-authored
--   direction, and its eastern counterpart when 'pfdMirrored' is
--   'True' (the W/SW/NW mirror fallback, matching the game's own
--   'Unit.Render.pickFrame' convention).
data PreviewFrameDir = PreviewFrameDir
  { pfdDirection ∷ !Text
  , pfdSource    ∷ !Text
  , pfdMirrored  ∷ !Bool
  , pfdFrames    ∷ ![Text]
    -- ^ Frame texture paths in numeric @frame_NNN.png@ order. Never
    --   empty — a direction with no frames is omitted entirely rather
    --   than listed as an empty cell.
  } deriving (Eq, Show)

-- | One animation of a previewed unit: the filesystem-derived frame
--   sets plus the playback metadata @data/units/\<name\>.yaml@
--   contributed (or the documented defaults when it didn't).
data PreviewAnim = PreviewAnim
  { paName  ∷ !Text
    -- ^ The animation directory's exact name — also its list label.
  , paFps   ∷ !Float
  , paLoop  ∷ !Bool
  , paFlip  ∷ !Bool
    -- ^ Whether western directions may mirror their eastern
    --   counterparts. From YAML when the animation has an entry;
    --   otherwise inferred from the stored direction set.
  , paThumb ∷ !Text
    -- ^ Frame-zero of the south direction — the list row's thumbnail.
    --   Empty when the animation stores no south frames at all.
  , paDirs  ∷ ![PreviewFrameDir]
    -- ^ Available directions in the game's own @S, SW, W, NW, N, NE,
    --   E, SE@ order; unavailable ones are omitted.
  } deriving (Eq, Show)

-- | A resolved @--preview units/\<name\>@ target (#887): every
--   animation the unit's own asset tree holds, ordered
--   case-sensitively by directory name, plus the default selection.
data PreviewUnit = PreviewUnit
  { puName    ∷ !Text
  , puAnims   ∷ ![PreviewAnim]
  , puDefault ∷ !Text
    -- ^ @"idle"@ when present, else the first animation in 'puAnims'
    --   order; empty only when the unit has no animations at all.
  } deriving (Eq, Show)

-- | Resolved browsing state, computed once in @Main@ before boot so the
--   discovery/containment logic ('Engine.Preview.Discovery',
--   'Engine.Preview.Unit') never has to run again from the Lua thread.
--   'PreviewList' backs a bare @--preview \<simple category\>@ (#886
--   Requirement 3); 'PreviewItem' backs a validated
--   @--preview \<simple category\>/\<item\>@ (#886 Requirement 4);
--   'PreviewUnitAnims' backs a validated @--preview units/\<name\>@
--   (#887). The remaining grouped categories (and anything outside
--   'BootPreview') carry no 'PreviewBrowse' at all —
--   'ecPreviewBrowse' stays 'Nothing' and the Phase 1 (#632)
--   placeholder-label boot is unaffected.
data PreviewBrowse
  = PreviewList ![PreviewEntry]
  | PreviewItem !PreviewEntry
  | PreviewUnitAnims !PreviewUnit
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
