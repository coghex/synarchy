module Engine.Core.Types
  ( BootProfile(..)
  , bootProfileTag
  , EngineConfig(..)
  , PreviewEntry(..)
  , PreviewFrameDir(..)
  , PreviewAnim(..)
  , PreviewUnit(..)
  , PreviewBuildingEntry(..)
  , PreviewBuilding(..)
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

-- | One browsable entry of a previewed building (#888): either a
--   recognized animation subdirectory (labeled by its directory name,
--   'pbeAnimated' 'True', frames in numeric order) or a loose static
--   PNG (labeled by its path relative to the building's own folder,
--   one frame). Both live in the SAME list — the building folder mixes
--   them, so the viewer's list does too.
data PreviewBuildingEntry = PreviewBuildingEntry
  { pbeLabel    ∷ !Text
  , pbeAnimated ∷ !Bool
  , pbeFps      ∷ !Float
    -- ^ From @data/buildings/\<name\>.yaml@ when the animation has a
    --   matching entry, else 'Engine.Preview.Building'\'s documented
    --   default (the SAME value @BuildingYamlAnim@ decodes to).
    --   Meaningless for a static entry, which never plays.
  , pbeLoop     ∷ !Bool
  , pbeFrames   ∷ ![Text]
    -- ^ Texture paths in numeric @frame_NNN.png@ order; exactly one for
    --   a static entry. Never empty.
  } deriving (Eq, Show)

-- | A resolved @--preview buildings/\<name\>@ target (#888): every
--   animation subdirectory and loose static texture the building's own
--   asset folder holds, ordered by label, plus the default selection.
data PreviewBuilding = PreviewBuilding
  { pbName    ∷ !Text
  , pbEntries ∷ ![PreviewBuildingEntry]
  , pbDefault ∷ !Text
    -- ^ The @state_animations.built@ animation where the building's own
    --   YAML defines a usable one, else its @sprite@, else
    --   @default.png@, else the first entry — empty only when the
    --   folder holds no browsable texture at all.
  } deriving (Eq, Show)

-- | Resolved browsing state, computed once in @Main@ before boot so the
--   discovery/containment logic ('Engine.Preview.Discovery',
--   'Engine.Preview.Unit', 'Engine.Preview.Building') never has to run
--   again from the Lua thread.
--   'PreviewList' backs a bare @--preview \<simple category\>@ (#886
--   Requirement 3) AND a @--preview flora\/\<name\>@ \/
--   @--preview structures\/\<name\>@ target, which #888 deliberately
--   routes into that same browser rooted at the item's own folder
--   rather than forking a viewer per category; 'PreviewItem' backs a
--   validated @--preview \<simple category\>/\<item\>@ (#886
--   Requirement 4); 'PreviewUnitAnims' backs a validated
--   @--preview units/\<name\>@ (#887); 'PreviewBuildingAssets' backs a
--   validated @--preview buildings/\<name\>@ (#888). Every canonical
--   category now resolves to one of these — outside 'BootPreview'
--   'ecPreviewBrowse' is simply 'Nothing'.
data PreviewBrowse
  = PreviewList ![PreviewEntry]
  | PreviewItem !PreviewEntry
  | PreviewUnitAnims !PreviewUnit
  | PreviewBuildingAssets !PreviewBuilding
  deriving (Eq, Show)

data EngineConfig = EngineConfig
  { ecHeadless      ∷ Bool
  , ecDebugPort     ∷ Int
  , ecBootProfile   ∷ BootProfile
  -- | (category, item) requested via @--preview category[/item]@.
  --   'Nothing' outside 'BootPreview'; item is 'Nothing' for a bare
  --   category (e.g. @--preview icons@).
  , ecPreviewTarget ∷ Maybe (Text, Maybe Text)
  -- | Resolved simple-category browsing state; see 'PreviewBrowse'.
  , ecPreviewBrowse ∷ Maybe PreviewBrowse
  } deriving (Eq, Show)
