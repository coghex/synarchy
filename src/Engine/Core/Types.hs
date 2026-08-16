module Engine.Core.Types
  ( BootProfile(..)
  , bootProfileTag
  , BootMode(..)
  , bootModeName
  , EngineConfig(..)
  , PreviewEntry(..)
  , PreviewFrame(..)
  , wholeImagePreviewFrame
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

-- | The boot mode @app\/Main.hs@ selected from argv.
--
--   Deliberately a type of its own rather than a reading of something
--   that already existed (#1190): 'BootProfile' names the world
--   topology a mode boots with (normal\/arena\/preview), not the mode,
--   and 'ecHeadless' cannot tell @--dump@ from @--headless@ — it is
--   'True' for both and 'False' for @--offscreen@. The debug-console
--   listener policy needs all five distinguished, because whether a
--   dead listener is fatal is exactly a per-mode question:
--   'Engine.Scripting.Lua.DebugServer.debugConsolePolicy'.
--
--   @--language-report@ is deliberately absent: it boots no engine and
--   starts no Lua thread, so it never reaches a listener policy at all.
data BootMode
  = ModeDump
  | ModeHeadless
  | ModeOffscreen
  | ModeGraphical
  | ModePreview
  deriving (Eq, Show, Enum, Bounded)

-- | The mode's name in a diagnostic — the SAME vocabulary
--   @app\/Main.hs@'s incompatible-flag rejections already print
--   (@"... is not supported in headless mode"@).
bootModeName ∷ BootMode → Text
bootModeName ModeDump      = "dump"
bootModeName ModeHeadless  = "headless"
bootModeName ModeOffscreen = "offscreen"
bootModeName ModeGraphical = "graphical"
bootModeName ModePreview   = "preview"

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

-- | ONE displayable frame of a previewed unit animation — the viewer's
--   storage-neutral counterpart to 'Unit.Atlas.Types.FrameSample'
--   (#1260, D-9), and deliberately the same three fields.
--
--   Before the acolyte pilot a preview frame was just a source PNG
--   path, because every unit was on the legacy per-frame path. Now an
--   ATLAS-backed animation's frames are all the SAME compiled image
--   with different sub-rects, so a bare path can no longer name a
--   frame: it would draw the whole sheet. 'pfPath' is what to load,
--   'pfUV' is where the frame lives inside it, and 'pfCell' is how big
--   the frame is when the storage knows (a legacy frame's image IS the
--   frame, so its consumer measures the texture as it always has).
data PreviewFrame = PreviewFrame
  { pfPath ∷ !Text
    -- ^ The texture to load: the animation's compiled atlas, or the
    --   source frame's own PNG on the legacy path.
  , pfUV   ∷ !(Float, Float, Float, Float)
    -- ^ @(u0, v0, u1, v1)@ WITHIN that texture. The atlas cell
    --   ('Unit.Atlas.Types.atlasCellUV' — the game's own frozen
    --   arithmetic, not a second copy), or the whole image.
  , pfCell ∷ !(Maybe (Int, Int))
    -- ^ The frame's own pixel size, from the compiled index. 'Nothing'
    --   on the legacy path.
  } deriving (Eq, Show)

-- | A legacy frame: a whole image of its own, with no known cell size.
wholeImagePreviewFrame ∷ Text → PreviewFrame
wholeImagePreviewFrame path = PreviewFrame
  { pfPath = path, pfUV = (0, 0, 1, 1), pfCell = Nothing }

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
  , pfdFrames    ∷ ![PreviewFrame]
    -- ^ Frames in playback order. Never empty — a direction with no
    --   frames is omitted entirely rather than listed as an empty cell.
  } deriving (Eq, Show)

-- | One animation of a previewed unit.
--
--   MEMBERSHIP is still the filesystem's ('Engine.Preview.Unit'
--   discovers the animation directories). What each animation IS —
--   which directions it has, how many frames each holds, its
--   @fps@\/@loop@\/@flip@, and the pixels themselves — comes from the
--   unit's compiled index when the animation is atlas-backed (#1260),
--   and from the source frames plus @data\/units\/\<name\>.yaml@
--   otherwise. The two never mix within one animation.
data PreviewAnim = PreviewAnim
  { paName  ∷ !Text
    -- ^ The animation directory's exact name — also its list label.
  , paFps   ∷ !Float
  , paLoop  ∷ !Bool
  , paFlip  ∷ !Bool
    -- ^ Whether western directions may mirror their eastern
    --   counterparts. From the compiled index for an atlas-backed
    --   animation, else from YAML when it has an entry, else inferred
    --   from the stored direction set.
  , paAtlas ∷ !(Maybe Text)
    -- ^ The compiled atlas this animation samples, when it is
    --   atlas-backed; 'Nothing' on the legacy per-frame path. Every
    --   frame in 'paDirs' names this same path — it is surfaced
    --   separately so the viewer's introspection dump can state the
    --   storage mode outright rather than leaving a probe to infer it
    --   from a path shape.
  , paThumb ∷ !(Maybe PreviewFrame)
    -- ^ Frame-zero of the south direction — the list row's thumbnail.
    --   'Nothing' when the animation stores no south frames at all.
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
  -- | Which boot mode argv selected (#1190). Every boot path stamps its
  --   own through @App.Boot.bootConfig@\/@previewBootConfig@; see
  --   'BootMode' for why this is not derivable from the two fields
  --   above it.
  , ecBootMode      ∷ BootMode
  -- | (category, item) requested via @--preview category[/item]@.
  --   'Nothing' outside 'BootPreview'; item is 'Nothing' for a bare
  --   category (e.g. @--preview icons@).
  , ecPreviewTarget ∷ Maybe (Text, Maybe Text)
  -- | Resolved simple-category browsing state; see 'PreviewBrowse'.
  , ecPreviewBrowse ∷ Maybe PreviewBrowse
  } deriving (Eq, Show)
