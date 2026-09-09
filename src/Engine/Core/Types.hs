module Engine.Core.Types
  ( BootProfile(..)
  , bootProfileTag
  , BootMode(..)
  , bootModeName
  , EngineConfig(..)
  , PreviewEntry(..)
  , PreviewFrame(..)
  , PreviewFrameDir(..)
  , PreviewAnim(..)
  , PreviewUnit(..)
  , PreviewBuildingEntry(..)
  , PreviewFacingCell(..)
  , PreviewDeclaredEntry(..)
  , PreviewFsClass(..)
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
--   path, because every unit was on the per-frame path. An animation's
--   frames are now all the SAME compiled image with different
--   sub-rects, so a bare path can no longer name a frame: it would draw
--   the whole sheet. 'pfPath' is what to load, 'pfUV' is where the
--   frame lives inside it, and 'pfCell' is how big the frame is.
--
--   All three are unconditional since #1261 retired per-frame unit
--   animations. They used to be a mode signal — a frame with no
--   'pfCell' WAS a legacy frame — and leaving them optional would keep
--   a state nothing can produce and nothing can test. The buildings
--   viewer is unaffected: it never used this record, carrying its frames
--   as plain paths ('Engine.Preview.Building.pbeFrames').
data PreviewFrame = PreviewFrame
  { pfPath ∷ !Text
    -- ^ The texture to load: the animation's compiled atlas.
  , pfUV   ∷ !(Float, Float, Float, Float)
    -- ^ @(u0, v0, u1, v1)@ WITHIN that texture: the atlas cell, from
    --   'Unit.Atlas.Types.atlasCellUV' — the game's own frozen
    --   arithmetic, not a second copy.
  , pfCell ∷ !(Int, Int)
    -- ^ The frame's own pixel size, from the compiled index. NOT the
    --   sheet's: a consumer that measured the resident texture here
    --   would size every frame to the whole atlas.
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
--   unit's compiled index (#1260), which since #1261 is where EVERY
--   unit animation's frames come from.
data PreviewAnim = PreviewAnim
  { paName  ∷ !Text
    -- ^ The animation directory's exact name — also its list label.
  , paFps   ∷ !Float
  , paLoop  ∷ !Bool
  , paFlip  ∷ !Bool
    -- ^ Whether western directions may mirror their eastern
    --   counterparts, read from the compiled index — which
    --   'Unit.Atlas.Index.planUnitAtlasStorage' has already proved
    --   equal to the unit YAML's own @flip@.
  , paAtlas ∷ !Text
    -- ^ The compiled atlas this animation samples. Every frame in
    --   'paDirs' names this same path — it is surfaced separately so
    --   the viewer's introspection dump can state the storage outright
    --   rather than leaving a probe to infer it from a path shape.
    --   Unconditional since #1261: its absence used to mean "legacy
    --   per-frame", and there is no such animation any more.
  , paThumb ∷ !(Maybe PreviewFrame)
    -- ^ Frame-zero of the south direction — the list row's thumbnail.
    --   'Nothing' when the animation stores no south frames at all.
  , paDirs  ∷ ![PreviewFrameDir]
    -- ^ Available directions in the game's own @S, SW, W, NW, N, NE,
    --   E, SE@ order; unavailable ones are omitted.
  } deriving (Eq, Show)

-- | A resolved @--preview units/\<name\>@ target (#887): every
--   animation the unit's own YAML declares, ordered case-sensitively by
--   name, plus the default selection.
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

-- | One camera facing of a DECLARED matrix entry (#2492): that
--   facing's own complete ordered path list, and whether the viewer can
--   actually show it.
--
--   A cell is DIAGNOSTIC rather than displayable when any one of its
--   declared paths fails the building-preview asset boundary. It then
--   keeps its declared paths — they are what the diagnostic is ABOUT —
--   but the viewer must never request a texture for them, and must
--   never substitute a path from another facing, role, static sprite or
--   raw filesystem row in their place.
data PreviewFacingCell = PreviewFacingCell
  { pfcFacing        ∷ !Text
    -- ^ @"south"@ \/ @"west"@ \/ @"north"@ \/ @"east"@ — the camera
    --   order 'Building.Schema.canonicalFacings' fixes.
  , pfcPaths         ∷ ![Text]
    -- ^ The facing's complete ordered declared path list: one path for
    --   a static sprite, the whole frame list for an animation. Empty
    --   only for an UNRESOLVED lifecycle row, which declares an
    --   animation name the definition never defines.
  , pfcMissing       ∷ !Bool
  , pfcMissingReason ∷ !(Maybe Text)
    -- ^ Why this cell is diagnostic: @"absent"@, @"directory"@,
    --   @"symlink"@, @"special"@, @"unsupported_extension"@,
    --   @"outside_root"@ (a declared path that does not resolve under
    --   the building's own asset folder — the trimmed-loading rule) or
    --   @"unresolved"@ (the lifecycle row's animation reference itself).
    --   'Nothing' exactly when 'pfcMissing' is 'False'.
  , pfcLegacy        ∷ !Bool
    -- ^ This cell's content was REPLICATED from one pre-#2080
    --   declaration rather than authored for this facing. True for
    --   every cell of a legacy entry, so four repeated views can never
    --   read as four authored ones.
  } deriving (Eq, Show)

-- | One row of the DECLARED inspection matrix (#2492), read only from
--   @data\/buildings\/\<name\>.yaml@: a lifecycle role, or the
--   building's static sprite.
--
--   Declaration provenance is per-ENTRY, never per-building: a
--   definition may legitimately declare its sprite legacy and one
--   animation canonically, so each row reports the 'AssetSource' of its
--   OWN 'Building.Schema.FacingAssets'.
data PreviewDeclaredEntry = PreviewDeclaredEntry
  { pdeIdentity ∷ !Text
    -- ^ The row's stable selection identity: @"lifecycle:\<role\>"@ or
    --   @"sprite"@. Distinct from every @"filesystem:\<label\>"@, so a
    --   lifecycle row and the raw row backing the same files can never
    --   alias each other.
  , pdeKind     ∷ !Text
    -- ^ @"lifecycle"@ or @"sprite"@.
  , pdeLabel    ∷ !Text
    -- ^ The row's display label.
  , pdeRole     ∷ !(Maybe Text)
    -- ^ The lifecycle role key; 'Nothing' for the sprite row.
  , pdeAnimName ∷ !(Maybe Text)
    -- ^ The animation name @state_animations@ referenced; 'Nothing' for
    --   the sprite row.
  , pdeResolved ∷ !Bool
    -- ^ Whether 'pdeAnimName' resolved to an @animations@ entry. A
    --   'False' lifecycle row is RETAINED as a diagnostic — never
    --   silently reclassified as undeclared, and never filled from
    --   another animation.
  , pdeFps      ∷ !Float
  , pdeLoop     ∷ !Bool
  , pdeSource   ∷ !Text
    -- ^ @"canonical"@ or @"legacy"@ — this entry's own provenance.
  , pdeLegacy   ∷ !Bool
  , pdeCells    ∷ ![PreviewFacingCell]
    -- ^ Exactly four cells, in camera order south, west, north, east.
  , pdeProjected ∷ !(Maybe Text)
    -- ^ The raw filesystem label this row projects onto for the
    --   compatibility @selected@ field, resolved by the pre-#2492
    --   matching rules and deliberately FACING-INDEPENDENT. 'Nothing'
    --   when no raw entry overlaps this declaration.
  } deriving (Eq, Show)

-- | How one RAW filesystem row relates to the declared matrix (#2492).
--   Classification only: no raw entry is ever filtered, reordered or
--   relabelled by it.
data PreviewFsClass = PreviewFsClass
  { pfcsLabel      ∷ !Text
    -- ^ The raw entry's own label, unchanged.
  , pfcsIdentity   ∷ !Text
    -- ^ @"filesystem:\<label\>"@.
  , pfcsDeclared   ∷ ![Text]
    -- ^ Identities of the declared rows whose paths this row's frames
    --   overlap, in declared order. Empty exactly when 'pfcsUndeclared'.
  , pfcsUndeclared ∷ !Bool
  } deriving (Eq, Show)

-- | A resolved @--preview buildings/\<name\>@ target (#888): every
--   animation subdirectory and loose static texture the building's own
--   asset folder holds, ordered by label, plus the default selection.
--
--   #2492 layers a DECLARED lifecycle\/facing matrix over that browser.
--   The two authorities stay separate: 'pbEntries' is still exactly
--   what the filesystem holds, in exactly its existing order, and
--   'pbDeclared' is read only from the building's own YAML. Rows from
--   the two have distinct selection identities even when they display
--   the same files, which is why duplicate-looking rows are expected
--   rather than a bug.
data PreviewBuilding = PreviewBuilding
  { pbName    ∷ !Text
  , pbEntries ∷ ![PreviewBuildingEntry]
  , pbDefault ∷ !Text
    -- ^ The @state_animations.built@ animation where the building's own
    --   YAML defines a usable one, else its @sprite@, else
    --   @default.png@, else the first entry — empty only when the
    --   folder holds no browsable texture at all.
    --
    --   Deliberately unchanged by #2492: it names a RAW entry, and the
    --   probe and the existing fixtures read it as one.
  , pbDeclared ∷ ![PreviewDeclaredEntry]
    -- ^ The declared rows, in the fixed order @construction@,
    --   @appearance@, @built@, @destruction@, then the static sprite.
    --   Empty for a building whose YAML is missing, unreadable,
    --   malformed, matches no definition, or cannot yield a usable
    --   declared matrix — every one of which preserves the complete raw
    --   browser and its existing default behavior.
  , pbFsClasses ∷ ![PreviewFsClass]
    -- ^ One entry per 'pbEntries' row, in the same order.
  , pbDefaultSelection ∷ !Text
    -- ^ The stable identity of the row selected initially: the declared
    --   @built@ row, else the declared sprite row, else
    --   @"filesystem:\<pbDefault\>"@, else empty for an empty browser.
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
