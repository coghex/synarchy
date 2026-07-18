{-# LANGUAGE Strict, UnicodeSyntax, DeriveGeneric, DeriveAnyClass, DerivingStrategies #-}
-- | Global, once-per-session components (issue #760, save-overhaul B2).
--   These carry the genuinely session-wide 'SessionSnapshot' fields —
--   the ones that are NOT scoped to any single world page:
--
--   - @"core-session"@ (required) — game time, the active + visible page
--     references, the single live render camera, and the three GLOBAL
--     identity allocators (item / building / unit). Owner: the session
--     itself. Boundary reason: these are the relationships and counters
--     every page shares; the allocators live here so they sit ABOVE every
--     governed id in exactly one place (requirement 9), not duplicated
--     per page the way the legacy 'WorldPageSave' embedded them.
--     Depends on @"world-pages"@ so its active/visible page refs resolve
--     against the authoritative page set (requirement 8).
--   - @"texture-palette"@ (required) — the path↔id texture palette.
--     Owner: the renderer's structure/edit layer. Boundary reason: it is
--     persistent reference data that genuinely CANNOT be rebuilt from
--     content definitions (requirement 2 bullet 10) — structure edits
--     store palette ids that only this table resolves back to paths.
--   - @"lua-state"@ (required, transitional) — the opaque per-module Lua
--     blob map, carried verbatim until B3 replaces its internal contract
--     (requirement 2 bullet 11). Owner: the Lua save-module registry.
module World.Save.Component.Session
    ( coreSessionCodec
    , texPaletteCodec
    , luaStateCodec
    , CoreSessionDTO(..)
    , LiveCameraDTO(..)
    , TexPaletteDTO(..)
    , LuaStateDTO(..)
    , applyCoreSession
    , toTexPaletteDTO
    , fromTexPaletteDTO
    , coreSessionTexPalette
    , coreSessionLua
    ) where

import UPrelude
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import Data.Serialize (Serialize)
import GHC.Generics (Generic)
import Structure.Palette (TexPalette(..))
import World.Page.Types (WorldPageId)
import Engine.Graphics.Camera (CameraFacing)
import World.Save.Snapshot
    ( SessionSnapshot(..), LiveCameraSnapshot(..) )
import World.Save.Component.Types

tshow ∷ Show a ⇒ a → Text
tshow = T.pack . show

-- core-session ------------------------------------------------------

-- | The live render camera, represented exactly once (requirement 5).
data LiveCameraDTO = LiveCameraDTO
    { lcdOwner  ∷ !(Maybe WorldPageId)
    , lcdX      ∷ !Float
    , lcdY      ∷ !Float
    , lcdZoom   ∷ !Float
    , lcdFacing ∷ !CameraFacing
    } deriving (Show, Eq, Generic, Serialize)

-- | Frozen v1 DTO for the @"core-session"@ component.
data CoreSessionDTO = CoreSessionDTO
    { csGameTime       ∷ !Double
    , csNextItemId     ∷ !Word64
    , csNextBuildingId ∷ !Word32
    , csNextUnitId     ∷ !Word32
    , csActivePage     ∷ !WorldPageId
    , csVisiblePages   ∷ ![WorldPageId]
    , csLiveCamera     ∷ !LiveCameraDTO
    } deriving (Show, Eq, Generic, Serialize)

coreSessionCodec ∷ ComponentCodec CoreSessionDTO
coreSessionCodec = serializeCodec
    coreSessionComponentId 1 True [worldPagesComponentId]
    encodeCore (\_ d → Right d) (const [])
  where
    encodeCore snap = CoreSessionDTO
        { csGameTime       = snapGameTime snap
        , csNextItemId     = snapNextItemId snap
        , csNextBuildingId = snapNextBuildingId snap
        , csNextUnitId     = snapNextUnitId snap
        , csActivePage     = snapActivePage snap
        , csVisiblePages   = snapVisiblePages snap
        , csLiveCamera     = toCameraDTO (snapLiveCamera snap)
        }
    toCameraDTO c = LiveCameraDTO
        { lcdOwner  = lcsOwnerPage c
        , lcdX      = lcsX c
        , lcdY      = lcsY c
        , lcdZoom   = lcsZoom c
        , lcdFacing = lcsFacing c
        }

-- | Fold the decoded core-session globals onto a snapshot skeleton
--   (assembly). The @snap@ already carries the assembled pages; this
--   writes the global scalars + camera.
applyCoreSession ∷ CoreSessionDTO → SessionSnapshot → SessionSnapshot
applyCoreSession d snap = snap
    { snapGameTime       = csGameTime d
    , snapNextItemId     = csNextItemId d
    , snapNextBuildingId = csNextBuildingId d
    , snapNextUnitId     = csNextUnitId d
    , snapActivePage     = csActivePage d
    , snapVisiblePages   = csVisiblePages d
    , snapLiveCamera     = LiveCameraSnapshot
        { lcsOwnerPage = lcdOwner (csLiveCamera d)
        , lcsX         = lcdX (csLiveCamera d)
        , lcsY         = lcdY (csLiveCamera d)
        , lcsZoom      = lcdZoom (csLiveCamera d)
        , lcsFacing    = lcdFacing (csLiveCamera d)
        }
    }

-- texture-palette ---------------------------------------------------

-- | Frozen mirror of 'TexPalette' (#760 round 8). The previous version
--   rode directly on 'TexPalette''s own hand-written 'Serialize'
--   instance ("Structure.Palette") via a @deriving newtype@ — exactly
--   the live-manager-identity case the frozen-DTO boundary rule (see
--   "World.Save.Component.Types") forbids: 'TexPalette' is mutated in
--   place by the renderer's structure/edit layer as new textures are
--   interned, so a change to ITS instance (not just its record shape)
--   could silently drift this component's bytes with no version bump
--   here to notice. This DTO instead mirrors exactly what that instance
--   already persists — the next id, then the (path, id) pairs, in that
--   order — via an explicit conversion; the inverse id→path map is
--   rebuilt on load exactly as 'TexPalette''s own @get@ already does.
--   Field order matches the live instance's @put@ exactly, so the
--   derived cereal layout is byte-identical to the previous embedding
--   (the frozen tracked fixture stays valid).
data TexPaletteDTO = TexPaletteDTO
    { tpdNextId ∷ !Int
    , tpdPairs  ∷ ![(Text, Int)]
    } deriving (Show, Eq, Generic, Serialize)

toTexPaletteDTO ∷ TexPalette → TexPaletteDTO
toTexPaletteDTO tp =
    TexPaletteDTO (tpNextId tp) (HM.toList (tpPathToId tp))

fromTexPaletteDTO ∷ TexPaletteDTO → TexPalette
fromTexPaletteDTO d =
    let pairs = tpdPairs d
    in TexPalette
        { tpPathToId = HM.fromList pairs
        , tpIdToPath = HM.fromList [ (i, p) | (p, i) ← pairs ]
        , tpNextId   = tpdNextId d
        }

-- | Component-local invariant (#760 round 9): 'TexPalette' is a
--   bijective path<->id map plus a "next id to hand out" allocator —
--   riding on the live instance's own hand-written 'put'/'get'
--   (see "Structure.Palette") used to guarantee this structurally
--   (round-tripping through the SAME in-memory maps the live code
--   maintains by construction), but the frozen DTO decodes an arbitrary
--   @(path, id)@ pair list off disk with no such guarantee. Reject:
--   a duplicate path (two ids claiming the same texture), a duplicate id
--   (two paths claiming the same slot — non-bijective, so
--   'fromTexPaletteDTO's @tpIdToPath@ rebuild would silently drop one),
--   and any id at or above the palette's own 'tpdNextId' allocator
--   (mirrors 'validateCraftBills'/'validatePowerNodes''s allocator
--   check).
validateTexPalette ∷ TexPaletteDTO → [ComponentError]
validateTexPalette (TexPaletteDTO nextId pairs) = concat
    [ [ ComponentError texPaletteComponentId 1 ValidatePhase
          ("duplicate texture palette path " <> tshow path)
      | (path, n) ← HM.toList
            (HM.fromListWith (+) [ (p, 1 ∷ Int) | (p, _) ← pairs ])
      , n > 1
      ]
    , [ ComponentError texPaletteComponentId 1 ValidatePhase
          ("duplicate texture palette id #" <> tshow pid)
      | (pid, n) ← HM.toList
            (HM.fromListWith (+) [ (i, 1 ∷ Int) | (_, i) ← pairs ])
      , n > 1
      ]
    , [ ComponentError texPaletteComponentId 1 ValidatePhase
          ("texture palette id #" <> tshow pid <> " is not below the \
           \palette's own allocator (" <> tshow nextId <> ")")
      | (_, pid) ← pairs, pid ≥ nextId
      ]
    ]

texPaletteCodec ∷ ComponentCodec TexPaletteDTO
texPaletteCodec = serializeCodec
    texPaletteComponentId 1 True []
    (\snap → toTexPaletteDTO (snapTexPalette snap)) (\_ d → Right d)
    validateTexPalette

coreSessionTexPalette ∷ TexPaletteDTO → SessionSnapshot → SessionSnapshot
coreSessionTexPalette d snap = snap { snapTexPalette = fromTexPaletteDTO d }

-- lua-state ---------------------------------------------------------

newtype LuaStateDTO = LuaStateDTO { lsdModules ∷ HM.HashMap Text Text }
    deriving stock (Generic)
    deriving newtype (Show, Eq, Serialize)

luaStateCodec ∷ ComponentCodec LuaStateDTO
luaStateCodec = serializeCodec
    luaStateComponentId 1 True []
    (\snap → LuaStateDTO (snapLuaModules snap)) (\_ d → Right d) (const [])

coreSessionLua ∷ LuaStateDTO → SessionSnapshot → SessionSnapshot
coreSessionLua d snap = snap { snapLuaModules = lsdModules d }
