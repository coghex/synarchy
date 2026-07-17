{-# LANGUAGE Strict, UnicodeSyntax, DeriveGeneric, DeriveAnyClass #-}
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
    , coreSessionTexPalette
    , coreSessionLua
    ) where

import UPrelude
import qualified Data.HashMap.Strict as HM
import Data.Serialize (Serialize)
import GHC.Generics (Generic)
import Structure.Palette (TexPalette)
import World.Page.Types (WorldPageId)
import Engine.Graphics.Camera (CameraFacing)
import World.Save.Snapshot
    ( SessionSnapshot(..), LiveCameraSnapshot(..) )
import World.Save.Component.Types

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

newtype TexPaletteDTO = TexPaletteDTO { tpdPalette ∷ TexPalette }
    deriving (Show, Eq, Generic, Serialize)

texPaletteCodec ∷ ComponentCodec TexPaletteDTO
texPaletteCodec = serializeCodec
    texPaletteComponentId 1 True []
    (\snap → TexPaletteDTO (snapTexPalette snap)) (\_ d → Right d) (const [])

coreSessionTexPalette ∷ TexPaletteDTO → SessionSnapshot → SessionSnapshot
coreSessionTexPalette d snap = snap { snapTexPalette = tpdPalette d }

-- lua-state ---------------------------------------------------------

newtype LuaStateDTO = LuaStateDTO { lsdModules ∷ HM.HashMap Text Text }
    deriving (Show, Eq, Generic, Serialize)

luaStateCodec ∷ ComponentCodec LuaStateDTO
luaStateCodec = serializeCodec
    luaStateComponentId 1 True []
    (\snap → LuaStateDTO (snapLuaModules snap)) (\_ d → Right d) (const [])

coreSessionLua ∷ LuaStateDTO → SessionSnapshot → SessionSnapshot
coreSessionLua d snap = snap { snapLuaModules = lsdModules d }
