{-# LANGUAGE Strict, UnicodeSyntax #-}
module World.Command.Types
    ( WorldTextureType(..)
    , WorldCommand(..)
    ) where

import UPrelude
import qualified Data.HashMap.Strict as HM
import Engine.Asset.Handle (TextureHandle(..))
import World.Page.Types (WorldPageId(..))
import World.Render.Zoom.Types (ZoomMapMode(..))
import World.Tool.Types (ToolMode(..))
import World.Save.Types (SaveData(..))
import World.Texture.Types (WorldTextureType(..))
import World.Fluid.Types (FluidType(..))

data WorldCommand
    = WorldInit WorldPageId Word64 Int Int
    | WorldInitArena WorldPageId
    | WorldInitArenaDone WorldPageId
    | WorldShow WorldPageId
    | WorldHide WorldPageId
    | WorldTick Double
    | WorldSetTexture WorldPageId WorldTextureType TextureHandle
    | WorldSetCamera WorldPageId Float Float
    | WorldSetTime WorldPageId Int Int         -- ^ hour minute
    | WorldSetDate WorldPageId Int Int Int     -- ^ year month day
    | WorldSetTimeScale WorldPageId Float      -- ^ game-minutes per real-second
    | WorldSetMapMode WorldPageId ZoomMapMode      -- ^ map mode
    | WorldSetZoomCursorHover WorldPageId Int Int
    | WorldSetZoomCursorSelect WorldPageId
    | WorldSetZoomCursorDeselect WorldPageId
    | WorldSetZoomCursorSelectTexture WorldPageId TextureHandle
    | WorldSetZoomCursorHoverTexture WorldPageId TextureHandle
    | WorldSetWorldCursorSelectTexture WorldPageId TextureHandle
    | WorldSetWorldCursorHoverTexture WorldPageId TextureHandle
    | WorldSetWorldCursorSelectBgTexture WorldPageId TextureHandle
    | WorldSetWorldCursorHoverBgTexture WorldPageId TextureHandle
    | WorldSetWorldCursorHover WorldPageId Int Int
    | WorldSetWorldCursorSelect WorldPageId
    | WorldSetWorldCursorDeselect WorldPageId
    | WorldSelectTileByCoord WorldPageId Int Int
        -- ^ Atomically set worldSelectedTile to the column at (gx, gy)
        --   using the loaded chunk's surface z. Bypasses the hover-
        --   then-select dance so the caller doesn't have to fight the
        --   continuous mouse-hover overwrites for one-shot selections
        --   (e.g. context-menu "Info" on a tile).
    | WorldSetToolMode WorldPageId ToolMode
    | WorldSave WorldPageId Text Text (HM.HashMap Text Text)
        -- ^ pageId, save-name, request-timestamp (ISO 8601 second
        --   precision), Lua-module blobs. The Lua side captures the
        --   timestamp at request time (so two saves queued close
        --   together get distinct timestamps reflecting when the
        --   player asked, not whenever the world thread happened to
        --   process them) and calls saveModules.serializeAll()
        --   before queueing this command so the world thread can
        --   stuff the blobs into SaveData.
    | WorldLoadSave WorldPageId SaveData
    | WorldDeleteTile WorldPageId Int Int      -- ^ worldId, gx, gy
    | WorldSetFluidTile WorldPageId Int Int FluidType
        -- ^ worldId, gx, gy, fluid kind. Sets one tile of fluid at
        --   surfaceZ + 1 on the given column. Idempotent; replaces any
        --   existing fluid cell. Currently a debug-tool affordance.
    | WorldDestroy !WorldPageId
    deriving (Show)
