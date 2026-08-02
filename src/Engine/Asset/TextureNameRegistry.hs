{-# LANGUAGE Strict #-}
-- | The runtime name→'TextureHandle' registry. Not a YAML loader: it
--   depends on nothing but 'Engine.Asset.Handle', so a consumer that
--   needs only the lookup table pulls in no loader machinery. See #1009.
module Engine.Asset.TextureNameRegistry
    ( TextureNameRegistry
    , emptyTextureNameRegistry
    , lookupTextureName
    , registerTextureName
    ) where

import UPrelude
import qualified Data.HashMap.Strict as HM
import Data.IORef (IORef, atomicModifyIORef')
import Engine.Asset.Handle (TextureHandle)

-- | Maps human-readable names to 'TextureHandle's. Populated by the Lua API
--   when textures are loaded.
--
--   Naming convention:
--
--   * @mat_tile_\<name\>@ — e.g. @"mat_tile_loam"@
--   * @mat_zoom_\<name\>@ — e.g. @"mat_zoom_loam"@
--   * @mat_bg_\<name\>@   — e.g. @"mat_bg_loam"@
--   * @veg_tile_\<id\>@   — e.g. @"veg_tile_1"@
type TextureNameRegistry = HM.HashMap Text TextureHandle

emptyTextureNameRegistry ∷ TextureNameRegistry
emptyTextureNameRegistry = HM.empty

lookupTextureName ∷ Text → TextureNameRegistry → Maybe TextureHandle
lookupTextureName = HM.lookup

registerTextureName ∷ IORef TextureNameRegistry → Text → TextureHandle → IO ()
registerTextureName ref name handle =
    atomicModifyIORef' ref $ \reg → (HM.insert name handle reg, ())
