-- | Texture loading and the scene SPRITE primitive family
--   (@engine.spawnSprite@ / @setPos@ / @setColor@ / @setSize@ /
--   @setVisible@ / @destroy@). The text primitive family lives in
--   "Engine.Scripting.Lua.API.Text"; the two share the mutation verbs
--   here, which act on whichever node kind the id names.
--
--   __Where a scene sprite sits relative to UI pages (#2192).__ The
--   @layer@ argument is the sprite's render 'LayerId', and
--   'World.Grid.uiLayerThreshold' (10) splits it in two:
--
--   * BELOW the threshold the sprite is world content: positioned in
--     world units, frustum-culled, and interleaved by depth with the
--     tiles and units of that layer through the same painter's merge.
--   * AT or ABOVE it the sprite is a UI-pipeline quad in framebuffer
--     pixels (origin top-left, @x@\/@y@ its CENTRE), never culled, and
--     drawn as its own item at exactly that layer — above every world
--     layer, and ordered against the UI pages' elements by layer number.
--     Pages start at the threshold and climb by 'UI.Types.uiLayerBand'
--     (HUD at the base, menus and modals in the tens of thousands, the
--     debug band highest), so a layer just above 10 sits under every
--     page while a layer above the debug band sits over all of them.
--
--   Within ONE layer the frame draws every sprite before every text, and
--   within either kind world-derived content first, then scene
--   primitives, then UI-page elements ("Engine.Scene.Assembly"). Every
--   mutation is picked up by the next frame; an invisible or destroyed
--   node draws nothing.
module Engine.Scripting.Lua.API.Graphics
  ( loadTextureFn
  , getTextureSizeFn
  , getLoadedTexturePathsFn
  , spawnSpriteFn
  , setPosFn
  , setColorFn
  , setSizeFn
  , setVisibleFn
  , destroyFn
  , getUIScaleFn
  ) where

import UPrelude
import Math (colorToVec4)
import Engine.Scripting.Lua.Types (LuaBackendState(..), LuaToEngineMsg(..))
import Engine.Asset.Manager (updateTextureState, generateTextureHandle)
import Engine.Asset.Handle (TextureHandle(..), AssetState(..))
import Engine.Asset.Types (AssetPool(..))
import Engine.Scene.Base (ObjectId(..), LayerId(..))
import Engine.Graphics.Config (VideoConfig(..))
import Engine.Graphics.Vulkan.Texture.Policy
  (UploadSampler(..), TextureCacheKey(..), parseUploadPolicy
  , uploadPolicyNames)
import Engine.Core.State (EngineEnv, loggerRef)
import Engine.Core.Capability.RenderView
  (RenderViewCapability(..), toRenderViewCapability)
import Engine.Core.Log (LogCategory(..), logWarn, logDebug)
import qualified Engine.Core.Queue as Q
import qualified Data.HashMap.Strict as HM
import qualified Data.Map.Strict as Map
import qualified Data.List.NonEmpty as NE
import qualified HsLua as Lua
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.IORef (readIORef, atomicModifyIORef')
import Control.Monad.IO.Class (liftIO)

getUIScaleFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
getUIScaleFn env = do
    vconfig ← Lua.liftIO $ readIORef (rvVideoConfigRef (toRenderViewCapability env))
    Lua.pushnumber (Lua.Number (realToFrac (vcUIScale vconfig)))
    return 1

-- | engine.loadTexture(path[, policy]) → handle | nil
--
--   @policy@ (#2075, D-4) declares which sampler the slot this load
--   creates is registered with: @"scene"@ follows the player's
--   nearest\/linear video setting and is repainted live by
--   @engine.setTextureFilter@; @"ui"@ is pinned to nearest for the
--   session, which is what UI chrome and the icons the UI\/HUD layers
--   draw want.
--
--   OMITTING it selects @"scene"@, so every pre-#2075 call site keeps
--   exactly the behavior it had. That is the only way to select the
--   default: a policy argument that is PRESENT but names no policy —
--   a typo, or a value of any type other than a string — is REFUSED
--   with a warning and a @nil@ handle, and queues no load at all.
--   Classifying it as scene art instead would be the silent
--   mis-categorisation this API exists to prevent, and would look
--   correct right up until the player toggled the filter.
--
--   The policy cannot be inferred from the path and is never guessed
--   from one: @assets\/textures\/icons\/location\/*@ are drawn on the
--   world's zoom map while the rest of @icons\/@ is toolbar chrome,
--   @assets\/textures\/ui\/hud\/utility\/world_*@ are the world cursor
--   overlays, and @assets\/textures\/utility\/white.png@ is drawn by
--   both layers.
loadTextureFn ∷ LuaBackendState → Lua.LuaE Lua.Exception Lua.NumResults
loadTextureFn backendState = do
  path ← Lua.tostring 1
  policy ← requestedUploadPolicy
  case (path, policy) of
    (Just pathBS, Just samplerPolicy) → do
      handle ← Lua.liftIO $ do
        let pathStr = TE.decodeUtf8Lenient pathBS
            (lteq, _) = lbsMsgQueues backendState
        pool ← readIORef (lbsAssetPool backendState)
        handle ← generateTextureHandle pool
        updateTextureState handle
          (AssetLoading (T.unpack pathStr) [] 0.0) pool
        Q.writeQueue lteq
          (LuaLoadTextureRequest handle (T.unpack pathStr) samplerPolicy)
        return handle
      let (TextureHandle n) = handle
      Lua.pushnumber (Lua.Number (fromIntegral n))
    (Just pathBS, Nothing) → do
      -- A named policy nothing recognises. Report the path AND the
      -- offending token so the call site is findable, then refuse:
      -- 'AssetLoading' is never written and nothing is queued, so no
      -- handle exists for a caller to hold.
      offending ← describePolicyArgument
      Lua.liftIO $ do
        logger ← readIORef (lbsLoggerRef backendState)
        logWarn logger CatLua $
          "engine.loadTexture: unknown upload policy " <> offending
          <> " for " <> TE.decodeUtf8Lenient pathBS
          <> " (expected one of "
          <> T.intercalate ", " uploadPolicyNames
          <> ", or omit the argument to follow the player's filter)"
      Lua.pushnil
    (Nothing, _) → Lua.pushnil
  return 1

-- | The upload policy argument 2 asks for.
--
--   @Just@ wraps a decision; @Nothing@ is a REFUSAL, not a default.
--   An ABSENT argument ('Lua.TypeNone') is the ONLY shape that yields
--   the backward-compatible 'UploadGlobalSampler'. An explicit @nil@ is
--   a present argument that names no policy and is refused with every
--   other unsupported value: a @nil@ reaching here is almost always a
--   variable that lost its value on the way — a pass-through helper
--   called without its policy — and accepting it would silently file
--   that texture as scene art, which is the one outcome this argument
--   exists to prevent. A string is looked up in 'parseUploadPolicy';
--   ANY other type is refused outright rather than coerced —
--   @Lua.tostring@ would happily turn the number @2@ into @"2"@, and a
--   coercion that can only ever fail the lookup adds nothing but a
--   confusing diagnostic.
requestedUploadPolicy ∷ Lua.LuaE Lua.Exception (Maybe UploadSampler)
requestedUploadPolicy = Lua.ltype 2 ⌦ \case
  Lua.TypeNone   → pure (Just UploadGlobalSampler)
  Lua.TypeString → do
    raw ← Lua.tostring 2
    pure (parseUploadPolicy . TE.decodeUtf8Lenient =≪ raw)
  _              → pure Nothing

-- | Argument 2 as it should appear in the refusal above: the token
--   itself when it is a string, otherwise the Lua type name, since a
--   table or function has no useful rendering.
describePolicyArgument ∷ Lua.LuaE Lua.Exception Text
describePolicyArgument = Lua.ltype 2 ⌦ \case
  Lua.TypeString → do
    raw ← Lua.tostring 2
    pure $ maybe "(unreadable)"
        (\bs → "'" <> TE.decodeUtf8Lenient bs <> "'") raw
  Lua.TypeNil → pure "an explicit nil (omit the argument instead)"
  other → pure $ "of type " <> tshow other

-- | engine.getTextureSize(handle) → {width=, height=} | nil
--   The natural pixel dimensions of a texture 'engine.loadTexture'
--   already finished uploading (populated into 'textureSizeRef' the
--   moment its GPU upload completes — see
--   'Engine.Scripting.Lua.Message.Texture'). 'nil' for an unknown
--   handle or one whose upload hasn't landed yet; a caller should only
--   query this from its own @onAssetLoaded("texture", handle, path)@
--   callback (#886's preview browser fits the selected texture into its
--   panel with aspect ratio preserved this way).
getTextureSizeFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
getTextureSizeFn env = do
  handleArg ← Lua.tointeger 1
  case handleArg of
    Just h → do
      mSize ← Lua.liftIO $ HM.lookup (TextureHandle (fromIntegral h))
          ⊚ readIORef (rvTextureSizeRef (toRenderViewCapability env))
      case mSize of
        Just (w, h') → do
          Lua.newtable
          Lua.pushinteger (fromIntegral w)
          Lua.setfield (-2) "width"
          Lua.pushinteger (fromIntegral h')
          Lua.setfield (-2) "height"
        Nothing → Lua.pushnil
    Nothing → Lua.pushnil
  return 1

-- | engine.getLoadedTexturePaths() → array of every currently-loaded
--   texture's file path. 'apAssetPaths' is the authoritative record
--   'engine.loadTexture'\'s own Haskell handler
--   ('Engine.Scripting.Lua.Message.Texture.handleLoadTextureBatch')
--   inserts into the moment an upload completes, regardless of WHICH
--   Lua caller requested it — so this is a ground-truth enumeration a
--   probe can check against an allowlist, not a caller's own self-
--   reported bookkeeping (#886's preview-mode trimmed-loading proof:
--   every entry here must resolve under the browsed category's root or
--   be a documented chrome asset).
--
--   One entry per distinct FILE, never per cache entry. Since #2075 the
--   cache is keyed by @(path, policy)@, so a genuinely dual-use texture
--   — @utility\/white.png@, drawn by both the UI and the world — holds
--   two entries and two slots. This API answers the question its
--   consumers actually ask ("which files did this session load?"), so
--   it collapses the policy out and reports that path once, in the
--   ascending path order it always had.
getLoadedTexturePathsFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
getLoadedTexturePathsFn env = do
  let rv = toRenderViewCapability env
  paths ← Lua.liftIO $
      map NE.head . NE.group . map tckPath . Map.keys . apAssetPaths
        ⊚ readIORef (rvAssetPoolRef rv)
  Lua.newtable
  forM_ (zip [1 ∷ Int ..] paths) $ \(i, p) → do
    Lua.pushstring (TE.encodeUtf8 p)
    Lua.rawseti (-2) (fromIntegral i)
  return 1

spawnSpriteFn ∷ EngineEnv → LuaBackendState → Lua.LuaE Lua.Exception Lua.NumResults
spawnSpriteFn env backendState = do
  x ← Lua.tonumber 1
  y ← Lua.tonumber 2
  width ← Lua.tonumber 3
  height ← Lua.tonumber 4
  texHandleNum ← Lua.tointeger 5
  layer ← Lua.tointeger 6
  
  case (x, y, width, height, texHandleNum) of
    (Just xVal, Just yVal, Just wVal, Just hVal, Just texNum) → do
      let layerId = LayerId $ fromIntegral $ fromMaybe 0 layer
      objId ← Lua.liftIO $ do
        logger ← readIORef $ loggerRef env
        objId ← atomicModifyIORef' (lbsNextObjectId backendState) 
          (\n → (n + 1, ObjectId n))
        
        logDebug logger CatLua $ "Lua spawning sprite with ID " 
                       <> tshow objId
        
        let (lteq, _) = lbsMsgQueues backendState
            texHandle = TextureHandle (fromIntegral texNum)
            msg = LuaSpawnSpriteRequest
              { lssObjectId      = objId
              , lssX             = realToFrac xVal
              , lssY             = realToFrac yVal
              , lssWidth         = realToFrac wVal
              , lssHeight        = realToFrac hVal
              , lssTextureHandle = texHandle
              , lssLayer         = layerId
              }
        Q.writeQueue lteq msg
        return objId
      
      let (ObjectId n) = objId
      Lua.pushinteger (Lua.Integer $ fromIntegral n)
      
    _ → do
      Lua.liftIO $ do
        logger ← readIORef $ loggerRef env
        logWarn logger CatLua
          "spawnSprite requires 5 arguments: x, y, width, height, textureHandle"
      Lua.pushnil
  return 1

setPosFn ∷ EngineEnv → LuaBackendState → Lua.LuaE Lua.Exception Lua.NumResults
setPosFn env backendState = do
  objIdNum ← Lua.tointeger 1
  x ← Lua.tonumber 2
  y ← Lua.tonumber 3
  case (objIdNum, x, y) of
    (Just idVal, Just xVal, Just yVal) → do
      Lua.liftIO $ do
        let (lteq, _) = lbsMsgQueues backendState
            msg = LuaSetPosRequest (ObjectId (fromIntegral idVal))
              (realToFrac xVal) (realToFrac yVal)
        Q.writeQueue lteq msg
      return 0
    _ → liftIO $ do
      logger ← readIORef $ loggerRef env
      logWarn logger CatLua
        "setPos requires 3 arguments: objectId, x, y"
      return 0

setColorFn ∷ EngineEnv → LuaBackendState → Lua.LuaE Lua.Exception Lua.NumResults
setColorFn env backendState = do
  objIdNum ← Lua.tointeger 1
  color ← Lua.tostring 2
  case (objIdNum, color) of
    (Just idVal, Just c) → do
      Lua.liftIO $ do
        let (lteq, _) = lbsMsgQueues backendState
            cStr = T.unpack $ TE.decodeUtf8Lenient c
            msg = LuaSetColorRequest (ObjectId (fromIntegral idVal)) (colorToVec4 cStr)
        Q.writeQueue lteq msg
      return 0
    _ → liftIO $ do
      logger ← readIORef $ loggerRef env
      logWarn logger CatLua
        "setColor requires 2 arguments: objectId, color"
      return 0

setSizeFn ∷ EngineEnv → LuaBackendState → Lua.LuaE Lua.Exception Lua.NumResults
setSizeFn env backendState = do
  objIdNum ← Lua.tointeger 1
  width ← Lua.tonumber 2
  height ← Lua.tonumber 3
  case (objIdNum, width, height) of
    (Just idVal, Just wVal, Just hVal) → do
      Lua.liftIO $ do
        let (lteq, _) = lbsMsgQueues backendState
            msg = LuaSetSizeRequest (ObjectId (fromIntegral idVal))
              (realToFrac wVal) (realToFrac hVal)
        Q.writeQueue lteq msg
      return 0
    _ → liftIO $ do
      logger ← readIORef $ loggerRef env
      logWarn logger CatLua
        "setSize requires 3 arguments: objectId, width, height"
      return 0

setVisibleFn ∷ EngineEnv → LuaBackendState → Lua.LuaE Lua.Exception Lua.NumResults
setVisibleFn env backendState = do
  objIdNum ← Lua.tointeger 1
  visible ← Lua.toboolean 2
  case objIdNum of
    Just idVal → do
      Lua.liftIO $ do
        let (lteq, _) = lbsMsgQueues backendState
            msg = LuaSetVisibleRequest (ObjectId (fromIntegral idVal)) visible
        Q.writeQueue lteq msg
      return 0
    _ → liftIO $ do
      logger ← readIORef $ loggerRef env
      logWarn logger CatLua
        "setVisible requires 2 arguments: objectId, visible"
      return 0

destroyFn ∷ EngineEnv → LuaBackendState → Lua.LuaE Lua.Exception Lua.NumResults
destroyFn env backendState = do
  objIdNum ← Lua.tointeger 1
  case objIdNum of
    Just idVal → do
      Lua.liftIO $ do
        logger ← readIORef $ loggerRef env
        let objId = ObjectId (fromIntegral idVal)
        logDebug logger CatLua $ "Lua destroying object with ID " 
                       <> tshow objId
        let (lteq, _) = lbsMsgQueues backendState
            msg = LuaDestroyRequest (ObjectId (fromIntegral idVal))
        Q.writeQueue lteq msg
      return 0
    _ → liftIO $ do
      logger ← readIORef $ loggerRef env
      logWarn logger CatLua
        "destroy requires 1 argument: objectId"
      return 0
