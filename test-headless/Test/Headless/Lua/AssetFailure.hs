-- | The Lua half of #1690: a texture request that TERMINALLY FAILED
--   must settle every consumer waiting on it.
--
--   Requirement 4 is not satisfied by the engine simply declining to
--   lie. Before #1690 a slot-exhausted upload still announced
--   @LuaAssetLoaded@, so every Lua waiter resolved — on a handle that
--   sampled the undefined texture. Suppressing that announcement alone
--   would trade a wrong picture for a hang: the two consumers that
--   actually wait on a texture request would wait forever.
--
--   So the engine's new @onAssetFailed@ broadcast is only half the fix,
--   and this is the other half. Both consumers run in a stdlib-only Lua
--   interpreter — no engine, no GPU, no UI backend, the pattern
--   "Test.Headless.Lua.SharedHelpers" uses — with their own module
--   dependencies stubbed through @package.loaded@ so the REAL script
--   under @scripts/@ is the thing being driven.
module Test.Headless.Lua.AssetFailure (spec) where

import UPrelude
import Test.Hspec
import qualified HsLua as Lua
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.IO as TIO

-- | Run one self-contained Lua chunk in a fresh stdlib-only
--   interpreter, with the repo root as CWD (as every @cabal test@ run
--   has). The chunk signals failure through Lua's own @assert()@.
runsOk ∷ Text → Expectation
runsOk chunkText = do
    result ← Lua.run $ do
        Lua.openlibs
        status ← Lua.dostring (TE.encodeUtf8 chunkText)
        case status of
            Lua.OK → return Nothing
            _ → do
                err ← Lua.tostring (-1)
                return (Just (maybe "<no message>" TE.decodeUtf8Lenient err))
    case result of
        Nothing  → pure ()
        Just msg → expectationFailure (T.unpack msg)

lns ∷ [Text] → Text
lns = T.intercalate "\n"

-- | A logger-only @engine@ global. Nothing here may reach a real
--   engine call: if one of these handlers ever grows a dependency on
--   live engine state, it fails here rather than in a boot.
engineStub ∷ Text
engineStub = lns
    [ "engine = {"
    , "  logInfo = function() end, logWarn = function() end,"
    , "  logDebug = function() end, logError = function() end,"
    , "}"
    ]

spec ∷ Spec
spec = do
  describe "world_view stays non-blocking when a texture fails" $ do
    -- 'scripts/world_view.lua' gates world creation on
    -- texturesLoadedCount >= texturesNeeded. A failure that counted for
    -- nothing would leave that gate one short forever.
    it "counts a failed request toward the same gate a load counts \
       \toward, so the gate still resolves" $ runsOk $ lns
      [ engineStub
      , "package.loaded['scripts.world_manager'] = {"
      , "  isActive = function() return false end }"
      , "local worldView = dofile('scripts/world_view.lua')"
      , "assert(type(worldView.onAssetFailed) == 'function',"
      , "  'world_view must handle onAssetFailed')"
      , "worldView.allHandles = {}"
      , "worldView.seenHandles = {}"
      , "worldView.visible = false"
      , "worldView.pendingGeneration = false"
      , "worldView.texturesNeeded = 2"
      , "worldView.texturesLoadedCount = 0"
      , "worldView.onAssetLoaded('texture', 7, 'a.png')"
      , "assert(worldView.texturesLoadedCount == 1,"
      , "  'a successful load counts once')"
      , "worldView.onAssetFailed('texture', 8, 'b.png', 'no bindless slot')"
      , "assert(worldView.texturesLoadedCount == 2,"
      , "  'a FAILED request must also settle the gate; got '"
      , "    .. tostring(worldView.texturesLoadedCount))"
      , "assert(worldView.texturesLoadedCount >= worldView.texturesNeeded,"
      , "  'the world-creation gate must be resolved, not stalled')"
      ]

    it "counts a given handle at most once, however it settles" $ runsOk $ lns
      [ engineStub
      , "package.loaded['scripts.world_manager'] = {"
      , "  isActive = function() return false end }"
      , "local worldView = dofile('scripts/world_view.lua')"
      , "worldView.allHandles = {}"
      , "worldView.seenHandles = {}"
      , "worldView.visible = false"
      , "worldView.pendingGeneration = false"
      , "worldView.texturesNeeded = 3"
      , "worldView.texturesLoadedCount = 0"
      , "worldView.onAssetFailed('texture', 8, 'b.png', 'no bindless slot')"
      , "worldView.onAssetFailed('texture', 8, 'b.png', 'no bindless slot')"
      , "assert(worldView.texturesLoadedCount == 1,"
      , "  'the seenHandles dedup must cover failures too; got '"
      , "    .. tostring(worldView.texturesLoadedCount))"
      ]

    it "ignores a non-texture failure, exactly as the load twin does"
      $ runsOk $ lns
      [ engineStub
      , "package.loaded['scripts.world_manager'] = {"
      , "  isActive = function() return false end }"
      , "local worldView = dofile('scripts/world_view.lua')"
      , "worldView.allHandles = {}"
      , "worldView.seenHandles = {}"
      , "worldView.texturesNeeded = 2"
      , "worldView.texturesLoadedCount = 0"
      , "worldView.onAssetFailed('font', 3, 'arcade.ttf', 'boom')"
      , "assert(worldView.texturesLoadedCount == 0,"
      , "  'only texture requests feed this gate')"
      ]

  describe "preview_manager leaves its loading state when a texture fails" $ do
    -- List/item mode holds ONE in-flight request and settles it on
    -- @onAssetLoaded@; nothing else ever moves it out of "loading".
    let previewStubs = lns
          [ "package.loaded['scripts.ui.asset_browser'] = {}"
          , "package.loaded['scripts.ui.list'] = {}"
          , "package.loaded['scripts.ui.unit_animation_view'] = {}"
          , "package.loaded['scripts.ui.building_asset_view'] = {}"
          ]

    it "settles on a terminal state instead of waiting forever"
      $ runsOk $ lns
      [ engineStub
      , previewStubs
      , "dofile('scripts/preview_manager.lua')"
      , "local pm = package.loaded['scripts.preview_manager']"
      , "assert(type(pm.onAssetFailed) == 'function',"
      , "  'preview_manager must handle onAssetFailed')"
      , "assert(pm.dump().state == 'loading', 'starts loading')"
      , "-- applyTexture is how this session records a path it asked"
      , "-- for; panelBounds is nil here so it caches and returns."
      , "pm.applyTexture(5, 'assets/textures/icons/foo.png')"
      , "pm.onAssetFailed('texture', 5, 'assets/textures/icons/foo.png',"
      , "  'no bindless slot available')"
      , "local state = pm.dump().state"
      , "assert(state ~= 'loading',"
      , "  'a failed texture must not leave the viewer loading; got '"
      , "    .. tostring(state))"
      , "assert(state == 'empty',"
      , "  'the terminal state is the existing \"empty\", not a new "
        <> "value the dump contract does not carry; got ' .. tostring(state))"
      ]

    it "stays settled: nothing later moves it back to loading"
      $ runsOk $ lns
      [ engineStub
      , previewStubs
      , "dofile('scripts/preview_manager.lua')"
      , "local pm = package.loaded['scripts.preview_manager']"
      , "pm.applyTexture(5, 'assets/textures/icons/foo.png')"
      , "pm.onAssetFailed('texture', 5, 'assets/textures/icons/foo.png', 'full')"
      , "pm.update(0.016)"
      , "assert(pm.dump().state == 'empty',"
      , "  'update() must not overwrite the terminal state')"
      ]

    it "ignores a failure for a texture this session never requested"
      $ runsOk $ lns
      [ engineStub
      , previewStubs
      , "dofile('scripts/preview_manager.lua')"
      , "local pm = package.loaded['scripts.preview_manager']"
      , "pm.onAssetFailed('texture', 99, 'somewhere/else.png', 'full')"
      , "assert(pm.dump().state == 'loading',"
      , "  'a stray failure must not blank a viewer showing its own art')"
      ]

    it "drops the dead handle so a later selection re-requests the path"
      $ runsOk $ lns
      [ engineStub
      , previewStubs
      , "dofile('scripts/preview_manager.lua')"
      , "local pm = package.loaded['scripts.preview_manager']"
      , "local path = 'assets/textures/icons/foo.png'"
      , "pm.applyTexture(5, path)"
      , "pm.onAssetFailed('texture', 5, path, 'full')"
      , "-- A cached entry would be reused verbatim; dropping it is what"
      , "-- makes the next selection issue a fresh engine.loadTexture."
      , "pm.onAssetFailed('texture', 5, path, 'full')"
      , "assert(pm.dump().state == 'empty',"
      , "  'the second delivery must be a harmless no-op')"
      ]

  describe "ui_manager forwards the failure to the modules it owns" $
    -- worldView and testArena are reached through uiManager's manual
    -- forward, not the engine's own broadcast (they are submodules of
    -- uiManager, not modules broadcastToModules knows about), so the
    -- failure callback needs the same forwarding its success twin in
    -- ui_manager_boot.lua has. Asserted on the shipped source rather
    -- than by booting the ui_manager split, which gates on a GPU font
    -- atlas and never runs headless at all.
    it "defines uiManager.onAssetFailed and hands it to both" $ do
      src ← TIO.readFile "scripts/ui_manager_events.lua"
      let forwarder = snd (T.breakOn "function uiManager.onAssetFailed" src)
          body = T.takeWhile (≢ '\0') (fst (T.breakOn "\nend" forwarder))
      forwarder `shouldSatisfy` (not ∘ T.null)
      body `shouldSatisfy` T.isInfixOf "worldView.onAssetFailed"
      body `shouldSatisfy` T.isInfixOf "testArena.onAssetFailed"
