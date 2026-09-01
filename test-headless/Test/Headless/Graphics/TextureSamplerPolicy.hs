-- | The upload-policy routing #2075 introduced, end to end minus the
--   GPU: a Lua caller declares a policy, the queued request carries it,
--   the message loop bursts by it, the path cache keys on it, and a
--   filter toggle then repaints only the slots that follow the player.
--
--   Four seams, all reachable with no 'Vulkan.Core10.Device':
--
--   1. The DECLARATION — the real @engine.loadTexture@ registered on a
--      live headless engine, driven through the same @loadstring@+pcall
--      primitive the TCP debug console uses. What a Lua caller writes is
--      the whole classification (D-4), so asserting on a hand-built
--      message would prove nothing about it.
--   2. The BATCHING — 'spanTextureLoads', the function
--      'Engine.Scripting.Lua.Message' itself splits bursts with.
--   3. The CACHE — 'classifyBatchRequests', the pure decision the upload
--      path makes about every request before it touches a device.
--   4. The OUTCOME — 'planFilterRebind', which decides per slot what a
--      toggle repaints.
--
--   The Vulkan handle values below are fabricated 'Word64' newtypes, as
--   in "Test.Headless.Graphics.BindlessRebind"; nothing dereferences
--   them.
module Test.Headless.Graphics.TextureSamplerPolicy (spec) where

import UPrelude
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import Data.IORef (newIORef)
import Test.Hspec
import Engine.Asset.Base (AssetId(..))
import Engine.Asset.Handle (TextureHandle(..))
import Engine.Asset.TextureCache
  (BatchClassification(..), classifyBatchRequests)
import Engine.Core.State (EngineEnv, luaToEngineQueue, luaQueue, assetPoolRef
    , nextObjectIdRef, inputStateRef, loggerRef)
import Engine.Core.Thread (ThreadControl(..))
import qualified Engine.Core.Queue as Q
import Engine.Graphics.Vulkan.Texture.Handle (BindlessTextureHandle(..))
import Engine.Graphics.Vulkan.Texture.Policy
  (UploadSampler(..), TextureCacheKey(..), parseUploadPolicy)
import Engine.Graphics.Vulkan.Texture.Rebind
  (FilterRebindPlan(..), SlotRebind(..), planFilterRebind)
import Engine.Graphics.Vulkan.Texture.Slot (TextureSlot(..))
import Engine.Scripting.Lua.API (registerLuaAPI)
import Engine.Scripting.Lua.Message (spanTextureLoads)
import Engine.Scripting.Lua.Thread (createLuaBackendState)
import Engine.Scripting.Lua.Thread.Console (executeDebugLua)
import Engine.Scripting.Lua.Types (LuaBackendState(..), LuaToEngineMsg(..))
import Vulkan.Core10 (ImageView(..), Sampler(..))

-- * The Lua declaration seam

-- | A bare Lua state with the real engine API bound to this env — the
--   same shape "Test.Headless.Asset.TextureFallback" and
--   "Test.Headless.UI.Clipping" each build for themselves.
newBareLuaBackend ∷ EngineEnv → IO LuaBackendState
newBareLuaBackend env = do
    ls ← createLuaBackendState (luaToEngineQueue env) (luaQueue env)
                                (assetPoolRef env) (nextObjectIdRef env)
                                (inputStateRef env) (loggerRef env)
    stateRef ← newIORef ThreadRunning
    registerLuaAPI (lbsLuaState ls) env ls stateRef
    pure ls

-- | Run one Lua expression through the console primitive.
evalDebug ∷ LuaBackendState → Text → IO Text
evalDebug ls = executeDebugLua (lbsLuaState ls)

-- | Every ordinary texture load the queue is holding, as
--   @(path, policy)@. Drains, so each case starts from empty.
drainTextureRequests ∷ EngineEnv → IO [(FilePath, UploadSampler)]
drainTextureRequests env = do
    msgs ← Q.flushQueue (luaToEngineQueue env)
    pure [ (path, policy) | LuaLoadTextureRequest _ path policy ← msgs ]

-- | Evaluate @engine.loadTexture(...)@ with the given argument list and
--   report whether it returned a handle, plus what it queued.
loadWith ∷ EngineEnv → LuaBackendState → Text
         → IO (Bool, [(FilePath, UploadSampler)])
loadWith env ls args = do
    _ ← drainTextureRequests env
    answer ← evalDebug ls
        ("return engine.loadTexture(" <> args <> ") ~= nil")
    queued ← drainTextureRequests env
    pure (T.strip answer ≡ "true", queued)

uiPath, scenePath ∷ Text
uiPath    = "assets/textures/ui/highlight.png"
scenePath = "assets/textures/world/loam/loam.png"

-- * Fabricated GPU-side values

-- | The UI slot's own pinned nearest sampler.
pinnedNearest ∷ Sampler
pinnedNearest = Sampler 0x0EA5

-- | The global sampler a toggle to LINEAR has just acquired.
newLinear ∷ Sampler
newLinear = Sampler 0x11EA

-- | The global sampler while the player's setting is NEAREST. A
--   distinct object from 'pinnedNearest' only so the assertions can
--   tell which one a slot received; in the engine both resolve through
--   the same refcounted cache and may legitimately BE the same object.
globalNearest ∷ Sampler
globalNearest = Sampler 0x0EA6

uiHandle, sceneHandle ∷ TextureHandle
uiHandle    = TextureHandle 11
sceneHandle = TextureHandle 12

uiView, sceneView ∷ ImageView
uiView    = ImageView 0x00A1
sceneView = ImageView 0x05CE

at ∷ TextureHandle → Word32 → (TextureHandle, BindlessTextureHandle)
at texHandle slotIdx =
  (texHandle, BindlessTextureHandle (TextureSlot slotIdx 0) texHandle)

-- | The two slots one dual-use path owns: the UI one pinned, the scene
--   one following the global sampler.
dualUsePlan ∷ Sampler → FilterRebindPlan
dualUsePlan newGlobal = planFilterRebind
    (Map.fromList [uiHandle `at` 1, sceneHandle `at` 2])
    (Map.fromList [(uiHandle, uiView), (sceneHandle, sceneView)])
    (Map.singleton uiHandle pinnedNearest)
    newGlobal

spec ∷ SpecWith EngineEnv
spec = do
  describe "the Lua upload-policy declaration" $ do
    it "routes an explicit \"ui\" load to the pinned-nearest policy" $ \env → do
        ls ← newBareLuaBackend env
        (ok, queued) ← loadWith env ls ("'" <> uiPath <> "', 'ui'")
        ok `shouldBe` True
        queued `shouldBe` [(T.unpack uiPath, UploadPinnedNearest)]

    -- D-4's backward-compatible default, and the reason 172 untouched
    -- call sites keep working.
    it "classifies a load with NO declared policy as scene art" $ \env → do
        ls ← newBareLuaBackend env
        (ok, queued) ← loadWith env ls ("'" <> scenePath <> "'")
        ok `shouldBe` True
        queued `shouldBe` [(T.unpack scenePath, UploadGlobalSampler)]

    it "treats an explicit nil the same as omission" $ \env → do
        ls ← newBareLuaBackend env
        (ok, queued) ← loadWith env ls ("'" <> scenePath <> "', nil")
        ok `shouldBe` True
        queued `shouldBe` [(T.unpack scenePath, UploadGlobalSampler)]

    it "accepts \"scene\" as the explicit spelling of that default" $ \env → do
        ls ← newBareLuaBackend env
        (ok, queued) ← loadWith env ls ("'" <> scenePath <> "', 'scene'")
        ok `shouldBe` True
        queued `shouldBe` [(T.unpack scenePath, UploadGlobalSampler)]

    -- The refusal contract: a PRESENT policy that names nothing must not
    -- fall back to scene art. Silently doing so is the exact
    -- mis-categorisation this argument exists to prevent, and it would
    -- look correct until the player toggled the filter.
    it "REFUSES an unrecognised policy instead of defaulting it" $ \env → do
        ls ← newBareLuaBackend env
        forM_ ["'UI'", "'Scene'", "'pinned'", "'nearest'", "''"] $ \bad → do
            (ok, queued) ← loadWith env ls ("'" <> uiPath <> "', " <> bad)
            (bad, ok) `shouldBe` (bad, False)
            (bad, queued) `shouldBe` (bad, [])

    -- A number would survive 'Lua.tostring' as "2", so the type check
    -- has to happen before any coercion.
    it "REFUSES a non-string policy rather than coercing it" $ \env → do
        ls ← newBareLuaBackend env
        forM_ ["2", "true", "{}", "print"] $ \bad → do
            (ok, queued) ← loadWith env ls ("'" <> uiPath <> "', " <> bad)
            (bad, ok) `shouldBe` (bad, False)
            (bad, queued) `shouldBe` (bad, [])

    it "spells the two policies exactly once, in the parser" $ \_env → do
        parseUploadPolicy "ui" `shouldBe` Just UploadPinnedNearest
        parseUploadPolicy "scene" `shouldBe` Just UploadGlobalSampler
        parseUploadPolicy "UI" `shouldBe` Nothing

  describe "the message loop's per-policy bursts" $ do
    let req h p policy = LuaLoadTextureRequest (TextureHandle h) p policy
        ui   h p = req h p UploadPinnedNearest
        scene h p = req h p UploadGlobalSampler

    it "extends a burst only while the policy is unchanged" $ \_env → do
        let msgs = [ ui 2 "b.png", ui 3 "c.png"
                   , scene 4 "d.png", ui 5 "e.png" ]
        spanTextureLoads UploadPinnedNearest msgs `shouldBe`
            ( [(TextureHandle 2, "b.png"), (TextureHandle 3, "c.png")]
            , [scene 4 "d.png", ui 5 "e.png"] )

    -- The whole point of the split: adjacent runs of different policies
    -- become consecutive batches, and nothing is dropped or reordered.
    it "splits adjacent mixed policies into consecutive batches" $ \_env → do
        let msgs = [ scene 4 "d.png", ui 5 "e.png" ]
            (sceneBurst, afterScene) =
                spanTextureLoads UploadGlobalSampler msgs
            (uiBurst, afterUi) = spanTextureLoads UploadPinnedNearest afterScene
        sceneBurst `shouldBe` [(TextureHandle 4, "d.png")]
        uiBurst `shouldBe` [(TextureHandle 5, "e.png")]
        afterUi `shouldBe` []

    it "stops at an unrelated message, as it always did" $ \_env → do
        let msgs = [ ui 2 "b.png", LuaReleaseFocus, ui 3 "c.png" ]
        spanTextureLoads UploadPinnedNearest msgs `shouldBe`
            ( [(TextureHandle 2, "b.png")]
            , [LuaReleaseFocus, ui 3 "c.png"] )

    it "never absorbs an atlas request" $ \_env → do
        let msgs = [ LuaLoadAtlasTextureRequest (TextureHandle 9) "walk.png" ]
        spanTextureLoads UploadPinnedNearest msgs `shouldBe` ([], msgs)

  describe "the policy-scoped upload cache" $ do
    -- A file both layers draw. The whole cache question is what a
    -- repeated request for it costs.
    let dual ∷ FilePath
        dual = "assets/textures/utility/white.png"
        dualKey = T.pack dual
        -- Every cached candidate is consistent with its key here; the
        -- refusing case gets its own example below.
        consistent = const True

        -- One batch against a cache, returning the classification and
        -- the cache as the batch leaves it: every fresh upload becomes a
        -- canonical entry under THIS batch's key, which is what makes a
        -- later batch a hit.
        runBatch policy (paths, atlases, nextId) reqs =
            let cls = classifyBatchRequests policy consistent paths atlases reqs
                fresh = zip [nextId ..] (bcFresh cls)
                paths' = foldl'
                    (\m (aid, (_, path)) →
                        Map.insert (TextureCacheKey (T.pack path) policy)
                            (AssetId aid) m)
                    paths fresh
                atlases' = foldl'
                    (\m (aid, (_, path)) →
                        Map.insert (AssetId aid) (T.pack path) m)
                    atlases fresh
            in (cls, (paths', atlases'
                     , nextId + fromIntegral (length fresh)))

        emptyCache = (Map.empty, Map.empty, 1 ∷ Word32)

    it "gives one path ONE canonical slot per policy, however often the \
       \two alternate" $ \_env → do
        -- scene -> UI -> scene -> UI, one request each.
        let one h = [(TextureHandle h, dual)]
            (c1, s1) = runBatch UploadGlobalSampler emptyCache (one 1)
            (c2, s2) = runBatch UploadPinnedNearest s1 (one 2)
            (c3, s3) = runBatch UploadGlobalSampler s2 (one 3)
            (c4, s4) = runBatch UploadPinnedNearest s3 (one 4)
            (finalPaths, finalAtlases, _) = s4
        -- The first of each policy uploads...
        map (map fst ∘ bcFresh) [c1, c2] `shouldBe`
            [[TextureHandle 1], [TextureHandle 2]]
        -- ...and every later request of either is a cache HIT, not a
        -- third and fourth upload.
        bcFresh c3 `shouldBe` []
        bcFresh c4 `shouldBe` []
        map (\(h, aid, _) → (h, aid)) (bcCached c3)
            `shouldBe` [(TextureHandle 3, AssetId 1)]
        map (\(h, aid, _) → (h, aid)) (bcCached c4)
            `shouldBe` [(TextureHandle 4, AssetId 2)]
        -- Exactly two canonical slots for the one file, one per policy.
        Map.keys finalPaths `shouldBe`
            [ TextureCacheKey dualKey UploadGlobalSampler
            , TextureCacheKey dualKey UploadPinnedNearest ]
        Map.size finalAtlases `shouldBe` 2

    it "cannot see the other policy's entry, so neither inherits its \
       \sampler" $ \_env → do
        let (_, seeded) = runBatch UploadGlobalSampler emptyCache
                              [(TextureHandle 1, dual)]
            (uiFirst, _) = runBatch UploadPinnedNearest seeded
                              [(TextureHandle 2, dual)]
        -- A scene entry exists for this exact path, and the UI batch
        -- still uploads its own rather than aliasing it.
        bcCached uiFirst `shouldBe` []
        bcFresh uiFirst `shouldBe` [(TextureHandle 2, dual)]

    it "folds a repeat WITHIN one batch into the first request's slot" $ \_env → do
        let (cls, _) = runBatch UploadPinnedNearest emptyCache
                [ (TextureHandle 1, dual)
                , (TextureHandle 2, dual)
                , (TextureHandle 3, "assets/textures/ui/highlight.png") ]
        bcFresh cls `shouldBe`
            [ (TextureHandle 1, dual)
            , (TextureHandle 3, "assets/textures/ui/highlight.png") ]
        bcAliases cls `shouldBe` [(TextureHandle 2, dual, TextureHandle 1)]
        bcCached cls `shouldBe` []

    it "falls through to a fresh upload when the GPU-side check refuses \
       \a candidate hit" $ \_env → do
        let (_, (paths, atlases, _)) =
                runBatch UploadPinnedNearest emptyCache [(TextureHandle 1, dual)]
            cls = classifyBatchRequests UploadPinnedNearest (const False)
                      paths atlases [(TextureHandle 2, dual)]
        bcCached cls `shouldBe` []
        bcFresh cls `shouldBe` [(TextureHandle 2, dual)]

  describe "what a filter toggle then repaints" $ do
    it "leaves the UI slot on nearest and moves the scene slot to the \
       \new global sampler" $ \_env → do
        frpRebinds (dualUsePlan newLinear) `shouldBe`
            [ SlotRebind 1 uiView pinnedNearest
            , SlotRebind 2 sceneView newLinear ]

    it "reports nothing unrecoverable for either" $ \_env →
        frpUnrecoverable (dualUsePlan newLinear) `shouldBe` []

    -- The two slots stay distinct SLOTS whatever the player picks; their
    -- samplers only have to DIVERGE once the global one is linear. While
    -- the setting is nearest they may legitimately agree -- in the
    -- engine they would be the same refcounted object.
    it "lets the two slots agree while the global filter is nearest" $ \_env → do
        frpRebinds (dualUsePlan globalNearest) `shouldBe`
            [ SlotRebind 1 uiView pinnedNearest
            , SlotRebind 2 sceneView globalNearest ]
        map srSlot (frpRebinds (dualUsePlan globalNearest)) `shouldBe` [1, 2]
