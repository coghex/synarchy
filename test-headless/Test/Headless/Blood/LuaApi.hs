{-# LANGUAGE Strict #-}
-- | GPU-free coverage for the registered @blood.gpuHandles@ Lua verb
--   (#1585) — the blood-OWNED GPU handle identity + per-registry
--   membership query that lets @tools/blood_gpu_lifecycle_probe.py@
--   judge the save-load replacement path by OWNERSHIP instead of by
--   engine-wide totals a replacement session also moves.
--
--   Driven through the real registered surface
--   ('Engine.Scripting.Lua.API.Blood.bloodGpuHandlesFn' via
--   'Engine.Scripting.Lua.API.registerLuaAPI'), because the contract
--   under test is what a Lua caller receives, not what a Haskell
--   function returns. The engine is this spec's own (@aroundAll
--   withHeadlessEngine@ in @Spec.hs@): it installs its own single-page
--   world manager and writes that page's blood handle map plus the
--   engine's texture-size cache directly, which would disturb the
--   shared worldgen engine.
--
--   No GPU is involved and none is needed. The verb reads two plain
--   'IORef's; the bindless system is 'Nothing' headless, which is
--   exactly the documented headless reading (@bindless@ false for
--   every handle) and is asserted as such. The texture-size cache IS
--   writable headless, so the two registries can be pinned to
--   DIFFERENT contents — which is what proves the verb reports them
--   independently, the property a partial-leak check depends on.
--   Whether real Vulkan disposal actually clears them is the GPU
--   probe's job.
--
--   Run just this gate: @cabal test synarchy-test-headless
--   --test-options='--match "Blood.LuaApi"'@.
module Test.Headless.Blood.LuaApi (spec) where

import UPrelude
import Test.Hspec
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import Data.IORef (newIORef, readIORef, writeIORef)
import Data.List (sort)
import Blood.Types (BloodTextureId(..))
import Engine.Asset.Handle (TextureHandle(..))
import Engine.Core.Capability.RenderView
    (RenderViewCapability(..), toRenderViewCapability)
import Engine.Core.State (EngineEnv(..))
import Engine.Core.Thread (ThreadControl(..))
import Engine.Scripting.Lua.API (registerLuaAPI)
import Engine.Scripting.Lua.Thread (createLuaBackendState)
import Engine.Scripting.Lua.Thread.Console (executeDebugLua)
import Engine.Scripting.Lua.Types (LuaBackendState(..))
import World.Page.Types (WorldPageId(..))
import World.State.Types
    (WorldManager(..), WorldState(..), emptyWorldManager, emptyWorldState)

-- * Fixture

-- | The one page this spec installs and makes visible.
page ∷ WorldPageId
page = WorldPageId "blood_lua_api"

-- | The page's synthetic blood handle map, as
--   @(BloodTextureId, TextureHandle)@ pairs.
--
--   The handles deliberately DESCEND as the ids ascend, so the
--   documented "ascending by 'BloodTextureId'" ordering cannot be
--   satisfied by accident — insertion order, hash order and handle
--   order all disagree with it.
fixtureHandles ∷ [(Word32, Int)]
fixtureHandles = [(3, 41), (1, 47), (2, 44)]

-- | The handles the engine's texture-size cache is seeded with: id 1's
--   and id 2's, but NOT id 3's. 'World.Render.BloodQuads.disposeBloodRecord'
--   drops the bindless registration and the size entry SEPARATELY, so a
--   handle present in one registry and absent from the other is a real
--   state a leak check has to be able to see.
fixtureSized ∷ [Int]
fixtureSized = [47, 44]

-- | A handle belonging to no page and to no registry — what a caller
--   asking about an already-disposed resource holds.
strangerHandle ∷ Int
strangerHandle = 909

-- | Install the page, its blood handle map, and the texture-size cache.
--   The cleanup actions are @pure ()@: nothing here disposes anything,
--   and the verb never runs them.
resetScene ∷ EngineEnv → IO WorldState
resetScene env = do
    ws ← emptyWorldState
    writeIORef (wsBloodTextureHandlesRef ws) $ HM.fromList
        [ (BloodTextureId tid, (TextureHandle h, pure ()))
        | (tid, h) ← fixtureHandles ]
    writeIORef (worldManagerRef env) emptyWorldManager
        { wmWorlds = [(page, ws)], wmVisible = [page] }
    writeIORef (rvTextureSizeRef (toRenderViewCapability env)) $ HM.fromList
        [ (TextureHandle h, (16, 16)) | h ← fixtureSized ]
    pure ws

-- * Lua plumbing

newBareLuaBackend ∷ EngineEnv → IO LuaBackendState
newBareLuaBackend env = do
    ls ← createLuaBackendState (luaToEngineQueue env) (luaQueue env)
                                (assetPoolRef env) (nextObjectIdRef env)
                                (inputStateRef env) (loggerRef env)
    stateRef ← newIORef ThreadRunning
    registerLuaAPI (lbsLuaState ls) env ls stateRef
    pure ls

-- | @blood.gpuHandles(<argExpr>)@ folded to one console-returnable
--   string, because the debug console returns only the first value.
--
--   @"nil"@ is the verb's own nil answer; otherwise the result is
--   @"rows:"@ followed by one @id\/handle\/bindless\/texSize@ record per
--   row, comma separated, in the order the verb returned them. A row
--   with no @id@ field reads as the literal @"nil"@ in that position —
--   which is how the explicit form's documented absence of @id@ is
--   asserted rather than assumed.
gpuHandles ∷ LuaBackendState → Text → IO Text
gpuHandles ls argExpr = do
    r ← executeDebugLua (lbsLuaState ls) $ T.concat
        [ "local rows = blood.gpuHandles(", argExpr, "); "
        , "if rows == nil then return 'nil' end; "
        , "local out = {}; for _, r in ipairs(rows) do "
        , "out[#out+1] = tostring(r.id) .. '/' .. tostring(r.handle) "
        , ".. '/' .. tostring(r.bindless) .. '/' .. tostring(r.texSize) "
        , "end; return 'rows:' .. table.concat(out, ',')" ]
    pure (T.filter (≢ '"') (T.strip r))

-- | The page's blood handle map as sortable @(id, handle)@ pairs — the
--   live state the purity checks compare across a call.
liveHandles ∷ WorldState → IO [(Word32, Int)]
liveHandles ws = do
    known ← readIORef (wsBloodTextureHandlesRef ws)
    pure $ sort [ (unBloodTextureId tid, h)
                | (tid, (TextureHandle h, _)) ← HM.toList known ]

-- | The engine-wide texture-size cache, sorted.
liveSizes ∷ EngineEnv → IO [(Int, (Int, Int))]
liveSizes env = do
    sizes ← readIORef (rvTextureSizeRef (toRenderViewCapability env))
    pure $ sort [ (h, wh) | (TextureHandle h, wh) ← HM.toList sizes ]

-- | @#blood.listDecals()@ and @#blood.listTextures()@ on the active
--   page — the blood MODEL state, observed through the same registered
--   surface, so the purity check covers what a caller could see change.
storeSizes ∷ LuaBackendState → IO Text
storeSizes ls = do
    r ← executeDebugLua (lbsLuaState ls)
        "return #blood.listDecals() .. '/' .. #blood.listTextures()"
    pure (T.filter (≢ '"') (T.strip r))

spec ∷ SpecWith EngineEnv
spec = describe "Blood.LuaApi blood.gpuHandles (#1585)" $ do

    it "reports the active page's blood-owned handles, ascending by \
       \texture id, with per-registry membership" $ \env → do
        _  ← resetScene env
        ls ← newBareLuaBackend env
        -- id 1 -> handle 47 and id 2 -> handle 44 are in the size cache;
        -- id 3 -> handle 41 is not. bindless is false throughout: there
        -- is no bindless system headless, which is the documented
        -- reading, not a missing assertion.
        gpuHandles ls "" ⌦
            (`shouldBe` "rows:1/47/false/true,2/44/false/true,3/41/false/false")

    it "reports exactly the handles an explicit array names, in the \
       \given order, with no texture id" $ \env → do
        _  ← resetScene env
        ls ← newBareLuaBackend env
        -- Reverse order, and a handle belonging to no page at all --
        -- the post-teardown case the probe actually asks about.
        gpuHandles ls "{41, 47, 909}" ⌦
            (`shouldBe` "rows:nil/41/false/false,nil/47/false/true,\
                        \nil/909/false/false")

    it "answers about a handle whose page is gone, so a captured handle \
       \can be re-checked after teardown" $ \env → do
        ws ← resetScene env
        ls ← newBareLuaBackend env
        -- Capture, then remove the page exactly as a teardown does. The
        -- active-page form now reports nothing; the explicit form still
        -- answers, which is the whole reason it exists.
        before ← liveHandles ws
        before `shouldBe` [(1, 47), (2, 44), (3, 41)]
        writeIORef (worldManagerRef env) emptyWorldManager
        gpuHandles ls "" ⌦ (`shouldBe` "rows:")
        gpuHandles ls "{47, 44, 41}" ⌦
            (`shouldBe` "rows:nil/47/false/true,nil/44/false/true,\
                        \nil/41/false/false")
        -- ...and it goes false once the size entries are dropped, which
        -- is what a correct disposal leaves behind.
        writeIORef (rvTextureSizeRef (toRenderViewCapability env)) HM.empty
        gpuHandles ls "{47, 44, 41}" ⌦
            (`shouldBe` "rows:nil/47/false/false,nil/44/false/false,\
                        \nil/41/false/false")

    it "returns an empty array, not nil, for an empty selection" $ \env → do
        _  ← resetScene env
        ls ← newBareLuaBackend env
        gpuHandles ls "{}" ⌦ (`shouldBe` "rows:")

    it "returns nil for a malformed argument rather than a partial or \
       \empty answer" $ \env → do
        _  ← resetScene env
        ls ← newBareLuaBackend env
        -- A non-table, an associative table, a table whose array part is
        -- shadowed by extra keys, and an array holding a non-integer.
        gpuHandles ls "'47'"          ⌦ (`shouldBe` "nil")
        gpuHandles ls "47"            ⌦ (`shouldBe` "nil")
        gpuHandles ls "{a = 47}"      ⌦ (`shouldBe` "nil")
        gpuHandles ls "{47, a = 1}"   ⌦ (`shouldBe` "nil")
        gpuHandles ls "{47, 'x'}"     ⌦ (`shouldBe` "nil")
        gpuHandles ls "{47, {}}"      ⌦ (`shouldBe` "nil")

    it "observes only: no form of the call mutates the handle map, the \
       \size cache, or the blood store" $ \env → do
        ws ← resetScene env
        ls ← newBareLuaBackend env
        -- Give the store something to lose, through the real spawn verb.
        _ ← executeDebugLua (lbsLuaState ls)
            "local d = blood.spawn(4.5, 5.5, 'stab', 'severe', \
            \{style = 'pool', seed = 7}); return tostring(d ~= nil)"
        handlesBefore ← liveHandles ws
        sizesBefore   ← liveSizes env
        storeBefore   ← storeSizes ls
        handlesBefore `shouldBe` [(1, 47), (2, 44), (3, 41)]
        sizesBefore   `shouldBe` [(44, (16, 16)), (47, (16, 16))]
        storeBefore   `shouldBe` "1/1"

        -- Every form, including the two rejected ones.
        _ ← gpuHandles ls ""
        _ ← gpuHandles ls "{47, 44, 41}"
        _ ← gpuHandles ls (T.pack ("{" <> show strangerHandle <> "}"))
        _ ← gpuHandles ls "{}"
        _ ← gpuHandles ls "'nope'"
        _ ← gpuHandles ls "{a = 1}"

        liveHandles ws ⌦ (`shouldBe` handlesBefore)
        liveSizes env  ⌦ (`shouldBe` sizesBefore)
        storeSizes ls  ⌦ (`shouldBe` storeBefore)
