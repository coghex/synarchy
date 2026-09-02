-- | 'resolveTexturePath' (#478): a yaml-declared texture path that
--   resolves to a real file loads unchanged; one that doesn't falls back
--   to the caller's subset placeholder instead of the missing-file path
--   reaching the Vulkan loader (which throws 'TextureLoadFailed').
--
--   Also the home for the persistence contract's visual-fallback policy
--   (#767, requirement 12, @docs/persistence_contract.md@ SS4): a
--   missing equipment/subset-specific texture substitutes its caller's
--   own placeholder (proven above via 'resolveTexturePath' directly —
--   every YAML-loading call site in @Engine.Scripting.Lua.API.*@ passes
--   its OWN category fallback, e.g. @missing_equipment.png@ for items,
--   @notexture.png@ for materials, @unknown_flora.png@ for flora), and
--   any OTHER missing visual this build has no specialized placeholder
--   for falls through to the magenta/black checkerboard "undefined"
--   texture (below) -- never a load failure, and never something that
--   could invalidate an otherwise-valid save (contract SS4: "missing
--   visual assets never by themselves invalidate an otherwise coherent
--   save"). No terrain-SPECIFIC placeholder asset exists in this build
--   (a separately-tracked, out-of-scope asset-production gap per
--   contract SS4/SS8) -- a missing terrain texture falls through to this
--   SAME magenta-checkerboard policy, not a bespoke one.
module Test.Headless.Asset.TextureFallback (spec) where

import UPrelude
import Test.Hspec
import Control.Concurrent (forkIO)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, readMVar, takeMVar)
import Control.Exception (finally)
import Data.List (sort)
import Data.IORef (IORef, newIORef, readIORef, writeIORef, modifyIORef')
import qualified Data.HashMap.Strict as HM
import qualified Data.Map.Strict as Map
import System.Directory
    ( getTemporaryDirectory, createDirectoryIfMissing, removeDirectoryRecursive
    , doesFileExist )
import System.FilePath ((</>))
import qualified Data.Text as T
import qualified Data.Vector.Storable as Vec
import Engine.Core.Init (EngineInitResult(..))
import Test.Headless.Harness.Log (initializeEngineHeadlessQuiet)
import Engine.Core.State (EngineEnv, floraCatalogRef, luaToEngineQueue, luaQueue
    , assetPoolRef, nextObjectIdRef, inputStateRef, loggerRef
    , textureSystemRef, textureSizeRef)
import Engine.Core.Thread (ThreadControl(..))
import Engine.Scripting.Lua.API (registerLuaAPI)
import Engine.Scripting.Lua.API.YamlTextures (resolveTexturePath)
import Engine.Scripting.Lua.API.Units (unknownUnitTexture)
import Engine.Scripting.Lua.Thread (createLuaBackendState)
import Engine.Scripting.Lua.Thread.Console (executeDebugLua)
import Engine.Scripting.Lua.Types (LuaBackendState(..))
import Engine.Asset.Base (AssetId(..))
import Engine.Asset.Handle (AssetState(..), TextureHandle(..)
    , missingTextureHandle, firstAllocatableTextureHandle
    , isMissingTextureHandle, toInt)
import Engine.Asset.Manager (TextureHandleReservation(..)
    , generateTextureHandle, reserveTextureHandle)
import Engine.Asset.Types (AssetPool(..), AtlasMetadata(..), TextureAtlas(..)
    , defaultAssetPool)
import Engine.Core.Log
    ( LogBackend(..), LogConfig(..), LogEntry(..), LogLevel(..)
    , defaultLogConfig, initLogger )
import Engine.Core.Monad (EngineM', modifyGraphicsState, runEngineM)
import Engine.Graphics.Camera (CameraFacing(..))
import Engine.Graphics.Vulkan.Sampler.Types (SamplerKind(..))
import Engine.Graphics.Vulkan.Texture.Bindless
    ( HandleAddressing(..), TextureRegistrationFailure(..)
    , checkRegistrableHandle
    , defaultBindlessConfig, registerPinnedTexture, registerTexture
    , registerSlotOnlyTexture
    , writeHandleSlotEntryPtr, handleSlotTableSize )
import Engine.Graphics.Vulkan.Texture.Handle
    (BindlessTextureHandle(..), toBindlessHandle)
import Engine.Graphics.Vulkan.Texture.Policy (UploadSampler(..), TextureCacheKey(..))
import Engine.Graphics.Vulkan.Texture.Slot
    (TextureSlot(..), TextureSlotAllocator(..), createSlotAllocator)
import Engine.Graphics.Vulkan.Texture.Types (BindlessTextureSystem(..))
import Engine.Graphics.Vulkan.Texture.Undefined (undefinedTextureData)
import Engine.Graphics.Vulkan.Types.Texture (UndefinedTexture(..), VulkanImage(..))
import Engine.Scripting.Lua.Message.Texture
    (duplicateCachedTextureHandle, handleLoadTextureBatch)
import Engine.Scripting.Lua.Types (LuaMsg(..))
import Blood.Types (BloodStore(..), BloodStyle(..), BloodTextureRequest(..)
    , SeverityBucket(..), FootprintBucket(..), AnisotropyBucket(..)
    , EdgeBucket(..), emptyBloodStore, requestTexture)
import Engine.Core.State (GraphicsState(..), worldManagerRef)
import Engine.Graphics.Types (DevQueues(..))
import World.Page.Types (WorldPageId(..))
import World.Render.BloodQuads (bloodTextureSource, uploadBloodTextures)
import World.State.Types (WorldState(..), WorldManager(..)
    , emptyWorldState, emptyWorldManager)
import qualified Engine.Core.Queue as Q
import qualified Vulkan.Core10 as Vk
import Vulkan.Zero (zero)
import Engine.Graphics.Vulkan.Types.Vertex (Vertex(..), noFaceMapVertexId)
import Engine.Scene.Types (SortableQuad(..))
import World.Render.FloraQuads (floraToQuad)
import World.Render.Textures.Types (defaultWorldTextures)
import Unit.Direction (Direction(..))
import World.Flora.Types (FloraCatalog(..), FloraId(..), FloraSpecies(..)
    , FloraHarvest(..), FloraInstance(..), lookupSpecies)
import World.Flora.Identity (floraInstanceIdNone)

-- A real, always-present repo asset — stands in for both "the preferred
-- path" (existing case) and "the fallback path" (missing case) so the
-- test doesn't depend on any of the new #478 unknown-texture assets.
realAsset ∷ FilePath
realAsset = "assets/textures/ui/placeholders/missing_equipment.png"

spec ∷ SpecWith EngineEnv
spec = do
    describe "resolveTexturePath" $ do
        it "returns the preferred path when it exists" $ \env → do
            resolved ← resolveTexturePath env "Test" "assets/textures/does/not/exist.png" realAsset
            resolved `shouldBe` realAsset

        it "substitutes the fallback when the preferred path is missing" $ \env → do
            resolved ← resolveTexturePath env "Test" realAsset "assets/textures/does/not/exist.png"
            resolved `shouldBe` realAsset

    -- The DIRECT single-texture families (#478) — which is all of them
    -- since #1261 retired per-frame unit-animation loading along with
    -- #485's `unknownUnitAnimFrame`. An animation is one compiled
    -- atlas now, and a missing or unusable one rejects the unit
    -- definition outright (Unit.Atlas.Load) instead of substituting a
    -- placeholder, so there is no per-frame fallback left to cover.
    describe "unknownUnitTexture" $ do
        it "names one static rotation per compass direction" $ \_env → do
            unknownUnitTexture DirS
                `shouldBe` "assets/textures/units/unknown_unit/rotations/south.png"
            unknownUnitTexture DirNE
                `shouldBe` "assets/textures/units/unknown_unit/rotations/north-east.png"

        it "resolves to a file that actually ships" $ \_env → do
            exists ← doesFileExist (unknownUnitTexture DirN)
            exists `shouldBe` True

    -- Contract requirement 12's final fall-through: ANY missing visual
    -- with no specialized placeholder of its own (or a specialized
    -- placeholder this build hasn't produced yet) ends up here, never a
    -- load/render failure.
    describe "undefinedTextureData (magenta/black checkerboard, #767 requirement 12)" $ do
        it "is an 8x8 RGBA texture" $ \_env →
            Vec.length undefinedTextureData `shouldBe` 8 * 8 * 4

        it "is built ONLY from opaque magenta and opaque black texels" $ \_env → do
            let texels = [ Vec.slice (i * 4) 4 undefinedTextureData | i ← [0 .. 8 * 8 - 1] ]
                magenta = Vec.fromList [255, 0, 255, 255]
                black   = Vec.fromList [0, 0, 0, 255]
            all (\t → t ≡ magenta ∨ t ≡ black) texels `shouldBe` True

        it "actually alternates -- both colors are present, not a solid fill" $ \_env → do
            let texels = [ Vec.slice (i * 4) 4 undefinedTextureData | i ← [0 .. 8 * 8 - 1] ]
                magenta = Vec.fromList [255, 0, 255, 255]
                black   = Vec.fromList [0, 0, 0, 255]
            any (≡ magenta) texels `shouldBe` True
            any (≡ black) texels `shouldBe` True

    -- Round-2 review: the checkerboard-pixel tests above prove the
    -- fallback ASSET's own shape, but never exercise a real "missing
    -- specialized placeholder" asset-resolution route reaching it. The
    -- one such route this codebase actually implements (registerFloraSpecies,
    -- Engine.Scripting.Lua.API.YamlTextures) is a flora harvest whose YAML
    -- omits `harvested_texture` entirely: `fyhHarvestedTexture = Nothing`
    -- resolves DIRECTLY to `missingTextureHandle`, the missing-texture
    -- SENTINEL, with no file-path resolution attempt at all (never mind
    -- one that could fail).
    --
    -- #1696 corrected this comment's explanation: handle 0 is not slot 0.
    -- The handle is an id into the shader's handle->slot table; slot 0 is
    -- the undefined checkerboard's own reserved DESCRIPTOR index
    -- (Engine.Graphics.Vulkan.Texture.Slot allocates real slots from 1).
    -- A zero handle reaches the checkerboard because table entry 0 stays
    -- zero for the whole process lifetime -- which was NOT true before
    -- #1696, since handle 0 was itself allocatable and the first texture
    -- registered in a process overwrote that entry with its real slot.
    -- The assertion below was always right; only its reason was wrong.
    -- The reservation that now makes it true is covered by the group
    -- after this one.
    --
    -- This is the concrete, headless-reachable proof of contract
    -- requirement 12's "missing specialized placeholders fall through to
    -- magenta checkerboard": no GPU/render pipeline involved, so it holds
    -- regardless of whether this build can even open a window.
    describe "missing specialized-placeholder asset resolution (#767 \
             \requirement 12, round-2 review)" $ do
        it "a flora species with no harvested_texture resolves DIRECTLY \
           \to the missing-texture sentinel handle -- never a file-path \
           \resolution attempt that could itself fail" $ \_sharedEnv → do
            -- Round-7/8 review: engine.loadFloraYaml permanently mutates
            -- SHARED engine state well beyond what a `finally` can cleanly
            -- undo -- floraCatalogRef gains a real species + worldGen
            -- entry, and registering its ground texture allocates a
            -- handle in the asset pool and a name in the texture-name
            -- registry. Reverting an insert into all of those correctly
            -- (rather than just flushing the one queue message round-7
            -- caught) risks missing another one down the line. A private,
            -- lightweight `initializeEngineHeadlessQuiet` env (the same
            -- primitive Test.Headless.World.LocationDiscovery/CursorInfo/
            -- Unit.LineOfSight already use for exactly this "needs its
            -- own throwaway engine state" case, no world/unit thread
            -- spawned) sidesteps the whole class of gap: this test's
            -- mutations only ever touch its own engine, discarded whole
            -- when the test ends, and the aroundAll-shared `_sharedEnv`
            -- above is deliberately unused.
            EngineInitResult env ← initializeEngineHeadlessQuiet
            tmp ← getTemporaryDirectory
            let dir = tmp </> "synarchy-texture-fallback-spec"
                path = dir </> "no_harvest_texture.yaml"
            createDirectoryIfMissing True dir
            writeFile path noHarvestedTextureYaml
            (`finally` removeDirectoryRecursive dir) $ do
                idBefore ← fcNextId <$> readIORef (floraCatalogRef env)
                ls ← newBareLuaBackend env
                result ← evalDebug ls
                    ("return engine.loadFloraYaml('" <> pathToLua path <> "')")
                result `shouldSatisfy` (`elem` ["1", "1.0"])
                catalog ← readIORef (floraCatalogRef env)
                case lookupSpecies (FloraId idBefore) catalog of
                    Nothing → expectationFailure
                        "the newly-registered species is missing from the catalog"
                    Just species → case fsHarvest species of
                        Nothing → expectationFailure
                            "the species was registered with no harvest block at all"
                        Just harvest →
                            fhHarvestedTexture harvest `shouldBe` missingTextureHandle

    sentinelHandleSpec
    unrepresentableHandleSpec

-- | #1696: handle 0 is the missing-texture SENTINEL, so no real texture
--   may ever own it. The defect this pins was live in three layers at
--   once, and a regression in any one of them alone reproduces the
--   original symptom, so all three are checked here:
--
--   1. ALLOCATION — 'generateTextureHandle' seeded its counter at 0 and
--      returned it before incrementing, so a fresh process handed the
--      sentinel id to its FIRST texture.
--   2. REGISTRATION — 'registerTextureImpl' then wrote that texture's
--      real bindless slot into @handleToSlot[0]@, and the cached-alias
--      fast path in "Engine.Scripting.Lua.Message.Texture" could do the
--      same while bypassing registration entirely.
--   3. THE PRODUCER — every flora quad passed a literal zero handle as
--      its face-map id, so flora took its lighting weights from whichever
--      texture happened to win the race for handle 0 instead of from
--      @fragDefaultFaceMapSlot@.
--
--   Everything below drives PRODUCTION definitions: the real allocator on
--   a real 'defaultAssetPool', the guard both registration paths run
--   ('checkRegistrableHandle') and the message they log
--   ('registrationFailureMessage'), the whole body of
--   'writeHandleSlotEntry' ('writeHandleSlotEntryPtr', which it is
--   defined as) against real table memory, and 'floraToQuad' itself.
sentinelHandleSpec ∷ SpecWith EngineEnv
sentinelHandleSpec = do
    describe "reserved missing-texture sentinel handle (#1696)" $ do

        -- Layer 1: allocation.
        it "a fresh asset pool's FIRST texture handle is 1, not the sentinel" $ \_env → do
            pool ← defaultAssetPool
            first ← generateTextureHandle pool
            first `shouldBe` TextureHandle firstAllocatableTextureHandle
            first `shouldNotBe` missingTextureHandle

        it "keeps handing out dense, monotonic, non-sentinel ids after that" $ \_env → do
            pool ← defaultAssetPool
            handles ← replicateM 8 (generateTextureHandle pool)
            map toInt handles `shouldBe` [1 .. 8]
            any isMissingTextureHandle handles `shouldBe` False

        -- Layer 2a: the guard both registration paths share, exercised
        -- BY those paths.
        it "the shared registration guard admits every handle the \
           \allocator hands out" $ \_env → do
            pool ← defaultAssetPool
            handles ← replicateM 8 (generateTextureHandle pool)
            map (checkRegistrableHandle ShaderAddressable) handles
                `shouldBe` replicate 8 (Right ())

        it "registerTexture refuses the sentinel, logging the zero handle \
           \and the caller's provenance, and mutates NOTHING" $ \_env →
            withRegistrationFixture $ \fx → do
                (outcome, entries) ← runRegistration fx $ \system →
                    registerTexture zero missingTextureHandle
                        "assets/textures/ui/blank.png" zero zero system
                expectSentinelRefusal fx outcome entries
                    "assets/textures/ui/blank.png"

        it "registerPinnedTexture refuses it on the same terms -- a \
           \pinned sampler is no way past the guard" $ \_env →
            withRegistrationFixture $ \fx → do
                (outcome, entries) ← runRegistration fx $ \system →
                    registerPinnedTexture zero missingTextureHandle
                        "zoom atlas" zero zero system
                expectSentinelRefusal fx outcome entries "zoom atlas"

        it "a handle already registered still short-circuits to its \
           \existing bindless handle, silently" $ \_env →
            withRegistrationFixture $ \fx → do
                (outcome, entries) ← runRegistration fx $ \system →
                    registerTexture zero residentHandle "already resident"
                        zero zero system
                fst outcome `shouldBe` Right residentBindlessHandle
                map leMessage entries `shouldBe` []

        it "a genuinely exhausted slot allocator reports EXHAUSTION, not \
           \a reserved handle -- the two diagnostics never merge" $ \_env →
            withRegistrationFixture $ \fx → do
                -- maxSlots 1 leaves nextSlot 1 with nothing to hand out,
                -- so this reaches 'refuse' through the allocator rather
                -- than through the sentinel guard -- Vulkan-free, like
                -- the refusals above.
                (outcome, entries) ← runRegistrationWith fx
                    (\sys → sys { btsSlotAllocator = createSlotAllocator 1 })
                    (\system → registerTexture zero (TextureHandle 42)
                        "assets/textures/ui/blank.png" zero zero system)
                fst outcome `shouldBe` Left TextureSlotsExhausted
                message ← soleWarning entries
                message `shouldSatisfy` T.isInfixOf "Failed to allocate bindless slot"
                message `shouldSatisfy` T.isInfixOf "handle 42"
                message `shouldNotSatisfy` T.isInfixOf "sentinel"

        -- Layer 2b: the low-level table mutation every path funnels through.
        it "the handle->slot table keeps entry 0 on the undefined slot, \
           \however a nonzero write reaches it" $ \_env →
            withHandleSlotTable $ \table → do
                -- An ordinary handle writes through normally.
                writeHandleSlotEntryPtr table 1 7
                peekElemOff table 1 `shouldReturn` 7
                -- The sentinel's entry refuses a real slot...
                writeHandleSlotEntryPtr table (toInt missingTextureHandle) 7
                peekElemOff table (toInt missingTextureHandle) `shouldReturn` 0
                -- ...but still accepts the zero-CLEARING write that
                -- unregisterTexture / releaseTextureHandles perform.
                writeHandleSlotEntryPtr table (toInt missingTextureHandle) 0
                peekElemOff table (toInt missingTextureHandle) `shouldReturn` 0
                -- The pre-existing out-of-range guard is unchanged: the
                -- default face map's deliberately out-of-table id is
                -- dropped rather than clobbering a neighbour.
                writeHandleSlotEntryPtr table handleSlotTableSize 9
                writeHandleSlotEntryPtr table 999999 9
                writeHandleSlotEntryPtr table (-1) 9
                peekElemOff table 0 `shouldReturn` 0
                peekElemOff table 1 `shouldReturn` 7

        -- Layer 2c: the OTHER path that can insert into btsHandleMap.
        it "the cached-alias fast path refuses the sentinel, logging the \
           \zero handle and the atlas path, and mutates NOTHING" $ \_env →
            withRegistrationFixture $ \fx → do
                before ← aliasObservables fx
                entries ← runAlias fx missingTextureHandle
                after ← aliasObservables fx
                after `shouldBe` before
                message ← soleWarning entries
                message `shouldSatisfy` T.isInfixOf "handle 0"
                message `shouldSatisfy` T.isInfixOf cachedAtlasPath
                message `shouldSatisfy` T.isInfixOf "handleToSlot[0]"
                message `shouldNotSatisfy`
                    T.isInfixOf "Failed to allocate bindless slot"
                peekTable fx (toInt missingTextureHandle) `shouldReturn` 0

        it "...while an ordinary alias handle DOES take the slot and all \
           \the bookkeeping, so the case above is a real refusal" $ \_env →
            withRegistrationFixture $ \fx → do
                entries ← runAlias fx aliasHandle
                map leMessage entries `shouldBe` []
                mSystem ← readIORef (textureSystemRef (rfEnv fx))
                case mSystem of
                    Nothing → expectationFailure "the bindless system vanished"
                    Just system →
                        Map.lookup aliasHandle (btsHandleMap system)
                            `shouldBe` Just residentBindlessHandle
                peekTable fx (toInt aliasHandle)
                    `shouldReturn` tsIndex (bthSlot residentBindlessHandle)
                sizes ← readIORef (textureSizeRef (rfEnv fx))
                HM.lookup aliasHandle sizes `shouldBe` Just (4, 4)
                pool ← readIORef (assetPoolRef (rfEnv fx))
                Map.member aliasHandle <$> readIORef (apTextureHandles pool)
                    `shouldReturn` True
                queued ← Q.flushQueue (luaQueue (rfEnv fx))
                length queued `shouldBe` 1

        -- Layer 2d: the public entry point both registration paths sit
        -- behind. Refusing at registration alone is too late -- the
        -- upload fold records asset bookkeeping for any prep that
        -- produced no bindless handle.
        it "the batch upload entry point drops a sentinel request before \
           \any upload or asset bookkeeping, and still serves the rest \
           \of the batch" $ \_env →
            withRegistrationFixture $ \fx → do
                seedPathCache fx
                -- The sentinel deliberately names an UNCACHED path, so
                -- without the entry-point filter it would be classified
                -- as a fresh upload rather than caught downstream by the
                -- alias path's own guard. That is what makes this case
                -- the filter's own discriminator: headless there is no
                -- Vulkan device, so an unfiltered sentinel reaches the
                -- "Cannot batch-load textures" branch and the single
                -- WARN below stops naming handle 0 at all.
                let freshPath = "assets/textures/ui/not_in_the_cache.png"
                    action ∷ EngineM' ()
                    action = handleLoadTextureBatch UploadGlobalSampler
                        [ (missingTextureHandle, freshPath)
                        , (aliasHandle,          T.unpack cachedAtlasPath)
                        ]
                _ ← either (fail ∘ show) pure
                        =≪ runEngineM action (rfEnv fx) pure
                entries ← drainLog fx

                -- The sentinel is refused, named, and left with nothing.
                message ← soleWarning entries
                message `shouldSatisfy` T.isInfixOf "handle 0"
                message `shouldSatisfy` T.isInfixOf (T.pack freshPath)
                message `shouldNotSatisfy` T.isInfixOf "Vulkan not ready"
                pool ← readIORef (assetPoolRef (rfEnv fx))
                states ← readIORef (apTextureHandles pool)
                Map.member missingTextureHandle states `shouldBe` False
                sizes ← readIORef (textureSizeRef (rfEnv fx))
                HM.member missingTextureHandle sizes `shouldBe` False
                peekTable fx (toInt missingTextureHandle) `shouldReturn` 0
                mSystem ← readIORef (textureSystemRef (rfEnv fx))
                (Map.member missingTextureHandle ∘ btsHandleMap)
                    <$> mSystem `shouldBe` Just False

                -- The ordinary handle beside it was served in full, so
                -- the drop is targeted rather than a batch-wide bail-out.
                Map.member aliasHandle states `shouldBe` True
                HM.member aliasHandle sizes `shouldBe` True
                (Map.member aliasHandle ∘ btsHandleMap) <$> mSystem
                    `shouldBe` Just True
                queued ← Q.flushQueue (luaQueue (rfEnv fx))
                length queued `shouldBe` 1

        -- Layer 3: the producer that made the defect visible.
        it "every vertex floraToQuad emits carries the canonical \
           \no-face-map marker, never a texture handle" $ \_env → do
            let inst = FloraInstance
                    { fiSpecies   = FloraId 0
                    , fiTileX     = 0
                    , fiTileY     = 0
                    , fiOffU      = 0
                    , fiOffV      = 0
                    , fiZ         = 0
                    , fiAge       = 1
                    , fiHealth    = 1
                    , fiVariant   = 0
                    , fiBaseWidth = 8
                    , fiInstanceId = floraInstanceIdNone
                    , fiChopDesignated = False
                    }
                lookupSlot = toInt
            case floraToQuad lookupSlot defaultWorldTextures FaceSouth
                     0 0 inst (TextureHandle 3) 0 8 1.0 (0, 0) HM.empty of
                Nothing → expectationFailure
                    "floraToQuad culled a quad on its own z-slice"
                Just quad → do
                    let fms = map faceMapId
                                [sqV0 quad, sqV1 quad, sqV2 quad, sqV3 quad]
                    fms `shouldBe` replicate 4 noFaceMapVertexId
                    -- The old value. It is a real, allocatable-looking id
                    -- that the shader resolves through the table, which is
                    -- exactly how flora ended up shaded by a stranger.
                    fms `shouldNotBe` replicate 4 (0 ∷ Float)

-- | Handle ids the shader's handle→slot table cannot resolve (#1699).
--
--   The defect: the table covers ids @[0, handleSlotTableSize)@ and
--   'writeHandleSlotEntry' silently DROPS a write outside it, while
--   'registerTextureImpl' went on to allocate a slot, write the
--   descriptor and answer @Right@. Every publishing caller then reported
--   the texture loaded — @AssetReady@, both pool caches, the size entry,
--   @LuaAssetLoaded@ — for a handle every vertex resolves to slot 0, the
--   undefined checkerboard. The handle namespace is monotonic with no
--   reset anywhere in the tree, so that is permanent for the rest of the
--   process rather than a transient shortage.
--
--   The fix is one guard at the registration boundary, so everything
--   here drives PRODUCTION definitions: the real allocator on a real
--   'defaultAssetPool', the guard itself, both registration entry points
--   and the slot-only one, the cached-alias fast path, and the batch
--   entry point Lua actually calls.
--
--   Device-free throughout for the same reason #1696's cases are: a
--   REFUSED registration returns before 'writeDescriptorSlot', and the
--   admission cases below are steered into the ALLOCATOR's refusal
--   instead, which is equally Vulkan-free. That is also what makes them
--   mutation-proof: neutralising the range half of
--   'checkRegistrableHandle' makes the refusal cases reach the null
--   device, and neutralising the ordering (checking after
--   'allocateSlot') flips the two boundary cases' diagnoses.
unrepresentableHandleSpec ∷ SpecWith EngineEnv
unrepresentableHandleSpec = do
    describe "texture handle ids the shader cannot resolve (#1699)" $ do

        -- The boundary itself, against the REAL allocator.
        it "the allocator walks straight off the end of the table, and \
           \the guard admits the last id and refuses the first one past \
           \it" $ \_env → do
            pool ← defaultAssetPool
            writeIORef (apNextTextureHandle pool) (handleSlotTableSize - 1)
            lastInTable ← generateTextureHandle pool
            firstPast   ← generateTextureHandle pool
            toInt lastInTable `shouldBe` handleSlotTableSize - 1
            toInt firstPast   `shouldBe` handleSlotTableSize
            checkRegistrableHandle ShaderAddressable lastInTable
                `shouldBe` Right ()
            checkRegistrableHandle ShaderAddressable firstPast
                `shouldBe` Left TextureHandleUnrepresentable

        it "reserves the last representable id, then reports the \
           \namespace spent to ONE claimant and never un-spends it" $ \_env → do
            pool ← defaultAssetPool
            writeIORef (apNextTextureHandle pool) (handleSlotTableSize - 1)
            reserveTextureHandle pool `shouldReturn`
                TextureHandleAllocated (TextureHandle (handleSlotTableSize - 1))
            -- The first caller past the boundary is handed the id that
            -- WOULD have been allocated, so its report can name it.
            reserveTextureHandle pool `shouldReturn`
                TextureHandlesSpent (Just (TextureHandle handleSlotTableSize))
            -- Everyone after it is told the same permanent answer with
            -- nothing to say about it, however many times they ask.
            reserveTextureHandle pool
                `shouldReturn` TextureHandlesSpent Nothing
            reserveTextureHandle pool
                `shouldReturn` TextureHandlesSpent Nothing
            -- A refusal allocates nothing: the counter stopped at the
            -- first id it could not hand out.
            readIORef (apNextTextureHandle pool)
                `shouldReturn` handleSlotTableSize

        it "refuses an id another allocator took first, rather than \
           \handing back one it had already judged safe" $ \_env → do
            -- The shared allocator: the Lua texture worker, the two
            -- transients and the blood diff all draw from this one
            -- pool. With one id left, whoever gets there first takes it
            -- and everyone else must be refused.
            pool ← defaultAssetPool
            writeIORef (apNextTextureHandle pool) (handleSlotTableSize - 1)
            stolen ← generateTextureHandle pool
            stolen `shouldBe` TextureHandle (handleSlotTableSize - 1)
            reserveTextureHandle pool `shouldReturn`
                TextureHandlesSpent (Just (TextureHandle handleSlotTableSize))

        it "hands out every representable id exactly ONCE under real \
           \contention, and never one past the cap" $ \_env → do
            -- The interleaving the atomicity exists for, run for real
            -- rather than argued: the decision and the allocation are
            -- one 'atomicModifyIORef'', so no two claimants racing
            -- across the boundary can both be told the same id is free.
            -- A reserve that read the counter and then allocated
            -- separately can be overtaken in that window and hand back
            -- a duplicate, or an id past the cap it had already judged
            -- safe -- either of which breaks the first assertion here.
            -- Rounds, not one shot: the window a non-atomic reserve
            -- leaves open is a handful of instructions wide, so a
            -- single race can miss it. Each round is a fresh pool with
            -- exactly ONE id left and many askers, which is the maximum
            -- pressure on the boundary.
            forM_ [1 .. 200 ∷ Int] $ \_round → do
                pool ← defaultAssetPool
                let room   = 1
                    askers = 24
                writeIORef (apNextTextureHandle pool)
                    (handleSlotTableSize - room)
                start ← newEmptyMVar
                slots ← replicateM askers newEmptyMVar
                forM_ slots $ \slot → forkIO $ do
                    () ← readMVar start
                    putMVar slot =≪ reserveTextureHandle pool
                putMVar start ()
                results ← mapM takeMVar slots

                let allocated = [ h | TextureHandleAllocated h ← results ]
                    claimed   = [ h | TextureHandlesSpent (Just h) ← results ]
                -- Exactly the representable ids, each to exactly one
                -- asker -- so none was double-issued, and none was past
                -- the cap.
                sort (map toInt allocated) `shouldBe`
                    [handleSlotTableSize - room .. handleSlotTableSize - 1]
                map (checkRegistrableHandle ShaderAddressable) allocated
                    `shouldBe` replicate room (Right ())
                -- Everyone else was refused, and the counter stopped
                -- dead at the first id it could not hand out.
                length results - length allocated `shouldBe` askers - room
                readIORef (apNextTextureHandle pool)
                    `shouldReturn` handleSlotTableSize
                -- One report between all of them, whoever won it.
                length claimed `shouldBe` 1

        it "refuses an out-of-table id only where the SHADER reads the \
           \table, never for a slot-only registration" $ \_env → do
            -- The default face map's deliberately out-of-table id
            -- ('DefaultFaceMap'): shader-addressable it is unusable,
            -- slot-only it is fine, because its slot reaches the shader
            -- through the UBO instead.
            let faceMap = TextureHandle 999999
            checkRegistrableHandle ShaderAddressable faceMap
                `shouldBe` Left TextureHandleUnrepresentable
            checkRegistrableHandle SlotOnly faceMap `shouldBe` Right ()
            -- The sentinel is refused under BOTH: slot-only is an
            -- exemption from the RANGE half alone (#1696 still holds).
            checkRegistrableHandle SlotOnly missingTextureHandle
                `shouldBe` Left TextureHandleReserved
            -- A negative id is out of the table just as surely.
            checkRegistrableHandle ShaderAddressable (TextureHandle (-1))
                `shouldBe` Left TextureHandleUnrepresentable

        -- The registration boundary, and the ORDER its two refusals
        -- run in.
        it "registerTexture refuses the first unrepresentable id, \
           \logging the id and the cap, and mutates NOTHING" $ \_env →
            withRegistrationFixture $ \fx → do
                (outcome, entries) ← runRegistration fx $ \system →
                    registerTexture zero (TextureHandle handleSlotTableSize)
                        "assets/textures/ui/blank.png" zero zero system
                expectUnrepresentableRefusal fx outcome entries
                    "assets/textures/ui/blank.png"

        it "registerPinnedTexture refuses it on the same terms" $ \_env →
            withRegistrationFixture $ \fx → do
                (outcome, entries) ← runRegistration fx $ \system →
                    registerPinnedTexture zero (TextureHandle 999999)
                        "zoom atlas" zero zero system
                expectUnrepresentableRefusal fx outcome entries "zoom atlas"

        it "detects it BEFORE the slot allocator, so the two capacity \
           \stories never merge" $ \_env →
            withRegistrationFixture $ \fx → do
                -- maxSlots 1 leaves nothing to hand out, so whichever
                -- guard the handle reaches first decides the answer.
                -- The LAST in-table id gets past the range guard and
                -- reports exhaustion; the FIRST one past the table is
                -- refused before the allocator is consulted at all.
                (admitted, _) ← runRegistrationWith fx
                    (\sys → sys { btsSlotAllocator = createSlotAllocator 1 })
                    (\system → registerTexture zero
                        (TextureHandle (handleSlotTableSize - 1))
                        "assets/textures/ui/blank.png" zero zero system)
                fst admitted `shouldBe` Left TextureSlotsExhausted
                (refused, entries) ← runRegistrationWith fx
                    (\sys → sys { btsSlotAllocator = createSlotAllocator 1 })
                    (\system → registerTexture zero
                        (TextureHandle handleSlotTableSize)
                        "assets/textures/ui/blank.png" zero zero system)
                fst refused `shouldBe` Left TextureHandleUnrepresentable
                message ← soleWarning entries
                message `shouldNotSatisfy`
                    T.isInfixOf "Failed to allocate bindless slot"

        it "still admits the default face map's out-of-table id through \
           \the slot-only entry point, so its bootstrap is untouched" $ \_env →
            withRegistrationFixture $ \fx → do
                -- Reaching the exhausted allocator is the proof it got
                -- PAST the range guard; refusing it here would drop
                -- 'dfmSlot' to the undefined slot 0.
                (outcome, entries) ← runRegistrationWith fx
                    (\sys → sys { btsSlotAllocator = createSlotAllocator 1 })
                    (\system → registerSlotOnlyTexture zero
                        (TextureHandle 999999) "default face map"
                        zero zero system)
                fst outcome `shouldBe` Left TextureSlotsExhausted
                message ← soleWarning entries
                message `shouldSatisfy`
                    T.isInfixOf "Failed to allocate bindless slot"

        -- The PER-FRAME registration path, whose diff would otherwise
        -- repeat the whole upload-and-refuse cycle forever.
        it "reports a spent namespace ONCE from the blood upload path, \
           \and uploads, registers and records nothing" $ \_env →
            withRegistrationFixture $ \fx → do
                let env = rfEnv fx
                ws ← seedBloodWorld env
                spendTextureHandles fx 0

                -- Two frames. The device handles are all null, which is
                -- exactly the point: the refusal must return before
                -- 'createTextureFromRGBABytes' or 'registerTexture'
                -- touches any of them, so reaching either one fails this
                -- case rather than passing it.
                runFrame fx
                first ← drainLog fx
                runFrame fx
                second ← drainLog fx

                -- Reported once, naming the kind, the refused id and the
                -- cap -- a procedural texture has no requested path.
                message ← soleWarning first
                message `shouldSatisfy` T.isInfixOf bloodTextureSource
                message `shouldSatisfy`
                    namesHandle (TextureHandle handleSlotTableSize)
                message `shouldSatisfy` T.isInfixOf (T.pack (show handleSlotTableSize))
                message `shouldSatisfy` T.isInfixOf "#1699"
                -- ...and never again, however many frames run.
                map leMessage (filter ((≡ LevelWarn) ∘ leLevel) second)
                    `shouldBe` []

                -- Nothing recorded for it, either frame: no handle entry
                -- (so nothing draws it) and no size entry.
                HM.null <$> readIORef (wsBloodTextureHandlesRef ws)
                    `shouldReturn` True
                sizes ← readIORef (textureSizeRef env)
                HM.null sizes `shouldBe` True
                -- The namespace was not advanced either: a refusal that
                -- allocated per frame would still be spending ids.
                pool ← readIORef (assetPoolRef env)
                readIORef (apNextTextureHandle pool)
                    `shouldReturn` handleSlotTableSize

        it "...and reports it once even when a concurrent allocator \
           \takes the last id out from under the frame" $ \_env →
            withRegistrationFixture $ \fx → do
                let env = rfEnv fx
                ws ← seedBloodWorld env
                -- One id left, and the Lua texture worker -- which draws
                -- from this same pool -- takes it before the frame runs.
                spendTextureHandles fx 1
                pool ← readIORef (assetPoolRef env)
                stolen ← generateTextureHandle pool
                stolen `shouldBe` TextureHandle (handleSlotTableSize - 1)

                runFrame fx
                first ← drainLog fx
                runFrame fx
                second ← drainLog fx

                -- A non-atomic reserve would have seen room, taken the
                -- id past the cap, uploaded it against the null device
                -- and then reported AGAIN the next frame.
                message ← soleWarning first
                message `shouldSatisfy` T.isInfixOf bloodTextureSource
                message `shouldSatisfy`
                    namesHandle (TextureHandle handleSlotTableSize)
                map leMessage (filter ((≡ LevelWarn) ∘ leLevel) second)
                    `shouldBe` []
                HM.null <$> readIORef (wsBloodTextureHandlesRef ws)
                    `shouldReturn` True
                readIORef (apNextTextureHandle pool)
                    `shouldReturn` handleSlotTableSize

        -- The OTHER 'btsHandleMap' insertion path.
        it "the cached-alias fast path refuses an unrepresentable id, \
           \writes none of a hit's bookkeeping, and SETTLES the request" $ \_env →
            withRegistrationFixture $ \fx → do
                before ← hitObservables fx
                let handle = TextureHandle handleSlotTableSize
                entries ← runAlias fx handle
                -- Nothing a HIT writes: no handle map entry, no table
                -- poke, no atlas refcount bump, no size entry. The
                -- terminal failure below is the only thing it does
                -- write, which is exactly what separates #1699's
                -- refusal from #1696's silent drop.
                after ← hitObservables fx
                after `shouldBe` before
                peekTable fx (toInt missingTextureHandle) `shouldReturn` 0
                message ← soleWarning entries
                message `shouldSatisfy` T.isInfixOf (T.pack (show handleSlotTableSize))
                message `shouldSatisfy` T.isInfixOf cachedAtlasPath
                message `shouldSatisfy` T.isInfixOf "#1699"
                -- ...but unlike the sentinel, this is a REAL request, so
                -- it ends terminally instead of being dropped: nothing
                -- waiting on it stalls.
                expectTerminalFailure fx handle cachedAtlasPath

        it "...while the LAST in-table id takes the hit and all of the \
           \bookkeeping, so the case above is a real refusal" $ \_env →
            withRegistrationFixture $ \fx → do
                let handle = TextureHandle (handleSlotTableSize - 1)
                entries ← runAlias fx handle
                map leMessage entries `shouldBe` []
                mSystem ← readIORef (textureSystemRef (rfEnv fx))
                (Map.lookup handle ∘ btsHandleMap) <$> mSystem
                    `shouldBe` Just (Just residentBindlessHandle)
                peekTable fx (toInt handle)
                    `shouldReturn` tsIndex (bthSlot residentBindlessHandle)
                sizes ← readIORef (textureSizeRef (rfEnv fx))
                HM.lookup handle sizes `shouldBe` Just (4, 4)
                queued ← Q.flushQueue (luaQueue (rfEnv fx))
                map assetEvent queued `shouldBe` [Just (True, toInt handle)]

        -- The in-batch DEDUP lane, which is where a refused request
        -- could inherit another handle's diagnostic.
        it "settles two FRESH same-path requests separately, each on the \
           \handle it names" $ \_env →
            withRegistrationFixture $ \fx → do
                -- Deliberately an UNCACHED path, so without the
                -- entry-point judgement the first request becomes the
                -- canonical fresh upload and the second is folded into
                -- it as an in-batch alias -- the alias then taking the
                -- canonical's reason, which names the canonical's id.
                -- Judged per request, neither ever reaches that lane.
                let freshPath = "assets/textures/ui/not_in_the_cache.png"
                    canonical = TextureHandle handleSlotTableSize
                    alias     = TextureHandle (handleSlotTableSize + 1)
                    action ∷ EngineM' ()
                    action = handleLoadTextureBatch UploadGlobalSampler
                        [ (canonical, freshPath), (alias, freshPath) ]
                _ ← either (fail ∘ show) pure
                        =≪ runEngineM action (rfEnv fx) pure
                entries ← drainLog fx

                -- Two refusals, two diagnostics, each naming its own id
                -- and neither naming the other's.
                let warnings = map leMessage
                        (filter ((≡ LevelWarn) ∘ leLevel) entries)
                    canonicalId = namesHandle canonical
                    aliasId     = namesHandle alias
                length warnings `shouldBe` 2
                case warnings of
                    [first, second] → do
                        first `shouldSatisfy` canonicalId
                        first `shouldNotSatisfy` aliasId
                        second `shouldSatisfy` aliasId
                        second `shouldNotSatisfy` canonicalId
                        -- Never mistaken for the batch giving up: with
                        -- no request left, no upload was attempted.
                        map (T.isInfixOf "Vulkan not ready") warnings
                            `shouldBe` [False, False]
                    _ → expectationFailure "expected exactly two warnings"

                -- Both settle terminally, on their own reason.
                expectTerminalFailure fx canonical (T.pack freshPath)
                expectTerminalFailure fx alias (T.pack freshPath)
                pool ← readIORef (assetPoolRef (rfEnv fx))
                states ← readIORef (apTextureHandles pool)
                case (Map.lookup canonical states, Map.lookup alias states) of
                    (Just (AssetFailed a), Just (AssetFailed b)) → do
                        a `shouldSatisfy` canonicalId
                        a `shouldNotSatisfy` aliasId
                        b `shouldSatisfy` aliasId
                        b `shouldNotSatisfy` canonicalId
                    other → expectationFailure $
                        "expected both requests to settle, got " ⧺ show other

                -- Each Lua notification carries its own handle AND its
                -- own reason: this is the pairing that had drifted.
                queued ← Q.flushQueue (luaQueue (rfEnv fx))
                let failures = [ (h, reason)
                               | LuaAssetFailed _ h _ reason ← queued ]
                map fst failures
                    `shouldBe` [toInt canonical, toInt alias]
                forM_ failures $ \(h, reason) →
                    reason `shouldSatisfy` namesHandle (TextureHandle h)
                map assetEvent queued `shouldBe`
                    [ Just (False, toInt canonical), Just (False, toInt alias) ]

                -- Nothing else was written for either of them.
                sizes ← readIORef (textureSizeRef (rfEnv fx))
                HM.member canonical sizes `shouldBe` False
                HM.member alias sizes `shouldBe` False
                Map.member
                    (TextureCacheKey (T.pack freshPath) UploadGlobalSampler)
                    (apAssetPaths pool)
                    `shouldBe` False

        -- The public entry point Lua reaches, end to end.
        it "the batch entry point settles an unrepresentable request and \
           \still serves the rest of the batch" $ \_env →
            withRegistrationFixture $ \fx → do
                seedPathCache fx
                -- Both name the CACHED path, so both take the alias lane
                -- and neither needs a Vulkan device.
                let spent = TextureHandle handleSlotTableSize
                    action ∷ EngineM' ()
                    action = handleLoadTextureBatch UploadGlobalSampler
                        [ (spent,       T.unpack cachedAtlasPath)
                        , (aliasHandle, T.unpack cachedAtlasPath) ]
                _ ← either (fail ∘ show) pure
                        =≪ runEngineM action (rfEnv fx) pure
                _ ← drainLog fx

                expectTerminalFailure fx spent cachedAtlasPath
                sizes ← readIORef (textureSizeRef (rfEnv fx))
                HM.member spent sizes `shouldBe` False
                mSystem ← readIORef (textureSystemRef (rfEnv fx))
                (Map.member spent ∘ btsHandleMap) <$> mSystem
                    `shouldBe` Just False

                -- The ordinary handle beside it was served in full.
                pool ← readIORef (assetPoolRef (rfEnv fx))
                states ← readIORef (apTextureHandles pool)
                Map.member aliasHandle states `shouldBe` True
                HM.member aliasHandle sizes `shouldBe` True
                queued ← Q.flushQueue (luaQueue (rfEnv fx))
                map assetEvent queued `shouldBe`
                    [ Just (False, toInt spent), Just (True, toInt aliasHandle) ]

-- | Everything a cache HIT writes, and nothing else: the bindless
--   handle map, the sentinel's table entry, the atlas refcounts and the
--   texture-size map.
--
--   Deliberately narrower than 'aliasObservables': #1699's refusal DOES
--   settle the request on 'AssetFailed' and DOES queue its own
--   notification, so those two are asserted separately rather than
--   frozen here.
hitObservables ∷ RegistrationFixture
               → IO ( Map.Map TextureHandle BindlessTextureHandle
                    , Word32, [Word32], Int )
hitObservables fx = do
    let env = rfEnv fx
    mSystem ← readIORef (textureSystemRef env)
    entry0 ← peekTable fx (toInt missingTextureHandle)
    pool ← readIORef (assetPoolRef env)
    sizes ← readIORef (textureSizeRef env)
    pure ( maybe Map.empty btsHandleMap mSystem
         , entry0
         , map taRefCount (Map.elems (apTextureAtlases pool))
         , HM.size sizes )

-- | A refused registration: the failure, an untouched system, an
--   untouched table, and one log line naming the id, the provenance and
--   the cap that refused it.
expectUnrepresentableRefusal
    ∷ HasCallStack
    ⇒ RegistrationFixture
    → ( Either TextureRegistrationFailure BindlessTextureHandle
      , BindlessTextureSystem )
    → [LogEntry]
    → Text
    → Expectation
expectUnrepresentableRefusal fx (outcome, system) entries source = do
    outcome `shouldBe` Left TextureHandleUnrepresentable
    btsHandleMap system `shouldBe` Map.singleton residentHandle residentBindlessHandle
    btsImageViews system `shouldBe` Map.singleton residentHandle zero
    btsPinned system `shouldBe` Map.empty
    tsaNextSlot (btsSlotAllocator system) `shouldBe` 1
    peekTable fx (toInt missingTextureHandle) `shouldReturn` 0
    message ← soleWarning entries
    message `shouldSatisfy` T.isInfixOf source
    message `shouldSatisfy` T.isInfixOf (T.pack (show handleSlotTableSize))
    message `shouldSatisfy` T.isInfixOf "#1699"
    -- Never either OTHER reason a registration yields no handle.
    message `shouldNotSatisfy` T.isInfixOf "Failed to allocate bindless slot"
    message `shouldNotSatisfy` T.isInfixOf "sentinel"

-- | One loaded world page holding exactly one blood-texture descriptor
--   and nothing uploaded yet, registered with the engine's world
--   manager so 'uploadBloodTextures' reaches it.
seedBloodWorld ∷ EngineEnv → IO WorldState
seedBloodWorld env = do
    ws ← emptyWorldState
    let (pool, _tid, _isNew, _evicted) =
            requestTexture bloodRequest (bstPool (emptyBloodStore 8))
    writeIORef (wsBloodStoreRef ws) (BloodStore pool (bstDecals (emptyBloodStore 8)))
    writeIORef (worldManagerRef env) emptyWorldManager
        { wmWorlds = [(WorldPageId "blood_page", ws)] }
    pure ws

bloodRequest ∷ BloodTextureRequest
bloodRequest = BloodTextureRequest
    { btrStyle      = StylePool
    , btrWoundKind  = "stab"
    , btrSeverity   = SeverityModerate
    , btrFootprint  = FootprintSmall
    , btrAnisotropy = AnisotropyNone
    , btrEdge       = EdgeSmooth
    , btrSeed       = 7
    }

-- | Leave the pool's counter @headroom@ ids short of the point past
--   which the shader's table can resolve nothing — the state a
--   long-lived process reaches on its own.
spendTextureHandles ∷ RegistrationFixture → Int → IO ()
spendTextureHandles fx headroom = do
    pool ← readIORef (assetPoolRef (rfEnv fx))
    writeIORef (apNextTextureHandle pool) (handleSlotTableSize - headroom)

-- | Run one frame's blood upload sync against ALL-NULL Vulkan handles.
--
--   Sound for the same reason the registration cases above are: the
--   refusal returns before any of them is used. A regression that
--   uploaded or registered anyway fails on the null device instead of
--   passing quietly.
runFrame ∷ RegistrationFixture → IO ()
runFrame fx = do
    let action ∷ EngineM' ()
        action = do
            modifyGraphicsState $ \gs → gs
                { vulkanDevice  = Just zero
                , vulkanPDevice = Just zero
                , vulkanCmdPool = Just zero
                , deviceQueues  = Just (DevQueues zero zero 0 0)
                }
            uploadBloodTextures
    result ← runEngineM action (rfEnv fx) pure
    either (fail ∘ show) pure result

-- | Does this diagnostic name THIS handle as the one it is about?
--
--   Deliberately positional rather than a bare substring: the message
--   also states the CAP, and the first refused id IS the cap, so
--   @T.isInfixOf "65536"@ matches every unrepresentable handle's
--   message including @65537@'s. The @handle N for@ slot is the one
--   place the id under discussion appears.
namesHandle ∷ TextureHandle → Text → Bool
namesHandle handle =
    T.isInfixOf ("handle " <> T.pack (show (toInt handle)) <> " for")

-- | #1690's terminal outcome: the handle settles on 'AssetFailed' and
--   the failure reaches Lua on its OWN message, never 'LuaAssetLoaded'.
expectTerminalFailure ∷ HasCallStack
                      ⇒ RegistrationFixture → TextureHandle → Text
                      → Expectation
expectTerminalFailure fx handle path = do
    pool ← readIORef (assetPoolRef (rfEnv fx))
    states ← readIORef (apTextureHandles pool)
    case Map.lookup handle states of
        Just (AssetFailed reason) → do
            reason `shouldSatisfy` T.isInfixOf "#1699"
            reason `shouldSatisfy` T.isInfixOf path
        other → expectationFailure $
            "expected AssetFailed for the refused handle, got " ⧺ show other

-- | Classify one queued asset notification: @Just (loaded?, handle)@.
assetEvent ∷ LuaMsg → Maybe (Bool, Int)
assetEvent = \case
    LuaAssetLoaded _ handle _   → Just (True, handle)
    LuaAssetFailed _ handle _ _ → Just (False, handle)
    _                           → Nothing

-- | Everything the two 'btsHandleMap' insertion paths need in order to
--   run for real in a headless process.
data RegistrationFixture = RegistrationFixture
    { rfEnv    ∷ EngineEnv
    , rfTable  ∷ Ptr Word32
    , rfLogRef ∷ IORef [LogEntry]
    }

-- | The atlas the alias path duplicates, and the handle already resident
--   in the fixture's bindless system.
cachedAtlasPath ∷ Text
cachedAtlasPath = "assets/textures/ui/cached_atlas.png"

residentHandle ∷ TextureHandle
residentHandle = TextureHandle 9

residentSlot ∷ TextureSlot
residentSlot = TextureSlot 5 0

residentBindlessHandle ∷ BindlessTextureHandle
residentBindlessHandle = toBindlessHandle residentSlot residentHandle

-- | A headless engine with a capturing logger, plus a bindless system
--   whose ONLY real resource is its handle->slot table.
--
--   Every Vulkan handle in it is @VK_NULL_HANDLE@, and that is sound for
--   exactly the reason under test: a REFUSED registration returns before
--   it reaches 'writeDescriptorSlot' or any other Vulkan call, so these
--   cases never touch the null device.
--
--   That is also what makes them mutation-proof rather than merely
--   green. Neutralising 'checkRegistrableHandle' was measured against
--   this fixture: the two registration cases fail with @vulkan@'s own
--   "The function pointer for vkUpdateDescriptorSets is null" (a clean
--   'IOException', not a crash) because the sentinel now reaches the
--   descriptor write, and the alias case fails on its bookkeeping
--   comparison. Dropping the log line alone fails 'soleWarning'.
--
--   The two positive controls -- an already-resident handle, and a
--   genuinely exhausted allocator -- reach 'refuse' and the map lookup
--   without any Vulkan call, so they prove the production function
--   really runs here rather than short-circuiting somewhere harmless.
withRegistrationFixture ∷ (RegistrationFixture → IO α) → IO α
withRegistrationFixture action = withHandleSlotTable $ \table → do
    EngineInitResult env ← initializeEngineHeadlessQuiet
    logRef ← newIORef []
    logger ← initLogger defaultLogConfig
        { lcBackend = LogToCallback (\e → modifyIORef' logRef (e :)) }
    writeIORef (loggerRef env) logger
    writeIORef (textureSystemRef env) (Just (bareBindlessSystem table))
    -- The atlas the alias path reads, resident in the asset pool.
    pool ← readIORef (assetPoolRef env)
    writeIORef (assetPoolRef env) pool
        { apTextureAtlases =
            Map.insert cachedAssetId cachedAtlas (apTextureAtlases pool) }
    action RegistrationFixture
        { rfEnv = env, rfTable = table, rfLogRef = logRef }

-- | An ordinary handle that shares 'cachedAtlasPath', so a batch can
--   carry it beside the sentinel and prove the drop is targeted.
aliasHandle ∷ TextureHandle
aliasHandle = TextureHandle 77

-- | Point the asset pool's path cache at 'cachedAtlas', so a batch
--   request for its path takes the CACHED lane and needs no Vulkan
--   device to complete.
seedPathCache ∷ RegistrationFixture → IO ()
seedPathCache fx = do
    pool ← readIORef (assetPoolRef (rfEnv fx))
    writeIORef (assetPoolRef (rfEnv fx)) pool
        { apAssetPaths = Map.insert
                             (TextureCacheKey cachedAtlasPath
                                 UploadGlobalSampler)
                             cachedAssetId
                             (apAssetPaths pool) }

cachedAssetId ∷ AssetId
cachedAssetId = AssetId 1

cachedAtlas ∷ TextureAtlas
cachedAtlas = TextureAtlas
    { taId            = cachedAssetId
    , taName          = "cached_atlas"
    , taPath          = cachedAtlasPath
    , taMetadata      = AtlasMetadata (4, 4) Vk.FORMAT_R8G8B8A8_UNORM Map.empty
    , taInfo          = Nothing
    , taRefCount      = 1
    , taCleanup       = Nothing
    , taBindlessSlot  = Just (tsIndex residentSlot)
    , taTextureHandle = residentHandle
    }

-- | A 'BindlessTextureSystem' carrying one already-registered handle and
--   a real table pointer; every Vulkan field is null (see above).
bareBindlessSystem ∷ Ptr Word32 → BindlessTextureSystem
bareBindlessSystem table = BindlessTextureSystem
    { btsConfig           = defaultBindlessConfig
    , btsDescriptorPool   = zero
    , btsDescriptorLayout = zero
    , btsDescriptorSet    = zero
    , btsSlotAllocator    = createSlotAllocator 64
    , btsUndefinedTexture = UndefinedTexture (VulkanImage zero zero) zero
    , btsHandleMap        = Map.singleton residentHandle residentBindlessHandle
    , btsImageViews       = Map.singleton residentHandle zero
    , btsTextureSampler   = zero
    , btsTextureKind      = SamplerTextureNearest
    , btsPinned           = Map.empty
    , btsHandleSlotBuffer = zero
    , btsHandleSlotMemory = zero
    , btsHandleSlotPtr    = table
    }

-- | Drive one registration against the fixture's system, returning its
--   result and whatever it logged.
runRegistration ∷ RegistrationFixture
                → (BindlessTextureSystem
                   → EngineM' ( Either TextureRegistrationFailure BindlessTextureHandle
                              , BindlessTextureSystem ))
                → IO ( ( Either TextureRegistrationFailure BindlessTextureHandle
                       , BindlessTextureSystem )
                     , [LogEntry] )
runRegistration fx = runRegistrationWith fx id

runRegistrationWith ∷ RegistrationFixture
                    → (BindlessTextureSystem → BindlessTextureSystem)
                    → (BindlessTextureSystem
                       → EngineM' ( Either TextureRegistrationFailure BindlessTextureHandle
                                  , BindlessTextureSystem ))
                    → IO ( ( Either TextureRegistrationFailure BindlessTextureHandle
                           , BindlessTextureSystem )
                         , [LogEntry] )
runRegistrationWith fx tweak act = do
    mSystem ← readIORef (textureSystemRef (rfEnv fx))
    system ← case mSystem of
        Nothing → fail "the fixture's bindless system vanished"
        Just s  → pure (tweak s)
    result ← runEngineM (act system) (rfEnv fx) pure
    outcome ← either (fail ∘ show) pure result
    entries ← drainLog fx
    pure (outcome, entries)

-- | Drive the cached-alias fast path for one handle.
runAlias ∷ RegistrationFixture → TextureHandle → IO [LogEntry]
runAlias fx handle = do
    let action ∷ EngineM' ()
        action = duplicateCachedTextureHandle (rfEnv fx) handle
                     cachedAssetId cachedAtlas
    result ← runEngineM action (rfEnv fx) pure
    either (fail ∘ show) pure result
    drainLog fx

-- | Everything the alias path is supposed to leave alone when it
--   refuses: the bindless handle map, the shader table entry, the
--   asset-pool handle states and atlas refcounts, the texture-size map,
--   and the notification queue.
aliasObservables ∷ RegistrationFixture
                 → IO ( Map.Map TextureHandle BindlessTextureHandle
                      , Word32
                      , [TextureHandle]
                      , [Word32]
                      , Int
                      , Int )
aliasObservables fx = do
    let env = rfEnv fx
    mSystem ← readIORef (textureSystemRef env)
    entry0 ← peekTable fx (toInt missingTextureHandle)
    pool ← readIORef (assetPoolRef env)
    states ← readIORef (apTextureHandles pool)
    sizes ← readIORef (textureSizeRef env)
    queued ← Q.flushQueue (luaQueue env)
    -- Put the drained messages back: flushQueue is the only read, and a
    -- "before" snapshot must not itself change what "after" observes.
    mapM_ (Q.writeQueue (luaQueue env)) queued
    pure ( maybe Map.empty btsHandleMap mSystem
         , entry0
         , Map.keys states
         , map taRefCount (Map.elems (apTextureAtlases pool))
         , HM.size sizes
         , length queued )

peekTable ∷ RegistrationFixture → Int → IO Word32
peekTable fx = peekElemOff (rfTable fx)

drainLog ∷ RegistrationFixture → IO [LogEntry]
drainLog fx = do
    entries ← reverse ⊚ readIORef (rfLogRef fx)
    writeIORef (rfLogRef fx) []
    pure entries

-- | Exactly one WARN was emitted; return its message.
soleWarning ∷ HasCallStack ⇒ [LogEntry] → IO Text
soleWarning entries = case filter ((≡ LevelWarn) ∘ leLevel) entries of
    [entry] → pure (leMessage entry)
    other   → do
        expectationFailure $
            "expected exactly one WARN log entry, got " ⧺ show (length other)
        pure ""

-- | A refused registration: the failure, an untouched system, an
--   untouched table entry 0, and one log line naming both the zero
--   handle and the provenance the caller passed.
expectSentinelRefusal
    ∷ HasCallStack
    ⇒ RegistrationFixture
    → ( Either TextureRegistrationFailure BindlessTextureHandle
      , BindlessTextureSystem )
    → [LogEntry]
    → Text
    → Expectation
expectSentinelRefusal fx (outcome, system) entries source = do
    outcome `shouldBe` Left TextureHandleReserved
    -- Nothing allocated, nothing recorded: the returned system is the
    -- one that went in.
    btsHandleMap system `shouldBe` Map.singleton residentHandle residentBindlessHandle
    btsImageViews system `shouldBe` Map.singleton residentHandle zero
    btsPinned system `shouldBe` Map.empty
    tsaNextSlot (btsSlotAllocator system) `shouldBe` 1
    peekTable fx (toInt missingTextureHandle) `shouldReturn` 0
    message ← soleWarning entries
    message `shouldSatisfy` T.isInfixOf "handle 0"
    message `shouldSatisfy` T.isInfixOf source
    message `shouldSatisfy` T.isInfixOf "handleToSlot[0]"
    -- Never the OTHER reason a registration yields no handle (#1690).
    message `shouldNotSatisfy` T.isInfixOf "Failed to allocate bindless slot"

-- | A real, zero-initialised handle->slot table of the production size,
--   so 'writeHandleSlotEntryPtr' is exercised against the same memory
--   shape the mapped storage buffer gives it.
withHandleSlotTable ∷ (Ptr Word32 → IO α) → IO α
withHandleSlotTable action = allocaArray handleSlotTableSize $ \table → do
    pokeArray table (replicate handleSlotTableSize (0 ∷ Word32))
    action table

pathToLua ∷ FilePath → Text
pathToLua = T.pack

noHarvestedTextureYaml ∷ String
noHarvestedTextureYaml = unlines
    [ "flora:"
    , "  - name: test_fallback_species"
    , "    type: groundcover"
    , "    texDir: assets/textures/flora/unknown_flora_test_dir"
    , "    phases: []"
    , "    harvestable:"
    , "      tags: [leaves]"
    , "      yield:"
    , "        - id: test_item"
    , "          count: [1, 1]"
    , "      regrowth_time: 100"
    , "      # harvested_texture deliberately omitted"
    , "    worldGen:"
    , "      category: groundcover"
    , "      minTemp: 0"
    , "      maxTemp: 40"
    , "      idealTemp: 20"
    , "      minPrecip: 0"
    , "      maxPrecip: 100"
    , "      idealPrecip: 50"
    ]

-- | A bare Lua backend wired to a real engine, no world/window --
--   mirrors the identical pattern several other test-headless modules
--   already establish (e.g. Test.Headless.UI.Clipping's own
--   'newBareLuaBackend').
newBareLuaBackend ∷ EngineEnv → IO LuaBackendState
newBareLuaBackend env = do
    ls ← createLuaBackendState (luaToEngineQueue env) (luaQueue env)
                                (assetPoolRef env) (nextObjectIdRef env)
                                (inputStateRef env) (loggerRef env)
    stateRef ← newIORef ThreadRunning
    registerLuaAPI (lbsLuaState ls) env ls stateRef
    pure ls

-- | Run one command through the exact loadstring+pcall primitive the
--   real TCP debug console itself uses ('executeDebugLua').
evalDebug ∷ LuaBackendState → Text → IO Text
evalDebug ls = executeDebugLua (lbsLuaState ls)
