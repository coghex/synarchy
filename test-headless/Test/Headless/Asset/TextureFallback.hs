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
import Control.Exception (finally)
import Data.IORef (newIORef, readIORef)
import qualified Data.HashMap.Strict as HM
import System.Directory
    ( getTemporaryDirectory, createDirectoryIfMissing, removeDirectoryRecursive
    , doesFileExist )
import System.FilePath ((</>))
import qualified Data.Text as T
import qualified Data.Vector.Storable as Vec
import Engine.Core.Init (initializeEngineHeadless, EngineInitResult(..))
import Engine.Core.State (EngineEnv, floraCatalogRef, luaToEngineQueue, luaQueue
    , assetPoolRef, nextObjectIdRef, inputStateRef, loggerRef)
import Engine.Core.Thread (ThreadControl(..))
import Engine.Scripting.Lua.API (registerLuaAPI)
import Engine.Scripting.Lua.API.YamlTextures (resolveTexturePath)
import Engine.Scripting.Lua.API.Units (unknownUnitTexture)
import Engine.Scripting.Lua.Thread (createLuaBackendState)
import Engine.Scripting.Lua.Thread.Console (executeDebugLua)
import Engine.Scripting.Lua.Types (LuaBackendState(..))
import Engine.Asset.Handle (TextureHandle(..), missingTextureHandle
    , firstAllocatableTextureHandle, isMissingTextureHandle, toInt)
import Engine.Asset.Manager (generateTextureHandle)
import Engine.Asset.Types (defaultAssetPool)
import Engine.Graphics.Camera (CameraFacing(..))
import Engine.Graphics.Vulkan.Texture.Bindless
    ( TextureRegistrationFailure(..), checkRegistrableHandle
    , registrationFailureMessage, writeHandleSlotEntryPtr, handleSlotTableSize )
import Engine.Graphics.Vulkan.Texture.Undefined (undefinedTextureData)
import Engine.Graphics.Vulkan.Types.Vertex (Vertex(..), noFaceMapVertexId)
import Engine.Scene.Types (SortableQuad(..))
import World.Render.FloraQuads (floraToQuad)
import World.Render.Textures.Types (defaultWorldTextures)
import Unit.Direction (Direction(..))
import World.Flora.Types (FloraCatalog(..), FloraId(..), FloraSpecies(..)
    , FloraHarvest(..), FloraInstance(..), lookupSpecies)

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
            -- lightweight `initializeEngineHeadless` env (the same
            -- primitive Test.Headless.World.LocationDiscovery/CursorInfo/
            -- Unit.LineOfSight already use for exactly this "needs its
            -- own throwaway engine state" case, no world/unit thread
            -- spawned) sidesteps the whole class of gap: this test's
            -- mutations only ever touch its own engine, discarded whole
            -- when the test ends, and the aroundAll-shared `_sharedEnv`
            -- above is deliberately unused.
            EngineInitResult env ← initializeEngineHeadless
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

        -- Layer 2a: the guard both registration paths share.
        it "the shared registration guard refuses the sentinel and admits \
           \every allocatable handle" $ \_env → do
            checkRegistrableHandle missingTextureHandle
                `shouldBe` Left TextureHandleReserved
            pool ← defaultAssetPool
            handles ← replicateM 8 (generateTextureHandle pool)
            map checkRegistrableHandle handles `shouldBe` replicate 8 (Right ())

        it "the refusal it logs names the zero handle and the caller's \
           \provenance, and does not read as slot exhaustion" $ \_env → do
            -- This is the exact Text 'registerTextureImpl' and
            -- 'duplicateCachedTextureHandle' hand to 'logWarnM' -- the
            -- rejection log line itself, not a restatement of it.
            let refusal = registrationFailureMessage TextureHandleReserved
                              missingTextureHandle "assets/textures/ui/blank.png"
            refusal `shouldSatisfy` T.isInfixOf "handle 0"
            refusal `shouldSatisfy` T.isInfixOf "assets/textures/ui/blank.png"
            refusal `shouldSatisfy` T.isInfixOf "handleToSlot[0]"
            -- Distinguishable from the OTHER reason a registration
            -- produces no handle, which is what #1690 is about.
            refusal `shouldSatisfy` (not ∘ T.isInfixOf "Failed to allocate bindless slot")
            let exhausted = registrationFailureMessage TextureSlotsExhausted
                                (TextureHandle 42) "assets/textures/ui/blank.png"
            exhausted `shouldSatisfy` T.isInfixOf "Failed to allocate bindless slot"
            exhausted `shouldSatisfy` T.isInfixOf "handle 42"

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
