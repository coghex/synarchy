-- | #2020 (WML-2): the checked map-image plan, and the three boundaries
--   that now consume it.
--
--   Everything here is GPU-free. Device limits are INJECTED
--   ('MapImageCeiling'), the atlas is assembled from synthetic chunk
--   blocks, and the upload boundary is driven through the very
--   continuation seam the real handler uses — so "refused before either
--   Vulkan allocation" is asserted against the engine's own control
--   flow rather than a copy of it.
module Test.Headless.World.MapImagePlan (spec) where

import UPrelude
import Test.Hspec
import qualified Data.ByteString as BS
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Vector as V
import Data.Either (isLeft, isRight)
import Data.IORef (newIORef, readIORef, modifyIORef')
import qualified HsLua as Lua
import World.Map.ImagePlan
import Engine.Core.Types (BootMode(..))
import Engine.Map.ImageAdmission
    ( bootModeNeedsDeviceCeiling, resolveMapImageCeiling
    , validateZoomAtlasUpload, withValidatedZoomAtlasUpload )
import World.ZoomMap.ChunkTexture (ZoomAtlasData(..), buildZoomAtlas)
import World.ZoomMap.Types (zoomTileSize)

-- * Helpers

-- | Plan with no ceiling: the GPU-free admission every headless and
--   dump session performs.
planFree ∷ MapImageSource → Either MapImageRefusal MapImagePlan
planFree = admitMapImage CeilingNotApplicable MapImageRGBA8

planAt ∷ Int → MapImageSource → Either MapImageRefusal MapImagePlan
planAt limit = admitMapImage (CeilingKnown limit) MapImageRGBA8

acceptedPlan ∷ HasCallStack ⇒ Either MapImageRefusal MapImagePlan → IO MapImagePlan
acceptedPlan (Right p)  = pure p
acceptedPlan (Left ref) =
    expectationFailure ("expected an accepted plan, got: "
                        ⧺ T.unpack (mapImageRefusalText ref))
        ≫ error "unreachable"

refusalText ∷ HasCallStack ⇒ Either MapImageRefusal α → IO Text
refusalText (Left ref) = pure (mapImageRefusalText ref)
refusalText (Right _)  =
    expectationFailure "expected a refusal, got an accepted plan"
        ≫ error "unreachable"

shouldMention ∷ HasCallStack ⇒ Text → Text → Expectation
shouldMention haystack needle =
    (needle, haystack) `shouldSatisfy` \(n, h) → n `T.isInfixOf` h

dims ∷ MapImagePlan → (Int, Int)
dims p = (mipWidth p, mipHeight p)

-- | One synthetic 32×32 RGBA8 block whose every byte depends on BOTH
--   the block index and the offset within it, so a wrong tile position,
--   a wrong row, or a wrong byte inside a row all show up as a
--   difference rather than cancelling out.
syntheticBlock ∷ Int → BS.ByteString
syntheticBlock i = BS.pack
    [ fromIntegral ((i * 131 + o * 7 + 13) `mod` 256)
    | o ← [0 .. zoomTileSize * zoomTileSize * 4 - 1] ]

-- | The atlas those blocks must produce, built here from the layout
--   rule rather than from 'buildZoomAtlas' — an independent statement
--   of "row-major tiles, each copied row by row, everything else zero".
expectedAtlas ∷ Int → Int → Int → BS.ByteString
expectedAtlas atlasW atlasH blockCount = BS.pack
    [ byteAt px py c
    | py ← [0 .. atlasH - 1], px ← [0 .. atlasW - 1], c ← [0 .. 3] ]
  where
    perRow = atlasW `div` zoomTileSize
    byteAt px py c =
        let col = px `div` zoomTileSize
            row = py `div` zoomTileSize
            i   = row * perRow + col
            tx  = px `mod` zoomTileSize
            ty  = py `mod` zoomTileSize
            o   = (ty * zoomTileSize + tx) * 4 + c
        in if i < blockCount
             then fromIntegral ((i * 131 + o * 7 + 13) `mod` 256)
             else 0

-- | Run one self-contained Lua chunk in a fresh stdlib-only
--   interpreter with the repo root as CWD, the way every @cabal test@
--   run has it. The chunk signals failure through Lua's own @assert@.
runsOk ∷ HasCallStack ⇒ Text → Expectation
runsOk chunkText = do
    result ← Lua.run @Lua.Exception $ do
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

-- * Spec

spec ∷ Spec
spec = describe "map image plan (#2020)" $ do
    describe "world zoom-atlas geometry" $ do
        it "reproduces the shipped dimensions for every normalized \
           \world size the engine offers today" $ do
            let expectations =
                    [ (64,   (1472, 1440))
                    , (128,  (2912, 2912))
                    , (256,  (5824, 5792))
                    , (512,  (11616, 11584))
                    , (1024, (23200, 23168)) ]
            forM_ expectations $ \(size, expected) → do
                plan ← acceptedPlan (planFree (ZoomAtlasSource size))
                (size, dims plan) `shouldBe` (size, expected)

        it "plans worldSize 1024 as 23200×23168 / 2,149,990,400 bytes" $ do
            plan ← acceptedPlan (planFree (ZoomAtlasSource 1024))
            dims plan `shouldBe` (23200, 23168)
            mipByteCount plan `shouldBe` 2149990400

        -- D-4: the map format is designed through 8192, and a synthetic
        -- plan that far out must stay VALID — it fits Word32 extents and
        -- 64-bit byte counts.
        it "keeps a synthetic worldSize 8192 valid at 185376×185376 / \
           \137,457,045,504 bytes" $ do
            plan ← acceptedPlan (planFree (ZoomAtlasSource 8192))
            dims plan `shouldBe` (185376, 185376)
            mipByteCount plan `shouldBe` 137457045504

        it "derives the tile count exactly as BuildPixels does \
           \(worldSize² / 2 for a normalized size)" $ do
            forM_ [8, 64, 256, 1024] $ \size → do
                plan ← acceptedPlan (planFree (ZoomAtlasSource size))
                case mipLayout plan of
                    LayoutTiled { milTileCount = n, milTileEdge = e } → do
                        (size, n) `shouldBe` (size, size * size `div` 2)
                        e `shouldBe` zoomTileSize
                    LayoutWhole →
                        expectationFailure "zoom atlas planned as a whole image"

    describe "unsupported world geometry" $ do
        -- normalizeWorldSize rounds up to a multiple of
        -- minimumWorldSize (8); that rule is what makes every accepted
        -- size even, which is what makes BuildPixels' own
        -- w = 2 * (worldSize `div` 2) derivation agree with the plan.
        it "refuses an odd world size rather than silently planning a \
           \different chunk count than BuildPixels would produce" $ do
            msg ← refusalText (planFree (ZoomAtlasSource 63))
            msg `shouldMention` "worldSize 63"
            msg `shouldMention` "multiple of 8"

        it "refuses an even size that is not a multiple of the minimum" $ do
            msg ← refusalText (planFree (ZoomAtlasSource 100))
            msg `shouldMention` "worldSize 100"

        it "refuses zero and negative world sizes" $ do
            forM_ [0, -8, -1024] $ \size →
                planFree (ZoomAtlasSource size) `shouldSatisfy` isLeft

        it "refuses a degenerate tiled or whole image" $ do
            planFree (TiledImageSource 0 32) `shouldSatisfy` isLeft
            planFree (TiledImageSource 4 0)  `shouldSatisfy` isLeft
            planFree (WholeImageSource 0 16) `shouldSatisfy` isLeft
            planFree (WholeImageSource 16 0) `shouldSatisfy` isLeft

    describe "exact integer ceiling square root" $ do
        it "is right on both sides of a small perfect square" $ do
            map integerCeilSqrt [4095, 4096, 4097] `shouldBe` [64, 64, 65]

        it "is right on both sides of a large perfect square, where a \
           \Double has no bit left to round with" $ do
            map integerCeilSqrt [4294967295, 4294967296, 4294967297]
                `shouldBe` [65536, 65536, 65537]
            map integerCeilSqrt [ 4611686014132420609
                                , 4611686018427387904
                                , 4611686018427387905 ]
                `shouldBe` [2147483647, 2147483648, 2147483649]

        it "agrees with brute force across a dense low range" $
            forM_ [0 .. 4000 ∷ Integer] $ \n →
                let r = integerCeilSqrt n
                in (n, r * r ≥ n ∧ (r ≡ 0 ∨ (r - 1) * (r - 1) < n))
                       `shouldBe` (n, True)

    describe "representability" $ do
        it "refuses a width that cannot be a Word32 Vulkan extent, \
           \naming the exact value and the bound" $ do
            msg ← refusalText (planFree (WholeImageSource 5000000000 16))
            msg `shouldMention` "5000000000"
            msg `shouldMention` "Word32 Vulkan extent"
            msg `shouldMention` T.pack (show vulkanExtentBound)

        it "refuses generic synthetic tiled geometry whose packed width \
           \overruns a Word32 extent" $ do
            msg ← refusalText
                    (planFree (TiledImageSource 100000000000000000 32))
            msg `shouldMention` "Word32 Vulkan extent"

        it "refuses a byte count that cannot be a Word64 DeviceSize" $ do
            msg ← refusalText
                    (planFree (WholeImageSource 4294967295 4294967295))
            msg `shouldMention` "73786976260478468100"
            msg `shouldMention` "Word64 Vulkan DeviceSize"
            msg `shouldMention` T.pack (show deviceSizeBound)

        it "refuses a byte count that fits a DeviceSize but not a host \
           \Int allocation" $ do
            msg ← refusalText
                    (planFree (WholeImageSource 2000000000 1250000000))
            msg `shouldMention` "10000000000000000000"
            msg `shouldMention` "host Int allocation size"
            msg `shouldMention` T.pack (show hostAllocationBound)

    describe "device ceiling" $ do
        it "accepts worldSize 512 at exactly its own width, and one \
           \pixel above it, and refuses one pixel below" $ do
            planAt 11616 (ZoomAtlasSource 512) `shouldSatisfy` isRight
            planAt 11617 (ZoomAtlasSource 512) `shouldSatisfy` isRight
            planAt 11615 (ZoomAtlasSource 512) `shouldSatisfy` isLeft

        it "refuses worldSize 1024 against an injected 16384 limit, \
           \naming the size, the dimensions, the bytes and the limit" $ do
            msg ← refusalText (planAt 16384 (ZoomAtlasSource 1024))
            msg `shouldMention` "worldSize 1024"
            msg `shouldMention` "23200×23168"
            msg `shouldMention` "2149990400"
            msg `shouldMention` "16384"

        it "accepts worldSize 1024 on a device that really does report \
           \at least 23200" $
            planAt 23200 (ZoomAtlasSource 1024) `shouldSatisfy` isRight

        it "refuses when a GPU-capable mode has no limit, naming the \
           \failed query and claiming no dimensions it never checked" $ do
            let ceiling = resolveMapImageCeiling ModeGraphical Nothing
            msg ← refusalText
                    (admitMapImage ceiling MapImageRGBA8 (ZoomAtlasSource 64))
            msg `shouldMention` "maxImageDimension2D"
            msg `shouldMention` "graphical"
            msg `shouldNotMention` "1472"

        it "reports invalid geometry rather than a missing device when \
           \both are wrong, so a refusal never claims an unchecked \
           \dimension" $ do
            let ceiling = resolveMapImageCeiling ModeOffscreen Nothing
            msg ← refusalText
                    (admitMapImage ceiling MapImageRGBA8 (ZoomAtlasSource 63))
            msg `shouldMention` "multiple of 8"

    describe "boot-mode classification" $ do
        it "answers for every BootMode constructor" $
            map (\m → (m, bootModeNeedsDeviceCeiling m)) [minBound .. maxBound]
                `shouldBe`
                    [ (ModeDump, False), (ModeHeadless, False)
                    , (ModeOffscreen, True), (ModeGraphical, True)
                    , (ModePreview, True) ]

        it "gives a GPU-free mode no ceiling even when a stale limit is \
           \present, and never turns its missing device into an error" $ do
            resolveMapImageCeiling ModeHeadless Nothing
                `shouldBe` CeilingNotApplicable
            resolveMapImageCeiling ModeDump (Just 4096)
                `shouldBe` CeilingNotApplicable

        it "gives a GPU-capable mode the real limit when one is \
           \published" $
            resolveMapImageCeiling ModeGraphical (Just 16384)
                `shouldBe` CeilingKnown 16384

        it "still rejects arithmetic and geometry errors with no \
           \ceiling at all (the headless/dump contract)" $ do
            planFree (ZoomAtlasSource 63) `shouldSatisfy` isLeft
            planFree (WholeImageSource 5000000000 16) `shouldSatisfy` isLeft
            planFree (ZoomAtlasSource 1024) `shouldSatisfy` isRight

    describe "atlas construction against the plan" $ do
        it "produces byte-for-byte the historical layout — row \
           \placement, intra-tile rows, and zero padding" $ do
            -- worldSize 8: 32 chunks packed 6 per row over 6 rows, so
            -- the last 4 cells are padding and must stay zero.
            plan ← acceptedPlan (planFree (ZoomAtlasSource 8))
            dims plan `shouldBe` (192, 192)
            let blocks = V.generate 32 syntheticBlock
            case buildZoomAtlas plan 32 blocks of
                Left ref → expectationFailure
                    (T.unpack (mapImageRefusalText ref))
                Right atlas → do
                    zadWidth atlas `shouldBe` 192
                    zadHeight atlas `shouldBe` 192
                    zadChunksPerRow atlas `shouldBe` 6
                    BS.length (zadPixelData atlas) `shouldBe` 192 * 192 * 4
                    zadPixelData atlas `shouldBe` expectedAtlas 192 192 32

        it "refuses a zoom-cache count that disagrees with the plan" $ do
            plan ← acceptedPlan (planFree (ZoomAtlasSource 8))
            let blocks = V.generate 32 syntheticBlock
            msg ← refusalText (buildZoomAtlas plan 31 blocks)
            msg `shouldMention` "zoom cache"
            msg `shouldMention` "32"
            msg `shouldMention` "31"

        it "refuses a block count that disagrees with the plan" $ do
            plan ← acceptedPlan (planFree (ZoomAtlasSource 8))
            forM_ [31, 33] $ \n →
                buildZoomAtlas plan 32 (V.generate n syntheticBlock)
                    `shouldSatisfy` isLeft

        it "refuses a block that is not exactly zoomTileSize² × 4 bytes, \
           \naming which block and both sizes" $ do
            plan ← acceptedPlan (planFree (ZoomAtlasSource 8))
            let short i | i ≡ 7     = BS.take 4095 (syntheticBlock i)
                        | otherwise = syntheticBlock i
            msg ← refusalText (buildZoomAtlas plan 32 (V.generate 32 short))
            msg `shouldMention` "tile block 7"
            msg `shouldMention` "4095"
            msg `shouldMention` "4096"

        it "refuses a long block too — a copy is only safe when every \
           \block is exactly the planned size" $ do
            plan ← acceptedPlan (planFree (ZoomAtlasSource 8))
            let long i | i ≡ 0     = syntheticBlock i <> BS.singleton 0
                       | otherwise = syntheticBlock i
            buildZoomAtlas plan 32 (V.generate 32 long) `shouldSatisfy` isLeft

    describe "upload boundary" $ do
        -- 'withValidatedZoomAtlasUpload' is the exact seam
        -- 'Engine.Scripting.Lua.Message.WorldTexture.handleZoomAtlasUpload'
        -- runs through, so the allocation hooks below stand in for
        -- createVulkanImage' / createVulkanBuffer at the real call site.
        let drive limit w h len = do
                hooks ← newIORef ([] ∷ [Text])
                refused ← newIORef ([] ∷ [Text])
                withValidatedZoomAtlasUpload limit w h len
                    (\ref → modifyIORef' refused
                                (mapImageRefusalText ref :))
                    (\_plan → do
                        modifyIORef' hooks ("createVulkanImage'" :)
                        modifyIORef' hooks ("createVulkanBuffer" :))
                (,) <$> readIORef hooks <*> readIORef refused

        it "allocates for a payload whose length matches the plan" $ do
            (hooks, refusals) ← drive 16384 1472 1440 (1472 * 1440 * 4)
            refusals `shouldBe` []
            hooks `shouldMatchList` ["createVulkanImage'", "createVulkanBuffer"]

        it "refuses a payload-length mismatch before either allocation" $ do
            (hooks, refusals) ← drive 16384 1472 1440 (1472 * 1440 * 4 - 1)
            hooks `shouldBe` []
            case refusals of
                [msg] → do
                    msg `shouldMention` "8478720"
                    msg `shouldMention` "8478719"
                other → expectationFailure ("expected one refusal: "
                                            ⧺ show other)

        it "refuses an over-limit image before either allocation, using \
           \the device's own reported ceiling" $ do
            (hooks, refusals) ← drive 16384 23200 23168 2149990400
            hooks `shouldBe` []
            case refusals of
                [msg] → do
                    msg `shouldMention` "23200×23168"
                    msg `shouldMention` "16384"
                other → expectationFailure ("expected one refusal: "
                                            ⧺ show other)

        it "checks the payload against the planner's own arithmetic, \
           \never an ad-hoc w * h * 4 at the call site" $ do
            -- 46341² × 4 overflows a 32-bit int; the planner works in
            -- Integer, so the expected length it reports is the real one.
            case validateZoomAtlasUpload 65536 46341 46341 0 of
                Left ref → mapImageRefusalText ref
                             `shouldMention` "8589953124"
                Right _ → expectationFailure
                    "a zero-length payload must never validate"

    describe "Create World admission (scripts/create_world/generation.lua)" $ do
        it "records rejected, never enters RUNNING, and destroys nothing \
           \when the map image is refused" $ runsOk $ lns
            [ createWorldStubs
            , "world.checkMapImagePlan = function()"
            , "  return false, 'Refusing world zoom atlas for worldSize 1024: nope.'"
            , "end"
            , "local generation = dofile('scripts/create_world/generation.lua')"
            , "local menu = newMenu(generation)"
            , "generation.start(menu, logPanelStub)"
            , "assert(menu.genState == generation.IDLE,"
            , "       'genState must stay IDLE on a refusal, got '"
            , "       .. tostring(menu.genState))"
            , "assert(calls.destroyWorld == 0, 'the live world was destroyed')"
            , "assert(calls.setGenConfig == 0, 'gen config was pushed')"
            , "assert(calls.startGeneration == 0, 'generation was started')"
            , "assert(#outcomes == 1, 'expected exactly one recorded outcome')"
            , "assert(outcomes[1].kind == 'createWorld.generate',"
            , "       'wrong outcome kind')"
            , "assert(outcomes[1].outcome == 'rejected',"
            , "       'expected rejected, got ' .. tostring(outcomes[1].outcome))"
            , "assert(shownStatus == 'Cannot generate this world',"
            , "       'the player was not told')"
            , "assert(shownLines[1]:find('worldSize 1024', 1, true),"
            , "       'the diagnostic was not displayed')"
            ]

        it "destroys, configures, starts and records accepted when the \
           \map image is admitted" $ runsOk $ lns
            [ createWorldStubs
            , "world.checkMapImagePlan = function() return true end"
            , "local generation = dofile('scripts/create_world/generation.lua')"
            , "local menu = newMenu(generation)"
            , "generation.start(menu, logPanelStub)"
            , "assert(menu.genState == generation.RUNNING,"
            , "       'genState must be RUNNING once admitted')"
            , "assert(calls.destroyWorld == 1, 'the previous world survived')"
            , "assert(calls.setGenConfig == 1, 'gen config was not pushed')"
            , "assert(calls.startGeneration == 1, 'generation did not start')"
            , "assert(#outcomes == 1, 'expected exactly one recorded outcome')"
            , "assert(outcomes[1].outcome == 'accepted',"
            , "       'expected accepted, got ' .. tostring(outcomes[1].outcome))"
            ]

        it "asks about the size it is actually about to generate" $ runsOk $ lns
            [ createWorldStubs
            , "local asked = nil"
            , "world.checkMapImagePlan = function(n) asked = n; return true end"
            , "local generation = dofile('scripts/create_world/generation.lua')"
            , "local menu = newMenu(generation)"
            , "menu.pending.worldSize = '1024'"
            , "generation.start(menu, logPanelStub)"
            , "assert(asked == 1024, 'checked size ' .. tostring(asked))"
            ]

        -- #2288: the second pre-destruction admission. A refused
        -- world.setGenConfig has to cost the player nothing, which is
        -- only true because the call happens BEFORE destroyWorld().
        it "records rejected with the diagnostic, destroys nothing and \
           \starts nothing when the gen config is refused" $ runsOk $ lns
            [ createWorldStubs
            , "world.checkMapImagePlan = function() return true end"
            , "genConfigResult = { false,"
            , "  'world_gen.erosion_intensity = Infinity is outside the domain'"
            , "  .. ' (a finite number from 0.0 to 2.0); the world generation'"
            , "  .. ' configuration is left unchanged.' }"
            , "local generation = dofile('scripts/create_world/generation.lua')"
            , "local menu = newMenu(generation)"
            , "generation.start(menu, logPanelStub)"
            , "assert(menu.genState == generation.IDLE,"
            , "       'genState must stay IDLE on a refusal, got '"
            , "       .. tostring(menu.genState))"
            , "assert(calls.setGenConfig == 1, 'the config was never offered')"
            , "assert(calls.destroyWorld == 0, 'the live world was destroyed')"
            , "assert(calls.startGeneration == 0, 'generation was started')"
            , "assert(worldViewStub.worldParams == nil,"
            , "       'worldView.worldParams was overwritten')"
            , "assert(#outcomes == 1, 'expected exactly one recorded outcome')"
            , "assert(outcomes[1].outcome == 'rejected',"
            , "       'expected rejected, got ' .. tostring(outcomes[1].outcome))"
            , "assert(outcomes[1].reason and"
            , "       outcomes[1].reason:find('erosion_intensity', 1, true),"
            , "       'the reason did not name the field: '"
            , "       .. tostring(outcomes[1].reason))"
            , "assert(shownStatus == 'Cannot generate this world',"
            , "       'the player was not told')"
            , "assert(shownLines[1]:find('erosion_intensity', 1, true),"
            , "       'the diagnostic was not displayed')"
            ]

        it "evaluates the gen config BEFORE destroying the live world" $
            runsOk $ lns
            [ createWorldStubs
            , "local order = {}"
            , "world.checkMapImagePlan = function() return true end"
            , "world.setGenConfig = function()"
            , "  calls.setGenConfig = calls.setGenConfig + 1"
            , "  order[#order+1] = 'setGenConfig'"
            , "  return true"
            , "end"
            , "package.loaded['scripts.world_manager'].destroyWorld = function()"
            , "  calls.destroyWorld = calls.destroyWorld + 1"
            , "  order[#order+1] = 'destroyWorld'"
            , "end"
            , "local generation = dofile('scripts/create_world/generation.lua')"
            , "local menu = newMenu(generation)"
            , "generation.start(menu, logPanelStub)"
            , "assert(order[1] == 'setGenConfig' and order[2] == 'destroyWorld',"
            , "       'wrong order: ' .. table.concat(order, ','))"
            ]

-- | Everything @scripts/create_world/generation.lua@ reaches for,
--   stubbed. Nothing here may touch a real engine call: the point is to
--   observe exactly which of these the script does and does not invoke
--   on each side of the admission decision.
createWorldStubs ∷ Text
createWorldStubs = lns
    [ "calls = { destroyWorld = 0, setGenConfig = 0, startGeneration = 0 }"
    , "outcomes = {}"
    , "shownLines = {}"
    , "shownStatus = nil"
    , "engine = { logInfo = function() end, logWarn = function() end,"
    , "           logError = function() end, logDebug = function() end }"
    , "genConfigResult = { true }"
    , "world = {"
    , "  setGenConfig = function()"
    , "    calls.setGenConfig = calls.setGenConfig + 1"
    , "    return table.unpack(genConfigResult)"
    , "  end,"
    , "}"
    , "debug = debug or {}"
    , "debug.recordOutcome = function(t) outcomes[#outcomes+1] = t end"
    , "logPanelStub = {"
    , "  clear = function() shownLines = {} end,"
    , "  setStatus = function(_, t) shownStatus = t end,"
    , "  addLine = function(_, t) shownLines[#shownLines+1] = t end,"
    , "}"
    , "local emptyTab = { getWidgetValues = function() return {} end }"
    , "package.loaded['scripts.create_world.advanced_tab'] = emptyTab"
    , "package.loaded['scripts.create_world.general_tab']  = emptyTab"
    , "package.loaded['scripts.create_world.timeline_tab'] = emptyTab"
    , "package.loaded['scripts.create_world.name_suggest'] = {"
    , "  identity = function() return nil, nil, nil, nil end,"
    , "}"
    , "package.loaded['scripts.world_manager'] = {"
    , "  isActive = function() return true end,"
    , "  destroyWorld = function() calls.destroyWorld = calls.destroyWorld + 1 end,"
    , "}"
    , "worldViewStub = {"
    , "  startGeneration = function()"
    , "    calls.startGeneration = calls.startGeneration + 1"
    , "  end,"
    , "}"
    , "package.loaded['scripts.world_view'] = worldViewStub"
    , "function newMenu(generation)"
    , "  return { genState = generation.IDLE, genElapsed = 0,"
    , "           pending = { seed = '2A', worldSize = '64',"
    , "                       plateCount = '5', worldName = 'Testworld' } }"
    , "end"
    ]

shouldNotMention ∷ HasCallStack ⇒ Text → Text → Expectation
shouldNotMention haystack needle =
    (needle, haystack) `shouldSatisfy` \(n, h) → not (n `T.isInfixOf` h)
