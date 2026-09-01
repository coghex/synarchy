{-# LANGUAGE OverloadedStrings #-}
-- | BDA-1 (#2080): the four-camera-facing building asset schema and the
--   split lifecycle roles.
--
--   Three things are proved here that nothing else can prove:
--
--   1. An ASYMMETRIC fixture — four different static paths, and four
--      different frame lists — survives BOTH the decoder and the real
--      loader conversion as four distinct values. A symmetric fixture
--      would pass against a decoder that silently collapsed the four
--      keys onto one, which is exactly the pre-#2080 behaviour.
--   2. Every rejection the schema promises actually fires, and names
--      the building (and where relevant the animation, direction or
--      stage) it fires about.
--   3. The two lifecycle arms report DIFFERENT activities, in Haskell
--      and on the Lua wire, while the frames they pick keep the
--      pre-#2080 progress-driven / time-driven / pinned behaviour.
module Test.Headless.Building.AssetSchema (spec) where

import UPrelude
import Test.Hspec
import Control.Exception (finally)
import Data.Foldable (toList)
import Data.IORef (atomicModifyIORef', readIORef, newIORef)
import Data.List (isInfixOf, sort)
import qualified Data.ByteString.Char8 as BS
import qualified Data.HashMap.Strict as HM
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import qualified Data.Vector as V
import qualified Data.Yaml as Yaml
import Building.Render (pickBuildingFrame)
import Building.Schema
import Building.Types
import Engine.Asset.Handle (TextureHandle(..))
import Engine.Asset.YamlBuildings
import Engine.Core.Capability.Building
    (BuildingCapability(..), toBuildingCapability)
import Engine.Core.State (EngineEnv, luaToEngineQueue, luaQueue, assetPoolRef
    , nextObjectIdRef, inputStateRef, loggerRef)
import Engine.Core.Thread (ThreadControl(..))
import Engine.Scripting.Lua.API (registerLuaAPI)
import Engine.Scripting.Lua.Thread (createLuaBackendState)
import qualified Engine.Core.Queue as Q
import Engine.Graphics.Camera (CameraFacing(..))
import Engine.Graphics.Vulkan.Texture.Policy (UploadSampler(..))
import Engine.Scripting.Lua.Thread.Console (executeDebugLua)
import Engine.Scripting.Lua.Types (LuaBackendState(..), LuaToEngineMsg(..))
import System.Directory (listDirectory, getTemporaryDirectory, removeFile)
import System.FilePath (takeExtension, (</>))
import Test.Headless.Harness (withHeadlessEngine)
import World.Page.Types (WorldPageId(..))
import World.Save.Types
    ( toBuildingInstanceSnapshot, fromBuildingInstanceSnapshot
    , BuildingInstanceSnapshot(..) )

spec ∷ Spec
spec = do
    describe "the canonical four-facing declaration" canonicalSpec
    describe "declaration rejections" rejectionSpec
    describe "legacy migration compatibility" legacySpec
    describe "the shipped definitions" shippedSpec
    describe "lifecycle roles and derived activity" lifecycleSpec
    describe "the loader conversion" $
        aroundAll withHeadlessEngine loaderSpec

-- * The canonical form

canonicalSpec ∷ Spec
canonicalSpec = do
    it "keeps four DISTINCT static paths, one per camera facing" $ do
        def ← decodeOne (asymmetricYaml [])
        faSource (bydSprites def) `shouldBe` AssetCanonical
        let views = faViews (bydSprites def)
        [ facingValue f views | f ← canonicalFacings ]
            `shouldBe` asymSpritePaths
        -- Four values, not one repeated: the pre-#2080 loader kept
        -- whichever direction it folded last.
        length (nubOrd (toList views)) `shouldBe` 4

    it "keeps every frame of four DISTINCT animation lists" $ do
        def ← decodeOne (asymmetricYaml [])
        anim ← lookupAnim "walkabout" def
        faSource (byaFrames anim) `shouldBe` AssetCanonical
        toList (faViews (byaFrames anim)) `shouldBe` asymFramePaths

    it "orders the facings south, west, north, east" $ do
        -- The order is CameraFacing's own, and the derived Foldable
        -- walks the record in that order — so a reordering of either
        -- would fail here rather than silently renaming three views.
        map facingKey canonicalFacings
            `shouldBe` ["south", "west", "north", "east"]
        canonicalFacings `shouldBe` [FaceSouth, FaceWest, FaceNorth, FaceEast]
        toList (FacingSet 'a' 'b' 'c' 'd') `shouldBe` "abcd"
        map (`facingValue` FacingSet 'a' 'b' 'c' 'd') canonicalFacings
            `shouldBe` "abcd"

    it "is declared with CameraFacing keys, never a unit direction" $ do
        -- Buildings have four views; units have eight facings and a
        -- mirror flag. A unit key must not decode.
        sort (map facingKey canonicalFacings)
            `shouldBe` sort ["south", "west", "north", "east"]
        facingFromKey "DirSE" `shouldBe` Nothing
        facingFromKey "southeast" `shouldBe` Nothing
        facingFromKey "default" `shouldBe` Nothing

-- * Rejections

rejectionSpec ∷ Spec
rejectionSpec = do
    it "rejects a `sprites` block missing a direction" $
        decodeYaml (buildingYaml
            [ "    sprites:"
            , "      south: \"s.png\""
            , "      west: \"w.png\""
            , "      north: \"n.png\""
            ]) `shouldFailWith` ["probe_hall", "sprites", "east", "required"]

    it "rejects an unknown direction key in `sprites`" $
        decodeYaml (buildingYaml
            [ "    sprites:"
            , "      south: \"s.png\""
            , "      west: \"w.png\""
            , "      north: \"n.png\""
            , "      east: \"e.png\""
            , "      up: \"u.png\""
            ]) `shouldFailWith` ["probe_hall", "sprites", "up", "unknown"]

    it "rejects `sprites` declared beside the legacy `sprite`" $
        decodeYaml (buildingYaml
            [ "    sprite: \"legacy.png\""
            , "    sprites:"
            , "      south: \"s.png\""
            , "      west: \"w.png\""
            , "      north: \"n.png\""
            , "      east: \"e.png\""
            ]) `shouldFailWith` ["probe_hall", "sprites", "sprite", "both"]

    it "rejects a definition declaring no sprite at all" $
        decodeYaml (buildingYaml [ "    tile_size: { x: 1, y: 1 }" ])
            `shouldFailWith` ["probe_hall", "sprites", "no sprite"]

    it "rejects an empty canonical frame list" $
        decodeYaml (asymmetricYaml
            [ "      walkabout:"
            , "        frames:"
            , "          south: []"
            , "          west: [\"w0.png\"]"
            , "          north: [\"n0.png\"]"
            , "          east: [\"e0.png\"]"
            ]) `shouldFailWith` ["probe_hall", "walkabout", "south", "empty"]

    it "rejects unequal canonical frame counts" $
        decodeYaml (asymmetricYaml
            [ "      walkabout:"
            , "        frames:"
            , "          south: [\"s0.png\", \"s1.png\"]"
            , "          west: [\"w0.png\"]"
            , "          north: [\"n0.png\", \"n1.png\"]"
            , "          east: [\"e0.png\", \"e1.png\"]"
            ]) `shouldFailWith`
                ["probe_hall", "walkabout", "west", "frame count"]

    it "rejects one path claimed by two facings at the same stage" $
        decodeYaml (asymmetricYaml
            [ "      walkabout:"
            , "        frames:"
            , "          south: [\"s0.png\", \"shared.png\"]"
            , "          west: [\"w0.png\", \"shared.png\"]"
            , "          north: [\"n0.png\", \"n1.png\"]"
            , "          east: [\"e0.png\", \"e1.png\"]"
            ]) `shouldFailWith`
                ["probe_hall", "walkabout", "stage 1", "shared.png"
                , "south", "west"]

    it "accepts one path reused at DIFFERENT stages of one animation" $ do
        -- The rule is per-stage. A frame legitimately recurring later in
        -- the same direction's own clip is not a collapsed declaration.
        def ← decodeOne (asymmetricYaml
            [ "      walkabout:"
            , "        frames:"
            , "          south: [\"s0.png\", \"w0.png\"]"
            , "          west: [\"w0.png\", \"s0.png\"]"
            , "          north: [\"n0.png\", \"n1.png\"]"
            , "          east: [\"e0.png\", \"e1.png\"]"
            ])
        anim ← lookupAnim "walkabout" def
        faSource (byaFrames anim) `shouldBe` AssetCanonical

    it "rejects a canonical direction key beside the legacy `default`" $
        decodeYaml (asymmetricYaml
            [ "      walkabout:"
            , "        frames:"
            , "          default: [\"d0.png\"]"
            , "          south: [\"s0.png\"]"
            , "          west: [\"w0.png\"]"
            , "          north: [\"n0.png\"]"
            , "          east: [\"e0.png\"]"
            ]) `shouldFailWith`
                ["probe_hall", "walkabout", "default", "south", "both"]

    it "rejects an animation with no `frames` at all" $
        decodeYaml (asymmetricYaml
            [ "      walkabout:"
            , "        fps: 4"
            ]) `shouldFailWith` ["probe_hall", "walkabout", "no `frames`"]

    it "rejects an unknown lifecycle key" $
        decodeYaml (buildingYaml
            [ "    sprite: \"legacy.png\""
            , "    state_animations:"
            , "      demolishing: whatever"
            ]) `shouldFailWith`
                ["probe_hall", "demolishing", "construction", "destruction"]

    it "rejects legacy `appearing` beside the role it resolves to" $ do
        -- Positive build_work resolves `appearing` to `construction`...
        decodeYaml (buildingYaml
            [ "    sprite: \"legacy.png\""
            , "    build_work: 100.0"
            , "    state_animations:"
            , "      appearing: a"
            , "      construction: c"
            ]) `shouldFailWith` ["probe_hall", "appearing", "construction"]
        -- ...and zero build_work resolves it to `appearance`.
        decodeYaml (buildingYaml
            [ "    sprite: \"legacy.png\""
            , "    state_animations:"
            , "      appearing: a"
            , "      appearance: b"
            ]) `shouldFailWith` ["probe_hall", "appearing", "appearance"]

    it "accepts legacy `appearing` beside a role it does NOT resolve to" $ do
        -- The mixing rule is about the ONE role the legacy key means,
        -- not about canonical keys in general: `built` is a different
        -- role and stays legal beside it.
        def ← decodeOne (buildingYaml
            [ "    sprite: \"legacy.png\""
            , "    state_animations:"
            , "      appearing: a"
            , "      built: b"
            ])
        bydRoleAnims def `shouldBe`
            Map.fromList [(RoleAppearance, "a"), (RoleBuilt, "b")]

    it "rejects a missing `visual_class`" $
        decodeYaml (BS.unlines
            [ "buildings:"
            , "  - name: \"probe_hall\""
            , "    sprite: \"legacy.png\""
            ]) `shouldFailWith`
                ["probe_hall", "visual_class", "indoor_fixture", "gateway"]

    it "rejects an unrecognized `visual_class`" $
        decodeYaml (BS.unlines
            [ "buildings:"
            , "  - name: \"probe_hall\""
            , "    sprite: \"legacy.png\""
            , "    visual_class: \"shed\""
            ]) `shouldFailWith` ["probe_hall", "visual_class", "shed"]

-- * Legacy compatibility

legacySpec ∷ Spec
legacySpec = do
    it "reads a legacy `sprite` and marks it AS legacy" $ do
        def ← decodeOne (buildingYaml [ "    sprite: \"legacy.png\"" ])
        -- The one observable difference between the two forms, and the
        -- signal BDA-13's whole-tree audit rejects shipped art on.
        -- Without this assertion the marker could be dropped and
        -- nothing would fail.
        isLegacyDeclared (bydSprites def) `shouldBe` True
        faSource (bydSprites def) `shouldBe` AssetLegacy
        toList (faViews (bydSprites def)) `shouldBe` replicate 4 "legacy.png"

    it "reads a legacy `frames.default` and marks it AS legacy" $ do
        def ← decodeOne (asymmetricYaml
            [ "      walkabout:"
            , "        frames:"
            , "          default: [\"d0.png\", \"d1.png\"]"
            ])
        anim ← lookupAnim "walkabout" def
        isLegacyDeclared (byaFrames anim) `shouldBe` True
        toList (faViews (byaFrames anim))
            `shouldBe` replicate 4 ["d0.png", "d1.png"]

    it "resolves legacy `appearing` by the definition's own build_work" $ do
        worker ← decodeOne (buildingYaml
            [ "    sprite: \"legacy.png\""
            , "    build_work: 100.0"
            , "    state_animations:"
            , "      appearing: raise"
            ])
        bydRoleAnims worker `shouldBe` Map.singleton RoleConstruction "raise"
        timed ← decodeOne (buildingYaml
            [ "    sprite: \"legacy.png\""
            , "    state_animations:"
            , "      appearing: unfold"
            ])
        bydRoleAnims timed `shouldBe` Map.singleton RoleAppearance "unfold"

    it "accepts legacy PATHS beside canonical lifecycle ROLES" $ do
        -- The two migration axes are independent, and this combination
        -- is exactly the post-slice state of every shipped definition:
        -- lifecycle roles migrated now, four-facing art deferred to the
        -- art slices.
        def ← decodeOne (buildingYaml
            [ "    sprite: \"legacy.png\""
            , "    build_work: 100.0"
            , "    state_animations:"
            , "      construction: raise"
            , "    animations:"
            , "      raise:"
            , "        frames:"
            , "          default: [\"d0.png\"]"
            ])
        isLegacyDeclared (bydSprites def) `shouldBe` True
        bydRoleAnims def `shouldBe` Map.singleton RoleConstruction "raise"
        anim ← lookupAnim "raise" def
        isLegacyDeclared (byaFrames anim) `shouldBe` True

-- * The shipped tree

shippedSpec ∷ Spec
shippedSpec = do
    it "decodes every shipped definition with its required visual class" $ do
        defs ← shippedDefs
        map (\d → (bydName d, bydVisualClass d)) defs `shouldMatchList`
            [ ("kitchen",              IndoorFixture)
            , ("workbench",            IndoorFixture)
            , ("machine_shop",         IndoorFixture)
            , ("cargo_hold_S",         FreestandingInstallation)
            , ("furnace",              FreestandingInstallation)
            , ("solar_panel",          FreestandingInstallation)
            , ("high_voltage_battery", FreestandingInstallation)
            , ("acolyte_portal",       Gateway)
            ]

    it "carries the migrated lifecycle roles and no legacy key" $ do
        defs ← shippedDefs
        let roles d = (bydName d, Map.toList (bydRoleAnims d))
        map roles defs `shouldMatchList`
            [ ("kitchen",              [])
            , ("solar_panel",          [])
            , ("high_voltage_battery", [])
            , ("workbench",    [(RoleConstruction, "workbench-construct")])
            , ("machine_shop", [(RoleConstruction, "machine-shop-construct")])
            , ("cargo_hold_S", [(RoleConstruction, "cargo-construct")])
            , ("furnace",      [(RoleConstruction, "furnace-construct")])
            , ("acolyte_portal", [ (RoleAppearance, "portal-appear")
                                 , (RoleBuilt, "portal-idle") ])
            ]

    it "still declares its art in the legacy form, pending the art slices" $ do
        -- Not an accident: the art migration is explicitly deferred, so
        -- this records the state BDA-13 will later refuse.
        defs ← shippedDefs
        map (isLegacyDeclared ∘ bydSprites) defs
            `shouldBe` replicate (length defs) True

-- * Lifecycle

lifecycleSpec ∷ Spec
lifecycleSpec = do
    it "reports Constructing for a worker-driven build below its target" $ do
        currentActivity 0 (instanceAt 0 0) workerDef `shouldBe` Constructing
        currentActivity 0 (instanceAt 0 119.9) workerDef `shouldBe` Constructing
        currentActivity 0 (instanceAt 0 120) workerDef `shouldBe` Built

    it "reports Appearing for a zero-work definition inside its clip" $ do
        -- 4 frames at 2 fps = 2 game-seconds, time-driven.
        currentActivity 0 (instanceAt 0 0) timedDef `shouldBe` Appearing
        currentActivity 1.9 (instanceAt 0 0) timedDef `shouldBe` Appearing
        currentActivity 2.0 (instanceAt 0 0) timedDef `shouldBe` Built

    it "gives the two arms DISTINCT Lua activity strings" $ do
        map buildingActivityLabel [Constructing, Appearing, Built]
            `shouldBe` ["constructing", "appearing", "built"]
        buildingActivityLabel (currentActivity 0 (instanceAt 0 0) workerDef)
            `shouldBe` "constructing"
        buildingActivityLabel (currentActivity 0 (instanceAt 0 0) timedDef)
            `shouldBe` "appearing"

    it "keeps construction progress-driven and appearance time-driven" $ do
        -- Construction indexes on progress and ignores the clock, so a
        -- stalled build freezes rather than animating on.
        pickBuildingFrame 0 (instanceAt 0 0) workerDef `shouldBe` handle 10
        pickBuildingFrame 9999 (instanceAt 0 0) workerDef `shouldBe` handle 10
        pickBuildingFrame 0 (instanceAt 0 60) workerDef `shouldBe` handle 12
        -- Appearance indexes on elapsed game time and ignores progress.
        pickBuildingFrame 0 (instanceAt 0 0) timedDef `shouldBe` handle 20
        pickBuildingFrame 1.0 (instanceAt 0 0) timedDef `shouldBe` handle 22

    it "pins the last frame of the role the definition's build_work picked" $ do
        -- Neither def declares a `built` animation, so a Built instance
        -- pins its own lifecycle role's final frame rather than snapping
        -- back to the static sprite.
        legacyRoleFor 120 `shouldBe` RoleConstruction
        legacyRoleFor 0 `shouldBe` RoleAppearance
        pickBuildingFrame 0 (instanceAt 0 120) workerDef `shouldBe` handle 13
        pickBuildingFrame 99 (instanceAt 0 0) timedDef `shouldBe` handle 23

    it "renders the static SOUTH view when no role animation applies" $ do
        let bare = workerDef { bdRoleAnims = Map.empty }
        pickBuildingFrame 0 (instanceAt 0 0) bare `shouldBe` handle 1
        bdSouthTexture bare `shouldBe` handle 1

    it "reloads a snapshot with progress intact and textures re-resolved" $ do
        -- Requirement 12: no orientation is stored, and the def-owned
        -- facing assets come back from the def by name.
        let live = (instanceAt 0 60) { biTexture = handle 999 }
            snap = toBuildingInstanceSnapshot live
            back = fromBuildingInstanceSnapshot (WorldPageId "p") workerDef snap
        bisBuildProgress snap `shouldBe` 60
        biBuildProgress back `shouldBe` 60
        biTexture back `shouldBe` bdSouthTexture workerDef
        currentActivity 0 back workerDef `shouldBe` Constructing

-- * The real loader

loaderSpec ∷ SpecWith EngineEnv
loaderSpec = do
    it "keeps four asymmetric paths four distinct handles" $ \env → do
        (msgs, def) ← runLoader env "probe_asymmetric.yaml"
                                   (asymmetricYaml [])
        let views = toList (faViews (bdTextures def))
        faSource (bdTextures def) `shouldBe` AssetCanonical
        length (nubOrd views) `shouldBe` 4
        -- Every declared path reached the upload queue on its own
        -- handle: a collapsing loader would queue one of them four
        -- times, or three of them not at all.
        let wanted = map T.unpack asymSpritePaths
        sort [ p | LuaLoadTextureRequest _ p UploadGlobalSampler ← msgs
                 , p `elem` wanted ]
            `shouldBe` sort wanted
        -- The build menu's pinned icon is the SOUTH view and nothing
        -- else (#2075's dual-use pair, unchanged by the split).
        [ p | LuaLoadTextureRequest _ p UploadPinnedNearest ← msgs ]
            `shouldBe` [T.unpack (decodeBS asymSouth)]
        anim ← case HM.lookup "walkabout" (bdAnimations def) of
            Just a → pure a
            Nothing → do
                expectationFailure "walkabout animation was not registered"
                pure (BuildingAnimation 0 False (legacyAssets V.empty))
        let frameViews = map V.toList (toList (faViews (banFrames anim)))
        map length frameViews `shouldBe` [2, 2, 2, 2]
        length (nubOrd (concat frameViews)) `shouldBe` 8

    it "exposes ONE legacy path through all four views, loaded once" $ \env → do
        (msgs, def) ← runLoader env "probe_legacy.yaml"
            (buildingYaml [ "    sprite: \"" <> legacySpritePath <> "\"" ])
        faSource (bdTextures def) `shouldBe` AssetLegacy
        length (nubOrd (toList (faViews (bdTextures def)))) `shouldBe` 1
        -- The scene upload plus the build-menu icon's pinned copy —
        -- exactly what an unmigrated definition has always cost. Four
        -- world uploads here would be a regression, not a feature.
        let legacyPath = T.unpack (decodeBS legacySpritePath)
        length [ () | LuaLoadTextureRequest _ p _ ← msgs, p ≡ legacyPath ]
            `shouldBe` 2

-- * Fixtures

-- | A definition with a DIFFERENT path per facing at every position, so
--   any collapse of the four views is visible as a lost value.
--
--   The paths are REAL checked-in files borrowed from four unrelated
--   buildings: the loader substitutes @unknown_building.png@ for a
--   missing path, which would make four distinct declarations resolve
--   to one and hide exactly the bug this fixture exists to catch.
asymmetricYaml ∷ [BS.ByteString] → BS.ByteString
asymmetricYaml animLines = buildingYaml $
    [ "    sprites:"
    , "      south: \"" <> asymSouth <> "\""
    , "      west: \"" <> asymWest <> "\""
    , "      north: \"" <> asymNorth <> "\""
    , "      east: \"" <> asymEast <> "\""
    , "    animations:"
    ] ⧺ (if null animLines then defaultAnim else animLines)
  where
    defaultAnim =
        [ "      walkabout:"
        , "        fps: 4"
        , "        frames:"
        ] ⧺ [ "          " <> key <> ": [\"" <> dir <> "/frame_001.png\", \""
                  <> dir <> "/frame_002.png\"]"
            | (key, dir) ← zip ["south", "west", "north", "east"] asymFrameDirs ]

asymSouth, asymWest, asymNorth, asymEast ∷ BS.ByteString
asymSouth = "assets/textures/buildings/cargo_hold_S/default.png"
asymWest  = "assets/textures/buildings/furnace/default.png"
asymNorth = "assets/textures/buildings/workbench/default.png"
asymEast  = "assets/textures/buildings/kitchen/default.png"

asymFrameDirs ∷ [BS.ByteString]
asymFrameDirs =
    [ "assets/textures/buildings/cargo_hold_S/construct"
    , "assets/textures/buildings/furnace/construct"
    , "assets/textures/buildings/workbench/construct"
    , "assets/textures/buildings/machine_shop/construct"
    ]

-- | The same paths as 'Text', for assertions.
asymSpritePaths ∷ [Text]
asymSpritePaths = map decodeBS [asymSouth, asymWest, asymNorth, asymEast]

asymFramePaths ∷ [[Text]]
asymFramePaths =
    [ [ decodeBS (d <> "/frame_001.png"), decodeBS (d <> "/frame_002.png") ]
    | d ← asymFrameDirs ]

decodeBS ∷ BS.ByteString → Text
decodeBS = T.pack ∘ BS.unpack

-- | A real checked-in path for the legacy loader case, for the same
--   reason the asymmetric fixture uses real ones.
legacySpritePath ∷ BS.ByteString
legacySpritePath = "assets/textures/buildings/solar_panel/default.png"

buildingYaml ∷ [BS.ByteString] → BS.ByteString
buildingYaml body = BS.unlines $
    [ "buildings:"
    , "  - name: \"probe_hall\""
    , "    visual_class: \"indoor_fixture\""
    ] ⧺ body

decodeYaml ∷ BS.ByteString → Either Yaml.ParseException BuildingYamlFile
decodeYaml = Yaml.decodeEither'

decodeOne ∷ BS.ByteString → IO BuildingYamlDef
decodeOne bytes = case decodeYaml bytes of
    Right file | [def] ← byfBuildings file → pure def
    Right _ → fail "expected exactly one building definition"
    Left err → fail ("fixture failed to decode: " <> show err)

lookupAnim ∷ Text → BuildingYamlDef → IO BuildingYamlAnim
lookupAnim name def = case Map.lookup name (bydAnimations def) of
    Just a  → pure a
    Nothing → fail ("animation " <> T.unpack name <> " is not declared")

-- | Every fragment must appear in the rejection message, so a rule that
--   fires for the wrong reason — or without naming what it fired
--   about — fails here.
shouldFailWith ∷ Either Yaml.ParseException BuildingYamlFile → [String]
               → Expectation
shouldFailWith result fragments = case result of
    Right _ → expectationFailure "expected the declaration to be rejected"
    Left err → do
        let msg = show err
        mapM_ (\frag → unless (frag `isInfixOf` msg) $ expectationFailure
                  ("rejection message did not mention " <> show frag
                   <> ": " <> msg)) fragments

shippedDefs ∷ IO [BuildingYamlDef]
shippedDefs = do
    names ← listDirectory "data/buildings"
    let paths = [ "data/buildings" </> n | n ← sort names
                , takeExtension n ≡ ".yaml" ]
    concat ⊚ mapM (\p → do
        parsed ← Yaml.decodeFileEither p
        case parsed of
            Right file → pure (byfBuildings file)
            Left err → fail (p <> " failed to decode: " <> show err)
        ) paths

handle ∷ Int → TextureHandle
handle = TextureHandle

-- | A 4-frame animation whose handle ids encode the frame index, so an
--   assertion names the frame that was picked.
clip ∷ Int → Float → BuildingAnimation
clip base fps = BuildingAnimation
    { banFps = fps, banLoop = False
    , banFrames = legacyAssets
        (V.fromList [ handle (base + i) | i ← [0 .. 3] ]) }

baseDef ∷ BuildingDef
baseDef = BuildingDef
    { bdName = "probe_hall", bdDisplayName = "Probe Hall"
    , bdCategory = "Test", bdDescription = ""
    , bdTextures = legacyAssets (handle 1), bdIconTexture = handle 2
    , bdTileW = 1, bdTileH = 1
    , bdPlacement = "flat_ground", bdIsStarting = False, bdRace = "acolyte"
    , bdSpriteAnchor = "diamond_bottom", bdBuildWork = 0
    , bdMaterials = HM.empty, bdStorageCapacity = 0, bdOperations = []
    , bdAnimations = HM.empty, bdRoleAnims = Map.empty
    , bdVisualClass = IndoorFixture
    , bdPowerDrain = 0, bdPowerNode = Nothing
    }

-- | Worker-driven: 120 s of work, a 4-frame construction clip.
workerDef ∷ BuildingDef
workerDef = baseDef
    { bdBuildWork = 120
    , bdRoleAnims = Map.singleton RoleConstruction "raise"
    , bdAnimations = HM.singleton "raise" (clip 10 4)
    }

-- | Zero-work: a 4-frame appearance clip at 2 fps = 2 game-seconds.
timedDef ∷ BuildingDef
timedDef = baseDef
    { bdBuildWork = 0
    , bdRoleAnims = Map.singleton RoleAppearance "unfold"
    , bdAnimations = HM.singleton "unfold" (clip 20 2)
    }

instanceAt ∷ Double → Float → BuildingInstance
instanceAt spawnedAt progress = BuildingInstance
    { biDefName = "probe_hall", biPage = WorldPageId "p"
    , biTexture = handle 1, biAnchorX = 0, biAnchorY = 0, biGridZ = 0
    , biSpawnedAt = spawnedAt, biTileW = 1, biTileH = 1
    , biSpawnRemaining = 0, biBuildProgress = progress
    , biMaterialsDelivered = HM.empty, biStorage = []
    }

-- | One fixture YAML through the REAL Lua entry point, restoring the
--   shared engine's own building definitions afterwards. Asserting on a
--   hand-built conversion would prove nothing about the loader, which
--   is where the pre-#2080 collapse lived.
runLoader ∷ EngineEnv → FilePath → BS.ByteString
          → IO ([LuaToEngineMsg], BuildingDef)
runLoader env fileName bytes = do
    let defsRef = bcBuildingManagerRef (toBuildingCapability env)
    bm0 ← readIORef defsRef
    let restore = atomicModifyIORef' defsRef $ \bm →
            (bm { bmDefs = bmDefs bm0 }, ())
    tmp ← getTemporaryDirectory
    let path = tmp </> "synarchy-bda1-" <> fileName
    BS.writeFile path bytes
    (`finally` (restore ≫ removeFile path)) $ do
        ls ← newBareLuaBackend env
        _ ← Q.flushQueue (luaToEngineQueue env)
        _ ← executeDebugLua (lbsLuaState ls)
                ("return engine.loadBuildingYaml('" <> T.pack path <> "')")
        msgs ← Q.flushQueue (luaToEngineQueue env)
        bm ← readIORef defsRef
        case HM.lookup "probe_hall" (bmDefs bm) of
            Just d → pure (msgs, d)
            Nothing → fail "the loader registered no probe_hall definition"

-- | A bare Lua state with the real engine API bound to this env, so the
--   fixture travels the SAME @engine.loadBuildingYaml@ entry point the
--   game boots through.
newBareLuaBackend ∷ EngineEnv → IO LuaBackendState
newBareLuaBackend env = do
    ls ← createLuaBackendState (luaToEngineQueue env) (luaQueue env)
                              (assetPoolRef env) (nextObjectIdRef env)
                              (inputStateRef env) (loggerRef env)
    stateRef ← newIORef ThreadRunning
    registerLuaAPI (lbsLuaState ls) env ls stateRef
    pure ls

nubOrd ∷ Eq a ⇒ [a] → [a]
nubOrd = go []
  where
    go seen [] = reverse seen
    go seen (x : xs)
        | x `elem` seen = go seen xs
        | otherwise     = go (x : seen) xs
