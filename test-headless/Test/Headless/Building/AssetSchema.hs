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
import Control.Exception (evaluate, finally)
import Data.Foldable (toList)
import Data.IORef (atomicModifyIORef', readIORef, newIORef)
import Data.List (isInfixOf, sort)
import qualified Data.ByteString.Char8 as BS
import qualified Data.HashMap.Strict as HM
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import qualified Data.Vector as V
import qualified Data.Yaml as Yaml
import Building.Visual (pickBuildingFrame)
import Building.Schema
import Building.Types
import Engine.Asset.Handle (TextureHandle(..))
import Engine.Asset.YamlBuildings
import Engine.Core.Log (LogConfig(..), defaultLogConfig, initLogger)
import Engine.Core.Log.Types (LogEntry(..))
import Engine.Preview.Building
    (BuildingPreviewMeta(..), emptyBuildingPreviewMeta, loadBuildingPreviewMeta)
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
import System.Directory
    ( createDirectoryIfMissing, getTemporaryDirectory, listDirectory
    , removeDirectoryRecursive, removeFile, withCurrentDirectory )
import System.FilePath (takeExtension, (</>))
import Test.Headless.Harness (withHeadlessEngine)
import Test.Headless.Harness.Log (newLogCapture)
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

    it "rejects one path claimed by two facings in `sprites`" $
        -- A canonical block repeating a path would be indistinguishable
        -- from the legacy branch in the runtime views while claiming to
        -- be real four-facing art, which is what makes `AssetLegacy`
        -- worth recording at all.
        decodeYaml (buildingYaml
            [ "    sprites:"
            , "      south: \"s.png\""
            , "      west: \"s.png\""
            , "      north: \"n.png\""
            , "      east: \"e.png\""
            ]) `shouldFailWith`
                ["probe_hall", "sprites", "s.png", "south", "west"]

    it "still accepts a legacy `sprite` reaching all four views" $ do
        -- The alias rule is CANONICAL-only: the compatibility branch's
        -- whole job is one path through four views.
        def ← decodeOne (buildingYaml [ "    sprite: \"legacy.png\"" ])
        toList (faViews (bydSprites def)) `shouldBe` replicate 4 "legacy.png"

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

    it "names an unknown frame key even when it is the ONLY key" $
        -- The lone-unknown-key block is the case that would otherwise
        -- fall through to the generic "declares no direction" message
        -- without ever telling the author which key was wrong.
        decodeYaml (asymmetricYaml
            [ "      walkabout:"
            , "        frames:"
            , "          up: [\"u0.png\"]"
            ]) `shouldFailWith`
                ["probe_hall", "walkabout", "up", "unknown", "south"]

    it "names an unknown frame key beside the legacy `default` list" $
        decodeYaml (asymmetricYaml
            [ "      walkabout:"
            , "        frames:"
            , "          default: [\"d0.png\"]"
            , "          up: [\"u0.png\"]"
            ]) `shouldFailWith` ["probe_hall", "walkabout", "up", "unknown"]

    it "rejects an empty `frames` block, naming no key it cannot name" $
        decodeYaml (asymmetricYaml
            [ "      walkabout:"
            , "        frames: {}"
            ]) `shouldFailWith`
                ["probe_hall", "walkabout", "no direction"]

    it "rejects an animation with no `frames` at all" $
        decodeYaml (asymmetricYaml
            [ "      walkabout:"
            , "        fps: 4"
            ]) `shouldFailWith` ["probe_hall", "walkabout", "no `frames`"]

    it "decodes all four lifecycle roles, independently" $ do
        -- Including `destruction`, which 'Building.Destruction' plays
        -- as a transient presentation after demolition (BDA-3, #2091):
        -- it has to DECODE here or the art slices have nowhere to
        -- declare it. Four distinct animation names, so a
        -- decoder collapsing two roles onto one would lose a value
        -- rather than merely reorder the map.
        def ← decodeOne (buildingYaml
            [ "    sprite: \"legacy.png\""
            , "    build_work: 100.0"
            , "    state_animations:"
            , "      construction: raise"
            , "      appearance: unfold"
            , "      built: hum"
            , "      destruction: crumble"
            ])
        bydRoleAnims def `shouldBe` Map.fromList
            [ (RoleConstruction, "raise"), (RoleAppearance, "unfold")
            , (RoleBuilt, "hum"), (RoleDestruction, "crumble") ]
        -- The map's own key order is the role order, and the vocabulary
        -- is closed to exactly these four.
        map roleKey (Map.keys (bydRoleAnims def))
            `shouldBe` ["construction", "appearance", "built", "destruction"]
        roleKeyList `shouldBe`
            ["construction", "appearance", "built", "destruction"]
        map roleFromKey roleKeyList `shouldBe`
            map Just [RoleConstruction, RoleAppearance, RoleBuilt, RoleDestruction]

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

    describe "numeric domains (#2347)" numericDomainSpec

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

-- * Numeric domains

-- | #2347: every number a definition authors is checked at THIS
--   boundary, against the value the engine will actually STORE.
--
--   Two things make these examples worth more than the domain
--   predicates they mirror:
--
--   1. Each asserts on the MESSAGE, so a rule that fires without
--      naming the building, the animation, the key and the authored
--      value fails here. A @materials@ count additionally names the
--      material, because one bad entry in a map is otherwise
--      unfindable from the diagnostic.
--   2. The rejected set is bounded from below by the ACCEPTED cases:
--      every documented default and every value exactly ON a boundary
--      still loads, so a domain that drifted one comparison too far
--      (@> 0@ where @≥ 0@ belongs, or the reverse) fails here rather
--      than silently refusing the shipped corpus.
numericDomainSpec ∷ Spec
numericDomainSpec = do
    describe "an animation's fps" $ do
        it "rejects a negative fps on a non-looping animation" $
            decodeYaml (animYaml [ "        fps: -4"
                                 , "        loop: false" ])
                `shouldFailWith`
                    [ "probe_hall", "idle", "fps", "strictly positive"
                    , "-4.0" ]

        it "rejects a zero fps" $
            decodeYaml (animYaml [ "        fps: 0" ]) `shouldFailWith`
                [ "probe_hall", "idle", "fps", "strictly positive", "0.0" ]

        it "rejects `.nan`, which YAML resolves to a STRING" $
            decodeYaml (animYaml [ "        fps: .nan" ]) `shouldFailWith`
                [ "probe_hall", "idle", "fps", ".nan" ]

        it "rejects `.inf`, likewise a string" $
            decodeYaml (animYaml [ "        fps: .inf" ]) `shouldFailWith`
                [ "probe_hall", "idle", "fps", ".inf" ]

        it "rejects a literal that OVERFLOWS to infinity as a Float" $
            -- 1.0e+100 is a perfectly ordinary Scientific: the check has
            -- to run on the narrowed 32-bit value the engine stores, or
            -- an infinite fps reaches gameplay.
            decodeYaml (animYaml [ "        fps: 1.0e+100" ])
                `shouldFailWith`
                    [ "probe_hall", "idle", "fps", "finite", "1.0e100" ]

        it "rejects a positive literal that UNDERFLOWS to zero" $
            -- The mirror case: strictly positive as authored, exactly 0
            -- once stored, which is the frozen animation the domain
            -- exists to exclude.
            decodeYaml (animYaml [ "        fps: 1.0e-50" ])
                `shouldFailWith`
                    [ "probe_hall", "idle", "fps", "strictly positive"
                    , "0.0" ]

        it "rejects an explicitly authored `fps: null`" $
            -- Aeson reads `key: null` as ABSENT, so only reading the
            -- whole value keeps an authored null from silently
            -- selecting the default.
            decodeYaml (animYaml [ "        fps: null" ]) `shouldFailWith`
                [ "probe_hall", "idle", "fps", "Null" ]

    describe "tile_size" $ do
        it "rejects an `x` below 1" $
            decodeYaml (numericYaml [ "    tile_size: { x: 0, y: 1 }" ])
                `shouldFailWith`
                    [ "probe_hall", "tile_size.x", "at least 1", "0" ]

        it "rejects a `y` below 1" $
            decodeYaml (numericYaml [ "    tile_size: { x: 1, y: -2 }" ])
                `shouldFailWith`
                    [ "probe_hall", "tile_size.y", "at least 1", "-2" ]

        it "rejects a fractional tile dimension" $
            decodeYaml (numericYaml [ "    tile_size: { x: 1.5, y: 1 }" ])
                `shouldFailWith`
                    [ "probe_hall", "tile_size.x", "whole number", "1.5" ]

        it "rejects an explicitly authored `tile_size: null`" $
            decodeYaml (numericYaml [ "    tile_size: null" ])
                `shouldFailWith` [ "probe_hall", "tile_size", "Null" ]

    describe "the non-negative floats" $ do
        it "rejects a negative `build_work`" $
            decodeYaml (numericYaml [ "    build_work: -1" ])
                `shouldFailWith`
                    [ "probe_hall", "build_work", "non-negative", "-1.0" ]

        it "rejects a non-finite `build_work`" $
            decodeYaml (numericYaml [ "    build_work: .inf" ])
                `shouldFailWith` [ "probe_hall", "build_work", ".inf" ]

        it "rejects a NaN `storage_capacity`" $
            decodeYaml (numericYaml [ "    storage_capacity: .nan" ])
                `shouldFailWith`
                    [ "probe_hall", "storage_capacity", ".nan" ]

        it "rejects a negative `storage_capacity`" $
            -- A negative capacity is not merely odd: every deposit
            -- against it refuses, so the building silently stops being
            -- storage at all.
            decodeYaml (numericYaml [ "    storage_capacity: -0.5" ])
                `shouldFailWith`
                    [ "probe_hall", "storage_capacity", "non-negative"
                    , "-0.5" ]

        it "rejects a negative `power_drain`" $
            decodeYaml (numericYaml [ "    power_drain: -3.5" ])
                `shouldFailWith`
                    [ "probe_hall", "power_drain", "non-negative", "-3.5" ]

        it "rejects a non-finite `power_drain`" $
            decodeYaml (numericYaml [ "    power_drain: 1.0e+100" ])
                `shouldFailWith`
                    [ "probe_hall", "power_drain", "finite", "1.0e100" ]

    describe "materials counts" $ do
        it "rejects a zero count, naming the MATERIAL as well" $
            -- The bad entry is the second of two, so a diagnostic that
            -- named only the building would not locate it.
            decodeYaml (numericYaml
                [ "    materials:"
                , "      wood_log: 2"
                , "      steel_bar: 0"
                ]) `shouldFailWith`
                    [ "probe_hall", "materials", "steel_bar", "at least 1"
                    , "0" ]

        it "rejects a negative count" $
            decodeYaml (numericYaml
                [ "    materials:"
                , "      wood_log: -3"
                ]) `shouldFailWith`
                    [ "probe_hall", "materials", "wood_log", "at least 1"
                    , "-3" ]

        it "rejects an explicitly authored `materials: null`" $
            decodeYaml (numericYaml [ "    materials: null" ])
                `shouldFailWith` [ "probe_hall", "materials", "Null" ]

    describe "what the domains still ACCEPT" $ do
        it "keeps every documented default when the key is omitted" $ do
            def ← decodeOne (animYaml [])
            bydBuildWork def `shouldBe` 0
            bydStorageCapacity def `shouldBe` 0
            bydPowerDrain def `shouldBe` 0
            bydTileSize def `shouldBe` BuildingYamlTileSize 1 1
            bydMaterials def `shouldBe` Map.empty
            anim ← lookupAnim "idle" def
            byaFps anim `shouldBe` 8

        it "accepts every value exactly ON a boundary" $ do
            def ← decodeOne (numericYaml
                [ "    tile_size: { x: 1, y: 1 }"
                , "    build_work: 0"
                , "    storage_capacity: 0"
                , "    power_drain: 0"
                , "    materials:"
                , "      wood_log: 1"
                , "    animations:"
                , "      idle:"
                , "        fps: 8"
                , "        frames: { default: [\"a.png\"] }"
                ])
            bydTileSize def `shouldBe` BuildingYamlTileSize 1 1
            bydBuildWork def `shouldBe` 0
            bydStorageCapacity def `shouldBe` 0
            bydPowerDrain def `shouldBe` 0
            bydMaterials def `shouldBe` Map.singleton "wood_log" 1
            anim ← lookupAnim "idle" def
            byaFps anim `shouldBe` 8

        it "accepts an explicitly EMPTY materials block as a free build" $ do
            def ← decodeOne (numericYaml [ "    materials: {}" ])
            bydMaterials def `shouldBe` Map.empty

    describe "the whole-file contract" $
        it "refuses the file entirely, sparing not even a valid sibling" $ do
            -- Engine.Asset.YamlList is all-or-nothing: one invalid
            -- declaration costs the file, and the warning has to name
            -- BOTH the file (which YamlList supplies) and the offending
            -- declaration (which only the decoder can).
            tmp ← getTemporaryDirectory
            let path = tmp </> "synarchy-2347-whole-file.yaml"
            BS.writeFile path siblingYaml
            (backend, drain) ← newLogCapture
            logger ← initLogger defaultLogConfig { lcBackend = backend }
            outcome ← loadBuildingYamlOutcome logger path
                          `finally` removeFile path
            outcome `shouldBe` Nothing
            entries ← drain
            let warned = unwords (map (T.unpack ∘ leMessage) entries)
            mapM_ (\frag → unless (frag `isInfixOf` warned) $
                      expectationFailure
                          ("the warning did not mention " <> show frag
                           <> ": " <> warned))
                  [ "synarchy-2347-whole-file.yaml", "probe_hall", "fps"
                  , "strictly positive" ]
            -- The valid sibling is named in the fixture and nowhere in
            -- the outcome: nothing partial survives the refusal.
            "good_hall" `isInfixOf` show outcome `shouldBe` False

    describe "the motivating crash" $
        it "makes the out-of-bounds `built` clip unauthorable" $ do
            -- The defect, first: a `built` role is selected through
            -- timeIdx, whose non-looping branch clamps only from ABOVE.
            -- A negative fps at positive elapsed time therefore reaches
            -- `V.!` with a negative index. BuildingDef is a public
            -- constructor, so this stays reachable by hand and stays
            -- pinned here.
            evaluate (pickBuildingFrame FaceSouth 1.0 (instanceAt 0 0)
                                        negativeFpsBuiltDef)
                `shouldThrow` anyErrorCall
            -- The fix: the same declaration can no longer be authored.
            decodeYaml (numericYaml
                [ "    build_work: 0"
                , "    state_animations:"
                , "      built: flicker"
                , "    animations:"
                , "      flicker:"
                , "        fps: -4"
                , "        loop: false"
                , "        frames: { default: [\"a.png\"] }"
                ]) `shouldFailWith`
                    [ "probe_hall", "flicker", "fps", "strictly positive" ]

    describe "the preview decoder's tolerance" $ do
        it "still browses a building whose fps the game now refuses" $
            withPreviewRoot invalidFpsPreviewYaml $ do
                meta ← loadBuildingPreviewMeta "probe_hall"
                -- A refused file is preview METADATA lost, never a
                -- building missing from the asset browser: the viewer
                -- falls back to its documented defaults.
                meta `shouldBe` emptyBuildingPreviewMeta

        it "still reads the metadata of a building it CAN parse" $
            -- Without this the example above would pass against a
            -- fixture the loader never found at all.
            withPreviewRoot validFpsPreviewYaml $ do
                meta ← loadBuildingPreviewMeta "probe_hall"
                Map.keys (bpmAnims meta) `shouldBe` ["idle"]
                fmap byaFps (Map.lookup "idle" (bpmAnims meta))
                    `shouldBe` Just 4

-- | A definition with the four required keys plus a legacy sprite, so a
--   numeric fixture is rejected for its NUMBER rather than for a
--   missing sprite the def-level checks reach first.
numericYaml ∷ [BS.ByteString] → BS.ByteString
numericYaml body = buildingYaml ([ "    sprite: \"a.png\"" ] ⧺ body)

-- | 'numericYaml' plus one minimal animation named @idle@, whose body
--   lines the caller supplies.
animYaml ∷ [BS.ByteString] → BS.ByteString
animYaml animBody = numericYaml $
    [ "    animations:"
    , "      idle:"
    ] ⧺ animBody ⧺
    [ "        frames: { default: [\"a.png\"] }" ]

-- | One file, two declarations: an invalid @probe_hall@ and a perfectly
--   valid @good_hall@ that must NOT survive it.
siblingYaml ∷ BS.ByteString
siblingYaml = BS.unlines
    [ "buildings:"
    , "  - name: \"probe_hall\""
    , "    visual_class: \"indoor_fixture\""
    , "    sprite: \"a.png\""
    , "    animations:"
    , "      idle:"
    , "        fps: 0"
    , "        frames: { default: [\"a.png\"] }"
    , "  - name: \"good_hall\""
    , "    visual_class: \"indoor_fixture\""
    , "    sprite: \"b.png\""
    ]

-- | The hand-built definition the decoder can no longer produce: a
--   `built` role at a negative fps, which 'pickBuildingFrame' selects
--   through timeIdx and indexes out of bounds at any positive elapsed
--   time.
negativeFpsBuiltDef ∷ BuildingDef
negativeFpsBuiltDef = baseDef
    { bdBuildWork = 0
    , bdRoleAnims = Map.singleton RoleBuilt "flicker"
    , bdAnimations = HM.singleton "flicker" (clip 30 (-4))
    }

-- | A building YAML the game's decoder now refuses, for the preview.
invalidFpsPreviewYaml ∷ BS.ByteString
invalidFpsPreviewYaml = BS.unlines
    [ "buildings:"
    , "  - name: \"probe_hall\""
    , "    visual_class: \"indoor_fixture\""
    , "    sprite: \"a.png\""
    , "    animations:"
    , "      idle:"
    , "        fps: -4"
    , "        frames: { default: [\"a.png\"] }"
    ]

-- | The same file with a valid fps, so the tolerance example above is
--   not passing against a fixture that was never read.
validFpsPreviewYaml ∷ BS.ByteString
validFpsPreviewYaml = BS.unlines
    [ "buildings:"
    , "  - name: \"probe_hall\""
    , "    visual_class: \"indoor_fixture\""
    , "    sprite: \"a.png\""
    , "    animations:"
    , "      idle:"
    , "        fps: 4"
    , "        frames: { default: [\"a.png\"] }"
    ]

-- | Run an action against a scratch RESOURCE ROOT holding exactly one
--   building YAML. 'Engine.Preview.Building.buildingDataPath' is
--   cwd-relative, so the only way to exercise the real
--   'loadBuildingPreviewMeta' on a fixture is to point the working
--   directory at a root the fixture owns — the same technique
--   'Test.Headless.Harness.Isolation' uses, narrowed to the one file
--   this decoder reads.
withPreviewRoot ∷ BS.ByteString → IO a → IO a
withPreviewRoot bytes act = do
    tmp ← getTemporaryDirectory
    let root = tmp </> "synarchy-2347-preview-root"
        dir  = root </> "data" </> "buildings"
    createDirectoryIfMissing True dir
    BS.writeFile (dir </> "probe_hall.yaml") bytes
    withCurrentDirectory root act `finally` removeDirectoryRecursive root

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

    it "keeps every numeric value the #2347 domains now check" $ do
        -- The corpus is the lower bound on the domains: if a check ever
        -- drifted to refuse a shipped value, the whole file would stop
        -- loading and these are the numbers that would go missing.
        defs ← shippedDefs
        let numbers d = ( bydName d
                        , bydBuildWork d, bydStorageCapacity d
                        , bydPowerDrain d
                        , (bytsX (bydTileSize d), bytsY (bydTileSize d)) )
        map numbers defs `shouldMatchList`
            [ ("kitchen",              100, 100, 0, (1, 1))
            , ("workbench",            120, 100, 0, (1, 1))
            , ("machine_shop",         200, 100, 0, (1, 1))
            , ("cargo_hold_S",         240, 200, 0, (1, 1))
            , ("furnace",              180, 100, 0, (1, 1))
            , ("solar_panel",            0,   0, 0, (1, 1))
            , ("high_voltage_battery",   0,   0, 0, (1, 1))
            , ("acolyte_portal",         0,   0, 0, (1, 1))
            ]
        -- Every declared fps, and every material count, still inside
        -- its domain — and non-empty, so this cannot pass vacuously.
        let fpsValues = [ byaFps a | d ← defs, a ← Map.elems (bydAnimations d) ]
            counts    = [ n | d ← defs, n ← Map.elems (bydMaterials d) ]
        fpsValues `shouldSatisfy` (not ∘ null)
        counts `shouldSatisfy` (not ∘ null)
        sort (nubOrd fpsValues) `shouldBe` [4, 8]
        filter (< 1) counts `shouldBe` []

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
        pickBuildingFrame FaceSouth 0 (instanceAt 0 0) workerDef `shouldBe` handle 10
        pickBuildingFrame FaceSouth 9999 (instanceAt 0 0) workerDef `shouldBe` handle 10
        pickBuildingFrame FaceSouth 0 (instanceAt 0 60) workerDef `shouldBe` handle 12
        -- Appearance indexes on elapsed game time and ignores progress.
        pickBuildingFrame FaceSouth 0 (instanceAt 0 0) timedDef `shouldBe` handle 20
        pickBuildingFrame FaceSouth 1.0 (instanceAt 0 0) timedDef `shouldBe` handle 22

    it "pins the last frame of the role the definition's build_work picked" $ do
        -- Neither def declares a `built` animation, so a Built instance
        -- pins its own lifecycle role's final frame rather than snapping
        -- back to the static sprite.
        legacyRoleFor 120 `shouldBe` RoleConstruction
        legacyRoleFor 0 `shouldBe` RoleAppearance
        pickBuildingFrame FaceSouth 0 (instanceAt 0 120) workerDef `shouldBe` handle 13
        pickBuildingFrame FaceSouth 99 (instanceAt 0 0) timedDef `shouldBe` handle 23

    it "renders the static view when no role animation applies" $ do
        let bare = workerDef { bdRoleAnims = Map.empty }
        pickBuildingFrame FaceSouth 0 (instanceAt 0 0) bare `shouldBe` handle 1
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
