{-# LANGUAGE Strict, UnicodeSyntax, OverloadedStrings #-}
-- | "Location map icons" (#781): paired undiscovered/discovered
--   zoom-map annotation textures — the @map_icons@ YAML schema
--   ('Engine.Asset.YamlLocations'), texture-name resolution
--   ('World.Render.Zoom.Icons.buildLocationIconMap'), and the pure,
--   GPU-free icon-quad generator ('World.Render.Zoom.Icons.
--   makeLocationIconQuads') — mirroring 'Test.Headless.Location.Bounds'
--   / 'Test.Headless.Location.Discovery' fixture style. No engine
--   needed: 'makeLocationIconQuads' is a pure function over world/
--   camera/discovery params, exactly like the existing terrain
--   'World.Render.Zoom.Quads.makeMapQuads' it sits beside.
module Test.Headless.Location.MapIcons (spec) where

import UPrelude
import Test.Hspec
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import qualified Data.Vector as V
import qualified Data.Yaml as Yaml
import Engine.Asset.Handle (TextureHandle(..))
import Engine.Asset.YamlTextures (emptyTextureNameRegistry)
import Engine.Graphics.Camera (CameraFacing(..))
import Engine.Graphics.Vulkan.Types.Vertex (Vertex(..), Vec2(..), Vec4(..))
import Engine.Scene.Base (LayerId(..))
import Engine.Scene.Types (SortableQuad(..))
import Engine.Asset.YamlLocations (LocationYamlDef(..), LocationYamlFile(..))
import Location.Types
    ( LocationDef(..), LocationRegistry, emptyLocationRegistry
    , registerLocation, locationIconTextureName
    )
import Location.Overlay.Types (LocationOverlay)
import Location.Bounds (RelBounds(..))
import World.Chunk.Types (ChunkCoord(..))
import World.Grid (gridToWorld)
import World.Generate.Types (WorldGenParams(..), defaultWorldGenParams)
import World.Render.Zoom.ViewBounds (ZoomViewBounds(..), bestZoomWrapOffset)
import World.Render.Zoom.Icons
    ( locationIconTargetPixels, iconWorldSize, buildLocationIconMap
    , makeLocationIconQuads
    )
import Test.Headless.Location.Bounds (decodeDef, rejectedNaming)

-- * Fixtures

-- | One ruin-shaped def at chunk (0,0) → anchor tile (8,8), matching
--   'Test.Headless.Location.Discovery''s fixture shape.
locDef ∷ Text → Maybe (Text, Text) → LocationDef
locDef lid icons = LocationDef
    { ldId              = lid
    , ldLabel           = "Test Ruin"
    , ldType            = "ruin"
    , ldBuilder         = "room_small"
    , ldAnchor          = []
    , ldMaxCount        = 0
    , ldMinSpacing      = 0
    , ldContents        = []
    , ldBounds          = RelBounds (-2) (-2) 2 2
    , ldDiscoveryMargin = 6
    , ldMapIcons        = icons
    }

undiscoveredTex, discoveredTex, fallbackTex ∷ TextureHandle
undiscoveredTex = TextureHandle 101
discoveredTex   = TextureHandle 102
fallbackTex     = TextureHandle 1

registryWithIcons ∷ LocationRegistry
registryWithIcons =
    registerLocation (locDef "loc1" (Just ("hidden.png", "discovered.png")))
        emptyLocationRegistry

overlayAt ∷ Int → Int → Text → LocationOverlay
overlayAt cx cy lid = HM.singleton (ChunkCoord cx cy) lid

iconMap1 ∷ HM.HashMap Text (TextureHandle, TextureHandle)
iconMap1 = HM.singleton "loc1" (undiscoveredTex, discoveredTex)

-- | Wide-open view bounds — every test below cares about wrap/culling
--   behaviour through explicit scenarios, not accidental clipping.
openView ∷ ZoomViewBounds
openView = ZoomViewBounds
    { zvLeft = -1.0e9, zvRight = 1.0e9, zvTop = -1.0e9, zvBottom = 1.0e9 }

paramsWith ∷ LocationOverlay → HS.HashSet ChunkCoord → WorldGenParams
paramsWith overlay discovered = defaultWorldGenParams
    { wgpLocationOverlay = overlay
    , wgpLocationDiscovered = discovered
    }

-- | Run the generator with FaceSouth, camera at the origin, full alpha,
--   a fixed 4-unit icon size — the common case most scenarios below
--   only need to vary the overlay/discovered-set/camera for.
runDefault ∷ LocationOverlay → HS.HashSet ChunkCoord → V.Vector SortableQuad
runDefault overlay discovered =
    makeLocationIconQuads (paramsWith overlay discovered) iconMap1
        FaceSouth openView 0 0 1.0 4.0 (LayerId 2) (\(TextureHandle n) → n) (-1)

spec ∷ Spec
spec = describe "Location map icons" $ do

    describe "map_icons YAML schema (#781)" $ do
        it "parses a valid undiscovered/discovered pair" $
            case decodeDef
                    "{ id: t, builder: b, discovery_margin: 6,\
                    \  bounds: { min_x: -2, min_y: -2, max_x: 2, max_y: 2 },\
                    \  map_icons: { undiscovered: a.png, discovered: b.png } }" of
                Left err → expectationFailure err
                Right def → lydMapIcons def `shouldBe` Just ("a.png", "b.png")

        it "no map_icons block parses as Nothing (no annotation)" $
            case decodeDef
                    "{ id: t, builder: b, discovery_margin: 6,\
                    \  bounds: { min_x: -2, min_y: -2, max_x: 2, max_y: 2 } }" of
                Left err → expectationFailure err
                Right def → lydMapIcons def `shouldBe` Nothing

        it "rejects a map_icons block missing 'discovered', naming the location" $
            decodeDef
                "{ id: t, builder: b, discovery_margin: 6,\
                \  bounds: { min_x: -2, min_y: -2, max_x: 2, max_y: 2 },\
                \  map_icons: { undiscovered: a.png } }"
                `shouldSatisfy` rejectedNaming "t"

        it "rejects a map_icons block missing 'undiscovered', naming the location" $
            decodeDef
                "{ id: t, builder: b, discovery_margin: 6,\
                \  bounds: { min_x: -2, min_y: -2, max_x: 2, max_y: 2 },\
                \  map_icons: { discovered: b.png } }"
                `shouldSatisfy` rejectedNaming "t"

        it "rejects a non-object map_icons value, naming the location" $
            decodeDef
                "{ id: t, builder: b, discovery_margin: 6,\
                \  bounds: { min_x: -2, min_y: -2, max_x: 2, max_y: 2 },\
                \  map_icons: nope }"
                `shouldSatisfy` rejectedNaming "t"

        it "the shipped ruin_small.yaml declares its map_icons pair" $ do
            result ← Yaml.decodeFileEither "data/locations/ruin_small.yaml"
            case result of
                Left err → expectationFailure (show (err ∷ Yaml.ParseException))
                Right lf → case lyfLocations lf of
                    [def] → lydMapIcons def `shouldBe` Just
                        ( "assets/textures/icons/location/ruin_hidden.png"
                        , "assets/textures/icons/location/ruin_discovered.png"
                        )
                    ds → expectationFailure
                        ("expected exactly one location def, got " <> show (length ds))

    describe "buildLocationIconMap" $ do
        it "a def with no ldMapIcons contributes no entry" $
            HM.lookup "loc1"
                (buildLocationIconMap
                    (registerLocation (locDef "loc1" Nothing) emptyLocationRegistry)
                    emptyTextureNameRegistry fallbackTex)
                `shouldBe` Nothing

        it "resolves both states via the registry's shared naming convention" $ do
            let reg = HM.fromList
                    [ (locationIconTextureName "loc1" False, undiscoveredTex)
                    , (locationIconTextureName "loc1" True,  discoveredTex)
                    ]
            buildLocationIconMap registryWithIcons reg fallbackTex
                `shouldBe` HM.singleton "loc1" (undiscoveredTex, discoveredTex)

        it "missing/not-yet-loaded textures fall back to the caller's handle, \
           \never dropping the entry" $
            buildLocationIconMap registryWithIcons emptyTextureNameRegistry fallbackTex
                `shouldBe` HM.singleton "loc1" (fallbackTex, fallbackTex)

    describe "makeLocationIconQuads: texture selection" $ do
        it "an undiscovered placement selects the undiscovered texture" $
            V.map sqTexture (runDefault (overlayAt 0 0 "loc1") HS.empty)
                `shouldBe` V.singleton undiscoveredTex

        it "a discovered placement selects the discovered texture" $
            V.map sqTexture
                (runDefault (overlayAt 0 0 "loc1") (HS.singleton (ChunkCoord 0 0)))
                `shouldBe` V.singleton discoveredTex

        it "a state change re-selects the texture live, with no other input changed" $ do
            let overlay = overlayAt 0 0 "loc1"
                before = runDefault overlay HS.empty
                after  = runDefault overlay (HS.singleton (ChunkCoord 0 0))
            V.map sqTexture before `shouldBe` V.singleton undiscoveredTex
            V.map sqTexture after  `shouldBe` V.singleton discoveredTex

        it "a location id absent from iconMap (no map_icons declared) renders nothing" $
            V.null (makeLocationIconQuads (paramsWith (overlayAt 0 0 "no_icons") HS.empty)
                iconMap1 FaceSouth openView 0 0 1.0 4.0 (LayerId 2)
                (\(TextureHandle n) → n) (-1))
                `shouldBe` True

    describe "makeLocationIconQuads: alpha (fade transition)" $ do
        let alphaOf v = case V.toList v of
                [q] → a (color (sqV0 q))
                qs  → error ("expected exactly one quad, got " <> show (length qs))
            withAlpha alpha = makeLocationIconQuads
                (paramsWith (overlayAt 0 0 "loc1") HS.empty) iconMap1
                FaceSouth openView 0 0 alpha 4.0 (LayerId 2)
                (\(TextureHandle n) → n) (-1)
        it "alpha 0 below the fade start (visually absent)" $
            alphaOf (withAlpha 0.0) `shouldBe` 0.0
        it "a fractional alpha mid-fade carries straight through" $
            alphaOf (withAlpha 0.42) `shouldBe` 0.42
        it "full alpha at full map visibility, independent of ZoomMapMode \
           \(icon color never routes through a mode's color function)" $
            alphaOf (withAlpha 1.0) `shouldBe` 1.0

    describe "iconWorldSize: constant logical screen size" $ do
        -- worldSize -> projected LOGICAL pixels, replaying the full
        -- projection chain 'iconWorldSize's Haddock derives: world units
        -- -> framebuffer pixels (fbH/(2*zoom)) -> logical pixels
        -- (divide by the DPI ratio fbH/winH). Proves the algebra: for
        -- ANY fbH (any DPI/framebuffer scale), the result lands back on
        -- targetPx exactly.
        let projectedLogicalPx targetPx zoom winH fbH =
                iconWorldSize targetPx zoom winH
                    * (fbH / (2.0 * zoom)) / (fbH / winH)
        it "is invariant to framebuffer size at a fixed zoom/window size" $
            forM_ [800.0, 1280.0, 1920.0, 3840.0 ∷ Float] $ \fbH →
                projectedLogicalPx locationIconTargetPixels 1.6 720.0 fbH
                    `shouldSatisfy` (\v → abs (v - locationIconTargetPixels) < 1.0e-3)

        it "is invariant to framebuffer size at a different zoom/window size" $
            forM_ [640.0, 1440.0, 2160.0 ∷ Float] $ \fbH →
                projectedLogicalPx locationIconTargetPixels 3.0 1080.0 fbH
                    `shouldSatisfy` (\v → abs (v - locationIconTargetPixels) < 1.0e-3)

        it "world size scales linearly with zoom (screen size stays constant)" $ do
            let s1 = iconWorldSize locationIconTargetPixels 1.6 720.0
                s2 = iconWorldSize locationIconTargetPixels 3.2 720.0
            s2 `shouldSatisfy` (\v → abs (v - 2.0 * s1) < 1.0e-3)

        it "world size scales inversely with logical window height" $ do
            let s1 = iconWorldSize locationIconTargetPixels 1.6 720.0
                s2 = iconWorldSize locationIconTargetPixels 1.6 1440.0
            s2 `shouldSatisfy` (\v → abs (v - s1 / 2.0) < 1.0e-3)

        it "a degenerate (non-positive) window height yields 0, not a blow-up" $ do
            iconWorldSize locationIconTargetPixels 1.6 0.0 `shouldBe` 0
            iconWorldSize locationIconTargetPixels 1.6 (-10.0) `shouldBe` 0

    describe "makeLocationIconQuads: screen-upright + anchor centering" $ do
        let iconSize = 4.0 ∷ Float
            quadAt facing = V.head $ makeLocationIconQuads
                (paramsWith (overlayAt 0 0 "loc1") HS.empty) iconMap1
                facing openView 0 0 1.0 iconSize (LayerId 2)
                (\(TextureHandle n) → n) (-1)
            corners q = (pos (sqV0 q), pos (sqV1 q), pos (sqV2 q), pos (sqV3 q))

        forM_ [FaceSouth, FaceWest, FaceNorth, FaceEast] $ \facing →
            it ("is an axis-aligned upright square for " <> show facing) $ do
                let (v0, v1, v2, v3) = corners (quadAt facing)
                y v0 `shouldBe` y v1
                x v0 `shouldBe` x v3
                x v1 `shouldBe` x v2
                y v2 `shouldBe` y v3
                (x v1 - x v0) `shouldSatisfy` (\d → abs (d - iconSize) < 1.0e-4)
                (y v3 - y v0) `shouldSatisfy` (\d → abs (d - iconSize) < 1.0e-4)

        forM_ [FaceSouth, FaceWest, FaceNorth, FaceEast] $ \facing →
            it ("is centered on the transformed anchor tile for " <> show facing) $ do
                let (v0, v1, _, v3) = corners (quadAt facing)
                    centerX = (x v0 + x v1) / 2.0
                    centerY = (y v0 + y v3) / 2.0
                    (ax, ay) = gridToWorld facing 8 8   -- chunk (0,0) center
                abs (centerX - ax) `shouldSatisfy` (< 1.0e-3)
                abs (centerY - ay) `shouldSatisfy` (< 1.0e-3)

    describe "makeLocationIconQuads: cylindrical wrap (seam)" $ do
        it "wraps to the nearest visible copy, matching bestZoomWrapOffset directly" $ do
            let ws = wgpWorldSize defaultWorldGenParams
                (ax, ay) = gridToWorld FaceSouth 8 8
                -- Camera sitting far in +X — bestZoomWrapOffset should
                -- select the wrapped (+w) copy over the raw one, exactly
                -- like a terrain chunk at the same anchor would.
                camX = ax + 1.0e6
                camY = ay
                (expOffX, _) = bestZoomWrapOffset FaceSouth ws camX camY ax ay
                overlay = overlayAt 0 0 "loc1"
                params  = paramsWith overlay HS.empty
                q = V.head $ makeLocationIconQuads params iconMap1
                        FaceSouth openView camX camY 1.0 4.0 (LayerId 2)
                        (\(TextureHandle n) → n) (-1)
                centerX = (x (pos (sqV0 q)) + x (pos (sqV1 q))) / 2.0
            abs (centerX - (ax + expOffX)) `shouldSatisfy` (< 1.0e-2)

        it "never emits more than one quad per placement, even when both the \
           \raw and wrapped copies would fit in view" $
            V.length (runDefault (overlayAt 0 0 "loc1") HS.empty) `shouldBe` 1

    describe "makeLocationIconQuads: multiple independent pages" $
        it "the same id/coord on two 'pages' (two WorldGenParams) selects each \
           \page's own discovery state independently" $ do
            let overlay = overlayAt 0 0 "loc1"
                pageA = runDefault overlay HS.empty
                pageB = runDefault overlay (HS.singleton (ChunkCoord 0 0))
            V.map sqTexture pageA `shouldBe` V.singleton undiscoveredTex
            V.map sqTexture pageB `shouldBe` V.singleton discoveredTex

    describe "makeLocationIconQuads: deterministic ordering" $ do
        let iconMap3 = HM.fromList
                [ ("loc1", (TextureHandle 11, TextureHandle 21))
                , ("loc2", (TextureHandle 12, TextureHandle 22))
                , ("loc3", (TextureHandle 13, TextureHandle 23))
                ]
            overlay3 = HM.fromList
                [ (ChunkCoord 2 (-1), "loc1")
                , (ChunkCoord (-3) 4, "loc2")
                , (ChunkCoord 0 0,    "loc3")
                ]
            run () = makeLocationIconQuads
                (paramsWith overlay3 HS.empty) iconMap3
                FaceSouth openView 0 0 1.0 4.0 (LayerId 2)
                (\(TextureHandle n) → n) (-1)
        it "matches overlayToList's sorted-by-(cx,cy) order" $
            -- (cx,cy): loc2@(-3,4) < loc3@(0,0) < loc1@(2,-1)
            V.map sqTexture (run ())
                `shouldBe` V.fromList
                    [TextureHandle 12, TextureHandle 13, TextureHandle 11]
        it "repeated calls with unchanged inputs never reorder or flicker" $ do
            let quadKey q = (sqSortKey q, sqTexture q, x (pos (sqV0 q)), y (pos (sqV0 q)))
            V.map quadKey (run ()) `shouldBe` V.map quadKey (run ())
