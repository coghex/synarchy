{-# LANGUAGE Strict #-}
-- | "Location map icons" (#781, reshaped by #1230): the singular
--   @map_icon@ YAML schema ('Engine.Asset.YamlLocations'), texture-name
--   resolution including the ONE shared unknown marker
--   ('World.Render.Zoom.Icons.buildLocationIconMap'), the explicit
--   per-lifecycle appearance mapping
--   ('World.Render.Zoom.Icons.locationIconAppearance'), and the pure,
--   GPU-free icon-quad generator ('World.Render.Zoom.Icons.
--   makeLocationIconQuads') — mirroring 'Test.Headless.Location.Bounds'
--   / 'Test.Headless.Location.Discovery' fixture style. No engine
--   needed: 'makeLocationIconQuads' is a pure function over world/
--   camera/lifecycle params, exactly like the existing terrain
--   @World.Render.Zoom.Quads.makeMapQuads@ it sits beside.
module Test.Headless.Location.MapIcons (spec) where

import UPrelude
import Test.Hspec
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import qualified Data.Vector as V
import qualified Data.Yaml as Yaml
import System.Directory (doesFileExist)
import Engine.Asset.Handle (TextureHandle(..))
import Engine.Asset.TextureNameRegistry (emptyTextureNameRegistry)
import Engine.Graphics.Camera (CameraFacing(..))
import Engine.Graphics.Vulkan.Types.Vertex (Vertex(..), Vec2(..), Vec4(..))
import Engine.Scene.Base (LayerId(..))
import Engine.Scene.Types (SortableQuad(..))
import Engine.Asset.YamlLocations (LocationYamlDef(..), LocationYamlFile(..))
import Location.Types
    ( LocationDef(..), LocationNaming(..), LocationRegistry
    , emptyLocationRegistry, registerLocation, locationIconTextureName
    , locationUnknownIconTextureName, locationUnknownIconPath
    )
import Location.Overlay.Types (LocationOverlay)
import Location.Instance
    ( LocationInstance(..), LocationInstanceId, LocationInstances
    , LocationLifecycle(..), buildLocationInstances, instancesToList
    , setLocationLifecycle )
import Location.Bounds (RelBounds(..))
import Test.Headless.Location.Fixture (expectGeometry)
import World.Chunk.Types (ChunkCoord(..))
import World.Grid (gridToWorld)
import World.Generate.Types (WorldGenParams(..), defaultWorldGenParams)
import World.Render.Zoom.ViewBounds (ZoomViewBounds(..), bestZoomWrapOffset)
import World.Render.Zoom.Icons
    ( locationIconTargetPixels, iconWorldSize, buildLocationIconMap
    , makeLocationIconQuads, LocationIconSet(..)
    , LocationIconAppearance(..), locationIconAppearance, clearedIconTint
    )
import Test.Headless.Location.Bounds (decodeDef, rejectedNaming)
import Language.Semantic.Types (ConceptId(..))

-- | The naming scheme every 'LocationDef' fixture in this module
--   carries (#1101). One concept per pool is enough: these specs are
--   about geometry, lifecycle, and identity, and every one of them
--   builds instances with NO namer, so the pools are never drawn from.
testNaming ∷ LocationNaming
testNaming = LocationNaming
    { lnHeads     = [ConceptId "KEEP"]
    , lnModifiers = [ConceptId "ASH"]
    }


-- * Fixtures

-- | One ruin-shaped def at chunk (0,0) → anchor tile (8,8), matching
--   'Test.Headless.Location.Discovery''s fixture shape.
locDef ∷ Text → Maybe Text → LocationDef
locDef lid icon = LocationDef
    { ldId              = lid
    , ldLabel           = "Test Ruin"
    , ldType            = "ruin"
    , ldBuilder         = "room_small"
    , ldAnchor          = []
    , ldMaxCount        = 0
    , ldMinSpacing      = 0
    , ldContents        = []
    , ldBounds          = RelBounds (-2) (-2) 2 2
    , ldMapIcon         = icon
    , ldNaming          = testNaming
    }

unknownTex, typeTex, fallbackTex ∷ TextureHandle
unknownTex  = TextureHandle 101   -- the ONE shared unknown marker
typeTex     = TextureHandle 102   -- loc1's own type icon
fallbackTex = TextureHandle 1

registryWithIcons ∷ LocationRegistry
registryWithIcons =
    registerLocation (locDef "loc1" (Just "ruin.png")) emptyLocationRegistry

-- | Every def id the icon-quad scenarios below place. #911 builds
--   instances from the overlay through the registry, so a placed id
--   must be registered to become an instance at all — including
--   "no_icons" (registered, but declaring no @map_icon@, which
--   is what that scenario is about) and the two extra ids the
--   deterministic-ordering scenario places.
registryForQuads ∷ LocationRegistry
registryForQuads = foldr registerLocation registryWithIcons
    [ locDef "no_icons" Nothing
    , locDef "loc2" (Just "ruin2.png")
    , locDef "loc3" (Just "ruin3.png")
    ]

overlayAt ∷ Int → Int → Text → LocationOverlay
overlayAt cx cy lid = HM.singleton (ChunkCoord cx cy) lid

iconSet1 ∷ LocationIconSet
iconSet1 = LocationIconSet
    { lisUnknown   = unknownTex
    , lisTypeIcons = HM.singleton "loc1" typeTex
    }

-- | Wide-open view bounds — every test below cares about wrap/culling
--   behaviour through explicit scenarios, not accidental clipping.
openView ∷ ZoomViewBounds
openView = ZoomViewBounds
    { zvLeft = -1.0e9, zvRight = 1.0e9, zvTop = -1.0e9, zvBottom = 1.0e9 }

-- | #911: the renderer reads the placed-location INSTANCE table, so the
--   fixtures below keep expressing a scenario as (overlay, discovered
--   chunks) and this translates it exactly the way world init + the
--   discovery tick would — build the instances from the overlay, then
--   promote the named chunks' instances to 'LifecycleDiscovered'.
paramsWith ∷ LocationOverlay → HS.HashSet ChunkCoord → WorldGenParams
paramsWith overlay discovered =
    paramsAtLifecycle overlay LifecycleDiscovered
        [ liId i | i ← instancesToList (baseFor overlay)
                 , HS.member (liChunk i) discovered ]

baseFor ∷ LocationOverlay → LocationInstances
baseFor = expectGeometry ∘ buildLocationInstances Nothing registryForQuads

-- | The same fixture generalised to ANY lifecycle state, for the
--   six-constructor appearance sweep (#1230 requirement 3).
paramsAtLifecycle
    ∷ LocationOverlay → LocationLifecycle → [LocationInstanceId]
    → WorldGenParams
paramsAtLifecycle overlay lifecycle ids = defaultWorldGenParams
    { wgpLocationOverlay   = overlay
    , wgpLocationInstances = foldr promote (baseFor overlay) ids
    }
  where
    promote iid lis = fromMaybe lis (setLocationLifecycle iid lifecycle lis)

-- | Every instance in the overlay, moved to @lifecycle@. 'LifecycleUnknown'
--   is where they already start, so it is the identity — which is
--   exactly right: 'setLocationLifecycle' refuses a same-state move.
paramsAll ∷ LocationOverlay → LocationLifecycle → WorldGenParams
paramsAll overlay lifecycle =
    paramsAtLifecycle overlay lifecycle
        [ liId i | i ← instancesToList (baseFor overlay) ]

-- | Run the generator with FaceSouth, camera at the origin, full alpha,
--   a fixed 4-unit icon size — the common case most scenarios below
--   only need to vary the overlay/discovered-set/camera for.
runDefault ∷ LocationOverlay → HS.HashSet ChunkCoord → V.Vector SortableQuad
runDefault overlay discovered =
    runParams (paramsWith overlay discovered)

runParams ∷ WorldGenParams → V.Vector SortableQuad
runParams params =
    makeLocationIconQuads params iconSet1
        FaceSouth openView 0 0 1.0 4.0 (LayerId 2) (\(TextureHandle n) → n) (-1)

spec ∷ Spec
spec = describe "Location map icons" $ do

    describe "map_icon YAML schema (#781, singular since #1230)" $ do
        it "parses a valid single type-icon path" $
            case decodeDef
                    "{ id: t, builder: b, naming: { heads: [KEEP], modifiers: [ASH] },\
                    \  bounds: { min_x: -2, min_y: -2, max_x: 2, max_y: 2 },\
                    \  map_icon: a.png }" of
                Left err → expectationFailure err
                Right def → lydMapIcon def `shouldBe` Just "a.png"

        it "no map_icon field parses as Nothing (no annotation)" $
            case decodeDef
                    "{ id: t, builder: b, naming: { heads: [KEEP], modifiers: [ASH] },\
                    \  bounds: { min_x: -2, min_y: -2, max_x: 2, max_y: 2 } }" of
                Left err → expectationFailure err
                Right def → lydMapIcon def `shouldBe` Nothing

        it "rejects a non-string map_icon value, naming the location" $
            decodeDef
                "{ id: t, builder: b, naming: { heads: [KEEP], modifiers: [ASH] },\
                \  bounds: { min_x: -2, min_y: -2, max_x: 2, max_y: 2 },\
                \  map_icon: 7 }"
                `shouldSatisfy` rejectedNaming "t"

        it "rejects the OLD paired map_icons object under the new key, \
           \naming the location" $
            -- The migration hazard: an unconverted definition that moved
            -- the key but kept the object must fail loudly rather than
            -- silently registering with no annotation.
            decodeDef
                "{ id: t, builder: b, naming: { heads: [KEEP], modifiers: [ASH] },\
                \  bounds: { min_x: -2, min_y: -2, max_x: 2, max_y: 2 },\
                \  map_icon: { undiscovered: a.png, discovered: b.png } }"
                `shouldSatisfy` rejectedNaming "t"

        it "IGNORES a leftover map_icons block — it is no longer part of \
           \the schema, so an unmigrated def loads with no annotation" $
            case decodeDef
                    "{ id: t, builder: b, naming: { heads: [KEEP], modifiers: [ASH] },\
                    \  bounds: { min_x: -2, min_y: -2, max_x: 2, max_y: 2 },\
                    \  map_icons: { undiscovered: a.png, discovered: b.png } }" of
                Left err → expectationFailure err
                Right def → lydMapIcon def `shouldBe` Nothing

        it "the shipped ruin_small.yaml declares one singular map_icon" $ do
            result ← Yaml.decodeFileEither "data/locations/ruin_small.yaml"
            case result of
                Left err → expectationFailure (show (err ∷ Yaml.ParseException))
                Right lf → case lyfLocations lf of
                    [def] → lydMapIcon def `shouldBe`
                        Just "assets/textures/icons/location/ruin.png"
                    ds → expectationFailure
                        ("expected exactly one location def, got " <> show (length ds))

    describe "locationIconAppearance: every lifecycle constructor (#1230)" $ do
        it "unknown and hinted draw the shared unknown marker" $ do
            locationIconAppearance LifecycleUnknown `shouldBe` IconUnknownMarker
            locationIconAppearance LifecycleHinted  `shouldBe` IconUnknownMarker
        it "discovered and active draw the definition's own type icon" $ do
            locationIconAppearance LifecycleDiscovered `shouldBe` IconTypeNormal
            locationIconAppearance LifecycleActive     `shouldBe` IconTypeNormal
        it "cleared and depleted draw that same type icon, darkened" $ do
            locationIconAppearance LifecycleCleared  `shouldBe` IconTypeDark
            locationIconAppearance LifecycleDepleted `shouldBe` IconTypeDark
        it "the dark tint is strictly below the normal white value" $ do
            clearedIconTint `shouldSatisfy` (< 1.0)
            clearedIconTint `shouldSatisfy` (> 0.0)

    describe "buildLocationIconMap" $ do
        it "a def with no ldMapIcon contributes no type-icon entry" $
            HM.lookup "loc1"
                (lisTypeIcons (buildLocationIconMap
                    (registerLocation (locDef "loc1" Nothing) emptyLocationRegistry)
                    emptyTextureNameRegistry fallbackTex))
                `shouldBe` Nothing

        it "resolves a def's type icon via the shared naming convention" $
            lisTypeIcons (buildLocationIconMap registryWithIcons
                (HM.singleton (locationIconTextureName "loc1") typeTex)
                fallbackTex)
                `shouldBe` HM.singleton "loc1" typeTex

        it "resolves the shared unknown marker independently of every \
           \definition — an EMPTY registry still yields it" $
            lisUnknown (buildLocationIconMap emptyLocationRegistry
                (HM.singleton locationUnknownIconTextureName unknownTex)
                fallbackTex)
                `shouldBe` unknownTex

        it "the unknown marker's registry key is not derived from any \
           \def id, so two definitions share ONE handle" $ do
            let reg = HM.fromList
                    [ (locationUnknownIconTextureName, unknownTex)
                    , (locationIconTextureName "loc1", typeTex)
                    , (locationIconTextureName "loc2", TextureHandle 103)
                    ]
                iconSet = buildLocationIconMap
                    (registerLocation (locDef "loc2" (Just "ruin2.png"))
                                      registryWithIcons)
                    reg fallbackTex
            -- Two annotated definitions, two DISTINCT type icons, and
            -- exactly one unknown marker covering both.
            HM.size (lisTypeIcons iconSet) `shouldBe` 2
            lisUnknown iconSet `shouldBe` unknownTex

        it "missing/not-yet-loaded textures fall back to the caller's handle, \
           \never dropping the entry" $ do
            let iconSet = buildLocationIconMap registryWithIcons
                              emptyTextureNameRegistry fallbackTex
            lisTypeIcons iconSet `shouldBe` HM.singleton "loc1" fallbackTex
            lisUnknown iconSet `shouldBe` fallbackTex

        it "no definition id can collide with the shared unknown marker's \
           \registry key — the two namespaces are disjoint" $
            -- A definition id is unrestricted authored text. Under a
            -- shared prefix, `id: unknown` would register its own type
            -- icon under the shared marker's key and overwrite it, and
            -- every location's unknown state would then draw that
            -- definition's icon — leaking the very type the marker
            -- exists to hide. The adversarial ids below are the ones
            -- that would land on it under any suffix-based scheme.
            forM_ [ "unknown", "", "_unknown", "icon", "loc_icon_unknown"
                  , locationUnknownIconTextureName ] $ \lid →
                locationIconTextureName lid
                    `shouldSatisfy` (≢ locationUnknownIconTextureName)

        it "a definition literally named \"unknown\" does NOT hijack the \
           \shared marker — both resolve to their own textures" $ do
            -- The end-to-end form of the case above, through the real
            -- resolution path: register the hostile definition, hand
            -- the registry BOTH keys, and require the marker to survive.
            let hostile   = locDef "unknown" (Just "hijack.png")
                hijackTex = TextureHandle 199
                reg = HM.fromList
                    [ (locationUnknownIconTextureName, unknownTex)
                    , (locationIconTextureName "unknown", hijackTex)
                    ]
                iconSet = buildLocationIconMap
                    (registerLocation hostile emptyLocationRegistry)
                    reg fallbackTex
            lisUnknown iconSet `shouldBe` unknownTex
            HM.lookup "unknown" (lisTypeIcons iconSet) `shouldBe` Just hijackTex

        it "…and that definition's own instances still draw the SHARED \
           \marker while unknown, never their type icon" $ do
            -- The behaviour the collision would have broken, at the
            -- quad level: an instance of the hostile definition must
            -- render the marker at `unknown` and only reveal its own
            -- icon once discovered.
            let hostile = locDef "unknown" (Just "hijack.png")
                hijackTex = TextureHandle 199
                reg = foldr registerLocation emptyLocationRegistry [hostile]
                overlay = overlayAt 0 0 "unknown"
                -- Resolved through the REAL name registry, not a
                -- hand-built set: that is the step a colliding key
                -- corrupts, so building the set directly here would
                -- render this scenario blind to the very bug it names.
                nameReg = HM.fromList
                    [ (locationUnknownIconTextureName, unknownTex)
                    , (locationIconTextureName "unknown", hijackTex)
                    ]
                iconSet = buildLocationIconMap reg nameReg fallbackTex
                base = expectGeometry
                    (buildLocationInstances Nothing reg overlay)
                paramsAt l = defaultWorldGenParams
                    { wgpLocationOverlay   = overlay
                    , wgpLocationInstances =
                        foldr (\iid lis →
                                  fromMaybe lis (setLocationLifecycle iid l lis))
                              base [ liId i | i ← instancesToList base ]
                    }
                run l = V.map sqTexture (makeLocationIconQuads (paramsAt l)
                    iconSet FaceSouth openView 0 0 1.0 4.0 (LayerId 2)
                    (\(TextureHandle n) → n) (-1))
            run LifecycleUnknown    `shouldBe` V.singleton unknownTex
            run LifecycleDiscovered `shouldBe` V.singleton hijackTex

        it "the shared unknown icon's canonical path is the one the issue \
           \specifies, and it ships" $ do
            locationUnknownIconPath `shouldBe`
                "assets/textures/icons/location/location_unknown.png"
            doesFileExist locationUnknownIconPath >>= (`shouldBe` True)

    describe "makeLocationIconQuads: texture selection per lifecycle" $ do
        let overlay = overlayAt 0 0 "loc1"
            texAt l = V.map sqTexture (runParams (paramsAll overlay l))
            rgbAt l = case V.toList (runParams (paramsAll overlay l)) of
                [q] → let c = color (sqV0 q) in (r c, g c, b c)
                qs  → error ("expected exactly one quad, got " <> show (length qs))

        forM_ [LifecycleUnknown, LifecycleHinted] $ \l →
            it (show l <> " selects the SHARED unknown marker, not the type icon") $
                texAt l `shouldBe` V.singleton unknownTex

        forM_ [LifecycleDiscovered, LifecycleActive] $ \l →
            it (show l <> " selects the definition's own type icon") $
                texAt l `shouldBe` V.singleton typeTex

        forM_ [LifecycleCleared, LifecycleDepleted] $ \l →
            it (show l <> " selects that SAME type icon, not a third texture") $
                texAt l `shouldBe` V.singleton typeTex

        forM_ [LifecycleUnknown, LifecycleHinted, LifecycleDiscovered
              , LifecycleActive] $ \l →
            it (show l <> " draws at full white RGB") $
                rgbAt l `shouldBe` (1.0, 1.0, 1.0)

        forM_ [LifecycleCleared, LifecycleDepleted] $ \l →
            it (show l <> " darkens EVERY RGB component below white") $ do
                let (rr, gg, bb) = rgbAt l
                rr `shouldSatisfy` (< 1.0)
                gg `shouldSatisfy` (< 1.0)
                bb `shouldSatisfy` (< 1.0)
                (rr, gg, bb) `shouldBe`
                    (clearedIconTint, clearedIconTint, clearedIconTint)

        it "a state change re-selects the texture live, with no other input changed" $ do
            texAt LifecycleUnknown    `shouldBe` V.singleton unknownTex
            texAt LifecycleDiscovered `shouldBe` V.singleton typeTex

        it "two definitions that are both unknown draw the IDENTICAL icon, \
           \so the map leaks nothing about which is which" $ do
            let overlay2 = HM.fromList
                    [ (ChunkCoord 0 0, "loc1"), (ChunkCoord 5 5, "loc2") ]
                iconSet = LocationIconSet
                    { lisUnknown   = unknownTex
                    , lisTypeIcons = HM.fromList
                        [ ("loc1", typeTex), ("loc2", TextureHandle 103) ]
                    }
                quads = makeLocationIconQuads
                    (paramsAll overlay2 LifecycleUnknown) iconSet
                    FaceSouth openView 0 0 1.0 4.0 (LayerId 2)
                    (\(TextureHandle n) → n) (-1)
            V.map sqTexture quads
                `shouldBe` V.fromList [unknownTex, unknownTex]

        it "a location whose def declares no map_icon renders nothing, in \
           \EVERY lifecycle state" $
            forM_ [minBound .. maxBound ∷ LocationLifecycle] $ \l →
                V.null (makeLocationIconQuads
                    (paramsAll (overlayAt 0 0 "no_icons") l)
                    iconSet1 FaceSouth openView 0 0 1.0 4.0 (LayerId 2)
                    (\(TextureHandle n) → n) (-1))
                    `shouldBe` True

    describe "makeLocationIconQuads: alpha (fade transition)" $ do
        let alphaOf v = case V.toList v of
                [q] → a (color (sqV0 q))
                qs  → error ("expected exactly one quad, got " <> show (length qs))
            withAlpha alpha = makeLocationIconQuads
                (paramsWith (overlayAt 0 0 "loc1") HS.empty) iconSet1
                FaceSouth openView 0 0 alpha 4.0 (LayerId 2)
                (\(TextureHandle n) → n) (-1)
        it "alpha 0 below the fade start (visually absent)" $
            alphaOf (withAlpha 0.0) `shouldBe` 0.0
        it "a fractional alpha mid-fade carries straight through" $
            alphaOf (withAlpha 0.42) `shouldBe` 0.42
        it "full alpha at full map visibility, independent of ZoomMapMode \
           \(icon color never routes through a mode's color function)" $
            alphaOf (withAlpha 1.0) `shouldBe` 1.0

        it "the supplied fade alpha survives EXACTLY in all six lifecycle \
           \states — darkening scales RGB only (#1230 requirement 3)" $
            forM_ [minBound .. maxBound ∷ LocationLifecycle] $ \l →
                forM_ [0.0, 0.42, 1.0 ∷ Float] $ \alpha →
                    case V.toList (makeLocationIconQuads
                            (paramsAll (overlayAt 0 0 "loc1") l) iconSet1
                            FaceSouth openView 0 0 alpha 4.0 (LayerId 2)
                            (\(TextureHandle n) → n) (-1)) of
                        [q] → a (color (sqV0 q)) `shouldBe` alpha
                        qs  → expectationFailure
                                ("expected one quad for " <> show l <> ", got "
                                    <> show (length qs))

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
                (paramsWith (overlayAt 0 0 "loc1") HS.empty) iconSet1
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
                q = V.head $ makeLocationIconQuads params iconSet1
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
            V.map sqTexture pageA `shouldBe` V.singleton unknownTex
            V.map sqTexture pageB `shouldBe` V.singleton typeTex

    describe "makeLocationIconQuads: deterministic ordering" $ do
        let iconSet3 = LocationIconSet
                { lisUnknown   = unknownTex
                , lisTypeIcons = HM.fromList
                    [ ("loc1", TextureHandle 11)
                    , ("loc2", TextureHandle 12)
                    , ("loc3", TextureHandle 13)
                    ]
                }
            overlay3 = HM.fromList
                [ (ChunkCoord 2 (-1), "loc1")
                , (ChunkCoord (-3) 4, "loc2")
                , (ChunkCoord 0 0,    "loc3")
                ]
            run () = makeLocationIconQuads
                (paramsAll overlay3 LifecycleDiscovered) iconSet3
                FaceSouth openView 0 0 1.0 4.0 (LayerId 2)
                (\(TextureHandle n) → n) (-1)
        it "matches instance-id order, allocated in overlayToList's \
           \sorted-by-(cx,cy) order" $
            -- (cx,cy): loc2@(-3,4) < loc3@(0,0) < loc1@(2,-1), so the
            -- instance ids run 1, 2, 3 in that same order.
            V.map sqTexture (run ())
                `shouldBe` V.fromList
                    [TextureHandle 12, TextureHandle 13, TextureHandle 11]
        it "assigns consecutive paint-order sort keys from \
           \iconSortKeyBase, in that same instance-id order" $
            -- Painter's order within 'zoomMapLayer' is decided by
            -- 'sqSortKey' alone, so pinning the exact keys is what
            -- "never reorder or flicker" actually rests on:
            -- 'makeLocationIconQuads' zips 'instancesToList' against
            -- @[iconSortKeyBase ..]@, giving 1000, 1001, 1002 to
            -- loc2, loc3, loc1 respectively.
            V.map sqSortKey (run ())
                `shouldBe` V.fromList [1000.0, 1001.0, 1002.0]
