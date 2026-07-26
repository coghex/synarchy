{-# LANGUAGE UnicodeSyntax, OverloadedStrings #-}
-- | Focused tests for 'Engine.Preview.Unit' (#887, Phase 3 of the
--   @--preview@ browser epic #427): the pure direction-mirroring table,
--   the default-selection rule, animation ordering/labeling, numeric
--   frame ordering, YAML metadata extraction and its exact defaults, the
--   unequal-frame-count playback rule, and the documented non-loop
--   end-of-clip policy — plus the filesystem containment rules that
--   reject a bad @units\/\<name\>@ target before a window is ever
--   created. No engine needed.
module Test.Headless.Preview.UnitAnimation (spec) where

import UPrelude
import Test.Hspec
import Control.Exception (finally)
import Data.List (sort, find)
import qualified Data.Text as T
import qualified Data.Map.Strict as Map
import System.Directory
    ( getTemporaryDirectory, createDirectoryIfMissing, removeDirectoryRecursive
    , doesDirectoryExist, createDirectoryLink, removeDirectoryLink )
import System.FilePath ((</>))
import Engine.Asset.YamlUnits (UnitYamlAnim(..))
import Engine.Core.Types (PreviewUnit(..), PreviewAnim(..), PreviewFrameDir(..))
import Engine.Preview.Unit
import Unit.Direction (Direction(..))

-- The real shipped acolyte tree — every ordering/mirroring claim below
-- is also proved against the ACTUAL canonical layout, not just a
-- synthetic fixture (the Test.Headless.Preview.Discovery convention).
realUnit ∷ String
realUnit = "acolyte"

-- A unit with NO data/units/<name>.yaml at all: exactly the
-- missing-metadata case the review amendment called out as real rather
-- than hypothetical (three of the seven shipped asset trees have none).
yamllessUnit ∷ String
yamllessUnit = "tiller"

anim ∷ Float → Bool → Bool → UnitYamlAnim
anim fps loop flipV = UnitYamlAnim
    { uyaFps = fps, uyaLoop = loop, uyaFlip = flipV, uyaFrames = Map.empty }

frames ∷ [(Direction, [Text])] → Map.Map Direction [Text]
frames = Map.fromList

-- A synthetic unit tree, for the cases no shipped asset exhibits:
-- an all-eight-direction animation, an unequal-frame-count animation,
-- an unpadded frame numbering, and a unit with no animations/ root.
--   <root>/five/{south,south-east,east,north-east,north}/frame_00N.png
--   <root>/eight/<all eight>/frame_000.png
--   <root>/uneven/{south (3 frames), east (2 frames)}/...
--   <root>/unpadded/south/frame_{1,2,10}.png
withUnitFixture ∷ (FilePath → IO ()) → IO ()
withUnitFixture action = do
    tmp ← getTemporaryDirectory
    let root = tmp </> "synarchy-preview-unit-spec"
        unitDir = root </> "fixture_unit"
        animDir a = unitDir </> "animations" </> a
        put a d fs = do
            createDirectoryIfMissing True (animDir a </> d)
            forM_ fs $ \f → writeFile (animDir a </> d </> f) ""
    forM_ ["south", "south-east", "east", "north-east", "north"] $ \d →
        put "five" d ["frame_000.png", "frame_001.png"]
    forM_ [ "south", "south-west", "west", "north-west"
          , "north", "north-east", "east", "south-east" ] $ \d →
        put "eight" d ["frame_000.png"]
    put "uneven" "south" ["frame_000.png", "frame_001.png", "frame_002.png"]
    put "uneven" "east"  ["frame_000.png", "frame_001.png"]
    put "unpadded" "south" ["frame_1.png", "frame_2.png", "frame_10.png"]
    -- A non-texture file and an unrecognized direction folder: both
    -- must be ignored rather than becoming frames/cells.
    writeFile (animDir "five" </> "south" </> "notes.txt") ""
    createDirectoryIfMissing True (animDir "five" </> "up")
    writeFile (animDir "five" </> "up" </> "frame_000.png") ""
    -- A sibling directory with no animations/ subtree at all.
    createDirectoryIfMissing True (root </> "no_anims")
    (`finally` removeDirectoryRecursive root) (action root)

-- A symlinked unit directory: refused unconditionally, the same rule
-- Engine.Preview.Discovery applies to every path it walks.
withSymlinkedUnit ∷ (FilePath → IO ()) → IO ()
withSymlinkedUnit action = do
    tmp ← getTemporaryDirectory
    let root = tmp </> "synarchy-preview-unit-symlink-spec"
        real = tmp </> "synarchy-preview-unit-symlink-spec-outside"
    createDirectoryIfMissing True (real </> "animations" </> "idle" </> "south")
    writeFile (real </> "animations" </> "idle" </> "south" </> "frame_000.png") ""
    createDirectoryIfMissing True root
    createDirectoryLink real (root </> "linked")
    let cleanup = do
            removeDirectoryLink (root </> "linked")
            removeDirectoryRecursive root
            removeDirectoryRecursive real
    (`finally` cleanup) (action root)

spec ∷ Spec
spec = do
    describe "directionDirName / parseDirectionDirName" $ do
        it "round-trips every direction through its folder-name spelling" $
            forM_ previewDirectionOrder $ \d →
                parseDirectionDirName (directionDirName d) `shouldBe` Just d

        it "accepts the short spellings the unit YAML also allows" $ do
            parseDirectionDirName "SE" `shouldBe` Just DirSE
            parseDirectionDirName "nw" `shouldBe` Just DirNW

        it "rejects a name that isn't a compass direction" $
            parseDirectionDirName "up" `shouldBe` Nothing

        it "orders the row clockwise from south, matching the game" $
            map directionDirName previewDirectionOrder `shouldBe`
                [ "south", "south-west", "west", "north-west"
                , "north", "north-east", "east", "south-east" ]

    describe "defaultAnimationName" $ do
        it "prefers idle wherever it sits in the list" $
            defaultAnimationName ["attack", "idle", "walk"] `shouldBe` Just "idle"
        it "falls back to the first entry in list order when there is no idle" $
            defaultAnimationName ["attack", "walk"] `shouldBe` Just "attack"
        it "is Nothing for a unit with no animations" $
            defaultAnimationName [] `shouldBe` Nothing

    describe "sortFrameFiles" $ do
        it "orders numerically, not lexicographically" $
            sortFrameFiles ["frame_10.png", "frame_2.png", "frame_1.png"]
                `shouldBe` ["frame_1.png", "frame_2.png", "frame_10.png"]
        it "keeps zero-padded names in the same order they already read in" $
            sortFrameFiles ["frame_002.png", "frame_000.png", "frame_001.png"]
                `shouldBe` ["frame_000.png", "frame_001.png", "frame_002.png"]
        it "sorts an unnumbered name after the numbered ones instead of dropping it" $
            sortFrameFiles ["pose.png", "frame_001.png"]
                `shouldBe` ["frame_001.png", "pose.png"]

    describe "effectiveFlip" $ do
        it "takes the YAML flag when the animation has an entry, even for \
           \a five-direction layout that would otherwise infer True" $
            effectiveFlip (Just (anim 10 False False))
                (frames [(d, ["f"]) | d ← [DirS, DirSE, DirE, DirNE, DirN]])
                `shouldBe` False

        it "takes a YAML True as well" $
            effectiveFlip (Just (anim 8 True True)) (frames [(DirS, ["f"])])
                `shouldBe` True

        it "infers True with no YAML entry for exactly the canonical five" $
            effectiveFlip Nothing
                (frames [(d, ["f"]) | d ← [DirS, DirSE, DirE, DirNE, DirN]])
                `shouldBe` True

        it "infers False with no YAML entry when all eight are authored" $
            effectiveFlip Nothing (frames [(d, ["f"]) | d ← previewDirectionOrder])
                `shouldBe` False

        it "infers False with no YAML entry for a partial set — a missing \
           \direction stays unavailable rather than being invented" $
            effectiveFlip Nothing (frames [(DirS, ["f"]), (DirE, ["f"])])
                `shouldBe` False

    describe "resolveAnimDirections" $ do
        it "mirrors W/SW/NW from their eastern counterparts when flipping \
           \is allowed, reporting each cell's real source" $ do
            let stored = frames [ (DirS, ["s"]), (DirSE, ["se"]), (DirE, ["e"])
                                , (DirNE, ["ne"]), (DirN, ["n"]) ]
                out = resolveAnimDirections True stored
            map pfdDirection out `shouldBe`
                [ "south", "south-west", "west", "north-west"
                , "north", "north-east", "east", "south-east" ]
            [ (pfdDirection d, pfdSource d, pfdFrames d)
                | d ← out, pfdMirrored d ] `shouldBe`
                [ ("south-west", "south-east", ["se"])
                , ("west", "east", ["e"])
                , ("north-west", "north-east", ["ne"]) ]

        it "omits the western directions entirely when flipping is off" $
            map pfdDirection (resolveAnimDirections False
                (frames [ (DirS, ["s"]), (DirSE, ["se"]), (DirE, ["e"])
                        , (DirNE, ["ne"]), (DirN, ["n"]) ]))
                `shouldBe` ["south", "north", "north-east", "east", "south-east"]

        it "prefers a directly authored western direction over mirroring, \
           \even with flipping enabled" $ do
            let stored = frames [ (DirS, ["s"]), (DirSE, ["se"]), (DirE, ["e"])
                                , (DirNE, ["ne"]), (DirN, ["n"])
                                , (DirW, ["authored-w"]) ]
                out = resolveAnimDirections True stored
                west = find ((≡ "west") ∘ pfdDirection) out
            fmap pfdMirrored west `shouldBe` Just False
            fmap pfdSource west `shouldBe` Just "west"
            fmap pfdFrames west `shouldBe` Just ["authored-w"]

        it "treats an empty frame list as no direction at all, and never \
           \mirrors from one" $
            resolveAnimDirections True (frames [(DirS, ["s"]), (DirE, [])])
                `shouldSatisfy` \out →
                    map pfdDirection out ≡ ["south"]

    describe "frameIndexAt" $ do
        it "wraps a looping clip at its own frame count" $
            map (\t → frameIndexAt True 8 4 t) [0, 0.13, 0.26, 0.4, 0.51]
                `shouldBe` [0, 1, 2, 3, 0]

        it "HOLDS the last frame past the end of a non-looping clip \
           \(the documented policy, matching Unit.Render.pickFrame)" $
            map (\t → frameIndexAt False 8 4 t) [0, 0.4, 0.6, 10.0]
                `shouldBe` [0, 3, 3, 3]

        it "gives each direction its OWN index from the SAME elapsed value, \
           \so unequal frame counts stay phase-aligned" $ do
            -- 0.9 s at 10 fps = raw frame 9: a 4-frame direction shows 1,
            -- a 3-frame direction shows 0 — both from one shared clock.
            frameIndexAt True 10 4 0.9 `shouldBe` 1
            frameIndexAt True 10 3 0.9 `shouldBe` 0

        it "is 0 for a single-frame or empty direction, at any time" $ do
            frameIndexAt True 8 1 99.0 `shouldBe` 0
            frameIndexAt True 8 0 99.0 `shouldBe` 0

        it "is 0 before the clock starts (a negative elapsed can't index \
           \out of range)" $
            frameIndexAt True 8 4 (-5.0) `shouldBe` 0

    describe "buildPreviewAnims (metadata extraction + exact defaults)" $ do
        it "uses the YAML fps/loop/flip when the animation has an entry" $ do
            let meta = Map.fromList [("attack", anim 12 False True)]
            map (\a → (paFps a, paLoop a, paFlip a))
                (buildPreviewAnims meta [("attack", frames [(DirS, ["s"])])])
                `shouldBe` [(12, False, True)]

        it "falls back to fps=8 / loop=true for an animation the YAML \
           \never mentions" $
            map (\a → (paFps a, paLoop a, paFlip a))
                (buildPreviewAnims Map.empty
                    [("roar", frames [(DirS, ["s"]), (DirE, ["e"])])])
                `shouldBe` [(8, True, False)]

        it "reports the south frame-zero thumbnail, and an empty one when \
           \the animation stores no south frames" $
            map paThumb (buildPreviewAnims Map.empty
                    [ ("a", frames [(DirS, ["s0", "s1"])])
                    , ("b", frames [(DirE, ["e0"])]) ])
                `shouldBe` ["s0", ""]

    describe "resolveUnitDir (pre-boot containment)" $ do
        it "resolves a real shipped unit" $ do
            result ← resolveUnitDir unitsCategoryRoot realUnit
            result `shouldBe` Right (unitsCategoryRoot </> realUnit)

        it "rejects an unknown unit" $ do
            result ← resolveUnitDir unitsCategoryRoot "nosuch"
            result `shouldBe` Left UnitNotFound

        it "rejects an empty name" $ do
            result ← resolveUnitDir unitsCategoryRoot ""
            result `shouldBe` Left UnitNameEscapesRoot

        it "rejects a name with path structure — a unit target is one \
           \directory name, never a path" $ do
            nested ← resolveUnitDir unitsCategoryRoot "acolyte/animations"
            nested `shouldBe` Left UnitNameEscapesRoot

        it "rejects an absolute path" $ do
            result ← resolveUnitDir unitsCategoryRoot "/etc"
            result `shouldBe` Left UnitNameEscapesRoot

        it "rejects dot and dot-dot traversal" $ do
            dot ← resolveUnitDir unitsCategoryRoot "."
            dotdot ← resolveUnitDir unitsCategoryRoot ".."
            up ← resolveUnitDir unitsCategoryRoot "../../etc"
            [dot, dotdot, up] `shouldBe` replicate 3 (Left UnitNameEscapesRoot)

        it "rejects a symlinked unit directory unconditionally" $
            withSymlinkedUnit $ \root → do
                result ← resolveUnitDir root "linked"
                result `shouldBe` Left UnitNameSymlink

        it "rejects a directory with no animations/ subtree" $
            withUnitFixture $ \root → do
                result ← resolveUnitDir root "no_anims"
                result `shouldBe` Left UnitNoAnimations

        it "reports UnitNotFound when the units root itself doesn't exist" $ do
            result ← resolveUnitDir "assets/textures/does-not-exist-887" "acolyte"
            result `shouldBe` Left UnitNotFound

    describe "discoverUnitAnimations (filesystem is authoritative)" $ do
        it "lists the fixture's animations in case-sensitive directory-name \
           \order, dropping unrecognized direction folders" $
            withUnitFixture $ \root → do
                found ← discoverUnitAnimations (root </> "fixture_unit")
                map fst found `shouldBe` ["eight", "five", "uneven", "unpadded"]
                lookup "five" found `shouldSatisfy`
                    maybe False ((≡ [DirS, DirN, DirNE, DirE, DirSE]) ∘ Map.keys)

        it "orders each direction's frames numerically, even unpadded" $
            withUnitFixture $ \root → do
                found ← discoverUnitAnimations (root </> "fixture_unit")
                let fs = Map.lookup DirS =≪ lookup "unpadded" found
                fmap (map (T.unpack ∘ T.takeWhileEnd (/= '/'))) fs
                    `shouldBe` Just ["frame_1.png", "frame_2.png", "frame_10.png"]

        it "finds every shipped acolyte animation the YAML never mentions \
           \(pushing_idle), proving the filesystem — not the YAML — is \
           \what decides membership" $ do
            found ← discoverUnitAnimations (unitsCategoryRoot </> realUnit)
            map fst found `shouldContain` ["pushing_idle"]
            map fst found `shouldContain` ["idle"]

    describe "buildPreviewUnit (the whole pre-boot pipeline)" $ do
        it "resolves the shipped acolyte to an idle-defaulted, ordered \
           \animation list with every direction populated" $ do
            result ← buildPreviewUnit unitsCategoryRoot realUnit
            case result of
                Left err → expectationFailure
                    (T.unpack (unitFocusErrorMessage err))
                Right u → do
                    puName u `shouldBe` "acolyte"
                    puDefault u `shouldBe` "idle"
                    map paName (puAnims u) `shouldSatisfy` \ns → ns ≡ sort ns
                    case find ((≡ "idle") ∘ paName) (puAnims u) of
                        Nothing → expectationFailure "acolyte has no idle animation"
                        Just idle → do
                            -- idle ships the canonical five and declares
                            -- flip: true, so all eight cells populate.
                            paFlip idle `shouldBe` True
                            map pfdDirection (paDirs idle) `shouldBe`
                                map directionDirName previewDirectionOrder
                            map pfdSource (filter pfdMirrored (paDirs idle))
                                `shouldBe` ["south-east", "east", "north-east"]
                            paThumb idle `shouldSatisfy`
                                T.isSuffixOf "/south/frame_000.png"

        it "resolves a unit with NO data/units YAML at all, using the \
           \documented defaults for every animation" $ do
            result ← buildPreviewUnit unitsCategoryRoot yamllessUnit
            case result of
                Left err → expectationFailure
                    (T.unpack (unitFocusErrorMessage err))
                Right u → do
                    puAnims u `shouldSatisfy` not ∘ null
                    puAnims u `shouldSatisfy` all (\a → paFps a ≡ 8 ∧ paLoop a)
                    -- Every cell still resolves for the canonical
                    -- five-direction layout, via the inferred mirroring.
                    puAnims u `shouldSatisfy` all (\a → not (null (paDirs a)))

        it "rejects an unknown unit through the same error the CLI prints" $ do
            result ← buildPreviewUnit unitsCategoryRoot "nosuch"
            result `shouldBe` Left UnitNotFound

    describe "loadUnitAnimMeta" $ do
        it "extracts the shipped acolyte's animation metadata" $ do
            meta ← loadUnitAnimMeta "acolyte"
            Map.lookup "idle" meta `shouldSatisfy` isJust
            fmap uyaFlip (Map.lookup "idle" meta) `shouldBe` Just True

        it "is empty (never an error) for a unit with no YAML file" $ do
            meta ← loadUnitAnimMeta (T.pack yamllessUnit)
            meta `shouldBe` Map.empty

        it "is empty for a name no YAML file exists for at all" $ do
            meta ← loadUnitAnimMeta "definitely_not_a_unit_887"
            meta `shouldBe` Map.empty

    -- Sanity: the real trees this spec leans on exist in this checkout,
    -- or half the assertions above prove nothing.
    describe "fixture sanity" $ do
        it "the shipped acolyte and tiller asset trees exist" $ do
            a ← doesDirectoryExist (unitsCategoryRoot </> realUnit)
            t ← doesDirectoryExist (unitsCategoryRoot </> yamllessUnit)
            (a, t) `shouldBe` (True, True)
