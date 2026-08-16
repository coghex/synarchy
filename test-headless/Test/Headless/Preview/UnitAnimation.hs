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
import Data.List (sort, find, nub)
import qualified Data.Text as T
import qualified Data.Map.Strict as Map
import System.Directory
    ( getTemporaryDirectory, createDirectoryIfMissing, removeDirectoryRecursive
    , doesDirectoryExist, doesFileExist, createDirectoryLink
    , removeDirectoryLink )
import System.FilePath ((</>))
import qualified Data.HashMap.Strict as HM
import Engine.Asset.YamlUnits (UnitYamlAnim(..))
import Engine.Core.Types
    ( PreviewUnit(..), PreviewAnim(..), PreviewFrameDir(..), PreviewFrame(..) )
import Engine.Preview.Unit
import Unit.Atlas.Index (unitAtlasIndexPath)
import Unit.Direction (Direction(..))

-- The real shipped acolyte tree — every ordering/mirroring claim below
-- is also proved against the ACTUAL canonical layout, not just a
-- synthetic fixture (the Test.Headless.Preview.Discovery convention).
realUnit ∷ String
realUnit = "acolyte"

-- A shipped ASSET-ONLY tree (#1257): it has a data/units/<name>.yaml,
-- but declared under `asset_units:`, so it is browsable and carries
-- real playback metadata while never entering the gameplay unit
-- registry. It replaces the old "no YAML at all" fixture, which no
-- shipped tree exhibits any more — the fallback for genuinely
-- undeclared content is still covered, by the pure `effectiveFlip`
-- cases and by the "a name no YAML file exists for at all" case below,
-- neither of which depends on a shipped tree.
assetOnlyUnit ∷ String
assetOnlyUnit = "tiller"

anim ∷ Float → Bool → Bool → UnitYamlAnim
anim fps loop flipV = UnitYamlAnim
    { uyaFps = fps, uyaLoop = loop, uyaFlip = flipV, uyaFrames = Map.empty }

frames ∷ [(Direction, [Text])] → Map.Map Direction [Text]
frames = Map.fromList

-- The same, already lifted into whole-image preview frames — what
-- 'resolveAnimDirections' takes now that a frame is a texture plus a
-- sub-rect rather than a bare path (#1260).
srcFrames ∷ [(Direction, [Text])] → Map.Map Direction [PreviewFrame]
srcFrames = fmap sourceFrames ∘ frames

framePaths ∷ PreviewFrameDir → [Text]
framePaths = map pfPath ∘ pfdFrames

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

-- Two symlink shapes, both refused unconditionally (the same rule
-- Engine.Preview.Discovery applies to every path it walks):
--   linked/            -- the whole unit directory is a symlink
--   real_unit/animations -> outside/animations
--                      -- a REAL unit directory whose animations root is
--                         a symlink into another tree. doesDirectoryExist
--                         follows links, so without an lstat here this
--                         unit would browse (and load textures from)
--                         assets it does not own.
withSymlinkedUnit ∷ (FilePath → IO ()) → IO ()
withSymlinkedUnit action = do
    tmp ← getTemporaryDirectory
    let root = tmp </> "synarchy-preview-unit-symlink-spec"
        real = tmp </> "synarchy-preview-unit-symlink-spec-outside"
    createDirectoryIfMissing True (real </> "animations" </> "idle" </> "south")
    writeFile (real </> "animations" </> "idle" </> "south" </> "frame_000.png") ""
    createDirectoryIfMissing True root
    createDirectoryLink real (root </> "linked")
    createDirectoryIfMissing True (root </> "real_unit")
    createDirectoryLink (real </> "animations") (root </> "real_unit" </> "animations")
    let cleanup = do
            removeDirectoryLink (root </> "real_unit" </> "animations")
            removeDirectoryLink (root </> "linked")
            removeDirectoryRecursive root
            removeDirectoryRecursive real
    (`finally` cleanup) (action root)

-- A unit whose compiled artifacts are BROKEN, in its own temp resource
-- root (#1260). Deliberately a fixture rather than a temporarily
-- corrupted copy of the shipped acolyte index: the rejection path must
-- be exercisable without ever writing into the tracked asset tree,
-- where a crashed run would leave a damaged artifact behind and the
-- inventory gate would then fail for an unrelated reason.
--
--   <tmp>/assets/textures/units/<brokenUnit>/animations/idle/south/frame_000.png
--   <tmp>/assets/textures/units/<brokenUnit>/atlas/index.json   (optional)
--
-- 'Nothing' leaves the atlas/ directory present but EMPTY, which is the
-- other half of the contract: an incomplete compiled artifact rejects
-- just as a malformed one does, and only a wholly ABSENT atlas/
-- directory means legacy.
brokenUnit ∷ String
brokenUnit = "spec_broken_atlas_unit"

withCompiledUnitFixture ∷ Maybe String → (FilePath → FilePath → IO ()) → IO ()
withCompiledUnitFixture mIndex action = do
    tmp ← getTemporaryDirectory
    let resRoot = tmp </> "synarchy-preview-unit-atlas-spec"
        catRoot = resRoot </> unitsCategoryRoot
        unitDir = catRoot </> brokenUnit
    createDirectoryIfMissing True (unitDir </> "animations" </> "idle" </> "south")
    writeFile (unitDir </> "animations" </> "idle" </> "south" </> "frame_000.png") ""
    createDirectoryIfMissing True (unitDir </> "atlas")
    forM_ mIndex $ \doc →
        writeFile (resRoot </> unitAtlasIndexPath (T.pack brokenUnit)) doc
    (`finally` removeDirectoryRecursive resRoot) (action resRoot catRoot)

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
            let stored = srcFrames [ (DirS, ["s"]), (DirSE, ["se"]), (DirE, ["e"])
                                   , (DirNE, ["ne"]), (DirN, ["n"]) ]
                out = resolveAnimDirections True stored
            map pfdDirection out `shouldBe`
                [ "south", "south-west", "west", "north-west"
                , "north", "north-east", "east", "south-east" ]
            [ (pfdDirection d, pfdSource d, framePaths d)
                | d ← out, pfdMirrored d ] `shouldBe`
                [ ("south-west", "south-east", ["se"])
                , ("west", "east", ["e"])
                , ("north-west", "north-east", ["ne"]) ]

        it "omits the western directions entirely when flipping is off" $
            map pfdDirection (resolveAnimDirections False
                (srcFrames [ (DirS, ["s"]), (DirSE, ["se"]), (DirE, ["e"])
                           , (DirNE, ["ne"]), (DirN, ["n"]) ]))
                `shouldBe` ["south", "north", "north-east", "east", "south-east"]

        it "prefers a directly authored western direction over mirroring, \
           \even with flipping enabled" $ do
            let stored = srcFrames [ (DirS, ["s"]), (DirSE, ["se"]), (DirE, ["e"])
                                   , (DirNE, ["ne"]), (DirN, ["n"])
                                   , (DirW, ["authored-w"]) ]
                out = resolveAnimDirections True stored
                west = find ((≡ "west") ∘ pfdDirection) out
            fmap pfdMirrored west `shouldBe` Just False
            fmap pfdSource west `shouldBe` Just "west"
            fmap framePaths west `shouldBe` Just ["authored-w"]

        it "treats an empty frame list as no direction at all, and never \
           \mirrors from one" $
            resolveAnimDirections True (srcFrames [(DirS, ["s"]), (DirE, [])])
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
                (buildPreviewAnims meta HM.empty
                    [("attack", frames [(DirS, ["s"])])])
                `shouldBe` [(12, False, True)]

        it "falls back to fps=8 / loop=true for an animation the YAML \
           \never mentions" $
            map (\a → (paFps a, paLoop a, paFlip a))
                (buildPreviewAnims Map.empty HM.empty
                    [("roar", frames [(DirS, ["s"]), (DirE, ["e"])])])
                `shouldBe` [(8, True, False)]

        it "reports the south frame-zero thumbnail, and none at all when \
           \the animation stores no south frames" $
            map (fmap pfPath ∘ paThumb) (buildPreviewAnims Map.empty HM.empty
                    [ ("a", frames [(DirS, ["s0", "s1"])])
                    , ("b", frames [(DirE, ["e0"])]) ])
                `shouldBe` [Just "s0", Nothing]

        it "leaves an unmigrated animation on the legacy path: source-frame \
           \paths, whole-image UVs, and no cell size of its own" $ do
            let out = buildPreviewAnims Map.empty HM.empty
                          [("roar", frames [(DirS, ["s0", "s1"])])]
            map paAtlas out `shouldBe` [Nothing]
            concatMap pfdFrames (concatMap paDirs out) `shouldBe`
                [ PreviewFrame "s0" (0, 0, 1, 1) Nothing
                , PreviewFrame "s1" (0, 0, 1, 1) Nothing ]

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

        it "rejects a REAL unit directory whose animations/ root is a \
           \symlink — doesDirectoryExist follows links, so without an \
           \lstat here the unit would browse another tree's assets" $
            withSymlinkedUnit $ \root → do
                result ← resolveUnitDir root "real_unit"
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

        it "is empty for a symlinked animations/ root, standalone — this \
           \function is exported and independently exercised, so it must \
           \be symlink-safe without relying on resolveUnitDir first" $
            withSymlinkedUnit $ \root → do
                found ← discoverUnitAnimations (root </> "real_unit")
                found `shouldBe` []

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
                            -- Since #1260 the thumbnail is a CELL of the
                            -- compiled atlas, not a source frame.
                            fmap pfPath (paThumb idle) `shouldBe`
                                Just "assets/textures/units/acolyte/atlas/idle.png"

        -- #1260 (TEX-4): the acolyte pilot. These run against the
        -- artifacts actually checked in, through the PRODUCTION loader
        -- the game registers with — which is the whole of D-9. A
        -- preview-only decoder would keep passing every assertion above
        -- while the compiled tree rotted underneath it.
        it "puts EVERY shipped acolyte animation on the atlas path — no \
           \animation of a migrated unit silently resolves to legacy \
           \per-frame storage" $ do
            result ← buildPreviewUnit unitsCategoryRoot realUnit
            case result of
                Left err → expectationFailure
                    (T.unpack (unitFocusErrorMessage err))
                Right u → do
                    let legacy = [paName a | a ← puAnims u, isNothing (paAtlas a)]
                    legacy `shouldBe` []
                    length (puAnims u) `shouldBe` 54
                    -- Each animation's own atlas, named after itself —
                    -- D-2's one-atlas-per-animation, observed rather
                    -- than assumed.
                    [ (paName a, paAtlas a) | a ← puAnims u ] `shouldSatisfy`
                        all (\(n, p) → p ≡ Just ("assets/textures/units/acolyte/atlas/"
                                                 <> n <> ".png"))
                    -- and every frame of every direction samples that
                    -- same image: one upload, one handle, one slot.
                    puAnims u `shouldSatisfy` all (\a →
                        all (all ((≡ fromMaybe "" (paAtlas a)) ∘ pfPath) ∘ pfdFrames)
                            (paDirs a))

        it "addresses frames as atlas CELLS: real per-direction counts \
           \(never the padded column count), a cell size from the index, \
           \and contiguous non-overlapping sub-rects across a row" $ do
            result ← buildPreviewUnit unitsCategoryRoot realUnit
            case result of
                Left err → expectationFailure
                    (T.unpack (unitFocusErrorMessage err))
                Right u →
                    -- injured_idle is one of the four checked-in acolyte
                    -- animations whose directions hold UNEQUAL frame
                    -- counts (D-5) — exactly where a padded column count
                    -- would show up as invented frames.
                    case find ((≡ "injured_idle") ∘ paName) (puAnims u) of
                        Nothing → expectationFailure "acolyte has no injured_idle"
                        Just a → do
                            let counts = map (length ∘ pfdFrames) (paDirs a)
                            length (nub counts) `shouldSatisfy` (> 1)
                            paDirs a `shouldSatisfy` all (not ∘ null ∘ pfdFrames)
                            -- Every frame carries the index's cell size…
                            let cells = [ pfCell f
                                        | d ← paDirs a, f ← pfdFrames d ]
                            nub cells `shouldSatisfy` \cs →
                                length cs ≡ 1 ∧ all isJust cs
                            -- …and one direction's columns tile its row
                            -- left to right without gaps or overlap.
                            case paDirs a of
                                [] → expectationFailure "injured_idle has no directions"
                                (d : _) → do
                                    let uvs = map pfUV (pfdFrames d)
                                        us  = [ (u0, u1) | (u0, _, u1, _) ← uvs ]
                                        vs  = [ (v0, v1) | (_, v0, _, v1) ← uvs ]
                                    length (nub vs) `shouldBe` 1
                                    us `shouldSatisfy` \xs →
                                        and [ u1 ≡ u0'
                                            | ((_, u1), (u0', _)) ← zip xs (drop 1 xs) ]
                                    us `shouldSatisfy` all (\(u0, u1) → u0 < u1)

        it "mirrors an atlas-backed clip from its own eastern cells, and \
           \leaves an all-eight-direction clip unmirrored" $ do
            result ← buildPreviewUnit unitsCategoryRoot realUnit
            case result of
                Left err → expectationFailure
                    (T.unpack (unitFocusErrorMessage err))
                Right u → do
                    -- flip comes from the INDEX now, and the index's
                    -- flip was proved equal to the YAML's before any of
                    -- this was published.
                    case find ((≡ "idle") ∘ paName) (puAnims u) of
                        Nothing → expectationFailure "acolyte has no idle"
                        Just a → do
                            paFlip a `shouldBe` True
                            map pfdSource (filter pfdMirrored (paDirs a))
                                `shouldBe` ["south-east", "east", "north-east"]
                            -- A mirrored cell reuses its SOURCE
                            -- direction's row: same v-range, and never
                            -- a row of its own.
                            let rowOf n = listToMaybe
                                    [ (v0, v1)
                                    | d ← paDirs a, pfdDirection d ≡ n
                                    , (_, v0, _, v1) ← take 1 (map pfUV (pfdFrames d)) ]
                            rowOf "west" `shouldBe` rowOf "east"
                    -- attack_heavy_RH_dagger authors all eight
                    -- directions with flip: false so the dagger never
                    -- swaps hands: eight distinct rows, nothing mirrored.
                    case find ((≡ "attack_heavy_RH_dagger") ∘ paName) (puAnims u) of
                        Nothing → expectationFailure "acolyte has no RH dagger attack"
                        Just a → do
                            paFlip a `shouldBe` False
                            filter pfdMirrored (paDirs a) `shouldBe` []
                            length (paDirs a) `shouldBe` 8
                            let rows = [ (v0, v1)
                                       | d ← paDirs a
                                       , (_, v0, _, v1) ← take 1 (map pfUV (pfdFrames d)) ]
                            length (nub rows) `shouldBe` 8

        it "REJECTS the whole target when the compiled index is unusable, \
           \instead of quietly showing the source frames beside it" $
            withCompiledUnitFixture (Just "{ not json") $ \resRoot catRoot → do
                result ← buildPreviewUnitIn resRoot catRoot brokenUnit
                case result of
                    Right _ → expectationFailure
                        "a corrupt index still resolved a preview target"
                    Left err → do
                        err `shouldSatisfy` \case
                            UnitAtlasRejected _ → True
                            _                   → False
                        unitFocusErrorMessage err `shouldSatisfy`
                            T.isInfixOf (T.pack brokenUnit)

        it "REJECTS an atlas/ directory with no index at all — an \
           \incomplete compiled artifact is not a legacy unit" $
            withCompiledUnitFixture Nothing $ \resRoot catRoot → do
                result ← buildPreviewUnitIn resRoot catRoot brokenUnit
                result `shouldSatisfy` \case
                    Left (UnitAtlasRejected _) → True
                    _                          → False

        it "stays on the legacy path when the unit ships no atlas/ \
           \directory at all — the only tolerated absence" $
            withUnitFixture $ \root → do
                result ← buildPreviewUnitIn root root "fixture_unit"
                case result of
                    Left err → expectationFailure
                        (T.unpack (unitFocusErrorMessage err))
                    Right u → map paAtlas (puAnims u)
                        `shouldSatisfy` all isNothing

        it "resolves a shipped ASSET-ONLY unit from its asset_units \
           \declaration, with the declared playback metadata" $ do
            result ← buildPreviewUnit unitsCategoryRoot assetOnlyUnit
            case result of
                Left err → expectationFailure
                    (T.unpack (unitFocusErrorMessage err))
                Right u → do
                    puAnims u `shouldSatisfy` not ∘ null
                    puAnims u `shouldSatisfy` all (\a → paFps a ≡ 8 ∧ paLoop a)
                    -- The declaration states flip: true over the
                    -- canonical five, so every one of the eight cells
                    -- still resolves — the same visible result the
                    -- pre-#1257 inference produced.
                    puAnims u `shouldSatisfy` all (\a → paFlip a)
                    puAnims u `shouldSatisfy` all
                        (\a → map pfdDirection (paDirs a)
                                ≡ map directionDirName previewDirectionOrder)

        it "rejects an unknown unit through the same error the CLI prints" $ do
            result ← buildPreviewUnit unitsCategoryRoot "nosuch"
            result `shouldBe` Left UnitNotFound

    describe "loadUnitAnimMeta" $ do
        it "extracts the shipped acolyte's animation metadata" $ do
            meta ← loadUnitAnimMeta "acolyte"
            Map.lookup "idle" meta `shouldSatisfy` isJust
            fmap uyaFlip (Map.lookup "idle" meta) `shouldBe` Just True

        it "extracts a shipped ASSET-ONLY unit's metadata from its \
           \asset_units: block, exactly as it does a gameplay unit's" $ do
            meta ← loadUnitAnimMeta (T.pack assetOnlyUnit)
            sort (Map.keys meta) `shouldBe` ["idle", "run"]
            fmap uyaFlip (Map.lookup "idle" meta) `shouldBe` Just True
            fmap uyaFps  (Map.lookup "idle" meta) `shouldBe` Just 8
            fmap uyaLoop (Map.lookup "idle" meta) `shouldBe` Just True

        it "is empty for a name no YAML file exists for at all" $ do
            meta ← loadUnitAnimMeta "definitely_not_a_unit_887"
            meta `shouldBe` Map.empty

    -- Sanity: the real trees this spec leans on exist in this checkout,
    -- or half the assertions above prove nothing.
    describe "fixture sanity" $ do
        it "the shipped acolyte and tiller asset trees exist, and the \
           \asset-only tree really does carry a declaration now" $ do
            a ← doesDirectoryExist (unitsCategoryRoot </> realUnit)
            t ← doesDirectoryExist (unitsCategoryRoot </> assetOnlyUnit)
            (a, t) `shouldBe` (True, True)
            declared ← doesFileExist (unitDataPath (T.pack assetOnlyUnit))
            declared `shouldBe` True
