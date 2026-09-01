-- | Focused tests for 'Engine.Preview.Unit' (#887, Phase 3 of the
--   @--preview@ browser epic #427): the pure direction-mirroring table,
--   the default-selection rule, animation ordering/labeling, numeric
--   frame ordering, YAML metadata extraction, the unequal-frame-count
--   playback rule, and the documented end-of-clip policy — since #1833
--   a CONTINUOUS REPLAY, for every clip, whatever its authored @loop@
--   says — plus the filesystem containment rules that reject a bad
--   @units\/\<name\>@ target before a window is ever created. No engine
--   needed.
--
--   Since #1261 (TEX-6) the viewer's animation list comes from the unit
--   YAML and its compiled index, not from a filesystem walk, and every
--   frame is an atlas cell. The end-to-end cases run against all SEVEN
--   shipped trees through the PRODUCTION loader and the artifacts
--   actually checked in — which is the whole of D-9, since a
--   preview-only decoder would keep passing while the compiled tree
--   rotted underneath it.
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
import Unit.Atlas.Types
    ( AtlasAnimation(..), AtlasDirectionRow(..), AtlasStorageFormat(..) )
import Unit.Direction (Direction(..))

-- The real shipped acolyte tree — every ordering/mirroring claim below
-- is also proved against the ACTUAL canonical layout, not just a
-- synthetic fixture (the Test.Headless.Preview.Discovery convention).
realUnit ∷ String
realUnit = "acolyte"

-- Every shipped unit tree, gameplay and asset-only alike. Each one must
-- resolve through the production preview loader.
shippedUnits ∷ [String]
shippedUnits =
    [ "acolyte", "bear_brown", "red_squirrel", "technomule"
    , "tiller", "unknown_unit", "white_tailed_deer", "nomad_primitive" ]

-- One of the three trees #1261 promoted from #1257's inventory-only
-- `asset_units:` form to a real `units:` definition. Its animations
-- declare the canonical five with flip: true, so every one of the eight
-- display cells still resolves.
promotedUnit ∷ String
promotedUnit = "tiller"

-- Per-direction preview frames LABELLED by path, for the pure mirroring
-- table: what that table owns is which direction's frames a display
-- cell resolved to, not the atlas arithmetic (covered end to end
-- below). The cell size is arbitrary and never read there.
labelledFrames ∷ [(Direction, [Text])] → Map.Map Direction [PreviewFrame]
labelledFrames = Map.fromList ∘ map (\(d, ps) → (d, map one ps))
  where one path = PreviewFrame path (0, 0, 1, 1) (1, 1)

-- A synthetic compiled-index record: since #1261 the atlas selection is
-- the ONLY input 'buildPreviewAnims' has.
fakeAtlas ∷ Text → Float → Bool → Bool → [(Direction, Int)] → AtlasAnimation
fakeAtlas name fps loop flipV dirs = AtlasAnimation
    { aaName         = name
    , aaFormat       = AtlasFormatPng
    , aaPath         = "assets/textures/units/fake/atlas/"
                       ⧺ T.unpack name ⧺ ".png"
    , aaAtlasWidth   = cols * (cellW + 2 * cellPad)
    , aaAtlasHeight  = rows * (cellH + 2 * cellPad)
    , aaCellWidth    = cellW
    , aaCellHeight   = cellH
    , aaCellPadding  = cellPad
    , aaColumns      = cols
    , aaRows         = rows
    , aaFlip         = flipV
    , aaFps          = fps
    , aaLoop         = loop
    , aaDirections   = Map.fromList
        [ (d, AtlasDirectionRow d r n) | (r, (d, n)) ← zip [0 ..] ordered ]
    , aaSourceDigest = "source-digest"
    , aaAtlasDigest  = "atlas-digest"
    }
  where
    cellW = 16
    cellH = 24
    -- The #2076 extrusion gutter. The preview viewer resolves its cells
    -- through the game's own `atlasCellUV`, so the sheet it describes
    -- has to be the padded one the compiler emits.
    cellPad = 1
    ordered = [ (d, n) | d ← previewDirectionOrder, Just n ← [lookup d dirs] ]
    rows = length ordered
    cols = maximum (1 : map snd ordered)

selection ∷ [AtlasAnimation] → HM.HashMap Text AtlasAnimation
selection = HM.fromList ∘ map (\aa → (aaName aa, aa))

-- | The single animation a one-entry selection assembles to. Partial on
--   purpose: a selection of one that yields anything else is itself the
--   failure, and reporting it here beats a pattern-match warning.
onlyAnim ∷ HasCallStack ⇒ HM.HashMap Text AtlasAnimation → PreviewAnim
onlyAnim sel = case buildPreviewAnims sel of
    [a] → a
    out → error ("expected one animation, got " ⧺ show (map paName out))

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

-- | A STRUCTURALLY sound index whose animation declares a
--   representation this build has no decoder for — the shape deferred
--   TEX-5 will one day make readable, and which must reject cleanly
--   until it does. Everything else about the document is canonical, so
--   a rejection here can only be about the format.
unknownFormatIndex ∷ String → String
unknownFormatIndex unit = concat
    [ "{\"schema_version\":2,\"generator\":\"tools/pack_atlas.py\""
    , ",\"tool_version\":2,\"digest_algorithm\":\"sha256\""
    , ",\"unit\":\"", unit, "\""
    , ",\"direction_order\":[\"south\",\"south-west\",\"west\""
    , ",\"north-west\",\"north\",\"north-east\",\"east\",\"south-east\"]"
    , ",\"animations\":[{\"name\":\"idle\",\"storage_format\":\"ktx2\""
    , ",\"atlas_path\":\"assets/textures/units/", unit, "/atlas/idle.png\""
    , ",\"atlas_width\":34,\"atlas_height\":34"
    , ",\"cell_width\":32,\"cell_height\":32,\"cell_padding\":1"
    , ",\"columns\":1,\"rows\":1"
    , ",\"flip\":true,\"fps\":8,\"loop\":true"
    , ",\"directions\":[{\"direction\":\"south\",\"row\":0"
    , ",\"frame_count\":1}]"
    , ",\"source_digest\":\"aaaa\",\"atlas_digest\":\"bbbb\"}]}"
    ]

-- A unit whose YAML DECLARES an animation while its tree ships no
-- compiled artifacts at all — the state every unit but acolyte was in
-- before #1261, and a rejection since. Its own temp resource root, for
-- the same reason 'withCompiledUnitFixture' has one.
uncompiledUnit ∷ String
uncompiledUnit = "spec_uncompiled_unit"

withUncompiledUnitFixture ∷ (FilePath → FilePath → IO ()) → IO ()
withUncompiledUnitFixture action = do
    tmp ← getTemporaryDirectory
    let resRoot = tmp </> "synarchy-preview-unit-uncompiled-spec"
        catRoot = resRoot </> unitsCategoryRoot
        unitDir = catRoot </> uncompiledUnit
        framePath = "assets/textures/units/" ⧺ uncompiledUnit
                    ⧺ "/animations/idle/south/frame_000.png"
    createDirectoryIfMissing True (unitDir </> "animations" </> "idle" </> "south")
    writeFile (resRoot </> framePath) ""
    createDirectoryIfMissing True (resRoot </> "data" </> "units")
    writeFile (resRoot </> unitDataPath (T.pack uncompiledUnit))
        (unlines
            [ "units:"
            , "  - name: " ⧺ uncompiledUnit
            , "    sprite: \"" ⧺ framePath ⧺ "\""
            , "    animations:"
            , "      idle:"
            , "        frames:"
            , "          south:"
            , "            - \"" ⧺ framePath ⧺ "\"" ])
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

    describe "resolveAnimDirections" $ do
        it "mirrors W/SW/NW from their eastern counterparts when flipping \
           \is allowed, reporting each cell's real source" $ do
            let stored = labelledFrames [ (DirS, ["s"]), (DirSE, ["se"]), (DirE, ["e"])
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
                (labelledFrames [ (DirS, ["s"]), (DirSE, ["se"]), (DirE, ["e"])
                           , (DirNE, ["ne"]), (DirN, ["n"]) ]))
                `shouldBe` ["south", "north", "north-east", "east", "south-east"]

        it "prefers a directly authored western direction over mirroring, \
           \even with flipping enabled" $ do
            let stored = labelledFrames [ (DirS, ["s"]), (DirSE, ["se"]), (DirE, ["e"])
                                   , (DirNE, ["ne"]), (DirN, ["n"])
                                   , (DirW, ["authored-w"]) ]
                out = resolveAnimDirections True stored
                west = find ((≡ "west") ∘ pfdDirection) out
            fmap pfdMirrored west `shouldBe` Just False
            fmap pfdSource west `shouldBe` Just "west"
            fmap framePaths west `shouldBe` Just ["authored-w"]

        it "treats an empty frame list as no direction at all, and never \
           \mirrors from one" $
            resolveAnimDirections True (labelledFrames [(DirS, ["s"]), (DirE, [])])
                `shouldSatisfy` \out →
                    map pfdDirection out ≡ ["south"]

    describe "frameIndexAt" $ do
        it "wraps a looping clip at its own frame count" $
            map (\t → frameIndexAt True 8 4 t) [0, 0.13, 0.26, 0.4, 0.51]
                `shouldBe` [0, 1, 2, 3, 0]

        -- #1833 Requirement 9. This is the whole of the preview replay
        -- policy: the source `loop` value is still an ARGUMENT (both
        -- Lua mirrors still pass it, and both dumps still report it
        -- unchanged), and this function deliberately never reads it.
        -- The clip below is a source `loop: false` one — 4 frames at
        -- 8 fps, so a 0.5 s cycle — and it must wrap at the cycle
        -- boundary and keep going, not freeze on frame 3.
        it "REPLAYS a source loop:false clip: it wraps at frameCount/fps \
           \and keeps advancing through later cycles (#1833)" $
            map (\t → frameIndexAt False 8 4 t)
                [0, 0.4, 0.5, 0.6, 0.9, 1.0, 10.0, 10.4]
                `shouldBe` [0, 3, 0, 0, 3, 0, 0, 3]

        it "replays a source loop:false clip identically to a loop:true one \
           \— the authored value never reaches the index (#1833)" $
            map (\t → frameIndexAt False 8 4 t) [0, 0.13, 0.26, 0.4, 0.51]
                `shouldBe` map (\t → frameIndexAt True 8 4 t)
                               [0, 0.13, 0.26, 0.4, 0.51]

        it "gives each direction its OWN index from the SAME elapsed value, \
           \so unequal frame counts stay phase-aligned" $ do
            -- 0.9 s at 10 fps = raw frame 9: a 4-frame direction shows 1,
            -- a 3-frame direction shows 0 — both from one shared clock.
            frameIndexAt True 10 4 0.9 `shouldBe` 1
            frameIndexAt True 10 3 0.9 `shouldBe` 0

        -- #1833: forced replay makes the unequal-frame-count case
        -- observable PAST the first cycle, where the two directions
        -- have wrapped a different number of times. That divergence is
        -- the intended reading of "phase-aligned" (one clock, each
        -- direction modulo its own count), so pin it — a future change
        -- to per-direction clocking has to fail here.
        it "keeps unequal-frame-count directions on ONE clock past their \
           \first wrap, so they no longer share a frame ordinal (#1833)" $ do
            -- 2.5 s at 10 fps = raw frame 25, well past both wraps:
            -- a 5-frame direction shows 0, a 3-frame direction shows 1.
            frameIndexAt False 10 5 2.5 `shouldBe` 0
            frameIndexAt False 10 3 2.5 `shouldBe` 1
            -- One tick later they advance together but stay divergent.
            frameIndexAt False 10 5 2.6 `shouldBe` 1
            frameIndexAt False 10 3 2.6 `shouldBe` 2

        it "is 0 for a single-frame or empty direction, at any time" $ do
            frameIndexAt True 8 1 99.0 `shouldBe` 0
            frameIndexAt True 8 0 99.0 `shouldBe` 0
            -- Forced replay must not make a one-frame clip start moving.
            frameIndexAt False 8 1 99.0 `shouldBe` 0

        it "stays on frame 0 at a non-positive effective fps, so replay \
           \never divides a zero-length cycle (#1833)" $ do
            frameIndexAt False 0 4 99.0 `shouldBe` 0
            frameIndexAt False (-5) 4 99.0 `shouldBe` 0

        it "is 0 before the clock starts (a negative elapsed can't index \
           \out of range)" $ do
            frameIndexAt True 8 4 (-5.0) `shouldBe` 0
            frameIndexAt False 8 4 (-5.0) `shouldBe` 0

    describe "buildPreviewAnims (the compiled index is the whole input)" $ do
        it "reads fps/loop/flip from each animation's index record" $
            map (\a → (paName a, paFps a, paLoop a, paFlip a))
                (buildPreviewAnims (selection
                    [ fakeAtlas "attack" 12 False True [(DirS, 2)]
                    , fakeAtlas "idle" 8 True False [(DirS, 1)] ]))
                `shouldBe` [("attack", 12, False, True), ("idle", 8, True, False)]

        it "orders animations case-sensitively by name, matching the \
           \simple browser's Ord-on-the-label rule" $
            map paName (buildPreviewAnims (selection
                [ fakeAtlas n 8 True False [(DirS, 1)]
                | n ← ["walk", "Idle", "attack_heavy_RH_dagger", "idle"] ]))
                `shouldBe` ["Idle", "attack_heavy_RH_dagger", "idle", "walk"]

        it "lists EXACTLY the selection — an animation folder that exists \
           \on disk but is absent from the unit YAML has no index record, \
           \so it cannot appear here at all" $
            map paName (buildPreviewAnims (selection
                [fakeAtlas "idle" 8 True False [(DirS, 1)]]))
                `shouldBe` ["idle"]

        it "reports the south frame-zero thumbnail as an atlas CELL, and \
           \none at all when the animation authors no south row" $ do
            let out = buildPreviewAnims (selection
                    [ fakeAtlas "a" 8 True False [(DirS, 2)]
                    , fakeAtlas "b" 8 True False [(DirE, 1)] ])
            map (fmap pfPath ∘ paThumb) out `shouldBe`
                [ Just "assets/textures/units/fake/atlas/a.png", Nothing ]
            map (fmap pfCell ∘ paThumb) out `shouldBe` [Just (16, 24), Nothing]

        it "gives every frame the animation's own atlas, its cell size, \
           \and a sub-rect that is never the whole sheet" $ do
            let a  = onlyAnim (selection
                          [fakeAtlas "run" 8 True False [(DirS, 3), (DirE, 3)]])
                fs = concatMap pfdFrames (paDirs a)
            paAtlas a `shouldBe` "assets/textures/units/fake/atlas/run.png"
            map pfPath fs `shouldSatisfy` all (≡ paAtlas a)
            map pfCell fs `shouldSatisfy` all (≡ (16, 24))
            map pfUV fs `shouldSatisfy` all (≢ (0, 0, 1, 1))

        it "addresses the index's REAL per-direction counts, never the \
           \padded column count (D-5)" $ do
            let a = onlyAnim (selection
                        [fakeAtlas "uneven" 8 True False [(DirS, 4), (DirE, 1)]])
            [ (pfdDirection d, length (pfdFrames d)) | d ← paDirs a ]
                `shouldBe` [("south", 4), ("east", 1)]

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

    -- #1261 flipped authority from the filesystem to the unit YAML and
    -- its compiled index. The exclusion below is the whole of that
    -- change's observable behaviour for undeclared content, and the
    -- fixture tree is exactly the shape a developer's uncommitted
    -- animation folder has: real frames on disk, nothing declaring
    -- them. Nothing COMMITTED can be in that state —
    -- `tools/pack_atlas.py --validate-only --strict` fails on any
    -- animation PNG no declaration owns, which is where an undeclared
    -- folder is reported loudly and by path.
    describe "YAML/index authority (#1261)" $ do
        it "EXCLUDES an animation folder that exists on disk but is \
           \declared nowhere, rather than rendering its source frames" $
            withUnitFixture $ \root → do
                -- four populated animation folders, no data/units YAML
                result ← buildPreviewUnitIn root root "fixture_unit"
                result `shouldBe` Left UnitNoAnimations

        it "REJECTS a unit whose YAML declares animations it ships no \
           \compiled atlas for, naming the unit" $
            withUncompiledUnitFixture $ \resRoot catRoot → do
                result ← buildPreviewUnitIn resRoot catRoot uncompiledUnit
                case result of
                    Right _ → expectationFailure
                        "an uncompiled unit still resolved a preview target"
                    Left err → do
                        err `shouldSatisfy` \case
                            UnitAtlasRejected _ → True
                            _                   → False
                        unitFocusErrorMessage err `shouldSatisfy`
                            T.isInfixOf (T.pack uncompiledUnit)

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
                    length (puAnims u) `shouldBe` 54
                    -- Each animation's own atlas, named after itself —
                    -- D-2's one-atlas-per-animation, observed rather
                    -- than assumed.
                    [ (paName a, paAtlas a) | a ← puAnims u ] `shouldSatisfy`
                        all (\(n, p) → p ≡ "assets/textures/units/acolyte/atlas/"
                                            <> n <> ".png")
                    -- and every frame of every direction samples that
                    -- same image: one upload, one handle, one slot.
                    puAnims u `shouldSatisfy` all (\a →
                        all (all ((≡ paAtlas a) ∘ pfPath) ∘ pfdFrames)
                            (paDirs a))

        it "addresses frames as atlas CELLS: real per-direction counts \
           \(never the padded column count), a cell size from the index, \
           \and gutter-separated non-overlapping sub-rects across a row" $ do
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
                                length cs ≡ 1 ∧ all (\(w, hh) → w > 0 ∧ hh > 0) cs
                            -- …and one direction's columns march left to
                            -- right across its row, separated by exactly
                            -- the #2076 extrusion gutter — two texels of
                            -- padding between neighbouring cell
                            -- interiors, never zero (which would be the
                            -- old edge-adjacent stride) and never a
                            -- whole cell (which would mean a skipped
                            -- column).
                            case paDirs a of
                                [] → expectationFailure "injured_idle has no directions"
                                (d : _) → do
                                    let uvs = map pfUV (pfdFrames d)
                                        us  = [ (u0, u1) | (u0, _, u1, _) ← uvs ]
                                        vs  = [ (v0, v1) | (_, v0, _, v1) ← uvs ]
                                        -- Widths in texels, from the
                                        -- index's own cell size, so the
                                        -- gap is asserted absolutely.
                                        sheetW = case nub [ pfCell f
                                                          | f ← pfdFrames d ] of
                                            ((cw, _) : _) → case us of
                                                ((u0, u1) : _) | u1 > u0 →
                                                    fromIntegral cw / (u1 - u0)
                                                _ → 0
                                            [] → 0
                                        gaps = [ (u0' - u1) * sheetW
                                               | ((_, u1), (u0', _)) ←
                                                   zip us (drop 1 us) ]
                                    length (nub vs) `shouldBe` 1
                                    gaps `shouldSatisfy` all
                                        (\g → abs (g - 2) < 0.01)
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

        -- D-9 keeps the preview on the production path, which means the
        -- format-neutral boundary (D-10) has to reject HERE too. A
        -- viewer that fell back to the source frames beside an
        -- unreadable representation would render art gameplay cannot,
        -- and that is precisely the class of regression this viewer
        -- exists to catch.
        it "REJECTS a representation this build cannot read, rather than \
           \falling back to the frames beside it" $
            withCompiledUnitFixture (Just (unknownFormatIndex brokenUnit)) $
                \resRoot catRoot → do
                    result ← buildPreviewUnitIn resRoot catRoot brokenUnit
                    case result of
                        Right _ → expectationFailure
                            "an unreadable representation still previewed"
                        Left err → do
                            err `shouldSatisfy` \case
                                UnitAtlasRejected _ → True
                                _                   → False
                            unitFocusErrorMessage err `shouldSatisfy`
                                T.isInfixOf "storage_format"

        it "resolves one of the trees #1261 promoted out of the \
           \inventory-only asset_units: form, with its declared \
           \playback metadata and all eight cells still populated" $ do
            result ← buildPreviewUnit unitsCategoryRoot promotedUnit
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
                    puAnims u `shouldSatisfy` all paFlip
                    puAnims u `shouldSatisfy` all
                        (\a → map pfdDirection (paDirs a)
                                ≡ map directionDirName previewDirectionOrder)

        -- Every shipped tree, every one of its animations, through the
        -- production loader and generated index/cell metadata. Per-unit
        -- `it`s ensure a failure names the tree that broke.
        forM_ shippedUnits $ \unitName →
            it ("resolves " ⧺ unitName ⧺ ": every animation atlas-backed, \
                \every frame a cell of its own animation's atlas") $ do
                result ← buildPreviewUnit unitsCategoryRoot unitName
                case result of
                    Left err → expectationFailure
                        (T.unpack (unitFocusErrorMessage err))
                    Right u → do
                        puAnims u `shouldSatisfy` not ∘ null
                        puDefault u `shouldSatisfy` not ∘ T.null
                        forM_ (puAnims u) $ \a → do
                            paAtlas a `shouldBe`
                                T.pack ("assets/textures/units/" ⧺ unitName
                                        ⧺ "/atlas/") <> paName a <> ".png"
                            -- Every direction has real frames, all
                            -- sampling that one image at a real cell
                            -- size and a sub-rect that is not the sheet.
                            paDirs a `shouldSatisfy` not ∘ null
                            let fs = concatMap pfdFrames (paDirs a)
                            fs `shouldSatisfy` not ∘ null
                            map pfPath fs `shouldSatisfy` all (≡ paAtlas a)
                            map pfCell fs `shouldSatisfy`
                                all (\(w, hh) → w > 0 ∧ hh > 0)
                            map pfUV fs `shouldSatisfy` all (≢ (0, 0, 1, 1))

        it "rejects an unknown unit through the same error the CLI prints" $ do
            result ← buildPreviewUnit unitsCategoryRoot "nosuch"
            result `shouldBe` Left UnitNotFound

    describe "loadUnitAnimMeta" $ do
        it "extracts the shipped acolyte's animation metadata" $ do
            meta ← loadUnitAnimMeta "acolyte"
            Map.lookup "idle" meta `shouldSatisfy` isJust
            fmap uyaFlip (Map.lookup "idle" meta) `shouldBe` Just True

        it "extracts a promoted tree's metadata from its units: block" $ do
            meta ← loadUnitAnimMeta (T.pack promotedUnit)
            sort (Map.keys meta) `shouldBe` ["idle", "run"]
            fmap uyaFlip (Map.lookup "idle" meta) `shouldBe` Just True
            fmap uyaFps  (Map.lookup "idle" meta) `shouldBe` Just 8
            fmap uyaLoop (Map.lookup "idle" meta) `shouldBe` Just True

        it "extracts the shipped nomad metadata from its units: block" $ do
            meta ← loadUnitAnimMeta "nomad_primitive"
            Map.size meta `shouldBe` 15
            fmap uyaLoop (Map.lookup "injured_combat_idle_unarmed" meta)
                `shouldBe` Just True

        it "is empty for a name no YAML file exists for at all" $ do
            meta ← loadUnitAnimMeta "definitely_not_a_unit_887"
            meta `shouldBe` Map.empty

    -- Sanity: the real trees this spec leans on exist in this checkout,
    -- or half the assertions above prove nothing.
    describe "fixture sanity" $ do
        it "every shipped unit tree exists and carries both its \
           \declaration and its compiled index" $
            forM_ shippedUnits $ \unitName → do
                tree ← doesDirectoryExist (unitsCategoryRoot </> unitName)
                declared ← doesFileExist (unitDataPath (T.pack unitName))
                indexed ← doesFileExist (unitAtlasIndexPath (T.pack unitName))
                (unitName, tree, declared, indexed)
                    `shouldBe` (unitName, True, True, True)
