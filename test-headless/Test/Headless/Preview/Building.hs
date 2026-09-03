-- | Focused tests for 'Engine.Preview.Building' and the shared
--   grouped-item containment rule 'Engine.Preview.Discovery.resolveItemDir'
--   (#888, Phase 4 of the @--preview@ browser epic #427): direct-child
--   containment and symlink rejection, the combined static/animation
--   entry list and its ordering, numeric frame ordering, the
--   CONTENT-based YAML association that resolves @portal-idle@ to the
--   @idle\/@ directory despite the name mismatch, the built /
--   sprite / default.png / first-entry default-selection ladder, the
--   exact building metadata defaults (@fps=8@, @loop=false@ — NOT the
--   units viewer's @loop=true@), a building with no YAML at all
--   (@dungeon_1@), and its non-animation @damaged\/@ subtree, plus
--   (#1417) the non-FILES a supported extension can name — a
--   @.png@-suffixed directory, through both the YAML-matched and the
--   numbered-name branches, a symlink pointing at a real frame, and a
--   FIFO, the special file @doesFileExist@ accepts — the last of those
--   reached through the ANIMATION branch and, since #2199, through the
--   STATIC branch as well.
--   No engine needed — everything here is pure or filesystem-only.
module Test.Headless.Preview.Building (spec) where

import UPrelude
import Test.Hspec
import Control.Exception (finally)
import Control.Monad (filterM)
import Data.List (isPrefixOf, sort)
import qualified Data.ByteString.Char8 as BS
import qualified Data.Text as T
import qualified Data.Map.Strict as Map
import qualified Data.Yaml as Yaml
import System.Directory
    ( getTemporaryDirectory, createDirectoryIfMissing, removeDirectoryRecursive
    , createDirectoryLink, removeDirectoryLink, createFileLink
    , doesDirectoryExist )
import System.FilePath ((</>))
import System.Posix.Files (createNamedPipe, stdFileMode)
import Building.Schema (legacyAssets)
import Engine.Asset.YamlBuildings (BuildingYamlAnim(..))
import Engine.Core.Types (PreviewBuilding(..), PreviewBuildingEntry(..))
import Engine.Preview.Building
import Engine.Preview.Discovery (ItemDirError(..), resolveItemDir)

-- The real shipped trees — every claim below is proved against the
-- ACTUAL canonical layout, not just a synthetic fixture (the
-- Test.Headless.Preview.Discovery / .UnitAnimation convention).
--
--   acolyte_portal — YAML animation names (portal-appear/portal-idle)
--                    that DON'T match their directories (appear/idle),
--                    plus state_animations.built and a default.png.
--   cargo_hold_S   — a YAML animation (construct) AND a demolish/
--                    folder the YAML never mentions, with NO
--                    state_animations.built at all.
--   dungeon_1      — no data/buildings YAML whatsoever, no default.png,
--                    and a damaged/ subtree of ordinary statics.
portalBuilding, cargoBuilding, dungeonBuilding ∷ String
portalBuilding  = "acolyte_portal"
cargoBuilding   = "cargo_hold_S"
dungeonBuilding = "dungeon_1"

labelsOf ∷ [PreviewBuildingEntry] → [Text]
labelsOf = map pbeLabel

entryNamed ∷ Text → [PreviewBuildingEntry] → Maybe PreviewBuildingEntry
entryNamed lbl = listToMaybe ∘ filter ((≡ lbl) ∘ pbeLabel)

-- Discover a real shipped building's entries through the same pairing
-- 'buildPreviewBuilding' uses (its own YAML augmenting its own folder).
realEntries ∷ String → IO [PreviewBuildingEntry]
realEntries name = do
    meta ← loadBuildingPreviewMeta (T.pack name)
    discoverBuildingEntries (bpmAnims meta) (buildingsCategoryRoot </> name)

anim ∷ Float → Bool → [Text] → BuildingYamlAnim
anim fps loop framePaths = BuildingYamlAnim
    { byaFps = fps, byaLoop = loop
    , byaFrames = legacyAssets framePaths }

static ∷ Text → Text → PreviewBuildingEntry
static lbl path = PreviewBuildingEntry
    { pbeLabel = lbl, pbeAnimated = False, pbeFps = buildingDefaultFps
    , pbeLoop = buildingDefaultLoop, pbeFrames = [path] }

animated ∷ Text → [Text] → PreviewBuildingEntry
animated lbl frames = PreviewBuildingEntry
    { pbeLabel = lbl, pbeAnimated = True, pbeFps = buildingDefaultFps
    , pbeLoop = buildingDefaultLoop, pbeFrames = frames }

-- A synthetic building tree, for the cases no shipped asset exhibits:
--   <root>/unpadded/frame_{1,2,10}.png -- numeric, not lexicographic
--   <root>/mixed/{frame_001.png, cover.png}
--                                  -- NOT the frame convention: every
--                                     png must surface as a static
--   <root>/top.png                 -- a loose top-level static
withBuildingFixture ∷ (FilePath → IO ()) → IO ()
withBuildingFixture action = do
    tmp ← getTemporaryDirectory
    let root = tmp </> "synarchy-preview-building-spec"
        item = root </> "fixture_building"
        put sub fs = do
            createDirectoryIfMissing True (item </> sub)
            forM_ fs $ \f → writeFile (item </> sub </> f) ""
    createDirectoryIfMissing True item
    put "unpadded" ["frame_1.png", "frame_2.png", "frame_10.png"]
    put "mixed"    ["frame_001.png", "cover.png"]
    writeFile (item </> "top.png") ""
    (`finally` removeDirectoryRecursive root) (action root)

-- A symlinked item directory: rejected unconditionally before boot,
-- exactly like a symlinked unit directory (#887) — doesDirectoryExist
-- follows links, so browsing one would load another tree's textures
-- and break the trimmed-loading contract.
withSymlinkFixture ∷ (FilePath → IO ()) → IO ()
withSymlinkFixture action = do
    tmp ← getTemporaryDirectory
    let root = tmp </> "synarchy-preview-building-symlink-spec"
        outside = tmp </> "synarchy-preview-building-symlink-spec-outside"
    createDirectoryIfMissing True root
    createDirectoryIfMissing True outside
    writeFile (outside </> "default.png") ""
    createDirectoryLink outside (root </> "shortcut")
    -- Unlink the directory symlink FIRST so removeDirectoryRecursive is
    -- never given a chance to follow it into 'outside'.
    let cleanup = do
            removeDirectoryLink (root </> "shortcut")
            removeDirectoryRecursive root
            removeDirectoryRecursive outside
    (`finally` cleanup) (action root)

-- A tree of NON-FILES carrying a supported extension (#1417). A
-- @.png@ suffix is a NAME test, so every one of these children looks
-- like a frame to the extension filter while none of them is loadable:
--
--   <root>/dir_only/frame_001.png/wall.png
--                     -- the candidate animation directory's ONLY
--                        .png-named child is a DIRECTORY: dir_only is
--                        not an animation, and the genuine texture
--                        buried inside still surfaces
--   <root>/yaml_matched/{frame_001.png (a real file),
--                        frame_002.png/ (a directory)}
--                     -- matched to an animation BY YAML, which skips
--                        the numbered-name guard entirely
--   <root>/symlinked/{frame_001.png (a real file),
--                     frame_002.png -> frame_001.png}
--                     -- a symlink to a REGULAR FILE, which passes
--                        doesFileExist and must still be excluded
--   <root>/special/{frame_001.png (a real file),
--                   frame_002.png (a FIFO)}
--                     -- a special file is not a directory, so
--                        doesFileExist ACCEPTS it; only a real type
--                        check rejects it
--   <root>/{loose.png (a real file), pipe.png (a FIFO)}
--                     -- #2199: directly under the building root, so
--                        the STATIC producer is the only thing that
--                        sees them — no animation directory, and
--                        therefore no classifyDir, is involved
--   <root>/plain/{wall.png (a real file), pipe.png (a FIFO)}
--                     -- the same pair one level down, in a subfolder
--                        classifyDir declines (wall.png is no frame
--                        name), so the walk descends and the static
--                        producer sees both children there too
withNonFileFixture ∷ (FilePath → IO ()) → IO ()
withNonFileFixture action = do
    tmp ← getTemporaryDirectory
    let root = tmp </> "synarchy-preview-building-nonfile-spec"
        item = root </> "fixture_building"
        dir sub = createDirectoryIfMissing True (item </> sub)
        file sub = writeFile (item </> sub) ""
    createDirectoryIfMissing True item
    dir  ("dir_only" </> "frame_001.png")
    file ("dir_only" </> "frame_001.png" </> "wall.png")
    dir  "yaml_matched"
    file ("yaml_matched" </> "frame_001.png")
    dir  ("yaml_matched" </> "frame_002.png")
    dir  "symlinked"
    file ("symlinked" </> "frame_001.png")
    -- Relative, and inside the fixture, so the recursive cleanup
    -- unlinks it without ever resolving outside the temp tree.
    createFileLink "frame_001.png" (item </> "symlinked" </> "frame_002.png")
    dir  "special"
    file ("special" </> "frame_001.png")
    createNamedPipe (item </> "special" </> "frame_002.png") stdFileMode
    file "loose.png"
    createNamedPipe (item </> "pipe.png") stdFileMode
    dir  "plain"
    file ("plain" </> "wall.png")
    createNamedPipe (item </> "plain" </> "pipe.png") stdFileMode
    (`finally` removeDirectoryRecursive root) (action root)

spec ∷ Spec
spec = do
    describe "resolveItemDir (shared grouped-item containment)" $ do
        it "rejects a name carrying path structure, before touching the disk" $
            forM_ ["", "/etc", "sub/dir", ".", "..", "a/../b"] $ \bad → do
                got ← resolveItemDir buildingsCategoryRoot bad
                got `shouldBe` Left ItemDirEscapesRoot

        it "rejects an unknown item" $ do
            got ← resolveItemDir buildingsCategoryRoot "nosuch_building"
            got `shouldBe` Left ItemDirNotFound

        it "rejects a FILE sitting beside the real item directories" $ do
            -- assets/textures/flora/unknown_flora.png — the per-category
            -- fallback texture, a real file in a grouped category root.
            got ← resolveItemDir ("assets" </> "textures" </> "flora")
                                 "unknown_flora.png"
            got `shouldBe` Left ItemDirNotADirectory

        it "rejects a symlinked item directory unconditionally" $
            withSymlinkFixture $ \root → do
                got ← resolveItemDir root "shortcut"
                got `shouldBe` Left ItemDirSymlink

        it "accepts a real, contained building directory" $ do
            got ← resolveItemDir buildingsCategoryRoot portalBuilding
            got `shouldBe` Right (buildingsCategoryRoot </> portalBuilding)

    describe "isFrameFileName (the numbered-frame convention)" $ do
        it "accepts the checked-in frame spellings" $
            map isFrameFileName ["frame_000.png", "frame_10.png", "frame1.png"
                                , "frame-3.PNG"]
                `shouldBe` [True, True, True, True]
        it "rejects the dungeon piece sprites (dungeon_1/damaged/)" $
            map isFrameFileName ["floor.png", "wall_ne.png", "ceiling.png"
                                , "post.png", "default.png", "frame.png"]
                `shouldBe` [False, False, False, False, False, False]

    describe "buildingDefault* (the exact metadata defaults)" $
        it "matches BuildingYamlAnim, NOT the units viewer's loop=true" $ do
            buildingDefaultFps `shouldBe` 8.0
            buildingDefaultLoop `shouldBe` False

    describe "matchAnimForDir (content association, never equal names)" $ do
        it "matches a directory through the frame paths its YAML declares" $ do
            let anims = Map.fromList
                    [ ("portal-idle", anim 8 True
                        ["assets/textures/buildings/acolyte_portal/idle/frame_000.png"])
                    , ("portal-appear", anim 8 False
                        ["assets/textures/buildings/acolyte_portal/appear/frame_000.png"])
                    ]
            fst <$> matchAnimForDir anims
                    ("assets" </> "textures" </> "buildings" </> "acolyte_portal"
                        </> "idle")
                `shouldBe` Just "portal-idle"

        it "does not match a directory merely sharing an animation's name" $ do
            let anims = Map.fromList
                    [ ("idle", anim 8 True ["assets/textures/buildings/x/other/f.png"]) ]
            fst <$> matchAnimForDir anims ("assets" </> "textures" </> "buildings"
                                            </> "x" </> "idle")
                `shouldBe` Nothing

    describe "discoverBuildingEntries (real shipped trees)" $ do
        it "lists acolyte_portal's animation dirs and its static together, \
           \in label order" $ do
            entries ← realEntries portalBuilding
            labelsOf entries `shouldBe` ["appear", "default.png", "idle"]
            map pbeAnimated entries `shouldBe` [True, False, True]

        it "augments a matched animation with its OWN yaml fps/loop" $ do
            entries ← realEntries portalBuilding
            -- portal-idle: fps 8, loop true; portal-appear: fps 8, loop false.
            fmap pbeLoop (entryNamed "idle" entries)   `shouldBe` Just True
            fmap pbeLoop (entryNamed "appear" entries) `shouldBe` Just False
            fmap pbeFps  (entryNamed "idle" entries)   `shouldBe` Just 8.0

        it "orders an animation's frames numerically, from the filesystem" $ do
            entries ← realEntries portalBuilding
            let frames = maybe [] pbeFrames (entryNamed "idle" entries)
            length frames `shouldBe` 8
            frames `shouldBe` sort frames   -- zero-padded: numeric ≡ lexicographic
            (T.unpack <$> listToMaybe frames)
                `shouldBe` Just (buildingsCategoryRoot </> portalBuilding
                                    </> "idle" </> "frame_000.png")

        it "recognizes a YAML-less numbered-frame directory by convention, \
           \with the documented defaults" $ do
            entries ← realEntries cargoBuilding
            labelsOf entries `shouldBe` ["construct", "default.png", "demolish"]
            -- construct is YAML-declared (fps 4, loop false); demolish is
            -- convention-recognized with no YAML entry at all.
            fmap pbeFps  (entryNamed "construct" entries) `shouldBe` Just 4.0
            fmap pbeAnimated (entryNamed "demolish" entries) `shouldBe` Just True
            fmap pbeFps  (entryNamed "demolish" entries)
                `shouldBe` Just buildingDefaultFps
            fmap pbeLoop (entryNamed "demolish" entries)
                `shouldBe` Just buildingDefaultLoop

        it "surfaces a non-animation subtree as ordinary statics \
           \(dungeon_1/damaged/)" $ do
            entries ← realEntries dungeonBuilding
            -- The damaged/ folder holds piece sprites, not frames: it is
            -- never ONE animation entry, and none of its textures is lost.
            entryNamed "damaged" entries `shouldBe` Nothing
            all (not ∘ pbeAnimated) entries `shouldBe` True
            let damaged = filter (("damaged/" `isPrefixOf`) ∘ T.unpack ∘ pbeLabel)
                                 entries
            labelsOf damaged `shouldBe`
                [ "damaged/floor.png", "damaged/post.png", "damaged/wall_ne.png"
                , "damaged/wall_nw.png", "damaged/wall_se.png"
                , "damaged/wall_sw.png" ]
            labelsOf entries `shouldBe` sort (labelsOf entries)

    describe "discoverBuildingEntries (synthetic edges)" $ do
        it "orders unpadded frames numerically and keeps a mixed \
           \directory's textures as statics" $
            withBuildingFixture $ \root → do
                entries ← discoverBuildingEntries Map.empty
                                                  (root </> "fixture_building")
                labelsOf entries `shouldBe`
                    [ "mixed/cover.png", "mixed/frame_001.png", "top.png"
                    , "unpadded" ]
                map (T.pack ∘ drop (length (root </> "fixture_building") + 1)
                        ∘ T.unpack)
                    (maybe [] pbeFrames (entryNamed "unpadded" entries))
                    `shouldBe` [ "unpadded/frame_1.png", "unpadded/frame_2.png"
                               , "unpadded/frame_10.png" ]

        -- #1417: a supported extension is a NAME test, so nothing but a
        -- regular file may become a frame.
        it "never makes a frame of a DIRECTORY carrying a supported \
           \extension, and descends into it instead" $
            withNonFileFixture $ \root → do
                let item = root </> "fixture_building"
                entries ← discoverBuildingEntries Map.empty item
                -- dir_only's only .png-named child is a directory, so
                -- dir_only is no animation; neither it nor its .png-named
                -- container is an entry of any kind, while the genuine
                -- texture beneath both still surfaces as a static.
                entryNamed "dir_only" entries `shouldBe` Nothing
                entryNamed "dir_only/frame_001.png" entries `shouldBe` Nothing
                let nested = entryNamed "dir_only/frame_001.png/wall.png" entries
                fmap pbeAnimated nested `shouldBe` Just False
                fmap pbeFrames nested `shouldBe`
                    Just [T.pack (item </> "dir_only" </> "frame_001.png"
                                       </> "wall.png")]
                -- And no entry anywhere claims a directory as a frame.
                dirFrames ← filterM (doesDirectoryExist ∘ T.unpack)
                                    (concatMap pbeFrames entries)
                dirFrames `shouldBe` []

        it "keeps a YAML-matched animation, with only its regular file as a \
           \frame" $
            withNonFileFixture $ \root → do
                let item  = root </> "fixture_building"
                    frame = T.pack (item </> "yaml_matched" </> "frame_001.png")
                    anims = Map.singleton "matched" (anim 3 True [frame])
                entries ← discoverBuildingEntries anims item
                let matched = entryNamed "yaml_matched" entries
                -- The YAML branch bypasses the numbered-name guard, so it
                -- is where a .png-named directory would slip in.
                fmap pbeAnimated matched `shouldBe` Just True
                fmap pbeFps matched `shouldBe` Just 3
                fmap pbeLoop matched `shouldBe` Just True
                fmap pbeFrames matched `shouldBe` Just [frame]

        it "keeps excluding a symlink that points at a real frame file" $
            withNonFileFixture $ \root → do
                let item = root </> "fixture_building"
                entries ← discoverBuildingEntries Map.empty item
                let linked = entryNamed "symlinked" entries
                -- doesFileExist FOLLOWS links, so the symlink test has to
                -- stay independent of it.
                fmap pbeAnimated linked `shouldBe` Just True
                fmap pbeFrames linked `shouldBe`
                    Just [T.pack (item </> "symlinked" </> "frame_001.png")]

        it "never makes a frame of a SPECIAL file carrying a supported \
           \extension" $
            withNonFileFixture $ \root → do
                let item = root </> "fixture_building"
                entries ← discoverBuildingEntries Map.empty item
                let special = entryNamed "special" entries
                -- A FIFO is not a directory, so "exists and is not a
                -- directory" accepts it; only a real type check does not.
                fmap pbeAnimated special `shouldBe` Just True
                fmap pbeFrames special `shouldBe`
                    Just [T.pack (item </> "special" </> "frame_001.png")]

        -- #2199: the same claim through the OTHER producer. The case
        -- above puts its FIFO inside special/, where classifyDir's
        -- regular-file filter is what excludes it; nothing there ever
        -- reaches the static branch, which guarded a loose texture by
        -- its NAME alone.
        it "never makes a STATIC frame of a SPECIAL file carrying a \
           \supported extension, at the building root or in a \
           \non-animation subfolder" $
            withNonFileFixture $ \root → do
                let item = root </> "fixture_building"
                entries ← discoverBuildingEntries Map.empty item
                -- The real files beside each FIFO still surface, as
                -- ordinary statics: this is the static producer, not an
                -- animation, so nothing here may be animated.
                let loose = entryNamed "loose.png" entries
                    nested = entryNamed "plain/wall.png" entries
                fmap pbeAnimated loose `shouldBe` Just False
                fmap pbeFrames loose
                    `shouldBe` Just [T.pack (item </> "loose.png")]
                fmap pbeAnimated nested `shouldBe` Just False
                fmap pbeFrames nested
                    `shouldBe` Just [T.pack (item </> "plain" </> "wall.png")]
                -- And the FIFOs are gone entirely — not merely absent
                -- as labels, but named by no frame path of any entry.
                entryNamed "pipe.png" entries `shouldBe` Nothing
                entryNamed "plain/pipe.png" entries `shouldBe` Nothing
                entryNamed "plain" entries `shouldBe` Nothing
                let fifos = [ item </> "pipe.png"
                            , item </> "plain" </> "pipe.png" ]
                    frames = map T.unpack (concatMap pbeFrames entries)
                filter (`elem` fifos) frames `shouldBe` []

    describe "preview metadata reads BOTH declaration forms (#2080)" $ do
        let decodeMeta bytes = case Yaml.decodeEither' bytes of
                Right f | [d] ← bamfBuildings f → Just (bamdSprite d)
                _ → Nothing

        it "reads the legacy singular `sprite`" $
            decodeMeta (BS.unlines
                [ "buildings:"
                , "  - name: \"x\""
                , "    sprite: \"assets/textures/buildings/x/default.png\""
                ]) `shouldBe`
                    Just (Just "assets/textures/buildings/x/default.png")

        it "reads the canonical `sprites.south`" $
            -- Without this the ladder's sprite rule would silently stop
            -- working for every building an art slice migrates: the
            -- viewer would fall through to default.png, or to whatever
            -- happens to sort first.
            decodeMeta (BS.unlines
                [ "buildings:"
                , "  - name: \"x\""
                , "    sprites:"
                , "      south: \"assets/textures/buildings/x/s.png\""
                , "      west: \"assets/textures/buildings/x/w.png\""
                , "      north: \"assets/textures/buildings/x/n.png\""
                , "      east: \"assets/textures/buildings/x/e.png\""
                ]) `shouldBe` Just (Just "assets/textures/buildings/x/s.png")

        it "yields no sprite, rather than failing, for neither form" $
            decodeMeta (BS.unlines
                [ "buildings:"
                , "  - name: \"x\""
                ]) `shouldBe` Just Nothing

        it "tolerates a malformed `sprites` block instead of failing the \
           \whole preview" $ do
            -- The viewer must never lose a building's animation list
            -- over an unrelated schema question in its static block.
            decodeMeta (BS.unlines
                [ "buildings:"
                , "  - name: \"x\""
                , "    sprites: \"not-a-block.png\""
                ]) `shouldBe` Just Nothing
            decodeMeta (BS.unlines
                [ "buildings:"
                , "  - name: \"x\""
                , "    sprites:"
                , "      west: \"w.png\""
                ]) `shouldBe` Just Nothing

    describe "defaultBuildingEntry (the selection ladder)" $ do
        it "prefers state_animations.built, resolved through the animation's \
           \OWN frame paths" $ do
            let frame = "assets/textures/buildings/x/idle/frame_000.png"
                meta = BuildingPreviewMeta
                    { bpmAnims  = Map.singleton "portal-idle" (anim 8 True [frame])
                    , bpmStates = Map.singleton "built" "portal-idle"
                    , bpmSprite = Just "assets/textures/buildings/x/default.png"
                    }
                entries = [ animated "idle" [frame]
                          , static "default.png"
                                   "assets/textures/buildings/x/default.png" ]
            defaultBuildingEntry meta entries `shouldBe` "idle"

        it "falls back to the building's own sprite when no built state exists" $ do
            let sprite = "assets/textures/buildings/x/default.png"
                meta = BuildingPreviewMeta
                    { bpmAnims  = Map.singleton "x-construct"
                                    (anim 4 False
                                        ["assets/textures/buildings/x/construct/frame_001.png"])
                    , bpmStates = Map.singleton "construction" "x-construct"
                    , bpmSprite = Just sprite
                    }
                entries = [ animated "construct"
                              ["assets/textures/buildings/x/construct/frame_001.png"]
                          , static "default.png" sprite ]
            defaultBuildingEntry meta entries `shouldBe` "default.png"

        it "falls back to default.png when the YAML names an unusable built \
           \animation" $ do
            let meta = BuildingPreviewMeta
                    { bpmAnims  = Map.empty        -- 'built' names a missing anim
                    , bpmStates = Map.singleton "built" "gone"
                    , bpmSprite = Nothing
                    }
                entries = [ static "ceiling.png" "assets/textures/buildings/x/ceiling.png"
                          , static "default.png" "assets/textures/buildings/x/default.png" ]
            defaultBuildingEntry meta entries `shouldBe` "default.png"

        it "falls back to the first entry, and is empty only for an empty folder" $ do
            let entries = [ static "ceiling.png" "assets/textures/buildings/x/ceiling.png"
                          , static "floor.png" "assets/textures/buildings/x/floor.png" ]
            defaultBuildingEntry emptyBuildingPreviewMeta entries
                `shouldBe` "ceiling.png"
            defaultBuildingEntry emptyBuildingPreviewMeta [] `shouldBe` ""

    describe "buildPreviewBuilding (real shipped trees, end to end)" $ do
        it "selects the built animation's DIRECTORY for acolyte_portal" $ do
            got ← buildPreviewBuilding buildingsCategoryRoot portalBuilding
            fmap pbDefault got `shouldBe` Right "idle"
            fmap (labelsOf ∘ pbEntries) got
                `shouldBe` Right ["appear", "default.png", "idle"]

        it "selects the sprite for a building with no built state" $ do
            got ← buildPreviewBuilding buildingsCategoryRoot cargoBuilding
            fmap pbDefault got `shouldBe` Right "default.png"

        it "browses a building with no YAML at all, defaulting to its first \
           \entry" $ do
            got ← buildPreviewBuilding buildingsCategoryRoot dungeonBuilding
            -- dungeon_1 has neither data/buildings/dungeon_1.yaml nor a
            -- default.png, so the ladder falls all the way through.
            meta ← loadBuildingPreviewMeta (T.pack dungeonBuilding)
            meta `shouldBe` emptyBuildingPreviewMeta
            fmap pbDefault got `shouldBe` Right "ceiling.png"
            fmap (pbName) got `shouldBe` Right (T.pack dungeonBuilding)

        it "rejects a bad target with the shared containment error" $ do
            got ← buildPreviewBuilding buildingsCategoryRoot "nosuch_building"
            fmap pbName got `shouldBe` Left ItemDirNotFound
