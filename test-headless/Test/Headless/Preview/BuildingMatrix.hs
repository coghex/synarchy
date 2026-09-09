-- | The buildings viewer's DECLARED lifecycle + facing matrix (BDA-4,
--   #2492): 'Engine.Preview.BuildingMatrix' and the fields
--   'Engine.Preview.Building.buildPreviewBuilding' now resolves beside
--   its unchanged filesystem browser.
--
--   No engine and no GPU — everything here is pure, or reads a synthetic
--   asset tree this spec created and owns. The shipped buildings supply
--   none of the interesting cases (all eight declare legacy @sprite@ and
--   @frames.default@, none declares @destruction@ or canonical
--   @sprites@), so the four-role, mixed-provenance, unresolved-reference
--   and invalid-cell fixtures are synthetic by necessity, exactly as the
--   issue's acceptance states.
--
--   The gameplay-equality group is the load-bearing one: it compares the
--   preview's own frame selection against the REAL
--   'Building.Visual.pickBuildingFrame' and
--   'Building.Destruction.destructionFrame' rather than a restatement of
--   their arithmetic, WITHIN one forced-replay cycle. Past that cycle
--   the two deliberately diverge — gameplay clamps a non-looping clip
--   and expires a destruction effect while the preview replays (#1833) —
--   so an equality asserted at an unbounded clock time would be
--   asserting the wrong contract.
module Test.Headless.Preview.BuildingMatrix (spec) where

import UPrelude
import Test.Hspec
import qualified Data.HashMap.Strict as HM
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Vector as V
import qualified Data.Yaml as Yaml
import Data.List (sort)
import System.Directory
    ( createDirectoryIfMissing, createDirectoryLink, createFileLink )
import System.FilePath ((</>))
import System.Posix.Files (createNamedPipe, stdFileMode)
import Building.Destruction (destructionFrame)
import Engine.Asset.YamlBuildings (BuildingYamlAnim(..))
import Building.Schema
import Building.Types
import Building.Visual (pickBuildingFrame)
import Engine.Asset.Handle (TextureHandle(..))
import Engine.Core.Types
    ( PreviewBuilding(..), PreviewBuildingEntry(..), PreviewDeclaredEntry(..)
    , PreviewFacingCell(..), PreviewFsClass(..) )
import Engine.Graphics.Camera (CameraFacing(..))
import Engine.Preview.Building
    ( buildPreviewBuilding, buildingsCategoryRoot, loadBuildingPreviewMeta )
import Engine.Preview.BuildingMatrix
import Test.Headless.Harness.Isolation (withExclusiveTempDirectory)
import World.Page.Types (WorldPageId(..))

-- * Fixture plumbing

-- | A building tree plus its @data/buildings/\<name\>.yaml@, built
--   inside one exclusively-owned temporary directory.
--
--   'buildPreviewBuilding' reads the YAML through a repo-relative path,
--   so a fixture cannot put its own YAML where that lookup would find
--   it without writing into the checkout. Every YAML-driven case
--   therefore drives 'declaredMatrixOf' and 'resolveDeclaredEntries'
--   directly against a decoded object — which is the same code
--   'buildPreviewBuilding' calls, just entered one step lower — and the
--   end-to-end wiring is proved separately against the real shipped
--   @acolyte_portal@.
withTree ∷ ([(FilePath, [FilePath])] → [FilePath] → FilePath → IO ()) → IO ()
withTree action =
    withExclusiveTempDirectory "synarchy-preview-building-matrix" $ \base → do
        let root = base </> "fixture_building"
            dirs = [ ("appear", ["frame_000.png", "frame_001.png"])
                   , ("idle",   ["frame_000.png", "frame_001.png"])
                   , ("boom",   ["frame_000.png", "frame_001.png"])
                   , ("south",  ["frame_000.png"])
                   , ("west",   ["frame_000.png"])
                   , ("north",  ["frame_000.png"])
                   , ("east",   ["frame_000.png"])
                   ]
            loose = ["default.png"]
        createDirectoryIfMissing True root
        forM_ dirs $ \(d, fs) → do
            createDirectoryIfMissing True (root </> d)
            forM_ fs $ \f → writeFile (root </> d </> f) ""
        forM_ loose $ \f → writeFile (root </> f) ""
        action dirs loose root

-- | Decode a YAML document into the ONE definition object the matrix
--   decoder takes. Deliberately routed through 'Data.Yaml' rather than a
--   hand-built 'Aeson.Object' so a fixture's YAML is exercised by the
--   real parser, the same way a shipped file is.
matrixOf ∷ Text → Text → Maybe BuildingDeclaredMatrix
matrixOf name doc = case Yaml.decodeEither' (TE.encodeUtf8 doc) of
    Left _  → Nothing
    Right o → declaredMatrixOf name o

-- | A canonical four-facing animation block.
canonicalAnim ∷ FilePath → Text → Text
canonicalAnim root nm = T.unlines
    [ "      " <> nm <> ":"
    , "        fps: 8"
    , "        loop: false"
    , "        frames:"
    , "          south: [\"" <> p "south/frame_000.png" <> "\"]"
    , "          west:  [\"" <> p "west/frame_000.png"  <> "\"]"
    , "          north: [\"" <> p "north/frame_000.png" <> "\"]"
    , "          east:  [\"" <> p "east/frame_000.png"  <> "\"]"
    ]
  where p rel = T.pack (root </> rel)

-- | A legacy @frames.default@ animation carrying MORE than one frame —
--   the shape the review correction insists is replicated intact rather
--   than collapsed to a single path.
legacyAnim ∷ FilePath → Text → FilePath → Text
legacyAnim root nm dir = T.unlines
    [ "      " <> nm <> ":"
    , "        fps: 4"
    , "        loop: false"
    , "        frames:"
    , "          default:"
    , "            - \"" <> p (dir </> "frame_000.png") <> "\""
    , "            - \"" <> p (dir </> "frame_001.png") <> "\""
    ]
  where p rel = T.pack (root </> rel)

rowFor ∷ Text → [PreviewDeclaredEntry] → Maybe PreviewDeclaredEntry
rowFor i = listToMaybe ∘ filter ((≡ i) ∘ pdeIdentity)

-- | A fixture whose YAML fails to yield a matrix, or whose expected row
--   is absent, has stopped testing what it claims to. Both say so
--   loudly instead of quietly matching nothing.
requireMatrix ∷ Text → Text → IO BuildingDeclaredMatrix
requireMatrix name doc = case matrixOf name doc of
    Just m  → pure m
    Nothing → fail $ "fixture YAML yielded no declared matrix for "
                  ⧺ T.unpack name

requireRow ∷ Text → [PreviewDeclaredEntry] → IO PreviewDeclaredEntry
requireRow i rows = case rowFor i rows of
    Just r  → pure r
    Nothing → fail $ "no declared row " ⧺ T.unpack i ⧺ "; got "
                  ⧺ show (map pdeIdentity rows)

-- * The four-role mixed-form fixture

-- | Canonical @construction@, legacy multi-frame @appearance@, legacy
--   @built@, canonical @destruction@, plus a legacy @sprite@ — the
--   mixed-provenance building the acceptance calls for, which no shipped
--   definition is.
mixedYaml ∷ FilePath → Text
mixedYaml root = T.unlines
    [ "name: \"fixture_building\""
    , "visual_class: \"gateway\""
    , "sprite: \"" <> T.pack (root </> "default.png") <> "\""
    , "build_work: 12.0"
    , "state_animations:"
    , "  construction: build-anim"
    , "  appearance:   appear-anim"
    , "  built:        idle-anim"
    , "  destruction:  boom-anim"
    , "animations:"
    ] <> canonicalAnim root "build-anim"
      <> legacyAnim root "appear-anim" "appear"
      <> legacyAnim root "idle-anim" "idle"
      <> canonicalAnim root "boom-anim"

-- * Gameplay-equality fixtures

-- | A 'BuildingAnimation' whose four facings carry DISTINCT handles per
--   stage, so an equality that accidentally compared the wrong facing
--   fails instead of passing on a coincidence.
gameAnim ∷ Float → Bool → Int → Text → BuildingAnimation
gameAnim fps loop n tag = BuildingAnimation
    { banFps = fps, banLoop = loop
    , banFrames = canonicalAssets FacingSet
        { fsSouth = viewOf "s", fsWest = viewOf "w"
        , fsNorth = viewOf "n", fsEast = viewOf "e" } }
  where
    viewOf d = V.fromList
        [ TextureHandle (fromIntegral (hash d i)) | i ← [0 .. n - 1] ]
    hash d i = T.length tag * 1000 + T.length d * 100 + i + 1

bareDef ∷ Text → BuildingDef
bareDef name = BuildingDef
    { bdName = name, bdDisplayName = name
    , bdCategory = "Test", bdDescription = ""
    , bdTextures = legacyAssets (TextureHandle 0)
    , bdIconTexture = TextureHandle 0
    , bdTileW = 1, bdTileH = 1, bdPlacement = "flat_ground"
    , bdIsStarting = False, bdRace = "acolyte"
    , bdSpriteAnchor = "diamond_bottom", bdBuildWork = 0
    , bdMaterials = HM.empty, bdStorageCapacity = 0
    , bdOperations = [], bdAnimations = HM.empty
    , bdRoleAnims = Map.empty
    , bdVisualClass = FreestandingInstallation
    , bdPowerDrain = 0, bdPowerNode = Nothing
    }

bareInst ∷ Text → BuildingInstance
bareInst name = BuildingInstance
    { biDefName = name, biPage = WorldPageId "test"
    , biTexture = TextureHandle 0
    , biAnchorX = 0, biAnchorY = 0, biGridZ = 0
    , biSpawnedAt = 0, biTileW = 1, biTileH = 1
    , biSpawnRemaining = 0, biBuildProgress = 0
    , biMaterialsDelivered = HM.empty, biStorage = [] }

-- | The sample times inside ONE forced-replay cycle at which preview and
--   gameplay must agree. Deliberately includes a point inside each
--   frame's own interval rather than only the boundaries, so an
--   off-by-one in either index computation is visible.
cycleSamples ∷ Float → Int → [Double]
cycleSamples fps n =
    [ (fromIntegral i + off) / realToFrac fps
    | i ← [0 .. n - 1], off ← [0.0, 0.5, 0.99 ∷ Double] ]

spec ∷ Spec
spec = do

  describe "the declared matrix decodes through the game's own rules" $ do
    it "exposes all four roles in the fixed order, with each entry's OWN \
       \provenance" $ withTree $ \_ _ root → do
        m ← requireMatrix "fixture_building" (mixedYaml root)
        map drRole (bdmRoles m) `shouldBe`
            [RoleConstruction, RoleAppearance, RoleBuilt, RoleDestruction]
        -- Entry-specific provenance: the sprite is legacy while
        -- `construction` is canonical, in ONE definition. A
        -- building-wide provenance value could not say this.
        faSource (bdmSprite m) `shouldBe` AssetLegacy
        let srcOf r = do
                dr ← listToMaybe (filter ((≡ r) ∘ drRole) (bdmRoles m))
                faSource ∘ byaFrames <$> drAnim dr
        srcOf RoleConstruction `shouldBe` Just AssetCanonical
        srcOf RoleAppearance   `shouldBe` Just AssetLegacy
        srcOf RoleBuilt        `shouldBe` Just AssetLegacy
        srcOf RoleDestruction  `shouldBe` Just AssetCanonical

    it "resolves a legacy `appearing` key by build_work, exactly as \
       \gameplay does" $ withTree $ \_ _ root → do
        let doc work = T.unlines
                [ "name: \"fixture_building\""
                , "visual_class: \"gateway\""
                , "sprite: \"" <> T.pack (root </> "default.png") <> "\""
                , "build_work: " <> work
                , "state_animations:"
                , "  appearing: idle-anim"
                , "animations:"
                ] <> legacyAnim root "idle-anim" "idle"
            rolesOf work = map drRole ∘ bdmRoles
                       <$> matrixOf "fixture_building" (doc work)
        rolesOf "12.0" `shouldBe` Just [RoleConstruction]
        rolesOf "0.0"  `shouldBe` Just [RoleAppearance]

        -- And with `build_work` OMITTED entirely, which is how the
        -- unmigrated definitions this compatibility path exists for are
        -- actually written: the gameplay DEFAULT of 0 applies, so the
        -- key resolves to `appearance`. A preview defaulting it to
        -- anything positive would expose the wrong role for exactly
        -- those definitions.
        let noWork = T.unlines
                [ "name: \"fixture_building\""
                , "visual_class: \"gateway\""
                , "sprite: \"" <> T.pack (root </> "default.png") <> "\""
                , "state_animations:"
                , "  appearing: idle-anim"
                , "animations:"
                ] <> legacyAnim root "idle-anim" "idle"
        (map drRole ∘ bdmRoles <$> matrixOf "fixture_building" noWork)
            `shouldBe` Just [RoleAppearance]
        -- The discriminator is build_work and nothing else: the SAME
        -- animation name lands on two different roles.
        legacyRoleFor 12.0 `shouldBe` RoleConstruction
        legacyRoleFor 0.0  `shouldBe` RoleAppearance

    it "retains an unresolved role rather than dropping it" $
      withTree $ \_ _ root → do
        let doc = T.unlines
                [ "name: \"fixture_building\""
                , "visual_class: \"gateway\""
                , "sprite: \"" <> T.pack (root </> "default.png") <> "\""
                , "state_animations:"
                , "  built: no-such-anim"
                , "animations:"
                ] <> legacyAnim root "idle-anim" "idle"
        m ← requireMatrix "fixture_building" doc
        map drRole (bdmRoles m) `shouldBe` [RoleBuilt]
        map drName (bdmRoles m) `shouldBe` ["no-such-anim"]
        map (isJust ∘ drAnim) (bdmRoles m) `shouldBe` [False]

    it "answers Nothing — never a partial matrix — for every rejection \
       \the game makes" $ withTree $ \_ _ root → do
        let base extra = T.unlines
                ([ "name: \"fixture_building\""
                 , "visual_class: \"gateway\"" ] ⧺ extra ⧺
                 [ "animations:" ]) <> legacyAnim root "idle-anim" "idle"
            sprite = "sprite: \"" <> T.pack (root </> "default.png") <> "\""
            unusable extra =
                matrixOf "fixture_building" (base extra) `shouldSatisfy` isNothing
        -- An unknown lifecycle key.
        unusable [sprite, "state_animations:", "  glowing: idle-anim"]
        -- The legacy key beside the canonical role it resolves to.
        unusable [sprite, "build_work: 0.0", "state_animations:"
                 , "  appearing: idle-anim", "  appearance: idle-anim"]
        -- Both sprite forms at once.
        unusable [ sprite
                 , "sprites:", "  south: a.png", "  west: b.png"
                 , "  north: c.png", "  east: d.png" ]
        -- No sprite at all.
        unusable ["build_work: 0.0"]
        -- A negative build_work, which the game's own domain refuses.
        unusable [sprite, "build_work: -1.0"]

  describe "cells against the asset boundary" $ do
    it "keeps each canonical facing's OWN ordered paths and marks none \
       \legacy" $ withTree $ \_ _ root → do
        m ← requireMatrix "fixture_building" (mixedYaml root)
        rows ← resolveDeclaredEntries root [] m
        c ← requireRow (lifecycleIdentity RoleConstruction) rows
        pdeSource c `shouldBe` "canonical"
        pdeLegacy c `shouldBe` False
        map pfcFacing (pdeCells c) `shouldBe` ["south", "west", "north", "east"]
        map pfcPaths (pdeCells c) `shouldBe`
            [ [T.pack (root </> "south/frame_000.png")]
            , [T.pack (root </> "west/frame_000.png")]
            , [T.pack (root </> "north/frame_000.png")]
            , [T.pack (root </> "east/frame_000.png")] ]
        map pfcMissing (pdeCells c) `shouldBe` [False, False, False, False]
        map pfcLegacy (pdeCells c) `shouldBe` [False, False, False, False]

    it "replicates a legacy animation's COMPLETE ordered frame list into \
       \every facing and flags all four legacy" $ withTree $ \_ _ root → do
        m ← requireMatrix "fixture_building" (mixedYaml root)
        rows ← resolveDeclaredEntries root [] m
        b ← requireRow (lifecycleIdentity RoleBuilt) rows
        let expected = [ T.pack (root </> "idle/frame_000.png")
                       , T.pack (root </> "idle/frame_001.png") ]
        pdeSource b `shouldBe` "legacy"
        pdeLegacy b `shouldBe` True
        -- The whole list, in order, four times — not one path, and not
        -- a truncated or reordered copy.
        map pfcPaths (pdeCells b) `shouldBe` replicate 4 expected
        map pfcLegacy (pdeCells b) `shouldBe` replicate 4 True
        map pfcMissing (pdeCells b) `shouldBe` replicate 4 False

    it "repeats a legacy sprite's one path across all four cells" $
      withTree $ \_ _ root → do
        m ← requireMatrix "fixture_building" (mixedYaml root)
        rows ← resolveDeclaredEntries root [] m
        s ← requireRow spriteIdentity rows
        pdeKind s `shouldBe` "sprite"
        pdeSource s `shouldBe` "legacy"
        map pfcPaths (pdeCells s) `shouldBe`
            replicate 4 [T.pack (root </> "default.png")]
        map pfcLegacy (pdeCells s) `shouldBe` replicate 4 True

    it "reports every invalid cell kind, substituting nothing" $
      withTree $ \_ _ root → do
        -- Five distinct faults, one per facing plus the sprite, so each
        -- reason is proved on its own rather than by one representative.
        createDirectoryIfMissing True (root </> "a_directory.png")
        writeFile (root </> "real.png") ""
        createFileLink (root </> "real.png") (root </> "a_link.png")
        createNamedPipe (root </> "a_fifo.png") stdFileMode
        writeFile (root </> "wrong.txt") ""
        let doc = T.unlines
                [ "name: \"fixture_building\""
                , "visual_class: \"gateway\""
                , "sprite: \"" <> p "wrong.txt" <> "\""
                , "state_animations:"
                , "  built: broken"
                , "animations:"
                , "  broken:"
                , "    fps: 8"
                , "    frames:"
                , "      south: [\"" <> p "absent.png" <> "\"]"
                , "      west:  [\"" <> p "a_directory.png" <> "\"]"
                , "      north: [\"" <> p "a_link.png" <> "\"]"
                , "      east:  [\"" <> p "a_fifo.png" <> "\"]"
                ]
            p rel = T.pack (root </> rel)
        m ← requireMatrix "fixture_building" doc
        rows ← resolveDeclaredEntries root [] m
        b ← requireRow (lifecycleIdentity RoleBuilt) rows
        s ← requireRow spriteIdentity rows
        map pfcMissing (pdeCells b) `shouldBe` replicate 4 True
        map pfcMissingReason (pdeCells b) `shouldBe`
            [Just "absent", Just "directory", Just "symlink", Just "special"]
        map pfcMissingReason (pdeCells s) `shouldBe`
            replicate 4 (Just "unsupported_extension")
        -- Nothing was borrowed from anywhere: every cell still reports
        -- exactly the path that was declared for it.
        map pfcPaths (pdeCells b) `shouldBe`
            [ [p "absent.png"], [p "a_directory.png"]
            , [p "a_link.png"], [p "a_fifo.png"] ]

    it "treats a declared path outside the building's own folder as a \
       \missing cell with its own reason" $ withTree $ \_ _ root → do
        let doc = T.unlines
                [ "name: \"fixture_building\""
                , "visual_class: \"gateway\""
                , "sprite: \"assets/textures/buildings/other/default.png\""
                , "state_animations: {}"
                , "animations: {}"
                ]
        m ← requireMatrix "fixture_building" doc
        rows ← resolveDeclaredEntries root [] m
        s ← requireRow spriteIdentity rows
        map pfcMissingReason (pdeCells s) `shouldBe`
            replicate 4 (Just "outside_root")

    it "names a DANGLING symlink as a symlink, not as absent" $
      withTree $ \_ _ root → do
        -- The ordering trap: an existence predicate FOLLOWS the link, so
        -- a broken one reads as "no such file" and never reaches the
        -- symlink rule. A reviewer needs the real fault named.
        createFileLink (root </> "gone.png") (root </> "dangling.png")
        let doc = T.unlines
                [ "name: \"fixture_building\""
                , "visual_class: \"gateway\""
                , "sprite: \"" <> T.pack (root </> "dangling.png") <> "\""
                , "state_animations: {}"
                , "animations: {}"
                ]
        m ← requireMatrix "fixture_building" doc
        rows ← resolveDeclaredEntries root [] m
        s ← requireRow spriteIdentity rows
        map pfcMissingReason (pdeCells s) `shouldBe`
            replicate 4 (Just "symlink")
        -- And the control: a plainly absent path still reads absent, so
        -- the case above is not just "everything is a symlink now".
        let absentDoc = T.unlines
                [ "name: \"fixture_building\""
                , "visual_class: \"gateway\""
                , "sprite: \"" <> T.pack (root </> "nothing.png") <> "\""
                , "state_animations: {}"
                , "animations: {}"
                ]
        m2 ← requireMatrix "fixture_building" absentDoc
        rows2 ← resolveDeclaredEntries root [] m2
        s2 ← requireRow spriteIdentity rows2
        map pfcMissingReason (pdeCells s2) `shouldBe`
            replicate 4 (Just "absent")

    it "refuses a path that escapes through a symlinked ANCESTOR, not \
       \just a symlinked leaf" $ withTree $ \_ _ root → do
        createDirectoryLink (root </> "idle") (root </> "linked")
        let doc = T.unlines
                [ "name: \"fixture_building\""
                , "visual_class: \"gateway\""
                , "sprite: \"" <> T.pack (root </> "linked/frame_000.png")
                    <> "\""
                , "state_animations: {}"
                , "animations: {}"
                ]
        m ← requireMatrix "fixture_building" doc
        rows ← resolveDeclaredEntries root [] m
        s ← requireRow spriteIdentity rows
        map pfcMissingReason (pdeCells s) `shouldBe`
            replicate 4 (Just "symlink")

    it "gives an unresolved lifecycle row four diagnostic cells, never \
       \another animation's art" $ withTree $ \_ _ root → do
        let doc = T.unlines
                [ "name: \"fixture_building\""
                , "visual_class: \"gateway\""
                , "sprite: \"" <> T.pack (root </> "default.png") <> "\""
                , "state_animations:"
                , "  built: no-such-anim"
                , "animations:"
                ] <> legacyAnim root "idle-anim" "idle"
        m ← requireMatrix "fixture_building" doc
        rows ← resolveDeclaredEntries root [] m
        b ← requireRow (lifecycleIdentity RoleBuilt) rows
        pdeResolved b `shouldBe` False
        pdeAnimName b `shouldBe` Just "no-such-anim"
        map pfcPaths (pdeCells b) `shouldBe` replicate 4 []
        map pfcMissingReason (pdeCells b) `shouldBe`
            replicate 4 (Just "unresolved")

  describe "identities, ordering and the compatibility projections" $ do
    it "orders lifecycle rows by role, then the sprite row, with \
       \distinct identities" $ withTree $ \_ _ root → do
        m ← requireMatrix "fixture_building" (mixedYaml root)
        rows ← resolveDeclaredEntries root [] m
        map pdeIdentity rows `shouldBe`
            [ "lifecycle:construction", "lifecycle:appearance"
            , "lifecycle:built", "lifecycle:destruction", "sprite" ]
        -- No declared identity can ever collide with a raw one.
        filesystemIdentity "sprite" `shouldBe` "filesystem:sprite"
        map pdeIdentity rows `shouldSatisfy`
            all (\i → i ≢ filesystemIdentity "idle")

    it "projects a lifecycle row to a raw ANIMATED entry and the sprite \
       \row to a raw STATIC one, independently of facing" $
      withTree $ \_ _ root → do
        m ← requireMatrix "fixture_building" (mixedYaml root)
        let raw =
              [ PreviewBuildingEntry "default.png" False 8 False
                  [T.pack (root </> "default.png")]
              , PreviewBuildingEntry "idle" True 4 False
                  [ T.pack (root </> "idle/frame_000.png")
                  , T.pack (root </> "idle/frame_001.png") ]
              , PreviewBuildingEntry "north" True 8 False
                  [T.pack (root </> "north/frame_000.png")]
              ]
        rows ← resolveDeclaredEntries root raw m
        (pdeProjected =≪ rowFor (lifecycleIdentity RoleBuilt) rows)
            `shouldBe` Just "idle"
        (pdeProjected =≪ rowFor spriteIdentity rows)
            `shouldBe` Just "default.png"
        -- The CANONICAL construction row declares south/west/north/east
        -- art; only `north` exists as a raw directory, and the row must
        -- still project onto it — a south-only match would make the
        -- projection facing-dependent.
        (pdeProjected =≪ rowFor (lifecycleIdentity RoleConstruction) rows)
            `shouldBe` Just "north"
        -- Nothing overlaps `appearance`'s appear/ frames here.
        (pdeProjected =≪ rowFor (lifecycleIdentity RoleAppearance) rows)
            `shouldBe` Nothing

    it "classifies each raw entry by overlap without filtering any" $
      withTree $ \_ _ root → do
        m ← requireMatrix "fixture_building" (mixedYaml root)
        let raw =
              [ PreviewBuildingEntry "default.png" False 8 False
                  [T.pack (root </> "default.png")]
              , PreviewBuildingEntry "idle" True 4 False
                  [ T.pack (root </> "idle/frame_000.png")
                  , T.pack (root </> "idle/frame_001.png") ]
              , PreviewBuildingEntry "stray.png" False 8 False
                  [T.pack (root </> "stray.png")]
              ]
        rows ← resolveDeclaredEntries root raw m
        let classes = classifyFsEntries rows raw
        map pfcsLabel classes `shouldBe` ["default.png", "idle", "stray.png"]
        map pfcsIdentity classes `shouldBe`
            [ "filesystem:default.png", "filesystem:idle"
            , "filesystem:stray.png" ]
        map pfcsDeclared classes `shouldBe`
            [ ["sprite"], ["lifecycle:built"], [] ]
        map pfcsUndeclared classes `shouldBe` [False, False, True]

    it "prefers built, then sprite, then the unchanged raw ladder" $
      withTree $ \_ _ root → do
        m ← requireMatrix "fixture_building" (mixedYaml root)
        let raw = [PreviewBuildingEntry "default.png" False 8 False ["x"]]
        rows ← resolveDeclaredEntries root raw m
        defaultSelectionIdentity rows "default.png" raw
            `shouldBe` "lifecycle:built"
        defaultSelectionIdentity (filter ((≢ "lifecycle:built") ∘ pdeIdentity) rows)
            "default.png" raw `shouldBe` "sprite"
        defaultSelectionIdentity [] "default.png" raw
            `shouldBe` "filesystem:default.png"
        defaultSelectionIdentity [] "" [] `shouldBe` ""

  describe "the raw browser is untouched by a rejected declaration" $ do
    it "browses the real acolyte_portal with its declared matrix and \
       \unchanged raw entries" $ do
        result ← buildPreviewBuilding buildingsCategoryRoot "acolyte_portal"
        case result of
            Left err → expectationFailure (show err)
            Right b  → do
                -- The pre-#2492 fields, verbatim.
                pbDefault b `shouldBe` "idle"
                sort (map pbeLabel (pbEntries b)) `shouldBe`
                    sort (map pbeLabel (pbEntries b))
                pbEntries b `shouldSatisfy` not ∘ null
                -- The declared matrix: this building declares
                -- `appearance` and `built` only, both legacy, plus a
                -- legacy sprite. An UNDECLARED role is simply absent —
                -- never reported as missing.
                map pdeIdentity (pbDeclared b) `shouldBe`
                    [ "lifecycle:appearance", "lifecycle:built", "sprite" ]
                map pdeSource (pbDeclared b) `shouldBe`
                    ["legacy", "legacy", "legacy"]
                map pdeResolved (pbDeclared b) `shouldBe` [True, True, True]
                -- Its art really is on disk, so nothing is diagnostic.
                concatMap (map pfcMissing ∘ pdeCells) (pbDeclared b)
                    `shouldSatisfy` all not
                pbDefaultSelection b `shouldBe` "lifecycle:built"
                (pdeProjected =≪ rowFor "lifecycle:built" (pbDeclared b))
                    `shouldBe` Just "idle"

    it "exposes no declared rows for a building with no YAML, and \
       \browses it exactly as before" $ do
        meta ← loadBuildingPreviewMeta "dungeon_1"
        result ← buildPreviewBuilding buildingsCategoryRoot "dungeon_1"
        case result of
            Left err → expectationFailure (show err)
            Right b  → do
                pbDeclared b `shouldBe` []
                pbEntries b `shouldSatisfy` not ∘ null
                pbDefaultSelection b
                    `shouldBe` filesystemIdentity (pbDefault b)
                -- Every raw row is undeclared, and every one is still
                -- present: classification never removes a row.
                map pfcsLabel (pbFsClasses b)
                    `shouldBe` map pbeLabel (pbEntries b)
                map pfcsUndeclared (pbFsClasses b)
                    `shouldSatisfy` and
        meta `shouldSatisfy` const True

  describe "preview/gameplay frame equality within one replay cycle" $ do
    it "construction: the preview's cycle phase maps onto build progress" $ do
        let n = 4
            fps = 8 ∷ Float
            def = (bareDef "b")
                { bdBuildWork = 12
                , bdRoleAnims = Map.fromList [(RoleConstruction, "c")]
                , bdAnimations = HM.fromList
                    [("c", gameAnim fps False n "c")] }
            inst = bareInst "b"
        forM_ (cycleSamples fps n) $ \t →
          forM_ canonicalFacings $ \f → do
            let phase = cyclePhaseAt fps n t
                -- The preview's own index at t.
                idx = previewFrameIndexAt fps n t
                -- Gameplay at the build progress that phase names.
                inst' = inst { biBuildProgress =
                                 realToFrac phase * bdBuildWork def }
                want = facingAsset f (banFrames (bdAnimations def HM.! "c"))
                         V.! idx
            pickBuildingFrame f 0 inst' def `shouldBe` want

    it "appearance and built: cycle-local elapsed time, honouring the \
       \declared loop or clamp" $ do
        forM_ [(RoleAppearance, "a"), (RoleBuilt, "b")] $ \(role, tag) →
          forM_ [True, False] $ \loops → do
            let n = 3
                fps = 8 ∷ Float
                def = (bareDef "x")
                    { bdBuildWork = 0
                    , bdRoleAnims = Map.fromList
                        [ (RoleAppearance, tag) | role ≡ RoleAppearance ]
                        <> Map.fromList
                        [ (RoleBuilt, tag) | role ≡ RoleBuilt ]
                    , bdAnimations = HM.fromList
                        [(tag, gameAnim fps loops n tag)] }
                inst = bareInst "x"
            forM_ (cycleSamples fps n) $ \t →
              forM_ canonicalFacings $ \f → do
                let idx = previewFrameIndexAt fps n t
                    want = facingAsset f
                             (banFrames (bdAnimations def HM.! tag)) V.! idx
                -- Inside the first cycle, `raw < n`, so gameplay's loop
                -- and clamp branches coincide with the preview's wrap —
                -- which is exactly the window the issue restricts the
                -- equality to.
                pickBuildingFrame f t inst def `shouldBe` want

    it "destruction: Building.Destruction, not Building.Visual" $ do
        let n = 5
            fps = 6 ∷ Float
            clip = DestructionClip
                { dcFps = fps, dcFrameCount = n
                , dcFrames = banFrames (gameAnim fps False n "d") }
            eff = DestructionEffect
                { deBuildingId = BuildingId 1, deDefName = "x"
                , dePage = WorldPageId "test"
                , deAnchorX = 0, deAnchorY = 0, deGridZ = 0
                , deAnchorOffset = 0, deClip = clip, deStartedAt = 0 }
        forM_ (cycleSamples fps n) $ \t →
          forM_ canonicalFacings $ \f → do
            let idx = previewFrameIndexAt fps n t
                want = facingAsset f (dcFrames clip) V.! idx
            destructionFrame f t eff `shouldBe` Just want
        -- And the divergence the issue names is REAL, so the
        -- cycle-bounded equality above is not vacuous: past the clip the
        -- effect has expired while the preview has replayed to frame 0.
        destructionFrame FaceSouth (fromIntegral n / realToFrac fps) eff
            `shouldBe` Nothing
        previewFrameIndexAt fps n (fromIntegral n / realToFrac fps)
            `shouldBe` 0
