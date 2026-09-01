{-# LANGUAGE OverloadedStrings #-}
-- | The shipped Workbench construction sequence (#1850): production-YAML
--   wiring and the exact asset contract that prevents a bad frame path from
--   silently resolving to the unknown-building fallback.
module Test.Headless.Building.WorkbenchConstruction (spec) where

import UPrelude
import Test.Hspec
import Building.Schema
import Data.Foldable (toList)
import qualified Codec.Picture as JP
import qualified Data.ByteString as BS
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import qualified Data.Vector.Storable as SV
import qualified Data.Yaml as Yaml
import Engine.Asset.YamlBuildings
    ( BuildingYamlAnim(..), BuildingYamlDef(..), BuildingYamlFile(..)
    , BuildingYamlTileSize(..) )
import System.Directory (doesFileExist)
import System.FilePath (takeExtension)

workbenchYaml ∷ FilePath
workbenchYaml = "data/buildings/workbench.yaml"

defaultSprite ∷ FilePath
defaultSprite = "assets/textures/buildings/workbench/default.png"

framePaths ∷ [FilePath]
framePaths =
    [ "assets/textures/buildings/workbench/construct/frame_001.png"
    , "assets/textures/buildings/workbench/construct/frame_002.png"
    , "assets/textures/buildings/workbench/construct/frame_003.png"
    , "assets/textures/buildings/workbench/construct/frame_004.png"
    ]

data NormalizedPng = NormalizedPng
    { npWidth  ∷ !Int
    , npHeight ∷ !Int
    , npRgba   ∷ !(SV.Vector Word8)
    } deriving Eq

spec ∷ Spec
spec = it "loads the approved four-frame sequence without a fallback" $ do
    yaml ← decodeWorkbenchYaml
    workbench ← case filter ((≡ "workbench") ∘ bydName) (byfBuildings yaml) of
        [def] → pure def
        defs  → do
            expectationFailure $ "expected exactly one workbench definition, got "
                <> show (length defs)
            pure emptyDef

    bydSpriteAnchor workbench `shouldBe` "diamond_bottom"
    -- The canonical lifecycle role (#2080). A positive-`build_work`
    -- definition declares `construction`; the legacy `appearing` key it
    -- replaced could mean either construction or timed appearance.
    Map.lookup RoleConstruction (bydRoleAnims workbench)
        `shouldBe` Just "workbench-construct"
    Map.lookup RoleAppearance (bydRoleAnims workbench) `shouldBe` Nothing
    Map.lookup RoleBuilt (bydRoleAnims workbench) `shouldBe` Nothing
    bydVisualClass workbench `shouldBe` IndoorFixture

    animation ← case Map.lookup "workbench-construct" (bydAnimations workbench) of
        Just found → pure found
        Nothing → do
            expectationFailure "workbench-construct animation is not declared"
            pure (BuildingYamlAnim 0 True (legacyAssets []))
    byaFps animation `shouldBe` 4
    byaLoop animation `shouldBe` False
    -- The frame paths are still the pre-#2080 singular `default` list —
    -- four-facing art is the art slices' job — so the declaration reads
    -- back as LEGACY, with that one list reaching all four views.
    faSource (byaFrames animation) `shouldBe` AssetLegacy
    map (map T.unpack) (toList (faViews (byaFrames animation)))
        `shouldBe` replicate 4 framePaths

    mapM_ assertRegularPng framePaths
    defaultImage ← decodeNormalizedPng defaultSprite
    frames ← mapM decodeNormalizedPng framePaths
    let defaultDimensions = dimensions defaultImage
    defaultDimensions `shouldBe` (96, 96)
    map dimensions frames `shouldBe` replicate 4 defaultDimensions
    map rgbaLength frames `shouldBe` replicate 4 (96 * 96 * 4)
    pairwiseDistinct frames `shouldBe` True
    case reverse frames of
        finished:_ → unless (finished ≡ defaultImage) $
            expectationFailure "frame_004.png does not match default.png"
        []         → expectationFailure "construction frame inventory is empty"

decodeWorkbenchYaml ∷ IO BuildingYamlFile
decodeWorkbenchYaml = do
    parsed ← Yaml.decodeFileEither workbenchYaml
    case parsed of
        Right yaml → pure yaml
        Left err → do
            expectationFailure $ workbenchYaml <> " failed to parse: " <> show err
            pure (BuildingYamlFile [])

assertRegularPng ∷ FilePath → Expectation
assertRegularPng path = do
    takeExtension path `shouldBe` ".png"
    doesFileExist path `shouldReturn` True

decodeNormalizedPng ∷ FilePath → IO NormalizedPng
decodeNormalizedPng path = do
    bytes ← BS.readFile path
    case JP.decodePng bytes of
        Right dynamic → do
            let image = JP.convertRGBA8 dynamic
            pure NormalizedPng
                { npWidth = JP.imageWidth image
                , npHeight = JP.imageHeight image
                , npRgba = JP.imageData image
                }
        Left err → do
            expectationFailure $ path <> " is not a decodable PNG: " <> err
            pure (NormalizedPng 0 0 SV.empty)

dimensions ∷ NormalizedPng → (Int, Int)
dimensions image = (npWidth image, npHeight image)

rgbaLength ∷ NormalizedPng → Int
rgbaLength = SV.length ∘ npRgba

pairwiseDistinct ∷ Eq a ⇒ [a] → Bool
pairwiseDistinct []       = True
pairwiseDistinct (x : xs) = all (x ≢) xs ∧ pairwiseDistinct xs

-- | Only reached after an earlier expectation failure; it lets the example
--   continue and report the remaining asset diagnostics in the same run.
emptyDef ∷ BuildingYamlDef
emptyDef = BuildingYamlDef
    { bydName = ""
    , bydDisplayName = ""
    , bydCategory = ""
    , bydDescription = ""
    , bydSprites = legacyAssets ""
    , bydVisualClass = IndoorFixture
    , bydTileSize = BuildingYamlTileSize 1 1
    , bydPlacement = ""
    , bydIsStarting = False
    , bydRace = ""
    , bydSpriteAnchor = ""
    , bydBuildWork = 0
    , bydMaterials = Map.empty
    , bydStorageCapacity = 0
    , bydOperations = []
    , bydRoleAnims = Map.empty
    , bydAnimations = Map.empty
    , bydPowerDrain = 0
    , bydPowerNode = Nothing
    }
