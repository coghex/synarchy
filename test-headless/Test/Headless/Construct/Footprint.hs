{-# LANGUAGE UnicodeSyntax, OverloadedStrings #-}
-- | Committed building blueprint footprint (#807). PR #295 kept exactly
--   one 'CtBuilding' designation at its anchor tile (still true here —
--   requirement 2), but the render pass only ever drew that one anchor
--   tile, so a multi-tile building's committed ghost misrepresented its
--   real footprint. The contract under test:
--   'World.Construct.Types.constructDesignationFootprint' — the SAME
--   pure function 'World.Render.CursorQuads' calls — expands a single
--   building designation into its def's full
--   @[ax..ax+w-1] x [ay..ay+h-1]@ footprint (matching
--   'Building.Types.footprintTiles', the identical convention
--   'Building.Placement.canPlaceAt' and 'building.spawn' use), leaves a
--   structure-piece designation (already one map entry per tile) and a
--   1x1 building untouched, and falls back to the anchor tile alone for
--   a def missing from the supplied map.
module Test.Headless.Construct.Footprint (spec) where

import UPrelude
import Test.Hspec
import qualified Data.HashMap.Strict as HM
import Building.Types (BuildingDef(..), footprintTiles)
import Engine.Asset.Handle (TextureHandle(..))
import World.Construct.Types
    ( ConstructTarget(..), StructurePiece(..), ConstructStatus(..)
    , ConstructDesignation(..), newConstructDesignation
    , constructDesignationFootprint )

-- | Minimal fixture def — only 'bdTileW'/'bdTileH' matter to the
--   footprint contract under test; every other field is a harmless
--   placeholder (mirrors how the python probes inject throwaway temp
--   fixtures rather than touching shipped gameplay content, #807
--   out-of-scope).
fixtureDef ∷ Text → Int → Int → BuildingDef
fixtureDef name w h = BuildingDef
    { bdName            = name
    , bdDisplayName     = name
    , bdCategory        = "Test"
    , bdDescription     = ""
    , bdTexture         = TextureHandle 0
    , bdTileW           = w
    , bdTileH           = h
    , bdPlacement       = "flat_ground"
    , bdIsStarting      = False
    , bdRace            = "human"
    , bdSpriteAnchor    = "diamond_bottom"
    , bdBuildWork       = 0
    , bdMaterials       = HM.empty
    , bdStorageCapacity = 0
    , bdOperations      = []
    , bdAnimations      = HM.empty
    , bdStateAnims      = HM.empty
    , bdPowerDrain      = 0
    }

spec ∷ Spec
spec = describe "Construction blueprint footprint" $ do
    it "expands a multi-tile building designation into its full footprint, staying one pending job" $ do
        let defs = HM.fromList [("cargo_hold_2x3", fixtureDef "cargo_hold_2x3" 2 3)]
            cd   = newConstructDesignation 5 (CtBuilding "cargo_hold_2x3")
            footprint = constructDesignationFootprint defs (100, 200) cd
        cdStatus cd `shouldBe` CsPending
        footprint `shouldBe`
            [ (100, 200), (100, 201), (100, 202)
            , (101, 200), (101, 201), (101, 202)
            ]
        length footprint `shouldBe` 6

    it "leaves an existing 1x1 building blueprint unchanged" $ do
        let defs = HM.fromList [("portal", fixtureDef "portal" 1 1)]
            cd   = newConstructDesignation 0 (CtBuilding "portal")
        constructDesignationFootprint defs (5, 5) cd `shouldBe` [(5, 5)]

    it "leaves a structure-piece designation single-tile regardless of building defs" $ do
        let piece = StructurePiece
                { spPack = "dungeon_1", spKind = "wall", spEdge = Just "ne" }
            cd = newConstructDesignation 0 (CtStructure piece)
        constructDesignationFootprint HM.empty (7, 8) cd `shouldBe` [(7, 8)]

    it "falls back to the anchor tile alone for a def missing from the map" $ do
        let cd = newConstructDesignation 0 (CtBuilding "does_not_exist")
        constructDesignationFootprint HM.empty (1, 1) cd `shouldBe` [(1, 1)]

    it "matches Building.Types.footprintTiles' anchor+tile_size convention directly (#807 req 4)" $ do
        let defs = HM.fromList [("wide", fixtureDef "wide" 3 1)]
            cd   = newConstructDesignation 0 (CtBuilding "wide")
        constructDesignationFootprint defs (9, 4) cd
            `shouldBe` footprintTiles 9 4 3 1
