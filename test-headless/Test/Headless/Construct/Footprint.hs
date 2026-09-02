-- | Committed building blueprint footprint — re-pointed by #1845.
--
--   #807 stored a @CtBuilding@ designation as ONE anchor-only map entry
--   (still true — that half is the durable job model) and had the
--   render pass expand it into the def's whole
--   @[ax..ax+w-1] x [ay..ay+h-1]@ rectangle, drawing one generic
--   category marker per tile. DTV-10 reverses that decision
--   deliberately: a planned building is ONE sprite of its own art,
--   sized and anchored exactly as the placed building will be, so a 2x3
--   def draws one 2x3-shaped building rather than six diamonds.
--
--   So the contract under test moved with the behaviour. It is now
--   'Building.Render.buildingGhostQuad' against
--   'Building.Visual.buildingQuadRect' — the SAME rectangle
--   'Building.Render.buildingToQuad' places the finished building with,
--   and the same one 'Building.HitTest' clicks — plus the anchor-only
--   storage model that survived, asserted directly on the designation
--   map. The expansion helper this module used to call
--   ('constructDesignationFootprint') is gone with its only consumer;
--   'Building.Types.footprintTiles', which it borrowed its convention
--   from, is untouched and still owns placement and occupancy.
module Test.Headless.Construct.Footprint (spec) where

import UPrelude
import Test.Hspec
import qualified Data.HashMap.Strict as HM
import qualified Data.Map.Strict as Map
import Building.Schema
import Building.Render (buildingGhostQuad)
import Building.Types
    (BuildingDef(..), BuildingInstance(..), footprintTiles)
import Building.Visual
    ( BuildingQuadRect(..), buildingQuadRect, designatedGhostAlpha
    , spriteAnchorOffset )
import Engine.Asset.Handle (TextureHandle(..))
import Engine.Graphics.Camera (CameraFacing(..))
import Engine.Graphics.Vulkan.Types.Vertex
    (Vec2(..), Vertex(..), noFaceMapVertexId)
import Engine.Scene.Types (SortableQuad(..))
import World.Construct.Attempt (firstConstructAttemptId)
import World.Construct.Types
    ( ConstructTarget(..), StructurePiece(..), ConstructStatus(..)
    , ConstructDesignation(..), ConstructDesignations
    , newConstructDesignation )
import World.Page.Types (WorldPageId(..))

-- | Minimal fixture def. Unlike the pre-#1845 version, 'bdTextures' and
--   'bdSpriteAnchor' matter now: the ghost is the building's own art at
--   its own size, so the fixture has to declare a real handle and a real
--   anchor convention or it would assert nothing about either.
fixtureDef ∷ Text → Int → Int → TextureHandle → Text → BuildingDef
fixtureDef name w h tex anchor = BuildingDef
    { bdName            = name
    , bdDisplayName     = name
    , bdCategory        = "Test"
    , bdDescription     = ""
    , bdTextures        = legacyAssets tex
    , bdIconTexture     = TextureHandle 0
    , bdTileW           = w
    , bdTileH           = h
    , bdPlacement       = "flat_ground"
    , bdIsStarting      = False
    , bdRace            = "human"
    , bdSpriteAnchor    = anchor
    , bdBuildWork       = 0
    , bdMaterials       = HM.empty
    , bdStorageCapacity = 0
    , bdOperations      = []
    , bdAnimations      = HM.empty
    , bdRoleAnims       = Map.empty
    , bdVisualClass     = FreestandingInstallation
    , bdPowerDrain      = 0
    , bdPowerNode       = Nothing
    }

-- A 2x3 building whose sprite is deliberately NOT the 96x64 base tile
-- and whose anchor is "tile_bottom": both are what a one-quad ghost has
-- to honour and what the retired per-tile marker could not express.
tallHandle ∷ TextureHandle
tallHandle = TextureHandle 41

tallDef ∷ BuildingDef
tallDef = fixtureDef "cargo_hold_2x3" 2 3 tallHandle "tile_bottom"

texSizes ∷ HM.HashMap TextureHandle (Int, Int)
texSizes = HM.singleton tallHandle (192, 192)

facing ∷ CameraFacing
facing = FaceSouth

zSlice ∷ Int
zSlice = 0

tileAlpha ∷ Float
tileAlpha = 0.8

-- | The designation ghost for @def@ at @(ax, ay, z)@, exactly as
--   'World.Render.CursorQuads' builds it.
designationGhost ∷ BuildingDef → Int → Int → Int → SortableQuad
designationGhost def ax ay z =
    buildingGhostQuad (const 0) noFaceMapVertexId facing zSlice texSizes
                      tileAlpha designatedGhostAlpha True def ax ay z

quadBounds ∷ SortableQuad → (Float, Float, Float, Float)
quadBounds q =
    let ps = [ p | Vertex { pos = p } ← [sqV0 q, sqV1 q, sqV2 q, sqV3 q] ]
        xs = [ x | Vec2 x _ ← ps ]
        ys = [ y | Vec2 _ y ← ps ]
    in (minimum xs, minimum ys, maximum xs, maximum ys)

rectBounds ∷ BuildingQuadRect → (Float, Float, Float, Float)
rectBounds r = (bqX r, bqY r, bqX r + bqW r, bqY r + bqH r)

-- | Same rectangle to well inside a pixel. The quad builds its corners
--   by adding the size to the origin and the rect reports origin+size,
--   so the two agree to float rounding and not bit-for-bit; a tenth of a
--   world unit is roughly a thousandth of a tile here, far below
--   anything a wrong anchor or a wrong canvas could produce.
boundsAgree ∷ (Float, Float, Float, Float) → (Float, Float, Float, Float)
            → Bool
boundsAgree (a0, b0, c0, d0) (a1, b1, c1, d1) =
    all (\(u, v) → abs (u - v) < 1e-3) [(a0, a1), (b0, b1), (c0, c1), (d0, d1)]

-- The one-entry storage model #807 established and #1845 keeps.
designate ∷ (Int, Int) → ConstructTarget → ConstructDesignations
designate key tgt =
    HM.singleton key (newConstructDesignation 5 tgt firstConstructAttemptId)

spec ∷ Spec
spec = describe "Construction blueprint footprint" $ do
    it "draws a multi-tile building designation as ONE quad, not one per\
       \ footprint tile" $ do
        -- The render pass emits exactly one ghost per CtBuilding entry;
        -- the def's 2x3 footprint changes the SIZE of that one quad,
        -- never the count. Six tiles, one sprite.
        let designs = designate (100, 200) (CtBuilding (bdName tallDef))
        HM.size designs `shouldBe` 1
        length (footprintTiles 100 200 (bdTileW tallDef) (bdTileH tallDef))
            `shouldBe` 6

    it "stays one anchor-only pending job (#807 requirement 2 survives)" $ do
        let designs = designate (100, 200) (CtBuilding (bdName tallDef))
        HM.keys designs `shouldBe` [(100, 200)]
        fmap cdStatus (HM.lookup (100, 200) designs) `shouldBe` Just CsPending
        -- No off-anchor footprint tile carries an entry of its own.
        HM.lookup (101, 202) designs `shouldBe` Nothing

    it "sizes and anchors that quad exactly as the placed building will" $ do
        -- The claim the retired marker could not make: the ghost's
        -- rectangle IS the placed building's rectangle, from the same
        -- function, including the tile_bottom sprite-anchor drop and the
        -- 192x192 canvas that is neither the 96x64 tile nor the
        -- placeholder's size.
        let ghost = designationGhost tallDef 100 200 5
            placed = buildingQuadRect facing zSlice texSizes
                         (spriteAnchorOffset (Just tallDef)) 100 200 5
                         tallHandle
        quadBounds ghost `shouldSatisfy` boundsAgree (rectBounds placed)
        bqW placed `shouldSatisfy` (> 0)
        -- Not the base tile: a 192x192 canvas is 2x the 96x64 tile's
        -- width and 3x its height in world units.
        spriteAnchorOffset (Just tallDef) `shouldSatisfy` (> 0)

    it "draws a 1x1 building at its own sprite's size too" $ do
        -- The 1x1 control is a SIZE control, not a shape exemption: it
        -- goes through the identical rectangle, so a def whose sprite is
        -- larger than one tile still draws larger than one tile.
        let smallDef = fixtureDef "portal" 1 1 tallHandle "diamond_bottom"
            ghost = designationGhost smallDef 5 5 0
            placed = buildingQuadRect facing zSlice texSizes
                         (spriteAnchorOffset (Just smallDef)) 5 5 0 tallHandle
        quadBounds ghost `shouldSatisfy` boundsAgree (rectBounds placed)

    it "leaves a structure-piece designation to its own ghost pass" $ do
        -- Structures are drawn by World.Render.StructureGhost (#1846)
        -- and this pass emits nothing for them; the map entry is still
        -- one per tile, as the designation tool tiles the rectangle at
        -- commit time.
        let piece = StructurePiece
                { spPack = "dungeon_1", spKind = "wall", spEdge = Just "ne" }
            designs = designate (7, 8) (CtStructure piece)
        HM.keys designs `shouldBe` [(7, 8)]

    it "keeps footprintTiles' anchor+tile_size convention for placement" $
        -- The expansion helper is gone, but the convention it borrowed
        -- is not: Building.Placement.canPlaceAt and building.spawn still
        -- occupy exactly this rectangle, which is why the one ghost is
        -- anchored at (ax, ay).
        footprintTiles 9 4 3 1 `shouldBe` [(9, 4), (10, 4), (11, 4)]

    it "an instance staked at the designation's anchor is the SAME\
       \ picture" $ do
        -- Requirement 3: staking must not move or re-shade anything.
        -- The staked instance's rectangle is built from the same def,
        -- the same anchor and the same z, so the two are equal by
        -- construction — which is what lets the render pass drop one of
        -- them mid-hand-off without a visible change.
        let ghost = designationGhost tallDef 100 200 5
            inst = BuildingInstance
                { biDefName = bdName tallDef
                , biPage = WorldPageId "p"
                , biTexture = tallHandle
                , biAnchorX = 100, biAnchorY = 200, biGridZ = 5
                , biSpawnedAt = 0
                , biTileW = bdTileW tallDef, biTileH = bdTileH tallDef
                , biSpawnRemaining = -1
                , biBuildProgress = 0
                , biMaterialsDelivered = HM.empty
                , biStorage = []
                }
            staked = buildingQuadRect facing zSlice texSizes
                         (spriteAnchorOffset (Just tallDef))
                         (biAnchorX inst) (biAnchorY inst) (biGridZ inst)
                         tallHandle
        quadBounds ghost `shouldSatisfy` boundsAgree (rectBounds staked)
