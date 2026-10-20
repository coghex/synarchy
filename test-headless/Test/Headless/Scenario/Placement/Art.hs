{-# LANGUAGE Strict #-}
{-# LANGUAGE OverloadedStrings #-}
-- | The art side of structure capture (#2712), without an engine: how a
--   variant's facemap is registered and resolved, which (sprite, facemap)
--   pairs a catalogue can place, and how a placed piece resolves back to
--   the pack and variant that places its pair. A piece stores only
--   palette ids, so the pair they resolve to is its whole art identity.
module Test.Headless.Scenario.Placement.Art (spec) where

import UPrelude
import Test.Hspec
import qualified Data.HashMap.Strict as HM
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import Scenario.Placement.Structure
import Scenario.Types
import Structure.ArtCatalog
import Structure.Facing (WallCaps(..), WallEdge(..))
import Structure.Palette (emptyTexPalette, internPath)
import Structure.Types (StructurePieceData(..), StructureSlot(..))
import Test.Headless.Structure.ConstructionFixture
import World.Arena.Bounds (mkArenaBounds)
import World.Chunk.Types (ChunkCoord(..), LoadedChunk(..))
import World.Generate.Arena (generateArenaChunkIn)

-- | The fixture pack with a @damaged@ variant: its own floor sprite, and
--   a post that INHERITS the default's sprite but overrides its facemap.
variantRegistration ∷ PackArtRegistration
variantRegistration = fixtureRegistration
    { parVariants =
        [ (AppearanceKey (Just damagedVariant) ApFloor, artAsset "fx/damaged_floor.png")
        , (AppearanceKey (Just damagedVariant) ApPost, artAsset "fx/post.png")
        , (AppearanceKey (Just damagedVariant) ApCeiling, artAsset "fx/ceiling.png") ]
    , parVariantFaces =
        [ ((damagedVariant, AkFloor), artAsset "fx/damaged_floorface.png")
        , ((damagedVariant, AkPost), artAsset "fx/damaged_postface.png") ] }

catalogWith ∷ PackArtRegistration → StructureArtCatalog
catalogWith reg = registerOrFail reg emptyStructureArtCatalog

pairsOf ∷ StructureArtCatalog → [((Text, Text), (Text, Maybe Text, ArtKey))]
pairsOf = placedArtPairs

spec ∷ Spec
spec = describe "structure art identity" $ do

    it "resolves a variant to its own sprite and its overridden facemap" $ do
        let cat = catalogWith variantRegistration
        pack ← maybe (fail "the fixture pack is not registered") pure (HM.lookup fixturePack (sacPacks cat))
        fmap (\a → (aaPath (paTexture a), aaPath (paFacemap a)))
             (resolveVariantArt pack damagedVariant AkFloor)
            `shouldBe` Just ("fx/damaged_floor.png", "fx/damaged_floorface.png")

    it "inherits the default's facemap where the variant states none" $ do
        let cat = catalogWith variantRegistration
        pack ← maybe (fail "the fixture pack is not registered") pure (HM.lookup fixturePack (sacPacks cat))
        fmap (aaPath ∘ paFacemap) (resolveVariantArt pack damagedVariant AkCeiling)
            `shouldBe` Just "fx/ceilingface.png"

    it "resolves nothing for a variant that declares no sprite for the slot" $ do
        let cat = catalogWith variantRegistration
        pack ← maybe (fail "the fixture pack is not registered") pure (HM.lookup fixturePack (sacPacks cat))
        resolveVariantArt pack damagedVariant (AkWall WallNE (WallCaps False False)) `shouldBe` Nothing
        resolveVariantArt pack "no_such_variant" AkFloor `shouldBe` Nothing

    it "refuses a variant facemap for a variant the registration declares no art for" $ do
        let reg = fixtureRegistration { parVariantFaces = [(("ghost", AkFloor), artAsset "fx/g.png")] }
        case registerPackArt reg emptyStructureArtCatalog of
            (_, ArtRegistrationRefused f) → show f `shouldSatisfy` T.isInfixOf "variant" ∘ T.pack
            (_, other) → expectationFailure ("not refused: " ⧺ show other)

    it "refuses the same variant facemap slot stated twice" $ do
        let reg = variantRegistration
                { parVariantFaces = parVariantFaces variantRegistration
                    ⧺ [((damagedVariant, AkFloor), artAsset "fx/other.png")] }
        case registerPackArt reg emptyStructureArtCatalog of
            (_, ArtRegistrationRefused _) → pure ()
            (_, other) → expectationFailure ("not refused: " ⧺ show other)

    it "lists every pair the catalogue can place, an inheriting variant sharing the default's" $ do
        let ps = pairsOf (catalogWith variantRegistration)
            claimants p = [ (v, k) | (pr, (_, v, k)) ← ps, pr ≡ p ]
        claimants ("fx/damaged_floor.png", "fx/damaged_floorface.png") `shouldBe` [(Just damagedVariant, AkFloor)]
        -- the damaged ceiling inherits sprite AND facemap: identical rendering, two claimants
        sort' (claimants ("fx/ceiling.png", "fx/ceilingface.png"))
            `shouldBe` sort' [(Nothing, AkCeiling), (Just damagedVariant, AkCeiling)]
        -- the damaged post overrides the facemap: its own pair, so it is told apart
        claimants ("fx/post.png", "fx/damaged_postface.png") `shouldBe` [(Just damagedVariant, AkPost)]
        claimants ("fx/post.png", "fx/postface.png") `shouldBe` [(Nothing, AkPost)]

    it "captures a piece as the default when its rendering is shared, and as the variant when its facemap differs" $ do
        let cat = catalogWith variantRegistration
            place (tex, face) pal0 =
                let (t, pal1) = internPath tex pal0
                    (f, pal2) = internPath face pal1
                in (StructurePieceData t f 1, pal2)
            (ceiling', p1) = place ("fx/ceiling.png", "fx/ceilingface.png") emptyTexPalette
            (post', p2) = place ("fx/post.png", "fx/damaged_postface.png") p1
            (floor', p3) = place ("fx/damaged_floor.png", "fx/damaged_floorface.png") p2
            ab = either (error ∘ show) id (mkArenaBounds 20 10)
            base = generateArenaChunkIn (Just ab) 0 (ChunkCoord 0 0)
            chunk = base { lcStructures = HM.fromList
                [ ((1, 1, slotTagOf SCeiling), ceiling')
                , ((1, 1, slotTagOf SPostN), post')
                , ((2, 1, slotTagOf SFloor), floor') ] }
            cap = captureStructures (StructureCaptureCtx cat p3 (Just ab)) (M.singleton (ChunkCoord 0 0) chunk) mempty
        scDiagnostics cap `shouldBe` []
        map (\p → (spPiece p, spVariant p)) (scPieces cap) `shouldBe`
            [ ("ceiling", Omitted), ("post_n", Authored damagedVariant), ("floor", Authored damagedVariant) ]

    it "reports a pair no registered pack places, and a sprite of the wrong kind for the slot" $ do
        let cat = catalogWith variantRegistration
            (t, p1) = internPath "fx/not_a_sprite.png" emptyTexPalette
            (f, p2) = internPath "fx/floorface.png" p1
            (ct, p3) = internPath "fx/ceiling.png" p2
            (cf, p4) = internPath "fx/ceilingface.png" p3
            ab = either (error ∘ show) id (mkArenaBounds 20 10)
            base = generateArenaChunkIn (Just ab) 0 (ChunkCoord 0 0)
            chunk = base { lcStructures = HM.fromList
                [ ((1, 1, slotTagOf SFloor), StructurePieceData t f 1)
                , ((2, 1, slotTagOf SFloor), StructurePieceData ct cf 1) ] }
            cap = captureStructures (StructureCaptureCtx cat p4 (Just ab)) (M.singleton (ChunkCoord 0 0) chunk) mempty
        scPieces cap `shouldBe` []
        length (scDiagnostics cap) `shouldBe` 2
  where
    sort' = map snd ∘ M.toAscList ∘ M.fromList ∘ map (\x → (show x, x))
