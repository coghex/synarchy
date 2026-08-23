{-# LANGUAGE Strict #-}
-- | Pure tests proving final-age mountain soil shed (#225 / PR #279) is
--   REDISTRIBUTED to the adjacent lower/gentler receiving terrain instead
--   of simply deleted (#812). 'World.Geology.Erosion.Math' computes purely
--   per-tile from a 1-ring neighbour stencil; a receiver recognises a
--   shedding donor neighbour from its OWN stencil alone (see the
--   'shedCredit' comment in Math.hs), so this spec exercises 'applyErosion'
--   directly with synthetic neighbour elevations, same as
--   'Test.Headless.WorldGen.SoilShed'.
module Test.Headless.WorldGen.SoilRedistribution (spec) where

import UPrelude
import qualified Data.HashMap.Strict as HM
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU
import Test.Hspec
import World.Generate.Strata (buildStrataCache, buildColumnStrata)
import World.Geology (GeoModification(..))
import World.Geology.Erosion (applyErosion)
import World.Geology.Timeline.BBox (noBBox)
import World.Geology.Timeline.Types
    ( ErosionParams(..), defaultErosionParams, GeoPeriod(..), GeoScale(..)
    , GeoTimeline(..), emptyTimeline )
import World.Material
    ( MaterialId(..), MaterialRegistry, MaterialProps(..)
    , defaultMaterialProps, emptyMaterialRegistry, registerMaterial
    , matGranite, matGlacier )
import World.Scale (computeWorldScale)

-- | Temperate last-age params: 'erosionSediment' will pick a soil
--   (material id ≥ 50) for a kept cap.
lastAgeParams ∷ ErosionParams
lastAgeParams = defaultErosionParams { epIsLastAge = True }

-- granite source rock, mid hardness, a single geological "age" period.
granite ∷ Word8
granite = 1

hardness ∷ Float
hardness = 0.5

-- | Indestructible surface material hardness — what @glacier@ (id 250)
--   and @mantle@ (id 251) declare in @data/materials/special.yaml@. A
--   tile of such a material takes 'applyErosion''s early-out: it never
--   erodes and never sheds a soil cap (#1591).
indestructible ∷ Float
indestructible = 1.0

-- | Run the final-age erosion at @elev@ with the four cardinal
--   neighbours at the given elevations, every neighbour being the same
--   erodible rock as the centre.
run ∷ Int → (Int, Int, Int, Int) → GeoModification
run elev nbrs = runWith elev nbrs
    (hardness, hardness, hardness, hardness)

-- | As 'run', but with the four cardinal neighbours' own material
--   hardness given explicitly, paired N/S/E/W with the elevations.
runWith ∷ Int → (Int, Int, Int, Int) → (Float, Float, Float, Float)
        → GeoModification
runWith elev nbrs nbrHard =
    applyErosion lastAgeParams 128 1 1.0 granite hardness elev nbrs nbrHard

spec ∷ Spec
spec = do
    mathSpec
    strataSpec

mathSpec ∷ Spec
mathSpec = describe "final-age soil shed redistribution (#812)" $ do
    it "a steep mountain face still exposes bare rock (donor, #225 unchanged)" $ do
        -- One coherent terrain profile: a donor at 54 dropping 4 tiles
        -- to a receiver at 50 on its S side (mirrored by the receiver
        -- test below, whose N neighbour is this same donor).
        let donor = run 54 (54, 50, 54, 54)
        gmMaterialOverride donor `shouldBe` Nothing
        gmIntrusionDepth donor `shouldBe` 0

    it "the adjacent lower receiver gains soil the donor no longer caps" $ do
        -- Same terrain profile as above: receiver at 50 with the steep
        -- donor (elev 54, a downhill drop of 4 ≥ soilShedRelief 3 from
        -- the donor's own perspective) immediately to its N.
        let receiverWithDonor = run 50 (54, 50, 50, 50)
            -- Otherwise-IDENTICAL fixture where the uphill neighbour is
            -- only 2 tiles up — below the shed threshold, so it is NOT
            -- a donor. Under the pre-#812 implementation these two
            -- fixtures produce IDENTICAL soil depth: 'maxDrop'/'reliefNorm'
            -- only read DOWNHILL neighbours, and an uphill neighbour
            -- never raises them regardless of how high it stands. This
            -- comparison is exactly the case the old per-cell
            -- implementation cannot distinguish, and must fail.
            receiverNoDonor = run 50 (52, 50, 50, 50)
        gmIntrusionDepth receiverWithDonor `shouldSatisfy`
            (> gmIntrusionDepth receiverNoDonor)
        gmMaterialOverride receiverWithDonor `shouldSatisfy` isJust

    it "an unaffected flat receiver's soil is unchanged (no phantom credit)" $ do
        -- All neighbours flat / below the shed threshold: no donor
        -- anywhere nearby, so behaviour must match pre-#812 exactly.
        let flat = run 50 (50, 50, 50, 50)
        gmIntrusionDepth flat `shouldBe` 2  -- max 1 (round (4*0.5*(1-0)))

    it "redistribution stays bounded when boxed in by donors on every side" $ do
        let single = run 50 (54, 50, 50, 50)
            boxed  = run 50 (54, 54, 54, 54)
        gmIntrusionDepth boxed `shouldSatisfy` (> gmIntrusionDepth single)
        -- Bounded: capped credit (soilShedRelief = 3), not one tile per
        -- donor neighbour, so 4 donor neighbours don't tower unbounded.
        gmIntrusionDepth boxed `shouldSatisfy` (≤ gmIntrusionDepth single + 2)

    it "an indestructible uphill neighbour credits nothing (#1591)" $ do
        -- The SAME receiver fixture as the paired case above (elev 50,
        -- one neighbour at 54 — a downhill drop of 4 ≥ soilShedRelief 3
        -- from that neighbour's own perspective), differing ONLY in the
        -- neighbour's material. A glacier / mantle neighbour takes
        -- 'applyErosion''s hardness ≥ 1.0 early-out when it is itself the
        -- centre tile: it never exposes rock and sheds no cap, so it must
        -- credit nothing. The erodible twin still credits one.
        let glacierDonor = runWith 50 (54, 50, 50, 50)
                (indestructible, hardness, hardness, hardness)
            rockDonor = runWith 50 (54, 50, 50, 50)
                (hardness, hardness, hardness, hardness)
        -- Exact values, not a comparison: the flat baseline is
        -- max 1 (round (4 * 0.5 * (1 - 0))) = 2, plus one credit for a
        -- single qualifying donor. This pins the >= 1.0 boundary.
        gmIntrusionDepth glacierDonor `shouldBe` 2
        gmIntrusionDepth rockDonor `shouldBe` 3
        -- The receiver still keeps a soil cap either way — only the
        -- shed credit is withheld.
        gmMaterialOverride glacierDonor `shouldSatisfy` isJust

    it "an indestructible neighbour credits nothing at any relief (#1591)" $ do
        -- Relief far above the threshold changes nothing: eligibility is
        -- a material property, not a steepness one.
        let sheerGlacier = runWith 50 (90, 50, 50, 50)
                (indestructible, hardness, hardness, hardness)
            flat = run 50 (50, 50, 50, 50)
        gmIntrusionDepth sheerGlacier `shouldBe` gmIntrusionDepth flat

    it "mixed donors credit only the erodible ones (#1591)" $ do
        -- Boxed in on all four sides, two donors indestructible: only
        -- the two erodible faces contribute, so the credit is 2 rather
        -- than the capped 3 an all-rock ring earns.
        let mixed = runWith 50 (54, 54, 54, 54)
                (indestructible, indestructible, hardness, hardness)
            allRock = run 50 (54, 54, 54, 54)
        gmIntrusionDepth mixed `shouldBe` 4        -- baseline 2 + 2 donors
        gmIntrusionDepth allRock `shouldBe` 5      -- baseline 2 + capped 3

    it "a donor itself never receives credit even beside a taller neighbour" $ do
        -- This tile (elev 54) itself sheds toward its S neighbour (50,
        -- drop 4) AND has an even taller neighbour to its N (60, drop
        -- from ITS perspective would be negative — 54 is downhill of
        -- 60) — but exposeRock zeroes soil regardless of any upslope
        -- donor, so #225's donor-side result is untouched by #812.
        let donorBesideTallerPeak = run 54 (60, 50, 54, 54)
        gmMaterialOverride donorBesideTallerPeak `shouldBe` Nothing
        gmIntrusionDepth donorBesideTallerPeak `shouldBe` 0

-- * The strata consumer
--
--   'World.Generate.Strata.buildStrataCache' is the PRODUCTION consumer
--   that records the shed credit as a column's @gmIntrusionDepth@, so
--   the gate above is only useful if it survives that hop. These cases
--   drive the real cache + column builder — the same pair
--   'World.Generate.Chunk.Columns.buildChunkColumns' calls — with the
--   two donor materials, and read the resulting column back.

-- | Registry holding just the two materials these cases name: erodible
--   granite and the indestructible glacier that ships at hardness 1.0.
strataRegistry ∷ MaterialRegistry
strataRegistry =
    registerMaterial (unMaterialId matGlacier)
        defaultMaterialProps { mpName = "glacier"
                             , mpHardness = indestructible }
  $ registerMaterial (unMaterialId matGranite)
        defaultMaterialProps { mpName = "granite", mpHardness = hardness }
        emptyMaterialRegistry

-- | A single event-free final-age period, so the column's only writer
--   is the erosion pass under test.
strataTimeline ∷ GeoTimeline
strataTimeline = emptyTimeline
    { gtWorldSize = strataWorldSize
    , gtPeriods =
        [ GeoPeriod
            { gpName            = "final age"
            , gpScale           = Age
            , gpDuration        = 1
            , gpDate            = 0
            , gpEvents          = []
            , gpErosion         = lastAgeParams
            , gpRegionalErosion = HM.empty
            , gpTaggedEvents    = []
            , gpExplodedEvents  = V.empty
            , gpPeriodBBox      = noBBox
            }
        ]
    }

strataWorldSize ∷ Int
strataWorldSize = 128

-- | Build the granite column at elevation 50 whose N neighbour stands at
--   54 — a downhill drop of 4 from that neighbour's own perspective, so
--   it qualifies as a donor on relief alone — with that neighbour's
--   material given explicitly. Returns the column's materials from
--   @z = 44@ up to the surface at @z = 50@.
strataColumn ∷ MaterialId → VU.Vector MaterialId
strataColumn donorMat =
    let base  = (50, matGranite)
        cache = buildStrataCache strataTimeline strataWorldSize
                    (computeWorldScale strataWorldSize) 0 0
                    strataRegistry base
                    (54, 50, 50, 50)
                    (donorMat, matGranite, matGranite, matGranite)
    in buildColumnStrata cache base 44 50

strataSpec ∷ Spec
strataSpec = describe "final-age shed credit through the strata column (#1591)" $ do
    it "an erodible donor deepens the receiver's soil cap by one tile" $ do
        -- shedCredit 1 on top of the flat baseline 2 ⇒ soil occupies
        -- z 48..50, so z 48 is soil rather than the base granite.
        let col = strataColumn matGranite
        col VU.! (50 - 44) `shouldSatisfy` (≢ matGranite)
        col VU.! (49 - 44) `shouldSatisfy` (≢ matGranite)
        col VU.! (48 - 44) `shouldSatisfy` (≢ matGranite)
        col VU.! (47 - 44) `shouldBe` matGranite

    it "an indestructible donor leaves the cap at the flat baseline" $ do
        -- No credit ⇒ soil occupies only z 49..50, leaving z 48 granite.
        let col = strataColumn matGlacier
        col VU.! (50 - 44) `shouldSatisfy` (≢ matGranite)
        col VU.! (49 - 44) `shouldSatisfy` (≢ matGranite)
        col VU.! (48 - 44) `shouldBe` matGranite

    it "the two columns differ in exactly the credited tile" $ do
        let rock    = strataColumn matGranite
            glacier = strataColumn matGlacier
            differing =
                [ z | z ← [44 .. 50]
                    , rock VU.! (z - 44) ≢ glacier VU.! (z - 44) ]
        differing `shouldBe` [48]
