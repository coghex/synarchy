-- | Retained-memory measurement for the gameplay tag registry (#2700,
--   requirement 10). OFF by default: it registers no examples unless
--   @SYNARCHY_TAG_MEMORY=1@ is set, and then needs RTS statistics:
--
--   > SYNARCHY_TAG_MEMORY=1 cabal test synarchy-test-headless \
--   >   --test-options='--match "Gameplay.Tags memory" +RTS -T -RTS'
--
--   Method: force a major GC and read 'GHC.Stats.gcdetails_live_bytes'
--   (all live heap data), build and fully force ('Control.DeepSeq.force')
--   the value under measurement, GC again and read again. The value is
--   still referenced after the second read, so the difference is the
--   heap it retains — both indexes, the target keys, and any tag text
--   not already live before the first read. Inputs built inside the
--   measured action are garbage by the second read and do not count.
--
--   Results print as @TAGMEM@ lines; the committed evidence is
--   @docs\/pr-proofs\/issue-2700-gameplay-tags-memory.md@.
module Test.Headless.Gameplay.TagsMemory (spec) where

import UPrelude
import Test.Hspec
import Control.DeepSeq (NFData, force)
import Control.Exception (evaluate)
import Data.Int (Int64)
import qualified Data.Set as Set
import qualified Data.Text as T
import GHC.Stats (getRTSStats, getRTSStatsEnabled, gc, gcdetails_live_bytes)
import System.Environment (lookupEnv)
import System.Mem (performMajorGC)
import Text.Printf (printf)
import Building.Types (BuildingId(..))
import Gameplay.Tags.Query
import Gameplay.Tags.Registry
import Gameplay.Tags.Types
import Location.Instance (LocationInstanceId(..))
import Unit.Types.Manager (UnitId(..))
import World.Flora.Identity (plantedFloraInstanceId)
import World.Page.Types (WorldPageId(..))

spec ∷ Spec
spec = do
    enabled ← runIO (lookupEnv "SYNARCHY_TAG_MEMORY")
    when (enabled ≡ Just "1") measurementSpec

liveBytes ∷ IO Int64
liveBytes = do
    performMajorGC
    performMajorGC
    fromIntegral . gcdetails_live_bytes . gc <$> getRTSStats

-- | Build and force a value; the live-byte growth it causes. The
--   argument is a function so the build cannot be shared with (or
--   floated out of) another measurement.
retained ∷ NFData α ⇒ (Int → α) → Int → IO (α, Int64)
retained build n = do
    b0 ← liveBytes
    x ← evaluate (force (build n))
    b1 ← liveBytes
    _ ← evaluate x
    pure (x, b1 - b0)

-- | 32 shared tag values, forced before any measurement: the common
--   case, where tag text is owned by scripts/authored data and the
--   registry only points at it.
sharedTags ∷ [GameplayTag]
sharedTags = [ fromMaybe (error "tag") (mkGameplayTag (T.pack ("tag_" <> show i)))
             | i ← [0 ∷ Int .. 31] ]

-- | Two tags per target, from the shared pool.
twoTags ∷ Int → Set.Set GameplayTag
twoTags i = Set.fromList [ sharedTags !! (i `mod` 32), sharedTags !! ((i * 7 + 3) `mod` 32) ]

page ∷ WorldPageId
page = WorldPageId "main_world"

targetFor ∷ TagCategory → Int → TagTarget
targetFor cat i = case cat of
    UnitCategory     → TargetUnit (UnitId (fromIntegral i))
    BuildingCategory → TargetBuilding (BuildingId (fromIntegral i))
    LocationCategory → TargetLocation (LocationTarget page (LocationInstanceId i))
    PlantCategory    → TargetPlant (fromMaybe (error "plant")
                         (mkPlantTarget page (plantedFloraInstanceId (fromIntegral i))))
    ItemCategory     → TargetItem (fromMaybe (error "item") (mkItemTarget (fromIntegral i)))
    TileCategory     → TargetTile (mkTileTarget id page (i `mod` 512) (i `div` 512) 0)

populated ∷ TagCategory → Int → TagRegistry
populated cat n = foldl' (\r i → addTags (targetFor cat i) (twoTags i) r)
                         emptyTagRegistry [1 .. n]

report ∷ String → Int64 → IO ()
report = printf "TAGMEM %-58s %12d bytes\n"

measurementSpec ∷ Spec
measurementSpec = describe "retained bytes" $ do
    it "needs +RTS -T" $
        getRTSStatsEnabled `shouldReturn` True

    it "measures the empty and populated registries by category" $ do
        _ ← evaluate (force sharedTags)
        (_, noise) ← retained (const ()) 0
        report "noise floor (measure nothing)" noise
        (_, empty) ← retained (\_ → emptyTagRegistry) 0
        report "empty registry" empty
        forM_ allTagCategories $ \cat →
            forM_ [1000, 10000, 100000] $ \n → do
                (r, bytes) ← retained (populated cat) n
                let memberships = sum (rcMemberships . snd <$> registryCounts r)
                report (printf "%s n=%d (%d memberships)" (show cat) n memberships) bytes
                printf "TAGMEM   %-56s %12.1f bytes\n"
                    ("per membership" ∷ String)
                    (fromIntegral bytes / fromIntegral memberships ∷ Double)
        -- Tag text the registry alone owns: every assignment builds its
        -- own copy of the tag string, so none of it was live before.
        (_, fresh) ← retained
            (\n → foldl' (\r i → addTags (targetFor UnitCategory i)
                            (Set.fromList
                                [ fromMaybe (error "tag") (mkGameplayTag
                                    (T.pack ("tag_" <> show (i `mod` 32))))
                                , fromMaybe (error "tag") (mkGameplayTag
                                    (T.pack ("tag_" <> show ((i * 7 + 3) `mod` 32)))) ])
                            r)
                         emptyTagRegistry [1 .. n])
            100000
        report "UnitCategory n=100000, registry-owned tag text" fresh

    it "a large untagged universe and repeated queries leave the registry as it was" $ do
        _ ← evaluate (force sharedTags)
        let universeSize = 1000000
            q1 = QComplement (tagged (sharedTags !! 0))
            q2 = QMatch (tagFilter [] [sharedTags !! 1, sharedTags !! 2] [sharedTags !! 3])
        (universe, universeBytes) ← retained
            (\n → unitUniverse (Set.fromList (UnitId . fromIntegral <$> [1 .. n])))
            universeSize
        report "caller-owned universe: 1000000 units" universeBytes
        (r, registryBytes) ← retained (populated UnitCategory) 1000
        report "registry: 1000 tagged of those units" registryBytes
        let countsBefore = registryCounts r
        b0 ← liveBytes
        forM_ [1 .. 100 ∷ Int] $ \_ → do
            _ ← evaluate (queryCount r universe q1)
            _ ← evaluate (queryExists r universe q2)
            pure ()
        b1 ← liveBytes
        report "live growth across 100 count+exists queries" (b1 - b0)
        -- 'Data.Set.difference' shares subtrees with its first
        -- argument, so while the caller still holds the universe the
        -- native complement result adds little of its own.
        (result, resultBytes) ← retained (\_ → evaluateQuery r universe q1) 0
        report (printf "native result set: complement, %d matches" (targetSetsSize result))
               resultBytes
        (listed, listBytes) ← retained (\_ → queryList r universe q1) 0
        report (printf "list result: complement, %d targets" (length listed)) listBytes
        (hits, hitBytes) ← retained (\_ → evaluateQuery r universe (tagged (sharedTags !! 0))) 0
        report (printf "native result set: one tag, %d matches" (targetSetsSize hits)) hitBytes
        registryCounts r `shouldBe` countsBefore
        r `shouldBe` populated UnitCategory 1000
        -- The untagged 99.9% of the universe created no entries.
        sum (rcForwardEntries . snd <$> registryCounts r) `shouldBe` 1000
        _ ← evaluate universe
        pure ()

    it "clearing every tag leaves the empty registry" $ do
        _ ← evaluate (force sharedTags)
        (r, _) ← retained (populated UnitCategory) 100000
        (cleared, bytes) ← retained
            (\n → foldl' (\acc i → clearTags (targetFor UnitCategory i) acc) r [1 .. n])
            100000
        report "UnitCategory n=100000, then every target cleared" bytes
        nullTagRegistry cleared `shouldBe` True
        all ((≡ RegistryCounts 0 0 0) . snd) (registryCounts cleared) `shouldBe` True
        -- Keep the populated registry live across both reads, so its
        -- own release is not subtracted from the cleared result.
        _ ← evaluate r
        pure ()
