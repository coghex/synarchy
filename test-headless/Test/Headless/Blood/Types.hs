{-# LANGUAGE UnicodeSyntax, OverloadedStrings #-}
-- | Blood decal model tests (#604): descriptor near-match reuse (and
--   its hard-gated dimensions), bounded FIFO eviction, and decal
--   cleanup-on-eviction — the pure transitions in Blood.Types that the
--   blood.* debug Lua surface wraps. No engine boot needed.
module Test.Headless.Blood.Types (spec) where

import UPrelude
import Test.Hspec
import Blood.Types
import World.Page.Types (WorldPageId(..))

page ∷ WorldPageId
page = WorldPageId "test_world"

baseReq ∷ BloodTextureRequest
baseReq = BloodTextureRequest
    { btrStyle      = StylePool
    , btrWoundKind  = "stab"
    , btrSeverity   = SeverityModerate
    , btrFootprint  = FootprintMedium
    , btrAnisotropy = AnisotropyNone
    , btrEdge       = EdgeModerate
    , btrSeed       = 1
    }

specFor ∷ BloodTextureId → BloodDecalSpec
specFor tid = BloodDecalSpec
    { bspTexture    = tid
    , bspPage       = page
    , bspX          = 10
    , bspY          = 12
    , bspSurfaceZ   = 0
    , bspOffsetX    = 0
    , bspOffsetY    = 0
    , bspRotation   = 0
    , bspScale      = 1
    , bspCreatedAt  = 0
    , bspWoundKind  = "stab"
    , bspSeverity   = SeverityModerate
    , bspSourceUnit = Nothing
    , bspOpacity    = 1
    }

spec ∷ Spec
spec = do
    describe "requestDistance" $ do
        it "is Nothing when style differs (hard gate)" $
            requestDistance baseReq { btrStyle = StyleDrops }
                (descriptorFor baseReq) `shouldBe` Nothing

        it "is Nothing when severity bucket differs (hard gate)" $
            requestDistance baseReq { btrSeverity = SeveritySevere }
                (descriptorFor baseReq) `shouldBe` Nothing

        it "is 0 for an identical request" $
            requestDistance baseReq (descriptorFor baseReq) `shouldBe` Just 0

        it "penalises a wound-kind mismatch even with everything else equal" $
            requestDistance baseReq { btrWoundKind = "slash" }
                (descriptorFor baseReq) `shouldBe` Just 2

        it "accumulates ordinal distance across near-match buckets" $
            requestDistance baseReq { btrFootprint = FootprintLarge }
                (descriptorFor baseReq) `shouldBe` Just 1

    describe "requestTexture / findMatch (near-match reuse)" $ do
        it "an empty pool always creates a new descriptor" $ do
            let (pool, tid, isNew, evicted) =
                    requestTexture baseReq (emptyBloodTexturePool 24)
            isNew `shouldBe` True
            evicted `shouldBe` []
            lookupTexture tid pool `shouldBe` Just (descriptorFor baseReq)

        it "an identical repeat request reuses the same descriptor" $ do
            let (pool1, tid1, _, _) =
                    requestTexture baseReq (emptyBloodTexturePool 24)
                (pool2, tid2, isNew2, evicted2) = requestTexture baseReq pool1
            tid2 `shouldBe` tid1
            isNew2 `shouldBe` False
            evicted2 `shouldBe` []
            length (allTextures pool2) `shouldBe` 1

        it "a request one near-match bucket step away reuses the descriptor" $ do
            let (pool1, tid1, _, _) =
                    requestTexture baseReq (emptyBloodTexturePool 24)
                nearReq = baseReq { btrFootprint = FootprintLarge }
                (pool2, tid2, isNew2, _) = requestTexture nearReq pool1
            tid2 `shouldBe` tid1
            isNew2 `shouldBe` False
            length (allTextures pool2) `shouldBe` 1

        it "a request two near-match buckets away creates a distinct descriptor" $ do
            let (pool1, tid1, _, _) =
                    requestTexture baseReq (emptyBloodTexturePool 24)
                farReq = baseReq { btrAnisotropy = AnisotropyHigh }
                (pool2, tid2, isNew2, _) = requestTexture farReq pool1
            tid2 `shouldNotBe` tid1
            isNew2 `shouldBe` True
            length (allTextures pool2) `shouldBe` 2

        it "a different style always creates a distinct descriptor" $ do
            let (pool1, tid1, _, _) =
                    requestTexture baseReq (emptyBloodTexturePool 24)
                (_, tid2, isNew2, _) =
                    requestTexture baseReq { btrStyle = StyleStreak } pool1
            tid2 `shouldNotBe` tid1
            isNew2 `shouldBe` True

        it "a different severity bucket always creates a distinct descriptor" $ do
            let (pool1, tid1, _, _) =
                    requestTexture baseReq (emptyBloodTexturePool 24)
                (_, tid2, isNew2, _) =
                    requestTexture baseReq { btrSeverity = SeverityCatastrophic } pool1
            tid2 `shouldNotBe` tid1
            isNew2 `shouldBe` True

    describe "FIFO eviction" $ do
        it "stays within cap, evicting the oldest descriptor first" $ do
            let pool0 = emptyBloodTexturePool 2
                reqs  = [ baseReq { btrSeverity = s }
                        | s ← [SeverityMinor, SeverityModerate, SeveritySevere] ]
                go (p, ids, evictedAcc) r =
                    let (p', tid, _, evicted) = requestTexture r p
                    in (p', ids ++ [tid], evictedAcc ++ evicted)
                (finalPool, allIds, allEvicted) = foldl' go (pool0, [], []) reqs
            length (allTextures finalPool) `shouldBe` 2
            case allIds of
                [id1, id2, id3] → do
                    allEvicted `shouldBe` [id1]
                    lookupTexture id1 finalPool `shouldBe` Nothing
                    map btdId (allTextures finalPool) `shouldBe` [id2, id3]
                _ → expectationFailure "expected exactly 3 distinct texture ids"

    describe "spawnDecal / eviction cascade" $ do
        it "evicting a texture removes every decal that referenced it" $ do
            let store0 = emptyBloodStore 2
                sevReq s = baseReq { btrSeverity = s }
                (store1, did1, tid1, _) =
                    spawnDecal (sevReq SeverityMinor) specFor store0
                (store2, did2, tid2, _) =
                    spawnDecal (sevReq SeverityModerate) specFor store1
                (store3, did3, tid3, _) =
                    spawnDecal (sevReq SeveritySevere) specFor store2
                finalIds = map bdeId (allDecals (bstDecals store3))
            tid1 `shouldNotBe` tid2
            tid2 `shouldNotBe` tid3
            -- the oldest texture (tid1) was evicted, taking did1's decal
            -- with it; did2 and did3 survive.
            lookupTexture tid1 (bstPool store3) `shouldBe` Nothing
            finalIds `shouldBe` [did2, did3]
            lookupDecal did1 (bstDecals store3) `shouldBe` Nothing

        it "a reused texture doesn't evict anything and both decals survive" $ do
            let store0 = emptyBloodStore 24
                (store1, did1, tid1, _) = spawnDecal baseReq specFor store0
                (store2, did2, tid2, isNew2) = spawnDecal baseReq specFor store1
            tid2 `shouldBe` tid1
            isNew2 `shouldBe` False
            map bdeId (allDecals (bstDecals store2)) `shouldBe` [did1, did2]
            length (allTextures (bstPool store2)) `shouldBe` 1

    describe "clearBlood" $
        it "leaves both descriptor and decal lists empty, counters reset" $ do
            let store0 = emptyBloodStore 24
                (store1, _, _, _) = spawnDecal baseReq specFor store0
                cleared = clearBlood store1
            allTextures (bstPool cleared) `shouldBe` []
            allDecals (bstDecals cleared) `shouldBe` []
            let (_, tid, isNew, _) = requestTexture baseReq (bstPool cleared)
            tid `shouldBe` BloodTextureId 1
            isNew `shouldBe` True

-- | The descriptor a fresh pool would assign to @req@ — for exercising
-- 'requestDistance' directly without going through 'requestTexture'.
descriptorFor ∷ BloodTextureRequest → BloodTextureDescriptor
descriptorFor req = BloodTextureDescriptor
    { btdId         = BloodTextureId 1
    , btdStyle      = btrStyle req
    , btdWoundKind  = btrWoundKind req
    , btdSeverity   = btrSeverity req
    , btdFootprint  = btrFootprint req
    , btdAnisotropy = btrAnisotropy req
    , btdEdge       = btrEdge req
    , btdSeed       = btrSeed req
    }
