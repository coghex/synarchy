{-# LANGUAGE ScopedTypeVariables #-}
-- | #2693 (WML-7): the versioned paged map artifact — manifest and page
--   formats, the PNG codec gate, publication through the generated-world
--   library, and fine pages as discardable cache files.
--
--   Format and storage only: no engine, no GPU, and no world
--   generation. Mandatory pages are synthetic RGBA8 fixtures (the format
--   does not care where bytes came from), except the root-reassembly
--   example, which generates real pages for a small world from an
--   injected cell source. Every library example runs in its own
--   exclusive scratch root.
--
--   Run just this gate: @cabal test synarchy-test-headless
--   --test-options='--match "paged map artifact"'@.
module Test.Headless.World.PagedMapArtifact (spec) where

import UPrelude
import Test.Hspec
import Control.Exception (Exception, throwIO, try)
import qualified Codec.Picture as JP
import qualified Crypto.Hash.SHA256 as SHA256
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.List as L
import qualified Data.Serialize.Put as P
import qualified Data.Text as T
import Numeric (showHex)
import System.Directory (createDirectoryIfMissing, createDirectoryLink, listDirectory, removeFile, renameDirectory)
import System.FilePath ((</>))

import World.GeneratedLibrary
import World.GeneratedLibrary.Layout (decodeEntryRecord, encodeEntryRecord, entryRecordFileName)
import World.Map.ImagePlan (MapImagePlan(..))
import World.Page.GeneratedId (GeneratedWorldId)
import World.ZoomMap.Pyramid
import World.ZoomMap.PagedArtifact
import World.ZoomMap.Types (zoomTileSize)
import Test.Headless.Harness.GeneratedIds (fixtureGeneratedWorldId)
import Test.Headless.Harness.Isolation (withExclusiveTempDirectory)

-- ---------------------------------------------------------------------
-- Fixtures
-- ---------------------------------------------------------------------

gidA, gidB ∷ GeneratedWorldId
gidA = fixtureGeneratedWorldId "paged-map-artifact-a"
gidB = fixtureGeneratedWorldId "paged-map-artifact-b"

compat ∷ MapCompatibility
compat = currentMapCompatibility

pageBytes ∷ Int
pageBytes = mapPageEdge * mapPageEdge * 4

-- | A deterministic page image whose every texel depends on the seed,
--   the key and the position, with genuinely transparent texels (and
--   non-zero colour under zero alpha) so a lossy or premultiplying codec
--   would show.
fixturePage ∷ Int → MapPageKey → BS.ByteString
fixturePage seed (MapPageKey l pu pv) = fst $ BS.unfoldrN pageBytes step 0
  where
    step i = let px = i `div` 4
                 x = px `mod` mapPageEdge
                 y = px `div` mapPageEdge
                 v = case i `mod` 4 of
                        0 → x * 3 + seed * 17 + l * 7
                        1 → y * 5 + pu * 11
                        2 → (x + y) `div` 3 + pv * 13
                        _ → if (x + y) `mod` 17 ≡ 0 then 0 else 90 + (x `div` 7) `mod` 160
             in Just (fromIntegral (v `mod` 256), i + 1)

inventoryOf ∷ HasCallStack ⇒ Int → IO MapPyramidInventory
inventoryOf n = either (\r → expectationFailure (show r) ≫ error "unreachable") pure
                       (mapPyramidInventory n)

mandatoryFixture ∷ Int → MapPyramidInventory → [(MapPageKey, BS.ByteString)]
mandatoryFixture seed inv = [ (k, fixturePage seed k) | k ← mandatoryPageKeys inv ]

-- | The page files and manifest bytes for a fixture, built with the
--   production encoders.
encodedFixture ∷ HasCallStack ⇒ GeneratedWorldId → Int → Int
               → IO ([(MapPageKey, BS.ByteString)], BS.ByteString)
encodedFixture gid seed worldSize = do
    inv ← inventoryOf worldSize
    files ← forM (mandatoryFixture seed inv) $ \(k, rgba) → do
        png ← ok (encodePagePng "fixture" rgba)
        pure (k, encodeMapPageFile (MapPageBinding gid compat k) png)
    m ← ok (buildMapManifest gid compat worldSize files)
    bytes ← ok (encodeMapManifest m)
    pure (files, bytes)

ok ∷ HasCallStack ⇒ Show ε ⇒ Either ε α → IO α
ok = either (\e → expectationFailure ("expected success, got " ⧺ show e) ≫ error "unreachable") pure

-- ---------------------------------------------------------------------
-- Byte surgery on well-formed files (each rewrite recomputes the trailer
-- so the rule under test — not the checksum — is what refuses)
-- ---------------------------------------------------------------------

retrail ∷ BS.ByteString → BS.ByteString
retrail bytes = let body = BS.take (BS.length bytes - 32) bytes in body <> SHA256.hash body

replaceAt ∷ Int → BS.ByteString → BS.ByteString → BS.ByteString
replaceAt off new bytes = BS.take off bytes <> new <> BS.drop (off + BS.length new) bytes

word32 ∷ Word32 → BS.ByteString
word32 = P.runPut ∘ P.putWord32be

word64 ∷ Word64 → BS.ByteString
word64 = P.runPut ∘ P.putWord64be

readWord32 ∷ Int → BS.ByteString → Int
readWord32 off bytes = foldl' (\a k → a * 256 + fromIntegral (BS.index bytes (off + k))) 0 [0 .. 3]

-- | Manifest layout: 20-byte framing, world 16, compatibility 16, world
--   size 4, encoding 1, seven geometry words, the level table, then the
--   page table. Every fixture name is 34 bytes, so an entry is 88.
levelCountOffset ∷ Int
levelCountOffset = 20 + 16 + 16 + 4 + 1 + 28

pageCountOffset ∷ BS.ByteString → Int
pageCountOffset m = levelCountOffset + 4 + 20 * readWord32 levelCountOffset m

entryOffset ∷ BS.ByteString → Int → Int
entryOffset m i = pageCountOffset m + 4 + 88 * i

entrySize ∷ Int
entrySize = 88

-- | Point manifest entry @i@ at different page-file bytes.
redirectEntry ∷ Int → BS.ByteString → BS.ByteString → BS.ByteString
redirectEntry i file m =
    retrail $ replaceAt (entryOffset m i + 12 + 2 + 34) (word64 (fromIntegral (BS.length file)))
            $ replaceAt (entryOffset m i + 12 + 2 + 34 + 8) (SHA256.hash file) m

-- | Remove manifest entry @i@, fixing the count and the declared length.
dropEntry ∷ Int → BS.ByteString → BS.ByteString
dropEntry i m =
    let off = entryOffset m i
        count = readWord32 (pageCountOffset m) m
        cut = BS.take off m <> BS.drop (off + entrySize) m
    in retrail $ replaceAt 12 (word64 (fromIntegral (BS.length cut)))
               $ replaceAt (pageCountOffset m) (word32 (fromIntegral (count - 1))) cut

-- | Swap manifest entries @i@ and @i + 1@.
swapEntries ∷ Int → BS.ByteString → BS.ByteString
swapEntries i m =
    let off = entryOffset m i
        a = BS.take entrySize (BS.drop off m)
        b = BS.take entrySize (BS.drop (off + entrySize) m)
    in retrail (replaceAt off (b <> a) m)

pngOf ∷ BS.ByteString → BS.ByteString
pngOf file = BS.take (BS.length file - mapPageHeaderBytes - mapPageTrailerBytes)
                     (BS.drop mapPageHeaderBytes file)

-- | Destroy the zlib header of a PNG's first IDAT so the native inflate
--   must fail, leaving the signature and IHDR intact.
corruptIdat ∷ BS.ByteString → BS.ByteString
corruptIdat png =
    let (before, rest) = BS.breakSubstring "IDAT" png
        at = BS.length before + 4
    in if BS.null rest then error "no IDAT" else replaceAt at (BS.pack [0xFF, 0xFF, 0xFF, 0xFF]) png

-- ---------------------------------------------------------------------
-- Library scratch
-- ---------------------------------------------------------------------

withLibrary ∷ (FilePath → Library → IO α) → IO α
withLibrary action =
    withExclusiveTempDirectory "synarchy-paged-map-artifact-spec" $ \root → do
        createDirectoryIfMissing True (root </> "saves")
        let cfg = LibraryConfig
                { lcRoot = root </> libraryDirectory
                , lcSavesDirectory = root </> "saves"
                , lcLockWaitMicros = 2_000_000 }
        lib ← openLibrary cfg ≫= ok
        action root lib

publishFixture ∷ HasCallStack ⇒ Library → GeneratedWorldId → Int → Int → IO PublishReport
publishFixture lib gid seed worldSize = do
    inv ← inventoryOf worldSize
    publishMapArtifact lib gid compat worldSize (mandatoryFixture seed inv) ≫= ok

data Interrupt = Interrupt deriving Show
instance Exception Interrupt

interrupting ∷ IO (Either MapArtifactRefusal PublishReport) → IO ()
interrupting action = do
    r ← try action
    case r of
        Left Interrupt → pure ()
        Right outcome → expectationFailure ("expected the hook to interrupt, got " ⧺ show outcome)

hex ∷ BS.ByteString → String
hex = concatMap byte ∘ BS.unpack
  where byte b = let s = showHex b "" in if length s ≡ 1 then '0' : s else s

-- ---------------------------------------------------------------------
-- Spec
-- ---------------------------------------------------------------------

spec ∷ Spec
spec = do
    coverageSpec
    formatSpec
    refusalSpec
    librarySpec
    fineSpec

coverageSpec ∷ Spec
coverageSpec = describe "mandatory coverage" $ do
    forM_ [8, 72, 136, 1024, 8192] $ \n →
        it ("is exactly cutoff through root, page-complete, for world size " ⧺ show n) $ do
            inv ← inventoryOf n
            let geom = mpiGeometry inv
                keys = mandatoryPageKeys inv
                expected =
                    [ MapPageKey l pu pv
                    | l ← [mpiCoarseCutoffLevel inv .. mpiRootLevel inv]
                    , pu ← [0 .. mapLevelPagesU geom l - 1]
                    , pv ← [0 .. mapLevelPagesV geom l - 1] ]
            keys `shouldBe` expected
            keys `shouldBe` L.sort keys
            length keys `shouldBe` sum [ mplPageCount l | l ← mapPyramidMandatoryLevels inv ]
            length keys `shouldSatisfy` (≤ mapManifestMaxPages)
            forM_ keys $ \k → checkMapPageKey geom k `shouldBe` Right ()

    it "a synthetic 8192 manifest validates without generating a world" $ do
        -- 8192's finest level is 131072 × 262144 texels; nothing here
        -- touches it — the manifest is built from key arithmetic and
        -- one constant page image.
        inv ← inventoryOf 8192
        mpiRootLevel inv `shouldBe` 7
        mpiCoarseCutoffLevel inv `shouldBe` 6
        png ← ok (encodePagePng "constant" (BS.replicate pageBytes 7))
        let files = [ (k, encodeMapPageFile (MapPageBinding gidA compat k) png)
                    | k ← mandatoryPageKeys inv ]
        m ← ok (buildMapManifest gidA compat 8192 files)
        bytes ← ok (encodeMapManifest m)
        decodeMapManifest gidA compat bytes `shouldBe` Right m
        BS.length bytes `shouldSatisfy` (≤ mapManifestMaxBytes)
        length (mmPages m) `shouldBe` 40

    it "the root is recovered from the root-level pages, gutters stripped and edges cropped" $ do
        -- World 72: root level 1 is 576 × 1152, so its 2 × 3 pages are
        -- partial on both axes.
        inv ← inventoryOf 72
        let root = mpiRootLevel inv
            geom = mpiGeometry inv
            source = MapCellSource (Right ∘ map cellTile)
        root `shouldBe` 1
        pages ← forM [ MapPageKey root pu pv
                     | pu ← [0 .. mapLevelPagesU geom root - 1]
                     , pv ← [0 .. mapLevelPagesV geom root - 1] ] $ \k →
            (k,) ⊚ ok (mapPageImage inv source k)
        files ← forM pages $ \(k, rgba) → do
            png ← ok (encodePagePng "root" rgba)
            pure (k, encodeMapPageFile (MapPageBinding gidA compat k) png)
        -- A manifest needs all mandatory pages; the cutoff level's are
        -- fixtures, which reassembly never reads.
        let others = [ (k, placeholder) | k ← mandatoryPageKeys inv, mpkLevel k ≢ root ]
        m ← ok (buildMapManifest gidA compat 72 (files ⧺ others))
        decoded ← forM files $ \(k, f) → do
            png ← ok (decodeMapPageFile (MapPageBinding gidA compat k) f)
            (k,) ⊚ (decodePagePng "root" png ≫= ok)
        assembled ← ok (reassembleMapRoot m decoded)
        whole ← ok (mapLevelRaster inv source root)
        assembled `shouldBe` mapRasterBytes whole
        BS.length assembled `shouldBe` mipByteCount (mpiRootPlan inv)
        reassembleMapRoot m (drop 1 decoded) `shouldSatisfy` either isMissing (const False)
  where
    placeholder = BS.replicate (mapPageHeaderBytes + 40 + mapPageTrailerBytes) 0
    isMissing (MapArtifactMissingRequired _) = True
    isMissing _ = False
    cellTile (MapCell cu cv) = BS.pack $ concat
        [ [ fromIntegral (cu * 7 + px), fromIntegral (cv * 5 + py)
          , fromIntegral (cu + cv), if (px + py) `mod` 5 ≡ 0 then 0 else 255 ]
        | py ← [0 .. zoomTileSize - 1], px ← [0 .. zoomTileSize - 1] ]

formatSpec ∷ Spec
formatSpec = describe "serialization" $ do
    it "page files round-trip to the exact decoded RGBA bytes, gutters and transparency included" $ do
        inv ← inventoryOf 72
        forM_ (take 3 (mandatoryFixture 1 inv)) $ \(k, rgba) → do
            png ← ok (encodePagePng "page" rgba)
            let file = encodeMapPageFile (MapPageBinding gidA compat k) png
            png' ← ok (decodeMapPageFile (MapPageBinding gidA compat k) file)
            png' `shouldBe` png
            decodePagePng "page" png' `shouldReturn` Right rgba

    it "identical inputs serialise to identical bytes" $ do
        (files1, m1) ← encodedFixture gidA 1 72
        (files2, m2) ← encodedFixture gidA 1 72
        m1 `shouldBe` m2
        files1 `shouldBe` files2

    it "manifest order is canonical whatever order the pages are supplied in" $ do
        (files, bytes) ← encodedFixture gidA 1 72
        let shuffled = reverse (drop 5 files) ⧺ take 5 files
        m ← ok (buildMapManifest gidA compat 72 shuffled)
        encodeMapManifest m `shouldBe` Right bytes
        map mmpKey (mmPages m) `shouldBe` L.sort (map fst files)

    it "golden: the format bytes are pinned" $ do
        -- A literal, not a recomputation: a change to the layout, the
        -- codec output or the ordering must show up here and bump
        -- 'mapArtifactFormatVersion' deliberately.
        (files, bytes) ← encodedFixture gidA 1 8
        map (hex ∘ SHA256.hash ∘ snd) files `shouldBe` [goldenPageDigest]
        hex (SHA256.hash bytes) `shouldBe` goldenManifestDigest

goldenPageDigest, goldenManifestDigest ∷ String
goldenPageDigest = "5f5671f925b6ed541443d58011c7ac1e731a040581e17008d296c807b8eb56af"
goldenManifestDigest = "f7bd1f139a289db78095742cae92b2f523098f73f0b75f61bd471e36ee673b27"

refusalSpec ∷ Spec
refusalSpec = describe "refusals" $ do
    it "a manifest for another world, or from another producer, is refused by name" $ do
        (_, bytes) ← encodedFixture gidA 1 72
        decodeMapManifest gidB compat bytes `shouldSatisfy`
            either (≡ MapArtifactWrongWorld gidB gidA) (const False)
        forM_ mapCompatFields $ \(field, get') → do
            let bumped = case field of
                    CompatGenerator → compat { mcGenerator = get' compat + 1 }
                    CompatContent   → compat { mcContent = get' compat + 1 }
                    CompatPalette   → compat { mcPalette = get' compat + 1 }
                    CompatMapSchema → compat { mcMapSchema = get' compat + 1 }
            decodeMapManifest gidA bumped bytes `shouldSatisfy`
                either (≡ MapArtifactIncompatible field (get' compat + 1) (get' compat)) (const False)

    it "an unknown version, a foreign file, truncation and checksum damage are distinct" $ do
        (files, bytes) ← encodedFixture gidA 1 72
        (key, file, key1) ← case files of
            (k, f) : (k1, _) : _ → pure (k, f, k1)
            _ → expectationFailure "fixture" ≫ error "unreachable"
        let binding = MapPageBinding gidA compat key
        decodeMapManifest gidA compat (retrail (replaceAt 8 (word32 2) bytes))
            `shouldSatisfy` isLeftWith (\case MapArtifactUnknownVersion _ 2 → True; _ → False)
        decodeMapPageFile binding (retrail (replaceAt 8 (word32 9) file))
            `shouldSatisfy` isLeftWith (\case MapArtifactUnknownVersion _ 9 → True; _ → False)
        decodeMapManifest gidA compat ("NOTAMAP!" <> BS.drop 8 bytes)
            `shouldSatisfy` isLeftWith (\case MapArtifactNotRecognised _ → True; _ → False)
        decodeMapManifest gidA compat (BS.take (BS.length bytes - 10) bytes)
            `shouldSatisfy` isLeftWith (\case MapArtifactTruncated {} → True; _ → False)
        decodeMapPageFile binding (BS.take (BS.length file - 1) file)
            `shouldSatisfy` isLeftWith (\case MapArtifactTruncated {} → True; _ → False)
        decodeMapPageFile binding (file <> "x")
            `shouldSatisfy` isLeftWith (\case MapArtifactLengthMismatch {} → True; _ → False)
        let flip' off b = replaceAt off (BS.pack [BS.index b off `xor` 1]) b
        decodeMapManifest gidA compat (flip' 40 bytes)
            `shouldSatisfy` isLeftWith (\case MapArtifactChecksumMismatch _ → True; _ → False)
        decodeMapPageFile binding (flip' 500 file)
            `shouldSatisfy` isLeftWith (\case MapArtifactChecksumMismatch _ → True; _ → False)
        decodeMapPageFile (binding { mpbKey = key1 }) file
            `shouldSatisfy` isLeftWith (\case MapArtifactPageBinding {} → True; _ → False)
        decodeMapPageFile (binding { mpbWorld = gidB }) file
            `shouldSatisfy` isLeftWith (≡ MapArtifactWrongWorld gidB gidA)

    it "duplicate, unordered, invalid and misnamed keys, and missing required entries, are refused" $ do
        (_, bytes) ← encodedFixture gidA 1 72
        let e0 = entryOffset bytes 0
            e1 = entryOffset bytes 1
            key0 = BS.take 12 (BS.drop e0 bytes)
        decodeMapManifest gidA compat (retrail (replaceAt e1 key0 bytes))
            `shouldSatisfy` isLeftWith (\case MapArtifactDuplicateKey _ → True
                                              MapArtifactConflictingName {} → True
                                              _ → False)
        decodeMapManifest gidA compat (swapEntries 0 bytes)
            `shouldSatisfy` isLeftWith (\case MapArtifactUnorderedKeys _ _ → True; _ → False)
        decodeMapManifest gidA compat (retrail (replaceAt e0 (word32 40) bytes))
            `shouldSatisfy` isLeftWith (\case MapArtifactInvalidKey _ _ → True; _ → False)
        decodeMapManifest gidA compat (retrail (replaceAt (e0 + 14) "x" bytes))
            `shouldSatisfy` isLeftWith (\case MapArtifactConflictingName {} → True; _ → False)
        decodeMapManifest gidA compat (dropEntry 3 bytes)
            `shouldSatisfy` isLeftWith (\case MapArtifactMissingRequired (RequiredPage _) → True; _ → False)
        -- A duplicate that keeps the canonical name is a duplicate.
        let dup = retrail (replaceAt e1 (BS.take entrySize (BS.drop e0 bytes)) bytes)
        decodeMapManifest gidA compat dup
            `shouldSatisfy` isLeftWith (\case MapArtifactDuplicateKey _ → True; _ → False)
        -- Recorded geometry that the recorded world size does not derive.
        decodeMapManifest gidA compat (retrail (replaceAt 77 (word32 999) bytes))
            `shouldSatisfy` isLeftWith (\case MapArtifactGeometryMismatch _ → True; _ → False)

    it "oversized declarations are refused before any allocation or pixel decode" $ do
        (files, bytes) ← encodedFixture gidA 1 72
        (key, file) ← case files of
            kf : _ → pure kf
            [] → expectationFailure "fixture" ≫ error "unreachable"
        let binding = MapPageBinding gidA compat key
        -- A page file declaring a 4 GiB PNG: refused from its 73-byte
        -- header, whatever follows.
        decodeMapPageFile binding (replaceAt 65 (word64 (4 * 1024 * 1024 * 1024)) file)
            `shouldSatisfy` isLeftWith (\case MapArtifactOversized {} → True; _ → False)
        decodeMapManifest gidA compat (replaceAt 12 (word64 (1024 * 1024 * 1024)) bytes)
            `shouldSatisfy` isLeftWith (\case MapArtifactOversized {} → True; _ → False)
        decodeMapManifest gidA compat
            (retrail (replaceAt (pageCountOffset bytes) (word32 maxBound) bytes))
            `shouldSatisfy` isLeftWith (\case MapArtifactMalformed {} → True; _ → False)
        -- A PNG whose IHDR claims 100000 × 100000: refused on its header.
        let png = pngOf file
            huge = replaceAt 16 (word32 100000 <> word32 100000) png
        checkPagePngHeader "huge" huge `shouldBe`
            Left (MapArtifactPngDimensions "huge" (mapPageEdge, mapPageEdge) (100000, 100000))
        decodePagePng "huge" huge `shouldReturn`
            Left (MapArtifactPngDimensions "huge" (mapPageEdge, mapPageEdge) (100000, 100000))

    it "PNG dimension, pixel-type and native-decode failures are distinct refusals" $ do
        let rgb = BL.toStrict $ JP.encodePng
                (JP.generateImage (\_ _ → JP.PixelRGB8 1 2 3) mapPageEdge mapPageEdge)
            narrow = BL.toStrict $ JP.encodePng
                (JP.generateImage (\_ _ → JP.PixelRGBA8 1 2 3 4) 512 mapPageEdge)
        decodePagePng "rgb" rgb `shouldReturn`
            Left (MapArtifactPngHeader "rgb" "colour type is 2, not 6")
        decodePagePng "narrow" narrow `shouldReturn`
            Left (MapArtifactPngDimensions "narrow" (mapPageEdge, mapPageEdge) (512, mapPageEdge))
        png ← ok (encodePagePng "p" (fixturePage 1 (MapPageKey 0 0 0)))
        r ← decodePagePng "idat" (corruptIdat png)
        r `shouldSatisfy` isLeftWith (\case MapArtifactDecodeFailure {} → True; _ → False)
        -- The public decoder bounds the encoded size itself.
        let padded = png <> BS.replicate (mapPagePngMaxBytes + 1 - BS.length png) 0
        decodePagePng "padded" padded `shouldReturn`
            Left (MapArtifactOversized "padded" (toInteger mapPagePngMaxBytes)
                                                (toInteger mapPagePngMaxBytes + 1))

librarySpec ∷ Spec
librarySpec = describe "library publication" $ do
    it "publishes, looks up and reads back every mandatory page exactly" $
        withLibrary $ \_ lib → do
            report ← publishFixture lib gidA 1 72
            prOutcome report `shouldBe` PublishedNew
            Right (Just entry) ← lookupEntry lib gidA
            leStatus entry `shouldBe` EntryCommitted
            opened ← openMapArtifact lib gidA compat ≫= ok
            inv ← inventoryOf 72
            forM_ (mandatoryFixture 1 inv) $ \(k, rgba) →
                readMandatoryPage opened k `shouldReturn` Right rgba
            -- Republishing identical content is byte-identical, hence
            -- unchanged.
            again ← publishFixture lib gidA 1 72
            prOutcome again `shouldBe` PublishedUnchanged

    it "an absent entry, a wrong world and an incompatible producer are refused distinctly" $
        withLibrary $ \_ lib → do
            _ ← publishFixture lib gidA 1 8
            r1 ← openMapArtifact lib gidB compat
            r1 `shouldBe` Left (MapArtifactAbsent gidB)
            r2 ← openMapArtifact lib gidA compat { mcPalette = 99 }
            r2 `shouldSatisfy` isLeftWith (\case MapArtifactIncompatible CompatPalette 99 1 → True; _ → False)

    it "a missing or damaged required file makes the mandatory artifact unusable" $
        withLibrary $ \_ lib → do
            _ ← publishFixture lib gidA 1 72
            let dir = entryDirectory lib gidA
                page = T.unpack (mapPageFileName (MapPageKey 0 1 2))
            original ← BS.readFile (dir </> page)
            BS.writeFile (dir </> page) (BS.take 1000 original)
            openMapArtifact lib gidA compat ≫= (`shouldSatisfy` isLeftWith
                (\case MapArtifactTruncated {} → True; _ → False))
            BS.writeFile (dir </> page) (original <> "grown")
            openMapArtifact lib gidA compat ≫= (`shouldSatisfy` isLeftWith
                (\case MapArtifactLengthMismatch {} → True; _ → False))
            BS.writeFile (dir </> page) (replaceAt 900 "!" original)
            openMapArtifact lib gidA compat ≫= (`shouldSatisfy` isLeftWith
                (\case MapArtifactChecksumMismatch _ → True; _ → False))
            removeFile (dir </> page)
            openMapArtifact lib gidA compat `shouldReturn`
                Left (MapArtifactMissingRequired (RequiredPage (MapPageKey 0 1 2)))
            removeFile (dir </> T.unpack mapManifestFileName)
            openMapArtifact lib gidA compat `shouldReturn`
                Left (MapArtifactMissingRequired RequiredManifest)

    it "a symlinked entry directory is refused before any payload is read through it" $
        withLibrary $ \root lib → do
            _ ← publishFixture lib gidA 1 8
            let dir = entryDirectory lib gidA
                elsewhere = root </> "elsewhere"
            renameDirectory dir elsewhere
            createDirectoryLink elsewhere dir
            -- Without the directory check this read the intact payload
            -- through the link and failed only at the record check.
            openMapArtifact lib gidA compat ≫= (`shouldSatisfy` isLeftWith
                (\case MapArtifactIO path _ → path ≡ dir; _ → False))
            -- For a fine-page read the same refusal is a cache miss.
            readFinePage dir gidA compat 136 (MapPageKey 0 0 0) ≫= (`shouldSatisfy` \case
                Right (FinePageMiss (FinePageInvalid (MapArtifactIO path _))) → path ≡ dir
                _ → False)

    it "a page read after opening re-checks the entry directory rather than following a new symlink" $
        withLibrary $ \root lib → do
            _ ← publishFixture lib gidA 1 8
            opened ← openMapArtifact lib gidA compat ≫= ok
            let dir = entryDirectory lib gidA
                elsewhere = root </> "elsewhere"
            renameDirectory dir elsewhere
            createDirectoryLink elsewhere dir
            readMandatoryPage opened (MapPageKey 0 0 0) ≫= (`shouldSatisfy` isLeftWith
                (\case MapArtifactIO path _ → path ≡ dir; _ → False))

    it "a corrupt deflate stream behind a recomputed checksum is refused by the native decoder" $
        withLibrary $ \_ lib → do
            _ ← publishFixture lib gidA 1 72
            let dir = entryDirectory lib gidA
                key = MapPageKey 0 1 2
                page = dir </> T.unpack (mapPageFileName key)
                manifestPath = dir </> T.unpack mapManifestFileName
            file ← BS.readFile page
            manifest ← BS.readFile manifestPath
            let damaged = encodeMapPageFile (MapPageBinding gidA compat key) (corruptIdat (pngOf file))
            ix ← entryIndex manifest key
            BS.writeFile page damaged
            BS.writeFile manifestPath (redirectEntry ix damaged manifest)
            openMapArtifact lib gidA compat ≫= (`shouldSatisfy` isLeftWith
                (\case MapArtifactDecodeFailure {} → True; _ → False))

    it "the library record must agree with the manifest about every map file" $
        withLibrary $ \_ lib → do
            _ ← publishFixture lib gidA 1 72
            let dir = entryDirectory lib gidA
                key = MapPageKey 0 1 2
                page = dir </> T.unpack (mapPageFileName key)
                manifestPath = dir </> T.unpack mapManifestFileName
                recordPath = dir </> entryRecordFileName
            original ← BS.readFile recordPath
            rec ← ok (decodeEntryRecord original)
            -- Same sizes, so the library's cheap check still calls the
            -- entry complete; only the map's cross-check can see it.
            let tamper d
                    | pdName d ≡ mapPageFileName key = d { pdDigest = SHA256.hash "another page" }
                    | otherwise = d
            BS.writeFile recordPath (encodeEntryRecord rec { erFiles = map tamper (erFiles rec) })
            openMapArtifact lib gidA compat ≫= (`shouldSatisfy` isLeftWith
                (\case MapArtifactSubstituted name _ → name ≡ mapPageFileName key; _ → False))
            BS.writeFile recordPath original
            _ ← openMapArtifact lib gidA compat ≫= ok
            -- A valid page of another size swapped in behind a manifest
            -- rewritten to match: the library no longer calls it complete.
            manifest ← BS.readFile manifestPath
            png ← ok (encodePagePng "other" (fixturePage 2 key))
            let other = encodeMapPageFile (MapPageBinding gidA compat key) png
            ix ← entryIndex manifest key
            BS.writeFile page other
            BS.writeFile manifestPath (redirectEntry ix other manifest)
            openMapArtifact lib gidA compat ≫= (`shouldSatisfy` isLeftWith
                (\case MapArtifactEntryIncomplete _ → True
                       MapArtifactSubstituted _ _ → True
                       _ → False))

    it "an incomplete or invalid candidate is refused and the existing entry is untouched" $
        withLibrary $ \_ lib → do
            _ ← publishFixture lib gidA 1 72
            inv ← inventoryOf 72
            let pages = mandatoryFixture 2 inv
            publishMapArtifact lib gidA compat 72 (drop 1 pages) ≫= (`shouldSatisfy` isLeftWith
                (\case MapArtifactMissingRequired (RequiredPage _) → True; _ → False))
            publishMapArtifact lib gidA compat 72 (take 1 pages ⧺ pages) ≫= (`shouldSatisfy` isLeftWith
                (\case MapArtifactDuplicateKey _ → True; _ → False))
            publishMapArtifact lib gidA compat 72 ((MapPageKey 0 0 0, "short") : drop 1 pages)
                ≫= (`shouldSatisfy` isLeftWith (\case MapArtifactImage _ → True; _ → False))
            publishMapArtifact lib gidA compat 72 ((MapPageKey 0 90 0, fixturePage 2 (MapPageKey 0 0 0)) : pages)
                ≫= (`shouldSatisfy` isLeftWith (\case MapArtifactInvalidKey _ _ → True; _ → False))
            opened ← openMapArtifact lib gidA compat ≫= ok
            readMandatoryPage opened (MapPageKey 0 0 0) `shouldReturn`
                Right (fixturePage 1 (MapPageKey 0 0 0))

    it "an interrupted replacement keeps a complete entry or a recoverable copy" $
        withLibrary $ \_ lib → do
            _ ← publishFixture lib gidA 1 8
            inv ← inventoryOf 8
            let replacement = mandatoryFixture 2 inv
                beforeDisplace = noPublishHooks { phAfterStaged = \_ → throwIO Interrupt }
                afterDisplace = noPublishHooks { phAfterDisplaced = \_ → throwIO Interrupt }
            interrupting (publishMapArtifactWith beforeDisplace lib gidA compat 8 replacement)
            opened ← openMapArtifact lib gidA compat ≫= ok
            readMandatoryPage opened (MapPageKey 0 0 0) `shouldReturn` Right (fixturePage 1 (MapPageKey 0 0 0))
            interrupting (publishMapArtifactWith afterDisplace lib gidA compat 8 replacement)
            lookupEntry lib gidA `shouldReturn` Right Nothing
            openMapArtifact lib gidA compat `shouldReturn` Left (MapArtifactAbsent gidA)
            (_, rc) ← reconcileLibrary lib ≫= ok
            rcRecovered rc `shouldBe` [gidA]
            recovered ← openMapArtifact lib gidA compat ≫= ok
            readMandatoryPage recovered (MapPageKey 0 0 0) `shouldReturn` Right (fixturePage 1 (MapPageKey 0 0 0))

fineSpec ∷ Spec
fineSpec = describe "fine pages" $ do
    it "are cache data: removing or corrupting them leaves mandatory coverage valid" $
        withLibrary $ \root lib → do
            -- World 136: cutoff level 1, so level 0 is fine.
            inv ← inventoryOf 136
            mpiCoarseCutoffLevel inv `shouldBe` 1
            _ ← publishFixture lib gidA 1 136
            let dir = entryDirectory lib gidA
                key = MapPageKey 0 3 5
                rgba = fixturePage 3 key
            path ← writeFinePage dir gidA compat 136 key rgba ≫= ok
            readFinePage dir gidA compat 136 key `shouldReturn` Right (FinePageHit rgba)
            -- Unlisted: the entry is still exactly the committed record.
            Right (Just entry) ← lookupEntry lib gidA
            leStatus entry `shouldBe` EntryCommitted
            _ ← openMapArtifact lib gidA compat ≫= ok
            bytes ← BS.readFile path
            BS.writeFile path (replaceAt 700 "?" bytes)
            readFinePage dir gidA compat 136 key ≫= (`shouldSatisfy` \case
                Right (FinePageMiss (FinePageInvalid (MapArtifactChecksumMismatch _))) → True
                _ → False)
            _ ← openMapArtifact lib gidA compat ≫= ok
            removeFile path
            readFinePage dir gidA compat 136 key `shouldReturn` Right (FinePageMiss FinePageAbsent)
            _ ← openMapArtifact lib gidA compat ≫= ok
            -- The same contract in a scratch directory outside any entry.
            let scratch = root </> "fine"
            createDirectoryIfMissing True scratch
            _ ← writeFinePage scratch gidB compat 136 key rgba ≫= ok
            readFinePage scratch gidA compat 136 key ≫= (`shouldSatisfy` \case
                Right (FinePageMiss (FinePageInvalid (MapArtifactWrongWorld _ _))) → True
                _ → False)
            readFinePage scratch gidB compat { mcGenerator = 2 } 136 key ≫= (`shouldSatisfy` \case
                Right (FinePageMiss (FinePageInvalid (MapArtifactIncompatible CompatGenerator 2 1))) → True
                _ → False)

    it "a fine-page directory behind a symlink, at itself or its parent, is never followed" $
        withExclusiveTempDirectory "synarchy-paged-map-fine-link" $ \root → do
            let real = root </> "real"
                linkedParent = root </> "linked"
                key = MapPageKey 0 0 0
                rgba = fixturePage 1 key
            createDirectoryIfMissing True (real </> "fine")
            createDirectoryLink real linkedParent
            createDirectoryLink (real </> "fine") (root </> "fine-link")
            forM_ [linkedParent </> "fine", root </> "fine-link"] $ \dir → do
                writeFinePage dir gidA compat 136 key rgba ≫= (`shouldSatisfy` isLeftWith
                    (\case MapArtifactIO path _ → path ≡ dir; _ → False))
                readFinePage dir gidA compat 136 key ≫= (`shouldSatisfy` \case
                    Right (FinePageMiss (FinePageInvalid (MapArtifactIO path _))) → path ≡ dir
                    _ → False)
            -- Nothing was written through either link.
            listDirectory (real </> "fine") `shouldReturn` []

    it "a fine-page request for a mandatory level or an invalid key is the caller's error" $
        withExclusiveTempDirectory "synarchy-paged-map-fine" $ \dir → do
            readFinePage dir gidA compat 136 (MapPageKey 1 0 0) `shouldReturn`
                Left (MapArtifactNotFineLevel (MapPageKey 1 0 0) 1)
            readFinePage dir gidA compat 136 (MapPageKey 0 99 0)
                ≫= (`shouldSatisfy` isLeftWith (\case MapArtifactInvalidKey _ _ → True; _ → False))
            writeFinePage dir gidA compat 136 (MapPageKey 2 0 0) (fixturePage 1 (MapPageKey 2 0 0))
                ≫= (`shouldSatisfy` isLeftWith (\case MapArtifactNotFineLevel _ _ → True; _ → False))

-- | Where a key's entry sits in a manifest's page table.
entryIndex ∷ HasCallStack ⇒ BS.ByteString → MapPageKey → IO Int
entryIndex manifest key = do
    m ← ok (decodeMapManifest gidA compat manifest)
    maybe (expectationFailure "key not in manifest" ≫ error "unreachable") pure
          (L.findIndex ((≡ key) ∘ mmpKey) (mmPages m))

isLeftWith ∷ (ε → Bool) → Either ε α → Bool
isLeftWith p (Left e) = p e
isLeftWith _ (Right _) = False
