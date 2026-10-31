{-# LANGUAGE OverloadedStrings #-}
-- | The Wire art contract (#2516): flat colour sprites, a dedicated
--   facemap for every appearance and every lifecycle frame, and the
--   engine's selection of the right colour/facemap pair.
--
--   Two groups, deliberately separate:
--
--     * @"Wire structure lifecycle art"@ reads the SHIPPED files and
--       judges the art itself — inventory, dimensions, binary alpha,
--       flatness, pairing, coverage, frame counts, both handoff
--       equalities, distinct endpoints, and the approved-content pins.
--     * @"Wire facemap integration"@ judges what the engine does with
--       them: registration, the refusals a malformed pair earns, pair
--       selection for placement, construction and destruction, and a
--       pre-change save's Wire being lit with the new maps.
--
--   == The approved-content pins
--
--   @test-headless/fixtures/wire_art_pins.txt@ holds one SHA-256 of the
--   DECODED RGBA (plus the size) per image, keyed by appearance and
--   stage. They are written by @tools/make_wire_textures.py --pins@ after
--   owner approval and are read here independently of the files being
--   checked, so exchanging two images' contents under unchanged paths —
--   which path pinning, dimensions, alpha, counts and endpoint equality
--   all miss — fails. 'pinMismatches' is the check and the exchange
--   examples below prove it can fail.
--
--   Run: @cabal test synarchy-test-headless --test-options='--match "Wire structure lifecycle art"'@
--   and the same with @"Wire facemap integration"@.
module Test.Headless.Structure.WireArt (spec) where

import UPrelude
import Test.Hspec
import qualified Codec.Picture as JP
import qualified Crypto.Hash.SHA256 as SHA256
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BC
import qualified Data.HashMap.Strict as HM
import qualified Data.List as L
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Vector as V
import qualified Data.Vector.Storable as VS
import qualified Data.Yaml as Y
import Data.Scientific (toRealFloat)
import Numeric (showHex)

import Engine.Asset.Handle (TextureHandle(..), toInt)
import Engine.Graphics.Camera (CameraFacing(..))
import Engine.Graphics.Vulkan.Types.Vertex
    (Vertex(..), renderFlagLifecycleAlpha)
import Engine.Scene.Types (SortableQuad(..))
import Structure.ArtCatalog
import Structure.Palette (emptyTexPalette, internPath)
import Structure.Render
    ( ResolvedPieceArt(..), noPlacedArtOverride, opaqueTint
    , structurePieceQuadsResolved, structurePieceQuadsWith )
import Structure.Types (StructurePieceData(..), StructureSlot(..))
import Structure.WallCatalog (emptyStructureWallCatalog)
import Structure.Facing (WallEdge(..))
import Structure.Wire
    ( WireShape(..), allWireArtShapes, allWireNeighbors
    , allWireRisers, allWireShapes, isWireRiser, wireShapeFor, wireShapeName )

import Test.Headless.Render.StructureGhostFixture
    ( PackFixture(..), handleForPath, loadShippedPacks, packCatalog )

type Img = JP.Image JP.PixelRGBA8

-- * The expected inventory, stated here and not read from the pack

-- | How many frames each lifecycle runs to, equal across the sixteen
--   shapes (requirement 3).
buildFrames, breakFrames ∷ Int
buildFrames = 4
breakFrames = 5

shapeNames ∷ [Text]
shapeNames = map wireShapeName allWireArtShapes

-- | The sixteen shapes the autotile can select, and the four risers.
riserNames ∷ [Text]
riserNames = map wireShapeName allWireRisers

-- | The two risers whose wall faces the camera at the default facing
--   (FaceSouth: the SE and SW edges are the screen-front ones, which are
--   the east and south sides), and so show the cable's UNDERSIDE.
undersideRisers ∷ [Text]
undersideRisers = ["riser_e", "riser_s"]

colourPath, facePath ∷ Text → Text
colourPath s = "assets/textures/structures/wire/" <> s <> ".png"
facePath   s = "assets/textures/facemap/wire/" <> s <> ".png"

buildPath, buildFace, breakPath, breakFace ∷ Text → Int → Text
buildPath s k = "assets/textures/structures/wire/build/" <> s <> "_" <> tshow k <> ".png"
buildFace s k = "assets/textures/facemap/wire/build/" <> s <> "_" <> tshow k <> ".png"
breakPath s k = "assets/textures/structures/wire/break/" <> s <> "_" <> tshow k <> ".png"
breakFace s k = "assets/textures/facemap/wire/break/" <> s <> "_" <> tshow k <> ".png"

-- | Every (key, colour path, facemap path) the pack must declare, in
--   pin-key vocabulary: @shape@, @shape/build<k>@, @shape/break<k>@.
inventory ∷ [(Text, Text, Text)]
inventory = concat
    [ (s, colourPath s, facePath s)
      : [ (s <> "/build" <> tshow k, buildPath s k, buildFace s k)
        | k ← [0 .. buildFrames - 1] ]
      ⧺ [ (s <> "/break" <> tshow k, breakPath s k, breakFace s k)
        | k ← [0 .. breakFrames - 1] ]
    | s ← shapeNames ]

-- * Decoding

loadPng ∷ Text → IO Img
loadPng path = JP.readImage (T.unpack path) ⌦ \case
    Left err → fail (T.unpack path ⧺ ": " ⧺ err)
    Right img → pure (JP.convertRGBA8 img)

-- | The pin digest, exactly as the generator computes it: SHA-256 over
--   @"<w>x<h>\n"@ then the raw RGBA bytes.
digestOf ∷ Img → Text
digestOf img = T.pack (concatMap hex2 (BS.unpack digest))
  where
    digest = SHA256.hash
        (BC.pack (show (JP.imageWidth img) ⧺ "x" ⧺ show (JP.imageHeight img)
                  ⧺ "\n")
         <> BS.pack (VS.toList (JP.imageData img)))
    hex2 b = let h = showHex b "" in if length h ≡ 1 then '0' : h else h

px ∷ Img → Int → Int → (Word8, Word8, Word8, Word8)
px img x y = let JP.PixelRGBA8 r g b a = JP.pixelAt img x y in (r, g, b, a)

pixels ∷ Img → [((Int, Int), (Word8, Word8, Word8, Word8))]
pixels img = [ ((x, y), px img x y)
             | y ← [0 .. JP.imageHeight img - 1]
             , x ← [0 .. JP.imageWidth img - 1] ]

alphaOf ∷ (Word8, Word8, Word8, Word8) → Word8
alphaOf (_, _, _, a) = a

-- | The visible artwork: position and colour of every pixel that is
--   not fully transparent. RGB beneath zero alpha is not art and does
--   not count (requirement 9).
visible ∷ Img → [((Int, Int), (Word8, Word8, Word8, Word8))]
visible img = [ p | p@(_, c) ← pixels img, alphaOf c > 0 ]

sameArt ∷ Img → Img → Bool
sameArt a b = JP.imageWidth a ≡ JP.imageWidth b
    ∧ JP.imageHeight a ≡ JP.imageHeight b ∧ visible a ≡ visible b

-- | Parsed @wire_art_pins.txt@: key and half → digest.
loadPins ∷ IO (M.Map (Text, Text) Text)
loadPins = do
    raw ← BS.readFile "test-headless/fixtures/wire_art_pins.txt"
    pure $ M.fromList
        [ ((k, half), d)
        | line ← T.lines (TE.decodeUtf8 raw)
        , not (T.null line), not ("#" `T.isPrefixOf` line)
        , [k, half, d] ← [T.splitOn "\t" line] ]

-- | Every pinned key whose decoded content is not the approved one.
--   Missing on either side counts as a mismatch.
pinMismatches ∷ M.Map (Text, Text) Text → M.Map (Text, Text) Img → [(Text, Text)]
pinMismatches pins actual =
    [ k | k ← S.toList (M.keysSet pins `S.union` M.keysSet actual)
        , fmap digestOf (M.lookup k actual) ≢ M.lookup k pins ]

-- | Every shipped image, decoded, keyed like the pins.
loadAll ∷ IO (M.Map (Text, Text) Img)
loadAll = M.fromList ∘ concat <$> forM inventory (\(k, c, f) → do
    ci ← loadPng c
    fi ← loadPng f
    pure [((k, "colour"), ci), ((k, "facemap"), fi)])

-- | The shipped pack's YAML, as the pack loaders read it.
loadPackYaml ∷ IO Aeson.Object
loadPackYaml = Y.decodeFileEither "data/structure_packs/wire.yaml" ⌦ \case
    Left err → fail (show err)
    Right (Aeson.Object o) → pure o
    Right _ → fail "wire.yaml is not a mapping"

-- * Spec

spec ∷ Spec
spec = do
    lifecycleArtSpec
    integrationSpec

lifecycleArtSpec ∷ Spec
lifecycleArtSpec = describe "Wire structure lifecycle art" $ do
    it "declares exactly the twenty appearances (sixteen autotile shapes and four risers), each with its own colour \
       \sprite, facemap, four paired construction frames and five paired \
       \destruction frames, at the paths this suite states" $ do
        top ← loadPackYaml
        let conns = objAt "connections" top
        L.sort (map Key.toText (KM.keys conns)) `shouldBe` L.sort shapeNames
        -- the static texture paths are the ones saves already name
        forM_ shapeNames $ \s → do
            let c = objAt s conns
                str k = case KM.lookup k c of
                    Just (Aeson.String t) → t
                    _ → "<missing " <> tshow k <> ">"
                strs v = case v of
                    Just (Aeson.Array xs) →
                        [ t | Aeson.String t ← V.toList xs ]
                    _ → []
                w = objAt "destruction" c
            str "texture" `shouldBe` colourPath s
            str "facemap" `shouldBe` facePath s
            strs (KM.lookup "construction" c)
                `shouldBe` [ buildPath s k | k ← [0 .. buildFrames - 1] ]
            strs (KM.lookup "construction_facemaps" c)
                `shouldBe` [ buildFace s k | k ← [0 .. buildFrames - 1] ]
            strs (KM.lookup "frames" w)
                `shouldBe` [ breakPath s k | k ← [0 .. breakFrames - 1] ]
            strs (KM.lookup "facemaps" w)
                `shouldBe` [ breakFace s k | k ← [0 .. breakFrames - 1] ]
            case KM.lookup "fps" w of
                Just (Aeson.Number n) → (toRealFloat n ∷ Double) `shouldSatisfy` (> 0)
                other → expectationFailure ("fps: " ⧺ show other)
        -- no pack-level shared facemap any more
        KM.member "facemap" top `shouldBe` False

    it "names 400 distinct existing PNG files" $ do
        let paths = concat [ [c, f] | (_, c, f) ← inventory ]
        length paths `shouldBe` 400
        S.size (S.fromList paths) `shouldBe` 400
        forM_ paths $ \p → (BS.length <$> BS.readFile (T.unpack p))
            ⌦ (`shouldSatisfy` (> 0))

    it "keeps the power cable THIN: a straight run is at most three pixels \
       \tall in every column (top face plus side face)" $ do
        imgs ← loadAll
        forM_ ["straight_ns", "straight_ew", "end_n", "end_e", "end_s", "end_w"] $ \s → do
            let img = imgs M.! (s, "colour")
                col x = length [ () | y ← [0 .. 63], alphaOf (snd (px4 img x y)) > 0 ]
                px4 i x y = ((x, y), px i x y)
            (s, maximum (0 : [ col x | x ← [0 .. 95] ])) `shouldSatisfy` \(_, m) → m ≤ 3

    it "makes every image 96x64 RGBA with a BINARY alpha channel" $ do
        imgs ← loadAll
        forM_ (M.toList imgs) $ \((k, half), img) → do
            (k, half, JP.imageWidth img, JP.imageHeight img)
                `shouldBe` (k, half, 96, 64)
            forM_ (pixels img) $ \(_, c) →
                alphaOf c `shouldSatisfy` (`elem` [0, 255])

    it "paints FLAT colour: the static sprites use one dark-orange \
       \insulation colour and NO outline, and no highlight colour appears \
       \anywhere" $ do
        imgs ← loadAll
        let flat = S.fromList [(166, 78, 22)]
            highlight = (230, 150, 84)
        forM_ shapeNames $ \s → do
            let img = imgs M.! (s, "colour")
                allowed | s `elem` undersideRisers = S.insert (134, 62, 16) flat
                        | otherwise = flat
            S.fromList [ (r, g, b) | (_, (r, g, b, _)) ← visible img ]
                `shouldSatisfy` (`S.isSubsetOf` allowed)
        forM_ (M.toList imgs) $ \((k, half), img) →
            when (half ≡ "colour") $
                (k, [ () | (_, (r, g, b, _)) ← visible img
                         , (r, g, b) ≡ highlight ]) `shouldBe` (k, [])

    it "gives every visible colour pixel intentional facemap coverage: the \
       \facemap has the colour image's exact silhouette and every covered \
       \pixel is a pure right, top or left face" $ do
        imgs ← loadAll
        forM_ inventory $ \(k, _, _) → do
            let ci = imgs M.! (k, "colour")
                fi = imgs M.! (k, "facemap")
            forM_ (zip (pixels ci) (pixels fi)) $ \((pos, (_, _, _, ca)), (_, fc)) → do
                let (fr, fg, fb, fa) = fc
                (k, pos, fa) `shouldBe` (k, pos, ca)
                when (ca > 0) $
                    (k, pos, (fr, fg, fb)) `shouldSatisfy` \(_, _, rgb) →
                        rgb `elem` [(255, 0, 0), (0, 255, 0), (0, 0, 255)]

    it "lights each static sprite from its own facemap: a top face and a \
       \side face are both present, and a straight ribbon's side is a \
       \single face" $ do
        imgs ← loadAll
        let faces s = S.fromList [ (r, g, b)
                                 | (_, (r, g, b, _)) ← visible (imgs M.! (s, "facemap")) ]
        forM_ shapeNames $ \s →
            S.member (0, 255, 0) (faces s) `shouldBe` True
        faces "straight_ns" `shouldBe` S.fromList [(0, 255, 0), (255, 0, 0)]
        faces "straight_ew" `shouldBe` S.fromList [(0, 255, 0), (0, 0, 255)]

    it "runs every shape's construction to the SAME frame count, and every \
       \shape's destruction to the same (different) count" $
        forM_ shapeNames $ \s → do
            length [ () | (k, _, _) ← inventory, (s <> "/build") `T.isPrefixOf` k ]
                `shouldBe` buildFrames
            length [ () | (k, _, _) ← inventory, (s <> "/break") `T.isPrefixOf` k ]
                `shouldBe` breakFrames

    it "ends construction on the static pair, pixel for pixel, colour AND \
       \facemap" $ do
        imgs ← loadAll
        forM_ shapeNames $ \s → forM_ ["colour", "facemap"] $ \half →
            ((s, half), digestOf (imgs M.! (s <> "/build" <> tshow (buildFrames - 1), half)))
                `shouldBe` ((s, half), digestOf (imgs M.! (s, half)))

    it "starts destruction on the same static pair, pixel for pixel" $ do
        imgs ← loadAll
        forM_ shapeNames $ \s → forM_ ["colour", "facemap"] $ \half →
            ((s, half), digestOf (imgs M.! (s <> "/break0", half)))
                `shouldBe` ((s, half), digestOf (imgs M.! (s, half)))

    it "builds the cable up frame by frame: visible area never shrinks \
       \during construction and ends larger than it began" $ do
        imgs ← loadAll
        forM_ shapeNames $ \s → do
            -- cable-coloured area only: the stripped-metal tip of an
            -- unfinished run is not cable and leaves the last frame
            let cable img = length [ () | (_, (r, g, b, _)) ← visible img
                                        , (r, g, b) ≡ (166, 78, 22) ]
                sizes = [ cable (imgs M.! (s <> "/build" <> tshow k, "colour"))
                        | k ← [0 .. buildFrames - 1] ]
            (s, sizes) `shouldSatisfy` \(_, xs) → and (zipWith (≤) xs (drop 1 xs))
                                                  ∧ head' xs < last' xs

    it "ends destruction on a visibly distinct NEAR-EMPTY frame for every \
       \shape: some visible pixels, far fewer than the static art, and no \
       \two shapes alike on visible pixels" $ do
        imgs ← loadAll
        let end s = imgs M.! (s <> "/break" <> tshow (breakFrames - 1), "colour")
            staticCount s = length (visible (imgs M.! (s, "colour")))
        forM_ shapeNames $ \s → do
            let n = length (visible (end s))
            (s, n) `shouldSatisfy` \(_, c) → c ≥ 3 ∧ c ≤ 40
            (s, n * 10 < staticCount s ∨ staticCount s < 400) `shouldBe` (s, True)
            sameArt (end s) (imgs M.! (s, "colour")) `shouldBe` False
        distinctArt (map end shapeNames) `shouldBe` True

    it "keeps the sixteen static sprites, and every stage of every \
       \lifecycle, visibly distinct from one another" $ do
        imgs ← loadAll
        distinctArt [ imgs M.! (s, "colour") | s ← shapeNames ] `shouldBe` True
        forM_ [0 .. buildFrames - 2] $ \k →
            distinctArt [ imgs M.! (s <> "/build" <> tshow k, "colour")
                        | s ← shapeNames ] `shouldBe` True
        forM_ [1 .. breakFrames - 1] $ \k →
            distinctArt [ imgs M.! (s <> "/break" <> tshow k, "colour")
                        | s ← shapeNames ] `shouldBe` True
        -- within a shape, no two frames of one lifecycle are the same art
        forM_ shapeNames $ \s → do
            distinctArt [ imgs M.! (s <> "/build" <> tshow k, "colour")
                        | k ← [0 .. buildFrames - 1] ] `shouldBe` True
            distinctArt [ imgs M.! (s <> "/break" <> tshow k, "colour")
                        | k ← [0 .. breakFrames - 1] ] `shouldBe` True

    it "authors destruction independently: no destruction frame after the \
       \first is any construction frame, and the clip is not construction \
       \played backwards" $ do
        imgs ← loadAll
        forM_ shapeNames $ \s → do
            let b k = imgs M.! (s <> "/build" <> tshow k, "colour")
                d k = imgs M.! (s <> "/break" <> tshow k, "colour")
            forM_ [1 .. breakFrames - 1] $ \i → forM_ [0 .. buildFrames - 1] $ \j →
                (s, i, j, sameArt (d i) (b j)) `shouldBe` (s, i, j, False)

    it "matches the approved decoded content of every image, independently \
       \of the files' names" $ do
        pins ← loadPins
        imgs ← loadAll
        M.size pins `shouldBe` 400
        pinMismatches pins imgs `shouldBe` []

    it "FAILS the approved-content check when two appearances' contents \
       \are exchanged under unchanged paths (corner_ne / corner_nw)" $ do
        pins ← loadPins
        imgs ← loadAll
        let swapped = exchange (("corner_ne", "colour"), ("corner_nw", "colour")) imgs
            bad = pinMismatches pins swapped
        bad `shouldBe` [("corner_ne", "colour"), ("corner_nw", "colour")]

    it "FAILS the check when intermediate frame contents are exchanged, \
       \colour or facemap, under unchanged paths" $ do
        pins ← loadPins
        imgs ← loadAll
        pinMismatches pins (exchange (("cross/build1", "colour"), ("cross/build2", "colour")) imgs)
            `shouldBe` [("cross/build1", "colour"), ("cross/build2", "colour")]
        pinMismatches pins (exchange (("tee_n/break2", "facemap"), ("tee_n/break3", "facemap")) imgs)
            `shouldBe` [("tee_n/break2", "facemap"), ("tee_n/break3", "facemap")]
        -- a facemap exchanged with another appearance's facemap
        pinMismatches pins (exchange (("end_n", "facemap"), ("end_e", "facemap")) imgs)
            `shouldBe` [("end_e", "facemap"), ("end_n", "facemap")]

    it "authors the four risers as ONLY the vertical cable on their wall \
       \face (no flat foot): at least twelve pixels tall and at most three \
       \wide, with a top face and a side face" $ do
        imgs ← loadAll
        forM_ riserNames $ \s → do
            let ci = imgs M.! (s, "colour")
                fi = imgs M.! (s, "facemap")
                ys = [ y | ((_, y), _) ← visible ci ]
                faces = S.fromList [ (r, g, b) | (_, (r, g, b, _)) ← visible fi ]
            (s, maximum ys - minimum ys) `shouldSatisfy` \(_, h) → h ≥ 12
            let xs = [ x | ((x, _), _) ← visible ci ]
            (s, maximum xs - minimum xs) `shouldSatisfy` \(_, w) → w ≤ 2
            S.member (0, 255, 0) faces `shouldBe` True
            S.size (S.delete (0, 255, 0) faces) `shouldSatisfy` (≥ 1)
            length [ () | (_, (r, g, b, _)) ← visible fi
                        , (r, g, b) ≡ (0, 0, 255) ∨ (r, g, b) ≡ (255, 0, 0) ]
                `shouldSatisfy` (> 12)

    it "paints the cable's UNDERSIDE a slightly darker orange on exactly the \
       \two camera-facing risers (east and south at the default facing), \
       \baked into the colour, with its facemap still a side face" $ do
        imgs ← loadAll
        forM_ riserNames $ \s → forM_ ([ "" ] ⧺ [ "/build" <> tshow k | k ← [0 .. buildFrames - 1] ]) $ \stage → do
            let key = s <> stage
                ci = imgs M.! (key, "colour")
                fi = imgs M.! (key, "facemap")
                sideColours = S.fromList
                    [ (r, g, b)
                    | ((_, (r, g, b, _)), (_, (fr, fg, fb, _))) ←
                          zip (visible ci) (visible fi)
                    , (fr, fg, fb) ≢ (0, 255, 0), (r, g, b) ≢ (168, 170, 178) ]
                topColours = S.fromList
                    [ (r, g, b)
                    | ((_, (r, g, b, _)), (_, (fr, fg, fb, _))) ←
                          zip (visible ci) (visible fi)
                    , (fr, fg, fb) ≡ (0, 255, 0), (r, g, b) ≢ (168, 170, 178) ]
            when (s `elem` undersideRisers) $
                (key, sideColours) `shouldBe` (key, S.fromList [(134, 62, 16)])
            when (s `notElem` undersideRisers) $
                (key, sideColours) `shouldBe` (key, S.fromList [(166, 78, 22)])
            (key, topColours) `shouldBe` (key, S.fromList [(166, 78, 22)])

    it "keeps the underside darker only slightly: the same orange hue, \
       \under 30 % darker in every channel" $ do
        let (r0, g0, b0) = (166, 78, 22) ∷ (Int, Int, Int)
            (r1, g1, b1) = (134, 62, 16)
        forM_ [(r0, r1), (g0, g1), (b0, b1)] $ \(a, b) → do
            b `shouldSatisfy` (< a)
            fromIntegral (a - b) / fromIntegral a `shouldSatisfy` (< (0.3 ∷ Double))

    it "does NOT count RGB beneath zero alpha as a difference" $ do
        imgs ← loadAll
        let a = imgs M.! ("cross", "colour")
            b = JP.pixelMap (\p@(JP.PixelRGBA8 _ _ _ al) →
                    if al ≡ 0 then JP.PixelRGBA8 9 9 9 0 else p) a
        sameArt a b `shouldBe` True
        a ≡ b `shouldBe` False
  where
    head' = fromMaybe 0 ∘ listToMaybe
    last' = fromMaybe 0 ∘ listToMaybe ∘ reverse

-- | Exchange two keys' contents, leaving every key (path) in place.
exchange ∷ ((Text, Text), (Text, Text)) → M.Map (Text, Text) Img
         → M.Map (Text, Text) Img
exchange (a, b) m = M.insert a (m M.! b) (M.insert b (m M.! a) m)

-- | No two images share their visible artwork.
distinctArt ∷ [Img] → Bool
distinctArt imgs = S.size (S.fromList (map visible' imgs)) ≡ length imgs
  where visible' = visible

-- * Integration

integrationSpec ∷ Spec
integrationSpec = describe "Wire facemap integration" $ do
    it "registers the shipped pack and resolves each shape's OWN colour and \
       \facemap — twenty distinct facemaps, none of them the floor's" $ do
        cat ← shippedCatalog
        forM_ allWireArtShapes $ \w → do
            let s = wireShapeName w
                art = resolveUnplacedArt cat "wire" "wire" Nothing
                          (defaultPieceArtContext { pacWireShape = w })
            fmap (aaPath ∘ paTexture) art `shouldBe` Just (colourPath s)
            fmap (aaPath ∘ paFacemap) art `shouldBe` Just (facePath s)
        S.size (S.fromList [ aaPath (paFacemap a)
                           | w ← allWireArtShapes
                           , Just a ← [resolveUnplacedArt cat "wire" "wire" Nothing
                                          (defaultPieceArtContext { pacWireShape = w })] ])
            `shouldBe` 20

    it "selects the construction frame AND its facemap together at every \
       \progress, for every shape" $ do
        cat ← shippedCatalog
        forM_ allWireArtShapes $ \w → forM_ [0, 0.1, 0.25, 0.49, 0.5, 0.74, 0.99, 1.0] $ \p → do
            let s = wireShapeName w
                k = constructionFrameIndex buildFrames p
                got = resolveConstructionFramePair cat "wire"
                          (AppearanceKey Nothing (ApWire w)) p
            (s, p, fmap (aaPath ∘ fst) got, fmap (fmap aaPath ∘ snd) got)
                `shouldBe` (s, p, Just (buildPath s k), Just (Just (buildFace s k)))

    it "pairs every destruction frame with its facemap and declares the \
       \authored rate" $ do
        cat ← shippedCatalog
        forM_ allWireArtShapes $ \w → do
            let s = wireShapeName w
                ds = must (resolveDestructionSequence cat "wire"
                              (AppearanceKey Nothing (ApWire w)))
            map aaPath (V.toList (dsFrames ds))
                `shouldBe` [ breakPath s k | k ← [0 .. breakFrames - 1] ]
            fmap (map aaPath ∘ V.toList) (dsFacemaps ds)
                `shouldBe` Just [ breakFace s k | k ← [0 .. breakFrames - 1] ]
            dsFps ds `shouldBe` 10

    it "refuses a Wire sequence whose facemap list is the wrong length" $ do
        reg ← shippedWire
        refused (mutateConstruction WireCross
                    (\cs → cs { csFacemaps = V.take 3 <$> csFacemaps cs }) reg)
            ["wire", "cross", "4 entries", "3"]
        refused (mutateDestruction WireCross
                    (\ds → ds { dsFacemaps = V.take 2 <$> dsFacemaps ds }) reg)
            ["wire", "cross", "5 entries", "2"]

    it "refuses a Wire destruction family whose rotation siblings disagree or \
       \are missing (#2901): a camera turn swaps between them" $ do
        reg ← shippedWire
        -- a shorter / longer sibling would truncate or outrun the clip
        refused (mutateDestruction WireStraightEW
                    (\ds → ds { dsFrames = V.take 3 (dsFrames ds)
                              , dsFacemaps = V.take 3 <$> dsFacemaps ds }) reg)
            ["wire", "straight_ns", "disagree", "straight_ew: 3 frame(s)"]
        refused (mutateDestruction WireStraightEW
                    (\ds → ds { dsFps = dsFps ds + 5 }) reg)
            ["wire", "straight_ns", "disagree", "fps"]
        -- an undeclared sibling would make the clip vanish when turned to
        forM_ [ (WireCornerNE, "corner_ne"), (WireTeeS, "tee_s"), (WireEndW, "end_w") ] $
          \(w, name) →
            refused (reg { parDestruction = [ e | e@(k, _) ← parDestruction reg
                                                , k ≢ AppearanceKey Nothing (ApWire w) ] })
                ["wire", name, "every one must declare its own clip"]

    it "accepts a lone Wire shape's clip without siblings: the cross is its \
       \own rotation orbit" $ do
        reg ← shippedWire
        outcomeOf (mutateDestruction WireCross
                      (\ds → ds { dsFps = dsFps ds + 5 }) reg)
            `shouldBe` ArtRegistered

    it "refuses an UNPAIRED Wire sequence: there is no shared-map fallback" $ do
        reg ← shippedWire
        refused (mutateConstruction WireCross (\cs → cs { csFacemaps = Nothing }) reg)
            ["wire", "cross", "pair every lifecycle frame", "no shared-map fallback"]
        refused (mutateDestruction WireTeeN (\ds → ds { dsFacemaps = Nothing }) reg)
            ["wire", "tee_n", "pair every lifecycle frame"]

    it "refuses a paired facemap that is unloaded, escaping, or \
       \unmeasurable" $ do
        reg ← shippedWire
        refused (mutateConstruction WireCross (\cs → cs
                    { csFacemaps = (\v → v V.// [(1, ArtAsset (buildFace "cross" 1) (TextureHandle 0))]) <$> csFacemaps cs }) reg)
            ["wire", "cross", "not a loaded handle"]
        refused (mutateConstruction WireCross (\cs → cs
                    { csFacemaps = (\v → v V.// [(1, ArtAsset "../up.png" (TextureHandle 7))]) <$> csFacemaps cs }) reg)
            ["wire", "cross", "escapes"]
        refused (reg { parSizes = HM.delete (buildFace "cross" 2) (parSizes reg) })
            ["wire", "cross", "could not be measured"]

    it "refuses a facemap whose size differs from its colour image, for a \
       \frame, a destruction frame and a static pair" $ do
        reg ← shippedWire
        refused (reg { parSizes = HM.insert (buildFace "cross" 2) (96, 63) (parSizes reg) })
            ["wire", "cross", "96x64", "96x63"]
        refused (reg { parSizes = HM.insert (breakFace "end_n" 4) (48, 32) (parSizes reg) })
            ["wire", "end_n", "96x64", "48x32"]
        refused (reg { parSizes = HM.insert (facePath "isolated") (97, 64) (parSizes reg) })
            ["wire", "isolated", "static pair", "97x64"]

    it "refuses a paired facemap list on a WALL appearance" $ do
        packs ← loadShippedPacks
        let dungeon = must (listToMaybe [ pfRegistration p | p ← packs, pfName p ≡ "dungeon_1" ])
            wallNE = AppearanceKey Nothing (ApWall WallNE)
            floorSeq = must (lookup (AppearanceKey Nothing ApFloor) (parFrames dungeon))
            paired = floorSeq { csFacemaps = Just (csFrames floorSeq) }
            reg = dungeon { parFrames = (wallNE, paired) : parFrames dungeon }
        refused reg ["dungeon_1", "wall ne", "not supported for a wall"]

    it "draws a pre-change Wire piece — stored with the shared floor facemap \
       \— with its shape's dedicated facemap" $ do
        cat ← shippedCatalog
        let legacyFace = "assets/textures/facemap/floorface.png"
            quadsFor override shape =
                let (tid, pal0) = internPath (colourPath shape) emptyTexPalette
                    (fid, pal) = internPath legacyFace pal0
                    handles = HM.fromList
                        [ (tid, handleForPath (colourPath shape))
                        , (fid, handleForPath legacyFace) ]
                in structurePieceQuadsWith override emptyStructureWallCatalog pal
                       handles lookupSlotId HM.empty FaceSouth 14 60 0.8 3 3
                       SWire (StructurePieceData tid fid 14)
        forM_ shapeNames $ \s → do
            faceIds (quadsFor (placedWireArt cat) s)
                `shouldBe` [slotOf (facePath s)]
            -- and without the catalogue the stored map is what draws
            faceIds (quadsFor noPlacedArtOverride s)
                `shouldBe` [slotOf "assets/textures/facemap/floorface.png"]

    it "follows a NEIGHBOUR-driven shape change: the same tile re-placed \
       \with another shape is lit with that shape's map" $ do
        cat ← shippedCatalog
        let place shape =
                let (tid, pal0) = internPath (colourPath shape) emptyTexPalette
                    (fid, pal) = internPath (facePath shape) pal0
                    handles = HM.fromList [ (tid, handleForPath (colourPath shape))
                                          , (fid, handleForPath (facePath shape)) ]
                in faceIds (structurePieceQuadsWith (placedWireArt cat)
                       emptyStructureWallCatalog pal handles lookupSlotId
                       HM.empty FaceSouth 14 60 0.8 3 3 SWire
                       (StructurePieceData tid fid 14))
        place "isolated" `shouldBe` [slotOf (facePath "isolated")]
        place "end_e" `shouldBe` [slotOf (facePath "end_e")]
        place "straight_ew" `shouldBe` [slotOf (facePath "straight_ew")]

    it "never lets the autotile select a riser, yet requires all four as \
       \art with no fallback" $ do
        cat ← shippedCatalog
        forM_ allWireNeighbors $ \n →
            isWireRiser (wireShapeFor n) `shouldBe` False
        length allWireShapes `shouldBe` 16
        forM_ allWireRisers $ \w → do
            let s = wireShapeName w
                art = resolveUnplacedArt cat "wire" "wire" Nothing
                          (defaultPieceArtContext { pacWireShape = w })
            fmap (aaPath ∘ paFacemap) art `shouldBe` Just (facePath s)
        reg ← shippedWire
        refused (reg { parEntries = [ e | e@(k, _) ← parEntries reg
                                        , k ≢ AkWire WireRiserE ] })
            ["wire", "riser_e", "no art for this slot"]

    it "never overrides a non-Wire piece or an unknown sprite" $ do
        cat ← shippedCatalog
        placedFacemapPath (placedWireArt cat) SFloor (Just (colourPath "cross"))
            `shouldBe` Nothing
        placedFacemapPath (placedWireArt cat) SWire (Just "assets/other.png")
            `shouldBe` Nothing
        placedFacemapPath (placedWireArt cat) SWire Nothing `shouldBe` Nothing

    it "lights a PAIRED lifecycle frame by its own facemap with no lifecycle \
       \flag, and an unpaired one by the appearance's map WITH the flag" $ do
        let art face lifeFace = ResolvedPieceArt
                { rpaTexture = handleForPath "t.png"
                , rpaFacemap = handleForPath face
                , rpaTexturePath = Just "t.png"
                , rpaFacemapPath = Just face
                , rpaLifecycle = Just (handleForPath "frame.png")
                , rpaLifecycleFace = handleForPath <$> lifeFace }
            quads a = structurePieceQuadsResolved emptyStructureWallCatalog
                          lookupSlotId HM.empty FaceSouth 14 60
                          (opaqueTint 0.8) 3 3 SWire a 14
        faceIds (quads (art "own.png" (Just "paired.png")))
            `shouldBe` [slotOf "paired.png"]
        flagsOf (quads (art "own.png" (Just "paired.png"))) `shouldBe` [0]
        faceIds (quads (art "own.png" Nothing)) `shouldBe` [slotOf "own.png"]
        flagsOf (quads (art "own.png" Nothing)) `shouldBe` [renderFlagLifecycleAlpha]
  where

-- * Helpers

must ∷ Maybe α → α
must = fromMaybe (error "unexpected Nothing in a fixture lookup")

-- | A mapping-valued key, or an empty mapping when it is absent (the
--   example then fails on the missing content, not on a partial match).
objAt ∷ Text → Aeson.Object → Aeson.Object
objAt k o = case KM.lookup (Key.fromText k) o of
    Just (Aeson.Object x) → x
    _ → KM.empty

lookupSlotId ∷ TextureHandle → Word32
lookupSlotId = fromIntegral ∘ toInt

slotOf ∷ Text → Float
slotOf = fromIntegral ∘ toInt ∘ handleForPath

faceIds ∷ [SortableQuad] → [Float]
faceIds = L.nub ∘ map faceMapId ∘ concatMap verts

flagsOf ∷ [SortableQuad] → [Word32]
flagsOf = L.nub ∘ map renderFlags ∘ concatMap verts

verts ∷ SortableQuad → [Vertex]
verts sq = [sqV0 sq, sqV1 sq, sqV2 sq, sqV3 sq]

shippedPack ∷ IO PackFixture
shippedPack = do
    packs ← loadShippedPacks
    case [ p | p ← packs, pfName p ≡ "wire" ] of
        (p : _) → pure p
        [] → fail "no wire pack"

shippedWire ∷ IO PackArtRegistration
shippedWire = pfRegistration <$> shippedPack

shippedCatalog ∷ IO StructureArtCatalog
shippedCatalog = (packCatalog ∘ pure) <$> shippedPack

outcomeOf ∷ PackArtRegistration → RegistrationOutcome
outcomeOf reg = snd (registerPackArt reg emptyStructureArtCatalog)

refused ∷ PackArtRegistration → [Text] → Expectation
refused reg needles = case outcomeOf reg of
    ArtRegistrationRefused f →
        forM_ needles $ \n →
            (n, artFaultMessage f) `shouldSatisfy` \(_, m) → n `T.isInfixOf` m
    other → expectationFailure ("expected a refusal, got " ⧺ show other)

mutateConstruction ∷ WireShape → (ConstructionSequence → ConstructionSequence)
                   → PackArtRegistration → PackArtRegistration
mutateConstruction w f reg = reg
    { parFrames = [ if k ≡ AppearanceKey Nothing (ApWire w) then (k, f cs) else (k, cs)
                  | (k, cs) ← parFrames reg ] }

mutateDestruction ∷ WireShape → (DestructionSequence → DestructionSequence)
                  → PackArtRegistration → PackArtRegistration
mutateDestruction w f reg = reg
    { parDestruction = [ if k ≡ AppearanceKey Nothing (ApWire w) then (k, f ds) else (k, ds)
                       | (k, ds) ← parDestruction reg ] }
