{-# LANGUAGE Strict, OverloadedStrings #-}
-- | Pure ARTIFACT and FRESHNESS tests for the compiled unit-animation
--   atlas (#1259, TEX-3): the content digest\'s parity with
--   @tools\/pack_atlas.py@, image-side validation of a decoded sheet,
--   atlas mode selection against what the unit YAML declares, source-art
--   freshness against the compiled cell, the real temporary-filesystem
--   loading boundary, and the source digest with the Python float
--   representation it depends on.
--
--   These answer from PIXELS and FILES, which is what separates them
--   from the document-only cases in
--   "Test.Headless.Unit.Atlas.Index". They boot no engine: the
--   filesystem cases build their own temp tree and tear it down again,
--   and the live unit-registration boundary remains
--   "Test.Headless.Unit.Atlas.Loader"\'s.
module Test.Headless.Unit.Atlas.Freshness (spec) where

import UPrelude
import Test.Hspec
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as BLC
import qualified Data.HashMap.Strict as HM
import qualified Data.Map.Strict as Map
import qualified Codec.Picture as JP
import qualified Data.Text as T
import qualified Data.Vector.Storable as SV
import Control.Exception (finally)
import System.Directory
    ( createDirectoryIfMissing, getTemporaryDirectory, removeDirectoryRecursive
    , removeFile, renameFile )
import System.FilePath ((</>), takeDirectory)
import Test.Headless.Unit.Atlas.Document (arr, goodIndex, obj, parse, str)
import Test.Headless.Unit.Atlas.Rejection
    ( isRejected, rejection, shouldReject )
import Test.Headless.Unit.Atlas.Sheet
    ( extrudedSheet, fixtureAtlas, fixtureCellH, fixtureCellPad, fixtureCellW
    , fixtureH, fixturePixels, fixtureSlotH, fixtureSlotW, fixtureW
    , legacyFramePixels )
import Unit.Atlas.Index
import Unit.Atlas.Load (loadUnitAtlasIndexIn)
import Unit.Atlas.Types
import Unit.Direction (Direction(..))

-- | The YAML facts an index animation was compiled from: the same
--   playback declarations and one synthetic source path per real frame.
factsFor ∷ AtlasAnimation → YamlAnimFacts
factsFor aa = YamlAnimFacts
    { yafFps = aaFps aa, yafLoop = aaLoop aa, yafFlip = aaFlip aa
    , yafFrames = Map.fromList
        [ (d, [ "animations/" ⧺ T.unpack (aaName aa) ⧺ "/" ⧺ show d
                    ⧺ "/frame_" ⧺ show i ⧺ ".png"
              | i ← [0 .. adrFrameCount row - 1] ])
        | (d, row) ← Map.toList (aaDirections aa) ] }
-- * The loader fixture: a real on-disk unit tree
--
--   Source frames, compiled atlases composed from them, and an index
--   whose digests are the library's own — so the whole
--   read-parse-decode-verify pipeline runs against files, and a test
--   can break exactly one of them.

fixtureUnit ∷ Text
fixtureUnit = "fixture_unit"

fixtureCell ∷ Int
fixtureCell = 2

-- | @(name, flip, fps, loop, [(direction, real frame count)])@.
fixtureAnims ∷ [(Text, Bool, Float, Bool, [(Direction, Int)])]
fixtureAnims =
    [ ("blink", False, 8, True,  [(DirS, 2), (DirN, 2)])
      -- Unequal rows, so the atlas is padded and the padding must stay
      -- unreachable through the real loader too.
    , ("step",  False, 12, False, [(DirS, 2), (DirN, 3)])
    ]

dirToken ∷ Direction → String
dirToken d = case d of
    DirS → "south"      ; DirSW → "south-west"
    DirW → "west"       ; DirNW → "north-west"
    DirN → "north"      ; DirNE → "north-east"
    DirE → "east"       ; DirSE → "south-east"

framePath ∷ Text → Direction → Int → FilePath
framePath anim d i =
    "assets/textures/units" </> T.unpack fixtureUnit </> "animations"
        </> T.unpack anim </> dirToken d </> ("frame_" ⧺ pad3 i ⧺ ".png")
  where
    pad3 n = let t = show n in replicate (3 - length t) '0' ⧺ t

-- | Deterministic, per-frame-distinct art.
framePixel ∷ Text → Direction → Int → Int → Int → JP.PixelRGBA8
framePixel anim d i x y = JP.PixelRGBA8
    (fromIntegral ((T.length anim * 37 + x * 11) `mod` 256))
    (fromIntegral ((fromEnum d * 53 + y * 17) `mod` 256))
    (fromIntegral ((i * 71 + x * 3 + y * 5) `mod` 256))
    255

frameImage ∷ Text → Direction → Int → JP.Image JP.PixelRGBA8
frameImage anim d i =
    JP.generateImage (framePixel anim d i) fixtureCell fixtureCell

-- | The rows of one animation, in the compiler's own direction order.
orderedRows ∷ [(Direction, Int)] → [(Direction, Int)]
orderedRows ds = [ (d, n) | d ← [minBound .. maxBound], Just n ← [lookup d ds] ]

-- | The extrusion gutter the compiler compiles with, per side (#2076).
fixturePad ∷ Int
fixturePad = 1

-- | One cell's PHYSICAL slot size in this fixture: the cell plus its
--   gutter on both sides.
fixtureSlot ∷ Int
fixtureSlot = fixtureCell + 2 * fixturePad

-- | The atlas for one animation, laid out exactly as
--   @tools\/pack_atlas.py@ does it: each cell at
--   @(c * slot + pad, r * slot + pad)@, its one-texel gutter holding a
--   copy of that cell's own edge texels (corners included, which is
--   what clamping BOTH axes gives), and every unused SLOT fully
--   transparent, gutter and all.
atlasImage ∷ Text → [(Direction, Int)] → JP.Image JP.PixelRGBA8
atlasImage anim ds =
    JP.generateImage px (cols * fixtureSlot) (rows * fixtureSlot)
  where
    ordered = orderedRows ds
    rows = length ordered
    cols = maximum (1 : map snd ordered)
    clampCell = max 0 ∘ min (fixtureCell - 1)
    px x y =
        let (r, ly) = y `divMod` fixtureSlot
            (c, lx) = x `divMod` fixtureSlot
            xx = clampCell (lx - fixturePad)
            yy = clampCell (ly - fixturePad)
        in case drop r ordered of
            ((d, n) : _) | c < n → framePixel anim d c xx yy
            _ → JP.PixelRGBA8 0 0 0 0

fixtureYaml ∷ Map.Map Text YamlAnimFacts
fixtureYaml = Map.fromList
    [ (name, YamlAnimFacts fps loop flipV (Map.fromList
        [ (d, [framePath name d i | i ← [0 .. n - 1]]) | (d, n) ← ds ]))
    | (name, flipV, fps, loop, ds) ← fixtureAnims ]

-- | The index the compiler would emit for this tree, digests included.
fixtureIndex ∷ BL.ByteString
fixtureIndex = BLC.pack ∘ T.unpack ∘ obj $
    [ ("schema_version", "2")
    , ("generator", str "tools/pack_atlas.py")
    , ("tool_version", "2")
    , ("digest_algorithm", str "sha256")
    , ("unit", str fixtureUnit)
    , ("direction_order", arr (map (str ∘ T.pack ∘ dirToken)
          [minBound .. maxBound]))
    , ("animations", arr (map animEntry fixtureAnims))
    ]
  where
    animEntry (name, flipV, fps, loop, ds) =
        let ordered = orderedRows ds
            rows = length ordered
            cols = maximum (1 : map snd ordered)
            img = atlasImage name ds
        in obj
            [ ("name", str name)
            , ("storage_format", str "png")
            , ("atlas_path", str (T.pack (unitAtlasDir fixtureUnit
                                          </> T.unpack name ⧺ ".png")))
            , ("atlas_width", tshow (cols * fixtureSlot))
            , ("atlas_height", tshow (rows * fixtureSlot))
            , ("cell_width", tshow fixtureCell)
            , ("cell_height", tshow fixtureCell)
            , ("cell_padding", tshow fixturePad)
            , ("columns", tshow cols), ("rows", tshow rows)
            , ("flip", if flipV then "true" else "false")
            , ("fps", tshow fps), ("loop", if loop then "true" else "false")
            , ("directions", arr
                [ obj [ ("direction", str (T.pack (dirToken d)))
                      , ("row", tshow r), ("frame_count", tshow n) ]
                | (r, (d, n)) ← zip [(0 ∷ Int) ..] ordered ])
            , ("source_digest", str (fixtureSourceDigest name flipV fps loop ds))
            , ("atlas_digest", str (atlasContentDigest
                  (JP.imageWidth img) (JP.imageHeight img)
                  (packImage img)))
            ]

packImage ∷ JP.Image JP.PixelRGBA8 → BS.ByteString
packImage = BS.pack ∘ SV.toList ∘ JP.imageData

-- | The fixture's own @source_digest@, computed the way the compiler
--   would — so the tree on disk is internally consistent and each
--   negative case below breaks exactly one thing.
fixtureSourceDigest ∷ Text → Bool → Float → Bool → [(Direction, Int)] → Text
fixtureSourceDigest name flipV fps loop ds = sourceDigest SourceAnimInput
    { saiUnit = fixtureUnit, saiName = name
    , saiFlip = flipV, saiLoop = loop, saiFps = fps
    , saiCellWidth = fixtureCell, saiCellHeight = fixtureCell
    , saiCellPadding = fixturePad
    , saiColumns = maximum (1 : map snd ordered)
    , saiDirections =
        [ SourceDirectionInput (indexDirectionToken d) r
            [ SourceFrameInput
                { sfiPath = T.pack (framePath name d i)
                , sfiWidth = fixtureCell, sfiHeight = fixtureCell
                , sfiPixels = packImage (frameImage name d i) }
            | i ← [0 .. n - 1] ]
        | (r, (d, n)) ← zip [0 ..] ordered ]
    }
  where
    ordered = orderedRows ds

-- | Build the whole tree in a temp directory and tear it down after.
withAtlasFixture ∷ (FilePath → IO ()) → IO ()
withAtlasFixture action = do
    tmp ← getTemporaryDirectory
    let root = tmp </> "synarchy-unit-atlas-spec"
        write path bytes = do
            createDirectoryIfMissing True (takeDirectory (root </> path))
            BS.writeFile (root </> path) bytes
    forM_ fixtureAnims $ \(name, _, _, _, ds) → do
        forM_ ds $ \(d, n) → forM_ [0 .. n - 1] $ \i →
            write (framePath name d i)
                  (BL.toStrict (JP.encodePng (frameImage name d i)))
        write (unitAtlasDir fixtureUnit </> T.unpack name ⧺ ".png")
              (BL.toStrict (JP.encodePng (atlasImage name ds)))
    write (unitAtlasIndexPath fixtureUnit) (BL.toStrict fixtureIndex)
    (`finally` removeDirectoryRecursive root) (action root)

-- | Change one texel of an existing PNG in place.
repaint ∷ FilePath → IO ()
repaint path = do
    r ← JP.readImage path
    case r of
        Left e → expectationFailure ("fixture image unreadable: " ⧺ e)
        Right dyn → do
            let img = JP.convertRGBA8 dyn
                bump x y = let JP.PixelRGBA8 rr g b a = JP.pixelAt img x y
                           in if x ≡ 0 ∧ y ≡ 0
                              then JP.PixelRGBA8 (rr + 91) g b a
                              else JP.PixelRGBA8 rr g b a
            JP.writePng path (JP.generateImage bump
                (JP.imageWidth img) (JP.imageHeight img))

type LoadResult = Either AtlasLoadError (HM.HashMap Text AtlasAnimation)

isRejectedLoad ∷ LoadResult → Bool
isRejectedLoad (Left _) = True
isRejectedLoad _        = False

selectionOf ∷ LoadResult → [Text]
selectionOf (Right m) = HM.keys m
selectionOf _         = []

showLoad ∷ LoadResult → String
showLoad (Left e)  = T.unpack (renderAtlasLoadError e)
showLoad (Right m) = show (HM.keys m)

-- | Replace the first occurrence of @needle@ with @repl@.
replaceFirst ∷ BL.ByteString → BL.ByteString → BS.ByteString → BS.ByteString
replaceFirst needle repl hay =
    let n = BL.toStrict needle
        r = BL.toStrict repl
        (before, rest) = BS.breakSubstring n hay
    in if BS.null rest then hay
       else before <> r <> BS.drop (BS.length n) rest

spec ∷ Spec
spec = do
    describe "Unit.Atlas.Index — the content digest matches the compiler" $ do
        -- Reference values produced by tools/pack_atlas.py's own
        -- `content_digest`, so this pins the CROSS-LANGUAGE agreement
        -- rather than only self-consistency.
        it "reproduces pack_atlas.py's digest for a 2x1 sheet" $
            atlasContentDigest 2 1 (BS.pack [0 .. 7]) `shouldBe`
                "725b97fc0e24ce6ac14542dbef5e3fc34cf1c69a50d74246cfb12e62b3b0ab28"

        it "reproduces pack_atlas.py's digest for the 8x4 padded fixture" $
            atlasContentDigest fixtureW fixtureH fixturePixels `shouldBe`
                "da72fdace1058b0551ee0ac0f58e2af6f5de0989f16b7495228976a5be1b3384"

        -- The length prefixes exist so no two field sequences can
        -- collide; moving a byte across the width/height boundary must
        -- change the hash.
        it "distinguishes sheets whose dimensions merely reassociate" $
            atlasContentDigest 21 1 (BS.replicate 84 0) `shouldNotBe`
                atlasContentDigest 2 11 (BS.replicate 88 0)

    describe "Unit.Atlas.Index — image-side validation" $ do
        let anim = fixtureAtlas
                { aaAtlasDigest =
                    atlasContentDigest fixtureW fixtureH fixturePixels }
        it "accepts the image the index describes" $
            validateAtlasImage "acolyte" anim (DecodedImage fixtureW fixtureH fixturePixels)
                `shouldBe` Right ()

        it "rejects a decoded image whose dimensions differ" $
            validateAtlasImage "acolyte" anim (DecodedImage 4 2 (BS.replicate 32 0))
                `shouldReject` "but the index declares 8x4"

        it "rejects a buffer that is not RGBA8 of that size" $
            validateAtlasImage "acolyte" anim
                (DecodedImage fixtureW fixtureH (BS.take 8 fixturePixels))
                `shouldReject` "expected 128 RGBA8 bytes"

        it "rejects tampered pixels" $
            let tampered = BS.pack (0xFF : drop 1 (BS.unpack fixturePixels))
            in validateAtlasImage "acolyte" anim
                   (DecodedImage fixtureW fixtureH tampered)
                `shouldReject` "does not match the index's"

        it "names the unit, the animation and the ATLAS file, not the index" $ do
            let msg = rejection (validateAtlasImage "acolyte" anim
                          (DecodedImage 4 2 (BS.replicate 32 0)))
            msg `shouldSatisfy` T.isInfixOf "acolyte"
            msg `shouldSatisfy` T.isInfixOf "clip"
            msg `shouldSatisfy` T.isInfixOf "clip.png"

    describe "Unit.Atlas.Index — atlas mode selection" $ do
        let (idle, swing) = case parse goodIndex of
                Right [a, b] → (a, b)
                other → error ("fixture index must parse to two animations: "
                               ⧺ show (fmap (map aaName) other))
            -- The YAML facts are DERIVED from the index fixtures, so
            -- the happy path agrees by construction and each negative
            -- case below perturbs exactly one thing.
            yaml = Map.fromList
                [ ("idle",  factsFor idle)
                , ("swing", factsFor swing) ]
            -- One MORE animation than the index names — the shape a
            -- unit takes when a YAML edit outruns the compiler.
            uncompiled = Map.insert "walk"
                (YamlAnimFacts 8 True True
                    (Map.singleton DirS ["walk/s/frame_000.png"])) yaml

        it "selects exactly the animations the YAML declares" $
            case planUnitAtlasStorage "acolyte" yaml [idle, swing] of
                Left e → expectationFailure (T.unpack (renderAtlasLoadError e))
                Right m →
                    -- One entry per animation: the loader allocates one
                    -- handle, queues one upload and publishes one
                    -- `Animation` each, so this IS the "one atlas per
                    -- animation" count.
                    HM.keys m `shouldMatchList` ["idle", "swing"]

        -- Before #1261 a declared-but-uncompiled animation simply
        -- stayed on the per-frame path. There is no such path now, so
        -- publishing the unit without it would silently drop art the
        -- file asks for.
        it "rejects an animation the YAML declares that the index does \
           \not name, naming it" $
            planUnitAtlasStorage "acolyte" uncompiled [idle, swing]
                `shouldReject` "'walk'"

        it "an index-free unit is only valid when it declares no \
           \animations either" $ do
            planUnitAtlasStorage "acolyte" Map.empty [] `shouldBe` Right HM.empty
            planUnitAtlasStorage "acolyte" yaml [] `shouldReject` "'idle'"

        it "rejects an animation the YAML no longer declares" $
            planUnitAtlasStorage "acolyte" (Map.delete "swing" yaml)
                [idle, swing]
                `shouldReject` "YAML does not"

        it "rejects an index whose fps predates a YAML edit" $
            planUnitAtlasStorage "acolyte"
                (Map.insert "idle" ((factsFor idle) { yafFps = 10 }) yaml)
                [idle, swing]
                `shouldReject` "index fps"

        it "rejects an index whose loop flag predates a YAML edit" $
            planUnitAtlasStorage "acolyte"
                (Map.insert "idle" ((factsFor idle) { yafLoop = False }) yaml)
                [idle, swing]
                `shouldReject` "index loop"

        it "rejects an index whose flip flag predates a YAML edit" $
            planUnitAtlasStorage "acolyte"
                (Map.insert "idle" ((factsFor idle) { yafFlip = False }) yaml)
                [idle, swing]
                `shouldReject` "index flip"

        -- Source-art freshness, declaration half: an added, removed, or
        -- re-authored direction and a frame appended to or dropped from
        -- one are exactly the source edits a stale atlas keeps serving.
        it "rejects an index whose direction set predates a YAML edit" $ do
            let dropped = (factsFor idle)
                    { yafFrames = Map.delete DirN (yafFrames (factsFor idle)) }
                added = (factsFor idle)
                    { yafFrames = Map.insert DirW ["a.png", "b.png", "c.png", "d.png"]
                                      (yafFrames (factsFor idle)) }
            planUnitAtlasStorage "acolyte" (Map.insert "idle" dropped yaml)
                [idle] `shouldReject` "index directions"
            planUnitAtlasStorage "acolyte" (Map.insert "idle" added yaml)
                [idle] `shouldReject` "index directions"

        it "rejects an index whose per-direction frame count predates a YAML edit" $ do
            let shortened = (factsFor swing)
                    { yafFrames = Map.adjust (drop 1) DirW
                                      (yafFrames (factsFor swing)) }
            planUnitAtlasStorage "acolyte" (Map.insert "swing" shortened yaml)
                [swing] `shouldReject` "but the YAML declares 4 frames"

        it "rejects an index whose column count no longer spans the longest row" $ do
            let grown = (factsFor idle)
                    { yafFrames = Map.adjust (⧺ ["extra.png"]) DirS
                                      (yafFrames (factsFor idle)) }
            -- The per-direction count check fires first and names the
            -- direction; the column check backs it up for the case where
            -- counts agree but the sheet was packed for a shorter clip.
            planUnitAtlasStorage "acolyte" (Map.insert "idle" grown yaml)
                [idle] `shouldSatisfy` isRejected

        -- No partial publication: one bad animation rejects the whole
        -- unit rather than returning the good ones, so the caller never
        -- registers half a unit.
        it "returns nothing at all when ONE animation is stale" $
            planUnitAtlasStorage "acolyte" (Map.delete "swing" yaml)
                [idle, swing]
                `shouldSatisfy` isRejected

    -- The check no metadata can make: a source PNG repainted while its
    -- compiled atlas and index were left in place. The atlas is still
    -- internally consistent and its own digest still matches, so only
    -- reading the source art catches it.
    describe "Unit.Atlas.Index — source art freshness against the atlas" $ do
        let atlas = DecodedImage fixtureW fixtureH fixturePixels
            frameOf col = DecodedImage fixtureCellW fixtureCellH
                              (legacyFramePixels col)
            check col path frame = validateSourceFrame "acolyte" fixtureAtlas
                atlas DirS 0 col path frame

        it "accepts a source frame the atlas cell really holds" $ do
            check 0 "animations/clip/south/frame_000.png" (frameOf 0)
                `shouldBe` Right ()
            check 1 "animations/clip/south/frame_001.png" (frameOf 1)
                `shouldBe` Right ()

        it "rejects a source frame whose pixels the atlas no longer holds" $
            let repainted = DecodedImage fixtureCellW fixtureCellH
                    (BS.pack (0xFF : drop 1 (BS.unpack (legacyFramePixels 1))))
            in check 1 "animations/clip/south/frame_001.png" repainted
                `shouldReject` "does not match the pixels its atlas cell holds"

        -- One repainted pixel is the whole point: a check that only
        -- compared sizes, or sampled a corner, would pass this.
        it "catches a single changed texel anywhere in the cell" $
            forM_ [0 .. fixtureCellW * fixtureCellH * 4 - 1] $ \i →
                let orig = BS.unpack (legacyFramePixels 0)
                    bumped = [ if j ≡ i then b + 1 else b
                             | (j, b) ← zip [0 ..] orig ]
                    frame = DecodedImage fixtureCellW fixtureCellH
                                (BS.pack bumped)
                in check 0 "f.png" frame `shouldSatisfy` isRejected

        it "rejects a source frame that is no longer the cell's size" $
            check 0 "f.png" (DecodedImage 3 2 (BS.replicate 24 0))
                `shouldReject` "but the index's cell is 2x2"

        -- A frame swapped with another of the same animation still
        -- decodes and still fits the cell, so nothing but the pixels
        -- distinguishes it.
        it "rejects two source frames swapped between columns" $ do
            check 0 "f.png" (frameOf 1) `shouldSatisfy` isRejected
            check 1 "f.png" (frameOf 0) `shouldSatisfy` isRejected

        it "names the unit, the animation and the SOURCE frame" $ do
            let msg = rejection
                    (check 0 "animations/clip/south/frame_000.png" (frameOf 1))
            msg `shouldSatisfy` T.isInfixOf "acolyte"
            msg `shouldSatisfy` T.isInfixOf "clip"
            msg `shouldSatisfy` T.isInfixOf "frame_000.png"
            msg `shouldSatisfy` T.isInfixOf "pack_atlas.py --compile"

        it "reads the cell at the row and column it was told" $ do
            -- A two-row sheet: row 1 holds different art, so a cell
            -- reader that ignored the row would match the wrong frame.
            let twoRow = fixtureAtlas
                    { aaAtlasHeight = 2 * fixtureSlotH, aaRows = 2
                    , aaDirections = Map.fromList
                        [ (DirS, AtlasDirectionRow DirS 0 2)
                        , (DirN, AtlasDirectionRow DirN 1 2) ] }
                art row col = BS.pack
                    [ fromIntegral ((x * 37 + y * 11 + row * 83 + col * 53 + c * 7)
                                        `mod` 256)
                    | y ← [0 .. fixtureCellH - 1], x ← [0 .. fixtureCellW - 1]
                    , c ← [0 .. 3 ∷ Int] ]
                sheet = DecodedImage (2 * fixtureSlotW) (2 * fixtureSlotH)
                    (extrudedSheet fixtureCellW fixtureCellH fixtureCellPad 2 2
                        (\r c → Just (art r c)))
                frame row col = DecodedImage fixtureCellW fixtureCellH
                                    (art row col)
                v row col = validateSourceFrame "acolyte" twoRow sheet
                                DirS row col "f.png" (frame row col)
            v 0 0 `shouldBe` Right ()
            v 1 1 `shouldBe` Right ()
            validateSourceFrame "acolyte" twoRow sheet DirS 0 0 "f.png"
                (frame 1 0) `shouldSatisfy` isRejected
            validateSourceFrame "acolyte" twoRow sheet DirS 0 0 "f.png"
                (frame 0 1) `shouldSatisfy` isRejected

    -- The loader end of the contract, against a REAL fixture tree: the
    -- pure checks above answer from values, these answer from files.
    describe "Unit.Atlas.Load — one request per animation, none when rejected" $ do
        -- Before #1261 an absent atlas/ directory meant "this unit is
        -- on the per-frame path". There is no such path now, so a unit
        -- that DECLARES animations and ships no compiled artifacts has
        -- nothing to render them from and rejects, naming the count.
        it "a unit with NO atlas directory rejects, since there is no \
           \per-frame path left to fall back to" $
            withAtlasFixture $ \root → do
                removeDirectoryRecursive (root </> unitAtlasDir fixtureUnit)
                r ← loadUnitAtlasIndexIn root fixtureUnit fixtureYaml
                r `shouldSatisfy` isRejectedLoad
                selectionOf r `shouldBe` []
                T.pack (showLoad r) `shouldSatisfy`
                    T.isInfixOf "ships no compiled atlas artifacts"

        -- …but a unit that declares NO animations needs no artifacts,
        -- and the compiler writes it none.
        it "a unit that declares no animations at all resolves to an \
           \empty selection with no atlas directory" $
            withAtlasFixture $ \root → do
                removeDirectoryRecursive (root </> unitAtlasDir fixtureUnit)
                r ← loadUnitAtlasIndexIn root fixtureUnit Map.empty
                r `shouldBe` Right HM.empty

        -- The reverse-coverage half of planUnitAtlasStorage: an index
        -- that is internally fine but does not name something the YAML
        -- declares would silently drop that animation from the unit.
        it "an index that omits a DECLARED animation rejects, naming it" $
            withAtlasFixture $ \root → do
                let extra = Map.insert "wave"
                        (YamlAnimFacts 8 True False
                            (Map.singleton DirS ["nowhere/frame_000.png"]))
                        fixtureYaml
                r ← loadUnitAtlasIndexIn root fixtureUnit extra
                r `shouldSatisfy` isRejectedLoad
                selectionOf r `shouldBe` []
                T.pack (showLoad r) `shouldSatisfy` T.isInfixOf "'wave'"

        -- An atlas directory without its index is an INCOMPLETE
        -- compiled artifact, not a legacy unit: compiled PNGs sit
        -- beside the source frames, and falling back would serve the
        -- legacy path while pretending nothing is wrong.
        it "an atlas directory missing its index rejects, not falls back" $
            withAtlasFixture $ \root → do
                removeFile (root </> unitAtlasIndexPath fixtureUnit)
                r ← loadUnitAtlasIndexIn root fixtureUnit fixtureYaml
                r `shouldSatisfy` isRejectedLoad
                selectionOf r `shouldBe` []
                T.pack (showLoad r) `shouldSatisfy`
                    T.isInfixOf "but no index"

        it "a valid index yields exactly ONE request per indexed animation" $
            withAtlasFixture $ \root → do
                r ← loadUnitAtlasIndexIn root fixtureUnit fixtureYaml
                case r of
                    Right sel → do
                        HM.keys sel `shouldMatchList` ["blink", "step"]
                        -- One upload/handle/slot each (D-2/D-10), and
                        -- each naming its OWN atlas — not the unit's,
                        -- and not another animation's.
                        [ (nm, reg, aaPath aa)
                            | (nm, reg, aa) ← atlasTextureRequests fixtureUnit sel ]
                          `shouldBe`
                            [ ( "blink"
                              , "unit_" <> fixtureUnit <> "_blink_atlas"
                              , unitAtlasDir fixtureUnit </> "blink.png" )
                            , ( "step"
                              , "unit_" <> fixtureUnit <> "_step_atlas"
                              , unitAtlasDir fixtureUnit </> "step.png" ) ]
                        -- Each request carries the animation's OWN index
                        -- record, so the loader publishes what it
                        -- uploaded rather than looking it back up.
                        [ (nm, aaName aa)
                            | (nm, _, aa) ← atlasTextureRequests fixtureUnit sel ]
                          `shouldBe` [("blink", "blink"), ("step", "step")]
                    other → expectationFailure ("expected a selection, got "
                                                ⧺ showLoad other)

        it "a repainted SOURCE frame rejects, so nothing is ever selected" $
            withAtlasFixture $ \root → do
                repaint (root </> framePath "step" DirS 1)
                r ← loadUnitAtlasIndexIn root fixtureUnit fixtureYaml
                r `shouldSatisfy` isRejectedLoad
                -- No selection means no map to derive requests from:
                -- the caller cannot allocate a handle or queue an upload
                -- for ANY of this unit's animations, not just the broken
                -- one.
                selectionOf r `shouldBe` []

        it "a tampered ATLAS rejects the whole unit too" $
            withAtlasFixture $ \root → do
                repaint (root </> unitAtlasDir fixtureUnit </> "blink.png")
                r ← loadUnitAtlasIndexIn root fixtureUnit fixtureYaml
                r `shouldSatisfy` isRejectedLoad
                selectionOf r `shouldBe` []

        it "a missing source frame rejects rather than skipping it" $
            withAtlasFixture $ \root → do
                removeFile (root </> framePath "step" DirN 0)
                r ← loadUnitAtlasIndexIn root fixtureUnit fixtureYaml
                r `shouldSatisfy` isRejectedLoad
                selectionOf r `shouldBe` []

        -- Only the digest can see a forged digest.
        it "a forged source_digest rejects" $
            withAtlasFixture $ \root → do
                let ix = root </> unitAtlasIndexPath fixtureUnit
                raw ← BS.readFile ix
                BS.writeFile ix (replaceFirst
                    (BLC.pack (T.unpack (fixtureSourceDigest "step" False 12 False
                        [(DirS, 2), (DirN, 3)])))
                    (BLC.pack (replicate 64 'a')) raw)
                r ← loadUnitAtlasIndexIn root fixtureUnit fixtureYaml
                r `shouldSatisfy` isRejectedLoad
                T.pack (showLoad r) `shouldSatisfy` T.isInfixOf "source digest"
                selectionOf r `shouldBe` []

        -- And only the digest can see a frame RENAMED to a file with
        -- byte-identical pixels: the atlas still holds exactly those
        -- pixels, so every per-frame comparison passes.
        it "a path-only source change with identical pixels rejects" $
            withAtlasFixture $ \root → do
                let old' = framePath "step" DirS 1
                    new' = "assets/textures/units/" ⧺ T.unpack fixtureUnit
                               ⧺ "/animations/step/south/frame_009.png"
                renameFile (root </> old') (root </> new')
                let renamed = Map.adjust
                        (\ya → ya { yafFrames = Map.adjust
                            (map (\q → if q ≡ old' then new' else q)) DirS
                            (yafFrames ya) }) "step" fixtureYaml
                r ← loadUnitAtlasIndexIn root fixtureUnit renamed
                r `shouldSatisfy` isRejectedLoad
                T.pack (showLoad r) `shouldSatisfy` T.isInfixOf "source digest"
                selectionOf r `shouldBe` []

        it "ONE broken animation rejects the other, unbroken one as well" $
            withAtlasFixture $ \root → do
                repaint (root </> framePath "blink" DirS 0)
                r ← loadUnitAtlasIndexIn root fixtureUnit fixtureYaml
                case r of
                    Left e → do
                        aleAnimation e `shouldBe` Just "blink"
                        selectionOf r `shouldBe` []
                    other → expectationFailure ("expected a rejection, got "
                                                ⧺ showLoad other)

    -- Reproducing @source_digest@ means reproducing Python's @repr()@ of
    -- the narrowed fps. These expectations are CPython's own output for
    -- each value, so a formatting divergence fails HERE rather than by
    -- rejecting every atlas of a unit whose fps happens to land outside
    -- the range where Haskell's `show` and Python's `repr` agree.
    describe "Unit.Atlas.Digest — Python float repr" $ do
        it "matches CPython for every reference value" $
            forM_ pythonReprReference $ \(v, expected) →
                (v, pythonFloatRepr v) `shouldBe` (v, expected)

        it "switches to scientific exactly where CPython does" $ do
            -- decpt <= -4 or decpt > 16 — thresholds Haskell's own
            -- `show` does not share (it switches at 0.1 and 1e7).
            pythonFloatRepr 1.0e7 `shouldBe` "10000000.0"
            pythonFloatRepr 0.01 `shouldNotSatisfy` T.isInfixOf "e"
            pythonFloatRepr 9.999999747378752e-05
                `shouldSatisfy` T.isInfixOf "e-05"

        it "pads the exponent to two digits and always signs it" $ do
            pythonFloatRepr 1.401298464324817e-45
                `shouldSatisfy` T.isInfixOf "e-45"
            pythonFloatRepr 9.999999747378752e-06
                `shouldSatisfy` T.isInfixOf "e-06"
            pythonFloatRepr 1.0000000272564224e16
                `shouldSatisfy` T.isInfixOf "e+16"

    describe "Unit.Atlas.Digest — source digest" $ do
        -- The reference value comes from tools/pack_atlas.py's own
        -- `source_digest`, run on exactly these inputs, so this pins the
        -- CROSS-LANGUAGE agreement rather than self-consistency.
        it "reproduces pack_atlas.py's digest for a known animation" $
            sourceDigest referenceSourceAnim `shouldBe`
                "1725088fbf27358e330387c4c9d2a20eb5ed77d7a99ada1dbfe7653b11309753"

        -- Every field is IN the stream, and the length prefixes make it
        -- injective: perturbing any one input must change the digest.
        it "changes when any single input changes" $ do
            let base = sourceDigest referenceSourceAnim
                perturbations =
                    [ ("unit",      referenceSourceAnim { saiUnit = "other" })
                    , ("animation", referenceSourceAnim { saiName = "walk" })
                    , ("flip",      referenceSourceAnim { saiFlip = True })
                    , ("loop",      referenceSourceAnim { saiLoop = True })
                    , ("fps",       referenceSourceAnim { saiFps = 8 })
                    , ("cell w",    referenceSourceAnim { saiCellWidth = 3 })
                    , ("cell h",    referenceSourceAnim { saiCellHeight = 3 })
                    , ("cell pad",  referenceSourceAnim { saiCellPadding = 2 })
                    , ("columns",   referenceSourceAnim { saiColumns = 4 })
                    , ("dir set",   referenceSourceAnim
                          { saiDirections = take 1 (saiDirections referenceSourceAnim) })
                    , ("row",       overFirstDir (\d → d { sdiRow = 7 }))
                    , ("dir token", overFirstDir (\d → d { sdiDirection = "east" }))
                      -- The path-only change: same pixels, renamed file.
                      -- NOTHING else in the index records frame paths, so
                      -- only the digest can see this.
                    , ("frame path", overFirstFrame (\f → f { sfiPath = "renamed.png" }))
                    , ("frame size", overFirstFrame (\f → f { sfiWidth = 4 }))
                    , ("frame pixels", overFirstFrame (\f →
                          f { sfiPixels = BS.pack (0xFF : drop 1 (BS.unpack (sfiPixels f))) }))
                    ]
            forM_ perturbations $ \(label, perturbed) →
                (label, sourceDigest perturbed ≡ base) `shouldBe` (label, False)

        -- Moving a byte across a field boundary must not collide — what
        -- the length prefixes exist for.
        it "does not collide when text moves across a field boundary" $
            sourceDigest (referenceSourceAnim { saiUnit = "fixture_unitstep"
                                              , saiName = "" })
                `shouldNotBe` sourceDigest referenceSourceAnim

-- | The animation @tools/pack_atlas.py@'s `source_digest` was run on to
--   produce the reference value above: two directions, unequal frame
--   counts, 2x2 cells at the one-texel gutter, fps 12 narrowed through
--   32-bit.
referenceSourceAnim ∷ SourceAnimInput
referenceSourceAnim = SourceAnimInput
    { saiUnit = "fixture_unit", saiName = "step"
    , saiFlip = False, saiLoop = False, saiFps = 12
    , saiCellWidth = 2, saiCellHeight = 2, saiCellPadding = 1
    , saiColumns = 3
    , saiDirections =
        [ SourceDirectionInput "south" 0
            [ refFrame "a/south/frame_000.png" 0
            , refFrame "a/south/frame_001.png" 1 ]
        , SourceDirectionInput "north" 1
            [ refFrame "a/north/frame_000.png" 2
            , refFrame "a/north/frame_001.png" 3
            , refFrame "a/north/frame_002.png" 4 ]
        ]
    }

refFrame ∷ Text → Int → SourceFrameInput
refFrame path seed = SourceFrameInput
    { sfiPath = path, sfiWidth = 2, sfiHeight = 2
    , sfiPixels = BS.pack
        [ fromIntegral ((x * 13 + y * 29 + seed * 7) `mod` 256)
        | y ← [0 .. 1 ∷ Int], x ← [0 .. 1 ∷ Int], _ ← [0 .. 3 ∷ Int] ] }

overFirstDir ∷ (SourceDirectionInput → SourceDirectionInput) → SourceAnimInput
overFirstDir f = case saiDirections referenceSourceAnim of
    (d:rest) → referenceSourceAnim { saiDirections = f d : rest }
    []       → referenceSourceAnim

overFirstFrame ∷ (SourceFrameInput → SourceFrameInput) → SourceAnimInput
overFirstFrame f = overFirstDir $ \d → case sdiFrames d of
    (fr:rest) → d { sdiFrames = f fr : rest }
    []        → d

-- | CPython @repr()@ output for float32-exact values across the whole
--   representable range, including both sides of each notation
--   threshold. Generated with @tools/pack_atlas.py@'s own narrowing.
pythonReprReference ∷ [(Float, Text)]
pythonReprReference =
    [ (1.0, "1.0")
    , (2.0, "2.0")
    , (4.0, "4.0")
    , (6.0, "6.0")
    , (8.0, "8.0")
    , (10.0, "10.0")
    , (12.0, "12.0")
    , (15.0, "15.0")
    , (24.0, "24.0")
    , (30.0, "30.0")
    , (60.0, "60.0")
    , (120.0, "120.0")
    , (240.0, "240.0")
    , (0.5, "0.5")
    , (12.5, "12.5")
    , (8.100000381469727, "8.100000381469727")
    , (0.3333333432674408, "0.3333333432674408")
    , (9.999999747378752e-06, "9.999999747378752e-06")
    , (9.999999747378752e-05, "9.999999747378752e-05")
    , (0.0010000000474974513, "0.0010000000474974513")
    , (0.10000000149011612, "0.10000000149011612")
    , (0.009999999776482582, "0.009999999776482582")
    , (10000000.0, "10000000.0")
    , (100000000.0, "100000000.0")
    , (999999986991104.0, "999999986991104.0")
    , (1.0000000272564224e16, "1.0000000272564224e+16")
    , (9.999999843067494e16, "9.999999843067494e+16")
    , (1.0000000200408773e20, "1.0000000200408773e+20")
    , (3.4028234663852886e38, "3.4028234663852886e+38")
    , (1.1754943508222875e-38, "1.1754943508222875e-38")
    , (1.401298464324817e-45, "1.401298464324817e-45")
    , (1.4999999621068127e-05, "1.4999999621068127e-05")
    , (123456.7890625, "123456.7890625")
    , (1.2676506002282294e30, "1.2676506002282294e+30")
    , (7.888609052210118e-31, "7.888609052210118e-31")
    ]
