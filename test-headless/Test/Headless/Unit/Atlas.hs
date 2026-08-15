{-# LANGUAGE Strict, OverloadedStrings #-}
-- | Pure tests for the compiled unit-animation atlas runtime (#1259,
--   TEX-3): index parsing and validation, the content digest, atlas
--   mode selection, and the CONSUMER geometry that reads a frame — the
--   render quad, the hit rect, and the UI sprite batch.
--
--   The consumer half is deliberately not metadata-only. A
--   'FrameSample' that reports the right cell size and the right UV
--   rect is worth nothing if the renderer keeps measuring the whole
--   sheet, so these gate 'Unit.Render.unitToQuad''s emitted vertices,
--   'Unit.HitTest.unitHitRect''s rect, and 'UI.Render.renderSpriteBatch''s
--   UVs, and they compare the TEXELS an atlas cell resolves to against
--   the texels its legacy frame would.
module Test.Headless.Unit.Atlas (spec) where

import UPrelude
import Test.Hspec
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as BLC
import qualified Data.HashMap.Strict as HM
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import qualified Data.Vector as V
import qualified Data.Vector.Storable as SV
import Engine.Asset.Handle (TextureHandle(..))
import Engine.Graphics.Camera (CameraFacing(..))
import Engine.Graphics.Vulkan.Texture.Handle (BindlessTextureHandle(..))
import Engine.Graphics.Vulkan.Texture.Rebind
    (FilterRebindPlan(..), SlotRebind(..), planFilterRebind)
import Engine.Graphics.Vulkan.Types.Vertex (Vertex(..), Vec2(..))
import Engine.Graphics.Vulkan.Texture.Slot (TextureSlot(..))
import Vulkan.Core10 (ImageView(..), Sampler(..))
import Engine.Scene.Types (SortableQuad(..))
import UI.Render (renderSpriteBatch)
import Engine.Scene.Base (LayerId(..))
import Engine.Scene.Types.Batch (RenderBatch(..))
import Unit.Atlas.Index
import Unit.Atlas.Types
import Unit.Direction (Direction(..))
import Unit.Faction (Faction(..))
import Unit.HitTest (unitHitRect)
import Unit.Render (unitToQuad)
import Unit.Types
import World.Page.Types (WorldPageId(..))

-- * The canonical index document
--
--   A two-animation index in exactly the shape @tools/pack_atlas.py@'s
--   `build_index_document` emits, built field by field so a test can
--   corrupt ONE thing and assert the specific rejection.

type Field = (Text, Text)

obj ∷ [Field] → Text
obj fs = "{" <> T.intercalate "," [ "\"" <> k <> "\":" <> v | (k, v) ← fs ] <> "}"

str ∷ Text → Text
str t = "\"" <> t <> "\""

arr ∷ [Text] → Text
arr xs = "[" <> T.intercalate "," xs <> "]"

directionEntry ∷ Text → Int → Int → Text
directionEntry d row n = obj
    [ ("direction", str d), ("row", tshow row), ("frame_count", tshow n) ]

-- | The @idle@ animation: five authored directions, mirroring on,
--   32x48 cells, four frames each.
idleFields ∷ [Field]
idleFields =
    [ ("name", str "idle")
    , ("storage_format", str "png")
    , ("atlas_path", str "assets/textures/units/acolyte/atlas/idle.png")
    , ("atlas_width", "128"), ("atlas_height", "240")
    , ("cell_width", "32"), ("cell_height", "48")
    , ("columns", "4"), ("rows", "5")
    , ("flip", "true"), ("fps", "8"), ("loop", "true")
    , ("directions", arr
        [ directionEntry "south" 0 4
        , directionEntry "north-west" 1 4
        , directionEntry "north" 2 4
        , directionEntry "north-east" 3 4
        , directionEntry "east" 4 4 ])
    , ("source_digest", str "aaaa"), ("atlas_digest", str "bbbb")
    ]

-- | The @swing@ animation: eight directions, unequal counts, no mirror.
swingFields ∷ [Field]
swingFields =
    [ ("name", str "swing")
    , ("storage_format", str "png")
    , ("atlas_path", str "assets/textures/units/acolyte/atlas/swing.png")
    , ("atlas_width", "192"), ("atlas_height", "384")
    , ("cell_width", "32"), ("cell_height", "48")
    , ("columns", "6"), ("rows", "8")
    , ("flip", "false"), ("fps", "12"), ("loop", "false")
    , ("directions", arr
        [ directionEntry "south" 0 6
        , directionEntry "south-west" 1 2
        , directionEntry "west" 2 5
        , directionEntry "north-west" 3 1
        , directionEntry "north" 4 6
        , directionEntry "north-east" 5 3
        , directionEntry "east" 6 4
        , directionEntry "south-east" 7 2 ])
    , ("source_digest", str "cccc"), ("atlas_digest", str "dddd")
    ]

indexWith ∷ [Field] → [[Field]] → BL.ByteString
indexWith top anims = BLC.pack ∘ T.unpack ∘ obj $
    ([ ("schema_version", "1")
     , ("generator", str "tools/pack_atlas.py")
     , ("tool_version", "1")
     , ("digest_algorithm", str "sha256")
     , ("unit", str "acolyte")
     , ("direction_order", arr (map str
         [ "south", "south-west", "west", "north-west"
         , "north", "north-east", "east", "south-east" ]))
     ] `override` top)
    <> [("animations", arr (map obj anims))]

-- | Replace matching keys, keeping order — so a test can override one
--   top-level field without restating the document.
override ∷ [Field] → [Field] → [Field]
override base new =
    [ (k, maybe v id (lookup k new)) | (k, v) ← base ]
    <> [ f | f@(k, _) ← new, isNothing (lookup k base) ]

-- | Replace one animation field.
setField ∷ Text → Text → [Field] → [Field]
setField k v = map (\(k', v') → if k' ≡ k then (k', v) else (k', v'))

dropField ∷ Text → [Field] → [Field]
dropField k = filter ((≢ k) ∘ fst)

goodIndex ∷ BL.ByteString
goodIndex = indexWith [] [idleFields, swingFields]

tshow ∷ Show a ⇒ a → Text
tshow = T.pack ∘ show

parse ∷ BL.ByteString → Either AtlasLoadError [AtlasAnimation]
parse = parseAtlasIndex "acolyte" "assets/textures/units/acolyte/atlas/index.json"

-- | The reason text of a rejection, or a marker when it unexpectedly
--   succeeded. Assertions match on a substring so they pin WHAT was
--   wrong without freezing the exact wording.
rejection ∷ Either AtlasLoadError a → Text
rejection (Left e)  = renderAtlasLoadError e
rejection (Right _) = "<<accepted>>"

shouldReject ∷ HasCallStack ⇒ Either AtlasLoadError a → Text → Expectation
shouldReject r needle =
    let msg = rejection r
    in if needle `T.isInfixOf` msg
        then pure ()
        else expectationFailure
            ("expected a rejection mentioning " ⧺ show needle
             ⧺ ", got: " ⧺ T.unpack msg)

-- * Consumer fixtures

-- | A 4x2 RGBA8 sheet holding two 2x2 cells side by side on one row.
--   Every texel is distinct, so a wrong sub-rect resolves to visibly
--   different bytes rather than coincidentally matching.
fixtureW, fixtureH, fixtureCellW, fixtureCellH ∷ Int
fixtureW = 4
fixtureH = 2
fixtureCellW = 2
fixtureCellH = 2

fixturePixels ∷ BS.ByteString
fixturePixels = BS.pack
    [ b | y ← [0 .. fixtureH - 1], x ← [0 .. fixtureW - 1]
        , b ← [ fromIntegral (x * 16 + y)
              , fromIntegral (255 - (x * 16 + y))
              , fromIntegral ((x * 7 + y * 13) `mod` 256)
              , 255 ] ]

-- | Read one RGBA texel out of a sheet.
texelAt ∷ Int → Int → BS.ByteString → Int → Int → [Word8]
texelAt w _h px x y =
    let o = (y * w + x) * 4
    in [BS.index px (o + i) | i ← [0 .. 3]]

-- | The texels a quad spanning @fw x fh@ screen pixels resolves to
--   under NEAREST sampling of @uv@ within a @w x h@ sheet, in screen
--   order. This is the GPU's own rule: fragment centre @i + 0.5@ maps
--   to @u@, and the sampler takes @floor (u * w)@.
--
--   'flipX' mirrors within the sub-rect, exactly as the renderer's
--   vertex assignment does.
sampleFrame
    ∷ Int → Int → BS.ByteString      -- ^ sheet
    → UVRect → Bool
    → Int → Int                      -- ^ frame pixel size
    → [[Word8]]
sampleFrame w h px (u0, v0, u1, v1) flipX fw fh =
    [ texelAt w h px (clampI 0 (w - 1) tx) (clampI 0 (h - 1) ty)
    | j ← [0 .. fh - 1], i ← [0 .. fw - 1]
    , let fx = (fromIntegral i + 0.5) / fromIntegral fw ∷ Float
          fy = (fromIntegral j + 0.5) / fromIntegral fh ∷ Float
          (uL, uR) = if flipX then (u1, u0) else (u0, u1)
          u = uL + fx * (uR - uL)
          v = v0 + fy * (v1 - v0)
          tx = floor (u * fromIntegral w)
          ty = floor (v * fromIntegral h)
    ]
  where
    clampI lo hi = max lo ∘ min hi

-- | The atlas metadata describing the fixture sheet as two 2x2 cells on
--   one row (DirS, two frames).
fixtureAtlas ∷ AtlasAnimation
fixtureAtlas = AtlasAnimation
    { aaName = "clip", aaFormat = AtlasFormatPng
    , aaPath = "assets/textures/units/acolyte/atlas/clip.png"
    , aaAtlasWidth = fixtureW, aaAtlasHeight = fixtureH
    , aaCellWidth = fixtureCellW, aaCellHeight = fixtureCellH
    , aaColumns = 2, aaRows = 1
    , aaFlip = False, aaFps = 8, aaLoop = True
    , aaDirections = Map.singleton DirS (AtlasDirectionRow DirS 0 2)
    , aaSourceDigest = "src", aaAtlasDigest = "atlas"
    }

fixtureStorage ∷ AnimStorage
fixtureStorage = StorageAtlas (ResidentAtlas fixtureAtlas (TextureHandle 900))

-- | The same two frames as standalone 2x2 legacy images.
legacyFramePixels ∷ Int → BS.ByteString
legacyFramePixels col = BS.pack
    [ b | y ← [0 .. fixtureCellH - 1], x ← [0 .. fixtureCellW - 1]
        , b ← concat [ texelAt fixtureW fixtureH fixturePixels
                           (col * fixtureCellW + x) y ] ]

-- * A unit instance the render / hit-test consumers can be run on

atlasTex ∷ TextureHandle
atlasTex = TextureHandle 900

sheetSizes ∷ HM.HashMap TextureHandle (Int, Int)
sheetSizes = HM.fromList
    [ (atlasTex, (fixtureW, fixtureH))
    , (TextureHandle 5, (fixtureCellW, fixtureCellH)) ]

atlasSample ∷ FrameSample
atlasSample = fixtureSampleAt 1

frame0Sample ∷ FrameSample
frame0Sample = fixtureSampleAt 0

fixtureSampleAt ∷ Int → FrameSample
fixtureSampleAt i = case storageSampleAt fixtureStorage DirS i False of
    Just s  → s
    Nothing → error ("fixture atlas must resolve DirS frame " ⧺ show i)

legacySample ∷ FrameSample
legacySample = wholeImageSample (TextureHandle 5) False

testInstance ∷ UnitInstance
testInstance = UnitInstance
    { uiDefName = "u", uiName = "", uiPage = WorldPageId "p"
    , uiTexture = TextureHandle 5, uiDirSprites = Map.empty
    , uiBaseWidth = 0
    , uiGridX = 0, uiGridY = 0, uiGridZ = 0, uiRealZ = 0
    , uiFacing = DirS, uiCurrentAnim = "", uiAnimStart = 0
    , uiAnimReverse = False, uiActivity = "idle", uiPose = "standing"
    , uiAnimStride = 1
    , uiStats = HM.empty, uiModifiers = HM.empty, uiSkills = HM.empty
    , uiKnowledge = HM.empty, uiInventory = [], uiEquipment = HM.empty
    , uiAccessories = [], uiFactionId = FactionNeutral
    , uiWounds = [], uiScars = [], uiImmuneResponse = 0
    , uiImmunities = HM.empty, uiBlood = 0
    , uiLastAttackerUid = Nothing, uiLastAttackerAt = 0
    , uiAnimOverride = "", uiFrozen = False, uiForceLoop = False
    , uiClimbDest = Nothing, uiTrailState = Nothing
    }

-- | A def whose only animation is the fixture atlas, so 'unitToQuad'
--   resolves through the real 'pickFrame'.
atlasDef ∷ UnitDef
atlasDef = UnitDef
    { udName = "u", udNamePool = Nothing, udDisplayName = Nothing
    , udTexture = TextureHandle 5, udPortrait = Nothing
    , udDirSprites = Map.empty, udBaseWidth = 0
    , udMaxSpeed = 1, udRunThreshold = 0.6
    , udAnimations = HM.singleton "clip" Animation
        { aFps = 8, aLoop = True, aFlip = False, aStorage = fixtureStorage }
    , udStateAnims = HM.empty, udEagerStats = False
    , udStatTemplates = HM.empty, udBodyTemplates = HM.empty
    , udSkillTemplates = HM.empty, udKnowledgeTemplates = HM.empty
    , udStartingInventory = [], udEquipmentClass = Nothing
    , udStartingEquipment = HM.empty, udStartingAccessories = []
    , udBodyParts = [], udNaturalResistance = defaultNaturalResistance
    , udNaturalWeapon = Nothing, udModifiers = []
    }

quadUVs ∷ SortableQuad → [(Float, Float)]
quadUVs q = [ (u, v) | Vertex { tex = Vec2 u v } ←
                  [sqV0 q, sqV1 q, sqV2 q, sqV3 q] ]

quadWidth ∷ SortableQuad → Float
quadWidth q =
    let xs = [ x | Vertex { pos = Vec2 x _ } ←
                   [sqV0 q, sqV1 q, sqV2 q, sqV3 q] ]
    in maximum xs - minimum xs

quadHeight ∷ SortableQuad → Float
quadHeight q =
    let ys = [ y | Vertex { pos = Vec2 _ y } ←
                   [sqV0 q, sqV1 q, sqV2 q, sqV3 q] ]
    in maximum ys - minimum ys

renderQuad ∷ Double → UnitInstance → Maybe UnitDef → Maybe SortableQuad
renderQuad now inst mDef =
    unitToQuad (const 0) 0 FaceSouth 0 8 1.0 False inst mDef now sheetSizes

-- | At fps 8, elapsed 0.125 s is exactly frame 1 — the SECOND cell, so
--   a sub-rect that silently stayed at the sheet's left edge fails.
frame1Time ∷ Double
frame1Time = 0.125

isRejected ∷ Either AtlasLoadError a → Bool
isRejected (Left _) = True
isRejected _        = False

spec ∷ Spec
spec = do
    describe "Unit.Atlas.Index — a well-formed index" $ do
        it "accepts the canonical document and reports both animations" $
            case parse goodIndex of
                Left e → expectationFailure (T.unpack (renderAtlasLoadError e))
                Right anims → map aaName anims `shouldBe` ["idle", "swing"]

        it "reads geometry, playback, and both digests verbatim" $
            case parse goodIndex of
                Right (idle:_) → do
                    aaFormat idle `shouldBe` AtlasFormatPng
                    aaPath idle `shouldBe`
                        "assets/textures/units/acolyte/atlas/idle.png"
                    (aaCellWidth idle, aaCellHeight idle) `shouldBe` (32, 48)
                    (aaColumns idle, aaRows idle) `shouldBe` (4, 5)
                    aaFlip idle `shouldBe` True
                    aaFps idle `shouldBe` 8
                    aaLoop idle `shouldBe` True
                    aaSourceDigest idle `shouldBe` "aaaa"
                    aaAtlasDigest idle `shouldBe` "bbbb"
                other → expectationFailure ("expected idle first, got " ⧺ show (fmap (map aaName) other))

        it "reads each direction's OWN row and real frame count" $
            case parse goodIndex of
                Right [_, swing] → do
                    Map.lookup DirNW (aaDirections swing)
                        `shouldBe` Just (AtlasDirectionRow DirNW 3 1)
                    Map.lookup DirS (aaDirections swing)
                        `shouldBe` Just (AtlasDirectionRow DirS 0 6)
                    -- Rows are NOT re-derived from a direction order:
                    -- `east` sits on row 6 because the document says so.
                    Map.lookup DirE (aaDirections swing)
                        `shouldBe` Just (AtlasDirectionRow DirE 6 4)
                other → expectationFailure ("expected two animations, got " ⧺ show (fmap (map aaName) other))

        it "does not require an animation to author all eight directions" $
            case parse goodIndex of
                Right (idle:_) → Map.size (aaDirections idle) `shouldBe` 5
                _ → expectationFailure "expected idle"

    describe "Unit.Atlas.Index — a malformed index is rejected, never sampled" $ do
        it "rejects bytes that are not JSON" $
            parse "not json at all" `shouldReject` "not valid JSON"

        it "rejects a truncated document" $
            parse (BL.take 60 goodIndex) `shouldReject` "not valid JSON"

        it "rejects an unsupported schema_version" $
            parse (indexWith [("schema_version", "2")] [idleFields])
                `shouldReject` "unsupported index schema_version 2"

        it "rejects an unsupported digest_algorithm" $
            parse (indexWith [("digest_algorithm", str "md5")] [idleFields])
                `shouldReject` "unsupported digest_algorithm"

        it "rejects an index belonging to another unit" $
            parse (indexWith [("unit", str "bear_brown")] [idleFields])
                `shouldReject` "declares unit 'bear_brown'"

        it "rejects an index that declares no animations" $
            parse (indexWith [] []) `shouldReject` "no animations"

        it "rejects duplicate animation names" $
            parse (indexWith [] [idleFields, idleFields])
                `shouldReject` "duplicate animation names"

        it "rejects a missing required field" $
            parse (indexWith [] [dropField "cell_width" idleFields])
                `shouldReject` "malformed"

        it "rejects an unsupported storage format" $
            parse (indexWith [] [setField "storage_format" (str "ktx2") idleFields])
                `shouldReject` "unsupported storage_format 'ktx2'"

        it "names the unit, the animation, AND the artifact" $ do
            let msg = rejection (parse (indexWith []
                          [setField "storage_format" (str "ktx2") idleFields]))
            msg `shouldSatisfy` T.isInfixOf "acolyte"
            msg `shouldSatisfy` T.isInfixOf "idle"
            msg `shouldSatisfy` T.isInfixOf "index.json"

        it "rejects a non-positive dimension" $ do
            parse (indexWith [] [setField "cell_width" "0" idleFields])
                `shouldReject` "cell_width must be positive"
            parse (indexWith [] [setField "atlas_height" "-48" idleFields])
                `shouldReject` "atlas_height must be positive"

        it "rejects a grid that would address pixels outside the atlas" $ do
            parse (indexWith [] [setField "columns" "5" idleFields])
                `shouldReject` "exceeds atlas_width"
            parse (indexWith [] [setField "rows" "6" idleFields])
                `shouldReject` "exceeds atlas_height"

        it "rejects a non-positive or non-finite fps" $ do
            parse (indexWith [] [setField "fps" "0" idleFields])
                `shouldReject` "fps must be a positive finite number"
            parse (indexWith [] [setField "fps" "-8" idleFields])
                `shouldReject` "fps must be a positive finite number"
            -- JSON has no infinity literal, but an exponent this large
            -- decodes to one in the Float the engine holds.
            parse (indexWith [] [setField "fps" "1e400" idleFields])
                `shouldReject` "fps must be a positive finite number"

        it "rejects an unknown direction name" $
            parse (indexWith []
                [setField "directions"
                    (arr [directionEntry "up" 0 4]) idleFields])
                `shouldReject` "unknown direction 'up'"

        it "rejects a row outside the animation's row count" $
            parse (indexWith []
                [setField "directions"
                    (arr [directionEntry "south" 5 4]) idleFields])
                `shouldReject` "outside the animation's 5 rows"

        it "rejects two directions sharing one row" $
            parse (indexWith []
                [setField "directions"
                    (arr [ directionEntry "south" 0 4
                         , directionEntry "north" 0 4 ]) idleFields])
                `shouldReject` "same row"

        it "rejects a duplicated direction" $
            parse (indexWith []
                [setField "directions"
                    (arr [ directionEntry "south" 0 4
                         , directionEntry "south" 1 4 ]) idleFields])
                `shouldReject` "more than once"

        -- D-5: the real count is the frame authority, so a count above
        -- the row's capacity would make padding — or off-sheet pixels —
        -- addressable as a frame.
        it "rejects a frame_count above the row capacity" $
            parse (indexWith []
                [setField "directions"
                    (arr [directionEntry "south" 0 5]) idleFields])
                `shouldReject` "above the animation's 4 columns"

        it "rejects a zero frame_count" $
            parse (indexWith []
                [setField "directions"
                    (arr [directionEntry "south" 0 0]) idleFields])
                `shouldReject` "frame_count 0"

        it "rejects an animation with no directions at all" $
            parse (indexWith [] [setField "directions" (arr []) idleFields])
                `shouldReject` "no directions"

        it "rejects an empty digest" $
            parse (indexWith [] [setField "atlas_digest" (str "") idleFields])
                `shouldReject` "atlas_digest is empty"

        -- A corrupt index must not be able to make the engine load an
        -- arbitrary file.
        it "rejects an atlas_path that escapes the unit's atlas directory" $ do
            let esc p = parse (indexWith []
                    [setField "atlas_path" (str p) idleFields])
            esc "assets/textures/units/acolyte/atlas/../../../secret.png"
                `shouldReject` "not a plain file"
            esc "/etc/passwd" `shouldReject` "not a plain file"
            esc "assets/textures/units/bear_brown/atlas/idle.png"
                `shouldReject` "not a plain file"
            esc "assets/textures/units/acolyte/atlas/nested/idle.png"
                `shouldReject` "not a plain file"

    describe "Unit.Atlas.Index — the content digest matches the compiler" $ do
        -- Reference values produced by tools/pack_atlas.py's own
        -- `content_digest`, so this pins the CROSS-LANGUAGE agreement
        -- rather than only self-consistency.
        it "reproduces pack_atlas.py's digest for a 2x1 sheet" $
            atlasContentDigest 2 1 (BS.pack [0 .. 7]) `shouldBe`
                "725b97fc0e24ce6ac14542dbef5e3fc34cf1c69a50d74246cfb12e62b3b0ab28"

        it "reproduces pack_atlas.py's digest for the 4x2 fixture" $
            atlasContentDigest fixtureW fixtureH fixturePixels `shouldBe`
                "cffb62ffd8c8c770709b2e0a405625b55c0bc9855e25f13f4d67a4e27153d6cb"

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
            validateAtlasImage "acolyte" anim fixtureW fixtureH fixturePixels
                `shouldBe` Right ()

        it "rejects a decoded image whose dimensions differ" $
            validateAtlasImage "acolyte" anim 8 2 (BS.replicate 64 0)
                `shouldReject` "but the index declares 4x2"

        it "rejects a buffer that is not RGBA8 of that size" $
            validateAtlasImage "acolyte" anim fixtureW fixtureH
                (BS.take 8 fixturePixels)
                `shouldReject` "expected 32 RGBA8 bytes"

        it "rejects tampered pixels" $
            let tampered = BS.pack (0xFF : drop 1 (BS.unpack fixturePixels))
            in validateAtlasImage "acolyte" anim fixtureW fixtureH tampered
                `shouldReject` "does not match the index's"

        it "names the unit, the animation and the ATLAS file, not the index" $ do
            let msg = rejection (validateAtlasImage "acolyte" anim 8 2
                          (BS.replicate 64 0))
            msg `shouldSatisfy` T.isInfixOf "acolyte"
            msg `shouldSatisfy` T.isInfixOf "clip"
            msg `shouldSatisfy` T.isInfixOf "clip.png"

    describe "Unit.Atlas.Index — atlas mode selection" $ do
        let (idle, swing) = case parse goodIndex of
                Right [a, b] → (a, b)
                other → error ("fixture index must parse to two animations: "
                               ⧺ show (fmap (map aaName) other))
            yaml = Map.fromList
                [ ("idle",  YamlAnimFacts 8 True True)
                , ("swing", YamlAnimFacts 12 False False)
                , ("walk",  YamlAnimFacts 8 True True) ]

        it "selects exactly the animations the index declares" $
            case planUnitAtlasStorage "acolyte" yaml [idle, swing] of
                Left e → expectationFailure (T.unpack (renderAtlasLoadError e))
                Right m → do
                    -- One entry per indexed animation: the loader
                    -- allocates one handle and queues one upload each,
                    -- so this IS the "one atlas per animation" count.
                    HM.keys m `shouldMatchList` ["idle", "swing"]
                    -- `walk` is declared in YAML but not compiled, so it
                    -- stays legacy rather than being invented.
                    HM.lookup "walk" m `shouldBe` Nothing

        it "an index-free unit selects nothing at all" $
            planUnitAtlasStorage "acolyte" yaml [] `shouldBe` Right HM.empty

        it "rejects an animation the YAML no longer declares" $
            planUnitAtlasStorage "acolyte" (Map.delete "swing" yaml)
                [idle, swing]
                `shouldReject` "YAML does not"

        it "rejects an index whose fps predates a YAML edit" $
            planUnitAtlasStorage "acolyte"
                (Map.insert "idle" (YamlAnimFacts 10 True True) yaml)
                [idle, swing]
                `shouldReject` "index fps"

        it "rejects an index whose loop flag predates a YAML edit" $
            planUnitAtlasStorage "acolyte"
                (Map.insert "idle" (YamlAnimFacts 8 False True) yaml)
                [idle, swing]
                `shouldReject` "index loop"

        it "rejects an index whose flip flag predates a YAML edit" $
            planUnitAtlasStorage "acolyte"
                (Map.insert "idle" (YamlAnimFacts 8 True False) yaml)
                [idle, swing]
                `shouldReject` "index flip"

        -- No partial publication: one bad animation rejects the whole
        -- unit rather than returning the good ones, so the caller never
        -- registers half a unit.
        it "returns nothing at all when ONE animation is stale" $
            planUnitAtlasStorage "acolyte" (Map.delete "swing" yaml)
                [idle, swing]
                `shouldSatisfy` isRejected

    describe "Unit.Atlas — the render quad uses cell geometry, not the sheet" $ do
        it "sizes the quad from the CELL even though the sheet is wider" $ do
            let atlasQ = renderQuad frame1Time
                             (testInstance { uiCurrentAnim = "clip" })
                             (Just atlasDef)
                legacyQ = renderQuad 0 testInstance Nothing
            case (atlasQ, legacyQ) of
                (Just aq, Just lq) → do
                    -- The legacy T-pose texture IS 2x2 (the cell size),
                    -- so an atlas frame of the same logical size must
                    -- produce the identical quad size. Measuring the 4x2
                    -- sheet instead would double the width.
                    quadWidth aq `shouldBe` quadWidth lq
                    quadHeight aq `shouldBe` quadHeight lq
                _ → expectationFailure "expected both quads"

        it "emits the frame's own UV sub-rect on the vertices" $
            case renderQuad frame1Time
                     (testInstance { uiCurrentAnim = "clip" })
                     (Just atlasDef) of
                Just q → quadUVs q `shouldBe`
                    -- Frame 1 of a two-column sheet: u 0.5..1, v 0..1.
                    [(0.5, 0), (1, 0), (1, 1), (0.5, 1)]
                Nothing → expectationFailure "expected a quad"

        it "a legacy frame still spans its whole image, byte for byte" $
            case renderQuad 0 testInstance Nothing of
                Just q → quadUVs q `shouldBe` [(0, 0), (1, 0), (1, 1), (0, 1)]
                Nothing → expectationFailure "expected a quad"

        it "a mirrored atlas frame swaps U WITHIN its own cell" $ do
            let mirrored = atlasSample { fsFlipX = True }
            -- The renderer's assignment, restated: left vertices take
            -- the sub-rect's right edge. Mirroring across the sheet
            -- (1-u) would give 0.5 and 0 — a DIFFERENT cell's texels.
            let (u0, _, u1, _) = fsUV mirrored
            (u1, u0) `shouldBe` (1, 0.5)

    describe "Unit.Atlas — the hit rect uses cell geometry too" $ do
        it "click and box selection share ONE rect helper" $
            -- Both hitTestUnitAt and hitTestUnitsInRect call this; a
            -- single implementation is what stops them drifting.
            unitHitRect FaceSouth 0 sheetSizes atlasSample testInstance
                `shouldBe`
                unitHitRect FaceSouth 0 sheetSizes legacySample testInstance

        it "sizes from the cell, not the whole sheet" $ do
            let (_, _, wCell, hCell) =
                    unitHitRect FaceSouth 0 sheetSizes atlasSample testInstance
                -- A sample that reported no cell size falls through to
                -- the sheet's own 4x2 entry — visibly wider.
                sheetSample = atlasSample { fsCell = Nothing }
                (_, _, wSheet, _) =
                    unitHitRect FaceSouth 0 sheetSizes sheetSample testInstance
            wSheet `shouldSatisfy` (> wCell)
            (wCell, hCell) `shouldSatisfy` \(a, b) → a > 0 ∧ b > 0

        it "agrees with the render quad's size for the same frame" $
            case renderQuad frame1Time
                     (testInstance { uiCurrentAnim = "clip" })
                     (Just atlasDef) of
                Just q →
                    let (_, _, wq, hq) = unitHitRect FaceSouth 0 sheetSizes
                                             atlasSample testInstance
                    in (wq, hq) `shouldBe` (quadWidth q, quadHeight q)
                Nothing → expectationFailure "expected a quad"

    describe "Unit.Atlas — an atlas cell resolves to its legacy frame's texels" $ do
        it "frame 0 samples exactly the legacy frame 0 image" $ do
            let s0 = frame0Sample
            sampleFrame fixtureW fixtureH fixturePixels (fsUV s0) False
                        fixtureCellW fixtureCellH
                `shouldBe`
                sampleFrame fixtureCellW fixtureCellH (legacyFramePixels 0)
                            wholeImageUV False fixtureCellW fixtureCellH

        it "frame 1 samples exactly the legacy frame 1 image" $
            sampleFrame fixtureW fixtureH fixturePixels (fsUV atlasSample) False
                        fixtureCellW fixtureCellH
                `shouldBe`
                sampleFrame fixtureCellW fixtureCellH (legacyFramePixels 1)
                            wholeImageUV False fixtureCellW fixtureCellH

        it "a MIRRORED atlas cell matches the mirrored legacy frame" $
            sampleFrame fixtureW fixtureH fixturePixels (fsUV atlasSample) True
                        fixtureCellW fixtureCellH
                `shouldBe`
                sampleFrame fixtureCellW fixtureCellH (legacyFramePixels 1)
                            wholeImageUV True fixtureCellW fixtureCellH

        it "frame 0 and frame 1 are genuinely different pixels" $ do
            let s0 = frame0Sample
            sampleFrame fixtureW fixtureH fixturePixels (fsUV s0) False
                        fixtureCellW fixtureCellH
                `shouldNotBe`
                sampleFrame fixtureW fixtureH fixturePixels (fsUV atlasSample)
                            False fixtureCellW fixtureCellH

    -- The UI half of the #887 flip×clip ordering, now that a sprite can
    -- be a sub-rect: a clip may only HIDE part of a mirrored atlas cell,
    -- never change which texel a screen position shows, and it must
    -- never widen the sample back to the whole sheet.
    describe "Unit.Atlas — a clipped mirrored atlas cell keeps its texel mapping" $ do
        let cell = (0.5, 0, 1, 1) ∷ UVRect
            sprite flipX clip = renderSpriteBatch (TextureHandle 900) (1, 1, 1, 1)
                cell flipX 50 0 100 50 (LayerId 0) clip
            uAt pick b = pick [ u | Vertex { tex = Vec2 u _ } ←
                                    SV.toList (rbVertices b) ]

        it "an unclipped mirrored cell spans the cell backwards" $
            case V.toList (fst (sprite True Nothing)) of
                [b] → (uAt maximum b, uAt minimum b) `shouldBe` (1, 0.5)
                other → expectationFailure ("expected one batch, got " ⧺ show (length other))

        it "a half clip keeps the left edge and lands on the cell's midpoint" $
            case (V.toList (fst (sprite True Nothing))
                 , V.toList (fst (sprite True (Just (0, 0, 100, 100))))) of
                ([_], [c]) → do
                    -- Screen x 50..150 mirrored samples u=1 at x=50 and
                    -- u=0.5 at x=150, so u=0.75 at x=100. Clipping to
                    -- x<=100 must leave exactly 0.75..1 — the cell's own
                    -- midpoint, NOT 0 or 0.5 (which is what reflecting
                    -- across the whole image, or reversing the surviving
                    -- interval, would produce).
                    uAt maximum c `shouldBe` 1
                    uAt minimum c `shouldBe` 0.75
                other → expectationFailure ("expected one batch each, got " ⧺ show other)

        it "an unmirrored clipped cell stays inside the cell as well" $
            case V.toList (fst (sprite False (Just (0, 0, 100, 100)))) of
                [b] → do
                    uAt minimum b `shouldBe` 0.5
                    uAt maximum b `shouldBe` 0.75
                other → expectationFailure ("expected one batch, got " ⧺ show (length other))

        it "a whole-image sprite is unchanged by the generalization" $
            case V.toList (fst (renderSpriteBatch (TextureHandle 1) (1, 1, 1, 1)
                     wholeImageUV True 50 0 100 50 (LayerId 0) Nothing)) of
                [b] → (uAt maximum b, uAt minimum b) `shouldBe` (1, 0)
                other → expectationFailure ("expected one batch, got " ⧺ show (length other))

    -- D-6: unit art is nearest-neighbour, and an atlas must STAY that
    -- way when the player toggles the global texture filter — a sheet
    -- resampled bilinearly would additionally bleed neighbouring cells
    -- across every frame edge. The upload path registers atlas slots
    -- through `registerPinnedTexture`, which is exactly "put the handle
    -- in btsPinned"; `planFilterRebind` is the pure decision
    -- `setTextureFilter` then makes, so this gates the outcome without a
    -- device. (The single mip level is structural: the shared image
    -- allocator creates one, and the atlas path reuses it unchanged.)
    describe "Unit.Atlas — an atlas slot stays nearest across a filter toggle" $ do
        let atlasHandle' = TextureHandle 900
            ordinary     = TextureHandle 901
            atlasView    = ImageView 0xA11A5
            ordinaryView = ImageView 0x0DD
            nearest      = Sampler 0x0E4E57
            newGlobal    = Sampler 0x11EA4
            handleMap = Map.fromList
                [ (atlasHandle', BindlessTextureHandle (TextureSlot 1 0) atlasHandle')
                , (ordinary,     BindlessTextureHandle (TextureSlot 2 0) ordinary) ]
            imageViews = Map.fromList
                [ (atlasHandle', atlasView), (ordinary, ordinaryView) ]
            plan = planFilterRebind handleMap imageViews
                       (Map.singleton atlasHandle' nearest) newGlobal

        it "repaints the ordinary slot but not the pinned atlas slot" $
            frpRebinds plan `shouldBe`
                [ SlotRebind 1 atlasView nearest
                , SlotRebind 2 ordinaryView newGlobal ]

        it "leaves nothing unrecoverable" $
            frpUnrecoverable plan `shouldBe` []
