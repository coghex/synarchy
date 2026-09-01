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
import qualified Codec.Picture as JP
import qualified Data.Text as T
import qualified Data.Vector as V
import qualified Data.Vector.Storable as SV
import Engine.Asset.Handle (TextureHandle(..))
import Engine.Graphics.Camera (CameraFacing(..))
import Control.Exception (finally)
import Engine.Scripting.Lua.Message.Texture
    (UploadSampler(..), cacheEntryReusable)
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
import System.Directory
    ( createDirectoryIfMissing, getTemporaryDirectory, removeDirectoryRecursive
    , removeFile, renameFile )
import System.FilePath ((</>), takeDirectory)
import UI.Manager
    ( createPage, createSprite, getElement, setSpriteFrame, setSpriteTexture )
import UI.Types
    ( UIElement(..), UILayer(..), UIRenderData(..), UISpriteStyle(..)
    , emptyUIPageManager )
import Unit.Atlas.Load (loadUnitAtlasIndexIn)
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
    -- Padded strides (#2076): 4 columns of (32+2) and 5 rows of (48+2).
    , ("atlas_width", "136"), ("atlas_height", "250")
    , ("cell_width", "32"), ("cell_height", "48"), ("cell_padding", "1")
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
    , ("atlas_width", "204"), ("atlas_height", "400")
    , ("cell_width", "32"), ("cell_height", "48"), ("cell_padding", "1")
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
    ([ ("schema_version", "2")
     , ("generator", str "tools/pack_atlas.py")
     , ("tool_version", "2")
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

-- | The canonical document with one top-level field removed.
indexWithout ∷ Text → BL.ByteString
indexWithout field =
    BLC.pack ∘ T.unpack ∘ obj ∘ dropField field $
        [ ("schema_version", "2")
        , ("generator", str "tools/pack_atlas.py")
        , ("tool_version", "2")
        , ("digest_algorithm", str "sha256")
        , ("unit", str "acolyte")
        , ("direction_order", arr (map str
            [ "south", "south-west", "west", "north-west"
            , "north", "north-east", "east", "south-east" ]))
        , ("animations", arr [obj idleFields])
        ]

goodIndex ∷ BL.ByteString
goodIndex = indexWith [] [idleFields, swingFields]

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

-- | An 8x4 RGBA8 sheet holding two 2x2 cells side by side on one row at
--   the #2076 padded stride: each cell sits at the centre of its own
--   4x4 slot, surrounded by a one-texel gutter that copies the cell's
--   own edge texels outward.
--
--   The two cells hold DIFFERENT art, so the gutter between them is
--   what a linear tap near either cell's inner edge would otherwise
--   cross — which is exactly what the isolation gate below measures.
fixtureW, fixtureH, fixtureCellW, fixtureCellH ∷ Int
fixtureCellW = 2
fixtureCellH = 2
fixtureW = fixtureCols * fixtureSlotW
fixtureH = fixtureRows * fixtureSlotH

fixtureCellPad, fixtureSlotW, fixtureSlotH, fixtureCols, fixtureRows ∷ Int
fixtureCellPad = 1
fixtureSlotW = fixtureCellW + 2 * fixtureCellPad
fixtureSlotH = fixtureCellH + 2 * fixtureCellPad
fixtureCols = 2
fixtureRows = 1

-- | Compose a padded, extruded sheet from a per-slot source frame —
--   @tools\/pack_atlas.py@'s own @compose_atlas@ layout, so a fixture
--   sheet here is what the compiler would really emit rather than an
--   approximation of it.
--
--   A slot with no frame ('Nothing') stays fully transparent, gutter
--   included: that is the rectangularization padding D-5 leaves
--   unaddressable.
extrudedSheet
    ∷ Int → Int → Int                    -- ^ cell width, cell height, padding
    → Int → Int                          -- ^ columns, rows
    → (Int → Int → Maybe BS.ByteString)  -- ^ row, column → that cell's RGBA8
    → BS.ByteString
extrudedSheet cw ch pad cols rows cellAt = BS.pack
    [ b | y ← [0 .. rows * sh - 1], x ← [0 .. cols * sw - 1], b ← texel x y ]
  where
    sw = cw + 2 * pad
    sh = ch + 2 * pad
    texel x y =
        let (col, lx) = x `divMod` sw
            (row, ly) = y `divMod` sh
            -- Clamping BOTH axes into the cell IS the extrusion rule:
            -- an edge texel for a side, and the single corner texel for
            -- a corner square.
            cx = max 0 (min (cw - 1) (lx - pad))
            cy = max 0 (min (ch - 1) (ly - pad))
        in case cellAt row col of
            Nothing → [0, 0, 0, 0]
            Just px → let o = (cy * cw + cx) * 4
                      in [ BS.index px (o + i) | i ← [0 .. 3] ]

fixturePixels ∷ BS.ByteString
fixturePixels = extrudedSheet fixtureCellW fixtureCellH fixtureCellPad
    fixtureCols fixtureRows (\_ col → Just (legacyFramePixels col))

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
--   one row (DirS, two frames), at the #2076 padded stride.
fixtureAtlas ∷ AtlasAnimation
fixtureAtlas = AtlasAnimation
    { aaName = "clip", aaFormat = AtlasFormatPng
    , aaPath = "assets/textures/units/acolyte/atlas/clip.png"
    , aaAtlasWidth = fixtureW, aaAtlasHeight = fixtureH
    , aaCellWidth = fixtureCellW, aaCellHeight = fixtureCellH
    , aaCellPadding = fixtureCellPad
    , aaColumns = fixtureCols, aaRows = fixtureRows
    , aaFlip = False, aaFps = 8, aaLoop = True
    , aaDirections = Map.singleton DirS (AtlasDirectionRow DirS 0 2)
    , aaSourceDigest = "src", aaAtlasDigest = "atlas"
    }

fixtureStorage ∷ AnimStorage
fixtureStorage = StorageAtlas (ResidentAtlas fixtureAtlas (TextureHandle 900))

-- | The fixture's frame-1 cell in UV, and the row's V span — derived
--   from the geometry rather than written out, so a stride change moves
--   the expectations with the layout instead of leaving stale literals.
frame1U0, frame1U1, cellV0, cellV1 ∷ Float
frame1U0 = fromIntegral (fixtureSlotW + fixtureCellPad)
             / fromIntegral fixtureW
frame1U1 = fromIntegral (fixtureSlotW + fixtureCellPad + fixtureCellW)
             / fromIntegral fixtureW
cellV0 = fromIntegral fixtureCellPad / fromIntegral fixtureH
cellV1 = fromIntegral (fixtureCellPad + fixtureCellH) / fromIntegral fixtureH

-- | The two SOURCE frames, as standalone 2x2 images — now the fixture's
--   primary art, with the sheet composed FROM them by 'extrudedSheet'
--   rather than sliced out of it. Every texel is distinct within a
--   frame and the two frames disagree everywhere, so a wrong sub-rect
--   resolves to visibly different bytes rather than coincidentally
--   matching.
legacyFramePixels ∷ Int → BS.ByteString
legacyFramePixels col = BS.pack
    [ b | y ← [0 .. fixtureCellH - 1], x ← [0 .. fixtureCellW - 1]
        , b ← [ fromIntegral (col * 96 + x * 16 + y * 4 + 1)
              , fromIntegral (255 - (col * 96 + x * 16 + y * 4))
              , fromIntegral ((x * 7 + y * 13 + col * 41) `mod` 256)
              , 255 ] ]

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

    -- D-10 keeps WHICH encoding an atlas uses behind an explicit,
    -- closed set rather than an inferred file extension, so deferred
    -- TEX-5's KTX2 slots in as a constructor. What that boundary owes
    -- TODAY, with PNG the only representation, is: accept the one
    -- token this build emits, refuse everything else outright, and
    -- never substitute a fallback for a representation it cannot read.
    -- The last part is the load-bearing one — a loader that quietly
    -- skipped an unreadable animation, or guessed PNG from the path,
    -- would publish a unit missing art and look healthy doing it.
    describe "Unit.Atlas — the format-neutral storage boundary (D-10)" $ do
        let withFormat v = indexWith [] [setField "storage_format" v idleFields]

        it "accepts the token pack_atlas.py emits, and reads it per \
           \animation" $
            -- Per ANIMATION, not once per unit: the index records a
            -- format on every record, which is the shape that lets one
            -- session hold different representations for different
            -- animations when TEX-5 lands.
            case parse goodIndex of
                Right anims → map aaFormat anims
                    `shouldBe` replicate (length anims) AtlasFormatPng
                Left e → expectationFailure (T.unpack (renderAtlasLoadError e))

        it "round-trips its own name, so the token it emits is the token \
           \it accepts" $
            atlasStorageFormatName AtlasFormatPng `shouldBe` "png"

        it "refuses every unknown representation rather than choosing one" $
            forM_ ["ktx2", "basis", "dds", "astc", "raw", ""] $ \token →
                parse (withFormat (str token))
                    `shouldReject` ("unsupported storage_format '"
                                    <> token <> "'")

        it "is an exact token, never case-folded or trimmed" $
            -- A tolerant match here would be a silent second spelling
            -- of a format, and the compiler emits exactly one.
            forM_ ["PNG", "Png", " png", "png "] $ \token →
                parse (withFormat (str token))
                    `shouldReject` "unsupported storage_format"

        it "refuses a non-string representation rather than coercing it" $
            forM_ ["1", "true", "null", "[\"png\"]", "{}"] $ \token →
                parse (withFormat token) `shouldReject` "malformed"

        it "never infers the representation from the atlas path" $ do
            -- The artifact really is the .png the compiler wrote; only
            -- the DECLARED format is unknown. An extension-sniffing
            -- fallback would accept this, which is exactly the guess
            -- D-10 forbids.
            let doc = indexWith [] [setField "storage_format" (str "ktx2")
                                        idleFields]
            lookup "atlas_path" idleFields
                `shouldBe` Just (str "assets/textures/units/acolyte/atlas/idle.png")
            parse doc `shouldReject` "unsupported storage_format 'ktx2'"

        it "rejects the WHOLE unit when one animation's representation is \
           \unreadable" $ do
            -- No partial publication and no synthetic fallback: an
            -- index whose OTHER animation is a perfectly good PNG must
            -- still yield nothing, or a unit would register missing an
            -- animation its YAML declares.
            let mixed = indexWith []
                    [idleFields, setField "storage_format" (str "ktx2")
                                     swingFields]
            parse mixed `shouldReject` "unsupported storage_format 'ktx2'"
            rejection (parse mixed) `shouldSatisfy` T.isInfixOf "swing"

        it "names the animation whose representation it could not read" $ do
            -- The first record is the good one, so a diagnostic naming
            -- 'idle' would be reporting the wrong animation.
            let msg = rejection (parse (indexWith []
                          [idleFields, setField "storage_format" (str "ktx2")
                                           swingFields]))
            msg `shouldSatisfy` T.isInfixOf "swing"
            msg `shouldSatisfy` not ∘ T.isInfixOf "'idle'"

    describe "Unit.Atlas.Index — a malformed index is rejected, never sampled" $ do
        it "rejects bytes that are not JSON" $
            parse "not json at all" `shouldReject` "not valid JSON"

        it "rejects a truncated document" $
            parse (BL.take 60 goodIndex) `shouldReject` "not valid JSON"

        it "rejects an unsupported schema_version" $
            parse (indexWith [("schema_version", "3")] [idleFields])
                `shouldReject` "unsupported index schema_version 3"

        -- #2076's format bump, tested against a document that really is
        -- the previous schema — edge-adjacent dimensions and no
        -- `cell_padding` at all — not merely a v2 one with the number
        -- changed. The VERSION must be the reported cause: the field
        -- v1 legitimately lacks is exactly what a decode-then-check
        -- order would blame instead, which would send a reader looking
        -- for a corrupt index rather than an outdated one.
        it "rejects a genuine schema-v1 index on its VERSION, not its fields" $ do
            let v1 = indexWith
                    [("schema_version", "1"), ("tool_version", "1")]
                    [ dropField "cell_padding"
                        (setField "atlas_width" "128"
                            (setField "atlas_height" "240" idleFields)) ]
                msg = rejection (parse v1)
            msg `shouldSatisfy` T.isInfixOf "unsupported index schema_version 1"
            msg `shouldSatisfy` T.isInfixOf "pack_atlas.py --compile"
            msg `shouldSatisfy` (not ∘ T.isInfixOf "cell_padding")
            msg `shouldSatisfy` (not ∘ T.isInfixOf "malformed")

        it "rejects an index that omits the required cell_padding" $
            parse (indexWith [] [dropField "cell_padding" idleFields])
                `shouldReject` "malformed"

        it "rejects a cell_padding this build does not implement" $ do
            parse (indexWith [] [setField "cell_padding" "0" idleFields])
                `shouldReject` "cell_padding 0 is not this build's one supported"
            parse (indexWith [] [setField "cell_padding" "2" idleFields])
                `shouldReject` "cell_padding 2 is not this build's one supported"

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

        -- Containment strides by the padded SLOT, not the logical cell.
        -- A sheet sized for four edge-adjacent 32-wide cells (128) is
        -- one texel short of holding four padded ones (136), and the
        -- shortfall is entirely gutter — so a check that measured cells
        -- alone would accept a sheet whose last column's extrusion runs
        -- off the right edge.
        it "measures containment by the padded slot, not the bare cell" $ do
            parse (indexWith [] [setField "atlas_width" "128" idleFields])
                `shouldReject` "exceeds atlas_width"
            parse (indexWith [] [setField "atlas_height" "240" idleFields])
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

        -- D-2 is one atlas per ANIMATION. Two animations naming one
        -- file each validate on their own, and the upload path would
        -- then legitimately alias the second onto the first's image and
        -- bindless slot — two animations reading one sheet.
        it "rejects two animations sharing one atlas_path" $
            let stepAsIdle = setField "atlas_path"
                    (str "assets/textures/units/acolyte/atlas/idle.png")
                    swingFields
            in parse (indexWith [] [idleFields, stepAsIdle])
                `shouldReject` "one atlas_path for more than one animation"

        -- The rule that makes the collision unreachable in the first
        -- place: the file is the animation's own canonical name, which
        -- is exactly what the compiler emits.
        it "rejects an atlas_path that is not the animation's canonical file" $ do
            parse (indexWith [] [setField "atlas_path"
                    (str "assets/textures/units/acolyte/atlas/walk.png")
                    idleFields])
                `shouldReject` "is not this animation's canonical atlas"
            parse (indexWith [] [setField "atlas_path"
                    (str "assets/textures/units/acolyte/atlas/idle.PNG")
                    idleFields])
                `shouldReject` "is not this animation's canonical atlas"

        it "accepts the canonical file the compiler emits" $
            case parse goodIndex of
                Right anims → map aaPath anims `shouldBe`
                    [ "assets/textures/units/acolyte/atlas/idle.png"
                    , "assets/textures/units/acolyte/atlas/swing.png" ]
                Left e → expectationFailure (T.unpack (renderAtlasLoadError e))

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
                    -- Frame 1 of a two-column PADDED sheet: the logical
                    -- cell is x 5..7 of 8 and y 1..3 of 4, so the gutter
                    -- lies outside the quad on every side.
                    [ (frame1U0, cellV0), (frame1U1, cellV0)
                    , (frame1U1, cellV1), (frame1U0, cellV1) ]
                Nothing → expectationFailure "expected a quad"

        it "a legacy frame still spans its whole image, byte for byte" $
            case renderQuad 0 testInstance Nothing of
                Just q → quadUVs q `shouldBe` [(0, 0), (1, 0), (1, 1), (0, 1)]
                Nothing → expectationFailure "expected a quad"

        it "a mirrored atlas frame swaps U WITHIN its own cell" $ do
            let mirrored = atlasSample { fsFlipX = True }
            -- The renderer's assignment, restated: left vertices take
            -- the sub-rect's right edge. Mirroring across the sheet
            -- (1-u) would give 0.375 and 0.125 — the OTHER cell, and at
            -- the padded stride not even a cell boundary.
            let (u0, _, u1, _) = fsUV mirrored
            (u1, u0) `shouldBe` (frame1U1, frame1U0)

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

    -- #2076: the whole point of the gutter. A LINEAR sample taken
    -- anywhere inside a logical cell reaches at most one texel past its
    -- edge, so every texel within one texel of the cell boundary must
    -- hold that cell's own edge colour rather than the neighbour's.
    --
    -- The fixture is a 3x2 grid of DELIBERATELY distinct cells with one
    -- rectangularization slot, so every adjacency this has to survive
    -- is present at once: horizontal and vertical neighbours, all four
    -- sides and all four corners, cells on the sheet's own edge, and an
    -- authored cell beside an unreachable transparent slot.
    describe "Unit.Atlas — a cell's extrusion ring isolates it under linear" $ do
        let cols = 3
            rows = 2
            -- (row, col) -> art, with (1, 2) left as rectangularization
            -- padding: DirN authors two frames in a three-column sheet.
            authored r c = r * cols + c < cols * rows - 1
            art r c = BS.pack
                [ fromIntegral ((r * 91 + c * 37 + x * 19 + y * 7 + ch * 61)
                                    `mod` 251 + 4)
                | y ← [0 .. fixtureCellH - 1], x ← [0 .. fixtureCellW - 1]
                , ch ← [0 .. 3 ∷ Int] ]
            cellAt r c = if authored r c then Just (art r c) else Nothing
            sheetW = cols * fixtureSlotW
            sheetH = rows * fixtureSlotH
            pixels = extrudedSheet fixtureCellW fixtureCellH fixtureCellPad
                         cols rows cellAt
            sheet = DecodedImage sheetW sheetH pixels
            anim = fixtureAtlas
                { aaAtlasWidth = sheetW, aaAtlasHeight = sheetH
                , aaColumns = cols, aaRows = rows
                , aaDirections = Map.fromList
                    [ (DirS, AtlasDirectionRow DirS 0 cols)
                    , (DirN, AtlasDirectionRow DirN 1 (cols - 1)) ] }
            texel x y = [ BS.index pixels ((y * sheetW + x) * 4 + i)
                        | i ← [0 .. 3] ]
            -- The cell's own texel a coordinate extrudes FROM: clamp
            -- into the cell on both axes.
            ownTexel r c lx ly =
                let px = art r c
                    cx = max 0 (min (fixtureCellW - 1) lx)
                    cy = max 0 (min (fixtureCellH - 1) ly)
                    o  = (cy * fixtureCellW + cx) * 4
                in [ BS.index px (o + i) | i ← [0 .. 3] ]

        it "every texel a linear tap can reach belongs to its own cell" $
            forM_ [ (r, c) | r ← [0 .. rows - 1], c ← [0 .. cols - 1]
                           , authored r c ] $ \(r, c) →
                -- The reachable neighbourhood is the cell expanded by
                -- one texel on every side: exactly the slot.
                forM_ [ (lx, ly)
                      | ly ← [negate fixtureCellPad
                                  .. fixtureCellH + fixtureCellPad - 1]
                      , lx ← [negate fixtureCellPad
                                  .. fixtureCellW + fixtureCellPad - 1] ]
                    $ \(lx, ly) → do
                        let x = c * fixtureSlotW + fixtureCellPad + lx
                            y = r * fixtureSlotH + fixtureCellPad + ly
                        ((r, c, lx, ly), texel x y)
                            `shouldBe` ((r, c, lx, ly), ownTexel r c lx ly)

        -- Stated as a NEGATIVE too, so the check above cannot pass by
        -- every cell coincidentally agreeing: each authored cell's ring
        -- must genuinely differ from the neighbour it shields against.
        it "a neighbouring cell's art really is different" $ do
            -- Horizontal: (0,0) beside (0,1).
            art 0 0 `shouldNotBe` art 0 1
            -- Vertical: (0,0) above (1,0).
            art 0 0 `shouldNotBe` art 1 0
            -- Diagonal, which is what the corner squares shield.
            art 0 0 `shouldNotBe` art 1 1

        it "an authored cell beside a transparent padding slot is unaffected" $ do
            -- (1, 2) is the rectangularization slot; (1, 1) is its
            -- authored left neighbour and (0, 2) its authored neighbour
            -- above. Neither may pick up transparency in its own ring.
            let slotTexels = [ texel (2 * fixtureSlotW + i) (fixtureSlotH + j)
                             | j ← [0 .. fixtureSlotH - 1]
                             , i ← [0 .. fixtureSlotW - 1] ]
            -- The unaddressable slot IS fully transparent, gutter and
            -- all — the sheet is rectangular for free.
            all (≡ [0, 0, 0, 0]) slotTexels `shouldBe` True
            -- And its authored neighbours' rings hold their own art,
            -- which the per-cell sweep above has already established
            -- pointwise; restated here on the specific right/bottom
            -- edges that touch the transparent slot.
            forM_ [0 .. fixtureCellH - 1] $ \ly →
                texel (fixtureSlotW + fixtureCellPad + fixtureCellW)
                      (fixtureSlotH + fixtureCellPad + ly)
                    `shouldBe` ownTexel 1 1 fixtureCellW ly
            forM_ [0 .. fixtureCellW - 1] $ \lx →
                texel (2 * fixtureSlotW + fixtureCellPad + lx)
                      (fixtureCellPad + fixtureCellH)
                    `shouldBe` ownTexel 0 2 lx fixtureCellH

        -- Sheet-edge cells have no neighbour to bleed from, but they
        -- still need the ring: without it a linear tap at the outer
        -- edge would clamp or wrap to whatever the sampler decides.
        it "cells on the sheet's own edge carry a full ring too" $ do
            let corner = texel 0 0
            corner `shouldBe` ownTexel 0 0 (negate 1) (negate 1)
            texel (sheetW - 1) 0
                `shouldBe` ownTexel 0 (cols - 1) fixtureCellW (negate 1)

        -- The runtime's own freshness check must ACCEPT this layout and
        -- REJECT a sheet whose ring was not built from the frame — the
        -- half a cell-only comparison cannot see, and the half that
        -- would silently reintroduce bleeding.
        it "validateSourceFrame accepts the ring and rejects a wrong one" $ do
            forM_ [ (DirS, 0, c) | c ← [0 .. cols - 1] ]
                $ \(d, r, c) →
                    validateSourceFrame "acolyte" anim sheet d r c "f.png"
                        (DecodedImage fixtureCellW fixtureCellH (art r c))
                        `shouldBe` Right ()
            -- Rebuild the sheet with cell (0,1)'s gutter taken from its
            -- NEIGHBOUR instead of itself — the exact defect the ring
            -- exists to prevent — leaving every logical cell untouched.
            let bledPixels = BS.pack
                    [ b
                    | y ← [0 .. sheetH - 1], x ← [0 .. sheetW - 1]
                    , let (c, lx) = x `divMod` fixtureSlotW
                          (r, ly) = y `divMod` fixtureSlotH
                          inCell = lx ≥ fixtureCellPad
                                 ∧ lx < fixtureCellPad + fixtureCellW
                                 ∧ ly ≥ fixtureCellPad
                                 ∧ ly < fixtureCellPad + fixtureCellH
                          src = if (r, c) ≡ (0, 1) ∧ not inCell
                                    then art 0 0 else art r c
                          cx = max 0 (min (fixtureCellW - 1)
                                          (lx - fixtureCellPad))
                          cy = max 0 (min (fixtureCellH - 1)
                                          (ly - fixtureCellPad))
                          o = (cy * fixtureCellW + cx) * 4
                    , b ← if authored r c
                              then [ BS.index src (o + i) | i ← [0 .. 3] ]
                              else [0, 0, 0, 0] ]
                bled = DecodedImage sheetW sheetH bledPixels
            -- The LOGICAL cell is still exactly right...
            BS.concat (atlasCellRows anim bled 0 1)
                `shouldBe` BS.concat (atlasCellRows anim sheet 0 1)
            -- ...and the frame is still rejected, on the ring.
            validateSourceFrame "acolyte" anim bled DirS 0 1 "f.png"
                (DecodedImage fixtureCellW fixtureCellH (art 0 1))
                `shouldReject` "extrusion ring"

        -- Requirement 5, restated where it can be measured: the UV rect
        -- that addresses this cell resolves under NEAREST to the cell's
        -- own texels and nothing else, mirrored or not.
        it "NEAREST sampling of the cell's UV reads only the cell" $
            forM_ [ (r, c) | r ← [0 .. rows - 1], c ← [0 .. cols - 1]
                           , authored r c ] $ \(r, c) →
                forM_ [False, True] $ \flipX →
                    sampleFrame sheetW sheetH pixels
                        (atlasCellUV anim r c) flipX
                        fixtureCellW fixtureCellH
                        `shouldBe`
                    sampleFrame fixtureCellW fixtureCellH (art r c)
                        wholeImageUV flipX fixtureCellW fixtureCellH

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

    -- The path cache is keyed by path alone, so reuse across an upload
    -- policy boundary would hand the new handle the wrong sampler.
    describe "Unit.Atlas — the texture cache will not reuse across policies" $ do
        let pinnedMap = Map.singleton (TextureHandle 7) (Sampler 0x0E4E57)
            reuse policy h' = cacheEntryReusable policy pinnedMap h'
        it "an atlas request may reuse only an already-pinned entry" $ do
            reuse UploadPinnedNearest (TextureHandle 7) `shouldBe` True
            -- The regression: an atlas inheriting an ordinary slot would
            -- follow global filter toggles and stop being nearest.
            reuse UploadPinnedNearest (TextureHandle 8) `shouldBe` False
        it "an ordinary request may reuse only an unpinned entry" $ do
            reuse UploadGlobalSampler (TextureHandle 8) `shouldBe` True
            reuse UploadGlobalSampler (TextureHandle 7) `shouldBe` False

    -- The whole generated schema is the contract, not just the parts
    -- this build consumes: a truncated document is truncated.
    describe "Unit.Atlas.Index — every generated top-level field is required" $ do
        it "rejects a document missing any one of them" $
            forM_ [ "schema_version", "generator", "tool_version"
                  , "digest_algorithm", "unit", "direction_order"
                  , "animations" ] $ \field →
                parse (indexWithout field) `shouldSatisfy` isRejected

        it "rejects an empty generator" $
            parse (indexWith [("generator", str "  ")] [idleFields])
                `shouldReject` "generator is empty"

        it "rejects a negative tool_version" $
            parse (indexWith [("tool_version", "-1")] [idleFields])
                `shouldReject` "tool_version -1 is negative"

        it "rejects a non-numeric tool_version rather than defaulting" $
            parse (indexWith [("tool_version", str "one")] [idleFields])
                `shouldReject` "malformed"

        -- The row order is documentation here — rows are read
        -- explicitly — but a document declaring a DIFFERENT order came
        -- from a compiler whose layout this build does not share.
        it "rejects a direction_order that is not this build's row order" $ do
            parse (indexWith [("direction_order", arr (map str
                    [ "south", "west", "south-west", "north-west"
                    , "north", "north-east", "east", "south-east" ]))]
                    [idleFields])
                `shouldReject` "is not this build's row order"
            parse (indexWith [("direction_order", arr (map str
                    ["south", "north"]))] [idleFields])
                `shouldReject` "is not this build's row order"

    -- A frame is a texture AND a sub-rect AND a mirror flag. Publishing
    -- them one at a time lets the render thread, which reads the manager
    -- concurrently, land between the writes and draw the wrong cell —
    -- or the whole sheet.
    describe "Unit.Atlas — a frame is published to the UI in one transition" $ do
        let build =
                let (pg, m1) = createPage "hud" LayerHUD emptyUIPageManager
                    (el, m2) = createSprite "portrait" 32 32
                                   (TextureHandle 1) (1, 1, 1, 1) pg m1
                in (el, m2)
            styleOf el mgr = case getElement el mgr of
                Just e → case ueRenderData e of
                    RenderSprite st → Just (ussTexture st, ussUV st, ussFlipX st)
                    _ → Nothing
                Nothing → Nothing

        it "a fresh sprite starts on the whole image, unmirrored" $
            let (el, mgr) = build
            in styleOf el mgr `shouldBe` Just (TextureHandle 1, (0, 0, 1, 1), False)

        it "setSpriteFrame lands texture, sub-rect and mirror together" $
            let (el, mgr) = build
                mgr' = setSpriteFrame el (TextureHandle 900) (0.5, 0, 1, 1) True mgr
            in styleOf el mgr' `shouldBe`
                Just (TextureHandle 900, (0.5, 0, 1, 1), True)

        it "publishing a whole-image portrait resets the sub-rect and flip" $
            let (el, mgr) = build
                atlasFrame = setSpriteFrame el (TextureHandle 900)
                                 (0.5, 0, 1, 1) True mgr
                portrait = setSpriteFrame el (TextureHandle 42) (0, 0, 1, 1)
                               False atlasFrame
            in styleOf el portrait `shouldBe`
                Just (TextureHandle 42, (0, 0, 1, 1), False)

        -- The regression the atomic verb exists to prevent: the
        -- intermediate state a texture-then-UV sequence passes through
        -- pairs the new atlas handle with the OLD frame's rect, which
        -- for a first switch from a portrait is the entire sheet.
        it "the separate setters really do expose that intermediate state" $
            let (el, mgr) = build
                halfway = setSpriteTexture el (TextureHandle 900) mgr
            in do
                styleOf el halfway `shouldBe`
                    Just (TextureHandle 900, (0, 0, 1, 1), False)
                styleOf el halfway `shouldNotBe`
                    styleOf el (setSpriteFrame el (TextureHandle 900)
                                    (0.5, 0, 1, 1) False mgr)

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
