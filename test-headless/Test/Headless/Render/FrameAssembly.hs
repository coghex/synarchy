{-# LANGUAGE OverloadedStrings #-}
-- | Frame layer assembly (#2192): where every drawable of a frame lands
--   in the layered map command recording walks, and in what order.
--
--   Before #2192 the frame consumed only the world quads and the UI
--   pages. 'bmTextBatches' was rebuilt every frame and read by nothing,
--   so @engine.spawnText@ drew nothing on any layer; and the one scene
--   extraction the frame did read kept sprites BELOW 'uiLayerThreshold'
--   only, so @engine.spawnSprite@ on a UI layer silently vanished.
--
--   The pure 'assembleLayeredBatches' is now the single merge, and it is
--   driven here with synthetic world-layer sprites, a UI-layer scene
--   sprite, scene text on both sides of the threshold, and UI-page items
--   — including an EQUAL-LAYER collision between a scene sprite, a scene
--   text and a UI page — asserting the exact item and source order the
--   recorder consumes, through the SAME 'layerSprites' \/ 'layerTexts' \/
--   '*InDrawOrder' partitions "Engine.Graphics.Vulkan.Command.Record"
--   and "Engine.Loop.Frame" call. Quads are tagged through their vertex
--   atlas id so a merged batch's contents can be read back; a
--   below-threshold scene sprite is shown to appear EXACTLY once, through
--   the tile interleave, and never as a standalone scene batch.
--
--   'envSpec' then drives the real route for a sprite — a
--   'LuaSpawnSpriteRequest' through 'processLuaMessages', the manager's
--   own per-frame rebuild, then the assembly — and shows @setVisible@ and
--   @destroy@ are reflected in the next rebuild. Text cannot take that
--   route headless ('collectTextBatches' needs a font atlas), which is
--   why the pure half carries synthetic text batches and the offscreen
--   @tools/scene_primitives_probe.py@ owns the pixel proof.
module Test.Headless.Render.FrameAssembly (spec, envSpec) where

import UPrelude
import Test.Hspec
import Control.Exception (bracket)
import qualified Data.Map.Strict as Map
import qualified Data.Vector as V
import qualified Data.Vector.Storable as VS
import Data.IORef (readIORef, writeIORef, atomicModifyIORef')
import Engine.Asset.Handle (FontHandle(..), TextureHandle(..))
import Engine.Core.Monad (EngineM', runEngineM)
import qualified Engine.Core.Queue as Q
import Engine.Core.State (EngineEnv(..), EngineState(..))
import Engine.Graphics.Camera (defaultCamera)
import Engine.Graphics.Font.Data (GlyphInstance(..))
import Engine.Graphics.Vulkan.Types.Vertex (Vertex(..), Vec2(..), Vec4(..), mkVertex)
import Engine.Scene.Assembly
import Engine.Scene.Base (LayerId(..), ObjectId(..))
import Engine.Scene.Batch.Update (updateBatches, updateTextBatches)
import Engine.Scene.Manager (createScene, setActiveScene, updateSceneManager)
import Engine.Scene.Types
import Engine.Scripting.Lua.Message (processLuaMessages)
import Engine.Scripting.Lua.Types (LuaToEngineMsg(..))
import World.Grid (uiLayerThreshold)

-- ---------------------------------------------------------------------
-- Layers
-- ---------------------------------------------------------------------

-- | The world layer the tile interleave is exercised on.
worldLayer0 ∷ LayerId
worldLayer0 = LayerId 0

-- | A second WORLD layer, carrying one static tile and one scene text:
--   the world-then-scene source order below the threshold.
worldLayer3 ∷ LayerId
worldLayer3 = LayerId 3

-- | The UI layer every collision case shares: a scene sprite, a scene
--   text and a UI page all declare it. Derived from the real threshold
--   so the case cannot drift below it.
uiLayerA ∷ LayerId
uiLayerA = let LayerId t = uiLayerThreshold in LayerId (t + 2)

-- | A UI layer only a page draws on.
uiLayerB ∷ LayerId
uiLayerB = let LayerId t = uiLayerThreshold in LayerId (t + 5)

-- ---------------------------------------------------------------------
-- Tagged fixtures
-- ---------------------------------------------------------------------

-- | A vertex whose atlas id is @tag@ — the one payload slot a merged
--   batch keeps per quad, so its contents can be read back in order.
tagged ∷ Float → Vec2 → Vertex
tagged tag p = mkVertex p (Vec2 0 0) (Vec4 1 1 1 1) tag 0

-- | A world quad at @depth@ carrying @tag@. Its rect is offset by the
--   tag too, so no two fixtures tie on 'quadPainterOrder''s rect terms.
worldQuad ∷ LayerId → Float → Float → SortableQuad
worldQuad layer depth tag = SortableQuad
    { sqSortKey = depth
    , sqV0      = tagged tag (Vec2 tag 0)
    , sqV1      = tagged tag (Vec2 (tag + 1) 0)
    , sqV2      = tagged tag (Vec2 (tag + 1) 1)
    , sqV3      = tagged tag (Vec2 tag 1)
    , sqTexture = worldTexture
    , sqLayer   = layer
    }

-- | A visible scene sprite as 'collectVisibleObjects' emits it.
sceneSprite ∷ ObjectId → TextureHandle → LayerId → Float → Float → DrawableObject
sceneSprite oid tex layer depth tag = DrawableObject
    { doId      = oid
    , doTexture = tex
    , doV0      = tagged tag (Vec2 tag 0)
    , doV1      = tagged tag (Vec2 (tag + 1) 0)
    , doV2      = tagged tag (Vec2 (tag + 1) 1)
    , doV3      = tagged tag (Vec2 tag 1)
    , doZIndex  = depth
    , doLayer   = layer
    }

glyph ∷ Float → GlyphInstance
glyph x = GlyphInstance (x, 0) (8, 8) (0, 0, 1, 1) (1, 1, 1, 1)

-- | A text batch as 'collectTextBatches' emits it: one font, one layer,
--   the node ids it laid out.
sceneText ∷ FontHandle → LayerId → ObjectId → Int → TextRenderBatch
sceneText font layer oid n = TextRenderBatch
    { trbFont      = font
    , trbLayer     = layer
    , trbInstances = V.fromList (map (glyph ∘ fromIntegral) [1 .. n])
    , trbObjects   = V.singleton oid
    }

-- | A UI page's items as 'UI.Render.renderUIPages' hands them over:
--   sprites first, then its per-font merged text with NO object ids.
uiPageSprite ∷ TextureHandle → LayerId → RenderBatch
uiPageSprite tex layer = RenderBatch
    { rbTexture  = tex
    , rbLayer    = layer
    , rbVertices = VS.fromList (replicate 6 (tagged 99 (Vec2 0 0)))
    , rbObjects  = V.empty
    , rbDirty    = True
    , rbAvgZ     = 0
    }

uiPageText ∷ FontHandle → LayerId → Int → TextRenderBatch
uiPageText font layer n = TextRenderBatch
    { trbFont      = font
    , trbLayer     = layer
    , trbInstances = V.fromList (map (glyph ∘ fromIntegral) [1 .. n])
    , trbObjects   = V.empty
    }

worldTexture, sceneWorldTexture, sceneUiTexture, pageTexture ∷ TextureHandle
worldTexture      = TextureHandle 1
sceneWorldTexture = TextureHandle 20
sceneUiTexture    = TextureHandle 21
pageTexture       = TextureHandle 30

sceneFont, pageFont ∷ FontHandle
sceneFont = FontHandle 7
pageFont  = FontHandle 9

sceneWorldId, sceneUiId, sceneTextWorldId, sceneTextUiId ∷ ObjectId
sceneWorldId     = ObjectId 200
sceneUiId        = ObjectId 201
sceneTextWorldId = ObjectId 300
sceneTextUiId    = ObjectId 301

-- Tags. The static run is [11, 14], the dynamic rest [15, 13] and the
-- scene sprite 12, so construction order and depth order disagree
-- everywhere a merge could be mistaken for a concatenation.
tagStaticNear, tagScene, tagDynamic, tagStaticFar, tagDynamicTie, tagStatic3 ∷ Float
tagStaticNear = 11   -- depth 1
tagScene      = 12   -- depth 2 (scene sprite, layer 0)
tagDynamic    = 13   -- depth 2.5
tagStaticFar  = 14   -- depth 3
tagDynamicTie = 15   -- depth 1: EXACTLY the near tile's depth
tagStatic3    = 16   -- layer 3's single static tile

-- ---------------------------------------------------------------------
-- The frame under test
-- ---------------------------------------------------------------------

worldQuads ∷ LayeredQuads
worldQuads = emptyLayeredQuads
    { lqStatic = Map.fromList
        [ (worldLayer0, V.fromList [ worldQuad worldLayer0 1 tagStaticNear
                                   , worldQuad worldLayer0 3 tagStaticFar ])
        , (worldLayer3, V.singleton (worldQuad worldLayer3 5 tagStatic3)) ]
    , lqDynamic = V.fromList [ worldQuad worldLayer0 1   tagDynamicTie
                             , worldQuad worldLayer0 2.5 tagDynamic ]
    }

-- | The manager as 'updateSceneForRender' leaves it: sprite batches from
--   the visible objects, text batches from the text nodes.
sceneManagerState ∷ BatchManager
sceneManagerState =
    updateTextBatches
        (V.fromList [ sceneText sceneFont uiLayerA    sceneTextUiId    3
                    , sceneText sceneFont worldLayer3 sceneTextWorldId 2 ])
        (updateBatches
            (V.fromList [ sceneSprite sceneWorldId sceneWorldTexture worldLayer0 2 tagScene
                        , sceneSprite sceneUiId    sceneUiTexture    uiLayerA   0 17 ])
            createBatchManager)

uiPages ∷ Map.Map LayerId (V.Vector RenderItem)
uiPages = Map.fromList
    [ (uiLayerA, V.fromList [ SpriteItem (uiPageSprite pageTexture uiLayerA)
                            , TextItem (uiPageText pageFont uiLayerA 4) ])
    , (uiLayerB, V.singleton (TextItem (uiPageText pageFont uiLayerB 1))) ]

assembled ∷ Map.Map LayerId (V.Vector RenderItem)
assembled = assembleLayeredBatches worldQuads sceneManagerState uiPages

-- ---------------------------------------------------------------------
-- Reading the result back
-- ---------------------------------------------------------------------

-- | What identifies a batch to the recorder, without needing 'Eq' on
--   the vertex payload: kind, texture or font, layer, and object ids.
data ItemKey
    = Sprite TextureHandle LayerId [ObjectId]
    | Text   FontHandle    LayerId [ObjectId]
    deriving (Eq, Show)

spriteKey ∷ RenderBatch → ItemKey
spriteKey b = Sprite (rbTexture b) (rbLayer b) (V.toList (rbObjects b))

textKey ∷ TextRenderBatch → ItemKey
textKey t = Text (trbFont t) (trbLayer t) (V.toList (trbObjects t))

itemKey ∷ RenderItem → ItemKey
itemKey (SpriteItem b) = spriteKey b
itemKey (TextItem t)   = textKey t

-- | The tags of a batch's quads in vertex order — one per six vertices.
quadTags ∷ RenderBatch → [Float]
quadTags b =
    [ atlasId (rbVertices b VS.! (i * 6)) | i ← [0 .. VS.length (rbVertices b) `div` 6 - 1] ]

itemsAt ∷ LayerId → V.Vector RenderItem
itemsAt layer = Map.findWithDefault V.empty layer assembled

-- The keys the fixtures above must produce.
worldBatch0, worldBatch3, sceneUiKey, sceneTextUiKey, sceneTextWorldKey
    , pageSpriteKey, pageTextAKey, pageTextBKey ∷ ItemKey
worldBatch0       = Sprite worldTexture worldLayer0 []
worldBatch3       = Sprite worldTexture worldLayer3 []
sceneUiKey        = Sprite sceneUiTexture uiLayerA [sceneUiId]
sceneTextUiKey    = Text sceneFont uiLayerA [sceneTextUiId]
sceneTextWorldKey = Text sceneFont worldLayer3 [sceneTextWorldId]
pageSpriteKey     = Sprite pageTexture uiLayerA []
pageTextAKey      = Text pageFont uiLayerA []
pageTextBKey      = Text pageFont uiLayerB []

-- ---------------------------------------------------------------------
-- Pure half
-- ---------------------------------------------------------------------

spec ∷ Spec
spec = describe "frame layer assembly (#2192)" $ do
    it "the fixture's collision layer really is a UI layer and its text \
       \layer really is a world layer" $ do
        (uiLayerA ≥ uiLayerThreshold) `shouldBe` True
        (worldLayer3 < uiLayerThreshold) `shouldBe` True

    it "keys the map by exactly the layers the three sources declared" $
        Map.keys assembled `shouldBe` [worldLayer0, worldLayer3, uiLayerA, uiLayerB]

    it "interleaves a below-threshold scene sprite with the tiles by \
       \depth, inside the layer's ONE merged sprite batch" $ do
        let items = itemsAt worldLayer0
        map itemKey (V.toList items) `shouldBe` [worldBatch0]
        -- near tile, the dynamic quad tied with it (ties draw AFTER the
        -- static run), the scene sprite, the dynamic rest, the far tile.
        concatMap quadTags (V.toList (layerSprites items))
            `shouldBe` [tagStaticNear, tagDynamicTie, tagScene, tagDynamic, tagStaticFar]

    it "never duplicates the interleaved scene sprite as a standalone \
       \scene batch anywhere in the frame" $ do
        let everyTag = concatMap quadTags (V.toList (spriteBatchesInDrawOrder assembled))
        length (filter (≡ tagScene) everyTag) `shouldBe` 1
        -- and the complementary split is exact: one quad went to the
        -- interleave, the other sprite is the standalone UI-layer batch.
        V.length (worldSceneQuads sceneManagerState) `shouldBe` 1
        map itemKey (concatMap V.toList (Map.elems (scenePrimitiveItems sceneManagerState)))
            `shouldBe` [sceneTextWorldKey, sceneUiKey, sceneTextUiKey]

    it "draws scene text on a world layer at that layer, after the \
       \layer's world content" $ do
        let items = itemsAt worldLayer3
        map itemKey (V.toList items) `shouldBe` [worldBatch3, sceneTextWorldKey]
        map textKey (V.toList (layerTexts items)) `shouldBe` [sceneTextWorldKey]
        concatMap quadTags (V.toList (layerSprites items)) `shouldBe` [tagStatic3]

    it "orders an equal-layer collision as scene primitives then the UI \
       \page, and the recorder's partition as sprites before texts with \
       \that source order kept inside each kind" $ do
        let items = itemsAt uiLayerA
        map itemKey (V.toList items)
            `shouldBe` [sceneUiKey, sceneTextUiKey, pageSpriteKey, pageTextAKey]
        map spriteKey (V.toList (layerSprites items)) `shouldBe` [sceneUiKey, pageSpriteKey]
        map textKey   (V.toList (layerTexts items))   `shouldBe` [sceneTextUiKey, pageTextAKey]

    it "leaves a UI-page-only layer exactly as the page rendered it" $
        map itemKey (V.toList (itemsAt uiLayerB)) `shouldBe` [pageTextBKey]

    it "lays the vertex buffer out in the recorder's draw order: \
       \ascending layer, sprites in item order within a layer" $
        map spriteKey (V.toList (spriteBatchesInDrawOrder assembled))
            `shouldBe` [worldBatch0, worldBatch3, sceneUiKey, pageSpriteKey]

    it "uploads glyph instances in the recorder's draw order too, so the \
       \per-layer slices stay aligned" $
        map textKey (V.toList (textBatchesInDrawOrder assembled))
            `shouldBe` [sceneTextWorldKey, sceneTextUiKey, pageTextAKey, pageTextBKey]

    it "assembles nothing from nothing" $
        Map.null (assembleLayeredBatches emptyLayeredQuads createBatchManager Map.empty)
            `shouldBe` True

-- ---------------------------------------------------------------------
-- The real sprite route, headless
-- ---------------------------------------------------------------------

liveId ∷ ObjectId
liveId = ObjectId 90210

-- | Queue @msgs@ and drain them through the real per-frame dispatcher.
pump ∷ EngineEnv → [LuaToEngineMsg] → IO ()
pump env msgs = do
    mapM_ (Q.writeQueue (luaToEngineQueue env)) msgs
    let action ∷ EngineM' ()
        action = processLuaMessages
    _ ← runEngineM action env pure
    pure ()

-- | The manager's own per-frame rebuild ('updateSceneManager', which is
--   what 'updateSceneForRender' calls), then the frame's assembly of it
--   with no world quads and no UI pages, so whatever comes out is the
--   scene's alone.
rebuild ∷ EngineEnv → IO (BatchManager, Map.Map LayerId (V.Vector RenderItem))
rebuild env = do
    st ← readIORef (engineStateRef env)
    let action ∷ EngineM' SceneManager
        action = updateSceneManager 800 600 (sceneManager st)
    r ← runEngineM action env pure
    case r of
        Left err  → fail ("updateSceneManager failed: " <> show err)
        Right mgr → do
            atomicModifyIORef' (engineStateRef env) $ \s → (s { sceneManager = mgr }, ())
            let bm = smBatchManager mgr
            pure (bm, assembleLayeredBatches emptyLayeredQuads bm Map.empty)

-- | Run @body@ with the boot-shaped @\"default\"@ scene active, restoring
--   the shared env's scene manager afterward however the body ends.
withActiveScene ∷ EngineEnv → IO α → IO α
withActiveScene env body = bracket install restore (const body)
  where
    install = do
        st ← readIORef (engineStateRef env)
        let mgr = setActiveScene "default"
                    (createScene "default" defaultCamera (sceneManager st))
        writeIORef (engineStateRef env) st { sceneManager = mgr }
        pure (sceneManager st)
    restore mgr =
        atomicModifyIORef' (engineStateRef env) $ \s → (s { sceneManager = mgr }, ())

spawnAt ∷ LayerId → LuaToEngineMsg
spawnAt layer = LuaSpawnSpriteRequest liveId 0 0 8 8 (TextureHandle 5) layer

envSpec ∷ SpecWith EngineEnv
envSpec = describe "frame layer assembly (#2192) through the real sprite route" $ do
    it "a spawned UI-layer sprite is assembled as a standalone item at \
       \its declared layer, and not as a world quad" $ \env →
        withActiveScene env $ do
            pump env [spawnAt uiLayerA]
            (bm, items) ← rebuild env
            V.length (worldSceneQuads bm) `shouldBe` 0
            map itemKey (concatMap V.toList (Map.elems items))
                `shouldBe` [Sprite (TextureHandle 5) uiLayerA [liveId]]

    it "a spawned world-layer sprite reaches the frame only through the \
       \tile interleave" $ \env →
        withActiveScene env $ do
            pump env [spawnAt worldLayer0]
            (bm, items) ← rebuild env
            V.length (worldSceneQuads bm) `shouldBe` 1
            Map.null (scenePrimitiveItems bm) `shouldBe` True
            -- the merged batch carries no object ids: it came through
            -- the interleave, not as the manager's own batch.
            map itemKey (concatMap V.toList (Map.elems items))
                `shouldBe` [Sprite (TextureHandle 5) worldLayer0 []]

    it "setVisible and destroy are reflected in the next rebuild" $ \env →
        withActiveScene env $ do
            pump env [spawnAt uiLayerA]
            (_, shown) ← rebuild env
            Map.keys shown `shouldBe` [uiLayerA]

            pump env [LuaSetVisibleRequest liveId False]
            (_, hidden) ← rebuild env
            Map.null hidden `shouldBe` True

            pump env [LuaSetVisibleRequest liveId True]
            (_, reshown) ← rebuild env
            Map.keys reshown `shouldBe` [uiLayerA]

            pump env [LuaDestroyRequest liveId]
            (_, gone) ← rebuild env
            Map.null gone `shouldBe` True
