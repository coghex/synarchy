{-# LANGUAGE Strict #-}
module Engine.Scene.Batch.Text
  ( collectTextBatches
  , groupByFontAndLayer
  , convertToTextBatches
  ) where

import UPrelude
import qualified Data.Vector as V
import qualified Data.Map as Map
import qualified Data.List as List
import qualified Data.Text as T
import Data.IORef (readIORef)
import Engine.Scene.Base (ObjectId, NodeType(..), LayerId(..), Transform2D(..))
import Engine.Scene.Types.Node (SceneNode(..), WorldTransform(..))
import Engine.Scene.Types.Graph (SceneGraph(..))
import Engine.Scene.Types.Batch (TextRenderBatch(..), TextBatch(..))
import Engine.Asset.Handle (FontHandle)
import Engine.Graphics.Font.Data (FontCache(..), FontAtlas, fcFonts)
import Engine.Graphics.Font.Draw (layoutText, layoutTextUI)
import Engine.Graphics.Vulkan.Types.Vertex (Vec4(..))
import Engine.Core.Monad
import Engine.Core.State (EngineEnv(..))
import Engine.Core.Log.Monad (logDebugM, logDebugSM)
import Engine.Core.Log (LogCategory(..))

-- | Collect text batches from scene graph
collectTextBatches ∷ SceneGraph → Float → Float → EngineM ε σ (V.Vector TextRenderBatch)
collectTextBatches graph screenW screenH = do
  let allNodes = Map.elems (sgNodes graph)
      textNodes = filter (\n → nodeType n ≡ TextObject && nodeVisible n) allNodes
  
  logDebugSM CatScene "Text batch collection started"
      [("totalTextNodes", T.pack $ show $ length textNodes)]
  
  cacheRef ← asks fontCacheRef
  cache ← liftIO $ readIORef cacheRef
  let grouped = groupByFontAndLayer textNodes
  batches ← forM grouped $ \((fontHandle, layerId), nodes) → do
    case Map.lookup fontHandle (fcFonts cache) of
      Nothing → do
        logDebugM CatFont $ "Font cache miss: Font " <> T.pack (show fontHandle)
                                     <> " not found in cache."
        return Nothing
      Just atlas → do
          logDebugM CatFont $ "Font cache hit: Found font " <> T.pack (show fontHandle)
          allInstances ← fmap V.concat $ forM nodes $ \node → do
              case (nodeText node, Map.lookup (nodeId node) (sgWorldTrans graph)) of
                  (Just text, Just worldTrans) → do
                      let (x,y) = wtPosition worldTrans
                          Vec4 r g b a = nodeColor node
                          color = (r, g, b, a)
                          size = case nodeFontSize node of
                                        Nothing → 32
                                        Just s → s
                          isUI = let (LayerId l) = layerId in l >= 10
                      let instances = if isUI
                                      then layoutTextUI atlas size x y text color
                                      else layoutText atlas x y screenW screenH text color
                      return instances
                  (Nothing, _) → do
                      logDebugM CatFont $ "      No text for node "
                                        <> T.pack (show (nodeId node))
                      return V.empty
                  (_, Nothing) → do
                      logDebugM CatFont $ "      No world transform for node"
                      return V.empty
          logDebugM CatFont $ "Text layout generated " <> T.pack (show $ V.length allInstances) <> " vertices"
          return $ Just $ TextRenderBatch
              { trbFont = fontHandle
              , trbLayer = layerId
              , trbInstances = allInstances
              , trbObjects = V.fromList $ map nodeId nodes }
  let result = V.fromList $ catMaybes batches
      totalVertices = V.sum $ V.map (V.length . trbInstances) result
  
  logDebugSM CatScene "Text batch generation complete"
      [("batches", T.pack $ show $ V.length result)
      ,("totalVertices", T.pack $ show totalVertices)]
  
  return result

-- | Group text nodes by font and layer
groupByFontAndLayer ∷ [SceneNode] → [((FontHandle, LayerId), [SceneNode])]
groupByFontAndLayer nodes =
  let nodesWithFont = filter (isJust . nodeFont) nodes
      sorted = List.sortOn (\n → (nodeFont n, nodeLayer n, zIndex $ nodeTransform n)) nodesWithFont
      grouped = List.groupBy (\a b → (nodeFont a, nodeLayer a) ≡ (nodeFont b, nodeLayer b)) sorted
      keyed = map (\grp → case grp of
                      [] → error "Empty group"
                      (n:_) → ((fromJust $ nodeFont n, nodeLayer n), grp)) grouped
  in keyed

-- | Convert TextRenderBatch to simplified TextBatch
convertToTextBatches ∷ V.Vector TextRenderBatch → V.Vector TextBatch
convertToTextBatches = V.map $ \trb → TextBatch
    { tbFontHandle = trbFont trb
    , tbInstances = trbInstances trb
    , tbLayer = trbLayer trb }
