{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE Strict, UnicodeSyntax #-}
module Engine.Graphics.Vulkan.Command.Text
  ( renderTextBatches
  , ensureTextInstanceBuffer
  , uploadTextInstances
  ) where

import UPrelude
import qualified Data.Vector as V
import qualified Data.Map as Map
import qualified Data.Text as T
import Data.IORef (readIORef)
import Foreign.Storable (sizeOf, pokeElemOff)
import Foreign.Ptr (castPtr)
import Engine.Core.Log (LogCategory(..))
import Engine.Core.Log.Monad (logDebugM, logDebugSM, logWarnM)
import Engine.Core.Monad
import Engine.Core.State (EngineEnv(..), GraphicsState(..))
import Engine.Graphics.Font.Data (FontCache(..), FontAtlas(..), GlyphInstance, fcFonts)
import Engine.Graphics.Vulkan.BufferUtils (createVulkanBufferManual)
import Engine.Graphics.Vulkan.Types
import Engine.Graphics.Vulkan.Types.Descriptor (DescriptorManager(..))
import Engine.Scene.Types (TextBatch(..), TextRenderBatch(..), TextInstanceBuffer(..))
import Vulkan.Core10
import Vulkan.Zero

-- | Ensure the cached text instance buffer is large enough.
--   Reuses the existing buffer when capacity suffices.
--   Destroys and reallocates (with 50% padding) when it must grow.
--   Uses createVulkanBufferManual — caller owns the lifetime.
ensureTextInstanceBuffer ∷ Device → PhysicalDevice → Word64
                         → Maybe TextInstanceBuffer
                         → EngineM ε σ TextInstanceBuffer
ensureTextInstanceBuffer device pDevice requiredInstances mOld = do
    case mOld of
        Just existing | tibCapacity existing ≥ requiredInstances → do
            logDebugSM CatRender "Reusing text instance buffer"
                [("capacity", T.pack $ show $ tibCapacity existing)
                ,("required", T.pack $ show requiredInstances)]
            pure existing
        _ → do
            -- Destroy old buffer if it exists
            case mOld of
                Just old → do
                    logDebugSM CatRender "Destroying old text instance buffer"
                        [("oldCapacity", T.pack $ show $ tibCapacity old)]
                    liftIO $ do
                        destroyBuffer device (tibBuffer old) Nothing
                        freeMemory device (tibMemory old) Nothing
                Nothing → pure ()

            let !instanceSize = fromIntegral $ sizeOf (undefined ∷ GlyphInstance)
                paddedCapacity = requiredInstances + (requiredInstances `div` 2)
                bufferSize = paddedCapacity * fromIntegral instanceSize

            logDebugSM CatRender "Creating text instance buffer"
                [("instances", T.pack $ show requiredInstances)
                ,("paddedCapacity", T.pack $ show paddedCapacity)
                ,("sizeBytes", T.pack $ show bufferSize)]

            (!memory, !buffer) ← createVulkanBufferManual device pDevice
                (fromIntegral bufferSize)
                BUFFER_USAGE_VERTEX_BUFFER_BIT
                (MEMORY_PROPERTY_HOST_VISIBLE_BIT .|. MEMORY_PROPERTY_HOST_COHERENT_BIT)

            pure $ TextInstanceBuffer
                { tibBuffer   = buffer
                , tibMemory   = memory
                , tibCapacity = paddedCapacity
                , tibUsed     = 0
                }

-- | Upload all glyph instances from all text batches into the cached buffer.
--   Returns the updated buffer and a vector of (firstInstance, instanceCount)
--   offsets, one per input batch, for use in draw calls.
uploadTextInstances ∷ Device → TextInstanceBuffer
                    → V.Vector TextRenderBatch
                    → EngineM ε σ (TextInstanceBuffer, V.Vector (Word32, Word32))
uploadTextInstances device tib batches = do
    let !instanceSize = fromIntegral $ sizeOf (undefined ∷ GlyphInstance)
        -- Pre-compute per-batch (firstInstance, instanceCount)
        offsets = V.prescanl' (\acc trb → acc + fromIntegral (V.length (trbInstances trb))) 0 batches
        counts  = V.map (fromIntegral . V.length . trbInstances) batches
        drawInfos = V.zip offsets counts
        totalInstances = V.sum counts
        totalSize = fromIntegral totalInstances * instanceSize

    logDebugSM CatRender "Uploading text instances"
        [("totalInstances", T.pack $ show totalInstances)
        ,("batches", T.pack $ show $ V.length batches)]

    if totalInstances ≡ 0
        then pure (tib { tibUsed = 0 }, drawInfos)
        else do
            dataPtr ← mapMemory device (tibMemory tib) 0
                          (fromIntegral totalSize) zero

            let !basePtr = castPtr dataPtr
            V.forM_ (V.zip offsets batches) $ \(!off, !trb) →
                liftIO $ V.iforM_ (trbInstances trb) $ \i inst →
                    pokeElemOff basePtr (fromIntegral off + i) inst

            unmapMemory device (tibMemory tib)

            let result = tib { tibUsed = fromIntegral totalInstances }
            pure (result, drawInfos)

-- | Render text batches using the shared instance buffer.
--   Each batch draws with a firstInstance offset into the single buffer.
renderTextBatches ∷ CommandBuffer → Device → PhysicalDevice
                  → Buffer → PipelineLayout → GraphicsState
                  → TextInstanceBuffer
                  → V.Vector (TextRenderBatch, (Word32, Word32))
                  → EngineM ε σ ()
renderTextBatches cmdBuf device pDevice quadBuffer layout state tib batchesWithOffsets = do
    env ← ask
    cache ← liftIO $ readIORef (fontCacheRef env)

    V.forM_ batchesWithOffsets $ \(trb, (firstInstance, instanceCount)) →
        when (instanceCount > 0) $
            case Map.lookup (trbFont trb) (fcFonts cache) of
                Nothing →
                    logWarnM CatFont $ "Font handle not found: "
                                     <> T.pack (show (trbFont trb))
                Just atlas →
                    case faDescriptorSet atlas of
                        Nothing →
                            logWarnM CatFont "Font atlas has no descriptor set"
                        Just descSet →
                            case descriptorState state of
                                Just manager → do
                                    cmdBindVertexBuffers cmdBuf 0
                                        (V.fromList [quadBuffer, tibBuffer tib])
                                        (V.fromList [0, 0])

                                    let !uniformSet = V.head (dmActiveSets manager)
                                    cmdBindDescriptorSets cmdBuf
                                        PIPELINE_BIND_POINT_GRAPHICS
                                        layout
                                        0
                                        (V.fromList [uniformSet, descSet])
                                        V.empty

                                    cmdDraw cmdBuf 6 instanceCount 0 firstInstance
                                Nothing →
                                    logWarnM CatFont "font render has no descriptor manager"
