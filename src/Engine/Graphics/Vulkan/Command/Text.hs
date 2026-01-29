{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE Strict #-}
module Engine.Graphics.Vulkan.Command.Text
  ( renderTextBatchInline
  , createTextInstanceBuffer
  ) where

import UPrelude
import qualified Data.Vector as V
import qualified Data.Map as Map
import Data.IORef (readIORef)
import Foreign.Storable (sizeOf, pokeElemOff)
import Foreign.Ptr (castPtr)
import Engine.Core.Monad
import Engine.Core.State (EngineEnv(..), GraphicsState(..))
import Engine.Core.Error.Exception (logDebug)
import Engine.Graphics.Font.Data (FontCache(..), FontAtlas(..), GlyphInstance, fcFonts)
import Engine.Graphics.Vulkan.BufferUtils (createVulkanBufferManual)
import Engine.Graphics.Vulkan.Types
import Engine.Graphics.Vulkan.Types.Descriptor
import Engine.Scene.Types (TextBatch(..))
import Vulkan.Core10
import Vulkan.Zero

-- | Render a single text batch inline (helper for recordSceneCommandBuffer)
renderTextBatchInline ∷ CommandBuffer → Device → PhysicalDevice 
                      → Buffer → PipelineLayout → TextBatch → GraphicsState
                      → EngineM ε σ (Maybe (Buffer, DeviceMemory))
renderTextBatchInline cmdBuf device pDevice quadBuffer layout batch state = do
    let instances = tbInstances batch
    if V.null instances
        then pure Nothing
        else do
            -- Create instance buffer
            !instanceBuf@(!instanceBuffer, !instanceMemory) ← createTextInstanceBuffer device pDevice instances
            -- Bind vertex buffers (quad + instances)
            cmdBindVertexBuffers cmdBuf 0 
                (V.fromList [quadBuffer, instanceBuffer])
                (V.fromList [0, 0])
            -- Bind font atlas descriptor set
            env ← ask
            let cacheRef = fontCacheRef env
            cache ← liftIO $ readIORef cacheRef 
            case Map.lookup (tbFontHandle batch) (fcFonts cache) of
                Nothing → do
                    logDebug $ "Font handle not found:  " <> show (tbFontHandle batch)
                    pure Nothing
                Just atlas → do
                    case faDescriptorSet atlas of
                        Nothing → do
                            logDebug "Font atlas has no descriptor set"
                            pure Nothing
                        Just descSet → do
                            case descriptorState state of
                                Just manager → do
                                    let !uniformSet = V.head (dmActiveSets manager)
                                    cmdBindDescriptorSets cmdBuf 
                                        PIPELINE_BIND_POINT_GRAPHICS 
                                        layout 
                                        0
                                        (V.fromList [uniformSet, descSet])
                                        V.empty
                                    -- Draw instanced
                                    let !instanceCount = fromIntegral $ V.length instances
                                    cmdDraw cmdBuf 6 instanceCount 0 0
                                    let !result = Just instanceBuf
                                    pure result
                                Nothing → do
                                    logDebug "No descriptor manager"
                                    pure Nothing

-- | Create instance buffer for text glyphs
createTextInstanceBuffer ∷ Device → PhysicalDevice → V.Vector GlyphInstance 
                         → EngineM ε σ (Buffer, DeviceMemory)
createTextInstanceBuffer device pDevice instances = do
    let !instanceSize = fromIntegral $ sizeOf (undefined ∷ GlyphInstance)
        !instanceCount = V.length instances
        !bufferSize = fromIntegral $ instanceSize * instanceCount
    when (instanceSize /= 48) $
        logDebug $ "Warning: Unusual instance size, expected 48, got: " <> show instanceSize
    
    -- Create buffer
    (!memory, !buffer) ← createVulkanBufferManual device pDevice bufferSize
        BUFFER_USAGE_VERTEX_BUFFER_BIT
        (MEMORY_PROPERTY_HOST_VISIBLE_BIT .|. MEMORY_PROPERTY_HOST_COHERENT_BIT)
    
    -- Upload instance data
    !dataPtr ← mapMemory device memory 0 bufferSize zero
    liftIO $ V.imapM_ (\i inst → pokeElemOff (castPtr dataPtr) i inst) instances
    unmapMemory device memory
    return (buffer, memory)
