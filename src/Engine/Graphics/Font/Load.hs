module Engine.Graphics.Font.Load where

import UPrelude
import qualified Data.Map.Strict as Map
import qualified Data.Vector as V
import qualified Data.Text as T
import Data.Word (Word8)
import Data.Char (ord)
import Data.Array.IO (IOArray, newArray, writeArray, getElems)
import Data.IORef (readIORef, atomicModifyIORef')
import Foreign.Ptr (castPtr)
import Foreign.Marshal.Array (pokeArray)
import Engine.Asset.Types
import Engine.Asset.Manager (generateTextureHandle)
import Engine.Asset.Handle
import Engine.Graphics.Font.Data
import Engine.Graphics.Font.STB
import Engine.Graphics.Types
import Engine.Graphics.Vulkan.Buffer (createVulkanBuffer)
import Engine.Graphics.Vulkan.BufferUtils (createVulkanBufferManual)
import Engine.Graphics.Vulkan.Image (createVulkanImage, VulkanImage(..), createVulkanImageView)
import Engine.Graphics.Vulkan.Texture (createTextureSampler, TexturePoolState(..))
import Engine.Graphics.Vulkan.Types.Texture
import Vulkan.Core10
import Vulkan.Zero
import Vulkan.CStruct.Extends
import Engine.Core.Log (logDebug, logWarn, LogCategory(..), LoggerState)
import Engine.Core.Log.Monad (logDebugM, logInfoM, logWarnM, logDebugSM, logInfoSM, logWarnSM, logAndThrowM)
import Engine.Core.Monad
import Engine.Core.State
import Engine.Core.Resource (allocResource)
import Engine.Core.Error.Exception (ExceptionType(..), GraphicsError(..))
import Control.Monad (forM_, when, foldM, forM)

-- | Create a descriptor pool dedicated to font atlas textures
-- Each font gets one descriptor set with one combined image sampler
createFontDescriptorPool ∷ Device → Word32 → EngineM ε σ DescriptorPool
createFontDescriptorPool device maxFonts = do
  let poolSize = zero
        { type' = DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER
        , descriptorCount = maxFonts  -- One sampler per font
        }
      poolInfo = zero
        { maxSets = fromIntegral maxFonts
        , poolSizes = V.singleton poolSize
        , flags = DESCRIPTOR_POOL_CREATE_FREE_DESCRIPTOR_SET_BIT
        }
  
  allocResource (\pool → destroyDescriptorPool device pool Nothing) $
    createDescriptorPool device poolInfo Nothing

-- | Load a TTF font at specified size
loadFont ∷ FontHandle → FilePath → Int → EngineM ε σ FontHandle
loadFont requestedHandle fontPath fontSize = do
    logInfoSM CatFont "Font atlas generation started"
        [("path", T.pack fontPath)
        ,("size", T.pack $ show fontSize)
        ,("char_range", "' ' to '~'")]
    
    cacheRef ← asks fontCacheRef
    cache ← liftIO $ readIORef cacheRef
    gs ← gets graphicsState
    case Map.lookup (fontPath, fontSize) (fcPathCache cache) of
        Just handle → do
            logWarnM CatFont $ "Font already loaded: " <> T.pack (show fontPath)
            return handle
        Nothing → do
            -- get the descriptor layout for font
            fontDescLayout ← case fontDescriptorLayout gs of
                Nothing → logAndThrowM CatFont (ExGraphics DescriptorError)
                              "Font descriptor layout not initialized"
                Just layout → return layout
            -- Generate atlas
            loggerRef ← asks loggerRef
            logger ← liftIO $ readIORef loggerRef
            atlas ← liftIO $ generateFontAtlas logger fontPath fontSize
            
            logDebugSM CatFont "Atlas texture dimensions"
                [("width", T.pack $ show $ faAtlasWidth atlas)
                ,("height", T.pack $ show $ faAtlasHeight atlas)
                ,("glyph_count", T.pack $ show $ Map.size $ faGlyphData atlas)]
            
            -- Upload to GPU
            (texHandle, descriptorSet, imgView, samp) ← uploadFontAtlasToGPU atlas fontDescLayout
            
            logInfoSM CatFont "Font GPU upload completion"
                [("atlas_size", T.pack $ show (faAtlasWidth atlas) <> "x" <> T.pack (show $ faAtlasHeight atlas))]
            
            let newAtlas = atlas { faTexture = texHandle
                                 , faDescriptorSet = Just descriptorSet
                                 , faImageView = Just imgView
                                 , faSampler = Just samp }
                handle = requestedHandle
            
            liftIO $ atomicModifyIORef' cacheRef $ \c → ((c
                { fcFonts = Map.insert handle newAtlas (fcFonts c)
                , fcPathCache = Map.insert (fontPath, fontSize) handle (fcPathCache c) }
                ), ())
            
            return handle

-----------------------------------------------------------
-- Atlas Generation with STB
-----------------------------------------------------------

generateFontAtlas ∷ LoggerState → FilePath → Int → IO FontAtlas
generateFontAtlas logger fontPath fontSize = do
    logDebug logger CatFont $ "Generating font atlas for: " <> T.pack fontPath
                            <> " size=" <> T.pack (show fontSize)
    
    maybeFont ← loadSTBFont logger fontPath
    case maybeFont of
        Nothing → error $ "Failed to load font: " ++ fontPath
        Just font → do
            scale ← scaleForPixelHeight font (fromIntegral fontSize)
            (ascent, descent, lineGap) ← getSTBFontMetrics font scale
            
            let chars = [' '..'~']
                numChars = length chars
            
            -- Enumerate chars with indices to limit debug logging to first 3 glyphs
            glyphDataWithMetrics ← forM (zip chars [0..]) $ \(c, idx) → do
                (w,h,xoff,yoff,pixels) ← renderGlyphWithMetrics logger font c scale
                (_,_,_,_,advance) ← getSTBGlyphMetrics font c scale
                -- Log metrics for first few glyphs
                when (idx < 3) $
                    logDebug logger CatFont $ "Glyph metrics: char='" <> T.singleton c <> "' "
                        <> "size=" <> T.pack (show w) <> "x" <> T.pack (show h)
                        <> " bearing=(" <> T.pack (show xoff) <> "," <> T.pack (show yoff) <> ")"
                        <> " advance=" <> T.pack (show advance)
                return (w, h, xoff, yoff, pixels, advance)

            freeSTBFont font
            
            let charsPerRow = 16
                maxWidth = maximum $ map (\(w,_,_,_,_,_) → w) glyphDataWithMetrics
                maxHeight = maximum $ map (\(_,h,_,_,_,_) → h) glyphDataWithMetrics
                cellWidth = maxWidth + 2
                cellHeight = maxHeight + 2
                atlasWidth = nextPowerOf2 (charsPerRow * cellWidth)
                numRows = (numChars + charsPerRow - 1) `div` charsPerRow
                atlasHeight = nextPowerOf2 (numRows * cellHeight)
            
            logDebug logger CatFont $
                "Font atlas size: " <> T.pack (show atlasWidth)
                <> "x" <> T.pack (show atlasHeight)
            
            -- Pack glyphs with metrics
            (atlasBitmap, glyphMap) ← packGlyphsSTBWithMetrics atlasWidth atlasHeight charsPerRow cellWidth cellHeight glyphDataWithMetrics chars
           
            logDebug logger CatFont $
                "Font atlas generated with " <> T.pack (show $ Map.size glyphMap)
                                             <> " glyphs."
            
            return $ FontAtlas
                { faTexture = TextureHandle 0
                , faGlyphData = glyphMap
                , faAtlasWidth = atlasWidth
                , faAtlasHeight = atlasHeight
                , faFontSize = fontSize
                , faLineHeight = ascent - descent + lineGap
                , faBaseline = ascent
                , faAtlasBitmap = atlasBitmap
                , faDescriptorSet = Nothing
                , faImageView = Nothing
                , faSampler = Nothing
                }

renderGlyphWithMetrics ∷ LoggerState → STBFont → Char → Float 
                       → IO (Int, Int, Int, Int, [Word8])
renderGlyphWithMetrics logger font char scale = do
    result ← renderSTBGlyph font char scale
    case result of
        Nothing → do
            -- Warn when specific glyphs fail to rasterize
            logWarn logger CatFont $ "Failed to rasterize glyph: '" <> T.singleton char <> "'"
            return (0, 0, 0, 0, [])
        Just glyph → return glyph

-- Updated to use metrics stored before font was freed
packGlyphsSTBWithMetrics ∷ Int → Int → Int → Int → Int
                         → [(Int, Int, Int, Int, [Word8], Float)] → [Char]
                         → IO ([Word8], Map.Map Char GlyphInfo)
packGlyphsSTBWithMetrics atlasWidth atlasHeight charsPerRow cellWidth cellHeight glyphData chars = do
    atlasArray ← newArray (0, atlasWidth * atlasHeight - 1) 0 ∷ IO (IOArray Int Word8)
    
    glyphMap ← foldM (packGlyph atlasArray) Map.empty (zip glyphData (zip chars [0..]))
    
    finalBitmap ← getElems atlasArray
    return (finalBitmap, glyphMap)
  where
    packGlyph atlasArray gmap ((w, h, xoff, yoff, pixels, advance), (char, idx)) = do
        let col = idx `mod` charsPerRow
            row = idx `div` charsPerRow
            atlasX = col * cellWidth + 1
            atlasY = row * cellHeight + 1
        
        forM_ [0..h-1] $ \y →
            forM_ [0..w-1] $ \x → do
                let srcIdx = y * w + x
                    dstIdx = (atlasY + y) * atlasWidth + (atlasX + x)
                when (srcIdx < length pixels) $
                    writeArray atlasArray dstIdx (pixels !! srcIdx)
        
        let u0 = fromIntegral atlasX / fromIntegral atlasWidth
            v0 = fromIntegral atlasY / fromIntegral atlasHeight
            u1 = fromIntegral (atlasX + w) / fromIntegral atlasWidth
            v1 = fromIntegral (atlasY + h) / fromIntegral atlasHeight
            
            glyphInfo = GlyphInfo
                { giUVRect = (u0, v0, u1, v1)
                , giSize = (fromIntegral w, fromIntegral h)
                , giBearing = (fromIntegral xoff, fromIntegral yoff)
                , giAdvance = advance  -- Use stored advance
                }
        
        return $ Map.insert char glyphInfo gmap

nextPowerOf2 ∷ Int → Int
nextPowerOf2 n = head $ dropWhile (< n) powersOf2
  where powersOf2 = iterate (*2) 1

-----------------------------------------------------------
-- GPU Upload
-----------------------------------------------------------

uploadFontAtlasToGPU ∷ FontAtlas → DescriptorSetLayout
  → EngineM ε σ (TextureHandle, DescriptorSet, ImageView, Sampler)
uploadFontAtlasToGPU atlas fontDescriptorsLayout = do
    state ← gets graphicsState
    
    -- Get Vulkan handles
    device ← case vulkanDevice state of
        Nothing → logAndThrowM CatFont (ExGraphics VulkanDeviceLost)
                                       "No device"
        Just d → pure d
    pDevice ← case vulkanPDevice state of
        Nothing → logAndThrowM CatFont (ExGraphics VulkanDeviceLost)
                                       "No physical device"
        Just pd → pure pd
    cmdPool ← case vulkanCmdPool state of
        Nothing → logAndThrowM CatFont (ExGraphics VulkanDeviceLost)
                                       "No command pool"
        Just pool → pure pool
    queues ← case deviceQueues state of
        Nothing → logAndThrowM CatFont (ExGraphics VulkanDeviceLost)
                                       "No device queues"
        Just qs → pure qs
    
    let width = faAtlasWidth atlas
        height = faAtlasHeight atlas
        pixels = faAtlasBitmap atlas
        queue = graphicsQueue queues

    -- Create grayscale texture
    (texHandle, descSet, imgView, samp) ← createFontTextureGrayscale device pDevice
                                            cmdPool queue width height pixels fontDescriptorsLayout
    
    return (texHandle, descSet, imgView, samp)

createFontTextureGrayscale ∷ Device → PhysicalDevice → CommandPool → Queue 
                           → Int → Int → [Word8] → DescriptorSetLayout
                           → EngineM ε σ (TextureHandle, DescriptorSet, ImageView, Sampler)
createFontTextureGrayscale device pDevice cmdPool queue width height pixels fontDescLayout = do
    let bufferSize = fromIntegral $ width * height
    
    -- 1. Create staging buffer
    (stagingMemory, stagingBuffer) ← createVulkanBufferManual device pDevice bufferSize
        BUFFER_USAGE_TRANSFER_SRC_BIT
        (MEMORY_PROPERTY_HOST_VISIBLE_BIT .|. MEMORY_PROPERTY_HOST_COHERENT_BIT)
    
    -- 2. Upload pixel data
    dataPtr ← mapMemory device stagingMemory 0 bufferSize zero
    liftIO $ pokeArray (castPtr dataPtr) pixels
    unmapMemory device stagingMemory
    
    -- 3. Create GPU image
    image ← createVulkanImage device pDevice
        (fromIntegral width, fromIntegral height)
        FORMAT_R8_UNORM
        IMAGE_TILING_OPTIMAL
        (IMAGE_USAGE_TRANSFER_DST_BIT .|. IMAGE_USAGE_SAMPLED_BIT)
        MEMORY_PROPERTY_DEVICE_LOCAL_BIT
    
    -- 4. Transition and copy
    runCommandsOnce device cmdPool queue $ \cmdBuf → do
        transitionImageLayout cmdBuf (viImage image) FORMAT_R8_UNORM
            IMAGE_LAYOUT_UNDEFINED IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL
        
        let region = zero
              { bufferOffset = 0
              , bufferRowLength = 0
              , bufferImageHeight = 0
              , imageSubresource = zero
                  { aspectMask = IMAGE_ASPECT_COLOR_BIT
                  , mipLevel = 0
                  , baseArrayLayer = 0
                  , layerCount = 1
                  }
              , imageOffset = Offset3D 0 0 0
              , imageExtent = Extent3D (fromIntegral width) (fromIntegral height) 1
              }
        cmdCopyBufferToImage cmdBuf stagingBuffer (viImage image) 
                            IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL (V.singleton region)
        
        transitionImageLayout cmdBuf (viImage image) FORMAT_R8_UNORM
            IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL
    
    -- 5. Create image view
    imageView ← createVulkanImageView device image FORMAT_R8_UNORM IMAGE_ASPECT_COLOR_BIT
    
    -- 6. Create sampler
    sampler ← createFontTextureSampler device
    
    -- 7. There is no number 7
    
    -- 8. Allocate and update descriptor set
    state ← get
    fontPool ← case fontDescriptorPool (graphicsState state) of
        Nothing → logAndThrowM CatFont (ExGraphics DescriptorError)
                                       "Font descriptor pool not initialized"
        Just pool → pure pool
    
    let allocInfo = zero
          { descriptorPool = fontPool
          , setLayouts = V.singleton fontDescLayout
          }
    descriptorSets ← liftIO $ allocateDescriptorSets device allocInfo
    let descSet = V.head descriptorSets
    
    logDebugM CatFont "Descriptor set allocated for font texture"
    
    -- Update descriptor set with font texture
    let imgInfo = zero
          { sampler = sampler
          , imageView = imageView
          , imageLayout = IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL
          }
        writeDescriptorSet = SomeStruct $ zero
          { dstSet = descSet
          , dstBinding = 0
          , dstArrayElement = 0
          , descriptorType = DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER
          , descriptorCount = 1
          , imageInfo = V.singleton imgInfo
          }
    
    updateDescriptorSets device (V.singleton writeDescriptorSet) V.empty
    -- Generate handle
    pool ← gets assetPool
    handle ← liftIO $ generateTextureHandle pool
    -- Cleanup staging buffer
    destroyBuffer device stagingBuffer Nothing
    freeMemory device stagingMemory Nothing
    return (handle, descSet, imageView, sampler)

-----------------------------------------------------------
-- Helper Functions
-----------------------------------------------------------

transitionImageLayout ∷ CommandBuffer → Image → Format 
                      → ImageLayout → ImageLayout → EngineM ε σ ()
transitionImageLayout cmdBuf image format oldLayout newLayout = do
    let (srcAccess, dstAccess, srcStage, dstStage) = case (oldLayout, newLayout) of
          (IMAGE_LAYOUT_UNDEFINED, IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL) →
              ( zero
              , ACCESS_TRANSFER_WRITE_BIT
              , PIPELINE_STAGE_TOP_OF_PIPE_BIT
              , PIPELINE_STAGE_TRANSFER_BIT
              )
          (IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL, IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL) →
              ( ACCESS_TRANSFER_WRITE_BIT
              , ACCESS_SHADER_READ_BIT
              , PIPELINE_STAGE_TRANSFER_BIT
              , PIPELINE_STAGE_FRAGMENT_SHADER_BIT
              )
          _ →
              ( ACCESS_MEMORY_READ_BIT .|. ACCESS_MEMORY_WRITE_BIT
              , ACCESS_MEMORY_READ_BIT .|. ACCESS_MEMORY_WRITE_BIT
              , PIPELINE_STAGE_ALL_COMMANDS_BIT
              , PIPELINE_STAGE_ALL_COMMANDS_BIT
              )
    
    let barrier = zero
          { srcAccessMask = srcAccess
          , dstAccessMask = dstAccess
          , oldLayout = oldLayout
          , newLayout = newLayout
          , srcQueueFamilyIndex = QUEUE_FAMILY_IGNORED
          , dstQueueFamilyIndex = QUEUE_FAMILY_IGNORED
          , image = image
          , subresourceRange = zero
              { aspectMask = IMAGE_ASPECT_COLOR_BIT
              , baseMipLevel = 0
              , levelCount = 1
              , baseArrayLayer = 0
              , layerCount = 1
              }
          }
    
    cmdPipelineBarrier cmdBuf srcStage dstStage zero V.empty V.empty (V.singleton $ SomeStruct barrier)

runCommandsOnce ∷ Device → CommandPool → Queue → (CommandBuffer → EngineM ε σ ()) → EngineM ε σ ()
runCommandsOnce device cmdPool queue action = do
    let allocInfo = zero
          { commandPool = cmdPool
          , level = COMMAND_BUFFER_LEVEL_PRIMARY
          , commandBufferCount = 1
          }
    
    cmdBuffers ← allocateCommandBuffers device allocInfo
    let cmdBuffer = V.head cmdBuffers
    
    let beginInfo = (zero ∷ CommandBufferBeginInfo '[])
          { flags = COMMAND_BUFFER_USAGE_ONE_TIME_SUBMIT_BIT }
    beginCommandBuffer cmdBuffer beginInfo
    
    action cmdBuffer
    
    endCommandBuffer cmdBuffer
    
    let submitInfo = zero 
          { commandBuffers = V.singleton (commandBufferHandle cmdBuffer) 
          }
    queueSubmit queue (V.singleton $ SomeStruct submitInfo) zero
    queueWaitIdle queue
    
    freeCommandBuffers device cmdPool cmdBuffers

createFontTextureSampler ∷ Device → EngineM ε σ Sampler
createFontTextureSampler device = do
    let samplerInfo = zero
          { magFilter = FILTER_LINEAR
          , minFilter = FILTER_LINEAR
          , mipmapMode = SAMPLER_MIPMAP_MODE_LINEAR
          , addressModeU = SAMPLER_ADDRESS_MODE_CLAMP_TO_EDGE
          , addressModeV = SAMPLER_ADDRESS_MODE_CLAMP_TO_EDGE
          , addressModeW = SAMPLER_ADDRESS_MODE_CLAMP_TO_EDGE
          , mipLodBias = 0.0
          , anisotropyEnable = False
          , maxAnisotropy = 1.0
          , compareEnable = False
          , compareOp = COMPARE_OP_ALWAYS
          , minLod = 0.0
          , maxLod = 0.0
          , borderColor = BORDER_COLOR_INT_OPAQUE_BLACK
          , unnormalizedCoordinates = False
          }
    
    allocResource (\s → destroySampler device s Nothing) $
        createSampler device samplerInfo Nothing
