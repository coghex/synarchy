{-# LANGUAGE Strict #-}
module Engine.Graphics.Sprite.Types
  ( Vec2(..)
  , Vec4(..)
  , Sprite(..)
  , SpriteBatch(..)
  , Camera2D(..)
  , SpriteRenderer(..)
  , createSprite
  , defaultCamera2D
  ) where

import UPrelude
import qualified Data.Vector as V
import Data.Word (Word32)
import Engine.Graphics.Vulkan.Types.Texture (TextureData)
import Engine.Graphics.Vulkan.Buffer (Buffer)
import Vulkan.Core10 (DeviceMemory)

-- | 2D Vector for positions and scales
data Vec2 = Vec2
  { vx ∷ Float  -- ^ x component
  , vy ∷ Float  -- ^ y component
  } deriving (Show, Eq)

-- | 4D Vector for colors (RGBA)
data Vec4 = Vec4
  { vr ∷ Float  -- ^ red component (0-1)
  , vg ∷ Float  -- ^ green component (0-1)
  , vb ∷ Float  -- ^ blue component (0-1)
  , va ∷ Float  -- ^ alpha component (0-1)
  } deriving (Show, Eq)

-- | Basic sprite data
data Sprite = Sprite
  { spriteTexture    ∷ TextureData    -- ^ Texture data for the sprite
  , spritePosition   ∷ Vec2           -- ^ Position in world space
  , spriteScale      ∷ Vec2           -- ^ Scale factors (1,1 is normal size)
  , spriteRotation   ∷ Float          -- ^ Rotation in radians
  , spriteColor      ∷ Vec4           -- ^ Tint color
  , spriteLayer      ∷ Int            -- ^ Rendering layer/z-order
  , spriteOrigin     ∷ Vec2           -- ^ Origin point for rotation/scaling
  , spriteSourceRect ∷ Maybe (Int, Int, Int, Int)  -- ^ Source rectangle for sprite sheets (x,y,w,h)
  } deriving (Show)

-- | Batch of sprites sharing the same texture
data SpriteBatch = SpriteBatch
  { batchTexture  ∷ TextureData       -- ^ Shared texture for all sprites in batch
  , batchVertices ∷ V.Vector Float    -- ^ Vertex data (position, texcoord, color)
  , batchIndices  ∷ V.Vector Word32   -- ^ Index data for drawing
  , batchCount    ∷ Int               -- ^ Number of sprites in batch
  } deriving (Show)

-- | Camera for 2D rendering
data Camera2D = Camera2D
  { cameraPosition  ∷ Vec2   -- ^ Camera position in world space
  , cameraZoom      ∷ Float  -- ^ Camera zoom factor (1.0 is normal)
  , cameraRotation  ∷ Float  -- ^ Camera rotation in radians
  , viewportWidth   ∷ Int    -- ^ Viewport width in pixels
  , viewportHeight  ∷ Int    -- ^ Viewport height in pixels
  , isDirty        ∷ Bool    -- ^ Whether camera matrices need updating
  } deriving (Show)

-- | Sprite renderer state
data SpriteRenderer = SpriteRenderer
  { rendererVBO       ∷ Buffer         -- ^ Vertex buffer
  , rendererVBOMemory ∷ DeviceMemory   -- ^ Vertex buffer memory
  , rendererIBO       ∷ Buffer         -- ^ Index buffer
  , rendererIBOMemory ∷ DeviceMemory   -- ^ Index buffer memory
  , rendererBatches   ∷ V.Vector SpriteBatch  -- ^ Active sprite batches
  , rendererCamera    ∷ Camera2D       -- ^ Current camera state
  } deriving (Show)

-- | Create a new sprite with default values
createSprite ∷ TextureData  -- ^ Texture for the sprite
            → Vec2          -- ^ Initial position
            → Sprite
createSprite tex pos = Sprite
  { spriteTexture    = tex
  , spritePosition   = pos
  , spriteScale      = Vec2 1 1
  , spriteRotation   = 0
  , spriteColor      = Vec4 1 1 1 1  -- White, fully opaque
  , spriteLayer      = 0
  , spriteOrigin     = Vec2 0 0      -- Top-left origin
  , spriteSourceRect = Nothing        -- Use entire texture
  }

-- | Create a default 2D camera
defaultCamera2D ∷ Int  -- ^ Viewport width
                → Int  -- ^ Viewport height
                → Camera2D
defaultCamera2D width height = Camera2D
  { cameraPosition  = Vec2 0 0
  , cameraZoom      = 1.0
  , cameraRotation  = 0.0
  , viewportWidth   = width
  , viewportHeight  = height
  , isDirty        = True
  }

-- | Uniform buffer data for sprite shaders
data SpriteShaderData = SpriteShaderData
  { ssdViewProj     ∷ Mat4         -- ^ Combined view-projection matrix
  , ssdBuffer       ∷ Buffer       -- ^ GPU buffer
  , ssdBufferMemory ∷ DeviceMemory -- ^ GPU memory for buffer
  , ssdDescriptor   ∷ DescriptorSet -- ^ Descriptor set for this data
  }

-- | Create shader data with allocated buffer
createSpriteShaderData ∷ Device 
                      → PhysicalDevice
                      → Mat4  -- ^ Initial view-projection matrix
                      → DescriptorPool
                      → DescriptorSetLayout
                      → IO SpriteShaderData
createSpriteShaderData device pDevice viewProj descPool descLayout = do
  let bufferSize = sizeOf (undefined ∷ Mat4)
      bufferUsage = BUFFER_USAGE_UNIFORM_BUFFER_BIT
      memProps = MEMORY_PROPERTY_HOST_VISIBLE_BIT
               .|. MEMORY_PROPERTY_HOST_COHERENT_BIT

  (bufferMemory, buffer) ← createBuffer 
    device 
    pDevice 
    (fromIntegral bufferSize)
    bufferUsage
    memProps

  -- Allocate descriptor set
  let allocInfo = zero
        { descriptorPool = descPool
        , setLayouts = V.singleton descLayout
        }
  (_, descSets) ← allocateDescriptorSets device allocInfo
  let descSet = V.head descSets

  -- Update descriptor set
  let bufferInfo = zero
        { buffer = buffer
        , offset = 0
        , range = fromIntegral bufferSize
        }
      writeInfo = zero
        { dstSet = descSet
        , dstBinding = 0
        , dstArrayElement = 0
        , descriptorType = DESCRIPTOR_TYPE_UNIFORM_BUFFER
        , bufferInfo = V.singleton bufferInfo
        }
  
  updateDescriptorSets device (V.singleton writeInfo) V.empty

  -- Write initial data
  dataPtr ← mapMemory device bufferMemory 0 (fromIntegral bufferSize) zero
  copyToPtr dataPtr viewProj
  unmapMemory device bufferMemory

  pure $ SpriteShaderData
    { ssdViewProj = viewProj
    , ssdBuffer = buffer
    , ssdBufferMemory = bufferMemory
    , ssdDescriptor = descSet
    }

-- | Helper to copy matrix to pointer
copyToPtr ∷ Ptr () → Mat4 → IO ()
copyToPtr ptr mat = do
  let matPtr = castPtr ptr ∷ Ptr Float
  pokeElemOff matPtr 0  (m00 mat)
  pokeElemOff matPtr 1  (m01 mat)
Then you'll need to update your descriptor set layout creation to match:

Haskell
createSpriteDescriptorSetLayout ∷ Device → IO DescriptorSetLayout
createSpriteDescriptorSetLayout device = do
  let binding = zero
        { binding = 0
        , descriptorType = DESCRIPTOR_TYPE_UNIFORM_BUFFER
        , descriptorCount = 1
        , stageFlags = SHADER_STAGE_VERTEX_BIT
        }
      layoutInfo = zero
        { bindings = V.singleton binding
        }
  
  createDescriptorSetLayout device layoutInfo Nothing


