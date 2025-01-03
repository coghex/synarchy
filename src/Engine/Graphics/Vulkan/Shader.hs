module Engine.Graphics.Vulkan.Shader
    ( createVulkanShaderStages
    ) where

import UPrelude
import qualified Data.Vector as V
import Engine.Core.Monad
import Engine.Core.Resource
import Vulkan.Core10
import Vulkan.Zero
import Vulkan.CStruct.Extends
import Engine.Graphics.Vulkan.ShaderCode (vertexShaderCode, fragmentShaderCode)

createVulkanShaderStages ∷ Device → EngineM ε σ (V.Vector (SomeStruct PipelineShaderStageCreateInfo))
createVulkanShaderStages device = do
    vertModule ← allocResource 
        (\sm → destroyShaderModule device sm Nothing) $
        createShaderModule device zero { code = vertexShaderCode } Nothing
        
    fragModule ← allocResource 
        (\sm → destroyShaderModule device sm Nothing) $
        createShaderModule device zero { code = fragmentShaderCode } Nothing

    let vertStageInfo = zero 
          { stage   = SHADER_STAGE_VERTEX_BIT
          , module' = vertModule
          , name    = "main"
          }
        fragStageInfo = zero 
          { stage   = SHADER_STAGE_FRAGMENT_BIT
          , module' = fragModule
          , name    = "main"
          }

    pure $ V.fromList [SomeStruct vertStageInfo, SomeStruct fragStageInfo]
