{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}

module Engine.Graphics.Vulkan.ShaderCode
    ( vertexShaderCode
    , fragmentShaderCode
    ) where

import Vulkan.Utils.ShaderQQ.GLSL.Glslang (vert, frag)
import qualified Data.ByteString as BS

-- | Vertex shader code for 2D sprite rendering with multiple atlases
vertexShaderCode :: BS.ByteString
vertexShaderCode = [vert|
    #version 450
    #extension GL_ARB_separate_shader_objects : enable

    // Vertex attributes
    layout(location = 0) in vec2 inPosition;    // 2D position
    layout(location = 1) in vec2 inTexCoord;    // Texture coordinates
    layout(location = 2) in vec4 inColor;       // Vertex color
    layout(location = 3) in float inAtlasId;    // Texture atlas identifier

    // Uniform buffer for transformation matrices
    layout(set = 0, binding = 0) uniform UniformBufferObject {
        mat4 model;      // Model matrix
        mat4 view;       // View matrix
        mat4 proj;       // Projection matrix
    } ubo;

    // Output to fragment shader
    layout(location = 0) out vec2 fragTexCoord;
    layout(location = 1) out vec4 fragColor;
    layout(location = 2) out flat float fragAtlasId;  // Use flat interpolation for atlas ID

    void main() {
        // Transform position to clip space
        gl_Position = ubo.proj * ubo.view * ubo.model * vec4(inPosition, 0.0, 1.0);
        
        // Pass data to fragment shader
        fragTexCoord = inTexCoord;
        fragColor = inColor;
        fragAtlasId = inAtlasId;
    }
|]

-- | Fragment shader code for 2D sprite rendering with multiple atlases
fragmentShaderCode :: BS.ByteString
fragmentShaderCode = [frag|
    #version 450
    #extension GL_ARB_separate_shader_objects : enable

    // Input from vertex shader
    layout(location = 0) in vec2 fragTexCoord;
    layout(location = 1) in vec4 fragColor;
    layout(location = 2) in flat float fragAtlasId;  // Use flat interpolation

    // Array of texture samplers for multiple atlases
    layout(set = 1, binding = 0) uniform sampler2D texSamplers[8];  // Support up to 8 texture atlases

    // Output color
    layout(location = 0) out vec4 outColor;

    void main() {
        // Convert atlas ID to integer index
        int atlasIndex = int(fragAtlasId);
        
        // Sample from the correct texture atlas
        vec4 texColor = texture(texSamplers[atlasIndex], fragTexCoord);
        
        // Apply color tint
        outColor = texColor * fragColor;
    }
|]
