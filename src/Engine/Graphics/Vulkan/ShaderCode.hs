{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}

module Engine.Graphics.Vulkan.ShaderCode
    ( vertexShaderCode
    , fragmentShaderCode
    ) where

import Vulkan.Utils.ShaderQQ.GLSL.Glslang (vert, frag)
import qualified Data.ByteString as BS

-- | Vertex shader code for 2D sprite rendering
vertexShaderCode :: BS.ByteString
vertexShaderCode = [vert|
    #version 450
    #extension GL_ARB_separate_shader_objects : enable

    // Vertex attributes
    layout(location = 0) in vec2 inPosition;    // 2D position
    layout(location = 1) in vec2 inTexCoord;    // Texture coordinates
    layout(location = 2) in vec4 inColor;       // Vertex color

    // Uniform buffer for transformation matrices
    layout(binding = 0) uniform UniformBufferObject {
        mat4 model;      // Model matrix
        mat4 view;       // View matrix
        mat4 proj;       // Projection matrix
    } ubo;

    // Output to fragment shader
    layout(location = 0) out vec2 fragTexCoord;
    layout(location = 1) out vec4 fragColor;

    void main() {
        // Transform position to clip space
        gl_Position = ubo.proj * ubo.view * ubo.model * vec4(inPosition, 0.0, 1.0);
        
        // Pass texture coordinates and color to fragment shader
        fragTexCoord = inTexCoord;
        fragColor = inColor;
    }
|]

-- | Fragment shader code for 2D sprite rendering
fragmentShaderCode :: BS.ByteString
fragmentShaderCode = [frag|
    #version 450
    #extension GL_ARB_separate_shader_objects : enable

    // Input from vertex shader
    layout(location = 0) in vec2 fragTexCoord;
    layout(location = 1) in vec4 fragColor;

    // Texture sampler
    layout(binding = 1) uniform sampler2D texSampler;

    // Output color
    layout(location = 0) out vec4 outColor;

    void main() {
        // Sample texture and multiply with vertex color
        vec4 texColor = texture(texSampler, fragTexCoord);
        outColor = texColor * fragColor;
    }
|]
