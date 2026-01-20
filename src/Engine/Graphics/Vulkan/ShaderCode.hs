{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}

module Engine.Graphics.Vulkan.ShaderCode
    ( vertexShaderCode
    , fragmentShaderCode
    , fontVertexShaderCode
    , fontFragmentShaderCode
    ) where

import Vulkan.Utils.ShaderQQ.GLSL.Glslang (vert, frag)
import qualified Data.ByteString as BS

vertexShaderCode :: BS.ByteString
vertexShaderCode = [vert|
    #version 450
    #extension GL_ARB_separate_shader_objects : enable

    // Vertex attributes
    layout(location = 0) in vec2 inPosition;
    layout(location = 1) in vec2 inTexCoord;
    layout(location = 2) in vec4 inColor;
    layout(location = 3) in float inAtlasId;

    // Uniform buffer
    layout(set = 0, binding = 0) uniform UniformBufferObject {
        mat4 model;
        mat4 view;
        mat4 proj;
    } ubo;

    // Outputs
    layout(location = 0) out vec2 fragTexCoord;
    layout(location = 1) out vec4 fragColor;
    layout(location = 2) out flat float fragAtlasId;

    void main() {
        vec4 worldPos = ubo.model * vec4(inPosition.xy, 0.0, 1.0);
        gl_Position = ubo.proj * ubo.view * worldPos;
        fragTexCoord = inTexCoord;
        fragColor = inColor;
        fragAtlasId = inAtlasId;
    }
|]

fragmentShaderCode :: BS.ByteString
fragmentShaderCode = [frag|
    #version 450
    #extension GL_ARB_separate_shader_objects : enable

    // Inputs
    layout(location = 0) in vec2 fragTexCoord;
    layout(location = 1) in vec4 fragColor;
    layout(location = 2) in flat float fragAtlasId;

    // Texture array (8 textures per array as specified in your descriptor set layout)
    layout(set = 1, binding = 0) uniform sampler2D textures[8];

    // Output
    layout(location = 0) out vec4 outColor;

    void main() {
        // Get texture index from atlas ID
        int texIndex = int(fragAtlasId);
        
        // Sample from the texture array
        vec4 texColor = texture(textures[texIndex], fragTexCoord);
        
        // Apply color
        outColor = texColor * fragColor;
    }
|]

-- | font vertex shader code (instanced rendering)
fontVertexShaderCode :: BS.ByteString
fontVertexShaderCode = [vert|
    #version 450

    // per-vertex data (quad template)
    layout(location = 0) in vec2 inPosition;  // [0-1, 0-1] quad corners
    layout(location = 1) in vec2 inTexCoord;  // [0-1, 0-1] texture UVs
    // per-instance data (one per character)
    layout(location = 2) in vec2 glyphPos;    // position of the glyph
    layout(location = 3) in vec2 glyphSize;   // size of the glyph
    layout(location = 4) in vec4 glyphUV;     // UV coords of the glyph in the atlas
    layout(location = 5) in vec4 glyphColor;  // color of the glyph
    // uniform buffer
    layout(set = 0, binding = 0) uniform UniformBufferObject {
        mat4 model;
        mat4 view;
        mat4 proj;
    } ubo;
    // outputs to fragment shader
    layout(location = 0) out vec2 fragTexCoord;
    layout(location = 1) out vec4 fragColor;
    void main() {
        // calculate final position
        vec2 pos = glyphPos + inPosition * glyphSize;
        // apply projection (2D, so just the projection matrix)
        gl_Position = ubo.proj * vec4(pos, 0.0, 1.0);
        // interpolate uv within atlas rect
        vec2 uv = mix(glyphUV.xy, glyphUV.zw, inTexCoord);
        fragTexCoord = uv;
        fragColor = glyphColor;
    }
|]

-- | font fragment shader code
fontFragmentShaderCode :: BS.ByteString
fontFragmentShaderCode = [frag|
    #version 450
    layout(location = 0) in vec2 fragTexCoord;
    layout(location = 1) in vec4 fragColor;
    // font atlas texture (grayscale)
    layout(set = 1, binding = 0) uniform sampler2D fontAtlas;
    layout(location = 0) out vec4 outColor;
    void main() {
        // sample alpha from font atlas
        float alpha = texture(fontAtlas, fragTexCoord).r;
        // apply color with alpha
        outColor = vec4(fragColor.rgb, fragColor.a * alpha);
    }
|]
