{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}

module Engine.Graphics.Vulkan.ShaderCode
    ( vertexShaderCode
    , fragmentShaderCode
    , fontVertexShaderCode
    , fontFragmentShaderCode
    , fontUIVertexShaderCode
    , fontSDFFragmentShaderCode
    , bindlessVertexShaderCode
    , bindlessFragmentShaderCode
    , bindlessUIVertexShaderCode
    ) where

import Vulkan.Utils.ShaderQQ.GLSL.Glslang (vert, frag)
import qualified Data.ByteString as BS

-- | Legacy vertex shader (non-bindless, kept for compatibility)
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
        mat4 uiView;
        mat4 uiProj;
        float brightness;
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

-- | Legacy fragment shader (non-bindless, kept for compatibility)
fragmentShaderCode :: BS.ByteString
fragmentShaderCode = [frag|
    #version 450
    #extension GL_ARB_separate_shader_objects : enable

    // Inputs
    layout(location = 0) in vec2 fragTexCoord;
    layout(location = 1) in vec4 fragColor;
    layout(location = 2) in flat float fragAtlasId;

    // Texture array
    layout(set = 1, binding = 0) uniform sampler2D textures[8];

    // Uniform buffer (for brightness)
    layout(set = 0, binding = 0) uniform UniformBufferObject {
        mat4 model;
        mat4 view;
        mat4 proj;
        mat4 uiView;
        mat4 uiProj;
        float brightness;
    } ubo;

    // Output
    layout(location = 0) out vec4 outColor;

    void main() {
        int texIndex = int(fragAtlasId);
        vec4 texColor = texture(textures[texIndex], fragTexCoord);
        vec4 color = texColor * fragColor;
        color.rgb *= ubo.brightness;
        outColor = color;
    }
|]

-- | Font vertex shader (instanced rendering, world camera / NDC)
fontVertexShaderCode :: BS.ByteString
fontVertexShaderCode = [vert|
    #version 450

    // per-vertex data (quad template)
    layout(location = 0) in vec2 inPosition;
    layout(location = 1) in vec2 inTexCoord;
    // per-instance data (one per character)
    layout(location = 2) in vec2 glyphPos;
    layout(location = 3) in vec2 glyphSize;
    layout(location = 4) in vec4 glyphUV;
    layout(location = 5) in vec4 glyphColor;

    // uniform buffer
    layout(set = 0, binding = 0) uniform UniformBufferObject {
        mat4 model;
        mat4 view;
        mat4 proj;
        mat4 uiView;
        mat4 uiProj;
        float brightness;
    } ubo;

    // outputs to fragment shader
    layout(location = 0) out vec2 fragTexCoord;
    layout(location = 1) out vec4 fragColor;
    layout(location = 2) out float fragBrightness;

    void main() {
        vec2 pos = glyphPos + inPosition * glyphSize;
        gl_Position = vec4(pos, 0.0, 1.0);
        vec2 uv = mix(glyphUV.xy, glyphUV.zw, inTexCoord);
        fragTexCoord = uv;
        fragColor = glyphColor;
        fragBrightness = ubo.brightness;
    }
|]

-- | Legacy font fragment shader (non-SDF, kept for compatibility)
fontFragmentShaderCode :: BS.ByteString
fontFragmentShaderCode = [frag|
    #version 450

    layout(location = 0) in vec2 fragTexCoord;
    layout(location = 1) in vec4 fragColor;
    layout(location = 2) in float fragBrightness;

    layout(set = 1, binding = 0) uniform sampler2D fontAtlas;

    layout(location = 0) out vec4 outColor;

    void main() {
        float alpha = texture(fontAtlas, fragTexCoord).r;
        vec3 color = fragColor.rgb * fragBrightness;
        outColor = vec4(color, fragColor.a * alpha);
    }
|]

-- | Bindless vertex shader (world camera)
bindlessVertexShaderCode :: BS.ByteString
bindlessVertexShaderCode = [vert|
    #version 450
    #extension GL_ARB_separate_shader_objects : enable

    // Vertex attributes
    layout(location = 0) in vec2 inPosition;
    layout(location = 1) in vec2 inTexCoord;
    layout(location = 2) in vec4 inColor;
    layout(location = 3) in float inTexIndex;

    // Uniform buffer
    layout(set = 0, binding = 0) uniform UniformBufferObject {
        mat4 model;
        mat4 view;
        mat4 proj;
        mat4 uiView;
        mat4 uiProj;
        float brightness;
    } ubo;

    // Outputs
    layout(location = 0) out vec2 fragTexCoord;
    layout(location = 1) out vec4 fragColor;
    layout(location = 2) out flat int fragTexIndex;
    layout(location = 3) out float fragBrightness;

    void main() {
        vec4 worldPos = ubo.model * vec4(inPosition.xy, 0.0, 1.0);
        gl_Position = ubo.proj * ubo.view * worldPos;
        fragTexCoord = inTexCoord;
        fragColor = inColor;
        fragTexIndex = int(inTexIndex);
        fragBrightness = ubo.brightness;
    }
|]

-- | Bindless fragment shader
-- Uses unbounded texture array with nonuniform indexing
bindlessFragmentShaderCode :: BS.ByteString
bindlessFragmentShaderCode = [frag|
    #version 450
    #extension GL_ARB_separate_shader_objects : enable
    #extension GL_EXT_nonuniform_qualifier : enable

    layout(location = 0) in vec2 fragTexCoord;
    layout(location = 1) in vec4 fragColor;
    layout(location = 2) in flat int fragTexIndex;
    layout(location = 3) in float fragBrightness;

    // Unbounded texture array - bindless! (moltenVk requires bounds)
    layout(set = 1, binding = 0) uniform sampler2D textures[16384];

    layout(location = 0) out vec4 outColor;

    void main() {
        vec4 texColor = texture(textures[nonuniformEXT(fragTexIndex)], fragTexCoord);
        vec4 color = texColor * fragColor;
        color.rgb *= fragBrightness;
        outColor = color;
    }
|]

-- | Bindless UI vertex shader (uses UI camera matrices)
bindlessUIVertexShaderCode :: BS.ByteString
bindlessUIVertexShaderCode = [vert|
    #version 450
    #extension GL_ARB_separate_shader_objects : enable

    // Vertex attributes
    layout(location = 0) in vec2 inPosition;
    layout(location = 1) in vec2 inTexCoord;
    layout(location = 2) in vec4 inColor;
    layout(location = 3) in float inTexIndex;

    // Uniform buffer
    layout(set = 0, binding = 0) uniform UniformBufferObject {
        mat4 model;
        mat4 view;
        mat4 proj;
        mat4 uiView;
        mat4 uiProj;
        float brightness;
    } ubo;

    // Outputs
    layout(location = 0) out vec2 fragTexCoord;
    layout(location = 1) out vec4 fragColor;
    layout(location = 2) out flat int fragTexIndex;
    layout(location = 3) out float fragBrightness;

    void main() {
        gl_Position = ubo.uiProj * vec4(inPosition.xy, 0.0, 1.0);
        fragTexCoord = inTexCoord;
        fragColor = inColor;
        fragTexIndex = int(inTexIndex);
        fragBrightness = ubo.brightness;
    }
|]

-- | Font UI vertex shader (uses UI projection matrix)
fontUIVertexShaderCode :: BS.ByteString
fontUIVertexShaderCode = [vert|
    #version 450

    // per-vertex data (quad template)
    layout(location = 0) in vec2 inPosition;
    layout(location = 1) in vec2 inTexCoord;
    // per-instance data (one per character)
    layout(location = 2) in vec2 glyphPos;
    layout(location = 3) in vec2 glyphSize;
    layout(location = 4) in vec4 glyphUV;
    layout(location = 5) in vec4 glyphColor;

    // uniform buffer
    layout(set = 0, binding = 0) uniform UniformBufferObject {
        mat4 model;
        mat4 view;
        mat4 proj;
        mat4 uiView;
        mat4 uiProj;
        float brightness;
    } ubo;

    // outputs to fragment shader
    layout(location = 0) out vec2 fragTexCoord;
    layout(location = 1) out vec4 fragColor;
    layout(location = 2) out float fragBrightness;

    void main() {
        vec2 pixelPos = glyphPos + inPosition * glyphSize;
        gl_Position = ubo.uiProj * vec4(pixelPos, 0.0, 1.0);
        vec2 uv = mix(glyphUV.xy, glyphUV.zw, inTexCoord);
        fragTexCoord = uv;
        fragColor = glyphColor;
        fragBrightness = ubo.brightness;
    }
|]

-- | Font SDF fragment shader (signed distance field rendering)
fontSDFFragmentShaderCode :: BS.ByteString
fontSDFFragmentShaderCode = [frag|
    #version 450

    layout(location = 0) in vec2 fragTexCoord;
    layout(location = 1) in vec4 fragColor;
    layout(location = 2) in float fragBrightness;

    layout(set = 1, binding = 0) uniform sampler2D fontAtlas;

    layout(location = 0) out vec4 outColor;

    void main() {
        float distance = texture(fontAtlas, fragTexCoord).r;
        float edge = 0.7;
        float smoothing = fwidth(distance) * 0.5;
        float alpha = smoothstep(edge - smoothing, edge + smoothing, distance);
        vec3 color = fragColor.rgb * fragBrightness;
        outColor = vec4(color, fragColor.a * alpha);
    }
|]
