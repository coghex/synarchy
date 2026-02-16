{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes, UnicodeSyntax #-}

module Engine.Graphics.Vulkan.ShaderCode
    ( fontVertexShaderCode
    , fontFragmentShaderCode
    , fontUIVertexShaderCode
    , fontSDFFragmentShaderCode
    , bindlessVertexShaderCode
    , bindlessFragmentShaderCode
    , bindlessUIVertexShaderCode
    , bindlessUIFragmentShaderCode
    ) where

import Vulkan.Utils.ShaderQQ.GLSL.Glslang (vert, frag)
import qualified Data.ByteString as BS

-- | Font vertex shader (instanced rendering, world camera / NDC)
fontVertexShaderCode ∷ BS.ByteString
fontVertexShaderCode = [vert|
    #version 450

    layout(location = 0) in vec2 inPosition;
    layout(location = 1) in vec2 inTexCoord;
    layout(location = 2) in vec2 glyphPos;
    layout(location = 3) in vec2 glyphSize;
    layout(location = 4) in vec4 glyphUV;
    layout(location = 5) in vec4 glyphColor;

    layout(set = 0, binding = 0) uniform UniformBufferObject {
        mat4 model;
        mat4 view;
        mat4 proj;
        mat4 uiView;
        mat4 uiProj;
        float brightness;
        float screenW;
        float screenH;
        float pixelSnap;
        float sunAngle;
        float ambientLight;
        float cameraFacing;
    } ubo;

    layout(location = 0) out vec2 fragTexCoord;
    layout(location = 1) out vec4 fragColor;
    layout(location = 2) out float fragBrightness;

    void main() {
        // World font shader: glyphPos is in NDC
        // Snap glyph origin, not individual vertices
        vec2 origin = glyphPos;
        if (ubo.pixelSnap > 0.5) {
            vec2 px = (origin * 0.5 + 0.5) * vec2(ubo.screenW, ubo.screenH);
            px = floor(px) + 0.5;
            origin = px / vec2(ubo.screenW, ubo.screenH) * 2.0 - 1.0;
        }
        vec2 pos = origin + inPosition * glyphSize;
        gl_Position = vec4(pos, 0.0, 1.0);

        vec2 uv = mix(glyphUV.xy, glyphUV.zw, inTexCoord);
        fragTexCoord = uv;
        fragColor = glyphColor;
        fragBrightness = ubo.brightness;
    }
|]

-- | Legacy font fragment shader (non-SDF, kept for compatibility)
fontFragmentShaderCode ∷ BS.ByteString
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

-- | Bindless vertex shader (world camera) with face map support
-- Pixel snap: shifts all vertices uniformly by removing the fractional
-- pixel offset, so quads translate rigidly without distortion
bindlessVertexShaderCode ∷ BS.ByteString
bindlessVertexShaderCode = [vert|
    #version 450
    #extension GL_ARB_separate_shader_objects : enable

    layout(location = 0) in vec2 inPosition;
    layout(location = 1) in vec2 inTexCoord;
    layout(location = 2) in vec4 inColor;
    layout(location = 3) in float inTexIndex;
    layout(location = 4) in float inFaceMapIndex;

    layout(set = 0, binding = 0) uniform UniformBufferObject {
        mat4 model;
        mat4 view;
        mat4 proj;
        mat4 uiView;
        mat4 uiProj;
        float brightness;
        float screenW;
        float screenH;
        float pixelSnap;
        float sunAngle;
        float ambientLight;
        float cameraFacing;
    } ubo;

    layout(location = 0) out vec2 fragTexCoord;
    layout(location = 1) out vec4 fragColor;
    layout(location = 2) out flat int fragTexIndex;
    layout(location = 3) out float fragBrightness;
    layout(location = 4) out flat int fragFaceMapIndex;
    layout(location = 5) out float fragSunAngle;
    layout(location = 6) out float fragAmbientLight;
    layout(location = 7) out float fragCameraFacing;

    void main() {
        vec4 worldPos = ubo.model * vec4(inPosition.xy, 0.0, 1.0);
        gl_Position = ubo.proj * ubo.view * worldPos;

        if (ubo.pixelSnap > 0.5) {
            vec2 ndc = gl_Position.xy / gl_Position.w;
            vec2 px = (ndc * 0.5 + 0.5) * vec2(ubo.screenW, ubo.screenH);
            vec2 frac_offset = fract(px) - 0.5;
            vec2 correction = frac_offset / vec2(ubo.screenW, ubo.screenH) * 2.0;
            gl_Position.xy -= correction * gl_Position.w;
        }

        fragTexCoord = inTexCoord;
        fragColor = inColor;
        fragTexIndex = int(inTexIndex);
        fragBrightness = ubo.brightness;
        fragFaceMapIndex = int(inFaceMapIndex);
        fragSunAngle = ubo.sunAngle;
        fragAmbientLight = ubo.ambientLight;
        fragCameraFacing = ubo.cameraFacing;
    }
|]

-- | Bindless fragment shader with face-map directional lighting
-- Used for world-space rendering (tiles, scene sprites)
bindlessFragmentShaderCode ∷ BS.ByteString
bindlessFragmentShaderCode = [frag|
    #version 450
    #extension GL_ARB_separate_shader_objects : enable
    #extension GL_EXT_nonuniform_qualifier : enable

    layout(location = 0) in vec2 fragTexCoord;
    layout(location = 1) in vec4 fragColor;
    layout(location = 2) in flat int fragTexIndex;
    layout(location = 3) in float fragBrightness;
    layout(location = 4) in flat int fragFaceMapIndex;
    layout(location = 5) in float fragSunAngle;
    layout(location = 6) in float fragAmbientLight;
    layout(location = 7) in float fragCameraFacing;

    layout(set = 1, binding = 0) uniform sampler2D textures[16384];

    layout(location = 0) out vec4 outColor;

    void main() {
        vec4 texColor = texture(textures[nonuniformEXT(fragTexIndex)], fragTexCoord);
        
        vec4 faceMapSample = texture(textures[nonuniformEXT(fragFaceMapIndex)], fragTexCoord);
        vec3 faceRaw = faceMapSample.rgb;
        float faceAlpha = faceMapSample.a;

        float total = faceRaw.r + faceRaw.g + faceRaw.b;
        
        vec3 weights;
        if (total < 0.001) {
            weights = vec3(0.0, 1.0, 0.0);
        } else {
            weights = faceRaw / total;
        }
        
        // Swizzle face map channels based on camera rotation.
        // The face map is baked: R=right, G=top, B=left (relative to FaceSouth).
        // When we rotate the camera, the "right" side of the tile becomes
        // a different side. For 90° rotations of an iso diamond,
        // left and right simply swap on odd rotations.
        int facing = int(fragCameraFacing + 0.5);
        vec3 w;
        if (facing == 0 || facing == 2) {
            // South or North: R=right, B=left (default)
            w = weights;
        } else {
            // West or East: the left/right sides swap
            w = vec3(weights.b, weights.g, weights.r);
        }
        
        // Sun direction rotated by camera facing
        // Each 90° CW rotation shifts apparent sun direction by +PI/2
        float facingOffset = float(facing) * 1.5707963;
        float baseAngle = fragSunAngle * 6.28318530718;
        float sunHeight = sin(baseAngle);
        float sunDir = cos(baseAngle + facingOffset);
        float ambient = fragAmbientLight;
        float sunIntensity = max(0.0, sunHeight);
        
        float topBright = ambient + (1.0 - ambient) * sunIntensity;
        
        float sideShadow = 0.85;
        float sideAmbient = ambient * sideShadow;
        float directStrength = (1.0 - ambient) * sunIntensity;
        float leftBright  = sideAmbient + directStrength * (0.4 + 0.6 * max(0.0, sunDir));
        float rightBright = sideAmbient + directStrength * (0.4 + 0.6 * max(0.0, -sunDir));
        
        float brightness = w.r * rightBright + w.g * topBright + w.b * leftBright;
        
        vec4 color = texColor * fragColor;
        color.rgb *= brightness * fragBrightness;
        outColor = vec4(color.rgb, color.a * faceAlpha);
    }
|]

-- | Bindless UI vertex shader (uses UI camera matrices)
-- NO pixel snap — UI vertices are already in integer pixel coordinates,
-- and the orthographic projection maps them 1:1 to screen pixels.
bindlessUIVertexShaderCode ∷ BS.ByteString
bindlessUIVertexShaderCode = [vert|
    #version 450
    #extension GL_ARB_separate_shader_objects : enable

    layout(location = 0) in vec2 inPosition;
    layout(location = 1) in vec2 inTexCoord;
    layout(location = 2) in vec4 inColor;
    layout(location = 3) in float inTexIndex;
    layout(location = 4) in float inFaceMapIndex;

    layout(set = 0, binding = 0) uniform UniformBufferObject {
        mat4 model;
        mat4 view;
        mat4 proj;
        mat4 uiView;
        mat4 uiProj;
        float brightness;
        float screenW;
        float screenH;
        float pixelSnap;
        float sunAngle;
        float ambientLight;
        float cameraFacing;
    } ubo;

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

-- | Bindless UI fragment shader — no face-map lighting, UI is unaffected by day/night
bindlessUIFragmentShaderCode ∷ BS.ByteString
bindlessUIFragmentShaderCode = [frag|
    #version 450
    #extension GL_ARB_separate_shader_objects : enable
    #extension GL_EXT_nonuniform_qualifier : enable

    layout(location = 0) in vec2 fragTexCoord;
    layout(location = 1) in vec4 fragColor;
    layout(location = 2) in flat int fragTexIndex;
    layout(location = 3) in float fragBrightness;

    layout(set = 1, binding = 0) uniform sampler2D textures[16384];

    layout(location = 0) out vec4 outColor;

    void main() {
        vec4 texColor = texture(textures[nonuniformEXT(fragTexIndex)], fragTexCoord);
        vec4 color = texColor * fragColor;
        color.rgb *= fragBrightness;
        outColor = color;
    }
|]

-- | Font UI vertex shader (uses UI projection matrix)
-- NO pixel snap — same reasoning as bindlessUIVertexShaderCode.
fontUIVertexShaderCode ∷ BS.ByteString
fontUIVertexShaderCode = [vert|
    #version 450

    layout(location = 0) in vec2 inPosition;
    layout(location = 1) in vec2 inTexCoord;
    layout(location = 2) in vec2 glyphPos;
    layout(location = 3) in vec2 glyphSize;
    layout(location = 4) in vec4 glyphUV;
    layout(location = 5) in vec4 glyphColor;

    layout(set = 0, binding = 0) uniform UniformBufferObject {
        mat4 model;
        mat4 view;
        mat4 proj;
        mat4 uiView;
        mat4 uiProj;
        float brightness;
        float screenW;
        float screenH;
        float pixelSnap;
        float sunAngle;
        float ambientLight;
        float cameraFacing;
    } ubo;

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
fontSDFFragmentShaderCode ∷ BS.ByteString
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
