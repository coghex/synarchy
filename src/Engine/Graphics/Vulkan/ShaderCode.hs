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
    layout(location = 5) in uint inRenderFlags;
    layout(location = 6) in uint inWorldUV;

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
        float defaultFaceMapSlot;
        float worldCircumferenceTiles;
    } ubo;

    layout(location = 0) out vec2 fragTexCoord;
    layout(location = 1) out vec4 fragColor;
    layout(location = 2) out flat int fragTexIndex;
    layout(location = 3) out float fragBrightness;
    layout(location = 4) out flat int fragFaceMapIndex;
    layout(location = 5) out float fragSunAngle;
    layout(location = 7) out float fragCameraFacing;
    layout(location = 8) out flat uint fragRenderFlags;
    layout(location = 9) out flat int fragDefaultFaceMapSlot;

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
        // inTexIndex / inFaceMapIndex now carry a STABLE texture-handle id
        // (#286), resolved to a live bindless slot in the fragment shader.
        fragTexIndex = int(inTexIndex);
        fragBrightness = ubo.brightness;
        fragFaceMapIndex = int(inFaceMapIndex);

        // Longitude-local day/night (#483): decode the tile's packed
        // world u (low 16 bits, sign-restored) and offset the global
        // sun angle by its fraction of a full trip around the world
        // cylinder (u = gx - gy). Raw world coords, NOT screen-space —
        // rotating the camera must not re-light the world.
        //
        // Deliberately NOT wrapped (no fract()) before interpolation:
        // a quad whose corners straddle the wrap point (e.g. u values
        // giving 0.99 and 0.01) would otherwise interpolate through
        // 0.5 — a false noon band across what should read as smooth
        // midnight. The unwrapped phase is smooth and continuous
        // everywhere (no per-vertex discontinuity to interpolate
        // across), and every consumer of fragSunAngle downstream
        // (this fragment shader's sin/cos and computeAmbientLight) is
        // built from sin()/cos(), which are exactly periodic for any
        // real input — so no fract() is needed anywhere in the pipeline.
        int rawU = int(inWorldUV & 0xFFFFu);
        if (rawU >= 32768) rawU -= 65536;
        float circumference = max(ubo.worldCircumferenceTiles, 1.0);
        fragSunAngle = ubo.sunAngle + float(rawU) / circumference;

        fragCameraFacing = ubo.cameraFacing;
        fragRenderFlags = inRenderFlags;
        fragDefaultFaceMapSlot = int(ubo.defaultFaceMapSlot);
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
    // Longitude-local day/night (#483): UNWRAPPED phase (no fract() on
    // the vertex side — see bindlessVertexShaderCode). Interpolated
    // across the primitive, then wrapped/lit here per-fragment so a
    // quad straddling the wrap point reads as smooth midnight instead
    // of a false noon band.
    layout(location = 5) in float fragSunAngle;
    layout(location = 7) in float fragCameraFacing;
    layout(location = 8) in flat uint fragRenderFlags;
    layout(location = 9) in flat int fragDefaultFaceMapSlot;

    layout(set = 1, binding = 0) uniform sampler2D textures[16384];

    // Ambient-light curve (#483) — GLSL port of Engine.Loop.Frame's
    // computeAmbientLight. Evaluated here (fragment-side, per-pixel)
    // from the per-fragment interpolated LOCAL sun angle, built
    // entirely from sin() — exactly periodic for any real input, so
    // it's correct whether or not fragSunAngle has been reduced to
    // [0,1).
    float computeAmbientLight(float sunAngle) {
        float angle = sunAngle * 6.28318530718;
        float sunHeight = sin(angle);
        if (sunHeight >= 0.0) {
            return 0.5 + 0.2 * sunHeight;
        } else {
            return 0.15 + 0.35 * (1.0 + sunHeight);
        }
    }

    // Handle→slot table (#286). Vertices carry a STABLE texture-handle id;
    // this maps it to the live bindless slot at draw time, so cached
    // geometry can never encode a stale/recycled slot. HANDLE_TABLE_SIZE
    // MUST match 'handleSlotTableSize' in Texture.Bindless.
    const int HANDLE_TABLE_SIZE = 65536;
    layout(set = 1, binding = 1, std430) readonly buffer HandleSlotTable {
        uint handleToSlot[HANDLE_TABLE_SIZE];
    };

    int resolveSlot(int handleId) {
        if (handleId < 0 || handleId >= HANDLE_TABLE_SIZE) return 0;
        return int(handleToSlot[handleId]);
    }

    layout(location = 0) out vec4 outColor;

    void main() {
        int texSlot = resolveSlot(fragTexIndex);
        vec4 texColor = texture(textures[nonuniformEXT(texSlot)], fragTexCoord);

        // Selection outline: bit 0 of renderFlags.
        // For each fragment, sample the 4 cardinal neighbor texels (one
        // texture-pixel away). If this fragment is transparent AND any
        // neighbor is opaque, this is an edge — emit pure white.
        // Otherwise fall through to normal shading.
        if ((fragRenderFlags & 1u) != 0u) {
            vec2 texSize = vec2(textureSize(textures[nonuniformEXT(texSlot)], 0));
            vec2 px = 1.0 / texSize;
            float aN = texture(textures[nonuniformEXT(texSlot)], fragTexCoord + vec2(0.0, -px.y)).a;
            float aS = texture(textures[nonuniformEXT(texSlot)], fragTexCoord + vec2(0.0,  px.y)).a;
            float aW = texture(textures[nonuniformEXT(texSlot)], fragTexCoord + vec2(-px.x, 0.0)).a;
            float aE = texture(textures[nonuniformEXT(texSlot)], fragTexCoord + vec2( px.x, 0.0)).a;
            float maxN = max(max(aN, aS), max(aW, aE));
            if (texColor.a < 0.5 && maxN >= 0.5) {
                outColor = vec4(1.0, 1.0, 1.0, 1.0);
                return;
            }
        }

        // Face map: a handle resolving to slot 0 (unregistered) falls back
        // to the default face map — the old 'lookupFmSlot' rule, now here.
        int fmSlot = resolveSlot(fragFaceMapIndex);
        if (fmSlot == 0) fmSlot = fragDefaultFaceMapSlot;
        vec4 faceMapSample = texture(textures[nonuniformEXT(fmSlot)], fragTexCoord);
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
        float ambient = computeAmbientLight(fragSunAngle);
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

    // Handle→slot table (#286) — same buffer the world fragment shader
    // reads; fragTexIndex carries a stable texture-handle id.
    const int HANDLE_TABLE_SIZE = 65536;
    layout(set = 1, binding = 1, std430) readonly buffer HandleSlotTable {
        uint handleToSlot[HANDLE_TABLE_SIZE];
    };

    layout(location = 0) out vec4 outColor;

    void main() {
        int texSlot = (fragTexIndex >= 0 && fragTexIndex < HANDLE_TABLE_SIZE)
                    ? int(handleToSlot[fragTexIndex]) : 0;
        vec4 texColor = texture(textures[nonuniformEXT(texSlot)], fragTexCoord);
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
