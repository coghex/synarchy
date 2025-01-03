{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}

module Engine.Graphics.Vulkan.ShaderCode
    ( vertexShaderCode
    , fragmentShaderCode
    ) where

import Vulkan.Utils.ShaderQQ.GLSL.Glslang (vert, frag)
import qualified Data.ByteString as BS

vertexShaderCode :: BS.ByteString
vertexShaderCode = [vert|
    #version 450
    #extension GL_ARB_separate_shader_objects : enable

|]

fragmentShaderCode :: BS.ByteString
fragmentShaderCode = [frag|
    #version 450
    #extension GL_ARB_separate_shader_objects : enable

|]
