-- | The Vulkan instance-configuration contract (#1402), pinned with no
--   GPU, driver, window, or display.
--
--   Before this spec, the only coverage of instance extension selection
--   was the graphical suite, which asserted that
--   @VK_KHR_portability_enumeration@ and
--   @VK_KHR_get_physical_device_properties2@ were always available and
--   that a debug messenger always existed in debug mode. Production has
--   treated all of those as optional since @5e54a5ff@: four extensions
--   and the validation layer are enabled only when discovered, because
--   enabling an absent extension fails instance creation with
--   @EXTENSION_NOT_PRESENT@. The spec pinned one machine's driver
--   inventory rather than the contract, so a valid Vulkan installation
--   without an optional extension failed the suite.
--
--   These specs pin the real contract instead, against synthetic
--   availability lists: which extensions are enabled and in what ORDER,
--   which layers, the create flags, the two independent debug gates,
--   whether the @LayerSettingsCreateInfoEXT@ pNext struct is chained,
--   and the one case that must be rejected — a GLFW surface extension
--   the driver does not offer, under 'InstanceForWindow' only.
module Test.Headless.Graphics.InstancePlan (spec) where

import UPrelude
import qualified Data.ByteString as BS
import Test.Hspec
import Engine.Graphics.Base (GraphicsConfig(..))
import Engine.Graphics.Vulkan.Instance.Plan
  (InstancePlan(..), InstancePlanError(..), InstanceSurfaceUse(..)
  ,planVulkanInstance)
import Vulkan.Core10.Enums.InstanceCreateFlagBits
  (InstanceCreateFlags
  ,InstanceCreateFlagBits(INSTANCE_CREATE_ENUMERATE_PORTABILITY_BIT_KHR))
import Vulkan.Zero (zero)

-- Every name below is written as a literal on purpose: this spec is an
-- independent statement of the wire strings production must use, so a
-- renamed constant has to be reflected here deliberately rather than
-- agreeing by construction.
surfaceExt, surfaceMetalExt, portabilityExt, props2Ext ∷ BS.ByteString
debugUtilsExt, layerSettingsExt, validationLayer ∷ BS.ByteString
surfaceExt       = "VK_KHR_surface"
surfaceMetalExt  = "VK_EXT_metal_surface"
portabilityExt   = "VK_KHR_portability_enumeration"
props2Ext        = "VK_KHR_get_physical_device_properties2"
debugUtilsExt    = "VK_EXT_debug_utils"
layerSettingsExt = "VK_EXT_layer_settings"
validationLayer  = "VK_LAYER_KHRONOS_validation"

-- | What GLFW asks for on a machine with a window system.
glfwExts ∷ [BS.ByteString]
glfwExts = [surfaceExt, surfaceMetalExt]

-- | A driver offering everything: the baseline each spec below spoils in
--   exactly one way.
fullExts ∷ [BS.ByteString]
fullExts =
  [surfaceExt, surfaceMetalExt, portabilityExt, props2Ext
  ,debugUtilsExt, layerSettingsExt]

-- | Availability lists are discovered in driver order, which is not the
--   order production enables them in. Shuffling the synthetic list keeps
--   the ordering assertions honest: they must pin the ORDER PRODUCTION
--   CHOOSES, not the order the driver happened to report.
shuffled ∷ [BS.ByteString] → [BS.ByteString]
shuffled = reverse

debugConfig, releaseConfig ∷ GraphicsConfig
debugConfig = GraphicsConfig
  { gcAppName = "InstancePlanTest", gcDebugMode = True, gcMaxFrames = 2 }
releaseConfig = debugConfig { gcDebugMode = False }

portabilityFlags ∷ InstanceCreateFlags
portabilityFlags = INSTANCE_CREATE_ENUMERATE_PORTABILITY_BIT_KHR

-- | Plan for a window instance against the given availability.
planWindow ∷ GraphicsConfig → [BS.ByteString] → [BS.ByteString]
           → Either InstancePlanError InstancePlan
planWindow config exts = planVulkanInstance config InstanceForWindow glfwExts exts

-- | The plan a spec expects to succeed, or a failure naming what came back.
expectPlan ∷ Either InstancePlanError InstancePlan → IO InstancePlan
expectPlan (Right plan) = pure plan
expectPlan (Left err) = do
  expectationFailure $ "expected a successful plan, got: " ⧺ show err
  error "unreachable"

spec ∷ Spec
spec = describe "Graphics.InstancePlan" $ do

  describe "a driver offering every optional capability" $ do
    it "enables all four extensions in production's own order, in debug mode" $ do
      plan ← expectPlan $ planWindow debugConfig (shuffled fullExts)
                                     (shuffled [validationLayer, "VK_LAYER_LUNARG_api_dump"])
      -- The exact ordered list, not membership: this is the byte-for-byte
      -- enabledExtensionNames handed to createInstance.
      ipEnabledExtensions plan `shouldBe`
        [ surfaceExt, surfaceMetalExt
        , debugUtilsExt, portabilityExt, props2Ext, layerSettingsExt ]
      ipEnabledLayers plan `shouldBe` [validationLayer]
      ipCreateFlags plan `shouldBe` portabilityFlags
      ipDebugMessenger plan `shouldBe` True
      ipValidationLayer plan `shouldBe` True
      ipLayerSettings plan `shouldBe` True

    it "enables neither debug utils nor validation when debug mode is off" $ do
      -- The two gates are independent of availability: gcDebugMode = False
      -- must suppress both even though the driver offers each.
      plan ← expectPlan $ planWindow releaseConfig (shuffled fullExts) [validationLayer]
      ipEnabledExtensions plan `shouldBe`
        [surfaceExt, surfaceMetalExt, portabilityExt, props2Ext, layerSettingsExt]
      ipEnabledLayers plan `shouldBe` []
      ipCreateFlags plan `shouldBe` portabilityFlags
      ipDebugMessenger plan `shouldBe` False
      ipValidationLayer plan `shouldBe` False
      -- Layer settings is NOT a debug capability: it stays chained.
      ipLayerSettings plan `shouldBe` True

  describe "an absent optional capability degrades rather than failing" $ do
    it "plans without VK_KHR_portability_enumeration and clears the flag" $ do
      let avail = filter (≢ portabilityExt) fullExts
      plan ← expectPlan $ planWindow debugConfig (shuffled avail) [validationLayer]
      ipEnabledExtensions plan `shouldBe`
        [surfaceExt, surfaceMetalExt, debugUtilsExt, props2Ext, layerSettingsExt]
      ipCreateFlags plan `shouldBe` zero
      ipDebugMessenger plan `shouldBe` True
      ipValidationLayer plan `shouldBe` True
      ipLayerSettings plan `shouldBe` True

    it "plans without VK_KHR_get_physical_device_properties2" $ do
      let avail = filter (≢ props2Ext) fullExts
      plan ← expectPlan $ planWindow debugConfig (shuffled avail) [validationLayer]
      ipEnabledExtensions plan `shouldBe`
        [surfaceExt, surfaceMetalExt, debugUtilsExt, portabilityExt, layerSettingsExt]
      ipCreateFlags plan `shouldBe` portabilityFlags
      ipDebugMessenger plan `shouldBe` True
      ipLayerSettings plan `shouldBe` True

    it "plans without VK_EXT_layer_settings and unchains its pNext struct" $ do
      let avail = filter (≢ layerSettingsExt) fullExts
      plan ← expectPlan $ planWindow debugConfig (shuffled avail) [validationLayer]
      ipEnabledExtensions plan `shouldBe`
        [surfaceExt, surfaceMetalExt, debugUtilsExt, portabilityExt, props2Ext]
      -- LayerSettingsCreateInfoEXT requires its own extension to be
      -- enabled (spec VU), so the chain must drop it too.
      ipLayerSettings plan `shouldBe` False
      ipDebugMessenger plan `shouldBe` True

    it "plans without VK_EXT_debug_utils while debug mode is on" $ do
      let avail = filter (≢ debugUtilsExt) fullExts
      plan ← expectPlan $ planWindow debugConfig (shuffled avail) [validationLayer]
      ipEnabledExtensions plan `shouldBe`
        [surfaceExt, surfaceMetalExt, portabilityExt, props2Ext, layerSettingsExt]
      ipDebugMessenger plan `shouldBe` False
      -- The validation layer is gated separately and survives.
      ipValidationLayer plan `shouldBe` True
      ipEnabledLayers plan `shouldBe` [validationLayer]

    it "plans without the validation layer while debug mode is on" $ do
      plan ← expectPlan $ planWindow debugConfig (shuffled fullExts)
                                     ["VK_LAYER_LUNARG_api_dump"]
      ipEnabledLayers plan `shouldBe` []
      ipValidationLayer plan `shouldBe` False
      -- The debug messenger is gated separately and survives.
      ipDebugMessenger plan `shouldBe` True
      ipEnabledExtensions plan `shouldBe`
        [ surfaceExt, surfaceMetalExt
        , debugUtilsExt, portabilityExt, props2Ext, layerSettingsExt ]

    it "plans on a driver offering nothing beyond the surface extensions" $ do
      plan ← expectPlan $ planWindow debugConfig glfwExts []
      ipEnabledExtensions plan `shouldBe` glfwExts
      ipEnabledLayers plan `shouldBe` []
      ipCreateFlags plan `shouldBe` zero
      ipDebugMessenger plan `shouldBe` False
      ipValidationLayer plan `shouldBe` False
      ipLayerSettings plan `shouldBe` False

  describe "a missing GLFW surface extension" $ do
    it "is rejected under InstanceForWindow, naming only what is missing" $ do
      let avail = filter (≢ surfaceMetalExt) fullExts
      planWindow debugConfig avail [validationLayer]
        `shouldBe` Left (MissingRequiredExtensions [surfaceMetalExt])

    it "reports every missing surface extension, in the order GLFW asked" $ do
      planWindow debugConfig [portabilityExt] []
        `shouldBe` Left (MissingRequiredExtensions [surfaceExt, surfaceMetalExt])

    it "is NOT rejected under InstanceOffscreen, and is not enabled either" $ do
      -- The offscreen mode (#650) never asks GLFW for extensions, because
      -- GLFW may not be initialized at all. Passing a nominal list that the
      -- driver cannot satisfy makes that independence observable here: the
      -- plan neither fails on it nor carries it.
      let avail = filter (≢ surfaceMetalExt) fullExts
      plan ← expectPlan $ planVulkanInstance debugConfig InstanceOffscreen
                                             glfwExts (shuffled avail)
                                             [validationLayer]
      ipEnabledExtensions plan `shouldBe`
        [debugUtilsExt, portabilityExt, props2Ext, layerSettingsExt]
      ipEnabledExtensions plan `shouldSatisfy` notElem surfaceExt
      ipEnabledExtensions plan `shouldSatisfy` notElem surfaceMetalExt
      ipEnabledLayers plan `shouldBe` [validationLayer]
      ipCreateFlags plan `shouldBe` portabilityFlags

    it "leaves an offscreen plan with no extensions at all on a bare driver" $ do
      plan ← expectPlan $ planVulkanInstance releaseConfig InstanceOffscreen
                                             glfwExts [] []
      ipEnabledExtensions plan `shouldBe` []
      ipEnabledLayers plan `shouldBe` []
      ipCreateFlags plan `shouldBe` zero
      ipDebugMessenger plan `shouldBe` False
      ipLayerSettings plan `shouldBe` False
