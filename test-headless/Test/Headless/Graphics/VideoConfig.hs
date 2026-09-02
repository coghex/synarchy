-- | Video-config YAML parsing tests (issue #433) and the video-config
--   domain (#2198).
--
--   #433's contract: the legacy-@fullscreen@ fallback (taken whenever
--   @window_mode@ is absent) must treat @fullscreen@ itself as
--   *optional* — a @video:@ section with neither key parses to windowed
--   defaults instead of failing the whole parse and silently resetting
--   every video setting back to 'defaultVideoConfig'.
--
--   #2198's contract, the @video config validation@ describe: one
--   authoritative domain ('Engine.Graphics.Config.Domain') applied at
--   every boundary.
--
--   * YAML loading defaults an out-of-domain leaf FIELD BY FIELD, with
--     a warning naming the file, the full field and the value, while a
--     structural failure keeps the whole-file default.
--   * Every Lua setter returns @true@ for a valid call and @false@ for
--     a type-invalid or out-of-domain one, having changed nothing:
--     not the config, not a companion mirror, and no queued render
--     message. The combined @engine.setVideoConfig@ is atomic across
--     all ten arguments.
--   * 'saveVideoConfig' refuses to write an invalid in-memory config.
--   * 'Engine.Core.Init.migrateLegacyConfig' decodes 'VideoConfigFile',
--     whose token strictness is unchanged: a legacy file with an
--     unknown token stays non-migratable.
--   * The Settings screen's bounds equal the engine's.
--
--   The Lua-setter group boots one headless engine (no world worker —
--   nothing here touches a page) with the REAL registered engine API,
--   inside 'withIsolatedResourceRoot' because the Settings
--   @loadDefaults@ case drives production config-writing paths.
module Test.Headless.Graphics.VideoConfig (spec) where

import UPrelude
import Test.Hspec
import Data.Aeson (FromJSON(..), decode, withObject, (.:))
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import Data.IORef (newIORef, readIORef, writeIORef, atomicModifyIORef', modifyIORef')
import Data.Proxy (Proxy(..))
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Yaml as Yaml
import System.Directory (doesFileExist)
import System.FilePath ((</>))
import Engine.Core.Init (migrateLegacyConfig, LegacyNeutralityCheck(..))
import Engine.Core.Log
  ( LogBackend(..), LogConfig(..), LogEntry(..), LogLevel(..), LoggerState
  , defaultLogConfig, initLogger )
import qualified Engine.Core.Queue as Q
import Engine.Core.State (EngineEnv(..))
import Engine.Core.Thread (ThreadControl(..))
import Engine.Graphics.Config
import Engine.Graphics.Config.Domain
import Engine.Scripting.Lua.API (registerLuaAPI)
import Engine.Scripting.Lua.Thread (createLuaBackendState)
import Engine.Scripting.Lua.Thread.Console (executeDebugLua)
import Engine.Scripting.Lua.Types (LuaBackendState(..), LuaToEngineMsg(..))
import Test.Headless.Harness (withHeadlessEngineNoWorld)
import Test.Headless.Harness.Isolation
  (withExclusiveTempDirectory, withIsolatedResourceRoot)
import UI.Types
  (UIPageManager(..), TooltipState(..), TooltipStyle(..), emptyUIPageManager)

-- * Shared helpers

-- | Parse a video-config YAML document or fail the test loudly.
parseConfig ∷ ByteString → VideoConfigFile
parseConfig bs = case Yaml.decodeEither' bs of
    Left err  → error ("decode failed: " ⧺ show err)
    Right cfg → cfg

minimalVideo ∷ ByteString
minimalVideo = "video:\n  resolution:\n    width: 1280\n    height: 720\n"

-- | A logger whose entries can be drained (returned in emission order
--   and cleared) so one fixture can serve many examples.
newDrainingLogger ∷ IO (LoggerState, IO [LogEntry])
newDrainingLogger = do
    ref ← newIORef []
    logger ← initLogger defaultLogConfig
        { lcBackend = LogToCallback (\e → atomicModifyIORef' ref (\es → (e : es, ()))) }
    pure (logger, reverse ⊚ atomicModifyIORef' ref (\es → ([], es)))

warningsOf ∷ [LogEntry] → [Text]
warningsOf = map leMessage ∘ filter ((≡ LevelWarn) ∘ leLevel)

infosOf ∷ [LogEntry] → [Text]
infosOf = map leMessage ∘ filter ((≡ LevelInfo) ∘ leLevel)

-- | The baseline document: every leaf present and every leaf DIFFERENT
--   from 'defaultVideoConfig', so a leaf that silently took the default
--   is visible.
baseLeaves ∷ [(Text, Text)]
baseLeaves =
    [ ("width", "1920"), ("height", "1080"), ("window_mode", "borderless")
    , ("ui_scale", "2.0"), ("vsync", "false"), ("frame_limit", "120")
    , ("msaa", "4"), ("brightness", "150"), ("pixel_snap", "true")
    , ("texture_filter", "linear"), ("tooltip_dwell_ms", "250")
    , ("tooltip_hint_delay_ms", "600") ]

baseConfig ∷ VideoConfig
baseConfig = VideoConfig
    { vcWidth = 1920, vcHeight = 1080, vcWindowMode = BorderlessWindowed
    , vcUIScale = 2.0, vcVSync = False, vcFrameLimit = Just 120, vcMSAA = 4
    , vcBrightness = 150, vcPixelSnap = True, vcTextureFilter = FilterLinear
    , vcTooltipDwellMs = 250, vcTooltipHintDelayMs = 600 }

resolutionKeys ∷ [Text]
resolutionKeys = ["width", "height"]

-- | Render a leaf list as a document: the resolution keys nest under
--   @resolution:@, everything else sits flat under @video:@.
renderDoc ∷ [(Text, Text)] → ByteString
renderDoc leaves = TE.encodeUtf8 $ T.unlines $
    [ "video:", "  resolution:" ]
    ⧺ [ "    " <> k <> ": " <> v | (k, v) ← leaves, k `elem` resolutionKeys ]
    ⧺ [ "  " <> k <> ": " <> v | (k, v) ← leaves, k `notElem` resolutionKeys ]

-- | The baseline with some leaves overridden.
docWith ∷ [(Text, Text)] → ByteString
docWith overrides =
    renderDoc [ (k, fromMaybe v (lookup k overrides)) | (k, v) ← baseLeaves ]

-- | The baseline with some leaves overridden AND extra leaves appended
--   (for keys the baseline does not carry, such as legacy @fullscreen@).
docPlus ∷ [(Text, Text)] → [(Text, Text)] → ByteString
docPlus overrides extra =
    renderDoc ([ (k, fromMaybe v (lookup k overrides)) | (k, v) ← baseLeaves ] ⧺ extra)

-- | The baseline with some leaves removed.
docWithout ∷ [Text] → ByteString
docWithout dropped = renderDoc [ leaf | leaf@(k, _) ← baseLeaves, k `notElem` dropped ]

-- | Write @doc@ into @dir@ and load it through the real loader,
--   returning the config and every warning the loader emitted.
loadDoc ∷ FilePath → ByteString → IO (VideoConfig, [Text])
loadDoc dir doc = do
    let path = dir </> "video.yaml"
    BS.writeFile path doc
    (logger, drain) ← newDrainingLogger
    cfg ← loadVideoConfig logger path
    warnings ← warningsOf ⊚ drain
    pure (cfg, warnings)

inTemp ∷ (FilePath → IO α) → IO α
inTemp = withExclusiveTempDirectory "video-config"

shouldContainText ∷ Text → Text → Expectation
shouldContainText haystack needle =
    (needle `T.isInfixOf` haystack, T.unpack haystack)
      `shouldBe` (True, T.unpack haystack)

-- * The Lua fixture

data Fixture = Fixture
    { fxEnv   ∷ EngineEnv
    , fxLua   ∷ LuaBackendState
    , fxDrain ∷ IO [LogEntry]
    }

-- | A bare Lua backend with the real engine API registered — the same
--   fixture 'Test.Headless.UI.SettingsRevert' uses — plus a draining
--   logger installed on the engine so a refusal's warning is visible.
withLuaFixture ∷ (Fixture → IO ()) → IO ()
withLuaFixture action = withIsolatedResourceRoot $ withHeadlessEngineNoWorld $ \env → do
    (logger, drain) ← newDrainingLogger
    writeIORef (loggerRef env) logger
    ls ← createLuaBackendState (luaToEngineQueue env) (luaQueue env)
                               (assetPoolRef env) (nextObjectIdRef env)
                               (inputStateRef env) (loggerRef env)
    stateRef ← newIORef ThreadRunning
    registerLuaAPI (lbsLuaState ls) env ls stateRef
    action (Fixture env ls drain)

-- | The debug console is single-line, so a scenario is spelled as
--   semicolon-separated statements joined here.
luaLines ∷ [Text] → Text
luaLines = T.intercalate " "

evalOk ∷ LuaBackendState → Text → IO Text
evalOk ls code = do
    t ← executeDebugLua (lbsLuaState ls) code
    when ("error:" `T.isPrefixOf` t ∨ "syntax error:" `T.isPrefixOf` t) $
        expectationFailure ("Lua error: " ⧺ T.unpack t)
    pure t

decodeOr ∷ FromJSON α ⇒ Text → IO α
decodeOr t = case decode (BL.fromStrict (TE.encodeUtf8 t)) of
    Just v  → pure v
    Nothing → fail ("undecodable Lua result: " ⧺ T.unpack t)

-- | Put the engine at the baseline: 'baseConfig' in the config ref,
--   every companion mirror agreeing with it, an empty Lua→engine queue
--   and an empty log.
resetLua ∷ Fixture → IO ()
resetLua fx = do
    let env = fxEnv fx
    writeIORef (videoConfigRef env) baseConfig
    writeIORef (textureFilterRef env) (vcTextureFilter baseConfig)
    writeIORef (pixelSnapRef env) (vcPixelSnap baseConfig)
    writeIORef (uiManagerRef env) emptyUIPageManager
    modifyIORef' (uiManagerRef env) $ \mgr →
        let tts = upmTooltip mgr
            style = (ttsStyle tts)
                { tsDwellMs = fromIntegral (vcTooltipDwellMs baseConfig)
                , tsHintDelayMs = fromIntegral (vcTooltipHintDelayMs baseConfig) }
        in mgr { upmTooltip = tts { ttsStyle = style } }
    void $ Q.flushQueue (luaToEngineQueue env)
    void $ fxDrain fx

-- | Everything a setter may touch: the config and its three immediate
--   mirrors (texture filter, pixel snap, the live tooltip timings).
data Snapshot = Snapshot VideoConfig TextureFilter Bool (Float, Float)
    deriving (Eq, Show)

snapshot ∷ EngineEnv → IO Snapshot
snapshot env = Snapshot
    ⊚ readIORef (videoConfigRef env)
    <*> readIORef (textureFilterRef env)
    <*> readIORef (pixelSnapRef env)
    <*> (tooltipTimings ⊚ readIORef (uiManagerRef env))

tooltipTimings ∷ UIPageManager → (Float, Float)
tooltipTimings mgr =
    let style = ttsStyle (upmTooltip mgr)
    in (tsDwellMs style, tsHintDelayMs style)

queued ∷ Fixture → IO [LuaToEngineMsg]
queued fx = Q.flushQueue (luaToEngineQueue (fxEnv fx))

storedConfig ∷ Fixture → IO VideoConfig
storedConfig fx = readIORef (videoConfigRef (fxEnv fx))

-- | Drive one call that must be refused: it returns @false@, nothing
--   in the snapshot changed, nothing was enqueued, and a warning names
--   the rejected field.
refused ∷ Fixture → Text → IO ()
refused fx call = do
    resetLua fx
    before ← snapshot (fxEnv fx)
    r ← evalOk (fxLua fx) ("return " <> call)
    (T.unpack call, r) `shouldBe` (T.unpack call, "false")
    after ← snapshot (fxEnv fx)
    (T.unpack call, after) `shouldBe` (T.unpack call, before)
    msgs ← queued fx
    (T.unpack call, map show msgs) `shouldBe` (T.unpack call, [])
    warnings ← warningsOf ⊚ fxDrain fx
    (T.unpack call, any (" rejected video." `T.isInfixOf`) warnings)
      `shouldBe` (T.unpack call, True)

-- | Drive one call that must succeed: it returns @true@.
accepted ∷ Fixture → Text → IO ()
accepted fx call = do
    r ← evalOk (fxLua fx) ("return " <> call)
    (T.unpack call, r) `shouldBe` (T.unpack call, "true")

-- | Every per-field call the domain refuses, one example each.
rejectedCalls ∷ [Text]
rejectedCalls =
    [ "engine.setUIScale(0.25)", "engine.setUIScale(4.5)"
    , "engine.setUIScale(0/0)", "engine.setUIScale(math.huge)"
    , "engine.setUIScale(-math.huge)", "engine.setUIScale(1e39)"
    , "engine.setUIScale('abc')", "engine.setUIScale()"
    , "engine.setFrameLimit(-1)", "engine.setFrameLimit(1)"
    , "engine.setFrameLimit(29)", "engine.setFrameLimit(241)"
    , "engine.setFrameLimit(1.5)", "engine.setFrameLimit('abc')"
    , "engine.setResolution(0, 720)", "engine.setResolution(1280, 0)"
    , "engine.setResolution(-1, 720)", "engine.setResolution(1280, -1)"
    , "engine.setResolution('a', 720)", "engine.setResolution(1280.5, 720)"
    , "engine.setWindowMode('sideways')", "engine.setWindowMode(5)"
    , "engine.setWindowMode()"
    , "engine.setMSAA(0)", "engine.setMSAA(3)", "engine.setMSAA(16)"
    , "engine.setMSAA()", "engine.setMSAA('abc')"
    , "engine.setBrightness(49)", "engine.setBrightness(301)"
    , "engine.setBrightness()"
    , "engine.setTextureFilter('cubic')", "engine.setTextureFilter()"
    , "engine.setTooltipDwellMs(-1)", "engine.setTooltipDwellMs(1001)"
    , "engine.setTooltipDwellMs(1.5)", "engine.setTooltipDwellMs('abc')"
    , "engine.setTooltipDwellMs()"
    , "engine.setTooltipHintDelayMs(-1)", "engine.setTooltipHintDelayMs(1001)"
    , "engine.setTooltipHintDelayMs(1.5)"
    ]

-- | The combined setter with every argument valid, and the config it
--   writes (tooltips are not among its ten arguments and stay at the
--   baseline).
validCombined ∷ Text
validCombined =
    "engine.setVideoConfig(1280, 720, 'WINDOWED', 1.5, true, 0, 8, 120, false, 'NEAREST')"

expectedCombined ∷ VideoConfig
expectedCombined = baseConfig
    { vcWidth = 1280, vcHeight = 720, vcWindowMode = Windowed, vcUIScale = 1.5
    , vcVSync = True, vcFrameLimit = Nothing, vcMSAA = 8, vcBrightness = 120
    , vcPixelSnap = False, vcTextureFilter = FilterNearest }

-- | 'validCombined' with exactly one argument out of the domain.
combinedRejections ∷ [(String, Text)]
combinedRejections =
    [ ("width 0",               combined "0, 720, 'windowed', 1.5, true, 0, 8, 120, false, 'nearest'")
    , ("width 1280.5",          combined "1280.5, 720, 'windowed', 1.5, true, 0, 8, 120, false, 'nearest'")
    , ("height -1",             combined "1280, -1, 'windowed', 1.5, true, 0, 8, 120, false, 'nearest'")
    , ("window mode sideways",  combined "1280, 720, 'sideways', 1.5, true, 0, 8, 120, false, 'nearest'")
    , ("ui scale 0.25",         combined "1280, 720, 'windowed', 0.25, true, 0, 8, 120, false, 'nearest'")
    , ("ui scale NaN",          combined "1280, 720, 'windowed', 0/0, true, 0, 8, 120, false, 'nearest'")
    , ("ui scale 1e39",         combined "1280, 720, 'windowed', 1e39, true, 0, 8, 120, false, 'nearest'")
    , ("frame limit 10",        combined "1280, 720, 'windowed', 1.5, true, 10, 8, 120, false, 'nearest'")
    , ("frame limit -1",        combined "1280, 720, 'windowed', 1.5, true, -1, 8, 120, false, 'nearest'")
    , ("msaa 3",                combined "1280, 720, 'windowed', 1.5, true, 0, 3, 120, false, 'nearest'")
    , ("brightness 20",         combined "1280, 720, 'windowed', 1.5, true, 0, 8, 20, false, 'nearest'")
    , ("brightness 301",        combined "1280, 720, 'windowed', 1.5, true, 0, 8, 301, false, 'nearest'")
    , ("texture filter cubic",  combined "1280, 720, 'windowed', 1.5, true, 0, 8, 120, false, 'cubic'")
    ]
  where
    combined args = "engine.setVideoConfig(" <> args <> ")"

-- | What @scripts/settings/data.lua@ believes the bounds are.
data SettingsBounds = SettingsBounds
    { sbUiScaleMin, sbUiScaleMax ∷ Double
    , sbFrameLimitMin, sbFrameLimitMax ∷ Int
    , sbBrightnessMin, sbBrightnessMax ∷ Int
    , sbTooltipDwellMin, sbTooltipDwellMax ∷ Int
    , sbTooltipHintDelayMin, sbTooltipHintDelayMax ∷ Int
    , sbMsaa ∷ [Int]
    , sbModes ∷ [Text]
    , sbFilters ∷ [Text]
    }

instance FromJSON SettingsBounds where
    parseJSON = withObject "SettingsBounds" $ \o → SettingsBounds
        ⊚ o .: "uiScaleMin" <*> o .: "uiScaleMax"
        <*> o .: "frameLimitMin" <*> o .: "frameLimitMax"
        <*> o .: "brightnessMin" <*> o .: "brightnessMax"
        <*> o .: "tooltipDwellMin" <*> o .: "tooltipDwellMax"
        <*> o .: "tooltipHintDelayMin" <*> o .: "tooltipHintDelayMax"
        <*> o .: "msaa" <*> o .: "modes" <*> o .: "filters"

data ScalePair = ScalePair { spCurrent ∷ Double, spStored ∷ Double }

instance FromJSON ScalePair where
    parseJSON = withObject "ScalePair" $ \o →
        ScalePair ⊚ o .: "current" <*> o .: "stored"

-- * The spec

spec ∷ Spec
spec = do
    describe "legacy fullscreen fallback" $ do
        it "parses when neither window_mode nor fullscreen is present" $ do
            let cfg = parseConfig minimalVideo
            vfWindowMode cfg `shouldBe` Windowed
            -- The point of #433: the rest of the section survives too.
            vfResolution cfg `shouldBe` Resolution 1280 720

        it "maps legacy fullscreen: true to Fullscreen" $ do
            let cfg = parseConfig (minimalVideo <> "  fullscreen: true\n")
            vfWindowMode cfg `shouldBe` Fullscreen

        it "maps legacy fullscreen: false to Windowed" $ do
            let cfg = parseConfig (minimalVideo <> "  fullscreen: false\n")
            vfWindowMode cfg `shouldBe` Windowed

        it "prefers window_mode over the legacy key when both appear" $ do
            let cfg = parseConfig
                    (minimalVideo <> "  window_mode: borderless\n  fullscreen: true\n")
            vfWindowMode cfg `shouldBe` BorderlessWindowed

    describe "video config validation" $ do
        domainSpec
        yamlSpec
        saveSpec
        migrationSpec
        aroundAll withLuaFixture luaSpec

-- | The domain itself, pinned to the numbers the issue states.
domainSpec ∷ Spec
domainSpec = describe "domain" $ do
    it "pins the bounds" $ do
        (uiScaleMin, uiScaleMax) `shouldBe` (0.5, 4.0)
        (frameLimitMin, frameLimitMax) `shouldBe` (30, 240)
        msaaChoices `shouldBe` [1, 2, 4, 8]
        (brightnessMin, brightnessMax) `shouldBe` (50, 300)
        (tooltipMsMin, tooltipMsMax) `shouldBe` (0, 1000)

    it "accepts defaultVideoConfig and every optional-field default" $ do
        validateVideoConfig defaultVideoConfig `shouldBe` []
        defaultVideoConfig `shouldBe` VideoConfig 800 600 Windowed 1.0 True Nothing
                                                  1 100 False FilterNearest 400 400

    it "treats every endpoint as inside the domain" $ do
        checkUIScale 0.5 `shouldBe` Nothing
        checkUIScale 4.0 `shouldBe` Nothing
        checkFrameLimit "0" Nothing `shouldBe` Nothing
        checkFrameLimit "0" (Just 30) `shouldBe` Nothing
        checkFrameLimit "0" (Just 240) `shouldBe` Nothing
        map checkMSAA [1, 2, 4, 8] `shouldBe` [Nothing, Nothing, Nothing, Nothing]
        checkBrightness 50 `shouldBe` Nothing
        checkBrightness 300 `shouldBe` Nothing
        checkTooltipMs fieldTooltipDwellMs 0 `shouldBe` Nothing
        checkTooltipMs fieldTooltipDwellMs 1000 `shouldBe` Nothing
        checkDimension fieldWidth 1 `shouldBe` Nothing

    it "rejects NaN and both infinities" $ do
        checkUIScale (0 / 0) `shouldSatisfy` isJust
        checkUIScale (1 / 0) `shouldSatisfy` isJust
        checkUIScale (-1 / 0) `shouldSatisfy` isJust

    it "judges ui_scale AFTER narrowing to the stored Float" $ do
        -- 1e39 is a finite Double and an infinite Float.
        let narrowed = narrowUIScale 1.0e39
        isInfinite (1.0e39 ∷ Double) `shouldBe` False
        isInfinite narrowed `shouldBe` True
        checkUIScale narrowed `shouldSatisfy` isJust
        checkUIScale (narrowUIScale 2.0) `shouldBe` Nothing
        -- Narrowing keeps NaN a NaN (the unoptimised realToFrac does not).
        isNaN (narrowUIScale (0 / 0)) `shouldBe` True

    it "names every rejected field with its value and domain" $
        validateVideoConfig baseConfig
            { vcWidth = 0, vcHeight = -1, vcUIScale = 0.25, vcFrameLimit = Just 29
            , vcMSAA = 3, vcBrightness = 301, vcTooltipDwellMs = -1
            , vcTooltipHintDelayMs = 1001 }
          `shouldBe`
            [ VideoFieldRejection fieldWidth "0" dimensionDomain
            , VideoFieldRejection fieldHeight "-1" dimensionDomain
            , VideoFieldRejection fieldUIScale "0.25" uiScaleDomain
            , VideoFieldRejection fieldFrameLimit "29" (frameLimitDomain "unlimited")
            , VideoFieldRejection fieldMSAA "3" msaaDomain
            , VideoFieldRejection fieldBrightness "301" brightnessDomain
            , VideoFieldRejection fieldTooltipDwellMs "-1" tooltipMsDomain
            , VideoFieldRejection fieldTooltipHintDelayMs "1001" tooltipMsDomain
            ]

-- | Loader behaviour, document by document.
yamlSpec ∷ Spec
yamlSpec = describe "YAML loading" $ do
    it "loads the all-valid baseline exactly, with no warning" $ inTemp $ \dir → do
        (cfg, warnings) ← loadDoc dir (docWith [])
        cfg `shouldBe` baseConfig
        warnings `shouldBe` []

    -- Each domain-bearing leaf with an invalid value: only THAT leaf
    -- takes its default, every other leaf survives, and one warning
    -- names the file, the full field and the rejected value.
    forM_ leafCases $ \(label, overrides, expected, field, value) →
        it ("defaults only the rejected leaf: " ⧺ label) $ inTemp $ \dir → do
            (cfg, warnings) ← loadDoc dir (docWith overrides)
            cfg `shouldBe` expected
            length warnings `shouldBe` 1
            let warning = T.concat warnings
            warning `shouldContainText` T.pack (dir </> "video.yaml")
            warning `shouldContainText` (field <> " = " <> value <> " ")

    it "defaults width and height independently" $ inTemp $ \dir → do
        (cfg, warnings) ← loadDoc dir (docWith [("width", "0"), ("height", "-2")])
        cfg `shouldBe` baseConfig { vcWidth = 800, vcHeight = 600 }
        length warnings `shouldBe` 2
        T.concat warnings `shouldContainText` (fieldWidth <> " = 0 ")
        T.concat warnings `shouldContainText` (fieldHeight <> " = -2 ")

    it "matches known window-mode and texture-filter tokens case-insensitively" $ inTemp $ \dir → do
        (cfg, warnings) ← loadDoc dir
            (docWith [("window_mode", "BORDERLESS"), ("texture_filter", "Linear")])
        cfg `shouldBe` baseConfig
        warnings `shouldBe` []

    it "gives a present but unknown window_mode the default mode, never the legacy fullscreen key" $ inTemp $ \dir → do
        (cfg, warnings) ← loadDoc dir
            (docPlus [("window_mode", "sideways")] [("fullscreen", "true")])
        vcWindowMode cfg `shouldBe` Windowed
        cfg `shouldBe` baseConfig { vcWindowMode = Windowed }
        length warnings `shouldBe` 1
        T.concat warnings `shouldContainText` (fieldWindowMode <> " = sideways ")

    it "consults the legacy fullscreen key only when window_mode is absent" $ inTemp $ \dir → do
        (cfg, warnings) ← loadDoc dir
            (renderDoc ([ leaf | leaf@(k, _) ← baseLeaves, k ≢ "window_mode" ]
                        ⧺ [("fullscreen", "true")]))
        cfg `shouldBe` baseConfig { vcWindowMode = Fullscreen }
        warnings `shouldBe` []

    it "reads a null or omitted frame_limit as unlimited without a warning" $ inTemp $ \dir → do
        (cfgNull, warningsNull) ← loadDoc dir (docWith [("frame_limit", "null")])
        cfgNull `shouldBe` baseConfig { vcFrameLimit = Nothing }
        warningsNull `shouldBe` []
        (cfgAbsent, warningsAbsent) ← loadDoc dir (docWithout ["frame_limit"])
        cfgAbsent `shouldBe` baseConfig { vcFrameLimit = Nothing }
        warningsAbsent `shouldBe` []

    it "keeps every optional-field default when the leaf is omitted" $ inTemp $ \dir → do
        (cfg, warnings) ← loadDoc dir (docWithout
            [ "window_mode", "ui_scale", "vsync", "frame_limit", "msaa", "brightness"
            , "pixel_snap", "texture_filter", "tooltip_dwell_ms", "tooltip_hint_delay_ms" ])
        cfg `shouldBe` defaultVideoConfig { vcWidth = 1920, vcHeight = 1080 }
        warnings `shouldBe` []

    -- Structural failures keep the whole-file fallback, as before.
    forM_ structuralCases $ \(label, doc) →
        it ("falls back to the whole default on a structural failure: " ⧺ label) $ inTemp $ \dir → do
            (cfg, warnings) ← loadDoc dir doc
            cfg `shouldBe` defaultVideoConfig
            length warnings `shouldBe` 1
            T.concat warnings `shouldContainText` "Error loading video config"
  where
    leafCases ∷ [(String, [(Text, Text)], VideoConfig, Text, Text)]
    leafCases =
        [ ("width 0",            [("width", "0")],            baseConfig { vcWidth = 800 },  fieldWidth, "0")
        , ("height -5",          [("height", "-5")],          baseConfig { vcHeight = 600 }, fieldHeight, "-5")
        , ("window_mode sideways", [("window_mode", "sideways")], baseConfig { vcWindowMode = Windowed }, fieldWindowMode, "sideways")
        , ("ui_scale 0.25",      [("ui_scale", "0.25")],      baseConfig { vcUIScale = 1.0 }, fieldUIScale, "0.25")
        , ("ui_scale 4.5",       [("ui_scale", "4.5")],       baseConfig { vcUIScale = 1.0 }, fieldUIScale, "4.5")
        , ("ui_scale +inf",      [("ui_scale", "+inf")],      baseConfig { vcUIScale = 1.0 }, fieldUIScale, "Infinity")
        , ("ui_scale -inf",      [("ui_scale", "-inf")],      baseConfig { vcUIScale = 1.0 }, fieldUIScale, "-Infinity")
        , ("ui_scale 1e39 (finite as written, infinite once narrowed to Float)",
                                 [("ui_scale", "1e39")],      baseConfig { vcUIScale = 1.0 }, fieldUIScale, "1.0e39")
        , ("frame_limit 0",      [("frame_limit", "0")],      baseConfig { vcFrameLimit = Nothing }, fieldFrameLimit, "0")
        , ("frame_limit 29",     [("frame_limit", "29")],     baseConfig { vcFrameLimit = Nothing }, fieldFrameLimit, "29")
        , ("frame_limit 241",    [("frame_limit", "241")],    baseConfig { vcFrameLimit = Nothing }, fieldFrameLimit, "241")
        , ("frame_limit -1",     [("frame_limit", "-1")],     baseConfig { vcFrameLimit = Nothing }, fieldFrameLimit, "-1")
        , ("msaa 3",             [("msaa", "3")],             baseConfig { vcMSAA = 1 }, fieldMSAA, "3")
        , ("brightness 49",      [("brightness", "49")],      baseConfig { vcBrightness = 100 }, fieldBrightness, "49")
        , ("brightness 301",     [("brightness", "301")],     baseConfig { vcBrightness = 100 }, fieldBrightness, "301")
        , ("texture_filter cubic", [("texture_filter", "cubic")], baseConfig { vcTextureFilter = FilterNearest }, fieldTextureFilter, "cubic")
        , ("tooltip_dwell_ms 1001", [("tooltip_dwell_ms", "1001")], baseConfig { vcTooltipDwellMs = 400 }, fieldTooltipDwellMs, "1001")
        , ("tooltip_dwell_ms -1", [("tooltip_dwell_ms", "-1")], baseConfig { vcTooltipDwellMs = 400 }, fieldTooltipDwellMs, "-1")
        , ("tooltip_hint_delay_ms 1001", [("tooltip_hint_delay_ms", "1001")], baseConfig { vcTooltipHintDelayMs = 400 }, fieldTooltipHintDelayMs, "1001")
        ]
    structuralCases ∷ [(String, ByteString)]
    structuralCases =
        [ ("wrong type (width: abc)", docWith [("width", "abc")])
        , ("wrong type (msaa: 4.5)", docWith [("msaa", "4.5")])
        , ("wrong type (window_mode: 5)", docWith [("window_mode", "5")])
        , ("schema-incomplete (no resolution)", docWithout resolutionKeys)
        , ("syntax error", "video: [\n  resolution: {\n")
        ]

-- | 'saveVideoConfig' against a temporary path.
saveSpec ∷ Spec
saveSpec = describe "saveVideoConfig" $ do
    it "writes a valid config that the loader reads back identically, without fallback or warning" $ inTemp $ \dir → do
        (logger, drain) ← newDrainingLogger
        let path = dir </> "video.local.yaml"
        saveVideoConfig logger path baseConfig `shouldReturn` True
        loadVideoConfig logger path `shouldReturn` baseConfig
        -- Unlimited survives write→read too: null on disk, Nothing in memory.
        let unlimited = baseConfig { vcFrameLimit = Nothing }
        saveVideoConfig logger path unlimited `shouldReturn` True
        loadVideoConfig logger path `shouldReturn` unlimited
        (warningsOf ⊚ drain) `shouldReturn` []

    it "refuses an invalid config and leaves an existing destination byte-for-byte unchanged" $ inTemp $ \dir → do
        (logger, drain) ← newDrainingLogger
        let path = dir </> "video.local.yaml"
            sentinel = "video: {resolution: {width: 1, height: 1}}\n# untouched\n"
        BS.writeFile path sentinel
        saveVideoConfig logger path baseConfig { vcWidth = 0, vcBrightness = 999 }
            `shouldReturn` False
        BS.readFile path `shouldReturn` sentinel
        entries ← drain
        let warnings = warningsOf entries
        length warnings `shouldBe` 2
        T.concat warnings `shouldContainText` (fieldWidth <> " = 0 ")
        T.concat warnings `shouldContainText` (fieldBrightness <> " = 999 ")
        filter ("saved to" `T.isInfixOf`) (infosOf entries) `shouldBe` []

    it "refuses an invalid config and leaves an absent destination absent" $ inTemp $ \dir → do
        (logger, drain) ← newDrainingLogger
        let path = dir </> "video.local.yaml"
        saveVideoConfig logger path baseConfig { vcUIScale = 0 / 0 } `shouldReturn` False
        doesFileExist path `shouldReturn` False
        warnings ← warningsOf ⊚ drain
        length warnings `shouldBe` 1
        T.concat warnings `shouldContainText` (fieldUIScale <> " = NaN ")

    it "logs every rejected field rather than substituting a default for any of them" $ inTemp $ \dir → do
        (logger, drain) ← newDrainingLogger
        let path = dir </> "video.local.yaml"
        saveVideoConfig logger path baseConfig
            { vcHeight = -1, vcFrameLimit = Just 5, vcMSAA = 3
            , vcTooltipHintDelayMs = 1001 }
            `shouldReturn` False
        doesFileExist path `shouldReturn` False
        warnings ← warningsOf ⊚ drain
        length warnings `shouldBe` 4
        forM_ [ fieldHeight <> " = -1 ", fieldFrameLimit <> " = 5 "
              , fieldMSAA <> " = 3 ", fieldTooltipHintDelayMs <> " = 1001 " ] $
            \needle → T.concat warnings `shouldContainText` needle

-- | 'migrateLegacyConfig' decoding 'VideoConfigFile', with the neutrality
--   check, against temporary paths.
migrationSpec ∷ Spec
migrationSpec = describe "migration boundary" $ do
    it "does not migrate a legacy file missing resolution (schema-incomplete)" $ inTemp $ \dir → do
        (localExists, entries) ← migrate dir (docWithout resolutionKeys) (docWith [])
        localExists `shouldBe` False
        T.concat (warningsOf entries) `shouldContainText` "could not be migrated"

    it "does not migrate a structurally valid legacy file carrying an unknown token (unchanged by #2198)" $ inTemp $ \dir → do
        (localExists, entries) ← migrate dir (docWith [("window_mode", "sideways")]) (docWith [])
        localExists `shouldBe` False
        T.concat (warningsOf entries) `shouldContainText` "could not be migrated"
        (localFilter, entriesFilter) ← migrate dir (docWith [("texture_filter", "cubic")]) (docWith [])
        localFilter `shouldBe` False
        T.concat (warningsOf entriesFilter) `shouldContainText` "could not be migrated"

    it "still migrates a legacy file whose numeric leaf is out of domain; the loader then defaults that leaf" $ inTemp $ \dir → do
        let legacy = docWith [("width", "-5")]
        (localExists, entries) ← migrate dir legacy (docWith [])
        localExists `shouldBe` True
        T.concat (infosOf entries) `shouldContainText` "Migrated legacy config"
        BS.readFile (dir </> "video.local.yaml") `shouldReturn` legacy
        (cfg, warnings) ← loadLocal dir
        cfg `shouldBe` baseConfig { vcWidth = 800 }
        length warnings `shouldBe` 1
        T.concat warnings `shouldContainText` (fieldWidth <> " = -5 ")

    it "suppresses a legacy placeholder semantically equal to the default rather than promoting it" $ inTemp $ \dir → do
        -- Same values, different spelling: neutrality is judged on the
        -- decoded config, never on bytes.
        let legacy = docWith [("window_mode", "Borderless"), ("ui_scale", "2.00")]
        (localExists, entries) ← migrate dir legacy (docWith [])
        localExists `shouldBe` False
        doesFileExist (dir </> "video.legacy-neutral.local.yaml") `shouldReturn` True
        T.concat (infosOf entries) `shouldContainText` "carries no player state"
        warningsOf entries `shouldBe` []

    it "migrates a valid, genuinely different legacy file with the migration log" $ inTemp $ \dir → do
        let legacy = docWith [("ui_scale", "1.5"), ("msaa", "8")]
        (localExists, entries) ← migrate dir legacy (docWith [])
        localExists `shouldBe` True
        BS.readFile (dir </> "video.local.yaml") `shouldReturn` legacy
        T.concat (infosOf entries) `shouldContainText` "Migrated legacy config"
        (cfg, warnings) ← loadLocal dir
        cfg `shouldBe` baseConfig { vcUIScale = 1.5, vcMSAA = 8 }
        warnings `shouldBe` []
  where
    -- Run the migration with @legacy@ at the legacy path and @dflt@ as
    -- the versioned template; report whether the local file appeared.
    migrate ∷ FilePath → ByteString → ByteString → IO (Bool, [LogEntry])
    migrate dir legacy dflt = do
        let legacyPath  = dir </> "video.yaml"
            localPath   = dir </> "video.local.yaml"
            defaultPath = dir </> "video_default.yaml"
            recordPath  = dir </> "video.legacy-neutral.local.yaml"
        BS.writeFile legacyPath legacy
        BS.writeFile defaultPath dflt
        (logger, drain) ← newDrainingLogger
        migrateLegacyConfig (Proxy ∷ Proxy VideoConfigFile) logger
            (Just LegacyNeutralityCheck
               { lncDefaultPath = defaultPath, lncRecordPath = recordPath })
            legacyPath localPath
        localExists ← doesFileExist localPath
        entries ← drain
        pure (localExists, entries)
    loadLocal ∷ FilePath → IO (VideoConfig, [Text])
    loadLocal dir = do
        (logger, drain) ← newDrainingLogger
        cfg ← loadVideoConfig logger (dir </> "video.local.yaml")
        warnings ← warningsOf ⊚ drain
        pure (cfg, warnings)

-- | The Lua setters against the real registered API.
luaSpec ∷ SpecWith Fixture
luaSpec = do
    describe "per-field Lua setters" $ do
        forM_ rejectedCalls $ \call →
            it ("refuses " ⧺ T.unpack call ⧺ ": false, config and mirrors unchanged, nothing enqueued") $
                \fx → refused fx call

        it "accepts the UI-scale endpoints and Lua's numeric-string coercion, enqueuing nothing" $ \fx → do
            resetLua fx
            accepted fx "engine.setUIScale(0.5)"
            (vcUIScale ⊚ storedConfig fx) `shouldReturn` 0.5
            accepted fx "engine.setUIScale(4.0)"
            (vcUIScale ⊚ storedConfig fx) `shouldReturn` 4.0
            accepted fx "engine.setUIScale('2')"
            (vcUIScale ⊚ storedConfig fx) `shouldReturn` 2.0
            (map show ⊚ queued fx) `shouldReturn` []

        it "stores frame limit 0 as unlimited and accepts 30 and 240" $ \fx → do
            resetLua fx
            accepted fx "engine.setFrameLimit(0)"
            (vcFrameLimit ⊚ storedConfig fx) `shouldReturn` Nothing
            accepted fx "engine.setFrameLimit(30)"
            (vcFrameLimit ⊚ storedConfig fx) `shouldReturn` Just 30
            accepted fx "engine.setFrameLimit(240)"
            (vcFrameLimit ⊚ storedConfig fx) `shouldReturn` Just 240
            (map show ⊚ queued fx) `shouldReturn` []

        it "setResolution writes both dimensions and enqueues the resize, numeric strings included" $ \fx → do
            resetLua fx
            accepted fx "engine.setResolution(1280, 720)"
            accepted fx "engine.setResolution('1600', '900')"
            cfg ← storedConfig fx
            (vcWidth cfg, vcHeight cfg) `shouldBe` (1600, 900)
            (map show ⊚ queued fx) `shouldReturn`
                map show [LuaSetResolution 1280 720, LuaSetResolution 1600 900]

        it "setWindowMode matches case-insensitively, writes the config and enqueues the mode" $ \fx → do
            resetLua fx
            accepted fx "engine.setWindowMode('FullScreen')"
            (vcWindowMode ⊚ storedConfig fx) `shouldReturn` Fullscreen
            (map show ⊚ queued fx) `shouldReturn` [show (LuaSetWindowMode Fullscreen)]

        it "setVSync writes the config and enqueues the change" $ \fx → do
            resetLua fx
            accepted fx "engine.setVSync(true)"
            (vcVSync ⊚ storedConfig fx) `shouldReturn` True
            (map show ⊚ queued fx) `shouldReturn` [show (LuaSetVSync True)]

        it "setMSAA accepts every choice, writing and enqueuing each" $ \fx → do
            resetLua fx
            forM_ msaaChoices $ \m → do
                accepted fx ("engine.setMSAA(" <> tshow m <> ")")
                (vcMSAA ⊚ storedConfig fx) `shouldReturn` m
            (map show ⊚ queued fx) `shouldReturn` map (show ∘ LuaSetMSAA) msaaChoices

        it "setBrightness accepts both endpoints, writing and enqueuing each" $ \fx → do
            resetLua fx
            accepted fx "engine.setBrightness(50)"
            (vcBrightness ⊚ storedConfig fx) `shouldReturn` 50
            accepted fx "engine.setBrightness(300)"
            (vcBrightness ⊚ storedConfig fx) `shouldReturn` 300
            (map show ⊚ queued fx) `shouldReturn`
                map show [LuaSetBrightness 50, LuaSetBrightness 300]

        it "setPixelSnap writes the mirror and the config, enqueuing nothing" $ \fx → do
            resetLua fx
            accepted fx "engine.setPixelSnap(false)"
            (vcPixelSnap ⊚ storedConfig fx) `shouldReturn` False
            readIORef (pixelSnapRef (fxEnv fx)) `shouldReturn` False
            (map show ⊚ queued fx) `shouldReturn` []

        it "setTextureFilter matches case-insensitively, writes the mirror and the config, and enqueues the filter" $ \fx → do
            resetLua fx
            accepted fx "engine.setTextureFilter('NEAREST')"
            (vcTextureFilter ⊚ storedConfig fx) `shouldReturn` FilterNearest
            readIORef (textureFilterRef (fxEnv fx)) `shouldReturn` FilterNearest
            (map show ⊚ queued fx) `shouldReturn` [show (LuaSetTextureFilter FilterNearest)]

        it "tooltip setters accept 0 and 1000, writing the config and the live style" $ \fx → do
            resetLua fx
            accepted fx "engine.setTooltipDwellMs(0)"
            accepted fx "engine.setTooltipHintDelayMs(1000)"
            cfg ← storedConfig fx
            (vcTooltipDwellMs cfg, vcTooltipHintDelayMs cfg) `shouldBe` (0, 1000)
            (tooltipTimings ⊚ readIORef (uiManagerRef (fxEnv fx))) `shouldReturn` (0, 1000)
            accepted fx "engine.setTooltipDwellMs(1000)"
            accepted fx "engine.setTooltipHintDelayMs(0)"
            (tooltipTimings ⊚ readIORef (uiManagerRef (fxEnv fx))) `shouldReturn` (1000, 0)
            (map show ⊚ queued fx) `shouldReturn` []

        it "logs the verb, field and value of a refused call" $ \fx → do
            refused fx "engine.setBrightness(301)"
            -- 'refused' drained the log; re-drive and read the line.
            resetLua fx
            void $ evalOk (fxLua fx) "return engine.setBrightness(301)"
            warnings ← warningsOf ⊚ fxDrain fx
            length warnings `shouldBe` 1
            T.concat warnings `shouldContainText`
                ("engine.setBrightness rejected " <> fieldBrightness <> " = 301 ")

    describe "engine.setVideoConfig" $ do
        it "writes all ten fields atomically, case-insensitively, and enqueues nothing" $ \fx → do
            resetLua fx
            accepted fx validCombined
            storedConfig fx `shouldReturn` expectedCombined
            (map show ⊚ queued fx) `shouldReturn` []

        it "round-trips getVideoConfig's ten values, frame-limit 0 included" $ \fx → do
            resetLua fx
            writeIORef (videoConfigRef (fxEnv fx)) baseConfig { vcFrameLimit = Nothing }
            accepted fx $ luaLines
                [ "(function()"
                , "  local w, h, wm, s, vs, fl, m, b, ps, tf = engine.getVideoConfig();"
                , "  assert(fl == 0, 'unlimited reads back as 0');"
                , "  return engine.setVideoConfig(w, h, wm, s, vs, fl, m, b, ps, tf)"
                , "end)()" ]
            storedConfig fx `shouldReturn` baseConfig { vcFrameLimit = Nothing }
            (map show ⊚ queued fx) `shouldReturn` []

        forM_ combinedRejections $ \(label, call) →
            it ("refuses the whole call when one argument is invalid: " ⧺ label) $
                \fx → refused fx call

        it "logs every invalid argument of a refused call" $ \fx → do
            resetLua fx
            r ← evalOk (fxLua fx)
                "return engine.setVideoConfig(0, 720, 'windowed', 9.0, true, 0, 8, 120, false, 'cubic')"
            r `shouldBe` "false"
            storedConfig fx `shouldReturn` baseConfig
            warnings ← warningsOf ⊚ fxDrain fx
            length warnings `shouldBe` 3
            forM_ [fieldWidth <> " = 0 ", fieldUIScale <> " = 9.0 ", fieldTextureFilter <> " = cubic "] $
                \needle → T.concat warnings `shouldContainText` needle

    describe "Settings agreement" $ do
        it "scripts/settings/data.lua's bounds and option lists equal the engine domain" $ \fx → do
            r ← evalOk (fxLua fx) $ luaLines
                [ "local d = require('scripts.settings.data');"
                , "local msaa = {}; for _, o in ipairs(d.msaaOptions) do table.insert(msaa, tonumber(o.value)) end;"
                , "local modes = {}; for _, o in ipairs(d.windowModes) do table.insert(modes, o.value) end;"
                , "local filters = {}; for _, o in ipairs(d.textureFilterOptions) do table.insert(filters, o.value) end;"
                , "return { uiScaleMin = d.uiScaleMin, uiScaleMax = d.uiScaleMax,"
                , "  frameLimitMin = d.frameLimitMin, frameLimitMax = d.frameLimitMax,"
                , "  brightnessMin = d.brightnessMin, brightnessMax = d.brightnessMax,"
                , "  tooltipDwellMin = d.tooltipDwellMin, tooltipDwellMax = d.tooltipDwellMax,"
                , "  tooltipHintDelayMin = d.tooltipHintDelayMin, tooltipHintDelayMax = d.tooltipHintDelayMax,"
                , "  msaa = msaa, modes = modes, filters = filters }"
                ]
            sb ← decodeOr r ∷ IO SettingsBounds
            (sbUiScaleMin sb, sbUiScaleMax sb)
                `shouldBe` (realToFrac uiScaleMin, realToFrac uiScaleMax)
            (sbFrameLimitMin sb, sbFrameLimitMax sb) `shouldBe` (frameLimitMin, frameLimitMax)
            (sbBrightnessMin sb, sbBrightnessMax sb) `shouldBe` (brightnessMin, brightnessMax)
            (sbTooltipDwellMin sb, sbTooltipDwellMax sb) `shouldBe` (tooltipMsMin, tooltipMsMax)
            (sbTooltipHintDelayMin sb, sbTooltipHintDelayMax sb) `shouldBe` (tooltipMsMin, tooltipMsMax)
            sbMsaa sb `shouldBe` msaaChoices
            map windowModeFromText (sbModes sb)
                `shouldBe` [Just Fullscreen, Just BorderlessWindowed, Just Windowed]
            map textureFilterFromText (sbFilters sb)
                `shouldBe` [Just FilterNearest, Just FilterLinear]

        it "loadDefaults clamps the HiDPI-scaled UI scale into the domain before pushing it to the engine" $ \fx → do
            resetLua fx
            -- A 4K default with ui_scale 2.0 multiplies to 5.0; the
            -- setter would refuse that and silently keep the old scale.
            r ← evalOk (fxLua fx) $ luaLines
                [ "local d = require('scripts.settings.data');"
                , "local orig = engine.loadDefaultConfig;"
                , "engine.loadDefaultConfig = function()"
                , "  return 3840, 2160, 'windowed', 2.0, true, 60, 1, 100, false, 'nearest'"
                , "end;"
                , "local ok, err = pcall(d.loadDefaults);"
                , "engine.loadDefaultConfig = orig;"
                , "assert(ok, err);"
                , "local _, _, _, scale = engine.getVideoConfig();"
                , "return { current = d.current.uiScale, stored = scale }"
                ]
            sp ← decodeOr r ∷ IO ScalePair
            (spCurrent sp, spStored sp) `shouldBe` (realToFrac uiScaleMax, realToFrac uiScaleMax)
