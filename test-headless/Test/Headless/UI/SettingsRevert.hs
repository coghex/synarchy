-- | #108's Settings Revert contract, executable again (#1607).
--
--   Settings live-previews tooltip dwell and hint delay: editing either
--   one pushes it straight into the engine, so @engine.getTooltipDwellMs()@
--   already reports the EDITED value by the time Revert runs. The
--   pre-#164 @data.revert@ read those getters and wrote the edited value
--   back to itself — a revert that reverted nothing. PR #164's fix
--   snapshots the last SAVED values (mirroring @savedBrightness@) and
--   restores those instead.
--
--   That fix shipped with @tools\/test_settings_revert.lua@ as its
--   regression oracle: a standalone harness with a hand-written engine
--   double. Settings later gained the #913 autosave family, whose three
--   engine calls (@getSaveConfig@, @setSaveConfig@,
--   @getDefaultSaveConfig@) the double never grew, so
--   @lua tools\/test_settings_revert.lua@ began aborting inside
--   @data.reload@ before a single assertion ran. Nothing referenced the
--   tool, so it sat red indefinitely and #108's contract went unproved.
--
--   This group is the durable replacement, and the harness is deleted
--   (#1607) rather than re-stubbed — a second hand-written engine double
--   would drift the same way. It carries every scenario that harness
--   covered — dwell, hint delay, save-then-revert ordering, and defined
--   brightness — against the REAL registered engine API and the real
--   @scripts\/settings\/data.lua@, in a suite that runs on every CI run,
--   so the same silent drift cannot happen again: a new engine call
--   reaching Settings is one the fixture already provides.
--
--   #2194 extends the group from those three snapshot-backed fields to
--   all eleven the Settings screen manages. Apply pushes each pending
--   value into the live @rvVideoConfigRef@ that @engine.getVideoConfig@
--   reads, and only @engine.saveVideoConfig@ writes that ref to
--   @config\/video.local.yaml@ — so the eight fields Back used to read
--   back from that getter after an Apply-without-Save "reverted" to
--   their applied values. 'perFieldCases' below is one example per
--   field, each one apply-only-then-Back from a @reload@-established
--   baseline AND Apply–Save–Apply–Back from a @save@-established one,
--   with that field's registered engine setter wrapped in a forwarding
--   counter reset immediately before Back. Dropping any single field
--   from @data.revert@ fails that field's example and no other.
--
--   Reachable on its own as
--   @--match "settings revert restores saved values"@.
--
--   Every example wraps
--   'Test.Headless.Harness.Isolation.withIsolatedResourceRoot' AROUND
--   'Test.Headless.Harness.withHeadlessEngine', per #1357: these are
--   production write paths — @data.save@ reaches
--   @engine.saveVideoConfig@ (@config\/video.local.yaml@) and
--   @engine.setSaveConfig@ (@config\/save.local.yaml@), both
--   cwd-relative — and engine initialization is itself a writer, so
--   isolation has to be established before the engine boots. Each
--   example asserts 'isInsideIsolatedResourceRoot' while its engine is
--   running, because @git status --porcelain config/@ cannot see a
--   modification to a gitignored @config\/*.local.yaml@ file; the first
--   example additionally compares the checkout's own @config\/@ tree
--   byte for byte across the run.
module Test.Headless.UI.SettingsRevert (spec) where

import UPrelude
import Test.Hspec
import qualified Data.ByteString as BS
import Data.IORef (newIORef)
import Data.List (sort)
import qualified Data.Text as T
import Engine.Core.State (EngineEnv(..))
import Engine.Core.Thread (ThreadControl(..))
import Engine.Scripting.Lua.API (registerLuaAPI)
import Engine.Scripting.Lua.Thread (createLuaBackendState)
import Engine.Scripting.Lua.Thread.Console (executeDebugLua)
import Engine.Scripting.Lua.Types (LuaBackendState(..))
import System.Directory (doesDirectoryExist, getCurrentDirectory, listDirectory)
import System.FilePath ((</>))
import Test.Headless.Harness (withHeadlessEngine)
import Test.Headless.Harness.Isolation
  (isInsideIsolatedResourceRoot, withIsolatedResourceRoot)

-- | A bare Lua backend with the real engine API registered — the same
--   fixture 'Test.Headless.UI.SettingsDefaultsKeybinds' uses, so
--   @scripts/settings/data.lua@ runs against the production
--   @engine.*@ functions rather than a hand-written double.
newBareLuaBackend ∷ EngineEnv → IO LuaBackendState
newBareLuaBackend env = do
    ls ← createLuaBackendState (luaToEngineQueue env) (luaQueue env)
                               (assetPoolRef env) (nextObjectIdRef env)
                               (inputStateRef env) (loggerRef env)
    stateRef ← newIORef ThreadRunning
    registerLuaAPI (lbsLuaState ls) env ls stateRef
    pure ls

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

-- | Every regular file under @dir@, as (relative path, bytes), ordered.
--   A missing directory is an empty snapshot, so "the run created
--   @config/@ where there was none" shows up as a difference rather
--   than an exception.
snapshotTree ∷ FilePath → IO [(FilePath, BS.ByteString)]
snapshotTree dir = do
    present ← doesDirectoryExist dir
    if not present then pure [] else sort ⊚ go ""
  where
    go rel = do
        names ← listDirectory (dir </> rel)
        concat ⊚ forM names (\name → do
            let relPath = if null rel then name else rel </> name
            isDir ← doesDirectoryExist (dir </> relPath)
            if isDir
              then go relPath
              else do
                  bytes ← BS.readFile (dir </> relPath)
                  pure [(relPath, bytes)])

-- | Open the settings screen the way it opens in game: @reload@ reads
--   the live config back and takes the revert snapshots, @resetPending@
--   seeds the pending table from it (an @apply@ against an empty
--   @data.pending@ would compare every field against @nil@).
openSettings ∷ Text
openSettings = luaLines
    [ "local d = require('scripts.settings.data');"
    , "d.reload();"
    , "d.resetPending();"
    ]

spec ∷ Spec
spec = describe "settings revert restores saved values (#108, #1607)" $ do

    -- Requirement 1: the exact #108 contract. Saved and edited values
    -- are distinct and well inside the engine setters' 0–1000 domain
    -- (Engine.Graphics.Config.Domain, #2198), and the edited values are
    -- OBSERVED live before the revert — without that, a revert that did
    -- nothing at all would pass.
    it "restores the last SAVED tooltip dwell and hint delay after a \
       \live edit, not the edited values" $ do
        srcRoot ← getCurrentDirectory
        let srcConfig = srcRoot </> "config"
        before ← snapshotTree srcConfig
        withIsolatedResourceRoot $ withHeadlessEngine $ \env → do
            isInsideIsolatedResourceRoot `shouldReturn` True
            ls ← newBareLuaBackend env
            result ← evalOk ls $ openSettings <> luaLines
                -- Save a pair of values that are not the defaults, so
                -- the revert target is a value this example established.
                [ "d.apply({tooltipDwellMs = 250, tooltipHintDelayMs = 700});"
                , "d.save({});"
                , "local sd, sh = engine.getTooltipDwellMs(),"
                , "               engine.getTooltipHintDelayMs();"
                -- Live-preview a DIFFERENT pair, exactly as the sliders do.
                , "d.apply({tooltipDwellMs = 880, tooltipHintDelayMs = 120});"
                , "local ed, eh = engine.getTooltipDwellMs(),"
                , "               engine.getTooltipHintDelayMs();"
                , "d.revert();"
                , "return sd, sh, ed, eh,"
                , "  engine.getTooltipDwellMs(), engine.getTooltipHintDelayMs(),"
                , "  d.current.tooltipDwellMs, d.current.tooltipHintDelayMs"
                ]
            -- saved | edited (live, pre-revert) | reverted | data.current
            T.splitOn "\t" result `shouldBe`
                ["250", "700", "880", "120", "250", "700", "250", "700"]
        after ← snapshotTree srcConfig
        after `shouldBe` before

    -- Requirement 3: Save moves the revert target. Three distinct values
    -- separate the two ways this can break — reverting to the value
    -- saved FIRST (data.save never refreshed the snapshots) and
    -- reverting to the unsaved edit (revert read the live getters, the
    -- pre-#164 bug).
    it "a value that has been saved becomes the new revert target" $
        withIsolatedResourceRoot $ withHeadlessEngine $ \env → do
            isInsideIsolatedResourceRoot `shouldReturn` True
            ls ← newBareLuaBackend env
            result ← evalOk ls $ openSettings <> luaLines
                [ "d.apply({tooltipDwellMs = 210}); d.save({});"
                , "local firstSaved = engine.getTooltipDwellMs();"
                , "d.apply({tooltipDwellMs = 530}); d.save({});"
                , "d.apply({tooltipDwellMs = 910});"
                , "local edited = engine.getTooltipDwellMs();"
                , "d.revert();"
                , "return firstSaved, edited,"
                , "  engine.getTooltipDwellMs(), d.current.tooltipDwellMs"
                ]
            -- first saved | unsaved edit | reverted to the SECOND save
            T.splitOn "\t" result `shouldBe` ["210", "910", "530", "530"]

    -- Requirement 2, as the retired harness actually spelled it: revert
    -- leaves data.current.brightness DEFINED and equal to the saved
    -- brightness. Revert reads brightness from data.savedBrightness
    -- rather than from getVideoConfig's eighth return, so a snapshot
    -- that stopped being taken would surface here as nil.
    it "leaves data.current.brightness defined and equal to the saved \
       \brightness" $
        withIsolatedResourceRoot $ withHeadlessEngine $ \env → do
            isInsideIsolatedResourceRoot `shouldReturn` True
            ls ← newBareLuaBackend env
            result ← evalOk ls $ openSettings <> luaLines
                [ "local _, _, _, _, _, _, _, savedBrightness ="
                , "  engine.getVideoConfig();"
                , "d.revert();"
                , "return savedBrightness, d.current.brightness"
                ]
            -- The console renders a Lua nil as "null", so "defined" is a
            -- real assertion here rather than a shape the formatting hides.
            case T.splitOn "\t" result of
                [savedText, revertedText] → do
                    savedText `shouldNotBe` "null"
                    revertedText `shouldBe` savedText
                other → expectationFailure
                    ("expected two returned values, got " ⧺ show other)

    -- Requirement 1, 2 and 6 (#2194): the same contract for every
    -- managed field, including the eight that used to read their
    -- "saved" value back out of the already-mutated live config. Each
    -- example is independent — its own engine, its own isolated
    -- resource root — so dropping one field from data.revert fails that
    -- field's example alone.
    forM_ perFieldCases $ \fc →
        it ("restores the persisted " ⧺ fcLabel fc
            ⧺ " after an apply-only change, reaching its engine setter") $
            withIsolatedResourceRoot $ withHeadlessEngine $ \env → do
                isInsideIsolatedResourceRoot `shouldReturn` True
                ls ← newBareLuaBackend env
                result ← evalOk ls (fieldScenario fc)
                -- baseline | applied (engine, data.current) | setter
                -- calls during Back | restored (engine, data.current),
                -- then the same five for the post-Save baseline.
                T.splitOn "\t" result `shouldBe` fieldExpectation fc

    -- Requirement 5 (#748, #2027): a Back that moves the LIVE UI scale
    -- still fans out to the three consumers that have no other way to
    -- learn about it, exactly once each. Driven through the real
    -- settingsMenu.onBack rather than data.revert, because onBack is
    -- what compares the scale either side of the revert and decides.
    -- The existing ResponsiveMenus example stubs data.revert outright
    -- and only asserts the shell count is non-zero, so this is the one
    -- place the real revert and the real fan-out meet.
    it "UI scale: settingsMenu.onBack restores the saved scale and fans \
       \it out to responsive, shell and gameplay exactly once each" $
        withIsolatedResourceRoot $ withHeadlessEngine $ \env → do
            isInsideIsolatedResourceRoot `shouldReturn` True
            ls ← newBareLuaBackend env
            result ← evalOk ls $ luaLines
                [ "local m = require('scripts.settings_menu');"
                , "m.init(1, 2, 3, 1280, 720);"
                , "local d = require('scripts.settings.data');"
                , "local responsive = require('scripts.ui.responsive');"
                , "local shell = require('scripts.shell');"
                -- Required only AFTER settings_menu is in
                -- package.loaded: settings_menu itself defers this
                -- require to break the ui_manager -> ui_manager_boot ->
                -- settings_menu cycle.
                , "local uiManager = require('scripts.ui_manager');"
                , "d.resetPending();"
                , "d.apply({uiScale = 1.25}); d.save({});"
                , "local saved = (select(4, engine.getVideoConfig()));"
                -- Apply WITHOUT Save: the exact state Back has to undo.
                , "d.apply({uiScale = 2.5});"
                , "local applied = (select(4, engine.getVideoConfig()));"
                , "local rc, sc, gc = 0, 0, 0;"
                , "local realR, realS, realG = responsive.notifyResize,"
                , "  shell.onFramebufferResize,"
                , "  uiManager.notifyGameplayRescale;"
                , "responsive.notifyResize ="
                , "  function(...) rc = rc + 1; return realR(...) end;"
                , "shell.onFramebufferResize ="
                , "  function(...) sc = sc + 1; return realS(...) end;"
                , "uiManager.notifyGameplayRescale ="
                , "  function(...) gc = gc + 1; return realG(...) end;"
                , "m.onBack();"
                , "responsive.notifyResize = realR;"
                , "shell.onFramebufferResize = realS;"
                , "uiManager.notifyGameplayRescale = realG;"
                , "return saved, applied, rc, sc, gc,"
                , "  (select(4, engine.getVideoConfig())), d.current.uiScale"
                ]
            -- saved | applied (live, pre-Back) | the three fan-out
            -- counts | restored (engine, data.current)
            T.splitOn "\t" result `shouldBe`
                ["1.25", "2.5", "1", "1", "1", "1.25", "1.25"]

    -- Requirement 3 (#2194): Defaults is the third and last point that
    -- establishes a baseline, so a Back taken after it returns to the
    -- factory defaults rather than to whatever was saved before them.
    -- This is the behaviour the three retired per-field snapshots
    -- already had; the generalized snapshot has to keep it. The default
    -- value itself comes from config/video_default.yaml and so is not
    -- spelled here — what is asserted is that it differs from the saved
    -- value, and that Back lands on it.
    it "Defaults establishes the new baseline, so a later Back returns \
       \to the defaults rather than the pre-Defaults saved value" $
        withIsolatedResourceRoot $ withHeadlessEngine $ \env → do
            isInsideIsolatedResourceRoot `shouldReturn` True
            ls ← newBareLuaBackend env
            result ← evalOk ls $ openSettings <> luaLines
                [ "d.apply({tooltipDwellMs = 137}); d.save({});"
                , "local saved = engine.getTooltipDwellMs();"
                , "d.loadDefaults();"
                , "local dflt = engine.getTooltipDwellMs();"
                , "d.apply({tooltipDwellMs = 911});"
                , "local applied = engine.getTooltipDwellMs();"
                , "d.revert();"
                , "return saved, dflt, applied,"
                , "  engine.getTooltipDwellMs(), d.current.tooltipDwellMs"
                ]
            case T.splitOn "\t" result of
                [saved, dflt, applied, reverted, current] → do
                    saved `shouldBe` "137"
                    applied `shouldBe` "911"
                    -- Without this the example would pass on a baseline
                    -- Defaults never refreshed, since 137 would be the
                    -- default too.
                    dflt `shouldNotBe` saved
                    reverted `shouldBe` dflt
                    current `shouldBe` dflt
                other → expectationFailure
                    ("expected five returned values, got " ⧺ show other)

-----------------------------------------------------------------------
-- #2194: one example per managed video field.
-----------------------------------------------------------------------

-- | One managed video field's Back coverage.
data FieldCase = FieldCase
  { fcLabel   ∷ String
    -- ^ Spelled into the example name.
  , fcSetter  ∷ Text
    -- ^ The key on the @engine@ table that @data.revert@ must reach for
    --   this field. Wrapped in a forwarding counter around Back, so the
    --   example proves the REAL registered setter ran rather than
    --   asserting on a double.
  , fcEngine  ∷ Text
    -- ^ Lua expression reading the field back out of the ENGINE.
    --   @getVideoConfig@'s multi-return is truncated with an extra pair
    --   of parentheses; without them the tail would splice itself into
    --   the scenario's own return list.
  , fcCurrent ∷ Text
    -- ^ Lua expression reading the same field from @data.current@.
  , fcSeed    ∷ Text
    -- ^ A direct @engine.set…@ call carrying value 1, made behind
    --   @data.lua@'s back: it moves the ENGINE without touching
    --   @data.current@ or the baseline, which is what lets the scenario
    --   put @data.reload@'s own capture under test rather than letting
    --   the preceding Save's capture stand in for it.
  , fcApply   ∷ Quad
    -- ^ Four complete \"stage the pending\/widget value, then Apply\"
    --   statements. 1 and 2 drive the reload-established baseline
    --   sequence, 3 and 4 the save-established one; within each pair
    --   the two must differ, or an applied value would be
    --   indistinguishable from the baseline it is supposed to replace.
  , fcShown   ∷ Quad
    -- ^ How the debug console renders those same four values — numbers
    --   bare, strings quoted, booleans as @true@\/@false@.
  }

-- | The four values a field's example walks through, as a tuple rather
--   than a list so \"exactly four\" is the type's job and neither
--   'fieldScenario' nor 'fieldExpectation' needs a partial match.
type Quad = (Text, Text, Text, Text)

-- | Both halves of requirement 2 for one field, in one console line.
--
--   The first half establishes the baseline through @data.reload@ (how
--   opening Settings establishes it), applies a different value WITHOUT
--   saving, observes that the change really went live, and takes Back.
--   The second half establishes the baseline through @data.save@ alone
--   with no intervening reload, so a field omitted from @data.save@'s
--   refresh cannot pass by riding on the reload capture.
--
--   The forwarding counter is installed and zeroed immediately before
--   each Back and removed immediately after, so it counts that Back's
--   calls only — not the Applies and Saves that set the scene.
fieldScenario ∷ FieldCase → Text
fieldScenario fc = luaLines
    [ "local d = require('scripts.settings.data');"
    , "local function res()"
    , "  local w, h = engine.getVideoConfig(); return w .. 'x' .. h end;"
    , "local calls, real = 0, nil;"
    , "local function countBack()"
    , "  calls = 0; real = engine." <> fcSetter fc <> ";"
    , "  engine." <> fcSetter fc <> " ="
    , "    function(...) calls = calls + 1; return real(...) end;"
    , "  d.revert();"
    , "  engine." <> fcSetter fc <> " = real;"
    , "  return calls end;"
    , "d.reload(); d.resetPending();"
    -- Put a DIFFERENT value on disk and in the baseline first, then
    -- move the engine to value 1 behind data.lua's back. Now only
    -- data.reload's own capture can make value 1 the baseline — with
    -- the Apply-then-Save baseline still reading value 3, a reload that
    -- stopped capturing shows up as a Back to the wrong value rather
    -- than passing on the Save capture's coat-tails.
    , applyStep 2, "d.save({});"
    , fcSeed fc
    , "d.reload(); d.resetPending();"
    , "local baseA = " <> fcEngine fc <> ";"
    , applyStep 1
    , "local appliedA, appliedCurA ="
    , "  " <> fcEngine fc <> ", " <> fcCurrent fc <> ";"
    , "local callsA = countBack();"
    , "local revA, revCurA = " <> fcEngine fc <> ", " <> fcCurrent fc <> ";"
    , applyStep 2, "d.save({});"
    , "local baseB = " <> fcEngine fc <> ";"
    , applyStep 3
    , "local appliedB = " <> fcEngine fc <> ";"
    , "local callsB = countBack();"
    , "return baseA, appliedA, appliedCurA, callsA, revA, revCurA,"
    , "  baseB, appliedB, callsB,"
    , "  " <> fcEngine fc <> ", " <> fcCurrent fc
    ]
  where
    (a1, a2, a3, a4) = fcApply fc
    applyStep ∷ Int → Text
    applyStep 0 = a1
    applyStep 1 = a2
    applyStep 2 = a3
    applyStep _ = a4

-- | What 'fieldScenario' must return: the baseline is observed, the
--   applied value is observed live in both the engine and
--   @data.current@, Back reaches the field's real setter exactly once,
--   and both readings land back on the baseline.
fieldExpectation ∷ FieldCase → [Text]
fieldExpectation fc =
    [ v1, v2, v2, "1", v1, v1
    , v3, v4, "1", v3, v3 ]
  where (v1, v2, v3, v4) = fcShown fc

-- | Every field the Settings screen manages. Values are chosen inside
--   each field's validated range and distinct within each half, and
--   resolution is compared as @WxH@ because its two numbers share one
--   setter.
perFieldCases ∷ [FieldCase]
perFieldCases =
  [ FieldCase
      { fcLabel   = "resolution"
      , fcSetter  = "setResolution"
      , fcEngine  = "res()"
      , fcCurrent = "(d.current.width .. 'x' .. d.current.height)"
      , fcSeed    = "engine.setResolution(1280, 720);"
      , fcApply   =
          ( "d.pending.width = 1280; d.pending.height = 720; d.apply({});"
          , "d.pending.width = 1600; d.pending.height = 900; d.apply({});"
          , "d.pending.width = 1920; d.pending.height = 1080; d.apply({});"
          , "d.pending.width = 2560; d.pending.height = 1440; d.apply({});"
          )
      , fcShown   =
          ("\"1280x720\"", "\"1600x900\"", "\"1920x1080\"", "\"2560x1440\"")
      }
  , FieldCase
      { fcLabel   = "window mode"
      , fcSetter  = "setWindowMode"
      , fcEngine  = "(select(3, engine.getVideoConfig()))"
      , fcCurrent = "d.current.windowMode"
      , fcSeed    = "engine.setWindowMode('windowed');"
      , fcApply   =
          ( "d.pending.windowMode = 'windowed'; d.apply({});"
          , "d.pending.windowMode = 'borderless'; d.apply({});"
          , "d.pending.windowMode = 'fullscreen'; d.apply({});"
          , "d.pending.windowMode = 'windowed'; d.apply({});"
          )
      , fcShown   =
          ("\"windowed\"", "\"borderless\"", "\"fullscreen\"", "\"windowed\"")
      }
  , FieldCase
      { fcLabel   = "UI scale"
      , fcSetter  = "setUIScale"
      , fcEngine  = "(select(4, engine.getVideoConfig()))"
      , fcCurrent = "d.current.uiScale"
      , fcSeed    = "engine.setUIScale(1.25);"
      -- Powers of two over a quarter: exact in the Float the video
      -- config stores, so the round trip through it is not a
      -- formatting coin flip.
      , fcApply   =
          ( "d.apply({uiScale = 1.25});"
          , "d.apply({uiScale = 2.5});"
          , "d.apply({uiScale = 0.75});"
          , "d.apply({uiScale = 3.5});"
          )
      , fcShown   = ("1.25", "2.5", "0.75", "3.5")
      }
  , FieldCase
      { fcLabel   = "VSync"
      , fcSetter  = "setVSync"
      , fcEngine  = "(select(5, engine.getVideoConfig()))"
      , fcCurrent = "d.current.vsync"
      , fcSeed    = "engine.setVSync(true);"
      , fcApply   =
          ( "d.pending.vsync = true; d.apply({});"
          , "d.pending.vsync = false; d.apply({});"
          , "d.pending.vsync = false; d.apply({});"
          , "d.pending.vsync = true; d.apply({});"
          )
      , fcShown   = ("true", "false", "false", "true")
      }
  , FieldCase
      { fcLabel   = "frame limit"
      , fcSetter  = "setFrameLimit"
      , fcEngine  = "(select(6, engine.getVideoConfig()))"
      , fcCurrent = "d.current.frameLimit"
      , fcSeed    = "engine.setFrameLimit(45);"
      , fcApply   =
          ( "d.apply({frameLimit = 45});"
          , "d.apply({frameLimit = 90});"
          , "d.apply({frameLimit = 120});"
          , "d.apply({frameLimit = 144});"
          )
      , fcShown   = ("45", "90", "120", "144")
      }
  , FieldCase
      { fcLabel   = "MSAA"
      , fcSetter  = "setMSAA"
      , fcEngine  = "(select(7, engine.getVideoConfig()))"
      , fcCurrent = "d.current.msaa"
      , fcSeed    = "engine.setMSAA(2);"
      , fcApply   =
          ( "d.pending.msaa = 2; d.apply({});"
          , "d.pending.msaa = 4; d.apply({});"
          , "d.pending.msaa = 8; d.apply({});"
          , "d.pending.msaa = 1; d.apply({});"
          )
      , fcShown   = ("2", "4", "8", "1")
      }
  , FieldCase
      { fcLabel   = "brightness"
      , fcSetter  = "setBrightness"
      , fcEngine  = "(select(8, engine.getVideoConfig()))"
      , fcCurrent = "d.current.brightness"
      , fcSeed    = "engine.setBrightness(120);"
      , fcApply   =
          ( "d.pending.brightness = 120; d.apply({});"
          , "d.pending.brightness = 180; d.apply({});"
          , "d.pending.brightness = 90; d.apply({});"
          , "d.pending.brightness = 250; d.apply({});"
          )
      , fcShown   = ("120", "180", "90", "250")
      }
  , FieldCase
      { fcLabel   = "pixel snap"
      , fcSetter  = "setPixelSnap"
      , fcEngine  = "(select(9, engine.getVideoConfig()))"
      , fcCurrent = "d.current.pixelSnap"
      , fcSeed    = "engine.setPixelSnap(true);"
      , fcApply   =
          ( "d.pending.pixelSnap = true; d.apply({});"
          , "d.pending.pixelSnap = false; d.apply({});"
          , "d.pending.pixelSnap = false; d.apply({});"
          , "d.pending.pixelSnap = true; d.apply({});"
          )
      , fcShown   = ("true", "false", "false", "true")
      }
  , FieldCase
      { fcLabel   = "texture filter"
      , fcSetter  = "setTextureFilter"
      , fcEngine  = "(select(10, engine.getVideoConfig()))"
      , fcCurrent = "d.current.textureFilter"
      , fcSeed    = "engine.setTextureFilter('linear');"
      , fcApply   =
          ( "d.pending.textureFilter = 'linear'; d.apply({});"
          , "d.pending.textureFilter = 'nearest'; d.apply({});"
          , "d.pending.textureFilter = 'nearest'; d.apply({});"
          , "d.pending.textureFilter = 'linear'; d.apply({});"
          )
      , fcShown   =
          ("\"linear\"", "\"nearest\"", "\"nearest\"", "\"linear\"")
      }
  , FieldCase
      { fcLabel   = "tooltip dwell"
      , fcSetter  = "setTooltipDwellMs"
      , fcEngine  = "engine.getTooltipDwellMs()"
      , fcCurrent = "d.current.tooltipDwellMs"
      , fcSeed    = "engine.setTooltipDwellMs(250);"
      , fcApply   =
          ( "d.apply({tooltipDwellMs = 250});"
          , "d.apply({tooltipDwellMs = 880});"
          , "d.apply({tooltipDwellMs = 130});"
          , "d.apply({tooltipDwellMs = 640});"
          )
      , fcShown   = ("250", "880", "130", "640")
      }
  , FieldCase
      { fcLabel   = "tooltip hint delay"
      , fcSetter  = "setTooltipHintDelayMs"
      , fcEngine  = "engine.getTooltipHintDelayMs()"
      , fcCurrent = "d.current.tooltipHintDelayMs"
      , fcSeed    = "engine.setTooltipHintDelayMs(300);"
      , fcApply   =
          ( "d.apply({tooltipHintDelayMs = 300});"
          , "d.apply({tooltipHintDelayMs = 700});"
          , "d.apply({tooltipHintDelayMs = 150});"
          , "d.apply({tooltipHintDelayMs = 920});"
          )
      , fcShown   = ("300", "700", "150", "920")
      }
  ]
