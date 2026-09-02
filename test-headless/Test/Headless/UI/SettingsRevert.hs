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
