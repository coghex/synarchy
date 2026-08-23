-- | #1592 gate: @popup.dismissAll()@ clears the PENDING QUEUE whether or
--   not a card is currently on screen.
--
--   The queue and the active cards are two containers with two
--   lifetimes, and for the whole window before the first
--   @ensureGameplayUI@ only the first of them exists:
--   @scripts\/popup.lua@'s @onShowPopup@ accepts and queues entries from
--   the moment the script loads, while @drainQueue@ refuses to run until
--   @popup.bootstrap@. An ordinary session sits in exactly that state on
--   the main menu, and #742's deliberately ungated Escape cascade
--   reaches @dismissAll@ from there — so "no active card" is a REAL
--   teardown state, not an impossible one, and it used to return before
--   the queue was cleared.
--
--   What that costs is not a cosmetic leak: the surviving entry is
--   presented later, out of its moment, by whichever gameplay bootstrap
--   happens to drain the queue next.
--
--   Every case runs on a bare Lua backend in the style of
--   'Test.Headless.Event.PopupCoordPage' — @scripts\/popup.lua@ itself
--   is the unit under test, and it is the real file, never a stub. Each
--   example gets its OWN engine and its OWN Lua VM for the same reason
--   that suite does: the pre-bootstrap state is a once-per-process
--   condition (@popup.bootstrapped@ is set by @bootstrap@ and cleared
--   only by @shutdown@), so a shared module table from an earlier case
--   would quietly destroy the very state under test.
--
--   Three deliberate fixture choices:
--
--   * The BOOTSTRAPPED control is not optional. A repair that
--     special-cased the pre-bootstrap path only would satisfy the
--     queue-only cases and still break card dismissal, so the
--     post-bootstrap behaviour (bounded active count, queued overflow,
--     coalescing, the return value) is asserted in the same suite.
--   * The Escape case drives the REAL @scripts\/init_keys.lua@ cascade
--     with a real, visible log panel sitting below the popup handler.
--     Asserting the numeric return of @dismissAll@ alone would not show
--     what requirement 2 is actually about: that the same press
--     continues past a queue-only teardown to the handler underneath.
--   * The event-log case emits through the ENGINE
--     (@Engine.PlayerEvent.Emit@) and delivers the resulting
--     'LuaShowPopup' through the real dispatch, because the ring and the
--     popup queue are written by two separate statements of one emit.
--     Clearing the transient queue must leave the persistent ring
--     untouched, and only a fixture that populated both can show it.
module Test.Headless.UI.PopupQueueTeardown (spec) where

import UPrelude
import Test.Hspec
import Data.Aeson (FromJSON(..), Value(..), decode, withObject, (.:))
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.IORef (newIORef)
import qualified Engine.Core.Queue as Q
import Engine.Core.State (EngineEnv(..))
import Engine.Core.Thread (ThreadControl(..))
import Engine.PlayerEvent.Emit (emitEvent)
import Engine.Scripting.Lua.API (registerLuaAPI)
import Engine.Scripting.Lua.Thread (createLuaBackendState)
import Engine.Scripting.Lua.Thread.Console (executeDebugLua)
import Engine.Scripting.Lua.Thread.Dispatch (processLuaMsg)
import Engine.Scripting.Lua.Types (LuaBackendState(..), LuaMsg(..))
import Test.Headless.Harness (withHeadlessEngine)

-----------------------------------------------------------
-- Fixture categories
-----------------------------------------------------------

-- | A popup-enabled category with NO @coalesce_window@ in
--   @data\/notification_categories.yaml@ — every entry becomes its own
--   card. Used wherever a case needs a predictable one-entry-one-card
--   count.
plainCategory ∷ Text
plainCategory = "save_load"

-- | A popup-enabled category WITH a @coalesce_window@ (1 s). Present in
--   the policy cases so the pre-bootstrap lifecycle rule is shown to be
--   category-independent: coalescing changes how queued entries are
--   PRESENTED on drain, never whether a teardown discarded them.
coalescingCategory ∷ Text
coalescingCategory = "survival_critical"

-----------------------------------------------------------
-- Lua fixtures
-----------------------------------------------------------

luaLines ∷ [Text] → Text
luaLines = T.intercalate " "

-- | Load the REAL @scripts\/popup.lua@ and leave it UNBOOTSTRAPPED —
--   the pre-gameplay state every queue-only case needs.
--
--   @scripts\/hud.lua@ is the one module @renderPopup@ reaches into, and
--   booting the real HUD needs a GPU font atlas, so it is stubbed with
--   the single field that is read (mirroring
--   'Test.Headless.Event.PopupCoordPage'). Nothing else is stubbed:
--   @popup.queue@, @popup.active@ and @dismissAll@ are all the genuine
--   article.
preBootstrapFixture ∷ Text
preBootstrapFixture = luaLines
    [ "package.loaded['scripts.hud'] ="
    , "  { getToolbarRects = function() return {} end };"
    -- Registered through the SAME engine.loadScript route production
    -- uses, which is what makes the module a broadcastToModules target
    -- and therefore what lets the event-log case deliver a real
    -- LuaShowPopup into it.
    , "assert(engine.loadScript('scripts/popup.lua', 3600.0) ~= nil,"
    , "  'production loadScript path failed');"
    , "local p = require('scripts.popup');"
    , "assert(not p.isBootstrapped(),"
    , "  'fixture precondition: popup must start unbootstrapped');"
    , "assert(p.queueLength() == 0 and p.activeCount() == 0,"
    , "  'fixture precondition: popup must start empty');"
    , "return 'ok'"
    ]

-- | Bootstrap the popup module with synthetic texture/font handles, the
--   same way 'Test.Headless.Event.PopupCoordPage' does.
bootstrapLua ∷ Text
bootstrapLua = luaLines
    [ "local p = require('scripts.popup');"
    , "p.bootstrap(1, 2, 3, 1280, 720);"
    , "assert(p.isBootstrapped(), 'bootstrap did not take');"
    , "return 'ok'"
    ]

-- | Queue @n@ entries of one category through the production entry
--   point, @popup.onShowPopup@ — the same function the engine's
--   'LuaShowPopup' broadcast lands on.
queueLua ∷ Text → Int → Text
queueLua category n = luaLines
    [ "local p = require('scripts.popup');"
    , "for i = 1, " <> T.pack (show n) <> " do"
    , "  p.onShowPopup('" <> category <> "', 'entry ' .. i,"
    , "                1, 1, 1, 1, nil, nil)"
    , "end;"
    , "return 'ok'"
    ]

-- | Everything a queue-teardown assertion needs, in one round trip.
statePureLua ∷ Text
statePureLua = luaLines
    [ "local p = require('scripts.popup');"
    , "return { queued = p.queueLength(), active = p.activeCount(),"
    , "         bootstrapped = p.isBootstrapped(),"
    , "         coalescedLines ="
    , "           p.activeLineCount('" <> coalescingCategory <> "'),"
    , "         coalescedLastCount ="
    , "           p.activeLastLineCount('" <> coalescingCategory <> "') }"
    ]

-- | The #742 Escape cascade with every handler ABOVE the popup stubbed
--   inert and a real, visible log panel sitting BELOW it.
--
--   The handlers above are stubbed because each of them owns a
--   window/page this suite has no business booting; the ones below are
--   stubbed because what matters is only whether the press reached
--   them, and a stub is the only way to observe that without a GPU.
--   @scripts\/popup.lua@ and @scripts\/init_keys.lua@ — the two modules
--   whose interaction requirement 2 is about — are both real.
escapeCascadeFixture ∷ Text
escapeCascadeFixture = luaLines
    [ "_G.__hidden = {};"
    , "local function inert() return false end;"
    , "package.loaded['scripts.build_tool_remote_warning'] ="
    , "  { handleKeyDown = inert };"
    , "package.loaded['scripts.ui.context_menu'] ="
    , "  { handleEscape = inert };"
    , "package.loaded['scripts.cargo_inventory_panel'] ="
    , "  { handleKeyDown = inert };"
    , "package.loaded['scripts.crafting_panel'] ="
    , "  { handleKeyDown = inert };"
    , "package.loaded['scripts.etymology_panel'] ="
    , "  { handleKeyDown = inert };"
    , "local function panel(name, visible)"
    , "  return { isVisible = function() return visible end,"
    , "           hide = function()"
    , "             table.insert(_G.__hidden, name) end } end;"
    -- The event log is VISIBLE: it is the lower-priority handler that
    -- must still get the press when dismissAll discards a queue only.
    , "package.loaded['scripts.event_log']        = panel('event_log', true);"
    , "package.loaded['scripts.combat_log']       = panel('combat_log', false);"
    , "package.loaded['scripts.injury_log_panel'] = panel('injury_log', false);"
    , "package.loaded['scripts.unit_log']         = panel('unit_log', false);"
    -- Shift is held: init_keys takes the dismissAll branch, not
    -- dismissTopmost.
    , "engine.isKeyDown = function(k) return k == 'LeftShift' end;"
    , "return 'ok'"
    ]

-- | Press Shift+Escape through the real cascade and report what the
--   press did on its way down.
pressShiftEscapeLua ∷ Text
pressShiftEscapeLua = luaLines
    [ "local p = require('scripts.popup');"
    , "require('scripts.init_keys').onKeyDown('Escape');"
    , "return { queued = p.queueLength(), active = p.activeCount(),"
    , "         hidden = table.concat(_G.__hidden, ',') }"
    ]

-----------------------------------------------------------
-- JSON shapes returned by the Lua probes
-----------------------------------------------------------

-- | Lua has one table type, so an EMPTY list serializes as @{}@ — a
--   JSON object, not an empty array. A list field therefore goes
--   through this wrapper, which accepts both shapes (the same treatment
--   'Test.Headless.Event.PopupCoordPage' gives its own list probes).
newtype LuaList α = LuaList { unLuaList ∷ [α] }

instance FromJSON α ⇒ FromJSON (LuaList α) where
    parseJSON (Object o) | null o = pure (LuaList [])
    parseJSON v                   = LuaList <$> parseJSON v

data PopupState = PopupState
    { psQueued             ∷ Int
    , psActive             ∷ Int
    , psBootstrapped       ∷ Bool
    , psCoalescedLines     ∷ Int
    , psCoalescedLastCount ∷ Int
    } deriving (Show, Eq)

instance FromJSON PopupState where
    parseJSON = withObject "PopupState" $ \o → PopupState
        <$> o .: "queued"
        <*> o .: "active"
        <*> o .: "bootstrapped"
        <*> o .: "coalescedLines"
        <*> o .: "coalescedLastCount"

data CascadeOutcome = CascadeOutcome
    { coQueued ∷ Int
    , coActive ∷ Int
    , coHidden ∷ Text
    } deriving (Show, Eq)

instance FromJSON CascadeOutcome where
    parseJSON = withObject "CascadeOutcome" $ \o → CascadeOutcome
        <$> o .: "queued" <*> o .: "active" <*> o .: "hidden"

-----------------------------------------------------------
-- Lua backend + eval helpers
-----------------------------------------------------------

newBareLuaBackend ∷ EngineEnv → IO LuaBackendState
newBareLuaBackend env = do
    ls ← createLuaBackendState (luaToEngineQueue env) (luaQueue env)
                               (assetPoolRef env) (nextObjectIdRef env)
                               (inputStateRef env) (loggerRef env)
    stateRef ← newIORef ThreadRunning
    registerLuaAPI (lbsLuaState ls) env ls stateRef
    pure ls

isLuaError ∷ Text → Bool
isLuaError t = "error:" `T.isPrefixOf` t ∨ "syntax error:" `T.isPrefixOf` t

evalOk ∷ LuaBackendState → Text → IO Text
evalOk ls code = do
    t ← executeDebugLua (lbsLuaState ls) code
    when (isLuaError t) $ expectationFailure ("Lua error: " ⧺ T.unpack t)
    pure t

-- | Run a fixture step whose only job is to succeed, and prove it did:
--   every fixture snippet ends in @return 'ok'@, so a silent failure
--   cannot be mistaken for a completed setup.
evalStep ∷ LuaBackendState → Text → IO ()
evalStep ls code = evalOk ls code ≫= (`shouldBe` "\"ok\"")

evalInt ∷ LuaBackendState → Text → IO Int
evalInt ls code = do
    t ← evalOk ls code
    case reads (T.unpack t) of
        [(n, "")] → pure n
        _         → do
            expectationFailure ("expected integer, got: " ⧺ T.unpack t)
            pure 0

evalJSON ∷ FromJSON α ⇒ LuaBackendState → Text → IO α
evalJSON ls code = do
    t ← evalOk ls code
    case decode (BL.fromStrict (TE.encodeUtf8 t)) of
        Just v  → pure v
        Nothing → fail ("failed to decode Lua result: " ⧺ T.unpack t)

popupState ∷ LuaBackendState → IO PopupState
popupState ls = evalJSON ls statePureLua

-- | @popup.dismissAll()@'s own return value: the number of ACTIVE CARDS
--   it dismissed.
dismissAll ∷ LuaBackendState → IO Int
dismissAll ls = evalInt ls "return require('scripts.popup').dismissAll()"

-- | The event-log ring as Lua sees it, flattened to @category|text@
--   rows so a before/after comparison is exact.
eventLogRows ∷ LuaBackendState → IO [Text]
eventLogRows ls = unLuaList <$> evalJSON ls (luaLines
    [ "local rows = {};"
    , "for _, e in ipairs(engine.getEventLog()) do"
    , "  table.insert(rows, e.category .. '|' .. e.text)"
    , "end;"
    , "return rows"
    ])

-- | Deliver every 'LuaShowPopup' currently queued for the Lua thread
--   through the SAME dispatch the Lua thread runs, so the queue entry
--   under test was produced by a real emit rather than a hand-built
--   call.
deliverQueuedPopups ∷ EngineEnv → LuaBackendState → IO ()
deliverQueuedPopups env ls = do
    stateRef ← newIORef ThreadRunning
    msgs ← Q.flushQueue (luaQueue env)
    forM_ [ m | m@(LuaShowPopup {}) ← msgs ] $ processLuaMsg env ls stateRef

-----------------------------------------------------------
-- Spec
-----------------------------------------------------------

spec ∷ Spec
spec = around withHeadlessEngine $
  describe "popup queue teardown" $ do

    describe "the queue-only state (no card active, never bootstrapped)" $ do

        it "clears a pending queue that no active card is holding open" $ \env → do
            ls ← newBareLuaBackend env
            evalStep ls preBootstrapFixture
            evalStep ls (queueLua plainCategory 3)
            before ← popupState ls
            -- The precondition the bug needed and the old guard never
            -- met: entries pending, nothing on screen.
            psQueued before       `shouldBe` 3
            psActive before       `shouldBe` 0
            psBootstrapped before `shouldBe` False
            dismissed ← dismissAll ls
            after ← popupState ls
            psQueued after `shouldBe` 0
            psActive after `shouldBe` 0
            -- Requirement 2: no card was on screen, so nothing visible
            -- was dismissed and the press was not consumed.
            dismissed `shouldBe` 0

        it "clears it for a coalescing category too — the lifecycle rule \
           \does not depend on category configuration" $ \env → do
            ls ← newBareLuaBackend env
            evalStep ls preBootstrapFixture
            evalStep ls (queueLua coalescingCategory 3)
            before ← popupState ls
            psQueued before `shouldBe` 3
            psActive before `shouldBe` 0
            dismissed ← dismissAll ls
            after ← popupState ls
            psQueued after `shouldBe` 0
            dismissed `shouldBe` 0

    describe "the bootstrapped control — established behaviour is unchanged" $ do

        it "dismisses every active card AND the overflow queue, returning \
           \the card count" $ \env → do
            ls ← newBareLuaBackend env
            evalStep ls preBootstrapFixture
            evalStep ls bootstrapLua
            -- Eight entries of a non-coalescing category against a
            -- six-slot bound: six spawn, two wait. Both containers are
            -- non-empty, which is the only state in which the old guard
            -- and the repair could have disagreed about the return.
            evalStep ls (queueLua plainCategory 8)
            before ← popupState ls
            psActive before `shouldBe` 6
            psQueued before `shouldBe` 2
            dismissed ← dismissAll ls
            after ← popupState ls
            psActive after `shouldBe` 0
            psQueued after `shouldBe` 0
            -- Still the ACTIVE-card count, never the discarded queued
            -- entries: 6, not 8.
            dismissed `shouldBe` 6

        it "still coalesces a bootstrapped category into one card" $ \env → do
            ls ← newBareLuaBackend env
            evalStep ls preBootstrapFixture
            evalStep ls bootstrapLua
            evalStep ls (queueLua coalescingCategory 3)
            s ← popupState ls
            psActive s             `shouldBe` 1
            psCoalescedLines s     `shouldBe` 1
            psCoalescedLastCount s `shouldBe` 3

    describe "the pre-bootstrap delivery policy (requirement 3)" $ do

        it "delivers entries queued before any gameplay UI when a later \
           \bootstrap drains them" $ \env → do
            ls ← newBareLuaBackend env
            evalStep ls preBootstrapFixture
            evalStep ls (queueLua plainCategory 2)
            evalStep ls (queueLua coalescingCategory 3)
            before ← popupState ls
            psQueued before `shouldBe` 5
            psActive before `shouldBe` 0
            evalStep ls bootstrapLua
            after ← popupState ls
            -- Nothing was dropped for having arrived early: two plain
            -- cards plus one coalesced card, and the queue is drained.
            psQueued after `shouldBe` 0
            psActive after `shouldBe` 3
            -- The coalescing category folded on DRAIN, exactly as it
            -- would have live — the policy defers delivery, it does not
            -- change it.
            psCoalescedLines after     `shouldBe` 1
            psCoalescedLastCount after `shouldBe` 3

        it "does not resurrect entries an earlier dismissAll discarded" $ \env → do
            ls ← newBareLuaBackend env
            evalStep ls preBootstrapFixture
            evalStep ls (queueLua plainCategory 2)
            evalStep ls (queueLua coalescingCategory 3)
            _ ← dismissAll ls
            cleared ← popupState ls
            psQueued cleared `shouldBe` 0
            evalStep ls bootstrapLua
            after ← popupState ls
            -- The other half of the policy: an explicit teardown ends
            -- eligibility permanently, for both categories.
            psQueued after `shouldBe` 0
            psActive after `shouldBe` 0

    describe "the #742 Escape cascade (requirement 2)" $ do

        it "clears the queue and lets the SAME press reach the log panel \
           \below when no card was dismissed" $ \env → do
            ls ← newBareLuaBackend env
            evalStep ls preBootstrapFixture
            evalStep ls escapeCascadeFixture
            evalStep ls (queueLua plainCategory 3)
            outcome ← evalJSON ls pressShiftEscapeLua ∷ IO CascadeOutcome
            -- Both halves in one press: the queue is gone …
            coQueued outcome `shouldBe` 0
            coActive outcome `shouldBe` 0
            -- … and the visible event log still got the Escape, because
            -- dismissAll reported that it consumed nothing.
            coHidden outcome `shouldBe` "event_log"

        it "consumes the press when there WAS a card to dismiss" $ \env → do
            ls ← newBareLuaBackend env
            evalStep ls preBootstrapFixture
            evalStep ls escapeCascadeFixture
            evalStep ls bootstrapLua
            evalStep ls (queueLua plainCategory 2)
            before ← popupState ls
            psActive before `shouldBe` 2
            outcome ← evalJSON ls pressShiftEscapeLua ∷ IO CascadeOutcome
            coActive outcome `shouldBe` 0
            coQueued outcome `shouldBe` 0
            -- The cascade stopped at the popup handler: the log panel
            -- below is untouched, which is the behaviour the queue-only
            -- case must NOT have acquired.
            coHidden outcome `shouldBe` ""

    describe "the event-log ring is a separate store (requirement 5)" $

        it "keeps every row when the transient popup queue is discarded" $ \env → do
            ls ← newBareLuaBackend env
            evalStep ls preBootstrapFixture
            -- Real emits: one statement of emitEventFullOnPage writes the
            -- ring, the next queues the popup broadcast. Delivering that
            -- broadcast through the real dispatch is what puts the SAME
            -- event into scripts/popup.lua's queue.
            emitEvent env plainCategory "Test.Headless.UI.PopupQueueTeardown"
                "load failed: bad slot"
            emitEvent env plainCategory "Test.Headless.UI.PopupQueueTeardown"
                "load failed: missing file"
            deliverQueuedPopups env ls
            queuedBefore ← popupState ls
            psQueued queuedBefore `shouldBe` 2
            psActive queuedBefore `shouldBe` 0
            rowsBefore ← eventLogRows ls
            rowsBefore `shouldSatisfy` ((≥ 2) ∘ length)
            _ ← dismissAll ls
            queuedAfter ← popupState ls
            psQueued queuedAfter `shouldBe` 0
            rowsAfter ← eventLogRows ls
            -- Byte-identical history: the popup queue is transient
            -- session UI, the ring is the session's record of what
            -- happened, and a teardown of the first never edits the
            -- second.
            rowsAfter `shouldBe` rowsBefore
