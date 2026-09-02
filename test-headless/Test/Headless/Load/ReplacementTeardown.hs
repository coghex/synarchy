-- | The "load replacement clears transient session surfaces" gate
--   (#2156). A published load is a whole-session replacement, but the
--   teardown it performed was split across two owners and neither
--   covered the modules holding session-bound UI state:
--
--     * Haskell-side, 'World.Load.Publish.resetTransientState' cleared
--       input, focus, the engine event streams and the popup queue but
--       never reconciled the LOCKED tooltip — 'UI.Tooltip.State.tickLocked'
--       freezes it and ignores hover and source validity, so a tooltip
--       locked before the load stayed on screen and
--       'isPointInLockedTooltip' kept swallowing clicks inside its box.
--     * Lua-side, @uiManager.onSaveLoaded@ — the ONE hook every load
--       reaches whatever triggered it — rebound the world\/HUD, released
--       the #1610 latch, reset the tool and closed the container stack,
--       and ran no general teardown. Six modules owning session-bound
--       state registered no load hook at all: @thought_log@ (a per-unit
--       ring keyed by raw uid, so a uid the replacement REUSES inherited
--       the old thoughts), @combat_log@ and @injury_log_panel@ (histories,
--       grouped battles\/logs, their id allocators, their modal pages),
--       @event_log@ and @unit_log@ (pages that stayed visible, a retained
--       subject uid), and @popup@ (cards and a pending queue still
--       naming entities of the replaced session).
--
--   What this gate pins, per the issue's requirements:
--
--     1. every load reaches ONE explicit cross-owner teardown — the
--        @"saveLoaded"@ transition of @scripts\/ui\/view_teardown.lua@,
--        swept from the production @uiManager.onSaveLoaded@ (every case
--        here drives THAT boundary, never an owner's reset helper);
--     2. thought\/combat\/injury histories, grouped battles\/logs and
--        both id allocators report nothing carried over — including for
--        a uid the replacement session reuses;
--     3. no log page visible beforehand is still visible, and the
--        panels retain no tab\/scroll\/subject state (asserted through the
--        engine's own page-visibility and input-blocking answers, not
--        the modules' bookkeeping alone);
--     4. no popup card of the replaced session stays active, and the
--        pending queue is discarded with it, indexes included;
--     5. the locked tooltip is unlocked AND hidden by the production
--        'resetTransientState', and its former box no longer swallows a
--        point it covered;
--     6. none of the ordinary player callbacks — close\/tab\/scroll
--        widgets' own @onClick@\/@onChange@\/@onScroll@ closures, the
--        routed @onTabClick@\/@onRowClick@\/@onLineClick@ family — fires
--        during the clear;
--     7. the rebinding, tool-reset and container-close effects
--        @onSaveLoaded@ already had are unchanged; and
--     8. a load reached BEFORE any gameplay UI was bootstrapped (the menu
--        + debug-console path) raises no hook error.
--
--   Requirement 8 of the issue (the Exit-to-Menu registry's id set and
--   single caller are untouched) is pinned by the existing
--   "Test.Headless.Lua.SessionTeardown" gate.
--
--   Same technique as "Test.Headless.UI.ContainerWindowStack": one shared
--   headless engine and one bare Lua VM, the REAL production modules
--   driven on a real page tree with synthetic texture\/font handles. Only
--   the engine reads the histories are fed from (@thought.drainEvents@,
--   @combat.drainEvents@, @injury.drainEvents@, @engine.gameTime@) and the
--   two world-thread effects @onSaveLoaded@ triggers
--   (@worldView.sendTexturesToWorld@, the container stack's
--   @closeIfOpen@) are stubbed — the latter two as counting spies, so the
--   requirement-7 effects are asserted as CALLS the boundary still makes.
--
--   Run just this gate: @cabal test synarchy-test-headless
--   --test-options='--match "load replacement clears transient session surfaces"'@.
module Test.Headless.Load.ReplacementTeardown (spec) where

import UPrelude
import Test.Hspec
import Data.Aeson (FromJSON(..), decode, withObject, (.:))
import qualified Data.ByteString.Lazy as BL
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.IORef (newIORef, readIORef, writeIORef, atomicModifyIORef')
import Engine.Asset.Handle (FontHandle(..))
import Engine.Core.State (EngineEnv(..))
import Engine.Core.Thread (ThreadControl(..))
import Engine.Graphics.Config (vcUIScale)
import Engine.Graphics.Font.Data (defaultFontCache)
import Engine.Scripting.Lua.API (registerLuaAPI)
import Engine.Scripting.Lua.Thread (createLuaBackendState)
import Engine.Scripting.Lua.Thread.Console (executeDebugLua)
import Engine.Scripting.Lua.Types (LuaBackendState(..))
import Test.Headless.Harness (withHeadlessEngine, installHudWorldPage)
import UI.Manager (createPage, showPage)
import UI.Tooltip
    ( rebuildVisuals, isPointInLockedTooltip, isTooltipLocked
    , isTooltipVisible )
import UI.Types
import World.Load.Publish (resetTransientState)

-----------------------------------------------------------
-- Fixture
-----------------------------------------------------------

luaLines ∷ [Text] → Text
luaLines = T.intercalate " "

withSharedFixture ∷ ((EngineEnv, LuaBackendState) → IO ()) → IO ()
withSharedFixture action = withHeadlessEngine $ \env → do
    ls ← newBareLuaBackend env
    action (env, ls)

resetFixture ∷ EngineEnv → LuaBackendState → IO ()
resetFixture env ls = do
    writeIORef (uiManagerRef env) emptyUIPageManager
    atomicModifyIORef' (videoConfigRef env) $ \c → (c { vcUIScale = 1.0 }, ())
    -- #1366: hud.lua is loaded by ui_manager and addresses hud.worldId
    -- ("main_world") — see 'installHudWorldPage'.
    installHudWorldPage env
    cleared ← evalOk ls
        "for k, _ in pairs(package.loaded) do package.loaded[k] = nil end; return true"
    cleared `shouldBe` "true"

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
evalOk ls src = do
    r ← executeDebugLua (lbsLuaState ls) src
    when (isLuaError r) $ expectationFailure ("Lua error: " ⧺ T.unpack r)
    pure r

evalJSON ∷ FromJSON α ⇒ LuaBackendState → Text → IO α
evalJSON ls src = do
    r ← evalOk ls src
    case decode (BL.fromStrict (TE.encodeUtf8 r)) of
        Just v  → pure v
        Nothing → fail ("failed to decode Lua result: " ⧺ T.unpack r)

-- | Every fixture step ends in @return 'ok'@, so a silent failure cannot
--   pass for a completed setup.
evalStep ∷ LuaBackendState → Text → IO ()
evalStep ls src = evalOk ls src ≫= (`shouldBe` "\"ok\"")

-----------------------------------------------------------
-- Lua fixtures
-----------------------------------------------------------

-- | Load the real @ui_manager@ (which loads every module under test)
--   and install the two kinds of spy the suite relies on:
--
--     * @__fired@ counts every PLAYER callback: the @onClick@ \/
--       @onChange@ \/ @onScroll@ closure of every button, tabbar and
--       scrollbar built from here on (wrapped at construction, so the
--       log panels' close buttons, event_log's tab strip, every history
--       scrollbar and every popup's OK\/X button are covered), plus the
--       ui_manager-routed handler family on each module.
--     * @__effects@ counts the two world-facing effects @onSaveLoaded@
--       already performed, which the sweep must leave in place.
--
--   @world.getActiveWorldId@ answers the HUD's own page so the rebinding
--   branch runs for real.
bootLua ∷ Text
bootLua = luaLines
    [ "_G.__fired, _G.__effects = {}, {};"
    , "local function bump(t, k) t[k] = (t[k] or 0) + 1 end;"
    , "local function wrapNew(mod, cbKey)"
    , "  local orig = mod.new;"
    , "  mod.new = function(params)"
    , "    local cb = params and params[cbKey];"
    , "    if cb then params[cbKey] = function(...)"
    , "      bump(_G.__fired, cbKey); return cb(...) end end;"
    , "    return orig(params) end end;"
    , "wrapNew(require('scripts.ui.button'),    'onClick');"
    , "wrapNew(require('scripts.ui.tabbar'),    'onChange');"
    , "wrapNew(require('scripts.ui.scrollbar'), 'onScroll');"
    , "world.getActiveWorldId = function() return 'main_world' end;"
    , "engine.gameTime = function() return 20 end;"
    , "UIM = require('scripts.ui_manager');"
    , "WM  = require('scripts.world_manager');"
    , "HUD = require('scripts.hud');"
    , "WV  = require('scripts.world_view');"
    , "WV.sendTexturesToWorld = function(id)"
    , "  bump(_G.__effects, 'sendTextures'); _G.__sentTo = id; return true end;"
    , "CIP = require('scripts.cargo_inventory_panel');"
    , "CIP.closeIfOpen = function() bump(_G.__effects, 'containerClose') end;"
    , "TL = require('scripts.thought_log');"
    , "CL = require('scripts.combat_log');"
    , "IL = require('scripts.injury_log_panel');"
    , "EL = require('scripts.event_log');"
    , "UL = require('scripts.unit_log');"
    , "P  = require('scripts.popup');"
    , "for _, spec in ipairs({"
    , "    { CL, {'onTabClick','onScrollPrev','onScrollNext','onContentScroll'} },"
    , "    { IL, {'onTabClick','onScrollPrev','onScrollNext','onContentScroll'} },"
    , "    { EL, {'onRowClick'} },"
    , "    { UL, {'onTabClick','onContentScroll'} },"
    , "    { P,  {'onLineClick','onMuteToggleClick'} } }) do"
    , "  local m, keys = spec[1], spec[2];"
    , "  for _, k in ipairs(keys) do"
    , "    local f = m[k];"
    , "    m[k] = function(...) bump(_G.__fired, k); return f(...) end end end;"
    , "function count(t) local n = 0; for _ in pairs(t) do n = n + 1 end; return n end;"
    , "function firedTotal() local n = 0;"
    , "  for _, v in pairs(_G.__fired) do n = n + v end; return n end;"
    , "return 'ok'"
    ]

-- | Bootstrap every surface with synthetic handles, feed each history
--   from its (stubbed) engine stream through the module's own drain,
--   open all four panels in a non-default view state, and put eight
--   cards of a non-coalescing category through @popup.onShowPopup@ —
--   six active against the six-slot bound, two queued — so both popup
--   containers are non-empty at the moment the load publishes.
sceneLua ∷ Text
sceneLua = luaLines
    [ "for _, m in ipairs({ CL, IL, EL, UL, P }) do m.bootstrap(1, 2, 3, 1280, 720) end;"
    , "thought.drainEvents = function() return {"
    , "  { target = 7, ts = 10, payload = { text = 'old thought' } },"
    , "  { target = 9, ts = 11, payload = { text = 'old too' } } } end;"
    , "TL.update(0.1); thought.drainEvents = function() return {} end;"
    , "combat.drainEvents = function() return {"
    , "  { kind = 'miss', attacker = 7,  target = 9,  ts = 12, payload = {} },"
    , "  { kind = 'miss', attacker = 21, target = 22, ts = 13, payload = {} } } end;"
    , "CL.update(0.1); combat.drainEvents = function() return {} end;"
    , "injury.drainEvents = function() return {"
    , "  { kind = 'fall', target = 7, ts = 14, payload = {} },"
    , "  { kind = 'fall', target = 9, ts = 15, payload = {} } } end;"
    , "IL.update(0.1); injury.drainEvents = function() return {} end;"
    , "assert(#CL.battles == 2 and CL.nextBattleId == 3, 'scene: two battles');"
    , "assert(#IL.unitLogs == 2 and IL.nextLogId == 3, 'scene: two unit logs');"
    , "assert(#TL.unitEntries(7) == 1, 'scene: a thought for uid 7');"
    , "CL.show(); CL.activeTabId = 2; CL.scrollOffset = 1; CL.contentScroll = 3;"
    , "IL.show(); IL.activeTabId = 2; IL.scrollOffset = 1; IL.contentScroll = 3;"
    , "EL.show(); EL.activeTabKey = 'combat'; EL.scrollOffset = 4;"
    , "UL.show(7); UL.activeTabKey = 'combat'; UL.contentScroll = 2;"
    , "for i = 1, 8 do P.onShowPopup('save_load', 'old card ' .. i, 1, 1, 1, 1, nil, nil) end;"
    , "assert(P.activeCount() == 6 and P.queueLength() == 2, 'scene: cards + queue');"
    , "assert(CL.isVisible() and IL.isVisible() and EL.isVisible() and UL.isVisible(),"
    , "       'scene: all four panels open');"
    , "assert(UI.isInputBlocked(), 'scene: a modal log page must block input');"
    , "_G.__tables = { TL.byUnit, CL.allEvents, CL.battles, IL.allEvents, IL.unitLogs };"
    , "_G.__pages  = { CL.pageId, IL.pageId, EL.pageId, UL.pageId };"
    , "_G.__fired  = {};"
    , "return 'ok'"
    ]

-- | The production boundary — the only thing a case ever calls to
--   perform the load-side reset.
loadLua ∷ Text
loadLua = "UIM.onSaveLoaded({ 7, 9 }, { 11 }); return 'ok'"

afterLoadProbeLua ∷ Text
afterLoadProbeLua = luaLines
    [ "local visiblePages = 0;"
    , "for _, pid in ipairs(_G.__pages) do"
    , "  if UI.isPageVisible(pid) then visiblePages = visiblePages + 1 end end;"
    , "local inPlace = _G.__tables[1] == TL.byUnit and _G.__tables[2] == CL.allEvents"
    , "  and _G.__tables[3] == CL.battles and _G.__tables[4] == IL.allEvents"
    , "  and _G.__tables[5] == IL.unitLogs;"
    , "return {"
    , "  thought7 = #TL.unitEntries(7), thought9 = #TL.unitEntries(9),"
    , "  thoughtRows = count(TL.byUnit),"
    , "  combatAll = #CL.allEvents, combatBattles = #CL.battles,"
    , "  combatNext = CL.nextBattleId, combatTab = tostring(CL.activeTabId),"
    , "  combatStrip = CL.scrollOffset, combatContent = CL.contentScroll,"
    , "  combatVisible = CL.isVisible(), combat7 = #CL.unitEntries(7),"
    , "  combatTabBoxes = count(CL.tabClickBoxes),"
    , "  injuryAll = #IL.allEvents, injuryLogs = #IL.unitLogs,"
    , "  injuryNext = IL.nextLogId, injuryTab = tostring(IL.activeTabId),"
    , "  injuryStrip = IL.scrollOffset, injuryContent = IL.contentScroll,"
    , "  injuryVisible = IL.isVisible(), injury7 = #IL.unitEntries(7),"
    , "  injuryTabBoxes = count(IL.tabClickBoxes),"
    , "  eventVisible = EL.isVisible(), eventTab = EL.activeTabKey,"
    , "  eventScroll = EL.scrollOffset,"
    , "  unitVisible = UL.isVisible(), unitHasUid = UL.uid ~= nil,"
    , "  unitTab = UL.activeTabKey, unitScroll = UL.contentScroll,"
    , "  unitTabBoxes = count(UL.tabClickBoxes),"
    , "  visiblePages = visiblePages, inputBlocked = UI.isInputBlocked(),"
    , "  popupActive = P.activeCount(), popupQueued = P.queueLength(),"
    , "  popupIndexes = count(P.lineByClickBox) + count(P.muteToggleByHandle)"
    , "                 + count(P.activeByCategory),"
    , "  inPlace = inPlace, fired = firedTotal(),"
    , "  sendTextures = _G.__effects.sendTextures or 0,"
    , "  sentTo = tostring(_G.__sentTo),"
    , "  containerClose = _G.__effects.containerClose or 0,"
    , "  currentWorld = tostring(WM.currentWorld), worldActive = WM.active == true,"
    , "  hudWorld = tostring(HUD.worldId), toolDirty = HUD.mainWorldToolDirty == true,"
    , "  latchReleased = not require('scripts.lib.session_teardown').isTornDown() }"
    ]

-- | The replacement session reuses uid 7: feed one fresh event per
--   history and read back exactly what each reports for that uid.
reusedUidLua ∷ Text
reusedUidLua = luaLines
    [ "thought.drainEvents = function() return {"
    , "  { target = 7, ts = 30, payload = { text = 'new thought' } } } end;"
    , "TL.update(0.1); thought.drainEvents = function() return {} end;"
    , "combat.drainEvents = function() return {"
    , "  { kind = 'miss', attacker = 7, target = 9, ts = 31, payload = {} } } end;"
    , "CL.update(0.1); combat.drainEvents = function() return {} end;"
    , "injury.drainEvents = function() return {"
    , "  { kind = 'fall', target = 7, ts = 32, payload = {} } } end;"
    , "IL.update(0.1); injury.drainEvents = function() return {} end;"
    , "local t7 = TL.unitEntries(7);"
    , "return { thought7 = #t7, thoughtText = t7[1] and t7[1].text or '',"
    , "         combat7 = #CL.unitEntries(7), battleId = CL.battles[1] and CL.battles[1].id or -1,"
    , "         battles = #CL.battles,"
    , "         injury7 = #IL.unitEntries(7), logId = IL.unitLogs[1] and IL.unitLogs[1].id or -1,"
    , "         logs = #IL.unitLogs }"
    ]

-----------------------------------------------------------
-- JSON shapes
-----------------------------------------------------------

data AfterLoad = AfterLoad
    { alThought7, alThought9, alThoughtRows ∷ Int
    , alCombatAll, alCombatBattles, alCombatNext ∷ Int
    , alCombatTab ∷ Text
    , alCombatStrip, alCombatContent ∷ Int
    , alCombatVisible ∷ Bool
    , alCombat7, alCombatTabBoxes ∷ Int
    , alInjuryAll, alInjuryLogs, alInjuryNext ∷ Int
    , alInjuryTab ∷ Text
    , alInjuryStrip, alInjuryContent ∷ Int
    , alInjuryVisible ∷ Bool
    , alInjury7, alInjuryTabBoxes ∷ Int
    , alEventVisible ∷ Bool
    , alEventTab ∷ Text
    , alEventScroll ∷ Int
    , alUnitVisible, alUnitHasUid ∷ Bool
    , alUnitTab ∷ Text
    , alUnitScroll, alUnitTabBoxes ∷ Int
    , alVisiblePages ∷ Int
    , alInputBlocked ∷ Bool
    , alPopupActive, alPopupQueued, alPopupIndexes ∷ Int
    , alInPlace ∷ Bool
    , alFired, alSendTextures ∷ Int
    , alSentTo ∷ Text
    , alContainerClose ∷ Int
    , alCurrentWorld ∷ Text
    , alWorldActive ∷ Bool
    , alHudWorld ∷ Text
    , alToolDirty, alLatchReleased ∷ Bool
    } deriving (Show)

instance FromJSON AfterLoad where
    parseJSON = withObject "AfterLoad" $ \o → AfterLoad
        <$> o .: "thought7" <*> o .: "thought9" <*> o .: "thoughtRows"
        <*> o .: "combatAll" <*> o .: "combatBattles" <*> o .: "combatNext"
        <*> o .: "combatTab"
        <*> o .: "combatStrip" <*> o .: "combatContent"
        <*> o .: "combatVisible"
        <*> o .: "combat7" <*> o .: "combatTabBoxes"
        <*> o .: "injuryAll" <*> o .: "injuryLogs" <*> o .: "injuryNext"
        <*> o .: "injuryTab"
        <*> o .: "injuryStrip" <*> o .: "injuryContent"
        <*> o .: "injuryVisible"
        <*> o .: "injury7" <*> o .: "injuryTabBoxes"
        <*> o .: "eventVisible"
        <*> o .: "eventTab"
        <*> o .: "eventScroll"
        <*> o .: "unitVisible" <*> o .: "unitHasUid"
        <*> o .: "unitTab"
        <*> o .: "unitScroll" <*> o .: "unitTabBoxes"
        <*> o .: "visiblePages"
        <*> o .: "inputBlocked"
        <*> o .: "popupActive" <*> o .: "popupQueued" <*> o .: "popupIndexes"
        <*> o .: "inPlace"
        <*> o .: "fired" <*> o .: "sendTextures"
        <*> o .: "sentTo"
        <*> o .: "containerClose"
        <*> o .: "currentWorld"
        <*> o .: "worldActive"
        <*> o .: "hudWorld"
        <*> o .: "toolDirty" <*> o .: "latchReleased"

data ReusedUid = ReusedUid
    { ruThought7 ∷ Int
    , ruThoughtText ∷ Text
    , ruCombat7, ruBattleId, ruBattles ∷ Int
    , ruInjury7, ruLogId, ruLogs ∷ Int
    } deriving (Show)

instance FromJSON ReusedUid where
    parseJSON = withObject "ReusedUid" $ \o → ReusedUid
        <$> o .: "thought7" <*> o .: "thoughtText"
        <*> o .: "combat7" <*> o .: "battleId" <*> o .: "battles"
        <*> o .: "injury7" <*> o .: "logId" <*> o .: "logs"

-----------------------------------------------------------
-- Tooltip fixture (the Haskell-owned half)
-----------------------------------------------------------

-- | A tooltip built and locked the way the engine does it: a real
--   tooltip page, 'rebuildVisuals' (which creates the box geometry
--   element the click-swallow region comes from), then the lock. The
--   boxless default style is used deliberately — it is the #117 branch
--   where the swallow region is an invisible geometry element rather
--   than a textured box, so a reset that only tore down textures would
--   still leave it swallowing.
lockedTooltipManager ∷ TooltipContent → (PageHandle, UIPageManager)
lockedTooltipManager content =
    let style = defaultTooltipStyle { tsFont = FontHandle 1 }
        mgr0 = emptyUIPageManager
                 { upmTooltip = (upmTooltip emptyUIPageManager)
                     { ttsStyle = style } }
        (pageH, mgr1) = createPage "__tooltip" LayerTooltip mgr0
        mgr2 = showPage pageH mgr1
        mgr3 = rebuildVisuals pageH content defaultFontCache mgr2
        tts  = (upmTooltip mgr3)
                 { ttsLocked        = True
                 , ttsActiveContent = Just content
                 , ttsActivePage    = Just pageH }
    in (pageH, mgr3 { upmTooltip = tts })

-- | Centre of the locked tooltip's box — the point a click would be
--   swallowed at.
boxCentre ∷ UIPageManager → Maybe (Float, Float)
boxCentre mgr = do
    h    ← ttsBoxHandle (upmTooltip mgr)
    el   ← Map.lookup h (upmElements mgr)
    let (x, y) = uePosition el
        (w, s) = ueSize el
    pure (x + w / 2, y + s / 2)

-----------------------------------------------------------
-- Spec
-----------------------------------------------------------

spec ∷ Spec
spec = aroundAll withSharedFixture $
  describe "load replacement clears transient session surfaces (#2156)" $ do

    describe "the Haskell-owned reset (World.Load.Publish.resetTransientState)" $

        it "unlocks and hides a locked tooltip, so its box no longer \
           \swallows pointer input" $ \(env, _) → do
            let content = TooltipContent (Just "pinned") Nothing [] Nothing
                (pageH, seeded) = lockedTooltipManager content
            centre ← maybe (fail "fixture built no tooltip box") pure
                           (boxCentre seeded)
            -- The precondition the bug needed: locked, visible, and
            -- swallowing at its centre.
            isTooltipLocked seeded `shouldBe` True
            isTooltipVisible seeded `shouldBe` True
            isPointInLockedTooltip centre seeded `shouldBe` True
            writeIORef (uiManagerRef env) seeded
            -- The reset a load publish actually performs, on the
            -- production manager ref.
            resetTransientState env
            after ← readIORef (uiManagerRef env)
            isTooltipLocked after `shouldBe` False
            isTooltipVisible after `shouldBe` False
            isPointInLockedTooltip centre after `shouldBe` False
            -- Hidden synchronously, visuals gone: not merely unlocked
            -- and left for a later tick to notice.
            ttsBoxHandle (upmTooltip after) `shouldBe` Nothing
            ttsActiveContent (upmTooltip after) `shouldBe` Nothing
            Set.member pageH (upmVisiblePages after) `shouldBe` False

    describe "the Lua-owned reset, through the production uiManager.onSaveLoaded" $ do

        it "clears the six surfaces, hides every open log page, discards \
           \the popup queue with the cards, fires no player callback, and \
           \keeps the existing rebinding, tool and container effects" $ \(env, ls) → do
            resetFixture env ls
            evalStep ls bootLua
            evalStep ls sceneLua
            evalStep ls loadLua
            p ← evalJSON ls afterLoadProbeLua ∷ IO AfterLoad
            -- Requirement 2: histories, groups and allocators.
            alThought7 p `shouldBe` 0
            alThought9 p `shouldBe` 0
            alThoughtRows p `shouldBe` 0
            alCombatAll p `shouldBe` 0
            alCombatBattles p `shouldBe` 0
            alCombatNext p `shouldBe` 1
            alCombat7 p `shouldBe` 0
            alInjuryAll p `shouldBe` 0
            alInjuryLogs p `shouldBe` 0
            alInjuryNext p `shouldBe` 1
            alInjury7 p `shouldBe` 0
            -- Emptied IN PLACE: every module-published table is the
            -- same object it was, so a direct reference stays valid.
            alInPlace p `shouldBe` True
            -- Requirement 3 (+ the review's combat/injury amendment):
            -- no page that was visible is still visible — by the
            -- engine's own answer, page by page and as the modal
            -- boundary — and no tab/scroll/subject state survives.
            alVisiblePages p `shouldBe` 0
            alInputBlocked p `shouldBe` False
            alCombatVisible p `shouldBe` False
            alCombatTab p `shouldBe` "all"
            alCombatStrip p `shouldBe` 0
            alCombatContent p `shouldBe` 0
            alCombatTabBoxes p `shouldBe` 0
            alInjuryVisible p `shouldBe` False
            alInjuryTab p `shouldBe` "all"
            alInjuryStrip p `shouldBe` 0
            alInjuryContent p `shouldBe` 0
            alInjuryTabBoxes p `shouldBe` 0
            alEventVisible p `shouldBe` False
            alEventTab p `shouldBe` "all"
            alEventScroll p `shouldBe` 0
            alUnitVisible p `shouldBe` False
            alUnitHasUid p `shouldBe` False
            alUnitTab p `shouldBe` "all"
            alUnitScroll p `shouldBe` 0
            alUnitTabBoxes p `shouldBe` 0
            -- Requirement 4: no active card, no queued entry waiting to
            -- become one, no stale lookup index.
            alPopupActive p `shouldBe` 0
            alPopupQueued p `shouldBe` 0
            alPopupIndexes p `shouldBe` 0
            -- Requirement 6: the clear wrote state directly — not one
            -- close/tab/scroll/dismiss callback ran.
            alFired p `shouldBe` 0
            -- Requirement 7: what onSaveLoaded already did, unchanged.
            alCurrentWorld p `shouldBe` "main_world"
            alWorldActive p `shouldBe` True
            alHudWorld p `shouldBe` "main_world"
            alSendTextures p `shouldBe` 1
            alSentTo p `shouldBe` "main_world"
            alToolDirty p `shouldBe` True
            alContainerClose p `shouldBe` 1
            alLatchReleased p `shouldBe` True

        it "lets a uid the replacement session reuses start from nothing \
           \in every history, with fresh group ids" $ \(env, ls) → do
            resetFixture env ls
            evalStep ls bootLua
            evalStep ls sceneLua
            evalStep ls loadLua
            r ← evalJSON ls reusedUidLua ∷ IO ReusedUid
            -- Exactly the one post-load entry each, nothing carried
            -- over from the replaced session's uid 7.
            ruThought7 r `shouldBe` 1
            ruThoughtText r `shouldSatisfy` T.isInfixOf "new thought"
            ruThoughtText r `shouldNotSatisfy` T.isInfixOf "old"
            ruCombat7 r `shouldBe` 1
            ruBattles r `shouldBe` 1
            -- Allocators restarted: the first post-load battle/log is
            -- id 1 again, not 3.
            ruBattleId r `shouldBe` 1
            ruInjury7 r `shouldBe` 1
            ruLogs r `shouldBe` 1
            ruLogId r `shouldBe` 1

        it "is idempotent, and raises no hook error when it runs before \
           \any gameplay UI was bootstrapped (the menu + debug-console path)" $ \(env, ls) → do
            resetFixture env ls
            evalStep ls bootLua
            -- Count every hook failure the sweep would report.
            evalStep ls (luaLines
                [ "_G.__errors = 0;"
                , "engine.logError = function() _G.__errors = _G.__errors + 1 end;"
                , "return 'ok'" ])
            -- Nothing bootstrapped, nothing open, nothing recorded.
            evalStep ls loadLua
            -- And again, on the already-clean state.
            evalStep ls loadLua
            errors ← evalOk ls "return _G.__errors"
            errors `shouldBe` "0"
            blocked ← evalOk ls "return UI.isInputBlocked()"
            blocked `shouldBe` "false"
            calls ← evalOk ls "return _G.__effects.sendTextures"
            calls `shouldBe` "2"
