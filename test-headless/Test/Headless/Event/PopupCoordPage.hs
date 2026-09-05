-- | #1588 gate: a player event's clickable coordinate is interpreted in
--   the world page it was EMITTED in, never in whichever page happens
--   to be active when the line is clicked.
--
--   The page identity used to be dropped twice on the way from emit to
--   click — once because 'Engine.Scripting.Lua.Types.LuaShowPopup'
--   carried no page at all, and once because @scripts\/event_log.lua@'s
--   row-click replay forwarded only the coordinates. Between them, a
--   coordinate stored in the session-lifetime event ring named no frame
--   whatsoever, while @camera.goToTile@ resolves the ACTIVE world.
--
--   Both halves of the repair are covered here, in the order the data
--   actually flows:
--
--   * __Attribution__: the one rule in
--     'Engine.PlayerEvent.Emit.resolveEventPage', asserted against a
--     live engine — the stored ring entry and the queued
--     'LuaShowPopup' carry the same effective page, and no emitter had
--     to opt in to get one. #2285 deleted the third surface, an
--     engine-side popup queue nothing ever read, so the delivered
--     message is now asserted whole (category, text, RGBA, coords,
--     page) rather than for its page alone.
--   * __Activation__: the refusal decision itself, on a bare Lua
--     backend in the style of 'Test.Headless.UI.PopupPlacement' —
--     same-page pans, different-page and page-less do not, the cycle
--     survives a refusal, and an event-log replay is indistinguishable
--     from live delivery.
--
--   Two deliberate fixture choices in the activation group:
--
--   * The ACTIVE PAGE is real. Each case installs a genuine
--     'WorldManager' and lets @world.getActiveWorldId@ resolve it
--     through 'Engine.Core.State.resolveActiveWorld' — the very
--     resolution @camera.goToTile@ itself performs. Stubbing that would
--     let the two disagree, which is the whole failure mode.
--   * The CAMERA is stubbed. Headless, the fixture pages carry no
--     generation parameters, so a real @camera.goToTile@ is a silent
--     no-op and "did not pan" would be unobservable — indistinguishable
--     from a pan correctly refused. The stub records every call, which
--     is what turns requirement 4 into an assertion.
module Test.Headless.Event.PopupCoordPage (spec) where

import UPrelude
import Test.Hspec
import Data.Aeson (FromJSON(..), Value(..), decode, withObject, (.:), (.:?))
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.IORef (newIORef, writeIORef, readIORef, modifyIORef')
import qualified Data.HashMap.Strict as HM
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Control.Concurrent.STM (readTVarIO)
import qualified Engine.Core.Queue as Q
import Engine.Core.State (EngineEnv(..))
import Engine.Core.Thread (ThreadControl(..))
import Engine.PlayerEvent.Emit
  (PlayerEvent(..), StoredEvent(..), EventStore(..), CategoryCfg(..)
  , emitEvent, emitEventAt, emitEventFullOnPage)
import Engine.Scripting.Lua.API (registerLuaAPI)
import Engine.Scripting.Lua.Thread (createLuaBackendState)
import Engine.Scripting.Lua.Thread.Console (executeDebugLua)
import Engine.Scripting.Lua.Thread.Dispatch (processLuaMsg)
import Engine.Scripting.Lua.Types (LuaBackendState(..), LuaMsg(..))
import Test.Headless.Harness (withHeadlessEngine)
import World.Page.Types (WorldPageId(..))
import World.Types (WorldManager(..), emptyWorldState, emptyWorldManager)

-----------------------------------------------------------
-- Fixture pages
-----------------------------------------------------------

-- | The page the fixture makes ACTIVE. Deliberately not @main_world@: a
--   hardcoded fallback to the conventional name would still satisfy a
--   test written against it, so neither fixture page is a name any
--   production path can produce by accident.
alphaPage ∷ Text
alphaPage = "page_alpha"

-- | A second, loaded-but-hidden page — the "wrong world" every refusal
--   case aims a coordinate at.
betaPage ∷ Text
betaPage = "page_beta"

-- | Install both fixture pages, with @visible@ naming the active one.
--   Bare 'emptyWorldState' pages carry no generation parameters, which
--   is what keeps the world worker alive (see
--   'Test.Headless.Harness.installHudWorldPage' for the incident that
--   established that rule).
installPages ∷ EngineEnv → [Text] → IO ()
installPages env visible = do
    alpha ← emptyWorldState
    beta  ← emptyWorldState
    writeIORef (worldManagerRef env) emptyWorldManager
        { wmWorlds  = [ (WorldPageId alphaPage, alpha)
                      , (WorldPageId betaPage,  beta) ]
        , wmVisible = map WorldPageId visible }

-- | No worlds at all — the main menu, and requirement 6's other half.
installNoPages ∷ EngineEnv → IO ()
installNoPages env =
    writeIORef (worldManagerRef env) emptyWorldManager
        { wmWorlds = [], wmVisible = [] }

-----------------------------------------------------------
-- Engine-side helpers
-----------------------------------------------------------

-- | The newest entry in the event ring. Unwraps the #1714
--   'StoredEvent' row: every assertion here is about the EVENT, and
--   the sequence metadata is 'Test.Headless.Event.PlayerEventProgress'
--   business.
newestEvent ∷ EngineEnv → IO PlayerEvent
newestEvent env =
    seEvent <$> (lastOf "event log" =≪ (esRows <$> readTVarIO (eventStoreRef env)))

lastOf ∷ String → Seq α → IO α
lastOf what s = case Seq.viewr s of
    Seq.EmptyR   → fail (what ⧺ " is empty — nothing was emitted")
    _ Seq.:> ev  → pure ev

-- | One drained 'LuaShowPopup', with every field the message carries.
--
--   Since #2285 removed the engine-side popup queue, this message is
--   the ONLY place a popup-enabled event goes, so the assertions below
--   pin it whole rather than projecting out the two fields #1588 was
--   about. A record rather than an 8-tuple so a mismatch renders which
--   field drifted.
data DeliveredPopup = DeliveredPopup
    { dpCategory ∷ Text
    , dpText     ∷ Text
    , dpColor    ∷ (Float, Float, Float, Float)
    , dpCoords   ∷ Maybe (Int, Int)
    , dpPage     ∷ Maybe Text
    } deriving (Eq, Show)

-- | Every 'LuaShowPopup' currently waiting on the engine-to-Lua queue.
--   Draining is destructive, so a case that asserts on it emits first
--   and drains once.
drainShowPopups ∷ EngineEnv → IO [DeliveredPopup]
drainShowPopups env = do
    msgs ← Q.flushQueue (luaQueue env)
    pure [ DeliveredPopup c t (r, g, b, a) mCoords mPage
         | LuaShowPopup c t r g b a mCoords mPage ← msgs ]

-- | The live registry entry for @category@ — the settings the emit
--   path itself reads. The popup's colour is asserted against THIS
--   rather than a literal copied out of
--   @data/notification_categories.yaml@, so the case proves the
--   registry value is what reaches the message (in channel order)
--   instead of re-stating the YAML in Haskell.
liveCategoryCfg ∷ EngineEnv → Text → IO CategoryCfg
liveCategoryCfg env category = do
    cfgMap ← readIORef (notificationCfgRef env)
    case HM.lookup category cfgMap of
        Just cfg → pure cfg
        Nothing  → fail ("category '" ⧺ T.unpack category
                          ⧺ "' is not in the notification registry")

-- | Rewrite one category's notification switches for the duration of a
--   case. 'CategoryCfg' makes @log@ and @popup@ independent, and the
--   shipped registry turns both on together, so the only way to
--   exercise the popup-without-log half is to set it here.
setCategorySwitches ∷ EngineEnv → Text → Bool → Bool → IO ()
setCategorySwitches env category logOn popupOn =
    modifyIORef' (notificationCfgRef env)
        (HM.adjust (\cfg → cfg { ccLog = logOn, ccPopup = popupOn })
                   category)

-- | How many rows the event ring currently holds.
eventRowCount ∷ EngineEnv → IO Int
eventRowCount env = Seq.length ∘ esRows <$> readTVarIO (eventStoreRef env)

-----------------------------------------------------------
-- Lua-side helpers
-----------------------------------------------------------

luaLines ∷ [Text] → Text
luaLines = T.intercalate " "

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

-- | Run a Lua snippet whose only job is to succeed, and prove it did:
--   every fixture snippet here ends in @return 'ok'@, so a silent
--   failure cannot be mistaken for a completed setup.
evalStep ∷ LuaBackendState → Text → IO ()
evalStep ls code = evalOk ls code ≫= (`shouldBe` "\"ok\"")

evalJSON ∷ FromJSON α ⇒ LuaBackendState → Text → IO α
evalJSON ls code = do
    t ← evalOk ls code
    case decode (BL.fromStrict (TE.encodeUtf8 t)) of
        Just v  → pure v
        Nothing → fail ("failed to decode Lua result: " ⧺ T.unpack t)

-- | How many times @needle@ occurs in @hay@ — used to prove a repeated
--   refusal does not stack a second unavailable marker onto the line.
occurrences ∷ Text → Text → Int
occurrences needle hay = length (T.breakOnAll needle hay)

-- | The marker @scripts\/popup.lua@ appends to a line whose activation
--   was refused. Spelled once here and compared against the ENGINE's
--   own rendered text, so a change to the wording fails loudly instead
--   of quietly un-testing the feedback requirement.
unavailableMarker ∷ Text
unavailableMarker = "(location unavailable)"

-- | Dispatch a real 'LuaShowPopup' the way the Lua thread does, so the
--   8th broadcast argument added by
--   'Engine.Scripting.Lua.Thread.Dispatch' is exercised rather than
--   assumed.
deliverPopup ∷ EngineEnv → LuaBackendState → Maybe (Int, Int) → Maybe Text
             → IO ()
deliverPopup env ls mCoords mPage = do
    stateRef ← newIORef ThreadRunning
    processLuaMsg env ls stateRef
        (LuaShowPopup "location_discovery" "Discovered: ruin"
                      0.9 0.8 0.3 1.0 mCoords mPage)

-- | Fixture preamble for every activation case: stub the camera (see
--   the module header), stub the one @scripts\/hud.lua@ field
--   @renderPopup@ reaches for, and register @scripts\/popup.lua@
--   through the SAME @engine.loadScript@ route production uses — which
--   is what makes it a 'broadcastToModules' target.
popupFixtureLua ∷ Text
popupFixtureLua = luaLines
    [ "_G.__pans = {};"
    , "_G.__camX, _G.__camY = -999, -999;"
    , "camera.goToTile = function(x, y)"
    , "  table.insert(_G.__pans, {x = x, y = y});"
    , "  _G.__camX, _G.__camY = x, y end;"
    , "camera.getPosition = function() return _G.__camX, _G.__camY end;"
    , "package.loaded['scripts.hud'] ="
    , "  { getToolbarRects = function() return {} end };"
    , "assert(engine.loadScript('scripts/popup.lua', 3600.0) ~= nil,"
    , "  'production loadScript path failed');"
    , "local p = require('scripts.popup');"
    , "p.bootstrap(1, 2, 3, 1280, 720);"
    , "_G.__clickLine = function(lineIdx)"
    , "  local handle = nil;"
    , "  for h, e in pairs(p.lineByClickBox) do"
    , "    if e.lineIdx == lineIdx then handle = h end end;"
    , "  assert(handle, 'no click box for popup line ' .. tostring(lineIdx));"
    , "  assert(p.onLineClick(handle), 'popup line click was not handled');"
    , "  return 'ok' end;"
    , "return 'ok'"
    ]

-- | Fold two coordinate-carrying events into ONE popup line, each with
--   its own page. @combat@ is the fixture category because its
--   @coalesce_window@ is 2 s — long enough that two back-to-back
--   deliveries reliably fold, which is the shape both the cycling case
--   and the mixed-page case need.
coalesceTwoLua ∷ Maybe Text → Maybe Text → Text
coalesceTwoLua pageA pageB = luaLines
    [ "local p = require('scripts.popup');"
    , "p.onShowPopup('combat', 'first',  1, 1, 1, 1, {x=1, y=1}, "
        <> luaPage pageA <> ");"
    , "p.onShowPopup('combat', 'second', 1, 1, 1, 1, {x=2, y=2}, "
        <> luaPage pageB <> ");"
    , "assert(#p.active == 1, 'expected one coalesced popup');"
    , "assert(#p.active[1].lines == 1,"
    , "  'expected both events to fold into one line');"
    , "return 'ok'"
    ]
  where
    luaPage Nothing   = "nil"
    luaPage (Just pg) = "'" <> pg <> "'"

-- | Move the camera somewhere no target sits, so the NEXT activation
--   takes the "camera moved -> restart the cycle" branch.
moveCameraAwayLua ∷ Text
moveCameraAwayLua = "_G.__camX, _G.__camY = -999, -999; return 'ok'"

-----------------------------------------------------------
-- JSON shapes returned by the Lua probes
-----------------------------------------------------------

-- | One popup line, flattened: the camera calls made so far, the text
--   the ENGINE actually holds for the line's label, the line's stored
--   targets, its cycle position, and whether the popup is still open.
-- | Lua has one table type, so an EMPTY list serializes as @{}@ — a
--   JSON object, not an empty array. Every list field below therefore
--   goes through this wrapper, which accepts both shapes; without it a
--   case whose whole point is "no camera call happened" would fail to
--   decode rather than pass.
newtype LuaList α = LuaList { unLuaList ∷ [α] }

instance FromJSON α ⇒ FromJSON (LuaList α) where
    parseJSON (Object o) | null o = pure (LuaList [])
    parseJSON v                   = LuaList <$> parseJSON v

data LineProbe = LineProbe
    { lpPans         ∷ [PanProbe]
    , lpText         ∷ Text
    , lpTargets      ∷ [TargetProbe]
    , lpActivePopups ∷ Int
    , lpCycleIdx     ∷ Int
    } deriving Show

instance FromJSON LineProbe where
    parseJSON = withObject "LineProbe" $ \o → LineProbe
        <$> (unLuaList <$> o .: "pans")
        <*> o .: "text"
        <*> (unLuaList <$> o .: "targets")
        <*> o .: "activePopups"
        <*> o .: "cycleIdx"

data PanProbe = PanProbe { ppX ∷ Int, ppY ∷ Int } deriving (Show, Eq)

instance FromJSON PanProbe where
    parseJSON = withObject "PanProbe" $ \o →
        PanProbe <$> o .: "x" <*> o .: "y"

data TargetProbe = TargetProbe
    { tpX ∷ Int, tpY ∷ Int, tpPage ∷ Maybe Text } deriving (Show, Eq)

instance FromJSON TargetProbe where
    parseJSON = withObject "TargetProbe" $ \o →
        TargetProbe <$> o .: "x" <*> o .: "y" <*> o .:? "page"

data LogRowProbe = LogRowProbe
    { lrCoords ∷ Maybe PanProbe
    , lrPage   ∷ Maybe Text
    , lrText   ∷ Text
    } deriving Show

instance FromJSON LogRowProbe where
    parseJSON = withObject "LogRowProbe" $ \o → LogRowProbe
        <$> o .:? "coords" <*> o .:? "page" <*> o .: "text"

-- | Read line @lineIdx@ of the single active popup.
--
--   The text comes from @UI.getVisibleElements()@ rather than from a
--   Lua-side copy: @scripts\/ui\/label.lua@ names its text element
--   @<label>_text@, so this is what the engine would actually draw, and
--   the unavailable marker has to have survived a real re-render to
--   appear in it.
--
--   Lua's array-vs-object ambiguity: an empty @pans@\/@targets@ table
--   serializes as @{}@, which decodes as an empty JSON OBJECT, not an
--   empty array. Both are given an explicit @n@-keyed rebuild through
--   an array constructor so the Haskell side always sees a list.
lineProbeLua ∷ Int → Text
lineProbeLua lineIdx = luaLines
    [ "local p = require('scripts.popup');"
    , "local pop = p.active[1];"
    , "local line = pop and pop.lines[" <> tshow lineIdx <> "];"
    , "local targets = {};"
    , "if line then for i, t in ipairs(line.coords) do"
    , "  targets[i] = { x = t.x, y = t.y, page = t.page } end end;"
    , "local pans = {};"
    , "for i, c in ipairs(_G.__pans) do pans[i] = { x = c.x, y = c.y } end;"
    , "local text = '';"
    , "local want = '^popup_line_lbl_%d+_" <> tshow lineIdx <> "_text$';"
    , "for _, e in ipairs(UI.getVisibleElements()) do"
    , "  if e.name and e.name:match(want) then text = e.text or '' end end;"
    , "return { pans = pans, targets = targets, text = text,"
    , "         cycleIdx = (line and line.cycleIdx) or -1,"
    , "         activePopups = #p.active }"
    ]

-----------------------------------------------------------
-- Spec
-----------------------------------------------------------

spec ∷ Spec
spec = do
    attributionSpec
    activationSpec

-- | Requirement 1, plus its interaction with #780's discovery emitter:
--   attribution happens once, centrally, for every emitter.
attributionSpec ∷ Spec
attributionSpec = around withHeadlessEngine $
  describe "player-event page attribution" $ do

    it "attributes a coords-carrying emit to the active page, identically \
       \on the ring and the whole Lua message" $ \env → do
        installPages env [alphaPage]
        _ ← drainShowPopups env      -- discard anything boot queued
        cfg ← liveCategoryCfg env "location_discovery"
        emitEventAt env "location_discovery" "Test" "Discovered: ruin"
            (Just (7, 9))

        stored ← newestEvent env
        peCoords stored `shouldBe` Just (7, 9)
        peSourcePage stored `shouldBe` Just alphaPage

        -- Requirement 8: both surfaces agree because 'resolveEventPage'
        -- ran exactly once, before either was written. The message is
        -- pinned WHOLE -- category, text, RGBA and coords as well as
        -- the page -- because since #2285 it is the only delivery
        -- surface a popup has, so a field dropped or transposed on the
        -- way out has nothing else left to catch it.
        drainShowPopups env ≫= (`shouldBe`
            [ DeliveredPopup
                { dpCategory = "location_discovery"
                , dpText     = "Discovered: ruin"
                , dpColor    = ccTextColor cfg
                , dpCoords   = Just (7, 9)
                , dpPage     = Just alphaPage
                } ])

    it "delivers a popup with logging off, storing no event row" $ \env → do
        -- 'ccLog' and 'ccPopup' are separate switches, so a player who
        -- silenced this category's log rows must still see its toast.
        -- With the write-only engine queue gone (#2285) the message is
        -- the whole of that delivery: if the emit path ever gated the
        -- popup on the store write, this is the case that fails.
        installPages env [alphaPage]
        _ ← drainShowPopups env
        setCategorySwitches env "location_discovery" False True
        cfg ← liveCategoryCfg env "location_discovery"
        ccLog cfg `shouldBe` False
        before ← eventRowCount env

        emitEventAt env "location_discovery" "Test" "Discovered: shrine"
            (Just (2, 3))

        eventRowCount env ≫= (`shouldBe` before)
        drainShowPopups env ≫= (`shouldBe`
            [ DeliveredPopup
                { dpCategory = "location_discovery"
                , dpText     = "Discovered: shrine"
                , dpColor    = ccTextColor cfg
                , dpCoords   = Just (2, 3)
                , dpPage     = Just alphaPage
                } ])

    it "follows the visible page, not the head of wmWorlds" $ \env → do
        -- 'installPages' lists alpha FIRST both times; only visibility
        -- differs. An implementation that grabbed the head of wmWorlds
        -- would pass the case above and fail this one — the exact class
        -- of bug epic #101 was about.
        installPages env [betaPage]
        emitEventAt env "location_discovery" "Test" "Discovered: shrine"
            (Just (1, 1))
        stored ← newestEvent env
        peSourcePage stored `shouldBe` Just betaPage

    it "records no page for a coords-free emit" $ \env → do
        installPages env [alphaPage]
        emitEvent env "location_discovery" "Test" "Saved"
        stored ← newestEvent env
        peCoords stored `shouldBe` Nothing
        -- An event with no location must not become pannable merely
        -- because a world happens to be active.
        peSourcePage stored `shouldBe` Nothing

    it "lets an explicit emitter page win over the active page" $ \env → do
        installPages env [alphaPage]
        emitEventFullOnPage env "location_discovery" "World.Thread.Discovery"
            "Discovered: cave" (Just (4, 5)) (Just 12) (Just betaPage)
        stored ← newestEvent env
        peCoords stored `shouldBe` Just (4, 5)
        peSourcePage stored `shouldBe` Just betaPage

    it "preserves #780's page-without-coords discovery case" $ \env → do
        installPages env [alphaPage]
        emitEventFullOnPage env "location_discovery" "World.Thread.Discovery"
            "Discovered: hidden vault" Nothing (Just 12) (Just betaPage)
        stored ← newestEvent env
        peCoords stored `shouldBe` Nothing
        peSourcePage stored `shouldBe` Just betaPage

    it "records no page for a coords emit with no world registered" $ \env → do
        -- The main menu. Requirement 6's other half: absence is carried
        -- honestly rather than papered over with an active page there
        -- isn't one of.
        installNoPages env
        emitEventAt env "location_discovery" "Test" "Discovered: nowhere"
            (Just (3, 3))
        stored ← newestEvent env
        peCoords stored `shouldBe` Just (3, 3)
        peSourcePage stored `shouldBe` Nothing

    it "exposes coords and page to Lua through engine.getEventLog()" $ \env → do
        installPages env [alphaPage]
        ls ← newBareLuaBackend env
        emitEventAt env "location_discovery" "Test" "with coords" (Just (7, 9))
        emitEventFullOnPage env "location_discovery" "World.Thread.Discovery"
            "page only" Nothing Nothing (Just betaPage)
        rows ← evalJSON ls (luaLines
            [ "local log = engine.getEventLog();"
            , "local out = {};"
            , "for i = #log - 1, #log do"
            , "  local e = log[i];"
            , "  table.insert(out, { text = e.text, page = e.page,"
            , "                      coords = e.coords })"
            , "end;"
            , "return out"
            ]) ∷ IO [LogRowProbe]
        map lrText rows   `shouldBe` ["with coords", "page only"]
        map lrPage rows   `shouldBe` [Just alphaPage, Just betaPage]
        map lrCoords rows `shouldBe` [Just (PanProbe 7 9), Nothing]

-- | Requirements 2-6: the activation decision itself, on a bare Lua
--   backend.
activationSpec ∷ Spec
activationSpec = around withHeadlessEngine $
  describe "popup coordinate activation" $ do

    it "pans when the recorded page is the active one, keeping the live \
       \broadcast's page on the stored target" $ \env → do
        installPages env [alphaPage]
        ls ← newBareLuaBackend env
        evalStep ls popupFixtureLua
        deliverPopup env ls (Just (7, 9)) (Just alphaPage)
        evalStep ls "return _G.__clickLine(1)"
        probe ← evalJSON ls (lineProbeLua 1) ∷ IO LineProbe
        -- Requirement 2: the page survived the LIVE delivery path.
        lpTargets probe `shouldBe` [TargetProbe 7 9 (Just alphaPage)]
        -- Requirement 5: it pans, exactly as it did before this change.
        lpPans probe `shouldBe` [PanProbe 7 9]
        lpText probe `shouldNotSatisfy` T.isInfixOf unavailableMarker

    it "refuses a different page — no camera call, cycle untouched, popup \
       \still open — and works again once that page is active" $ \env → do
        installPages env [alphaPage]
        ls ← newBareLuaBackend env
        evalStep ls popupFixtureLua
        deliverPopup env ls (Just (7, 9)) (Just betaPage)

        evalStep ls "return _G.__clickLine(1)"
        refused ← evalJSON ls (lineProbeLua 1) ∷ IO LineProbe
        lpPans refused         `shouldBe` []     -- requirement 4
        lpCycleIdx refused     `shouldBe` 0      -- cycle not advanced
        lpActivePopups refused `shouldBe` 1      -- popup still open
        lpText refused `shouldSatisfy` T.isInfixOf unavailableMarker

        -- A second refusal stays a refusal, and does not stack a second
        -- marker onto the line.
        evalStep ls "return _G.__clickLine(1)"
        again ← evalJSON ls (lineProbeLua 1) ∷ IO LineProbe
        lpPans again `shouldBe` []
        occurrences unavailableMarker (lpText again) `shouldBe` 1

        -- Return to the recorded page: the very same activation now
        -- succeeds, and the marker clears.
        installPages env [betaPage]
        evalStep ls "return _G.__clickLine(1)"
        recovered ← evalJSON ls (lineProbeLua 1) ∷ IO LineProbe
        lpPans recovered     `shouldBe` [PanProbe 7 9]
        lpCycleIdx recovered `shouldBe` 1
        lpText recovered `shouldNotSatisfy` T.isInfixOf unavailableMarker

    it "refuses a page-less entry, including a legacy seven-argument \
       \onShowPopup call" $ \env → do
        installPages env [alphaPage]
        ls ← newBareLuaBackend env
        evalStep ls popupFixtureLua
        -- The compatibility clause: a call written against the old
        -- signature still WORKS, and its coordinate is non-panning
        -- rather than aimed at whichever page is active.
        evalStep ls $ luaLines
            [ "local p = require('scripts.popup');"
            , "p.onShowPopup('location_discovery', 'legacy',"
            , "              0.9, 0.8, 0.3, 1.0, {x = 2, y = 3});"
            , "return 'ok'"
            ]
        evalStep ls "return _G.__clickLine(1)"
        probe ← evalJSON ls (lineProbeLua 1) ∷ IO LineProbe
        lpTargets probe      `shouldBe` [TargetProbe 2 3 Nothing]
        lpPans probe         `shouldBe` []
        lpActivePopups probe `shouldBe` 1
        lpText probe `shouldSatisfy` T.isInfixOf unavailableMarker

    it "keeps same-page multi-coordinate cycling and camera-moved restart \
       \exactly as they were" $ \env → do
        installPages env [alphaPage]
        ls ← newBareLuaBackend env
        evalStep ls popupFixtureLua
        evalStep ls (coalesceTwoLua (Just alphaPage) (Just alphaPage))

        evalStep ls "return _G.__clickLine(1)"
        evalStep ls "return _G.__clickLine(1)"
        evalStep ls "return _G.__clickLine(1)"
        cycled ← evalJSON ls (lineProbeLua 1) ∷ IO LineProbe
        -- 1 -> 2 -> back to 1: the camera sits where the previous pan
        -- left it each time, so each click advances modulo.
        lpPans cycled `shouldBe` [PanProbe 1 1, PanProbe 2 2, PanProbe 1 1]

        evalStep ls moveCameraAwayLua
        evalStep ls "return _G.__clickLine(1)"
        restarted ← evalJSON ls (lineProbeLua 1) ∷ IO LineProbe
        -- Camera moved -> restart at coordinate one, not continue.
        lpPans restarted `shouldBe`
            [PanProbe 1 1, PanProbe 2 2, PanProbe 1 1, PanProbe 1 1]

    it "keeps each coordinate's own page on a coalesced, mixed-page line" $
      \env → do
        installPages env [alphaPage]
        ls ← newBareLuaBackend env
        evalStep ls popupFixtureLua
        evalStep ls (coalesceTwoLua (Just alphaPage) (Just betaPage))

        start ← evalJSON ls (lineProbeLua 1) ∷ IO LineProbe
        -- One line, two targets, two DIFFERENT pages: a single page
        -- stamped on the LINE would have to be wrong for one of them.
        lpTargets start `shouldBe`
            [ TargetProbe 1 1 (Just alphaPage)
            , TargetProbe 2 2 (Just betaPage) ]

        evalStep ls "return _G.__clickLine(1)"
        evalStep ls "return _G.__clickLine(1)"
        mixed ← evalJSON ls (lineProbeLua 1) ∷ IO LineProbe
        -- The alpha coordinate pans; the beta one in the SAME line does
        -- not, and leaves the cycle parked on the alpha coordinate.
        lpPans mixed     `shouldBe` [PanProbe 1 1]
        lpCycleIdx mixed `shouldBe` 1
        lpText mixed `shouldSatisfy` T.isInfixOf unavailableMarker

        installPages env [betaPage]
        evalStep ls "return _G.__clickLine(1)"
        afterSwitch ← evalJSON ls (lineProbeLua 1) ∷ IO LineProbe
        lpPans afterSwitch     `shouldBe` [PanProbe 1 1, PanProbe 2 2]
        lpCycleIdx afterSwitch `shouldBe` 2

    it "replays an event-log row with the stored page, producing the same \
       \target live delivery does" $ \env → do
        installPages env [alphaPage]
        ls ← newBareLuaBackend env
        evalStep ls popupFixtureLua

        -- A real ring entry, through the real emit path.
        emitEventAt env "location_discovery" "Test" "Discovered: ruin"
            (Just (7, 9))

        -- Requirement 3: drive the REAL row-click path — event_log's own
        -- rendered row overlay, resolved by name, not a hand-built call.
        evalStep ls $ luaLines
            [ "local el = require('scripts.event_log');"
            , "el.bootstrap(1, 2, 3, 1280, 720);"
            , "el.show();"
            , "local handle = nil;"
            , "for _, e in ipairs(UI.getVisibleElements()) do"
            , "  if e.name == 'evlog_row_click_1' then handle = e.handle end"
            , "end;"
            , "assert(handle, 'no event-log row overlay was rendered');"
            , "assert(el.onRowClick(handle), 'row click was not handled');"
            , "return 'ok'"
            ]
        replayed ← evalJSON ls (lineProbeLua 1) ∷ IO LineProbe
        lpTargets replayed `shouldBe` [TargetProbe 7 9 (Just alphaPage)]

        -- ...and the same coordinate delivered LIVE produces an
        -- identical target. Requirement 8: the two routes cannot drop
        -- different metadata.
        evalStep ls $ luaLines
            [ "require('scripts.popup').dismissAll();"
            , "return 'ok'"
            ]
        deliverPopup env ls (Just (7, 9)) (Just alphaPage)
        live ← evalJSON ls (lineProbeLua 1) ∷ IO LineProbe
        lpTargets live `shouldBe` lpTargets replayed

        evalStep ls "return _G.__clickLine(1)"
        panned ← evalJSON ls (lineProbeLua 1) ∷ IO LineProbe
        lpPans panned `shouldBe` [PanProbe 7 9]
