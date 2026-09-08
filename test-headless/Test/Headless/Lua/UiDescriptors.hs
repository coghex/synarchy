-- | #2479: the declarative registration contract, held against the
--   namespace it was piloted on.
--
--   @tools/lua_registration_audit.py@ certifies that every descriptor
--   registration yields a verb NAME attributed to a namespace, and
--   nothing more: it never runs the engine, so it cannot tell whether a
--   descriptor's arity, argument kinds or return shape describe the
--   action sitting beside it. The existing UI suites cannot either —
--   they exercise behaviour, and inaccurate metadata changes no
--   behaviour at all. This module is where the metadata is confronted
--   with reality:
--
--     * the live @UI@ table carries exactly the descriptors' names, all
--       of them functions, and nothing else;
--     * every descriptor is structurally well-formed;
--     * representative verbs really push what their descriptor says
--       they push — zero results as against one nil result, ordered
--       multiple results, nullable scalars, nested nullable records and
--       arrays of records;
--     * both registrars share one exception guard, with the three
--       guarantees "Engine.Scripting.Lua.API.Internal" documents and
--       @docs/engine_contracts.md@ §Local-config writes relies on.
--
--   The descriptor set comes from 'installUIAPI' — the very expression
--   that registers the verbs — so nothing here can pass by describing a
--   second, hand-maintained list of names. The expectations are derived
--   from each descriptor rather than restated beside it, so weakening a
--   descriptor moves the expectation with it and only a descriptor that
--   disagrees with its own action can fail.
module Test.Headless.Lua.UiDescriptors (spec) where

import UPrelude
import Test.Hspec
import Control.Exception (SomeAsyncException(..), AsyncException(UserInterrupt)
                         , throwIO, try)
import Data.IORef (newIORef, writeIORef)
import Data.List (sort)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified HsLua as Lua
import Engine.Core.State (EngineEnv(..))
import Engine.Core.Capability.RenderView
    (RenderViewCapability(..), toRenderViewCapability)
import Engine.Core.Thread (ThreadControl(..))
import Engine.Scripting.Lua.API (registerLuaAPI)
import Engine.Scripting.Lua.API.Descriptor
import Engine.Scripting.Lua.API.Internal (registerLuaVerb, guardLuaAction)
import Engine.Scripting.Lua.API.Register.UI (installUIAPI)
import Engine.Scripting.Lua.Thread (createLuaBackendState)
import Engine.Scripting.Lua.Thread.Console (executeDebugLua)
import Engine.Scripting.Lua.Types (LuaBackendState(..))
import Test.Headless.Harness (withHeadlessEngineNoWorld)
import Test.Headless.Harness.Isolation (withIsolatedResourceRoot)

spec ∷ Spec
spec = around withDescriptorEngine $ do
    describe "UI descriptor registration boundary (#2479)" $ do
        it "installs exactly the descriptors' names, every one a function" $ \env → do
            ds ← uiDescriptors env
            ls ← newBareLuaBackend env
            members ← liveUiMembers ls
            members `shouldBe` sort (map verbLabel ds)

        it "names every verb exactly once, and names at least one" $ \env → do
            ds ← uiDescriptors env
            let names = map verbName ds
            ds `shouldNotSatisfy` null
            sort names `shouldBe` dedup (sort names)

        it "carries a structurally well-formed descriptor for every verb" $ \env → do
            ds ← uiDescriptors env
            [ (verbName d, m) | d ← ds, m ← verbMalformations d ] `shouldBe` []

    describe "UI descriptors against live results (#2479)" $ do
        it "distinguishes zero results from one nil result" $ \env → do
            (ls, _) ← newFixture env
            -- A ReturnsNothing verb: the console reports a chunk that
            -- pushed nothing as "ok".
            evalDebug ls "UI.setPosition(_G.__box, 11, 21)" ≫= (`shouldBe` "ok")
            -- A one-nullable-result verb on an element with no buffer:
            -- one value, and that value is nil.
            evalDebug ls "return UI.getTextInput(_G.__box)" ≫= (`shouldBe` "null")
            evalDebug ls "return select('#', UI.getTextInput(_G.__box))"
                ≫= (`shouldBe` "1")

        it "pushes placePopup's three bare values in descriptor order" $ \env → do
            (ls, ds) ← newFixture env
            checkShape ls ds "placePopup" placePopupBelow
            -- Order is the contract, and a type check cannot carry it:
            -- x and y are both TNumber, so a swap of the descriptor's
            -- first two results — or of the action's first two pushes —
            -- would read as "number number boolean" either way. Pin the
            -- VALUES instead, over a framebuffer this test fixes so the
            -- arithmetic is exact and x is distinguishable from y.
            --
            -- Anchor (10, 20) sized 30x40, content 50x60, 800x600
            -- framebuffer: 'below' is y = 20 + 40 = 60, which fits, so
            -- UI.PopupPlacement leaves x at the anchor's 10 and neither
            -- clamp binds. x = 10, y = 60, flipped = false.
            setFramebuffer env (800, 600)
            checkNamedResults ls ds "placePopup" placePopupBelow
                [("x", "10"), ("y", "60"), ("flipped", "false")]
            -- The same request against the bottom edge cannot open
            -- below (560 + 40 + 60 > 600), so it flips above to
            -- 560 - 60 = 500 and reports it. This is what pins the
            -- third result as the flip flag rather than as "whatever
            -- boolean happened to be pushed last".
            checkNamedResults ls ds "placePopup" placePopupFlipped
                [("x", "10"), ("y", "500"), ("flipped", "true")]
            -- The direction argument really is optional, and omitting it
            -- changes neither the count nor the values' positions.
            -- Anchored placement ignores the anchor's size and clamps
            -- only, so the pair stays asymmetric: x = 10, y = 20.
            checkShape ls ds "placePopup" "UI.placePopup(10, 20, 30, 40, 50, 60)"
            checkNamedResults ls ds "placePopup" "UI.placePopup(10, 20, 30, 40, 50, 60)"
                [("x", "10"), ("y", "20"), ("flipped", "false")]

        it "pushes findHoverTarget's two nullable values, together" $ \env → do
            (ls, ds) ← newFixture env
            checkShape ls ds "findHoverTarget" "UI.findHoverTarget(-1000, -1000)"
            evalDebug ls "return UI.findHoverTarget(-1000, -1000)"
                ≫= (`shouldBe` "null\tnull")
            checkShape ls ds "findHoverTarget" "UI.findHoverTarget(60, 40)"
            hit ← evalDebug ls (T.concat
                [ "local h, cb = UI.findHoverTarget(60, 40) "
                , "return type(h) .. ' ' .. type(cb)" ])
            unquoted hit `shouldBe` "number string"
            -- The descriptor calls this the left-click AFFORDANCE, not
            -- the active target: findClickableAncestor tests ueOnClick
            -- alone, so revoking `clickable` leaves the hover target
            -- reported while getElementInfo's leftClickTarget goes
            -- false. Restating it as "the target a click would activate"
            -- fails here.
            disabled ← evalDebug ls (T.concat
                [ "UI.setClickable(_G.__box, false) "
                , "local h, cb = UI.findHoverTarget(60, 40) "
                , "local info = UI.getElementInfo(_G.__box) "
                , "return (h == _G.__box) and (type(cb) == 'string') "
                , "and (info.leftClickAffordance == true) "
                , "and (info.leftClickTarget == false)" ])
            disabled `shouldBe` "true"

        it "pushes getElementInfo's nullable record with every documented field" $ \env → do
            (ls, ds) ← newFixture env
            checkShape ls ds "getElementInfo" "UI.getElementInfo(_G.__child)"
            checkShape ls ds "getElementInfo" "UI.getElementInfo(999999)"
            evalDebug ls "return UI.getElementInfo(999999)" ≫= (`shouldBe` "null")
            -- The nested nullable record is a table for a clipped
            -- element and nil for an unclipped one, so the descriptor's
            -- TNullable (TRecord …) is exercised both ways.
            clipped ← evalDebug ls
                "return type(UI.getElementInfo(_G.__child).effectiveClip)"
            unquoted clipped `shouldBe` "table"
            evalDebug ls "return UI.getElementInfo(_G.__box).effectiveClip"
                ≫= (`shouldBe` "null")

        it "pushes getVisibleElements as an array of those same records" $ \env → do
            (ls, ds) ← newFixture env
            checkShape ls ds "getVisibleElements" "UI.getVisibleElements()"
            -- An empty array would satisfy the shape check vacuously.
            evalDebug ls "return #UI.getVisibleElements() >= 2" ≫= (`shouldBe` "true")

        it "pushes getEffectiveClip's nullable rect both ways" $ \env → do
            (ls, ds) ← newFixture env
            checkShape ls ds "getEffectiveClip" "UI.getEffectiveClip(_G.__child)"
            checkShape ls ds "getEffectiveClip" "UI.getEffectiveClip(_G.__box)"
            clipped ← evalDebug ls "return type(UI.getEffectiveClip(_G.__child))"
            unquoted clipped `shouldBe` "table"
            evalDebug ls "return UI.getEffectiveClip(_G.__box)" ≫= (`shouldBe` "null")

        it "pushes the nullable scalars and plain values it describes" $ \env → do
            (ls, ds) ← newFixture env
            checkShape ls ds "getTextInput" "UI.getTextInput(_G.__child)"
            checkShape ls ds "getTextInput" "UI.getTextInput(_G.__box)"
            checkShape ls ds "getCursor" "UI.getCursor(_G.__child)"
            checkShape ls ds "getElementOnClick" "UI.getElementOnClick(_G.__box)"
            checkShape ls ds "getFocus" "UI.getFocus()"
            checkShape ls ds "isPageVisible" "UI.isPageVisible(_G.__page)"
            checkShape ls ds "isInputBlocked" "UI.isInputBlocked()"
            checkShape ls ds "armPresentation" "UI.armPresentation()"
            checkShape ls ds "isPresented" "UI.isPresented(1)"
            checkShape ls ds "fitVisibleRows" "UI.fitVisibleRows(10, 12, 100)"
            checkShape ls ds "newPage" "UI.newPage('shape_probe', 'hud')"
            checkShape ls ds "loadBoxTextures"
                "UI.loadBoxTextures(1, 1, 1, 1, 1, 1, 1, 1, 1)"

        it "documents Lua.toboolean's coercion the way it really behaves" $ \env → do
            (ls, _) ← newFixture env
            -- flagArg's wording covers eleven descriptors, so pin what
            -- it claims: an explicit false is false (it is not "any
            -- value but nil"), an omitted argument is false, and 0 —
            -- truthy in Lua, unlike in C — is true.
            coercion ← evalDebug ls (T.concat
                [ "UI.setClipChildren(_G.__box, false) "
                , "local explicitFalse = UI.isClipChildren(_G.__box) "
                , "UI.setClipChildren(_G.__box, 0) "
                , "local zero = UI.isClipChildren(_G.__box) "
                , "UI.setClipChildren(_G.__box) "
                , "local omitted = UI.isClipChildren(_G.__box) "
                , "return (explicitFalse == false) and (zero == true) "
                , "and (omitted == false)" ])
            coercion `shouldBe` "true"

        it "describes scroll capture as the raw opt-in it really is" $ \env → do
            (ls, _) ← newFixture env
            -- The box carries an onClick. Pointer blocking is derived —
            -- a clickable element with a callback blocks whether or not
            -- it opted in — but scroll capture is not: #743 keeps the
            -- two policies independent, so the callback buys nothing
            -- here. Describing scrollCapturing as "effective, not just
            -- the raw opt-in" fails this.
            independent ← evalDebug ls (T.concat
                [ "local blocking = UI.isPointerBlocking(_G.__box) "
                , "local capturing = UI.isScrollCapturing(_G.__box) "
                , "UI.setScrollCapture(_G.__box, true) "
                , "local optedIn = UI.isScrollCapturing(_G.__box) "
                , "return (blocking == true) and (capturing == false) "
                , "and (optedIn == true)" ])
            independent `shouldBe` "true"

        it "describes page scope as requiring visibility" $ \env → do
            (ls, ds) ← newFixture env
            checkShape ls ds "isPageInScope" "UI.isPageInScope(_G.__page)"
            -- pagesInScope is drawn from getVisiblePages, so a hidden
            -- page is out of scope even with no modal boundary anywhere.
            -- "in scope whenever there is no boundary" fails this.
            scoped ← evalDebug ls (T.concat
                [ "local blocked = UI.isInputBlocked() "
                , "local shown = UI.isPageInScope(_G.__page) "
                , "UI.hidePage(_G.__page) "
                , "local hidden = UI.isPageInScope(_G.__page) "
                , "UI.showPage(_G.__page) "
                , "return (blocked == false) and (shown == true) "
                , "and (hidden == false)" ])
            scoped `shouldBe` "true"

        it "describes control focus as the unvalidated handle it really is" $ \env → do
            (ls, ds) ← newFixture env
            checkShape ls ds "getControlFocus" "UI.getControlFocus()"
            checkShape ls ds "hasControlFocus" "UI.hasControlFocus(999999)"
            -- UI.Manager.Focus.setControlFocus stores whatever integer
            -- it is handed; nothing validates the element until keyboard
            -- dispatch does. The descriptors say so, and this is why.
            unvalidated ← evalDebug ls (T.concat
                [ "UI.setControlFocus(999999) "
                , "return (UI.getControlFocus() == 999999) "
                , "and (UI.hasControlFocus(999999) == true)" ])
            unvalidated `shouldBe` "true"
            -- Text focus is the opposite — setElementFocus looks the
            -- handle up and ignores an unknown one — which is what keeps
            -- hasFocus's "false for an unknown handle" wording honest.
            validated ← evalDebug ls (T.concat
                [ "UI.setFocus(999999) "
                , "return (UI.getFocus() == nil) "
                , "and (UI.hasFocus(999999) == false)" ])
            validated `shouldBe` "true"

        it "pushes nothing from the setters that describe no results" $ \env → do
            (ls, ds) ← newFixture env
            checkShape ls ds "setVisible" "UI.setVisible(_G.__box, true)"
            checkShape ls ds "setZIndex" "UI.setZIndex(_G.__box, 3)"
            checkShape ls ds "setText" "UI.setText(_G.__child, 'other')"
            checkShape ls ds "clearFocus" "UI.clearFocus()"
            checkShape ls ds "setTooltip" "UI.setTooltip(_G.__box, 'tip')"
            checkShape ls ds "setTooltipStyle" "UI.setTooltipStyle({padding = 4})"

    describe "the registrars' shared exception guard (#2479)" $ do
        it "converts an ordinary Haskell exception into a Lua error" $ \_ → do
            (results, message) ← Lua.run $ do
                n ← guardLuaAction "boom" (Lua.liftIO (throwIO (userError "kaboom")))
                m ← Lua.tostring (-1)
                pure (n, TE.decodeUtf8Lenient (fromMaybe "" m))
            -- hslua signals an error from a Haskell function by leaving
            -- two results — its private HSLUA_ERR sentinel and the
            -- message — which @hslua_call_hs@ then re-raises as a Lua
            -- error. Two results with the message on top is that
            -- protocol, not an ordinary return.
            results `shouldBe` 2
            message `shouldSatisfy` T.isInfixOf "Haskell exception in boom"
            message `shouldSatisfy` T.isInfixOf "kaboom"

        it "re-throws a Lua exception for hslua's own conversion" $ \_ → do
            thrown ← try $ Lua.run
                (guardLuaAction "boom" (Lua.failLua "explicit lua error"))
            case thrown of
                Left e  → T.pack (show (e ∷ Lua.Exception))
                    `shouldSatisfy` T.isInfixOf "explicit lua error"
                Right n → expectationFailure
                    ("the guard swallowed a Lua exception, returning " <> show n)

        it "lets an asynchronous exception escape so killThread still works" $ \_ → do
            thrown ← try $ Lua.run $ guardLuaAction "boom"
                (Lua.liftIO (throwIO (SomeAsyncException UserInterrupt)))
            case thrown of
                Left (SomeAsyncException _) → pure ()
                Right n → expectationFailure
                    ("the guard swallowed an async exception, returning " <> show n)

        it "applies that guard to a descriptor-registered verb too" $ \env → do
            ls ← newBareLuaBackend env
            installGuardProbe ls
            evalDebug ls "return guardProbe.fine()" ≫= (`shouldBe` "true")
            evalDebug ls (T.concat
                [ "local ok, err = pcall(guardProbe.boom) "
                , "return (not ok) and "
                , "(string.find(err, 'Haskell exception in boom', 1, true) ~= nil)" ])
                ≫= (`shouldBe` "true")
            evalDebug ls (T.concat
                [ "local ok, err = pcall(guardProbe.luaBoom) "
                , "return (not ok) and "
                , "(string.find(err, 'explicit lua error', 1, true) ~= nil)" ])
                ≫= (`shouldBe` "true")

-- * Engine and Lua fixtures

-- | Engine init is itself a @config/@ writer, so isolation is
--   established around it, never inside (#1357). No world page is
--   needed, so the world worker never has to start.
withDescriptorEngine ∷ (EngineEnv → IO α) → IO α
withDescriptorEngine action = withIsolatedResourceRoot (withHeadlessEngineNoWorld action)

-- | A real Lua backend carrying the full production API surface, the
--   same device-free pattern 'Test.Headless.UI.BarFillColor' uses. The
--   @UI@ table under test is the one 'registerLuaAPI' installed, not a
--   copy made here.
newBareLuaBackend ∷ EngineEnv → IO LuaBackendState
newBareLuaBackend env = do
    ls ← createLuaBackendState (luaToEngineQueue env) (luaQueue env)
                                (assetPoolRef env) (nextObjectIdRef env)
                                (inputStateRef env) (loggerRef env)
    stateRef ← newIORef ThreadRunning
    registerLuaAPI (lbsLuaState ls) env ls stateRef
    pure ls

-- | The descriptors 'installUIAPI' installs, read from a throwaway Lua
--   state so the state under test is registered exactly once, by the
--   production path. The registrar needs no standard library: it only
--   builds a table and installs a global.
uiDescriptors ∷ EngineEnv → IO [LuaVerb]
uiDescriptors env = Lua.run (installUIAPI env)

-- | @"\<name\>/\<lua type\>"@ for one descriptor, the form
--   'liveUiMembers' reports the live table in.
verbLabel ∷ LuaVerb → Text
verbLabel d = TE.decodeUtf8Lenient (verbName d) <> "/function"

-- | Every member of the live @UI@ table, sorted, each tagged with its
--   Lua type. An extra member and a missing one are both visible.
liveUiMembers ∷ LuaBackendState → IO [Text]
liveUiMembers ls = do
    raw ← evalDebug ls (T.concat
        [ "local ks = {} "
        , "for k, v in pairs(UI) do ks[#ks+1] = k .. '/' .. type(v) end "
        , "table.sort(ks) "
        , "return table.concat(ks, ' ')" ])
    pure (T.words (unquoted raw))

-- | A shown page carrying a clipping box, a text child inside it, and a
--   click callback — enough for the nested-record, array-of-record and
--   non-nil hover cases to be real rather than vacuously nil. The
--   texture and font handles are synthetic: nothing here draws, and
--   @UI.newBox@/@UI.newText@ only store the handle.
newFixture ∷ EngineEnv → IO (LuaBackendState, [LuaVerb])
newFixture env = do
    ds ← uiDescriptors env
    ls ← newBareLuaBackend env
    setup ← evalDebug ls fixtureLua
    setup `shouldNotSatisfy` isLuaError
    pure (ls, ds)

-- | The placement requests the order check pins. Named so the shape
--   check and the value check cannot drift apart into two different
--   calls.
placePopupBelow, placePopupFlipped ∷ Text
placePopupBelow   = "UI.placePopup(10, 20, 30, 40, 50, 60, 'below')"
placePopupFlipped = "UI.placePopup(10, 560, 30, 40, 50, 60, 'below')"

-- | Fix the framebuffer @UI.placePopup@ reads. Without this the
--   placement depends on whatever size the headless engine happens to
--   hold, and no concrete result could be asserted.
setFramebuffer ∷ EngineEnv → (Int, Int) → IO ()
setFramebuffer env size =
    writeIORef (rvFramebufferSizeRef (toRenderViewCapability env)) size

fixtureLua ∷ Text
fixtureLua = T.concat
    [ "local page = UI.newPage('desc_page', 'hud') "
    , "UI.showPage(page) "
    , "local box = UI.newBox('desc_box', 100, 40, 1, 4, 1, 1, 1, 1, 0, page) "
    , "UI.addToPage(page, box, 10, 20) "
    , "UI.setClipChildren(box, true) "
    , "UI.setClickable(box, true) "
    , "UI.setOnClick(box, 'descClick') "
    , "local child = UI.newText('desc_child', 'hello', 1, 12, 1, 1, 1, 1, page) "
    , "UI.addChild(box, child, 2, 3) "
    , "UI.enableTextInput(child) "
    , "UI.setTextInput(child, 'abc') "
    , "_G.__page = page _G.__box = box _G.__child = child "
    , "return true"
    ]

-- | Install a scratch namespace through the descriptor registrar, one
--   verb per exception outcome the guard must produce. It is built here
--   rather than in @src/@ deliberately: the registration audit's
--   registrar glob covers @Register/*.hs@ only, so this probe adds
--   nothing to the certified surface.
installGuardProbe ∷ LuaBackendState → IO ()
installGuardProbe ls = Lua.runWith (lbsLuaState ls) $ do
    Lua.newtable
    _ ← registerLuaVerb
        (luaVerb "fine" [] (retVals [resVal "ok" TBoolean "Always true."])
            "Returns without raising anything.")
        (Lua.pushboolean True ≫ pure 1)
    _ ← registerLuaVerb
        (luaVerb "boom" [] retNone "Raises an ordinary Haskell exception.")
        (Lua.liftIO (throwIO (userError "kaboom")))
    _ ← registerLuaVerb
        (luaVerb "luaBoom" [] retNone "Raises a Lua exception.")
        (Lua.failLua "explicit lua error")
    Lua.setglobal (Lua.Name "guardProbe")

-- * Descriptor-driven assertions

-- | Hold one live call against its descriptor: the number of values it
--   pushes, and each value's Lua type, both read off the descriptor
--   rather than restated here. A nullable result may be nil; a record
--   must carry every field the descriptor names, at the kind it names;
--   every element of an array must match its element type.
checkShape ∷ LuaBackendState → [LuaVerb] → Text → Text → Expectation
checkShape ls ds name call = case [ d | d ← ds, verbName d ≡ TE.encodeUtf8 name ] of
    []      → expectationFailure ("no UI descriptor named " <> T.unpack name)
    (d : _) → do
        answer ← evalDebug ls (returnShapeChunk d call)
        answer `shouldBe` "true"

-- | Pin one call's actual RESULT VALUES to positions through the
--   descriptor's own result names.
--
--   A type check cannot express order when two results share a kind, and
--   a bare value assertion cannot either — it never consults the
--   descriptor, so reordering the descriptor alone would leave it
--   passing. Pairing expected values with names and letting the
--   DESCRIPTOR decide which position each name occupies closes both
--   sides: reorder the descriptor and position 1 starts expecting y's
--   value against x's actual; reorder the action's pushes and position 1
--   stops producing x's. Renaming a result fails too, since its name no
--   longer appears in the expectations.
checkNamedResults ∷ LuaBackendState → [LuaVerb] → Text → Text
                  → [(Text, Text)] → Expectation
checkNamedResults ls ds name call expected =
    case [ d | d ← ds, verbName d ≡ TE.encodeUtf8 name ] of
        []      → expectationFailure ("no UI descriptor named " <> T.unpack name)
        (d : _) → case verbReturns d of
            ReturnsNothing → expectationFailure
                (T.unpack name <> " describes no results, so it has no order to pin")
            ReturnsValues results
                | length results ≢ length expected → expectationFailure
                    (T.unpack name <> " describes " <> show (length results)
                        <> " results but " <> show (length expected)
                        <> " were expected")
                | [ r | r ← results, isNothing (lookup (resultName r) expected) ] ≢ []
                    → expectationFailure
                        (T.unpack name <> " describes a result this check does not "
                            <> "name: " <> show (map resultName results))
                | otherwise → do
                    let binds = [ "v" <> tshow i | i ← [1 .. length results] ]
                        checks =
                            [ "(" <> v <> " == " <> fromMaybe "" (lookup (resultName r) expected) <> ")"
                            | (v, r) ← zip binds results ]
                        chunk = T.intercalate "\n"
                            [ "local " <> T.intercalate ", " binds <> " = " <> call
                            , "return " <> T.intercalate " and " checks
                            ]
                    answer ← evalDebug ls chunk
                    -- Report the actual triple on failure, not just False.
                    if answer ≡ "true" then pure () else do
                        actual ← evalDebug ls ("return " <> call)
                        expectationFailure
                            (T.unpack name <> " " <> T.unpack call <> ": expected "
                                <> show [ (resultName r, lookup (resultName r) expected)
                                        | r ← results ]
                                <> " in that order, got " <> show actual)

-- | A Lua chunk answering @true@ when one call matches @verb@'s return
--   shape.
returnShapeChunk ∷ LuaVerb → Text → Text
returnShapeChunk verb call = T.intercalate "\n" $
    [ "local function allOf(t, f)"
    , "  for i = 1, #t do if not f(t[i]) then return false end end"
    , "  return true"
    , "end"
    , "local function onlyKeys(t, ks)"
    , "  local allowed = {}"
    , "  for _, k in ipairs(ks) do allowed[k] = true end"
    , "  for k in pairs(t) do if not allowed[k] then return false end end"
    , "  return true"
    , "end"
    , "local n = select('#', " <> call <> ")"
    ] <> binders <>
    [ "return n == " <> tshow arity <> checks ]
  where
    results = case verbReturns verb of
        ReturnsNothing     → []
        ReturnsValues vals → vals
    arity = returnArity (verbReturns verb)
    names = [ "v" <> tshow i | i ← [1 .. arity] ]
    binders
        | arity ≡ 0 = []
        | otherwise = ["local " <> T.intercalate ", " names <> " = " <> call]
    checks = T.concat
        [ " and (" <> typePredicate (resultType r) v <> ")"
        | (v, r) ← zip names results ]

-- | A Lua expression that is true when @expr@ holds a value of @ty@.
--
--   A record is checked in both directions: every field the descriptor
--   names must be there at the kind it names, AND the value must carry
--   no key the descriptor does not name. Without that second half a
--   descriptor could quietly stop describing a field and still pass,
--   which is exactly the drift this suite exists to catch.
typePredicate ∷ LuaType → Text → Text
typePredicate ty expr = case ty of
    TString     → "type(" <> expr <> ") == 'string'"
    -- Lua 5.4 keeps integer and float as distinct subtypes of `number`,
    -- and math.type reports which — so the two numeric kinds the
    -- descriptor distinguishes are checkable, and checkable BOTH ways.
    -- `type(v) == 'number'` alone would not be: it accepts a fractional
    -- value where TInteger is promised, and it also lets a descriptor
    -- weaken TInteger to TNumber unnoticed, since an integer satisfies
    -- the looser claim. Pushing an integer is Lua.pushinteger and
    -- pushing a float is Lua.pushnumber, which is exactly the
    -- distinction Descriptor's TInteger/TNumber document.
    TNumber     → "math.type(" <> expr <> ") == 'float'"
    TInteger    → "math.type(" <> expr <> ") == 'integer'"
    TBoolean    → "type(" <> expr <> ") == 'boolean'"
    TNullable t → "((" <> expr <> ") == nil or (" <> typePredicate t expr <> "))"
    TRecord fs  → T.intercalate " and "
        ( ("type(" <> expr <> ") == 'table'")
        : ("onlyKeys(" <> expr <> ", {" <> fieldKeyList fs <> "})")
        : [ "(" <> typePredicate (fieldType f) (expr <> "." <> fieldName f) <> ")"
          | f ← fs ] )
    TArray t    → "type(" <> expr <> ") == 'table' and allOf(" <> expr
                    <> ", function(e) return " <> typePredicate t "e" <> " end)"

fieldKeyList ∷ [LuaField] → Text
fieldKeyList fs = T.intercalate ", " [ "'" <> fieldName f <> "'" | f ← fs ]

-- * Console helpers

-- | One command through the exact loadstring+pcall primitive the real
--   TCP debug console uses.
evalDebug ∷ LuaBackendState → Text → IO Text
evalDebug ls = executeDebugLua (lbsLuaState ls)

isLuaError ∷ Text → Bool
isLuaError t = "error:" `T.isPrefixOf` t ∨ "syntax error:" `T.isPrefixOf` t

-- | The console renders a returned string as a JSON string. Every value
--   compared through this helper is drawn from @[A-Za-z0-9_\/ ]@, so
--   stripping the delimiters is exact rather than an unescape.
unquoted ∷ Text → Text
unquoted t = fromMaybe t (T.stripSuffix "\"" =≪ T.stripPrefix "\"" t)

dedup ∷ Eq α ⇒ [α] → [α]
dedup (x : y : rest) | x ≡ y     = dedup (y : rest)
                     | otherwise = x : dedup (y : rest)
dedup xs = xs
