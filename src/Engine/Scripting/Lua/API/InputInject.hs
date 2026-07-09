-- | input.* — synthetic input injection over the debug console (#644),
--   the actor-output channel of the UX playtest harness (#641). Each
--   verb synthesizes internal 'InputEvent's (see "Engine.Input.Inject")
--   into the same queue the GLFW callbacks feed, then blocks until the
--   input thread has consumed them, so a lockstep harness knows the
--   action landed before it steps the sim.
--
--   Coordinates are FRAMEBUFFER pixels — the exact space
--   debug.captureScreenshot (#643) reports as width/height — converted
--   internally to the window coordinates the input pipeline expects
--   (on HiDPI displays the two differ by the DPI scale).
--
--   Every verb returns an ack table: @{ ok = true }@ on success,
--   @{ error = \"...\" }@ on failure (unknown key/button/modifier name,
--   headless engine, degenerate window, or consumption timeout).
--
--   Surface:
--     input.moveMouse(x, y)
--     input.click(x, y[, button[, mods]])      button: left|right|middle
--     input.mouseDown(x, y[, button[, mods]])  mods: e.g. {"shift","ctrl"}
--     input.mouseUp(x, y[, button[, mods]])
--     input.scroll(dx, dy)
--     input.key(name[, mods])                  name: keyToText vocabulary
--     input.keyDown(name[, mods])
--     input.keyUp(name[, mods])
--     input.type(text)
module Engine.Scripting.Lua.API.InputInject
  ( inputMoveMouseFn
  , inputClickFn
  , inputMouseDownFn
  , inputMouseUpFn
  , inputScrollFn
  , inputKeyFn
  , inputKeyDownFn
  , inputKeyUpFn
  , inputTypeFn
  ) where

import UPrelude
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.IORef (readIORef)
import qualified Graphics.UI.GLFW as GLFW
import qualified HsLua as Lua
import Engine.Core.State (EngineEnv(..))
import Engine.Core.Types (EngineConfig(..))
import Engine.Input.Inject
import Engine.Input.Types (InputEvent)

-- | How long a verb waits for the input thread to drain the queue.
--   The thread ticks every ~16 ms, so a live one answers in one tick;
--   only a stalled/absent input thread runs this out.
consumeTimeoutMicros ∷ Int
consumeTimeoutMicros = 2 * 1000 * 1000

-- | Inject a prebuilt sequence and push the ack table.
ackInject ∷ EngineEnv → [InputEvent] → Lua.LuaE Lua.Exception Lua.NumResults
ackInject env evs = do
    consumed ← Lua.liftIO $
        injectEvents (inputQueue env) consumeTimeoutMicros evs
    if consumed
        then pushOk
        else pushError $
            "input: timed out waiting for the input thread to consume "
            <> "the synthesized events — is the engine's input thread "
            <> "running?"

-- | Shared guard + framebuffer→window conversion for the mouse verbs.
--   Runs the continuation with the WINDOW-space position.
withWindowCoords ∷ EngineEnv → Double → Double
                 → ((Double, Double) → Lua.LuaE Lua.Exception Lua.NumResults)
                 → Lua.LuaE Lua.Exception Lua.NumResults
withWindowCoords env x y k = do
    sizes ← Lua.liftIO $ do
        win ← readIORef (windowSizeRef env)
        fb  ← readIORef (framebufferSizeRef env)
        pure (win, fb)
    case uncurry fbToWindow sizes (x, y) of
        Just posWin → k posWin
        Nothing → pushError $
            "input: window/framebuffer size unavailable (minimized "
            <> "window?) — cannot convert framebuffer coordinates"

-- | Guard every verb: injection needs the real input pipeline, which
--   only runs on a windowed (or preview) instance.
headlessGuard ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
              → Lua.LuaE Lua.Exception Lua.NumResults
headlessGuard env k
    | ecHeadless (engineConfig env) = pushError $
        "input: no input pipeline — this engine is running --headless; "
        <> "input injection needs a windowed instance"
    | otherwise = k

-- | Optional button argument (default left).
readButtonArg ∷ Lua.StackIndex → Lua.LuaE Lua.Exception (Either Text GLFW.MouseButton)
readButtonArg idx = do
    ty ← Lua.ltype idx
    if ty ≡ Lua.TypeNone ∨ ty ≡ Lua.TypeNil
        then pure (Right GLFW.MouseButton'1)
        else do
            mbs ← Lua.tostring idx
            case mbs of
                Nothing → pure (Left "input: button must be a string")
                Just bs → case resolveButton (TE.decodeUtf8 bs) of
                    Just b  → pure (Right b)
                    Nothing → pure . Left $
                        "input: unknown button \"" <> TE.decodeUtf8 bs
                        <> "\" (expected left|right|middle)"

-- | Optional mods argument: absent/nil → no modifiers; otherwise a
--   list of strings from {\"shift\",\"ctrl\",\"alt\",\"super\"}.
readModsArg ∷ Lua.StackIndex
            → Lua.LuaE Lua.Exception (Either Text ([GLFW.Key], GLFW.ModifierKeys))
readModsArg idx = do
    ty ← Lua.ltype idx
    if ty ≡ Lua.TypeNone ∨ ty ≡ Lua.TypeNil
        then pure (Right ([], noMods))
        else do
            mNames ← readStringList idx
            case mNames of
                Nothing → pure (Left "input: mods must be a list of strings")
                Just names → case resolveMods names of
                    Just mm → pure (Right mm)
                    Nothing → pure . Left $
                        "input: unknown modifier in "
                        <> T.intercalate "," names
                        <> " (expected shift|ctrl|alt|super)"

-- | Read a Lua array of strings at the given stack index.
readStringList ∷ Lua.StackIndex → Lua.LuaE Lua.Exception (Maybe [Text])
readStringList idx = do
    isT ← Lua.istable idx
    if not isT
        then return Nothing
        else do
            n ← Lua.rawlen idx
            let go i acc
                    | i > fromIntegral n = return (Just (reverse acc))
                    | otherwise = do
                        _ ← Lua.rawgeti idx i
                        ty ← Lua.ltype (-1)
                        ms ← if ty ≡ Lua.TypeString
                                then Lua.tostring (-1)
                                else return Nothing
                        Lua.pop 1
                        case ms of
                            Just bs → go (i + 1) (TE.decodeUtf8 bs : acc)
                            Nothing → return Nothing
            go (1 ∷ Lua.Integer) []

-- | input.moveMouse(x, y) — set the cursor position (updates
--   hover/pick/tooltip state like a real move).
inputMoveMouseFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
inputMoveMouseFn env = headlessGuard env $ do
    mx ← Lua.tonumber 1
    my ← Lua.tonumber 2
    case (mx, my) of
        (Just x, Just y) → withWindowCoords env (realToFrac x) (realToFrac y) $
            \pos → ackInject env (moveSequence pos)
        _ → pushError "input.moveMouse: x and y (numbers) required"

-- | Common body for click / mouseDown / mouseUp.
mouseVerb ∷ ((Double, Double) → GLFW.MouseButton
             → ([GLFW.Key], GLFW.ModifierKeys) → [InputEvent])
          → Text → EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
mouseVerb buildSeq verbName env = headlessGuard env $ do
    mx ← Lua.tonumber 1
    my ← Lua.tonumber 2
    eBtn ← readButtonArg 3
    eMods ← readModsArg 4
    case (mx, my) of
        (Just x, Just y) → case (eBtn, eMods) of
            (Right btn, Right mm) →
                withWindowCoords env (realToFrac x) (realToFrac y) $
                    \pos → ackInject env (buildSeq pos btn mm)
            (Left err, _) → pushError err
            (_, Left err) → pushError err
        _ → pushError $ "input." <> verbName
                <> ": x and y (numbers) required"

inputClickFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
inputClickFn = mouseVerb clickSequence "click"

inputMouseDownFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
inputMouseDownFn = mouseVerb mouseDownSequence "mouseDown"

inputMouseUpFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
inputMouseUpFn = mouseVerb mouseUpSequence "mouseUp"

-- | input.scroll(dx, dy) — wheel scroll at the current cursor position
--   (moveMouse first to target a specific element).
inputScrollFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
inputScrollFn env = headlessGuard env $ do
    mdx ← Lua.tonumber 1
    mdy ← Lua.tonumber 2
    case (mdx, mdy) of
        (Just dx, Just dy) → ackInject env
            (scrollSequence (realToFrac dx) (realToFrac dy))
        _ → pushError "input.scroll: dx and dy (numbers) required"

-- | Common body for key / keyDown / keyUp.
keyVerb ∷ (GLFW.Key → ([GLFW.Key], GLFW.ModifierKeys) → [InputEvent])
        → Text → EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
keyVerb buildSeq verbName env = headlessGuard env $ do
    mName ← Lua.tostring 1
    eMods ← readModsArg 2
    case mName of
        Nothing → pushError $ "input." <> verbName
            <> ": key name (string) required"
        Just nameBS → do
            let name = TE.decodeUtf8 nameBS
            case (resolveKeyName name, eMods) of
                (Just k, Right mm) → ackInject env (buildSeq k mm)
                (Nothing, _) → pushError $
                    "input." <> verbName <> ": unknown key name \""
                    <> name <> "\" (use the keybind vocabulary, e.g. "
                    <> "\"A\", \"Space\", \"Enter\", \"LeftShift\")"
                (_, Left err) → pushError err

inputKeyFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
inputKeyFn = keyVerb keyTapSequence "key"

inputKeyDownFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
inputKeyDownFn = keyVerb keyDownSequence "keyDown"

inputKeyUpFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
inputKeyUpFn = keyVerb keyUpSequence "keyUp"

-- | input.type(text) — character events through the text-input path
--   (needs a focused text field, like real typing). Non-char editing
--   keys (Enter/Backspace/Tab/Escape) go through input.key instead.
inputTypeFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
inputTypeFn env = headlessGuard env $ do
    mText ← Lua.tostring 1
    case mText of
        Nothing → pushError "input.type: text (string) required"
        Just bs → ackInject env (typeSequence (TE.decodeUtf8 bs))

pushOk ∷ Lua.LuaE Lua.Exception Lua.NumResults
pushOk = do
    Lua.newtable
    Lua.pushboolean True
    Lua.setfield (-2) "ok"
    return 1

pushError ∷ Text → Lua.LuaE Lua.Exception Lua.NumResults
pushError msg = do
    Lua.newtable
    Lua.pushstring (TE.encodeUtf8 msg)
    Lua.setfield (-2) "error"
    return 1
