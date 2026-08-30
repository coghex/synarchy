{-# LANGUAGE Strict #-}
module Engine.Input.Types where

import UPrelude
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Graphics.UI.GLFW as GLFW
import UI.ControlActivation (PendingActivation)

data InputState = InputState
    { inpKeyStates ∷ Map.Map GLFW.Key KeyState
    , inpMousePos  ∷ (Double, Double)    -- ^ Current mouse position
    , inpMouseBtns ∷ Map.Map GLFW.MouseButton Bool
    , inpMouseRoutes ∷ Map.Map GLFW.MouseButton ClickRoute
      -- ^ Where each button's most recent press was routed, so the
      --   matching release can tell Lua what its down did
    , inpWindowFocused ∷ Bool            -- ^ Is window currently focused
    , inpPendingUIClick ∷ Map.Map GLFW.MouseButton PendingUIClick
      -- ^ F4 (#730): a ClickUI-routed press's deferred record (see
      --   'PendingUIClick'), held until the matching release so
      --   Engine.Input.Thread can classify the WHOLE gesture as a
      --   plain click or a UI-widget drag exactly once — the same
      --   defer-to-release pattern 'CharBatch' below uses for
      --   synthetic text, and 'scripts/unit_drag_select.lua' uses for
      --   game-world box-selection. Removed on release regardless of
      --   outcome.
    , inpPendingActivation ∷ Map.Map GLFW.MouseButton PendingActivation
      -- ^ #745: present only for a DISCRETE (non-'ueDragActivation')
      --   'UI.InputOwnership.RouteElement' press — the click callback
      --   has NOT fired yet ('inpPendingUIClick' above still records
      --   the F4 bookkeeping exactly as before #745, but no
      --   Lua event rides with it any more for this route). The
      --   matching release resolves this via
      --   'UI.ControlActivation.resolveActivation' and fires
      --   'Engine.Scripting.Lua.Types.LuaUIClickEvent'/
      --   'LuaUIRightClickEvent' only on 'UI.ControlActivation.Activate'.
      --   Removed on release regardless of outcome, same lifecycle as
      --   'inpPendingUIClick'.
    , inpCharBatch ∷ Maybe CharBatch
      -- ^ F4 (#730) Layer A: running tally of 'InputCharEvent's seen
      --   since the last flush, so a synthetic multi-character
      --   @input.type@ sequence — which arrives as N individual char
      --   events with no other event interleaved before its trailing
      --   'InputBarrier' — collapses into exactly ONE aggregate
      --   outcome record instead of N. Flushed (see
      --   'Engine.Input.Thread.Char.flushPendingCharBatch') whenever a
      --   non-char event is processed, or once at the tail of every
      --   queue drain — real typing always has an interleaving key
      --   event between characters (GLFW fires key-down, char,
      --   key-up per keystroke), so it naturally flushes once per
      --   real character.
    , inpControlFocusConsumedKeys ∷ Set.Set GLFW.Key
      -- ^ #745: GLFW keys currently mid-hold that the
      --   keyboard control-focus layer consumed at their initial
      --   Pressed dispatch (Tab/Shift+Tab, Enter/Space, a steppable
      --   arrow). 'GLFW.KeyState'Pressed'/'Repeating'/'Released' each
      --   arrive as a SEPARATE 'Engine.Input.Thread.Keyboard.
      --   dispatchKeyEvent' call with its own fresh local "consumed
      --   this dispatch" tracking, so without this the layer's
      --   suppression of the gameplay onKeyDown broadcast and
      --   inpKeyStates withholding would only cover the initial press
      --   — a HELD steppable arrow would leak Repeating events to
      --   gameplay/camera-pan, and Released would broadcast an
      --   unpaired LuaKeyUpEvent with no matching key-down. Inserted
      --   on a freshly-consumed press, consulted (in addition to that
      --   dispatch's own fresh consumption) on every dispatch for the
      --   same key, and removed on release regardless of outcome.
    , inpInjectHolds ∷ Map.Map InjectGesture (Set.Set GLFW.Key)
      -- ^ #1927: modifier keys each ACTIVE synthetic split hold is
      --   currently bracketing — the ownership record 'inpKeyStates'
      --   alone cannot express. 'inpKeyStates' keeps meaning exactly
      --   what it always did (the DIRECT owner: real GLFW input, and
      --   synthetic events not bracketing a split hold); a modifier is
      --   PUBLISHED held when EITHER owner class holds it, which is
      --   what 'keyHeld' — and therefore @engine.isKeyDown@ — answers.
      --
      --   Two opposite defects this closes, both live-reproduced in
      --   @docs/project_review_693-682.md@ PRR-1: a split hold whose up
      --   half omits the modifier list used to leak its own modifier
      --   held forever, and a split hold that DID repeat the list used
      --   to release a modifier an independent hold (an earlier
      --   @input.keyDown(\"Shift\")@, or a physical press) still owned.
      --   Ownership, not the up half's argument list, decides what a
      --   gesture releases, so neither can happen: a gesture releases
      --   exactly what it claimed, and only once no other owner holds
      --   it.
      --
      --   Entries live from the down half's 'InputGestureHold' to the
      --   up half's fenced 'InputGestureRelease' — deliberately spanning
      --   the primary release's own Lua callbacks, so #697's fenced
      --   contract still shows those callbacks the modifier as held.
      --   Cleared wholesale by 'Engine.Input.State.clearHeldInput' on
      --   focus loss / minimize, like every other held-input map.
    } deriving (Show, Eq)

-- | Identity of a synthetic SPLIT hold (#1927). Its down and up halves
--   are INDEPENDENT verb calls carrying no shared handle, so the
--   gesture is keyed by the only thing both halves name: the held key
--   ('Engine.Input.Inject.keyDownSequence' /
--   'Engine.Input.Inject.keyUpSequence') or the held mouse button
--   ('Engine.Input.Inject.mouseDownSequence' /
--   'Engine.Input.Inject.mouseUpSequence').
--
--   Taps and clicks are deliberately NOT gestures: their modifiers
--   never outlive one synthesized sequence, so #697's fence already
--   resolves them and their behaviour is unchanged.
data InjectGesture
    = GestureKey GLFW.Key
    | GestureMouse GLFW.MouseButton
    deriving (Show, Eq, Ord)

-- | Which owner class a key event's HOLD is attributed to (#1927) —
--   the metadata 'Engine.Input.State.updateKeyState' needs to keep the
--   two classes apart. Routing, broadcasts and every downstream
--   consumer are identical either way; only the state attribution
--   differs.
data KeyHoldOwner
    = OwnerDirect
      -- ^ Real GLFW input, and every synthetic event that is not a
      --   split hold's modifier bracket. Recorded in 'inpKeyStates',
      --   exactly as before #1927.
    | OwnerGesture InjectGesture
      -- ^ A split hold's modifier bracket. Recorded in
      --   'inpInjectHolds' under the owning gesture, so the direct
      --   owner's own state is neither overwritten on claim nor
      --   cleared on release.
    deriving (Show, Eq, Ord)

-- | Is this exact physical key PUBLISHED as held? True when the direct
--   owner holds it ('inpKeyStates') or any active synthetic split hold
--   brackets it ('inpInjectHolds') — the #1927 ownership union every
--   held-key poller must ask, rather than reading 'inpKeyStates'
--   directly.
keyHeld ∷ InputState → GLFW.Key → Bool
keyHeld state key =
    directlyHeld state key
    ∨ any (Set.member key) (Map.elems (inpInjectHolds state))

-- | Would this key still be held after @gesture@ dropped its claim?
--   The #1927 over-release guard: a gesture's up half emits a real key
--   release ONLY when nothing else holds the key — one physical Shift
--   produces no key-up while another owner is still pressing it.
keyHeldByOtherOwner ∷ InputState → InjectGesture → GLFW.Key → Bool
keyHeldByOtherOwner state gesture key =
    directlyHeld state key
    ∨ any (Set.member key)
          (Map.elems (Map.delete gesture (inpInjectHolds state)))

-- | The DIRECT owner's own hold for one key — 'inpKeyStates' read
--   literally, with an absent entry meaning "never pressed", exactly
--   as before #1927.
directlyHeld ∷ InputState → GLFW.Key → Bool
directlyHeld state key =
    maybe False keyPressed (Map.lookup key (inpKeyStates state))

-- | The modifier keys @gesture@ currently claims, in the order a
--   matching release should emit them: the reverse of the ascending
--   order 'InputGestureHold' claimed them in, mirroring the
--   reverse-order releases 'Engine.Input.Inject' has always used for
--   taps and clicks.
gestureHeldKeys ∷ InputState → InjectGesture → [GLFW.Key]
gestureHeldKeys state gesture =
    reverse . Set.toAscList $
        Map.findWithDefault Set.empty gesture (inpInjectHolds state)

-- | F4 (#730): a ClickUI-routed (or middle-button camera-drag) press
--   whose ONE action-outcome record is deferred until the matching
--   release can classify the whole gesture — see 'inpPendingUIClick'.
--
--   #1676: the press position is retained in BOTH coordinate spaces
--   because neither is recoverable from the other later.
--   'pucPressX'/'pucPressY' are the WINDOW pixels the click/drag
--   threshold compares against; 'pucPressFbX'/'pucPressFbY' are the
--   framebuffer-pixel oracle position (#774) captured from the
--   window/framebuffer geometry live at PRESS dispatch. A DPI change
--   or a window/framebuffer resize during the hold moves that ratio,
--   so reconverting the retained window coordinate at resolution time
--   reports the press under a ratio it never happened at. The
--   framebuffer pair falls back to the raw window coordinate when the
--   press-time viewport is degenerate (the same all-four-dimensions
--   guard 'Engine.Input.Inject.windowToFb' applies), and stays that
--   raw value however the geometry later recovers.
data PendingUIClick = PendingUIClick
    { pucKind     ∷ !Text
      -- ^ F4 kind to record if the gesture resolves as a CLICK:
      --   @"input.click"@ or @"input.rightClick"@. A gesture past
      --   the drag threshold records @"input.drag"@ instead.
    , pucCallback ∷ !Text   -- ^ Handler name, recorded as @aoHandler@.
    , pucPressX   ∷ !Double -- ^ Press x, WINDOW pixels.
    , pucPressY   ∷ !Double -- ^ Press y, WINDOW pixels.
    , pucPressFbX ∷ !Double -- ^ Press x, FRAMEBUFFER pixels at press time.
    , pucPressFbY ∷ !Double -- ^ Press y, FRAMEBUFFER pixels at press time.
    } deriving (Show, Eq)

-- | One in-flight aggregate of 'InputCharEvent' outcomes — see
--   'inpCharBatch'. @cbHandler@ reports the domain an APPLIED
--   character landed in (shell text / UI text), preferred over a
--   drop classification so a partially-delivered batch still names
--   its real destination; @cbDropReason@ separately carries why any
--   dropped characters were dropped.
data CharBatch = CharBatch
    { cbRequested  ∷ !Int
    , cbApplied    ∷ !Int
    , cbDropped    ∷ !Int
    , cbHandler    ∷ !(Maybe Text)
    , cbTarget     ∷ !(Maybe Word32)
    , cbDropReason ∷ !(Maybe Text)
    } deriving (Show, Eq)

emptyCharBatch ∷ CharBatch
emptyCharBatch = CharBatch
    { cbRequested = 0, cbApplied = 0, cbDropped = 0
    , cbHandler = Nothing, cbTarget = Nothing, cbDropReason = Nothing
    }

-- | Where the input thread routed a mouse press. onMouseUp always
--   fires on physical release (UI widget drags that started from a
--   LuaUIClickEvent depend on it to end); the route travels with the
--   release as onMouseUp's 4th argument so handlers that want strict
--   down/up pairing can filter on \"game\".
data ClickRoute
    = ClickGame      -- ^ Dispatched as LuaMouseDownEvent (game world)
    | ClickUI        -- ^ A UI element ate it (LuaUIClickEvent / right-click)
    | ClickSwallowed -- ^ Consumed with no Lua event (tooltip lock, minimized window)
    deriving (Show, Eq)

-- | Name handed to Lua as onMouseUp's 4th argument.
clickRouteText ∷ ClickRoute → Text
clickRouteText ClickGame      = "game"
clickRouteText ClickUI        = "ui"
clickRouteText ClickSwallowed = "swallowed"

-- * Input events

data InputEvent
    = InputKeyEvent 
        { ikeKey      ∷ GLFW.Key        -- ^ The key being pressed/released
        , ikeKeyState ∷ GLFW.KeyState   -- ^ Whether it's pressed or released
        , ikeMods     ∷ GLFW.ModifierKeys -- ^ Modifier keys (shift, ctrl, etc)
        }
    | InputCharEvent
        { iceChar ∷ Char                 -- ^ Character input
        }
    | InputWindowEvent
        { iweWinEvent ∷ WindowEvent     -- ^ Window-related events
        }
    | InputMouseEvent
        { imeMouseBtn   ∷ GLFW.MouseButton  -- ^ Mouse button
        , imeMousePos   ∷ (Double, Double)  -- ^ Cursor position
        , imeMouseState ∷ GLFW.MouseButtonState -- ^ Button state
        }
    | InputCursorMove Double Double
    | InputScrollEvent
        { iseScrollX   ∷ Double         -- ^ Scroll X offset
        , iseScrollY    ∷ Double         -- ^ Scroll Y offset
        }
      -- | Fence for synthetic sequences (#697): the input thread
      --   forwards the carried events to the Lua thread
      --   (LuaInjectFollowup), which re-injects them into the input
      --   queue only after dispatching every Lua broadcast the
      --   preceding events produced. Both queues are FIFO, so the
      --   carried events (modifier releases) are processed strictly
      --   after the callbacks that must still observe the pre-fence
      --   state. Never produced by GLFW callbacks.
    | InputFollowup [InputEvent]
      -- | Completion marker for synthetic injection (#727). Carries a
      --   caller-allocated, monotonically increasing token
      --   ('Engine.Input.Inject.newBarrierToken') — processing it only
      --   advances 'Engine.Core.State.inputBarrierRef' to (at least)
      --   that token. Appended after a pushed batch so
      --   'Engine.Input.Inject.waitForBarrier' has a race-free "MY
      --   events are done" signal: FIFO ordering guarantees a barrier
      --   is only ever processed after everything queued ahead of it,
      --   real GLFW input never produces one (so unrelated concurrent
      --   activity can never satisfy someone else's wait), and the
      --   unique token means even a STALE barrier from an earlier
      --   caller that gave up waiting (timeout) can't satisfy a LATER
      --   caller's wait for its own, numerically higher token — a
      --   bare counter shared across calls could (#727 review).
      --   Never produced by GLFW callbacks.
    | InputBarrier Int
      -- | A synthetic split hold's modifier CLAIM (#1927), emitted by
      --   'Engine.Input.Inject.keyDownSequence' /
      --   'Engine.Input.Inject.mouseDownSequence' in place of the bare
      --   modifier presses those sequences used to emit. Each carried
      --   key is still dispatched as a real key press — same routing,
      --   same Lua broadcast, same 'GLFW.ModifierKeys' — but its HOLD
      --   is attributed to the gesture ('inpInjectHolds') instead of
      --   the direct owner, so the gesture can later release exactly
      --   what it introduced and nothing else. Never produced by GLFW
      --   callbacks; never emitted when the hold has no modifiers.
    | InputGestureHold InjectGesture [GLFW.Key] GLFW.ModifierKeys
      -- | A synthetic split hold ENDING (#1927), emitted by
      --   'Engine.Input.Inject.keyUpSequence' /
      --   'Engine.Input.Inject.mouseUpSequence' right after the
      --   primary release. The up half cannot know what the down half
      --   claimed, so this asks the INPUT THREAD — the one place that
      --   knows — to resolve it: if the gesture holds nothing this is a
      --   no-op (a modifier-free split hold behaves exactly as it did
      --   before #1927, fence included: none), and otherwise it fences
      --   an 'InputGestureRelease' through the Lua thread the same way
      --   'InputFollowup' fences a tap's releases, so the primary
      --   release's own callbacks still observe the modifier held
      --   (#697). Never produced by GLFW callbacks.
    | InputGestureEnd InjectGesture GLFW.ModifierKeys
      -- | The fenced half of 'InputGestureEnd' (#1927), re-injected by
      --   the Lua thread after the up half's broadcasts have run. Drops
      --   the gesture's claim and emits a real key release for each
      --   claimed key NO OTHER owner still holds — an independently
      --   held modifier (an outstanding @input.keyDown(\"Shift\")@, or a
      --   physical press) survives untouched. Never produced by GLFW
      --   callbacks.
    | InputGestureRelease InjectGesture GLFW.ModifierKeys
    deriving (Show, Eq)

-- * Window events

data WindowEvent
    = WindowResize Int Int        -- ^ New width and height
    | FramebufferResize Int Int -- ^ New framebuffer width and height
    | WindowClose                 -- ^ Window close request
    | WindowFocus Bool           -- ^ Window focus gained/lost
    | WindowMinimize Bool        -- ^ Window minimized/restored
    deriving (Show, Eq)

-- * Key state

data KeyState = KeyState
    { keyPressed ∷ Bool         -- ^ Is the key currently pressed
    , keyMods    ∷ GLFW.ModifierKeys  -- ^ Active modifiers when pressed
    , keyTime    ∷ Double      -- ^ Time of last state change
    } deriving (Show, Eq)

-- * Platform-independent key type

data Key
    = KeyA | KeyB | KeyC | KeyD | KeyE | KeyF | KeyG | KeyH | KeyI | KeyJ
    | KeyK | KeyL | KeyM | KeyN | KeyO | KeyP | KeyQ | KeyR | KeyS | KeyT
    | KeyU | KeyV | KeyW | KeyX | KeyY | KeyZ
    | Key0 | Key1 | Key2 | Key3 | Key4 | Key5 | Key6 | Key7 | Key8 | Key9
    | KeySpace | KeyEnter | KeyEscape | KeyTab | KeyBackspace | KeyDelete
    | KeyUp | KeyDown | KeyLeft | KeyRight | KeyHome | KeyEnd
    | KeyShift | KeyCtrl | KeyAlt | KeySuper
    | KeyGrave | KeyMinus | KeyEqual | KeyComma | KeyPeriod
    | KeyF1 | KeyF2 | KeyF3 | KeyF4 | KeyF5 | KeyF6
    | KeyF7 | KeyF8 | KeyF9 | KeyF10 | KeyF11 | KeyF12
    | KeyUnknown
    deriving (Eq, Ord, Show, Read, Enum, Bounded)

fromGLFWKey ∷ GLFW.Key → Key
fromGLFWKey GLFW.Key'A = KeyA
fromGLFWKey GLFW.Key'B = KeyB
fromGLFWKey GLFW.Key'C = KeyC
fromGLFWKey GLFW.Key'D = KeyD
fromGLFWKey GLFW.Key'E = KeyE
fromGLFWKey GLFW.Key'F = KeyF
fromGLFWKey GLFW.Key'G = KeyG
fromGLFWKey GLFW.Key'H = KeyH
fromGLFWKey GLFW.Key'I = KeyI
fromGLFWKey GLFW.Key'J = KeyJ
fromGLFWKey GLFW.Key'K = KeyK
fromGLFWKey GLFW.Key'L = KeyL
fromGLFWKey GLFW.Key'M = KeyM
fromGLFWKey GLFW.Key'N = KeyN
fromGLFWKey GLFW.Key'O = KeyO
fromGLFWKey GLFW.Key'P = KeyP
fromGLFWKey GLFW.Key'Q = KeyQ
fromGLFWKey GLFW.Key'R = KeyR
fromGLFWKey GLFW.Key'S = KeyS
fromGLFWKey GLFW.Key'T = KeyT
fromGLFWKey GLFW.Key'U = KeyU
fromGLFWKey GLFW.Key'V = KeyV
fromGLFWKey GLFW.Key'W = KeyW
fromGLFWKey GLFW.Key'X = KeyX
fromGLFWKey GLFW.Key'Y = KeyY
fromGLFWKey GLFW.Key'Z = KeyZ
fromGLFWKey GLFW.Key'0 = Key0
fromGLFWKey GLFW.Key'1 = Key1
fromGLFWKey GLFW.Key'2 = Key2
fromGLFWKey GLFW.Key'3 = Key3
fromGLFWKey GLFW.Key'4 = Key4
fromGLFWKey GLFW.Key'5 = Key5
fromGLFWKey GLFW.Key'6 = Key6
fromGLFWKey GLFW.Key'7 = Key7
fromGLFWKey GLFW.Key'8 = Key8
fromGLFWKey GLFW.Key'9 = Key9
fromGLFWKey GLFW.Key'Space = KeySpace
fromGLFWKey GLFW.Key'Enter = KeyEnter
fromGLFWKey GLFW.Key'Escape = KeyEscape
fromGLFWKey GLFW.Key'Tab = KeyTab
fromGLFWKey GLFW.Key'Backspace = KeyBackspace
fromGLFWKey GLFW.Key'Delete = KeyDelete
fromGLFWKey GLFW.Key'Up = KeyUp
fromGLFWKey GLFW.Key'Down = KeyDown
fromGLFWKey GLFW.Key'Left = KeyLeft
fromGLFWKey GLFW.Key'Right = KeyRight
fromGLFWKey GLFW.Key'LeftShift = KeyShift
fromGLFWKey GLFW.Key'RightShift = KeyShift
fromGLFWKey GLFW.Key'LeftControl = KeyCtrl
fromGLFWKey GLFW.Key'RightControl = KeyCtrl
fromGLFWKey GLFW.Key'LeftAlt = KeyAlt
fromGLFWKey GLFW.Key'RightAlt = KeyAlt
fromGLFWKey GLFW.Key'LeftSuper = KeySuper
fromGLFWKey GLFW.Key'RightSuper = KeySuper
fromGLFWKey GLFW.Key'Home = KeyHome
fromGLFWKey GLFW.Key'End = KeyEnd
fromGLFWKey GLFW.Key'GraveAccent = KeyGrave
fromGLFWKey GLFW.Key'Minus = KeyMinus
fromGLFWKey GLFW.Key'Equal = KeyEqual
fromGLFWKey GLFW.Key'Comma = KeyComma
fromGLFWKey GLFW.Key'Period = KeyPeriod
fromGLFWKey GLFW.Key'F1 = KeyF1
fromGLFWKey GLFW.Key'F2 = KeyF2
fromGLFWKey GLFW.Key'F3 = KeyF3
fromGLFWKey GLFW.Key'F4 = KeyF4
fromGLFWKey GLFW.Key'F5 = KeyF5
fromGLFWKey GLFW.Key'F6 = KeyF6
fromGLFWKey GLFW.Key'F7 = KeyF7
fromGLFWKey GLFW.Key'F8 = KeyF8
fromGLFWKey GLFW.Key'F9 = KeyF9
fromGLFWKey GLFW.Key'F10 = KeyF10
fromGLFWKey GLFW.Key'F11 = KeyF11
fromGLFWKey GLFW.Key'F12 = KeyF12
fromGLFWKey _ = KeyUnknown

keyToText ∷ Key → Text
keyToText KeyA = "A"
keyToText KeyB = "B"
keyToText KeyC = "C"
keyToText KeyD = "D"
keyToText KeyE = "E"
keyToText KeyF = "F"
keyToText KeyG = "G"
keyToText KeyH = "H"
keyToText KeyI = "I"
keyToText KeyJ = "J"
keyToText KeyK = "K"
keyToText KeyL = "L"
keyToText KeyM = "M"
keyToText KeyN = "N"
keyToText KeyO = "O"
keyToText KeyP = "P"
keyToText KeyQ = "Q"
keyToText KeyR = "R"
keyToText KeyS = "S"
keyToText KeyT = "T"
keyToText KeyU = "U"
keyToText KeyV = "V"
keyToText KeyW = "W"
keyToText KeyX = "X"
keyToText KeyY = "Y"
keyToText KeyZ = "Z"
keyToText Key0 = "0"
keyToText Key1 = "1"
keyToText Key2 = "2"
keyToText Key3 = "3"
keyToText Key4 = "4"
keyToText Key5 = "5"
keyToText Key6 = "6"
keyToText Key7 = "7"
keyToText Key8 = "8"
keyToText Key9 = "9"
keyToText KeySpace = "Space"
keyToText KeyEnter = "Enter"
keyToText KeyEscape = "Escape"
keyToText KeyTab = "Tab"
keyToText KeyBackspace = "Backspace"
keyToText KeyDelete = "Delete"
keyToText KeyUp = "Up"
keyToText KeyDown = "Down"
keyToText KeyLeft = "Left"
keyToText KeyRight = "Right"
keyToText KeyShift = "Shift"
keyToText KeyCtrl = "Ctrl"
keyToText KeyAlt = "Alt"
keyToText KeySuper = "Super"
keyToText KeyGrave = "Grave"
keyToText KeyMinus = "Minus"
keyToText KeyEqual = "Equal"
keyToText KeyComma = "Comma"
keyToText KeyPeriod = "Period"
keyToText KeyF1 = "F1"
keyToText KeyF2 = "F2"
keyToText KeyF3 = "F3"
keyToText KeyF4 = "F4"
keyToText KeyF5 = "F5"
keyToText KeyF6 = "F6"
keyToText KeyF7 = "F7"
keyToText KeyF8 = "F8"
keyToText KeyF9 = "F9"
keyToText KeyF10 = "F10"
keyToText KeyF11 = "F11"
keyToText KeyF12 = "F12"
keyToText KeyHome = "Home"
keyToText KeyEnd = "End"
keyToText KeyUnknown = "Unknown"

-- | Inverse of 'keyToText', built mechanically over the whole enum so
--   the two can never drift. Every name 'keyToText' can hand to Lua is
--   guaranteed to parse back. KeyUnknown is excluded — "Unknown" is
--   not a bindable name.
textToKey ∷ Text → Maybe Key
textToKey t = Map.lookup t textToKeyMap

textToKeyMap ∷ Map.Map Text Key
textToKeyMap = Map.fromList
    [ (keyToText k, k) | k ← [minBound .. maxBound], k ≢ KeyUnknown ]

-- | All GLFW keys that map to a logical key — derived from
--   'fromGLFWKey' so the inverse can't drift. Merged modifiers fan out
--   to both sides (KeyShift → [LeftShift, RightShift], etc.).
keyToGLFW ∷ Key → [GLFW.Key]
keyToGLFW k = Map.findWithDefault [] k keyToGLFWMap

keyToGLFWMap ∷ Map.Map Key [GLFW.Key]
keyToGLFWMap = Map.fromListWith (flip (<>))
    [ (fromGLFWKey g, [g])
    | g ← [minBound .. maxBound]
    , fromGLFWKey g ≢ KeyUnknown ]

defaultKeyState ∷ KeyState
defaultKeyState = KeyState
    { keyPressed = False
    , keyMods    = GLFW.ModifierKeys
        { GLFW.modifierKeysShift    = False
        , GLFW.modifierKeysControl  = False
        , GLFW.modifierKeysAlt      = False
        , GLFW.modifierKeysSuper    = False
        , GLFW.modifierKeysCapsLock = False
        , GLFW.modifierKeysNumLock  = False
        }
    , keyTime    = 0.0
    }

defaultInputState ∷ InputState
defaultInputState = InputState
    { inpKeyStates = Map.empty
    , inpMousePos = (0.0, 0.0)
    , inpMouseBtns = Map.empty
    , inpMouseRoutes = Map.empty
    , inpWindowFocused = True
    , inpPendingUIClick = Map.empty
    , inpPendingActivation = Map.empty
    , inpCharBatch = Nothing
    , inpControlFocusConsumedKeys = Set.empty
    , inpInjectHolds = Map.empty
    }
