{-# LANGUAGE Strict, StrictData #-}
module Engine.Scripting.Lua.Types where

import UPrelude
import Data.IORef (IORef)
import Control.Concurrent.STM.TVar (TVar)
import Control.Concurrent.STM.TQueue (TQueue)
import Engine.Scripting.Lua.DebugServer (DebugCommand)
import Engine.Asset.Base
import Engine.Asset.Types
import Engine.Asset.Handle
import Engine.Input.Types
import Engine.Scene.Base
import Engine.Graphics.Vulkan.Types.Vertex
import Engine.Graphics.Config (WindowMode(..), TextureFilter(..))
import Engine.Graphics.Vulkan.Texture.Policy (UploadSampler(..))
import UI.Types (ElementHandle(..))
import qualified Graphics.UI.GLFW as GLFW
import qualified Engine.Core.Queue as Q
import Engine.Core.Log (LoggerState)
import World.Save.Payload (LoadReconcileContext)
import Language.Semantic.Types (Catalogue)
import Language.Generated.Types (LanguageProvenance)
import Language.Suggest (NameSuggester)
import qualified Data.Map.Strict as Map
import qualified HsLua as Lua

-- | A Lua-representable value, marshalled across the Haskell/Lua
--   boundary by "Engine.Scripting.Lua.Util"'s push/pull helpers.
--   Folded in from the deleted one-type module that used to sit beside
--   this namespace (#1059), once #992 removed its last
--   non-Lua-specific constructor: every consumer was already under
--   @Engine.Scripting.Lua.*@ and already imported this module.
--
--   The fields are explicitly lazy. This module enables 'StrictData'
--   and the declaration's former home did not, so leaving them bare
--   would force each field at construction time instead of at push
--   time — a behavior change, not a move (broadcast argument lists are
--   built for callbacks that may never run).
data ScriptValue
  = ScriptNumber ~Double
  | ScriptString ~Text
  | ScriptBool   ~Bool
  | ScriptNil
  | ScriptTable  ~[(ScriptValue, ScriptValue)]
  deriving (Show)

-- | Represents a single Lua script's metadata
data LuaScript = LuaScript
  { scriptId        ∷ Word32   -- unique identifier
  , scriptPath      ∷ FilePath -- path to the Lua script
  , scriptTickRate  ∷ Double   -- seconds between updates
  , scriptNextTick  ∷ Double   -- next scheduled tick time
  , scriptModuleRef ∷ Lua.Reference -- reference to the returned table
  , scriptPaused    ∷ Bool     -- is the script paused
  } deriving (Eq, Show)

-- | Thread-safe map of Lua scripts
type LuaScripts = TVar (Map.Map FilePath LuaScript)

-- | What @world.suggestName@ (#1106) keeps between dice presses, so a
--   press is a handful of hashes rather than a YAML read plus a
--   whole-catalogue root assignment (#1106 requirement 8 — the Create
--   World dice button runs synchronously and must not hitch a frame).
--
--   Two levels, because the two invalidate at different rates: editing
--   the world seed picks a DIFFERENT language and drops
--   'lcSuggester', while the catalogue behind it is the same file
--   contents and is kept. Rerolling within one seed reuses both.
--
--   'lcCatalogue' caches a FAILED load as readily as a successful one,
--   and the failure is sticky for the session. That is deliberate:
--   re-reading a missing or malformed file on every press of a menu
--   button is precisely the per-press filesystem I/O the synchronous
--   suggestion path must not do, and @data/language/concepts.yaml@ is a
--   shipped data file — an installation that cannot supply it does not
--   repair itself mid-session.
--
--   Purely derived, purely a cache: every field is reconstructible from
--   @data/language/concepts.yaml@ plus the provenance, nothing here is
--   authoritative for anything, and it is never persisted. It lives on
--   'LuaBackendState' because the Lua thread is the only thread that
--   suggests names.
data LanguageCache = LanguageCache
  { lcCatalogue ∷ Either Text Catalogue
    -- ^ The resolved catalogue, or the descriptive reason it could not
    --   be resolved.
  , lcSuggester ∷ Maybe (LanguageProvenance, NameSuggester)
  }

-- | Lua-specific state (wraps Lua.  State with script tracking)
data LuaBackendState = LuaBackendState
  { lbsLuaState     ∷ Lua.State
  , lbsScripts      ∷ TVar (Map.Map Word32 LuaScript)
  , lbsNextScriptId ∷ IORef Word32
  , lbsMsgQueues    ∷ (Q.Queue LuaToEngineMsg, Q.Queue LuaMsg)
  , lbsAssetPool    ∷ IORef AssetPool
  , lbsNextObjectId ∷ IORef Word32
  , lbsInputState   ∷ IORef InputState
  , lbsLoggerRef    ∷ IORef LoggerState
    -- ^ Engine logger, so 'callModuleFunction' can log Lua callback
    --   errors (now caught via pcall) without threading a logger
    --   through every broadcast call site.
  , lbsLanguageCache ∷ IORef (Maybe LanguageCache)
    -- ^ @world.suggestName@'s catalogue/suggester cache (#1106). Starts
    --   empty and fills on the first dice press; a profile that has
    --   never suggested a name never reads the catalogue at all.
  , lbsDebugQueue   ∷ TQueue DebugCommand
    -- ^ The debug-console command queue (issue #763):
    --   reachable from 'ls' at every 'processLuaMsg' call site so the
    --   'LuaSaveLoaded' handler can quarantine any command still queued
    --   at that point (queued sometime during the now-replaced session,
    --   since the debug server keeps accepting commands regardless of
    --   the save-barrier's capture-lock state) without threading a new
    --   parameter through 'processLuaMsg'/'processLuaMsgs' and their
    --   several unrelated callers (input injection, headless tests).
  }

data LuaLogLevel = LuaLogDebug
                 | LuaLogInfo
                 | LuaLogWarn
                 | LuaLogError
                 deriving (Eq, Show)

-- * Lua-to-engine messages

data LuaToEngineMsg = LuaLog LuaLogLevel String
                    | LuaSetWindowMode WindowMode
                    | LuaSetVSync Bool
                    | LuaSetMSAA Int
                    | LuaSetResolution Int Int
                    | LuaSetBrightness Int
                    | LuaSetPixelSnap Bool
                    | LuaSetTextureFilter TextureFilter
                    | LuaLoadTextureRequest TextureHandle FilePath
                                            UploadSampler
                      -- ^ An ordinary image-file load, carrying the
                      --   sampler policy its REQUESTER declared (#2075).
                      --   The policy travels on the request rather than
                      --   being decided by the handler, because it
                      --   cannot be recovered from the path: the same
                      --   directory holds world-drawn map icons and
                      --   toolbar chrome, and @utility\/white.png@ is
                      --   drawn by both. Omitting the declaration at the
                      --   call site yields 'UploadGlobalSampler', which
                      --   is exactly the pre-#2075 behavior.
                    | LuaLoadAtlasTextureRequest TextureHandle FilePath
                      -- ^ A compiled unit-animation atlas (#1259). Same
                      --   upload as a global-policy
                      --   'LuaLoadTextureRequest', so gameplay unit art
                      --   follows the player's nearest/linear setting
                      --   (#2085). Kept a constructor of its own because
                      --   the atlas path also carries D-2's
                      --   one-image-per-animation contract.
                    | LuaLoadFontRequest FontHandle FilePath Int
                    | LuaSpawnTextRequest ObjectId Float Float FontHandle
                                                   Text Vec4 LayerId Float
                    | LuaSpawnSpriteRequest
                        { lssObjectId    ∷ ObjectId -- generated in lua thread
                        , lssX           ∷ Float
                        , lssY           ∷ Float
                        , lssWidth       ∷ Float
                        , lssHeight      ∷ Float
                        , lssTextureHandle ∷ TextureHandle
                        , lssLayer       ∷ LayerId }
                    | LuaSetSpriteScaleRequest ObjectId Float Float
                    | LuaSetColorRequest ObjectId Vec4
                    | LuaSetSizeRequest ObjectId Float Float
                    | LuaSetPosRequest ObjectId Float Float
                    | LuaSetVisibleRequest ObjectId Bool
                    | LuaSetTextRequest ObjectId Text
                    | LuaDestroyRequest ObjectId
                    | LuaRequestFocus Word32
                    | LuaReleaseFocus
                    | LuaRegisterFocusable Bool Int
                    | LuaUnregisterFocusable Word32
                    deriving (Eq, Show)

-- * Engine-to-Lua messages

data LuaMsg = LuaTextureLoaded TextureHandle AssetId
            | LuaFontLoaded FontHandle FilePath
            | LuaFontLoadFailed Text
            | LuaThreadKill
            | LuaMouseDownEvent GLFW.MouseButton Double Double
            | LuaMouseUpEvent GLFW.MouseButton Double Double ClickRoute
            | LuaScrollEvent Double Double
            | LuaZSliceScroll Double Double
            -- | Logical (merged) key for the onKeyDown string, plus the
            --   exact GLFW key so engine.keyMatchesAction can resolve which
            --   side of a modifier was pressed without racing input state.
            | LuaKeyDownEvent Key GLFW.Key
            | LuaKeyUpEvent Key
              -- | Fence follow-up for synthetic input (#697): queued by
              --   the input thread when it processes an 'InputFollowup',
              --   so it sits in this queue BEHIND every broadcast the
              --   fenced sequence produced. Handling it re-injects the
              --   carried events (modifier releases) into the input
              --   queue — strictly after those broadcasts have run, so a
              --   shift-click's callback still observes shift held.
            | LuaInjectFollowup [InputEvent]
            | LuaShellToggle
            | LuaWindowResize Int Int
            | LuaFramebufferResize Int Int
            | LuaAssetLoaded Text Int Text
            | LuaAssetFailed Text Int Text Text
              -- ^ A load request reached its TERMINAL FAILURE (#1690):
              --   (assetType, handle, path, reason). Deliberately its
              --   own message rather than a flag on 'LuaAssetLoaded',
              --   which is the success-only protocol Lua sees as
              --   @onAssetLoaded@ — a waiter that mistook a failure for
              --   a load would read a handle resolving to the undefined
              --   texture. Broadcast as @onAssetFailed@, so a module
              --   holding a pending handle settles it instead of waiting
              --   forever for a load that is never coming.
            | LuaArenaReady Text
            | LuaStampLocation Text Text Int Int
              -- ^ A just-loaded chunk hosts a placed location (#89):
              --   (pageId, locationId, anchorGx, anchorGy). Broadcast to
              --   Lua as onStampLocation so the stamper materializes the
              --   geometry via the #88 builder — issued on every load of
              --   the chunk (the stamper skips it if already stamped), so
              --   a location always materializes from the persisted
              --   overlay, even after a save/load that preceded stamping.
            | LuaConstructInvalidated Text Int Int Word64
              -- ^ #1844: the world thread removed a construction
              --   designation out from under a live claimant —
              --   (pageId, gx, gy, attempt id). Broadcast to Lua as
              --   @onConstructInvalidated@ so the build AI drops that
              --   EXACT attempt's claim and job immediately, rather than
              --   holding the tile until the claimant's next decision
              --   tick or its claim timeout. The attempt is what keeps a
              --   SUCCESSOR designated at the same tile untouched: a
              --   worker that has since claimed one keeps its own job.
            | LuaConstructCompleted Text Int Int Word64
              -- ^ #1844: the world thread ACCEPTED the placement one
              --   construction attempt queued — (pageId, gx, gy, attempt
              --   id). Broadcast as @onConstructCompleted@ so the
              --   claimant grants its work XP for a piece that really
              --   landed. @structure.place@ returns once the piece is
              --   staged and queued, so Lua cannot tell on its own; a
              --   DECLINED placement sends 'LuaConstructInvalidated'
              --   instead, and neither is sent twice.
            | LuaOpenArena
            | LuaFocusLost Word32
            | LuaCharInput Word32 Char
            | LuaTextBackspace Word32
            | LuaTabPressed Word32
            | LuaTextSubmit Word32
            | LuaCursorUp Word32
            | LuaCursorDown Word32
            | LuaCursorLeft Word32
            | LuaCursorRight Word32
            | LuaCursorHome Word32
            | LuaCursorEnd Word32
            | LuaTextDelete Word32
            | LuaInterrupt Word32
            | LuaUIClickEvent ElementHandle Text Double Double
              -- ^ Element, callback name, and the click's raw window
              --   coordinates (#646) — carried purely so
              --   Dispatch.hs's F4 action-outcome record for this route
              --   isn't stuck reporting no location, same convention as
              --   LuaMouseDownEvent's own trailing x/y.
            | LuaUIRightClickEvent ElementHandle Text Double Double
            | LuaUIPressBeginEvent ElementHandle Text
              -- ^ #745: a discrete (non-drag-activation) control was
              --   just pressed — the callback has NOT fired (that's
              --   deferred to a validated release; see
              --   'LuaUIClickEvent' above), this is purely the signal a
              --   widget module needs to show a pending/pressed visual.
              --   Carries the callback name so a shared dispatcher can
              --   route it by widget family the same way
              --   'uiManager.onHoverEnter'/'onHoverLeave' already do.
            | LuaUIControlFocusChanged (Maybe ElementHandle)
              -- ^ #745: keyboard CONTROL focus moved (Tab/Shift+Tab) or
              --   cleared (Escape, invalidation) — distinct from the
              --   pre-existing text-focus 'LuaUIFocusLost'. Lets a
              --   widget module render a focus indicator; the engine
              --   itself already owns the focus state this reports.
            | LuaUIStepEvent ElementHandle Int
              -- ^ #745: arrow-key step on a steppable control (a
              --   slider) that holds keyboard control focus. Direction
              --   is +1/-1; magnitude of one step is the widget's own
              --   concern.
            | LuaUIScrollEvent ElementHandle Double Double Bool
              -- ^ Element, deltas, and whether Shift was held (#744) —
              --   lets 'uiManager.onUIScroll' and any future UI scroll
              --   handler distinguish modified from unmodified wheel
              --   input, the same way ordinary wheel and Shift-wheel
              --   now share one routing decision
              --   ('Engine.Input.Thread.Scroll.dispatchScrollEvent').
            | LuaUICharInput Char
            | LuaUIBackspace
            | LuaUIDelete
            | LuaUISubmit
            | LuaUIEscape
            | LuaUICursorLeft
            | LuaUICursorRight
            | LuaUIHome
            | LuaUIEnd
            | LuaUIFocusLost
            | LuaDebugShow
            | LuaDebugHide
            | LuaDebugToggle
            | LuaWorldGenLog Text
              -- | A save finished loading on the world thread. Emitted
              --   once after units + buildings are written back, so by
              --   the time the Lua thread processes it the engine entity
              --   set is authoritative. Lets per-id Lua modules
              --   (unit_ai, building_spawn) reconcile their state against
              --   the entities that actually survived the load — orphan
              --   units/buildings whose defs were dropped leave no live
              --   entity, so their stale per-id state must be pruned or a
              --   reused id would inherit it (#195). Carries the loaded
              --   page's surviving unit ids and building ids. The Lua side
              --   rebuilds each singleton table as "survivors restored from
              --   the blob + every other still-live (off-page) entity's
              --   pre-load state", so a load touches only loaded-page state
              --   and other live pages are untouched (#191); nested refs
              --   are scrubbed against the survivor set, and (issue
              --   #1589) per-page ones against the trailing context.
            | LuaSaveLoaded Int [Int] [Int] LoadReconcileContext
              -- ^ The leading 'Int' is the load
              --   transaction's request id, so the dispatcher can
              --   report 'Engine.Load.Status.LoadPublished' only once
              --   THIS broadcast (below) actually completes. The
              --   trailing 'World.Save.Payload.LoadReconcileContext'
              --   (issue #1589) carries what the two survivor lists
              --   cannot: session item instances, unit-to-page
              --   ownership, and the PER-PAGE craft-bill and
              --   ground-item id sets. Without it a Lua reconcile
              --   could only ask the ACTIVE page about a per-page id,
              --   which is exactly the wrong-entity match
              --   'World.Save.Integrity.luaEdgeResolves' refuses to
              --   make on the Haskell side.
            | LuaHudLogInfo Text Text Text
              -- ^ HUD info-panel push: basic, advanced, and a SOURCE
              -- kind ("tile" | "chunk"). The kind lets entity-info
              -- watchers (unit/building/item panels) tell a real
              -- zoomed-in tile selection apart from a zoom-map chunk
              -- selection, which share this same broadcast (issue #133).
            | LuaHudLogWeatherInfo Text
            | LuaHudLogResourcesInfo Text
            | LuaWorldPreviewReady Int Word64
              -- ^ handle, generation. Issue #763: the
              -- generation is validated at DELIVERY time
              -- ('Engine.Scripting.Lua.Thread.Dispatch', gated behind
              -- the same save-barrier owner gate every Lua-thread
              -- message is), not at upload-completion time — see
              -- 'Engine.Core.State.worldPreviewGenerationRef'.
            | LuaShowPopup Text Text Float Float Float Float
                           (Maybe (Int, Int)) (Maybe Text)
              -- ^ Player-events popup. Fields, in order:
              --     1. category id (e.g. "save_load")
              --     2. body text
              --     3-6. text color r,g,b,a
              --     7. optional (gx, gy) grid coords. When present the
              --        popup line is clickable; 'Nothing' leaves the
              --        line non-clickable.
              --     8. optional source world page — the raw
              --        'World.Page.Types.WorldPageId' text field 7's
              --        coordinates are indexed in
              --        ('Engine.PlayerEvent.peSourcePage', #1588).
              --        Carried alongside the coords precisely because a
              --        popup outlives the emit instant: the click
              --        refuses to pan unless this page is still the
              --        active one, so 'Nothing' (no world at emit time)
              --        is a non-panning line, never "wherever the
              --        player happens to be now".
            | LuaLoadStaged Int
              -- ^ Issue #763 (save-overhaul C2): the world thread just
              --   finished STAGING a whole-session load transaction
              --   (its request id) without touching any live ref. The
              --   Lua thread is the one that drives the publish barrier
              --   (see "Engine.Scripting.Lua.Thread.Dispatch") — it
              --   applies the prepared Lua-side state and queues the
              --   matching 'World.Command.Types.WorldLoadPublish' once
              --   every other state-owner thread has quiesced, mirroring
              --   how 'engine.saveWorld' drives the save barrier.
            | LuaLoadStagingFailed Int
              -- ^ Staging (the world thread, off to the
              --   side of any live ref) FAILED for this request id
              --   before ever reaching 'LuaLoadStaged' — a staging
              --   exception or 'World.Load.Stage.StageError'. By this
              --   point 'Engine.Scripting.Lua.API.Save.Bridge.prepareLuaLoad'
              --   already succeeded (staging only ever runs after it
              --   does), leaving Lua's registration guard
              --   (@saveModules._loadActive@) active with no
              --   'LuaLoadStaged' ever coming to drive
              --   'Engine.Scripting.Lua.API.Save.Bridge.applyLuaLoad' (the only
              --   other thing that clears it) — so this tells the Lua
              --   thread to call
              --   'Engine.Scripting.Lua.API.Save.Bridge.abortLuaLoad' instead.
            deriving (Eq, Show)

data LuaResult = LuaSuccess
               | LuaError String
               | LuaNoop
               deriving (Eq, Show)
