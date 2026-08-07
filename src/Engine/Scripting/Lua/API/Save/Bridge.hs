{-# LANGUAGE Strict #-}
-- | The @scripts.lib.save_modules@ bridge (issue #761): every HsLua call
--   the save/load transaction makes into Lua-owned persistent state, plus
--   the stack readers that decode what those calls return. Split out of
--   "Engine.Scripting.Lua.API.Save" by issue #985: every definition here
--   takes a 'LoggerState' or nothing, so this module stays outside the
--   save/load path's permanent full-access exception
--   (@docs\/engineenv_capability_inventory.md@ §6.1).
module Engine.Scripting.Lua.API.Save.Bridge
    ( describeLuaComponents
    , collectLuaComponents
    , prepareLuaLoad
    , applyLuaLoad
    , abortLuaLoad
    ) where

import UPrelude
import qualified HsLua as Lua
import qualified Data.ByteString as BS
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Engine.Core.Log (LogCategory(..), LoggerState, logWarn)
import World.Page.Types (WorldPageId(..))
import World.Save.Integrity (KnownEntities(..))
import World.Save.Payload (LuaComponentSpec(..), LuaRefEdge(..))

-- | Pop the Lua error message at the top of the stack and log it
--   via the engine logger. Used by every save_modules.* bridge call
--   below to surface pcall failures that would otherwise be silent.
luaLogPcallError ∷ LoggerState → Text → Lua.LuaE Lua.Exception ()
luaLogPcallError logger ctx = do
    err ← Lua.tostring (-1)
    Lua.pop 1
    Lua.liftIO $ logWarn logger CatLua $
        ctx <> ": " <> maybe "<no message>" TE.decodeUtf8Lenient err

-- | require("scripts.lib.save_modules") and call one of its functions,
--   pushing the arguments @pushArgs@ leaves on the stack (must push
--   exactly @nargs@ values, in order) and requesting exactly ONE Lua
--   return value. On success ('True'), that one value is left on the
--   stack top for the caller to read and pop; on any failure (require
--   failing, the function missing/not-a-function, or the call itself
--   crashing), nothing is left on the stack and 'False' is returned,
--   having already logged the reason via the engine logger.
callSaveModules1
    ∷ LoggerState → Text → Lua.NumArgs → Lua.LuaE Lua.Exception ()
    → Lua.LuaE Lua.Exception Bool
callSaveModules1 logger fnName nargs pushArgs = do
    _ ← Lua.getglobal "require"
    Lua.pushstring "scripts.lib.save_modules"
    requireStatus ← Lua.pcall 1 1 Nothing
    case requireStatus of
        Lua.OK → do
            _ ← Lua.getfield (-1) (Lua.Name (TE.encodeUtf8 fnName))
            isFun ← Lua.isfunction (-1)
            if not isFun
                then do
                    Lua.pop 2  -- non-function value + module table
                    Lua.liftIO $ logWarn logger CatLua $
                        "save_modules." <> fnName <> " is not a function"
                    return False
                else do
                    pushArgs
                    callStatus ← Lua.pcall nargs 1 Nothing
                    case callStatus of
                        Lua.OK → do
                            Lua.remove (-2)  -- drop the module table, keep the 1 result
                            return True
                        _ → do
                            luaLogPcallError logger
                                ("save_modules." <> fnName <> " crashed")
                            Lua.pop 1  -- module table
                            return False
        _ → do
            luaLogPcallError logger
                "require scripts.lib.save_modules failed"
            return False

-- | Same as 'callSaveModules1', but for a call with no arguments and no
--   return value the caller cares about (@applyAll@).
callSaveModules0 ∷ LoggerState → Text → Lua.LuaE Lua.Exception Bool
callSaveModules0 logger fnName = do
    _ ← Lua.getglobal "require"
    Lua.pushstring "scripts.lib.save_modules"
    requireStatus ← Lua.pcall 1 1 Nothing
    case requireStatus of
        Lua.OK → do
            _ ← Lua.getfield (-1) (Lua.Name (TE.encodeUtf8 fnName))
            isFun ← Lua.isfunction (-1)
            if not isFun
                then do
                    Lua.pop 2
                    Lua.liftIO $ logWarn logger CatLua $
                        "save_modules." <> fnName <> " is not a function"
                    return False
                else do
                    callStatus ← Lua.pcall 0 0 Nothing
                    case callStatus of
                        Lua.OK → do
                            Lua.pop 1  -- module table
                            return True
                        _ → do
                            luaLogPcallError logger
                                ("save_modules." <> fnName <> " crashed")
                            Lua.pop 1
                            return False
        _ → do
            luaLogPcallError logger
                "require scripts.lib.save_modules failed"
            return False

-- | Read every element of the Lua array at the top of the stack (NOT
--   popped) via @readElem@, which must read whatever is at the NEW
--   stack top (the current element, already pushed) and leave the
--   stack depth unchanged relative to its own entry. FAILS CLOSED
--   (issue #761): a malformed element ('Nothing') aborts
--   the whole read with 'Left' rather than being silently skipped —
--   the two callers this backs (component descriptors, snapshot
--   payloads) can each carry a REQUIRED component's own record, and a
--   value HsLua can't convert (e.g. a Lua @version@ outside Word32's
--   range) must not be able to make that component quietly vanish from
--   a save/load instead of failing it outright.
readLuaArrayAt
    ∷ Lua.LuaE Lua.Exception (Maybe a)
    → Lua.LuaE Lua.Exception (Either Text [a])
readLuaArrayAt readElem = do
    n ← Lua.rawlen (-1)
    go 1 (fromIntegral n) []
  where
    go i n acc
        | i > (n ∷ Int) = return (Right (reverse acc))
        | otherwise = do
            _ ← Lua.rawgeti (-1) (fromIntegral i)
            mv ← readElem
            Lua.pop 1
            case mv of
                Nothing → return (Left ("malformed array element at index "
                    <> T.pack (show i)))
                Just v  → go (i + 1) n (v : acc)

-- | Read {id=string, version=number, required=boolean} from the table
--   at the top of the stack.
readComponentDescriptorField
    ∷ Lua.LuaE Lua.Exception (Maybe (Text, Word32, Bool))
readComponentDescriptorField = do
    _ ← Lua.getfield (-1) "id"
    midB ← Lua.tostring (-1)
    Lua.pop 1
    _ ← Lua.getfield (-1) "version"
    mver ← Lua.tointeger (-1)
    Lua.pop 1
    _ ← Lua.getfield (-1) "required"
    req ← Lua.toboolean (-1)
    Lua.pop 1
    case (midB, mver) of
        (Just idb, Just ver) →
            return (Just (TE.decodeUtf8Lenient idb, fromIntegral ver, req))
        _ → return Nothing

-- | Call @saveModules.describeAll()@ (issue #761): every currently-
--   registered persistent Lua component's (name, version, required),
--   used to build the envelope's dynamic known/required id sets before
--   both encode and decode. Returns 'Left' on any error (require
--   failing, describeAll missing/crashing, or a malformed descriptor —
--   fails closed) — the caller decides whether that's fatal for its
--   own operation.
describeLuaComponents
    ∷ LoggerState
    → Lua.LuaE Lua.Exception (Either Text [(Text, Word32, Bool)])
describeLuaComponents logger = do
    ok ← callSaveModules1 logger "describeAll" 0 (return ())
    if not ok
      then return (Left "save_modules.describeAll() could not be called \
                         \(see engine log)")
      else do
        result ← readLuaArrayAt readComponentDescriptorField
        Lua.pop 1  -- describeAll() result array
        return $ case result of
            Right xs → Right xs
            Left err → Left ("save_modules.describeAll() returned a "
                <> "malformed component descriptor: " <> err)

-- | Read {id=string, version=number, required=boolean, payload=string}
--   from the table at the top of the stack.
readSnapshotComponentField
    ∷ Lua.LuaE Lua.Exception (Maybe LuaComponentSpec)
readSnapshotComponentField = do
    _ ← Lua.getfield (-1) "id"
    midB ← Lua.tostring (-1)
    Lua.pop 1
    _ ← Lua.getfield (-1) "version"
    mver ← Lua.tointeger (-1)
    Lua.pop 1
    _ ← Lua.getfield (-1) "required"
    req ← Lua.toboolean (-1)
    Lua.pop 1
    _ ← Lua.getfield (-1) "payload"
    mpayload ← Lua.tostring (-1)
    Lua.pop 1
    case (midB, mver, mpayload) of
        (Just idb, Just ver, Just payload) →
            return (Just LuaComponentSpec
                { lcsId       = TE.decodeUtf8Lenient idb
                , lcsVersion  = fromIntegral ver
                , lcsRequired = req
                , lcsPayload  = payload })
        _ → return Nothing

-- | Call @saveModules.snapshotAll()@ (issue #761): a REQUIRED
--   component's snapshot/encode failure aborts the WHOLE save —
--   reported as 'Left' so the caller fails the save transaction before
--   anything is queued to the world thread, rather than silently
--   continuing with partial Lua state the way the pre-#761 blob map
--   used to (the engine save no longer "still proceeds, just without
--   Lua blobs"). On success, also returns every reference edge
--   ('readReferenceEdgeField') collected on the SAME live snapshot
--   (issue #764) — the caller cross-validates these the same way the
--   load boundary does, so save and load share one integrity graph.
collectLuaComponents
    ∷ LoggerState
    → Lua.LuaE Lua.Exception
        (Either Text ([LuaComponentSpec], [LuaRefEdge]))
collectLuaComponents logger = do
    ok ← callSaveModules1 logger "snapshotAll" 0 (return ())
    if not ok
      then return (Left "save_modules.snapshotAll() could not be called \
                         \(see engine log)")
      else do
        _ ← Lua.getfield (-1) "ok"
        isOk ← Lua.toboolean (-1)
        Lua.pop 1
        result ←
            if isOk
              then do
                _ ← Lua.getfield (-1) "components"
                arrResult ← readLuaArrayAt readSnapshotComponentField
                Lua.pop 1  -- components array
                case arrResult of
                    -- Fail closed: a component record
                    -- HsLua can't fully read (e.g. an out-of-range
                    -- version) must abort the save, not vanish from
                    -- the list — dropping it here is indistinguishable
                    -- from that REQUIRED component never having
                    -- existed at all.
                    Left err → return (Left ("save_modules.snapshotAll() \
                        \returned a malformed component record: " <> err))
                    Right xs → do
                        _ ← Lua.getfield (-1) "references"
                        refResult ← readLuaArrayAt readReferenceEdgeField
                        Lua.pop 1  -- references array
                        return (Right (xs, either (const []) id refResult))
              else do
                _ ← Lua.getfield (-1) "error"
                merr ← Lua.tostring (-1)
                Lua.pop 1
                return (Left (maybe "unknown save_modules.snapshotAll() \
                                    \error" TE.decodeUtf8Lenient merr))
        Lua.pop 1  -- snapshotAll() result table
        return result

-- | Push a Lua array of {id=, version=, payload=} tables from a list of
--   (name, version, payload) — the shape @saveModules.prepareLoad@
--   expects.
pushComponentsArray
    ∷ [(Text, Word32, BS.ByteString)] → Lua.LuaE Lua.Exception ()
pushComponentsArray xs = do
    Lua.newtable
    forM_ (zip [1..] xs) $ \(i, (name, ver, payload)) → do
        Lua.newtable
        Lua.pushstring (TE.encodeUtf8 name)
        Lua.setfield (-2) "id"
        Lua.pushinteger (fromIntegral ver)
        Lua.setfield (-2) "version"
        Lua.pushstring payload
        Lua.setfield (-2) "payload"
        Lua.rawseti (-2) i

-- | Push @save_modules.prepareLoad@'s @restoredEntities@ argument
--   (issue #900): the entity context of the session being loaded, so each
--   Lua component's @apply()@ can resolve its rows' ownership instead of
--   clobbering its whole singleton and reconciling afterward.
--
--   Shape — keyed by the same reference-KIND vocabulary the
--   @references()@ hooks and 'World.Save.Integrity' already speak, so a
--   component names the set it resolves against with the same string it
--   already tags its edges with:
--
-- > { unit     = { [uid] = true, ... },
-- >   building = { [bid] = true, ... },
-- >   unitPage = { [uid] = "<page id>", ... } }
--
--   @unit@/@building@ are SESSION-GLOBAL id sets — 'UnitId'/'BuildingId'
--   are single-counter allocators for the whole session, which is why
--   rows stay keyed by the global id and are never re-keyed per page.
--   @unitPage@ exists for the one thing that global id can't answer: a
--   nested reference to a PER-PAGE allocator (a craft bill, a ground
--   item) is only meaningful relative to its owning unit's page, so a
--   component resolving one during apply looks the page up here rather
--   than guessing session-wide — the same @keUnitPage@ indirection
--   'World.Save.Integrity.luaEdgeResolves' already uses for those kinds.
pushRestoredEntities ∷ KnownEntities → Lua.LuaE Lua.Exception ()
pushRestoredEntities ke = do
    Lua.newtable
    pushIdSet (keUnits ke)
    Lua.setfield (-2) "unit"
    pushIdSet (keBuildings ke)
    Lua.setfield (-2) "building"
    pushUnitPages (keUnitPage ke)
    Lua.setfield (-2) "unitPage"
  where
    pushIdSet s = do
        Lua.newtable
        forM_ (HS.toList s) $ \i → do
            Lua.pushboolean True
            Lua.rawseti (-2) (fromIntegral i)
    pushUnitPages m = do
        Lua.newtable
        forM_ (HM.toList m) $ \(uid, WorldPageId pid) → do
            Lua.pushstring (TE.encodeUtf8 pid)
            Lua.rawseti (-2) (fromIntegral uid)

readErrorStringField ∷ Lua.LuaE Lua.Exception (Maybe Text)
readErrorStringField = do
    ty ← Lua.ltype (-1)
    if ty ≡ Lua.TypeString
        then (TE.decodeUtf8Lenient ⊚) ⊚ Lua.tostring (-1)
        else return Nothing

-- | Read {component=string, kind=string, id=number, owner=number|nil,
--   path=string|nil} from the table at the top of the stack — one entry
--   of @save_modules.lua@'s @prepareLoad@/@snapshotAll@ @references@
--   result array (issue #764, save-overhaul C3): a single reference edge
--   a Lua component's @references()@ hook reported. @owner@ (the owning
--   unit id, when the hook supplied one) and @path@ (the source field
--   path — e.g. "unit[7].attackTargetUid") are both
--   optional diagnostics-only fields: 'Nothing'/empty when absent or
--   not the expected type, never a reason to drop the whole edge the
--   way a malformed @component@/@kind@/@id@ does.
--
--   @page@ (#915) is optional in the same shape-tolerant sense, but for
--   the one kind that declares it (@location_instance@) it is NOT merely
--   diagnostic: a per-page instance id names nothing on its own, so an
--   edge missing its page resolves against nothing — see
--   'World.Save.Integrity.luaEdgeResolves'.
readReferenceEdgeField ∷ Lua.LuaE Lua.Exception (Maybe LuaRefEdge)
readReferenceEdgeField = do
    _ ← Lua.getfield (-1) "component"
    mcompB ← Lua.tostring (-1)
    Lua.pop 1
    _ ← Lua.getfield (-1) "kind"
    mkindB ← Lua.tostring (-1)
    Lua.pop 1
    _ ← Lua.getfield (-1) "id"
    mid ← Lua.tointeger (-1)
    Lua.pop 1
    _ ← Lua.getfield (-1) "owner"
    mowner ← Lua.tointeger (-1)
    Lua.pop 1
    _ ← Lua.getfield (-1) "path"
    mpathB ← Lua.tostring (-1)
    Lua.pop 1
    _ ← Lua.getfield (-1) "page"
    mpageB ← Lua.tostring (-1)
    Lua.pop 1
    case (mcompB, mkindB, mid) of
        (Just compB, Just kindB, Just i) →
            return (Just LuaRefEdge
                { lreComponent = TE.decodeUtf8Lenient compB
                , lreKind      = TE.decodeUtf8Lenient kindB
                , lreId        = fromIntegral i
                , lreOwner     = fromIntegral ⊚ mowner
                , lrePath      = maybe "" TE.decodeUtf8Lenient mpathB
                , lrePage      = TE.decodeUtf8Lenient ⊚ mpageB })
        _ → return Nothing

-- | Call @saveModules.prepareLoad(components, requestId, isMigrating)@
--   (issue #761; the third argument added by issue #766, save-overhaul
--   C4): decode + migrate + component-locally-validate EVERY registered
--   Lua component with NO live mutation (requirement 11). Any failure —
--   a require/call failure, or a reported validation error — aborts the
--   load; nothing has touched live Lua state yet either way. @requestId@
--   is stashed on the Lua side alongside the prepared data so a later
--   'abortLuaLoad' for a DIFFERENT, stale request can't clear it (see
--   'abortLuaLoad'). @isMigratingLegacyBaseline@ is
--   'True' only for a recognized pre-#760 compatibility migration
--   (always empty @components@) — every currently-required module then
--   gets its own empty-state default (via @reg.decode(reg.version,
--   nil)@, which every registered persistent component already
--   tolerates) instead of failing on "missing", since every one of them
--   post-dates that baseline (requirement 7: an honest default, not a
--   guess).
-- | On success, also returns every reference edge
--   ('readReferenceEdgeField') the just-prepared components' @references()@
--   hooks reported (issue #764) — "Engine.Scripting.Lua.API.Save"'s
--   @continueLoad@ cross-validates these against the loaded session's
--   real entity sets. A malformed entry in that array degrades to being
--   dropped (best-effort diagnostics only; this never gates the load —
--   see "World.Save.Integrity"'s haddock).
--   The 'KnownEntities' argument (issue #900) is the restored session's
--   own entity context, pushed as @prepareLoad@'s fourth argument and
--   stashed there for the LATER @applyAll@ call — see
--   'pushRestoredEntities'. It rides along with the prepare rather than
--   the apply because the two are separated by a world-thread staging
--   round trip ('Engine.Scripting.Lua.Thread.Dispatch.handleLoadStaged'
--   is reached via a @LuaLoadStaged@ message carrying only a request id),
--   and @save_modules@ already spans exactly that gap for the prepared
--   payloads and the request id itself. So no new carrier — Haskell-side
--   or otherwise — has to exist for it.
prepareLuaLoad
    ∷ LoggerState → Int → [(Text, Word32, BS.ByteString)] → Bool
    → KnownEntities
    → Lua.LuaE Lua.Exception (Either Text [LuaRefEdge])
prepareLuaLoad logger requestId components isMigratingLegacyBaseline known = do
    ok ← callSaveModules1 logger "prepareLoad" 4
            (pushComponentsArray components
                ≫ Lua.pushinteger (fromIntegral requestId)
                ≫ Lua.pushboolean isMigratingLegacyBaseline
                ≫ pushRestoredEntities known)
    if not ok
      then return (Left "save_modules.prepareLoad() could not be called \
                         \(see engine log)")
      else do
        _ ← Lua.getfield (-1) "ok"
        isOk ← Lua.toboolean (-1)
        Lua.pop 1
        result ←
            if isOk
              then do
                _ ← Lua.getfield (-1) "references"
                refResult ← readLuaArrayAt readReferenceEdgeField
                Lua.pop 1  -- references array
                return (Right (either (const []) id refResult))
              else do
                _ ← Lua.getfield (-1) "errors"
                -- Purely a diagnostic message list here — the load is
                -- already known to be failing (isOk == False)
                -- regardless of whether every entry parses, so a
                -- malformed entry degrades to an empty list (falling
                -- into the "no error detail" message below) rather
                -- than needing its own fail-closed handling.
                arrResult ← readLuaArrayAt readErrorStringField
                Lua.pop 1
                let errs = either (const []) id arrResult
                return (Left (if null errs
                                then "save_modules.prepareLoad() failed \
                                     \(no error detail)"
                                else T.intercalate "; " errs))
        Lua.pop 1  -- prepareLoad() result table
        return result

-- | Call @saveModules.applyAll()@ (issue #761): apply the load prepared
--   by the most recent successful 'prepareLuaLoad', then run every
--   registered reset hook. Only reachable after 'prepareLuaLoad'
--   returned 'Right', so a failure here is a genuine apply()/reset-hook
--   bug rather than a data problem — but it must still be REPORTED
--   (never warning-only, requirement 6): the caller
--   ('Engine.Scripting.Lua.Thread.Dispatch.handleLoadStaged') aborts the
--   whole load rather than queuing the Haskell-side restore on top of a
--   Lua state that only partially applied.
applyLuaLoad ∷ LoggerState → Lua.LuaE Lua.Exception (Either Text ())
applyLuaLoad logger = do
    ok ← callSaveModules0 logger "applyAll"
    return $ if ok then Right ()
             else Left "save_modules.applyAll() failed (see engine log)"

-- | Call @saveModules.abortPreparedLoad(requestId)@:
--   every failure path that can occur AFTER a successful 'prepareLuaLoad'
--   but BEFORE 'applyLuaLoad' ever runs (a staging exception/'StageError'
--   on the world thread, or the publish barrier itself failing/timing
--   out — see 'World.Thread.Command.Save.handleWorldLoadTransactionCommand'
--   and 'Engine.Scripting.Lua.Thread.Dispatch.handleLoadStaged') must
--   call this so Lua's registry-mutation guard
--   (@saveModules.register@/@registerResetHook@ refusing to run while
--   @_loadActive@) doesn't stay wedged open for the rest of the
--   session. Best-effort: a failure here is only logged, never
--   propagated, since it always runs FROM an existing failure path that
--   must still report ITS OWN error regardless.
--
--   The world-thread-queued failure path
--   ('LuaLoadStagingFailed') reaches its caller as a QUEUED Lua message,
--   which can sit unprocessed for a while — long enough for the failing
--   request to already be terminal
--   ('Engine.Load.Status.failLoad') and a BRAND NEW request to have been
--   accepted and successfully run its OWN 'prepareLuaLoad' before the
--   stale message is finally handled. Passing @requestId@ through lets
--   @save_modules.abortPreparedLoad@ compare it against whatever it most
--   recently stashed and no-op when they don't match, so a stale abort
--   can never clear a newer, still-in-flight request's prepared state.
abortLuaLoad ∷ LoggerState → Int → Lua.LuaE Lua.Exception ()
abortLuaLoad logger requestId = do
    ok ← callSaveModules1 logger "abortPreparedLoad" 1
            (Lua.pushinteger (fromIntegral requestId))
    if ok
      then Lua.pop 1  -- discard abortPreparedLoad()'s (nil) return value
      else Lua.liftIO $ logWarn logger CatLua
            "save_modules.abortPreparedLoad() failed (see engine log) -- \
            \a prepared load may remain stuck active"
