{-# LANGUAGE TypeApplications #-}
-- | @scripts/lib/save_modules.lua@'s registration, lifecycle,
--   rollback and Haskell-bridge contracts -- one of the four owners
--   'Test.Headless.Lua.SaveModules' composes (issue #2047).
--
--   Three groups: the registry itself (issue #761 requirements 2/3/4,
--   including issue #1277's mirror of the authoritative Haskell
--   component ids), rollback double faults (issue #1200), and what
--   the REAL 'Engine.Scripting.Lua.API.Save.Bridge.applyLuaLoad'
--   reports back to @engine.getLoadStatus()@ (issue #1200
--   requirement 3).
--
--   Its domain fixtures live here with it: 'validSpecLua' and the
--   authoritative-id payload for the registry cases,
--   'rollbackFixtureLua' and 'applyViaBridge' for the rollback and
--   bridge cases.
module Test.Headless.Lua.SaveModules.Registry (spec) where

import UPrelude
import Test.Hspec
import qualified HsLua as Lua
import qualified Data.ByteString as BS
import qualified Data.HashSet as HS
import qualified Data.List as L
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import System.IO (stderr)

import Engine.Core.Log
    (initLogger, defaultLogConfig, LogConfig(..), LogBackend(..))
import Engine.Scripting.Lua.API.Save.Bridge (applyLuaLoad)
import World.Save.Component (componentKnownIds)
import World.Save.Component.Types (metadataComponentId)
import World.Save.Envelope.Types (ComponentId(..))

import Test.Headless.Lua.SaveModules.Support
    (engineStub, lns, runsOk, runsOkWithPayloads)

-- | The authoritative set of Haskell-owned top-level component ids that
--   @scripts/lib/save_modules.lua@'s hand-kept @HASKELL_COMPONENT_IDS@
--   mirror must equal, sorted (issue #1277).
--
--   It is 'metadataComponentId' plus 'componentKnownIds' -- equivalently
--   @"metadata"@ plus every id in 'World.Save.Component.saveComponentRegistry',
--   since 'componentKnownIds' is mechanically derived from that registry.
--   The metadata component is deliberately outside the gameplay registry
--   (it is the envelope layer's own), which is why the union is the
--   authority here and 'componentKnownIds' alone is not:
--   'World.Save.Envelope.knownComponentIds' unions exactly these two the
--   same way when it builds the reader's real known-id set.
--
--   Reading it from the live registry rather than restating a literal
--   list is the whole point: a component registered on the Haskell side
--   lands here automatically, so the mirror spec below fails until the
--   Lua table is updated to match.
authoritativeHaskellComponentIds ∷ [Text]
authoritativeHaskellComponentIds =
    L.sort
        [ t | ComponentId t
              ← HS.toList (HS.insert metadataComponentId componentKnownIds) ]

-- | 'authoritativeHaskellComponentIds' as one newline-separated blob,
--   ready to push as a Lua string global via 'runsOkWithPayloads' (no id
--   contains a newline -- they are lowercase-and-hyphen literals).
authoritativeIdsPayload ∷ BS.ByteString
authoritativeIdsPayload =
    TE.encodeUtf8 (T.intercalate "\n" authoritativeHaskellComponentIds)

-- | A complete, valid persistent-component spec table literal, as Lua
--   source text, parameterised by id -- the shortest well-formed
--   registration every "valid" test builds on.
validSpecLua ∷ Text → Text
validSpecLua ident = lns
    [ "{ version = 1, inputVersions = {1}, required = true, scope = 'global', deps = {},"
    , "  snapshot = function() return { x = 1 } end,"
    , "  decode = function(v, d) return d end,"
    , "  validate = function(d) return nil end,"
    , "  apply = function(d) _G['" <> ident <> "_applied'] = d end }"
    ]

-- | Issue #1200: drive the REAL Haskell bridge
--   ('Engine.Scripting.Lua.API.Save.Bridge.applyLuaLoad') against a
--   standalone VM whose registry @setupChunk@ has already registered and
--   prepared, returning exactly the 'Either' the load path receives. A
--   Lua-only @pcall(saveModules.applyAll)@ assertion cannot cover
--   requirement 3: the bridge is precisely where the Lua diagnostic used
--   to be replaced by a fixed \"see engine log\" string, so only calling
--   it proves what actually reaches @engine.getLoadStatus()@ and the
--   @CatWorld@ warning —
--   'Engine.Scripting.Lua.Thread.Dispatch.handleLoadStaged' hands this
--   text to both verbatim.
applyViaBridge ∷ Text → IO (Either Text ())
applyViaBridge setupChunk = do
    logger ← initLogger defaultLogConfig { lcBackend = LogToHandle stderr }
    Lua.run @Lua.Exception $ do
        Lua.openlibs
        _ ← Lua.dostring (TE.encodeUtf8 engineStub)
        status ← Lua.dostring (TE.encodeUtf8 setupChunk)
        case status of
            Lua.OK → applyLuaLoad logger
            _ → do
                err ← Lua.tostring (-1)
                Lua.pop 1
                return (Left ("test setup chunk failed: "
                    <> maybe "<no message>" TE.decodeUtf8Lenient err))

-- | Issue #1200: three components whose rollback restores can be ARMED
--   to throw. Each @apply@ counts its own calls, so a component can
--   succeed on the forward pass (call 1) and fail on its rollback
--   restore (call 2) -- the double fault this gate exists for -- while a
--   LATER successful load (call 3+) still recovers, which is what keeps
--   #864's recovery contract testable across the same fixture. A restore
--   that throws does so BEFORE writing its live value, so the live
--   singleton keeps the NEW session's row: that is the mixed state, and
--   the tests assert it directly rather than inferring it.
rollbackFixtureLua ∷ Text
rollbackFixtureLua = lns
    [ "local codec = require('scripts.lib.data_codec')"
    , "local live = { a='old-a', b='old-b', c='old-c' }"
    , "local calls = { a=0, b=0, c=0 }"
    , "local armed = {}"
    , "local function comp(key)"
    , "  return { version=1, inputVersions={1}, required=true,"
    , "    scope='global', deps={},"
    , "    snapshot = function() return { v = live[key] } end,"
    , "    decode = function(v, d) return d end,"
    , "    validate = function(d) return nil end,"
    , "    apply = function(d)"
    , "      calls[key] = calls[key] + 1"
    , "      if armed[key] == calls[key] then"
    , "        error('RESTORE_FAIL_' .. key)"
    , "      end"
    , "      live[key] = d.v"
    , "    end }"
    , "end"
    , "_G.live, _G.calls, _G.armed, _G.comp = live, calls, armed, comp"
    , "_G.payloadFor = function(v) return codec.encode({ v = v }) end"
    ]

spec ∷ Spec
spec = do
    describe "save_modules registry (issue #761 requirements 2/3/4)" $ do
        it "accepts a valid required registration and a valid optional \
           \registration" $ runsOk $ lns
            [ "local saveModules = require('scripts.lib.save_modules')"
            , "saveModules.register('t_valid_required', " <> validSpecLua "t_valid_required" <> ")"
            , "saveModules.register('t_valid_optional', {"
            , "  version = 1, inputVersions = {1}, required = false, scope = 'global',"
            , "  deps = {},"
            , "  snapshot = function() return { x = 1 } end,"
            , "  decode = function(v, d) return d end,"
            , "  validate = function(d) return nil end,"
            , "  apply = function(d) end,"
            , "  default = function() return { x = 0 } end })"
            ]

        it "rejects a duplicate component id" $ runsOk $ lns
            [ "local saveModules = require('scripts.lib.save_modules')"
            , "saveModules.register('t_dup', " <> validSpecLua "t_dup" <> ")"
            , "local ok = pcall(saveModules.register, 't_dup', " <> validSpecLua "t_dup" <> ")"
            , "assert(not ok, 'duplicate id should fail')"
            ]

        it "rejects an invalid identifier" $ runsOk $ lns
            [ "local saveModules = require('scripts.lib.save_modules')"
            , "local ok = pcall(saveModules.register, 'Bad-Id!', " <> validSpecLua "bad" <> ")"
            , "assert(not ok, 'invalid id should fail')"
            ]

        it "rejects an invalid version (zero, negative, non-integer)" $
            runsOk $ lns
            [ "local saveModules = require('scripts.lib.save_modules')"
            , "local mk = function(v) return { version = v, required = true,"
            , "  snapshot = function() end, decode = function() end,"
            , "  validate = function() end, apply = function() end } end"
            , "assert(not pcall(saveModules.register, 't_v0', mk(0)))"
            , "assert(not pcall(saveModules.register, 't_vneg', mk(-1)))"
            , "assert(not pcall(saveModules.register, 't_vfrac', mk(1.5)))"
            ]

        it "rejects a registration with no inputVersions declared at all \
           \(issue #761 round-5 review) -- requirement 2 requires every \
           \persistent component to explicitly list every schema version \
           \its decode() can still read; silently defaulting to just the \
           \current version would hide that nothing was ever declared" $
            runsOk $ lns
            [ "local saveModules = require('scripts.lib.save_modules')"
            , "local ok = pcall(saveModules.register, 't_no_input_versions', {"
            , "  version = 1, required = true, scope = 'global', deps = {},"
            , "  snapshot = function() end, decode = function() end,"
            , "  validate = function() end, apply = function() end })"
            , "assert(not ok, 'a registration with no inputVersions field \
                             \must be rejected, not defaulted')"
            , "local ok2 = pcall(saveModules.register, 't_empty_input_versions', {"
            , "  version = 1, inputVersions = {}, required = true,"
            , "  scope = 'global', deps = {},"
            , "  snapshot = function() end, decode = function() end,"
            , "  validate = function() end, apply = function() end })"
            , "assert(not ok2, 'an explicit but empty inputVersions must \
                              \also be rejected')"
            ]

        it "rejects an inputVersions table that is not a genuine dense \
           \array (issue #761 round-8 review) -- e.g. {1, [3] = 2} \
           \previously registered successfully and then silently dropped \
           \version 2 everywhere inputVersions is consumed via ipairs \
           \(isVersionSupported), exactly the deps bug fixed in round 7" $
            runsOk $ lns
            [ "local saveModules = require('scripts.lib.save_modules')"
            , "local function mk(iv) return { version = 1, inputVersions = iv,"
            , "  required = true, scope = 'global', deps = {},"
            , "  snapshot = function() end, decode = function() end,"
            , "  validate = function() end, apply = function() end } end"
            , "local ok1 = pcall(saveModules.register, 't_iv_sparse',"
            , "  mk({ 1, [3] = 2 }))"
            , "assert(not ok1, 'a sparse inputVersions table must be rejected')"
            , "local ok2 = pcall(saveModules.register, 't_iv_assoc',"
            , "  mk({ current = 1 }))"
            , "assert(not ok2, 'an associative-shaped inputVersions table \
                              \must be rejected')"
            , "local ok3 = pcall(saveModules.register, 't_iv_ok', mk({ 1 }))"
            , "assert(ok3, 'a genuine dense array of versions must still register')"
            ]

        it "rejects a registration with no deps declared at all (issue \
           \#761 round-6 review) -- requirement 2 requires every \
           \persistent component to explicitly declare its dependencies, \
           \possibly as an empty list, rather than silently defaulting a \
           \missing field to {}" $ runsOk $ lns
            [ "local saveModules = require('scripts.lib.save_modules')"
            , "local ok = pcall(saveModules.register, 't_no_deps', {"
            , "  version = 1, inputVersions = {1}, required = true, scope = 'global',"
            , "  snapshot = function() end, decode = function() end,"
            , "  validate = function() end, apply = function() end })"
            , "assert(not ok, 'a registration with no deps field must be \
                             \rejected, not defaulted to {}')"
            ]

        it "rejects a deps table that is not a genuine dense array (issue \
           \#761 round-7 review) -- an associative/sparse table (e.g. a \
           \typo'd {hibernate = 'core-session'} instead of {'core-session'}) \
           \was previously accepted at registration and then silently \
           \skipped everywhere deps is consumed via ipairs, reporting zero \
           \static errors for a dependency that was never actually \
           \declared" $ runsOk $ lns
            [ "local saveModules = require('scripts.lib.save_modules')"
            , "local function mk(deps) return { version = 1, inputVersions = {1},"
            , "  required = true, scope = 'global', deps = deps,"
            , "  snapshot = function() end, decode = function() end,"
            , "  validate = function() end, apply = function() end } end"
            , "local ok1 = pcall(saveModules.register, 't_deps_assoc',"
            , "  mk({ hibernate = 'core-session' }))"
            , "assert(not ok1, 'an associative-shaped deps table must be rejected')"
            , "local ok2 = pcall(saveModules.register, 't_deps_sparse',"
            , "  mk({ [1] = 'core-session', [3] = 'units' }))"
            , "assert(not ok2, 'a sparse deps table must be rejected')"
            , "local ok3 = pcall(saveModules.register, 't_deps_nonstring',"
            , "  mk({ 42 }))"
            , "assert(not ok3, 'a deps entry that is not a string must be rejected')"
            , "local ok4 = pcall(saveModules.register, 't_deps_ok', mk({ 'core-session' }))"
            , "assert(ok4, 'a genuine dense array of string ids must still register')"
            ]

        it "accepts a Lua component's dependency on a known Haskell \
           \component id (issue #761 requirement 2's \"dependencies on \
           \Haskell or Lua components\" -- round-7 review correction: an \
           \earlier round wrongly rejected every Haskell id as \
           \\"unregistered\"), while still rejecting one that names \
           \neither a registered Lua id nor a real Haskell one" $
            runsOk $ lns
            [ "local saveModules = require('scripts.lib.save_modules')"
            , "saveModules.register('t_depends_on_units', {"
            , "  version = 1, inputVersions = {1}, required = true, scope = 'global',"
            , "  deps = { 'units' },"
            , "  snapshot = function() end, decode = function() end,"
            , "  validate = function() end, apply = function() end })"
            , "local errs1 = saveModules.registryStaticErrors()"
            , "assert(#errs1 == 0, 'a dependency on the known Haskell units "
              <> "component must not be reported as unregistered: ' "
              <> ".. table.concat(errs1, '; '))"
            , "saveModules.register('t_depends_on_nothing', {"
            , "  version = 1, inputVersions = {1}, required = true, scope = 'global',"
            , "  deps = { 'not_a_real_component_anywhere' },"
            , "  snapshot = function() end, decode = function() end,"
            , "  validate = function() end, apply = function() end })"
            , "local errs2 = saveModules.registryStaticErrors()"
            , "local found = false"
            , "for _, e in ipairs(errs2) do"
            , "  if e:find('not_a_real_component_anywhere') then found = true end"
            , "end"
            , "assert(found, 'a dependency naming neither a Lua nor a "
              <> "Haskell component must still be rejected')"
            ]

        it "the hand-kept Haskell-component mirror equals the \
           \authoritative Haskell registry exactly, in BOTH directions \
           \(issue #1277) -- hand-keeping alone already failed once: \
           \#1087/PR #1126 registered 'container-knowledge' on the \
           \Haskell side without adding it here, so a Lua component \
           \declaring that real dependency was rejected as depending on \
           \an unregistered component, which fails every save/load. This \
           \compares the ids HASKELL_COMPONENT_IDS actually accepts \
           \against metadataComponentId + componentKnownIds, naming every \
           \id missing from the mirror and every stale one left in it" $
            runsOkWithPayloads
                [("AUTHORITATIVE_IDS", authoritativeIdsPayload)] $ lns
            [ "local saveModules = require('scripts.lib.save_modules')"
            , "local authoritative = {}"
            , "local authoritativeCount = 0"
            , "for id in AUTHORITATIVE_IDS:gmatch('[^\\n]+') do"
            , "  authoritative[id] = true"
            , "  authoritativeCount = authoritativeCount + 1"
            , "end"
            , "assert(authoritativeCount > 0, 'the authoritative Haskell "
              <> "component set must not be empty -- the payload never "
              <> "reached this chunk')"
            , "local mirrored = {}"
            , "for _, id in ipairs(saveModules.haskellComponentIds()) do"
            , "  mirrored[id] = true"
            , "end"
            , "local missing, unexpected = {}, {}"
            , "for id in pairs(authoritative) do"
            , "  if not mirrored[id] then missing[#missing + 1] = id end"
            , "end"
            , "for id in pairs(mirrored) do"
            , "  if not authoritative[id] then"
            , "    unexpected[#unexpected + 1] = id"
            , "  end"
            , "end"
            , "table.sort(missing)"
            , "table.sort(unexpected)"
            , "assert(#missing == 0 and #unexpected == 0,"
            , "  'save_modules.lua HASKELL_COMPONENT_IDS has drifted from "
              <> "the Haskell registry -- missing from the Lua mirror: [' "
            , "  .. table.concat(missing, ', ') .. ']; present in the Lua "
              <> "mirror but not in the Haskell registry: [' "
            , "  .. table.concat(unexpected, ', ') .. ']')"
            ]

        it "accepts a Lua component declaring deps = {'container-knowledge'} \
           \-- and a dependency on every other authoritative Haskell id \
           \-- while a genuinely unknown dependency is still rejected \
           \(issue #1277). Goes through registryStaticErrors() rather \
           \than the mirror accessor, so acceptance is proven on the real \
           \validation path the save/load boundary runs" $
            runsOkWithPayloads
                [("AUTHORITATIVE_IDS", authoritativeIdsPayload)] $ lns
            [ "local saveModules = require('scripts.lib.save_modules')"
            , "local function mk(dep) return { version = 1,"
            , "  inputVersions = {1}, required = true, scope = 'global',"
            , "  deps = { dep },"
            , "  snapshot = function() end, decode = function() end,"
            , "  validate = function() end, apply = function() end } end"
            , "saveModules.register('t_ck_dep', mk('container-knowledge'))"
            , "local errs0 = saveModules.registryStaticErrors()"
            , "assert(#errs0 == 0, \"a dependency on the real "
              <> "'container-knowledge' component must not be reported as "
              <> "unregistered: \" .. table.concat(errs0, '; '))"
            , "local sawContainerKnowledge = false"
            , "local n = 0"
            , "for id in AUTHORITATIVE_IDS:gmatch('[^\\n]+') do"
            , "  n = n + 1"
            , "  if id == 'container-knowledge' then"
            , "    sawContainerKnowledge = true"
            , "  end"
            , "  saveModules.register('t_auth_dep_' .. n, mk(id))"
            , "end"
            , "assert(sawContainerKnowledge, \"the authoritative Haskell "
              <> "registry must contain 'container-knowledge' -- if it no "
              <> "longer does, this issue's premise is gone and this "
              <> "example needs rewriting, not deleting\")"
            , "local errs1 = saveModules.registryStaticErrors()"
            , "assert(#errs1 == 0, 'no authoritative Haskell component id "
              <> "may be reported as unregistered: ' "
            , "  .. table.concat(errs1, '; '))"
            , "saveModules.register('t_auth_dep_unknown',"
            , "  mk('not_a_real_component_anywhere'))"
            , "local errs2 = saveModules.registryStaticErrors()"
            , "local found = false"
            , "for _, e in ipairs(errs2) do"
            , "  if e:find('not_a_real_component_anywhere', 1, true) then"
            , "    found = true"
            , "  end"
            , "end"
            , "assert(found, 'widening the mirror must not have made every "
              <> "unknown dependency acceptable')"
            ]

        it "rejects registration missing a required callback" $ runsOk $ lns
            [ "local saveModules = require('scripts.lib.save_modules')"
            , "local ok = pcall(saveModules.register, 't_missing_cb', {"
            , "  version = 1, required = true, snapshot = function() end })"
            , "assert(not ok, 'missing decode/validate/apply should fail')"
            ]

        it "rejects an optional registration with no default()" $ runsOk $ lns
            [ "local saveModules = require('scripts.lib.save_modules')"
            , "local ok = pcall(saveModules.register, 't_no_default', {"
            , "  version = 1, required = false,"
            , "  snapshot = function() end, decode = function() end,"
            , "  validate = function() end, apply = function() end })"
            , "assert(not ok, 'optional without default() should fail')"
            ]

        it "keeps persistent components, reset hooks, and their id \
           \namespace distinct -- a reset hook id colliding with a \
           \persistent id is rejected, and vice versa" $ runsOk $ lns
            [ "local saveModules = require('scripts.lib.save_modules')"
            , "saveModules.register('t_persistent_a', " <> validSpecLua "t_persistent_a" <> ")"
            , "local ok1 = pcall(saveModules.registerResetHook, 't_persistent_a', function() end)"
            , "assert(not ok1, 'reset hook id colliding with a persistent id should fail')"
            , "saveModules.registerResetHook('t_reset_a', function() _G.t_reset_a_ran = true end)"
            , "local ok2 = pcall(saveModules.register, 't_reset_a', " <> validSpecLua "t_reset_a" <> ")"
            , "assert(not ok2, 'persistent id colliding with a reset-hook id should fail')"
            ]

        it "reports describeAll()/snapshotAll() in canonical (id-ascending) \
           \order regardless of registration order" $ runsOk $ lns
            [ "local saveModules = require('scripts.lib.save_modules')"
            , "saveModules.register('zz_last', " <> validSpecLua "zz_last" <> ")"
            , "saveModules.register('aa_first', " <> validSpecLua "aa_first" <> ")"
            , "local desc = saveModules.describeAll()"
            , "assert(desc[1].id == 'aa_first' and desc[2].id == 'zz_last',"
            , "  'describeAll must be canonically (id-ascending) ordered')"
            ]

        it "orders dependent components after their dependencies, and \
           \rejects a dependency cycle" $ runsOk $ lns
            [ "local saveModules = require('scripts.lib.save_modules')"
            , "saveModules.register('dep_child', { version=1, inputVersions={1}, required=true, scope='global',"
            , "  deps = {'dep_parent'},"
            , "  snapshot=function() return {} end, decode=function(v,d) return d end,"
            , "  validate=function() return nil end, apply=function() end })"
            , "saveModules.register('dep_parent', { version=1, inputVersions={1}, required=true, scope='global', deps={},"
            , "  snapshot=function() return {} end, decode=function(v,d) return d end,"
            , "  validate=function() return nil end, apply=function() end })"
            , "local order = saveModules.dependencyOrder()"
            , "local posParent, posChild"
            , "for i, id in ipairs(order) do"
            , "  if id == 'dep_parent' then posParent = i end"
            , "  if id == 'dep_child' then posChild = i end"
            , "end"
            , "assert(posParent < posChild, 'dependency must precede its dependent')"
            , "saveModules.register('cyc_a', { version=1, inputVersions={1}, required=true, scope='global', deps={'cyc_b'},"
            , "  snapshot=function() return {} end, decode=function(v,d) return d end,"
            , "  validate=function() return nil end, apply=function() end })"
            , "saveModules.register('cyc_b', { version=1, inputVersions={1}, required=true, scope='global', deps={'cyc_a'},"
            , "  snapshot=function() return {} end, decode=function(v,d) return d end,"
            , "  validate=function() return nil end, apply=function() end })"
            , "local errs = saveModules.registryStaticErrors()"
            , "assert(#errs > 0, 'a dependency cycle must be reported')"
            ]

        it "runs the full snapshotAll -> prepareLoad -> applyAll round trip, \
           \and runs registered reset hooks on every apply" $ runsOk $ lns
            [ "local saveModules = require('scripts.lib.save_modules')"
            , "saveModules.register('rt_required', " <> validSpecLua "rt_required" <> ")"
            , "saveModules.registerResetHook('rt_reset', function() _G.rt_reset_ran = true end)"
            , "local snap = saveModules.snapshotAll()"
            , "assert(snap.ok, 'snapshotAll should succeed')"
            , "local prep = saveModules.prepareLoad(snap.components)"
            , "assert(prep.ok, 'prepareLoad should succeed')"
            , "saveModules.applyAll()"
            , "assert(_G.rt_required_applied.x == 1, 'apply must run with decoded data')"
            , "assert(_G.rt_reset_ran == true, 'reset hooks must run on every apply')"
            ]

        it "aborts the whole load when a required component is missing \
           \from the save (requirement 6/11)" $ runsOk $ lns
            [ "local saveModules = require('scripts.lib.save_modules')"
            , "saveModules.register('missing_required', " <> validSpecLua "missing_required" <> ")"
            , "local prep = saveModules.prepareLoad({})"
            , "assert(not prep.ok, 'a missing required component must fail the whole load')"
            , "assert(#prep.errors > 0)"
            ]

        it "aborts the whole save when a required component's snapshot \
           \fails (requirement 6)" $ runsOk $ lns
            [ "local saveModules = require('scripts.lib.save_modules')"
            , "saveModules.register('boom_required', { version=1, inputVersions={1}, required=true, scope='global', deps={},"
            , "  snapshot = function() error('synthetic snapshot failure') end,"
            , "  decode=function(v,d) return d end, validate=function() return nil end,"
            , "  apply=function() end })"
            , "local snap = saveModules.snapshotAll()"
            , "assert(not snap.ok, 'a required snapshot failure must abort the whole save')"
            ]

        it "aborts the whole save when a required component's snapshotted \
           \data fails its OWN validate() (round-6 review, issue #764) -- \
           \previously validate() ran only on the load side, so a \
           \malformed live state could snapshot, encode, and WRITE to \
           \disk untouched, only surfacing as a silently-dropped \
           \reference edge on a LATER load rather than as a save-time \
           \failure" $ runsOk $ lns
            [ "local saveModules = require('scripts.lib.save_modules')"
            , "saveModules.register('bad_data_required', { version=1, inputVersions={1}, required=true, scope='global', deps={},"
            , "  snapshot = function() return { x = 1 } end,"
            , "  decode=function(v,d) return d end,"
            , "  validate = function(data) return { 'synthetic validate failure' } end,"
            , "  apply=function() end })"
            , "local snap = saveModules.snapshotAll()"
            , "assert(not snap.ok, 'a required validate() failure must abort the whole save')"
            , "assert(string.find(snap.error, 'synthetic validate failure') ~= nil,"
            , "  'the validate() error text must surface in the save failure: ' .. tostring(snap.error))"
            ]

        it "rejects a save whose LIVE unit_ai state has been mutated into \
           \a malformed shape (round-6 review, issue #764) -- the exact \
           \attackTargetUid-corrupted-to-a-string scenario the review \
           \cited, driven through the REAL unit_ai registration rather \
           \than a synthetic component" $
            runsOk $ lns
            [ "unit = { exists = function(_uid) return true end }"
            , "craft = { get = function(_id) return nil end }"
            , "item = { listDefs = function() return {} end }"
            , "local unitAiSave = require('scripts.unit_ai_save')"
            , "local fakeAiState = { [7] = { attackTargetUid = 'not_a_number' } }"
            , "unitAiSave.register(fakeAiState)"
            , "local saveModules = require('scripts.lib.save_modules')"
            , "local snap = saveModules.snapshotAll()"
            , "assert(not snap.ok,"
            , "  'a live aiState with a non-numeric attackTargetUid must fail the save, '"
            , "  .. 'not silently write a malformed reference to disk: ' .. tostring(snap.ok))"
            ]

        it "blocks new registration while a save snapshot is in progress \
           \(requirement 3)" $ runsOk $ lns
            [ "local saveModules = require('scripts.lib.save_modules')"
            , "saveModules.register('mid_capture', { version=1, inputVersions={1}, required=true, scope='global', deps={},"
            , "  snapshot = function()"
            , "    local ok = pcall(saveModules.register, 'sneaky', " <> validSpecLua "sneaky" <> ")"
            , "    assert(not ok, 'registration during an active capture must fail')"
            , "    return {}"
            , "  end,"
            , "  decode=function(v,d) return d end, validate=function() return nil end,"
            , "  apply=function() end })"
            , "local snap = saveModules.snapshotAll()"
            , "assert(snap.ok, 'the snapshot itself must still succeed')"
            ]

        it "recovers the registry after a crashing component apply() \
           \(issue #864): the original crash marker survives in the \
           \surfaced failure, register/registerResetHook are still \
           \rejected from inside the active apply, the registry is \
           \immediately usable again with no intervening successful \
           \load, a stale re-applyAll (with no fresh prepareLoad) still \
           \fails with the single-line no-prepared-load diagnostic, and \
           \a later full round trip -- including the previously-crashed \
           \component and newly-registered late components -- succeeds" $
            runsOk $ lns
            [ "local saveModules = require('scripts.lib.save_modules')"
            , "local crashCompCalls = 0"
            , "local crashCompApplied = nil"
            , "saveModules.register('crash_apply_comp', { version=1, inputVersions={1}, required=true, scope='global', deps={},"
            , "  snapshot = function() return { n = 1 } end,"
            , "  decode = function(v, d) return d end,"
            , "  validate = function(d) return nil end,"
            , "  apply = function(d)"
            , "    crashCompCalls = crashCompCalls + 1"
            , "    if crashCompCalls == 1 then"
            , "      local okReg = pcall(saveModules.register, 'sneaky_during_apply', " <> validSpecLua "sneaky_during_apply" <> ")"
            , "      assert(not okReg, 'register during an active apply must be rejected')"
            , "      local okHook = pcall(saveModules.registerResetHook, 'sneaky_hook_during_apply', function() end)"
            , "      assert(not okHook, 'registerResetHook during an active apply must be rejected')"
            , "      error('CRASH_APPLY_MARKER_12345')"
            , "    end"
            , "    crashCompApplied = d"
            , "  end })"
            , "local snap = saveModules.snapshotAll()"
            , "assert(snap.ok, 'snapshotAll should succeed')"
            , "local prep = saveModules.prepareLoad(snap.components)"
            , "assert(prep.ok, 'prepareLoad should succeed')"
            , "local ok1, err1 = pcall(saveModules.applyAll)"
            , "assert(not ok1, 'applyAll must fail when a component apply() crashes')"
            , "assert(string.find(tostring(err1), 'CRASH_APPLY_MARKER_12345', 1, true) ~= nil,"
            , "  'the original crash marker must survive in the surfaced failure: ' .. tostring(err1))"
            , "local ok2, err2 = pcall(saveModules.applyAll)"
            , "assert(not ok2, 'applyAll must fail again with no fresh prepareLoad')"
            , "local msg2 = tostring(err2)"
            , "assert(string.find(msg2, 'no prepared load', 1, true) ~= nil,"
            , "  'expected the no-prepared-load diagnostic, got: ' .. msg2)"
            , "assert(not string.find(msg2, '\\n', 1, true), 'diagnostic must not contain a newline: ' .. msg2)"
            , "assert(not string.find(msg2, '\\r', 1, true), 'diagnostic must not contain a carriage return: ' .. msg2)"
            , "saveModules.register('crash_apply_late', " <> validSpecLua "crash_apply_late" <> ")"
            , "local lateResetRan = false"
            , "saveModules.registerResetHook('crash_apply_late_reset', function() lateResetRan = true end)"
            , "local snap2 = saveModules.snapshotAll()"
            , "assert(snap2.ok, 'snapshotAll should succeed after recovery')"
            , "local prep2 = saveModules.prepareLoad(snap2.components)"
            , "assert(prep2.ok, 'prepareLoad should succeed after recovery')"
            , "saveModules.applyAll()"
            , "assert(crashCompApplied ~= nil and crashCompApplied.n == 1,"
            , "  'the previously-crashing component must apply successfully this time')"
            , "assert(_G.crash_apply_late_applied.x == 1, 'the newly-registered late component must apply')"
            , "assert(lateResetRan, 'the newly-registered late reset hook must run')"
            ]

        it "recovers the registry after a crashing reset hook (issue \
           \#864): the original crash marker survives in the surfaced \
           \failure, register/registerResetHook are still rejected from \
           \inside the active reset hook, the registry is immediately \
           \usable again with no intervening successful load, a stale \
           \re-applyAll (with no fresh prepareLoad) still fails with the \
           \single-line no-prepared-load diagnostic, and a later full \
           \round trip -- including the previously-crashed reset hook \
           \and newly-registered late components -- succeeds" $
            runsOk $ lns
            [ "local saveModules = require('scripts.lib.save_modules')"
            , "saveModules.register('reset_crash_comp', " <> validSpecLua "reset_crash_comp" <> ")"
            , "local resetCrashCalls = 0"
            , "local resetCrashRan = false"
            , "saveModules.registerResetHook('reset_crash_hook', function()"
            , "  resetCrashCalls = resetCrashCalls + 1"
            , "  if resetCrashCalls == 1 then"
            , "    local okReg = pcall(saveModules.register, 'sneaky_during_reset', " <> validSpecLua "sneaky_during_reset" <> ")"
            , "    assert(not okReg, 'register during an active reset hook must be rejected')"
            , "    local okHook = pcall(saveModules.registerResetHook, 'sneaky_hook_during_reset', function() end)"
            , "    assert(not okHook, 'registerResetHook during an active reset hook must be rejected')"
            , "    error('CRASH_RESET_MARKER_67890')"
            , "  end"
            , "  resetCrashRan = true"
            , "end)"
            , "local snap = saveModules.snapshotAll()"
            , "assert(snap.ok, 'snapshotAll should succeed')"
            , "local prep = saveModules.prepareLoad(snap.components)"
            , "assert(prep.ok, 'prepareLoad should succeed')"
            , "local ok1, err1 = pcall(saveModules.applyAll)"
            , "assert(not ok1, 'applyAll must fail when a reset hook crashes')"
            , "assert(string.find(tostring(err1), 'CRASH_RESET_MARKER_67890', 1, true) ~= nil,"
            , "  'the original crash marker must survive in the surfaced failure: ' .. tostring(err1))"
            , "local ok2, err2 = pcall(saveModules.applyAll)"
            , "assert(not ok2, 'applyAll must fail again with no fresh prepareLoad')"
            , "local msg2 = tostring(err2)"
            , "assert(string.find(msg2, 'no prepared load', 1, true) ~= nil,"
            , "  'expected the no-prepared-load diagnostic, got: ' .. msg2)"
            , "assert(not string.find(msg2, '\\n', 1, true), 'diagnostic must not contain a newline: ' .. msg2)"
            , "assert(not string.find(msg2, '\\r', 1, true), 'diagnostic must not contain a carriage return: ' .. msg2)"
            , "saveModules.register('reset_crash_late', " <> validSpecLua "reset_crash_late" <> ")"
            , "local lateResetRan = false"
            , "saveModules.registerResetHook('reset_crash_late_reset', function() lateResetRan = true end)"
            , "local snap2 = saveModules.snapshotAll()"
            , "assert(snap2.ok, 'snapshotAll should succeed after recovery')"
            , "local prep2 = saveModules.prepareLoad(snap2.components)"
            , "assert(prep2.ok, 'prepareLoad should succeed after recovery')"
            , "saveModules.applyAll()"
            , "assert(resetCrashRan, 'the previously-crashing reset hook must run successfully this time')"
            , "assert(_G.reset_crash_late_applied.x == 1, 'the newly-registered late component must apply')"
            , "assert(lateResetRan, 'the newly-registered late reset hook must run')"
            ]

        it "keeps an optional component's default() distinct from a \
           \required component's hard failure, and never uses \
           \optionality to hide a validation error" $ runsOk $ lns
            [ "local saveModules = require('scripts.lib.save_modules')"
            , "saveModules.register('opt_present', { version=1, inputVersions={1}, required=false, scope='global', deps={},"
            , "  snapshot=function() return {} end,"
            , "  decode=function(v,d) return d end,"
            , "  validate=function(d) return {'synthetic validation error'} end,"
            , "  apply=function() end,"
            , "  default=function() return { defaulted = true } end })"
            , "local prep = saveModules.prepareLoad({"
            , "  { id = 'opt_present', version = 1, payload = require('scripts.lib.data_codec').encode({}) }"
            , "})"
            , "assert(not prep.ok, 'a present-but-invalid optional component must still fail')"
            ]

        it "actually CALLS a declared references() function during \
           \prepareLoad (requirement 11/12) rather than storing it unused \
           \-- a crash inside references() is reported as a load failure" $
            runsOk $ lns
            [ "local saveModules = require('scripts.lib.save_modules')"
            , "local codec = require('scripts.lib.data_codec')"
            , "local called = false"
            , "saveModules.register('refs_ok', { version=1, inputVersions={1}, required=true, scope='global', deps={},"
            , "  snapshot=function() return { x = 1 } end,"
            , "  decode=function(v,d) return d end,"
            , "  validate=function() return nil end,"
            , "  apply=function() end,"
            , "  references=function(d) called = true; return {{kind='unit', id=d.x}} end })"
            , "saveModules.register('refs_crash', { version=1, inputVersions={1}, required=true, scope='global', deps={},"
            , "  snapshot=function() return {} end,"
            , "  decode=function(v,d) return d end,"
            , "  validate=function() return nil end,"
            , "  apply=function() end,"
            , "  references=function() error('synthetic references() crash') end })"
            , "-- One prepareLoad call covering both: refs_crash's failure must"
            , "-- abort the WHOLE load, but refs_ok's own references() must"
            , "-- still have run (the loop accumulates every component's"
            , "-- errors rather than short-circuiting on the first one)."
            , "local prep = saveModules.prepareLoad({"
            , "  { id = 'refs_ok', version = 1, payload = codec.encode({x = 1}) },"
            , "  { id = 'refs_crash', version = 1, payload = codec.encode({}) },"
            , "})"
            , "assert(not prep.ok, 'a crashing references() must fail the whole load')"
            , "assert(called, 'references() must actually be invoked during prepareLoad')"
            ]

        it "returns every references() edge, flattened across components, \
           \on a SUCCESSFUL prepareLoad (issue #764, save-overhaul C3) -- \
           \previously only ever CALLED for its crash-check, the returned \
           \list itself was discarded" $
            runsOk $ lns
            [ "local saveModules = require('scripts.lib.save_modules')"
            , "local codec = require('scripts.lib.data_codec')"
            , "saveModules.register('refs_a', { version=1, inputVersions={1}, required=true, scope='global', deps={},"
            , "  snapshot=function() return { u = 5 } end,"
            , "  decode=function(v,d) return d end,"
            , "  validate=function() return nil end,"
            , "  apply=function() end,"
            , "  references=function(d) return {{kind='unit', id=d.u}} end })"
            , "saveModules.register('refs_b', { version=1, inputVersions={1}, required=true, scope='global', deps={},"
            , "  snapshot=function() return { b = 9 } end,"
            , "  decode=function(v,d) return d end,"
            , "  validate=function() return nil end,"
            , "  apply=function() end,"
            , "  references=function(d) return {{kind='building', id=d.b}} end })"
            , "local prep = saveModules.prepareLoad({"
            , "  { id = 'refs_a', version = 1, payload = codec.encode({u = 5}) },"
            , "  { id = 'refs_b', version = 1, payload = codec.encode({b = 9}) },"
            , "})"
            , "assert(prep.ok, 'expected prepareLoad to succeed')"
            , "assert(type(prep.references) == 'table', 'expected a references array')"
            , "local byComponent = {}"
            , "for _, r in ipairs(prep.references) do byComponent[r.component] = r end"
            , "assert(byComponent.refs_a ~= nil, 'expected an edge from refs_a')"
            , "assert(byComponent.refs_a.kind == 'unit', 'expected refs_a edge kind unit')"
            , "assert(byComponent.refs_a.id == 5, 'expected refs_a edge id 5')"
            , "assert(byComponent.refs_b ~= nil, 'expected an edge from refs_b')"
            , "assert(byComponent.refs_b.kind == 'building', 'expected refs_b edge kind building')"
            , "assert(byComponent.refs_b.id == 9, 'expected refs_b edge id 9')"
            , "assert(#prep.references == 2, 'expected exactly 2 edges, got ' .. #prep.references)"
            ]

        it "carries EVERY diagnostic/resolution field a references() hook \
           \sets -- owner, path AND page -- through both snapshotAll and \
           \prepareLoad, not just component/kind/id (#915)" $
            -- The flatteners rebuild each edge field by field, so a hook
            -- reporting a field they don't copy loses it silently before
            -- Haskell ever sees it. For `page` that is not cosmetic: a
            -- location_instance id is PER PAGE, so an edge arriving
            -- without its page resolves against nothing and every valid
            -- memory would be reported as dangling
            -- (World.Save.Integrity.luaEdgeResolves). Asserted on the
            -- real snapshotAll/prepareLoad results, since a hook's own
            -- return value proves nothing about what the flattener kept.
            runsOk $ lns
            [ "local saveModules = require('scripts.lib.save_modules')"
            , "local codec = require('scripts.lib.data_codec')"
            , "local function edge() return {{ kind='location_instance',"
            , "  id=3, owner=7, path='unit[7].knownLocations[1]',"
            , "  page='main_world' }} end"
            , "saveModules.register('refs_page', { version=1, inputVersions={1},"
            , "  required=true, scope='global', deps={},"
            , "  snapshot=function() return { k = 1 } end,"
            , "  decode=function(v,d) return d end,"
            , "  validate=function() return nil end,"
            , "  apply=function() end,"
            , "  references=function(d) return edge() end })"
            , "local function checkEdges(refs, what)"
            , "  assert(type(refs) == 'table', what .. ': no references array')"
            , "  assert(#refs == 1, what .. ': expected 1 edge, got ' .. #refs)"
            , "  local r = refs[1]"
            , "  assert(r.component == 'refs_page', what .. ': component lost')"
            , "  assert(r.kind == 'location_instance', what .. ': kind lost')"
            , "  assert(r.id == 3, what .. ': id lost')"
            , "  assert(r.owner == 7, what .. ': owner lost')"
            , "  assert(r.path == 'unit[7].knownLocations[1]',"
            , "         what .. ': path lost')"
            , "  assert(r.page == 'main_world', what .. ': page lost')"
            , "end"
            , "local snap = saveModules.snapshotAll()"
            , "assert(snap.ok, 'expected snapshotAll to succeed')"
            , "checkEdges(snap.references, 'snapshotAll')"
            , "local prep = saveModules.prepareLoad({"
            , "  { id = 'refs_page', version = 1, payload = codec.encode({k = 1}) },"
            , "})"
            , "assert(prep.ok, 'expected prepareLoad to succeed')"
            , "checkEdges(prep.references, 'prepareLoad')"
            ]

        it "correlates abortPreparedLoad(requestId) with the request id \
           \prepareLoad stashed, so a stale abort for an OLD, already- \
           \superseded request cannot clear a NEWER requests prepared \
           \state (round 9 review, issue #763): LuaLoadStagingFailed is a \
           \queued message that can arrive after a new request has \
           \already prepared" $ runsOk $ lns
            [ "local saveModules = require('scripts.lib.save_modules')"
            , "local codec = require('scripts.lib.data_codec')"
            , "saveModules.register('reqid_c', " <> validSpecLua "reqid_c" <> ")"
            , "local snap = saveModules.snapshotAll()"
            , "assert(snap.ok, 'snapshotAll should succeed')"
            , "local prep1 = saveModules.prepareLoad(snap.components, 1)"
            , "assert(prep1.ok, 'first prepareLoad should succeed')"
            , "-- Request 1 is superseded (e.g. it failed staging on the world"
            , "-- thread) and request 2 is accepted and prepares its own state"
            , "-- before request 1's stale abort message is ever processed."
            , "local prep2 = saveModules.prepareLoad(snap.components, 2)"
            , "assert(prep2.ok, 'second prepareLoad should succeed')"
            , "-- A stale abort naming the OLD request id must be a no-op."
            , "saveModules.abortPreparedLoad(1)"
            , "assert(saveModules._pendingApply ~= nil, "
              <> "'a stale abort for the old request must not clear the "
              <> "newer requests prepared state')"
            , "assert(saveModules._loadActive == true, "
              <> "'loadActive must stay true for the still-prepared newer "
              <> "request')"
            , "-- An abort naming the CURRENT request id still works."
            , "saveModules.abortPreparedLoad(2)"
            , "assert(saveModules._pendingApply == nil, "
              <> "'an abort for the current request id must clear it')"
            , "assert(saveModules._loadActive == false)"
            ]

        it "abortPreparedLoad with no requestId (nil) always clears the \
           \pending load unconditionally, matching pre-round-9 callers \
           \with no request in play" $ runsOk $ lns
            [ "local saveModules = require('scripts.lib.save_modules')"
            , "saveModules.register('reqid_nil', " <> validSpecLua "reqid_nil" <> ")"
            , "local snap = saveModules.snapshotAll()"
            , "assert(snap.ok)"
            , "local prep = saveModules.prepareLoad(snap.components, 7)"
            , "assert(prep.ok)"
            , "saveModules.abortPreparedLoad()"
            , "assert(saveModules._pendingApply == nil, "
              <> "'a nil requestId must still clear a pending load')"
            , "assert(saveModules._loadActive == false)"
            ]

    -- Issue #900: per-entity application, exercised against the GENERIC
    -- mechanism with synthetic components. It has to be tested here
    -- rather than through unit_ai/building_spawn themselves: this group
    -- runs a bare Lua VM that loads only save_modules.lua + data_codec.lua,
    -- while those modules need unit.exists/building.getInfo and the wider
    -- script graph. Their own per-entity apply is gated end-to-end by
    -- tools/lua_orphan_prune_probe.py against a real engine instead.
    -- Issue #1200. Every rollback restore used to be an unchecked
    -- `pcall` whose result was discarded, after which the raised
    -- diagnostic claimed unconditionally that it had "rolled back every
    -- already-applied component". A restore that threw was therefore
    -- silently swallowed, leaving the process running the OLD Haskell
    -- session against partly-NEW Lua singletons -- the exact
    -- mixed-generation session rollback exists to prevent -- while both
    -- engine.getLoadStatus() and the CatWorld warning reported a cleanly
    -- aborted load. These cases arm restores to throw and assert BOTH
    -- the live disposition and the reported text.
    describe "rollback double faults (issue #1200)" $ do
        it "keeps unwinding after a restore throws, names EVERY component \
           \it could not restore, and still attempts the restores behind \
           \them -- while preserving #864's recovery contract so the same \
           \session loads successfully afterwards" $ runsOk $ lns
            [ rollbackFixtureLua
            , "local saveModules = require('scripts.lib.save_modules')"
            , "local codec = require('scripts.lib.data_codec')"
            , "saveModules.register('rb_a', comp('a'))"
            , "saveModules.register('rb_b', comp('b'))"
            , "saveModules.register('rb_c', comp('c'))"
            , "-- Sorts last, so it applies after all three and its"
            , "-- failure unwinds them in reverse: c, b, a."
            , "local boomCalls = 0"
            , "saveModules.register('rb_z_boom', { version=1,"
            , "  inputVersions={1}, required=true, scope='global', deps={},"
            , "  snapshot=function() return {} end,"
            , "  decode=function(v,d) return d end,"
            , "  validate=function() return nil end,"
            , "  apply=function()"
            , "    boomCalls = boomCalls + 1"
            , "    if boomCalls == 1 then error('FORWARD_BOOM_1200') end"
            , "  end })"
            , "-- Arm the two restores that unwind FIRST (c, then b) to"
            , "-- throw, leaving rb_a's restore -- which runs last -- as"
            , "-- the proof that one failure never short-circuits the rest."
            , "armed.c, armed.b = 2, 2"
            , "local function payloads()"
            , "  return { { id='rb_a', version=1, payload=payloadFor('new-a') },"
            , "           { id='rb_b', version=1, payload=payloadFor('new-b') },"
            , "           { id='rb_c', version=1, payload=payloadFor('new-c') },"
            , "           { id='rb_z_boom', version=1, payload=codec.encode({}) } }"
            , "end"
            , "assert(saveModules.prepareLoad(payloads(), 1, false, nil).ok)"
            , "local ok, err = pcall(saveModules.applyAll)"
            , "assert(not ok, 'the forward failure must still abort the load')"
            , "local msg = tostring(err)"
            , "assert(string.find(msg, 'FORWARD_BOOM_1200', 1, true) ~= nil,"
            , "  'the ORIGINAL forward failure must survive: ' .. msg)"
            , "assert(string.find(msg, 'ROLLBACK FAILED', 1, true) ~= nil,"
            , "  'a failed rollback must never report as a clean abort: ' .. msg)"
            , "assert(string.find(msg, 'MIXED', 1, true) ~= nil,"
            , "  'the mixed-session disposition must be stated: ' .. msg)"
            , "assert(string.find(msg, 'rolled back every', 1, true) == nil,"
            , "  'a complete rollback must NOT be claimed: ' .. msg)"
            , "assert(string.find(msg, 'rb_b', 1, true) ~= nil"
            , "  and string.find(msg, 'rb_c', 1, true) ~= nil,"
            , "  'every unrestored component must be named: ' .. msg)"
            , "-- The live disposition, not just the wording: the two"
            , "-- failed restores kept the NEW rows (this is the mix)..."
            , "assert(live.b == 'new-b' and live.c == 'new-c',"
            , "  'an unrestored component keeps the new session row')"
            , "-- ...while the restore BEHIND them still ran and won."
            , "assert(live.a == 'old-a',"
            , "  'unwinding must not stop at the first failed restore')"
            , "assert(calls.a == 2, 'rb_a restore must have been ATTEMPTED')"
            , "-- #864 recovery across a DOUBLE fault: bookkeeping is"
            , "-- clear, a stale re-apply still refuses, registration"
            , "-- works again, and a fresh load recovers the session."
            , "local ok2, err2 = pcall(saveModules.applyAll)"
            , "assert(not ok2, 'a stale re-applyAll must still fail')"
            , "assert(string.find(tostring(err2), 'no prepared load', 1, true)"
            , "  ~= nil, 'expected the no-prepared-load diagnostic: '"
            , "  .. tostring(err2))"
            , "saveModules.register('rb_late', " <> validSpecLua "rb_late" <> ")"
            , "armed.b, armed.c = nil, nil"
            , "local second = payloads()"
            , "second[#second + 1] ="
            , "  { id='rb_late', version=1, payload=codec.encode({ x = 1 }) }"
            , "local prep2 = saveModules.prepareLoad(second, 2, false, nil)"
            , "assert(prep2.ok, 'prepareLoad must work again after the double fault')"
            , "saveModules.applyAll()"
            , "assert(live.a == 'new-a' and live.b == 'new-b'"
            , "  and live.c == 'new-c', 'a later load must recover the session')"
            , "assert(_G.rb_late_applied.x == 1, 'late registration must apply')"
            ]

        it "reports the FAILING component's own restore when that throws \
           \too, leaving its half-applied singleton live -- and still \
           \unwinds the components behind it" $ runsOk $ lns
            [ rollbackFixtureLua
            , "local saveModules = require('scripts.lib.save_modules')"
            , "local codec = require('scripts.lib.data_codec')"
            , "saveModules.register('own_a', comp('a'))"
            , "local ownCalls = 0"
            , "local ownLive = 'old-own'"
            , "saveModules.register('own_z_boom', { version=1,"
            , "  inputVersions={1}, required=true, scope='global', deps={},"
            , "  snapshot=function() return { v = ownLive } end,"
            , "  decode=function(v,d) return d end,"
            , "  validate=function() return nil end,"
            , "  apply=function(d)"
            , "    ownCalls = ownCalls + 1"
            , "    if ownCalls == 1 then"
            , "      -- Mutate PART of its own singleton, then throw: this"
            , "      -- is why the failing component's own pre-load"
            , "      -- restore exists at all."
            , "      ownLive = 'half-applied'"
            , "      error('OWN_FORWARD_BOOM_1200')"
            , "    end"
            , "    -- ...and that own restore throws in turn."
            , "    error('OWN_RESTORE_BOOM_1200')"
            , "  end })"
            , "assert(saveModules.prepareLoad("
            , "  { { id='own_a', version=1, payload=payloadFor('new-a') },"
            , "    { id='own_z_boom', version=1, payload=codec.encode({}) } },"
            , "  1, false, nil).ok)"
            , "local ok, err = pcall(saveModules.applyAll)"
            , "assert(not ok, 'the forward failure must still abort the load')"
            , "local msg = tostring(err)"
            , "assert(string.find(msg, 'OWN_FORWARD_BOOM_1200', 1, true) ~= nil,"
            , "  'the ORIGINAL forward failure must survive: ' .. msg)"
            , "assert(string.find(msg, 'ROLLBACK FAILED', 1, true) ~= nil,"
            , "  'the failed own-restore must be reported: ' .. msg)"
            , "assert(string.find(msg, 'own_z_boom', 1, true) ~= nil,"
            , "  'the component left unrestored must be named: ' .. msg)"
            , "assert(ownLive == 'half-applied',"
            , "  'the half-applied singleton is exactly what stays live')"
            , "assert(live.a == 'old-a',"
            , "  'a failed own-restore must not stop the unwind behind it')"
            ]

        it "still reports the aggregate when a failed restore raised a \
           \value that cannot even be rendered -- error() takes any Lua \
           \value, and a throwing __tostring must not replace the whole \
           \diagnostic with its own error" $ runsOk $ lns
            [ rollbackFixtureLua
            , "local saveModules = require('scripts.lib.save_modules')"
            , "local codec = require('scripts.lib.data_codec')"
            , "saveModules.register('vile_a', comp('a'))"
            , "-- Restores after vile_b, so vile_a's restore still runs."
            , "local vileCalls = 0"
            , "saveModules.register('vile_b', { version=1,"
            , "  inputVersions={1}, required=true, scope='global', deps={},"
            , "  snapshot=function() return {} end,"
            , "  decode=function(v,d) return d end,"
            , "  validate=function() return nil end,"
            , "  apply=function()"
            , "    vileCalls = vileCalls + 1"
            , "    if vileCalls == 2 then"
            , "      -- A rollback restore raising a table whose own"
            , "      -- __tostring throws: rendering it is what used to"
            , "      -- take the aggregate diagnostic down with it."
            , "      error(setmetatable({}, { __tostring = function()"
            , "        error('TOSTRING_ITSELF_EXPLODES') end }))"
            , "    end"
            , "  end })"
            , "saveModules.register('vile_z_boom', { version=1,"
            , "  inputVersions={1}, required=true, scope='global', deps={},"
            , "  snapshot=function() return {} end,"
            , "  decode=function(v,d) return d end,"
            , "  validate=function() return nil end,"
            , "  apply=function() error('VILE_FORWARD_BOOM_1200') end })"
            , "assert(saveModules.prepareLoad("
            , "  { { id='vile_a', version=1, payload=payloadFor('new-a') },"
            , "    { id='vile_b', version=1, payload=codec.encode({}) },"
            , "    { id='vile_z_boom', version=1, payload=codec.encode({}) } },"
            , "  1, false, nil).ok)"
            , "local ok, err = pcall(saveModules.applyAll)"
            , "assert(not ok, 'the forward failure must still abort the load')"
            , "local msg = tostring(err)"
            , "assert(string.find(msg, 'TOSTRING_ITSELF_EXPLODES', 1, true)"
            , "  == nil, 'the render failure must NOT become the surfaced "
              <> "error: ' .. msg)"
            , "assert(string.find(msg, 'VILE_FORWARD_BOOM_1200', 1, true)"
            , "  ~= nil, 'the ORIGINAL forward failure must survive: ' .. msg)"
            , "assert(string.find(msg, 'ROLLBACK FAILED', 1, true) ~= nil"
            , "  and string.find(msg, 'MIXED', 1, true) ~= nil,"
            , "  'the mixed-session distinction must survive: ' .. msg)"
            , "assert(string.find(msg, 'vile_b', 1, true) ~= nil,"
            , "  'the unrestored component must still be named: ' .. msg)"
            , "assert(string.find(msg, 'unrenderable', 1, true) ~= nil,"
            , "  'the unrenderable value must degrade to a placeholder: ' .. msg)"
            , "assert(live.a == 'old-a',"
            , "  'the restore behind the unrenderable failure still ran')"
            , "-- #864: an unrenderable double fault must still leave the"
            , "-- registry usable rather than wedged mid-transaction."
            , "local ok2, err2 = pcall(saveModules.applyAll)"
            , "assert(not ok2 and string.find(tostring(err2),"
            , "  'no prepared load', 1, true) ~= nil,"
            , "  'the transaction must still have been cleared: ' .. tostring(err2))"
            ]

        it "reports a failed unwind on the RESET-HOOK path too, where \
           \every component had already committed" $ runsOk $ lns
            [ rollbackFixtureLua
            , "local saveModules = require('scripts.lib.save_modules')"
            , "saveModules.register('hook_a', comp('a'))"
            , "saveModules.register('hook_b', comp('b'))"
            , "-- hook_b's restore throws while unwinding after the hook"
            , "-- fails; hook_a's (which unwinds after it) must still run."
            , "armed.b = 2"
            , "saveModules.registerResetHook('hook_boom', function()"
            , "  error('RESET_BOOM_1200')"
            , "end)"
            , "assert(saveModules.prepareLoad("
            , "  { { id='hook_a', version=1, payload=payloadFor('new-a') },"
            , "    { id='hook_b', version=1, payload=payloadFor('new-b') } },"
            , "  1, false, nil).ok)"
            , "local ok, err = pcall(saveModules.applyAll)"
            , "assert(not ok, 'a failing reset hook must abort the load')"
            , "local msg = tostring(err)"
            , "assert(string.find(msg, 'RESET_BOOM_1200', 1, true) ~= nil,"
            , "  'the ORIGINAL reset-hook failure must survive: ' .. msg)"
            , "assert(string.find(msg, 'ROLLBACK FAILED', 1, true) ~= nil,"
            , "  'the failed unwind must be reported: ' .. msg)"
            , "assert(string.find(msg, 'hook_b', 1, true) ~= nil,"
            , "  'the unrestored component must be named: ' .. msg)"
            , "assert(string.find(msg, 'rolled back every', 1, true) == nil,"
            , "  'a complete rollback must NOT be claimed: ' .. msg)"
            , "assert(live.b == 'new-b', 'the unrestored component stays new')"
            , "assert(live.a == 'old-a', 'the restore behind it still ran')"
            ]

    -- Issue #1200 requirement 3: the distinction has to survive the
    -- HASKELL boundary, not just exist inside Lua. applyLuaLoad used to
    -- discard every Lua diagnostic and return a fixed "see engine log"
    -- string, so engine.getLoadStatus() and the CatWorld warning could
    -- not tell a clean abort from a mixed session no matter what
    -- save_modules said. These drive the real bridge.
    describe "applyLuaLoad load-failure reporting (issue #1200)" $ do
        it "carries the ROLLBACK FAILED distinction and the original \
           \forward failure into the Left the load path reports" $ do
            result ← applyViaBridge $ lns
                [ rollbackFixtureLua
                , "local saveModules = require('scripts.lib.save_modules')"
                , "local codec = require('scripts.lib.data_codec')"
                , "saveModules.register('bridge_a', comp('a'))"
                , "armed.a = 2"
                , "saveModules.register('bridge_z_boom', { version=1,"
                , "  inputVersions={1}, required=true, scope='global', deps={},"
                , "  snapshot=function() return {} end,"
                , "  decode=function(v,d) return d end,"
                , "  validate=function() return nil end,"
                , "  apply=function() error('BRIDGE_FORWARD_BOOM_1200') end })"
                , "assert(saveModules.prepareLoad("
                , "  { { id='bridge_a', version=1, payload=payloadFor('new-a') },"
                , "    { id='bridge_z_boom', version=1, payload=codec.encode({}) } },"
                , "  1, false, nil).ok)"
                ]
            case result of
                Right () → expectationFailure
                    "expected applyLuaLoad to report the failed load"
                Left err → do
                    T.unpack err `shouldContain` "BRIDGE_FORWARD_BOOM_1200"
                    T.unpack err `shouldContain` "ROLLBACK FAILED"
                    T.unpack err `shouldContain` "bridge_a"

        it "still reports a CLEAN abort as a clean abort when every \
           \restore succeeded -- the two dispositions must not collapse \
           \into one message" $ do
            result ← applyViaBridge $ lns
                [ rollbackFixtureLua
                , "local saveModules = require('scripts.lib.save_modules')"
                , "local codec = require('scripts.lib.data_codec')"
                , "saveModules.register('clean_a', comp('a'))"
                , "local cleanCalls = 0"
                , "saveModules.register('clean_z_boom', { version=1,"
                , "  inputVersions={1}, required=true, scope='global', deps={},"
                , "  snapshot=function() return {} end,"
                , "  decode=function(v,d) return d end,"
                , "  validate=function() return nil end,"
                , "  -- Throws on the FORWARD pass only: its own pre-load"
                , "  -- restore (call 2) succeeds, as does every other"
                , "  -- restore, which is what makes this the clean case."
                , "  apply=function()"
                , "    cleanCalls = cleanCalls + 1"
                , "    if cleanCalls == 1 then error('CLEAN_FORWARD_BOOM_1200') end"
                , "  end })"
                , "assert(saveModules.prepareLoad("
                , "  { { id='clean_a', version=1, payload=payloadFor('new-a') },"
                , "    { id='clean_z_boom', version=1, payload=codec.encode({}) } },"
                , "  1, false, nil).ok)"
                ]
            case result of
                Right () → expectationFailure
                    "expected applyLuaLoad to report the failed load"
                Left err → do
                    T.unpack err `shouldContain` "CLEAN_FORWARD_BOOM_1200"
                    T.unpack err `shouldContain`
                        "rolled back every already-applied component"
                    T.unpack err `shouldNotContain` "ROLLBACK FAILED"
