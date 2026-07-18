{-# LANGUAGE UnicodeSyntax, OverloadedStrings, ScopedTypeVariables #-}
-- | The "Lua persistence components" gate (issue #761, save-overhaul
--   B3): a standalone Lua VM (no engine, no world/unit threads, no
--   HsLua-side marshalling of the registry's internals) exercising
--   @scripts/lib/data_codec.lua@ and @scripts/lib/save_modules.lua@
--   directly, the same pattern this suite already uses for pure
--   Haskell logic ("Test.Headless.UI.Clipping" etc.) applied to Lua:
--   each 'it' runs one self-contained Lua chunk via 'Lua.dostring' in
--   a fresh interpreter (stdlib + a minimal @engine@ stub — the only
--   global these two modules ever reach outside a real engine boot),
--   asserting inside Lua via @assert()@/@error()@ and surfacing a
--   non-OK 'Lua.Status' as an hspec failure with the Lua message.
--
--   Runs with @cabal test@'s CWD at the repo root (same as every other
--   repo-root-relative Lua path in this codebase), so
--   @require("scripts.lib.*")@ resolves via Lua's own default
--   @package.path@ with no extra setup.
--
--   Run just this gate: @cabal test synarchy-test-headless
--   --test-options='--match "Lua persistence components"'@.
module Test.Headless.Lua.SaveModules (spec) where

import UPrelude
import Test.Hspec
import qualified HsLua as Lua
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE

-- | A minimal @engine@ global -- the only thing these two modules call
--   outside of a real engine boot (@engine.logWarn@ from
--   @save_modules.snapshotAll@'s optional-component-omitted warning).
engineStub ∷ Text
engineStub =
    "engine = { logWarn = function(...) end, logInfo = function(...) end }"

-- | Run one self-contained Lua chunk in a fresh interpreter (stdlib +
-- 'engineStub' loaded first). The chunk must signal failure via Lua's
-- own @assert()@/@error()@ -- a non-OK 'Lua.Status' becomes an hspec
-- 'expectationFailure' carrying the Lua error message (file:line
-- included, since chunks are loaded with a name below).
runsOk ∷ Text → Expectation
runsOk chunkText = do
    result ← Lua.run $ do
        Lua.openlibs
        _ ← Lua.dostring (TE.encodeUtf8 engineStub)
        status ← Lua.dostring (TE.encodeUtf8 chunkText)
        case status of
            Lua.OK → return Nothing
            _ → do
                err ← Lua.tostring (-1)
                return (Just (maybe "<no message>" TE.decodeUtf8Lenient err))
    case result of
        Nothing  → pure ()
        Just msg → expectationFailure (T.unpack msg)

lns ∷ [Text] → Text
lns = T.intercalate "\n"

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

spec ∷ Spec
spec = do
    describe "data_codec (issue #761 requirement 8)" $ do
        it "round-trips scalars, strings, arrays, and maps" $ runsOk $ lns
            [ "local codec = require('scripts.lib.data_codec')"
            , "assert(codec.decode(codec.encode(true)) == true)"
            , "assert(codec.decode(codec.encode(false)) == false)"
            , "assert(codec.decode(codec.encode(42)) == 42)"
            , "assert(codec.decode(codec.encode(-3.5)) == -3.5)"
            , "assert(codec.decode(codec.encode('hello')) == 'hello')"
            , "assert(codec.decode(codec.encode('')) == '')"
            , "local arr = codec.decode(codec.encode({1,2,3,'four'}))"
            , "assert(#arr == 4 and arr[4] == 'four')"
            , "local m = codec.decode(codec.encode({a=1, b='two', c=true}))"
            , "assert(m.a == 1 and m.b == 'two' and m.c == true)"
            , "local im = codec.decode(codec.encode({[5]='five',[100]='hundred'}))"
            , "assert(im[5] == 'five' and im[100] == 'hundred')"
            ]

        it "encodes maps in canonical key order regardless of insertion order" $
            runsOk $ lns
            [ "local codec = require('scripts.lib.data_codec')"
            , "local m1 = {b=2, a=1, [3]=30, [1]=10}"
            , "local m2 = {a=1, [1]=10, b=2, [3]=30}"
            , "assert(codec.encode(m1) == codec.encode(m2))"
            ]

        it "rejects functions, userdata-shaped, threads, and metatables" $
            runsOk $ lns
            [ "local codec = require('scripts.lib.data_codec')"
            , "local ok1 = codec.encode({f = function() end})"
            , "assert(ok1 == nil, 'function should be rejected')"
            , "local ok2 = codec.encode(coroutine.create(function() end))"
            , "assert(ok2 == nil, 'thread should be rejected')"
            , "local mt = setmetatable({}, {__index = function() return 1 end})"
            , "local ok3 = codec.encode(mt)"
            , "assert(ok3 == nil, 'metatable-carrying table should be rejected')"
            ]

        it "rejects cyclic tables" $ runsOk $ lns
            [ "local codec = require('scripts.lib.data_codec')"
            , "local cyc = {}"
            , "cyc.self = cyc"
            , "local ok, err = codec.encode(cyc)"
            , "assert(ok == nil, 'cyclic table should be rejected')"
            , "assert(err ~= nil)"
            ]

        it "rejects excessively deep nesting" $ runsOk $ lns
            [ "local codec = require('scripts.lib.data_codec')"
            , "local t = {}"
            , "local cur = t"
            , "for i = 1, codec.MAX_DEPTH + 10 do"
            , "  cur.next = {}"
            , "  cur = cur.next"
            , "end"
            , "local ok = codec.encode(t)"
            , "assert(ok == nil, 'excessive depth should be rejected')"
            ]

        it "rejects unsupported key types (boolean/table keys)" $ runsOk $ lns
            [ "local codec = require('scripts.lib.data_codec')"
            , "local ok1 = codec.encode({[true] = 1})"
            , "assert(ok1 == nil, 'boolean key should be rejected')"
            , "local ok2 = codec.encode({[{}] = 1})"
            , "assert(ok2 == nil, 'table key should be rejected')"
            ]

        it "rejects NaN and +/-infinity with a data path in the error, \
           \unless finite" $ runsOk $ lns
            [ "local codec = require('scripts.lib.data_codec')"
            , "local ok1, err1 = codec.encode({x = 1/0})"
            , "assert(ok1 == nil and err1:find('root%[x%]'), '+inf rejected with path')"
            , "local ok2, err2 = codec.encode({x = -1/0})"
            , "assert(ok2 == nil and err2:find('root%[x%]'), '-inf rejected with path')"
            , "local ok3, err3 = codec.encode({x = 0/0})"
            , "assert(ok3 == nil and err3:find('root%[x%]'), 'nan rejected with path')"
            , "assert(codec.encode(1.5) ~= nil, 'an ordinary finite float still encodes')"
            ]

        it "rejects malformed and truncated payloads without executing them" $
            runsOk $ lns
            [ "local codec = require('scripts.lib.data_codec')"
            , "local d1, e1 = codec.decode('S5:ab')"
            , "assert(d1 == nil and e1 ~= nil, 'truncated string should fail')"
            , "local d2, e2 = codec.decode('Q1:x')"
            , "assert(d2 == nil and e2 ~= nil, 'unknown tag should fail')"
            , "local d3, e3 = codec.decode('A999999999999999999999:')"
            , "assert(d3 == nil and e3 ~= nil, 'malformed huge count should fail')"
            , "-- Proof decode never executes: a code-shaped STRING value"
            , "-- must decode back as inert data, not run as Lua."
            , "local ranMarker = false"
            , "local codeShaped = codec.encode('ranMarker = true')"
            , "local back = codec.decode(codeShaped)"
            , "assert(back == 'ranMarker = true' and ranMarker == false,"
            , "  'a code-shaped string must decode as inert data')"
            ]

        it "rejects overlong encodings, surrogate halves, and out-of-range \
           \codepoints, both at encode time and on the raw decode path" $
            runsOk $ lns
            [ "local codec = require('scripts.lib.data_codec')"
            , "local overlong = '\\xC0\\xAF'"  -- overlong 2-byte '/'
            , "assert(codec.encode(overlong) == nil, 'overlong encoding rejected')"
            , "local surrogate = '\\xED\\xA0\\x80'"  -- U+D800 half
            , "assert(codec.encode(surrogate) == nil, 'surrogate half rejected')"
            , "local outOfRange = '\\xF4\\x90\\x80\\x80'"  -- > U+10FFFF
            , "assert(codec.encode(outOfRange) == nil, 'out-of-range codepoint rejected')"
            , "-- Hand-crafted payload, bypassing encode() entirely: decode"
            , "-- must independently reject invalid UTF-8 on the read path."
            , "local d1, e1 = codec.decode('S2:\\xC0\\xAF')"
            , "assert(d1 == nil and e1 ~= nil, 'decode rejects invalid UTF-8 too')"
            ]

        it "rejects a hand-crafted map payload with a fractional numeric \
           \key, or a duplicate key, even though encode() would never \
           \produce either" $ runsOk $ lns
            [ "local codec = require('scripts.lib.data_codec')"
            , "local d1, e1 = codec.decode('M1:N3:1.5T')"
            , "assert(d1 == nil and e1 ~= nil, 'fractional numeric key rejected')"
            , "local d2, e2 = codec.decode('M2:N1:1T:N1:1F')"
            , "assert(d2 == nil and e2 ~= nil, 'duplicate map key rejected')"
            , "local m = codec.decode(codec.encode({[5] = 'five', [10] = 'ten'}))"
            , "assert(m[5] == 'five' and m[10] == 'ten', 'ordinary integer keys still decode')"
            ]

        it "rejects a hand-crafted number payload that parses to a \
           \non-finite value on decode, not just at encode" $ runsOk $ lns
            [ "local codec = require('scripts.lib.data_codec')"
            , "local d1, e1 = codec.decode('N5:1e999')"
            , "assert(d1 == nil and e1 ~= nil, 'decode rejects +inf-producing digits')"
            , "local d2, e2 = codec.decode('N6:-1e999')"
            , "assert(d2 == nil and e2 ~= nil, 'decode rejects -inf-producing digits')"
            ]

        it "enforces the max-entries limit on a single table" $ runsOk $ lns
            [ "local codec = require('scripts.lib.data_codec')"
            , "local big = {}"
            , "for i = 1, codec.MAX_TABLE_ENTRIES + 10 do big[i] = i end"
            , "local ok = codec.encode(big)"
            , "assert(ok == nil, 'exceeding max table entries should be rejected')"
            ]

    describe "save_modules registry (issue #761 requirements 2/3/4)" $ do
        it "accepts a valid required registration and a valid optional \
           \registration" $ runsOk $ lns
            [ "local saveModules = require('scripts.lib.save_modules')"
            , "saveModules.register('t_valid_required', " <> validSpecLua "t_valid_required" <> ")"
            , "saveModules.register('t_valid_optional', {"
            , "  version = 1, inputVersions = {1}, required = false, scope = 'global',"
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
            , "saveModules.register('dep_child', { version=1, required=true, scope='global',"
            , "  deps = {'dep_parent'},"
            , "  snapshot=function() return {} end, decode=function(v,d) return d end,"
            , "  validate=function() return nil end, apply=function() end })"
            , "saveModules.register('dep_parent', { version=1, required=true, scope='global', deps={},"
            , "  snapshot=function() return {} end, decode=function(v,d) return d end,"
            , "  validate=function() return nil end, apply=function() end })"
            , "local order = saveModules.dependencyOrder()"
            , "local posParent, posChild"
            , "for i, id in ipairs(order) do"
            , "  if id == 'dep_parent' then posParent = i end"
            , "  if id == 'dep_child' then posChild = i end"
            , "end"
            , "assert(posParent < posChild, 'dependency must precede its dependent')"
            , "saveModules.register('cyc_a', { version=1, required=true, scope='global', deps={'cyc_b'},"
            , "  snapshot=function() return {} end, decode=function(v,d) return d end,"
            , "  validate=function() return nil end, apply=function() end })"
            , "saveModules.register('cyc_b', { version=1, required=true, scope='global', deps={'cyc_a'},"
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
            , "saveModules.register('boom_required', { version=1, required=true, scope='global', deps={},"
            , "  snapshot = function() error('synthetic snapshot failure') end,"
            , "  decode=function(v,d) return d end, validate=function() return nil end,"
            , "  apply=function() end })"
            , "local snap = saveModules.snapshotAll()"
            , "assert(not snap.ok, 'a required snapshot failure must abort the whole save')"
            ]

        it "blocks new registration while a save snapshot is in progress \
           \(requirement 3)" $ runsOk $ lns
            [ "local saveModules = require('scripts.lib.save_modules')"
            , "saveModules.register('mid_capture', { version=1, required=true, scope='global', deps={},"
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

        it "keeps an optional component's default() distinct from a \
           \required component's hard failure, and never uses \
           \optionality to hide a validation error" $ runsOk $ lns
            [ "local saveModules = require('scripts.lib.save_modules')"
            , "saveModules.register('opt_present', { version=1, required=false, scope='global', deps={},"
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
            , "saveModules.register('refs_ok', { version=1, required=true, scope='global', deps={},"
            , "  snapshot=function() return { x = 1 } end,"
            , "  decode=function(v,d) return d end,"
            , "  validate=function() return nil end,"
            , "  apply=function() end,"
            , "  references=function(d) called = true; return {{kind='unit', id=d.x}} end })"
            , "saveModules.register('refs_crash', { version=1, required=true, scope='global', deps={},"
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
