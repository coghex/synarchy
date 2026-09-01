{-# LANGUAGE ScopedTypeVariables, TypeApplications #-}
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

-- | A minimal @engine@ global -- everything these modules, and the real
--   registrations driven through them, call outside of a real engine
--   boot: @engine.logWarn@ (from @save_modules.snapshotAll@'s
--   optional-component-omitted warning and @applyEntityRows@' absent-owner
--   drop), @engine.logInfo@, and @engine.gameTime@.
--
--   @gameTime@ arrived with issue #2055: @lua.unit_ai@'s @apply@ now
--   normalizes each retained row against @scripts/unit_ai_defaults.lua@,
--   and one of the three runtime defaults it supplies is
--   @actionStartedAt = engine.gameTime()@. It reads a FROZEN @NOW@ rather
--   than a real clock so a filled value is checkable by value, not merely
--   by presence.
engineStub ∷ Text
engineStub = lns
    [ "NOW = 1000.0"
    , "engine = { logWarn = function(...) end, logInfo = function(...) end,"
    , "  gameTime = function() return NOW end }"
    ]

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

-- | Same as 'runsOk', but first pushes each (name, bytes) pair as a
--   GLOBAL Lua string (via 'Lua.pushstring' -- a Lua string is an
--   arbitrary byte string, not required to be UTF-8, exactly like the
--   real @data_codec.lua@ wire payload this exists to inject) before
--   running the chunk. Lets a chunk reference a tracked fixture's REAL
--   on-disk bytes (e.g. @FIXTURE@) by name instead of re-synthesizing
--   the payload inline via @codec.encode@ -- proving the tracked file
--   itself, not merely this test's own encoder output, is what
--   @saveModules.prepareLoad@ accepts (issue #766, save-overhaul C4).
runsOkWithPayloads ∷ [(Text, BS.ByteString)] → Text → Expectation
runsOkWithPayloads payloads chunkText = do
    result ← Lua.run @Lua.Exception $ do
        Lua.openlibs
        _ ← Lua.dostring (TE.encodeUtf8 engineStub)
        forM_ payloads $ \(name, bytes) → do
            Lua.pushstring bytes
            Lua.setglobal (Lua.Name (TE.encodeUtf8 name))
        status ← Lua.dostring (TE.encodeUtf8 chunkText)
        case status of
            Lua.OK → return Nothing
            _ → do
                err ← Lua.tostring (-1)
                return (Just (maybe "<no message>" TE.decodeUtf8Lenient err))
    case result of
        Nothing  → pure ()
        Just msg → expectationFailure (T.unpack msg)

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

-- | Issue #900: a per-entity component whose @apply@ is nothing but an
--   'applyEntityRows' call against @live@, the shape both production
--   per-entity callbacks have. Module-level since issue #1279's isolation
--   block needs the same fixture as the #900 filtering block.
perEntitySpec ∷ Text → Text → Text
perEntitySpec name live = T.concat
    [ "{ version=1, inputVersions={1}, required=true, "
    , "scope='global', deps={},"
    , "  snapshot=function()"
    , "    local out = {}"
    , "    for k, v in pairs(", live, ") do out[k] = v end"
    , "    return out end,"
    , "  decode=function(v,d) return d end,"
    , "  validate=function() return nil end,"
    , "  apply=function(data, entities)"
    , "    require('scripts.lib.save_modules').applyEntityRows("
    , live, ", data, entities,"
    , "      { kind='unit', component='", name, "' })"
    , "  end }"
    ]

-- | Collect @engine.logWarn@ text into a Lua-local @warnings@ array, so a
--   chunk can assert on the absent-owner drop diagnostic.
captureWarnings ∷ [Text]
captureWarnings =
    [ "local warnings = {}"
    , "engine.logWarn = function(msg)"
    , "  warnings[#warnings + 1] = tostring(msg) end"
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


-- | Everything the REAL @lua.unit_ai@ component and
--   @scripts/unit_ai_reconcile.lua@ reach outside a live engine, plus
--   the registration and the reconciliation context both #1589 cases
--   share (issue #1589).
--
--   @CTX@ describes a restored two-page session: item instance 900
--   exists session-wide, unit 1 lives on page A and unit 2 on page B,
--   BOTH pages carry their own bill 5, and ground item 7 exists on page
--   A only. @MULE_MOVES@ counts the item hand-backs
--   @unit_ai_repair.lua@'s abort path performs, which is how the
--   repairJob drop proves it went through that path rather than a bare
--   field assignment.
unitAiReconcilePrelude ∷ [Text]
unitAiReconcilePrelude =
    [ "package.loaded['scripts.unit_ai'] = {}"
    , "package.loaded['scripts.movement_speed'] = {"
    , "  comfort = function() return 1.0 end, ordered = function() return 1.15 end,"
    , "  sprint = function() return 2.0 end, meander = function() return 0.5 end }"
    , "LOG, MULE_MOVES = {}, 0"
    , "engine = { gameTime = function() return 1000 end,"
    , "  logInfo = function(m) LOG[#LOG + 1] = m end,"
    , "  logWarn = function() end, logError = function() end,"
    , "  emitEventForUnit = function() end, loadYaml = function() return nil end }"
    , "unit = { exists = function() return true end,"
    -- #1673: the AI pairs every candidate with the ACTING unit's own
    -- page, so the stub world needs one; everyone shares it here.
    , "  getInfo = function(u)"
    , "    if u == 77 then"
    , "      return { gridX = 0, gridY = 0, defName = 'technomule',"
    , "               page = 'stub_page' } end"
    , "    return { gridX = 0, gridY = 0, defName = 'acolyte',"
    , "             page = 'stub_page' } end,"
    , "  getAllIds = function() return { 77 } end,"
    , "  getStat = function() return 1.0 end, getSkill = function() return 25.0 end,"
    , "  getInventory = function() return {} end,"
    , "  transferItemToUnit = function() MULE_MOVES = MULE_MOVES + 1"
    , "    return true end,"
    , "  moveTo = function() end, stop = function() end, addXP = function() end,"
    , "  setAnimOverride = function() end, clearAnimOverride = function() end }"
    , "world = { getActiveWorldId = function() return 'A' end,"
    , "  getLocationInstance = function() return nil end }"
    , "craft = { get = function(id)"
    , "  if id == 'known_recipe' then return { id = id } end end }"
    , "repair = { get = function(id)"
    , "  if id == 'known_repair' then return { id = id } end end,"
    , "  repairAt = function() return true end }"
    , "item = { listGround = function() return {} end,"
    , "  -- #1666: owning-page ground lookup; nothing on this page."
    , "  getGroundForUnit = function() return nil, true end,"
    , "  listDefs = function() return { { name = 'axe_steel' },"
    , "                                 { name = 'whetstone' } } end }"
    , "building = { findStation = function() return nil end,"
    , "  getInfo = function() return nil end,"
    , "  listDefs = function() return { { name = 'hut' } } end }"
    , "flora = { exists = function() return true end }"
    , "aiState = {}"
    , "require('scripts.unit_ai_save').register(aiState)"
    , "reconcile = require('scripts.unit_ai_reconcile')"
    , "saveModules = require('scripts.lib.save_modules')"
    , "codec = require('scripts.lib.data_codec')"
    , "CTX = { item_instance = { [900] = true },"
    , "        unitPage = { [1] = 'A', [2] = 'B' },"
    , "        byPage = { craft_bill = { A = { [5] = true }, B = { [5] = true } },"
    , "                   ground_item = { A = { [7] = true }, B = {} } } }"
    ]

-- | Issue #2055's shared stubs. The fill it covers happens at the
--   POST-PUBLISH reconcile, so these cases have to drive the real
--   @scripts/unit_ai_reconcile.lua@ too, not just prepareLoad/applyAll
--   — hence 'unitAiReconcilePrelude' as the base rather than a fresh
--   set of stubs.
--
--   What is added on top is a SETTABLE clock. The base prelude pins
--   @engine.gameTime@ to a constant; these cases need to move it,
--   because the whole reason the fill is at reconcile rather than at
--   decode is that staging and the restored session read different
--   values. @NOW@ starts at the base prelude's own 1000 so the cases
--   that do not care about the clock read the same number it always
--   did.
unitAiDefaultsPrelude ∷ [Text]
unitAiDefaultsPrelude = unitAiReconcilePrelude ⧺
    [ "NOW = 1000.0"
    , "engine.gameTime = function() return NOW end"
    -- unit_ai_core requires unit_ai_hold, which requires movement_speed
    -- at module scope; the base prelude already stubs that.
    , "unit.exists = function() return true end"
    , "item.listDefs = function() return {} end"
    , "building.listDefs = function() return { { name = 'hut' } } end"
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

        -- Issue #1279: the limits are PAYLOAD limits. One caller -- an
        -- engine-generated snapshot that never reaches disk -- needs them
        -- lifted, without relaxing them for anything that does.
        it "takes a per-call limits override that lifts the payload caps \
           \for that call ALONE, leaving the module defaults and every \
           \other caller exactly as they were" $ runsOk $ lns
            [ "local codec = require('scripts.lib.data_codec')"
            , "local big = {}"
            , "for i = 1, codec.MAX_TABLE_ENTRIES + 1 do big[i] = true end"
            , "local capped, err = codec.encode(big)"
            , "assert(capped == nil, 'the default cap must still reject it')"
            , "assert(err:find('max entries', 1, true) ~= nil, err)"
            , "local free = codec.encode(big, codec.UNBOUNDED)"
            , "assert(free ~= nil, 'UNBOUNDED must encode it')"
            , "local back = codec.decode(free, codec.UNBOUNDED)"
            , "assert(back ~= nil and #back == codec.MAX_TABLE_ENTRIES + 1"
            , "  and back[1] == true and back[#back] == true,"
            , "  'it must round-trip through the same allowance')"
            -- A payload written with an allowance its reader lacks would
            -- be unreadable; the caps stay symmetric on purpose.
            , "assert(codec.decode(free) == nil, 'the DEFAULT decode must "
              <> "still refuse it -- the override is not sticky')"
            , "assert(codec.MAX_TABLE_ENTRIES == 200000"
            , "  and codec.MAX_DEPTH == 64"
            , "  and codec.MAX_STRING_BYTES == 4 * 1024 * 1024"
            , "  and codec.MAX_TOTAL_BYTES == 16 * 1024 * 1024,"
            , "  'the module defaults must be untouched by an override')"
            , "assert(codec.decode(codec.encode({a=1})) ~= nil,"
            , "  'an ordinary call still behaves exactly as before')"
            ]

        it "honours a PARTIAL limits override, keeping the module default \
           \for every key the caller did not name" $ runsOk $ lns
            [ "local codec = require('scripts.lib.data_codec')"
            , "local big = {}"
            , "for i = 1, codec.MAX_TABLE_ENTRIES + 1 do big[i] = true end"
            , "local only = { MAX_TABLE_ENTRIES = math.huge }"
            , "assert(codec.encode(big, only) ~= nil,"
            , "  'the named key must be lifted')"
            , "local deep = {}"
            , "local cur = deep"
            , "for _ = 1, codec.MAX_DEPTH + 2 do"
            , "  cur.next = {}; cur = cur.next end"
            , "local d, derr = codec.encode(deep, only)"
            , "assert(d == nil and derr:find('max depth', 1, true) ~= nil,"
            , "  'an unnamed key must keep its default: ' .. tostring(derr))"
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

        -- Issue #865: %.17g always coerced a number to a float before
        -- formatting, so an integer above 2^53 silently lost precision
        -- and a whole-valued float's subtype flattened to an integer on
        -- decode. The cases below drive the fixed I/D-tag encoding
        -- directly, plus the legacy N-tag's frozen decode-compatibility
        -- contract for already-written saves.
        it "round-trips a Lua integer above 2^53 exactly, preserving both \
           \value and subtype (issue #865)" $ runsOk $ lns
            [ "local codec = require('scripts.lib.data_codec')"
            , "local v = 9007199254740993"
            , "local back = codec.decode(codec.encode(v))"
            , "assert(back == v, 'expected exact round-trip of 2^53+1, got ' .. tostring(back))"
            , "assert(math.type(back) == 'integer',"
            , "  'expected integer subtype, got ' .. tostring(math.type(back)))"
            ]

        it "round-trips math.maxinteger and math.mininteger exactly, both \
           \staying integers (issue #865) -- mininteger's decimal \
           \magnitude overflows Lua's own SOURCE-literal integer parser \
           \to a float, which is exactly where a naive digit-based fix \
           \fails; data_codec's tonumber()-based string-to-number path \
           \is not the source-literal parser and handles it correctly" $
            runsOk $ lns
            [ "local codec = require('scripts.lib.data_codec')"
            , "local maxi = math.maxinteger"
            , "local backMax = codec.decode(codec.encode(maxi))"
            , "assert(backMax == maxi, 'maxinteger round-trip failed: ' .. tostring(backMax))"
            , "assert(math.type(backMax) == 'integer',"
            , "  'maxinteger subtype must stay integer, got ' .. tostring(math.type(backMax)))"
            , "local mini = math.mininteger"
            , "local backMin = codec.decode(codec.encode(mini))"
            , "assert(backMin == mini, 'mininteger round-trip failed: ' .. tostring(backMin))"
            , "assert(math.type(backMin) == 'integer',"
            , "  'mininteger subtype must stay integer, got ' .. tostring(math.type(backMin)))"
            ]

        it "keeps a whole-valued float's subtype on decode instead of \
           \collapsing it to an integer (issue #865)" $ runsOk $ lns
            [ "local codec = require('scripts.lib.data_codec')"
            , "local backF = codec.decode(codec.encode(3.0))"
            , "assert(backF == 3.0, 'expected 3.0 to round-trip')"
            , "assert(math.type(backF) == 'float',"
            , "  'expected float subtype to survive, got ' .. tostring(math.type(backF)))"
            ]

        it "preserves -0.0's sign through a round trip (issue #865)" $
            runsOk $ lns
            [ "local codec = require('scripts.lib.data_codec')"
            , "local backZ = codec.decode(codec.encode(-0.0))"
            , "assert(math.type(backZ) == 'float', 'expected -0.0 to decode as a float')"
            , "assert(1 / backZ == -math.huge,"
            , "  'expected -0.0 sign to survive, got 1/x = ' .. tostring(1 / backZ))"
            ]

        it "keeps two distinct integer map keys above 2^53 as two distinct \
           \entries, in canonical ascending order regardless of insertion \
           \order (issue #865) -- the pre-fix %.17g encoding formatted \
           \both keys identically, so encode() itself produced a payload \
           \its own decode() rejected as a duplicate key" $ runsOk $ lns
            [ "local codec = require('scripts.lib.data_codec')"
            , "local m1 = { [9007199254740993] = 'a', [9007199254740992] = 'b' }"
            , "local back = codec.decode(codec.encode(m1))"
            , "assert(back[9007199254740993] == 'a', 'expected distinct key 2^53+1 to survive')"
            , "assert(back[9007199254740992] == 'b', 'expected distinct key 2^53 to survive')"
            , "local count = 0"
            , "for _ in pairs(back) do count = count + 1 end"
            , "assert(count == 2, 'expected exactly 2 distinct map entries, got ' .. count)"
            , "local m2 = { [9007199254740992] = 'b', [9007199254740993] = 'a' }"
            , "assert(codec.encode(m1) == codec.encode(m2),"
            , "  'insertion order must not affect canonical byte output')"
            ]

        it "round-trips finite-float boundaries: the smallest positive \
           \subnormal and the largest finite double (issue #865)" $
            runsOk $ lns
            [ "local codec = require('scripts.lib.data_codec')"
            , "local subnormal = 2^-1074"
            , "local backSub = codec.decode(codec.encode(subnormal))"
            , "assert(backSub == subnormal, 'smallest positive subnormal must round-trip exactly')"
            , "assert(math.type(backSub) == 'float', 'subnormal must decode as float')"
            , "local hugeFinite = 1.7976931348623157e+308"
            , "local backHuge = codec.decode(codec.encode(hugeFinite))"
            , "assert(backHuge == hugeFinite, 'largest finite double must round-trip exactly')"
            , "assert(math.type(backHuge) == 'float', 'largest finite double must decode as float')"
            ]

        it "rejects a hand-crafted I-tag payload whose digits overflow the \
           \64-bit integer range, and a D-tag payload that is not exactly \
           \8 bytes or that decodes to a non-finite value (issue #865) -- \
           \the new wire forms get the same loud, path-named rejection as \
           \every other malformed/subtype-inconsistent shape" $ runsOk $ lns
            [ "local codec = require('scripts.lib.data_codec')"
            , "local d1, e1 = codec.decode('I20:99999999999999999999')"
            , "assert(d1 == nil and e1 ~= nil,"
            , "  'decode must reject out-of-int64-range I-tag digits')"
            , "local d2, e2 = codec.decode('D3:abc')"
            , "assert(d2 == nil and e2 ~= nil,"
            , "  'decode must reject a D-tag payload that is not exactly 8 bytes')"
            , "local nanBytes = string.pack('<d', 0/0)"
            , "local d3, e3 = codec.decode('D' .. #nanBytes .. ':' .. nanBytes)"
            , "assert(d3 == nil and e3 ~= nil,"
            , "  'decode must reject a hand-crafted D-tag NaN payload')"
            , "local infBytes = string.pack('<d', 1/0)"
            , "local d4, e4 = codec.decode('D' .. #infBytes .. ':' .. infBytes)"
            , "assert(d4 == nil and e4 ~= nil,"
            , "  'decode must reject a hand-crafted D-tag +inf payload')"
            ]

        it "rejects an I-tag body that is lexically not the exact %d form \
           \data_codec's own encoder produces, even though plain \
           \tonumber() would accept it (round-1 review, issue #865) -- \
           \hex, a leading '+', interior whitespace, and a leading zero \
           \must all be rejected, not silently coerced to an integer" $
            runsOk $ lns
            [ "local codec = require('scripts.lib.data_codec')"
            , "local badForms = {'I4:0x10', 'I2: 1', 'I3:+42', 'I2:01', 'I2:-0'}"
            , "for _, payload in ipairs(badForms) do"
            , "  local d, e = codec.decode(payload)"
            , "  assert(d == nil and e ~= nil,"
            , "    'expected rejection for non-canonical I-tag body: ' .. payload)"
            , "end"
            ]

        it "encodes a representative integer and float to their exact, \
           \hard-coded canonical wire bytes (round-1 review, issue #865) \
           \-- locks the tag, length prefix, and byte content/order \
           \themselves, independently of string.pack, so a tag or \
           \byte-order regression cannot hide behind a round-trip-only \
           \assertion" $ runsOk $ lns
            [ "local codec = require('scripts.lib.data_codec')"
            , "assert(codec.encode(42) == 'I2:42',"
            , "  'expected the canonical I-tag encoding of 42: ' .. tostring(codec.encode(42)))"
            , "-- 1.5 as IEEE-754 binary64 is the well-known bit pattern"
            , "-- 0x3FF8000000000000; little-endian byte order is that"
            , "-- reversed, i.e. bytes {0,0,0,0,0,0,0xF8,0x3F} -- spelled"
            , "-- out via string.char, independently of string.pack, so"
            , "-- this catches a byte-order or packing regression rather"
            , "-- than merely re-deriving the same bytes the encoder used."
            , "local expectedFloatBytes = string.char(0, 0, 0, 0, 0, 0, 0xF8, 0x3F)"
            , "local expected = 'D8:' .. expectedFloatBytes"
            , "assert(codec.encode(1.5) == expected,"
            , "  'expected the canonical D-tag encoding of 1.5')"
            ]

        it "decodes legacy (pre-#865) N-tag payloads to exactly the values \
           \today's decoder already produces -- wire compatibility with \
           \already-written saves (issue #865 requirement 5): a whole \
           \digit string stays an integer, a decimal/exponent form stays \
           \a float, and a legacy negative-zero digit string loses its \
           \sign, all unchanged by this fix" $ runsOk $ lns
            [ "local codec = require('scripts.lib.data_codec')"
            , "local v1 = codec.decode('N1:3')"
            , "assert(v1 == 3 and math.type(v1) == 'integer',"
            , "  'legacy N1:3 must still decode to integer 3')"
            , "local v2 = codec.decode('N2:-0')"
            , "assert(v2 == 0 and math.type(v2) == 'integer' and 1 / v2 == math.huge,"
            , "  'legacy N2:-0 must still decode to positive integer zero (sign lost)')"
            , "local v3 = codec.decode('N16:9007199254740992')"
            , "assert(v3 == 9007199254740992 and math.type(v3) == 'integer',"
            , "  'legacy N16:9007199254740992 must still decode to that exact integer')"
            , "local v4 = codec.decode('N22:9.2233720368547758e+18')"
            , "assert(v4 == 9.2233720368547758e+18 and math.type(v4) == 'float',"
            , "  'legacy N22 maxinteger-corruption digits must still decode to that exact float')"
            , "local v5 = codec.decode('N23:-9.2233720368547758e+18')"
            , "assert(v5 == -9.2233720368547758e+18 and math.type(v5) == 'float',"
            , "  'legacy N23 mininteger-corruption digits must still decode to that exact float')"
            ]

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

    describe "per-entity component application (issue #900)" $ do
        it "applies each row against the restored session's own entity \
           \set: a row whose owner is absent is dropped with a diagnostic \
           \while its siblings apply normally" $ runsOk $ lns $
            [ "local saveModules = require('scripts.lib.save_modules')"
            , "local codec = require('scripts.lib.data_codec')"
            ] ⧺ captureWarnings ⧺
            [ "local live = {}"
            , "saveModules.register('pe_drop', "
              <> perEntitySpec "pe_drop" "live" <> ")"
            , "local prep = saveModules.prepareLoad("
            , "  { { id='pe_drop', version=1,"
            , "      payload=codec.encode({ [7]={tag='seven'},"
            , "                             [9]={tag='nine'} }) } },"
            , "  1, false, { unit = { [7]=true }, building = {} })"
            , "assert(prep.ok, 'an absent owner is tolerated-dangling, "
              <> "never a load failure: ' .. tostring(prep.errors and prep.errors[1]))"
            , "saveModules.applyAll()"
            , "assert(live[7] ~= nil and live[7].tag == 'seven',"
            , "  'a row whose unit is in the restored session must apply')"
            , "assert(live[9] == nil,"
            , "  'a row whose unit is absent must be dropped, not applied')"
            , "assert(#warnings == 1, 'the drop must emit exactly one "
              <> "diagnostic, got ' .. #warnings)"
            , "assert(warnings[1]:find('9', 1, true) ~= nil,"
            , "  'the diagnostic must name the dropped entity: ' .. warnings[1])"
            ]

        it "keeps structurally malformed rows FAIL-FAST even when their \
           \owner is absent from the restored session -- only unresolved \
           \OWNERSHIP is tolerated-and-dropped, never malformed data" $
            runsOk $ lns
            [ "local saveModules = require('scripts.lib.save_modules')"
            , "local codec = require('scripts.lib.data_codec')"
            , "local live = {}"
            , "saveModules.register('pe_strict', { version=1, "
              <> "inputVersions={1}, required=true, scope='global', deps={},"
            , "  snapshot=function() return {} end,"
            , "  decode=function(v,d) return d end,"
            , "  validate=function(d)"
            , "    local errs = {}"
            , "    for uid, row in pairs(d) do"
            , "      if type(row) ~= 'table' then"
            , "        errs[#errs+1] = 'row ' .. tostring(uid)"
            , "          .. ' is not a table' end"
            , "    end"
            , "    if #errs > 0 then return errs end"
            , "    return nil end,"
            , "  apply=function(data, entities)"
            , "    saveModules.applyEntityRows(live, data, entities,"
            , "      { kind='unit', component='pe_strict' })"
            , "  end })"
            , "local prep = saveModules.prepareLoad("
            , "  { { id='pe_strict', version=1,"
            , "      payload=codec.encode({ [4]='not_a_table' }) } },"
            , "  1, false, { unit = {}, building = {} })"
            , "assert(not prep.ok, 'a malformed row must fail the whole "
              <> "load even though its owner is absent')"
            ]

        it "leaves the module holding EXACTLY the payload's applicable \
           \rows: a live row with no row of its own in the payload does \
           \not survive, even when its id exists in the restored session \
           \(ids are session-global allocators that restart per session, \
           \so a merge would let the new session's unit 7 inherit the old \
           \session's unit 7 state)" $ runsOk $ lns
            [ "local saveModules = require('scripts.lib.save_modules')"
            , "local codec = require('scripts.lib.data_codec')"
            , "local live = { [7] = { tag='OLD SESSION' } }"
            , "saveModules.register('pe_exact', "
              <> perEntitySpec "pe_exact" "live" <> ")"
            , "local prep = saveModules.prepareLoad("
            , "  { { id='pe_exact', version=1,"
            , "      payload=codec.encode({ [9]={tag='nine'} }) } },"
            , "  1, false, { unit = { [7]=true, [9]=true }, building = {} })"
            , "assert(prep.ok)"
            , "saveModules.applyAll()"
            , "assert(live[7] == nil, 'a pre-load row absent from the "
              <> "payload must NOT survive, even though unit 7 exists in "
              <> "the restored session')"
            , "assert(live[9] ~= nil and live[9].tag == 'nine')"
            ]

        it "rolls a failed apply back VERBATIM, with no ownership \
           \filtering -- the rollback capture is the OLD session's \
           \snapshot, whose owners are by definition absent from the \
           \RESTORED context, so filtering it would erase exactly the \
           \state the rollback exists to restore" $ runsOk $ lns
            [ "local saveModules = require('scripts.lib.save_modules')"
            , "local codec = require('scripts.lib.data_codec')"
            , "local live = { [42] = { tag='preload' } }"
            , "saveModules.register('pe_a_rollback', "
              <> perEntitySpec "pe_a_rollback" "live" <> ")"
            , "-- Sorts after pe_a_rollback, so it applies second."
            , "saveModules.register('pe_z_boom', { version=1, "
              <> "inputVersions={1}, required=true, scope='global', deps={},"
            , "  snapshot=function() return {} end,"
            , "  decode=function(v,d) return d end,"
            , "  validate=function() return nil end,"
            , "  apply=function() error('synthetic apply failure') end })"
            , "local prep = saveModules.prepareLoad("
            , "  { { id='pe_a_rollback', version=1,"
            , "      payload=codec.encode({ [9]={tag='nine'} }) },"
            , "    { id='pe_z_boom', version=1, payload=codec.encode({}) } },"
            , "  1, false, { unit = { [9]=true }, building = {} })"
            , "assert(prep.ok)"
            , "local ok = pcall(saveModules.applyAll)"
            , "assert(not ok, 'the failing component must abort the apply')"
            , "assert(live[42] ~= nil and live[42].tag == 'preload',"
            , "  'rollback must restore the pre-load row verbatim even "
              <> "though unit 42 is absent from the restored session')"
            , "assert(live[9] == nil, 'the rolled-back component must not "
              <> "retain the new session rows')"
            ]

        it "resolves per-page nested references through the OWNING unit's \
           \page, so equal per-page ids on two different pages neither \
           \alias nor contaminate one another (rows themselves stay keyed \
           \by the session-global unit id and are never re-keyed per page)" $
            runsOk $ lns
            [ "local saveModules = require('scripts.lib.save_modules')"
            , "local codec = require('scripts.lib.data_codec')"
            , "local live = {}"
            , "saveModules.register('pe_pages', { version=1, "
              <> "inputVersions={1}, required=true, scope='global', deps={},"
            , "  snapshot=function() return {} end,"
            , "  decode=function(v,d) return d end,"
            , "  validate=function() return nil end,"
            , "  apply=function(data, entities)"
            , "    saveModules.applyEntityRows(live, data, entities,"
            , "      { kind='unit', component='pe_pages' })"
            , "    -- A billId is a PER-PAGE allocator: it only means"
            , "    -- anything relative to its owning unit's page."
            , "    for uid, row in pairs(live) do"
            , "      row.billPage = entities and entities.unitPage"
            , "        and entities.unitPage[uid] or nil"
            , "    end"
            , "  end })"
            , "local prep = saveModules.prepareLoad("
            , "  { { id='pe_pages', version=1,"
            , "      payload=codec.encode({ [3]={billId=1}, [4]={billId=1} }) } },"
            , "  1, false, { unit = { [3]=true, [4]=true }, building = {},"
            , "              unitPage = { [3]='alpha', [4]='beta' } })"
            , "assert(prep.ok)"
            , "saveModules.applyAll()"
            , "assert(live[3].billId == 1 and live[4].billId == 1,"
            , "  'both pages legitimately carry bill id 1')"
            , "assert(live[3].billPage == 'alpha', 'unit 3 resolves to its "
              <> "own page, got ' .. tostring(live[3].billPage))"
            , "assert(live[4].billPage == 'beta', 'unit 4 resolves to its "
              <> "own page, got ' .. tostring(live[4].billPage))"
            ]

        it "treats a CONTEXTLESS applyAll as 'apply every row' rather than \
           \'the restored session is empty', so every pre-#900 caller \
           \keeps working unchanged" $ runsOk $ lns
            [ "local saveModules = require('scripts.lib.save_modules')"
            , "local codec = require('scripts.lib.data_codec')"
            , "local live = {}"
            , "saveModules.register('pe_nocontext', "
              <> perEntitySpec "pe_nocontext" "live" <> ")"
            , "local prep = saveModules.prepareLoad("
            , "  { { id='pe_nocontext', version=1,"
            , "      payload=codec.encode({ [7]={tag='seven'},"
            , "                             [9]={tag='nine'} }) } })"
            , "assert(prep.ok)"
            , "saveModules.applyAll()"
            , "assert(live[7] ~= nil and live[9] ~= nil,"
            , "  'no context must mean no ownership filtering')"
            ]

    -- Issue #1279: #900 requirement 1 promised apply() "a read-only
    -- restored-entity context" and shipped one shared MUTABLE table, so an
    -- earlier component could delete or rewrite entries a later one still
    -- filters against -- making apply ORDER load-correctness-relevant and
    -- contradicting Haskell's authoritative KnownEntities. The production
    -- callbacks only read it, so this was latent; nothing enforced it.
    --
    -- The mechanism is an independent per-component COPY, so these cases
    -- assert the observable guarantee (isolation) rather than a raise:
    -- a component's mutations land on its own copy and reach nobody.
    describe "restored-entity context isolation (issue #1279)" $ do
        -- Sorts before the observer, so it applies FIRST -- the ordering
        -- this whole block exists to make irrelevant. The `entities == nil`
        -- guard is the ROLLBACK pass, contextless by design (#900).
        let mutatorSpec body = T.concat
                [ "{ version=1, inputVersions={1}, required=true, "
                , "scope='global', deps={},"
                , "  snapshot=function() return {} end,"
                , "  decode=function(v,d) return d end,"
                , "  validate=function() return nil end,"
                , "  apply=function(data, entities)"
                , "    if entities == nil then return end "
                , body, " end }"
                ]

        it "keeps a LATER component's view of membership and owner pages \
           \exactly as Haskell supplied it after an earlier component \
           \deletes, inserts, rewrites an owner page, rebinds an outer \
           \field and rawsets -- and applyEntityRows still retains the \
           \present-owner row while dropping the genuinely absent-owner one" $
            runsOk $ lns $
            [ "local saveModules = require('scripts.lib.save_modules')"
            , "local codec = require('scripts.lib.data_codec')"
            ] ⧺ captureWarnings ⧺
            [ "local live = {}"
            , "local seen = {}"
            -- Every write here SUCCEEDS -- on this component's own copy.
            -- The point is that none of them is observable downstream.
            , "saveModules.register('iso_a_mutator', "
              <> mutatorSpec (T.concat
                  [ "entities.unit[7] = nil;"
                  , "entities.unit[9] = true;"
                  , "entities.unitPage[7] = 'HIJACKED';"
                  , "entities.building[1] = true;"
                  , "rawset(entities, 'unit', { [999]=true })"
                  ]) <> ")"
            , "saveModules.register('iso_z_observer', { version=1, "
              <> "inputVersions={1}, required=true, scope='global', deps={},"
            , "  snapshot=function() return {} end,"
            , "  decode=function(v,d) return d end,"
            , "  validate=function() return nil end,"
            , "  apply=function(data, entities)"
            , "    seen.unit7 = entities.unit[7]"
            , "    seen.unit9 = entities.unit[9]"
            , "    seen.unit999 = entities.unit[999]"
            , "    seen.page7 = entities.unitPage[7]"
            , "    seen.building1 = entities.building[1]"
            , "    saveModules.applyEntityRows(live, data, entities,"
            , "      { kind='unit', component='iso_z_observer' })"
            , "  end })"
            , "local prep = saveModules.prepareLoad("
            , "  { { id='iso_a_mutator', version=1, payload=codec.encode({}) },"
            , "    { id='iso_z_observer', version=1,"
            , "      payload=codec.encode({ [7]={tag='seven'},"
            , "                             [9]={tag='nine'} }) } },"
            , "  1, false, { unit = { [7]=true }, building = {},"
            , "              unitPage = { [7]='alpha' } })"
            , "assert(prep.ok, tostring(prep.errors and prep.errors[1]))"
            , "saveModules.applyAll()"
            , "assert(seen.unit7 == true, 'a deleted membership entry must "
              <> "still be present downstream, got ' .. tostring(seen.unit7))"
            , "assert(seen.unit9 == nil, 'an inserted membership entry must "
              <> "not reach downstream, got ' .. tostring(seen.unit9))"
            , "assert(seen.unit999 == nil, 'a rawset table must not reach "
              <> "downstream, got ' .. tostring(seen.unit999))"
            , "assert(seen.page7 == 'alpha', 'a rewritten owner page must "
              <> "not reach downstream, got ' .. tostring(seen.page7))"
            , "assert(seen.building1 == nil, 'a different kind is isolated "
              <> "too, got ' .. tostring(seen.building1))"
            , "assert(live[7] ~= nil and live[7].tag == 'seven',"
            , "  'the present-owner row must still be retained')"
            , "assert(live[9] == nil, 'the absent-owner row must still be "
              <> "dropped -- an earlier insert must not have rescued it')"
            , "assert(#warnings == 1, 'exactly one drop diagnostic, got '"
            , "  .. #warnings)"
            ]

        it "hands every component a table that shares NOTHING -- neither \
           \with Haskell's own table nor with any sibling's copy -- so \
           \there is no shared source left for a mutation to reach \
           \through, by any route" $ runsOk $ lns
            [ "local saveModules = require('scripts.lib.save_modules')"
            , "local codec = require('scripts.lib.data_codec')"
            , "local source = { unit = { [7]=true }, building = {},"
            , "                 unitPage = { [7]='alpha' } }"
            , "local got = {}"
            , "saveModules.register('idn_a', "
              <> mutatorSpec "got[#got + 1] = entities" <> ")"
            , "saveModules.register('idn_b', "
              <> mutatorSpec "got[#got + 1] = entities" <> ")"
            , "local prep = saveModules.prepareLoad("
            , "  { { id='idn_a', version=1, payload=codec.encode({}) },"
            , "    { id='idn_b', version=1, payload=codec.encode({}) } },"
            , "  1, false, source)"
            , "assert(prep.ok)"
            , "saveModules.applyAll()"
            , "assert(#got == 2, 'both components must have applied')"
            , "assert(got[1] ~= got[2], 'each component needs its OWN copy')"
            , "assert(got[1] ~= source and got[2] ~= source,"
            , "  'no component may receive Haskell own table')"
            , "assert(got[1].unit ~= got[2].unit"
            , "  and got[1].unit ~= source.unit,"
            , "  'the NESTED maps must be copied too, not shared')"
            , "assert(got[1].unitPage ~= got[2].unitPage"
            , "  and got[1].unitPage ~= source.unitPage,"
            , "  'every nested map, not just the ones named unit')"
            , "-- The values themselves are still exactly Haskell answer."
            , "assert(got[2].unit[7] == true and got[2].unitPage[7] == 'alpha')"
            , "assert(source.unit[7] == true and source.unitPage[7] == 'alpha',"
            , "  'the source itself must come through untouched')"
            ]

        it "is unaffected by an earlier component reaching AROUND its own \
           \context to mutate the module's pending source directly, since \
           \the encoded snapshot every context is rebuilt from is taken -- \
           \and the raw source dropped -- before the first apply runs" $
            runsOk $ lns $
            [ "local saveModules = require('scripts.lib.save_modules')"
            , "local codec = require('scripts.lib.data_codec')"
            ] ⧺ captureWarnings ⧺
            [ "local live = {}"
            , "local seen = {}"
            -- _pendingEntities is a public field on the module table, so
            -- this is a route that needs no debug library and no reference
            -- captured from the value the component was handed.
            , "saveModules.register('src_a_mutator', "
              <> mutatorSpec (T.concat
                  [ "local sm = require('scripts.lib.save_modules');"
                  , "seen.pendingCleared = (sm._pendingEntities == nil);"
                  , "if sm._pendingEntities ~= nil then"
                  , "  sm._pendingEntities.unit[7] = nil;"
                  , "  sm._pendingEntities.unitPage[7] = 'HIJACKED' end;"
                  , "sm._pendingEntities = { unit = { [999]=true },"
                  , "  building = {}, unitPage = {} }"
                  ]) <> ")"
            , "saveModules.register('src_z_observer', { version=1, "
              <> "inputVersions={1}, required=true, scope='global', deps={},"
            , "  snapshot=function() return {} end,"
            , "  decode=function(v,d) return d end,"
            , "  validate=function() return nil end,"
            , "  apply=function(data, entities)"
            , "    seen.unit7 = entities.unit[7]"
            , "    seen.unit999 = entities.unit[999]"
            , "    seen.page7 = entities.unitPage[7]"
            , "    saveModules.applyEntityRows(live, data, entities,"
            , "      { kind='unit', component='src_z_observer' })"
            , "  end })"
            , "local prep = saveModules.prepareLoad("
            , "  { { id='src_a_mutator', version=1, payload=codec.encode({}) },"
            , "    { id='src_z_observer', version=1,"
            , "      payload=codec.encode({ [7]={tag='seven'},"
            , "                             [9]={tag='nine'} }) } },"
            , "  1, false, { unit = { [7]=true }, building = {},"
            , "              unitPage = { [7]='alpha' } })"
            , "assert(prep.ok, tostring(prep.errors and prep.errors[1]))"
            , "saveModules.applyAll()"
            , "assert(seen.pendingCleared, 'the raw source must already be "
              <> "gone by the time any component runs')"
            , "assert(seen.unit7 == true, 'the later component must still "
              <> "see unit 7 present, got ' .. tostring(seen.unit7))"
            , "assert(seen.unit999 == nil, 'a wholesale replacement of the "
              <> "pending source must not reach it either')"
            , "assert(seen.page7 == 'alpha', 'the later component must see "
              <> "Haskell owner page, got ' .. tostring(seen.page7))"
            , "assert(live[7] ~= nil and live[7].tag == 'seven',"
            , "  'the present-owner row must still be retained')"
            , "assert(live[9] == nil, 'the absent-owner row must still be "
              <> "dropped')"
            ]

        it "keeps no sibling context alive to be found: an earlier \
           \component sweeping the applyAll frame with debug.getlocal \
           \reaches only its own table and an immutable encoded string, \
           \never a later component's context" $ runsOk $ lns $
            [ "local saveModules = require('scripts.lib.save_modules')"
            , "local codec = require('scripts.lib.data_codec')"
            ] ⧺ captureWarnings ⧺
            [ "local live = {}"
            , "local seen = { mutatedTables = 0, strings = 0 }"
            -- Walk every local of every active frame above this one,
            -- exactly as the reachability concern describes, and hijack
            -- anything shaped like an entity context. `mine` is this
            -- component's OWN context, which it is of course allowed to
            -- reach; any OTHER one would be a sibling's. Deliberately
            -- narrow: `prepared` and the rollback captures also live in
            -- that frame, and they are legitimately shared per-component
            -- payloads -- this issue is about the entity CONTEXT.
            , "saveModules.register('dbg_a_mutator', "
              <> mutatorSpec (T.concat
                  [ "local mine = entities;"
                  , "for level = 2, 12 do"
                  , "  local i = 1;"
                  , "  while true do"
                  , "    local ok, name, value = pcall(debug.getlocal, level, i);"
                  , "    if not ok or name == nil then break end;"
                  , "    if type(value) == 'table' and value ~= mine"
                  , "      and (rawget(value, 'unit') ~= nil"
                  , "           or rawget(value, 'unitPage') ~= nil) then"
                  , "      seen.mutatedTables = seen.mutatedTables + 1;"
                  , "      pcall(function()"
                  , "        if value.unit ~= nil then value.unit[7] = nil end;"
                  , "        if value.unitPage ~= nil then"
                  , "          value.unitPage[7] = 'HIJACKED' end"
                  , "      end) end;"
                  , "    if type(value) == 'string' then"
                  , "      seen.strings = seen.strings + 1 end;"
                  , "    i = i + 1 end end"
                  ]) <> ")"
            , "saveModules.register('dbg_z_observer', { version=1, "
              <> "inputVersions={1}, required=true, scope='global', deps={},"
            , "  snapshot=function() return {} end,"
            , "  decode=function(v,d) return d end,"
            , "  validate=function() return nil end,"
            , "  apply=function(data, entities)"
            , "    seen.unit7 = entities.unit[7]"
            , "    seen.page7 = entities.unitPage[7]"
            , "    saveModules.applyEntityRows(live, data, entities,"
            , "      { kind='unit', component='dbg_z_observer' })"
            , "  end })"
            , "local prep = saveModules.prepareLoad("
            , "  { { id='dbg_a_mutator', version=1, payload=codec.encode({}) },"
            , "    { id='dbg_z_observer', version=1,"
            , "      payload=codec.encode({ [7]={tag='seven'},"
            , "                             [9]={tag='nine'} }) } },"
            , "  1, false, { unit = { [7]=true }, building = {},"
            , "              unitPage = { [7]='alpha' } })"
            , "assert(prep.ok, tostring(prep.errors and prep.errors[1]))"
            , "saveModules.applyAll()"
            , "assert(seen.unit7 == true, 'the later component must still "
              <> "see unit 7 present, got ' .. tostring(seen.unit7))"
            , "assert(seen.page7 == 'alpha', 'the later component must see "
              <> "Haskell owner page, got ' .. tostring(seen.page7))"
            , "assert(live[7] ~= nil and live[7].tag == 'seven',"
            , "  'the present-owner row must still be retained')"
            , "assert(live[9] == nil, 'the absent-owner row must still be "
              <> "dropped')"
            , "assert(seen.mutatedTables == 0, 'no sibling context may be "
              <> "reachable at all, found ' .. seen.mutatedTables)"
            -- The sweep has to have actually run, or this proves nothing.
            , "assert(seen.strings > 0, 'the sweep must have reached the "
              <> "applyAll frame -- the encoded context is a local string "
              <> "there; found none, so the walk never got that far')"
            ]

        it "is an ORDINARY table under every standard idiom -- next, pairs \
           \and getmetatable included -- so a component using raw iteration \
           \cannot silently conclude the restored session is empty" $
            runsOk $ lns
            [ "local saveModules = require('scripts.lib.save_modules')"
            , "local codec = require('scripts.lib.data_codec')"
            , "local seen = {}"
            -- `next` is a primitive: it consults no metatable at all, so a
            -- proxy shell would report an EMPTY context here.
            , "saveModules.register('iter_probe', "
              <> mutatorSpec (T.concat
                  [ "seen.nextOuter = (next(entities) ~= nil);"
                  , "seen.nextUnit = (next(entities.unit) ~= nil);"
                  , "seen.rawUnit7 = rawget(entities.unit, 7);"
                  , "seen.meta = getmetatable(entities);"
                  , "seen.outer = 0;"
                  , "for _ in pairs(entities) do"
                  , "  seen.outer = seen.outer + 1 end;"
                  , "seen.units = {};"
                  , "for uid in next, entities.unit do"
                  , "  seen.units[#seen.units + 1] = uid end"
                  ]) <> ")"
            , "local prep = saveModules.prepareLoad("
            , "  { { id='iter_probe', version=1, payload=codec.encode({}) } },"
            , "  1, false, { unit = { [7]=true }, building = {},"
            , "              unitPage = { [7]='alpha' } })"
            , "assert(prep.ok)"
            , "saveModules.applyAll()"
            , "assert(seen.nextOuter, 'next(entities) must see the context')"
            , "assert(seen.nextUnit, 'next(entities.unit) must see the "
              <> "restored membership -- a metatable-only view would not')"
            , "assert(seen.rawUnit7 == true, 'rawget must find real storage')"
            , "assert(seen.meta == nil, 'a plain table carries no metatable, "
              <> "so there is nothing for debug.getmetatable to reach into')"
            , "assert(seen.outer == 3, 'pairs() must yield every kind "
              <> "Haskell supplied, got ' .. tostring(seen.outer))"
            , "assert(#seen.units == 1 and seen.units[1] == 7,"
            , "  'raw iteration over a nested map must yield its membership')"
            ]

        -- The encoded snapshot rides data_codec, whose DEFAULT limits are
        -- PAYLOAD limits: MAX_TABLE_ENTRIES is 200,000 per table, and
        -- KnownEntities has no matching bound -- the context names every
        -- entity in the session while a component's payload carries only
        -- the rows it saved, so a session can exceed the cap with a tiny
        -- payload. This snapshot never reaches disk, so it passes
        -- UNBOUNDED; the same session must load, on the SAME immutable
        -- path as any other (no second, weaker mechanism for big worlds).
        it "applies a session whose entity set is past the codec's default \
           \per-table cap on the ordinary immutable path, keeping both the \
           \real membership and sibling isolation" $
            runsOk $ lns $
            [ "local saveModules = require('scripts.lib.save_modules')"
            , "local codec = require('scripts.lib.data_codec')"
            ] ⧺ captureWarnings ⧺
            [ "local live = {}"
            , "local seen = {}"
            , "local units = {}"
            , "for i = 1, codec.MAX_TABLE_ENTRIES + 1 do units[i] = true end"
            , "saveModules.register('big_a_mutator', "
              <> mutatorSpec (T.concat
                  [ "entities.unit[7] = nil;"
                  , "entities.unitPage[7] = 'HIJACKED'"
                  ]) <> ")"
            , "saveModules.register('big_z_observer', { version=1, "
              <> "inputVersions={1}, required=true, scope='global', deps={},"
            , "  snapshot=function() return {} end,"
            , "  decode=function(v,d) return d end,"
            , "  validate=function() return nil end,"
            , "  apply=function(data, entities)"
            , "    seen.unit7 = entities.unit[7]"
            , "    seen.page7 = entities.unitPage[7]"
            , "    seen.absent = entities.unit[999999]"
            , "    saveModules.applyEntityRows(live, data, entities,"
            , "      { kind='unit', component='big_z_observer' })"
            , "  end })"
            , "local prep = saveModules.prepareLoad("
            , "  { { id='big_a_mutator', version=1, payload=codec.encode({}) },"
            , "    { id='big_z_observer', version=1,"
            , "      payload=codec.encode({ [7]={tag='seven'},"
            , "                             [999999]={tag='gone'} }) } },"
            , "  1, false, { unit = units, building = {},"
            , "              unitPage = { [7]='alpha' } })"
            , "assert(prep.ok, tostring(prep.errors and prep.errors[1]))"
            , "saveModules.applyAll()"
            , "assert(seen.unit7 == true, 'the later component must still "
              <> "see unit 7 present, got ' .. tostring(seen.unit7))"
            , "assert(seen.page7 == 'alpha', 'an earlier mutation must "
              <> "still not reach it, got ' .. tostring(seen.page7))"
            , "assert(seen.absent == nil, 'the oversized set is still the "
              <> "REAL membership, not an everything-passes stand-in')"
            , "assert(live[7] ~= nil and live[7].tag == 'seven',"
            , "  'the present-owner row must still be retained')"
            , "assert(live[999999] == nil, 'the absent-owner row must still "
              <> "be dropped -- filtering is not skipped in this path')"
            -- The DEFAULT limits must be untouched by the override: this
            -- same context is still refused for anything disk-bound.
            , "assert(codec.encode({ unit = units }) == nil,"
            , "  'the payload default cap must still reject this context "
              <> "-- the allowance is per call, not a global relaxation')"
            , "assert(#warnings == 1, 'the dropped row must still report "
              <> "its diagnostic, got ' .. #warnings)"
            ]

        it "leaves the two documented compatibility semantics untouched: a \
           \nil context is still 'apply every row' and an EMPTY per-kind \
           \set still filters" $ runsOk $ lns
            [ "local saveModules = require('scripts.lib.save_modules')"
            , "local codec = require('scripts.lib.data_codec')"
            , "local liveNil, liveEmpty = {}, {}"
            , "saveModules.register('compat_nil', "
              <> perEntitySpec "compat_nil" "liveNil" <> ")"
            , "local prep = saveModules.prepareLoad("
            , "  { { id='compat_nil', version=1,"
            , "      payload=codec.encode({ [7]={tag='seven'} }) } },"
            , "  1, false, nil)"
            , "assert(prep.ok)"
            , "saveModules.applyAll()"
            , "assert(liveNil[7] ~= nil, 'a nil context must not filter')"
            , "saveModules.register('compat_empty', "
              <> perEntitySpec "compat_empty" "liveEmpty" <> ")"
            , "prep = saveModules.prepareLoad("
            , "  { { id='compat_nil', version=1, payload=codec.encode({}) },"
            , "    { id='compat_empty', version=1,"
            , "      payload=codec.encode({ [7]={tag='seven'} }) } },"
            , "  2, false, { unit = {}, building = {} })"
            , "assert(prep.ok)"
            , "saveModules.applyAll()"
            , "assert(liveEmpty[7] == nil, 'an empty per-kind set is a real "
              <> "answer and must still filter')"
            ]

    describe "unit_ai save component (issue #761 requirements 13/14)" $ do
        it "strips every transient *Candidate scratch field from the \
           \persisted snapshot -- craftCandidate in particular embeds a \
           \full live RecipeDef (craft.get()'s return value), which must \
           \never be copied into a save payload" $ runsOk $ lns
            [ "unit = { exists = function(_uid) return true end }"
            , "local unitAiSave = require('scripts.unit_ai_save')"
            , "local fakeAiState = { [1] = {"
            , "  currentAction = 'idle',"
            , "  craftCandidate = { bill = { id = 5, station = 10 },"
            , "    recipe = { id = 'x', inputs = { a = 1 }, outputs = { b = 2 },"
            , "               station = 'forge' }, demands = {}, dist = 3 },"
            , "  repairCandidate = { instanceId = 42, defName = 'axe' },"
            , "  digCandidate = { x = 3, y = 4 } } }"
            , "unitAiSave.register(fakeAiState)"
            , "local saveModules = require('scripts.lib.save_modules')"
            , "local snap = saveModules.registry.unit_ai.snapshot()"
            , "assert(snap[1] ~= nil, 'live unit state must still be present')"
            , "assert(snap[1].currentAction == 'idle', 'non-candidate fields survive')"
            , "assert(snap[1].craftCandidate == nil,"
            , "  'craftCandidate (which embeds a live RecipeDef) must be stripped')"
            , "assert(snap[1].repairCandidate == nil, 'repairCandidate must be stripped')"
            , "assert(snap[1].digCandidate == nil, 'digCandidate must be stripped')"
            , "-- The live singleton itself must be untouched (only the"
            , "-- SNAPSHOT copy is stripped) -- the AI loop still needs its"
            , "-- own in-memory candidate on this same tick."
            , "assert(fakeAiState[1].craftCandidate ~= nil,"
            , "  'stripping must not mutate the live aiState singleton')"
            , "-- The encoded payload itself must not contain the recipe id"
            , "-- as a smuggled string anywhere, proving no leftover copy"
            , "-- survives via some other path."
            , "local codec = require('scripts.lib.data_codec')"
            , "local payload = codec.encode(snap)"
            , "assert(payload:find('forge') == nil,"
            , "  'no trace of the live recipe content may reach the encoded payload')"
            ]

        it "rejects a load whose craftJob/repairJob reference a recipe or \
           \item def no longer registered (issue #761 round-4 review), \
           \during prepareLoad -- before any live state is touched -- and \
           \accepts one whose references all still resolve" $ runsOk $ lns
            [ "unit = { exists = function(_uid) return true end }"
            , "craft = { get = function(id)"
            , "  if id == 'known_recipe' then return { id = 'known_recipe' } end"
            , "  return nil end }"
            , "repair = { get = function(id)"
            , "  if id == 'known_repair' then return { id = 'known_repair' } end"
            , "  return nil end }"
            , "item = { listDefs = function()"
            , "  return { { name = 'wood' }, { name = 'stone' } } end }"
            , "local unitAiSave = require('scripts.unit_ai_save')"
            , "unitAiSave.register({})"
            , "local saveModules = require('scripts.lib.save_modules')"
            , "local codec = require('scripts.lib.data_codec')"
            , "local function prepareWith(state)"
            , "  return saveModules.prepareLoad({"
            , "    { id = 'unit_ai', version = 1, payload = codec.encode(state) },"
            , "  })"
            , "end"
            , "local removedRecipe = prepareWith({ [1] = { craftJob = {"
            , "  billId = 5, bid = 9, recipeId = 'removed_recipe', need = { wood = 2 } } } })"
            , "assert(not removedRecipe.ok,"
            , "  'a craftJob referencing a removed recipe must reject the load')"
            , "local removedItem = prepareWith({ [1] = { craftJob = {"
            , "  billId = 5, bid = 9, recipeId = 'known_recipe',"
            , "  fromGround = { unobtainium = 3 } } } })"
            , "assert(not removedItem.ok,"
            , "  'a craftJob fetch map referencing a removed item must reject the load')"
            , "local removedRepairRefs = prepareWith({ [1] = { repairJob = {"
            , "  instanceId = 900, recipeId = 'removed_recipe', defName = 'ghost_axe',"
            , "  consumable = 'ghost_wood' } } })"
            , "assert(not removedRepairRefs.ok,"
            , "  'a repairJob referencing removed content defs must reject the load')"
            , "local allPresent = prepareWith({ [1] = {"
            , "  craftJob = { billId = 5, bid = 9, recipeId = 'known_recipe',"
            , "               need = { wood = 2 }, fromGround = { stone = 1 } },"
            , "} })"
            , "assert(allPresent.ok,"
            , "  'a craftJob whose recipe/items all still exist must not be rejected: '"
            , "  .. table.concat(allPresent.errors or {}, '; '))"
            , "local repairPresent = prepareWith({ [2] = {"
            , "  repairJob = { instanceId = 900, recipeId = 'known_repair',"
            , "                defName = 'wood', consumable = 'stone' },"
            , "} })"
            , "assert(repairPresent.ok,"
            , "  'a repairJob whose recipe/items all still exist must not be rejected: '"
            , "  .. table.concat(repairPresent.errors or {}, '; '))"
            ]

        it "rejects a craftJob missing its REQUIRED billId/bid, and a \
           \repairJob missing its REQUIRED instanceId (round-6 review, \
           \issue #764) -- craftJob.billId/bid and repairJob.instanceId \
           \are unconditionally set the instant their job is created \
           \(unit_ai_craft.lua/unit_ai_repair.lua), so a v2/v3 payload \
           \whose job table is present but missing one is structurally \
           \malformed, not a legitimate earlier job phase -- unlike a \
           \dangling id (a real id whose TARGET later vanished), which \
           \stays a tolerated, non-blocking diagnostic elsewhere" $
            runsOk $ lns
            [ "unit = { exists = function(_uid) return true end }"
            , "craft = { get = function(_id) return { id = _id } end }"
            , "repair = { get = function(_id) return { id = _id } end }"
            , "item = { listDefs = function() return { { name = 'wood' } } end }"
            , "local unitAiSave = require('scripts.unit_ai_save')"
            , "local fakeAiState = {}"
            , "unitAiSave.register(fakeAiState)"
            , "local saveModules = require('scripts.lib.save_modules')"
            , "local codec = require('scripts.lib.data_codec')"
            , "local function prepareWith(state)"
            , "  return saveModules.prepareLoad({"
            , "    { id = 'unit_ai', version = 2, payload = codec.encode(state) },"
            , "  })"
            , "end"
            , "local noBillId = prepareWith({ [1] = { craftJob = {"
            , "  bid = { __ref = 'building', id = 9 }, recipeId = 'x' } } })"
            , "assert(not noBillId.ok,"
            , "  'a craftJob with no billId at all must reject the load')"
            , "local noBid = prepareWith({ [1] = { craftJob = {"
            , "  billId = { __ref = 'craft_bill', id = 5 }, recipeId = 'x' } } })"
            , "assert(not noBid.ok,"
            , "  'a craftJob with no bid (station) at all must reject the load')"
            , "local noInstanceId = prepareWith({ [1] = { repairJob = {"
            , "  recipeId = 'x', defName = 'wood' } } })"
            , "assert(not noInstanceId.ok,"
            , "  'a repairJob with no instanceId at all must reject the load')"
            , "-- repairJob.bid is deliberately OPTIONAL. unit_ai_repair.lua"
            , "-- DOES set it -- but only once the job reaches its walking"
            , "-- phase and building.findStation resolves a station -- so a"
            , "-- job saved in an earlier phase legitimately carries none,"
            , "-- and requiring it would reject a real repair job."
            , "local repairNoBid = prepareWith({ [2] = { repairJob = {"
            , "  instanceId = { __ref = 'item_instance', id = 900 },"
            , "  recipeId = 'x', defName = 'wood' } } })"
            , "assert(repairNoBid.ok,"
            , "  'repairJob.bid must stay optional (it is only set once the '"
            , "  .. 'job reaches the walking phase in unit_ai_repair.lua): '"
            , "  .. table.concat(repairNoBid.errors or {}, '; '))"
            ]

        it "extends the same missing-content-reference rejection to \
           \constructJob/deliveryClaim/deliveryPendingTarget/plantJob \
           \(issue #761 round-5 review), and strips constructJob's live \
           \structure-pack build table from the snapshot without \
           \mutating the live job" $ runsOk $ lns
            [ "unit = { exists = function(_uid) return true end }"
            , "item = { listDefs = function()"
            , "  return { { name = 'wood' }, { name = 'stone' } } end }"
            , "building = { listDefs = function()"
            , "  return { { name = 'workbench' } } end }"
            , "flora = { exists = function(name) return name == 'wheat' end }"
            , "engine.loadYaml = function(path)"
            , "  if path == 'data/structure_packs/known_pack.yaml' then"
            , "    return { build = { wall = { materials = { wood = 2 },"
            , "                                build_work = 3 } } }"
            , "  end"
            , "  return nil"
            , "end"
            , "local unitAiSave = require('scripts.unit_ai_save')"
            , "local saveModules = require('scripts.lib.save_modules')"
            , "local codec = require('scripts.lib.data_codec')"
            , "-- Register BEFORE any prepareLoad call -- prepareLoad only"
            , "-- validates components already present in the registry, so"
            , "-- registering after would leave every prepareWith() call"
            , "-- below validating against an empty registry and passing"
            , "-- vacuously."
            , "local liveBuild = { materials = { wood = 2 }, build_work = 3 }"
            , "local fakeAiState = { [1] = { constructJob = {"
            , "  category = 'structure', pack = 'known_pack', kind = 'wall',"
            , "  build = liveBuild, need = { wood = 2 } } } }"
            , "unitAiSave.register(fakeAiState)"
            , "local function prepareWith(state)"
            , "  return saveModules.prepareLoad({"
            , "    { id = 'unit_ai', version = 1, payload = codec.encode(state) },"
            , "  })"
            , "end"
            -- #1844 requirement 20: a structurally valid structure job
            -- whose pack or kind no longer resolves must REACH load
            -- staging, where the engine self-clears the matching
            -- designation and refunds its persisted receipt exactly
            -- once. Rejecting here would abort a whole load for a
            -- situation that is now resolved losslessly, which is why
            -- the two assertions below are the reverse of what they
            -- were. The narrow rejections either side of them are
            -- deliberately unchanged.
            , "local badPack = prepareWith({ [1] = { constructJob = {"
            , "  category = 'structure', pack = 'ghost_pack', kind = 'wall',"
            , "  need = {} } } })"
            , "assert(badPack.ok,"
            , "  'a removed structure pack must reach load reconciliation: '"
            , "  .. table.concat(badPack.errors or {}, '; '))"
            , "local badKind = prepareWith({ [1] = { constructJob = {"
            , "  category = 'structure', pack = 'known_pack', kind = 'ghost_kind',"
            , "  need = {} } } })"
            , "assert(badKind.ok,"
            , "  'a removed pack kind must reach load reconciliation: '"
            , "  .. table.concat(badKind.errors or {}, '; '))"
            , "local badConstructItem = prepareWith({ [1] = { constructJob = {"
            , "  category = 'structure', pack = 'known_pack', kind = 'wall',"
            , "  need = {}, fromGround = { unobtainium = 1 } } } })"
            , "assert(not badConstructItem.ok,"
            , "  'a constructJob fetch map referencing a removed item must reject the load')"
            , "local goodConstruct = prepareWith({ [1] = { constructJob = {"
            , "  category = 'structure', pack = 'known_pack', kind = 'wall',"
            , "  need = { wood = 2 }, fromGround = { stone = 1 } } } })"
            , "assert(goodConstruct.ok,"
            , "  'a constructJob whose pack/kind/items all still exist must not be rejected: '"
            , "  .. table.concat(goodConstruct.errors or {}, '; '))"
            , "local buildingConstruct = prepareWith({ [1] = { constructJob = {"
            , "  category = 'building', building = 'workbench', x = 1, y = 1 } } })"
            , "assert(buildingConstruct.ok,"
            , "  'a known building-category constructJob must not be rejected: '"
            , "  .. table.concat(buildingConstruct.errors or {}, '; '))"
            , "local badBuildingConstruct = prepareWith({ [1] = { constructJob = {"
            , "  category = 'building', building = 'ghost_building', x = 1, y = 1 } } })"
            , "assert(not badBuildingConstruct.ok,"
            , "  'a constructJob referencing a removed building def must reject the load')"
            , "local badDeliveryClaim = prepareWith({ [1] = { deliveryClaim = {"
            , "  bid = 1, materials = { unobtainium = 1 } } } })"
            , "assert(not badDeliveryClaim.ok,"
            , "  'a deliveryClaim referencing a removed material must reject the load')"
            , "local badDeliveryTarget = prepareWith({ [1] = { deliveryPendingTarget = {"
            , "  bid = 1, claim = { unobtainium = 1 } } } })"
            , "assert(not badDeliveryTarget.ok,"
            , "  'a deliveryPendingTarget referencing a removed material must reject the load')"
            , "local goodDelivery = prepareWith({ [1] = { deliveryClaim = {"
            , "  bid = 1, materials = { wood = 1 }, fromGround = { stone = 1 } } } })"
            , "assert(goodDelivery.ok,"
            , "  'a deliveryClaim whose materials all still exist must not be rejected: '"
            , "  .. table.concat(goodDelivery.errors or {}, '; '))"
            , "local badPlant = prepareWith({ [1] = { plantJob = {"
            , "  x = 1, y = 1, crop = 'ghost_crop' } } })"
            , "assert(not badPlant.ok,"
            , "  'a plantJob referencing a removed crop species must reject the load')"
            , "local goodPlant = prepareWith({ [1] = { plantJob = {"
            , "  x = 1, y = 1, crop = 'wheat' } } })"
            , "assert(goodPlant.ok,"
            , "  'a plantJob whose crop still exists must not be rejected: '"
            , "  .. table.concat(goodPlant.errors or {}, '; '))"
            , "-- The .build sub-field itself must never reach the encoded"
            , "-- payload (requirement 14), and stripping it must not mutate"
            , "-- the live aiState singleton's own job table."
            , "local snap = saveModules.registry.unit_ai.snapshot()"
            , "assert(snap[1].constructJob.build == nil,"
            , "  'constructJob.build must be stripped from the snapshot')"
            , "assert(snap[1].constructJob.pack == 'known_pack',"
            , "  'sibling constructJob fields must survive the strip')"
            , "assert(fakeAiState[1].constructJob.build == liveBuild,"
            , "  'stripping must not mutate the live constructJob table')"
            , "local payload = codec.encode(snap)"
            , "assert(payload:find('build_work') == nil,"
            , "  'no trace of the live build-cost content may reach the encoded payload')"
            ]

        it "includes the OUTER per-unit key itself as a unit reference \
           \(issue #761 round-6 review), mirroring building_spawn.lua's \
           \own references() including its per-building key -- not just \
           \the ids nested inside claim/job fields" $ runsOk $ lns
            [ "unit = { exists = function(_uid) return true end }"
            , "local unitAiSave = require('scripts.unit_ai_save')"
            , "local saveModules = require('scripts.lib.save_modules')"
            , "unitAiSave.register({})"
            , "local refs = saveModules.registry.unit_ai.references("
            , "  { [42] = { currentAction = 'idle' } })"
            , "local found = false"
            , "for _, r in ipairs(refs) do"
            , "  if r.kind == 'unit' and r.id == 42 then found = true end"
            , "end"
            , "assert(found, 'the outer unit id itself must be a declared reference')"
            ]

        it "types every persisted reference field on the wire (issue #764, \
           \save-overhaul C3 requirement 13): a v1 payload with BARE-NUMBER \
           \reference fields migrates to the typed {__ref=,id=} shape, \
           \references() reads it correctly, and apply() unwraps it back \
           \to a bare number in the LIVE aiState (every other module \
           \still sees plain numbers)" $ runsOk $ lns
            [ "unit = { exists = function(_uid) return true end }"
            , "craft = { get = function(id)"
            , "  if id == 'x' then return { id = 'x' } end return nil end }"
            , "item = { listDefs = function() return {} end }"
            , "local unitAiSave = require('scripts.unit_ai_save')"
            , "local fakeAiState = {}"
            , "unitAiSave.register(fakeAiState)"
            , "local saveModules = require('scripts.lib.save_modules')"
            , "local codec = require('scripts.lib.data_codec')"
            , "-- A v1 payload: every reference field is a BARE NUMBER,"
            , "-- exactly as #761 originally shipped it."
            , "local v1 = { [7] = {"
            , "  attackTargetUid = 8, buildTarget = 20,"
            , "  craftJob = { billId = 3, bid = 21, recipeId = 'x' },"
            , "} }"
            , "local prep = saveModules.prepareLoad({"
            , "  { id = 'unit_ai', version = 1, payload = codec.encode(v1) },"
            , "})"
            , "assert(prep.ok, 'v1 payload must migrate cleanly: '"
            , "  .. table.concat(prep.errors or {}, '; '))"
            , "local found = {}"
            , "for _, r in ipairs(prep.references) do"
            , "  found[r.kind .. ':' .. tostring(r.id)] = r.owner"
            , "end"
            , "assert(found['unit:8'] == 7,"
            , "  'attackTargetUid must resolve through the wrapped v1->v2 shape')"
            , "assert(found['building:20'] == 7,"
            , "  'buildTarget must resolve through the wrapped v1->v2 shape')"
            , "assert(found['craft_bill:3'] == 7,"
            , "  'craftJob.billId must resolve through the wrapped v1->v2 shape')"
            , "assert(found['building:21'] == 7,"
            , "  'craftJob.bid must resolve through the wrapped v1->v2 shape')"
            , "saveModules.applyAll()"
            , "assert(fakeAiState[7].attackTargetUid == 8,"
            , "  'apply() must unwrap attackTargetUid back to a bare number in LIVE aiState')"
            , "assert(type(fakeAiState[7].attackTargetUid) == 'number',"
            , "  'LIVE aiState must never hold a wrapped table -- every OTHER '"
            , "  .. 'module (unit_ai_combat.lua etc.) reads a bare number')"
            , "assert(fakeAiState[7].craftJob.billId == 3,"
            , "  'apply() must unwrap nested craftJob.billId too')"
            , "-- Round-trip through the engine's OWN encoder: snapshot() on"
            , "-- this now-live (unwrapped) state must re-wrap it as v2 --"
            , "-- the wire format is typed even for freshly-written saves,"
            , "-- not merely a migration-only artifact."
            , "local snap = saveModules.registry.unit_ai.snapshot()"
            , "assert(type(snap[7].attackTargetUid) == 'table'"
            , "  and snap[7].attackTargetUid.__ref == 'unit'"
            , "  and snap[7].attackTargetUid.id == 8,"
            , "  'snapshot() must write the TYPED structured-reference shape, '"
            , "  .. 'not a bare number, for a fresh v2 save')"
            , "-- Round-6 review: the OUTER per-unit key (7) is ALSO typed,"
            , "-- via a self-describing __owner field on the row."
            , "assert(type(snap[7].__owner) == 'table'"
            , "  and snap[7].__owner.__ref == 'unit' and snap[7].__owner.id == 7,"
            , "  'snapshot() must write a __owner field typing the outer '"
            , "  .. 'per-unit key too')"
            , "assert(fakeAiState[7].__owner == nil,"
            , "  '__owner must never leak into the LIVE aiState apply() writes back')"
            ]

        it "migrates a v2 unit_ai payload (every reference field wrapped, \
           \but no __owner yet) to v3 by adding ONLY __owner, without \
           \re-wrapping fields that are already wrapped (round-6 review, \
           \issue #764)" $
            runsOk $ lns
            [ "unit = { exists = function(_uid) return true end }"
            , "craft = { get = function(_id) return nil end }"
            , "item = { listDefs = function() return {} end }"
            , "local unitAiSave = require('scripts.unit_ai_save')"
            , "local fakeAiState = {}"
            , "unitAiSave.register(fakeAiState)"
            , "local saveModules = require('scripts.lib.save_modules')"
            , "local codec = require('scripts.lib.data_codec')"
            , "local v2 = { [7] = {"
            , "  attackTargetUid = { __ref = 'unit', id = 8 },"
            , "} }"
            , "local prep = saveModules.prepareLoad({"
            , "  { id = 'unit_ai', version = 2, payload = codec.encode(v2) },"
            , "})"
            , "assert(prep.ok, 'a v2 payload must migrate to v3 cleanly: '"
            , "  .. table.concat(prep.errors or {}, '; '))"
            , "saveModules.applyAll()"
            , "assert(fakeAiState[7].attackTargetUid == 8,"
            , "  'a v2-shaped attackTargetUid must still unwrap correctly after '"
            , "  .. 'the v2->v3 __owner-only migration')"
            ]

        it "rejects a v3 unit_ai payload with NO __owner at all, and one \
           \whose __owner id does not match its own outer key (round-6 \
           \review, issue #764) -- __owner is REQUIRED on every entry, \
           \unlike lastUid/attackTargetUid/etc., which are legitimately \
           \absent" $
            runsOk $ lns
            [ "unit = { exists = function(_uid) return true end }"
            , "craft = { get = function(_id) return nil end }"
            , "item = { listDefs = function() return {} end }"
            , "local unitAiSave = require('scripts.unit_ai_save')"
            , "local fakeAiState = {}"
            , "unitAiSave.register(fakeAiState)"
            , "local saveModules = require('scripts.lib.save_modules')"
            , "local codec = require('scripts.lib.data_codec')"
            , "local noOwner = { [7] = {} }"
            , "local prep = saveModules.prepareLoad({"
            , "  { id = 'unit_ai', version = 3, payload = codec.encode(noOwner) },"
            , "})"
            , "assert(not prep.ok, 'a v3 entry missing __owner entirely must reject the load')"
            , "local mismatched = { [7] = { __owner = { __ref = 'unit', id = 8 } } }"
            , "local prep2 = saveModules.prepareLoad({"
            , "  { id = 'unit_ai', version = 3, payload = codec.encode(mismatched) },"
            , "})"
            , "assert(not prep2.ok,"
            , "  \"a __owner id that doesn't match its own outer key must reject the load\")"
            , "local matched = { [7] = { __owner = { __ref = 'unit', id = 7 } } }"
            , "local prep3 = saveModules.prepareLoad({"
            , "  { id = 'unit_ai', version = 3, payload = codec.encode(matched) },"
            , "})"
            , "assert(prep3.ok, 'a correctly-matched __owner must load cleanly: '"
            , "  .. table.concat(prep3.errors or {}, '; '))"
            ]

        it "types building_spawn's OUTER per-building key via __owner too \
           \(round-6 review, issue #764) -- migrates a v1 payload to v3 \
           \(synthesizing __owner even though NO lastUid was ever set), \
           \migrates a v2 payload by adding only __owner, and rejects a \
           \v3 payload with a missing or mismatched __owner" $
            runsOk $ lns
            [ "building = { getInfo = function(_bid) return { id = _bid } end }"
            , "local buildingSpawn = require('scripts.building_spawn')"
            , "buildingSpawn.init('test')"
            , "local saveModules = require('scripts.lib.save_modules')"
            , "local codec = require('scripts.lib.data_codec')"
            , "-- v1: no lastUid at all (a building that hasn't spawned yet)."
            , "local v1 = { [12] = { lastSpawnedAt = 1.0 } }"
            , "local prep = saveModules.prepareLoad({"
            , "  { id = 'building_spawn', version = 1, payload = codec.encode(v1) },"
            , "})"
            , "assert(prep.ok, 'a v1 payload with no lastUid must still migrate '"
            , "  .. 'cleanly and gain __owner: ' .. table.concat(prep.errors or {}, '; '))"
            , "saveModules.applyAll()"
            , "local snap = saveModules.registry.building_spawn.snapshot()"
            , "assert(type(snap[12].__owner) == 'table'"
            , "  and snap[12].__owner.__ref == 'building' and snap[12].__owner.id == 12,"
            , "  'a fresh snapshot() must carry __owner even for a building with no lastUid')"
            , "-- v2: lastUid already wrapped, no __owner yet."
            , "local v2 = { [12] = { lastUid = { __ref = 'unit', id = 4 } } }"
            , "local prep2 = saveModules.prepareLoad({"
            , "  { id = 'building_spawn', version = 2, payload = codec.encode(v2) },"
            , "})"
            , "assert(prep2.ok, 'a v2 payload must migrate to v3 cleanly: '"
            , "  .. table.concat(prep2.errors or {}, '; '))"
            , "-- v3: missing __owner entirely must reject."
            , "local noOwner = { [12] = {} }"
            , "local prep3 = saveModules.prepareLoad({"
            , "  { id = 'building_spawn', version = 3, payload = codec.encode(noOwner) },"
            , "})"
            , "assert(not prep3.ok, 'a v3 entry missing __owner entirely must reject the load')"
            , "-- v3: mismatched __owner id must reject."
            , "local mismatched = { [12] = { __owner = { __ref = 'building', id = 13 } } }"
            , "local prep4 = saveModules.prepareLoad({"
            , "  { id = 'building_spawn', version = 3, payload = codec.encode(mismatched) },"
            , "})"
            , "assert(not prep4.ok,"
            , "  \"a __owner id that doesn't match its own outer key must reject the load\")"
            ]

        it "rejects a v2 payload whose wrapped reference carries the WRONG \
           \__ref kind for its field (round-2 review, issue #764) -- \
           \unwrapUnitState used to trust field position alone and would \
           \have silently applied a building id as if it were a unit id" $
            runsOk $ lns
            [ "unit = { exists = function(_uid) return true end }"
            , "craft = { get = function(_id) return nil end }"
            , "item = { listDefs = function() return {} end }"
            , "local unitAiSave = require('scripts.unit_ai_save')"
            , "local fakeAiState = {}"
            , "unitAiSave.register(fakeAiState)"
            , "local saveModules = require('scripts.lib.save_modules')"
            , "local codec = require('scripts.lib.data_codec')"
            , "-- attackTargetUid must be __ref='unit' -- this payload"
            , "-- tags it 'building' instead, same numeric id."
            , "local badKind = { [7] = {"
            , "  attackTargetUid = { __ref = 'building', id = 8 },"
            , "} }"
            , "local prep = saveModules.prepareLoad({"
            , "  { id = 'unit_ai', version = 2, payload = codec.encode(badKind) },"
            , "})"
            , "assert(not prep.ok,"
            , "  'a wrong-kind wrapper on attackTargetUid must reject the load')"
            , "-- Untagged (no __ref at all) must also be rejected -- not"
            , "-- silently treated as a bare-number v1-shaped field, since"
            , "-- this component's declared version is 2."
            , "local untagged = { [7] = { attackTargetUid = { id = 8 } } }"
            , "local prep2 = saveModules.prepareLoad({"
            , "  { id = 'unit_ai', version = 2, payload = codec.encode(untagged) },"
            , "})"
            , "assert(not prep2.ok,"
            , "  'an untagged wrapper on attackTargetUid must reject the load')"
            , "-- A correctly-tagged payload must still succeed -- this is a"
            , "-- kind check, not a blanket rejection of every wrapped value."
            , "local goodKind = { [7] = {"
            , "  attackTargetUid = { __ref = 'unit', id = 8 },"
            , "} }"
            , "local prep3 = saveModules.prepareLoad({"
            , "  { id = 'unit_ai', version = 2, payload = codec.encode(goodKind) },"
            , "})"
            , "assert(prep3.ok, 'a correctly-tagged wrapper must still load: '"
            , "  .. table.concat(prep3.errors or {}, '; '))"
            ]

        it "rejects a v2 payload whose wrapped reference has the RIGHT \
           \__ref kind but a non-numeric or invalid id (round-3 review, \
           \issue #764) -- a tag-only check would still accept \
           \{__ref='unit', id='bad'}, which would unwrap into live \
           \aiState and be silently dropped by every diagnostic that \
           \Lua.tointeger()s the id instead of being reported" $
            runsOk $ lns
            [ "unit = { exists = function(_uid) return true end }"
            , "craft = { get = function(_id) return nil end }"
            , "item = { listDefs = function() return {} end }"
            , "local unitAiSave = require('scripts.unit_ai_save')"
            , "local fakeAiState = {}"
            , "unitAiSave.register(fakeAiState)"
            , "local saveModules = require('scripts.lib.save_modules')"
            , "local codec = require('scripts.lib.data_codec')"
            , "local badId = { [7] = {"
            , "  attackTargetUid = { __ref = 'unit', id = 'bad' },"
            , "} }"
            , "local prep = saveModules.prepareLoad({"
            , "  { id = 'unit_ai', version = 2, payload = codec.encode(badId) },"
            , "})"
            , "assert(not prep.ok,"
            , "  'a non-numeric id on a correctly-tagged wrapper must reject the load')"
            , "-- Zero / negative / fractional ids are equally invalid --"
            , "-- the same positive-integer contract every other id in"
            , "-- this codebase enforces."
            , "local zeroId = { [7] = {"
            , "  attackTargetUid = { __ref = 'unit', id = 0 },"
            , "} }"
            , "local prep2 = saveModules.prepareLoad({"
            , "  { id = 'unit_ai', version = 2, payload = codec.encode(zeroId) },"
            , "})"
            , "assert(not prep2.ok, 'a zero id must reject the load')"
            , "local fracId = { [7] = {"
            , "  attackTargetUid = { __ref = 'unit', id = 8.5 },"
            , "} }"
            , "local prep3 = saveModules.prepareLoad({"
            , "  { id = 'unit_ai', version = 2, payload = codec.encode(fracId) },"
            , "})"
            , "assert(not prep3.ok, 'a fractional id must reject the load')"
            ]

        it "accepts a ground_item reference id of 0 (round-4 review, issue \
           \#764) -- Item.Ground's ground-item allocator is ZERO-based \
           \(emptyGroundItems starts gisNextId at 0), unlike unit/building/ \
           \craft_bill/item_instance's allocators, which all start at 1; a \
           \blanket 'id >= 1' minimum incorrectly rejected the very first \
           \ground item a save could ever legitimately reference" $
            runsOk $ lns
            [ "unit = { exists = function(_uid) return true end }"
            , "craft = { get = function(_id) return nil end }"
            , "item = { listDefs = function() return {} end }"
            , "local unitAiSave = require('scripts.unit_ai_save')"
            , "local fakeAiState = {}"
            , "unitAiSave.register(fakeAiState)"
            , "local saveModules = require('scripts.lib.save_modules')"
            , "local codec = require('scripts.lib.data_codec')"
            , "local zeroGid = { [7] = {"
            , "  pickupOrder = { gid = { __ref = 'ground_item', id = 0 } },"
            , "} }"
            , "local prep = saveModules.prepareLoad({"
            , "  { id = 'unit_ai', version = 2, payload = codec.encode(zeroGid) },"
            , "})"
            , "assert(prep.ok, 'a ground_item id of 0 must be accepted: '"
            , "  .. table.concat(prep.errors or {}, '; '))"
            , "-- A negative ground_item id is still invalid -- the fix"
            , "-- widens the floor to 0, it doesn't remove it."
            , "local negGid = { [7] = {"
            , "  pickupOrder = { gid = { __ref = 'ground_item', id = -1 } },"
            , "} }"
            , "local prep2 = saveModules.prepareLoad({"
            , "  { id = 'unit_ai', version = 2, payload = codec.encode(negGid) },"
            , "})"
            , "assert(not prep2.ok, 'a negative ground_item id must still reject the load')"
            ]

        it "rejects a v2 building_spawn payload whose lastUid has the \
           \RIGHT __ref kind but a non-numeric id (round-3 review, \
           \issue #764) -- mirrors the unit_ai id-type check" $
            runsOk $ lns
            [ "building = { getInfo = function(_bid) return { id = _bid } end }"
            , "local buildingSpawn = require('scripts.building_spawn')"
            , "buildingSpawn.init('test')"
            , "local saveModules = require('scripts.lib.save_modules')"
            , "local codec = require('scripts.lib.data_codec')"
            , "local badId = { [12] = { lastUid = { __ref = 'unit', id = 'bad' } } }"
            , "local prep = saveModules.prepareLoad({"
            , "  { id = 'building_spawn', version = 2, payload = codec.encode(badId) },"
            , "})"
            , "assert(not prep.ok,"
            , "  'a non-numeric id on lastUid must reject the load')"
            ]

        it "rejects a v2 building_spawn payload whose lastUid carries the \
           \WRONG __ref kind (round-2 review, issue #764) -- mirrors the \
           \unit_ai wrapper-tag check for building_spawn's own sole \
           \reference field" $
            runsOk $ lns
            [ "building = { getInfo = function(_bid) return { id = _bid } end }"
            , "local buildingSpawn = require('scripts.building_spawn')"
            , "buildingSpawn.init('test')"
            , "local saveModules = require('scripts.lib.save_modules')"
            , "local codec = require('scripts.lib.data_codec')"
            , "local badKind = { [12] = { lastUid = { __ref = 'building', id = 8 } } }"
            , "local prep = saveModules.prepareLoad({"
            , "  { id = 'building_spawn', version = 2, payload = codec.encode(badKind) },"
            , "})"
            , "assert(not prep.ok,"
            , "  'a wrong-kind wrapper on lastUid must reject the load')"
            , "local goodKind = { [12] = { lastUid = { __ref = 'unit', id = 8 } } }"
            , "local prep2 = saveModules.prepareLoad({"
            , "  { id = 'building_spawn', version = 2, payload = codec.encode(goodKind) },"
            , "})"
            , "assert(prep2.ok, 'a correctly-tagged lastUid must still load: '"
            , "  .. table.concat(prep2.errors or {}, '; '))"
            ]

        it "declares real Haskell-owned dependencies on the ACTUAL \
           \unit_ai and building_spawn registrations (issue #761 \
           \round-8 review) -- not just a synthetic component in the \
           \registry-mechanism tests above, since a mechanism nobody's \
           \real registration exercises doesn't satisfy requirement 2" $
            runsOk $ lns
            [ "unit = { exists = function(_uid) return true end }"
            , "local unitAiSave = require('scripts.unit_ai_save')"
            , "local buildingSpawn = require('scripts.building_spawn')"
            , "local saveModules = require('scripts.lib.save_modules')"
            , "unitAiSave.register({})"
            , "buildingSpawn.init('test')"
            , "local function hasDep(regId, dep)"
            , "  for _, d in ipairs(saveModules.registry[regId].deps) do"
            , "    if d == dep then return true end"
            , "  end"
            , "  return false"
            , "end"
            , "assert(hasDep('unit_ai', 'units'),"
            , "  'unit_ai must declare a real dependency on units')"
            , "assert(hasDep('unit_ai', 'buildings'),"
            , "  'unit_ai must declare a real dependency on buildings')"
            , "assert(hasDep('building_spawn', 'buildings'),"
            , "  'building_spawn must declare a real dependency on buildings')"
            , "assert(hasDep('building_spawn', 'units'),"
            , "  'building_spawn must declare a real dependency on units')"
            ]

        it "types building_spawn's lastUid reference field on the wire too \
           \(issue #764, save-overhaul C3 requirement 13): a v1 payload \
           \with a BARE-NUMBER lastUid migrates to the typed shape, \
           \references() reads it, apply() unwraps it back to a bare \
           \number, and a fresh snapshot() re-wraps it as v2" $ runsOk $ lns
            [ "building = { getInfo = function(_bid) return { id = _bid } end }"
            , "local buildingSpawn = require('scripts.building_spawn')"
            , "buildingSpawn.init('test')"
            , "local saveModules = require('scripts.lib.save_modules')"
            , "local codec = require('scripts.lib.data_codec')"
            , "local v1 = { [9] = { lastUid = 4, lastSpawnedAt = 1.0 } }"
            , "local prep = saveModules.prepareLoad({"
            , "  { id = 'building_spawn', version = 1, payload = codec.encode(v1) },"
            , "})"
            , "assert(prep.ok, 'v1 payload must migrate cleanly: '"
            , "  .. table.concat(prep.errors or {}, '; '))"
            , "local found = false"
            , "for _, r in ipairs(prep.references) do"
            , "  if r.kind == 'unit' and r.id == 4 then found = true end"
            , "end"
            , "assert(found, 'lastUid must resolve through the wrapped v1->v2 shape')"
            , "saveModules.applyAll()"
            , "local snap = saveModules.registry.building_spawn.snapshot()"
            , "assert(type(snap[9].lastUid) == 'table'"
            , "  and snap[9].lastUid.__ref == 'unit' and snap[9].lastUid.id == 4,"
            , "  'a fresh snapshot() must write the TYPED structured-reference '"
            , "  .. 'shape -- if apply() had left lastUid wrapped in LIVE state '"
            , "  .. 'this would double-wrap or crash instead')"
            , "local errs = saveModules.registryStaticErrors()"
            , "assert(#errs == 0, 'the real registrations must resolve their "
              <> "own deps cleanly: ' .. table.concat(errs, '; '))"
            ]


    -- Issue #2055. A row restored from an accepted schema version need
    -- not carry the transient runtime fields the thought tick reads
    -- before it has decided anything: this component's validator
    -- accepts a free-form state row on purpose, and applyEntityRows
    -- installs each decoded row verbatim. Such a row survived decode,
    -- canonical comparison, resave, restart and reload and then errored
    -- on its first live tick.
    --
    -- The fill happens at the POST-PUBLISH reconcile, and these cases
    -- pin all three reasons it has to be there rather than at decode()
    -- or apply(): the restored clock is live by then, a rolled-back
    -- load never reaches it, and nothing has ticked yet. It is also one
    -- stage rather than a back-fill per migration branch, which is what
    -- lets the version matrix below be a loop.
    describe "unit_ai transient runtime defaults (issue #2055)" $ do
        it "supplies every declared runtime default a restored row \
           \omits, for EVERY accepted inputVersion -- one stage that \
           \every version's decode branch has already converged on by \
           \reconcile time, so a payload from any accepted version \
           \comes out tickable" $ runsOk $ lns $ unitAiDefaultsPrelude ⧺
            [ "local refs = require('scripts.unit_ai_save_refs')"
            , "local defaults = require('scripts.unit_ai_defaults')"
            , "-- The accepted set comes off the registration itself, so a"
            , "-- version added there is covered by this loop the moment it"
            , "-- exists. The exact-set assertion below is the deliberate"
            , "-- tripwire beside that: a new version also needs its WIRE"
            , "-- shape taught to payloadFor, which no derived loop can"
            , "-- infer, so adding one must be a conscious act here."
            , "local accepted = saveModules.registry.unit_ai.inputVersions"
            , "assert(table.concat(accepted, ',') == '1,2,3,4,5,6,7,8',"
            , "  'expected inputVersions {1..8} (1-7 legacy, 8 current), got {'"
            , "  .. table.concat(accepted, ',') .. '}')"
            , "-- The tracked b3-lua-versioned-session-v1 fixture's own v1"
            , "-- row, verbatim: sparse, one reference field, none of the"
            , "-- runtime fields."
            , "local function sparseRow() return { buildTarget = 1 } end"
            , "-- Each version's WIRE shape, built with the component's own"
            , "-- helpers rather than hand-rolled: v1 is bare, v2 is wrapped"
            , "-- without __owner, v3+ carries __owner too. #1844's v8 is a"
            , "-- SEMANTIC bump on v7's layout (a constructJob gained the"
            , "-- attempt it claimed), and a sparse row carries no"
            , "-- constructJob, so the two share one wire shape here."
            , "local function payloadFor(version)"
            , "  local rows = { [1] = sparseRow() }"
            , "  if version == 1 then return codec.encode(rows) end"
            , "  local wrapped = refs.wrapAiState(rows)"
            , "  if version == 2 then wrapped[1].__owner = nil end"
            , "  return codec.encode(wrapped)"
            , "end"
            , "for _, version in ipairs(accepted) do"
            , "  for k in pairs(aiState) do aiState[k] = nil end"
            , "  local prep = saveModules.prepareLoad({"
            , "    { id = 'unit_ai', version = version,"
            , "      payload = payloadFor(version) },"
            , "  }, 1, false, { unit = { [1] = true }, building = {} })"
            , "  assert(prep.ok, 'a sparse v' .. version .. ' payload must "
              <> "still be accepted: ' .. table.concat(prep.errors or {}, '; '))"
            , "  saveModules.applyAll()"
            , "  assert(aiState[1] ~= nil, 'v' .. version .. ': the row must apply')"
            , "  assert(aiState[1].buildTarget == 1, 'v' .. version .. ': the "
              <> "row\\'s own field must survive unwrapped')"
            , "  -- STAGING must not have filled anything: gameTimeRef is"
            , "  -- still the outgoing session's until publish."
            , "  for _, f in ipairs(defaults.FIELDS) do"
            , "    assert(aiState[1][f.name] == nil, 'v' .. version .. ': "
              <> "decode/apply run before publish and must fill nothing')"
            , "  end"
            , "  -- Building 1 survives, so the row\'s own buildTarget is"
            , "  -- not a dangling reference the scrub would clear -- this"
            , "  -- case is about the runtime defaults, not the scrub."
            , "  reconcile.reconcile(aiState, { 1 }, { 1 }, CTX)"
            , "  for _, f in ipairs(defaults.FIELDS) do"
            , "    assert(aiState[1][f.name] ~= nil, 'v' .. version .. ': the "
              <> "reconciled row must carry a ' .. f.name .. ' default')"
            , "  end"
            , "  assert(aiState[1].currentAction == 'idle', 'v' .. version"
            , "    .. ': the fresh-row currentAction')"
            , "  assert(aiState[1].nextActionAt == 0, 'v' .. version"
            , "    .. ': 0 means decide on first sight, not wait out an "
              <> "interval nobody scheduled')"
            , "  assert(aiState[1].actionStartedAt == NOW, 'v' .. version"
            , "    .. ': actionStartedAt is the RESTORED clock')"
            , "  assert(aiState[1].commandedTask == nil, 'v' .. version"
            , "    .. ': nil IS commandedTask\\'s value -- defaulting it "
              <> "would invent an order nobody issued')"
            , "end"
            ]

        it "stamps actionStartedAt from the RESTORED session's clock, \
           \not the outgoing one: decode and apply run during staging, \
           \before World.Load.Publish swaps gameTimeRef, so a partially \
           \sparse wander row filled there would have wanderUtility \
           \subtract a foreign timestamp and abandon a wander on time it \
           \never spent" $ runsOk $ lns $ unitAiDefaultsPrelude ⧺
            [ "local refs = require('scripts.unit_ai_save_refs')"
            , "-- The OUTGOING session's clock, live for the whole of"
            , "-- staging. In a fresh process this is 0; here it is a"
            , "-- deliberately WRONG-and-obvious value instead, so a stamp"
            , "-- taken from it is unmistakable."
            , "NOW = 5000.0"
            , "-- Only actionStartedAt is missing. currentAction says the"
            , "-- unit was wandering, which is exactly the row"
            , "-- unit_ai_needs.lua's wanderUtility does arithmetic for:"
            , "--   timeInSession = engine.gameTime() - s.actionStartedAt"
            , "local prep = saveModules.prepareLoad({"
            , "  { id = 'unit_ai', version = 7,"
            , "    payload = codec.encode(refs.wrapAiState("
            , "      { [1] = { currentAction = 'wander', nextActionAt = 0 } })) },"
            , "}, 1, false, { unit = { [1] = true }, building = {} })"
            , "assert(prep.ok, table.concat(prep.errors or {}, '; '))"
            , "saveModules.applyAll()"
            , "assert(aiState[1].actionStartedAt == nil,"
            , "  'staging must not stamp a clock the restored session does "
              <> "not own yet')"
            , "-- Publish swaps gameTimeRef to the save's own game time."
            , "NOW = 42.0"
            , "reconcile.reconcile(aiState, { 1 }, {}, CTX)"
            , "assert(aiState[1].actionStartedAt == 42.0,"
            , "  'the stamp must be the RESTORED clock, got '"
            , "  .. tostring(aiState[1].actionStartedAt))"
            , "-- The consequence, stated as wanderUtility computes it: a"
            , "-- row with no recorded start has spent NO time in this"
            , "-- session's wander, which is the same answer a freshly"
            , "-- seen unit gets. A staging-time stamp would have made"
            , "-- this 42 - 5000 = -4958."
            , "local timeInSession = engine.gameTime() - aiState[1].actionStartedAt"
            , "assert(timeInSession == 0,"
            , "  'a restored wander must start from zero elapsed, got '"
            , "  .. tostring(timeInSession))"
            , "-- And the fields the row DID carry are still its own."
            , "assert(aiState[1].currentAction == 'wander')"
            , "assert(aiState[1].nextActionAt == 0)"
            ]

        it "fills ONLY what a restored row is missing: every value the \
           \payload actually carries survives, including a nextActionAt \
           \in the past and a currentAction the action list no longer \
           \knows -- a save's own scheduling is the save's to state" $
            runsOk $ lns $ unitAiDefaultsPrelude ⧺
            [ "local refs = require('scripts.unit_ai_save_refs')"
            , "-- Unit 1 is complete, unit 2 has exactly one of the three."
            , "local rows = {"
            , "  [1] = { currentAction = 'retired_action',"
            , "          actionStartedAt = 1.5, nextActionAt = 2.5 },"
            , "  [2] = { nextActionAt = 7.5 },"
            , "}"
            , "local prep = saveModules.prepareLoad({"
            , "  { id = 'unit_ai', version = 7,"
            , "    payload = codec.encode(refs.wrapAiState(rows)) },"
            , "}, 1, false, { unit = { [1] = true, [2] = true }, building = {} })"
            , "assert(prep.ok, table.concat(prep.errors or {}, '; '))"
            , "saveModules.applyAll()"
            , "reconcile.reconcile(aiState, { 1, 2 }, {}, CTX)"
            , "assert(aiState[1].currentAction == 'retired_action',"
            , "  'a restored currentAction must never be reset to idle')"
            , "assert(aiState[1].actionStartedAt == 1.5,"
            , "  'a restored actionStartedAt must never be re-clocked')"
            , "assert(aiState[1].nextActionAt == 2.5,"
            , "  'a restored nextActionAt must never be reset to 0')"
            , "assert(aiState[2].nextActionAt == 7.5,"
            , "  'a partially sparse row keeps the value it does carry')"
            , "assert(aiState[2].currentAction == 'idle' and"
            , "       aiState[2].actionStartedAt == NOW,"
            , "  'and gains only the ones it does not')"
            ]

        it "leaves applyEntityRows' generic semantics untouched: an \
           \absent-owner row is still dropped with its one diagnostic \
           \and is never normalized into existence, and the published \
           \aiState is the SAME table object consumers already hold \
           \(#900)" $ runsOk $ lns $ unitAiDefaultsPrelude ⧺
            [ "local refs = require('scripts.unit_ai_save_refs')"
            , "local warnings = {}"
            , "engine.logWarn = function(msg)"
            , "  warnings[#warnings + 1] = tostring(msg) end"
            , "-- What a CONSUMER holds: the reference every other unit-AI"
            , "-- module took when the singleton was created, captured"
            , "-- BEFORE the load. If the restore rebound aiState to a"
            , "-- fresh table instead of mutating it, this reference would"
            , "-- still point at the old one and would never see the"
            , "-- restored rows -- the orphaning #900 exists to prevent,"
            , "-- and what makes this more than comparing a local to"
            , "-- itself."
            , "local consumerRef = aiState"
            , "local rows = { [1] = { buildTarget = 1 }, [9] = { buildTarget = 1 } }"
            , "local prep = saveModules.prepareLoad({"
            , "  { id = 'unit_ai', version = 7,"
            , "    payload = codec.encode(refs.wrapAiState(rows)) },"
            , "}, 1, false, { unit = { [1] = true }, building = {} })"
            , "assert(prep.ok, 'an absent owner is tolerated-dangling: '"
            , "  .. table.concat(prep.errors or {}, '; '))"
            , "saveModules.applyAll()"
            , "assert(aiState[9] == nil,"
            , "  'a row whose unit is absent must be dropped')"
            , "assert(#warnings == 1, 'exactly one drop diagnostic, got '"
            , "  .. #warnings)"
            , "assert(warnings[1]:find('9', 1, true) ~= nil,"
            , "  'the diagnostic must name the dropped unit: ' .. warnings[1])"
            , "-- Building 1 survives too, so buildTarget stays resolvable"
            , "-- and the reference below is testing table identity rather"
            , "-- than the dangling-reference scrub."
            , "reconcile.reconcile(aiState, { 1 }, { 1 }, CTX)"
            , "assert(aiState[1] ~= nil and aiState[1].nextActionAt == 0,"
            , "  'the retained row applies AND is normalized')"
            , "assert(aiState[9] == nil,"
            , "  'normalizing the retained rows must not resurrect the "
              <> "dropped one')"
            , "assert(consumerRef[1] ~= nil and consumerRef[1].buildTarget == 1"
            , "       and consumerRef[1].nextActionAt == 0,"
            , "  'the reference a consumer took BEFORE the load must see the "
              <> "restored, normalized rows -- aiState is mutated in place, "
              <> "never rebound')"
            ]

        it "leaves a SPARSE pre-load row untouched when an abandoned \
           \load unwinds through it: apply() is also applyAll's rollback \
           \entry point, and that unwind must restore the old session \
           \VERBATIM -- the fill is post-PUBLICATION, which a rolled-back \
           \load never reaches" $ runsOk $ lns $ unitAiDefaultsPrelude ⧺
            [ "local refs = require('scripts.unit_ai_save_refs')"
            , "local defaults = require('scripts.unit_ai_defaults')"
            , "-- The PRE-LOAD live session carries a sparse row. (A real"
            , "-- one cannot, now that both installers normalize -- but the"
            , "-- rollback contract is 'verbatim', not 'verbatim for rows"
            , "-- that happen to be complete', and it is the contract this"
            , "-- pins.)"
            , "aiState[1] = { buildTarget = 1 }"
            , "-- A reset hook that throws: it runs only AFTER every"
            , "-- component has committed, so unit_ai's forward apply has"
            , "-- definitely happened and is then unwound -- the exact"
            , "-- ordering an apply-failure in a later component produces,"
            , "-- without needing to force one."
            , "saveModules.registerResetHook('boom', function()"
            , "  error('reset hook failed') end)"
            , "local prep = saveModules.prepareLoad({"
            , "  { id = 'unit_ai', version = 7,"
            , "    payload = codec.encode(refs.wrapAiState("
            , "      { [9] = { buildTarget = 1 } })) },"
            , "}, 1, false, { unit = { [9] = true }, building = {} })"
            , "assert(prep.ok, table.concat(prep.errors or {}, '; '))"
            , "local ok = pcall(saveModules.applyAll)"
            , "assert(not ok, 'a throwing reset hook must fail the load')"
            , "-- The unwind restored the OLD session. Its sparse row must"
            , "-- come back exactly as it was, and reconcile -- which is"
            , "-- what would have filled it -- never ran."
            , "assert(aiState[1] ~= nil, 'the pre-load row must be restored')"
            , "assert(aiState[1].buildTarget == 1, 'restored verbatim')"
            , "assert(aiState[9] == nil,"
            , "  'the row from the abandoned load must be gone')"
            , "for _, f in ipairs(defaults.FIELDS) do"
            , "  assert(aiState[1][f.name] == nil,"
            , "    'a rollback must not add ' .. f.name .. ' to a pre-load "
              <> "row -- the unwind is VERBATIM, and the load it belongs to "
              <> "was abandoned')"
            , "end"
            ]

        it "fills a restored row from the SAME declaration ensureState \
           \builds a fresh one from, so the two installers cannot drift \
           \-- the enumeration is one list, not two agreeing by \
           \coincidence" $ runsOk $ lns $ unitAiDefaultsPrelude ⧺
            [ "local defaults = require('scripts.unit_ai_defaults')"
            , "local core = require('scripts.unit_ai_core')"
            , "local fresh = core.ensureState(42)"
            , "local normalized = defaults.normalize({})"
            , "-- Same keys, same values: whatever ensureState produces for"
            , "-- a unit the AI has never seen is exactly what a sparse"
            , "-- restored row is brought up to."
            , "for k, v in pairs(fresh) do"
            , "  assert(normalized[k] == v,"
            , "    'ensureState set ' .. k .. ' but normalize did not match')"
            , "end"
            , "for k, v in pairs(normalized) do"
            , "  assert(fresh[k] == v,"
            , "    'normalize set ' .. k .. ' but ensureState did not match')"
            , "end"
            , "-- And the declaration names exactly the fields a tick reads"
            , "-- before it has decided anything (#2055 requirement 2)."
            , "local named = {}"
            , "for _, f in ipairs(defaults.FIELDS) do named[f.name] = true end"
            , "assert(named.currentAction and named.actionStartedAt"
            , "       and named.nextActionAt,"
            , "  'the three fields the pre-decision tick path reads must all "
              <> "be declared')"
            , "assert(#defaults.FIELDS == 3,"
            , "  'a field added to FIELDS needs its own justification here: '"
            , "  .. 'the list is the fields a tick reads BEFORE deciding, '"
            , "  .. 'not every field a row may carry')"
            , "assert(named.commandedTask == nil,"
            , "  'commandedTask must NOT be defaulted -- nil is its value')"
            ]

    describe "unit_ai post-load reconciliation (issue #1589)" $ do
        -- The WHOLE persisted path, not a hand-built table: a versioned
        -- payload goes through the REAL registered lua.unit_ai
        -- component's decode/migrate/validate/apply, and only then
        -- through the REAL reconcile -- so the wire wrap/unwrap, the
        -- per-entity apply and the stale-reference scrub are all
        -- exercised against the same rows in one pass. Requirement 9
        -- asks for exactly that here, including the per-page cases:
        -- bill 5 exists on BOTH pages (two different entities that
        -- share a number) and ground item 7 on page A only.
        it "resolves or clears EVERY reference family the schema \
           \declares on a row restored through the real component -- \
           \including the six the pre-#1589 scrub never reached -- and \
           \resolves the per-page kinds against the OWNING unit's page, \
           \so a same-numbered bill on another page is a different \
           \entity and a page A ground item is absent for a page B unit" $
            runsOk $ lns $ unitAiReconcilePrelude ⧺
            [ "-- v1 (bare ids) so the migration + wire wrap/unwrap run"
            , "-- before the reconcile ever sees a row."
            , "local prep = saveModules.prepareLoad({ { id = 'unit_ai',"
            , "  version = 1, payload = codec.encode({"
            , "  [1] = { attackTargetUid = 99,"
            , "          craftJob = { billId = 5, bid = 3,"
            , "                       recipeId = 'known_recipe' },"
            , "          repairJob = { instanceId = 901, itemFetched = true,"
            , "                        recipeId = 'known_repair',"
            , "                        defName = 'axe_steel',"
            , "                        consumable = 'whetstone' },"
            , "          pickupOrder = { gid = 7 },"
            , "          forageTarget = { kind = 'ground', gid = 71, x = 1, y = 1 },"
            , "          forageLoot = { 7, 71 }, foragePhase = 'collecting',"
            , "          harvestLoot = { 70 }, harvestPhase = 'collecting' },"
            , "  [2] = { craftJob = { billId = 5, bid = 42,"
            , "                       recipeId = 'known_recipe' },"
            , "          pickupOrder = { gid = 7 } } }) } },"
            , "  1, false, { unit = { [1] = true, [2] = true },"
            , "              building = { [42] = true },"
            , "              unitPage = { [1] = 'A', [2] = 'B' } })"
            , "assert(prep.ok, 'a DANGLING reference is tolerated at load, "
              <> "never a failure: ' .. table.concat(prep.errors or {}, '; '))"
            , "saveModules.applyAll()"
            , "assert(aiState[1] ~= nil and aiState[2] ~= nil,"
            , "  'both rows apply -- their units are in the restored session')"
            , "assert(aiState[1].craftJob.billId == 5,"
            , "  'apply unwraps the wire reference back to a bare number, "
              <> "which is what the reconcile then resolves')"
            , ""
            , "reconcile.reconcile(aiState, { 1, 2 }, { 42 }, CTX)"
            , ""
            , "local one = aiState[1]"
            , "assert(one.attackTargetUid == nil,"
            , "  'a unit ref outside the survivor set clears (the #195 case)')"
            , "assert(one.craftJob == nil,"
            , "  'craftJob is dropped WHOLE: its bill resolved, but its "
              <> "station did not')"
            , "assert(one.repairJob == nil and one.repairPhase == nil,"
            , "  'repairJob goes out through unit_ai_repair.lua\\'s abort "
              <> "path, which clears the phase too')"
            , "assert(MULE_MOVES == 1,"
            , "  'and hands the already-fetched item back to the mule, "
              <> "rather than leaving it stranded: ' .. tostring(MULE_MOVES))"
            , "assert(one.pickupOrder ~= nil and one.pickupOrder.gid == 7,"
            , "  'ground item 7 exists on page A, where unit 1 lives, so its "
              <> "order survives untouched')"
            , "assert(one.forageTarget == nil,"
            , "  'a ground forageTarget naming no live ground item clears')"
            , "assert(#one.forageLoot == 1 and one.forageLoot[1] == 7,"
            , "  'forageLoot keeps its resolvable gid as a dense array')"
            , "assert(one.foragePhase == 'collecting',"
            , "  'a still-populated forage list keeps its phase')"
            , "assert(one.harvestLoot == nil and one.harvestPhase == nil,"
            , "  'an EMPTIED harvest list leaves the shape its own "
              <> "exhaustion path leaves')"
            , ""
            , "local two = aiState[2]"
            , "assert(two.craftJob ~= nil and two.craftJob.billId == 5,"
            , "  'unit 2 lives on page B, which has its OWN bill 5 -- a "
              <> "different entity that happens to share the number')"
            , "assert(two.pickupOrder == nil,"
            , "  'ground item 7 exists only on page A, so it is absent for "
              <> "a page B unit -- never resolved session-wide')"
            , ""
            , "-- Seven dangling declared edges: attackTargetUid,"
            , "-- craftJob.bid, repairJob.instanceId, forageTarget.gid,"
            , "-- forageLoot[2] and harvestLoot[1] on unit 1, plus unit 2's"
            , "-- pickupOrder.gid. craftJob.billId resolved, so the sibling"
            , "-- removed with the dropped job counts nothing."
            , "local reported = tonumber(LOG[#LOG]:match('(%d+) stale ref'))"
            , "assert(reported == 7,"
            , "  'the reconcile log must count every dangling edge removed, "
              <> "got ' .. tostring(reported))"
            ]

        it "refuses to reconcile at all when the engine supplies no \
           \reconciliation context, rather than silently resolving \
           \per-page ids against whichever page is active" $
            runsOk $ lns $ unitAiReconcilePrelude ⧺
            [ "aiState[1] = { pickupOrder = { gid = 7 } }"
            , "assert(not pcall(reconcile.reconcile, aiState, { 1 }, {}, nil),"
            , "  'an absent context must raise, not be treated as empty')"
            , "assert(aiState[1].pickupOrder ~= nil,"
            , "  'and must not have cleared anything on its way out')"
            ]

    describe "component version bounds (issue #761 round-4 review)" $ do
        it "rejects a version or inputVersions entry that is non-finite or \
           \outside Word32's representable range -- such a value passed \
           \Lua's own \"positive integer\" check (floor(math.huge) is \
           \math.huge) but HsLua's tointeger can't convert it, which used \
           \to make the whole component record silently vanish instead of \
           \failing the registration" $ runsOk $ lns
            [ "local saveModules = require('scripts.lib.save_modules')"
            , "local function tryRegister(version, inputVersions)"
            , "  return pcall(saveModules.register, 'bad_version', {"
            , "    version = version, inputVersions = inputVersions,"
            , "    required = true, scope = 'global', deps = {},"
            , "    snapshot = function() return {} end,"
            , "    decode = function(_v, d) return d end,"
            , "    validate = function() return nil end,"
            , "    apply = function() end,"
            , "  })"
            , "end"
            , "local ok1 = tryRegister(math.huge, { math.huge })"
            , "assert(not ok1, 'math.huge must not be accepted as a version')"
            , "local ok2 = tryRegister(-math.huge, { -math.huge })"
            , "assert(not ok2, '-math.huge must not be accepted as a version')"
            , "local ok3 = tryRegister(4294967296, { 4294967296 })"
            , "assert(not ok3, 'a version above Word32 max must not be accepted')"
            , "local ok4 = tryRegister(0/0, { 0/0 })"
            , "assert(not ok4, 'NaN must not be accepted as a version')"
            , "local ok5 = tryRegister(1, { 1 })"
            , "assert(ok5, 'an ordinary positive integer version must still register')"
            ]

    -- Issue #766 (save-overhaul C4): docs/save_compat/manifest.json's
    -- "b3-lua-versioned" baseline tracks these two .bin fixtures --
    -- REAL v1 unit_ai/building_spawn payloads encoded through the
    -- genuine scripts/lib/data_codec.lua (via a real HsLua VM, see
    -- tools/save_compat_audit.py's "add tracked Lua payload/session
    -- fixtures with canonical expectations and exercise them through
    -- the real Lua preparation path" requirement) -- not re-synthesized
    -- inline via codec.encode the way every OTHER test above does. This
    -- proves the tracked BYTES ON DISK are what saveModules.prepareLoad
    -- accepts, matching test-headless/data/save-compat/
    -- lua-unit-ai-v1.expected.json / lua-building-spawn-v1.expected.json.
    describe "tracked v1 fixtures from disk (issue #766, save-overhaul C4)" $ do
        it "migrates the tracked lua-unit-ai-v1.bin fixture through \
           \saveModules.prepareLoad/applyAll to exactly the canonical \
           \unwrapped aiState and reference edges its .expected.json \
           \records" $ do
            bytes ← BS.readFile
                "test-headless/data/save-compat/lua-unit-ai-v1.bin"
            runsOkWithPayloads [("FIXTURE", bytes)] $ lns
                [ "unit = { exists = function(_uid) return true end }"
                , "craft = { get = function(id)"
                , "  if id == 'x' then return { id = 'x' } end return nil end }"
                , "item = { listDefs = function() return {} end }"
                , "local unitAiSave = require('scripts.unit_ai_save')"
                , "local fakeAiState = {}"
                , "unitAiSave.register(fakeAiState)"
                , "local saveModules = require('scripts.lib.save_modules')"
                , "local prep = saveModules.prepareLoad({"
                , "  { id = 'unit_ai', version = 1, payload = FIXTURE },"
                , "})"
                , "assert(prep.ok, 'the tracked v1 fixture must migrate cleanly: '"
                , "  .. table.concat(prep.errors or {}, '; '))"
                , "local found = {}"
                , "for _, r in ipairs(prep.references) do"
                , "  found[r.kind .. ':' .. tostring(r.id)] = r.owner"
                , "end"
                , "assert(found['unit:7'] == 7,"
                , "  'the outer per-unit key itself must be a reference')"
                , "assert(found['unit:8'] == 7, 'attackTargetUid must resolve')"
                , "assert(found['building:20'] == 7, 'buildTarget must resolve')"
                , "assert(found['craft_bill:3'] == 7, 'craftJob.billId must resolve')"
                , "assert(found['building:21'] == 7, 'craftJob.bid must resolve')"
                , "saveModules.applyAll()"
                , "assert(fakeAiState[7].attackTargetUid == 8,"
                , "  'apply() must unwrap attackTargetUid to a bare number')"
                , "assert(fakeAiState[7].buildTarget == 20,"
                , "  'apply() must unwrap buildTarget to a bare number')"
                , "assert(fakeAiState[7].craftJob.billId == 3,"
                , "  'apply() must unwrap craftJob.billId to a bare number')"
                , "assert(fakeAiState[7].craftJob.bid == 21,"
                , "  'apply() must unwrap craftJob.bid to a bare number')"
                , "assert(fakeAiState[7].craftJob.recipeId == 'x',"
                , "  'non-reference fields must survive the migration untouched')"
                ]

        it "decodes the tracked lua-unit-ai-v4.bin fixture's per-unit \
           \location memories (#915) through saveModules.prepareLoad/ \
           \applyAll, keeping each entry's page, id and remembered anchor, \
           \and reporting one page-qualified location_instance edge each" $ do
            -- The complete-session baseline's canonical summary is
            -- Lua-OPAQUE (SessionSnapshot carries no Lua state), so
            -- save_compat_audit alone would pass even if this typed
            -- memory were dropped or mis-encoded. THIS is the assertion
            -- that would fail: real tracked bytes, produced by the real
            -- wrapAiState encoder, driven through the real preparation
            -- path — the same shape the e1 session fixture carries.
            bytes ← BS.readFile
                "test-headless/data/save-compat/lua-unit-ai-v4.bin"
            runsOkWithPayloads [("FIXTURE", bytes)] $ lns
                [ "unit = { exists = function(_uid) return true end }"
                , "item = { listDefs = function() return {} end }"
                , "local unitAiSave = require('scripts.unit_ai_save')"
                , "local fakeAiState = {}"
                , "unitAiSave.register(fakeAiState)"
                , "local saveModules = require('scripts.lib.save_modules')"
                , "local prep = saveModules.prepareLoad({"
                , "  { id = 'unit_ai', version = 4, payload = FIXTURE },"
                , "})"
                , "assert(prep.ok, 'the tracked v4 fixture must prepare cleanly: '"
                , "  .. table.concat(prep.errors or {}, '; '))"
                -- Each memory is reported as its OWN page-qualified edge:
                -- an id alone would be ambiguous, since the fixture
                -- deliberately carries the SAME instance id (1) on two
                -- different pages.
                , "local edges = {}"
                , "for _, r in ipairs(prep.references) do"
                , "  if r.kind == 'location_instance' then"
                , "    edges[r.path] = r end end"
                , "local a = edges['unit[7].knownLocations[1]']"
                , "local b = edges['unit[7].knownLocations[2]']"
                , "assert(a and a.id == 1 and a.page == 'generated_page'"
                , "       and a.owner == 7, 'first memory edge wrong')"
                , "assert(b and b.id == 1 and b.page == 'other_page'"
                , "       and b.owner == 7, 'second memory edge wrong')"
                , "saveModules.applyAll()"
                , "local ks = fakeAiState[7].knownLocations"
                , "assert(type(ks) == 'table' and #ks == 2,"
                , "  'apply() must restore both memories')"
                , "assert(ks[1].page == 'generated_page' and ks[1].id == 1"
                , "       and ks[1].x == 104 and ks[1].y == 40,"
                , "  'the first memory lost its page/id/anchor')"
                , "assert(ks[2].page == 'other_page' and ks[2].id == 1"
                , "       and ks[2].x == 3 and ks[2].y == 4,"
                , "  'the second memory lost its page/id/anchor')"
                -- aiState's LIVE shape never grows the wire tag.
                , "assert(ks[1].__ref == nil and ks[2].__ref == nil,"
                , "  'apply() must strip the __ref wire tag')"
                -- …and the sibling reference fields still migrate.
                , "assert(fakeAiState[7].attackTargetUid == 8,"
                , "  'a v4 payload must still unwrap its other references')"
                ]

        it "migrates the tracked lua-building-spawn-v1.bin fixture through \
           \saveModules.prepareLoad/applyAll to exactly the canonical \
           \unwrapped state and reference edges its .expected.json \
           \records" $ do
            bytes ← BS.readFile
                "test-headless/data/save-compat/lua-building-spawn-v1.bin"
            runsOkWithPayloads [("FIXTURE", bytes)] $ lns
                [ "building = { getInfo = function(_bid) return { id = _bid } end }"
                , "local buildingSpawn = require('scripts.building_spawn')"
                , "buildingSpawn.init('test')"
                , "local saveModules = require('scripts.lib.save_modules')"
                , "local prep = saveModules.prepareLoad({"
                , "  { id = 'building_spawn', version = 1, payload = FIXTURE },"
                , "})"
                , "assert(prep.ok, 'the tracked v1 fixture must migrate cleanly: '"
                , "  .. table.concat(prep.errors or {}, '; '))"
                , "local found = {}"
                , "for _, r in ipairs(prep.references) do"
                , "  found[r.kind .. ':' .. tostring(r.id)] = true"
                , "end"
                , "assert(found['building:12'],"
                , "  'the outer per-building key itself must be a reference')"
                , "assert(found['unit:4'], 'lastUid must be a reference')"
                , "saveModules.applyAll()"
                , "assert(buildingSpawn.state[12].lastUid == 4,"
                , "  'apply() must unwrap lastUid to a bare number in LIVE state')"
                , "assert(buildingSpawn.state[12].lastSpawnedAt == 123.5,"
                , "  'non-reference fields must survive the migration untouched')"
                ]
