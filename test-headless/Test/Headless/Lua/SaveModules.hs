{-# LANGUAGE UnicodeSyntax, OverloadedStrings, ScopedTypeVariables, TypeApplications #-}
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
            , "local fakeUnitAi = {}"
            , "unitAiSave.register(fakeUnitAi, fakeAiState)"
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
            , "local fakeUnitAi = {}"
            , "unitAiSave.register(fakeUnitAi, fakeAiState)"
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
            , "unitAiSave.register({}, {})"
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
            , "local fakeUnitAi = {}"
            , "unitAiSave.register(fakeUnitAi, fakeAiState)"
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
            , "-- repairJob.bid is deliberately OPTIONAL -- unit_ai_repair.lua"
            , "-- never actually sets it, so a repairJob with everything"
            , "-- else present but no bid must NOT be rejected."
            , "local repairNoBid = prepareWith({ [2] = { repairJob = {"
            , "  instanceId = { __ref = 'item_instance', id = 900 },"
            , "  recipeId = 'x', defName = 'wood' } } })"
            , "assert(repairNoBid.ok,"
            , "  'repairJob.bid must stay optional (it is never actually set '"
            , "  .. 'by unit_ai_repair.lua): ' .. table.concat(repairNoBid.errors or {}, '; '))"
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
            , "unitAiSave.register({}, fakeAiState)"
            , "local function prepareWith(state)"
            , "  return saveModules.prepareLoad({"
            , "    { id = 'unit_ai', version = 1, payload = codec.encode(state) },"
            , "  })"
            , "end"
            , "local badPack = prepareWith({ [1] = { constructJob = {"
            , "  category = 'structure', pack = 'ghost_pack', kind = 'wall',"
            , "  need = {} } } })"
            , "assert(not badPack.ok,"
            , "  'a constructJob referencing a removed structure pack must reject the load')"
            , "local badKind = prepareWith({ [1] = { constructJob = {"
            , "  category = 'structure', pack = 'known_pack', kind = 'ghost_kind',"
            , "  need = {} } } })"
            , "assert(not badKind.ok,"
            , "  'a constructJob referencing a removed pack kind must reject the load')"
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
            , "unitAiSave.register({}, {})"
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
            , "local fakeUnitAi = {}"
            , "unitAiSave.register(fakeUnitAi, fakeAiState)"
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
            , "local fakeUnitAi = {}"
            , "unitAiSave.register(fakeUnitAi, fakeAiState)"
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
            , "local fakeUnitAi = {}"
            , "unitAiSave.register(fakeUnitAi, fakeAiState)"
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
            , "local fakeUnitAi = {}"
            , "unitAiSave.register(fakeUnitAi, fakeAiState)"
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
            , "local fakeUnitAi = {}"
            , "unitAiSave.register(fakeUnitAi, fakeAiState)"
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
            , "local fakeUnitAi = {}"
            , "unitAiSave.register(fakeUnitAi, fakeAiState)"
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
            , "unitAiSave.register({}, {})"
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
                , "local fakeUnitAi = {}"
                , "unitAiSave.register(fakeUnitAi, fakeAiState)"
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
