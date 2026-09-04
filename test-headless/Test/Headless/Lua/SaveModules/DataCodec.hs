-- | @scripts/lib/data_codec.lua@'s wire-format contract (issue #761
--   requirement 8) -- one of the four owners
--   'Test.Headless.Lua.SaveModules' composes (issue #2047).
--
--   Round-tripping, canonical key order, the payload caps and their
--   per-call override, the escape grammar, and the legacy decode
--   shapes still on disk. Nothing here registers a component or
--   touches the registry.
module Test.Headless.Lua.SaveModules.DataCodec (spec) where

import UPrelude
import Test.Hspec

import Test.Headless.Lua.SaveModules.Support (lns, runsOk)

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
