-- Offline regression harness for issue #321: the save-state serializer
-- (scripts/lib/serialize.lua) must round-trip non-finite numbers.
--
-- Before the fix, %.17g emitted "inf"/"-inf"/"nan", which on reload in
-- the empty deserialize env became undefined globals: inf/nan silently
-- dropped to nil, and "-inf" parsed as -(global inf) → an arithmetic
-- error that lost the module's ENTIRE saved blob. PASS = every value
-- round-trips and finite numbers are unchanged.

package.path = "./?.lua;" .. package.path

local s = require("scripts.lib.serialize")

local failures = 0
local function check(name, cond)
    if cond then
        print("PASS  " .. name)
    else
        print("FAIL  " .. name)
        failures = failures + 1
    end
end

local function roundtrip(v)
    return s.deserialize(s.serialize(v))
end

-- Non-finite scalars survive the round-trip.
check("+inf round-trips", roundtrip(math.huge) == math.huge)
check("-inf round-trips", roundtrip(-math.huge) == -math.huge)
local nan = roundtrip(0/0)
check("nan round-trips", nan ~= nan)  -- nan is the only value ~= itself

-- Finite numbers are byte-identical to the old %.17g output.
for _, v in ipairs({ 0, -1, 3.5, 1234567.89, 1e-9, math.pi, -0.0 }) do
    check("finite " .. tostring(v) .. " unchanged",
          roundtrip(v) == v and s.serialize(v) == string.format("%.17g", v))
end

-- The real trigger: a saved table carrying math.huge sentinels (e.g.
-- unit_ai AI memory) must not lose sibling fields. Pre-fix, the -inf
-- field threw and the whole table failed to deserialize.
local blob = { cooldown = math.huge, floor = -math.huge, bad = 0/0,
               name = "acolyte", n = 42, nested = { t = math.huge } }
local dec = roundtrip(blob)
check("sentinel table: cooldown", dec.cooldown == math.huge)
check("sentinel table: floor",    dec.floor == -math.huge)
check("sentinel table: bad(nan)", dec.bad ~= dec.bad)
check("sentinel table: name kept", dec.name == "acolyte")
check("sentinel table: n kept",    dec.n == 42)
check("sentinel table: nested",    dec.nested.t == math.huge)

-- An infinite numeric KEY also round-trips (nan keys are illegal in Lua,
-- so they can never reach the serializer from a key slot).
local keyed = roundtrip({ [math.huge] = "topcap" })
check("inf numeric key", keyed[math.huge] == "topcap")

if failures == 0 then
    print("ALL PASS")
    os.exit(0)
else
    print(failures .. " FAILURE(S)")
    os.exit(1)
end
