-- Private random stream for the UI widget kit (#1330).
--
-- Lua gives a state exactly ONE `math.random` stream, and gameplay
-- draws from it: AI decision cadence, thoughts, mental state, wildlife
-- wander, sleep, water scanning, location rolls. A widget that rolls a
-- suggested world seed off that same stream shifts every later
-- simulation decision, so the kit keeps its own stream here.
--
-- Nothing in this file touches `math.random` or `math.randomseed`.
-- Gameplay's stream stays exactly where `luaopen_math` left it — the
-- per-state entropy the engine establishes for it in
-- `Engine.Scripting.Lua.Thread.createLuaBackendState`, before any
-- script loads.
local random = {}

-----------------------------------------------------------
-- Entropy
-----------------------------------------------------------

-- Fold a value's `tostring` identity into an integer. For a table that
-- reads `table: 0x...`, which is this state's own heap address for it.
--
-- That is deliberately the same entropy Lua's auto-seed uses: lmathlib's
-- `randseed` seeds each fresh state from `time(NULL)` and the address of
-- its `lua_State`. Two states alive at once cannot share one address, so
-- their anchors differ by construction; between processes started in the
-- same second, address space layout randomization supplies the
-- difference. The byte fold is the fallback for a build whose `tostring`
-- prints no address at all.
local function identityBits(value)
    local text = tostring(value)
    local hex = text:match("0[xX](%x+)")
    local acc = 0
    if hex then
        for digit in hex:gmatch("%x") do
            acc = (acc << 4) ~ tonumber(digit, 16)
        end
    else
        for i = 1, #text do
            acc = (acc * 31) ~ text:byte(i)
        end
    end
    return acc
end

-----------------------------------------------------------
-- SplitMix64
-----------------------------------------------------------

local GOLDEN_GAMMA = 0x9E3779B97F4A7C15
local MIX_A = 0xBF58476D1CE4E5B9
local MIX_B = 0x94D049BB133111EB
-- 2^63 - 1: a draw is masked down to non-negative, so `%` and the
-- comparisons below never meet a signed wrap.
local MAX_DRAW = 0x7FFFFFFFFFFFFFFF

-- `random` itself is the address anchor: `package.loaded` holds the
-- module table for the whole life of the Lua state, so this address is
-- stable within one state and unique between two live ones.
local state = os.time() ~ identityBits(random)

-- SplitMix64. Integer arithmetic wraps and `>>` fills with zeros in
-- Lua 5.4+, which is exactly what this generator wants.
local function nextBits()
    state = state + GOLDEN_GAMMA
    local z = state
    z = (z ~ (z >> 30)) * MIX_A
    z = (z ~ (z >> 27)) * MIX_B
    return (z ~ (z >> 31)) & MAX_DRAW
end

-----------------------------------------------------------
-- Public API
-----------------------------------------------------------

-- A uniform integer in [minVal, maxVal], inclusive at both ends — the
-- contract `math.random(m, n)` has, so a call site swapping to this
-- stream keeps its range semantics unchanged.
--
-- Rejection-sampled rather than reduced modulo, so no value in the range
-- comes up more often than another.
function random.integer(minVal, maxVal)
    minVal = math.floor(minVal)
    maxVal = math.floor(maxVal)
    if minVal > maxVal then
        error("random.integer: interval is empty", 2)
    end
    local span = maxVal - minVal + 1
    if span == 1 then return minVal end
    if span <= 0 then
        -- The subtraction wrapped: the range is wider than a Lua
        -- integer. Nothing here can serve it, and silently returning a
        -- value from part of it would be worse than saying so.
        error("random.integer: interval is wider than an integer", 2)
    end

    -- Largest draw keeping [0, limit] an exact multiple of `span`, so
    -- every residue is reachable equally often.
    local remainder = MAX_DRAW % span
    local limit = MAX_DRAW
    if remainder + 1 ~= span then
        limit = MAX_DRAW - remainder - 1
    end

    local draw = nextBits()
    while draw > limit do
        draw = nextBits()
    end
    return minVal + draw % span
end

return random
