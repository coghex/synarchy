-- Minimal Lua table → string serializer for save/load v2 Phase 5.
--
-- Handles: nil, boolean, number, string, and tables of any of the
-- above (recursively). Tables must use number or string keys only;
-- function/userdata/coroutine values are NOT supported and throw an
-- error.
--
-- Output is a valid Lua expression — `serialize({a=1}) == '{["a"]=1}'`.
-- Deserialize uses load() with an empty environment so the loaded
-- chunk can't reach globals or call into the runtime.
--
-- Designed for the small AI/spawn state tables in unit_ai +
-- building_spawn + pause. Not optimized for large payloads.

local serializeLib = {}

local function ser(v)
    local t = type(v)
    if t == "nil"     then return "nil"
    elseif t == "boolean" then return tostring(v)
    elseif t == "number"  then return string.format("%.17g", v)
    elseif t == "string"  then return string.format("%q", v)
    elseif t == "table"   then
        local parts = {}
        for k, x in pairs(v) do
            local kt = type(k)
            local ks
            if kt == "string" then
                ks = string.format("[%q]", k)
            elseif kt == "number" then
                ks = "[" .. string.format("%.17g", k) .. "]"
            else
                error("serialize: unsupported key type " .. kt)
            end
            parts[#parts + 1] = ks .. "=" .. ser(x)
        end
        return "{" .. table.concat(parts, ",") .. "}"
    else
        error("serialize: unsupported value type " .. t)
    end
end

function serializeLib.serialize(value)
    return ser(value)
end

function serializeLib.deserialize(str)
    if str == nil or str == "" then return nil end
    -- "t" mode = text-only chunk (no bytecode). Empty env table
    -- prevents the chunk from reaching _G, io, os, etc.
    local fn, err = load("return " .. str, "savedata", "t", {})
    if not fn then
        error("deserialize: " .. tostring(err))
    end
    return fn()
end

return serializeLib
