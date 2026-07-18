-- Canonical, safe, DATA-ONLY codec for Lua save components (issue #761,
-- save-overhaul B3). Replaces scripts/lib/serialize.lua's load()-based
-- Lua-expression codec for anything that reaches a save file: decoding
-- NEVER compiles or executes saved text (no load()/loadstring() calls
-- anywhere in this module), and every encodable shape is declared
-- explicitly rather than "whatever Lua expression happens to parse".
--
-- Supported values: boolean, finite number, string (must already be
-- valid UTF-8), and tables built recursively from those -- either a
-- dense ARRAY (integer keys 1..n, no gaps) or a MAP (string or
-- whole-number-integer keys, any mix, no gaps required). Rejected at
-- encode time: functions, userdata, threads, tables carrying a
-- metatable, cyclic tables, non-finite numbers (NaN/+-inf, no current
-- schema needs them -- see CLAUDE.md's #761 section), non-string/
-- non-integer keys, invalid-UTF-8 strings, and anything exceeding the
-- size/depth/entry limits below. Every rejection names a data path
-- (e.g. "root.claims[3].power") so a caller can report exactly what
-- failed.
--
-- Wire format: a tag-prefixed, length/count-delimited encoding that is
-- only ever WALKED, never evaluated:
--
--   T / F                      -- true / false
--   N<len>:<digits>            -- number, %.17g-formatted ASCII digits
--   S<len>:<bytes>             -- string, raw UTF-8 bytes (len = byte count)
--   A<count>:<elem>...         -- array, `count` encoded values in order
--   M<count>:<key><val>...     -- map, `count` (key,value) pairs; a key is
--                                  itself an N or S value; canonical order:
--                                  numeric keys ascending, then string keys
--                                  ascending
--
-- Every string/array/map body is length- or count-prefixed so decode
-- never scans for a delimiter inside arbitrary payload bytes, and every
-- read is bounds-checked against the remaining input so a truncated or
-- hand-edited payload fails cleanly (an error with a path) instead of
-- indexing past the end of the string.

local M = {}

M.MAX_DEPTH         = 64
M.MAX_TABLE_ENTRIES = 200000       -- per single table (array or map)
M.MAX_STRING_BYTES  = 4 * 1024 * 1024
M.MAX_TOTAL_BYTES   = 16 * 1024 * 1024

local function isFiniteNumber(v)
    return v == v and v ~= math.huge and v ~= -math.huge
end

-- A strict UTF-8 validator (RFC 3629): walks byte-by-byte checking not
-- just the leading-byte / continuation-byte COUNT but the exact
-- allowed range of the first continuation byte per leading byte, which
-- is what rules out overlong encodings (e.g. a 2-byte encoding of a
-- codepoint that fits in 1 byte), UTF-16 surrogate halves (U+D800..
-- U+DFFF, never valid UTF-8 scalar values), and codepoints beyond the
-- Unicode range (> U+10FFFF). A truncated sequence or an invalid
-- leading/continuation byte is rejected the same as before.
local function isValidUtf8(s)
    local i, n = 1, #s
    while i <= n do
        local b = s:byte(i)
        local extra, lo2, hi2
        if b < 0x80 then extra = 0
        elseif b >= 0xC2 and b <= 0xDF then extra = 1; lo2 = 0x80; hi2 = 0xBF
        elseif b == 0xE0 then extra = 2; lo2 = 0xA0; hi2 = 0xBF  -- no overlong
        elseif b >= 0xE1 and b <= 0xEC then extra = 2; lo2 = 0x80; hi2 = 0xBF
        elseif b == 0xED then extra = 2; lo2 = 0x80; hi2 = 0x9F  -- no surrogates
        elseif b >= 0xEE and b <= 0xEF then extra = 2; lo2 = 0x80; hi2 = 0xBF
        elseif b == 0xF0 then extra = 3; lo2 = 0x90; hi2 = 0xBF  -- no overlong
        elseif b >= 0xF1 and b <= 0xF3 then extra = 3; lo2 = 0x80; hi2 = 0xBF
        elseif b == 0xF4 then extra = 3; lo2 = 0x80; hi2 = 0x8F  -- cap at U+10FFFF
        else return false end
        if i + extra > n then return false end
        if extra > 0 then
            local b2 = s:byte(i + 1)
            if b2 == nil or b2 < lo2 or b2 > hi2 then return false end
            for j = 2, extra do
                local cb = s:byte(i + j)
                if cb == nil or cb < 0x80 or cb > 0xBF then return false end
            end
        end
        i = i + extra + 1
    end
    return true
end

-- Whether table `t` is a dense array (keys exactly 1..n, no other
-- keys). Returns (isArray, n).
local function tableShape(t)
    local n = 0
    local isArrayShape = true
    for k in pairs(t) do
        n = n + 1
        if type(k) ~= "number" or k ~= math.floor(k) or k < 1 then
            isArrayShape = false
        end
    end
    if isArrayShape then
        for i = 1, n do
            if t[i] == nil then isArrayShape = false; break end
        end
    end
    return isArrayShape, n
end

local function isIntegerKey(k)
    return type(k) == "number" and isFiniteNumber(k) and k == math.floor(k)
end

-- Canonical map-key order: numeric keys ascending, then string keys
-- ascending (documented, deterministic -- requirement 8).
local function keyLess(a, b)
    local ta, tb = type(a), type(b)
    if ta ~= tb then return ta == "number" end
    return a < b
end

local function encodeValue(v, path, seen, depth)
    if depth > M.MAX_DEPTH then
        error("data_codec: exceeded max depth at " .. path)
    end
    local t = type(v)
    if t == "boolean" then
        return v and "T" or "F"
    elseif t == "number" then
        if not isFiniteNumber(v) then
            error("data_codec: non-finite number not supported at " .. path)
        end
        local digits = string.format("%.17g", v)
        return "N" .. #digits .. ":" .. digits
    elseif t == "string" then
        if not isValidUtf8(v) then
            error("data_codec: invalid UTF-8 string at " .. path)
        end
        if #v > M.MAX_STRING_BYTES then
            error("data_codec: string exceeds max size at " .. path)
        end
        return "S" .. #v .. ":" .. v
    elseif t == "table" then
        if getmetatable(v) ~= nil then
            error("data_codec: table with a metatable is not supported at "
                .. path)
        end
        if seen[v] then
            error("data_codec: cyclic table at " .. path)
        end
        seen[v] = true
        local ok, out = pcall(function()
            local isArr, n = tableShape(v)
            if isArr then
                if n > M.MAX_TABLE_ENTRIES then
                    error("data_codec: table exceeds max entries at " .. path)
                end
                local parts = { "A", tostring(n), ":" }
                for i = 1, n do
                    parts[#parts + 1] =
                        encodeValue(v[i], path .. "[" .. i .. "]", seen, depth + 1)
                end
                return table.concat(parts)
            else
                local keys, count = {}, 0
                for k in pairs(v) do
                    if type(k) ~= "string" and not isIntegerKey(k) then
                        error("data_codec: unsupported key type " .. type(k)
                            .. " at " .. path)
                    end
                    count = count + 1
                    keys[count] = k
                end
                if count > M.MAX_TABLE_ENTRIES then
                    error("data_codec: table exceeds max entries at " .. path)
                end
                table.sort(keys, keyLess)
                local parts = { "M", tostring(count), ":" }
                for _, k in ipairs(keys) do
                    local keyPath = path .. "[" .. tostring(k) .. "]"
                    parts[#parts + 1] =
                        encodeValue(k, keyPath .. "(key)", seen, depth + 1)
                    parts[#parts + 1] =
                        encodeValue(v[k], keyPath, seen, depth + 1)
                end
                return table.concat(parts)
            end
        end)
        seen[v] = nil
        if not ok then error(out, 0) end
        return out
    else
        error("data_codec: unsupported value type " .. t .. " at " .. path)
    end
end

-- Encode a data-only Lua value. Returns (bytes) on success, or
-- (nil, errorMessage) on any rejection.
function M.encode(value)
    local ok, result = pcall(encodeValue, value, "root", {}, 1)
    if not ok then
        return nil, tostring(result)
    end
    if #result > M.MAX_TOTAL_BYTES then
        return nil, "data_codec: encoded payload exceeds max size ("
            .. #result .. " bytes)"
    end
    return result
end

-- Read a `<digits>:` prefix starting at `pos` (used for both string/
-- number lengths and array/map counts). Bounds the digit run itself
-- (no realistic size needs more than 15 digits) so a pathological
-- input can't force an unbounded scan for the delimiter.
local function readPrefix(s, pos, path)
    local searchEnd = math.min(#s, pos + 15)
    local colon = s:find(":", pos, true)
    if not colon or colon > searchEnd + 1 then
        error("data_codec: malformed length/count prefix at " .. path)
    end
    local digits = s:sub(pos, colon - 1)
    if digits == "" or not digits:match("^%d+$") then
        error("data_codec: malformed length/count prefix at " .. path)
    end
    local n = tonumber(digits)
    if n == nil then
        error("data_codec: malformed length/count prefix at " .. path)
    end
    return n, colon + 1
end

local function decodeValue(s, pos, depth, path)
    if depth > M.MAX_DEPTH then
        error("data_codec: exceeded max depth at " .. path)
    end
    if pos > #s then
        error("data_codec: truncated payload at " .. path)
    end
    local tag = s:sub(pos, pos)
    if tag == "T" then
        return true, pos + 1
    elseif tag == "F" then
        return false, pos + 1
    elseif tag == "N" then
        local len, rest = readPrefix(s, pos + 1, path)
        local digits = s:sub(rest, rest + len - 1)
        if #digits ~= len then
            error("data_codec: truncated number at " .. path)
        end
        local num = tonumber(digits)
        if num == nil then
            error("data_codec: malformed number at " .. path)
        end
        if not isFiniteNumber(num) then
            error("data_codec: non-finite number not supported at " .. path)
        end
        return num, rest + len
    elseif tag == "S" then
        local len, rest = readPrefix(s, pos + 1, path)
        if len > M.MAX_STRING_BYTES then
            error("data_codec: string exceeds max size at " .. path)
        end
        local str = s:sub(rest, rest + len - 1)
        if #str ~= len then
            error("data_codec: truncated string at " .. path)
        end
        if not isValidUtf8(str) then
            error("data_codec: invalid UTF-8 string at " .. path)
        end
        return str, rest + len
    elseif tag == "A" then
        local count, rest = readPrefix(s, pos + 1, path)
        if count > M.MAX_TABLE_ENTRIES then
            error("data_codec: table exceeds max entries at " .. path)
        end
        local out, p = {}, rest
        for i = 1, count do
            local v
            v, p = decodeValue(s, p, depth + 1, path .. "[" .. i .. "]")
            out[i] = v
        end
        return out, p
    elseif tag == "M" then
        local count, rest = readPrefix(s, pos + 1, path)
        if count > M.MAX_TABLE_ENTRIES then
            error("data_codec: table exceeds max entries at " .. path)
        end
        local out, p = {}, rest
        for _ = 1, count do
            local k, v
            k, p = decodeValue(s, p, depth + 1, path .. "(key)")
            if type(k) ~= "string" and type(k) ~= "number" then
                error("data_codec: malformed map key at " .. path)
            end
            v, p = decodeValue(s, p, depth + 1, path .. "[" .. tostring(k) .. "]")
            out[k] = v
        end
        return out, p
    else
        error("data_codec: unrecognized tag " .. string.format("%q", tag)
            .. " at " .. path)
    end
end

-- Decode a payload produced by M.encode. Returns (value) on success, or
-- (nil, errorMessage) on any malformed/truncated/oversized input. Never
-- executes the input as code.
function M.decode(s)
    if type(s) ~= "string" then
        return nil, "data_codec: input is not a string"
    end
    if #s > M.MAX_TOTAL_BYTES then
        return nil, "data_codec: payload exceeds max size (" .. #s .. " bytes)"
    end
    local ok, result = pcall(function()
        local v, pos = decodeValue(s, 1, 1, "root")
        if pos ~= #s + 1 then
            error("data_codec: trailing bytes after decoded value")
        end
        return v
    end)
    if not ok then
        return nil, tostring(result)
    end
    return result
end

return M
