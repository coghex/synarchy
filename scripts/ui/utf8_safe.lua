-- Shared UTF-8 coordinate helpers.
--
-- UI.TextBuffer and the UI.* text-input API expose cursor positions as
-- zero-based Unicode CODE-POINT offsets. Lua's # and string.sub use BYTE
-- offsets. Editable widgets must convert through this module before slicing;
-- byte offsets are only exposed for the older read-only truncation callers.
-- Inputs are expected to be valid UTF-8, matching UI.TextBuffer's Text value.
local utf8Safe = {}

-- Number of Unicode code points. Combining marks remain separate positions;
-- grapheme-cluster navigation is deliberately outside this contract.
function utf8Safe.codepointLength(text)
    local length, invalidAt = utf8.len(text)
    assert(length ~= nil, "invalid UTF-8 at byte " .. tostring(invalidAt))
    return length
end

local function clampToLength(index, length)
    index = tonumber(index) or 0
    index = math.floor(index)
    if index < 0 then return 0 end
    if index > length then return length end
    return index
end

-- Clamp an external zero-based code-point cursor to [0, length].
function utf8Safe.clampIndex(text, index)
    local length = utf8Safe.codepointLength(text)
    return clampToLength(index, length)
end

local function byteOffsetAt(text, index, length)
    if index == length then return #text + 1 end
    return utf8.offset(text, index + 1)
end

-- Convert a zero-based code-point cursor to the corresponding one-based Lua
-- byte position. The end cursor maps to #text + 1, ready for string.sub.
function utf8Safe.byteOffset(text, index)
    local length = utf8Safe.codepointLength(text)
    index = clampToLength(index, length)
    return byteOffsetAt(text, index, length)
end

-- Slice by a half-open range of zero-based code-point offsets [start, end).
-- Missing end means the logical end of the string. Both positions clamp.
function utf8Safe.slice(text, startIndex, endIndex)
    local length = utf8Safe.codepointLength(text)
    local first = clampToLength(startIndex, length)
    local last = endIndex == nil and length or clampToLength(endIndex, length)
    if last < first then last = first end
    local firstByte = byteOffsetAt(text, first, length)
    local lastByte = byteOffsetAt(text, last, length)
    return string.sub(text, firstByte, lastByte - 1)
end

function utf8Safe.prefix(text, endIndex)
    return utf8Safe.slice(text, 0, endIndex)
end

function utf8Safe.suffix(text, startIndex)
    return utf8Safe.slice(text, startIndex)
end

-- True if the byte at 1-based position i is a UTF-8 continuation byte
-- (10xxxxxx). A continuation byte is never the start of a character, so a
-- string cut right before one would split whatever multi-byte sequence it
-- belongs to.
local function isContinuation(text, i)
    local b = string.byte(text, i)
    return b ~= nil and b >= 0x80 and b <= 0xBF
end

-- Snap byte length `n` (0..#text) down to the nearest length that does not
-- end mid-character. Lua strings are raw byte arrays -- string.sub cuts by
-- BYTE offset, not Unicode codepoint -- so a pixel-width binary search over
-- byte length can otherwise land its cut point inside a multi-byte UTF-8
-- sequence (e.g. right after the lead byte of an accented letter), producing
-- a candidate that ends in a dangling lead byte with no continuation byte.
function utf8Safe.snapToCharBoundary(text, n)
    n = math.max(0, math.min(#text, math.floor(tonumber(n) or 0)))
    while n > 0 and isContinuation(text, n + 1) do
        n = n - 1
    end
    return n
end

return utf8Safe
