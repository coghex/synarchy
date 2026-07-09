-- UTF-8-safe byte-offset helper for binary-search text truncation
local utf8Safe = {}

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
    while n > 0 and isContinuation(text, n + 1) do
        n = n - 1
    end
    return n
end

return utf8Safe
