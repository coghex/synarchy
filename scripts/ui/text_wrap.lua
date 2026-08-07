-- Shared pixel-width wrapping for text DISPLAY surfaces (#1159).
--
-- Lua strings are UTF-8 byte arrays, and both `#`/`string.sub` AND Lua
-- patterns work in BYTES -- `.` matches one byte, not one character. A
-- wrapper that advances a byte at a time therefore cuts multi-byte
-- sequences in half and renders mojibake, and measures the line once per
-- byte instead of once per character. Every wrapping helper must advance
-- one Unicode CODE POINT at a time; this module is the one implementation
-- that does, shared by the debug console (character wrap) and the log
-- panels (word wrap with a character hard-break).
--
-- Unlike scripts/ui/utf8_safe.lua -- whose editable-widget contract lets it
-- ASSERT on malformed UTF-8 -- these functions must never raise: they wrap
-- whatever an arbitrary Lua value happened to stringify to. The walk below
-- is total: for valid UTF-8 it yields exactly one code point per step, and
-- for a stray byte it yields that byte rather than dropping it, so the
-- returned lines always concatenate back to the input.
local textWrap = {}

-- True if the byte at 1-based position i is a UTF-8 continuation byte
-- (10xxxxxx), which is never the start of a character.
local function isContinuation(text, i)
    local b = string.byte(text, i)
    return b ~= nil and b >= 0x80 and b <= 0xBF
end

-- Iterate `text` one code point at a time. One left-to-right pass, so the
-- number of width measurements a caller makes scales with CHARACTERS, never
-- with bytes.
local function codepoints(text)
    local i, n = 1, #text
    return function()
        if i > n then return nil end
        local j = i + 1
        while j <= n and isContinuation(text, j) do j = j + 1 end
        local char = string.sub(text, i, j - 1)
        i = j
        return char
    end
end

-- Wrap `text` to `maxWidth` pixels, breaking between CHARACTERS wherever the
-- line would overflow (no word awareness -- spaces are ordinary characters).
-- Returns at least one line; the lines always concatenate back to `text`.
function textWrap.byCharacter(text, maxWidth, font, fontSize)
    local lines = {}
    local currentLine = ""

    for char in codepoints(text) do
        local testLine = currentLine .. char
        local width = engine.getTextWidth(font, testLine, fontSize)

        if width > maxWidth and currentLine ~= "" then
            lines[#lines + 1] = currentLine
            currentLine = char
        else
            currentLine = testLine
        end
    end

    if currentLine ~= "" then lines[#lines + 1] = currentLine end
    if #lines == 0 then lines[1] = text end
    return lines
end

-- Wrap `text` to `maxWidth` pixels on whitespace, hard-breaking (between
-- CHARACTERS) any single word wider than the line. Runs of whitespace
-- collapse -- the log panels render prose, not preformatted text. Returns a
-- list of line strings (always at least one).
function textWrap.byWord(text, maxWidth, font, fontSize)
    local function fits(str)
        return engine.getTextWidth(font, str, fontSize) <= maxWidth
    end
    local lines, cur = {}, ""

    for word in text:gmatch("%S+") do
        local trial = (cur == "") and word or (cur .. " " .. word)
        if fits(trial) then
            cur = trial
        else
            if cur ~= "" then lines[#lines + 1] = cur; cur = "" end
            if fits(word) then
                cur = word
            else
                -- A single word wider than the panel: hard-break it.
                local chunk = ""
                for ch in codepoints(word) do
                    if fits(chunk .. ch) then
                        chunk = chunk .. ch
                    else
                        if chunk ~= "" then lines[#lines + 1] = chunk end
                        chunk = ch
                    end
                end
                cur = chunk
            end
        end
    end

    if cur ~= "" then lines[#lines + 1] = cur end
    if #lines == 0 then lines[1] = "" end
    return lines
end

return textWrap
