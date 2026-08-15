-- Shared pixel-width FITTING for text DISPLAY surfaces (#1159, #1107):
-- wrapping text that may run onto more lines, and truncating text that
-- must stay on one.
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
local utf8Safe = require("scripts.ui.utf8_safe")
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

-- The ONE ellipsis every width-bounded UI surface truncates with (#1157).
-- Two dots, not three: this is the form the shared helper already rendered
-- for the save browser, the list widget and the three inventory panels, so
-- unifying on it moves only the two surfaces that still disagreed rather
-- than restyling every surface already routed through here.
local ELLIPSIS = ".."

-- Fit `text` into `maxPx` pixels on ONE line, appending ELLIPSIS when
-- something had to be dropped (#618's rule; extracted here by #1107 so
-- the save browser's row columns and the unit-info inventory share one
-- implementation instead of each carrying a private copy, and adopted by
-- the popup and the event log in #1157, which retired the last two
-- private copies). The complete case list, which #1157 settled because
-- the five private copies each guarded a different subset of it:
--   * nil or empty text returns unchanged;
--   * a nil maxPx is "no bound at all" and returns unchanged;
--   * non-empty text with maxPx <= 0 returns "" -- the same answer as
--     the unfittable-ellipsis case below, since a non-positive budget
--     cannot fit the ellipsis either. #1107 returned the ORIGINAL text
--     here; #1157's spec replaced that carve-out with the uniform rule,
--     so a caller whose allocated column collapsed to nothing (an
--     out-of-envelope framebuffer/scale combination) drops the field
--     instead of drawing full-width text across its neighbour;
--   * text that already fits returns unchanged;
--   * a positive maxPx too narrow for the ellipsis itself returns "";
--   * a maxPx that fits the ellipsis but no character of text returns
--     the ellipsis alone -- never an over-width first character;
--   * otherwise the longest complete UTF-8 prefix that fits with it.
--
-- Width is measured ONE way: every measurement below is of a complete
-- candidate, ellipsis included (the pre-check measures the candidate
-- whose prefix is empty). Measuring the prefix and adding a precomputed
-- ellipsis width is a different quantity the moment the font path grows
-- kerning, and it was one of the three ways the five copies diverged.
--
-- The binary search runs over BYTE lengths (so it costs log2(#text)
-- measurements rather than one per character), which is exactly why
-- every candidate cut is snapped back to a character boundary first:
-- string.sub cuts by byte offset, and a cut landing inside a multi-byte
-- sequence would both mis-measure and render a dangling lead byte.
function textWrap.truncateToWidth(text, font, fontPx, maxPx)
    if not text or text == "" then return text end
    if not maxPx then return text end
    if maxPx <= 0 then return "" end
    local full = engine.getTextWidth(font, text, fontPx) or 0
    if full <= maxPx then return text end
    -- lo counts BYTES kept, starting at 0: the ellipsis-alone candidate.
    -- Rejecting it here is what makes "" the answer when even the dots
    -- do not fit, and starting at 0 rather than 1 is what keeps an
    -- over-width first character out of the result.
    if (engine.getTextWidth(font, ELLIPSIS, fontPx) or 0) > maxPx then return "" end
    local lo, hi = 0, #text
    while lo < hi do
        local mid = math.floor((lo + hi + 1) / 2)
        local cut = utf8Safe.snapToCharBoundary(text, mid)
        local candidate = text:sub(1, cut) .. ELLIPSIS
        if (engine.getTextWidth(font, candidate, fontPx) or 0) <= maxPx then
            lo = mid
        else
            hi = mid - 1
        end
    end
    return text:sub(1, utf8Safe.snapToCharBoundary(text, lo)) .. ELLIPSIS
end

return textWrap
